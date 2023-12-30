#!/usr/bin/env -S guix shell --pure bash coreutils cryptsetup dosfstools e2fsprogs expect grep gnupg parted sed util-linux zfs -- bash
set -euo pipefail

# ./partition-dutchie.sh target-disk [ / -?-u(n?mount)? / ]

. ./scripts/utils.sh

###
### Destroy / Create Partitions
###

KEYFILE="./privkeys/dutchie/system.key"
ZPOOL="dutchie"
LUKS_FORMAT=( --batch-mode luksFormat --type luks2 )

einfo "Checking for existing mounts"
for dir in /run/keystore/"$ZPOOL" /mnt/"$ZPOOL"-altroot/{boot/efi,boot,proc,sys,tmp,home,persist/cache,persist,root,var/log,var/guix,gnu/store,run/keystore/dutchie,}; do
  if mountpoint -q -- "$dir"; then erun "Unmounting $dir" umount "$dir"; fi
done

zfs unload-key -r dutchie || true
for part in boot swap keystore; do
  if [[ -b /dev/mapper/"$ZPOOL"-$part ]]; then
    erun "Closing crypt-device "$ZPOOL"-$part" cryptsetup close "$ZPOOL"-$part
  fi
done

# Just a shortcut.
re='(^|[ \t])-?-u(n?mount)?($|[ \t])'
if [[ "${@}" =~ $re ]]; then
  export-pool () {
    zpool status "$ZPOOL" \
      && zpool export dutchie \
      || true
  }
  erun "Exporting zpool $ZPOOL" export-pool
  exit 0
fi

zpool status "$ZPOOL" 2> /dev/null && {
  einfo "Found existing zpool "$ZPOOL". Destroy?"
  econfirm
  erun "Destroying zpool "$ZPOOL"" zpool destroy "$ZPOOL"
}

TARGET="$1"
erun "Confirming disk $TARGET exists" test -b "$TARGET"

esc="$(printf '\033')"
lsblk "$TARGET" | sed -E 's/^(sdc)((\s+\S+){2}\s+)(\S+)/'"$esc[43m\\1$esc[0m\\2$esc[43m\\4$esc[0m/"
einfo "Target disk:" "$TARGET"
ewarn "All data will be lost!"
econfirm

erun "Wiping $TARGET" wipefs -a "$TARGET"

erun "Creating partitions" \
  expect << EOF
spawn \
  parted --align=optimal -- $TARGET \
    unit MB \
    mklabel gpt \
    mkpart primary 0G 1G \
    set 1 esp on \
    name 1 $ZPOOL-efi \
    mkpart primary 1G 3G \
    name 2 $ZPOOL-boot \
    mkpart primary 3G 43G \
    name 3 $ZPOOL-swap \
    mkpart primary 43G 100% \
    name 4 $ZPOOL-root
expect {
  "Is this still acceptable to you?" {
    expect "Yes/No? "
    send -- "Y\r"
    exp_continue
  }
  eof
}
EOF

einfo "Confirming paritions exist"
INDENT+="  "
for i in $(seq 3); do erun "${TARGET}$i" test -b "${TARGET}$i"; done
INDENT="${INDENT:2}"

###
### Format and Encrypt Partitions
###

read-encryption-passphrase

format-efi () { mkfs.fat -F32 "${TARGET}1"; fatlabel "${TARGET}1" "$ZPOOL"-efi; }
erun "Formatting /boot/efi (Partition 1) as FAT32" format-efi

create-keyfile () {
  touch "$KEYFILE";
  chmod 600 "$KEYFILE"
  dd status=none bs=8 count=4 if=/dev/random of="$KEYFILE" iflag=fullblock
  gpg --batch --passphrase-fd 0 --symmetric "$KEYFILE" <<< "$PASSPHRASE"
  chmod 400 "$KEYFILE"
}
[[ -s "$KEYFILE" ]] \
  && einfo "Existing keyfile found" \
  || erun "Creating keyfile" create-keyfile

create-luks-volume () {
  # create-luks-volume partition-idx name &optional keyfile
  [[ -n "$1" ]] && [[ -n "$2" ]]
  DEV="$1"
  MAPPED_DEV=/dev/mapper/"$2"
  DO_PASSWORD="${3:-0}"
  DO_INTEGRITY="${4:-0}"
  EXTRA_FORMAT_ARGS=()
  if (( DO_INTEGRITY )); then
    EXTRA_FORMAT_ARGS=( --integrity hmac-sha256 )
  fi
  if (( DO_PASSWORD )); then
    expect-passphrase-prompt "Enter passphrase for" \
      cryptsetup ${LUKS_FORMAT[@]} ${EXTRA_FORMAT_ARGS[@]} "$DEV"
    expect-passphrase-prompt "Enter any existing passphrase:" \
      cryptsetup luksAddKey "$DEV" "$KEYFILE"
  else
    cryptsetup ${LUKS_FORMAT[@]} ${EXTRA_FORMAT_ARGS[@]} "$DEV" "$KEYFILE"
  fi
  cryptsetup open --key-file="$KEYFILE" "$DEV" "${MAPPED_DEV##*/}"
}

einfo "Creating /boot (on Partition 2)"
INDENT+="  "
format-boot () { mkfs.ext4 "$TARGET"2; e2label "$TARGET"2 "$ZPOOL"-boot; }
erun "Formatting as EXT4" format-boot
INDENT="${INDENT:2}"

einfo "Creating swap (on Partition 3)"
INDENT+="  "
erun "Encrypting" create-luks-volume "$TARGET"3 "$ZPOOL"-swap 0 1
format-swap () { mkswap /dev/mapper/"$ZPOOL"-swap; swaplabel -L "$ZPOOL"-swap /dev/mapper/"$ZPOOL"-swap; }
erun "Formatting as swap" format-swap
INDENT="${INDENT:2}"

[[ -d /mnt/"$ZPOOL"-altroot ]] || \
  erun "Creating altroot mount-point" mkdir -p /mnt/"$ZPOOL"-altroot

erun "Creating zpool $ZPOOL (on Partition 4)" \
  zpool create \
      -f \
      -m none \
      -R /mnt/"$ZPOOL"-altroot \
      -O acltype=posixacl \
      -O xattr=sa \
      -O relatime=on \
      -O compression=zstd-9 \
      "$ZPOOL" "${TARGET}4"

einfo "Creating keystore"
INDENT+="  "
KEYSTORE_ZVOL=/dev/zvol/"$ZPOOL"/keystore
KEYSTORE_DEV=/dev/mapper/"$ZPOOL"-keystore
KEYSTORE=/run/keystore/"$ZPOOL"
KEYSTORE_KEY="$KEYSTORE"/system.key
mount-keystore () {
  [[ -d "$KEYSTORE" ]] || mkdir -p "$KEYSTORE"
  mount "$KEYSTORE_DEV" "$KEYSTORE"
}
erun "Creating ZFS Volume '$ZPOOL/keystore'" zfs create -V 518M "$ZPOOL"/keystore
erun "Encrypting" create-luks-volume "$KEYSTORE_ZVOL" "${KEYSTORE_DEV##*/}" 1
format-keystore () { mkfs.ext4 "$KEYSTORE_DEV"; e2label "$KEYSTORE_DEV" "$ZPOOL"-keystore; }
erun "Formatting as EXT4" format-keystore
erun "Mounting" mount-keystore
erun "Installing keyfile" cp -f "$KEYFILE" "$KEYSTORE_KEY"
INDENT="${INDENT:2}"

einfo "Creating encrypted datasets"
INDENT+="  "
create-zfs-dataset () {
  NAME="$1"
  EXTRA_ARGS=()
  case "$NAME" in
    "gnu-store") EXTRA_ARGS=( -o atime=off ) ;;
  esac
  zfs create \
    ${EXTRA_ARGS[@]} \
    -o mountpoint=legacy \
    -o encryption=on \
    -o keyformat=raw \
    -o keylocation=file://"$KEYSTORE_KEY" \
    "$ZPOOL/$NAME"
  mkdir -p /mnt/"$ZPOOL"-altroot/"$2" || true
  mount -t zfs "$ZPOOL/$NAME" /mnt/"$ZPOOL"-altroot/"$2"
}

create-zfs-dataset guix-root ""

einfo "Creating immutable stubs"
INDENT+="  "
stub_at_path () {
  mkdir -p "$1"
  chmod 000 "$1"
  chattr +i "$1"
}
for STUB in boot gnu/store persist proc sys tmp var/log var/guix; do
  STUB_PATH=/mnt/"$ZPOOL"-altroot/"$STUB"
  erun "Stubbing $ZPOOL/guix-root/$STUB" stub_at_path "$STUB_PATH"
  if [[ "$STUB" =~ ^(proc|sys|tmp)$ ]]; then
    INDENT+="  "
    erun "Covering with TMPFS" \
      mount -t tmpfs "$ZPOOL-${STUB//\//-}" "$STUB_PATH"
    INDENT="${INDENT:2}"
  fi
done
INDENT="${INDENT:2}"

erun "dutchie/gnu-store"     create-zfs-dataset gnu-store     gnu/store
erun "dutchie/home"          create-zfs-dataset home          home

erun "dutchie/persist"       create-zfs-dataset persist       persist
erun "Stubbing /persist/cache" stub_at_path /mnt/"$ZPOOL"-altroot/persist/cache

erun "dutchie/persist-cache" create-zfs-dataset persist-cache persist/cache
erun "dutchie/root"          create-zfs-dataset root          root
erun "dutchie/var-log"       create-zfs-dataset var-log       var/log
erun "dutchie/var-guix"       create-zfs-dataset var-guix       var/guix

INDENT="${INDENT:2}"

einfo "Creating initial snapshots"
INDENT+="  "
for dataset in guix-root gnu-store home persist persist-cache root var-log var-guix; do
  erun "$ZPOOL/$dataset@blank" zfs snapshot $ZPOOL/$dataset@blank
done
INDENT="${INDENT:2}"

erun "Mounting /boot" mount "$TARGET"2  /mnt/"$ZPOOL"-altroot/boot
erun "Stubbing /boot/efi" stub_at_path /mnt/"$ZPOOL"-altroot/boot/efi
erun "Mounting /boot/efi" mount "$TARGET"1 /mnt/"$ZPOOL"-altroot/boot/efi
