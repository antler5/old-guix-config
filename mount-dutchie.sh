#!/usr/bin/env bash
set -euo pipefail

. ./scripts/utils.sh

TARGET="/dev/disk/by-id/ata-TSA_64GB_2019121400128"
ZPOOL="dutchie"

import-pool () {
  zpool status "$ZPOOL" || \
    zpool import -o altroot=/mnt/"$ZPOOL"-altroot "$ZPOOL"
}
zpool status "$ZPOOL" >& /dev/null || erun "Importing zpool $ZPOOL" import-pool

mount-root-dataset () {
  mkdir -p /mnt/"$ZPOOL"-altroot || true
  erun "Loading key" \
    zfs load-key -L file:///home/antlers/projects/guix/privkeys/"$ZPOOL"/system.key "$ZPOOL/guix-root" || true
  mount -t zfs "$ZPOOL"/guix-root /mnt/"$ZPOOL"-altroot
}
mountpoint -q -- /mnt/"$ZPOOL"-altroot || erun "Mounting root dataset" mount-root-dataset

mount-other-dataset () {
  local ds && ds="$1" && [[ -n "$ds" ]] || return 1
  mkdir -p /mnt/"$ZPOOL"-altroot/"${ds//-/\/}" || true
  erun "Loading key" \
    zfs load-key -L file:///home/antlers/projects/guix/privkeys/"$ZPOOL"/system.key "$ZPOOL/$ds" || true
  mount -t zfs "$ZPOOL/$ds" /mnt/"$ZPOOL"-altroot/"${ds//-/\/}"
}
einfo "Mounting datasets"; INDENT+="  "; {
  # TOOD: remove gnu-store
  for ds in gnu-store home var-log var-guix root persist persist-cache; do
    mountpoint -q -- /mnt/"$ZPOOL"-altroot/"${ds//-/\/}" \
      || erun /mnt/"$ZPOOL"-altroot/"${ds//-/\/}" mount-other-dataset "$ds"
  done
}; INDENT="${INDENT:2}"

open=( cryptsetup --key-file=./privkeys/"$ZPOOL"/system.key open )
unlock-cryptvol () {
  local vol && vol="$1" && [[ -n "$vol" ]] || return 1
  local dev && dev="$2" && [[ -n "$dev" ]] || return 1
  [[ -b /dev/mapper/"$ZPOOL-$vol" ]] || ${open[@]} "$dev" "$ZPOOL-$vol"
}
einfo "Unlocking cryptvols"; INDENT+="  "; {
  [[ -b "/dev/mapper/$ZPOOL-swap" ]] \
    || erun "/dev/mapper/$ZPOOL-swap" unlock-cryptvol swap "$(find-partition "$TARGET" 3)"
  [[ -b "/dev/mapper/$ZPOOL-keystore" ]] \
    || erun "/dev/mapper/$ZPOOL-keystore" unlock-cryptvol keystore /dev/zvol/"$ZPOOL"/keystore
}; INDENT="${INDENT:2}"

create-stub () {
  local STUB && STUB="$1" && [[ -n "$STUB" ]] || return 1
  local STUB_PATH && STUB_PATH="$2" && [[ -n "$STUB_PATH" ]] || return 1
  mkdir -p "$STUB_PATH" || true
  chmod 000 "$STUB_PATH"
  chattr +i "$STUB_PATH"
}
einfo "Creating stubs"; INDENT+="  "; {
  for STUB in proc sys tmp; do
    STUB_PATH=/mnt/"$ZPOOL"-altroot/$STUB
    mountpoint -q -- "$STUB_PATH" || {
      erun "$STUB_PATH" create-stub "$STUB" "$STUB_PATH"
      erun "Mounting tmpfs" mount -t tmpfs "$ZPOOL-${STUB//\//-}" "$STUB_PATH"
    }
  done
  STUB=boot
  STUB_PATH=/mnt/"$ZPOOL"-altroot/$STUB
  mountpoint -q -- "$STUB_PATH" \
    || erun "$STUB_PATH" create-stub "$STUB" "$STUB_PATH"
}; INDENT="${INDENT:2}"

mount-partition () {
  local src && src="$1" && [[ -b "$src" ]] || return 1
  local dst && dst="$2" && [[ -n "$dst" ]] || return 1
  mkdir -p "$dst" || true
  mount "$src" "$dst"
}
einfo "Mounting partitions"; INDENT+="  "; {
  mountpoint -q -- /run/keystore/"$ZPOOL" \
    || mount-partition /dev/mapper/"$ZPOOL"-keystore /run/keystore/"$ZPOOL"
  mountpoint -q -- /mnt/"$ZPOOL"-altroot/boot \
    || mount-partition $(find-partition "$TARGET" 2) /mnt/"$ZPOOL"-altroot/boot
}; INDENT="${INDENT:2}"

STUB_PATH=/mnt/"$ZPOOL"-altroot/boot/efi
mountpoint -q -- "$STUB_PATH" \
  || { ewrun "Stubbing $STUB_PATH" create-stub "$STUB" "$STUB_PATH" || true
       erun "Mounting $STUB_PATH" mount-partition $(find-partition "$TARGET" 1) /mnt/"$ZPOOL"-altroot/boot/efi; }
