#!/usr/bin/env -S guix time-machine --channels=./channels.scm -L /tmp/antlers-guix/modules --disable-authentication -- shell bash gnupg -- bash --
# TODO: Only decrypt key when needed
set -euo pipefail

. ./scripts/utils.sh
export PATH

GPG_KEY='grub-cfg'
BOOT_DIR="$1"          # source, no trailing /
EFI_DIR="${2:-$1/efi}" # target, no trailing /
erun "Confirming boot directory exists" test -e "$BOOT_DIR"

TMPDIR="$(mktemp -d)"
function cleanup { erun "Cleaning tmpdir" rm -rf "$$TMPDIR"; }
trap cleanup EXIT
create-tmp-dir () {
  chmod 700 "$TMPDIR"
}
# XXX: Misnomer bc of var scoping and subshell issues
erun "Creating tmpdir" create-tmp-dir

GPG_FLAGS=( --homedir "$(realpath ./efikeys/gnupg)" --pinentry-mode loopback --default-key "$GPG_KEY" )
GPGV_FLAGS=( --homedir "$(realpath ./efikeys/gnupg)" --verbose )

# Mark our key as trusted:
# https://blog.oddbit.com/post/2020-10-05-a-note-about-running-gpgv/

# Touch the keyring fist:
# https://superuser.com/a/1641496
# We'll re-create it each time, to be sure we've got just our intended key.
creating-keyring () {
  if [[ -f "$(realpath ./efikeys/gnupg)"/trustedkeys.kbx ]]; then
    rm -rf "$(realpath ./efikeys/gnupg)"/trustedkeys.kbx
  fi
  touch "$(realpath ./efikeys/gnupg)"/trustedkeys.kbx
  gpg ${GPG_FLAGS[@]} --export "$GPG_KEY" \
    | gpg ${GPG_FLAGS[@]} \
        --no-default-keyring \
        --keyring "$(realpath ./efikeys/gnupg)"/trustedkeys.kbx \
        --import
}
erun "Creating keyring" creating-keyring

einfo "Ensuring /boot files are signed"
INDENT+="  "
function gpg_sign {
  local PRINT_PATH
  [[ $1 =~ ^\./gnu/store/ ]] \
    && PRINT_PATH="$(pretty-print-store-item 1 52 "$1")" \
    || PRINT_PATH="$1"
  if [[ ! -f "$1".sig ]] \
     || ! { ewrun "Verifying \"$PRINT_PATH\"" \
              gpgv ${GPGV_FLAGS[@]} "$1".sig "$1" \
              || { rm "$1".sig && false; }
            } 2> /dev/null
  then
    INDENT+="  "
    erun "Signing \"$PRINT_PATH\"" gpg ${GPG_FLAGS[@]} --detach-sign "$1"
    INDENT="${INDENT:2}"
  fi
}
for x in \
  $(grep -E '^  (linux|initrd) ' "$BOOT_DIR"/grub/grub.cfg | awk '{ print $2 }' | sort | uniq) \
  $(grep -Eo '/gnu.*(-grub-locales|-grub-image.png)' "$BOOT_DIR"/grub/grub.cfg | sort | uniq)
do
  pushd "$BOOT_DIR" > /dev/null; {
    if [[ -d "$x" ]]; then
      for y in $(find $x -type f); do gpg_sign ./"${y#/}"; done
    else
      gpg_sign ./"${x#/}"
    fi
  }; popd > /dev/null
done
INDENT="${INDENT:2}"

update-grubcfg () {
  [[ -d "$EFI_DIR/grub" ]] || mkdir -p "$EFI_DIR/grub"
  sed -E \
    -e 's/(^menuentry.*)( \{)/\1 --unrestricted\2/' \
    /boot/grub/grub.cfg \
    > "$EFI_DIR"/grub/grub.cfg
}
erun "Updating grubcfg" update-grubcfg
gpg_sign "$EFI_DIR"/grub/grub.cfg
