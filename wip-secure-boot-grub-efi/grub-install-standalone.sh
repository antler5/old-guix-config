#!/usr/bin/env -S guix time-machine --channels=./channels-default.scm --commit=cacc0cb6ab22218a3783a51ba9986405ede4e0d8 -- shell bash gnupg grub-efi sbsigntools expect -- bash --

set -euo pipefail

. ./scripts/utils.sh

BOOTDIR="$1" && [[ -e $BOOTDIR ]]
EFI_UUID="$2" && [[ -n $EFI_UUID ]]
GPG_KEY='grub-cfg'
TARGET_EFI="$BOOTDIR"'/efi/EFI/boot/bootx64.efi'
SECUREBOOT_DB_KEY='./efikeys/artifacts/db.key'
SECUREBOOT_DB_CRT='./efikeys/artifacts/db.crt'

# GRUB doesn't allow loading new modules from disk when secure boot is in
# effect, therefore pre-load the required modules.
MODULES=""
MODULES="$MODULES part_gpt fat ext2"           # partition and file systems for EFI
MODULES="$MODULES configfile"                  # source command
# Gotta' have a verifier
# https://forums.gentoo.org/viewtopic-p-8695155.html?sid=b5339783ec40d47b3e7af3dd1bd11120
# Got a hunch that I need TPM even tho I'm not using the TPM...
MODULES="$MODULES pgp tpm" # signature verification
# Missing 'verify'
# MODULES="$MODULES verify gcry_sha512 gcry_rsa" # signature verification
MODULES="$MODULES gcry_sha512 gcry_rsa" # signature verification
MODULES="$MODULES password_pbkdf2"             # hashed password
# Missing 'linuxefi'
# MODULES="$MODULES echo normal linux linuxefi"  # boot linux
MODULES="$MODULES echo normal linux"  # boot linux
MODULES="$MODULES all_video gfxterm"           # video output
MODULES="$MODULES search search_fs_uuid"       # search --fs-uuid
MODULES="$MODULES reboot sleep"                # sleep, reboot
MODULES="$MODULES png"                         # decor

read-encryption-passphrase

TMPDIR="$(mktemp -d)"
function cleanup { erun "Cleaning tmpdir" rm -rf "$TMPDIR"; }
trap cleanup EXIT

TMP_GPG_KEY="$TMPDIR"/gpg.key
TMP_GRUB_CFG="$TMPDIR"/grub-initial.cfg
TMP_GRUB_SIG="$TMP_GRUB_CFG.sig"
TMP_GRUB_EFI="$TMPDIR"/grubx64.efi

GPG_FLAGS=( --homedir "$(realpath ./efikeys/gnupg)" --pinentry-mode loopback --default-key "$GPG_KEY" )

create-gpg-homedir () {
  set -x # TMP
  mkdir -p ./efikeys/gnupg
  if ! gpg ${GPG_FLAGS[@]} --list-keys "$GPG_KEY"; then
    gpg ${GPG_FLAGS[@]} --batch --quick-gen-key --passphrase '' "$GPG_KEY"
  fi
}
erun "Ensuring GPG homedir exists" create-gpg-homedir

# XXX: Kinda a misnomer because of variable scoping / subshell issues.
create-tmp-dir () {
  chmod 700 "$TMPDIR"
  gpg ${GPG_FLAGS[@]} --export "$GPG_KEY" >"$TMP_GPG_KEY"
  sed 's,{{ EFI_UUID }},'"$EFI_UUID"',' \
      ./efikeys/grub-initial.cfg \
      > "$TMP_GRUB_CFG"
  expect-passphrase-prompt "Enter passphrase: " \
    gpg ${GPG_FLAGS[@]} --detach-sign "$TMP_GRUB_CFG"
}
erun "Creating tmpdir" create-tmp-dir

# Find grub modules
if [[ -d /lib/grub/x86_64-efi ]]; then
  GRUB_DIRECTORY="/lib/grub"
elif [[ -d "$GUIX_ENVIRONMENT"/lib/grub/x86_64-efi ]]; then
  GRUB_DIRECTORY="$GUIX_ENVIRONMENT/lib/grub"
fi
einfo "Located grub at \"$(pretty-print-store-item 0 52 "$GRUB_DIRECTORY")\""

# XXX: On Fedora this was called grub2-...
if command -V grub2-mkstandalone &> /dev/null; then
  alias grub-mkstandalone="grub2-mkstandalone"
fi

erun "Creating standalone GRUB" \
  grub-mkstandalone \
      --directory "$GRUB_DIRECTORY"/x86_64-efi \
      --format x86_64-efi \
      --modules "$MODULES" \
      --pubkey "$TMP_GPG_KEY" \
      --output "$TMP_GRUB_EFI" \
      --disable-shim-lock \
      "boot/grub/grub.cfg=$TMP_GRUB_CFG" \
      "boot/grub/grub.cfg.sig=$TMP_GRUB_SIG"

erun "Signing standalone GRUB" \
  sbsign --key "$SECUREBOOT_DB_KEY" \
         --cert "$SECUREBOOT_DB_CRT" \
         --output "$TMP_GRUB_EFI" \
         "$TMP_GRUB_EFI"

write-grub-efi () {
  mkdir -p "$(dirname "$TARGET_EFI")"
  cp -f "$TMP_GRUB_EFI" "$TARGET_EFI"
}
erun "Writing signed grub.efi to '$TARGET_EFI'" write-grub-efi
