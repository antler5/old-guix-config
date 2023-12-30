#!/usr/bin/env -S guix shell cpio grub-efi -- bash

# Pulls the initramfs referenced in BOOT_DIR/grub/grub.cfg into BOOT_DIR,
# inserting KEYFILE into the CPIO archive via concatenation if specified.

# Only supports one KEYFILE ATM because that's all I need for my setup.

# Based on Maxim Cournoyer's [example](https://issues.guix.gnu.org/48172#4).

#\ Usage: ./install_initramfs.sh BOOT_DIR [KEYFILE]
[[ "${@#* }" == "-h"* ]] || [[ "${@#* }" == "--h"* ]] \
  && { grep --color=never -Po '(?<=#\\ ).*' "$BASH_SOURCE"; exit 0; }

. ./scripts/utils.sh
set -euo pipefail

BOOT_DIR="$1" && [[ -d $BOOT_DIR ]]
ROOT_UUID="$2"
BOOT_UUID="${3:-}"
KEYFILE="${4:-}"

if [[ -n $KEYFILE ]]; then
  [[ -e $KEYFILE ]]
  KEYFILE="$(realpath $KEYFILE)"
fi

umask 0077

tmpdir="$(mktemp -d)"
function cleanup { rm -rf "$tmpdir"; }
trap cleanup EXIT

# Copy kernel and initrd images to BOOT_DIR
images=$(grep -E '^  (linux|initrd) ' "$BOOT_DIR"/grub/grub.cfg \
  | awk '{ print $2 }' | sed 's|.*/gnu|/gnu|g' | sort | uniq)

cpio_flags=(--quiet --create --format=newc --no-absolute-filenames)
insert_keyfile () {
  # Call ONLY in tmpdir.
  local img && img="$1" && [[ -n "$img" ]] || return 1
  cp -f "$KEYFILE" ./"$(basename $KEYFILE)"
  if [[ ./"$img" == *".gz" ]]; then
    # Compressed archives don't need to be aligned
    echo ./"$(basename $KEYFILE)" | cpio ${cpio_flags[@]} | gzip >> ./"$img"
  else
    # Here we prepend, to ensure that *we're* aligned
    echo ./"$(basename $KEYFILE)" | cpio ${cpio_flags[@]} >> ./"$img".tmp
    cat ./"$img" >> ./"$img".tmp
    rm ./"$img"
    mv ./"$img".tmp ./"$img"
  fi
}
pushd "$tmpdir" > /dev/null; {
  for image in $images; do
    mkdir -p ./$(dirname $image) || true
    PRETTY_ITEM="$(pretty-print-store-item 0 27 "/gnu/store/$(basename $(dirname $image))/$(basename $image)")"
    PRETTY_IMAGE="$(pretty-print-store-item -10 23 "$image")"
    erun "copying $PRETTY_ITEM to .$PRETTY_IMAGE" \
      cp /gnu/store/"$(basename $(dirname $image))"/"$(basename $image)" ./"$image"
    if [[ -n $KEYFILE ]] && [[ $image == *"initrd"* ]]; then
      erun "inserting $(basename $KEYFILE) into .$PRETTY_IMAGE" \
        insert_keyfile "$image"
    fi
    if [[ ! -e ${BOOT_DIR}/$image ]] || ! cmp -s ./"$image" "${BOOT_DIR}/$image"; then
      mkdir -p "$(dirname ${BOOT_DIR}/$image)" || true
      erun "copying $PRETTY_IMAGE to ${BOOT_DIR}$PRETTY_IMAGE..." \
        mv ./"$image" "${BOOT_DIR}/$image"
    fi
  done
}; popd > /dev/null

einfo "Don't forget the decor!"
for suffix in "-grub-image.png" "-grub-locales"; do
  path="$(grep -o '[^ ]*'"$suffix" "$BOOT_DIR"/grub/grub.cfg | head -n 1)"
  path="${path:1}"
  [[ -f "${BOOT_DIR}/$path" ]] \
    || erun "Copying \"$(pretty-print-store-item 0 35 /gnu/store/$(basename $path))\" to \"${BOOT_DIR}/$(pretty-print-store-item -10 35 $(basename $path))\"" cp -r /gnu/store/$(basename $path) "${BOOT_DIR}/$path"
done

# Adjust BOOT_DIR/grub/grub.cfg
# 2nd sub: `btrfs-rpool' LUKS-UUID -> `cryptboot' LUKS-UUID
# TODO 3rd sub: target signed files
if [[ -n "$ROOT_UUID" ]] && [[ -n "$BOOT_UUID" ]]; then
  einfo "Updating grub.cfg"
  sed --in-place --regexp-extended \
      -e 's,^(  (linux|initrd) )[^ ]*(/gnu/[^ ]* ?),\1\3,' \
      -e 's,(^\s*search .*)'"$ROOT_UUID"',\1'"$BOOT_UUID"',' \
      "$BOOT_DIR"/grub/grub.cfg
fi

export GRUB_ENABLE_CRYPTODISK=y
erun "Re-installing GRUB" \
  grub-install --boot-directory "$BOOT_DIR" --bootloader-id=Guix --efi-directory "$BOOT_DIR"/efi --removable
