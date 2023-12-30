#!/usr/bin/env bash
set -x

MOUNT="$MOUNT"
SUBVOLS=( home root persist persist/cache var/log gnu/store )
BTRFS_OPTS="defaults,compress=zstd,noatime"

# Pass -u to unmount.
if [[ ${1:-} != -u ]]; then
  # Decrypt
  cryptsetup open /dev/disk/by-id/ata-TSA_64GB_2019121400128-part1 cryptboot
  cryptsetup open /dev/disk/by-id/ata-TSA_64GB_2019121400128-part6 btrfs-rpool

  # Mount root subvolume
  mkdir -p $MOUNT
  mount -t btrfs -o $BTRFS_OPTS /dev/mapper/btrfs-rpool $MOUNT
  
  # Mount other subvolumes
  for SUBVOL in ${SUBVOLS[@]}; do
    mkdir -p $MOUNT/$SUBVOL
    mount -t btrfs -o $BTRFS_OPTS,subvol=${SUBVOL//\//-} /dev/mapper/btrfs-rpool $MOUNT/$SUBVOL
  done
  
  # Mount boot & efi partitions
  mkdir -p $MOUNT/boot
  mount /dev/mapper/cryptboot $MOUNT/boot
  mkdir -p $MOUNT/boot/EFI
  mount /dev/disk/by-id/ata-TSA_64GB_2019121400128-part2 $MOUNT/boot/EFI
else
  # Unmount boot & efi partitions
  umount -l $MOUNT/boot/EFI
  umount -l $MOUNT/boot
  
  # Unmount subvols
  for SUBVOL in ${SUBVOLS[@]}; do
    umount -l $MOUNT/${SUBVOL//\//-}
  done
  umount -l $MOUNT
  
  # Unmount decrypted volumes
  cryptsetup close cryptboot
  cryptsetup close btrfs-rpool

  rmdir $MOUNT
fi
