#!/usr/bin/env bash
set -euo pipefail

. just

${WITH_CHANNELS[@]} system -L ./modules init ./modules/antlers/systems/dutchie.scm /mnt/dutchie-altroot
./install-bootloader.sh /mnt/dutchie-altroot/boot "" "" ./privkeys/dutchie/system.key.gpg
