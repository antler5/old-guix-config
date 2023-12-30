#!/usr/bin/env bash
set -xeuo pipefail

rm -rf /tmp/antlers-guix || true
mkdir -p /tmp/antlers-guix/modules/antlers.tmp
cat > /tmp/antlers-guix/.guix-channel << "EOF"
(channel
 (version 0)
 (directory "modules"))
EOF

case "$(hostname)" in
  dutchie)
    cat > /tmp/antlers-guix/modules/antlers.tmp/variables.scm << "    EOF"
    (define-module (antlers.tmp variables))
    (define-public channels-root "/persist/assorted_upstreams")
    (define-public guix-root (string-append channels-root "/../guix-checkout"))
    (define-public guix-altroot #f)
    EOF
    ;;
  tower)
    cat > /tmp/antlers-guix/modules/antlers.tmp/variables.scm << "    EOF"
    (define-module (antlers.tmp variables))
    (define-public channels-root "/mnt/old_root/home/antlers/projects/assorted_upstreams")
    (define-public guix-root (string-append channels-root "/../guix"))
    (define-public guix-altroot "/mnt/dutchie-altroot")
    EOF
    ;;
esac

pushd /tmp/antlers-guix; {
  GIT_PREFIX=( env GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null GIT_COMMITTER_DATE="1970-01-01T00:00+00:00" )
  GIT_ARGS=( -c "user.name=antlers" -c "user.email=antlers@illucid.net" )
  GIT_COMMIT_ARGS=( --date="1970-01-01T00:00+00:00" )
  find . -exec touch --no-create -a -m -t 199001010000 \{} \;
  ${GIT_PREFIX[@]} git ${GIT_ARGS[@]} init
  ${GIT_PREFIX[@]} git ${GIT_ARGS[@]} add .
  ${GIT_PREFIX[@]} git ${GIT_ARGS[@]} commit ${GIT_COMMIT_ARGS[@]} -m init
}; popd

BUILD_ARGS=( --fallback --allow-downgrades )
export BUILD_ARGS

WITH_CHANNELS=( guix time-machine --disable-authentication --channels=./channels.scm -L /tmp/antlers-guix/modules -- )
WITH_CHANNELS_LATEST=( guix time-machine --disable-authentication --channels=./channels-latest.scm -L /tmp/antlers-guix/modules -- )

(return 0 2>/dev/null) && sourced=1 || sourced=0
if [[ $sourced == 0 ]]; then

  SYSTEM="$2"
  export SYSTEM
  
  function current-commit () {
    guix repl -L ./modules /dev/stdin "${1:-TODO_RENAME_ME_IN_JUSTFILE}" "${2:-"./channels.scm"}" << "    EOF"
    (use-modules (guix channels)
                 (srfi srfi-1))
    (let ((target (string->symbol (second (command-line))))
          (file (third (command-line))))
      (display
        (channel-commit
          (or (find (lambda (c) (equal? (channel-name c) target))
                    (primitive-load file))
              (error "channel not found in file:" target file)))))
    EOF
  }
  export -f current-commit
  
  case $1 in
    update)
      # Initially tower-specific, just to get all the steps together.
      case "$SYSTEM" in
        tower)
          # TODO: Doesnt seem to pull correctly...
          git --work-tree="../assorted_upstreams/guix" --git-dir="../assorted_upstreams/guix/.git" pull --all
          git --work-tree="../assorted_upstreams/nonguix" --git-dir="../assorted_upstreams/nonguix/.git" pull --all
          ${WITH_CHANNELS_LATEST[@]} shell -- bash << "          EOF"
          set -xeuo pipefail
          if guix system build ${BUILD_ARGS[@]} ./modules/antlers/systems/"$SYSTEM".scm; then
            CURRENT_GUIX_COMMIT="$(current-commit guix)"
            CURRENT_NONGUIX_COMMIT="$(current-commit nonguix)"
            LATEST_GUIX_COMMIT="$(current-commit guix <(guix describe --format=channels))"
            LATEST_NONGUIX_COMMIT="$(current-commit nonguix <(guix describe --format=channels))"
            sed --follow-symlinks --in-place \
              -e "s/$CURRENT_GUIX_COMMIT/$LATEST_GUIX_COMMIT/g" \
              -e "s/$CURRENT_NONGUIX_COMMIT/$LATEST_NONGUIX_COMMIT/g" \
              ./channels.scm
            :;
          fi
          EOF
          ./just reconfigure $SYSTEM
          ./just install-bootloader $SYSTEM
          ;;
        dutchie)
          git push dutchie
          script() {
            cat << "            EOF"
            set -euo pipefail
            cd "$GUIX_ROOT"
            git pull --ff-only
            sudo ./just reconfigure dutchie
            EOF
          }
          ssh -t -p 220 antlers@192.168.0.124 bash -c "$(script)"
      esac
      ;;
    build)
      ${WITH_CHANNELS[@]} system build ${BUILD_ARGS[@]} ./modules/antlers/systems/"$SYSTEM".scm
      ;;
    reconfigure)
      ${WITH_CHANNELS[@]} system reconfigure ${BUILD_ARGS[@]} ./modules/antlers/systems/"$SYSTEM".scm
      ./just install-bootloader "$SYSTEM"
      ;;
    create-installer)
      ${WITH_CHANNELS[@]} system image -t iso9660 ${BUILD_ARGS[@]} -e "((@ (antlers systems installer) operating-system-installer) (@ (antlers systems $SYSTEM) %system))"
      ;;
    install-bootloader)
      case "$SYSTEM" in
        tower)
          ROOT_UUID="5baa097b-94a8-436d-a2d8-f26130dc2472"
          # BOOT_UUID="$ROOT_UUID"
          BOOT_UUID="e55e23ea-47d2-42b2-90c6-150a49210bd5"
          EFI_UUID="2392-5054"
          # KEYFILE="./privkeys/tower/system.key.gpg"
          ;;
        dutchie)
          ROOT_UUID="96c969c0427444898452da2fe00454ee"
          BOOT_UUID="$(blkid -s UUID -o value /dev/disk/by-id/ata-TSA_64GB_2019121400128-part2)"
          EFI_UUID="$(blkid -s UUID -o value /dev/disk/by-id/ata-TSA_64GB_2019121400128-part2)"
          KEYFILE="./privkeys/dutchie/system.key.gpg"
          ;;
      esac
      ./install-bootloader.sh /boot "$ROOT_UUID" "$BOOT_UUID" "${KEYFILE:-}"
      # TODO: Check keys ahead of time and abort if not prepared to sign?
      ./wip-secure-boot-grub-efi/grub-install-standalone.sh /boot "$EFI_UUID"
      ./wip-secure-boot-grub-efi/grub-sign-initramfs.sh /boot /boot/efi
      ;;
  esac

fi
