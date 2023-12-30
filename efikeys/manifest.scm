(use-modules (guix profiles))

(concatenate-manifests
 (list
  (specifications->manifest
   '("efitools"
     "gnupg"
     "grub-efi"
     "openssl"
     "sbsigntools"))))
