#| AP6256 Firmware (PBP WiFi) |#

;;; ----- Modules -----
(define-module (antlers packages ap6256-firmware)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)
  #:use-module (nonguix licenses)
  #:export (ap6256-firmware))

;;; ----- Variables -----
(define ap6256-gitlab-repo
  "https://gitlab.manjaro.org/manjaro-arm/packages/community/ap6256-firmware")

;;; ----- Package Definitions -----
(define manjaro-ap6256-firmware
  (origin
    (method git-fetch)
    (uri (git-reference
          (url (string-append ap6256-gitlab-repo ".git"))
          (commit "910f0df6c215515fbf5c246d119ebc6f638a08fb")))
    (sha256
     (base32
      "0ld686b5656v0q6va4l5jwyx9bryjzisl956vcmhxq49cq3450al"))))

(define ap6256-firmware
  (package
    (name "ap6256-firmware")
    (version "2020.02")
    (source manjaro-ap6256-firmware)
    (synopsis "Firmware files for the ap6256 wifi/bt module")
    (description "Firmware files for the ap6256 wifi/bt module")
    (license (nonfree "about:blank"))
    (home-page ap6256-gitlab-repo)
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       `(("BCM4345C5.hcd" "lib/firmware/brcm/BCM.hcd")
         ("fw_bcm43456c5_ag.bin" "lib/firmware/brcm/brcmfmac43456-sdio.bin")
         ("." "lib/firmware/" #:include ("BCM4345C5.hcd" "nvram_ap6256.txt"))
         ("." "lib/firmware/brcm/" #:include ("BCM4345C5.hcd" "brcmfmac43456-sdio.clm_blob"))
         ;; TODO: What are these /for/?
         ;; https://github.com/Thra11/nixpkgs-overlays-rk3399/blob/21e3cf472e8beb4e5d271fa0fc7a8b97e9822241/pkgs/ap6256-firmware/default.nix#L17-L18
         ,@(map (lambda (target) (list "nvram_ap6256.txt" target))
                (list "lib/firmware/brcm/brcmfmac43456-sdio.radxa,rockpi4.txt"
                      "lib/firmware/brcm/brcmfmac43456-sdio.pine64,pinebook-pro.txt")))))))

ap6256-firmware
