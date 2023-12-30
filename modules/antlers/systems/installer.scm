(define-module (antlers systems installer)
  #:use-module (antlers systems base)
  #:use-module (antlers systems transformations zfs)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (guix packages)
  #:export (operating-system-installer))

(define* (operating-system-installer os #:optional base-os)
  (set! base-os (or base-os %base-system))
  ((compose
     ;; ZFS shouldn't really be specified here,
     ;; but somehow inherited from OS or BASE-OS...
     os-with-zfs)
   (operating-system
     (inherit installation-os)
     (timezone (operating-system-timezone base-os))
     (locale (operating-system-locale base-os))
     (kernel (operating-system-kernel base-os))
     (initrd (operating-system-initrd base-os))
     (initrd-modules (operating-system-initrd-modules base-os))
     (label (string-append "Antlers' GNU Guix Installer "
                           (or (getenv "GUIX_DISPLAYED_VERSION")
                               (package-version (specification->package "guix")))))
     (services
       (modify-services
         (operating-system-user-services installation-os)
         (gc-root-service-type config => (cons os config)))))))
