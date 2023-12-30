(define-module (antlers systems transformations nvidia)
  #:use-module ((srfi srfi-1) #:select (find remove))
  #:use-module (antlers records)
  #:use-module (antlers systems transformations oot-modules)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix transformations)
  #:use-module (nongnu packages nvidia)
  #:export (os-with-nvidia))

(use-service-modules base linux sddm xorg)

(define sddm-configuration-xorg
  (@@ (gnu services sddm) sddm-configuration-xorg))
(define gdm-configuration-xorg
  (@@ (gnu services xorg) gdm-configuration-xorg))

(define (os-with-nvidia parent)
  (let ((nvidia-module (linux-module-with-kernel
                         (operating-system-kernel parent)
                         nvidia-module)))
    (modify-record parent
      (firmware -> (cons nvidia-firmware <>))
      ;; XXX: Clears any existing `modprobe.blacklist`.
      (kernel-arguments ->
        (cons* (string-append
                 "modprobe.blacklist="
                 (string-join `("nouveau"
                                ,@(@@ (gnu system) %default-modprobe-blacklist))
                              ","))
               "nvidia-drm.modeset=1"
               (remove (lambda (args)
                         (or (string-contains args "modprobe.blacklist")
                             (string-contains args "nvidia-drm.modeset")))
                       <>)))
      ;; The initramfs builder looks for modules in the provided kernel
      ;; profile, so we'll just put them in there.
      (kernel -> (kernel-with-oot-modules <> `(,nvidia-module)))
      (initrd-modules => (append <> (list
        "nvidia"
        "nvidia_modeset"
        "nvidia_uvm"
        "nvidia_drm")))
      ;; XXX: There's probably a better way to eg. catch the raised conditions here.
      (services =>
        ((lambda (services)
           (when (find (lambda (s)
                     (eq? (service-kind s) gdm-service-type))
                   services)
             (set! services
               (modify-services <>
                 (gdm-service-type
                   config => (modify-record config
                               (xorg-configuration ->
                                 (modify-record <>
                                   (modules => (cons nvidia-driver <>))
                                   (server  -> ((options->transformation '((with-graft . "mesa=nvda"))) <>))
                                   (drivers -> (cons "nvidia" <>)))))))))
           services)
         <>))
      (services =>
        ((lambda (services)
           (when (find (lambda (s)
                     (eq? (service-kind s) sddm-service-type))
                   services)
             (set! services
               (modify-services <>
                 (sddm-service-type
                   config => (modify-record config
                               (xorg-configuration ->
                                 (modify-record <>
                                   (modules => (cons nvidia-driver <>))
                                   (server  -> ((options->transformation '((with-graft . "mesa=nvda"))) <>))
                                   (drivers -> (cons "nvidia" <>)))))))))
           services)
         <>))
      (services => (append <> (list
        (simple-service 'nvidia-udev-rules udev-service-type `(,nvidia-driver))
        ;; Here's a question: why just these modules? And why ipmi?
        (service kernel-module-loader-service-type
          '("ipmi_devintf"
            "nvidia"
            "nvidia_modeset"
            "nvidia_uvm")))))
      (packages -> (cons nvidia-driver <>)))))
