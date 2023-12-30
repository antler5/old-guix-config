(define-module (antlers systems transformations zfs)
  #:use-module (antlers records)
  #:use-module (antlers systems transformations oot-modules)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:export (os-with-zfs))

(define (os-with-zfs parent)
  (let ((zfs (linux-module-with-kernel (operating-system-kernel parent) zfs)))
    (modify-record parent
      (kernel -> (kernel-with-oot-modules <> `(,#~#$zfs:module)))
      (initrd-modules => (cons "zfs" <>))
      (services => (append <> (list
        (simple-service 'zfs-mod-loader kernel-module-loader-service-type '("zfs"))
        (simple-service 'zfs-udev-rules udev-service-type `(,zfs)))))
      (packages -> (cons zfs <>))
      )))

