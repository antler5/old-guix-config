#|

ZFS on GuixSD

With the greatest of thanks to raid5atemyhomework:
- https://www.reddit.com/r/GUIX/comments/s7qu25/guide_using_zfs_on_guix/
- https://issues.guix.gnu.org/45692

|#

;;; ----- Modules -----
(define-module (antlers packages zfs)
  ;; Annotated
  #:use-module ((gnu services linux)
                #:select (kernel-module-loader-service-type))
  ;; Simple
  #:use-module (gnu packages file-systems)
  #:use-module (gnu services)
  #:use-module (guix packages)
  #:export (my-zfs-package
            my-zfs-loader-service))

;;; ----- Package & Service Definitions -----
(define (my-zfs-package kernel)
  (package
    (inherit zfs)
    (arguments
     (cons* #:linux kernel
            (package-arguments zfs)))))

(define my-zfs-loader-service
  (simple-service 'my-zfs-loader
                  kernel-module-loader-service-type
                  '("zfs")))
