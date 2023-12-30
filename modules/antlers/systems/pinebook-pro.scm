(define-module (antlers systems pinebook-pro)
  #:use-module (antlers packages ap6256-firmware)
  #:use-module (antlers systems base)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu bootloader)
  #:use-module (gnu image)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix platforms arm)
  #:use-module (guix transformations)
  #:use-module (guix utils)
  #:use-module (nongnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(use-service-modules networking)
(use-system-modules image)
(use-package-modules xfce)

(let ((og thunar))
  (set! thunar
    (package
      (inherit thunar)
      (inputs
        (filter (lambda (package)
                  (let ((pkg (if (package? package) package (cadr package))))
                    (not (member (package-name pkg) (list "gvfs")))))
                (package-inputs og))))))

(define (without-tests spec)
  (let ((orig (specification->package spec)))
    (cons orig
          (package
            (inherit orig)
            (arguments
              (substitute-keyword-arguments (package-arguments orig)
                ((#:tests? _ #f) #f)))))))

(define (without-broken-aarch64-tests package)
  (let ((funct
         (package-input-rewriting
           (map without-tests
                (list "libnma")))))
    (if (package? package)
        (funct package)
        (cons (funct (car package)) (cdr package)))))

(define %system
  (let ((parent %graphical-system))
    (operating-system
      (inherit parent)
      (host-name "pinebook-pro")
      (kernel linux-arm64-generic)
      (bootloader (bootloader-configuration
                   (bootloader u-boot-pinebook-pro-rk3399-bootloader)
                   (targets '("/dev/vda"))))
      (file-systems (append (list
                              (file-system
                                (mount-point "/")
                                (device (file-system-label "rootfs"))
                                (type "ext4")))
                            %base-tmpfs-list
                            %base-file-systems))
      (firmware (cons ap6256-firmware
                      (operating-system-firmware parent)))
      (services
        (cons* (service agetty-service-type
                        (agetty-configuration
                          (extra-options '("--local-line"))
                          (baud-rate "1500000")
                          (term "vt100")
                          (tty "ttyS2")))
               (operating-system-user-services parent)))
      ;; (packages
      ;;   (filter (lambda (package)
      ;;             (let ((pkg (if (package? package) package (car package))))
      ;;               (and (not (member (package-name pkg) (list "firefox")))
      ;;                    (not (string-contains (package-name pkg) "emacs")))))
      ;;           (map without-broken-aarch64-tests
      ;;                (operating-system-packages parent))))
      (packages
        (filter (lambda (package)
                  (let ((pkg (if (package? package) package (car package))))
                    (not (string-contains (package-name pkg) "ffmpeg"))))
                (map without-broken-aarch64-tests
                     (operating-system-packages parent))))
      )))

(define pinebook-pro-image-type
  (image-type
   (name 'pinebook-pro-raw)
   (constructor (cut image-with-os
                     (raw-with-offset-disk-image
                      (* 9 (expt 2 20))) ;9MiB
                     <>))))

(define pinebook-pro-raw-image
  (image
   (inherit
    (os+platform->image %system
                        aarch64-linux
                        #:type pinebook-pro-image-type))
   (name 'pinebook-pro-raw-image)))

pinebook-pro-raw-image
