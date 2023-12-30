(define-module (antlers systems transformations yubi)
  #:use-module (antlers records)
  #:use-module (gnu packages)
  #:use-module (gnu services base)
  #:use-module (gnu services security-token)
  #:use-module (gnu services)
  #:use-module (gnu system accounts)
  #:use-module (gnu system)
  #:export (os-with-yubi))

(define (os-with-yubi parent)
  (modify-record parent
    (groups -> (cons (user-group (name "plugdev")) <>))
    (users  -> (map (lambda (user)
                      (if (member (user-account-name user)
                                  '("antlers"))
                          (modify-record user
                            (supplementary-groups -> (cons "plugdev" <>)))
                          user))
                    <>))
    (services => (append <> (list
      (service pcscd-service-type)
      (simple-service 'u2f-udev-rules udev-service-type
                      (list (specification->package "libu2f-host")))
      (simple-service 'yubi-udev-rules udev-service-type
                      (list (specification->package "yubikey-personalization"))))))))
