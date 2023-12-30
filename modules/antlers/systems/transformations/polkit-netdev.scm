;;; "ConsoleKit in Debian 7.0 does not consider sessions started using startx
;;; or display managers lacking consolekit integration (e.g. xdm or slim) as
;;; local, which might prevent access to some devices."

;; Apparently not an issue on Tower? But I need this on my other devices, so
;; into base.scm it goes.

(define-module (antlers systems transformations polkit-netdev)
  #:use-module (antlers records)
  #:use-module (gnu services dbus)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:export (os-with-polkit-netdev))

(define polkit-netdev
  (file-union
   "polkit-netdev"
   `(("share/polkit-1/rules.d/netdev.rules"
      ,(plain-file
        "netdev.rules"
        "\
polkit.addRule(function(action, subject) {
  if (action.id.indexOf(\"org.freedesktop.NetworkManager.\") == 0 && subject.isInGroup(\"netdev\")) {
    return polkit.Result.YES;
  }
});\n")))))

(define (os-with-polkit-netdev parent)
  (modify-record parent
    (services => (append <> (list
      (simple-service 'polkit-netdev polkit-service-type
                      (list polkit-netdev)))))))
