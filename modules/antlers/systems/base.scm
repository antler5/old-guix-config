(define-module (antlers systems base)
  ;; Annotated
  #:use-module ((gnu system accounts)
                #:select (user-account))
  #:use-module ((gnu system shadow)
                #:select (%base-user-accounts))
  ;; Simple
  #:use-module (antlers records)
  #:use-module (antlers systems transformations polkit-netdev)
  #:use-module (gnu system)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix records)
  #:use-module (ice-9 string-fun)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%bare-system
            %base-system
            %base-tmpfs-list
            %graphical-system))

(use-package-modules linux
                     haskell-apps) ; kmonad
(use-service-modules desktop
                     networking
                     sddm
                     security
                     ssh
                     xorg)

(define dummy-bootloader
  (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))))

(define %base-tmpfs-list
  (list (file-system
          (mount-point "/tmp")
          (device "tmpfs")
          (type "tmpfs")
          (flags '(no-dev no-suid)))))

(define dummy-file-systems
  (append (list (file-system
                  (mount-point "/")
                  (device "/dev/vda1")
                  (type "ext4")))
          %base-tmpfs-list
          %base-file-systems))

(define base-packages-blacklist
  ;; 'base-packages' that I don't think I need
  '(;; Editors
    "mg" "nano"
    ;; File-System Utils
    "f2fs-tools"
    "xfsprogs"
    ;; Misc
    "wireless-tools"))

(define %bare-system
  (operating-system
    (host-name "bare-system")
    (locale "en_US.utf8")
    (timezone "America/Los_Angeles")
    (kernel linux-libre)
    (bootloader dummy-bootloader)
    (file-systems dummy-file-systems)
    (users (cons* (user-account
                    (name "antlers")
                    (comment "antlers")
                    (uid 1000)
                    (group "users")
                    (home-directory "/home/antlers")
                    (supplementary-groups
                      '("audio" "netdev" "video" "wheel")))
                  (user-account
                    (name "madden")
                    (comment "madden")
                    (uid 1001)
                    (group "users")
                    (home-directory "/home/madden")
                    (create-home-directory? #f))
                  (user-account
                    (name "guix-offload")
                    (group "nogroup")
                    (home-directory "/var/empty")
                    (system? #t))
                  %base-user-accounts))
    (services (cons (service openssh-service-type
                      (openssh-configuration
                       (port-number 220)
                       (password-authentication? #f)
                       (permit-root-login 'prohibit-password)
                       (authorized-keys
                        `(("antlers" ,(local-file "../../../pubkeys/ssh/antlers.pub"))
                          ("guix-offload" ,(local-file "../../../pubkeys/ssh/guix-offload.pub"))))))
                    %base-services))
    (packages (append (map specification->package
                           '("nss-certs"))
                      ;; Removes package that I don't think I need.
                      (remove (lambda (package)
                                (member (package-name package)
                                        base-packages-blacklist))
                              %base-packages)))))

(define iptables-rules-prefix
  (plain-file "iptables.rules"
              (string-join
                `("*filter"
                  ":INPUT ACCEPT"
                  ":FORWARD ACCEPT"
                  ":OUTPUT ACCEPT"
                  ;; Allow WG-YGG subnet forwarding
                  "-A FORWARD -i tower  -o enp4s0 -j ACCEPT"
                  "-A FORWARD -i tower  -o tun0   -j ACCEPT"
                  "-A FORWARD -i tun0   -o tower  -j ACCEPT"
                  ;; General Rules
                  "-A INPUT -i lo     -j ACCEPT"
                  "-A INPUT -m conntrack --ctstate INVALID -j DROP"
                  "-A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT"
                  ;; General ICMP Rules
                  ;; Inspired by https://gist.github.com/jirutka/3742890
                  ,@(map (cut string-append "-4 -A INPUT -p icmp --icmp-type " <> " -m conntrack --ctstate NEW -j ACCEPT")
                         (map number->string '(0 3 11)))
                  ,@(map (cut string-append "-6 -A INPUT -p ipv6-icmp --icmpv6-type " <> " -j ACCEPT")
                         (map number->string '(1 2 3 4 133 134 135 136 137 141 142 148 149)))
                  ,@(map (cut string-append "-6 -A INPUT -s fe80::/10 -p ipv6-icmp --icmpv6-type " <> " -j ACCEPT")
                         (map number->string '(130 131 132 143 151 152 153)))
                  ,@(map (cut string-append <> " -m hashlimit --hashlimit-name PING --hashlimit-mode srcip --hashlimit-upto 3/sec -j ACCEPT")
                         '("-4 -A INPUT -p icmp --icmp-type 8  -m conntrack --ctstate NEW"
                           "-6 -A INPUT -p ipv6-icmp --icmpv6-type 128"))
                  ;; Services
                  ,@(let ((spec->rule (lambda* (proto port #:optional v netmask)
                                        (string-join `(,@(if v `(,(string-append "-" (number->string v))) '())
                                                       "--append INPUT"
                                                       ,@(if netmask `("--source" ,netmask) '())
                                                       "--protocol" ,proto
                                                       "--dport" ,(number->string port)
                                                       "--jump ACCEPT")
                                                     " "))))
                        ;; Local
                      `(,@(append-map (lambda (proto-port)
                                        (map (lambda (v-netmask) (apply spec->rule `(,@proto-port ,@v-netmask)))
                                             '((4 "192.168.0.0/24") (4 "10.0.0.0/24") (6 "fe80::/10"))))
                            '(("tcp" 65000)     ; yggdrasil (local)
                              ("tcp" 8000)      ; archivebox XXX: dutchie only
                              ("tcp" 445)       ; samba
                              ("tcp" 2049)      ; nfs
                              ))
                        ;; Remote
                        ,@(map (lambda (proto-port) (apply spec->rule proto-port))
                            '(("tcp" 220)       ; ssh
                              ("udp" 60220)     ; mosh
                              ("udp" 60820))))) ; wireguard
                  ;; Yggdrasil (remote: rate-limited)
                  ,(string-join '("-A INPUT -p tcp --dport 65000"
                                  "-m hashlimit --hashlimit-name YGG --hashlimit-upto 768kb/s --hashlimit-burst 1mb"
                                  "-j ACCEPT") " ")
                  ;; Drop / reject everything else
                  "-A INPUT -j DROP"
                  "-A FORWARD -j DROP"
                  "COMMIT")
                "\n" 'suffix)))

;; (define iptables-rules-suffix
;;   (plain-file "iptables.rules"
;;               (string-join
;;                 '("")
;;                 "\n" 'suffix)))
;; 
;; (define tower-iptables-rules
;;   (plain-file "iptables.rules"
;;               (string-join
;;                 '("")
;;                 "\n" 'suffix)))

(define %base-system
  (modify-record %bare-system
    (host-name -> "base-system")
    (firmware  -> (list linux-firmware))
    (kernel    -> linux-lts)
    ;; XXX: Replacing the initrd is close to top priority.
    (initrd    -> microcode-initrd)
    ;; XXX: I've commented out the modules that should be in %base-initrd-modules already.
    ;;      This could re-surface Dutchie's initramfs keyboard issues.
    (initrd-modules => (cons* #;"ahci" "igb" #;"usbhid" <>))
    (services => (modify-services <>
      (guix-service-type config =>
        (modify-record config
          ;; Non-Guix Subs Server
          (substitute-urls -> (cons "https://substitutes.nonguix.org" <>))
          (authorized-keys -> (cons (local-file "../../../pubkeys/signing-keys/nonguix.pub") <>))))))
    (services => (append <> (list
      (service iptables-service-type
               (iptables-configuration
                 (ipv4-rules iptables-rules-prefix)
                 (ipv6-rules iptables-rules-prefix)))
      (service fail2ban-service-type
               (fail2ban-configuration
                 (extra-jails
                   (list (fail2ban-jail-configuration
                           (name "sshd")
                           (enabled? #t)
                           (ignore-ip '("192.168.0.0/24"
                                        "10.0.0.0/24"
                                        "fe80::/10"
                                        "301:a1d7:c1d:1c20::/64"
                                        )))))))
      ;; TODO: Ygg. depends on networking services that aren't actually in the base system.
      (service yggdrasil-service-type
               (yggdrasil-configuration
                 (json-config
                   '((peers . #("XXXXXXXXXXXXXXXXX124:443"       ; 60ms
                                "XXXXXXXXXXXXXXXXX98:7040"       ; 69ms
                                "XXXXXXXXXXXXXXXXXtwork:443"     ; 95ms
                                "XXXXXXXXXXXXXXXXXe:9002"        ; 101ms
                                "XXXXXXXXXXXXXXXXXb001:379:5400:3ff:fe68:1cc6]:9002" ; Probably the same
                                "XXXXXXXXXXXXXXXXXer.land:9002" #; 104ms ))
                     (listen . #("tls://[::]:65000"))
                     (allowedpublicKeys . #("5XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX7" ; tower
                                            "8XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX7" ; moto
                                            "9XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXd" ; dutchie
                                            ))
                     (nodeinfoprivacy . #t)))))
      (service package-database-service-type) ; for `guix locate'
      (simple-service 'session-env-tz session-environment-service-type
        '(("TZ" . ":/etc/timezone"))))))
    (packages -> (append <>
      (map specification->package
           '("btrfs-progs" "cryptsetup"))))))

(define %graphical-system
  (modify-record %base-system
    (host-name -> "graphical-system")
    (services => (append <>
      (remove (lambda (s) (or (memq s %base-services)
                              (eq? (service-kind s) gdm-service-type)))
              %desktop-services)
      (list (service gnome-desktop-service-type)
            (service sddm-service-type)
            (service xfce-desktop-service-type)
            (service mate-desktop-service-type)
            (service plasma-desktop-service-type))
            ;; (service gc-root-service-type
            ;;   (append (list ;; %home
            ;;                 glibc-utf8-locales
            ;;                 texinfo
            ;;                 guile-3.0)
            ;;                 %default-locale-libcs))
            ))
    (packages -> (append <>
      (specifications->packages
        '("emacs-exwm"
          "emacs-desktop-environment"
          "gparted"
          "pinentry-emacs"))
      ;;; Packages from Emacs manifest pre-EXWM
      ;; Supporting Packages
      (specifications->packages
       '("bash"      ; needed for running decompression commands via sh
         "diffutils" ; for, umm, diffing
         "findutils" ; find-grep-dired
         "fontconfig"
         "gawk"      ; for man pages
         "git"       ; eshell prompt / magit
         "gnupg"
         "gnuplot"
         "grep"      ; TODO: use FD/RG by default? (ie. with commpands like project-find-regexp)
         "guile"
         "guix"      ; for emacs-guix
         "hicolor-icon-theme" ; Gtk-WARNING **: . . .  'hicolor' theme was not found . . .
         "ispell"
         "netcat"
         "nss-certs"
         "openssh"   ; tramp
         "patch"     ; for debbugs
         "sed"       ; used via git when calling projectile-recentf
         ;; For Dirvish
         "ffmpegthumbnailer"
         "imagemagick"
         "mediainfo"
         "poppler"
         "tar"
         "unzip"
         ;; TODO: These don't work on aarch64, but should be present otherwise
         ;; "ripgrep"   ; consult
         ;; "fd" ; dirvish
         ))
      ;; Emacs + Packages
      (specifications->packages
       '("emacs-next"
         "emacs-all-the-icons"
         "emacs-all-the-icons-completion"
         "emacs-all-the-icons-dired"
         "emacs-avy"
         "emacs-beacon"
         "emacs-cape"
         "emacs-consult"
         "emacs-corfu"
         "emacs-corfu-doc"
         "emacs-dash"
         "emacs-debbugs"
         "emacs-dimmer"
         "emacs-dirvish"
         "emacs-embark"
         "emacs-ert-expectations"
         "emacs-eshell-prompt-extras"
         "emacs-eshell-syntax-highlighting"
         "emacs-evil"
         "emacs-evil-collection"
         "emacs-evil-paredit"
         "emacs-focus"
         "emacs-geiser"
         "emacs-geiser-guile"
         "emacs-general"
         "emacs-git-gutter"
         "emacs-gnuplot"
         "emacs-guix"
         "emacs-highlight-indent-guides"
         "emacs-hydra"
         "emacs-janet-mode"
         "emacs-kbd" ; kmonad .kbd files
         "emacs-kind-icon"
         "emacs-ledger-mode"
         "emacs-lispy"
         "emacs-lispyville"
         "emacs-magit"
         "emacs-marginalia"
         "emacs-minions"
         "emacs-moody"
         "emacs-no-littering"
         "emacs-orderless"
         "emacs-org"
         "emacs-ox-haunt"
         "emacs-paredit"
         "emacs-paren-face"
         "emacs-plantuml-mode"
         "emacs-poet-theme"
         "emacs-rainbow-delimiters"
         "emacs-rainbow-mode"
         "emacs-tup-mode"
         "emacs-undo-tree"
         "emacs-use-package"
         "emacs-vertico"
         "emacs-which-key"
         "emacs-ws-butler"))
      ))))

(define %graphical-system
  ;; 10/30/2022: For nm-applet in XFCE under SDDM (on aarch64?)
  ((compose os-with-polkit-netdev)
   %graphical-system))

%graphical-system
