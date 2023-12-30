(define-module (antlers home antlers)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(use-package-modules admin)

(define (pair->env-var pair)
  "Upcase CAR & double-quote string CDR, else #f."
  ;; TODO: Match two strings specifically?
  (cond (pair? pair) (cons (string-upcase (car pair))
                           (string-append "\"" (cdr pair) "\""))
        (else #f)))

(define %home
  (home-environment
    (packages
      (append (specifications->packages
                ;; TODO: Split these up into gui / cli lists?
                `(;; GUI Software
                  "gimp"
                  "gparted"
                  "vlc"
                  ;; X11 Utils
                  "setxkbmap"
                  "xcape"
                  ;; Fonts
                  "font-iosevka"
                  ;; Network and Web Software
                  "nmap"
                  "lynx"
                  "mosh"
                  "sshfs"
                  "transmission"
                  "transmission-remote-gtk"
                  ;; Kernel Userspace Interfaces
                  "lm-sensors"
                  "wireguard-tools"
                  ;; Security and Encryption
                  "yubikey-personalization"
                  "python-yubikey-manager"
                  "gnupg"
                  "pinentry"
                  ;; CLI Software
                  "emacs-next"
                  "make"
                  ;; CLI Apps
                  "htop"
                  "iotop"
                  "tmux"
                  ;; CLI Utils
                  "binutils"
                  "curl"
                  "direnv"
                  "expect" ; for unbuffer
                  "file"
                  "git"
                  "git:send-email"
                  "moreutils"
                  "nmap"
                  "tmux"
                  "tree"
                  ;; Guile & Guix
                  "guile"
                  "guile-curl"
                  "guile-lib"
                  "guile-readline"
                  ;; Compression
                  "p7zip"
                  "pigz" ;; TODO: Alias or wrap
                  "zstd"))
              ;; Aarch64 build failures:
              ;; TODO: Kmonad (on arm pls?)
              (if (target-x86?)
                  (cons `(,(@ (nongnu packages mozilla) firefox) "out")
                        (specifications->packages
                          '("keepassxc"
                            "neovim")))
                  '())))
    (services
      (list
        ;; TODO: emacs-pinentry or eqv.
        ;; TODO: Map files/config automatically?
        (simple-service 'cool-retro-term-conf
                        home-files-service-type
                        (list `(".config/cool-retro-term/crt-cata.json"
                                ,(local-file "files/crt-cata.json"))))
        (simple-service 'gpg-agent-conf
                        home-files-service-type
                        (list `(".gnupg/gpg-agent.conf"
                                ,(plain-file "gpg-agent.conf"
                                             (string-join
                                               '("allow-loopback-pinentry"
                                                 "")
                                               "\n")))))
        (service home-shepherd-service-type
                 (home-shepherd-configuration
                   (services
                     (list (shepherd-service
                             (provision '(syncthing))
                             (documentation "Run and control syncthing.")
                             (start #~(make-forkexec-constructor
                                        (list #$(file-append (specification->package "syncthing")
                                                             "/bin/syncthing")
                                              "-no-browser")
                                        #:log-file (string-append (getenv "HOME")
                                                                  "/.local/var/log/syncthing.log")
                                        ))
                             (stop #~(make-kill-destructor))
                             (respawn? #t))))))
        (service home-bash-service-type
                 (home-bash-configuration
                   (guix-defaults? #t)
                   (aliases
                     '(;; -- Navigation
                       (".." . "cd ..")
                       (":q" . "exit")
                       (",q" . "exit")
                       (":w" . "sync")
                       (":wq" . "sync && exit")
                       ("mkd" . "mkdir -pv")
                       ;; -- Emacs
                       ;; TODO: I need to rework my .emacs.d/ again...
                       ("emacs" . "guix shell --pure --manifest=\"$HOME/.emacs.d/manifest.scm\" -- emacs")
                       ;; -- Color & Formatting
                       ("la" . "ls -a")
                       ("lla" . "ls -la")
                       ("diff" . "diff --color=auto")
                       ;; -- Aliases (ie. Shortcuts)
                       ("sdn" . "sudo shutdown")
                       ;; -- Safety
                       ("mv" . "mv -iv")
                       ("cp" . "cp -iv")
                       ("rm" . "rm -Iv")))
                   (bash-profile (list (plain-file "bash-profile"
                                                   "# Guix Stuff
                                                   GUIX_PROFILE=\"/home/antlers/.guix-profile\"
                                                   . \"$GUIX_PROFILE/etc/profile\"

                                                   # Override default bashrc-content (can't be an alias)
                                                   alias ls=\"ls -hN --color=auto --group-directories-first\"

                                                   # Bash Options
                                                   set -o vi
                                                   shopt -s extglob
                                                   shopt -s histappend
                                                   shopt -s checkwinsize
                                                   shopt -s expand_aliases

                                                   # https://susam.net/blog/from-xon-xoff-to-forward-incremental-search.html
                                                   stty -ixon

                                                   # Scroll history based on prefix
                                                   bind '\"\\e[A\": history-search-backward'
                                                   bind '\"\\e[B\": history-search-forward'

                                                   export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
                                                   export HISTFILE=$XDG_CACHE_HOME/.bash_history")))))
        (simple-service 'env-vars-service
                        home-environment-variables-service-type
                        (filter-map pair->env-var
                                    '(("histcontrol"  . "ignoreboth")
                                      ("histsize"     . "1000")
                                      ("histfilesize" . "2000")
                                      ;; grub-install on ZFS
                                      ;; https://openzfs.github.io/openzfs-docs/Getting%20Started/Arch%20Linux/Root%20on%20ZFS/5-bootloader.html#apply-workarounds
                                      ("zpool_vdev_name_path" . "yes"))))
        (simple-service 'test-config
                        home-xdg-configuration-files-service-type
                        (list `("nvim/init.vim"
                                ,(plain-file "init.vim"
                                             "set expandtab
                                             set tabstop=2
                                             set shiftwidth=2"))))))))

%home
