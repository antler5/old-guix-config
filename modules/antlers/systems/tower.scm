(define-module (antlers systems tower)
  #:use-module ((gnu system accounts)
                #:select (user-account))
  ;; #:use-module (antlers channels)
  #:use-module (antlers records)
  #:use-module (antlers systems base)
  #:use-module (antlers systems transformations nvidia)
  #:use-module (antlers systems transformations yubi)
  #:use-module (antlers systems transformations zfs)
  #:use-module (gnu)
  #:use-module (guix records))

(use-package-modules linux)

(use-service-modules cgit
                     dns
                     monitoring
                     nfs
                     samba
                     ssh
                     sysctl
                     virtualization)

(use-system-modules mapped-devices
                    uuid)

(use-modules (guix packages)
             (gnu packages version-control)
             (guix git-download))
(define %system
  (modify-record %graphical-system
    (host-name -> "tower")
    (kernel -> (customize-linux
                 #:name "linux-lts-tower"
                 #:linux <>
                 ;; TODO: Needs tailored to initrd, or vice versa.
                 ;; #:defconfig (local-file "../../../kconfig/tower.kconfig")
                 ))
    (kernel-arguments -> (cons "systemd.unified_cgroup_hierarchy=0" <>))
    (file-systems -> (append
      (list (file-system
              (mount-point "/")
              (device (uuid "5baa097b-94a8-436d-a2d8-f26130dc2472" 'btrfs))
              (type "btrfs"))
            (file-system
              (mount-point "/boot")
              (device (uuid "e55e23ea-47d2-42b2-90c6-150a49210bd5" 'ext4))
              (needed-for-boot? #t)
              (type "ext4"))
            (file-system
              (mount-point "/boot/efi")
              (device (uuid "2392-5054" 'fat32))
              (type "vfat")))
      %base-tmpfs-list
      %base-file-systems))
    (users -> (append
      (map (lambda (user)
             (if (not (equal? (user-account-name user) "antlers"))
                 user
               (modify-record user
                 (supplementary-groups -> (cons "libvirt" <>)))))
           <>)))
    (packages -> (append <>
      (specifications->packages
        '("podman"
          "qemu"
          "virt-manager"))))
    (services => (modify-services <>
      (sysctl-service-type
        config => (sysctl-configuration
                    ;; TODO: Still need these?
                    (settings (append '(("net.ipv4.ip_forward" . "1")
                                        ("net.ipv6.conf.all.forwarding" . "1"))
                                      %default-sysctl-settings))))))
    (services => (append <> (list
      (service cgit-service-type
               (cgit-configuration
                 (package
                   (package
                     (inherit cgit)
                     (name "cgit")
                     (version "1.2.4")
                     (source
                       (origin
                         (inherit (package-source cgit))
                         (method git-fetch)
                         (uri (git-reference
                                (url "/home/antlers/projects/assorted_upstreams/cgit")
                                (commit "065a99654a049530474b20a24e8fd2d8cfa9afcb")))
                         (file-name (git-file-name name version))
                         (sha256
                           (base32 "0v11vjyisk243zi0ym90bnqb229j7iaqx1lwqdkszxzn1yxwq4ck"))))))))
      (simple-service 'subugid-config etc-service-type
                      `(("subuid" ,(plain-file "subuid" "antlers:100000:65536\n"))
                        ("subgid" ,(plain-file "subgid" "antlers:100000:65536\n"))))
      (simple-service 'containers etc-service-type
                      `(("containers/storage.conf" ,(plain-file "containers-storage.conf" "[storage]\ndriver = \"btrfs\"\n"))
                        ("containers/policy.json" ,(local-file "../../../files/policy.json"))))
      (service qemu-binfmt-service-type
               (qemu-binfmt-configuration
                 (platforms (lookup-qemu-platforms
                              "arm" "aarch64"))))
      ;; TODO: Secrets
      ;; (service unattended-upgrade-service-type
      ;;          (unattended-upgrade-configuration
      ;;            (channels #~%antlers-default-channels)
      ;;            (operating-system-file
      ;;              (file-append (local-file "." "config-dir" #:recursive? #t)
      ;;                           "/config.scm"))))
      (service darkstat-service-type
               (darkstat-configuration
                 (interface "enp4s0")))
      ;; BORKED: later
      ;; (service ddclient-service-type)
	    (service libvirt-service-type
        (libvirt-configuration
         (unix-sock-group "libvirt")))
      (service virtlog-service-type
        (virtlog-configuration
         (max-clients 1000)))
      (extra-special-file "/usr/share/OVMF/OVMF_CODE.fd"
        (file-append (specification->package "ovmf")
                     "/share/firmware/ovmf_x64.bin"))
      (extra-special-file "/usr/share/OVMF/OVMF_VARS.fd"
        (file-append (specification->package "ovmf")
                     "/share/firmware/ovmf_x64.bin"))
      (simple-service 'etc-netgroup etc-service-type
        (list `("netgroup" ,(plain-file "netgroup" "trusted (192.168.0.162,XXXXXX,)"))))
      (service nfs-service-type
               (nfs-configuration
                 (exports
                   '(("/media/pc_array/90-99-Legacy_Data/service/share/"
                      "@trusted (rw,fsid=0)")
                     ; ("/media/pc_array/90-99-Legacy_Data/service/share/XXXXXX"
                     ;  "192.168.0.162(rw,fsid=1)")
                     ; ("/media/pc_array/90-99-Legacy_Data/service/share/XXXXXX"
                     ;  "192.168.0.103(rw,fsid=2)")
                     ; ("/media/pc_array/90-99-Legacy_Data/service/share/eli"
                     ;  "192.168.0.147(rw,fsid=3)")
                     ))))
      (service samba-service-type
               (samba-configuration
                 (enable-smbd? #t)
                 (config-file (plain-file "smb.conf" "\
[global]
logging = syslog@1
allow insecure wide links = yes

[public]
path = /public
valid users = antlers XXXXXX
force group = users
read only = no
create mask = 0664
directory mask = 2775
follow symlinks = yes
wide links = yes
# unix extensions = no
"))))
      ;; Not working yet.
      ;; (udev-rules-service 'yubi-lock
      ;;                     (udev-rule
      ;;                       "90-yubi-lock.rules"
      ;;                       "ACTION==\"remove\", SUBSYSTEM==\"hidraw\", ATTRS{idVendor}==\"1050\", ATTRS{idProduct}==\"0113|0114|0115|0116|0120|0200|0402|0403|0406|0407|0410\", RUN+=\"DISPLAY=:0 xflock4\""))
      )))))

(define %system
  ((compose ; os-with-nvidia ; XXX: Has to be last...
            os-with-yubi
            os-with-zfs)
   %system))

%system
