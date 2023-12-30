(define-module (antlers channels-latest)
  #:use-module (guix channels))

(define-public %antlers-latest-channels
  (list (channel
        (name 'guix)
        (url "/mnt/old_root/home/antlers/projects/assorted_upstreams/guix")
        (branch "bump-zfs")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'antlers)
        (url "/mnt/old_root/home/antlers/projects/guix-last")
        (branch "master")
        (introduction
          (make-channel-introduction
            "ca55f3479581b7bb4b5cd220190bbbe81d4cc7e6"
            (openpgp-fingerprint
              "DACB 035F B9B0 EE9C 7E13  1AAA C310 15D9 6620 A955"))))
      (channel
        (name 'nonguix)
        (url "/mnt/old_root/home/antlers/projects/assorted_upstreams/nonguix")
        (branch "master")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

%antlers-latest-channels
