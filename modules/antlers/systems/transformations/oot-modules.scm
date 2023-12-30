(define-module (antlers systems transformations oot-modules)
  #:use-module (antlers records)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 format)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (linux-module-with-kernel
            kernel-with-oot-modules))

(define (linux-module-with-kernel kernel module)
  (modify-record module
    (arguments => (substitute-keyword-arguments <>
      ((#:linux _ #f) kernel)))))

;; I hate having to compile the kernel twice, but the modules need built
;; against the parent kernel to avoid a recursive dependency and then I need to
;; repeat the build phases before merging the modules in because other
;; functions depend on `operating-system' records having a kernel package with
;; expected build phases...
;;
;; TODO: Can I just add a build phase to the parent kernel that copies the
;; build tree into an output, and then unify that into our CWD before building
;; again? How does this effect file modification times / the build system?
;;
;; TODO: 9 months later... IK! I just need to do it per-output over
;; the union of each package's outputs.
;;
(define (kernel-with-oot-modules kernel modules)
  (modify-record kernel
    (outputs -> (cons "pre-custom-modules" <>))
    (arguments => (substitute-keyword-arguments <>
      ((#:phases phases '%standard-phases)
       (with-imported-modules '((guix build union))
         #~(let* ((phases #$phases)
                  (modules '#$modules)
                  (install-custom-modules
                    (lambda (modules)
                      (lambda* (#:key outputs return-modules
                                #:allow-other-keys)
                        (if return-modules
                            modules
                            (begin
                              (use-modules (guix build union))
                              (rename-file #$output
                                           #$output:pre-custom-modules)
                              (union-build #$output
                                (cons #$output:pre-custom-modules modules)
                                #:create-all-directories? #t)))))))
             ;; XXX: The Right Thing might be to redefine
             ;; `warn-about-collision', but this'll do.
             (let-syntax ((push!* (syntax-rules ()
                                   ((push!* elts ... lst)
                                    (set! lst (cons* elts ... lst))))))
               (push!* "loaders.cache"
                       (@@ (guix build union) %harmless-collisions)))
             (if (not (assoc-ref phases 'install-custom-modules))
                 (modify-phases phases
                   (add-after 'install 'install-custom-modules
                     (install-custom-modules modules)))
                 (modify-phases phases
                   (replace 'install-custom-modules
                     (install-custom-modules
                       (append ((assoc-ref phases 'install-custom-modules)
                                #:return-modules #t)
                               modules))))))))))))
