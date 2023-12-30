(use-modules (guix modules)
             (ice-9 ftw)
             (system base compile))

(define channel-directory "modules")
(define project-root
  (string-append (dirname (gmk-expand "$(abspath $(lastword $(MAKEFILE_LIST)))"))
                 "/" channel-directory))

(define (internal-path-or-module-name? path-or-module-name)
  (if (string? path-or-module-name)
      (string-prefix? project-root path-or-module-name)
      (file-exists? (module-name->path path-or-module-name))))

(define (path->module-name path)
  (file-name->module-name (substring path (string-prefix-length project-root path))))
(define (module-name->path module)
  (string-append project-root "/" (module-name->file-name module)))

(define (scm-path->prereqs path)
  (map (compose compiled-file-name module-name->path)
       (filter (lambda (module-name)
                 (not (equal? module-name (path->module-name path))))
               (source-module-closure (list (path->module-name path))
                                      (cons* project-root %load-path)
                                      #:select? internal-path-or-module-name?))))

(define (scm-path->makefile-rule path)
  (string-append
    "\n" (compiled-file-name path)
    ": " (string-join (cons path (scm-path->prereqs path)))
    "\n\tguild compile"
    ;; XXX: Hmm...
    " -L " project-root
    " -L /tmp/antlers-guix/modules"
    ; " -L /home/antlers/projects/assorted_upstreams/guix"
    " -L /home/antlers/projects/assorted_upstreams/nonguix"
    " -W 3"
    " -Wunused-module"
    " $<\n"))

(let ((files '()))
  (ftw project-root
       (lambda (filename statinfo flag)
         (when (string-suffix? ".scm" filename)
           (set! files (cons filename files)))
         #t))
  (gmk-eval
    (string-join
      `("\n.PHONY: sources\n\nsources: " ,(string-join (map compiled-file-name files)) "\n\t:;\n"
        ,@(map scm-path->makefile-rule files))
      "")))
