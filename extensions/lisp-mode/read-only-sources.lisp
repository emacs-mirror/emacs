(uiop:define-package :lem-lisp-mode/read-only-sources
  (:use :cl)
  (:import-from :lem/read-only-sources
                :define-read-only-source))
(in-package :lem-lisp-mode/read-only-sources)

(define-read-only-source sbcl-source (directory)
  (alexandria:when-let (connection (lem-lisp-mode/internal:current-connection))
    (dolist (pattern (lem-lisp-mode/connection:connection-system-file-patterns connection))
      (when (pathname-match-p directory pattern)
        (return t)))))

(define-read-only-source quicklisp-dists (directory)
  (search "/dists/quicklisp/software/" directory))
