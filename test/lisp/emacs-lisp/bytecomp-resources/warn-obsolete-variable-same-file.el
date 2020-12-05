;;; -*- lexical-binding: t -*-

(defvar foo-obsolete nil)
(make-obsolete-variable 'foo-obsolete nil "99.99")

;; From bytecomp.el:
;; If foo.el declares `toto' as obsolete, it is likely that foo.el will
;; actually use `toto' in order for this obsolete variable to still work
;; correctly, so paradoxically, while byte-compiling foo.el, the presence
;; of a make-obsolete-variable call for `toto' is an indication that `toto'
;; should not trigger obsolete-warnings in foo.el.
(defun foo ()
  foo-obsolete)
