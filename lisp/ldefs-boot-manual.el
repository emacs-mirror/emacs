;; These appear to be necessary as they are used elsewhere in macro definitions.
(load "emacs-lisp/gv.el")
(load "emacs-lisp/nadvice.el")
(load "emacs-lisp/inline.el")

;; This variable is used by bytecomp.el
(defvar warning-series nil)

;; This variable is used by emacs-lisp-mode which is used heavily
;; during the byte-compile phase
(defvar electric-pair-text-pairs '((34 . 34)))


(load "ldefs-boot-auto.el")

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
