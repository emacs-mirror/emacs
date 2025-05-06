;; -*- lexical-binding: t; -*-

;;; Old Sizes:

;; % (cd lisp/emacs-lisp/; l cl-*.elc)
;; -rw-r--r-- 1 monnier monnier  68920  5 mai 13:49 cl-generic.elc
;; -rw-r--r-- 1 monnier monnier  41841  5 mai 13:49 cl-preloaded.elc
;; -rw-r--r-- 1 monnier monnier  23037  5 mai 13:58 cl-lib.elc
;; -rw-r--r-- 1 monnier monnier  32664  5 mai 14:14 cl-extra.elc
;; -rw-r--r-- 1 monnier monnier  53769  5 mai 14:14 cl-loaddefs.elc
;; -rw-r--r-- 1 monnier monnier  17921  5 mai 14:14 cl-indent.elc
;; -rw-r--r-- 1 monnier monnier  18295  5 mai 14:14 cl-print.elc
;; -rw-r--r-- 1 monnier monnier 101608  5 mai 14:14 cl-macs.elc
;; -rw-r--r-- 1 monnier monnier  43849  5 mai 14:14 cl-seq.elc
;; -rw-r--r-- 1 monnier monnier   8691  5 mai 18:53 cl-types.elc
;; %

;;; After the move:

;; % (cd lisp/emacs-lisp/; l cl-*.elc)
;; -rw-r--r-- 1 monnier monnier  46390  5 mai 23:04 cl-preloaded.elc
;; -rw-r--r-- 1 monnier monnier  68920  5 mai 23:04 cl-generic.elc
;; -rw-r--r-- 1 monnier monnier  23620  5 mai 23:05 cl-lib.elc
;; -rw-r--r-- 1 monnier monnier  54752  5 mai 23:15 cl-loaddefs.elc
;; -rw-r--r-- 1 monnier monnier  17921  5 mai 23:05 cl-indent.elc
;; -rw-r--r-- 1 monnier monnier  34065  5 mai 23:05 cl-extra.elc
;; -rw-r--r-- 1 monnier monnier  18295  5 mai 23:05 cl-print.elc
;; -rw-r--r-- 1 monnier monnier 102581  5 mai 23:05 cl-macs.elc
;; -rw-r--r-- 1 monnier monnier    159  5 mai 23:05 cl-types.elc
;; -rw-r--r-- 1 monnier monnier  43849  5 mai 23:05 cl-seq.elc
;; %

;; cl-preloaded:  +4549    41841 => 46390
;; cl-lib:        + 583    23037 => 23620
;; cl-macs:       + 973   101608 => 102581
;; cl-extra       +1401    32664 => 34065
;; cl-loaddefs:   + 983    53769 => 54752

;; Data types defined by `cl-deftype' are now recognized as argument
;; types for dispatching generic functions methods.

;; Needed until merged in existing libraries.
(require 'cl-lib)
(eval-when-compile (require 'cl-macs))  ;For cl--find-class.
(declare-function cl-remprop "cl-extra" (symbol propname))
(declare-function cl--class-children "cl-extra" (class))




(provide 'cl-types)

;;; cl-types.el ends here
