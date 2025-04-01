(uiop/package:define-package :lem-migemo/main (:use :cl :lem))
(in-package :lem-migemo/main)
;;;don't edit above

(defun search-forward-migemo (point query &optional limit-point)
  (search-forward-regexp point (cl-migemo:query query) limit-point))

(defun search-backward-migemo (point query &optional limit-point)
  (search-backward-regexp point (cl-migemo:query query) limit-point))

(define-command isearch-forward-migemo (&optional prompt) ()
  (lem/isearch::isearch-start (or prompt "ISearch Migemo: ")
                              (lem/isearch::make-add-char-callback #'search-forward-migemo)
                              #'search-forward-migemo
                              #'search-backward-migemo
                              ""))

(define-command isearch-backward-migemo (&optional prompt) ()
  (lem/isearch::isearch-start (or prompt "ISearch Migemo: ")
                              (lem/isearch::make-add-char-callback #'search-backward-migemo)
                              #'search-forward-migemo
                              #'search-backward-migemo
                              ""))

(define-minor-mode migemo-mode (:global t
                                :keymap *migemo-mode-keymap*
                                :name "migemo"))

(define-key *migemo-mode-keymap* 'lem/isearch:isearch-forward 'isearch-forward-migemo)
(define-key *migemo-mode-keymap* 'lem/isearch:isearch-backward 'isearch-backward-migemo)
