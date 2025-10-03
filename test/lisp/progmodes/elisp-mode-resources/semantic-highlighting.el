;;; semantic-highlighting.el ---   -*- lexical-binding: t; -*-

(defun foo (bar)
;; ^ (elisp-macro-call font-lock-keyword-face)
;;     ^ (elisp-defun font-lock-function-name-face)
;;          ^ elisp-binding-variable
  (let ((cpa current-prefix-arg))
;; ^ (elisp-special-form font-lock-keyword-face)
;;       ^ elisp-binding-variable
;;           ^ elisp-free-variable
    (or cpa (ignore bar)))
;;   ^ (elisp-special-form font-lock-keyword-face)
;;      ^ elisp-bound-variable
;;           ^ elisp-function-reference
;;                  ^ elisp-bound-variable
  )

(add-face-text-property
;; ^ elisp-function-reference
 (point) (mark)
;; ^ elisp-function-reference
;;        ^ elisp-function-reference
 (if not-good
;; ^ (elisp-special-form font-lock-keyword-face)
;;   ^ elisp-free-variable
     'error
;;    ^ elisp-face
   (message "Good.")
;;  ^ elisp-function-reference
   'success))
;;  ^ elisp-face
