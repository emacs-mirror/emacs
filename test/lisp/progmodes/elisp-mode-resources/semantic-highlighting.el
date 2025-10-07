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

(require 'cl-lib)
;; ^ (elisp-function-reference font-lock-keyword-face)
;;        ^ (elisp-feature font-lock-constant-face)

;; Taken from `completion-shorthand-try-completion' in minibuffer.el:
(defun foobaz (string table pred point)
;; ^ (elisp-macro-call font-lock-keyword-face)
;;     ^ (elisp-defun font-lock-function-name-face)
;;             ^      ^     ^    ^ elisp-binding-variable
  (cl-loop with expanded
;; ^ (elisp-macro-call font-lock-keyword-face)
;;              ^ elisp-binding-variable
           for (short . long) in
;;              ^ elisp-binding-variable
;;                      ^ elisp-binding-variable
           (with-current-buffer minibuffer--original-buffer
;;          ^ (elisp-macro-call font-lock-keyword-face)
;;                              ^ elisp-free-variable
             read-symbol-shorthands)
;;           ^ elisp-free-variable
           for probe =
;;             ^ elisp-binding-variable
           (and (> point (length short))
;;          ^ (elisp-special-form font-lock-keyword-face)
;;               ^ elisp-function-reference
;;                 ^ elisp-bound-variable
;;                        ^ elisp-function-reference
;;                               ^ elisp-bound-variable
                (string-prefix-p short string)
;;               ^ elisp-function-reference
;;                               ^ elisp-bound-variable
;;                                     ^ elisp-bound-variable
                (try-completion (setq expanded
;;               ^ elisp-function-reference
;;                                ^ (elisp-special-form font-lock-keyword-face)
;;                                     ^ elisp-bound-variable
                                      (concat long
;;                                     ^ elisp-function-reference
;;                                             ^ elisp-bound-variable
                                              (substring
;;                                             ^ elisp-function-reference
                                               string
;;                                             ^ elisp-bound-variable
                                               (length short))))
;;                                              ^ elisp-function-reference
;;                                                     ^ elisp-bound-variable
                                table pred))
;;                              ^ elisp-bound-variable
;;                                    ^ elisp-bound-variable
           when probe
;;              ^ elisp-bound-variable
           do (message "Shorthand expansion")
;;             ^ elisp-function-reference
           and return (cons expanded (max (length long)
;;                     ^ elisp-function-reference
;;                          ^ elisp-bound-variable
;;                                    ^ elisp-function-reference
;;                                         ^ elisp-function-reference
;;                                                ^ elisp-bound-variable
                                          (+ (- point (length short))
;;                                         ^ elisp-function-reference
;;                                            ^ elisp-function-reference
;;                                                     ^ elisp-function-reference
;;                                                            ^ elisp-bound-variable
                                             (length long))))))
;;                                            ^ elisp-function-reference
;;                                                   ^ elisp-bound-variable

(let ((foo 'bar))
;; ^ (elisp-special-form font-lock-keyword-face)
;;     ^ elisp-binding-variable
  (cl-flet ((foo () 'baz))
;; ^ (elisp-macro-call font-lock-keyword-face)
;;           ^ elisp-function-reference
    (foo)
;;   ^ elisp-function-reference
    (cl-macrolet ((foo () 'foo))
;;   ^ (elisp-macro-call font-lock-keyword-face)
;;                 ^ elisp-macro-call
      (foo))))
;;     ^ elisp-macro-call

(when-let* ((foo (symbol-at-point))
;; ^ (elisp-macro-call font-lock-keyword-face)
;;           ^ elisp-binding-variable

            ((commandp foo)))
;;            ^ elisp-function-reference
;;                     ^ elisp-bound-variable
  foo)
;; ^ elisp-bound-variable
