;;; semantic-highlighting.el ---   -*- lexical-binding: t; -*-

(defun foo (bar)
;; ^ (elisp-macro font-lock-keyword-face)
;;     ^ (elisp-defun font-lock-function-name-face)
;;          ^ elisp-binding-variable
  (let ((cpa current-prefix-arg))
;; ^ (elisp-special-form font-lock-keyword-face)
;;       ^ elisp-binding-variable
;;           ^ elisp-free-variable
    (or cpa (ignore bar)))
;;   ^ (elisp-special-form font-lock-keyword-face)
;;      ^ elisp-bound-variable
;;           ^ elisp-function
;;                  ^ elisp-bound-variable
  )

(add-face-text-property
;; ^ elisp-function
 (point) (mark)
;; ^ elisp-function
;;        ^ elisp-function
 (if not-good
;; ^ (elisp-special-form font-lock-keyword-face)
;;   ^ elisp-free-variable
     'error
;;    ^ elisp-face
   (message "Good.")
;;  ^ elisp-function
   'success))
;;  ^ elisp-face

(require 'cl-lib)
;; ^ (elisp-function font-lock-keyword-face)
;;        ^ (elisp-feature font-lock-constant-face)

;; Taken from `completion-shorthand-try-completion' in minibuffer.el:
(defun foobaz (string table pred point)
;; ^ (elisp-macro font-lock-keyword-face)
;;     ^ (elisp-defun font-lock-function-name-face)
;;             ^      ^     ^    ^ elisp-binding-variable
  (cl-loop with expanded
;; ^ (elisp-macro font-lock-keyword-face)
;;              ^ elisp-binding-variable
           for (short . long) in
;;              ^ elisp-binding-variable
;;                      ^ elisp-binding-variable
           (with-current-buffer minibuffer--original-buffer
;;          ^ (elisp-macro font-lock-keyword-face)
;;                              ^ elisp-free-variable
             read-symbol-shorthands)
;;           ^ elisp-free-variable
           for probe =
;;             ^ elisp-binding-variable
           (and (> point (length short))
;;          ^ (elisp-special-form font-lock-keyword-face)
;;               ^ elisp-function
;;                 ^ elisp-bound-variable
;;                        ^ elisp-function
;;                               ^ elisp-bound-variable
                (string-prefix-p short string)
;;               ^ elisp-function
;;                               ^ elisp-bound-variable
;;                                     ^ elisp-bound-variable
                (try-completion (setq expanded
;;               ^ elisp-function
;;                                ^ (elisp-special-form font-lock-keyword-face)
;;                                     ^ elisp-bound-variable
                                      (concat long
;;                                     ^ elisp-function
;;                                             ^ elisp-bound-variable
                                              (substring
;;                                             ^ elisp-function
                                               string
;;                                             ^ elisp-bound-variable
                                               (length short))))
;;                                              ^ elisp-function
;;                                                     ^ elisp-bound-variable
                                table pred))
;;                              ^ elisp-bound-variable
;;                                    ^ elisp-bound-variable
           when probe
;;              ^ elisp-bound-variable
           do (message "Shorthand expansion")
;;             ^ elisp-function
           and return (cons expanded (max (length long)
;;                     ^ elisp-function
;;                          ^ elisp-bound-variable
;;                                    ^ elisp-function
;;                                         ^ elisp-function
;;                                                ^ elisp-bound-variable
                                          (+ (- point (length short))
;;                                         ^ elisp-function
;;                                            ^ elisp-function
;;                                                     ^ elisp-function
;;                                                            ^ elisp-bound-variable
                                             (length long))))))
;;                                            ^ elisp-function
;;                                                   ^ elisp-bound-variable

(let ((foo 'bar))
;; ^ (elisp-special-form font-lock-keyword-face)
;;     ^ elisp-binding-variable
  (cl-flet ((foo () 'baz))
;; ^ (elisp-macro font-lock-keyword-face)
;;           ^ elisp-function
    (foo)
;;   ^ elisp-function
    (cl-macrolet ((foo () 'foo))
;;   ^ (elisp-macro font-lock-keyword-face)
;;                 ^ elisp-macro
      (foo))))
;;     ^ elisp-macro

(when-let* ((foo (symbol-at-point))
;; ^ (elisp-macro font-lock-keyword-face)
;;           ^ elisp-binding-variable
;;                ^ elisp-function
            current-prefix-arg
;;          ^ elisp-shadowing-variable
            ((commandp foo)))
;;            ^ elisp-function
;;                     ^ elisp-bound-variable
  foo)
;; ^ elisp-bound-variable

;; Taken from minibuffer.el:
(defcustom my-foo nil
;; ^ (elisp-macro font-lock-keyword-face)
;;         ^ (elisp-defvar font-lock-variable-name-face)
  "Foo."
  :type '(choice (const :tag "No special message handling" nil)
;;        ^ elisp-widget-type
;;                ^ elisp-widget-type
                 (repeat
;;                ^ elisp-widget-type
                  (choice (function-item :tag "Inhibit some messages"
;;                 ^ elisp-widget-type
;;                         ^ elisp-widget-type
                                         inhibit-message)
;;                                       ^ elisp-function
                          (function-item :tag "Accumulate messages"
                                         set-multi-message)
;;                                       ^ elisp-function
                          (function-item :tag "Handle minibuffer"
                                         set-minibuffer-message)
;;                                       ^ elisp-function
                          (function :tag "Custom function")
;;                         ^ (elisp-widget-type font-lock-keyword-face)
                          )))
  :version "29.1")

;; Taken from browse-url.el:
(defcustom baz-opt 'browse-url-mail
  "Baz."
  :type '(function-item :tag "Emacs Mail" :value browse-url-mail))
;;        ^ elisp-widget-type
;;                                         ^ (elisp-constant font-lock-builtin-face)

(e-s-analyze-form #'ignore)
;; ^ (elisp-shorthand-font-lock-face elisp-function)
;;   ^ elisp-function
;;                  ^ elisp-function

(defface foobar
  '((default :inherit font-lock-function-call-face)
;;            ^ (elisp-constant font-lock-builtin-face)
;;                    ^ elisp-face
    (((background light)) :foreground "#00008b")
    (((background dark))  :foreground "#5c9cff"))
  "Face for highlighting symbol role names in Emacs Lisp code."
  :version "31.1")
;; ^ (elisp-constant font-lock-builtin-face)

(propertize foo
            'face
            (cond
             ((random) '(success (:foreground "green" :inherit default)))
;;                       ^ elisp-face
;;                                                             ^ elisp-face
             ((foobar) 'font-lock-keyword-face)
;;                      ^ elisp-face
             (t '(:inherit error))))
;;                         ^ elisp-face
