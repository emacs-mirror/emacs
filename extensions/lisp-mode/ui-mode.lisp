(defpackage :lem-lisp-mode/ui-mode
  (:use :cl
        :lem)
  (:import-from :lem/button
                :button-at
                :button-action
                :forward-button)
  (:export :lisp-ui-mode
           :lisp-ui-default-action
           :lisp-ui-forward-button))
(in-package :lem-lisp-mode/ui-mode)

(define-major-mode lisp-ui-mode nil
    (:name "lisp-ui" ;TODO
     :keymap *lisp-ui-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *lisp-ui-keymap* "Return" 'lisp-ui-default-action)
(define-key *lisp-ui-keymap* "Tab" 'lisp-ui-forward-button)
(define-key *lisp-ui-keymap* "q" 'quit-active-window)
(define-key *lisp-ui-keymap* "M-q" 'quit-active-window)

(define-command lisp-ui-default-action () ()
  (let ((button (button-at (current-point))))
    (when button (button-action button))))

(define-command lisp-ui-forward-button () ()
  (let ((p (current-point)))
    (or (forward-button p)
        (progn
          (buffer-start p)
          (forward-button p)))))
