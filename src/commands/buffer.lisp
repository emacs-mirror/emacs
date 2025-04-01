(defpackage :lem-core/commands/buffer
  (:use :cl :lem-core)
  (:export :*read-only-function*
           :indent-current-buffer
           :toggle-read-only
           :rename-buffer
           :unmark-buffer)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/buffer)

(defvar *read-only-function* nil)

(define-key *global-keymap* "C-x C-q" 'toggle-read-only)
(define-key *global-keymap* "M-~" 'unmark-buffer)

(define-command indent-current-buffer () ()
  "Indent the current buffer."
  (indent-buffer (current-buffer)))

(define-command toggle-read-only () ()
  "Toggle the buffer read-only."
  (setf (buffer-read-only-p (current-buffer))
        (not (buffer-read-only-p (current-buffer))))
  (when *read-only-function*
    (funcall *read-only-function*
             (buffer-read-only-p (current-buffer)))))

(define-command rename-buffer (name) ((:string "Rename buffer: "))
  "Rename the buffer."
  (buffer-rename (current-buffer) name))

(define-command unmark-buffer () ()
  "Remove the mark where the buffer was changed."
  (buffer-unmark (current-buffer)))
