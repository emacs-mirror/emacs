(in-package :lem-user)

;;; vi-mode
(define-key lem-vi-mode:*normal-keymap* "q" 'quit-active-window)
(define-key lem-vi-mode:*insert-keymap* "C-n" 'lem/abbrev:abbrev-with-pop-up-window)

(add-hook lem-vi-mode:*enable-hook*
          (lambda ()
            (message "enable")))

(add-hook lem-vi-mode:*disable-hook*
          (lambda ()
            (message "disable")))

(lem-vi-mode:define-ex-command "load" (range argument)
  (declare (ignore range))
  (let ((filename (string-trim " " argument)))
    (lem-lisp-mode:lisp-load-file (if (string= filename "")
                                      (buffer-filename (current-buffer))
                                      filename))))
