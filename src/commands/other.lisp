(defpackage :lem-core/commands/other
  (:use :cl :lem-core)
  (:export :nop-command
           :undefined-key
           :keyboard-quit
           :escape
           :exit-lem
           :quick-exit
           :execute-command
           :show-context-menu
           :load-library)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/other)

(define-key *global-keymap* "NopKey" 'nop-command)
(define-key *global-keymap* "C-g" 'keyboard-quit)
(define-key *global-keymap* "Escape" 'escape)
(define-key *global-keymap* "C-x C-c" 'exit-lem)
(define-key *global-keymap* "M-x" 'execute-command)
(define-key *global-keymap* "Shift-F10" 'show-context-menu)
(define-key *global-keymap* "M-h" 'show-context-menu)

(define-command nop-command () ()
  "No operation; it does nothing and return nil.")

(define-command undefined-key () ()
  "Signal undefined key error."
  (error 'undefined-key-error))

(define-command keyboard-quit () ()
  "Signal a `quit` condition."
  (error 'editor-abort :message nil))

(define-command escape () ()
  "Signal a `quit` condition silently."
  (error 'editor-abort :message nil))

(define-command exit-lem (&optional (ask t)) ()
  "Ask for modified buffers before exiting lem."
  (let ((modified-buffers
          (mapcar #'buffer-name (modified-buffers))))
    (and (or
          (null ask)
          (not modified-buffers)
          (prompt-for-y-or-n-p
           (format nil
                   "Modified buffers exist:~%~{~a~%~}Leave anyway?"
                   modified-buffers)))
         (exit-editor))))

(define-command quick-exit () ()
  "Exit the lem job and kill it."
  (lem-core/commands/file:save-some-buffers t)
  (exit-editor))

(define-command execute-command (arg) (:universal-nil)
  "Read a command name, then read the ARG and call the command."
  (let* ((name (prompt-for-command
                (if arg
                    (format nil "~D M-x " arg)
                    "M-x ")))
         (command (find-command name)))
    (if command
        (call-command command arg)
        (message "invalid command"))))

(define-command show-context-menu () ()
  (let ((context-menu (buffer-context-menu (current-buffer))))
    (when context-menu
      (lem-core::update-point-on-context-menu-open (current-point))
      (lem-if:display-context-menu (implementation) context-menu '(:gravity :cursor)))))

(define-command load-library (name)
    ((prompt-for-library "load library: " :history-symbol 'load-library))
  "Load the Lisp library named NAME."
  (message "Loading ~A." name)
  (cond ((ignore-errors (maybe-load-systems (format nil "lem-~A" name) :silent t))
         (message "Loaded ~A." name))
        (t (message "Can't find Library ~A." name))))

(setf lem-core:*abort-key* 'keyboard-quit)
