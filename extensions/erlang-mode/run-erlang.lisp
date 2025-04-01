(defpackage :lem-erl-mode.run-erlang
  (:use :cl :lem :lem-erlang-mode)
  (:export :*erlang-run-command*
           :run-erlang-mode))

(in-package :lem-erl-mode.run-erlang)

(defvar *erlang-run-command* "erl")

(defvar *process* nil)

(define-major-mode run-erlang-mode nil
    (:name "Run Erlang"
     :keymap *run-erlang-mode-keymap*
     :syntax-table *erlang-syntax-table*)
  (reset-listener-variables (current-buffer))
  (lem/listener-mode:start-listener-mode))


(define-key *erlang-mode-keymap* "C-c C-r" 'erlang-eval-region)

(defun reset-listener-variables (buffer)
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function :buffer buffer)
        #'identity
        (variable-value 'lem/listener-mode:listener-check-input-function :buffer buffer)
        (constantly t)
        (variable-value 'lem/listener-mode:listener-execute-function :buffer buffer)
        'execute-input))

(defun execute-input (point string)
  (declare (ignore point))
  (unless (alive-process-p)
    (editor-error "Erlang process doesn't exist."))
  (lem-process:process-send-input *process*
                                  (concatenate 'string string (string #\newline))))

(defun alive-process-p ()
  (and *process*
       (lem-process:process-alive-p *process*)))

(defun repl-buffer-exists-p ()
  (get-buffer "*erlang*"))

(defun get-repl-buffer ()
  (let ((buffer (make-buffer "*erlang*")))
    (unless (eq (buffer-major-mode buffer) 'run-erlang-mode)
      (change-buffer-mode buffer 'run-erlang-mode))
    buffer))

(defun output-callback (string)
  (let* ((already-exists (repl-buffer-exists-p))
         (buffer (get-repl-buffer))
         (p (buffer-point buffer)))
    (buffer-end p)
    (setf string (ppcre:regex-replace-all "\\r\\n" string (string #\newline)))
    (insert-string p string)
    (when (ppcre:scan "^[0-9]+>" (line-string p))
      (lem/listener-mode:refresh-prompt buffer nil))
    (unless already-exists
      (switch-to-window (pop-to-buffer buffer)))
    (alexandria:when-let (window (first (get-buffer-windows buffer)))
      (with-current-window window
        (buffer-end p)
        (window-see window)))
    (redraw-display)))

(defun run-erlang-internal ()
  (unless (alive-process-p)
    (when *process*
      (lem-process:delete-process *process*))
    (setf *process*
          (lem-process:run-process *erlang-run-command*
                                   :name "run-erlang"
                                   :output-callback 'output-callback))))

(define-command erlang-eval-region (start end) (:region)
  (unless (alive-process-p)
    (editor-error "Erlang process doesn't exist."))
  (lem-process:process-send-input *process* (points-to-string start end)))

(define-command run-erlang () ()
  (run-erlang-internal))

(add-hook *exit-editor-hook*
          (lambda ()
            (when *process*
              (lem-process:delete-process *process*))))

