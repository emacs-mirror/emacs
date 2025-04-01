(defpackage :lem-elixir-mode.run-elixir
  (:use :cl :lem :lem-elixir-mode)
  (:export :*elixir-run-command*
           :run-elixir))

(in-package :lem-elixir-mode.run-elixir)

(defvar *elixir-run-command* "iex")

(defvar *process* nil)

(define-major-mode run-elixir-mode lem-elixir-mode:elixir-mode
    (:name "elixir"
     :keymap *run-elixir-mode-keymap*
     :syntax-table *elixir-syntax-table*)
  (reset-listener-variables (current-buffer))
  (lem/listener-mode:start-listener-mode))

(define-key *elixir-mode-keymap* "C-c C-r" 'elixir-eval-region)

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
    (editor-error "Elixir process doesn't exist."))
  (lem-process:process-send-input *process*
                                  (concatenate 'string string (string #\newline))))

(defun alive-process-p ()
  (and *process*
       (lem-process:process-alive-p *process*)))

(defun repl-buffer-exists-p ()
  (get-buffer "*elixir*"))

(defun get-repl-buffer ()
  (let ((buffer (make-buffer "*elixir*")))
    (unless (eq (buffer-major-mode buffer) 'run-elixir-mode)
      (change-buffer-mode buffer 'run-elixir-mode))
    buffer))

(defun output-callback (string)
  (let* ((already-exists (repl-buffer-exists-p))
         (buffer (get-repl-buffer))
         (p (buffer-point buffer)))
    (buffer-end p)
    (setf string (ppcre:regex-replace-all "\\r\\n" string (string #\newline)))
    (insert-string p string)
    (when (ppcre:scan "^(iex|...)(.+)" (line-string p))
      (lem/listener-mode:refresh-prompt buffer nil))
    (unless already-exists
      (switch-to-window (pop-to-buffer buffer)))
    (alexandria:when-let (window (first (get-buffer-windows buffer)))
      (with-current-window window
        (buffer-end p)
        (window-see window)))
    (redraw-display)))

(defun run-elixir-internal ()
  (unless (alive-process-p)
    (when *process*
      (lem-process:delete-process *process*))
    (setf *process*
          (lem-process:run-process *elixir-run-command*
                                   :name "run-elixir"
                                   :output-callback 'output-callback))))

(define-command elixir-eval-region (start end) (:region)
  (unless (alive-process-p)
    (editor-error "elixir process doesn't exist."))
  (lem-process:process-send-input *process* (points-to-string start end)))

(define-command run-elixir () ()
  (run-elixir-internal))

(add-hook *exit-editor-hook*
          (lambda ()
            (when *process*
              (lem-process:delete-process *process*))))
