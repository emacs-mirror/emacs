(defpackage :lem-python-mode.run-python
  (:use :cl :lem :lem-python-mode)
  (:export :*python-run-command*
           :run-python))
(in-package :lem-python-mode.run-python)

(defvar *python-run-command* #-win32 "python3"
                             #+win32 '("python" "-i"))

(defvar *process* nil)

(define-major-mode run-python-mode lem-python-mode:python-mode
    (:name "Python"
     :keymap *run-python-mode-keymap*
     :syntax-table lem-python-mode::*python-syntax-table*)
  (reset-listener-variables (current-buffer))
  (lem/listener-mode:start-listener-mode))

(define-key lem-python-mode::*python-mode-keymap* "C-c C-r" 'python-eval-region)

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
    (editor-error "Python process doesn't exist."))
  (lem-process:process-send-input *process*
                                  (concatenate 'string string (string #\newline))))

(defun alive-process-p ()
  (and *process*
       (lem-process:process-alive-p *process*)))

(defun repl-buffer-exists-p ()
  (get-buffer "*python*"))

(defun get-repl-buffer ()
  (let ((buffer (make-buffer "*python*")))
    (unless (eq (buffer-major-mode buffer) 'run-python-mode)
      (change-buffer-mode buffer 'run-python-mode))
    buffer))

(defun output-callback (string)
  (let* ((already-exists (repl-buffer-exists-p))
         (buffer (get-repl-buffer))
         (p (buffer-point buffer)))
    (buffer-end p)
    (setf string (ppcre:regex-replace-all "\\r\\n" string (string #\newline)))
    (insert-string p string)
    (when (or (ppcre:scan "^>>> " (line-string p))
              (ppcre:scan "^... " (line-string p)))
      (lem/listener-mode:refresh-prompt buffer nil))
    (unless already-exists
      (switch-to-window (pop-to-buffer buffer)))
    (alexandria:when-let (window (first (get-buffer-windows buffer)))
      (with-current-window window
        (buffer-end p)
        (window-see window)))
    (redraw-display)))

(defun run-python-internal ()
  (unless (alive-process-p)
    (when *process*
      (lem-process:delete-process *process*))
    (setf *process*
          (lem-process:run-process *python-run-command*
                                   :name "run-python"
                                   :output-callback 'output-callback))))

(define-command python-eval-region (start end) (:region)
  (unless (alive-process-p)
    (editor-error "Python process doesn't exist."))
  (lem-process:process-send-input *process* (points-to-string start end)))

(define-command run-python () ()
  (run-python-internal))

(add-hook *exit-editor-hook*
          (lambda ()
            (when *process*
              (lem-process:delete-process *process*))))
