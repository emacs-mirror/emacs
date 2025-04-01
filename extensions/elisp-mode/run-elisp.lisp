(defpackage :lem-elisp-mode.run-elisp
  (:use :cl :lem :lem-elisp-mode)
  (:export :*elisp-run-command*
           :run-elisp-mode))

(in-package :lem-elisp-mode.run-elisp)

(defvar *elisp-run-command* "eltr")

(defvar *process* nil)

(define-major-mode run-elisp-mode lem-elisp-mode:elisp-mode
    (:name "Emacs Lisp"
     :keymap *run-elisp-mode-keymap*
     :syntax-table *elisp-syntax-table*)
  (reset-listener-variables (current-buffer))
  (lem/listener-mode:start-listener-mode))

(define-key *elisp-mode-keymap* "C-c C-r" 'elisp-eval-region)
(define-key *elisp-mode-keymap* "C-c C-c" 'elisp-eval-defun)
(define-key *elisp-mode-keymap* "C-c C-l" 'elisp-load-file)


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
    (editor-error "Emacs Lisp Terminal process doesn't exist."))
  (lem-process:process-send-input *process*
                                  (concatenate 'string string (string #\newline))))

(defun alive-process-p ()
  (and *process*
       (lem-process:process-alive-p *process*)))

(defun repl-buffer-exists-p ()
  (get-buffer "*elisp*"))

(defun get-repl-buffer ()
  (let ((buffer (make-buffer "*elisp*")))
    (unless (eq (buffer-major-mode buffer) 'run-elisp-mode)
      (change-buffer-mode buffer 'run-elisp-mode))
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

(defun run-elisp-internal ()
  (unless (alive-process-p)
    (when *process*
      (lem-process:delete-process *process*))
    (setf *process*
          (lem-process:run-process *elisp-run-command*
                                   :name "run-elisp"
                                   :output-callback 'output-callback))))

(define-command elisp-eval-region (start end) (:region)
  (unless (alive-process-p)
    (editor-error "Emacs Lisp Terminal process doesn't exist."))
  (lem-process:process-send-input *process* 
                                  (format nil "~a ~c" (points-to-string start end) #\Newline)))


(define-command elisp-eval-defun () ()
  (unless (alive-process-p)
    (editor-error "Emacs Lisp Terminal process doesn't exist."))
  
  (with-point ((point (current-point)))
    (lem-lisp-syntax:top-of-defun point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (lem-process:process-send-input *process* 
                                      (format nil "~a ~c" (points-to-string start end) #\Newline)))))

(define-command elisp-load-file (filename)
    ((prompt-for-file "Load File: "
                      :directory (or (buffer-filename) (buffer-directory))
                      :default nil
                      :existing t))
  "Load the Lisp file named FILENAME."
  (unless (alive-process-p)
    (editor-error "Emacs Lisp Terminal process doesn't exist."))
  
  (when (uiop:file-exists-p filename)
      (lem-process:process-send-input *process* 
                                      (format nil "(load-file \"~a\")~c" 
                                              filename #\Newline))))


(define-command run-elisp () ()
  (run-elisp-internal))

(add-hook *exit-editor-hook*
          (lambda ()
            (when *process*
              (lem-process:delete-process *process*))))
