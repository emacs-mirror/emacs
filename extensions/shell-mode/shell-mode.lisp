(defpackage :lem-shell-mode
  (:use :cl :lem :alexandria)
  (:export :*default-shell-command*)
  #+sbcl
  (:lock t))
(in-package :lem-shell-mode)

(defvar *default-shell-command* nil
  "Set if you do want to use non default shell. '(\"/usr/local/bin/bash\")")

(defun shell-command ()
  (or *default-shell-command*
      (let ((shell
              (or
               #-windows
               (uiop:getenv "SHELL")
               #+windows
               (let ((windir (uiop:getenv "windir")))
                 (and windir
                      (merge-pathnames "system32/cmd.exe" windir))))))
        (list shell))))

(defun buffer-process (buffer)
  (buffer-value buffer 'process))

(defun process-buffer (process)
  (find process (buffer-list) :key #'buffer-process))

(define-major-mode run-shell-mode nil
    (:name "Shell"
     :keymap *run-shell-mode-keymap*)
  (reset-listener-variable (current-buffer))
  (lem/listener-mode:start-listener-mode))

(defun reset-listener-variable (buffer)
  (setf (variable-value 'lem/listener-mode:listener-set-prompt-function :buffer buffer)
        #'identity
        (variable-value 'lem/listener-mode:listener-check-input-function :buffer buffer)
        (constantly t)
        (variable-value 'lem/listener-mode:listener-execute-function :buffer buffer)
        'execute-input
        (variable-value 'lem/listener-mode:listener-prompt-attribute :buffer buffer)
        nil))

(defmethod lem/listener-mode:clear-listener-using-mode ((mode run-shell-mode) buffer)
  ;; FIXME: It is assumed that PS1 is a single line.
  (with-point ((end (buffer-end-point buffer)))
    (skip-whitespace-backward end)
    (line-start end)
    (unless (= 1 (line-number-at-point end))
      (let ((*inhibit-read-only* t))
        (delete-between-points (buffer-start-point buffer)
                               end))))
  (lem/listener-mode:refresh-prompt buffer nil))

(defun execute-input (point string)
  (setf string (concatenate 'string string (string #\newline)))
  (lem-process:process-send-input (buffer-process (point-buffer point))
                                  string))

(defun delete-shell-buffer (buffer)
  (lem-process:delete-process (buffer-process buffer))
  buffer)

(defun shell-buffer-name (process)
  (format nil "*~A pid:~D*"
          (lem-process::process-name process)
          (async-process::process-pid (lem-process::process-pointer process))))

(defun create-shell-buffer (process)
  (let ((buffer (make-buffer (shell-buffer-name process))))
    (unless (eq (buffer-major-mode buffer) 'run-shell-mode)
      (change-buffer-mode buffer 'run-shell-mode))
    (add-hook (variable-value 'kill-buffer-hook :buffer buffer)
              'delete-shell-buffer)
    (setf (buffer-value buffer 'process) process)
    buffer))

(defun set-link-attribute (start end)
  (with-point ((point start)
               (point2 start))
    (loop :while (point< point end)
          :do (let ((attribute (text-property-at point :sticky-attribute)))
                (character-offset (move-point point2 point) 1)
                (put-text-property
                 point
                 point2
                 :sticky-attribute
                 (if attribute
                     (merge-attribute
                      attribute
                      (ensure-attribute 'lem/link::link-attribute))
                     'lem/link::link-attribute))
                (character-offset point 1)))))

(defun output-callback (process string)
  (when-let* ((buffer (process-buffer process))
              (point (buffer-point buffer)))
    (buffer-end point)
    (with-point ((start point))
      ;; TODO: lisp-modeに依存するのはおかしいので汎用的なパッケージを用意する
      (lem-lisp-mode/internal::insert-escape-sequence-string point string)
      (lem/link::scan-link start point :set-attribute-function 'set-link-attribute))
    (lem/listener-mode:refresh-prompt buffer nil)))

(defmethod execute ((mode run-shell-mode) (command lem/listener-mode:listener-return) argument)
  (if (lem/link::link-at (current-point))
      (lem/link:link-open)
      (call-next-method)))

(defun run-shell-internal ()
  (create-shell-buffer
   (lem-process:run-process (shell-command)
                            :name "shell"
                            :output-callback 'output-callback
                            :output-callback-type :process-input)))

(define-command run-shell () ()
  (switch-to-window
   (pop-to-buffer (run-shell-internal))))
