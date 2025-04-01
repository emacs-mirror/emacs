(in-package :lem-core)

(defvar *editor-abort-hook* '())
(defvar *exit-editor-hook* '())

(defun bailout (condition)
  (signal 'exit-editor
          :report (with-output-to-string (stream)
                    (princ condition stream)
                    (uiop:print-backtrace
                     :stream stream
                     :condition condition))))

(defun pop-up-backtrace (condition)
  (let ((o (with-output-to-string (stream)
             (princ condition stream)
             (fresh-line stream)
             (uiop:print-backtrace
              :stream stream
              :count 100))))
    (funcall 'pop-up-typeout-window
             (make-buffer "*EDITOR ERROR*")
             :function (lambda (stream)
                         (format stream "~A" o))
             :erase t)))

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error
                        (lambda (condition)
                          (handler-bind ((error #'bailout))
                            (pop-up-backtrace condition)
                            (redraw-display)))))
         ,@body)
     (error ())))

(defvar *interactive-p* nil)
(defun interactive-p () *interactive-p*)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defmacro save-continue-flags (&body body)
  `(let ((*last-flags* *last-flags*)
         (*curr-flags* *curr-flags*))
     ,@body))

(defun continue-flag (flag)
  (prog1 (cdr (assoc flag *last-flags*))
    (push (cons flag t) *last-flags*)
    (push (cons flag t) *curr-flags*)))

(defun nullify-last-flags (flag &rest more-flags)
  "Set FLAG and MORE-FLAGS to nil in *LAST-FLAGS*."
  (push (cons flag nil) *last-flags*)
  (when more-flags
    (dotimes (i (length more-flags))
      (push (cons (nth i more-flags) nil) *last-flags*))))

(defmacro do-command-loop ((&key interactive) &body body)
  (alexandria:once-only (interactive)
    `(loop :for *last-flags* := nil :then *curr-flags*
           :for *curr-flags* := nil
           :do (let ((*interactive-p* ,interactive)) ,@body))))

(defun fix-current-buffer-if-broken ()
  (unless (eq (window-buffer (current-window))
              (current-buffer))
    (setf (current-buffer) (window-buffer (current-window)))))

(defun command-loop-body ()
  (flet ((redraw ()
           (when (= 0 (event-queue-length))
             (without-interrupts
               (handler-bind ((error #'bailout))
                 (redraw-display)))))

         (read-command-and-call ()
           (let ((cmd (with-idle-timers ()
                        (read-command))))
             (unless (or (eq cmd '<mouse-motion-event>)
                         (eq cmd '<mouse-event>))
               (message nil))
             (call-command cmd nil)))

         (editor-abort-handler (c)
           (declare (ignore c))
           (buffer-mark-cancel (current-buffer)) ; TODO: define handler
           (run-hooks *editor-abort-hook*)
           )

         (editor-condition-handler (c)
           (declare (ignore c))
           (stop-record-key) ; TODO: define handler
           ))

    (redraw)

    (handler-case
        (handler-bind ((editor-abort
                         #'editor-abort-handler)
                       (editor-condition
                         #'editor-condition-handler))
          (let ((*this-command-keys* nil))
            (read-command-and-call)))
      (editor-condition (c)
        (restart-case (error c)
          (lem-restart:message ()
            (let ((message (princ-to-string c)))
              (unless (equal "" message)
                (message "~A" message))))
          (lem-restart:call-function (fn)
            (funcall fn)))))))

(defvar *toplevel-command-loop-p* t)

(defun toplevel-command-loop-p ()
  *toplevel-command-loop-p*)

(defvar *command-loop-counter* 0)

(defun command-loop-counter ()
  *command-loop-counter*)

(defun command-loop ()
  (do-command-loop (:interactive t)
    (incf *command-loop-counter*)
    (if (toplevel-command-loop-p)
        (with-error-handler ()
          (let ((*toplevel-command-loop-p* nil))
            (handler-bind ((editor-condition
                             (lambda (c)
                               (declare (ignore c))
                               (invoke-restart 'lem-restart:message))))
              (command-loop-body))))
        (command-loop-body))
    (fix-current-buffer-if-broken)))

(defun toplevel-command-loop (initialize-function)
  (handler-bind ((exit-editor
                   (lambda (c)
                     (return-from toplevel-command-loop
                       (exit-editor-report c)))))
    (with-error-handler ()
      (funcall initialize-function))
    (with-editor-stream ()
      (command-loop))))

(defun exit-editor (&optional report)
  (run-hooks *exit-editor-hook*)
  (mapc #'disable-minor-mode (active-global-minor-modes))
  (signal 'exit-editor :report report))

(defun call-background-job (function cont)
  (bt2:make-thread
   (lambda ()
     (let ((error-text))
       (handler-case
           (handler-bind ((error (lambda (c)
                                   (setf error-text
                                         (with-output-to-string (stream)
                                           (princ c stream)
                                           (fresh-line stream)
                                           (uiop:print-backtrace
                                            :stream stream
                                            :count 100))))))
             (let ((result (funcall function)))
               (send-event (lambda () (funcall cont result)))))
         (error ()
           (send-event (lambda ()
                         (let ((buffer (make-buffer "*BACKGROUND JOB ERROR*")))
                           (erase-buffer buffer)
                           (insert-string (buffer-point buffer)
                                          error-text)
                           (pop-to-buffer buffer)
                           (buffer-start (buffer-point buffer)))))))))))
