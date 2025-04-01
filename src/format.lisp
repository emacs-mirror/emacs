(in-package :lem-core)

(defvar *auto-format* nil
  "Set to true to format on save.")

;; Formatter methods for lem/format.
;; You don't need to use this directly, the `register-formatter` will do it for you.
(defgeneric formatter-impl (mode buffer))

(defmacro register-formatter (mode handler)
  "Register a formatter for a mode. `handler' is a function that takes a buffer as argument."
  `(defmethod formatter-impl ((mode (eql (quote ,mode))) buffer)
     (funcall ,handler buffer)))

(defmacro register-formatters (&body bindings)
  "Register multiple formatters at once."
  `(progn ,@(mapcar
             (lambda (binding)
               `(register-formatter
                 ,(first binding)
                 ,(second binding)))
             bindings)))

(defun save-without-hooks (buffer)
  "Bypass hooks to avoid infinite looping."
  (write-to-file-without-write-hook buffer (buffer-filename buffer)))

(defun format-buffer (&key buffer auto)
  "Write unsaved changes and format this buffer.

See the interactive command `format-current-buffer'."
  (let* ((buf (or buffer (current-buffer)))
         (mode (buffer-major-mode buf)))
    ;; early return if no file
    (unless (buffer-filename buf)
      (message "No file to format.")
      (return-from format-buffer))
    ;; save if not run by after-save-hook
    (unless auto (save-without-hooks buf))
    ;; try formatting
    (handler-case (formatter-impl mode buf)
      (error (c)
        (declare (ignore c))
        (unless auto (message "No formatter for mode ~a" mode))))
    ;; save formatted (might make formatter responsible for this)
    (save-without-hooks buf)))

;; When `*auto-format*` is true, try to format a buffer when it is saved.
(add-hook (variable-value 'after-save-hook :global t)
          (lambda (buffer)
            (when *auto-format*
              (format-buffer :buffer buffer :auto t))))
