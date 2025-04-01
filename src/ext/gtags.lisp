(defpackage :lem/gtags
  (:use :cl :lem :lem/language-mode)
  (:export :gtags-definition-list
           :gtags-find-definitions
           :gtags-find-references)
  #+sbcl
  (:lock t))
(in-package :lem/gtags)

(defclass content ()
  ((name
    :initarg :name
    :reader content-name)
   (file
    :initarg :file
    :reader content-file)
   (line-number
    :initarg :line-number
    :reader content-line-number)
   (desc
    :initarg :desc
    :reader content-desc)))

(defclass reference-content (content) ())

(defmethod xref-insert-content ((content content) point level)
  (insert-string point (content-name content) :attribute 'xref-content-attribute))

(defmethod xref-insert-content ((content reference-content) point level)
  (insert-string point (content-file content) :attribute 'lem/peek-source:filename-attribute)
  (insert-string point ":")
  (insert-string point
                 (princ-to-string (content-line-number content))
                 :attribute 'lem/peek-source:position-attribute)
  (insert-string point ":")
  (insert-string point (content-desc content)))

(defun parse-line (line)
  (ppcre:register-groups-bind (name line-number file desc)
      ("^(\\S+)\\s+(\\S+)\\s+(\\S+)\\s+(.*)$" line)
    (when (and name line-number file desc)
      (list name (parse-integer line-number) file desc))))

(defun parts-name (parts) (first parts))
(defun parts-line-number (parts) (second parts))
(defun parts-file (parts) (third parts))
(defun parts-desc (parts) (fourth parts))

(defun global (directory &rest args)
  (with-output-to-string (out)
    (uiop:run-program (cons "global" args)
                      :directory directory
                      :output out
                      :ignore-error-status t)))

(defun parse-global-output (text)
  (with-input-from-string (in text)
    (loop :for line := (read-line in nil)
          :while line
          :for parts := (parse-line line)
          :when parts
          :collect parts)))

(defun result-to-xref-locations (text content-name)
  (loop :for (name line-number file desc) :in (parse-global-output text)
        :collect (make-xref-location :filespec (merge-pathnames file (buffer-directory))
                                     :position (lem/language-mode::make-position line-number 0)
                                     :content (make-instance content-name
                                                             :name name
                                                             :line-number line-number
                                                             :file file
                                                             :desc desc))))

(defun read-name (point prompt)
  (or (symbol-string-at-point point)
      (prompt-for-string prompt :history-symbol 'read-name)))

(defun gtags-find-definitions (point)
  (let* ((name (read-name point "gtags -x "))
         (text (global (buffer-directory) "-x" name)))
    (display-xref-locations (result-to-xref-locations text 'content))))

(defun gtags-find-references (point)
  (let* ((name (read-name point "gtags -rx "))
         (text (global (buffer-directory (point-buffer point)) "-rx" name))
         (locations (result-to-xref-locations text 'reference-content)))
    (display-xref-references (make-xref-references :locations locations))))

(defun gtags-path ()
  (string-right-trim
   '(#\newline)
   (with-output-to-string (out)
     (uiop:run-program '("global" "-p")
                       :output out
                       :ignore-error-status t))))

(defun fetch-gtags-definitions (basedir)
  (loop :for filename
        :in (sort (append (directory (merge-pathnames "*.c" basedir))
                          (directory (merge-pathnames "*.h" basedir)))
                  #'string<
                  :key #'file-namestring)
        :append (parse-global-output (global basedir "-f" (file-namestring filename)))))

(defun gtags-definition-list-cont (basedir parts-list)
  (let ((max-len (loop :for parts :in parts-list
                       :for name := (parts-name parts)
                       :maximize (+ 3 (length name)))))
    (lem/peek-source:with-collecting-sources (collector)
      (dolist (parts parts-list)
        (let ((name (parts-name parts))
              (file (parts-file parts))
              (linum (parts-line-number parts)))
          (lem/peek-source:with-appending-source
              (p :move-function (lambda ()
                                  (alexandria:when-let ((buffer (or (get-buffer file)
                                                                    (find-file-buffer
                                                                     (merge-pathnames file basedir)))))
                                    (move-to-line (buffer-point buffer) linum))))
            (insert-string p name)
            (move-to-column p max-len t)
            (insert-string p file :attribute 'lem/peek-source:filename-attribute)
            (insert-string p ":")
            (insert-string p (princ-to-string linum)
                           :attribute 'lem/peek-source:position-attribute))))))
  (redraw-display))

(define-command gtags-definition-list () ()
  (let ((basedir (buffer-directory)))
    (call-background-job (lambda ()
                           (fetch-gtags-definitions basedir))
                         (alexandria:curry #'gtags-definition-list-cont basedir))))
