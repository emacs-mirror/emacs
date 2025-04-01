(defpackage :lem-lisp-syntax.defstruct-to-defclass
  (:use :cl :lem)
  (:export :defstruct-to-defclass)
  #+sbcl
  (:lock t))
(in-package :lem-lisp-syntax.defstruct-to-defclass)

(defvar *save-points* '())

(defun save-point (point)
  (let ((new-point (copy-point point :left-inserting)))
    (push new-point *save-points*)
    new-point))

(defun cleanup-save-points ()
  (dolist (point *save-points*)
    (delete-point point))
  (setf *save-points* '()))

(defun call-with-temporary-points (function)
  (let ((*save-points* '()))
    (unwind-protect (funcall function)
      (cleanup-save-points))))

(defmacro with-temporary-points (() &body body)
  `(call-with-temporary-points (lambda () ,@body)))

(define-condition scan-failed (editor-error)
  ((point :initarg :point
          :reader scan-failed-point))
  (:report (lambda (c s)
             (format s "Scan failed line ~A column ~A"
                     (line-number-at-point (scan-failed-point c))
                     (point-column (scan-failed-point c))))))

(defun exact (result point)
  (unless result
    (error 'scan-failed :point point))
  result)

(let ((cached-readtable nil))
  (defun safe-readtable ()
    (or cached-readtable
        (let ((readtable (copy-readtable nil)))
          (setf (readtable-case readtable) :preserve)
          (setf cached-readtable readtable)))))

(defun safe-read-from-string (point string &key preserve-p)
  (handler-case
      (let ((*read-eval* nil)
            (*readtable* (if preserve-p
                             (safe-readtable)
                             *readtable*)))
        (read-from-string string))
    (reader-error ()
      (error 'scan-failed :point point))))

(defvar *struct-info*)

(defun forward-token (point &key (case-sensitive-p t))
  (skip-space-and-comment-forward point)
  (cond ((eql (character-at point) #\()
         :list-start)
        ((eql (character-at point) #\))
         :list-end)
        (t
         (let ((string (symbol-string-at-point point)))
           (cond ((null string)
                  nil)
                 (case-sensitive-p
                  string)
                 (t
                  (string-downcase string)))))))

(defun forward-form (point)
  (exact (form-offset point 1) point)
  (with-point ((start point))
    (exact (form-offset start -1) start)
    (let ((*read-eval* nil))
      (safe-read-from-string start (points-to-string start point)))))

(defun enter-list (point)
  (scan-lists point 1 -1))

(defun exit-list (point)
  (scan-lists point 1 1))

(defstruct (slot-description-info (:conc-name slot-description-))
  name
  point
  initial-value-start-point
  initial-value-end-point
  read-only-p
  type-start-point
  type-end-point
  complex-p)

(defstruct (struct-info (:constructor make-struct-info ())
                        (:conc-name struct-))
  start-point
  end-point
  name
  options
  name-and-options-start-point
  name-and-options-end-point
  slot-descriptions)

(defstruct (options-info (:conc-name options-))
  conc-name)

(defun parse-name-and-options (name-and-options)
  (trivia:match name-and-options
    ((cons (trivia:guard name (symbolp name)) options)
     (let ((options-info (make-options-info)))
       (dolist (option options)
         (trivia:match option
           ((or :|conc-name|
                :conc-name
                (list :conc-name)
                (list :|conc-name|))
            (setf (options-conc-name options-info) ""))
           ((list (or :conc-name :|conc-name|)
                  conc-name)
            (unless (symbolp conc-name)
              (return-from parse-name-and-options nil))
            (setf (options-conc-name options-info)
                  conc-name))))
       (values name options-info)))))

(defun scan-defstruct-name-and-options (point)
  (trivia:match (forward-token point)
    ; (defstruct |(structure-name ...
    ((eq :list-start)
     (with-point ((start point)
                  (end point))
       (exact (form-offset end 1) end)
       (multiple-value-bind (structure-name options-info)
           (parse-name-and-options
            (safe-read-from-string start (points-to-string start end)
                                   :preserve-p t))
         (exact (and structure-name options-info) point)
         (setf (struct-name *struct-info*) (string structure-name)
               (struct-options *struct-info*) options-info
               (struct-name-and-options-start-point *struct-info*) (save-point start)
               (struct-name-and-options-end-point *struct-info*) (save-point end)))
       (move-point point end)))
    ; (defstruct |structure-name ...
    ((trivia:guard structure-name (stringp structure-name))
     (setf (struct-name *struct-info*)
           structure-name)
     (setf (struct-name-and-options-start-point *struct-info*)
           (save-point point))
     (form-offset point 1))
    (otherwise
     nil)))

(defun scan-slot-description-option (point slot-info)
  (trivia:match (forward-token point :case-sensitive-p nil)
    (":type"
     (form-offset point 1)
     ; (slot-name ... :type| type)
     (skip-space-and-comment-forward point)
     ; (slot-name ... :type |type)
     (setf (slot-description-type-start-point slot-info) (save-point point))
     (exact (form-offset point 1) point)
     ; (slot-name ... :type type|)
     (setf (slot-description-type-end-point slot-info) (save-point point))
     t)
    (":read-only"
     (form-offset point 1)
     ; (slot-name ... :read-only| boolean)
     (when (forward-form point)
       (setf (slot-description-read-only-p slot-info) t))
     ; (slot-name ... :read-only boolean|)
     t)
    ((eq :list-end)
     nil)
    (otherwise
     nil)))

(defun scan-complex-slot-description (point)
  (flet ((scan-slot-name ()
           (let ((slot-name (forward-token point)))
             (exact (stringp slot-name) point)
             (form-offset point 1)
             slot-name))
         (scan-initform ()
           ; (defstruct structure-name (slot-name |init-form :type integer))
           (with-point ((start point))
             (cond ((form-offset point 1)
                    ; (defstruct structure-name (slot-name init-form| :type integer))
                    (values (save-point start) (save-point point)))
                   (t
                    nil)))))
    (enter-list point)
    (let ((slot-info
            (make-slot-description-info :point (save-point point)
                                        :name (scan-slot-name)
                                        :complex-p t)))
      (skip-space-and-comment-forward point)
      (multiple-value-bind (start end) (scan-initform)
        (cond ((null start)
               (prog1 slot-info
                 (exit-list point)))
              (t
               (setf (slot-description-initial-value-start-point slot-info) start
                     (slot-description-initial-value-end-point slot-info) end)
               (loop :repeat 2 :while (scan-slot-description-option point slot-info))
               (prog1 slot-info
                 (exit-list point))))))))

(defun scan-simple-slot-description (point slot-name)
  (prog1 (make-slot-description-info :name slot-name
                                     :point (save-point point)
                                     :complex-p nil)
    (form-offset point 1)))

(defun scan-forward-slot-description (point)
  (trivia:ematch (forward-token point)
    ((trivia:guard slot-name (stringp slot-name))
     (scan-simple-slot-description point slot-name))
    ((eq :list-start)
     (scan-complex-slot-description point))
    ((eq :list-end)
     nil)
    ((eq nil)
     nil)))

(defun move-to-toplevel (point)
  (loop :while (scan-lists point -1 1 t)))

(defun scan-defstruct (point)
  (unless (eql (character-at point) #\()
    (move-to-toplevel point))
  (when (eql (character-at point) #\() ; |(defstruct
    (enter-list point)
    (let ((token (forward-token point)))
      (when (and (stringp token) (string-equal token "defstruct")) ; (|defstruct
        (setf (struct-start-point *struct-info*)
              (save-point point))
        ; (|defstruct ...
        (exact (form-offset point 1) point)
        ; (defstruct| ...
        (exact (scan-defstruct-name-and-options point) point)
        ; (defstruct structure-name| slot-name ...)
        (loop :for slot-description := (scan-forward-slot-description point)
              :while slot-description
              :do (alexandria:nconcf (struct-slot-descriptions *struct-info*)
                                     (list slot-description)))
        (skip-space-and-comment-forward point)
        (exact (char= (character-at point) #\)) point)
        (setf (struct-end-point *struct-info*)
              (save-point point))
        *struct-info*))))

(defun analyze-defstruct (point *struct-info*)
  (with-point ((point point))
    (scan-defstruct point)))


(defun replace-at-point (point old new)
  (assert (string= old (symbol-string-at-point point)))
  (delete-character point (length old))
  (insert-string point new))

(defun delete-forward-whitespaces (point)
  (loop :while (syntax-space-char-p (character-at point)) :do (delete-character point 1)))

(defun delete-backward-whitespaces (point)
  (let ((n (skip-chars-backward point #'syntax-space-char-p)))
    (delete-character point n)))

(defun just-one-space* (point)
  (assert (eq (point-kind point) :left-inserting))
  (delete-forward-whitespaces point)
  (insert-character point #\space)
  (character-offset point -1))

(defun beginning-of-line-p (point)
  (with-point ((point point))
    (let ((charpos (point-charpos point)))
      (back-to-indentation point)
      (= charpos (point-charpos point)))))

(defun end-of-line-p (point)
  (with-point ((point point))
    (let ((line-number (line-number-at-point point)))
      (skip-space-and-comment-forward point)
      (= line-number (line-number-at-point point)))))

(defun translate-to-defclass-with-info (point struct-info)
  (labels ((conc-name ()
             (or (and (struct-options struct-info)
                      (options-conc-name (struct-options struct-info)))
                 (format nil "~A-" (struct-name struct-info))))
           (emit-initform (already-initform-p)
             (insert-string point ":initform")
             (if already-initform-p
                 (form-offset point 1)
                 (insert-string point " nil")))
           (emit-initarg (slot-info)
             (insert-string point
                            (format nil ":initarg :~A"
                                    (slot-description-name slot-info)))
             (insert-character point #\newline))
           (emit-accessor (slot-info)
             (insert-string point
                            (format nil
                                    ":~:[accessor~;reader~] ~A~A"
                                    (slot-description-read-only-p slot-info)
                                    (conc-name)
                                    (slot-description-name slot-info)))
             (insert-character point #\newline))
           (translate-simple-slot (slot-info)
             (move-point point (slot-description-point slot-info))
             (insert-character point #\()
             (line-end point)
             (when (point<= (struct-end-point struct-info) point)
               (move-point point (struct-end-point struct-info)))
             (insert-character point #\newline)
             (emit-initarg slot-info)
             (emit-initform nil)
             (insert-character point #\newline)
             (emit-accessor slot-info)
             (delete-backward-whitespaces point)
             (insert-character point #\)))
           (translate-complex-slot (slot-info)
             (move-point point (slot-description-point slot-info))
             (form-offset point 1)
             (cond ((null (slot-description-initial-value-start-point slot-info))
                    (insert-character point #\newline)
                    (emit-initarg slot-info)
                    (emit-initform nil))
                   (t
                    (insert-character point #\newline)
                    (emit-initarg slot-info)
                    (just-one-space* point)
                    (emit-initform t)))
             (delete-forward-whitespaces point)
             (insert-character point #\newline)
             (emit-accessor slot-info)
             (loop
               (skip-space-and-comment-forward point)
               (let ((string (symbol-string-at-point point)))
                 (when (and string (string-equal ":read-only" string))
                   (with-point ((start point))
                     (form-offset point 2)
                     (delete-between-points start point)
                     (delete-backward-whitespaces point))))
               (when (and (not (beginning-of-line-p point))
                          (end-of-line-p point))
                 (insert-character point #\newline))
               (unless (form-offset point 2)
                 (return)))
             (delete-backward-whitespaces point)
             (exit-list point))
           (cut-options ()
             (when (struct-options struct-info)
               (delete-between-points (struct-name-and-options-start-point struct-info)
                                      (struct-name-and-options-end-point struct-info))
               (move-point point (struct-name-and-options-start-point struct-info))
               (insert-string point (struct-name struct-info)))))
    (cut-options)
    (move-point point (struct-start-point struct-info))
    (replace-at-point point "defstruct" "defclass")
    (form-offset point 1)
    (insert-character point #\space)
    (insert-string point "()")
    (line-offset point 1)
    (back-to-indentation point)
    (insert-character point #\()
    (dolist (slot-info (struct-slot-descriptions struct-info))
      (if (slot-description-complex-p slot-info)
          (translate-complex-slot slot-info)
          (translate-simple-slot slot-info)))
    (insert-character point #\))
    (indent-points (struct-start-point struct-info)
                   (struct-end-point struct-info))))

(defun defstruct-to-defclass (point)
  (with-temporary-points ()
    (alexandria:when-let ((info (analyze-defstruct point (make-struct-info))))
      (translate-to-defclass-with-info point info))))
