(load "scripts/patch.lisp")

(ql:quickload :rove)

(ql:quickload :lem-tests)

(ql:quickload :lem-ncurses)

(defpackage :lem-documentation-mode/tests
  (:use :cl :lem :lem-documentation-mode/internal)
  (:export))

(in-package :lem-documentation-mode/tests)

(defvar *lem-tests* (make-hash-table :test 'equal))

(defun launch-tests (&key (system :lem-tests)
			  clean
			  show-output)
  (when clean
    (setf *lem-tests* (make-hash-table :test 'equal)))

  (let ((*standard-output* (or (and (not show-output)
				    (make-string-output-stream))
			       *standard-output*)))
    (multiple-value-bind (out tests failures)
	(rove:run system)
      (declare (ignore out))

      (loop :for test :in (nconc tests failures)
	    :do (setf (gethash (rove:test-name test) *lem-tests*)
		      test))

      (when (null failures)
	t))))

(defgeneric test-assertions (test))

(defmethod test-assertions ((assertion rove:failed-assertion))
  (make-instance 'lem-documentation-mode/internal::table-item
		 :values
		 (list 
		  (format nil "~a" (rove:assertion-labels assertion))
		  (str:shorten 100
			       (format nil "~a" (rove:assertion-description assertion)))
		  "<strong> ERROR </strong>")))

(defmethod test-assertions ((assertion rove:passed-assertion))
  (make-instance 'lem-documentation-mode/internal::table-item
		 :values
		 (list 
		  (format nil "~a" (rove:assertion-labels assertion))
		  (str:shorten 100
			       (format nil "~a" (rove:assertion-description assertion)))
		  "<strong> OK </strong>")))

(defmethod test-assertions ((test rove:test))
  (loop :for item :in (concatenate 'list
				   (rove:passed-tests test)
				   (rove:failed-tests test))
	:collect (test-assertions item)))

(defun construct-test-documentation (test-name test)
  (make-instance
   'lem-documentation-mode/internal::table
   :title test-name
   :items (cons (make-instance 'lem-documentation-mode/internal::table-header
			       :values (list "Test" "Description" "Result"))
		(alexandria:flatten
		 (append (loop :for assertion :in (rove:passed-tests test)
			       :collect (test-assertions assertion))

			 (loop :for assertion  :in (rove:failed-tests test)
			       :collect (test-assertions assertion)))))))

(defun construct-global-test-documentation ()
  (unless (> (hash-table-count *lem-tests*) 0)
    (launch-tests :clean t))

  (make-instance 'lem-documentation-mode/internal::chunk
		 :items
		 (loop :for k :being :the :hash-key
		       :using (hash-value v) :of *lem-tests*
		       :collect (construct-test-documentation k v))))

(defmethod generate-markdown-file (filename (type (eql :test)))
  (let* ((buffer (make-buffer nil :temporary t))
         (point (buffer-point buffer)))

    (unless (> (hash-table-count *lem-tests*) 0)
      (launch-tests :clean t :show-output t))

    (erase-buffer buffer)

    (insert-string point
		   (format nil "{{< expand \"~a - ~a\" >}}"
			   (lisp-implementation-type)
			   (lisp-implementation-version)))
    (insert-character point #\newline)
    (insert-string point
		   (format nil
			   "{{< progress title=\"Progress\" value=\"~a\" icon=\"gdoc_info_outline\" >}}"
			   (truncate
			    (* (/ (loop :for k :being :the :hash-value :of *lem-tests*
					:when (rove:passedp k)
					:sum 1)
				  (hash-table-count *lem-tests*))
			       100))))

    (insert-character point #\newline)

    (lem-documentation-mode/internal::generate
     (make-instance 'lem-documentation-mode/internal::markdown-generator)
     (construct-global-test-documentation)
     point)

    (insert-string point "{{< /expand >}}")
    (insert-character point #\newline)

    (alexandria:write-string-into-file (buffer-text buffer)
                                       filename
				       :if-does-not-exist :create
                                       :if-exists :append)))
