(defpackage :lem-lisp-mode/package-inferred-system
  (:use :cl :lem :lem-lisp-mode/internal)
  (:import-from :trivial-types
                :property-list-p)
  (:import-from :alexandria
                :make-keyword
                :if-let
                :when-let
                :lastcar))
(in-package :lem-lisp-mode/package-inferred-system)

(defstruct project-root
  asd-file
  name
  pathname)

(defun root-directory-p (directory)
  (uiop:pathname-equal (uiop:pathname-parent-directory-pathname directory)
                       directory))

(defun join-strings (delimitor strings)
  (with-output-to-string (out)
    (loop :for (string . rest) :on strings
          :do (write-string string out)
              (when rest (princ delimitor out)))))

(defun empty-buffer-p (buffer)
  (point= (buffer-start-point buffer)
          (buffer-end-point buffer)))

(defun list-asd-files (directory)
  (directory (make-pathname :name :wild
                            :type "asd"
                            :defaults directory)))

(defun ensure-system-name (value)
  (etypecase value
    (string value)
    (symbol (string-downcase value))))

(defun get-project-roots-from-asd-file (asd-file)
  (let ((asdf-package (find-package :asdf-user))
        (eof-value '#:eof-value))
    (labels ((read-form (stream)
               (let ((*package* asdf-package))
                 (let ((form (handler-case (read stream nil eof-value)
                               (reader-error ()
                                 (return-from get-project-roots-from-asd-file
                                   nil)))))
                   (values form
                           (not (eq form eof-value))))))
             (eof-value-p (value)
               (eq value eof-value)))
      (with-open-file (in asd-file)
        (loop :with *read-eval* := nil
              :for form := (read-form in)
              :until (eof-value-p form)
              :when (and (consp form)
                         (eq 'asdf:defsystem (first form))
                         (property-list-p (cddr form))
                         (eq :package-inferred-system (getf (cddr form) :class)))
              :collect (destructuring-bind (&key pathname &allow-other-keys)
                           (cddr form)
                         (make-project-root
                          :name (ensure-system-name (second form))
                          :asd-file asd-file
                          :pathname pathname)))))))

(defun find-project-roots-from-working-directory (directory)
  (labels ((recursive (directory)
             (let ((asd-files (list-asd-files directory)))
               (cond (asd-files
                      (loop :for asd-file :in asd-files
                            :append (get-project-roots-from-asd-file asd-file)))
                     ((not (root-directory-p directory))
                      (recursive (uiop:pathname-parent-directory-pathname directory)))
                     (t
                      nil)))))
    (recursive (uiop:pathname-directory-pathname directory))))

(defun infer-package-name-1 (project-root lisp-file)
  (labels ((infer (project-root lisp-file)
             ;; (project-root-asd-file project-root) =>
             ;;   #P"/common-lisp/project-root/project-root.asd"
             ;; lisp-file => #P"/common-lisp/project-root/tests/a/b.lisp
             (let ((dir1 (pathname-directory (project-root-asd-file project-root)))
                   (dir2 (pathname-directory lisp-file)))
               (assert (eq :absolute (first dir1)))
               (assert (eq :absolute (first dir2)))
               (let ((mismatch (or (mismatch dir1 dir2 :test #'equal)
                                   (length dir1))))
                 (unless (or (eql mismatch (length dir1))
                             (null mismatch))
                   (return-from infer nil))
                 (if-let (pathname (project-root-pathname project-root))
                   (let ((prefix-directory (pathname-directory
                                            (uiop:ensure-directory-pathname pathname))))
                     ; prefix-directory => (:relative "tests")
                     (when (eq :relative (first prefix-directory))
                       (let* ((relative-lisp-file-directory (subseq dir2 mismatch))
                              (mismatch2
                                (or (mismatch relative-lisp-file-directory
                                              (rest prefix-directory)
                                              :test #'equal)
                                    (length (rest prefix-directory)))))
                         ;; defsystem内に:pathname "tests"とあった場合
                         ;; project-root/tests/a/bの/a/bだけを残す
                         (when (eql mismatch2 (length (rest prefix-directory)))
                           (append (list (project-root-name project-root))
                                   (subseq relative-lisp-file-directory mismatch2)
                                   (list (pathname-name lisp-file)))))))
                   (append (list (project-root-name project-root))
                           (subseq dir2 mismatch)
                           (list (pathname-name lisp-file))))))))
    (when-let (names (infer project-root lisp-file))
      (join-strings "/" names))))

(defun infer-package-name (lisp-file)
  (loop :for project-root :in (find-project-roots-from-working-directory lisp-file)
        :when (infer-package-name-1 project-root lisp-file)
        :return :it))

(defun ensure-keyword (package-name)
  (etypecase package-name
    (string (make-keyword (string-upcase package-name)))
    (symbol (if (keywordp package-name)
                package-name
                (ensure-keyword (string package-name))))))

(defun replace-buffer-text-to-defpackage (buffer package-name)
  (let ((package-name (ensure-keyword package-name)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (with-open-stream (stream (make-buffer-output-stream (buffer-point buffer) nil))
        (let ((*print-case* :downcase))
          (dolist (form (list `(defpackage ,package-name
                                 (:use :cl))
                              `(in-package ,package-name)))
            (prin1 form stream)
            (terpri stream)))))))

(defmethod execute-find-file (executor (mode (eql 'lisp-mode)) pathname)
  (let ((buffer (call-next-method)))
    (when (empty-buffer-p buffer)
      (when-let (package-name (infer-package-name (buffer-filename buffer)))
        (replace-buffer-text-to-defpackage buffer package-name)))
    buffer))
