(defpackage :lem-vi-mode/utils
  (:use :cl
        :lem)
  (:import-from :cl-ppcre)
  (:export :change-directory*
           :expand-filename-modifiers
           :kill-region-without-appending
           :save-column))
(in-package :lem-vi-mode/utils)

(defvar *previous-cwd* nil)

(defun change-directory* (new-directory)
  (check-type new-directory (or string pathname))
  (let* ((previous-directory (uiop:getcwd))
         (new-directory (cond
                          ((equal new-directory "")
                           (user-homedir-pathname))
                          ((equal new-directory "-")
                           (or *previous-cwd* previous-directory))
                          (t
                           (truename
                            (merge-pathnames (uiop:ensure-directory-pathname new-directory) previous-directory))))))
    (assert (uiop:absolute-pathname-p new-directory))
    (uiop:chdir new-directory)
    (unless (uiop:pathname-equal *previous-cwd* previous-directory)
      (setf *previous-cwd* previous-directory))
    new-directory))

(defun expand-filename-modifiers (string &optional (base-filename (buffer-filename)))
  (ppcre:regex-replace-all "%(?::[a-z])*"
                           string
                           (lambda (match &rest registers)
                             (declare (ignore registers))
                             (let ((result (or base-filename
                                               (buffer-filename)
                                               (uiop:getcwd))))
                               (ppcre:do-matches-as-strings (flag "(?<=:)([a-z])" match
                                                                  (enough-namestring result))
                                 (setf result
                                       (ecase (aref flag 0)
                                         (#\p (namestring
                                               (uiop:ensure-absolute-pathname result (uiop:getcwd))))
                                         (#\h
                                          (let ((result-path (pathname result)))
                                            (namestring
                                             (if (uiop:directory-pathname-p result-path)
                                                 (uiop:pathname-parent-directory-pathname result-path)
                                                 (uiop:pathname-directory-pathname result-path)))))
                                         (#\t
                                          (let ((result-path (pathname result)))
                                            (namestring
                                             (make-pathname :name (pathname-name result-path)
                                                            :type (pathname-type result-path)))))
                                         (#\r
                                          (namestring
                                           (make-pathname :defaults (pathname result)
                                                          :type nil)))
                                         (#\e (or (pathname-type (pathname result))
                                                  "")))))))
                           :simple-calls t))

(defun kill-region-without-appending (start end)
  "Same as lem:kill-region except this won't append to the existing killring"
  (when (point< end start)
    (rotatef start end))
  (let ((killed-string (delete-character start (count-characters start end))))
    (copy-to-clipboard-with-killring killed-string)))

(defmacro save-column (&body body)
  (let ((column (gensym "COLUMN")))
    `(let ((,column (point-column (current-point))))
       (unwind-protect (progn ,@body)
         (move-to-column (current-point) ,column)))))
