(in-package :lem-core)

(defparameter *deployed* nil)

(defun lem-relative-pathname (pathname)
  (when *deployed*
    (truename (merge-pathnames pathname
			       #+sbcl
                               (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*))
			       #-sbcl
                               (uiop:pathname-directory-pathname (uiop/os:getcwd))))))

(defun get-pid ()
  #+sbcl
  (sb-posix:getpid)
  #+ccl
  (ccl::getpid)
  #+lispworks
  (progn
    #+win32 (win32:get-current-process-id)
    #-win32 (system::getpid)))

(defun exist-program-p (program)
  (let ((status
          (nth-value 2
                     (uiop:run-program (list "which" program)
                                       :ignore-error-status t))))
    (= status 0)))

(defun open-external-file (pathname)
  #+linux
  (uiop:launch-program (list "xdg-open" (namestring pathname)))
  #+darwin
  (uiop:launch-program (list "open" (namestring pathname)))
  #+windows
  (uiop:launch-program (list "explorer" (namestring pathname)) :ignore-error-status t))
