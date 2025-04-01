(in-package :lem-lisp-mode/internal)

(defvar *file-conversion-map* '()
  "This variable is an alist for converting remote file names to local file names.
Uses include mapping files in docker to files in the local environment.

For example, set the following.
\(setf *file-conversion-map*
      `((\"/app/\" .
         ,(merge-pathnames \"common-lisp/my-project/\" (user-homedir-pathname)))))
")

(defun convert-remote-to-local-file (filename)
  (loop :for (remote-file . local-file) :in *file-conversion-map*
        :do (when (alexandria:starts-with-subseq remote-file filename)
              (return (concatenate 'string local-file (subseq filename (length remote-file)))))
        :finally (return filename)))

(defun convert-local-to-remote-file (filename)
  (loop :for (remote-file . local-file) :in *file-conversion-map*
        :do (when (alexandria:starts-with-subseq local-file filename)
              (return (concatenate 'string remote-file (subseq filename (length local-file)))))
        :finally (return filename)))
