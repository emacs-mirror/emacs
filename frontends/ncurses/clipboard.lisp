(defpackage :lem-ncurses/clipboard
  (:use :cl :alexandria)
  (:export :copy
           :paste))
(in-package :lem-ncurses/clipboard)

(defparameter *unix-copy-commands*
  '(("wl-copy")
    ("xsel" "--input" "--clipboard")
    ("xclip" "-in" "-selection" "clipboard")))

(defparameter *unix-paste-commands*
  '(("wl-paste")
    ("xsel" "--output" "--clipboard")
    ("xclip" "-out" "-selection" "clipboard")))

(defun execute-copy (command text)
  (with-input-from-string (input text)
    (uiop:run-program command :input input))
  (values))

(defun execute-paste (command)
  (with-output-to-string (output)
    (uiop:run-program command :output output)))

(defgeneric copy-aux (os text))
(defgeneric paste-aux (os))

(defclass os () ())

;;;
(defclass mac (os) ())

(defmethod copy-aux ((os mac) text)
  (execute-copy '("pbcopy") text))

(defmethod paste-aux ((os mac))
  (execute-paste '("pbpaste")))

;;;
(defclass unix (os)
  ((copy-command
    :initform nil
    :accessor unix-copy-command)
   (paste-command
    :initform nil
    :accessor unix-paste-command)))

(defun do-command (commands function)
  (dolist (command commands)
    (handler-case (funcall function command)
      (error ())
      (:no-error (&rest values)
        (return (apply #'values command values))))))

(defmethod copy-aux ((os unix) text)
  (if-let (command (unix-copy-command os))
    (execute-copy command text)
    (let ((command (do-command *unix-copy-commands*
                     (rcurry #'execute-copy text))))
      (setf (unix-copy-command os) command))))

(defmethod paste-aux ((os unix))
  (if-let (command (unix-paste-command os))
    (execute-paste command)
    (multiple-value-bind (command text)
        (do-command *unix-paste-commands*
          #'execute-paste)
      (setf (unix-paste-command os) command)
      text)))

;;;
(defclass wsl (os)
  ())

(defmethod copy-aux ((os wsl) text)
  (execute-copy '("clip.exe") text))

(defmethod paste-aux ((os wsl))
  (let ((text (execute-paste '("powershell.exe" "Get-Clipboard"))))
    (setf text (ppcre:regex-replace-all "\\r" text ""))
    (when (and (plusp (length text))
               (char= #\newline (char text (1- (length text)))))
      (setf text (subseq text 0 (1- (length text)))))
    text))

;;;
(defclass windows (os)
  ())

;;;
(defun get-os-name ()
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (or #+darwin
      'mac
      #+unix
      (if (lem:wsl-p) 'wsl 'unix)
      'windows))

(defvar *os*)

(defun os ()
  (if (boundp '*os*)
      *os*
      (setf *os* (make-instance (get-os-name)))))

(defun copy (text)
  (copy-aux (os) text)
  (values))

(defun paste ()
  (paste-aux (os)))
