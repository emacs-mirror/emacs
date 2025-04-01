(in-package :lem-core)

(defun wsl-p ()
  "Return t when we are using WSL."
  (zerop (nth-value 2 (uiop:run-program '("which" "clip.exe") :ignore-error-status t))))

(defun sbcl-2.0.0-or-later-p ()
  "Return t when we are using SBCL 2.0.0 or later."
  (and (string-equal "sbcl" (lisp-implementation-type))
       (let ((version (mapcar #'parse-integer
                              (uiop:split-string (lisp-implementation-version)
                                                 :separator "."))))
         (trivia:match version
           ((cons major _)
            (<= 2 major))))))

(defparameter *enable-clipboard-p*
  (ignore-errors
    #+darwin (sbcl-2.0.0-or-later-p)
    #-darwin (not (wsl-p))))

(defmacro with-enable-clipboard (value &body body)
  "Execute BODY with clipboard enabled/disabled.

Argument VALUE is a boolean, and it will be set to enable/disable the clipboard."
  `(let ((*enable-clipboard-p* ,value))
     ,@body))

(defun enable-clipboard ()
  "Enable clipboard."
  (setf *enable-clipboard-p* t))

(defun disable-clipboard ()
  "Disable clipboard."
  (setf *enable-clipboard-p* nil))

(defun enable-clipboard-p ()
  "Return t if clipboard is enabled."
  *enable-clipboard-p*)

(defun copy-to-clipboard (string)
  "Save STRING to clipboard, so it lives on top of the stack."
  (lem-if:clipboard-copy (implementation) string))

(defun get-clipboard-data ()
  "Return the clipboard data."
  (lem-if:clipboard-paste (implementation)))
