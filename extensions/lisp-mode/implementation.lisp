(defpackage :lem-lisp-mode/implementation
  (:use :cl :lem)
  (:export :default-command
           :set-default-command
           :get-usable-commands))
(in-package :lem-lisp-mode/implementation)

(defparameter *default-command* nil)

(defun default-command ()
  (or *default-command*
      (first (get-usable-commands))))

(defun set-default-command (command)
  (setf *default-command* command))

(defun roswell-installed-p ()
  (exist-program-p "ros"))

(defun qlot-installed-p ()
  (exist-program-p "qlot"))

(defun list-installed-implementations-with-roswell ()
  (uiop:split-string (string-trim '(#\newline #\space)
                                  (uiop:run-program '("ros" "list" "installed")
                                                    :output :string))
                     :separator '(#\newline)))

(defun list-roswell-commands ()
  (when (roswell-installed-p)
    (cons "ros run"
          (loop :for implementation :in (list-installed-implementations-with-roswell)
                :collect (format nil "ros -L ~A run" implementation)))))

(defun list-roswell-with-qlot-commands ()
  (when (and (roswell-installed-p)
             (qlot-installed-p))
    (cons "qlot exec ros run"
          (loop :for implementation :in (list-installed-implementations-with-roswell)
                :collect (format nil "qlot exec ros -L ~A run" implementation)))))

(defun list-installed-implementations ()
  (when (exist-program-p "sbcl")
    (list "sbcl")))

(defstruct cache
  value
  time)

(defvar *cache-get-usable-commands* nil)

(defun valid-cache-p ()
  (when (and *cache-get-usable-commands*
             (< (- (get-universal-time)
                   (cache-time *cache-get-usable-commands*)) 5))
    (cache-value *cache-get-usable-commands*)))

(defun get-usable-commands ()
  (or (valid-cache-p)
      (let ((value (append (list-roswell-commands)
                           (list-roswell-with-qlot-commands)
                           (list-installed-implementations))))
        (setf *cache-get-usable-commands*
              (make-cache :value value
                          :time (get-universal-time)))
        value)))
