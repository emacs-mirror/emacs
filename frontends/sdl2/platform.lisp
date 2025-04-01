(defpackage :lem-sdl2/platform
  (:use :cl)
  (:export :platform
           :linux
           :mac
           :windows
           :get-platform))
(in-package :lem-sdl2/platform)

(defclass platform () ())
(defclass linux (platform) ())
(defclass windows (platform) ())
(defclass mac (platform) ())

(defvar *platform* nil)

(defun get-platform ()
  (or *platform*
      (setf *platform*
            (let ((platform-name (sdl2:platform)))
              (alexandria:switch (platform-name :test #'equal)
                ("Windows"
                 (make-instance 'windows))
                ("Mac OS X"
                 (make-instance 'mac))
                (otherwise
                 (make-instance 'linux)))))))
