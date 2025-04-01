(defpackage :lem/common/character/icon
  (:use :cl)
  (:export :register-icon
           :register-icon-ext
           :icon-code-p
           :icon-string
           :icon-string-by-ext
           :icon-value))
(in-package :lem/common/character/icon)

(defvar *icon-name-table* (make-hash-table :test 'equal))
(defvar *icon-code-table* (make-hash-table :test 'eql))
(defvar *icon-ext-table* (make-hash-table :test 'equal))

(defstruct icon
  name
  code
  plist)

(defun register-icon (name code &rest plist)
  (let ((icon (make-icon :name name :code code :plist plist)))
    (setf (gethash code *icon-code-table*) icon)
    (setf (gethash name *icon-name-table*) icon)
    icon))

(defun register-icon-ext (ext name)
  (setf (gethash ext *icon-ext-table*) name))

(defun icon-code-p (code)
  (not (null (gethash code *icon-code-table*))))

(defun icon-string (name)
  (let ((icon (gethash name *icon-name-table*)))
    (when icon
      (string (code-char (icon-code icon))))))

(defun icon-string-by-ext (ext)
  (let ((name (gethash ext *icon-ext-table*)))
    (icon-string name)))

(defun icon-value (code key)
  (let ((icon (gethash code *icon-code-table*)))
    (when icon
      (getf (icon-plist icon) key))))

(register-icon "folder" #x1f4c1)
(register-icon "lock" #x1F512)
(register-icon "right-pointing-triangle" #x25B8)
(register-icon "down-pointing-triangle" #x25BE)
