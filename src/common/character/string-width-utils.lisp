(defpackage :lem/common/character/string-width-utils
  (:use :cl)
  (:export :+default-tab-size+
           :control-char
           :wide-char-p
           :char-width
           :string-width
           :wide-index))
(in-package :lem/common/character/string-width-utils)

(defconstant +default-tab-size+ 8)

(defparameter *char-replacement*
  (let ((table (make-hash-table)))
    (setf (gethash (code-char 0) table) "^@")
    (setf (gethash (code-char 1) table) "^A")
    (setf (gethash (code-char 2) table) "^B")
    (setf (gethash (code-char 3) table) "^C")
    (setf (gethash (code-char 4) table) "^D")
    (setf (gethash (code-char 5) table) "^E")
    (setf (gethash (code-char 6) table) "^F")
    (setf (gethash (code-char 7) table) "^G")
    (setf (gethash (code-char 8) table) "^H")
    (setf (gethash (code-char 9) table) "^I")
    (setf (gethash (code-char 11) table) "^K")
    (setf (gethash (code-char 12) table) "^L")
    (setf (gethash (code-char 13) table) "^R")
    (setf (gethash (code-char 14) table) "^N")
    (setf (gethash (code-char 15) table) "^O")
    (setf (gethash (code-char 16) table) "^P")
    (setf (gethash (code-char 17) table) "^Q")
    (setf (gethash (code-char 18) table) "^R")
    (setf (gethash (code-char 19) table) "^S")
    (setf (gethash (code-char 20) table) "^T")
    (setf (gethash (code-char 21) table) "^U")
    (setf (gethash (code-char 22) table) "^V")
    (setf (gethash (code-char 23) table) "^W")
    (setf (gethash (code-char 24) table) "^X")
    (setf (gethash (code-char 25) table) "^Y")
    (setf (gethash (code-char 26) table) "^Z")
    (setf (gethash (code-char 27) table) "^[")
    (setf (gethash (code-char 28) table) "^\\")
    (setf (gethash (code-char 29) table) "^]")
    (setf (gethash (code-char 30) table) "^^")
    (setf (gethash (code-char 31) table) "^_")
    (setf (gethash (code-char 127) table) "^?")
    (loop :for i :from 0 :to #xff
          :do (setf (gethash (code-char (+ #xe000 i)) table)
                    (format nil "\\~D" i)))
    table))

(defun control-char (char)
  (gethash char *char-replacement*))

(defun wide-char-p (char)
  (declare (character char))
  (or (char= char #\▼)
      (lem/common/character/icon:icon-code-p (char-code char))
      (lem/common/character/eastasian:eastasian-code-p (char-code char))
      (control-char char)))

(defun char-width (char width &key (tab-size +default-tab-size+))
  (declare (character char) (fixnum width))
  (cond ((char= char #\tab)
         (+ (* (floor width tab-size) tab-size) tab-size))
        ((char= char #\newline)
         0)
        ((control-char char)
         (loop :for char :across (control-char char)
               :do (setf width (char-width char width :tab-size tab-size)))
         width)
        ((wide-char-p char)
         (+ width 2))
        (t
         (+ width 1))))

(defun string-width (string &key (start 0) end (tab-size +default-tab-size+))
  (loop :with width := 0
        :for index :from start :below (or end (length string))
        :for char := (aref string index)
        :do (setq width (char-width char width :tab-size tab-size))
        :finally (return width)))

(defun wide-index (string goal &key (start 0) (tab-size +default-tab-size+))
  (loop :with width := 0
        :for index :from start :below (length string)
        :for char := (aref string index)
        :do (setq width (char-width char width :tab-size tab-size))
            (when (< goal width)
              (return index))))
