(uiop:define-package :lem-encodings-table/8bit
  (:use :cl)
  (:export :generate-table))
(in-package :lem-encodings-table/8bit)

(defun map-bytes (fun)
  (loop for c from #x00 to #xff
        do (funcall fun c)))

(defun generate-table (file code)
  (with-open-file (out file 
                       :direction :output 
                       :if-exists :supersede)
    (map-bytes
     (lambda (&rest x)
       (ignore-errors
        (let ((string (iconv:iconv-to-string code (coerce x 'simple-vector))))
          (format out "0x~{~2,'0x~} U+~4,'0x ~A~A~%"
                  x
                  (char-code (aref string 0))
                  (cond ((equal string (format nil "~%")) "\\n")
                        ((equal string " ") "SPC")
                        (t string))
                  (if (ignore-errors
                       (equal x (coerce (iconv:iconv-from-string code string) 'list)))
                      ""
                      (format nil " 0x~{~2,'0x~}" (coerce (iconv:iconv-from-string code string) 'list))))))))))
