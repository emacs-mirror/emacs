(uiop:define-package :lem-encodings-table/sjis
  (:use :cl)
  (:export :generate-table))
(in-package :lem-encodings-table/sjis)

(defun map-bytes (fun)
  "from https://en.wikipedia.org/wiki/Shift_JIS#Shift_JISx0213_and_Shift_JIS-2004"
  (loop for c from #x00 to #x7f
        do (funcall fun c))
  (loop for c from #xa1 to #xdf
        do (funcall fun c))
  (loop for c1 from #x81 to #x9f
        do (loop for c2 from #x40 to #xfc
                 unless (= c2 #x7f)
                 do (funcall fun c1 c2)))
  (loop for c1 from #xe0 to #xfc
        do (loop for c2 from #x40 to #xfc
                 unless (= c2 #x7f)
                 do (funcall fun c1 c2))))

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
