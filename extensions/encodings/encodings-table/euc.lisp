(uiop:define-package :lem-encodings-table/euc
  (:use :cl)
  (:export :generate-table))
(in-package :lem-encodings-table/euc)

(defun map-bytes (fun &key country)
  "from https://en.wikipedia.org/wiki/Extended_Unix_Code"
  (loop for c from #x00 to #x7f
        do (funcall fun c))
  (loop for b1 from #xa1 to #xfe
        do (loop for b2 from #xa1 to #xfe
                 do (funcall fun b1 b2)))
  (cond ((eql country :jp)
         (loop for b1 in '(#x8e)
               do (loop for b2 from #xa1 to #xfe
                        do (funcall fun b1 b2)))
         (loop for b1 in '(#x8f)
               do (loop for b2 from #xa1 to #xfe
                        do (loop for b3 from #xa1 to #xfe
                                 do (funcall fun b1 b2 b3)))))))

(defun generate-table (file code &key country)
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
                      (format nil " 0x~{~2,'0x~}" (coerce (iconv:iconv-from-string code string) 'list)))))))
     :country country)))
