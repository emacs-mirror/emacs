(uiop:define-package :lem-encodings-table/main
  (:use :cl)
  (:export :generate-table))
(in-package :lem-encodings-table/main)

(defun generate-table ()
  (lem-encodings-table/sjis:generate-table 
   (asdf:system-relative-pathname :lem-encodings "cp932.table")
   "CP932")
  (lem-encodings-table/euc:generate-table
   (asdf:system-relative-pathname :lem-encodings "gb2312.table")
   "GB2312")
  (lem-encodings-table/euc:generate-table 
   (asdf:system-relative-pathname :lem-encodings "euc-jp.table")
   "EUC-JP" :country :jp)
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "koi8-u.table")
   "KOI8-U")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-2.table")
   "ISO-8859-2")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-5.table")
   "ISO-8859-5")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-6.table")
   "ISO-8859-6")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-7.table")
   "ISO-8859-7")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-8.table")
   "ISO-8859-8")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-9.table")
   "ISO-8859-9")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "iso-8859-13.table")
   "ISO-8859-13")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "koi8-r.table")
   "KOI8-R")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "cp866.table")
   "CP866")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "cp1250.table")
   "CP1250")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "cp1251.table")
   "CP1251")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "cp1253.table")
   "CP1253")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "cp1254.table")
   "CP1254")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "cp1255.table")
   "CP1255")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "cp1256.table")
   "CP1256")
  (lem-encodings-table/8bit:generate-table
   (asdf:system-relative-pathname :lem-encodings "cp1257.table")
   "CP1257"))
