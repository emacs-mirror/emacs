(defsystem "lem-encodings"
  :depends-on ("lem")
  :class :package-inferred-system
  :components ((:file "table")
               (:file "8bit")
               (:file "gb2312")
               (:file "euc-jp")
               (:file "cp932")
               (:file "iso-8859-1")
               (:file "utf-8")
               (:file "utf-16")))
