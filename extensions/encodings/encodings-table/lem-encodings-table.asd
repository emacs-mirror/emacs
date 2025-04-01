(defsystem "lem-encodings-table"
  :depends-on ("iconv")
  :class :package-inferred-system
  :components ((:file "8bit")
               (:file "euc")
               (:file "sjis")
               (:file "main")))
