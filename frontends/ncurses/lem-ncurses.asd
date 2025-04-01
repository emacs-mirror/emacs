(defsystem "lem-ncurses"
  :depends-on ("cffi"
               "cl-charms"
               "cl-setlocale"
               "lem"
               "lem/extensions")
  :serial t
  :components (#+pdcurses(:file "cl-charms-pdcurseswin32")
               (:file "config")
               (:file "term")
               (:file "clipboard")
               (:file "style")
               (:file "key")
               (:file "attribute")
               (:file "drawing-object")
               (:file "view")
               (:file "render")
               (:file "input")
               (:file "mainloop")
               (:file "ncurses")))
