(defsystem "lem-pdcurses"
  :depends-on ("lem-ncurses")
  :serial t
  :components (#+win32(:file "ncurses-pdcurseswin32")))
