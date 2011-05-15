(defun semanticdb-ebrowse-run-tests ()
  "Run some tests of the semanticdb-ebrowse system.
All systems are different.  Ask questions along the way."
  (interactive)
  (let ((doload nil))
    (when (y-or-n-p "Create a system database to test with? ")
      (call-interactively 'semanticdb-create-ebrowse-database)
      (setq doload t))
    ;;	Should we load in caches
    (when (if doload
              (y-or-n-p "New database created.	Reload system databases? ")
            (y-or-n-p "Load in all system databases? "))
      (semanticdb-load-ebrowse-caches)))
  ;; Ok, databases were creatd.	 Lets try some searching.
  (when (not (or (eq major-mode 'c-mode)
              (eq major-mode 'c++-mode)))
    (error "Please make your default buffer be a C or C++ file, then
run the test again..")))
