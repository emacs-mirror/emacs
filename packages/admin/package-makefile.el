(require 'seq)

(defvar package-makefile--elpa
  (when (getenv "ELPA")
    (concat (getenv "ELPA")
            "/packages")))

(defvar package-makefile--log-targets nil)
(defvar package-makefile--pkg-targets nil)

(defun package-makefile--package-dirs (directory)
  (directory-files directory nil "[^.].*"))

(defun package-makefile--target-pkg-el (top-dir base-dir)
  (let ((pkgfile (concat top-dir "/" base-dir "/" base-dir "-pkg.el")))
    (add-to-list
     'package-makefile--pkg-targets pkgfile)
    (format
     "%s-pkg: %s

%s: %s
\t$(EMACS) --batch --directory=admin \\
\t\t--directory=%s/%s \\
\t\t--load admin/package-build.el \\
\t\t--eval '(package-build-prepare \"%s/%s\")'
"

     base-dir pkgfile
     pkgfile
     (mapconcat
      (lambda (n)
        (concat top-dir "/" base-dir "/" n))
      (seq-remove
       (lambda (n)
         (or
          (string-match-p
           ".*-autoloads.el" n)
          (string-match-p
           ".*-pkg.el" n)))
       (directory-files
        (concat top-dir "/" base-dir)
        nil
        ".*el$"))
      " ")
     top-dir base-dir
     top-dir base-dir)))

(defun package-makefile--pkg-targets (top-dir all-dirs)
  (mapconcat
   (lambda (base-dir)
     (package-makefile--target-pkg-el top-dir base-dir))
   all-dirs
   "\n"))

(defun package-makefile--log-target (top-dir base-dir)
  (let* ((fulldir (concat top-dir "/" base-dir))
         (filestem (concat top-dir "/" base-dir "/" base-dir))
         (logfile (concat filestem ".log"))
         (pkgfile (concat filestem "-pkg.el")))
    (add-to-list
     'package-makefile--log-targets logfile)
    (format
     "%s: %s
	$(EMACS) --batch --load admin/assess-discover.el \\
		--eval '(assess-discover-run-and-exit-batch-dir \"%s\")' \\
		$(WRITE_LOG)

%s:
	test ! -f %s || mv %s %s~
	$(MAKE) %s WRITE_LOG=

%s: %s

.PHONY: %s %s
"
     logfile pkgfile
     fulldir
     fulldir
     logfile logfile logfile
     logfile
     base-dir fulldir
     fulldir base-dir
  )))

(defun package-makefile--test-targets (top-dir all-dirs)
  (concat
   (mapconcat
    (lambda (base-dir)
      (package-makefile--log-target top-dir base-dir))
    all-dirs
    "\n")))

(defun package-makefile--check-packages-target (targets)
  (concat
    "check-packages: ")
   (mapconcat
    (lambda (base-dir)
      (concat top-dir "/" base-dir "/" base-dir ".log"))
    all-dirs
    " "))

(defun package-makefile--core-packages ()
  (package-makefile--package-dirs "core"))


(defun package-makefile--elpa-dirs (elpa-package-dir)
  (seq-remove
   (lambda (n)
     (string= "elpa.rss" n))
   (package-makefile--package-dirs elpa-package-dir)))


(defun package-makefile--1 ()
  (let ((package-makefile--log-targets nil)
        (package-makefile--pkg-targets nil))
    (concat
     (package-makefile--pkg-targets
      "core"
      (package-makefile--core-packages))
     "\n"

     (package-makefile--test-targets
      "core"
      (package-makefile--core-packages))
     "\n"

     (when package-makefile--elpa
       (concat
        (package-makefile--pkg-targets
         package-makefile--elpa
         (package-makefile--elpa-dirs
          package-makefile--elpa))
        "\n"

        (package-makefile--test-targets
         package-makefile--elpa
         (package-makefile--elpa-dirs
          package-makefile--elpa))))


     "ert-summarize: " (mapconcat 'identity package-makefile--log-targets " ")
     "\n\t$(EMACS) --batch -l ert -f ert-summarize-tests-batch-and-exit $^\n\n"

     "pkg-all: " (mapconcat 'identity package-makefile--pkg-targets " ")
     "\n\n"
     "check-packages: " (mapconcat 'identity package-makefile--log-targets " "))))

(defun package-makefile ()
  (with-temp-buffer
    (insert
     (package-makefile--1))
    (write-file "makefile-inc.mk")))

;; example: core/example/example-pkg.el
;; core/example/example-pkg.el
;; 	$(EMACS) --batch --load package-build.el --eval '(package-build-prepare "core/example"")'

;; core: core/core-pkg.el

;; core/core-pkg.el:

;; 	$(EMACS) --batch --load package-build.el --eval '(package-build-prepare "core")'

;; core-test:
;; 	$(EMACS) --batch --load package-test.el --eval '(assess-discover-run-and-exit-batch-dir "core")'

;; Rest of core not done yet
