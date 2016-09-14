(require 'seq)

(defun package-makefile--package-dirs (directory)
  (directory-files directory nil "[^.].*"))

(defun package-makefile--target-pkg-el (top-dir base-dir)
  (format
   "%s-pkg: %s/%s/%s-pkg.el

%s/%s/%s-pkg.el: %s
\t$(EMACS) --batch --directory=admin \\
\t\t--load admin/package-build.el \\
\t\t--eval '(package-build-prepare \"%s/%s\")'
"

   base-dir top-dir base-dir base-dir
   top-dir base-dir base-dir
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
   top-dir base-dir))

(defun package-makefile--pkg-targets (top-dir all-dirs)
  (concat
   "pkg-all: "
   (mapconcat
    'identity
    all-dirs
    "-pkg ")
   "-pkg"
   "\n\n"
   (mapconcat
    (lambda (base-dir)
      (package-makefile--target-pkg-el top-dir base-dir))
    all-dirs
    "\n")))

(defun package-makefile--log-target (top-dir base-dir)
  (let* ((fulldir (concat top-dir "/" base-dir))
         (filestem (concat top-dir "/" base-dir "/" base-dir))
         (logfile (concat filestem ".log"))
         (pkgfile (concat filestem "-pkg.el")))
    (format
     "%s: %s
	@$(EMACS) --batch --load admin/assess-discover.el \\
		--eval '(assess-discover-run-and-exit-batch-dir \"%s\")' \\
		$(WRITE_LOG)

%s:
	test ! -f ./%s || mv ./%s ./%s~
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
    "\n")

   "
check-packages: "

   (mapconcat
    (lambda (base-dir)
      (concat top-dir "/" base-dir "/" base-dir ".log"))
    all-dirs
    " ")))

(defun package-makefile--core-packages ()
  (package-makefile--package-dirs "core"))

(defun package-makefile--1 ()
  (concat
   (package-makefile--pkg-targets
    "core"
    (package-makefile--core-packages))
   "\n"

   (package-makefile--test-targets
    "core"
    (package-makefile--core-packages))
   "\n"))


(defun package-makefile ()
  (with-temp-buffer
    (insert
     (package-makefile--1))
    (write-file "gnumakefile-inc.mk")))

;; example: core/example/example-pkg.el
;; core/example/example-pkg.el
;; 	$(EMACS) --batch --load package-build.el --eval '(package-build-prepare "core/example"")'

;; core: core/core-pkg.el

;; core/core-pkg.el:

;; 	$(EMACS) --batch --load package-build.el --eval '(package-build-prepare "core")'

;; core-test:
;; 	$(EMACS) --batch --load package-test.el --eval '(assess-discover-run-and-exit-batch-dir "core")'

;; Rest of core not done yet
