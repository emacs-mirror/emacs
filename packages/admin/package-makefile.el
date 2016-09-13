(defvar package-makefile-archives
  '("core")
  "List of directories with packages in them.

Directories can be either relative to the \"packages\" directory
or absolute. The order is important because we want to only build
packages which occur earlier in the list.")

(defvar package-makefile--packages-seen nil
  "List of packages we have already seen.")

(defun package-makefile--package-dirs (directory)
  (directory-files directory nil "[^.].*"))


(defun package-makefile--target-pkg-el (top-dir base-dir)
  (format
   "%s-pkg: %s/%s/%s-pkg.el

%s/%s/%s-pkg.el:
\t$(EMACS) --batch --directory=admin \\
\t\t--load admin/package-build.el \\
\t\t--eval '(package-build-prepare \"%s/%s\")'
"

   base-dir top-dir base-dir base-dir
   top-dir base-dir base-dir
   top-dir base-dir))

(defun package-makefile--makefile-pkg-targets (top-dir)
  (concat
   "pkg-all: "
   (mapconcat
    'identity
    (package-makefile--package-dirs top-dir) "" "-pkg ")
   "\n\n"
   (mapconcat
    (lambda (base-dir)
      (package-makefile--target-pkg-el top-dir base-dir))
    (package-makefile--package-dirs top-dir)
    "\n")))

(defun package-makefile--makefile ()
  (mapconcat
   (lambda (top-dir)
     (package-makefile--makefile-pkg-targets top-dir))
   package-makefile-archives
   "\n"))


(defun package-makefile ()
  (with-temp-buffer
    (insert
     (package-makefile--makefile))
    
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
