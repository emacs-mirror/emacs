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

;; example: core/example/example-pkg.el
;; core/example/example-pkg.el
;; 	$(EMACS) --batch --load package-build.el --eval '(package-build-prepare "core/example"")'

;; core: core/core-pkg.el

;; core/core-pkg.el:

;; 	$(EMACS) --batch --load package-build.el --eval '(package-build-prepare "core")'

;; core-test:
;; 	$(EMACS) --batch --load package-test.el --eval '(assess-discover-run-and-exit-batch-dir "core")'

;; Rest of core not done yet
