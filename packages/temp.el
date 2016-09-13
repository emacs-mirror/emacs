(package-makefile--package-dirs "core")
(package-makefile--package-dirs "/home/phillord/src/git/elpa/master/packages")


(package-makefile--target-pkg-el "core" "example")
(package-makefile--makefile-pkg-targets "core")
(package-makefile--makefile)

(package-build-prepare "core/example")

(defun temp ()
  (interactive)
  (package-makefile))
