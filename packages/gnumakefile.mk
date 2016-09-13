example: core/example/example-pkg.el

core/example/example-pkg.el:
	$(EMACS) --batch --directory=admin \
		--load admin/package-build.el \
		--eval '(package-build-prepare "core/example"")'
