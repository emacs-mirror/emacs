core/example/example.log: core/example/example-pkg.el
	@$(EMACS) --batch --load admin/assess-discover.el \
		--eval '(assess-discover-run-and-exit-batch-dir "core/example")' \
		$(WRITE_LOG)


core/example:
	test ! -f core/example/example.log || mv ./core/example/example.log ./core/example/example.log~
	$(MAKE) core/example/example.log WRITE_LOG=

example: core/example

.PHONY: core/example example


check-packages: core/example/example.log
