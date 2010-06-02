RM=rm
EMACS=emacs

clean:
	-$(RM) *.test

# TODO:
# - mark the places where the indentation is known to be incorrect,
#   and allow either ignoring those errors or not.
%.test: %
	-$(RM) $<.test
	$(EMACS) --batch $< \
	    --eval '(indent-region (point-min) (point-max) nil)' \
	    --eval '(write-region (point-min) (point-max) "$<.test")'
	diff -u -B $< $<.test
