RM=rm
EMACS=emacs

clean:
	-$(RM) *.test

# TODO:
# - mark the places where the indentation is known to be incorrect,
#   and allow either ignoring those errors or not.
%.test: %
	-$(RM) $<.new
	$(EMACS) --batch $< \
	    --eval '(indent-region (point-min) (point-max) nil)' \
	    --eval '(write-region (point-min) (point-max) "$<.new")'
	diff -u -B $< $<.new
