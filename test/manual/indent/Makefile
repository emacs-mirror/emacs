RM=rm
EMACS=../../../src/emacs

all: clean $(addsuffix .test,$(wildcard *.*))

clean:
	-$(RM) -f *.new

# TODO:
# - mark the places where the indentation is known to be incorrect,
#   and allow either ignoring those errors or not.
%.test: %
	$(EMACS) --batch $< \
	    --eval '(indent-region (point-min) (point-max) nil)' \
	    --eval '(write-region (point-min) (point-max) "$<.new")'
	diff -u -B $< $<.new
