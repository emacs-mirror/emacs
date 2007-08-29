### Makefile for Emacs etc/ directory.

.PHONY: unlock relock

## All non-directory files, if that matters.
SOURCES = [A-Z]* *.* *-* e/* refcards/* tutorials/* \
	images/README images/*.* images/*/README images/*/*.* \
	images/*/*/README images/*/*/*.*

unlock:
	chmod u+w $(SOURCES)

relock:
	chmod u-w $(SOURCES)

## See e/README.
TIC=tic
e/eterm-color: e/eterm-color.ti
	TERMINFO=`pwd`; export TERMINFO; $(TIC) e/eterm-color.ti

## arch-tag: 4261f003-cf77-4478-a10a-5284e9d8f797
### Makefile ends here
