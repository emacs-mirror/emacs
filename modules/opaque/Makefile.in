ROOT = ../..

all: opaque.so opaque.doc

%.so: %.o
	gcc -shared -o $@ $<

%.o: %.c
	gcc -ggdb3 -Wall -I$(ROOT)/src -I$(ROOT)/lib -fPIC -c $<

%.doc: %.c
	$(ROOT)/lib-src/make-docfile $< > $@
