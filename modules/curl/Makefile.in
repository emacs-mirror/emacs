ROOT = ../..

CFLAGS  = `pkg-config libcurl --cflags`
LDFLAGS = `pkg-config libcurl --libs`

all: curl.so curl.doc

%.so: %.o
	gcc -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	gcc -ggdb3 -Wall -I$(ROOT)/src -I$(ROOT)/lib $(CFLAGS) -fPIC -c $<

%.doc: %.c
	$(ROOT)/lib-src/make-docfile $< > $@
