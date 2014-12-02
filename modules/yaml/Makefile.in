ROOT = ../..

CFLAGS  = `pkg-config yaml-0.1 --cflags`
LDFLAGS = `pkg-config yaml-0.1 --libs`

all: yaml.so yaml.doc

%.so: %.o
	gcc -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	gcc -ggdb3 -Wall -I$(ROOT)/src -I$(ROOT)/lib $(CFLAGS) -fPIC -c $<

%.doc: %.c
	$(ROOT)/lib-src/make-docfile $< > $@
