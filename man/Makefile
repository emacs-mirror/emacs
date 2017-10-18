all: texindex

clean:
	rm -f texindex.o texindex

texindex: texindex.o
	$(CC) -o $@ $(LDFLAGS) $(CFLAGS) $?
texindex.o: texindex.c
