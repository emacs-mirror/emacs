/* Copyright    Massachusetts Institute of Technology    1985	*/

/*

Copyright 1985, 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
M.I.T. makes no representations about the suitability of
this software for any purpose.  It is provided "as is"
without express or implied warranty.

*/



#include <config.h>
#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include "X10.h"

#ifndef NULL
#define NULL 0
#endif

/*
 * XLookUpAssoc - Retrieve the data stored in an XAssocTable by its XId.
 * If an appropriately matching XId can be found in the table the routine will
 * return apointer to the data associated with it. If the XId can not be found
 * in the table the routine will return a NULL pointer.  All XId's are relative
 * to the currently active Display.
 */
caddr_t XLookUpAssoc(Display *dpy,
		     XAssocTable *table,  /* XAssocTable to search in. */
		     XID x_id)		  /* XId to search for. */
{
	int hash;
	register XAssoc *bucket;
	register XAssoc *Entry;

	/* Hash the XId to get the bucket number. */
	hash = x_id & (table->size - 1);
	/* Look up the bucket to get the entries in that bucket. */
	bucket = &table->buckets[hash];
	/* Get the first entry in the bucket. */
	Entry = bucket->next;

	/* Scan through the entries in the bucket for the right XId. */
	for (; Entry != bucket; Entry = Entry->next) {
		if (Entry->x_id == x_id) {
			/* We have the right XId. */
			if (Entry->display == dpy) {
				/* We have the right display. */
				/* We have the right entry! */
				return(Entry->data);
			}
			/* Oops, identical XId's on different displays! */
			continue;
		}
		if (Entry->x_id > x_id) {
			/* We have gone past where it should be. */
			/* It is apparently not in the table. */
			return(NULL);
		}
	}
	/* It is apparently not in the table. */
	return(NULL);
}
