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



#include "XMenuInt.h"

/*
 * XDeleteAssoc - Delete an association in an XAssocTable keyed on
 * an XId.  An association may be removed only once.  Redundant
 * deletes are meaningless (but cause no problems).
 */
void
XDeleteAssoc(register Display *dpy, register XAssocTable *table, register XID x_id)
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
				/* Remove it from the queue and */
				/* free the entry. */
				emacs_remque((struct qelem *)Entry);
				free((char *)Entry);
				return;
			}
			/* Oops, identical XId's on different displays! */
			continue;
		}
		if (Entry->x_id > x_id) {
			/* We have gone past where it should be. */
			/* It is apparently not in the table. */
			return;
		}
	}
	/* It is apparently not in the table. */
	return;
}
