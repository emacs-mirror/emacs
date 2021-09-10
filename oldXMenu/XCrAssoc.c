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
#include <stdlib.h>
#include <X11/Xlib.h>
#include <errno.h>
#include "X10.h"

#ifndef NULL
#define NULL 0
#endif

/*
 * XCreateAssocTable - Create an XAssocTable.  The size argument should be
 * a power of two for efficiency reasons.  Some size suggestions: use 32
 * buckets per 100 objects;  a reasonable maximum number of object per
 * buckets is 8.  If there is an error creating the XAssocTable, a NULL
 * pointer is returned.
 */
XAssocTable *XCreateAssocTable(register int size)
	                  		/* Desired size of the table. */
{
	register XAssocTable *table;	/* XAssocTable to be initialized. */
	register XAssoc *buckets;	/* Pointer to the first bucket in */
					/* the bucket array. */

	/* Malloc the XAssocTable. */
	if ((table = (XAssocTable *)malloc(sizeof(XAssocTable))) == NULL) {
		/* malloc call failed! */
		errno = ENOMEM;
		return(NULL);
	}

	/* calloc the buckets (actually just their headers). */
	buckets = (XAssoc *)calloc((unsigned)size, (unsigned)sizeof(XAssoc));
	if (buckets == NULL) {
		/* calloc call failed! */
		errno = ENOMEM;
		return(NULL);
	}

	/* Insert table data into the XAssocTable structure. */
	table->buckets = buckets;
	table->size = size;

	while (--size >= 0) {
		/* Initialize each bucket. */
		buckets->prev = buckets;
		buckets->next = buckets;
		buckets++;
	}

	return(table);
}

