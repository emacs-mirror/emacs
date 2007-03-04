/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


#include <X11/Xlib.h>
#include "X10.h"

/*
 * XDestroyAssocTable - Destroy (free the memory associated with)
 * an XAssocTable.
 */
XDestroyAssocTable(table)
	register XAssocTable *table;
{
	register int i;
	register XAssoc *bucket;
	register XAssoc *Entry, *entry_next;

	/* Free the buckets. */
	for (i = 0; i < table->size; i++) {
		bucket = &table->buckets[i];
		for (
	        	Entry = bucket->next;
			Entry != bucket;
			Entry = entry_next
		) {
		        entry_next = Entry->next;
			free((char *)Entry);
		}
	}

	/* Free the bucket array. */
	free((char *)table->buckets);

	/* Free the table. */
	free((char *)table);
}

/* arch-tag: a536bf02-8d63-45f2-8c1a-c7f9fd4da2cf
   (do not change this comment) */
