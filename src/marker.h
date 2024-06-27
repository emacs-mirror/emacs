#ifndef EMACS_MARKER_H
#define EMACS_MARKER_H

#include "lisp.h"

#ifndef ABSTRACT_LISP_MARKER
struct Lisp_Marker
{
  union vectorlike_header header;

  /* This is the buffer that the marker points into, or 0 if it points nowhere.
     Note: a chain of markers can contain markers pointing into different
     buffers (the chain is per buffer_text rather than per buffer, so it's
     shared between indirect buffers).  */
  /* This is used for (other than NULL-checking):
     - Fmarker_buffer
     - Fset_marker: check eq(oldbuf, newbuf) to avoid unchain+rechain.
     - unchain_marker: to find the list from which to unchain.
     - Fkill_buffer: to only unchain the markers of current indirect buffer.
     */
  struct buffer *buffer;

  /* This flag is temporarily used in the functions
     decode/encode_coding_object to record that the marker position
     must be adjusted after the conversion.  */
  bool_bf need_adjustment : 1;
  /* True means normal insertion at the marker's position
     leaves the marker after the inserted text.  */
  bool_bf insertion_type : 1;
  /* True means that this is a chars<->bytes conversion cache entry
     rather than a true marker.  */
  bool_bf cache : 1;

  /* The remaining fields are meaningless in a marker that
     does not point anywhere.  */

  /* This is the char position where the marker points.  */
  ptrdiff_t charpos;
  /* This is the byte position.
     It's mostly used as a charpos<->bytepos cache (i.e. it's not directly
     used to implement the functionality of markers, but rather to (ab)use
     markers as a cache for char<->byte mappings).  */
  ptrdiff_t bytepos;
} GCALIGNED_STRUCT;
#endif

struct Lisp_Markers;

extern bool markers_full_p (struct Lisp_Markers *t);
extern struct Lisp_Markers *markers_new (unsigned int size);
extern void markers_kill (struct Lisp_Markers *t, struct Lisp_Marker *m);
extern struct Lisp_Markers *markers_add (struct Lisp_Markers *t,
					 struct Lisp_Marker *m);
extern void markers_adjust_for_insert (struct Lisp_Markers *t,
				       ptrdiff_t from, ptrdiff_t from_byte,
				       ptrdiff_t to, ptrdiff_t to_byte,
				       bool before_markers);
extern void markers_adjust_for_replace
	   (struct Lisp_Markers *t,
	    ptrdiff_t from, ptrdiff_t from_byte,
	    ptrdiff_t old_chars, ptrdiff_t old_bytes,
	    ptrdiff_t new_chars, ptrdiff_t new_bytes);
extern void markers_adjust_for_delete (struct Lisp_Markers *t,
				       ptrdiff_t from, ptrdiff_t from_byte,
				       ptrdiff_t to, ptrdiff_t to_byte);



struct markers_iterator
{
  struct Lisp_Markers *t;
  struct Lisp_Marker *m;
  unsigned int i;
};

extern struct markers_iterator markers_iterator_all (struct Lisp_Markers *t);
extern void markers_iterator_next (struct markers_iterator *it);

#define MARKERS_DO_ALL(it, t) 				      \
  for (struct markers_iterator it = markers_iterator_all (t); \
	it.m; markers_iterator_next (&it))

/**********************************************************************/

extern void clear_charpos_cache (struct buffer *b);
extern ptrdiff_t buf_charpos_to_bytepos (struct buffer *b, ptrdiff_t charpos);
extern ptrdiff_t buf_bytepos_to_charpos (struct buffer *b, ptrdiff_t bytepos);
extern Lisp_Object set_marker_restricted (Lisp_Object marker,
					  Lisp_Object position,
					  Lisp_Object buffer);
extern Lisp_Object set_marker_both (Lisp_Object marker, Lisp_Object buffer,
				    ptrdiff_t charpos, ptrdiff_t bytepos);
extern Lisp_Object set_marker_restricted_both (Lisp_Object marker,
					       Lisp_Object buffer,
					       ptrdiff_t charpos,
					       ptrdiff_t bytepos);
extern void detach_marker (Lisp_Object marker);
extern void unchain_marker (register struct Lisp_Marker *marker);
extern ptrdiff_t marker_position (Lisp_Object marker);
extern ptrdiff_t marker_byte_position (Lisp_Object marker);
extern Lisp_Object build_marker (struct buffer *buf, ptrdiff_t charpos,
				 ptrdiff_t bytepos);
extern void syms_of_marker (void);

#endif
