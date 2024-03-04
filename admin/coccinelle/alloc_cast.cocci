// Remove redundant casts from memory allocation functions.
@@
type T;
@@
-(T *)
 \(xmalloc\|xzalloc\|xrealloc\|xpalloc\|xnrealloc\)(...)
