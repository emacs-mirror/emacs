#ifndef EMACS_GC_HANDLES_H
#define EMACS_GC_HANDLES_H

#include "config.h"
#include "lisp.h"

struct gc_handle_struct;
typedef struct gc_handle_struct *gc_handle;

gc_handle gc_handle_for (Lisp_Object);
gc_handle gc_handle_for_pvec (struct vectorlike_header *);
void free_gc_handle (gc_handle);
Lisp_Object gc_handle_value (gc_handle);

void syms_of_gc_handles (void);

#endif
