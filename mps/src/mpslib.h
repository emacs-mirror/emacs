/* impl.h.mpslib: HARLEQUIN MEMORY POOL SYSTEM LIBRARY INTERFACE
 *
 * $HopeName: MMsrc!mpslib.h(trunk.2) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved.
 *
 * .readership: MPS client application developers, MPS developers.
 * .sources: design.mps.lib, design.mps.exec-env in Memory Management 
 *   Information System
 * .doc: Full documentation not yet available.
 * .purpose: The purpose of this file is to declare the functions and types
 *   required for the MPS downward library interface, and also to include those
 *   headers permitted to free-standing implementations (see .free-standing),
 *   except float.h.
 */

#ifndef mpslib_h
#define mpslib_h

/* .free-standing: These three headers may be used in free-standing 
 * implementations.  See standard.ansic section 4 and design.mps.exec-env.
 */
#include <stdarg.h>
#include <limits.h>
#include <stddef.h>

extern int mps_lib_get_EOF(void);
#define mps_lib_EOF     (mps_lib_get_EOF())

typedef struct mps_lib_stream_s mps_lib_FILE;

extern mps_lib_FILE *mps_lib_get_stderr(void);
extern mps_lib_FILE *mps_lib_get_stdout(void);
#define mps_lib_stderr  (mps_lib_get_stderr())
#define mps_lib_stdout  (mps_lib_get_stdout())

extern int mps_lib_fputc(int, mps_lib_FILE *);
extern int mps_lib_fputs(const char *, mps_lib_FILE *);

extern void mps_lib_abort(void);

void *mps_lib_memset(void *, int, size_t);

#endif /* mpslib_h */
