/* impl.h.mpslib: HARLEQUIN MEMORY POOL SYSTEM LIBRARY INTERFACE
 *
 * $HopeName: MMsrc!mpslib.h(trunk.8) $
 * Copyright (C) 1998 Harlequin Limited.  All rights reserved.
 *
 * .readership: MPS client application developers, MPS developers.
 * .sources: design.mps.lib
 *
 * .purpose: The purpose of this file is to declare the functions and types
 * required for the MPS library interface.
 */

#ifndef mpslib_h
#define mpslib_h

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


extern void *(mps_lib_memset)(void *, int, size_t);
extern void *(mps_lib_memcpy)(void *, const void *, size_t);
extern int (mps_lib_memcmp)(const void *, const void *, size_t);


typedef unsigned long mps_clock_t;
extern mps_clock_t mps_clock(void);


extern unsigned long mps_lib_telemetry_control(void);


#endif /* mpslib_h */
