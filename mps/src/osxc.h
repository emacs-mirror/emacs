/* impl.h.osxc: MacOS X (Carbon-compatible) system header hacks
 *
 * $HopeName: MMsrc!osxc.h(MM_epcore_brisling.1) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * .purpose: This header fixes bugs in the system headers.
 */


#ifndef osxc_h
#define osxc_h


#ifdef MPS_BUILD_GC
/* __inline__ is supposed to do nothing in gcc -ansi, but there's a bug in */
/* DP3, that causes it to signal error (for __sputc in stdio.h). */
#define __inline__
#endif


/* cabs doesn't have a proper prototype; taken from glibc 2.0.6 manual. */
/* Define a structure tag to avoid warnings. */
struct mps_complex { double real, imag; };
extern double cabs(struct mps_complex z);


#endif /* osxc_h */
