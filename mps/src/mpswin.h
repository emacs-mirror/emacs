/* impl.h.mpswin: HARLEQUIN MEMORY POOL SYSTEM WINDOWS.H INTERFACE
 *
 * $HopeName$
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * .readership: customers, MPS developers.
 *
 * .purpose: Shared file for the incantations needed to include windows.h.
 */

#ifndef mpswin_h
#define mpswin_h

/* Suppress Visual C warnings from windows.h at warning level 4, */
/* see mail.richard.1997-09-25.13-26. */
#ifdef MPS_BUILD_MV
#pragma warning(disable: 4514)
#pragma warning(disable: 4115 4201 4214)
#endif
#include <windows.h>
#ifdef MPS_BUILD_MV
#pragma warning(default: 4115 4201 4214)
#endif

#endif /* mpswin_h */
