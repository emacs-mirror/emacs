/* impl.h.mpswin: HARLEQUIN MEMORY POOL SYSTEM WINDOWS.H INTERFACE
 *
 * $HopeName: MMsrc!mpswin.h(trunk.3) $
 * Copyright (C) 1997 Harlequin Limited.  All rights reserved.
 *
 * .readership: For MPS client application developers, MPS developers.
 *
 * .purpose: Shared file for the incantations needed to include windows.h.
 */

#ifndef mpswin_h
#define mpswin_h

/* Suppress Visual C warnings from windows.h at warning level 4. */
#ifdef MPS_BUILD_MV
#pragma warning(disable: 4115 4201 4209 4214)
#endif

#include <windows.h>

#ifdef MPS_BUILD_MV
#pragma warning(default: 4115 4201 4209 4214)
/* windows.h might also cause warnings about "unreferenced inline
 * function has been removed".  In Visual C, these can be turned off:
 * #pragma warning(disable: 4514)
 * But they are generated at the end of the compilation, so you have
 * to turn them off permanently.
 */
#endif

#endif /* mpswin_h */
