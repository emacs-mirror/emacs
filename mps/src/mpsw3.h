/* impl.h.mpsw3: HARLEQUIN MEMORY POOL SYSTEM C INTERFACE, WINDOWS PART
 *
 * $HopeName: MMsrc!mpsw3.h(trunk.2) $
 * Copyright (C) 1998 Harlequin Limited.  All rights reserved.
 *
 * .readership: customers, MPS developers.
 * .sources: design.mps.interface.c.
 */

#ifndef mpsw3_h
#define mpsw3_h

#include "mps.h"               /* needed for mps_tramp_t */
#include "mpswin.h"            /* needed for SEH filter */


extern LONG mps_SEH_filter(LPEXCEPTION_POINTERS, void **, size_t *);
extern void mps_SEH_handler(void *, size_t);


#undef mps_tramp /* Override generic version */

#define mps_tramp(r_o, f, p, s) \
  MPS_BEGIN \
    void **_r_o = (r_o); \
    mps_tramp_t _f = (f); \
    void *_p = (p); \
    size_t _s = (s); \
    void *_hp = NULL; size_t _hs = 0; \
    __try { \
      *_r_o = (*_f)(_p, _s); \
    } __except(mps_SEH_filter(GetExceptionInformation(), \
               &_hp, &_hs)) { \
      mps_SEH_handler(_hp, _hs); \
    } \
  MPS_END


#endif /* mpsw3_h */
