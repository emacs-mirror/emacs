/* $Id$
arg.h
   useful things for arg-err tests
*/

#ifndef arg_h
#define arg_h

#include "testlib.h"

#undef UNALIGNED
#define UNALIGNED ((mps_addr_t) (((char *) NULL) + 1))

#define MPS_RANK_MIN 0
#define MPS_RANK_MAX 3

#define MPS_RM_MIN 0
#define MPS_RM_MAX 3

#define MPS_ALIGN_MIN 1

/* possibly nasty values with high bits set */

#define HIGHBIT_CHAR (~(((unsigned char)  -1) >> 1))
#define HIGHBIT_INT (~(((unsigned int) -1) >> 1))
#define HIGHBIT_SHORT (~(((unsigned short) -1) >> 1))
#define HIGHBIT_LONG (~(((unsigned long) -1) >> 1))
#define HIGHBIT_SIZE (~((~ (size_t) 0) >> 1))

/* n.b. the last line above will work in ansi C because
   size_t is an unsigned type. In sos libs, size_t, however
   is a signed type; it still works tho', because in sos,
   >> on a negative value is a logical (not arithmetical) shift.
*/

#endif

