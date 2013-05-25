/* $Id$
Before modern interface;
 - no closure arguments for mps_root_create_reg
 - no rank argument for mps_ap_create (ever)
*/

#define mps_root_create_reg(ro, sp, ra, rm, thr, scan, p, _s) \
 (mps_root_create_reg)(ro, sp, ra, rm, thr, scan, p)

#define mps_ap_create(a, p, _r) \
 (mps_ap_create)(a, p)


#include "preGR.h"
