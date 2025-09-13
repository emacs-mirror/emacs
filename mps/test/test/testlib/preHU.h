/* $Id$
Before humanist interface;
 -- there was no spare committed fund
*/

#define mps_arena_spare_committed(a) ((size_t) 0)
#define mps_arena_spare_commit_limit(a) ((size_t) 0)
#define mps_arena_spare_commit_limit_set(a, l) \
 asserts(0, \
 "MPS interface versions before HU do not support the spare committed fund")


/* #include "preXX.h" -- the next interface version, when there is one */

