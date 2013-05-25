/* $Id$
In grotesq interface
 - "space" becomes "arena"
*/

#include "postMO.h"

#define mps_space_clamp(a) \
 mps_arena_clamp(a)

#define mps_space_release(a) \
 mps_arena_release(a)

#define mps_space_park(a) \
 mps_arena_park(a)

#define mps_space_collect(a) \
 mps_arena_collect(a)

#define mps_space_destroy(a) \
 mps_arena_destroy(a)

#define mps_space_reserved(a) \
 mps_arena_reserved(a)

#define mps_space_committed(a) \
 mps_arena_committed(a)


