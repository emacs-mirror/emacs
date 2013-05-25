/* $Id$
Before grotesque interface;
 - "arena" was called "space" in several functions
*/


#define mps_arena_clamp(a) \
 mps_space_clamp(a)

#define mps_arena_release(a) \
 mps_space_release(a)

#define mps_arena_park(a) \
 mps_space_park(a)

#define mps_arena_collect(a) \
 mps_space_collect(a)

#define mps_arena_destroy(a) \
 mps_space_destroy(a)

#define mps_arena_reserved(a) \
 mps_space_reserved(a)

#define mps_arena_committed(a) \
 mps_space_committed(a)


#include "preBQ.h"
