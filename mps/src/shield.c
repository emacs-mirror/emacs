/* impl.c.shield: SHIELD IMPLEMENTATION
 *
 * $HopeName: MMsrc!shield.c(trunk.3) $
 *
 * See: idea.shield, design.mps.shield.
 *
 * Invariant: The protected memory is a subset of the shielded memory when
 *            inside the shield, and the same set when outside.
 */

#include "std.h"
#include "space.h"
#include "shield.h"
#include "prot.h"
#include "poolar.h"
#include "th.h"

SRCID("$HopeName$");

static void protect(Arena arena, Addr seg, ProtMode mode)
{
  if(ArenaProtMode(arena, seg) != mode) {
    ProtSet(seg, seg + ArenaSegSize(arena, seg), mode);
    ArenaSetProtMode(arena, seg, mode);
  }
}

void ShieldRaise(Space space, Addr seg, ProtMode mode)
{
  ProtMode shieldMode;
  Arena arena;

  AVER(ISVALID(Space, space));

  arena = SpaceArena(space);

  shieldMode = ArenaShieldMode(arena, seg);
  AVER((shieldMode & mode) == ProtNONE);
  shieldMode |= mode;
  ArenaSetShieldMode(arena, seg, shieldMode);

  if(shieldMode >> 2 == 0)
    protect(arena, seg, shieldMode);
}

void ShieldLower(Space space, Addr seg, ProtMode mode)
{
  ProtMode shieldMode;
  Arena arena;

  AVER(ISVALID(Space, space));

  arena = SpaceArena(space);

  shieldMode = ArenaShieldMode(arena, seg);
  AVER((shieldMode & mode) == mode);
  shieldMode &= ~mode;
  ArenaSetShieldMode(arena, seg, shieldMode);

  if(shieldMode >> 2 == 0)
    protect(arena, seg, shieldMode); /* will only remove protection */
}


void ShieldEnter(Space space)
{
  AVER(ISVALID(Space, space));
  AVER(!space->insideShield);

  ThreadDequeSuspend(SpaceThreadDeque(space));
  space->insideShield = TRUE;
}

void ShieldLeave(Space space)
{
  AVER(ISVALID(Space, space));
  AVER(space->insideShield);

/* .opt.lazy-cover:
  for all segs {
    protect(arena, seg, ArenaShieldMode(arena, seg));
  }
 */

  ThreadDequeResume(SpaceThreadDeque(space));
  space->insideShield = FALSE;
}


void ShieldExpose(Space space, Addr seg)
{
  ProtMode shieldMode;
  Arena arena;

  AVER(ISVALID(Space, space));
  AVER(space->insideShield);

  arena = SpaceArena(space);
  shieldMode = ArenaShieldMode(arena, seg);
  shieldMode += 4;
  ArenaSetShieldMode(arena, seg, shieldMode);

  protect(arena, seg, ProtNONE);
}

void ShieldCover(Space space, Addr seg)
{
  ProtMode shieldMode;
  Arena arena;

  AVER(ISVALID(Space, space));
  AVER(ArenaProtMode(SpaceArena(space), seg) == ProtNONE);

  arena = SpaceArena(space);
  shieldMode = ArenaShieldMode(arena, seg);
  AVER(shieldMode >= 4);
  shieldMode -= 4;
  ArenaSetShieldMode(arena, seg, shieldMode);

  if(shieldMode >> 2 == 0)
    protect(arena, seg, shieldMode);
}
