/* impl.c.format: OBJECT FORMATS
 *
 *  $HopeName: MMsrc!format.c(trunk.6) $
 */

#include "std.h"
#include "space.h"
#include "format.h"
#include "pool.h"

SRCID("$HopeName: MMsrc!format.c(trunk.6) $");


#ifdef DEBUG

Bool FormatIsValid(Format format, ValidationType validParam)
{
  AVER(format != NULL);
  AVER(format->sig == FormatSig);
  AVER(ISVALIDNESTED(Space, format->space));
  AVER(IsPoT(format->alignment));
  /* **** alignment should be less than maximum allowed */
  AVER(format->scan != NULL);
  AVER(format->skip != NULL);
  AVER(format->move != NULL);
  AVER(format->isMoved != NULL);
  AVER(format->copy != NULL);
  return TRUE;
}

#endif /* DEBUG */


Error FormatCreate(Format *formatReturn, Space space,
                   Addr alignment,
                   FormatScanMethod scan,
                   FormatSkipMethod skip,
                   FormatMoveMethod move,
                   FormatIsMovedMethod isMoved,
                   FormatCopyMethod copy)
{
  Format format;
  Error e;

  AVER(formatReturn != NULL);

  e = PoolAlloc((Addr *)&format, SpaceControlPool(space),
                 sizeof(FormatStruct));
  if(e != ErrSUCCESS)
    return e;

  format->space = space;
  format->alignment = alignment;
  format->scan = scan;
  format->skip = skip;
  format->move = move;
  format->isMoved = isMoved;
  format->copy = copy;

  format->sig = FormatSig;

  AVER(ISVALID(Format, format));
  
  *formatReturn = format;
  return ErrSUCCESS;
}


void FormatDestroy(Format format)
{
  AVER(ISVALID(Format, format));
  format->sig = SigInvalid;

  PoolFree(SpaceControlPool(format->space),
           (Addr)format, sizeof(FormatStruct));
}

/* thread safe */
Space FormatSpace(Format format)
{
  return format->space;
}
