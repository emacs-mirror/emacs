/*  impl.c.format
 *
 *           OBJECT FORMATS
 *
 *  $HopeName: MMsrc/!format.c(trunk.2)$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 */

#include "std.h"
#include "format.h"


#ifdef DEBUG_SIGN
static struct SigStruct FormatSigStruct;
#endif


#ifdef DEBUG_ASSERT

Bool FormatIsValid(Format format, ValidationType validParam)
{
  AVER(format != NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, &FormatSigStruct));
  AVER(format->sig == &FormatSigStruct);
#endif
  AVER(ISVALIDNESTED(Space, format->space));
  AVER(IsPoT(format->alignment));
  /* **** alignment should be less than maximum allowed */
  AVER(format->scan != NULL);
  AVER(format->skip != NULL);
  AVER(format->length != NULL);
  AVER(format->probe != NULL);
  AVER(format->move != NULL);
  AVER(format->isMoved != NULL);
  AVER(format->copy != NULL);
  return(TRUE);
}

#endif /* DEBUG_ASSERT */


Error FormatCreate(Format *formatReturn, Space space,
                   Addr alignment,
                   FormatScanMethod scan,
                   FormatSkipMethod skip,
                   FormatLengthMethod length,
                   FormatProbeMethod probe,
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
  format->length = length;
  format->probe = probe;
  format->move = move;
  format->isMoved = isMoved;
  format->copy = copy;

#ifdef DEBUG_SIGN
  SigInit(&FormatSigStruct, "Format");
  format->sig = &FormatSigStruct;
#endif

  AVER(ISVALID(Format, format));
  
  *formatReturn = format;
  return ErrSUCCESS;
}


void FormatDestroy(Format format)
{
  AVER(ISVALID(Format, format));
#ifdef DEBUG_SIGN
  format->sig = SigInvalid;
#endif

  PoolFree(SpaceControlPool(format->space),
           (Addr)format, sizeof(FormatStruct));
}
