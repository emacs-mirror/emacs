/*  impl.c.format
 *
 *           OBJECT FORMATS
 *
 *  $HopeName: MMsrc/!format.c(trunk.1)$
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


Error FormatInit(Format format, Addr alignment,
		 FormatScanMethod scan,
		 FormatSkipMethod skip,
		 FormatLengthMethod length,
		 FormatProbeMethod probe,
		 FormatMoveMethod move,
		 FormatIsMovedMethod isMoved,
		 FormatCopyMethod copy)
{
  AVER(format != NULL);

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
  
  return(ErrSUCCESS);
}


void FormatFinish(Format format)
{
#if !defined(DEBUG_ASSERT) && !defined(DEBUG_SIGN)
  UNUSED(format);
#endif
  AVER(ISVALID(Format, format));
#ifdef DEBUG_SIGN
  format->sig = SigInvalid;
#endif
}
