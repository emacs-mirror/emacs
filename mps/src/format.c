/*  ==== OBJECT FORMATS ====
 *
 *  $HopeName$
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
  AVER(format->scan1 != NULL);
  AVER(format->skip != NULL);
  AVER(format->length != NULL);
  AVER(format->isNotObject != NULL);
  AVER(format->isMoved != NULL);
  AVER(format->move != NULL);
  return(TRUE);
}

#endif /* DEBUG_ASSERT */


Error FormatInit(Format format, Addr alignment,
		 FormatScan scan,
		 FormatScan1 scan1,
		 FormatSkip skip,
		 FormatLength length,
		 FormatIsNotObject isNotObject,
		 FormatIsMoved isMoved,
		 FormatMove move)
{
  AVER(format != NULL);

  format->alignment = alignment;
  format->scan = scan;
  format->scan1 = scan1;
  format->skip = skip;
  format->length = length;
  format->isNotObject = isNotObject;
  format->isMoved = isMoved;
  format->move = move;

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
