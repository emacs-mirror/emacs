/* impl.c.format: OBJECT FORMATS
 *
 *  $HopeName: MMsrc!format.c(trunk.13) $
 */

#include "mpm.h"

SRCID(format, "$HopeName: MMsrc!format.c(trunk.13) $");


Bool FormatCheck(Format format)
{
  CHECKS(Format, format);
  CHECKU(Space, format->space);
  CHECKL(format->serial < format->space->formatSerial);
  CHECKL(RingCheck(&format->spaceRing));
  CHECKL(AlignCheck(format->alignment));
  /* @@@@ alignment should be less than maximum allowed */
  CHECKL(format->scan != NULL);
  CHECKL(format->skip != NULL);
  CHECKL(format->move != NULL);
  CHECKL(format->isMoved != NULL);
  CHECKL(format->copy != NULL);
  CHECKL(format->pad != NULL);
  return TRUE;
}


Res FormatCreate(Format *formatReturn, Space space,
                 Align alignment,
                 FormatScanMethod scan,
                 FormatSkipMethod skip,
                 FormatMoveMethod move,
                 FormatIsMovedMethod isMoved,
                 FormatCopyMethod copy,
                 FormatPadMethod pad)
{
  Format format;
  Res res;
  void *p;

  AVER(formatReturn != NULL);

  res = SpaceAlloc(&p, space, sizeof(FormatStruct));
  if(res != ResOK)
    return res;
  format = (Format)p; /* avoid pun */

  format->space = space;
  RingInit(&format->spaceRing);
  format->alignment = alignment;
  format->scan = scan;
  format->skip = skip;
  format->move = move;
  format->isMoved = isMoved;
  format->copy = copy;
  format->pad = pad;

  format->sig = FormatSig;
  format->serial = space->formatSerial;
  ++space->formatSerial;

  AVERT(Format, format);
  
  RingAppend(&space->formatRing, &format->spaceRing);

  *formatReturn = format;
  return ResOK;
}


void FormatDestroy(Format format)
{
  AVERT(Format, format);

  RingRemove(&format->spaceRing);

  format->sig = SigInvalid;
  
  RingFinish(&format->spaceRing);

  SpaceFree(format->space, (Addr)format, sizeof(FormatStruct));
}

/* Must be thread safe.  See design.mps.interface.c.thread-safety. */
Space FormatSpace(Format format)
{
  return format->space;
}


Res FormatDescribe(Format format, mps_lib_FILE *stream)
{
  Res res;
  
  res = WriteF(stream,
               "Format $P ($U) {\n", (WriteFP)format, (WriteFU)format->serial,
               "  space $P ($U)\n", 
               (WriteFP)format->space, (WriteFU)format->space->serial,
               "  alignment $W\n", (WriteFW)format->alignment,
               "  scan $F\n", (WriteFF)format->scan,
               "  skip $F\n", (WriteFF)format->skip,
               "  move $F\n", (WriteFF)format->move,
               "  isMoved $F\n", (WriteFF)format->isMoved,
               "  copy $F\n", (WriteFF)format->copy,
               "  pad $F\n", (WriteFF)format->pad,
               "} Format $P ($U)\n", (WriteFP)format, (WriteFU)format->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}
