/* impl.c.format: OBJECT FORMATS
 *
 *  $HopeName: MMsrc!format.c(MMdevel_lib.2) $
 */

#include "mpm.h"

SRCID(format, "$HopeName: MMsrc!format.c(MMdevel_lib.2) $");


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

  AVER(formatReturn != NULL);

  res = SpaceAlloc((Addr *)&format, space, sizeof(FormatStruct));
  if(res != ResOK)
    return res;

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

/* Must be thread safe.  See impl.c.mpsi.thread-safety. */
Space FormatSpace(Format format)
{
  return format->space;
}


Res FormatDescribe(Format format, mps_lib_FILE *stream)
{
  Res res;
  
  res = WriteF(stream,
               "Format $P ($U) {\n", (void *)format, (unsigned long)format->serial,
               "  space $P ($U)\n", (void *)format->space, (unsigned long)format->space->serial,
               "  alignment $W\n", (Word)format->alignment,
               "  scan $P\n", (void *)format->scan,
               "  skip $P\n", (void *)format->skip,
               "  move $P\n", (void *)format->move,
               "  isMoved $P\n", (void *)format->isMoved,
               "  copy $P\n", (void *)format->copy,
               "  pad $P\n", (void *)format->pad,
               "} Format $P ($U)\n", (void *)format, (unsigned long)format->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}
