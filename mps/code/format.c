/* format.c: OBJECT FORMATS
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 *
 * DESIGN
 *
 * See protocol.mps.format.
 */

#include "mpm.h"

SRCID(format, "$Id$");


/* FormatCheck -- check a format */

Bool FormatCheck(Format format)
{
  CHECKS(Format, format);
  CHECKU(Arena, format->arena);
  CHECKL(format->serial < format->arena->formatSerial);
  CHECKD_NOSIG(Ring, &format->arenaRing);
  CHECKL(AlignCheck(format->alignment));
  /* TODO: Define the concept of the maximum alignment it is possible to
     request from the MPS, document and provide an interface to it, and then
     check that this alignment is not greater than that, as well as all other
     alignments. */
  CHECKL(FUNCHECK(format->scan));
  CHECKL(FUNCHECK(format->skip));
  CHECKL(FUNCHECK(format->move));
  CHECKL(FUNCHECK(format->isMoved));
  CHECKL(FUNCHECK(format->pad));
  CHECKL(FUNCHECK(format->class));

  return TRUE;
}


/* FormatNo methods -- default values for format keyword arguments */

static mps_res_t FormatNoScan(mps_ss_t mps_ss, mps_addr_t base,
                              mps_addr_t limit)
{
    UNUSED(mps_ss);
    UNUSED(base);
    UNUSED(limit);
    NOTREACHED;
    return ResFAIL;
}

static mps_addr_t FormatNoSkip(mps_addr_t object)
{
    UNUSED(object);
    NOTREACHED;
    return NULL;
}

static void FormatNoMove(mps_addr_t old, mps_addr_t new)
{
    UNUSED(old);
    UNUSED(new);
    NOTREACHED;
}

static mps_addr_t FormatNoIsMoved(mps_addr_t object)
{
    UNUSED(object);
    NOTREACHED;
    return NULL;
}

static void FormatNoPad(mps_addr_t addr, size_t size)
{
    UNUSED(addr);
    UNUSED(size);
    NOTREACHED;
}

static mps_addr_t FormatDefaultClass(mps_addr_t object)
{
  AVER(object != NULL);

  return ((mps_addr_t *)object)[0];
}


/* FormatCreate -- create a format */

ARG_DEFINE_KEY(fmt_align, Align);
ARG_DEFINE_KEY(fmt_scan, Fun);
ARG_DEFINE_KEY(fmt_skip, Fun);
ARG_DEFINE_KEY(fmt_fwd, Fun);
ARG_DEFINE_KEY(fmt_isfwd, Fun);
ARG_DEFINE_KEY(fmt_pad, Fun);
ARG_DEFINE_KEY(fmt_header_size, Size);
ARG_DEFINE_KEY(fmt_class, Fun);

Res FormatCreate(Format *formatReturn, Arena arena, ArgList args)
{
  ArgStruct arg;
  Format format;
  Res res;
  void *p;
  Align fmtAlign = FMT_ALIGN_DEFAULT;
  Size fmtHeaderSize = FMT_HEADER_SIZE_DEFAULT;
  mps_fmt_scan_t fmtScan = FMT_SCAN_DEFAULT;
  mps_fmt_skip_t fmtSkip = FMT_SKIP_DEFAULT;
  mps_fmt_fwd_t fmtFwd = FMT_FWD_DEFAULT;
  mps_fmt_isfwd_t fmtIsfwd = FMT_ISFWD_DEFAULT;
  mps_fmt_pad_t fmtPad = FMT_PAD_DEFAULT;
  mps_fmt_class_t fmtClass = FMT_CLASS_DEFAULT;

  AVER(formatReturn != NULL);
  AVERT(Arena, arena);
  AVERT(ArgList, args);

  if (ArgPick(&arg, args, MPS_KEY_FMT_ALIGN))
    fmtAlign = arg.val.align;
  if (ArgPick(&arg, args, MPS_KEY_FMT_HEADER_SIZE))
    fmtHeaderSize = arg.val.size;
  if (ArgPick(&arg, args, MPS_KEY_FMT_SCAN))
    fmtScan = arg.val.fmt_scan;
  if (ArgPick(&arg, args, MPS_KEY_FMT_SKIP))
    fmtSkip = arg.val.fmt_skip;
  if (ArgPick(&arg, args, MPS_KEY_FMT_FWD))
    fmtFwd = arg.val.fmt_fwd;
  if (ArgPick(&arg, args, MPS_KEY_FMT_ISFWD))
    fmtIsfwd = arg.val.fmt_isfwd;
  if (ArgPick(&arg, args, MPS_KEY_FMT_PAD))
    fmtPad = arg.val.fmt_pad;
  if (ArgPick(&arg, args, MPS_KEY_FMT_CLASS))
    fmtClass = arg.val.fmt_class;

  res = ControlAlloc(&p, arena, sizeof(FormatStruct),
                     /* withReservoirPermit */ FALSE);
  if(res != ResOK)
    return res;
  format = (Format)p; /* avoid pun */

  format->arena = arena;
  RingInit(&format->arenaRing);
  format->alignment = fmtAlign;
  format->headerSize = fmtHeaderSize;
  format->scan = fmtScan;
  format->skip = fmtSkip;
  format->move = fmtFwd;
  format->isMoved = fmtIsfwd;
  format->pad = fmtPad;
  format->class = fmtClass;

  format->sig = FormatSig;
  format->serial = arena->formatSerial;
  ++arena->formatSerial;

  AVERT(Format, format);
 
  RingAppend(&arena->formatRing, &format->arenaRing);

  *formatReturn = format;
  return ResOK;
}


/* FormatDestroy -- destroy a format */

void FormatDestroy(Format format)
{
  AVERT(Format, format);

  RingRemove(&format->arenaRing);

  format->sig = SigInvalid;
 
  RingFinish(&format->arenaRing);

  ControlFree(format->arena, format, sizeof(FormatStruct));
}


/* FormatArena -- find the arena of a format
 *
 * Must be thread-safe.  See <design/interface-c/#thread-safety>. */

Arena FormatArena(Format format)
{
  /* Can't AVER format as that would not be thread-safe */
  /* AVERT(Format, format); */
  return format->arena;
}


/* FormatDescribe -- describe a format */

Res FormatDescribe(Format format, mps_lib_FILE *stream, Count depth)
{
  Res res;
 
  res = WriteF(stream, depth,
               "Format $P ($U) {\n", (WriteFP)format, (WriteFU)format->serial,
               "  arena $P ($U)\n",
               (WriteFP)format->arena, (WriteFU)format->arena->serial,
               "  alignment $W\n", (WriteFW)format->alignment,
               "  scan $F\n", (WriteFF)format->scan,
               "  skip $F\n", (WriteFF)format->skip,
               "  move $F\n", (WriteFF)format->move,
               "  isMoved $F\n", (WriteFF)format->isMoved,
               "  pad $F\n", (WriteFF)format->pad,
               "  headerSize $W\n", (WriteFW)format->headerSize,
               "} Format $P ($U)\n", (WriteFP)format, (WriteFU)format->serial,
               NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
