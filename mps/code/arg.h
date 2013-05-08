/* arg.h: Keyword argument lists
 *
 * $Id$
 * Copyright (c) 2013 Ravenbrook Limited.  See end of file for license.
 *
 * .source: FIXME: Write up design from email discussion.
 */

#ifndef arg_h
#define arg_h

#include "mpmtypes.h"

/* FIXME: Whether to have a check field should be conditional on variety */

#define ARG_DEFINE_KEY(id, type) \
  const KeyStruct _mps_key_##id = {KeySig, #id, ArgCheck##type}

#define argsNone mps_args_none
extern Bool KeyCheck(Key key);
extern Bool ArgCheck(Arg arg);
extern Bool ArgListCheck(ArgList args);
extern Bool ArgPick(ArgStruct *argOut, ArgList args, Key key);
extern Bool ArgCheckCant(Arg arg);
extern Bool ArgCheckFormat(Arg arg);
extern Bool ArgCheckChain(Arg arg);
extern Bool ArgCheckSize(Arg arg);
extern Bool ArgCheckAddr(Arg arg);
extern Bool ArgCheckPoolDebugOptions(Arg arg);
extern Bool ArgCheckFun(Arg arg);
extern Bool ArgCheckAlign(Arg arg);
extern Bool ArgCheckBool(Arg arg);
extern Bool ArgCheckCount(Arg arg);

#endif /* arg_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
