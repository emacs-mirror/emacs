/* arg.h: Keyword argument lists
 *
 * $Id$
 * Copyright (c) 2013-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .source: <design/keyword-arguments.rst>.
 */

#ifndef arg_h
#define arg_h

#include "mpmtypes.h"


/* KeyStruct -- keyword argument structure
 *
 * NOTE: Whether or not to have an argument checking field and functions
 * could be conditional on build variety.  Checking arguments isn't on
 * the critical path, but this might save space if the MPS is being
 * deployed in a tight memory situation.
 */

#define KeySig          ((Sig)0x519CE111) /* SIGnature KEYyy */
typedef struct mps_key_s {
  Sig sig;
  const char *name;
  Bool (*check)(Arg arg);
} KeyStruct;

#define ARG_DEFINE_KEY(id, type) \
  extern const KeyStruct _mps_key_##id; \
  const KeyStruct _mps_key_##id = {KeySig, "MPS_KEY_" #id, ArgCheck##type}

#define argsNone mps_args_none

extern Bool KeyCheck(Key key);
extern Bool ArgCheck(Arg arg);
extern Bool ArgListCheck(ArgList args);

extern Bool ArgPick(ArgStruct *argOut, ArgList args, Key key);
extern void ArgRequire(ArgStruct *argOut, ArgList args, Key key);
extern void ArgTrivVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs);

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
extern Bool ArgCheckPointer(Arg arg);
extern Bool ArgCheckRankSet(Arg arg);
extern Bool ArgCheckRank(Arg arg);
extern Bool ArgCheckdouble(Arg arg);
extern Bool ArgCheckPool(Arg arg);


#endif /* arg_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2013-2020 Ravenbrook Limited <http://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
