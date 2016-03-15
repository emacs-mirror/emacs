/* arg.c: ARGUMENT LISTS
 *
 * $Id$
 * Copyright (c) 2013-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .source: See <design/keyword-arguments.rst>.
 */

#include "config.h"
#include "check.h"
#include "mpm.h"
#include "dbgpool.h"

SRCID(arg, "$Id$");


/* ArgCheckCant -- default argument checker
 *
 * This is a default value for the KeyStruct check field for keywords
 * that don't have any meaningful checking they can do.
 */

Bool ArgCheckCant(Arg arg) {
  UNUSED(arg);
  return TRUE;
}

static Bool ArgCheckShouldnt(Arg arg) {
  UNUSED(arg);
  NOTREACHED;
  return FALSE;
}

Bool ArgCheckFormat(Arg arg) {
  CHECKD(Format, arg->val.format);
  return TRUE;
}

Bool ArgCheckChain(Arg arg) {
  CHECKD(Chain, arg->val.chain);
  return TRUE;
}

Bool ArgCheckSize(Arg arg) {
  UNUSED(arg); /* TODO: Add and call SizeCheck */
  return TRUE;
}

Bool ArgCheckAddr(Arg arg) {
  UNUSED(arg); /* TODO: Add and call AddrCheck */
  return TRUE;
}

Bool ArgCheckPoolDebugOptions(Arg arg) {
  CHECKD_NOSIG(PoolDebugOptions, (PoolDebugOptions)arg->val.pool_debug_options);
  return TRUE;
}

Bool ArgCheckFun(Arg arg) {
  CHECKL(FUNCHECK(arg->val.addr_method)); /* FIXME: Potential pun here */
  return TRUE;
}

Bool ArgCheckAlign(Arg arg) {
  CHECKL(AlignCheck(arg->val.align));
  return TRUE;
}

Bool ArgCheckBool(Arg arg) {
  CHECKL(BoolCheck(arg->val.b));
  return TRUE;
}

Bool ArgCheckCount(Arg arg) {
  UNUSED(arg); /* TODO: Add and call CountCheck */
  return TRUE;
}

Bool ArgCheckPointer(Arg arg) {
  CHECKL(arg != NULL);
  return TRUE;
}

Bool ArgCheckRankSet(Arg arg) {
  CHECKL(COMPATTYPE(RankSet, unsigned));
  CHECKL(RankSetCheck(arg->val.u));
  return TRUE;
}

Bool ArgCheckRank(Arg arg) {
  CHECKL(RankCheck(arg->val.rank));
  return TRUE;
}

Bool ArgCheckdouble(Arg arg) {
  /* Don't call isfinite() here because it's not in C89, and because
     infinity is a valid value for MPS_KEY_PAUSE_TIME. */
  UNUSED(arg);
  return TRUE;
}

Bool ArgCheckPool(Arg arg) {
  CHECKD(Pool, arg->val.pool);
  return TRUE;
}


ARG_DEFINE_KEY(ARGS_END, Shouldnt);

ArgStruct mps_args_none[] = {{MPS_KEY_ARGS_END, {0}}};


/* KeyCheck -- check the validity of an argument key */

Bool KeyCheck(Key key)
{
  CHECKS(Key, key);
  CHECKL(key->name != NULL);
  CHECKL(FUNCHECK(key->check));
  return TRUE;
}


Bool ArgCheck(Arg arg)
{
  CHECKL(arg != NULL);
  CHECKD(Key, arg->key);
  CHECKL(arg->key->check(arg));
  return TRUE;
}


/* ArgCheck -- check the validity of an argument list */

Bool ArgListCheck(ArgList args)
{
  Index i;
  CHECKL(args != NULL);
  for (i = 0; args[i].key != MPS_KEY_ARGS_END; ++i) {
    CHECKL(i < MPS_ARGS_MAX);
    CHECKD_NOSIG(Arg, &args[i]);
  }
  return TRUE;
}


/* ArgPick -- try to pick an argument out of the argument list by keyword */

Bool ArgPick(ArgStruct *argOut, ArgList args, Key key) {
  Index i;
  
  AVER(argOut != NULL);
  AVERT(ArgList, args);
  AVERT(Key, key);

  for (i = 0; args[i].key != MPS_KEY_ARGS_END; ++i)
    if (args[i].key == key)
      goto found;
  return FALSE;

found:
  AVERT(Arg, &args[i]);
  *argOut = args[i];
  for(;;) {
    args[i] = args[i + 1];
    if (args[i].key == MPS_KEY_ARGS_END)
      break;
    ++i;
  }
  return TRUE;
}


/* ArgRequire -- take a required argument out of the argument list by keyword */

void ArgRequire(ArgStruct *argOut, ArgList args, Key key) {
  Bool b = ArgPick(argOut, args, key);
  ASSERT(b, key->name);
}


/* ArgTrivVarargs -- class method to ignore deprecated varargs */

void ArgTrivVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  UNUSED(varargs);
  args[0].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
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
