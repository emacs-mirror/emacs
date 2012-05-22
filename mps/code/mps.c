/* mps.c: MEMORY POOL SYSTEM ALL-IN-ONE TRANSLATION UNIT
 *
 * $Id$
 * Copyright (C) 2012 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This file can be compiled to create the complete MPS library in
 * a single compilation, allowing the compiler to apply global optimizations
 * and inlining effectively.  On most modern compilers this is also faster
 * than compiling each file separately.
 *
 * .purpose.universal: This file also allows simple building of a Mac OS X
 * "universal" (multiple architecture) binary when the set of source files
 * differs by architecture.  It may work for other platforms in a similar
 * manner.
 *
 * .rule.simple: This file should never be more than a simple list of
 * includes of other source code, with ifdefs for platform configuration,
 * which closely mirror those in the makefiles.
 */

/* MPM Core */

#include "mpsi.c"
#include "mpm.c"
#include "arenavm.c"
#include "arenacl.c"
#include "arena.c"
#include "global.c"
#include "locus.c"
#include "tract.c"
#include "walk.c"
#include "reserv.c"
#include "protocol.c"
#include "pool.c"
#include "poolabs.c"
#include "trace.c"
#include "traceanc.c"
#include "root.c"
#include "seg.c"
#include "format.c"
#include "buffer.c"
#include "ref.c"
#include "bt.c"
#include "ring.c"
#include "shield.c"
#include "ld.c"
#include "event.c"
#include "sac.c"
#include "message.c"
#include "poolmrg.c"
#include "poolmfs.c"
#include "poolmv.c"
#include "dbgpool.c"
#include "dbgpooli.c"
#include "boot.c"
#include "meter.c"
#include "splay.c"
#include "cbs.c"
#include "diag.c"
#include "version.c"

/* Additional pool classes */

#include "poolamc.c"
#include "poolams.c"
#include "poolamsi.c"
#include "poolawl.c"
#include "poollo.c"
#include "poolsnc.c"
#include "pooln.c"
#include "poolmvff.c"

/* ANSI Plinth */

#include "mpsliban.c"
#include "mpsioan.c"

/* Platform interface */

#include "mpstd.h"

#if defined(MPS_PF_XCI3LL)

#include "lockix.c"
#include "than.c"
#include "vmix.c"
#include "protix.c"
#include "protsgix.c"
#include "prmcan.c"
#include "span.c"
#include "ssixi3.c"

#elif defined(MPS_PF_XCI6LL)

#include "lockix.c"
#include "than.c"
#include "vmix.c"
#include "protix.c"
#include "protsgix.c"
#include "prmcan.c"
#include "span.c"
#include "ssan.c"

#else

#error "Unknown platform -- can't determine platform specific parts."

#endif



/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2012 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
