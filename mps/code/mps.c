/* mps.c: MEMORY POOL SYSTEM ALL-IN-ONE TRANSLATION UNIT
 *
 * $Id$
 * Copyright (C) 2012-2018 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This file can be compiled to create the complete MPS library in
 * a single compilation, allowing the compiler to apply global optimizations
 * and inlining effectively.  On most modern compilers this is also faster
 * than compiling each file separately.
 *
 * .purpose.universal: This file also allows simple building of a macOS
 * "universal" (multiple architecture) binary when the set of source files
 * differs by architecture.  It may work for other platforms in a similar
 * manner.
 *
 * .rule.simple: This file should never be more than a simple list of
 * includes of other source code, with ifdefs for platform configuration,
 * which closely mirror those in the makefiles.
 */


/* Platform interface
 *
 * This must be included first as it defines symbols which affect system
 * headers, such as _POSIX_C_SOURCE _REENTRANT etc.
 */

#include "mpstd.h"


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
#include "protocol.c"
#include "pool.c"
#include "poolabs.c"
#include "trace.c"
#include "traceanc.c"
#include "scan.c"
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
#include "tree.c"
#include "rangetree.c"
#include "splay.c"
#include "cbs.c"
#include "ss.c"
#include "version.c"
#include "table.c"
#include "arg.c"
#include "abq.c"
#include "range.c"
#include "freelist.c"
#include "sa.c"
#include "nailboard.c"
#include "land.c"
#include "failover.c"
#include "vm.c"
#include "policy.c"

/* Additional pool classes */

#include "poolamc.c"
#include "poolams.c"
#include "poolawl.c"
#include "poollo.c"
#include "poolsnc.c"
#include "poolmv2.c"
#include "poolmvff.c"

/* ANSI Plinth */

#if defined(PLINTH)     /* see CONFIG_PLINTH_NONE in config.h  */
#include "mpsliban.c"
#include "mpsioan.c"
#endif

/* Generic ("ANSI") platform */

#if defined(PLATFORM_ANSI)

#include "lockan.c"     /* generic locks */
#include "than.c"       /* generic threads manager */
#include "vman.c"       /* malloc-based pseudo memory mapping */
#include "protan.c"     /* generic memory protection */
#include "prmcan.c"     /* generic operating system mutator context */
#include "prmcanan.c"   /* generic architecture mutator context */
#include "span.c"       /* generic stack probe */

/* macOS on IA-32 built with Clang or GCC */

#elif defined(MPS_PF_XCI3LL) || defined(MPS_PF_XCI3GC)

#include "lockix.c"     /* Posix locks */
#include "thxc.c"       /* macOS Mach threading */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protxc.c"     /* macOS Mach exception handling */
#include "prmci3.c"     /* IA-32 mutator context */
#include "prmcxc.c"     /* macOS mutator context */
#include "prmcxci3.c"   /* IA-32 for macOS mutator context */
#include "span.c"       /* generic stack probe */

/* macOS on x86-64 build with Clang or GCC */

#elif defined(MPS_PF_XCI6LL) || defined(MPS_PF_XCI6GC)

#include "lockix.c"     /* Posix locks */
#include "thxc.c"       /* macOS Mach threading */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protxc.c"     /* macOS Mach exception handling */
#include "prmci6.c"     /* x86-64 mutator context */
#include "prmcxc.c"     /* macOS mutator context */
#include "prmcxci6.c"   /* x86-64 for macOS mutator context */
#include "span.c"       /* generic stack probe */

/* FreeBSD on IA-32 built with GCC or Clang */

#elif defined(MPS_PF_FRI3GC) || defined(MPS_PF_FRI3LL)

#include "lockix.c"     /* Posix locks */
#include "thix.c"       /* Posix threading */
#include "pthrdext.c"   /* Posix thread extensions */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protsgix.c"   /* Posix signal handling */
#include "prmcanan.c"   /* generic architecture mutator context */
#include "prmcix.c"     /* Posix mutator context */
#include "prmcfri3.c"   /* IA-32 for FreeBSD mutator context */
#include "span.c"       /* generic stack probe */

/* FreeBSD on x86-64 built with GCC or Clang */

#elif defined(MPS_PF_FRI6GC) || defined(MPS_PF_FRI6LL)

#include "lockix.c"     /* Posix locks */
#include "thix.c"       /* Posix threading */
#include "pthrdext.c"   /* Posix thread extensions */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protsgix.c"   /* Posix signal handling */
#include "prmcanan.c"   /* generic architecture mutator context */
#include "prmcix.c"     /* Posix mutator context */
#include "prmcfri6.c"   /* x86-64 for FreeBSD mutator context */
#include "span.c"       /* generic stack probe */

/* Linux on IA-32 with GCC */

#elif defined(MPS_PF_LII3GC)

#include "lockix.c"     /* Posix locks */
#include "thix.c"       /* Posix threading */
#include "pthrdext.c"   /* Posix thread extensions */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protsgix.c"   /* Posix signal handling */
#include "prmci3.c"     /* IA-32 mutator context */
#include "prmcix.c"     /* Posix mutator context */
#include "prmclii3.c"   /* IA-32 for Linux mutator context */
#include "span.c"       /* generic stack probe */

/* Linux on x86-64 with GCC or Clang */

#elif defined(MPS_PF_LII6GC) || defined(MPS_PF_LII6LL)

#include "lockix.c"     /* Posix locks */
#include "thix.c"       /* Posix threading */
#include "pthrdext.c"   /* Posix thread extensions */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protsgix.c"   /* Posix signal handling */
#include "prmci6.c"     /* x86-64 mutator context */
#include "prmcix.c"     /* Posix mutator context */
#include "prmclii6.c"   /* x86-64 for Linux mutator context */
#include "span.c"       /* generic stack probe */

/* Windows on IA-32 with Microsoft Visual Studio or Pelles C */

#elif defined(MPS_PF_W3I3MV) || defined(MPS_PF_W3I3PC)

#include "lockw3.c"     /* Windows locks */
#include "thw3.c"       /* Windows threading */
#include "vmw3.c"       /* Windows virtual memory */
#include "protw3.c"     /* Windows protection */
#include "prmci3.c"     /* IA-32 mutator context */
#include "prmcw3.c"     /* Windows mutator context */
#include "prmcw3i3.c"   /* Windows on IA-32 mutator context */
#include "spw3i3.c"     /* Windows on IA-32 stack probe */
#include "mpsiw3.c"     /* Windows interface layer extras */

/* Windows on x86-64 with Microsoft Visual Studio or Pelles C */

#elif defined(MPS_PF_W3I6MV) || defined(MPS_PF_W3I6PC)

#include "lockw3.c"     /* Windows locks */
#include "thw3.c"       /* Windows threading */
#include "vmw3.c"       /* Windows virtual memory */
#include "protw3.c"     /* Windows protection */
#include "prmci6.c"     /* x86-64 mutator context */
#include "prmcw3.c"     /* Windows mutator context */
#include "prmcw3i6.c"   /* Windows on x86-64 mutator context */
#include "spw3i6.c"     /* Windows on x86-64 stack probe */
#include "mpsiw3.c"     /* Windows interface layer extras */

#else

#error "Unknown platform -- can't determine platform specific parts."

#endif



/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2012-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
