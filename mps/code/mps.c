/* mps.c: MEMORY POOL SYSTEM ALL-IN-ONE TRANSLATION UNIT
 *
 * $Id$
 * Copyright (C) 2012-2014 Ravenbrook Limited.  See end of file for license.
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
#include "tree.c"
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
#include "poolamsi.c"
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
#include "prmcan.c"     /* generic protection mutator context */
#include "span.c"       /* generic stack probe */
#include "ssan.c"       /* generic stack scanner */

/* Mac OS X on 32-bit Intel built with Clang or GCC */

#elif defined(MPS_PF_XCI3LL) || defined(MPS_PF_XCI3GC)

#include "lockix.c"     /* Posix locks */
#include "thxc.c"       /* OS X Mach threading */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protxc.c"     /* OS X Mach exception handling */
#include "proti3.c"     /* 32-bit Intel mutator context decoding */
#include "prmci3xc.c"   /* 32-bit Intel for Mac OS X mutator context */
#include "span.c"       /* generic stack probe */
#include "ssixi3.c"     /* Posix on 32-bit Intel stack scan */

/* Mac OS X on 64-bit Intel build with Clang or GCC */

#elif defined(MPS_PF_XCI6LL) || defined(MPS_PF_XCI6GC)

#include "lockix.c"     /* Posix locks */
#include "thxc.c"       /* OS X Mach threading */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protxc.c"     /* OS X Mach exception handling */
#include "proti6.c"     /* 64-bit Intel mutator context decoding */
#include "prmci6xc.c"   /* 64-bit Intel for Mac OS X mutator context */
#include "span.c"       /* generic stack probe */
#include "ssixi6.c"     /* Posix on 64-bit Intel stack scan */

/* FreeBSD on 32-bit Intel built with GCC */

#elif defined(MPS_PF_FRI3GC)

#include "lockix.c"     /* Posix locks */
#include "thix.c"       /* Posix threading */
#include "pthrdext.c"   /* Posix thread extensions */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protsgix.c"   /* Posix signal handling */
#include "prmcan.c"     /* generic mutator context */
#include "prmci3fr.c"   /* 32-bit Intel for FreeBSD mutator context */
#include "span.c"       /* generic stack probe */
#include "ssixi3.c"     /* Posix on 32-bit Intel stack scan */

/* FreeBSD on 64-bit Intel built with GCC */

#elif defined(MPS_PF_FRI6GC)

#include "lockix.c"     /* Posix locks */
#include "thix.c"       /* Posix threading */
#include "pthrdext.c"   /* Posix thread extensions */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protsgix.c"   /* Posix signal handling */
#include "prmcan.c"     /* generic mutator context */
#include "prmci6fr.c"   /* 64-bit Intel for FreeBSD mutator context */
#include "span.c"       /* generic stack probe */
#include "ssixi6.c"     /* Posix on 64-bit Intel stack scan */

/* Linux on 32-bit Intel with GCC */

#elif defined(MPS_PF_LII3GC)

#include "lockli.c"     /* Linux locks */
#include "thix.c"       /* Posix threading */
#include "pthrdext.c"   /* Posix thread extensions */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protli.c"     /* Linux protection */
#include "proti3.c"     /* 32-bit Intel mutator context */
#include "prmci3li.c"   /* 32-bit Intel for Linux mutator context */
#include "span.c"       /* generic stack probe */
#include "ssixi3.c"     /* Posix on 32-bit Intel stack scan */

/* Linux on 64-bit Intel with GCC or Clang */

#elif defined(MPS_PF_LII6GC) || defined(MPS_PF_LII6LL)

#include "lockli.c"     /* Linux locks */
#include "thix.c"       /* Posix threading */
#include "pthrdext.c"   /* Posix thread extensions */
#include "vmix.c"       /* Posix virtual memory */
#include "protix.c"     /* Posix protection */
#include "protli.c"     /* Linux protection */
#include "proti6.c"     /* 64-bit Intel mutator context */
#include "prmci6li.c"   /* 64-bit Intel for Linux mutator context */
#include "span.c"       /* generic stack probe */
#include "ssixi6.c"     /* Posix on 64-bit Intel stack scan */

/* Windows on 32-bit Intel with Microsoft Visual Studio */

#elif defined(MPS_PF_W3I3MV)

#include "lockw3.c"     /* Windows locks */
#include "thw3.c"       /* Windows threading */
#include "thw3i3.c"     /* Windows on 32-bit Intel thread stack scan */
#include "vmw3.c"       /* Windows virtual memory */
#include "protw3.c"     /* Windows protection */
#include "proti3.c"     /* 32-bit Intel mutator context decoding */
#include "prmci3w3.c"   /* Windows on 32-bit Intel mutator context */
#include "ssw3i3mv.c"   /* Windows on 32-bit Intel stack scan for Microsoft C */
#include "spw3i3.c"     /* Windows on 32-bit Intel stack probe */
#include "mpsiw3.c"     /* Windows interface layer extras */

/* Windows on 64-bit Intel with Microsoft Visual Studio */

#elif defined(MPS_PF_W3I6MV)

#include "lockw3.c"     /* Windows locks */
#include "thw3.c"       /* Windows threading */
#include "thw3i6.c"     /* Windows on 64-bit Intel thread stack scan */
#include "vmw3.c"       /* Windows virtual memory */
#include "protw3.c"     /* Windows protection */
#include "proti6.c"     /* 64-bit Intel mutator context decoding */
#include "prmci6w3.c"   /* Windows on 64-bit Intel mutator context */
#include "ssw3i6mv.c"   /* Windows on 64-bit Intel stack scan for Microsoft C */
#include "spw3i6.c"     /* Windows on 64-bit Intel stack probe */
#include "mpsiw3.c"     /* Windows interface layer extras */

/* Windows on 32-bit Intel with Pelles C */

#elif defined(MPS_PF_W3I3PC)

#include "lockw3.c"     /* Windows locks */
#include "thw3.c"       /* Windows threading */
#include "thw3i3.c"     /* Windows on 32-bit Intel thread stack scan */
#include "vmw3.c"       /* Windows virtual memory */
#include "protw3.c"     /* Windows protection */
#include "proti3.c"     /* 32-bit Intel mutator context decoding */
#include "prmci3w3.c"   /* Windows on 32-bit Intel mutator context */
#include "ssw3i3pc.c"   /* Windows on 32-bit stack scan for Pelles C */
#include "spw3i3.c"     /* 32-bit Intel stack probe */
#include "mpsiw3.c"     /* Windows interface layer extras */

/* Windows on 64-bit Intel with Pelles C */

#elif defined(MPS_PF_W3I6PC)

#include "lockw3.c"     /* Windows locks */
#include "thw3.c"       /* Windows threading */
#include "thw3i6.c"     /* Windows on 64-bit Intel thread stack scan */
#include "vmw3.c"       /* Windows virtual memory */
#include "protw3.c"     /* Windows protection */
#include "proti6.c"     /* 64-bit Intel mutator context decoding */
#include "prmci6w3.c"   /* Windows on 64-bit Intel mutator context */
#include "ssw3i6pc.c"   /* Windows on 64-bit stack scan for Pelles C */
#include "spw3i6.c"     /* 64-bit Intel stack probe */
#include "mpsiw3.c"     /* Windows interface layer extras */

#else

#error "Unknown platform -- can't determine platform specific parts."

#endif



/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2012-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
