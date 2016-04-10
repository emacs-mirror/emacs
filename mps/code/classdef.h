/* classdef.h -- table of MPS classes
 *
 * $Id$
 * Copyright (C) 2016 Ravenbrook Limited.  See end of file for license.
 */

#ifndef classdef_h
#define classdef_h

/* CLASSES -- the class table
 *
 * "identifier" is the root of the class name, used to form other
 * names.  For example, structures of class Inst are of type
 * InstStruct and the its classes start with InstClassStruct
 * structures.
 *
 * "kind" determines the class object.  For example, the class of
 * CBSLand is stored in a LandClassStruct and can be checked by
 * LandClassCheck.
 *
 * "super" is the superclass of the class.
 */

#define CLASSES(CLASS, X) \
  /*       identifier      kind       super */ \
  CLASS(X, Inst,           Inst,      NoSuper) \
  CLASS(X, AbstractArena,  Arena,     Inst) \
  CLASS(X, ClientArena,    Arena,     AbstractArena) \
  CLASS(X, VMArena,        Arena,     AbstractArena) \
  CLASS(X, Buffer,         Buffer,    Inst) \
  CLASS(X, SegBuf,         Buffer,    Buffer) \
  CLASS(X, amcBuf,         Buffer,    SegBuf) \
  CLASS(X, RankBuf,        Buffer,    SegBuf) \
  CLASS(X, SNCBuf,         Buffer,    RankBuf) \
  CLASS(X, Land,           Land,      Inst) \
  CLASS(X, Failover,       Land,      Land) \
  CLASS(X, Freelist,       Land,      Land) \
  CLASS(X, CBS,            Land,      Land) \
  CLASS(X, CBSFast,        Land,      CBS) \
  CLASS(X, CBSZoned,       Land,      CBSFast) \
  CLASS(X, Seg,            Seg,       Inst) \
  CLASS(X, MRGLinkSeg,     Seg,       Seg) \
  CLASS(X, GCSeg,          Seg,       Seg) \
  CLASS(X, amcSeg,         Seg,       GCSeg) \
  CLASS(X, AWLSeg,         Seg,       GCSeg) \
  CLASS(X, LOSeg,          Seg,       GCSeg) \
  CLASS(X, MRGRefSeg,      Seg,       GCSeg) \
  CLASS(X, SNCSeg,         Seg,       GCSeg) \
  CLASS(X, AMSSeg,         Seg,       GCSeg) \
  CLASS(X, AMSTSeg,        Seg,       AMSSeg) \
  CLASS(X, AbstractPool,   Pool,      Inst) \
  CLASS(X, MFSPool,        Pool,      AbstractPool) \
  CLASS(X, MRGPool,        Pool,      AbstractPool) \
  CLASS(X, NPool,          Pool,      AbstractPool) \
  CLASS(X, OOMPool,        Pool,      AbstractPool) \
  CLASS(X, MVFFPool,       Pool,      AbstractPool) \
  CLASS(X, MVFFDebugPool,  Pool,      MVFFPool) \
  CLASS(X, AbstractBufferPool, Pool,  AbstractPool) \
  CLASS(X, MVTPool,        Pool,      AbstractBufferPool) \
  CLASS(X, MVPool,         Pool,      AbstractBufferPool) \
  CLASS(X, MVDebugPool,    Pool,      MVPool) \
  CLASS(X, AbstractSegBufPool, Pool,  AbstractBufferPool) \
  CLASS(X, LOPool,         Pool,      AbstractSegBufPool) \
  CLASS(X, AMCZPool,       Pool,      AbstractSegBufPool) \
  CLASS(X, AMCPool,        Pool,      AMCZPool) \
  CLASS(X, AbstractScanPool, Pool,    AbstractSegBufPool) \
  CLASS(X, SNCPool,        Pool,      AbstractScanPool) \
  CLASS(X, AbstractCollectPool, Pool, AbstractScanPool) \
  CLASS(X, AWLPool,        Pool,      AbstractCollectPool) \
  CLASS(X, AMSPool,        Pool,      AbstractCollectPool) \
  CLASS(X, AMSDebugPool,   Pool,      AMSPool) \
  CLASS(X, AMSTPool,       Pool,      AMSPool)

#endif /* classdef_h */

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
