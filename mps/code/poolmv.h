/* .impl.h.poolmv: MANUAL VARIABLE POOL
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This is the interface to the manual-variable pool class.
 *
 * .mv: Manual-variable pools manage variably-sized blocks of memory in a
 *  flexible manner.  They have higher overheads than a fixed-size pool.
 *
 * .init: This class adds the following arguments to PoolCreate:
 *
 *    Size extendBy
 *
 *  extendBy is the default number of bytes reserved by the pool at a time.
 *  A large size will make allocation cheaper but have a higher resource
 *  overhead.  A typical value might be 65536.  See note 2.
 *
 *    Size avgSize
 *
 *  avgSize is an estimate of the average size of an allocation, and is used
 *  to choose the size of internal tables.  An accurate estimate will
 *  improve the efficiency of the pool.  A low estimate will make the pool
 *  less space efficient.  A high estimate will make the pool less time
 *  efficient.  A typical value might be 32.  avgSize must not be less than
 *  extendBy.
 *
 *    Size maxSize
 *
 *  maxSize is an estimate of the maximum total size that the pool will
 *  reach.  Setting this parameter does not actually contrain the pool, but
 *  an accurate estimate will improve the efficiency of the pool.  maxSize
 *  must not be less than extendBy.
 *
 *  Notes
 *   2. The documentation could suggest a segment size according to the
 *      distribution of allocation size requests.  richard 1994-11-08
 */

#ifndef poolmv_h
#define poolmv_h


#include "mpmtypes.h"

typedef struct MVStruct *MV;

extern PoolClass PoolClassMV(void);

extern Bool MVCheck(MV mv);

#define MVPool(mv) (&(mv)->poolStruct)
extern Pool (MVPool)(MV mv);


#endif /* poolmv_h */


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
