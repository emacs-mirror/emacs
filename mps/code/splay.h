/* splay.h: SPLAY TREE HEADER
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .source: <design/splay/>
 */

#ifndef splay_h
#define splay_h

#include "mpmtypes.h" /* for Res, etc. */
#include "tree.h"


typedef struct SplayTreeStruct *SplayTree;

typedef Bool (*SplayTestNodeMethod)(SplayTree tree, Tree node,
                                    void *closureP, Size closureS);
typedef Bool (*SplayTestTreeMethod)(SplayTree tree, Tree node,
                                    void *closureP, Size closureS);
typedef Res (*SplayNodeDescribeMethod)(Tree node, mps_lib_FILE *stream);

typedef void (*SplayUpdateNodeMethod)(SplayTree tree, Tree node);
extern void SplayTrivUpdate(SplayTree tree, Tree node);

typedef struct SplayTreeStruct {
  TreeCompare compare;
  SplayUpdateNodeMethod updateNode;
  Tree root;
} SplayTreeStruct;


extern Bool SplayTreeCheck(SplayTree tree);
extern void SplayTreeInit(SplayTree tree,
                          TreeCompare compare,
                          SplayUpdateNodeMethod updateNode);
extern void SplayTreeFinish(SplayTree tree);

extern Res SplayTreeInsert(SplayTree tree, Tree node, void *key);
extern Res SplayTreeDelete(SplayTree tree, Tree node, void *key);

extern Res SplayTreeSearch(Tree *nodeReturn,
                           SplayTree tree, void *key );
extern Res SplayTreeNeighbours(Tree *leftReturn,
                               Tree *rightReturn,
                               SplayTree tree, void *key);

extern Tree SplayTreeFirst(SplayTree tree, void *zeroKey);
extern Tree SplayTreeNext(SplayTree tree, Tree oldNode,
                               void *oldKey);

extern Bool SplayFindFirst(Tree *nodeReturn, SplayTree tree,
                           SplayTestNodeMethod testNode,
                           SplayTestTreeMethod testTree,
                           void *closureP, Size closureS);
extern Bool SplayFindLast(Tree *nodeReturn, SplayTree tree,
                          SplayTestNodeMethod testNode,
                          SplayTestTreeMethod testTree,
                          void *closureP, Size closureS);

extern void SplayNodeRefresh(SplayTree tree, Tree node, void *key);

extern Res SplayTreeDescribe(SplayTree tree, mps_lib_FILE *stream,
                             SplayNodeDescribeMethod nodeDescribe);

extern Bool SplayRoot(Tree *nodeReturn, SplayTree tree);


#endif /* splay_h */


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
