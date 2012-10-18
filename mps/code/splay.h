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


typedef struct SplayTreeStruct *SplayTree;
typedef struct SplayNodeStruct *SplayNode;
typedef unsigned Compare;
typedef Compare (*SplayCompareMethod)(void *key, SplayNode node);
typedef Bool (*SplayTestNodeMethod)(SplayTree tree, SplayNode node,
                                    void *closureP, Size closureS);
typedef Bool (*SplayTestTreeMethod)(SplayTree tree, SplayNode node,
                                    void *closureP, Size closureS);
typedef void (*SplayUpdateNodeMethod)(SplayTree tree, SplayNode node,
                                      SplayNode leftChild,
                                      SplayNode rightChild);
typedef Res (*SplayNodeDescribeMethod)(SplayNode node, mps_lib_FILE *stream);
enum {
  CompareLESS = 1,
  CompareEQUAL,
  CompareGREATER
};


typedef struct SplayTreeStruct {
  SplayCompareMethod compare;
  SplayUpdateNodeMethod updateNode;
  SplayNode root;
} SplayTreeStruct;

typedef struct SplayNodeStruct {
  SplayNode left;
  SplayNode right;
} SplayNodeStruct;


extern Bool SplayTreeCheck(SplayTree tree);
extern Bool SplayNodeCheck(SplayNode node);
extern void SplayTreeInit(SplayTree tree, SplayCompareMethod compare,
                          SplayUpdateNodeMethod updateNode);
extern void SplayNodeInit(SplayNode node);
extern void SplayNodeFinish(SplayNode node);
extern void SplayTreeFinish(SplayTree tree);

extern Res SplayTreeInsert(SplayTree tree, SplayNode node, void *key);
extern Res SplayTreeDelete(SplayTree tree, SplayNode node, void *key);

extern Res SplayTreeSearch(SplayNode *nodeReturn,
                           SplayTree tree, void *key );
extern Res SplayTreeNeighbours(SplayNode *leftReturn,
                               SplayNode *rightReturn,
                               SplayTree tree, void *key);

extern SplayNode SplayTreeFirst(SplayTree tree, void *zeroKey);
extern SplayNode SplayTreeNext(SplayTree tree, SplayNode oldNode,
                               void *oldKey);

extern Bool SplayFindFirst(SplayNode *nodeReturn, SplayTree tree,
                           SplayTestNodeMethod testNode,
                           SplayTestTreeMethod testTree,
                           void *closureP, Size closureS);
extern Bool SplayFindLast(SplayNode *nodeReturn, SplayTree tree,
                          SplayTestNodeMethod testNode,
                          SplayTestTreeMethod testTree,
                          void *closureP, Size closureS);

extern void SplayNodeRefresh(SplayTree tree, SplayNode node, void *key);

extern Res SplayTreeDescribe(SplayTree tree, mps_lib_FILE *stream,
                             SplayNodeDescribeMethod nodeDescribe);

extern Bool SplayRoot(SplayNode *nodeReturn, SplayTree tree);


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
