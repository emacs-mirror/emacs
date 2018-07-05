/* rangetree.c -- binary trees of address ranges
 *
 * $Id$
 * Copyright (C) 2016 Ravenbrook Limited.  See end of file for license.
 */

#ifndef rangetree_h
#define rangetree_h

#include "mpmtypes.h"
#include "range.h"
#include "tree.h"

#define RangeTreeTree(rangeTree) (&(rangeTree)->treeStruct)
#define RangeTreeRange(rangeTree) (&(rangeTree)->rangeStruct)
#define RangeTreeOfTree(tree) PARENT(RangeTreeStruct, treeStruct, tree)
#define RangeTreeOfRange(range) PARENT(RangeTreeStruct, rangeStruct, range)

#define RangeTreeBase(block) RangeBase(RangeTreeRange(block))
#define RangeTreeLimit(block) RangeLimit(RangeTreeRange(block))
#define RangeTreeSetBase(block, addr) RangeSetBase(RangeTreeRange(block), addr)
#define RangeTreeSetLimit(block, addr) RangeSetLimit(RangeTreeRange(block), addr)
#define RangeTreeSize(block) RangeSize(RangeTreeRange(block))

extern void RangeTreeInit(RangeTree rangeTree, Addr base, Addr limit);
extern void RangeTreeInitFromRange(RangeTree rangeTree, Range range);
extern Bool RangeTreeCheck(RangeTree rangeTree);
extern void RangeTreeFinish(RangeTree rangeTree);


/* Compare and key functions for use with TreeFind, TreeInsert, etc.
 *
 * We pass the rangeTree base directly as a TreeKey (void *) assuming
 * that Addr can be encoded, possibly breaking <design/type/#addr.use>.
 * On an exotic platform where this isn't true, pass the address of
 * base: that is, add an &.
 */

#define RangeTreeKeyOfBaseVar(baseVar) ((TreeKey)(baseVar))
#define RangeTreeBaseOfKey(key)        ((Addr)(key))

extern Compare RangeTreeCompare(Tree tree, TreeKey key);
extern TreeKey RangeTreeKey(Tree tree);


/* RangeTreeStruct -- address range in a tree */

typedef struct RangeTreeStruct {
  TreeStruct treeStruct;
  RangeStruct rangeStruct;
} RangeTreeStruct;

#endif /* rangetree_h */

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
