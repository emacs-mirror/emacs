/* rangetree.c -- binary trees of address ranges
 *
 * $Id$
 * Copyright (C) 2016-2020 Ravenbrook Limited.  See end of file for license.
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

extern void RangeTreeInit(RangeTree rangeTree, Range range);
extern Bool RangeTreeCheck(RangeTree rangeTree);
extern void RangeTreeFinish(RangeTree rangeTree);


/* Compare and key functions for use with TreeFind, TreeInsert, etc.
 *
 * We pass the rangeTree base directly as a TreeKey (void *) assuming
 * that Addr can be encoded, possibly breaking <design/type#.addr.use>.
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
 * Copyright (C) 2016-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
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
