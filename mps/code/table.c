/* table.h: A dictionary mapping a Word to a void*
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .note.good-hash: As is common in hash table implementations, we
 * assume that the hash function is good.
 */

#include "table.h"
#include "mpm.h"

#include <stddef.h>


SRCID(table, "$Id$");


/* tableHash -- return a hash value from an address
 *
 * This uses a single cycle of an MLCG, more commonly seen as a 
 * pseudorandom number generator.  It works extremely well as a 
 * hash function.
 *
 * (In particular, it is substantially better than simply doing this:
 *   seed = (unsigned long)addr * 48271;
 * Tested by RHSK 2010-12-28.)
 *
 * This MLCG is a full period generator: it cycles through every 
 * number from 1 to m-1 before repeating.  Therefore, no two numbers 
 * in that range hash to the same value.  Furthermore, it has prime 
 * modulus, which tends to avoid recurring patterns in the low-order 
 * bits, which is good because the hash will be used modulus the 
 * number of slots in the table.
 *
 * Of course it's only a 31-bit cycle, so we start by losing the top 
 * bit of the address, but that's hardly a great problem.
 *
 * See `rnd` in testlib.c for more technical details.
 *
 * The implementation is quite subtle.  See rnd() in testlib.c, where 
 * it has been exhaustively (ie: totally) tested.  RHSK 2010-12-28.
 *
 * NOTE: According to NB, still a fine function for producing a 31-bit hash
 * value, although of course it only hashes on the lower 31 bits of the
 * key; we could cheaply make it choose a different 31 bits if we'd prefer
 * (e.g. ((key >> 2) & 0x7FFFFFFF)), or combine more of the key bits (e.g.
 * ((key ^ (key >> 31)) & 0x7fffffff)).
 */
 
#define R_m 2147483647UL
#define R_a 48271UL

typedef Word Hash;

static Hash tableHash(Word key)
{
  Hash hash = (Hash)(key & 0x7FFFFFFF);
  /* requires m == 2^31-1, a < 2^16 */
  Hash bot = R_a * (hash & 0x7FFF);
  Hash top = R_a * (hash >> 15);
  hash = bot + ((top & 0xFFFF) << 15) + (top >> 16);
  if(hash > R_m)
    hash -= R_m;
  return hash;
}


Bool TableCheck(Table table)
{
  CHECKS(Table, table);
  CHECKL(table->count <= table->length);
  CHECKL(table->length == 0 || table->array != NULL);
  CHECKL(FUNCHECK(table->alloc));
  CHECKL(FUNCHECK(table->free));
  /* can't check allocClosure -- it could be anything */
  CHECKL(table->unusedKey != table->deletedKey);
  return TRUE;
}


static Bool entryIsActive(Table table, TableEntry entry)
{
  return !(entry->key == table->unusedKey ||
           entry->key == table->deletedKey);
}


/* tableFind -- finds the entry for this key, or NULL
 *
 * .worst: In the worst case, this looks at every slot before giving up,
 * but that's what you have to do in a closed hash table, to make sure
 * that all the items still fit in after growing the table.
 */

static TableEntry tableFind(Table table, Word key, Bool skip_deleted)
{
  Hash hash;
  Index i;
  Word mask;

  /* .find.visit: Ensure the length is a power of two so that the stride
     is coprime and so visits all entries in the array eventually. */
  AVER(WordIsP2(table->length)); /* .find.visit */

  mask = table->length - 1; 
  hash = tableHash(key) & mask;
  i = hash;
  do {
    Word k = table->array[i].key;
    if (k == key ||
        k == table->unusedKey ||
        (!skip_deleted && key == table->deletedKey))
      return &table->array[i];
    i = (i + (hash | 1)) & mask; /* .find.visit */
  } while(i != hash);

  return NULL;
}


/* TableGrow -- increase the capacity of the table
 *
 * Ensure the transform's hashtable can accommodate N entries (filled 
 * slots), without becoming cramped.  If necessary, resize the 
 * hashtable by allocating a new one and rehashing all old entries.
 * If insufficient memory, return error without modifying table.
 *
 * .hash.spacefraction: As with all closed hash tables, we must choose 
 * an appropriate proportion of slots to remain free.  More free slots 
 * help avoid large-sized contiguous clumps of full cells and their 
 * associated linear search costs. 
 *
 * .hash.initial: Any reasonable number.
 *
 * .hash.growth: A compromise between space inefficiency (growing bigger 
 * than required) and time inefficiency (growing too slowly, with all 
 * the rehash costs at every step).  A factor of 2 means that at the 
 * point of growing to a size X table, hash-work equivalent to filling 
 * a size-X table has already been done.  So we do at most 2x the 
 * hash-work we would have done if we had been able to guess the right 
 * table size initially.
 *
 * Numbers of slots maintain this relation:
 *     occupancy <= capacity < enough <= cSlots
 */

#define SPACEFRACTION 0.75      /* .hash.spacefraction */

Res TableGrow(Table table, Count extraCapacity)
{
  TableEntry oldArray, newArray;
  Count oldLength, newLength;
  Count required, minimum;
  Count i, found;

  required = table->count + extraCapacity;
  if (required < table->count)  /* overflow? */
    return ResLIMIT;

  /* Calculate the minimum table length that would allow for the required
     capacity without growing again. */
  minimum = (Count)(required / SPACEFRACTION);
  if (minimum < required)       /* overflow? */
    return ResLIMIT;

  /* Double the table length until it's larger than the minimum */
  oldLength = table->length;
  newLength = oldLength;
  while(newLength < minimum) {
    Count doubled = newLength > 0 ? newLength * 2 : 1; /* .hash.growth */
    if (doubled <= newLength)   /* overflow? */
      return ResLIMIT;
    newLength = doubled;
  }

  if (newLength == oldLength)   /* already enough space? */
    return ResOK;

  /* TODO: An event would be good here */

  oldArray = table->array;
  newArray = table->alloc(table->allocClosure,
                          sizeof(TableEntryStruct) * newLength);
  if(newArray == NULL)
    return ResMEMORY;

  for(i = 0; i < newLength; ++i) {
    newArray[i].key = table->unusedKey;
    newArray[i].value = NULL;
  }
 
  table->length = newLength;
  table->array = newArray;

  found = 0;
  for(i = 0; i < oldLength; ++i) {
    if (entryIsActive(table, &oldArray[i])) {
      TableEntry entry;
      entry = tableFind(table, oldArray[i].key, FALSE /* none deleted */);
      AVER(entry != NULL);
      AVER(entry->key == table->unusedKey);
      entry->key = oldArray[i].key;
      entry->value = oldArray[i].value;
      ++found;
    }
  }
  AVER(found == table->count);

  if (oldLength > 0) {
    AVER(oldArray != NULL);
    table->free(table->allocClosure,
                oldArray,
                sizeof(TableEntryStruct) * oldLength);
  }

  return ResOK;
}


/* TableCreate -- makes a new table */

extern Res TableCreate(Table *tableReturn,
                       Count length,
                       TableAllocFunction tableAlloc,
                       TableFreeFunction tableFree,
                       void *allocClosure,
                       Word unusedKey,
                       Word deletedKey)
{
  Table table;
  Res res;

  AVER(tableReturn != NULL);
  AVER(FUNCHECK(tableAlloc));
  AVER(FUNCHECK(tableFree));
  AVER(unusedKey != deletedKey);

  table = tableAlloc(allocClosure, sizeof(TableStruct));
  if(table == NULL)
    return ResMEMORY;

  table->length = 0;
  table->count = 0;
  table->array = NULL;
  table->alloc = tableAlloc;
  table->free = tableFree;
  table->allocClosure = allocClosure;
  table->unusedKey = unusedKey;
  table->deletedKey = deletedKey;
  table->sig = TableSig;
  
  AVERT(Table, table);

  res = TableGrow(table, length);
  if (res != ResOK)
    return res;
 
  *tableReturn = table;
  return ResOK;
}


/* TableDestroy -- destroy a table */

extern void TableDestroy(Table table)
{
  AVER(table != NULL);
  if (table->length > 0) {
    AVER(table->array != NULL);
    table->free(table->allocClosure,
                table->array,
                sizeof(TableEntryStruct) * table->length);
  }
  table->sig = SigInvalid;
  table->free(table->allocClosure, table, sizeof(TableStruct));
}


/* TableLookup -- look up */

extern Bool TableLookup(void **valueReturn, Table table, Word key)
{
  TableEntry entry = tableFind(table, key, TRUE /* skip deleted */);

  if(entry == NULL || !entryIsActive(table, entry))
    return FALSE;
  *valueReturn = entry->value;
  return TRUE;
}


/* TableDefine -- add a new mapping */

extern Res TableDefine(Table table, Word key, void *value)
{
  TableEntry entry;
  
  AVER(key != table->unusedKey);
  AVER(key != table->deletedKey);

  if (table->count >= table->length * SPACEFRACTION) {
    Res res = TableGrow(table, 1);
    if (res != ResOK)
      return res;
    entry = tableFind(table, key, FALSE /* no deletions yet */);
    AVER(entry != NULL);
    if (entryIsActive(table, entry))
      return ResFAIL;
  } else {
    entry = tableFind(table, key, TRUE /* skip deleted */);
    if (entry != NULL && entryIsActive(table, entry))
      return ResFAIL;
    /* Search again to find the best slot, deletions included. */
    entry = tableFind(table, key, FALSE /* don't skip deleted */);
    AVER(entry != NULL);
  }

  entry->key = key;
  entry->value = value;
  ++table->count;

  return ResOK;
}


/* TableRedefine -- redefine an existing mapping */

extern Res TableRedefine(Table table, Word key, void *value)
{
  TableEntry entry;
 
  AVER(key != table->unusedKey);
  AVER(key != table->deletedKey);

  entry = tableFind(table, key, TRUE /* skip deletions */);
  if (entry == NULL || !entryIsActive(table, entry))
    return ResFAIL;
  AVER(entry->key == key);
  entry->value = value;
  return ResOK;
}


/* TableRemove -- remove a mapping */

extern Res TableRemove(Table table, Word key)
{
  TableEntry entry;

  AVER(key != table->unusedKey);
  AVER(key != table->deletedKey);

  entry = tableFind(table, key, TRUE);
  if (entry == NULL || !entryIsActive(table, entry))
    return ResFAIL;
  entry->key = table->deletedKey;
  --table->count;
  return ResOK;
}


/* TableMap -- apply a function to all the mappings */

extern void TableMap(Table table,
                     void (*fun)(void *closure, Word key, void*value),
                     void *closure)
{
  Index i;
  for (i = 0; i < table->length; i++)
    if (entryIsActive(table, &table->array[i]))
      (*fun)(closure, table->array[i].key, table->array[i].value);
}


/* TableCount -- count the number of mappings in the table */

extern Count TableCount(Table table)
{
  return table->count;
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
