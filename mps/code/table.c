/* table.c: A dictionary mapping a Word to a void*
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 */

#include "table.h"
#include "mpm.h"

#include <stddef.h>


SRCID(table, "$Id$");


#define TABLE_REHASH_TRIES  8

#define TableLength(table) ((Count)1 << (table)->log2length)


Bool TableCheck(Table table)
{
  CHECKS(Table, table);
  CHECKL(table->count <= TableLength(table));
  CHECKL(table->array != NULL);
  CHECKL(table->maxChainLength >= 2);
  CHECKL(table->maxChainLength <= TableLength(table));
  CHECKL(FUNCHECK(table->alloc));
  CHECKL(FUNCHECK(table->free));
  /* can't check allocClosure -- it could be anything */
  CHECKL(table->unusedKey != table->deletedKey);
  CHECKL((table->posHashParamStruct.multiplicand & 1) != 0);
  CHECKL((table->skipHashParamStruct.multiplicand & 1) != 0);
  return TRUE;
}


/* <https://en.wikipedia.org/wiki/Universal_hashing> */

/* FIXME: This is a copy of the random number generator from
   testlib.c.  It probably needs importing into the MPS properly,
   perhaps in its own module, and the unit tests moving out of
   testlib.c as well. */

static unsigned long tableSeed = 1;
#define R_m 2147483647UL
#define R_a 48271UL

static unsigned long tableRnd(void)
{
  /* requires m == 2^31-1, a < 2^16 */
  unsigned long bot = R_a * (tableSeed & 0x7FFF);
  unsigned long top = R_a * (tableSeed >> 15);
  tableSeed = bot + ((top & 0xFFFF) << 15) + (top >> 16);
  if(tableSeed > R_m)
    tableSeed -= R_m;
  return tableSeed;
  /* Have you modified this code?  Run rnd_verify(3) please!  RHSK */
}

static Word tableRnd64(void)
{
  return ((Word)tableRnd() << 32) | (Word)tableRnd();
}

static void tableHashParamInit(TableHashParam param)
{
  param->multiplicand = tableRnd64() | 1;
  param->addend = tableRnd64();
}

static Word tableHash(TableHashParam param, TableKey key, Shift log2length)
{
  Word product = key * param->multiplicand;
  Word sum = product + param->addend;
  return sum >> (sizeof(Word) * CHAR_BIT - log2length);
}
  

/* tableFind -- finds the entry for this key, or NULL
 *
 * .worst: In the worst case, this looks at every slot before giving up,
 * but that's what you have to do in a closed hash table, to make sure
 * that all the items still fit in after growing the table.
 */

static void tablePos(Index *posReturn, Count *skipReturn, Table table, TableKey key)
{
  *posReturn = tableHash(&table->posHashParamStruct, key, table->log2length);
  *skipReturn = tableHash(&table->skipHashParamStruct, key, table->log2length) | 1;
}

static Index tableStep(Index pos, Count skip, Count length)
{
  return (pos + skip) & (length - 1);
}

static TableEntry tableFind(Table table, TableKey key)
{
  Index i;
  Count length = (Count)1 << table->log2length;
  Index pos;
  Count skip;

  tablePos(&pos, &skip, table, key);
  for (i = 0; i < table->maxChainLength; ++i) {
    if (table->array[pos].key == key)
      return &table->array[pos];
    pos = tableStep(pos, skip, length);
  }
  
  return NULL;
}


#ifdef TABLE_DEBUG
static Bool tableFindBrute(Table table, TableKey key)
{
  Index i;
  for (i = 0; i < TableLength(table); ++i)
    if (table->array[i].key == key)
      return TRUE;
  return FALSE;
}
#endif


/* tablePut -- put a key/value pair into the table
 *
 * Attempts to add a key/value pair to the table.  Returns ResFAIL if
 * the key (or another key) is duplicated in the table, or ResLIMIT if
 * it was not possible to place the key in a chain not longer than
 * maxChainLength hops using the current hash function.
 *
 * Uses a displacing mechanism similar to a cuckoo hash to move
 * entries out of the way, in order to guarantee O(1) lookups.  If
 * insert fails with ResLIMIT, *keyIO and *valueIO contain a displaced
 * key/value pair, which is no longer in the table.
 */

static Res tablePut(Table table, TableKey *keyIO, TableValue *valueIO)
{
  Index j;
  TableKey key = *keyIO;
  TableValue value = *valueIO;
  Count length = TableLength(table);

  for (j = 0; j < length; ++j) { /* detect cycle */
    Index i;
    Index pos, last;
    Count skip;
    TableKey tk;
    TableValue tv;

#ifdef TABLE_DEBUG
    AVER(!tableFindBrute(table, key));
#endif

    tablePos(&pos, &skip, table, key);
    last = pos;
    for (i = 0; i < table->maxChainLength; ++i) {
      tk = table->array[pos].key;
      if (tk == table->unusedKey || tk == table->deletedKey) {
        table->array[pos].key = key;
        table->array[pos].value = value;
        ++table->count;
        return ResOK;
      }
      if (tk == key)
        return ResFAIL;
      last = pos;
      pos = tableStep(pos, skip, length);
    }
    
    /* Chain is full.  Kick out last slot and try to insert it elsewhere. */
    tk = table->array[last].key;
    tv = table->array[last].value;
    AVER(tk != key);
    AVER(tk != table->unusedKey);
    AVER(tk != table->deletedKey);
    table->array[last].key = key;
    table->array[last].value = value;
    key = tk;
    value = tv;
  }
  
  /* Cycle detected.  Give up. */
  *keyIO = key;
  *valueIO = value;
  
  return ResLIMIT;
}


/* tablePutSomewhere -- put the key/value pair somewhere random
 *
 * Adds a key/value pair to a random unoccupied slot in the table.
 * This makes the table invalid, since the key is unlikely to be
 * found.  It is intended for forcing a key/value pair into the table
 * before a rehash.
 */

static void tablePutSomewhere(Table table, TableKey key, TableValue value)
{
  Index i;
  AVER(table->count < TableLength(table));
  for (i = 0; i < TableLength(table); ++i) {
    TableKey tk = table->array[i].key;
    AVER(tk != key);
    if (tk == table->unusedKey || tk == table->deletedKey)
      goto found;
  }
  NOTREACHED;
found:
  table->array[i].key = key;
  table->array[i].value = value;
  ++table->count;
  for (i++; i < TableLength(table); ++i) {
    AVER(table->array[i].key != key);
  }
}


/* tableClear -- set all entries in a table to unused */

static void tableClear(Table table)
{
  Count length = TableLength(table);
  TableEntry array = table->array;
  TableKey unusedKey = table->unusedKey;
  Index i;
  for (i = 0; i < length; ++i)
    array[i].key = unusedKey;
}


/* tableRehashTry -- make one attempt to rehash the table
 *
 * Generate a new random hash function and make a pass over the table
 * rehashing in place.  Returns ResLIMIT if it was not possible to
 * rehash with chains not longer than maxChainLength hops using the
 * hash function.
 */

static Res tableRehashTry(Table table)
{
  Index i;
  Res res;
  Count length = TableLength(table);
  TableEntry array = table->array;

  tableHashParamInit(&table->posHashParamStruct);
  tableHashParamInit(&table->skipHashParamStruct);

  for (i = 0; i < length; ++i) {
    if (array[i].key != table->unusedKey) {
      TableKey key = array[i].key;
      TableValue value = array[i].value;
      array[i].key = table->unusedKey;
      --table->count;
      res = tablePut(table, &key, &value);
      if (res != ResOK) {
	AVER(res != ResFAIL); /* duplicate key */
        tablePutSomewhere(table, key, value);
        return res;
      }
    }
  }
  
  return ResOK;
}


/* tableRehash -- rehash the table making with multiple attempts
 *
 * Make TABLE_REHASH_TRIES attempts to randomly rehash the table.  If
 * that fails, increase the maximum chain length and try again.  This
 * is guaranteed to terminate since the chain length will eventually
 * equal the table length, resulting in a very poor hash table.
 * However, this is extraordinarily unlikely.
 *
 * TODO: Experiment with growing the table instead of extending the
 * chain length, to keep lookup times down.  In fact, that may support
 * a fixed chain length of 2 and a faster cuckoo lookup.
 */

static void tableRehash(Table table)
{
  Index i;

  for (;;) {
    for (i = 0; i < TABLE_REHASH_TRIES; ++i) {
      Res res = tableRehashTry(table);
      if (res == ResOK)
        return;
      AVER(res == ResLIMIT);
    }
    ++table->maxChainLength;

    /* Even in the worst case (a single chain) the maximum chain length
       can't exceed the number of table entries. */
    AVER(table->maxChainLength <= table->count);
  }
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

#define SPACEFRACTION 0.5      /* .hash.spacefraction */

Res TableGrow(Table table, Count extraCapacity)
{
  TableEntry oldArray, newArray;
  Count oldLength, newLength;
  Count required, minimum;
  Count oldCount;

  required = table->count + extraCapacity;
  if (required < table->count)  /* overflow? */
    return ResLIMIT;

  /* Calculate the minimum table length that would allow for the required
     capacity without growing again. */
  minimum = (Count)(required / SPACEFRACTION);
  if (minimum < required)       /* overflow? */
    return ResLIMIT;

  /* Double the table length until it's larger than the minimum */
  oldLength = TableLength(table);
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

  table->log2length = SizeLog2(newLength);
  table->array = newArray;
  oldCount = table->count;

  tableClear(table);
  mps_lib_memcpy(newArray, oldArray, sizeof(TableEntryStruct) * oldLength);
  tableRehash(table);
  AVER(table->count == oldCount);

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
                       TableKey unusedKey,
                       TableKey deletedKey)
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

  table->log2length = SizeLog2(length);
  table->count = 0;
  table->array = NULL;
  table->alloc = tableAlloc;
  table->free = tableFree;
  table->allocClosure = allocClosure;
  table->unusedKey = unusedKey;
  table->deletedKey = deletedKey;
  table->maxChainLength = 2;
  
  table->array = tableAlloc(allocClosure, sizeof(TableEntryStruct) * length);
  if (table->array == NULL) {
    res = ResMEMORY;
    goto failArrayAlloc;
  }
  
  tableHashParamInit(&table->posHashParamStruct);
  tableHashParamInit(&table->skipHashParamStruct);

  table->sig = TableSig;

  tableClear(table);
  
  AVERT(Table, table);

  *tableReturn = table;
  return ResOK;

failArrayAlloc:
  tableFree(allocClosure, table, sizeof(TableEntryStruct) * length);
  return res;
}


/* TableDestroy -- destroy a table */

extern void TableDestroy(Table table)
{
  AVER(table != NULL);
  table->free(table->allocClosure,
              table->array,
              sizeof(TableEntryStruct) * TableLength(table));
  table->sig = SigInvalid;
  table->free(table->allocClosure, table, sizeof(TableStruct));
}


/* TableLookup -- look up */

extern Bool TableLookup(TableValue *valueReturn, Table table, TableKey key)
{
  TableEntry entry;

  AVERT(Table, table);
  AVER(key != table->unusedKey);
  AVER(key != table->deletedKey);
  
  entry = tableFind(table, key);
  if (entry == NULL)
    return FALSE;

  *valueReturn = entry->value;
  return TRUE;
}


/* TableDefine -- add a new mapping */

extern Res TableDefine(Table table, TableKey key, TableValue value)
{
  Res res;
  TableKey origKey = key;
  TableValue origValue = value;

  AVERT(Table, table);
  AVER(key != table->unusedKey);
  AVER(key != table->deletedKey);
  /* value is arbitrary */

  res = tablePut(table, &key, &value);
  if (res == ResLIMIT) {
    res = TableGrow(table, 1);
    if (res != ResOK)
      return res;
    res = tablePut(table, &key, &value);
    if (res == ResLIMIT) { /* doesn't fit with current hash */
      tablePutSomewhere(table, key, value);
      tableRehash(table);
      res = ResOK;
    }
  }
  
  AVER(res == ResOK || key == origKey);
  AVER(res == ResOK || value == origValue);

  return res;
}


/* TableRedefine -- redefine an existing mapping */

extern Res TableRedefine(Table table, TableKey key, TableValue value)
{
  TableEntry entry;

  AVERT(Table, table);
  AVER(key != table->unusedKey);
  AVER(key != table->deletedKey);
  /* value is arbitrary */

  entry = tableFind(table, key);
  if (entry == NULL)
    return ResFAIL;
  AVER(entry->key == key);
  entry->value = value;

  return ResOK;
}


/* TableRemove -- remove a mapping */

extern Res TableRemove(Table table, TableKey key)
{
  TableEntry entry;

  AVERT(Table, table);
  AVER(key != table->unusedKey);
  AVER(key != table->deletedKey);

  entry = tableFind(table, key);
  if (entry == NULL)
    return ResFAIL;
  entry->key = table->deletedKey;
  --table->count;

  return ResOK;
}


/* TableMap -- apply a function to all the mappings */

extern void TableMap(Table table, TableVisitor visit, void *closure)
{
  Index i;
  for (i = 0; i < TableLength(table); i++) {
    TableKey tk = table->array[i].key;
    if (tk != table->unusedKey && tk != table->deletedKey)
      visit(closure, tk, table->array[i].value);
  }
}


/* TableCount -- count the number of mappings in the table */

extern Count TableCount(Table table)
{
  return table->count;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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
