/* impl.h.table: A dictionary mapping a Word to a void*
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .note.good-hash: As is common in hash table implementations, we
 * assume that the hash function is good.
 */

#include "table.h"
#include "mpmtypes.h"

#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


typedef unsigned long ulong;


#define tableUNUSED	((Word)0x2AB7E040)
#define tableDELETED   ((Word)0x2AB7EDE7)
#define tableACTIVE    ((Word)0x2AB7EAC2)


typedef struct TableEntryStruct *TableEntry;
typedef struct TableEntryStruct {
  Word status;
  Word key;
  void *value;
} TableEntryStruct;


typedef struct TableStruct {
  size_t length;
  size_t count;
  size_t limit;
  TableEntry array;
} TableStruct;



/* sizeFloorLog2 -- logarithm base 2 */

static size_t sizeFloorLog2(size_t size)
{
  size_t l = 0;

  assert(size != 0);
  while(size > 1) {
    ++l;
    size >>= 1;
  }
  return l;
}


/* TableHash -- table hashing function */

static ulong TableHash(Word key)
{
  /* Shift some randomness into the low bits. */
  return (key >> 10) + key;
}


/* TableFind -- finds the entry for this key, or NULL
 *
 * .worst: In the worst case, this looks at every slot before giving up,
 * but that's what you have to do in a closed hash table, to make sure
 * that all the items still fit in after growing the table.
 */

static TableEntry TableFind(Table table, Word key, int skip_deleted)
{
  ulong hash;
  size_t i, mask = table->length - 1;
 
  hash = TableHash(key) & mask;
  i = hash;
  do {
    switch (table->array[i].status) {
    case tableACTIVE:
      if (table->array[i].key == key)
	return &table->array[i];
      break;
    case tableDELETED:
      if (!skip_deleted)
	return &table->array[i];
      break;
    case tableUNUSED:
      return &table->array[i];
      break;
    }
    i = (i + 1) & mask;
  } while(i != hash);

  return NULL;
}


/* TableGrow -- doubles the size of the table */

static Res TableGrow(Table table)
{
  TableEntry oldArray, newArray;
  size_t i, oldLength, newLength;

  oldLength = table->length;
  oldArray = table->array;
  newLength = oldLength * 2;
  newArray = malloc(sizeof(TableEntryStruct) * newLength);
  if(newArray == NULL) return ResMEMORY;

  for(i = 0; i < newLength; ++i) {
    newArray[i].key = 0;
    newArray[i].value = NULL;
    newArray[i].status = tableUNUSED;
  }
 
  table->length = newLength;
  table->array = newArray;
  table->limit *= 2;

  for(i = 0; i < oldLength; ++i) {
    if (oldArray[i].status == tableACTIVE) {
      TableEntry entry;
      entry = TableFind(table, oldArray[i].key, 0 /* none deleted */);
      assert(entry != NULL);
      assert(entry->status == tableUNUSED);
      entry->key = oldArray[i].key;
      entry->value = oldArray[i].value;
      entry->status = tableACTIVE;
    }
  }
  free(oldArray);

  return ResOK;
}


/* TableCreate -- makes a new table */

extern Res TableCreate(Table *tableReturn, size_t length)
{
  Table table;
  size_t i;

  assert(tableReturn != NULL);

  table = malloc(sizeof(TableStruct));
  if(table == NULL) goto failMallocTable;
  if (length < 2) length = 2;
  /* Table size is length rounded up to the next power of 2. */
  table->length = 1 << (sizeFloorLog2(length-1) + 1);
  table->count = 0;
  table->limit = (size_t)(.5 * length);
  table->array = malloc(sizeof(TableEntryStruct) * length);
  if(table->array == NULL) goto failMallocArray;
  for(i = 0; i < length; ++i) {
    table->array[i].key = 0;
    table->array[i].value = NULL;
    table->array[i].status = tableUNUSED;
  }
 
  *tableReturn = table;
  return ResOK;

failMallocArray:
  free(table);
failMallocTable:
  return ResMEMORY;
}


/* TableDestroy -- destroy a table */

extern void TableDestroy(Table table)
{
  assert(table != NULL);
  free(table->array);
  free(table);
}


/* TableLookup -- look up */

extern Bool TableLookup(void **valueReturn, Table table, Word key)
{
  TableEntry entry = TableFind(table, key, 1 /* skip deleted */);

  if(entry == NULL || entry->status != tableACTIVE)
    return FALSE;
  *valueReturn = entry->value;
  return TRUE;
}


/* TableDefine -- add a new mapping */

extern Res TableDefine(Table table, Word key, void *value)
{
  TableEntry entry;

  if (table->count >= table->limit) {
    Res res = TableGrow(table);
    if(res != ResOK) return res;
    entry = TableFind(table, key, 0 /* no deletions yet */);
    assert(entry != NULL);
    if (entry->status == tableACTIVE)
      return ResFAIL;
  } else {
    entry = TableFind(table, key, 1 /* skip deleted */);
    if (entry != NULL && entry->status == tableACTIVE)
      return ResFAIL;
    /* Search again to find the best slot, deletions included. */
    entry = TableFind(table, key, 0 /* don't skip deleted */);
    assert(entry != NULL);
  }

  entry->status = tableACTIVE;
  entry->key = key;
  entry->value = value;
  ++table->count;

  return ResOK;
}


/* TableRedefine -- redefine an existing mapping */

extern Res TableRedefine(Table table, Word key, void *value)
{
  TableEntry entry = TableFind(table, key, 1 /* skip deletions */);
 
  if (entry == NULL || entry->status != tableACTIVE)
    return ResFAIL;
  assert(entry->key == key);
  entry->value = value;
  return ResOK;
}


/* TableRemove -- remove a mapping */

extern Res TableRemove(Table table, Word key)
{
  TableEntry entry = TableFind(table, key, 1);

  if (entry == NULL || entry->status != tableACTIVE)
    return ResFAIL;
  entry->status = tableDELETED;
  --table->count;
  return ResOK;
}


/* TableMap -- apply a function to all the mappings */

extern void TableMap(Table table, void(*fun)(Word key, void*value))
{
  size_t i;
  for (i = 0; i < table->length; i++)
    if (table->array[i].status == tableACTIVE)
      (*fun)(table->array[i].key, table->array[i].value);
}


/* TableCount -- count the number of mappings in the table */

extern size_t TableCount(Table table)
{
  return table->count;
}


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
