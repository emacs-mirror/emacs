/* impl.h.table: A dictionary mapping a Word to a void*
 * Copyright (C) 1997, 1999 Harlequin Group plc.  All rights reserved.
 *
 * $HopeName: MMsrc!table.c(trunk.1) $
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


#define TABLE_UNUSED	((Word)0x2AB7E040)
#define TABLE_DELETED   ((Word)0x2AB7EDE7)
#define TABLE_ACTIVE    ((Word)0x2AB7EAC2)


typedef struct TableEntryStruct *TableEntry;
typedef struct TableEntryStruct {
  Word status;
  Word key;
  void *value;
} TableEntryStruct;


typedef struct TableStruct {
  size_t length;
  size_t count;
  TableEntry array;
} TableStruct;


static ulong TableHash(Word key)
{
  return key;
}


/* TableFind -- finds the entry for this key, or NULL */

static TableEntry TableFind(Table table, Word key, int skip_deleted)
{
  ulong hash;
  size_t i;
  
  hash = TableHash(key) & (table->length - 1);
  i = hash;
  do {
    switch (table->array[i].status) {
    case TABLE_ACTIVE:
      if (table->array[i].key == key)
	return &table->array[i];
      break;
    case TABLE_DELETED:
      if (!skip_deleted)
	return &table->array[i];
      break;
    case TABLE_UNUSED:
      return &table->array[i];
      break;
    default:
      assert(0);
    }
    i = (i + (hash | 1)) & (table->length - 1);
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
  newLength = table->length * 2;
  newArray = malloc(sizeof(TableEntryStruct) * newLength);
  if(newArray == NULL) return ResMEMORY;

  for(i = 0; i < newLength; ++i) {
    newArray[i].key = 0;
    newArray[i].value = NULL;
    newArray[i].status = TABLE_UNUSED;
  }
  
  table->length = newLength;
  table->array = newArray;

  for(i = 0; i < oldLength; ++i) {
    TableEntry entry;
    assert(oldArray[i].status == TABLE_ACTIVE); /* should be full */
    entry = TableFind(table, oldArray[i].key, 0 /* none deleted */);
    assert(entry->status == TABLE_UNUSED); /* shouldn't be defined yet */
    entry->key = oldArray[i].key;
    entry->value = oldArray[i].value;
    entry->status = TABLE_ACTIVE;
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
  table->length = length; table->count = 0;
  table->array = malloc(sizeof(TableEntryStruct) * length);
  if(table->array == NULL) goto failMallocArray;
  for(i = 0; i < length; ++i) {
    table->array[i].key = 0;
    table->array[i].value = NULL;
    table->array[i].status = TABLE_UNUSED;
  }
  
  *tableReturn = table;
  return ResOK;

failMallocArray:
  free(table);
failMallocTable:
  return ResMEMORY;
}


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

  if(entry == NULL || entry->status != TABLE_ACTIVE)
    return FALSE;
  *valueReturn = entry->value;
  return TRUE;
}


/* TableDefine -- add a new mapping */

extern Res TableDefine(Table table, Word key, void *value)
{
  TableEntry entry = TableFind(table, key, 1 /* skip deleted */);

  if (entry != NULL && entry->status == TABLE_ACTIVE)
    return ResFAIL;

  if (entry == NULL) {
    Res res;
    entry = TableFind(table, key, 0 /* do not skip deletions */);
    if (entry == NULL) {
      /* table is full.  Must grow the table to make room. */
      res = TableGrow(table);
      if(res != ResOK) return res;
      entry = TableFind(table, key, 0 /* do not skip deletions */);
    }
  }
  assert(entry != NULL && entry->status != TABLE_ACTIVE);

  entry->status = TABLE_ACTIVE;
  entry->key = key;
  entry->value = value;
  ++table->count;

  return ResOK;
}


/* TableRedefine -- redefine an existing mapping */

extern Res TableRedefine(Table table, Word key, void *value)
{
  TableEntry entry = TableFind(table, key, 1 /* skip deletions */);
  
  if (entry == NULL || entry->status != TABLE_ACTIVE)
    return ResFAIL;
  assert(entry->key == key);
  entry->value = value;
  return ResOK;
}


/* TableRemove -- remove a mapping */

extern Res TableRemove(Table table, Word key)
{
  TableEntry entry = TableFind(table, key, 1);

  if (entry == NULL || entry->status != TABLE_ACTIVE)
    return ResFAIL;
  entry->status = TABLE_DELETED;
  --table->count;
  return ResOK;
}


/* TableMap -- apply a function to all the mappings */

extern void TableMap(Table table, void(*fun)(Word key, void*value))
{
  size_t i;
  for (i = 0; i < table->length; i++)
    if (table->array[i].status == TABLE_ACTIVE)
      (*fun)(table->array[i].key, table->array[i].value);
}


/* TableCount -- count the number of mappings in the table */

extern size_t TableCount(Table table)
{
  return table->count;
}
