/* table.h: Interface for a dictionary
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * A table is a hashed mapping from keys to values.
 */

#ifndef table_h
#define table_h

#include "mpmtypes.h"
#include <stddef.h>


typedef struct TableStruct *Table;

typedef Word TableKey;
typedef void *TableValue;

typedef struct TableEntryStruct {
  TableKey key;
  TableValue value;
} TableEntryStruct, *TableEntry;

typedef void *(*TableAllocFunction)(void *closure, size_t size);
typedef void (*TableFreeFunction)(void *closure, void *p, size_t size);

#define TableSig        ((Sig)0x5192AB13) /* SIGnature TABLE */

typedef struct TableStruct {
  Sig sig;                      /* <design/sig> */
  Count length;                 /* Number of slots in the array */
  Count count;                  /* Active entries in the table */
  TableEntry array;             /* Array of table slots */
  TableAllocFunction alloc;
  TableFreeFunction free;
  void *allocClosure;
  TableKey unusedKey;           /* key marking unused (undefined) entries */
  TableKey deletedKey;          /* key marking deleted entries */
} TableStruct;

extern Res TableCreate(Table *tableReturn,
                       Count length,
                       TableAllocFunction tableAlloc,
                       TableFreeFunction tableFree,
                       void *allocClosure,
                       TableKey unusedKey,
                       TableKey deletedKey);
extern void TableDestroy(Table table);
extern Bool TableCheck(Table table);
extern Res TableDefine(Table table, TableKey key, TableValue value);
extern Res TableRedefine(Table table, TableKey key, TableValue value);
extern Bool TableLookup(TableValue *valueReturn, Table table, TableKey key);
extern Res TableRemove(Table table, TableKey key);
extern Count TableCount(Table table);
extern void TableMap(Table table,
                     void(*fun)(void *closure, TableKey key, TableValue value),
                     void *closure);
extern Res TableGrow(Table table, Count extraCapacity);


#endif /* table_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <http://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
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
