/* impl.h.table: Interface for a dictionary
 * Copyright (C) 1997, 1999 Harlequin Group plc.  All rights reserved.
 *
 * $HopeName$
 */

#ifndef table_h
#define table_h

#include "mpmtypes.h"
#include <stddef.h>


typedef struct TableStruct *Table;

extern Res TableCreate(Table *tableReturn, size_t length);
extern void TableDestroy(Table table);
extern Res TableDefine(Table table, Word key, void *value);
extern Res TableRedefine(Table table, Word key, void *value);
extern void *TableLookup(Table table, Word key);
extern void *TableSlowLookup(Table table, Word key);
extern Res TableRemove(Table table, Word key);
extern size_t TableCount(Table table);
extern void TableApply(Table table, void(*fun)(Word key, void *value));


#endif /* table_h */
