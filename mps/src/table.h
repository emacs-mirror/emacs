/* impl.h.table: Interface for a dictionary
 * Copyright (C) 1997, 1999 Harlequin Group plc.  All rights reserved.
 *
 * $HopeName: MMsrc!table.h(trunk.1) $
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
extern Bool TableLookup(void **valueReturn, Table table, Word key);
extern Res TableRemove(Table table, Word key);
extern size_t TableCount(Table table);
extern void TableMap(Table table, void(*fun)(Word key, void *value));


#endif /* table_h */
