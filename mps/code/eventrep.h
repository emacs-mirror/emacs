/* impl.h.eventrep: Allocation replayer interface
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * $Id$
 */

#ifndef eventrep_h
#define eventrep_h

#include "config.h"
/* override variety setting for EVENT */
#define EVENT

#include "eventcom.h"
#include "mpmtypes.h"


extern Res EventRepInit(Bool partial);
extern void EventRepFinish(void);

extern void EventReplay(Event event, Word etime);


#endif /* eventrep_h */
