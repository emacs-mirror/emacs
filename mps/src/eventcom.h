/* impl.h.eventcom -- Event Logging Common Definitions
 *
 * Copyright (C) 1997, 1998, 1999 Harlequin Group plc.  All rights reserved.
 * $HopeName: MMsrc!eventcom.h(trunk.16) $
 *
 * .readership: MPS developers
 * .sources: mps.design.telemetry
 */

#ifndef eventcom_h
#define eventcom_h

/* #include "eventgen.h" later in the file */
#include "mpmtypes.h" /* for Word */


/* Types for event fields */


typedef Word EventType;
typedef size_t EventCode;
typedef Index EventKind;

typedef Byte EventStringLen;

typedef struct {
  EventStringLen len;
  char str[EventStringLengthMAX];
} EventStringStruct;

typedef EventStringStruct *EventString;


/* @@@@ Put these in eventdef? */
#define EventNameMAX ((size_t)19)
#define EventCodeMAX ((EventCode)0x0041)


/* eventgen.h is just the automatically generated part of this file */
#include "eventgen.h"


/* Event types -- see design.mps.telemetry
 *
 * These names are intended to be mnemonic.  They are derived from 
 * selected letters as indicated, using the transliteration in 
 * guide.hex.trans.
 *
 * These definitions will be unnecessary when the event codes are
 * changed to 16-bit.  See impl.h.eventdef.
 */
                                                    /* EVent ... */
#define EventEventTime      ((EventType)0xEF213E99) /* TIME */
#define EventPoolInit       ((EventType)0xEFB07141) /* POoL INIt */
#define EventPoolFinish     ((EventType)0xEFB07F14) /* POoL FINish */
#define EventPoolAlloc      ((EventType)0xEFB07A77) /* POoL ALLoc */
#define EventPoolFree       ((EventType)0xEFB07F6E) /* POoL FREe */
#define EventArenaCreate    ((EventType)0xEFA64C6E) /* AReNa CREate */
#define EventArenaDestroy   ((EventType)0xEFA64DE5) /* AReNa DEStroy */
#define EventSegAlloc       ((EventType)0xEF5E9A77) /* SEG ALLoc */
#define EventSegFree	    ((EventType)0xEF5E9F6E) /* SEG FREe */
#define EventAMCGenCreate   ((EventType)0xEFA3C94C) /* AMC GeN Create */
#define EventAMCGenDestroy  ((EventType)0xEFA3C94D) /* AMC GeN Destroy */
#define EventAMCInit        ((EventType)0xEFA3C141) /* AMC INIt */
#define EventAMCFinish      ((EventType)0xEFA3CF14) /* AMC FINish */
#define EventAMCTraceBegin  ((EventType)0xEFA3C26B) /* AMC TRace Begin */
#define EventAMCCondemn     ((EventType)0xEFA3CC04) /* AMC CONdemn */
#define EventAMCScanBegin   ((EventType)0xEFA3C5CB) /* AMC SCan Begin */
#define EventAMCScanEnd     ((EventType)0xEFA3C5CE) /* AMC SCan End */
#define EventAMCFix         ((EventType)0xEFA3CF18) /* AMC FIX */
#define EventAMCFixInPlace  ((EventType)0xEFA3CF8A) /* AMC FiX Ambig */
#define EventAMCFixForward  ((EventType)0xEFA3CF8F) /* AMC FiX Forward */
#define EventAMCReclaim     ((EventType)0xEFA3C6EC) /* AMC REClaim */
#define EventAMCTraceEnd    ((EventType)0xEFA3C26E) /* AMC TRace End */
#define EventTraceStart     ((EventType)0xEF26AC52) /* TRACe STart */
#define EventTraceCreate    ((EventType)0xEF26ACC6) /* TRACe CReate */
#define EventTraceDestroy   ((EventType)0xEF26ACDE) /* TRACe DEstroy */
#define EventSegSetGrey     ((EventType)0xEF59596A) /* SeG Set GRAy */
#define EventTraceFlipBegin ((EventType)0xEF26AF7B) /* TRAce FLip Begin */
#define EventTraceFlipEnd   ((EventType)0xEF26AF7E) /* TRAce FLip End */
#define EventTraceReclaim   ((EventType)0xEF26A6EC) /* TRAce REClaim */
#define EventTraceScan      ((EventType)0xEF26AC5C) /* TRACe SCan */
#define EventTraceScanSeg   ((EventType)0xEF26A559) /* TRAce ScanSeG */
#define EventTraceScanSingleRef \
                            ((EventType)0xEF26A556) /* TRAce ScanSingleRef */
#define EventTraceAccess    ((EventType)0xEF26AACC) /* TRAce ACCess */
#define EventTracePoll      ((EventType)0xEF26AB01) /* TRAce POLl */
#define EventTraceStep      ((EventType)0xEF26A52B) /* TRAce STeP */
#define EventTraceFix       ((EventType)0xEF26AF18) /* TRAce FIX */
#define EventTraceFixSeg    ((EventType)0xEF26AF85) /* TRAce FiX Seg */
#define EventTraceFixWhite  ((EventType)0xEF26AF83) /* TRAce FiX White */
#define EventTraceScanArea  ((EventType)0xEF26A5CA) /* TRAce SCan Area */
#define EventTraceScanAreaTagged ((EventType)0xEF26A5C2) /* TRAce SCan area Tagged */
#define EventVMCreate       ((EventType)0xEFF3C6EA) /* VM CREAte */
#define EventVMDestroy      ((EventType)0xEFF3DE52) /* VM DESTroy */
#define EventVMMap          ((EventType)0xEFF33AB9) /* VM MAP */
#define EventVMUnmap        ((EventType)0xEFF3043B) /* VM UNMaP */
#define EventIntern         ((EventType)0xEF142E64) /* INTERN */
#define EventArenaExtend    ((EventType)0xEFA64E82) /* AReNa EXTend */
#define EventArenaRetract   ((EventType)0xEFA646E2) /* AReNa RETract */
#define EventRootScan       ((EventType)0xEF625CA4) /* RooT SCAN */
#define EventLabel          ((EventType)0xEF7ABE79) /* LABEL */
#define EventTraceSegGreyen ((EventType)0xEF26A599) /* TRAce SeG Greyen */
#define EventBufferReserve  ((EventType)0xEFB0FF6E) /* BUFFer REserve */
#define EventBufferCommit   ((EventType)0xEFB0FFC0) /* BUFFer COmmit */
#define EventBufferInit     ((EventType)0xEFB0FF14) /* BUFFer INit */
#define EventBufferFinish   ((EventType)0xEFB0FFF1) /* BUFFer FInish */
#define EventMV2Finish      ((EventType)0xEF3F2F14) /* MV2 FINish */
#define EventBufferFill     ((EventType)0xEFB0FFF7) /* BUFFer FilL */
#define EventBufferEmpty    ((EventType)0xEFB0FFE3) /* BUFFer EMpty */
#define EventSegAllocFail   ((EventType)0xEF5E9A7F) /* SEG ALloc Fail */
#define EventMeterInit      ((EventType)0xEF3E2141) /* METer INIt */
#define EventMeterValues    ((EventType)0xEF3E2FA7) /* METer VALues */
#define EventCBSInit        ((EventType)0xEFCB5141) /* CBS INIt */
#define EventTraceStatCondemn ((EventType)0xEF26A5C0) /* TRAce Stat COndemn */
#define EventTraceStatScan  ((EventType)0xEF26A55C) /* TRAce Stat SCan */
#define EventTraceStatFix   ((EventType)0xEF26A5F8) /* TRAce Stat FiX */
#define EventTraceStatReclaim ((EventType)0xEF26A56E) /* TRAce Stat REclaim */
#define EventArenaWriteFaults ((EventType)0xEFA6436F) /* AReNa WRite Faults */


#endif /* eventcom_h */
