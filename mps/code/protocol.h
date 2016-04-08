/* protocol.h: PROTOCOL INHERITANCE DEFINITIONS
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * See design.mps.protocol.
 */

#ifndef protocol_h
#define protocol_h

#include "config.h"
#include "mpmtypes.h"
#include "classdef.h"


/* CLASS_* -- identifier derivation macros.
 *
 * These turn the base identifier of a class (e.g. "Inst") into other
 * identifiers (e.g. "InstClassStruct").  These are not intended to be
 * used outside of this file.
 */

#define INST_TYPE(ident) ident
#define INST_STRUCT(ident) ident ## Struct
#define CLASS_TYPE(ident) ident ## Class
#define CLASS_STRUCT(ident) ident ## ClassStruct
#define CLASS_ENSURE(ident) ident ## ClassGet
#define CLASS_INIT(ident) ident ## ClassInit
#define CLASS_CHECK(ident) ident ## ClassCheck
#define CLASS_SUPER(ident) ident ## SuperClassGet


/* DECLARE_CLASS -- declare the existence of a protocol class */

#define DECLARE_CLASS(kind, ident) \
  extern CLASS_TYPE(kind) CLASS_ENSURE(ident)(void); \
  extern void CLASS_INIT(ident)(CLASS_TYPE(kind) var)


/* DEFINE_CLASS -- define a protocol class */

#define DEFINE_CLASS(kind, ident, var) \
  DECLARE_CLASS(kind, ident); \
  void CLASS_INIT(ident)(CLASS_TYPE(kind)); \
  CLASS_TYPE(kind) CLASS_ENSURE(ident)(void) \
  { \
    static guardian = FALSE; \
    static CLASS_STRUCT(kind) classStruct; \
    if (guardian == FALSE) { \
      LockClaimGlobalRecursive(); \
      if (guardian == FALSE) { \
        CLASS_INIT(ident)(&classStruct); \
        AVER(CLASS_CHECK(kind)); \
        guardian = TRUE; \
      } \
      LockReleaseGlobalRecursive(); \
    } \
    return &classStruct; \
  } \
  void CLASS_INIT(ident)(CLASS_TYPE(kind) var)


/* CLASS -- expression for getting a class */

#define CLASS(ident) (CLASS_ENSURE(ident)())


/* ClassIdEnum -- unique identifier for each class
 *
 * This defines enum constants like ClassIdLand with a unique small
 * number for each class -- essentially the row number in the class
 * table.
 */

#define CLASS_ID_ENUM(prefix, ident, kind, super) prefix ## ident,
typedef enum ClassIdEnum {
  ClassIdInvalid, /* index zero reserved for invalid classes */
  CLASSES(CLASS_ID_ENUM, ClassId)
  ClassIdLIMIT
} ClassIdEnum;

/* ClassLevelEnum -- depth of class in hierarchy */

#define CLASS_LEVEL_ENUM(prefix, ident, kind, super) prefix ## ident = prefix ## super + 1,
typedef enum ClassLevelEnum {
  ClassLevelNoSuper = 0, /* because everything secretly inherits from Inst */
  CLASSES(CLASS_LEVEL_ENUM, ClassLevel)
  ClassLevelTerminalCommaNotAllowedInC89
} ClassLevelEnum;


/* INHERIT_CLASS -- the standard macro for inheriting from a superclass */

#define INHERIT_CLASS(this, _class, super) \
  BEGIN \
    InstClass instClass = (InstClass)(this); \
    CLASS_INIT(super)(this); \
    instClass->superclass = (InstClass)CLASS(super); \
    instClass->name = #_class; \
    instClass->level = instClass->superclass->level + 1; \
    AVER(instClass->level < ClassDEPTH); \
    instClass->display[instClass->level] = ClassId ## _class; \
  END


#define InstClassSig ((Sig)0x519B60C7) /* SIGnature PROtocol CLass */
#define InstSig  ((Sig)0x519B6014) /* SIGnature PROtocol INst */


/* Inst -- the instance structure for support of the protocol */

typedef struct InstStruct *Inst;
typedef struct InstClassStruct *InstClass;

typedef struct InstStruct {
  InstClass class;
} InstStruct;


/* InstClass -- the class containing the support for the protocol */

typedef const char *InstClassName;
typedef unsigned long ProtocolTypeId;
typedef unsigned char ClassId;
typedef unsigned char ClassLevel;
#define ClassDEPTH 8            /* maximum depth of class hierarchy */

typedef struct InstClassStruct {
  Sig sig;                      /* <design/sig/> */
  InstClassName name;           /* human readable name such as "Land" */
  InstClass superclass;         /* pointer to direct superclass */
  ClassLevel level;             /* distance from root of class hierarchy */
  ClassId display[ClassDEPTH];  /* ids of classes at this level and above */
} InstClassStruct;


/* InstClass -- the root of the protocol class hierarchy */

DECLARE_CLASS(Inst, Inst);


/* Checking functions */

extern Bool InstClassCheck(InstClass class);
extern Bool InstCheck(Inst pro);


/* Protocol introspection interface */

/* The following are macros because of the need to cast */
/* subtypes of InstClass. Nevertheless they are named */
/* as functions. See <design/protocol/#introspect.c-lang> */


#define InstClassSuperclassPoly(class) \
  (((InstClass)(class))->superclass)

#define ClassOfPoly(inst) (MustBeA(Inst, inst)->class)


/* SUPERCLASS  - get the superclass object, given a class name
 *
 * Returns the superclass, with type InstClass. Clients will
 * probably wish to cast this. See
 * <design/protocol/#int.static-superclass>
 */

#define CLASS_DECLARE_SUPER(UNUSED, ident, kind, super) \
  CLASS_TYPE(kind) CLASS_SUPER(ident)(void);
CLASSES(CLASS_DECLARE_SUPER, UNUSED)

#define SUPERCLASS(className) (CLASS_SUPER(className)())


/* IsA, CouldBeA, MustBeA -- coerce instances safely
 *
 * FIXME: Enumerate TypeIds to avoid call to ensure method and
 * subclass test.
 *
 * FIXME: Wrap mps_lib_assert_fail_expr in check.h so that it is
 * elided from some varieties.
 */

#define CouldBeA(class, inst) ((INST_TYPE(class))inst)

#define IsSubclass(sub, super) \
  (((InstClass)(sub))->display[ClassLevel ## super] == ClassId ## super)
  
#define IsA(_class, inst) \
  IsSubclass(CouldBeA(Inst, inst)->class, _class)

#define MustBeA(_class, inst) \
  CouldBeA(_class, \
           (inst) != NULL && \
           CouldBeA(Inst, inst)->class != NULL && \
           IsA(_class, inst) ? \
           inst : \
           mps_lib_assert_fail_expr(MPS_FILE, __LINE__, \
                                    "MustBeA " #_class ": " #inst, \
                                    inst))


#endif /* protocol_h */


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
