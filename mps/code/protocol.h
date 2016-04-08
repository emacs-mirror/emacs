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
#define CLASS_GUARDIAN(ident) ident ## ClassGuardian
#define CLASS_STATIC(ident) static ## ident ## ClassStruct


/* DECLARE_CLASS -- declare the existence of a protocol class */

#define DECLARE_CLASS(kind, ident) \
  extern CLASS_TYPE(kind) CLASS_ENSURE(ident)(void); \
  extern void CLASS_INIT(ident)(CLASS_TYPE(kind) var)


/* DEFINE_CLASS -- the standard macro for defining a InstClass */

#define DEFINE_CLASS(ident, var) \
  DECLARE_CLASS(ident, ident); \
  void CLASS_INIT(ident)(CLASS_TYPE(ident)); \
  CLASS_TYPE(ident) CLASS_ENSURE(ident)(void)     \
  { \
    static Bool CLASS_GUARDIAN(ident) = FALSE; \
    static CLASS_STRUCT(ident) CLASS_STATIC(ident); \
    if (CLASS_GUARDIAN(ident) == FALSE) { \
      LockClaimGlobalRecursive(); \
      if (CLASS_GUARDIAN(ident) == FALSE) { \
        CLASS_INIT(ident) \
          (&CLASS_STATIC(ident)); \
        CLASS_GUARDIAN(ident) = TRUE; \
      } \
      LockReleaseGlobalRecursive(); \
    } \
    return &CLASS_STATIC(ident); \
  } \
  void CLASS_INIT(ident)(CLASS_TYPE(ident) var)


/* CLASS -- expression for getting a class */

#define CLASS(ident) (CLASS_ENSURE(ident)())


/* INHERIT_CLASS -- the standard macro for inheriting from a superclass */

extern unsigned ProtocolPrime[1000];

#define CLASS_INDEX_ENUM(prefix, ident, kind, super) prefix ## ident,
typedef enum ProtocolClassIndexEnum {
  ProtocolClassIndexInvalid, /* index zero (prime 2) reserved for invalid classes */
  CLASSES(CLASS_INDEX_ENUM, ProtocolClassIndex)
  ProtocolClassIndexLIMIT
} ProtocolClassIndexEnum;

#define INHERIT_CLASS(this, _class, super) \
  BEGIN \
    InstClass protocolClass = (InstClass)(this); \
    CLASS_INIT(super)(this); \
    protocolClass->superclass = (InstClass)CLASS(super); \
    protocolClass->name = #_class; \
    protocolClass->typeId = \
      ProtocolPrime[ProtocolClassIndex ## _class] * \
      protocolClass->superclass->typeId; \
  END


/* DEFINE_ALIAS_CLASS -- define a new class for the same type
 *
 * A convenience macro. Aliases the structure and pointer types
 * for className to be the same as typeName, and then defines
 * the class className.
 */
#define DEFINE_ALIAS_CLASS(ident, other, var) \
  typedef CLASS_TYPE(other) CLASS_TYPE(ident); \
  typedef CLASS_STRUCT(other) CLASS_STRUCT(ident); \
  DEFINE_CLASS(ident, var)


#define InstClassSig ((Sig)0x519B60C7) /* SIGnature PROtocol CLass */
#define InstSig  ((Sig)0x519B6014) /* SIGnature PROtocol INst */


/* Inst -- the instance structure for support of the protocol */

typedef struct InstStruct *Inst;
typedef struct InstClassStruct *InstClass;

typedef struct InstStruct {
  Sig sig;                      /* <design/sig/> */
  InstClass class;
} InstStruct;


/* InstClass -- the class containing the support for the protocol */

typedef const char *InstClassName;
typedef unsigned long ProtocolTypeId;

typedef struct InstClassStruct {
  Sig sig;                      /* <design/sig/> */
  InstClassName name;
  InstClass superclass;
  ProtocolTypeId typeId;
} InstClassStruct;


/* InstClass -- the root of the protocol class hierarchy */

DECLARE_CLASS(Inst, Inst);


/* Checking functions */

extern Bool InstClassCheck(InstClass class);
extern Bool InstCheck(Inst pro);


/* ProtocolIsSubclass - use macro IsSubclass to access this.
 *
 * A predicate for testing subclass relationships.  A protocol class
 * is always a subclass of itself.
 */

extern Bool ProtocolIsSubclass(InstClass sub, InstClass super);


/* Protocol introspection interface */

/* The following are macros because of the need to cast */
/* subtypes of InstClass. Nevertheless they are named */
/* as functions. See <design/protocol/#introspect.c-lang> */


#define InstClassSuperclassPoly(class) \
  (((InstClass)(class))->superclass)

#define ClassOfPoly(inst) ((Inst)(inst)->class)

#define IsSubclassPoly(sub, super) \
   ProtocolIsSubclass((InstClass)(sub), (InstClass)(super))


/* SUPERCLASS  - get the superclass object, given a class name
 *
 * Returns the superclass, with type InstClass. Clients will
 * probably wish to cast this. See
 * <design/protocol/#int.static-superclass>
 */
#define SUPERCLASS(className)  \
  InstClassSuperclassPoly(CLASS_ENSURE(className)())


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
