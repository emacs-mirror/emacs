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


/* Name derivation macros. These are not intended to be used */
/* outside of this file */

#define DERIVE_LOCAL(name) protocol ## name
#define DERIVE_STRUCT(name) name ## Struct
#define DERIVE_ENSURE(name) name ## Get
#define DERIVE_ENSURE_INTERNAL(name) protocolGet ## name
#define DERIVE_GUARDIAN(name) protocol ## name ## Guardian
#define DERIVE_STATIC_STORAGE(name) protocol ## name ## Struct


/* Macro to set the superclass field. This is not intended */
/* to be used outside this file. This is a polymorphic macro */
/* named as a function. See <design/protocol/#introspect.c-lang> */

#define ProtocolClassSetSuperclassPoly(class, super) \
  (((ProtocolClass)(class))->superclass) = (ProtocolClass)(super)


/* DEFINE_CLASS -- the standard macro for defining a ProtocolClass */

#define DEFINE_CLASS(className, var) \
  static Bool DERIVE_GUARDIAN(className) = FALSE; \
  static DERIVE_STRUCT(className) DERIVE_STATIC_STORAGE(className); \
  static void DERIVE_ENSURE_INTERNAL(className)(className); \
  extern className DERIVE_ENSURE(className)(void); \
  className DERIVE_ENSURE(className)(void) \
  { \
    if (DERIVE_GUARDIAN(className) == FALSE) { \
      LockClaimGlobalRecursive(); \
      if (DERIVE_GUARDIAN(className) == FALSE) { \
        DERIVE_ENSURE_INTERNAL(className) \
          (&DERIVE_STATIC_STORAGE(className)); \
        DERIVE_GUARDIAN(className) = TRUE; \
      } \
      LockReleaseGlobalRecursive(); \
    } \
    return &DERIVE_STATIC_STORAGE(className); \
  } \
  static void DERIVE_ENSURE_INTERNAL(className) (className var)


/* DECLARE_CLASS -- declare the existence of a protocol class */

#define DECLARE_CLASS(classKind, className) \
  extern classKind DERIVE_ENSURE(className)(void)


/* CLASS -- expression for getting a class */

#define CLASS(className) (DERIVE_ENSURE(className)())


/* INHERIT_CLASS -- the standard macro for inheriting from a superclass */

#define INHERIT_CLASS(this, parentName) \
  BEGIN \
    parentName DERIVE_LOCAL(parentName) = DERIVE_ENSURE(parentName)(); \
    *this = *(DERIVE_LOCAL(parentName)); \
    ProtocolClassSetSuperclassPoly(this, DERIVE_LOCAL(parentName)); \
  END


/* DEFINE_ALIAS_CLASS -- define a new class for the same type
 *
 * A convenience macro. Aliases the structure and pointer types
 * for className to be the same as typeName, and then defines
 * the class className.
 */
#define DEFINE_ALIAS_CLASS(className, typeName, var) \
  typedef typeName className; \
  typedef DERIVE_STRUCT(typeName) DERIVE_STRUCT(className); \
  DEFINE_CLASS(className, var)



#define ProtocolClassSig ((Sig)0x519B60C7) /* SIGnature PROtocol CLass */
#define ProtocolInstSig  ((Sig)0x519B6014) /* SIGnature PROtocol INst */


/* ProtocolClass -- the class containing the support for the protocol */

typedef struct ProtocolClassStruct *ProtocolClass;


/* ProtocolInst -- the instance structure for support of the protocol */

typedef struct ProtocolInstStruct *ProtocolInst;


/* ProtocolCoerceInstMethod -- coerce "pro" to an instance of "interface"
 *
 * If "pro" is an instance of "interface", then returns TRUE
 * and sets coerceResult to point directly to the part of "pro"
 * which contains the slots for "interface"
 * RHSK  2006-04-05  s/interface/interfaceIn/: job000605, suspect msvc bug.
 */
typedef Bool (*ProtocolCoerceInstMethod)(ProtocolInst *coerceResult,
                                         ProtocolInst pro,
                                         ProtocolClass interfaceIn);

/* ProtocolCoerceClassMethod -- coerce "proClass" to an "interface" class
 *
 * If "proClass" is a subclass of "interface", then returns TRUE
 * and sets coerceResult to point directly to the part of
 * "proClass" which contains the slots for "interface".
 * RHSK  2006-04-05  s/interface/interfaceIn/: job000605, suspect msvc bug.
 */
typedef Bool (*ProtocolCoerceClassMethod)(ProtocolClass *coerceResult,
                                          ProtocolClass proClass,
                                          ProtocolClass interfaceIn);



typedef struct ProtocolClassStruct {
  Sig sig;                               /* <design/sig/> */
  ProtocolClass superclass;              /* the superclass */
  ProtocolCoerceInstMethod coerceInst;   /* coerce instance to super */
  ProtocolCoerceClassMethod coerceClass; /* coerce class to superclass */
} ProtocolClassStruct;


typedef struct ProtocolInstStruct {
  Sig sig;                      /* <design/sig/> */
  ProtocolClass class;          /* the class  */
} ProtocolInstStruct;


/* ProtocolClassGet -- Returns the root of the protocol class hierarchy
 *
 * Function name conforms to standard conventions for
 * protocols.
 */

DECLARE_CLASS(ProtocolClass, ProtocolClass);


/* Checking functions */

extern Bool ProtocolClassCheck(ProtocolClass class);
extern Bool ProtocolInstCheck(ProtocolInst pro);


/* ProtocolIsSubclass - use macro IsSubclass to access this.
 *
 * A predicate for testing subclass relationships.
 * A protocol class is always a subclass of itself.
 */
extern Bool ProtocolIsSubclass(ProtocolClass sub, ProtocolClass super);


/* Protocol introspection interface */

/* The following are macros because of the need to cast */
/* subtypes of ProtocolClass. Nevertheless they are named */
/* as functions. See <design/protocol/#introspect.c-lang> */


#define ProtocolClassSuperclassPoly(class) \
  (((ProtocolClass)(class))->superclass)

#define ClassOfPoly(inst) ((ProtocolInst)(inst)->class)

#define IsSubclassPoly(sub, super) \
   ProtocolIsSubclass((ProtocolClass)(sub), (ProtocolClass)(super))


/* SUPERCLASS  - get the superclass object, given a class name
 *
 * Returns the superclass, with type ProtocolClass. Clients will
 * probably wish to cast this. See
 * <design/protocol/#int.static-superclass>
 */
#define SUPERCLASS(className)  \
  ProtocolClassSuperclassPoly(DERIVE_ENSURE(className)())


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
