/* impl.h.protocol: PROTOCOL INHERITANCE DEFINITIONS
 *
 * $HopeName: MMsrc!protocol.h(trunk.4) $
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
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
#define DERIVE_ENSURE_OLD(name) Ensure ## name
#define DERIVE_ENSURE_INTERNAL(name) protocolGet ## name
#define DERIVE_GUARDIAN(name) protocol ## name ## Guardian
#define DERIVE_STATIC_STORAGE(name) protocol ## name ## Struct


/* Macro to set the superclass field. This is not intended */
/* to be used outside this file. This is a polymorphic macro */
/* named as a function. See design.mps.protocol.introspect.c-lang */

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
  /* old name for backward compatibility */ \
  extern className DERIVE_ENSURE_OLD(className)(void); \
  className DERIVE_ENSURE_OLD(className)(void) \
  { \
    return DERIVE_ENSURE(className)(); \
  } \
  static void DERIVE_ENSURE_INTERNAL(className) (className var)


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
 */
typedef Bool (*ProtocolCoerceInstMethod)(ProtocolInst *coerceResult,
                                         ProtocolInst pro, 
                                         ProtocolClass interface);

/* ProtocolCoerceClassMethod -- coerce "proClass" to an "interface" class
 *
 * If "proClass" is a subclass of "interface", then returns TRUE
 * and sets coerceResult to point directly to the part of 
 * "proClass" which contains the slots for "interface".
 */
typedef Bool (*ProtocolCoerceClassMethod)(ProtocolClass *coerceResult,
                                          ProtocolClass proClass, 
                                          ProtocolClass interface);



typedef struct ProtocolClassStruct {
  Sig sig;                               /* design.mps.sig */
  ProtocolClass superclass;              /* the superclass */
  ProtocolCoerceInstMethod coerceInst;   /* coerce instance to super */
  ProtocolCoerceClassMethod coerceClass; /* coerce class to superclass */
} ProtocolClassStruct;


typedef struct ProtocolInstStruct {
  Sig sig;                      /* design.mps.sig */
  ProtocolClass class;          /* the class  */
} ProtocolInstStruct;


/* ProtocolClassGet -- Returns the root of the protocol class hierarchy
 *
 * Function name conforms to standard conventions for 
 * protocols.
 */
extern ProtocolClass ProtocolClassGet(void);


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
/* as functions. See design.mps.protocol.introspect.c-lang */


#define ProtocolClassSuperclassPoly(class) \
  (((ProtocolClass)(class))->superclass)

#define ClassOfPoly(inst) ((ProtocolInst)(inst)->class)

#define IsSubclassPoly(sub, super) \
   ProtocolIsSubclass((ProtocolClass)(sub), (ProtocolClass)(super))


/* SUPERCLASS  - get the superclass object, given a class name
 *
 * Returns the superclass, with type ProtocolClass. Clients will
 * probably wish to cast this. See 
 * design.mps.protocol.int.static-superclass
 */
#define SUPERCLASS(className)  \
  ProtocolClassSuperclassPoly(DERIVE_ENSURE(className)())


#endif /* protocol_h */
