/* impl.h.protocol: PROTOCOL INHERITANCE DEFINITIONS
 *
 * $HopeName: MMsrc!protocol.h(MMdevel_tony_inheritance.4) $
 * Copyright (C) 1998.  Harlequin Group plc.  All rights reserved.
 */

#ifndef protocol_h
#define protocol_h

#include "config.h"
#include "mpmtypes.h"


/* Name derivation macros. These are not intended to be used */
/* outside of this file */

#define DERIVE_LOCAL(name) protocol ## name
#define DERIVE_STRUCT(name) name ## Struct
#define DERIVE_ENSURE(name) Ensure ## name 
#define DERIVE_ENSURE_INTERNAL(name) protocolEnsure ## name
#define DERIVE_GUARDIAN(name) protocol ## name ## Guardian
#define DERIVE_STATIC_STORAGE(name) protocol ## name ## Struct


/* DEFINE_CLASS
 *
 * The standard macro for defining a ProtocolClass.
 */

#define DEFINE_CLASS(className, var) \
  static Bool DERIVE_GUARDIAN(className) = FALSE; \
  static DERIVE_STRUCT(className) DERIVE_STATIC_STORAGE(className); \
  static void DERIVE_ENSURE_INTERNAL(className) (className); \
  extern className DERIVE_ENSURE(className) (void); \
  extern className DERIVE_ENSURE(className) (void) \
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


/* INHERIT_CLASS
 *
 * The standard macro for inheriting from a superclass.
 */

#define INHERIT_CLASS(this, parentName) \
  BEGIN \
    parentName DERIVE_LOCAL(parentName) = DERIVE_ENSURE(parentName)(); \
    *this = *(DERIVE_LOCAL(parentName)); \
    ProtocolClassSetSuperclass(this, DERIVE_LOCAL(parentName)); \
  END


/* DEFINE_ALIAS_CLASS
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


/* ProtocolClass
 *
 * The structure which supports classes for the inheritance
 *  protocol.
 */

typedef struct ProtocolClassStruct *ProtocolClass;


/* ProtocolInst
 *
 * The structure which supports instances for the inheritance 
 * protocol.
 */

typedef struct ProtocolInstStruct *ProtocolInst;


/* ProtocolCoerceInstMethod
 *
 * coerce "pro" to an instance of "interface"
 * If "pro" is an instance of "interface", then returns TRUE 
 * and sets coerceResult to point directly to the part of "pro" 
 * which contains the slots for "interface" 
 */

typedef Bool (*ProtocolCoerceInstMethod)(ProtocolInst *coerceResult,
                                         ProtocolInst pro, 
                                         ProtocolClass interface);

/* ProtocolCoerceClassMethod
 *
 * coerce "proClass" to an "interface" class 
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


/* EnsureProtocolClass
 *
 * Returns the root of the protocol class hierarchy.
 * Function name conforms to standard conventions for 
 * protocols.
 */

extern ProtocolClass EnsureProtocolClass(void);


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

/* The following a macros because of the need to cast */
/* subtypes of ProtocolClass. Nevertheless they are named */
/* as functions. See design.mps.protocol.introspect.c-lang */

#define ProtocolClassSetSuperclass(class, super) \
  (((ProtocolClass)(class))->superclass) = (ProtocolClass)(super)

#define ProtocolClassSuperclass(class) \
  (((ProtocolClass)(class))->superclass)

#define ClassOf(inst) ((ProtocolInst)(inst)->class)

#define IsSubclass(sub, super) \
   ProtocolIsSubclass((ProtocolClass)(sub), (ProtocolClass)(super))


#endif /* protocol_h */
