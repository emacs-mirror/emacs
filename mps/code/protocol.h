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
#define INST_CHECK(ident) ident ## Check
#define CLASS_TYPE(ident) ident ## Class
#define CLASS_STRUCT(ident) ident ## ClassStruct
#define CLASS_ENSURE(ident) ident ## ClassGet
#define CLASS_INIT(ident) ident ## ClassInit
#define CLASS_CHECK(ident) ident ## ClassCheck
#define CLASS_SUPER(ident) ident ## SuperClassGet
#define CLASS_GUARDIAN(ident) ClassGuardian ## ident
#define CLASS_STATIC(ident) ClassStatic ## ident
#define KIND_CLASS(ident) ident ## Class


/* DECLARE_CLASS -- declare the existence of a protocol class
 *
 * Declares a prototype for the class ensure function, which ensures
 * that the class is initialized once and return it.
 */

#define DECLARE_CLASS(kind, ident) \
  extern CLASS_TYPE(kind) CLASS_ENSURE(ident)(void); \
  extern void CLASS_INIT(ident)(CLASS_TYPE(kind) var)


/* DEFINE_CLASS -- define a protocol class */

#define DEFINE_CLASS(kind, ident, var) \
  DECLARE_CLASS(kind, ident); \
  static Bool CLASS_GUARDIAN(ident) = FALSE; \
  static CLASS_STRUCT(kind) CLASS_STATIC(ident); \
  CLASS_TYPE(kind) CLASS_ENSURE(ident)(void) \
  { \
    CLASS_TYPE(kind) class = &CLASS_STATIC(ident); \
    if (CLASS_GUARDIAN(ident) == FALSE) { \
      LockClaimGlobalRecursive(); \
      if (CLASS_GUARDIAN(ident) == FALSE) { \
        CLASS_INIT(ident)(class); \
	/* Prevent infinite regress. */ \
	if (ClassId ## ident != ClassIdInstClass && \
	    ClassId ## ident != ClassIdInst) \
          SetClassOfPoly(class, CLASS(KIND_CLASS(kind))); \
        AVER(CLASS_CHECK(kind)(class)); \
        CLASS_GUARDIAN(ident) = TRUE; \
	ClassRegister(MustBeA(InstClass, class)); \
      } \
      LockReleaseGlobalRecursive(); \
    } \
    return class; \
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


/* ClassLevelEnum -- depth of class in hierarchy
 *
 * This defines enum constants like ClassLevelLand equal to the
 * distance from the root of the class hierarchy.
 */

#define CLASS_LEVEL_ENUM(prefix, ident, kind, super) prefix ## ident = prefix ## super + 1,
typedef enum ClassLevelEnum {
  ClassLevelNoSuper = -1, /* so that root classes (e.g. Inst) get level zero */
  CLASSES(CLASS_LEVEL_ENUM, ClassLevel)
  ClassLevelTerminalCommaNotAllowedInC89
} ClassLevelEnum;


/* INHERIT_CLASS -- inheriting from a superclass
 *
 * This macro is used at the start of a class definition to inherit
 * the superclass and override the fields essential to the
 * workings of the protocol.
 */

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


/* Inst -- the instance structure for support of the protocol
 *
 * An InstStruct named instStruct must be the first field of any
 * instance structure using the protocol, because the protocol uses
 * casting between structures with common prefixes to implement
 * polymorphism.
 */

typedef struct InstStruct *Inst;
typedef struct InstClassStruct *InstClass;

typedef struct InstStruct {
  InstClass class;
  /* Do not add permanent fields here.  Introduce a subclass. */
} InstStruct;


/* InstClass -- the class containing the support for the protocol */

typedef const char *ClassName;
typedef unsigned char ClassId;
typedef unsigned char ClassLevel;
#define ClassDEPTH 8            /* maximum depth of class hierarchy */

#define InstClassSig ((Sig)0x519B60C7) /* SIGnature PROtocol CLass */

typedef struct InstClassStruct {
  InstStruct instStruct;        /* classes are instances of kinds */
  Sig sig;                      /* <design/sig/> */
  ClassName name;               /* human readable name such as "Land" */
  InstClass superclass;         /* pointer to direct superclass */
  ClassLevel level;             /* distance from root of class hierarchy */
  ClassId display[ClassDEPTH];  /* ids of classes at this level and above */
} InstClassStruct;

DECLARE_CLASS(Inst, InstClass);
DECLARE_CLASS(Inst, Inst);

extern Bool InstClassCheck(InstClass class);
extern Bool InstCheck(Inst inst);
extern void InstInit(Inst inst);
extern void InstFinish(Inst inst);
extern Res InstDescribe(Inst inst, mps_lib_FILE *stream, Count depth);


/* ClassRegister -- class registration
 *
 * This is called once for each class initialised by DEFINE_CLASS and
 * is not intended for use outside this file.
 */

extern void ClassRegister(InstClass class);


/* IsSubclass, IsA -- fast subclass test
 *
 * The InstClassStruct is arranged to make this test fast and simple,
 * so that it can be used as a consistency check in the MPS.  Each
 * class has an array of the ids of its superclasses, indexed by the
 * level in the hierarchy of the class.  The level and id are
 * statically known, so they can be tested by accessing just one
 * class.
 */

#define IsSubclass(sub, super) \
  (((InstClass)(sub))->display[ClassLevel ## super] == ClassId ## super)
  
#define IsA(_class, inst) \
  IsSubclass(CouldBeA(Inst, inst)->class, _class)

#define IsNonNullAndA(_class, inst) \
  ((inst) != NULL && \
   CouldBeA(Inst, inst)->class != NULL && \
   IsA(_class, inst))


/* CouldBeA, MustBeA -- coerce instances
 *
 * CouldBeA converts an instance to another class without checking.
 * It is intended to be equivalent to the C++ "static_cast", although
 * since this is C there is no actual static checking, so in fact it's
 * more like "reinterpret_cast".
 *
 * MustBeA converts an instance to another class, but checks that the
 * object is a subclass, causing an assertion if not (depending on
 * build variety).  It is like C++ "dynamic_cast" with an assert.
 */

#define CouldBeA(class, inst) ((INST_TYPE(class))(inst))

#define MustBeA(_class, inst) \
  CouldBeA(_class, \
	   AVERPC(IsNonNullAndA(_class, inst), \
		  "MustBeA " #_class ": " #inst, \
		  inst))

#define MustBeA_CRITICAL(_class, inst) \
  CouldBeA(_class, \
	   AVERPC_CRITICAL(IsNonNullAndA(_class, inst), \
			   "MustBeA " #_class ": " #inst, \
			   inst))


/* Protocol introspection interface
 *
 * The following are macros because of the need to cast subtypes of
 * InstClass. Nevertheless they are named as functions. See
 * <design/protocol/#introspect.c-lang>.
 */

#define SuperclassPoly(kind, class) \
  MustBeA(KIND_CLASS(kind), MustBeA(InstClass, class)->superclass)

#define ClassOfPoly(kind, inst) \
  MustBeA(KIND_CLASS(kind), MustBeA(Inst, inst)->class)


/* ClassName -- get the human readable name of a class
 *
 * ClassName is used in describe methods and other unsafe places, so
 * we don't use MustBeA.
 */

#define ClassName(class) RVALUE(CouldBeA(InstClass, class)->name)


/* SetClassOfPoly -- set the class of an object
 *
 * This should only be used when specialising an instance to be a
 * member of a subclass.  Each Init function should call its
 * superclass init, finally reaching InstInit, and then, once it has
 * set up its fields, use SetClassOfPoly to set the class and check
 * the instance with its check method.  Compare with design.mps.sig.
 */

#define SetClassOfPoly(inst, _class) \
  BEGIN MustBeA(Inst, inst)->class = MustBeA(InstClass, _class); END


/* Method -- method call
 *
 * Use this macro to call a method on a class, rather than accessing
 * the class directly.  For example:
 *
 *     res = Method(Land, land, insert)(land, range);
 */

#define Method(kind, inst, meth) (ClassOfPoly(kind, inst)->meth)


/* NextMethod -- call a method in the superclass
 *
 * See design.mps.protocol.int.static-superclass.
 *
 * TODO: All uses of NextMethod are statically known, but several
 * experiments with statically generating some kind of SUPERCLASS
 * lookup have failed because the names of types, classes, and the
 * hierarchy are inconsistent.  Revisit this later.
 */

#define SUPERCLASS(kind, ident) \
  MustBeA(KIND_CLASS(kind), CouldBeA(InstClass, CLASS(ident))->superclass)

#define NextMethod(kind, ident, meth) (SUPERCLASS(kind, ident)->meth)


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
