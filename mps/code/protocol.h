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


/* Identifier derivation macros.
 *
 * These turn the base identifier of a class (e.g. "Inst") into other
 * identifiers (e.g. "InstClassStruct").  These are not intended to be
 * used outside of this file.  These macros implement
 * design.mps.protocol.overview.naming and
 * design.mps.impl.derived-names.
 *
 * INST_TYPE derives the type of an instance of the class,
 * e.g. "Land", which will be a pointer to an INST_STRUCT.
 *
 * INST_STRUCT derives the type of a structure of an instance,
 * e.g. "LandStruct".
 *
 * INST_CHECK derives the name of the checking function for the
 * instance, e.g. "LandCheck".
 *
 * CLASS_TYPE derives the type of the class, e.g. "LandClass", which
 * will be a pointer to a CLASS_STRUCT.
 *
 * CLASS_STRUCT derives the type of the structure of the class,
 * e.g. "LandClassStruct".
 *
 * CLASS_ENSURE derives the name of the ensure function that returns
 * the canonical class object, e.g. "LandClassGet".
 *
 * CLASS_INIT derives the name of the init function that initializes a
 * CLASS_STRUCT, e.g. "LandClassInit".
 *
 * CLASS_CHECK derives the name of the checking function for the
 * class, e.g. "LandClassCheck".
 *
 * CLASS_GUARDIAN derives the name of a boolean that indicates whether
 * the canonical class object has been initialized yet,
 * e.g. "ClassGuardianLand".
 *
 * CLASS_STATIC derives the name of a static global variable that
 * contains the canonical class object, e.g. "ClassStaticLand".
 *
 * KIND_CLASS derives the class name of a kind, which is used in
 * contexts like CLASS_TYPE(KIND_CLASS(kind)), so that the kind "Land"
 * is implemented by the canonical "LandClassClass".
 */

#define INST_TYPE(klass) klass
#define INST_STRUCT(klass) klass ## Struct
#define INST_CHECK(klass) klass ## Check
#define CLASS_TYPE(klass) klass ## Class
#define CLASS_STRUCT(klass) klass ## ClassStruct
#define CLASS_ENSURE(klass) klass ## ClassGet
#define CLASS_INIT(klass) klass ## ClassInit
#define CLASS_CHECK(klass) klass ## ClassCheck
#define CLASS_GUARDIAN(klass) ClassGuardian ## klass
#define CLASS_STATIC(klass) ClassStatic ## klass
#define KIND_CLASS(klass) klass ## Class


/* ClassId -- static identity of a class
 *
 * We use the address of the static storage for the canonical class
 * object as the class id, suitable for fast comparison.  This is not
 * intended to be dereferenced.  We would like to define it as a
 * pointer to an incomplete structure, but GCC 4.7 buggily complains
 * about punning if we do that, so use void *, even though that's a
 * bit more error prone.
 */

typedef void *ClassId;
#define CLASS_ID(klass) ((ClassId)&CLASS_STATIC(klass))


/* DECLARE_CLASS -- declare the existence of a protocol class
 *
 * Declares a prototype for the class ensure function, which ensures
 * that the class is initialized once and return it.  See
 * design.mps.protocol.if.declare-class.
 */

#define DECLARE_CLASS(kind, klass, super) \
  extern CLASS_TYPE(kind) CLASS_ENSURE(klass)(void); \
  extern void CLASS_INIT(klass)(CLASS_TYPE(kind) var); \
  extern CLASS_STRUCT(kind) CLASS_STATIC(klass); \
  enum { ClassLevel ## klass = ClassLevel ## super + 1 }


/* DEFINE_CLASS -- define a protocol class
 *
 * Defines the static storage and functions for the canonical class
 * object for a class.  Takes care to avoid initializing the class
 * twice, even when called asynchronously from multiple threads, since
 * this code can be reached without first entering an arena. See
 * design.mps.protocol.if.define-class.
 */

#define DEFINE_CLASS(kind, className, var) \
  static Bool CLASS_GUARDIAN(className) = FALSE; \
  CLASS_STRUCT(kind) CLASS_STATIC(className); \
  CLASS_TYPE(kind) CLASS_ENSURE(className)(void) \
  { \
    CLASS_TYPE(kind) klass = &CLASS_STATIC(className); \
    if (CLASS_GUARDIAN(className) == FALSE) { \
      LockClaimGlobalRecursive(); \
      if (CLASS_GUARDIAN(className) == FALSE) { \
        CLASS_INIT(className)(klass); \
        /* Prevent infinite regress. */ \
        if (CLASS_ID(className) != CLASS_ID(InstClass) && \
            CLASS_ID(className) != CLASS_ID(Inst)) \
          SetClassOfPoly(klass, CLASS(KIND_CLASS(kind))); \
        AVER(CLASS_CHECK(kind)(klass)); \
        CLASS_GUARDIAN(className) = TRUE; \
        ClassRegister(MustBeA(InstClass, klass)); \
      } \
      LockReleaseGlobalRecursive(); \
    } \
    return klass; \
  } \
  void CLASS_INIT(className)(CLASS_TYPE(kind) var)


/* CLASS -- expression for getting a class
 *
 * Use this to get a class, rather than calling anything defined by
 * DEFINE_CLASS directly.  See design.mps.protocol.if.class.
 */

#define CLASS(klass) (CLASS_ENSURE(klass)())


/* INHERIT_CLASS -- inheriting from a superclass
 *
 * This macro is used at the start of a class definition to inherit
 * the superclass and override the fields essential to the workings of
 * the protocol.  See design.mps.protocol.if.inheritance.
 */

#define INHERIT_CLASS(this, _class, super) \
  BEGIN \
    InstClass instClass = (InstClass)(this); \
    CLASS_INIT(super)(this); \
    instClass->superclass = (InstClass)CLASS(super); \
    instClass->name = #_class; \
    instClass->level = instClass->superclass->level + 1; \
    AVER(instClass->level < ClassDEPTH); \
    instClass->display[instClass->level] = CLASS_ID(_class); \
  END


/* Inst -- the base class of the hierarchy
 *
 * An InstStruct named instStruct must be the first field of any
 * instance structure using the protocol
 * (design.mps.protocol.overview.prefix).
 */

typedef struct InstStruct *Inst;
typedef struct InstClassStruct *InstClass;

typedef struct InstStruct {
  InstClass klass;
  /* Do not add permanent fields here.  Introduce a subclass. */
} InstStruct;

typedef const char *ClassName;
typedef unsigned char ClassLevel;
typedef Res (*DescribeMethod)(Inst inst, mps_lib_FILE *stream, Count depth);
#define ClassDEPTH 8            /* maximum depth of class hierarchy */

#define InstClassSig ((Sig)0x519B1452) /* SIGnature Protocol INST */

typedef struct InstClassStruct {
  InstStruct instStruct;        /* classes are instances of kinds */
  Sig sig;                      /* <design/sig/> */
  ClassName name;               /* human readable name such as "Land" */
  InstClass superclass;         /* pointer to direct superclass */
  ClassLevel level;             /* distance from root of class hierarchy */
  ClassId display[ClassDEPTH];  /* classes at this level and above */
  DescribeMethod describe;      /* write a debugging description */
} InstClassStruct;

enum {ClassLevelNoSuper = -1};
DECLARE_CLASS(Inst, Inst, NoSuper);
DECLARE_CLASS(Inst, InstClass, Inst);

extern Bool InstClassCheck(InstClass klass);
extern Bool InstCheck(Inst inst);
extern void InstInit(Inst inst);
extern void InstFinish(Inst inst);
extern Res InstDescribe(Inst inst, mps_lib_FILE *stream, Count depth);


/* ClassRegister -- class registration
 *
 * This is called once for each class initialised by DEFINE_CLASS and
 * is not intended for use outside this file.
 */

extern void ClassRegister(InstClass klass);


/* IsSubclass, IsA -- fast subclass test
 *
 * The InstClassStruct is arranged to make these tests fast and
 * simple, so that it can be used as a consistency check in the MPS.
 * See design.mps.protocol.impl.subclass.
 */

#define IsSubclass(sub, super) \
  (((InstClass)(sub))->display[ClassLevel ## super] == CLASS_ID(super))
  
#define IsA(_class, inst) \
  IsSubclass(CouldBeA(Inst, inst)->klass, _class)

#define IsNonNullAndA(_class, inst) \
  ((inst) != NULL && \
   CouldBeA(Inst, inst)->klass != NULL && \
   IsA(_class, inst))


/* CouldBeA, MustBeA -- coerce instances
 *
 * CouldBeA converts an instance to another class without checking,
 * like C++ ``static_cast``.  See design.mps.protocol.if.could-be-a.
 *
 * MustBeA converts an instance to another class, but checks that the
 * object is a subclass, causing an assertion if not (depending on
 * build variety).  See design.mps.protocol.if.must-be-a.  It is like
 * C++ "dynamic_cast" with an assert.
 *
 * MustBeA_CRITICAL is like MustBeA for use on the critical path,
 * where it does no checking at all in production builds.  See
 * design.mps.protocol.if.must-be-a.critical.
 */

#define CouldBeA(klass, inst) ((INST_TYPE(klass))(inst))

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
 * design.mps.protocol.introspect.
 */

#define SuperclassPoly(kind, klass) \
  MustBeA(KIND_CLASS(kind), MustBeA(InstClass, klass)->superclass)

#define ClassOfPoly(kind, inst) \
  MustBeA(KIND_CLASS(kind), MustBeA(Inst, inst)->klass)


/* ClassName -- get the human readable name of a class
 *
 * ClassName is used in describe methods and other unsafe places, so
 * we don't use MustBeA.
 */

#define ClassName(klass) RVALUE(CouldBeA(InstClass, klass)->name)


/* SetClassOfPoly -- set the class of an object
 *
 * This should only be used when specialising an instance to be a
 * member of a subclass, once the instance has been initialized.  See
 * design.mps.protocol.if.set-class.
 */

#define SetClassOfPoly(inst, _class) \
  BEGIN MustBeA(Inst, inst)->klass = MustBeA(InstClass, _class); END


/* Method -- method call
 *
 * Use this macro to call a method on a class, rather than accessing
 * the class directly.  See design.mps.protocol.if.method.  For
 * example:
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

#define SUPERCLASS(kind, klass) \
  MustBeA(KIND_CLASS(kind), CouldBeA(InstClass, CLASS(klass))->superclass)

#define NextMethod(kind, klass, meth) (SUPERCLASS(kind, klass)->meth)


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
