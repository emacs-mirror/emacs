/* The class definition for the root of the hierarchy */

/* pool.c: PROTOCOL IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * See design.mps.protocol.
 */

#include "mpm.h"
#include "protocol.h"

SRCID(protocol, "$Id$");


/* The class definitions for the root of the hierarchy */

static void InstClassInitInternal(InstClass klass);

DEFINE_CLASS(Inst, Inst, klass)
{
  InstClassInitInternal(klass);
  klass->instStruct.klass = CLASS(InstClass);
}

DEFINE_CLASS(Inst, InstClass, klass)
{
  /* Can't use INHERIT_CLASS(klass, InstClass, Inst) here because it
     causes infinite regression, so we have to set this one up by
     hand. */
  InstClassInitInternal(klass);
  klass->superclass = &CLASS_STATIC(Inst);
  klass->name = "InstClass";
  klass->level = ClassLevelInstClass;
  klass->display[ClassLevelInstClass] = CLASS_ID(InstClass);
}

static void InstClassInitInternal(InstClass klass)
{
  ClassLevel i;

  klass->name = "Inst";
  klass->superclass = NULL;
  for (i = 0; i < ClassDEPTH; ++i)
    klass->display[i] = NULL;
  klass->level = 0;
  klass->display[klass->level] = CLASS_ID(Inst);

  /* Generic methods */
  klass->describe = InstDescribe;
  klass->finish = InstFinish;

  /* We can't call CLASS(InstClass) here because it causes a loop back
     to here, so we have to tie this knot specially. */
  klass->instStruct.klass = &CLASS_STATIC(InstClass);

  klass->sig = InstClassSig;
  AVERT(InstClass, klass);
}


/* InstClassCheck -- check a protocol class */

Bool InstClassCheck(InstClass klass)
{
  ClassLevel i;
  CHECKS(InstClass, klass);
  CHECKL(klass->name != NULL);
  CHECKL(klass->level < ClassDEPTH);
  for (i = 0; i <= klass->level; ++i) {
    CHECKL(klass->display[i] != NULL);
  }
  for (i = klass->level + 1; i < ClassDEPTH; ++i) {
    CHECKL(klass->display[i] == NULL);
  }
  CHECKL(FUNCHECK(klass->describe));
  CHECKL(FUNCHECK(klass->finish));
  return TRUE;
}


/* InstInit -- initialize a protocol instance
 *
 * Initialisation makes the instance valid, so that it will pass
 * InstCheck, and the instance can be specialized to be a member of a
 * subclass.
 */

void InstInit(Inst inst)
{
  AVER(inst != NULL);
  inst->klass = CLASS(Inst);
  AVERC(Inst, inst);
}


/* InstFinish -- finish a protocol instance
 *
 * Finishing makes the instance invalid, so that it will fail
 * InstCheck and can't be used.
 */

static InstClassStruct invalidClassStruct = {
  /* .instStruct = */ {&invalidClassStruct},
  /* .sig = */        SigInvalid,
  /* .name = */       "Invalid",
  /* .superclass = */ &invalidClassStruct,
  /* .level = */      0,
  /* .display = */    {(ClassId)&invalidClassStruct},
  /* .describe = */   NULL,
  /* .finish = */     NULL,
};
  
void InstFinish(Inst inst)
{
  AVERC(Inst, inst);
  inst->klass = &invalidClassStruct;
}


/* InstCheck -- check a protocol instance */

Bool InstCheck(Inst inst)
{
  CHECKD(InstClass, inst->klass);
  return TRUE;
}


void ClassRegister(InstClass klass)
{
  Word classId;

  /* label the pool class with its name */
  EventInit();
  classId = EventInternString(ClassName(klass));
  /* NOTE: this breaks <design/type/#addr.use> */
  EventLabelAddr((Addr)klass, classId);
}


Res InstDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  InstClass klass;
  
  if (!TESTC(Inst, inst))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  klass = ClassOfPoly(Inst, inst);
  return WriteF(stream, depth,
                "$S $P\n", (WriteFS)ClassName(klass), inst,
                NULL);
}


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
