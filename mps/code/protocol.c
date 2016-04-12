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
#include "classdef.h"

SRCID(protocol, "$Id$");


/* The class definitions for the root of the hierarchy */

static void InstClassInitInternal(InstClass class);

DEFINE_CLASS(Inst, Inst, class)
{
  InstClassInitInternal(class);
  class->instStruct.class = CLASS(InstClass);
}

DEFINE_CLASS(Inst, InstClass, class)
{
  /* Can't use INHERIT_CLASS(class, InstClass, Inst) here because it
     causes infinite regression, so we have to set this one up by
     hand. */
  InstClassInitInternal(class);
  class->superclass = &CLASS_STATIC(Inst);
  class->name = "InstClass";
  class->level = ClassLevelInstClass;
  class->display[ClassLevelInstClass] = ClassIdInstClass;
}

static void InstClassInitInternal(InstClass class)
{
  ClassLevel i;

  class->name = "Inst";
  class->superclass = NULL;
  for (i = 0; i < ClassDEPTH; ++i)
    class->display[i] = 0;
  class->level = 0;
  class->display[class->level] = ClassIdInst;

  /* We can't call CLASS(InstClass) here because it causes a loop back
     to here, so we have to tie this knot specially. */
  class->instStruct.class = &CLASS_STATIC(InstClass);

  class->sig = InstClassSig;
  AVERT(InstClass, class);
}


/* InstClassCheck -- check a protocol class */

Bool InstClassCheck(InstClass class)
{
  ClassLevel i;
  CHECKS(InstClass, class);
  CHECKL(class->name != NULL);
  CHECKL(class->level < ClassDEPTH);
  for (i = 0; i <= class->level; ++i) {
    CHECKL(class->display[i] != 0);
    CHECKL(class->display[i] < ClassIdLIMIT);
  }
  for (i = class->level + 1; i < ClassDEPTH; ++i) {
    CHECKL(class->display[i] == 0);
  }
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
  inst->class = CLASS(Inst);
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
  /* .level = */      ClassIdInvalid,
  /* .display = */    {ClassIdInvalid}
};
  
void InstFinish(Inst inst)
{
  AVERC(Inst, inst);
  inst->class = &invalidClassStruct;
}


/* InstCheck -- check a protocol instance */

Bool InstCheck(Inst inst)
{
  CHECKD(InstClass, inst->class);
  return TRUE;
}


void ClassRegister(InstClass class)
{
  Word classId;

  /* label the pool class with its name */
  EventInit();
  classId = EventInternString(ClassName(class));
  /* NOTE: this breaks <design/type/#addr.use> */
  EventLabelAddr((Addr)class, classId);
}


Res InstDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  InstClass class;
  
  if (!TESTC(Inst, inst))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  class = ClassOfPoly(Inst, inst);
  return WriteF(stream, depth,
		"$S $P\n", (WriteFS)ClassName(class), inst,
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
