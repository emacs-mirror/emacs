/* trans.h: TRANSFORMS INTERFACE
 *
 * $Id$
 * Copyright 2011-2022 Ravenbrook Limited.  See end of file for license.
 */

#ifndef trans_h
#define trans_h

#include "mpm.h"


typedef struct mps_transform_s *Transform;

typedef struct OldNewStruct *OldNew;

extern Res TransformCreate(Transform *transformReturn, Arena arena);

extern Res TransformAddOldNew(Transform transform,
                              Ref old_list[],
                              Ref new_list[],
                              Count count);

extern Res TransformApply(Bool *appliedReturn, Transform transform);

extern void TransformDestroy(Transform transform);

extern Bool TransformCheck(Transform transform);

extern Arena TransformArena(Transform transform);


#endif /* trans_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2011-2022 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
