/*  impl.h.poolmfs  draft impl
 *
 *                    MANUAL FIXED SMALL UNIT POOL
 *
 *  $HopeName: MMsrc!poolmfs.h(MMdevel_restr2.2) $
 *
 *  Copyright (C) 1994,1995 Harlequin Group, all rights reserved
 *
 *  The MFS pool is used to manage small fixed-size chunks of memory.  It
 *  stores control structures in the memory it manages, rather than to one
 *  side.  It therefore achieves better locality for small objects, but
 *  wastes memory for large objects.  It should not be used unless you are
 *  packing a reasonable number of objects on to a page.
 *
 *  Create and Init take the following arguments:
 *
 *    Size extendBy
 *
 *  extendBy is the default number of bytes reserved by the pool at a time.
 *  A large size will make allocation cheaper but have a higher resource
 *  overhead.  A typical value might be 65536.  See note 2.
 *
 *    Size unitSize
 *
 *  unitSize is the size in bytes of the objects you with to allocate.  It
 *  must be larger than the minimum unit size returned by GetInfo, and not
 *  larger than extendBy.
 */

#ifndef poolmfs_h
#define poolmfs_h

#include "mpm.h"

typedef struct MFSStruct *MFS;

extern PoolClass PoolClassMFS(void);

extern Bool MFSCheck(MFS mfs);
extern Pool (MFSPool)(MFS mfs);


typedef const struct MFSInfoStruct *MFSInfo;

struct MFSInfoStruct {
  Size unitSizeMin;             /* minimum unit size */
};

extern MFSInfo MFSGetInfo(void);

#endif /* poolmfs_h */
