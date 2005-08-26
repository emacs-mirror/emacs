/* Implementation of chdir on the Mac for use with make-docfile.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Contributed by Andrew Choi (akochoi@mac.com).  */

#include <string.h>
#include <Files.h>
#include <TextUtils.h>

int chdir(const char *path)
{
  WDPBRec wdpb;

  Str255 mypath;
  OSErr error;

  strcpy(mypath, path);
  c2pstr(mypath);

  wdpb.ioNamePtr = mypath;
  wdpb.ioVRefNum = 0;
  wdpb.ioWDDirID = 0;
  error = PBHSetVolSync(&wdpb);

  return error == noErr ? 0 : -1;
}

/* arch-tag: f567b034-fd9e-43d1-94cb-9012375237d1
   (do not change this comment) */
