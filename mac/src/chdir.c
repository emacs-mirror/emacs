/* Implementation of chdir on the Mac for use with make-docfile.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004,
      2005, 2006, 2007, 2008  Free Software Foundation, Inc.

Contributed by Andrew Choi (akochoi@mac.com).

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


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
