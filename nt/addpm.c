/* Add entries to the GNU Emacs Program Manager folder.
   Copyright (C) 1995 Free Software Foundation, Inc.

   This file is part of GNU Emacs.

   GNU Emacs is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any later
   version.

   GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with GNU Emacs; see the file COPYING.  If not, write to the Free Software
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/****************************************************************************
 *
 * Program: addpm	(adds emacs to the Windows program manager)
 *
 * Usage:
 *   	argv[1] = full path to program to execute
 *	argv[2] = full path to icon for emacs (optional)
 */

#include <windows.h>
#include <ddeml.h>
#include <stdlib.h>
#include <stdio.h>

HDDEDATA CALLBACK DdeCallback (UINT uType, UINT uFmt, HCONV hconv,
			       HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
			       DWORD dwData1, DWORD dwData2)
{
  return ((HDDEDATA)NULL);
}

#define DdeCommand(str) 	\
	DdeClientTransaction (str, strlen(str)+1, HConversation, (HSZ)NULL, \
		              CF_TEXT, XTYP_EXECUTE, 30000, NULL)

main (argc, argv)
     int argc;
     char *argv[];			
{
  DWORD idDde;
  HCONV HConversation;
  HSZ ProgMan;
  char additem[MAX_PATH*2 + 100];

  if (argc < 2 || argc > 3)
    {
      fprintf(stderr, "usage: addpm exe_path [icon_path]\n");
      exit(1);
    }

  DdeInitialize (&idDde, (PFNCALLBACK)DdeCallback, APPCMD_CLIENTONLY, 0);

  ProgMan = DdeCreateStringHandle (idDde, "PROGMAN", CP_WINANSI);

  if (HConversation = DdeConnect (idDde, ProgMan, ProgMan, NULL))
    {
      DdeCommand ("[CreateGroup(Gnu Emacs)]");
      DdeCommand ("[ReplaceItem(Emacs)]");
      sprintf (additem, "[AddItem(%s,Emacs%c%s)]",
	       argv[1], (argc>2 ? ',' : ' '),
	       (argc>2 ? argv[2] : ""));
      DdeCommand (additem);

      DdeDisconnect (HConversation);
    }

  DdeFreeStringHandle (idDde, ProgMan);

  DdeUninitialize (idDde);

  return (0);
}
