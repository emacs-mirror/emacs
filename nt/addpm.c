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

/* addpm: Adds entries to the GNU Emacs Program Manager folder.

   	argv[1] = full path to program to execute
	argv[2] = full path to icon for emacs (optional)
 */

#include <windows.h>    // required for all Windows applications
#include <ddeml.h>      // required for DDEML
#include <string.h>     // required for strcpy and strlen

HDDEDATA EXPENTRY dde_callback (WORD, WORD, HCONV, HSZ, HSZ, HDDEDATA, DWORD, DWORD);
BOOL              send_shell_command (DWORD, LPSTR);

// Global variables
HANDLE gh_inst;           // current instance

/****************************************************************************
    FUNCTION: WinMain()

    PURPOSE:  Calls initialization function, processes message loop

    PARAMETERS:
        HANDLE h_instance
        HANDLE h_prev_instance
        LPSTR  lp_cmd_line
        int    n_cmd_show

    RETURNS:
        int
****************************************************************************/

int PASCAL 
WinMain (HANDLE h_instance,      // current instance
	 HANDLE h_prev_instance, // previous instance
	 LPSTR  lp_cmd_line,     // command line
	 int    n_cmd_show)      // show-window type (open/icon)
{
  DWORD id_inst = 0L;      // instance identifier
  FARPROC lp_dde_proc;
  char *path, *icon, *s;
  char additem[MAX_PATH*2 + 100];

  gh_inst = h_instance;

  for (path = NULL, s = lp_cmd_line; *s && isspace (*s); s++);
  if (*s) 
    {
      path = s;
      while (*s && !isspace (*s)) 
	s++;
      if (*s) 
	*(s++) = '\0';
    }
  for (icon = NULL; *s && isspace (*s); s++);
  if (*s) 
    {
      icon = s;
      while (*s && !isspace (*s)) 
	s++;
      if (*s) 
	*(s++) = '\0';
    }
  
  lp_dde_proc = MakeProcInstance ((FARPROC) dde_callback, gh_inst);
  
  DdeInitialize (&id_inst,                   // receives instance ID
		 (PFNCALLBACK) lp_dde_proc,  // address of callback function
		 APPCMD_CLIENTONLY,          // this is a client app
		 0L);                        // reserved

  send_shell_command (id_inst, (LPSTR) "[CreateGroup(Gnu Emacs)]");

  send_shell_command (id_inst, (LPSTR) "[ReplaceItem(Emacs)]");

  sprintf (additem, "[AddItem(%s,Emacs%c%s)]",
	   path, (icon ? ',' : ' '), (icon ? icon : ""));
  send_shell_command (id_inst, additem);

  DdeUninitialize (id_inst);

  return (0);
}


/****************************************************************************
    FUNCTION: dde_callback()

    PURPOSE:  Processes messages for DDEML conversation

    PARAMETERS:
        WORD     w_type
        WORD     w_fmt
        HCONV    h_conv
        HSZ      hsz1
        HSZ      hsz2
        HDDEDATA h_data
        DWORD    dw_data1
        DWORD    dw_data2

    RETURNS:
        HDDEDATA
****************************************************************************/

HDDEDATA EXPENTRY 
dde_callback (WORD     w_type,   // transaction type
	      WORD     w_fmt,    // clipboard format
	      HCONV    h_conv,   // handle of the conversation
	      HSZ      hsz1,    // handle of a string
	      HSZ      hsz2,    // handle of a string
	      HDDEDATA h_data,   // handle of a global memory object
	      DWORD    dw_data1, // transaction-specific data
	      DWORD    dw_data2) // transaction-specific data
{
  // Nothing need be done here...
  return (HDDEDATA) NULL;
}


/****************************************************************************
    FUNCTION: send_shell_command()

    PURPOSE:  Sends the given command string to Program Manager

    PARAMETERS:
        LPSTR - pointer to command string

    RETURNS:
        BOOL  - TRUE if this function succeeds, FALSE otherwise
****************************************************************************/

BOOL 
send_shell_command (DWORD id_inst,    // instance identifier
		    LPSTR lp_command) // command string to execute
{
  HSZ      hsz_serv_top;     // Service and Topic name are "PROGMAN"
  HCONV    hconv;            // handle of conversation
  int      n_len;            // length of command string
  HDDEDATA h_data;           // return value of DdeClientTransaction
  DWORD    dw_result;        // result of transaction
  BOOL     b_result = FALSE; // TRUE if this function is successful

  // create string handle to service/topic
  hsz_serv_top = DdeCreateStringHandle (id_inst, "PROGMAN", CP_WINANSI);

  // attempt to start conversation with server app
  if ((hconv = DdeConnect (id_inst, hsz_serv_top, hsz_serv_top, NULL))
      != (HCONV) NULL)
    {
      // get length of the command string
      n_len = lstrlen ((LPSTR) lp_command);

      // send command to server app
      h_data = DdeClientTransaction ((LPBYTE) lp_command, // data to pass
				     n_len + 1,       // length of data
				     hconv,           // handle of conversation
				     (HCONV) NULL,    // handle of name-string
				     CF_TEXT,         // clipboard format
				     XTYP_EXECUTE,    // transaction type
				     1000,            // timeout duration
				     &dw_result);     // transaction result

      if (h_data)
	b_result = TRUE;

      // end conversation
      DdeDisconnect (hconv);
    }

  // free service/topic string handle
  DdeFreeStringHandle (id_inst, hsz_serv_top);

  return b_result;
}
