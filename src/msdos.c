/* MS-DOS specific C utilities.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Contributed by Morten Welinder */

/* Note: some of the stuff here was taken from end of sysdep.c in demacs. */

#include <config.h>

#ifdef MSDOS
#include "lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/time.h>
#include <dos.h>
#include "dosfns.h"
#include "msdos.h"
#include "systime.h"
#include "termhooks.h"
#include "dispextern.h"
#include "termopts.h"
#include "frame.h"
#include "window.h"
#include <go32.h>
#include <pc.h>
#include <ctype.h>
/* #include <process.h> */
/* Damn that local process.h!  Instead we can define P_WAIT ourselves.  */
#define P_WAIT 1

static int break_stat;	 /* BREAK check mode status.	*/
static int stdin_stat;	 /* stdin IOCTL status.		*/
static int extended_kbd; /* 101 (102) keyboard present.	*/

int have_mouse;          /* Mouse present?		*/
static int mouse_last_x;
static int mouse_last_y;

/*  Turn off Dos' Ctrl-C checking and inhibit interpretation of control chars
    by Dos.  Determine the keyboard type.  */
int
dos_ttraw ()
{
  union REGS inregs, outregs;

  inregs.h.ah = 0xc0;
  int86 (0x15, &inregs, &outregs);
  extended_kbd = (!outregs.x.cflag) && (outregs.h.ah == 0);

  break_stat = getcbrk ();
  setcbrk (0);
  install_ctrl_break_check ();
  have_mouse = mouse_init1 ();

  inregs.x.ax = 0x4400;	/* Get IOCTL status. */
  inregs.x.bx = 0x00;	/* 0 = stdin. */
  intdos (&inregs, &outregs);
  stdin_stat = outregs.h.dl;

  inregs.x.dx = (outregs.x.dx | 0x0020) & 0x0027; /* raw mode */
  inregs.h.al = 0x01;
  intdos (&inregs, &outregs);
  return !outregs.x.cflag;
}

/*  Restore status of standard input and Ctrl-C checking.  */
int
dos_ttcooked ()
{
  union REGS inregs, outregs;

  setcbrk (break_stat);
  if (have_mouse) mouse_off ();

  inregs.x.ax = 0x4401;	/* Set IOCTL status.	*/
  inregs.x.bx = 0x00;	/* 0 = stdin.		*/
  inregs.x.dx = stdin_stat;
  intdos (&inregs, &outregs);
  return !outregs.x.cflag;
}

static unsigned short
ibmpc_translate_map[] =
{
  /* --------------- 00 to 0f --------------- */
  0, /* Ctrl Break */
  0xff1b, /* Escape */
  0xffb1, /* Keypad 1 */
  0xffb2, /* Keypad 2 */
  0xffb3, /* Keypad 3 */
  0xffb4, /* Keypad 4 */
  0xffb5, /* Keypad 5 */
  0xffb6, /* Keypad 6 */
  0xffb7, /* Keypad 7 */
  0xffb8, /* Keypad 8 */
  0xffb9, /* Keypad 9 */
  0xffb0, /* Keypad 0 */
  '-', '=',
  0xff08, /* Backspace */
  0xff74, /* (Shift) Tab [Tab doesn't use this table] */

  /* --------------- 10 to 1f --------------- */
  'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']',
  0xff8d, /* Keypad Enter */
  0, /* Ctrl */
  'a', 's',

  /* --------------- 20 to 2f --------------- */
  'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`',
  0, /* Left shift */
  '\\', 'z', 'x', 'c', 'v',

  /* --------------- 30 to 3f --------------- */
  'b', 'n', 'm', ',', '.',
  0xffaf, /* Grey / */
  0, /* Right shift */
  0xffaa, /* Grey * */
  0, /* Alt */
  ' ',
  0, /* Caps Lock */
  0xffbe, /* F1 */
  0xffbf, /* F2 */
  0xffc0, /* F3 */
  0xffc1, /* F4 */
  0xffc2, /* F5 */

  /* --------------- 40 to 4f --------------- */
  0xffc3, /* F6 */
  0xffc4, /* F7 */
  0xffc5, /* F8 */
  0xffc6, /* F9 */
  0xffc7, /* F10 */
  0, /* Num Lock */
  0, /* Scroll Lock */
  0xff50, /* Home */
  0xff52, /* Up */
  0xff55, /* Page Up */
  0xffad, /* Grey - */
  0xff51, /* Left */
  0xffb5, /* Keypad 5 */
  0xff53, /* Right */
  0xffab, /* Grey + */
  0xff57, /* End */

  /* --------------- 50 to 5f --------------- */
  0xff54, /* Down */
  0xff56, /* Page Down */
  0xff63, /* Insert */
  0xffff, /* Delete */
  0xffbe, /* (Shift) F1 */
  0xffbf, /* (Shift) F2 */
  0xffc0, /* (Shift) F3 */
  0xffc1, /* (Shift) F4 */
  0xffc2, /* (Shift) F5 */
  0xffc3, /* (Shift) F6 */
  0xffc4, /* (Shift) F7 */
  0xffc5, /* (Shift) F8 */
  0xffc6, /* (Shift) F9 */
  0xffc7, /* (Shift) F10 */
  0xffbe, /* (Ctrl) F1 */
  0xffbf, /* (Ctrl) F2 */

  /* --------------- 60 to 6f --------------- */
  0xffc0, /* (Ctrl) F3 */
  0xffc1, /* (Ctrl) F4 */
  0xffc2, /* (Ctrl) F5 */
  0xffc3, /* (Ctrl) F6 */
  0xffc4, /* (Ctrl) F7 */
  0xffc5, /* (Ctrl) F8 */
  0xffc6, /* (Ctrl) F9 */
  0xffc7, /* (Ctrl) F10 */
  0xffbe, /* (Alt) F1 */
  0xffbf, /* (Alt) F2 */
  0xffc0, /* (Alt) F3 */
  0xffc1, /* (Alt) F4 */
  0xffc2, /* (Alt) F5 */
  0xffc3, /* (Alt) F6 */
  0xffc4, /* (Alt) F7 */
  0xffc5, /* (Alt) F8 */

  /* --------------- 70 to 7f --------------- */
  0xffc6, /* (Alt) F9 */
  0xffc7, /* (Alt) F10 */
  0xff6d, /* (Ctrl) Sys Rq */
  0xff51, /* (Ctrl) Left */
  0xff53, /* (Ctrl) Right */
  0xff57, /* (Ctrl) End */
  0xff56, /* (Ctrl) Page Down */
  0xff50, /* (Ctrl) Home */
  '1', '2', '3', '4', '5', '6', '7', '8', /* (Alt) */

  /* --------------- 80 to 8f --------------- */
  '9', '0', '-', '=', /* (Alt) */
  0xff55, /* (Ctrl) Page Up */
  0xffc8, /* F11 */
  0xffc9, /* F12 */
  0xffc8, /* (Shift) F11 */
  0xffc9, /* (Shift) F12 */
  0xffc8, /* (Ctrl) F11 */
  0xffc9, /* (Ctrl) F12 */
  0xffc8, /* (Alt) F11 */
  0xffc9, /* (Alt) F12 */
  0xff52, /* (Ctrl) Up */
  0xffae, /* (Ctrl) Grey - */
  0xffb5, /* (Ctrl) Keypad 5 */

  /* --------------- 90 to 9f --------------- */
  0xffab, /* (Ctrl) Grey + */
  0xff54, /* (Ctrl) Down */
  0xff63, /* (Ctrl) Insert */
  0xffff, /* (Ctrl) Delete */
  0xff09, /* (Ctrl) Tab */
  0xffaf, /* (Ctrl) Grey / */
  0xffaa, /* (Ctrl) Grey * */
  0xff50, /* (Alt) Home */
  0xff52, /* (Alt) Up */
  0xff55, /* (Alt) Page Up */
  0, /* NO KEY */
  0xff51, /* (Alt) Left */
  0, /* NO KEY */
  0xff53, /* (Alt) Right */
  0, /* NO KEY */
  0xff57, /* (Alt) End */

  /* --------------- a0 to af --------------- */
  0xff54, /* (Alt) Down */
  0xff56, /* (Alt) Page Down */
  0xff63, /* (Alt) Insert */
  0xffff, /* (Alt) Delete */
  0xffaf, /* (Alt) Grey / */
  0xff09, /* (Alt) Tab */
  0xff0d  /* (Alt) Enter */
};

/* Get a char from keyboard.  Function keys are put into the event queue.  */
static int
dos_rawgetc ()
{
  struct input_event event;
  struct timeval tv;
  union REGS regs;
  int ctrl_p, alt_p, shift_p;

  /* Calculate modifier bits */
  regs.h.ah = extended_kbd ? 0x12 : 0x02;
  int86 (0x16, &regs, &regs);
  ctrl_p = ((regs.h.al & 4) != 0);
  shift_p = ((regs.h.al & 3) != 0);
  /* Please be very careful here not to break international keyboard support.
     When Keyb.Com is loaded, the key marked `Alt Gr' is used for accessing
     characters like { and } if their positions are overlaid.  */
  alt_p = ((extended_kbd ? (regs.h.ah & 2) : (regs.h.al & 8)) != 0);

  /* The following condition is equivalent to `kbhit ()', except that
     it uses the bios to do its job.  This pleases DESQview/X.  */
  while ((regs.h.ah = extended_kbd ? 0x11 : 0x01),
	 int86 (0x16, &regs, &regs),
	 (regs.x.flags & 0x40) == 0)
    {
      union REGS regs;
      register unsigned char c;
      int sc, code;

      regs.h.ah = extended_kbd ? 0x10 : 0x00;
      int86 (0x16, &regs, &regs);
      c = regs.h.al;
      sc = regs.h.ah;

      /* Determine from the scan code if a keypad key was pressed.  */
      if (c >= '0' && c <= '9' && sc > 0xb)
	sc = (c == '0') ? 0xb : (c - '0' + 1), c = 0;
      else if (sc == 0x53 && c != 0xe0)
	{
	  code = 0xffae; /* Keypad decimal point/comma.  */
	  goto nonascii;
	}
      else if (sc == 0xe0)
	{
	  switch (c)
	    {
	    case 10: /* Ctrl Enter */
	    case 13:
	      sc = 0x1c;
	      break;
	    case '/':
	      sc = 0x35;
	      break;
	    default:
	      sc = 0;
	    };
	  c = 0;
	}

      if (c == 0
	  || c == ' '
	  || alt_p
	  || (ctrl_p && shift_p)
	  || (c == 0xe0 && sc != 0)     /* Pseudo-key */
	  || sc == 0x37                 /* Grey * */
	  || sc == 0x4a                 /* Grey - */
	  || sc == 0x4e                 /* Grey + */
	  || sc == 0x0e)                /* Back space *key*, not Ctrl-h */
      {
	if (sc >= (sizeof (ibmpc_translate_map) / sizeof (short)))
	  code = 0;
	else
	  code = ibmpc_translate_map[sc];
	if (code != 0)
	  {
	    if (code >= 0x100)
	      {
	      nonascii:
		event.kind = non_ascii_keystroke;
		event.code = (code & 0xff) + 0xff00;
	      }
	    else
	      {
		/* Don't return S- if we don't have to.  `shifted' is
		   supposed to be the shifted versions of the characters
		   in `unshifted'.  Unfortunately, this is only true for
		   US keyboard layout.  If anyone knows how to do this
		   right, please tell us.  */
		static char *unshifted
		  = "abcdefghijklmnopqrstuvwxyz,./=;[\\]'-`0123456789";
		static char *shifted
		  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ<>?+:{|}\"_~)!@#$%^&*(";
		char *pos;

		if (shift_p && (pos = strchr (unshifted, code)))
		  {
		    c = shifted[pos - unshifted];
		    shift_p = 0;
		  }
		else
		  if (c == 0) c = code;
		event.kind = ascii_keystroke;
		event.code = c;
	      }
	    event.modifiers
	      = (shift_p ? shift_modifier : 0)
		+ (ctrl_p ? ctrl_modifier : 0)
		  + (alt_p ? meta_modifier : 0);
	    /* EMACS == Enter Meta Alt Control Shift */
	    XSETFRAME (event.frame_or_window, selected_frame);
	    gettimeofday (&tv, NULL);
	    event.timestamp = tv.tv_usec;
	    kbd_buffer_store_event (&event);
	  }
      } else
	return c;
    }

  if (have_mouse)
    {
      int but, press, x, y, ok;

      /* Check for mouse movement *before* buttons.  */
      mouse_check_moved ();

      for (but = 0; but < NUM_MOUSE_BUTTONS; but++)
	for (press = 0; press < 2; press++)
	  {
	    if (press)
	      ok = mouse_pressed (but, &x, &y);
	    else
	      ok = mouse_released (but, &x, &y);
	    if (ok)
	      {
		event.kind = mouse_click;
		event.code = but;
		event.modifiers
		  = (shift_p ? shift_modifier : 0)
		    + (ctrl_p ? ctrl_modifier : 0)
		      + (alt_p ? meta_modifier : 0)
			+ (press ? down_modifier : up_modifier);
		event.x = x;
		event.y = y;
		XSETFRAME (event.frame_or_window, selected_frame);
		gettimeofday (&tv, NULL);
		event.timestamp = tv.tv_usec;
		kbd_buffer_store_event (&event);
	      }
	  }
    }

  return -1;
}

static int prev_get_char = -1;

/* Return 1 if a key is ready to be read without suspending execution.  */
dos_keysns ()
{
  if (prev_get_char != -1)
    return 1;
  else
    return ((prev_get_char = dos_rawgetc ()) != -1);
}

/* Read a key.  Return -1 if no key is ready.  */
dos_keyread ()
{
  if (prev_get_char != -1)
    {
      int c = prev_get_char;
      prev_get_char = -1;
      return c;
    }
  else
    return dos_rawgetc ();
}

/* Hostnames for a pc are not really funny, but they are used in change log
   so we emulate the best we can.  */
gethostname (p, size)
     char *p;
     int size;
{
  char *q = egetenv ("HOSTNAME");

  if (!q) q = "pc";
  strcpy (p, q);
  return 0;
}

/* Destructively turn backslashes into slashes.  */
void
dostounix_filename (p)
     register char *p;
{
  while (*p)
    {
      if (*p == '\\')
	*p = '/';
      p++;
    }
}

/* Destructively turn slashes into backslashes.  */
void
unixtodos_filename (p)
     register char *p;
{
  while (*p)
    {
      if (*p == '/')
	*p = '\\';
      p++;
    }
}

/* Get the default directory for a given drive.  0=def, 1=A, 2=B, ...  */
int
getdefdir (drive, dst)
     int drive;
     char *dst;
{
  union REGS regs;

  *dst++ = '/';
  regs.h.dl = drive;
  regs.x.si = (int) dst;
  regs.h.ah = 0x47;
  intdos (&regs, &regs);
  return !regs.x.cflag;
}

/* Remove all CR's that are followed by a LF.  */
int
crlf_to_lf (n, buf)
     register int n;
     register unsigned char *buf;
{
  unsigned char *np = buf;
  unsigned char *startp = buf;
  unsigned char *endp = buf + n;
  unsigned char c;

  if (n == 0)
    return n;
  while (buf < endp - 1)
    {
      if (*buf == 0x0d)
	{
	  if (*(++buf) != 0x0a)
	    *np++ = 0x0d;
	}
      else
	*np++ = *buf++;
    }
  if (buf < endp)
    *np++ = *buf++;
  return np - startp;
}


/* Run command as specified by ARGV in directory DIR.
   The command is run with input from TEMPIN and output to file TEMPOUT.  */
int
run_msdos_command (argv, dir, tempin, tempout)
     unsigned char **argv;
     Lisp_Object dir;
     int tempin, tempout;
{
  char *saveargv1, *saveargv2, **envv;
  char oldwd[MAXPATHLEN + 1]; /* Fixed size is safe on MSDOS.  */
  int msshell, result = -1;
  int in, out, inbak, outbak, errbak;
  Lisp_Object cmd;

  /* Get current directory as MSDOS cwd is not per-process.  */
  getwd (oldwd);

  cmd = Ffile_name_nondirectory (build_string (argv[0]));
  msshell = !NILP (Fmember (cmd, Fsymbol_value (intern ("msdos-shells"))))
    && !strcmp ("-c", argv[1]);
  if (msshell)
    {
      saveargv1 = argv[1];
      saveargv2 = argv[2];
      argv[1] = "/c";
      if (argv[2])
	{
	  char *p = alloca (strlen (argv[2]) + 1);

	  strcpy (argv[2] = p, saveargv2);
	  while (*p && isspace (*p))
	    p++;
	  while (*p && !isspace (*p))
	    if (*p == '/')
	      *p++ = '\\';
	    else
	      p++;
	}
    }

  /* Build the environment array.  */
  {
    extern Lisp_Object Vprocess_environment;
    Lisp_Object tmp, lst;
    int i, len;

    lst = Vprocess_environment;
    len = XFASTINT (Flength (lst));

    envv = alloca ((len + 1) * sizeof (char *));
    for (i = 0; i < len; i++)
      {
	tmp = Fcar (lst);
	lst = Fcdr (lst);
	CHECK_STRING (tmp, 0);
	envv[i] = alloca (XSTRING (tmp)->size + 1);
	strcpy (envv[i], XSTRING (tmp)->data);
      }
    envv[len] = (char *) 0;
  }

  if (STRINGP (dir))
    chdir (XSTRING (dir)->data);
  inbak = dup (0);
  outbak = dup (1);
  errbak = dup (2);
  if (inbak < 0 || outbak < 0 || errbak < 0)
    goto done; /* Allocation might fail due to lack of descriptors.  */
  dup2 (tempin, 0);
  dup2 (tempout, 1);
  dup2 (tempout, 2);
  dos_ttcooked ();
  result = spawnve (P_WAIT, argv[0], argv, envv);
  dos_ttraw ();
  dup2 (inbak, 0);
  dup2 (outbak, 1);
  dup2 (errbak, 2);

 done:
  chdir (oldwd);
  if (msshell)
    {
      argv[1] = saveargv1;
      argv[2] = saveargv2;
    }
  return result;
}


croak (badfunc)
     char *badfunc;
{
  fprintf (stderr, "%s not yet implemented\r\n", badfunc);
  reset_sys_modes ();
  exit (1);
}

/* A list of unimplemented functions that we silently ignore.  */
unsigned alarm (s) unsigned s; {}
fork () { return 0; }
int kill (x, y) int x, y; { return -1; }
nice (p) int p; {}
void volatile pause () {}
request_sigio () {}
setpgrp () {return 0; }
setpriority (x,y,z) int x,y,z; { return 0; }
sigsetmask (x) int x; { return 0; }
unrequest_sigio () {}

#ifdef chdir
#undef chdir
#endif

int
sys_chdir (path)
     const char* path;
{
  int len = strlen (path);
  char *tmp = (char *) alloca (len + 1);
  /* Gotta do this extern here due to the corresponding #define: */
  extern int chdir ();

  if (*path && path[1] == ':' && (getdisk () != tolower (path[0]) - 'a'))
    setdisk (tolower (path[0]) - 'a');

  strcpy (tmp, path);
  if (strcmp (path, "/") && strcmp (path + 1, ":/") && (path[len - 1] == '/'))
    tmp[len - 1] = 0;
  return chdir (tmp);
}

/* Sleep SECS.  If KBDOK also return immediately if a key is pressed.  */
void
sleep_or_kbd_hit (secs, kbdok)
     int secs, kbdok;
{
  long clnow, clthen;
  struct timeval t;

  gettimeofday (&t, NULL);
  clnow = t.tv_sec * 100 + t.tv_usec / 10000;
  clthen = clnow + (100 * secs);

  do
    {
      gettimeofday (&t, NULL);
      clnow = t.tv_sec * 100 + t.tv_usec / 10000;
      if (kbdok && detect_input_pending ())
	return;
    }
  while (clnow < clthen);
}

/* The Emacs root directory as determined by init_environment.  */
static char emacsroot[MAXPATHLEN];

char *
rootrelativepath (rel)
     char *rel;
{
  static char result[MAXPATHLEN + 10];

  strcpy (result, emacsroot);
  strcat (result, "/");
  strcat (result, rel);
  return result;
}

/* Define a lot of environment variables if not already defined.  Don't
   remove anything unless you know what you're doing -- lots of code will
   break if one or more of these are missing.  */
void
init_environment (argc, argv, skip_args)
     int argc;
     char **argv;
     int skip_args;
{
  char *s, *t, *root;
  int len;

  /* Find our root from argv[0].  Assuming argv[0] is, say,
     "c:/emacs/bin/emacs.exe" our root will be "c:/emacs".  */
  _fixpath (argv[0], root = alloca (MAXPATHLEN + 20));
  strlwr (root);
  len = strlen (root);
  while (len > 0 && root[len] != '/' && root[len] != ':')
    len--;
  root[len] = '\0';
  if (len > 4 && strcmp (root + len - 4, "/bin") == 0)
    root[len - 4] = '\0';
  else
    strcpy (root, "c:/emacs");  /* Only under debuggers, I think.  */
  len = strlen (root);
  strcpy (emacsroot, root);

  /* We default HOME to our root.  */
  setenv ("HOME", root, 0);

  /* We default EMACSPATH to root + "/bin".  */
  strcpy (root + len, "/bin");
  setenv ("EMACSPATH", root, 0);

  /* I don't expect anybody to ever use other terminals so the internal
     terminal is the default.  */
  setenv ("TERM", "internal", 0);

#ifdef HAVE_X_WINDOWS
  /* Emacs expects DISPLAY to be set.  */
  setenv ("DISPLAY", "unix:0.0", 0);
#endif

  /* SHELL is a bit tricky -- COMSPEC is the closest we come, but we must
     downcase it and mirror the backslashes.  */
  s = getenv ("COMSPEC");
  if (!s) s = "c:/command.com";
  t = alloca (strlen (s) + 1);
  strcpy (t, s);
  strlwr (t);
  dostounix_filename (t);
  setenv ("SHELL", t, 0);

  /* PATH is also downcased and backslashes mirrored.  */
  s = getenv ("PATH");
  if (!s) s = "";
  t = alloca (strlen (s) + 3);
  /* Current directory is always considered part of MsDos's path but it is
     not normally mentioned.  Now it is.  */
  strcat (strcpy (t, ".;"), s);
  strlwr (t);
  dostounix_filename (t); /* Not a single file name, but this should work.  */
  setenv ("PATH", t, 1);

  /* In some sense all dos users have root privileges, so...  */
  setenv ("USER", "root", 0);
  setenv ("NAME", getenv ("USER"), 0);

  /* Time zone determined from country code.  To make this possible, the
     country code may not span more than one time zone.  In other words,
     in the USA, you lose.  */
  switch (dos_country_code)
    {
    case 31: /* Belgium */
    case 32: /* The Netherlands */
    case 33: /* France */
    case 34: /* Spain */
    case 36: /* Hungary */
    case 38: /* Yugoslavia (or what's left of it?) */
    case 39: /* Italy */
    case 41: /* Switzerland */
    case 42: /* Tjekia */
    case 45: /* Denmark */
    case 46: /* Sweden */
    case 47: /* Norway */
    case 48: /* Poland */
    case 49: /* Germany */
      /* Daylight saving from last Sunday in March to last Sunday in
	 September, both at 2AM.  */
      setenv ("TZ", "MET-01METDST-02,M3.5.0/02:00,M9.5.0/02:00", 0);
      break;
    case 44: /* United Kingdom */
    case 351: /* Portugal */
    case 354: /* Iceland */
      setenv ("TZ", "GMT+00", 0);
      break;
    case 81: /* Japan */
    case 82: /* Korea */
      setenv ("TZ", "???-09", 0);
      break;
    case 90: /* Turkey */
    case 358: /* Finland */
    case 972: /* Israel */
      setenv ("TZ", "EET-02", 0);
      break;
    }
  tzset ();
  init_gettimeofday ();
}

/* Flash the screen as a substitute for BEEPs.  */

static void
do_visible_bell (xorattr)
     unsigned char xorattr;
{
  asm volatile
    ("  movb   $1,%%dl
visible_bell_0:
	movl   _ScreenPrimary,%%eax
	call   dosmemsetup
	movl   %%eax,%%ebx
	movl   %1,%%ecx
	movb   %0,%%al
	incl   %%ebx
visible_bell_1:
	xorb   %%al,%%gs:(%%ebx)
	addl   $2,%%ebx
	decl   %%ecx
	jne    visible_bell_1
	decb   %%dl
	jne    visible_bell_3
visible_bell_2:
	movzwl %%ax,%%eax
        movzwl %%ax,%%eax
	movzwl %%ax,%%eax
	movzwl %%ax,%%eax
	decw   %%cx
	jne    visible_bell_2
	jmp    visible_bell_0
visible_bell_3:"
     : /* no output */
     : "m" (xorattr), "g" (ScreenCols () * ScreenRows ())
     : "%eax", "%ebx", /* "%gs",*/ "%ecx", "%edx");
}

/* At screen position (X,Y), output C characters from string S with
   attribute A.  Do it fast!  */

static void
output_string (x, y, s, c, a)
     int x, y, c;
     unsigned char *s;
     unsigned char a;
{
  char *t = (char *)ScreenPrimary + 2 * (x + ScreenCols () * y);
  asm volatile
    ("  movl   %1,%%eax
        call   dosmemsetup
        movl   %%eax,%%edi
        movb   %0,%%ah
        movl   %2,%%ecx
        movl   %3,%%esi
output_string1:
        movb   (%%esi),%%al
        movw   %%ax,%%gs:(%%edi)
        addl   $2,%%edi
        incl   %%esi
        decl   %%ecx
        jne    output_string1"
     : /* no output */
     : "m" (a), "g" (t), "g" (c), "g" (s)
     : "%eax", "%ecx", /* "%gs",*/ "%esi", "%edi");
}

static int internal_terminal = 0;
static int highlight;

#undef fflush

int
internal_flush (f)
     FILE *f;
{
  static char spaces[] = "                                                                                ";
  static int x;
  static int y;
  unsigned char *cp, *cp0;
  int count, i, j;

  if (internal_terminal && f == stdout)
    {
      if (have_mouse) mouse_off ();
      cp = stdout->_base;
      count = stdout->_ptr - stdout->_base;
      while (count > 0)
	{
	  switch (*cp++)
	    {
	    case 27:
	      switch (*cp++)
		{
		case '@':
		  y = *cp++;
		  x = *cp++;
		  count -= 4;
		  break;
		case 'A':
		  ScreenAttrib = *cp++;
		  count -= 3;
		  break;
		case 'B':
		  do_visible_bell (*cp++);
		  count -= 3;
		  break;
		case 'C':
		  ScreenClear ();
		  x = y = 0;
		  count -= 2;
		  break;
		case 'E':
		  i = ScreenCols () - x;
		  j = x;
		  while (i >= sizeof spaces)
		    {
		      output_string (j, y, spaces, sizeof spaces,
				     ScreenAttrib);
		      j += sizeof spaces;
		      i -= sizeof spaces;
		    }
		  if (i > 0)
		    output_string (j, y, spaces, i, ScreenAttrib);
		  count -= 2;
		  break;
		case 'R':
		  x++;
		  count -= 2;
		  break;
		case 'U':
		  y--;
		  count -= 2;
		  break;
		case 'X':
		  ScreenAttrib ^= *cp++;
		  count -= 3;
		  break;
		default:
		  count -= 2;
		}
	      break;
	    case 7:
	      write (1, "\007", 1);
	      count--;
	      break;
	    case 8:
	      x--;
	      count--;
	      break;
	    case 13:
	      x = 0;
	      count--;
	      break;
	    case 10:
	      y++;
	      count--;
	      break;
	    default:
	      cp0 = cp - 1;
	      count--;
	      while (count > 0 && *cp >= ' ')
		cp++, count--;
	      output_string (x, y, cp0, cp - cp0, ScreenAttrib);
	      x += (cp - cp0);
	    }
	}
      fpurge (stdout);
      ScreenSetCursor (y, x);
      if (have_mouse) mouse_on ();
    }
  else
    /* This is a call to the original fflush.  */
    fflush (f);
}

#ifndef HAVE_X_WINDOWS
static void
rien_du_tout ()
{
  /* Rien du tout, cela va sans dire!  */
}

static
IT_ring_bell ()
{
  if (visible_bell)
    {
      /* This creates an xor-mask that will swap the default fore- and
	 background colors.  */
      if (have_mouse) mouse_off ();
      do_visible_bell (((the_only_x_display.foreground_pixel
			 ^ the_only_x_display.background_pixel)
			* 0x11) & 0x7f);
      if (have_mouse) mouse_on ();
    }
  else
    /* Write it directly to ms-dos -- don't let it go through our terminal
       emulator.  This way the mouse cursor won't blink.  */
    write (1, "\007", 1);
}

static void
IT_set_face (int face)
{
  struct face *fp;
  extern struct face *intern_face (/* FRAME_PTR, struct face * */);

  if (face == 1 || (face == 0 && highlight))
    fp = FRAME_MODE_LINE_FACE (foo);
  else if (face <= 0 || face >= FRAME_N_COMPUTED_FACES (foo))
    fp = FRAME_DEFAULT_FACE (foo);
  else
    fp = intern_face (selected_frame, FRAME_COMPUTED_FACES (foo)[face]);
  putchar ('\e');
  putchar ('A');
  putchar ((FACE_BACKGROUND (fp) << 4) | FACE_FOREGROUND (fp));
}

static
IT_write_glyphs (GLYPH *str, int len)
{
  int face = -1;
  int newface;

  while (len > 0)
    {
      newface = FAST_GLYPH_FACE (*str);
      if (newface != face)
	IT_set_face ((face = newface));
      putchar (FAST_GLYPH_CHAR (*str));
      str++, len--;
    }
}

static
IT_clear_end_of_line (first_unused)
{
  putchar ('\e');
  putchar ('E');
}

static
IT_cursor_to (int y, int x)
{
  putchar ('\e');
  putchar ('@');
  putchar (y);
  putchar (x);
}

IT_reassert_line_highlight (new, vpos)
     int new, vpos;
{
  highlight = new;
  IT_set_face (0); /* To possibly clear the highlighting.  */
}

static
IT_change_line_highlight (new_highlight, vpos, first_unused_hpos)
{
  highlight = new_highlight;
  IT_set_face (0); /* To possibly clear the highlighting.  */
  IT_cursor_to (vpos, 0);
  IT_clear_end_of_line (first_unused_hpos);
}

static
IT_update_begin ()
{
  highlight = 0;
  IT_set_face (0); /* To possibly clear the highlighting.  */
}

/* This was more or less copied from xterm.c */
static void
IT_set_menu_bar_lines (window, n)
  Lisp_Object window;
  int n;
{
  struct window *w = XWINDOW (window);

  XSETFASTINT (w->top, XFASTINT (w->top) + n);
  XSETFASTINT (w->height, XFASTINT (w->height) - n);

  /* Handle just the top child in a vertical split.  */
  if (!NILP (w->vchild))
    IT_set_menu_bar_lines (w->vchild, n);

  /* Adjust all children in a horizontal split.  */
  for (window = w->hchild; !NILP (window); window = w->next)
    {
      w = XWINDOW (window);
      IT_set_menu_bar_lines (window, n);
    }
}

void
IT_set_frame_parameters (frame, alist)
     FRAME_PTR frame;
     Lisp_Object alist;
{
  Lisp_Object tail;
  int redraw;
  extern unsigned long load_color ();
  FRAME_PTR f = (FRAME_PTR) &the_only_frame;

  redraw = 0;
  for (tail = alist; CONSP (tail); tail = Fcdr (tail))
    {
      Lisp_Object elt, prop, val;

      elt = Fcar (tail);
      prop = Fcar (elt);
      val = Fcdr (elt);
      CHECK_SYMBOL (prop, 1);

      if (EQ (prop, intern ("foreground-color")))
	{
	  unsigned long new_color = load_color (f, val);
	  if (new_color != ~0)
	    {
	      FRAME_FOREGROUND_PIXEL (f) = new_color;
	      redraw = 1;
	    }
	}
      else if (EQ (prop, intern ("background-color")))
	{
	  unsigned long new_color = load_color (f, val);
	  if (new_color != ~0)
	    {
	      FRAME_BACKGROUND_PIXEL (f) = new_color & ~8;
	      redraw = 1;
	    }
	}
      else if (EQ (prop, intern ("menu-bar-lines")))
	{
	  int new;
	  int old = FRAME_MENU_BAR_LINES (the_only_frame);

	  if (INTEGERP (val))
	    new = XINT (val);
	  else
	    new = 0;
	  FRAME_MENU_BAR_LINES (f) = new;
	  IT_set_menu_bar_lines (the_only_frame.root_window, new - old);
	}
    }

  if (redraw)
    {
      recompute_basic_faces (f);
      Fredraw_frame (Fselected_frame ());
    }
}

/* Similar to the_only_frame.  */
struct x_display the_only_x_display;

/* This is never dereferenced.  */
Display *x_current_display;

#endif /* !HAVE_X_WINDOWS */

/* Do we need the internal terminal? */
void
internal_terminal_init ()
{
  char *term = getenv ("TERM");

#ifdef HAVE_X_WINDOWS
  if (!inhibit_window_system)
    return;
#endif

  internal_terminal
    = (!noninteractive) && term && !strcmp (term, "internal");

#ifndef HAVE_X_WINDOWS
  if (internal_terminal && !inhibit_window_system)
    {
      Vwindow_system = intern ("pc");
      Vwindow_system_version = make_number (1);
 
      bzero (&the_only_x_display, sizeof the_only_x_display);
      the_only_x_display.background_pixel = 7; /* White */
      the_only_x_display.foreground_pixel = 0; /* Black */
      the_only_x_display.line_height = 1;
      the_only_frame.display.x = &the_only_x_display;
      the_only_frame.output_method = output_msdos_raw;

      init_frame_faces ((FRAME_PTR) &the_only_frame);

      ring_bell_hook = IT_ring_bell;
      write_glyphs_hook = IT_write_glyphs;
      cursor_to_hook = raw_cursor_to_hook = IT_cursor_to;
      clear_end_of_line_hook = IT_clear_end_of_line;
      change_line_highlight_hook = IT_change_line_highlight;
      update_begin_hook = IT_update_begin;
      reassert_line_highlight_hook = IT_reassert_line_highlight;

      /* These hooks are called by term.c without being checked.  */
      set_terminal_modes_hook
	= reset_terminal_modes_hook
	  = update_end_hook
	    = set_terminal_window_hook
	      = (void *)rien_du_tout;
    }
  else
    the_only_frame.output_method = output_termcap;
#endif
}

/* When time zones are set from Ms-Dos too may C-libraries are playing
   tricks with time values.  We solve this by defining our own version
   of `gettimeofday' bypassing GO32.  Our version needs to be initialized
   once and after each call to `tzset' with TZ changed.  */

static int daylight, gmtoffset;

int
gettimeofday (struct timeval *tp, struct timezone *tzp)
{
  if (tp)
    {
      struct time t;
      struct date d;
      struct tm tmrec;

      gettime (&t);
      getdate (&d);
      tmrec.tm_year = d.da_year - 1900;
      tmrec.tm_mon = d.da_mon - 1;
      tmrec.tm_mday = d.da_day;
      tmrec.tm_hour = t.ti_hour;
      tmrec.tm_min = t.ti_min;
      tmrec.tm_sec = t.ti_sec;
      tmrec.tm_gmtoff = gmtoffset;
      tmrec.tm_isdst = daylight;
      tp->tv_sec = mktime (&tmrec);
      tp->tv_usec = t.ti_hund * (1000000 / 100);
    }
  if (tzp)
    {
      tzp->tz_minuteswest = gmtoffset;
      tzp->tz_dsttime = daylight;
    }
  return 0;
}

void
init_gettimeofday ()
{
  time_t ltm, gtm;
  struct tm *lstm;

  daylight = 0;
  gmtoffset = 0;
  ltm = gtm = time (NULL);
  ltm = mktime (lstm = localtime (&ltm));
  gtm = mktime (gmtime (&gtm));
  daylight = lstm->tm_isdst;
  gmtoffset = (int)(gtm - ltm) / 60;
}

/* These must be global.  */
static _go32_dpmi_seginfo ctrl_break_vector;
static _go32_dpmi_registers ctrl_break_regs;
static int ctrlbreakinstalled = 0;

/* Interrupt level detection of Ctrl-Break.  Don't do anything fancy here!  */
void
ctrl_break_func (regs)
     _go32_dpmi_registers *regs;
{
  Vquit_flag = Qt;
}

void
install_ctrl_break_check ()
{
  if (!ctrlbreakinstalled)
    {
      /* Don't press Ctrl-Break if you don't have either DPMI or Emacs
	 was compiler with Djgpp 1.11 maintenance level 5 or later!  */
      ctrlbreakinstalled = 1;
      ctrl_break_vector.pm_offset = (int) ctrl_break_func;
      _go32_dpmi_allocate_real_mode_callback_iret (&ctrl_break_vector,
						   &ctrl_break_regs);
      _go32_dpmi_set_real_mode_interrupt_vector (0x1b, &ctrl_break_vector);
    }
}

/* Mouse routines follow.  Coordinates are in screen positions and zero
   based.  Mouse buttons are numbered from left to right and also zero
   based.  */

static int mouse_button_translate[NUM_MOUSE_BUTTONS];
static int mouse_button_count;

void
mouse_init ()
{
  union REGS regs;

  regs.x.ax = 0x0007;
  regs.x.cx = 0;
  regs.x.dx = 8 * (ScreenCols () - 1);
  int86 (0x33, &regs, &regs);

  regs.x.ax = 0x0008;
  regs.x.cx = 0;
  regs.x.dx = 8 * (ScreenRows () - 1);
  int86 (0x33, &regs, &regs);

  mouse_moveto (ScreenCols () - 1, ScreenRows () - 1);
  mouse_on ();
}

void
mouse_on ()
{
  union REGS regs;

  regs.x.ax = 0x0001;
  int86 (0x33, &regs, &regs);
}

void
mouse_off ()
{
  union REGS regs;

  regs.x.ax = 0x0002;
  int86 (0x33, &regs, &regs);
}

void
mouse_moveto (x, y)
     int x, y;
{
  union REGS regs;

  regs.x.ax = 0x0004;
  mouse_last_x = regs.x.cx = x * 8;
  mouse_last_y = regs.x.dx = y * 8;
  int86 (0x33, &regs, &regs);
}

int
mouse_pressed (b, xp, yp)
     int b, *xp, *yp;
{
  union REGS regs;

  if (b >= mouse_button_count)
    return 0;
  regs.x.ax = 0x0005;
  regs.x.bx = mouse_button_translate[b];
  int86 (0x33, &regs, &regs);
  if (regs.x.bx)
    *xp = regs.x.cx / 8, *yp = regs.x.dx / 8;
  return (regs.x.bx != 0);
}

int
mouse_released (b, xp, yp)
     int b, *xp, *yp;
{
  union REGS regs;

  if (b >= mouse_button_count)
    return 0;
  regs.x.ax = 0x0006;
  regs.x.bx = mouse_button_translate[b];
  int86 (0x33, &regs, &regs);
  if (regs.x.bx)
    *xp = regs.x.cx / 8, *yp = regs.x.dx / 8;
  return (regs.x.bx != 0);
}

static void
mouse_get_xy (int *x, int *y)
{
  union REGS regs;

  regs.x.ax = 0x0003;
  int86 (0x33, &regs, &regs);
  *x = regs.x.cx / 8;
  *y = regs.x.dx / 8;
}

void
mouse_get_pos (f, bar_window, part, x, y, time)
     FRAME_PTR *f;
     Lisp_Object *bar_window, *x, *y;
     enum scroll_bar_part *part;
     unsigned long *time;
{
  int ix, iy;
  union REGS regs;
  struct timeval tv;

  regs.x.ax = 0x0003;
  int86 (0x33, &regs, &regs);
  *f = selected_frame;
  *bar_window = Qnil;
  gettimeofday (&tv, NULL);
  mouse_get_xy (&ix, &iy);
  mouse_moved = 0;
  *x = make_number (ix);
  *y = make_number (iy);
  *time = tv.tv_usec;
}

void
mouse_check_moved ()
{
  int x, y;

  mouse_get_xy (&x, &y);
  mouse_moved |= (x != mouse_last_x || y != mouse_last_y);
  mouse_last_x = x;
  mouse_last_y = y;
}

int
mouse_init1 ()
{
  union REGS regs;
  int present;

#ifdef HAVE_X_WINDOWS
  if (!inhibit_window_system)
    return 0;
#endif
  if (!internal_terminal)
    return 0;

  regs.x.ax = 0x0021;
  int86 (0x33, &regs, &regs);
  present = (regs.x.ax & 0xffff) == 0xffff;
  if (!present)
    {
      /* Reportedly, the above doesn't work for some mouse drivers.  There
	 is an additional detection method that should work, but might be
	 a little slower.  Use that as an alternative.  */
      regs.x.ax = 0x0000;
      int86 (0x33, &regs, &regs);
      present = (regs.x.ax & 0xffff) == 0xffff;
    }

  if (present)
    {
      if (regs.x.bx == 3)
	{
	  mouse_button_count = 3;
	  mouse_button_translate[0] = 0; /* Left */
	  mouse_button_translate[1] = 2; /* Middle */
	  mouse_button_translate[2] = 1; /* Right */
	}
      else
	{
	  mouse_button_count = 2;
	  mouse_button_translate[0] = 0;
	  mouse_button_translate[1] = 1;
	}
      mouse_position_hook = &mouse_get_pos;
      mouse_init ();
   }
  return present;
}

#ifndef HAVE_X_WINDOWS
/* See xterm.c for more info.  */
void
pixel_to_glyph_coords (f, pix_x, pix_y, x, y, bounds, noclip)
     FRAME_PTR f;
     register int pix_x, pix_y;
     register int *x, *y;
     void /* XRectangle */ *bounds;
     int noclip;
{
  if (bounds) abort ();

  /* Ignore clipping.  */

  *x = pix_x;
  *y = pix_y;
}

void
glyph_to_pixel_coords (f, x, y, pix_x, pix_y)
     FRAME_PTR f;
     register int x, y;
     register int *pix_x, *pix_y;
{
  *pix_x = x;
  *pix_y = y;
}

/* Simulation of X's menus.  Nothing too fancy here -- just make it work
   for now.

   Actually, I don't know the meaning of all the parameters of the functions
   here -- I only know how they are called by xmenu.c.  I could of course
   grab the nearest Xlib manual (down the hall, second-to-last door on the
   left), but I don't think it's worth the effort.  */

static XMenu *
IT_menu_create ()
{
  XMenu *menu;

  menu = (XMenu *) xmalloc (sizeof (XMenu));
  menu->allocated = menu->count = menu->panecount = menu->width = 0;
  return menu;
}

/* Allocate some (more) memory for MENU ensuring that there is room for one
   for item.  */
static void
IT_menu_make_room (XMenu *menu)
{
  if (menu->allocated == 0)
    {
      int count = menu->allocated = 10;
      menu->text = (char **) xmalloc (count * sizeof (char *));
      menu->submenu = (XMenu **) xmalloc (count * sizeof (XMenu *));
      menu->panenumber = (int *) xmalloc (count * sizeof (int));
    }
  else if (menu->allocated == menu->count)
    {
      int count = menu->allocated = menu->allocated + 10;
      menu->text
	= (char **) xrealloc (menu->text, count * sizeof (char *));
      menu->submenu
	= (XMenu **) xrealloc (menu->submenu, count * sizeof (XMenu *));
      menu->panenumber
	= (int *) xrealloc (menu->panenumber, count * sizeof (int));
    }
}

/* Search the given menu structure for a given pane number.  */
static XMenu *
IT_menu_search_pane (XMenu *menu, int pane)
{
  int i;
  XMenu *try;

  for (i = 0; i < menu->count; i++)
    if (menu->submenu[i])
      if (pane == menu->panenumber[i])
	return menu->submenu[i];
      else
	if ((try = IT_menu_search_pane (menu->submenu[i], pane)))
	  return try;
  return (XMenu *) 0;
}

/* Determine how much screen space a given menu needs.  */
static void
IT_menu_calc_size (XMenu *menu, int *width, int *height)
{
  int i, h2, w2, maxsubwidth, maxheight;

  maxsubwidth = 0;
  maxheight = menu->count;
  for (i = 0; i < menu->count; i++)
    {
      if (menu->submenu[i])
	{
	  IT_menu_calc_size (menu->submenu[i], &w2, &h2);
	  if (w2 > maxsubwidth) maxsubwidth = w2;
	  if (i + h2 > maxheight) maxheight = i + h2;
	}
    }
  *width = menu->width + maxsubwidth;
  *height = maxheight;
}

/* Display MENU at (X,Y) using FACES. */
static void
IT_menu_display (XMenu *menu, int y, int x, int *faces)
{
  int i, j, face, width;
  GLYPH *text, *p;
  char *q;
  int mx, my;
  int enabled, mousehere;
  int row, col;

  width = menu->width;
  text = (GLYPH *) xmalloc ((width + 2) * sizeof (GLYPH));
  ScreenGetCursor (&row, &col);
  mouse_get_xy (&mx, &my);
  mouse_off ();
  (*update_begin_hook) ();
  for (i = 0; i < menu->count; i++)
    {
      (*cursor_to_hook) (y + i, x);
      enabled
	= (!menu->submenu[i] && menu->panenumber[i]) || (menu->submenu[i]);
      mousehere = (y + i == my && x <= mx && mx < x + width + 2);
      face = faces[enabled + mousehere * 2];
      p = text;
      *p++ = FAST_MAKE_GLYPH (' ', face);
      for (j = 0, q = menu->text[i]; *q; j++)
	*p++ = FAST_MAKE_GLYPH (*q++, face);
      for (; j < width; j++)
	*p++ = FAST_MAKE_GLYPH (' ', face);
      *p++ = FAST_MAKE_GLYPH (menu->submenu[i] ? 16 : ' ', face);
      (*write_glyphs_hook) (text, width + 2);
    }
  internal_flush (stdout);
  (*update_end_hook) ();
  mouse_on ();
  ScreenSetCursor (row, col);
  xfree (text);
}

/* Create a brand new menu structure.  */
XMenu *
XMenuCreate (Display *foo1, Window foo2, char *foo3)
{
  return IT_menu_create ();
}

/* Create a new pane and place it on the outer-most level.  It is not
   clear that it should be placed out there, but I don't know what else
   to do.  */
int
XMenuAddPane (Display *foo, XMenu *menu, char *txt, int enable)
{
  int len;

  if (!enable)
    abort ();

  IT_menu_make_room (menu);
  menu->submenu[menu->count] = IT_menu_create ();
  menu->text[menu->count] = txt;
  menu->panenumber[menu->count] = ++menu->panecount;
  menu->count++;
  if ((len = strlen (txt)) > menu->width) menu->width = len;
  return menu->panecount;
}

/* Create a new item in a menu pane.  */
int
XMenuAddSelection (Display *bar, XMenu *menu, int pane,
		   int foo, char *txt, int enable)
{
  int len;

  if (pane)
    if (!(menu = IT_menu_search_pane (menu, pane)))
      return XM_FAILURE;
  IT_menu_make_room (menu);
  menu->submenu[menu->count] = (XMenu *) 0;
  menu->text[menu->count] = txt;
  menu->panenumber[menu->count] = enable;
  menu->count++;
  if ((len = strlen (txt)) > menu->width) menu->width = len;
  return XM_SUCCESS;
}

/* Decide where the menu would be placed if requested at (X,Y).  */
void
XMenuLocate (Display *foo0, XMenu *menu, int foo1, int foo2, int x, int y,
	     int *ulx, int *uly, int *width, int *height)
{
  if (menu->count == 1 && menu->submenu[0])
      /* Special case: the menu consists of only one pane.  */
    IT_menu_calc_size (menu->submenu[0], width, height);
  else
    IT_menu_calc_size (menu, width, height);
  *ulx = x + 1;
  *uly = y;
  *width += 2;
}

typedef struct
{
  void *screen_behind;
  XMenu *menu;
  int pane;
  int x, y;
} IT_menu_state;


/* Display menu, wait for user's response, and return that response.  */
int
XMenuActivate (Display *foo, XMenu *menu, int *pane, int *selidx,
	       int x0, int y0, unsigned ButtonMask, char **txt)
{
  IT_menu_state *state;
  int statecount;
  int x, y, i, b;
  int screensize;
  int faces[4], selectface;
  int leave, result, onepane;

  /* Just in case we got here without a mouse present...  */
  if (!have_mouse)
    return XM_IA_SELECT;

  state = alloca (menu->panecount * sizeof (IT_menu_state));
  screensize = ScreenRows () * ScreenCols () * 2;
  faces[0]
    = compute_glyph_face (&the_only_frame,
			  face_name_id_number
			  (&the_only_frame,
			   intern ("msdos-menu-passive-face")),
			  0);
  faces[1]
    = compute_glyph_face (&the_only_frame,
			  face_name_id_number
			  (&the_only_frame,
			   intern ("msdos-menu-active-face")),
			  0);
  selectface
    = face_name_id_number (&the_only_frame, intern ("msdos-menu-select-face"));
  faces[2] = compute_glyph_face (&the_only_frame, selectface, faces[0]);
  faces[3] = compute_glyph_face (&the_only_frame, selectface, faces[1]);

  statecount = 1;
  state[0].menu = menu;
  mouse_off ();
  ScreenRetrieve (state[0].screen_behind = xmalloc (screensize));
  mouse_on ();
  if ((onepane = menu->count == 1 && menu->submenu[0]))
    {
      menu->width = menu->submenu[0]->width;
      state[0].menu = menu->submenu[0];
    }
  else
    {
      state[0].menu = menu;
    }
  state[0].x = x0 - 1;
  state[0].y = y0;
  state[0].pane = onepane;

  mouse_last_x = -1;  /* A hack that forces display.  */
  leave = 0;
  while (!leave)
    {
      mouse_check_moved ();
      if (mouse_moved)
	{
	  mouse_moved = 0;
	  result = XM_IA_SELECT;
	  mouse_get_xy (&x, &y);
	  for (i = 0; i < statecount; i++)
	    if (state[i].x <= x && x < state[i].x + state[i].menu->width + 2)
	      {
		int dy = y - state[i].y;
		if (0 <= dy && dy < state[i].menu->count)
		  {
		    if (!state[i].menu->submenu[dy])
		      if (state[i].menu->panenumber[dy])
			result = XM_SUCCESS;
		      else
			result = XM_IA_SELECT;
		    *pane = state[i].pane - 1;
		    *selidx = dy;
		    /* We hit some part of a menu, so drop extra menues that
		       have been opened.  That does not include an open and
		       active submenu.  */
		    if (i != statecount - 2
			|| state[i].menu->submenu[dy] != state[i+1].menu)
		      while (i != statecount - 1)
			{
			  statecount--;
			  mouse_off ();
			  ScreenUpdate (state[statecount].screen_behind);
			  mouse_on ();
			  xfree (state[statecount].screen_behind);
			}
		    if (i == statecount - 1 && state[i].menu->submenu[dy])
		      {
			IT_menu_display (state[i].menu,
					 state[i].y,
					 state[i].x,
					 faces);
			state[statecount].menu = state[i].menu->submenu[dy];
			state[statecount].pane = state[i].menu->panenumber[dy];
			mouse_off ();
			ScreenRetrieve (state[statecount].screen_behind
					= xmalloc (screensize));
			mouse_on ();
			state[statecount].x
			  = state[i].x + state[i].menu->width + 2;
			state[statecount].y = y;
			statecount++;			  
		      }
		  }
	      }
	  IT_menu_display (state[statecount - 1].menu,
			   state[statecount - 1].y,
			   state[statecount - 1].x,
			   faces);
	}
      for (b = 0; b < mouse_button_count; b++)
	{
	  (void) mouse_pressed (b, &x, &y);
	  if (mouse_released (b, &x, &y))
	    leave = 1;
	}
    }

  mouse_off ();
  ScreenUpdate (state[0].screen_behind);
  mouse_on ();
  while (statecount--)
    xfree (state[statecount].screen_behind);
  return result;
}

/* Dispose of a menu.  */
void
XMenuDestroy (Display *foo, XMenu *menu)
{
  int i;
  if (menu->allocated)
    {
      for (i = 0; i < menu->count; i++)
	if (menu->submenu[i])
	  XMenuDestroy (foo, menu->submenu[i]);
      xfree (menu->text);
      xfree (menu->submenu);
      xfree (menu->panenumber);
    }
  xfree (menu);
}

int x_pixel_width (struct frame *f)
{
  return FRAME_WIDTH(f);
}

int x_pixel_height (struct frame *f)
{
  return FRAME_HEIGHT(f);
}
#endif /* !HAVE_X_WINDOWS */

#endif /* MSDOS */
