/* MS-DOS specific C utilities.  Coded by Morten Welinder 1993
   Copyright (C) 1993 Free Software Foundation, Inc.

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

/* Note: some of the stuff here was taken from end of sysdep.c in demacs. */

#include "config.h"

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
#include "frame.h"
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
  have_mouse = Mouse_init1 ();

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
  if (have_mouse) Mouse_off ();

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
  alt_p = ((extended_kbd ? (regs.h.ah & 2) : (regs.h.al & 8)) != 0);

  while (kbhit ())
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
      else if (sc == 0xe0)
	{
	  switch (c)
	    {
	    case 10: /* Ctrl Enter */
	    case 13:
	      sc = 0x1c;
	      break;
	    case '.': /* Decimal point or decimal comma */
	    case ',':
	      sc = 0x53;
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
		event.kind = non_ascii_keystroke;
		event.code = code & 0xff;
	      }
	    else
	      {
		/* Don't return S- if we don't have to.  */
		if (code >= 'a' && code <= 'z')
		  {
		    c = shift_p ? toupper (code) : code;
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
	    event.frame_or_window = selected_frame;
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
      Mouse_check_moved ();

      for (but = 0; but < NUM_MOUSE_BUTTONS; but++)
	for (press = 0; press < 2; press++)
	  {
	    if (press)
	      ok = Mouse_pressed (but, &x, &y);
	    else
	      ok = Mouse_released (but, &x, &y);
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
		event.frame_or_window = selected_frame;
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
  while (buf < endp)
    {
      if (*buf == 0x0d)
	{
	  if (*(++buf) != 0x0a)
	    *np++ = 0x0d;
	}
      else
	*np++ = *buf++;
    }
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
      argv[1] = "/c";
      if (argv[2])
	{
	  saveargv2 = argv[2];
	  unixtodos_filename (argv[2] = strdup (argv[2]));
	}
    }

  /* Build the environment array.  */
  {
    extern Lisp_Object Vprocess_environment;
    Lisp_Object tmp, lst = Vprocess_environment;
    int i, len = XFASTINT (Flength (lst));

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

  if (XTYPE (dir) == Lisp_String)
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
      if (argv[2])
	{
	  free (argv[2]);
	  argv[2] = saveargv2;
	}
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

/* Define a lot of environment variables if not already defined.  Don't
   remove anything unless you know what you're doing -- lots of code will
   break if one or more of these are missing.  */
void
init_environment (argc, argv, skip_args)
     int argc;
     char **argv;
     int skip_args;
{
  char *s, *t;

  /* We default HOME to the directory from which Emacs was started, but with
     a "/bin" suffix removed.  */
  s = argv[0];
  t = alloca (strlen (s) + 1);
  strcpy (t, s);
  s = t + strlen (t);
  while (s != t && *s != '/' && *s != ':') s--;
  if (s == t)
    t = "c:/emacs"; /* When run under debug32.  */
  else
    {
      if (*s == ':') s++;
      *s = 0;
      if (s - 4 >= t && strcmp (s - 4, "/bin") == 0)
	s[strlen (s) - 4] = 0;
    }
  setenv ("HOME", t, 0);

  /* We set EMACSPATH to ~/bin (expanded) */
  s = getenv ("HOME");
  t = strcpy (alloca (strlen (s) + 6), s);
  if (s[strlen (s) - 1] != '/') strcat (t, "/");
  strcat (t, "bin");
  setenv ("EMACSPATH", t, 0);

  /* I don't expect anybody to ever use other terminals so the internal
     terminal is the default.  */
  setenv ("TERM", "internal", 0);

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
}

/* Flash the screen as a substitute for BEEPs.  */

static unsigned char _xorattr;

void
visible_bell (xorattr)
     unsigned char xorattr;
{
  _xorattr = xorattr;
  asm ("  pushl  %eax
	  pushl  %ebx
	  pushl  %ecx
	  pushl  %edx
	  movl   $1,%edx
visible_bell_0:
	  movl   _ScreenPrimary,%eax
	  call   dosmemsetup
	  movl   %eax,%ebx
	  call   _ScreenRows
	  movl   %eax,%ecx
	  call   _ScreenCols
	  imull  %eax,%ecx
	  movb   (__xorattr),%al
	  incl   %ebx
visible_bell_1:
	  xorb   %al,%gs:(%ebx)
	  addl   $2,%ebx
	  decl   %ecx
	  jne    visible_bell_1
	  decl   %edx
	  jne    visible_bell_3
visible_bell_2:
	  movzwl %ax,%eax
          movzwl %ax,%eax
	  movzwl %ax,%eax
	  movzwl %ax,%eax
	  decw   %cx
	  jne    visible_bell_2
	  jmp    visible_bell_0
visible_bell_3:
	  popl  %edx
	  popl  %ecx
	  popl  %ebx
	  popl  %eax");
}

static int internal_terminal = 0;
#undef fflush

int
internal_flush (f)
     FILE *f;
{
  static int x;
  static int y;
  char c, *cp;
  int count, i;

  if (internal_terminal && f == stdout)
    {
      if (have_mouse) Mouse_off ();
      cp = stdout->_base;
      count = stdout->_ptr - stdout->_base;
      while (count > 0)
	{
	  switch (c = *cp++)
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
		  visible_bell (*cp++);
		  count -= 3;
		  break;
		case 'C':
		  ScreenClear ();
		  x = y = 0;
		  count -= 2;
		  break;
		case 'E':
		  for (i = ScreenCols () - 1; i >= x; i--)
		    ScreenPutChar (' ', ScreenAttrib, i, y);
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
	      ScreenPutChar (c, ScreenAttrib, x++, y);
	      count--;
	    }
	}
      fpurge (stdout);
      ScreenSetCursor (y, x);
      if (have_mouse) Mouse_on ();
    }
  else
    /* This is a call to the original fflush.  */
    fflush (f);
}

/* Do we need the internal terminal? */
void
internal_terminal_init ()
{
  char *term = getenv ("TERM");

  internal_terminal
    = (!noninteractive) && term && !strcmp (term, "internal");
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
	 was compiler with Djgpp 1.11 maintenance level 2 or later!  */
      ctrlbreakinstalled = 1;
      ctrl_break_vector.pm_offset = (int) ctrl_break_func;
      _go32_dpmi_allocate_real_mode_callback_iret (&ctrl_break_vector,
						   &ctrl_break_regs);
      _go32_dpmi_set_real_mode_interrupt_vector (0x1b, &ctrl_break_vector);
    }
}


/* Mouse routines under devellopment follow.  Coordinates are in screen
   positions and zero based.  Mouse buttons are numbered from left to 
   right and also zero based.  */

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
    *xp = regs.x.cx / 8, *yp = regs.x.dx /8;
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

void
mouse_get_pos (f, bar_window, part, x, y, time)
     FRAME_PTR *f;
     Lisp_Object *bar_window, *x, *y;
     enum scroll_bar_part *part;
     unsigned long *time;
{
  union REGS regs;
  struct timeval tv;

  regs.x.ax = 0x0003;
  int86 (0x33, &regs, &regs);
  *f = selected_frame;
  *bar_window = Qnil;
  gettimeofday (&tv, NULL);
  *x = make_number (regs.x.cx / 8);
  *y = make_number (regs.x.dx / 8);
  *time = tv.tv_usec;
  mouse_moved = 0;
}

void
mouse_check_moved ()
{
  union REGS regs;

  regs.x.ax = 0x0003;
  int86 (0x33, &regs, &regs);
  if (regs.x.cx != mouse_last_x || regs.x.dx != mouse_last_y)
    {
      mouse_moved = 1;
      mouse_last_x = regs.x.cx;
      mouse_last_y = regs.x.dx;
    }
}

int
mouse_init1 ()
{
  union REGS regs;
  int present;

  regs.x.ax = 0x0021;
  int86 (0x33, &regs, &regs);
  present = internal_terminal && (regs.x.ax & 0xffff) == 0xffff;
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

#endif /* MSDOS */
