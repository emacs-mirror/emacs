/* Keyboard and mouse input; editor command loop.
   Copyright (C) 1985,86,87,88,89,93,94,95 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Allow config.h to undefine symbols found here.  */
#include <signal.h>

#include <config.h>
#include <stdio.h>
#include "termchar.h"
#include "termopts.h"
#include "lisp.h"
#include "termhooks.h"
#include "macros.h"
#include "frame.h"
#include "window.h"
#include "commands.h"
#include "buffer.h"
#include "disptab.h"
#include "dispextern.h"
#include "keyboard.h"
#include "intervals.h"
#include "blockinput.h"
#include <setjmp.h>
#include <errno.h>

#ifdef MSDOS
#include "msdos.h"
#include <time.h>
#else /* not MSDOS */
#ifndef VMS
#include <sys/ioctl.h>
#endif
#endif /* not MSDOS */

#include "syssignal.h"
#include "systty.h"

/* This is to get the definitions of the XK_ symbols.  */
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif

/* Include systime.h after xterm.h to avoid double inclusion of time.h. */
#include "systime.h"

extern int errno;

/* Variables for blockinput.h: */

/* Non-zero if interrupt input is blocked right now.  */
int interrupt_input_blocked;

/* Nonzero means an input interrupt has arrived
   during the current critical section.  */
int interrupt_input_pending;


/* File descriptor to use for input.  */
extern int input_fd;

#ifdef HAVE_X_WINDOWS
/* Make all keyboard buffers much bigger when using X windows.  */
#define KBD_BUFFER_SIZE 4096
#else	/* No X-windows, character input */
#define KBD_BUFFER_SIZE 256
#endif	/* No X-windows */

/* Following definition copied from eval.c */

struct backtrace
  {
    struct backtrace *next;
    Lisp_Object *function;
    Lisp_Object *args;	/* Points to vector of args. */
    int nargs;		/* length of vector.  If nargs is UNEVALLED,
			   args points to slot holding list of
			   unevalled args */
    char evalargs;
  };

#ifdef MULTI_PERDISPLAY
PERDISPLAY *current_perdisplay;
PERDISPLAY *all_perdisplays;
int display_locked;
#else
PERDISPLAY the_only_perdisplay;
#endif

/* Non-nil disable property on a command means
   do not execute it; call disabled-command-hook's value instead.  */
Lisp_Object Qdisabled, Qdisabled_command_hook;

#define NUM_RECENT_KEYS (100)
int recent_keys_index;	/* Index for storing next element into recent_keys */
int total_keys;		/* Total number of elements stored into recent_keys */
Lisp_Object recent_keys; /* A vector, holding the last 100 keystrokes */

/* Vector holding the key sequence that invoked the current command.
   It is reused for each command, and it may be longer than the current
   sequence; this_command_key_count indicates how many elements
   actually mean something.
   It's easier to staticpro a single Lisp_Object than an array.  */
Lisp_Object this_command_keys;
int this_command_key_count;

extern int minbuf_level;

extern struct backtrace *backtrace_list;

/* Nonzero means do menu prompting.  */
static int menu_prompting;

/* Character to see next line of menu prompt.  */
static Lisp_Object menu_prompt_more_char;

/* For longjmp to where kbd input is being done.  */
static jmp_buf getcjmp;

/* True while doing kbd input.  */
int waiting_for_input;

/* True while displaying for echoing.   Delays C-g throwing.  */
static int echoing;

/* Nonzero means disregard local maps for the menu bar.  */
static int inhibit_local_menu_bar_menus;

/* Nonzero means C-g should cause immediate error-signal.  */
int immediate_quit;

/* Character to recognize as the help char.  */
Lisp_Object Vhelp_char;

/* Form to execute when help char is typed.  */
Lisp_Object Vhelp_form;

/* Command to run when the help character follows a prefix key.  */
Lisp_Object Vprefix_help_command;

/* List of items that should move to the end of the menu bar.  */
Lisp_Object Vmenu_bar_final_items;

/* Character that causes a quit.  Normally C-g.

   If we are running on an ordinary terminal, this must be an ordinary
   ASCII char, since we want to make it our interrupt character.

   If we are not running on an ordinary terminal, it still needs to be
   an ordinary ASCII char.  This character needs to be recognized in
   the input interrupt handler.  At this point, the keystroke is
   represented as a struct input_event, while the desired quit
   character is specified as a lispy event.  The mapping from struct
   input_events to lispy events cannot run in an interrupt handler,
   and the reverse mapping is difficult for anything but ASCII
   keystrokes.

   FOR THESE ELABORATE AND UNSATISFYING REASONS, quit_char must be an
   ASCII character.  */
int quit_char;

extern Lisp_Object current_global_map;
extern int minibuf_level;

/* If non-nil, this is a map that overrides all other local maps.  */
Lisp_Object Voverriding_local_map;

/* If non-nil, Voverriding_local_map applies to the menu bar.  */
Lisp_Object Voverriding_local_map_menu_flag;

/* Current depth in recursive edits.  */
int command_loop_level;

/* Total number of times command_loop has read a key sequence.  */
int num_input_keys;

/* Last input character read as a command.  */
Lisp_Object last_command_char;

/* Last input character read as a command, not counting menus
   reached by the mouse.  */
Lisp_Object last_nonmenu_event;

/* Last input character read for any purpose.  */
Lisp_Object last_input_char;

/* If not Qnil, a list of objects to be read as subsequent command input.  */
Lisp_Object Vunread_command_events;

/* If not -1, an event to be read as subsequent command input.  */
int unread_command_char;

/* If not Qnil, this is a switch-frame event which we decided to put
   off until the end of a key sequence.  This should be read as the
   next command input, after any unread_command_events.

   read_key_sequence uses this to delay switch-frame events until the
   end of the key sequence; Fread_char uses it to put off switch-frame
   events until a non-ASCII event is acceptable as input.  */
Lisp_Object unread_switch_frame;

/* A mask of extra modifier bits to put into every keyboard char.  */
int extra_keyboard_modifiers;

/* Char to use as prefix when a meta character is typed in.
   This is bound on entry to minibuffer in case ESC is changed there.  */

Lisp_Object meta_prefix_char;

/* Last size recorded for a current buffer which is not a minibuffer.  */
static int last_non_minibuf_size;

/* Number of idle seconds before an auto-save and garbage collection.  */
static Lisp_Object Vauto_save_timeout;

/* Total number of times read_char has returned.  */
int num_input_chars;

/* Total number of times read_char has returned, outside of macros.  */
int num_nonmacro_input_chars;

/* Auto-save automatically when this many characters have been typed
   since the last time.  */

static int auto_save_interval;

/* Value of num_nonmacro_input_chars as of last auto save.  */

int last_auto_save;

/* Last command executed by the editor command loop, not counting
   commands that set the prefix argument.  */

Lisp_Object last_command;

/* The command being executed by the command loop.
   Commands may set this, and the value set will be copied into last_command
   instead of the actual command.  */
Lisp_Object this_command;

/* The value of point when the last command was executed.  */
int last_point_position;

/* The buffer that was current when the last command was started.  */
Lisp_Object last_point_position_buffer;

/* The timestamp of the last input event we received from the X server.
   X Windows wants this for selection ownership.  */
unsigned long last_event_timestamp;

Lisp_Object Qself_insert_command;
Lisp_Object Qforward_char;
Lisp_Object Qbackward_char;
Lisp_Object Qundefined;

/* read_key_sequence stores here the command definition of the
   key sequence that it reads.  */
Lisp_Object read_key_sequence_cmd;

/* Form to evaluate (if non-nil) when Emacs is started.  */
Lisp_Object Vtop_level;

/* User-supplied string to translate input characters through.  */
Lisp_Object Vkeyboard_translate_table;

/* Keymap mapping ASCII function key sequences onto their preferred forms.  */
extern Lisp_Object Vfunction_key_map;

/* Keymap mapping ASCII function key sequences onto their preferred forms.  */
Lisp_Object Vkey_translation_map;

/* Non-nil means deactivate the mark at end of this command.  */
Lisp_Object Vdeactivate_mark;

/* Menu bar specified in Lucid Emacs fashion.  */

Lisp_Object Vlucid_menu_bar_dirty_flag;
Lisp_Object Qrecompute_lucid_menubar, Qactivate_menubar_hook;

/* Hooks to run before and after each command.  */
Lisp_Object Qpre_command_hook, Qpost_command_hook;
Lisp_Object Vpre_command_hook, Vpost_command_hook;
Lisp_Object Qcommand_hook_internal, Vcommand_hook_internal;

/* List of deferred actions to be performed at a later time.
   The precise format isn't relevant here; we just check whether it is nil.  */
Lisp_Object Vdeferred_action_list;

/* Function to call to handle deferred actions, when there are any.  */
Lisp_Object Vdeferred_action_function;
Lisp_Object Qdeferred_action_function;

/* File in which we write all commands we read.  */
FILE *dribble;

/* Nonzero if input is available.  */
int input_pending;

/* 1 if should obey 0200 bit in input chars as "Meta", 2 if should
   keep 0200 bit in input chars.  0 to ignore the 0200 bit.  */

int meta_key;

extern char *pending_malloc_warning;

/* Circular buffer for pre-read keyboard input.  */
static struct input_event kbd_buffer[KBD_BUFFER_SIZE];

/* Vector to GCPRO the frames and windows mentioned in kbd_buffer.

   The interrupt-level event handlers will never enqueue an event on a
   frame which is not in Vframe_list, and once an event is dequeued,
   internal_last_event_frame or the event itself points to the frame.
   So that's all fine.

   But while the event is sitting in the queue, it's completely
   unprotected.  Suppose the user types one command which will run for
   a while and then delete a frame, and then types another event at
   the frame that will be deleted, before the command gets around to
   it.  Suppose there are no references to this frame elsewhere in
   Emacs, and a GC occurs before the second event is dequeued.  Now we
   have an event referring to a freed frame, which will crash Emacs
   when it is dequeued.

   Similar things happen when an event on a scroll bar is enqueued; the
   window may be deleted while the event is in the queue.

   So, we use this vector to protect the frame_or_window field in the
   event queue.  That way, they'll be dequeued as dead frames or
   windows, but still valid lisp objects.

   If kbd_buffer[i].kind != no_event, then
     (XVECTOR (kbd_buffer_frame_or_window)->contents[i]
      == kbd_buffer[i].frame_or_window.  */
static Lisp_Object kbd_buffer_frame_or_window;

/* Pointer to next available character in kbd_buffer.
   If kbd_fetch_ptr == kbd_store_ptr, the buffer is empty.
   This may be kbd_buffer + KBD_BUFFER_SIZE, meaning that the the
   next available char is in kbd_buffer[0].  */
static struct input_event *kbd_fetch_ptr;

/* Pointer to next place to store character in kbd_buffer.  This
   may be kbd_buffer + KBD_BUFFER_SIZE, meaning that the next
   character should go in kbd_buffer[0].  */
static volatile struct input_event *kbd_store_ptr;

/* The above pair of variables forms a "queue empty" flag.  When we
   enqueue a non-hook event, we increment kbd_store_ptr.  When we
   dequeue a non-hook event, we increment kbd_fetch_ptr.  We say that
   there is input available iff the two pointers are not equal.

   Why not just have a flag set and cleared by the enqueuing and
   dequeuing functions?  Such a flag could be screwed up by interrupts
   at inopportune times.  */

#ifdef HAVE_MOUSE
/* If this flag is a frame, we check mouse_moved to see when the
   mouse moves, and motion events will appear in the input stream.
   Otherwise, mouse motion is ignored.  */
static Lisp_Object do_mouse_tracking;

/* The window system handling code should set this if the mouse has
   moved since the last call to the mouse_position_hook.  Calling that
   hook should clear this.  Code assumes that if this is set, it can
   call mouse_position_hook to get the promised position, so don't set
   it unless you're prepared to substantiate the claim!  */
int mouse_moved;
#endif /* HAVE_MOUSE.  */

/* Symbols to head events.  */
Lisp_Object Qmouse_movement;
Lisp_Object Qscroll_bar_movement;
Lisp_Object Qswitch_frame;
Lisp_Object Qdelete_frame;
Lisp_Object Qiconify_frame;
Lisp_Object Qmake_frame_visible;

/* Symbols to denote kinds of events.  */
Lisp_Object Qfunction_key;
Lisp_Object Qmouse_click;
/* Lisp_Object Qmouse_movement; - also an event header */

/* Properties of event headers.  */
Lisp_Object Qevent_kind;
Lisp_Object Qevent_symbol_elements;

Lisp_Object Qmenu_enable;

/* An event header symbol HEAD may have a property named
   Qevent_symbol_element_mask, which is of the form (BASE MODIFIERS);
   BASE is the base, unmodified version of HEAD, and MODIFIERS is the
   mask of modifiers applied to it.  If present, this is used to help
   speed up parse_modifiers.  */
Lisp_Object Qevent_symbol_element_mask;

/* An unmodified event header BASE may have a property named
   Qmodifier_cache, which is an alist mapping modifier masks onto
   modified versions of BASE.  If present, this helps speed up
   apply_modifiers.  */
Lisp_Object Qmodifier_cache;

/* Symbols to use for parts of windows.  */
Lisp_Object Qmode_line;
Lisp_Object Qvertical_line;
Lisp_Object Qvertical_scroll_bar;
Lisp_Object Qmenu_bar;

extern Lisp_Object Qmenu_enable;

Lisp_Object recursive_edit_unwind (), command_loop ();
Lisp_Object Fthis_command_keys ();
Lisp_Object Qextended_command_history;

Lisp_Object Qpolling_period;

/* Address (if not 0) of EMACS_TIME to zero out if a SIGIO interrupt
   happens.  */
EMACS_TIME *input_available_clear_time;

/* Nonzero means use SIGIO interrupts; zero means use CBREAK mode.
   Default is 1 if INTERRUPT_INPUT is defined.  */
int interrupt_input;

/* Nonzero while interrupts are temporarily deferred during redisplay.  */
int interrupts_deferred;

/* nonzero means use ^S/^Q for flow control.  */
int flow_control;

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#endif

/* We are unable to use interrupts if FIONREAD is not available,
   so flush SIGIO so we won't try.  */
#ifndef FIONREAD
#ifdef SIGIO
#undef SIGIO
#endif
#endif

/* If we support X Windows, turn on the code to poll periodically
   to detect C-g.  It isn't actually used when doing interrupt input.  */
#ifdef HAVE_X_WINDOWS
#define POLL_FOR_INPUT
#endif

/* Global variable declarations.  */

/* Function for init_keyboard to call with no args (if nonzero).  */
void (*keyboard_init_hook) ();

static int read_avail_input ();
static void get_input_pending ();
static int readable_events ();
static Lisp_Object read_char_x_menu_prompt ();
static Lisp_Object read_char_minibuf_menu_prompt ();
static Lisp_Object make_lispy_event ();
static Lisp_Object make_lispy_movement ();
static Lisp_Object modify_event_symbol ();
static Lisp_Object make_lispy_switch_frame ();

/* > 0 if we are to echo keystrokes.  */
static int echo_keystrokes;

/* Nonzero means don't try to suspend even if the operating system seems
   to support it.  */
static int cannot_suspend;

#define	min(a,b)	((a)<(b)?(a):(b))
#define	max(a,b)	((a)>(b)?(a):(b))

/* Install the string STR as the beginning of the string of echoing,
   so that it serves as a prompt for the next character.
   Also start echoing.  */

echo_prompt (str)
     char *str;
{
  int len = strlen (str);

  if (len > ECHOBUFSIZE - 4)
    len = ECHOBUFSIZE - 4;
  bcopy (str, current_perdisplay->echobuf, len);
  current_perdisplay->echoptr = current_perdisplay->echobuf + len;
  *current_perdisplay->echoptr = '\0';

  current_perdisplay->echo_after_prompt = len;

  echo ();
}

/* Add C to the echo string, if echoing is going on.  
   C can be a character, which is printed prettily ("M-C-x" and all that
   jazz), or a symbol, whose name is printed.  */

echo_char (c)
     Lisp_Object c;
{
  extern char *push_key_description ();

  if (current_perdisplay->immediate_echo)
    {
      char *ptr = current_perdisplay->echoptr;
      
      if (ptr != current_perdisplay->echobuf)
	*ptr++ = ' ';

      /* If someone has passed us a composite event, use its head symbol.  */
      c = EVENT_HEAD (c);

      if (INTEGERP (c))
	{
	  if (ptr - current_perdisplay->echobuf > ECHOBUFSIZE - 6)
	    return;

	  ptr = push_key_description (XINT (c), ptr);
	}
      else if (SYMBOLP (c))
	{
	  struct Lisp_String *name = XSYMBOL (c)->name;
	  if (((ptr - current_perdisplay->echobuf) + name->size + 4)
	      > ECHOBUFSIZE)
	    return;
	  bcopy (name->data, ptr, name->size);
	  ptr += name->size;
	}

      if (current_perdisplay->echoptr == current_perdisplay->echobuf
	  && EQ (c, Vhelp_char))
	{
	  strcpy (ptr, " (Type ? for further options)");
	  ptr += strlen (ptr);
	}

      *ptr = 0;
      current_perdisplay->echoptr = ptr;

      echo ();
    }
}

/* Temporarily add a dash to the end of the echo string if it's not
   empty, so that it serves as a mini-prompt for the very next character.  */

echo_dash ()
{
  if (!current_perdisplay->immediate_echo
      && current_perdisplay->echoptr == current_perdisplay->echobuf)
    return;
  /* Do nothing if we just printed a prompt.  */
  if (current_perdisplay->echo_after_prompt
      == current_perdisplay->echoptr - current_perdisplay->echobuf)
    return;
  /* Do nothing if not echoing at all.  */
  if (current_perdisplay->echoptr == 0)
    return;

  /* Put a dash at the end of the buffer temporarily,
     but make it go away when the next character is added.  */
  current_perdisplay->echoptr[0] = '-';
  current_perdisplay->echoptr[1] = 0;

  echo ();
}

/* Display the current echo string, and begin echoing if not already
   doing so.  */

echo ()
{
  if (!current_perdisplay->immediate_echo)
    {
      int i;
      current_perdisplay->immediate_echo = 1;

      for (i = 0; i < this_command_key_count; i++)
	{
	  Lisp_Object c;
	  c = XVECTOR (this_command_keys)->contents[i];
	  if (! (EVENT_HAS_PARAMETERS (c)
		 && EQ (EVENT_HEAD_KIND (EVENT_HEAD (c)), Qmouse_movement)))
	    echo_char (c);
	}
      echo_dash ();
    }

  echoing = 1;
  message1_nolog (current_perdisplay->echobuf);
  echoing = 0;

  if (waiting_for_input && !NILP (Vquit_flag))
    quit_throw_to_read_char ();
}

/* Turn off echoing, for the start of a new command.  */

cancel_echoing ()
{
  current_perdisplay->immediate_echo = 0;
  current_perdisplay->echoptr = current_perdisplay->echobuf;
  current_perdisplay->echo_after_prompt = -1;
}

/* Return the length of the current echo string.  */

static int
echo_length ()
{
  return current_perdisplay->echoptr - current_perdisplay->echobuf;
}

/* Truncate the current echo message to its first LEN chars.
   This and echo_char get used by read_key_sequence when the user
   switches frames while entering a key sequence.  */

static void
echo_truncate (len)
     int len;
{
  current_perdisplay->echobuf[len] = '\0';
  current_perdisplay->echoptr = current_perdisplay->echobuf + len;
  truncate_echo_area (len);
}


/* Functions for manipulating this_command_keys.  */
static void
add_command_key (key)
     Lisp_Object key;
{
  int size = XVECTOR (this_command_keys)->size;

  if (this_command_key_count >= size)
    {
      Lisp_Object new_keys;

      new_keys = Fmake_vector (make_number (size * 2), Qnil);
      bcopy (XVECTOR (this_command_keys)->contents,
	     XVECTOR (new_keys)->contents,
	     size * sizeof (Lisp_Object));

      this_command_keys = new_keys;
    }

  XVECTOR (this_command_keys)->contents[this_command_key_count++] = key;
}

Lisp_Object
recursive_edit_1 ()
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object val;

  if (command_loop_level > 0)
    {
      specbind (Qstandard_output, Qt);
      specbind (Qstandard_input, Qt);
    }

  val = command_loop ();
  if (EQ (val, Qt))
    Fsignal (Qquit, Qnil);

  return unbind_to (count, Qnil);
}

/* When an auto-save happens, record the "time", and don't do again soon.  */

record_auto_save ()
{
  last_auto_save = num_nonmacro_input_chars;
}

/* Make an auto save happen as soon as possible at command level.  */

force_auto_save_soon ()
{
  last_auto_save = - auto_save_interval - 1;

  record_asynch_buffer_change ();
}

DEFUN ("recursive-edit", Frecursive_edit, Srecursive_edit, 0, 0, "",
  "Invoke the editor command loop recursively.\n\
To get out of the recursive edit, a command can do `(throw 'exit nil)';\n\
that tells this function to return.\n\
Alternately, `(throw 'exit t)' makes this function signal an error.\n\
This function is called by the editor initialization to begin editing.")
  ()
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object val;

  command_loop_level++;
  update_mode_lines = 1;

  record_unwind_protect (recursive_edit_unwind,
			 (command_loop_level
			  && current_buffer != XBUFFER (XWINDOW (selected_window)->buffer))
			 ? Fcurrent_buffer ()
			 : Qnil);
  recursive_edit_1 ();
  return unbind_to (count, Qnil);
}

Lisp_Object
recursive_edit_unwind (buffer)
     Lisp_Object buffer;
{
  if (!NILP (buffer))
    Fset_buffer (buffer);

  command_loop_level--;
  update_mode_lines = 1;
  return Qnil;
}

Lisp_Object
cmd_error (data)
     Lisp_Object data;
{
  Vstandard_output = Qt;
  Vstandard_input = Qt;
  Vexecuting_macro = Qnil;
  if (!current_perdisplay)
    abort ();
  current_perdisplay->Vprefix_arg = Qnil;
  cancel_echoing ();
  cmd_error_internal (data, 0);

  Vquit_flag = Qnil;

  Vinhibit_quit = Qnil;
#ifdef MULTI_PERDISPLAY
  current_perdisplay = 0;
  display_locked = 0;
#endif

  return make_number (0);
}

cmd_error_internal (data, context)
     Lisp_Object data;
     char *context;
{
  Lisp_Object errmsg, tail, errname, file_error;
  Lisp_Object stream;
  struct gcpro gcpro1;
  int i;

  Vquit_flag = Qnil;
  Vinhibit_quit = Qt;
  echo_area_glyphs = 0;

  /* If the window system or terminal frame hasn't been initialized
     yet, or we're not interactive, it's best to dump this message out
     to stderr and exit.  */
  if (! FRAME_MESSAGE_BUF (selected_frame)
      || noninteractive)
    stream = Qexternal_debugging_output;
  else
    {
      Fdiscard_input ();
      bitch_at_user ();
      stream = Qt;
    }

  if (context != 0)
    write_string_1 (context, -1, stream);

  errname = Fcar (data);

  if (EQ (errname, Qerror))
    {
      data = Fcdr (data);
      if (!CONSP (data)) data = Qnil;
      errmsg = Fcar (data);
      file_error = Qnil;
    }
  else
    {
      errmsg = Fget (errname, Qerror_message);
      file_error = Fmemq (Qfile_error,
			  Fget (errname, Qerror_conditions));
    }

  /* Print an error message including the data items.
     This is done by printing it into a scratch buffer
     and then making a copy of the text in the buffer. */

  if (!CONSP (data)) data = Qnil;
  tail = Fcdr (data);
  GCPRO1 (tail);

  /* For file-error, make error message by concatenating
     all the data items.  They are all strings.  */
  if (!NILP (file_error) && !NILP (tail))
    errmsg = XCONS (tail)->car, tail = XCONS (tail)->cdr;

  if (STRINGP (errmsg))
    Fprinc (errmsg, stream);
  else
    write_string_1 ("peculiar error", -1, stream);

  for (i = 0; CONSP (tail); tail = Fcdr (tail), i++)
    {
      write_string_1 (i ? ", " : ": ", 2, stream);
      if (!NILP (file_error))
	Fprinc (Fcar (tail), stream);
      else
	Fprin1 (Fcar (tail), stream);
    }
  UNGCPRO;

  /* If the window system or terminal frame hasn't been initialized
     yet, or we're in -batch mode, this error should cause Emacs to exit.  */
  if (! FRAME_MESSAGE_BUF (selected_frame)
      || noninteractive)
    {
      Fterpri (stream);
      Fkill_emacs (make_number (-1));
    }
}

Lisp_Object command_loop_1 ();
Lisp_Object command_loop_2 ();
Lisp_Object top_level_1 ();

/* Entry to editor-command-loop.
   This level has the catches for exiting/returning to editor command loop.
   It returns nil to exit recursive edit, t to abort it.  */

Lisp_Object
command_loop ()
{
  if (command_loop_level > 0 || minibuf_level > 0)
    {
      return internal_catch (Qexit, command_loop_2, Qnil);
    }
  else
    while (1)
      {
	internal_catch (Qtop_level, top_level_1, Qnil);
	internal_catch (Qtop_level, command_loop_2, Qnil);
	
	/* End of file in -batch run causes exit here.  */
	if (noninteractive)
	  Fkill_emacs (Qt);
      }
}

/* Here we catch errors in execution of commands within the
   editing loop, and reenter the editing loop.
   When there is an error, cmd_error runs and returns a non-nil
   value to us.  A value of nil means that cmd_loop_1 itself
   returned due to end of file (or end of kbd macro).  */

Lisp_Object
command_loop_2 ()
{
  register Lisp_Object val;

  do
    val = internal_condition_case (command_loop_1, Qerror, cmd_error);
  while (!NILP (val));

  return Qnil;
}

Lisp_Object
top_level_2 ()
{
  return Feval (Vtop_level);
}

Lisp_Object
top_level_1 ()
{
  /* On entry to the outer level, run the startup file */
  if (!NILP (Vtop_level))
    internal_condition_case (top_level_2, Qerror, cmd_error);
  else if (!NILP (Vpurify_flag))
    message ("Bare impure Emacs (standard Lisp code not loaded)");
  else
    message ("Bare Emacs (standard Lisp code not loaded)");
  return Qnil;
}

DEFUN ("top-level", Ftop_level, Stop_level, 0, 0, "",
  "Exit all recursive editing levels.")
  ()
{
  Fthrow (Qtop_level, Qnil);
}

DEFUN ("exit-recursive-edit", Fexit_recursive_edit, Sexit_recursive_edit, 0, 0, "",
  "Exit from the innermost recursive edit or minibuffer.")
  ()
{
  if (command_loop_level > 0 || minibuf_level > 0)
    Fthrow (Qexit, Qnil);

  error ("No recursive edit is in progress");
}

DEFUN ("abort-recursive-edit", Fabort_recursive_edit, Sabort_recursive_edit, 0, 0, "",
  "Abort the command that requested this recursive edit or minibuffer input.")
  ()
{
  if (command_loop_level > 0 || minibuf_level > 0)
    Fthrow (Qexit, Qt);

  error ("No recursive edit is in progress");
}

/* This is the actual command reading loop,
   sans error-handling encapsulation.  */

Lisp_Object Fcommand_execute ();
static int read_key_sequence ();
static void safe_run_hooks ();

Lisp_Object
command_loop_1 ()
{
  Lisp_Object cmd, tem;
  int lose;
  int nonundocount;
  Lisp_Object keybuf[30];
  int i;
  int no_redisplay;
  int no_direct;
  int prev_modiff;
  struct buffer *prev_buffer;
#ifdef MULTI_PERDISPLAY
  PERDISPLAY *outer_perdisplay = current_perdisplay;
#endif

  Vdeactivate_mark = Qnil;
  waiting_for_input = 0;
  if (current_perdisplay)
    cancel_echoing ();

  nonundocount = 0;
  no_redisplay = 0;
  this_command_key_count = 0;

  /* Make sure this hook runs after commands that get errors and
     throw to top level.  */
  /* Note that the value cell will never directly contain nil
     if the symbol is a local variable.  */
  if (!NILP (XSYMBOL (Qpost_command_hook)->value) && !NILP (Vrun_hooks))
    safe_run_hooks (Qpost_command_hook);

  if (!NILP (Vdeferred_action_list))
    call0 (Vdeferred_action_function);

  /* Do this after running Vpost_command_hook, for consistency.  */
  last_command = this_command;

  while (1)
    {
      /* Make sure the current window's buffer is selected.  */
      if (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer)
	set_buffer_internal (XBUFFER (XWINDOW (selected_window)->buffer));

      /* Display any malloc warning that just came out.  Use while because
	 displaying one warning can cause another.  */

      while (pending_malloc_warning)
	display_malloc_warning ();

      no_direct = 0;

      Vdeactivate_mark = Qnil;

      /* If minibuffer on and echo area in use,
	 wait 2 sec and redraw minibuffer.  */

      if (minibuf_level && echo_area_glyphs)
	{
	  /* Bind inhibit-quit to t so that C-g gets read in
	     rather than quitting back to the minibuffer.  */
	  int count = specpdl_ptr - specpdl;
	  specbind (Qinhibit_quit, Qt);
	  Fsit_for (make_number (2), Qnil, Qnil);
	  unbind_to (count, Qnil);

	  echo_area_glyphs = 0;
	  no_direct = 1;
	  if (!NILP (Vquit_flag))
	    {
	      Vquit_flag = Qnil;
	      Vunread_command_events = Fcons (make_number (quit_char), Qnil);
	    }
	}

#ifdef C_ALLOCA
      alloca (0);		/* Cause a garbage collection now */
				/* Since we can free the most stuff here.  */
#endif /* C_ALLOCA */

#if 0
#ifdef MULTI_FRAME
      /* Select the frame that the last event came from.  Usually,
	 switch-frame events will take care of this, but if some lisp
	 code swallows a switch-frame event, we'll fix things up here.
	 Is this a good idea?  */
      if (FRAMEP (internal_last_event_frame)
	  && XFRAME (internal_last_event_frame) != selected_frame)
	Fselect_frame (internal_last_event_frame, Qnil);
#endif
#endif
      /* If it has changed current-menubar from previous value,
	 really recompute the menubar from the value.  */
      if (! NILP (Vlucid_menu_bar_dirty_flag)
	  && !NILP (Ffboundp (Qrecompute_lucid_menubar)))
	call0 (Qrecompute_lucid_menubar);

      /* Read next key sequence; i gets its length.  */
      i = read_key_sequence (keybuf, sizeof keybuf / sizeof keybuf[0], Qnil, 0);

      ++num_input_keys;

      /* Now we have read a key sequence of length I,
	 or else I is 0 and we found end of file.  */

      if (i == 0)		/* End of file -- happens only in */
	return Qnil;		/* a kbd macro, at the end.  */
      /* -1 means read_key_sequence got a menu that was rejected.
	 Just loop around and read another command.  */
      if (i == -1)
	{
	  cancel_echoing ();
	  this_command_key_count = 0;
	  goto finalize;
	}

      last_command_char = keybuf[i - 1];

      /* If the previous command tried to force a specific window-start,
	 forget about that, in case this command moves point far away
	 from that position.  */
      XWINDOW (selected_window)->force_start = Qnil;

      cmd = read_key_sequence_cmd;
      if (!NILP (Vexecuting_macro))
	{
	  if (!NILP (Vquit_flag))
	    {
	      Vexecuting_macro = Qt;
	      QUIT;		/* Make some noise. */
				/* Will return since macro now empty. */
	    }
	}

      /* Do redisplay processing after this command except in special
	 cases identified below that set no_redisplay to 1.
	 (actually, there's currently no way to prevent the redisplay,
	 and no_redisplay is ignored.
	 Perhaps someday we will really implement it.)  */
      no_redisplay = 0;

      prev_buffer = current_buffer;
      prev_modiff = MODIFF;
      last_point_position = PT;
      XSETBUFFER (last_point_position_buffer, prev_buffer);

      /* Execute the command.  */

      this_command = cmd;
      /* Note that the value cell will never directly contain nil
	 if the symbol is a local variable.  */
      if (!NILP (XSYMBOL (Qpre_command_hook)->value) && !NILP (Vrun_hooks))
	safe_run_hooks (Qpre_command_hook);

      if (NILP (this_command))
	{
	  /* nil means key is undefined.  */
	  bitch_at_user ();
	  defining_kbd_macro = 0;
	  update_mode_lines = 1;
	  current_perdisplay->Vprefix_arg = Qnil;

	}
      else
	{
	  if (NILP (current_perdisplay->Vprefix_arg) && ! no_direct)
	    {
	      /* Recognize some common commands in common situations and
		 do them directly.  */
	      if (EQ (this_command, Qforward_char) && PT < ZV)
		{
                  struct Lisp_Vector *dp
		    = window_display_table (XWINDOW (selected_window));
		  lose = FETCH_CHAR (PT);
		  SET_PT (PT + 1);
		  if ((dp
		       ? (VECTORP (DISP_CHAR_VECTOR (dp, lose))
			  ? XVECTOR (DISP_CHAR_VECTOR (dp, lose))->size == 1
                          : (NILP (DISP_CHAR_VECTOR (dp, lose))
                             && (lose >= 0x20 && lose < 0x7f)))
		       : (lose >= 0x20 && lose < 0x7f))
		      && (XFASTINT (XWINDOW (selected_window)->last_modified)
			  >= MODIFF)
		      && (XFASTINT (XWINDOW (selected_window)->last_point)
			  == PT - 1)
		      && !windows_or_buffers_changed
		      && EQ (current_buffer->selective_display, Qnil)
		      && !detect_input_pending ()
		      && NILP (Vexecuting_macro))
		    no_redisplay = direct_output_forward_char (1);
		  goto directly_done;
		}
	      else if (EQ (this_command, Qbackward_char) && PT > BEGV)
		{
                  struct Lisp_Vector *dp
		    = window_display_table (XWINDOW (selected_window));
		  SET_PT (PT - 1);
		  lose = FETCH_CHAR (PT);
		  if ((dp
		       ? (VECTORP (DISP_CHAR_VECTOR (dp, lose))
			  ? XVECTOR (DISP_CHAR_VECTOR (dp, lose))->size == 1
                          : (NILP (DISP_CHAR_VECTOR (dp, lose))
                             && (lose >= 0x20 && lose < 0x7f)))
		       : (lose >= 0x20 && lose < 0x7f))
		      && (XFASTINT (XWINDOW (selected_window)->last_modified)
			  >= MODIFF)
		      && (XFASTINT (XWINDOW (selected_window)->last_point)
			  == PT + 1)
		      && !windows_or_buffers_changed
		      && EQ (current_buffer->selective_display, Qnil)
		      && !detect_input_pending ()
		      && NILP (Vexecuting_macro))
		    no_redisplay = direct_output_forward_char (-1);
		  goto directly_done;
		}
	      else if (EQ (this_command, Qself_insert_command)
		       /* Try this optimization only on ascii keystrokes.  */
		       && INTEGERP (last_command_char))
		{
		  unsigned char c = XINT (last_command_char);
		  int value;

		  if (NILP (Vexecuting_macro)
		      && !EQ (minibuf_window, selected_window))
		    {
		      if (!nonundocount || nonundocount >= 20)
			{
			  Fundo_boundary ();
			  nonundocount = 0;
			}
		      nonundocount++;
		    }
		  lose = ((XFASTINT (XWINDOW (selected_window)->last_modified)
			   < MODIFF)
			  || (XFASTINT (XWINDOW (selected_window)->last_point)
			      != PT)
			  || MODIFF <= SAVE_MODIFF
			  || windows_or_buffers_changed
			  || !EQ (current_buffer->selective_display, Qnil)
			  || detect_input_pending ()
			  || !NILP (Vexecuting_macro));
		  value = internal_self_insert (c, 0);
		  if (value)
		    lose = 1;
		  if (value == 2)
		    nonundocount = 0;

		  if (!lose
		      && (PT == ZV || FETCH_CHAR (PT) == '\n'))
		    {
		      struct Lisp_Vector *dp
			= window_display_table (XWINDOW (selected_window));
		      int lose = c;

		      if (dp)
			{
			  Lisp_Object obj;

			  obj = DISP_CHAR_VECTOR (dp, lose);
			  if (NILP (obj))
			    {
			      /* Do it only for char codes
				 that by default display as themselves.  */
			      if (lose >= 0x20 && lose <= 0x7e)
				no_redisplay = direct_output_for_insert (lose);
			    }
			  else if (VECTORP (obj)
				   && XVECTOR (obj)->size == 1
				   && (obj = XVECTOR (obj)->contents[0],
				       INTEGERP (obj))
				   /* Insist face not specified in glyph.  */
				   && (XINT (obj) & ((-1) << 8)) == 0)
			    no_redisplay
			      = direct_output_for_insert (XINT (obj));
			}
		      else
			{
			  if (lose >= 0x20 && lose <= 0x7e)
			    no_redisplay = direct_output_for_insert (lose);
			}
		    }
		  goto directly_done;
		}
	    }

	  /* Here for a command that isn't executed directly */

	  nonundocount = 0;
	  if (NILP (current_perdisplay->Vprefix_arg))
	    Fundo_boundary ();
	  Fcommand_execute (this_command, Qnil);

	}
    directly_done: ;

      /* Note that the value cell will never directly contain nil
	 if the symbol is a local variable.  */
      if (!NILP (XSYMBOL (Qpost_command_hook)->value) && !NILP (Vrun_hooks))
	safe_run_hooks (Qpost_command_hook);

      if (!NILP (Vdeferred_action_list))
	safe_run_hooks (Qdeferred_action_function);

      /* If there is a prefix argument,
	 1) We don't want last_command to be ``universal-argument''
	 (that would be dumb), so don't set last_command,
	 2) we want to leave echoing on so that the prefix will be
	 echoed as part of this key sequence, so don't call
	 cancel_echoing, and
	 3) we want to leave this_command_key_count non-zero, so that
	 read_char will realize that it is re-reading a character, and
	 not echo it a second time.  */
      if (NILP (current_perdisplay->Vprefix_arg))
	{
	  last_command = this_command;
	  cancel_echoing ();
	  this_command_key_count = 0;
	}

      if (!NILP (current_buffer->mark_active) && !NILP (Vrun_hooks))
	{
	  if (!NILP (Vdeactivate_mark) && !NILP (Vtransient_mark_mode))
	    {
	      current_buffer->mark_active = Qnil;
	      call1 (Vrun_hooks, intern ("deactivate-mark-hook"));
	    }
	  else if (current_buffer != prev_buffer || MODIFF != prev_modiff)
	    call1 (Vrun_hooks, intern ("activate-mark-hook"));
	}

    finalize:
      /* Install chars successfully executed in kbd macro.  */

      if (defining_kbd_macro && NILP (current_perdisplay->Vprefix_arg))
	finalize_kbd_macro_chars ();

#ifdef MULTI_PERDISPLAY
      current_perdisplay = outer_perdisplay;
      display_locked = (current_perdisplay != 0);
#endif
    }
}

/* If we get an error while running the hook, cause the hook variable
   to be nil.  Also inhibit quits, so that C-g won't cause the hook
   to mysteriously evaporate.  */
static void
safe_run_hooks (hook)
     Lisp_Object hook;
{
  Lisp_Object value;
  int count = specpdl_ptr - specpdl;
  specbind (Qinhibit_quit, Qt);

  /* We read and set the variable with functions,
     in case it's buffer-local.  */
  value = Vcommand_hook_internal = Fsymbol_value (hook);
  Fset (hook, Qnil);
  call1 (Vrun_hooks, Qcommand_hook_internal);
  Fset (hook, value);

  unbind_to (count, Qnil);
}

/* Number of seconds between polling for input.  */
int polling_period;

/* Nonzero means polling for input is temporarily suppressed.  */
int poll_suppress_count;

/* Nonzero if polling_for_input is actually being used.  */
int polling_for_input;

#ifdef POLL_FOR_INPUT

/* Handle an alarm once each second and read pending input
   so as to handle a C-g if it comces in.  */

SIGTYPE
input_poll_signal (signalnum)	/* If we don't have an argument, */
     int signalnum;		/* some compilers complain in signal calls. */
{
  if (interrupt_input_blocked == 0
      && !waiting_for_input)
    read_avail_input (0);
  signal (SIGALRM, input_poll_signal);
  alarm (polling_period);
}

#endif

/* Begin signals to poll for input, if they are appropriate.
   This function is called unconditionally from various places.  */

start_polling ()
{
#ifdef POLL_FOR_INPUT
  if (read_socket_hook && !interrupt_input)
    {
      poll_suppress_count--;
      if (poll_suppress_count == 0)
	{
	  signal (SIGALRM, input_poll_signal);
	  polling_for_input = 1;
	  alarm (polling_period);
	}
    }
#endif
}

/* Nonzero if we are using polling to handle input asynchronously.  */

int
input_polling_used ()
{
#ifdef POLL_FOR_INPUT
  return read_socket_hook && !interrupt_input;
#else
  return 0;
#endif
}

/* Turn off polling.  */

stop_polling ()
{
#ifdef POLL_FOR_INPUT
  if (read_socket_hook && !interrupt_input)
    {
      if (poll_suppress_count == 0)
	{
	  polling_for_input = 0;
	  alarm (0);
	}
      poll_suppress_count++;
    }
#endif
}

/* Set the value of poll_suppress_count to COUNT
   and start or stop polling accordingly.  */

void
set_poll_suppress_count (count)
     int count;
{
#ifdef POLL_FOR_INPUT
  if (count == 0 && poll_suppress_count != 0)
    {
      poll_suppress_count = 1;
      start_polling ();
    }
  else if (count != 0 && poll_suppress_count == 0)
    {
      stop_polling ();
    }
  poll_suppress_count = count;
#endif
}

/* Bind polling_period to a value at least N.
   But don't decrease it.  */

bind_polling_period (n)
     int n;
{
#ifdef POLL_FOR_INPUT
  int new = polling_period;

  if (n > new)
    new = n;

  stop_polling ();
  specbind (Qpolling_period, make_number (new));
  /* Start a new alarm with the new period.  */
  start_polling ();
#endif
}

/* Applying the control modifier to CHARACTER.  */
int
make_ctrl_char (c)
     int c;
{
  /* Save the upper bits here.  */
  int upper = c & ~0177;

  c &= 0177;

  /* Everything in the columns containing the upper-case letters
     denotes a control character.  */
  if (c >= 0100 && c < 0140)
    {
      int oc = c;
      c &= ~0140;
      /* Set the shift modifier for a control char
	 made from a shifted letter.  But only for letters!  */
      if (oc >= 'A' && oc <= 'Z')
	c |= shift_modifier;
    }

  /* The lower-case letters denote control characters too.  */
  else if (c >= 'a' && c <= 'z')
    c &= ~0140;

  /* Include the bits for control and shift
     only if the basic ASCII code can't indicate them.  */
  else if (c >= ' ')
    c |= ctrl_modifier;

  /* Replace the high bits.  */
  c |= (upper & ~ctrl_modifier);

  return c;
}



/* Input of single characters from keyboard */

Lisp_Object print_help ();
static Lisp_Object kbd_buffer_get_event ();
static void record_char ();

#ifdef MULTI_PERDISPLAY
static jmp_buf wrong_display_jmpbuf;
#endif

/* read a character from the keyboard; call the redisplay if needed */
/* commandflag 0 means do not do auto-saving, but do do redisplay.
   -1 means do not do redisplay, but do do autosaving.
   1 means do both.  */

/* The arguments MAPS and NMAPS are for menu prompting.
   MAPS is an array of keymaps;  NMAPS is the length of MAPS.

   PREV_EVENT is the previous input event, or nil if we are reading
   the first event of a key sequence.

   If USED_MOUSE_MENU is non-zero, then we set *USED_MOUSE_MENU to 1
   if we used a mouse menu to read the input, or zero otherwise.  If
   USED_MOUSE_MENU is zero, *USED_MOUSE_MENU is left alone.

   Value is t if we showed a menu and the user rejected it.  */

Lisp_Object
read_char (commandflag, nmaps, maps, prev_event, used_mouse_menu)
     int commandflag;
     int nmaps;
     Lisp_Object *maps;
     Lisp_Object prev_event;
     int *used_mouse_menu;
{
  register Lisp_Object c;
  int count;
  jmp_buf save_jump;
  int key_already_recorded = 0;
  Lisp_Object also_record;
  also_record = Qnil;

  if (CONSP (Vunread_command_events))
    {
      c = XCONS (Vunread_command_events)->car;
      Vunread_command_events = XCONS (Vunread_command_events)->cdr;

      if (this_command_key_count == 0)
	goto reread_first;
      else
	goto reread;
    }

  if (unread_command_char != -1)
    {
      XSETINT (c, unread_command_char);
      unread_command_char = -1;

      if (this_command_key_count == 0)
	goto reread_first;
      else
	goto reread;
    }

  if (!NILP (Vexecuting_macro))
    {
#ifdef MULTI_FRAME
      /* We set this to Qmacro; since that's not a frame, nobody will
	 try to switch frames on us, and the selected window will
	 remain unchanged.

         Since this event came from a macro, it would be misleading to
	 leave internal_last_event_frame set to wherever the last
	 real event came from.  Normally, a switch-frame event selects
	 internal_last_event_frame after each command is read, but
	 events read from a macro should never cause a new frame to be
	 selected. */
      if (!current_perdisplay)
	abort ();
      current_perdisplay->internal_last_event_frame = Qmacro;
      current_perdisplay->Vlast_event_frame = Qmacro;
#endif

      /* Exit the macro if we are at the end.
	 Also, some things replace the macro with t
	 to force an early exit.  */
      if (EQ (Vexecuting_macro, Qt)
	  || executing_macro_index >= XFASTINT (Flength (Vexecuting_macro)))
	{
	  XSETINT (c, -1);
	  return c;
	}
      
      c = Faref (Vexecuting_macro, make_number (executing_macro_index));
      if (STRINGP (Vexecuting_macro)
	  && (XINT (c) & 0x80))
	XSETFASTINT (c, CHAR_META | (XINT (c) & ~0x80));

      executing_macro_index++;

      goto from_macro;
    }

  if (!NILP (unread_switch_frame))
    {
      c = unread_switch_frame;
      unread_switch_frame = Qnil;

      /* This event should make it into this_command_keys, and get echoed
	 again, so we go to reread_first, rather than reread.  */
      goto reread_first;
    }

  /* Don't bother updating menu bars while doing mouse tracking.
     We get events very rapidly then, and the menu bar won't be changing.
     We do update the menu bar once on entry to Ftrack_mouse.  */
  if (commandflag > 0 && !input_pending && !detect_input_pending ())
    prepare_menu_bars ();

  /* Save outer setjmp data, in case called recursively.  */
  save_getcjmp (save_jump);

  stop_polling ();

  if (commandflag >= 0 && !input_pending && !detect_input_pending ())
    redisplay ();

  if (_setjmp (getcjmp))
    {
      XSETINT (c, quit_char);
#ifdef MULTI_FRAME
      XSETFRAME (current_perdisplay->internal_last_event_frame,
		 selected_frame);
      current_perdisplay->Vlast_event_frame
	= current_perdisplay->internal_last_event_frame;
#endif
      /* If we report the quit char as an event,
	 don't do so more than once.  */
      if (!NILP (Vinhibit_quit))
	Vquit_flag = Qnil;

      goto non_reread;
    }

  /* Message turns off echoing unless more keystrokes turn it on again. */
  if (echo_area_glyphs && *echo_area_glyphs
      && echo_area_glyphs != current_perdisplay->echobuf)
    cancel_echoing ();
  else
    /* If already echoing, continue.  */
    echo_dash ();

  /* Try reading a character via menu prompting in the minibuf.
     Try this before the sit-for, because the sit-for
     would do the wrong thing if we are supposed to do
     menu prompting. If EVENT_HAS_PARAMETERS then we are reading
     after a mouse event so don't try a minibuf menu. */
  c = Qnil;
  if (nmaps > 0 && INTERACTIVE
      && !NILP (prev_event) && ! EVENT_HAS_PARAMETERS (prev_event)
      /* Don't bring up a menu if we already have another event.  */
      && NILP (Vunread_command_events)
      && unread_command_char < 0
      && !detect_input_pending ())
    {
      c = read_char_minibuf_menu_prompt (commandflag, nmaps, maps);
      if (! NILP (c))
	{
	  key_already_recorded = 1;
	  goto non_reread;
	}
    }

  /* If in middle of key sequence and minibuffer not active,
     start echoing if enough time elapses.  */
  if (minibuf_level == 0 && !current_perdisplay->immediate_echo
      && this_command_key_count > 0
      && ! noninteractive
      && echo_keystrokes > 0
      && (echo_area_glyphs == 0 || *echo_area_glyphs == 0))
    {
      Lisp_Object tem0;

      /* After a mouse event, start echoing right away.
	 This is because we are probably about to display a menu,
	 and we don't want to delay before doing so.  */
      if (EVENT_HAS_PARAMETERS (prev_event))
	echo ();
      else
	{
	  tem0 = sit_for (echo_keystrokes, 0, 1, 1);
	  if (EQ (tem0, Qt))
	    echo ();
	}
    }

  /* Maybe auto save due to number of keystrokes or idle time.  */

  if (commandflag != 0
      && auto_save_interval > 0
      && num_nonmacro_input_chars - last_auto_save > max (auto_save_interval, 20)
      && !detect_input_pending ())
    {
      jmp_buf temp;
      save_getcjmp (temp);
      Fdo_auto_save (Qnil, Qnil);
      /* Hooks can actually change some buffers in auto save.  */
      redisplay ();
      restore_getcjmp (temp);
    }

  /* Try reading using an X menu.
     This is never confused with reading using the minibuf
     because the recursive call of read_char in read_char_minibuf_menu_prompt
     does not pass on any keymaps.  */
  if (nmaps > 0 && INTERACTIVE
      && !NILP (prev_event) && EVENT_HAS_PARAMETERS (prev_event)
      /* Don't bring up a menu if we already have another event.  */
      && NILP (Vunread_command_events)
      && unread_command_char < 0)
    c = read_char_x_menu_prompt (nmaps, maps, prev_event, used_mouse_menu);

  /* Slow down auto saves logarithmically in size of current buffer,
     and garbage collect while we're at it.  */
  if (INTERACTIVE && NILP (c))
    {
      int delay_level, buffer_size;

      if (! MINI_WINDOW_P (XWINDOW (selected_window)))
	last_non_minibuf_size = Z - BEG;
      buffer_size = (last_non_minibuf_size >> 8) + 1;
      delay_level = 0;
      while (buffer_size > 64)
	delay_level++, buffer_size -= buffer_size >> 2;
      if (delay_level < 4) delay_level = 4;
      /* delay_level is 4 for files under around 50k, 7 at 100k,
	 9 at 200k, 11 at 300k, and 12 at 500k.  It is 15 at 1 meg.  */

      /* Auto save if enough time goes by without input.  */
      if (commandflag != 0
	  && num_nonmacro_input_chars > last_auto_save
	  && INTEGERP (Vauto_save_timeout)
	  && XINT (Vauto_save_timeout) > 0)
	{
	  Lisp_Object tem0;
	  int delay = delay_level * XFASTINT (Vauto_save_timeout) / 4;
	  tem0 = sit_for (delay, 0, 1, 1);
	  if (EQ (tem0, Qt))
	    {
	      jmp_buf temp;
	      save_getcjmp (temp);
	      Fdo_auto_save (Qnil, Qnil);
	      restore_getcjmp (temp);

	      /* If we have auto-saved and there is still no input
		 available, garbage collect if there has been enough
		 consing going on to make it worthwhile.  */
	      if (!detect_input_pending ()
		  && consing_since_gc > gc_cons_threshold / 2)
		Fgarbage_collect ();
	      /* prepare_menu_bars isn't safe here, but it should
		 also be unnecessary.  */
	      redisplay ();
	    }
	}
    }

  if (NILP (c))
    {
      PERDISPLAY *perd;
      /* Check for something on one of the side queues.  Give priority to
	 the current display, but if we're not locked, then check the other
	 displays as well.  */
      if (current_perdisplay && CONSP (current_perdisplay->kbd_queue))
	perd = current_perdisplay;
      else if (!display_locked)
	{
	  for (perd = all_perdisplays; perd; perd = perd->next_perdisplay)
	    if (CONSP (perd->kbd_queue))
	      break;
	}
      else
	perd = 0;

      /* If we found something on a side queue, use that.
	 Otherwise, read from the main queue, and if that gives us
	 something we can't use yet, put it on the side queue and
	 try again.  */
      if (perd)
	{
	  c = XCONS (perd->kbd_queue)->car;
	  perd->kbd_queue = XCONS (perd->kbd_queue)->cdr;
	}
      else
	{
	wrong_display:
	  /* Actually read a character, waiting if necessary.  */
	  while (c = kbd_buffer_get_event (&perd), NILP (c))
	    {
	      if (commandflag >= 0
		  && !input_pending && !detect_input_pending ())
		{
		  prepare_menu_bars ();
		  redisplay ();
		}
	    }
	  if (display_locked && perd != current_perdisplay)
	    {
	      Lisp_Object *tailp = &perd->kbd_queue;
	      while (CONSP (*tailp))
		tailp = &XCONS (*tailp)->cdr;
	      if (!NILP (*tailp))
		abort ();
	      *tailp = Fcons (c, Qnil);
	      goto wrong_display;
	    }
	}
#ifdef MULTI_PERDISPLAY
      if (!current_perdisplay)
	current_perdisplay = perd;
      if (perd != current_perdisplay)
	{
	  /* We shouldn't get here if we were locked onto one display!  */
	  if (display_locked)
	    abort ();
	  perd->kbd_queue = Fcons (c, perd->kbd_queue);
	  current_perdisplay = perd;
	  longjmp (wrong_display_jmpbuf, 1);
	}
#endif
    }
  /* Terminate Emacs in batch mode if at eof.  */
  if (noninteractive && INTEGERP (c) && XINT (c) < 0)
    Fkill_emacs (make_number (1));

  if (INTEGERP (c))
    {
      /* Add in any extra modifiers, where appropriate.  */
      if ((extra_keyboard_modifiers & CHAR_CTL)
	  || ((extra_keyboard_modifiers & 0177) < ' '
	      && (extra_keyboard_modifiers & 0177) != 0))
	XSETINT (c, make_ctrl_char (XINT (c)));

      /* Transfer any other modifier bits directly from
	 extra_keyboard_modifiers to c.  Ignore the actual character code
	 in the low 16 bits of extra_keyboard_modifiers.  */
      XSETINT (c, XINT (c) | (extra_keyboard_modifiers & ~0xff7f & ~CHAR_CTL));
    }

 non_reread:

  restore_getcjmp (save_jump);

  start_polling ();

  /* Buffer switch events are only for internal wakeups
     so don't show them to the user.  */
  if (BUFFERP (c))
    return c;

  if (key_already_recorded)
    return c;

  /* Wipe the echo area.  */
  echo_area_glyphs = 0;

  /* Handle things that only apply to characters.  */
  if (INTEGERP (c))
    {
      /* If kbd_buffer_get_event gave us an EOF, return that.  */
      if (XINT (c) == -1)
	return c;

      if (STRINGP (Vkeyboard_translate_table)
	  && XSTRING (Vkeyboard_translate_table)->size > XFASTINT (c))
	XSETINT (c, XSTRING (Vkeyboard_translate_table)->data[XFASTINT (c)]);
    }

  /* If this event is a mouse click in the menu bar,
     return just menu-bar for now.  Modify the mouse click event
     so we won't do this twice, then queue it up.  */
  if (EVENT_HAS_PARAMETERS (c)
      && CONSP (XCONS (c)->cdr)
      && CONSP (EVENT_START (c))
      && CONSP (XCONS (EVENT_START (c))->cdr))
    {
      Lisp_Object posn;

      posn = POSN_BUFFER_POSN (EVENT_START (c));
      /* Handle menu-bar events:
	 insert the dummy prefix event `menu-bar'.  */
      if (EQ (posn, Qmenu_bar))
	{
	  /* Change menu-bar to (menu-bar) as the event "position".  */
	  POSN_BUFFER_POSN (EVENT_START (c)) = Fcons (posn, Qnil);

	  also_record = c;
	  Vunread_command_events = Fcons (c, Vunread_command_events);
	  c = posn;
	}
    }

  record_char (c);
  if (! NILP (also_record))
    record_char (also_record);

 from_macro:
 reread_first:

  /* Don't echo mouse motion events.  */
  if (echo_keystrokes
      && ! (EVENT_HAS_PARAMETERS (c)
	    && EQ (EVENT_HEAD_KIND (EVENT_HEAD (c)), Qmouse_movement)))
    {
      echo_char (c);
      if (! NILP (also_record))
	echo_char (also_record);
    }

  /* Record this character as part of the current key.  */
  add_command_key (c);
  if (! NILP (also_record))
    add_command_key (also_record);

  /* Re-reading in the middle of a command */
 reread:
  last_input_char = c;
  num_input_chars++;

  /* Process the help character specially if enabled */
  if (EQ (c, Vhelp_char) && !NILP (Vhelp_form))
    {
      Lisp_Object tem0;
      count = specpdl_ptr - specpdl;

      record_unwind_protect (Fset_window_configuration,
			     Fcurrent_window_configuration (Qnil));

      tem0 = Feval (Vhelp_form);
      if (STRINGP (tem0))
	internal_with_output_to_temp_buffer ("*Help*", print_help, tem0);

      cancel_echoing ();
      do
	c = read_char (0, 0, 0, Qnil, 0);
      while (BUFFERP (c));
      /* Remove the help from the frame */
      unbind_to (count, Qnil);
      prepare_menu_bars ();
      redisplay ();
      if (EQ (c, make_number (040)))
	{
	  cancel_echoing ();
	  do
	    c = read_char (0, 0, 0, Qnil, 0);
	  while (BUFFERP (c));
	}
    }

  return c;
}

/* Record the input event C in various ways.  */

static void
record_char (c)
     Lisp_Object c;
{
  total_keys++;
  XVECTOR (recent_keys)->contents[recent_keys_index] = c;
  if (++recent_keys_index >= NUM_RECENT_KEYS)
    recent_keys_index = 0;

  /* Write c to the dribble file.  If c is a lispy event, write
     the event's symbol to the dribble file, in <brackets>.  Bleaugh.
     If you, dear reader, have a better idea, you've got the source.  :-) */
  if (dribble)
    {
      if (INTEGERP (c))
	{
	  if (XUINT (c) < 0x100)
	    putc (XINT (c), dribble);
	  else
	    fprintf (dribble, " 0x%x", XUINT (c));
	}
      else
	{
	  Lisp_Object dribblee;

	  /* If it's a structured event, take the event header.  */
	  dribblee = EVENT_HEAD (c);

	  if (SYMBOLP (dribblee))
	    {
	      putc ('<', dribble);
	      fwrite (XSYMBOL (dribblee)->name->data, sizeof (char),
		      XSYMBOL (dribblee)->name->size,
		      dribble);
	      putc ('>', dribble);
	    }
	}

      fflush (dribble);
    }

  store_kbd_macro_char (c);

  num_nonmacro_input_chars++;
}

Lisp_Object
print_help (object)
     Lisp_Object object;
{
  struct buffer *old = current_buffer;
  Fprinc (object, Qnil);
  set_buffer_internal (XBUFFER (Vstandard_output));
  call0 (intern ("help-mode"));
  set_buffer_internal (old);
  return Qnil;
}

/* Copy out or in the info on where C-g should throw to.
   This is used when running Lisp code from within get_char,
   in case get_char is called recursively.
   See read_process_output.  */

save_getcjmp (temp)
     jmp_buf temp;
{
  bcopy (getcjmp, temp, sizeof getcjmp);
}

restore_getcjmp (temp)
     jmp_buf temp;
{
  bcopy (temp, getcjmp, sizeof getcjmp);
}


#ifdef HAVE_MOUSE

/* Restore mouse tracking enablement.  See Ftrack_mouse for the only use
   of this function.  */

static Lisp_Object
tracking_off (old_value)
     Lisp_Object old_value;
{
  do_mouse_tracking = old_value;
  if (NILP (old_value))
    {
      /* Redisplay may have been preempted because there was input
	 available, and it assumes it will be called again after the
	 input has been processed.  If the only input available was
	 the sort that we have just disabled, then we need to call
	 redisplay.  */
      if (!readable_events ())
	{
	  prepare_menu_bars ();
	  redisplay_preserve_echo_area ();
	  get_input_pending (&input_pending);
	}
    }
}

DEFUN ("track-mouse", Ftrack_mouse, Strack_mouse, 0, UNEVALLED, 0,
  "Evaluate BODY with mouse movement events enabled.\n\
Within a `track-mouse' form, mouse motion generates input events that\n\
you can read with `read-event'.\n\
Normally, mouse motion is ignored.")
  (args)
     Lisp_Object args;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object val;

  record_unwind_protect (tracking_off, do_mouse_tracking);

  if (!input_pending && !detect_input_pending ())
    prepare_menu_bars ();

  XSETFRAME (do_mouse_tracking, selected_frame);
  
  val = Fprogn (args);
  return unbind_to (count, val);
}

#endif	/* HAVE_MOUSE */

/* Low level keyboard/mouse input.
   kbd_buffer_store_event places events in kbd_buffer, and
   kbd_buffer_get_event retrieves them.
   mouse_moved indicates when the mouse has moved again, and
   *mouse_position_hook provides the mouse position.  */

/* Return true iff there are any events in the queue that read-char
   would return.  If this returns false, a read-char would block.  */
static int
readable_events ()
{
  if (kbd_fetch_ptr != kbd_store_ptr)
    return 1;
#ifdef HAVE_MOUSE
  if (FRAMEP (do_mouse_tracking) && mouse_moved)
    return 1;
#endif
  return 0;
}

/* Set this for debugging, to have a way to get out */
int stop_character;

/* Store an event obtained at interrupt level into kbd_buffer, fifo */

void
kbd_buffer_store_event (event)
     register struct input_event *event;
{
  if (event->kind == no_event)
    abort ();

  if (event->kind == ascii_keystroke)
    {
      register int c = event->code & 0377;

      if (event->modifiers & ctrl_modifier)
	c = make_ctrl_char (c);

      c |= (event->modifiers
	    & (meta_modifier | alt_modifier
	       | hyper_modifier | super_modifier));

      if (c == quit_char)
	{
	  extern SIGTYPE interrupt_signal ();

#ifdef MULTI_FRAME
	  /* If this results in a quit_char being returned to Emacs as
	     input, set Vlast_event_frame properly.  If this doesn't
	     get returned to Emacs as an event, the next event read
	     will set Vlast_event_frame again, so this is safe to do.  */
	  {
	    Lisp_Object focus;
	    PERDISPLAY *perd;

	    focus = FRAME_FOCUS_FRAME (XFRAME (event->frame_or_window));
	    if (NILP (focus))
	      focus = event->frame_or_window;
	    perd = get_perdisplay (XFRAME (focus));
	    perd->internal_last_event_frame = focus;
	    perd->Vlast_event_frame = focus;
	  }
#endif

	  last_event_timestamp = event->timestamp;
	  interrupt_signal ();
	  return;
	}

      if (c && c == stop_character)
	{
	  sys_suspend ();
	  return;
	}
    }

  if (kbd_store_ptr - kbd_buffer == KBD_BUFFER_SIZE)
    kbd_store_ptr = kbd_buffer;

  /* Don't let the very last slot in the buffer become full,
     since that would make the two pointers equal,
     and that is indistinguishable from an empty buffer.
     Discard the event if it would fill the last slot.  */
  if (kbd_fetch_ptr - 1 != kbd_store_ptr)
    {
      volatile struct input_event *sp = kbd_store_ptr;
      sp->kind = event->kind;
      if (event->kind == selection_request_event)
	{
	  /* We must not use the ordinary copying code for this case,
	     since `part' is an enum and copying it might not copy enough
	     in this case.  */
	  bcopy (event, (char *) sp, sizeof (*event));
	}
      else
	{
	  sp->code = event->code;
	  sp->part = event->part;
	  sp->frame_or_window = event->frame_or_window;
	  sp->modifiers = event->modifiers;
	  sp->x = event->x;
	  sp->y = event->y;
	  sp->timestamp = event->timestamp;
	}
      (XVECTOR (kbd_buffer_frame_or_window)->contents[kbd_store_ptr
						      - kbd_buffer]
       = event->frame_or_window);

      kbd_store_ptr++;
    }
}

/* Read one event from the event buffer, waiting if necessary.
   The value is a Lisp object representing the event.
   The value is nil for an event that should be ignored,
   or that was handled here.
   We always read and discard one event.  */

static Lisp_Object
kbd_buffer_get_event (PERDISPLAY **perdp)
{
  register int c;
  Lisp_Object obj;

  if (noninteractive)
    {
      c = getchar ();
      XSETINT (obj, c);
      *perdp = all_perdisplays;  /* There'd better be exactly one!  */
      return obj;
    }

  /* Wait until there is input available.  */
  for (;;)
    {
      if (kbd_fetch_ptr != kbd_store_ptr)
	break;
#ifdef HAVE_MOUSE
      if (FRAMEP (do_mouse_tracking) && mouse_moved)
	break;
#endif

      /* If the quit flag is set, then read_char will return
	 quit_char, so that counts as "available input."  */
      if (!NILP (Vquit_flag))
	quit_throw_to_read_char ();

      /* One way or another, wait until input is available; then, if
	 interrupt handlers have not read it, read it now.  */

#ifdef OLDVMS
      wait_for_kbd_input ();
#else
/* Note SIGIO has been undef'd if FIONREAD is missing.  */
#ifdef SIGIO
      gobble_input (0);
#endif /* SIGIO */
      if (kbd_fetch_ptr != kbd_store_ptr)
	break;
#ifdef HAVE_MOUSE
      if (FRAMEP (do_mouse_tracking) && mouse_moved)
	break;
#endif
      {
	Lisp_Object minus_one;

	XSETINT (minus_one, -1);
	wait_reading_process_input (0, 0, minus_one, 1);

	if (!interrupt_input && kbd_fetch_ptr == kbd_store_ptr)
	  /* Pass 1 for EXPECT since we just waited to have input.  */
	  read_avail_input (1);
      }
#endif /* not VMS */
    }

  /* At this point, we know that there is a readable event available
     somewhere.  If the event queue is empty, then there must be a
     mouse movement enabled and available.  */
  if (kbd_fetch_ptr != kbd_store_ptr)
    {
      struct input_event *event;

      event = ((kbd_fetch_ptr < kbd_buffer + KBD_BUFFER_SIZE)
	       ? kbd_fetch_ptr
	       : kbd_buffer);

      last_event_timestamp = event->timestamp;

      {
	Lisp_Object frame;
	frame = event->frame_or_window;
	if (CONSP (frame))
	  frame = XCONS (frame)->car;
	else if (WINDOWP (frame))
	  frame = WINDOW_FRAME (XWINDOW (frame));
	if (!FRAMEP (frame))
	  abort ();
	*perdp = get_perdisplay (XFRAME (frame));
      }

      obj = Qnil;

      /* These two kinds of events get special handling
	 and don't actually appear to the command loop.
	 We return nil for them.  */
      if (event->kind == selection_request_event)
	{
#ifdef HAVE_X11
	  struct input_event copy = *event;
	  /* Remove it from the buffer before processing it,
	     since otherwise swallow_events will see it
	     and process it again.  */
	  kbd_fetch_ptr = event + 1;
	  x_handle_selection_request (&copy);
#else
	  /* We're getting selection request events, but we don't have
             a window system.  */
	  abort ();
#endif
	}

      else if (event->kind == selection_clear_event)
	{
#ifdef HAVE_X11
	  x_handle_selection_clear (event);
	  kbd_fetch_ptr = event + 1;
#else
	  /* We're getting selection request events, but we don't have
             a window system.  */
	  abort ();
#endif
	}
#ifdef HAVE_X11
      else if (event->kind == delete_window_event)
	{
	  /* Make an event (delete-frame (FRAME)).  */
	  obj = Fcons (event->frame_or_window, Qnil);
	  obj = Fcons (Qdelete_frame, Fcons (obj, Qnil));
	  kbd_fetch_ptr = event + 1;
	}
      else if (event->kind == iconify_event)
	{
	  /* Make an event (iconify-frame (FRAME)).  */
	  obj = Fcons (event->frame_or_window, Qnil);
	  obj = Fcons (Qiconify_frame, Fcons (obj, Qnil));
	  kbd_fetch_ptr = event + 1;
	}
      else if (event->kind == deiconify_event)
	{
	  /* Make an event (make-frame-visible (FRAME)).  */
	  obj = Fcons (event->frame_or_window, Qnil);
	  obj = Fcons (Qmake_frame_visible, Fcons (obj, Qnil));
	  kbd_fetch_ptr = event + 1;
	}
#endif
      else if (event->kind == menu_bar_event)
	{
	  /* The event value is in the cdr of the frame_or_window slot.  */
	  if (!CONSP (event->frame_or_window))
	    abort ();
	  obj = XCONS (event->frame_or_window)->cdr;
	  kbd_fetch_ptr = event + 1;
	}
      else if (event->kind == buffer_switch_event)
	{
	  /* The value doesn't matter here; only the type is tested.  */
	  XSETBUFFER (obj, current_buffer);
	  kbd_fetch_ptr = event + 1;
	}
      /* Just discard these, by returning nil.
	 (They shouldn't be found in the buffer,
	 but on some machines it appears they do show up.)  */
      else if (event->kind == no_event)
	kbd_fetch_ptr = event + 1;

      /* If this event is on a different frame, return a switch-frame this
	 time, and leave the event in the queue for next time.  */
      else
	{
#ifdef MULTI_FRAME
	  Lisp_Object frame;
	  Lisp_Object focus;

	  frame = event->frame_or_window;
	  if (WINDOWP (frame))
	    frame = WINDOW_FRAME (XWINDOW (frame));

	  focus = FRAME_FOCUS_FRAME (XFRAME (frame));
	  if (! NILP (focus))
	    frame = focus;

	  if (! EQ (frame, (*perdp)->internal_last_event_frame)
	      && XFRAME (frame) != selected_frame)
	    obj = make_lispy_switch_frame (frame);
	  (*perdp)->internal_last_event_frame = frame;
#endif /* MULTI_FRAME */

	  /* If we didn't decide to make a switch-frame event, go ahead
	     and build a real event from the queue entry.  */

	  if (NILP (obj))
	    {
	      obj = make_lispy_event (event);

	      /* Wipe out this event, to catch bugs.  */
	      event->kind = no_event;
	      XVECTOR (kbd_buffer_frame_or_window)->contents[event - kbd_buffer] = Qnil;

	      kbd_fetch_ptr = event + 1;
	    }
	}
    }
#ifdef HAVE_MOUSE
  /* Try generating a mouse motion event.  */
  else if (FRAMEP (do_mouse_tracking) && mouse_moved)
    {
      FRAME_PTR f = XFRAME (do_mouse_tracking);
      Lisp_Object bar_window;
      enum scroll_bar_part part;
      Lisp_Object x, y;
      unsigned long time;

      if (!current_perdisplay)
	abort ();

      *perdp = current_perdisplay;
      /* Note that this uses F to determine which display to look at.
	 If there is no valid info, it does not store anything
	 so x remains nil.  */
      x = Qnil;
      (*mouse_position_hook) (&f, &bar_window, &part, &x, &y, &time);

      obj = Qnil;

#ifdef MULTI_FRAME
      /* Decide if we should generate a switch-frame event.  Don't
	 generate switch-frame events for motion outside of all Emacs
	 frames.  */
      if (!NILP (x) && f)
	{
	  Lisp_Object frame;

	  frame = FRAME_FOCUS_FRAME (f);
	  if (NILP (frame))
	    XSETFRAME (frame, f);

	  if (! EQ (frame, (*perdp)->internal_last_event_frame)
	      && XFRAME (frame) != selected_frame)
	    obj = make_lispy_switch_frame (frame);
	  (*perdp)->internal_last_event_frame = frame;
	}
#endif

      /* If we didn't decide to make a switch-frame event, go ahead and 
	 return a mouse-motion event.  */
      if (!NILP (x) && NILP (obj))
	obj = make_lispy_movement (f, bar_window, part, x, y, time);
    }
#endif	/* HAVE_MOUSE */
  else
    /* We were promised by the above while loop that there was
       something for us to read!  */
    abort ();

  input_pending = readable_events ();

#ifdef MULTI_FRAME
  (*perdp)->Vlast_event_frame = (*perdp)->internal_last_event_frame;
#endif

  return (obj);
}

/* Process any events that are not user-visible,
   then return, without reading any user-visible events.  */

void
swallow_events ()
{
  while (kbd_fetch_ptr != kbd_store_ptr)
    {
      struct input_event *event;

      event = ((kbd_fetch_ptr < kbd_buffer + KBD_BUFFER_SIZE)
	       ? kbd_fetch_ptr
	       : kbd_buffer);

      last_event_timestamp = event->timestamp;

      /* These two kinds of events get special handling
	 and don't actually appear to the command loop.  */
      if (event->kind == selection_request_event)
	{
#ifdef HAVE_X11
	  struct input_event copy;
	  copy = *event;
	  kbd_fetch_ptr = event + 1;
	  x_handle_selection_request (&copy);
#else
	  /* We're getting selection request events, but we don't have
             a window system.  */
	  abort ();
#endif
	}

      else if (event->kind == selection_clear_event)
	{
#ifdef HAVE_X11
	  x_handle_selection_clear (event);
	  kbd_fetch_ptr = event + 1;
#else
	  /* We're getting selection request events, but we don't have
             a window system.  */
	  abort ();
#endif
	}
      else
	break;
    }

  get_input_pending (&input_pending);
}

/* Caches for modify_event_symbol.  */
static Lisp_Object accent_key_syms;
static Lisp_Object system_key_syms;
static Lisp_Object func_key_syms;
static Lisp_Object mouse_syms;

Lisp_Object Vsystem_key_alist;

/* This is a list of keysym codes for special "accent" characters.
   It parallels lispy_accent_keys.  */

static int lispy_accent_codes[] =
{
#ifdef XK_dead_circumflex
  XK_dead_circumflex,
#else
  0,
#endif
#ifdef XK_dead_grave
  XK_dead_grave,
#else
  0,
#endif
#ifdef XK_dead_tilde
  XK_dead_tilde,
#else
  0,
#endif
#ifdef XK_dead_diaeresis
  XK_dead_diaeresis,
#else
  0,
#endif
#ifdef XK_dead_macron
  XK_dead_macron,
#else
  0,
#endif
#ifdef XK_dead_degree
  XK_dead_degree,
#else
  0,
#endif
#ifdef XK_dead_acute
  XK_dead_acute,
#else
  0,
#endif
#ifdef XK_dead_cedilla
  XK_dead_cedilla,
#else
  0,
#endif
#ifdef XK_dead_breve
  XK_dead_breve,
#else
  0,
#endif
#ifdef XK_dead_ogonek
  XK_dead_ogonek,
#else
  0,
#endif
#ifdef XK_dead_caron
  XK_dead_caron,
#else
  0,
#endif
#ifdef XK_dead_doubleacute
  XK_dead_doubleacute,
#else
  0,
#endif
#ifdef XK_dead_abovedot
  XK_dead_abovedot,
#else
  0,
#endif
};

/* This is a list of Lisp names for special "accent" characters.
   It parallels lispy_accent_codes.  */

static char *lispy_accent_keys[] =
{
  "dead-circumflex",
  "dead-grave",
  "dead-tilde",
  "dead-diaeresis",
  "dead-macron",
  "dead-degree",
  "dead-acute",
  "dead-cedilla",
  "dead-breve",
  "dead-ogonek",
  "dead-caron",
  "dead-doubleacute",
  "dead-abovedot",
};

/* You'll notice that this table is arranged to be conveniently
   indexed by X Windows keysym values.  */
static char *lispy_function_keys[] =
  {
    /* X Keysym value */

    0, 0, 0, 0, 0, 0, 0, 0,	/* 0xff00 */
    "backspace",
    "tab",
    "linefeed",
    "clear",
    0,
    "return",
    0, 0,
    0, 0, 0,			/* 0xff10 */
    "pause",
    0, 0, 0, 0, 0, 0, 0,
    "escape",
    0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   /* 0xff20...2f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   /* 0xff30...3f */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   /* 0xff40...4f */

    "home",			/* 0xff50 */	/* IsCursorKey */
    "left",
    "up",
    "right",
    "down",
    "prior",
    "next",
    "end",
    "begin",
    0,				/* 0xff59 */
    0, 0, 0, 0, 0, 0,
    "select",			/* 0xff60 */	/* IsMiscFunctionKey */
    "print",
    "execute",
    "insert",
    0,		/* 0xff64 */
    "undo",
    "redo",
    "menu",
    "find",
    "cancel",
    "help",
    "break",			/* 0xff6b */

    0, 0, 0, 0, 0, 0, 0, 0, "backtab", 0,
    0,				/* 0xff76 */
    0, 0, 0, 0, 0, 0, 0, 0, "kp-numlock",	/* 0xff7f */
    "kp-space",			/* 0xff80 */	/* IsKeypadKey */
    0, 0, 0, 0, 0, 0, 0, 0,
    "kp-tab",			/* 0xff89 */
    0, 0, 0,
    "kp-enter",			/* 0xff8d */
    0, 0, 0,
    "kp-f1",			/* 0xff91 */
    "kp-f2",
    "kp-f3",
    "kp-f4",
    "kp-home",			/* 0xff95 */
    "kp-left",
    "kp-up",
    "kp-right",
    "kp-down",
    "kp-prior",			/* kp-page-up */
    "kp-next",			/* kp-page-down */
    "kp-end",
    "kp-begin",
    "kp-insert",
    "kp-delete",
    0,				/* 0xffa0 */
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    "kp-multiply",		/* 0xffaa */
    "kp-add",
    "kp-separator",
    "kp-subtract",
    "kp-decimal",
    "kp-divide",		/* 0xffaf */
    "kp-0",			/* 0xffb0 */
    "kp-1",	"kp-2",	"kp-3",	"kp-4",	"kp-5",	"kp-6",	"kp-7",	"kp-8",	"kp-9",
    0,		/* 0xffba */
    0, 0,
    "kp-equal",			/* 0xffbd */
    "f1",			/* 0xffbe */	/* IsFunctionKey */
    "f2",
    "f3", "f4", "f5", "f6", "f7", "f8",	"f9", "f10", /* 0xffc0 */
    "f11", "f12", "f13", "f14", "f15", "f16", "f17", "f18",
    "f19", "f20", "f21", "f22", "f23", "f24", "f25", "f26", /* 0xffd0 */
    "f27", "f28", "f29", "f30", "f31", "f32", "f33", "f34",
    "f35", 0, 0, 0, 0, 0, 0, 0,	/* 0xffe0 */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,     /* 0xfff0 */
    0, 0, 0, 0, 0, 0, 0, "delete"
    };

static char *lispy_mouse_names[] = 
{
  "mouse-1", "mouse-2", "mouse-3", "mouse-4", "mouse-5"
};

/* Scroll bar parts.  */
Lisp_Object Qabove_handle, Qhandle, Qbelow_handle;

/* An array of scroll bar parts, indexed by an enum scroll_bar_part value.  */
Lisp_Object *scroll_bar_parts[] = {
  &Qabove_handle, &Qhandle, &Qbelow_handle
};


/* A vector, indexed by button number, giving the down-going location
   of currently depressed buttons, both scroll bar and non-scroll bar.

   The elements have the form
     (BUTTON-NUMBER MODIFIER-MASK . REST)
   where REST is the cdr of a position as it would be reported in the event.

   The make_lispy_event function stores positions here to tell the
   difference between click and drag events, and to store the starting
   location to be included in drag events.  */

static Lisp_Object button_down_location;

/* Information about the most recent up-going button event:  Which
   button, what location, and what time. */

static int last_mouse_button;
static int last_mouse_x;
static int last_mouse_y;
static unsigned long button_down_time;

/* The maximum time between clicks to make a double-click,
   or Qnil to disable double-click detection,
   or Qt for no time limit.  */
Lisp_Object Vdouble_click_time;

/* The number of clicks in this multiple-click. */

int double_click_count;

#ifdef USE_X_TOOLKIT
extern Lisp_Object map_event_to_object ();
#endif /* USE_X_TOOLKIT  */

/* Given a struct input_event, build the lisp event which represents
   it.  If EVENT is 0, build a mouse movement event from the mouse
   movement buffer, which should have a movement event in it.

   Note that events must be passed to this function in the order they
   are received; this function stores the location of button presses
   in order to build drag events when the button is released.  */

static Lisp_Object
make_lispy_event (event)
     struct input_event *event;
{
  int i;

  switch (SWITCH_ENUM_CAST (event->kind))
    {
      /* A simple keystroke.  */
    case ascii_keystroke:
      {
	Lisp_Object lispy_c;
	int c = event->code & 0377;
	/* Turn ASCII characters into control characters
	   when proper.  */
	if (event->modifiers & ctrl_modifier)
	  c = make_ctrl_char (c);

	/* Add in the other modifier bits.  We took care of ctrl_modifier
	   just above, and the shift key was taken care of by the X code,
	   and applied to control characters by make_ctrl_char.  */
	c |= (event->modifiers
	      & (meta_modifier | alt_modifier
		 | hyper_modifier | super_modifier));
	button_down_time = 0;
	XSETFASTINT (lispy_c, c);
	return lispy_c;
      }

      /* A function key.  The symbol may need to have modifier prefixes
	 tacked onto it.  */
    case non_ascii_keystroke:
      button_down_time = 0;

      for (i = 0; i < sizeof (lispy_accent_codes) / sizeof (int); i++)
	if (event->code == lispy_accent_codes[i])
	  return modify_event_symbol (i,
				      event->modifiers,
				      Qfunction_key, Qnil,
				      lispy_accent_keys, &accent_key_syms,
				      (sizeof (lispy_accent_keys)
				       / sizeof (lispy_accent_keys[0])));

      /* Handle system-specific keysyms.  */
      if (event->code & (1 << 28))
	{
	  /* We need to use an alist rather than a vector as the cache
	     since we can't make a vector long enuf.  */
	  if (NILP (system_key_syms))
	    system_key_syms = Fcons (Qnil, Qnil);
	  return modify_event_symbol (event->code & 0xffffff,
				      event->modifiers,
				      Qfunction_key, Vsystem_key_alist,
				      0, &system_key_syms, 0xffffff);
	}

      return modify_event_symbol (event->code - 0xff00,
				  event->modifiers,
				  Qfunction_key, Qnil,
				  lispy_function_keys, &func_key_syms,
				  (sizeof (lispy_function_keys)
				   / sizeof (lispy_function_keys[0])));
      break;

#if defined (MULTI_FRAME) || defined (HAVE_MOUSE)
      /* A mouse click.  Figure out where it is, decide whether it's 
         a press, click or drag, and build the appropriate structure.  */
    case mouse_click:
    case scroll_bar_click:
      {
	int button = event->code;
	int is_double;
	Lisp_Object position;
	Lisp_Object *start_pos_ptr;
	Lisp_Object start_pos;

	if (button < 0 || button >= NUM_MOUSE_BUTTONS)
	  abort ();

	/* Build the position as appropriate for this mouse click.  */
	if (event->kind == mouse_click)
	  {
	    int part;
	    FRAME_PTR f = XFRAME (event->frame_or_window);
	    Lisp_Object window;
	    Lisp_Object posn;
	    int row, column;

	    /* Ignore mouse events that were made on frame that
	       have been deleted.  */
	    if (! FRAME_LIVE_P (f))
	      return Qnil;

	    pixel_to_glyph_coords (f, XINT (event->x), XINT (event->y),
				   &column, &row, 0, 1);

#ifndef USE_X_TOOLKIT
	    /* In the non-toolkit version, clicks on the menu bar
	       are ordinary button events in the event buffer.
	       Distinguish them, and invoke the menu.

	       (In the toolkit version, the toolkit handles the menu bar
	       and Emacs doesn't know about it until after the user
	       makes a selection.)  */
	    if (row >= 0 && row < FRAME_MENU_BAR_LINES (f))
	      {
		Lisp_Object items, item;
		int hpos;
		int i;

		/* Activate the menu bar on the down event.  If the
		   up event comes in before the menu code can deal with it,
		   just ignore it.  */
		if (! (event->modifiers & down_modifier))
		  return Qnil;

		item = Qnil;
		items = FRAME_MENU_BAR_ITEMS (f);
		for (i = 0; i < XVECTOR (items)->size; i += 3)
		  {
		    Lisp_Object pos, string;
		    string = XVECTOR (items)->contents[i + 1];
		    pos = XVECTOR (items)->contents[i + 2];
		    if (NILP (string))
		      break;
		    if (column >= XINT (pos)
			&& column < XINT (pos) + XSTRING (string)->size)
		      {
			item = XVECTOR (items)->contents[i];
			break;
		      }
		  }

		position
		  = Fcons (event->frame_or_window,
			   Fcons (Qmenu_bar,
				  Fcons (Fcons (event->x, event->y),
					 Fcons (make_number (event->timestamp),
						Qnil))));

		return Fcons (item, Fcons (position, Qnil));
	      }
#endif /* not USE_X_TOOLKIT */

	    window = window_from_coordinates (f, column, row, &part);

	    if (!WINDOWP (window))
	      {
		window = event->frame_or_window;
		posn = Qnil;
	      }
	    else
	      {
		int pixcolumn, pixrow;
		column -= XINT (XWINDOW (window)->left);
		row -= XINT (XWINDOW (window)->top);
		glyph_to_pixel_coords (f, column, row, &pixcolumn, &pixrow);
		XSETINT (event->x, pixcolumn);
		XSETINT (event->y, pixrow);

		if (part == 1)
		  posn = Qmode_line;
		else if (part == 2)
		  posn = Qvertical_line;
		else
		  XSETINT (posn,
			   buffer_posn_from_coords (XWINDOW (window),
						    column, row));
	      }

	    position
	      = Fcons (window,
		       Fcons (posn,
			      Fcons (Fcons (event->x, event->y),
				     Fcons (make_number (event->timestamp),
					    Qnil))));
	  }
	else
	  {
	    Lisp_Object window;
	    Lisp_Object portion_whole;
	    Lisp_Object part;

	    window = event->frame_or_window;
	    portion_whole = Fcons (event->x, event->y);
	    part = *scroll_bar_parts[(int) event->part];

	    position =
	      Fcons (window,
		     Fcons (Qvertical_scroll_bar,
			    Fcons (portion_whole,
				   Fcons (make_number (event->timestamp),
					  Fcons (part, Qnil)))));
	  }

	start_pos_ptr = &XVECTOR (button_down_location)->contents[button];

	start_pos = *start_pos_ptr;
	*start_pos_ptr = Qnil;

	is_double = (button == last_mouse_button
		     && XINT (event->x) == last_mouse_x
		     && XINT (event->y) == last_mouse_y
		     && button_down_time != 0
		     && (EQ (Vdouble_click_time, Qt)
			 || (INTEGERP (Vdouble_click_time)
			     && ((int)(event->timestamp - button_down_time)
				 < XINT (Vdouble_click_time)))));
	last_mouse_button = button;
	last_mouse_x = XINT (event->x);
	last_mouse_y = XINT (event->y);

	/* If this is a button press, squirrel away the location, so
           we can decide later whether it was a click or a drag.  */
	if (event->modifiers & down_modifier)
	  {
	    if (is_double)
	      {
		double_click_count++;
		event->modifiers |= ((double_click_count > 2)
				     ? triple_modifier
				     : double_modifier);
	      }
	    else
	      double_click_count = 1;
	    button_down_time = event->timestamp;
	    *start_pos_ptr = Fcopy_alist (position);
	  }

	/* Now we're releasing a button - check the co-ordinates to
           see if this was a click or a drag.  */
	else if (event->modifiers & up_modifier)
	  {
	    /* If we did not see a down before this up,
	       ignore the up.  Probably this happened because
	       the down event chose a menu item.
	       It would be an annoyance to treat the release
	       of the button that chose the menu item
	       as a separate event.  */

	    if (!CONSP (start_pos))
	      return Qnil;

	    event->modifiers &= ~up_modifier;
#if 0 /* Formerly we treated an up with no down as a click event.  */
	    if (!CONSP (start_pos))
	      event->modifiers |= click_modifier;
	    else
#endif
	      {
		/* The third element of every position should be the (x,y)
		   pair.  */
		Lisp_Object down;

		down = Fnth (make_number (2), start_pos);
		if (EQ (event->x, XCONS (down)->car)
		    && EQ (event->y, XCONS (down)->cdr))
		  {
		    event->modifiers |= click_modifier;
		  }
		else
		  {
		    button_down_time = 0;
		    event->modifiers |= drag_modifier;
		  }
		/* Don't check is_double; treat this as multiple
		   if the down-event was multiple.  */
		if (double_click_count > 1)
		  event->modifiers |= ((double_click_count > 2)
				       ? triple_modifier
				       : double_modifier);
	      }
	  }
	else
	  /* Every mouse event should either have the down_modifier or
             the up_modifier set.  */
	  abort ();

	{
	  /* Get the symbol we should use for the mouse click.  */
	  Lisp_Object head;

	  head = modify_event_symbol (button,
				      event->modifiers,
				      Qmouse_click, Qnil,
				      lispy_mouse_names, &mouse_syms,
				      (sizeof (lispy_mouse_names)
				       / sizeof (lispy_mouse_names[0])));
	  if (event->modifiers & drag_modifier)
	    return Fcons (head,
			  Fcons (start_pos,
				 Fcons (position,
					Qnil)));
	  else if (event->modifiers & (double_modifier | triple_modifier))
	    return Fcons (head,
			  Fcons (position,
				 Fcons (make_number (double_click_count),
					Qnil)));
	  else
	    return Fcons (head,
			  Fcons (position,
				 Qnil));
	}
      }
#endif /* MULTI_FRAME or HAVE_MOUSE */

      /* The 'kind' field of the event is something we don't recognize.  */
    default:
      abort ();
    }
}

#if defined (MULTI_FRAME) || defined (HAVE_MOUSE)

static Lisp_Object
make_lispy_movement (frame, bar_window, part, x, y, time)
     FRAME_PTR frame;
     Lisp_Object bar_window;
     enum scroll_bar_part part;
     Lisp_Object x, y;
     unsigned long time;
{
#ifdef MULTI_FRAME
  /* Is it a scroll bar movement?  */
  if (frame && ! NILP (bar_window))
    {
      Lisp_Object part_sym;

      part_sym = *scroll_bar_parts[(int) part];
      return Fcons (Qscroll_bar_movement,
		    (Fcons (Fcons (bar_window,
				   Fcons (Qvertical_scroll_bar,
					  Fcons (Fcons (x, y),
						 Fcons (make_number (time),
							Fcons (part_sym,
							       Qnil))))),
			    Qnil)));
    }

  /* Or is it an ordinary mouse movement?  */
  else
#endif /* MULTI_FRAME */
    {
      int area;
      Lisp_Object window;
      Lisp_Object posn;
      int column, row;

#ifdef MULTI_FRAME
      if (frame)
#else
      if (1)
#endif
	{
	  /* It's in a frame; which window on that frame?  */
	  pixel_to_glyph_coords (frame, XINT (x), XINT (y), &column, &row, 0, 1);
	  window = window_from_coordinates (frame, column, row, &area);
	}
      else
	window = Qnil;

      if (WINDOWP (window))
	{
	  int pixcolumn, pixrow;
	  column -= XINT (XWINDOW (window)->left);
	  row -= XINT (XWINDOW (window)->top);
	  glyph_to_pixel_coords (frame, column, row, &pixcolumn, &pixrow);
	  XSETINT (x, pixcolumn);
	  XSETINT (y, pixrow);

	  if (area == 1)
	    posn = Qmode_line;
	  else if (area == 2)
	    posn = Qvertical_line;
	  else
	    XSETINT (posn,
		     buffer_posn_from_coords (XWINDOW (window), column, row));
	}
#ifdef MULTI_FRAME
      else if (frame != 0)
	{
	  XSETFRAME (window, frame);
	  posn = Qnil;
	}
#endif
      else
	{
	  window = Qnil;
	  posn = Qnil;
	  XSETFASTINT (x, 0);
	  XSETFASTINT (y, 0);
	}

      return Fcons (Qmouse_movement,
		    Fcons (Fcons (window,
				  Fcons (posn,
					 Fcons (Fcons (x, y),
						Fcons (make_number (time),
						       Qnil)))),
			   Qnil));
    }
}

#endif /* neither MULTI_FRAME nor HAVE_MOUSE */

/* Construct a switch frame event.  */
static Lisp_Object
make_lispy_switch_frame (frame)
     Lisp_Object frame;
{
  return Fcons (Qswitch_frame, Fcons (frame, Qnil));
}

/* Manipulating modifiers.  */

/* Parse the name of SYMBOL, and return the set of modifiers it contains.

   If MODIFIER_END is non-zero, set *MODIFIER_END to the position in
   SYMBOL's name of the end of the modifiers; the string from this
   position is the unmodified symbol name.

   This doesn't use any caches.  */
static int
parse_modifiers_uncached (symbol, modifier_end)
     Lisp_Object symbol;
     int *modifier_end;
{
  struct Lisp_String *name;
  int i;
  int modifiers;

  CHECK_SYMBOL (symbol, 1);
  
  modifiers = 0;
  name = XSYMBOL (symbol)->name;


  for (i = 0; i+2 <= name->size; )
    switch (name->data[i])
      {
#define SINGLE_LETTER_MOD(bit)					\
        if (name->data[i+1] != '-')				\
	  goto no_more_modifiers;				\
	modifiers |= bit;					\
	i += 2;

      case 'A':
	SINGLE_LETTER_MOD (alt_modifier);
	break;

      case 'C':
	SINGLE_LETTER_MOD (ctrl_modifier);
	break;

      case 'H':
	SINGLE_LETTER_MOD (hyper_modifier);
	break;

      case 'M':
	SINGLE_LETTER_MOD (meta_modifier);
	break;

      case 'S':
	SINGLE_LETTER_MOD (shift_modifier);
	break;

      case 's':
	SINGLE_LETTER_MOD (super_modifier);
	break;

      case 'd':
	if (i + 5 > name->size)
	  goto no_more_modifiers;
	if (! strncmp (name->data + i, "drag-", 5))
	  {
	    modifiers |= drag_modifier;
	    i += 5;
	  }
	else if (! strncmp (name->data + i, "down-", 5))
	  {
	    modifiers |= down_modifier;
	    i += 5;
	  }
	else if (i + 7 <= name->size
		 && ! strncmp (name->data + i, "double-", 7))
	  {
	    modifiers |= double_modifier;
	    i += 7;
	  }
	else
	  goto no_more_modifiers;
	break;

      case 't':
	if (i + 7 > name->size)
	  goto no_more_modifiers;
	if (! strncmp (name->data + i, "triple-", 7))
	  {
	    modifiers |= triple_modifier;
	    i += 7;
	  }
	else
	  goto no_more_modifiers;
	break;

      default:
	goto no_more_modifiers;

#undef SINGLE_LETTER_MOD
      }
 no_more_modifiers:

  /* Should we include the `click' modifier?  */
  if (! (modifiers & (down_modifier | drag_modifier
		      | double_modifier | triple_modifier))
      && i + 7 == name->size
      && strncmp (name->data + i, "mouse-", 6) == 0
      && ('0' <= name->data[i + 6] && name->data[i + 6] <= '9'))
    modifiers |= click_modifier;

  if (modifier_end)
    *modifier_end = i;

  return modifiers;
}


/* Return a symbol whose name is the modifier prefixes for MODIFIERS
   prepended to the string BASE[0..BASE_LEN-1].
   This doesn't use any caches.  */
static Lisp_Object
apply_modifiers_uncached (modifiers, base, base_len)
     int modifiers;
     char *base;
     int base_len;
{
  /* Since BASE could contain nulls, we can't use intern here; we have
     to use Fintern, which expects a genuine Lisp_String, and keeps a
     reference to it.  */
  char *new_mods =
    (char *) alloca (sizeof ("A-C-H-M-S-s-down-drag-double-triple-"));
  int mod_len;

  {
    char *p = new_mods;

    /* Only the event queue may use the `up' modifier; it should always
       be turned into a click or drag event before presented to lisp code.  */
    if (modifiers & up_modifier)
      abort ();

    if (modifiers & alt_modifier)   { *p++ = 'A'; *p++ = '-'; }
    if (modifiers & ctrl_modifier)  { *p++ = 'C'; *p++ = '-'; }
    if (modifiers & hyper_modifier) { *p++ = 'H'; *p++ = '-'; }
    if (modifiers & meta_modifier)  { *p++ = 'M'; *p++ = '-'; }
    if (modifiers & shift_modifier) { *p++ = 'S'; *p++ = '-'; }
    if (modifiers & super_modifier) { *p++ = 's'; *p++ = '-'; }
    if (modifiers & double_modifier)  { strcpy (p, "double-");  p += 7; }
    if (modifiers & triple_modifier)  { strcpy (p, "triple-");  p += 7; }
    if (modifiers & down_modifier)  { strcpy (p, "down-");  p += 5; }
    if (modifiers & drag_modifier)  { strcpy (p, "drag-");  p += 5; }
    /* The click modifier is denoted by the absence of other modifiers.  */

    *p = '\0';

    mod_len = p - new_mods;
  }

  {
    Lisp_Object new_name;
    
    new_name = make_uninit_string (mod_len + base_len);
    bcopy (new_mods, XSTRING (new_name)->data,	       mod_len);
    bcopy (base,     XSTRING (new_name)->data + mod_len, base_len);

    return Fintern (new_name, Qnil);
  }
}


static char *modifier_names[] =
{
  "up", "down", "drag", "click", "double", "triple", 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, "alt", "super", "hyper", "shift", "control", "meta"
};
#define NUM_MOD_NAMES (sizeof (modifier_names) / sizeof (modifier_names[0]))

static Lisp_Object modifier_symbols;

/* Return the list of modifier symbols corresponding to the mask MODIFIERS.  */
static Lisp_Object
lispy_modifier_list (modifiers)
     int modifiers;
{
  Lisp_Object modifier_list;
  int i;

  modifier_list = Qnil;
  for (i = 0; (1<<i) <= modifiers && i < NUM_MOD_NAMES; i++)
    if (modifiers & (1<<i))
      modifier_list = Fcons (XVECTOR (modifier_symbols)->contents[i],
			     modifier_list);

  return modifier_list;
}


/* Parse the modifiers on SYMBOL, and return a list like (UNMODIFIED MASK),
   where UNMODIFIED is the unmodified form of SYMBOL,
   MASK is the set of modifiers present in SYMBOL's name.
   This is similar to parse_modifiers_uncached, but uses the cache in
   SYMBOL's Qevent_symbol_element_mask property, and maintains the
   Qevent_symbol_elements property.  */
static Lisp_Object
parse_modifiers (symbol)
     Lisp_Object symbol;
{
  Lisp_Object elements;

  elements = Fget (symbol, Qevent_symbol_element_mask);
  if (CONSP (elements))
    return elements;
  else
    {
      int end;
      int modifiers = parse_modifiers_uncached (symbol, &end);
      Lisp_Object unmodified;
      Lisp_Object mask;

      unmodified = Fintern (make_string (XSYMBOL (symbol)->name->data + end,
					 XSYMBOL (symbol)->name->size - end),
			    Qnil);

      if (modifiers & ~((1<<VALBITS) - 1))
	abort ();
      XSETFASTINT (mask, modifiers);
      elements = Fcons (unmodified, Fcons (mask, Qnil));

      /* Cache the parsing results on SYMBOL.  */
      Fput (symbol, Qevent_symbol_element_mask,
	    elements);
      Fput (symbol, Qevent_symbol_elements,
	    Fcons (unmodified, lispy_modifier_list (modifiers)));

      /* Since we know that SYMBOL is modifiers applied to unmodified,
	 it would be nice to put that in unmodified's cache.
	 But we can't, since we're not sure that parse_modifiers is
	 canonical.  */

      return elements;
    }
}

/* Apply the modifiers MODIFIERS to the symbol BASE.
   BASE must be unmodified.

   This is like apply_modifiers_uncached, but uses BASE's
   Qmodifier_cache property, if present.  It also builds
   Qevent_symbol_elements properties, since it has that info anyway.

   apply_modifiers copies the value of BASE's Qevent_kind property to
   the modified symbol.  */
static Lisp_Object
apply_modifiers (modifiers, base)
     int modifiers;
     Lisp_Object base;
{
  Lisp_Object cache, index, entry, new_symbol;

  /* Mask out upper bits.  We don't know where this value's been.  */
  modifiers &= (1<<VALBITS) - 1;

  /* The click modifier never figures into cache indices.  */
  cache = Fget (base, Qmodifier_cache);
  XSETFASTINT (index, (modifiers & ~click_modifier));
  entry = assq_no_quit (index, cache);

  if (CONSP (entry))
    new_symbol = XCONS (entry)->cdr;
  else
    {
      /* We have to create the symbol ourselves.  */  
      new_symbol = apply_modifiers_uncached (modifiers,
					     XSYMBOL (base)->name->data,
					     XSYMBOL (base)->name->size);

      /* Add the new symbol to the base's cache.  */
      entry = Fcons (index, new_symbol);
      Fput (base, Qmodifier_cache, Fcons (entry, cache));

      /* We have the parsing info now for free, so add it to the caches.  */
      XSETFASTINT (index, modifiers);
      Fput (new_symbol, Qevent_symbol_element_mask,
	    Fcons (base, Fcons (index, Qnil)));
      Fput (new_symbol, Qevent_symbol_elements,
	    Fcons (base, lispy_modifier_list (modifiers)));
    }

  /* Make sure this symbol is of the same kind as BASE.  

     You'd think we could just set this once and for all when we
     intern the symbol above, but reorder_modifiers may call us when
     BASE's property isn't set right; we can't assume that just
     because it has a Qmodifier_cache property it must have its
     Qevent_kind set right as well.  */
  if (NILP (Fget (new_symbol, Qevent_kind)))
    {
      Lisp_Object kind;

      kind = Fget (base, Qevent_kind);
      if (! NILP (kind))
	Fput (new_symbol, Qevent_kind, kind);
    }

  return new_symbol;
}


/* Given a symbol whose name begins with modifiers ("C-", "M-", etc),
   return a symbol with the modifiers placed in the canonical order.
   Canonical order is alphabetical, except for down and drag, which
   always come last.  The 'click' modifier is never written out.

   Fdefine_key calls this to make sure that (for example) C-M-foo
   and M-C-foo end up being equivalent in the keymap.  */

Lisp_Object
reorder_modifiers (symbol)
     Lisp_Object symbol;
{
  /* It's hopefully okay to write the code this way, since everything
     will soon be in caches, and no consing will be done at all.  */
  Lisp_Object parsed;

  parsed = parse_modifiers (symbol);
  return apply_modifiers (XCONS (XCONS (parsed)->cdr)->car,
			  XCONS (parsed)->car);
}


/* For handling events, we often want to produce a symbol whose name
   is a series of modifier key prefixes ("M-", "C-", etcetera) attached
   to some base, like the name of a function key or mouse button.
   modify_event_symbol produces symbols of this sort.

   NAME_TABLE should point to an array of strings, such that NAME_TABLE[i]
   is the name of the i'th symbol.  TABLE_SIZE is the number of elements
   in the table.

   Alternatively, NAME_ALIST is an alist mapping codes into symbol names.
   NAME_ALIST is used if it is non-nil; otherwise NAME_TABLE is used.

   SYMBOL_TABLE should be a pointer to a Lisp_Object whose value will
   persist between calls to modify_event_symbol that it can use to
   store a cache of the symbols it's generated for this NAME_TABLE
   before.  The object stored there may be a vector or an alist.

   SYMBOL_NUM is the number of the base name we want from NAME_TABLE.
   
   MODIFIERS is a set of modifier bits (as given in struct input_events)
   whose prefixes should be applied to the symbol name.

   SYMBOL_KIND is the value to be placed in the event_kind property of
   the returned symbol. 

   The symbols we create are supposed to have an
   `event-symbol-elements' property, which lists the modifiers present
   in the symbol's name.  */

static Lisp_Object
modify_event_symbol (symbol_num, modifiers, symbol_kind, name_alist,
                     name_table, symbol_table, table_size)
     int symbol_num;
     unsigned modifiers;
     Lisp_Object symbol_kind;
     Lisp_Object name_alist;
     char **name_table;
     Lisp_Object *symbol_table;
     int table_size;
{
  Lisp_Object value;
  Lisp_Object symbol_int;

  XSETINT (symbol_int, symbol_num);

  /* Is this a request for a valid symbol?  */
  if (symbol_num < 0 || symbol_num >= table_size)
    return Qnil;

  if (CONSP (*symbol_table))
    value = Fcdr (assq_no_quit (symbol_int, *symbol_table));

  /* If *symbol_table doesn't seem to be initialized properly, fix that.
     *symbol_table should be a lisp vector TABLE_SIZE elements long,
     where the Nth element is the symbol for NAME_TABLE[N], or nil if
     we've never used that symbol before.  */
  else
    {
      if (! VECTORP (*symbol_table)
	  || XVECTOR (*symbol_table)->size != table_size)
	{
	  Lisp_Object size;

	  XSETFASTINT (size, table_size);
	  *symbol_table = Fmake_vector (size, Qnil);
	}

      value = XVECTOR (*symbol_table)->contents[symbol_num];
    }

  /* Have we already used this symbol before?  */
  if (NILP (value))
    {
      /* No; let's create it.  */
      if (!NILP (name_alist))
	value = Fcdr_safe (Fassq (symbol_int, name_alist));
      else if (name_table[symbol_num])
	value = intern (name_table[symbol_num]);

      if (NILP (value))
	{
	  char buf[20];
	  sprintf (buf, "key-%d", symbol_num);
	  value = intern (buf);
	}

      if (CONSP (*symbol_table))
	*symbol_table = Fcons (value, *symbol_table);
      else
	XVECTOR (*symbol_table)->contents[symbol_num] = value;

      /* Fill in the cache entries for this symbol; this also 	
	 builds the Qevent_symbol_elements property, which the user
	 cares about.  */
      apply_modifiers (modifiers & click_modifier, value);
      Fput (value, Qevent_kind, symbol_kind);
    }

  /* Apply modifiers to that symbol.  */
  return apply_modifiers (modifiers, value);
}


/* Store into *addr a value nonzero if terminal input chars are available.
   Serves the purpose of ioctl (0, FIONREAD, addr)
   but works even if FIONREAD does not exist.
   (In fact, this may actually read some input.)  */

static void
get_input_pending (addr)
     int *addr;
{
  /* First of all, have we already counted some input?  */
  *addr = !NILP (Vquit_flag) || readable_events ();

  /* If input is being read as it arrives, and we have none, there is none.  */
  if (*addr > 0 || (interrupt_input && ! interrupts_deferred))
    return;

  /* Try to read some input and see how much we get.  */
  gobble_input (0);
  *addr = !NILP (Vquit_flag) || readable_events ();
}

/* Interface to read_avail_input, blocking SIGIO or SIGALRM if necessary.  */

int
gobble_input (expected)
     int expected;
{
#ifndef VMS
#ifdef SIGIO
  if (interrupt_input)
    {
      SIGMASKTYPE mask;
      mask = sigblockx (SIGIO);
      read_avail_input (expected);
      sigsetmask (mask);
    }
  else
#ifdef POLL_FOR_INPUT
  if (read_socket_hook && !interrupt_input && poll_suppress_count == 0)
    {
      SIGMASKTYPE mask;
      mask = sigblockx (SIGALRM);
      read_avail_input (expected);
      sigsetmask (mask);
    }
  else
#endif
#endif
    read_avail_input (expected);
#endif
}

/* Put a buffer_switch_event in the buffer
   so that read_key_sequence will notice the new current buffer.  */

record_asynch_buffer_change ()
{
  struct input_event event;
  Lisp_Object tem;

  event.kind = buffer_switch_event;
  event.frame_or_window = Qnil;

#ifdef subprocesses
  /* We don't need a buffer-switch event unless Emacs is waiting for input.
     The purpose of the event is to make read_key_sequence look up the
     keymaps again.  If we aren't in read_key_sequence, we don't need one,
     and the event could cause trouble by messing up (input-pending-p).  */
  tem = Fwaiting_for_user_input_p ();
  if (NILP (tem))
    return;
#else
  /* We never need these events if we have no asynchronous subprocesses.  */
  return;
#endif

  /* Make sure no interrupt happens while storing the event.  */
#ifdef SIGIO
  if (interrupt_input)
    {
      SIGMASKTYPE mask;
      mask = sigblockx (SIGIO);
      kbd_buffer_store_event (&event);
      sigsetmask (mask);
    }
  else
#endif
    {
      stop_polling ();
      kbd_buffer_store_event (&event);
      start_polling ();
    }
}

#ifndef VMS

/* Read any terminal input already buffered up by the system
   into the kbd_buffer, but do not wait.

   EXPECTED should be nonzero if the caller knows there is some input.

   Except on VMS, all input is read by this function.
   If interrupt_input is nonzero, this function MUST be called
   only when SIGIO is blocked.

   Returns the number of keyboard chars read, or -1 meaning
   this is a bad time to try to read input.  */

static int
read_avail_input (expected)
     int expected;
{
  struct input_event buf[KBD_BUFFER_SIZE];
  register int i;
  int nread;

  if (read_socket_hook)
    /* No need for FIONREAD or fcntl; just say don't wait.  */
    nread = (*read_socket_hook) (input_fd, buf, KBD_BUFFER_SIZE,
				 expected, expected);
  else
    {
      /* Using KBD_BUFFER_SIZE - 1 here avoids reading more than
	 the kbd_buffer can really hold.  That may prevent loss
	 of characters on some systems when input is stuffed at us.  */
      unsigned char cbuf[KBD_BUFFER_SIZE - 1];
      int n_to_read;

      /* Determine how many characters we should *try* to read.  */
#ifdef WINDOWSNT
      return 0;
#else /* not WINDOWSNT */
#ifdef MSDOS
      n_to_read = dos_keysns ();
      if (n_to_read == 0)
	return 0;
#else /* not MSDOS */
#ifdef FIONREAD
      /* Find out how much input is available.  */
      if (ioctl (input_fd, FIONREAD, &n_to_read) < 0)
	/* Formerly simply reported no input, but that sometimes led to
	   a failure of Emacs to terminate.
	   SIGHUP seems appropriate if we can't reach the terminal.  */
	/* ??? Is it really right to send the signal just to this process
	   rather than to the whole process group?
	   Perhaps on systems with FIONREAD Emacs is alone in its group.  */
	kill (getpid (), SIGHUP);
      if (n_to_read == 0)
	return 0;
      if (n_to_read > sizeof cbuf)
	n_to_read = sizeof cbuf;
#else /* no FIONREAD */
#if defined(USG) || defined(DGUX)
      /* Read some input if available, but don't wait.  */
      n_to_read = sizeof cbuf;
      fcntl (input_fd, F_SETFL, O_NDELAY);
#else
      you lose;
#endif
#endif
#endif /* not MSDOS */
#endif /* not WINDOWSNT */

      /* Now read; for one reason or another, this will not block.
	 NREAD is set to the number of chars read.  */
      do
	{
#ifdef MSDOS
	  cbuf[0] = dos_keyread();
	  nread = 1;
#else
	  nread = read (input_fd, cbuf, n_to_read);
#endif
#if defined (AIX) && (! defined (aix386) && defined (_BSD))
	  /* The kernel sometimes fails to deliver SIGHUP for ptys.
	     This looks incorrect, but it isn't, because _BSD causes
	     O_NDELAY to be defined in fcntl.h as O_NONBLOCK,
	     and that causes a value other than 0 when there is no input.  */
	  if (nread == 0)
	    kill (0, SIGHUP);
#endif
	}
      while (
	     /* We used to retry the read if it was interrupted.
		But this does the wrong thing when O_NDELAY causes
		an EAGAIN error.  Does anybody know of a situation
		where a retry is actually needed?  */
#if 0
	     nread < 0 && (errno == EAGAIN
#ifdef EFAULT
			   || errno == EFAULT
#endif
#ifdef EBADSLT
			   || errno == EBADSLT
#endif
			   )
#else
	     0
#endif
	     );

#ifndef FIONREAD
#if defined (USG) || defined (DGUX)
      fcntl (input_fd, F_SETFL, 0);
#endif /* USG or DGUX */
#endif /* no FIONREAD */
      for (i = 0; i < nread; i++)
	{
	  buf[i].kind = ascii_keystroke;
	  buf[i].modifiers = 0;
	  if (meta_key == 1 && (cbuf[i] & 0x80))
	    buf[i].modifiers = meta_modifier;
	  if (meta_key != 2)
	    cbuf[i] &= ~0x80;

	  buf[i].code = cbuf[i];
#ifdef MULTI_FRAME
	  XSETFRAME (buf[i].frame_or_window, selected_frame);
#else
	  buf[i].frame_or_window = Qnil;
#endif
	}
    }

  /* Scan the chars for C-g and store them in kbd_buffer.  */
  for (i = 0; i < nread; i++)
    {
      kbd_buffer_store_event (&buf[i]);
      /* Don't look at input that follows a C-g too closely.
	 This reduces lossage due to autorepeat on C-g.  */
      if (buf[i].kind == ascii_keystroke
	  && buf[i].code == quit_char)
	break;
    }

  return nread;
}
#endif /* not VMS */

#ifdef SIGIO   /* for entire page */
/* Note SIGIO has been undef'd if FIONREAD is missing.  */

SIGTYPE
input_available_signal (signo)
     int signo;
{
  /* Must preserve main program's value of errno.  */
  int old_errno = errno;
#ifdef BSD4_1
  extern int select_alarmed;
#endif

#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (signo, input_available_signal);
#endif /* USG */

#ifdef BSD4_1
  sigisheld (SIGIO);
#endif

  if (input_available_clear_time)
    EMACS_SET_SECS_USECS (*input_available_clear_time, 0, 0);

  while (1)
    {
      int nread;
      nread = read_avail_input (1);
      /* -1 means it's not ok to read the input now.
	 UNBLOCK_INPUT will read it later; now, avoid infinite loop.
	 0 means there was no keyboard input available.  */
      if (nread <= 0)
	break;

#ifdef BSD4_1
      select_alarmed = 1;  /* Force the select emulator back to life */
#endif
    }

#ifdef BSD4_1
  sigfree ();
#endif
  errno = old_errno;
}
#endif /* SIGIO */

/* Send ourselves a SIGIO.

   This function exists so that the UNBLOCK_INPUT macro in
   blockinput.h can have some way to take care of input we put off
   dealing with, without assuming that every file which uses
   UNBLOCK_INPUT also has #included the files necessary to get SIGIO. */
void
reinvoke_input_signal ()
{
#ifdef SIGIO  
  kill (0, SIGIO);
#endif
}



/* Return the prompt-string of a sparse keymap.
   This is the first element which is a string.
   Return nil if there is none.  */

Lisp_Object
map_prompt (map)
     Lisp_Object map;
{
  while (CONSP (map))
    {
      register Lisp_Object tem;
      tem = Fcar (map);
      if (STRINGP (tem))
	return tem;
      map = Fcdr (map);
    }
  return Qnil;
}

static void menu_bar_item ();
static void menu_bar_one_keymap ();

/* These variables hold the vector under construction within
   menu_bar_items and its subroutines, and the current index
   for storing into that vector.  */
static Lisp_Object menu_bar_items_vector;
static int menu_bar_items_index;

/* Return a vector of menu items for a menu bar, appropriate
   to the current buffer.  Each item has three elements in the vector:
   KEY STRING MAPLIST.

   OLD is an old vector we can optionally reuse, or nil.  */

Lisp_Object
menu_bar_items (old)
     Lisp_Object old;
{
  /* The number of keymaps we're scanning right now, and the number of
     keymaps we have allocated space for.  */
  int nmaps;

  /* maps[0..nmaps-1] are the prefix definitions of KEYBUF[0..t-1]
     in the current keymaps, or nil where it is not a prefix.  */
  Lisp_Object *maps;

  Lisp_Object def, tem, tail;

  Lisp_Object result;

  int mapno;
  Lisp_Object oquit;

  int i;

  struct gcpro gcpro1;

  /* In order to build the menus, we need to call the keymap
     accessors.  They all call QUIT.  But this function is called
     during redisplay, during which a quit is fatal.  So inhibit
     quitting while building the menus.
     We do this instead of specbind because (1) errors will clear it anyway
     and (2) this avoids risk of specpdl overflow.  */
  oquit = Vinhibit_quit;
  Vinhibit_quit = Qt; 

  if (!NILP (old))
    menu_bar_items_vector = old;
  else
    menu_bar_items_vector = Fmake_vector (make_number (24), Qnil);
  menu_bar_items_index = 0;

  GCPRO1 (menu_bar_items_vector);

  /* Build our list of keymaps.
     If we recognize a function key and replace its escape sequence in
     keybuf with its symbol, or if the sequence starts with a mouse
     click and we need to switch buffers, we jump back here to rebuild
     the initial keymaps from the current buffer.  */
  { 
    Lisp_Object *tmaps;

    /* Should overriding-local-map apply, here?  */
    if (!NILP (Voverriding_local_map_menu_flag))
      {
	if (NILP (Voverriding_local_map))
	  {
	    /* Yes, and it is nil.  Use just global map.  */
	    nmaps = 1;
	    maps = (Lisp_Object *) alloca (nmaps * sizeof (maps[0]));
	  }
	else
	  {
	    /* Yes, and it is non-nil.  Use it and the global map.  */
	    nmaps = 2;
	    maps = (Lisp_Object *) alloca (nmaps * sizeof (maps[0]));
	    maps[0] = Voverriding_local_map;
	  }
      }
    else
      {
	/* No, so use major and minor mode keymaps.  */
	nmaps = current_minor_maps (0, &tmaps) + 2;
	maps = (Lisp_Object *) alloca (nmaps * sizeof (maps[0]));
	bcopy (tmaps, maps, (nmaps - 2) * sizeof (maps[0]));
#ifdef USE_TEXT_PROPERTIES
	maps[nmaps-2] = get_local_map (PT, current_buffer);
#else
	maps[nmaps-2] = current_buffer->keymap;
#endif
      }
    maps[nmaps-1] = current_global_map;
  }

  /* Look up in each map the dummy prefix key `menu-bar'.  */

  result = Qnil;

  for (mapno = nmaps - 1; mapno >= 0; mapno--)
    {
      if (! NILP (maps[mapno]))
	def = get_keyelt (access_keymap (maps[mapno], Qmenu_bar, 1, 0));
      else
	def = Qnil;

      tem = Fkeymapp (def);
      if (!NILP (tem))
	menu_bar_one_keymap (def);
    }

  /* Move to the end those items that should be at the end.  */

  for (tail = Vmenu_bar_final_items; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      int i;
      int end = menu_bar_items_index;

      for (i = 0; i < end; i += 3)
	if (EQ (XCONS (tail)->car, XVECTOR (menu_bar_items_vector)->contents[i]))
	  {
	    Lisp_Object tem0, tem1, tem2;
	    /* Move the item at index I to the end,
	       shifting all the others forward.  */
	    tem0 = XVECTOR (menu_bar_items_vector)->contents[i + 0];
	    tem1 = XVECTOR (menu_bar_items_vector)->contents[i + 1];
	    tem2 = XVECTOR (menu_bar_items_vector)->contents[i + 2];
	    if (end > i + 3)
	      bcopy (&XVECTOR (menu_bar_items_vector)->contents[i + 3],
		     &XVECTOR (menu_bar_items_vector)->contents[i],
		     (end - i - 3) * sizeof (Lisp_Object));
	    XVECTOR (menu_bar_items_vector)->contents[end - 3] = tem0;
	    XVECTOR (menu_bar_items_vector)->contents[end - 2] = tem1;
	    XVECTOR (menu_bar_items_vector)->contents[end - 1] = tem2;
	    break;
	  }
    }

  /* Add nil, nil, nil at the end.  */
  i = menu_bar_items_index;
  if (i + 3 > XVECTOR (menu_bar_items_vector)->size)
    {
      Lisp_Object tem;
      int newsize = 2 * i;
      tem = Fmake_vector (make_number (2 * i), Qnil);
      bcopy (XVECTOR (menu_bar_items_vector)->contents,
	     XVECTOR (tem)->contents, i * sizeof (Lisp_Object));
      menu_bar_items_vector = tem;
    }
  /* Add this item.  */
  XVECTOR (menu_bar_items_vector)->contents[i++] = Qnil;
  XVECTOR (menu_bar_items_vector)->contents[i++] = Qnil;
  XVECTOR (menu_bar_items_vector)->contents[i++] = Qnil;
  menu_bar_items_index = i;

  Vinhibit_quit = oquit;
  UNGCPRO;
  return menu_bar_items_vector;
}

/* Scan one map KEYMAP, accumulating any menu items it defines
   in menu_bar_items_vector.  */

static void
menu_bar_one_keymap (keymap)
     Lisp_Object keymap;
{
  Lisp_Object tail, item, key, binding, item_string, table;

  /* Loop over all keymap entries that have menu strings.  */
  for (tail = keymap; CONSP (tail); tail = XCONS (tail)->cdr)
    {
      item = XCONS (tail)->car;
      if (CONSP (item))
	{
	  key = XCONS (item)->car;
	  binding = XCONS (item)->cdr;
	  if (CONSP (binding))
	    {
	      item_string = XCONS (binding)->car;
	      if (STRINGP (item_string))
		menu_bar_item (key, item_string, Fcdr (binding));
	    }
	  else if (EQ (binding, Qundefined))
	    menu_bar_item (key, Qnil, binding);
	}
      else if (VECTORP (item))
	{
	  /* Loop over the char values represented in the vector.  */
	  int len = XVECTOR (item)->size;
	  int c;
	  for (c = 0; c < len; c++)
	    {
	      Lisp_Object character;
	      XSETFASTINT (character, c);
	      binding = XVECTOR (item)->contents[c];
	      if (CONSP (binding))
		{
		  item_string = XCONS (binding)->car;
		  if (STRINGP (item_string))
		    menu_bar_item (key, item_string, Fcdr (binding));
		}
	      else if (EQ (binding, Qundefined))
		menu_bar_item (key, Qnil, binding);
	    }
	}
    }
}

/* This is used as the handler when calling internal_condition_case_1.  */

static Lisp_Object
menu_bar_item_1 (arg)
     Lisp_Object arg;
{
  return Qnil;
}

/* Add one item to menu_bar_items_vector, for KEY, ITEM_STRING and DEF.
   If there's already an item for KEY, add this DEF to it.  */

static void
menu_bar_item (key, item_string, def)
     Lisp_Object key, item_string, def;
{
  Lisp_Object tem;
  Lisp_Object enabled;
  int i;

  if (EQ (def, Qundefined))
    {
      /* If a map has an explicit `undefined' as definition,
	 discard any previously made menu bar item.  */

      for (i = 0; i < menu_bar_items_index; i += 3)
	if (EQ (key, XVECTOR (menu_bar_items_vector)->contents[i]))
	  {
	    if (menu_bar_items_index > i + 3)
	      bcopy (&XVECTOR (menu_bar_items_vector)->contents[i + 3],
		     &XVECTOR (menu_bar_items_vector)->contents[i],
		     (menu_bar_items_index - i - 3) * sizeof (Lisp_Object));
	    menu_bar_items_index -= 3;
	    return;
	  }

      /* If there's no definition for this key yet,
	 just ignore `undefined'.  */
      return;
    }

  /* See if this entry is enabled.  */
  enabled = Qt;

  if (SYMBOLP (def))
    {
      /* No property, or nil, means enable.
	 Otherwise, enable if value is not nil.  */
      tem = Fget (def, Qmenu_enable);
      if (!NILP (tem))
	/* (condition-case nil (eval tem)
	     (error nil))  */
	enabled = internal_condition_case_1 (Feval, tem, Qerror,
					     menu_bar_item_1);
    }

  /* Ignore this item if it's not enabled.  */
  if (NILP (enabled))
    return;

  /* Find any existing item for this KEY.  */
  for (i = 0; i < menu_bar_items_index; i += 3)
    if (EQ (key, XVECTOR (menu_bar_items_vector)->contents[i]))
      break;

  /* If we did not find this KEY, add it at the end.  */
  if (i == menu_bar_items_index)
    {
      /* If vector is too small, get a bigger one.  */
      if (i + 3 > XVECTOR (menu_bar_items_vector)->size)
	{
	  Lisp_Object tem;
	  int newsize = 2 * i;
	  tem = Fmake_vector (make_number (2 * i), Qnil);
	  bcopy (XVECTOR (menu_bar_items_vector)->contents,
		 XVECTOR (tem)->contents, i * sizeof (Lisp_Object));
	  menu_bar_items_vector = tem;
	}
      /* Add this item.  */
      XVECTOR (menu_bar_items_vector)->contents[i++] = key;
      XVECTOR (menu_bar_items_vector)->contents[i++] = item_string;
      XVECTOR (menu_bar_items_vector)->contents[i++] = Fcons (def, Qnil);
      menu_bar_items_index = i;
    }
  /* We did find an item for this KEY.  Add DEF to its list of maps.  */
  else
    {
      Lisp_Object old;
      old = XVECTOR (menu_bar_items_vector)->contents[i + 2];
      XVECTOR (menu_bar_items_vector)->contents[i + 2] = Fcons (def, old);
    }
}

/* Read a character using menus based on maps in the array MAPS.
   NMAPS is the length of MAPS.  Return nil if there are no menus in the maps.
   Return t if we displayed a menu but the user rejected it.

   PREV_EVENT is the previous input event, or nil if we are reading
   the first event of a key sequence.

   If USED_MOUSE_MENU is non-zero, then we set *USED_MOUSE_MENU to 1
   if we used a mouse menu to read the input, or zero otherwise.  If
   USED_MOUSE_MENU is zero, *USED_MOUSE_MENU is left alone.

   The prompting is done based on the prompt-string of the map
   and the strings associated with various map elements.  

   This can be done with X menus or with menus put in the minibuf.
   These are done in different ways, depending on how the input will be read.
   Menus using X are done after auto-saving in read-char, getting the input
   event from Fx_popup_menu; menus using the minibuf use read_char recursively
   and do auto-saving in the inner call of read_char. */

static Lisp_Object
read_char_x_menu_prompt (nmaps, maps, prev_event, used_mouse_menu)
     int nmaps;
     Lisp_Object *maps;
     Lisp_Object prev_event;
     int *used_mouse_menu;
{
  int mapno;
  register Lisp_Object name;
  Lisp_Object rest, vector;

  if (used_mouse_menu)
    *used_mouse_menu = 0;

  /* Use local over global Menu maps */

  if (! menu_prompting)
    return Qnil;

  /* Optionally disregard all but the global map.  */
  if (inhibit_local_menu_bar_menus)
    {
      maps += (nmaps - 1);
      nmaps = 1;
    }

  /* Get the menu name from the first map that has one (a prompt string).  */
  for (mapno = 0; mapno < nmaps; mapno++)
    {
      name = map_prompt (maps[mapno]);
      if (!NILP (name))
	break;
    }

  /* If we don't have any menus, just read a character normally.  */
  if (mapno >= nmaps)
    return Qnil;

#if (defined (HAVE_X_WINDOWS) && defined (HAVE_X_MENU)) || defined (MSDOS)
  /* If we got to this point via a mouse click,
     use a real menu for mouse selection.  */
  if (EVENT_HAS_PARAMETERS (prev_event))
    {
      /* Display the menu and get the selection.  */
      Lisp_Object *realmaps
	= (Lisp_Object *) alloca (nmaps * sizeof (Lisp_Object));
      Lisp_Object value;
      int nmaps1 = 0;

      /* Use the maps that are not nil.  */
      for (mapno = 0; mapno < nmaps; mapno++)
	if (!NILP (maps[mapno]))
	  realmaps[nmaps1++] = maps[mapno];

      value = Fx_popup_menu (prev_event, Flist (nmaps1, realmaps));
      if (CONSP (value))
	{
	  /* If we got more than one event, put all but the first
	     onto this list to be read later.
	     Return just the first event now.  */
	  Vunread_command_events
	    = nconc2 (XCONS (value)->cdr, Vunread_command_events);
	  value = XCONS (value)->car;
	}
      else if (NILP (value))
	value = Qt;
      if (used_mouse_menu)
	*used_mouse_menu = 1;
      return value;
    }
#endif /* (HAVE_X_WINDOWS && HAVE_X_MENU) || MSDOS */
  return Qnil ;
}

static Lisp_Object
read_char_minibuf_menu_prompt (commandflag, nmaps, maps)
     int commandflag ;
     int nmaps;
     Lisp_Object *maps;
{
  int mapno;
  register Lisp_Object name;
  int nlength;
  int width = FRAME_WIDTH (selected_frame) - 4;
  char *menu = (char *) alloca (width + 4);
  int idx = -1;
  int nobindings = 1;
  Lisp_Object rest, vector;

  if (! menu_prompting)
    return Qnil;

  /* Get the menu name from the first map that has one (a prompt string).  */
  for (mapno = 0; mapno < nmaps; mapno++)
    {
      name = map_prompt (maps[mapno]);
      if (!NILP (name))
	break;
    }

  /* If we don't have any menus, just read a character normally.  */
  if (mapno >= nmaps)
    return Qnil;

  /* Prompt string always starts with map's prompt, and a space.  */
  strcpy (menu, XSTRING (name)->data);
  nlength = XSTRING (name)->size;
  menu[nlength++] = ':';
  menu[nlength++] = ' ';
  menu[nlength] = 0;

  /* Start prompting at start of first map.  */
  mapno = 0;
  rest = maps[mapno];

  /* Present the documented bindings, a line at a time.  */
  while (1)
    {
      int notfirst = 0;
      int i = nlength;
      Lisp_Object obj;
      int ch;
      int orig_defn_macro ;

      /* Loop over elements of map.  */
      while (i < width)
	{
	  Lisp_Object s, elt;

	  /* If reached end of map, start at beginning of next map.  */
	  if (NILP (rest))
	    {
	      mapno++;
	      /* At end of last map, wrap around to first map if just starting,
		 or end this line if already have something on it.  */
	      if (mapno == nmaps)
		{
		  mapno = 0;
		  if (notfirst || nobindings) break;
		}
	      rest = maps[mapno];
	    }

	  /* Look at the next element of the map.  */
	  if (idx >= 0)
	    elt = XVECTOR (vector)->contents[idx];
	  else
	    elt = Fcar_safe (rest);

	  if (idx < 0 && VECTORP (elt))
	    {
	      /* If we found a dense table in the keymap,
		 advanced past it, but start scanning its contents.  */
	      rest = Fcdr_safe (rest);
	      vector = elt;
	      idx = 0;
	    }
	  else
	    {
	      /* An ordinary element.  */
	      if ( idx < 0 )
		s = Fcar_safe (Fcdr_safe (elt));	/* alist */
	      else
		s = Fcar_safe(elt);			/* vector */
	      if (!STRINGP (s))
		/* Ignore the element if it has no prompt string.  */
		;
	      /* If we have room for the prompt string, add it to this line.
		 If this is the first on the line, always add it.  */
	      else if (XSTRING (s)->size + i + 2 < width
		       || !notfirst)
		{
		  int thiswidth;

		  /* Punctuate between strings.  */
		  if (notfirst)
		    {
		      strcpy (menu + i, ", ");
		      i += 2;
		    }
		  notfirst = 1;
		  nobindings = 0 ;

		  /* Add as much of string as fits.  */
		  thiswidth = XSTRING (s)->size;
		  if (thiswidth + i > width)
		    thiswidth = width - i;
		  bcopy (XSTRING (s)->data, menu + i, thiswidth);
		  i += thiswidth;
		  menu[i] = 0;
		}
	      else
		{
		  /* If this element does not fit, end the line now,
		     and save the element for the next line.  */
		  strcpy (menu + i, "...");
		  break;
		}

	      /* Move past this element.  */
	      if (idx >= 0 && idx + 1 >= XVECTOR (vector)->size)
		/* Handle reaching end of dense table.  */
		idx = -1;
	      if (idx >= 0)
		idx++;
	      else
		rest = Fcdr_safe (rest);
	    }
	}

      /* Prompt with that and read response.  */
      message1 (menu);

      /* Make believe its not a keyboard macro in case the help char 
	 is pressed.  Help characters are not recorded because menu prompting
	 is not used on replay.
	 */
      orig_defn_macro = defining_kbd_macro ;
      defining_kbd_macro = 0 ;
      do
	obj = read_char (commandflag, 0, 0, Qnil, 0);
      while (BUFFERP (obj));
      defining_kbd_macro = orig_defn_macro ;

      if (!INTEGERP (obj))
	return obj;
      else
	ch = XINT (obj);

      if (! EQ (obj, menu_prompt_more_char)
	  && (!INTEGERP (menu_prompt_more_char)
	      || ! EQ (obj, make_number (Ctl (XINT (menu_prompt_more_char))))))
	{
	  if ( defining_kbd_macro )
	    store_kbd_macro_char(obj) ;
	  return obj;
	}
      /* Help char - go round again */
    }
}

/* Reading key sequences.  */

/* Follow KEY in the maps in CURRENT[0..NMAPS-1], placing its bindings
   in DEFS[0..NMAPS-1].  Set NEXT[i] to DEFS[i] if DEFS[i] is a
   keymap, or nil otherwise.  Return the index of the first keymap in
   which KEY has any binding, or NMAPS if no map has a binding.

   If KEY is a meta ASCII character, treat it like meta-prefix-char
   followed by the corresponding non-meta character.  Keymaps in
   CURRENT with non-prefix bindings for meta-prefix-char become nil in
   NEXT.

   If KEY has no bindings in any of the CURRENT maps, NEXT is left
   unmodified.

   NEXT may == CURRENT.  */

static int
follow_key (key, nmaps, current, defs, next)
     Lisp_Object key;
     Lisp_Object *current, *defs, *next;
     int nmaps;
{
  int i, first_binding;

  /* If KEY is a meta ASCII character, treat it like meta-prefix-char
     followed by the corresponding non-meta character.  */
  if (INTEGERP (key) && (XINT (key) & CHAR_META))
    {
      for (i = 0; i < nmaps; i++)
	if (! NILP (current[i]))
	  {
	    next[i] =
	      get_keyelt (access_keymap (current[i], meta_prefix_char, 1, 0));

	    /* Note that since we pass the resulting bindings through
	       get_keymap_1, non-prefix bindings for meta-prefix-char
	       disappear.  */
	    next[i] = get_keymap_1 (next[i], 0, 1);
	  }
	else
	  next[i] = Qnil;

      current = next;
      XSETINT (key, XFASTINT (key) & ~CHAR_META);
    }

  first_binding = nmaps;
  for (i = nmaps - 1; i >= 0; i--)
    {
      if (! NILP (current[i]))
	{
	  defs[i] = get_keyelt (access_keymap (current[i], key, 1, 0));
	  if (! NILP (defs[i]))
	    first_binding = i;
	}
      else
	defs[i] = Qnil;
    }

  /* Given the set of bindings we've found, produce the next set of maps.  */
  if (first_binding < nmaps)
    for (i = 0; i < nmaps; i++)
      next[i] = NILP (defs[i]) ? Qnil : get_keymap_1 (defs[i], 0, 1);

  return first_binding;
}

/* Read a sequence of keys that ends with a non prefix character, 
   storing it in KEYBUF, a buffer of size BUFSIZE.
   Prompt with PROMPT.
   Return the length of the key sequence stored.
   Return -1 if the user rejected a command menu.

   Echo starting immediately unless `prompt' is 0.

   Where a key sequence ends depends on the currently active keymaps.
   These include any minor mode keymaps active in the current buffer,
   the current buffer's local map, and the global map.

   If a key sequence has no other bindings, we check Vfunction_key_map
   to see if some trailing subsequence might be the beginning of a
   function key's sequence.  If so, we try to read the whole function
   key, and substitute its symbolic name into the key sequence.

   We ignore unbound `down-' mouse clicks.  We turn unbound `drag-' and
   `double-' events into similar click events, if that would make them
   bound.  We try to turn `triple-' events first into `double-' events,
   then into clicks.

   If we get a mouse click in a mode line, vertical divider, or other
   non-text area, we treat the click as if it were prefixed by the
   symbol denoting that area - `mode-line', `vertical-line', or
   whatever.

   If the sequence starts with a mouse click, we read the key sequence
   with respect to the buffer clicked on, not the current buffer.

   If the user switches frames in the midst of a key sequence, we put
   off the switch-frame event until later; the next call to
   read_char will return it.  */

static int
read_key_sequence (keybuf, bufsize, prompt, dont_downcase_last)
     Lisp_Object *keybuf;
     int bufsize;
     Lisp_Object prompt;
     int dont_downcase_last;
{
  int count = specpdl_ptr - specpdl;

  /* How many keys there are in the current key sequence.  */
  int t;

  /* The length of the echo buffer when we started reading, and
     the length of this_command_keys when we started reading.  */
  int echo_start;
  int keys_start;

  /* The number of keymaps we're scanning right now, and the number of
     keymaps we have allocated space for.  */
  int nmaps;
  int nmaps_allocated = 0;

  /* defs[0..nmaps-1] are the definitions of KEYBUF[0..t-1] in
     the current keymaps.  */
  Lisp_Object *defs;

  /* submaps[0..nmaps-1] are the prefix definitions of KEYBUF[0..t-1]
     in the current keymaps, or nil where it is not a prefix.  */
  Lisp_Object *submaps;

  /* The local map to start out with at start of key sequence.  */
  Lisp_Object orig_local_map;

  /* 1 if we have already considered switching to the local-map property
     of the place where a mouse click occurred.  */
  int localized_local_map = 0;

  /* The index in defs[] of the first keymap that has a binding for
     this key sequence.  In other words, the lowest i such that
     defs[i] is non-nil.  */
  int first_binding;

  /* If t < mock_input, then KEYBUF[t] should be read as the next
     input key.

     We use this to recover after recognizing a function key.  Once we
     realize that a suffix of the current key sequence is actually a
     function key's escape sequence, we replace the suffix with the
     function key's binding from Vfunction_key_map.  Now keybuf
     contains a new and different key sequence, so the echo area,
     this_command_keys, and the submaps and defs arrays are wrong.  In
     this situation, we set mock_input to t, set t to 0, and jump to
     restart_sequence; the loop will read keys from keybuf up until
     mock_input, thus rebuilding the state; and then it will resume
     reading characters from the keyboard.  */
  int mock_input = 0;

  /* If the sequence is unbound in submaps[], then
     keybuf[fkey_start..fkey_end-1] is a prefix in Vfunction_key_map,
     and fkey_map is its binding.

     These might be > t, indicating that all function key scanning
     should hold off until t reaches them.  We do this when we've just
     recognized a function key, to avoid searching for the function
     key's again in Vfunction_key_map.  */
  int fkey_start = 0, fkey_end = 0;
  Lisp_Object fkey_map;

  /* Likewise, for key_translation_map.  */
  int keytran_start = 0, keytran_end = 0;
  Lisp_Object keytran_map;

  /* If we receive a ``switch-frame'' event in the middle of a key sequence,
     we put it off for later.  While we're reading, we keep the event here.  */
  Lisp_Object delayed_switch_frame;

  /* See the comment below... */
#if defined (GOBBLE_FIRST_EVENT)
  Lisp_Object first_event;
#endif

  Lisp_Object original_uppercase;
  int original_uppercase_position = -1;

  /* Gets around Microsoft compiler limitations.  */
  int dummyflag = 0;

  struct buffer *starting_buffer;

  /* Nonzero if we seem to have got the beginning of a binding
     in function_key_map.  */
  int function_key_possible = 0;
  int key_translation_possible = 0;

  int junk;

  last_nonmenu_event = Qnil;

  delayed_switch_frame = Qnil;
  fkey_map = Vfunction_key_map;
  keytran_map = Vkey_translation_map;

  /* If there is no function-key-map, turn off function key scanning.  */
  if (NILP (Fkeymapp (Vfunction_key_map)))
    fkey_start = fkey_end = bufsize + 1;

  /* If there is no key-translation-map, turn off scanning.  */
  if (NILP (Fkeymapp (Vkey_translation_map)))
    keytran_start = keytran_end = bufsize + 1;

  if (INTERACTIVE)
    {
      if (!NILP (prompt))
	echo_prompt (XSTRING (prompt)->data);
      else if (cursor_in_echo_area && echo_keystrokes)
	/* This doesn't put in a dash if the echo buffer is empty, so
	   you don't always see a dash hanging out in the minibuffer.  */
	echo_dash ();
    }

  /* Record the initial state of the echo area and this_command_keys;
     we will need to restore them if we replay a key sequence.  */
  if (INTERACTIVE)
    echo_start = echo_length ();
  keys_start = this_command_key_count;

#if defined (GOBBLE_FIRST_EVENT)
  /* This doesn't quite work, because some of the things that read_char
     does cannot safely be bypassed.  It seems too risky to try to make
     this work right.  */ 

  /* Read the first char of the sequence specially, before setting
     up any keymaps, in case a filter runs and switches buffers on us.  */
  first_event = read_char (NILP (prompt), 0, submaps, last_nonmenu_event,
			   &junk);
#endif /* GOBBLE_FIRST_EVENT */

  orig_local_map = get_local_map (PT, current_buffer);

  /* We jump here when the key sequence has been thoroughly changed, and
     we need to rescan it starting from the beginning.  When we jump here,
     keybuf[0..mock_input] holds the sequence we should reread.  */
 replay_sequence:

  starting_buffer = current_buffer;
  function_key_possible = 0;
  key_translation_possible = 0;

  /* Build our list of keymaps.
     If we recognize a function key and replace its escape sequence in
     keybuf with its symbol, or if the sequence starts with a mouse
     click and we need to switch buffers, we jump back here to rebuild
     the initial keymaps from the current buffer.  */
  { 
    Lisp_Object *maps;

    if (!NILP (Voverriding_local_map))
      {
	nmaps = 2;
	if (nmaps > nmaps_allocated)
	  {
	    submaps = (Lisp_Object *) alloca (nmaps * sizeof (submaps[0]));
	    defs    = (Lisp_Object *) alloca (nmaps * sizeof (defs[0]));
	    nmaps_allocated = nmaps;
	  }
	submaps[0] = Voverriding_local_map;
      }
    else
      {
	nmaps = current_minor_maps (0, &maps) + 2;
	if (nmaps > nmaps_allocated)
	  {
	    submaps = (Lisp_Object *) alloca (nmaps * sizeof (submaps[0]));
	    defs    = (Lisp_Object *) alloca (nmaps * sizeof (defs[0]));
	    nmaps_allocated = nmaps;
	  }
	bcopy (maps, submaps, (nmaps - 2) * sizeof (submaps[0]));
#ifdef USE_TEXT_PROPERTIES
	submaps[nmaps-2] = orig_local_map;
#else
	submaps[nmaps-2] = current_buffer->keymap;
#endif
      }
    submaps[nmaps-1] = current_global_map;
  }

  /* Find an accurate initial value for first_binding.  */
  for (first_binding = 0; first_binding < nmaps; first_binding++)
    if (! NILP (submaps[first_binding]))
      break;

  /* Start from the beginning in keybuf.  */
  t = 0;

  /* These are no-ops the first time through, but if we restart, they
     revert the echo area and this_command_keys to their original state.  */
  this_command_key_count = keys_start;
  if (INTERACTIVE && t < mock_input)
    echo_truncate (echo_start);

  /* If the best binding for the current key sequence is a keymap, or
     we may be looking at a function key's escape sequence, keep on
     reading.  */
  while ((first_binding < nmaps && ! NILP (submaps[first_binding]))
	 || (first_binding >= nmaps
	     && fkey_start < t
	     /* mock input is never part of a function key's sequence.  */
	     && mock_input <= fkey_start)
	 || (first_binding >= nmaps
	     && keytran_start < t && key_translation_possible)
	 /* Don't return in the middle of a possible function key sequence,
	    if the only bindings we found were via case conversion.
	    Thus, if ESC O a has a function-key-map translation
	    and ESC o has a binding, don't return after ESC O,
	    so that we can translate ESC O plus the next character.  */
	 )
    {
      Lisp_Object key;
      int used_mouse_menu = 0;

      /* Where the last real key started.  If we need to throw away a
         key that has expanded into more than one element of keybuf
         (say, a mouse click on the mode line which is being treated
         as [mode-line (mouse-...)], then we backtrack to this point
         of keybuf.  */
      int last_real_key_start;

      /* These variables are analogous to echo_start and keys_start;
	 while those allow us to restart the entire key sequence,
	 echo_local_start and keys_local_start allow us to throw away
	 just one key.  */
      int echo_local_start, keys_local_start, local_first_binding;

      if (t >= bufsize)
	error ("key sequence too long");

      if (INTERACTIVE)
	echo_local_start = echo_length ();
      keys_local_start = this_command_key_count;
      local_first_binding = first_binding;
      
    replay_key:
      /* These are no-ops, unless we throw away a keystroke below and
	 jumped back up to replay_key; in that case, these restore the
	 variables to their original state, allowing us to replay the
	 loop.  */
      if (INTERACTIVE && t < mock_input)
	echo_truncate (echo_local_start);
      this_command_key_count = keys_local_start;
      first_binding = local_first_binding;

      /* By default, assume each event is "real".  */
      last_real_key_start = t;

      /* Does mock_input indicate that we are re-reading a key sequence?  */
      if (t < mock_input)
	{
	  key = keybuf[t];
	  add_command_key (key);
	  if (echo_keystrokes)
	    echo_char (key);
	}

      /* If not, we should actually read a character.  */
      else
	{
	  struct buffer *buf = current_buffer;

	  {
#ifdef MULTI_PERDISPLAY
	    PERDISPLAY *interrupted_perdisplay = current_perdisplay;
	    if (setjmp (wrong_display_jmpbuf))
	      {
		while (t > 0)
		  interrupted_perdisplay->kbd_queue
		    = Fcons (keybuf[--t], interrupted_perdisplay->kbd_queue);
		mock_input = 0;
		goto replay_sequence;
	      }
#endif
	    key = read_char (NILP (prompt), nmaps, submaps, last_nonmenu_event,
			     &used_mouse_menu);
	  }

	  /* read_char returns t when it shows a menu and the user rejects it.
	     Just return -1.  */
	  if (EQ (key, Qt))
	    return -1;

	  /* read_char returns -1 at the end of a macro.
	     Emacs 18 handles this by returning immediately with a
	     zero, so that's what we'll do.  */
	  if (INTEGERP (key) && XINT (key) == -1)
	    {
	      t = 0;
	      /* The Microsoft C compiler can't handle the goto that
		 would go here.  */
	      dummyflag = 1;
	      break;
	    }
	  
	  /* If the current buffer has been changed from under us, the
	     keymap may have changed, so replay the sequence.  */
	  if (BUFFERP (key))
	    {
	      mock_input = t;
	      goto replay_sequence;
	    }

	  /* If we have a quit that was typed in another frame, and
	     quit_throw_to_read_char switched buffers,
	     replay to get the right keymap.  */
	  if (XINT (key) == quit_char && current_buffer != starting_buffer)
	    {
	      keybuf[t++] = key;
	      mock_input = t;
	      Vquit_flag = Qnil;
	      goto replay_sequence;
	    }

	  Vquit_flag = Qnil;
	}

      /* Clicks in non-text areas get prefixed by the symbol 
	 in their CHAR-ADDRESS field.  For example, a click on
	 the mode line is prefixed by the symbol `mode-line'.

	 Furthermore, key sequences beginning with mouse clicks
	 are read using the keymaps of the buffer clicked on, not
	 the current buffer.  So we may have to switch the buffer
	 here.

	 When we turn one event into two events, we must make sure
	 that neither of the two looks like the original--so that,
	 if we replay the events, they won't be expanded again.
	 If not for this, such reexpansion could happen either here
	 or when user programs play with this-command-keys.  */
      if (EVENT_HAS_PARAMETERS (key))
	{
	  Lisp_Object kind;

	  kind = EVENT_HEAD_KIND (EVENT_HEAD (key));
	  if (EQ (kind, Qmouse_click))
	    {
	      Lisp_Object window, posn;

	      window = POSN_WINDOW      (EVENT_START (key));
	      posn   = POSN_BUFFER_POSN (EVENT_START (key));
	      if (CONSP (posn))
		{
		  /* We're looking at the second event of a
		     sequence which we expanded before.  Set
		     last_real_key_start appropriately.  */
		  if (t > 0)
		    last_real_key_start = t - 1;
		}

	      /* Key sequences beginning with mouse clicks are
		 read using the keymaps in the buffer clicked on,
		 not the current buffer.  If we're at the
		 beginning of a key sequence, switch buffers.  */
	      if (last_real_key_start == 0
		  && WINDOWP (window)
		  && BUFFERP (XWINDOW (window)->buffer)
		  && XBUFFER (XWINDOW (window)->buffer) != current_buffer)
		{
		  keybuf[t] = key;
		  mock_input = t + 1;

		  /* Arrange to go back to the original buffer once we're
		     done reading the key sequence.  Note that we can't
		     use save_excursion_{save,restore} here, because they
		     save point as well as the current buffer; we don't
		     want to save point, because redisplay may change it,
		     to accommodate a Fset_window_start or something.  We
		     don't want to do this at the top of the function,
		     because we may get input from a subprocess which
		     wants to change the selected window and stuff (say,
		     emacsclient).  */
		  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());

		  set_buffer_internal (XBUFFER (XWINDOW (window)->buffer));
		  orig_local_map = get_local_map (PT, current_buffer);
		  goto replay_sequence;
		}
	      /* For a mouse click, get the local text-property keymap
		 of the place clicked on, rather than point.  */
	      if (last_real_key_start == 0 && CONSP (XCONS (key)->cdr)
		  && ! localized_local_map)
		{
		  Lisp_Object map_here, start, pos;

		  localized_local_map = 1;
		  start = EVENT_START (key);
		  if (CONSP (start) && CONSP (XCONS (start)->cdr))
		    {
		      pos = POSN_BUFFER_POSN (start);
		      if (INTEGERP (pos))
			{
			  map_here = get_local_map (XINT (pos), current_buffer);
			  if (!EQ (map_here, orig_local_map))
			    {
			      orig_local_map = map_here;
			      keybuf[t] = key;
			      mock_input = t + 1;

			      goto replay_sequence;
			    }
			}
		    }
		}

	      /* Expand mode-line and scroll-bar events into two events:
		 use posn as a fake prefix key.  */
	      if (SYMBOLP (posn))
		{
		  if (t + 1 >= bufsize)
		    error ("key sequence too long");
		  keybuf[t] = posn;
		  keybuf[t+1] = key;
		  mock_input = t + 2;

		  /* Zap the position in key, so we know that we've
		     expanded it, and don't try to do so again.  */
		  POSN_BUFFER_POSN (EVENT_START (key))
		    = Fcons (posn, Qnil);
		  goto replay_key;
		}
	    }
	  else if (EQ (kind, Qswitch_frame))
	    {
	      /* If we're at the beginning of a key sequence, go
		 ahead and return this event.  If we're in the
		 midst of a key sequence, delay it until the end. */
	      if (t > 0)
		{
		  delayed_switch_frame = key;
		  goto replay_key;
		}
	    }
	  else if (CONSP (XCONS (key)->cdr)
		   && CONSP (EVENT_START (key))
		   && CONSP (XCONS (EVENT_START (key))->cdr))
	    {
	      Lisp_Object posn;

	      posn = POSN_BUFFER_POSN (EVENT_START (key));
	      /* Handle menu-bar events:
		 insert the dummy prefix event `menu-bar'.  */
	      if (EQ (posn, Qmenu_bar))
		{
		  if (t + 1 >= bufsize)
		    error ("key sequence too long");
		  /* Run the Lucid hook.  */
		  if (!NILP (Vrun_hooks))
		    call1 (Vrun_hooks, Qactivate_menubar_hook);
		  /* If it has changed current-menubar from previous value,
		     really recompute the menubar from the value.  */
		  if (! NILP (Vlucid_menu_bar_dirty_flag))
		    call0 (Qrecompute_lucid_menubar);
		  keybuf[t] = posn;
		  keybuf[t+1] = key;

		  /* Zap the position in key, so we know that we've
		     expanded it, and don't try to do so again.  */
		  POSN_BUFFER_POSN (EVENT_START (key))
		    = Fcons (posn, Qnil);

		  mock_input = t + 2;
		  goto replay_sequence;
		}
	      else if (CONSP (posn))
		{
		  /* We're looking at the second event of a
		     sequence which we expanded before.  Set
		     last_real_key_start appropriately.  */
		  if (last_real_key_start == t && t > 0)
		    last_real_key_start = t - 1;
		}
	    }
	}

      /* We have finally decided that KEY is something we might want
	 to look up.  */
      first_binding = (follow_key (key,
				   nmaps   - first_binding,
				   submaps + first_binding,
				   defs    + first_binding,
				   submaps + first_binding)
		       + first_binding);

      /* If KEY wasn't bound, we'll try some fallbacks.  */
      if (first_binding >= nmaps)
	{
	  Lisp_Object head;

	  head = EVENT_HEAD (key);
	  if (EQ (head, Vhelp_char))
	    {
	      read_key_sequence_cmd = Vprefix_help_command;
	      keybuf[t++] = key;
	      last_nonmenu_event = key;
	      /* The Microsoft C compiler can't handle the goto that
		 would go here.  */
	      dummyflag = 1;
	      break;
	    }

	  if (SYMBOLP (head))
	    {
	      Lisp_Object breakdown;
	      int modifiers;

	      breakdown = parse_modifiers (head);
	      modifiers = XINT (XCONS (XCONS (breakdown)->cdr)->car);
	      /* Attempt to reduce an unbound mouse event to a simpler
		 event that is bound:
		   Drags reduce to clicks.
		   Double-clicks reduce to clicks.
		   Triple-clicks reduce to double-clicks, then to clicks.
		   Down-clicks are eliminated.
		   Double-downs reduce to downs, then are eliminated.
		   Triple-downs reduce to double-downs, then to downs,
		     then are eliminated. */
	      if (modifiers & (down_modifier | drag_modifier
			       | double_modifier | triple_modifier))
		{
		  while (modifiers & (down_modifier | drag_modifier
				      | double_modifier | triple_modifier))
		    {
		      Lisp_Object new_head, new_click;
		      if (modifiers & triple_modifier)
			modifiers ^= (double_modifier | triple_modifier);
		      else if (modifiers & double_modifier)
			modifiers &= ~double_modifier;
		      else if (modifiers & drag_modifier)
			modifiers &= ~drag_modifier;
		      else
			{
			  /* Dispose of this `down' event by simply jumping
			     back to replay_key, to get another event.

			     Note that if this event came from mock input,
			     then just jumping back to replay_key will just
			     hand it to us again.  So we have to wipe out any
			     mock input.

			     We could delete keybuf[t] and shift everything
			     after that to the left by one spot, but we'd also
			     have to fix up any variable that points into
			     keybuf, and shifting isn't really necessary
			     anyway.

			     Adding prefixes for non-textual mouse clicks
			     creates two characters of mock input, and both
			     must be thrown away.  If we're only looking at
			     the prefix now, we can just jump back to
			     replay_key.  On the other hand, if we've already
			     processed the prefix, and now the actual click
			     itself is giving us trouble, then we've lost the
			     state of the keymaps we want to backtrack to, and
			     we need to replay the whole sequence to rebuild
			     it.

			     Beyond that, only function key expansion could
			     create more than two keys, but that should never
			     generate mouse events, so it's okay to zero
			     mock_input in that case too.

			     Isn't this just the most wonderful code ever?  */
			  if (t == last_real_key_start)
			    {
			      mock_input = 0;
			      goto replay_key;
			    }
			  else
			    {
			      mock_input = last_real_key_start;
			      goto replay_sequence;
			    }
			}

		      new_head
			= apply_modifiers (modifiers, XCONS (breakdown)->car);
		      new_click
			= Fcons (new_head, Fcons (EVENT_START (key), Qnil));

		      /* Look for a binding for this new key.  follow_key
			 promises that it didn't munge submaps the
			 last time we called it, since key was unbound.  */
		      first_binding
			= (follow_key (new_click,
				       nmaps   - local_first_binding,
				       submaps + local_first_binding,
				       defs    + local_first_binding,
				       submaps + local_first_binding)
			   + local_first_binding);

		      /* If that click is bound, go for it.  */
		      if (first_binding < nmaps)
			{
			  key = new_click;
			  break;
			}
		      /* Otherwise, we'll leave key set to the drag event.  */
		    }
		}
	    }
	}

      keybuf[t++] = key;
      /* Normally, last_nonmenu_event gets the previous key we read.
	 But when a mouse popup menu is being used,
	 we don't update last_nonmenu_event; it continues to hold the mouse
	 event that preceded the first level of menu.  */
      if (!used_mouse_menu)
	last_nonmenu_event = key;

      /* If the sequence is unbound, see if we can hang a function key
	 off the end of it.  We only want to scan real keyboard input
	 for function key sequences, so if mock_input says that we're
	 re-reading old events, don't examine it.  */
      if (first_binding >= nmaps
	  && t >= mock_input)
	{
	  Lisp_Object fkey_next;

	  /* Continue scan from fkey_end until we find a bound suffix.
	     If we fail, increment fkey_start
	     and start fkey_end from there.  */
	  while (fkey_end < t)
	    {
	      Lisp_Object key;

	      key = keybuf[fkey_end++];
	      /* Look up meta-characters by prefixing them
		 with meta_prefix_char.  I hate this.  */
	      if (INTEGERP (key) && XINT (key) & meta_modifier)
		{
		  fkey_next
		    = get_keymap_1
		      (get_keyelt
		       (access_keymap (fkey_map, meta_prefix_char, 1, 0)),
		       0, 1);
		  XSETFASTINT (key, XFASTINT (key) & ~meta_modifier);
		}
	      else
		fkey_next = fkey_map;

	      fkey_next
		= get_keyelt (access_keymap (fkey_next, key, 1, 0));

#if 0 /* I didn't turn this on, because it might cause trouble
	 for the mapping of return into C-m and tab into C-i.  */
	      /* Optionally don't map function keys into other things.
		 This enables the user to redefine kp- keys easily.  */
	      if (SYMBOLP (key) && !NILP (Vinhibit_function_key_mapping))
		fkey_next = Qnil;
#endif

	      /* If the function key map gives a function, not an
		 array, then call the function with no args and use
		 its value instead.  */
	      if (SYMBOLP (fkey_next) && ! NILP (Ffboundp (fkey_next))
		  && fkey_end == t)
		{
		  struct gcpro gcpro1, gcpro2, gcpro3;
		  Lisp_Object tem;
		  tem = fkey_next;

		  GCPRO3 (fkey_map, keytran_map, delayed_switch_frame);
		  fkey_next = call1 (fkey_next, prompt);
		  UNGCPRO;
		  /* If the function returned something invalid,
		     barf--don't ignore it.
		     (To ignore it safely, we would need to gcpro a bunch of 
		     other variables.)  */
		  if (! (VECTORP (fkey_next) || STRINGP (fkey_next)))
		    error ("Function in function-key-map returns invalid key sequence");
		}

	      function_key_possible = ! NILP (fkey_next);

	      /* If keybuf[fkey_start..fkey_end] is bound in the
		 function key map and it's a suffix of the current
		 sequence (i.e. fkey_end == t), replace it with
		 the binding and restart with fkey_start at the end. */
	      if ((VECTORP (fkey_next) || STRINGP (fkey_next))
		  && fkey_end == t)
		{
		  int len = XFASTINT (Flength (fkey_next));

		  t = fkey_start + len;
		  if (t >= bufsize)
		    error ("key sequence too long");

		  if (VECTORP (fkey_next))
		    bcopy (XVECTOR (fkey_next)->contents,
			   keybuf + fkey_start,
			   (t - fkey_start) * sizeof (keybuf[0]));
		  else if (STRINGP (fkey_next))
		    {
		      int i;

		      for (i = 0; i < len; i++)
			XSETFASTINT (keybuf[fkey_start + i],
				     XSTRING (fkey_next)->data[i]);
		    }
		  
		  mock_input = t;
		  fkey_start = fkey_end = t;
		  fkey_map = Vfunction_key_map;

		  /* Do pass the results through key-translation-map.  */
		  keytran_start = keytran_end = 0;
		  keytran_map = Vkey_translation_map;

		  goto replay_sequence;
		}
	      
	      fkey_map = get_keymap_1 (fkey_next, 0, 1);

	      /* If we no longer have a bound suffix, try a new positions for 
		 fkey_start.  */
	      if (NILP (fkey_map))
		{
		  fkey_end = ++fkey_start;
		  fkey_map = Vfunction_key_map;
		  function_key_possible = 0;
		}
	    }
	}

      /* Look for this sequence in key-translation-map.  */
      {
	Lisp_Object keytran_next;

	/* Scan from keytran_end until we find a bound suffix.  */
	while (keytran_end < t)
	  {
	    Lisp_Object key;

	    key = keybuf[keytran_end++];
	    /* Look up meta-characters by prefixing them
	       with meta_prefix_char.  I hate this.  */
	    if (INTEGERP (key) && XINT (key) & meta_modifier)
	      {
		keytran_next
		  = get_keymap_1
		    (get_keyelt
		     (access_keymap (keytran_map, meta_prefix_char, 1, 0)),
		     0, 1);
		XSETFASTINT (key, XFASTINT (key) & ~meta_modifier);
	      }
	    else
	      keytran_next = keytran_map;

	    keytran_next
	      = get_keyelt (access_keymap (keytran_next, key, 1, 0));

	    /* If the key translation map gives a function, not an
	       array, then call the function with no args and use
	       its value instead.  */
	    if (SYMBOLP (keytran_next) && ! NILP (Ffboundp (keytran_next))
		&& keytran_end == t)
	      {
		struct gcpro gcpro1, gcpro2, gcpro3;
		Lisp_Object tem;
		tem = keytran_next;

		GCPRO3 (fkey_map, keytran_map, delayed_switch_frame);
		keytran_next = call1 (keytran_next, prompt);
		UNGCPRO;
		/* If the function returned something invalid,
		   barf--don't ignore it.
		   (To ignore it safely, we would need to gcpro a bunch of 
		   other variables.)  */
		if (! (VECTORP (keytran_next) || STRINGP (keytran_next)))
		  error ("Function in key-translation-map returns invalid key sequence");
	      }

	    key_translation_possible = ! NILP (keytran_next);

	    /* If keybuf[keytran_start..keytran_end] is bound in the
	       key translation map and it's a suffix of the current
	       sequence (i.e. keytran_end == t), replace it with
	       the binding and restart with keytran_start at the end. */
	    if ((VECTORP (keytran_next) || STRINGP (keytran_next))
		&& keytran_end == t)
	      {
		int len = XFASTINT (Flength (keytran_next));

		t = keytran_start + len;
		if (t >= bufsize)
		  error ("key sequence too long");

		if (VECTORP (keytran_next))
		  bcopy (XVECTOR (keytran_next)->contents,
			 keybuf + keytran_start,
			 (t - keytran_start) * sizeof (keybuf[0]));
		else if (STRINGP (keytran_next))
		  {
		    int i;

		    for (i = 0; i < len; i++)
		      XSETFASTINT (keybuf[keytran_start + i],
				   XSTRING (keytran_next)->data[i]);
		  }

		mock_input = t;
		keytran_start = keytran_end = t;
		keytran_map = Vkey_translation_map;

		/* Don't pass the results of key-translation-map
		   through function-key-map.  */
		fkey_start = fkey_end = t;
		fkey_map = Vkey_translation_map;

		goto replay_sequence;
	      }

	    keytran_map = get_keymap_1 (keytran_next, 0, 1);

	    /* If we no longer have a bound suffix, try a new positions for 
	       keytran_start.  */
	    if (NILP (keytran_map))
	      {
		keytran_end = ++keytran_start;
		keytran_map = Vkey_translation_map;
		key_translation_possible = 0;
	      }
	  }
      }

      /* If KEY is not defined in any of the keymaps,
	 and cannot be part of a function key or translation,
	 and is an upper case letter
	 use the corresponding lower-case letter instead.  */
      if (first_binding == nmaps && ! function_key_possible
	  && ! key_translation_possible
	  && INTEGERP (key)
	  && ((((XINT (key) & 0x3ffff)
		< XSTRING (current_buffer->downcase_table)->size)
	       && UPPERCASEP (XINT (key) & 0x3ffff))
	      || (XINT (key) & shift_modifier)))
	{
	  original_uppercase = key;
	  original_uppercase_position = t - 1;

	  if (XINT (key) & shift_modifier)
	    XSETINT (key, XINT (key) & ~shift_modifier);
	  else
	    XSETINT (key, (DOWNCASE (XINT (key) & 0x3ffff)
			   | (XINT (key) & ~0x3ffff)));

	  keybuf[t - 1] = key;
	  mock_input = t;
	  goto replay_sequence;
	}
      /* If KEY is not defined in any of the keymaps,
	 and cannot be part of a function key or translation,
	 and is a shifted function key,
	 use the corresponding unshifted function key instead.  */
      if (first_binding == nmaps && ! function_key_possible
	  && ! key_translation_possible
	  && SYMBOLP (key))
	{
	  Lisp_Object breakdown;
	  int modifiers;

	  original_uppercase = key;
	  original_uppercase_position = t - 1;

	  breakdown = parse_modifiers (key);
	  modifiers = XINT (XCONS (XCONS (breakdown)->cdr)->car);
	  if (modifiers & shift_modifier)
	    {
	      modifiers &= ~shift_modifier;
	      key = apply_modifiers (make_number (modifiers),
				     XCONS (breakdown)->car);

	      keybuf[t - 1] = key;
	      mock_input = t;
	      goto replay_sequence;
	    }
	}
    }

  if (!dummyflag)
    read_key_sequence_cmd = (first_binding < nmaps
			     ? defs[first_binding]
			     : Qnil);

  unread_switch_frame = delayed_switch_frame;
  unbind_to (count, Qnil);

  if (dont_downcase_last && t - 1 == original_uppercase_position)
    keybuf[t - 1] = original_uppercase;

  /* Occasionally we fabricate events, perhaps by expanding something
     according to function-key-map, or by adding a prefix symbol to a
     mouse click in the scroll bar or modeline.  In this cases, return
     the entire generated key sequence, even if we hit an unbound
     prefix or a definition before the end.  This means that you will
     be able to push back the event properly, and also means that
     read-key-sequence will always return a logical unit.

     Better ideas?  */
  for (; t < mock_input; t++)
    {
      if (echo_keystrokes)
	echo_char (keybuf[t]);
      add_command_key (keybuf[t]);
    }

  return t;
}

#if 0 /* This doc string is too long for some compilers.
	 This commented-out definition serves for DOC.  */
DEFUN ("read-key-sequence", Fread_key_sequence, Sread_key_sequence, 1, 2, 0,
  "Read a sequence of keystrokes and return as a string or vector.\n\
The sequence is sufficient to specify a non-prefix command in the\n\
current local and global maps.\n\
\n\
First arg PROMPT is a prompt string.  If nil, do not prompt specially.\n\
Second (optional) arg CONTINUE-ECHO, if non-nil, means this key echos\n\
as a continuation of the previous key.\n\
\n\
The third (optional) arg DONT-DOWNCASE-LAST, if non-nil, means do not\n\
convert the last event to lower case.  (Normally any upper case event\n\
is converted to lower case if the original event is undefined and the lower\n\
case equivalent is defined.)  A non-nil value is appropriate for reading\n\
a key sequence to be defined.\n\
\n\
A C-g typed while in this function is treated like any other character,\n\
and `quit-flag' is not set.\n\
\n\
If the key sequence starts with a mouse click, then the sequence is read\n\
using the keymaps of the buffer of the window clicked in, not the buffer\n\
of the selected window as normal.\n\
""\n\
`read-key-sequence' drops unbound button-down events, since you normally\n\
only care about the click or drag events which follow them.  If a drag\n\
or multi-click event is unbound, but the corresponding click event would\n\
be bound, `read-key-sequence' turns the event into a click event at the\n\
drag's starting position.  This means that you don't have to distinguish\n\
between click and drag, double, or triple events unless you want to.\n\
\n\
`read-key-sequence' prefixes mouse events on mode lines, the vertical\n\
lines separating windows, and scroll bars with imaginary keys\n\
`mode-line', `vertical-line', and `vertical-scroll-bar'.\n\
\n\
If the user switches frames in the middle of a key sequence, the\n\
frame-switch event is put off until after the current key sequence.\n\
\n\
`read-key-sequence' checks `function-key-map' for function key\n\
sequences, where they wouldn't conflict with ordinary bindings.  See\n\
`function-key-map' for more details.")
  (prompt, continue_echo)
#endif

DEFUN ("read-key-sequence", Fread_key_sequence, Sread_key_sequence, 1, 3, 0,
  0)
  (prompt, continue_echo, dont_downcase_last)
     Lisp_Object prompt, continue_echo, dont_downcase_last;
{
  Lisp_Object keybuf[30];
  register int i;
  struct gcpro gcpro1, gcpro2;

  if (!NILP (prompt))
    CHECK_STRING (prompt, 0);
  QUIT;

  bzero (keybuf, sizeof keybuf);
  GCPRO1 (keybuf[0]);
  gcpro1.nvars = (sizeof keybuf/sizeof (keybuf[0]));

  if (NILP (continue_echo))
    this_command_key_count = 0;

  i = read_key_sequence (keybuf, (sizeof keybuf/sizeof (keybuf[0])),
			 prompt, ! NILP (dont_downcase_last));

  if (i == -1)
    {
      Vquit_flag = Qt;
      QUIT;
    }
  UNGCPRO;
  return make_event_array (i, keybuf);
}

DEFUN ("command-execute", Fcommand_execute, Scommand_execute, 1, 2, 0,
 "Execute CMD as an editor command.\n\
CMD must be a symbol that satisfies the `commandp' predicate.\n\
Optional second arg RECORD-FLAG non-nil\n\
means unconditionally put this command in `command-history'.\n\
Otherwise, that is done only if an arg is read using the minibuffer.")
     (cmd, record)
     Lisp_Object cmd, record;
{
  register Lisp_Object final;
  register Lisp_Object tem;
  Lisp_Object prefixarg;
  struct backtrace backtrace;
  extern int debug_on_next_call;

  prefixarg = current_perdisplay->Vprefix_arg;
  current_perdisplay->Vprefix_arg = Qnil;
  current_perdisplay->Vcurrent_prefix_arg = prefixarg;
  debug_on_next_call = 0;

  if (SYMBOLP (cmd))
    {
      tem = Fget (cmd, Qdisabled);
      if (!NILP (tem) && !NILP (Vrun_hooks))
	return call1 (Vrun_hooks, Qdisabled_command_hook);
    }

  while (1)
    {
      final = Findirect_function (cmd);

      if (CONSP (final) && (tem = Fcar (final), EQ (tem, Qautoload)))
	do_autoload (final, cmd);
      else
	break;
    }

  if (STRINGP (final) || VECTORP (final))
    {
      /* If requested, place the macro in the command history.  For
	 other sorts of commands, call-interactively takes care of
	 this.  */
      if (!NILP (record))
	Vcommand_history
	  = Fcons (Fcons (Qexecute_kbd_macro,
			  Fcons (final, Fcons (prefixarg, Qnil))),
		   Vcommand_history);

      return Fexecute_kbd_macro (final, prefixarg);
    }
  if (CONSP (final) || SUBRP (final) || COMPILEDP (final))
    {
      backtrace.next = backtrace_list;
      backtrace_list = &backtrace;
      backtrace.function = &Qcall_interactively;
      backtrace.args = &cmd;
      backtrace.nargs = 1;
      backtrace.evalargs = 0;

      tem = Fcall_interactively (cmd, record);

      backtrace_list = backtrace.next;
      return tem;
    }
  return Qnil;
}

DEFUN ("execute-extended-command", Fexecute_extended_command, Sexecute_extended_command,
  1, 1, "P",
  "Read function name, then read its arguments and call it.")
  (prefixarg)
     Lisp_Object prefixarg;
{
  Lisp_Object function;
  char buf[40];
  Lisp_Object saved_keys;
  struct gcpro gcpro1;

  saved_keys = Fvector (this_command_key_count,
			XVECTOR (this_command_keys)->contents);
  buf[0] = 0;
  GCPRO1 (saved_keys);

  if (EQ (prefixarg, Qminus))
    strcpy (buf, "- ");
  else if (CONSP (prefixarg) && XINT (XCONS (prefixarg)->car) == 4)
    strcpy (buf, "C-u ");
  else if (CONSP (prefixarg) && INTEGERP (XCONS (prefixarg)->car))
    sprintf (buf, "%d ", XINT (XCONS (prefixarg)->car));
  else if (INTEGERP (prefixarg))
    sprintf (buf, "%d ", XINT (prefixarg));

  /* This isn't strictly correct if execute-extended-command
     is bound to anything else.  Perhaps it should use
     this_command_keys?  */
  strcat (buf, "M-x ");

  /* Prompt with buf, and then read a string, completing from and
     restricting to the set of all defined commands.  Don't provide
     any initial input.  Save the command read on the extended-command
     history list. */
  function = Fcompleting_read (build_string (buf),
			       Vobarray, Qcommandp,
			       Qt, Qnil, Qextended_command_history);

  /* Set this_command_keys to the concatenation of saved_keys and
     function, followed by a RET.  */
  {
    struct Lisp_String *str;
    Lisp_Object *keys;
    int i;
    Lisp_Object tem;

    this_command_key_count = 0;

    keys = XVECTOR (saved_keys)->contents;
    for (i = 0; i < XVECTOR (saved_keys)->size; i++)
      add_command_key (keys[i]);

    str = XSTRING (function);
    for (i = 0; i < str->size; i++)
      {
	XSETFASTINT (tem, str->data[i]);
	add_command_key (tem);
      }

    XSETFASTINT (tem, '\015');
    add_command_key (tem);
  }

  UNGCPRO;

  function = Fintern (function, Qnil);
  current_perdisplay->Vprefix_arg = prefixarg;
  this_command = function;

  return Fcommand_execute (function, Qt);
}


detect_input_pending ()
{
  if (!input_pending)
    get_input_pending (&input_pending);

  return input_pending;
}

/* This is called in some cases before a possible quit.
   It cases the next call to detect_input_pending to recompute input_pending.
   So calling this function unnecessarily can't do any harm.  */
clear_input_pending ()
{
  input_pending = 0;
}

DEFUN ("input-pending-p", Finput_pending_p, Sinput_pending_p, 0, 0, 0,
  "T if command input is currently available with no waiting.\n\
Actually, the value is nil only if we can be sure that no input is available.")
  ()
{
  if (!NILP (Vunread_command_events) || unread_command_char != -1)
    return (Qt);

  return detect_input_pending () ? Qt : Qnil;
}

DEFUN ("recent-keys", Frecent_keys, Srecent_keys, 0, 0, 0,
  "Return vector of last 100 events, not counting those from keyboard macros.")
  ()
{
  Lisp_Object *keys = XVECTOR (recent_keys)->contents;
  Lisp_Object val;

  if (total_keys < NUM_RECENT_KEYS)
    return Fvector (total_keys, keys);
  else
    {
      val = Fvector (NUM_RECENT_KEYS, keys);
      bcopy (keys + recent_keys_index,
	     XVECTOR (val)->contents,
	     (NUM_RECENT_KEYS - recent_keys_index) * sizeof (Lisp_Object));
      bcopy (keys,
	     XVECTOR (val)->contents + NUM_RECENT_KEYS - recent_keys_index,
	     recent_keys_index * sizeof (Lisp_Object));
      return val;
    }
}

DEFUN ("this-command-keys", Fthis_command_keys, Sthis_command_keys, 0, 0, 0,
  "Return the key sequence that invoked this command.\n\
The value is a string or a vector.")
  ()
{
  return make_event_array (this_command_key_count,
			   XVECTOR (this_command_keys)->contents);
}

DEFUN ("recursion-depth", Frecursion_depth, Srecursion_depth, 0, 0, 0,
  "Return the current depth in recursive edits.")
  ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, command_loop_level + minibuf_level);
  return temp;
}

DEFUN ("open-dribble-file", Fopen_dribble_file, Sopen_dribble_file, 1, 1,
  "FOpen dribble file: ",
  "Start writing all keyboard characters to a dribble file called FILE.\n\
If FILE is nil, close any open dribble file.")
  (file)
     Lisp_Object file;
{
  if (NILP (file))
    {
      if (dribble)
	{
	  fclose (dribble);
	  dribble = 0;
	}
    }
  else
    {
      file = Fexpand_file_name (file, Qnil);
      dribble = fopen (XSTRING (file)->data, "w");
    }
  return Qnil;
}

DEFUN ("discard-input", Fdiscard_input, Sdiscard_input, 0, 0, 0,
  "Discard the contents of the terminal input buffer.\n\
Also cancel any kbd macro being defined.")
  ()
{
  defining_kbd_macro = 0;
  update_mode_lines++;

  Vunread_command_events = Qnil;
  unread_command_char = -1;

  discard_tty_input ();

  /* Without the cast, GCC complains that this assignment loses the
     volatile qualifier of kbd_store_ptr.  Is there anything wrong
     with that?  */
  kbd_fetch_ptr = (struct input_event *) kbd_store_ptr;
  Ffillarray (kbd_buffer_frame_or_window, Qnil);
  input_pending = 0;

  return Qnil;
}

DEFUN ("suspend-emacs", Fsuspend_emacs, Ssuspend_emacs, 0, 1, "",
  "Stop Emacs and return to superior process.  You can resume later.\n\
If `cannot-suspend' is non-nil, or if the system doesn't support job\n\
control, run a subshell instead.\n\n\
If optional arg STUFFSTRING is non-nil, its characters are stuffed\n\
to be read as terminal input by Emacs's parent, after suspension.\n\
\n\
Before suspending, run the normal hook `suspend-hook'.\n\
After resumption run the normal hook `suspend-resume-hook'.\n\
\n\
Some operating systems cannot stop the Emacs process and resume it later.\n\
On such systems, Emacs starts a subshell instead of suspending.")
  (stuffstring)
     Lisp_Object stuffstring;
{
  Lisp_Object tem;
  int count = specpdl_ptr - specpdl;
  int old_height, old_width;
  int width, height;
  struct gcpro gcpro1, gcpro2;
  extern init_sys_modes ();

  if (!NILP (stuffstring))
    CHECK_STRING (stuffstring, 0);

  /* Run the functions in suspend-hook.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, intern ("suspend-hook"));

  GCPRO1 (stuffstring);
  get_frame_size (&old_width, &old_height);
  reset_sys_modes ();
  /* sys_suspend can get an error if it tries to fork a subshell
     and the system resources aren't available for that.  */
  record_unwind_protect (init_sys_modes, 0);
  stuff_buffered_input (stuffstring);
  if (cannot_suspend)
    sys_subshell ();
  else
    sys_suspend ();
  unbind_to (count, Qnil);

  /* Check if terminal/window size has changed.
     Note that this is not useful when we are running directly
     with a window system; but suspend should be disabled in that case.  */
  get_frame_size (&width, &height);
  if (width != old_width || height != old_height)
    change_frame_size (selected_frame, height, width, 0, 0);

  /* Run suspend-resume-hook.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, intern ("suspend-resume-hook"));
  
  UNGCPRO;
  return Qnil;
}

/* If STUFFSTRING is a string, stuff its contents as pending terminal input.
   Then in any case stuff anything Emacs has read ahead and not used.  */

stuff_buffered_input (stuffstring)
     Lisp_Object stuffstring;
{
/* stuff_char works only in BSD, versions 4.2 and up.  */
#ifdef BSD
#ifndef BSD4_1
  register unsigned char *p;

  if (STRINGP (stuffstring))
    {
      register int count;

      p = XSTRING (stuffstring)->data;
      count = XSTRING (stuffstring)->size;
      while (count-- > 0)
	stuff_char (*p++);
      stuff_char ('\n');
    }
  /* Anything we have read ahead, put back for the shell to read.  */
  /* ?? What should this do when we have multiple keyboards??
     Should we ignore anything that was typed in at the "wrong" display?  */
  for (; kbd_fetch_ptr != kbd_store_ptr; kbd_fetch_ptr++)
    {
      if (kbd_fetch_ptr == kbd_buffer + KBD_BUFFER_SIZE)
	kbd_fetch_ptr = kbd_buffer;
      if (kbd_fetch_ptr->kind == ascii_keystroke)
	stuff_char (kbd_fetch_ptr->code);
      kbd_fetch_ptr->kind = no_event;
      (XVECTOR (kbd_buffer_frame_or_window)->contents[kbd_fetch_ptr
						      - kbd_buffer]
       = Qnil);
    }
  input_pending = 0;
#endif
#endif /* BSD and not BSD4_1 */
}

set_waiting_for_input (time_to_clear)
     EMACS_TIME *time_to_clear;
{
  input_available_clear_time = time_to_clear;

  /* Tell interrupt_signal to throw back to read_char,  */
  waiting_for_input = 1;

  /* If interrupt_signal was called before and buffered a C-g,
     make it run again now, to avoid timing error. */
  if (!NILP (Vquit_flag))
    quit_throw_to_read_char ();
}

clear_waiting_for_input ()
{
  /* Tell interrupt_signal not to throw back to read_char,  */
  waiting_for_input = 0;
  input_available_clear_time = 0;
}

/* This routine is called at interrupt level in response to C-G.
 If interrupt_input, this is the handler for SIGINT.
 Otherwise, it is called from kbd_buffer_store_event,
 in handling SIGIO or SIGTINT.

 If `waiting_for_input' is non zero, then unless `echoing' is nonzero,
 immediately throw back to read_char.

 Otherwise it sets the Lisp variable  quit-flag  not-nil.
 This causes  eval  to throw, when it gets a chance.
 If  quit-flag  is already non-nil, it stops the job right away.  */

SIGTYPE
interrupt_signal (signalnum)	/* If we don't have an argument, */
     int signalnum;		/* some compilers complain in signal calls. */
{
  char c;
  /* Must preserve main program's value of errno.  */
  int old_errno = errno;

#ifdef USG
  if (!read_socket_hook && NILP (Vwindow_system))
    {
      /* USG systems forget handlers when they are used;
	 must reestablish each time */
      signal (SIGINT, interrupt_signal);
      signal (SIGQUIT, interrupt_signal);
    }
#endif /* USG */

  cancel_echoing ();

  if (!NILP (Vquit_flag) && FRAME_TERMCAP_P (selected_frame))
    {
      fflush (stdout);
      reset_sys_modes ();
      sigfree ();
#ifdef SIGTSTP			/* Support possible in later USG versions */
/*
 * On systems which can suspend the current process and return to the original
 * shell, this command causes the user to end up back at the shell.
 * The "Auto-save" and "Abort" questions are not asked until
 * the user elects to return to emacs, at which point he can save the current
 * job and either dump core or continue.
 */
      sys_suspend ();
#else
#ifdef VMS
      if (sys_suspend () == -1)
	{
	  printf ("Not running as a subprocess;\n");
	  printf ("you can continue or abort.\n");
	}
#else /* not VMS */
      /* Perhaps should really fork an inferior shell?
	 But that would not provide any way to get back
	 to the original shell, ever.  */
      printf ("No support for stopping a process on this operating system;\n");
      printf ("you can continue or abort.\n");
#endif /* not VMS */
#endif /* not SIGTSTP */
#ifdef MSDOS
      /* We must remain inside the screen area when the internal terminal
	 is used.  Note that [Enter] is not echoed by dos.  */
      cursor_to (0, 0);
#endif
      printf ("Auto-save? (y or n) ");
      fflush (stdout);
      if (((c = getchar ()) & ~040) == 'Y')
	{
	  Fdo_auto_save (Qt, Qnil);
#ifdef MSDOS
	  printf ("\r\nAuto-save done");
#else /* not MSDOS */
	  printf ("Auto-save done\n");
#endif /* not MSDOS */
	}
      while (c != '\n') c = getchar ();
#ifdef MSDOS
      printf ("\r\nAbort?  (y or n) ");
#else /* not MSDOS */
#ifdef VMS
      printf ("Abort (and enter debugger)? (y or n) ");
#else /* not VMS */
      printf ("Abort (and dump core)? (y or n) ");
#endif /* not VMS */
#endif /* not MSDOS */
      fflush (stdout);
      if (((c = getchar ()) & ~040) == 'Y')
	abort ();
      while (c != '\n') c = getchar ();
#ifdef MSDOS
      printf ("\r\nContinuing...\r\n");
#else /* not MSDOS */
      printf ("Continuing...\n");
#endif /* not MSDOS */
      fflush (stdout);
      init_sys_modes ();
    }
  else
    {
      /* If executing a function that wants to be interrupted out of
	 and the user has not deferred quitting by binding `inhibit-quit'
	 then quit right away.  */
      if (immediate_quit && NILP (Vinhibit_quit))
	{
	  immediate_quit = 0;
          sigfree ();
	  Fsignal (Qquit, Qnil);
	}
      else
	/* Else request quit when it's safe */
	Vquit_flag = Qt;
    }

  if (waiting_for_input && !echoing)
    quit_throw_to_read_char ();

  errno = old_errno;
}

/* Handle a C-g by making read_char return C-g.  */

quit_throw_to_read_char ()
{
  quit_error_check ();
  sigfree ();
  /* Prevent another signal from doing this before we finish.  */
  clear_waiting_for_input ();
  input_pending = 0;

  Vunread_command_events = Qnil;
  unread_command_char = -1;

#ifdef POLL_FOR_INPUT
  /* May be > 1 if in recursive minibuffer.  */
  if (poll_suppress_count == 0)
    abort ();
#endif
#ifdef MULTI_FRAME
  {
    Lisp_Object frame;

    if (!current_perdisplay)
      abort ();
    frame = current_perdisplay->internal_last_event_frame;
    if (FRAMEP (frame) && XFRAME (frame) != selected_frame)
      Fhandle_switch_frame (make_lispy_switch_frame (frame));
  }
#endif

  _longjmp (getcjmp, 1);
}

DEFUN ("set-input-mode", Fset_input_mode, Sset_input_mode, 3, 4, 0,
  "Set mode of reading keyboard input.\n\
First arg INTERRUPT non-nil means use input interrupts;\n\
 nil means use CBREAK mode.\n\
Second arg FLOW non-nil means use ^S/^Q flow control for output to terminal\n\
 (no effect except in CBREAK mode).\n\
Third arg META t means accept 8-bit input (for a Meta key).\n\
 META nil means ignore the top bit, on the assumption it is parity.\n\
 Otherwise, accept 8-bit input and don't use the top bit for Meta.\n\
Optional fourth arg QUIT if non-nil specifies character to use for quitting.\n\
See also `current-input-mode'.")
  (interrupt, flow, meta, quit)
     Lisp_Object interrupt, flow, meta, quit;
{
  if (!NILP (quit)
      && (!INTEGERP (quit) || XINT (quit) < 0 || XINT (quit) > 0400))
    error ("set-input-mode: QUIT must be an ASCII character");

#ifdef POLL_FOR_INPUT
  stop_polling ();
#endif

  reset_sys_modes ();
#ifdef SIGIO
/* Note SIGIO has been undef'd if FIONREAD is missing.  */
#ifdef NO_SOCK_SIGIO
  if (read_socket_hook)
    interrupt_input = 0;	/* No interrupts if reading from a socket.  */
  else
#endif /* NO_SOCK_SIGIO */
    interrupt_input = !NILP (interrupt);
#else /* not SIGIO */
  interrupt_input = 0;
#endif /* not SIGIO */
/* Our VMS input only works by interrupts, as of now.  */
#ifdef VMS
  interrupt_input = 1;
#endif
  flow_control = !NILP (flow);
  if (NILP (meta))
    meta_key = 0;
  else if (EQ (meta, Qt))
    meta_key = 1;
  else
    meta_key = 2;
  if (!NILP (quit))
    /* Don't let this value be out of range.  */
    quit_char = XINT (quit) & (meta_key ? 0377 : 0177);

  init_sys_modes ();

#ifdef POLL_FOR_INPUT
  poll_suppress_count = 1;
  start_polling ();
#endif
  return Qnil;
}

DEFUN ("current-input-mode", Fcurrent_input_mode, Scurrent_input_mode, 0, 0, 0,
  "Return information about the way Emacs currently reads keyboard input.\n\
The value is a list of the form (INTERRUPT FLOW META QUIT), where\n\
  INTERRUPT is non-nil if Emacs is using interrupt-driven input; if\n\
    nil, Emacs is using CBREAK mode.\n\
  FLOW is non-nil if Emacs uses ^S/^Q flow control for output to the\n\
    terminal; this does not apply if Emacs uses interrupt-driven input.\n\
  META is t if accepting 8-bit input with 8th bit as Meta flag.\n\
    META nil means ignoring the top bit, on the assumption it is parity.\n\
    META is neither t nor nil if accepting 8-bit input and using\n\
    all 8 bits as the character code.\n\
  QUIT is the character Emacs currently uses to quit.\n\
The elements of this list correspond to the arguments of\n\
`set-input-mode'.")
  ()
{
  Lisp_Object val[4];

  val[0] = interrupt_input ? Qt : Qnil;
  val[1] = flow_control ? Qt : Qnil;
  val[2] = meta_key == 2 ? make_number (0) : meta_key == 1 ? Qt : Qnil;
  XSETFASTINT (val[3], quit_char);

  return Flist (sizeof (val) / sizeof (val[0]), val);
}


/*
 * Set up a perdisplay object with reasonable initial values.
 */
void
init_perdisplay (perd)
     PERDISPLAY *perd;
{
  perd->Vprefix_arg = Qnil;
  perd->Vcurrent_prefix_arg = Qnil;
#ifdef MULTI_FRAME
  /* This means that command_loop_1 won't try to select anything the first
     time through.  */
  perd->internal_last_event_frame = Qnil;
#endif
  perd->kbd_queue = Qnil;
  perd->Vlast_event_frame = Qnil;
  perd->immediate_echo = 0;
  perd->echoptr = perd->echobuf;
  perd->echo_after_prompt = -1;
}

/*
 * Destroy the contents of a perdisplay object, but not the object itself.
 * We use this just before deleteing it, or if we're going to initialize
 * it a second time.
 */
void
wipe_perdisplay (perd)
     PERDISPLAY *perd;
{
  /* Free anything that was malloc'd in init_perdisplay.  */
  /* Placeholder -- currently no action required.  */
}

init_keyboard ()
{
  /* This is correct before outermost invocation of the editor loop */
  command_loop_level = -1;
  immediate_quit = 0;
  quit_char = Ctl ('g');
  Vunread_command_events = Qnil;
  unread_command_char = -1;
  total_keys = 0;
  recent_keys_index = 0;
  kbd_fetch_ptr = kbd_buffer;
  kbd_store_ptr = kbd_buffer;
  kbd_buffer_frame_or_window
    = Fmake_vector (make_number (KBD_BUFFER_SIZE), Qnil);
#ifdef HAVE_MOUSE
  do_mouse_tracking = Qnil;
#endif
  input_pending = 0;

#ifndef MULTI_PERDISPLAY
  if (initialized)
    wipe_perdisplay (&the_only_perdisplay);
  init_perdisplay (&the_only_perdisplay);
#endif

  if (initialized)
    Ffillarray (kbd_buffer_frame_or_window, Qnil);

  kbd_buffer_frame_or_window
    = Fmake_vector (make_number (KBD_BUFFER_SIZE), Qnil);
  if (!noninteractive && !read_socket_hook && NILP (Vwindow_system))
    {
      signal (SIGINT, interrupt_signal);
#if defined (HAVE_TERMIO) || defined (HAVE_TERMIOS)
      /* For systems with SysV TERMIO, C-g is set up for both SIGINT and
	 SIGQUIT and we can't tell which one it will give us.  */
      signal (SIGQUIT, interrupt_signal);
#endif /* HAVE_TERMIO */
    }
/* Note SIGIO has been undef'd if FIONREAD is missing.  */
#ifdef SIGIO
  if (!noninteractive)
    signal (SIGIO, input_available_signal);
#endif /* SIGIO */

/* Use interrupt input by default, if it works and noninterrupt input
   has deficiencies.  */

#ifdef INTERRUPT_INPUT
  interrupt_input = 1;
#else
  interrupt_input = 0;
#endif

/* Our VMS input only works by interrupts, as of now.  */
#ifdef VMS
  interrupt_input = 1;
#endif

  sigfree ();
  dribble = 0;

  if (keyboard_init_hook)
    (*keyboard_init_hook) ();

#ifdef POLL_FOR_INPUT
  poll_suppress_count = 1;
  start_polling ();
#endif
}

/* This type's only use is in syms_of_keyboard, to initialize the 
   event header symbols and put properties on them.  */
struct event_head {
  Lisp_Object *var;
  char *name;
  Lisp_Object *kind;
};

struct event_head head_table[] = {
  &Qmouse_movement,	"mouse-movement",	&Qmouse_movement,
  &Qscroll_bar_movement, "scroll-bar-movement",	&Qmouse_movement,
  &Qswitch_frame,	"switch-frame",		&Qswitch_frame,
  &Qdelete_frame,	"delete-frame",		&Qdelete_frame,
  &Qiconify_frame,	"iconify-frame",	&Qiconify_frame,
  &Qmake_frame_visible,	"make-frame-visible",	&Qmake_frame_visible,
};

syms_of_keyboard ()
{
  Qdisabled_command_hook = intern ("disabled-command-hook");
  staticpro (&Qdisabled_command_hook);

  Qself_insert_command = intern ("self-insert-command");
  staticpro (&Qself_insert_command);

  Qforward_char = intern ("forward-char");
  staticpro (&Qforward_char);

  Qbackward_char = intern ("backward-char");
  staticpro (&Qbackward_char);

  Qdisabled = intern ("disabled");
  staticpro (&Qdisabled);

  Qundefined = intern ("undefined");
  staticpro (&Qundefined);

  Qpre_command_hook = intern ("pre-command-hook");
  staticpro (&Qpre_command_hook);

  Qpost_command_hook = intern ("post-command-hook");
  staticpro (&Qpost_command_hook);

  Qdeferred_action_function = intern ("deferred-action-function");
  staticpro (&Qdeferred_action_function);

  Qcommand_hook_internal = intern ("command-hook-internal");
  staticpro (&Qcommand_hook_internal);

  Qfunction_key = intern ("function-key");
  staticpro (&Qfunction_key);
  Qmouse_click = intern ("mouse-click");
  staticpro (&Qmouse_click);

  Qmenu_enable = intern ("menu-enable");
  staticpro (&Qmenu_enable);

  Qmode_line = intern ("mode-line");
  staticpro (&Qmode_line);
  Qvertical_line = intern ("vertical-line");
  staticpro (&Qvertical_line);
  Qvertical_scroll_bar = intern ("vertical-scroll-bar");
  staticpro (&Qvertical_scroll_bar);
  Qmenu_bar = intern ("menu-bar");
  staticpro (&Qmenu_bar);

  Qabove_handle = intern ("above-handle");
  staticpro (&Qabove_handle);
  Qhandle = intern ("handle");
  staticpro (&Qhandle);
  Qbelow_handle = intern ("below-handle");
  staticpro (&Qbelow_handle);

  Qevent_kind = intern ("event-kind");
  staticpro (&Qevent_kind);
  Qevent_symbol_elements = intern ("event-symbol-elements");
  staticpro (&Qevent_symbol_elements);
  Qevent_symbol_element_mask = intern ("event-symbol-element-mask");
  staticpro (&Qevent_symbol_element_mask);
  Qmodifier_cache = intern ("modifier-cache");
  staticpro (&Qmodifier_cache);

  Qrecompute_lucid_menubar = intern ("recompute-lucid-menubar");
  staticpro (&Qrecompute_lucid_menubar);
  Qactivate_menubar_hook = intern ("activate-menubar-hook");
  staticpro (&Qactivate_menubar_hook);

  Qpolling_period = intern ("polling-period");
  staticpro (&Qpolling_period);

  {
    struct event_head *p;

    for (p = head_table;
	 p < head_table + (sizeof (head_table) / sizeof (head_table[0]));
	 p++)
      {
	*p->var = intern (p->name);
	staticpro (p->var);
	Fput (*p->var, Qevent_kind, *p->kind);
	Fput (*p->var, Qevent_symbol_elements, Fcons (*p->var, Qnil));
      }
  }

  button_down_location = Fmake_vector (make_number (NUM_MOUSE_BUTTONS), Qnil);
  staticpro (&button_down_location);

  {
    int i;
    int len = sizeof (modifier_names) / sizeof (modifier_names[0]);

    modifier_symbols = Fmake_vector (make_number (len), Qnil);
    for (i = 0; i < len; i++)
      if (modifier_names[i])
	XVECTOR (modifier_symbols)->contents[i] = intern (modifier_names[i]);
    staticpro (&modifier_symbols);
  }

  recent_keys = Fmake_vector (make_number (NUM_RECENT_KEYS), Qnil);
  staticpro (&recent_keys);

  this_command_keys = Fmake_vector (make_number (40), Qnil);
  staticpro (&this_command_keys);

  Qextended_command_history = intern ("extended-command-history");
  Fset (Qextended_command_history, Qnil);
  staticpro (&Qextended_command_history);

  kbd_buffer_frame_or_window
    = Fmake_vector (make_number (KBD_BUFFER_SIZE), Qnil);
  staticpro (&kbd_buffer_frame_or_window);

  accent_key_syms = Qnil;
  staticpro (&accent_key_syms);

  func_key_syms = Qnil;
  staticpro (&func_key_syms);

  system_key_syms = Qnil;
  staticpro (&system_key_syms);

  mouse_syms = Qnil;
  staticpro (&mouse_syms);

  unread_switch_frame = Qnil;
  staticpro (&unread_switch_frame);

  defsubr (&Sread_key_sequence);
  defsubr (&Srecursive_edit);
#ifdef HAVE_MOUSE
  defsubr (&Strack_mouse);
#endif
  defsubr (&Sinput_pending_p);
  defsubr (&Scommand_execute);
  defsubr (&Srecent_keys);
  defsubr (&Sthis_command_keys);
  defsubr (&Ssuspend_emacs);
  defsubr (&Sabort_recursive_edit);
  defsubr (&Sexit_recursive_edit);
  defsubr (&Srecursion_depth);
  defsubr (&Stop_level);
  defsubr (&Sdiscard_input);
  defsubr (&Sopen_dribble_file);
  defsubr (&Sset_input_mode);
  defsubr (&Scurrent_input_mode);
  defsubr (&Sexecute_extended_command);

  DEFVAR_LISP ("last-command-char", &last_command_char,
    "Last input event that was part of a command.");

  DEFVAR_LISP_NOPRO ("last-command-event", &last_command_char,
    "Last input event that was part of a command.");

  DEFVAR_LISP ("last-nonmenu-event", &last_nonmenu_event,
    "Last input event in a command, except for mouse menu events.\n\
Mouse menus give back keys that don't look like mouse events;\n\
this variable holds the actual mouse event that led to the menu,\n\
so that you can determine whether the command was run by mouse or not.");

  DEFVAR_LISP ("last-input-char", &last_input_char,
    "Last input event.");

  DEFVAR_LISP_NOPRO ("last-input-event", &last_input_char,
    "Last input event.");

  DEFVAR_LISP ("unread-command-events", &Vunread_command_events,
    "List of objects to be read as next command input events.");

  DEFVAR_INT ("unread-command-char", &unread_command_char,
    "If not -1, an object to be read as next command input event.");

  DEFVAR_LISP ("meta-prefix-char", &meta_prefix_char,
    "Meta-prefix character code.  Meta-foo as command input\n\
turns into this character followed by foo.");
  XSETINT (meta_prefix_char, 033);

  DEFVAR_LISP ("last-command", &last_command,
    "The last command executed.  Normally a symbol with a function definition,\n\
but can be whatever was found in the keymap, or whatever the variable\n\
`this-command' was set to by that command.\n\
\n\
The value `mode-exit' is special; it means that the previous command\n\
read an event that told it to exit, and it did so and unread that event.\n\
In other words, the present command is the event that made the previous\n\
command exit.\n\
\n\
The value `kill-region' is special; it means that the previous command\n\
was a kill command.");
  last_command = Qnil;

  DEFVAR_LISP ("this-command", &this_command,
    "The command now being executed.\n\
The command can set this variable; whatever is put here\n\
will be in `last-command' during the following command.");
  this_command = Qnil;

  DEFVAR_INT ("auto-save-interval", &auto_save_interval,
    "*Number of keyboard input characters between auto-saves.\n\
Zero means disable autosaving due to number of characters typed.");
  auto_save_interval = 300;

  DEFVAR_LISP ("auto-save-timeout", &Vauto_save_timeout,
    "*Number of seconds idle time before auto-save.\n\
Zero or nil means disable auto-saving due to idleness.\n\
After auto-saving due to this many seconds of idle time,\n\
Emacs also does a garbage collection if that seems to be warranted.");
  XSETFASTINT (Vauto_save_timeout, 30);

  DEFVAR_INT ("echo-keystrokes", &echo_keystrokes,
    "*Nonzero means echo unfinished commands after this many seconds of pause.");
  echo_keystrokes = 1;

  DEFVAR_INT ("polling-period", &polling_period,
    "*Interval between polling for input during Lisp execution.\n\
The reason for polling is to make C-g work to stop a running program.\n\
Polling is needed only when using X windows and SIGIO does not work.\n\
Polling is automatically disabled in all other cases.");
  polling_period = 2;
  
  DEFVAR_LISP ("double-click-time", &Vdouble_click_time,
    "*Maximum time between mouse clicks to make a double-click.\n\
Measured in milliseconds.  nil means disable double-click recognition;\n\
t means double-clicks have no time limit and are detected\n\
by position only.");
  Vdouble_click_time = make_number (500);

  DEFVAR_BOOL ("inhibit-local-menu-bar-menus", &inhibit_local_menu_bar_menus,
    "*Non-nil means inhibit local map menu bar menus.");
  inhibit_local_menu_bar_menus = 0;

  DEFVAR_INT ("num-input-keys", &num_input_keys,
    "Number of complete keys read from the keyboard so far.");
  num_input_keys = 0;

  DEFVAR_LISP ("help-char", &Vhelp_char,
    "Character to recognize as meaning Help.\n\
When it is read, do `(eval help-form)', and display result if it's a string.\n\
If the value of `help-form' is nil, this char can be read normally.");
  XSETINT (Vhelp_char, Ctl ('H'));

  DEFVAR_LISP ("help-form", &Vhelp_form,
    "Form to execute when character `help-char' is read.\n\
If the form returns a string, that string is displayed.\n\
If `help-form' is nil, the help char is not recognized.");
  Vhelp_form = Qnil;

  DEFVAR_LISP ("prefix-help-command", &Vprefix_help_command,
    "Command to run when `help-char' character follows a prefix key.\n\
This command is used only when there is no actual binding\n\
for that character after that prefix key.");
  Vprefix_help_command = Qnil;

  DEFVAR_LISP ("top-level", &Vtop_level,
    "Form to evaluate when Emacs starts up.\n\
Useful to set before you dump a modified Emacs.");
  Vtop_level = Qnil;

  DEFVAR_LISP ("keyboard-translate-table", &Vkeyboard_translate_table,
    "String used as translate table for keyboard input, or nil.\n\
Each character is looked up in this string and the contents used instead.\n\
If string is of length N, character codes N and up are untranslated.");
  Vkeyboard_translate_table = Qnil;

  DEFVAR_LISP ("key-translation-map", &Vkey_translation_map,
    "Keymap of key translations that can override keymaps.\n\
This keymap works like `function-key-map', but comes after that,\n\
and applies even for keys that have ordinary bindings.");
  Vkey_translation_map = Qnil;

  DEFVAR_BOOL ("cannot-suspend", &cannot_suspend,
    "Non-nil means to always spawn a subshell instead of suspending,\n\
even if the operating system has support for stopping a process.");
  cannot_suspend = 0;

  DEFVAR_BOOL ("menu-prompting", &menu_prompting,
    "Non-nil means prompt with menus when appropriate.\n\
This is done when reading from a keymap that has a prompt string,\n\
for elements that have prompt strings.\n\
The menu is displayed on the screen\n\
if X menus were enabled at configuration\n\
time and the previous event was a mouse click prefix key.\n\
Otherwise, menu prompting uses the echo area.");
  menu_prompting = 1;

  DEFVAR_LISP ("menu-prompt-more-char", &menu_prompt_more_char,
    "Character to see next line of menu prompt.\n\
Type this character while in a menu prompt to rotate around the lines of it.");
  XSETINT (menu_prompt_more_char, ' ');

  DEFVAR_INT ("extra-keyboard-modifiers", &extra_keyboard_modifiers,
    "A mask of additional modifier keys to use with every keyboard character.\n\
Emacs applies the modifiers of the character stored here to each keyboard\n\
character it reads.  For example, after evaluating the expression\n\
    (setq extra-keyboard-modifiers ?\\C-x)\n\
all input characters will have the control modifier applied to them.\n\
\n\
Note that the character ?\\C-@, equivalent to the integer zero, does\n\
not count as a control character; rather, it counts as a character\n\
with no modifiers; thus, setting `extra-keyboard-modifiers' to zero\n\
cancels any modification.");
  extra_keyboard_modifiers = 0;

  DEFVAR_LISP ("deactivate-mark", &Vdeactivate_mark,
    "If an editing command sets this to t, deactivate the mark afterward.\n\
The command loop sets this to nil before each command,\n\
and tests the value when the command returns.\n\
Buffer modification stores t in this variable.");
  Vdeactivate_mark = Qnil;

  DEFVAR_LISP ("command-hook-internal", &Vcommand_hook_internal,
    "Temporary storage of pre-command-hook or post-command-hook.");
  Vcommand_hook_internal = Qnil;

  DEFVAR_LISP ("pre-command-hook", &Vpre_command_hook,
    "Normal hook run before each command is executed.\n\
While the hook is run, its value is temporarily set to nil\n\
to avoid an unbreakable infinite loop if a hook function gets an error.\n\
As a result, a hook function cannot straightforwardly alter the value of\n\
`pre-command-hook'.  See the Emacs Lisp manual for a way of\n\
implementing hook functions that alter the set of hook functions.");
  Vpre_command_hook = Qnil;

  DEFVAR_LISP ("post-command-hook", &Vpost_command_hook,
    "Normal hook run after each command is executed.\n\
While the hook is run, its value is temporarily set to nil\n\
to avoid an unbreakable infinite loop if a hook function gets an error.\n\
As a result, a hook function cannot straightforwardly alter the value of\n\
`post-command-hook'.  See the Emacs Lisp manual for a way of\n\
implementing hook functions that alter the set of hook functions.");
  Vpost_command_hook = Qnil;

  DEFVAR_LISP ("lucid-menu-bar-dirty-flag", &Vlucid_menu_bar_dirty_flag,
    "t means menu bar, specified Lucid style, needs to be recomputed.");
  Vlucid_menu_bar_dirty_flag = Qnil;

  DEFVAR_LISP ("menu-bar-final-items", &Vmenu_bar_final_items,
    "List of menu bar items to move to the end of the menu bar.\n\
The elements of the list are event types that may have menu bar bindings.");
  Vmenu_bar_final_items = Qnil;

  DEFVAR_LISP ("overriding-local-map", &Voverriding_local_map,
    "Keymap that overrides all other local keymaps.\n\
If this variable is non-nil, it is used as a keymap instead of the\n\
buffer's local map, and the minor mode keymaps and text property keymaps.");
  Voverriding_local_map = Qnil;

  DEFVAR_LISP ("overriding-local-map-menu-flag", &Voverriding_local_map_menu_flag,
    "Non-nil means `overriding-local-map' applies to the menu bar.\n\
Otherwise, the menu bar continues to reflect the buffer's local map\n\
and the minor mode maps regardless of `overriding-local-map'.");
  Voverriding_local_map_menu_flag = Qnil;

#ifdef HAVE_MOUSE
  DEFVAR_LISP ("track-mouse", &do_mouse_tracking,
	       "*Non-nil means generate motion events for mouse motion.");
#endif

  DEFVAR_LISP ("system-key-alist", &Vsystem_key_alist,
    "Alist of system-specific X windows key symbols.\n\
Each element should have the form (N . SYMBOL) where N is the\n\
numeric keysym code (sans the \"system-specific\" bit 1<<28)\n\
and SYMBOL is its name.");
  Vsystem_key_alist = Qnil;

  DEFVAR_LISP ("deferred-action-list", &Vdeferred_action_list,
    "List of deferred actions to be performed at a later time.\n\
The precise format isn't relevant here; we just check whether it is nil.");
  Vdeferred_action_list = Qnil;

  DEFVAR_LISP ("deferred-action-function", &Vdeferred_action_function,
    "Function to call to handle deferred actions, after each command.\n\
This function is called with no arguments after each command\n\
whenever `deferred-action-list' is non-nil.");
  Vdeferred_action_function = Qnil;

  DEFVAR_DISPLAY ("prefix-arg", Vprefix_arg,
    "The value of the prefix argument for the next editing command.\n\
It may be a number, or the symbol `-' for just a minus sign as arg,\n\
or a list whose car is a number for just one or more C-U's\n\
or nil if no argument has been specified.\n\
\n\
You cannot examine this variable to find the argument for this command\n\
since it has been set to nil by the time you can look.\n\
Instead, you should use the variable `current-prefix-arg', although\n\
normally commands can get this prefix argument with (interactive \"P\").");

  DEFVAR_DISPLAY ("current-prefix-arg", Vcurrent_prefix_arg,
    "The value of the prefix argument for this editing command.\n\
It may be a number, or the symbol `-' for just a minus sign as arg,\n\
or a list whose car is a number for just one or more C-U's\n\
or nil if no argument has been specified.\n\
This is what `(interactive \"P\")' returns.");

  DEFVAR_DISPLAY ("last-event-frame", Vlast_event_frame,
    "The frame in which the most recently read event occurred.\n\
If the last event came from a keyboard macro, this is set to `macro'.");
}

keys_of_keyboard ()
{
  initial_define_key (global_map, Ctl ('Z'), "suspend-emacs");
  initial_define_key (control_x_map, Ctl ('Z'), "suspend-emacs");
  initial_define_key (meta_map, Ctl ('C'), "exit-recursive-edit");
  initial_define_key (global_map, Ctl (']'), "abort-recursive-edit");
  initial_define_key (meta_map, 'x', "execute-extended-command");
}
