/* Keyboard and mouse input; editor command loop.
   Copyright (C) 1985,86,87,88,89,93,94,95,96,97,99, 2000, 2001
     Free Software Foundation, Inc.

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
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <config.h>
#include <signal.h>
#include <stdio.h>
#include "termchar.h"
#include "termopts.h"
#include "lisp.h"
#include "termhooks.h"
#include "macros.h"
#include "keyboard.h"
#include "frame.h"
#include "window.h"
#include "commands.h"
#include "buffer.h"
#include "charset.h"
#include "disptab.h"
#include "dispextern.h"
#include "syntax.h"
#include "intervals.h"
#include "keymap.h"
#include "blockinput.h"
#include "puresize.h"
#include "systime.h"
#include "atimer.h"
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

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* This is to get the definitions of the XK_ symbols.  */
#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif

#ifdef HAVE_NTGUI
#include "w32term.h"
#endif /* HAVE_NTGUI */

#ifdef MAC_OS
#include "macterm.h"
#endif

#ifndef USE_CRT_DLL
extern int errno;
#endif

/* Variables for blockinput.h: */

/* Non-zero if interrupt input is blocked right now.  */
int interrupt_input_blocked;

/* Nonzero means an input interrupt has arrived
   during the current critical section.  */
int interrupt_input_pending;


/* File descriptor to use for input.  */
extern int input_fd;

#ifdef HAVE_WINDOW_SYSTEM
/* Make all keyboard buffers much bigger when using X windows.  */
#ifdef MAC_OS8
/* But not too big (local data > 32K error) if on Mac OS Classic.  */
#define KBD_BUFFER_SIZE 512
#else
#define KBD_BUFFER_SIZE 4096
#endif
#else	/* No X-windows, character input */
#define KBD_BUFFER_SIZE 4096
#endif	/* No X-windows */

#define abs(x)		((x) >= 0 ? (x) : -(x))

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

#ifdef MULTI_KBOARD
KBOARD *initial_kboard;
KBOARD *current_kboard;
KBOARD *all_kboards;
int single_kboard;
#else
KBOARD the_only_kboard;
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

/* This vector is used as a buffer to record the events that were actually read
   by read_key_sequence.  */
Lisp_Object raw_keybuf;
int raw_keybuf_count;

#define GROW_RAW_KEYBUF							\
 if (raw_keybuf_count == XVECTOR (raw_keybuf)->size)			\
  {									\
    int newsize = 2 * XVECTOR (raw_keybuf)->size;			\
    Lisp_Object new;							\
    new = Fmake_vector (make_number (newsize), Qnil);			\
    bcopy (XVECTOR (raw_keybuf)->contents, XVECTOR (new)->contents,	\
	   raw_keybuf_count * sizeof (Lisp_Object));			\
    raw_keybuf = new;							\
  }

/* Number of elements of this_command_keys
   that precede this key sequence.  */
int this_single_command_key_start;

/* Record values of this_command_key_count and echo_length ()
   before this command was read.  */
static int before_command_key_count;
static int before_command_echo_length;
/* Values of before_command_key_count and before_command_echo_length
   saved by reset-this-command-lengths.  */
static int before_command_key_count_1;
static int before_command_echo_length_1;
/* Flag set by reset-this-command-lengths,
   saying to reset the lengths when add_command_key is called.  */
static int before_command_restore_flag;

extern int minbuf_level;

extern int message_enable_multibyte;

extern struct backtrace *backtrace_list;

/* If non-nil, the function that implements the display of help.
   It's called with one argument, the help string to display.  */

Lisp_Object Vshow_help_function;

/* If a string, the message displayed before displaying a help-echo
   in the echo area.  */

Lisp_Object Vpre_help_message;

/* Nonzero means do menu prompting.  */

static int menu_prompting;

/* Character to see next line of menu prompt.  */

static Lisp_Object menu_prompt_more_char;

/* For longjmp to where kbd input is being done.  */

static jmp_buf getcjmp;

/* True while doing kbd input.  */
int waiting_for_input;

/* True while displaying for echoing.   Delays C-g throwing.  */

int echoing;

/* Non-null means we can start echoing at the next input pause even
   though there is something in the echo area.  */

static struct kboard *ok_to_echo_at_next_pause;

/* The kboard last echoing, or null for none.  Reset to 0 in
   cancel_echoing.  If non-null, and a current echo area message
   exists, and echo_message_buffer is eq to the current message
   buffer, we know that the message comes from echo_kboard.  */

struct kboard *echo_kboard;

/* The buffer used for echoing.  Set in echo_now, reset in
   cancel_echoing.  */

Lisp_Object echo_message_buffer;

/* Nonzero means disregard local maps for the menu bar.  */
static int inhibit_local_menu_bar_menus;

/* Nonzero means C-g should cause immediate error-signal.  */
int immediate_quit;

/* The user's ERASE setting.  */
Lisp_Object Vtty_erase_char;

/* Character to recognize as the help char.  */
Lisp_Object Vhelp_char;

/* List of other event types to recognize as meaning "help".  */
Lisp_Object Vhelp_event_list;

/* Form to execute when help char is typed.  */
Lisp_Object Vhelp_form;

/* Command to run when the help character follows a prefix key.  */
Lisp_Object Vprefix_help_command;

/* List of items that should move to the end of the menu bar.  */
Lisp_Object Vmenu_bar_final_items;

/* Non-nil means show the equivalent key-binding for
   any M-x command that has one.
   The value can be a length of time to show the message for.
   If the value is non-nil and not a number, we wait 2 seconds.  */
Lisp_Object Vsuggest_key_bindings;

/* How long to display an echo-area message when the minibuffer is active.
   If the value is not a number, such messages don't time out.  */
Lisp_Object Vminibuffer_message_timeout;

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

/* Keymap that defines special misc events that should
   be processed immediately at a low level.  */
Lisp_Object Vspecial_event_map;

/* Current depth in recursive edits.  */
int command_loop_level;

/* Total number of times command_loop has read a key sequence.  */
EMACS_INT num_input_keys;

/* Last input character read as a command.  */
Lisp_Object last_command_char;

/* Last input character read as a command, not counting menus
   reached by the mouse.  */
Lisp_Object last_nonmenu_event;

/* Last input character read for any purpose.  */
Lisp_Object last_input_char;

/* If not Qnil, a list of objects to be read as subsequent command input.  */
Lisp_Object Vunread_command_events;

/* If not Qnil, a list of objects to be read as subsequent command input
   including input method processing.  */
Lisp_Object Vunread_input_method_events;

/* If not Qnil, a list of objects to be read as subsequent command input
   but NOT including input method processing.  */
Lisp_Object Vunread_post_input_method_events;

/* If not -1, an event to be read as subsequent command input.  */
EMACS_INT unread_command_char;

/* If not Qnil, this is a switch-frame event which we decided to put
   off until the end of a key sequence.  This should be read as the
   next command input, after any unread_command_events.

   read_key_sequence uses this to delay switch-frame events until the
   end of the key sequence; Fread_char uses it to put off switch-frame
   events until a non-ASCII event is acceptable as input.  */
Lisp_Object unread_switch_frame;

/* A mask of extra modifier bits to put into every keyboard char.  */
EMACS_INT extra_keyboard_modifiers;

/* Char to use as prefix when a meta character is typed in.
   This is bound on entry to minibuffer in case ESC is changed there.  */

Lisp_Object meta_prefix_char;

/* Last size recorded for a current buffer which is not a minibuffer.  */
static int last_non_minibuf_size;

/* Number of idle seconds before an auto-save and garbage collection.  */
static Lisp_Object Vauto_save_timeout;

/* Total number of times read_char has returned.  */
int num_input_events;

/* Total number of times read_char has returned, outside of macros.  */
EMACS_INT num_nonmacro_input_events;

/* Auto-save automatically when this many characters have been typed
   since the last time.  */

static EMACS_INT auto_save_interval;

/* Value of num_nonmacro_input_events as of last auto save.  */

int last_auto_save;

/* The command being executed by the command loop.
   Commands may set this, and the value set will be copied into
   current_kboard->Vlast_command instead of the actual command.  */
Lisp_Object Vthis_command;

/* This is like Vthis_command, except that commands never set it.  */
Lisp_Object real_this_command;

/* If the lookup of the command returns a binding, the original
   command is stored in this-original-command.  It is nil otherwise.  */
Lisp_Object Vthis_original_command;

/* The value of point when the last command was executed.  */
int last_point_position;

/* The buffer that was current when the last command was started.  */
Lisp_Object last_point_position_buffer;

/* The frame in which the last input event occurred, or Qmacro if the
   last event came from a macro.  We use this to determine when to
   generate switch-frame events.  This may be cleared by functions
   like Fselect_frame, to make sure that a switch-frame event is
   generated by the next character.  */
Lisp_Object internal_last_event_frame;

/* A user-visible version of the above, intended to allow users to
   figure out where the last event came from, if the event doesn't
   carry that information itself (i.e. if it was a character).  */
Lisp_Object Vlast_event_frame;

/* The timestamp of the last input event we received from the X server.
   X Windows wants this for selection ownership.  */
unsigned long last_event_timestamp;

Lisp_Object Qself_insert_command;
Lisp_Object Qforward_char;
Lisp_Object Qbackward_char;
Lisp_Object Qundefined;
Lisp_Object Qtimer_event_handler;

/* read_key_sequence stores here the command definition of the
   key sequence that it reads.  */
Lisp_Object read_key_sequence_cmd;

/* Echo unfinished commands after this many seconds of pause.  */
Lisp_Object Vecho_keystrokes;

/* Form to evaluate (if non-nil) when Emacs is started.  */
Lisp_Object Vtop_level;

/* User-supplied string to translate input characters through.  */
Lisp_Object Vkeyboard_translate_table;

/* Keymap mapping ASCII function key sequences onto their preferred forms.  */
extern Lisp_Object Vfunction_key_map;

/* Another keymap that maps key sequences into key sequences.
   This one takes precedence over ordinary definitions.  */
extern Lisp_Object Vkey_translation_map;

/* If non-nil, this implements the current input method.  */
Lisp_Object Vinput_method_function;
Lisp_Object Qinput_method_function;

/* When we call Vinput_method_function,
   this holds the echo area message that was just erased.  */
Lisp_Object Vinput_method_previous_message;

/* Non-nil means deactivate the mark at end of this command.  */
Lisp_Object Vdeactivate_mark;

/* Menu bar specified in Lucid Emacs fashion.  */

Lisp_Object Vlucid_menu_bar_dirty_flag;
Lisp_Object Qrecompute_lucid_menubar, Qactivate_menubar_hook;

Lisp_Object Qecho_area_clear_hook;

/* Hooks to run before and after each command.  */
Lisp_Object Qpre_command_hook, Vpre_command_hook;
Lisp_Object Qpost_command_hook, Vpost_command_hook;
Lisp_Object Qcommand_hook_internal, Vcommand_hook_internal;
/* Hook run after a command if there's no more input soon.  */
Lisp_Object Qpost_command_idle_hook, Vpost_command_idle_hook;

/* Delay time in microseconds before running post-command-idle-hook.  */
EMACS_INT post_command_idle_delay;

/* List of deferred actions to be performed at a later time.
   The precise format isn't relevant here; we just check whether it is nil.  */
Lisp_Object Vdeferred_action_list;

/* Function to call to handle deferred actions, when there are any.  */
Lisp_Object Vdeferred_action_function;
Lisp_Object Qdeferred_action_function;

Lisp_Object Qinput_method_exit_on_first_char;
Lisp_Object Qinput_method_use_echo_area;

/* File in which we write all commands we read.  */
FILE *dribble;

/* Nonzero if input is available.  */
int input_pending;

/* 1 if should obey 0200 bit in input chars as "Meta", 2 if should
   keep 0200 bit in input chars.  0 to ignore the 0200 bit.  */

int meta_key;

/* Non-zero means force key bindings update in parse_menu_item.  */

int update_menu_bindings;

extern char *pending_malloc_warning;

/* Circular buffer for pre-read keyboard input.  */

static struct input_event kbd_buffer[KBD_BUFFER_SIZE];

/* Vector to GCPRO the Lisp objects referenced from kbd_buffer.

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

   So, we use this vector to protect the Lisp_Objects in the event
   queue.  That way, they'll be dequeued as dead frames or windows,
   but still valid Lisp objects.

   If kbd_buffer[i].kind != no_event, then

   AREF (kbd_buffer_gcpro, 2 * i) == kbd_buffer[i].frame_or_window.
   AREF (kbd_buffer_gcpro, 2 * i + 1) == kbd_buffer[i].arg.  */

static Lisp_Object kbd_buffer_gcpro;

/* Pointer to next available character in kbd_buffer.
   If kbd_fetch_ptr == kbd_store_ptr, the buffer is empty.
   This may be kbd_buffer + KBD_BUFFER_SIZE, meaning that the
   next available char is in kbd_buffer[0].  */
static struct input_event *kbd_fetch_ptr;

/* Pointer to next place to store character in kbd_buffer.  This
   may be kbd_buffer + KBD_BUFFER_SIZE, meaning that the next
   character should go in kbd_buffer[0].  */
static struct input_event * volatile kbd_store_ptr;

/* The above pair of variables forms a "queue empty" flag.  When we
   enqueue a non-hook event, we increment kbd_store_ptr.  When we
   dequeue a non-hook event, we increment kbd_fetch_ptr.  We say that
   there is input available iff the two pointers are not equal.

   Why not just have a flag set and cleared by the enqueuing and
   dequeuing functions?  Such a flag could be screwed up by interrupts
   at inopportune times.  */

/* If this flag is non-nil, we check mouse_moved to see when the
   mouse moves, and motion events will appear in the input stream.
   Otherwise, mouse motion is ignored.  */
Lisp_Object do_mouse_tracking;

/* Symbols to head events.  */
Lisp_Object Qmouse_movement;
Lisp_Object Qscroll_bar_movement;
Lisp_Object Qswitch_frame;
Lisp_Object Qdelete_frame;
Lisp_Object Qiconify_frame;
Lisp_Object Qmake_frame_visible;
Lisp_Object Qselect_window;
Lisp_Object Qhelp_echo;

/* Symbols to denote kinds of events.  */
Lisp_Object Qfunction_key;
Lisp_Object Qmouse_click;
#ifdef WINDOWSNT
Lisp_Object Qmouse_wheel;
Lisp_Object Qlanguage_change;
#endif
Lisp_Object Qdrag_n_drop;
Lisp_Object Qsave_session;

/* Lisp_Object Qmouse_movement; - also an event header */

/* Properties of event headers.  */
Lisp_Object Qevent_kind;
Lisp_Object Qevent_symbol_elements;

/* menu item parts */
Lisp_Object Qmenu_alias;
Lisp_Object Qmenu_enable;
Lisp_Object QCenable, QCvisible, QChelp, QCfilter, QCkeys, QCkey_sequence;
Lisp_Object QCbutton, QCtoggle, QCradio;
extern Lisp_Object Vdefine_key_rebound_commands;
extern Lisp_Object Qmenu_item;

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
extern Lisp_Object Qleft_margin, Qright_margin;

Lisp_Object recursive_edit_unwind (), command_loop ();
Lisp_Object Fthis_command_keys ();
Lisp_Object Qextended_command_history;
EMACS_TIME timer_check ();

extern Lisp_Object Vhistory_length;

extern char *x_get_keysym_name ();

static void record_menu_key ();

Lisp_Object Qpolling_period;

/* List of absolute timers.  Appears in order of next scheduled event.  */
Lisp_Object Vtimer_list;

/* List of idle time timers.  Appears in order of next scheduled event.  */
Lisp_Object Vtimer_idle_list;

/* Incremented whenever a timer is run.  */
int timers_run;

extern Lisp_Object Vprint_level, Vprint_length;

/* Address (if not 0) of EMACS_TIME to zero out if a SIGIO interrupt
   happens.  */
EMACS_TIME *input_available_clear_time;

/* Nonzero means use SIGIO interrupts; zero means use CBREAK mode.
   Default is 1 if INTERRUPT_INPUT is defined.  */
int interrupt_input;

/* Nonzero while interrupts are temporarily deferred during redisplay.  */
int interrupts_deferred;

/* Nonzero means use ^S/^Q for flow control.  */
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

/* If we support a window system, turn on the code to poll periodically
   to detect C-g.  It isn't actually used when doing interrupt input.  */
#ifdef HAVE_WINDOW_SYSTEM
#define POLL_FOR_INPUT
#endif

/* After a command is executed, if point is moved into a region that
   has specific properties (e.g. composition, display), we adjust
   point to the boundary of the region.  But, if a command sets this
   variable to non-nil, we suppress this point adjustment.  This
   variable is set to nil before reading a command.  */

Lisp_Object Vdisable_point_adjustment;

/* If non-nil, always disable point adjustment.  */

Lisp_Object Vglobal_disable_point_adjustment;

/* The time when Emacs started being idle.  */

static EMACS_TIME timer_idleness_start_time;


/* Global variable declarations.  */

/* Function for init_keyboard to call with no args (if nonzero).  */
void (*keyboard_init_hook) ();

static int read_avail_input P_ ((int));
static void get_input_pending P_ ((int *, int));
static int readable_events P_ ((int));
static Lisp_Object read_char_x_menu_prompt P_ ((int, Lisp_Object *,
						Lisp_Object, int *));
static Lisp_Object read_char_x_menu_prompt ();
static Lisp_Object read_char_minibuf_menu_prompt P_ ((int, int,
						      Lisp_Object *));
static Lisp_Object make_lispy_event P_ ((struct input_event *));
#ifdef HAVE_MOUSE
static Lisp_Object make_lispy_movement P_ ((struct frame *, Lisp_Object,
					    enum scroll_bar_part,
					    Lisp_Object, Lisp_Object,
					    unsigned long));
#endif
static Lisp_Object modify_event_symbol P_ ((int, unsigned, Lisp_Object,
					    Lisp_Object, char **,
					    Lisp_Object *, unsigned));
static Lisp_Object make_lispy_switch_frame P_ ((Lisp_Object));
static int parse_solitary_modifier P_ ((Lisp_Object));
static int parse_solitary_modifier ();
static void save_getcjmp P_ ((jmp_buf));
static void save_getcjmp ();
static void restore_getcjmp P_ ((jmp_buf));
static Lisp_Object apply_modifiers P_ ((int, Lisp_Object));
static void clear_event P_ ((struct input_event *));
static void any_kboard_state P_ ((void));

/* Nonzero means don't try to suspend even if the operating system seems
   to support it.  */
static int cannot_suspend;

/* Install the string STR as the beginning of the string of echoing,
   so that it serves as a prompt for the next character.
   Also start echoing.  */

void
echo_prompt (str)
     Lisp_Object str;
{
  current_kboard->echo_string = str;
  current_kboard->echo_after_prompt = SCHARS (str);
  echo_now ();
}

/* Add C to the echo string, if echoing is going on.
   C can be a character, which is printed prettily ("M-C-x" and all that
   jazz), or a symbol, whose name is printed.  */

void
echo_char (c)
     Lisp_Object c;
{
  if (current_kboard->immediate_echo)
    {
      int size = KEY_DESCRIPTION_SIZE + 100;
      char *buffer = (char *) alloca (size);
      char *ptr = buffer;
      Lisp_Object echo_string;

      echo_string = current_kboard->echo_string;
      
      /* If someone has passed us a composite event, use its head symbol.  */
      c = EVENT_HEAD (c);

      if (INTEGERP (c))
	{
	  ptr = push_key_description (XINT (c), ptr, 1);
	}
      else if (SYMBOLP (c))
	{
	  struct Lisp_String *name = XSTRING (SYMBOL_NAME (c));
	  int nbytes = STRING_BYTES (name);
	  
	  if (size - (ptr - buffer) < nbytes)
	    {
	      int offset = ptr - buffer;
	      size = max (2 * size, size + nbytes);
	      buffer = (char *) alloca (size);
	      ptr = buffer + offset;
	    }

	  ptr += copy_text (name->data, ptr, nbytes,
			    name->size_byte >= 0, 1);
	}

      if ((NILP (echo_string) || SCHARS (echo_string) == 0)
	  && help_char_p (c))
	{
	  const char *text = " (Type ? for further options)";
	  int len = strlen (text);
	  
	  if (size - (ptr - buffer) < len)
	    {
	      int offset = ptr - buffer;
	      size += len;
	      buffer = (char *) alloca (size);
	      ptr = buffer + offset;
	    }

	  bcopy (text, ptr, len);
	  ptr += len;
	}

      /* Replace a dash from echo_dash with a space, otherwise
	 add a space at the end as a separator between keys.  */
      if (STRINGP (echo_string)
	  && SCHARS (echo_string) > 0)
	{
	  Lisp_Object last_char, idx;

	  idx = make_number (SCHARS (echo_string) - 1);
	  last_char = Faref (echo_string, idx);

	  if (XINT (last_char) == '-')
	    Faset (echo_string, idx, make_number (' '));
	  else
	    echo_string = concat2 (echo_string, build_string (" "));
	}

      current_kboard->echo_string
	= concat2 (echo_string, make_string (buffer, ptr - buffer));

      echo_now ();
    }
}

/* Temporarily add a dash to the end of the echo string if it's not
   empty, so that it serves as a mini-prompt for the very next character.  */

void
echo_dash ()
{
  /* Do nothing if not echoing at all.  */
  if (NILP (current_kboard->echo_string))
    return;

  if (!current_kboard->immediate_echo
      && SCHARS (current_kboard->echo_string) == 0)
    return;
      
  /* Do nothing if we just printed a prompt.  */
  if (current_kboard->echo_after_prompt
      == SCHARS (current_kboard->echo_string))
    return;
      
  /* Put a dash at the end of the buffer temporarily,
     but make it go away when the next character is added.  */
  current_kboard->echo_string = concat2 (current_kboard->echo_string,
					 build_string ("-"));
  echo_now ();
}

/* Display the current echo string, and begin echoing if not already
   doing so.  */

void
echo_now ()
{
  if (!current_kboard->immediate_echo)
    {
      int i;
      current_kboard->immediate_echo = 1;

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
  message3_nolog (current_kboard->echo_string,
		  SBYTES (current_kboard->echo_string),
		  SMBP (current_kboard->echo_string));
  echoing = 0;

  /* Record in what buffer we echoed, and from which kboard.  */
  echo_message_buffer = echo_area_buffer[0];
  echo_kboard = current_kboard;

  if (waiting_for_input && !NILP (Vquit_flag))
    quit_throw_to_read_char ();
}

/* Turn off echoing, for the start of a new command.  */

void
cancel_echoing ()
{
  current_kboard->immediate_echo = 0;
  current_kboard->echo_after_prompt = -1;
  current_kboard->echo_string = Qnil;
  ok_to_echo_at_next_pause = NULL;
  echo_kboard = NULL;
  echo_message_buffer = Qnil;
}

/* Return the length of the current echo string.  */

static int
echo_length ()
{
  return (STRINGP (current_kboard->echo_string)
	  ? SCHARS (current_kboard->echo_string)
	  : 0);
}

/* Truncate the current echo message to its first LEN chars.
   This and echo_char get used by read_key_sequence when the user
   switches frames while entering a key sequence.  */

static void
echo_truncate (nchars)
     int nchars;
{
  if (STRINGP (current_kboard->echo_string))
    current_kboard->echo_string
      = Fsubstring (current_kboard->echo_string,
		    make_number (0), make_number (nchars));
  truncate_echo_area (nchars);
}


/* Functions for manipulating this_command_keys.  */
static void
add_command_key (key)
     Lisp_Object key;
{
  /* If reset-this-command-length was called recently, obey it now.
     See the doc string of that function for an explanation of why.  */
  if (before_command_restore_flag)
    {
      this_command_key_count = before_command_key_count_1;
      if (this_command_key_count < this_single_command_key_start)
	this_single_command_key_start = this_command_key_count;
      echo_truncate (before_command_echo_length_1);
      before_command_restore_flag = 0;
    }

  if (this_command_key_count >= ASIZE (this_command_keys))
    this_command_keys = larger_vector (this_command_keys,
				       2 * ASIZE (this_command_keys),
				       Qnil);

  AREF (this_command_keys, this_command_key_count) = key;
  ++this_command_key_count;
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

#ifdef HAVE_X_WINDOWS
  /* The command loop has started an hourglass timer, so we have to
     cancel it here, otherwise it will fire because the recursive edit
     can take some time.  Do not check for display_hourglass_p here,
     because it could already be nil.  */
    cancel_hourglass ();
#endif

  /* This function may have been called from a debugger called from
     within redisplay, for instance by Edebugging a function called
     from fontification-functions.  We want to allow redisplay in
     the debugging session.

     The recursive edit is left with a `(throw exit ...)'.  The `exit'
     tag is not caught anywhere in redisplay, i.e. when we leave the
     recursive edit, the original redisplay leading to the recursive
     edit will be unwound.  The outcome should therefore be safe.  */
  specbind (Qinhibit_redisplay, Qnil);
  redisplaying_p = 0;

  val = command_loop ();
  if (EQ (val, Qt))
    Fsignal (Qquit, Qnil);
  /* Handle throw from read_minibuf when using minibuffer
     while it's active but we're in another window.  */
  if (STRINGP (val))
    Fsignal (Qerror, Fcons (val, Qnil));

  return unbind_to (count, Qnil);
}

/* When an auto-save happens, record the "time", and don't do again soon.  */

void
record_auto_save ()
{
  last_auto_save = num_nonmacro_input_events;
}

/* Make an auto save happen as soon as possible at command level.  */

void
force_auto_save_soon ()
{
  last_auto_save = - auto_save_interval - 1;

  record_asynch_buffer_change ();
}

DEFUN ("recursive-edit", Frecursive_edit, Srecursive_edit, 0, 0, "",
       doc: /* Invoke the editor command loop recursively.
To get out of the recursive edit, a command can do `(throw 'exit nil)';
that tells this function to return.
Alternately, `(throw 'exit t)' makes this function signal an error.
This function is called by the editor initialization to begin editing.  */)
     ()
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object buffer;

  command_loop_level++;
  update_mode_lines = 1;

  if (command_loop_level
      && current_buffer != XBUFFER (XWINDOW (selected_window)->buffer))
    buffer = Fcurrent_buffer ();
  else
    buffer = Qnil;

  /* If we leave recursive_edit_1 below with a `throw' for instance,
     like it is done in the splash screen display, we have to
     make sure that we restore single_kboard as command_loop_1
     would have done if it were left normally.  */
  record_unwind_protect (recursive_edit_unwind,
			 Fcons (buffer, single_kboard ? Qt : Qnil));

  recursive_edit_1 ();
  return unbind_to (count, Qnil);
}

Lisp_Object
recursive_edit_unwind (info)
     Lisp_Object info;
{
  if (BUFFERP (XCAR (info)))
    Fset_buffer (XCAR (info));
  
  if (NILP (XCDR (info)))
    any_kboard_state ();
  else
    single_kboard_state ();
      
  command_loop_level--;
  update_mode_lines = 1;
  return Qnil;
}


static void
any_kboard_state ()
{
#ifdef MULTI_KBOARD
#if 0 /* Theory: if there's anything in Vunread_command_events,
	 it will right away be read by read_key_sequence,
	 and then if we do switch KBOARDS, it will go into the side
	 queue then.  So we don't need to do anything special here -- rms.  */
  if (CONSP (Vunread_command_events))
    {
      current_kboard->kbd_queue
	= nconc2 (Vunread_command_events, current_kboard->kbd_queue);
      current_kboard->kbd_queue_has_data = 1;
    }
  Vunread_command_events = Qnil;
#endif
  single_kboard = 0;
#endif
}

/* Switch to the single-kboard state, making current_kboard
   the only KBOARD from which further input is accepted.  */

void
single_kboard_state ()
{
#ifdef MULTI_KBOARD
  single_kboard = 1;
#endif
}

/* Maintain a stack of kboards, so other parts of Emacs
   can switch temporarily to the kboard of a given frame
   and then revert to the previous status.  */

struct kboard_stack
{
  KBOARD *kboard;
  struct kboard_stack *next;
};

static struct kboard_stack *kboard_stack;

void
push_frame_kboard (f)
     FRAME_PTR f;
{
#ifdef MULTI_KBOARD
  struct kboard_stack *p
    = (struct kboard_stack *) xmalloc (sizeof (struct kboard_stack));

  p->next = kboard_stack;
  p->kboard = current_kboard;
  kboard_stack = p;

  current_kboard = FRAME_KBOARD (f);
#endif
}

void
pop_frame_kboard ()
{
#ifdef MULTI_KBOARD
  struct kboard_stack *p = kboard_stack;
  current_kboard = p->kboard;
  kboard_stack = p->next;
  xfree (p);
#endif
}

/* Handle errors that are not handled at inner levels
   by printing an error message and returning to the editor command loop.  */

Lisp_Object
cmd_error (data)
     Lisp_Object data;
{
  Lisp_Object old_level, old_length;
  char macroerror[50];

  if (!NILP (executing_macro))
    {
      if (executing_macro_iterations == 1)
	sprintf (macroerror, "After 1 kbd macro iteration: ");
      else
	sprintf (macroerror, "After %d kbd macro iterations: ",
		 executing_macro_iterations);
    }
  else
    *macroerror = 0;

  Vstandard_output = Qt;
  Vstandard_input = Qt;
  Vexecuting_macro = Qnil;
  executing_macro = Qnil;
  current_kboard->Vprefix_arg = Qnil;
  current_kboard->Vlast_prefix_arg = Qnil;
  cancel_echoing ();

  /* Avoid unquittable loop if data contains a circular list.  */
  old_level = Vprint_level;
  old_length = Vprint_length;
  XSETFASTINT (Vprint_level, 10);
  XSETFASTINT (Vprint_length, 10);
  cmd_error_internal (data, macroerror);
  Vprint_level = old_level;
  Vprint_length = old_length;

  Vquit_flag = Qnil;

  Vinhibit_quit = Qnil;
#ifdef MULTI_KBOARD
  any_kboard_state ();
#endif

  return make_number (0);
}

/* Take actions on handling an error.  DATA is the data that describes
   the error.

   CONTEXT is a C-string containing ASCII characters only which
   describes the context in which the error happened.  If we need to
   generalize CONTEXT to allow multibyte characters, make it a Lisp
   string.  */

void
cmd_error_internal (data, context)
     Lisp_Object data;
     char *context;
{
  Lisp_Object stream;
  int kill_emacs_p = 0;
  struct frame *sf = SELECTED_FRAME ();

  Vquit_flag = Qnil;
  Vinhibit_quit = Qt;
  clear_message (1, 0);

  /* If the window system or terminal frame hasn't been initialized
     yet, or we're not interactive, it's best to dump this message out
     to stderr and exit.  */
  if (!sf->glyphs_initialized_p
      /* This is the case of the frame dumped with Emacs, when we're
	 running under a window system.  */
      || (!NILP (Vwindow_system)
	  && !inhibit_window_system
	  && FRAME_TERMCAP_P (sf))
      || noninteractive)
    {
      stream = Qexternal_debugging_output;
      kill_emacs_p = 1;
    }
  else
    {
      Fdiscard_input ();
      bitch_at_user ();
      stream = Qt;
    }

  if (context != 0)
    write_string_1 (context, -1, stream);

  print_error_message (data, stream);

  /* If the window system or terminal frame hasn't been initialized
     yet, or we're in -batch mode, this error should cause Emacs to exit.  */
  if (kill_emacs_p)
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
      Lisp_Object val;
      val = internal_catch (Qexit, command_loop_2, Qnil);
      executing_macro = Qnil;
      return val;
    }
  else
    while (1)
      {
	internal_catch (Qtop_level, top_level_1, Qnil);
	internal_catch (Qtop_level, command_loop_2, Qnil);
	executing_macro = Qnil;

	/* End of file in -batch run causes exit here.  */
	if (noninteractive)
	  Fkill_emacs (Qt);
      }
}

/* Here we catch errors in execution of commands within the
   editing loop, and reenter the editing loop.
   When there is an error, cmd_error runs and returns a non-nil
   value to us.  A value of nil means that command_loop_1 itself
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
       doc: /* Exit all recursive editing levels.  */)
     ()
{
#ifdef HAVE_X_WINDOWS
  if (display_hourglass_p)
    cancel_hourglass ();
#endif
  return Fthrow (Qtop_level, Qnil);
}

DEFUN ("exit-recursive-edit", Fexit_recursive_edit, Sexit_recursive_edit, 0, 0, "",
       doc: /* Exit from the innermost recursive edit or minibuffer.  */)
     ()
{
  if (command_loop_level > 0 || minibuf_level > 0)
    Fthrow (Qexit, Qnil);

  error ("No recursive edit is in progress");
  return Qnil;
}

DEFUN ("abort-recursive-edit", Fabort_recursive_edit, Sabort_recursive_edit, 0, 0, "",
       doc: /* Abort the command that requested this recursive edit or minibuffer input.  */)
     ()
{
  if (command_loop_level > 0 || minibuf_level > 0)
    Fthrow (Qexit, Qt);

  error ("No recursive edit is in progress");
  return Qnil;
}

/* This is the actual command reading loop,
   sans error-handling encapsulation.  */

static int read_key_sequence P_ ((Lisp_Object *, int, Lisp_Object,
				  int, int, int));
void safe_run_hooks P_ ((Lisp_Object));
static void adjust_point_for_property P_ ((int));

Lisp_Object
command_loop_1 ()
{
  Lisp_Object cmd;
  int lose;
  int nonundocount;
  Lisp_Object keybuf[30];
  int i;
  int no_direct;
  int prev_modiff;
  struct buffer *prev_buffer = NULL;
#ifdef MULTI_KBOARD
  int was_locked = single_kboard;
#endif

  current_kboard->Vprefix_arg = Qnil;
  current_kboard->Vlast_prefix_arg = Qnil;
  Vdeactivate_mark = Qnil;
  waiting_for_input = 0;
  cancel_echoing ();

  nonundocount = 0;
  this_command_key_count = 0;
  this_single_command_key_start = 0;

  /* Make sure this hook runs after commands that get errors and
     throw to top level.  */
  /* Note that the value cell will never directly contain nil
     if the symbol is a local variable.  */
  if (!NILP (Vpost_command_hook) && !NILP (Vrun_hooks))
    safe_run_hooks (Qpost_command_hook);

  /* If displaying a message, resize the echo area window to fit
     that message's size exactly.  */
  if (!NILP (echo_area_buffer[0]))
    resize_echo_area_exactly ();

  if (!NILP (Vdeferred_action_list))
    call0 (Vdeferred_action_function);

  if (!NILP (Vpost_command_idle_hook) && !NILP (Vrun_hooks))
    {
      if (NILP (Vunread_command_events)
	  && NILP (Vunread_input_method_events)
	  && NILP (Vunread_post_input_method_events)
	  && NILP (Vexecuting_macro)
	  && !NILP (sit_for (0, post_command_idle_delay, 0, 1, 1)))
	safe_run_hooks (Qpost_command_idle_hook);
    }

  /* Do this after running Vpost_command_hook, for consistency.  */
  current_kboard->Vlast_command = Vthis_command;
  current_kboard->Vreal_last_command = real_this_command;

  while (1)
    {
      if (! FRAME_LIVE_P (XFRAME (selected_frame)))
	Fkill_emacs (Qnil);

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
	 wait a short time and redraw minibuffer.  */

      if (minibuf_level
	  && !NILP (echo_area_buffer[0])
	  && EQ (minibuf_window, echo_area_window)
	  && NUMBERP (Vminibuffer_message_timeout))
	{
	  /* Bind inhibit-quit to t so that C-g gets read in
	     rather than quitting back to the minibuffer.  */
	  int count = specpdl_ptr - specpdl;
	  specbind (Qinhibit_quit, Qt);

	  Fsit_for (Vminibuffer_message_timeout, Qnil, Qnil);
	  /* Clear the echo area.  */
	  message2 (0, 0, 0);
	  safe_run_hooks (Qecho_area_clear_hook);

	  unbind_to (count, Qnil);

	  /* If a C-g came in before, treat it as input now.  */
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
      /* Select the frame that the last event came from.  Usually,
	 switch-frame events will take care of this, but if some lisp
	 code swallows a switch-frame event, we'll fix things up here.
	 Is this a good idea?  */
      if (FRAMEP (internal_last_event_frame)
	  && !EQ (internal_last_event_frame, selected_frame))
	Fselect_frame (internal_last_event_frame, Qnil);
#endif
      /* If it has changed current-menubar from previous value,
	 really recompute the menubar from the value.  */
      if (! NILP (Vlucid_menu_bar_dirty_flag)
	  && !NILP (Ffboundp (Qrecompute_lucid_menubar)))
	call0 (Qrecompute_lucid_menubar);

      before_command_key_count = this_command_key_count;
      before_command_echo_length = echo_length ();

      Vthis_command = Qnil;
      real_this_command = Qnil;

      /* Read next key sequence; i gets its length.  */
      i = read_key_sequence (keybuf, sizeof keybuf / sizeof keybuf[0],
			     Qnil, 0, 1, 1);

      /* A filter may have run while we were reading the input.  */
      if (! FRAME_LIVE_P (XFRAME (selected_frame)))
	Fkill_emacs (Qnil);
      if (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer)
	set_buffer_internal (XBUFFER (XWINDOW (selected_window)->buffer));

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
	  this_single_command_key_start = 0;
	  goto finalize;
	}

      last_command_char = keybuf[i - 1];

      /* If the previous command tried to force a specific window-start,
	 forget about that, in case this command moves point far away
	 from that position.  But also throw away beg_unchanged and
	 end_unchanged information in that case, so that redisplay will
	 update the whole window properly.  */
      if (!NILP (XWINDOW (selected_window)->force_start))
	{
	  struct buffer *b;
	  XWINDOW (selected_window)->force_start = Qnil;
	  b = XBUFFER (XWINDOW (selected_window)->buffer);
	  BUF_BEG_UNCHANGED (b) = BUF_END_UNCHANGED (b) = 0;
	}

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
	 cases identified below.  */
      prev_buffer = current_buffer;
      prev_modiff = MODIFF;
      last_point_position = PT;
      XSETBUFFER (last_point_position_buffer, prev_buffer);

      /* By default, we adjust point to a boundary of a region that
         has such a property that should be treated intangible
         (e.g. composition, display).  But, some commands will set
         this variable differently.  */
      Vdisable_point_adjustment = Qnil;

      /* Process filters and timers may have messed with deactivate-mark.
	 reset it before we execute the command. */
      Vdeactivate_mark = Qnil;

      /* Remap command through active keymaps */
      Vthis_original_command = cmd;
      if (SYMBOLP (cmd))
	{
	  Lisp_Object cmd1;
	  if (cmd1 = Fremap_command (cmd), !NILP (cmd1))
	    cmd = cmd1;
	}

      /* Execute the command.  */

      Vthis_command = cmd;
      real_this_command = cmd;
      /* Note that the value cell will never directly contain nil
	 if the symbol is a local variable.  */
      if (!NILP (Vpre_command_hook) && !NILP (Vrun_hooks))
	safe_run_hooks (Qpre_command_hook);
      
      if (NILP (Vthis_command))
	{
	  /* nil means key is undefined.  */
	  bitch_at_user ();
	  current_kboard->defining_kbd_macro = Qnil;
	  update_mode_lines = 1;
	  current_kboard->Vprefix_arg = Qnil;
	}
      else
	{
	  if (NILP (current_kboard->Vprefix_arg) && ! no_direct)
	    {
	      /* In case we jump to directly_done.  */
	      Vcurrent_prefix_arg = current_kboard->Vprefix_arg;

	      /* Recognize some common commands in common situations and
		 do them directly.  */
	      if (EQ (Vthis_command, Qforward_char) && PT < ZV)
		{
                  struct Lisp_Char_Table *dp
		    = window_display_table (XWINDOW (selected_window));
		  lose = FETCH_CHAR (PT_BYTE);
		  SET_PT (PT + 1);
		  if ((dp
		       ? (VECTORP (DISP_CHAR_VECTOR (dp, lose))
			  ? XVECTOR (DISP_CHAR_VECTOR (dp, lose))->size == 1
                          : (NILP (DISP_CHAR_VECTOR (dp, lose))
                             && (lose >= 0x20 && lose < 0x7f)))
		       : (lose >= 0x20 && lose < 0x7f))
		      /* To extract the case of continuation on
                         wide-column characters.  */
		      && (WIDTH_BY_CHAR_HEAD (FETCH_BYTE (PT_BYTE)) == 1)
		      && (XFASTINT (XWINDOW (selected_window)->last_modified)
			  >= MODIFF)
		      && (XFASTINT (XWINDOW (selected_window)->last_overlay_modified)
			  >= OVERLAY_MODIFF)
		      && (XFASTINT (XWINDOW (selected_window)->last_point)
			  == PT - 1)
		      && !windows_or_buffers_changed
		      && EQ (current_buffer->selective_display, Qnil)
		      && !detect_input_pending ()
		      && NILP (XWINDOW (selected_window)->column_number_displayed)
		      && NILP (Vexecuting_macro))
		    direct_output_forward_char (1);
		  goto directly_done;
		}
	      else if (EQ (Vthis_command, Qbackward_char) && PT > BEGV)
		{
                  struct Lisp_Char_Table *dp
		    = window_display_table (XWINDOW (selected_window));
		  SET_PT (PT - 1);
		  lose = FETCH_CHAR (PT_BYTE);
		  if ((dp
		       ? (VECTORP (DISP_CHAR_VECTOR (dp, lose))
			  ? XVECTOR (DISP_CHAR_VECTOR (dp, lose))->size == 1
                          : (NILP (DISP_CHAR_VECTOR (dp, lose))
                             && (lose >= 0x20 && lose < 0x7f)))
		       : (lose >= 0x20 && lose < 0x7f))
		      && (XFASTINT (XWINDOW (selected_window)->last_modified)
			  >= MODIFF)
		      && (XFASTINT (XWINDOW (selected_window)->last_overlay_modified)
			  >= OVERLAY_MODIFF)
		      && (XFASTINT (XWINDOW (selected_window)->last_point)
			  == PT + 1)
		      && !windows_or_buffers_changed
		      && EQ (current_buffer->selective_display, Qnil)
		      && !detect_input_pending ()
		      && NILP (XWINDOW (selected_window)->column_number_displayed)
		      && NILP (Vexecuting_macro))
		    direct_output_forward_char (-1);
		  goto directly_done;
		}
	      else if (EQ (Vthis_command, Qself_insert_command)
		       /* Try this optimization only on ascii keystrokes.  */
		       && INTEGERP (last_command_char))
		{
		  unsigned int c = XINT (last_command_char);
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
			  || (XFASTINT (XWINDOW (selected_window)->last_overlay_modified)
			      < OVERLAY_MODIFF)
			  || (XFASTINT (XWINDOW (selected_window)->last_point)
			      != PT)
			  || MODIFF <= SAVE_MODIFF
			  || windows_or_buffers_changed
			  || !EQ (current_buffer->selective_display, Qnil)
			  || detect_input_pending ()
			  || !NILP (XWINDOW (selected_window)->column_number_displayed)
			  || !NILP (Vexecuting_macro));
		  
		  value = internal_self_insert (c, 0);

		  if (value == 2)
		    nonundocount = 0;

		  /* VALUE == 1 when AFTER-CHANGE functions are
		     installed which is the case most of the time
		     because FONT-LOCK installs one.  */
		  if (!lose && !value)
		    direct_output_for_insert (c);
		  goto directly_done;
		}
	    }

	  /* Here for a command that isn't executed directly */

#ifdef HAVE_X_WINDOWS
	  if (display_hourglass_p
	      && NILP (Vexecuting_macro))
	    start_hourglass ();
#endif

	  nonundocount = 0;
	  if (NILP (current_kboard->Vprefix_arg))
	    Fundo_boundary ();
	  Fcommand_execute (Vthis_command, Qnil, Qnil, Qnil);

#ifdef HAVE_X_WINDOWS
	  /* Do not check display_hourglass_p here, because
	     Fcommand_execute could change it, but we should cancel
	     hourglass cursor anyway.
	     But don't cancel the hourglass within a macro
	     just because a command in the macro finishes.  */
	  if (NILP (Vexecuting_macro))
	    cancel_hourglass ();
#endif
	}
    directly_done: ;
      current_kboard->Vlast_prefix_arg = Vcurrent_prefix_arg;

      /* Note that the value cell will never directly contain nil
	 if the symbol is a local variable.  */
      if (!NILP (Vpost_command_hook) && !NILP (Vrun_hooks))
	safe_run_hooks (Qpost_command_hook);

      /* If displaying a message, resize the echo area window to fit
	 that message's size exactly.  */
      if (!NILP (echo_area_buffer[0]))
	resize_echo_area_exactly ();

      if (!NILP (Vdeferred_action_list))
	safe_run_hooks (Qdeferred_action_function);

      if (!NILP (Vpost_command_idle_hook) && !NILP (Vrun_hooks))
	{
	  if (NILP (Vunread_command_events)
	      && NILP (Vunread_input_method_events)
	      && NILP (Vunread_post_input_method_events)
	      && NILP (Vexecuting_macro)
	      && !NILP (sit_for (0, post_command_idle_delay, 0, 1, 1)))
	    safe_run_hooks (Qpost_command_idle_hook);
	}

      /* If there is a prefix argument,
	 1) We don't want Vlast_command to be ``universal-argument''
	 (that would be dumb), so don't set Vlast_command,
	 2) we want to leave echoing on so that the prefix will be
	 echoed as part of this key sequence, so don't call
	 cancel_echoing, and
	 3) we want to leave this_command_key_count non-zero, so that
	 read_char will realize that it is re-reading a character, and
	 not echo it a second time.

	 If the command didn't actually create a prefix arg,
	 but is merely a frame event that is transparent to prefix args,
	 then the above doesn't apply.  */
      if (NILP (current_kboard->Vprefix_arg) || CONSP (last_command_char))
	{
	  current_kboard->Vlast_command = Vthis_command;
	  current_kboard->Vreal_last_command = real_this_command;
	  cancel_echoing ();
	  this_command_key_count = 0;
	  this_single_command_key_start = 0;
	}

      if (!NILP (current_buffer->mark_active) && !NILP (Vrun_hooks))
	{
	  if (!NILP (Vdeactivate_mark) && !NILP (Vtransient_mark_mode))
	    {
	      /* We could also call `deactivate'mark'.  */
	      if (EQ (Vtransient_mark_mode, Qlambda))
		Vtransient_mark_mode = Qnil;
	      else
		{
		  current_buffer->mark_active = Qnil;
		  call1 (Vrun_hooks, intern ("deactivate-mark-hook"));
		}
	    }
	  else if (current_buffer != prev_buffer || MODIFF != prev_modiff)
	    call1 (Vrun_hooks, intern ("activate-mark-hook"));
	}

    finalize:

      if (current_buffer == prev_buffer
	  && last_point_position != PT
	  && NILP (Vdisable_point_adjustment)
	  && NILP (Vglobal_disable_point_adjustment))
	adjust_point_for_property (last_point_position);

      /* Install chars successfully executed in kbd macro.  */

      if (!NILP (current_kboard->defining_kbd_macro)
	  && NILP (current_kboard->Vprefix_arg))
	finalize_kbd_macro_chars ();

#ifdef MULTI_KBOARD
      if (!was_locked)
	any_kboard_state ();
#endif
    }
}

extern Lisp_Object Qcomposition, Qdisplay;

/* Adjust point to a boundary of a region that has such a property
   that should be treated intangible.  For the moment, we check
   `composition' and `display' property.  LAST_PT is the last position
   of point.  */

static void
adjust_point_for_property (last_pt)
     int last_pt;
{
  int start, end;
  Lisp_Object val;
  int check_composition = 1, check_display = 1;

  while (check_composition || check_display)
    {
      if (check_composition
	  && PT > BEGV && PT < ZV
	  && get_property_and_range (PT, Qcomposition, &val, &start, &end, Qnil)
	  && COMPOSITION_VALID_P (start, end, val)
	  && start < PT && end > PT
	  && (last_pt <= start || last_pt >= end))
	{
	  if (PT < last_pt)
	    SET_PT (start);
	  else
	    SET_PT (end);
	  check_display = 1;
	}
      check_composition = 0;
      if (check_display
	  && PT > BEGV && PT < ZV
	  && get_property_and_range (PT, Qdisplay, &val, &start, &end, Qnil)
	  && display_prop_intangible_p (val)
	  && start < PT && end > PT
	  && (last_pt <= start || last_pt >= end))
	{
	  if (PT < last_pt)
	    SET_PT (start);
	  else
	    SET_PT (end);
	  check_composition = 1;
	}
      check_display = 0;
    }
}

/* Subroutine for safe_run_hooks: run the hook HOOK.  */

static Lisp_Object
safe_run_hooks_1 (hook)
     Lisp_Object hook;
{
  return call1 (Vrun_hooks, Vinhibit_quit);
}

/* Subroutine for safe_run_hooks: handle an error by clearing out the hook.  */

static Lisp_Object
safe_run_hooks_error (data)
     Lisp_Object data;
{
  return Fset (Vinhibit_quit, Qnil);
}

/* If we get an error while running the hook, cause the hook variable
   to be nil.  Also inhibit quits, so that C-g won't cause the hook
   to mysteriously evaporate.  */

void
safe_run_hooks (hook)
     Lisp_Object hook;
{
  int count = specpdl_ptr - specpdl;
  specbind (Qinhibit_quit, hook);

  internal_condition_case (safe_run_hooks_1, Qt, safe_run_hooks_error);

  unbind_to (count, Qnil);
}


/* Number of seconds between polling for input.  This is a Lisp
   variable that can be bound.  */

EMACS_INT polling_period;

/* Nonzero means polling for input is temporarily suppressed.  */

int poll_suppress_count;

/* Asynchronous timer for polling.  */

struct atimer *poll_timer;


#ifdef POLL_FOR_INPUT

/* Poll for input, so what we catch a C-g if it comes in.  This
   function is called from x_make_frame_visible, see comment
   there.  */

void
poll_for_input_1 ()
{
  if (interrupt_input_blocked == 0
      && !waiting_for_input)
    read_avail_input (0);
}

/* Timer callback function for poll_timer.  TIMER is equal to
   poll_timer.  */

void
poll_for_input (timer)
     struct atimer *timer;
{
  if (poll_suppress_count == 0)
    poll_for_input_1 ();
}

#endif /* POLL_FOR_INPUT */

/* Begin signals to poll for input, if they are appropriate.
   This function is called unconditionally from various places.  */

void
start_polling ()
{
#ifdef POLL_FOR_INPUT
  if (read_socket_hook && !interrupt_input)
    {
      /* Turn alarm handling on unconditionally.  It might have
	 been turned off in process.c.  */
      turn_on_atimers (1);
      
      /* If poll timer doesn't exist, are we need one with
	 a different interval, start a new one.  */
      if (poll_timer == NULL
	  || EMACS_SECS (poll_timer->interval) != polling_period)
	{
	  EMACS_TIME interval;

	  if (poll_timer)
	    cancel_atimer (poll_timer);
      
	  EMACS_SET_SECS_USECS (interval, polling_period, 0);
	  poll_timer = start_atimer (ATIMER_CONTINUOUS, interval,
				     poll_for_input, NULL);
	}

      /* Let the timer's callback function poll for input
	 if this becomes zero.  */
      --poll_suppress_count;
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

void
stop_polling ()
{
#ifdef POLL_FOR_INPUT
  if (read_socket_hook && !interrupt_input)
    ++poll_suppress_count;
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

void
bind_polling_period (n)
     int n;
{
#ifdef POLL_FOR_INPUT
  int new = polling_period;

  if (n > new)
    new = n;

  stop_other_atimers (poll_timer);
  stop_polling ();
  specbind (Qpolling_period, make_number (new));
  /* Start a new alarm with the new period.  */
  start_polling ();
#endif
}

/* Apply the control modifier to CHARACTER.  */

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

/* Display help echo in the echo area.

   HELP a string means display that string, HELP nil means clear the
   help echo.  If HELP is a function, call it with OBJECT and POS as
   arguments; the function should return a help string or nil for
   none.  For all other types of HELP evaluate it to obtain a string.

   WINDOW is the window in which the help was generated, if any.
   It is nil if not in a window.

   If OBJECT is a buffer, POS is the position in the buffer where the
   `help-echo' text property was found.

   If OBJECT is an overlay, that overlay has a `help-echo' property,
   and POS is the position in the overlay's buffer under the mouse.

   If OBJECT is a string (an overlay string or a string displayed with
   the `display' property).  POS is the position in that string under
   the mouse.

   OK_TO_OVERWRITE_KEYSTROKE_ECHO non-zero means it's okay if the help
   echo overwrites a keystroke echo currently displayed in the echo
   area.

   Note: this function may only be called with HELP nil or a string
   from X code running asynchronously.  */

void
show_help_echo (help, window, object, pos, ok_to_overwrite_keystroke_echo)
     Lisp_Object help, window, object, pos;
     int ok_to_overwrite_keystroke_echo;
{
  if (!NILP (help) && !STRINGP (help))
    {
      if (FUNCTIONP (help))
	{
	  Lisp_Object args[4];
	  args[0] = help;
	  args[1] = window;
	  args[2] = object;
	  args[3] = pos;
	  help = safe_call (4, args);
	}
      else
	help = safe_eval (help);
      
      if (!STRINGP (help))
	return;
    }

  if (STRINGP (help) || NILP (help))
    {
      if (!NILP (Vshow_help_function))
	call1 (Vshow_help_function, help);
      else if (/* Don't overwrite minibuffer contents.  */
	       !MINI_WINDOW_P (XWINDOW (selected_window))
	       /* Don't overwrite a keystroke echo.  */
	       && (NILP (echo_message_buffer)
		   || ok_to_overwrite_keystroke_echo)
	       /* Don't overwrite a prompt.  */
	       && !cursor_in_echo_area)
	{
	  if (STRINGP (help))
	    {
	      int count = BINDING_STACK_SIZE ();

	      if (!help_echo_showing_p)
		Vpre_help_message = current_message ();
	      
	      specbind (Qmessage_truncate_lines, Qt);
	      message3_nolog (help, STRING_BYTES (XSTRING (help)),
			      STRING_MULTIBYTE (help));
	      unbind_to (count, Qnil);
	    }
	  else if (STRINGP (Vpre_help_message))
	    {
	      message3_nolog (Vpre_help_message,
			      STRING_BYTES (XSTRING (Vpre_help_message)),
			      STRING_MULTIBYTE (Vpre_help_message));
	      Vpre_help_message = Qnil;
	    }
	  else
	    message (0);
	}
      
      help_echo_showing_p = STRINGP (help);
    }
}



/* Input of single characters from keyboard */

Lisp_Object print_help ();
static Lisp_Object kbd_buffer_get_event ();
static void record_char ();

#ifdef MULTI_KBOARD
static jmp_buf wrong_kboard_jmpbuf;
#endif

/* read a character from the keyboard; call the redisplay if needed */
/* commandflag 0 means do not do auto-saving, but do do redisplay.
   -1 means do not do redisplay, but do do autosaving.
   1 means do both.  */

/* The arguments MAPS and NMAPS are for menu prompting.
   MAPS is an array of keymaps;  NMAPS is the length of MAPS.

   PREV_EVENT is the previous input event, or nil if we are reading
   the first event of a key sequence (or not reading a key sequence).
   If PREV_EVENT is t, that is a "magic" value that says
   not to run input methods, but in other respects to act as if
   not reading a key sequence.

   If USED_MOUSE_MENU is non-null, then we set *USED_MOUSE_MENU to 1
   if we used a mouse menu to read the input, or zero otherwise.  If
   USED_MOUSE_MENU is null, we don't dereference it.

   Value is t if we showed a menu and the user rejected it.  */

Lisp_Object
read_char (commandflag, nmaps, maps, prev_event, used_mouse_menu)
     int commandflag;
     int nmaps;
     Lisp_Object *maps;
     Lisp_Object prev_event;
     int *used_mouse_menu;
{
  volatile Lisp_Object c;
  int count;
  jmp_buf local_getcjmp;
  jmp_buf save_jump;
  volatile int key_already_recorded = 0;
  Lisp_Object tem, save;
  volatile Lisp_Object previous_echo_area_message;
  volatile Lisp_Object also_record;
  volatile int reread;
  struct gcpro gcpro1, gcpro2;
  EMACS_TIME last_idle_start;

  also_record = Qnil;

  before_command_key_count = this_command_key_count;
  before_command_echo_length = echo_length ();
  c = Qnil;
  previous_echo_area_message = Qnil;

  GCPRO2 (c, previous_echo_area_message);

 retry:

  reread = 0;
  if (CONSP (Vunread_post_input_method_events))
    {
      c = XCAR (Vunread_post_input_method_events);
      Vunread_post_input_method_events
	= XCDR (Vunread_post_input_method_events);

      /* Undo what read_char_x_menu_prompt did when it unread
	 additional keys returned by Fx_popup_menu.  */
      if (CONSP (c)
	  && (SYMBOLP (XCAR (c)) || INTEGERP (XCAR (c)))
	  && NILP (XCDR (c)))
	c = XCAR (c);

      reread = 1;
      goto reread_first;
    }

  if (unread_command_char != -1)
    {
      XSETINT (c, unread_command_char);
      unread_command_char = -1;

      reread = 1;
      goto reread_first;
    }

  if (CONSP (Vunread_command_events))
    {
      c = XCAR (Vunread_command_events);
      Vunread_command_events = XCDR (Vunread_command_events);

      /* Undo what read_char_x_menu_prompt did when it unread
	 additional keys returned by Fx_popup_menu.  */
      if (CONSP (c)
	  && EQ (XCDR (c), Qdisabled)
	  && (SYMBOLP (XCAR (c)) || INTEGERP (XCAR (c))))
	c = XCAR (c);
      
      /* If the queued event is something that used the mouse,
         set used_mouse_menu accordingly.  */
      if (used_mouse_menu
	  && (EQ (c, Qtool_bar) || EQ (c, Qmenu_bar)))
	*used_mouse_menu = 1;
      
      reread = 1;
      goto reread_for_input_method;
    }

  if (CONSP (Vunread_input_method_events))
    {
      c = XCAR (Vunread_input_method_events);
      Vunread_input_method_events = XCDR (Vunread_input_method_events);

      /* Undo what read_char_x_menu_prompt did when it unread
	 additional keys returned by Fx_popup_menu.  */
      if (CONSP (c)
	  && (SYMBOLP (XCAR (c)) || INTEGERP (XCAR (c)))
	  && NILP (XCDR (c)))
	c = XCAR (c);
      reread = 1;
      goto reread_for_input_method;
    }

  /* If there is no function key translated before
     reset-this-command-lengths takes effect, forget about it.  */
  before_command_restore_flag = 0;

  if (!NILP (Vexecuting_macro))
    {
      /* We set this to Qmacro; since that's not a frame, nobody will
	 try to switch frames on us, and the selected window will
	 remain unchanged.

         Since this event came from a macro, it would be misleading to
	 leave internal_last_event_frame set to wherever the last
	 real event came from.  Normally, a switch-frame event selects
	 internal_last_event_frame after each command is read, but
	 events read from a macro should never cause a new frame to be
	 selected. */
      Vlast_event_frame = internal_last_event_frame = Qmacro;

      /* Exit the macro if we are at the end.
	 Also, some things replace the macro with t
	 to force an early exit.  */
      if (EQ (Vexecuting_macro, Qt)
	  || executing_macro_index >= XFASTINT (Flength (Vexecuting_macro)))
	{
	  XSETINT (c, -1);
	  RETURN_UNGCPRO (c);
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
	 again, so we do not set `reread'.  */
      goto reread_first;
    }

  /* if redisplay was requested */
  if (commandflag >= 0)
    {
	/* If there is pending input, process any events which are not
	   user-visible, such as X selection_request events.  */
      if (input_pending
	  || detect_input_pending_run_timers (0))
	swallow_events (0);		/* may clear input_pending */

      /* Redisplay if no pending input.  */
      while (!input_pending)
	{
	  if (help_echo_showing_p && !EQ (selected_window, minibuf_window))
	    redisplay_preserve_echo_area (5);
	  else
	    redisplay ();

	  if (!input_pending)
	    /* Normal case: no input arrived during redisplay.  */
	    break;

	  /* Input arrived and pre-empted redisplay.
	     Process any events which are not user-visible.  */
	  swallow_events (0);
	  /* If that cleared input_pending, try again to redisplay.  */
	}
    }

  /* Message turns off echoing unless more keystrokes turn it on again.
     
     The code in 20.x for the condition was

     1. echo_area_glyphs && *echo_area_glyphs
     2. && echo_area_glyphs != current_kboard->echobuf
     3. && ok_to_echo_at_next_pause != echo_area_glyphs

     (1) means there's a current message displayed
     
     (2) means it's not the message from echoing from the current
     kboard.
     
     (3) There's only one place in 20.x where ok_to_echo_at_next_pause
     is set to a non-null value.  This is done in read_char and it is
     set to echo_area_glyphs after a call to echo_char.  That means
     ok_to_echo_at_next_pause is either null or
     current_kboard->echobuf with the appropriate current_kboard at
     that time.

     So, condition (3) means in clear text ok_to_echo_at_next_pause
     must be either null, or the current message isn't from echoing at
     all, or it's from echoing from a different kboard than the
     current one.  */
  
  if (/* There currently is something in the echo area.  */
      !NILP (echo_area_buffer[0])
      && (/* And it's either not from echoing.  */
	  !EQ (echo_area_buffer[0], echo_message_buffer)
	  /* Or it's an echo from a different kboard.  */
	  || echo_kboard != current_kboard
	  /* Or we explicitly allow overwriting whatever there is.  */
	  || ok_to_echo_at_next_pause == NULL))
    cancel_echoing ();
  else
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
      && !detect_input_pending_run_timers (0))
    {
      c = read_char_minibuf_menu_prompt (commandflag, nmaps, maps);
      if (! NILP (c))
	{
	  key_already_recorded = 1;
	  goto non_reread_1;
	}
    }

  /* Make a longjmp point for quits to use, but don't alter getcjmp just yet.
     We will do that below, temporarily for short sections of code,
     when appropriate.  local_getcjmp must be in effect
     around any call to sit_for or kbd_buffer_get_event;
     it *must not* be in effect when we call redisplay.  */

  if (_setjmp (local_getcjmp))
    {
      XSETINT (c, quit_char);
      internal_last_event_frame = selected_frame;
      Vlast_event_frame = internal_last_event_frame;
      /* If we report the quit char as an event,
	 don't do so more than once.  */
      if (!NILP (Vinhibit_quit))
	Vquit_flag = Qnil;

#ifdef MULTI_KBOARD
      {
	KBOARD *kb = FRAME_KBOARD (XFRAME (selected_frame));
	if (kb != current_kboard)
	  {
	    Lisp_Object link = kb->kbd_queue;
	    /* We shouldn't get here if we were in single-kboard mode!  */
	    if (single_kboard)
	      abort ();
	    if (CONSP (link))
	      {
		while (CONSP (XCDR (link)))
		  link = XCDR (link);
		if (!NILP (XCDR (link)))
		  abort ();
	      }
	    if (!CONSP (link))
	      kb->kbd_queue = Fcons (c, Qnil);
	    else
	      XSETCDR (link, Fcons (c, Qnil));
	    kb->kbd_queue_has_data = 1;
	    current_kboard = kb;
	    /* This is going to exit from read_char
	       so we had better get rid of this frame's stuff.  */
	    UNGCPRO;
	    longjmp (wrong_kboard_jmpbuf, 1);
	  }
      }
#endif
      goto non_reread;
    }

  timer_start_idle ();

  /* If in middle of key sequence and minibuffer not active,
     start echoing if enough time elapses.  */

  if (minibuf_level == 0 
      && !current_kboard->immediate_echo
      && this_command_key_count > 0
      && ! noninteractive
      && (FLOATP (Vecho_keystrokes) || INTEGERP (Vecho_keystrokes))
      && NILP (Fzerop (Vecho_keystrokes))
      && (/* No message.  */
	  NILP (echo_area_buffer[0])
	  /* Or empty message.  */
	  || (BUF_BEG (XBUFFER (echo_area_buffer[0]))
	      == BUF_Z (XBUFFER (echo_area_buffer[0])))
	  /* Or already echoing from same kboard.  */
	  || (echo_kboard && ok_to_echo_at_next_pause == echo_kboard)
	  /* Or not echoing before and echoing allowed.  */
	  || (!echo_kboard && ok_to_echo_at_next_pause)))
    {
      Lisp_Object tem0;
      
      /* After a mouse event, start echoing right away.
	 This is because we are probably about to display a menu,
	 and we don't want to delay before doing so.  */
      if (EVENT_HAS_PARAMETERS (prev_event))
	echo_now ();
      else
	{
	  int sec, usec;
	  double duration = extract_float (Vecho_keystrokes);
	  sec = (int) duration;
	  usec = (duration - sec) * 1000000;
	  save_getcjmp (save_jump);
	  restore_getcjmp (local_getcjmp);
	  tem0 = sit_for (sec, usec, 1, 1, 0);
	  restore_getcjmp (save_jump);
	  if (EQ (tem0, Qt)
	      && ! CONSP (Vunread_command_events))
	    echo_now ();
	}
    }

  /* Maybe auto save due to number of keystrokes.  */

  if (commandflag != 0
      && auto_save_interval > 0
      && num_nonmacro_input_events - last_auto_save > max (auto_save_interval, 20)
      && !detect_input_pending_run_timers (0))
    {
      Fdo_auto_save (Qnil, Qnil);
      /* Hooks can actually change some buffers in auto save.  */
      redisplay ();
    }

  /* Try reading using an X menu.
     This is never confused with reading using the minibuf
     because the recursive call of read_char in read_char_minibuf_menu_prompt
     does not pass on any keymaps.  */

  if (nmaps > 0 && INTERACTIVE
      && !NILP (prev_event)
      && EVENT_HAS_PARAMETERS (prev_event)
      && !EQ (XCAR (prev_event), Qmenu_bar)
      && !EQ (XCAR (prev_event), Qtool_bar)
      /* Don't bring up a menu if we already have another event.  */
      && NILP (Vunread_command_events)
      && unread_command_char < 0)
    {
      c = read_char_x_menu_prompt (nmaps, maps, prev_event, used_mouse_menu);

      /* Now that we have read an event, Emacs is not idle.  */
      timer_stop_idle ();

      RETURN_UNGCPRO (c);
    }

  /* Maybe autosave and/or garbage collect due to idleness.  */

  if (INTERACTIVE && NILP (c))
    {
      int delay_level, buffer_size;

      /* Slow down auto saves logarithmically in size of current buffer,
	 and garbage collect while we're at it.  */
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
	  && num_nonmacro_input_events > last_auto_save
	  && INTEGERP (Vauto_save_timeout)
	  && XINT (Vauto_save_timeout) > 0)
	{
	  Lisp_Object tem0;

	  save_getcjmp (save_jump);
	  restore_getcjmp (local_getcjmp);
	  tem0 = sit_for (delay_level * XFASTINT (Vauto_save_timeout) / 4,
			  0, 1, 1, 0);
	  restore_getcjmp (save_jump);

	  if (EQ (tem0, Qt)
	      && ! CONSP (Vunread_command_events))
	    {
	      Fdo_auto_save (Qnil, Qnil);

	      /* If we have auto-saved and there is still no input
		 available, garbage collect if there has been enough
		 consing going on to make it worthwhile.  */
	      if (!detect_input_pending_run_timers (0)
		  && consing_since_gc > gc_cons_threshold / 2)
		Fgarbage_collect ();

	      redisplay ();
	    }
	}
    }

  /* If this has become non-nil here, it has been set by a timer
     or sentinel or filter.  */
  if (CONSP (Vunread_command_events))
    {
      c = XCAR (Vunread_command_events);
      Vunread_command_events = XCDR (Vunread_command_events);
    }

  /* Read something from current KBOARD's side queue, if possible.  */

  if (NILP (c))
    {
      if (current_kboard->kbd_queue_has_data)
	{
	  if (!CONSP (current_kboard->kbd_queue))
	    abort ();
	  c = XCAR (current_kboard->kbd_queue);
	  current_kboard->kbd_queue
	    = XCDR (current_kboard->kbd_queue);
	  if (NILP (current_kboard->kbd_queue))
	    current_kboard->kbd_queue_has_data = 0;
	  input_pending = readable_events (0);
	  if (EVENT_HAS_PARAMETERS (c)
	      && EQ (EVENT_HEAD_KIND (EVENT_HEAD (c)), Qswitch_frame))
	    internal_last_event_frame = XCAR (XCDR (c));
	  Vlast_event_frame = internal_last_event_frame;
	}
    }

#ifdef MULTI_KBOARD
  /* If current_kboard's side queue is empty check the other kboards.
     If one of them has data that we have not yet seen here,
     switch to it and process the data waiting for it.

     Note: if the events queued up for another kboard
     have already been seen here, and therefore are not a complete command,
     the kbd_queue_has_data field is 0, so we skip that kboard here.
     That's to avoid an infinite loop switching between kboards here.  */
  if (NILP (c) && !single_kboard)
    {
      KBOARD *kb;
      for (kb = all_kboards; kb; kb = kb->next_kboard)
	if (kb->kbd_queue_has_data)
	  {
	    current_kboard = kb;
	    /* This is going to exit from read_char
	       so we had better get rid of this frame's stuff.  */
	    UNGCPRO;
	    longjmp (wrong_kboard_jmpbuf, 1);
	  }
    }
#endif

 wrong_kboard:

  stop_polling ();

  /* Finally, we read from the main queue,
     and if that gives us something we can't use yet, we put it on the
     appropriate side queue and try again.  */

  if (NILP (c))
    {
      KBOARD *kb;

      /* Actually read a character, waiting if necessary.  */
      save_getcjmp (save_jump);
      restore_getcjmp (local_getcjmp);
      timer_start_idle ();
      c = kbd_buffer_get_event (&kb, used_mouse_menu);
      restore_getcjmp (save_jump);

#ifdef MULTI_KBOARD
      if (! NILP (c) && (kb != current_kboard))
	{
	  Lisp_Object link = kb->kbd_queue;
	  if (CONSP (link))
	    {
	      while (CONSP (XCDR (link)))
		link = XCDR (link);
	      if (!NILP (XCDR (link)))
		abort ();
	    }
	  if (!CONSP (link))
	    kb->kbd_queue = Fcons (c, Qnil);
	  else
	    XSETCDR (link, Fcons (c, Qnil));
	  kb->kbd_queue_has_data = 1;
	  c = Qnil;
	  if (single_kboard)
	    goto wrong_kboard;
	  current_kboard = kb;
	  /* This is going to exit from read_char
	     so we had better get rid of this frame's stuff.  */
	  UNGCPRO;
	  longjmp (wrong_kboard_jmpbuf, 1);
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

  /* Record the last idle start time so that we can reset it
     should the next event read be a help-echo.  */
  last_idle_start = timer_idleness_start_time;
  timer_stop_idle ();
  start_polling ();

  if (NILP (c))
    {
      if (commandflag >= 0
	  && !input_pending && !detect_input_pending_run_timers (0))
	redisplay ();

      goto wrong_kboard;
    }

 non_reread_1:

  /* Buffer switch events are only for internal wakeups
     so don't show them to the user.
     Also, don't record a key if we already did.  */
  if (BUFFERP (c) || key_already_recorded)
    RETURN_UNGCPRO (c);

  /* Process special events within read_char
     and loop around to read another event.  */
  save = Vquit_flag;
  Vquit_flag = Qnil;
  tem = access_keymap (get_keymap (Vspecial_event_map, 0, 1), c, 0, 0, 1);
  Vquit_flag = save;

  if (!NILP (tem))
    {
      int was_locked = single_kboard;

      last_input_char = c;
      Fcommand_execute (tem, Qnil, Fvector (1, &last_input_char), Qt);

      if (CONSP (c) && EQ (XCAR (c), Qselect_window))
	/* We stopped being idle for this event; undo that.  This
	   prevents automatic window selection (under
	   mouse_autoselect_window from acting as a real input event, for
	   example banishing the mouse under mouse-avoidance-mode.  */
	timer_idleness_start_time = last_idle_start;

      /* Resume allowing input from any kboard, if that was true before.  */
      if (!was_locked)
	any_kboard_state ();

      goto retry;
    }

  /* Handle things that only apply to characters.  */
  if (INTEGERP (c))
    {
      /* If kbd_buffer_get_event gave us an EOF, return that.  */
      if (XINT (c) == -1)
	RETURN_UNGCPRO (c);

      if ((STRINGP (Vkeyboard_translate_table)
	   && XSTRING (Vkeyboard_translate_table)->size > (unsigned) XFASTINT (c))
	  || (VECTORP (Vkeyboard_translate_table)
	      && XVECTOR (Vkeyboard_translate_table)->size > (unsigned) XFASTINT (c))
	  || (CHAR_TABLE_P (Vkeyboard_translate_table)
	      && CHAR_TABLE_ORDINARY_SLOTS > (unsigned) XFASTINT (c)))
	{
	  Lisp_Object d;
	  d = Faref (Vkeyboard_translate_table, c);
	  /* nil in keyboard-translate-table means no translation.  */
	  if (!NILP (d))
	    c = d;
	}
    }

  /* If this event is a mouse click in the menu bar,
     return just menu-bar for now.  Modify the mouse click event
     so we won't do this twice, then queue it up.  */
  if (EVENT_HAS_PARAMETERS (c)
      && CONSP (XCDR (c))
      && CONSP (EVENT_START (c))
      && CONSP (XCDR (EVENT_START (c))))
    {
      Lisp_Object posn;

      posn = POSN_BUFFER_POSN (EVENT_START (c));
      /* Handle menu-bar events:
	 insert the dummy prefix event `menu-bar'.  */
      if (EQ (posn, Qmenu_bar) || EQ (posn, Qtool_bar))
	{
	  /* Change menu-bar to (menu-bar) as the event "position".  */
	  POSN_BUFFER_SET_POSN (EVENT_START (c), Fcons (posn, Qnil));

	  also_record = c;
	  Vunread_command_events = Fcons (c, Vunread_command_events);
	  c = posn;
	}
    }

  /* Store these characters into recent_keys, the dribble file if any,
     and the keyboard macro being defined, if any.  */
  record_char (c);
  if (! NILP (also_record))
    record_char (also_record);

  /* Wipe the echo area.
     But first, if we are about to use an input method,
     save the echo area contents for it to refer to.  */
  if (INTEGERP (c)
      && ! NILP (Vinput_method_function)
      && (unsigned) XINT (c) >= ' '
      && (unsigned) XINT (c) != 127
      && (unsigned) XINT (c) < 256)
    {
      previous_echo_area_message = Fcurrent_message ();
      Vinput_method_previous_message = previous_echo_area_message;
    }

  /* Now wipe the echo area, except for help events which do their
     own stuff with the echo area.  */
  if (!CONSP (c)
      || (!(EQ (Qhelp_echo, XCAR (c)))
	  && !(EQ (Qswitch_frame, XCAR (c)))))
    {
      if (!NILP (echo_area_buffer[0]))
	safe_run_hooks (Qecho_area_clear_hook);
      clear_message (1, 0);
    }

 reread_for_input_method:
 from_macro:
  /* Pass this to the input method, if appropriate.  */
  if (INTEGERP (c)
      && ! NILP (Vinput_method_function)
      /* Don't run the input method within a key sequence,
	 after the first event of the key sequence.  */
      && NILP (prev_event)
      && (unsigned) XINT (c) >= ' '
      && (unsigned) XINT (c) != 127
      && (unsigned) XINT (c) < 256)
    {
      Lisp_Object keys; 
      int key_count;
      struct gcpro gcpro1;
      int count = specpdl_ptr - specpdl;

      /* Save the echo status.  */
      int saved_immediate_echo = current_kboard->immediate_echo;
      struct kboard *saved_ok_to_echo = ok_to_echo_at_next_pause;
      int saved_echo_after_prompt = current_kboard->echo_after_prompt;

      if (before_command_restore_flag)
	{
	  this_command_key_count = before_command_key_count_1;
	  if (this_command_key_count < this_single_command_key_start)
	    this_single_command_key_start = this_command_key_count;
	  echo_truncate (before_command_echo_length_1);
	  before_command_restore_flag = 0;
	}

      /* Save the this_command_keys status.  */
      key_count = this_command_key_count;

      if (key_count > 0)
	keys = Fcopy_sequence (this_command_keys);
      else
	keys = Qnil;
      GCPRO1 (keys);

      /* Clear out this_command_keys.  */
      this_command_key_count = 0;

      /* Now wipe the echo area.  */
      if (!NILP (echo_area_buffer[0]))
	safe_run_hooks (Qecho_area_clear_hook);
      clear_message (1, 0);
      echo_truncate (0);

      /* If we are not reading a key sequence,
	 never use the echo area.  */
      if (maps == 0)
	{
	  specbind (Qinput_method_use_echo_area, Qt);
	}

      /* Call the input method.  */
      tem = call1 (Vinput_method_function, c);

      tem = unbind_to (count, tem);

      /* Restore the saved echoing state
	 and this_command_keys state.  */
      this_command_key_count = key_count;
      if (key_count > 0)
	this_command_keys = keys;

      cancel_echoing ();
      ok_to_echo_at_next_pause = saved_ok_to_echo;
      current_kboard->echo_after_prompt = saved_echo_after_prompt;
      if (saved_immediate_echo)
	echo_now ();

      UNGCPRO;

      /* The input method can return no events.  */
      if (! CONSP (tem))
	{
	  /* Bring back the previous message, if any.  */
	  if (! NILP (previous_echo_area_message))
	    message_with_string ("%s", previous_echo_area_message, 0);
	  goto retry;
	}
      /* It returned one event or more.  */
      c = XCAR (tem);
      Vunread_post_input_method_events
	= nconc2 (XCDR (tem), Vunread_post_input_method_events);
    }

 reread_first:

  /* Display help if not echoing.  */
  if (CONSP (c) && EQ (XCAR (c), Qhelp_echo))
    {
      /* (help-echo FRAME HELP WINDOW OBJECT POS).  */
      Lisp_Object help, object, position, window, tem;

      tem = Fcdr (XCDR (c));
      help = Fcar (tem);
      tem = Fcdr (tem);
      window = Fcar (tem);
      tem = Fcdr (tem);
      object = Fcar (tem);
      tem = Fcdr (tem);
      position = Fcar (tem);

      show_help_echo (help, window, object, position, 0);

      /* We stopped being idle for this event; undo that.  */
      timer_idleness_start_time = last_idle_start;
      goto retry;
    }
  
  if (this_command_key_count == 0 || ! reread)
    {
      before_command_key_count = this_command_key_count;
      before_command_echo_length = echo_length ();

      /* Don't echo mouse motion events.  */
      if ((FLOATP (Vecho_keystrokes) || INTEGERP (Vecho_keystrokes))
	  && NILP (Fzerop (Vecho_keystrokes))
	  && ! (EVENT_HAS_PARAMETERS (c)
		&& EQ (EVENT_HEAD_KIND (EVENT_HEAD (c)), Qmouse_movement)))
	{
	  echo_char (c);
	  if (! NILP (also_record))
	    echo_char (also_record);
	  /* Once we reread a character, echoing can happen
	     the next time we pause to read a new one.  */
	  ok_to_echo_at_next_pause = current_kboard;
	}

      /* Record this character as part of the current key.  */
      add_command_key (c);
      if (! NILP (also_record))
	add_command_key (also_record);
    }

  last_input_char = c;
  num_input_events++;

  /* Process the help character specially if enabled */
  if (!NILP (Vhelp_form) && help_char_p (c))
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

      redisplay ();
      if (EQ (c, make_number (040)))
	{
	  cancel_echoing ();
	  do
	    c = read_char (0, 0, 0, Qnil, 0);
	  while (BUFFERP (c));
	}
    }

  RETURN_UNGCPRO (c);
}

/* Record a key that came from a mouse menu.
   Record it for echoing, for this-command-keys, and so on.  */

static void
record_menu_key (c)
     Lisp_Object c;
{
  /* Wipe the echo area.  */
  clear_message (1, 0);

  record_char (c);

  before_command_key_count = this_command_key_count;
  before_command_echo_length = echo_length ();

  /* Don't echo mouse motion events.  */
  if ((FLOATP (Vecho_keystrokes) || INTEGERP (Vecho_keystrokes))
      && NILP (Fzerop (Vecho_keystrokes)))
    {
      echo_char (c);

      /* Once we reread a character, echoing can happen
	 the next time we pause to read a new one.  */
      ok_to_echo_at_next_pause = 0;
    }

  /* Record this character as part of the current key.  */
  add_command_key (c);

  /* Re-reading in the middle of a command */
  last_input_char = c;
  num_input_events++;
}

/* Return 1 if should recognize C as "the help character".  */

int
help_char_p (c)
     Lisp_Object c;
{
  Lisp_Object tail;

  if (EQ (c, Vhelp_char))
    return 1;
  for (tail = Vhelp_event_list; CONSP (tail); tail = XCDR (tail))
    if (EQ (c, XCAR (tail)))
      return 1;
  return 0;
}

/* Record the input event C in various ways.  */

static void
record_char (c)
     Lisp_Object c;
{
  int recorded = 0;

  if (CONSP (c) && (EQ (XCAR (c), Qhelp_echo) || EQ (XCAR (c), Qmouse_movement)))
    {
      /* To avoid filling recent_keys with help-echo and mouse-movement
	 events, we filter out repeated help-echo events, only store the
	 first and last in a series of mouse-movement events, and don't
	 store repeated help-echo events which are only separated by
	 mouse-movement events.  */

      Lisp_Object ev1, ev2, ev3;
      int ix1, ix2, ix3;
      
      if ((ix1 = recent_keys_index - 1) < 0)
	ix1 = NUM_RECENT_KEYS - 1;
      ev1 = AREF (recent_keys, ix1);
      
      if ((ix2 = ix1 - 1) < 0)
	ix2 = NUM_RECENT_KEYS - 1;
      ev2 = AREF (recent_keys, ix2);
      
      if ((ix3 = ix2 - 1) < 0)
	ix3 = NUM_RECENT_KEYS - 1;
      ev3 = AREF (recent_keys, ix3);
     
      if (EQ (XCAR (c), Qhelp_echo))
	{
	  /* Don't record `help-echo' in recent_keys unless it shows some help
	     message, and a different help than the previously recorded
	     event.  */
	  Lisp_Object help, last_help;

	  help = Fcar_safe (Fcdr_safe (XCDR (c)));
	  if (!STRINGP (help))
	    recorded = 1;
	  else if (CONSP (ev1) && EQ (XCAR (ev1), Qhelp_echo)
		   && (last_help = Fcar_safe (Fcdr_safe (XCDR (ev1))), EQ (last_help, help)))
	    recorded = 1;
	  else if (CONSP (ev1) && EQ (XCAR (ev1), Qmouse_movement)
		   && CONSP (ev2) && EQ (XCAR (ev2), Qhelp_echo)
		   && (last_help = Fcar_safe (Fcdr_safe (XCDR (ev2))), EQ (last_help, help)))
	    recorded = -1;
	  else if (CONSP (ev1) && EQ (XCAR (ev1), Qmouse_movement)
		   && CONSP (ev2) && EQ (XCAR (ev2), Qmouse_movement)
		   && CONSP (ev3) && EQ (XCAR (ev3), Qhelp_echo)
		   && (last_help = Fcar_safe (Fcdr_safe (XCDR (ev3))), EQ (last_help, help)))
	    recorded = -2;
	}
      else if (EQ (XCAR (c), Qmouse_movement))
	{
	  /* Only record one pair of `mouse-movement' on a window in recent_keys.
	     So additional mouse movement events replace the last element.  */
	  Lisp_Object last_window, window;

	  window = Fcar_safe (Fcar_safe (XCDR (c)));
	  if (CONSP (ev1) && EQ (XCAR (ev1), Qmouse_movement)
	      && (last_window = Fcar_safe (Fcar_safe (XCDR (ev1))), EQ (last_window, window))
	      && CONSP (ev2) && EQ (XCAR (ev2), Qmouse_movement)
	      && (last_window = Fcar_safe (Fcar_safe (XCDR (ev2))), EQ (last_window, window)))
	    {
	      ASET (recent_keys, ix1, c);
	      recorded = 1;
	    }
	}
    }
  else
    store_kbd_macro_char (c);

  if (!recorded)
    {
      total_keys++;
      ASET (recent_keys, recent_keys_index, c);
      if (++recent_keys_index >= NUM_RECENT_KEYS)
	recent_keys_index = 0;
    }
  else if (recorded < 0)
    {
      /* We need to remove one or two events from recent_keys.
         To do this, we simply put nil at those events and move the
	 recent_keys_index backwards over those events.  Usually,
	 users will never see those nil events, as they will be
	 overwritten by the command keys entered to see recent_keys
	 (e.g. C-h l).  */

      while (recorded++ < 0 && total_keys > 0)
	{
	  if (total_keys < NUM_RECENT_KEYS)
	    total_keys--;
	  if (--recent_keys_index < 0)
	    recent_keys_index = NUM_RECENT_KEYS - 1;
	  ASET (recent_keys, recent_keys_index, Qnil);
	}
    }

  num_nonmacro_input_events++;
      
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
	    fprintf (dribble, " 0x%x", (int) XUINT (c));
	}
      else
	{
	  Lisp_Object dribblee;

	  /* If it's a structured event, take the event header.  */
	  dribblee = EVENT_HEAD (c);

	  if (SYMBOLP (dribblee))
	    {
	      putc ('<', dribble);
	      fwrite (XSTRING (SYMBOL_NAME (dribblee))->data, sizeof (char),
		      STRING_BYTES (XSTRING (SYMBOL_NAME (dribblee))),
		      dribble);
	      putc ('>', dribble);
	    }
	}

      fflush (dribble);
    }
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

static void
save_getcjmp (temp)
     jmp_buf temp;
{
  bcopy (getcjmp, temp, sizeof getcjmp);
}

static void
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
      if (!readable_events (1))
	{
	  redisplay_preserve_echo_area (6);
	  get_input_pending (&input_pending, 1);
	}
    }
  return Qnil;
}

DEFUN ("track-mouse", Ftrack_mouse, Strack_mouse, 0, UNEVALLED, 0,
       doc: /* Evaluate BODY with mouse movement events enabled.
Within a `track-mouse' form, mouse motion generates input events that
you can read with `read-event'.
Normally, mouse motion is ignored.
usage: (track-mouse BODY ...)  */)
     (args)
     Lisp_Object args;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object val;

  record_unwind_protect (tracking_off, do_mouse_tracking);

  do_mouse_tracking = Qt;

  val = Fprogn (args);
  return unbind_to (count, val);
}

/* If mouse has moved on some frame, return one of those frames.
   Return 0 otherwise.  */

static FRAME_PTR
some_mouse_moved ()
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    {
      if (XFRAME (frame)->mouse_moved)
	return XFRAME (frame);
    }

  return 0;
}

#endif	/* HAVE_MOUSE */

/* Low level keyboard/mouse input.
   kbd_buffer_store_event places events in kbd_buffer, and
   kbd_buffer_get_event retrieves them.  */

/* Return true iff there are any events in the queue that read-char
   would return.  If this returns false, a read-char would block.  */
static int
readable_events (do_timers_now)
     int do_timers_now;
{
  if (do_timers_now)
    timer_check (do_timers_now);

  if (kbd_fetch_ptr != kbd_store_ptr)
    return 1;
#ifdef HAVE_MOUSE
  if (!NILP (do_mouse_tracking) && some_mouse_moved ())
    return 1;
#endif
  if (single_kboard)
    {
      if (current_kboard->kbd_queue_has_data)
	return 1;
    }
  else
    {
      KBOARD *kb;
      for (kb = all_kboards; kb; kb = kb->next_kboard)
	if (kb->kbd_queue_has_data)
	  return 1;
    }
  return 0;
}

/* Set this for debugging, to have a way to get out */
int stop_character;

#ifdef MULTI_KBOARD
static KBOARD *
event_to_kboard (event)
     struct input_event *event;
{
  Lisp_Object frame;
  frame = event->frame_or_window;
  if (CONSP (frame))
    frame = XCAR (frame);
  else if (WINDOWP (frame))
    frame = WINDOW_FRAME (XWINDOW (frame));

  /* There are still some events that don't set this field.
     For now, just ignore the problem.
     Also ignore dead frames here.  */
  if (!FRAMEP (frame) || !FRAME_LIVE_P (XFRAME (frame)))
    return 0;
  else
    return FRAME_KBOARD (XFRAME (frame));
}
#endif

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
	  static SIGTYPE interrupt_signal (int);
#ifdef MULTI_KBOARD
	  KBOARD *kb;
	  struct input_event *sp;

	  if (single_kboard
	      && (kb = FRAME_KBOARD (XFRAME (event->frame_or_window)),
		  kb != current_kboard))
	    {
	      kb->kbd_queue
		= Fcons (make_lispy_switch_frame (event->frame_or_window),
			 Fcons (make_number (c), Qnil));
	      kb->kbd_queue_has_data = 1;
	      for (sp = kbd_fetch_ptr; sp != kbd_store_ptr; sp++)
		{
		  if (sp == kbd_buffer + KBD_BUFFER_SIZE)
		    sp = kbd_buffer;

		  if (event_to_kboard (sp) == kb)
		    {
		      sp->kind = no_event;
		      sp->frame_or_window = Qnil;
		      sp->arg = Qnil;
		    }
		}
	      return;
	    }
#endif

	  /* If this results in a quit_char being returned to Emacs as
	     input, set Vlast_event_frame properly.  If this doesn't
	     get returned to Emacs as an event, the next event read
	     will set Vlast_event_frame again, so this is safe to do.  */
	  {
	    Lisp_Object focus;

	    focus = FRAME_FOCUS_FRAME (XFRAME (event->frame_or_window));
	    if (NILP (focus))
	      focus = event->frame_or_window;
	    internal_last_event_frame = focus;
	    Vlast_event_frame = focus;
	  }

	  last_event_timestamp = event->timestamp;
	  interrupt_signal (0 /* dummy */);
	  return;
	}

      if (c && c == stop_character)
	{
	  sys_suspend ();
	  return;
	}
    }
  /* Don't insert two buffer_switch_event's in a row.
     Just ignore the second one.  */
  else if (event->kind == buffer_switch_event
	   && kbd_fetch_ptr != kbd_store_ptr
	   && kbd_store_ptr->kind == buffer_switch_event)
    return;

  if (kbd_store_ptr - kbd_buffer == KBD_BUFFER_SIZE)
    kbd_store_ptr = kbd_buffer;

  /* Don't let the very last slot in the buffer become full,
     since that would make the two pointers equal,
     and that is indistinguishable from an empty buffer.
     Discard the event if it would fill the last slot.  */
  if (kbd_fetch_ptr - 1 != kbd_store_ptr)
    {
      int idx;
      
#if 0 /* The selection_request_event case looks bogus, and it's error
	 prone to assign individual members for other events, in case
	 the input_event structure is changed.  --2000-07-13, gerd.  */
      struct input_event *sp = kbd_store_ptr;
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
	  sp->arg = event->arg;
	  sp->modifiers = event->modifiers;
	  sp->x = event->x;
	  sp->y = event->y;
	  sp->timestamp = event->timestamp;
	}
#else
      *kbd_store_ptr = *event;
#endif

      idx = 2 * (kbd_store_ptr - kbd_buffer);
      ASET (kbd_buffer_gcpro, idx, event->frame_or_window);
      ASET (kbd_buffer_gcpro, idx + 1, event->arg);
      ++kbd_store_ptr;
    }
}


/* Generate HELP_EVENT input_events in BUFP which has room for
   SIZE events.  If there's not enough room in BUFP, ignore this
   event.

   HELP is the help form.

   FRAME is the frame on which the help is generated.  OBJECT is the
   Lisp object where the help was found (a buffer, a string, an
   overlay, or nil if neither from a string nor from a buffer.  POS is
   the position within OBJECT where the help was found.

   Value is the number of input_events generated.  */

int
gen_help_event (bufp, size, help, frame, window, object, pos)
     struct input_event *bufp;
     int size;
     Lisp_Object help, frame, object, window;
     int pos;
{
  if (size >= 1)
    {
      bufp->kind = HELP_EVENT;
      bufp->frame_or_window = frame;
      bufp->arg = object;
      bufp->x = WINDOWP (window) ? window : frame;
      bufp->y = help;
      bufp->code = pos;
      return 1;
    }
  return 0;
}


/* Store HELP_EVENTs for HELP on FRAME in the input queue.  */

void
kbd_buffer_store_help_event (frame, help)
     Lisp_Object frame, help;
{
  struct input_event event;

  event.kind = HELP_EVENT;
  event.frame_or_window = frame;
  event.arg = Qnil;
  event.x = Qnil;
  event.y = help;
  event.code = 0;
  kbd_buffer_store_event (&event);
}


/* Discard any mouse events in the event buffer by setting them to
   no_event.  */
void
discard_mouse_events ()
{
  struct input_event *sp;
  for (sp = kbd_fetch_ptr; sp != kbd_store_ptr; sp++)
    {
      if (sp == kbd_buffer + KBD_BUFFER_SIZE)
	sp = kbd_buffer;

      if (sp->kind == mouse_click
#ifdef WINDOWSNT
	  || sp->kind == w32_scroll_bar_click
#endif
	  || sp->kind == scroll_bar_click)
	{
	  sp->kind = no_event;
	}
    }
}


/* Return non-zero if there are any real events waiting in the event
   buffer, not counting `no_event's.

   If DISCARD is non-zero, discard no_event events at the front of
   the input queue, possibly leaving the input queue empty if there
   are no real input events.  */

int
kbd_buffer_events_waiting (discard)
     int discard;
{
  struct input_event *sp;
  
  for (sp = kbd_fetch_ptr;
       sp != kbd_store_ptr && sp->kind == no_event;
       ++sp)
    {
      if (sp == kbd_buffer + KBD_BUFFER_SIZE)
	sp = kbd_buffer;
    }

  if (discard)
    kbd_fetch_ptr = sp;

  return sp != kbd_store_ptr && sp->kind != no_event;
}


/* Clear input event EVENT.  */

static INLINE void
clear_event (event)
     struct input_event *event;
{
  int idx = 2 * (event - kbd_buffer);
  ASET (kbd_buffer_gcpro, idx, Qnil);
  ASET (kbd_buffer_gcpro, idx + 1, Qnil);
  event->kind = no_event;
}


/* Read one event from the event buffer, waiting if necessary.
   The value is a Lisp object representing the event.
   The value is nil for an event that should be ignored,
   or that was handled here.
   We always read and discard one event.  */

static Lisp_Object
kbd_buffer_get_event (kbp, used_mouse_menu)
     KBOARD **kbp;
     int *used_mouse_menu;
{
  register int c;
  Lisp_Object obj;

  if (noninteractive)
    {
      c = getchar ();
      XSETINT (obj, c);
      *kbp = current_kboard;
      return obj;
    }

  /* Wait until there is input available.  */
  for (;;)
    {
      if (kbd_fetch_ptr != kbd_store_ptr)
	break;
#ifdef HAVE_MOUSE
      if (!NILP (do_mouse_tracking) && some_mouse_moved ())
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
      if (!NILP (do_mouse_tracking) && some_mouse_moved ())
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

  if (CONSP (Vunread_command_events))
    {
      Lisp_Object first;
      first = XCAR (Vunread_command_events);
      Vunread_command_events = XCDR (Vunread_command_events);
      *kbp = current_kboard;
      return first;
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

#ifdef MULTI_KBOARD
      *kbp = event_to_kboard (event);
      if (*kbp == 0)
	*kbp = current_kboard;  /* Better than returning null ptr?  */
#else
      *kbp = &the_only_kboard;
#endif

      obj = Qnil;

      /* These two kinds of events get special handling
	 and don't actually appear to the command loop.
	 We return nil for them.  */
      if (event->kind == selection_request_event)
	{
#ifdef HAVE_X11
	  struct input_event copy;

	  /* Remove it from the buffer before processing it,
	     since otherwise swallow_events will see it
	     and process it again.  */
	  copy = *event;
	  kbd_fetch_ptr = event + 1;
	  input_pending = readable_events (0);
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
	  struct input_event copy;

	  /* Remove it from the buffer before processing it.  */
	  copy = *event;
	  kbd_fetch_ptr = event + 1;
	  input_pending = readable_events (0);
	  x_handle_selection_clear (&copy);
#else
	  /* We're getting selection request events, but we don't have
             a window system.  */
	  abort ();
#endif
	}
#if defined (HAVE_X11) || defined (HAVE_NTGUI) || defined (MAC_OS)
      else if (event->kind == delete_window_event)
	{
	  /* Make an event (delete-frame (FRAME)).  */
	  obj = Fcons (event->frame_or_window, Qnil);
	  obj = Fcons (Qdelete_frame, Fcons (obj, Qnil));
	  kbd_fetch_ptr = event + 1;
	}
#endif
#if defined (HAVE_X11) || defined (HAVE_NTGUI)
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
      else if (event->kind == buffer_switch_event)
	{
	  /* The value doesn't matter here; only the type is tested.  */
	  XSETBUFFER (obj, current_buffer);
	  kbd_fetch_ptr = event + 1;
	}
#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) || defined (MAC_OS)
      else if (event->kind == menu_bar_activate_event)
	{
	  kbd_fetch_ptr = event + 1;
	  input_pending = readable_events (0);
	  if (FRAME_LIVE_P (XFRAME (event->frame_or_window)))
	    x_activate_menubar (XFRAME (event->frame_or_window));
	}
#endif
#ifdef WINDOWSNT
      else if (event->kind == language_change_event)
	{
	  /* Make an event (language-change (FRAME CHARSET LCID)).  */
	  obj = Fcons (event->modifiers, Qnil);
	  obj = Fcons (event->code, obj);
	  obj = Fcons (event->frame_or_window, obj);
	  obj = Fcons (Qlanguage_change, Fcons (obj, Qnil));
	  kbd_fetch_ptr = event + 1;
	}
#endif
      else if (event->kind == save_session_event)
        {
          obj = Fcons (Qsave_session, Qnil);
	  kbd_fetch_ptr = event + 1;
        }
      /* Just discard these, by returning nil.
	 With MULTI_KBOARD, these events are used as placeholders
	 when we need to randomly delete events from the queue.
	 (They shouldn't otherwise be found in the buffer,
	 but on some machines it appears they do show up
	 even without MULTI_KBOARD.)  */
      /* On Windows NT/9X, no_event is used to delete extraneous
         mouse events during a popup-menu call.  */
      else if (event->kind == no_event)
	kbd_fetch_ptr = event + 1;
      else if (event->kind == HELP_EVENT)
	{
	  Lisp_Object object, position, help, frame, window;

	  frame = event->frame_or_window;
	  object = event->arg;
	  position = make_number (event->code);
	  window = event->x;
	  help = event->y;
	  clear_event (event);

	  kbd_fetch_ptr = event + 1;
	  if (!WINDOWP (window))
	    window = Qnil;
	  obj = Fcons (Qhelp_echo,
		       list5 (frame, help, window, object, position));
	}
      else if (event->kind == FOCUS_IN_EVENT)
	{
	  /* Notification of a FocusIn event.  The frame receiving the
	     focus is in event->frame_or_window.  Generate a
	     switch-frame event if necessary.  */
	  Lisp_Object frame, focus;

	  frame = event->frame_or_window;
	  focus = FRAME_FOCUS_FRAME (XFRAME (frame));
	  if (FRAMEP (focus))
	    frame = focus;

	  if (!EQ (frame, internal_last_event_frame)
	      && !EQ (frame, selected_frame))
	    obj = make_lispy_switch_frame (frame);
	  internal_last_event_frame = frame;
	  kbd_fetch_ptr = event + 1;
	}
      else if (event->kind == SELECT_WINDOW_EVENT)
	{
	  /* Make an event (select-window (WINDOW)).  */
	  obj = Fcons (event->frame_or_window, Qnil);
	  obj = Fcons (Qselect_window, Fcons (obj, Qnil));

	  kbd_fetch_ptr = event + 1;
	}
      else
	{
	  /* If this event is on a different frame, return a switch-frame this
	     time, and leave the event in the queue for next time.  */
	  Lisp_Object frame;
	  Lisp_Object focus;

	  frame = event->frame_or_window;
	  if (CONSP (frame))
	    frame = XCAR (frame);
	  else if (WINDOWP (frame))
	    frame = WINDOW_FRAME (XWINDOW (frame));

	  focus = FRAME_FOCUS_FRAME (XFRAME (frame));
	  if (! NILP (focus))
	    frame = focus;

	  if (! EQ (frame, internal_last_event_frame)
	      && !EQ (frame, selected_frame))
	    obj = make_lispy_switch_frame (frame);
	  internal_last_event_frame = frame;

	  /* If we didn't decide to make a switch-frame event, go ahead
	     and build a real event from the queue entry.  */

	  if (NILP (obj))
	    {
	      obj = make_lispy_event (event);
	      
#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI)
	      /* If this was a menu selection, then set the flag to inhibit
		 writing to last_nonmenu_event.  Don't do this if the event
		 we're returning is (menu-bar), though; that indicates the
		 beginning of the menu sequence, and we might as well leave
		 that as the `event with parameters' for this selection.  */
	      if (used_mouse_menu
		  && !EQ (event->frame_or_window, event->arg)
		  && (event->kind == MENU_BAR_EVENT
		      || event->kind == TOOL_BAR_EVENT))
		*used_mouse_menu = 1;
#endif

	      /* Wipe out this event, to catch bugs.  */
	      clear_event (event);
	      kbd_fetch_ptr = event + 1;
	    }
	}
    }
#ifdef HAVE_MOUSE
  /* Try generating a mouse motion event.  */
  else if (!NILP (do_mouse_tracking) && some_mouse_moved ())
    {
      FRAME_PTR f = some_mouse_moved ();
      Lisp_Object bar_window;
      enum scroll_bar_part part;
      Lisp_Object x, y;
      unsigned long time;

      *kbp = current_kboard;
      /* Note that this uses F to determine which display to look at.
	 If there is no valid info, it does not store anything
	 so x remains nil.  */
      x = Qnil;
      (*mouse_position_hook) (&f, 0, &bar_window, &part, &x, &y, &time);

      obj = Qnil;

      /* Decide if we should generate a switch-frame event.  Don't
	 generate switch-frame events for motion outside of all Emacs
	 frames.  */
      if (!NILP (x) && f)
	{
	  Lisp_Object frame;

	  frame = FRAME_FOCUS_FRAME (f);
	  if (NILP (frame))
	    XSETFRAME (frame, f);

	  if (! EQ (frame, internal_last_event_frame)
	      && !EQ (frame, selected_frame))
	    obj = make_lispy_switch_frame (frame);
	  internal_last_event_frame = frame;
	}

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

  input_pending = readable_events (0);

  Vlast_event_frame = internal_last_event_frame;

  return (obj);
}

/* Process any events that are not user-visible,
   then return, without reading any user-visible events.  */

void
swallow_events (do_display)
     int do_display;
{
  int old_timers_run;

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

	  /* Remove it from the buffer before processing it,
	     since otherwise swallow_events called recursively could see it
	     and process it again.  */
	  copy = *event;
	  kbd_fetch_ptr = event + 1;
	  input_pending = readable_events (0);
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
	  struct input_event copy;

	  /* Remove it from the buffer before processing it,  */
	  copy = *event;

	  kbd_fetch_ptr = event + 1;
	  input_pending = readable_events (0);
	  x_handle_selection_clear (&copy);
#else
	  /* We're getting selection request events, but we don't have
             a window system.  */
	  abort ();
#endif
	}
      else
	break;
    }

  old_timers_run = timers_run;
  get_input_pending (&input_pending, 1);

  if (timers_run != old_timers_run && do_display)
    redisplay_preserve_echo_area (7);
}

/* Record the start of when Emacs is idle,
   for the sake of running idle-time timers.  */

void
timer_start_idle ()
{
  Lisp_Object timers;

  /* If we are already in the idle state, do nothing.  */
  if (! EMACS_TIME_NEG_P (timer_idleness_start_time))
    return;

  EMACS_GET_TIME (timer_idleness_start_time);

  /* Mark all idle-time timers as once again candidates for running.  */
  for (timers = Vtimer_idle_list; CONSP (timers); timers = XCDR (timers))
    {
      Lisp_Object timer;

      timer = XCAR (timers);

      if (!VECTORP (timer) || XVECTOR (timer)->size != 8)
	continue;
      XVECTOR (timer)->contents[0] = Qnil;
    }
}

/* Record that Emacs is no longer idle, so stop running idle-time timers.  */

void
timer_stop_idle ()
{
  EMACS_SET_SECS_USECS (timer_idleness_start_time, -1, -1);
}

/* This is only for debugging.  */
struct input_event last_timer_event;

/* Check whether a timer has fired.  To prevent larger problems we simply
   disregard elements that are not proper timers.  Do not make a circular
   timer list for the time being.

   Returns the number of seconds to wait until the next timer fires.  If a
   timer is triggering now, return zero seconds.
   If no timer is active, return -1 seconds.

   If a timer is ripe, we run it, with quitting turned off.

   DO_IT_NOW is now ignored.  It used to mean that we should
   run the timer directly instead of queueing a timer-event.
   Now we always run timers directly.  */

EMACS_TIME
timer_check (do_it_now)
     int do_it_now;
{
  EMACS_TIME nexttime;
  EMACS_TIME now, idleness_now;
  Lisp_Object timers, idle_timers, chosen_timer;
  struct gcpro gcpro1, gcpro2, gcpro3;

  EMACS_SET_SECS (nexttime, -1);
  EMACS_SET_USECS (nexttime, -1);

  /* Always consider the ordinary timers.  */
  timers = Vtimer_list;
  /* Consider the idle timers only if Emacs is idle.  */
  if (! EMACS_TIME_NEG_P (timer_idleness_start_time))
    idle_timers = Vtimer_idle_list;
  else
    idle_timers = Qnil;
  chosen_timer = Qnil;
  GCPRO3 (timers, idle_timers, chosen_timer);

  if (CONSP (timers) || CONSP (idle_timers))
    {
      EMACS_GET_TIME (now);
      if (! EMACS_TIME_NEG_P (timer_idleness_start_time))
	EMACS_SUB_TIME (idleness_now, now, timer_idleness_start_time);
    }

  while (CONSP (timers) || CONSP (idle_timers))
    {
      Lisp_Object *vector;
      Lisp_Object timer = Qnil, idle_timer = Qnil;
      EMACS_TIME timer_time, idle_timer_time;
      EMACS_TIME difference, timer_difference, idle_timer_difference;

      /* Skip past invalid timers and timers already handled.  */
      if (!NILP (timers))
	{
	  timer = XCAR (timers);
	  if (!VECTORP (timer) || XVECTOR (timer)->size != 8)
	    {
	      timers = XCDR (timers);
	      continue;
	    }
	  vector = XVECTOR (timer)->contents;

	  if (!INTEGERP (vector[1]) || !INTEGERP (vector[2])
	      || !INTEGERP (vector[3])
	      || ! NILP (vector[0]))
	    {
	      timers = XCDR (timers);
	      continue;
	    }
	}
      if (!NILP (idle_timers))
	{
	  timer = XCAR (idle_timers);
	  if (!VECTORP (timer) || XVECTOR (timer)->size != 8)
	    {
	      idle_timers = XCDR (idle_timers);
	      continue;
	    }
	  vector = XVECTOR (timer)->contents;

	  if (!INTEGERP (vector[1]) || !INTEGERP (vector[2])
	      || !INTEGERP (vector[3])
	      || ! NILP (vector[0]))
	    {
	      idle_timers = XCDR (idle_timers);
	      continue;
	    }
	}

      /* Set TIMER, TIMER_TIME and TIMER_DIFFERENCE
	 based on the next ordinary timer.
	 TIMER_DIFFERENCE is the distance in time from NOW to when
	 this timer becomes ripe (negative if it's already ripe).  */
      if (!NILP (timers))
	{
	  timer = XCAR (timers);
	  vector = XVECTOR (timer)->contents;
	  EMACS_SET_SECS (timer_time,
			  (XINT (vector[1]) << 16) | (XINT (vector[2])));
	  EMACS_SET_USECS (timer_time, XINT (vector[3]));
	  EMACS_SUB_TIME (timer_difference, timer_time, now);
	}

      /* Set IDLE_TIMER, IDLE_TIMER_TIME and IDLE_TIMER_DIFFERENCE
	 based on the next idle timer.  */
      if (!NILP (idle_timers))
	{
	  idle_timer = XCAR (idle_timers);
	  vector = XVECTOR (idle_timer)->contents;
	  EMACS_SET_SECS (idle_timer_time,
			  (XINT (vector[1]) << 16) | (XINT (vector[2])));
	  EMACS_SET_USECS (idle_timer_time, XINT (vector[3]));
	  EMACS_SUB_TIME (idle_timer_difference, idle_timer_time, idleness_now);
	}

      /* Decide which timer is the next timer,
	 and set CHOSEN_TIMER, VECTOR and DIFFERENCE accordingly.
	 Also step down the list where we found that timer.  */

      if (! NILP (timers) && ! NILP (idle_timers))
	{
	  EMACS_TIME temp;
	  EMACS_SUB_TIME (temp, timer_difference, idle_timer_difference);
	  if (EMACS_TIME_NEG_P (temp))
	    {
	      chosen_timer = timer;
	      timers = XCDR (timers);
	      difference = timer_difference;
	    }
	  else
	    {
	      chosen_timer = idle_timer;
	      idle_timers = XCDR (idle_timers);
	      difference = idle_timer_difference;
	    }
	}
      else if (! NILP (timers))
	{
	  chosen_timer = timer;
	  timers = XCDR (timers);
	  difference = timer_difference;
	}
      else
	{
	  chosen_timer = idle_timer;
	  idle_timers = XCDR (idle_timers);
	  difference = idle_timer_difference;
	}
      vector = XVECTOR (chosen_timer)->contents;
	
      /* If timer is ripe, run it if it hasn't been run.  */
      if (EMACS_TIME_NEG_P (difference)
	  || (EMACS_SECS (difference) == 0
	      && EMACS_USECS (difference) == 0))
	{
	  if (NILP (vector[0]))
	    {
	      int was_locked = single_kboard;
	      int count = BINDING_STACK_SIZE ();
	      Lisp_Object old_deactivate_mark = Vdeactivate_mark;

	      /* Mark the timer as triggered to prevent problems if the lisp
		 code fails to reschedule it right.  */
	      vector[0] = Qt;

	      specbind (Qinhibit_quit, Qt);
	      
	      call1 (Qtimer_event_handler, chosen_timer);
	      Vdeactivate_mark = old_deactivate_mark;
	      timers_run++;
	      unbind_to (count, Qnil);

	      /* Resume allowing input from any kboard, if that was true before.  */
	      if (!was_locked)
		any_kboard_state ();

	      /* Since we have handled the event,
		 we don't need to tell the caller to wake up and do it.  */
	    }
	}
      else
	/* When we encounter a timer that is still waiting,
	   return the amount of time to wait before it is ripe.  */
	{
	  UNGCPRO;
	  return difference;
	}
    }

  /* No timers are pending in the future.  */
  /* Return 0 if we generated an event, and -1 if not.  */
  UNGCPRO;
  return nexttime;
}

/* Caches for modify_event_symbol.  */
static Lisp_Object accent_key_syms;
static Lisp_Object func_key_syms;
static Lisp_Object mouse_syms;
#ifdef WINDOWSNT
static Lisp_Object mouse_wheel_syms;
#endif
static Lisp_Object drag_n_drop_syms;

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

#ifdef HAVE_NTGUI
#define FUNCTION_KEY_OFFSET 0x0

char *lispy_function_keys[] =
  {
    0,                /* 0                      */
    
    0,                /* VK_LBUTTON        0x01 */
    0,                /* VK_RBUTTON        0x02 */
    "cancel",         /* VK_CANCEL         0x03 */
    0,                /* VK_MBUTTON        0x04 */
    
    0, 0, 0,          /*    0x05 .. 0x07        */
    
    "backspace",      /* VK_BACK           0x08 */
    "tab",            /* VK_TAB            0x09 */
    
    0, 0,             /*    0x0A .. 0x0B        */
    
    "clear",          /* VK_CLEAR          0x0C */
    "return",         /* VK_RETURN         0x0D */
    
    0, 0,             /*    0x0E .. 0x0F        */
  
    0,                /* VK_SHIFT          0x10 */
    0,                /* VK_CONTROL        0x11 */
    0,                /* VK_MENU           0x12 */
    "pause",          /* VK_PAUSE          0x13 */
    "capslock",       /* VK_CAPITAL        0x14 */
    
    0, 0, 0, 0, 0, 0, /*    0x15 .. 0x1A        */
    
    "escape",         /* VK_ESCAPE         0x1B */
    
    0, 0, 0, 0,       /*    0x1C .. 0x1F        */
    
    0,                /* VK_SPACE          0x20 */
    "prior",          /* VK_PRIOR          0x21 */
    "next",           /* VK_NEXT           0x22 */
    "end",            /* VK_END            0x23 */
    "home",           /* VK_HOME           0x24 */
    "left",           /* VK_LEFT           0x25 */
    "up",             /* VK_UP             0x26 */
    "right",          /* VK_RIGHT          0x27 */
    "down",           /* VK_DOWN           0x28 */
    "select",         /* VK_SELECT         0x29 */
    "print",          /* VK_PRINT          0x2A */
    "execute",        /* VK_EXECUTE        0x2B */
    "snapshot",       /* VK_SNAPSHOT       0x2C */
    "insert",         /* VK_INSERT         0x2D */
    "delete",         /* VK_DELETE         0x2E */
    "help",           /* VK_HELP           0x2F */
  
    /* VK_0 thru VK_9 are the same as ASCII '0' thru '9' (0x30 - 0x39) */
    
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    
    0, 0, 0, 0, 0, 0, 0, /* 0x3A .. 0x40       */
    
    /* VK_A thru VK_Z are the same as ASCII 'A' thru 'Z' (0x41 - 0x5A) */
    
    0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0,
    
    "lwindow",       /* VK_LWIN           0x5B */
    "rwindow",       /* VK_RWIN           0x5C */
    "apps",          /* VK_APPS           0x5D */
    
    0, 0,            /*    0x5E .. 0x5F        */
    
    "kp-0",          /* VK_NUMPAD0        0x60 */
    "kp-1",          /* VK_NUMPAD1        0x61 */
    "kp-2",          /* VK_NUMPAD2        0x62 */
    "kp-3",          /* VK_NUMPAD3        0x63 */
    "kp-4",          /* VK_NUMPAD4        0x64 */
    "kp-5",          /* VK_NUMPAD5        0x65 */
    "kp-6",          /* VK_NUMPAD6        0x66 */
    "kp-7",          /* VK_NUMPAD7        0x67 */
    "kp-8",          /* VK_NUMPAD8        0x68 */
    "kp-9",          /* VK_NUMPAD9        0x69 */
    "kp-multiply",   /* VK_MULTIPLY       0x6A */
    "kp-add",        /* VK_ADD            0x6B */
    "kp-separator",  /* VK_SEPARATOR      0x6C */
    "kp-subtract",   /* VK_SUBTRACT       0x6D */
    "kp-decimal",    /* VK_DECIMAL        0x6E */
    "kp-divide",     /* VK_DIVIDE         0x6F */
    "f1",            /* VK_F1             0x70 */
    "f2",            /* VK_F2             0x71 */
    "f3",            /* VK_F3             0x72 */
    "f4",            /* VK_F4             0x73 */
    "f5",            /* VK_F5             0x74 */
    "f6",            /* VK_F6             0x75 */
    "f7",            /* VK_F7             0x76 */
    "f8",            /* VK_F8             0x77 */
    "f9",            /* VK_F9             0x78 */
    "f10",           /* VK_F10            0x79 */
    "f11",           /* VK_F11            0x7A */
    "f12",           /* VK_F12            0x7B */
    "f13",           /* VK_F13            0x7C */
    "f14",           /* VK_F14            0x7D */
    "f15",           /* VK_F15            0x7E */
    "f16",           /* VK_F16            0x7F */
    "f17",           /* VK_F17            0x80 */
    "f18",           /* VK_F18            0x81 */
    "f19",           /* VK_F19            0x82 */
    "f20",           /* VK_F20            0x83 */
    "f21",           /* VK_F21            0x84 */
    "f22",           /* VK_F22            0x85 */
    "f23",           /* VK_F23            0x86 */
    "f24",           /* VK_F24            0x87 */
    
    0, 0, 0, 0,      /*    0x88 .. 0x8B        */
    0, 0, 0, 0,      /*    0x8C .. 0x8F        */
    
    "kp-numlock",    /* VK_NUMLOCK        0x90 */
    "scroll",        /* VK_SCROLL         0x91 */
    
    "kp-space",	     /* VK_NUMPAD_CLEAR   0x92 */
    "kp-enter",	     /* VK_NUMPAD_ENTER   0x93 */
    "kp-prior",	     /* VK_NUMPAD_PRIOR   0x94 */
    "kp-next",	     /* VK_NUMPAD_NEXT    0x95 */
    "kp-end",	     /* VK_NUMPAD_END     0x96 */
    "kp-home",	     /* VK_NUMPAD_HOME    0x97 */
    "kp-left",	     /* VK_NUMPAD_LEFT    0x98 */
    "kp-up",	     /* VK_NUMPAD_UP      0x99 */
    "kp-right",	     /* VK_NUMPAD_RIGHT   0x9A */
    "kp-down",	     /* VK_NUMPAD_DOWN    0x9B */
    "kp-insert",     /* VK_NUMPAD_INSERT  0x9C */
    "kp-delete",     /* VK_NUMPAD_DELETE  0x9D */

    0, 0,	     /*    0x9E .. 0x9F        */

    /*
     * VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
     * Used only as parameters to GetAsyncKeyState and GetKeyState.
     * No other API or message will distinguish left and right keys this way.
     */
    /* 0xA0 .. 0xEF */
    
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    
    /* 0xF0 .. 0xF5 */
    
    0, 0, 0, 0, 0, 0,
    
    "attn",          /* VK_ATTN           0xF6 */
    "crsel",         /* VK_CRSEL          0xF7 */
    "exsel",         /* VK_EXSEL          0xF8 */
    "ereof",         /* VK_EREOF          0xF9 */
    "play",          /* VK_PLAY           0xFA */
    "zoom",          /* VK_ZOOM           0xFB */
    "noname",        /* VK_NONAME         0xFC */
    "pa1",           /* VK_PA1            0xFD */
    "oem_clear",     /* VK_OEM_CLEAR      0xFE */
    0 /* 0xFF */
  };

#else /* not HAVE_NTGUI */

#ifdef XK_kana_A
static char *lispy_kana_keys[] =
  {
    /* X Keysym value */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x400 .. 0x40f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x410 .. 0x41f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x420 .. 0x42f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x430 .. 0x43f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x440 .. 0x44f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x450 .. 0x45f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x460 .. 0x46f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,"overline",0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x480 .. 0x48f */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x490 .. 0x49f */
    0, "kana-fullstop", "kana-openingbracket", "kana-closingbracket", 
    "kana-comma", "kana-conjunctive", "kana-WO", "kana-a",
    "kana-i", "kana-u", "kana-e", "kana-o",
    "kana-ya", "kana-yu", "kana-yo", "kana-tsu",
    "prolongedsound", "kana-A", "kana-I", "kana-U",
    "kana-E", "kana-O", "kana-KA", "kana-KI",
    "kana-KU", "kana-KE", "kana-KO", "kana-SA",
    "kana-SHI", "kana-SU", "kana-SE", "kana-SO",
    "kana-TA", "kana-CHI", "kana-TSU", "kana-TE",
    "kana-TO", "kana-NA", "kana-NI", "kana-NU",
    "kana-NE", "kana-NO", "kana-HA", "kana-HI",
    "kana-FU", "kana-HE", "kana-HO", "kana-MA",
    "kana-MI", "kana-MU", "kana-ME", "kana-MO",
    "kana-YA", "kana-YU", "kana-YO", "kana-RA",
    "kana-RI", "kana-RU", "kana-RE", "kana-RO",
    "kana-WA", "kana-N", "voicedsound", "semivoicedsound",
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x4e0 .. 0x4ef */
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,	/* 0x4f0 .. 0x4ff */
  };
#endif /* XK_kana_A */

#define FUNCTION_KEY_OFFSET 0xff00

/* You'll notice that this table is arranged to be conveniently
   indexed by X Windows keysym values.  */
static char *lispy_function_keys[] =
  {
    /* X Keysym value */

    0, 0, 0, 0, 0, 0, 0, 0,			      /* 0xff00...0f */
    "backspace", "tab", "linefeed", "clear",
    0, "return", 0, 0,
    0, 0, 0, "pause",				      /* 0xff10...1f */
    0, 0, 0, 0, 0, 0, 0, "escape",
    0, 0, 0, 0,
    0, "kanji", "muhenkan", "henkan",		      /* 0xff20...2f */
    "romaji", "hiragana", "katakana", "hiragana-katakana",
    "zenkaku", "hankaku", "zenkaku-hankaku", "touroku",
    "massyo", "kana-lock", "kana-shift", "eisu-shift",
    "eisu-toggle",				      /* 0xff30...3f */
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   /* 0xff40...4f */

    "home", "left", "up", "right", /* 0xff50 */	/* IsCursorKey */
    "down", "prior", "next", "end",
    "begin", 0, 0, 0, 0, 0, 0, 0,
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

    0, 0, 0, 0,
    0, 0, 0, 0, "backtab", 0, 0, 0,		/* 0xff70... */
    0, 0, 0, 0, 0, 0, 0, "kp-numlock",		/* 0xff78... */
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

/* ISO 9995 Function and Modifier Keys; the first byte is 0xFE.  */
#define ISO_FUNCTION_KEY_OFFSET 0xfe00

static char *iso_lispy_function_keys[] =
  {
    0, 0, 0, 0, 0, 0, 0, 0,	/* 0xfe00 */
    0, 0, 0, 0, 0, 0, 0, 0,	/* 0xfe08 */
    0, 0, 0, 0, 0, 0, 0, 0,	/* 0xfe10 */
    0, 0, 0, 0, 0, 0, 0, 0,	/* 0xfe18 */
    "iso-lefttab",		/* 0xfe20 */
    "iso-move-line-up", "iso-move-line-down", 
    "iso-partial-line-up", "iso-partial-line-down", 
    "iso-partial-space-left", "iso-partial-space-right", 
    "iso-set-margin-left", "iso-set-margin-right", /* 0xffe27, 28 */
    "iso-release-margin-left", "iso-release-margin-right",
    "iso-release-both-margins",
    "iso-fast-cursor-left", "iso-fast-cursor-right",
    "iso-fast-cursor-up", "iso-fast-cursor-down",
    "iso-continuous-underline", "iso-discontinuous-underline", /* 0xfe30, 31 */
    "iso-emphasize", "iso-center-object", "iso-enter", /* ... 0xfe34 */
  };

#endif /* not HAVE_NTGUI */

Lisp_Object Vlispy_mouse_stem;

#ifdef WINDOWSNT
/* mouse-wheel events are generated by the wheel on devices such as
   the MS Intellimouse.  The wheel sits in between the left and right
   mouse buttons, and is typically used to scroll or zoom the window
   underneath the pointer.  mouse-wheel events specify the object on
   which they operate, and a delta corresponding to the amount and
   direction that the wheel is rotated.  Clicking the mouse-wheel
   generates a mouse-2 event.  */
static char *lispy_mouse_wheel_names[] = 
{
  "mouse-wheel"
};

#endif /* WINDOWSNT */

/* drag-n-drop events are generated when a set of selected files are
   dragged from another application and dropped onto an Emacs window.  */
static char *lispy_drag_n_drop_names[] =
{
  "drag-n-drop"
};

/* Scroll bar parts.  */
Lisp_Object Qabove_handle, Qhandle, Qbelow_handle;
Lisp_Object Qup, Qdown, Qbottom, Qend_scroll;
Lisp_Object Qtop, Qratio;

/* An array of scroll bar parts, indexed by an enum scroll_bar_part value.  */
Lisp_Object *scroll_bar_parts[] = {
  &Qabove_handle, &Qhandle, &Qbelow_handle,
  &Qup, &Qdown, &Qtop, &Qbottom, &Qend_scroll, &Qratio
};

/* User signal events.  */
Lisp_Object Qusr1_signal, Qusr2_signal;

Lisp_Object *lispy_user_signals[] =
{
  &Qusr1_signal, &Qusr2_signal
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

/* The maximum time between clicks to make a double-click, or Qnil to
   disable double-click detection, or Qt for no time limit.  */

Lisp_Object Vdouble_click_time;

/* Maximum number of pixels the mouse may be moved between clicks
   to make a double-click.  */

EMACS_INT double_click_fuzz;

/* The number of clicks in this multiple-click. */

int double_click_count;

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
	/* Distinguish Shift-SPC from SPC.  */
	if ((event->code & 0377) == 040
	    && event->modifiers & shift_modifier)
	  c |= shift_modifier;
	button_down_time = 0;
	XSETFASTINT (lispy_c, c);
	return lispy_c;
      }

    case multibyte_char_keystroke:
      {
	Lisp_Object lispy_c;

	XSETFASTINT (lispy_c, event->code);
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

#ifdef XK_kana_A
      if (event->code >= 0x400 && event->code < 0x500)
	return modify_event_symbol (event->code - 0x400,
				    event->modifiers & ~shift_modifier,
				    Qfunction_key, Qnil,
				    lispy_kana_keys, &func_key_syms,
				    (sizeof (lispy_kana_keys)
				     / sizeof (lispy_kana_keys[0])));
#endif /* XK_kana_A */

#ifdef ISO_FUNCTION_KEY_OFFSET
      if (event->code < FUNCTION_KEY_OFFSET
	  && event->code >= ISO_FUNCTION_KEY_OFFSET)
	return modify_event_symbol (event->code - ISO_FUNCTION_KEY_OFFSET,
				    event->modifiers,
				    Qfunction_key, Qnil,
				    iso_lispy_function_keys, &func_key_syms,
				    (sizeof (iso_lispy_function_keys)
				     / sizeof (iso_lispy_function_keys[0])));
#endif

      /* Handle system-specific or unknown keysyms.  */
      if (event->code & (1 << 28)
	  || event->code - FUNCTION_KEY_OFFSET < 0
	  || (event->code - FUNCTION_KEY_OFFSET
	      >= sizeof lispy_function_keys / sizeof *lispy_function_keys)
	  || !lispy_function_keys[event->code - FUNCTION_KEY_OFFSET])
	{
	  /* We need to use an alist rather than a vector as the cache
	     since we can't make a vector long enuf.  */
	  if (NILP (current_kboard->system_key_syms))
	    current_kboard->system_key_syms = Fcons (Qnil, Qnil);
	  return modify_event_symbol (event->code,
				      event->modifiers,
				      Qfunction_key,
				      current_kboard->Vsystem_key_alist,
				      0, &current_kboard->system_key_syms,
				      (unsigned) -1);
	}

      return modify_event_symbol (event->code - FUNCTION_KEY_OFFSET,
				  event->modifiers,
				  Qfunction_key, Qnil,
				  lispy_function_keys, &func_key_syms,
				  (sizeof (lispy_function_keys)
				   / sizeof (lispy_function_keys[0])));

#ifdef HAVE_MOUSE
      /* A mouse click.  Figure out where it is, decide whether it's
         a press, click or drag, and build the appropriate structure.  */
    case mouse_click:
#ifndef USE_TOOLKIT_SCROLL_BARS
    case scroll_bar_click:
#endif
      {
	int button = event->code;
	int is_double;
	Lisp_Object position;
	Lisp_Object *start_pos_ptr;
	Lisp_Object start_pos;
	Lisp_Object window;

	position = Qnil;

	/* Build the position as appropriate for this mouse click.  */
	if (event->kind == mouse_click)
	  {
	    int part;
	    struct frame *f = XFRAME (event->frame_or_window);
	    Lisp_Object posn;
	    Lisp_Object string_info = Qnil;
	    int row, column;

	    /* Ignore mouse events that were made on frame that
	       have been deleted.  */
	    if (! FRAME_LIVE_P (f))
	      return Qnil;

	    /* EVENT->x and EVENT->y are frame-relative pixel
	       coordinates at this place.  Under old redisplay, COLUMN
	       and ROW are set to frame relative glyph coordinates
	       which are then used to determine whether this click is
	       in a menu (non-toolkit version).  */
 	    pixel_to_glyph_coords (f, XINT (event->x), XINT (event->y),
	 			   &column, &row, NULL, 1);

#ifndef USE_X_TOOLKIT
	    /* In the non-toolkit version, clicks on the menu bar
	       are ordinary button events in the event buffer.
	       Distinguish them, and invoke the menu.

	       (In the toolkit version, the toolkit handles the menu bar
	       and Emacs doesn't know about it until after the user
	       makes a selection.)  */
	    if (row >= 0 && row < FRAME_MENU_BAR_LINES (f)
		&& (event->modifiers & down_modifier))
	      {
		Lisp_Object items, item;
		int hpos;
		int i;

#if 0
		/* Activate the menu bar on the down event.  If the
		   up event comes in before the menu code can deal with it,
		   just ignore it.  */
		if (! (event->modifiers & down_modifier))
		  return Qnil;
#endif

		/* Find the menu bar item under `column'.  */
		item = Qnil;
		items = FRAME_MENU_BAR_ITEMS (f);
		for (i = 0; i < XVECTOR (items)->size; i += 4)
		  {
		    Lisp_Object pos, string;
		    string = AREF (items, i + 1);
		    pos = AREF (items, i + 3);
		    if (NILP (string))
		      break;
		    if (column >= XINT (pos)
			&& column < XINT (pos) + XSTRING (string)->size)
		      {
			item = AREF (items, i);
			break;
		      }
		  }

		/* ELisp manual 2.4b says (x y) are window relative but
		   code says they are frame-relative.  */
		position
		  = Fcons (event->frame_or_window,
			   Fcons (Qmenu_bar,
				  Fcons (Fcons (event->x, event->y),
					 Fcons (make_number (event->timestamp),
						Qnil))));

		return Fcons (item, Fcons (position, Qnil));
	      }
#endif /* not USE_X_TOOLKIT */

	    /* Set `window' to the window under frame pixel coordinates
	       event->x/event->y.  */
	    window = window_from_coordinates (f, XINT (event->x),
					      XINT (event->y), &part, 0);

	    if (!WINDOWP (window))
	      {
		window = event->frame_or_window;
		posn = Qnil;
	      }
	    else
	      {
		/* It's a click in window window at frame coordinates
		   event->x/ event->y.  */
		struct window *w = XWINDOW (window);

		/* Get window relative coordinates.  Original code
		   `rounded' this to glyph boundaries.  */
		int wx = FRAME_TO_WINDOW_PIXEL_X (w, XINT (event->x));
		int wy = FRAME_TO_WINDOW_PIXEL_Y (w, XINT (event->y));

		/* Set event coordinates to window-relative coordinates
		   for constructing the Lisp event below.  */
		XSETINT (event->x, wx);
		XSETINT (event->y, wy);

		if (part == 1 || part == 3)
		  {
		    /* Mode line or header line.  Look for a string under
		       the mouse that may have a `local-map' property.  */
		    Lisp_Object string;
		    int charpos;
		    
		    posn = part == 1 ? Qmode_line : Qheader_line;
		    string = mode_line_string (w, wx, wy, part == 1, &charpos);
		    if (STRINGP (string))
		      string_info = Fcons (string, make_number (charpos));
		  }
		else if (part == 2)
		  posn = Qvertical_line;
		else if (part == 6 || part == 7)
		  {
		    int charpos;
		    Lisp_Object object = marginal_area_string (w, wx, wy, part,
							       &charpos);
		    posn = (part == 6) ? Qleft_margin : Qright_margin;
		    if (STRINGP (object))
		      string_info = Fcons (object, make_number (charpos));
		  }
		else
		  {
		    Lisp_Object object;
		    struct display_pos p;
		    buffer_posn_from_coords (w, &wx, &wy, &object, &p);
		    posn = make_number (CHARPOS (p.pos));
		    if (STRINGP (object))
		      string_info
			= Fcons (object,
				 make_number (CHARPOS (p.string_pos)));
		  }
	      }

	    position
	      = Fcons (window,
		       Fcons (posn,
			      Fcons (Fcons (event->x, event->y),
				     Fcons (make_number (event->timestamp),
					    (NILP (string_info)
					     ? Qnil
					     : Fcons (string_info, Qnil))))));
	  }
#ifndef USE_TOOLKIT_SCROLL_BARS
	else
	  {
	    /* It's a scrollbar click.  */
	    Lisp_Object portion_whole;
	    Lisp_Object part;

	    window = event->frame_or_window;
	    portion_whole = Fcons (event->x, event->y);
	    part = *scroll_bar_parts[(int) event->part];

	    position
	      = Fcons (window,
		       Fcons (Qvertical_scroll_bar,
			      Fcons (portion_whole,
				     Fcons (make_number (event->timestamp),
					    Fcons (part, Qnil)))));
	  }
#endif /* not USE_TOOLKIT_SCROLL_BARS */

	if (button >= ASIZE (button_down_location))
	  {
	    button_down_location = larger_vector (button_down_location,
						  button + 1, Qnil);
	    mouse_syms = larger_vector (mouse_syms, button + 1, Qnil);
	  }
	
	start_pos_ptr = &AREF (button_down_location, button);
	start_pos = *start_pos_ptr;
	*start_pos_ptr = Qnil;

	{
	  /* On window-system frames, use the value of
	     double-click-fuzz as is.  On other frames, interpret it
	     as a multiple of 1/8 characters.  */
	  struct frame *f;
	  int fuzz;

	  if (WINDOWP (event->frame_or_window))
	    f = XFRAME (XWINDOW (event->frame_or_window)->frame);
	  else if (FRAMEP (event->frame_or_window))
	    f = XFRAME (event->frame_or_window);
	  else
	    abort ();

	  if (FRAME_WINDOW_P (f))
	    fuzz = double_click_fuzz;
	  else
	    fuzz = double_click_fuzz / 8;

	  is_double = (button == last_mouse_button
		       && (abs (XINT (event->x) - last_mouse_x) <= fuzz)
		       && (abs (XINT (event->y) - last_mouse_y) <= fuzz)
		       && button_down_time != 0
		       && (EQ (Vdouble_click_time, Qt)
			   || (INTEGERP (Vdouble_click_time)
			       && ((int)(event->timestamp - button_down_time)
				   < XINT (Vdouble_click_time)))));
	}
	
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
	    /* If we did not see a down before this up, ignore the up.
	       Probably this happened because the down event chose a
	       menu item.  It would be an annoyance to treat the
	       release of the button that chose the menu item as a
	       separate event.  */

	    if (!CONSP (start_pos))
	      return Qnil;

	    event->modifiers &= ~up_modifier;
#if 0 /* Formerly we treated an up with no down as a click event.  */
	    if (!CONSP (start_pos))
	      event->modifiers |= click_modifier;
	    else
#endif
	      {
		Lisp_Object down;
		EMACS_INT xdiff = double_click_fuzz, ydiff = double_click_fuzz;

		/* The third element of every position
		   should be the (x,y) pair.  */
		down = Fcar (Fcdr (Fcdr (start_pos)));
		if (CONSP (down)
		    && INTEGERP (XCAR (down)) && INTEGERP (XCDR (down)))
		  {
		    xdiff = XFASTINT (event->x) - XFASTINT (XCAR (down));
		    ydiff = XFASTINT (event->y) - XFASTINT (XCDR (down));
		  }

		if (xdiff < double_click_fuzz && xdiff > - double_click_fuzz
		    && ydiff < double_click_fuzz
		    && ydiff > - double_click_fuzz)
		  /* Mouse hasn't moved (much).  */
		  event->modifiers |= click_modifier;
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
				      Qmouse_click, Vlispy_mouse_stem,
				      NULL,
				      &mouse_syms,
				      XVECTOR (mouse_syms)->size);
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

#if USE_TOOLKIT_SCROLL_BARS

      /* We don't have down and up events if using toolkit scroll bars,
	 so make this always a click event.  Store in the `part' of
	 the Lisp event a symbol which maps to the following actions:

	 `above_handle'		page up
	 `below_handle'		page down
	 `up'			line up
	 `down'			line down
	 `top'			top of buffer
	 `bottom'		bottom of buffer
	 `handle'		thumb has been dragged.
	 `end-scroll'		end of interaction with scroll bar

	 The incoming input_event contains in its `part' member an
	 index of type `enum scroll_bar_part' which we can use as an
	 index in scroll_bar_parts to get the appropriate symbol.  */
	 
    case scroll_bar_click:
      {
	Lisp_Object position, head, window, portion_whole, part;

	window = event->frame_or_window;
	portion_whole = Fcons (event->x, event->y);
	part = *scroll_bar_parts[(int) event->part];

	position
	  = Fcons (window,
		   Fcons (Qvertical_scroll_bar,
			  Fcons (portion_whole,
				 Fcons (make_number (event->timestamp),
					Fcons (part, Qnil)))));

	/* Always treat scroll bar events as clicks. */
	event->modifiers |= click_modifier;

	/* Get the symbol we should use for the mouse click.  */
	head = modify_event_symbol (event->code,
				    event->modifiers,
				    Qmouse_click, 
				    Vlispy_mouse_stem,
				    NULL, &mouse_syms,
				    XVECTOR (mouse_syms)->size);
	return Fcons (head, Fcons (position, Qnil));
      }
      
#endif /* USE_TOOLKIT_SCROLL_BARS */

#ifdef WINDOWSNT
    case w32_scroll_bar_click:
      {
	int button = event->code;
	int is_double;
	Lisp_Object position;
	Lisp_Object *start_pos_ptr;
	Lisp_Object start_pos;

	{
	  Lisp_Object window;
	  Lisp_Object portion_whole;
	  Lisp_Object part;

	  window = event->frame_or_window;
	  portion_whole = Fcons (event->x, event->y);
	  part = *scroll_bar_parts[(int) event->part];

	  position
	    = Fcons (window,
		     Fcons (Qvertical_scroll_bar,
			    Fcons (portion_whole,
				   Fcons (make_number (event->timestamp),
					  Fcons (part, Qnil)))));
	}

	/* Always treat W32 scroll bar events as clicks. */
	event->modifiers |= click_modifier;

	{
	  /* Get the symbol we should use for the mouse click.  */
	  Lisp_Object head;

	  head = modify_event_symbol (button,
				      event->modifiers,
				      Qmouse_click, 
				      Vlispy_mouse_stem,
				      NULL, &mouse_syms,
				      XVECTOR (mouse_syms)->size);
	  return Fcons (head,
			Fcons (position,
			       Qnil));
	}
      }
    case mouse_wheel:
      {
	int part;
	FRAME_PTR f = XFRAME (event->frame_or_window);
	Lisp_Object window;
	Lisp_Object posn;
	Lisp_Object head, position;
	int row, column;

	/* Ignore mouse events that were made on frame that
	   have been deleted.  */
	if (! FRAME_LIVE_P (f))
	  return Qnil;
	pixel_to_glyph_coords (f, XINT (event->x), XINT (event->y),
			       &column, &row, NULL, 1);
	window = window_from_coordinates (f, XINT (event->x),
                                          XINT (event->y), &part, 0);

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
	    glyph_to_pixel_coords (XWINDOW(window), column, row,
                                   &pixcolumn, &pixrow);
	    XSETINT (event->x, pixcolumn);
	    XSETINT (event->y, pixrow);

	    if (part == 1)
	      posn = Qmode_line;
	    else if (part == 2)
	      posn = Qvertical_line;
	    else if (part == 3)
	      posn = Qheader_line;
	    else
	      {
		Lisp_Object object;
		struct display_pos p;
		buffer_posn_from_coords (XWINDOW (window), &column, &row,
					 &object, &p);
		posn = make_number (CHARPOS (p.pos));
	      }
	  }

	{
	  Lisp_Object head, position;

	  position
	    = Fcons (window,
		     Fcons (posn,
			    Fcons (Fcons (event->x, event->y),
				   Fcons (make_number (event->timestamp),
					  Qnil))));

	  head = modify_event_symbol (0, event->modifiers,
				      Qmouse_wheel, Qnil,
				      lispy_mouse_wheel_names,
				      &mouse_wheel_syms, 1);
	  return Fcons (head,
			Fcons (position,
			       /* Insert 1 here so event-click-count works.  */
			       Fcons (make_number (1),
				      Fcons (make_number (event->code),
					     Qnil))));
	}
      }
#endif /* WINDOWSNT */

    case drag_n_drop:
      {
	int part;
	FRAME_PTR f;
	Lisp_Object window;
	Lisp_Object posn;
	Lisp_Object files;

	/* The frame_or_window field should be a cons of the frame in
	   which the event occurred and a list of the filenames
	   dropped.  */
	if (! CONSP (event->frame_or_window))
	  abort ();

	f = XFRAME (XCAR (event->frame_or_window));
	files = XCDR (event->frame_or_window);

	/* Ignore mouse events that were made on frames that
	   have been deleted.  */
	if (! FRAME_LIVE_P (f))
	  return Qnil;

	window = window_from_coordinates (f, XINT (event->x),
                                          XINT (event->y), &part, 0);

	if (!WINDOWP (window))
	  {
	    window = XCAR (event->frame_or_window);
	    posn = Qnil;
	  }
	else
	  {
	    /* It's an event in window `window' at frame coordinates
	       event->x/ event->y.  */
	    struct window *w = XWINDOW (window);

	    /* Get window relative coordinates.  */
	    int wx = FRAME_TO_WINDOW_PIXEL_X (w, XINT (event->x));
	    int wy = FRAME_TO_WINDOW_PIXEL_Y (w, XINT (event->y));

	    /* Set event coordinates to window-relative coordinates
	       for constructing the Lisp event below.  */
	    XSETINT (event->x, wx);
	    XSETINT (event->y, wy);

	    if (part == 1)
	      posn = Qmode_line;
	    else if (part == 2)
	      posn = Qvertical_line;
	    else if (part == 3)
	      posn = Qheader_line;
	    else
	      {
		Lisp_Object object;
		struct display_pos p;
		buffer_posn_from_coords (w, &wx, &wy, &object, &p);
		posn = make_number (CHARPOS (p.pos));
	      }
	  }

	{
	  Lisp_Object head, position;

	  position
	    = Fcons (window,
		     Fcons (posn,
			    Fcons (Fcons (event->x, event->y),
				   Fcons (make_number (event->timestamp),
					  Qnil))));

	  head = modify_event_symbol (0, event->modifiers,
				      Qdrag_n_drop, Qnil,
				      lispy_drag_n_drop_names,
				      &drag_n_drop_syms, 1);
	  return Fcons (head,
			Fcons (position,
			       Fcons (files,
				      Qnil)));
	}
      }
#endif /* HAVE_MOUSE */

#if defined (USE_X_TOOLKIT) || defined (HAVE_NTGUI) || defined (MAC_OS)
    case MENU_BAR_EVENT:
      if (EQ (event->arg, event->frame_or_window))
	/* This is the prefix key.  We translate this to
	   `(menu_bar)' because the code in keyboard.c for menu
	   events, which we use, relies on this.  */
	return Fcons (Qmenu_bar, Qnil);
      return event->arg;
#endif

    case TOOL_BAR_EVENT:
      if (EQ (event->arg, event->frame_or_window))
	/* This is the prefix key.  We translate this to
	   `(tool_bar)' because the code in keyboard.c for tool bar
	   events, which we use, relies on this.  */
	return Fcons (Qtool_bar, Qnil);
      else if (SYMBOLP (event->arg))
	return apply_modifiers (event->modifiers, event->arg);
      return event->arg;

    case USER_SIGNAL_EVENT:
      /* A user signal.  */
      return *lispy_user_signals[event->code];
      
    case save_session_event:
      return Qsave_session;
      
      /* The 'kind' field of the event is something we don't recognize.  */
    default:
      abort ();
    }
}

#ifdef HAVE_MOUSE

static Lisp_Object
make_lispy_movement (frame, bar_window, part, x, y, time)
     FRAME_PTR frame;
     Lisp_Object bar_window;
     enum scroll_bar_part part;
     Lisp_Object x, y;
     unsigned long time;
{
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
    {
      int area;
      Lisp_Object window;
      Lisp_Object posn;

      if (frame)
	/* It's in a frame; which window on that frame?  */
	window = window_from_coordinates (frame, XINT (x), XINT (y), &area, 0);
      else
	window = Qnil;

      if (WINDOWP (window))
	{
	  struct window *w = XWINDOW (window);
	  int wx, wy;
	  
	  /* Get window relative coordinates.  */
	  wx = FRAME_TO_WINDOW_PIXEL_X (w, XINT (x));
	  wy = FRAME_TO_WINDOW_PIXEL_Y (w, XINT (y));
	  XSETINT (x, wx);
	  XSETINT (y, wy);
	  
	  if (area == 1)
	    posn = Qmode_line;
	  else if (area == 2)
	    posn = Qvertical_line;
	  else if (area == 3)
	    posn = Qheader_line;
	  else
	    {
	      Lisp_Object object;
	      struct display_pos p;
	      buffer_posn_from_coords (w, &wx, &wy, &object, &p);
	      posn = make_number (CHARPOS (p.pos));
	    }
	}
      else if (frame != 0)
	{
	  XSETFRAME (window, frame);
	  posn = Qnil;
	}
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

#endif /* HAVE_MOUSE */

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

  CHECK_SYMBOL (symbol);

  modifiers = 0;
  name = XSTRING (SYMBOL_NAME (symbol));

  for (i = 0; i+2 <= STRING_BYTES (name); )
    {
      int this_mod_end = 0;
      int this_mod = 0;

      /* See if the name continues with a modifier word.
	 Check that the word appears, but don't check what follows it.
	 Set this_mod and this_mod_end to record what we find.  */

      switch (name->data[i])
	{
#define SINGLE_LETTER_MOD(BIT)				\
	  (this_mod_end = i + 1, this_mod = BIT)

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

#undef SINGLE_LETTER_MOD
	}

      /* If we found no modifier, stop looking for them.  */
      if (this_mod_end == 0)
	break;

      /* Check there is a dash after the modifier, so that it
	 really is a modifier.  */
      if (this_mod_end >= STRING_BYTES (name)
	  || name->data[this_mod_end] != '-')
	break;

      /* This modifier is real; look for another.  */
      modifiers |= this_mod;
      i = this_mod_end + 1;
    }

  /* Should we include the `click' modifier?  */
  if (! (modifiers & (down_modifier | drag_modifier
		      | double_modifier | triple_modifier))
      && i + 7 == STRING_BYTES (name)
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
apply_modifiers_uncached (modifiers, base, base_len, base_len_byte)
     int modifiers;
     char *base;
     int base_len, base_len_byte;
{
  /* Since BASE could contain nulls, we can't use intern here; we have
     to use Fintern, which expects a genuine Lisp_String, and keeps a
     reference to it.  */
  char *new_mods
    = (char *) alloca (sizeof ("A-C-H-M-S-s-down-drag-double-triple-"));
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

    new_name = make_uninit_multibyte_string (mod_len + base_len,
					     mod_len + base_len_byte);
    bcopy (new_mods, XSTRING (new_name)->data,	       mod_len);
    bcopy (base,     XSTRING (new_name)->data + mod_len, base_len_byte);

    return Fintern (new_name, Qnil);
  }
}


static char *modifier_names[] =
{
  "up", "down", "drag", "click", "double", "triple", 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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

Lisp_Object
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

      unmodified = Fintern (make_string (XSTRING (SYMBOL_NAME (symbol))->data + end,
					 STRING_BYTES (XSTRING (SYMBOL_NAME (symbol))) - end),
			    Qnil);

      if (modifiers & ~VALMASK)
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
  modifiers &= VALMASK;

  /* The click modifier never figures into cache indices.  */
  cache = Fget (base, Qmodifier_cache);
  XSETFASTINT (index, (modifiers & ~click_modifier));
  entry = assq_no_quit (index, cache);

  if (CONSP (entry))
    new_symbol = XCDR (entry);
  else
    {
      /* We have to create the symbol ourselves.  */
      new_symbol = apply_modifiers_uncached (modifiers,
					     XSTRING (SYMBOL_NAME (base))->data,
					     XSTRING (SYMBOL_NAME (base))->size,
					     STRING_BYTES (XSTRING (SYMBOL_NAME (base))));

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
  return apply_modifiers ((int) XINT (XCAR (XCDR (parsed))),
			  XCAR (parsed));
}


/* For handling events, we often want to produce a symbol whose name
   is a series of modifier key prefixes ("M-", "C-", etcetera) attached
   to some base, like the name of a function key or mouse button.
   modify_event_symbol produces symbols of this sort.

   NAME_TABLE should point to an array of strings, such that NAME_TABLE[i]
   is the name of the i'th symbol.  TABLE_SIZE is the number of elements
   in the table.

   Alternatively, NAME_ALIST_OR_STEM is either an alist mapping codes
   into symbol names, or a string specifying a name stem used to
   construct a symbol name or the form `STEM-N', where N is the decimal
   representation of SYMBOL_NUM.  NAME_ALIST_OR_STEM is used if it is
   non-nil; otherwise NAME_TABLE is used.

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
modify_event_symbol (symbol_num, modifiers, symbol_kind, name_alist_or_stem,
                     name_table, symbol_table, table_size)
     int symbol_num;
     unsigned modifiers;
     Lisp_Object symbol_kind;
     Lisp_Object name_alist_or_stem;
     char **name_table;
     Lisp_Object *symbol_table;
     unsigned int table_size;
{
  Lisp_Object value;
  Lisp_Object symbol_int;

  /* Get rid of the "vendor-specific" bit here.  */
  XSETINT (symbol_int, symbol_num & 0xffffff);

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
      if (CONSP (name_alist_or_stem))
	value = Fcdr_safe (Fassq (symbol_int, name_alist_or_stem));
      else if (STRINGP (name_alist_or_stem))
	{
	  int len = STRING_BYTES (XSTRING (name_alist_or_stem));
	  char *buf = (char *) alloca (len + 50);
	  sprintf (buf, "%s-%d", XSTRING (name_alist_or_stem)->data,
		   XINT (symbol_int) + 1);
	  value = intern (buf);
	}
      else if (name_table != 0 && name_table[symbol_num])
	value = intern (name_table[symbol_num]);

#ifdef HAVE_WINDOW_SYSTEM
      if (NILP (value))
	{
	  char *name = x_get_keysym_name (symbol_num);
	  if (name)
	    value = intern (name);
	}
#endif

      if (NILP (value))
	{
	  char buf[20];
	  sprintf (buf, "key-%d", symbol_num);
	  value = intern (buf);
	}

      if (CONSP (*symbol_table))
        *symbol_table = Fcons (Fcons (symbol_int, value), *symbol_table);
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

/* Convert a list that represents an event type,
   such as (ctrl meta backspace), into the usual representation of that
   event type as a number or a symbol.  */

DEFUN ("event-convert-list", Fevent_convert_list, Sevent_convert_list, 1, 1, 0,
       doc: /* Convert the event description list EVENT-DESC to an event type.
EVENT-DESC should contain one base event type (a character or symbol)
and zero or more modifier names (control, meta, hyper, super, shift, alt,
drag, down, double or triple).  The base must be last.
The return value is an event type (a character or symbol) which
has the same base event type and all the specified modifiers.  */)
     (event_desc)
     Lisp_Object event_desc;
{
  Lisp_Object base;
  int modifiers = 0;
  Lisp_Object rest;

  base = Qnil;
  rest = event_desc;
  while (CONSP (rest))
    {
      Lisp_Object elt;
      int this = 0;

      elt = XCAR (rest);
      rest = XCDR (rest);

      /* Given a symbol, see if it is a modifier name.  */
      if (SYMBOLP (elt) && CONSP (rest))
	this = parse_solitary_modifier (elt);

      if (this != 0)
	modifiers |= this;
      else if (!NILP (base))
	error ("Two bases given in one event");
      else
	base = elt;

    }

  /* Let the symbol A refer to the character A.  */
  if (SYMBOLP (base) && XSTRING (SYMBOL_NAME (base))->size == 1)
    XSETINT (base, XSTRING (SYMBOL_NAME (base))->data[0]);

  if (INTEGERP (base))
    {
      /* Turn (shift a) into A.  */
      if ((modifiers & shift_modifier) != 0
	  && (XINT (base) >= 'a' && XINT (base) <= 'z'))
	{
	  XSETINT (base, XINT (base) - ('a' - 'A'));
	  modifiers &= ~shift_modifier;
	}

      /* Turn (control a) into C-a.  */
      if (modifiers & ctrl_modifier)
	return make_number ((modifiers & ~ctrl_modifier)
			    | make_ctrl_char (XINT (base)));
      else
	return make_number (modifiers | XINT (base));
    }
  else if (SYMBOLP (base))
    return apply_modifiers (modifiers, base);
  else
    {
      error ("Invalid base event");
      return Qnil;
    }
}

/* Try to recognize SYMBOL as a modifier name.
   Return the modifier flag bit, or 0 if not recognized.  */

static int
parse_solitary_modifier (symbol)
     Lisp_Object symbol;
{
  struct Lisp_String *name = XSTRING (SYMBOL_NAME (symbol));

  switch (name->data[0])
    {
#define SINGLE_LETTER_MOD(BIT)				\
      if (STRING_BYTES (name) == 1)			\
	return BIT;

#define MULTI_LETTER_MOD(BIT, NAME, LEN)		\
      if (LEN == STRING_BYTES (name)			\
	  && ! strncmp (name->data, NAME, LEN))		\
	return BIT;

    case 'A':
      SINGLE_LETTER_MOD (alt_modifier);
      break;

    case 'a':
      MULTI_LETTER_MOD (alt_modifier, "alt", 3);
      break;

    case 'C':
      SINGLE_LETTER_MOD (ctrl_modifier);
      break;

    case 'c':
      MULTI_LETTER_MOD (ctrl_modifier, "ctrl", 4);
      MULTI_LETTER_MOD (ctrl_modifier, "control", 7);
      break;

    case 'H':
      SINGLE_LETTER_MOD (hyper_modifier);
      break;

    case 'h':
      MULTI_LETTER_MOD (hyper_modifier, "hyper", 5);
      break;

    case 'M':
      SINGLE_LETTER_MOD (meta_modifier);
      break;

    case 'm':
      MULTI_LETTER_MOD (meta_modifier, "meta", 4);
      break;

    case 'S':
      SINGLE_LETTER_MOD (shift_modifier);
      break;

    case 's':
      MULTI_LETTER_MOD (shift_modifier, "shift", 5);
      MULTI_LETTER_MOD (super_modifier, "super", 5);
      SINGLE_LETTER_MOD (super_modifier);
      break;

    case 'd':
      MULTI_LETTER_MOD (drag_modifier, "drag", 4);
      MULTI_LETTER_MOD (down_modifier, "down", 4);
      MULTI_LETTER_MOD (double_modifier, "double", 6);
      break;

    case 't':
      MULTI_LETTER_MOD (triple_modifier, "triple", 6);
      break;

#undef SINGLE_LETTER_MOD
#undef MULTI_LETTER_MOD
    }

  return 0;
}

/* Return 1 if EVENT is a list whose elements are all integers or symbols.
   Such a list is not valid as an event,
   but it can be a Lucid-style event type list.  */

int
lucid_event_type_list_p (object)
     Lisp_Object object;
{
  Lisp_Object tail;

  if (! CONSP (object))
    return 0;

  if (EQ (XCAR (object), Qhelp_echo)
      || EQ (XCAR (object), Qvertical_line)
      || EQ (XCAR (object), Qmode_line)
      || EQ (XCAR (object), Qheader_line))
    return 0;

  for (tail = object; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object elt;
      elt = XCAR (tail);
      if (! (INTEGERP (elt) || SYMBOLP (elt)))
	return 0;
    }

  return NILP (tail);
}

/* Store into *addr a value nonzero if terminal input chars are available.
   Serves the purpose of ioctl (0, FIONREAD, addr)
   but works even if FIONREAD does not exist.
   (In fact, this may actually read some input.)

   If DO_TIMERS_NOW is nonzero, actually run timer events that are ripe.  */

static void
get_input_pending (addr, do_timers_now)
     int *addr;
     int do_timers_now;
{
  /* First of all, have we already counted some input?  */
  *addr = !NILP (Vquit_flag) || readable_events (do_timers_now);

  /* If input is being read as it arrives, and we have none, there is none.  */
  if (*addr > 0 || (interrupt_input && ! interrupts_deferred))
    return;

  /* Try to read some input and see how much we get.  */
  gobble_input (0);
  *addr = !NILP (Vquit_flag) || readable_events (do_timers_now);
}

/* Interface to read_avail_input, blocking SIGIO or SIGALRM if necessary.  */

void
gobble_input (expected)
     int expected;
{
#ifndef VMS
#ifdef SIGIO
  if (interrupt_input)
    {
      SIGMASKTYPE mask;
      mask = sigblock (sigmask (SIGIO));
      read_avail_input (expected);
      sigsetmask (mask);
    }
  else
#ifdef POLL_FOR_INPUT
  if (read_socket_hook && !interrupt_input && poll_suppress_count == 0)
    {
      SIGMASKTYPE mask;
      mask = sigblock (sigmask (SIGALRM));
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

void
record_asynch_buffer_change ()
{
  struct input_event event;
  Lisp_Object tem;

  event.kind = buffer_switch_event;
  event.frame_or_window = Qnil;
  event.arg = Qnil;

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
      mask = sigblock (sigmask (SIGIO));
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
    nread = (*read_socket_hook) (input_fd, buf, KBD_BUFFER_SIZE, expected);
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
#if defined (USG) || defined (DGUX)
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
	  cbuf[0] = dos_keyread ();
	  nread = 1;
#else
	  nread = emacs_read (input_fd, cbuf, n_to_read);
#endif
	  /* POSIX infers that processes which are not in the session leader's
	     process group won't get SIGHUP's at logout time.  BSDI adheres to
	     this part standard and returns -1 from read (0) with errno==EIO
	     when the control tty is taken away.
	     Jeffrey Honig <jch@bsdi.com> says this is generally safe.  */
	  if (nread == -1 && errno == EIO)
	    kill (0, SIGHUP);
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
	  buf[i].frame_or_window = selected_frame;
	  buf[i].arg = Qnil;
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

#if defined (USG) && !defined (POSIX_SIGNALS)
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
  kill (getpid (), SIGIO);
#endif
}



static void menu_bar_item P_ ((Lisp_Object, Lisp_Object));
static void menu_bar_one_keymap P_ ((Lisp_Object));

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

  Lisp_Object def, tail;

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

    /* Should overriding-terminal-local-map and overriding-local-map apply?  */
    if (!NILP (Voverriding_local_map_menu_flag))
      {
	/* Yes, use them (if non-nil) as well as the global map.  */
	maps = (Lisp_Object *) alloca (3 * sizeof (maps[0]));
	nmaps = 0;
	if (!NILP (current_kboard->Voverriding_terminal_local_map))
	  maps[nmaps++] = current_kboard->Voverriding_terminal_local_map;
	if (!NILP (Voverriding_local_map))
	  maps[nmaps++] = Voverriding_local_map;
      }
    else
      {
	/* No, so use major and minor mode keymaps and keymap property.
	   Note that menu-bar bindings in the local-map and keymap
	   properties may not work reliable, as they are only
	   recognized when the menu-bar (or mode-line) is updated,
	   which does not normally happen after every command.  */
	Lisp_Object tem;
	int nminor;
	nminor = current_minor_maps (NULL, &tmaps);
	maps = (Lisp_Object *) alloca ((nminor + 3) * sizeof (maps[0]));
	nmaps = 0;
	if (tem = get_local_map (PT, current_buffer, Qkeymap), !NILP (tem))
	  maps[nmaps++] = tem;
	bcopy (tmaps, (void *) (maps + nmaps), nminor * sizeof (maps[0]));
	nmaps += nminor;
	maps[nmaps++] = get_local_map (PT, current_buffer, Qlocal_map);
      }
    maps[nmaps++] = current_global_map;
  }

  /* Look up in each map the dummy prefix key `menu-bar'.  */

  result = Qnil;

  for (mapno = nmaps - 1; mapno >= 0; mapno--)
    if (!NILP (maps[mapno]))
      {
	def = get_keymap (access_keymap (maps[mapno], Qmenu_bar, 1, 0, 1),
			  0, 1);
	if (CONSP (def))
	  menu_bar_one_keymap (def);
      }

  /* Move to the end those items that should be at the end.  */

  for (tail = Vmenu_bar_final_items; CONSP (tail); tail = XCDR (tail))
    {
      int i;
      int end = menu_bar_items_index;

      for (i = 0; i < end; i += 4)
	if (EQ (XCAR (tail), XVECTOR (menu_bar_items_vector)->contents[i]))
	  {
	    Lisp_Object tem0, tem1, tem2, tem3;
	    /* Move the item at index I to the end,
	       shifting all the others forward.  */
	    tem0 = XVECTOR (menu_bar_items_vector)->contents[i + 0];
	    tem1 = XVECTOR (menu_bar_items_vector)->contents[i + 1];
	    tem2 = XVECTOR (menu_bar_items_vector)->contents[i + 2];
	    tem3 = XVECTOR (menu_bar_items_vector)->contents[i + 3];
	    if (end > i + 4)
	      bcopy (&XVECTOR (menu_bar_items_vector)->contents[i + 4],
		     &XVECTOR (menu_bar_items_vector)->contents[i],
		     (end - i - 4) * sizeof (Lisp_Object));
	    XVECTOR (menu_bar_items_vector)->contents[end - 4] = tem0;
	    XVECTOR (menu_bar_items_vector)->contents[end - 3] = tem1;
	    XVECTOR (menu_bar_items_vector)->contents[end - 2] = tem2;
	    XVECTOR (menu_bar_items_vector)->contents[end - 1] = tem3;
	    break;
	  }
    }

  /* Add nil, nil, nil, nil at the end.  */
  i = menu_bar_items_index;
  if (i + 4 > XVECTOR (menu_bar_items_vector)->size)
    {
      Lisp_Object tem;
      tem = Fmake_vector (make_number (2 * i), Qnil);
      bcopy (XVECTOR (menu_bar_items_vector)->contents,
	     XVECTOR (tem)->contents, i * sizeof (Lisp_Object));
      menu_bar_items_vector = tem;
    }
  /* Add this item.  */
  XVECTOR (menu_bar_items_vector)->contents[i++] = Qnil;
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

static Lisp_Object menu_bar_one_keymap_changed_items;

static void
menu_bar_one_keymap (keymap)
     Lisp_Object keymap;
{
  Lisp_Object tail, item;

  menu_bar_one_keymap_changed_items = Qnil;

  /* Loop over all keymap entries that have menu strings.  */
  for (tail = keymap; CONSP (tail); tail = XCDR (tail))
    {
      item = XCAR (tail);
      if (CONSP (item))
	menu_bar_item (XCAR (item), XCDR (item));
      else if (VECTORP (item))
	{
	  /* Loop over the char values represented in the vector.  */
	  int len = XVECTOR (item)->size;
	  int c;
	  for (c = 0; c < len; c++)
	    {
	      Lisp_Object character;
	      XSETFASTINT (character, c);
	      menu_bar_item (character, XVECTOR (item)->contents[c]);
	    }
	}
    }
}

/* Add one item to menu_bar_items_vector, for KEY, ITEM_STRING and DEF.
   If there's already an item for KEY, add this DEF to it.  */

Lisp_Object item_properties;

static void
menu_bar_item (key, item)
     Lisp_Object key, item;
{
  struct gcpro gcpro1;
  int i;
  Lisp_Object tem;

  if (EQ (item, Qundefined))
    {
      /* If a map has an explicit `undefined' as definition,
	 discard any previously made menu bar item.  */

      for (i = 0; i < menu_bar_items_index; i += 4)
	if (EQ (key, XVECTOR (menu_bar_items_vector)->contents[i]))
	  {
	    if (menu_bar_items_index > i + 4)
	      bcopy (&XVECTOR (menu_bar_items_vector)->contents[i + 4],
		     &XVECTOR (menu_bar_items_vector)->contents[i],
		     (menu_bar_items_index - i - 4) * sizeof (Lisp_Object));
	    menu_bar_items_index -= 4;
	  }
    }

  /* If this keymap has already contributed to this KEY,
     don't contribute to it a second time.  */
  tem = Fmemq (key, menu_bar_one_keymap_changed_items);
  if (!NILP (tem) || NILP (item))
    return;

  menu_bar_one_keymap_changed_items
    = Fcons (key, menu_bar_one_keymap_changed_items);

  /* We add to menu_bar_one_keymap_changed_items before doing the
     parse_menu_item, so that if it turns out it wasn't a menu item,
     it still correctly hides any further menu item.  */
  GCPRO1 (key);
  i = parse_menu_item (item, 0, 1);
  UNGCPRO;
  if (!i)
    return;

  item = XVECTOR (item_properties)->contents[ITEM_PROPERTY_DEF];

  /* Find any existing item for this KEY.  */
  for (i = 0; i < menu_bar_items_index; i += 4)
    if (EQ (key, XVECTOR (menu_bar_items_vector)->contents[i]))
      break;

  /* If we did not find this KEY, add it at the end.  */
  if (i == menu_bar_items_index)
    {
      /* If vector is too small, get a bigger one.  */
      if (i + 4 > XVECTOR (menu_bar_items_vector)->size)
	{
	  Lisp_Object tem;
	  tem = Fmake_vector (make_number (2 * i), Qnil);
	  bcopy (XVECTOR (menu_bar_items_vector)->contents,
		 XVECTOR (tem)->contents, i * sizeof (Lisp_Object));
	  menu_bar_items_vector = tem;
	}

      /* Add this item.  */
      XVECTOR (menu_bar_items_vector)->contents[i++] = key;
      XVECTOR (menu_bar_items_vector)->contents[i++]
	= XVECTOR (item_properties)->contents[ITEM_PROPERTY_NAME];
      XVECTOR (menu_bar_items_vector)->contents[i++] = Fcons (item, Qnil);
      XVECTOR (menu_bar_items_vector)->contents[i++] = make_number (0);
      menu_bar_items_index = i;
    }
  /* We did find an item for this KEY.  Add ITEM to its list of maps.  */
  else
    {
      Lisp_Object old;
      old = XVECTOR (menu_bar_items_vector)->contents[i + 2];
      XVECTOR (menu_bar_items_vector)->contents[i + 2] = Fcons (item, old);
    }
}

 /* This is used as the handler when calling menu_item_eval_property.  */
static Lisp_Object
menu_item_eval_property_1 (arg)
     Lisp_Object arg;
{
  /* If we got a quit from within the menu computation,
     quit all the way out of it.  This takes care of C-] in the debugger.  */
  if (CONSP (arg) && EQ (XCAR (arg), Qquit))
    Fsignal (Qquit, Qnil);

  return Qnil;
}

/* Evaluate an expression and return the result (or nil if something 
   went wrong).  Used to evaluate dynamic parts of menu items.  */
Lisp_Object
menu_item_eval_property (sexpr)
     Lisp_Object sexpr;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object val;
  specbind (Qinhibit_redisplay, Qt);
  val = internal_condition_case_1 (Feval, sexpr, Qerror,
				   menu_item_eval_property_1);
  return unbind_to (count, val);
}

/* This function parses a menu item and leaves the result in the
   vector item_properties.
   ITEM is a key binding, a possible menu item.
   If NOTREAL is nonzero, only check for equivalent key bindings, don't
   evaluate dynamic expressions in the menu item.
   INMENUBAR is > 0 when this is considered for an entry in a menu bar
   top level.
   INMENUBAR is < 0 when this is considered for an entry in a keyboard menu.
   parse_menu_item returns true if the item is a menu item and false
   otherwise.  */

int
parse_menu_item (item, notreal, inmenubar)
     Lisp_Object item;
     int notreal, inmenubar;
{
  Lisp_Object def, tem, item_string, start;
  Lisp_Object cachelist;
  Lisp_Object filter;
  Lisp_Object keyhint;
  int i;
  int newcache = 0;

  cachelist = Qnil;
  filter = Qnil;
  keyhint = Qnil;

  if (!CONSP (item))
    return 0;

  /* Create item_properties vector if necessary.  */
  if (NILP (item_properties))
    item_properties
      = Fmake_vector (make_number (ITEM_PROPERTY_ENABLE + 1), Qnil);

  /* Initialize optional entries.  */
  for (i = ITEM_PROPERTY_DEF; i < ITEM_PROPERTY_ENABLE; i++)
    AREF (item_properties, i) = Qnil;
  AREF (item_properties, ITEM_PROPERTY_ENABLE) = Qt;
	 
  /* Save the item here to protect it from GC.  */
  AREF (item_properties, ITEM_PROPERTY_ITEM) = item;

  item_string = XCAR (item);

  start = item;
  item = XCDR (item);
  if (STRINGP (item_string))
    {
      /* Old format menu item.  */
      AREF (item_properties, ITEM_PROPERTY_NAME) = item_string;

      /* Maybe help string.  */
      if (CONSP (item) && STRINGP (XCAR (item)))
	{
	  AREF (item_properties, ITEM_PROPERTY_HELP) = XCAR (item);
	  start = item;
	  item = XCDR (item);
	}
	  
      /* Maybe key binding cache.  */
      if (CONSP (item) && CONSP (XCAR (item))
	  && (NILP (XCAR (XCAR (item)))
	      || VECTORP (XCAR (XCAR (item)))))
	{
	  cachelist = XCAR (item);
	  item = XCDR (item);
	}
      
      /* This is the real definition--the function to run.  */
      AREF (item_properties, ITEM_PROPERTY_DEF) = item;

      /* Get enable property, if any.  */
      if (SYMBOLP (item))
	{
	  tem = Fget (item, Qmenu_enable);
	  if (!NILP (tem))
	    AREF (item_properties, ITEM_PROPERTY_ENABLE) = tem;
	}
    }
  else if (EQ (item_string, Qmenu_item) && CONSP (item))
    {
      /* New format menu item.  */
      AREF (item_properties, ITEM_PROPERTY_NAME) = XCAR (item);
      start = XCDR (item);
      if (CONSP (start))
	{
	  /* We have a real binding.  */
	  AREF (item_properties, ITEM_PROPERTY_DEF) = XCAR (start);

	  item = XCDR (start);
	  /* Is there a cache list with key equivalences. */
	  if (CONSP (item) && CONSP (XCAR (item)))
	    {
	      cachelist = XCAR (item);
	      item = XCDR (item);
	    }

	  /* Parse properties.  */
	  while (CONSP (item) && CONSP (XCDR (item)))
	    {
	      tem = XCAR (item);
	      item = XCDR (item);

	      if (EQ (tem, QCenable))
		AREF (item_properties, ITEM_PROPERTY_ENABLE) = XCAR (item);
	      else if (EQ (tem, QCvisible) && !notreal)
		{
		  /* If got a visible property and that evaluates to nil
		     then ignore this item.  */
		  tem = menu_item_eval_property (XCAR (item));
		  if (NILP (tem))
		    return 0;
	 	}
	      else if (EQ (tem, QChelp))
		AREF (item_properties, ITEM_PROPERTY_HELP) = XCAR (item);
	      else if (EQ (tem, QCfilter))
		filter = item;
	      else if (EQ (tem, QCkey_sequence))
		{
		  tem = XCAR (item);
		  if (NILP (cachelist)
		      && (SYMBOLP (tem) || STRINGP (tem) || VECTORP (tem)))
		    /* Be GC protected. Set keyhint to item instead of tem. */
		    keyhint = item;
		}
	      else if (EQ (tem, QCkeys))
		{
		  tem = XCAR (item);
		  if (CONSP (tem) || (STRINGP (tem) && NILP (cachelist)))
		    AREF (item_properties, ITEM_PROPERTY_KEYEQ) = tem;
		}
	      else if (EQ (tem, QCbutton) && CONSP (XCAR (item)))
		{
		  Lisp_Object type;
		  tem = XCAR (item);
		  type = XCAR (tem);
		  if (EQ (type, QCtoggle) || EQ (type, QCradio))
		    {
		      AREF (item_properties, ITEM_PROPERTY_SELECTED)
			= XCDR (tem);
		      AREF (item_properties, ITEM_PROPERTY_TYPE)
			= type;
		    }
		}
	      item = XCDR (item);
	    }
	}
      else if (inmenubar || !NILP (start))
	return 0;
    }
  else
    return 0;			/* not a menu item */

  /* If item string is not a string, evaluate it to get string.
     If we don't get a string, skip this item.  */
  item_string = AREF (item_properties, ITEM_PROPERTY_NAME);
  if (!(STRINGP (item_string) || notreal))
    {
      item_string = menu_item_eval_property (item_string);
      if (!STRINGP (item_string))
	return 0;
      AREF (item_properties, ITEM_PROPERTY_NAME) = item_string;
    }
     
  /* If got a filter apply it on definition.  */
  def = AREF (item_properties, ITEM_PROPERTY_DEF);
  if (!NILP (filter))
    {
      def = menu_item_eval_property (list2 (XCAR (filter),
					    list2 (Qquote, def)));

      AREF (item_properties, ITEM_PROPERTY_DEF) = def;
    }

  /* Enable or disable selection of item.  */
  tem = AREF (item_properties, ITEM_PROPERTY_ENABLE);
  if (!EQ (tem, Qt))
    {
      if (notreal)
	tem = Qt;
      else
	tem = menu_item_eval_property (tem);
      if (inmenubar && NILP (tem))
	return 0;		/* Ignore disabled items in menu bar.  */
      AREF (item_properties, ITEM_PROPERTY_ENABLE) = tem;
    }

  /* If we got no definition, this item is just unselectable text which
     is OK in a submenu but not in the menubar.  */
  if (NILP (def))
    return (inmenubar ? 0 : 1);
 
  /* See if this is a separate pane or a submenu.  */
  def = AREF (item_properties, ITEM_PROPERTY_DEF);
  tem = get_keymap (def, 0, 1);
  /* For a subkeymap, just record its details and exit.  */
  if (CONSP (tem))
    {
      AREF (item_properties, ITEM_PROPERTY_MAP) = tem;
      AREF (item_properties, ITEM_PROPERTY_DEF) = tem;
      return 1;
    }
  
  /* At the top level in the menu bar, do likewise for commands also.
     The menu bar does not display equivalent key bindings anyway.
     ITEM_PROPERTY_DEF is already set up properly.  */
  if (inmenubar > 0)
    return 1;

  /* This is a command.  See if there is an equivalent key binding. */
  if (NILP (cachelist))
    {
      /* We have to create a cachelist.  */
      CHECK_IMPURE (start);
      XSETCDR (start, Fcons (Fcons (Qnil, Qnil), XCDR (start)));
      cachelist = XCAR (XCDR (start));
      newcache = 1;
      tem = AREF (item_properties, ITEM_PROPERTY_KEYEQ);
      if (!NILP (keyhint))
	{
	  XSETCAR (cachelist, XCAR (keyhint));
	  newcache = 0;
	}
      else if (STRINGP (tem))
	{
	  XSETCDR (cachelist, Fsubstitute_command_keys (tem));
	  XSETCAR (cachelist, Qt);
	}
    }
  
  tem = XCAR (cachelist);
  if (!EQ (tem, Qt))
    {
      int chkcache = 0;
      Lisp_Object prefix;

      if (!NILP (tem))
	tem = Fkey_binding (tem, Qnil, Qnil);

      prefix = AREF (item_properties, ITEM_PROPERTY_KEYEQ);
      if (CONSP (prefix))
	{
	  def = XCAR (prefix);
	  prefix = XCDR (prefix);
	}
      else
	def = AREF (item_properties, ITEM_PROPERTY_DEF);

      if (!update_menu_bindings)
	chkcache = 0;
      else if (NILP (XCAR (cachelist))) /* Have no saved key.  */
	{
	  if (newcache		/* Always check first time.  */
	      /* Should we check everything when precomputing key
		 bindings?  */
	      /* If something had no key binding before, don't recheck it
		 because that is too slow--except if we have a list of
		 rebound commands in Vdefine_key_rebound_commands, do
		 recheck any command that appears in that list. */
	      || (CONSP (Vdefine_key_rebound_commands)
		  && !NILP (Fmemq (def, Vdefine_key_rebound_commands))))
	    chkcache = 1;
	}
      /* We had a saved key. Is it still bound to the command?  */
      else if (NILP (tem)
	       || (!EQ (tem, def)
		   /* If the command is an alias for another
		      (such as lmenu.el set it up), check if the
		      original command matches the cached command.  */
		   && !(SYMBOLP (def) && EQ (tem, XSYMBOL (def)->function))))
	chkcache = 1;		/* Need to recompute key binding.  */

      if (chkcache)
	{
	  /* Recompute equivalent key binding.  If the command is an alias
	     for another (such as lmenu.el set it up), see if the original
	     command name has equivalent keys.  Otherwise look up the
	     specified command itself.  We don't try both, because that
	     makes lmenu menus slow. */
	  if (SYMBOLP (def)
	      && SYMBOLP (XSYMBOL (def)->function)
	      && ! NILP (Fget (def, Qmenu_alias)))
	    def = XSYMBOL (def)->function;
	  tem = Fwhere_is_internal (def, Qnil, Qt, Qnil, Qt);
	  XSETCAR (cachelist, tem);
	  if (NILP (tem))
	    {
	      XSETCDR (cachelist, Qnil);
	      chkcache = 0;
	    }
	}
      else if (!NILP (keyhint) && !NILP (XCAR (cachelist)))
	{
	  tem = XCAR (cachelist);
	  chkcache = 1;
	}

      newcache = chkcache;
      if (chkcache)
	{
	  tem = Fkey_description (tem);
	  if (CONSP (prefix))
	    {
	      if (STRINGP (XCAR (prefix)))
		tem = concat2 (XCAR (prefix), tem);
	      if (STRINGP (XCDR (prefix)))
		tem = concat2 (tem, XCDR (prefix));
	    }
	  XSETCDR (cachelist, tem);
	}
    }

  tem = XCDR (cachelist);
  if (newcache && !NILP (tem))
    {
      tem = concat3 (build_string ("  ("), tem, build_string (")"));
      XSETCDR (cachelist, tem);
    }

  /* If we only want to precompute equivalent key bindings, stop here. */
  if (notreal)
    return 1;

  /* If we have an equivalent key binding, use that.  */
  AREF (item_properties, ITEM_PROPERTY_KEYEQ) = tem;

  /* Include this when menu help is implemented.
  tem = XVECTOR (item_properties)->contents[ITEM_PROPERTY_HELP];
  if (!(NILP (tem) || STRINGP (tem)))
    {
      tem = menu_item_eval_property (tem);
      if (!STRINGP (tem))
	tem = Qnil;
      XVECTOR (item_properties)->contents[ITEM_PROPERTY_HELP] = tem;
    }
  */

  /* Handle radio buttons or toggle boxes.  */ 
  tem = AREF (item_properties, ITEM_PROPERTY_SELECTED);
  if (!NILP (tem))
    AREF (item_properties, ITEM_PROPERTY_SELECTED)
      = menu_item_eval_property (tem);

  return 1;
}



/***********************************************************************
			       Tool-bars
 ***********************************************************************/

/* A vector holding tool bar items while they are parsed in function
   tool_bar_items. Each item occupies TOOL_BAR_ITEM_NSCLOTS elements
   in the vector.  */

static Lisp_Object tool_bar_items_vector;

/* A vector holding the result of parse_tool_bar_item.  Layout is like
   the one for a single item in tool_bar_items_vector.  */

static Lisp_Object tool_bar_item_properties;

/* Next free index in tool_bar_items_vector.  */

static int ntool_bar_items;

/* The symbols `tool-bar', and `:image'.  */

extern Lisp_Object Qtool_bar;
Lisp_Object QCimage;

/* Function prototypes.  */

static void init_tool_bar_items P_ ((Lisp_Object));
static void process_tool_bar_item P_ ((Lisp_Object, Lisp_Object));
static int parse_tool_bar_item P_ ((Lisp_Object, Lisp_Object));
static void append_tool_bar_item P_ ((void));


/* Return a vector of tool bar items for keymaps currently in effect.
   Reuse vector REUSE if non-nil.  Return in *NITEMS the number of
   tool bar items found.  */

Lisp_Object
tool_bar_items (reuse, nitems)
     Lisp_Object reuse;
     int *nitems;
{
  Lisp_Object *maps;
  int nmaps, i;
  Lisp_Object oquit;
  Lisp_Object *tmaps;

  *nitems = 0;

  /* In order to build the menus, we need to call the keymap
     accessors.  They all call QUIT.  But this function is called
     during redisplay, during which a quit is fatal.  So inhibit
     quitting while building the menus.  We do this instead of
     specbind because (1) errors will clear it anyway and (2) this
     avoids risk of specpdl overflow.  */
  oquit = Vinhibit_quit;
  Vinhibit_quit = Qt;
  
  /* Initialize tool_bar_items_vector and protect it from GC.  */
  init_tool_bar_items (reuse);

  /* Build list of keymaps in maps.  Set nmaps to the number of maps
     to process.  */
  
  /* Should overriding-terminal-local-map and overriding-local-map apply?  */
  if (!NILP (Voverriding_local_map_menu_flag))
    {
      /* Yes, use them (if non-nil) as well as the global map.  */
      maps = (Lisp_Object *) alloca (3 * sizeof (maps[0]));
      nmaps = 0;
      if (!NILP (current_kboard->Voverriding_terminal_local_map))
	maps[nmaps++] = current_kboard->Voverriding_terminal_local_map;
      if (!NILP (Voverriding_local_map))
	maps[nmaps++] = Voverriding_local_map;
    }
  else
    {
      /* No, so use major and minor mode keymaps and keymap property.
	 Note that tool-bar bindings in the local-map and keymap
	 properties may not work reliable, as they are only
	 recognized when the tool-bar (or mode-line) is updated,
	 which does not normally happen after every command.  */
      Lisp_Object tem;
      int nminor;
      nminor = current_minor_maps (NULL, &tmaps);
      maps = (Lisp_Object *) alloca ((nminor + 3) * sizeof (maps[0]));
      nmaps = 0;
      if (tem = get_local_map (PT, current_buffer, Qkeymap), !NILP (tem))
	maps[nmaps++] = tem;
      bcopy (tmaps, (void *) (maps + nmaps), nminor * sizeof (maps[0]));
      nmaps += nminor;
      maps[nmaps++] = get_local_map (PT, current_buffer, Qlocal_map);
    }

  /* Add global keymap at the end.  */
  maps[nmaps++] = current_global_map;

  /* Process maps in reverse order and look up in each map the prefix
     key `tool-bar'.  */
  for (i = nmaps - 1; i >= 0; --i)
    if (!NILP (maps[i]))
      {
	Lisp_Object keymap;

	keymap = get_keymap (access_keymap (maps[i], Qtool_bar, 1, 0, 1), 0, 1);
	if (CONSP (keymap))
	  {
	    Lisp_Object tail;
	    
	    /* KEYMAP is a list `(keymap (KEY . BINDING) ...)'.  */
	    for (tail = keymap; CONSP (tail); tail = XCDR (tail))
	      {
		Lisp_Object keydef = XCAR (tail);
		if (CONSP (keydef))
		  process_tool_bar_item (XCAR (keydef), XCDR (keydef));
	      }
	  }
      }

  Vinhibit_quit = oquit;
  *nitems = ntool_bar_items / TOOL_BAR_ITEM_NSLOTS;
  return tool_bar_items_vector;
}


/* Process the definition of KEY which is DEF.  */

static void
process_tool_bar_item (key, def)
     Lisp_Object key, def;
{
  int i;
  extern Lisp_Object Qundefined;
  struct gcpro gcpro1, gcpro2;

  /* Protect KEY and DEF from GC because parse_tool_bar_item may call
     eval.  */
  GCPRO2 (key, def);

  if (EQ (def, Qundefined))
    {
      /* If a map has an explicit `undefined' as definition,
	 discard any previously made item.  */
      for (i = 0; i < ntool_bar_items; i += TOOL_BAR_ITEM_NSLOTS)
	{
	  Lisp_Object *v = XVECTOR (tool_bar_items_vector)->contents + i;
	  
	  if (EQ (key, v[TOOL_BAR_ITEM_KEY]))
	    {
	      if (ntool_bar_items > i + TOOL_BAR_ITEM_NSLOTS)
		bcopy (v + TOOL_BAR_ITEM_NSLOTS, v,
		       ((ntool_bar_items - i - TOOL_BAR_ITEM_NSLOTS)
			* sizeof (Lisp_Object)));
	      ntool_bar_items -= TOOL_BAR_ITEM_NSLOTS;
	      break;
	    }
	}
    }
  else if (parse_tool_bar_item (key, def))
    /* Append a new tool bar item to tool_bar_items_vector.  Accept
       more than one definition for the same key.  */
    append_tool_bar_item ();

  UNGCPRO;
}


/* Parse a tool bar item specification ITEM for key KEY and return the
   result in tool_bar_item_properties.  Value is zero if ITEM is
   invalid.

   ITEM is a list `(menu-item CAPTION BINDING PROPS...)'.
   
   CAPTION is the caption of the item,  If it's not a string, it is
   evaluated to get a string.
   
   BINDING is the tool bar item's binding.  Tool-bar items with keymaps
   as binding are currently ignored.

   The following properties are recognized:

   - `:enable FORM'.
   
   FORM is evaluated and specifies whether the tool bar item is
   enabled or disabled.
   
   - `:visible FORM'
   
   FORM is evaluated and specifies whether the tool bar item is visible.
   
   - `:filter FUNCTION'

   FUNCTION is invoked with one parameter `(quote BINDING)'.  Its
   result is stored as the new binding.
   
   - `:button (TYPE SELECTED)'

   TYPE must be one of `:radio' or `:toggle'.  SELECTED is evaluated
   and specifies whether the button is selected (pressed) or not.
   
   - `:image IMAGES'

   IMAGES is either a single image specification or a vector of four
   image specifications.  See enum tool_bar_item_images.
   
   - `:help HELP-STRING'.
   
   Gives a help string to display for the tool bar item.  */

static int
parse_tool_bar_item (key, item)
     Lisp_Object key, item;
{
  /* Access slot with index IDX of vector tool_bar_item_properties.  */
#define PROP(IDX) XVECTOR (tool_bar_item_properties)->contents[IDX]

  Lisp_Object filter = Qnil;
  Lisp_Object caption;
  int i;

  /* Defininition looks like `(menu-item CAPTION BINDING PROPS...)'.
     Rule out items that aren't lists, don't start with
     `menu-item' or whose rest following `tool-bar-item' is not a
     list.  */
  if (!CONSP (item)
      || !EQ (XCAR (item), Qmenu_item)
      || (item = XCDR (item),
	  !CONSP (item)))
    return 0;

  /* Create tool_bar_item_properties vector if necessary.  Reset it to
     defaults.  */
  if (VECTORP (tool_bar_item_properties))
    {
      for (i = 0; i < TOOL_BAR_ITEM_NSLOTS; ++i)
	PROP (i) = Qnil;
    }
  else
    tool_bar_item_properties
      = Fmake_vector (make_number (TOOL_BAR_ITEM_NSLOTS), Qnil);
  
  /* Set defaults.  */
  PROP (TOOL_BAR_ITEM_KEY) = key;
  PROP (TOOL_BAR_ITEM_ENABLED_P) = Qt;
	 
  /* Get the caption of the item.  If the caption is not a string,
     evaluate it to get a string.  If we don't get a string, skip this
     item.  */
  caption = XCAR (item);
  if (!STRINGP (caption))
    {
      caption = menu_item_eval_property (caption);
      if (!STRINGP (caption))
	return 0;
    }
  PROP (TOOL_BAR_ITEM_CAPTION) = caption;

  /* Give up if rest following the caption is not a list.  */
  item = XCDR (item);
  if (!CONSP (item))
    return 0;

  /* Store the binding.  */
  PROP (TOOL_BAR_ITEM_BINDING) = XCAR (item);
  item = XCDR (item);

  /* Ignore cached key binding, if any.  */
  if (CONSP (item) && CONSP (XCAR (item)))
    item = XCDR (item);

  /* Process the rest of the properties.  */
  for (; CONSP (item) && CONSP (XCDR (item)); item = XCDR (XCDR (item)))
    {
      Lisp_Object key, value;

      key = XCAR (item);
      value = XCAR (XCDR (item));

      if (EQ (key, QCenable))
	/* `:enable FORM'.  */
	PROP (TOOL_BAR_ITEM_ENABLED_P) = value;
      else if (EQ (key, QCvisible))
	{
	  /* `:visible FORM'.  If got a visible property and that
	     evaluates to nil then ignore this item.  */
	  if (NILP (menu_item_eval_property (value)))
	    return 0;
	}
      else if (EQ (key, QChelp))
	/* `:help HELP-STRING'.  */
	PROP (TOOL_BAR_ITEM_HELP) = value;
      else if (EQ (key, QCfilter))
	/* ':filter FORM'.  */
	filter = value;
      else if (EQ (key, QCbutton) && CONSP (value))
	{
	  /* `:button (TYPE . SELECTED)'.  */
	  Lisp_Object type, selected;

	  type = XCAR (value);
	  selected = XCDR (value);
	  if (EQ (type, QCtoggle) || EQ (type, QCradio))
	    {
	      PROP (TOOL_BAR_ITEM_SELECTED_P) = selected;
	      PROP (TOOL_BAR_ITEM_TYPE) = type;
	    }
	}
      else if (EQ (key, QCimage)
	       && (CONSP (value)
		   || (VECTORP (value) && XVECTOR (value)->size == 4)))
	/* Value is either a single image specification or a vector
	   of 4 such specifications for the different button states.  */
	PROP (TOOL_BAR_ITEM_IMAGES) = value;
    }

  /* If got a filter apply it on binding.  */
  if (!NILP (filter))
    PROP (TOOL_BAR_ITEM_BINDING)
      = menu_item_eval_property (list2 (filter,
					list2 (Qquote,
					       PROP (TOOL_BAR_ITEM_BINDING))));

  /* See if the binding is a keymap.  Give up if it is.  */
  if (CONSP (get_keymap (PROP (TOOL_BAR_ITEM_BINDING), 0, 1)))
    return 0;

  /* Enable or disable selection of item.  */
  if (!EQ (PROP (TOOL_BAR_ITEM_ENABLED_P), Qt))
    PROP (TOOL_BAR_ITEM_ENABLED_P)
      = menu_item_eval_property (PROP (TOOL_BAR_ITEM_ENABLED_P));

  /* Handle radio buttons or toggle boxes.  */ 
  if (!NILP (PROP (TOOL_BAR_ITEM_SELECTED_P)))
    PROP (TOOL_BAR_ITEM_SELECTED_P)
      = menu_item_eval_property (PROP (TOOL_BAR_ITEM_SELECTED_P));

  return 1;
  
#undef PROP
}


/* Initialize tool_bar_items_vector.  REUSE, if non-nil, is a vector
   that can be reused.  */

static void
init_tool_bar_items (reuse)
     Lisp_Object reuse;
{
  if (VECTORP (reuse))
    tool_bar_items_vector = reuse;
  else
    tool_bar_items_vector = Fmake_vector (make_number (64), Qnil);
  ntool_bar_items = 0;
}


/* Append parsed tool bar item properties from
   tool_bar_item_properties */

static void
append_tool_bar_item ()
{
  Lisp_Object *to, *from;
  
  /* Enlarge tool_bar_items_vector if necessary.  */
  if (ntool_bar_items + TOOL_BAR_ITEM_NSLOTS
      >= XVECTOR (tool_bar_items_vector)->size)
    {
      Lisp_Object new_vector;
      int old_size = XVECTOR (tool_bar_items_vector)->size;

      new_vector = Fmake_vector (make_number (2 * old_size), Qnil);
      bcopy (XVECTOR (tool_bar_items_vector)->contents,
	     XVECTOR (new_vector)->contents,
	     old_size * sizeof (Lisp_Object));
      tool_bar_items_vector = new_vector;
    }

  /* Append entries from tool_bar_item_properties to the end of
     tool_bar_items_vector.  */
  to = XVECTOR (tool_bar_items_vector)->contents + ntool_bar_items;
  from = XVECTOR (tool_bar_item_properties)->contents;
  bcopy (from, to, TOOL_BAR_ITEM_NSLOTS * sizeof *to);
  ntool_bar_items += TOOL_BAR_ITEM_NSLOTS;
}





/* Read a character using menus based on maps in the array MAPS.
   NMAPS is the length of MAPS.  Return nil if there are no menus in the maps.
   Return t if we displayed a menu but the user rejected it.

   PREV_EVENT is the previous input event, or nil if we are reading
   the first event of a key sequence.

   If USED_MOUSE_MENU is non-null, then we set *USED_MOUSE_MENU to 1
   if we used a mouse menu to read the input, or zero otherwise.  If
   USED_MOUSE_MENU is null, we don't dereference it.

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
      name = Fkeymap_prompt (maps[mapno]);
      if (!NILP (name))
	break;
    }

  /* If we don't have any menus, just read a character normally.  */
  if (!STRINGP (name))
    return Qnil;

#ifdef HAVE_MENUS
  /* If we got to this point via a mouse click,
     use a real menu for mouse selection.  */
  if (EVENT_HAS_PARAMETERS (prev_event)
      && !EQ (XCAR (prev_event), Qmenu_bar)
      && !EQ (XCAR (prev_event), Qtool_bar))
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
	  Lisp_Object tem;

	  record_menu_key (XCAR (value));

	  /* If we got multiple events, unread all but
	     the first.
	     There is no way to prevent those unread events
	     from showing up later in last_nonmenu_event.
	     So turn symbol and integer events into lists,
	     to indicate that they came from a mouse menu,
	     so that when present in last_nonmenu_event
	     they won't confuse things.  */
	  for (tem = XCDR (value); !NILP (tem); tem = XCDR (tem))
	    {
	      record_menu_key (XCAR (tem));
	      if (SYMBOLP (XCAR (tem))
		  || INTEGERP (XCAR (tem)))
		XSETCAR (tem, Fcons (XCAR (tem), Qdisabled));
	    }

	  /* If we got more than one event, put all but the first
	     onto this list to be read later.
	     Return just the first event now.  */
	  Vunread_command_events
	    = nconc2 (XCDR (value), Vunread_command_events);
	  value = XCAR (value);
	}
      else if (NILP (value))
	value = Qt;
      if (used_mouse_menu)
	*used_mouse_menu = 1;
      return value;
    }
#endif /* HAVE_MENUS */
  return Qnil ;
}

/* Buffer in use so far for the minibuf prompts for menu keymaps.
   We make this bigger when necessary, and never free it.  */
static char *read_char_minibuf_menu_text;
/* Size of that buffer.  */
static int read_char_minibuf_menu_width;

static Lisp_Object
read_char_minibuf_menu_prompt (commandflag, nmaps, maps)
     int commandflag ;
     int nmaps;
     Lisp_Object *maps;
{
  int mapno;
  register Lisp_Object name;
  int nlength;
  int width = FRAME_WIDTH (SELECTED_FRAME ()) - 4;
  int idx = -1;
  int nobindings = 1;
  Lisp_Object rest, vector;
  char *menu;

  vector = Qnil;
  name = Qnil;

  if (! menu_prompting)
    return Qnil;

  /* Make sure we have a big enough buffer for the menu text.  */
  if (read_char_minibuf_menu_text == 0)
    {
      read_char_minibuf_menu_width = width + 4;
      read_char_minibuf_menu_text = (char *) xmalloc (width + 4);
    }
  else if (width + 4 > read_char_minibuf_menu_width)
    {
      read_char_minibuf_menu_width = width + 4;
      read_char_minibuf_menu_text
	= (char *) xrealloc (read_char_minibuf_menu_text, width + 4);
    }
  menu = read_char_minibuf_menu_text;

  /* Get the menu name from the first map that has one (a prompt string).  */
  for (mapno = 0; mapno < nmaps; mapno++)
    {
      name = Fkeymap_prompt (maps[mapno]);
      if (!NILP (name))
	break;
    }

  /* If we don't have any menus, just read a character normally.  */
  if (!STRINGP (name))
    return Qnil;

  /* Prompt string always starts with map's prompt, and a space.  */
  strcpy (menu, XSTRING (name)->data);
  nlength = STRING_BYTES (XSTRING (name));
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
      Lisp_Object orig_defn_macro;

      /* Loop over elements of map.  */
      while (i < width)
	{
	  Lisp_Object elt;

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
	      Lisp_Object event, tem;

	      if (idx < 0)
		{
		  event = Fcar_safe (elt); /* alist */
		  elt = Fcdr_safe (elt);
		}
	      else
		{
		  XSETINT (event, idx); /* vector */
		}

	      /* Ignore the element if it has no prompt string.  */
	      if (INTEGERP (event) && parse_menu_item (elt, 0, -1))
		{
		  /* 1 if the char to type matches the string.  */
		  int char_matches;
		  Lisp_Object upcased_event, downcased_event;
		  Lisp_Object desc = Qnil;
		  Lisp_Object s
		    = XVECTOR (item_properties)->contents[ITEM_PROPERTY_NAME];

		  upcased_event = Fupcase (event);
		  downcased_event = Fdowncase (event);
		  char_matches = (XINT (upcased_event) == XSTRING (s)->data[0]
				  || XINT (downcased_event) == XSTRING (s)->data[0]);
		  if (! char_matches)
		    desc = Fsingle_key_description (event, Qnil);

		  tem
		    = XVECTOR (item_properties)->contents[ITEM_PROPERTY_KEYEQ];
		  if (!NILP (tem))
		    /* Insert equivalent keybinding. */
		    s = concat2 (s, tem);

		  tem
		    = XVECTOR (item_properties)->contents[ITEM_PROPERTY_TYPE];
		  if (EQ (tem, QCradio) || EQ (tem, QCtoggle))
		    {
		      /* Insert button prefix. */
		      Lisp_Object selected
			= XVECTOR (item_properties)->contents[ITEM_PROPERTY_SELECTED];
		      if (EQ (tem, QCradio))
			tem = build_string (NILP (selected) ? "(*) " : "( ) ");
		      else
			tem = build_string (NILP (selected) ? "[X] " : "[ ] ");
		      s = concat2 (tem, s);
		    }
		  

		  /* If we have room for the prompt string, add it to this line.
		     If this is the first on the line, always add it.  */
		  if ((XSTRING (s)->size + i + 2
		       + (char_matches ? 0 : XSTRING (desc)->size + 3))
		      < width
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

		      /* If the char to type doesn't match the string's
			 first char, explicitly show what char to type.  */
		      if (! char_matches)
			{
			  /* Add as much of string as fits.  */
			  thiswidth = XSTRING (desc)->size;
			  if (thiswidth + i > width)
			    thiswidth = width - i;
			  bcopy (XSTRING (desc)->data, menu + i, thiswidth);
			  i += thiswidth;
			  strcpy (menu + i, " = ");
			  i += 3;
			}

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
      message2_nolog (menu, strlen (menu), 
		      ! NILP (current_buffer->enable_multibyte_characters));

      /* Make believe its not a keyboard macro in case the help char
	 is pressed.  Help characters are not recorded because menu prompting
	 is not used on replay.
	 */
      orig_defn_macro = current_kboard->defining_kbd_macro;
      current_kboard->defining_kbd_macro = Qnil;
      do
	obj = read_char (commandflag, 0, 0, Qt, 0);
      while (BUFFERP (obj));
      current_kboard->defining_kbd_macro = orig_defn_macro;

      if (!INTEGERP (obj))
	return obj;
      else
	ch = XINT (obj);

      if (! EQ (obj, menu_prompt_more_char)
	  && (!INTEGERP (menu_prompt_more_char)
	      || ! EQ (obj, make_number (Ctl (XINT (menu_prompt_more_char))))))
	{
	  if (!NILP (current_kboard->defining_kbd_macro))
	    store_kbd_macro_char (obj);
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

   NEXT may be the same array as CURRENT.  */

static int
follow_key (key, nmaps, current, defs, next)
     Lisp_Object key;
     Lisp_Object *current, *defs, *next;
     int nmaps;
{
  int i, first_binding;
  int did_meta = 0;

  first_binding = nmaps;
  for (i = nmaps - 1; i >= 0; i--)
    {
      if (! NILP (current[i]))
	{
	  Lisp_Object map;
	  if (did_meta)
	    map = defs[i];
	  else
	    map = current[i];

	  defs[i] = access_keymap (map, key, 1, 0, 1);
	  if (! NILP (defs[i]))
	    first_binding = i;
	}
      else
	defs[i] = Qnil;
    }

  /* Given the set of bindings we've found, produce the next set of maps.  */
  if (first_binding < nmaps)
    for (i = 0; i < nmaps; i++)
      next[i] = NILP (defs[i]) ? Qnil : get_keymap (defs[i], 0, 1);

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
   read_char will return it.

   If FIX_CURRENT_BUFFER is nonzero, we restore current_buffer
   from the selected window's buffer.  */

static int
read_key_sequence (keybuf, bufsize, prompt, dont_downcase_last,
		   can_return_switch_frame, fix_current_buffer)
     Lisp_Object *keybuf;
     int bufsize;
     Lisp_Object prompt;
     int dont_downcase_last;
     int can_return_switch_frame;
     int fix_current_buffer;
{
  volatile Lisp_Object from_string;
  volatile int count = specpdl_ptr - specpdl;

  /* How many keys there are in the current key sequence.  */
  volatile int t;

  /* The length of the echo buffer when we started reading, and
     the length of this_command_keys when we started reading.  */
  volatile int echo_start;
  volatile int keys_start;

  /* The number of keymaps we're scanning right now, and the number of
     keymaps we have allocated space for.  */
  volatile int nmaps;
  volatile int nmaps_allocated = 0;

  /* defs[0..nmaps-1] are the definitions of KEYBUF[0..t-1] in
     the current keymaps.  */
  Lisp_Object *volatile defs = NULL;

  /* submaps[0..nmaps-1] are the prefix definitions of KEYBUF[0..t-1]
     in the current keymaps, or nil where it is not a prefix.  */
  Lisp_Object *volatile submaps = NULL;

  /* The local map to start out with at start of key sequence.  */
  volatile Lisp_Object orig_local_map;

  /* The map from the `keymap' property to start out with at start of
     key sequence.  */
  volatile Lisp_Object orig_keymap;

  /* 1 if we have already considered switching to the local-map property
     of the place where a mouse click occurred.  */
  volatile int localized_local_map = 0;

  /* The index in defs[] of the first keymap that has a binding for
     this key sequence.  In other words, the lowest i such that
     defs[i] is non-nil.  */
  volatile int first_binding;
  /* Index of the first key that has no binding.
     It is useless to try fkey_start larger than that.  */
  volatile int first_unbound;

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
  volatile int mock_input = 0;

  /* If the sequence is unbound in submaps[], then
     keybuf[fkey_start..fkey_end-1] is a prefix in Vfunction_key_map,
     and fkey_map is its binding.

     These might be > t, indicating that all function key scanning
     should hold off until t reaches them.  We do this when we've just
     recognized a function key, to avoid searching for the function
     key's again in Vfunction_key_map.  */
  volatile int fkey_start = 0, fkey_end = 0;
  volatile Lisp_Object fkey_map;

  /* Likewise, for key_translation_map.  */
  volatile int keytran_start = 0, keytran_end = 0;
  volatile Lisp_Object keytran_map;

  /* If we receive a ``switch-frame'' event in the middle of a key sequence,
     we put it off for later.  While we're reading, we keep the event here.  */
  volatile Lisp_Object delayed_switch_frame;

  /* See the comment below... */
#if defined (GOBBLE_FIRST_EVENT)
  Lisp_Object first_event;
#endif

  volatile Lisp_Object original_uppercase;
  volatile int original_uppercase_position = -1;

  /* Gets around Microsoft compiler limitations.  */
  int dummyflag = 0;

  struct buffer *starting_buffer;

  /* Nonzero if we seem to have got the beginning of a binding
     in function_key_map.  */
  volatile int function_key_possible = 0;
  volatile int key_translation_possible = 0;

  /* List of events for which a fake prefix key has been generated.  */
  volatile Lisp_Object fake_prefixed_keys = Qnil;

  /* Save the status of key translation before each step,
     so that we can restore this after downcasing.  */
  Lisp_Object prev_fkey_map;
  int prev_fkey_start;
  int prev_fkey_end;

  Lisp_Object prev_keytran_map;
  int prev_keytran_start;
  int prev_keytran_end;

#if defined (GOBBLE_FIRST_EVENT)
  int junk;
#endif

  struct gcpro gcpro1;

  GCPRO1 (fake_prefixed_keys);
  raw_keybuf_count = 0;

  last_nonmenu_event = Qnil;

  delayed_switch_frame = Qnil;
  fkey_map = Vfunction_key_map;
  keytran_map = Vkey_translation_map;

  /* If there is no function-key-map, turn off function key scanning.  */
  if (!KEYMAPP (Vfunction_key_map))
    fkey_start = fkey_end = bufsize + 1;

  /* If there is no key-translation-map, turn off scanning.  */
  if (!KEYMAPP (Vkey_translation_map))
    keytran_start = keytran_end = bufsize + 1;

  if (INTERACTIVE)
    {
      if (!NILP (prompt))
	echo_prompt (prompt);
      else if (cursor_in_echo_area
	       && (FLOATP (Vecho_keystrokes) || INTEGERP (Vecho_keystrokes))
	       && NILP (Fzerop (Vecho_keystrokes)))
	/* This doesn't put in a dash if the echo buffer is empty, so
	   you don't always see a dash hanging out in the minibuffer.  */
	echo_dash ();
    }

  /* Record the initial state of the echo area and this_command_keys;
     we will need to restore them if we replay a key sequence.  */
  if (INTERACTIVE)
    echo_start = echo_length ();
  keys_start = this_command_key_count;
  this_single_command_key_start = keys_start;

#if defined (GOBBLE_FIRST_EVENT)
  /* This doesn't quite work, because some of the things that read_char
     does cannot safely be bypassed.  It seems too risky to try to make
     this work right.  */

  /* Read the first char of the sequence specially, before setting
     up any keymaps, in case a filter runs and switches buffers on us.  */
  first_event = read_char (NILP (prompt), 0, submaps, last_nonmenu_event,
			   &junk);
#endif /* GOBBLE_FIRST_EVENT */

  orig_local_map = get_local_map (PT, current_buffer, Qlocal_map);
  orig_keymap = get_local_map (PT, current_buffer, Qkeymap);
  from_string = Qnil;

  /* We jump here when the key sequence has been thoroughly changed, and
     we need to rescan it starting from the beginning.  When we jump here,
     keybuf[0..mock_input] holds the sequence we should reread.  */
 replay_sequence:

  starting_buffer = current_buffer;
  function_key_possible = 0;
  key_translation_possible = 0;
  first_unbound = bufsize + 1;

  /* Build our list of keymaps.
     If we recognize a function key and replace its escape sequence in
     keybuf with its symbol, or if the sequence starts with a mouse
     click and we need to switch buffers, we jump back here to rebuild
     the initial keymaps from the current buffer.  */
  nmaps = 0;

  if (!NILP (current_kboard->Voverriding_terminal_local_map)
      || !NILP (Voverriding_local_map))
    {
      if (3 > nmaps_allocated)
	{
	  submaps = (Lisp_Object *) alloca (3 * sizeof (submaps[0]));
	  defs    = (Lisp_Object *) alloca (3 * sizeof (defs[0]));
	  nmaps_allocated = 3;
	}
      if (!NILP (current_kboard->Voverriding_terminal_local_map))
	submaps[nmaps++] = current_kboard->Voverriding_terminal_local_map;
      if (!NILP (Voverriding_local_map))
	submaps[nmaps++] = Voverriding_local_map;
    }
  else
    {
      int nminor;
      int total;
      Lisp_Object *maps;

      nminor = current_minor_maps (0, &maps);
      total = nminor + (!NILP (orig_keymap) ? 3 : 2);

      if (total > nmaps_allocated)
	{
	  submaps = (Lisp_Object *) alloca (total * sizeof (submaps[0]));
	  defs    = (Lisp_Object *) alloca (total * sizeof (defs[0]));
	  nmaps_allocated = total;
	}

      if (!NILP (orig_keymap))
	submaps[nmaps++] = orig_keymap;

      bcopy (maps, (void *) (submaps + nmaps),
	     nminor * sizeof (submaps[0]));

      nmaps += nminor;

      submaps[nmaps++] = orig_local_map;
    }
  submaps[nmaps++] = current_global_map;

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
	     && fkey_start < t)
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
      volatile int last_real_key_start;

      /* These variables are analogous to echo_start and keys_start;
	 while those allow us to restart the entire key sequence,
	 echo_local_start and keys_local_start allow us to throw away
	 just one key.  */
      volatile int echo_local_start, keys_local_start, local_first_binding;


      if (first_unbound < fkey_start && first_unbound < keytran_start)
	{ /* The prefix upto first_unbound has no binding and has
	     no translation left to do either, so we know it's unbound.
	     If we don't stop now, we risk staying here indefinitely
	     (if the user keeps entering fkey or keytran prefixes
	     like C-c ESC ESC ESC ESC ...)  */
	  int i;
	  for (i = first_unbound + 1; i < t; i++)
	    keybuf[i - first_unbound - 1] = keybuf[i];
	  mock_input = t - first_unbound - 1;
	  fkey_end = fkey_start -= first_unbound + 1;
	  fkey_map = Vfunction_key_map;
	  keytran_end = keytran_start -= first_unbound + 1;
	  keytran_map = Vkey_translation_map;
	  goto replay_sequence;
	}

      if (t >= bufsize)
	error ("Key sequence too long");

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
	  if ((FLOATP (Vecho_keystrokes) || INTEGERP (Vecho_keystrokes))
	      && NILP (Fzerop (Vecho_keystrokes)))
	    echo_char (key);
	}

      /* If not, we should actually read a character.  */
      else
	{
	  {
#ifdef MULTI_KBOARD
	    KBOARD *interrupted_kboard = current_kboard;
	    struct frame *interrupted_frame = SELECTED_FRAME ();
	    if (setjmp (wrong_kboard_jmpbuf))
	      {
		if (!NILP (delayed_switch_frame))
		  {
		    interrupted_kboard->kbd_queue
		      = Fcons (delayed_switch_frame,
			       interrupted_kboard->kbd_queue);
		    delayed_switch_frame = Qnil;
		  }
		while (t > 0)
		  interrupted_kboard->kbd_queue
		    = Fcons (keybuf[--t], interrupted_kboard->kbd_queue);

		/* If the side queue is non-empty, ensure it begins with a
		   switch-frame, so we'll replay it in the right context.  */
		if (CONSP (interrupted_kboard->kbd_queue)
		    && (key = XCAR (interrupted_kboard->kbd_queue),
			!(EVENT_HAS_PARAMETERS (key)
			  && EQ (EVENT_HEAD_KIND (EVENT_HEAD (key)),
				 Qswitch_frame))))
		  {
		    Lisp_Object frame;
		    XSETFRAME (frame, interrupted_frame);
		    interrupted_kboard->kbd_queue
		      = Fcons (make_lispy_switch_frame (frame),
			       interrupted_kboard->kbd_queue);
		  }
		mock_input = 0;
		orig_local_map = get_local_map (PT, current_buffer, Qlocal_map);
		orig_keymap = get_local_map (PT, current_buffer, Qkeymap);
		goto replay_sequence;
	      }
#endif
	    key = read_char (NILP (prompt), nmaps,
			     (Lisp_Object *) submaps, last_nonmenu_event,
			     &used_mouse_menu);
	  }

	  /* read_char returns t when it shows a menu and the user rejects it.
	     Just return -1.  */
	  if (EQ (key, Qt))
	    {
	      unbind_to (count, Qnil);
	      UNGCPRO;
	      return -1;
	    }

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
	      /* Reset the current buffer from the selected window
		 in case something changed the former and not the latter.
		 This is to be more consistent with the behavior
		 of the command_loop_1.  */
	      if (fix_current_buffer)
		{
		  if (! FRAME_LIVE_P (XFRAME (selected_frame)))
		    Fkill_emacs (Qnil);
		  if (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer)
		    Fset_buffer (XWINDOW (selected_window)->buffer);
		}

	      orig_local_map = get_local_map (PT, current_buffer, Qlocal_map);
	      orig_keymap = get_local_map (PT, current_buffer, Qkeymap);
	      goto replay_sequence;
	    }

	  /* If we have a quit that was typed in another frame, and
	     quit_throw_to_read_char switched buffers,
	     replay to get the right keymap.  */
	  if (INTEGERP (key)
	      && XINT (key) == quit_char
	      && current_buffer != starting_buffer)
	    {
	      GROW_RAW_KEYBUF;
	      XVECTOR (raw_keybuf)->contents[raw_keybuf_count++] = key;
	      keybuf[t++] = key;
	      mock_input = t;
	      Vquit_flag = Qnil;
	      orig_local_map = get_local_map (PT, current_buffer, Qlocal_map);
	      orig_keymap = get_local_map (PT, current_buffer, Qkeymap);
	      goto replay_sequence;
	    }

	  Vquit_flag = Qnil;

	  if (EVENT_HAS_PARAMETERS (key)
	      && EQ (EVENT_HEAD_KIND (EVENT_HEAD (key)), Qswitch_frame))
	    {
	      /* If we're at the beginning of a key sequence, and the caller
		 says it's okay, go ahead and return this event.  If we're
		 in the midst of a key sequence, delay it until the end. */
	      if (t > 0 || !can_return_switch_frame)
		{
		  delayed_switch_frame = key;
		  goto replay_key;
		}
	    }

	  GROW_RAW_KEYBUF;
	  XVECTOR (raw_keybuf)->contents[raw_keybuf_count++] = key;
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

	      if (CONSP (posn)
		  || (!NILP (fake_prefixed_keys)
		      && !NILP (Fmemq (key, fake_prefixed_keys))))
		{
		  /* We're looking a second time at an event for which
		     we generated a fake prefix key.  Set
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
		  XVECTOR (raw_keybuf)->contents[raw_keybuf_count++] = key;
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

		  if (! FRAME_LIVE_P (XFRAME (selected_frame)))
		    Fkill_emacs (Qnil);
		  set_buffer_internal (XBUFFER (XWINDOW (window)->buffer));
		  orig_local_map = get_local_map (PT, current_buffer,
						  Qlocal_map);
		  orig_keymap = get_local_map (PT, current_buffer, Qkeymap);
		  goto replay_sequence;
		}
	      
	      /* For a mouse click, get the local text-property keymap
		 of the place clicked on, rather than point.  */
	      if (last_real_key_start == 0
		  && CONSP (XCDR (key))
		  && ! localized_local_map)
		{
		  Lisp_Object map_here, start, pos;

		  localized_local_map = 1;
		  start = EVENT_START (key);
		  
		  if (CONSP (start) && CONSP (XCDR (start)))
		    {
		      pos = POSN_BUFFER_POSN (start);
		      if (INTEGERP (pos)
			  && XINT (pos) >= BEG && XINT (pos) <= Z)
			{
			  map_here = get_local_map (XINT (pos),
						    current_buffer, Qlocal_map);
			  if (!EQ (map_here, orig_local_map))
			    {
			      orig_local_map = map_here;
			      keybuf[t] = key;
			      mock_input = t + 1;

			      goto replay_sequence;
			    }
			  map_here = get_local_map (XINT (pos),
						     current_buffer, Qkeymap);
			  if (!EQ (map_here, orig_keymap))
			    {
			      orig_keymap = map_here;
			      keybuf[t] = key;
			      mock_input = t + 1;

			      goto replay_sequence;
			    }
			}
		    }
		}

	      /* Expand mode-line and scroll-bar events into two events:
		 use posn as a fake prefix key.  */
	      if (SYMBOLP (posn)
		  && (NILP (fake_prefixed_keys)
		      || NILP (Fmemq (key, fake_prefixed_keys))))
		{
		  if (t + 1 >= bufsize)
		    error ("Key sequence too long");
		  
		  keybuf[t]     = posn;
		  keybuf[t + 1] = key;
		  mock_input    = t + 2;

		  /* Record that a fake prefix key has been generated
		     for KEY.  Don't modify the event; this would
		     prevent proper action when the event is pushed
		     back tino unread-command-events.  */
		  fake_prefixed_keys = Fcons (key, fake_prefixed_keys);

		  /* If on a mode line string with a local keymap,
		     reconsider the key sequence with that keymap.  */
		  if (CONSP (POSN_STRING (EVENT_START (key))))
		    {
		      Lisp_Object string, pos, map, map2;

		      string = POSN_STRING (EVENT_START (key));
		      pos = XCDR (string);
		      string = XCAR (string);
                      if (XINT (pos) >= 0
			  && XINT (pos) < XSTRING (string)->size)
                        {
                          map = Fget_text_property (pos, Qlocal_map, string);
                          if (!NILP (map))
                            orig_local_map = map;
                          map2 = Fget_text_property (pos, Qkeymap, string);
                          if (!NILP (map2))
                            orig_keymap = map2;
                          if (!NILP (map) || !NILP (map2))
                            goto replay_sequence;
                        }
		    }

		  goto replay_key;
		}
	      else if (CONSP (POSN_STRING (EVENT_START (key)))
		       && NILP (from_string))
		{
		  /* For a click on a string, i.e. overlay string or a
		     string displayed via the `display' property,
		     consider `local-map' and `keymap' properties of
		     that string.  */
		  Lisp_Object string, pos, map, map2;

		  string = POSN_STRING (EVENT_START (key));
		  pos = XCDR (string);
		  string = XCAR (string);
		  if (XINT (pos) >= 0
		      && XINT (pos) < XSTRING (string)->size)
		    {
		      map = Fget_text_property (pos, Qlocal_map, string);
		      if (!NILP (map))
			orig_local_map = map;
		      map2 = Fget_text_property (pos, Qkeymap, string);
		      if (!NILP (map2))
			orig_keymap = map2;

		      if (!NILP (map) || !NILP (map2))
			{
			  from_string = string;
			  goto replay_sequence;
			}
		    }
		}
	    }
	  else if (CONSP (XCDR (key))
		   && CONSP (EVENT_START (key))
		   && CONSP (XCDR (EVENT_START (key))))
	    {
	      Lisp_Object posn;

	      posn = POSN_BUFFER_POSN (EVENT_START (key));
	      /* Handle menu-bar events:
		 insert the dummy prefix event `menu-bar'.  */
	      if (EQ (posn, Qmenu_bar) || EQ (posn, Qtool_bar))
		{
		  if (t + 1 >= bufsize)
		    error ("Key sequence too long");
		  keybuf[t] = posn;
		  keybuf[t+1] = key;

		  /* Zap the position in key, so we know that we've
		     expanded it, and don't try to do so again.  */
		  POSN_BUFFER_SET_POSN (EVENT_START (key),
					Fcons (posn, Qnil));

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
	  
	  /* Remember the position to put an upper bound on fkey_start.  */
	  first_unbound = min (t, first_unbound);

	  head = EVENT_HEAD (key);
	  if (help_char_p (head) && t > 0)
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
	      modifiers = XINT (XCAR (XCDR (breakdown)));
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
			= apply_modifiers (modifiers, XCAR (breakdown));
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

      /* Record what part of this_command_keys is the current key sequence.  */
      this_single_command_key_start = this_command_key_count - t;

      prev_fkey_map = fkey_map;
      prev_fkey_start = fkey_start;
      prev_fkey_end = fkey_end;

      prev_keytran_map = keytran_map;
      prev_keytran_start = keytran_start;
      prev_keytran_end = keytran_end;

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
	      fkey_next
		= access_keymap (fkey_map, key, 1, 0, 1);

	      /* Handle symbol with autoload definition.  */
	      if (SYMBOLP (fkey_next) && ! NILP (Ffboundp (fkey_next))
		  && CONSP (XSYMBOL (fkey_next)->function)
		  && EQ (XCAR (XSYMBOL (fkey_next)->function), Qautoload))
		do_autoload (XSYMBOL (fkey_next)->function,
			     fkey_next);

	      /* Handle a symbol whose function definition is a keymap
		 or an array.  */
	      if (SYMBOLP (fkey_next) && ! NILP (Ffboundp (fkey_next))
		  && (!NILP (Farrayp (XSYMBOL (fkey_next)->function))
		      || KEYMAPP (XSYMBOL (fkey_next)->function)))
		fkey_next = XSYMBOL (fkey_next)->function;

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
		    error ("Function in key-translation-map returns invalid key sequence");
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
		    error ("Key sequence too long");

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

		  /* Do pass the results through key-translation-map.
		     But don't retranslate what key-translation-map
		     has already translated.  */
		  keytran_end = keytran_start;
		  keytran_map = Vkey_translation_map;

		  goto replay_sequence;
		}

	      fkey_map = get_keymap (fkey_next, 0, 1);

	      /* If we no longer have a bound suffix, try a new positions for
		 fkey_start.  */
	      if (!CONSP (fkey_map))
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
	    keytran_next
	      = access_keymap (keytran_map, key, 1, 0, 1);

	    /* Handle symbol with autoload definition.  */
	    if (SYMBOLP (keytran_next) && ! NILP (Ffboundp (keytran_next))
		&& CONSP (XSYMBOL (keytran_next)->function)
		&& EQ (XCAR (XSYMBOL (keytran_next)->function), Qautoload))
	      do_autoload (XSYMBOL (keytran_next)->function,
			   keytran_next);

	    /* Handle a symbol whose function definition is a keymap
	       or an array.  */
	    if (SYMBOLP (keytran_next) && ! NILP (Ffboundp (keytran_next))
		&& (!NILP (Farrayp (XSYMBOL (keytran_next)->function))
		    || KEYMAPP (XSYMBOL (keytran_next)->function)))
	      keytran_next = XSYMBOL (keytran_next)->function;
	    
	    /* If the key translation map gives a function, not an
	       array, then call the function with one arg and use
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
		  error ("Key sequence too long");

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
		fkey_map = Vfunction_key_map;

		goto replay_sequence;
	      }

	    keytran_map = get_keymap (keytran_next, 0, 1);

	    /* If we no longer have a bound suffix, try a new positions for
	       keytran_start.  */
	    if (!CONSP (keytran_map))
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
		< XCHAR_TABLE (current_buffer->downcase_table)->size)
	       && UPPERCASEP (XINT (key) & 0x3ffff))
	      || (XINT (key) & shift_modifier)))
	{
	  Lisp_Object new_key;

	  original_uppercase = key;
	  original_uppercase_position = t - 1;

	  if (XINT (key) & shift_modifier)
	    XSETINT (new_key, XINT (key) & ~shift_modifier);
	  else
	    XSETINT (new_key, (DOWNCASE (XINT (key) & 0x3ffff)
			       | (XINT (key) & ~0x3ffff)));

	  /* We have to do this unconditionally, regardless of whether
	     the lower-case char is defined in the keymaps, because they
	     might get translated through function-key-map.  */
	  keybuf[t - 1] = new_key;
	  mock_input = t;

	  fkey_map = prev_fkey_map;
	  fkey_start = prev_fkey_start;
	  fkey_end = prev_fkey_end;

	  keytran_map = prev_keytran_map;
	  keytran_start = prev_keytran_start;
	  keytran_end = prev_keytran_end;

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

	  breakdown = parse_modifiers (key);
	  modifiers = XINT (XCAR (XCDR (breakdown)));
	  if (modifiers & shift_modifier)
	    {
	      Lisp_Object new_key;

	      original_uppercase = key;
	      original_uppercase_position = t - 1;

	      modifiers &= ~shift_modifier;
	      new_key = apply_modifiers (modifiers,
					 XCAR (breakdown));

	      keybuf[t - 1] = new_key;
	      mock_input = t;

	      fkey_map = prev_fkey_map;
	      fkey_start = prev_fkey_start;
	      fkey_end = prev_fkey_end;

	      keytran_map = prev_keytran_map;
	      keytran_start = prev_keytran_start;
	      keytran_end = prev_keytran_end;

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

  /* Don't downcase the last character if the caller says don't.
     Don't downcase it if the result is undefined, either.  */
  if ((dont_downcase_last || first_binding >= nmaps)
      && t - 1 == original_uppercase_position)
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
      if ((FLOATP (Vecho_keystrokes) || INTEGERP (Vecho_keystrokes))
	  && NILP (Fzerop (Vecho_keystrokes)))
	echo_char (keybuf[t]);
      add_command_key (keybuf[t]);
    }

  

  UNGCPRO;
  return t;
}

DEFUN ("read-key-sequence", Fread_key_sequence, Sread_key_sequence, 1, 5, 0,
       doc: /* Read a sequence of keystrokes and return as a string or vector.
The sequence is sufficient to specify a non-prefix command in the
current local and global maps.

First arg PROMPT is a prompt string.  If nil, do not prompt specially.
Second (optional) arg CONTINUE-ECHO, if non-nil, means this key echos
as a continuation of the previous key.

The third (optional) arg DONT-DOWNCASE-LAST, if non-nil, means do not
convert the last event to lower case.  (Normally any upper case event
is converted to lower case if the original event is undefined and the lower
case equivalent is defined.)  A non-nil value is appropriate for reading
a key sequence to be defined.

A C-g typed while in this function is treated like any other character,
and `quit-flag' is not set.

If the key sequence starts with a mouse click, then the sequence is read
using the keymaps of the buffer of the window clicked in, not the buffer
of the selected window as normal.

`read-key-sequence' drops unbound button-down events, since you normally
only care about the click or drag events which follow them.  If a drag
or multi-click event is unbound, but the corresponding click event would
be bound, `read-key-sequence' turns the event into a click event at the
drag's starting position.  This means that you don't have to distinguish
between click and drag, double, or triple events unless you want to.

`read-key-sequence' prefixes mouse events on mode lines, the vertical
lines separating windows, and scroll bars with imaginary keys
`mode-line', `vertical-line', and `vertical-scroll-bar'.

Optional fourth argument CAN-RETURN-SWITCH-FRAME non-nil means that this
function will process a switch-frame event if the user switches frames
before typing anything.  If the user switches frames in the middle of a
key sequence, or at the start of the sequence but CAN-RETURN-SWITCH-FRAME
is nil, then the event will be put off until after the current key sequence.

`read-key-sequence' checks `function-key-map' for function key
sequences, where they wouldn't conflict with ordinary bindings.  See
`function-key-map' for more details.

The optional fifth argument COMMAND-LOOP, if non-nil, means
that this key sequence is being read by something that will
read commands one after another.  It should be nil if the caller
will read just one key sequence.  */)
  (prompt, continue_echo, dont_downcase_last, can_return_switch_frame,
   command_loop)
     Lisp_Object prompt, continue_echo, dont_downcase_last;
     Lisp_Object can_return_switch_frame, command_loop;
{
  Lisp_Object keybuf[30];
  register int i;
  struct gcpro gcpro1;
  int count = specpdl_ptr - specpdl;

  if (!NILP (prompt))
    CHECK_STRING (prompt);
  QUIT;

  specbind (Qinput_method_exit_on_first_char,
	    (NILP (command_loop) ? Qt : Qnil));
  specbind (Qinput_method_use_echo_area,
	    (NILP (command_loop) ? Qt : Qnil));

  bzero (keybuf, sizeof keybuf);
  GCPRO1 (keybuf[0]);
  gcpro1.nvars = (sizeof keybuf/sizeof (keybuf[0]));

  if (NILP (continue_echo))
    {
      this_command_key_count = 0;
      this_single_command_key_start = 0;
    }

#ifdef HAVE_X_WINDOWS
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  i = read_key_sequence (keybuf, (sizeof keybuf/sizeof (keybuf[0])),
			 prompt, ! NILP (dont_downcase_last),
			 ! NILP (can_return_switch_frame), 0);

#if 0  /* The following is fine for code reading a key sequence and
	  then proceeding with a lenghty computation, but it's not good
	  for code reading keys in a loop, like an input method.  */
#ifdef HAVE_X_WINDOWS
  if (display_hourglass_p)
    start_hourglass ();
#endif
#endif

  if (i == -1)
    {
      Vquit_flag = Qt;
      QUIT;
    }
  UNGCPRO;
  return unbind_to (count, make_event_array (i, keybuf));
}

DEFUN ("read-key-sequence-vector", Fread_key_sequence_vector,
       Sread_key_sequence_vector, 1, 5, 0,
       doc: /* Like `read-key-sequence' but always return a vector.  */)
     (prompt, continue_echo, dont_downcase_last, can_return_switch_frame,
      command_loop)
     Lisp_Object prompt, continue_echo, dont_downcase_last;
     Lisp_Object can_return_switch_frame, command_loop;
{
  Lisp_Object keybuf[30];
  register int i;
  struct gcpro gcpro1;
  int count = specpdl_ptr - specpdl;

  if (!NILP (prompt))
    CHECK_STRING (prompt);
  QUIT;

  specbind (Qinput_method_exit_on_first_char,
	    (NILP (command_loop) ? Qt : Qnil));
  specbind (Qinput_method_use_echo_area,
	    (NILP (command_loop) ? Qt : Qnil));

  bzero (keybuf, sizeof keybuf);
  GCPRO1 (keybuf[0]);
  gcpro1.nvars = (sizeof keybuf/sizeof (keybuf[0]));

  if (NILP (continue_echo))
    {
      this_command_key_count = 0;
      this_single_command_key_start = 0;
    }

#ifdef HAVE_X_WINDOWS
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  i = read_key_sequence (keybuf, (sizeof keybuf/sizeof (keybuf[0])),
			 prompt, ! NILP (dont_downcase_last),
			 ! NILP (can_return_switch_frame), 0);

#ifdef HAVE_X_WINDOWS
  if (display_hourglass_p)
    start_hourglass ();
#endif

  if (i == -1)
    {
      Vquit_flag = Qt;
      QUIT;
    }
  UNGCPRO;
  return unbind_to (count, Fvector (i, keybuf));
}

DEFUN ("command-execute", Fcommand_execute, Scommand_execute, 1, 4, 0,
       doc: /* Execute CMD as an editor command.
CMD must be a symbol that satisfies the `commandp' predicate.
Optional second arg RECORD-FLAG non-nil
means unconditionally put this command in `command-history'.
Otherwise, that is done only if an arg is read using the minibuffer.
The argument KEYS specifies the value to use instead of (this-command-keys)
when reading the arguments; if it is nil, (this-command-keys) is used.
The argument SPECIAL, if non-nil, means that this command is executing
a special event, so ignore the prefix argument and don't clear it.  */)
     (cmd, record_flag, keys, special)
     Lisp_Object cmd, record_flag, keys, special;
{
  register Lisp_Object final;
  register Lisp_Object tem;
  Lisp_Object prefixarg;
  struct backtrace backtrace;
  extern int debug_on_next_call;

  debug_on_next_call = 0;

  if (NILP (special))
    {
      prefixarg = current_kboard->Vprefix_arg;
      Vcurrent_prefix_arg = prefixarg;
      current_kboard->Vprefix_arg = Qnil;
    }
  else
    prefixarg = Qnil;

  if (SYMBOLP (cmd))
    {
      tem = Fget (cmd, Qdisabled);
      if (!NILP (tem) && !NILP (Vrun_hooks))
	{
	  tem = Fsymbol_value (Qdisabled_command_hook);
	  if (!NILP (tem))
	    return call1 (Vrun_hooks, Qdisabled_command_hook);
	}
    }

  while (1)
    {
      final = Findirect_function (cmd);

      if (CONSP (final) && (tem = Fcar (final), EQ (tem, Qautoload)))
	{
	  struct gcpro gcpro1, gcpro2;

	  GCPRO2 (cmd, prefixarg);
	  do_autoload (final, cmd);
	  UNGCPRO;
	}
      else
	break;
    }

  if (STRINGP (final) || VECTORP (final))
    {
      /* If requested, place the macro in the command history.  For
	 other sorts of commands, call-interactively takes care of
	 this.  */
      if (!NILP (record_flag))
	{
	  Vcommand_history
	    = Fcons (Fcons (Qexecute_kbd_macro,
			    Fcons (final, Fcons (prefixarg, Qnil))),
		     Vcommand_history);

	  /* Don't keep command history around forever.  */
	  if (NUMBERP (Vhistory_length) && XINT (Vhistory_length) > 0)
	    {
	      tem = Fnthcdr (Vhistory_length, Vcommand_history);
	      if (CONSP (tem))
		XSETCDR (tem, Qnil);
	    }
	}

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

      tem = Fcall_interactively (cmd, record_flag, keys);

      backtrace_list = backtrace.next;
      return tem;
    }
  return Qnil;
}



DEFUN ("execute-extended-command", Fexecute_extended_command, Sexecute_extended_command,
       1, 1, "P",
       doc: /* Read function name, then read its arguments and call it.  */)
     (prefixarg)
     Lisp_Object prefixarg;
{
  Lisp_Object function;
  char buf[40];
  int saved_last_point_position;
  Lisp_Object saved_keys, saved_last_point_position_buffer;
  Lisp_Object bindings, value;
  struct gcpro gcpro1, gcpro2, gcpro3;

  saved_keys = Fvector (this_command_key_count,
			XVECTOR (this_command_keys)->contents);
  saved_last_point_position_buffer = last_point_position_buffer;
  saved_last_point_position = last_point_position;
  buf[0] = 0;
  GCPRO3 (saved_keys, prefixarg, saved_last_point_position_buffer);

  if (EQ (prefixarg, Qminus))
    strcpy (buf, "- ");
  else if (CONSP (prefixarg) && XINT (XCAR (prefixarg)) == 4)
    strcpy (buf, "C-u ");
  else if (CONSP (prefixarg) && INTEGERP (XCAR (prefixarg)))
    {
      if (sizeof (int) == sizeof (EMACS_INT))
	sprintf (buf, "%d ", XINT (XCAR (prefixarg)));
      else if (sizeof (long) == sizeof (EMACS_INT))
	sprintf (buf, "%ld ", (long) XINT (XCAR (prefixarg)));
      else
	abort ();
    }
  else if (INTEGERP (prefixarg))
    {
      if (sizeof (int) == sizeof (EMACS_INT))
	sprintf (buf, "%d ", XINT (prefixarg));
      else if (sizeof (long) == sizeof (EMACS_INT))
	sprintf (buf, "%ld ", (long) XINT (prefixarg));
      else
	abort ();
    }

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
			       Qt, Qnil, Qextended_command_history, Qnil,
			       Qnil);

  if (STRINGP (function) && XSTRING (function)->size == 0)
    error ("No command name given");

  /* Set this_command_keys to the concatenation of saved_keys and
     function, followed by a RET.  */
  {
    struct Lisp_String *str;
    Lisp_Object *keys;
    int i;

    this_command_key_count = 0;
    this_single_command_key_start = 0;

    keys = XVECTOR (saved_keys)->contents;
    for (i = 0; i < XVECTOR (saved_keys)->size; i++)
      add_command_key (keys[i]);

    str = XSTRING (function);
    for (i = 0; i < str->size; i++)
      add_command_key (Faref (function, make_number (i)));

    add_command_key (make_number ('\015'));
  }

  last_point_position = saved_last_point_position;
  last_point_position_buffer = saved_last_point_position_buffer;

  UNGCPRO;

  function = Fintern (function, Qnil);
  current_kboard->Vprefix_arg = prefixarg;
  Vthis_command = function;
  real_this_command = function;

  /* If enabled, show which key runs this command.  */
  if (!NILP (Vsuggest_key_bindings)
      && NILP (Vexecuting_macro)
      && SYMBOLP (function))
    bindings = Fwhere_is_internal (function, Voverriding_local_map,
				   Qt, Qnil, Qnil);
  else
    bindings = Qnil;

  value = Qnil;
  GCPRO2 (bindings, value);
  value = Fcommand_execute (function, Qt, Qnil, Qnil);

  /* If the command has a key binding, print it now.  */
  if (!NILP (bindings)
      && ! (VECTORP (bindings) && EQ (Faref (bindings, make_number (0)),
				      Qmouse_movement)))
    {
      /* But first wait, and skip the message if there is input.  */
      int delay_time;
      if (!NILP (echo_area_buffer[0]))
	/* This command displayed something in the echo area;
	   so wait a few seconds, then display our suggestion message.  */
	delay_time = (NUMBERP (Vsuggest_key_bindings)
		      ? XINT (Vsuggest_key_bindings) : 2);
      else
	/* This command left the echo area empty,
	   so display our message immediately.  */
	delay_time = 0;

      if (!NILP (Fsit_for (make_number (delay_time), Qnil, Qnil))
	  && ! CONSP (Vunread_command_events))
	{
	  Lisp_Object binding;
	  char *newmessage;
	  int message_p = push_message ();
	  int count = BINDING_STACK_SIZE ();

	  record_unwind_protect (push_message_unwind, Qnil);
	  binding = Fkey_description (bindings);

	  newmessage
	    = (char *) alloca (XSTRING (SYMBOL_NAME (function))->size
			       + STRING_BYTES (XSTRING (binding))
			       + 100);
	  sprintf (newmessage, "You can run the command `%s' with %s",
		   XSTRING (SYMBOL_NAME (function))->data,
		   XSTRING (binding)->data);
	  message2_nolog (newmessage,
			  strlen (newmessage),
			  STRING_MULTIBYTE (binding));
	  if (!NILP (Fsit_for ((NUMBERP (Vsuggest_key_bindings)
				? Vsuggest_key_bindings : make_number (2)),
			       Qnil, Qnil))
	      && message_p)
	    restore_message ();

	  unbind_to (count, Qnil);
	}
    }

  RETURN_UNGCPRO (value);
}


/* Return nonzero if input events are pending.  */

int
detect_input_pending ()
{
  if (!input_pending)
    get_input_pending (&input_pending, 0);

  return input_pending;
}

/* Return nonzero if input events are pending, and run any pending timers.  */

int
detect_input_pending_run_timers (do_display)
     int do_display;
{
  int old_timers_run = timers_run;

  if (!input_pending)
    get_input_pending (&input_pending, 1);

  if (old_timers_run != timers_run && do_display)
    {
      redisplay_preserve_echo_area (8);
      /* The following fixes a bug when using lazy-lock with
	 lazy-lock-defer-on-the-fly set to t, i.e.  when fontifying
	 from an idle timer function.  The symptom of the bug is that
	 the cursor sometimes doesn't become visible until the next X
	 event is processed.  --gerd.  */
      if (rif)
	rif->flush_display (NULL);
    }

  return input_pending;
}

/* This is called in some cases before a possible quit.
   It cases the next call to detect_input_pending to recompute input_pending.
   So calling this function unnecessarily can't do any harm.  */

void
clear_input_pending ()
{
  input_pending = 0;
}

/* Return nonzero if there are pending requeued events.
   This isn't used yet.  The hope is to make wait_reading_process_input
   call it, and return if it runs Lisp code that unreads something.
   The problem is, kbd_buffer_get_event needs to be fixed to know what
   to do in that case.  It isn't trivial.  */

int
requeued_events_pending_p ()
{
  return (!NILP (Vunread_command_events) || unread_command_char != -1);
}


DEFUN ("input-pending-p", Finput_pending_p, Sinput_pending_p, 0, 0, 0,
       doc: /* Return t if command input is currently available with no wait.
Actually, the value is nil only if we can be sure that no input is available;
if there is a doubt, the value is t.  */)
     ()
{
  if (!NILP (Vunread_command_events) || unread_command_char != -1)
    return (Qt);

  get_input_pending (&input_pending, 1);
  return input_pending > 0 ? Qt : Qnil;
}

DEFUN ("recent-keys", Frecent_keys, Srecent_keys, 0, 0, 0,
       doc: /* Return vector of last 100 events, not counting those from keyboard macros.  */)
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
       doc: /* Return the key sequence that invoked this command.
However, if the command has called `read-key-sequence', it returns
the last key sequence that has been read.
The value is a string or a vector.  */)
     ()
{
  return make_event_array (this_command_key_count,
			   XVECTOR (this_command_keys)->contents);
}

DEFUN ("this-command-keys-vector", Fthis_command_keys_vector, Sthis_command_keys_vector, 0, 0, 0,
       doc: /* Return the key sequence that invoked this command, as a vector.
However, if the command has called `read-key-sequence', it returns
the last key sequence that has been read.  */)
     ()
{
  return Fvector (this_command_key_count,
		  XVECTOR (this_command_keys)->contents);
}

DEFUN ("this-single-command-keys", Fthis_single_command_keys,
       Sthis_single_command_keys, 0, 0, 0,
       doc: /* Return the key sequence that invoked this command.
More generally, it returns the last key sequence read, either by
the command loop or by `read-key-sequence'.
Unlike `this-command-keys', this function's value
does not include prefix arguments.
The value is always a vector.  */)
     ()
{
  return Fvector (this_command_key_count
		  - this_single_command_key_start,
		  (XVECTOR (this_command_keys)->contents
		   + this_single_command_key_start));
}

DEFUN ("this-single-command-raw-keys", Fthis_single_command_raw_keys,
       Sthis_single_command_raw_keys, 0, 0, 0,
       doc: /* Return the raw events that were read for this command.
More generally, it returns the last key sequence read, either by
the command loop or by `read-key-sequence'.
Unlike `this-single-command-keys', this function's value
shows the events before all translations (except for input methods).
The value is always a vector.  */)
     ()
{
  return Fvector (raw_keybuf_count,
		  (XVECTOR (raw_keybuf)->contents));
}

DEFUN ("reset-this-command-lengths", Freset_this_command_lengths,
       Sreset_this_command_lengths, 0, 0, 0,
       doc: /* Used for complicated reasons in `universal-argument-other-key'.

`universal-argument-other-key' rereads the event just typed.
It then gets translated through `function-key-map'.
The translated event gets included in the echo area and in
the value of `this-command-keys' in addition to the raw original event.
That is not right.

Calling this function directs the translated event to replace
the original event, so that only one version of the event actually
appears in the echo area and in the value of `this-command-keys'.  */)
     ()
{
  before_command_restore_flag = 1;
  before_command_key_count_1 = before_command_key_count;
  before_command_echo_length_1 = before_command_echo_length;
  return Qnil;
}

DEFUN ("clear-this-command-keys", Fclear_this_command_keys,
       Sclear_this_command_keys, 0, 0, 0,
       doc: /* Clear out the vector that `this-command-keys' returns.
Also clear the record of the last 100 events.  */)
     ()
{
  int i;
  
  this_command_key_count = 0;

  for (i = 0; i < XVECTOR (recent_keys)->size; ++i)
    XVECTOR (recent_keys)->contents[i] = Qnil;
  total_keys = 0;
  recent_keys_index = 0;
  return Qnil;
}

DEFUN ("recursion-depth", Frecursion_depth, Srecursion_depth, 0, 0, 0,
       doc: /* Return the current depth in recursive edits.  */)
     ()
{
  Lisp_Object temp;
  XSETFASTINT (temp, command_loop_level + minibuf_level);
  return temp;
}

DEFUN ("open-dribble-file", Fopen_dribble_file, Sopen_dribble_file, 1, 1,
       "FOpen dribble file: ",
       doc: /* Start writing all keyboard characters to a dribble file called FILE.
If FILE is nil, close any open dribble file.  */)
     (file)
     Lisp_Object file;
{
  if (dribble)
    {
      fclose (dribble);
      dribble = 0;
    }
  if (!NILP (file))
    {
      file = Fexpand_file_name (file, Qnil);
      dribble = fopen (XSTRING (file)->data, "w");
      if (dribble == 0)
	report_file_error ("Opening dribble", Fcons (file, Qnil));
    }
  return Qnil;
}

DEFUN ("discard-input", Fdiscard_input, Sdiscard_input, 0, 0, 0,
       doc: /* Discard the contents of the terminal input buffer.
Also cancel any kbd macro being defined.  */)
     ()
{
  current_kboard->defining_kbd_macro = Qnil;
  update_mode_lines++;

  Vunread_command_events = Qnil;
  unread_command_char = -1;

  discard_tty_input ();

  kbd_fetch_ptr =  kbd_store_ptr;
  Ffillarray (kbd_buffer_gcpro, Qnil);
  input_pending = 0;

  return Qnil;
}

DEFUN ("suspend-emacs", Fsuspend_emacs, Ssuspend_emacs, 0, 1, "",
       doc: /* Stop Emacs and return to superior process.  You can resume later.
If `cannot-suspend' is non-nil, or if the system doesn't support job
control, run a subshell instead.

If optional arg STUFFSTRING is non-nil, its characters are stuffed
to be read as terminal input by Emacs's parent, after suspension.

Before suspending, run the normal hook `suspend-hook'.
After resumption run the normal hook `suspend-resume-hook'.

Some operating systems cannot stop the Emacs process and resume it later.
On such systems, Emacs starts a subshell instead of suspending.  */)
     (stuffstring)
     Lisp_Object stuffstring;
{
  int count = specpdl_ptr - specpdl;
  int old_height, old_width;
  int width, height;
  struct gcpro gcpro1;

  if (!NILP (stuffstring))
    CHECK_STRING (stuffstring);

  /* Run the functions in suspend-hook.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, intern ("suspend-hook"));

  GCPRO1 (stuffstring);
  get_frame_size (&old_width, &old_height);
  reset_sys_modes ();
  /* sys_suspend can get an error if it tries to fork a subshell
     and the system resources aren't available for that.  */
  record_unwind_protect ((Lisp_Object (*) P_ ((Lisp_Object))) init_sys_modes,
			 Qnil);
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
    change_frame_size (SELECTED_FRAME (), height, width, 0, 0, 0);

  /* Run suspend-resume-hook.  */
  if (!NILP (Vrun_hooks))
    call1 (Vrun_hooks, intern ("suspend-resume-hook"));

  UNGCPRO;
  return Qnil;
}

/* If STUFFSTRING is a string, stuff its contents as pending terminal input.
   Then in any case stuff anything Emacs has read ahead and not used.  */

void
stuff_buffered_input (stuffstring)
     Lisp_Object stuffstring;
{
/* stuff_char works only in BSD, versions 4.2 and up.  */
#ifdef BSD_SYSTEM
#ifndef BSD4_1
  register unsigned char *p;

  if (STRINGP (stuffstring))
    {
      register int count;

      p = XSTRING (stuffstring)->data;
      count = STRING_BYTES (XSTRING (stuffstring));
      while (count-- > 0)
	stuff_char (*p++);
      stuff_char ('\n');
    }
  
  /* Anything we have read ahead, put back for the shell to read.  */
  /* ?? What should this do when we have multiple keyboards??
     Should we ignore anything that was typed in at the "wrong" kboard?  */
  for (; kbd_fetch_ptr != kbd_store_ptr; kbd_fetch_ptr++)
    {
      int idx;
      
      if (kbd_fetch_ptr == kbd_buffer + KBD_BUFFER_SIZE)
	kbd_fetch_ptr = kbd_buffer;
      if (kbd_fetch_ptr->kind == ascii_keystroke)
	stuff_char (kbd_fetch_ptr->code);
      
      kbd_fetch_ptr->kind = no_event;
      idx = 2 * (kbd_fetch_ptr - kbd_buffer);
      ASET (kbd_buffer_gcpro, idx, Qnil);
      ASET (kbd_buffer_gcpro, idx + 1, Qnil);
    }
  
  input_pending = 0;
#endif
#endif /* BSD_SYSTEM and not BSD4_1 */
}

void
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

void
clear_waiting_for_input ()
{
  /* Tell interrupt_signal not to throw back to read_char,  */
  waiting_for_input = 0;
  input_available_clear_time = 0;
}

/* This routine is called at interrupt level in response to C-g.
   
   If interrupt_input, this is the handler for SIGINT.  Otherwise, it
   is called from kbd_buffer_store_event, in handling SIGIO or
   SIGTINT.

   If `waiting_for_input' is non zero, then unless `echoing' is
   nonzero, immediately throw back to read_char.

   Otherwise it sets the Lisp variable quit-flag not-nil.  This causes
   eval to throw, when it gets a chance.  If quit-flag is already
   non-nil, it stops the job right away.  */

SIGTYPE
interrupt_signal (signalnum)	/* If we don't have an argument, */
     int signalnum;		/* some compilers complain in signal calls. */
{
  char c;
  /* Must preserve main program's value of errno.  */
  int old_errno = errno;
  struct frame *sf = SELECTED_FRAME ();

#if defined (USG) && !defined (POSIX_SIGNALS)
  if (!read_socket_hook && NILP (Vwindow_system))
    {
      /* USG systems forget handlers when they are used;
	 must reestablish each time */
      signal (SIGINT, interrupt_signal);
      signal (SIGQUIT, interrupt_signal);
    }
#endif /* USG */

  cancel_echoing ();

  if (!NILP (Vquit_flag)
      && (FRAME_TERMCAP_P (sf) || FRAME_MSDOS_P (sf)))
    {
      /* If SIGINT isn't blocked, don't let us be interrupted by
	 another SIGINT, it might be harmful due to non-reentrancy
	 in I/O functions.  */
      sigblock (sigmask (SIGINT));

      fflush (stdout);
      reset_sys_modes ();

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
      /* It doesn't work to autosave while GC is in progress;
	 the code used for auto-saving doesn't cope with the mark bit.  */
      if (!gc_in_progress)
	{
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
	}
      else 
	{
	  /* During GC, it must be safe to reenable quitting again.  */
	  Vinhibit_quit = Qnil;
#ifdef MSDOS
	  printf ("\r\n");
#endif /* not MSDOS */
	  printf ("Garbage collection in progress; cannot auto-save now\r\n");
	  printf ("but will instead do a real quit after garbage collection ends\r\n");
	  fflush (stdout);
	}

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
      sigfree ();
    }
  else
    {
      /* If executing a function that wants to be interrupted out of
	 and the user has not deferred quitting by binding `inhibit-quit'
	 then quit right away.  */
      if (immediate_quit && NILP (Vinhibit_quit))
	{
	  struct gl_state_s saved;
	  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

	  immediate_quit = 0;
          sigfree ();
	  saved = gl_state;
	  GCPRO4 (saved.object, saved.global_code,
		  saved.current_syntax_table, saved.old_prop);
	  Fsignal (Qquit, Qnil);
	  gl_state = saved;
	  UNGCPRO;
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

void
quit_throw_to_read_char ()
{
  sigfree ();
  /* Prevent another signal from doing this before we finish.  */
  clear_waiting_for_input ();
  input_pending = 0;

  Vunread_command_events = Qnil;
  unread_command_char = -1;

#if 0 /* Currently, sit_for is called from read_char without turning
	 off polling.  And that can call set_waiting_for_input.
	 It seems to be harmless.  */
#ifdef POLL_FOR_INPUT
  /* May be > 1 if in recursive minibuffer.  */
  if (poll_suppress_count == 0)
    abort ();
#endif
#endif
  if (FRAMEP (internal_last_event_frame)
      && !EQ (internal_last_event_frame, selected_frame))
    do_switch_frame (make_lispy_switch_frame (internal_last_event_frame),
		     0, 0);

  _longjmp (getcjmp, 1);
}

DEFUN ("set-input-mode", Fset_input_mode, Sset_input_mode, 3, 4, 0,
       doc: /* Set mode of reading keyboard input.
First arg INTERRUPT non-nil means use input interrupts;
 nil means use CBREAK mode.
Second arg FLOW non-nil means use ^S/^Q flow control for output to terminal
 (no effect except in CBREAK mode).
Third arg META t means accept 8-bit input (for a Meta key).
 META nil means ignore the top bit, on the assumption it is parity.
 Otherwise, accept 8-bit input and don't use the top bit for Meta.
Optional fourth arg QUIT if non-nil specifies character to use for quitting.
See also `current-input-mode'.  */)
     (interrupt, flow, meta, quit)
     Lisp_Object interrupt, flow, meta, quit;
{
  if (!NILP (quit)
      && (!INTEGERP (quit) || XINT (quit) < 0 || XINT (quit) > 0400))
    error ("set-input-mode: QUIT must be an ASCII character");

#ifdef POLL_FOR_INPUT
  stop_polling ();
#endif

#ifndef DOS_NT
  /* this causes startup screen to be restored and messes with the mouse */
  reset_sys_modes ();
#endif

#ifdef SIGIO
/* Note SIGIO has been undef'd if FIONREAD is missing.  */
  if (read_socket_hook)
    {
      /* When using X, don't give the user a real choice,
	 because we haven't implemented the mechanisms to support it.  */
#ifdef NO_SOCK_SIGIO
      interrupt_input = 0;
#else /* not NO_SOCK_SIGIO */
      interrupt_input = 1;
#endif /* NO_SOCK_SIGIO */
    }
  else
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

#ifndef DOS_NT
  init_sys_modes ();
#endif

#ifdef POLL_FOR_INPUT
  poll_suppress_count = 1;
  start_polling ();
#endif
  return Qnil;
}

DEFUN ("current-input-mode", Fcurrent_input_mode, Scurrent_input_mode, 0, 0, 0,
       doc: /* Return information about the way Emacs currently reads keyboard input.
The value is a list of the form (INTERRUPT FLOW META QUIT), where
  INTERRUPT is non-nil if Emacs is using interrupt-driven input; if
    nil, Emacs is using CBREAK mode.
  FLOW is non-nil if Emacs uses ^S/^Q flow control for output to the
    terminal; this does not apply if Emacs uses interrupt-driven input.
  META is t if accepting 8-bit input with 8th bit as Meta flag.
    META nil means ignoring the top bit, on the assumption it is parity.
    META is neither t nor nil if accepting 8-bit input and using
    all 8 bits as the character code.
  QUIT is the character Emacs currently uses to quit.
The elements of this list correspond to the arguments of
`set-input-mode'.  */)
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
 * Set up a new kboard object with reasonable initial values.
 */
void
init_kboard (kb)
     KBOARD *kb;
{
  kb->Voverriding_terminal_local_map = Qnil;
  kb->Vlast_command = Qnil;
  kb->Vreal_last_command = Qnil;
  kb->Vprefix_arg = Qnil;
  kb->Vlast_prefix_arg = Qnil;
  kb->kbd_queue = Qnil;
  kb->kbd_queue_has_data = 0;
  kb->immediate_echo = 0;
  kb->echo_string = Qnil;
  kb->echo_after_prompt = -1;
  kb->kbd_macro_buffer = 0;
  kb->kbd_macro_bufsize = 0;
  kb->defining_kbd_macro = Qnil;
  kb->Vlast_kbd_macro = Qnil;
  kb->reference_count = 0;
  kb->Vsystem_key_alist = Qnil;
  kb->system_key_syms = Qnil;
  kb->Vdefault_minibuffer_frame = Qnil;
}

/*
 * Destroy the contents of a kboard object, but not the object itself.
 * We use this just before deleting it, or if we're going to initialize
 * it a second time.
 */
static void
wipe_kboard (kb)
     KBOARD *kb;
{
  if (kb->kbd_macro_buffer)
    xfree (kb->kbd_macro_buffer);
}

#ifdef MULTI_KBOARD

/* Free KB and memory referenced from it.  */

void
delete_kboard (kb)
     KBOARD *kb;
{
  KBOARD **kbp;
  
  for (kbp = &all_kboards; *kbp != kb; kbp = &(*kbp)->next_kboard)
    if (*kbp == NULL)
      abort ();
  *kbp = kb->next_kboard;

  /* Prevent a dangling reference to KB.  */
  if (kb == current_kboard
      && FRAMEP (selected_frame)
      && FRAME_LIVE_P (XFRAME (selected_frame)))
    {
      current_kboard = XFRAME (selected_frame)->kboard;
      if (current_kboard == kb)
	abort ();
    }
  
  wipe_kboard (kb);
  xfree (kb);
}

#endif /* MULTI_KBOARD */

void
init_keyboard ()
{
  /* This is correct before outermost invocation of the editor loop */
  command_loop_level = -1;
  immediate_quit = 0;
  quit_char = Ctl ('g');
  Vunread_command_events = Qnil;
  unread_command_char = -1;
  EMACS_SET_SECS_USECS (timer_idleness_start_time, -1, -1);
  total_keys = 0;
  recent_keys_index = 0;
  kbd_fetch_ptr = kbd_buffer;
  kbd_store_ptr = kbd_buffer;
  kbd_buffer_gcpro = Fmake_vector (make_number (2 * KBD_BUFFER_SIZE), Qnil);
#ifdef HAVE_MOUSE
  do_mouse_tracking = Qnil;
#endif
  input_pending = 0;

  /* This means that command_loop_1 won't try to select anything the first
     time through.  */
  internal_last_event_frame = Qnil;
  Vlast_event_frame = internal_last_event_frame;

#ifdef MULTI_KBOARD
  current_kboard = initial_kboard;
#endif
  wipe_kboard (current_kboard);
  init_kboard (current_kboard);

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
  {&Qmouse_movement,      "mouse-movement",      &Qmouse_movement},
  {&Qscroll_bar_movement, "scroll-bar-movement", &Qmouse_movement},
  {&Qswitch_frame,        "switch-frame",        &Qswitch_frame},
  {&Qdelete_frame,        "delete-frame",        &Qdelete_frame},
  {&Qiconify_frame,       "iconify-frame",       &Qiconify_frame},
  {&Qmake_frame_visible,  "make-frame-visible",  &Qmake_frame_visible},
  {&Qselect_window,       "select-window",       &Qselect_window}
};

void
syms_of_keyboard ()
{
  Vpre_help_message = Qnil;
  staticpro (&Vpre_help_message);
  
  Vlispy_mouse_stem = build_string ("mouse");
  staticpro (&Vlispy_mouse_stem);

  /* Tool-bars.  */
  QCimage = intern (":image");
  staticpro (&QCimage);

  staticpro (&Qhelp_echo);
  Qhelp_echo = intern ("help-echo");

  staticpro (&item_properties);
  item_properties = Qnil;

  staticpro (&tool_bar_item_properties);
  tool_bar_item_properties = Qnil;
  staticpro (&tool_bar_items_vector);
  tool_bar_items_vector = Qnil;

  staticpro (&real_this_command);
  real_this_command = Qnil;

  Qtimer_event_handler = intern ("timer-event-handler");
  staticpro (&Qtimer_event_handler);

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

  Qpost_command_idle_hook = intern ("post-command-idle-hook");
  staticpro (&Qpost_command_idle_hook);

  Qdeferred_action_function = intern ("deferred-action-function");
  staticpro (&Qdeferred_action_function);

  Qcommand_hook_internal = intern ("command-hook-internal");
  staticpro (&Qcommand_hook_internal);

  Qfunction_key = intern ("function-key");
  staticpro (&Qfunction_key);
  Qmouse_click = intern ("mouse-click");
  staticpro (&Qmouse_click);
#ifdef WINDOWSNT
  Qmouse_wheel = intern ("mouse-wheel");
  staticpro (&Qmouse_wheel);
  Qlanguage_change = intern ("language-change");
  staticpro (&Qlanguage_change);
#endif
  Qdrag_n_drop = intern ("drag-n-drop");
  staticpro (&Qdrag_n_drop);

  Qsave_session = intern ("save-session");
  staticpro(&Qsave_session);
  
  Qusr1_signal = intern ("usr1-signal");
  staticpro (&Qusr1_signal);
  Qusr2_signal = intern ("usr2-signal");
  staticpro (&Qusr2_signal);

  Qmenu_enable = intern ("menu-enable");
  staticpro (&Qmenu_enable);
  Qmenu_alias = intern ("menu-alias");
  staticpro (&Qmenu_alias);
  QCenable = intern (":enable");
  staticpro (&QCenable);
  QCvisible = intern (":visible");
  staticpro (&QCvisible);
  QChelp = intern (":help");
  staticpro (&QChelp);
  QCfilter = intern (":filter");
  staticpro (&QCfilter);
  QCbutton = intern (":button");
  staticpro (&QCbutton);
  QCkeys = intern (":keys");
  staticpro (&QCkeys);
  QCkey_sequence = intern (":key-sequence");
  staticpro (&QCkey_sequence);
  QCtoggle = intern (":toggle");
  staticpro (&QCtoggle);
  QCradio = intern (":radio");
  staticpro (&QCradio);

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
  Qup = intern ("up");
  staticpro (&Qup);
  Qdown = intern ("down");
  staticpro (&Qdown);
  Qtop = intern ("top");
  staticpro (&Qtop);
  Qbottom = intern ("bottom");
  staticpro (&Qbottom);
  Qend_scroll = intern ("end-scroll");
  staticpro (&Qend_scroll);
  Qratio = intern ("ratio");
  staticpro (&Qratio);

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

  Qinput_method_function = intern ("input-method-function");
  staticpro (&Qinput_method_function);

  Qinput_method_exit_on_first_char = intern ("input-method-exit-on-first-char");
  staticpro (&Qinput_method_exit_on_first_char);
  Qinput_method_use_echo_area = intern ("input-method-use-echo-area");
  staticpro (&Qinput_method_use_echo_area);

  Fset (Qinput_method_exit_on_first_char, Qnil);
  Fset (Qinput_method_use_echo_area, Qnil);

  last_point_position_buffer = Qnil;

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

  button_down_location = Fmake_vector (make_number (1), Qnil);
  staticpro (&button_down_location);
  mouse_syms = Fmake_vector (make_number (1), Qnil);
  staticpro (&mouse_syms);

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

  raw_keybuf = Fmake_vector (make_number (30), Qnil);
  staticpro (&raw_keybuf);

  Qextended_command_history = intern ("extended-command-history");
  Fset (Qextended_command_history, Qnil);
  staticpro (&Qextended_command_history);

  kbd_buffer_gcpro = Fmake_vector (make_number (2 * KBD_BUFFER_SIZE), Qnil);
  staticpro (&kbd_buffer_gcpro);

  accent_key_syms = Qnil;
  staticpro (&accent_key_syms);

  func_key_syms = Qnil;
  staticpro (&func_key_syms);

#ifdef WINDOWSNT
  mouse_wheel_syms = Qnil;
  staticpro (&mouse_wheel_syms);
  
  drag_n_drop_syms = Qnil;
  staticpro (&drag_n_drop_syms);
#endif

  unread_switch_frame = Qnil;
  staticpro (&unread_switch_frame);

  internal_last_event_frame = Qnil;
  staticpro (&internal_last_event_frame);

  read_key_sequence_cmd = Qnil;
  staticpro (&read_key_sequence_cmd);

  menu_bar_one_keymap_changed_items = Qnil;
  staticpro (&menu_bar_one_keymap_changed_items);

  defsubr (&Sevent_convert_list);
  defsubr (&Sread_key_sequence);
  defsubr (&Sread_key_sequence_vector);
  defsubr (&Srecursive_edit);
#ifdef HAVE_MOUSE
  defsubr (&Strack_mouse);
#endif
  defsubr (&Sinput_pending_p);
  defsubr (&Scommand_execute);
  defsubr (&Srecent_keys);
  defsubr (&Sthis_command_keys);
  defsubr (&Sthis_command_keys_vector);
  defsubr (&Sthis_single_command_keys);
  defsubr (&Sthis_single_command_raw_keys);
  defsubr (&Sreset_this_command_lengths);
  defsubr (&Sclear_this_command_keys);
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
	       doc: /* Last input event that was part of a command.  */);

  DEFVAR_LISP_NOPRO ("last-command-event", &last_command_char,
		     doc: /* Last input event that was part of a command.  */);

  DEFVAR_LISP ("last-nonmenu-event", &last_nonmenu_event,
	       doc: /* Last input event in a command, except for mouse menu events.
Mouse menus give back keys that don't look like mouse events;
this variable holds the actual mouse event that led to the menu,
so that you can determine whether the command was run by mouse or not.  */);

  DEFVAR_LISP ("last-input-char", &last_input_char,
	       doc: /* Last input event.  */);

  DEFVAR_LISP_NOPRO ("last-input-event", &last_input_char,
		     doc: /* Last input event.  */);

  DEFVAR_LISP ("unread-command-events", &Vunread_command_events,
	       doc: /* List of events to be read as the command input.
These events are processed first, before actual keyboard input.  */);
  Vunread_command_events = Qnil;

  DEFVAR_INT ("unread-command-char", &unread_command_char,
	      doc: /* If not -1, an object to be read as next command input event.  */);

  DEFVAR_LISP ("unread-post-input-method-events", &Vunread_post_input_method_events,
	       doc: /* List of events to be processed as input by input methods.
These events are processed after `unread-command-events', but
before actual keyboard input.  */);
  Vunread_post_input_method_events = Qnil;

  DEFVAR_LISP ("unread-input-method-events", &Vunread_input_method_events,
	       doc: /* List of events to be processed as input by input methods.
These events are processed after `unread-command-events', but
before actual keyboard input.  */);
  Vunread_input_method_events = Qnil;

  DEFVAR_LISP ("meta-prefix-char", &meta_prefix_char,
	       doc: /* Meta-prefix character code.
Meta-foo as command input turns into this character followed by foo.  */);
  XSETINT (meta_prefix_char, 033);

  DEFVAR_KBOARD ("last-command", Vlast_command,
		 doc: /* The last command executed.
Normally a symbol with a function definition, but can be whatever was found
in the keymap, or whatever the variable `this-command' was set to by that
command.

The value `mode-exit' is special; it means that the previous command
read an event that told it to exit, and it did so and unread that event.
In other words, the present command is the event that made the previous
command exit.

The value `kill-region' is special; it means that the previous command
was a kill command.  */);

  DEFVAR_KBOARD ("real-last-command", Vreal_last_command,
		 doc: /* Same as `last-command', but never altered by Lisp code.  */);

  DEFVAR_LISP ("this-command", &Vthis_command,
	       doc: /* The command now being executed.
The command can set this variable; whatever is put here
will be in `last-command' during the following command.  */);
  Vthis_command = Qnil;

  DEFVAR_LISP ("this-original-command", &Vthis_original_command,
	       doc: /* If non-nil, the original command bound to the current key sequence.
The value of `this-command' is the result of looking up the original
command in the active keymaps.  */);
  Vthis_original_command = Qnil;

  DEFVAR_INT ("auto-save-interval", &auto_save_interval,
	      doc: /* *Number of input events between auto-saves.
Zero means disable autosaving due to number of characters typed.  */);
  auto_save_interval = 300;

  DEFVAR_LISP ("auto-save-timeout", &Vauto_save_timeout,
	       doc: /* *Number of seconds idle time before auto-save.
Zero or nil means disable auto-saving due to idleness.
After auto-saving due to this many seconds of idle time,
Emacs also does a garbage collection if that seems to be warranted.  */);
  XSETFASTINT (Vauto_save_timeout, 30);

  DEFVAR_LISP ("echo-keystrokes", &Vecho_keystrokes,
	       doc: /* *Nonzero means echo unfinished commands after this many seconds of pause.
The value may be integer or floating point.  */);
  Vecho_keystrokes = make_number (1);

  DEFVAR_INT ("polling-period", &polling_period,
	      doc: /* *Interval between polling for input during Lisp execution.
The reason for polling is to make C-g work to stop a running program.
Polling is needed only when using X windows and SIGIO does not work.
Polling is automatically disabled in all other cases.  */);
  polling_period = 2;

  DEFVAR_LISP ("double-click-time", &Vdouble_click_time,
	       doc: /* *Maximum time between mouse clicks to make a double-click.
Measured in milliseconds.  nil means disable double-click recognition;
t means double-clicks have no time limit and are detected
by position only.  */);
  Vdouble_click_time = make_number (500);

  DEFVAR_INT ("double-click-fuzz", &double_click_fuzz,
	      doc: /* *Maximum mouse movement between clicks to make a double-click.
On window-system frames, value is the number of pixels the mouse may have
moved horizontally or vertically between two clicks to make a double-click.
On non window-system frames, value is interpreted in units of 1/8 characters
instead of pixels.

This variable is also the threshold for motion of the mouse
to count as a drag.  */);
  double_click_fuzz = 3;
  
  DEFVAR_BOOL ("inhibit-local-menu-bar-menus", &inhibit_local_menu_bar_menus,
	       doc: /* *Non-nil means inhibit local map menu bar menus.  */);
  inhibit_local_menu_bar_menus = 0;

  DEFVAR_INT ("num-input-keys", &num_input_keys,
	      doc: /* Number of complete key sequences read as input so far.
This includes key sequences read from keyboard macros.
The number is effectively the number of interactive command invocations.  */);
  num_input_keys = 0;

  DEFVAR_INT ("num-nonmacro-input-events", &num_nonmacro_input_events,
	      doc: /* Number of input events read from the keyboard so far.
This does not include events generated by keyboard macros.  */);
  num_nonmacro_input_events = 0;

  DEFVAR_LISP ("last-event-frame", &Vlast_event_frame,
	       doc: /* The frame in which the most recently read event occurred.
If the last event came from a keyboard macro, this is set to `macro'.  */);
  Vlast_event_frame = Qnil;

  /* This variable is set up in sysdep.c.  */
  DEFVAR_LISP ("tty-erase-char", &Vtty_erase_char,
	       doc: /* The ERASE character as set by the user with stty.  */);

  DEFVAR_LISP ("help-char", &Vhelp_char,
	       doc: /* Character to recognize as meaning Help.
When it is read, do `(eval help-form)', and display result if it's a string.
If the value of `help-form' is nil, this char can be read normally.  */);
  XSETINT (Vhelp_char, Ctl ('H'));

  DEFVAR_LISP ("help-event-list", &Vhelp_event_list,
	       doc: /* List of input events to recognize as meaning Help.
These work just like the value of `help-char' (see that).  */);
  Vhelp_event_list = Qnil;

  DEFVAR_LISP ("help-form", &Vhelp_form,
	       doc: /* Form to execute when character `help-char' is read.
If the form returns a string, that string is displayed.
If `help-form' is nil, the help char is not recognized.  */);
  Vhelp_form = Qnil;

  DEFVAR_LISP ("prefix-help-command", &Vprefix_help_command,
	       doc: /* Command to run when `help-char' character follows a prefix key.
This command is used only when there is no actual binding
for that character after that prefix key.  */);
  Vprefix_help_command = Qnil;

  DEFVAR_LISP ("top-level", &Vtop_level,
	       doc: /* Form to evaluate when Emacs starts up.
Useful to set before you dump a modified Emacs.  */);
  Vtop_level = Qnil;

  DEFVAR_LISP ("keyboard-translate-table", &Vkeyboard_translate_table,
	       doc: /* Translate table for keyboard input, or nil.
Each character is looked up in this string and the contents used instead.
The value may be a string, a vector, or a char-table.
If it is a string or vector of length N,
character codes N and up are untranslated.
In a vector or a char-table, an element which is nil means "no translation".  */);
  Vkeyboard_translate_table = Qnil;

  DEFVAR_BOOL ("cannot-suspend", &cannot_suspend,
	       doc: /* Non-nil means to always spawn a subshell instead of suspending.
\(Even if the operating system has support for stopping a process.\)  */);
  cannot_suspend = 0;

  DEFVAR_BOOL ("menu-prompting", &menu_prompting,
	       doc: /* Non-nil means prompt with menus when appropriate.
This is done when reading from a keymap that has a prompt string,
for elements that have prompt strings.
The menu is displayed on the screen
if X menus were enabled at configuration
time and the previous event was a mouse click prefix key.
Otherwise, menu prompting uses the echo area.  */);
  menu_prompting = 1;

  DEFVAR_LISP ("menu-prompt-more-char", &menu_prompt_more_char,
	       doc: /* Character to see next line of menu prompt.
Type this character while in a menu prompt to rotate around the lines of it.  */);
  XSETINT (menu_prompt_more_char, ' ');

  DEFVAR_INT ("extra-keyboard-modifiers", &extra_keyboard_modifiers,
	      doc: /* A mask of additional modifier keys to use with every keyboard character.
Emacs applies the modifiers of the character stored here to each keyboard
character it reads.  For example, after evaluating the expression
    (setq extra-keyboard-modifiers ?\\C-x)
all input characters will have the control modifier applied to them.

Note that the character ?\\C-@, equivalent to the integer zero, does
not count as a control character; rather, it counts as a character
with no modifiers; thus, setting `extra-keyboard-modifiers' to zero
cancels any modification.  */);
  extra_keyboard_modifiers = 0;

  DEFVAR_LISP ("deactivate-mark", &Vdeactivate_mark,
	       doc: /* If an editing command sets this to t, deactivate the mark afterward.
The command loop sets this to nil before each command,
and tests the value when the command returns.
Buffer modification stores t in this variable.  */);
  Vdeactivate_mark = Qnil;

  DEFVAR_LISP ("command-hook-internal", &Vcommand_hook_internal,
	       doc: /* Temporary storage of pre-command-hook or post-command-hook.  */);
  Vcommand_hook_internal = Qnil;

  DEFVAR_LISP ("pre-command-hook", &Vpre_command_hook,
	       doc: /* Normal hook run before each command is executed.
If an unhandled error happens in running this hook,
the hook value is set to nil, since otherwise the error
might happen repeatedly and make Emacs nonfunctional.  */);
  Vpre_command_hook = Qnil;

  DEFVAR_LISP ("post-command-hook", &Vpost_command_hook,
	       doc: /* Normal hook run after each command is executed.
If an unhandled error happens in running this hook,
the hook value is set to nil, since otherwise the error
might happen repeatedly and make Emacs nonfunctional.  */);
  Vpost_command_hook = Qnil;

  DEFVAR_LISP ("post-command-idle-hook", &Vpost_command_idle_hook,
	       doc: /* Normal hook run after each command is executed, if idle.
Errors running the hook are caught and ignored.
This feature is obsolete; use idle timers instead.  See `etc/NEWS'.  */);
  Vpost_command_idle_hook = Qnil;

  DEFVAR_INT ("post-command-idle-delay", &post_command_idle_delay,
	      doc: /* Delay time before running `post-command-idle-hook'.
This is measured in microseconds.  */);
  post_command_idle_delay = 100000;

#if 0
  DEFVAR_LISP ("echo-area-clear-hook", ...,
	       doc: /* Normal hook run when clearing the echo area.  */);
#endif
  Qecho_area_clear_hook = intern ("echo-area-clear-hook");
  SET_SYMBOL_VALUE (Qecho_area_clear_hook, Qnil);

  DEFVAR_LISP ("lucid-menu-bar-dirty-flag", &Vlucid_menu_bar_dirty_flag,
	       doc: /* Non-nil means menu bar, specified Lucid style, needs to be recomputed.  */);
  Vlucid_menu_bar_dirty_flag = Qnil;

  DEFVAR_LISP ("menu-bar-final-items", &Vmenu_bar_final_items,
	       doc: /* List of menu bar items to move to the end of the menu bar.
The elements of the list are event types that may have menu bar bindings.  */);
  Vmenu_bar_final_items = Qnil;

  DEFVAR_KBOARD ("overriding-terminal-local-map",
		 Voverriding_terminal_local_map,
		 doc: /* Per-terminal keymap that overrides all other local keymaps.
If this variable is non-nil, it is used as a keymap instead of the
buffer's local map, and the minor mode keymaps and text property keymaps.
This variable is intended to let commands such as `universal-argument'
set up a different keymap for reading the next command.  */);

  DEFVAR_LISP ("overriding-local-map", &Voverriding_local_map,
	       doc: /* Keymap that overrides all other local keymaps.
If this variable is non-nil, it is used as a keymap instead of the
buffer's local map, and the minor mode keymaps and text property keymaps.  */);
  Voverriding_local_map = Qnil;

  DEFVAR_LISP ("overriding-local-map-menu-flag", &Voverriding_local_map_menu_flag,
	       doc: /* Non-nil means `overriding-local-map' applies to the menu bar.
Otherwise, the menu bar continues to reflect the buffer's local map
and the minor mode maps regardless of `overriding-local-map'.  */);
  Voverriding_local_map_menu_flag = Qnil;

  DEFVAR_LISP ("special-event-map", &Vspecial_event_map,
	       doc: /* Keymap defining bindings for special events to execute at low level.  */);
  Vspecial_event_map = Fcons (intern ("keymap"), Qnil);

  DEFVAR_LISP ("track-mouse", &do_mouse_tracking,
	       doc: /* *Non-nil means generate motion events for mouse motion.  */);

  DEFVAR_KBOARD ("system-key-alist", Vsystem_key_alist,
		 doc: /* Alist of system-specific X windows key symbols.
Each element should have the form (N . SYMBOL) where N is the
numeric keysym code (sans the \"system-specific\" bit 1<<28)
and SYMBOL is its name.  */);

  DEFVAR_LISP ("deferred-action-list", &Vdeferred_action_list,
	       doc: /* List of deferred actions to be performed at a later time.
The precise format isn't relevant here; we just check whether it is nil.  */);
  Vdeferred_action_list = Qnil;

  DEFVAR_LISP ("deferred-action-function", &Vdeferred_action_function,
	       doc: /* Function to call to handle deferred actions, after each command.
This function is called with no arguments after each command
whenever `deferred-action-list' is non-nil.  */);
  Vdeferred_action_function = Qnil;

  DEFVAR_LISP ("suggest-key-bindings", &Vsuggest_key_bindings,
	       doc: /* *Non-nil means show the equivalent key-binding when M-x command has one.
The value can be a length of time to show the message for.
If the value is non-nil and not a number, we wait 2 seconds.  */);
  Vsuggest_key_bindings = Qt;

  DEFVAR_LISP ("timer-list", &Vtimer_list,
	       doc: /* List of active absolute time timers in order of increasing time.  */);
  Vtimer_list = Qnil;

  DEFVAR_LISP ("timer-idle-list", &Vtimer_idle_list,
	       doc: /* List of active idle-time timers in order of increasing time.  */);
  Vtimer_idle_list = Qnil;

  DEFVAR_LISP ("input-method-function", &Vinput_method_function,
	       doc: /* If non-nil, the function that implements the current input method.
It's called with one argument, a printing character that was just read.
\(That means a character with code 040...0176.)
Typically this function uses `read-event' to read additional events.
When it does so, it should first bind `input-method-function' to nil
so it will not be called recursively.

The function should return a list of zero or more events
to be used as input.  If it wants to put back some events
to be reconsidered, separately, by the input method,
it can add them to the beginning of `unread-command-events'.

The input method function can find in `input-method-previous-method'
the previous echo area message.

The input method function should refer to the variables
`input-method-use-echo-area' and `input-method-exit-on-first-char'
for guidance on what to do.  */);
  Vinput_method_function = Qnil;

  DEFVAR_LISP ("input-method-previous-message",
	       &Vinput_method_previous_message,
	       doc: /* When `input-method-function' is called, hold the previous echo area message.
This variable exists because `read-event' clears the echo area
before running the input method.  It is nil if there was no message.  */);
  Vinput_method_previous_message = Qnil;

  DEFVAR_LISP ("show-help-function", &Vshow_help_function,
	       doc: /* If non-nil, the function that implements the display of help.
It's called with one argument, the help string to display.  */);
  Vshow_help_function = Qnil;

  DEFVAR_LISP ("disable-point-adjustment", &Vdisable_point_adjustment,
	       doc: /* If non-nil, suppress point adjustment after executing a command.

After a command is executed, if point is moved into a region that has
special properties (e.g. composition, display), we adjust point to
the boundary of the region.  But, several special commands sets this
variable to non-nil, then we suppress the point adjustment.

This variable is set to nil before reading a command, and is checked
just after executing the command.  */);
  Vdisable_point_adjustment = Qnil;

  DEFVAR_LISP ("global-disable-point-adjustment",
	       &Vglobal_disable_point_adjustment,
	       doc: /* *If non-nil, always suppress point adjustment.

The default value is nil, in which case, point adjustment are
suppressed only after special commands that set
`disable-point-adjustment' (which see) to non-nil.  */);
  Vglobal_disable_point_adjustment = Qnil;

  DEFVAR_BOOL ("update-menu-bindings", &update_menu_bindings,
	       doc: /* Non-nil means updating menu bindings is allowed.
A value of nil means menu bindings should not be updated.
Used during Emacs' startup.  */);
  update_menu_bindings = 1;

  DEFVAR_LISP ("minibuffer-message-timeout", &Vminibuffer_message_timeout,
	       doc: /* *How long to display an echo-area message when the minibuffer is active.
If the value is not a number, such messages don't time out.  */);
  Vminibuffer_message_timeout = make_number (2);
}

void
keys_of_keyboard ()
{
  initial_define_key (global_map, Ctl ('Z'), "suspend-emacs");
  initial_define_key (control_x_map, Ctl ('Z'), "suspend-emacs");
  initial_define_key (meta_map, Ctl ('C'), "exit-recursive-edit");
  initial_define_key (global_map, Ctl (']'), "abort-recursive-edit");
  initial_define_key (meta_map, 'x', "execute-extended-command");

  initial_define_lispy_key (Vspecial_event_map, "delete-frame",
			    "handle-delete-frame");
  initial_define_lispy_key (Vspecial_event_map, "iconify-frame",
			    "ignore-event");
  initial_define_lispy_key (Vspecial_event_map, "make-frame-visible",
			    "ignore-event");
  initial_define_lispy_key (Vspecial_event_map, "select-window",
			    "handle-select-window");
  initial_define_lispy_key (Vspecial_event_map, "save-session",
			    "handle-save-session");
}
