/* X Communication module for terminals which understand the X protocol.

Copyright (C) 1989, 1993-2026 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

/* New display code by Gerd Moellmann <gerd@gnu.org>.  */
/* Xt features made by Fred Pierresteguy.  */

/* X window system support for GNU Emacs

   This file is part of the X window system support for GNU Emacs.  It
   contains subroutines comprising the redisplay interface, setting up
   scroll bars and widgets, and handling input.

   X WINDOW SYSTEM

   The X Window System is a windowing system for bitmap graphics
   displays which originated at MIT in 1984.  Version 11, which is
   currently supported by Emacs, first appeared in September 1987.

   X has a long history and has been developed by many different
   organizations over the years; at present, it is being primarily
   developed by the X.Org Foundation.  It is the main window system
   that Emacs is developed and tested against, and X version 10 was
   the first window system that Emacs was ported to.  As a consequence
   of its age and wide availability, X contains many idiosyncrasies,
   but that has not prevented it from becoming the dominant free
   window system, and the platform of reference for all GUI code in
   Emacs.

   Some of what is explained below also applies to the other window
   systems that Emacs supports, to varying degrees.  YMMV.

   INPUT

   Emacs handles input by running pselect in a loop, which returns
   whenever there is input available on the connection to the X
   server.  On some systems, Emacs also arranges for any new input on
   that connection to send an asynchronous signal.  Whenever pselect
   returns, or such a signal is received and input is not blocked,
   XTread_socket is called and translates X11 events read by Xlib into
   struct input_events, which are then stored in the keyboard buffer,
   to be processed and acted upon at some later time.  The function
   handle_one_xevent is responsible for handling core events after
   they are filtered, and filtering X Input Extension events.  It also
   performs actions on some special events, such as updating the
   dimensions of a frame after a ConfigureNotify is sent by the X
   server to inform us that it changed.

   Before such events are translated, an Emacs build with
   internationalization enabled (the default since X11R6) will filter
   events through an X Input Method (XIM) or GTK, which might decide
   to intercept the event and send a different one in its place, for
   reasons such as enabling the user to insert international
   characters that aren't on his keyboard by typing a sequence of
   characters which are.  See the function x_filter_event and its
   callers for more details.

   Events that cause Emacs to quit are treated specially by the code
   that stores them in the keyboard buffer and generally cause an
   immediate interrupt.  Such an interrupt can lead to a longjmp from
   the code that stored the keyboard event, which isn't safe inside
   XTread_socket.  To avoid this problem, XTread_socket is provided a
   special event buffer named hold_quit.  When a quit event is
   encountered, it is stored inside this special buffer, which will
   cause the keyboard code that called XTread_socket to store it at a
   later time when it is safe to do so.

   handle_one_xevent will generally have to determine which frame an
   event should be attributed to.  This is not easy, because events
   can come from multiple X windows, and a frame can also have
   multiple windows.  handle_one_xevent usually calls the function
   x_any_window_to_frame, which searches for a frame by toplevel
   window and widget windows.  There are also some other functions for
   searching by specific types of window, such as
   x_top_window_to_frame (which only searches for frames by toplevel
   window), and x_menubar_window_to_frame (which will only search
   through frame menu bars).

   INPUT FOCUS

   Under X, the window where keyboard input is sent is not always
   explicitly defined.  When there is a focus window, it receives what
   is referred to as "explicit focus", but when there is none, it
   receives "implicit focus" whenever the pointer enters it, and loses
   that focus when the pointer leaves.  When the toplevel window of a
   frame receives an explicit focus event (FocusIn or FocusOut), we
   treat that frame as having the current input focus, but when there
   is no focus window, we treat each frame as having the input focus
   whenever the pointer enters it, and undo that treatment when the
   pointer leaves it.  See the callers of x_detect_focus_change for
   more details.

   REDISPLAY

   The redisplay engine communicates with X through the "redisplay
   interface", which is a structure containing pointers to functions
   which output graphics to a frame.

   Some of the functions included in the redisplay interface include
   `x_clear_frame_area', which is called by the display engine when it
   determines that a part of the display has to be cleared,
   x_draw_window_cursor, which is called to perform the calculations
   necessary to display the cursor glyph with a special "highlight"
   (more on that later) and to set the input method spot location.

   Most of the actual display is performed by the function
   `x_draw_glyph_string', also included in the redisplay interface.
   It takes a list of glyphs of the same type and face, computes the
   correct graphics context for the string through the function
   `x_set_glyph_string_gc', and draws whichever glyphs it might
   contain, along with decorations such as the box face, underline and
   overline.  That list is referred to as a "glyph string".

   GRAPHICS CONTEXTS

   A graphics context ("GC") is an X server-side object which contains
   drawing attributes such as fill style, stipple, and foreground and
   background pixel values.

   Usually, one graphics context is computed for each face when it is
   about to be displayed for the first time, and this graphics context
   is the one which is used for future X drawing operations in a glyph
   string with that face.  (See `prepare_face_for_display' in
   xfaces.c).

   However, when drawing glyph strings for special display elements
   such as the cursor, or mouse sensitive text, different GCs may be
   used.  When displaying the cursor, for example, the frame's cursor
   graphics context is used for the common case where the cursor is
   drawn with the default font, and the colors of the string's face
   are the same as the default face.  In all other cases, a temporary
   graphics context is created with the foreground and background
   colors of the cursor face adjusted to ensure that the cursor can be
   distinguished from its surroundings and that the text inside the
   cursor stays visible.

   Various graphics contexts are also calculated when the frame is
   created by the function `x_make_gcs' in xfns.c, and are adjusted
   whenever the foreground or background colors change.  The "normal"
   graphics context is used for operations performed without a face,
   and always corresponds to the foreground and background colors of
   the frame's default face, the "reverse" graphics context is used to
   draw text in inverse video, and the cursor graphics context is used
   to display the cursor in the most common case.

   N.B. that some of the other window systems supported by use an
   emulation of graphics contexts to hold the foreground and
   background colors used in a glyph string, while the some others
   ports compute those colors directly based on the colors of the
   string's face and its highlight, but only on X are graphics
   contexts a data structure inherent to the window system.

   COLOR ALLOCATION

   In (and only in) X, pixel values for colors are not guaranteed to
   correspond to their individual components.  The rules for
   converting colors into pixel values are defined by the visual class
   of each display opened by Emacs.  When a display is opened, a
   suitable visual is obtained from the X server, and a colormap is
   created based on that visual, which is then used for each frame
   created.

   The colormap is then used by the X server to convert pixel values
   from a frame created by Emacs into actual colors which are output
   onto the physical display.

   When the visual class is TrueColor, the colormap will be indexed
   based on the red, green, and blue (RGB) components of the pixel
   values, and the colormap will be statically allocated so as to
   contain linear ramps for each component.  As such, most of the
   color allocation described below is bypassed, and the pixel values
   are computed directly from the color.

   Otherwise, each time Emacs wants a pixel value that corresponds to
   a color, Emacs has to ask the X server to obtain the pixel value
   that corresponds to a "color cell" containing the color (or a close
   approximation) from the colormap.  Exactly how this is accomplished
   further depends on the visual class, since some visuals have
   immutable colormaps which contain color cells with pre-defined
   values, while others have colormaps where the color cells are
   dynamically allocated by individual X clients.

   With visuals that have a visual class of StaticColor and StaticGray
   (where the former is the case), the X server is asked to procure
   the pixel value of a color cell that contains the closest
   approximation of the color which Emacs wants.  On the other hand,
   when the visual class is DirectColor, PseudoColor, or GrayScale,
   where color cells are dynamically allocated by clients, Emacs asks
   the X server to allocate a color cell containing the desired color,
   and uses its pixel value.

   (If the color already exists, the X server returns an existing color
   cell, but increases its reference count, so it still has to be
   freed afterwards.)

   Otherwise, if no color could be allocated (due to the colormap
   being full), Emacs looks for a color cell inside the colormap
   closest to the desired color, and uses its pixel value instead.

   Since the capacity of a colormap is finite, X clients have to take
   special precautions in order to not allocate too many color cells
   that are never used.  Emacs allocates its color cells when a face
   is being realized or when a frame changes its foreground and
   background colors, and releases them alongside the face or frame.
   See calls to `unload_color' and `load_color' in xterm.c, xfaces.c
   and xfns.c for more details.

   The driving logic behind color allocation is in
   `x_alloc_nearest_color_1', while the optimization for TrueColor
   visuals is in `x_make_truecolor_pixel'.  Also see `x_query_colors`,
   which is used to determine the color values for given pixel
   values.

   In other window systems supported by Emacs, color allocation is
   handled by the window system itself, to whom Emacs simply passes 24
   (or 32-bit) RGB values.

   OPTIONAL FEATURES

   While X servers and client libraries tend to come with many
   extensions to the core X11R6 protocol, dependencies on anything
   other than the core X11R6 protocol and Xlib should be optional at
   both compile-time and runtime.  Emacs should also not crash
   regardless of what combination of X server and client-side features
   are present.  For example, if you are developing a feature that
   will need Xfixes, then add a test in configure.ac for the library
   at compile-time which defines `HAVE_XFIXES', like this:

     ### Use Xfixes (-lXfixes) if available
     HAVE_XFIXES=no
     if test "${HAVE_X11}" = "yes"; then
       XFIXES_REQUIRED=4.0.0
       XFIXES_MODULES="xfixes >= $XFIXES_REQUIRED"
       EMACS_CHECK_MODULES([XFIXES], [$XFIXES_MODULES])
       if test $HAVE_XFIXES = no; then
	 # Test old way in case pkg-config doesn't have it (older machines).
	 AC_CHECK_HEADER([X11/extensions/Xfixes.h],
	   [AC_CHECK_LIB([Xfixes], [XFixesHideCursor], [HAVE_XFIXES=yes])])
	 if test $HAVE_XFIXES = yes; then
	   XFIXES_LIBS=-lXfixes
	 fi
       fi
       if test $HAVE_XFIXES = yes; then
	 AC_DEFINE([HAVE_XFIXES], [1],
	   [Define to 1 if you have the Xfixes extension.])
       fi
     fi
     AC_SUBST([XFIXES_CFLAGS])
     AC_SUBST([XFIXES_LIBS])

  Then, make sure to adjust CFLAGS and LIBES in src/Makefile.in and
  add the new XFIXES_CFLAGS and XFIXES_LIBS variables to
  msdos/sed1v2.inp.  (The latter has to be adjusted for any new
  variables that are included in CFLAGS and LIBES even if the
  libraries are not used by the MS-DOS port.)

  Finally, add some fields in `struct x_display_info' which specify
  the major and minor versions of the extension, and whether or not to
  support them.  They (and their accessors) should be protected by the
  `HAVE_XFIXES' preprocessor conditional.  Then, these fields should
  be set in `x_term_init', and all Xfixes calls must be protected by
  not only the preprocessor conditional, but also by checks against
  those variables.

  X TOOLKIT SUPPORT

  Emacs supports being built with many different toolkits (and also no
  toolkit at all), which provide decorations such as menu bars and
  scroll bars, along with handy features like file panels, dialog
  boxes, font panels, and popup menus.  Those configurations can
  roughly be classified as belonging to one of three categories:

    - Using no toolkit at all.
    - Using the X Toolkit Intrinsics (Xt).
    - Using GTK.

  The no toolkit configuration is the simplest: no toolkit widgets are
  used, Emacs uses its own implementation of scroll bars, and the
  XMenu library that came with X11R2 and earlier versions of X is used
  for popup menus.  There is also no complicated window structure to
  speak of.

  The Xt configurations come in either the Lucid or Motif flavors.
  The former utilizes Emacs's own Xt-based Lucid widget library for
  menus, and Xaw (or derivatives such as neXTaw and Xaw3d) for dialog
  boxes and, optionally, scroll bars.  It does not support file
  panels.  The latter uses either Motif or LessTif for menu bars,
  popup menus, dialogs and file panels.

  The GTK configurations come in the GTK+ 2 or GTK 3 configurations,
  where the toolkit provides all the aforementioned decorations and
  features.  They work mostly the same, though GTK 3 has various small
  annoyances that complicate maintenance.

  All of those configurations have various special technicalities
  about event handling and the layout of windows inside a frame that
  must be kept in mind when writing X code which is run on all of
  them.

  The no toolkit configuration has no noteworthy aspects about the
  layout of windows inside a frame, since each frame has only one
  associated window aside from scroll bars.  However, in the Xt
  configurations, every widget is a separate window, and there are
  quite a few widgets.  The "outer widget", a widget of class
  ApplicationShell, is the top-level window of a frame.  Its window is
  accessed via the macro `FRAME_OUTER_WINDOW'.  The "edit widget", a
  widget class of EmacsFrame, is a child of the outer widget that
  controls the size of a frame as known to Emacs, and is the widget
  that Emacs draws to during display operations.  The "menu bar
  widget" is the widget holding the menu bar.

  Special care must be taken when performing operations on a frame.
  Properties that are used by the window manager, for example, must be
  set on the outer widget.  Drawing, on the other hand, must be done
  to the edit widget, and button press events on the menu bar widget
  must be redirected and not sent to Xt until the Lisp code is run to
  update the menu bar.

  The EmacsFrame widget is specific to Emacs and is implemented in
  widget.c.  See that file for more details.

  In the GTK configurations, GTK widgets do not necessarily correspond
  to X windows, since the toolkit might decide to keep only a
  client-side record of the widgets for performance reasons.

  Because the GtkFixed widget that holds the "edit area" might not
  correspond to an X window, drawing operations may be directly
  performed on the outer window, with special care taken to not
  overwrite the surrounding GTK widgets.  This also means that the
  only important window for most purposes is the outer window, which
  on GTK builds can usually be accessed using the macro
  `FRAME_X_WINDOW'.

  How `handle_one_xevent' is called also depends on the configuration.
  Without a toolkit, Emacs performs all event processing by itself,
  running XPending and XNextEvent in a loop whenever there is input,
  passing the event to `handle_one_xevent'.

  When using Xt, the same is performed, but `handle_one_xevent' may
  also decide to call XtDispatchEvent on an event after Emacs finishes
  processing it.

  When using GTK, however, `handle_one_xevent' is called from an event
  filter installed on the GTK event loop.  Unless the event filter
  elects to drop the event, it will be passed to GTK right after
  leaving the event filter.

  Fortunately, `handle_one_xevent' is provided a `*finish' parameter
  that abstracts away all these details.  If it is `X_EVENT_DROP',
  then the event will not be dispatched to Xt or utilized by GTK.
  Code inside `handle_one_xevent' should thus avoid making assumptions
  about the event dispatch mechanism and use that parameter
  instead.

  FRAME RESIZING

  In the following explanations "frame size" refers to the "native
  size" of a frame as reported by the (frame.h) macros
  FRAME_PIXEL_WIDTH and FRAME_PIXEL_HEIGHT.  These specify the size of
  a frame as the values passed to/received from a toolkit and the
  window manager.  The "text size" Emacs Lisp code uses in functions
  like 'set-frame-size' or sees in the ‘width’ and 'height' frame
  parameters is only loosely related to the native size.  The
  necessary translations are provided by the macros
  FRAME_TEXT_TO_PIXEL_WIDTH and FRAME_TEXT_TO_PIXEL_HEIGHT as well as
  FRAME_PIXEL_TO_TEXT_WIDTH and FRAME_PIXEL_TO_TEXT_HEIGHT (in
  frame.h).

  Lisp functions may ask for resizing a frame either explicitly, using
  one of the interfaces provided for that purpose like, for example,
  'set-frame-size' or changing the 'height' or 'width' parameter of
  that frame, or implicitly, for example, by turning off/on or
  changing the width of fringes or scroll bars for that frame.  Any
  such request passes through the routine 'adjust_frame_size' (in
  frame.c) which decides, among others, whether the native frame size
  would really change and whether it is allowed to change it at that
  moment.  Only if 'adjust_frame_size' decides that the corresponding
  terminal's 'set_window_size_hook' may be run, it will dispatch
  execution to the appropriate function which, for X builds, is
  'x_set_window_size' in this file.

  For GTK builds, 'x_set_window_size' calls 'xg_frame_set_char_size'
  in gtkutil.c if the frame has an edit widget and
  'x_set_window_size_1' in this file otherwise.  For non-GTK builds,
  'x_set_window_size' always calls 'x_set_window_size_1' directly.

  'xg_frame_set_char_size' calls the GTK function 'gtk_window_resize'
  for the frame's outer widget; x_set_window_size_1 calls the Xlib
  function 'XResizeWindow' instead.  In either case, if Emacs thinks
  that the frame is visible, it will wait for a ConfigureNotify event
  (see below) to occur within a timeout of 'x-wait-for-event-timeout'
  (the default is 0.1 seconds).  If Emacs thinks that the frame is not
  visible, it calls 'adjust_frame_size' to run 'resize_frame_windows'
  (see below) and hopes for the best.

  Note that if Emacs receives a ConfigureEvent in response to an
  earlier resize request, the sizes specified by that event are not
  necessarily the sizes Emacs requested.  Window manager and toolkit
  may override any of the requested sizes for their own reasons.

  On X, size notifications are received as ConfigureNotify events.
  The expected reaction to such an event on the Emacs side is to
  resize all Emacs windows that are on the frame referred to by the
  event.  Since resizing Emacs windows and redisplaying their buffers
  is a costly operation, Emacs may collapse several subsequent
  ConfigureNotify events into one to avoid that Emacs falls behind in
  user interactions like resizing a frame by dragging one of its
  borders with the mouse.

  Each ConfigureEvent event specifies a window, a width and a height.
  The event loop uses 'x_top_window_to_frame' to associate the window
  with its frame.  Once the frame has been identified, on GTK the
  event is dispatched to 'xg_frame_resized'.  On Motif/Lucid
  'x_window' has installed 'EmacsFrameResize' as the routine that
  handles resize events.  In either case, these routines end up
  calling the function 'change_frame_size' in dispnew.c.  On
  non-toolkit builds the effect is to call 'change_frame_size'
  directly from the event loop.  In either case, the value true is
  passed as the DELAY argument.

  'change_frame_size' is the central function to decide whether it is
  safe to process a resize request immediately or it has to be delayed
  (usually because its DELAY argument is true).  Since resizing a
  frame's windows may run arbitrary Lisp code, Emacs cannot generally
  process resize requests during redisplay and therefore has to queue
  them.  If processing the event must be delayed, the new sizes (that
  is, the ones requested by the ConfigureEvent) are stored in the
  new_width and new_height slots of the respective frame structure,
  possibly replacing ones that have been stored there upon the receipt
  of a preceding ConfigureEvent.

  Delayed size changes are applied eventually upon calls of the
  function 'do_pending_window_change' (in dispnew.c) which is called
  by the redisplay code at suitable spots where it's safe to change
  sizes.  'do_pending_window_change' calls 'change_frame_size' with
  its DELAY argument false in the hope that it is now safe to call the
  function 'resize_frame_windows' (in window.c) which is in charge of
  adjusting the sizes of all Emacs windows on the frame accordingly.
  Note that if 'resize_frame_windows' decides that the windows of a
  frame do not fit into the constraints set up by the new frame sizes,
  it will resize the windows to some minimum sizes with the effect
  that parts of the frame at the right and bottom will appear clipped
  off.

  In addition to explicitly passing width and height values in
  functions like 'gtk_window_resize' or 'XResizeWindow', Emacs also
  sets window manager size hints - a more implicit form of asking for
  the size Emacs would like its frames to assume.  Some of these hints
  only restate the size and the position explicitly requested for a
  frame.  Another hint specifies the increments in which the window
  manager should resize a frame to - either set to the default
  character size of a frame or to one pixel for a non-nil value of
  'frame-resize-pixelwise'.  See the function 'x_wm_set_size_hint' -
  in gtkutil.c for GTK and in this file for other builds - for the
  details.

  We have not discussed here a number of special issues like, for
  example, how to handle size requests and notifications for maximized
  and fullscreen frames or how to resize child frames.  Some of these
  require special treatment depending on the desktop or window manager
  used.

  One thing that might come handy when investigating problems wrt
  resizing frames is the variable 'frame-size-history'.  Setting this
  to a non-nil value, will cause Emacs to start recording frame size
  adjustments, usually specified by the function that asked for an
  adjustment, a sizes part that records the old and new values of the
  frame's width and height and maybe some additional information.  The
  internal function `frame--size-history' can then be used to display
  the value of this variable in a more readable form.

  FRAME RESIZE SYNCHRONIZATION

  The X window system operates asynchronously.  That is to say, the
  window manager and X server might think a window has been resized
  before Emacs has a chance to process the ConfigureNotify event that
  was sent.

  When a compositing manager is present, and the X server and Emacs
  both support the X synchronization extension, the semi-standard
  frame synchronization protocol can be used to notify the compositing
  manager of when Emacs has actually finished redisplaying the
  contents of a frame after a resize.  The compositing manager will
  customarily then postpone displaying the contents of the frame until
  the redisplay is complete.

  Emacs announces support for this protocol by creating an X
  server-side counter object, and setting it as the
  `_NET_WM_SYNC_REQUEST_COUNTER' property of the frame's top-level
  window.  The window manager then initiates the synchronized resize
  process by sending Emacs a ClientMessage event before the
  ConfigureNotify event where:

    type = ClientMessage
    window = the respective client window
    message_type = WM_PROTOCOLS
    format = 32
    data.l[0] = _NET_WM_SYNC_REQUEST
    data.l[1] = timestamp
    data.l[2] = low 32 bits of a provided frame counter value
    data.l[3] = high 32 bits of a provided frame counter value
    data.l[4] = 1 if the extended frame counter should be updated,
    otherwise 0

  Upon receiving such an event, Emacs constructs and saves a counter
  value from the provided low and high 32 bits.  Then, when the
  display engine tells us that a frame has been completely updated
  (presumably because of a redisplay caused by a ConfigureNotify
  event), we set the counter to the saved value, telling the
  compositing manager that the contents of the window now accurately
  reflect the new size.  The compositing manager will then display the
  contents of the window, and the window manager might also postpone
  updating the window decorations until this moment.

  DRAG AND DROP

  Drag and drop in Emacs is implemented in two ways, depending on
  which side initiated the drag-and-drop operation.  When another X
  client initiates a drag, and the user drops something on Emacs, a
  `drag-n-drop-event' is sent with the contents of the ClientMessage,
  and further processing (i.e. retrieving selection contents and
  replying to the initiating client) is performed from Lisp inside
  `x-dnd.el'.

  However, dragging contents from Emacs is implemented almost entirely
  in C.  X Windows has several competing drag-and-drop protocols, of
  which Emacs supports two on the C level: the XDND protocol (see
  https://freedesktop.org/wiki/Specifications/XDND) and the Motif drag
  and drop protocols.  These protocols are based on the initiator
  owning a special selection, specifying an action the recipient
  should perform, grabbing the mouse, and sending various different
  client messages to the toplevel window underneath the mouse as it
  moves, or when buttons are released.

  The Lisp interface to drag-and-drop is synchronous, and involves
  running a nested event loop with some global state until the drag
  finishes.  When the mouse moves, Emacs looks up the toplevel window
  underneath the pointer (the target window) either using a cache
  provided by window managers that support the
  _NET_WM_CLIENT_LIST_STACKING root window property, or by calling
  XTranslateCoordinates in a loop until a toplevel window is found,
  and sends various entry, exit, or motion events to the window
  containing a list of targets the special selection can be converted
  to, and the chosen action that the recipient should perform.  The
  recipient can then send messages in reply detailing the action it
  has actually chosen to perform.  Finally, when the mouse buttons are
  released over the recipient window, Emacs sends a "drop" message to
  the target window, waits for a reply, and returns the action
  selected by the recipient to the Lisp code that initiated the
  drag-and-drop operation.

  When a drop happens on a window not supporting any protocol
  implemented on the C level, the function inside
  `x-dnd-unsupported-drop-function' is called with some parameters of
  the drop.  If it returns non-nil, then Emacs tries to simulate a
  drop happening with the primary selection and synthetic button
  events (see `x_dnd_do_unsupported_drop').  That function implements
  the OffiX drag-and-drop protocol by default.  See
  `x-dnd-handle-unsupported-drop' in `x-dnd.el' for more details.

  DISPLAY ERROR HANDLING

  While error handling under X was originally designed solely as a
  mechanism for the X server to report fatal errors to clients, most
  clients (including Emacs) have adopted a system of "error traps" to
  handle or discard these errors as they arrive.  Discarding errors is
  usually necessary when Emacs performs an X request that might fail:
  for example, sending a message to a window that may no longer exist,
  or might not exist at all.  Handling errors is then necessary when
  the detailed error must be reported to another piece of code: for
  example, as a Lisp error.

  It is not acceptable for Emacs to crash when it is sent invalid data
  by another client, or by Lisp.  As a result, errors must be caught
  around Xlib functions generating requests containing resource
  identifiers that could potentially be invalid, such as window or
  atom identifiers provided in a client message from another program,
  or a child window ID obtained through XTranslateCoordinates that may
  refer to a window that has been deleted in the meantime.

  There are two sets of functions used to perform this "error
  trapping".  Which one should be used depends on what kind of
  processing must be done on the error.  The first consists of the
  functions `x_ignore_errors_for_next_request' and
  `x_stop_ignoring_errors', which ignore errors generated by requests
  made in between a call to the first function and a corresponding
  call to the second.  They should be used for simple asynchronous
  requests that do not require a reply from the X server: using them
  instead of the second set improves performance, as they simply
  record a range of request serials to ignore errors from, instead of
  synchronizing with the X server to handle errors.

  The second set consists of the following functions:

    - x_catch_errors_with_handler
    - x_catch_errors
    - x_uncatch_errors_after_check
    - x_uncatch_errors
    - x_check_errors
    - x_had_errors_p
    - x_clear_errors

  Callers using this set should consult the comment(s) on top of the
  aforementioned functions.  They should not be used when the requests
  being made do not require roundtrips to the X server, and obtaining
  the details of any error generated is unnecessary, as
  `x_uncatch_errors' will always synchronize with the X server, which
  is a potentially slow operation.  */

#include <config.h>
#include <stdlib.h>
#include <math.h>
#include <signal.h>

#include "lisp.h"
#include "blockinput.h"
#include "sysstdio.h"

/* This may include sys/types.h, and that somehow loses
   if this is not done before the other system files.  */
#include "xterm.h"
#include <X11/cursorfont.h>

#ifdef HAVE_X_I18N
#include "textconv.h"
#endif

#ifdef USE_XCB
#include <xcb/xproto.h>
#include <xcb/xcb.h>
#endif

/* If we have Xfixes extension, use it for pointer blanking.  */
#ifdef HAVE_XFIXES
#include <X11/extensions/Xfixes.h>
#endif

#ifdef HAVE_XDBE
#include <X11/extensions/Xdbe.h>
#endif

#ifdef HAVE_XINPUT2
#include <X11/extensions/XInput2.h>
#endif

#ifdef HAVE_XRANDR
#include <X11/extensions/Xrandr.h>
#endif

#ifdef HAVE_XSYNC
#include <X11/extensions/sync.h>
#endif

#ifdef HAVE_XINERAMA
#include <X11/extensions/Xinerama.h>
#endif

#ifdef HAVE_XCOMPOSITE
#include <X11/extensions/Xcomposite.h>
#endif

#ifdef HAVE_XSHAPE
#include <X11/extensions/shape.h>
#endif

#ifdef HAVE_XCB_SHAPE
#include <xcb/shape.h>
#endif

/* Load sys/types.h if not already loaded.
   In some systems loading it twice is suicidal.  */
#ifndef makedev
#include <sys/types.h>
#endif /* makedev */

#include <sys/ioctl.h>

#include "systime.h"

#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <flexmember.h>
#include <c-ctype.h>
#include <byteswap.h>

#include "character.h"
#include "coding.h"
#include "composite.h"
#include "frame.h"
#include "dispextern.h"
#include "xwidget.h"
#include "fontset.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "emacs-icon.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "atimer.h"
#include "font.h"
#include "xsettings.h"
#include "sysselect.h"
#include "menu.h"
#include "pdumper.h"

#ifdef USE_X_TOOLKIT
#include <X11/Shell.h>
#include <X11/ShellP.h>
#endif

#include <unistd.h>

#ifdef USE_GTK
#include "gtkutil.h"
#ifdef HAVE_GTK3
#include <X11/Xproto.h>
#endif
#endif

#if defined (USE_LUCID) || defined (USE_MOTIF)
#include "../lwlib/xlwmenu.h"
#endif

#ifdef HAVE_XWIDGETS
#include <cairo-xlib.h>
#endif

#ifdef USE_MOTIF
#include <Xm/Xm.h>
#include <Xm/CascadeB.h>
#endif

#ifdef USE_X_TOOLKIT

/* Include toolkit specific headers for the scroll bar widget.  */
#ifdef USE_TOOLKIT_SCROLL_BARS
#if defined USE_MOTIF
#include <Xm/ScrollBar.h>
#else /* !USE_MOTIF i.e. use Xaw */

#ifdef HAVE_XAW3D
#include <X11/Xaw3d/Simple.h>
#include <X11/Xaw3d/Scrollbar.h>
#include <X11/Xaw3d/ThreeD.h>
#else /* !HAVE_XAW3D */
#include <X11/Xaw/Simple.h>
#include <X11/Xaw/Scrollbar.h>
#endif /* !HAVE_XAW3D */
#ifndef XtNpickTop
#define XtNpickTop "pickTop"
#endif /* !XtNpickTop */
#endif /* !USE_MOTIF */
#endif /* USE_TOOLKIT_SCROLL_BARS */

#endif /* USE_X_TOOLKIT */

#ifdef USE_X_TOOLKIT
#include "widget.h"
#ifndef XtNinitialState
#define XtNinitialState "initialState"
#endif
#endif

#ifdef USE_GTK
#include <xgselect.h>
#endif

#include "bitmaps/gray.xbm"

#ifdef HAVE_XKB
#include <X11/XKBlib.h>
#endif

/* Although X11/Xlib.h commonly defines the types XErrorHandler and
   XIOErrorHandler, they are not in the Xlib spec, so for portability
   define and use names with an Emacs_ prefix instead.  */
typedef int (*Emacs_XErrorHandler) (Display *, XErrorEvent *);
typedef int (*Emacs_XIOErrorHandler) (Display *);

#if defined USE_XCB && defined USE_CAIRO_XCB
#define USE_CAIRO_XCB_SURFACE
#endif

#if XCB_SHAPE_MAJOR_VERSION > 1	      \
  || (XCB_SHAPE_MAJOR_VERSION == 1 && \
      XCB_SHAPE_MINOR_VERSION >= 1)
#define HAVE_XCB_SHAPE_INPUT_RECTS
#endif

#ifdef USE_GTK
/* GTK can't tolerate a call to `handle_interrupt' inside an event
   signal handler, but we have to store input events inside the
   handler for native input to work.

   This acts as a `hold_quit', and it is stored in the keyboard buffer
   (thereby causing the call to `handle_interrupt') after the GTK
   signal handler exits and control returns to XTread_socket.  */
struct input_event xg_pending_quit_event = { .kind = NO_EVENT };
#endif

/* Non-zero means that a HELP_EVENT has been generated since Emacs
   start.  */

static bool any_help_event_p;

/* This is a chain of structures for all the X displays currently in
   use.  */

struct x_display_info *x_display_list;

#ifdef USE_X_TOOLKIT

/* The application context for Xt use.  */
XtAppContext Xt_app_con;
static String Xt_default_resources[] = {0};

/* Non-zero means user is interacting with a toolkit scroll bar.  */
static bool toolkit_scroll_bar_interaction;

#endif /* USE_X_TOOLKIT */

/* Non-zero timeout value means ignore next mouse click if it arrives
   before that timeout elapses (i.e. as part of the same sequence of
   events resulting from clicking on a frame to select it).  */

static Time ignore_next_mouse_click_timeout;

/* The display that ignore_next_mouse_click_timeout applies to.  */

static struct x_display_info *mouse_click_timeout_display;

/* Used locally within XTread_socket.  */

static int x_noop_count;

#ifdef USE_GTK
/* The name of the Emacs icon file.  */
static Lisp_Object xg_default_icon_file;
#endif

#ifdef HAVE_X_I18N
/* Some functions take this as char *, not const char *.  */
static char emacs_class[] = EMACS_CLASS;
#endif

#ifdef USE_GTK
static int current_count;
static int current_finish;
static struct input_event *current_hold_quit;
#endif

#ifdef HAVE_XINPUT2
#ifndef X_XIGrabDevice
#define X_XIGrabDevice 51
#endif

#ifndef X_XIUngrabDevice
#define X_XIUngrabDevice 52
#endif

#ifndef X_XIAllowEvents
#define X_XIAllowEvents 53
#endif
#endif

/* Queue selection requests in `pending_selection_requests' if more
   than 0.  */
static int x_use_pending_selection_requests;

/* Like `next_kbd_event', but for use in X code.  */
#define X_NEXT_KBD_EVENT(ptr) \
  ((ptr) == kbd_buffer + KBD_BUFFER_SIZE - 1 ? kbd_buffer : (ptr) + 1)

static void x_push_selection_request (struct selection_input_event *);

/* Defer selection requests.  Between this and
   x_release_selection_requests, any selection requests can be
   processed by calling `x_handle_pending_selection_requests'.

   Also run through and queue all the selection events already in the
   keyboard buffer.  */
void
x_defer_selection_requests (void)
{
  union buffered_input_event *event;
  bool between;

  between = false;

  block_input ();
  if (!x_use_pending_selection_requests)
    {
      event = kbd_fetch_ptr;

      while (event != kbd_store_ptr)
	{
	  if (event->ie.kind == SELECTION_REQUEST_EVENT
	      || event->ie.kind == SELECTION_CLEAR_EVENT)
	    {
	      x_push_selection_request (&event->sie);

	      /* Mark this selection event as invalid.   */
	      SELECTION_EVENT_DPYINFO (&event->sie) = NULL;

	      /* Move the kbd_fetch_ptr along if doing so would not
		 result in any other events being skipped.  This
		 avoids exhausting the keyboard buffer with some
		 over-enthusiastic clipboard managers.  */
	      if (!between)
		{
		  kbd_fetch_ptr = X_NEXT_KBD_EVENT (event);

		  /* `detect_input_pending' will then recompute
		     whether or not pending input events exist.  */
		  input_pending = false;
		}
	    }
	  else
	    between = true;

	  event = X_NEXT_KBD_EVENT (event);
	}
    }

  x_use_pending_selection_requests++;
  unblock_input ();
}

static void
x_release_selection_requests (void)
{
  x_use_pending_selection_requests--;
}

void
x_release_selection_requests_and_flush (void)
{
  x_release_selection_requests ();

  if (!x_use_pending_selection_requests)
    x_handle_pending_selection_requests ();
}

struct x_selection_request_event
{
  /* The selection request event.  */
  struct selection_input_event se;

  /* The next unprocessed selection request event.  */
  struct x_selection_request_event *next;
};

/* Chain of unprocessed selection request events.  Used to handle
   selection requests inside long-lasting modal event loops, such as
   the drag-and-drop loop.  */

static struct x_selection_request_event *pending_selection_requests;

struct x_atom_ref
{
  /* Atom name.  */
  const char *name;

  /* Offset of atom in the display info structure.  */
  int offset;
};

/* List of all atoms that should be interned when connecting to a
   display.  */
static const struct x_atom_ref x_atom_refs[] =
  {
#define ATOM_REFS_INIT(string, member)				\
    { string, offsetof (struct x_display_info, member) },
    ATOM_REFS_INIT ("WM_PROTOCOLS", Xatom_wm_protocols)
    ATOM_REFS_INIT ("WM_TAKE_FOCUS", Xatom_wm_take_focus)
    ATOM_REFS_INIT ("WM_SAVE_YOURSELF", Xatom_wm_save_yourself)
    ATOM_REFS_INIT ("WM_DELETE_WINDOW", Xatom_wm_delete_window)
    ATOM_REFS_INIT ("WM_CHANGE_STATE", Xatom_wm_change_state)
    ATOM_REFS_INIT ("WM_STATE", Xatom_wm_state)
    ATOM_REFS_INIT ("WM_CONFIGURE_DENIED", Xatom_wm_configure_denied)
    ATOM_REFS_INIT ("WM_MOVED", Xatom_wm_window_moved)
    ATOM_REFS_INIT ("WM_CLIENT_LEADER", Xatom_wm_client_leader)
    ATOM_REFS_INIT ("WM_TRANSIENT_FOR", Xatom_wm_transient_for)
    ATOM_REFS_INIT ("Editres", Xatom_editres)
    ATOM_REFS_INIT ("CLIPBOARD", Xatom_CLIPBOARD)
    ATOM_REFS_INIT ("TIMESTAMP", Xatom_TIMESTAMP)
    ATOM_REFS_INIT ("TEXT", Xatom_TEXT)
    ATOM_REFS_INIT ("COMPOUND_TEXT", Xatom_COMPOUND_TEXT)
    ATOM_REFS_INIT ("UTF8_STRING", Xatom_UTF8_STRING)
    ATOM_REFS_INIT ("DELETE", Xatom_DELETE)
    ATOM_REFS_INIT ("MULTIPLE", Xatom_MULTIPLE)
    ATOM_REFS_INIT ("INCR", Xatom_INCR)
    ATOM_REFS_INIT ("_EMACS_TMP_",  Xatom_EMACS_TMP)
    ATOM_REFS_INIT ("_EMACS_SERVER_TIME_PROP", Xatom_EMACS_SERVER_TIME_PROP)
    ATOM_REFS_INIT ("TARGETS", Xatom_TARGETS)
    ATOM_REFS_INIT ("NULL", Xatom_NULL)
    ATOM_REFS_INIT ("ATOM", Xatom_ATOM)
    ATOM_REFS_INIT ("ATOM_PAIR", Xatom_ATOM_PAIR)
    ATOM_REFS_INIT ("CLIPBOARD_MANAGER", Xatom_CLIPBOARD_MANAGER)
    ATOM_REFS_INIT ("_XEMBED_INFO", Xatom_XEMBED_INFO)
    ATOM_REFS_INIT ("_MOTIF_WM_HINTS", Xatom_MOTIF_WM_HINTS)
    ATOM_REFS_INIT ("_EMACS_DRAG_ATOM", Xatom_EMACS_DRAG_ATOM)
    /* For properties of font.  */
    ATOM_REFS_INIT ("PIXEL_SIZE", Xatom_PIXEL_SIZE)
    ATOM_REFS_INIT ("AVERAGE_WIDTH", Xatom_AVERAGE_WIDTH)
    ATOM_REFS_INIT ("_MULE_BASELINE_OFFSET", Xatom_MULE_BASELINE_OFFSET)
    ATOM_REFS_INIT ("_MULE_RELATIVE_COMPOSE", Xatom_MULE_RELATIVE_COMPOSE)
    ATOM_REFS_INIT ("_MULE_DEFAULT_ASCENT", Xatom_MULE_DEFAULT_ASCENT)
    /* Ghostscript support.  */
    ATOM_REFS_INIT ("DONE", Xatom_DONE)
    ATOM_REFS_INIT ("PAGE", Xatom_PAGE)
    ATOM_REFS_INIT ("_EMACS_SCROLLBAR", Xatom_Scrollbar)
    ATOM_REFS_INIT ("_EMACS_HORIZONTAL_SCROLLBAR", Xatom_Horizontal_Scrollbar)
    ATOM_REFS_INIT ("_XEMBED", Xatom_XEMBED)
    /* EWMH */
    ATOM_REFS_INIT ("_NET_WM_STATE", Xatom_net_wm_state)
    ATOM_REFS_INIT ("_NET_WM_STATE_FULLSCREEN", Xatom_net_wm_state_fullscreen)
    ATOM_REFS_INIT ("_NET_WM_STATE_MAXIMIZED_HORZ",
		    Xatom_net_wm_state_maximized_horz)
    ATOM_REFS_INIT ("_NET_WM_STATE_MAXIMIZED_VERT",
		    Xatom_net_wm_state_maximized_vert)
    ATOM_REFS_INIT ("_NET_WM_STATE_STICKY", Xatom_net_wm_state_sticky)
    ATOM_REFS_INIT ("_NET_WM_STATE_SHADED", Xatom_net_wm_state_shaded)
    ATOM_REFS_INIT ("_NET_WM_STATE_HIDDEN", Xatom_net_wm_state_hidden)
    ATOM_REFS_INIT ("_NET_WM_WINDOW_TYPE", Xatom_net_window_type)
    ATOM_REFS_INIT ("_NET_WM_WINDOW_TYPE_TOOLTIP",
		    Xatom_net_window_type_tooltip)
    ATOM_REFS_INIT ("_NET_WM_ICON_NAME", Xatom_net_wm_icon_name)
    ATOM_REFS_INIT ("_NET_WM_NAME", Xatom_net_wm_name)
    ATOM_REFS_INIT ("_NET_SUPPORTED",  Xatom_net_supported)
    ATOM_REFS_INIT ("_NET_SUPPORTING_WM_CHECK", Xatom_net_supporting_wm_check)
    ATOM_REFS_INIT ("_NET_WM_WINDOW_OPACITY", Xatom_net_wm_window_opacity)
    ATOM_REFS_INIT ("_NET_ACTIVE_WINDOW", Xatom_net_active_window)
    ATOM_REFS_INIT ("_NET_FRAME_EXTENTS", Xatom_net_frame_extents)
    ATOM_REFS_INIT ("_NET_CURRENT_DESKTOP", Xatom_net_current_desktop)
    ATOM_REFS_INIT ("_NET_WORKAREA", Xatom_net_workarea)
    ATOM_REFS_INIT ("_NET_WM_SYNC_REQUEST", Xatom_net_wm_sync_request)
    ATOM_REFS_INIT ("_NET_WM_SYNC_REQUEST_COUNTER", Xatom_net_wm_sync_request_counter)
    ATOM_REFS_INIT ("_NET_WM_SYNC_FENCES", Xatom_net_wm_sync_fences)
    ATOM_REFS_INIT ("_NET_WM_BYPASS_COMPOSITOR", Xatom_net_wm_bypass_compositor)
    ATOM_REFS_INIT ("_NET_WM_FRAME_DRAWN", Xatom_net_wm_frame_drawn)
    ATOM_REFS_INIT ("_NET_WM_FRAME_TIMINGS", Xatom_net_wm_frame_timings)
    ATOM_REFS_INIT ("_NET_WM_USER_TIME", Xatom_net_wm_user_time)
    ATOM_REFS_INIT ("_NET_WM_USER_TIME_WINDOW", Xatom_net_wm_user_time_window)
    ATOM_REFS_INIT ("_NET_CLIENT_LIST_STACKING", Xatom_net_client_list_stacking)
    /* Session management */
    ATOM_REFS_INIT ("SM_CLIENT_ID", Xatom_SM_CLIENT_ID)
    ATOM_REFS_INIT ("_XSETTINGS_SETTINGS", Xatom_xsettings_prop)
    ATOM_REFS_INIT ("MANAGER", Xatom_xsettings_mgr)
    ATOM_REFS_INIT ("_NET_WM_STATE_SKIP_TASKBAR", Xatom_net_wm_state_skip_taskbar)
    ATOM_REFS_INIT ("_NET_WM_STATE_ABOVE", Xatom_net_wm_state_above)
    ATOM_REFS_INIT ("_NET_WM_STATE_BELOW", Xatom_net_wm_state_below)
    ATOM_REFS_INIT ("_NET_WM_OPAQUE_REGION", Xatom_net_wm_opaque_region)
    ATOM_REFS_INIT ("_NET_WM_PING", Xatom_net_wm_ping)
    ATOM_REFS_INIT ("_NET_WM_PID", Xatom_net_wm_pid)
#ifdef HAVE_XKB
    ATOM_REFS_INIT ("Meta", Xatom_Meta)
    ATOM_REFS_INIT ("Super", Xatom_Super)
    ATOM_REFS_INIT ("Hyper", Xatom_Hyper)
    ATOM_REFS_INIT ("ShiftLock", Xatom_ShiftLock)
    ATOM_REFS_INIT ("Alt", Xatom_Alt)
#endif
    /* DND source.  */
    ATOM_REFS_INIT ("XdndAware", Xatom_XdndAware)
    ATOM_REFS_INIT ("XdndSelection", Xatom_XdndSelection)
    ATOM_REFS_INIT ("XdndTypeList", Xatom_XdndTypeList)
    ATOM_REFS_INIT ("XdndActionCopy", Xatom_XdndActionCopy)
    ATOM_REFS_INIT ("XdndActionMove", Xatom_XdndActionMove)
    ATOM_REFS_INIT ("XdndActionLink", Xatom_XdndActionLink)
    ATOM_REFS_INIT ("XdndActionAsk", Xatom_XdndActionAsk)
    ATOM_REFS_INIT ("XdndActionPrivate", Xatom_XdndActionPrivate)
    ATOM_REFS_INIT ("XdndActionList", Xatom_XdndActionList)
    ATOM_REFS_INIT ("XdndActionDescription", Xatom_XdndActionDescription)
    ATOM_REFS_INIT ("XdndProxy", Xatom_XdndProxy)
    ATOM_REFS_INIT ("XdndEnter", Xatom_XdndEnter)
    ATOM_REFS_INIT ("XdndPosition", Xatom_XdndPosition)
    ATOM_REFS_INIT ("XdndStatus", Xatom_XdndStatus)
    ATOM_REFS_INIT ("XdndLeave", Xatom_XdndLeave)
    ATOM_REFS_INIT ("XdndDrop", Xatom_XdndDrop)
    ATOM_REFS_INIT ("XdndFinished", Xatom_XdndFinished)
    /* XDS source and target.  */
    ATOM_REFS_INIT ("XdndDirectSave0", Xatom_XdndDirectSave0)
    ATOM_REFS_INIT ("XdndActionDirectSave", Xatom_XdndActionDirectSave)
    ATOM_REFS_INIT ("text/plain", Xatom_text_plain)
    /* Motif drop protocol support.  */
    ATOM_REFS_INIT ("_MOTIF_DRAG_WINDOW", Xatom_MOTIF_DRAG_WINDOW)
    ATOM_REFS_INIT ("_MOTIF_DRAG_TARGETS", Xatom_MOTIF_DRAG_TARGETS)
    ATOM_REFS_INIT ("_MOTIF_DRAG_AND_DROP_MESSAGE",
		    Xatom_MOTIF_DRAG_AND_DROP_MESSAGE)
    ATOM_REFS_INIT ("_MOTIF_DRAG_INITIATOR_INFO",
		    Xatom_MOTIF_DRAG_INITIATOR_INFO)
    ATOM_REFS_INIT ("_MOTIF_DRAG_RECEIVER_INFO",
		    Xatom_MOTIF_DRAG_RECEIVER_INFO)
    ATOM_REFS_INIT ("XmTRANSFER_SUCCESS", Xatom_XmTRANSFER_SUCCESS)
    ATOM_REFS_INIT ("XmTRANSFER_FAILURE", Xatom_XmTRANSFER_FAILURE)
    /* Old OffiX (a.k.a. old KDE) drop protocol support.  */
    ATOM_REFS_INIT ("DndProtocol", Xatom_DndProtocol)
    ATOM_REFS_INIT ("_DND_PROTOCOL", Xatom_DND_PROTOCOL)
    /* Here are some atoms that are not actually used from C, just
       defined to make replying to selection requests fast.  */
    ATOM_REFS_INIT ("text/plain;charset=utf-8", Xatom_text_plain_charset_utf_8)
    ATOM_REFS_INIT ("LENGTH", Xatom_LENGTH)
    ATOM_REFS_INIT ("FILE_NAME", Xatom_FILE_NAME)
    ATOM_REFS_INIT ("CHARACTER_POSITION", Xatom_CHARACTER_POSITION)
    ATOM_REFS_INIT ("LINE_NUMBER", Xatom_LINE_NUMBER)
    ATOM_REFS_INIT ("COLUMN_NUMBER", Xatom_COLUMN_NUMBER)
    ATOM_REFS_INIT ("OWNER_OS", Xatom_OWNER_OS)
    ATOM_REFS_INIT ("HOST_NAME", Xatom_HOST_NAME)
    ATOM_REFS_INIT ("USER", Xatom_USER)
    ATOM_REFS_INIT ("CLASS", Xatom_CLASS)
    ATOM_REFS_INIT ("NAME", Xatom_NAME)
    ATOM_REFS_INIT ("SAVE_TARGETS", Xatom_SAVE_TARGETS)
  };

enum
{
  X_EVENT_NORMAL,
  X_EVENT_GOTO_OUT,
  X_EVENT_DROP
};

enum xembed_info
  {
    XEMBED_MAPPED = 1 << 0
  };

enum xembed_message
  {
    XEMBED_EMBEDDED_NOTIFY        = 0,
    XEMBED_WINDOW_ACTIVATE        = 1,
    XEMBED_WINDOW_DEACTIVATE      = 2,
    XEMBED_REQUEST_FOCUS          = 3,
    XEMBED_FOCUS_IN               = 4,
    XEMBED_FOCUS_OUT              = 5,
    XEMBED_FOCUS_NEXT             = 6,
    XEMBED_FOCUS_PREV             = 7,

    XEMBED_MODALITY_ON            = 10,
    XEMBED_MODALITY_OFF           = 11,
    XEMBED_REGISTER_ACCELERATOR   = 12,
    XEMBED_UNREGISTER_ACCELERATOR = 13,
    XEMBED_ACTIVATE_ACCELERATOR   = 14
  };

static bool x_alloc_nearest_color_1 (Display *, Colormap, XColor *);
static void x_raise_frame (struct frame *);
static void x_lower_frame (struct frame *);
static int x_io_error_quitter (Display *);
static struct terminal *x_create_terminal (struct x_display_info *);
static void x_frame_rehighlight (struct x_display_info *);

static void x_clip_to_row (struct window *, struct glyph_row *,
			   enum glyph_row_area, GC, XRectangle *);
static struct scroll_bar *x_window_to_scroll_bar (Display *, Window, int);
static struct frame *x_window_to_frame (struct x_display_info *, int);
static void x_scroll_bar_report_motion (struct frame **, Lisp_Object *,
                                        enum scroll_bar_part *,
                                        Lisp_Object *, Lisp_Object *,
                                        Time *);
static void x_horizontal_scroll_bar_report_motion (struct frame **, Lisp_Object *,
						   enum scroll_bar_part *,
						   Lisp_Object *, Lisp_Object *,
						   Time *);
static bool x_handle_net_wm_state (struct frame *, const XPropertyEvent *);
static void x_check_fullscreen (struct frame *);
static void x_check_expected_move (struct frame *, int, int);
static void x_sync_with_move (struct frame *, int, int, bool);
#ifndef HAVE_XINPUT2
static int handle_one_xevent (struct x_display_info *,
			      const XEvent *, int *,
			      struct input_event *);
#else
static int handle_one_xevent (struct x_display_info *,
			      XEvent *, int *,
			      struct input_event *);
#endif
#if ! (defined USE_X_TOOLKIT || defined USE_MOTIF) && defined USE_GTK
static int x_dispatch_event (XEvent *, Display *);
#endif
static void x_wm_set_window_state (struct frame *, int);
static void x_wm_set_icon_pixmap (struct frame *, ptrdiff_t);
static void x_initialize (void);

static bool x_get_current_wm_state (struct frame *, Window, int *, bool *, bool *);
static void x_update_opaque_region (struct frame *, XEvent *);

#ifdef HAVE_X_I18N
static int x_filter_event (struct x_display_info *, XEvent *);
#endif
static void x_clean_failable_requests (struct x_display_info *);

static struct frame *x_tooltip_window_to_frame (struct x_display_info *,
						Window, bool *);
static Window x_get_window_below (Display *, Window, int, int, int *, int *);
static void x_set_input_focus (struct x_display_info *, Window, Time);

#ifndef USE_TOOLKIT_SCROLL_BARS
static void x_scroll_bar_redraw (struct scroll_bar *);
#endif



/* Global state maintained during a drag-and-drop operation.  */

/* Flag that indicates if a drag-and-drop operation is in progress.  */
bool x_dnd_in_progress;

/* The frame where the drag-and-drop operation originated.  */
struct frame *x_dnd_frame;

/* That frame, but set when x_dnd_waiting_for_finish is true.  Used to
   prevent the frame from being deleted inside selection handlers and
   other callbacks.  */
struct frame *x_dnd_finish_frame;

/* Flag that indicates if a drag-and-drop operation is no longer in
   progress, but the nested event loop should continue to run, because
   handle_one_xevent is waiting for the drop target to return some
   important information.  */
bool x_dnd_waiting_for_finish;

/* Flag that means (when set in addition to
   `x_dnd_waiting_for_finish') to run the unsupported drop function
   with the given arguments.  */
static bool x_dnd_run_unsupported_drop_function;

/* The "before"-time of the unsupported drop.  */
static Time x_dnd_unsupported_drop_time;

/* The target window of the unsupported drop.  */
static Window x_dnd_unsupported_drop_window;

/* The Lisp data associated with the unsupported drop function.  */
static Lisp_Object x_dnd_unsupported_drop_data;

/* Whether or not to move the tooltip along with the mouse pointer
   during drag-and-drop.  */
static bool x_dnd_update_tooltip;

/* Monitor attribute list used for updating the tooltip position.  */
static Lisp_Object x_dnd_monitors;

/* The display the drop target that is supposed to send information is
   on.  */
static Display *x_dnd_finish_display;

/* State of the Motif drop operation.

   0 means nothing has happened, i.e. the event loop should not wait
   for the receiver to send any data.  1 means an XmDROP_START message
   was sent to the target, but no response has yet been received.  2
   means a response to our XmDROP_START message was received and the
   target accepted the drop, so Emacs should start waiting for the
   drop target to convert one of the special selections
   XmTRANSFER_SUCCESS or XmTRANSFER_FAILURE.  */
static int x_dnd_waiting_for_motif_finish;

/* The display the Motif drag receiver will send response data
   from.  */
static struct x_display_info *x_dnd_waiting_for_motif_finish_display;

/* Whether or not F1 was pressed during the drag-and-drop operation.

   Motif programs rely on this to decide whether or not help
   information about the drop site should be displayed.  */
static bool x_dnd_xm_use_help;

/* Whether or not Motif drag initiator info was set up.  */
static bool x_dnd_motif_setup_p;

/* The Motif drag atom used during the drag-and-drop operation.  */
static Atom x_dnd_motif_atom;

/* The target window we are waiting for an XdndFinished message
   from.  */
static Window x_dnd_pending_finish_target;

/* The protocol version of that target window.  */
static int x_dnd_waiting_for_finish_proto;

/* Whether or not it is OK for something to be dropped on the frame
   where the drag-and-drop operation originated.  */
static bool x_dnd_allow_current_frame;

/* Whether or not the `XdndTypeList' property has already been set on
   the drag frame.  */
static bool x_dnd_init_type_lists;

/* Whether or not to return a frame from `x_dnd_begin_drag_and_drop'.

   0 means to do nothing.  1 means to wait for the mouse to first exit
   `x_dnd_frame'.  2 means to wait for the mouse to move onto a frame,
   and 3 means to return `x_dnd_return_frame_object'.  */
static int x_dnd_return_frame;

/* The frame that should be returned by
   `x_dnd_begin_drag_and_drop'.  */
static struct frame *x_dnd_return_frame_object;

/* The last drop target window the mouse pointer moved over.  This can
   be different from `x_dnd_last_seen_toplevel' if that window had an
   XdndProxy.  */
static Window x_dnd_last_seen_window;

/* The last toplevel the mouse pointer moved over.  */
static Window x_dnd_last_seen_toplevel;

/* The window where the drop happened.  Normally None, but it is set
   when something is actually dropped.  */
static Window x_dnd_end_window;

/* The XDND protocol version of `x_dnd_last_seen_window'.  -1 means it
   did not support XDND.  */
static int x_dnd_last_protocol_version;

/* Whether or not the last seen window is actually one of our
   frames.  */
static bool x_dnd_last_window_is_frame;

/* The Motif drag and drop protocol style of `x_dnd_last_seen_window'.
   XM_DRAG_STYLE_NONE means the window does not support the Motif drag
   or drop protocol.  XM_DRAG_STYLE_DROP_ONLY means the window does
   not respond to any drag protocol messages, so only drops should be
   sent.  Any other value means that the window supports both the drag
   and drop protocols.  */
static int x_dnd_last_motif_style;

/* The timestamp where Emacs last acquired ownership of the
   `XdndSelection' selection.  */
static Time x_dnd_selection_timestamp;

/* The drop target window to which the rectangle below applies.  */
static Window x_dnd_mouse_rect_target;

/* A rectangle where XDND position messages should not be sent to the
   drop target if the mouse pointer lies within.  */
static XRectangle x_dnd_mouse_rect;

/* If not None, Emacs is waiting for an XdndStatus event from this
   window.  */
static Window x_dnd_waiting_for_status_window;

/* If .type != 0, an event that should be sent to .xclient.window
   upon receiving an XdndStatus event from said window.  */
static XEvent x_dnd_pending_send_position;

/* Whether or not that event corresponds to a button press.  */
static bool x_dnd_pending_send_position_button;

/* The root-window position of that event.  */
static int x_dnd_pending_send_position_root_x;

/* Likewise.  */
static int x_dnd_pending_send_position_root_y;

/* If true, send a drop from `x_dnd_finish_frame' to the pending
   status window after receiving all pending XdndStatus events.  */
static bool x_dnd_need_send_drop;

/* The protocol version of any such drop.  */
static int x_dnd_send_drop_proto;

/* The action the drop target actually chose to perform.

   Under XDND, this is set upon receiving the XdndFinished or
   XdndStatus messages from the drop target.

   Under Motif, this is changed upon receiving a XmDROP_START message
   in reply to our own.

   When dropping on a target that doesn't support any drag-and-drop
   protocol, this is set to the atom XdndActionPrivate.  */
static Atom x_dnd_action;

/* The symbol to return from `x-begin-drag' if non-nil.  Takes
   precedence over `x_dnd_action`.  */
static Lisp_Object x_dnd_action_symbol;

/* The action we want the drop target to perform.  The drop target may
   elect to perform some different action, which is guaranteed to be
   in `x_dnd_action' upon completion of a drop.  */
static Atom x_dnd_wanted_action;

/* The set of optional actions available to a Motif drop target
   computed at the start of the drag-and-drop operation.  */
static uint8_t x_dnd_motif_operations;

/* The preferred optional action out of that set.  Only takes effect
   if `x_dnd_action' is XdndAsk.  */
static uint8_t x_dnd_first_motif_operation;

/* Array of selection targets available to the drop target.  */
static Atom *x_dnd_targets;

/* The number of elements in that array.  */
static int x_dnd_n_targets;

/* The old window attributes of the root window before the
   drag-and-drop operation started.  It is used to keep the old event
   mask around, since that should be restored after the operation
   finishes.  */
static XWindowAttributes x_dnd_old_window_attrs;

/* Whether or not `x_dnd_cleaup_drag_and_drop' should actually clean
   up the drag and drop operation.  */
static bool x_dnd_unwind_flag;

/* The frame for which `x-dnd-movement-function' should be called.  */
static struct frame *x_dnd_movement_frame;

/* The coordinates which the movement function should be called
   with.  */
static int x_dnd_movement_x, x_dnd_movement_y;

/* The frame for which `x-dnd-wheel-function' should be called.  */
static struct frame *x_dnd_wheel_frame;

/* The coordinates which the wheel function should be called with.  */
static int x_dnd_wheel_x, x_dnd_wheel_y;

/* The button that was pressed.  */
static int x_dnd_wheel_button;

/* The modifier state when the button was pressed.  */
static int x_dnd_wheel_state;

/* When the button was pressed.  */
static Time x_dnd_wheel_time;

#ifdef HAVE_XKB
/* The keyboard state during the drag-and-drop operation.  */
static unsigned int x_dnd_keyboard_state;
#endif

/* jmp_buf that gets us out of the IO error handler if an error occurs
   terminating DND as part of the display disconnect handler.  */
static sigjmp_buf x_dnd_disconnect_handler;

/* Whether or not the current invocation of handle_one_xevent
   happened inside the drag_and_drop event loop.  */
static bool x_dnd_inside_handle_one_xevent;

/* The recursive edit depth when the drag-and-drop operation was
   started.  */
static int x_dnd_recursion_depth;

/* The cons cell containing the selection alias between the Motif drag
   selection and `XdndSelection'.  The car and cdr are only set when
   initiating Motif drag-and-drop for the first time.  */
static Lisp_Object x_dnd_selection_alias_cell;

/* The last known position of the tooltip window.  */
static int x_dnd_last_tooltip_x, x_dnd_last_tooltip_y;

/* Whether or not those values are actually known yet.  */
static bool x_dnd_last_tooltip_valid;

#ifdef HAVE_XINPUT2
/* The master pointer device being used for the drag-and-drop
   operation.  */
static int x_dnd_pointer_device;

/* The keyboard device attached to that pointer device.  */
static int x_dnd_keyboard_device;
#endif

/* Structure describing a single window that can be the target of
   drag-and-drop operations.  */
struct x_client_list_window
{
  /* The window itself.  */
  Window window;

  /* The display that window is on.  */
  Display *dpy;

  /* Its X and Y coordinates from the root window.  */
  int x, y;

  /* The width and height of the window.  */
  int width, height;

  /* A bitmask describing events Emacs was listening for from the
     window before some extra events were added in
     `x_dnd_compute_toplevels'.  */
  long previous_event_mask;

  /* The window manager state of the window.  */
  unsigned long wm_state;

  /* The next window in this list.  */
  struct x_client_list_window *next;

  /* The extents of the frame window in each direction.  */
  int frame_extents_left;
  int frame_extents_right;
  int frame_extents_top;
  int frame_extents_bottom;

#ifdef HAVE_XSHAPE
  /* The border width of this window.  */
  int border_width;

  /* The number of rectangles composing the input shape.  */
  int n_input_rects;

  /* The rectangles making up the input shape.  */
  XRectangle *input_rects;

  /* The rectangles making up the bounding shape.  */
  XRectangle *bounding_rects;

  /* The number of rectangles composing the bounding shape.  */
  int n_bounding_rects;
#endif

  /* The Motif protocol style of this window, if any.  */
  uint8_t xm_protocol_style;

  /* Whether or not the window is mapped.  */
  bool mapped_p;
};

/* List of all toplevels in stacking order, from top to bottom.  */
static struct x_client_list_window *x_dnd_toplevels;

/* Whether or not the window manager supports the required features
   for `x_dnd_toplevels' to work.  */
static bool x_dnd_use_toplevels;



/* Motif drag-and-drop protocol support.  */

/* Pointer to a variable which stores whether or not an X error
   occurred while trying to create the Motif drag window.  */
static volatile bool *xm_drag_window_error;

typedef enum xm_byte_order
  {
    XM_BYTE_ORDER_LSB_FIRST = 'l',
    XM_BYTE_ORDER_MSB_FIRST = 'B',
#ifndef WORDS_BIGENDIAN
    XM_BYTE_ORDER_CUR_FIRST = 'l',
#else
    XM_BYTE_ORDER_CUR_FIRST = 'B',
#endif
  } xm_byte_order;

#ifdef ENABLE_CHECKING

#define SWAPCARD32(l)				\
  {						\
    struct { unsigned t : 32; } bit32;		\
    char n, *tp = (char *) &bit32;		\
    bit32.t = l;				\
    n = tp[0]; tp[0] = tp[3]; tp[3] = n;	\
    n = tp[1]; tp[1] = tp[2]; tp[2] = n;	\
    l = bit32.t;				\
  }

#define SWAPCARD16(s)				\
  {						\
    struct { unsigned t : 16; } bit16;		\
    char n, *tp = (char *) &bit16;		\
    bit16.t = s;				\
    n = tp[0]; tp[0] = tp[1]; tp[1] = n;	\
    s = bit16.t;				\
  }

#else
#define SWAPCARD32(l)	((l) = bswap_32 (l))
#define SWAPCARD16(l)	((l) = bswap_16 (l))
#endif

typedef struct xm_targets_table_header
{
  /* BYTE   */ uint8_t byte_order;
  /* BYTE   */ uint8_t protocol;

  /* CARD16 */ uint16_t target_list_count;
  /* CARD32 */ uint32_t total_data_size;
} xm_targets_table_header;

typedef struct xm_targets_table_rec
{
  /* CARD16 */ uint16_t n_targets;
  /* CARD32 */ uint32_t targets[FLEXIBLE_ARRAY_MEMBER];
} xm_targets_table_rec;

typedef struct xm_drop_start_message
{
  /* BYTE   */ uint8_t reason;
  /* BYTE   */ uint8_t byte_order;

  /* CARD16 */ uint16_t side_effects;
  /* CARD32 */ uint32_t timestamp;
  /* CARD16 */ uint16_t x, y;
  /* CARD32 */ uint32_t index_atom;
  /* CARD32 */ uint32_t source_window;
} xm_drop_start_message;

typedef struct xm_drop_start_reply
{
  /* BYTE   */ uint8_t reason;
  /* BYTE   */ uint8_t byte_order;

  /* CARD16 */ uint16_t side_effects;
  /* CARD16 */ uint16_t better_x;
  /* CARD16 */ uint16_t better_y;
} xm_drop_start_reply;

typedef struct xm_drag_initiator_info
{
  /* BYTE   */ uint8_t byteorder;
  /* BYTE   */ uint8_t protocol;

  /* CARD16 */ uint16_t table_index;
  /* CARD32 */ uint32_t selection;
} xm_drag_initiator_info;

typedef struct xm_drag_receiver_info
{
  /* BYTE   */ uint8_t byteorder;
  /* BYTE   */ uint8_t protocol;

  /* BYTE   */ uint8_t protocol_style;
  /* BYTE   */ uint8_t unspecified0;
  /* CARD32 */ uint32_t unspecified1;
  /* CARD32 */ uint32_t unspecified2;
  /* CARD32 */ uint32_t unspecified3;
} xm_drag_receiver_info;

typedef struct xm_top_level_enter_message
{
  /* BYTE   */ uint8_t reason;
  /* BYTE   */ uint8_t byteorder;

  /* CARD16 */ uint16_t zero;
  /* CARD32 */ uint32_t timestamp;
  /* CARD32 */ uint32_t source_window;
  /* CARD32 */ uint32_t index_atom;
} xm_top_level_enter_message;

typedef struct xm_drag_motion_message
{
  /* BYTE   */ uint8_t reason;
  /* BYTE   */ uint8_t byteorder;

  /* CARD16 */ uint16_t side_effects;
  /* CARD32 */ uint32_t timestamp;
  /* CARD16 */ uint16_t x, y;
} xm_drag_motion_message;

typedef struct xm_drag_motion_reply
{
  /* BYTE   */ uint8_t reason;
  /* BYTE   */ uint8_t byte_order;

  /* CARD16 */ uint16_t side_effects;
  /* CARD32 */ uint32_t timestamp;
  /* CARD16 */ uint16_t better_x;
  /* CARD16 */ uint16_t better_y;
} xm_drag_motion_reply;

typedef struct xm_top_level_leave_message
{
  /* BYTE   */ uint8_t reason;
  /* BYTE   */ uint8_t byteorder;

  /* CARD16 */ uint16_t zero;
  /* CARD32 */ uint32_t timestamp;
  /* CARD32 */ uint32_t source_window;
} xm_top_level_leave_message;

#define XM_DRAG_SIDE_EFFECT(op, site, ops, act)		\
  ((op) | ((site) << 4) | ((ops) << 8) | ((act) << 12))

/* Some of the macros below are temporarily unused.  */

#define XM_DRAG_SIDE_EFFECT_OPERATION(effect)	((effect) & 0xf)
#define XM_DRAG_SIDE_EFFECT_SITE_STATUS(effect)	(((effect) & 0xf0) >> 4)
/* #define XM_DRAG_SIDE_EFFECT_OPERATIONS(effect)	(((effect) & 0xf00) >> 8) */
#define XM_DRAG_SIDE_EFFECT_DROP_ACTION(effect)	(((effect) & 0xf000) >> 12)

enum xm_drag_operation
  {
    XM_DRAG_NOOP     = 0,
    XM_DRAG_MOVE     = (1L << 0),
    XM_DRAG_COPY     = (1L << 1),
    XM_DRAG_LINK     = (1L << 2),
    XM_DRAG_LINK_REC = 3,
  };

#define XM_DRAG_OPERATION_IS_LINK(op)	((op) == XM_DRAG_LINK		\
					 || (op) == XM_DRAG_LINK_REC)

enum xm_drag_action
  {
    XM_DROP_ACTION_DROP	       = 0,
    XM_DROP_ACTION_DROP_HELP   = 1,
    XM_DROP_ACTION_DROP_CANCEL = 2,
  };

#define XM_DRAG_REASON(originator, code)	((code) | ((originator) << 7))
#define XM_DRAG_REASON_ORIGINATOR(reason)	(((reason) & 0x80) ? 1 : 0)
#define XM_DRAG_REASON_CODE(reason)		((reason) & 0x7f)

enum xm_drag_reason
  {
    XM_DRAG_REASON_DROP_START	   = 5,
    XM_DRAG_REASON_TOP_LEVEL_ENTER = 0,
    XM_DRAG_REASON_TOP_LEVEL_LEAVE = 1,
    XM_DRAG_REASON_DRAG_MOTION	   = 2,
  };

enum xm_drag_originator
  {
    XM_DRAG_ORIGINATOR_INITIATOR = 0,
    XM_DRAG_ORIGINATOR_RECEIVER	 = 1,
  };

enum xm_drag_style
  {
    /* The values ending with _REC should be treated as equivalent to
       the ones without in messages from the receiver.  */
    XM_DRAG_STYLE_NONE		= 0,
    XM_DRAG_STYLE_DROP_ONLY	= 1,
    XM_DRAG_STYLE_DROP_ONLY_REC = 3,
    XM_DRAG_STYLE_DYNAMIC	= 5,
    XM_DRAG_STYLE_DYNAMIC_REC	= 2,
    XM_DRAG_STYLE_DYNAMIC_REC1	= 4,
  };

#define XM_DRAG_STYLE_IS_DROP_ONLY(n)	((n) == XM_DRAG_STYLE_DROP_ONLY	\
					 || (n) == XM_DRAG_STYLE_DROP_ONLY_REC)
#define XM_DRAG_STYLE_IS_DYNAMIC(n)	((n) == XM_DRAG_STYLE_DYNAMIC	\
					 || (n) == XM_DRAG_STYLE_DYNAMIC_REC \
					 || (n) == XM_DRAG_STYLE_DYNAMIC_REC1)

enum xm_drop_site_status
  {
    XM_DROP_SITE_VALID	 = 3,
    XM_DROP_SITE_INVALID = 2,
    XM_DROP_SITE_NONE	 = 1,
  };

/* The version of the Motif drag-and-drop protocols that Emacs
   supports.  */
#define XM_DRAG_PROTOCOL_VERSION	0

static uint8_t
xm_side_effect_from_action (struct x_display_info *dpyinfo, Atom action)
{
  if (action == dpyinfo->Xatom_XdndActionCopy)
    return XM_DRAG_COPY;
  else if (action == dpyinfo->Xatom_XdndActionMove)
    return XM_DRAG_MOVE;
  else if (action == dpyinfo->Xatom_XdndActionLink)
    return XM_DRAG_LINK;
  else if (action == dpyinfo->Xatom_XdndActionAsk)
    return x_dnd_first_motif_operation;

  return XM_DRAG_NOOP;
}

static uint8_t
xm_operations_from_actions (struct x_display_info *dpyinfo,
			    Atom *ask_actions, int n_ask_actions)
{
  int i;
  uint8_t flags;

  flags = 0;

  for (i = 0; i < n_ask_actions; ++i)
    {
      if (ask_actions[i] == dpyinfo->Xatom_XdndActionCopy)
	flags |= XM_DRAG_COPY;
      else if (ask_actions[i] == dpyinfo->Xatom_XdndActionMove)
	flags |= XM_DRAG_MOVE;
      else if (ask_actions[i] == dpyinfo->Xatom_XdndActionLink)
	flags |= XM_DRAG_LINK;
    }

  return flags;
}

static int
xm_read_targets_table_header (uint8_t *bytes, ptrdiff_t length,
			      xm_targets_table_header *header_return,
			      xm_byte_order *byteorder_return)
{
  if (length < 8)
    return -1;

  header_return->byte_order = *byteorder_return = *(bytes++);
  header_return->protocol = *(bytes++);

  header_return->target_list_count = *(uint16_t *) bytes;
  header_return->total_data_size = *(uint32_t *) (bytes + 2);

  if (header_return->byte_order != XM_BYTE_ORDER_CUR_FIRST)
    {
      SWAPCARD16 (header_return->target_list_count);
      SWAPCARD32 (header_return->total_data_size);
    }

  header_return->byte_order = XM_BYTE_ORDER_CUR_FIRST;

  return 8;
}

static xm_targets_table_rec *
xm_read_targets_table_rec (uint8_t *bytes, ptrdiff_t length,
			   xm_byte_order byteorder)
{
  uint16_t nitems, i;
  xm_targets_table_rec *rec;

  if (length < 2)
    return NULL;

  nitems = *(uint16_t *) bytes;

  if (byteorder != XM_BYTE_ORDER_CUR_FIRST)
    SWAPCARD16 (nitems);

  if (length < 2 + nitems * 4)
    return NULL;

  rec = xmalloc (FLEXSIZEOF (struct xm_targets_table_rec,
			     targets, nitems * 4));
  rec->n_targets = nitems;

  for (i = 0; i < nitems; ++i)
    {
      rec->targets[i] = ((uint32_t *) (bytes + 2))[i];

      if (byteorder != XM_BYTE_ORDER_CUR_FIRST)
	SWAPCARD32 (rec->targets[i]);
    }

  return rec;
}

static int
xm_find_targets_table_idx (xm_targets_table_header *header,
			   xm_targets_table_rec **recs,
			   Atom *sorted_targets, int ntargets)
{
  int j;
  uint16_t i;
  uint32_t *targets;

  targets = alloca (sizeof *targets * ntargets);

  for (j = 0; j < ntargets; ++j)
    targets[j] = sorted_targets[j];

  for (i = 0; i < header->target_list_count; ++i)
    {
      if (recs[i]->n_targets == ntargets
	  && !memcmp (&recs[i]->targets, targets,
		      sizeof *targets * ntargets))
	return i;
    }

  return -1;
}

static int
x_atoms_compare (const void *a, const void *b)
{
  return *(Atom *) a - *(Atom *) b;
}

static void
xm_write_targets_table (Display *dpy, Window wdesc,
			Atom targets_table_atom,
			xm_targets_table_header *header,
			xm_targets_table_rec **recs)
{
  uint8_t *header_buffer, *ptr, *rec_buffer;
  ptrdiff_t rec_buffer_size;
  uint16_t i, j;

  header_buffer = alloca (8);
  ptr = header_buffer;

  *(header_buffer++) = header->byte_order;
  *(header_buffer++) = header->protocol;
  *((uint16_t *) header_buffer) = header->target_list_count;
  *((uint32_t *) (header_buffer + 2)) = header->total_data_size;

  rec_buffer = xmalloc (600);
  rec_buffer_size = 600;

  XChangeProperty (dpy, wdesc, targets_table_atom,
		   targets_table_atom, 8, PropModeReplace,
		   (unsigned char *) ptr, 8);

  for (i = 0; i < header->target_list_count; ++i)
    {
      if (rec_buffer_size < 2 + recs[i]->n_targets * 4)
	{
	  rec_buffer_size = 2 + recs[i]->n_targets * 4;
	  rec_buffer = xrealloc (rec_buffer, rec_buffer_size);
	}

      *((uint16_t *) rec_buffer) = recs[i]->n_targets;

      for (j = 0; j < recs[i]->n_targets; ++j)
	((uint32_t *) (rec_buffer + 2))[j] = recs[i]->targets[j];

      XChangeProperty (dpy, wdesc, targets_table_atom,
		       targets_table_atom, 8, PropModeAppend,
		       (unsigned char *) rec_buffer,
		       2 + recs[i]->n_targets * 4);
    }

  xfree (rec_buffer);
}

static void
xm_write_drag_initiator_info (Display *dpy, Window wdesc,
			      Atom prop_name, Atom type_name,
			      xm_drag_initiator_info *info)
{
  uint8_t *buf;

  buf = alloca (8);
  buf[0] = info->byteorder;
  buf[1] = info->protocol;

  if (info->byteorder != XM_BYTE_ORDER_CUR_FIRST)
    {
      SWAPCARD16 (info->table_index);
      SWAPCARD16 (info->selection);
    }

  *((uint16_t *) (buf + 2)) = info->table_index;
  *((uint32_t *) (buf + 4)) = info->selection;

  XChangeProperty (dpy, wdesc, prop_name, type_name, 8,
		   PropModeReplace, (unsigned char *) buf, 8);
}

static int
xm_drag_window_error_handler (Display *display, XErrorEvent *event)
{
  if (xm_drag_window_error)
    *xm_drag_window_error = true;

  return 0;
}

static _Noreturn int
xm_drag_window_io_error_handler (Display *dpy)
{
  /* DPY isn't created through GDK, so it doesn't matter if we don't
     crash here.  */
  siglongjmp (x_dnd_disconnect_handler, 1);
}

/* Determine whether or not WINDOW exists on DPYINFO by selecting for
   input from it.  */
static bool
x_special_window_exists_p (struct x_display_info *dpyinfo,
			   Window window)
{
  bool rc;

  x_catch_errors (dpyinfo->display);
  XSelectInput (dpyinfo->display, window,
		StructureNotifyMask);
  rc = !x_had_errors_p (dpyinfo->display);
  x_uncatch_errors_after_check ();

  return rc;
}

/* Drag window creation strategy (very tricky, but race-free):

   First look for _MOTIF_DRAG_WINDOW.  If it is already present,
   return it immediately to avoid the overhead of new display
   connections.

   Otherwise, create a new connection to the display. In that
   connection, create a window, which will be the new drag window. Set
   the client disconnect mode of the new connection to
   RetainPermanent, and close it.

   Grab the current display.  Look up _MOTIF_DRAG_WINDOW, the current
   drag window.  If it exists (which means _MOTIF_DRAG_WINDOW was
   created between the first step and now), kill the client that
   created the new drag window to free the client slot on the X
   server.  Otherwise, set _MOTIF_DRAG_WINDOW to the new drag window.

   Ungrab the display and return whichever window is currently in
   _MOTIF_DRAG_WINDOW.  */

static Window
xm_get_drag_window_1 (struct x_display_info *dpyinfo)
{
  Atom actual_type;
  int rc, actual_format;
  unsigned long nitems, bytes_remaining;
  unsigned char *tmp_data = NULL;
  Window drag_window;
  XSetWindowAttributes attrs;
  Display *temp_display;
  Emacs_XErrorHandler old_handler;
  Emacs_XIOErrorHandler old_io_handler;

  /* This is volatile because GCC mistakenly warns about them being
     clobbered by longjmp.  */
  volatile bool error;

  drag_window = None;
  rc = XGetWindowProperty (dpyinfo->display, dpyinfo->root_window,
			   dpyinfo->Xatom_MOTIF_DRAG_WINDOW,
			   0, 1, False, XA_WINDOW, &actual_type,
			   &actual_format, &nitems, &bytes_remaining,
			   &tmp_data) == Success;

  if (rc && actual_type == XA_WINDOW
      && actual_format == 32 && nitems == 1
      && tmp_data)
    {
      drag_window = *(Window *) tmp_data;

      /* This has the side effect of selecting for
	 StructureNotifyMask, meaning that we will get notifications
	 once it is deleted.  */
      rc = x_special_window_exists_p (dpyinfo, drag_window);

      if (!rc)
	drag_window = None;
    }

  if (tmp_data)
    XFree (tmp_data);

  if (drag_window == None)
    {
      block_input ();
      old_io_handler = XSetIOErrorHandler (xm_drag_window_io_error_handler);

      if (sigsetjmp (x_dnd_disconnect_handler, 1))
	{
	  XSetIOErrorHandler (old_io_handler);
	  unblock_input ();

	  return None;
	}

      unrequest_sigio ();
      temp_display = XOpenDisplay (XDisplayString (dpyinfo->display));
      request_sigio ();

      if (!temp_display)
	{
	  XSetIOErrorHandler (old_io_handler);
	  unblock_input ();

	  return None;
	}

      error = false;
      xm_drag_window_error = &error;

      XSetCloseDownMode (temp_display, RetainPermanent);
      old_handler = XSetErrorHandler (xm_drag_window_error_handler);

      attrs.override_redirect = True;
      drag_window = XCreateWindow (temp_display, DefaultRootWindow (temp_display),
				   -1, -1, 1, 1, 0, CopyFromParent, InputOnly,
				   CopyFromParent, CWOverrideRedirect, &attrs);

      /* Handle all errors now.   */
      XSync (temp_display, False);

      /* Some part of the drag window creation process failed, so
	 punt.  Release all resources too.  */
      if (error)
	{
	  XSetCloseDownMode (temp_display, DestroyAll);
	  drag_window = None;
	}

      xm_drag_window_error = NULL;

      /* FIXME: why does XCloseDisplay hang if SIGIO arrives and there
	 are multiple displays? */
      unrequest_sigio ();
      XCloseDisplay (temp_display);
      request_sigio ();

      XSetErrorHandler (old_handler);
      XSetIOErrorHandler (old_io_handler);

      /* Make sure the drag window created is actually valid for the
	 current display, and the XOpenDisplay above didn't
	 accidentally connect to some other display.  */
      if (!x_special_window_exists_p (dpyinfo, drag_window))
	drag_window = None;
      unblock_input ();

      if (drag_window != None)
	{
	  XGrabServer (dpyinfo->display);

	  x_catch_errors (dpyinfo->display);
	  tmp_data = NULL;

	  rc = XGetWindowProperty (dpyinfo->display, dpyinfo->root_window,
				   dpyinfo->Xatom_MOTIF_DRAG_WINDOW,
				   0, 1, False, XA_WINDOW, &actual_type,
				   &actual_format, &nitems, &bytes_remaining,
				   &tmp_data) == Success;

	  if (rc && actual_type == XA_WINDOW
	      && actual_format == 32 && nitems == 1
	      && tmp_data
	      && x_special_window_exists_p (dpyinfo,
					    *(Window *) tmp_data))
	    {
	      /* Kill the client now to avoid leaking a client slot,
		 which is a limited resource.  */
	      XKillClient (dpyinfo->display, drag_window);
	      drag_window = *(Window *) tmp_data;
	    }
	  else
	    XChangeProperty (dpyinfo->display, dpyinfo->root_window,
			     dpyinfo->Xatom_MOTIF_DRAG_WINDOW,
			     XA_WINDOW, 32, PropModeReplace,
			     (unsigned char *) &drag_window, 1);

	  if (tmp_data)
	    XFree (tmp_data);

	  if (x_had_errors_p (dpyinfo->display))
	    drag_window = None;
	  x_uncatch_errors ();

	  XUngrabServer (dpyinfo->display);
	}
    }

  return drag_window;
}

static Window
xm_get_drag_window (struct x_display_info *dpyinfo)
{
  if (dpyinfo->motif_drag_window != None)
    return dpyinfo->motif_drag_window;

  dpyinfo->motif_drag_window = xm_get_drag_window_1 (dpyinfo);
  return dpyinfo->motif_drag_window;
}

static int
xm_setup_dnd_targets (struct x_display_info *dpyinfo,
		      Atom *targets, int ntargets)
{
  Window drag_window;
  Atom *targets_sorted, actual_type;
  unsigned char *tmp_data = NULL;
  unsigned long nitems, bytes_remaining;
  int rc, actual_format, idx;
  bool had_errors;
  xm_targets_table_header header;
  xm_targets_table_rec **recs UNINIT;
  xm_byte_order byteorder;
  uint8_t *data;
  ptrdiff_t total_bytes, total_items, i;
  uint32_t size, target_count;

 retry_drag_window:

  drag_window = xm_get_drag_window (dpyinfo);

  if (drag_window == None || ntargets > 64)
    return -1;

  targets_sorted = xmalloc (sizeof *targets * ntargets);
  memcpy (targets_sorted, targets,
	  sizeof *targets * ntargets);
  qsort (targets_sorted, ntargets,
	 sizeof (Atom), x_atoms_compare);

  XGrabServer (dpyinfo->display);

  x_catch_errors (dpyinfo->display);
  rc = XGetWindowProperty (dpyinfo->display, drag_window,
			   dpyinfo->Xatom_MOTIF_DRAG_TARGETS,
			   0L, LONG_MAX, False,
			   dpyinfo->Xatom_MOTIF_DRAG_TARGETS,
			   &actual_type, &actual_format, &nitems,
			   &bytes_remaining, &tmp_data) == Success;
  had_errors = x_had_errors_p (dpyinfo->display);
  x_uncatch_errors_after_check ();

  /* The drag window is probably invalid, so remove our record of
     it.  */
  if (had_errors)
    {
      dpyinfo->motif_drag_window = None;
      XUngrabServer (dpyinfo->display);

      goto retry_drag_window;
    }

  if (rc && tmp_data && !bytes_remaining
      && actual_type == dpyinfo->Xatom_MOTIF_DRAG_TARGETS
      && actual_format == 8)
    {
      data = (uint8_t *) tmp_data;
      if (xm_read_targets_table_header ((uint8_t *) tmp_data,
					nitems, &header,
					&byteorder) == 8)
	{
	  data += 8;
	  nitems -= 8;
	  total_bytes = 0;
	  total_items = 0;

	  /* The extra rec is used to store a new target list if a
	     preexisting one doesn't already exist.  */
	  recs = xmalloc ((header.target_list_count + 1)
			  * sizeof *recs);

	  while (total_items < header.target_list_count)
	    {
	      recs[total_items] = xm_read_targets_table_rec (data + total_bytes,
							     nitems, byteorder);

	      if (!recs[total_items])
		break;

	      total_bytes += 2 + recs[total_items]->n_targets * 4;
	      nitems -= 2 + recs[total_items]->n_targets * 4;
	      total_items++;
	    }

	  if (header.target_list_count != total_items
	      || header.total_data_size != 8 + total_bytes)
	    {
	      for (i = 0; i < total_items; ++i)
		{
		  if (recs[i])
		      xfree (recs[i]);
		  else
		    break;
		}

	      xfree (recs);

	      rc = false;
	    }
	}
      else
	rc = false;
    }
  else
    rc = false;

  if (tmp_data)
    XFree (tmp_data);

  /* Now rc means whether or not the target lists weren't updated and
     shouldn't be written to the drag window.  */

  if (!rc)
    {
      header.byte_order = XM_BYTE_ORDER_CUR_FIRST;
      header.protocol = XM_DRAG_PROTOCOL_VERSION;
      header.target_list_count = 1;
      header.total_data_size = 8 + 2 + ntargets * 4;

      recs = xmalloc (sizeof *recs);
      recs[0] = xmalloc (FLEXSIZEOF (struct xm_targets_table_rec,
				     targets, ntargets * 4));

      recs[0]->n_targets = ntargets;

      for (i = 0; i < ntargets; ++i)
	recs[0]->targets[i] = targets_sorted[i];

      idx = 0;
    }
  else
    {
      idx = xm_find_targets_table_idx (&header, recs,
				       targets_sorted,
				       ntargets);

      if (idx == -1)
	{
	  target_count = header.target_list_count;
	  rc = false;

	  if (ckd_add (&header.target_list_count, header.target_list_count, 1)
	      || ckd_mul (&size, ntargets, 4)
	      || ckd_add (&header.total_data_size, header.total_data_size, size)
	      || ckd_add (&header.total_data_size, header.total_data_size, 2))
	    {
	      /* Overflow, remove every entry from the targets table
		 and add one for our current targets list.  This
		 confuses real Motif but not GTK 2.x, and there is no
		 other choice.  */

	      for (i = 0; i < target_count; ++i)
		xfree (recs[i]);

	      xfree (recs);

	      header.byte_order = XM_BYTE_ORDER_CUR_FIRST;
	      header.protocol = XM_DRAG_PROTOCOL_VERSION;
	      header.target_list_count = 1;
	      header.total_data_size = 8 + 2 + ntargets * 4;

	      recs = xmalloc (sizeof *recs);
	      recs[0] = xmalloc (FLEXSIZEOF (struct xm_targets_table_rec,
					     targets, ntargets * 4));

	      recs[0]->n_targets = ntargets;

	      for (i = 0; i < ntargets; ++i)
		recs[0]->targets[i] = targets_sorted[i];

	      idx = 0;
	    }
	  else
	    {
	      recs[header.target_list_count - 1]
		= xmalloc (FLEXSIZEOF (struct xm_targets_table_rec,
				       targets, ntargets * 4));
	      recs[header.target_list_count - 1]->n_targets = ntargets;

	      for (i = 0; i < ntargets; ++i)
		recs[header.target_list_count - 1]->targets[i] = targets_sorted[i];

	      idx = header.target_list_count - 1;
	    }
	}
    }

  if (!rc)
    {
      /* Some implementations of Motif DND set the protocol version of
	 just the targets table to 1 without actually changing the
	 data format.  To avoid confusing Motif when that happens, set
	 it back to 0.  There will probably be no more updates to the
	 protocol either.  */
      header.protocol = XM_DRAG_PROTOCOL_VERSION;

      x_catch_errors (dpyinfo->display);
      xm_write_targets_table (dpyinfo->display, drag_window,
			      dpyinfo->Xatom_MOTIF_DRAG_TARGETS,
			      &header, recs);
      /* Presumably we got a BadAlloc upon writing the targets
	 table.  */
      if (x_had_errors_p (dpyinfo->display))
	idx = -1;
      x_uncatch_errors_after_check ();
    }

  XUngrabServer (dpyinfo->display);

  for (i = 0; i < header.target_list_count; ++i)
    xfree (recs[i]);

  xfree (recs);
  xfree (targets_sorted);

  return idx;
}

/* Allocate an atom that will be used for the Motif selection during
   the drag-and-drop operation.

   Grab the server, and then retrieve a list of atoms named
   _EMACS_DRAG_ATOM from the root window.  Find the first atom that
   has no selection owner, own it and return it.  If there is no such
   atom, add a unique atom to the end of the list and return that
   instead.  */

static Atom
xm_get_drag_atom_1 (struct x_display_info *dpyinfo,
		    struct frame *source_frame)
{
  Atom actual_type, *atoms, atom;
  unsigned long nitems, bytes_remaining;
  unsigned char *tmp_data;
  int rc, actual_format;
  unsigned long i;
  char *buffer;
  Window owner;

  /* Make sure this operation is done atomically.  */
  XGrabServer (dpyinfo->display);

  rc = XGetWindowProperty (dpyinfo->display, dpyinfo->root_window,
			   dpyinfo->Xatom_EMACS_DRAG_ATOM,
			   0, LONG_MAX, False, XA_ATOM, &actual_type,
			   &actual_format, &nitems, &bytes_remaining,
			   &tmp_data);
  atom = None;
  /* GCC thinks i is used uninitialized, but it's always initialized if
     `atoms' exists at that particular spot.  */
  i = 0;

  if (rc == Success
      && actual_format == 32 && nitems
      && actual_type == XA_ATOM)
    {
      atoms = (Atom *) tmp_data;

      x_catch_errors (dpyinfo->display);

      for (i = 0; i < nitems; ++i)
	{
	  owner = XGetSelectionOwner (dpyinfo->display, atoms[i]);

	  if (!x_had_errors_p (dpyinfo->display)
	      && (owner == None
		  /* If we already own this selection (even if another
		     frame owns it), use it.  There is no way of
		     knowing when ownership was asserted, so it still
		     has to be owned again.  */
		  || x_window_to_frame (dpyinfo, owner)))
	    {
	      atom = atoms[i];

	      break;
	    }
	}

      x_uncatch_errors ();
    }

  if (tmp_data)
    XFree (tmp_data);

  buffer = dpyinfo->motif_drag_atom_name;

  if (atom)
    {
      sprintf (buffer, "_EMACS_ATOM_%lu", i + 1);
      XSetSelectionOwner (dpyinfo->display, atom,
			  FRAME_X_WINDOW (source_frame),
			  dpyinfo->last_user_time);

      /* The selection's last-change time is newer than our
	 last_user_time, so create a new selection instead.  */
      if (XGetSelectionOwner (dpyinfo->display, atom)
	  != FRAME_X_WINDOW (source_frame))
	atom = None;
    }

  while (!atom)
    {
      sprintf (buffer, "_EMACS_ATOM_%lu", nitems + 1);
      atom = XInternAtom (dpyinfo->display, buffer, False);

      XSetSelectionOwner (dpyinfo->display, atom,
			  FRAME_X_WINDOW (source_frame),
			  dpyinfo->last_user_time);

      XChangeProperty (dpyinfo->display, dpyinfo->root_window,
		       dpyinfo->Xatom_EMACS_DRAG_ATOM, XA_ATOM, 32,
		       (rc != Success
			|| (actual_format != 32
			    || actual_type != XA_ATOM)
			? PropModeReplace : PropModeAppend),
		       (unsigned char *) &atom, 1);

      actual_format = 32;
      actual_type = XA_ATOM;
      rc = Success;
      nitems += 1;

      /* The selection's last-change time is newer than our
	 last_user_time, so create a new selection (again).  */
      if (XGetSelectionOwner (dpyinfo->display, atom)
	  != FRAME_X_WINDOW (source_frame))
	atom = None;
    }

  dpyinfo->motif_drag_atom_time = dpyinfo->last_user_time;
  dpyinfo->motif_drag_atom_owner = source_frame;

  XUngrabServer (dpyinfo->display);
  return atom;
}

static Atom
xm_get_drag_atom (struct x_display_info *dpyinfo)
{
  Atom atom;

  if (dpyinfo->motif_drag_atom != None)
    atom = dpyinfo->motif_drag_atom;
  else
    atom = xm_get_drag_atom_1 (dpyinfo, x_dnd_frame);

  dpyinfo->motif_drag_atom = atom;
  return atom;
}

static void
xm_setup_drag_info (struct x_display_info *dpyinfo,
		    struct frame *source_frame)
{
  Atom atom;
  xm_drag_initiator_info drag_initiator_info;
  int idx;

  atom = xm_get_drag_atom (dpyinfo);

  if (atom == None)
    return;

  XSETCAR (x_dnd_selection_alias_cell,
	   x_atom_to_symbol (dpyinfo, atom));
  XSETCDR (x_dnd_selection_alias_cell, QXdndSelection);

  idx = xm_setup_dnd_targets (dpyinfo, x_dnd_targets,
			      x_dnd_n_targets);

  if (idx != -1)
    {
      drag_initiator_info.byteorder = XM_BYTE_ORDER_CUR_FIRST;
      drag_initiator_info.protocol = XM_DRAG_PROTOCOL_VERSION;
      drag_initiator_info.table_index = idx;
      drag_initiator_info.selection = atom;

      xm_write_drag_initiator_info (dpyinfo->display,
				    FRAME_X_WINDOW (source_frame), atom,
				    dpyinfo->Xatom_MOTIF_DRAG_INITIATOR_INFO,
				    &drag_initiator_info);

      x_dnd_motif_setup_p = true;
      x_dnd_motif_atom = atom;
    }
}

static void
xm_send_drop_message (struct x_display_info *dpyinfo, Window source,
		      Window target, xm_drop_start_message *dmsg)
{
  XEvent msg;

  msg.xclient.type = ClientMessage;
  msg.xclient.message_type
    = dpyinfo->Xatom_MOTIF_DRAG_AND_DROP_MESSAGE;
  msg.xclient.format = 8;
  msg.xclient.window = target;
  msg.xclient.data.b[0] = dmsg->reason;
  msg.xclient.data.b[1] = dmsg->byte_order;
  *((uint16_t *) &msg.xclient.data.b[2]) = dmsg->side_effects;
  *((uint32_t *) &msg.xclient.data.b[4]) = dmsg->timestamp;
  *((uint16_t *) &msg.xclient.data.b[8]) = dmsg->x;
  *((uint16_t *) &msg.xclient.data.b[10]) = dmsg->y;
  *((uint32_t *) &msg.xclient.data.b[12]) = dmsg->index_atom;
  *((uint32_t *) &msg.xclient.data.b[16]) = dmsg->source_window;

  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSendEvent (dpyinfo->display, target, False, NoEventMask, &msg);
  x_stop_ignoring_errors (dpyinfo);
}

static void
xm_send_top_level_enter_message (struct x_display_info *dpyinfo, Window source,
				 Window target, xm_top_level_enter_message *dmsg)
{
  XEvent msg;

  msg.xclient.type = ClientMessage;
  msg.xclient.message_type
    = dpyinfo->Xatom_MOTIF_DRAG_AND_DROP_MESSAGE;
  msg.xclient.format = 8;
  msg.xclient.window = target;
  msg.xclient.data.b[0] = dmsg->reason;
  msg.xclient.data.b[1] = dmsg->byteorder;
  *((uint16_t *) &msg.xclient.data.b[2]) = dmsg->zero;
  *((uint32_t *) &msg.xclient.data.b[4]) = dmsg->timestamp;
  *((uint32_t *) &msg.xclient.data.b[8]) = dmsg->source_window;
  *((uint32_t *) &msg.xclient.data.b[12]) = dmsg->index_atom;
  msg.xclient.data.b[16] = 0;
  msg.xclient.data.b[17] = 0;
  msg.xclient.data.b[18] = 0;
  msg.xclient.data.b[19] = 0;

  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSendEvent (dpyinfo->display, target, False, NoEventMask, &msg);
  x_stop_ignoring_errors (dpyinfo);
}

static void
xm_send_drag_motion_message (struct x_display_info *dpyinfo, Window source,
			     Window target, xm_drag_motion_message *dmsg)
{
  XEvent msg;

  msg.xclient.type = ClientMessage;
  msg.xclient.message_type
    = dpyinfo->Xatom_MOTIF_DRAG_AND_DROP_MESSAGE;
  msg.xclient.format = 8;
  msg.xclient.window = target;
  msg.xclient.data.b[0] = dmsg->reason;
  msg.xclient.data.b[1] = dmsg->byteorder;
  *((uint16_t *) &msg.xclient.data.b[2]) = dmsg->side_effects;
  *((uint32_t *) &msg.xclient.data.b[4]) = dmsg->timestamp;
  *((uint16_t *) &msg.xclient.data.b[8]) = dmsg->x;
  *((uint16_t *) &msg.xclient.data.b[10]) = dmsg->y;
  msg.xclient.data.b[12] = 0;
  msg.xclient.data.b[13] = 0;
  msg.xclient.data.b[14] = 0;
  msg.xclient.data.b[15] = 0;
  msg.xclient.data.b[16] = 0;
  msg.xclient.data.b[17] = 0;
  msg.xclient.data.b[18] = 0;
  msg.xclient.data.b[19] = 0;

  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSendEvent (dpyinfo->display, target, False, NoEventMask, &msg);
  x_stop_ignoring_errors (dpyinfo);
}

static void
xm_send_top_level_leave_message (struct x_display_info *dpyinfo, Window source,
				 Window target, xm_top_level_leave_message *dmsg)
{
  XEvent msg;
  xm_drag_motion_message mmsg;

  /* Motif support for TOP_LEVEL_LEAVE has bitrotted, since these days
     it assumes every client supports the preregister protocol style,
     but we only support drop-only and dynamic.  (Interestingly enough
     LessTif works fine.)  Sending an event with impossible
     coordinates serves to get rid of any active drop site that might
     still be around in the target drag context.  */

  if (x_dnd_fix_motif_leave)
    {
      mmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
				    XM_DRAG_REASON_DRAG_MOTION);
      mmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
      mmsg.side_effects = XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (dpyinfo,
									   x_dnd_wanted_action),
					       XM_DROP_SITE_NONE, x_dnd_motif_operations,
					       XM_DROP_ACTION_DROP_CANCEL);
      mmsg.timestamp = dmsg->timestamp;

      /* Use X_SHRT_MAX instead of the max value of uint16_t since
	 that will be interpreted as a plausible position by Motif,
	 and as such breaks if the drop target is beneath that
	 position.  */
      mmsg.x = X_SHRT_MAX;
      mmsg.y = X_SHRT_MAX;

      xm_send_drag_motion_message (dpyinfo, source, target, &mmsg);
    }

  msg.xclient.type = ClientMessage;
  msg.xclient.message_type
    = dpyinfo->Xatom_MOTIF_DRAG_AND_DROP_MESSAGE;
  msg.xclient.format = 8;
  msg.xclient.window = target;
  msg.xclient.data.b[0] = dmsg->reason;
  msg.xclient.data.b[1] = dmsg->byteorder;
  *((uint16_t *) &msg.xclient.data.b[2]) = dmsg->zero;
  *((uint32_t *) &msg.xclient.data.b[4]) = dmsg->timestamp;
  *((uint32_t *) &msg.xclient.data.b[8]) = dmsg->source_window;
  msg.xclient.data.b[12] = 0;
  msg.xclient.data.b[13] = 0;
  msg.xclient.data.b[14] = 0;
  msg.xclient.data.b[15] = 0;
  msg.xclient.data.b[16] = 0;
  msg.xclient.data.b[17] = 0;
  msg.xclient.data.b[18] = 0;
  msg.xclient.data.b[19] = 0;

  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSendEvent (dpyinfo->display, target, False, NoEventMask, &msg);
  x_stop_ignoring_errors (dpyinfo);
}

static int
xm_read_drop_start_reply (const XEvent *msg, xm_drop_start_reply *reply)
{
  const uint8_t *data;

  data = (const uint8_t *) &msg->xclient.data.b[0];

  if ((XM_DRAG_REASON_ORIGINATOR (data[0])
       != XM_DRAG_ORIGINATOR_RECEIVER)
      || (XM_DRAG_REASON_CODE (data[0])
	  != XM_DRAG_REASON_DROP_START))
    return 1;

  reply->reason = *(data++);
  reply->byte_order = *(data++);
  reply->side_effects = *(uint16_t *) data;
  reply->better_x = *(uint16_t *) (data + 2);
  reply->better_y = *(uint16_t *) (data + 4);

  if (reply->byte_order != XM_BYTE_ORDER_CUR_FIRST)
    {
      SWAPCARD16 (reply->side_effects);
      SWAPCARD16 (reply->better_x);
      SWAPCARD16 (reply->better_y);
    }

  reply->byte_order = XM_BYTE_ORDER_CUR_FIRST;

  return 0;
}

static int
xm_read_drop_start_message (const XEvent *msg,
			    xm_drop_start_message *dmsg)
{
  const uint8_t *data;

  data = (const uint8_t *) &msg->xclient.data.b[0];

  if ((XM_DRAG_REASON_ORIGINATOR (data[0])
       != XM_DRAG_ORIGINATOR_INITIATOR)
      || (XM_DRAG_REASON_CODE (data[0])
	  != XM_DRAG_REASON_DROP_START))
    return 1;

  dmsg->reason = *(data++);
  dmsg->byte_order = *(data++);
  dmsg->side_effects = *(uint16_t *) data;
  dmsg->timestamp = *(uint32_t *) (data + 2);
  dmsg->x = *(uint16_t *) (data + 6);
  dmsg->y = *(uint16_t *) (data + 8);
  dmsg->index_atom = *(uint32_t *) (data + 10);
  dmsg->source_window = *(uint32_t *) (data + 14);

  if (dmsg->byte_order != XM_BYTE_ORDER_CUR_FIRST)
    {
      SWAPCARD16 (dmsg->side_effects);
      SWAPCARD32 (dmsg->timestamp);
      SWAPCARD16 (dmsg->x);
      SWAPCARD16 (dmsg->y);
      SWAPCARD32 (dmsg->index_atom);
      SWAPCARD32 (dmsg->source_window);
    }

  dmsg->byte_order = XM_BYTE_ORDER_CUR_FIRST;

  return 0;
}

static int
xm_read_drag_receiver_info (struct x_display_info *dpyinfo,
			    Window wdesc, xm_drag_receiver_info *rec)
{
  Atom actual_type;
  int rc, actual_format;
  unsigned long nitems, bytes_remaining;
  unsigned char *tmp_data = NULL;
  uint8_t *data;

  x_catch_errors (dpyinfo->display);
  rc = XGetWindowProperty (dpyinfo->display, wdesc,
			   dpyinfo->Xatom_MOTIF_DRAG_RECEIVER_INFO,
			   0, 4, False,
			   dpyinfo->Xatom_MOTIF_DRAG_RECEIVER_INFO,
			   &actual_type, &actual_format, &nitems,
			   &bytes_remaining,
			   &tmp_data) == Success;

  if (x_had_errors_p (dpyinfo->display)
      || actual_format != 8 || nitems < 16 || !tmp_data
      || actual_type != dpyinfo->Xatom_MOTIF_DRAG_RECEIVER_INFO)
    rc = 0;
  x_uncatch_errors_after_check ();

  if (rc)
    {
      data = (uint8_t *) tmp_data;

      if (data[1] > XM_DRAG_PROTOCOL_VERSION)
	return 1;

      rec->byteorder = data[0];
      rec->protocol = data[1];
      rec->protocol_style = data[2];
      rec->unspecified0 = data[3];
      rec->unspecified1 = *(uint32_t *) &data[4];
      rec->unspecified2 = *(uint32_t *) &data[8];
      rec->unspecified3 = *(uint32_t *) &data[12];

      if (rec->byteorder != XM_BYTE_ORDER_CUR_FIRST)
	{
	  SWAPCARD32 (rec->unspecified1);
	  SWAPCARD32 (rec->unspecified2);
	  SWAPCARD32 (rec->unspecified3);
	}

      rec->byteorder = XM_BYTE_ORDER_CUR_FIRST;
    }

  if (tmp_data)
    XFree (tmp_data);

  return !rc;
}

static int
xm_read_drag_motion_message (const XEvent *msg,
			     xm_drag_motion_message *dmsg)
{
  const uint8_t *data;

  data = (const uint8_t *) &msg->xclient.data.b[0];

  if ((XM_DRAG_REASON_CODE (data[0])
       != XM_DRAG_REASON_DRAG_MOTION)
      || (XM_DRAG_REASON_ORIGINATOR (data[0])
	  != XM_DRAG_ORIGINATOR_INITIATOR))
    return 1;

  dmsg->reason = *(data++);
  dmsg->byteorder = *(data++);
  dmsg->side_effects = *(uint16_t *) data;
  dmsg->timestamp = *(uint32_t *) (data + 2);
  dmsg->x = *(uint16_t *) (data + 6);
  dmsg->y = *(uint16_t *) (data + 8);

  if (dmsg->byteorder != XM_BYTE_ORDER_CUR_FIRST)
    {
      SWAPCARD16 (dmsg->side_effects);
      SWAPCARD32 (dmsg->timestamp);
      SWAPCARD16 (dmsg->x);
      SWAPCARD16 (dmsg->y);
    }

  dmsg->byteorder = XM_BYTE_ORDER_CUR_FIRST;

  return 0;
}

static int
xm_read_drag_motion_reply (const XEvent *msg, xm_drag_motion_reply *reply)
{
  const uint8_t *data;

  data = (const uint8_t *) &msg->xclient.data.b[0];

  if ((XM_DRAG_REASON_CODE (data[0])
       != XM_DRAG_REASON_DRAG_MOTION)
      || (XM_DRAG_REASON_ORIGINATOR (data[0])
	  != XM_DRAG_ORIGINATOR_RECEIVER))
    return 1;

  reply->reason = *(data++);
  reply->byte_order = *(data++);
  reply->side_effects = *(uint16_t *) data;
  reply->timestamp = *(uint32_t *) (data + 2);
  reply->better_x = *(uint16_t *) (data + 6);
  reply->better_y = *(uint16_t *) (data + 8);

  if (reply->byte_order != XM_BYTE_ORDER_CUR_FIRST)
    {
      SWAPCARD16 (reply->side_effects);
      SWAPCARD32 (reply->timestamp);
      SWAPCARD16 (reply->better_x);
      SWAPCARD16 (reply->better_y);
    }

  reply->byte_order = XM_BYTE_ORDER_CUR_FIRST;

  return 0;
}

static void
x_dnd_send_xm_leave_for_drop (struct x_display_info *dpyinfo,
			      struct frame *f, Window wdesc,
			      Time timestamp)
{
  xm_top_level_leave_message lmsg;

  lmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
				XM_DRAG_REASON_TOP_LEVEL_LEAVE);
  lmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
  lmsg.zero = 0;
  lmsg.timestamp = timestamp;
  lmsg.source_window = FRAME_X_WINDOW (f);

  if (x_dnd_motif_setup_p)
    xm_send_top_level_leave_message (dpyinfo, FRAME_X_WINDOW (f),
				     wdesc, &lmsg);
}



/* Drag-and-drop and XDND protocol primitives employed by the event
   loop.  */

static void
x_dnd_free_toplevels (bool display_alive)
{
  struct x_client_list_window *last;
  struct x_client_list_window *tem = x_dnd_toplevels;
  ptrdiff_t n_windows, i, buffer_size;
  Window *destroy_windows UNINIT;
  unsigned long *prev_masks UNINIT;
  specpdl_ref count;
  Display *dpy UNINIT;

  if (!x_dnd_toplevels)
    /* Probably called inside an IO error handler.  */
    return;

  if (display_alive)
    {
      buffer_size = 1024;
      destroy_windows = xmalloc (sizeof *destroy_windows
				 * buffer_size);
      prev_masks = xmalloc (sizeof *prev_masks *
			    buffer_size);
      n_windows = 0;
    }

  block_input ();
  while (tem)
    {
      last = tem;
      tem = tem->next;

      if (display_alive)
	{
	  if (++n_windows >= buffer_size)
	    {
	      buffer_size += 1024;
	      destroy_windows
		= xrealloc (destroy_windows, (sizeof *destroy_windows
					      * buffer_size));
	      prev_masks
		= xrealloc (prev_masks, (sizeof *prev_masks
					 * buffer_size));
	    }

	  dpy = last->dpy;
	  prev_masks[n_windows - 1] = last->previous_event_mask;
	  destroy_windows[n_windows - 1] = last->window;
	}

#ifdef HAVE_XSHAPE
      if (last->n_input_rects != -1)
	xfree (last->input_rects);
      if (last->n_bounding_rects != -1)
	xfree (last->bounding_rects);
#endif

      xfree (last);
    }

  x_dnd_toplevels = NULL;

  if (!display_alive)
    {
      unblock_input ();
      return;
    }

  count = SPECPDL_INDEX ();
  record_unwind_protect_ptr (xfree, destroy_windows);
  record_unwind_protect_ptr (xfree, prev_masks);

  if (display_alive && n_windows)
    {
      struct x_display_info *dpyinfo = x_dpyinfo (dpy);

      x_ignore_errors_for_next_request (dpyinfo, 0);

      for (i = 0; i < n_windows; ++i)
	{
	  XSelectInput (dpy, destroy_windows[i], prev_masks[i]);
#ifdef HAVE_XSHAPE
	  XShapeSelectInput (dpy, destroy_windows[i], None);
#endif
	}

      x_stop_ignoring_errors (dpyinfo);
    }

  unbind_to (count, Qnil);
  unblock_input ();
}

static int
x_dnd_compute_toplevels (struct x_display_info *dpyinfo)
{
  Atom type;
  Window *toplevels;
  int format, rc;
  unsigned long nitems, bytes_after;
  unsigned long i, real_nitems;
  unsigned char *data = NULL;
  int frame_extents[4];

#ifndef USE_XCB
  int dest_x, dest_y;
  unsigned long *wmstate;
  unsigned long wmstate_items, extent_items;
  unsigned char *wmstate_data = NULL, *extent_data = NULL;
  XWindowAttributes attrs;
  Window child;
  xm_drag_receiver_info xm_info;
#else
  uint32_t *wmstate, *fextents;
  uint8_t *xmdata;
  xcb_get_window_attributes_cookie_t *window_attribute_cookies;
  xcb_translate_coordinates_cookie_t *translate_coordinate_cookies;
  xcb_get_property_cookie_t *get_property_cookies;
  xcb_get_property_cookie_t *xm_property_cookies;
  xcb_get_property_cookie_t *extent_property_cookies;
  xcb_get_geometry_cookie_t *get_geometry_cookies;
  xcb_get_window_attributes_reply_t attrs, *attrs_reply;
  xcb_translate_coordinates_reply_t *coordinates_reply;
  xcb_get_property_reply_t *property_reply;
  xcb_get_property_reply_t *xm_property_reply;
  xcb_get_property_reply_t *extent_property_reply;
  xcb_get_geometry_reply_t *geometry_reply;
  xcb_generic_error_t *error;
#endif

#ifdef HAVE_XCB_SHAPE
  xcb_shape_get_rectangles_cookie_t *bounding_rect_cookies;
  xcb_shape_get_rectangles_reply_t *bounding_rect_reply;
  xcb_rectangle_iterator_t bounding_rect_iterator;
#endif

#ifdef HAVE_XCB_SHAPE_INPUT_RECTS
  xcb_shape_get_rectangles_cookie_t *input_rect_cookies;
  xcb_shape_get_rectangles_reply_t *input_rect_reply;
  xcb_rectangle_iterator_t input_rect_iterator;
#endif

  struct x_client_list_window *tem;
#if defined HAVE_XSHAPE && !defined HAVE_XCB_SHAPE_INPUT_RECTS
  int count, ordering;
  XRectangle *rects;
#endif

  rc = XGetWindowProperty (dpyinfo->display, dpyinfo->root_window,
			   dpyinfo->Xatom_net_client_list_stacking,
			   0, LONG_MAX, False, XA_WINDOW, &type,
			   &format, &nitems, &bytes_after, &data);

  if (rc != Success)
    return 1;

  if (format != 32 || type != XA_WINDOW)
    {
      XFree (data);
      return 1;
    }

  toplevels = (Window *) data;

  for (i = 0, real_nitems = 0; i < nitems; ++i)
    {
      /* Some window managers with built in compositors end up putting
	 tooltips in the client list, which is silly.  */
      if (!x_tooltip_window_to_frame (dpyinfo, toplevels[i], NULL))
	toplevels[real_nitems++] = toplevels[i];
    }

  nitems = real_nitems;

#ifdef USE_XCB
  USE_SAFE_ALLOCA;

  SAFE_NALLOCA (window_attribute_cookies, 1, nitems);
  SAFE_NALLOCA (translate_coordinate_cookies, 1, nitems);
  SAFE_NALLOCA (get_property_cookies, 1, nitems);
  SAFE_NALLOCA (xm_property_cookies, 1, nitems);
  SAFE_NALLOCA (extent_property_cookies, 1, nitems);
  SAFE_NALLOCA (get_geometry_cookies, 1, nitems);

#ifdef HAVE_XCB_SHAPE
  SAFE_NALLOCA (bounding_rect_cookies, 1, nitems);
#endif

#ifdef HAVE_XCB_SHAPE_INPUT_RECTS
  SAFE_NALLOCA (input_rect_cookies, 1, nitems);
#endif

  for (i = 0; i < nitems; ++i)
    {
      window_attribute_cookies[i]
	= xcb_get_window_attributes (dpyinfo->xcb_connection,
				     (xcb_window_t) toplevels[i]);
      translate_coordinate_cookies[i]
	= xcb_translate_coordinates (dpyinfo->xcb_connection,
				     (xcb_window_t) toplevels[i],
				     (xcb_window_t) dpyinfo->root_window,
				     0, 0);
      get_property_cookies[i]
	= xcb_get_property (dpyinfo->xcb_connection, 0, (xcb_window_t) toplevels[i],
			    (xcb_atom_t) dpyinfo->Xatom_wm_state, 0,
			    0, 2);
      xm_property_cookies[i]
	= xcb_get_property (dpyinfo->xcb_connection, 0, (xcb_window_t) toplevels[i],
			    (xcb_atom_t) dpyinfo->Xatom_MOTIF_DRAG_RECEIVER_INFO,
			    (xcb_atom_t) dpyinfo->Xatom_MOTIF_DRAG_RECEIVER_INFO,
			    0, 4);
      extent_property_cookies[i]
	= xcb_get_property (dpyinfo->xcb_connection, 0,
			    (xcb_window_t) toplevels[i],
			    (xcb_atom_t) dpyinfo->Xatom_net_frame_extents,
			    XA_CARDINAL, 0, 4);
      get_geometry_cookies[i]
	= xcb_get_geometry (dpyinfo->xcb_connection, (xcb_window_t) toplevels[i]);

#ifdef HAVE_XCB_SHAPE
      bounding_rect_cookies[i]
	= xcb_shape_get_rectangles (dpyinfo->xcb_connection,
				    (xcb_window_t) toplevels[i],
				    XCB_SHAPE_SK_BOUNDING);
#endif

#ifdef HAVE_XCB_SHAPE_INPUT_RECTS
      if (dpyinfo->xshape_major > 1
	  || (dpyinfo->xshape_major == 1
	      && dpyinfo->xshape_minor >= 1))
	input_rect_cookies[i]
	  = xcb_shape_get_rectangles (dpyinfo->xcb_connection,
				      (xcb_window_t) toplevels[i],
				      XCB_SHAPE_SK_INPUT);
#endif
    }
#endif

  /* Actually right because _NET_CLIENT_LIST_STACKING has bottom-up
     order.  */
  for (i = 0; i < nitems; ++i)
    {
      frame_extents[0] = 0;
      frame_extents[1] = 0;
      frame_extents[2] = 0;
      frame_extents[3] = 0;

#ifndef USE_XCB
      x_catch_errors (dpyinfo->display);
      rc = (XGetWindowAttributes (dpyinfo->display,
				  toplevels[i], &attrs)
	    && !x_had_errors_p (dpyinfo->display));

      if (rc)
	rc = (XTranslateCoordinates (dpyinfo->display, toplevels[i],
				     attrs.root, -attrs.border_width,
				     -attrs.border_width, &dest_x,
				     &dest_y, &child)
	      && !x_had_errors_p (dpyinfo->display));
      if (rc)
	rc = ((XGetWindowProperty (dpyinfo->display,
				   toplevels[i],
				   dpyinfo->Xatom_wm_state,
				   0, 2, False, AnyPropertyType,
				   &type, &format, &wmstate_items,
				   &bytes_after, &wmstate_data)
	       == Success)
	      && !x_had_errors_p (dpyinfo->display)
	      && wmstate_data && wmstate_items == 2 && format == 32);

      if (XGetWindowProperty (dpyinfo->display, toplevels[i],
			      dpyinfo->Xatom_net_frame_extents,
			      0, 4, False, XA_CARDINAL, &type,
			      &format, &extent_items, &bytes_after,
			      &extent_data) == Success
	  && !x_had_errors_p (dpyinfo->display)
	  && extent_data && extent_items >= 4 && format == 32)
	{
	  frame_extents[0] = ((unsigned long *) extent_data)[0];
	  frame_extents[1] = ((unsigned long *) extent_data)[1];
	  frame_extents[2] = ((unsigned long *) extent_data)[2];
	  frame_extents[3] = ((unsigned long *) extent_data)[3];
	}

      if (extent_data)
	XFree (extent_data);

      x_uncatch_errors ();
#else
      rc = true;

      attrs_reply
	= xcb_get_window_attributes_reply (dpyinfo->xcb_connection,
					   window_attribute_cookies[i],
					   &error);

      if (!attrs_reply)
	{
	  rc = false;
	  free (error);
	}

      coordinates_reply
	= xcb_translate_coordinates_reply (dpyinfo->xcb_connection,
					   translate_coordinate_cookies[i],
					   &error);

      if (!coordinates_reply)
	{
	  rc = false;
	  free (error);
	}

      property_reply = xcb_get_property_reply (dpyinfo->xcb_connection,
					       get_property_cookies[i],
					       &error);

      if (!property_reply)
	{
	  rc = false;
	  free (error);
	}

      /* These requests don't set rc on failure because they aren't
	 required.  */

      xm_property_reply = xcb_get_property_reply (dpyinfo->xcb_connection,
						  xm_property_cookies[i],
						  &error);

      if (!xm_property_reply)
	free (error);

      extent_property_reply
	= xcb_get_property_reply (dpyinfo->xcb_connection,
				  extent_property_cookies[i],
				  &error);

      if (!extent_property_reply)
	free (error);
      else
	{
	  if (xcb_get_property_value_length (extent_property_reply) == 16
	      && extent_property_reply->format == 32
	      && extent_property_reply->type == XA_CARDINAL)
	    {
	      fextents = xcb_get_property_value (extent_property_reply);
	      frame_extents[0] = fextents[0];
	      frame_extents[1] = fextents[1];
	      frame_extents[2] = fextents[2];
	      frame_extents[3] = fextents[3];
	    }

	  free (extent_property_reply);
	}

      if (property_reply
	  && (xcb_get_property_value_length (property_reply) != 8
	      || property_reply->format != 32))
	rc = false;

      geometry_reply = xcb_get_geometry_reply (dpyinfo->xcb_connection,
					       get_geometry_cookies[i],
					       &error);

      if (!geometry_reply)
	{
	  rc = false;
	  free (error);
	}
#endif

      if (rc)
	{
#ifdef USE_XCB
	  wmstate = (uint32_t *) xcb_get_property_value (property_reply);
	  attrs = *attrs_reply;
#else
	  wmstate = (unsigned long *) wmstate_data;
#endif

	  tem = xmalloc (sizeof *tem);
	  tem->window = toplevels[i];
	  tem->dpy = dpyinfo->display;
	  tem->frame_extents_left = frame_extents[0];
	  tem->frame_extents_right = frame_extents[1];
	  tem->frame_extents_top = frame_extents[2];
	  tem->frame_extents_bottom = frame_extents[3];

#ifndef USE_XCB
	  tem->x = dest_x;
	  tem->y = dest_y;
	  tem->width = attrs.width + attrs.border_width;
	  tem->height = attrs.height + attrs.border_width;
	  tem->mapped_p = (attrs.map_state != IsUnmapped);
#else
	  tem->x = (coordinates_reply->dst_x
		    - geometry_reply->border_width);
	  tem->y = (coordinates_reply->dst_y
		    - geometry_reply->border_width);
	  tem->width = (geometry_reply->width
			+ geometry_reply->border_width);
	  tem->height = (geometry_reply->height
			 + geometry_reply->border_width);
	  tem->mapped_p = (attrs.map_state != XCB_MAP_STATE_UNMAPPED);
#endif
	  tem->next = x_dnd_toplevels;
	  tem->previous_event_mask = attrs.your_event_mask;
	  tem->wm_state = wmstate[0];
	  tem->xm_protocol_style = XM_DRAG_STYLE_NONE;

#ifndef USE_XCB
	  if (!xm_read_drag_receiver_info (dpyinfo, toplevels[i], &xm_info))
	    tem->xm_protocol_style = xm_info.protocol_style;
#else
	  if (xm_property_reply
	      && xm_property_reply->format == 8
	      && (xm_property_reply->type
		  == dpyinfo->Xatom_MOTIF_DRAG_RECEIVER_INFO)
	      && xcb_get_property_value_length (xm_property_reply) >= 4)
	    {
	      xmdata = xcb_get_property_value (xm_property_reply);

	      if (xmdata[1] <= XM_DRAG_PROTOCOL_VERSION)
		tem->xm_protocol_style = xmdata[2];
	    }
#endif

#ifdef HAVE_XSHAPE
#ifndef USE_XCB
	  tem->border_width = attrs.border_width;
#else
	  tem->border_width = geometry_reply->border_width;
#endif
	  tem->n_bounding_rects = -1;
	  tem->n_input_rects = -1;

	  if (dpyinfo->xshape_supported_p)
	    {
	      x_ignore_errors_for_next_request (dpyinfo, 0);
	      XShapeSelectInput (dpyinfo->display,
				 toplevels[i],
				 ShapeNotifyMask);
	      x_stop_ignoring_errors (dpyinfo);

#ifndef HAVE_XCB_SHAPE
	      x_catch_errors (dpyinfo->display);
	      rects = XShapeGetRectangles (dpyinfo->display,
					   toplevels[i],
					   ShapeBounding,
					   &count, &ordering);
	      rc = x_had_errors_p (dpyinfo->display);
	      x_uncatch_errors_after_check ();

	      /* Does XShapeGetRectangles allocate anything upon an
		 error?  */
	      if (!rc)
		{
		  tem->n_bounding_rects = count;
		  tem->bounding_rects
		    = xmalloc (sizeof *tem->bounding_rects * count);
		  memcpy (tem->bounding_rects, rects,
			  sizeof *tem->bounding_rects * count);

		  XFree (rects);
		}
#else
	      bounding_rect_reply
		= xcb_shape_get_rectangles_reply (dpyinfo->xcb_connection,
						  bounding_rect_cookies[i],
						  &error);

	      if (bounding_rect_reply)
		{
		  bounding_rect_iterator
		    = xcb_shape_get_rectangles_rectangles_iterator (bounding_rect_reply);
		  tem->n_bounding_rects = bounding_rect_iterator.rem + 1;
		  tem->bounding_rects = xmalloc (tem->n_bounding_rects
						 * sizeof *tem->bounding_rects);
		  tem->n_bounding_rects = 0;

		  for (; bounding_rect_iterator.rem;
		       xcb_rectangle_next (&bounding_rect_iterator))
		    {
		      tem->bounding_rects[tem->n_bounding_rects].x
			= bounding_rect_iterator.data->x;
		      tem->bounding_rects[tem->n_bounding_rects].y
			= bounding_rect_iterator.data->y;
		      tem->bounding_rects[tem->n_bounding_rects].width
			= bounding_rect_iterator.data->width;
		      tem->bounding_rects[tem->n_bounding_rects].height
			= bounding_rect_iterator.data->height;

		      tem->n_bounding_rects++;
		    }

		  free (bounding_rect_reply);
		}
	      else
		free (error);
#endif

#ifdef HAVE_XCB_SHAPE_INPUT_RECTS
	      if (dpyinfo->xshape_major > 1
		  || (dpyinfo->xshape_major == 1
		      && dpyinfo->xshape_minor >= 1))
		{
		  input_rect_reply
		    = xcb_shape_get_rectangles_reply (dpyinfo->xcb_connection,
						      input_rect_cookies[i],
						      &error);

		  if (input_rect_reply)
		    {
		      input_rect_iterator
			= xcb_shape_get_rectangles_rectangles_iterator (input_rect_reply);
		      tem->n_input_rects = input_rect_iterator.rem + 1;
		      tem->input_rects = xmalloc (tem->n_input_rects
						  * sizeof *tem->input_rects);
		      tem->n_input_rects = 0;

		      for (; input_rect_iterator.rem;
			   xcb_rectangle_next (&input_rect_iterator))
			{
			  tem->input_rects[tem->n_input_rects].x
			    = input_rect_iterator.data->x;
			  tem->input_rects[tem->n_input_rects].y
			    = input_rect_iterator.data->y;
			  tem->input_rects[tem->n_input_rects].width
			    = input_rect_iterator.data->width;
			  tem->input_rects[tem->n_input_rects].height
			    = input_rect_iterator.data->height;

			  tem->n_input_rects++;
			}

		      free (input_rect_reply);
		    }
		  else
		    free (error);
		}
#else
#ifdef ShapeInput
	      if (dpyinfo->xshape_major > 1
		  || (dpyinfo->xshape_major == 1
		      && dpyinfo->xshape_minor >= 1))
		{
		  x_catch_errors (dpyinfo->display);
		  rects = XShapeGetRectangles (dpyinfo->display,
					       toplevels[i], ShapeInput,
					       &count, &ordering);
		  rc = x_had_errors_p (dpyinfo->display);
		  x_uncatch_errors_after_check ();

		  /* Does XShapeGetRectangles allocate anything upon
		     an error?  */
		  if (!rc)
		    {
		      tem->n_input_rects = count;
		      tem->input_rects
			= xmalloc (sizeof *tem->input_rects * count);
		      memcpy (tem->input_rects, rects,
			      sizeof *tem->input_rects * count);

		      XFree (rects);
		    }
		}
#endif
#endif
	    }

	  /* Handle the common case where the input shape equals the
	     bounding shape.  */

	  if (tem->n_input_rects != -1
	      && tem->n_bounding_rects == tem->n_input_rects
	      && !memcmp (tem->bounding_rects, tem->input_rects,
			  tem->n_input_rects * sizeof *tem->input_rects))
	    {
	      xfree (tem->input_rects);
	      tem->n_input_rects = -1;
	    }

	  /* And the common case where there is no input rect and the
	     bounding rect equals the window dimensions.  */

	  if (tem->n_input_rects == -1
	      && tem->n_bounding_rects == 1
#ifdef USE_XCB
	      && (tem->bounding_rects[0].width
		  == (geometry_reply->width
		      + geometry_reply->border_width))
	      && (tem->bounding_rects[0].height
		  == (geometry_reply->height
		      + geometry_reply->border_width))
	      && (tem->bounding_rects[0].x
		  == -geometry_reply->border_width)
	      && (tem->bounding_rects[0].y
		  == -geometry_reply->border_width)
#else
	      && (tem->bounding_rects[0].width
		  == attrs.width + attrs.border_width)
	      && (tem->bounding_rects[0].height
		  == attrs.height + attrs.border_width)
	      && (tem->bounding_rects[0].x
		  == -attrs.border_width)
	      && (tem->bounding_rects[0].y
		  == -attrs.border_width)
#endif
	      )
	    {
	      xfree (tem->bounding_rects);
	      tem->n_bounding_rects = -1;
	    }
#endif

	  x_ignore_errors_for_next_request (dpyinfo, 0);
	  XSelectInput (dpyinfo->display, toplevels[i],
			(attrs.your_event_mask
			 | StructureNotifyMask
			 | PropertyChangeMask));
	  x_stop_ignoring_errors (dpyinfo);

	  x_dnd_toplevels = tem;
	}
      else
	{
#ifdef HAVE_XCB_SHAPE
	  if (dpyinfo->xshape_supported_p)
	    {
	      bounding_rect_reply
		= xcb_shape_get_rectangles_reply (dpyinfo->xcb_connection,
						  bounding_rect_cookies[i],
						  &error);

	      if (bounding_rect_reply)
		free (bounding_rect_reply);
	      else
		free (error);
	    }
#endif

#ifdef HAVE_XCB_SHAPE_INPUT_RECTS
	  if (dpyinfo->xshape_supported_p
	      && (dpyinfo->xshape_major > 1
		  || (dpyinfo->xshape_major == 1
		      && dpyinfo->xshape_minor >= 1)))
	    {
	      input_rect_reply
		= xcb_shape_get_rectangles_reply (dpyinfo->xcb_connection,
						  input_rect_cookies[i],
						  &error);

	      if (input_rect_reply)
		free (input_rect_reply);
	      else
		free (error);
	    }
#endif
	}

#ifdef USE_XCB
      if (attrs_reply)
	free (attrs_reply);

      if (coordinates_reply)
	free (coordinates_reply);

      if (property_reply)
	free (property_reply);

      if (xm_property_reply)
	free (xm_property_reply);

      if (geometry_reply)
	free (geometry_reply);
#endif

#ifndef USE_XCB
      if (wmstate_data)
	{
	  XFree (wmstate_data);
	  wmstate_data = NULL;
	}
#endif
    }

#ifdef USE_XCB
  SAFE_FREE ();
#endif

  if (data)
    XFree (data);

  return 0;
}

static _Noreturn int
x_dnd_io_error_handler (Display *display)
{
#ifdef USE_GTK
  emacs_abort ();
#else
  siglongjmp (x_dnd_disconnect_handler, 1);
#endif
}

#define X_DND_SUPPORTED_VERSION 5

static int x_dnd_get_window_proto (struct x_display_info *, Window);
static Window x_dnd_get_window_proxy (struct x_display_info *, Window);
static void x_dnd_update_state (struct x_display_info *, Time);

#ifdef USE_XCB
static void
x_dnd_get_proxy_proto (struct x_display_info *dpyinfo, Window wdesc,
		       Window *proxy_out, int *proto_out)
{
  xcb_get_property_cookie_t xdnd_proto_cookie;
  xcb_get_property_cookie_t xdnd_proxy_cookie;
  xcb_get_property_reply_t *reply;
  xcb_generic_error_t *error;

  if (proxy_out)
    *proxy_out = None;

  if (proto_out)
    *proto_out = -1;

  if (proxy_out)
    xdnd_proxy_cookie = xcb_get_property (dpyinfo->xcb_connection, 0,
					  (xcb_window_t) wdesc,
					  (xcb_atom_t) dpyinfo->Xatom_XdndProxy,
					  XA_WINDOW, 0, 1);

  if (proto_out)
    xdnd_proto_cookie = xcb_get_property (dpyinfo->xcb_connection, 0,
					  (xcb_window_t) wdesc,
					  (xcb_atom_t) dpyinfo->Xatom_XdndAware,
					  XA_ATOM, 0, 1);

  if (proxy_out)
    {
      reply = xcb_get_property_reply (dpyinfo->xcb_connection,
				      xdnd_proxy_cookie, &error);

      if (!reply)
	free (error);
      else
	{
	  if (reply->format == 32
	      && reply->type == XA_WINDOW
	      && (xcb_get_property_value_length (reply) >= 4))
	    *proxy_out = *(xcb_window_t *) xcb_get_property_value (reply);

	  free (reply);
	}
    }

  if (proto_out)
    {
      reply = xcb_get_property_reply (dpyinfo->xcb_connection,
				      xdnd_proto_cookie, &error);

      if (!reply)
	free (error);
      else
	{
	  if (reply->format == 32
	      && reply->type == XA_ATOM
	      && (xcb_get_property_value_length (reply) >= 4))
	    *proto_out = (int) *(xcb_atom_t *) xcb_get_property_value (reply);

	  free (reply);
	}
    }
}
#endif

#ifdef HAVE_XSHAPE
static bool
x_dnd_get_target_window_2 (XRectangle *rects, int nrects,
			   int x, int y)
{
  int i;
  XRectangle *tem;

  for (i = 0; i < nrects; ++i)
    {
      tem = &rects[i];

      if (x >= tem->x && y >= tem->y
	  && x < tem->x + tem->width
	  && y < tem->y + tem->height)
	return true;
    }

  return false;
}
#endif

static Window
x_dnd_get_target_window_1 (struct x_display_info *dpyinfo,
			   int root_x, int root_y, int *motif_out,
			   bool *extents_p)
{
  struct x_client_list_window *tem, *chosen = NULL;

  /* Loop through x_dnd_toplevels until we find the toplevel where
     root_x and root_y are.  */

  *motif_out = XM_DRAG_STYLE_NONE;

  for (tem = x_dnd_toplevels; tem; tem = tem->next)
    {
      if (!tem->mapped_p || tem->wm_state != NormalState)
	continue;

      /* Test if the coordinates are inside the window's frame
	 extents, and return None in that case.  */

      *extents_p = true;
      if (root_x > tem->x - tem->frame_extents_left
	  && root_x < tem->x
	  && root_y > tem->y - tem->frame_extents_top
	  && root_y < (tem->y + tem->height - 1
		       + tem->frame_extents_bottom))
	return None;

      if (root_x > tem->x + tem->width
	  && root_x < (tem->x + tem->width - 1
		       + tem->frame_extents_right)
	  && root_y > tem->y - tem->frame_extents_top
	  && root_y < (tem->y + tem->height - 1
		       + tem->frame_extents_bottom))
	return None;

      if (root_y > tem->y - tem->frame_extents_top
	  && root_y < tem->y
	  && root_x > tem->x - tem->frame_extents_left
	  && root_x < (tem->x + tem->width - 1
		       + tem->frame_extents_right))
	return None;

      if (root_y > tem->y + tem->height
	  && root_y < (tem->y + tem->height - 1
		       + tem->frame_extents_bottom)
	  && root_x >= tem->x - tem->frame_extents_left
	  && root_x < (tem->x + tem->width - 1
		       + tem->frame_extents_right))
	return None;
      *extents_p = false;

      if (root_x >= tem->x && root_y >= tem->y
	  && root_x < tem->x + tem->width
	  && root_y < tem->y + tem->height)
	{
#ifdef HAVE_XSHAPE
	  if (tem->n_bounding_rects == -1)
#endif
	    {
	      chosen = tem;
	      break;
	    }

#ifdef HAVE_XSHAPE
	  if (x_dnd_get_target_window_2 (tem->bounding_rects,
					 tem->n_bounding_rects,
					 tem->border_width + root_x - tem->x,
					 tem->border_width + root_y - tem->y))
	    {
	      if (tem->n_input_rects == -1
		  || x_dnd_get_target_window_2 (tem->input_rects,
						tem->n_input_rects,
						(tem->border_width
						 + root_x - tem->x),
						(tem->border_width
						 + root_y - tem->y)))
		{
		  chosen = tem;
		  break;
		}
	    }
#endif
	}
    }

  if (chosen)
    {
      *motif_out = (x_dnd_disable_motif_protocol
		    ? XM_DRAG_STYLE_NONE
		    : chosen->xm_protocol_style);
      return chosen->window;
    }
  else
    *motif_out = XM_DRAG_STYLE_NONE;

  return None;
}

static int
x_dnd_get_wm_state_and_proto (struct x_display_info *dpyinfo,
			      Window window, int *wmstate_out,
			      int *proto_out, int *motif_out,
			      Window *proxy_out)
{
#ifndef USE_XCB
  Atom type;
  int format;
  unsigned long nitems, bytes_after;
  unsigned char *data = NULL;
  xm_drag_receiver_info xm_info;
#else
  xcb_get_property_cookie_t wmstate_cookie;
  xcb_get_property_cookie_t xdnd_proto_cookie;
  xcb_get_property_cookie_t xdnd_proxy_cookie;
  xcb_get_property_cookie_t xm_style_cookie;
  xcb_get_property_reply_t *reply;
  xcb_generic_error_t *error;
  uint8_t *xmdata;
#endif
  int rc;

#ifndef USE_XCB
  x_catch_errors (dpyinfo->display);
  rc = ((XGetWindowProperty (dpyinfo->display, window,
			     dpyinfo->Xatom_wm_state,
			     0, 2, False, AnyPropertyType,
			     &type, &format, &nitems,
			     &bytes_after, &data)
	 == Success)
	&& !x_had_errors_p (dpyinfo->display)
	&& data && nitems == 2 && format == 32);
  x_uncatch_errors ();

  if (rc)
    *wmstate_out = *(unsigned long *) data;

  *proto_out = x_dnd_get_window_proto (dpyinfo, window);

  if (!xm_read_drag_receiver_info (dpyinfo, window, &xm_info))
    *motif_out = xm_info.protocol_style;
  else
    *motif_out = XM_DRAG_STYLE_NONE;

  *proxy_out = x_dnd_get_window_proxy (dpyinfo, window);

  if (data)
    XFree (data);
#else
  rc = true;

  wmstate_cookie = xcb_get_property (dpyinfo->xcb_connection, 0,
				     (xcb_window_t) window,
				     (xcb_atom_t) dpyinfo->Xatom_wm_state,
				     0, 0, 2);
  xdnd_proto_cookie = xcb_get_property (dpyinfo->xcb_connection, 0,
					(xcb_window_t) window,
					(xcb_atom_t) dpyinfo->Xatom_XdndAware,
					XA_ATOM, 0, 1);
  xdnd_proxy_cookie = xcb_get_property (dpyinfo->xcb_connection, 0,
					(xcb_window_t) window,
					(xcb_atom_t) dpyinfo->Xatom_XdndProxy,
					XA_WINDOW, 0, 1);
  xm_style_cookie
    = xcb_get_property (dpyinfo->xcb_connection, 0,
			(xcb_window_t) window,
			(xcb_atom_t) dpyinfo->Xatom_MOTIF_DRAG_RECEIVER_INFO,
			(xcb_atom_t) dpyinfo->Xatom_MOTIF_DRAG_RECEIVER_INFO,
			0, 4);

  reply = xcb_get_property_reply (dpyinfo->xcb_connection,
				  wmstate_cookie, &error);

  if (!reply)
    free (error), rc = false;
  else
    {
      if (reply->format != 32
	  || xcb_get_property_value_length (reply) != 8)
	rc = false;
      else
	*wmstate_out = *(uint32_t *) xcb_get_property_value (reply);

      free (reply);
    }

  reply = xcb_get_property_reply (dpyinfo->xcb_connection,
				  xdnd_proto_cookie, &error);

  *proto_out = -1;
  if (!reply)
    free (error);
  else
    {
      if (reply->format == 32
	  && xcb_get_property_value_length (reply) >= 4)
	*proto_out = *(uint32_t *) xcb_get_property_value (reply);

      free (reply);
    }

  *proxy_out = None;
  reply = xcb_get_property_reply (dpyinfo->xcb_connection,
				  xdnd_proxy_cookie, &error);

  if (!reply)
    free (error);
  else
    {
      if (reply->format == 32
	  && reply->type == XA_WINDOW
	  && (xcb_get_property_value_length (reply) >= 4))
	*proxy_out = *(xcb_window_t *) xcb_get_property_value (reply);

      free (reply);
    }

  *motif_out = XM_DRAG_STYLE_NONE;

  reply = xcb_get_property_reply (dpyinfo->xcb_connection,
				  xm_style_cookie, &error);

  if (!reply)
    free (error);
 else
   {
     if (reply->format == 8
	 && reply->type == dpyinfo->Xatom_MOTIF_DRAG_RECEIVER_INFO
	 && xcb_get_property_value_length (reply) >= 4)
       {
	 xmdata = xcb_get_property_value (reply);
	 *motif_out = xmdata[2];
       }

     free (reply);
   }
#endif

  return rc;
}

/* From the XDND protocol specification:

   Dropping on windows that do not support XDND

   Since middle clicking is the universal shortcut for pasting
   in X, one can drop data into a window that does not support
   XDND by:

   1. After the mouse has been released to trigger the drop,
   obtain ownership of XA_PRIMARY.

   2. Send a ButtonPress event and then a ButtonRelease event to
   the deepest subwindow containing the mouse to simulate a
   middle click.  The times for these events should be the time
   of the actual button release +1 and +2, respectively.  These
   values will not be used by anybody else, so one can
   unambiguously recognize the resulting `XConvertSelection'
   request.

   3. If a request for XA_PRIMARY arrives bearing the timestamp
   of either the ButtonPress or the ButtonRelease event, treat
   it as a request for XdndSelection.  Note that you must use
   the X data types instead of the MIME types in this case.
   (e.g. XA_STRING instead of text/plain).  */
static void
x_dnd_do_unsupported_drop (struct x_display_info *dpyinfo,
			   Lisp_Object frame, Lisp_Object value,
			   Lisp_Object targets, Window target_window,
			   int root_x, int root_y, Time before)
{
  XEvent event;
  int dest_x, dest_y;
  Window child_return, child, owner;
  Lisp_Object current_value;
  struct frame *f;

  f = decode_window_system_frame (frame);

  if (NILP (value))
    return;

  if (!x_dnd_use_unsupported_drop)
    return;

  event.xbutton.serial = 0;
  event.xbutton.send_event = True;
  event.xbutton.display = dpyinfo->display;
  event.xbutton.root = dpyinfo->root_window;
  event.xbutton.x_root = root_x;
  event.xbutton.y_root = root_y;

  x_catch_errors (dpyinfo->display);

  child = dpyinfo->root_window;
  dest_x = root_x;
  dest_y = root_y;

  while (XTranslateCoordinates (dpyinfo->display, dpyinfo->root_window,
				child, root_x, root_y, &dest_x, &dest_y,
				&child_return)
	 && child_return != None)
    child = child_return;

  x_uncatch_errors ();

  if (!CONSP (value))
    return;

  current_value = assq_no_quit (QPRIMARY,
				dpyinfo->terminal->Vselection_alist);

  if (!NILP (current_value))
    current_value = XCAR (XCDR (current_value));

  x_own_selection (QPRIMARY, current_value, frame,
		   XCAR (XCDR (value)), before);

  owner = XGetSelectionOwner (dpyinfo->display, XA_PRIMARY);

  /* If we didn't successfully obtain selection ownership, refrain
     from generating events that will insert something else.  */

  if (owner != FRAME_X_WINDOW (f))
    return;

  /* mouse-drag-and-drop-region will immediately deactivate the mark
     after this is set.  Make sure the primary selection is not
     clobbered in that case by setting `deactivate-mark' to
     Qdont_save.  */
  Vdeactivate_mark = Qdont_save;

  event.xbutton.window = child;
  event.xbutton.subwindow = None;
  event.xbutton.x = dest_x;
  event.xbutton.y = dest_y;
  event.xbutton.state = 0;
  event.xbutton.button = 2;
  event.xbutton.same_screen = True;

  dpyinfo->pending_dnd_time = before;

  event.xbutton.type = ButtonPress;
  event.xbutton.time = before + 1;

  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSendEvent (dpyinfo->display, child,
	      True, ButtonPressMask, &event);

  event.xbutton.type = ButtonRelease;
  event.xbutton.time = before + 2;

  XSendEvent (dpyinfo->display, child,
	      True, ButtonReleaseMask, &event);
  x_stop_ignoring_errors (dpyinfo);

  x_dnd_action_symbol = QXdndActionPrivate;

  return;
}

static void
x_dnd_send_unsupported_drop (struct x_display_info *dpyinfo, Window target_window,
			     int root_x, int root_y, Time before)
{
  Lisp_Object targets, arg;
  int i;
  char **atom_names, *name;

  targets = Qnil;
  atom_names = alloca (sizeof *atom_names * x_dnd_n_targets);

  if (!XGetAtomNames (dpyinfo->display, x_dnd_targets,
		      x_dnd_n_targets, atom_names))
      return;

  for (i = x_dnd_n_targets; i > 0; --i)
    {
      targets = Fcons (build_string (atom_names[i - 1]),
		       targets);
      XFree (atom_names[i - 1]);
    }

  name = x_get_atom_name (dpyinfo, x_dnd_wanted_action,
			  NULL);

  if (name)
    {
      arg = intern (name);
      xfree (name);
    }
  else
    arg = Qnil;

  x_dnd_run_unsupported_drop_function = true;
  x_dnd_unsupported_drop_time = before;
  x_dnd_unsupported_drop_window = target_window;
  x_dnd_unsupported_drop_data
    = list (assq_no_quit (QXdndSelection,
			  dpyinfo->terminal->Vselection_alist),
	    targets, arg, make_fixnum (root_x),
	    make_fixnum (root_y));

  x_dnd_waiting_for_finish = true;
  x_dnd_finish_display = dpyinfo->display;
}

static Window
x_dnd_fill_empty_target (int *proto_out, int *motif_out,
			 Window *toplevel_out, bool *was_frame)
{
  *proto_out = -1;
  *motif_out = XM_DRAG_STYLE_NONE;
  *toplevel_out = None;
  *was_frame = false;

  return None;
}

static Window
x_dnd_get_target_window (struct x_display_info *dpyinfo,
			 int root_x, int root_y, int *proto_out,
			 int *motif_out, Window *toplevel_out,
			 bool *was_frame)
{
  Window child_return, child, proxy;
  int dest_x_return, dest_y_return, rc, proto, motif;
  int parent_x, parent_y;
  bool extents_p;
#if defined HAVE_XCOMPOSITE && (XCOMPOSITE_MAJOR > 0 || XCOMPOSITE_MINOR > 2)
  Window overlay_window;
  XWindowAttributes attrs;
#endif
  int wmstate;
  struct frame *tooltip, *f;
  bool unrelated;

  child_return = dpyinfo->root_window;
  dest_x_return = root_x;
  dest_y_return = root_y;

  proto = -1;
  *motif_out = XM_DRAG_STYLE_NONE;
  *toplevel_out = None;
  *was_frame = false;

  if (x_dnd_use_toplevels)
    {
      extents_p = false;
      child = x_dnd_get_target_window_1 (dpyinfo, root_x,
					 root_y, motif_out,
					 &extents_p);

      if (!x_dnd_allow_current_frame
	  && FRAME_X_WINDOW (x_dnd_frame) == child)
	*motif_out = XM_DRAG_STYLE_NONE;

      f = x_top_window_to_frame (dpyinfo, child);

      *toplevel_out = child;

      if (child != None)
	{
	  if (f)
	    {
	      *was_frame = true;
	      *proto_out = -1;
	      *motif_out = XM_DRAG_STYLE_NONE;

	      return child;
	    }

#ifndef USE_XCB
	  proxy = x_dnd_get_window_proxy (dpyinfo, child);
#else
	  x_dnd_get_proxy_proto (dpyinfo, child, &proxy, proto_out);
#endif

	  if (proxy != None)
	    {
	      proto = x_dnd_get_window_proto (dpyinfo, proxy);

	      if (proto != -1)
		{
		  *proto_out = proto;
		  return proxy;
		}
	    }

#ifndef USE_XCB
	  *proto_out = x_dnd_get_window_proto (dpyinfo, child);
#endif
	  return child;
	}

      if (extents_p)
	{
	  *proto_out = -1;
	  *motif_out = XM_DRAG_STYLE_NONE;
	  *toplevel_out = None;

	  return None;
	}

      /* Then look at the composite overlay window.  */
#if defined HAVE_XCOMPOSITE && (XCOMPOSITE_MAJOR > 0 || XCOMPOSITE_MINOR > 2)
      if (dpyinfo->composite_supported_p
	  && (dpyinfo->composite_major > 0
	      || dpyinfo->composite_minor > 2))
	{
	  if (XGetSelectionOwner (dpyinfo->display,
				  dpyinfo->Xatom_NET_WM_CM_Sn) != None)
	    {
	      x_catch_errors (dpyinfo->display);
	      XGrabServer (dpyinfo->display);
	      overlay_window = XCompositeGetOverlayWindow (dpyinfo->display,
							   dpyinfo->root_window);
	      XCompositeReleaseOverlayWindow (dpyinfo->display,
					      dpyinfo->root_window);
	      XUngrabServer (dpyinfo->display);

	      if (!x_had_errors_p (dpyinfo->display))
		{
		  XGetWindowAttributes (dpyinfo->display, overlay_window, &attrs);

		  if (attrs.map_state == IsViewable)
		    {
		      proxy = x_dnd_get_window_proxy (dpyinfo, overlay_window);

		      if (proxy != None)
			{
			  proto = x_dnd_get_window_proto (dpyinfo, proxy);

			  if (proto != -1)
			    {
			      *proto_out = proto;
			      *toplevel_out = overlay_window;
			      x_uncatch_errors_after_check ();

			      return proxy;
			    }
			}
		    }
		}
	      x_uncatch_errors_after_check ();
	    }
	}
#endif

      /* Now look for an XdndProxy on the root window.  */

      proxy = x_dnd_get_window_proxy (dpyinfo, dpyinfo->root_window);

      if (proxy != None)
	{
	  proto = x_dnd_get_window_proto (dpyinfo, dpyinfo->root_window);

	  if (proto != -1)
	    {
	      *toplevel_out = dpyinfo->root_window;
	      *proto_out = proto;
	      return proxy;
	    }
	}

      /* No toplevel was found and the overlay and root windows were
	 not proxies, so return None.  */
      *proto_out = -1;
      *toplevel_out = dpyinfo->root_window;
      return None;
    }

  /* Not strictly necessary, but satisfies GCC.  */
  child = dpyinfo->root_window;

  while (child_return != None)
    {
      child = child_return;
      parent_x = dest_x_return;
      parent_y = dest_y_return;

      x_catch_errors (dpyinfo->display);
      rc = XTranslateCoordinates (dpyinfo->display, dpyinfo->root_window,
				  child_return, root_x, root_y, &dest_x_return,
				  &dest_y_return, &child_return);

      if (x_had_errors_p (dpyinfo->display) || !rc)
	{
	  x_uncatch_errors_after_check ();
	  break;
	}

      if (child_return)
	{
	  /* If child_return is a tooltip frame, look beneath it.  We
	     never want to drop anything onto a tooltip frame.  */

	  tooltip = x_tooltip_window_to_frame (dpyinfo, child_return,
					       &unrelated);

	  if (tooltip || unrelated)
	    child_return = x_get_window_below (dpyinfo->display, child_return,
					       parent_x, parent_y, &dest_x_return,
					       &dest_y_return);

	  if (!child_return)
	    {
	      x_uncatch_errors ();
	      break;
	    }

	  f = x_top_window_to_frame (dpyinfo, child_return);

	  if (f)
	    {
	      *proto_out = -1;
	      *motif_out = XM_DRAG_STYLE_NONE;
	      *toplevel_out = child_return;
	      *was_frame = true;

	      return child_return;
	    }

	  if (x_dnd_get_wm_state_and_proto (dpyinfo, child_return,
					    &wmstate, &proto, &motif,
					    &proxy)
	      /* `proto' and `motif' are set by x_dnd_get_wm_state
		 even if getting the wm state failed.  */
	      || proto != -1 || motif != XM_DRAG_STYLE_NONE)
	    {
	      *proto_out = proto;
	      *motif_out = (x_dnd_disable_motif_protocol
			    ? XM_DRAG_STYLE_NONE : motif);
	      *toplevel_out = child_return;
	      x_uncatch_errors ();

	      return child_return;
	    }

	  if (proxy != None)
	    {
	      proto = x_dnd_get_window_proto (dpyinfo, proxy);

	      if (proto != -1)
		{
		  *proto_out = proto;
		  *toplevel_out = child_return;

		  x_uncatch_errors ();
		  return proxy;
		}
	    }
	}

      x_uncatch_errors ();
    }

#if defined HAVE_XCOMPOSITE && (XCOMPOSITE_MAJOR > 0 || XCOMPOSITE_MINOR > 2)
  if (child != dpyinfo->root_window)
    {
#endif
      if (child != None)
	{
	  proxy = x_dnd_get_window_proxy (dpyinfo, child);

	  if (proxy)
	    {
	      proto = x_dnd_get_window_proto (dpyinfo, proxy);

	      if (proto != -1)
		{
		  *proto_out = proto;
		  *toplevel_out = child;
		  return proxy;
		}
	    }
	}

      *proto_out = x_dnd_get_window_proto (dpyinfo, child);
      return child;
#if defined HAVE_XCOMPOSITE && (XCOMPOSITE_MAJOR > 0 || XCOMPOSITE_MINOR > 2)
    }
  else if (dpyinfo->composite_supported_p
	   && (dpyinfo->composite_major > 0
	       || dpyinfo->composite_minor > 2))
    {
      /* Only do this if a compositing manager is present.  */
      if (XGetSelectionOwner (dpyinfo->display,
			      dpyinfo->Xatom_NET_WM_CM_Sn) != None)
	{
	  x_catch_errors (dpyinfo->display);
	  XGrabServer (dpyinfo->display);
	  overlay_window = XCompositeGetOverlayWindow (dpyinfo->display,
						       dpyinfo->root_window);
	  XCompositeReleaseOverlayWindow (dpyinfo->display,
					  dpyinfo->root_window);
	  XUngrabServer (dpyinfo->display);

	  if (!x_had_errors_p (dpyinfo->display))
	    {
	      XGetWindowAttributes (dpyinfo->display, overlay_window, &attrs);

	      if (attrs.map_state == IsViewable)
		{
		  proxy = x_dnd_get_window_proxy (dpyinfo, overlay_window);

		  if (proxy != None)
		    {
		      proto = x_dnd_get_window_proto (dpyinfo, proxy);

		      if (proto != -1)
			{
			  *proto_out = proto;
			  *toplevel_out = overlay_window;
			  x_uncatch_errors_after_check ();

			  return proxy;
			}
		    }
		}
	    }
	  x_uncatch_errors_after_check ();
	}
    }

  if (child != None)
    {
      proxy = x_dnd_get_window_proxy (dpyinfo, child);

      if (proxy)
	{
	  proto = x_dnd_get_window_proto (dpyinfo, proxy);

	  if (proto != -1)
	    {
	      *toplevel_out = child;
	      *proto_out = proto;
	      return proxy;
	    }
	}
    }

  *proto_out = x_dnd_get_window_proto (dpyinfo, child);
  *toplevel_out = child;
  return child;
#endif
}

static Window
x_dnd_get_window_proxy (struct x_display_info *dpyinfo, Window wdesc)
{
  int rc, actual_format;
  unsigned long actual_size, bytes_remaining;
  unsigned char *tmp_data = NULL;
  XWindowAttributes attrs;
  Atom actual_type;
  Window proxy;

  proxy = None;
  x_catch_errors (dpyinfo->display);
  rc = XGetWindowProperty (dpyinfo->display, wdesc,
			   dpyinfo->Xatom_XdndProxy,
			   0, 1, False, XA_WINDOW,
			   &actual_type, &actual_format,
			   &actual_size, &bytes_remaining,
			   &tmp_data);

  if (!x_had_errors_p (dpyinfo->display)
      && rc == Success
      && tmp_data
      && actual_type == XA_WINDOW
      && actual_format == 32
      && actual_size == 1)
    {
      proxy = *(Window *) tmp_data;

      /* Verify the proxy window exists.  */
      XGetWindowAttributes (dpyinfo->display, proxy, &attrs);

      if (x_had_errors_p (dpyinfo->display))
	proxy = None;
    }

  if (tmp_data)
    XFree (tmp_data);
  x_uncatch_errors_after_check ();

  return proxy;
}

static int
x_dnd_get_window_proto (struct x_display_info *dpyinfo, Window wdesc)
{
  Atom actual, value;
  unsigned char *tmp_data = NULL;
  int rc, format;
  unsigned long n, left;
  bool had_errors;

  if (wdesc == None || (!x_dnd_allow_current_frame
			&& wdesc == FRAME_OUTER_WINDOW (x_dnd_frame)))
    return -1;

  x_catch_errors (dpyinfo->display);
  rc = XGetWindowProperty (dpyinfo->display, wdesc, dpyinfo->Xatom_XdndAware,
			   0, 1, False, XA_ATOM, &actual, &format, &n, &left,
			   &tmp_data);
  had_errors = x_had_errors_p (dpyinfo->display);
  x_uncatch_errors_after_check ();

  if (had_errors || rc != Success || actual != XA_ATOM || format != 32 || n < 1
      || !tmp_data)
    {
      if (tmp_data)
	XFree (tmp_data);
      return -1;
    }

  value = (int) *(Atom *) tmp_data;
  XFree (tmp_data);

  return min (X_DND_SUPPORTED_VERSION, (int) value);
}

static void
x_dnd_send_enter (struct frame *f, Window target, Window toplevel,
		  int supported)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  int i;
  XEvent msg;

  msg.xclient.type = ClientMessage;
  msg.xclient.message_type = dpyinfo->Xatom_XdndEnter;
  msg.xclient.format = 32;
  msg.xclient.window = toplevel;
  msg.xclient.data.l[0] = FRAME_X_WINDOW (f);
  msg.xclient.data.l[1] = (((unsigned int) min (X_DND_SUPPORTED_VERSION,
						supported) << 24)
			   | (x_dnd_n_targets > 3 ? 1 : 0));
  msg.xclient.data.l[2] = 0;
  msg.xclient.data.l[3] = 0;
  msg.xclient.data.l[4] = 0;

  for (i = 0; i < min (3, x_dnd_n_targets); ++i)
    msg.xclient.data.l[i + 2] = x_dnd_targets[i];

  if (x_dnd_n_targets > 3 && !x_dnd_init_type_lists)
    XChangeProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		     dpyinfo->Xatom_XdndTypeList, XA_ATOM, 32,
		     PropModeReplace, (unsigned char *) x_dnd_targets,
		     x_dnd_n_targets);

  /* Now record that the type list has already been set (if required),
     so we don't have to set it again.  */
  x_dnd_init_type_lists = true;

  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSendEvent (FRAME_X_DISPLAY (f), target, False, NoEventMask, &msg);
  x_stop_ignoring_errors (dpyinfo);
}

static void
x_dnd_send_position (struct frame *f, Window target, Window toplevel,
		     int supported, unsigned short root_x,
		     unsigned short root_y, Time timestamp, Atom action,
		     int button, unsigned state)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  XEvent msg;

  msg.xclient.type = ClientMessage;
  msg.xclient.message_type = dpyinfo->Xatom_XdndPosition;
  msg.xclient.format = 32;
  msg.xclient.window = toplevel;
  msg.xclient.data.l[0] = FRAME_X_WINDOW (f);
  msg.xclient.data.l[1] = 0;

  /* This is problematic because it's not specified in the
     freedesktop.org copy of the protocol specification, but the copy
     maintained by the original author of the protocol specifies it
     for all versions.  Since at least one program supports these
     flags, but uses protocol v4 (and not v5), set them for all
     protocol versions.  */
  if (button >= 4 && button <= 7)
    {
      msg.xclient.data.l[1] |= (1 << 10);
      msg.xclient.data.l[1] |= (button - 4) << 8;
    }
  else if (button)
    return;

  msg.xclient.data.l[1] |= state & 0xff;

  msg.xclient.data.l[2] = (root_x << 16) | root_y;
  msg.xclient.data.l[3] = 0;
  msg.xclient.data.l[4] = 0;

  if (supported >= 3)
    msg.xclient.data.l[3] = timestamp;

  if (supported >= 4)
    msg.xclient.data.l[4] = action;

  if (x_dnd_waiting_for_status_window == target)
    {
      x_dnd_pending_send_position = msg;
      x_dnd_pending_send_position_button = button;
      x_dnd_pending_send_position_root_x = root_x;
      x_dnd_pending_send_position_root_y = root_y;
    }
  else
    {
      if (target == x_dnd_mouse_rect_target
	  && x_dnd_mouse_rect.width
	  && x_dnd_mouse_rect.height
	  /* Ignore the mouse rectangle if we're supposed to be sending a
	     button press instead.  */
	  && !button)
	{
	  if (root_x >= x_dnd_mouse_rect.x
	      && root_x < (x_dnd_mouse_rect.x
			   + x_dnd_mouse_rect.width)
	      && root_y >= x_dnd_mouse_rect.y
	      && root_y < (x_dnd_mouse_rect.y
			   + x_dnd_mouse_rect.height))
	    return;
	}

      x_ignore_errors_for_next_request (dpyinfo, 0);
      XSendEvent (FRAME_X_DISPLAY (f), target, False, NoEventMask, &msg);
      x_stop_ignoring_errors (dpyinfo);

      x_dnd_waiting_for_status_window = target;
    }
}

static void
x_dnd_send_leave (struct frame *f, Window target, Window toplevel)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  XEvent msg;

  msg.xclient.type = ClientMessage;
  msg.xclient.message_type = dpyinfo->Xatom_XdndLeave;
  msg.xclient.format = 32;
  msg.xclient.window = toplevel;
  msg.xclient.data.l[0] = FRAME_X_WINDOW (f);
  msg.xclient.data.l[1] = 0;
  msg.xclient.data.l[2] = 0;
  msg.xclient.data.l[3] = 0;
  msg.xclient.data.l[4] = 0;

  x_dnd_waiting_for_status_window = None;
  x_dnd_pending_send_position.type = 0;

  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSendEvent (FRAME_X_DISPLAY (f), target, False, NoEventMask, &msg);
  x_stop_ignoring_errors (dpyinfo);
}

static bool
x_dnd_send_drop (struct frame *f, Window target, Window toplevel,
		 Time timestamp, int supported)
{
  struct x_display_info *dpyinfo;
  XEvent msg;

  if (x_dnd_action == None)
    {
      x_dnd_send_leave (f, target, toplevel);
      return false;
    }

  dpyinfo = FRAME_DISPLAY_INFO (f);

  msg.xclient.type = ClientMessage;
  msg.xclient.message_type = dpyinfo->Xatom_XdndDrop;
  msg.xclient.format = 32;
  msg.xclient.window = toplevel;
  msg.xclient.data.l[0] = FRAME_X_WINDOW (f);
  msg.xclient.data.l[1] = 0;
  msg.xclient.data.l[2] = 0;
  msg.xclient.data.l[3] = 0;
  msg.xclient.data.l[4] = 0;

  if (supported >= 1)
    msg.xclient.data.l[2] = timestamp;

  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSendEvent (FRAME_X_DISPLAY (f), target, False, NoEventMask, &msg);
  x_stop_ignoring_errors (dpyinfo);
  return true;
}

static bool
x_dnd_do_drop (Window target, Window toplevel, int supported)
{
  if (x_dnd_waiting_for_status_window != target)
    return x_dnd_send_drop (x_dnd_frame, target, toplevel,
			    x_dnd_selection_timestamp, supported);

  x_dnd_need_send_drop = true;
  x_dnd_send_drop_proto = supported;

  return true;
}

static void
x_set_dnd_targets (Atom *targets, int ntargets)
{
  if (x_dnd_targets)
    xfree (x_dnd_targets);

  block_input ();
  x_dnd_targets = xmalloc (sizeof *targets * ntargets);
  x_dnd_n_targets = ntargets;

  memcpy (x_dnd_targets, targets,
	  sizeof *targets * ntargets);
  unblock_input ();
}

static void
x_free_dnd_targets (void)
{
  if (!x_dnd_targets)
    return;

  xfree (x_dnd_targets);
  x_dnd_targets = NULL;
  x_dnd_n_targets = 0;
}

/* Clear some Lisp variables after the drop finishes, so they are
   freed by the GC.  */

static void
x_clear_dnd_variables (void)
{
  x_dnd_monitors = Qnil;
  x_dnd_unsupported_drop_data = Qnil;
}

static void
x_free_dnd_toplevels (void)
{
  if (!x_dnd_use_toplevels || !x_dnd_toplevels)
    return;

  /* If the display is deleted, x_dnd_toplevels will already be
     NULL, so we can always assume the display is alive here.  */

  x_dnd_free_toplevels (true);
}

/* Restore event masks and window properties changed during a
   drag-and-drop operation, after it finishes.  */
static void
x_restore_events_after_dnd (struct frame *f, XWindowAttributes *wa)
{
  struct x_display_info *dpyinfo;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  /* Restore the old event mask.  */
  XSelectInput (dpyinfo->display, dpyinfo->root_window,
		wa->your_event_mask);
#ifdef HAVE_XKB
  if (dpyinfo->supports_xkb)
    XkbSelectEvents (dpyinfo->display, XkbUseCoreKbd,
		     XkbStateNotifyMask, 0);
#endif
  /* Delete the Motif drag initiator info if it was set up.  */
  if (x_dnd_motif_setup_p)
    XDeleteProperty (dpyinfo->display, FRAME_X_WINDOW (f),
		     x_dnd_motif_atom);

  /* Remove any type list set as well.  */
  if (x_dnd_init_type_lists && x_dnd_n_targets > 3)
    XDeleteProperty (dpyinfo->display, FRAME_X_WINDOW (f),
		     dpyinfo->Xatom_XdndTypeList);
}

#ifdef HAVE_XINPUT2

/* Cancel the current drag-and-drop operation, sending leave messages
   to any relevant toplevels.  This is called from the event loop when
   an event is received telling Emacs to gracefully cancel the
   drag-and-drop operation.  */

static void
x_dnd_cancel_dnd_early (void)
{
  struct frame *f;
  xm_drop_start_message dmsg;

  eassert (x_dnd_frame && x_dnd_in_progress);

  f = x_dnd_frame;

  if (x_dnd_last_seen_window != None
      && x_dnd_last_protocol_version != -1)
    x_dnd_send_leave (x_dnd_frame,
		      x_dnd_last_seen_window,
		      x_dnd_last_seen_toplevel);
  else if (x_dnd_last_seen_window != None
	   && !XM_DRAG_STYLE_IS_DROP_ONLY (x_dnd_last_motif_style)
	   && x_dnd_last_motif_style != XM_DRAG_STYLE_NONE
	   && x_dnd_motif_setup_p)
    {
      dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
				    XM_DRAG_REASON_DROP_START);
      dmsg.byte_order = XM_BYTE_ORDER_CUR_FIRST;
      dmsg.timestamp = FRAME_DISPLAY_INFO (f)->last_user_time;
      dmsg.side_effects
	= XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (FRAME_DISPLAY_INFO (f),
							   x_dnd_wanted_action),
			       XM_DROP_SITE_VALID, x_dnd_motif_operations,
			       XM_DROP_ACTION_DROP_CANCEL);
      dmsg.x = 0;
      dmsg.y = 0;
      dmsg.index_atom = x_dnd_motif_atom;
      dmsg.source_window = FRAME_X_WINDOW (f);

      x_dnd_send_xm_leave_for_drop (FRAME_DISPLAY_INFO (f), f,
				    x_dnd_last_seen_window,
				    FRAME_DISPLAY_INFO (f)->last_user_time);
      xm_send_drop_message (FRAME_DISPLAY_INFO (f), FRAME_X_WINDOW (f),
			    x_dnd_last_seen_window, &dmsg);
    }

  x_dnd_last_seen_window = None;
  x_dnd_last_seen_toplevel = None;
  x_dnd_in_progress = false;
  x_dnd_waiting_for_finish = false;
  x_dnd_return_frame_object = NULL;
  x_dnd_movement_frame = NULL;
  x_dnd_wheel_frame = NULL;
  x_dnd_frame = NULL;
  x_dnd_action = None;
  x_dnd_action_symbol = Qnil;
}

#endif

static void
x_dnd_cleanup_drag_and_drop (void *frame)
{
  struct frame *f = frame;
  xm_drop_start_message dmsg;

  if (!x_dnd_unwind_flag)
    return;

  if (x_dnd_in_progress)
    {
      eassert (x_dnd_frame);

      block_input ();
      if (x_dnd_last_seen_window != None
	  && x_dnd_last_protocol_version != -1)
	x_dnd_send_leave (x_dnd_frame,
			  x_dnd_last_seen_window,
			  x_dnd_last_seen_toplevel);
      else if (x_dnd_last_seen_window != None
	       && !XM_DRAG_STYLE_IS_DROP_ONLY (x_dnd_last_motif_style)
	       && x_dnd_last_motif_style != XM_DRAG_STYLE_NONE
	       && x_dnd_motif_setup_p)
	{
	  dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
					XM_DRAG_REASON_DROP_START);
	  dmsg.byte_order = XM_BYTE_ORDER_CUR_FIRST;
	  dmsg.timestamp = FRAME_DISPLAY_INFO (f)->last_user_time;
	  dmsg.side_effects
	    = XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (FRAME_DISPLAY_INFO (f),
							       x_dnd_wanted_action),
				   XM_DROP_SITE_VALID, x_dnd_motif_operations,
				   XM_DROP_ACTION_DROP_CANCEL);
	  dmsg.x = 0;
	  dmsg.y = 0;
	  dmsg.index_atom = x_dnd_motif_atom;
	  dmsg.source_window = FRAME_X_WINDOW (f);

	  x_dnd_send_xm_leave_for_drop (FRAME_DISPLAY_INFO (f), f,
					x_dnd_last_seen_window,
					FRAME_DISPLAY_INFO (f)->last_user_time);
	  xm_send_drop_message (FRAME_DISPLAY_INFO (f), FRAME_X_WINDOW (f),
				x_dnd_last_seen_window, &dmsg);
	}
      unblock_input ();

      x_dnd_end_window = x_dnd_last_seen_window;
      x_dnd_last_seen_window = None;
      x_dnd_last_seen_toplevel = None;
      x_dnd_in_progress = false;
    }

  x_dnd_waiting_for_finish = false;

  FRAME_DISPLAY_INFO (f)->grabbed = 0;
#ifdef USE_GTK
  current_hold_quit = NULL;
#endif
  x_dnd_return_frame_object = NULL;
  x_dnd_movement_frame = NULL;
  x_dnd_wheel_frame = NULL;
  x_dnd_frame = NULL;

  x_restore_events_after_dnd (f, &x_dnd_old_window_attrs);
}



/* Primitives for simplified drag-and-drop tracking when items are
   being dragged between frames comprising the same Emacs session.  */

static void
x_dnd_note_self_position (struct x_display_info *dpyinfo, Window target,
			  unsigned short root_x, unsigned short root_y)
{
  struct frame *f;
  int dest_x, dest_y;

  f = x_top_window_to_frame (dpyinfo, target);

  if (f)
    {
      x_translate_coordinates (f, root_x, root_y, &dest_x, &dest_y);

      x_dnd_movement_frame = f;
      x_dnd_movement_x = dest_x;
      x_dnd_movement_y = dest_y;

      return;
    }
}

static void
x_dnd_note_self_wheel (struct x_display_info *dpyinfo, Window target,
		       unsigned short root_x, unsigned short root_y,
		       int button, unsigned int state, Time time)
{
  struct frame *f;
  int dest_x, dest_y;

  if (button < 4 || button > 7)
    return;

  f = x_top_window_to_frame (dpyinfo, target);

  if (f)
    {
      x_translate_coordinates (f, root_x, root_y, &dest_x, &dest_y);

      x_dnd_wheel_frame = f;
      x_dnd_wheel_x = dest_x;
      x_dnd_wheel_y = dest_y;
      x_dnd_wheel_button = button;
      x_dnd_wheel_state = state;
      x_dnd_wheel_time = time;

      return;
    }
}

static void
x_dnd_note_self_drop (struct x_display_info *dpyinfo, Window target,
		      unsigned short root_x, unsigned short root_y,
		      Time timestamp)
{
  struct input_event ie;
  struct frame *f;
  Lisp_Object lval;
  char **atom_names;
  char *name;
  int win_x, win_y, i;

  if (!x_dnd_allow_current_frame
      && (FRAME_OUTER_WINDOW (x_dnd_frame)
	  == target))
    return;

  f = x_top_window_to_frame (dpyinfo, target);

  if (!f)
    return;

  if (NILP (Vx_dnd_native_test_function))
    return;

  x_translate_coordinates (f, root_x, root_y, &win_x, &win_y);

  /* Emacs can't respond to DND events inside the nested event loop,
     so when dragging items to itself, call the test function
     manually.  */

  XSETFRAME (lval, f);
  x_dnd_action = None;
  x_dnd_action_symbol
    = safe_calln (Vx_dnd_native_test_function,
		  Fposn_at_x_y (make_fixnum (win_x),
				make_fixnum (win_y),
				lval, Qnil),
		  x_atom_to_symbol (dpyinfo,
				    x_dnd_wanted_action));

  if (!SYMBOLP (x_dnd_action_symbol))
    return;

  EVENT_INIT (ie);

  ie.kind = DRAG_N_DROP_EVENT;
  XSETFRAME (ie.frame_or_window, f);

  lval = Qnil;
  atom_names = alloca (x_dnd_n_targets * sizeof *atom_names);
  name = x_get_atom_name (dpyinfo, x_dnd_wanted_action, NULL);

  if (!XGetAtomNames (dpyinfo->display, x_dnd_targets,
		      x_dnd_n_targets, atom_names))
    {
      xfree (name);
      return;
    }

  for (i = x_dnd_n_targets; i != 0; --i)
    {
      lval = Fcons (intern (atom_names[i - 1]), lval);
      XFree (atom_names[i - 1]);
    }

  lval = Fcons (assq_no_quit (QXdndSelection,
			      FRAME_TERMINAL (f)->Vselection_alist),
		lval);
  lval = Fcons (intern (name), lval);
  lval = Fcons (QXdndSelection, lval);
  ie.arg = lval;
  ie.timestamp = timestamp;

  XSETINT (ie.x, win_x);
  XSETINT (ie.y, win_y);

  xfree (name);
  kbd_buffer_store_event (&ie);
}



/* Miscellaneous X event and graphics extension functions.  */

/* Flush display of frame F.  */

static void
x_flush (struct frame *f)
{
  eassert (f && FRAME_X_P (f));
  /* Don't call XFlush when it is not safe to redisplay; the X
     connection may be broken.  */
  if (!NILP (Vinhibit_redisplay))
    return;

  block_input ();
  XFlush (FRAME_X_DISPLAY (f));
  unblock_input ();
}

#ifdef HAVE_XDBE
static void
x_drop_xrender_surfaces (struct frame *f)
{
  font_drop_xrender_surfaces (f);

#ifdef HAVE_XRENDER
  if (f && FRAME_X_DOUBLE_BUFFERED_P (f)
      && FRAME_X_PICTURE (f) != None)
    {
      XRenderFreePicture (FRAME_X_DISPLAY (f),
			  FRAME_X_PICTURE (f));
      FRAME_X_PICTURE (f) = None;
    }
#endif
}
#endif

#ifdef HAVE_XRENDER
void
x_xr_ensure_picture (struct frame *f)
{
  if (FRAME_X_PICTURE (f) == None && FRAME_X_PICTURE_FORMAT (f))
    {
      XRenderPictureAttributes attrs;
      attrs.clip_mask = None;
      XRenderPictFormat *fmt = FRAME_X_PICTURE_FORMAT (f);

      FRAME_X_PICTURE (f) = XRenderCreatePicture (FRAME_X_DISPLAY (f),
						  FRAME_X_RAW_DRAWABLE (f),
						  fmt, CPClipMask, &attrs);
    }
}
#endif


/***********************************************************************
			      Debugging
 ***********************************************************************/

#if false

/* This is a function useful for recording debugging information about
   the sequence of occurrences in this file.  */

struct record
{
  char *locus;
  int type;
};

struct record event_record[100];

int event_record_index;

void
record_event (char *locus, int type)
{
  if (event_record_index == ARRAYELTS (event_record))
    event_record_index = 0;

  event_record[event_record_index].locus = locus;
  event_record[event_record_index].type = type;
  event_record_index++;
}

#endif



/* Miscellaneous event handling functions.  */

static void
x_toolkit_position (struct frame *f, int x, int y,
		    bool *menu_bar_p, bool *tool_bar_p)
{
#ifdef USE_GTK
  GdkRectangle test_rect;
  int scale;

  y += (FRAME_MENUBAR_HEIGHT (f)
	+ FRAME_TOOLBAR_TOP_HEIGHT (f));
  x += FRAME_TOOLBAR_LEFT_WIDTH (f);

  if (FRAME_EXTERNAL_MENU_BAR (f))
    *menu_bar_p = (x >= 0 && x < FRAME_PIXEL_WIDTH (f)
		   && y >= 0 && y < FRAME_MENUBAR_HEIGHT (f));

  if (FRAME_X_OUTPUT (f)->toolbar_widget)
    {
      scale = xg_get_scale (f);
      test_rect.x = x / scale;
      test_rect.y = y / scale;
      test_rect.width = 1;
      test_rect.height = 1;

      *tool_bar_p = gtk_widget_intersect (FRAME_X_OUTPUT (f)->toolbar_widget,
					  &test_rect, NULL);
    }
#elif defined USE_X_TOOLKIT
  *menu_bar_p = (x > 0 && x < FRAME_PIXEL_WIDTH (f)
		 && (y < 0 && y >= -FRAME_MENUBAR_HEIGHT (f)));
#else
  *menu_bar_p = (WINDOWP (f->menu_bar_window)
		 && (x > 0 && x < FRAME_PIXEL_WIDTH (f)
		     && (y > 0 && y < FRAME_MENU_BAR_HEIGHT (f))));
#endif
}

static void
x_update_opaque_region (struct frame *f, XEvent *configure)
{
  unsigned long opaque_region[] = {0, 0,
				   (configure
				    ? configure->xconfigure.width
				    : FRAME_PIXEL_WIDTH (f)),
				   (configure
				    ? configure->xconfigure.height
				    : FRAME_PIXEL_HEIGHT (f))};

  if (!FRAME_DISPLAY_INFO (f)->alpha_bits)
    return;

  if (f->alpha_background < 1.0)
    XChangeProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		     FRAME_DISPLAY_INFO (f)->Xatom_net_wm_opaque_region,
		     XA_CARDINAL, 32, PropModeReplace,
		     NULL, 0);
#ifndef HAVE_GTK3
  else
    XChangeProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		     FRAME_DISPLAY_INFO (f)->Xatom_net_wm_opaque_region,
		     XA_CARDINAL, 32, PropModeReplace,
		     (unsigned char *) &opaque_region, 4);
#else
  else if (FRAME_TOOLTIP_P (f))
    XChangeProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		     FRAME_DISPLAY_INFO (f)->Xatom_net_wm_opaque_region,
		     XA_CARDINAL, 32, PropModeReplace,
		     (unsigned char *) &opaque_region, 4);
  else
    {
      /* This causes child frames to not update correctly for an
	 unknown reason.  (bug#55779) */
      if (!FRAME_PARENT_FRAME (f))
	{
	  GObjectClass *object_class
	    = G_OBJECT_GET_CLASS (FRAME_GTK_OUTER_WIDGET (f));
	  GtkWidgetClass *class = GTK_WIDGET_CLASS (object_class);

	  if (class->style_updated)
	    class->style_updated (FRAME_GTK_OUTER_WIDGET (f));
	}
    }
#endif
}


#if defined USE_CAIRO || defined HAVE_XRENDER
static int
x_gc_free_ext_data_private (XExtData *extension)
{
  xfree (extension->private_data);

  return 0;
}

static struct x_gc_ext_data *
x_gc_get_ext_data (struct frame *f, GC gc, int create_if_not_found_p)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  XEDataObject object;
  XExtData **head, *ext_data;

  object.gc = gc;
  head = XEHeadOfExtensionList (object);
  ext_data = XFindOnExtensionList (head, dpyinfo->ext_codes->extension);
  if (ext_data == NULL)
    {
      if (!create_if_not_found_p)
	return NULL;
      else
	{
	  ext_data = xzalloc (sizeof (*ext_data));
	  ext_data->number = dpyinfo->ext_codes->extension;
	  ext_data->private_data = xzalloc (sizeof (struct x_gc_ext_data));
	  ext_data->free_private = x_gc_free_ext_data_private;
	  XAddToExtensionList (head, ext_data);
	}
    }
  return (struct x_gc_ext_data *) ext_data->private_data;
}

static void
x_extension_initialize (struct x_display_info *dpyinfo)
{
  XExtCodes *ext_codes = XAddExtension (dpyinfo->display);

  dpyinfo->ext_codes = ext_codes;
}
#endif

#ifdef USE_CAIRO

#define FRAME_CR_CONTEXT(f)	((f)->output_data.x->cr_context)
#define FRAME_CR_SURFACE_DESIRED_WIDTH(f) \
  ((f)->output_data.x->cr_surface_desired_width)
#define FRAME_CR_SURFACE_DESIRED_HEIGHT(f) \
  ((f)->output_data.x->cr_surface_desired_height)

#endif /* HAVE_CAIRO */



/* X input extension device and event mask management functions.  */

#ifdef HAVE_XINPUT2

# if defined USE_X_TOOLKIT || !(defined USE_GTK && defined HAVE_GTK3)
bool
xi_frame_selected_for (struct frame *f, unsigned long event)
{
  XIEventMask *masks;
  int i;

  masks = FRAME_X_OUTPUT (f)->xi_masks;

  if (!masks)
    return false;

  for (i = 0; i < FRAME_X_OUTPUT (f)->num_xi_masks; ++i)
    {
      if (masks[i].mask_len >= XIMaskLen (event)
	  && XIMaskIsSet (masks[i].mask, event))
	return true;
    }

  return false;
}
# endif

/* Convert XI2 button state IN to a standard X button modifier
   mask, and place it in OUT.  */

static void
xi_convert_button_state (XIButtonState *in, unsigned int *out)
{
  int i;

  if (in->mask_len)
    {
      for (i = 1; i <= 8; ++i)
	{
	  if (XIMaskIsSet (in->mask, i))
	    *out |= (Button1Mask << (i - 1));
	}
    }
}

/* Return the modifier state in XEV as a standard X modifier mask.
   This should be used for non-keyboard events, where the group does
   not matter.  */

#ifdef USE_GTK
static
#endif /* USE_GTK */
unsigned int
xi_convert_event_state (XIDeviceEvent *xev)
{
  unsigned int mods, buttons;

  mods = xev->mods.effective;
  buttons = 0;

  xi_convert_button_state (&xev->buttons, &buttons);

  return mods | buttons;
}

/* Like the above.  However, buttons are not converted, while the
   group is.  This should be used for key events being passed to the
   likes of input methods and Xt.  */

static unsigned int
xi_convert_event_keyboard_state (XIDeviceEvent *xev)
{
  return ((xev->mods.effective & ~(1 << 13 | 1 << 14))
	  | (xev->group.effective << 13));
}

/* Free all XI2 devices on DPYINFO.  */

static void
x_free_xi_devices (struct x_display_info *dpyinfo)
{
#ifdef HAVE_XINPUT2_2
  struct xi_touch_point_t *tem, *last;
#endif /* HAVE_XINPUT2_2 */

  block_input ();

  if (dpyinfo->num_devices)
    {
      for (int i = 0; i < dpyinfo->num_devices; ++i)
	{
#ifdef HAVE_XINPUT2_1
	  xfree (dpyinfo->devices[i].valuators);
#endif /* HAVE_XINPUT2_1 */

#ifdef HAVE_XINPUT2_2
	  tem = dpyinfo->devices[i].touchpoints;
	  while (tem)
	    {
	      last = tem;
	      tem = tem->next;
	      xfree (last);
	    }
#endif /* HAVE_XINPUT2_2 */
	}

      xfree (dpyinfo->devices);
      dpyinfo->devices = NULL;
      dpyinfo->num_devices = 0;
    }

  unblock_input ();
}

#ifdef HAVE_XINPUT2_1

struct xi_known_valuator
{
  /* The current value of this valuator.  */
  double current_value;

  /* The number of the valuator.  */
  int number;

  /* The next valuator whose value we already know.  */
  struct xi_known_valuator *next;
};

/* Populate the scroll valuator at INDEX in DEVICE with the scroll
   valuator information provided in INFO.

   The information consists of:

     - whether or not the valuator is horizontal.

     - whether or not the valuator's value is currently unknown,
       until the next XI_Motion event is received or the valuator's
       value is restored by the caller upon encountering valuator
       class data.

     - what the current value of the valuator is.  This is set to
       DBL_MIN for debugging purposes, but can be any value, as
       invalid_p is currently true.

     - the increment, which defines the amount of movement equal to a
       single unit of scrolling.  For example, if the increment is
       2.0, then a WHEEL_DOWN or WHEEL_UP event will be sent every
       time the valuator value changes by 2.0, unless
       mwheel-coalesce-scroll-events is nil.

     - the number used in XI_Motion events and elsewhere to identify
       the valuator.  */

static void
xi_populate_scroll_valuator (struct xi_device_t *device,
			     int index, XIScrollClassInfo *info)
{
  struct xi_scroll_valuator_t *valuator;

  valuator = &device->valuators[index];
  valuator->horizontal
    = (info->scroll_type == XIScrollTypeHorizontal);
  valuator->invalid_p = true;
  valuator->emacs_value = DBL_MIN;
  valuator->increment = info->increment;
  valuator->number = info->number;
}

#endif /* HAVE_XINPUT2_1 */

static void
xi_populate_device_from_info (struct x_display_info *dpyinfo,
			      struct xi_device_t *xi_device,
			      XIDeviceInfo *device)
{
#ifdef HAVE_XINPUT2_1
  struct xi_known_valuator *values, *tem;
  int actual_valuator_count, c;
  XIScrollClassInfo *info;
  XIValuatorClassInfo *valuator_info;
#endif /* HAVE_XINPUT2_1 */
#ifdef HAVE_XINPUT2_2
  XITouchClassInfo *touch_info;
#endif /* HAVE_XINPUT2_2 */

#ifdef HAVE_XINPUT2_1
  USE_SAFE_ALLOCA;
#endif /* HAVE_XINPUT2_1 */

  /* Initialize generic information about the device: its ID, which
     buttons are currently pressed and thus presumably actively
     grabbing the device, what kind of device it is (master pointer,
     master keyboard, slave pointer, slave keyboard, or floating
     slave), and its attachment.

     Here is a brief description of what device uses and attachments
     are.  Under XInput 2, user input from individual input devices is
     multiplexed into specific seats before being delivered, with each
     seat corresponding to a single on-screen mouse pointer and having
     its own keyboard focus.  Each seat consists of two virtual
     devices: the master keyboard and the master pointer, the first of
     which is used to report all keyboard input, with the other used
     to report all other input.

     Input from each physical device (mouse, trackpad or keyboard) is
     then associated with that slave device's paired master device.
     For example, moving the device "Logitech USB Optical Mouse",
     enslaved by the master pointer device "Virtual core pointer",
     will result in movement of the mouse pointer associated with that
     master device's seat.  If the pointer moves over an Emacs frame,
     then the frame will receive XI_Enter and XI_Motion events from
     that master pointer.

     Likewise, keyboard input from the device "AT Translated Set 2
     keyboard", enslaved by the master keyboard device "Virtual core
     keyboard", will be reported to its seat's input focus window.

     The device use describes what the device is.  The meanings of
     MasterPointer, MasterKeyboard, SlavePointer and SlaveKeyboard
     should be obvious.  FloatingSlave means the device is a slave
     device that is not associated with any seat, and thus generates
     no input.

     The device attachment is a device ID whose meaning varies
     depending on the device's use.  If a device is a master device,
     then its attachment is the device ID of the other device in its
     seat (the master keyboard for master pointer devices and vice
     versa.)  Otherwise, it is the ID of the master device the slave
     device is attached to.  For slave devices not attached to any
     seat, its value is undefined.

     Emacs receives ordinary pointer and keyboard events from the
     master devices associated with each seat, discarding events from
     slave devices.  However, multiplexing events from touch devices
     onto a master device poses problems: if both dependent and direct
     touch devices are attached to the same master pointer device, the
     coordinate space of touch events sent from that seat becomes
     ambiguous.  In addition, the X server does not send TouchEnd
     events to cancel ongoing touch sequences if the slave device that
     is their source is detached.  As a result of these ambiguities,
     touch events are processed from and recorded onto their slave
     devices instead.  */

  xi_device->device_id = device->deviceid;
  xi_device->grab = 0;
  xi_device->use = device->use;
  xi_device->name = build_string (device->name);
  xi_device->attachment = device->attachment;

  /* Clear the list of active touch points on the device, which are
     individual touches tracked by a touchscreen.  */

#ifdef HAVE_XINPUT2_2
  xi_device->touchpoints = NULL;
  xi_device->direct_p = false;
#endif /* HAVE_XINPUT2_1 */

#ifdef HAVE_XINPUT2_1
  if (!dpyinfo->xi2_version)
    {
      /* Skip everything below as there are no classes of interest on
	 XI 2.0 servers.  */
      xi_device->valuators = NULL;
      xi_device->scroll_valuator_count = 0;

      SAFE_FREE ();
      return;
    }

  actual_valuator_count = 0;
  xi_device->valuators = xnmalloc (device->num_classes,
				   sizeof *xi_device->valuators);
  values = NULL;

  /* Initialize device info based on a list of "device classes".
     Device classes are little pieces of information associated with a
     device.  Emacs is interested in scroll valuator information and
     touch handling information, which respectively describe the axes
     (if any) along which the device's scroll wheel rotates, and how
     the device reports touch input.  */

  for (c = 0; c < device->num_classes; ++c)
    {
      switch (device->classes[c]->type)
	{
	case XIScrollClass:
	  {
	    info = (XIScrollClassInfo *) device->classes[c];
	    xi_populate_scroll_valuator (xi_device, actual_valuator_count++,
					 info);
	    break;
	  }

	case XIValuatorClass:
	  {
	    valuator_info = (XIValuatorClassInfo *) device->classes[c];
	    tem = SAFE_ALLOCA (sizeof *tem);

	    /* Avoid restoring bogus values if some driver
	       accidentally specifies relative values in scroll
	       valuator classes how the input extension spec says they
	       should be, but allow restoring values when a value is
	       set, which is how the input extension actually
	       behaves.  */

	    if (valuator_info->value == 0.0
		&& valuator_info->mode != XIModeAbsolute)
	      continue;

	    tem->next = values;
	    tem->number = valuator_info->number;
	    tem->current_value = valuator_info->value;

	    values = tem;
	    break;
	  }

#ifdef HAVE_XINPUT2_2
	case XITouchClass:
	  {
	    touch_info = (XITouchClassInfo *) device->classes[c];

	    /* touch_info->mode indicates the coordinate space that
	       this device reports in its touch events.

	       DirectTouch means that the device uses a coordinate
	       space that corresponds to locations on the screen.  It
	       is set by touch screen devices which are overlaid
	       over the raster itself.

	       The other value (DependentTouch) means that the device
	       uses a separate abstract coordinate space corresponding
	       to its own surface.  Emacs ignores events from these
	       devices because it does not support recognizing touch
	       gestures from surfaces other than the screen.

	       Master devices may report multiple touch classes for
	       attached slave devices, leaving the nature of touch
	       events they send ambiguous.  The problem of
	       discriminating between these events is bypassed
	       entirely through only processing touch events from the
	       slave devices where they originate.  */

	    if (touch_info->mode == XIDirectTouch)
	      xi_device->direct_p = true;
	    else
	      xi_device->direct_p = false;
	  }
#endif /* HAVE_XINPUT2_2 */
	default:
	  break;
	}
    }

  xi_device->scroll_valuator_count = actual_valuator_count;

  /* Now look through all the valuators whose values are already known
     and populate our client-side records with their current
     values.  */

  for (tem = values; values; values = values->next)
    {
      for (c = 0; c < xi_device->scroll_valuator_count; ++c)
	{
	  if (xi_device->valuators[c].number == tem->number)
	    {
	      xi_device->valuators[c].invalid_p = false;
	      xi_device->valuators[c].current_value
		= tem->current_value;
	      xi_device->valuators[c].emacs_value = 0.0;
	    }
	}
    }

  SAFE_FREE ();
#endif /* HAVE_XINPUT2_1 */
}

/* Populate our client-side record of all devices, which includes
   basic information about the device and also touchscreen tracking
   information and scroll valuators.

   Keeping track of scroll valuators is required in order to support
   scroll wheels that report information in a fashion more detailed
   than a single turn of a "step" in the wheel.

   When the input extension is being utilized, the states of the mouse
   wheels on each axis are stored as absolute values inside
   "valuators" attached to each mouse device.  To obtain the delta of
   the scroll wheel from a motion event (which is used to report that
   some valuator has changed), it is necessary to iterate over every
   valuator that changed, and compare its previous value to the
   current value of the valuator.

   Each individual valuator also has an "interval", which is the
   amount you must divide that delta by in order to obtain a delta in
   the terms of scroll units.

   This delta however is still intermediate, to make driver
   implementations easier.  The XInput developers recommend (and most
   programs use) the following algorithm to convert from scroll unit
   deltas to pixel deltas by which the display must actually be
   scrolled:

     pixels_scrolled = pow (window_height, 2.0 / 3.0) * delta;  */

static void
x_cache_xi_devices (struct x_display_info *dpyinfo)
{
  int ndevices, actual_devices, i;
  XIDeviceInfo *infos;

  actual_devices = 0;
  block_input ();
  x_free_xi_devices (dpyinfo);
  infos = XIQueryDevice (dpyinfo->display,
			 XIAllDevices,
			 &ndevices);

  if (!ndevices)
    {
      XIFreeDeviceInfo (infos);
      unblock_input ();
      return;
    }

  dpyinfo->devices = xzalloc (sizeof *dpyinfo->devices * ndevices);

  for (i = 0; i < ndevices; ++i)
    {
      if (infos[i].enabled)
	xi_populate_device_from_info (dpyinfo,
				      &dpyinfo->devices[actual_devices++],
				      &infos[i]);
    }

  dpyinfo->num_devices = actual_devices;
  XIFreeDeviceInfo (infos);
  unblock_input ();
}

#ifdef HAVE_XINPUT2_1
/* Return the delta of the scroll valuator VALUATOR_NUMBER under
   DEVICE in the display DPYINFO with VALUE.  The valuator's valuator
   will be set to VALUE afterwards.  In case no scroll valuator is
   found, or if the valuator state is invalid (see the comment under
   XI_Enter in handle_one_xevent).  Otherwise, the valuator is
   returned in VALUATOR_RETURN.  */
static double
x_get_scroll_valuator_delta (struct x_display_info *dpyinfo,
			     struct xi_device_t *device,
			     int valuator_number, double value,
			     struct xi_scroll_valuator_t **valuator_return)
{
  struct xi_scroll_valuator_t *sv;
  double delta;
  int i;

  for (i = 0; i < device->scroll_valuator_count; ++i)
    {
      sv = &device->valuators[i];

      if (sv->number == valuator_number)
	{
	  *valuator_return = sv;

	  if (sv->increment == 0)
	    return DBL_MAX;

	  if (sv->invalid_p)
	    {
	      sv->current_value = value;
	      sv->invalid_p = false;

	      return DBL_MAX;
	    }
	  else
	    {
	      delta = (sv->current_value - value) / sv->increment;
	      sv->current_value = value;

	      return delta;
	    }
	}
    }

  *valuator_return = NULL;
  return DBL_MAX;
}

#endif

struct xi_device_t *
xi_device_from_id (struct x_display_info *dpyinfo, int deviceid)
{
  for (int i = 0; i < dpyinfo->num_devices; ++i)
    {
      if (dpyinfo->devices[i].device_id == deviceid)
	return &dpyinfo->devices[i];
    }

  return NULL;
}

#ifdef HAVE_XINPUT2_2

/* Record a touch sequence with the identifier DETAIL from the given
   FRAME on the specified DEVICE.  Round X and Y and record them as its
   current position, assign an identifier to the touch sequence suitable
   for reporting to Lisp, and return the same.  */

static EMACS_INT
xi_link_touch_point (struct xi_device_t *device,
		     int detail, double x, double y,
		     struct frame *frame)
{
  struct xi_touch_point_t *touchpoint;
  static EMACS_INT local_detail;

  /* Assign an identifier suitable for reporting to Lisp.  On builds
     with 64-bit Lisp_Object, this is largely a theoretical problem, but
     CARD32s easily overflow 32-bit systems, as they are not specific to
     X clients (e.g. Emacs) but grow uniformly across all of them.  */

  if (FIXNUM_OVERFLOW_P (local_detail))
    local_detail = 0;

  touchpoint = xmalloc (sizeof *touchpoint);
  touchpoint->next = device->touchpoints;
  touchpoint->x = lrint (x);
  touchpoint->y = lrint (y);
  touchpoint->number = detail;
  touchpoint->local_detail = local_detail++;
  touchpoint->frame = frame;
  touchpoint->ownership = TOUCH_OWNERSHIP_NONE;
  device->touchpoints = touchpoint;
  return touchpoint->local_detail;
}

/* Free and remove the touch sequence with the identifier DETAIL.
   DEVICE is the device in which the touch sequence should be
   recorded.  If such a touch sequence exists, return its local
   identifier in *LOCAL_DETAIL.

   Value is 0 if no touch sequence by that identifier exists inside
   DEVICE, 1 if a touch sequence has been found but is not owned by
   Emacs, and 2 otherwise.  */

static int
xi_unlink_touch_point (int detail, struct xi_device_t *device,
		       EMACS_INT *local_detail)
{
  struct xi_touch_point_t *last, *tem;
  enum xi_touch_ownership ownership;

  for (last = NULL, tem = device->touchpoints; tem;
       last = tem, tem = tem->next)
    {
      if (tem->number == detail)
	{
	  if (!last)
	    device->touchpoints = tem->next;
	  else
	    last->next = tem->next;

	  ownership = tem->ownership;
	  *local_detail = tem->local_detail;
	  xfree (tem);

	  if (ownership == TOUCH_OWNERSHIP_SELF)
	    return 2;

	  return 1;
	}
    }

  return 0;
}

/* Unlink all touch points associated with the frame F.
   This is done upon unmapping or destroying F's window, because
   touch point delivery after that point is undefined.  */

static void
xi_unlink_touch_points (struct frame *f)
{
  struct xi_device_t *device;
  struct xi_touch_point_t **next, *last;
  int i;

  for (i = 0; i < FRAME_DISPLAY_INFO (f)->num_devices; ++i)
    {
      device = &FRAME_DISPLAY_INFO (f)->devices[i];

      /* Now unlink all touch points on DEVICE matching F.  */

      for (next = &device->touchpoints; (last = *next);)
	{
	  if (last->frame == f)
	    {
	      *next = last->next;
	      xfree (last);
	    }
	  else
	    next = &last->next;
	}
    }
}

/* Return the data associated with a touch sequence DETAIL recorded by
   `xi_link_touch_point' from DEVICE, or NULL if it can't be
   found.  */

static struct xi_touch_point_t *
xi_find_touch_point (struct xi_device_t *device, int detail)
{
  struct xi_touch_point_t *point;

  for (point = device->touchpoints; point; point = point->next)
    {
      if (point->number == detail)
	return point;
    }

  return NULL;
}

#endif /* HAVE_XINPUT2_2 */

#ifdef HAVE_XINPUT2_1

static void
xi_reset_scroll_valuators_for_device_id (struct x_display_info *dpyinfo,
					 int id)
{
  struct xi_device_t *device;
  struct xi_scroll_valuator_t *valuator;
  int i;

  device = xi_device_from_id (dpyinfo, id);

  if (!device)
    return;

  if (!device->scroll_valuator_count)
    return;

  for (i = 0; i < device->scroll_valuator_count; ++i)
    {
      valuator = &device->valuators[i];
      valuator->invalid_p = true;
      valuator->emacs_value = 0.0;
    }

  return;
}

#endif /* HAVE_XINPUT2_1 */
#endif /* HAVE_XINPUT2 */



/* Cairo context, X rendering extension, and GC auxiliary data
   management functions.  */

#ifdef USE_CAIRO

void
x_cr_destroy_frame_context (struct frame *f)
{
  if (FRAME_CR_CONTEXT (f))
    {
      cairo_destroy (FRAME_CR_CONTEXT (f));
      FRAME_CR_CONTEXT (f) = NULL;
    }
}

void
x_cr_update_surface_desired_size (struct frame *f, int width, int height)
{
  if (FRAME_CR_SURFACE_DESIRED_WIDTH (f) != width
      || FRAME_CR_SURFACE_DESIRED_HEIGHT (f) != height)
    {
      x_cr_destroy_frame_context (f);
      FRAME_CR_SURFACE_DESIRED_WIDTH (f) = width;
      FRAME_CR_SURFACE_DESIRED_HEIGHT (f) = height;
    }
}

static void
x_cr_gc_clip (cairo_t *cr, struct frame *f, GC gc)
{
  if (gc)
    {
      struct x_gc_ext_data *gc_ext = x_gc_get_ext_data (f, gc, 0);

      if (gc_ext && gc_ext->n_clip_rects)
	{
	  for (int i = 0; i < gc_ext->n_clip_rects; i++)
	    cairo_rectangle (cr, gc_ext->clip_rects[i].x,
			     gc_ext->clip_rects[i].y,
			     gc_ext->clip_rects[i].width,
			     gc_ext->clip_rects[i].height);
	  cairo_clip (cr);
	}
    }
}

cairo_t *
x_begin_cr_clip (struct frame *f, GC gc)
{
  cairo_t *cr = FRAME_CR_CONTEXT (f);

  if (!cr)
    {
      int width = FRAME_CR_SURFACE_DESIRED_WIDTH (f);
      int height = FRAME_CR_SURFACE_DESIRED_HEIGHT (f);
      cairo_surface_t *surface;
#ifdef USE_CAIRO_XCB_SURFACE
      if (FRAME_DISPLAY_INFO (f)->xcb_visual)
	surface = cairo_xcb_surface_create (FRAME_DISPLAY_INFO (f)->xcb_connection,
					    (xcb_drawable_t) FRAME_X_RAW_DRAWABLE (f),
					    FRAME_DISPLAY_INFO (f)->xcb_visual,
					    width, height);
      else
#endif
	surface = cairo_xlib_surface_create (FRAME_X_DISPLAY (f),
					     FRAME_X_RAW_DRAWABLE (f),
					     FRAME_X_VISUAL (f),
					     width, height);

      cr = FRAME_CR_CONTEXT (f) = cairo_create (surface);
      cairo_surface_destroy (surface);
    }
  cairo_save (cr);
  x_cr_gc_clip (cr, f, gc);

  return cr;
}

void
x_end_cr_clip (struct frame *f)
{
  cairo_restore (FRAME_CR_CONTEXT (f));
#ifdef HAVE_XDBE
  if (FRAME_X_DOUBLE_BUFFERED_P (f))
    x_mark_frame_dirty (f);
#endif
}

void
x_set_cr_source_with_gc_foreground (struct frame *f, GC gc,
				    bool respect_alpha_background)
{
  XGCValues xgcv;
  XColor color;
  unsigned int depth;

  XGetGCValues (FRAME_X_DISPLAY (f), gc, GCForeground, &xgcv);
  color.pixel = xgcv.foreground;
  x_query_colors (f, &color, 1);
  depth = FRAME_DISPLAY_INFO (f)->n_planes;

  if (f->alpha_background < 1.0 && depth == 32
      && respect_alpha_background)
    {
      cairo_set_source_rgba (FRAME_CR_CONTEXT (f), color.red / 65535.0,
			     color.green / 65535.0, color.blue / 65535.0,
			     f->alpha_background);

      cairo_set_operator (FRAME_CR_CONTEXT (f), CAIRO_OPERATOR_SOURCE);
    }
  else
    {
      cairo_set_source_rgb (FRAME_CR_CONTEXT (f), color.red / 65535.0,
			    color.green / 65535.0, color.blue / 65535.0);
      cairo_set_operator (FRAME_CR_CONTEXT (f), CAIRO_OPERATOR_OVER);
    }
}

void
x_set_cr_source_with_gc_background (struct frame *f, GC gc,
				    bool respect_alpha_background)
{
  XGCValues xgcv;
  XColor color;
  unsigned int depth;

  XGetGCValues (FRAME_X_DISPLAY (f), gc, GCBackground, &xgcv);
  color.pixel = xgcv.background;

  x_query_colors (f, &color, 1);

  depth = FRAME_DISPLAY_INFO (f)->n_planes;

  if (f->alpha_background < 1.0 && depth == 32
      && respect_alpha_background)
    {
      cairo_set_source_rgba (FRAME_CR_CONTEXT (f), color.red / 65535.0,
			     color.green / 65535.0, color.blue / 65535.0,
			     f->alpha_background);

      cairo_set_operator (FRAME_CR_CONTEXT (f), CAIRO_OPERATOR_SOURCE);
    }
  else
    {
      cairo_set_source_rgb (FRAME_CR_CONTEXT (f), color.red / 65535.0,
			    color.green / 65535.0, color.blue / 65535.0);
      cairo_set_operator (FRAME_CR_CONTEXT (f), CAIRO_OPERATOR_OVER);
    }
}

static const cairo_user_data_key_t xlib_surface_key, saved_drawable_key;

static void
x_cr_destroy_xlib_surface (cairo_surface_t *xlib_surface)
{
  if (xlib_surface)
    {
      XFreePixmap (cairo_xlib_surface_get_display (xlib_surface),
		   cairo_xlib_surface_get_drawable (xlib_surface));
      cairo_surface_destroy (xlib_surface);
    }
}

static bool
x_try_cr_xlib_drawable (struct frame *f, GC gc)
{
  cairo_t *cr = FRAME_CR_CONTEXT (f);
  if (!cr)
    return true;

  cairo_surface_t *surface = cairo_get_target (cr);
  switch (cairo_surface_get_type (surface))
    {
    case CAIRO_SURFACE_TYPE_XLIB:
#ifdef USE_CAIRO_XCB_SURFACE
    case CAIRO_SURFACE_TYPE_XCB:
#endif
      cairo_surface_flush (surface);
      return true;

    case CAIRO_SURFACE_TYPE_IMAGE:
      break;

    default:
      return false;
    }

  /* FRAME_CR_CONTEXT (f) is an image surface we can not draw into
     directly with Xlib.  Set up a Pixmap so we can copy back the
     result later in x_end_cr_xlib_drawable.  */
  cairo_surface_t *xlib_surface = cairo_get_user_data (cr, &xlib_surface_key);
  int width = FRAME_CR_SURFACE_DESIRED_WIDTH (f);
  int height = FRAME_CR_SURFACE_DESIRED_HEIGHT (f);
  Pixmap pixmap;
  if (xlib_surface
      && cairo_xlib_surface_get_width (xlib_surface) == width
      && cairo_xlib_surface_get_height (xlib_surface) == height)
    pixmap = cairo_xlib_surface_get_drawable (xlib_surface);
  else
    {
      pixmap = XCreatePixmap (FRAME_X_DISPLAY (f), FRAME_X_RAW_DRAWABLE (f),
			      width, height,
			      DefaultDepthOfScreen (FRAME_X_SCREEN (f)));
      xlib_surface = cairo_xlib_surface_create (FRAME_X_DISPLAY (f),
						pixmap, FRAME_X_VISUAL (f),
						width, height);
      cairo_set_user_data (cr, &xlib_surface_key, xlib_surface,
			   (cairo_destroy_func_t) x_cr_destroy_xlib_surface);
    }

  cairo_t *buf = cairo_create (xlib_surface);
  cairo_set_source_surface (buf, surface, 0, 0);
  cairo_matrix_t matrix;
  cairo_get_matrix (cr, &matrix);
  cairo_pattern_set_matrix (cairo_get_source (cr), &matrix);
  cairo_set_operator (buf, CAIRO_OPERATOR_SOURCE);
  x_cr_gc_clip (buf, f, gc);
  cairo_paint (buf);
  cairo_destroy (buf);

  cairo_set_user_data (cr, &saved_drawable_key,
		       (void *) (uintptr_t) FRAME_X_RAW_DRAWABLE (f), NULL);
  FRAME_X_RAW_DRAWABLE (f) = pixmap;
  cairo_surface_flush (xlib_surface);

  return true;
}

static void
x_end_cr_xlib_drawable (struct frame *f, GC gc)
{
  cairo_t *cr = FRAME_CR_CONTEXT (f);
  if (!cr)
    return;

  Drawable saved_drawable
    = (uintptr_t) cairo_get_user_data (cr, &saved_drawable_key);
  cairo_surface_t *surface = (saved_drawable
			      ? cairo_get_user_data (cr, &xlib_surface_key)
			      : cairo_get_target (cr));
  struct x_gc_ext_data *gc_ext = x_gc_get_ext_data (f, gc, 0);
  if (gc_ext && gc_ext->n_clip_rects)
    for (int i = 0; i < gc_ext->n_clip_rects; i++)
      cairo_surface_mark_dirty_rectangle (surface, gc_ext->clip_rects[i].x,
					  gc_ext->clip_rects[i].y,
					  gc_ext->clip_rects[i].width,
					  gc_ext->clip_rects[i].height);
  else
    cairo_surface_mark_dirty (surface);
  if (!saved_drawable)
    return;

  cairo_save (cr);
  cairo_set_source_surface (cr, surface, 0, 0);
  cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
  x_cr_gc_clip (cr, f, gc);
  cairo_paint (cr);
  cairo_restore (cr);

  FRAME_X_RAW_DRAWABLE (f) = saved_drawable;
  cairo_set_user_data (cr, &saved_drawable_key, NULL, NULL);
}

/* Fringe bitmaps.  */

static int max_fringe_bmp = 0;
static cairo_pattern_t **fringe_bmp = 0;

static void
x_cr_define_fringe_bitmap (int which, unsigned short *bits, int h, int wd)
{
  int i, stride;
  cairo_surface_t *surface;
  cairo_pattern_t *pattern;
  unsigned char *data;

  if (which >= max_fringe_bmp)
    {
      i = max_fringe_bmp;
      max_fringe_bmp = which + 20;
      fringe_bmp = xrealloc (fringe_bmp, max_fringe_bmp * sizeof (*fringe_bmp));
      while (i < max_fringe_bmp)
	fringe_bmp[i++] = 0;
    }

  block_input ();

  surface = cairo_image_surface_create (CAIRO_FORMAT_A1, wd, h);
  stride = cairo_image_surface_get_stride (surface);
  data = cairo_image_surface_get_data (surface);

  for (i = 0; i < h; i++)
    {
      *((unsigned short *) data) = bits[i];
      data += stride;
    }

  cairo_surface_mark_dirty (surface);
  pattern = cairo_pattern_create_for_surface (surface);
  cairo_surface_destroy (surface);

  unblock_input ();

  fringe_bmp[which] = pattern;
}

static void
x_cr_destroy_fringe_bitmap (int which)
{
  if (which >= max_fringe_bmp)
    return;

  if (fringe_bmp[which])
    {
      block_input ();
      cairo_pattern_destroy (fringe_bmp[which]);
      unblock_input ();
    }
  fringe_bmp[which] = 0;
}

static void
x_cr_draw_image (struct frame *f, GC gc, cairo_pattern_t *image,
		 int src_x, int src_y, int width, int height,
		 int dest_x, int dest_y, bool overlay_p)
{
  cairo_t *cr = x_begin_cr_clip (f, gc);

  if (overlay_p)
    cairo_rectangle (cr, dest_x, dest_y, width, height);
  else
    {
      x_set_cr_source_with_gc_background (f, gc, false);
      cairo_rectangle (cr, dest_x, dest_y, width, height);
      cairo_fill_preserve (cr);
    }

  cairo_translate (cr, dest_x - src_x, dest_y - src_y);

  cairo_surface_t *surface;
  cairo_pattern_get_surface (image, &surface);
  cairo_format_t format = cairo_image_surface_get_format (surface);
  if (format != CAIRO_FORMAT_A8 && format != CAIRO_FORMAT_A1)
    {
      cairo_set_source (cr, image);
      cairo_fill (cr);
    }
  else
    {
      x_set_cr_source_with_gc_foreground (f, gc, false);
      cairo_clip (cr);
      cairo_mask (cr, image);
    }

  x_end_cr_clip (f);
}

void
x_cr_draw_frame (cairo_t *cr, struct frame *f)
{
  int width, height;

  width = FRAME_PIXEL_WIDTH (f);
  height = FRAME_PIXEL_HEIGHT (f);

  cairo_t *saved_cr = FRAME_CR_CONTEXT (f);
  FRAME_CR_CONTEXT (f) = cr;
  x_clear_area (f, 0, 0, width, height);
  expose_frame (f, 0, 0, width, height);
  FRAME_CR_CONTEXT (f) = saved_cr;
}

static cairo_status_t
x_cr_accumulate_data (void *closure, const unsigned char *data,
		      unsigned int length)
{
  Lisp_Object *acc = (Lisp_Object *) closure;

  *acc = Fcons (make_unibyte_string ((char const *) data, length), *acc);

  return CAIRO_STATUS_SUCCESS;
}

static void
x_cr_destroy (void *cr)
{
  block_input ();
  cairo_destroy (cr);
  unblock_input ();
}

Lisp_Object
x_cr_export_frames (Lisp_Object frames, cairo_surface_type_t surface_type)
{
  struct frame *f;
  cairo_surface_t *surface;
  cairo_t *cr;
  int width, height;
  void (*surface_set_size_func) (cairo_surface_t *, double, double) = NULL;
  Lisp_Object acc = Qnil;
  specpdl_ref count = SPECPDL_INDEX ();

  redisplay_preserve_echo_area (31);

  f = XFRAME (XCAR (frames));
  frames = XCDR (frames);
  width = FRAME_PIXEL_WIDTH (f);
  height = FRAME_PIXEL_HEIGHT (f);

  block_input ();
#ifdef CAIRO_HAS_PDF_SURFACE
  if (surface_type == CAIRO_SURFACE_TYPE_PDF)
    {
      surface = cairo_pdf_surface_create_for_stream (x_cr_accumulate_data, &acc,
						     width, height);
      surface_set_size_func = cairo_pdf_surface_set_size;
    }
  else
#endif
#ifdef CAIRO_HAS_PNG_FUNCTIONS
  if (surface_type == CAIRO_SURFACE_TYPE_IMAGE)
    surface = cairo_image_surface_create (CAIRO_FORMAT_RGB24, width, height);
  else
#endif
#ifdef CAIRO_HAS_PS_SURFACE
  if (surface_type == CAIRO_SURFACE_TYPE_PS)
    {
      surface = cairo_ps_surface_create_for_stream (x_cr_accumulate_data, &acc,
						    width, height);
      surface_set_size_func = cairo_ps_surface_set_size;
    }
  else
#endif
#ifdef CAIRO_HAS_SVG_SURFACE
  if (surface_type == CAIRO_SURFACE_TYPE_SVG)
    surface = cairo_svg_surface_create_for_stream (x_cr_accumulate_data, &acc,
						   width, height);
  else
#endif
    abort ();

  cr = cairo_create (surface);
  cairo_surface_destroy (surface);
  record_unwind_protect_ptr (x_cr_destroy, cr);

  while (1)
    {
      cairo_t *saved_cr = FRAME_CR_CONTEXT (f);
      FRAME_CR_CONTEXT (f) = cr;
      x_clear_area (f, 0, 0, width, height);
      expose_frame (f, 0, 0, width, height);
      FRAME_CR_CONTEXT (f) = saved_cr;

      if (NILP (frames))
	break;

      cairo_surface_show_page (surface);
      f = XFRAME (XCAR (frames));
      frames = XCDR (frames);
      width = FRAME_PIXEL_WIDTH (f);
      height = FRAME_PIXEL_HEIGHT (f);
      if (surface_set_size_func)
	(*surface_set_size_func) (surface, width, height);

      unblock_input ();
      maybe_quit ();
      block_input ();
    }

#ifdef CAIRO_HAS_PNG_FUNCTIONS
  if (surface_type == CAIRO_SURFACE_TYPE_IMAGE)
    {
      cairo_surface_flush (surface);
      cairo_surface_write_to_png_stream (surface, x_cr_accumulate_data, &acc);
    }
#endif
  unblock_input ();

  unbind_to (count, Qnil);

  return CALLN (Fapply, Qconcat, Fnreverse (acc));
}

#endif	/* USE_CAIRO */

#if defined HAVE_XRENDER

void
x_xr_apply_ext_clip (struct frame *f, GC gc)
{
  eassert (FRAME_X_PICTURE (f) != None);

  struct x_gc_ext_data *data = x_gc_get_ext_data (f, gc, 1);

  if (data->n_clip_rects)
    XRenderSetPictureClipRectangles (FRAME_X_DISPLAY (f),
				     FRAME_X_PICTURE (f),
				     0, 0, data->clip_rects,
				     data->n_clip_rects);
}

void
x_xr_reset_ext_clip (struct frame *f)
{
  XRenderPictureAttributes attrs = { .clip_mask = None };

  XRenderChangePicture (FRAME_X_DISPLAY (f),
			FRAME_X_PICTURE (f),
			CPClipMask, &attrs);
}

#endif /* HAVE_XRENDER */

static void
x_set_clip_rectangles (struct frame *f, GC gc, XRectangle *rectangles, int n)
{
  XSetClipRectangles (FRAME_X_DISPLAY (f), gc, 0, 0, rectangles, n, Unsorted);
#if defined USE_CAIRO || defined HAVE_XRENDER
  eassert (n >= 0 && n <= MAX_CLIP_RECTS);

  {
    struct x_gc_ext_data *gc_ext = x_gc_get_ext_data (f, gc, 1);

    gc_ext->n_clip_rects = n;
    memcpy (gc_ext->clip_rects, rectangles, sizeof (XRectangle) * n);
  }
#endif
}

static void
x_reset_clip_rectangles (struct frame *f, GC gc)
{
  XSetClipMask (FRAME_X_DISPLAY (f), gc, None);
#if defined USE_CAIRO || defined HAVE_XRENDER
  {
    struct x_gc_ext_data *gc_ext = x_gc_get_ext_data (f, gc, 0);

    if (gc_ext)
      gc_ext->n_clip_rects = 0;
  }
#endif
}

#ifdef HAVE_XRENDER
# if !defined USE_CAIRO && (RENDER_MAJOR > 0 || RENDER_MINOR >= 2)
static void
x_xrender_color_from_gc_foreground (struct frame *f, GC gc, XRenderColor *color,
				    bool apply_alpha_background)
{
  XGCValues xgcv;
  XColor xc;

  XGetGCValues (FRAME_X_DISPLAY (f), gc, GCForeground, &xgcv);
  xc.pixel = xgcv.foreground;
  x_query_colors (f, &xc, 1);

  color->alpha = (apply_alpha_background
		  ? 65535 * f->alpha_background
		  : 65535);

  if (color->alpha == 65535)
    {
      color->red = xc.red;
      color->blue = xc.blue;
      color->green = xc.green;
    }
  else
    {
      color->red = (xc.red * color->alpha) / 65535;
      color->blue = (xc.blue * color->alpha) / 65535;
      color->green = (xc.green * color->alpha) / 65535;
    }
}
# endif

void
x_xrender_color_from_gc_background (struct frame *f, GC gc, XRenderColor *color,
				    bool apply_alpha_background)
{
  XGCValues xgcv;
  XColor xc;

  XGetGCValues (FRAME_X_DISPLAY (f), gc, GCBackground, &xgcv);
  xc.pixel = xgcv.background;
  x_query_colors (f, &xc, 1);

  color->alpha = (apply_alpha_background
		  ? 65535 * f->alpha_background
		  : 65535);

  if (color->alpha == 65535)
    {
      color->red = xc.red;
      color->blue = xc.blue;
      color->green = xc.green;
    }
  else
    {
      color->red = (xc.red * color->alpha) / 65535;
      color->blue = (xc.blue * color->alpha) / 65535;
      color->green = (xc.green * color->alpha) / 65535;
    }
}
#endif

static void
x_fill_rectangle (struct frame *f, GC gc, int x, int y, int width, int height,
		  bool respect_alpha_background)
{
#ifdef USE_CAIRO
  Display *dpy = FRAME_X_DISPLAY (f);
  cairo_t *cr;
  XGCValues xgcv;

  cr = x_begin_cr_clip (f, gc);
  XGetGCValues (dpy, gc, GCFillStyle | GCStipple, &xgcv);
  if (xgcv.fill_style == FillSolid
      /* Invalid resource ID (one or more of the three most
	 significant bits set to 1) is obtained if the GCStipple
	 component has never been explicitly set.  It should be
	 regarded as Pixmap of unspecified size filled with ones.  */
      || (xgcv.stipple & ((Pixmap) 7 << (sizeof (Pixmap) * CHAR_BIT - 3))))
    {
      x_set_cr_source_with_gc_foreground (f, gc, respect_alpha_background);
      cairo_rectangle (cr, x, y, width, height);
      cairo_fill (cr);
    }
  else
    {
      eassert (xgcv.fill_style == FillOpaqueStippled);
      eassert (xgcv.stipple != None);
      x_set_cr_source_with_gc_background (f, gc, respect_alpha_background);
      cairo_rectangle (cr, x, y, width, height);
      cairo_fill_preserve (cr);

      cairo_pattern_t *pattern = x_bitmap_stipple (f, xgcv.stipple);
      if (pattern)
	{
	  x_set_cr_source_with_gc_foreground (f, gc, respect_alpha_background);
	  cairo_clip (cr);
	  cairo_mask (cr, pattern);
	}
    }
  x_end_cr_clip (f);
#else
#if defined HAVE_XRENDER && (RENDER_MAJOR > 0 || (RENDER_MINOR >= 2))
  if (respect_alpha_background
      && f->alpha_background != 1.0
      && FRAME_DISPLAY_INFO (f)->alpha_bits
      && FRAME_CHECK_XR_VERSION (f, 0, 2))
    {
      x_xr_ensure_picture (f);

      if (FRAME_X_PICTURE (f) != None)
	{
	  XRenderColor xc;

#if RENDER_MAJOR > 0 || (RENDER_MINOR >= 10)
	  XGCValues xgcv;
	  XRenderPictureAttributes attrs;
	  XRenderColor alpha;
	  Picture stipple, fill;
#endif

	  x_xr_apply_ext_clip (f, gc);

#if RENDER_MAJOR > 0 || (RENDER_MINOR >= 10)
	  XGetGCValues (FRAME_X_DISPLAY (f),
			gc, GCFillStyle | GCStipple, &xgcv);

	  if (xgcv.fill_style == FillOpaqueStippled
	      && FRAME_CHECK_XR_VERSION (f, 0, 10))
	    {
	      x_xrender_color_from_gc_background (f, gc, &alpha, true);
	      x_xrender_color_from_gc_foreground (f, gc, &xc, true);
	      attrs.repeat = RepeatNormal;

	      stipple = XRenderCreatePicture (FRAME_X_DISPLAY (f),
					      xgcv.stipple,
					      XRenderFindStandardFormat (FRAME_X_DISPLAY (f),
									 PictStandardA1),
					      CPRepeat, &attrs);

	      XRenderFillRectangle (FRAME_X_DISPLAY (f), PictOpSrc,
				    FRAME_X_PICTURE (f),
				    &alpha, x, y, width, height);

	      fill = XRenderCreateSolidFill (FRAME_X_DISPLAY (f), &xc);

	      XRenderComposite (FRAME_X_DISPLAY (f), PictOpOver, fill, stipple,
				FRAME_X_PICTURE (f), 0, 0, x, y, x, y, width, height);

	      XRenderFreePicture (FRAME_X_DISPLAY (f), stipple);
	      XRenderFreePicture (FRAME_X_DISPLAY (f), fill);
	    }
	  else
#endif
	    {
	      x_xrender_color_from_gc_foreground (f, gc, &xc, true);
	      XRenderFillRectangle (FRAME_X_DISPLAY (f),
				    PictOpSrc, FRAME_X_PICTURE (f),
				    &xc, x, y, width, height);
	    }
	  x_xr_reset_ext_clip (f);
	  x_mark_frame_dirty (f);

	  return;
	}
    }
#endif
  XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
		  gc, x, y, width, height);
#endif
}



/* Graphics primitives.  */

static void
x_clear_rectangle (struct frame *f, GC gc, int x, int y, int width, int height,
		   bool respect_alpha_background)
{
#ifdef USE_CAIRO
  cairo_t *cr;

  cr = x_begin_cr_clip (f, gc);
  x_set_cr_source_with_gc_background (f, gc, respect_alpha_background);
  cairo_rectangle (cr, x, y, width, height);
  cairo_fill (cr);
  x_end_cr_clip (f);
#else
#if defined HAVE_XRENDER && (RENDER_MAJOR > 0 || (RENDER_MINOR >= 2))
  if (respect_alpha_background
      && f->alpha_background != 1.0
      && FRAME_DISPLAY_INFO (f)->alpha_bits
      && FRAME_CHECK_XR_VERSION (f, 0, 2))
    {
      x_xr_ensure_picture (f);

      if (FRAME_X_PICTURE (f) != None)
	{
	  XRenderColor xc;

	  x_xr_apply_ext_clip (f, gc);
	  x_xrender_color_from_gc_background (f, gc, &xc, true);
	  XRenderFillRectangle (FRAME_X_DISPLAY (f),
				PictOpSrc, FRAME_X_PICTURE (f),
				&xc, x, y, width, height);
	  x_xr_reset_ext_clip (f);
	  x_mark_frame_dirty (f);

	  return;
	}
    }
#endif

  XGCValues xgcv;
  Display *dpy = FRAME_X_DISPLAY (f);
  XGetGCValues (dpy, gc, GCBackground | GCForeground, &xgcv);
  XSetForeground (dpy, gc, xgcv.background);
  XFillRectangle (dpy, FRAME_X_DRAWABLE (f),
		  gc, x, y, width, height);
  XSetForeground (dpy, gc, xgcv.foreground);
#endif
}

static void
x_draw_rectangle (struct frame *f, GC gc, int x, int y, int width, int height)
{
#ifdef USE_CAIRO
  cairo_t *cr;

  cr = x_begin_cr_clip (f, gc);
  x_set_cr_source_with_gc_foreground (f, gc, false);
  cairo_rectangle (cr, x + 0.5, y + 0.5, width, height);
  cairo_set_line_width (cr, 1);
  cairo_stroke (cr);
  x_end_cr_clip (f);
#else
  XDrawRectangle (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
		  gc, x, y, width, height);
#endif
}

static void
x_clear_window (struct frame *f)
{
#ifdef USE_CAIRO
  cairo_t *cr;

  cr = x_begin_cr_clip (f, NULL);
  x_set_cr_source_with_gc_background (f, f->output_data.x->normal_gc, true);
  cairo_paint (cr);
  x_end_cr_clip (f);
#else
#ifndef USE_GTK
  if (f->alpha_background != 1.0
#ifdef HAVE_XDBE
      || FRAME_X_DOUBLE_BUFFERED_P (f)
#endif
      )
#endif
    x_clear_area (f, 0, 0, FRAME_PIXEL_WIDTH (f),
		  FRAME_PIXEL_HEIGHT (f));
#ifndef USE_GTK
  else
    XClearWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif
#endif
}

#ifdef USE_CAIRO
static void
x_fill_trapezoid_for_relief (struct frame *f, GC gc, int x, int y,
			     int width, int height, int top_p)
{
  cairo_t *cr;

  cr = x_begin_cr_clip (f, gc);
  x_set_cr_source_with_gc_foreground (f, gc, false);
  cairo_move_to (cr, top_p ? x : x + height, y);
  cairo_line_to (cr, x, y + height);
  cairo_line_to (cr, top_p ? x + width - height : x + width, y + height);
  cairo_line_to (cr, x + width, y);
  cairo_fill (cr);
  x_end_cr_clip (f);
}

enum corners
  {
    CORNER_BOTTOM_RIGHT,	/* 0 -> pi/2 */
    CORNER_BOTTOM_LEFT,		/* pi/2 -> pi */
    CORNER_TOP_LEFT,		/* pi -> 3pi/2 */
    CORNER_TOP_RIGHT,		/* 3pi/2 -> 2pi */
    CORNER_LAST
  };

static void
x_erase_corners_for_relief (struct frame *f, GC gc, int x, int y,
			    int width, int height,
			    double radius, double margin, int corners)
{
  cairo_t *cr;
  int i;

  cr = x_begin_cr_clip (f, gc);
  x_set_cr_source_with_gc_background (f, gc, false);
  for (i = 0; i < CORNER_LAST; i++)
    if (corners & (1 << i))
      {
	double xm, ym, xc, yc;

	if (i == CORNER_TOP_LEFT || i == CORNER_BOTTOM_LEFT)
	  xm = x - margin, xc = xm + radius;
	else
	  xm = x + width + margin, xc = xm - radius;
	if (i == CORNER_TOP_LEFT || i == CORNER_TOP_RIGHT)
	  ym = y - margin, yc = ym + radius;
	else
	  ym = y + height + margin, yc = ym - radius;

	cairo_move_to (cr, xm, ym);
	cairo_arc (cr, xc, yc, radius, i * M_PI_2, (i + 1) * M_PI_2);
      }
  cairo_clip (cr);
  cairo_rectangle (cr, x, y, width, height);
  cairo_fill (cr);
  x_end_cr_clip (f);
}

static void
x_draw_horizontal_wave (struct frame *f, GC gc, int x, int y,
			int width, int height, int wave_length)
{
  cairo_t *cr;
  double dx = wave_length, dy = height - 1;
  int xoffset, n;

  cr = x_begin_cr_clip (f, gc);
  x_set_cr_source_with_gc_foreground (f, gc, false);
  cairo_rectangle (cr, x, y, width, height);
  cairo_clip (cr);

  if (x >= 0)
    {
      xoffset = x % (wave_length * 2);
      if (xoffset == 0)
	xoffset = wave_length * 2;
    }
  else
    xoffset = x % (wave_length * 2) + wave_length * 2;
  n = (width + xoffset) / wave_length + 1;
  if (xoffset > wave_length)
    {
      xoffset -= wave_length;
      --n;
      y += height - 1;
      dy = -dy;
    }

  cairo_move_to (cr, x - xoffset + 0.5, y + 0.5);
  while (--n >= 0)
    {
      cairo_rel_line_to (cr, dx, dy);
      dy = -dy;
    }
  cairo_set_line_width (cr, 1);
  cairo_stroke (cr);
  x_end_cr_clip (f);
}
#endif


/* Return the struct x_display_info corresponding to DPY,
   when it is guaranteed that one will correspond.  */

struct x_display_info *
x_dpyinfo (Display *dpy)
{
  for (struct x_display_info *dpyinfo = x_display_list; ;
       dpyinfo = dpyinfo->next)
    if (dpyinfo->display == dpy)
      return dpyinfo;
}

/* Return the struct x_display_info corresponding to DPY,
   or a null pointer if none corresponds.  */

struct x_display_info *
x_display_info_for_display (Display *dpy)
{
  struct x_display_info *dpyinfo;

  for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    if (dpyinfo->display == dpy)
      return dpyinfo;

  return 0;
}

static Window
x_find_topmost_parent (struct frame *f)
{
  struct x_output *x = f->output_data.x;
  Window win = None, wi = x->parent_desc;
  Display *dpy = FRAME_X_DISPLAY (f);

  while (wi != FRAME_DISPLAY_INFO (f)->root_window)
    {
      Window root;
      Window *children;
      unsigned int nchildren;

      win = wi;
      if (XQueryTree (dpy, win, &root, &wi, &children, &nchildren))
	XFree (children);
      else
	break;
    }

  return win;
}

#define OPAQUE  0xffffffff

static void
x_set_frame_alpha (struct frame *f)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Display *dpy = FRAME_X_DISPLAY (f);
  Window win = FRAME_OUTER_WINDOW (f);
  double alpha = 1.0;
  double alpha_min = 1.0;
  unsigned long opac;
  Window parent;

  if (dpyinfo->highlight_frame == f)
    alpha = f->alpha[0];
  else
    alpha = f->alpha[1];

  if (alpha < 0.0)
    return;

  if (FLOATP (Vframe_alpha_lower_limit))
    alpha_min = XFLOAT_DATA (Vframe_alpha_lower_limit);
  else if (FIXNUMP (Vframe_alpha_lower_limit))
    alpha_min = (XFIXNUM (Vframe_alpha_lower_limit)) / 100.0;

  if (alpha > 1.0)
    alpha = 1.0;
  else if (alpha < alpha_min && alpha_min <= 1.0)
    alpha = alpha_min;

  opac = alpha * OPAQUE;

  /* If there is a parent from the window manager, put the property there
     also, to work around broken window managers that fail to do that.
     Do this unconditionally as this function is called on reparent when
     alpha has not changed on the frame.  */

  x_ignore_errors_for_next_request (dpyinfo, 0);

  if (!FRAME_PARENT_FRAME (f))
    {
      parent = x_find_topmost_parent (f);

      if (parent != None)
	{
	  XChangeProperty (dpy, parent,
			   dpyinfo->Xatom_net_wm_window_opacity,
			   XA_CARDINAL, 32, PropModeReplace,
			   (unsigned char *) &opac, 1);
	}
    }

  XChangeProperty (dpy, win, dpyinfo->Xatom_net_wm_window_opacity,
		   XA_CARDINAL, 32, PropModeReplace,
		   (unsigned char *) &opac, 1);
  x_stop_ignoring_errors (dpyinfo);
}



/***********************************************************************
		    Starting and ending an update
 ***********************************************************************/

#if defined HAVE_XSYNC && !defined USE_GTK && defined HAVE_CLOCK_GETTIME

/* Wait for an event matching PREDICATE to show up in the event
   queue, or TIMEOUT to elapse.

   If TIMEOUT passes without an event being found, return 1.
   Otherwise, return 0 and behave as XIfEvent would.  */

static int
x_if_event (Display *dpy, XEvent *event_return,
	    Bool (*predicate) (Display *, XEvent *, XPointer),
	    XPointer arg, struct timespec timeout)
{
  struct timespec current_time, target;
  int fd;
  fd_set fds;

  fd = ConnectionNumber (dpy);
  current_time = current_timespec ();
  target = timespec_add (current_time, timeout);

  /* Check if an event is already in the queue.  If it is, avoid
     syncing.  */
  if (XCheckIfEvent (dpy, event_return, predicate, arg))
    return 0;

  while (true)
    {
      /* Get events into the queue.  */
      XSync (dpy, False);

      /* Look for an event again.  */
      if (XCheckIfEvent (dpy, event_return, predicate, arg))
	return 0;

      /* Calculate the timeout.  */
      current_time = current_timespec ();
      timeout = timespec_sub (target, current_time);

      /* If not, wait for some input to show up on the X connection,
	 or for the timeout to elapse.  */
      FD_ZERO (&fds);
      FD_SET (fd, &fds);

      /* If this fails due to an IO error, XSync will call the IO
	 error handler.  */
      pselect (fd + 1, &fds, NULL, NULL, &timeout, NULL);

      /* Timeout elapsed.  */
      current_time = current_timespec ();
      if (timespec_cmp (target, current_time) < 0)
	return 1;
    }
}

/* Return the monotonic time corresponding to the high-resolution
   server timestamp TIMESTAMP.  Return 0 if the necessary information
   is not available.  */

static uint_fast64_t
x_sync_get_monotonic_time (struct x_display_info *dpyinfo,
			   uint_fast64_t timestamp)
{
  if (dpyinfo->server_time_monotonic_p)
    return timestamp;

  /* This means we haven't yet initialized the server time offset.  */
  if (!dpyinfo->server_time_offset)
    return 0;

  uint_fast64_t t;
  return ckd_sub (&t, timestamp, dpyinfo->server_time_offset) ? 0 : t;
}

/* Return the current monotonic time in the same format as a
   high-resolution server timestamp, or 0 if not available.  */

static uint_fast64_t
x_sync_current_monotonic_time (void)
{
  struct timespec time = monotonic_coarse_timespec ();
  uint_fast64_t t;
  return ((ckd_mul (&t, time.tv_sec, 1000000)
	   || ckd_add (&t, t, time.tv_nsec / 1000))
	  ? 0 : t);
}

/* Decode a _NET_WM_FRAME_DRAWN message and calculate the time it took
   to draw the last frame.  */

static void
x_sync_note_frame_times (struct x_display_info *dpyinfo,
			 struct frame *f, XEvent *event)
{
  uint_fast64_t low, high, time;
  struct x_output *output;

  low = event->xclient.data.l[2];
  high = event->xclient.data.l[3];
  output = FRAME_X_OUTPUT (f);

  time = x_sync_get_monotonic_time (dpyinfo, low | (high << 32));

  if (!time || !output->temp_frame_time
      || ckd_sub (&output->last_frame_time, time, output->temp_frame_time))
    output->last_frame_time = 0;

#ifdef FRAME_DEBUG
  uint_fast64_t last_frame_ms = output->last_frame_time / 1000;
  fprintf (stderr,
	   "Drawing the last frame took: %"PRIuFAST64" ms (%"PRIuFAST64")\n",
	   last_frame_ms, time);
#endif
}

static Bool
x_sync_is_frame_drawn_event (Display *dpy, XEvent *event,
			     XPointer user_data)
{
  struct frame *f;
  struct x_display_info *dpyinfo;

  f = (struct frame *) user_data;
  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (event->type == ClientMessage
      && (event->xclient.message_type
	  == dpyinfo->Xatom_net_wm_frame_drawn)
      && event->xclient.window == FRAME_OUTER_WINDOW (f))
    return True;

  return False;
}

/* Wait for the compositing manager to finish drawing the last frame.
   If the compositing manager has already drawn everything, do
   nothing.  */

static void
x_sync_wait_for_frame_drawn_event (struct frame *f)
{
  XEvent event;
  struct x_display_info *dpyinfo;

  if (!FRAME_X_WAITING_FOR_DRAW (f)
      /* The compositing manager can't draw a frame if it is
	 unmapped.  */
      || !FRAME_VISIBLE_P (f))
    return;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  /* Wait for the frame drawn message to arrive.  */
  if (x_if_event (FRAME_X_DISPLAY (f), &event,
		  x_sync_is_frame_drawn_event, (XPointer) f,
		  make_timespec (1, 0)))
    {
      /* The first time a draw hangs, treat it as a random fluctuation
	 on the part of the compositor.  If the next draw continues to
	 hang, disable frame synchronization.  */
      if (FRAME_X_DRAW_JUST_HUNG (f))
	{
	  fprintf (stderr, "Warning: compositing manager spent more than 1 "
		   "second drawing a frame.  Frame synchronization has "
		   "been disabled\n");
	  FRAME_X_OUTPUT (f)->use_vsync_p = false;

	  /* Remove the compositor bypass property from the outer
	     window.  */
	  XDeleteProperty (dpyinfo->display, FRAME_OUTER_WINDOW (f),
			   dpyinfo->Xatom_net_wm_bypass_compositor);

	  /* Also change the frame parameter to reflect the new
	     state.  */
	  store_frame_param (f, Quse_frame_synchronization, Qnil);
	}
      else
	{
	  fprintf (stderr, "Warning: compositing manager spent more than 1 "
		   "second drawing a frame.  Frame synchronization will be "
		   "disabled if this happens again\n");
	  FRAME_X_DRAW_JUST_HUNG (f) = true;
	}
    }
  else
    x_sync_note_frame_times (dpyinfo, f, &event);

  FRAME_X_WAITING_FOR_DRAW (f) = false;
}

/* Tell the compositing manager to postpone updates of F until a frame
   has finished drawing.  */

static void
x_sync_update_begin (struct frame *f)
{
  XSyncValue value, add;
  Bool overflow;

  if (FRAME_X_EXTENDED_COUNTER (f) == None)
    return;

  value = FRAME_X_COUNTER_VALUE (f);

  if (FRAME_X_OUTPUT (f)->ext_sync_end_pending_p)
    {
      FRAME_X_COUNTER_VALUE (f)
	= FRAME_X_OUTPUT (f)->resize_counter_value;

      value = FRAME_X_COUNTER_VALUE (f);

      if (XSyncValueLow32 (value) % 2)
	{
	  XSyncIntToValue (&add, 1);
	  XSyncValueAdd (&value, value, add, &overflow);

	  if (overflow)
	    XSyncIntToValue (&value, 0);
	}

      FRAME_X_OUTPUT (f)->ext_sync_end_pending_p = false;
    }

  /* Since a frame is already in progress, there is no point in
     continuing.  */
  if (XSyncValueLow32 (value) % 2)
    return;

  /* Wait for the last frame to be drawn before drawing this one.  */
  x_sync_wait_for_frame_drawn_event (f);

  /* Make a note of the time at which we started to draw this
     frame.  */
  FRAME_X_OUTPUT (f)->temp_frame_time = x_sync_current_monotonic_time ();

  /* Since Emacs needs a non-urgent redraw, ensure that value % 4 ==
     1.  Later, add 3 to create the even counter value.  */
  if (XSyncValueLow32 (value) % 4 == 2)
    XSyncIntToValue (&add, 3);
  else
    XSyncIntToValue (&add, 1);

  XSyncValueAdd (&FRAME_X_COUNTER_VALUE (f),
		 value, add, &overflow);

  if (overflow)
    XSyncIntToValue (&FRAME_X_COUNTER_VALUE (f), 3);

  eassert (XSyncValueLow32 (FRAME_X_COUNTER_VALUE (f)) % 4 == 1);

  XSyncSetCounter (FRAME_X_DISPLAY (f),
		   FRAME_X_EXTENDED_COUNTER (f),
		   FRAME_X_COUNTER_VALUE (f));
}

#ifdef HAVE_XSYNCTRIGGERFENCE

/* Trigger the sync fence for counter VALUE immediately before a frame
   finishes.  */

static void
x_sync_trigger_fence (struct frame *f, XSyncValue value)
{
  uint_fast64_t n, low, high, idx;

  /* Sync fences aren't supported by the X server.  */
  if (FRAME_DISPLAY_INFO (f)->xsync_major < 3
      || (FRAME_DISPLAY_INFO (f)->xsync_major == 3
	  && FRAME_DISPLAY_INFO (f)->xsync_minor < 1))
    return;

  low = XSyncValueLow32 (value);
  high = XSyncValueHigh32 (value);

  n = low | (high << 32);
  idx = (n / 4) % 2;

#ifdef FRAME_DEBUG
  fprintf (stderr, "Triggering synchronization fence: %lu\n", idx);
#endif

  XSyncTriggerFence (FRAME_X_DISPLAY (f),
		     FRAME_X_OUTPUT (f)->sync_fences[idx]);
}

/* Initialize the sync fences on F.  */

void
x_sync_init_fences (struct frame *f)
{
  struct x_output *output;
  struct x_display_info *dpyinfo;

  output = FRAME_X_OUTPUT (f);
  dpyinfo = FRAME_DISPLAY_INFO (f);

  /* Sync fences aren't supported by the X server.  */
  if (dpyinfo->xsync_major < 3
      || (dpyinfo->xsync_major == 3
	  && dpyinfo->xsync_minor < 1))
    return;

  /* Suppress errors around XSyncCreateFence requests, since its
     implementations on certain X servers erroneously reject valid
     drawables, such as the frame's inner window.  (bug#69762) */

  x_catch_errors (dpyinfo->display);
  output->sync_fences[0]
    = XSyncCreateFence (FRAME_X_DISPLAY (f),
			/* The drawable given below is only used to
			   determine the screen on which the fence is
			   created.  */
			FRAME_X_WINDOW (f),
			False);
  output->sync_fences[1]
    = XSyncCreateFence (FRAME_X_DISPLAY (f),
			FRAME_X_WINDOW (f),
			False);
  if (x_had_errors_p (dpyinfo->display))
    output->sync_fences[1] = output->sync_fences[0] = None;
  x_uncatch_errors_after_check ();

  XChangeProperty (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		   dpyinfo->Xatom_net_wm_sync_fences, XA_CARDINAL,
		   32, PropModeReplace,
		   (unsigned char *) &output->sync_fences, 2);
}

static void
x_sync_free_fences (struct frame *f)
{
  if (FRAME_X_OUTPUT (f)->sync_fences[0] != None)
    XSyncDestroyFence (FRAME_X_DISPLAY (f),
		       FRAME_X_OUTPUT (f)->sync_fences[0]);

  if (FRAME_X_OUTPUT (f)->sync_fences[1] != None)
    XSyncDestroyFence (FRAME_X_DISPLAY (f),
		       FRAME_X_OUTPUT (f)->sync_fences[1]);
}

#endif

/* Tell the compositing manager that FRAME has been drawn and can be
   updated.  */

static void
x_sync_update_finish (struct frame *f)
{
  XSyncValue value, add;
  Bool overflow;

  if (FRAME_X_EXTENDED_COUNTER (f) == None)
    return;

  value = FRAME_X_COUNTER_VALUE (f);

  if (!(XSyncValueLow32 (value) % 2))
    return;

  if ((XSyncValueLow32 (value) % 4) == 1)
    /* This means the frame is non-urgent and should be drawn at the
       next redraw point.  */
    XSyncIntToValue (&add, 3);
  else
    /* Otherwise, the frame is urgent and should be drawn as soon as
       possible.  */
    XSyncIntToValue (&add, 1);

  XSyncValueAdd (&FRAME_X_COUNTER_VALUE (f),
		 value, add, &overflow);

  if (overflow)
    XSyncIntToValue (&FRAME_X_COUNTER_VALUE (f), 0);

  /* Trigger any sync fences if necessary.  */
#ifdef HAVE_XSYNCTRIGGERFENCE
  x_sync_trigger_fence (f, FRAME_X_COUNTER_VALUE (f));
#endif

  XSyncSetCounter (FRAME_X_DISPLAY (f),
		   FRAME_X_EXTENDED_COUNTER (f),
		   FRAME_X_COUNTER_VALUE (f));

  if (FRAME_OUTPUT_DATA (f)->use_vsync_p)
    FRAME_X_WAITING_FOR_DRAW (f) = true;
}

/* Handle a _NET_WM_FRAME_DRAWN message from the compositor.  */

static void
x_sync_handle_frame_drawn (struct x_display_info *dpyinfo,
			   XEvent *message, struct frame *f)
{
  XSyncValue value, counter;

  if (FRAME_OUTER_WINDOW (f) == message->xclient.window)
    {
      counter = FRAME_X_COUNTER_VALUE (f);

      /* Check that the counter in the message is the same as the
	 counter in the frame.  */
      XSyncIntsToValue (&value,
			message->xclient.data.l[0] & 0xffffffff,
			message->xclient.data.l[1] & 0xffffffff);

      if (XSyncValueEqual (value, counter))
	FRAME_X_WAITING_FOR_DRAW (f) = false;

      /* As long as a _NET_WM_FRAME_DRAWN message arrives, we know
	 that the compositor is still sending events, so avoid timing
	 out.  */
      FRAME_X_DRAW_JUST_HUNG (f) = false;
    }

  x_sync_note_frame_times (dpyinfo, f, message);
}
#endif

/* Start an update of frame F.  This function is installed as a hook
   for update_begin, i.e. it is called when update_begin is called.
   This function is called prior to calls to gui_update_window_begin for
   each window being updated.  Currently, there is nothing to do here
   because all interesting stuff is done on a window basis.  */

static void
x_update_begin (struct frame *f)
{
#if defined HAVE_XSYNC && !defined USE_GTK && defined HAVE_CLOCK_GETTIME
  /* If F is double-buffered, we can make the entire frame center
     around XdbeSwapBuffers.  */
#ifdef HAVE_XDBE
  if (!FRAME_X_DOUBLE_BUFFERED_P (f))
#endif
    x_sync_update_begin (f);
#else
  /* Nothing to do.  */
#endif

#ifdef HAVE_XDBE
  if (FRAME_X_DOUBLE_BUFFERED_P (f))
    /* The frame is no longer complete, as it is in the midst of an
       update.  */
    FRAME_X_COMPLETE_P (f) = false;
#endif
}

/* Draw a vertical window border from (x,y0) to (x,y1)  */

static void
x_draw_vertical_window_border (struct window *w, int x, int y0, int y1)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face;

  face = FACE_FROM_ID_OR_NULL (f, VERTICAL_BORDER_FACE_ID);
  if (face)
    XSetForeground (FRAME_X_DISPLAY (f), f->output_data.x->normal_gc,
		    face->foreground);

#ifdef USE_CAIRO
  x_fill_rectangle (f, f->output_data.x->normal_gc, x, y0, 1, y1 - y0, false);
#else
  XDrawLine (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
	     f->output_data.x->normal_gc, x, y0, x, y1);
#endif
}

/* Draw a window divider from (x0,y0) to (x1,y1)  */

static void
x_draw_window_divider (struct window *w, int x0, int x1, int y0, int y1)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct face *face = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_FACE_ID);
  struct face *face_first
    = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_FIRST_PIXEL_FACE_ID);
  struct face *face_last
    = FACE_FROM_ID_OR_NULL (f, WINDOW_DIVIDER_LAST_PIXEL_FACE_ID);
  unsigned long color = face ? face->foreground : FRAME_FOREGROUND_PIXEL (f);
  unsigned long color_first = (face_first
			       ? face_first->foreground
			       : FRAME_FOREGROUND_PIXEL (f));
  unsigned long color_last = (face_last
			      ? face_last->foreground
			      : FRAME_FOREGROUND_PIXEL (f));
  Display *display = FRAME_X_DISPLAY (f);

  if ((y1 - y0 > x1 - x0) && (x1 - x0 >= 3))
    /* A vertical divider, at least three pixels wide: Draw first and
       last pixels differently.  */
    {
      XSetForeground (display, f->output_data.x->normal_gc, color_first);
      x_fill_rectangle (f, f->output_data.x->normal_gc,
			x0, y0, 1, y1 - y0,
                        f->borders_respect_alpha_background);
      XSetForeground (display, f->output_data.x->normal_gc, color);
      x_fill_rectangle (f, f->output_data.x->normal_gc,
			x0 + 1, y0, x1 - x0 - 2, y1 - y0,
                        f->borders_respect_alpha_background);
      XSetForeground (display, f->output_data.x->normal_gc, color_last);
      x_fill_rectangle (f, f->output_data.x->normal_gc,
			x1 - 1, y0, 1, y1 - y0,
                        f->borders_respect_alpha_background);
    }
  else if ((x1 - x0 > y1 - y0) && (y1 - y0 >= 3))
    /* A horizontal divider, at least three pixels high: Draw first and
       last pixels differently.  */
    {
      XSetForeground (display, f->output_data.x->normal_gc, color_first);
      x_fill_rectangle (f, f->output_data.x->normal_gc,
			x0, y0, x1 - x0, 1,
                        f->borders_respect_alpha_background);
      XSetForeground (display, f->output_data.x->normal_gc, color);
      x_fill_rectangle (f, f->output_data.x->normal_gc,
			x0, y0 + 1, x1 - x0, y1 - y0 - 2,
                        f->borders_respect_alpha_background);
      XSetForeground (display, f->output_data.x->normal_gc, color_last);
      x_fill_rectangle (f, f->output_data.x->normal_gc,
			x0, y1 - 1, x1 - x0, 1,
                        f->borders_respect_alpha_background);
    }
  else
    {
    /* In any other case do not draw the first and last pixels
       differently.  */
      XSetForeground (display, f->output_data.x->normal_gc, color);
      x_fill_rectangle (f, f->output_data.x->normal_gc,
			x0, y0, x1 - x0, y1 - y0,
                        f->borders_respect_alpha_background);
    }
}

#ifdef HAVE_XDBE

/* Show the frame back buffer.  If frame is double-buffered,
   atomically publish to the user's screen graphics updates made since
   the last call to show_back_buffer.  */

static void
show_back_buffer (struct frame *f)
{
  XdbeSwapInfo swap_info;
#ifdef USE_CAIRO
  cairo_t *cr;
#endif

  if (FRAME_X_DOUBLE_BUFFERED_P (f))
    {
#if defined HAVE_XSYNC && !defined USE_GTK && defined HAVE_CLOCK_GETTIME
      /* Wait for drawing of the previous frame to complete before
	 displaying this new frame.  */
      x_sync_wait_for_frame_drawn_event (f);

      /* Begin a new frame.  */
      x_sync_update_begin (f);
#endif

#ifdef USE_CAIRO
      cr = FRAME_CR_CONTEXT (f);
      if (cr)
	cairo_surface_flush (cairo_get_target (cr));
#endif
      memset (&swap_info, 0, sizeof (swap_info));
      swap_info.swap_window = FRAME_X_WINDOW (f);
      swap_info.swap_action = XdbeCopied;
      XdbeSwapBuffers (FRAME_X_DISPLAY (f), &swap_info, 1);

#if defined HAVE_XSYNC && !defined USE_GTK && defined HAVE_CLOCK_GETTIME
      /* Finish the frame here.  */
      x_sync_update_finish (f);
#endif
    }

  FRAME_X_NEED_BUFFER_FLIP (f) = false;
}

#endif

/* Updates back buffer and flushes changes to display.  Called from
   minibuf read code.  Note that we display the back buffer even if
   buffer flipping is blocked.  */
static void
x_flip_and_flush (struct frame *f)
{
  /* Flipping buffers requires a working connection to the X server,
     which isn't always present if `inhibit-redisplay' is t, since
     this can be called from the IO error handler.  */
  if (!NILP (Vinhibit_redisplay)
      /* This has to work for tooltip frames, however, and redisplay
	 cannot happen when they are being flushed anyway.  (bug#55519) */
      && !FRAME_TOOLTIP_P (f))
    return;

  block_input ();
#ifdef HAVE_XDBE
  if (FRAME_X_NEED_BUFFER_FLIP (f))
    show_back_buffer (f);

  /* The frame is complete again as its contents were just
     flushed.  */
  FRAME_X_COMPLETE_P (f) = true;
#endif
  x_flush (f);
  unblock_input ();
}

/* End update of frame F.  This function is installed as a hook in
   update_end.  */

static void
x_update_end (struct frame *f)
{
  /* Mouse highlight may be displayed again.  */
  MOUSE_HL_INFO (f)->mouse_face_defer = false;

#ifdef USE_CAIRO
# ifdef HAVE_XDBE
  if (!FRAME_X_DOUBLE_BUFFERED_P (f) && FRAME_CR_CONTEXT (f))
    cairo_surface_flush (cairo_get_target (FRAME_CR_CONTEXT (f)));
# endif
#endif

  /* If double buffering is disabled, finish the update here.
     Otherwise, finish the update when the back buffer is next
     displayed.  */
#if defined HAVE_XSYNC && !defined USE_GTK && defined HAVE_CLOCK_GETTIME
#ifdef HAVE_XDBE
  if (!FRAME_X_DOUBLE_BUFFERED_P (f))
#endif
    x_sync_update_finish (f);
#endif
}

/* This function is called from various places in xdisp.c
   whenever a complete update has been performed.  */

static void
XTframe_up_to_date (struct frame *f)
{
  eassert (FRAME_X_P (f));
  block_input ();
  FRAME_MOUSE_UPDATE (f);

#ifdef HAVE_XDBE
  if (!buffer_flipping_blocked_p ()
      && FRAME_X_NEED_BUFFER_FLIP (f))
    show_back_buffer (f);

  /* The frame is now complete, as its contents have been drawn.  */
  FRAME_X_COMPLETE_P (f) = true;
#endif

#ifdef HAVE_XSYNC
#ifndef HAVE_GTK3
  if (FRAME_X_OUTPUT (f)->sync_end_pending_p
      && FRAME_X_BASIC_COUNTER (f) != None)
    {
      XSyncSetCounter (FRAME_X_DISPLAY (f),
		       FRAME_X_BASIC_COUNTER (f),
		       FRAME_X_OUTPUT (f)->pending_basic_counter_value);
      FRAME_X_OUTPUT (f)->sync_end_pending_p = false;
    }
#else
  if (FRAME_X_OUTPUT (f)->xg_sync_end_pending_p)
    {
      GtkWidget *widget = FRAME_GTK_OUTER_WIDGET (f);
      GdkWindow *window = gtk_widget_get_window (widget);
      eassert (window);
      GdkFrameClock *clock = gdk_window_get_frame_clock (window);
      eassert (clock);

      gdk_frame_clock_request_phase (clock,
				     GDK_FRAME_CLOCK_PHASE_AFTER_PAINT);
      FRAME_X_OUTPUT (f)->xg_sync_end_pending_p = false;
    }
#endif
#endif
  unblock_input ();
}

#ifdef HAVE_XDBE
static void
XTbuffer_flipping_unblocked_hook (struct frame *f)
{
  block_input ();

  if (FRAME_X_NEED_BUFFER_FLIP (f))
    show_back_buffer (f);

  unblock_input ();
}
#endif



/**
 * x_clear_under_internal_border:
 *
 * Clear area of frame F's internal border.  If the internal border face
 * of F has been specified (is not null), fill the area with that face.
 */
void
x_clear_under_internal_border (struct frame *f)
{
  if (FRAME_INTERNAL_BORDER_WIDTH (f) > 0)
    {
      int border = FRAME_INTERNAL_BORDER_WIDTH (f);
      int width = FRAME_PIXEL_WIDTH (f);
      int height = FRAME_PIXEL_HEIGHT (f);
      int margin = FRAME_TOP_MARGIN_HEIGHT (f);
      int bottom_margin = FRAME_BOTTOM_MARGIN_HEIGHT (f);
      int face_id = (FRAME_PARENT_FRAME (f)
		     ? (!NILP (Vface_remapping_alist)
			? lookup_basic_face (NULL, f,
					     CHILD_FRAME_BORDER_FACE_ID)
			: CHILD_FRAME_BORDER_FACE_ID)
		     : (!NILP (Vface_remapping_alist)
			? lookup_basic_face (NULL, f,
					     INTERNAL_BORDER_FACE_ID)
			: INTERNAL_BORDER_FACE_ID));
      struct face *face = FACE_FROM_ID_OR_NULL (f, face_id);

      if (face)
	{
	  unsigned long color = face->background;
	  Display *display = FRAME_X_DISPLAY (f);
	  GC gc = f->output_data.x->normal_gc;

	  XSetForeground (display, gc, color);
	  x_fill_rectangle (f, gc, 0, margin, width, border,
                            f->borders_respect_alpha_background);
	  x_fill_rectangle (f, gc, 0, 0, border, height,
                            f->borders_respect_alpha_background);
	  x_fill_rectangle (f, gc, width - border, 0, border, height,
                            f->borders_respect_alpha_background);
	  x_fill_rectangle (f, gc, 0, height - bottom_margin - border,
			    width, border,
                            f->borders_respect_alpha_background);
	  XSetForeground (display, gc, FRAME_FOREGROUND_PIXEL (f));
	}
      else
	{
	  x_clear_area (f, 0, 0, border, height);
	  x_clear_area (f, 0, margin, width, border);
	  x_clear_area (f, width - border, 0, border, height);
	  x_clear_area (f, 0, height - bottom_margin - border,
			width, border);
	}
    }
}

/* Draw truncation mark bitmaps, continuation mark bitmaps, overlay
   arrow bitmaps, or clear the fringes if no bitmaps are required
   before DESIRED_ROW is made current.  This function is called from
   update_window_line only if it is known that there are differences
   between bitmaps to be drawn between current row and DESIRED_ROW.  */

static void
x_after_update_window_line (struct window *w, struct glyph_row *desired_row)
{
  eassert (w);

  if (!desired_row->mode_line_p && !w->pseudo_window_p)
    desired_row->redraw_fringe_bitmaps_p = true;

#ifdef USE_X_TOOLKIT
  /* When a window has disappeared, make sure that no rest of
     full-width rows stays visible in the internal border.  Could
     check here if updated window is the leftmost/rightmost window,
     but I guess it's not worth doing since vertically split windows
     are almost never used, internal border is rarely set, and the
     overhead is very small.  */
  {
    struct frame *f;
    int width, height;

    if (windows_or_buffers_changed
	&& desired_row->full_width_p
	&& (f = XFRAME (w->frame),
	    width = FRAME_INTERNAL_BORDER_WIDTH (f),
	    width != 0)
	&& (height = desired_row->visible_height,
	    height > 0))
      {
	int y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, desired_row->y));
	int face_id =
	  (FRAME_PARENT_FRAME (f)
	   ? (!NILP (Vface_remapping_alist)
	      ? lookup_basic_face (NULL, f, CHILD_FRAME_BORDER_FACE_ID)
	      : CHILD_FRAME_BORDER_FACE_ID)
	   : (!NILP (Vface_remapping_alist)
	      ? lookup_basic_face (NULL, f, INTERNAL_BORDER_FACE_ID)
	      : INTERNAL_BORDER_FACE_ID));
	struct face *face = FACE_FROM_ID_OR_NULL (f, face_id);

	if (face)
	  {
	    unsigned long color = face->background;
	    Display *display = FRAME_X_DISPLAY (f);
	    GC gc = f->output_data.x->normal_gc;

	    XSetForeground (display, gc, color);
	    x_fill_rectangle (f, gc, 0, y, width, height, true);
	    x_fill_rectangle (f, gc, FRAME_PIXEL_WIDTH (f) - width, y,
			      width, height, true);
	    XSetForeground (display, gc, FRAME_FOREGROUND_PIXEL (f));
	  }
	else
	  {
	    x_clear_area (f, 0, y, width, height);
	    x_clear_area (f, FRAME_PIXEL_WIDTH (f) - width, y, width, height);
	  }
      }
  }
#endif
}

/* Generate a premultiplied pixel value for COLOR with ALPHA applied
   on the given display.  COLOR will be modified.  The display must
   use a visual that supports an alpha channel.

   This is possibly dead code on builds which do not support
   XRender.  */

#ifndef USE_CAIRO

static unsigned long
x_premultiply_pixel (struct x_display_info *dpyinfo,
		     XColor *color, double alpha)
{
  unsigned long pixel;

  eassert (dpyinfo->alpha_bits);

  /* Multiply the RGB channels.  */
  color->red *= alpha;
  color->green *= alpha;
  color->blue *= alpha;

  /* First, allocate a fully opaque pixel.  */
  pixel = x_make_truecolor_pixel (dpyinfo, color->red,
				  color->green,
				  color->blue);

  /* Next, erase the alpha component.  */
  pixel &= ~dpyinfo->alpha_mask;

  /* And add an alpha channel.  */
  pixel |= (((unsigned long) (alpha * 65535)
	     >> (16 - dpyinfo->alpha_bits))
	    << dpyinfo->alpha_offset);

  return pixel;
}

#endif

static void
x_draw_fringe_bitmap (struct window *w, struct glyph_row *row,
		      struct draw_fringe_bitmap_params *p)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  Display *display = FRAME_X_DISPLAY (f);
  GC gc = f->output_data.x->normal_gc;
  struct face *face = p->face;
  XRectangle clip_rect;

  /* Must clip because of partially visible lines.  */
  x_clip_to_row (w, row, ANY_AREA, gc, &clip_rect);

  if (p->bx >= 0 && !p->overlay_p)
    {
      /* In case the same realized face is used for fringes and
	 for something displayed in the text (e.g. face `region' on
	 mono-displays, the fill style may have been changed to
	 FillSolid in x_draw_glyph_string_background.  */
      if (face->stipple)
	{
	  XSetFillStyle (display, face->gc, FillOpaqueStippled);
	  x_fill_rectangle (f, face->gc, p->bx, p->by, p->nx, p->ny,
			    true);
	  XSetFillStyle (display, face->gc, FillSolid);

	  row->stipple_p = true;
	}
      else
	{
	  XSetBackground (display, face->gc, face->background);
	  x_clear_rectangle (f, face->gc, p->bx, p->by, p->nx, p->ny,
			   true);
	  XSetForeground (display, face->gc, face->foreground);
	}
    }

#ifdef USE_CAIRO
  if (p->which
      && p->which < max_fringe_bmp
      && p->which < max_used_fringe_bitmap)
    {
      XGCValues gcv;

      XGetGCValues (display, gc, GCForeground | GCBackground, &gcv);
      XSetForeground (display, gc, (p->cursor_p
				    ? (p->overlay_p ? face->background
				       : f->output_data.x->cursor_pixel)
				    : face->foreground));
      XSetBackground (display, gc, face->background);
      if (!fringe_bmp[p->which])
	{
	  /* This fringe bitmap is known to fringe.c, but lacks the
	     cairo_pattern_t pattern which shadows that bitmap.  This
	     is typical to define-fringe-bitmap being called when the
	     selected frame was not a GUI frame, for example, when
	     packages that define fringe bitmaps are loaded by a
	     daemon Emacs.  Create the missing pattern now.  */
	  gui_define_fringe_bitmap (f, p->which);
	}
      x_cr_draw_image (f, gc, fringe_bmp[p->which], 0, p->dh,
		       p->wd, p->h, p->x, p->y, p->overlay_p);
      XSetForeground (display, gc, gcv.foreground);
      XSetBackground (display, gc, gcv.background);
    }
#else  /* not USE_CAIRO */
  if (p->which)
    {
      Drawable drawable = FRAME_X_DRAWABLE (f);
      char *bits;
      Pixmap pixmap, clipmask = None;
      int depth = FRAME_DISPLAY_INFO (f)->n_planes;
      XGCValues gcv;
      unsigned long background = face->background;
      XColor bg;
#ifdef HAVE_XRENDER
      Picture picture = None;
      XRenderPictureAttributes attrs;

      memset (&attrs, 0, sizeof attrs);
#endif
      XRectangle image_rect, dest;
      int px, py, pwidth, pheight;

      /* Intersect the destination rectangle with that of the row.
	 Setting a clip mask overrides the clip rectangles provided by
	 x_clip_to_row, so clipping must be performed by hand.  */

      image_rect.x = p->x;
      image_rect.y = p->y;
      image_rect.width = p->wd;
      image_rect.height = p->h;

      if (!gui_intersect_rectangles (&clip_rect, &image_rect, &dest))
	/* The entire destination rectangle falls outside the row.  */
	goto undo_clip;

      /* Extrapolate the source rectangle from the difference between
	 the destination and image rectangles.  */

      px = dest.x - image_rect.x;
      py = dest.y - image_rect.y;
      pwidth = dest.width;
      pheight = dest.height;

      if (p->wd > 8)
	bits = (char *) (p->bits + p->dh);
      else
	bits = (char *) p->bits + p->dh;

      if (FRAME_DISPLAY_INFO (f)->alpha_bits
	  && f->alpha_background < 1.0)
	{
	  /* Extend the background color with an alpha channel
	     according to f->alpha_background.  */
	  bg.pixel = background;
	  x_query_colors (f, &bg, 1);

	  background
	    = x_premultiply_pixel (FRAME_DISPLAY_INFO (f),
				   &bg,
				   f->alpha_background);
	}

      /* Draw the bitmap.  I believe these small pixmaps can be cached
	 by the server.  */
      pixmap = XCreatePixmapFromBitmapData (display, drawable, bits, p->wd, p->h,
					    (p->cursor_p
					     ? (p->overlay_p ? face->background
						: f->output_data.x->cursor_pixel)
					     : face->foreground),
					    background, depth);

#ifdef HAVE_XRENDER
      if (FRAME_X_PICTURE_FORMAT (f)
	  && (x_xr_ensure_picture (f), FRAME_X_PICTURE (f)))
	picture = XRenderCreatePicture (display, pixmap,
					FRAME_X_PICTURE_FORMAT (f),
					0, &attrs);
#endif

      if (p->overlay_p)
	{
	  clipmask = XCreatePixmapFromBitmapData (display,
						  FRAME_DISPLAY_INFO (f)->root_window,
						  bits, p->wd, p->h,
						  1, 0, 1);

#ifdef HAVE_XRENDER
	  if (picture != None)
	    {
	      attrs.clip_mask = clipmask;
	      attrs.clip_x_origin = p->x;
	      attrs.clip_y_origin = p->y;

	      XRenderChangePicture (display, FRAME_X_PICTURE (f),
				    CPClipMask | CPClipXOrigin | CPClipYOrigin,
				    &attrs);
	    }
	  else
#endif
	    {
	      gcv.clip_mask = clipmask;
	      gcv.clip_x_origin = p->x;
	      gcv.clip_y_origin = p->y;
	      XChangeGC (display, gc, GCClipMask | GCClipXOrigin | GCClipYOrigin, &gcv);
	    }
	}

#ifdef HAVE_XRENDER
      if (picture != None)
	{
	  x_xr_apply_ext_clip (f, gc);
	  XRenderComposite (display, PictOpSrc, picture,
			    None, FRAME_X_PICTURE (f),
			    px, py, px, py, dest.x, dest.y,
			    pwidth, pheight);
	  x_xr_reset_ext_clip (f);

	  XRenderFreePicture (display, picture);
	}
      else
#endif
	XCopyArea (display, pixmap, drawable, gc, px, py,
		   pwidth, pheight, dest.x, dest.y);
      XFreePixmap (display, pixmap);

      if (p->overlay_p)
	{
	  gcv.clip_mask = (Pixmap) 0;
	  XChangeGC (display, gc, GCClipMask, &gcv);
	  XFreePixmap (display, clipmask);
	}
    }

  undo_clip:
#endif  /* not USE_CAIRO */

  x_reset_clip_rectangles (f, gc);
}

/***********************************************************************
			    Glyph display
 ***********************************************************************/

static bool x_alloc_lighter_color (struct frame *, Display *, Colormap,
				   unsigned long *, double, int);
static void x_scroll_bar_clear (struct frame *);

#ifdef GLYPH_DEBUG
static void x_check_font (struct frame *, struct font *);
#endif

/* If SEND_EVENT, make sure that TIME is larger than the current last
   user time.  We don't sanitize timestamps from events sent by the X
   server itself because some Lisp might have set the user time to a
   ridiculously large value, and this way a more reasonable timestamp
   can be obtained upon the next event.

   Alternatively, the server time could've overflowed.

   SET_PROPERTY specifies whether or not to change the user time
   property for the active frame.  The important thing is to not set
   the last user time upon leave events; on Metacity and GNOME Shell,
   mapping a new frame on top of the old frame potentially causes
   crossing events to be sent to the old frame if it contains the
   pointer, as the new frame will initially stack above the old frame.
   If _NET_WM_USER_TIME is changed at that point, then GNOME may get
   notified about the user time change on the old frame before it
   tries to focus the new frame, which will make it consider the new
   frame (whose user time property will not have been updated at that
   point, due to not being focused) as having been mapped
   out-of-order, and lower the new frame, which is typically not what
   users want.  */

static void
x_display_set_last_user_time (struct x_display_info *dpyinfo, Time time,
			      bool send_event, bool set_property)
{
#if defined HAVE_XSYNC && !defined USE_GTK && defined HAVE_CLOCK_GETTIME
  uint_fast64_t monotonic_time;
  uint_fast64_t monotonic_ms;
  int_fast64_t diff_ms;
#endif
#ifndef USE_GTK
  struct frame *focus_frame;
  Time old_time;

  focus_frame = dpyinfo->x_focus_frame;
  old_time = dpyinfo->last_user_time;
#endif

#ifdef ENABLE_CHECKING
  eassert (time <= X_ULONG_MAX);
#endif

  if (!send_event || time > dpyinfo->last_user_time)
    dpyinfo->last_user_time = time;

#if defined HAVE_XSYNC && !defined USE_GTK && defined HAVE_CLOCK_GETTIME
  if (!send_event)
    {
      /* See if the current CLOCK_MONOTONIC time is reasonably close
	 to the X server time.  */
      monotonic_time = x_sync_current_monotonic_time ();
      monotonic_ms = monotonic_time / 1000;

      dpyinfo->server_time_monotonic_p
	= (monotonic_time != 0
	   && !ckd_sub (&diff_ms, time, monotonic_ms)
	   && -500 < diff_ms && diff_ms < 500);

      if (!dpyinfo->server_time_monotonic_p)
	{
	  /* Compute an offset that can be subtracted from the server
	     time to estimate the monotonic time on the X server.  */

	  if (!monotonic_time
	      || ckd_mul (&dpyinfo->server_time_offset, time, 1000)
	      || ckd_sub (&dpyinfo->server_time_offset,
			  dpyinfo->server_time_offset, monotonic_time))
	    dpyinfo->server_time_offset = 0;

	  /* If the server time is reasonably close to the monotonic
	     time after the latter is truncated to CARD32, simply make
	     the offset that between the server time in ms and the
	     actual time in ms.  */

	  monotonic_ms = monotonic_ms & 0xffffffff;
	  if (!ckd_sub (&diff_ms, time, monotonic_ms)
	      && -500 < diff_ms && diff_ms < 500)
	    {
	      /* The server timestamp overflowed.  Make the time
		 offset exactly how much it overflowed by.  */

	      if (ckd_sub (&dpyinfo->server_time_offset,
			   monotonic_time / 1000, monotonic_ms)
		  || ckd_mul (&dpyinfo->server_time_offset,
			      dpyinfo->server_time_offset, 1000)
		  || ckd_sub (&dpyinfo->server_time_offset,
			      0, dpyinfo->server_time_offset))
		dpyinfo->server_time_offset = 0;
	    }
	}
    }
#endif

#ifndef USE_GTK
  /* Don't waste bandwidth if the time hasn't actually changed.  */
  if (focus_frame && old_time != dpyinfo->last_user_time
      && set_property)
    {
      time = dpyinfo->last_user_time;

      while (FRAME_PARENT_FRAME (focus_frame))
	focus_frame = FRAME_PARENT_FRAME (focus_frame);

      if (FRAME_X_OUTPUT (focus_frame)->user_time_window != None)
	XChangeProperty (dpyinfo->display,
			 FRAME_X_OUTPUT (focus_frame)->user_time_window,
			 dpyinfo->Xatom_net_wm_user_time,
			 XA_CARDINAL, 32, PropModeReplace,
			 (unsigned char *) &time, 1);
    }
#endif
}

#ifdef USE_GTK

static void
x_set_gtk_user_time (struct frame *f, Time time)
{
  GtkWidget *widget;
  GdkWindow *window;

  widget = FRAME_GTK_OUTER_WIDGET (f);
  window = gtk_widget_get_window (widget);

  /* This widget isn't realized yet.  */
  if (!window)
    return;

  gdk_x11_window_set_user_time (window, time);
}

#endif

#if !defined USE_GTK || defined HAVE_XFIXES

/* Create and return a special window for receiving events such as
   selection notify events, and reporting user time.  The window is an
   1x1 unmapped override-redirect InputOnly window at -1, -1 relative
   to the parent, which should prevent it from doing anything.  */

static Window
x_create_special_window (struct x_display_info *dpyinfo,
			 Window parent_window)
{
  XSetWindowAttributes attrs;

  attrs.override_redirect = True;

  return XCreateWindow (dpyinfo->display, parent_window,
			-1, -1, 1, 1, 0, CopyFromParent, InputOnly,
			CopyFromParent, CWOverrideRedirect, &attrs);
}

#endif

/* Not needed on GTK because GTK handles reporting the user time
   itself.  */

#ifndef USE_GTK

static void
x_update_frame_user_time_window (struct frame *f)
{
  struct x_output *output;
  struct x_display_info *dpyinfo;

  output = FRAME_X_OUTPUT (f);
  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (!NILP (Vx_no_window_manager)
      || !x_wm_supports (f, dpyinfo->Xatom_net_wm_user_time))
    {
      if (output->user_time_window != None
	  && output->user_time_window != FRAME_OUTER_WINDOW (f))
	{
	  XDestroyWindow (dpyinfo->display, output->user_time_window);
	  XDeleteProperty (dpyinfo->display, FRAME_OUTER_WINDOW (f),
			   dpyinfo->Xatom_net_wm_user_time_window);
	}
      else
	XDeleteProperty (dpyinfo->display, FRAME_OUTER_WINDOW (f),
			 dpyinfo->Xatom_net_wm_user_time);

      output->user_time_window = None;
      return;
    }

  if (!x_wm_supports (f, dpyinfo->Xatom_net_wm_user_time_window))
    {
      if (output->user_time_window == None)
	output->user_time_window = FRAME_OUTER_WINDOW (f);
      else if (output->user_time_window != FRAME_OUTER_WINDOW (f))
	{
	  XDestroyWindow (dpyinfo->display,
			  output->user_time_window);
	  XDeleteProperty (dpyinfo->display,
			   FRAME_OUTER_WINDOW (f),
			   dpyinfo->Xatom_net_wm_user_time_window);
	  output->user_time_window = FRAME_OUTER_WINDOW (f);
	}
    }
  else
    {
      if (output->user_time_window == FRAME_OUTER_WINDOW (f)
	  || output->user_time_window == None)
	{
	  /* Create a "user time" window that is used to report user
	     activity on a given frame.  This is used in preference to
	     _NET_WM_USER_TIME, as using a separate window allows the
	     window manager to express interest in other properties
	     while only reading the user time when necessary, thereby
	     improving battery life by not involving the window
	     manager in each key press.  */

	  output->user_time_window
	    = x_create_special_window (dpyinfo, FRAME_X_WINDOW (f));

	  XDeleteProperty (dpyinfo->display, FRAME_OUTER_WINDOW (f),
			   dpyinfo->Xatom_net_wm_user_time);
	  XChangeProperty (dpyinfo->display, FRAME_OUTER_WINDOW (f),
			   dpyinfo->Xatom_net_wm_user_time_window,
			   XA_WINDOW, 32, PropModeReplace,
			   (unsigned char *) &output->user_time_window, 1);
	}
    }
}

#endif

void
x_set_last_user_time_from_lisp (struct x_display_info *dpyinfo,
				Time time)
{
  x_display_set_last_user_time (dpyinfo, time, true, true);
}


/* Set S->gc to a suitable GC for drawing glyph string S in cursor
   face.  */

static void
x_set_cursor_gc (struct glyph_string *s)
{
  if (s->font == FRAME_FONT (s->f)
      && s->face->background == FRAME_BACKGROUND_PIXEL (s->f)
      && s->face->foreground == FRAME_FOREGROUND_PIXEL (s->f)
      && !s->cmp)
    s->gc = s->f->output_data.x->cursor_gc;
  else
    {
      /* Cursor on non-default face: must merge.  */
      XGCValues xgcv;
      unsigned long mask;
      Display *display = FRAME_X_DISPLAY (s->f);

      xgcv.background = s->f->output_data.x->cursor_pixel;
      xgcv.foreground = s->face->background;

      /* If the glyph would be invisible, try a different foreground.  */
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->f->output_data.x->cursor_foreground_pixel;
      if (xgcv.foreground == xgcv.background)
	xgcv.foreground = s->face->foreground;

      /* Make sure the cursor is distinct from text in this face.  */
      if (xgcv.background == s->face->background
	  && xgcv.foreground == s->face->foreground)
	{
	  xgcv.background = s->face->foreground;
	  xgcv.foreground = s->face->background;
	}

      IF_DEBUG (x_check_font (s->f, s->font));
      xgcv.graphics_exposures = False;
      xgcv.line_width = 1;
      mask = (GCForeground | GCBackground
	      | GCGraphicsExposures
	      | GCLineWidth);

      if (FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	XChangeGC (display, FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc,
		   mask, &xgcv);
      else
	FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc
          = XCreateGC (display, FRAME_X_DRAWABLE (s->f), mask, &xgcv);

      s->gc = FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc;
    }
}


/* Set up S->gc of glyph string S for drawing text in mouse face.  */

static void
x_set_mouse_face_gc (struct glyph_string *s)
{
  if (s->font == s->face->font)
    s->gc = s->face->gc;
  else
    {
      /* Otherwise construct scratch_cursor_gc with values from FACE
	 except for FONT.  */
      XGCValues xgcv;
      unsigned long mask;
      Display *display = FRAME_X_DISPLAY (s->f);

      xgcv.background = s->face->background;
      xgcv.foreground = s->face->foreground;
      xgcv.graphics_exposures = False;
      xgcv.line_width = 1;

      mask = (GCForeground | GCBackground
	      | GCGraphicsExposures
	      | GCLineWidth);

      if (FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc)
	XChangeGC (display, FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc,
		   mask, &xgcv);
      else
	FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc
          = XCreateGC (display, FRAME_X_DRAWABLE (s->f), mask, &xgcv);

      s->gc = FRAME_DISPLAY_INFO (s->f)->scratch_cursor_gc;

    }
  eassert (s->gc != 0);
}


/* Set S->gc of glyph string S to a GC suitable for drawing a mode line.
   Faces to use in the mode line have already been computed when the
   matrix was built, so there isn't much to do, here.  */

static void
x_set_mode_line_face_gc (struct glyph_string *s)
{
  s->gc = s->face->gc;
}


/* Set S->gc of glyph string S for drawing that glyph string.  Set
   S->stippled_p to a non-zero value if the face of S has a stipple
   pattern.  */

static void
x_set_glyph_string_gc (struct glyph_string *s)
{
  prepare_face_for_display (s->f, s->face);

  if (s->hl == DRAW_NORMAL_TEXT)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_INVERSE_VIDEO)
    {
      x_set_mode_line_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_CURSOR)
    {
      x_set_cursor_gc (s);
      s->stippled_p = false;
    }
  else if (s->hl == DRAW_MOUSE_FACE)
    {
      x_set_mouse_face_gc (s);
      s->stippled_p = s->face->stipple != 0;
    }
  else if (s->hl == DRAW_IMAGE_RAISED
	   || s->hl == DRAW_IMAGE_SUNKEN)
    {
      s->gc = s->face->gc;
      s->stippled_p = s->face->stipple != 0;
    }
  else
    emacs_abort ();

  /* GC must have been set.  */
  eassert (s->gc != 0);
}


/* Set clipping for output of glyph string S.  S may be part of a mode
   line or menu if we don't have X toolkit support.  */

static void
x_set_glyph_string_clipping (struct glyph_string *s)
{
  XRectangle *r = s->clip;
  int n = get_glyph_string_clip_rects (s, r, 2);

  if (n > 0)
    x_set_clip_rectangles (s->f, s->gc, r, n);
  s->num_clips = n;
}


/* Set SRC's clipping for output of glyph string DST.  This is called
   when we are drawing DST's left_overhang or right_overhang only in
   the area of SRC.  */

static void
x_set_glyph_string_clipping_exactly (struct glyph_string *src, struct glyph_string *dst)
{
  XRectangle r;

  r.x = src->x;
  r.width = src->width;
  r.y = src->y;
  r.height = src->height;
  dst->clip[0] = r;
  dst->num_clips = 1;
  x_set_clip_rectangles (dst->f, dst->gc, &r, 1);
}


/* RIF:
   Compute left and right overhang of glyph string S.  */

static void
x_compute_glyph_string_overhangs (struct glyph_string *s)
{
  if (s->cmp == NULL
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))
    {
      struct font_metrics metrics;

      if (s->first_glyph->type == CHAR_GLYPH)
	{
	  struct font *font = s->font;
	  font->driver->text_extents (font, s->char2b, s->nchars, &metrics);
	}
      else
	{
	  Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);

	  composition_gstring_width (gstring, s->cmp_from, s->cmp_to, &metrics);
	}
      s->right_overhang = (metrics.rbearing > metrics.width
			   ? metrics.rbearing - metrics.width : 0);
      s->left_overhang = metrics.lbearing < 0 ? - metrics.lbearing : 0;
    }
  else if (s->cmp)
    {
      s->right_overhang = s->cmp->rbearing - s->cmp->pixel_width;
      s->left_overhang = - s->cmp->lbearing;
    }
}


/* Fill rectangle X, Y, W, H with background color of glyph string S.  */

static void
x_clear_glyph_string_rect (struct glyph_string *s, int x, int y, int w, int h)
{
  x_clear_rectangle (s->f, s->gc, x, y, w, h, s->hl != DRAW_CURSOR);
}

#ifndef USE_CAIRO

static void
x_clear_point (struct frame *f, GC gc, int x, int y,
	       bool respect_alpha_background)
{
  XGCValues xgcv;
  Display *dpy;

  dpy = FRAME_X_DISPLAY (f);

  if (f->alpha_background != 1.0
      && respect_alpha_background)
    {
      x_clear_rectangle (f, gc, x, y, 1, 1, true);
      return;
    }

  XGetGCValues (dpy, gc, GCBackground | GCForeground, &xgcv);
  XSetForeground (dpy, gc, xgcv.background);
  XDrawPoint (dpy, FRAME_X_DRAWABLE (f), gc, x, y);
  XSetForeground (dpy, gc, xgcv.foreground);
}

#endif

/* Draw the background of glyph_string S.  If S->background_filled_p
   is non-zero don't draw it.  FORCE_P non-zero means draw the
   background even if it wouldn't be drawn normally.  This is used
   when a string preceding S draws into the background of S, or S
   contains the first component of a composition.  */

static void
x_draw_glyph_string_background (struct glyph_string *s, bool force_p)
{
  /* Nothing to do if background has already been drawn or if it
     shouldn't be drawn in the first place.  */
  if (!s->background_filled_p)
    {
      int box_line_width = max (s->face->box_horizontal_line_width, 0);

      if (s->stippled_p)
	{
          Display *display = FRAME_X_DISPLAY (s->f);

	  /* Fill background with a stipple pattern.  */
	  XSetFillStyle (display, s->gc, FillOpaqueStippled);
	  x_fill_rectangle (s->f, s->gc, s->x,
			    s->y + box_line_width,
			    s->background_width,
			    s->height - 2 * box_line_width,
			    s->hl != DRAW_CURSOR);
	  XSetFillStyle (display, s->gc, FillSolid);
	  s->background_filled_p = true;
	}
      else if (FONT_HEIGHT (s->font) < s->height - 2 * box_line_width
	       /* When xdisp.c ignores FONT_HEIGHT, we cannot trust
		  font dimensions, since the actual glyphs might be
		  much smaller.  So in that case we always clear the
		  rectangle with background color.  */
	       || FONT_TOO_HIGH (s->font)
	       || s->font_not_found_p
	       || s->extends_to_end_of_line_p
	       || force_p)
	{
	  x_clear_glyph_string_rect (s, s->x, s->y + box_line_width,
				     s->background_width,
				     s->height - 2 * box_line_width);
	  s->background_filled_p = true;
	}
    }
}


/* Draw the foreground of glyph string S.  */

static void
x_draw_glyph_string_foreground (struct glyph_string *s)
{
  int i, x;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  /* Draw characters of S as rectangles if S's font could not be
     loaded.  */
  if (s->font_not_found_p)
    {
      for (i = 0; i < s->nchars; ++i)
	{
	  struct glyph *g = s->first_glyph + i;
	  x_draw_rectangle (s->f,
			  s->gc, x, s->y, g->pixel_width - 1,
			  s->height - 1);
	  x += g->pixel_width;
	}
    }
  else
    {
      struct font *font = s->font;
#ifdef USE_CAIRO
      if (!EQ (font->driver->type, Qx)
	  || x_try_cr_xlib_drawable (s->f, s->gc))
	{
#endif	/* USE_CAIRO */
	  int boff = font->baseline_offset;
	  int y;

	  if (font->vertical_centering)
	    boff = VCENTER_BASELINE_OFFSET (font, s->f) - boff;

	  y = s->ybase - boff;
	  if (s->for_overlaps
	      || (s->background_filled_p && s->hl != DRAW_CURSOR))
	    font->driver->draw (s, 0, s->nchars, x, y, false);
	  else
	    font->driver->draw (s, 0, s->nchars, x, y, true);
	  if (s->face->overstrike)
	    font->driver->draw (s, 0, s->nchars, x + 1, y, false);
#ifdef USE_CAIRO
	  if (EQ (font->driver->type, Qx))
	    x_end_cr_xlib_drawable (s->f, s->gc);
	}
      else
	{
	  /* Fallback for the case that no Xlib Drawable is available
	     for drawing text with X core fonts.  */
	  if (!(s->for_overlaps
		|| (s->background_filled_p && s->hl != DRAW_CURSOR)))
	    {
	      int box_line_width = max (s->face->box_horizontal_line_width, 0);

	      if (s->stippled_p)
		{
		  Display *display = FRAME_X_DISPLAY (s->f);

		  /* Fill background with a stipple pattern.  */
		  XSetFillStyle (display, s->gc, FillOpaqueStippled);
		  x_fill_rectangle (s->f, s->gc, s->x,
				    s->y + box_line_width,
				    s->background_width,
				    s->height - 2 * box_line_width,
				    false);
		  XSetFillStyle (display, s->gc, FillSolid);
		}
	      else
		x_clear_glyph_string_rect (s, s->x, s->y + box_line_width,
					   s->background_width,
					   s->height - 2 * box_line_width);
	    }
	  for (i = 0; i < s->nchars; ++i)
	    {
	      struct glyph *g = s->first_glyph + i;
	      x_draw_rectangle (s->f,
				s->gc, x, s->y, g->pixel_width - 1,
				s->height - 1);
	      x += g->pixel_width;
	    }
	}
#endif	/* USE_CAIRO */
    }
}

/* Draw the foreground of composite glyph string S.  */

static void
x_draw_composite_glyph_string_foreground (struct glyph_string *s)
{
  int i, j, x;
  struct font *font = s->font;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  /* S is a glyph string for a composition.  S->cmp_from is the index
     of the first character drawn for glyphs of this composition.
     S->cmp_from == 0 means we are drawing the very first character of
     this composition.  */

  /* Draw a rectangle for the composition if the font for the very
     first character of the composition could not be loaded.  */
  if (s->font_not_found_p)
    {
      if (s->cmp_from == 0)
	x_draw_rectangle (s->f, s->gc, x, s->y,
			s->width - 1, s->height - 1);
    }
  else
#ifdef USE_CAIRO
    if (!EQ (font->driver->type, Qx)
	|| x_try_cr_xlib_drawable (s->f, s->gc))
      {
#endif	/* USE_CAIRO */
	if (! s->first_glyph->u.cmp.automatic)
	  {
	    int y = s->ybase;

	    for (i = 0, j = s->cmp_from; i < s->nchars; i++, j++)
	      /* TAB in a composition means display glyphs with
		 padding space on the left or right.  */
	      if (COMPOSITION_GLYPH (s->cmp, j) != '\t')
		{
		  int xx = x + s->cmp->offsets[j * 2];
		  int yy = y - s->cmp->offsets[j * 2 + 1];

		  font->driver->draw (s, j, j + 1, xx, yy, false);
		  if (s->face->overstrike)
		    font->driver->draw (s, j, j + 1, xx + 1, yy, false);
		}
	  }
	else
	  {
	    Lisp_Object gstring = composition_gstring_from_id (s->cmp_id);
	    Lisp_Object glyph;
	    int y = s->ybase;
	    int width = 0;

	    for (i = j = s->cmp_from; i < s->cmp_to; i++)
	      {
		glyph = LGSTRING_GLYPH (gstring, i);
		if (NILP (LGLYPH_ADJUSTMENT (glyph)))
		  width += LGLYPH_WIDTH (glyph);
		else
		  {
		    int xoff, yoff, wadjust;

		    if (j < i)
		      {
			font->driver->draw (s, j, i, x, y, false);
			if (s->face->overstrike)
			  font->driver->draw (s, j, i, x + 1, y, false);
			x += width;
		      }
		    xoff = LGLYPH_XOFF (glyph);
		    yoff = LGLYPH_YOFF (glyph);
		    wadjust = LGLYPH_WADJUST (glyph);
		    font->driver->draw (s, i, i + 1, x + xoff, y + yoff, false);
		    if (s->face->overstrike)
		      font->driver->draw (s, i, i + 1, x + xoff + 1, y + yoff,
					  false);
		    x += wadjust;
		    j = i + 1;
		    width = 0;
		  }
	      }
	    if (j < i)
	      {
		font->driver->draw (s, j, i, x, y, false);
		if (s->face->overstrike)
		  font->driver->draw (s, j, i, x + 1, y, false);
	      }
	  }
#ifdef USE_CAIRO
	if (EQ (font->driver->type, Qx))
	  x_end_cr_xlib_drawable (s->f, s->gc);
      }
    else
      {
	/* Fallback for the case that no Xlib Drawable is available
	   for drawing text with X core fonts.  */
	if (s->cmp_from == 0)
	  x_draw_rectangle (s->f, s->gc, x, s->y,
			    s->width - 1, s->height - 1);
      }
#endif	/* USE_CAIRO */
}


/* Draw the foreground of glyph string S for glyphless characters.  */

static void
x_draw_glyphless_glyph_string_foreground (struct glyph_string *s)
{
  struct glyph *glyph = s->first_glyph;
  unsigned char2b[8];
  int x, i, j;

  /* If first glyph of S has a left box line, start drawing the text
     of S to the right of that box line.  */
  if (s->face && s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p)
    x = s->x + max (s->face->box_vertical_line_width, 0);
  else
    x = s->x;

  s->char2b = char2b;

  for (i = 0; i < s->nchars; i++, glyph++)
    {
#ifdef GCC_LINT
      enum { PACIFY_GCC_BUG_81401 = 1 };
#else
      enum { PACIFY_GCC_BUG_81401 = 0 };
#endif
      char buf[7 + PACIFY_GCC_BUG_81401];
      char *str = NULL;
      int len = glyph->u.glyphless.len;

      if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_ACRONYM)
	{
	  if (len > 0
	      && CHAR_TABLE_P (Vglyphless_char_display)
	      && (CHAR_TABLE_EXTRA_SLOTS (XCHAR_TABLE (Vglyphless_char_display))
		  >= 1))
	    {
	      Lisp_Object acronym
		= (! glyph->u.glyphless.for_no_font
		   ? CHAR_TABLE_REF (Vglyphless_char_display,
				     glyph->u.glyphless.ch)
		   : XCHAR_TABLE (Vglyphless_char_display)->extras[0]);
	      if (CONSP (acronym))
		acronym = XCAR (acronym);
	      if (STRINGP (acronym))
		str = SSDATA (acronym);
	    }
	}
      else if (glyph->u.glyphless.method == GLYPHLESS_DISPLAY_HEX_CODE)
	{
	  unsigned int ch = glyph->u.glyphless.ch;
	  eassume (ch <= MAX_CHAR);
	  sprintf (buf, "%0*X", ch < 0x10000 ? 4 : 6, ch);
	  str = buf;
	}

      if (str)
	{
	  int upper_len = (len + 1) / 2;

	  /* It is assured that all LEN characters in STR is ASCII.  */
	  for (j = 0; j < len; j++)
            char2b[j] = s->font->driver->encode_char (s->font, str[j]) & 0xFFFF;
	  s->font->driver->draw (s, 0, upper_len,
				 x + glyph->slice.glyphless.upper_xoff,
				 s->ybase + glyph->slice.glyphless.upper_yoff,
				 false);
	  s->font->driver->draw (s, upper_len, len,
				 x + glyph->slice.glyphless.lower_xoff,
				 s->ybase + glyph->slice.glyphless.lower_yoff,
				 false);
	}
      if (glyph->u.glyphless.method != GLYPHLESS_DISPLAY_THIN_SPACE)
	x_draw_rectangle (s->f, s->gc,
			x, s->ybase - glyph->ascent,
			glyph->pixel_width - 1,
			glyph->ascent + glyph->descent - 1);
      x += glyph->pixel_width;
   }

  /* Defend against hypothetical bad code elsewhere that uses
     s->char2b after this function returns.  */
  s->char2b = NULL;
}

#ifdef USE_X_TOOLKIT

#ifdef USE_LUCID

/* Return the frame on which widget WIDGET is used.. Abort if frame
   cannot be determined.  */

static struct frame *
x_frame_of_widget (Widget widget)
{
  struct x_display_info *dpyinfo;
  Lisp_Object tail, frame;
  struct frame *f;

  dpyinfo = x_dpyinfo (XtDisplay (widget));

  /* Find the top-level shell of the widget.  Note that this function
     can be called when the widget is not yet realized, so XtWindow
     (widget) == 0.  That's the reason we can't simply use
     x_any_window_to_frame.  */
  while (!XtIsTopLevelShell (widget))
    widget = XtParent (widget);

  /* Look for a frame with that top-level widget.  Allocate the color
     on that frame to get the right gamma correction value.  */
  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);
      if (FRAME_X_P (f)
	  && FRAME_DISPLAY_INFO (f) == dpyinfo
	  && f->output_data.x->widget == widget)
	return f;
    }
  emacs_abort ();
}

/* Allocate a color which is lighter or darker than *PIXEL by FACTOR
   or DELTA.  Try a color with RGB values multiplied by FACTOR first.
   If this produces the same color as PIXEL, try a color where all RGB
   values have DELTA added.  Return the allocated color in *PIXEL.
   DISPLAY is the X display, CMAP is the colormap to operate on.
   Value is true if successful.  */

bool
x_alloc_lighter_color_for_widget (Widget widget, Display *display, Colormap cmap,
				  unsigned long *pixel, double factor, int delta)
{
  struct frame *f = x_frame_of_widget (widget);
  return x_alloc_lighter_color (f, display, cmap, pixel, factor, delta);
}

#endif /* USE_LUCID */


/* Structure specifying which arguments should be passed by Xt to
   cvt_string_to_pixel.  We want the widget's screen and colormap.  */

static XtConvertArgRec cvt_string_to_pixel_args[] =
  {
    {XtWidgetBaseOffset, (XtPointer) offsetof (WidgetRec, core.screen),
     sizeof (Screen *)},
    {XtWidgetBaseOffset, (XtPointer) offsetof (WidgetRec, core.colormap),
     sizeof (Colormap)}
  };


/* The address of this variable is returned by
   cvt_string_to_pixel.  */

static Pixel cvt_string_to_pixel_value;


/* Convert a color name to a pixel color.

   DPY is the display we are working on.

   ARGS is an array of *NARGS XrmValue structures holding additional
   information about the widget for which the conversion takes place.
   The contents of this array are determined by the specification
   in cvt_string_to_pixel_args.

   FROM is a pointer to an XrmValue which points to the color name to
   convert.  TO is an XrmValue in which to return the pixel color.

   CLOSURE_RET is a pointer to user-data, in which we record if
   we allocated the color or not.

   Value is True if successful, False otherwise.  */

static Boolean
cvt_string_to_pixel (Display *dpy, XrmValue *args, Cardinal *nargs,
		     XrmValue *from, XrmValue *to,
		     XtPointer *closure_ret)
{
  Screen *screen;
  Colormap cmap;
  Pixel pixel;
  String color_name;
  XColor color;

  if (*nargs != 2)
    {
      XtAppWarningMsg (XtDisplayToApplicationContext (dpy),
		       "wrongParameters", "cvt_string_to_pixel",
		       "XtToolkitError",
		       "Screen and colormap args required", NULL, NULL);
      return False;
    }

  screen = *(Screen **) args[0].addr;
  cmap = *(Colormap *) args[1].addr;
  color_name = (String) from->addr;

  if (strcmp (color_name, XtDefaultBackground) == 0)
    {
      *closure_ret = (XtPointer) False;
      pixel = WhitePixelOfScreen (screen);
    }
  else if (strcmp (color_name, XtDefaultForeground) == 0)
    {
      *closure_ret = (XtPointer) False;
      pixel = BlackPixelOfScreen (screen);
    }
  else if (XParseColor (dpy, cmap, color_name, &color)
	   && x_alloc_nearest_color_1 (dpy, cmap, &color))
    {
      pixel = color.pixel;
      *closure_ret = (XtPointer) True;
    }
  else
    {
      String params[1];
      Cardinal nparams = 1;

      params[0] = color_name;
      XtAppWarningMsg (XtDisplayToApplicationContext (dpy),
		       "badValue", "cvt_string_to_pixel",
		       "XtToolkitError", "Invalid color '%s'",
		       params, &nparams);
      return False;
    }

  if (to->addr != NULL)
    {
      if (to->size < sizeof (Pixel))
	{
	  to->size = sizeof (Pixel);
	  return False;
	}

      *(Pixel *) to->addr = pixel;
    }
  else
    {
      cvt_string_to_pixel_value = pixel;
      to->addr = (XtPointer) &cvt_string_to_pixel_value;
    }

  to->size = sizeof (Pixel);
  return True;
}


/* Free a pixel color which was previously allocated via
   cvt_string_to_pixel.  This is registered as the destructor
   for this type of resource via XtSetTypeConverter.

   APP is the application context in which we work.

   TO is a pointer to an XrmValue holding the color to free.
   CLOSURE is the value we stored in CLOSURE_RET for this color
   in cvt_string_to_pixel.

   ARGS and NARGS are like for cvt_string_to_pixel.  */

static void
cvt_pixel_dtor (XtAppContext app, XrmValuePtr to, XtPointer closure, XrmValuePtr args,
		Cardinal *nargs)
{
  if (*nargs != 2)
    {
      XtAppWarningMsg (app, "wrongParameters", "cvt_pixel_dtor",
		       "XtToolkitError",
		       "Screen and colormap arguments required",
		       NULL, NULL);
    }
  else if (closure != NULL)
    {
      /* We did allocate the pixel, so free it.  */
      Screen *screen = *(Screen **) args[0].addr;
      Colormap cmap = *(Colormap *) args[1].addr;
      x_free_dpy_colors (DisplayOfScreen (screen), screen, cmap,
			 (Pixel *) to->addr, 1);
    }
}


#endif /* USE_X_TOOLKIT */


/* Value is an array of XColor structures for the contents of the
   color map of display DPY.  Set *NCELLS to the size of the array.
   Note that this probably shouldn't be called for large color maps,
   say a 24-bit TrueColor map.  */

static const XColor *
x_color_cells (Display *dpy, int *ncells)
{
  struct x_display_info *dpyinfo = x_dpyinfo (dpy);

  if (dpyinfo->color_cells == NULL)
    {
      int ncolor_cells = dpyinfo->visual_info.colormap_size;
      int i;

      dpyinfo->color_cells = xnmalloc (ncolor_cells,
				       sizeof *dpyinfo->color_cells);
      dpyinfo->ncolor_cells = ncolor_cells;

      for (i = 0; i < ncolor_cells; ++i)
	dpyinfo->color_cells[i].pixel = i;

      XQueryColors (dpy, dpyinfo->cmap,
		    dpyinfo->color_cells, ncolor_cells);
    }

  *ncells = dpyinfo->ncolor_cells;
  return dpyinfo->color_cells;
}


/* On frame F, translate pixel colors to RGB values for the NCOLORS
   colors in COLORS.  Use cached information, if available.

   Pixel values are in unsigned normalized format, meaning that
   extending missing bits is done straightforwardly without any
   complex colorspace conversions.  */

void
x_query_colors (struct frame *f, XColor *colors, int ncolors)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  int i;

  if (dpyinfo->red_bits > 0)
    {
      /* For TrueColor displays, we can decompose the RGB value
	 directly.  */
      unsigned int rmult, gmult, bmult;
      unsigned int rmask, gmask, bmask;

      rmask = (1 << dpyinfo->red_bits) - 1;
      gmask = (1 << dpyinfo->green_bits) - 1;
      bmask = (1 << dpyinfo->blue_bits) - 1;
      /* If we're widening, for example, 8 bits in the pixel value to
	 16 bits for the separate-color representation, we want to
	 extrapolate the lower bits based on those bits available --
	 in other words, we'd like 0xff to become 0xffff instead of
	 the 0xff00 we'd get by just zero-filling the lower bits.

         We generate a 32-bit scaled-up value and shift it, in case
         the bit count doesn't divide 16 evenly (e.g., when dealing
         with a 3-3-2 bit RGB display), to get more of the lower bits
         correct.

         Should we cache the multipliers in dpyinfo?  Maybe
         special-case the 8-8-8 common case?  */
      rmult = 0xffffffff / rmask;
      gmult = 0xffffffff / gmask;
      bmult = 0xffffffff / bmask;

      for (i = 0; i < ncolors; ++i)
	{
	  unsigned int r, g, b;
	  unsigned long pixel = colors[i].pixel;

	  r = (pixel >> dpyinfo->red_offset) & rmask;
	  g = (pixel >> dpyinfo->green_offset) & gmask;
	  b = (pixel >> dpyinfo->blue_offset) & bmask;

	  colors[i].red = (r * rmult) >> 16;
	  colors[i].green = (g * gmult) >> 16;
	  colors[i].blue = (b * bmult) >> 16;
	}

      return;
    }

  if (dpyinfo->color_cells)
    {
      int i;
      for (i = 0; i < ncolors; ++i)
	{
	  unsigned long pixel = colors[i].pixel;
	  eassert (pixel < dpyinfo->ncolor_cells);
	  eassert (dpyinfo->color_cells[pixel].pixel == pixel);
	  colors[i] = dpyinfo->color_cells[pixel];
	}
      return;
    }

  XQueryColors (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), colors, ncolors);
}

/* Store F's real background color into *BGCOLOR.  */

static void
x_query_frame_background_color (struct frame *f, XColor *bgcolor)
{
  unsigned long background = FRAME_BACKGROUND_PIXEL (f);
#ifndef USE_CAIRO
  XColor bg;
#endif

  if (FRAME_DISPLAY_INFO (f)->alpha_bits)
    {
#ifdef USE_CAIRO
      background = (background & ~FRAME_DISPLAY_INFO (f)->alpha_mask);
      background |= (((unsigned long) (f->alpha_background * 0xffff)
		      >> (16 - FRAME_DISPLAY_INFO (f)->alpha_bits))
		     << FRAME_DISPLAY_INFO (f)->alpha_offset);
#else
      if (FRAME_DISPLAY_INFO (f)->alpha_bits
	  && f->alpha_background < 1.0)
	{
	  bg.pixel = background;
	  x_query_colors (f, &bg, 1);

	  background
	    = x_premultiply_pixel (FRAME_DISPLAY_INFO (f),
				   &bg, f->alpha_background);
	}
#endif
    }

  bgcolor->pixel = background;

  x_query_colors (f, bgcolor, 1);
}

static unsigned int
x_hash_string_ignore_case (const char *string)
{
  unsigned int i;

  i = 3323198485ul;
  for (; *string; ++string)
    {
      i ^= c_tolower (*string);
      i *= 0x5bd1e995;
      i ^= i >> 15;
    }
  return i;
}

/* On frame F, translate the color name to RGB values.  Use cached
   information, if possible.

   If too many entries are placed in the cache, the least recently
   used entries are removed.  */

Status
x_parse_color (struct frame *f, const char *color_name,
	       XColor *color)
{
  unsigned short r, g, b;
  Display *dpy;
  Colormap cmap;
  struct x_display_info *dpyinfo;
  struct color_name_cache_entry *cache_entry, *last;
  struct color_name_cache_entry *next, *color_entry;
  unsigned int hash, idx;
  int rc, i;

  /* Don't pass #RGB strings directly to XParseColor, because that
     follows the X convention of zero-extending each channel
     value: #f00 means #f00000.  We want the convention of scaling
     channel values, so #f00 means #ff0000, just as it does for
     HTML, SVG, and CSS.  */
  if (parse_color_spec (color_name, &r, &g, &b))
    {
      color->red = r;
      color->green = g;
      color->blue = b;

      return 1;
    }

  /* Some X servers send BadValue on empty color names.  */
  if (!strlen (color_name))
    return 0;

  cmap = FRAME_X_COLORMAP (f);
  dpy = FRAME_X_DISPLAY (f);
  dpyinfo = FRAME_DISPLAY_INFO (f);

  hash = x_hash_string_ignore_case (color_name);
  idx = hash % dpyinfo->color_names_size;

  last = NULL;

  for (cache_entry = dpyinfo->color_names[idx];
       cache_entry; cache_entry = cache_entry->next)
    {
      if (!xstrcasecmp (cache_entry->name, color_name))
	{
	  /* Move recently used entries to the start of the color
	     cache.  */

	  if (last)
	    {
	      last->next = cache_entry->next;
	      cache_entry->next = dpyinfo->color_names[idx];

	      dpyinfo->color_names[idx] = cache_entry;
	    }

	  if (cache_entry->valid)
	    *color = cache_entry->rgb;

	  return cache_entry->valid;
	}

      last = cache_entry;
    }

  block_input ();
  rc = XParseColor (dpy, cmap, color_name, color);
  unblock_input ();

  cache_entry = xzalloc (sizeof *cache_entry);
  dpyinfo->color_names_length[idx] += 1;

  if (rc)
    cache_entry->rgb = *color;

  cache_entry->valid = rc;
  cache_entry->name = xstrdup (color_name);
  cache_entry->next = dpyinfo->color_names[idx];

  dpyinfo->color_names[idx] = cache_entry;

  /* Don't let the color cache become too big.  */
  if (dpyinfo->color_names_length[idx] > (x_color_cache_bucket_size > 0
					  ? x_color_cache_bucket_size : 128))
    {
      i = 0;

      for (last = dpyinfo->color_names[idx]; last; last = last->next)
	{
	  if (++i == (x_color_cache_bucket_size > 0
		      ? x_color_cache_bucket_size : 128))
	    {
	      next = last->next;
	      last->next = NULL;

	      for (color_entry = next; color_entry; color_entry = last)
		{
		  last = color_entry->next;

		  xfree (color_entry->name);
		  xfree (color_entry);

		  dpyinfo->color_names_length[idx] -= 1;
		}

	      return rc;
	    }
	}
    }

  return rc;
}


/* Allocate the color COLOR->pixel on DISPLAY, colormap CMAP.  If an
   exact match can't be allocated, try the nearest color available.
   Value is true if successful.  Set *COLOR to the color
   allocated.  */

static bool
x_alloc_nearest_color_1 (Display *dpy, Colormap cmap, XColor *color)
{
  struct x_display_info *dpyinfo = x_dpyinfo (dpy);
  bool rc = XAllocColor (dpy, cmap, color) != 0;

  if (dpyinfo->visual_info.class == DirectColor)
    return rc;

  if (!rc)
    {
      /* If we got to this point, the colormap is full, so we're going
	 to try and get the next closest color.  The algorithm used is
	 a least-squares matching, which is what X uses for closest
	 color matching with StaticColor visuals.  */

      const XColor *cells;
      int no_cells;
      int nearest;
      long nearest_delta, trial_delta;
      int x;
      Status status;
      bool retry = false;
      int ncolor_cells, i;
      bool temp_allocated;
      XColor temp;

    start:
      cells = x_color_cells (dpy, &no_cells);
      temp_allocated = false;

      nearest = 0;
      /* I'm assuming CSE so I'm not going to condense this. */
      nearest_delta = ((((color->red >> 8) - (cells[0].red >> 8))
			* ((color->red >> 8) - (cells[0].red >> 8)))
		       + (((color->green >> 8) - (cells[0].green >> 8))
			  * ((color->green >> 8) - (cells[0].green >> 8)))
		       + (((color->blue >> 8) - (cells[0].blue >> 8))
			  * ((color->blue >> 8) - (cells[0].blue >> 8))));
      for (x = 1; x < no_cells; x++)
	{
	  trial_delta = ((((color->red >> 8) - (cells[x].red >> 8))
			  * ((color->red >> 8) - (cells[x].red >> 8)))
			 + (((color->green >> 8) - (cells[x].green >> 8))
			    * ((color->green >> 8) - (cells[x].green >> 8)))
			 + (((color->blue >> 8) - (cells[x].blue >> 8))
			    * ((color->blue >> 8) - (cells[x].blue >> 8))));
	  if (trial_delta < nearest_delta)
	    {
	      /* We didn't decide to use this color, so free it.  */
	      if (temp_allocated)
		{
		  XFreeColors (dpy, cmap, &temp.pixel, 1, 0);
		  temp_allocated = false;
		}

	      temp.red = cells[x].red;
	      temp.green = cells[x].green;
	      temp.blue = cells[x].blue;
	      status = XAllocColor (dpy, cmap, &temp);

	      if (status)
		{
		  temp_allocated = true;
		  nearest = x;
		  nearest_delta = trial_delta;
		}
	    }
	}
      color->red = cells[nearest].red;
      color->green = cells[nearest].green;
      color->blue = cells[nearest].blue;

      if (!temp_allocated)
	status = XAllocColor (dpy, cmap, color);
      else
	{
	  *color = temp;
	  status = 1;
	}

      if (status == 0 && !retry)
	{
	  /* Our private cache of color cells is probably out of date.
	     Refresh it here, and try to allocate the nearest color
	     from the new colormap.  */

	  retry = true;
	  xfree (dpyinfo->color_cells);

	  ncolor_cells = dpyinfo->visual_info.colormap_size;

	  dpyinfo->color_cells = xnmalloc (ncolor_cells,
					   sizeof *dpyinfo->color_cells);
	  dpyinfo->ncolor_cells = ncolor_cells;

	  for (i = 0; i < ncolor_cells; ++i)
	    dpyinfo->color_cells[i].pixel = i;

	  XQueryColors (dpy, dpyinfo->cmap,
			dpyinfo->color_cells, ncolor_cells);

	  goto start;
	}

      rc = status != 0;
    }
  else
    {
      /* If allocation succeeded, and the allocated pixel color is not
         equal to a cached pixel color recorded earlier, there was a
         change in the colormap, so clear the color cache.  */
      struct x_display_info *dpyinfo = x_dpyinfo (dpy);

      if (dpyinfo->color_cells)
	{
	  XColor *cached_color = &dpyinfo->color_cells[color->pixel];
	  if (cached_color->red != color->red
	      || cached_color->blue != color->blue
	      || cached_color->green != color->green)
	    {
	      xfree (dpyinfo->color_cells);
	      dpyinfo->color_cells = NULL;
	      dpyinfo->ncolor_cells = 0;
	    }
	}
    }

#ifdef DEBUG_X_COLORS
  if (rc)
    register_color (color->pixel);
#endif /* DEBUG_X_COLORS */

  return rc;
}


/* Allocate the color COLOR->pixel on frame F, colormap CMAP, after
   gamma correction.  If an exact match can't be allocated, try the
   nearest color available.  Value is true if successful.  Set *COLOR
   to the color allocated.  */

bool
x_alloc_nearest_color (struct frame *f, Colormap cmap, XColor *color)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  gamma_correct (f, color);

  if (dpyinfo->red_bits > 0)
    {
      color->pixel = x_make_truecolor_pixel (dpyinfo,
					     color->red,
					     color->green,
					     color->blue);
      return true;
    }

  return x_alloc_nearest_color_1 (FRAME_X_DISPLAY (f), cmap, color);
}


/* Allocate color PIXEL on frame F.  PIXEL must already be allocated.
   It's necessary to do this instead of just using PIXEL directly to
   get color reference counts right.  */

unsigned long
x_copy_color (struct frame *f, unsigned long pixel)
{
  XColor color;

  /* If display has an immutable color map, freeing colors is not
     necessary and some servers don't allow it.  Since we won't free a
     color once we've allocated it, we don't need to re-allocate it to
     maintain the server's reference count.  */
  if (!x_mutable_colormap (FRAME_X_VISUAL_INFO (f)))
    return pixel;

  color.pixel = pixel;
  block_input ();
  /* The color could still be found in the color_cells array.  */
  x_query_colors (f, &color, 1);
  XAllocColor (FRAME_X_DISPLAY (f), FRAME_X_COLORMAP (f), &color);
  unblock_input ();
#ifdef DEBUG_X_COLORS
  register_color (pixel);
#endif
  return color.pixel;
}


/* Brightness beyond which a color won't have its highlight brightness
   boosted.

   Nominally, highlight colors for `3d' faces are calculated by
   brightening an object's color by a constant scale factor, but this
   doesn't yield good results for dark colors, so for colors who's
   brightness is less than this value (on a scale of 0-65535) have an
   use an additional additive factor.

   The value here is set so that the default menu-bar/mode-line color
   (grey75) will not have its highlights changed at all.  */
#define HIGHLIGHT_COLOR_DARK_BOOST_LIMIT 48000


/* Allocate a color which is lighter or darker than *PIXEL by FACTOR
   or DELTA.  Try a color with RGB values multiplied by FACTOR first.
   If this produces the same color as PIXEL, try a color where all RGB
   values have DELTA added.  Return the allocated color in *PIXEL.
   DISPLAY is the X display, CMAP is the colormap to operate on.
   Value is non-zero if successful.  */

static bool
x_alloc_lighter_color (struct frame *f, Display *display, Colormap cmap,
		       unsigned long *pixel, double factor, int delta)
{
  XColor color, new;
  long bright;
  bool success_p;

  /* Get RGB color values.  */
  color.pixel = *pixel;
  x_query_colors (f, &color, 1);

  /* Change RGB values by specified FACTOR.  Avoid overflow!  */
  eassert (factor >= 0);
  new.red = min (0xffff, factor * color.red);
  new.green = min (0xffff, factor * color.green);
  new.blue = min (0xffff, factor * color.blue);

  /* Calculate brightness of COLOR.  */
  bright = (2 * color.red + 3 * color.green + color.blue) / 6;

  /* We only boost colors that are darker than
     HIGHLIGHT_COLOR_DARK_BOOST_LIMIT.  */
  if (bright < HIGHLIGHT_COLOR_DARK_BOOST_LIMIT)
    /* Make an additive adjustment to NEW, because it's dark enough so
       that scaling by FACTOR alone isn't enough.  */
    {
      /* How far below the limit this color is (0 - 1, 1 being darker).  */
      double dimness = 1 - (double) bright / HIGHLIGHT_COLOR_DARK_BOOST_LIMIT;
      /* The additive adjustment.  */
      int min_delta = delta * dimness * factor / 2;

      if (factor < 1)
	{
	  new.red =   max (0, new.red -   min_delta);
	  new.green = max (0, new.green - min_delta);
	  new.blue =  max (0, new.blue -  min_delta);
	}
      else
	{
	  new.red =   min (0xffff, min_delta + new.red);
	  new.green = min (0xffff, min_delta + new.green);
	  new.blue =  min (0xffff, min_delta + new.blue);
	}
    }

  /* Try to allocate the color.  */
  success_p = x_alloc_nearest_color (f, cmap, &new);
  if (success_p)
    {
      if (new.pixel == *pixel)
	{
	  /* If we end up with the same color as before, try adding
	     delta to the RGB values.  */
	  x_free_colors (f, &new.pixel, 1);

	  new.red = min (0xffff, delta + color.red);
	  new.green = min (0xffff, delta + color.green);
	  new.blue = min (0xffff, delta + color.blue);
	  success_p = x_alloc_nearest_color (f, cmap, &new);
	}
      else
	success_p = true;
      *pixel = new.pixel;
    }

  return success_p;
}


/* Set up the foreground color for drawing relief lines of glyph
   string S.  RELIEF is a pointer to a struct relief containing the GC
   with which lines will be drawn.  Use a color that is FACTOR or
   DELTA lighter or darker than the relief's background which is found
   in S->f->output_data.x->relief_background.  If such a color cannot
   be allocated, use DEFAULT_PIXEL, instead.  */

static void
x_setup_relief_color (struct frame *f, struct relief *relief, double factor,
		      int delta, unsigned long default_pixel)
{
  XGCValues xgcv;
  struct x_output *di = f->output_data.x;
  unsigned long mask = GCForeground | GCLineWidth | GCGraphicsExposures;
  unsigned long pixel;
  unsigned long background = di->relief_background;
  Colormap cmap = FRAME_X_COLORMAP (f);
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Display *dpy = FRAME_X_DISPLAY (f);

  xgcv.graphics_exposures = False;
  xgcv.line_width = 1;

  /* Free previously allocated color.  The color cell will be reused
     when it has been freed as many times as it was allocated, so this
     doesn't affect faces using the same colors.  */
  if (relief->gc && relief->pixel != -1)
    {
      x_free_colors (f, &relief->pixel, 1);
      relief->pixel = -1;
    }

  /* Allocate new color.  */
  xgcv.foreground = default_pixel;
  pixel = background;
  if (dpyinfo->n_planes != 1
      && x_alloc_lighter_color (f, dpy, cmap, &pixel, factor, delta))
    xgcv.foreground = relief->pixel = pixel;

  if (relief->gc == 0)
    {
      xgcv.stipple = dpyinfo->gray;
      mask |= GCStipple;
      relief->gc = XCreateGC (dpy, FRAME_X_DRAWABLE (f), mask, &xgcv);
    }
  else
    XChangeGC (dpy, relief->gc, mask, &xgcv);
}


/* Set up colors for the relief lines around glyph string S.  */

static void
x_setup_relief_colors (struct glyph_string *s)
{
  struct x_output *di = s->f->output_data.x;
  unsigned long color;

  if (s->face->use_box_color_for_shadows_p)
    color = s->face->box_color;
  else if (s->first_glyph->type == IMAGE_GLYPH
	   && s->img->pixmap
	   && !IMAGE_BACKGROUND_TRANSPARENT (s->img, s->f, 0))
    color = IMAGE_BACKGROUND (s->img, s->f, 0);
  else
    {
      XGCValues xgcv;

      /* Get the background color of the face.  */
      XGetGCValues (FRAME_X_DISPLAY (s->f), s->gc, GCBackground, &xgcv);
      color = xgcv.background;
    }

  if (di->white_relief.gc == 0
      || color != di->relief_background)
    {
      di->relief_background = color;
      x_setup_relief_color (s->f, &di->white_relief, 1.2, 0x8000,
			    WHITE_PIX_DEFAULT (s->f));
      x_setup_relief_color (s->f, &di->black_relief, 0.6, 0x4000,
			    BLACK_PIX_DEFAULT (s->f));
    }
}

#ifndef USE_CAIRO
static void
x_fill_triangle (struct frame *f, GC gc, XPoint point1,
		 XPoint point2, XPoint point3)
{
  XPoint abc[3];

  abc[0] = point1;
  abc[1] = point2;
  abc[2] = point3;

  XFillPolygon (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f),
		gc, abc, 3, Convex, CoordModeOrigin);
}

static XPoint
x_make_point (int x, int y)
{
  XPoint pt;

  pt.x = x;
  pt.y = y;

  return pt;
}

static bool
x_inside_rect_p (XRectangle *rects, int nrects, int x, int y)
{
  int i;

  for (i = 0; i < nrects; ++i)
    {
      if (x >= rects[i].x && y >= rects[i].y
	  && x < rects[i].x + rects[i].width
	  && y < rects[i].y + rects[i].height)
	return true;
    }

  return false;
}
#endif

/* Draw a relief on frame F inside the rectangle given by LEFT_X,
   TOP_Y, RIGHT_X, and BOTTOM_Y.  VWIDTH and HWIDTH are respectively
   the thickness of the vertical relief (left and right) and
   horizontal relief (top and bottom) to draw, it must be >= 0.
   RAISED_P means draw a raised relief.  LEFT_P means draw a relief on
   the left side of the rectangle.  RIGHT_P means draw a relief on the
   right side of the rectangle.  CLIP_RECT is the clipping rectangle
   to use when drawing.  */

static void
x_draw_relief_rect (struct frame *f, int left_x, int top_y, int right_x,
		    int bottom_y, int hwidth, int vwidth, bool raised_p,
		    bool top_p, bool bot_p, bool left_p, bool right_p,
		    XRectangle *clip_rect)
{
#ifdef USE_CAIRO
  GC top_left_gc, bottom_right_gc;
  int corners = 0;

  if (raised_p)
    {
      top_left_gc = f->output_data.x->white_relief.gc;
      bottom_right_gc = f->output_data.x->black_relief.gc;
    }
  else
    {
      top_left_gc = f->output_data.x->black_relief.gc;
      bottom_right_gc = f->output_data.x->white_relief.gc;
    }

  x_set_clip_rectangles (f, top_left_gc, clip_rect, 1);
  x_set_clip_rectangles (f, bottom_right_gc, clip_rect, 1);

  if (left_p)
    {
      x_fill_rectangle (f, top_left_gc, left_x, top_y,
			vwidth, bottom_y + 1 - top_y, false);
      if (top_p)
	corners |= 1 << CORNER_TOP_LEFT;
      if (bot_p)
	corners |= 1 << CORNER_BOTTOM_LEFT;
    }
  if (right_p)
    {
      x_fill_rectangle (f, bottom_right_gc, right_x + 1 - vwidth, top_y,
			vwidth, bottom_y + 1 - top_y, false);
      if (top_p)
	corners |= 1 << CORNER_TOP_RIGHT;
      if (bot_p)
	corners |= 1 << CORNER_BOTTOM_RIGHT;
    }
  if (top_p)
    {
      if (!right_p)
	x_fill_rectangle (f, top_left_gc, left_x, top_y,
			  right_x + 1 - left_x, hwidth, false);
      else
	x_fill_trapezoid_for_relief (f, top_left_gc, left_x, top_y,
				     right_x + 1 - left_x, hwidth, 1);
    }
  if (bot_p)
    {
      if (!left_p)
	x_fill_rectangle (f, bottom_right_gc, left_x, bottom_y + 1 - hwidth,
			  right_x + 1 - left_x, hwidth, false);
      else
	x_fill_trapezoid_for_relief (f, bottom_right_gc,
				     left_x, bottom_y + 1 - hwidth,
				     right_x + 1 - left_x, hwidth, 0);
    }
  if (left_p && vwidth > 1)
    x_fill_rectangle (f, bottom_right_gc, left_x, top_y,
		      1, bottom_y + 1 - top_y, false);
  if (top_p && hwidth > 1)
    x_fill_rectangle (f, bottom_right_gc, left_x, top_y,
		      right_x + 1 - left_x, 1, false);
  if (corners)
    {
      XSetBackground (FRAME_X_DISPLAY (f), top_left_gc,
		      FRAME_BACKGROUND_PIXEL (f));
      x_erase_corners_for_relief (f, top_left_gc, left_x, top_y,
				  right_x - left_x + 1, bottom_y - top_y + 1,
				  6, 1, corners);
    }

  x_reset_clip_rectangles (f, top_left_gc);
  x_reset_clip_rectangles (f, bottom_right_gc);
#else
  GC gc, white_gc, black_gc, normal_gc;
  Drawable drawable;
  Display *dpy;

  /* This code is more complicated than it has to be, because of two
     minor hacks to make the boxes look nicer: (i) if width > 1, draw
     the outermost line using the black relief.  (ii) Omit the four
     corner pixels.  */

  white_gc = f->output_data.x->white_relief.gc;
  black_gc = f->output_data.x->black_relief.gc;
  normal_gc = f->output_data.x->normal_gc;

  drawable = FRAME_X_DRAWABLE (f);
  dpy = FRAME_X_DISPLAY (f);

  x_set_clip_rectangles (f, white_gc, clip_rect, 1);
  x_set_clip_rectangles (f, black_gc, clip_rect, 1);

  if (raised_p)
    gc = white_gc;
  else
    gc = black_gc;

  /* Draw lines.  */

  if (top_p)
    x_fill_rectangle (f, gc, left_x, top_y,
		      right_x - left_x + 1, hwidth,
		      false);

  if (left_p)
    x_fill_rectangle (f, gc, left_x, top_y, vwidth,
		      bottom_y - top_y + 1, false);

  if (raised_p)
    gc = black_gc;
  else
    gc = white_gc;

  if (bot_p)
    x_fill_rectangle (f, gc, left_x, bottom_y - hwidth + 1,
		      right_x - left_x + 1, hwidth, false);

  if (right_p)
    x_fill_rectangle (f, gc, right_x - vwidth + 1, top_y,
		      vwidth, bottom_y - top_y + 1, false);

  /* Draw corners.  */

  if (bot_p && left_p)
    x_fill_triangle (f, raised_p ? white_gc : black_gc,
		     x_make_point (left_x, bottom_y - hwidth),
		     x_make_point (left_x + vwidth, bottom_y - hwidth),
		     x_make_point (left_x, bottom_y));

  if (top_p && right_p)
    x_fill_triangle (f, raised_p ? white_gc : black_gc,
		     x_make_point (right_x - vwidth, top_y),
		     x_make_point (right_x, top_y),
		     x_make_point (right_x - vwidth, top_y + hwidth));

  /* Draw outer line.  */

  if (top_p && left_p && bot_p && right_p
      && hwidth > 1 && vwidth > 1)
    x_draw_rectangle (f, black_gc, left_x, top_y,
		      right_x - left_x, bottom_y - top_y);
  else
    {
      if (top_p && hwidth > 1)
	XDrawLine (dpy, drawable, black_gc, left_x, top_y,
		   right_x + 1, top_y);

      if (bot_p && hwidth > 1)
	XDrawLine (dpy, drawable, black_gc, left_x, bottom_y,
		   right_x + 1, bottom_y);

      if (left_p && vwidth > 1)
	XDrawLine (dpy, drawable, black_gc, left_x, top_y,
		   left_x, bottom_y + 1);

      if (right_p && vwidth > 1)
	XDrawLine (dpy, drawable, black_gc, right_x, top_y,
		   right_x, bottom_y + 1);
    }

  /* Erase corners.  */

  if (hwidth > 1 && vwidth > 1)
    {
      if (left_p && top_p && x_inside_rect_p (clip_rect, 1,
					      left_x, top_y))
	/* This should respect `alpha-background' since it's being
	   cleared with the background color of the frame.  */
	x_clear_point (f, normal_gc, left_x, top_y, true);

      if (left_p && bot_p && x_inside_rect_p (clip_rect, 1,
					      left_x, bottom_y))
	x_clear_point (f, normal_gc, left_x, bottom_y, true);

      if (right_p && top_p && x_inside_rect_p (clip_rect, 1,
					       right_x, top_y))
	x_clear_point (f, normal_gc, right_x, top_y, true);

      if (right_p && bot_p && x_inside_rect_p (clip_rect, 1,
					       right_x, bottom_y))
	x_clear_point (f, normal_gc, right_x, bottom_y, true);
    }

  x_reset_clip_rectangles (f, white_gc);
  x_reset_clip_rectangles (f, black_gc);
#endif
}


/* Draw a box on frame F inside the rectangle given by LEFT_X, TOP_Y,
   RIGHT_X, and BOTTOM_Y.  WIDTH is the thickness of the lines to
   draw, it must be >= 0.  LEFT_P means draw a line on the
   left side of the rectangle.  RIGHT_P means draw a line
   on the right side of the rectangle.  CLIP_RECT is the clipping
   rectangle to use when drawing.  */

static void
x_draw_box_rect (struct glyph_string *s,
		 int left_x, int top_y, int right_x, int bottom_y, int hwidth,
		 int vwidth, bool left_p, bool right_p, XRectangle *clip_rect)
{
  Display *display = FRAME_X_DISPLAY (s->f);
  XGCValues xgcv;

  XGetGCValues (display, s->gc, GCForeground, &xgcv);
  XSetForeground (display, s->gc, s->face->box_color);
  x_set_clip_rectangles (s->f, s->gc, clip_rect, 1);

  /* Top.  */
  x_fill_rectangle (s->f, s->gc,
		    left_x, top_y, right_x - left_x + 1, hwidth,
		    false);

  /* Left.  */
  if (left_p)
    x_fill_rectangle (s->f, s->gc,
		      left_x, top_y, vwidth, bottom_y - top_y + 1,
		      false);

  /* Bottom.  */
  x_fill_rectangle (s->f, s->gc,
		    left_x, bottom_y - hwidth + 1, right_x - left_x + 1, hwidth,
		    false);

  /* Right.  */
  if (right_p)
    x_fill_rectangle (s->f, s->gc,
		      right_x - vwidth + 1, top_y, vwidth, bottom_y - top_y + 1,
		      false);

  XSetForeground (display, s->gc, xgcv.foreground);
  x_reset_clip_rectangles (s->f, s->gc);
}


/* Draw a box around glyph string S.  */

static void
x_draw_glyph_string_box (struct glyph_string *s)
{
  int hwidth, vwidth, left_x, right_x, top_y, bottom_y, last_x;
  bool raised_p, left_p, right_p;
  struct glyph *last_glyph;
  XRectangle clip_rect;

  last_x = ((s->row->full_width_p && !s->w->pseudo_window_p)
	    ? WINDOW_RIGHT_EDGE_X (s->w)
	    : window_box_right (s->w, s->area));

  /* The glyph that may have a right box line.  For static
     compositions and images, the right-box flag is on the first glyph
     of the glyph string; for other types it's on the last glyph.  */
  if (s->cmp || s->img)
    last_glyph = s->first_glyph;
  else if (s->first_glyph->type == COMPOSITE_GLYPH
	   && s->first_glyph->u.cmp.automatic)
    {
      /* For automatic compositions, we need to look up the last glyph
	 in the composition.  */
        struct glyph *end = s->row->glyphs[s->area] + s->row->used[s->area];
	struct glyph *g = s->first_glyph;
	for (last_glyph = g++;
	     g < end && g->u.cmp.automatic && g->u.cmp.id == s->cmp_id
	       && g->slice.cmp.to < s->cmp_to;
	     last_glyph = g++)
	  ;
    }
  else
    last_glyph = s->first_glyph + s->nchars - 1;

  vwidth = eabs (s->face->box_vertical_line_width);
  hwidth = eabs (s->face->box_horizontal_line_width);
  raised_p = s->face->box == FACE_RAISED_BOX;
  left_x = s->x;
  right_x = (s->row->full_width_p && s->extends_to_end_of_line_p
	     ? last_x - 1
	     : min (last_x, s->x + s->background_width) - 1);
  top_y = s->y;
  bottom_y = top_y + s->height - 1;

  left_p = (s->first_glyph->left_box_line_p
	    || (s->hl == DRAW_MOUSE_FACE
		&& (s->prev == NULL
		    || s->prev->hl != s->hl)));
  right_p = (last_glyph->right_box_line_p
	     || (s->hl == DRAW_MOUSE_FACE
		 && (s->next == NULL
		     || s->next->hl != s->hl)));

  get_glyph_string_clip_rect (s, &clip_rect);

  if (s->face->box == FACE_SIMPLE_BOX)
    x_draw_box_rect (s, left_x, top_y, right_x, bottom_y, hwidth,
		     vwidth, left_p, right_p, &clip_rect);
  else
    {
      x_setup_relief_colors (s);
      x_draw_relief_rect (s->f, left_x, top_y, right_x, bottom_y, hwidth,
			  vwidth, raised_p, true, true, left_p, right_p,
			  &clip_rect);
    }
}


#ifndef USE_CAIRO

static void
x_composite_image (struct glyph_string *s, Pixmap dest,
#ifdef HAVE_XRENDER
		   Picture destination,
#endif
                   int srcX, int srcY, int dstX, int dstY,
                   int width, int height)
{
  Display *display;
#ifdef HAVE_XRENDER
  XRenderPictFormat *default_format;
  XRenderPictureAttributes attr UNINIT;
#endif

  display = FRAME_X_DISPLAY (s->f);

#ifdef HAVE_XRENDER
  if (s->img->picture && FRAME_X_PICTURE_FORMAT (s->f))
    {
      if (destination == None)
	{
	  /* The destination picture was not specified.  This means we
	     have to create a picture representing the */
	  default_format = FRAME_X_PICTURE_FORMAT (s->f);
	  destination = XRenderCreatePicture (display, dest,
					      default_format, 0, &attr);

	  XRenderComposite (display, (s->img->mask_picture
				      ? PictOpOver : PictOpSrc),
			    s->img->picture, s->img->mask_picture,
			    destination, srcX, srcY, srcX, srcY,
			    dstX, dstY, width, height);

	  XRenderFreePicture (display, destination);
	}
      else
	XRenderComposite (display, (s->img->mask_picture
				    ? PictOpOver : PictOpSrc),
			  s->img->picture, s->img->mask_picture,
			  destination, srcX, srcY, srcX, srcY,
			  dstX, dstY, width, height);

      return;
    }
#endif

  XCopyArea (display, s->img->pixmap,
	     dest, s->gc,
	     srcX, srcY,
	     width, height, dstX, dstY);
}

#endif	/* !USE_CAIRO */


/* Draw foreground of image glyph string S.  */

static void
x_draw_image_foreground (struct glyph_string *s)
{
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += max (s->face->box_vertical_line_width, 0);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

#ifdef USE_CAIRO
  if (s->img->cr_data)
    {
      x_set_glyph_string_clipping (s);
      x_cr_draw_image (s->f, s->gc, s->img->cr_data,
		       s->slice.x, s->slice.y, s->slice.width, s->slice.height,
		       x, y, true);
      if (!s->img->mask)
	{
	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    {
	      int relief = eabs (s->img->relief);
	      x_draw_rectangle (s->f, s->gc, x - relief, y - relief,
				s->slice.width + relief*2 - 1,
				s->slice.height + relief*2 - 1);
	    }
	}
    }
#else  /* ! USE_CAIRO */
  if (s->img->pixmap)
    {
      if (s->img->mask)
	{
	  /* We can't set both a clip mask and use XSetClipRectangles
	     because the latter also sets a clip mask.  We also can't
	     trust on the shape extension to be available
	     (XShapeCombineRegion).  So, compute the rectangle to draw
	     manually.  */
          /* FIXME: Do we need to do this when using XRender compositing?  */
	  unsigned long mask = (GCClipMask | GCClipXOrigin | GCClipYOrigin
				| GCFunction);
	  XGCValues xgcv;
	  XRectangle clip_rect, image_rect, r;

	  xgcv.clip_mask = s->img->mask;
	  xgcv.clip_x_origin = x;
	  xgcv.clip_y_origin = y;
	  xgcv.function = GXcopy;
	  XChangeGC (FRAME_X_DISPLAY (s->f), s->gc, mask, &xgcv);

	  get_glyph_string_clip_rect (s, &clip_rect);
	  image_rect.x = x;
	  image_rect.y = y;
	  image_rect.width = s->slice.width;
	  image_rect.height = s->slice.height;
	  if (gui_intersect_rectangles (&clip_rect, &image_rect, &r))
            x_composite_image (s, FRAME_X_DRAWABLE (s->f),
#ifdef HAVE_XRENDER
			       FRAME_X_PICTURE (s->f),
#endif
			       s->slice.x + r.x - x, s->slice.y + r.y - y,
                               r.x, r.y, r.width, r.height);
	}
      else
	{
	  XRectangle clip_rect, image_rect, r;

	  get_glyph_string_clip_rect (s, &clip_rect);
	  image_rect.x = x;
	  image_rect.y = y;
	  image_rect.width = s->slice.width;
	  image_rect.height = s->slice.height;
	  if (gui_intersect_rectangles (&clip_rect, &image_rect, &r))
            x_composite_image (s, FRAME_X_DRAWABLE (s->f),
#ifdef HAVE_XRENDER
			       FRAME_X_PICTURE (s->f),
#endif
			       s->slice.x + r.x - x,
			       s->slice.y + r.y - y,
                               r.x, r.y, r.width, r.height);

	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    {
	      int relief = eabs (s->img->relief);
	      x_draw_rectangle (s->f, s->gc,
			      x - relief, y - relief,
			      s->slice.width + relief*2 - 1,
			      s->slice.height + relief*2 - 1);
	    }
	}
    }
#endif	/* ! USE_CAIRO */
  else
    /* Draw a rectangle if image could not be loaded.  */
    x_draw_rectangle (s->f, s->gc, x, y,
		    s->slice.width - 1, s->slice.height - 1);
}


/* Draw a relief around the image glyph string S.  */

static void
x_draw_image_relief (struct glyph_string *s)
{
  int x1, y1, thick;
  bool raised_p, top_p, bot_p, left_p, right_p;
  int extra_x, extra_y;
  XRectangle r;
  int x = s->x;
  int y = s->ybase - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += max (s->face->box_vertical_line_width, 0);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->hl == DRAW_IMAGE_SUNKEN
      || s->hl == DRAW_IMAGE_RAISED)
    {
      if (s->face->id == TAB_BAR_FACE_ID)
	thick = (tab_bar_button_relief < 0
		 ? DEFAULT_TAB_BAR_BUTTON_RELIEF
		 : min (tab_bar_button_relief, 1000000));
      else
	thick = (tool_bar_button_relief < 0
		 ? DEFAULT_TOOL_BAR_BUTTON_RELIEF
		 : min (tool_bar_button_relief, 1000000));
      raised_p = s->hl == DRAW_IMAGE_RAISED;
    }
  else
    {
      thick = eabs (s->img->relief);
      raised_p = s->img->relief > 0;
    }

  x1 = x + s->slice.width - 1;
  y1 = y + s->slice.height - 1;

  extra_x = extra_y = 0;
  if (s->face->id == TAB_BAR_FACE_ID)
    {
      if (CONSP (Vtab_bar_button_margin)
	  && FIXNUMP (XCAR (Vtab_bar_button_margin))
	  && FIXNUMP (XCDR (Vtab_bar_button_margin)))
	{
	  extra_x = XFIXNUM (XCAR (Vtab_bar_button_margin)) - thick;
	  extra_y = XFIXNUM (XCDR (Vtab_bar_button_margin)) - thick;
	}
      else if (FIXNUMP (Vtab_bar_button_margin))
	extra_x = extra_y = XFIXNUM (Vtab_bar_button_margin) - thick;
    }

  if (s->face->id == TOOL_BAR_FACE_ID)
    {
      if (CONSP (Vtool_bar_button_margin)
	  && FIXNUMP (XCAR (Vtool_bar_button_margin))
	  && FIXNUMP (XCDR (Vtool_bar_button_margin)))
	{
	  extra_x = XFIXNUM (XCAR (Vtool_bar_button_margin));
	  extra_y = XFIXNUM (XCDR (Vtool_bar_button_margin));
	}
      else if (FIXNUMP (Vtool_bar_button_margin))
	extra_x = extra_y = XFIXNUM (Vtool_bar_button_margin);
    }

  top_p = bot_p = left_p = right_p = false;

  if (s->slice.x == 0)
    x -= thick + extra_x, left_p = true;
  if (s->slice.y == 0)
    y -= thick + extra_y, top_p = true;
  if (s->slice.x + s->slice.width == s->img->width)
    x1 += thick + extra_x, right_p = true;
  if (s->slice.y + s->slice.height == s->img->height)
    y1 += thick + extra_y, bot_p = true;

  x_setup_relief_colors (s);
  get_glyph_string_clip_rect (s, &r);
  x_draw_relief_rect (s->f, x, y, x1, y1, thick, thick, raised_p,
		      top_p, bot_p, left_p, right_p, &r);
}


#ifndef USE_CAIRO
/* Draw the foreground of image glyph string S to PIXMAP.  */

static void
x_draw_image_foreground_1 (struct glyph_string *s, Pixmap pixmap)
{
  int x = 0;
  int y = s->ybase - s->y - image_ascent (s->img, s->face, &s->slice);

  /* If first glyph of S has a left box line, start drawing it to the
     right of that line.  */
  if (s->face->box != FACE_NO_BOX
      && s->first_glyph->left_box_line_p
      && s->slice.x == 0)
    x += max (s->face->box_vertical_line_width, 0);

  /* If there is a margin around the image, adjust x- and y-position
     by that margin.  */
  if (s->slice.x == 0)
    x += s->img->hmargin;
  if (s->slice.y == 0)
    y += s->img->vmargin;

  if (s->img->pixmap)
    {
      Display *display = FRAME_X_DISPLAY (s->f);

      if (s->img->mask)
	{
	  /* We can't set both a clip mask and use XSetClipRectangles
	     because the latter also sets a clip mask.  We also can't
	     trust on the shape extension to be available
	     (XShapeCombineRegion).  So, compute the rectangle to draw
	     manually.  */
          /* FIXME: Do we need to do this when using XRender compositing?  */
	  unsigned long mask = (GCClipMask | GCClipXOrigin | GCClipYOrigin
				| GCFunction);
	  XGCValues xgcv;

	  xgcv.clip_mask = s->img->mask;
	  xgcv.clip_x_origin = x - s->slice.x;
	  xgcv.clip_y_origin = y - s->slice.y;
	  xgcv.function = GXcopy;
	  XChangeGC (display, s->gc, mask, &xgcv);

	  x_composite_image (s, pixmap,
#ifdef HAVE_XRENDER
			     None,
#endif
                             s->slice.x, s->slice.y, x, y,
			     s->slice.width, s->slice.height);
	  XSetClipMask (display, s->gc, None);
	}
      else
	{
	  XCopyArea (display, s->img->pixmap, pixmap, s->gc,
		     s->slice.x, s->slice.y,
		     s->slice.width, s->slice.height, x, y);

	  /* When the image has a mask, we can expect that at
	     least part of a mouse highlight or a block cursor will
	     be visible.  If the image doesn't have a mask, make
	     a block cursor visible by drawing a rectangle around
	     the image.  I believe it's looking better if we do
	     nothing here for mouse-face.  */
	  if (s->hl == DRAW_CURSOR)
	    {
	      int r = eabs (s->img->relief);
	      x_draw_rectangle (s->f, s->gc, x - r, y - r,
			      s->slice.width + r*2 - 1,
			      s->slice.height + r*2 - 1);
	    }
	}
    }
  else
    /* Draw a rectangle if image could not be loaded.  */
    x_draw_rectangle (s->f, s->gc, x, y,
		    s->slice.width - 1, s->slice.height - 1);
}
#endif	/* ! USE_CAIRO */


/* Draw part of the background of glyph string S.  X, Y, W, and H
   give the rectangle to draw.  */

static void
x_draw_glyph_string_bg_rect (struct glyph_string *s, int x, int y, int w, int h)
{
  if (s->stippled_p)
    {
      Display *display = FRAME_X_DISPLAY (s->f);

      /* Fill background with a stipple pattern.  */
      XSetFillStyle (display, s->gc, FillOpaqueStippled);
      x_fill_rectangle (s->f, s->gc, x, y, w, h, true);
      XSetFillStyle (display, s->gc, FillSolid);
    }
  else
    x_clear_glyph_string_rect (s, x, y, w, h);
}


/* Draw image glyph string S.

            s->y
   s->x      +-------------------------
	     |   s->face->box
	     |
	     |     +-------------------------
	     |     |  s->img->margin
	     |     |
	     |     |       +-------------------
	     |     |       |  the image

 */

static void
x_draw_image_glyph_string (struct glyph_string *s)
{
  int box_line_hwidth = max (s->face->box_vertical_line_width, 0);
  int box_line_vwidth = max (s->face->box_horizontal_line_width, 0);
  int height;
#ifndef USE_CAIRO
  Display *display = FRAME_X_DISPLAY (s->f);
  Pixmap pixmap = None;
#endif

  height = s->height;
  if (s->slice.y == 0)
    height -= box_line_vwidth;
  if (s->slice.y + s->slice.height >= s->img->height)
    height -= box_line_vwidth;

  /* Fill background with face under the image.  Do it only if row is
     taller than image or if image has a clip mask to reduce
     flickering.  */
  s->stippled_p = s->face->stipple != 0;
  if (height > s->slice.height
      || s->img->hmargin
      || s->img->vmargin
      || s->img->mask
      || s->img->pixmap == 0
      || s->width != s->background_width)
    {
      if (s->stippled_p)
	s->row->stipple_p = true;

#ifndef USE_CAIRO
      if (s->img->mask)
	{
	  /* Create a pixmap as large as the glyph string.  Fill it
	     with the background color.  Copy the image to it, using
	     its mask.  Copy the temporary pixmap to the display.  */
	  int depth = FRAME_DISPLAY_INFO (s->f)->n_planes;

	  /* Create a pixmap as large as the glyph string.  */
          pixmap = XCreatePixmap (display, FRAME_X_DRAWABLE (s->f),
				  s->background_width,
				  s->height, depth);

	  /* Don't clip in the following because we're working on the
	     pixmap.  */
	  XSetClipMask (display, s->gc, None);

	  /* Fill the pixmap with the background color/stipple.  */
	  if (s->stippled_p)
	    {
	      /* Fill background with a stipple pattern.  */
	      XSetFillStyle (display, s->gc, FillOpaqueStippled);
	      XSetTSOrigin (display, s->gc, - s->x, - s->y);
	      XFillRectangle (display, pixmap, s->gc,
			      0, 0, s->background_width, s->height);
	      XSetFillStyle (display, s->gc, FillSolid);
	      XSetTSOrigin (display, s->gc, 0, 0);
	    }
	  else
	    {
	      XGCValues xgcv;
#if defined HAVE_XRENDER && (RENDER_MAJOR > 0 || (RENDER_MINOR >= 2))
	      if (FRAME_DISPLAY_INFO (s->f)->alpha_bits
		  && s->f->alpha_background != 1.0
		  && FRAME_CHECK_XR_VERSION (s->f, 0, 2)
		  && FRAME_X_PICTURE_FORMAT (s->f))
		{
		  XRenderColor xc;
		  XRenderPictureAttributes attrs;
		  Picture pict;
		  memset (&attrs, 0, sizeof attrs);

		  pict = XRenderCreatePicture (display, pixmap,
					       FRAME_X_PICTURE_FORMAT (s->f),
					       0, &attrs);
		  x_xrender_color_from_gc_background (s->f, s->gc, &xc, true);
		  XRenderFillRectangle (FRAME_X_DISPLAY (s->f), PictOpSrc, pict,
					&xc, 0, 0, s->background_width, s->height);
		  XRenderFreePicture (display, pict);
		}
	      else
#endif
		{
		  XGetGCValues (display, s->gc, GCForeground | GCBackground,
				&xgcv);
		  XSetForeground (display, s->gc, xgcv.background);
		  XFillRectangle (display, pixmap, s->gc,
				  0, 0, s->background_width, s->height);
		  XSetForeground (display, s->gc, xgcv.foreground);
		}
	    }
	}
      else
#endif	/* ! USE_CAIRO */
	{
	  int x = s->x;
	  int y = s->y;
	  int width = s->background_width;

	  if (s->first_glyph->left_box_line_p
	      && s->slice.x == 0)
	    {
	      x += box_line_hwidth;
	      width -= box_line_hwidth;
	    }

	  if (s->slice.y == 0)
	    y += box_line_vwidth;

	  x_draw_glyph_string_bg_rect (s, x, y, width, height);
	}

      s->background_filled_p = true;
    }

  /* Draw the foreground.  */
#ifndef USE_CAIRO
  if (pixmap != None)
    {
      x_draw_image_foreground_1 (s, pixmap);
      x_set_glyph_string_clipping (s);
      XCopyArea (display, pixmap, FRAME_X_DRAWABLE (s->f), s->gc,
		 0, 0, s->background_width, s->height, s->x, s->y);
      XFreePixmap (display, pixmap);
    }
  else
#endif	/* ! USE_CAIRO */
    x_draw_image_foreground (s);

  /* If we must draw a relief around the image, do it.  */
  if (s->img->relief
      || s->hl == DRAW_IMAGE_RAISED
      || s->hl == DRAW_IMAGE_SUNKEN)
    x_draw_image_relief (s);
}


/* Draw stretch glyph string S.  */

static void
x_draw_stretch_glyph_string (struct glyph_string *s)
{
  eassert (s->first_glyph->type == STRETCH_GLYPH);

  if (s->hl == DRAW_CURSOR
      && !x_stretch_cursor_p)
    {
      /* If `x-stretch-cursor' is nil, don't draw a block cursor as
	 wide as the stretch glyph.  */
      int width, background_width = s->background_width;
      int x = s->x;

      if (!s->row->reversed_p)
	{
	  int left_x = window_box_left_offset (s->w, TEXT_AREA);

	  if (x < left_x)
	    {
	      background_width -= left_x - x;
	      x = left_x;
	    }
	}
      else
	{
	  /* In R2L rows, draw the cursor on the right edge of the
	     stretch glyph.  */
	  int right_x = window_box_right (s->w, TEXT_AREA);

	  if (x + background_width > right_x)
	    background_width -= x - right_x;
	  x += background_width;
	}
      width = min (FRAME_COLUMN_WIDTH (s->f), background_width);
      if (s->row->reversed_p)
	x -= width;

      /* Draw cursor.  */
      x_draw_glyph_string_bg_rect (s, x, s->y, width, s->height);

      /* Clear rest using the GC of the original non-cursor face.  */
      if (width < background_width)
	{
	  int y = s->y;
	  int w = background_width - width, h = s->height;
          Display *display = FRAME_X_DISPLAY (s->f);
	  XRectangle r;
	  GC gc;

	  if (!s->row->reversed_p)
	    x += width;
	  else
	    x = s->x;
	  if (s->row->mouse_face_p
	      && cursor_in_mouse_face_p (s->w))
	    {
	      x_set_mouse_face_gc (s);
	      gc = s->gc;
	    }
	  else
	    gc = s->face->gc;

	  get_glyph_string_clip_rect (s, &r);
	  x_set_clip_rectangles (s->f, gc, &r, 1);

	  if (s->face->stipple)
	    {
	      /* Fill background with a stipple pattern.  */
	      XSetFillStyle (display, gc, FillOpaqueStippled);
	      x_fill_rectangle (s->f, gc, x, y, w, h, true);
	      XSetFillStyle (display, gc, FillSolid);

	      s->row->stipple_p = true;
	    }
	  else
	    {
	      XGCValues xgcv;
	      XGetGCValues (display, gc, GCForeground | GCBackground, &xgcv);
	      XSetForeground (display, gc, xgcv.background);
	      x_fill_rectangle (s->f, gc, x, y, w, h, true);
	      XSetForeground (display, gc, xgcv.foreground);
	    }

	  x_reset_clip_rectangles (s->f, gc);
	}
    }
  else if (!s->background_filled_p)
    {
      int background_width = s->background_width;
      int x = s->x, text_left_x = window_box_left (s->w, TEXT_AREA);

      /* Don't draw into left fringe or scrollbar area except for
         header line and mode line.  */
      if (s->area == TEXT_AREA
	  && x < text_left_x && !s->row->mode_line_p)
	{
	  background_width -= text_left_x - x;
	  x = text_left_x;
	}

      if (!s->row->stipple_p)
	s->row->stipple_p = s->stippled_p;

      if (background_width > 0)
	x_draw_glyph_string_bg_rect (s, x, s->y,
				     background_width, s->height);
    }

  s->background_filled_p = true;
}

static void
x_get_scale_factor (struct x_display_info *dpyinfo,
		    int *scale_x, int *scale_y)
{
  int base_res = 96;

  *scale_x = *scale_y = 1;

  if (dpyinfo)
    {
      if (dpyinfo->resx > base_res)
	*scale_x = floor (dpyinfo->resx / base_res);
      if (dpyinfo->resy > base_res)
	*scale_y = floor (dpyinfo->resy / base_res);
    }
}

/*
   Draw a wavy line under S. The wave fills wave_height pixels from y0.

                    x0         wave_length = 2
                                 --
                y0   *   *   *   *   *
                     |* * * * * * * * *
    wave_height = 3  | *   *   *   *

*/
static void
x_draw_underwave (struct glyph_string *s, int decoration_width)
{
  struct x_display_info *dpyinfo;
  /* Adjust for scale/HiDPI.  */
  int scale_x, scale_y;

  dpyinfo = FRAME_DISPLAY_INFO (s->f);
  x_get_scale_factor (dpyinfo, &scale_x, &scale_y);

  int wave_height = 3 * scale_y, wave_length = 2 * scale_x;

#ifdef USE_CAIRO
  x_draw_horizontal_wave (s->f, s->gc, s->x, s->ybase - wave_height + 3,
			  decoration_width, wave_height, wave_length);
#else  /* not USE_CAIRO */
  Display *display;
  int dx, dy, x0, y0, width, x1, y1, x2, y2, xmax, thickness = scale_y;;
  bool odd;
  XRectangle wave_clip, string_clip, final_clip;

  dx = wave_length;
  dy = wave_height - 1;
  x0 = s->x;
  y0 = s->ybase + wave_height / 2 - scale_y;
  width = decoration_width;
  xmax = x0 + width;

  /* Find and set clipping rectangle */

  wave_clip.x = x0;
  wave_clip.y = y0;
  wave_clip.width = width;
  wave_clip.height = wave_height;
  get_glyph_string_clip_rect (s, &string_clip);

  if (!gui_intersect_rectangles (&wave_clip, &string_clip, &final_clip))
    return;

  display = dpyinfo->display;
  XSetClipRectangles (display, s->gc, 0, 0, &final_clip, 1, Unsorted);

  /* Draw the waves */

  x1 = x0 - (x0 % dx);
  x2 = x1 + dx;
  odd = (x1 / dx) & 1;
  y1 = y2 = y0;

  if (odd)
    y1 += dy;
  else
    y2 += dy;

  if (INT_MAX - dx < xmax)
    emacs_abort ();

  while (x1 <= xmax)
    {
      XSetLineAttributes (display, s->gc, thickness, LineSolid, CapButt,
                          JoinRound);
      XDrawLine (display, FRAME_X_DRAWABLE (s->f), s->gc, x1, y1, x2, y2);
      x1  = x2, y1 = y2;
      x2 += dx, y2 = y0 + odd*dy;
      odd = !odd;
    }

  /* Restore previous clipping rectangle(s) */
  XSetClipRectangles (display, s->gc, 0, 0, s->clip, s->num_clips, Unsorted);
#endif	/* not USE_CAIRO */
}

/* Draw a dashed underline of thickness THICKNESS and width WIDTH onto F
   at a vertical offset of OFFSET from the position of the glyph string
   S, with each segment SEGMENT pixels in length.  */

static void
x_draw_dash (struct frame *f, struct glyph_string *s, int width,
	     char segment, int offset, int thickness)
{
#ifndef USE_CAIRO
  GC gc;
  Display *display;
  XGCValues gcv;
  int y_center;

  /* Configure the GC, the dash pattern and a suitable offset.  */
  gc = s->gc;
  display = FRAME_X_DISPLAY (f);

  gcv.line_style = LineOnOffDash;
  gcv.line_width = thickness;
  XChangeGC (display, s->gc, GCLineStyle | GCLineWidth, &gcv);
  XSetDashes (display, s->gc, s->x, &segment, 1);

  /* Offset the origin of the line by half the line width. */
  y_center = s->ybase + offset + thickness / 2;
  XDrawLine (display, FRAME_X_DRAWABLE (f), gc,
	     s->x, y_center, s->x + width, y_center);

  /* Restore the initial line style.  */
  gcv.line_style = LineSolid;
  gcv.line_width = 1;
  XChangeGC (display, s->gc, GCLineStyle | GCLineWidth, &gcv);
#else /* USE_CAIRO */
  cairo_t *cr;
  double cr_segment, y_center;

  cr = x_begin_cr_clip (f, s->gc);
  cr_segment = (double) segment;
  y_center = s->ybase + offset + (thickness / 2.0);

  x_set_cr_source_with_gc_foreground (f, s->gc, false);
  cairo_set_dash (cr, &cr_segment, 1, s->x);
  cairo_set_line_width (cr, thickness);
  cairo_move_to (cr, s->x, y_center);
  cairo_line_to (cr, s->x + width, y_center);
  cairo_stroke (cr);
  x_end_cr_clip (f);
#endif /* USE_CAIRO */
}

/* Draw an underline of STYLE onto F at an offset of POSITION from the
   baseline of the glyph string S, DECORATION_WIDTH in length, and
   THICKNESS in height.  */

static void
x_fill_underline (struct frame *f, struct glyph_string *s,
		  enum face_underline_type style, int position,
		  int decoration_width, int thickness)
{
  int segment;
  char x_segment;

  segment = thickness * 3;

  switch (style)
    {
      /* FACE_UNDERLINE_DOUBLE_LINE is treated identically to SINGLE, as
	 the second line will be filled by another invocation of this
	 function.  */
    case FACE_UNDERLINE_SINGLE:
    case FACE_UNDERLINE_DOUBLE_LINE:
      x_fill_rectangle (f, s->gc, s->x, s->ybase + position,
			decoration_width, thickness, false);
      break;

    case FACE_UNDERLINE_DOTS:
      segment = thickness;
      FALLTHROUGH;

    case FACE_UNDERLINE_DASHES:
      x_segment = min (segment, CHAR_MAX);
      x_draw_dash (f, s, decoration_width, x_segment, position,
		   thickness);
      break;

    case FACE_NO_UNDERLINE:
    case FACE_UNDERLINE_WAVE:
    default:
      emacs_abort ();
    }
}

/* Draw glyph string S.  */

static void
x_draw_glyph_string (struct glyph_string *s)
{
  bool relief_drawn_p = false;

  /* If S draws into the background of its successors, draw the
     background of the successors first so that S can draw into it.
     This makes S->next use XDrawString instead of XDrawImageString.  */
  if (s->next && s->right_overhang && !s->for_overlaps)
    {
      int width;
      struct glyph_string *next;

      for (width = 0, next = s->next;
	   next && width < s->right_overhang;
	   width += next->width, next = next->next)
	if (next->first_glyph->type != IMAGE_GLYPH)
	  {
	    x_set_glyph_string_gc (next);
	    x_set_glyph_string_clipping (next);
	    if (next->first_glyph->type == STRETCH_GLYPH)
	      x_draw_stretch_glyph_string (next);
	    else
	      x_draw_glyph_string_background (next, true);
	    next->num_clips = 0;
	  }
    }

  /* Set up S->gc, set clipping and draw S.  */
  x_set_glyph_string_gc (s);

  /* Draw relief (if any) in advance for char/composition so that the
     glyph string can be drawn over it.  */
  if (!s->for_overlaps
      && s->face->box != FACE_NO_BOX
      && (s->first_glyph->type == CHAR_GLYPH
	  || s->first_glyph->type == COMPOSITE_GLYPH))

    {
      x_set_glyph_string_clipping (s);
      x_draw_glyph_string_background (s, true);
      x_draw_glyph_string_box (s);
      x_set_glyph_string_clipping (s);
      relief_drawn_p = true;
    }
  else if (!s->clip_head /* draw_glyphs didn't specify a clip mask. */
	   && !s->clip_tail
	   && ((s->prev && s->prev->hl != s->hl && s->left_overhang)
	       || (s->next && s->next->hl != s->hl && s->right_overhang)))
    /* We must clip just this glyph.  left_overhang part has already
       drawn when s->prev was drawn, and right_overhang part will be
       drawn later when s->next is drawn. */
    x_set_glyph_string_clipping_exactly (s, s);
  else
    x_set_glyph_string_clipping (s);

  switch (s->first_glyph->type)
    {
    case IMAGE_GLYPH:
      x_draw_image_glyph_string (s);
      break;

    case XWIDGET_GLYPH:
      x_draw_xwidget_glyph_string (s);
      break;

    case STRETCH_GLYPH:
      x_draw_stretch_glyph_string (s);
      break;

    case CHAR_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = true;
      else
	x_draw_glyph_string_background (s, false);
      x_draw_glyph_string_foreground (s);
      break;

    case COMPOSITE_GLYPH:
      if (s->for_overlaps || (s->cmp_from > 0
			      && ! s->first_glyph->u.cmp.automatic))
	s->background_filled_p = true;
      else
	x_draw_glyph_string_background (s, true);
      x_draw_composite_glyph_string_foreground (s);
      break;

    case GLYPHLESS_GLYPH:
      if (s->for_overlaps)
	s->background_filled_p = true;
      else
	x_draw_glyph_string_background (s, true);
      x_draw_glyphless_glyph_string_foreground (s);
      break;

    default:
      emacs_abort ();
    }

  if (!s->for_overlaps)
    {
      int area_x, area_y, area_width, area_height;
      int area_max_x, decoration_width;

      /* Prevent the underline from overwriting surrounding areas
	 and the fringe.  */
      window_box (s->w, s->area, &area_x, &area_y,
		  &area_width, &area_height);
      area_max_x = area_x + area_width - 1;

      decoration_width = s->width;
      if (!s->row->mode_line_p
	  && !s->row->tab_line_p
	  && area_max_x < (s->x + decoration_width - 1))
	decoration_width -= (s->x + decoration_width - 1) - area_max_x;

      /* Draw relief if not yet drawn.  */
      if (!relief_drawn_p && s->face->box != FACE_NO_BOX)
	x_draw_glyph_string_box (s);

      /* Draw underline.  */
      if (s->face->underline)
        {
          if (s->face->underline == FACE_UNDERLINE_WAVE)
            {
              if (s->face->underline_defaulted_p)
                x_draw_underwave (s, decoration_width);
              else
                {
                  Display *display = FRAME_X_DISPLAY (s->f);
                  XGCValues xgcv;
                  XGetGCValues (display, s->gc, GCForeground, &xgcv);
                  XSetForeground (display, s->gc, s->face->underline_color);
                  x_draw_underwave (s, decoration_width);
                  XSetForeground (display, s->gc, xgcv.foreground);
                }
            }
          else if (s->face->underline >= FACE_UNDERLINE_SINGLE)
            {
              unsigned long thickness, position;

              if (s->prev
		  && (s->prev->face->underline != FACE_UNDERLINE_WAVE
		      && s->prev->face->underline >= FACE_UNDERLINE_SINGLE)
		  && (s->prev->face->underline_at_descent_line_p
		      == s->face->underline_at_descent_line_p)
		  && (s->prev->face->underline_pixels_above_descent_line
		      == s->face->underline_pixels_above_descent_line))
                {
                  /* We use the same underline style as the previous one.  */
                  thickness = s->prev->underline_thickness;
                  position = s->prev->underline_position;
                }
              else
                {
		  struct font *font = font_for_underline_metrics (s);
		  unsigned long minimum_offset;
		  bool underline_at_descent_line;
		  bool use_underline_position_properties;
		  Lisp_Object val = (WINDOW_BUFFER_LOCAL_VALUE
				     (Qunderline_minimum_offset, s->w));

		  if (FIXNUMP (val))
		    minimum_offset = max (0, XFIXNUM (val));
		  else
		    minimum_offset = 1;

		  val = (WINDOW_BUFFER_LOCAL_VALUE
			 (Qx_underline_at_descent_line, s->w));
		  underline_at_descent_line
		    = (!(NILP (val) || BASE_EQ (val, Qunbound))
		       || s->face->underline_at_descent_line_p);

		  val = (WINDOW_BUFFER_LOCAL_VALUE
			 (Qx_use_underline_position_properties, s->w));
		  use_underline_position_properties
		    = !(NILP (val) || BASE_EQ (val, Qunbound));

                  /* Get the underline thickness.  Default is 1 pixel.  */
                  if (font && font->underline_thickness > 0)
                    thickness = font->underline_thickness;
                  else
                    thickness = 1;
                  if (underline_at_descent_line)
		    position = ((s->height - thickness)
				- (s->ybase - s->y)
				- s->face->underline_pixels_above_descent_line);
                  else
                    {
                      /* Get the underline position.  This is the
                         recommended vertical offset in pixels from
                         the baseline to the top of the underline.
                         This is a signed value according to the
                         specs, and its default is

                         ROUND ((maximum descent) / 2), with
                         ROUND(x) = floor (x + 0.5)  */

                      if (use_underline_position_properties
                          && font && font->underline_position >= 0)
                        position = font->underline_position;
                      else if (font)
                        position = (font->descent + 1) / 2;
                      else
                        position = minimum_offset;
                    }

		  /* Ignore minimum_offset if the amount of pixels was
		     explicitly specified.  */
		  if (!s->face->underline_pixels_above_descent_line)
		    position = max (position, minimum_offset);
                }
              /* Check the sanity of thickness and position.  We should
                 avoid drawing underline out of the current line area.  */
	      if (s->y + s->height <= s->ybase + position)
		position = (s->height - 1) - (s->ybase - s->y);
              if (s->y + s->height < s->ybase + position + thickness)
                thickness = (s->y + s->height) - (s->ybase + position);
              s->underline_thickness = thickness;
              s->underline_position = position;

	      {
		Display *display = FRAME_X_DISPLAY (s->f);
		XGCValues xgcv;

		if (!s->face->underline_defaulted_p)
		  {
		    XGetGCValues (display, s->gc, GCForeground, &xgcv);
		    XSetForeground (display, s->gc, s->face->underline_color);
		  }

		x_fill_underline (s->f, s, s->face->underline,
				  position, decoration_width,
				  thickness);

		/* Place a second underline above the first if this was
		   requested in the face specification.  */

		if (s->face->underline == FACE_UNDERLINE_DOUBLE_LINE)
		  {
		    /* Compute the position of the second underline.  */
		    position = position - thickness - 1;
		    x_fill_underline (s->f, s, s->face->underline,
				      position, decoration_width,
				      thickness);
		  }

		if (!s->face->underline_defaulted_p)
		  XSetForeground (display, s->gc, xgcv.foreground);
	      }
            }
        }
      /* Draw overline.  */
      if (s->face->overline_p)
	{
	  unsigned long dy = 0, h = 1;

	  if (s->face->overline_color_defaulted_p)
	    x_fill_rectangle (s->f, s->gc, s->x, s->y + dy,
			      decoration_width, h, false);
	  else
	    {
              Display *display = FRAME_X_DISPLAY (s->f);
	      XGCValues xgcv;
	      XGetGCValues (display, s->gc, GCForeground, &xgcv);
	      XSetForeground (display, s->gc, s->face->overline_color);
	      x_fill_rectangle (s->f, s->gc, s->x, s->y + dy,
				decoration_width, h, false);
	      XSetForeground (display, s->gc, xgcv.foreground);
	    }
	}

      /* Draw strike-through.  */
      if (s->face->strike_through_p)
	{
	  /* Y-coordinate and height of the glyph string's first
	     glyph.  We cannot use s->y and s->height because those
	     could be larger if there are taller display elements
	     (e.g., characters displayed with a larger font) in the
	     same glyph row.  */
	  int glyph_y = s->ybase - s->first_glyph->ascent;
	  int glyph_height = s->first_glyph->ascent + s->first_glyph->descent;
	  /* Strike-through width and offset from the glyph string's
	     top edge.  */
          unsigned long h = 1;
          unsigned long dy = (glyph_height - h) / 2;

	  if (s->face->strike_through_color_defaulted_p)
	    x_fill_rectangle (s->f, s->gc, s->x, glyph_y + dy,
			      s->width, h, false);
	  else
	    {
              Display *display = FRAME_X_DISPLAY (s->f);
	      XGCValues xgcv;
	      XGetGCValues (display, s->gc, GCForeground, &xgcv);
	      XSetForeground (display, s->gc, s->face->strike_through_color);
	      x_fill_rectangle (s->f, s->gc, s->x, glyph_y + dy,
				decoration_width, h, false);
	      XSetForeground (display, s->gc, xgcv.foreground);
	    }
	}

      if (s->prev)
	{
	  struct glyph_string *prev;

	  for (prev = s->prev; prev; prev = prev->prev)
	    if (prev->hl != s->hl
		&& prev->x + prev->width + prev->right_overhang > s->x)
	      {
		/* As prev was drawn while clipped to its own area, we
		   must draw the right_overhang part using s->hl now.  */
		enum draw_glyphs_face save = prev->hl;

		prev->hl = s->hl;
		x_set_glyph_string_gc (prev);
		x_set_glyph_string_clipping_exactly (s, prev);
		if (prev->first_glyph->type == CHAR_GLYPH)
		  x_draw_glyph_string_foreground (prev);
		else
		  x_draw_composite_glyph_string_foreground (prev);
		x_reset_clip_rectangles (prev->f, prev->gc);
		prev->hl = save;
		prev->num_clips = 0;
	      }
	}

      if (s->next)
	{
	  struct glyph_string *next;

	  for (next = s->next; next; next = next->next)
	    if (next->hl != s->hl
		&& next->x - next->left_overhang < s->x + s->width)
	      {
		/* As next will be drawn while clipped to its own area,
		   we must draw the left_overhang part using s->hl now.  */
		enum draw_glyphs_face save = next->hl;

		next->hl = s->hl;
		x_set_glyph_string_gc (next);
		x_set_glyph_string_clipping_exactly (s, next);
		if (next->first_glyph->type == CHAR_GLYPH)
		  x_draw_glyph_string_foreground (next);
		else
		  x_draw_composite_glyph_string_foreground (next);
		x_reset_clip_rectangles (next->f, next->gc);
		next->hl = save;
		next->num_clips = 0;
		next->clip_head = s->next;
	      }
	}
    }

  /* Reset clipping.  */
  x_reset_clip_rectangles (s->f, s->gc);
  s->num_clips = 0;

  /* Set the stippled flag that tells redisplay whether or not a
     stipple was actually draw.  */

  if (s->first_glyph->type != STRETCH_GLYPH
      && s->first_glyph->type != IMAGE_GLYPH
      && !s->row->stipple_p)
    s->row->stipple_p = s->stippled_p;
}

/* Shift display to make room for inserted glyphs.   */

static void
x_shift_glyphs_for_insert (struct frame *f, int x, int y, int width, int height, int shift_by)
{
/* Never called on a GUI frame, see
   https://lists.gnu.org/r/emacs-devel/2015-05/msg00456.html
*/
  XCopyArea (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f), FRAME_X_DRAWABLE (f),
	     f->output_data.x->normal_gc,
	     x, y, width, height,
	     x + shift_by, y);
}

/* Delete N glyphs at the nominal cursor position.  Not implemented
   for X frames.  */

static void
x_delete_glyphs (struct frame *f, int n)
{
  emacs_abort ();
}


/* Like XClearArea, but check that WIDTH and HEIGHT are reasonable.
   If they are <= 0, this is probably an error.  */

#if defined USE_GTK || !defined USE_CAIRO
static void
x_clear_area1 (Display *dpy, Window window,
               int x, int y, int width, int height, int exposures)
{
  eassert (width > 0 && height > 0);
  XClearArea (dpy, window, x, y, width, height, exposures);
}
#endif

void
x_clear_area (struct frame *f, int x, int y, int width, int height)
{
#ifdef USE_CAIRO
  cairo_t *cr;

  eassert (width > 0 && height > 0);

  cr = x_begin_cr_clip (f, NULL);
  x_set_cr_source_with_gc_background (f, f->output_data.x->normal_gc,
				      true);
  cairo_rectangle (cr, x, y, width, height);
  cairo_fill (cr);
  x_end_cr_clip (f);
#else
#ifndef USE_GTK
  if (f->alpha_background != 1.0
#ifdef HAVE_XDBE
      || FRAME_X_DOUBLE_BUFFERED_P (f)
#endif
      )
#endif
    {
#if defined HAVE_XRENDER && \
  (RENDER_MAJOR > 0 || (RENDER_MINOR >= 2))
      x_xr_ensure_picture (f);
      if (FRAME_DISPLAY_INFO (f)->alpha_bits
	  && FRAME_X_PICTURE (f) != None
	  && f->alpha_background != 1.0
	  && FRAME_CHECK_XR_VERSION (f, 0, 2))
	{
	  XRenderColor xc;
	  GC gc = f->output_data.x->normal_gc;

	  x_xr_apply_ext_clip (f, gc);
	  x_xrender_color_from_gc_background (f, gc, &xc, true);
	  XRenderFillRectangle (FRAME_X_DISPLAY (f),
				PictOpSrc, FRAME_X_PICTURE (f),
				&xc, x, y, width, height);
	  x_xr_reset_ext_clip (f);
	  x_mark_frame_dirty (f);
	}
      else
#endif
	XFillRectangle (FRAME_X_DISPLAY (f),
			FRAME_X_DRAWABLE (f),
			f->output_data.x->reverse_gc,
			x, y, width, height);
    }
#ifndef USE_GTK
  else
    x_clear_area1 (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
                   x, y, width, height, False);
#endif
#endif
}


/* Clear an entire frame.  */

static void
x_clear_frame (struct frame *f)
{
  /* Clearing the frame will erase any cursor, so mark them all as no
     longer visible.  */
  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));

  block_input ();
  font_drop_xrender_surfaces (f);
  x_clear_window (f);

  /* We have to clear the scroll bars.  If we have changed colors or
     something like that, then they should be notified.  */
  x_scroll_bar_clear (f);

  unblock_input ();
}

/* Send a message to frame F telling the event loop to track whether
   or not an hourglass is being displayed.  This is required to ignore
   the right events when the hourglass is mapped without calling XSync
   after displaying or hiding the hourglass.  */

static void
x_send_hourglass_message (struct frame *f, bool hourglass_enabled)
{
  struct x_display_info *dpyinfo;
  XEvent msg;

  dpyinfo = FRAME_DISPLAY_INFO (f);
  memset (&msg, 0, sizeof msg);

  msg.xclient.type = ClientMessage;
  msg.xclient.message_type
    = dpyinfo->Xatom_EMACS_TMP;
  msg.xclient.format = 8;
  msg.xclient.window = FRAME_X_WINDOW (f);
  msg.xclient.data.b[0] = hourglass_enabled ? 1 : 0;

  XSendEvent (dpyinfo->display, FRAME_X_WINDOW (f),
	      False, NoEventMask, &msg);
}

/* RIF: Show hourglass cursor on frame F.  */

static void
x_show_hourglass (struct frame *f)
{
  Display *dpy = FRAME_X_DISPLAY (f);

  if (dpy)
    {
      struct x_output *x = FRAME_X_OUTPUT (f);

      /* If the hourglass window is mapped inside a popup menu, input
	 could be lost if the menu is popped down and the grab is
	 relinquished, but the hourglass window is still up.  Just
	 avoid displaying the hourglass at all while popups are
	 active.  */

      if (popup_activated ())
	return;

      x_send_hourglass_message (f, true);

#ifdef USE_X_TOOLKIT
      if (x->widget)
#else
      if (FRAME_OUTER_WINDOW (f))
#endif
       {
         if (!x->hourglass_window)
           {
#ifndef USE_XCB
	     unsigned long mask = CWCursor;
	     XSetWindowAttributes attrs;
#ifdef USE_GTK
             Window parent = FRAME_X_WINDOW (f);
#else
             Window parent = FRAME_OUTER_WINDOW (f);
#endif
	     attrs.cursor = x->hourglass_cursor;

             x->hourglass_window = XCreateWindow
               (dpy, parent, 0, 0, 32000, 32000, 0, 0,
                InputOnly, CopyFromParent, mask, &attrs);
#else
	     uint32_t cursor = (uint32_t) x->hourglass_cursor;
#ifdef USE_GTK
             xcb_window_t parent = (xcb_window_t) FRAME_X_WINDOW (f);
#else
             xcb_window_t parent = (xcb_window_t) FRAME_OUTER_WINDOW (f);
#endif
	     x->hourglass_window
	       = (Window) xcb_generate_id (FRAME_DISPLAY_INFO (f)->xcb_connection);

	     xcb_create_window (FRAME_DISPLAY_INFO (f)->xcb_connection,
				XCB_COPY_FROM_PARENT,
				(xcb_window_t) x->hourglass_window,
				parent, 0, 0, FRAME_PIXEL_WIDTH (f),
				FRAME_PIXEL_HEIGHT (f), 0,
				XCB_WINDOW_CLASS_INPUT_ONLY,
				XCB_COPY_FROM_PARENT, XCB_CW_CURSOR,
				&cursor);
#endif
           }

#ifndef USE_XCB
         XMapRaised (dpy, x->hourglass_window);
	 /* Ensure that the spinning hourglass is shown.  */
	 flush_frame (f);
#else
	 uint32_t value = XCB_STACK_MODE_ABOVE;

	 xcb_configure_window (FRAME_DISPLAY_INFO (f)->xcb_connection,
			       (xcb_window_t) x->hourglass_window,
			       XCB_CONFIG_WINDOW_STACK_MODE, &value);
	 xcb_map_window (FRAME_DISPLAY_INFO (f)->xcb_connection,
			 (xcb_window_t) x->hourglass_window);
	 xcb_flush (FRAME_DISPLAY_INFO (f)->xcb_connection);
#endif
       }
    }
}

/* RIF: Cancel hourglass cursor on frame F.  */

static void
x_hide_hourglass (struct frame *f)
{
  struct x_output *x = FRAME_X_OUTPUT (f);

  /* Watch out for newly created frames.  */
  if (x->hourglass_window)
    {
#ifndef USE_XCB
      XUnmapWindow (FRAME_X_DISPLAY (f), x->hourglass_window);
#else
      xcb_unmap_window (FRAME_DISPLAY_INFO (f)->xcb_connection,
			(xcb_window_t) x->hourglass_window);
#endif
      x_send_hourglass_message (f, false);
    }
}

/* Invert the middle quarter of the frame for .15 sec.  */

static void
XTflash (struct frame *f)
{
  GC gc;
  XGCValues values;
  fd_set fds;
  int fd, rc;

  block_input ();
  values.function = GXinvert;
  gc = XCreateGC (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		  GCFunction, &values);

  /* Get the height not including a menu bar widget.  */
  int height = FRAME_PIXEL_HEIGHT (f);
  /* Height of each line to flash.  */
  int flash_height = FRAME_LINE_HEIGHT (f);
  /* These will be the left and right margins of the rectangles.  */
  int flash_left = FRAME_INTERNAL_BORDER_WIDTH (f);
  int flash_right = FRAME_PIXEL_WIDTH (f) - FRAME_INTERNAL_BORDER_WIDTH (f);
  int width = flash_right - flash_left;

  /* If window is tall, flash top and bottom line.  */
  if (height > 3 * FRAME_LINE_HEIGHT (f))
    {
      XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
		      flash_left,
		      (FRAME_INTERNAL_BORDER_WIDTH (f)
		       + FRAME_TOP_MARGIN_HEIGHT (f)),
		      width, flash_height);
      XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
		      flash_left,
		      (height - flash_height
		       - FRAME_INTERNAL_BORDER_WIDTH (f)
		       - FRAME_BOTTOM_MARGIN_HEIGHT (f)),
		      width, flash_height);

    }
  else
    /* If it is short, flash it all.  */
    XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
		    flash_left, FRAME_INTERNAL_BORDER_WIDTH (f),
		    width, height - 2 * FRAME_INTERNAL_BORDER_WIDTH (f));

  x_flush (f);

  struct timespec delay = make_timespec (0, 150 * 1000 * 1000);
  struct timespec wakeup = timespec_add (current_timespec (), delay);
  fd = ConnectionNumber (FRAME_X_DISPLAY (f));

  /* Keep waiting until past the time wakeup or any input gets
     available.  */
  while (! detect_input_pending ())
    {
      struct timespec current = current_timespec ();
      struct timespec timeout;

      /* Break if result would not be positive.  */
      if (timespec_cmp (wakeup, current) <= 0)
	break;

      /* How long `select' should wait.  */
      timeout = make_timespec (0, 10 * 1000 * 1000);

      /* Wait for some input to become available on the X
	 connection.  */
      FD_ZERO (&fds);
      FD_SET (fd, &fds);

      /* Try to wait that long--but we might wake up sooner.  */
      rc = pselect (fd + 1, &fds, NULL, NULL, &timeout, NULL);

      /* Some input is available, exit the visible bell.  */
      if (rc >= 0 && FD_ISSET (fd, &fds))
	break;
    }

  /* If window is tall, flash top and bottom line.  */
  if (height > 3 * FRAME_LINE_HEIGHT (f))
    {
      XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
		      flash_left,
		      (FRAME_INTERNAL_BORDER_WIDTH (f)
		       + FRAME_TOP_MARGIN_HEIGHT (f)),
		      width, flash_height);
      XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
		      flash_left,
		      (height - flash_height
		       - FRAME_INTERNAL_BORDER_WIDTH (f)
		       - FRAME_BOTTOM_MARGIN_HEIGHT (f)),
		      width, flash_height);
    }
  else
    /* If it is short, flash it all.  */
    XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), gc,
		    flash_left, FRAME_INTERNAL_BORDER_WIDTH (f),
		    width, height - 2 * FRAME_INTERNAL_BORDER_WIDTH (f));

  XFreeGC (FRAME_X_DISPLAY (f), gc);
  x_flush (f);

  unblock_input ();
}

/* Make audible bell.  */

static void
XTring_bell (struct frame *f)
{
  struct x_display_info *dpyinfo;

  if (!FRAME_X_DISPLAY (f))
    return;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (visible_bell)
    XTflash (f);
  else
    {
      /* When Emacs is untrusted, Bell requests sometimes generate
	 Access errors.  This is not in the security extension
	 specification but seems to be a bug in the X consortium XKB
	 implementation.  */

      block_input ();
      x_ignore_errors_for_next_request (dpyinfo, 0);
#ifdef HAVE_XKB
      XkbBell (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), 0, None);
#else
      XBell (FRAME_X_DISPLAY (f), 0);
#endif
      XFlush (FRAME_X_DISPLAY (f));
      x_stop_ignoring_errors (dpyinfo);
      unblock_input ();
    }
}

/***********************************************************************
			      Line Dance
 ***********************************************************************/

/* Perform an insert-lines or delete-lines operation, inserting N
   lines or deleting -N lines at vertical position VPOS.  */

static void
x_ins_del_lines (struct frame *f, int vpos, int n)
{
  emacs_abort ();
}


/* Scroll part of the display as described by RUN.  */

static void
x_scroll_run (struct window *w, struct run *run)
{
  struct frame *f = XFRAME (w->frame);
  int x, y, width, height, from_y, to_y, bottom_y;

  /* Get frame-relative bounding box of the text display area of W,
     without mode lines.  Include in this box the left and right
     fringe of W.  */
  window_box (w, ANY_AREA, &x, &y, &width, &height);

  from_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->current_y);
  to_y = WINDOW_TO_FRAME_PIXEL_Y (w, run->desired_y);
  bottom_y = y + height;

  if (to_y < from_y)
    {
      /* Scrolling up.  Make sure we don't copy part of the mode
	 line at the bottom.  */
      if (from_y + run->height > bottom_y)
	height = bottom_y - from_y;
      else
	height = run->height;
    }
  else
    {
      /* Scrolling down.  Make sure we don't copy over the mode line.
	 at the bottom.  */
      if (to_y + run->height > bottom_y)
	height = bottom_y - to_y;
      else
	height = run->height;
    }

  block_input ();

  /* Cursor off.  Will be switched on again in gui_update_window_end.  */
  gui_clear_cursor (w);

#ifdef HAVE_XWIDGETS
  /* "Copy" xwidget windows in the area that will be scrolled.  */
  Display *dpy = FRAME_X_DISPLAY (f);
  Window window = FRAME_X_WINDOW (f);

  Window root, parent, *children;
  unsigned int nchildren;

  if (XQueryTree (dpy, window, &root, &parent, &children, &nchildren))
    {
      /* Now find xwidget views situated between from_y and to_y, and
	 attached to w.  */
      for (unsigned int i = 0; i < nchildren; ++i)
	{
	  Window child = children[i];
	  struct xwidget_view *view = xwidget_view_from_window (child);

	  if (view && !view->hidden)
	    {
	      int window_y = view->y + view->clip_top;
	      int window_height = view->clip_bottom - view->clip_top;

	      Emacs_Rectangle r1, r2, result;
	      r1.x = w->pixel_left;
	      r1.y = from_y;
	      r1.width = w->pixel_width;
	      r1.height = height;
	      r2 = r1;
	      r2.y = window_y;
	      r2.height = window_height;

	      /* The window is offscreen, just unmap it.  */
	      if (window_height == 0)
		{
		  view->hidden = true;
		  XUnmapWindow (dpy, child);
		  continue;
		}

	      bool intersects_p =
		gui_intersect_rectangles (&r1, &r2, &result);

	      if (XWINDOW (view->w) == w && intersects_p)
		{
		  int y = view->y + (to_y - from_y);
		  int text_area_x, text_area_y, text_area_width, text_area_height;
		  int clip_top, clip_bottom;

		  window_box (w, view->area, &text_area_x, &text_area_y,
			      &text_area_width, &text_area_height);

		  view->y = y;

		  clip_top = 0;
		  clip_bottom = XXWIDGET (view->model)->height;

		  if (y < text_area_y)
		    clip_top = text_area_y - y;

		  if ((y + clip_bottom) > (text_area_y + text_area_height))
		    {
		      clip_bottom -= (y + clip_bottom) - (text_area_y + text_area_height);
		    }

		  view->clip_top = clip_top;
		  view->clip_bottom = clip_bottom;

		  /* This means the view has moved offscreen.  Unmap
		     it and hide it here.  */
		  if ((view->clip_bottom - view->clip_top) <= 0)
		    {
		      view->hidden = true;
		      XUnmapWindow (dpy, child);
		    }
		  else
		    {
		      XMoveResizeWindow (dpy, child, view->x + view->clip_left,
					 view->y + view->clip_top,
					 view->clip_right - view->clip_left,
					 view->clip_bottom - view->clip_top);
		      cairo_xlib_surface_set_size (view->cr_surface,
						   view->clip_right - view->clip_left,
						   view->clip_bottom - view->clip_top);
		    }
		  xwidget_expose (view);
		}
            }
	}
      XFree (children);
    }
#endif

#ifdef USE_CAIRO_XCB_SURFACE
  /* Some of the following code depends on `normal_gc' being
     up-to-date on the X server, but doesn't call a routine that will
     flush it first.  So do this ourselves instead.  */
  XFlushGC (FRAME_X_DISPLAY (f),
	    f->output_data.x->normal_gc);
#endif

#ifdef USE_CAIRO
  if (FRAME_CR_CONTEXT (f))
    {
      cairo_surface_t *surface = cairo_get_target (FRAME_CR_CONTEXT (f));
      if (cairo_surface_get_type (surface) == CAIRO_SURFACE_TYPE_XLIB)
	{
	  eassert (cairo_xlib_surface_get_display (surface)
		   == FRAME_X_DISPLAY (f));
	  eassert (cairo_xlib_surface_get_drawable (surface)
		   == FRAME_X_RAW_DRAWABLE (f));
	  cairo_surface_flush (surface);
	  XCopyArea (FRAME_X_DISPLAY (f),
		     FRAME_X_DRAWABLE (f), FRAME_X_DRAWABLE (f),
		     f->output_data.x->normal_gc,
		     x, from_y,
		     width, height,
		     x, to_y);
	  cairo_surface_mark_dirty_rectangle (surface, x, to_y, width, height);
	}
#ifdef USE_CAIRO_XCB_SURFACE
      else if (cairo_surface_get_type (surface) == CAIRO_SURFACE_TYPE_XCB)
	{
	  cairo_surface_flush (surface);
	  xcb_copy_area (FRAME_DISPLAY_INFO (f)->xcb_connection,
			 (xcb_drawable_t) FRAME_X_DRAWABLE (f),
			 (xcb_drawable_t) FRAME_X_DRAWABLE (f),
			 (xcb_gcontext_t) XGContextFromGC (f->output_data.x->normal_gc),
			 x, from_y, x, to_y, width, height);
	  cairo_surface_mark_dirty_rectangle (surface, x, to_y, width, height);
	}
#endif
      else
	{
	  cairo_surface_t *s
	    = cairo_surface_create_similar (surface,
					    cairo_surface_get_content (surface),
					    width, height);
	  cairo_t *cr = cairo_create (s);
	  cairo_set_source_surface (cr, surface, -x, -from_y);
	  cairo_paint (cr);
	  cairo_destroy (cr);

	  cr = FRAME_CR_CONTEXT (f);
	  cairo_save (cr);
	  cairo_set_source_surface (cr, s, x, to_y);
	  cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
	  cairo_rectangle (cr, x, to_y, width, height);
	  cairo_fill (cr);
	  cairo_restore (cr);
	  cairo_surface_destroy (s);
	}
    }
  else
#endif	/* USE_CAIRO */
    XCopyArea (FRAME_X_DISPLAY (f),
	       FRAME_X_DRAWABLE (f), FRAME_X_DRAWABLE (f),
	       f->output_data.x->normal_gc,
	       x, from_y,
	       width, height,
	       x, to_y);

  unblock_input ();
}



/***********************************************************************
			   Exposure Events
 ***********************************************************************/


static void
x_frame_highlight (struct frame *f)
{
  struct x_display_info *dpyinfo;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  /* We used to only do this if Vx_no_window_manager was non-nil, but
     the ICCCM (section 4.1.6) says that the window's border pixmap
     and border pixel are window attributes which are "private to the
     client", so we can always change it to whatever we want.  */
  block_input ();
  /* I recently started to get errors in this XSetWindowBorder, depending on
     the window-manager in use, tho something more is at play since I've been
     using that same window-manager binary for ever.  Let's not crash just
     because of this (bug#9310).  */
  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSetWindowBorder (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		    f->output_data.x->border_pixel);
  x_stop_ignoring_errors (dpyinfo);
  unblock_input ();
  gui_update_cursor (f, true);

  if (!FRAME_X_OUTPUT (f)->alpha_identical_p)
    x_set_frame_alpha (f);
}

static void
x_frame_unhighlight (struct frame *f)
{
  struct x_display_info *dpyinfo;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  /* We used to only do this if Vx_no_window_manager was non-nil, but
     the ICCCM (section 4.1.6) says that the window's border pixmap
     and border pixel are window attributes which are "private to the
     client", so we can always change it to whatever we want.  */

  block_input ();
  /* Same as above for XSetWindowBorder (bug#9310).  */
  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSetWindowBorderPixmap (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			  f->output_data.x->border_tile);
  x_stop_ignoring_errors (dpyinfo);
  unblock_input ();

  gui_update_cursor (f, true);

  /* Eschew modifying the frame alpha when the alpha values for
     focused and background frames are identical; otherwise, this will
     upset the order in which changes to the alpha property
     immediately subsequent to a focus change are propagated into a
     frame's alpha property.  (bug#66398) */

  if (!FRAME_X_OUTPUT (f)->alpha_identical_p)
    x_set_frame_alpha (f);
}

/* The focus has changed.  Update the frames as necessary to reflect
   the new situation.  Note that we can't change the selected frame
   here, because the Lisp code we are interrupting might become confused.
   Each event gets marked with the frame in which it occurred, so the
   Lisp code can tell when the switch took place by examining the events.  */

static void
x_new_focus_frame (struct x_display_info *dpyinfo, struct frame *frame)
{
  struct frame *old_focus = dpyinfo->x_focus_frame;
#if defined USE_GTK && !defined HAVE_GTK3 && defined HAVE_XINPUT2
  XIEventMask mask;
  if (dpyinfo->supports_xi2)
    {
      ptrdiff_t l = XIMaskLen (XI_LASTEVENT);
      mask.mask = alloca (l);
      mask.mask_len = l;
      memset (mask.mask, 0, l);

      mask.deviceid = XIAllDevices;
    }
#endif

  if (frame != dpyinfo->x_focus_frame)
    {
      /* Set this before calling other routines, so that they see
	 the correct value of x_focus_frame.  */
      dpyinfo->x_focus_frame = frame;

      /* Once none of our frames are focused anymore, stop selecting
	 for raw input events from the root window.  */

#if defined USE_GTK && !defined HAVE_GTK3 && defined HAVE_XINPUT2
      if (frame && dpyinfo->supports_xi2)
	XISetMask (mask.mask, XI_RawKeyPress);

      if (dpyinfo->supports_xi2)
	XISelectEvents (dpyinfo->display, dpyinfo->root_window, &mask, 1);
#endif

      if (old_focus && old_focus->auto_lower)
	x_lower_frame (old_focus);

      if (dpyinfo->x_focus_frame && dpyinfo->x_focus_frame->auto_raise)
	dpyinfo->x_pending_autoraise_frame = dpyinfo->x_focus_frame;
      else
	dpyinfo->x_pending_autoraise_frame = NULL;
    }

  x_frame_rehighlight (dpyinfo);
}

#if defined HAVE_XFIXES && XFIXES_VERSION >= 40000

/* True if the display in DPYINFO supports a version of Xfixes
   sufficient for pointer blanking.  */

static bool
x_fixes_pointer_blanking_supported (struct x_display_info *dpyinfo)
{
  return (dpyinfo->xfixes_supported_p
	  && dpyinfo->xfixes_major >= 4);
}

#endif /* HAVE_XFIXES && XFIXES_VERSION >= 40000 */

/* Toggle mouse pointer visibility on frame F using the XFixes
   extension.  */
#if defined HAVE_XFIXES && XFIXES_VERSION >= 40000

static void
xfixes_toggle_visible_pointer (struct frame *f, bool invisible)

{
  if (invisible)
    XFixesHideCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
  else
    XFixesShowCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
  f->pointer_invisible = invisible;
}

#endif /* HAVE_XFIXES */

/* Create invisible cursor on the X display referred by DPYINFO.  */
static Cursor
make_invisible_cursor (struct x_display_info *dpyinfo)
{
  Display *dpy = dpyinfo->display;
  static char const no_data[] = { 0 };
  Pixmap pix;
  XColor col;
  Cursor c;

  c = None;

  x_catch_errors (dpy);
  pix = XCreateBitmapFromData (dpy, dpyinfo->root_window, no_data, 1, 1);
  if (!x_had_errors_p (dpy) && pix != None)
    {
      Cursor pixc;
      col.pixel = 0;
      col.red = col.green = col.blue = 0;
      col.flags = DoRed | DoGreen | DoBlue;
      pixc = XCreatePixmapCursor (dpy, pix, pix, &col, &col, 0, 0);
      if (! x_had_errors_p (dpy) && pixc != None)
        c = pixc;
      XFreePixmap (dpy, pix);
    }

  x_uncatch_errors ();

  return c;
}

/* Toggle mouse pointer visibility on frame F by using an invisible
   cursor.  */
static void
x_toggle_visible_pointer (struct frame *f, bool invisible)
{
  struct x_display_info *dpyinfo;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  /* We could have gotten a BadAlloc error while creating the
     invisible cursor.  Try to create it again, but if that fails,
     just give up.  */
  if (dpyinfo->invisible_cursor == None)
    dpyinfo->invisible_cursor = make_invisible_cursor (dpyinfo);

#if !defined HAVE_XFIXES || XFIXES_VERSION < 40000
  if (dpyinfo->invisible_cursor == None)
    invisible = false;
#else
  /* But if Xfixes is available, try using it instead.  */
  if (dpyinfo->invisible_cursor == None)
    {
      if (x_fixes_pointer_blanking_supported (dpyinfo))
	{
	  dpyinfo->fixes_pointer_blanking = true;
	  xfixes_toggle_visible_pointer (f, invisible);

	  return;
	}
      else
	invisible = false;
    }
#endif

  if (invisible)
    XDefineCursor (dpyinfo->display, FRAME_X_WINDOW (f),
		   dpyinfo->invisible_cursor);
  else
    XDefineCursor (dpyinfo->display, FRAME_X_WINDOW (f),
		   f->output_data.x->current_cursor);

  f->pointer_invisible = invisible;
}

static void
XTtoggle_invisible_pointer (struct frame *f, bool invisible)
{
  block_input ();
#if defined HAVE_XFIXES && XFIXES_VERSION >= 40000
  if (FRAME_DISPLAY_INFO (f)->fixes_pointer_blanking
      && x_fixes_pointer_blanking_supported (FRAME_DISPLAY_INFO (f)))
    xfixes_toggle_visible_pointer (f, invisible);
  else
#endif
    x_toggle_visible_pointer (f, invisible);
  unblock_input ();
}

/* Handle FocusIn and FocusOut state changes for FRAME.  If FRAME has
   focus and there exists more than one frame, puts a FOCUS_IN_EVENT
   into *BUFP.  Note that this code is not used to handle focus
   changes on builds that can use the X Input extension for handling
   input focus when it is available (currently the no toolkit and GTK
   3 toolkits).  */

static void
x_focus_changed (int type, int state, struct x_display_info *dpyinfo,
		 struct frame *frame, struct input_event *bufp)
{
  if (type == FocusIn)
    {
      if (dpyinfo->x_focus_event_frame != frame)
        {
          x_new_focus_frame (dpyinfo, frame);
          dpyinfo->x_focus_event_frame = frame;
          bufp->kind = FOCUS_IN_EVENT;
          XSETFRAME (bufp->frame_or_window, frame);
        }

      frame->output_data.x->focus_state |= state;

#ifdef HAVE_X_I18N
      if (FRAME_XIC (frame))
	XSetICFocus (FRAME_XIC (frame));
#ifdef USE_GTK
      GtkWidget *widget;

      if (x_gtk_use_native_input)
	{
	  gtk_im_context_focus_in (FRAME_X_OUTPUT (frame)->im_context);
	  widget = FRAME_GTK_OUTER_WIDGET (frame);
	  gtk_im_context_set_client_window (FRAME_X_OUTPUT (frame)->im_context,
					    gtk_widget_get_window (widget));
	}
#endif
#endif
    }
  else if (type == FocusOut)
    {
      frame->output_data.x->focus_state &= ~state;

      if (dpyinfo->x_focus_event_frame == frame)
        {
          dpyinfo->x_focus_event_frame = 0;
          x_new_focus_frame (dpyinfo, 0);

          bufp->kind = FOCUS_OUT_EVENT;
          XSETFRAME (bufp->frame_or_window, frame);
        }

      if (!frame->output_data.x->focus_state)
	{
#ifdef HAVE_X_I18N
	  if (FRAME_XIC (frame))
	    XUnsetICFocus (FRAME_XIC (frame));
#ifdef USE_GTK
	  if (x_gtk_use_native_input)
	    {
	      gtk_im_context_focus_out (FRAME_X_OUTPUT (frame)->im_context);
	      gtk_im_context_set_client_window (FRAME_X_OUTPUT (frame)->im_context, NULL);
	    }
#endif
#endif
	}

      if (frame->pointer_invisible)
        XTtoggle_invisible_pointer (frame, false);
    }
}

/* Return the Emacs frame-object corresponding to an X window.  It
   could be the frame's main window, an icon window, or an xwidget
   window.  */

static struct frame *
x_window_to_frame (struct x_display_info *dpyinfo, int wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f;

  if (wdesc == None)
    return NULL;

#ifdef HAVE_XWIDGETS
  struct xwidget_view *xvw = xwidget_view_from_window (wdesc);

  if (xvw && xvw->frame)
    return xvw->frame;
#endif

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);
      if (!FRAME_X_P (f) || FRAME_DISPLAY_INFO (f) != dpyinfo)
	continue;
      if (f->output_data.x->hourglass_window == wdesc)
	return f;
#ifdef USE_X_TOOLKIT
      if ((f->output_data.x->edit_widget
	   && XtWindow (f->output_data.x->edit_widget) == wdesc)
	  /* A tooltip frame?  */
	  || (!f->output_data.x->edit_widget
	      && FRAME_X_WINDOW (f) == wdesc)
          || f->output_data.x->icon_desc == wdesc)
        return f;
#else /* not USE_X_TOOLKIT */
#ifdef USE_GTK
      if (f->output_data.x->edit_widget)
      {
        GtkWidget *gwdesc = xg_win_to_widget (dpyinfo->display, wdesc);
        struct x_output *x = f->output_data.x;
        if (gwdesc != 0 && gwdesc == x->edit_widget)
          return f;
      }
#endif /* USE_GTK */
      if (FRAME_X_WINDOW (f) == wdesc
          || f->output_data.x->icon_desc == wdesc)
        return f;
#endif /* not USE_X_TOOLKIT */
    }
  return 0;
}

/* Like x_any_window_to_frame but only try to find tooltip frames.

   If wdesc is a toolkit tooltip without an associated frame, set
   UNRELATED_TOOLTIP_P to true.  Otherwise, set it to false.  */
static struct frame *
x_tooltip_window_to_frame (struct x_display_info *dpyinfo,
			   Window wdesc, bool *unrelated_tooltip_p)
{
  Lisp_Object tail, frame;
  struct frame *f;
#ifdef USE_GTK
  GtkWidget *widget;
  GdkWindow *tooltip_window;
#endif

  if (unrelated_tooltip_p)
    *unrelated_tooltip_p = false;

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);

      if (FRAME_X_P (f) && FRAME_TOOLTIP_P (f)
	  && FRAME_DISPLAY_INFO (f) == dpyinfo
	  && FRAME_X_WINDOW (f) == wdesc)
	return f;

#ifdef USE_GTK
      if (!FRAME_X_P (f))
	continue;

      if (FRAME_X_OUTPUT (f)->ttip_window)
	widget = GTK_WIDGET (FRAME_X_OUTPUT (f)->ttip_window);
      else
	widget = NULL;

      if (widget)
	tooltip_window = gtk_widget_get_window (widget);
      else
	tooltip_window = NULL;

#ifdef HAVE_GTK3
      if (tooltip_window
	  && (gdk_x11_window_get_xid (tooltip_window) == wdesc))
	{
	  if (unrelated_tooltip_p)
	    *unrelated_tooltip_p = true;
	  break;
	}
#else
      if (tooltip_window
	  && (GDK_WINDOW_XID (tooltip_window) == wdesc))
	{
	  if (unrelated_tooltip_p)
	    *unrelated_tooltip_p = true;
	  break;
	}
#endif
#endif
    }

  return NULL;
}

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)

/* Like x_window_to_frame but also compares the window with the widget's
   windows.  */

static struct frame *
x_any_window_to_frame (struct x_display_info *dpyinfo, int wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f, *found = NULL;
  struct x_output *x;

  if (wdesc == None)
    return NULL;

#ifdef HAVE_XWIDGETS
  struct xwidget_view *xv = xwidget_view_from_window (wdesc);

  if (xv)
    return xv->frame;
#endif

  FOR_EACH_FRAME (tail, frame)
    {
      if (found)
        break;
      f = XFRAME (frame);
      if (FRAME_X_P (f) && FRAME_DISPLAY_INFO (f) == dpyinfo)
	{
	  /* This frame matches if the window is any of its widgets.  */
	  x = f->output_data.x;
	  if (x->hourglass_window == wdesc)
	    found = f;
	  else if (x->widget)
	    {
#ifdef USE_GTK
              GtkWidget *gwdesc = xg_win_to_widget (dpyinfo->display, wdesc);
              if (gwdesc != 0
                  && gtk_widget_get_toplevel (gwdesc) == x->widget)
                found = f;
#else
	      if (wdesc == XtWindow (x->widget)
		  || wdesc == XtWindow (x->column_widget)
		  || wdesc == XtWindow (x->edit_widget))
		found = f;
	      /* Match if the window is this frame's menubar.  */
	      else if (lw_window_is_in_menubar (wdesc, x->menubar_widget))
		found = f;
#endif
	    }
	  else if (FRAME_X_WINDOW (f) == wdesc)
	    /* A tooltip frame.  */
	    found = f;
	}
    }

  return found;
}

/* Likewise, but consider only the menu bar widget.  */

static struct frame *
x_menubar_window_to_frame (struct x_display_info *dpyinfo,
			   const XEvent *event)
{
  Window wdesc;
#ifdef HAVE_XINPUT2
  if (event->type == GenericEvent
      && dpyinfo->supports_xi2
      && (event->xcookie.evtype == XI_ButtonPress
	  || event->xcookie.evtype == XI_ButtonRelease))
    wdesc = ((XIDeviceEvent *) event->xcookie.data)->event;
  else
#endif
    wdesc = event->xany.window;
  Lisp_Object tail, frame;
  struct frame *f;
  struct x_output *x;

  if (wdesc == None)
    return NULL;

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);
      if (!FRAME_X_P (f) || FRAME_DISPLAY_INFO (f) != dpyinfo)
	continue;
      x = f->output_data.x;
#ifdef USE_GTK
      if (x->menubar_widget && xg_event_is_for_menubar (f, event))
        return f;
#else
      /* Match if the window is this frame's menubar.  */
      if (x->menubar_widget
	  && lw_window_is_in_menubar (wdesc, x->menubar_widget))
	return f;
#endif
    }
  return 0;
}

/* Return the frame whose principal (outermost) window is WDESC.
   If WDESC is some other (smaller) window, we return 0.  */

struct frame *
x_top_window_to_frame (struct x_display_info *dpyinfo, int wdesc)
{
  Lisp_Object tail, frame;
  struct frame *f;
  struct x_output *x;

  if (wdesc == None)
    return NULL;

  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);
      if (!FRAME_X_P (f) || FRAME_DISPLAY_INFO (f) != dpyinfo)
	continue;
      x = f->output_data.x;

      if (x->widget)
	{
	  /* This frame matches if the window is its topmost widget.  */
#ifdef USE_GTK
          GtkWidget *gwdesc = xg_win_to_widget (dpyinfo->display, wdesc);
          if (gwdesc == x->widget)
            return f;
#else
	  if (wdesc == XtWindow (x->widget))
	    return f;
#endif
	}
      else if (FRAME_X_WINDOW (f) == wdesc)
	/* Tooltip frame.  */
	return f;
    }
  return 0;
}

#else /* !USE_X_TOOLKIT && !USE_GTK */

#define x_any_window_to_frame(d, i) x_window_to_frame (d, i)

struct frame *
x_top_window_to_frame (struct x_display_info *dpyinfo, int wdesc)
{
  return x_window_to_frame (dpyinfo, wdesc);
}

static void
x_next_event_from_any_display (XEvent *event)
{
  struct x_display_info *dpyinfo;
  fd_set fds, rfds;
  int fd, maxfd, rc;

  rc = -1;
  FD_ZERO (&rfds);

  while (true)
    {
      FD_ZERO (&fds);
      maxfd = -1;

      for (dpyinfo = x_display_list; dpyinfo;
	   dpyinfo = dpyinfo->next)
	{
	  fd = ConnectionNumber (dpyinfo->display);

	  if ((rc < 0 || FD_ISSET (fd, &rfds))
	      && XPending (dpyinfo->display))
	    {
	      XNextEvent (dpyinfo->display, event);
	      return;
	    }

	  if (fd > maxfd)
	    maxfd = fd;

	  eassert (fd < FD_SETSIZE);
	  FD_SET (fd, &fds);
	}

      eassert (maxfd >= 0);

      /* Continue to read input even if pselect fails, because if an
	 error occurs XPending will call the IO error handler, which
	 then brings us out of this loop.  */
      rc = pselect (maxfd + 1, &fds, NULL, NULL, NULL, NULL);

      if (rc >= 0)
	rfds = fds;
    }
}

#endif /* USE_X_TOOLKIT || USE_GTK */

static void
x_handle_pending_selection_requests_1 (struct x_selection_request_event *tem)
{
  specpdl_ref count;
  struct selection_input_event se;

  count = SPECPDL_INDEX ();
  se = tem->se;

  record_unwind_protect_ptr (xfree, tem);
  x_handle_selection_event (&se);
  unbind_to (count, Qnil);
}

/* Handle all pending selection request events from modal event
   loops.  */
void
x_handle_pending_selection_requests (void)
{
  struct x_selection_request_event *tem;

  while (pending_selection_requests)
    {
      tem = pending_selection_requests;
      pending_selection_requests = tem->next;

      x_handle_pending_selection_requests_1 (tem);
    }
}

static void
x_push_selection_request (struct selection_input_event *se)
{
  struct x_selection_request_event *tem;

  tem = xmalloc (sizeof *tem);
  tem->next = pending_selection_requests;
  tem->se = *se;
  pending_selection_requests = tem;
}

bool
x_detect_pending_selection_requests (void)
{
  return !!pending_selection_requests;
}

static void
x_clear_dnd_action (void)
{
  x_dnd_action_symbol = Qnil;
}

/* Delete action descriptions from F after drag-and-drop.  */
static void
x_dnd_delete_action_list (Lisp_Object frame)
{
  struct frame *f;

  /* Delete those two properties, since some clients look at them and
     not the action to decide whether or not the user should be
     prompted to select an action.  This can be called with FRAME no
     longer alive (or its display dead).  */

  f = XFRAME (frame);

  if (!FRAME_LIVE_P (f) || !FRAME_DISPLAY_INFO (f)->display)
    return;

  block_input ();
  XDeleteProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   FRAME_DISPLAY_INFO (f)->Xatom_XdndActionList);
  XDeleteProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		   FRAME_DISPLAY_INFO (f)->Xatom_XdndActionDescription);
  unblock_input ();
}

static void
x_dnd_lose_ownership (Lisp_Object timestamp_and_frame)
{
  struct frame *f;

  f = XFRAME (XCDR (timestamp_and_frame));

  if (FRAME_LIVE_P (f))
    Fx_disown_selection_internal (QXdndSelection,
				  XCAR (timestamp_and_frame),
				  XCDR (timestamp_and_frame));
}

/* Clean up an existing drag-and-drop operation in preparation for its
   sudden termination.  */

static void
x_dnd_process_quit (struct frame *f, Time timestamp)
{
  xm_drop_start_message dmsg;

  if (x_dnd_in_progress)
    {
      if (x_dnd_last_seen_window != None
	  && x_dnd_last_protocol_version != -1)
	x_dnd_send_leave (f, x_dnd_last_seen_window,
			  x_dnd_last_seen_toplevel);
      else if (x_dnd_last_seen_window != None
	       && !XM_DRAG_STYLE_IS_DROP_ONLY (x_dnd_last_motif_style)
	       && x_dnd_last_motif_style != XM_DRAG_STYLE_NONE
	       && x_dnd_motif_setup_p)
	{
	  dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
					XM_DRAG_REASON_DROP_START);
	  dmsg.byte_order = XM_BYTE_ORDER_CUR_FIRST;
	  dmsg.timestamp = timestamp;
	  dmsg.side_effects
	    = XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (FRAME_DISPLAY_INFO (f),
							       x_dnd_wanted_action),
				   XM_DROP_SITE_VALID, x_dnd_motif_operations,
				   XM_DROP_ACTION_DROP_CANCEL);
	  dmsg.x = 0;
	  dmsg.y = 0;
	  dmsg.index_atom = x_dnd_motif_atom;
	  dmsg.source_window = FRAME_X_WINDOW (f);

	  x_dnd_send_xm_leave_for_drop (FRAME_DISPLAY_INFO (f), f,
					x_dnd_last_seen_window,
					timestamp);
	  xm_send_drop_message (FRAME_DISPLAY_INFO (f), FRAME_X_WINDOW (f),
				x_dnd_last_seen_window, &dmsg);
	}

      x_dnd_end_window = x_dnd_last_seen_window;
      x_dnd_last_seen_window = None;
      x_dnd_last_seen_toplevel = None;
      x_dnd_in_progress = false;
      x_dnd_frame = NULL;
    }

  x_dnd_waiting_for_finish = false;
  x_dnd_return_frame_object = NULL;
  x_dnd_movement_frame = NULL;
  x_dnd_wheel_frame = NULL;
}

/* This function is defined far away from the rest of the XDND code so
   it can utilize `x_any_window_to_frame'.  */

/* Implementers beware!  On most other platforms (where drag-and-drop
   data is not provided via selections, but some kind of serialization
   mechanism), it is usually much easier to implement a suitable
   primitive instead of copying the C code here, and then to build
   `x-begin-drag' on top of that, by making it a wrapper function in
   Lisp that converts the list of targets and value of `XdndSelection'
   to serialized data.  Also be sure to update the data types used in
   dnd.el.

   For examples of how to do this, see `haiku-drag-message' and
   `x-begin-drag' in haikuselect.c and lisp/term/haiku-win.el, and
   `ns-begin-drag' and `x-begin-drag' in nsselect.m and
   lisp/term/ns-win.el.  */

Lisp_Object
x_dnd_begin_drag_and_drop (struct frame *f, Time time, Atom xaction,
			   Lisp_Object return_frame, Atom *ask_action_list,
			   const char **ask_action_names, size_t n_ask_actions,
			   bool allow_current_frame, Atom *target_atoms,
			   int ntargets, Lisp_Object selection_target_list,
			   bool follow_tooltip)
{
#ifndef USE_GTK
  XEvent next_event;
  int finish;
#endif
  XWindowAttributes root_window_attrs;
  struct input_event hold_quit;
  char *atom_name, *ask_actions;
  Lisp_Object action, ltimestamp, val;
  specpdl_ref ref, count, base;
  ptrdiff_t i, end, fill;
  XTextProperty prop;
  Lisp_Object frame_object, x, y, frame, local_value;
  bool signals_were_pending, need_sync;
#ifdef HAVE_XKB
  XkbStateRec keyboard_state;
#endif
#ifndef USE_GTK
  struct x_display_info *event_display;
#endif
  unsigned int additional_mask;
#ifdef HAVE_XINPUT2
  struct xi_device_t *device;
#endif

  if (FRAME_DISPLAY_INFO (f)->untrusted)
    /* Untrusted clients cannot send messages to trusted clients or
       read the window tree, so drag and drop will likely not work at
       all.  */
    error ("Drag-and-drop is not possible when the client is"
	   " not trusted by the X server.");

  base = SPECPDL_INDEX ();

  /* Bind this here to avoid juggling bindings and SAFE_FREE in
     Fx_begin_drag.  */
  specbind (Qx_dnd_targets_list, selection_target_list);

  if (!FRAME_VISIBLE_P (f))
    error ("Frame must be visible");

  XSETFRAME (frame, f);
  local_value = assq_no_quit (QXdndSelection,
			      FRAME_TERMINAL (f)->Vselection_alist);

  if (x_dnd_in_progress || x_dnd_waiting_for_finish)
    error ("A drag-and-drop session is already in progress");

  DEFER_SELECTIONS;

  /* If local_value is nil, then we lost ownership of XdndSelection.
     Signal a more informative error than args-out-of-range.  */
  if (NILP (local_value))
    error ("No local value for XdndSelection");

  if (popup_activated ())
    error ("Trying to drag-and-drop from within a menu-entry");

  x_set_dnd_targets (target_atoms, ntargets);
  record_unwind_protect_void (x_free_dnd_targets);
  record_unwind_protect_void (x_clear_dnd_action);

  ltimestamp = x_timestamp_for_selection (FRAME_DISPLAY_INFO (f),
					  QXdndSelection);

  if (NILP (ltimestamp))
    error ("No local value for XdndSelection");

  if (BIGNUMP (ltimestamp))
    x_dnd_selection_timestamp = bignum_to_intmax (ltimestamp);
  else
    x_dnd_selection_timestamp = XFIXNUM (ltimestamp);

  /* Release ownership of XdndSelection after this function returns.
     VirtualBox uses the owner of XdndSelection to determine whether
     or not mouse motion is part of a drag-and-drop operation.  */

  if (!x_dnd_preserve_selection_data)
    record_unwind_protect (x_dnd_lose_ownership,
			   Fcons (ltimestamp, frame));

  x_dnd_motif_operations
    = xm_side_effect_from_action (FRAME_DISPLAY_INFO (f), xaction);

  x_dnd_first_motif_operation = XM_DRAG_NOOP;

  if (n_ask_actions)
    {
      x_dnd_motif_operations
	= xm_operations_from_actions (FRAME_DISPLAY_INFO (f),
				      ask_action_list,
				      n_ask_actions);
      x_dnd_first_motif_operation
	= xm_side_effect_from_action (FRAME_DISPLAY_INFO (f),
				      ask_action_list[0]);

      record_unwind_protect (x_dnd_delete_action_list, frame);

      ask_actions = NULL;
      end = 0;
      count = SPECPDL_INDEX ();

      for (i = 0; i < n_ask_actions; ++i)
	{
	  fill = end;
	  end += strlen (ask_action_names[i]) + 1;

	  if (ask_actions)
	    ask_actions = xrealloc (ask_actions, end);
	  else
	    ask_actions = xmalloc (end);

	  strncpy (ask_actions + fill,
		   ask_action_names[i],
		   end - fill);
	}

      prop.value = (unsigned char *) ask_actions;
      prop.encoding = XA_STRING;
      prop.format = 8;
      prop.nitems = end;

      record_unwind_protect_ptr (xfree, ask_actions);

      /* This can potentially store a lot of data in window
	 properties, so check for allocation errors.  */
      block_input ();
      x_catch_errors (FRAME_X_DISPLAY (f));
      XSetTextProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			&prop, FRAME_DISPLAY_INFO (f)->Xatom_XdndActionDescription);

      XChangeProperty (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
		       FRAME_DISPLAY_INFO (f)->Xatom_XdndActionList, XA_ATOM, 32,
		       PropModeReplace, (unsigned char *) ask_action_list,
		       n_ask_actions);
      x_check_errors (FRAME_X_DISPLAY (f),
		      "Can't set action descriptions: %s");
      x_uncatch_errors_after_check ();
      unblock_input ();

      unbind_to (count, Qnil);
    }

  record_unwind_protect_void (x_clear_dnd_variables);

  if (follow_tooltip)
    {
#if defined HAVE_XRANDR || defined USE_GTK
      x_dnd_monitors
	= FRAME_DISPLAY_INFO (f)->last_monitor_attributes_list;

      if (NILP (x_dnd_monitors))
#endif
	x_dnd_monitors
	  = Fx_display_monitor_attributes_list (frame);
    }

  x_dnd_update_tooltip = follow_tooltip;

  /* This shouldn't happen.  */
  if (x_dnd_toplevels)
    x_dnd_free_toplevels (true);

#ifdef USE_GTK
  /* Prevent GTK+ timeouts from being run, since they can call
     handle_one_xevent behind our back.  */
  suppress_xg_select ();
  record_unwind_protect_void (release_xg_select);
#endif

  /* Set up a meaningless alias.  */
  XSETCAR (x_dnd_selection_alias_cell, QSECONDARY);
  XSETCDR (x_dnd_selection_alias_cell, QSECONDARY);

  /* Bind this here.  The cell doesn't actually alias between
     anything until `xm_setup_dnd_targets' is called.  */
  specbind (Qx_selection_alias_alist,
	    Fcons (x_dnd_selection_alias_cell,
		   Vx_selection_alias_alist));

  /* Initialize most of the state for the drag-and-drop operation.  */
  x_dnd_in_progress = true;
  x_dnd_recursion_depth = command_loop_level + minibuf_level;
  x_dnd_frame = f;
  x_dnd_last_seen_window = None;
  x_dnd_last_seen_toplevel = None;
  x_dnd_last_protocol_version = -1;
  x_dnd_last_window_is_frame = false;
  x_dnd_last_motif_style = XM_DRAG_STYLE_NONE;
  x_dnd_mouse_rect_target = None;
  x_dnd_action = None;
  x_dnd_action_symbol = Qnil;
  x_dnd_wanted_action = xaction;
  x_dnd_return_frame = 0;
  x_dnd_waiting_for_finish = false;
  x_dnd_waiting_for_motif_finish = 0;
  x_dnd_waiting_for_status_window = None;
  x_dnd_pending_send_position.type = 0;
  x_dnd_xm_use_help = false;
  x_dnd_motif_setup_p = false;
  x_dnd_end_window = None;
  x_dnd_run_unsupported_drop_function = false;
  x_dnd_use_toplevels
    = x_wm_supports (f, FRAME_DISPLAY_INFO (f)->Xatom_net_client_list_stacking);
  x_dnd_last_tooltip_valid = false;
  x_dnd_toplevels = NULL;
  x_dnd_allow_current_frame = allow_current_frame;
  x_dnd_movement_frame = NULL;
  x_dnd_wheel_frame = NULL;
  x_dnd_init_type_lists = false;
  x_dnd_need_send_drop = false;

#ifdef HAVE_XINPUT2

  if (FRAME_DISPLAY_INFO (f)->supports_xi2)
    {
      /* Only accept input from the last master pointer to have interacted
	 with Emacs.  This prevents another pointer device getting our
	 idea of the button state messed up.  */
      if (FRAME_DISPLAY_INFO (f)->client_pointer_device != -1)
	x_dnd_pointer_device
	  = FRAME_DISPLAY_INFO (f)->client_pointer_device;
      else
	/* This returns Bool but cannot actually fail.  */
	XIGetClientPointer (FRAME_X_DISPLAY (f), None,
			    &x_dnd_pointer_device);

      x_dnd_keyboard_device = -1;

      device = xi_device_from_id (FRAME_DISPLAY_INFO (f),
				  x_dnd_pointer_device);

      if (device)
	x_dnd_keyboard_device = device->attachment;
    }
  else
    {
      x_dnd_pointer_device = -1;
      x_dnd_keyboard_device = -1;
    }

#endif

#ifdef HAVE_XKB
  x_dnd_keyboard_state = 0;

  if (FRAME_DISPLAY_INFO (f)->supports_xkb)
    {
      XkbSelectEvents (FRAME_X_DISPLAY (f), XkbUseCoreKbd,
		       XkbStateNotifyMask, XkbStateNotifyMask);
      XkbGetState (FRAME_X_DISPLAY (f), XkbUseCoreKbd,
		   &keyboard_state);

      x_dnd_keyboard_state = (keyboard_state.mods
			      | keyboard_state.ptr_buttons);
    }
#endif

  if (x_dnd_use_toplevels)
    {
      if (x_dnd_compute_toplevels (FRAME_DISPLAY_INFO (f)))
	{
	  x_dnd_free_toplevels (true);
	  x_dnd_use_toplevels = false;
	}
      else
	record_unwind_protect_void (x_free_dnd_toplevels);
    }

  if (!NILP (return_frame))
    x_dnd_return_frame = 1;

  if (EQ (return_frame, Qnow))
    x_dnd_return_frame = 2;

  /* Now select for SubstructureNotifyMask and PropertyChangeMask on
     the root window, so we can get notified when window stacking
     changes, a common operation during drag-and-drop.  */

  XGetWindowAttributes (FRAME_X_DISPLAY (f),
			FRAME_DISPLAY_INFO (f)->root_window,
			&root_window_attrs);

  additional_mask = SubstructureNotifyMask;

  if (x_dnd_use_toplevels)
    additional_mask |= PropertyChangeMask;

  XSelectInput (FRAME_X_DISPLAY (f),
		FRAME_DISPLAY_INFO (f)->root_window,
		root_window_attrs.your_event_mask
		| additional_mask);

  if (EQ (return_frame, Qnow))
    x_dnd_update_state (FRAME_DISPLAY_INFO (f), CurrentTime);

  while (x_dnd_in_progress || x_dnd_waiting_for_finish)
    {
      EVENT_INIT (hold_quit);

#ifdef USE_GTK
      current_finish = X_EVENT_NORMAL;
      current_hold_quit = &hold_quit;
      current_count = 0;
      xg_pending_quit_event.kind = NO_EVENT;
#endif

      block_input ();
      x_dnd_inside_handle_one_xevent = true;
#ifdef USE_GTK
      gtk_main_iteration ();
#elif defined USE_X_TOOLKIT
      XtAppNextEvent (Xt_app_con, &next_event);
#else
      x_next_event_from_any_display (&next_event);
#endif

#ifndef USE_GTK
      event_display
	= x_display_info_for_display (next_event.xany.display);

      if (event_display)
	{
#ifdef HAVE_X_I18N
#ifdef HAVE_XINPUT2
	  if (next_event.type != GenericEvent
	      || !event_display->supports_xi2
	      || (next_event.xgeneric.extension
		  != event_display->xi2_opcode))
	    {
#endif
	      if (!x_filter_event (event_display, &next_event))
		handle_one_xevent (event_display,
				   &next_event, &finish, &hold_quit);
#ifdef HAVE_XINPUT2
	    }
	  else
	    handle_one_xevent (event_display,
			       &next_event, &finish, &hold_quit);
#endif
#else
	  handle_one_xevent (event_display,
			     &next_event, &finish, &hold_quit);
#endif
	}
#else
      /* Clear these before the read_socket_hook can be called.  */
      current_count = -1;
      current_hold_quit = NULL;
#endif
      x_dnd_inside_handle_one_xevent = false;

      /* Clean up any event handlers that are now out of date.  */
      x_clean_failable_requests (FRAME_DISPLAY_INFO (f));

      /* The unblock_input below might try to read input, but
	 XTread_socket does nothing inside a drag-and-drop event
	 loop, so don't let it clear the pending_signals flag.  */
      signals_were_pending = pending_signals;
      unblock_input ();
      pending_signals = signals_were_pending;

      /* Ignore mouse movement from displays that aren't the DND
	 display.  */
#ifndef USE_GTK
      if (event_display == FRAME_DISPLAY_INFO (f))
	{
#endif
	  if (x_dnd_movement_frame
	      /* FIXME: how come this can end up with movement frames
		 from other displays on GTK builds?  */
	      && (FRAME_X_DISPLAY (x_dnd_movement_frame)
		  == FRAME_X_DISPLAY (f))
	      /* If both those variables are false, then F is no
		 longer protected from deletion by Lisp code.  This
		 can only happen during the final iteration of the DND
		 event loop.  */
	      && (x_dnd_in_progress || x_dnd_waiting_for_finish))
	    {
	      XSETFRAME (frame_object, x_dnd_movement_frame);
	      XSETINT (x, x_dnd_movement_x);
	      XSETINT (y, x_dnd_movement_y);
	      x_dnd_movement_frame = NULL;

	      if (!NILP (Vx_dnd_movement_function)
		  && FRAME_LIVE_P (XFRAME (frame_object))
		  && !FRAME_TOOLTIP_P (XFRAME (frame_object))
		  && x_dnd_movement_x >= 0
		  && x_dnd_movement_y >= 0
		  && x_dnd_frame
		  && (XFRAME (frame_object) != x_dnd_frame
		      || x_dnd_allow_current_frame))
		{
		  x_dnd_old_window_attrs = root_window_attrs;
		  x_dnd_unwind_flag = true;

		  ref = SPECPDL_INDEX ();
		  record_unwind_protect_ptr (x_dnd_cleanup_drag_and_drop, f);
		  calln (Vx_dnd_movement_function, frame_object,
			 Fposn_at_x_y (x, y, frame_object, Qnil));
		  x_dnd_unwind_flag = false;
		  unbind_to (ref, Qnil);

		  /* Redisplay this way to preserve the echo area.
		     Otherwise, the contents will abruptly disappear
		     when the mouse moves over a frame.  */
		  redisplay_preserve_echo_area (33);
		}
	    }

	  if (x_dnd_wheel_frame
	      && (x_dnd_in_progress || x_dnd_waiting_for_finish))
	    {
	      XSETFRAME (frame_object, x_dnd_wheel_frame);
	      XSETINT (x, x_dnd_wheel_x);
	      XSETINT (y, x_dnd_wheel_y);
	      x_dnd_wheel_frame = NULL;

	      if (!NILP (Vx_dnd_wheel_function)
		  && FRAME_LIVE_P (XFRAME (frame_object))
		  && !FRAME_TOOLTIP_P (XFRAME (frame_object))
		  && x_dnd_movement_x >= 0
		  && x_dnd_movement_y >= 0
		  && x_dnd_frame
		  && (XFRAME (frame_object) != x_dnd_frame
		      || x_dnd_allow_current_frame))
		{
		  x_dnd_old_window_attrs = root_window_attrs;
		  x_dnd_unwind_flag = true;

		  ref = SPECPDL_INDEX ();
		  record_unwind_protect_ptr (x_dnd_cleanup_drag_and_drop, f);
		  calln (Vx_dnd_wheel_function,
			 Fposn_at_x_y (x, y, frame_object, Qnil),
			 make_fixnum (x_dnd_wheel_button),
			 make_uint (x_dnd_wheel_state),
			 make_uint (x_dnd_wheel_time));
		  x_dnd_unwind_flag = false;
		  unbind_to (ref, Qnil);

		  /* Redisplay this way to preserve the echo area.
		     Otherwise, the contents will abruptly disappear
		     when the mouse moves over a frame.  */
		  redisplay_preserve_echo_area (33);
		}
	    }

	  if (hold_quit.kind != NO_EVENT)
	    {
	      x_dnd_process_quit (f, hold_quit.timestamp);
#ifdef USE_GTK
	      current_hold_quit = NULL;
#endif
	      /* Restore the old event mask.  */
	      x_restore_events_after_dnd (f, &root_window_attrs);

	      /* Call kbd_buffer_store event, which calls
		 handle_interrupt and sets `last-event-frame' along
		 with various other things.  */
	      kbd_buffer_store_event (&hold_quit);
	      /* Now quit anyway.  */
	      quit ();
	    }

	  if (pending_selection_requests
	      && (x_dnd_in_progress || x_dnd_waiting_for_finish))
	    {
	      x_dnd_old_window_attrs = root_window_attrs;
	      x_dnd_unwind_flag = true;

	      ref = SPECPDL_INDEX ();
	      record_unwind_protect_ptr (x_dnd_cleanup_drag_and_drop, f);
	      x_handle_pending_selection_requests ();
	      x_dnd_unwind_flag = false;
	      unbind_to (ref, Qnil);
	    }

	  /* Sometimes C-g can be pressed inside a selection
	     converter, where quitting is inhibited.  We want
	     to quit after the converter exits.  */
	  if (!NILP (Vquit_flag) && !NILP (Vinhibit_quit))
	    {
	      x_dnd_process_quit (f, FRAME_DISPLAY_INFO (f)->last_user_time);
#ifdef USE_GTK
	      current_hold_quit = NULL;
#endif
	      x_restore_events_after_dnd (f, &root_window_attrs);
	      quit ();
	    }

	  if (x_dnd_run_unsupported_drop_function
	      && x_dnd_waiting_for_finish)
	    {
	      x_dnd_run_unsupported_drop_function = false;
	      x_dnd_waiting_for_finish = false;
	      x_dnd_unwind_flag = true;

	      ref = SPECPDL_INDEX ();
	      record_unwind_protect_ptr (x_dnd_cleanup_drag_and_drop, f);

	      if (!NILP (Vx_dnd_unsupported_drop_function))
		val = calln (Vx_dnd_unsupported_drop_function,
			     XCAR (XCDR (x_dnd_unsupported_drop_data)),
			     Fnth (make_fixnum (3), x_dnd_unsupported_drop_data),
			     Fnth (make_fixnum (4), x_dnd_unsupported_drop_data),
			     Fnth (make_fixnum (2), x_dnd_unsupported_drop_data),
			     make_uint (x_dnd_unsupported_drop_window),
			     frame, make_uint (x_dnd_unsupported_drop_time),
			     Fcopy_sequence (XCAR (x_dnd_unsupported_drop_data)));
	      else
		val = Qnil;

	      if (NILP (val))
		x_dnd_do_unsupported_drop (FRAME_DISPLAY_INFO (f),
					   frame, XCAR (x_dnd_unsupported_drop_data),
					   XCAR (XCDR (x_dnd_unsupported_drop_data)),
					   x_dnd_unsupported_drop_window,
					   XFIXNUM (Fnth (make_fixnum (3),
							  x_dnd_unsupported_drop_data)),
					   XFIXNUM (Fnth (make_fixnum (4),
							  x_dnd_unsupported_drop_data)),
					   x_dnd_unsupported_drop_time);
	      else if (SYMBOLP (val))
		x_dnd_action_symbol = val;

	      x_dnd_unwind_flag = false;
	      unbind_to (ref, Qnil);

	      /* Break out of the loop now, since DND has
		 completed.  */
	      break;
	    }

#ifdef USE_GTK
	  if (xg_pending_quit_event.kind != NO_EVENT)
	    {
	      xg_pending_quit_event.kind = NO_EVENT;
	      current_hold_quit = NULL;

	      x_dnd_process_quit (f, FRAME_DISPLAY_INFO (f)->last_user_time);
	      x_restore_events_after_dnd (f, &root_window_attrs);
	      quit ();
	    }
#else
	}
      else
	{
	  if (x_dnd_movement_frame)
	    x_dnd_movement_frame = NULL;

	  if (x_dnd_wheel_frame)
	    x_dnd_wheel_frame = NULL;

	  if (hold_quit.kind != NO_EVENT)
	    EVENT_INIT (hold_quit);
	}
#endif
    }

  x_dnd_waiting_for_finish = false;

#ifdef USE_GTK
  current_hold_quit = NULL;
#endif
  x_dnd_movement_frame = NULL;
  x_dnd_wheel_frame = NULL;
  x_restore_events_after_dnd (f, &root_window_attrs);

  if (x_dnd_return_frame == 3
      && FRAME_LIVE_P (x_dnd_return_frame_object))
    {
      /* Deliberately preserve the last device if
	 x_dnd_return_frame_object is the drag source.  */

      if (x_dnd_return_frame_object != x_dnd_frame)
	x_dnd_return_frame_object->last_mouse_device = Qnil;

      x_dnd_return_frame_object->mouse_moved = true;

      XSETFRAME (action, x_dnd_return_frame_object);
      x_dnd_return_frame_object = NULL;

      return unbind_to (base, action);
    }

  x_dnd_return_frame_object = NULL;
  FRAME_DISPLAY_INFO (f)->grabbed = 0;

  if (!NILP (x_dnd_action_symbol))
    return unbind_to (base, x_dnd_action_symbol);

  if (x_dnd_action != None)
    {
      block_input ();
      x_catch_errors (FRAME_X_DISPLAY (f));
      atom_name = x_get_atom_name (FRAME_DISPLAY_INFO (f),
				   x_dnd_action, &need_sync);

      if (need_sync)
	x_uncatch_errors ();
      else
	/* No protocol request actually happened, so avoid the extra
	   sync by calling x_uncatch_errors_after_check instead.  */
	x_uncatch_errors_after_check ();

      if (atom_name)
	{
	  action = intern (atom_name);
	  xfree (atom_name);
	}
      else
	action = Qnil;
      unblock_input ();

      return unbind_to (base, action);
    }

  return unbind_to (base, Qnil);
}

#ifdef HAVE_XINPUT2

/* Disable per-device keyboard focus tracking within X toolkit and GTK
   2.x builds, given that these builds receive updates to the keyboard
   input focus as core events.  */

#if !defined USE_X_TOOLKIT && (!defined USE_GTK || defined HAVE_GTK3)

/* Since the input extension assigns a keyboard focus to each master
   device, there is no longer a 1:1 correspondence between the
   selected frame and the focus frame immediately after the keyboard
   focus is switched to a given frame.  This situation is handled by
   keeping track of each master device's focus frame, the time of the
   last interaction with that frame, and always keeping the focus on
   the most recently selected frame.  We also use the pointer of the
   device that is keeping the current frame focused in functions like
   `mouse-position'.  */

static void
xi_handle_focus_change (struct x_display_info *dpyinfo)
{
  struct input_event ie;
  struct frame *focus, *new;
  struct xi_device_t *device, *source = NULL;
  ptrdiff_t i;
  Time time;
#ifdef USE_GTK
  struct x_output *output;
  GtkWidget *widget;
#endif

  focus = dpyinfo->x_focus_frame;
  new = NULL;
  time = 0;

  dpyinfo->client_pointer_device = -1;

  for (i = 0; i < dpyinfo->num_devices; ++i)
    {
      device = &dpyinfo->devices[i];

      if (device->focus_frame
	  && device->focus_frame_time > time)
	{
	  new = device->focus_frame;
	  time = device->focus_frame_time;
	  source = device;

	  /* Use this device for future calls to `mouse-position' etc.
	     If it is a keyboard, use its attached pointer.  */

	  if (device->use == XIMasterKeyboard)
	    dpyinfo->client_pointer_device = device->attachment;
	  else
	    dpyinfo->client_pointer_device = device->device_id;
	}
      /* Even if the implicit focus was set after the explicit focus
	 on this specific device, the explicit focus is what really
	 matters.  So use it instead.  */
      else if (device->focus_implicit_frame
	       && device->focus_implicit_time > time)
	{
	  new = device->focus_implicit_frame;
	  time = device->focus_implicit_time;
	  source = device;

	  /* Use this device for future calls to `mouse-position' etc.
	     If it is a keyboard, use its attached pointer.  */

	  if (device->use == XIMasterKeyboard)
	    dpyinfo->client_pointer_device = device->attachment;
	  else
	    dpyinfo->client_pointer_device = device->device_id;
	}
    }

  if (new != focus && focus)
    {
#ifdef HAVE_X_I18N
      if (FRAME_XIC (focus))
	XUnsetICFocus (FRAME_XIC (focus));
#endif

#ifdef USE_GTK
      output = FRAME_X_OUTPUT (focus);

      if (x_gtk_use_native_input)
	{
	  gtk_im_context_focus_out (output->im_context);
	  gtk_im_context_set_client_window (output->im_context,
					    NULL);
	}
#endif

      EVENT_INIT (ie);
      ie.kind = FOCUS_OUT_EVENT;
      XSETFRAME (ie.frame_or_window, focus);

      kbd_buffer_store_event (&ie);
    }

  if (new != focus && new)
    {
#ifdef HAVE_X_I18N
      if (FRAME_XIC (new))
	XSetICFocus (FRAME_XIC (new));
#endif

#ifdef USE_GTK
      output = FRAME_X_OUTPUT (new);

      if (x_gtk_use_native_input)
	{
	  widget = FRAME_GTK_OUTER_WIDGET (new);

	  gtk_im_context_focus_in (output->im_context);
	  gtk_im_context_set_client_window (output->im_context,
					    gtk_widget_get_window (widget));
	}
#endif

      EVENT_INIT (ie);
      ie.kind = FOCUS_IN_EVENT;
      ie.device = source->name;
      XSETFRAME (ie.frame_or_window, new);

      kbd_buffer_store_event (&ie);
    }

  x_new_focus_frame (dpyinfo, new);
}

static void
xi_focus_handle_for_device (struct x_display_info *dpyinfo,
			    struct frame *mentioned_frame,
			    XIEvent *base_event)
{
  struct xi_device_t *device;
  XIEnterEvent *event;

  /* XILeaveEvent, XIFocusInEvent, etc are just synonyms for
     XIEnterEvent.  */
  event = (XIEnterEvent *) base_event;
  device = xi_device_from_id (dpyinfo, event->deviceid);

  if (!device)
    return;

  switch (event->evtype)
    {
    case XI_FocusIn:
      /* The last-focus-change time of the device changed, so update the
	 frame's user time.  */
      x_display_set_last_user_time (dpyinfo, event->time,
				    event->send_event, true);

      device->focus_frame = mentioned_frame;
      device->focus_frame_time = event->time;
      break;

    case XI_FocusOut:
      /* The last-focus-change time of the device changed, so update the
	 frame's user time.  */
      x_display_set_last_user_time (dpyinfo, event->time,
				    event->send_event, false);
      device->focus_frame = NULL;

      /* So, unfortunately, the X Input Extension is implemented such
	 that XI_Leave events will not have their focus field set if
	 the core focus is transferred to another window after an
	 entry event that pretends to (or really does) set the
	 implicit focus.  In addition, if the core focus is set, but
	 the extension focus on the client pointer is not, all
	 XI_Enter events will have their focus fields set, despite not
	 actually changing the effective focus window.  Combined with
	 almost all window managers not setting the focus on input
	 extension devices, this means that Emacs will continue to
	 think the implicit focus is set on one of its frames if the
	 actual (core) focus is transferred to another window while
	 the pointer remains inside a frame.  The only workaround in
	 this case is to clear the implicit focus along with
	 XI_FocusOut events, which is not correct at all, but better
	 than leaving frames in an incorrectly-focused state.
	 (bug#57468) */
      device->focus_implicit_frame = NULL;
      break;

    case XI_Enter:
      if (!event->focus)
	break;

      if (device->use == XIMasterPointer)
	device = xi_device_from_id (dpyinfo, device->attachment);

      if (!device)
	break;

      device->focus_implicit_frame = mentioned_frame;
      device->focus_implicit_time = event->time;
      break;

    case XI_Leave:
      if (!event->focus)
	break;

      if (device->use == XIMasterPointer)
	device = xi_device_from_id (dpyinfo, device->attachment);

      if (!device)
	break;

      device->focus_implicit_frame = NULL;
      break;
    }

  xi_handle_focus_change (dpyinfo);
}

#endif /* !USE_X_TOOLKIT && (!USE_GTK || HAVE_GTK3) */

static void
xi_handle_delete_frame (struct x_display_info *dpyinfo,
			struct frame *f)
{
  struct xi_device_t *device;
  ptrdiff_t i;

  for (i = 0; i < dpyinfo->num_devices; ++i)
    {
      device = &dpyinfo->devices[i];

      if (device->focus_frame == f)
	device->focus_frame = NULL;

      if (device->focus_implicit_frame == f)
	device->focus_implicit_frame = NULL;
    }
}

/* Handle an interaction by DEVICE on frame F.  TIME is the time of
   the interaction; if F isn't currently the global focus frame, but
   is the focus of DEVICE, make it the focus frame.  */

static void
xi_handle_interaction (struct x_display_info *dpyinfo,
		       struct frame *f, struct xi_device_t *device,
		       Time time)
{
#if !defined USE_X_TOOLKIT && (!defined USE_GTK || defined HAVE_GTK3)
  bool change;

  /* If DEVICE is a pointer, use its attached keyboard device.  */
  if (device->use == XIMasterPointer)
    device = xi_device_from_id (dpyinfo, device->attachment);

  if (!device)
    return;

  change = false;

  if (device->focus_frame == f)
    {
      device->focus_frame_time = time;
      change = true;
    }

  if (device->focus_implicit_frame == f)
    {
      device->focus_implicit_time = time;
      change = true;
    }

  /* If F isn't currently focused, update the focus state.  */
  if (change && f != dpyinfo->x_focus_frame)
    xi_handle_focus_change (dpyinfo);
#endif /* !USE_X_TOOLKIT && (!USE_GTK || HAVE_GTK3) */
}

/* Return whether or not XEV actually represents a change in the
   position of the pointer on DEVICE, with respect to the last event
   received.  This is necessary because the input extension reports
   motion events in very high resolution, while Emacs is only fast
   enough to process motion events aligned to the pixel grid.  */

static bool
xi_position_changed (struct xi_device_t *device, XIDeviceEvent *xev)
{
  bool changed;

  changed = true;

  if (xev->event != device->last_motion_window)
    goto out;

  if (lrint (xev->event_x) == device->last_motion_x
      && lrint (xev->event_y) == device->last_motion_y)
    {
      changed = false;
      goto out;
    }

 out:
  device->last_motion_x = lrint (xev->event_x);
  device->last_motion_y = lrint (xev->event_y);
  device->last_motion_window = xev->event;

  return changed;
}

static void
xi_report_motion_window_clear (struct xi_device_t *device)
{
  device->last_motion_window = None;
}

#ifdef HAVE_XINPUT2_1

/* Look up a scroll valuator in DEVICE by NUMBER.  */

static struct xi_scroll_valuator_t *
xi_get_scroll_valuator (struct xi_device_t *device, int number)
{
  int i;

  for (i = 0; i < device->scroll_valuator_count; ++i)
    {
      if (device->valuators[i].number == number)
	return &device->valuators[i];
    }

  return NULL;
}

/* Check if EVENT, a DeviceChanged event, contains any scroll
   valuators.  */

static bool
xi_has_scroll_valuators (XIDeviceChangedEvent *event)
{
  int i;

  for (i = 0; i < event->num_classes; ++i)
    {
      if (event->classes[i]->type == XIScrollClass)
	return true;
    }

  return false;
}

/* Repopulate the information (touchpoint tracking information, scroll
   valuators, etc) in DEVICE with the device classes provided in
   CLASSES.  This is called upon receiving a DeviceChanged event.

   This function is not present on XI 2.0 as there are no worthwhile
   classes there.  */

static void
xi_handle_new_classes (struct x_display_info *dpyinfo, struct xi_device_t *device,
		       XIAnyClassInfo **classes, int num_classes)
{
  XIScrollClassInfo *scroll;
  struct xi_scroll_valuator_t *valuator;
  XIValuatorClassInfo *valuator_info;
  int i;
#ifdef HAVE_XINPUT2_2
  XITouchClassInfo *touch;
#endif

  if (dpyinfo->xi2_version < 1)
    /* Emacs is connected to an XI 2.0 server, which reports no
       classes of interest.  */
    return;

  device->valuators = xnmalloc (num_classes,
				sizeof *device->valuators);
  device->scroll_valuator_count = 0;
#ifdef HAVE_XINPUT2_2
  device->direct_p = false;
#endif /* HAVE_XINPUT2_2 */

  for (i = 0; i < num_classes; ++i)
    {
      switch (classes[i]->type)
	{
	case XIScrollClass:
	  scroll = (XIScrollClassInfo *) classes[i];

	  xi_populate_scroll_valuator (device,
				       device->scroll_valuator_count++,
				       scroll);
	  break;

#ifdef HAVE_XINPUT2_2
	case XITouchClass:
	  touch = (XITouchClassInfo *) classes[i];

	  /* touch_info->mode indicates the coordinate space that this
	     device reports in its touch events.

	     DirectTouch means that the device uses a coordinate space
	     that corresponds to locations on the screen.  It is set
	     by touch screen devices which are overlaid over the
	     raster itself.

	     The other value (DependentTouch) means that the device
	     uses a separate abstract coordinate space corresponding
	     to its own surface.  Emacs ignores events from these
	     devices because it does not support recognizing touch
	     gestures from surfaces other than the screen.

	     Master devices may report multiple touch classes for
	     attached slave devices, leaving the nature of touch
	     events they send ambiguous.  The problem of
	     discriminating between these events is bypassed entirely
	     through only processing touch events from the slave
	     devices where they originate.  */

	  if (touch->mode == XIDirectTouch)
	    device->direct_p = true;
	  else
	    device->direct_p = false;

	  break;
#endif /* HAVE_XINPUT2_2 */
	}
    }

  /* Restore the values of any scroll valuators that we already
     know about.  */

  for (i = 0; i < num_classes; ++i)
    {
      if (classes[i]->type != XIValuatorClass)
	continue;

      valuator_info = (XIValuatorClassInfo *) classes[i];

      /* Avoid restoring bogus values if some driver accidentally
	 specifies relative values in scroll valuator classes how the
	 input extension spec says they should be, but allow restoring
	 values when a value is set, which is how the input extension
	 actually behaves.  */

      if (valuator_info->value == 0.0
	  && valuator_info->mode != XIModeAbsolute)
	continue;

      valuator = xi_get_scroll_valuator (device,
					 valuator_info->number);

      if (!valuator)
	continue;

      valuator->invalid_p = false;
      valuator->current_value = valuator_info->value;
      valuator->emacs_value = 0;

      break;
    }
}

#endif /* HAVE_XINPUT2_1 */

/* Handle EVENT, a DeviceChanged event.  Look up the device that
   changed, and update its information with the data in EVENT.  */

static void
xi_handle_device_changed (struct x_display_info *dpyinfo,
			  struct xi_device_t *device,
			  XIDeviceChangedEvent *event)
{
#ifdef HAVE_XINPUT2_1
  int ndevices;
  XIDeviceInfo *info;
#endif
#ifdef HAVE_XINPUT2_2
  struct xi_touch_point_t *tem, *last;
#endif

#ifdef HAVE_XINPUT2_1
  if (xi_has_scroll_valuators (event))
    /* Scroll valuators are provided by this event.  Use the values
       provided in this event to populate the device's new scroll
       valuator list: if this event is a SlaveSwitch event caused by
       wheel movement, then querying for the device info will probably
       return the value after the wheel movement, leading to a delta
       of 0 being computed upon handling the subsequent XI_Motion
       event.  (bug#58980) */
    xi_handle_new_classes (dpyinfo, device, event->classes,
			   event->num_classes);
  else
    {
      /* When a DeviceChange event is received for a master device,
	 the X server sometimes does not send any scroll valuators
	 along with it.  This is possibly an X server bug but I really
	 don't want to dig any further, so fetch the scroll valuators
	 manually.  (bug#57020) */

      x_catch_errors (dpyinfo->display);
      info = XIQueryDevice (dpyinfo->display, event->deviceid,
			    /* ndevices is always 1 if a deviceid is
			       specified.  If the request fails, NULL will
			       be returned.  */
			    &ndevices);
      x_uncatch_errors ();

      if (!info)
	return;

      /* info contains the classes currently associated with the
	 event.  Apply them.  */
      xi_handle_new_classes (dpyinfo, device, info->classes,
			     info->num_classes);
    }
#endif

#ifdef HAVE_XINPUT2_2
  /* The device is no longer a DirectTouch device, so remove any
     touchpoints that we might have recorded.  */
  if (!device->direct_p)
    {
      tem = device->touchpoints;

      while (tem)
	{
	  last = tem;
	  tem = tem->next;
	  xfree (last);
	}

      device->touchpoints = NULL;
    }
#endif
}

/* Remove the client-side record of every device in TO_DISABLE.
   Called while processing XI_HierarchyChanged events.  We batch up
   multiple disabled devices because it is more efficient to disable
   them at once.  */

static void
xi_disable_devices (struct x_display_info *dpyinfo,
		    int *to_disable, int n_disabled)
{
  struct xi_device_t *devices;
  int ndevices, i, j;
#ifdef HAVE_XINPUT2_2
  struct xi_touch_point_t *tem, *last;
#endif
#if defined HAVE_XINPUT2_2 && !defined HAVE_EXT_TOOL_BAR
  struct x_output *output;
  Lisp_Object tail, frame;
#endif

  /* Don't pointlessly copy dpyinfo->devices if there are no devices
     to disable.  */
  if (!n_disabled)
    return;

  ndevices = 0;
  devices = xzalloc (sizeof *devices * dpyinfo->num_devices);

  /* Loop through every device currently in DPYINFO, and copy it to
     DEVICES if it is not in TO_DISABLE.  Note that this function
     should be called with input blocked, since xfree can otherwise
     call GC, which will call mark_xterm with invalid state.  */
  for (i = 0; i < dpyinfo->num_devices; ++i)
    {
      for (j = 0; j < n_disabled; ++j)
	{
	  if (to_disable[j] == dpyinfo->devices[i].device_id)
	    {
	      if (x_dnd_in_progress
		  /* If the drag-and-drop pointer device is being
		     disabled, then cancel the drag and drop
		     operation.  */
		  && to_disable[j] == x_dnd_pointer_device)
		x_dnd_cancel_dnd_early ();

	      /* Free any scroll valuators that might be on this
		 device.  */
#ifdef HAVE_XINPUT2_1
	      xfree (dpyinfo->devices[i].valuators);
#endif

	      /* Free any currently active touch points on this
		 device.  */
#ifdef HAVE_XINPUT2_2
	      tem = dpyinfo->devices[i].touchpoints;
	      while (tem)
		{
		  last = tem;
		  tem = tem->next;
		  xfree (last);
		}

#ifndef HAVE_EXT_TOOL_BAR

	      /* Now look through each frame on DPYINFO.  If it has an
		 outstanding tool bar press for this device, release
		 the tool bar.  */

	      FOR_EACH_FRAME (tail, frame)
		{
		  if (!FRAME_X_P (XFRAME (frame))
		      || (FRAME_DISPLAY_INFO (XFRAME (frame))
			  != dpyinfo))
		    continue;

		  output = FRAME_OUTPUT_DATA (XFRAME (frame));

		  if (output->tool_bar_touch_device
		      == dpyinfo->devices[i].device_id)
		    {
		      if (XFRAME (frame)->last_tool_bar_item != -1
			  && WINDOWP (XFRAME (frame)->tool_bar_window))
			handle_tool_bar_click (XFRAME (frame), 0, 0,
					       false, 0);

		      output->tool_bar_touch_device = 0;
		    }
		}
#endif
#endif

	      goto out;
	    }
	}

      devices[ndevices++] = dpyinfo->devices[i];

    out:
      continue;
    }

  /* Free the old devices array and replace it with ndevices.  */
  xfree (dpyinfo->devices);

  dpyinfo->devices = devices;
  dpyinfo->num_devices = ndevices;
}

#endif

/* The focus may have changed.  Figure out if it is a real focus change,
   by checking both FocusIn/Out and Enter/LeaveNotify events.

   Returns FOCUS_IN_EVENT event in *BUFP. */

static void
x_detect_focus_change (struct x_display_info *dpyinfo, struct frame *frame,
		       const XEvent *event, struct input_event *bufp)
{
  if (!frame)
    return;

  switch (event->type)
    {
    case EnterNotify:
    case LeaveNotify:
      {
        struct frame *focus_frame = dpyinfo->x_focus_event_frame;
        int focus_state
          = focus_frame ? focus_frame->output_data.x->focus_state : 0;

        if (event->xcrossing.detail != NotifyInferior
            && event->xcrossing.focus
            && ! (focus_state & FOCUS_EXPLICIT))
          x_focus_changed ((event->type == EnterNotify ? FocusIn : FocusOut),
			   FOCUS_IMPLICIT,
			   dpyinfo, frame, bufp);
      }
      break;

    case FocusIn:
    case FocusOut:
      /* Ignore transient focus events from hotkeys, window manager
         gadgets, and other odd sources.  Some buggy window managers
         (e.g., Muffin 4.2.4) send FocusIn events of this type without
         corresponding FocusOut events even when some other window
         really has focus, and these kinds of focus event don't
         correspond to real user input changes.  GTK+ uses the same
         filtering. */
      if (event->xfocus.mode == NotifyGrab
          || event->xfocus.mode == NotifyUngrab)
        return;
      x_focus_changed (event->type,
		       (event->xfocus.detail == NotifyPointer ?
			FOCUS_IMPLICIT : FOCUS_EXPLICIT),
		       dpyinfo, frame, bufp);
      break;

    case ClientMessage:
      if (event->xclient.message_type == dpyinfo->Xatom_XEMBED)
	{
	  enum xembed_message msg = event->xclient.data.l[1];
	  x_focus_changed ((msg == XEMBED_FOCUS_IN ? FocusIn : FocusOut),
			   FOCUS_EXPLICIT, dpyinfo, frame, bufp);
	}
      break;
    }
}


#if (defined USE_LUCID && defined HAVE_XINPUT2) \
  || (!defined USE_X_TOOLKIT && !defined USE_GTK)
/* Handle an event saying the mouse has moved out of an Emacs frame.  */

void
x_mouse_leave (struct x_display_info *dpyinfo)
{
#if defined HAVE_XINPUT2 && !defined USE_X_TOOLKIT
  struct xi_device_t *device;
#endif
  Mouse_HLInfo *hlinfo;

  hlinfo = &dpyinfo->mouse_highlight;

  if (hlinfo->mouse_face_mouse_frame)
    {
      clear_mouse_face (hlinfo);
      hlinfo->mouse_face_mouse_frame = NULL;
    }

#if defined HAVE_XINPUT2 && !defined USE_X_TOOLKIT
  if (!dpyinfo->supports_xi2)
    /* The call below is supposed to reset the implicit focus and
       revert the focus back to the last explicitly focused frame.  It
       doesn't work on input extension builds because focus tracking
       does not set x_focus_event_frame, and proceeds on a per-device
       basis.  On such builds, clear the implicit focus of the client
       pointer instead.  */
#endif
    x_new_focus_frame (dpyinfo, dpyinfo->x_focus_event_frame);
#if defined HAVE_XINPUT2 && !defined USE_X_TOOLKIT
  else
    {
      if (dpyinfo->client_pointer_device == -1)
	/* If there's no client pointer device, then no implicit focus
	   is currently set.  */
	return;

      device = xi_device_from_id (dpyinfo, dpyinfo->client_pointer_device);

      if (device && device->focus_implicit_frame)
	{
	  device->focus_implicit_frame = NULL;

	  /* The focus might have changed; compute the new focus.  */
	  xi_handle_focus_change (dpyinfo);
	}
    }
#endif
}
#endif

/* The focus has changed, or we have redirected a frame's focus to
   another frame (this happens when a frame uses a surrogate
   mini-buffer frame).  Shift the highlight as appropriate.

   The FRAME argument doesn't necessarily have anything to do with which
   frame is being highlighted or un-highlighted; we only use it to find
   the appropriate X display info.  */

static void
XTframe_rehighlight (struct frame *frame)
{
  x_frame_rehighlight (FRAME_DISPLAY_INFO (frame));
}

static void
x_frame_rehighlight (struct x_display_info *dpyinfo)
{
  struct frame *old_highlight = dpyinfo->highlight_frame;

  if (dpyinfo->x_focus_frame)
    {
      dpyinfo->highlight_frame
	= ((FRAMEP (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame)))
	   ? XFRAME (FRAME_FOCUS_FRAME (dpyinfo->x_focus_frame))
	   : dpyinfo->x_focus_frame);
      if (! FRAME_LIVE_P (dpyinfo->highlight_frame))
	{
	  fset_focus_frame (dpyinfo->x_focus_frame, Qnil);
	  dpyinfo->highlight_frame = dpyinfo->x_focus_frame;
	}
    }
  else
    dpyinfo->highlight_frame = 0;

  if (dpyinfo->highlight_frame != old_highlight)
    {
      if (old_highlight)
	x_frame_unhighlight (old_highlight);
      if (dpyinfo->highlight_frame)
	x_frame_highlight (dpyinfo->highlight_frame);
    }
}



/* Keyboard processing - modifier keys, vendor-specific keysyms, etc.  */

/* Initialize mode_switch_bit and modifier_meaning.  */
static void
x_find_modifier_meanings (struct x_display_info *dpyinfo)
{
  int min_code, max_code;
  KeySym *syms;
  int syms_per_code;
  XModifierKeymap *mods;
#ifdef HAVE_XKB
  int i;
  int found_meta_p = false;
  unsigned int vmodmask;
#endif

  dpyinfo->meta_mod_mask = 0;
  dpyinfo->shift_lock_mask = 0;
  dpyinfo->alt_mod_mask = 0;
  dpyinfo->super_mod_mask = 0;
  dpyinfo->hyper_mod_mask = 0;

#ifdef HAVE_XKB
  if (dpyinfo->xkb_desc
      && dpyinfo->xkb_desc->server)
    {
      for (i = 0; i < XkbNumVirtualMods; i++)
	{
	  vmodmask = dpyinfo->xkb_desc->server->vmods[i];

	  if (dpyinfo->xkb_desc->names->vmods[i] == dpyinfo->Xatom_Meta)
	    {
	      dpyinfo->meta_mod_mask |= vmodmask;

	      if (vmodmask)
		found_meta_p = true;
	    }
	  else if (dpyinfo->xkb_desc->names->vmods[i] == dpyinfo->Xatom_Alt)
	    dpyinfo->alt_mod_mask |= vmodmask;
	  else if (dpyinfo->xkb_desc->names->vmods[i] == dpyinfo->Xatom_Super)
	    dpyinfo->super_mod_mask |= vmodmask;
	  else if (dpyinfo->xkb_desc->names->vmods[i] == dpyinfo->Xatom_Hyper)
	    dpyinfo->hyper_mod_mask |= vmodmask;
	  else if (dpyinfo->xkb_desc->names->vmods[i] == dpyinfo->Xatom_ShiftLock)
	    dpyinfo->shift_lock_mask |= vmodmask;
	}

      if (!found_meta_p)
	{
	  dpyinfo->meta_mod_mask = dpyinfo->alt_mod_mask;
	  dpyinfo->alt_mod_mask = 0;
	}

      if (dpyinfo->alt_mod_mask & dpyinfo->meta_mod_mask)
	dpyinfo->alt_mod_mask &= ~dpyinfo->meta_mod_mask;

      if (dpyinfo->hyper_mod_mask & dpyinfo->super_mod_mask)
	dpyinfo->hyper_mod_mask &= ~dpyinfo->super_mod_mask;

      return;
    }
#endif

  XDisplayKeycodes (dpyinfo->display, &min_code, &max_code);

  syms = XGetKeyboardMapping (dpyinfo->display,
			      min_code, max_code - min_code + 1,
			      &syms_per_code);

  if (!syms)
    {
      dpyinfo->meta_mod_mask = Mod1Mask;
      dpyinfo->super_mod_mask = Mod2Mask;
      return;
    }

  mods = XGetModifierMapping (dpyinfo->display);

  /* Scan the modifier table to see which modifier bits the Meta and
     Alt keysyms are on.  */
  {
    int row, col;	/* The row and column in the modifier table.  */
    bool found_alt_or_meta;

    for (row = 3; row < 8; row++)
      {
	found_alt_or_meta = false;
	for (col = 0; col < mods->max_keypermod; col++)
	  {
	    KeyCode code = mods->modifiermap[(row * mods->max_keypermod) + col];

	    /* Zeroes are used for filler.  Skip them.  */
	    if (code == 0)
	      continue;

	    /* Are any of this keycode's keysyms a meta key?  */
	    {
	      int code_col;

	      for (code_col = 0; code_col < syms_per_code; code_col++)
		{
		  int sym = syms[((code - min_code) * syms_per_code) + code_col];

		  switch (sym)
		    {
		    case XK_Meta_L:
		    case XK_Meta_R:
		      found_alt_or_meta = true;
		      dpyinfo->meta_mod_mask |= (1 << row);
		      break;

		    case XK_Alt_L:
		    case XK_Alt_R:
		      found_alt_or_meta = true;
		      dpyinfo->alt_mod_mask |= (1 << row);
		      break;

		    case XK_Hyper_L:
		    case XK_Hyper_R:
		      if (!found_alt_or_meta)
			dpyinfo->hyper_mod_mask |= (1 << row);
		      code_col = syms_per_code;
		      col = mods->max_keypermod;
		      break;

		    case XK_Super_L:
		    case XK_Super_R:
		      if (!found_alt_or_meta)
			dpyinfo->super_mod_mask |= (1 << row);
		      code_col = syms_per_code;
		      col = mods->max_keypermod;
		      break;

		    case XK_Shift_Lock:
		      /* Ignore this if it's not on the lock modifier.  */
		      if (!found_alt_or_meta && ((1 << row) == LockMask))
			dpyinfo->shift_lock_mask = LockMask;
		      code_col = syms_per_code;
		      col = mods->max_keypermod;
		      break;
		    }
		}
	    }
	  }
      }
  }

  /* If we couldn't find any meta keys, accept any alt keys as meta keys.  */
  if (! dpyinfo->meta_mod_mask)
    {
      dpyinfo->meta_mod_mask = dpyinfo->alt_mod_mask;
      dpyinfo->alt_mod_mask = 0;
    }

  /* If some keys are both alt and meta,
     make them just meta, not alt.  */
  if (dpyinfo->alt_mod_mask & dpyinfo->meta_mod_mask)
    {
      dpyinfo->alt_mod_mask &= ~dpyinfo->meta_mod_mask;
    }

  /* If some keys are both super and hyper, make them just super.
     Many X servers are misconfigured so that super and hyper are both
     Mod4, but most users have no hyper key.  */
  if (dpyinfo->hyper_mod_mask & dpyinfo->super_mod_mask)
    dpyinfo->hyper_mod_mask &= ~dpyinfo->super_mod_mask;

  XFree (syms);

  if (dpyinfo->modmap)
    XFreeModifiermap (dpyinfo->modmap);
  dpyinfo->modmap = mods;
}

/* Convert between the modifier bits X uses and the modifier bits
   Emacs uses.  */

int
x_x_to_emacs_modifiers (struct x_display_info *dpyinfo, int state)
{
  int mod_ctrl = ctrl_modifier;
  int mod_meta = meta_modifier;
  int mod_alt  = alt_modifier;
  int mod_hyper = hyper_modifier;
  int mod_super = super_modifier;
  Lisp_Object tem;

  tem = Fget (Vx_ctrl_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_ctrl = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_alt_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_alt = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_meta_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_meta = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_hyper_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_hyper = XFIXNUM (tem) & INT_MAX;
  tem = Fget (Vx_super_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_super = XFIXNUM (tem) & INT_MAX;

  return (  ((state & (ShiftMask | dpyinfo->shift_lock_mask)) ? shift_modifier : 0)
            | ((state & ControlMask)			? mod_ctrl	: 0)
            | ((state & dpyinfo->meta_mod_mask)		? mod_meta	: 0)
            | ((state & dpyinfo->alt_mod_mask)		? mod_alt	: 0)
            | ((state & dpyinfo->super_mod_mask)	? mod_super	: 0)
            | ((state & dpyinfo->hyper_mod_mask)	? mod_hyper	: 0));
}

int
x_emacs_to_x_modifiers (struct x_display_info *dpyinfo, intmax_t state)
{
  EMACS_INT mod_ctrl = ctrl_modifier;
  EMACS_INT mod_meta = meta_modifier;
  EMACS_INT mod_alt  = alt_modifier;
  EMACS_INT mod_hyper = hyper_modifier;
  EMACS_INT mod_super = super_modifier;

  Lisp_Object tem;

  tem = Fget (Vx_ctrl_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_ctrl = XFIXNUM (tem);
  tem = Fget (Vx_alt_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_alt = XFIXNUM (tem);
  tem = Fget (Vx_meta_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_meta = XFIXNUM (tem);
  tem = Fget (Vx_hyper_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_hyper = XFIXNUM (tem);
  tem = Fget (Vx_super_keysym, Qmodifier_value);
  if (FIXNUMP (tem)) mod_super = XFIXNUM (tem);


  return (  ((state & mod_alt)		? dpyinfo->alt_mod_mask   : 0)
            | ((state & mod_super)	? dpyinfo->super_mod_mask : 0)
            | ((state & mod_hyper)	? dpyinfo->hyper_mod_mask : 0)
            | ((state & shift_modifier)	? ShiftMask        : 0)
            | ((state & mod_ctrl)	? ControlMask      : 0)
            | ((state & mod_meta)	? dpyinfo->meta_mod_mask  : 0));
}

/* Convert a keysym to its name.  */

char *
get_keysym_name (int keysym)
{
  char *value;

  block_input ();
  value = XKeysymToString (keysym);
  unblock_input ();

  return value;
}

/* Given the root and event coordinates of an X event destined for F's
   edit window, compute the offset between that window and F's root
   window.  This information is then used as an optimization to avoid
   synchronizing when converting coordinates from some other event to
   F's edit window.  */

static void
x_compute_root_window_offset (struct frame *f, int root_x, int root_y,
			      int event_x, int event_y)
{
  FRAME_X_OUTPUT (f)->window_offset_certain_p = true;
  FRAME_X_OUTPUT (f)->root_x = root_x - event_x;
  FRAME_X_OUTPUT (f)->root_y = root_y - event_y;
}

/* Translate the given coordinates from the root window to the edit
   window of FRAME, taking into account any cached root window
   offsets.  This allows Emacs to avoid excessive calls to _XReply in
   many cases while handling events, which would otherwise result in
   slowdowns over slow network connections.  */

void
x_translate_coordinates (struct frame *f, int root_x, int root_y,
			 int *x_out, int *y_out)
{
  struct x_output *output;
  Window dummy;

  output = FRAME_X_OUTPUT (f);

  if (output->window_offset_certain_p)
    {
      /* Use the cached root window offset.  */
      *x_out = root_x - output->root_x;
      *y_out = root_y - output->root_y;

      return;
    }

  /* Otherwise, do the transformation manually.  Then, cache the root
     window position.  */
  if (!XTranslateCoordinates (FRAME_X_DISPLAY (f),
			      FRAME_DISPLAY_INFO (f)->root_window,
			      FRAME_X_WINDOW (f), root_x, root_y,
			      x_out, y_out, &dummy))
    /* Use some dummy values.  This is not supposed to be called with
       coordinates out of the screen.  */
    *x_out = 0, *y_out = 0;
  else
    {
      /* Cache the root window offset of the edit window.  */
      output->window_offset_certain_p = true;
      output->root_x = root_x - *x_out;
      output->root_y = root_y - *y_out;
    }
}

/* Translate the given coordinates from the edit window of FRAME,
   taking into account any cached root window offsets.  This is mainly
   used from the popup menu code.  */

void
x_translate_coordinates_to_root (struct frame *f, int x, int y,
				 int *x_out, int *y_out)
{
  struct x_output *output;
  Window dummy;

  output = FRAME_X_OUTPUT (f);

  if (output->window_offset_certain_p)
    {
      /* Use the cached root window offset.  */
      *x_out = x + output->root_x;
      *y_out = y + output->root_y;

      return;
    }

  /* Otherwise, do the transform manually and compute and cache the
     root window position.  */
  if (!XTranslateCoordinates (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			      FRAME_DISPLAY_INFO (f)->root_window,
			      x, y, x_out, y_out, &dummy))
    *x_out = 0, *y_out = 0;
  else
    {
      /* Cache the root window offset of the edit window.  */
      output->window_offset_certain_p = true;
      output->root_x = *x_out - x;
      output->root_y = *y_out - y;
    }
}

/* Do x-translate-coordinates, but try to avoid a roundtrip to the X
   server at the cost of not returning `child', which most callers
   have no reason to use.  */

Lisp_Object
x_handle_translate_coordinates (struct frame *f, Lisp_Object dest_window,
				int source_x, int source_y)
{
  if (NILP (dest_window))
    {
      /* We are translating coordinates from a frame to the root
	 window.  Avoid a roundtrip if possible by using cached
	 coordinates.  */

      if (!FRAME_X_OUTPUT (f)->window_offset_certain_p)
	return Qnil;

      return list3 (make_fixnum (source_x + FRAME_X_OUTPUT (f)->root_x),
		    make_fixnum (source_y + FRAME_X_OUTPUT (f)->root_y),
		    Qnil);
    }

  return Qnil;
}

/* The same, but for an XIDeviceEvent.  */

#ifdef HAVE_XINPUT2

static void
xi_compute_root_window_offset (struct frame *f, XIDeviceEvent *xev)
{
  /* Truncate coordinates instead of rounding them, because that is
     how the X server handles window hierarchy.  */
  x_compute_root_window_offset (f, xev->root_x, xev->root_y,
				xev->event_x, xev->event_y);
}

static void
xi_compute_root_window_offset_enter (struct frame *f, XIEnterEvent *enter)
{
  x_compute_root_window_offset (f, enter->root_x, enter->root_y,
				enter->event_x, enter->event_y);
}

#ifdef HAVE_XINPUT2_4

static void
xi_compute_root_window_offset_pinch (struct frame *f, XIGesturePinchEvent *pev)
{
  /* Truncate coordinates instead of rounding them, because that is
     how the X server handles window hierarchy.  */
  x_compute_root_window_offset (f, pev->root_x, pev->root_y,
				pev->event_x, pev->event_y);
}

#endif

#endif

static Bool
x_query_pointer_1 (struct x_display_info *dpyinfo,
		   int client_pointer_device, Window w,
		   Window *root_return, Window *child_return,
		   int *root_x_return, int *root_y_return,
		   int *win_x_return, int *win_y_return,
		   unsigned int *mask_return)
{
  Bool rc;
  Display *dpy;
#ifdef HAVE_XINPUT2
  bool had_errors;
  XIModifierState modifiers;
  XIButtonState buttons;
  XIGroupState group; /* Unused.  */
  double root_x, root_y, win_x, win_y;
  unsigned int state;
#endif

  dpy = dpyinfo->display;

#ifdef HAVE_XINPUT2
  if (client_pointer_device != -1)
    {
      /* Catch errors caused by the device going away.  This is not
	 very expensive, since XIQueryPointer will sync anyway.  */
      x_catch_errors (dpy);
      rc = XIQueryPointer (dpyinfo->display,
			   dpyinfo->client_pointer_device,
			   w, root_return, child_return,
			   &root_x, &root_y, &win_x, &win_y,
			   &buttons, &modifiers, &group);
      had_errors = x_had_errors_p (dpy);
      x_uncatch_errors_after_check ();

      if (had_errors)
	{
	  /* If the specified client pointer is the display's client
	     pointer, clear it now.  A new client pointer might not be
	     found before the next call to x_query_pointer_1 and
	     waiting for the error leads to excessive syncing.  */

	  if (client_pointer_device == dpyinfo->client_pointer_device)
	    dpyinfo->client_pointer_device = -1;

	  rc = XQueryPointer (dpyinfo->display, w, root_return,
			      child_return, root_x_return,
			      root_y_return, win_x_return,
			      win_y_return, mask_return);
	}
      else
	{
	  state = 0;

	  xi_convert_button_state (&buttons, &state);
	  *mask_return = state | modifiers.effective;

	  XFree (buttons.mask);

	  *root_x_return = lrint (root_x);
	  *root_y_return = lrint (root_y);
	  *win_x_return = lrint (win_x);
	  *win_y_return = lrint (win_y);
	}
    }
  else
#endif
    rc = XQueryPointer (dpy, w, root_return, child_return,
			root_x_return, root_y_return, win_x_return,
			win_y_return, mask_return);

  return rc;
}

Bool
x_query_pointer (Display *dpy, Window w, Window *root_return,
		 Window *child_return, int *root_x_return,
		 int *root_y_return, int *win_x_return,
		 int *win_y_return, unsigned int *mask_return)
{
  struct x_display_info *dpyinfo = x_dpyinfo (dpy);

#ifdef HAVE_XINPUT2
  return x_query_pointer_1 (dpyinfo, dpyinfo->client_pointer_device,
			    w, root_return, child_return, root_x_return,
			    root_y_return, win_x_return, win_y_return,
			    mask_return);
#else
  return x_query_pointer_1 (dpyinfo, -1, w, root_return, child_return,
			    root_x_return, root_y_return, win_x_return,
			    win_y_return, mask_return);
#endif
}

/* Mouse clicks and mouse movement.  Rah.

   Formerly, we used PointerMotionHintMask (in standard_event_mask)
   so that we would have to call XQueryPointer after each MotionNotify
   event to ask for another such event.  However, this made mouse tracking
   slow, and there was a bug that made it eventually stop.

   Simply asking for MotionNotify all the time seems to work better.

   In order to avoid asking for motion events and then throwing most
   of them away or busy-polling the server for mouse positions, we ask
   the server for pointer motion hints.  This means that we get only
   one event per group of mouse movements.  "Groups" are delimited by
   other kinds of events (focus changes and button clicks, for
   example), or by XQueryPointer calls; when one of these happens, we
   get another MotionNotify event the next time the mouse moves.  This
   is at least as efficient as getting motion events when mouse
   tracking is on, and I suspect only negligibly worse when tracking
   is off.  */

/* Prepare a mouse-event in *RESULT for placement in the input queue.

   If the event is a button press, then note that we have grabbed
   the mouse.

   The XButtonEvent structure passed as EVENT might not come from the
   X server, and instead be artificially constructed from input
   extension events.  In these special events, the only fields that
   are initialized are `time', `button', `state', `type', `window' and
   `x', `y', `x_root' and `y_root'.  This function should not access
   any other fields in EVENT without also initializing the
   corresponding fields in `bv' under the XI_ButtonPress and
   XI_ButtonRelease labels inside `handle_one_xevent'.

   XI2 indicates that this click comes from XInput2 rather than core
   event.   */

static Lisp_Object
x_construct_mouse_click (struct input_event *result,
                         const XButtonEvent *event,
                         struct frame *f, bool xi2)
{
  int x = event->x;
  int y = event->y;

  result->kind = MOUSE_CLICK_EVENT;
  result->code = event->button - Button1;
  result->timestamp = event->time;
  result->modifiers = (x_x_to_emacs_modifiers (FRAME_DISPLAY_INFO (f),
					       event->state)
		       | (event->type == ButtonRelease
			  ? up_modifier
			  : down_modifier));

  /* Convert pre-XInput2 wheel events represented as mouse-clicks.  */
  if (!xi2)
    {
      Lisp_Object base
        = Fcdr_safe (Fassq (make_fixnum (result->code + 1),
                            Fsymbol_value (Qmouse_wheel_buttons)));
      int wheel
        = (NILP (base) ? -1
           : BASE_EQ (base, Qwheel_down)  ? 0
           : BASE_EQ (base, Qwheel_up)    ? 1
           : BASE_EQ (base, Qwheel_left)  ? 2
           : BASE_EQ (base, Qwheel_right) ? 3
           : -1);
      if (wheel >= 0)
        {
          result->kind = (event->type != ButtonRelease ? NO_EVENT
		          : wheel & 2 ? HORIZ_WHEEL_EVENT : WHEEL_EVENT);
          result->code = 0;         /* Not used.  */
          result->modifiers &= ~(up_modifier | down_modifier);
          result->modifiers |= wheel & 1 ? up_modifier : down_modifier;
        }
    }

  /* If result->window is not the frame's edit widget (which can
     happen with GTK+ scroll bars, for example), translate the
     coordinates so they appear at the correct position.  */
  if (event->window != FRAME_X_WINDOW (f))
    x_translate_coordinates (f, event->x_root, event->y_root,
			     &x, &y);

  XSETINT (result->x, x);
  XSETINT (result->y, y);
  XSETFRAME (result->frame_or_window, f);
  result->arg = Qnil;
  return Qnil;
}

/* Function to report a mouse movement to the mainstream Emacs code.
   The input handler calls this.

   We have received a mouse movement event, which is given in *event.
   If the mouse is over a different glyph than it was last time, tell
   the mainstream emacs code by setting mouse_moved.  If not, ask for
   another motion event, so we can check again the next time it moves.

   The XMotionEvent structure passed as EVENT might not come from the
   X server, and instead be artificially constructed from input
   extension events.  In these special events, the only fields that
   are initialized are `time', `window', `send_event', `x' and `y'.
   This function should not access any other fields in EVENT without
   also initializing the corresponding fields in `ev' under the
   XI_Motion, XI_Enter and XI_Leave labels inside
   `handle_one_xevent'.  */

static bool
x_note_mouse_movement (struct frame *frame, const XMotionEvent *event,
		       Lisp_Object device)
{
  XRectangle *r;
  struct x_display_info *dpyinfo;

  if (!FRAME_X_OUTPUT (frame))
    return false;

  dpyinfo = FRAME_DISPLAY_INFO (frame);
  dpyinfo->last_mouse_movement_time = event->time;
  dpyinfo->last_mouse_movement_time_send_event = event->send_event;
  dpyinfo->last_mouse_motion_frame = frame;
  dpyinfo->last_mouse_motion_x = event->x;
  dpyinfo->last_mouse_motion_y = event->y;

  if (event->window != FRAME_X_WINDOW (frame))
    {
      frame->mouse_moved = true;
      frame->last_mouse_device = device;
      dpyinfo->last_mouse_scroll_bar = NULL;
      note_mouse_highlight (frame, -1, -1);
      dpyinfo->last_mouse_glyph_frame = NULL;
      return true;
    }


  /* Has the mouse moved off the glyph it was on at the last sighting?  */
  r = &dpyinfo->last_mouse_glyph;
  if (frame != dpyinfo->last_mouse_glyph_frame
      || event->x < r->x || event->x >= r->x + r->width
      || event->y < r->y || event->y >= r->y + r->height)
    {
      frame->mouse_moved = true;
      frame->last_mouse_device = device;
      dpyinfo->last_mouse_scroll_bar = NULL;
      note_mouse_highlight (frame, event->x, event->y);
      /* Remember which glyph we're now on.  */
      remember_mouse_glyph (frame, event->x, event->y, r);
      dpyinfo->last_mouse_glyph_frame = frame;
      return true;
    }

  return false;
}

/* Get a sibling below WINDOW on DPY at PARENT_X and PARENT_Y.  */
static Window
x_get_window_below (Display *dpy, Window window,
		    int parent_x, int parent_y,
		    int *inner_x, int *inner_y)
{
  int rc, i, cx, cy;
  XWindowAttributes attrs;
  unsigned int nchildren;
  Window root, parent, *children, value;
  bool window_seen;

  /* TODO: rewrite to have less dependencies.  */

  children = NULL;
  window_seen = false;
  value = None;

  rc = XQueryTree (dpy, window, &root, &parent,
		   &children, &nchildren);

  if (rc)
    {
      if (children)
	XFree (children);

      rc = XQueryTree (dpy, parent, &root,
		       &parent, &children, &nchildren);
    }

  if (rc)
    {
      for (i = nchildren - 1; i >= 0; --i)
	{
	  if (children[i] == window)
	    {
	      window_seen = true;
	      continue;
	    }

	  if (!window_seen)
	    continue;

	  rc = XGetWindowAttributes (dpy, children[i], &attrs);

	  if (rc && attrs.map_state != IsViewable)
	    continue;

	  if (rc && parent_x >= attrs.x
	      && parent_y >= attrs.y
	      && parent_x < attrs.x + attrs.width
	      && parent_y < attrs.y + attrs.height)
	    {
	      value = children[i];
	      cx = parent_x - attrs.x;
	      cy = parent_y - attrs.y;

	      break;
	    }
	}
    }

  if (children)
    XFree (children);

  if (value)
    {
      *inner_x = cx;
      *inner_y = cy;
    }

  return value;
}

/* Like XTmouse_position, but much faster.  */

static void
x_fast_mouse_position (struct frame **fp, int insist, Lisp_Object *bar_window,
		       enum scroll_bar_part *part, Lisp_Object *x, Lisp_Object *y,
		       Time *timestamp)
{
  int root_x, root_y, win_x, win_y;
  unsigned int mask;
  Window dummy;
  struct scroll_bar *bar;
  struct x_display_info *dpyinfo;
  Lisp_Object tail, frame;
  struct frame *f1;

  dpyinfo = FRAME_DISPLAY_INFO (*fp);

  if (dpyinfo->last_mouse_scroll_bar && !insist)
    {
      bar = dpyinfo->last_mouse_scroll_bar;

      if (bar->horizontal)
	x_horizontal_scroll_bar_report_motion (fp, bar_window, part,
					       x, y, timestamp);
      else
	x_scroll_bar_report_motion (fp, bar_window, part,
				    x, y, timestamp);

      return;
    }

  FOR_EACH_FRAME (tail, frame)
    {
      if (FRAME_X_P (XFRAME (frame))
	  && (FRAME_DISPLAY_INFO (XFRAME (frame))
	      == dpyinfo))
	XFRAME (frame)->mouse_moved = false;
    }

  if (!EQ (Vx_use_fast_mouse_position, Qreally_fast))
    {
      /* This means that Emacs should select a frame and report the
	 mouse position relative to it.  The approach used here avoids
	 making multiple roundtrips to the X server querying for the
	 window beneath the pointer, and was borrowed from
	 haiku_mouse_position in haikuterm.c.  */

      if (gui_mouse_grabbed (dpyinfo)
	  && !EQ (track_mouse, Qdropping)
	  && !EQ (track_mouse, Qdrag_source))
	/* Pick the last mouse frame if dropping.  */
	f1 = dpyinfo->last_mouse_frame;
      else
	/* Otherwise, pick the last mouse motion frame.  */
	f1 = dpyinfo->last_mouse_motion_frame;

      if (!f1 && (FRAME_X_P (SELECTED_FRAME ())
		  && (FRAME_DISPLAY_INFO (SELECTED_FRAME ())
		      == dpyinfo)))
	f1 = SELECTED_FRAME ();

      if (!f1 || (!FRAME_X_P (f1) && (insist > 0)))
	FOR_EACH_FRAME (tail, frame)
	  if (FRAME_X_P (XFRAME (frame))
	      && (FRAME_DISPLAY_INFO (XFRAME (frame))
		  == dpyinfo)
	      && !FRAME_TOOLTIP_P (XFRAME (frame)))
	    f1 = XFRAME (frame);

      if (f1 && FRAME_TOOLTIP_P (f1))
	f1 = NULL;

      if (f1 && FRAME_X_P (f1) && FRAME_X_WINDOW (f1))
	{
	  if (!x_query_pointer (dpyinfo->display, FRAME_X_WINDOW (f1),
				&dummy, &dummy, &root_x, &root_y,
				&win_x, &win_y, &mask))
	    /* The pointer is out of the screen.  */
	    return;

	  remember_mouse_glyph (f1, win_x, win_y,
				&dpyinfo->last_mouse_glyph);
	  dpyinfo->last_mouse_glyph_frame = f1;

	  *bar_window = Qnil;
	  *part = scroll_bar_nowhere;

	  /* If track-mouse is `drag-source' and the mouse pointer is
	     certain to not be actually under the chosen frame, return
	     NULL in FP.  */
	  if (EQ (track_mouse, Qdrag_source)
	      && (win_x < 0 || win_y < 0
		  || win_x >= FRAME_PIXEL_WIDTH (f1)
		  || win_y >= FRAME_PIXEL_HEIGHT (f1)))
	    *fp = NULL;
	  else
	    *fp = f1;

	  *timestamp = dpyinfo->last_mouse_movement_time;
	  XSETINT (*x, win_x);
	  XSETINT (*y, win_y);
	}
    }
  else
    {
      /* This means Emacs should only report the coordinates of the last
	 mouse motion.  */

      if (dpyinfo->last_mouse_motion_frame)
	{
	  *fp = dpyinfo->last_mouse_motion_frame;
	  *timestamp = dpyinfo->last_mouse_movement_time;
	  *x = make_fixnum (dpyinfo->last_mouse_motion_x);
	  *y = make_fixnum (dpyinfo->last_mouse_motion_y);
	  *bar_window = Qnil;
	  *part = scroll_bar_nowhere;
	  dpyinfo->last_mouse_motion_frame->mouse_moved = false;
	}
    }
}

/* Return the current position of the mouse.
   *FP should be a frame which indicates which display to ask about.

   If the mouse movement started in a scroll bar, set *FP, *BAR_WINDOW,
   and *PART to the frame, window, and scroll bar part that the mouse
   is over.  Set *X and *Y to the portion and whole of the mouse's
   position on the scroll bar.

   If the mouse movement started elsewhere, set *FP to the frame the
   mouse is on, *BAR_WINDOW to nil, and *X and *Y to the character cell
   the mouse is over.

   Set *TIMESTAMP to the server time-stamp for the time at which the mouse
   was at this position.

   Don't store anything if we don't have a valid set of values to report.

   This clears the mouse_moved flag, so we can wait for the next mouse
   movement.  */

static void
XTmouse_position (struct frame **fp, int insist, Lisp_Object *bar_window,
		  enum scroll_bar_part *part, Lisp_Object *x, Lisp_Object *y,
		  Time *timestamp)
{
  struct frame *f1, *maybe_tooltip;
  struct x_display_info *dpyinfo;
  bool unrelated_tooltip;

  dpyinfo = FRAME_DISPLAY_INFO (*fp);

  if (!NILP (Vx_use_fast_mouse_position))
    {
      /* The user says that Emacs is running over the network, and a
	 fast approximation of `mouse-position' should be used.

	 Depending on what the value of `x-use-fast-mouse-position'
	 is, do one of two things: only perform the XQueryPointer to
	 obtain the coordinates from the last mouse frame, or only
	 return the last mouse motion frame and the
	 last_mouse_motion_x and Y.  */

      x_fast_mouse_position (fp, insist, bar_window, part, x,
			     y, timestamp);
      return;
    }

  block_input ();

  if (dpyinfo->last_mouse_scroll_bar && insist == 0)
    {
      struct scroll_bar *bar = dpyinfo->last_mouse_scroll_bar;

      if (bar->horizontal)
	x_horizontal_scroll_bar_report_motion (fp, bar_window, part, x, y, timestamp);
      else
	x_scroll_bar_report_motion (fp, bar_window, part, x, y, timestamp);
    }
  else
    {
      Window root;
      int root_x, root_y;

      Window dummy_window;
      int dummy;

      Lisp_Object frame, tail;

      /* Clear the mouse-moved flag for every frame on this display.  */
      FOR_EACH_FRAME (tail, frame)
	if (FRAME_X_P (XFRAME (frame))
            && FRAME_X_DISPLAY (XFRAME (frame)) == FRAME_X_DISPLAY (*fp))
	  XFRAME (frame)->mouse_moved = false;

      dpyinfo->last_mouse_scroll_bar = NULL;

      /* Figure out which root window we're on.  */
      x_query_pointer (FRAME_X_DISPLAY (*fp),
		       DefaultRootWindow (FRAME_X_DISPLAY (*fp)),
		       /* The root window which contains the pointer.  */
		       &root,
		       /* Trash which we can't trust if the pointer is on
			  a different screen.  */
		       &dummy_window,
		       /* The position on that root window.  */
		       &root_x, &root_y,
		       /* More trash we can't trust.  */
		       &dummy, &dummy,
		       /* Modifier keys and pointer buttons, about which
			  we don't care.  */
		       (unsigned int *) &dummy);

      /* Now we have a position on the root; find the innermost window
	 containing the pointer.  */
      {
	Window win, child;
#ifdef USE_GTK
	Window first_win = 0;
#endif
	int win_x, win_y;
	int parent_x, parent_y;

	win = root;
	parent_x = root_x;
	parent_y = root_y;

	/* XTranslateCoordinates can get errors if the window
	   structure is changing at the same time this function
	   is running.  So at least we must not crash from them.  */

	x_catch_errors (FRAME_X_DISPLAY (*fp));

	if (gui_mouse_grabbed (dpyinfo) && !EQ (track_mouse, Qdropping)
	    && !EQ (track_mouse, Qdrag_source))
	  {
	    /* If mouse was grabbed on a frame, give coords for that frame
	       even if the mouse is now outside it.  */
	    XTranslateCoordinates (FRAME_X_DISPLAY (*fp),
				   /* From-window.  */
				   root,
				   /* To-window.  */
				   FRAME_X_WINDOW (dpyinfo->last_mouse_frame),
				   /* From-position, to-position.  */
				   root_x, root_y, &win_x, &win_y,
				   /* Child of win.  */
				   &child);
	    f1 = dpyinfo->last_mouse_frame;
	  }
	else
	  {
	    while (true)
	      {
		XTranslateCoordinates (FRAME_X_DISPLAY (*fp),
				       /* From-window, to-window.  */
				       root, win,
				       /* From-position, to-position.  */
				       root_x, root_y, &win_x, &win_y,
				       /* Child of win.  */
				       &child);

		/* If CHILD is a tooltip frame, look below it if
		   track-mouse is drag-source.  */
		if (child != None
		    && (EQ (track_mouse, Qdrag_source)
			|| EQ (track_mouse, Qdropping)))
		  {
		    maybe_tooltip = x_tooltip_window_to_frame (dpyinfo, child,
							       &unrelated_tooltip);

		    if (maybe_tooltip || unrelated_tooltip)
		      child = x_get_window_below (dpyinfo->display, child,
						  parent_x, parent_y, &win_x,
						  &win_y);
		  }

		if (child == None || child == win)
		  {
#ifdef USE_GTK
		    /* On GTK we have not inspected WIN yet.  If it has
		       a frame and that frame has a parent, use it.  */
		    struct frame *f = x_window_to_frame (dpyinfo, win);

		    if (f && FRAME_PARENT_FRAME (f))
		      first_win = win;
#endif
		    break;
		  }
#ifdef USE_GTK
		/* We don't want to know the innermost window.  We
		   want the edit window.  For non-Gtk+ the innermost
		   window is the edit window.  For Gtk+ it might not
		   be.  It might be the tool bar for example.  */
		if (x_window_to_frame (dpyinfo, win))
		  /* But don't hurry.  We might find a child frame
		     beneath.  */
		  first_win = win;
#endif
		win = child;
		parent_x = win_x;
		parent_y = win_y;
	      }

#ifdef USE_GTK
	    if (first_win)
	      win = first_win;
#endif

	    /* Now we know that:
	       win is the innermost window containing the pointer
	       (XTC says it has no child containing the pointer),
	       win_x and win_y are the pointer's position in it
	       (XTC did this the last time through), and
	       parent_x and parent_y are the pointer's position in win's parent.
	       (They are what win_x and win_y were when win was child.
	       If win is the root window, it has no parent, and
	       parent_{x,y} are invalid, but that's okay, because we'll
	       never use them in that case.)  */

#ifdef USE_GTK
	    /* We don't want to know the innermost window.  We
	       want the edit window.  */
	    f1 = x_window_to_frame (dpyinfo, win);
#else
	    /* Is win one of our frames?  */
	    f1 = x_any_window_to_frame (dpyinfo, win);
#endif

#ifdef USE_X_TOOLKIT
	    /* If we end up with the menu bar window, say it's not
	       on the frame.  */
	    if (f1 != NULL
		&& f1->output_data.x->menubar_widget
		&& win == XtWindow (f1->output_data.x->menubar_widget))
	      f1 = NULL;
#endif /* USE_X_TOOLKIT */
	  }

	/* Set last user time to avoid confusing some window managers
	   about the tooltip displayed during drag-and-drop.  */

	if ((EQ (track_mouse, Qdrag_source)
	     || EQ (track_mouse, Qdropping))
	    && (dpyinfo->last_user_time
		< dpyinfo->last_mouse_movement_time))
	  x_display_set_last_user_time (dpyinfo,
					dpyinfo->last_mouse_movement_time,
					dpyinfo->last_mouse_movement_time_send_event,
					true);

	if ((!f1 || FRAME_TOOLTIP_P (f1))
	    && (EQ (track_mouse, Qdropping)
		|| EQ (track_mouse, Qdrag_source))
	    && gui_mouse_grabbed (dpyinfo))
	  {
	    /* When dropping then if we didn't get a frame or only a
	       tooltip frame and the mouse was grabbed on a frame,
	       give coords for that frame even if the mouse is now
	       outside it.  */
	    XTranslateCoordinates (FRAME_X_DISPLAY (*fp),
				   /* From-window.  */
				   root,
				   /* To-window.  */
				   FRAME_X_WINDOW (dpyinfo->last_mouse_frame),
				   /* From-position, to-position.  */
				   root_x, root_y, &win_x, &win_y,
				   /* Child of win.  */
				   &child);

	    if (!EQ (track_mouse, Qdrag_source)
		/* Don't let tooltips interfere.  */
		|| (f1 && FRAME_TOOLTIP_P (f1)))
	      f1 = dpyinfo->last_mouse_frame;
	    else
	      {
		/* Don't set FP but do set WIN_X and WIN_Y in this
		   case, so make_lispy_movement knows which
		   coordinates to report.  */
		*bar_window = Qnil;
		*part = 0;
		*fp = NULL;
		XSETINT (*x, win_x);
		XSETINT (*y, win_y);
		*timestamp = dpyinfo->last_mouse_movement_time;
	      }
	  }
	else if (f1 && FRAME_TOOLTIP_P (f1))
	  f1 = NULL;

	if (x_had_errors_p (dpyinfo->display))
	  f1 = NULL;

	x_uncatch_errors_after_check ();

	/* If not, is it one of our scroll bars?  */
	if (!f1)
	  {
	    struct scroll_bar *bar;

            bar = x_window_to_scroll_bar (dpyinfo->display, win, 2);

	    if (bar)
	      {
		f1 = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
		win_x = parent_x;
		win_y = parent_y;
	      }
	  }

	if (!f1 && insist > 0)
	  f1 = SELECTED_FRAME ();

	if (f1 && FRAME_X_P (f1))
	  {
	    /* Ok, we found a frame.  Store all the values.
	       last_mouse_glyph is a rectangle used to reduce the
	       generation of mouse events.  To not miss any motion
	       events, we must divide the frame into rectangles of the
	       size of the smallest character that could be displayed
	       on it, i.e. into the same rectangles that matrices on
	       the frame are divided into.  */

	    dpyinfo = FRAME_DISPLAY_INFO (f1);
	    remember_mouse_glyph (f1, win_x, win_y, &dpyinfo->last_mouse_glyph);
	    dpyinfo->last_mouse_glyph_frame = f1;

	    *bar_window = Qnil;
	    *part = 0;
	    *fp = f1;
	    XSETINT (*x, win_x);
	    XSETINT (*y, win_y);
	    *timestamp = dpyinfo->last_mouse_movement_time;
	  }
      }
    }

  unblock_input ();
}



/***********************************************************************
			       Scroll bars
 ***********************************************************************/

/* Scroll bar support.  */

#if defined HAVE_XINPUT2

/* Select for input extension events used by scroll bars.  This will
   result in the corresponding core events not being generated for
   SCROLL_BAR.  */

MAYBE_UNUSED static void
xi_select_scroll_bar_events (struct x_display_info *dpyinfo,
			     Window scroll_bar)
{
  XIEventMask mask;
  unsigned char *m;
  ptrdiff_t length;

  length = XIMaskLen (XI_LASTEVENT);
  mask.mask = m = alloca (length);
  memset (m, 0, length);
  mask.mask_len = length;

  mask.deviceid = XIAllMasterDevices;
  XISetMask (m, XI_ButtonPress);
  XISetMask (m, XI_ButtonRelease);
  XISetMask (m, XI_Motion);
  XISetMask (m, XI_Enter);
  XISetMask (m, XI_Leave);

  XISelectEvents (dpyinfo->display, scroll_bar, &mask, 1);
}

#endif

/* Given an X window ID and a DISPLAY, find the struct scroll_bar which
   manages it.
   This can be called in GC, so we have to make sure to strip off mark
   bits.  */

static struct scroll_bar *
x_window_to_scroll_bar (Display *display, Window window_id, int type)
{
  Lisp_Object tail, frame;

#if defined (USE_GTK) && !defined (HAVE_GTK3) && defined (USE_TOOLKIT_SCROLL_BARS)
  window_id = (Window) xg_get_scroll_id_for_window (display, window_id);
#endif /* USE_GTK && !HAVE_GTK3  && USE_TOOLKIT_SCROLL_BARS */

  FOR_EACH_FRAME (tail, frame)
    {
      Lisp_Object bar, condemned;

      if (! FRAME_X_P (XFRAME (frame)))
        continue;

      /* Scan this frame's scroll bar list for a scroll bar with the
         right window ID.  */
      condemned = FRAME_CONDEMNED_SCROLL_BARS (XFRAME (frame));
      for (bar = FRAME_SCROLL_BARS (XFRAME (frame));
	   /* This trick allows us to search both the ordinary and
              condemned scroll bar lists with one loop.  */
	   ! NILP (bar) || (bar = condemned,
			       condemned = Qnil,
			       ! NILP (bar));
	   bar = XSCROLL_BAR (bar)->next)
	if (XSCROLL_BAR (bar)->x_window == window_id
            && FRAME_X_DISPLAY (XFRAME (frame)) == display
	    && (type == 2
		|| (type == 1 && XSCROLL_BAR (bar)->horizontal)
		|| (type == 0 && !XSCROLL_BAR (bar)->horizontal)))
	  return XSCROLL_BAR (bar);
    }

  return NULL;
}


#if defined USE_LUCID

/* Return the Lucid menu bar WINDOW is part of.  Return null
   if WINDOW is not part of a menu bar.  */

static Widget
x_window_to_menu_bar (Window window)
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    if (FRAME_X_P (XFRAME (frame)))
      {
	Widget menu_bar = XFRAME (frame)->output_data.x->menubar_widget;

	if (menu_bar && xlwmenu_window_p (menu_bar, window))
	  return menu_bar;
      }
  return NULL;
}

#endif /* USE_LUCID */


/************************************************************************
			 Toolkit scroll bars
 ************************************************************************/

#ifdef USE_TOOLKIT_SCROLL_BARS

static void x_send_scroll_bar_event (Lisp_Object, enum scroll_bar_part,
                                     int, int, bool);

/* Lisp window being scrolled.  Set when starting to interact with
   a toolkit scroll bar, reset to nil when ending the interaction.  */

static Lisp_Object window_being_scrolled;

static Time
x_get_last_toolkit_time (struct x_display_info *dpyinfo)
{
#ifdef USE_X_TOOLKIT
  return XtLastTimestampProcessed (dpyinfo->display);
#else
  return dpyinfo->last_user_time;
#endif
}

#ifndef USE_GTK
/* Id of action hook installed for scroll bars and horizontal scroll
   bars.  */

static XtActionHookId action_hook_id;
static XtActionHookId horizontal_action_hook_id;

/* Whether this is an Xaw with arrow-scrollbars.  This should imply
   that movements of 1/20 of the screen size are mapped to up/down.  */

static Boolean xaw3d_arrow_scroll;

/* Whether the drag scrolling maintains the mouse at the top of the
   thumb.  If not, resizing the thumb needs to be done more carefully
   to avoid jerkiness.  */

static Boolean xaw3d_pick_top;

/* Action hook installed via XtAppAddActionHook when toolkit scroll
   bars are used..  The hook is responsible for detecting when
   the user ends an interaction with the scroll bar, and generates
   a `end-scroll' SCROLL_BAR_CLICK_EVENT' event if so.  */

static void
xt_action_hook (Widget widget, XtPointer client_data, String action_name,
		XEvent *event, String *params, Cardinal *num_params)
{
  bool scroll_bar_p;
  const char *end_action;

#ifdef USE_MOTIF
  scroll_bar_p = XmIsScrollBar (widget);
  end_action = "Release";
#else /* !USE_MOTIF i.e. use Xaw */
  scroll_bar_p = XtIsSubclass (widget, scrollbarWidgetClass);
  end_action = "EndScroll";
#endif /* USE_MOTIF */

  if (scroll_bar_p
      && strcmp (action_name, end_action) == 0
      && WINDOW_LIVE_P (window_being_scrolled))
    {
      struct window *w;
      struct scroll_bar *bar;

      x_send_scroll_bar_event (window_being_scrolled,
			       scroll_bar_end_scroll, 0, 0, false);
      w = XWINDOW (window_being_scrolled);
      bar = XSCROLL_BAR (w->vertical_scroll_bar);

      if (bar->dragging != -1)
	{
	  bar->dragging = -1;
	  /* The thumb size is incorrect while dragging: fix it.  */
	  set_vertical_scroll_bar (w);
	}
      window_being_scrolled = Qnil;
#if defined (USE_LUCID)
      bar->last_seen_part = scroll_bar_nowhere;
#endif
      /* Xt timeouts no longer needed.  */
      toolkit_scroll_bar_interaction = false;
    }
}


static void
xt_horizontal_action_hook (Widget widget, XtPointer client_data, String action_name,
			   XEvent *event, String *params, Cardinal *num_params)
{
  bool scroll_bar_p;
  const char *end_action;

#ifdef USE_MOTIF
  scroll_bar_p = XmIsScrollBar (widget);
  end_action = "Release";
#else /* !USE_MOTIF i.e. use Xaw */
  scroll_bar_p = XtIsSubclass (widget, scrollbarWidgetClass);
  end_action = "EndScroll";
#endif /* USE_MOTIF */

  if (scroll_bar_p
      && strcmp (action_name, end_action) == 0
      && WINDOWP (window_being_scrolled))
    {
      struct window *w;
      struct scroll_bar *bar;

      x_send_scroll_bar_event (window_being_scrolled,
			       scroll_bar_end_scroll, 0, 0, true);
      w = XWINDOW (window_being_scrolled);
      if (!NILP (w->horizontal_scroll_bar))
	{
	  bar = XSCROLL_BAR (w->horizontal_scroll_bar);
	  if (bar->dragging != -1)
	    {
	      bar->dragging = -1;
	      /* The thumb size is incorrect while dragging: fix it.  */
	      set_horizontal_scroll_bar (w);
	    }
	  window_being_scrolled = Qnil;
#if defined (USE_LUCID)
	  bar->last_seen_part = scroll_bar_nowhere;
#endif
	  /* Xt timeouts no longer needed.  */
	  toolkit_scroll_bar_interaction = false;
	}
    }
}
#endif /* not USE_GTK */

/* Protect WINDOW from garbage collection until a matching scroll bar
   message is received.  Return whether or not protection
   succeeded.  */
static bool
x_protect_window_for_callback (struct x_display_info *dpyinfo,
			       Lisp_Object window)
{
  if (dpyinfo->n_protected_windows + 1
      >= dpyinfo->protected_windows_max)
    return false;

  dpyinfo->protected_windows[dpyinfo->n_protected_windows++]
    = window;
  return true;
}

static Lisp_Object
x_unprotect_window_for_callback (struct x_display_info *dpyinfo)
{
  Lisp_Object window;

  if (!dpyinfo->n_protected_windows)
    return Qnil;

  window = dpyinfo->protected_windows[0];

  dpyinfo->n_protected_windows--;

  if (dpyinfo->n_protected_windows)
    memmove (dpyinfo->protected_windows, &dpyinfo->protected_windows[1],
	     sizeof (Lisp_Object) * dpyinfo->n_protected_windows);

  return window;
}

/* Send a client message with message type Xatom_Scrollbar for a
   scroll action to the frame of WINDOW.  PART is a value identifying
   the part of the scroll bar that was clicked on.  PORTION is the
   amount to scroll of a whole of WHOLE.  */

static void
x_send_scroll_bar_event (Lisp_Object window, enum scroll_bar_part part,
			 int portion, int whole, bool horizontal)
{
  XEvent event;
  XClientMessageEvent *ev = &event.xclient;
  struct window *w = XWINDOW (window);
  struct frame *f = XFRAME (w->frame);
  static_assert (INTPTR_WIDTH <= 64);

  /* Don't do anything if too many scroll bar events have been
     sent but not received.  */
  if (!x_protect_window_for_callback (FRAME_DISPLAY_INFO (f), window))
    return;

  block_input ();
  /* Construct a ClientMessage event to send to the frame.  */
  ev->type = ClientMessage;
  ev->message_type = (horizontal
		      ? FRAME_DISPLAY_INFO (f)->Xatom_Horizontal_Scrollbar
		      : FRAME_DISPLAY_INFO (f)->Xatom_Scrollbar);
  ev->display = FRAME_X_DISPLAY (f);
  ev->window = FRAME_X_WINDOW (f);
  ev->format = 32;

  /* These messages formerly contained a pointer to the window, but
     now that information is kept internally.  The following two
     fields are thus zero.  */
  ev->data.l[0] = 0;
  ev->data.l[1] = 0;
  ev->data.l[2] = part;
  ev->data.l[3] = portion;
  ev->data.l[4] = whole;

  /* Make Xt timeouts work while the scroll bar is active.  */
#ifdef USE_X_TOOLKIT
  toolkit_scroll_bar_interaction = true;
  x_activate_timeout_atimer ();
#endif

  /* Setting the event mask to zero means that the message will
     be sent to the client that created the window, and if that
     window no longer exists, no event will be sent.  */
  XSendEvent (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), False,
	      NoEventMask, &event);
  unblock_input ();
}


/* Transform a scroll bar ClientMessage EVENT to an Emacs input event
   in *IEVENT.  */

static void
x_scroll_bar_to_input_event (struct x_display_info *dpyinfo,
			     const XEvent *event,
			     struct input_event *ievent)
{
  Lisp_Object window;

  /* Every time a scroll bar ClientMessage event is sent, the window
     is pushed onto a queue that is traced for garbage collection.
     Every time we need a window for a read scroll bar event, we
     simply read from the other side of the queue.  */
  window = x_unprotect_window_for_callback (dpyinfo);

  if (NILP (window))
    {
      /* This means we are getting extra scroll bar events for some
	 reason, and shouldn't be possible in practice.  */
      EVENT_INIT (*ievent);
      return;
    }

  ievent->kind = SCROLL_BAR_CLICK_EVENT;
  ievent->frame_or_window = window;
  ievent->arg = Qnil;
  ievent->timestamp = x_get_last_toolkit_time (dpyinfo);
  ievent->code = 0;
  ievent->part = event->xclient.data.l[2];
  ievent->x = make_fixnum (event->xclient.data.l[3]);
  ievent->y = make_fixnum (event->xclient.data.l[4]);
  ievent->modifiers = 0;
}

/* Transform a horizontal scroll bar ClientMessage EVENT to an Emacs
   input event in *IEVENT.  */

static void
x_horizontal_scroll_bar_to_input_event (struct x_display_info *dpyinfo,
					const XEvent *event,
					struct input_event *ievent)
{
  Lisp_Object window;

  /* Every time a scroll bar ClientMessage event is sent, the window
     is pushed onto a queue that is traced for garbage collection.
     Every time we need a window for a read scroll bar event, we
     simply read from the other side of the queue.  */
  window = x_unprotect_window_for_callback (dpyinfo);

  if (NILP (window))
    {
      /* This means we are getting extra scroll bar events for some
	 reason, and shouldn't be possible in practice.  */
      EVENT_INIT (*ievent);
      return;
    }

  ievent->kind = HORIZONTAL_SCROLL_BAR_CLICK_EVENT;
  ievent->frame_or_window = window;
  ievent->arg = Qnil;
  ievent->timestamp = x_get_last_toolkit_time (dpyinfo);
  ievent->code = 0;
  ievent->part = event->xclient.data.l[2];
  ievent->x = make_fixnum (event->xclient.data.l[3]);
  ievent->y = make_fixnum (event->xclient.data.l[4]);
  ievent->modifiers = 0;
}


#ifdef USE_MOTIF

/* Minimum and maximum values used for Motif scroll bars.  */

#define XM_SB_MAX 10000000

/* Scroll bar callback for Motif scroll bars.  WIDGET is the scroll
   bar widget.  CLIENT_DATA is a pointer to the scroll_bar structure.
   CALL_DATA is a pointer to a XmScrollBarCallbackStruct.  */

static void
xm_scroll_callback (Widget widget, XtPointer client_data, XtPointer call_data)
{
  struct scroll_bar *bar = client_data;
  XmScrollBarCallbackStruct *cs = call_data;
  enum scroll_bar_part part = scroll_bar_nowhere;
  bool horizontal = bar->horizontal;
  int whole = 0, portion = 0;

  switch (cs->reason)
    {
    case XmCR_DECREMENT:
      bar->dragging = -1;
      part = horizontal ? scroll_bar_left_arrow : scroll_bar_up_arrow;
      break;

    case XmCR_INCREMENT:
      bar->dragging = -1;
      part = horizontal ? scroll_bar_right_arrow : scroll_bar_down_arrow;
      break;

    case XmCR_PAGE_DECREMENT:
      bar->dragging = -1;
      part = horizontal ? scroll_bar_before_handle : scroll_bar_above_handle;
      break;

    case XmCR_PAGE_INCREMENT:
      bar->dragging = -1;
      part = horizontal ? scroll_bar_after_handle : scroll_bar_below_handle;
      break;

    case XmCR_TO_TOP:
      bar->dragging = -1;
      part = horizontal ? scroll_bar_to_leftmost : scroll_bar_to_top;
      break;

    case XmCR_TO_BOTTOM:
      bar->dragging = -1;
      part = horizontal ? scroll_bar_to_rightmost : scroll_bar_to_bottom;
      break;

    case XmCR_DRAG:
      {
	int slider_size;

	block_input ();
	XtVaGetValues (widget, XmNsliderSize, &slider_size, NULL);
	unblock_input ();

	if (horizontal)
	  {
	    portion = bar->whole * ((float)cs->value / XM_SB_MAX);
	    whole = bar->whole * ((float)(XM_SB_MAX - slider_size) / XM_SB_MAX);
	    portion = min (portion, whole);
	    part = scroll_bar_horizontal_handle;
	  }
	else
	  {
	    whole = XM_SB_MAX - slider_size;
	    portion = min (cs->value, whole);
	    part = scroll_bar_handle;
	  }

	bar->dragging = cs->value;
      }
      break;

    case XmCR_VALUE_CHANGED:
      break;
    };

  if (part != scroll_bar_nowhere)
    {
      window_being_scrolled = bar->window;
      x_send_scroll_bar_event (bar->window, part, portion, whole,
			       bar->horizontal);
    }
}

#elif defined USE_GTK

/* Scroll bar callback for GTK scroll bars.  WIDGET is the scroll
   bar widget.  DATA is a pointer to the scroll_bar structure. */

static gboolean
xg_scroll_callback (GtkRange *range, GtkScrollType scroll,
                    gdouble value, gpointer user_data)
{
  int whole, portion;
  struct scroll_bar *bar;
  enum scroll_bar_part part;
  GtkAdjustment *adj;
  struct frame *f;
  guint32 time;
  struct x_display_info *dpyinfo;

  if (xg_ignore_gtk_scrollbar) return false;

  whole = 0;
  portion = 0;
  bar = user_data;
  part = scroll_bar_nowhere;
  adj = GTK_ADJUSTMENT (gtk_range_get_adjustment (range));
  f = g_object_get_data (G_OBJECT (range), XG_FRAME_DATA);
  time = gtk_get_current_event_time ();
  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (time != GDK_CURRENT_TIME)
    x_display_set_last_user_time (dpyinfo, time, true,
				  true);

  switch (scroll)
    {
    case GTK_SCROLL_JUMP:
      /* Buttons 1 2 or 3 must be grabbed.  */
      if (FRAME_DISPLAY_INFO (f)->grabbed != 0
          && FRAME_DISPLAY_INFO (f)->grabbed < (1 << 4))
        {
	  if (bar->horizontal)
	    {
	      part = scroll_bar_horizontal_handle;
	      whole = (int)(gtk_adjustment_get_upper (adj) -
			    gtk_adjustment_get_page_size (adj));
	      portion = min ((int)value, whole);
	      bar->dragging = portion;
	    }
	  else
	    {
	      part = scroll_bar_handle;
	      whole = gtk_adjustment_get_upper (adj) -
		gtk_adjustment_get_page_size (adj);
	      portion = min ((int)value, whole);
	      bar->dragging = portion;
	    }
	}
      break;
    case GTK_SCROLL_STEP_BACKWARD:
      part = (bar->horizontal
	      ? scroll_bar_left_arrow : scroll_bar_up_arrow);
      bar->dragging = -1;
      break;
    case GTK_SCROLL_STEP_FORWARD:
      part = (bar->horizontal
	      ? scroll_bar_right_arrow : scroll_bar_down_arrow);
      bar->dragging = -1;
      break;
    case GTK_SCROLL_PAGE_BACKWARD:
      part = (bar->horizontal
	      ? scroll_bar_before_handle : scroll_bar_above_handle);
      bar->dragging = -1;
      break;
    case GTK_SCROLL_PAGE_FORWARD:
      part = (bar->horizontal
	      ? scroll_bar_after_handle : scroll_bar_below_handle);
      bar->dragging = -1;
      break;
    default:
      break;
    }

  if (part != scroll_bar_nowhere)
    {
      window_being_scrolled = bar->window;
      x_send_scroll_bar_event (bar->window, part, portion, whole,
			       bar->horizontal);
    }

  return false;
}

/* Callback for button release. Sets dragging to -1 when dragging is done.  */

static gboolean
xg_end_scroll_callback (GtkWidget *widget,
                        GdkEventButton *event,
                        gpointer user_data)
{
  struct scroll_bar *bar;

  bar = user_data;
  bar->dragging = -1;

  if (WINDOWP (window_being_scrolled))
    {
      x_send_scroll_bar_event (window_being_scrolled,
                               scroll_bar_end_scroll, 0, 0, bar->horizontal);
      window_being_scrolled = Qnil;
    }

  return false;
}


#else /* not USE_GTK and not USE_MOTIF */

/* Xaw scroll bar callback.  Invoked when the thumb is dragged.
   WIDGET is the scroll bar widget.  CLIENT_DATA is a pointer to the
   scroll bar struct.  CALL_DATA is a pointer to a float saying where
   the thumb is.  */

static void
xaw_jump_callback (Widget widget, XtPointer client_data, XtPointer call_data)
{
  struct scroll_bar *bar = client_data;
  float *top_addr = call_data;
  float top = *top_addr;
  float shown;
  int whole, portion, height, width;
  enum scroll_bar_part part;
  bool horizontal = bar->horizontal;

  if (horizontal)
    {
      /* Get the size of the thumb, a value between 0 and 1.  */
      block_input ();
      XtVaGetValues (widget, XtNshown, &shown, XtNwidth, &width, NULL);
      unblock_input ();

      if (shown < 1)
	{
	  whole = bar->whole - (shown * bar->whole);
	  portion = min (top * bar->whole, whole);
	}
      else
	{
	  whole = bar->whole;
	  portion = 0;
	}

      part = scroll_bar_horizontal_handle;
    }
  else
    {
      /* Get the size of the thumb, a value between 0 and 1.  */
      block_input ();
      XtVaGetValues (widget, XtNshown, &shown, XtNheight, &height, NULL);
      unblock_input ();

      whole = 10000000;
      portion = shown < 1 ? top * whole : 0;

      if (shown < 1 && (eabs (top + shown - 1) < 1.0f / height))
	/* Some derivatives of Xaw refuse to shrink the thumb when you reach
	   the bottom, so we force the scrolling whenever we see that we're
	   too close to the bottom (in x_set_toolkit_scroll_bar_thumb
	   we try to ensure that we always stay two pixels away from the
	   bottom).  */
	part = scroll_bar_down_arrow;
      else
	part = scroll_bar_handle;
    }

  window_being_scrolled = bar->window;
  bar->dragging = portion;
  bar->last_seen_part = part;
  x_send_scroll_bar_event (bar->window, part, portion, whole, bar->horizontal);
}


/* Xaw scroll bar callback.  Invoked for incremental scrolling.,
   i.e. line or page up or down.  WIDGET is the Xaw scroll bar
   widget.  CLIENT_DATA is a pointer to the scroll_bar structure for
   the scroll bar.  CALL_DATA is an integer specifying the action that
   has taken place.  Its magnitude is in the range 0..height of the
   scroll bar.  Negative values mean scroll towards buffer start.
   Values < height of scroll bar mean line-wise movement.  */

static void
xaw_scroll_callback (Widget widget, XtPointer client_data, XtPointer call_data)
{
  struct scroll_bar *bar = client_data;
  /* The position really is stored cast to a pointer.  */
  int position = (intptr_t) call_data;
  Dimension height, width;
  enum scroll_bar_part part;

  if (bar->horizontal)
    {
      /* Get the width of the scroll bar.  */
      block_input ();
      XtVaGetValues (widget, XtNwidth, &width, NULL);
      unblock_input ();

      if (eabs (position) >= width)
	part = (position < 0) ? scroll_bar_before_handle : scroll_bar_after_handle;

      /* If Xaw3d was compiled with ARROW_SCROLLBAR,
	 it maps line-movement to call_data = max(5, height/20).  */
      else if (xaw3d_arrow_scroll && eabs (position) <= max (5, width / 20))
	part = (position < 0) ? scroll_bar_left_arrow : scroll_bar_right_arrow;
      else
	part = scroll_bar_move_ratio;

      window_being_scrolled = bar->window;
      bar->dragging = -1;
      bar->last_seen_part = part;
      x_send_scroll_bar_event (bar->window, part, position, width,
			       bar->horizontal);
    }
  else
    {

      /* Get the height of the scroll bar.  */
      block_input ();
      XtVaGetValues (widget, XtNheight, &height, NULL);
      unblock_input ();

      if (eabs (position) >= height)
	part = (position < 0) ? scroll_bar_above_handle : scroll_bar_below_handle;

      /* If Xaw3d was compiled with ARROW_SCROLLBAR,
	 it maps line-movement to call_data = max(5, height/20).  */
      else if (xaw3d_arrow_scroll && eabs (position) <= max (5, height / 20))
	part = (position < 0) ? scroll_bar_up_arrow : scroll_bar_down_arrow;
      else
	part = scroll_bar_move_ratio;

      window_being_scrolled = bar->window;
      bar->dragging = -1;
      bar->last_seen_part = part;
      x_send_scroll_bar_event (bar->window, part, position, height,
			       bar->horizontal);
    }
}

#endif /* not USE_GTK and not USE_MOTIF */

#define SCROLL_BAR_NAME "verticalScrollBar"
#define SCROLL_BAR_HORIZONTAL_NAME "horizontalScrollBar"

/* Create the widget for scroll bar BAR on frame F.  Record the widget
   and X window of the scroll bar in BAR.  */

#ifdef USE_GTK
static void
x_create_toolkit_scroll_bar (struct frame *f, struct scroll_bar *bar)
{
  const char *scroll_bar_name = SCROLL_BAR_NAME;

  block_input ();
  xg_create_scroll_bar (f, bar, G_CALLBACK (xg_scroll_callback),
                        G_CALLBACK (xg_end_scroll_callback),
                        scroll_bar_name);
  unblock_input ();
}

static void
x_create_horizontal_toolkit_scroll_bar (struct frame *f, struct scroll_bar *bar)
{
  const char *scroll_bar_name = SCROLL_BAR_HORIZONTAL_NAME;

  block_input ();
  xg_create_horizontal_scroll_bar (f, bar, G_CALLBACK (xg_scroll_callback),
				   G_CALLBACK (xg_end_scroll_callback),
				   scroll_bar_name);
  unblock_input ();
}

#else /* not USE_GTK */

static void
x_create_toolkit_scroll_bar (struct frame *f, struct scroll_bar *bar)
{
  Window xwindow;
  Widget widget;
  Arg av[20];
  int ac = 0;
  const char *scroll_bar_name = SCROLL_BAR_NAME;
  unsigned long pixel;

  block_input ();

#ifdef USE_MOTIF
  /* Set resources.  Create the widget.  */
  XtSetArg (av[ac], XtNmappedWhenManaged, False); ++ac;
  XtSetArg (av[ac], XmNminimum, 0); ++ac;
  XtSetArg (av[ac], XmNmaximum, XM_SB_MAX); ++ac;
  XtSetArg (av[ac], XmNorientation, XmVERTICAL); ++ac;
  XtSetArg (av[ac], XmNprocessingDirection, XmMAX_ON_BOTTOM), ++ac;
  XtSetArg (av[ac], XmNincrement, 1); ++ac;
  XtSetArg (av[ac], XmNpageIncrement, 1); ++ac;

  /* Note: "background" is the thumb color, and "trough" is the color behind
     everything. */
  pixel = f->output_data.x->scroll_bar_foreground_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XmNbackground, pixel);
      ++ac;
    }

  pixel = f->output_data.x->scroll_bar_background_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XmNtroughColor, pixel);
      ++ac;
    }

  widget = XmCreateScrollBar (f->output_data.x->edit_widget,
			      (char *) scroll_bar_name, av, ac);

  /* Add one callback for everything that can happen.  */
  XtAddCallback (widget, XmNdecrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNdragCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNincrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNpageDecrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNpageIncrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNtoBottomCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNtoTopCallback, xm_scroll_callback,
		 (XtPointer) bar);

  /* Realize the widget.  Only after that is the X window created.  */
  XtRealizeWidget (widget);

  /* Set the cursor to an arrow.  I didn't find a resource to do that.
     And I'm wondering why it hasn't an arrow cursor by default.  */
  XDefineCursor (XtDisplay (widget), XtWindow (widget),
                 f->output_data.x->nontext_cursor);

#ifdef HAVE_XINPUT2
  /* Ask for input extension button and motion events.  This lets us
     send the proper `wheel-up' or `wheel-down' events to Emacs.  */
  if (FRAME_DISPLAY_INFO (f)->supports_xi2)
    xi_select_scroll_bar_events (FRAME_DISPLAY_INFO (f),
				 XtWindow (widget));
#endif
#else /* !USE_MOTIF i.e. use Xaw */

  /* Set resources.  Create the widget.  The background of the
     Xaw3d scroll bar widget is a little bit light for my taste.
     We don't alter it here to let users change it according
     to their taste with `emacs*verticalScrollBar.background: xxx'.  */
  XtSetArg (av[ac], XtNmappedWhenManaged, False); ++ac;
  XtSetArg (av[ac], XtNorientation, XtorientVertical); ++ac;
  /* For smoother scrolling with Xaw3d   -sm */
  /* XtSetArg (av[ac], XtNpickTop, True); ++ac; */

  pixel = f->output_data.x->scroll_bar_foreground_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XtNforeground, pixel);
      ++ac;
    }

  pixel = f->output_data.x->scroll_bar_background_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XtNbackground, pixel);
      ++ac;
    }

  /* Top/bottom shadow colors.  */

  /* Allocate them, if necessary.  */
  if (f->output_data.x->scroll_bar_top_shadow_pixel == -1)
    {
      pixel = f->output_data.x->scroll_bar_background_pixel;
      if (pixel != -1)
        {
          if (!x_alloc_lighter_color (f, FRAME_X_DISPLAY (f),
                                      FRAME_X_COLORMAP (f),
                                      &pixel, 1.2, 0x8000))
            pixel = -1;
          f->output_data.x->scroll_bar_top_shadow_pixel = pixel;
        }
    }
  if (f->output_data.x->scroll_bar_bottom_shadow_pixel == -1)
    {
      pixel = f->output_data.x->scroll_bar_background_pixel;
      if (pixel != -1)
        {
          if (!x_alloc_lighter_color (f, FRAME_X_DISPLAY (f),
                                      FRAME_X_COLORMAP (f),
                                      &pixel, 0.6, 0x4000))
            pixel = -1;
          f->output_data.x->scroll_bar_bottom_shadow_pixel = pixel;
        }
    }

#ifdef XtNbeNiceToColormap
  /* Tell the toolkit about them.  */
  if (f->output_data.x->scroll_bar_top_shadow_pixel == -1
      || f->output_data.x->scroll_bar_bottom_shadow_pixel == -1)
    /* We tried to allocate a color for the top/bottom shadow, and
       failed, so tell Xaw3d to use dithering instead.   */
    /* But only if we have a small colormap.  Xaw3d can allocate nice
       colors itself.  */
    {
      XtSetArg (av[ac], (String) XtNbeNiceToColormap,
                DefaultDepthOfScreen (FRAME_X_SCREEN (f)) < 16);
      ++ac;
    }
  else
    /* Tell what colors Xaw3d should use for the top/bottom shadow, to
       be more consistent with other emacs 3d colors, and since Xaw3d is
       not good at dealing with allocation failure.  */
    {
      /* This tells Xaw3d to use real colors instead of dithering for
	 the shadows.  */
      XtSetArg (av[ac], (String) XtNbeNiceToColormap, False);
      ++ac;

      /* Specify the colors.  */
      pixel = f->output_data.x->scroll_bar_top_shadow_pixel;
      if (pixel != -1)
	{
	  XtSetArg (av[ac], (String) XtNtopShadowPixel, pixel);
	  ++ac;
	}
      pixel = f->output_data.x->scroll_bar_bottom_shadow_pixel;
      if (pixel != -1)
	{
	  XtSetArg (av[ac], (String) XtNbottomShadowPixel, pixel);
	  ++ac;
	}
    }
#endif

  widget = XtCreateWidget (scroll_bar_name, scrollbarWidgetClass,
			   f->output_data.x->edit_widget, av, ac);

  {
    char const *initial = "";
    char const *val = initial;
    XtVaGetValues (widget, XtNscrollVCursor, (XtPointer) &val,
#ifdef XtNarrowScrollbars
		   XtNarrowScrollbars, (XtPointer) &xaw3d_arrow_scroll,
#endif
		   XtNpickTop, (XtPointer) &xaw3d_pick_top, NULL);
    if (xaw3d_arrow_scroll || val == initial)
      {	/* ARROW_SCROLL */
	xaw3d_arrow_scroll = True;
	/* Isn't that just a personal preference ?   --Stef */
	XtVaSetValues (widget, XtNcursorName, "top_left_arrow", NULL);
      }
  }

  /* Define callbacks.  */
  XtAddCallback (widget, XtNjumpProc, xaw_jump_callback, (XtPointer) bar);
  XtAddCallback (widget, XtNscrollProc, xaw_scroll_callback,
		 (XtPointer) bar);

  /* Realize the widget.  Only after that is the X window created.  */
  XtRealizeWidget (widget);

#endif /* !USE_MOTIF */

  /* Install an action hook that lets us detect when the user
     finishes interacting with a scroll bar.  */
  if (action_hook_id == 0)
    action_hook_id = XtAppAddActionHook (Xt_app_con, xt_action_hook, 0);

  /* Remember X window and widget in the scroll bar vector.  */
  SET_SCROLL_BAR_X_WIDGET (bar, widget);
  xwindow = XtWindow (widget);
  bar->x_window = xwindow;
  bar->whole = 1;
  bar->horizontal = false;

  unblock_input ();
}

static void
x_create_horizontal_toolkit_scroll_bar (struct frame *f, struct scroll_bar *bar)
{
  Window xwindow;
  Widget widget;
  Arg av[20];
  int ac = 0;
  const char *scroll_bar_name = SCROLL_BAR_HORIZONTAL_NAME;
  unsigned long pixel;

  block_input ();

#ifdef USE_MOTIF
  /* Set resources.  Create the widget.  */
  XtSetArg (av[ac], XtNmappedWhenManaged, False); ++ac;
  XtSetArg (av[ac], XmNminimum, 0); ++ac;
  XtSetArg (av[ac], XmNmaximum, XM_SB_MAX); ++ac;
  XtSetArg (av[ac], XmNorientation, XmHORIZONTAL); ++ac;
  XtSetArg (av[ac], XmNprocessingDirection, XmMAX_ON_RIGHT), ++ac;
  XtSetArg (av[ac], XmNincrement, 1); ++ac;
  XtSetArg (av[ac], XmNpageIncrement, 1); ++ac;

  /* Note: "background" is the thumb color, and "trough" is the color behind
     everything. */
  pixel = f->output_data.x->scroll_bar_foreground_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XmNbackground, pixel);
      ++ac;
    }

  pixel = f->output_data.x->scroll_bar_background_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XmNtroughColor, pixel);
      ++ac;
    }

  widget = XmCreateScrollBar (f->output_data.x->edit_widget,
			      (char *) scroll_bar_name, av, ac);

  /* Add one callback for everything that can happen.  */
  XtAddCallback (widget, XmNdecrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNdragCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNincrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNpageDecrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNpageIncrementCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNtoBottomCallback, xm_scroll_callback,
		 (XtPointer) bar);
  XtAddCallback (widget, XmNtoTopCallback, xm_scroll_callback,
		 (XtPointer) bar);

  /* Realize the widget.  Only after that is the X window created.  */
  XtRealizeWidget (widget);

  /* Set the cursor to an arrow.  I didn't find a resource to do that.
     And I'm wondering why it hasn't an arrow cursor by default.  */
  XDefineCursor (XtDisplay (widget), XtWindow (widget),
                 f->output_data.x->nontext_cursor);

#ifdef HAVE_XINPUT2
  /* Ask for input extension button and motion events.  This lets us
     send the proper `wheel-up' or `wheel-down' events to Emacs.  */
  if (FRAME_DISPLAY_INFO (f)->supports_xi2)
    xi_select_scroll_bar_events (FRAME_DISPLAY_INFO (f),
				 XtWindow (widget));
#endif
#else /* !USE_MOTIF i.e. use Xaw */

  /* Set resources.  Create the widget.  The background of the
     Xaw3d scroll bar widget is a little bit light for my taste.
     We don't alter it here to let users change it according
     to their taste with `emacs*verticalScrollBar.background: xxx'.  */
  XtSetArg (av[ac], XtNmappedWhenManaged, False); ++ac;
  XtSetArg (av[ac], XtNorientation, XtorientHorizontal); ++ac;
  /* For smoother scrolling with Xaw3d   -sm */
  /* XtSetArg (av[ac], XtNpickTop, True); ++ac; */

  pixel = f->output_data.x->scroll_bar_foreground_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XtNforeground, pixel);
      ++ac;
    }

  pixel = f->output_data.x->scroll_bar_background_pixel;
  if (pixel != -1)
    {
      XtSetArg (av[ac], XtNbackground, pixel);
      ++ac;
    }

  /* Top/bottom shadow colors.  */

  /* Allocate them, if necessary.  */
  if (f->output_data.x->scroll_bar_top_shadow_pixel == -1)
    {
      pixel = f->output_data.x->scroll_bar_background_pixel;
      if (pixel != -1)
        {
          if (!x_alloc_lighter_color (f, FRAME_X_DISPLAY (f),
                                      FRAME_X_COLORMAP (f),
                                      &pixel, 1.2, 0x8000))
            pixel = -1;
          f->output_data.x->scroll_bar_top_shadow_pixel = pixel;
        }
    }
  if (f->output_data.x->scroll_bar_bottom_shadow_pixel == -1)
    {
      pixel = f->output_data.x->scroll_bar_background_pixel;
      if (pixel != -1)
        {
          if (!x_alloc_lighter_color (f, FRAME_X_DISPLAY (f),
                                      FRAME_X_COLORMAP (f),
                                      &pixel, 0.6, 0x4000))
            pixel = -1;
          f->output_data.x->scroll_bar_bottom_shadow_pixel = pixel;
        }
    }

#ifdef XtNbeNiceToColormap
  /* Tell the toolkit about them.  */
  if (f->output_data.x->scroll_bar_top_shadow_pixel == -1
      || f->output_data.x->scroll_bar_bottom_shadow_pixel == -1)
    /* We tried to allocate a color for the top/bottom shadow, and
       failed, so tell Xaw3d to use dithering instead.   */
    /* But only if we have a small colormap.  Xaw3d can allocate nice
       colors itself.  */
    {
      XtSetArg (av[ac], (String) XtNbeNiceToColormap,
                DefaultDepthOfScreen (FRAME_X_SCREEN (f)) < 16);
      ++ac;
    }
  else
    /* Tell what colors Xaw3d should use for the top/bottom shadow, to
       be more consistent with other emacs 3d colors, and since Xaw3d is
       not good at dealing with allocation failure.  */
    {
      /* This tells Xaw3d to use real colors instead of dithering for
	 the shadows.  */
      XtSetArg (av[ac], (String) XtNbeNiceToColormap, False);
      ++ac;

      /* Specify the colors.  */
      pixel = f->output_data.x->scroll_bar_top_shadow_pixel;
      if (pixel != -1)
	{
	  XtSetArg (av[ac], (String) XtNtopShadowPixel, pixel);
	  ++ac;
	}
      pixel = f->output_data.x->scroll_bar_bottom_shadow_pixel;
      if (pixel != -1)
	{
	  XtSetArg (av[ac], (String) XtNbottomShadowPixel, pixel);
	  ++ac;
	}
    }
#endif

  widget = XtCreateWidget (scroll_bar_name, scrollbarWidgetClass,
			   f->output_data.x->edit_widget, av, ac);

  {
    char const *initial = "";
    char const *val = initial;
    XtVaGetValues (widget, XtNscrollVCursor, (XtPointer) &val,
#ifdef XtNarrowScrollbars
		   XtNarrowScrollbars, (XtPointer) &xaw3d_arrow_scroll,
#endif
		   XtNpickTop, (XtPointer) &xaw3d_pick_top, NULL);
    if (xaw3d_arrow_scroll || val == initial)
      {	/* ARROW_SCROLL */
	xaw3d_arrow_scroll = True;
	/* Isn't that just a personal preference ?   --Stef */
	XtVaSetValues (widget, XtNcursorName, "top_left_arrow", NULL);
      }
  }

  /* Define callbacks.  */
  XtAddCallback (widget, XtNjumpProc, xaw_jump_callback, (XtPointer) bar);
  XtAddCallback (widget, XtNscrollProc, xaw_scroll_callback,
		 (XtPointer) bar);

  /* Realize the widget.  Only after that is the X window created.  */
  XtRealizeWidget (widget);

#endif /* !USE_MOTIF */

  /* Install an action hook that lets us detect when the user
     finishes interacting with a scroll bar.  */
  if (horizontal_action_hook_id == 0)
   horizontal_action_hook_id
     = XtAppAddActionHook (Xt_app_con, xt_horizontal_action_hook, 0);

  /* Remember X window and widget in the scroll bar vector.  */
  SET_SCROLL_BAR_X_WIDGET (bar, widget);
  xwindow = XtWindow (widget);
  bar->x_window = xwindow;
  bar->whole = 1;
  bar->horizontal = true;

  unblock_input ();
}
#endif /* not USE_GTK */


/* Set the thumb size and position of scroll bar BAR.  We are currently
   displaying PORTION out of a whole WHOLE, and our position POSITION.  */

#ifdef USE_GTK
static void
x_set_toolkit_scroll_bar_thumb (struct scroll_bar *bar, int portion, int position, int whole)
{
  xg_set_toolkit_scroll_bar_thumb (bar, portion, position, whole);
}

static void
x_set_toolkit_horizontal_scroll_bar_thumb (struct scroll_bar *bar, int portion, int position, int whole)
{
  xg_set_toolkit_horizontal_scroll_bar_thumb (bar, portion, position, whole);
}

#else /* not USE_GTK */
static void
x_set_toolkit_scroll_bar_thumb (struct scroll_bar *bar, int portion, int position,
				int whole)
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  Widget widget = SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar);
  float top, shown;

  block_input ();

#ifdef USE_MOTIF

  if (scroll_bar_adjust_thumb_portion_p)
    {
      /* We use an estimate of 30 chars per line rather than the real
         `portion' value.  This has the disadvantage that the thumb size
         is not very representative, but it makes our life a lot easier.
         Otherwise, we have to constantly adjust the thumb size, which
         we can't always do quickly enough: while dragging, the size of
         the thumb might prevent the user from dragging the thumb all the
         way to the end.  but Motif and some versions of Xaw3d don't allow
         updating the thumb size while dragging.  Also, even if we can update
         its size, the update will often happen too late.
         If you don't believe it, check out revision 1.650 of xterm.c to see
         what hoops we were going through and the still poor behavior we got.  */
      portion = WINDOW_TOTAL_LINES (XWINDOW (bar->window)) * 30;
      /* When the thumb is at the bottom, position == whole.
         So we need to increase `whole' to make space for the thumb.  */
      whole += portion;
    }

  if (whole <= 0)
    top = 0, shown = 1;
  else
    {
      top = (float) position / whole;
      shown = (float) portion / whole;
    }

  if (bar->dragging == -1)
    {
      int size, value;

      /* Slider size.  Must be in the range [1 .. MAX - MIN] where MAX
         is the scroll bar's maximum and MIN is the scroll bar's minimum
	 value.  */
      size = clip_to_bounds (1, shown * XM_SB_MAX, XM_SB_MAX);

      /* Position.  Must be in the range [MIN .. MAX - SLIDER_SIZE].  */
      value = top * XM_SB_MAX;
      value = min (value, XM_SB_MAX - size);

      XmScrollBarSetValues (widget, value, size, 0, 0, False);
    }
#else /* !USE_MOTIF i.e. use Xaw */

  if (whole == 0)
    top = 0, shown = 1;
  else
    {
      top = (float) position / whole;
      shown = (float) portion / whole;
    }

  {
    float old_top, old_shown;
    Dimension height;
    XtVaGetValues (widget,
		   XtNtopOfThumb, &old_top,
		   XtNshown, &old_shown,
		   XtNheight, &height,
		   NULL);

    /* Massage the top+shown values.  */
    if (bar->dragging == -1 || bar->last_seen_part == scroll_bar_down_arrow)
      top = max (0, min (1, top));
    else
      top = old_top;
#if ! defined (HAVE_XAW3D)
    /* With Xaw, 'top' values too closer to 1.0 may
       cause the thumb to disappear.  Fix that.  */
    top = min (top, 0.99f);
#endif
    /* Keep two pixels available for moving the thumb down.  */
    shown = max (0, min (1 - top - (2.0f / height), shown));
#if ! defined (HAVE_XAW3D)
    /* Likewise with too small 'shown'.  */
    shown = max (shown, 0.01f);
#endif

    /* If the call to XawScrollbarSetThumb below doesn't seem to
       work, check that 'NARROWPROTO' is defined in src/config.h.
       If this is not so, most likely you need to fix configure.  */
    if (top != old_top || shown != old_shown)
      {
	if (bar->dragging == -1)
	  XawScrollbarSetThumb (widget, top, shown);
	else
	  {
	    /* Try to make the scrolling a tad smoother.  */
	    if (!xaw3d_pick_top)
	      shown = min (shown, old_shown);

	    XawScrollbarSetThumb (widget, top, shown);
	  }
      }
  }
#endif /* !USE_MOTIF */

  unblock_input ();
}

static void
x_set_toolkit_horizontal_scroll_bar_thumb (struct scroll_bar *bar, int portion, int position,
				int whole)
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  Widget widget = SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar);
  float top, shown;

  block_input ();

#ifdef USE_MOTIF
  bar->whole = whole;
  shown = (float) portion / whole;
  top = (float) position / (whole - portion);
  {
    int size = clip_to_bounds (1, shown * XM_SB_MAX, XM_SB_MAX);
    int value = clip_to_bounds (0, top * (XM_SB_MAX - size), XM_SB_MAX - size);

    XmScrollBarSetValues (widget, value, size, 0, 0, False);
  }
#else /* !USE_MOTIF i.e. use Xaw */
  bar->whole = whole;
  if (whole == 0)
    top = 0, shown = 1;
  else
    {
      top = (float) position / whole;
      shown = (float) portion / whole;
    }

  {
    float old_top, old_shown;
    Dimension height;
    XtVaGetValues (widget,
		   XtNtopOfThumb, &old_top,
		   XtNshown, &old_shown,
		   XtNheight, &height,
		   NULL);

#if false
    /* Massage the top+shown values.  */
    if (bar->dragging == -1 || bar->last_seen_part == scroll_bar_down_arrow)
      top = max (0, min (1, top));
    else
      top = old_top;
#if ! defined (HAVE_XAW3D)
    /* With Xaw, 'top' values too closer to 1.0 may
       cause the thumb to disappear.  Fix that.  */
    top = min (top, 0.99f);
#endif
    /* Keep two pixels available for moving the thumb down.  */
    shown = max (0, min (1 - top - (2.0f / height), shown));
#if ! defined (HAVE_XAW3D)
    /* Likewise with too small 'shown'.  */
    shown = max (shown, 0.01f);
#endif
#endif

    /* If the call to XawScrollbarSetThumb below doesn't seem to
       work, check that 'NARROWPROTO' is defined in src/config.h.
       If this is not so, most likely you need to fix configure.  */
    XawScrollbarSetThumb (widget, top, shown);
#if false
    if (top != old_top || shown != old_shown)
      {
	if (bar->dragging == -1)
	  XawScrollbarSetThumb (widget, top, shown);
	else
	  {
	    /* Try to make the scrolling a tad smoother.  */
	    if (!xaw3d_pick_top)
	      shown = min (shown, old_shown);

	    XawScrollbarSetThumb (widget, top, shown);
	  }
      }
#endif
  }
#endif /* !USE_MOTIF */

  unblock_input ();
}
#endif /* not USE_GTK */

#endif /* USE_TOOLKIT_SCROLL_BARS */



/************************************************************************
			 Scroll bars, general
 ************************************************************************/

/* Create a scroll bar and return the scroll bar vector for it.  W is
   the Emacs window on which to create the scroll bar. TOP, LEFT,
   WIDTH and HEIGHT are the pixel coordinates and dimensions of the
   scroll bar. */

static struct scroll_bar *
x_scroll_bar_create (struct window *w, int top, int left,
		     int width, int height, bool horizontal)
{
  struct frame *f = XFRAME (w->frame);
  struct scroll_bar *bar = ALLOCATE_PSEUDOVECTOR (struct scroll_bar, prev,
						  PVEC_OTHER);
  Lisp_Object barobj;

  block_input ();

#ifdef USE_TOOLKIT_SCROLL_BARS
  if (horizontal)
    x_create_horizontal_toolkit_scroll_bar (f, bar);
  else
    x_create_toolkit_scroll_bar (f, bar);
#else /* not USE_TOOLKIT_SCROLL_BARS */
  {
    XSetWindowAttributes a;
    unsigned long mask;
    Window window;

    a.event_mask = (ButtonPressMask | ButtonReleaseMask
		    | ButtonMotionMask | PointerMotionHintMask);
    a.cursor = FRAME_DISPLAY_INFO (f)->vertical_scroll_bar_cursor;

    mask = (CWEventMask | CWCursor);

    /* Clear the area of W that will serve as a scroll bar.  This is
       for the case that a window has been split horizontally.  In
       this case, no clear_frame is generated to reduce flickering.  */
    if (width > 0 && window_box_height (w) > 0)
      x_clear_area (f, left, top, width, window_box_height (w));

    /* Create an input only window.  Scroll bar contents are drawn to
       the frame window itself, so they can be double buffered and
       synchronized using the same mechanism as the frame.  */
    window = XCreateWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f),
			    /* Position and size of scroll bar.  */
			    left, top, width, height,
			    /* Border width.  */
			    0,
			    /* Depth.  */
			    CopyFromParent,
			    /* Class.  */
			    InputOnly,
			    /* Visual class.  */
			    CopyFromParent,
			     /* Attributes.  */
			    mask, &a);

#ifdef HAVE_XINPUT2
  /* Ask for input extension button and motion events.  This lets us
     send the proper `wheel-up' or `wheel-down' events to Emacs.  */
  if (FRAME_DISPLAY_INFO (f)->supports_xi2)
    xi_select_scroll_bar_events (FRAME_DISPLAY_INFO (f),
				 window);
#endif

    bar->x_window = window;
  }
#endif /* not USE_TOOLKIT_SCROLL_BARS */

  XSETWINDOW (bar->window, w);
  bar->top = top;
  bar->left = left;
  bar->width = width;
  bar->height = height;
  bar->start = 0;
  bar->end = 0;
  bar->dragging = -1;
  bar->horizontal = horizontal;
#if defined (USE_TOOLKIT_SCROLL_BARS) && defined (USE_LUCID)
  bar->last_seen_part = scroll_bar_nowhere;
#endif

  /* Add bar to its frame's list of scroll bars.  */
  bar->next = FRAME_SCROLL_BARS (f);
  bar->prev = Qnil;
  XSETVECTOR (barobj, bar);
  fset_scroll_bars (f, barobj);
  if (!NILP (bar->next))
    XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);

  /* Map the window/widget.  */
#ifdef USE_TOOLKIT_SCROLL_BARS
  {
#ifdef USE_GTK
    if (horizontal)
      xg_update_horizontal_scrollbar_pos (f, bar->x_window, top,
					  left, width, max (height, 1));
    else
      xg_update_scrollbar_pos (f, bar->x_window, top,
			       left, width, max (height, 1));
#else /* not USE_GTK */
    Widget scroll_bar = SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar);
    XtConfigureWidget (scroll_bar, left, top, width, max (height, 1), 0);
    XtMapWidget (scroll_bar);
    /* Don't obscure any child frames.  */
    XLowerWindow (FRAME_X_DISPLAY (f), bar->x_window);
#endif /* not USE_GTK */
    }
#else /* not USE_TOOLKIT_SCROLL_BARS */
  XMapWindow (FRAME_X_DISPLAY (f), bar->x_window);
  /* Don't obscure any child frames.  */
  XLowerWindow (FRAME_X_DISPLAY (f), bar->x_window);
#endif /* not USE_TOOLKIT_SCROLL_BARS */

  unblock_input ();
  return bar;
}


#ifndef USE_TOOLKIT_SCROLL_BARS

/* Draw BAR's handle in the proper position.

   If the handle is already drawn from START to END, don't bother
   redrawing it, unless REBUILD; in that case, always
   redraw it.  (REBUILD is handy for drawing the handle after expose
   events.)

   Normally, we want to constrain the start and end of the handle to
   fit inside its rectangle, but if the user is dragging the scroll
   bar handle, we want to let them drag it down all the way, so that
   the bar's top is as far down as it goes; otherwise, there's no way
   to move to the very end of the buffer.  */

static void
x_scroll_bar_set_handle (struct scroll_bar *bar, int start, int end,
			 bool rebuild)
{
  bool dragging;
  struct frame *f;
  Drawable w;
  GC gc;
  int inside_width, inside_height, top_range, length;

  /* If the display is already accurate, do nothing.  */
  if (! rebuild
      && start == bar->start
      && end == bar->end)
    return;

  f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  dragging = bar->dragging != -1;
  gc = f->output_data.x->normal_gc;
  w = FRAME_X_DRAWABLE (f);

  block_input ();

  inside_width = VERTICAL_SCROLL_BAR_INSIDE_WIDTH (f, bar->width);
  inside_height = VERTICAL_SCROLL_BAR_INSIDE_HEIGHT (f, bar->height);
  top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, bar->height);

  /* Make sure the values are reasonable, and try to preserve
     the distance between start and end.  */
  length = end - start;

  if (start < 0)
    start = 0;
  else if (start > top_range)
    start = top_range;
  end = start + length;

  if (end < start)
    end = start;
  else if (end > top_range && ! dragging)
    end = top_range;

  /* Store the adjusted setting in the scroll bar.  */
  bar->start = start;
  bar->end = end;

  /* Clip the end position, just for display.  */
  if (end > top_range)
    end = top_range;

  /* Draw bottom positions VERTICAL_SCROLL_BAR_MIN_HANDLE pixels
     below top positions, to make sure the handle is always at least
     that many pixels tall.  */
  end += VERTICAL_SCROLL_BAR_MIN_HANDLE;

  /* Draw the empty space above the handle.  Note that we can't clear
     zero-height areas; that means "clear to end of window."  */
  if ((inside_width > 0) && (start > 0))
    {
      if (f->output_data.x->scroll_bar_background_pixel != -1)
	XSetForeground (FRAME_X_DISPLAY (f), gc,
			f->output_data.x->scroll_bar_background_pixel);
      else
	XSetForeground (FRAME_X_DISPLAY (f), gc,
			FRAME_BACKGROUND_PIXEL (f));

      XFillRectangle (FRAME_X_DISPLAY (f), w, gc,
		      bar->left + VERTICAL_SCROLL_BAR_LEFT_BORDER,
		      bar->top + VERTICAL_SCROLL_BAR_TOP_BORDER,
		      inside_width, start);

      XSetForeground (FRAME_X_DISPLAY (f), gc,
		      FRAME_FOREGROUND_PIXEL (f));
    }

  /* Change to proper foreground color if one is specified.  */
  if (f->output_data.x->scroll_bar_foreground_pixel != -1)
    XSetForeground (FRAME_X_DISPLAY (f), gc,
		    f->output_data.x->scroll_bar_foreground_pixel);

  /* Draw the handle itself.  */
  XFillRectangle (FRAME_X_DISPLAY (f), w, gc,
		  /* x, y, width, height */
		  bar->left + VERTICAL_SCROLL_BAR_LEFT_BORDER,
		  bar->top + VERTICAL_SCROLL_BAR_TOP_BORDER + start,
		  inside_width, end - start);


  /* Draw the empty space below the handle.  Note that we can't
     clear zero-height areas; that means "clear to end of window." */
  if ((inside_width > 0) && (end < inside_height))
    {
      if (f->output_data.x->scroll_bar_background_pixel != -1)
	XSetForeground (FRAME_X_DISPLAY (f), gc,
			f->output_data.x->scroll_bar_background_pixel);
      else
	XSetForeground (FRAME_X_DISPLAY (f), gc,
			FRAME_BACKGROUND_PIXEL (f));

      XFillRectangle (FRAME_X_DISPLAY (f), w, gc,
		      bar->left + VERTICAL_SCROLL_BAR_LEFT_BORDER,
		      bar->top + VERTICAL_SCROLL_BAR_TOP_BORDER + end,
		      inside_width, inside_height - end);

      XSetForeground (FRAME_X_DISPLAY (f), gc,
		      FRAME_FOREGROUND_PIXEL (f));
    }

  /* Restore the foreground color of the GC if we changed it above.  */
  if (f->output_data.x->scroll_bar_foreground_pixel != -1)
    XSetForeground (FRAME_X_DISPLAY (f), gc,
		    FRAME_FOREGROUND_PIXEL (f));

  unblock_input ();
}

#endif /* !USE_TOOLKIT_SCROLL_BARS */

/* Destroy scroll bar BAR, and set its Emacs window's scroll bar to
   nil.  */

static void
x_scroll_bar_remove (struct scroll_bar *bar)
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  block_input ();

#ifdef USE_TOOLKIT_SCROLL_BARS
#ifdef USE_GTK
  xg_remove_scroll_bar (f, bar->x_window);
#else /* not USE_GTK */
  XtDestroyWidget (SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar));
#endif /* not USE_GTK */
#else
  XDestroyWindow (FRAME_X_DISPLAY (f), bar->x_window);
#endif

  /* Dissociate this scroll bar from its window.  */
  if (bar->horizontal)
    wset_horizontal_scroll_bar (XWINDOW (bar->window), Qnil);
  else
    wset_vertical_scroll_bar (XWINDOW (bar->window), Qnil);

  unblock_input ();
}


/* Set the handle of the vertical scroll bar for WINDOW to indicate
   that we are displaying PORTION characters out of a total of WHOLE
   characters, starting at POSITION.  If WINDOW has no scroll bar,
   create one.  */

static void
XTset_vertical_scroll_bar (struct window *w, int portion, int whole, int position)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object barobj;
  struct scroll_bar *bar;
  int top, height, left, width;
  int window_y, window_height;

  /* Get window dimensions.  */
  window_box (w, ANY_AREA, 0, &window_y, 0, &window_height);
  top = window_y;
  height = window_height;
  left = WINDOW_SCROLL_BAR_AREA_X (w);
  width = WINDOW_SCROLL_BAR_AREA_WIDTH (w);

  /* Does the scroll bar exist yet?  */
  if (NILP (w->vertical_scroll_bar))
    {
      if (width > 0 && height > 0)
	{
	  block_input ();
          x_clear_area (f, left, top, width, height);
	  unblock_input ();
	}

      bar = x_scroll_bar_create (w, top, left, width, max (height, 1), false);
#ifndef USE_TOOLKIT_SCROLL_BARS
      /* Since non-toolkit scroll bars don't display their contents to
	 a dedicated window, no expose event will be generated.
	 Redraw the scroll bar manually.  */
      x_scroll_bar_redraw (bar);
#endif
    }
  else
    {
      /* It may just need to be moved and resized.  */
      unsigned int mask = 0;

      bar = XSCROLL_BAR (w->vertical_scroll_bar);

      block_input ();

      if (left != bar->left)
	mask |= CWX;
      if (top != bar->top)
	mask |= CWY;
      if (width != bar->width)
	mask |= CWWidth;
      if (height != bar->height)
	mask |= CWHeight;

#ifdef USE_TOOLKIT_SCROLL_BARS

      /* Move/size the scroll bar widget.  */
      if (mask)
	{
	  /* Since toolkit scroll bars are smaller than the space reserved
	     for them on the frame, we have to clear "under" them.  */
	  if (width > 0 && height > 0)
	    x_clear_area (f, left, top, width, height);
#ifdef USE_GTK
          xg_update_scrollbar_pos (f, bar->x_window, top,
				   left, width, max (height, 1));
#else /* not USE_GTK */
          XtConfigureWidget (SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar),
                             left, top, width, max (height, 1), 0);
#endif /* not USE_GTK */
	}
#else /* not USE_TOOLKIT_SCROLL_BARS */

      /* Move/size the scroll bar window.  */
      if (mask)
	{
	  XWindowChanges wc;

	  wc.x = left;
	  wc.y = top;
	  wc.width = width;
	  wc.height = height;
	  XConfigureWindow (FRAME_X_DISPLAY (f), bar->x_window,
			    mask, &wc);
	}

#endif /* not USE_TOOLKIT_SCROLL_BARS */

      /* Remember new settings.  */
      bar->left = left;
      bar->top = top;
      bar->width = width;
      bar->height = height;

#ifndef USE_TOOLKIT_SCROLL_BARS
      /* Redraw the scroll bar.  */
      x_scroll_bar_redraw (bar);
#endif

      unblock_input ();
    }

#ifdef USE_TOOLKIT_SCROLL_BARS
  x_set_toolkit_scroll_bar_thumb (bar, portion, position, whole);
#else /* not USE_TOOLKIT_SCROLL_BARS */
  /* Set the scroll bar's current state, unless we're currently being
     dragged.  */
  if (bar->dragging == -1)
    {
      int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, height);

      if (whole == 0)
	x_scroll_bar_set_handle (bar, 0, top_range, false);
      else
	{
	  int start = ((double) position * top_range) / whole;
	  int end = ((double) (position + portion) * top_range) / whole;
	  x_scroll_bar_set_handle (bar, start, end, false);
	}
    }
#endif /* not USE_TOOLKIT_SCROLL_BARS */

  XSETVECTOR (barobj, bar);
  wset_vertical_scroll_bar (w, barobj);
}


static void
XTset_horizontal_scroll_bar (struct window *w, int portion, int whole, int position)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object barobj;
  struct scroll_bar *bar;
  int top, height, left, width;
  int window_x, window_width;
  int pixel_width = WINDOW_PIXEL_WIDTH (w);

  /* Get window dimensions.  */
  window_box (w, ANY_AREA, &window_x, 0, &window_width, 0);
  left = window_x;
  width = window_width;
  top = WINDOW_SCROLL_BAR_AREA_Y (w);
  height = WINDOW_SCROLL_BAR_AREA_HEIGHT (w);

  /* Does the scroll bar exist yet?  */
  if (NILP (w->horizontal_scroll_bar))
    {
      if (width > 0 && height > 0)
	{
	  block_input ();

	  /* Clear also part between window_width and
	     WINDOW_PIXEL_WIDTH.  */
	  x_clear_area (f, left, top, pixel_width, height);
	  unblock_input ();
	}

      bar = x_scroll_bar_create (w, top, left, width, height, true);
    }
  else
    {
      /* It may just need to be moved and resized.  */
      unsigned int mask = 0;

      bar = XSCROLL_BAR (w->horizontal_scroll_bar);

      block_input ();

      if (left != bar->left)
	mask |= CWX;
      if (top != bar->top)
	mask |= CWY;
      if (width != bar->width)
	mask |= CWWidth;
      if (height != bar->height)
	mask |= CWHeight;

#ifdef USE_TOOLKIT_SCROLL_BARS
      /* Move/size the scroll bar widget.  */
      if (mask)
	{
	  /* Since toolkit scroll bars are smaller than the space reserved
	     for them on the frame, we have to clear "under" them.  */
	  if (width > 0 && height > 0)
	    x_clear_area (f,
			  WINDOW_LEFT_EDGE_X (w), top,
			  pixel_width - WINDOW_RIGHT_DIVIDER_WIDTH (w), height);
#ifdef USE_GTK
          xg_update_horizontal_scrollbar_pos (f, bar->x_window, top, left,
					      width, height);
#else /* not USE_GTK */
          XtConfigureWidget (SCROLL_BAR_X_WIDGET (FRAME_X_DISPLAY (f), bar),
                             left, top, width, height, 0);
#endif /* not USE_GTK */
	}
#else /* not USE_TOOLKIT_SCROLL_BARS */

      /* Clear areas not covered by the scroll bar because it's not as
	 wide as the area reserved for it.  This makes sure a
	 previous mode line display is cleared after C-x 2 C-x 1, for
	 example.  */
      {
	int area_height = WINDOW_CONFIG_SCROLL_BAR_HEIGHT (w);
	int rest = area_height - height;
	if (rest > 0 && width > 0)
	  x_clear_area (f, left, top, width, rest);
      }

      /* Move/size the scroll bar window.  */
      if (mask)
	{
	  XWindowChanges wc;

	  wc.x = left;
	  wc.y = top;
	  wc.width = width;
	  wc.height = height;
	  XConfigureWindow (FRAME_X_DISPLAY (f), bar->x_window,
			    mask, &wc);
	}

#endif /* not USE_TOOLKIT_SCROLL_BARS */

      /* Remember new settings.  */
      bar->left = left;
      bar->top = top;
      bar->width = width;
      bar->height = height;

      unblock_input ();
    }

#ifdef USE_TOOLKIT_SCROLL_BARS
  x_set_toolkit_horizontal_scroll_bar_thumb (bar, portion, position, whole);
#else /* not USE_TOOLKIT_SCROLL_BARS */
  /* Set the scroll bar's current state, unless we're currently being
     dragged.  */
  if (bar->dragging == -1)
    {
      int left_range = HORIZONTAL_SCROLL_BAR_LEFT_RANGE (f, width);

      if (whole == 0)
	x_scroll_bar_set_handle (bar, 0, left_range, false);
      else
	{
	  int start = ((double) position * left_range) / whole;
	  int end = ((double) (position + portion) * left_range) / whole;
	  x_scroll_bar_set_handle (bar, start, end, false);
	}
    }
#endif /* not USE_TOOLKIT_SCROLL_BARS */

  XSETVECTOR (barobj, bar);
  wset_horizontal_scroll_bar (w, barobj);
}


/* The following three hooks are used when we're doing a thorough
   redisplay of the frame.  We don't explicitly know which scroll bars
   are going to be deleted, because keeping track of when windows go
   away is a real pain - "Can you say set-window-configuration, boys
   and girls?"  Instead, we just assert at the beginning of redisplay
   that *all* scroll bars are to be removed, and then save a scroll bar
   from the fiery pit when we actually redisplay its window.  */

/* Arrange for all scroll bars on FRAME to be removed at the next call
   to `*judge_scroll_bars_hook'.  A scroll bar may be spared if
   `*redeem_scroll_bar_hook' is applied to its window before the judgment.  */

static void
XTcondemn_scroll_bars (struct frame *frame)
{
  if (!NILP (FRAME_SCROLL_BARS (frame)))
    {
      if (!NILP (FRAME_CONDEMNED_SCROLL_BARS (frame)))
	{
	  /* Prepend scrollbars to already condemned ones.  */
	  Lisp_Object last = FRAME_SCROLL_BARS (frame);

	  while (!NILP (XSCROLL_BAR (last)->next))
	    last = XSCROLL_BAR (last)->next;

	  XSCROLL_BAR (last)->next = FRAME_CONDEMNED_SCROLL_BARS (frame);
	  XSCROLL_BAR (FRAME_CONDEMNED_SCROLL_BARS (frame))->prev = last;
	}

      fset_condemned_scroll_bars (frame, FRAME_SCROLL_BARS (frame));
      fset_scroll_bars (frame, Qnil);
    }
}


/* Un-mark WINDOW's scroll bar for deletion in this judgment cycle.
   Note that WINDOW isn't necessarily condemned at all.  */

static void
XTredeem_scroll_bar (struct window *w)
{
  struct scroll_bar *bar;
  Lisp_Object barobj;
  struct frame *f;

  /* We can't redeem this window's scroll bar if it doesn't have one.  */
  if (NILP (w->vertical_scroll_bar) && NILP (w->horizontal_scroll_bar))
    emacs_abort ();

  if (!NILP (w->vertical_scroll_bar) && WINDOW_HAS_VERTICAL_SCROLL_BAR (w))
    {
      bar = XSCROLL_BAR (w->vertical_scroll_bar);
      /* Unlink it from the condemned list.  */
      f = XFRAME (WINDOW_FRAME (w));
      if (NILP (bar->prev))
	{
	  /* If the prev pointer is nil, it must be the first in one of
	     the lists.  */
	  if (EQ (FRAME_SCROLL_BARS (f), w->vertical_scroll_bar))
	    /* It's not condemned.  Everything's fine.  */
	    goto horizontal;
	  else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
		       w->vertical_scroll_bar))
	    fset_condemned_scroll_bars (f, bar->next);
	  else
	    /* If its prev pointer is nil, it must be at the front of
	       one or the other!  */
	    emacs_abort ();
	}
      else
	XSCROLL_BAR (bar->prev)->next = bar->next;

      if (! NILP (bar->next))
	XSCROLL_BAR (bar->next)->prev = bar->prev;

      bar->next = FRAME_SCROLL_BARS (f);
      bar->prev = Qnil;
      XSETVECTOR (barobj, bar);
      fset_scroll_bars (f, barobj);
      if (! NILP (bar->next))
	XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
    }

 horizontal:
  if (!NILP (w->horizontal_scroll_bar) && WINDOW_HAS_HORIZONTAL_SCROLL_BAR (w))
    {
      bar = XSCROLL_BAR (w->horizontal_scroll_bar);
      /* Unlink it from the condemned list.  */
      f = XFRAME (WINDOW_FRAME (w));
      if (NILP (bar->prev))
	{
	  /* If the prev pointer is nil, it must be the first in one of
	     the lists.  */
	  if (EQ (FRAME_SCROLL_BARS (f), w->horizontal_scroll_bar))
	    /* It's not condemned.  Everything's fine.  */
	    return;
	  else if (EQ (FRAME_CONDEMNED_SCROLL_BARS (f),
		       w->horizontal_scroll_bar))
	    fset_condemned_scroll_bars (f, bar->next);
	  else
	    /* If its prev pointer is nil, it must be at the front of
	       one or the other!  */
	    emacs_abort ();
	}
      else
	XSCROLL_BAR (bar->prev)->next = bar->next;

      if (! NILP (bar->next))
	XSCROLL_BAR (bar->next)->prev = bar->prev;

      bar->next = FRAME_SCROLL_BARS (f);
      bar->prev = Qnil;
      XSETVECTOR (barobj, bar);
      fset_scroll_bars (f, barobj);
      if (! NILP (bar->next))
	XSETVECTOR (XSCROLL_BAR (bar->next)->prev, bar);
    }
}

/* Remove all scroll bars on FRAME that haven't been saved since the
   last call to `*condemn_scroll_bars_hook'.  */

static void
XTjudge_scroll_bars (struct frame *f)
{
  Lisp_Object bar, next;

  bar = FRAME_CONDEMNED_SCROLL_BARS (f);

  /* Clear out the condemned list now so we won't try to process any
     more events on the hapless scroll bars.  */
  fset_condemned_scroll_bars (f, Qnil);

  for (; ! NILP (bar); bar = next)
    {
      struct scroll_bar *b = XSCROLL_BAR (bar);

      x_scroll_bar_remove (b);

      next = b->next;
      b->next = b->prev = Qnil;
    }

  /* Now there should be no references to the condemned scroll bars,
     and they should get garbage-collected.  */
}


#ifndef USE_TOOLKIT_SCROLL_BARS
/* Handle exposure event EVENT generated for F, by redrawing all
   intersecting scroll bars.  */

static void
x_scroll_bar_handle_exposure (struct frame *f, XEvent *event)
{
  int x, y, width, height;
  XRectangle rect, scroll_bar_rect, intersection;
  Lisp_Object bar, condemned;
  struct scroll_bar *b;

  if (event->type == GraphicsExpose)
    {
      x = event->xgraphicsexpose.x;
      y = event->xgraphicsexpose.y;
      width = event->xgraphicsexpose.width;
      height = event->xgraphicsexpose.height;
    }
  else
    {
      x = event->xexpose.x;
      y = event->xexpose.y;
      width = event->xexpose.width;
      height = event->xexpose.height;
    }

  rect.x = x;
  rect.y = y;
  rect.width = width;
  rect.height = height;

  /* Scan this frame's scroll bar list for intersecting scroll
     bars.  */
  condemned = FRAME_CONDEMNED_SCROLL_BARS (f);
  for (bar = FRAME_SCROLL_BARS (f);
       /* This trick allows us to search both the ordinary and
	  condemned scroll bar lists with one loop.  */
       !NILP (bar) || (bar = condemned,
		       condemned = Qnil,
		       !NILP (bar));
       bar = XSCROLL_BAR (bar)->next)
    {
      b = XSCROLL_BAR (bar);

      scroll_bar_rect.x = b->left;
      scroll_bar_rect.y = b->top;
      scroll_bar_rect.width = b->width;
      scroll_bar_rect.height = b->height;

      if (gui_intersect_rectangles (&rect,
				    &scroll_bar_rect,
				    &intersection))
	x_scroll_bar_redraw (b);
    }
}

/* Redraw the scroll bar BAR.  Draw its border and set its thumb.
   This is usually called from x_clear_frame, but is also used to
   handle exposure events that overlap scroll bars.  */

static void
x_scroll_bar_redraw (struct scroll_bar *bar)
{
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  GC gc = f->output_data.x->normal_gc;

  if (f->output_data.x->scroll_bar_background_pixel != -1)
    XSetForeground (FRAME_X_DISPLAY (f), gc,
		    f->output_data.x->scroll_bar_background_pixel);
  else
    XSetForeground (FRAME_X_DISPLAY (f), gc,
		    FRAME_BACKGROUND_PIXEL (f));

  XFillRectangle (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f), gc,
		  bar->left, bar->top, bar->width, bar->height);

  XSetForeground (FRAME_X_DISPLAY (f), gc,
		  FRAME_FOREGROUND_PIXEL (f));

  x_scroll_bar_set_handle (bar, bar->start, bar->end, true);

  /* Switch to scroll bar foreground color.  */
  if (f->output_data.x->scroll_bar_foreground_pixel != -1)
    XSetForeground (FRAME_X_DISPLAY (f), gc,
		    f->output_data.x->scroll_bar_foreground_pixel);

  /* Draw a one-pixel border just inside the edges of the scroll bar.  */
  XDrawRectangle (FRAME_X_DISPLAY (f), FRAME_X_DRAWABLE (f), gc,
		  bar->left, bar->top, bar->width - 1, bar->height - 1);

  /* Restore the foreground color of the GC if we changed it above.  */
  if (f->output_data.x->scroll_bar_foreground_pixel != -1)
    XSetForeground (FRAME_X_DISPLAY (f), gc,
		    FRAME_FOREGROUND_PIXEL (f));
}
#endif /* not USE_TOOLKIT_SCROLL_BARS */

/* Handle a mouse click on the scroll bar BAR.  If *EMACS_EVENT's kind
   is set to something other than NO_EVENT, it is enqueued.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */


static void
x_scroll_bar_handle_click (struct scroll_bar *bar,
			   const XEvent *event,
			   struct input_event *emacs_event,
			   Lisp_Object device)
{
  int left_range, x, top_range, y;
#ifndef USE_TOOLKIT_SCROLL_BARS
  int new_start, new_end;
#endif

  if (! WINDOWP (bar->window))
    emacs_abort ();

  emacs_event->kind = (bar->horizontal
		       ? HORIZONTAL_SCROLL_BAR_CLICK_EVENT
		       : SCROLL_BAR_CLICK_EVENT);
  emacs_event->code = event->xbutton.button - Button1;
  emacs_event->modifiers
    = (x_x_to_emacs_modifiers (FRAME_DISPLAY_INFO
			       (XFRAME (WINDOW_FRAME (XWINDOW (bar->window)))),
			       event->xbutton.state)
       | (event->type == ButtonRelease
	  ? up_modifier
	  : down_modifier));
  emacs_event->frame_or_window = bar->window;
  emacs_event->arg = Qnil;
  emacs_event->timestamp = event->xbutton.time;

  if (!NILP (device))
    emacs_event->device = device;

  if (bar->horizontal)
    {

      left_range = HORIZONTAL_SCROLL_BAR_LEFT_RANGE (f, bar->width);
      x = event->xbutton.x - HORIZONTAL_SCROLL_BAR_LEFT_BORDER;

      if (x < 0) x = 0;
      if (x > left_range) x = left_range;

      if (x < bar->start)
	emacs_event->part = scroll_bar_before_handle;
      else if (x < bar->end + HORIZONTAL_SCROLL_BAR_MIN_HANDLE)
	emacs_event->part = scroll_bar_horizontal_handle;
      else
	emacs_event->part = scroll_bar_after_handle;

#ifndef USE_TOOLKIT_SCROLL_BARS
      /* If the user has released the handle, set it to its final position.  */
      if (event->type == ButtonRelease && bar->dragging != -1)
	{
	  new_start = - bar->dragging;
	  new_end = new_start + bar->end - bar->start;

	  x_scroll_bar_set_handle (bar, new_start, new_end, false);
	  bar->dragging = -1;
	}
#endif

      XSETINT (emacs_event->x, left_range);
      XSETINT (emacs_event->y, x);
    }
  else
    {
      top_range
	= VERTICAL_SCROLL_BAR_TOP_RANGE (f, bar->height);
      y = event->xbutton.y - VERTICAL_SCROLL_BAR_TOP_BORDER;

      if (y < 0) y = 0;
      if (y > top_range) y = top_range;

      if (y < bar->start)
	emacs_event->part = scroll_bar_above_handle;
      else if (y < bar->end + VERTICAL_SCROLL_BAR_MIN_HANDLE)
	emacs_event->part = scroll_bar_handle;
      else
	emacs_event->part = scroll_bar_below_handle;

#ifndef USE_TOOLKIT_SCROLL_BARS
      /* If the user has released the handle, set it to its final position.  */
      if (event->type == ButtonRelease && bar->dragging != -1)
	{
	  new_start = y - bar->dragging;
	  new_end = new_start + bar->end - bar->start;

	  x_scroll_bar_set_handle (bar, new_start, new_end, false);
	  bar->dragging = -1;
	}
#endif

      XSETINT (emacs_event->x, y);
      XSETINT (emacs_event->y, top_range);
    }
}

#ifndef USE_TOOLKIT_SCROLL_BARS

/* Handle some mouse motion while someone is dragging the scroll bar.

   This may be called from a signal handler, so we have to ignore GC
   mark bits.  */

static void
x_scroll_bar_note_movement (struct scroll_bar *bar,
			    const XMotionEvent *event)
{
  struct frame *f = XFRAME (XWINDOW (bar->window)->frame);
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  dpyinfo->last_mouse_movement_time = event->time;
  dpyinfo->last_mouse_movement_time_send_event = event->send_event;
  dpyinfo->last_mouse_scroll_bar = bar;
  f->mouse_moved = true;

  /* If we're dragging the bar, display it.  */
  if (bar->dragging != -1)
    {
      /* Where should the handle be now?  */
      int new_start = event->y - bar->dragging;

      if (new_start != bar->start)
	{
	  int new_end = new_start + bar->end - bar->start;

	  x_scroll_bar_set_handle (bar, new_start, new_end, false);
	}
    }
}

#endif /* !USE_TOOLKIT_SCROLL_BARS */

/* Return information to the user about the current position of the mouse
   on the scroll bar.  */

static void
x_scroll_bar_report_motion (struct frame **fp, Lisp_Object *bar_window,
			    enum scroll_bar_part *part, Lisp_Object *x,
			    Lisp_Object *y, Time *timestamp)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (*fp);
  struct scroll_bar *bar = dpyinfo->last_mouse_scroll_bar;
  Window w = bar->x_window;
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  int win_x, win_y;
  Window dummy_window;
  int dummy_coord;
  unsigned int dummy_mask;

  block_input ();

  /* Get the mouse's position relative to the scroll bar window, and
     report that.  */
  if (x_query_pointer (FRAME_X_DISPLAY (f), w,

		       /* Root, child, root x and root y.  */
		       &dummy_window, &dummy_window,
		       &dummy_coord, &dummy_coord,

		       /* Position relative to scroll bar.  */
		       &win_x, &win_y,

		       /* Mouse buttons and modifier keys.  */
		       &dummy_mask))
    {
      int top_range = VERTICAL_SCROLL_BAR_TOP_RANGE (f, bar->height);

      win_y -= VERTICAL_SCROLL_BAR_TOP_BORDER;

      if (bar->dragging != -1)
	win_y -= bar->dragging;

      if (win_y < 0)
	win_y = 0;
      if (win_y > top_range)
	win_y = top_range;

      *fp = f;
      *bar_window = bar->window;

      if (bar->dragging != -1)
	*part = scroll_bar_handle;
      else if (win_y < bar->start)
	*part = scroll_bar_above_handle;
      else if (win_y < bar->end + VERTICAL_SCROLL_BAR_MIN_HANDLE)
	*part = scroll_bar_handle;
      else
	*part = scroll_bar_below_handle;

      XSETINT (*x, win_y);
      XSETINT (*y, top_range);

      f->mouse_moved = false;
      dpyinfo->last_mouse_scroll_bar = NULL;
      *timestamp = dpyinfo->last_mouse_movement_time;
    }

  unblock_input ();
}


/* Return information to the user about the current position of the mouse
   on the scroll bar.  */

static void
x_horizontal_scroll_bar_report_motion (struct frame **fp, Lisp_Object *bar_window,
				       enum scroll_bar_part *part, Lisp_Object *x,
				       Lisp_Object *y, Time *timestamp)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (*fp);
  struct scroll_bar *bar = dpyinfo->last_mouse_scroll_bar;
  Window w = bar->x_window;
  struct frame *f = XFRAME (WINDOW_FRAME (XWINDOW (bar->window)));
  int win_x, win_y;
  Window dummy_window;
  int dummy_coord;
  unsigned int dummy_mask;

  block_input ();

  /* Get the mouse's position relative to the scroll bar window, and
     report that.  */
  if (x_query_pointer (FRAME_X_DISPLAY (f), w,

		       /* Root, child, root x and root y.  */
		       &dummy_window, &dummy_window,
		       &dummy_coord, &dummy_coord,

		       /* Position relative to scroll bar.  */
		       &win_x, &win_y,

		       /* Mouse buttons and modifier keys.  */
		       &dummy_mask))
    {
      int left_range = HORIZONTAL_SCROLL_BAR_LEFT_RANGE (f, bar->width);

      win_x -= HORIZONTAL_SCROLL_BAR_LEFT_BORDER;

      if (bar->dragging != -1)
	win_x -= bar->dragging;

      if (win_x < 0)
	win_x = 0;
      if (win_x > left_range)
	win_x = left_range;

      *fp = f;
      *bar_window = bar->window;

      if (bar->dragging != -1)
	*part = scroll_bar_horizontal_handle;
      else if (win_x < bar->start)
	*part = scroll_bar_before_handle;
      else if (win_x < bar->end + HORIZONTAL_SCROLL_BAR_MIN_HANDLE)
	*part = scroll_bar_handle;
      else
	*part = scroll_bar_after_handle;

      XSETINT (*y, win_x);
      XSETINT (*x, left_range);

      f->mouse_moved = false;
      dpyinfo->last_mouse_scroll_bar = NULL;
      *timestamp = dpyinfo->last_mouse_movement_time;
    }

  unblock_input ();
}


/* The screen has been cleared and foreground or background colors may
   have changed, so the scroll bars need to be redrawn.  Clear the
   scroll bars and redraw them.  */

static void
x_scroll_bar_clear (struct frame *f)
{
#ifndef USE_TOOLKIT_SCROLL_BARS
  Lisp_Object bar, condemned;
  GC gc = f->output_data.x->normal_gc;

  if (f->output_data.x->scroll_bar_background_pixel != -1)
    XSetForeground (FRAME_X_DISPLAY (f), gc,
		    f->output_data.x->scroll_bar_background_pixel);
  else
    XSetForeground (FRAME_X_DISPLAY (f), gc,
		    FRAME_BACKGROUND_PIXEL (f));

  /* We can have scroll bars even if this is 0,
     if we just turned off scroll bar mode.
     But in that case we should not clear them.  */
  if (FRAME_HAS_VERTICAL_SCROLL_BARS (f))
    {
      condemned = FRAME_CONDEMNED_SCROLL_BARS (f);
      for (bar = FRAME_SCROLL_BARS (f);
	   /* This trick allows us to search both the ordinary and
              condemned scroll bar lists with one loop.  */
	   !NILP (bar) || (bar = condemned,
			   condemned = Qnil,
			   !NILP (bar));
	   bar = XSCROLL_BAR (bar)->next)
	x_scroll_bar_redraw (XSCROLL_BAR (bar));
    }

  XSetForeground (FRAME_X_DISPLAY (f), gc,
		  FRAME_FOREGROUND_PIXEL (f));
#endif /* not USE_TOOLKIT_SCROLL_BARS */
}

#ifdef ENABLE_CHECKING

/* Record the last 100 characters stored
   to help debug the loss-of-chars-during-GC problem.  */

static int temp_index;
static short temp_buffer[100];

#define STORE_KEYSYM_FOR_DEBUG(keysym)				\
  if (temp_index == ARRAYELTS (temp_buffer))			\
    temp_index = 0;						\
  temp_buffer[temp_index++] = (keysym)

#else /* not ENABLE_CHECKING */

#define STORE_KEYSYM_FOR_DEBUG(keysym) ((void)0)

#endif /* ENABLE_CHECKING */

/* Set this to nonzero to fake an "X I/O error"
   on a particular display.  */

static struct x_display_info *XTread_socket_fake_io_error;

/* When we find no input here, we occasionally do a no-op command
   to verify that the X server is still running and we can still talk with it.
   We try all the open displays, one by one.
   This variable is used for cycling thru the displays.  */

static struct x_display_info *next_noop_dpyinfo;

/* Filter events for the current X input method.
   DPYINFO is the display this event is for.
   EVENT is the X event to filter.

   Returns non-zero if the event was filtered, caller shall not process
   this event further.
   Returns zero if event is wasn't filtered.  */

#ifdef HAVE_X_I18N
static int
x_filter_event (struct x_display_info *dpyinfo, XEvent *event)
{
  /* XFilterEvent returns non-zero if the input method has
   consumed the event.  We pass the frame's X window to
   XFilterEvent because that's the one for which the IC
   was created.  */

  struct frame *f1;

#if defined HAVE_XINPUT2 && defined USE_GTK
  bool xinput_event = false;
  if (dpyinfo->supports_xi2
      && event->type == GenericEvent
      && (event->xgeneric.extension
	  == dpyinfo->xi2_opcode)
      && ((event->xgeneric.evtype
	   == XI_KeyPress)
	  || (event->xgeneric.evtype
	      == XI_KeyRelease)))
    {
      f1 = x_any_window_to_frame (dpyinfo,
				  ((XIDeviceEvent *)
				   event->xcookie.data)->event);
      xinput_event = true;
    }
  else
#endif
    f1 = x_any_window_to_frame (dpyinfo,
				event->xclient.window);

#ifdef USE_GTK
  if (!x_gtk_use_native_input
      && !dpyinfo->prefer_native_input)
    {
#endif
      return XFilterEvent (event, f1 ? FRAME_X_WINDOW (f1) : None);
#ifdef USE_GTK
    }
  else if (f1 && (event->type == KeyPress
		  || event->type == KeyRelease
#ifdef HAVE_XINPUT2
		  || xinput_event
#endif
		  ))
    {
      bool result;

      block_input ();
      result = xg_filter_key (f1, event);
      unblock_input ();

      /* Clear `xg_pending_quit_event' so we don't end up reacting to quit
	 events sent outside the main event loop (i.e. those sent from
	 inside a popup menu event loop).  */

      if (popup_activated ())
	xg_pending_quit_event.kind = NO_EVENT;

      if (result && f1)
	/* There will probably be a GDK event generated soon, so
	   exercise the wire to make pselect return.  */
	XNoOp (FRAME_X_DISPLAY (f1));

      return result;
    }

  return 0;
#endif
}
#endif

#ifdef USE_GTK
/* This is the filter function invoked by the GTK event loop.
   It is invoked before the XEvent is translated to a GdkEvent,
   so we have a chance to act on the event before GTK.  */
static GdkFilterReturn
event_handler_gdk (GdkXEvent *gxev, GdkEvent *ev, gpointer data)
{
  XEvent *xev = (XEvent *) gxev;

  block_input ();
  if (current_count >= 0)
    {
      struct x_display_info *dpyinfo;

      dpyinfo = x_display_info_for_display (xev->xany.display);

#ifdef HAVE_X_I18N
      /* Filter events for the current X input method.
         GTK calls XFilterEvent but not for key press and release,
         so we do it here.  */
      if ((xev->type == KeyPress || xev->type == KeyRelease)
	  && dpyinfo
	  && x_filter_event (dpyinfo, xev))
	{
	  unblock_input ();
	  return GDK_FILTER_REMOVE;
	}
#elif USE_GTK
      if (dpyinfo && (dpyinfo->prefer_native_input
		      || x_gtk_use_native_input)
	  && (xev->type == KeyPress
#ifdef HAVE_XINPUT2
	      /* GTK claims cookies for us, so we don't have to claim
		 them here.  */
	      || (dpyinfo->supports_xi2
		  && xev->type == GenericEvent
		  && (xev->xgeneric.extension
		      == dpyinfo->xi2_opcode)
		  && ((xev->xgeneric.evtype
		       == XI_KeyPress)
		      || (xev->xgeneric.evtype
			  == XI_KeyRelease)))
#endif
	      ))
	{
	  struct frame *f;

#ifdef HAVE_XINPUT2
	  if (xev->type == GenericEvent)
	    f = x_any_window_to_frame (dpyinfo,
				       ((XIDeviceEvent *) xev->xcookie.data)->event);
	  else
#endif
	    f = x_any_window_to_frame (dpyinfo, xev->xany.window);

	  if (f && xg_filter_key (f, xev))
	    {
	      unblock_input ();
	      return GDK_FILTER_REMOVE;
	    }
	}
#endif

      if (! dpyinfo)
        current_finish = X_EVENT_NORMAL;
      else
	current_count
	  += handle_one_xevent (dpyinfo, xev, &current_finish,
				current_hold_quit);
    }
  else
    current_finish = x_dispatch_event (xev, xev->xany.display);

  unblock_input ();

  if (current_finish == X_EVENT_GOTO_OUT || current_finish == X_EVENT_DROP)
    return GDK_FILTER_REMOVE;

  return GDK_FILTER_CONTINUE;
}
#endif /* USE_GTK */


static void xembed_send_message (struct frame *f, Time,
                                 enum xembed_message,
                                 long detail, long data1, long data2);

static void
x_net_wm_state (struct frame *f, Window window)
{
  int value = FULLSCREEN_NONE;
  Lisp_Object lval = Qnil;
  bool sticky = false, shaded = false;

  x_get_current_wm_state (f, window, &value, &sticky, &shaded);

  switch (value)
    {
    case FULLSCREEN_WIDTH:
      lval = Qfullwidth;
      break;
    case FULLSCREEN_HEIGHT:
      lval = Qfullheight;
      break;
    case FULLSCREEN_BOTH:
      lval = Qfullboth;
      break;
    case FULLSCREEN_MAXIMIZED:
      lval = Qmaximized;
      break;
    }

  store_frame_param (f, Qfullscreen, lval);
  store_frame_param (f, Qsticky, sticky ? Qt : Qnil);
  store_frame_param (f, Qshaded, shaded ? Qt : Qnil);
}

/* Flip back buffers on F if it has undrawn content.  */

static void
x_flush_dirty_back_buffer_on (struct frame *f)
{
#ifdef HAVE_XDBE
  if (FRAME_GARBAGED_P (f)
      || buffer_flipping_blocked_p ()
      /* If the frame is not already up to date, do not flush buffers
	 on input, as that will result in flicker.  */
      || !FRAME_X_COMPLETE_P (f)
      || !FRAME_X_NEED_BUFFER_FLIP (f))
    return;

  show_back_buffer (f);
#endif
}

#ifdef HAVE_GTK3
void
x_scroll_bar_configure (GdkEvent *event)
{
  XEvent configure;
  GdkDisplay *gdpy;
  Display *dpy;

  configure.xconfigure.type = ConfigureNotify;
  configure.xconfigure.serial = 0;
  configure.xconfigure.send_event = event->configure.send_event;
  configure.xconfigure.x = event->configure.x;
  configure.xconfigure.y = event->configure.y;
  configure.xconfigure.width = event->configure.width;
  configure.xconfigure.height = event->configure.height;
  configure.xconfigure.border_width = 0;
  configure.xconfigure.event = GDK_WINDOW_XID (event->configure.window);
  configure.xconfigure.window = GDK_WINDOW_XID (event->configure.window);
  configure.xconfigure.above = None;
  configure.xconfigure.override_redirect = False;

  gdpy = gdk_window_get_display (event->configure.window);
  dpy = gdk_x11_display_get_xdisplay (gdpy);

  x_dispatch_event (&configure, dpy);
}
#endif

/**
  mouse_or_wdesc_frame: When not dropping and the mouse was grabbed
  for DPYINFO, return the frame where the mouse was seen last.  If
  there's no such frame, return the frame according to WDESC.  When
  dropping, return the frame according to WDESC.  If there's no such
  frame and the mouse was grabbed for DPYINFO, return the frame where
  the mouse was seen last.  In either case, never return a tooltip
  frame.  */
static struct frame *
mouse_or_wdesc_frame (struct x_display_info *dpyinfo, int wdesc)
{
  struct frame *lm_f = (gui_mouse_grabbed (dpyinfo)
			? dpyinfo->last_mouse_frame
			: NULL);

  if (lm_f && !EQ (track_mouse, Qdropping)
      && !EQ (track_mouse, Qdrag_source))
    return lm_f;
  else
    {
      struct frame *w_f = x_window_to_frame (dpyinfo, wdesc);

      /* Do not return a tooltip frame.  */
      if (!w_f || FRAME_TOOLTIP_P (w_f))
	return EQ (track_mouse, Qdropping) ? lm_f : NULL;
      else
	/* When dropping it would be probably nice to raise w_f
	   here.  */
	return w_f;
    }
}

static void
x_dnd_compute_tip_xy (int *root_x, int *root_y, Lisp_Object attributes)
{
  Lisp_Object monitor, geometry;
  int min_x, min_y, max_x, max_y;
  int width, height;

  width = FRAME_PIXEL_WIDTH (XFRAME (tip_frame));
  height = FRAME_PIXEL_HEIGHT (XFRAME (tip_frame));

  max_y = -1;

  /* Try to determine the monitor where the mouse pointer is and
     its geometry.  See bug#22549.  */
  while (CONSP (attributes))
    {
      monitor = XCAR (attributes);
      geometry = assq_no_quit (Qgeometry, monitor);

      if (CONSP (geometry))
	{
	  min_x = XFIXNUM (Fnth (make_fixnum (1), geometry));
	  min_y = XFIXNUM (Fnth (make_fixnum (2), geometry));
	  max_x = min_x + XFIXNUM (Fnth (make_fixnum (3), geometry));
	  max_y = min_y + XFIXNUM (Fnth (make_fixnum (4), geometry));

	  if (min_x <= *root_x && *root_x < max_x
	      && min_y <= *root_y && *root_y < max_y)
	    break;

	  max_y = -1;
	}

      attributes = XCDR (attributes);
    }

  /* It was not possible to determine the monitor's geometry, so we
     assign some sane defaults here: */
  if (max_y < 0)
    {
      min_x = 0;
      min_y = 0;
      max_x = x_display_pixel_width (FRAME_DISPLAY_INFO (x_dnd_frame));
      max_y = x_display_pixel_height (FRAME_DISPLAY_INFO (x_dnd_frame));
    }

  if (*root_y + XFIXNUM (tip_dy) <= min_y)
    *root_y = min_y; /* Can happen for negative dy */
  else if (*root_y + XFIXNUM (tip_dy) + height <= max_y)
    /* It fits below the pointer */
    *root_y += XFIXNUM (tip_dy);
  else if (height + XFIXNUM (tip_dy) + min_y <= *root_y)
    /* It fits above the pointer.  */
    *root_y -= height + XFIXNUM (tip_dy);
  else
    /* Put it on the top.  */
    *root_y = min_y;

  if (*root_x + XFIXNUM (tip_dx) <= min_x)
    *root_x = 0; /* Can happen for negative dx */
  else if (*root_x + XFIXNUM (tip_dx) + width <= max_x)
    /* It fits to the right of the pointer.  */
    *root_x += XFIXNUM (tip_dx);
  else if (width + XFIXNUM (tip_dx) + min_x <= *root_x)
    /* It fits to the left of the pointer.  */
    *root_x -= width + XFIXNUM (tip_dx);
  else
    /* Put it left justified on the screen -- it ought to fit that way.  */
    *root_x = min_x;
}

static void
x_dnd_update_tooltip_position (int root_x, int root_y)
{
  struct frame *tip_f;

  if (!x_dnd_in_progress || !x_dnd_update_tooltip)
    return;

  if (!FRAMEP (tip_frame))
    return;

  tip_f = XFRAME (tip_frame);

  if (!FRAME_LIVE_P (tip_f)
      || !FRAME_VISIBLE_P (tip_f)
      || (FRAME_X_DISPLAY (tip_f)
	  != FRAME_X_DISPLAY (x_dnd_frame)))
    return;

  if (tip_window != None
      && FIXNUMP (tip_dx) && FIXNUMP (tip_dy))
    {
      x_dnd_compute_tip_xy (&root_x, &root_y,
			    x_dnd_monitors);

      if (x_dnd_last_tooltip_valid
	  && root_x == x_dnd_last_tooltip_x
	  && root_y == x_dnd_last_tooltip_y)
	return;

      x_dnd_last_tooltip_x = root_x;
      x_dnd_last_tooltip_y = root_y;
      x_dnd_last_tooltip_valid = true;

      XMoveWindow (FRAME_X_DISPLAY (x_dnd_frame),
		   tip_window, root_x, root_y);
    }
}

static void
x_dnd_update_tooltip_now (void)
{
  int root_x, root_y;
  Window root, child;
  int win_x, win_y;
  unsigned int mask;
  Bool rc;
  struct x_display_info *dpyinfo;

  if (!x_dnd_in_progress || !x_dnd_update_tooltip)
    return;

  dpyinfo = FRAME_DISPLAY_INFO (x_dnd_frame);

#ifndef HAVE_XINPUT2
  rc = XQueryPointer (dpyinfo->display,
		      dpyinfo->root_window,
		      &root, &child, &root_x,
		      &root_y, &win_x, &win_y,
		      &mask);
#else
  rc = x_query_pointer_1 (dpyinfo, x_dnd_pointer_device,
			  dpyinfo->root_window,
			  &root, &child, &root_x,
			  &root_y, &win_x, &win_y,
			  &mask);
#endif

  if (rc)
    x_dnd_update_tooltip_position (root_x, root_y);
}

/* Get the window underneath the pointer, see if it moved, and update
   the DND state accordingly.  */
static void
x_dnd_update_state (struct x_display_info *dpyinfo, Time timestamp)
{
  int root_x, root_y, dummy_x, dummy_y, target_proto, motif_style;
  unsigned int dummy_mask;
  Window dummy, dummy_child, target, toplevel;
  xm_top_level_leave_message lmsg;
  xm_top_level_enter_message emsg;
  xm_drag_motion_message dmsg;
  xm_drop_start_message dsmsg;
  bool was_frame;

  if (x_query_pointer_1 (dpyinfo,
#ifdef HAVE_XINPUT2
			 x_dnd_pointer_device,
#else
			 -1,
#endif
			 dpyinfo->root_window,
			 &dummy, &dummy_child,
			 &root_x, &root_y,
			 &dummy_x, &dummy_y,
			 &dummy_mask))
    {
      target = x_dnd_get_target_window (dpyinfo, root_x,
					root_y, &target_proto,
					&motif_style, &toplevel,
					&was_frame);

      if (toplevel != x_dnd_last_seen_toplevel)
	{
	  if (toplevel != FRAME_OUTER_WINDOW (x_dnd_frame)
	      && x_dnd_return_frame == 1)
	    x_dnd_return_frame = 2;

	  if (x_dnd_return_frame == 2
	      && x_any_window_to_frame (dpyinfo, toplevel))
	    {
	      if (x_dnd_last_seen_window != None
		  && x_dnd_last_protocol_version != -1
		  && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
		x_dnd_send_leave (x_dnd_frame, x_dnd_last_seen_window,
				  x_dnd_last_seen_toplevel);
	      else if (x_dnd_last_seen_window != None
		       && XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style)
		       && !x_dnd_disable_motif_drag
		       && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
		{
		  if (!x_dnd_motif_setup_p)
		    xm_setup_drag_info (dpyinfo, x_dnd_frame);

		  lmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
						XM_DRAG_REASON_TOP_LEVEL_LEAVE);
		  lmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
		  lmsg.zero = 0;
		  lmsg.timestamp = timestamp;
		  lmsg.source_window = FRAME_X_WINDOW (x_dnd_frame);

		  if (x_dnd_motif_setup_p)
		    xm_send_top_level_leave_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
						     x_dnd_last_seen_window, &lmsg);
		}

	      x_dnd_end_window = x_dnd_last_seen_window;
	      x_dnd_last_seen_window = None;
	      x_dnd_last_seen_toplevel = None;
	      x_dnd_in_progress = false;
	      x_dnd_return_frame_object
		= x_any_window_to_frame (dpyinfo, toplevel);
	      x_dnd_return_frame = 3;
	      x_dnd_waiting_for_finish = false;
	      target = None;
	    }
	}

      if (target != x_dnd_last_seen_window)
	{
	  if (x_dnd_last_seen_window != None
	      && x_dnd_last_protocol_version != -1
	      && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
	    x_dnd_send_leave (x_dnd_frame, x_dnd_last_seen_window,
			      x_dnd_last_seen_toplevel);
	  else if (x_dnd_last_seen_window != None
		   && XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style)
		   && !x_dnd_disable_motif_drag
		   && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
	    {
	      if (!x_dnd_motif_setup_p)
		xm_setup_drag_info (dpyinfo, x_dnd_frame);

	      lmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
					    XM_DRAG_REASON_TOP_LEVEL_LEAVE);
	      lmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
	      lmsg.zero = 0;
	      lmsg.timestamp = timestamp;
	      lmsg.source_window = FRAME_X_WINDOW (x_dnd_frame);

	      if (x_dnd_motif_setup_p)
		xm_send_top_level_leave_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
						 x_dnd_last_seen_window, &lmsg);
	    }

	  x_dnd_action = None;
	  x_dnd_last_seen_toplevel = toplevel;
	  x_dnd_last_seen_window = target;
	  x_dnd_last_protocol_version = target_proto;
	  x_dnd_last_motif_style = motif_style;
	  x_dnd_last_window_is_frame = was_frame;

	  if (target != None && x_dnd_last_protocol_version != -1)
	    x_dnd_send_enter (x_dnd_frame, target, x_dnd_last_seen_toplevel,
			      x_dnd_last_protocol_version);
	  else if (target != None && XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style)
		   && !x_dnd_disable_motif_drag)
	    {
	      if (!x_dnd_motif_setup_p)
		xm_setup_drag_info (dpyinfo, x_dnd_frame);

	      emsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
					    XM_DRAG_REASON_TOP_LEVEL_ENTER);
	      emsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
	      emsg.zero = 0;
	      emsg.timestamp = timestamp;
	      emsg.source_window = FRAME_X_WINDOW (x_dnd_frame);
	      emsg.index_atom = x_dnd_motif_atom;

	      if (x_dnd_motif_setup_p)
		xm_send_top_level_enter_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
						 target, &emsg);
	    }
	}
      else
	x_dnd_last_seen_toplevel = toplevel;

      if (x_dnd_last_window_is_frame && target != None)
	x_dnd_note_self_position (dpyinfo, target, root_x, root_y);
      else if (x_dnd_last_protocol_version != -1 && target != None)
	x_dnd_send_position (x_dnd_frame, target,
			     x_dnd_last_seen_toplevel,
			     x_dnd_last_protocol_version,
			     root_x, root_y,
			     x_dnd_selection_timestamp,
			     x_dnd_wanted_action, 0,
#ifdef HAVE_XKB
			     x_dnd_keyboard_state
#else
			     0
#endif
			     );
      else if (XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style) && target != None
	       && !x_dnd_disable_motif_drag)
	{
	  if (!x_dnd_motif_setup_p)
	    xm_setup_drag_info (dpyinfo, x_dnd_frame);

	  dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
					XM_DRAG_REASON_DRAG_MOTION);
	  dmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
	  dmsg.side_effects
	    = XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (dpyinfo,
							       x_dnd_wanted_action),
				   XM_DROP_SITE_VALID, x_dnd_motif_operations,
				   (!x_dnd_xm_use_help
				    ? XM_DROP_ACTION_DROP
				    : XM_DROP_ACTION_DROP_HELP));
	  dmsg.timestamp = timestamp;
	  dmsg.x = root_x;
	  dmsg.y = root_y;

	  if (x_dnd_motif_setup_p)
	    xm_send_drag_motion_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
					 target, &dmsg);
	}

      x_dnd_update_tooltip_position (root_x, root_y);
    }
  /* The pointer moved out of the screen.  */
  else if (x_dnd_last_protocol_version != -1)
    {
      if (x_dnd_last_seen_window != None
	  && x_dnd_last_protocol_version != -1)
	x_dnd_send_leave (x_dnd_frame,
			  x_dnd_last_seen_window,
			  x_dnd_last_seen_toplevel);
      else if (x_dnd_last_seen_window != None
	       && !XM_DRAG_STYLE_IS_DROP_ONLY (x_dnd_last_motif_style)
	       && x_dnd_last_motif_style != XM_DRAG_STYLE_NONE
	       && x_dnd_motif_setup_p)
	{
	  dsmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
					 XM_DRAG_REASON_DROP_START);
	  dmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
	  dsmsg.timestamp = timestamp;
	  dsmsg.side_effects
	    = XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (dpyinfo,
							       x_dnd_wanted_action),
				   XM_DROP_SITE_VALID, x_dnd_motif_operations,
				   XM_DROP_ACTION_DROP_CANCEL);
	  dsmsg.x = 0;
	  dsmsg.y = 0;
	  dsmsg.index_atom = x_dnd_motif_atom;
	  dsmsg.source_window = FRAME_X_WINDOW (x_dnd_frame);

	  x_dnd_send_xm_leave_for_drop (dpyinfo, x_dnd_frame,
					x_dnd_last_seen_window, timestamp);
	  xm_send_drop_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
				x_dnd_last_seen_window, &dsmsg);
	}

      x_dnd_end_window = x_dnd_last_seen_window;
      x_dnd_last_seen_window = None;
      x_dnd_last_seen_toplevel = None;
      x_dnd_in_progress = false;
      x_dnd_waiting_for_finish = false;
      x_dnd_frame = NULL;
    }
}

int
x_display_pixel_height (struct x_display_info *dpyinfo)
{
  if (dpyinfo->screen_height)
    return dpyinfo->screen_height;

  return HeightOfScreen (dpyinfo->screen);
}

int
x_display_pixel_width (struct x_display_info *dpyinfo)
{
  if (dpyinfo->screen_width)
    return dpyinfo->screen_width;

  return WidthOfScreen (dpyinfo->screen);
}

/* Handle events from each display until CELL's car becomes non-nil,
   or TIMEOUT elapses.  */
void
x_wait_for_cell_change (Lisp_Object cell, struct timespec timeout)
{
  struct x_display_info *dpyinfo;
  fd_set fds;
  int fd, maxfd;
#ifndef USE_GTK
  int finish, rc;
  XEvent event;
  fd_set rfds;
#endif
  struct input_event hold_quit;
  struct timespec current, at;

  at = timespec_add (current_timespec (), timeout);

#ifndef USE_GTK
  FD_ZERO (&rfds);
  rc = -1;
#endif

  while (true)
    {
      FD_ZERO (&fds);
      maxfd = -1;

      for (dpyinfo = x_display_list; dpyinfo;
	   dpyinfo = dpyinfo->next)
	{
	  fd = ConnectionNumber (dpyinfo->display);

#ifndef USE_GTK
	  if ((rc < 0 || FD_ISSET (fd, &rfds))
	      /* If pselect failed, the erroring display's IO error
		 handler will eventually be called.  */
	      && XPending (dpyinfo->display))
	    {
	      while (XPending (dpyinfo->display))
		{
		  EVENT_INIT (hold_quit);

		  XNextEvent (dpyinfo->display, &event);
		  handle_one_xevent (dpyinfo, &event,
				     &finish, &hold_quit);

		  if (!NILP (XCAR (cell)))
		    return;

		  if (finish == X_EVENT_GOTO_OUT)
		    break;

		  /* Make us quit now.  */
		  if (hold_quit.kind != NO_EVENT)
		    kbd_buffer_store_event (&hold_quit);
		}
	    }
#endif

	  if (fd > maxfd)
	    maxfd = fd;

	  eassert (fd < FD_SETSIZE);
	  FD_SET (fd, &fds);
	}

      /* Prevent events from being lost (from GTK's point of view) by
	 using GDK to run the event loop.  */
#ifdef USE_GTK
      while (gtk_events_pending ())
	{
	  EVENT_INIT (hold_quit);
	  current_count = 0;
	  current_hold_quit = &hold_quit;
	  current_finish = X_EVENT_NORMAL;

	  gtk_main_iteration ();

	  current_count = -1;
	  current_hold_quit = NULL;

	  /* Make us quit now.  */
	  if (hold_quit.kind != NO_EVENT)
	    kbd_buffer_store_event (&hold_quit);

	  if (!NILP (XCAR (cell)))
	    return;

	  if (current_finish == X_EVENT_GOTO_OUT)
	    break;
	}
#endif

      eassert (maxfd >= 0);

      current = current_timespec ();

      if (timespec_cmp (at, current) < 0
	  || !NILP (XCAR (cell)))
	return;

      timeout = timespec_sub (at, current);

#ifndef USE_GTK
      rc = pselect (maxfd + 1, &fds, NULL, NULL, &timeout, NULL);

      if (rc >= 0)
	rfds = fds;
#else
      pselect (maxfd + 1, &fds, NULL, NULL, &timeout, NULL);
#endif
    }
}

/* Find whether or not an undelivered MONITORS_CHANGED_EVENT is
   already on the event queue.  DPYINFO is the display any such event
   must apply to.  */

static bool
x_find_monitors_changed_event (struct x_display_info *dpyinfo)
{
  union buffered_input_event *event;

  event = kbd_fetch_ptr;

  while (event != kbd_store_ptr)
    {
      if (event->ie.kind == MONITORS_CHANGED_EVENT
	  && XTERMINAL (event->ie.arg) == dpyinfo->terminal)
	return true;

      event = X_NEXT_KBD_EVENT (event);
    }

  return false;
}

#ifdef USE_GTK
static void
x_monitors_changed_cb (GdkScreen *gscr, gpointer user_data)
{
  struct x_display_info *dpyinfo;
  struct input_event ie;
  Lisp_Object current_monitors, terminal;
  GdkDisplay *gdpy;
  Display *dpy;

  gdpy = gdk_screen_get_display (gscr);
  dpy = gdk_x11_display_get_xdisplay (gdpy);
  dpyinfo = x_display_info_for_display (dpy);

  if (!dpyinfo)
    return;

  if (x_find_monitors_changed_event (dpyinfo))
    return;

  XSETTERMINAL (terminal, dpyinfo->terminal);

  current_monitors
    = Fx_display_monitor_attributes_list (terminal);

  if (NILP (Fequal (current_monitors,
		    dpyinfo->last_monitor_attributes_list)))
    {
      EVENT_INIT (ie);
      ie.kind = MONITORS_CHANGED_EVENT;
      ie.arg = terminal;

      kbd_buffer_store_event (&ie);

      if (x_dnd_in_progress && x_dnd_update_tooltip)
	x_dnd_monitors = current_monitors;

      x_dnd_update_tooltip_now ();
    }

  dpyinfo->last_monitor_attributes_list = current_monitors;
}
#endif

/* Extract the root window coordinates from the client message EVENT
   if it is a message that we already understand.  Return false if the
   event was not understood.  */
static bool
x_coords_from_dnd_message (struct x_display_info *dpyinfo,
			   XEvent *event, int *x_out, int *y_out)
{
  xm_drag_motion_message dmsg;
  xm_drag_motion_reply dreply;
  xm_drop_start_message smsg;
  xm_drop_start_reply reply;
  unsigned long kde_data;

  if (event->type != ClientMessage)
    return false;

  if (event->xclient.message_type == dpyinfo->Xatom_XdndPosition)
    {
      if (event->xclient.format != 32)
	return false;

      *x_out = (((unsigned long) event->xclient.data.l[2]) >> 16
		& 0xffff);
      *y_out = (event->xclient.data.l[2] & 0xffff);

      return true;
    }

  if ((event->xclient.message_type
       == dpyinfo->Xatom_MOTIF_DRAG_AND_DROP_MESSAGE)
      && event->xclient.format == 8)
    {
      if (!xm_read_drag_motion_message (event, &dmsg))
	{
	  *x_out = dmsg.x;
	  *y_out = dmsg.y;

	  return true;
	}
      else if (!xm_read_drag_motion_reply (event, &dreply))
	{
	  *x_out = dreply.better_x;
	  *y_out = dreply.better_y;

	  return true;
	}
      else if (!xm_read_drop_start_message (event, &smsg))
	{
	  *x_out = smsg.x;
	  *y_out = smsg.y;

	  return true;
	}
      else if (!xm_read_drop_start_reply (event, &reply))
	{
	  *x_out = reply.better_x;
	  *y_out = reply.better_y;

	  return true;
	}
    }

  if (((event->xclient.message_type
	== dpyinfo->Xatom_DndProtocol)
       || (event->xclient.message_type
	   == dpyinfo->Xatom_DND_PROTOCOL))
      && event->xclient.format == 32
      /* Check that the version of the old KDE protocol is new
	 enough to include coordinates.  */
      && event->xclient.data.l[4])
    {
      kde_data = (unsigned long) event->xclient.data.l[3];

      *x_out = (kde_data & 0xffff);
      *y_out = (kde_data >> 16 & 0xffff);

      return true;
    }

  return false;
}

static void
x_handle_wm_state (struct frame *f, struct input_event *ie)
{
  struct x_display_info *dpyinfo;
  Atom type;
  int format;
  unsigned long nitems, bytes_after;
  unsigned char *data;
  unsigned long *state;

  data = NULL;
  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (XGetWindowProperty (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
			  dpyinfo->Xatom_wm_state, 0, 2, False,
			  AnyPropertyType, &type, &format, &nitems,
			  &bytes_after, &data) != Success)
    return;

  if (!data || nitems != 2 || format != 32)
    {
      if (data)
	XFree (data);

      return;
    }

  state = (unsigned long *) data;

  if (state[0] == NormalState && FRAME_ICONIFIED_P (f))
    {
      /* The frame has been deiconified.  It has not been withdrawn
	 and is now visible.  */
      SET_FRAME_VISIBLE (f, 1);
      SET_FRAME_ICONIFIED (f, false);
      f->output_data.x->has_been_visible = true;

      ie->kind = DEICONIFY_EVENT;
      XSETFRAME (ie->frame_or_window, f);
    }
  else if (state[0] == IconicState
	   /* _NET_WM_STATE_HIDDEN should be used if the window
	      manager supports that.  */
	   && !x_wm_supports (f, dpyinfo->Xatom_net_wm_state_hidden))
    {
      /* The frame is actually iconified right now.  Mark it as
	 such.  */

      SET_FRAME_VISIBLE (f, 0);
      SET_FRAME_ICONIFIED (f, true);

      ie->kind = ICONIFY_EVENT;
      XSETFRAME (ie->frame_or_window, f);
    }

  /* state[0] can also be WithdrawnState, meaning that the window has
     been withdrawn and is no longer iconified.  However, Emacs sets
     the correct flags upon withdrawing the window, so there is no
     need to do anything here.  */

  XFree (data);
}

#ifdef HAVE_XFIXES

static bool
x_handle_selection_monitor_event (struct x_display_info *dpyinfo,
				  const XEvent *event)
{
  XFixesSelectionNotifyEvent *notify;
  int i;

  notify = (XFixesSelectionNotifyEvent *) event;

  if (notify->window != dpyinfo->selection_tracking_window)
    return false;

  for (i = 0; i < dpyinfo->n_monitored_selections; ++i)
    {
      /* We don't have to keep track of timestamps here.  */
      if (notify->selection == dpyinfo->monitored_selections[i].name)
	dpyinfo->monitored_selections[i].owner = notify->owner;
    }

  return true;
}

Window
x_find_selection_owner (struct x_display_info *dpyinfo, Atom selection)
{
  int i;

  for (i = 0; i < dpyinfo->n_monitored_selections; ++i)
    {
      if (selection == dpyinfo->monitored_selections[i].name)
	return dpyinfo->monitored_selections[i].owner;
    }

  return X_INVALID_WINDOW;
}

#endif

/* Handles the XEvent EVENT on display DPYINFO.

   *FINISH is X_EVENT_GOTO_OUT if caller should stop reading events.
   *FINISH is zero if caller should continue reading events.
   *FINISH is X_EVENT_DROP if event should not be passed to the toolkit.
   *EVENT is unchanged unless we're processing KeyPress event.

   We return the number of characters stored into the buffer.  */

static int
handle_one_xevent (struct x_display_info *dpyinfo,
#ifndef HAVE_XINPUT2
		   const XEvent *event,
#else
		   XEvent *event,
#endif
		   int *finish, struct input_event *hold_quit)
{
  union buffered_input_event inev;
  int count = 0;
  int do_help = 0;
#ifdef HAVE_XINPUT2
  struct xi_device_t *gen_help_device;
  Time gen_help_time UNINIT;
#endif
  ptrdiff_t nbytes = 0;
  struct frame *any, *f = NULL, *mouse_frame;
  Mouse_HLInfo *hlinfo = &dpyinfo->mouse_highlight;
  /* This holds the state XLookupString needs to implement dead keys
     and other tricks known as "compose processing".  _X Window System_
     says that a portable program can't use this, but Stephen Gildea assures
     me that letting the compiler initialize it to zeros will work okay.  */
  static XComposeStatus compose_status;
  XEvent configureEvent;
  XEvent next_event;
  Lisp_Object coding;
#if defined USE_X_TOOLKIT && defined HAVE_XINPUT2
  /* Some XInput 2 events are important for Motif and Lucid menu bars
     to work correctly, so they must be translated into core events
     before being passed to XtDispatchEvent.  */
  bool use_copy = false;
  XEvent copy;
#elif defined USE_GTK && !defined HAVE_GTK3 && defined HAVE_XINPUT2
  GdkEvent *copy = NULL;
  GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (dpyinfo->display);
#endif
  int dx, dy;

  /* Avoid warnings when SAFE_ALLOCA is not actually used.  */
#if defined HAVE_XINPUT2 || defined HAVE_XKB || defined HAVE_X_I18N
  USE_SAFE_ALLOCA;
#endif

  /* This function is not reentrant, so input should be blocked before
     it is called.  */

  if (!input_blocked_p ())
    emacs_abort ();

  *finish = X_EVENT_NORMAL;

  EVENT_INIT (inev.ie);
  inev.ie.kind = NO_EVENT;
  inev.ie.arg = Qnil;
#ifdef HAVE_XINPUT2
  gen_help_device = NULL;
#endif

  /* Ignore events coming from various extensions, such as XFIXES and
     XKB.  */
  if (event->type < LASTEvent)
    {
#ifdef HAVE_XINPUT2
      if (event->type != GenericEvent)
#endif
	any = x_any_window_to_frame (dpyinfo, event->xany.window);
#ifdef HAVE_XINPUT2
      else
	any = NULL;
#endif
    }
  else
    any = NULL;

  if (any && any->wait_event_type == event->type)
    any->wait_event_type = 0; /* Indicates we got it.  */

  switch (event->type)
    {
    case ClientMessage:
      {
	int rc;

	if (((x_dnd_in_progress
	      && FRAME_DISPLAY_INFO (x_dnd_frame) == dpyinfo)
	     || (x_dnd_waiting_for_finish
		 && FRAME_DISPLAY_INFO (x_dnd_finish_frame) == dpyinfo))
	    && event->xclient.message_type == dpyinfo->Xatom_XdndStatus)
	  {
	    Window target;
	    unsigned long r1, r2;
	    int root_x, root_y;
	    bool button;

	    target = event->xclient.data.l[0];

	    if (x_dnd_last_protocol_version != -1
		&& x_dnd_in_progress
		&& target == x_dnd_last_seen_toplevel
		/* The XDND documentation is not very clearly worded.
		   But this should be the correct behavior, since
		   "kDNDStatusSendHereFlag" in the reference
		   implementation is 2, and means the mouse rect
		   should be ignored.  */
		&& !(event->xclient.data.l[1] & 2))
	      {
		r1 = event->xclient.data.l[2];
		r2 = event->xclient.data.l[3];

		x_dnd_mouse_rect_target = target;
		x_dnd_mouse_rect.x = (r1 & 0xffff0000) >> 16;
		x_dnd_mouse_rect.y = (r1 & 0xffff);
		x_dnd_mouse_rect.width = (r2 & 0xffff0000) >> 16;
		x_dnd_mouse_rect.height = (r2 & 0xffff);
	      }
	    else
	      x_dnd_mouse_rect_target = None;

	    if (x_dnd_last_protocol_version != -1
		&& (x_dnd_in_progress
		    && target == x_dnd_last_seen_window))
	      {
		if (event->xclient.data.l[1] & 1)
		  {
		    if (x_dnd_last_protocol_version >= 2)
		      x_dnd_action = event->xclient.data.l[4];
		    else
		      x_dnd_action = dpyinfo->Xatom_XdndActionCopy;
		  }
		else
		  x_dnd_action = None;
	      }

	    /* Send any pending XdndPosition message.  */
	    if (x_dnd_waiting_for_status_window == target)
	      {
		if (x_dnd_pending_send_position.type != 0)
		  {
		    /* If the last XdndStatus specified a mouse
		       rectangle and this event falls inside, don't
		       send the event, but clear
		       x_dnd_waiting_for_status_window instead.  */

		    root_x = x_dnd_pending_send_position_root_x;
		    root_y = x_dnd_pending_send_position_root_y;
		    button = x_dnd_pending_send_position_button;

		    if (target == x_dnd_mouse_rect_target
			&& x_dnd_mouse_rect.width
			&& x_dnd_mouse_rect.height
			/* Ignore the mouse rectangle if we're
			   supposed to be sending a button press
			   instead.  */
			&& !button
			&& (root_x >= x_dnd_mouse_rect.x
			    && root_x < (x_dnd_mouse_rect.x
					 + x_dnd_mouse_rect.width)
			    && root_y >= x_dnd_mouse_rect.y
			    && root_y < (x_dnd_mouse_rect.y
					 + x_dnd_mouse_rect.height)))
		      x_dnd_waiting_for_status_window = None;
		    else
		      {
			x_ignore_errors_for_next_request (dpyinfo, 0);
			XSendEvent (dpyinfo->display, target,
				    False, NoEventMask,
				    &x_dnd_pending_send_position);
			x_stop_ignoring_errors (dpyinfo);
			x_dnd_pending_send_position.type = 0;

			/* Since we sent another XdndPosition message, we
			   have to wait for another one in reply, so don't
			   reset `x_dnd_waiting_for_status_window'
			   here.  */
		      }
		  }
		else
		  x_dnd_waiting_for_status_window = None;

		/* Send any pending drop if warranted.  */
		if (x_dnd_waiting_for_finish && x_dnd_need_send_drop
		    && x_dnd_waiting_for_status_window == None)
		  {
		    if (event->xclient.data.l[1] & 1)
		      {
			if (x_dnd_send_drop_proto >= 2)
			  x_dnd_action = event->xclient.data.l[4];
			else
			  x_dnd_action = dpyinfo->Xatom_XdndActionCopy;
		      }
		    else
		      x_dnd_action = None;

		    x_dnd_waiting_for_finish
		      = x_dnd_send_drop (x_dnd_finish_frame,
					 target,
					 x_dnd_last_seen_toplevel,
					 x_dnd_selection_timestamp,
					 x_dnd_send_drop_proto);
		  }
	      }

	    goto done;
	  }

	if (event->xclient.message_type == dpyinfo->Xatom_XdndFinished
	    && (x_dnd_waiting_for_finish && !x_dnd_waiting_for_motif_finish)
	    /* Also check that the display is correct, since
	       `x_dnd_pending_finish_target' could still be valid on
	       another X server.  */
	    && dpyinfo->display == x_dnd_finish_display
	    && event->xclient.data.l[0] == x_dnd_pending_finish_target)
	  {
	    x_dnd_waiting_for_finish = false;

	    if (x_dnd_waiting_for_finish_proto >= 5)
	      x_dnd_action = event->xclient.data.l[2];

	    if (x_dnd_waiting_for_finish_proto >= 5
		&& !(event->xclient.data.l[1] & 1))
	      x_dnd_action = None;

	    goto done;
	  }

	if ((event->xclient.message_type
	     == dpyinfo->Xatom_MOTIF_DRAG_AND_DROP_MESSAGE)
	    && x_dnd_waiting_for_finish
	    && x_dnd_waiting_for_motif_finish == 1
	    && dpyinfo == x_dnd_waiting_for_motif_finish_display)
	  {
	    xm_drop_start_reply reply;
	    uint16_t operation, status, action;

	    if (!xm_read_drop_start_reply (event, &reply))
	      {
		operation = XM_DRAG_SIDE_EFFECT_OPERATION (reply.side_effects);
		status = XM_DRAG_SIDE_EFFECT_SITE_STATUS (reply.side_effects);
		action = XM_DRAG_SIDE_EFFECT_DROP_ACTION (reply.side_effects);

		if (operation != XM_DRAG_MOVE
		    && operation != XM_DRAG_COPY
		    && XM_DRAG_OPERATION_IS_LINK (operation))
		  {
		    x_dnd_waiting_for_finish = false;
		    goto done;
		  }

		if (status != XM_DROP_SITE_VALID
		    || (action == XM_DROP_ACTION_DROP_CANCEL
			|| action == XM_DROP_ACTION_DROP_HELP))
		  {
		    x_dnd_waiting_for_finish = false;
		    goto done;
		  }

		switch (operation)
		  {
		  case XM_DRAG_MOVE:
		    x_dnd_action_symbol = QXdndActionMove;
		    break;

		  case XM_DRAG_COPY:
		    x_dnd_action_symbol = QXdndActionCopy;
		    break;

		    /* This means XM_DRAG_OPERATION_IS_LINK (operation).  */
		  default:
		    x_dnd_action_symbol = QXdndActionLink;
		    break;
		  }

		x_dnd_waiting_for_motif_finish = 2;
		goto done;
	      }
	  }

	if (event->xclient.message_type == dpyinfo->Xatom_EMACS_TMP
	    && event->xclient.format == 8)
	  {
	    /* This is actually an hourglass message.  Set whether or
	       not events from here on have the hourglass enabled.  */

	    if (any)
	      FRAME_X_OUTPUT (any)->hourglass_p = event->xclient.data.b[0];
	  }

        if (event->xclient.message_type == dpyinfo->Xatom_wm_protocols
            && event->xclient.format == 32)
          {
            if (event->xclient.data.l[0] == dpyinfo->Xatom_wm_take_focus)
              {
                /* Use the value returned by x_any_window_to_frame
		   because this could be the shell widget window
		   if the frame has no title bar.  */
                f = any;
#ifdef HAVE_X_I18N
                /* Not quite sure this is needed -pd */
                if (f && FRAME_XIC (f))
                  XSetICFocus (FRAME_XIC (f));
#endif
#if false
      /* Emacs sets WM hints whose `input' field is `true'.  This
	 instructs the WM to set the input focus automatically for
	 Emacs with a call to XSetInputFocus.  Setting WM_TAKE_FOCUS
	 tells the WM to send us a ClientMessage WM_TAKE_FOCUS after
	 it has set the focus.  So, XSetInputFocus below is not
	 needed.

	 The call to XSetInputFocus below has also caused trouble.  In
	 cases where the XSetInputFocus done by the WM and the one
	 below are temporally close (on a fast machine), the call
	 below can generate additional FocusIn events which confuse
	 Emacs.  */

                /* Since we set WM_TAKE_FOCUS, we must call
                   XSetInputFocus explicitly.  But not if f is null,
                   since that might be an event for a deleted frame.  */
                if (f)
                  {
                    Display *d = event->xclient.display;
                    /* Catch and ignore errors, in case window has been
                       iconified by a window manager such as GWM.  */
                    x_catch_errors (d);
                    XSetInputFocus (d, event->xclient.window,
                                    /* The ICCCM says this is
                                       the only valid choice.  */
                                    RevertToParent,
                                    event->xclient.data.l[1]);
                    x_uncatch_errors ();
                  }
                /* Not certain about handling scroll bars here */
#endif
		/* Set the provided time as the user time, which is
		   required for SetInputFocus to work correctly after
		   taking the input focus.  */

		/* Time can be sign extended if retrieved from a client message.
		   Make sure it is always 32 bits, or systems with 64-bit longs
		   will crash after 24 days of X server uptime.  (bug#59480) */
		x_display_set_last_user_time (dpyinfo, (event->xclient.data.l[1]
							& 0xffffffff),
					      true, true);
		goto done;
              }

            if (event->xclient.data.l[0] == dpyinfo->Xatom_wm_save_yourself)
              {
                /* Save state modify the WM_COMMAND property to
                   something which can reinstate us.  This notifies
                   the session manager, who's looking for such a
                   PropertyNotify.  Can restart processing when
                   a keyboard or mouse event arrives.  */
                /* If we have a session manager, don't set this.
                   KDE will then start two Emacsen, one for the
                   session manager and one for this. */
#ifdef HAVE_X_SM
                if (! x_session_have_connection ())
#endif
                  {
                    f = x_top_window_to_frame (dpyinfo,
                                               event->xclient.window);
                    /* This is just so we only give real data once
                       for a single Emacs process.  */
                    if (f == SELECTED_FRAME ())
                      XSetCommand (FRAME_X_DISPLAY (f),
                                   event->xclient.window,
                                   initial_argv, initial_argc);
                    else if (f)
                      XSetCommand (FRAME_X_DISPLAY (f),
                                   event->xclient.window,
                                   0, 0);
                  }
		goto done;
              }

            if (event->xclient.data.l[0] == dpyinfo->Xatom_wm_delete_window)
              {
                f = x_top_window_to_frame (dpyinfo,
					   event->xclient.window);

                if (!f)
		  goto OTHER; /* May be a dialog that is to be removed  */

		inev.ie.kind = DELETE_WINDOW_EVENT;
		inev.ie.timestamp = event->xclient.data.l[1];
		XSETFRAME (inev.ie.frame_or_window, f);
		goto done;
              }


	    if (event->xclient.data.l[0] == dpyinfo->Xatom_net_wm_ping
		/* Handling window stacking changes during
		   drag-and-drop requires Emacs to select for
		   SubstructureNotifyMask, which in turn causes the
		   message to be sent to Emacs itself using the event
		   mask specified by the EWMH.  To avoid an infinite
		   loop, make sure the client message's window is not
		   the root window if DND is in progress.  */
		&& (!(x_dnd_in_progress
		      || x_dnd_waiting_for_finish)
		    || event->xclient.window != dpyinfo->root_window)
		&& event->xclient.format == 32)
	      {
		XEvent send_event = *event;

		send_event.xclient.window = dpyinfo->root_window;
		XSendEvent (dpyinfo->display, dpyinfo->root_window, False,
			    SubstructureRedirectMask | SubstructureNotifyMask,
			    &send_event);

		*finish = X_EVENT_DROP;
		goto done;
	      }

#if defined HAVE_XSYNC
	    if (event->xclient.data.l[0] == dpyinfo->Xatom_net_wm_sync_request
		&& event->xclient.format == 32
		&& dpyinfo->xsync_supported_p)
	      {
		struct frame *f
		  = x_top_window_to_frame (dpyinfo,
					   event->xclient.window);
		if (f)
		  {
#ifndef HAVE_GTK3
		    if (event->xclient.data.l[4] == 0)
		      {
			XSyncIntsToValue (&FRAME_X_OUTPUT (f)->pending_basic_counter_value,
					  event->xclient.data.l[2], event->xclient.data.l[3]);
			FRAME_X_OUTPUT (f)->sync_end_pending_p = true;
		      }
		    else if (event->xclient.data.l[4] == 1)
		      {
			XSyncIntsToValue (&FRAME_X_OUTPUT (f)->resize_counter_value,
					  event->xclient.data.l[2],
					  event->xclient.data.l[3]);

			FRAME_X_OUTPUT (f)->ext_sync_end_pending_p = true;
		      }

		    *finish = X_EVENT_DROP;
#else
		    GtkWidget *widget = FRAME_GTK_OUTER_WIDGET (f);
		    GdkWindow *window = gtk_widget_get_window (widget);
		    eassert (window);

		    /* This could be a (former) child frame for which
		       frame synchronization was disabled.  Enable it
		       now.  */
		    gdk_x11_window_set_frame_sync_enabled (window, TRUE);

		    if (widget && !FRAME_X_OUTPUT (f)->xg_sync_end_pending_p)
		      {
			GdkFrameClock *frame_clock = gdk_window_get_frame_clock (window);
			eassert (frame_clock);

			gdk_frame_clock_request_phase (frame_clock,
						       GDK_FRAME_CLOCK_PHASE_BEFORE_PAINT);
			FRAME_X_OUTPUT (f)->xg_sync_end_pending_p = true;
		      }
#endif
		    goto done;
		  }
	      }
#endif

	    goto done;
          }

        if (event->xclient.message_type == dpyinfo->Xatom_wm_configure_denied)
	  goto done;

        if (event->xclient.message_type == dpyinfo->Xatom_wm_window_moved)
          {
            int new_x, new_y;
	    f = x_window_to_frame (dpyinfo, event->xclient.window);

            new_x = event->xclient.data.s[0];
            new_y = event->xclient.data.s[1];

            if (f)
              {
                f->left_pos = new_x;
                f->top_pos = new_y;
              }
	    goto done;
          }

#ifdef X_TOOLKIT_EDITRES
        if (event->xclient.message_type == dpyinfo->Xatom_editres)
          {
	    f = any;
	    if (f)
	      {
		_XEditResCheckMessages (f->output_data.x->widget,
					NULL, (XEvent *) event, NULL);
		goto done;
	      }

	    goto OTHER;
          }
#endif /* X_TOOLKIT_EDITRES */

        if (event->xclient.message_type == dpyinfo->Xatom_DONE
	    || event->xclient.message_type == dpyinfo->Xatom_PAGE)
          {
            /* Ghostview job completed.  Kill it.  We could
               reply with "Next" if we received "Page", but we
               currently never do because we are interested in
               images, only, which should have 1 page.  */
	    f = x_window_to_frame (dpyinfo, event->xclient.window);
	    if (!f)
	      goto OTHER;
#ifndef USE_CAIRO
            Pixmap pixmap = (Pixmap) event->xclient.data.l[1];
	    /* FIXME: why does this sometimes generate a BadMatch
	       error?  */
	    x_catch_errors (dpyinfo->display);
            x_kill_gs_process (pixmap, f);
	    x_uncatch_errors ();
            expose_frame (f, 0, 0, 0, 0);
#endif	/* !USE_CAIRO */
	    goto done;
          }

#ifdef USE_TOOLKIT_SCROLL_BARS
        /* Scroll bar callbacks send a ClientMessage from which
           we construct an input_event.  */
        if (event->xclient.message_type == dpyinfo->Xatom_Scrollbar)
          {
	    /* Convert the scroll bar event to an input event using
	       the first window entered into the scroll bar event
	       queue. */
            x_scroll_bar_to_input_event (dpyinfo, event, &inev.ie);

	    *finish = X_EVENT_GOTO_OUT;
            goto done;
          }
        else if (event->xclient.message_type == dpyinfo->Xatom_Horizontal_Scrollbar)
          {
	    /* Convert the horizontal scroll bar event to an input
	       event using the first window entered into the scroll
	       bar event queue. */
            x_horizontal_scroll_bar_to_input_event (dpyinfo, event,
						    &inev.ie);

	    *finish = X_EVENT_GOTO_OUT;
            goto done;
          }
#endif /* USE_TOOLKIT_SCROLL_BARS */

	/* XEmbed messages from the embedder (if any).  */
        if (event->xclient.message_type == dpyinfo->Xatom_XEMBED)
          {
	    enum xembed_message msg = event->xclient.data.l[1];
	    if (msg == XEMBED_FOCUS_IN || msg == XEMBED_FOCUS_OUT)
	      x_detect_focus_change (dpyinfo, any, event, &inev.ie);

	    *finish = X_EVENT_GOTO_OUT;
            goto done;
          }

#if defined HAVE_XSYNC && !defined USE_GTK && defined HAVE_CLOCK_GETTIME
	/* These messages are sent by the compositing manager after a
	   frame is drawn under extended synchronization.  */
	if (event->xclient.message_type
	    == dpyinfo->Xatom_net_wm_frame_drawn)
	  {
	    if (any)
	      x_sync_handle_frame_drawn (dpyinfo, (XEvent *) event, any);

	    goto done;
	  }

	if (event->xclient.message_type
	    == dpyinfo->Xatom_net_wm_frame_timings)
	  goto done;
#endif

        if (xft_settings_event (dpyinfo, event))
	  goto done;

	f = any;
	/* We don't want to ever leak tooltip frames to Lisp code.  */
	if (!f || FRAME_TOOLTIP_P (f))
	  goto OTHER;

	/* These values are always used initialized, but GCC doesn't
	   know that.  */
	dx = 0;
	dy = 0;

	rc = x_coords_from_dnd_message (dpyinfo, (XEvent *) event,
					&dx, &dy);

	if (x_handle_dnd_message (f, &event->xclient, dpyinfo, &inev.ie,
				  rc, dx, dy))
	  *finish = X_EVENT_DROP;
      }
      break;

    case SelectionNotify:
#if defined USE_X_TOOLKIT || defined USE_GTK
      if (!x_window_to_frame (dpyinfo, event->xselection.requestor))
        goto OTHER;
#endif /* not USE_X_TOOLKIT and not USE_GTK */
#ifdef HAVE_GTK3
      /* GTK 3 apparently chokes on these events since they have no
	 associated device.  (bug#56869, another bug as well that I
	 can't find) */
      *finish = X_EVENT_DROP;
#endif
      x_handle_selection_notify (&event->xselection);
      break;

    case SelectionClear:	/* Someone has grabbed ownership.  */
#if defined USE_X_TOOLKIT || defined USE_GTK
      if (!x_window_to_frame (dpyinfo, event->xselectionclear.window))
        goto OTHER;
#endif /* not USE_X_TOOLKIT and not USE_GTK */
#ifdef HAVE_GTK3
      *finish = X_EVENT_DROP;
#endif
      {
        const XSelectionClearEvent *eventp = &event->xselectionclear;

	if (eventp->selection == dpyinfo->motif_drag_atom
	    && (eventp->time == CurrentTime
		|| dpyinfo->motif_drag_atom_time <= eventp->time))
	  dpyinfo->motif_drag_atom = None;

        inev.sie.kind = SELECTION_CLEAR_EVENT;
        SELECTION_EVENT_DPYINFO (&inev.sie) = dpyinfo;
        SELECTION_EVENT_SELECTION (&inev.sie) = eventp->selection;
        SELECTION_EVENT_TIME (&inev.sie) = eventp->time;

	if (x_use_pending_selection_requests)
	  {
	    x_push_selection_request (&inev.sie);
	    EVENT_INIT (inev.ie);
	  }
      }
      break;

    case SelectionRequest:	/* Someone wants our selection.  */
#ifdef USE_X_TOOLKIT
      if (!x_window_to_frame (dpyinfo, event->xselectionrequest.owner))
        goto OTHER;
#endif /* USE_X_TOOLKIT */
#ifdef HAVE_GTK3
      *finish = X_EVENT_DROP;
#endif
      {
	const XSelectionRequestEvent *eventp = &event->xselectionrequest;

	inev.sie.kind = SELECTION_REQUEST_EVENT;
	SELECTION_EVENT_DPYINFO (&inev.sie) = dpyinfo;
	SELECTION_EVENT_REQUESTOR (&inev.sie) = eventp->requestor;
	SELECTION_EVENT_SELECTION (&inev.sie) = eventp->selection;
	SELECTION_EVENT_TARGET (&inev.sie) = eventp->target;
	SELECTION_EVENT_PROPERTY (&inev.sie) = eventp->property;
	SELECTION_EVENT_TIME (&inev.sie) = eventp->time;

	/* If drag-and-drop or another modal dialog/menu is in
	   progress, handle SelectionRequest events immediately, by
	   pushing it onto the selection queue.  */

	if (x_use_pending_selection_requests)
	  {
	    x_push_selection_request (&inev.sie);
	    EVENT_INIT (inev.ie);
	  }

	if (x_dnd_waiting_for_finish
	    && x_dnd_waiting_for_motif_finish == 2
	    && dpyinfo == x_dnd_waiting_for_motif_finish_display
	    && eventp->selection == x_dnd_motif_atom
	    && (eventp->target == dpyinfo->Xatom_XmTRANSFER_SUCCESS
		|| eventp->target == dpyinfo->Xatom_XmTRANSFER_FAILURE))
	  {
	    x_dnd_waiting_for_finish = false;

	    /* If the transfer failed, then return nil from
	       `x-begin-drag'.  */
	    if (eventp->target == dpyinfo->Xatom_XmTRANSFER_FAILURE)
	      x_dnd_action = None;
	  }
      }
      break;

    case PropertyNotify:
      if (x_dnd_in_progress && x_dnd_use_toplevels
	  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame)
	  && event->xproperty.atom == dpyinfo->Xatom_wm_state)
	{
	  struct x_client_list_window *tem, *last;

	  for (last = NULL, tem = x_dnd_toplevels; tem;
	       last = tem, tem = tem->next)
	    {
	      if (tem->window == event->xproperty.window)
		{
		  Atom actual_type;
		  int actual_format, rc;
		  unsigned long nitems, bytesafter;
		  unsigned char *data = NULL;

		  if (event->xproperty.state == PropertyDelete)
		    {
		      if (!last)
			x_dnd_toplevels = tem->next;
		      else
			last->next = tem->next;

#ifdef HAVE_XSHAPE
		      if (tem->n_input_rects != -1)
			xfree (tem->input_rects);
		      if (tem->n_bounding_rects != -1)
			xfree (tem->bounding_rects);
#endif
		      xfree (tem);
		    }
		  else
		    {
		      x_catch_errors (dpyinfo->display);
		      rc = XGetWindowProperty (dpyinfo->display,
					       event->xproperty.window,
					       dpyinfo->Xatom_wm_state,
					       0, 2, False, AnyPropertyType,
					       &actual_type, &actual_format,
					       &nitems, &bytesafter, &data);

		      if (!x_had_errors_p (dpyinfo->display) && rc == Success && data
			  && nitems == 2 && actual_format == 32)
			tem->wm_state = ((unsigned long *) data)[0];
		      else
			tem->wm_state = WithdrawnState;

		      if (data)
			XFree (data);
		      x_uncatch_errors_after_check ();
		    }

		  x_dnd_update_state (dpyinfo, event->xproperty.time);
		  break;
		}
	    }
	}

      f = x_top_window_to_frame (dpyinfo, event->xproperty.window);
      if (f && event->xproperty.atom == dpyinfo->Xatom_net_wm_state
	  /* This should never happen with embedded windows.  */
	  && !FRAME_X_EMBEDDED_P (f))
	{
          bool not_hidden = x_handle_net_wm_state (f, &event->xproperty);

	  if (not_hidden && FRAME_ICONIFIED_P (f))
	    {
	      if (CONSP (frame_size_history))
		frame_size_history_plain
		  (f, build_string ("PropertyNotify, not hidden & iconified"));

	      /* Gnome shell does not iconify us when C-z is pressed.
		 It hides the frame.  So if our state says we aren't
		 hidden anymore, treat it as deiconified.  */
	      SET_FRAME_VISIBLE (f, 1);
	      SET_FRAME_ICONIFIED (f, false);

	      f->output_data.x->has_been_visible = true;
	      inev.ie.kind = DEICONIFY_EVENT;
#if defined USE_GTK && defined HAVE_GTK3
	      /* If GTK3 wants to impose some old size here (Bug#24526),
		 tell it that the current size is what we want.  */
	      if (f->was_invisible)
		{
		  xg_frame_set_char_size
		    (f, FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f));
		  f->was_invisible = false;
		}
#endif
	      XSETFRAME (inev.ie.frame_or_window, f);
	    }
	  else if (!not_hidden && !FRAME_ICONIFIED_P (f))
	    {
	      if (CONSP (frame_size_history))
		frame_size_history_plain
		  (f, build_string ("PropertyNotify, hidden & not iconified"));

	      SET_FRAME_VISIBLE (f, 0);
	      SET_FRAME_ICONIFIED (f, true);
	      inev.ie.kind = ICONIFY_EVENT;
	      XSETFRAME (inev.ie.frame_or_window, f);
	    }
	}

      if (f && event->xproperty.atom == dpyinfo->Xatom_wm_state
	  && !FRAME_X_EMBEDDED_P (f) && !FRAME_PARENT_FRAME (f))
	/* Handle WM_STATE.  We use this to clear the iconified flag
	   on a frame if it is set.

	   GTK builds ignore deiconifying frames on FocusIn or Expose
	   by default, and cannot wait for the window manager to
	   remove _NET_WM_STATE_HIDDEN, as it is ambiguous when not
	   set.  Handling UnmapNotify also checks for
	   _NET_WM_STATE_HIDDEN, and thus suffers from the same
	   problem.  */
	x_handle_wm_state (f, &inev.ie);

      if (f && FRAME_X_OUTPUT (f)->alpha_identical_p
	  && (event->xproperty.atom
	      == dpyinfo->Xatom_net_wm_window_opacity))
	{
#ifndef USE_XCB
	  int rc, actual_format;
	  Atom actual;
	  unsigned char *tmp_data;
	  unsigned long n, left, opacity;

	  tmp_data = NULL;
#else
	  xcb_get_property_cookie_t opacity_cookie;
	  xcb_get_property_reply_t *opacity_reply;
	  xcb_generic_error_t *error;
	  bool rc;
	  uint32_t value;
#endif

	  if (event->xproperty.state == PropertyDelete)
	    {
	      f->alpha[0] = 1.0;
	      f->alpha[1] = 1.0;

	      store_frame_param (f, Qalpha, Qnil);
	    }
	  else
	    {
#ifndef USE_XCB
	      rc = XGetWindowProperty (dpyinfo->display, FRAME_OUTER_WINDOW (f),
				       dpyinfo->Xatom_net_wm_window_opacity,
				       0, 1, False, AnyPropertyType, &actual,
				       &actual_format, &n, &left, &tmp_data);

	      if (rc == Success && actual_format == 32
		  && (actual == XA_CARDINAL
		      /* Some broken programs set the opacity property
			 to those types, but window managers accept
			 them anyway.  */
		      || actual == XA_ATOM
		      || actual == XA_WINDOW) && n)
		{
		  opacity = *(unsigned long *) tmp_data & OPAQUE;
		  f->alpha[0] = (double) opacity / (double) OPAQUE;
		  f->alpha[1] = (double) opacity / (double) OPAQUE;

		  store_frame_param (f, Qalpha, make_float (f->alpha[0]));
		}
	      else
		{
		  f->alpha[0] = 1.0;
		  f->alpha[1] = 1.0;

		  store_frame_param (f, Qalpha, Qnil);
		}
#else
	      opacity_cookie
		= xcb_get_property (dpyinfo->xcb_connection, 0,
				    (xcb_window_t) FRAME_OUTER_WINDOW (f),
				    (xcb_atom_t) dpyinfo->Xatom_net_wm_window_opacity,
				    XA_CARDINAL, 0, 1);
	      opacity_reply
		= xcb_get_property_reply (dpyinfo->xcb_connection,
					  opacity_cookie, &error);

	      if (!opacity_reply)
		free (error), rc = false;
	      else
		rc = (opacity_reply->format == 32
		      && (opacity_reply->type == XA_CARDINAL
			  || opacity_reply->type == XA_ATOM
			  || opacity_reply->type == XA_WINDOW)
		      && (xcb_get_property_value_length (opacity_reply) >= 4));

	      if (rc)
		{
		  value = *(uint32_t *) xcb_get_property_value (opacity_reply);

		  f->alpha[0] = (double) value / (double) OPAQUE;
		  f->alpha[1] = (double) value / (double) OPAQUE;
		  store_frame_param (f, Qalpha, make_float (f->alpha[0]));
		}
	      else
		{
		  f->alpha[0] = 1.0;
		  f->alpha[1] = 1.0;

		  store_frame_param (f, Qalpha, Qnil);
		}

	      if (opacity_reply)
		free (opacity_reply);
#endif
	    }

#ifndef USE_XCB
	  if (tmp_data)
	    XFree (tmp_data);
#endif
	}

      if (event->xproperty.window == dpyinfo->root_window
	  && (event->xproperty.atom == dpyinfo->Xatom_net_client_list_stacking
	      || event->xproperty.atom == dpyinfo->Xatom_net_current_desktop)
	  && x_dnd_in_progress
	  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
	{
	  if (x_dnd_use_toplevels)
	    {
	      x_dnd_free_toplevels (true);

	      if (x_dnd_compute_toplevels (dpyinfo))
		{
		  x_dnd_free_toplevels (true);
		  x_dnd_use_toplevels = false;
		}
	    }

	  x_dnd_update_state (dpyinfo, event->xproperty.time);
	}

      x_handle_property_notify (&event->xproperty);
      xft_settings_event (dpyinfo, event);
      goto OTHER;

    case ReparentNotify:
      f = x_top_window_to_frame (dpyinfo, event->xreparent.window);
      if (f)
        {
#ifndef USE_GTK
	  if (FRAME_OUTPUT_DATA (f)->parent_desc
	      && FRAME_X_EMBEDDED_P (f))
	    {
	      /* The frame's embedder was destroyed; mark the frame as
		 no longer embedded, and map the frame.  An
		 UnmapNotify event must have previously been received
		 during the start of save-set processing.  */

	      FRAME_X_OUTPUT (f)->explicit_parent = false;

	      /* Remove the leftover XEMBED_INFO property.  */
	      XDeleteProperty (dpyinfo->display, FRAME_OUTER_WINDOW (f),
			       dpyinfo->Xatom_XEMBED_INFO);
	      x_make_frame_visible (f);
	    }
#endif

	  /* Maybe we shouldn't set this for child frames ??  */
	  f->output_data.x->parent_desc = event->xreparent.parent;

	  if (!FRAME_PARENT_FRAME (f))
	    {
	      x_real_positions (f, &f->left_pos, &f->top_pos);

	      /* Perhaps reparented due to a WM restart.  Reset this.  */
	      dpyinfo->wm_type = X_WMTYPE_UNKNOWN;
	      dpyinfo->net_supported_window = 0;

#ifndef USE_GTK
	      /* The window manager could have restarted and the new
		 window manager might not support user time windows,
		 so update what is used accordingly.

	         Note that this doesn't handle changes between
	         non-reparenting window managers.  */
	      if (FRAME_X_OUTPUT (f)->has_been_visible)
		x_update_frame_user_time_window (f);
#endif
	    }
	  else
	    {
	      Window root;
	      unsigned int dummy_uint;

	      XGetGeometry (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
			    &root, &f->left_pos, &f->top_pos,
			    &dummy_uint, &dummy_uint, &dummy_uint, &dummy_uint);
	    }

          x_set_frame_alpha (f);
        }
      goto OTHER;

    case Expose:

#ifdef HAVE_XWIDGETS
      {
	struct xwidget_view *xv;

	xv = xwidget_view_from_window (event->xexpose.window);

	if (xv)
	  {
	    xwidget_expose (xv);
	    goto OTHER;
	  }
      }
#endif

      f = x_window_to_frame (dpyinfo, event->xexpose.window);
      if (f)
        {
          if (!FRAME_VISIBLE_P (f))
            {
	      /* By default, do not set the frame's visibility here, see
		 https://lists.gnu.org/archive/html/emacs-devel/2017-02/msg00133.html.
		 The default behavior can be overridden by setting
		 'x-set-frame-visibility-more-laxly' (Bug#49955,
		 Bug#53298).  */
	      if (EQ (x_set_frame_visibility_more_laxly, Qexpose)
		  || EQ (x_set_frame_visibility_more_laxly, Qt))
		{
		  SET_FRAME_VISIBLE (f, 1);
		  SET_FRAME_ICONIFIED (f, false);
		}

#ifdef HAVE_XDBE
	      if (FRAME_X_DOUBLE_BUFFERED_P (f))
                x_drop_xrender_surfaces (f);
#endif
              f->output_data.x->has_been_visible = true;
              SET_FRAME_GARBAGED (f);
            }
          else if (FRAME_GARBAGED_P (f))
	    {
#ifdef USE_GTK
	      /* Go around the back buffer and manually clear the
		 window the first time we show it.  This way, we avoid
		 showing users the sanity-defying horror of whatever
		 GtkWindow is rendering beneath us.  We've garbaged
		 the frame, so we'll redraw the whole thing on next
		 redisplay anyway.  Yuck.  */
	      x_clear_area1 (FRAME_X_DISPLAY (f),
			     FRAME_X_WINDOW (f),
			     event->xexpose.x, event->xexpose.y,
			     event->xexpose.width, event->xexpose.height,
			     0);
	      x_clear_under_internal_border (f);
#endif
	    }

          if (!FRAME_GARBAGED_P (f))
            {
#if defined USE_X_TOOLKIT && defined USE_TOOLKIT_SCROLL_BARS
	      if (f->output_data.x->edit_widget)
		/* The widget's expose proc will be run in this
		   case.  */
		goto OTHER;
#endif
#ifdef USE_GTK
              /* This seems to be needed for GTK 2.6 and later, see
                 https://debbugs.gnu.org/cgi/bugreport.cgi?bug=15398.  */
              x_clear_area (f,
                            event->xexpose.x, event->xexpose.y,
                            event->xexpose.width, event->xexpose.height);
#endif
              expose_frame (f, event->xexpose.x, event->xexpose.y,
			    event->xexpose.width, event->xexpose.height);
#ifndef USE_TOOLKIT_SCROLL_BARS
	      x_scroll_bar_handle_exposure (f, (XEvent *) event);
#endif
#ifdef USE_GTK
	      x_clear_under_internal_border (f);
#endif
            }
#ifndef USE_TOOLKIT_SCROLL_BARS
	  else
	    x_scroll_bar_handle_exposure (f, (XEvent *) event);
#endif

#ifdef HAVE_XDBE
          if (!FRAME_GARBAGED_P (f))
            show_back_buffer (f);
#endif
        }
      else
        {
#if defined USE_LUCID
          /* Submenus of the Lucid menu bar aren't widgets
             themselves, so there's no way to dispatch events
             to them.  Recognize this case separately.  */
          {
            Widget widget = x_window_to_menu_bar (event->xexpose.window);
            if (widget)
              xlwmenu_redisplay (widget);
          }
#endif /* USE_LUCID */

          /* Dispatch event to the widget.  */
          goto OTHER;
        }
      break;

    case GraphicsExpose:	/* This occurs when an XCopyArea's
                                   source area was obscured or not
                                   available.  */
      f = x_window_to_frame (dpyinfo, event->xgraphicsexpose.drawable);
      if (f)
        {
          expose_frame (f, event->xgraphicsexpose.x,
                        event->xgraphicsexpose.y,
                        event->xgraphicsexpose.width,
                        event->xgraphicsexpose.height);
#ifndef USE_TOOLKIT_SCROLL_BARS
	  x_scroll_bar_handle_exposure (f, (XEvent *) event);
#endif
#ifdef USE_GTK
	  x_clear_under_internal_border (f);
#endif
#ifdef HAVE_XDBE
	  show_back_buffer (f);
#endif
        }
#ifdef USE_X_TOOLKIT
      else
        goto OTHER;
#endif /* USE_X_TOOLKIT */
      break;

    case NoExpose:		/* This occurs when an XCopyArea's
                                   source area was completely
                                   available.  */
#ifdef USE_X_TOOLKIT
      *finish = X_EVENT_DROP;
#endif
      break;

    case UnmapNotify:
      if (x_dnd_in_progress && x_dnd_use_toplevels
	  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
	{
	  for (struct x_client_list_window *tem = x_dnd_toplevels; tem;
	       tem = tem->next)
	    {
	      if (tem->window == event->xunmap.window)
		{
		  tem->mapped_p = false;
		  break;
		}
	    }
	}

      /* Redo the mouse-highlight after the tooltip has gone.  */
      if (event->xunmap.window == tip_window)
        {
          tip_window = None;
          gui_redo_mouse_highlight (dpyinfo);
        }

      f = x_top_window_to_frame (dpyinfo, event->xunmap.window);
      if (f)		/* F may no longer exist if
                           the frame was deleted.  */
        {
	  bool visible = FRAME_VISIBLE_P (f);

#ifdef USE_LUCID
	  /* Bloodcurdling hack alert: The Lucid menu bar widget's
	     redisplay procedure is not called when a tip frame over
	     menu items is unmapped.  Redisplay the menu manually...  */
	  if (FRAME_TOOLTIP_P (f) && popup_activated ())
	    {
	      Widget w;
	      Lisp_Object tail, frame;
	      struct frame *f1;

	      FOR_EACH_FRAME (tail, frame)
		{
		  if (!FRAME_X_P (XFRAME (frame)))
		    continue;

		  f1 = XFRAME (frame);

		  if (FRAME_LIVE_P (f1))
		    {
		      w = FRAME_X_OUTPUT (f1)->menubar_widget;

		      if (w && !DoesSaveUnders (FRAME_DISPLAY_INFO (f1)->screen))
			xlwmenu_redisplay (w);
		    }
		}
	    }
#endif /* USE_LUCID */

	  /* While a frame is unmapped, display generation is
             disabled; you don't want to spend time updating a
             display that won't ever be seen.  */
          SET_FRAME_VISIBLE (f, 0);
          /* We can't distinguish, from the event, whether the window
             has become iconified or invisible.  So assume, if it
             was previously visible, than now it is iconified.
             But x_make_frame_invisible clears both
             the visible flag and the iconified flag;
             and that way, we know the window is not iconified now.  */
          if (visible || FRAME_ICONIFIED_P (f))
            {
	      if (CONSP (frame_size_history))
		frame_size_history_plain
		  (f, build_string ("UnmapNotify, visible | iconified"));

              SET_FRAME_ICONIFIED (f, true);
	      inev.ie.kind = ICONIFY_EVENT;
              XSETFRAME (inev.ie.frame_or_window, f);
            }
	  else if (CONSP (frame_size_history))
	    frame_size_history_plain
	      (f, build_string ("UnmapNotify, not visible & not iconified"));
        }
      goto OTHER;

    case MapNotify:
#if defined HAVE_XINPUT2 && defined HAVE_GTK3
      if (xg_is_menu_window (dpyinfo->display, event->xmap.window))
	popup_activated_flag = 1;
#endif

      if (x_dnd_in_progress
	  /* When _NET_WM_CLIENT_LIST stacking is being used, changes
	     in that property are watched for, and it's not necessary
	     to update the state in response to ordinary window
	     substructure events.  */
	  && !x_dnd_use_toplevels
	  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
	x_dnd_update_state (dpyinfo, dpyinfo->last_user_time);

      if (x_dnd_in_progress && x_dnd_use_toplevels
	  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
	{
	  for (struct x_client_list_window *tem = x_dnd_toplevels; tem;
	       tem = tem->next)
	    {
	      if (tem->window == event->xmap.window)
		{
		  tem->mapped_p = true;
		  break;
		}
	    }
	}

      /* We use x_top_window_to_frame because map events can
         come for sub-windows and they don't mean that the
         frame is visible.  */
      f = x_top_window_to_frame (dpyinfo, event->xmap.window);
      if (f)
        {
	  bool iconified = FRAME_ICONIFIED_P (f);
	  int value;
	  bool sticky, shaded;
          bool not_hidden = x_get_current_wm_state (f, event->xmap.window, &value, &sticky,
						    &shaded);

	  if (CONSP (frame_size_history))
	    frame_size_history_extra
	      (f,
	       iconified
	       ? (not_hidden
		  ? build_string ("MapNotify, not hidden & iconified")
		  : build_string ("MapNotify, hidden & iconified"))
	       : (not_hidden
		  ? build_string ("MapNotify, not hidden & not iconified")
		  : build_string ("MapNotify, hidden & not iconified")),
	       FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f),
	       -1, -1, f->new_width, f->new_height);

	  /* Check if fullscreen was specified before we where mapped the
             first time, i.e. from the command line.  */
          if (!f->output_data.x->has_been_visible)
	    {

	      x_check_fullscreen (f);
#ifndef USE_GTK
	      /* For systems that cannot synthesize `skip_taskbar' for
		 unmapped windows do the following.  */
	      if (FRAME_SKIP_TASKBAR (f))
		x_set_skip_taskbar (f, Qt, Qnil);
#endif /* Not USE_GTK */
	    }

	  if (!iconified)
	    {
	      /* The `z-group' is reset every time a frame becomes
		 invisible.  Handle this here.  */
	      if (FRAME_Z_GROUP (f) == z_group_above)
		x_set_z_group (f, Qabove, Qnil);
	      else if (FRAME_Z_GROUP (f) == z_group_below)
		x_set_z_group (f, Qbelow, Qnil);
	    }

	  /* Embedded frames might have NET_WM_STATE left over, but
	     are always visible once mapped.  */
	  if (not_hidden || FRAME_X_EMBEDDED_P (f))
	    {
	      SET_FRAME_VISIBLE (f, 1);
	      SET_FRAME_ICONIFIED (f, false);
#if defined USE_GTK && defined HAVE_GTK3
	      /* If GTK3 wants to impose some old size here (Bug#24526),
		 tell it that the current size is what we want.  */
	      if (f->was_invisible)
		{
		  xg_frame_set_char_size
		    (f, FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f));
		  f->was_invisible = false;
		}
#endif
	      f->output_data.x->has_been_visible = true;
	    }

	  x_update_opaque_region (f, NULL);

          if ((not_hidden || FRAME_X_EMBEDDED_P (f)) && iconified)
            {
              inev.ie.kind = DEICONIFY_EVENT;
              XSETFRAME (inev.ie.frame_or_window, f);
            }
        }
      goto OTHER;

    case KeyPress:
      x_display_set_last_user_time (dpyinfo, event->xkey.time,
				    event->xkey.send_event,
				    true);
      ignore_next_mouse_click_timeout = 0;

      coding = Qlatin_1;

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
      /* Dispatch KeyPress events when in menu.  */
      if (popup_activated ())
        goto OTHER;
#endif

      f = any;

#ifdef USE_GTK
      if (f)
	x_set_gtk_user_time (f, event->xkey.time);
#endif

      /* If mouse-highlight is an integer, input clears out
	 mouse highlighting.  */
      if (!hlinfo->mouse_face_hidden && FIXNUMP (Vmouse_highlight)
	  && (f == NULL
	      || (!EQ (f->tab_bar_window, hlinfo->mouse_face_window)
#if ! defined (USE_GTK)
		  && !EQ (f->tool_bar_window, hlinfo->mouse_face_window)
#endif
		 )))
        {
	  mouse_frame = hlinfo->mouse_face_mouse_frame;

	  clear_mouse_face (hlinfo);
	  hlinfo->mouse_face_hidden = true;

	  if (mouse_frame)
	    x_flush_dirty_back_buffer_on (mouse_frame);
	}

#if defined USE_MOTIF && defined USE_TOOLKIT_SCROLL_BARS
      if (f == 0)
        {
          /* Scroll bars consume key events, but we want
             the keys to go to the scroll bar's frame.  */
          Widget widget = XtWindowToWidget (dpyinfo->display,
                                            event->xkey.window);
          if (widget && XmIsScrollBar (widget))
            {
              widget = XtParent (widget);
              f = x_any_window_to_frame (dpyinfo, XtWindow (widget));
            }
        }
#endif /* USE_MOTIF and USE_TOOLKIT_SCROLL_BARS */

      if (f != 0)
        {
          KeySym keysym, orig_keysym;
          /* al%imercury@uunet.uu.net says that making this 81
             instead of 80 fixed a bug whereby meta chars made
             his Emacs hang.

             It seems that some version of XmbLookupString has
             a bug of not returning XBufferOverflow in
             status_return even if the input is too long to
             fit in 81 bytes.  So, we must prepare sufficient
             bytes for copy_buffer.  513 bytes (256 chars for
             two-byte character set) seems to be a fairly good
             approximation.  -- 2000.8.10 handa@gnu.org  */
          unsigned char copy_buffer[513];
          unsigned char *copy_bufptr = copy_buffer;
          int copy_bufsiz = sizeof (copy_buffer);
          int modifiers;
	  Lisp_Object c;
	  /* `xkey' will be modified, but it's not important to modify
	     `event' itself.  */
	  XKeyEvent xkey = event->xkey;

	  if (event->xkey.window == FRAME_X_WINDOW (f))
	    /* See the comment above x_compute_root_window_offset for
	       why this optimization is performed.  */
	    x_compute_root_window_offset (f, event->xkey.x_root,
					  event->xkey.y_root,
					  event->xkey.x, event->xkey.y);

#ifdef HAVE_XINPUT2
	  Time pending_keystroke_time;
	  struct xi_device_t *source;

	  pending_keystroke_time = dpyinfo->pending_keystroke_time;

	  if (event->xkey.time >= pending_keystroke_time)
	    {
#if defined USE_GTK && !defined HAVE_GTK3
	      if (!dpyinfo->pending_keystroke_time_special_p)
#endif
		dpyinfo->pending_keystroke_time = 0;
#if defined USE_GTK && !defined HAVE_GTK3
	      else
		dpyinfo->pending_keystroke_time_special_p = false;
#endif
	    }
#endif

#ifdef USE_GTK
          /* Don't pass keys to GTK.  A Tab will shift focus to the
             tool bar in GTK 2.4.  Keys will still go to menus and
             dialogs because in that case popup_activated is nonzero
             (see above).  */
          *finish = X_EVENT_DROP;
#endif

          xkey.state |= x_emacs_to_x_modifiers (dpyinfo,
						extra_keyboard_modifiers);
          modifiers = xkey.state;

          /* This will have to go some day...  */

          /* make_lispy_event turns chars into control chars.
             Don't do it here because XLookupString is too eager.  */
          xkey.state &= ~ControlMask;
          xkey.state &= ~(dpyinfo->meta_mod_mask
			  | dpyinfo->super_mod_mask
			  | dpyinfo->hyper_mod_mask
			  | dpyinfo->alt_mod_mask);

          /* In case Meta is ComposeCharacter,
             clear its status.  According to Markus Ehrnsperger
             Markus.Ehrnsperger@lehrstuhl-bross.physik.uni-muenchen.de
             this enables ComposeCharacter to work whether or
             not it is combined with Meta.  */
          if (modifiers & dpyinfo->meta_mod_mask)
            memset (&compose_status, 0, sizeof (compose_status));

#ifdef HAVE_X_I18N
          if (FRAME_XIC (f))
            {
              Status status_return;

              nbytes = XmbLookupString (FRAME_XIC (f),
                                        &xkey, (char *) copy_bufptr,
                                        copy_bufsiz, &keysym,
                                        &status_return);
	      coding = FRAME_X_XIM_CODING (f);
              if (status_return == XBufferOverflow)
                {
                  copy_bufsiz = nbytes + 1;
                  copy_bufptr = SAFE_ALLOCA (copy_bufsiz);
                  nbytes = XmbLookupString (FRAME_XIC (f),
                                            &xkey, (char *) copy_bufptr,
                                            copy_bufsiz, &keysym,
                                            &status_return);
                }
              /* Xutf8LookupString is a new but already deprecated interface.  -stef  */
              if (status_return == XLookupNone)
                break;
              else if (status_return == XLookupChars)
                {
                  keysym = NoSymbol;
                  modifiers = 0;
                }
              else if (status_return != XLookupKeySym
                       && status_return != XLookupBoth)
                emacs_abort ();
            }
          else
#endif
	    {
#ifdef HAVE_XKB
	      int overflow;
	      unsigned int consumed;
	      KeySym sym;

	      if (dpyinfo->xkb_desc)
		{
		  /* Translate the keycode into the keysym it
		     represents, using STATE.  CONSUMED is set to the
		     modifier bits consumed while undertaking this
		     translation and should be subsequently ignored
		     during keysym translation.  */

		  if (!XkbTranslateKeyCode (dpyinfo->xkb_desc,
					    xkey.keycode, xkey.state,
					    &consumed, &keysym))
		    goto done_keysym;

		  /* Save the original keysym in case
		     XkbTranslateKeysym overflows.  */
		  sym = keysym, overflow = 0;

		  nbytes = XkbTranslateKeySym (dpyinfo->display, &keysym,
					       xkey.state & ~consumed,
					       (char *) copy_bufptr,
					       copy_bufsiz, &overflow);

		  if (overflow)
		    {
		      copy_bufsiz += overflow;
		      copy_bufptr = SAFE_ALLOCA (copy_bufsiz);
		      overflow = 0;

		      /* Use the original keysym derived from the
			 keycode translation in this second call to
			 XkbTranslateKeysym.  */
		      nbytes = XkbTranslateKeySym (dpyinfo->display, &sym,
						   xkey.state & ~consumed,
						   (char *) copy_bufptr,
						   copy_bufsiz, &overflow);

		      if (overflow)
			nbytes = 0;
		    }

		  if (nbytes)
		    coding = Qnil;
		}
	      else
#endif
		nbytes = XLookupString (&xkey, (char *) copy_bufptr,
					copy_bufsiz, &keysym,
					&compose_status);
	    }

#ifdef XK_F1
	  if (x_dnd_in_progress && keysym == XK_F1)
	    {
	      x_dnd_xm_use_help = true;
	      goto done_keysym;
	    }
#endif

	  /* See if keysym should make Emacs quit.  */

	  if (dpyinfo->quit_keysym)
	    {
	      if (keysym == dpyinfo->quit_keysym
		  && (xkey.time - dpyinfo->quit_keysym_time
		      <= 350))
		{
		  Vquit_flag = Qt;
		  goto done_keysym;
		}

	      if (keysym == dpyinfo->quit_keysym)
		{
		  /* Otherwise, set the last time that keysym was
		     pressed.  */
		  dpyinfo->quit_keysym_time = xkey.time;
		  goto done_keysym;
		}
	    }

          /* If not using XIM/XIC, and a compose sequence is in progress,
             we break here.  Otherwise, chars_matched is always 0.  */
          if (compose_status.chars_matched > 0 && nbytes == 0)
            break;

          memset (&compose_status, 0, sizeof (compose_status));
          orig_keysym = keysym;

	  /* Common for all keysym input events.  */
	  XSETFRAME (inev.ie.frame_or_window, f);
	  inev.ie.modifiers
	    = x_x_to_emacs_modifiers (dpyinfo, modifiers);
	  inev.ie.timestamp = xkey.time;

	  /* First deal with keysyms which have defined
	     translations to characters.  */
	  if (keysym >= 32 && keysym < 128)
	    /* Avoid explicitly decoding each ASCII character.  */
	    {
	      inev.ie.kind = ASCII_KEYSTROKE_EVENT;
	      inev.ie.code = keysym;

#ifdef HAVE_XINPUT2
	      if (event->xkey.time == pending_keystroke_time)
		{
		  source = xi_device_from_id (dpyinfo,
					      dpyinfo->pending_keystroke_source);

		  if (source)
		    inev.ie.device = source->name;
		}
#endif

	      goto done_keysym;
	    }

	  /* Keysyms directly mapped to Unicode characters.  */
	  if (keysym >= 0x01000000 && keysym <= 0x0110FFFF)
	    {
	      if (keysym < 0x01000080)
		inev.ie.kind = ASCII_KEYSTROKE_EVENT;
	      else
		inev.ie.kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;
	      inev.ie.code = keysym & 0xFFFFFF;

#ifdef HAVE_XINPUT2
	      if (event->xkey.time == pending_keystroke_time)
		{
		  source = xi_device_from_id (dpyinfo,
					      dpyinfo->pending_keystroke_source);

		  if (source)
		    inev.ie.device = source->name;
		}
#endif

	      goto done_keysym;
	    }

	  /* Now non-ASCII.  */
	  if (HASH_TABLE_P (Vx_keysym_table)
	      && (c = Fgethash (make_fixnum (keysym),
				Vx_keysym_table,
				Qnil),
		  FIXNATP (c)))
	    {
	      inev.ie.kind = (SINGLE_BYTE_CHAR_P (XFIXNAT (c))
                              ? ASCII_KEYSTROKE_EVENT
                              : MULTIBYTE_CHAR_KEYSTROKE_EVENT);
	      inev.ie.code = XFIXNAT (c);

#ifdef HAVE_XINPUT2
	      if (event->xkey.time == pending_keystroke_time)
		{
		  source = xi_device_from_id (dpyinfo,
					      dpyinfo->pending_keystroke_source);

		  if (source)
		    inev.ie.device = source->name;
		}
#endif

	      goto done_keysym;
	    }

	  /* Random non-modifier sorts of keysyms.  */
	  if (((keysym >= XK_BackSpace && keysym <= XK_Escape)
	       || keysym == XK_Delete
#ifdef XK_ISO_Left_Tab
	       || (keysym >= XK_ISO_Left_Tab
		   && keysym <= XK_ISO_Enter)
#endif
	       || IsCursorKey (keysym) /* 0xff50 <= x < 0xff60 */
	       || IsMiscFunctionKey (keysym) /* 0xff60 <= x < VARIES */
#ifdef HPUX
	       /* This recognizes the "extended function
		  keys".  It seems there's no cleaner way.
		  Test IsModifierKey to avoid handling
		  mode_switch incorrectly.  */
	       || (XK_Select <= keysym && keysym < XK_KP_Space)
#endif
#ifdef XK_dead_circumflex
	       || orig_keysym == XK_dead_circumflex
#endif
#ifdef XK_dead_grave
	       || orig_keysym == XK_dead_grave
#endif
#ifdef XK_dead_tilde
	       || orig_keysym == XK_dead_tilde
#endif
#ifdef XK_dead_diaeresis
	       || orig_keysym == XK_dead_diaeresis
#endif
#ifdef XK_dead_macron
	       || orig_keysym == XK_dead_macron
#endif
#ifdef XK_dead_degree
	       || orig_keysym == XK_dead_degree
#endif
#ifdef XK_dead_acute
	       || orig_keysym == XK_dead_acute
#endif
#ifdef XK_dead_cedilla
	       || orig_keysym == XK_dead_cedilla
#endif
#ifdef XK_dead_breve
	       || orig_keysym == XK_dead_breve
#endif
#ifdef XK_dead_ogonek
	       || orig_keysym == XK_dead_ogonek
#endif
#ifdef XK_dead_caron
	       || orig_keysym == XK_dead_caron
#endif
#ifdef XK_dead_doubleacute
	       || orig_keysym == XK_dead_doubleacute
#endif
#ifdef XK_dead_abovedot
	       || orig_keysym == XK_dead_abovedot
#endif
#ifdef XK_dead_abovering
	       || orig_keysym == XK_dead_abovering
#endif
#ifdef XK_dead_belowdot
	       || orig_keysym == XK_dead_belowdot
#endif
#ifdef XK_dead_voiced_sound
	       || orig_keysym == XK_dead_voiced_sound
#endif
#ifdef XK_dead_semivoiced_sound
	       || orig_keysym == XK_dead_semivoiced_sound
#endif
#ifdef XK_dead_hook
	       || orig_keysym == XK_dead_hook
#endif
#ifdef XK_dead_horn
	       || orig_keysym == XK_dead_horn
#endif
#ifdef XK_dead_stroke
	       || orig_keysym == XK_dead_stroke
#endif
#ifdef XK_dead_abovecomma
	       || orig_keysym == XK_dead_abovecomma
#endif
	       || IsKeypadKey (keysym) /* 0xff80 <= x < 0xffbe */
	       || IsFunctionKey (keysym) /* 0xffbe <= x < 0xffe1 */
	       /* Any "vendor-specific" key is ok.  */
	       || (orig_keysym & (1 << 28))
	       || (keysym != NoSymbol && nbytes == 0))
	      && ! (IsModifierKey (orig_keysym)
		    /* The symbols from XK_ISO_Lock
		       to XK_ISO_Last_Group_Lock
		       don't have real modifiers but
		       should be treated similarly to
		       Mode_switch by Emacs. */
#if defined XK_ISO_Lock && defined XK_ISO_Last_Group_Lock
		    || (XK_ISO_Lock <= orig_keysym
			&& orig_keysym <= XK_ISO_Last_Group_Lock)
#endif
		    ))
	    {
	      STORE_KEYSYM_FOR_DEBUG (keysym);
	      /* make_lispy_event will convert this to a symbolic
		 key.  */
	      inev.ie.kind = NON_ASCII_KEYSTROKE_EVENT;
	      inev.ie.code = keysym;

#ifdef HAVE_XINPUT2
	      if (event->xkey.time == pending_keystroke_time)
		{
		  source = xi_device_from_id (dpyinfo,
					      dpyinfo->pending_keystroke_source);

		  if (source)
		    inev.ie.device = source->name;
		}
#endif

	      goto done_keysym;
	    }

	  {	/* Raw bytes, not keysym.  */
	    ptrdiff_t i;

	    for (i = 0; i < nbytes; i++)
	      {
		STORE_KEYSYM_FOR_DEBUG (copy_bufptr[i]);
	      }

	    if (nbytes)
	      {
		inev.ie.kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;
		inev.ie.arg = make_unibyte_string ((char *) copy_bufptr, nbytes);

		Fput_text_property (make_fixnum (0), make_fixnum (nbytes),
				    Qcoding, coding, inev.ie.arg);

#ifdef HAVE_XINPUT2
		if (event->xkey.time == pending_keystroke_time
		    /* I-Bus sometimes sends events generated from
		       multiple filtered keystrokes with a time of 0,
		       so just use the recorded source device if it
		       exists.  */
		    || (pending_keystroke_time && !event->xkey.time))
		  {
		    source = xi_device_from_id (dpyinfo,
						dpyinfo->pending_keystroke_source);

		    if (source)
		      inev.ie.device = source->name;
		  }
#endif
	      }

	    if (keysym == NoSymbol)
	      break;
	  }
        }
    done_keysym:
#ifdef HAVE_X_I18N
      if (f)
	{
	  struct window *w = XWINDOW (f->selected_window);
	  xic_set_preeditarea (w, w->cursor.x, w->cursor.y);

	  if (FRAME_XIC (f) && (FRAME_XIC_STYLE (f) & XIMStatusArea))
            xic_set_statusarea (f);
	}

      /* Don't dispatch this event since XtDispatchEvent calls
         XFilterEvent, and two calls in a row may freeze the
         client.  */
      break;
#else
      goto OTHER;
#endif

    case KeyRelease:
#ifdef HAVE_X_I18N
      /* Don't dispatch this event since XtDispatchEvent calls
         XFilterEvent, and two calls in a row may freeze the
         client.  */
      break;
#else
      goto OTHER;
#endif

    case EnterNotify:
      x_display_set_last_user_time (dpyinfo, event->xcrossing.time,
				    event->xcrossing.send_event,
				    false);

#ifdef HAVE_XWIDGETS
      {
	struct xwidget_view *xvw;
	Mouse_HLInfo *hlinfo;

	xvw = xwidget_view_from_window (event->xcrossing.window);

	if (xvw)
	  {
	    xwidget_motion_or_crossing (xvw, event);
	    hlinfo = MOUSE_HL_INFO (xvw->frame);

	    if (xvw->frame == hlinfo->mouse_face_mouse_frame)
	      {
		clear_mouse_face (hlinfo);
		hlinfo->mouse_face_mouse_frame = 0;
		x_flush_dirty_back_buffer_on (xvw->frame);
	      }

	    if (any_help_event_p)
	      do_help = -1;

	    goto OTHER;
	  }
      }
#endif

      /* Apply the fix for bug#57468 on GTK 3.x and no toolkit builds,
	 but not GTK+ 2.x and X toolkit builds, where it is required
	 to treat implicit focus correctly.  (bug#65919) */
#if defined USE_X_TOOLKIT || (defined USE_GTK && !defined HAVE_GTK3)
      if (x_top_window_to_frame (dpyinfo, event->xcrossing.window))
	x_detect_focus_change (dpyinfo, any, event, &inev.ie);
#endif /* defined USE_X_TOOLKIT || (defined USE_GTK && !defined HAVE_GTK3) */

#ifdef HAVE_XINPUT2
      /* For whatever reason, the X server continues to deliver
	 EnterNotify and LeaveNotify events despite us selecting for
	 related XI_Enter and XI_Leave events.  It's not just our
	 problem, since windows created by "xinput test-xi2" suffer
	 from the same defect.  Simply ignore all such events while
	 the input extension is enabled.  (bug#57468) */

      if (dpyinfo->supports_xi2)
	goto OTHER;
#endif /* HAVE_XINPUT2 */

      /* Apply the fix for bug#57468 on GTK 3.x and no toolkit
	 builds.  */
#if !defined USE_X_TOOLKIT || (!defined USE_GTK || defined HAVE_GTK3)
      if (x_top_window_to_frame (dpyinfo, event->xcrossing.window))
	x_detect_focus_change (dpyinfo, any, event, &inev.ie);
#endif /* !defined USE_X_TOOLKIT || (!defined USE_GTK || defined HAVE_GTK3) */

      f = any;

      if (f && event->xcrossing.window == FRAME_X_WINDOW (f))
	x_compute_root_window_offset (f, event->xcrossing.x_root,
				      event->xcrossing.y_root,
				      event->xcrossing.x,
				      event->xcrossing.y);

      /* The code below relies on the first several fields of
	 XCrossingEvent being laid out the same way as
	 XMotionEvent.  */

      if (f && x_mouse_click_focus_ignore_position)
	{
	  ignore_next_mouse_click_timeout = (event->xmotion.time
					     + x_mouse_click_focus_ignore_time);
	  mouse_click_timeout_display = dpyinfo;
	}

      /* EnterNotify counts as mouse movement,
	 so update things that depend on mouse position.  */
      if (f && !f->output_data.x->hourglass_p)
	x_note_mouse_movement (f, &event->xmotion, Qnil);
#ifdef USE_GTK
      /* We may get an EnterNotify on the buttons in the toolbar.  In that
         case we moved out of any highlighted area and need to note this.  */
      if (!f && dpyinfo->last_mouse_glyph_frame)
        x_note_mouse_movement (dpyinfo->last_mouse_glyph_frame, &event->xmotion,
			       Qnil);
#endif
      goto OTHER;

    case FocusIn:
#if defined HAVE_XINPUT2						\
  && (defined HAVE_GTK3 || (!defined USE_GTK && !defined USE_X_TOOLKIT))
      /* If a FocusIn event is received (because the window manager
	 sent us one), don't set the core focus if XInput 2 is
	 enabled, since that would mess up the device-specific focus
	 tracking.

	 The long looking preprocessor conditional only enables this
	 code on GTK 3 and no toolkit builds, since those are the only
	 builds where focus is tracked specific to each master device.
	 Other builds use core events and the client pointer to handle
	 focus, much like on a build without XInput 2.  */
      if (dpyinfo->supports_xi2)
	goto OTHER;
#endif
#ifdef USE_GTK
      /* Some WMs (e.g. Mutter in Gnome Shell), don't unmap
         minimized/iconified windows; thus, for those WMs we won't get
         a MapNotify when unminimizing/deiconifying.  Check here if we
         are deiconizing a window (Bug42655).

	 But don't do that by default on GTK since it may cause a plain
	 invisible frame get reported as iconified, compare
	 https://lists.gnu.org/archive/html/emacs-devel/2017-02/msg00133.html.
	 That is fixed above but bites us here again.

	 The option x_set_frame_visibility_more_laxly enables
	 overriding the default behavior (Bug#49955, Bug#53298).  */
      if (EQ (x_set_frame_visibility_more_laxly, Qfocus_in)
	  || EQ (x_set_frame_visibility_more_laxly, Qt))
#endif /* USE_GTK */
	{
	  f = any;
	  if (f && FRAME_ICONIFIED_P (f))
	    {
	      SET_FRAME_VISIBLE (f, 1);
	      SET_FRAME_ICONIFIED (f, false);
	      f->output_data.x->has_been_visible = true;
	      inev.ie.kind = DEICONIFY_EVENT;
	      XSETFRAME (inev.ie.frame_or_window, f);
	    }
	}

      x_detect_focus_change (dpyinfo, any, event, &inev.ie);
      goto OTHER;

    case LeaveNotify:
      x_display_set_last_user_time (dpyinfo, event->xcrossing.time,
				    event->xcrossing.send_event, false);

      /* Apply the fix for bug#57468 on GTK 3.x and no toolkit builds,
	 but not GTK+ 2.x and X toolkit builds, where it is required
	 to treat implicit focus correctly.  */
#if defined USE_X_TOOLKIT || (defined USE_GTK && !defined HAVE_GTK3)
      if (x_top_window_to_frame (dpyinfo, event->xcrossing.window))
	x_detect_focus_change (dpyinfo, any, event, &inev.ie);
#endif /* defined USE_X_TOOLKIT || (defined USE_GTK && !defined HAVE_GTK3) */

#ifdef HAVE_XINPUT2
      /* For whatever reason, the X server continues to deliver
	 EnterNotify and LeaveNotify events despite us selecting for
	 related XI_Enter and XI_Leave events.  It's not just our
	 problem, since windows created by "xinput test-xi2" suffer
	 from the same defect.  Simply ignore all such events while
	 the input extension is enabled.  (bug#57468) */

      if (dpyinfo->supports_xi2)
	{
#if !defined USE_X_TOOLKIT && (!defined USE_GTK || defined HAVE_GTK3)
	  goto OTHER;
#else /* USE_X_TOOLKIT || (USE_GTK && !HAVE_GTK3) */
	  /* Unfortunately, X toolkit popups generate LeaveNotify
	     events due to the core grabs they acquire (and our
	     releasing of the device grab).  This leads to the mouse
	     face persisting if a popup is activated by clicking on a
	     button, and then dismissed by releasing the mouse button
	     outside the frame, in which case no XI_Enter event is
	     generated for the grab.  */
	  goto just_clear_mouse_face;
#endif /* !USE_X_TOOLKIT && (!USE_GTK || HAVE_GTK3) */
	}
#endif /* HAVE_XINPUT2 */

      /* Apply the fix for bug#57468 on GTK 3.x and no toolkit
	 builds.  */
#if !defined USE_X_TOOLKIT || (!defined USE_GTK || defined HAVE_GTK3)
      if (x_top_window_to_frame (dpyinfo, event->xcrossing.window))
	x_detect_focus_change (dpyinfo, any, event, &inev.ie);
#endif /* !defined USE_X_TOOLKIT || (!defined USE_GTK || defined HAVE_GTK3) */

#ifdef HAVE_XWIDGETS
      {
	struct xwidget_view *xvw;

	xvw = xwidget_view_from_window (event->xcrossing.window);

	if (xvw)
	  {
	    xwidget_motion_or_crossing (xvw, event);
	    goto OTHER;
	  }
      }
#endif

#if defined HAVE_XINPUT2						\
  && (defined USE_X_TOOLKIT || (defined USE_GTK && !defined HAVE_GTK3))
    just_clear_mouse_face:
#endif

#if defined USE_X_TOOLKIT
      /* If the mouse leaves the edit widget, then any mouse highlight
	 should be cleared.  */
      f = x_window_to_frame (dpyinfo, event->xcrossing.window);

      if (!f)
	f = x_top_window_to_frame (dpyinfo, event->xcrossing.window);
#else
      f = x_top_window_to_frame (dpyinfo, event->xcrossing.window);
#endif

      if (f)
        {
	  /* Now clear dpyinfo->last_mouse_motion_frame, or
	     gui_redo_mouse_highlight will end up highlighting the
	     last known position of the mouse if a tooltip frame is
	     later unmapped.  */

	  if (f == dpyinfo->last_mouse_motion_frame)
	    dpyinfo->last_mouse_motion_frame = NULL;

	  /* Something similar applies to
	     dpyinfo->last_mouse_glyph_frame.  */
	  if (f == dpyinfo->last_mouse_glyph_frame)
	    dpyinfo->last_mouse_glyph_frame = NULL;

          if (f == hlinfo->mouse_face_mouse_frame)
            {
              /* If we move outside the frame, then we're
                 certainly no longer on any text in the frame.  */
              clear_mouse_face (hlinfo);
              hlinfo->mouse_face_mouse_frame = 0;
	      x_flush_dirty_back_buffer_on (f);
            }

          /* Generate a nil HELP_EVENT to cancel a help-echo.
             Do it only if there's something to cancel.
             Otherwise, the startup message is cleared when
             the mouse leaves the frame.  */
          if (any_help_event_p
	      /* But never if `mouse-drag-and-drop-region' is in
		 progress, since that results in the tooltip being
		 dismissed when the mouse moves on top.  */
	      && !((EQ (track_mouse, Qdrag_source)
		    || EQ (track_mouse, Qdropping))
		   && gui_mouse_grabbed (dpyinfo)))
	    do_help = -1;

	  if (event->xcrossing.window == FRAME_X_WINDOW (f))
	    x_compute_root_window_offset (f, event->xcrossing.x_root,
					  event->xcrossing.y_root,
					  event->xcrossing.x,
					  event->xcrossing.y);

        }
#ifdef USE_GTK
      /* See comment in EnterNotify above */
      else if (dpyinfo->last_mouse_glyph_frame)
        x_note_mouse_movement (dpyinfo->last_mouse_glyph_frame,
			       &event->xmotion, Qnil);
#endif
      goto OTHER;

    case FocusOut:
#if defined HAVE_XINPUT2						\
  && (defined HAVE_GTK3 || (!defined USE_GTK && !defined USE_X_TOOLKIT))
      /* If a FocusIn event is received (because the window manager
	 sent us one), don't set the core focus if XInput 2 is
	 enabled, since that would mess up the device-specific focus
	 tracking.

	 The long looking preprocessor conditional only enables this
	 code on GTK 3 and no toolkit builds, since those are the only
	 builds where focus is tracked specific to each master device.
	 Other builds use core events and the client pointer to handle
	 focus, much like on a build without XInput 2.  */
      if (dpyinfo->supports_xi2)
	goto OTHER;
#endif
      x_detect_focus_change (dpyinfo, any, event, &inev.ie);
      goto OTHER;

    case MotionNotify:
      {
	XMotionEvent xmotion = event->xmotion;

        previous_help_echo_string = help_echo_string;
        help_echo_string = Qnil;

	if (hlinfo->mouse_face_hidden)
	  {
            hlinfo->mouse_face_hidden = false;
            clear_mouse_face (hlinfo);
          }

	f = mouse_or_wdesc_frame (dpyinfo, event->xmotion.window);

	if (f && event->xmotion.window == FRAME_X_WINDOW (f))
	  /* See the comment above x_compute_root_window_offset for
	     why this optimization is performed.  */
	  x_compute_root_window_offset (f, event->xmotion.x_root,
					event->xmotion.y_root,
					event->xmotion.x,
					event->xmotion.y);

	if (x_dnd_in_progress
	    /* Handle these events normally if the recursion
	       level is higher than when the drag-and-drop
	       operation was initiated.  This is so that mouse
	       input works while we're in the debugger for, say,
	       `x-dnd-movement-function`.  */
	    && (command_loop_level + minibuf_level
		<= x_dnd_recursion_depth)
	    && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
	  {
	    Window target, toplevel;
	    int target_proto, motif_style;
	    xm_top_level_leave_message lmsg;
	    xm_top_level_enter_message emsg;
	    xm_drag_motion_message dmsg;
	    XRectangle *r;
	    bool was_frame;

	    /* Always clear mouse face.  */
	    clear_mouse_face (hlinfo);
	    hlinfo->mouse_face_hidden = true;

	    /* Sometimes the drag-and-drop operation starts with the
	       pointer of a frame invisible due to input.  Since
	       motion events are ignored during that, make the pointer
	       visible manually.  */

	    if (f)
	      {
		XTtoggle_invisible_pointer (f, false);

		r = &dpyinfo->last_mouse_glyph;

		/* Also remember the mouse glyph and set
		   mouse_moved.  */
		if (f != dpyinfo->last_mouse_glyph_frame
		    || event->xmotion.x < r->x
		    || event->xmotion.x >= r->x + r->width
		    || event->xmotion.y < r->y
		    || event->xmotion.y >= r->y + r->height)
		  {
		    f->mouse_moved = true;
		    f->last_mouse_device = Qnil;
		    dpyinfo->last_mouse_scroll_bar = NULL;

		    remember_mouse_glyph (f, event->xmotion.x,
					  event->xmotion.y, r);
		    dpyinfo->last_mouse_glyph_frame = f;
		  }
	      }

	    if (event->xmotion.same_screen)
	      target = x_dnd_get_target_window (dpyinfo,
						event->xmotion.x_root,
						event->xmotion.y_root,
						&target_proto,
						&motif_style, &toplevel,
						&was_frame);
	    else
	      target = x_dnd_fill_empty_target (&target_proto, &motif_style,
						&toplevel, &was_frame);

	    if (toplevel != x_dnd_last_seen_toplevel)
	      {
		if (toplevel != FRAME_OUTER_WINDOW (x_dnd_frame)
		    && x_dnd_return_frame == 1)
		  x_dnd_return_frame = 2;

		if (x_dnd_return_frame == 2
		    && x_any_window_to_frame (dpyinfo, toplevel))
		  {
		    if (x_dnd_last_seen_window != None
			&& x_dnd_last_protocol_version != -1
			&& x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
		      x_dnd_send_leave (x_dnd_frame, x_dnd_last_seen_window,
					x_dnd_last_seen_toplevel);
		    else if (x_dnd_last_seen_window != None
			     && XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style)
			     && !x_dnd_disable_motif_drag
			     && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
		      {
			if (!x_dnd_motif_setup_p)
			  xm_setup_drag_info (dpyinfo, x_dnd_frame);

			lmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
						      XM_DRAG_REASON_TOP_LEVEL_LEAVE);
			lmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
			lmsg.zero = 0;
			lmsg.timestamp = event->xmotion.time;
			lmsg.source_window = FRAME_X_WINDOW (x_dnd_frame);

			if (x_dnd_motif_setup_p)
			  xm_send_top_level_leave_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
							   x_dnd_last_seen_window, &lmsg);
		      }

		    x_dnd_end_window = x_dnd_last_seen_window;
		    x_dnd_last_seen_window = None;
		    x_dnd_last_seen_toplevel = None;
		    x_dnd_in_progress = false;
		    x_dnd_return_frame_object
		      = x_any_window_to_frame (dpyinfo, toplevel);
		    x_dnd_return_frame = 3;
		    x_dnd_waiting_for_finish = false;
		    target = None;
		  }
	      }

	    if (target != x_dnd_last_seen_window)
	      {
		if (x_dnd_last_seen_window != None
		    && x_dnd_last_protocol_version != -1
		    && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
		  x_dnd_send_leave (x_dnd_frame, x_dnd_last_seen_window,
				    x_dnd_last_seen_toplevel);
		else if (x_dnd_last_seen_window != None
			 && XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style)
			 && x_dnd_disable_motif_drag
			 && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
		  {
		    if (!x_dnd_motif_setup_p)
		      xm_setup_drag_info (dpyinfo, x_dnd_frame);

		    /* This is apparently required.  If we don't send
		       a motion event with the current root window
		       coordinates of the pointer before the top level
		       leave, then Motif displays an ugly black border
		       around the previous drop site.  */

		    dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
						  XM_DRAG_REASON_DRAG_MOTION);
		    dmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
		    dmsg.side_effects = XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (dpyinfo,
											 x_dnd_wanted_action),
							     XM_DROP_SITE_NONE, x_dnd_motif_operations,
							     XM_DROP_ACTION_DROP_CANCEL);
		    dmsg.timestamp = event->xmotion.time;
		    dmsg.x = event->xmotion.x_root;
		    dmsg.y = event->xmotion.y_root;

		    lmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
						  XM_DRAG_REASON_TOP_LEVEL_LEAVE);
		    lmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
		    lmsg.zero = 0;
		    lmsg.timestamp = event->xbutton.time;
		    lmsg.source_window = FRAME_X_WINDOW (x_dnd_frame);

		    if (x_dnd_motif_setup_p)
		      {
			xm_send_drag_motion_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
						     x_dnd_last_seen_window, &dmsg);
			xm_send_top_level_leave_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
							 x_dnd_last_seen_window, &lmsg);
		      }
		  }

		x_dnd_action = None;
		x_dnd_last_seen_toplevel = toplevel;
		x_dnd_last_seen_window = target;
		x_dnd_last_protocol_version = target_proto;
		x_dnd_last_motif_style = motif_style;
		x_dnd_last_window_is_frame = was_frame;

		if (target != None && x_dnd_last_protocol_version != -1)
		  x_dnd_send_enter (x_dnd_frame, target,
				    x_dnd_last_seen_toplevel,
				    x_dnd_last_protocol_version);
		else if (target != None && XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style)
			 && !x_dnd_disable_motif_drag)
		  {
		    if (!x_dnd_motif_setup_p)
		      xm_setup_drag_info (dpyinfo, x_dnd_frame);

		    emsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
						  XM_DRAG_REASON_TOP_LEVEL_ENTER);
		    emsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
		    emsg.zero = 0;
		    emsg.timestamp = event->xbutton.time;
		    emsg.source_window = FRAME_X_WINDOW (x_dnd_frame);
		    emsg.index_atom = x_dnd_motif_atom;

		    if (x_dnd_motif_setup_p)
		      xm_send_top_level_enter_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
						       target, &emsg);
		  }
	      }
	    else
	      x_dnd_last_seen_toplevel = toplevel;

	    if (x_dnd_last_window_is_frame && target != None)
	      x_dnd_note_self_position (dpyinfo, target,
					event->xbutton.x_root,
					event->xbutton.y_root);
	    else if (x_dnd_last_protocol_version != -1 && target != None)
	      x_dnd_send_position (x_dnd_frame, target,
				   x_dnd_last_seen_toplevel,
				   x_dnd_last_protocol_version,
				   event->xmotion.x_root,
				   event->xmotion.y_root,
				   x_dnd_selection_timestamp,
				   x_dnd_wanted_action, 0,
				   event->xmotion.state);
	    else if (XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style) && target != None
		     && !x_dnd_disable_motif_drag)
	      {
		if (!x_dnd_motif_setup_p)
		  xm_setup_drag_info (dpyinfo, x_dnd_frame);

		dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
					      XM_DRAG_REASON_DRAG_MOTION);
		dmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
		dmsg.side_effects = XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (dpyinfo,
										     x_dnd_wanted_action),
							 XM_DROP_SITE_VALID, x_dnd_motif_operations,
							 (!x_dnd_xm_use_help
							  ? XM_DROP_ACTION_DROP
							  : XM_DROP_ACTION_DROP_HELP));
		dmsg.timestamp = event->xbutton.time;
		dmsg.x = event->xmotion.x_root;
		dmsg.y = event->xmotion.y_root;

		if (x_dnd_motif_setup_p)
		  xm_send_drag_motion_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
					       target, &dmsg);
	      }

	    x_dnd_update_tooltip_position (event->xmotion.x_root,
					   event->xmotion.y_root);

	    goto OTHER;
	  }

#ifdef USE_GTK
        if (f && xg_event_is_for_scrollbar (f, event, false))
          f = 0;
#endif
#ifdef HAVE_XWIDGETS
	struct xwidget_view *xvw;

	xvw = xwidget_view_from_window (event->xmotion.window);

	if (xvw)
	  xwidget_motion_or_crossing (xvw, event);
#endif
        if (f)
          {
	    /* Maybe generate a SELECT_WINDOW_EVENT for
	       `mouse-autoselect-window' but don't let popup menus
	       interfere with this (Bug#1261).  */
            if (!NILP (Vmouse_autoselect_window)
		&& !popup_activated ()
		/* Don't switch if we're currently in the minibuffer.
		   This tries to work around problems where the
		   minibuffer gets unselected unexpectedly, and where
		   you then have to move your mouse all the way down to
		   the minibuffer to select it.  */
		&& !MINI_WINDOW_P (XWINDOW (selected_window))
		/* With `focus-follows-mouse' non-nil create an event
		   also when the target window is on another frame.  */
		&& (f == XFRAME (selected_frame)
		    || !NILP (focus_follows_mouse)))
	      {
		if (xmotion.window != FRAME_X_WINDOW (f))
		  {
		    x_translate_coordinates (f, xmotion.x_root, xmotion.y_root,
					     &xmotion.x, &xmotion.y);
		    xmotion.window = FRAME_X_WINDOW (f);
		  }

		Lisp_Object window = window_from_coordinates
		  (f, xmotion.x, xmotion.y, 0, false, false, false);

		/* A window will be autoselected only when it is not
		   selected now and the last mouse movement event was
		   not in it.  The remainder of the code is a bit vague
		   wrt what a "window" is.  For immediate autoselection,
		   the window is usually the entire window but for GTK
		   where the scroll bars don't count.  For delayed
		   autoselection the window is usually the window's text
		   area including the margins.  */
		if (WINDOWP (window)
		    && !EQ (window, last_mouse_window)
		    && !EQ (window, selected_window))
		  {
		    inev.ie.kind = SELECT_WINDOW_EVENT;
		    inev.ie.frame_or_window = window;
		  }

		/* Remember the last window where we saw the mouse.  */
		last_mouse_window = window;
	      }

            if (!x_note_mouse_movement (f, &xmotion, Qnil))
	      help_echo_string = previous_help_echo_string;
          }
        else
          {
#ifndef USE_TOOLKIT_SCROLL_BARS
            struct scroll_bar *bar
              = x_window_to_scroll_bar (event->xmotion.display,
                                        event->xmotion.window, 2);

            if (bar)
              x_scroll_bar_note_movement (bar, &event->xmotion);
#endif /* USE_TOOLKIT_SCROLL_BARS */

            /* If we move outside the frame, then we're
               certainly no longer on any text in the frame.  */
            clear_mouse_face (hlinfo);
          }

        /* If the contents of the global variable help_echo_string
           has changed, generate a HELP_EVENT.  */
        if (!NILP (help_echo_string)
            || !NILP (previous_help_echo_string))
	  do_help = 1;

	if (f)
	  x_flush_dirty_back_buffer_on (f);
        goto OTHER;
      }

    case ConfigureNotify:
      /* An opaque move can generate a stream of events as the window
         is dragged around.  If the connection round trip time isn't
         really short, they may come faster than we can respond to
         them, given the multiple queries we can do to check window
         manager state, translate coordinates, etc.

         So if this ConfigureNotify is immediately followed by another
         for the same window, use the info from the latest update, and
         consider the events all handled.  */

      /* Opaque resize may be trickier; ConfigureNotify events are
         mixed with Expose events for multiple windows.  */
      configureEvent = *event;
      while (XPending (dpyinfo->display))
        {
          XNextEvent (dpyinfo->display, &next_event);
          if (next_event.type != ConfigureNotify
              || next_event.xconfigure.window != event->xconfigure.window
              /* Skipping events with different sizes can lead to a
                 mispositioned mode line at initial window creation.
                 Only drop window motion events for now.  */
              || next_event.xconfigure.width != event->xconfigure.width
              || next_event.xconfigure.height != event->xconfigure.height)
            {
              XPutBackEvent (dpyinfo->display, &next_event);
              break;
            }
          else
	    configureEvent = next_event;
        }

      /* If we get a ConfigureNotify for the root window, this means
	 the dimensions of the screen it's on changed.  */

      if (configureEvent.xconfigure.window == dpyinfo->root_window)
	{
#ifdef HAVE_XRANDR
	  /* This function is OK to call even if the X server doesn't
	     support RandR.  */
	  XRRUpdateConfiguration (&configureEvent);
#elif !defined USE_GTK
	  /* Catch screen size changes even if RandR is not available
	     on the client.  GTK does this internally.  */

	  if (configureEvent.xconfigure.width != dpyinfo->screen_width
	      || configureEvent.xconfigure.height != dpyinfo->screen_height)
	    {
	      /* Also avoid storing duplicate events here, since
		 Fx_display_monitor_attributes_list will return the
		 same information for both invocations of the
		 hook.  */
	      if (!x_find_monitors_changed_event (dpyinfo))
		{
		  inev.ie.kind = MONITORS_CHANGED_EVENT;
		  XSETTERMINAL (inev.ie.arg, dpyinfo->terminal);

		  /* Store this event now since inev.ie.type could be set to
		     MOVE_FRAME_EVENT later.  */
		  kbd_buffer_store_event (&inev.ie);
		  inev.ie.kind = NO_EVENT;
		}

	      /* Also update the position of the drag-and-drop
		 tooltip.  */
	      x_dnd_update_tooltip_now ();
	    }
#endif

	  dpyinfo->screen_width = configureEvent.xconfigure.width;
	  dpyinfo->screen_height = configureEvent.xconfigure.height;
	}

      if (x_dnd_in_progress && x_dnd_use_toplevels
	  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
	{
	  int rc, dest_x, dest_y;
	  Window child;
	  struct x_client_list_window *tem, *last = NULL;

	  for (tem = x_dnd_toplevels; tem; last = tem, tem = tem->next)
	    {
	      /* Not completely right, since the parent could be
		 unmapped, but good enough.  */

	      if (tem->window == configureEvent.xconfigure.window)
		{
		  x_catch_errors (dpyinfo->display);
		  rc = (XTranslateCoordinates (dpyinfo->display,
					       configureEvent.xconfigure.window,
					       dpyinfo->root_window,
					       -configureEvent.xconfigure.border_width,
					       -configureEvent.xconfigure.border_width,
					       &dest_x, &dest_y, &child)
			&& !x_had_errors_p (dpyinfo->display));
		  x_uncatch_errors_after_check ();

		  if (rc)
		    {
		      tem->x = dest_x;
		      tem->y = dest_y;
		      tem->width = (configureEvent.xconfigure.width
				    + configureEvent.xconfigure.border_width);
		      tem->height = (configureEvent.xconfigure.height
				     + configureEvent.xconfigure.border_width);
		    }
		  else
		    {
		      /* The window was probably destroyed, so get rid
			 of it.  */

		      if (!last)
			x_dnd_toplevels = tem->next;
		      else
			last->next = tem->next;

#ifdef HAVE_XSHAPE
		      if (tem->n_input_rects != -1)
			xfree (tem->input_rects);
		      if (tem->n_bounding_rects != -1)
			xfree (tem->bounding_rects);
#endif
		      xfree (tem);
		    }

		  break;
		}
	    }
	}

#if defined HAVE_GTK3 && defined USE_TOOLKIT_SCROLL_BARS
      struct scroll_bar *bar = x_window_to_scroll_bar (dpyinfo->display,
						       configureEvent.xconfigure.window, 2);

      /* There is really no other way to make GTK scroll bars fit
	 in the dimensions we want them to.  */
      if (bar)
	{
	  /* Skip all the pending configure events, not just the
	     ones where window motion occurred.  */
	  while (XPending (dpyinfo->display))
	    {
	      XNextEvent (dpyinfo->display, &next_event);
	      if (next_event.type != ConfigureNotify
		  || next_event.xconfigure.window != event->xconfigure.window)
		{
		  XPutBackEvent (dpyinfo->display, &next_event);
		  break;
		}
	      else
		configureEvent = next_event;
	    }

	  if (configureEvent.xconfigure.width != max (bar->width, 1)
	      || configureEvent.xconfigure.height != max (bar->height, 1))
	    {
	      XResizeWindow (dpyinfo->display, bar->x_window,
			     max (bar->width, 1), max (bar->height, 1));
	      x_flush (WINDOW_XFRAME (XWINDOW (bar->window)));
	    }

#ifdef HAVE_XDBE
	  if (f && FRAME_X_DOUBLE_BUFFERED_P (f))
	    x_drop_xrender_surfaces (f);
#endif

	  goto OTHER;
	}
#endif

      /* XXX: it is strictly only necessary to provide the edit window
	 to many of the statements below which only modify or invalidate
	 resources assigned there.  Most conditionals that alternate
	 between `f' and `any' could ideally be removed.  */
      f = x_top_window_to_frame (dpyinfo, configureEvent.xconfigure.window);

      /* This means we can no longer be certain of the root window
	 coordinates of any's edit window.  */
      if (any)
	FRAME_X_OUTPUT (any)->window_offset_certain_p = false;

      /* Unfortunately, we need to call x_drop_xrender_surfaces for
         _all_ ConfigureNotify events, otherwise we miss some and
         flicker.  Don't try to optimize these calls by looking only
         for size changes: that's not sufficient.  We miss some
         surface invalidations and flicker.  */
#ifdef HAVE_XDBE
      {
#if defined USE_GTK || defined USE_X_TOOLKIT
	/* Only modifications to the edit window (on which pictures are
	   created) must be accompanied by invalidations.  (bug#77988) */
	struct frame *f
	  = x_window_to_frame (dpyinfo, configureEvent.xconfigure.window);
#endif /* USE_GTK || USE_X_TOOLKIT */
	if (f && FRAME_X_DOUBLE_BUFFERED_P (f))
	  x_drop_xrender_surfaces (f);
      }
#endif
#if defined USE_CAIRO && !defined USE_GTK
      if (f)
	x_cr_update_surface_desired_size (f, configureEvent.xconfigure.width,
					  configureEvent.xconfigure.height);
      else if (any && configureEvent.xconfigure.window == FRAME_X_WINDOW (any))
	x_cr_update_surface_desired_size (any,
					  configureEvent.xconfigure.width,
					  configureEvent.xconfigure.height);
#endif

#if !defined USE_X_TOOLKIT && !defined USE_GTK

      /* Make the new size of the frame its opaque region.  This is a
	 region describing areas of the window which are always
	 guaranteed to be completely opaque and can be treated as such
	 by the compositor.  It is set to the width and height of the
	 only window in no-toolkit builds when `alpha_background' is
	 not set, and is cleared otherwise.  */

      if (f || (any && configureEvent.xconfigure.window
		== FRAME_OUTER_WINDOW (any)))
	x_update_opaque_region (f ? f : any, &configureEvent);

#endif /* !defined USE_X_TOOLKIT && !defined USE_GTK */

#ifdef USE_GTK
      if (!f
	  && (f = any)
	  && configureEvent.xconfigure.window == FRAME_X_WINDOW (f)
	  && (FRAME_VISIBLE_P (f)
	      || !(configureEvent.xconfigure.width <= 1
		   && configureEvent.xconfigure.height <= 1)))
        {

	  if (CONSP (frame_size_history))
	    frame_size_history_extra
	      (f, build_string ("ConfigureNotify"),
	       FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f),
	       configureEvent.xconfigure.width,
	       configureEvent.xconfigure.height,
	       f->new_width, f->new_height);

#ifdef HAVE_XDBE
          if (FRAME_X_DOUBLE_BUFFERED_P (f))
            x_drop_xrender_surfaces (f);
#endif
          xg_frame_resized (f, configureEvent.xconfigure.width,
                            configureEvent.xconfigure.height);
#ifdef USE_CAIRO
	  x_cr_update_surface_desired_size (f, configureEvent.xconfigure.width,
					    configureEvent.xconfigure.height);
#endif
	  x_update_opaque_region (f, &configureEvent);
          f = 0;
	}
#endif
      if (f && (FRAME_VISIBLE_P (f)
		|| !(configureEvent.xconfigure.width <= 1
		     && configureEvent.xconfigure.height <= 1)))
	{
#ifdef USE_GTK
	  /* For GTK+ don't call x_net_wm_state for the scroll bar
	     window.  (Bug#24963, Bug#25887) */
	  if (configureEvent.xconfigure.window == FRAME_X_WINDOW (f))
#endif
	    x_net_wm_state (f, configureEvent.xconfigure.window);

#if defined USE_X_TOOLKIT || defined USE_GTK
          /* Tip frames are pure X window, set size for them.  */
          if (FRAME_TOOLTIP_P (f))
            {
              if (FRAME_PIXEL_HEIGHT (f) != configureEvent.xconfigure.height
                  || FRAME_PIXEL_WIDTH (f) != configureEvent.xconfigure.width)
		SET_FRAME_GARBAGED (f);

              FRAME_PIXEL_HEIGHT (f) = configureEvent.xconfigure.height;
              FRAME_PIXEL_WIDTH (f) = configureEvent.xconfigure.width;
            }
#endif

#ifndef USE_X_TOOLKIT
#ifndef USE_GTK
          int width = configureEvent.xconfigure.width;
          int height = configureEvent.xconfigure.height;

	  if (CONSP (frame_size_history))
	    frame_size_history_extra
	      (f, build_string ("ConfigureNotify"),
	       FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f),
	       width, height, f->new_width, f->new_height);

	  /* In the toolkit version, change_frame_size
             is called by the code that handles resizing
             of the EmacsFrame widget.  */

          /* Even if the number of character rows and columns has
             not changed, the font size may have changed, so we need
             to check the pixel dimensions as well.  */

          if (width != FRAME_PIXEL_WIDTH (f)
              || height != FRAME_PIXEL_HEIGHT (f)
	      || (f->new_size_p
		  && ((f->new_width >= 0 && width != f->new_width)
		      || (f->new_height >= 0 && height != f->new_height))))
            {
              change_frame_size (f, width, height, false, true, false);
              x_clear_under_internal_border (f);
              SET_FRAME_GARBAGED (f);
              cancel_mouse_face (f);
            }
#endif /* not USE_GTK */
#endif

#ifdef USE_GTK
          /* GTK creates windows but doesn't map them.
             Only get real positions when mapped.  */
          if (FRAME_GTK_OUTER_WIDGET (f)
              && gtk_widget_get_mapped (FRAME_GTK_OUTER_WIDGET (f)))
#endif
	    {
	      int old_left = f->left_pos;
	      int old_top = f->top_pos;
	      Lisp_Object frame;

	      XSETFRAME (frame, f);

	      if (!FRAME_PARENT_FRAME (f))
		x_real_positions (f, &f->left_pos, &f->top_pos);
	      else
		{
		  Window root;
		  unsigned int dummy_uint;

		  XGetGeometry (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
				&root, &f->left_pos, &f->top_pos,
				&dummy_uint, &dummy_uint, &dummy_uint, &dummy_uint);
		}

	      if (!FRAME_TOOLTIP_P (f)
		  && (old_left != f->left_pos || old_top != f->top_pos))
		{
		  inev.ie.kind = MOVE_FRAME_EVENT;
		  XSETFRAME (inev.ie.frame_or_window, f);
		}
	    }


#ifdef HAVE_X_I18N
	  if (f)
	    {
	      if (FRAME_XIC (f) && (FRAME_XIC_STYLE (f) & XIMStatusArea))
		xic_set_statusarea (f);

	      struct window *w = XWINDOW (f->selected_window);
	      xic_set_preeditarea (w, w->cursor.x, w->cursor.y);
	    }
#endif

#ifdef HAVE_XINPUT2
	  if (f && dpyinfo->supports_xi2)
	    {
	      Mouse_HLInfo *hlinfo;

	      /* The input extension doesn't report motion events when
		 the part of the window below the pointer changes.  To
		 avoid outdated information from keeping
		 i.e. mouse-highlight at the wrong position after the
		 frame is moved or resized, reset the mouse highlight
		 and last_mouse_motion_frame.  */

	      if (dpyinfo->last_mouse_motion_frame == f)
		dpyinfo->last_mouse_motion_frame = NULL;

	      hlinfo = MOUSE_HL_INFO (f);

	      if (hlinfo->mouse_face_mouse_frame == f)
		reset_mouse_highlight (hlinfo);
	    }
#endif

	}

      if (x_dnd_in_progress
	  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
	x_dnd_update_state (dpyinfo, dpyinfo->last_user_time);
      goto OTHER;

    case ButtonRelease:
    case ButtonPress:
      {
	if (event->xbutton.type == ButtonPress)
	  x_display_set_last_user_time (dpyinfo, event->xbutton.time,
					event->xbutton.send_event, true);

#ifdef HAVE_XWIDGETS
	struct xwidget_view *xvw;

	xvw = xwidget_view_from_window (event->xbutton.window);

	if (xvw)
	  {
	    xwidget_button (xvw, event->type == ButtonPress,
			    event->xbutton.x, event->xbutton.y,
			    event->xbutton.button, event->xbutton.state,
			    event->xbutton.time);

            if (!EQ (selected_window, xvw->w) && (event->xbutton.button < 4))
              {
		inev.ie.kind = SELECT_WINDOW_EVENT;
		inev.ie.frame_or_window = xvw->w;
	      }

	    *finish = X_EVENT_DROP;
	    goto OTHER;
	  }
#endif
        /* If we decide we want to generate an event to be seen
           by the rest of Emacs, we put it here.  */
        Lisp_Object tab_bar_arg = Qnil;
        bool tab_bar_p = false;
        bool tool_bar_p = false;
	bool dnd_grab = false;

	if (x_dnd_in_progress
	    /* Handle these events normally if the recursion
	       level is higher than when the drag-and-drop
	       operation was initiated.  This is so that mouse
	       input works while we're in the debugger for, say,
	       `x-dnd-movement-function`.  */
	    && (command_loop_level + minibuf_level
		<= x_dnd_recursion_depth)
	    && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
	  {
	    f = mouse_or_wdesc_frame (dpyinfo, event->xbutton.window);

	    if (f && event->xbutton.window == FRAME_X_WINDOW (f))
	      /* See the comment above x_compute_root_window_offset
		 for why this optimization is performed.  */
	      x_compute_root_window_offset (f, event->xbutton.x_root,
					    event->xbutton.y_root,
					    event->xbutton.x,
					    event->xbutton.y);

	    if (event->type == ButtonPress)
	      {
		x_display_set_last_user_time (dpyinfo, event->xbutton.time,
					      event->xbutton.send_event, true);

		dpyinfo->grabbed |= (1 << event->xbutton.button);
		dpyinfo->last_mouse_frame = f;

		if (f)
		  f->last_tab_bar_item = -1;
#if ! defined (USE_GTK)
		if (f)
		  f->last_tool_bar_item = -1;
#endif /* not USE_GTK */
	      }
	    else
	      dpyinfo->grabbed &= ~(1 << event->xbutton.button);

	    if (event->xbutton.type == ButtonPress
		&& x_dnd_last_seen_window != None)
	      {
		if (x_dnd_last_window_is_frame)
		  x_dnd_note_self_wheel (dpyinfo,
					 x_dnd_last_seen_window,
					 event->xbutton.x_root,
					 event->xbutton.y_root,
					 event->xbutton.button,
					 event->xbutton.state,
					 event->xbutton.time);
		else if (x_dnd_last_protocol_version != -1)
		  x_dnd_send_position (x_dnd_frame,
				       x_dnd_last_seen_window,
				       x_dnd_last_seen_toplevel,
				       x_dnd_last_protocol_version,
				       event->xbutton.x_root,
				       event->xbutton.y_root,
				       event->xbutton.time,
				       x_dnd_wanted_action,
				       event->xbutton.button,
				       event->xbutton.state);

		goto OTHER;
	      }

	    if (event->xbutton.type == ButtonRelease)
	      {
		for (int i = 1; i < 8; ++i)
		  {
		    if (i != event->xbutton.button
			&& event->xbutton.state & (Button1Mask << (i - 1)))
		      dnd_grab = true;
		  }

		if (!dnd_grab)
		  {
		    x_dnd_end_window = x_dnd_last_seen_window;
		    x_dnd_in_progress = false;

		    if (x_dnd_update_tooltip
			&& FRAMEP (tip_frame)
			&& FRAME_LIVE_P (XFRAME (tip_frame))
			&& (FRAME_X_DISPLAY (XFRAME (tip_frame))
			    == FRAME_X_DISPLAY (x_dnd_frame)))
		      Fx_hide_tip ();

		    x_dnd_finish_frame = x_dnd_frame;

		    if (x_dnd_last_seen_window != None
			&& x_dnd_last_window_is_frame)
		      {
			x_dnd_waiting_for_finish = false;
			x_dnd_note_self_drop (dpyinfo,
					      x_dnd_last_seen_window,
					      event->xbutton.x_root,
					      event->xbutton.y_root,
					      event->xbutton.time);
		      }
		    else if (x_dnd_last_seen_window != None
			     && x_dnd_last_protocol_version != -1)
		      {
			x_dnd_pending_finish_target = x_dnd_last_seen_toplevel;
			x_dnd_waiting_for_finish_proto = x_dnd_last_protocol_version;

			x_dnd_waiting_for_finish
			  = x_dnd_do_drop (x_dnd_last_seen_window,
					   x_dnd_last_seen_toplevel,
					   x_dnd_last_protocol_version);
			x_dnd_finish_display = dpyinfo->display;
		      }
		    else if (x_dnd_last_seen_window != None)
		      {
			xm_drop_start_message dmsg;
			xm_drag_receiver_info drag_receiver_info;

			if (!xm_read_drag_receiver_info (dpyinfo, x_dnd_last_seen_window,
							 &drag_receiver_info)
			    && !x_dnd_disable_motif_protocol
			    && drag_receiver_info.protocol_style != XM_DRAG_STYLE_NONE
			    && (x_dnd_allow_current_frame
				|| x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame)))
			  {
			    if (!x_dnd_motif_setup_p)
			      xm_setup_drag_info (dpyinfo, x_dnd_frame);

			    if (x_dnd_motif_setup_p)
			      {
				memset (&dmsg, 0, sizeof dmsg);

				dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
							      XM_DRAG_REASON_DROP_START);
				dmsg.byte_order = XM_BYTE_ORDER_CUR_FIRST;
				dmsg.side_effects
				  = XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (dpyinfo,
										     x_dnd_wanted_action),
							 XM_DROP_SITE_VALID, x_dnd_motif_operations,
							 (!x_dnd_xm_use_help
							  ? XM_DROP_ACTION_DROP
							  : XM_DROP_ACTION_DROP_HELP));
				dmsg.timestamp = event->xbutton.time;
				dmsg.x = event->xbutton.x_root;
				dmsg.y = event->xbutton.y_root;
				dmsg.index_atom = x_dnd_motif_atom;
				dmsg.source_window = FRAME_X_WINDOW (x_dnd_frame);

				if (!XM_DRAG_STYLE_IS_DROP_ONLY (drag_receiver_info.protocol_style))
				  x_dnd_send_xm_leave_for_drop (FRAME_DISPLAY_INFO (x_dnd_frame),
								x_dnd_frame, x_dnd_last_seen_window,
								event->xbutton.time);

				xm_send_drop_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
						      x_dnd_last_seen_window, &dmsg);

				x_dnd_waiting_for_finish = true;
				x_dnd_waiting_for_motif_finish_display = dpyinfo;
				x_dnd_waiting_for_motif_finish = 1;
				x_dnd_finish_display = dpyinfo->display;
			      }
			  }
			else
			  x_dnd_send_unsupported_drop (dpyinfo, (x_dnd_last_seen_toplevel != None
								 ? x_dnd_last_seen_toplevel
								 : x_dnd_last_seen_window),
						       event->xbutton.x_root, event->xbutton.y_root,
						       event->xbutton.time);
		      }
		    else if (x_dnd_last_seen_toplevel != None)
		      x_dnd_send_unsupported_drop (dpyinfo, x_dnd_last_seen_toplevel,
						   event->xbutton.x_root,
						   event->xbutton.y_root,
						   event->xbutton.time);


		    x_dnd_last_protocol_version = -1;
		    x_dnd_last_motif_style = XM_DRAG_STYLE_NONE;
		    x_dnd_last_seen_window = None;
		    x_dnd_last_seen_toplevel = None;
		    x_dnd_last_window_is_frame = false;
		    x_dnd_frame = NULL;
		  }
	      }

	    goto OTHER;
	  }

	if (x_dnd_in_progress
	    && (command_loop_level + minibuf_level
		<= x_dnd_recursion_depth))
	  goto OTHER;

	memset (&compose_status, 0, sizeof (compose_status));
	dpyinfo->last_mouse_glyph_frame = NULL;

	f = mouse_or_wdesc_frame (dpyinfo, event->xbutton.window);

	if (f && event->xbutton.window == FRAME_X_WINDOW (f))
	  /* See the comment above x_compute_root_window_offset
	     for why this optimization is performed.  */
	  x_compute_root_window_offset (f, event->xbutton.x_root,
					event->xbutton.y_root,
					event->xbutton.x,
					event->xbutton.y);

	if (f && event->xbutton.type == ButtonPress
	    && !popup_activated ()
	    && !x_window_to_scroll_bar (event->xbutton.display,
					event->xbutton.window, 2)
	    && !FRAME_NO_ACCEPT_FOCUS (f))
	  {
	    /* When clicking into a child frame or when clicking
	       into a parent frame with the child frame selected and
	       `no-accept-focus' is not set, select the clicked
	       frame.  */
	    struct frame *hf = dpyinfo->highlight_frame;

	    if (FRAME_PARENT_FRAME (f) || (hf && frame_ancestor_p (f, hf)))
	      {
		x_ignore_errors_for_next_request (dpyinfo, 0);
		XSetInputFocus (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
				RevertToParent, event->xbutton.time);
	        x_stop_ignoring_errors (dpyinfo);

		if (FRAME_PARENT_FRAME (f))
		  XRaiseWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f));
	      }
	  }

#ifdef USE_GTK
	if (!f)
	  {
	    f = x_any_window_to_frame (dpyinfo, event->xbutton.window);

	    if (event->xbutton.button > 3
		&& event->xbutton.button < 8
		&& f)
	      {
		if (ignore_next_mouse_click_timeout
		    && dpyinfo == mouse_click_timeout_display)
		  {
		    if (event->type == ButtonPress
			&& event->xbutton.time > ignore_next_mouse_click_timeout)
		      {
			ignore_next_mouse_click_timeout = 0;
			x_construct_mouse_click (&inev.ie, &event->xbutton,
			                         f, false);
		      }
		    if (event->type == ButtonRelease)
		      ignore_next_mouse_click_timeout = 0;
		  }
		else
		  x_construct_mouse_click (&inev.ie, &event->xbutton, f, false);

		*finish = X_EVENT_DROP;
		goto OTHER;
	      }
	    else
	      f = NULL;
	  }

        if (f && xg_event_is_for_scrollbar (f, event, false))
          f = 0;
#endif
        if (f)
          {
            /* Is this in the tab-bar?  */
            if (WINDOWP (f->tab_bar_window)
                && WINDOW_TOTAL_LINES (XWINDOW (f->tab_bar_window)))
              {
                Lisp_Object window;
                int x = event->xbutton.x;
                int y = event->xbutton.y;

                window = window_from_coordinates (f, x, y, 0, true, true, true);
                tab_bar_p = EQ (window, f->tab_bar_window);

                if (tab_bar_p)
		  {
		    tab_bar_arg = handle_tab_bar_click
		      (f, x, y, event->xbutton.type == ButtonPress,
		       x_x_to_emacs_modifiers (dpyinfo, event->xbutton.state));
		    x_flush_dirty_back_buffer_on (f);
		  }
              }

#if ! defined (USE_GTK)
            /* Is this in the tool-bar?  */
            if (WINDOWP (f->tool_bar_window)
                && WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)))
              {
                Lisp_Object window;
                int x = event->xbutton.x;
                int y = event->xbutton.y;

                window = window_from_coordinates (f, x, y, 0, true, true, true);
                tool_bar_p = (EQ (window, f->tool_bar_window)
			      && (event->xbutton.type != ButtonRelease
				  || f->last_tool_bar_item != -1));

                if (tool_bar_p && event->xbutton.button < 4)
		  {
		    handle_tool_bar_click
		      (f, x, y, event->xbutton.type == ButtonPress,
		       x_x_to_emacs_modifiers (dpyinfo, event->xbutton.state));
		    x_flush_dirty_back_buffer_on (f);
		  }
              }
#endif /* !USE_GTK */

            if (!(tab_bar_p && NILP (tab_bar_arg)) && !tool_bar_p)
#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
              if (! popup_activated ())
#endif
                {
                  if (ignore_next_mouse_click_timeout
		      && dpyinfo == mouse_click_timeout_display)
                    {
                      if (event->type == ButtonPress
                          && event->xbutton.time > ignore_next_mouse_click_timeout)
                        {
                          ignore_next_mouse_click_timeout = 0;
                          x_construct_mouse_click (&inev.ie, &event->xbutton,
                                                   f, false);
                        }
                      if (event->type == ButtonRelease)
                        ignore_next_mouse_click_timeout = 0;
                    }
                  else
                    x_construct_mouse_click (&inev.ie, &event->xbutton,
                                             f, false);

		  if (!NILP (tab_bar_arg))
		    inev.ie.arg = tab_bar_arg;
                }

            if (FRAME_X_EMBEDDED_P (f)
		&& !FRAME_NO_ACCEPT_FOCUS (f))
              xembed_send_message (f, event->xbutton.time,
                                   XEMBED_REQUEST_FOCUS, 0, 0, 0);
          }
        else
          {
            struct scroll_bar *bar
              = x_window_to_scroll_bar (event->xbutton.display,
                                        event->xbutton.window, 2);

#ifdef USE_TOOLKIT_SCROLL_BARS
            /* Make the "Ctrl-Mouse-2 splits window" work for toolkit
               scroll bars.  */
            if (bar && event->xbutton.state & ControlMask)
              {
                x_scroll_bar_handle_click (bar, event, &inev.ie, Qnil);
                *finish = X_EVENT_DROP;
              }
#else /* not USE_TOOLKIT_SCROLL_BARS */
            if (bar)
              x_scroll_bar_handle_click (bar, event, &inev.ie, Qnil);
#endif /* not USE_TOOLKIT_SCROLL_BARS */
          }

        if (event->type == ButtonPress)
          {
            dpyinfo->grabbed |= (1 << event->xbutton.button);
            dpyinfo->last_mouse_frame = f;
            if (f && !tab_bar_p)
              f->last_tab_bar_item = -1;
#if ! defined (USE_GTK)
            if (f && !tool_bar_p)
              f->last_tool_bar_item = -1;
#endif /* not USE_GTK */
          }
        else
          dpyinfo->grabbed &= ~(1 << event->xbutton.button);

	/* Ignore any mouse motion that happened before this event;
	   any subsequent mouse-movement Emacs events should reflect
	   only motion after the ButtonPress/Release.  */
	if (f != 0)
	  f->mouse_moved = false;

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
        f = x_menubar_window_to_frame (dpyinfo, event);
        /* For a down-event in the menu bar, don't pass it to Xt or
           GTK right away.  Instead, save it and pass it to Xt or GTK
           from kbd_buffer_get_event.  That way, we can run some Lisp
           code first.  */
        if (! popup_activated ()
#ifdef USE_GTK
            /* Gtk+ menus only react to the first three buttons. */
            && event->xbutton.button < 3
#endif
            && f && event->type == ButtonPress
            /* Verify the event is really within the menu bar
               and not just sent to it due to grabbing.  */
            && event->xbutton.x >= 0
            && event->xbutton.x < FRAME_PIXEL_WIDTH (f)
            && event->xbutton.y >= 0
            && event->xbutton.y < FRAME_MENUBAR_HEIGHT (f)
            && event->xbutton.same_screen)
          {
#ifdef USE_MOTIF
	    Widget widget;

	    widget = XtWindowToWidget (dpyinfo->display,
				       event->xbutton.window);

	    if (widget && XmIsCascadeButton (widget)
		&& XtIsSensitive (widget))
	      {
#endif
		if (!f->output_data.x->saved_menu_event)
		  f->output_data.x->saved_menu_event = xmalloc (sizeof *event);
		*f->output_data.x->saved_menu_event = *event;
		inev.ie.kind = MENU_BAR_ACTIVATE_EVENT;
		XSETFRAME (inev.ie.frame_or_window, f);
		*finish = X_EVENT_DROP;
#ifdef USE_MOTIF
	      }
#endif
          }
        else
          goto OTHER;
#endif /* USE_X_TOOLKIT || USE_GTK */
      }
      break;

    case CirculateNotify:
      if (x_dnd_in_progress
	  /* When _NET_WM_CLIENT_LIST stacking is being used, changes
	     in that property are watched for, and it's not necessary
	     to update the state in response to ordinary window
	     substructure events.  */
	  && !x_dnd_use_toplevels
	  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
	x_dnd_update_state (dpyinfo, dpyinfo->last_user_time);
      goto OTHER;

    case CirculateRequest:
      goto OTHER;

    case VisibilityNotify:
      f = x_top_window_to_frame (dpyinfo, event->xvisibility.window);

      if (f)
	FRAME_X_OUTPUT (f)->visibility_state = event->xvisibility.state;

      goto OTHER;

    case MappingNotify:
      /* Someone has changed the keyboard mapping - update the
         local cache.  */
      switch (event->xmapping.request)
        {
        case MappingModifier:
          x_find_modifier_meanings (dpyinfo);
	  FALLTHROUGH;
        case MappingKeyboard:
          XRefreshKeyboardMapping ((XMappingEvent *) &event->xmapping);
        }
      goto OTHER;

    case DestroyNotify:
      if (event->xdestroywindow.window
	  == dpyinfo->net_supported_window)
	dpyinfo->net_supported_window = None;

      if (event->xdestroywindow.window
	  == dpyinfo->motif_drag_window)
	/* We get DestroyNotify events for the drag window because
	   x_special_window_exists_p selects for structure
	   notification.  The drag window is not supposed to go away
	   but not all clients obey that requirement when setting the
	   drag window property.  */
	dpyinfo->motif_drag_window = None;

      xft_settings_event (dpyinfo, event);
      break;

#ifdef HAVE_XINPUT2
    case GenericEvent:
      {
	if (!dpyinfo->supports_xi2)
	  goto OTHER;

	if (event->xgeneric.extension != dpyinfo->xi2_opcode)
	  /* Not an XI2 event. */
	  goto OTHER;

	bool must_free_data = false;
	XIEvent *xi_event = (XIEvent *) event->xcookie.data;

	/* Sometimes the event is already claimed by GTK, which
	   will free its data in due course. */
	if (!xi_event)
	  {
	    if (XGetEventData (dpyinfo->display, &event->xcookie))
	      must_free_data = true;

	    xi_event = (XIEvent *) event->xcookie.data;
	  }

	XIDeviceEvent *xev = (XIDeviceEvent *) xi_event;

	if (!xi_event)
	  {
	    /* It may turn out that the event data has already been
	       implicitly freed for various reasons up to and
	       including XMenuActivate pushing some other event onto
	       the foreign-event queue, or x_menu_wait_for_events
	       calling XNextEvent through a timer that tries to wait
	       for input.

	       In that case, XGetEventData will return True, but
	       cookie->data will be NULL.  Since handling such input
	       events is not really important, we can afford to
	       discard them.

	       The way Xlib is currently implemented makes calling
	       XFreeEventData unnecessary in this case, but call it
	       anyway, since not doing so may lead to a memory leak in
	       the future.  */

	    if (must_free_data)
	      XFreeEventData (dpyinfo->display, &event->xcookie);
	    goto OTHER;
	  }

	switch (event->xcookie.evtype)
	  {
	    /* XI focus events aren't employed under X toolkit or GTK+
	       2.x because windows created by these two toolkits are
	       incompatible with input extension focus events.  */
#if !defined USE_X_TOOLKIT && (!defined USE_GTK || defined HAVE_GTK3)
	  case XI_FocusIn:
	    {
	      XIFocusInEvent *focusin;

	      focusin = (XIFocusInEvent *) xi_event;
	      any = x_any_window_to_frame (dpyinfo, focusin->event);

#ifdef USE_GTK
	      /* Some WMs (e.g. Mutter in Gnome Shell), don't unmap
		 minimized/iconified windows; thus, for those WMs we
		 won't get a MapNotify when unminimizing/deiconifying.
		 Check here if we are deiconizing a window (Bug42655).

		 But don't do that by default on GTK since it may
		 cause a plain invisible frame get reported as
		 iconified, compare
		 https://lists.gnu.org/archive/html/emacs-devel/2017-02/msg00133.html.
		 That is fixed above but bites us here again.

		 The option x_set_frame_visibility_more_laxly enables
		 overriding the default behavior (Bug#49955,
		 Bug#53298).  */
	      if (EQ (x_set_frame_visibility_more_laxly, Qfocus_in)
		  || EQ (x_set_frame_visibility_more_laxly, Qt))
#endif /* USE_GTK */
		{
		  f = any;
		  if (f && FRAME_ICONIFIED_P (f))
		    {
		      SET_FRAME_VISIBLE (f, 1);
		      SET_FRAME_ICONIFIED (f, false);
		      f->output_data.x->has_been_visible = true;
		      inev.ie.kind = DEICONIFY_EVENT;
		      XSETFRAME (inev.ie.frame_or_window, f);
		    }
		}

	      xi_focus_handle_for_device (dpyinfo, any, xi_event);

	      goto XI_OTHER;
	    }

	  case XI_FocusOut:
	    {
	      XIFocusOutEvent *focusout;

	      focusout = (XIFocusOutEvent *) xi_event;
	      any = x_any_window_to_frame (dpyinfo, focusout->event);
	      xi_focus_handle_for_device (dpyinfo, any, xi_event);

	      goto XI_OTHER;
	    }
#endif /* !USE_X_TOOLKIT && (!USE_GTK || HAVE_GTK3) */

	  case XI_Enter:
	    {
	      XIEnterEvent *enter = (XIEnterEvent *) xi_event;
	      XMotionEvent ev;
	      struct xi_device_t *source;

	      any = x_top_window_to_frame (dpyinfo, enter->event);
	      source = xi_device_from_id (dpyinfo, enter->sourceid);

	      ev.x = lrint (enter->event_x);
	      ev.y = lrint (enter->event_y);
	      ev.window = enter->event;
	      ev.time = enter->time;
	      ev.send_event = enter->send_event;

	      x_display_set_last_user_time (dpyinfo, enter->time,
					    enter->send_event, false);

#ifdef USE_MOTIF
	      use_copy = true;

	      copy.xcrossing.type = EnterNotify;
	      copy.xcrossing.serial = enter->serial;
	      copy.xcrossing.send_event = enter->send_event;
	      copy.xcrossing.display = dpyinfo->display;
	      copy.xcrossing.window = enter->event;
	      copy.xcrossing.root = enter->root;
	      copy.xcrossing.subwindow = enter->child;
	      copy.xcrossing.time = enter->time;
	      copy.xcrossing.x = lrint (enter->event_x);
	      copy.xcrossing.y = lrint (enter->event_y);
	      copy.xcrossing.x_root = lrint (enter->root_x);
	      copy.xcrossing.y_root = lrint (enter->root_y);
	      copy.xcrossing.mode = enter->mode;
	      copy.xcrossing.detail = enter->detail;
	      copy.xcrossing.focus = enter->focus;
	      copy.xcrossing.state = 0;
	      copy.xcrossing.same_screen = True;
#endif

	      /* There is no need to handle entry/exit events for
		 passive focus from non-top windows at all, since they
		 are an inferiors of the frame's top window, which will
		 get virtual events.  */

#if !defined USE_X_TOOLKIT && (!defined USE_GTK || defined HAVE_GTK3)
	      if (any)
		xi_focus_handle_for_device (dpyinfo, any, xi_event);
#endif /* !USE_X_TOOLKIT && (!USE_GTK || HAVE_GTK3) */

	      if (!any)
		any = x_any_window_to_frame (dpyinfo, enter->event);

#ifdef HAVE_XINPUT2_1
	      /* xfwm4 selects for button events on the frame window,
		 resulting in passive grabs being generated along with
		 the delivery of emulated button events; this then
		 interferes with scrolling, since device valuators
		 will constantly be reset as the crossing events
		 related to those grabs arrive.  The only way to
		 remedy this is to never reset scroll valuators on a
		 grab-related crossing event.  (bug#57476) */

	      if (enter->mode != XINotifyUngrab
		  && enter->mode != XINotifyGrab
		  && enter->mode != XINotifyPassiveGrab
		  && enter->mode != XINotifyPassiveUngrab)
		xi_reset_scroll_valuators_for_device_id (dpyinfo,
							 enter->deviceid);
#endif

	      {
#ifdef HAVE_XWIDGETS
		struct xwidget_view *xwidget_view;

		xwidget_view = xwidget_view_from_window (enter->event);

		if (xwidget_view)
		  {
		    xwidget_motion_or_crossing (xwidget_view, event);

		    goto XI_OTHER;
		  }
#endif
	      }

	      f = any;

	      if (f && enter->event == FRAME_X_WINDOW (f))
		xi_compute_root_window_offset_enter (f, enter);

	      if (f && x_mouse_click_focus_ignore_position)
		{
		  ignore_next_mouse_click_timeout = (enter->time
						     + x_mouse_click_focus_ignore_time);
		  mouse_click_timeout_display = dpyinfo;
		}

	      /* EnterNotify counts as mouse movement,
		 so update things that depend on mouse position.  */
	      if (f && !f->output_data.x->hourglass_p)
		x_note_mouse_movement (f, &ev, source ? source->name : Qnil);
#ifdef USE_GTK
	      /* We may get an EnterNotify on the buttons in the toolbar.  In that
		 case we moved out of any highlighted area and need to note this.  */
	      if (!f && dpyinfo->last_mouse_glyph_frame)
		x_note_mouse_movement (dpyinfo->last_mouse_glyph_frame, &ev,
				       source ? source->name : Qnil);
#endif
	      goto XI_OTHER;
	    }

	  case XI_Leave:
	    {
	      XILeaveEvent *leave;
	      struct xi_device_t *device;

	      leave = (XILeaveEvent *) xi_event;
#ifdef USE_GTK
	      struct xi_device_t *source;
	      XMotionEvent ev;

	      ev.x = lrint (leave->event_x);
	      ev.y = lrint (leave->event_y);
	      ev.window = leave->event;
	      ev.time = leave->time;
	      ev.send_event = leave->send_event;
#endif

	      any = x_top_window_to_frame (dpyinfo, leave->event);

#ifdef USE_GTK
	      source = xi_device_from_id (dpyinfo, leave->sourceid);
#endif
	      device = xi_device_from_id (dpyinfo, leave->deviceid);

	      if (device)
		xi_report_motion_window_clear (device);

	      /* This allows us to catch LeaveNotify events generated by
		 popup menu grabs.  FIXME: this is right when there is a
		 focus menu, but implicit focus tracking can get screwed
		 up if we get this and no XI_Enter event later.   */

#ifdef USE_X_TOOLKIT
	      if (popup_activated ()
		  && (leave->mode == XINotifyPassiveUngrab
		      || leave->mode == XINotifyUngrab))
		any = x_any_window_to_frame (dpyinfo, leave->event);
#endif

#ifdef USE_MOTIF
	      use_copy = true;

	      copy.xcrossing.type = LeaveNotify;
	      copy.xcrossing.serial = leave->serial;
	      copy.xcrossing.send_event = leave->send_event;
	      copy.xcrossing.display = dpyinfo->display;
	      copy.xcrossing.window = leave->event;
	      copy.xcrossing.root = leave->root;
	      copy.xcrossing.subwindow = leave->child;
	      copy.xcrossing.time = leave->time;
	      copy.xcrossing.x = lrint (leave->event_x);
	      copy.xcrossing.y = lrint (leave->event_y);
	      copy.xcrossing.x_root = lrint (leave->root_x);
	      copy.xcrossing.y_root = lrint (leave->root_y);
	      copy.xcrossing.mode = leave->mode;
	      copy.xcrossing.detail = leave->detail;
	      copy.xcrossing.focus = leave->focus;
	      copy.xcrossing.state = 0;
	      copy.xcrossing.same_screen = True;
#endif

	      /* One problem behind the design of XInput 2 scrolling is
		 that valuators are not unique to each window, but only
		 the window that has grabbed the valuator's device or
		 the window that the device's pointer is on top of can
		 receive motion events.  There is also no way to
		 retrieve the value of a valuator outside of each motion
		 event.

		 As such, to prevent wildly inaccurate results when
		 the valuators have changed outside Emacs, we reset
		 our records of each valuator's value whenever the
		 pointer moves out of a frame.  Ideally, this would
		 ignore events with a detail of XINotifyInferior, as
		 the window the pointer moved to would be one known to
		 Emacs, but the code to keep track of which valuators
		 had to be reset upon the corresponding XI_Enter event
		 was very complicated and kept running into server
		 bugs.  */
#ifdef HAVE_XINPUT2_1
	      /* xfwm4 selects for button events on the frame window,
		 resulting in passive grabs being generated along with
		 the delivery of emulated button events; this then
		 interferes with scrolling, since device valuators
		 will constantly be reset as the crossing events
		 related to those grabs arrive.  The only way to
		 remedy this is to never reset scroll valuators on a
		 grab-related crossing event.  (bug#57476) */

	      if (leave->mode != XINotifyUngrab
		  && leave->mode != XINotifyGrab
		  && leave->mode != XINotifyPassiveUngrab
		  && leave->mode != XINotifyPassiveGrab)
		xi_reset_scroll_valuators_for_device_id (dpyinfo,
							 leave->deviceid);
#endif

	      x_display_set_last_user_time (dpyinfo, leave->time,
					    leave->send_event, false);

#ifdef HAVE_XWIDGETS
	      {
		struct xwidget_view *xvw;

		xvw = xwidget_view_from_window (leave->event);

		if (xvw)
		  {
		    *finish = X_EVENT_DROP;
		    xwidget_motion_or_crossing (xvw, event);

		    goto XI_OTHER;
		  }
	      }
#endif

#if !defined USE_X_TOOLKIT && (!defined USE_GTK || defined HAVE_GTK3)
	      if (any)
		xi_focus_handle_for_device (dpyinfo, any, xi_event);
#endif /* !USE_X_TOOLKIT && (!USE_GTK || HAVE_GTK3) */

#ifndef USE_X_TOOLKIT
	      f = x_top_window_to_frame (dpyinfo, leave->event);
#else
	      /* On Xt builds that have XI2, the enter and leave event
		 masks are set on the frame widget's window.  */
	      f = x_window_to_frame (dpyinfo, leave->event);

	      if (!f)
		f = x_top_window_to_frame (dpyinfo, leave->event);
#endif

	      if (f)
		{
		  /* Now clear dpyinfo->last_mouse_motion_frame, or
		     gui_redo_mouse_highlight will end up highlighting
		     the last known position of the mouse if a
		     tooltip frame is later unmapped.  */

		  if (f == dpyinfo->last_mouse_motion_frame)
		    dpyinfo->last_mouse_motion_frame = NULL;

		  /* Something similar applies to
		     dpyinfo->last_mouse_glyph_frame.  */
		  if (f == dpyinfo->last_mouse_glyph_frame)
		    dpyinfo->last_mouse_glyph_frame = NULL;

		  if (f == hlinfo->mouse_face_mouse_frame)
		    {
		      /* If we move outside the frame, then we're
			 certainly no longer on any text in the frame.  */
		      clear_mouse_face (hlinfo);
		      hlinfo->mouse_face_mouse_frame = 0;
		      x_flush_dirty_back_buffer_on (f);
		    }

		  /* Generate a nil HELP_EVENT to cancel a help-echo.
		     Do it only if there's something to cancel.
		     Otherwise, the startup message is cleared when
		     the mouse leaves the frame.  */
		  if (any_help_event_p
		      /* But never if `mouse-drag-and-drop-region' is
			 in progress, since that results in the
			 tooltip being dismissed when the mouse moves
			 on top.  */
		      && !((EQ (track_mouse, Qdrag_source)
			    || EQ (track_mouse, Qdropping))
			   && gui_mouse_grabbed (dpyinfo)))
		    do_help = -1;

		  if (f && leave->event == FRAME_X_WINDOW (f))
		    xi_compute_root_window_offset_enter (f, leave);
		}
#ifdef USE_GTK
	      /* See comment in EnterNotify above */
	      else if (dpyinfo->last_mouse_glyph_frame)
		x_note_mouse_movement (dpyinfo->last_mouse_glyph_frame, &ev,
				       source ? source->name : Qnil);
#endif
	      goto XI_OTHER;
	    }

	  case XI_Motion:
	    {
	      struct xi_device_t *device, *source;
#ifdef HAVE_XINPUT2_1
	      XIValuatorState *states;
	      double *values;
	      bool found_valuator = false;
	      bool other_valuators_found = false;
#endif
	      /* A fake XMotionEvent for x_note_mouse_movement. */
	      XMotionEvent ev;
	      xm_top_level_leave_message lmsg;
	      xm_top_level_enter_message emsg;
	      xm_drag_motion_message dmsg;
	      unsigned int dnd_state;

	      source = xi_device_from_id (dpyinfo, xev->sourceid);

#ifdef HAVE_XINPUT2_1
	      states = &xev->valuators;
	      values = states->values;
#endif

	      device = xi_device_from_id (dpyinfo, xev->deviceid);

	      if (!device)
		goto XI_OTHER;

#ifdef HAVE_XINPUT2_2
	      if (xev->flags & XIPointerEmulated)
		goto XI_OTHER;
#endif

#ifdef HAVE_XINPUT2_1
#ifdef HAVE_XWIDGETS
	      struct xwidget_view *xv = xwidget_view_from_window (xev->event);
	      double xv_total_x = 0.0;
	      double xv_total_y = 0.0;
#endif
	      double total_x = 0.0;
	      double total_y = 0.0;

	      int real_x, real_y;

	      for (int i = 0; i < states->mask_len * 8; i++)
		{
		  if (XIMaskIsSet (states->mask, i))
		    {
		      struct xi_scroll_valuator_t *val;
		      double delta, scroll_unit;
		      int scroll_height;
		      Lisp_Object window;
		      struct scroll_bar *bar;

		      bar = NULL;

		      /* See the comment on top of x_cache_xi_devices
			 for more details on how scroll wheel movement
			 is reported on XInput 2.  */
		      delta = x_get_scroll_valuator_delta (dpyinfo, device,
							   i, *values, &val);
		      values++;

		      if (!val)
			{
			  other_valuators_found = true;
			  continue;
			}

		      if (delta != DBL_MAX)
			{
			  if (!f)
			    {
			      f = x_any_window_to_frame (dpyinfo, xev->event);

			      if (!f)
				{
#if defined USE_MOTIF || !defined USE_TOOLKIT_SCROLL_BARS
				  bar = x_window_to_scroll_bar (dpyinfo->display,
								xev->event, 2);

				  if (bar)
				    f = WINDOW_XFRAME (XWINDOW (bar->window));

				  if (!f)
#endif
				    goto XI_OTHER;
				}
			    }

#ifdef USE_GTK
			  if (f && xg_event_is_for_scrollbar (f, event, true))
			    *finish = X_EVENT_DROP;
#endif

			  if (FRAME_X_WINDOW (f) != xev->event)
			    {
			      if (!bar)
				bar = x_window_to_scroll_bar (dpyinfo->display, xev->event, 2);

			      /* If this is a scroll bar, compute the
				 actual position directly to avoid an
				 extra roundtrip.  */

			      if (bar)
				{
				  real_x = lrint (xev->event_x + bar->left);
				  real_y = lrint (xev->event_y + bar->top);
				}
			      else
			        x_translate_coordinates (f,
							 lrint (xev->root_x),
							 lrint (xev->root_y),
							 &real_x, &real_y);
			    }
			  else
			    {
			      real_x = lrint (xev->event_x);
			      real_y = lrint (xev->event_y);
			    }

#ifdef HAVE_XWIDGETS
			  if (xv)
			    {
			      if (val->horizontal)
				xv_total_x += delta;
			      else
				xv_total_y += delta;

			      found_valuator = true;
			      continue;
			    }
#endif

			  if (delta == 0.0)
			    found_valuator = true;

			  if (signbit (delta) != signbit (val->emacs_value))
			    val->emacs_value = 0;

			  val->emacs_value += delta;

			  if (mwheel_coalesce_scroll_events
			      && (fabs (val->emacs_value) < 1)
			      && (fabs (delta) > 0))
			    continue;

			  window = window_from_coordinates (f, real_x, real_y, NULL,
							    false, false, false);

			  if (WINDOWP (window))
			    scroll_height = XWINDOW (window)->pixel_height;
			  else
			    /* EVENT_X and EVENT_Y can be outside the
			       frame if F holds the input grab, so fall
			       back to the height of the frame instead.  */
			    scroll_height = FRAME_PIXEL_HEIGHT (f);

			  scroll_unit = pow (scroll_height, 2.0 / 3.0);

			  if (NUMBERP (Vx_scroll_event_delta_factor))
			    scroll_unit *= XFLOATINT (Vx_scroll_event_delta_factor);

			  if (val->horizontal)
			    total_x += val->emacs_value * scroll_unit;
			  else
			    total_y += val->emacs_value * scroll_unit;

			  found_valuator = true;
			  val->emacs_value = 0;
			}
		    }
		}

#ifdef HAVE_XWIDGETS
	      if (xv)
		{
		  unsigned int state;

		  state = xi_convert_event_state (xev);
		  x_display_set_last_user_time (dpyinfo, xev->time,
						xev->send_event, true);

		  if (found_valuator)
		    xwidget_scroll (xv, xev->event_x, xev->event_y,
				    -xv_total_x, -xv_total_y, state,
				    xev->time, (xv_total_x == 0.0
						&& xv_total_y == 0.0));
		  else
		    xwidget_motion_notify (xv, xev->event_x, xev->event_y,
					   xev->root_x, xev->root_y, state,
					   xev->time);

		  goto XI_OTHER;
		}
	      else
		{
#endif
		  if (found_valuator)
		    {
		      x_display_set_last_user_time (dpyinfo, xev->time,
						    xev->send_event, true);


#if defined USE_GTK && !defined HAVE_GTK3
		      /* Unlike on Motif, we can't select for XI
			 events on the scroll bar window under GTK+ 2.
			 So instead of that, just ignore XI wheel
			 events which land on a scroll bar.

			 Here we assume anything which isn't the edit
			 widget window is a scroll bar.  */

		      if (xev->child != None
			  && xev->child != FRAME_X_WINDOW (f))
			goto XI_OTHER;
#endif

		      /* If this happened during a drag-and-drop
			 operation, don't send an event.  We only have
			 to set the user time.  */
		      if (x_dnd_in_progress
			  /* If another master device moved the
			     pointer, we should put a wheel event on
			     the keyboard buffer as usual.  It will be
			     run once the drag-and-drop operation
			     completes.  */
			  && xev->deviceid == x_dnd_pointer_device
			  && (command_loop_level + minibuf_level
			      <= x_dnd_recursion_depth)
			  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
			goto XI_OTHER;

		      if (fabs (total_x) > 0 || fabs (total_y) > 0)
			{
			  inev.ie.kind = (fabs (total_y) >= fabs (total_x)
					  ? WHEEL_EVENT : HORIZ_WHEEL_EVENT);
			  inev.ie.timestamp = xev->time;

			  XSETINT (inev.ie.x, lrint (real_x));
			  XSETINT (inev.ie.y, lrint (real_y));
			  XSETFRAME (inev.ie.frame_or_window, f);

			  inev.ie.modifiers = (signbit (fabs (total_y) >= fabs (total_x)
							? total_y : total_x)
					       ? down_modifier : up_modifier);
			  inev.ie.modifiers
			    |= x_x_to_emacs_modifiers (dpyinfo,
						       xev->mods.effective);
			  inev.ie.arg = list3 (Qnil,
					       make_float (total_x),
					       make_float (total_y));
			}
		      else
			{
			  inev.ie.kind = TOUCH_END_EVENT;
			  inev.ie.timestamp = xev->time;

			  XSETINT (inev.ie.x, lrint (real_x));
			  XSETINT (inev.ie.y, lrint (real_y));
			  XSETFRAME (inev.ie.frame_or_window, f);
			}

		      if (source && !NILP (source->name))
			inev.ie.device = source->name;

		      if (!other_valuators_found)
			goto XI_OTHER;
		    }
#ifdef HAVE_XWIDGETS
		}
#endif
#endif /* HAVE_XINPUT2_1 */

	      if (!xi_position_changed (device, xev))
		goto XI_OTHER;

	      ev.x = lrint (xev->event_x);
	      ev.y = lrint (xev->event_y);
	      ev.window = xev->event;
	      ev.time = xev->time;
	      ev.send_event = xev->send_event;

#ifdef USE_MOTIF
	      use_copy = true;

	      copy.xmotion.type = MotionNotify;
	      copy.xmotion.serial = xev->serial;
	      copy.xmotion.send_event = xev->send_event;
	      copy.xmotion.display = dpyinfo->display;
	      copy.xmotion.window = xev->event;
	      copy.xmotion.root = xev->root;
	      copy.xmotion.subwindow = xev->child;
	      copy.xmotion.time = xev->time;
	      copy.xmotion.x = lrint (xev->event_x);
	      copy.xmotion.y = lrint (xev->event_y);
	      copy.xmotion.x_root = lrint (xev->root_x);
	      copy.xmotion.y_root = lrint (xev->root_y);
	      copy.xmotion.state = xi_convert_event_state (xev);

	      copy.xmotion.is_hint = False;
	      copy.xmotion.same_screen = True;
#endif

	      previous_help_echo_string = help_echo_string;
	      help_echo_string = Qnil;

	      if (hlinfo->mouse_face_hidden)
		{
		  hlinfo->mouse_face_hidden = false;
		  clear_mouse_face (hlinfo);
		}

	      f = mouse_or_wdesc_frame (dpyinfo, xev->event);

	      if (f && xev->event == FRAME_X_WINDOW (f))
		/* See the comment above x_compute_root_window_offset
		   for why this optimization is performed.  */
		xi_compute_root_window_offset (f, xev);

	      if (x_dnd_in_progress
		  /* Handle these events normally if the recursion
		     level is higher than when the drag-and-drop
		     operation was initiated.  This is so that mouse
		     input works while we're in the debugger for, say,
		     `x-dnd-movement-function`.  */
		  && (command_loop_level + minibuf_level
		      <= x_dnd_recursion_depth)
		  && xev->deviceid == x_dnd_pointer_device
		  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
		{
		  Window target, toplevel;
		  int target_proto, motif_style;
		  XRectangle *r;
		  bool was_frame;

		  /* Always clear mouse face.  */
		  clear_mouse_face (hlinfo);
		  hlinfo->mouse_face_hidden = true;

		  /* Sometimes the drag-and-drop operation starts with the
		     pointer of a frame invisible due to input.  Since
		     motion events are ignored during that, make the pointer
		     visible manually.  */

		  if (f)
		    {
		      XTtoggle_invisible_pointer (f, false);

		      r = &dpyinfo->last_mouse_glyph;

		      /* Also remember the mouse glyph and set
			 mouse_moved.  */
		      if (f != dpyinfo->last_mouse_glyph_frame
			  || lrint (xev->event_x) < r->x
			  || lrint (xev->event_x) >= r->x + r->width
			  || lrint (xev->event_y) < r->y
			  || lrint (xev->event_y) >= r->y + r->height)
			{
			  f->mouse_moved = true;
			  f->last_mouse_device = (source ? source->name
						  : Qnil);
			  dpyinfo->last_mouse_scroll_bar = NULL;

			  remember_mouse_glyph (f, lrint (xev->event_x),
						lrint (xev->event_y), r);
			  dpyinfo->last_mouse_glyph_frame = f;
			}
		    }

		  if (xev->root == dpyinfo->root_window)
		    target = x_dnd_get_target_window (dpyinfo,
						      lrint (xev->root_x),
						      lrint (xev->root_y),
						      &target_proto,
						      &motif_style,
						      &toplevel,
						      &was_frame);
		  else
		    target = x_dnd_fill_empty_target (&target_proto,
						      &motif_style,
						      &toplevel,
						      &was_frame);

		  if (toplevel != x_dnd_last_seen_toplevel)
		    {
		      if (toplevel != FRAME_OUTER_WINDOW (x_dnd_frame)
			  && x_dnd_return_frame == 1)
			x_dnd_return_frame = 2;

		      if (x_dnd_return_frame == 2
			  && x_any_window_to_frame (dpyinfo, toplevel))
			{
			  if (x_dnd_last_seen_window != None
			      && x_dnd_last_protocol_version != -1
			      && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
			    x_dnd_send_leave (x_dnd_frame, x_dnd_last_seen_window,
					      x_dnd_last_seen_toplevel);
			  else if (x_dnd_last_seen_window != None
				   && XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style)
				   && !x_dnd_disable_motif_drag
				   && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
			    {
			      if (!x_dnd_motif_setup_p)
				xm_setup_drag_info (dpyinfo, x_dnd_frame);

			      lmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
							    XM_DRAG_REASON_TOP_LEVEL_LEAVE);
			      lmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
			      lmsg.zero = 0;
			      lmsg.timestamp = xev->time;
			      lmsg.source_window = FRAME_X_WINDOW (x_dnd_frame);

			      if (x_dnd_motif_setup_p)
				xm_send_top_level_leave_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
								 x_dnd_last_seen_window, &lmsg);
			    }

			  x_dnd_end_window = x_dnd_last_seen_window;
			  x_dnd_last_seen_window = None;
			  x_dnd_last_seen_toplevel = None;
			  x_dnd_in_progress = false;
			  x_dnd_return_frame_object
			    = x_any_window_to_frame (dpyinfo, toplevel);
			  x_dnd_return_frame = 3;
			  x_dnd_waiting_for_finish = false;
			  target = None;
			}
		    }

		  if (target != x_dnd_last_seen_window)
		    {
		      if (x_dnd_last_seen_window != None
			  && x_dnd_last_protocol_version != -1
			  && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
			x_dnd_send_leave (x_dnd_frame, x_dnd_last_seen_window,
					  x_dnd_last_seen_toplevel);
		      else if (x_dnd_last_seen_window != None
			       && XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style)
			       && !x_dnd_disable_motif_drag
			       && x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame))
			{
			  if (!x_dnd_motif_setup_p)
			    xm_setup_drag_info (dpyinfo, x_dnd_frame);

			  /* This is apparently required.  If we don't
			     send a motion event with the current root
			     window coordinates of the pointer before
			     the top level leave, then Motif displays
			     an ugly black border around the previous
			     drop site.  */

			  dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
							XM_DRAG_REASON_DRAG_MOTION);
			  dmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
			  dmsg.side_effects
			    = XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (dpyinfo,
									       x_dnd_wanted_action),
						   XM_DROP_SITE_NONE, x_dnd_motif_operations,
						   XM_DROP_ACTION_DROP_CANCEL);
			  dmsg.timestamp = xev->time;
			  dmsg.x = lrint (xev->root_x);
			  dmsg.y = lrint (xev->root_y);

			  lmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
							XM_DRAG_REASON_TOP_LEVEL_LEAVE);
			  lmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
			  lmsg.zero = 0;
			  lmsg.timestamp = xev->time;
			  lmsg.source_window = FRAME_X_WINDOW (x_dnd_frame);

			  if (x_dnd_motif_setup_p)
			    {
			      xm_send_drag_motion_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
							   x_dnd_last_seen_window, &dmsg);
			      xm_send_top_level_leave_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
							       x_dnd_last_seen_window, &lmsg);
			    }
			}

		      x_dnd_action = None;
		      x_dnd_last_seen_toplevel = toplevel;
		      x_dnd_last_seen_window = target;
		      x_dnd_last_protocol_version = target_proto;
		      x_dnd_last_motif_style = motif_style;
		      x_dnd_last_window_is_frame = was_frame;

		      if (target != None && x_dnd_last_protocol_version != -1)
			x_dnd_send_enter (x_dnd_frame, target,
					  x_dnd_last_seen_toplevel,
					  x_dnd_last_protocol_version);
		      else if (target != None && XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style)
			       && !x_dnd_disable_motif_drag)
			{
			  if (!x_dnd_motif_setup_p)
			    xm_setup_drag_info (dpyinfo, x_dnd_frame);

			  emsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
							XM_DRAG_REASON_TOP_LEVEL_ENTER);
			  emsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
			  emsg.zero = 0;
			  emsg.timestamp = xev->time;
			  emsg.source_window = FRAME_X_WINDOW (x_dnd_frame);
			  emsg.index_atom = x_dnd_motif_atom;

			  if (x_dnd_motif_setup_p)
			    xm_send_top_level_enter_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
							     target, &emsg);
			}
		    }
		  else
		    x_dnd_last_seen_toplevel = toplevel;

		  if (x_dnd_last_window_is_frame && target != None)
		    x_dnd_note_self_position (dpyinfo, target,
					      lrint (xev->root_x),
					      lrint (xev->root_y));
		  else if (x_dnd_last_protocol_version != -1 && target != None)
		    {
		      dnd_state = xi_convert_event_state (xev);

		      x_dnd_send_position (x_dnd_frame, target,
					   x_dnd_last_seen_toplevel,
					   x_dnd_last_protocol_version,
					   lrint (xev->root_x),
					   lrint (xev->root_y),
					   x_dnd_selection_timestamp,
					   x_dnd_wanted_action, 0,
					   dnd_state);
		    }
		  else if (XM_DRAG_STYLE_IS_DYNAMIC (x_dnd_last_motif_style) && target != None
			   && !x_dnd_disable_motif_drag)
		    {
		      if (!x_dnd_motif_setup_p)
			xm_setup_drag_info (dpyinfo, x_dnd_frame);

		      dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
						    XM_DRAG_REASON_DRAG_MOTION);
		      dmsg.byteorder = XM_BYTE_ORDER_CUR_FIRST;
		      dmsg.side_effects
			= XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (dpyinfo,
									   x_dnd_wanted_action),
					       XM_DROP_SITE_VALID, x_dnd_motif_operations,
					       (!x_dnd_xm_use_help
						? XM_DROP_ACTION_DROP
						: XM_DROP_ACTION_DROP_HELP));
		      dmsg.timestamp = xev->time;
		      dmsg.x = lrint (xev->root_x);
		      dmsg.y = lrint (xev->root_y);

		      if (x_dnd_motif_setup_p)
			xm_send_drag_motion_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
						     target, &dmsg);
		    }

		  x_dnd_update_tooltip_position (xev->root_x, xev->root_y);

		  goto XI_OTHER;
		}

#ifdef USE_GTK
	      if (f && xg_event_is_for_scrollbar (f, event, false))
		f = 0;
#endif
	      if (f)
		{
		  if (xev->event != FRAME_X_WINDOW (f))
		    {
		      x_translate_coordinates (f, lrint (xev->root_x),
					       lrint (xev->root_y),
					       &ev.x, &ev.y);
		      ev.window = FRAME_X_WINDOW (f);
		    }

		  /* Maybe generate a SELECT_WINDOW_EVENT for
		     `mouse-autoselect-window' but don't let popup menus
		     interfere with this (Bug#1261).  */
		  if (!NILP (Vmouse_autoselect_window)
		      && !popup_activated ()
		      /* Don't switch if we're currently in the minibuffer.
			 This tries to work around problems where the
			 minibuffer gets unselected unexpectedly, and where
			 you then have to move your mouse all the way down to
			 the minibuffer to select it.  */
		      && !MINI_WINDOW_P (XWINDOW (selected_window))
		      /* With `focus-follows-mouse' non-nil create an event
			 also when the target window is on another frame.  */
		      && (f == XFRAME (selected_frame)
			  || !NILP (focus_follows_mouse)))
		    {
		      Lisp_Object window = window_from_coordinates (f, ev.x, ev.y, 0, false, false,
								    false);

		      /* A window will be autoselected only when it is not
			 selected now and the last mouse movement event was
			 not in it.  The remainder of the code is a bit vague
			 wrt what a "window" is.  For immediate autoselection,
			 the window is usually the entire window but for GTK
			 where the scroll bars don't count.  For delayed
			 autoselection the window is usually the window's text
			 area including the margins.  */
		      if (WINDOWP (window)
			  && !EQ (window, last_mouse_window)
			  && !EQ (window, selected_window))
			{
			  inev.ie.kind = SELECT_WINDOW_EVENT;
			  inev.ie.frame_or_window = window;

			  if (source)
			    inev.ie.device = source->name;
			}

		      /* Remember the last window where we saw the mouse.  */
		      last_mouse_window = window;
		    }

		  if (!x_note_mouse_movement (f, &ev, source ? source->name : Qnil))
		    help_echo_string = previous_help_echo_string;
		}
	      else
		{
#ifndef USE_TOOLKIT_SCROLL_BARS
		  struct scroll_bar *bar
		    = x_window_to_scroll_bar (dpyinfo->display, xev->event, 2);

		  if (bar)
		    x_scroll_bar_note_movement (bar, &ev);
#endif /* USE_TOOLKIT_SCROLL_BARS */

		  /* If we move outside the frame, then we're
		     certainly no longer on any text in the frame.  */
		  clear_mouse_face (hlinfo);
		}

	      /* If the contents of the global variable help_echo_string
		 has changed, generate a HELP_EVENT.  */
	      if (!NILP (help_echo_string)
		  || !NILP (previous_help_echo_string))
		{
		  /* Also allow the focus and client pointer to be
		     adjusted accordingly, in case a help tooltip is
		     shown.  */
		  gen_help_device = device;
		  gen_help_time = xev->time;

		  do_help = 1;
		}

	      if (f)
		x_flush_dirty_back_buffer_on (f);
	      goto XI_OTHER;
	    }

	  case XI_ButtonRelease:
	  case XI_ButtonPress:
	    {
	      /* If we decide we want to generate an event to be seen
		 by the rest of Emacs, we put it here.  */
	      Lisp_Object tab_bar_arg = Qnil;
	      bool tab_bar_p = false;
	      bool tool_bar_p = false;
	      struct xi_device_t *device, *source;
#ifdef HAVE_XWIDGETS
	      struct xwidget_view *xvw;
#endif
	      /* A fake XButtonEvent for x_construct_mouse_click. */
	      XButtonEvent bv;
	      bool dnd_grab = false;
	      int dnd_state;

	      if (x_dnd_in_progress
		  && (command_loop_level + minibuf_level
		      <= x_dnd_recursion_depth)
		  && xev->deviceid == x_dnd_pointer_device
		  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
		{
		  f = mouse_or_wdesc_frame (dpyinfo, xev->event);
		  device = xi_device_from_id (dpyinfo, xev->deviceid);

		  if (f && xev->event == FRAME_X_WINDOW (f))
		    /* See the comment above
		       x_compute_root_window_offset for why this
		       optimization is performed.  */
		    xi_compute_root_window_offset (f, xev);

		  /* Don't track grab status for emulated pointer
		     events, because they are ignored by the regular
		     mouse click processing code.  */
#ifdef XIPointerEmulated
		  if (!(xev->flags & XIPointerEmulated))
		    {
#endif
		      if (xev->evtype == XI_ButtonPress)
			{
			  x_display_set_last_user_time (dpyinfo, xev->time,
							xev->send_event, true);

			  dpyinfo->grabbed |= (1 << xev->detail);
			  dpyinfo->last_mouse_frame = f;

			  if (device)
			    device->grab |= (1 << xev->detail);

			  if (f)
			    f->last_tab_bar_item = -1;
#if ! defined (USE_GTK)
			  if (f)
			    f->last_tool_bar_item = -1;
#endif /* not USE_GTK */
			}
		      else
			{
			  dpyinfo->grabbed &= ~(1 << xev->detail);
			  if (device)
			    device->grab &= ~(1 << xev->detail);
			}
#ifdef XIPointerEmulated
		    }
#endif

		  if (f && device)
		    xi_handle_interaction (dpyinfo, f, device,
					   xev->time);

		  if (xev->evtype == XI_ButtonPress
		      && x_dnd_last_seen_window != None)
		    {
		      dnd_state = xi_convert_event_state (xev);

		      if (x_dnd_last_window_is_frame)
			{
#ifdef XI_PointerEmulated
			  /* Set the last user time here even if this
			     is an emulated button event, since
			     something happened in response.  */

			  if (xev->flags & XIPointerEmulated)
			    x_display_set_last_user_time (dpyinfo, xev->time,
							  xev->send_event, true);
#endif
			  x_dnd_note_self_wheel (dpyinfo,
						 x_dnd_last_seen_window,
						 lrint (xev->root_x),
						 lrint (xev->root_y),
						 xev->detail, dnd_state,
						 xev->time);
			}
		      else
			x_dnd_send_position (x_dnd_frame,
					     x_dnd_last_seen_window,
					     x_dnd_last_seen_toplevel,
					     x_dnd_last_protocol_version,
					     lrint (xev->root_x),
					     lrint (xev->root_y),
					     xev->time, x_dnd_wanted_action,
					     xev->detail, dnd_state);

		      goto OTHER;
		    }

		  if (xev->evtype == XI_ButtonRelease)
		    {
		      for (int i = 0; i < xev->buttons.mask_len * 8; ++i)
			{
			  if (i != xev->detail && XIMaskIsSet (xev->buttons.mask, i))
			    dnd_grab = true;
			}

		      if (!dnd_grab)
			{
			  x_dnd_end_window = x_dnd_last_seen_window;
			  x_dnd_in_progress = false;

			  /* If a tooltip that we're following is
			     displayed, hide it now.  */

			  if (x_dnd_update_tooltip
			      && FRAMEP (tip_frame)
			      && FRAME_LIVE_P (XFRAME (tip_frame))
			      && (FRAME_X_DISPLAY (XFRAME (tip_frame))
				  == FRAME_X_DISPLAY (x_dnd_frame)))
			    Fx_hide_tip ();

			  /* This doesn't have to be marked since it
			     is only accessed if
			     x_dnd_waiting_for_finish is true, which
			     is only possible inside the DND event
			     loop where that frame is on the
			     stack.  */
			  x_dnd_finish_frame = x_dnd_frame;

			  if (x_dnd_last_seen_window != None
			      && x_dnd_last_window_is_frame)
			    {
			      x_dnd_waiting_for_finish = false;
			      x_dnd_note_self_drop (dpyinfo, x_dnd_last_seen_window,
						    lrint (xev->root_x),
						    lrint (xev->root_y), xev->time);
			    }
			  else if (x_dnd_last_seen_window != None
				   && x_dnd_last_protocol_version != -1)
			    {
			      x_dnd_pending_finish_target = x_dnd_last_seen_toplevel;
			      x_dnd_waiting_for_finish_proto = x_dnd_last_protocol_version;

			      x_dnd_waiting_for_finish
				= x_dnd_do_drop (x_dnd_last_seen_window,
						 x_dnd_last_seen_toplevel,
						 x_dnd_last_protocol_version);
			      x_dnd_finish_display = dpyinfo->display;
			    }
			  else if (x_dnd_last_seen_window != None)
			    {
			      xm_drop_start_message dmsg;
			      xm_drag_receiver_info drag_receiver_info;

			      if (!xm_read_drag_receiver_info (dpyinfo, x_dnd_last_seen_window,
							       &drag_receiver_info)
				  && !x_dnd_disable_motif_protocol
				  && drag_receiver_info.protocol_style != XM_DRAG_STYLE_NONE
				  && (x_dnd_allow_current_frame
				      || x_dnd_last_seen_window != FRAME_OUTER_WINDOW (x_dnd_frame)))
				{
				  if (!x_dnd_motif_setup_p)
				    xm_setup_drag_info (dpyinfo, x_dnd_frame);

				  if (x_dnd_motif_setup_p)
				    {
				      memset (&dmsg, 0, sizeof dmsg);

				      dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
								    XM_DRAG_REASON_DROP_START);
				      dmsg.byte_order = XM_BYTE_ORDER_CUR_FIRST;
				      dmsg.side_effects
					= XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (dpyinfo,
											   x_dnd_wanted_action),
							       XM_DROP_SITE_VALID, x_dnd_motif_operations,
							       (!x_dnd_xm_use_help
								? XM_DROP_ACTION_DROP
								: XM_DROP_ACTION_DROP_HELP));
				      dmsg.timestamp = xev->time;
				      dmsg.x = lrint (xev->root_x);
				      dmsg.y = lrint (xev->root_y);
				      /* This atom technically has to be
					 unique to each drag-and-drop
					 operation, but that isn't easy to
					 accomplish, since we cannot
					 randomly move data around between
					 selections.  Let's hope no two
					 instances of Emacs try to drag
					 into the same window at the same
					 time.  */
				      dmsg.index_atom = x_dnd_motif_atom;
				      dmsg.source_window = FRAME_X_WINDOW (x_dnd_frame);

				      if (!XM_DRAG_STYLE_IS_DROP_ONLY (drag_receiver_info.protocol_style))
					x_dnd_send_xm_leave_for_drop (FRAME_DISPLAY_INFO (x_dnd_frame),
								      x_dnd_frame, x_dnd_last_seen_window,
								      xev->time);

				      xm_send_drop_message (dpyinfo, FRAME_X_WINDOW (x_dnd_frame),
							    x_dnd_last_seen_window, &dmsg);

				      x_dnd_waiting_for_finish = true;
				      x_dnd_waiting_for_motif_finish_display = dpyinfo;
				      x_dnd_waiting_for_motif_finish = 1;
				      x_dnd_finish_display = dpyinfo->display;
				    }
				}
			      else
				x_dnd_send_unsupported_drop (dpyinfo, (x_dnd_last_seen_toplevel != None
								       ? x_dnd_last_seen_toplevel
								       : x_dnd_last_seen_window),
							     lrint (xev->root_x),
							     lrint (xev->root_y), xev->time);
			    }
			  else if (x_dnd_last_seen_toplevel != None)
			    x_dnd_send_unsupported_drop (dpyinfo,
							 x_dnd_last_seen_toplevel,
							 lrint (xev->root_x),
							 lrint (xev->root_y),
							 xev->time);

			  x_dnd_last_protocol_version = -1;
			  x_dnd_last_motif_style = XM_DRAG_STYLE_NONE;
			  x_dnd_last_seen_window = None;
			  x_dnd_last_seen_toplevel = None;
			  x_dnd_last_window_is_frame = false;
			  x_dnd_frame = NULL;

			  goto XI_OTHER;
			}
		    }
		}

	      if (x_dnd_in_progress
		  && (command_loop_level + minibuf_level
		      <= x_dnd_recursion_depth))
		goto XI_OTHER;

#ifdef USE_MOTIF
#ifdef USE_TOOLKIT_SCROLL_BARS
	      struct scroll_bar *bar
		= x_window_to_scroll_bar (dpyinfo->display,
					  xev->event, 2);
#endif

	      use_copy = true;
	      copy.xbutton.type = (xev->evtype == XI_ButtonPress
				   ? ButtonPress : ButtonRelease);
	      copy.xbutton.serial = xev->serial;
	      copy.xbutton.send_event = xev->send_event;
	      copy.xbutton.display = dpyinfo->display;
	      copy.xbutton.window = xev->event;
	      copy.xbutton.root = xev->root;
	      copy.xbutton.subwindow = xev->child;
	      copy.xbutton.time = xev->time;
	      copy.xbutton.x = lrint (xev->event_x);
	      copy.xbutton.y = lrint (xev->event_y);
	      copy.xbutton.x_root = lrint (xev->root_x);
	      copy.xbutton.y_root = lrint (xev->root_y);
	      copy.xbutton.state = xi_convert_event_state (xev);
	      copy.xbutton.button = xev->detail;
	      copy.xbutton.same_screen = True;

#elif defined USE_GTK && !defined HAVE_GTK3
	      copy = gdk_event_new (xev->evtype == XI_ButtonPress
				    ? GDK_BUTTON_PRESS : GDK_BUTTON_RELEASE);

	      copy->button.window = gdk_x11_window_lookup_for_display (gdpy, xev->event);
	      copy->button.send_event = xev->send_event;
	      copy->button.time = xev->time;
	      copy->button.x = xev->event_x;
	      copy->button.y = xev->event_y;
	      copy->button.x_root = xev->root_x;
	      copy->button.y_root = xev->root_y;
	      copy->button.state = xi_convert_event_state (xev);
	      copy->button.button = xev->detail;

	      if (!copy->button.window)
		emacs_abort ();

	      g_object_ref (copy->button.window);

	      if (popup_activated ())
		{
		  /* GTK+ popup menus don't respond to core buttons
		     after Button3, so don't dismiss popup menus upon
		     wheel movement here either.  */
		  if (xev->detail > 3)
		    *finish = X_EVENT_DROP;

		  if (xev->evtype == XI_ButtonRelease)
		    goto XI_OTHER;
		}
#endif

#ifdef HAVE_XINPUT2_1
	      /* Ignore emulated scroll events when XI2 native
		 scroll events are present.  */
	      if (xev->flags & XIPointerEmulated)
		{
#if !defined USE_MOTIF || !defined USE_TOOLKIT_SCROLL_BARS
		  *finish = X_EVENT_DROP;
#else
		  if (bar)
		    *finish = X_EVENT_DROP;
#endif
		  goto XI_OTHER;
		}
#endif

	      if (xev->evtype == XI_ButtonPress)
		x_display_set_last_user_time (dpyinfo, xev->time,
					      xev->send_event, true);

	      source = xi_device_from_id (dpyinfo, xev->sourceid);
	      device = xi_device_from_id (dpyinfo, xev->deviceid);

#ifdef HAVE_XWIDGETS
	      xvw = xwidget_view_from_window (xev->event);
	      if (xvw)
		{
		  /* If the user interacts with a frame that's focused
		     on another device, but not the current focus
		     frame, make it the focus frame.  */
		  if (device)
		    xi_handle_interaction (dpyinfo, xvw->frame,
					   device, xev->time);

		  xwidget_button (xvw, xev->evtype == XI_ButtonPress,
				  lrint (xev->event_x), lrint (xev->event_y),
				  xev->detail, xi_convert_event_state (xev),
				  xev->time);

		  if (!EQ (selected_window, xvw->w) && (xev->detail < 4))
		    {
		      inev.ie.kind = SELECT_WINDOW_EVENT;
		      inev.ie.frame_or_window = xvw->w;

		      if (source)
			inev.ie.device = source->name;
		    }

		  *finish = X_EVENT_DROP;
		  goto XI_OTHER;
		}
#endif

	      if (!device)
		goto XI_OTHER;

	      bv.button = xev->detail;
	      bv.type = xev->evtype == XI_ButtonPress ? ButtonPress : ButtonRelease;
	      bv.x = lrint (xev->event_x);
	      bv.y = lrint (xev->event_y);
	      bv.x_root = lrint (xev->root_x);
	      bv.y_root = lrint (xev->root_y);
	      bv.window = xev->event;
	      bv.state = xi_convert_event_state (xev);
	      bv.time = xev->time;

	      dpyinfo->last_mouse_glyph_frame = NULL;

	      f = mouse_or_wdesc_frame (dpyinfo, xev->event);

	      if (f && xev->event == FRAME_X_WINDOW (f))
		/* See the comment above x_compute_root_window_offset
		   for why this optimization is performed.  */
		xi_compute_root_window_offset (f, xev);

	      if (f && xev->evtype == XI_ButtonPress
		  && !popup_activated ()
		  && !x_window_to_scroll_bar (dpyinfo->display, xev->event, 2)
		  && !FRAME_NO_ACCEPT_FOCUS (f))
		{
		  /* When clicking into a child frame or when clicking
		     into a parent frame with the child frame selected and
		     `no-accept-focus' is not set, select the clicked
		     frame.  */
		  struct frame *hf = dpyinfo->highlight_frame;

		  if (FRAME_PARENT_FRAME (f) || (hf && frame_ancestor_p (f, hf)))
		    {
#if defined HAVE_GTK3 || (!defined USE_GTK && !defined USE_X_TOOLKIT)
		      if (device)
			{
			  /* This can generate XI_BadDevice if the
			     device's attachment was destroyed
			     server-side.  */
			  x_ignore_errors_for_next_request (dpyinfo, 0);
			  XISetFocus (dpyinfo->display, device->attachment,
				      /* Note that the input extension
					 only supports RevertToParent-type
					 behavior.  */
				      FRAME_OUTER_WINDOW (f), xev->time);
			  x_stop_ignoring_errors (dpyinfo);
			}
#else
		      /* Non-no toolkit builds without GTK 3 use core
			 events to handle focus.  Errors are still
			 caught here in case the window is not
			 viewable.  */
		      x_ignore_errors_for_next_request (dpyinfo, 0);
		      XSetInputFocus (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
				      RevertToParent, xev->time);
		      x_stop_ignoring_errors (dpyinfo);
#endif
		      if (FRAME_PARENT_FRAME (f))
			XRaiseWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f));
		    }
		}

	      if (f)
		{
		  /* If the user interacts with a frame that's focused
		     on another device, but not the current focus
		     frame, make it the focus frame.  */
		  if (device)
		    xi_handle_interaction (dpyinfo, f, device,
					   xev->time);
		}

#ifdef USE_GTK
	      if (!f)
		{
		  int real_x = lrint (xev->root_x);
		  int real_y = lrint (xev->root_y);

		  f = x_any_window_to_frame (dpyinfo, xev->event);

		  if (xev->detail > 3 && xev->detail < 8 && f)
		    {
		      if (xev->evtype == XI_ButtonRelease)
			{
			  if (FRAME_X_WINDOW (f) != xev->event)
			    x_translate_coordinates (f, real_x, real_y,
						     &real_x, &real_y);

			  if (xev->detail <= 5)
			    inev.ie.kind = WHEEL_EVENT;
			  else
			    inev.ie.kind = HORIZ_WHEEL_EVENT;

			  if (source)
			    inev.ie.device = source->name;

			  inev.ie.timestamp = xev->time;

			  XSETINT (inev.ie.x, real_x);
			  XSETINT (inev.ie.y, real_y);
			  XSETFRAME (inev.ie.frame_or_window, f);

			  inev.ie.modifiers
			    |= x_x_to_emacs_modifiers (dpyinfo,
						       xev->mods.effective);

			  inev.ie.modifiers |= xev->detail % 2 ? down_modifier : up_modifier;
			}

		      *finish = X_EVENT_DROP;
		      goto XI_OTHER;
		    }
		  else
		    f = NULL;
		}

	      if (f && xg_event_is_for_scrollbar (f, event, false))
		f = 0;
#endif

	      if (f)
		{
		  if (xev->detail >= 4 && xev->detail < 8)
		    {
		      if (xev->evtype == XI_ButtonRelease)
			{
			  if (xev->detail <= 5)
			    inev.ie.kind = WHEEL_EVENT;
			  else
			    inev.ie.kind = HORIZ_WHEEL_EVENT;

			  if (source)
			    inev.ie.device = source->name;

			  inev.ie.timestamp = xev->time;

			  XSETINT (inev.ie.x, lrint (xev->event_x));
			  XSETINT (inev.ie.y, lrint (xev->event_y));
			  XSETFRAME (inev.ie.frame_or_window, f);

			  inev.ie.modifiers
			    |= x_x_to_emacs_modifiers (dpyinfo,
						       xev->mods.effective);

			  inev.ie.modifiers |= xev->detail % 2 ? down_modifier : up_modifier;
			}

		      goto XI_OTHER;
		    }

		  /* Is this in the tab-bar?  */
		  if (WINDOWP (f->tab_bar_window)
		      && WINDOW_TOTAL_LINES (XWINDOW (f->tab_bar_window)))
		    {
		      Lisp_Object window;
		      int x = bv.x;
		      int y = bv.y;

		      window = window_from_coordinates (f, x, y, 0, true, true, true);
		      tab_bar_p = EQ (window, f->tab_bar_window);

		      if (tab_bar_p)
			{
			  tab_bar_arg = handle_tab_bar_click
			    (f, x, y, xev->evtype == XI_ButtonPress,
			     x_x_to_emacs_modifiers (dpyinfo, bv.state));
			  x_flush_dirty_back_buffer_on (f);
			}
		    }

#if ! defined (USE_GTK)
		  /* Is this in the tool-bar?  */
		  if (WINDOWP (f->tool_bar_window)
		      && WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)))
		    {
		      Lisp_Object window;
		      int x = bv.x;
		      int y = bv.y;

		      window = window_from_coordinates (f, x, y, 0, true, true, true);
		      /* Ignore button release events if the mouse
			 wasn't previously pressed on the tool bar.
			 We do this because otherwise selecting some
			 text with the mouse and then releasing it on
			 the tool bar doesn't stop selecting text,
			 since the tool bar eats the button up
			 event.  */
		      tool_bar_p = (EQ (window, f->tool_bar_window)
				    && (xev->evtype != XI_ButtonRelease
					|| f->last_tool_bar_item != -1));

		      if (tool_bar_p && xev->detail < 4)
			{
			  handle_tool_bar_click_with_device
			    (f, x, y, xev->evtype == XI_ButtonPress,
			     x_x_to_emacs_modifiers (dpyinfo, bv.state),
			     source ? source->name : Qt);
			  x_flush_dirty_back_buffer_on (f);
			}
		    }
#endif /* !USE_GTK */

		  if (!(tab_bar_p && NILP (tab_bar_arg)) && !tool_bar_p)
#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
		    if (! popup_activated ())
#endif
		      {
			if (ignore_next_mouse_click_timeout)
			  {
			    if (xev->evtype == XI_ButtonPress
				&& xev->time > ignore_next_mouse_click_timeout)
			      {
				ignore_next_mouse_click_timeout = 0;
				x_construct_mouse_click (&inev.ie, &bv, f, true);
			      }
			    if (xev->evtype == XI_ButtonRelease)
			      ignore_next_mouse_click_timeout = 0;
			  }
			else
			  x_construct_mouse_click (&inev.ie, &bv, f, true);

			if (!NILP (tab_bar_arg))
			  inev.ie.arg = tab_bar_arg;
		      }

		  if (FRAME_X_EMBEDDED_P (f)
		      && !FRAME_NO_ACCEPT_FOCUS (f))
		    xembed_send_message (f, xev->time,
					 XEMBED_REQUEST_FOCUS, 0, 0, 0);
		}
	      else
		{
		  struct scroll_bar *bar
		    = x_window_to_scroll_bar (dpyinfo->display,
					      xev->event, 2);

#ifndef USE_TOOLKIT_SCROLL_BARS
		  if (bar)
		    x_scroll_bar_handle_click (bar, (XEvent *) &bv, &inev.ie,
					       source ? source->name : Qnil);
#else
		  /* Make the "Ctrl-Mouse-2 splits window" work for toolkit
		     scroll bars.  */
		  if (bar && xev->mods.effective & ControlMask)
		    {
		      x_scroll_bar_handle_click (bar, (XEvent *) &bv, &inev.ie,
						 source ? source->name : Qnil);
		      *finish = X_EVENT_DROP;
		    }
#endif
		}

	      if (xev->evtype == XI_ButtonPress)
		{
		  dpyinfo->grabbed |= (1 << xev->detail);
		  device->grab |= (1 << xev->detail);
		  dpyinfo->last_mouse_frame = f;
		  if (f && !tab_bar_p)
		    f->last_tab_bar_item = -1;
#if ! defined (USE_GTK)
		  if (f && !tool_bar_p)
		    f->last_tool_bar_item = -1;
#endif /* not USE_GTK */

		}
	      else
		{
		  dpyinfo->grabbed &= ~(1 << xev->detail);
		  device->grab &= ~(1 << xev->detail);
		}

	      if (source && inev.ie.kind != NO_EVENT)
		inev.ie.device = source->name;

	      if (f)
		f->mouse_moved = false;

#if defined (USE_GTK)
	      /* No Xt toolkit currently available has support for XI2.
	         So the code here assumes use of GTK.  */
	      f = x_menubar_window_to_frame (dpyinfo, event);
	      if (f /* Gtk+ menus only react to the first three buttons. */
		  && xev->detail < 3)
		{
		  /* What is done with Core Input ButtonPressed is not
		     possible here, because GenericEvents cannot be saved.  */
		  bool was_waiting_for_input = waiting_for_input;
		  /* This hack was adopted from the NS port.  Whether
		     or not it is actually safe is a different story
		     altogether.  */
		  if (waiting_for_input)
		    waiting_for_input = 0;
		  set_frame_menubar (f, true);
		  waiting_for_input = was_waiting_for_input;
		}
#endif
	      goto XI_OTHER;
	    }

	  case XI_KeyPress:
	    {
	      int state = xev->mods.effective;
	      Lisp_Object c;
#ifdef HAVE_XKB
	      unsigned int mods_rtrn;
#endif /* HAVE_XKB */
	      int keycode = xev->detail;
	      KeySym keysym;
	      char copy_buffer[81];
	      char *copy_bufptr = copy_buffer;
	      int copy_bufsiz = sizeof (copy_buffer);
	      ptrdiff_t i;
	      unsigned int old_state;
	      struct xi_device_t *device, *source;
	      XKeyPressedEvent xkey;

	      coding = Qlatin_1;

	      /* The code under this label is quite desultory.  There
		 are also several important discrepancies with the
		 core KeyPress code to mind.

		 There are three principal objectives:

		 The first is to produce a core or GDK translation of
	         this XI_KeyPress event, which is relayed to the
	         toolkit.  This transpires by setting `copy' to a
	         close copy of XEV, which is later copied or
	         dispatched to the toolkit by the code beneath the
	         OTHER label.

	         The second objective is to filter the event through
	         an input method, by generating a second copy of the
	         event expressly tailored for such a purpose.  The
	         core KeyPress code does not endeavor to do so;
	         instead, this action is taken prior to calling
	         handle_one_xevent.  Calls to `x_filter_event' or
	         `xg_filter_key' serve to implement this objective.

	         If the event is not removed by the input method's
	         filter, the third objective is to establish either a
	         keysym or a sequence of characters to insert, using
	         the information supplied within the key event.

	         When an input method connection is available, this
	         responsibility is vested in the hands of the input
	         method -- yet another copy of XEV as a core event is
	         produced, and the input method is responsible for
	         deriving a keysym or text to insert.

	         Otherwise, if the XKB extension is available, calls
	         are made to XkbTranslateKeyCode and
	         XkbTranslateKeySym.

	         And if all else fails, XEV is transformed into a core
	         event and provided to XLookupString, in a manner
	         analogous to the core event processing under the
	         KeyPress label.

	         A wide number of variables are employed during this
	         translation process.  The most pertinent ones are:

		 `copy'

		   This variable is defined when an X toolkit
		   incognizant of input extension events is being
		   employed.  If a popup is active, Emacs copies
		   fields of interest from the extension event to
		   COPY, sets the `use_copy' flag, and jumps to the
		   XI_OTHER label.  `copy' is then relayed to the
		   toolkit.

		 `xkey'

		   This variable is defined to a copy of the event
		   used by input methods or XLookupString at various
		   points during the execution of this label.

		 `coding'

		   This variable is consulted at the conclusion of
		   event generation, and holds the coding system
		   for any generated string.

		 `keysym'

		   This variable is eventually set to the keysym tied
		   to the event, which may be directly provided within
		   a generated struct input_event, should it bear a
		   direct relation to an ASCII or Unicode character,
		   or if it is a control key.

		 `copy_buffer', `copy_bufptr', `copy_bufsiz'

		   These variables hold the buffer that incorporates
		   characters generated during the keycode-to-keysym
		   conversion process.

		 `nbytes'

		   Holds the number of characters within that buffer,
		   in bytes.  These characters are encoded using the
		   coding system in `coding'.

		   If greater than 0 and KEYSYM does not immediately
		   relate to a function key, control key or character,
		   it is provided as the string to insert within a
		   MULTIBYTE_CHAR_KEYSTROKE_EVENT.

		 `state'

		   Holds the keyboard and group (but not button)
		   state.  After event filtering concludes, modifier
		   bits within `extra_keyboard_modifiers' are also
		   introduced.

	         This illustration may reflect the treatment taken
	         towards core key events to some degree.  */

	      device = xi_device_from_id (dpyinfo, xev->deviceid);
	      source = xi_device_from_id (dpyinfo, xev->sourceid);

	      if (!device)
		goto XI_OTHER;

	      /* Convert the keyboard state within XEV to a core
		 modifier mask, later supplied as arguments to XKB and
		 core functions.  This encompasses the keyboard group
		 and effective modifiers but not the button state.  */
	      state = xi_convert_event_keyboard_state (xev);

#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
	      /* Dispatch XI_KeyPress events when in menu.  */
	      if (popup_activated ())
		{
#ifdef USE_LUCID
		  /* This makes key navigation work inside menus.  */
		  use_copy = true;
		  copy.xkey.type = KeyPress;
		  copy.xkey.serial = xev->serial;
		  copy.xkey.send_event = xev->send_event;
		  copy.xkey.display = dpyinfo->display;
		  copy.xkey.window = xev->event;
		  copy.xkey.root = xev->root;
		  copy.xkey.subwindow = xev->child;
		  copy.xkey.time = xev->time;
		  copy.xkey.state = state;
		  xi_convert_button_state (&xev->buttons, &copy.xkey.state);

		  copy.xkey.x = lrint (xev->event_x);
		  copy.xkey.y = lrint (xev->event_y);
		  copy.xkey.x_root = lrint (xev->root_x);
		  copy.xkey.y_root = lrint (xev->root_y);
		  copy.xkey.keycode = xev->detail;
		  copy.xkey.same_screen = True;
#endif /* USE_LUCID */
		  goto XI_OTHER;
		}
#endif /* USE_X_TOOLKIT || USE_GTK */

	      x_display_set_last_user_time (dpyinfo, xev->time,
					    xev->send_event, true);
	      ignore_next_mouse_click_timeout = 0;

	      f = x_any_window_to_frame (dpyinfo, xev->event);

	      if (f && xev->event == FRAME_X_WINDOW (f))
		/* See the comment above x_compute_root_window_offset
		   for why this optimization is performed.  */
		xi_compute_root_window_offset (f, xev);

	      /* GTK handles TAB events in an undesirable manner, so
		 keyboard events are always dropped.  But as a side
		 effect, the user time will no longer be set by GDK,
		 so do that manually.  */
#ifdef USE_GTK
	      if (f)
		x_set_gtk_user_time (f, xev->time);
#endif /* USE_GTK */

	      if (f)
		{
		  /* If the user interacts with a frame that's focused
		     on another device, but not the current focus
		     frame, make it the focus frame.  */
		  if (device)
		    xi_handle_interaction (dpyinfo, f, device,
					   xev->time);
		}

	      /* Convert the XI event into a core event structure
		 provided to old Xlib functions and input method
		 filter functions.  */

	      memset (&xkey, 0, sizeof xkey);

	      xkey.type = KeyPress;
	      xkey.serial = xev->serial;
	      xkey.send_event = xev->send_event;
	      xkey.display = dpyinfo->display;
	      xkey.window = xev->event;
	      xkey.root = xev->root;
	      xkey.subwindow = xev->child;
	      xkey.time = xev->time;
	      xkey.state = state;
	      xkey.x = lrint (xev->event_x);
	      xkey.y = lrint (xev->event_y);
	      xkey.x_root = lrint (xev->root_x);
	      xkey.y_root = lrint (xev->root_y);

	      /* Some input methods react differently depending on the
		 buttons that are pressed.  */
	      xi_convert_button_state (&xev->buttons, &xkey.state);

	      xkey.keycode = xev->detail;
	      xkey.same_screen = True;

#ifdef HAVE_X_I18N
#ifdef USE_GTK
	      if ((!x_gtk_use_native_input
		   && x_filter_event (dpyinfo, (XEvent *) &xkey))
		  || (x_gtk_use_native_input
		      && x_filter_event (dpyinfo, event)))
		{
		  /* Try to attribute core key events from the input
		     method to the input extension event that caused
		     them.  */
		  dpyinfo->pending_keystroke_time = xev->time;
		  dpyinfo->pending_keystroke_source = xev->sourceid;

		  *finish = X_EVENT_DROP;
		  goto XI_OTHER;
		}
#else /* !USE_GTK */
	      if (x_filter_event (dpyinfo, (XEvent *) &xkey))
		{
		  /* Try to attribute core key events from the input
		     method to the input extension event that caused
		     them.  */
		  dpyinfo->pending_keystroke_time = xev->time;
		  dpyinfo->pending_keystroke_source = xev->sourceid;

		  *finish = X_EVENT_DROP;
		  goto XI_OTHER;
		}
#endif /* HAVE_X_I18N */
#elif USE_GTK /* && !HAVE_X_I18N */
	      if ((x_gtk_use_native_input
		   || dpyinfo->prefer_native_input)
		  && xg_filter_key (any, event))
		{
		  /* Try to attribute core key events from the input
		     method to the input extension event that caused
		     them.  */
		  dpyinfo->pending_keystroke_time = xev->time;
		  dpyinfo->pending_keystroke_source = xev->sourceid;

		  *finish = X_EVENT_DROP;
		  goto XI_OTHER;
		}
#endif /* HAVE_X_I18N || USE_GTK */

	      state |= x_emacs_to_x_modifiers (dpyinfo, extra_keyboard_modifiers);

	      /* If mouse-highlight is an integer, input clears out
		 mouse highlighting.  */
	      if (!hlinfo->mouse_face_hidden && FIXNUMP (Vmouse_highlight)
		  && (f == 0
#if ! defined (USE_GTK)
		      || !EQ (f->tool_bar_window, hlinfo->mouse_face_window)
#endif /* !USE_GTK */
		      || !EQ (f->tab_bar_window, hlinfo->mouse_face_window))
		  )
		{
		  mouse_frame = hlinfo->mouse_face_mouse_frame;

		  clear_mouse_face (hlinfo);
		  hlinfo->mouse_face_hidden = true;

		  if (mouse_frame)
		    x_flush_dirty_back_buffer_on (mouse_frame);
		}

	      if (f != 0)
		{
#ifdef USE_GTK
		  /* Don't pass keys to GTK.  A Tab will shift focus to the
		     tool bar in GTK 2.4.  Keys will still go to menus and
		     dialogs because in that case popup_activated is nonzero
		     (see above).  */
		  *finish = X_EVENT_DROP;
#endif /* USE_GTK */

		  XSETFRAME (inev.ie.frame_or_window, f);
		  inev.ie.timestamp = xev->time;

#ifdef HAVE_X_I18N
		  if (FRAME_XIC (f))
		    {
		      Status status_return;
		      nbytes = XmbLookupString (FRAME_XIC (f),
						&xkey, (char *) copy_bufptr,
						copy_bufsiz, &keysym,
						&status_return);
		      coding = FRAME_X_XIM_CODING (f);

		      if (status_return == XBufferOverflow)
			{
			  copy_bufsiz = nbytes + 1;
			  copy_bufptr = SAFE_ALLOCA (copy_bufsiz);
			  nbytes = XmbLookupString (FRAME_XIC (f),
						    &xkey, (char *) copy_bufptr,
						    copy_bufsiz, &keysym,
						    &status_return);
			}

		      if (status_return == XLookupNone)
			goto xi_done_keysym;
		      else if (status_return == XLookupChars)
			{
			  keysym = NoSymbol;
			  state = 0;
			}
		      else if (status_return != XLookupKeySym
			       && status_return != XLookupBoth)
			emacs_abort ();
		    }
		  else
#endif /* HAVE_X_I18N */
		    {
#ifdef HAVE_XKB
		      if (dpyinfo->xkb_desc)
			{
			  KeySym sym;
			  int overflow;

			  /* Translate the keycode into the keysym it
			     represents, using STATE.  MODS_RTRN is
			     set to the modifier bits consumed while
			     undertaking this translation and should
			     be subsequently ignored during keysym
			     translation.  */

			  if (!XkbTranslateKeyCode (dpyinfo->xkb_desc,
						    keycode, state,
						    &mods_rtrn, &keysym))
			    goto xi_done_keysym;

			  /* Save the original keysym in case
			     XkbTranslateKeySym overflows.  */
			  sym = keysym, overflow = 0;

			  /* Translate this keysym and its modifier
			     state into the actual symbol and string
			     it represents.  */
			  nbytes = XkbTranslateKeySym (dpyinfo->display,
						       &keysym,
						       state & ~mods_rtrn,
						       copy_bufptr,
						       copy_bufsiz,
						       &overflow);
			  if (overflow)
			    {
			      copy_bufsiz += overflow;
			      copy_bufptr = SAFE_ALLOCA (copy_bufsiz);
			      overflow = 0;

			      /* Use the original keysym derived from
				 the keycode translation.  */
			      nbytes = XkbTranslateKeySym (dpyinfo->display,
							   &sym,
							   state & ~mods_rtrn,
							   copy_bufptr,
							   copy_bufsiz,
							   &overflow);

			      if (overflow)
				nbytes = 0;
			    }

			  coding = Qnil;
			}
		      else
#endif /* HAVE_XKB */
			{
			  /* Save the state within XKEY, then remove
			     all modifier keys Emacs understands from
			     it, forestalling any attempt by
			     XLookupString to introduce control
			     characters.  */

			  old_state = xkey.state;
			  xkey.state &= ~ControlMask;
			  xkey.state &= ~(dpyinfo->meta_mod_mask
					  | dpyinfo->super_mod_mask
					  | dpyinfo->hyper_mod_mask
					  | dpyinfo->alt_mod_mask);

			  nbytes = XLookupString (&xkey, copy_bufptr,
						  copy_bufsiz, &keysym,
						  NULL);

			  xkey.state = old_state;
			}
		    }

		  inev.ie.modifiers = x_x_to_emacs_modifiers (dpyinfo, state);

#ifdef XK_F1
		  if (x_dnd_in_progress
		      && xev->deviceid == x_dnd_keyboard_device
		      && keysym == XK_F1)
		    {
		      x_dnd_xm_use_help = true;
		      goto xi_done_keysym;
		    }
#endif /* XK_F1 */

		  /* See if keysym should make Emacs quit.  */

		  if (dpyinfo->quit_keysym)
		    {
		      if (keysym == dpyinfo->quit_keysym
			  && (xev->time - dpyinfo->quit_keysym_time
			      <= 350))
			{
			  Vquit_flag = Qt;
			  goto xi_done_keysym;
			}

		      if (keysym == dpyinfo->quit_keysym)
			{
			  /* Otherwise, set the last time that keysym
			     was pressed.  */
			  dpyinfo->quit_keysym_time = xev->time;
			  goto xi_done_keysym;
			}
		    }

		  /* First deal with keysyms which have defined
		     translations to characters.  */
		  if (keysym >= 32 && keysym < 128)
		    /* Avoid explicitly decoding each ASCII character.  */
		    {
		      inev.ie.kind = ASCII_KEYSTROKE_EVENT;
		      inev.ie.code = keysym;

		      if (source)
			inev.ie.device = source->name;

		      goto xi_done_keysym;
		    }

		  /* Keysyms directly mapped to Unicode characters.  */
		  if (keysym >= 0x01000000 && keysym <= 0x0110FFFF)
		    {
		      if (keysym < 0x01000080)
			inev.ie.kind = ASCII_KEYSTROKE_EVENT;
		      else
			inev.ie.kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;

		      if (source)
			inev.ie.device = source->name;

		      inev.ie.code = keysym & 0xFFFFFF;
		      goto xi_done_keysym;
		    }

		  /* Now non-ASCII.  */
		  if (HASH_TABLE_P (Vx_keysym_table)
		      && (c = Fgethash (make_fixnum (keysym),
					Vx_keysym_table,
					Qnil),
			  FIXNATP (c)))
		    {
		      inev.ie.kind = (SINGLE_BYTE_CHAR_P (XFIXNAT (c))
				      ? ASCII_KEYSTROKE_EVENT
				      : MULTIBYTE_CHAR_KEYSTROKE_EVENT);
		      inev.ie.code = XFIXNAT (c);

		      if (source)
			inev.ie.device = source->name;

		      goto xi_done_keysym;
		    }

		  /* Random non-modifier sorts of keysyms.  */
		  if (((keysym >= XK_BackSpace && keysym <= XK_Escape)
		       || keysym == XK_Delete
#ifdef XK_ISO_Left_Tab
		       || (keysym >= XK_ISO_Left_Tab
			   && keysym <= XK_ISO_Enter)
#endif
		       || IsCursorKey (keysym) /* 0xff50 <= x < 0xff60 */
		       || IsMiscFunctionKey (keysym) /* 0xff60 <= x < VARIES */
#ifdef HPUX
		       /* This recognizes the "extended function
			  keys".  It seems there's no cleaner way.
			  Test IsModifierKey to avoid handling
			  mode_switch incorrectly.  */
		       || (XK_Select <= keysym && keysym < XK_KP_Space)
#endif
#ifdef XK_dead_circumflex
		       || keysym == XK_dead_circumflex
#endif
#ifdef XK_dead_grave
		       || keysym == XK_dead_grave
#endif
#ifdef XK_dead_tilde
		       || keysym == XK_dead_tilde
#endif
#ifdef XK_dead_diaeresis
		       || keysym == XK_dead_diaeresis
#endif
#ifdef XK_dead_macron
		       || keysym == XK_dead_macron
#endif
#ifdef XK_dead_degree
		       || keysym == XK_dead_degree
#endif
#ifdef XK_dead_acute
		       || keysym == XK_dead_acute
#endif
#ifdef XK_dead_cedilla
		       || keysym == XK_dead_cedilla
#endif
#ifdef XK_dead_breve
		       || keysym == XK_dead_breve
#endif
#ifdef XK_dead_ogonek
		       || keysym == XK_dead_ogonek
#endif
#ifdef XK_dead_caron
		       || keysym == XK_dead_caron
#endif
#ifdef XK_dead_doubleacute
		       || keysym == XK_dead_doubleacute
#endif
#ifdef XK_dead_abovedot
		       || keysym == XK_dead_abovedot
#endif
#ifdef XK_dead_abovering
		       || keysym == XK_dead_abovering
#endif
#ifdef XK_dead_belowdot
		       || keysym == XK_dead_belowdot
#endif
#ifdef XK_dead_voiced_sound
		       || keysym == XK_dead_voiced_sound
#endif
#ifdef XK_dead_semivoiced_sound
		       || keysym == XK_dead_semivoiced_sound
#endif
#ifdef XK_dead_hook
		       || keysym == XK_dead_hook
#endif
#ifdef XK_dead_horn
		       || keysym == XK_dead_horn
#endif
#ifdef XK_dead_stroke
		       || keysym == XK_dead_stroke
#endif
#ifdef XK_dead_abovecomma
		       || keysym == XK_dead_abovecomma
#endif
		       || IsKeypadKey (keysym) /* 0xff80 <= x < 0xffbe */
		       || IsFunctionKey (keysym) /* 0xffbe <= x < 0xffe1 */
		       /* Any "vendor-specific" key is ok.  */
		       || (keysym & (1 << 28))
		       || (keysym != NoSymbol && nbytes == 0))
		      && ! (IsModifierKey (keysym)
			    /* The symbols from XK_ISO_Lock
			       to XK_ISO_Last_Group_Lock
			       don't have real modifiers but
			       should be treated similarly to
			       Mode_switch by Emacs. */
#if defined XK_ISO_Lock && defined XK_ISO_Last_Group_Lock
			    || (XK_ISO_Lock <= keysym
				&& keysym <= XK_ISO_Last_Group_Lock)
#endif
			    ))
		    {
		      STORE_KEYSYM_FOR_DEBUG (keysym);
		      /* make_lispy_event will convert this to a symbolic
			 key.  */
		      inev.ie.kind = NON_ASCII_KEYSTROKE_EVENT;
		      inev.ie.code = keysym;

		      if (source)
			inev.ie.device = source->name;

		      goto xi_done_keysym;
		    }

		  for (i = 0; i < nbytes; i++)
		    {
		      STORE_KEYSYM_FOR_DEBUG (copy_bufptr[i]);
		    }

		  if (nbytes)
		    {
		      inev.ie.kind = MULTIBYTE_CHAR_KEYSTROKE_EVENT;
		      inev.ie.arg = make_unibyte_string (copy_bufptr, nbytes);

		      Fput_text_property (make_fixnum (0), make_fixnum (nbytes),
					  Qcoding, coding, inev.ie.arg);

		      if (source)
			inev.ie.device = source->name;
		    }
		  goto xi_done_keysym;
		}

	      goto XI_OTHER;
	    }

#if defined USE_GTK && !defined HAVE_GTK3
	  case XI_RawKeyPress:
	    {
	      XIRawEvent *raw_event = (XIRawEvent *) xi_event;

	      /* This is the only way to attribute core keyboard
		 events generated on GTK+ 2.x to the extension device
		 that generated them.  */
	      dpyinfo->pending_keystroke_time = raw_event->time;
	      dpyinfo->pending_keystroke_source = raw_event->sourceid;
	      dpyinfo->pending_keystroke_time_special_p = true;
	      goto XI_OTHER;
	    }
#endif

	  case XI_KeyRelease:
#if defined HAVE_X_I18N || defined USE_GTK || defined USE_LUCID
	    {
	      XKeyPressedEvent xkey;

	      memset (&xkey, 0, sizeof xkey);

	      xkey.type = KeyRelease;
	      xkey.serial = xev->serial;
	      xkey.send_event = xev->send_event;
	      xkey.display = dpyinfo->display;
	      xkey.window = xev->event;
	      xkey.root = xev->root;
	      xkey.subwindow = xev->child;
	      xkey.time = xev->time;
	      xkey.state = xi_convert_event_keyboard_state (xev);
	      xkey.x = lrint (xev->event_x);
	      xkey.y = lrint (xev->event_y);
	      xkey.x_root = lrint (xev->root_x);
	      xkey.y_root = lrint (xev->root_y);

	      /* Some input methods react differently depending on the
		 buttons that are pressed.  */
	      xi_convert_button_state (&xev->buttons, &xkey.state);

	      xkey.keycode = xev->detail;
	      xkey.same_screen = True;

#ifdef USE_LUCID
	      if (!popup_activated ())
		{
#endif
#ifdef HAVE_X_I18N
		  if (x_filter_event (dpyinfo, (XEvent *) &xkey))
		    *finish = X_EVENT_DROP;
#elif defined USE_GTK
		  f = x_any_window_to_frame (xkey->event);

		  if (f && xg_filter_key (f, event))
		    *finish = X_EVENT_DROP;
#endif
#ifdef USE_LUCID
		}
	      else
		{
		  /* FIXME: the Lucid menu bar pops down upon any key
		     release event, so we don't dispatch these events
		     at all, which doesn't seem to be the right
		     solution.

		     use_copy = true;
		     copy.xkey = xkey; */
		}
#endif
	    }
#endif

	    goto XI_OTHER;

	  case XI_PropertyEvent:
	    goto XI_OTHER;

	  case XI_HierarchyChanged:
	    {
	      XIHierarchyEvent *hev;
	      XIDeviceInfo *info;
	      int i, ndevices, n_disabled, *disabled;
	      struct xi_device_t *device;
#if !defined USE_X_TOOLKIT && (!defined USE_GTK || defined HAVE_GTK3)
	      bool any_changed = false;
#endif /* !USE_X_TOOLKIT && (!USE_GTK || HAVE_GTK3) */
	      hev = (XIHierarchyEvent *) xi_event;
	      SAFE_NALLOCA (disabled, 1, hev->num_info);
	      n_disabled = 0;

	      for (i = 0; i < hev->num_info; ++i)
		{
		  if (hev->info[i].flags & XIDeviceEnabled)
		    {
		      /* Handle all disabled devices now, to prevent
			 things happening out-of-order later.  */

		      if (n_disabled)
			{
			  xi_disable_devices (dpyinfo, disabled, n_disabled);
			  n_disabled = 0;

#if !defined USE_X_TOOLKIT && (!defined USE_GTK || defined HAVE_GTK3)
			  /* This flag really just means that disabled
			     devices were handled early and should be
			     used in conjunction with n_disabled.  */
			  any_changed = true;
#endif /* !USE_X_TOOLKIT && (!USE_GTK || HAVE_GTK3) */
			}

		      /* Under unknown circumstances, multiple
			 XIDeviceEnabled events are sent at once,
			 causing the device to be duplicated.  Check
			 that the device doesn't exist before adding
			 it.  */

		      if (!xi_device_from_id (dpyinfo,
					      hev->info[i].deviceid))
			{
			  x_catch_errors (dpyinfo->display);
			  info = XIQueryDevice (dpyinfo->display,
						hev->info[i].deviceid,
						&ndevices);
			  x_uncatch_errors ();

			  if (info && info->enabled)
			    {
			      dpyinfo->devices
				= xrealloc (dpyinfo->devices,
					    (sizeof *dpyinfo->devices
					     * ++dpyinfo->num_devices));
			      memset (dpyinfo->devices + dpyinfo->num_devices - 1,
				      0, sizeof *dpyinfo->devices);
			      device = &dpyinfo->devices[dpyinfo->num_devices - 1];
			      xi_populate_device_from_info (dpyinfo, device, info);
			    }

			  if (info)
			    XIFreeDeviceInfo (info);
			}
		    }
		  else if (hev->info[i].flags & XIDeviceDisabled)
		    disabled[n_disabled++] = hev->info[i].deviceid;
		  else if (hev->info[i].flags & XISlaveDetached
			   || hev->info[i].flags & XISlaveAttached)
		    {
		      device = xi_device_from_id (dpyinfo, hev->info[i].deviceid);
		      x_catch_errors (dpyinfo->display);
		      info = XIQueryDevice (dpyinfo->display, hev->info[i].deviceid,
					    &ndevices);
		      x_uncatch_errors ();

		      if (info)
			{
			  if (device)
			    {
			      device->use = info->use;
			      device->attachment = info->attachment;
			    }

			  /* device could have been disabled by now.
			     But instead of removing it immediately,
			     wait for XIDeviceDisabled, or internal
			     state could be left inconsistent.  */

			  XIFreeDeviceInfo (info);
			}
		    }
		}

	      /* Delete all devices that were disabled by this
		 event.  */
	      xi_disable_devices (dpyinfo, disabled, n_disabled);

#if !defined USE_X_TOOLKIT && (!defined USE_GTK || defined HAVE_GTK3)
	      /* If the device hierarchy has been changed, recompute
		 focus.  This might seem like a micro-optimization but
		 it actually keeps the focus from changing in some
		 cases where it would be undesierable.  */
	      if (any_changed || n_disabled)
		xi_handle_focus_change (dpyinfo);
#endif /* !USE_X_TOOLKIT && (!USE_GTK || HAVE_GTK3) */

	      goto XI_OTHER;
	    }

	  case XI_DeviceChanged:
	    {
	      XIDeviceChangedEvent *device_changed;
	      struct xi_device_t *device;

	      device_changed = (XIDeviceChangedEvent *) xi_event;
	      device = xi_device_from_id (dpyinfo, device_changed->deviceid);

	      /* If the device isn't enabled, then stop handling this
		 event.  A HierarchyChanged event will be sent if it
		 is enabled afterwards.  */
	      if (!device)
		goto XI_OTHER;

	      /* Now handle the event by retrieving scroll valuators
		 and touch info.  */
	      xi_handle_device_changed (dpyinfo, device, device_changed);
	      goto XI_OTHER;
	    }

#ifdef HAVE_XINPUT2_2
	  case XI_TouchBegin:
	    {
	      struct xi_device_t *device, *source;
	      bool menu_bar_p = false, tool_bar_p = false;
	      EMACS_INT local_detail;
	      device = xi_device_from_id (dpyinfo, xev->deviceid);
	      source = xi_device_from_id (dpyinfo, xev->sourceid);
	      x_display_set_last_user_time (dpyinfo, xev->time,
					    xev->send_event, true);

	      /* Don't process touch sequences from this device if
	         it's a master pointer.  Touch sequences aren't
	         canceled by the X server if a slave device is
	         detached, and master pointers may also represent
	         dependent touch devices.  */

	      if (!device || device->use == XIMasterPointer)
		goto XI_OTHER;

	      if (xi_find_touch_point (device, xev->detail))
		emacs_abort ();

	      f = x_window_to_frame (dpyinfo, xev->event);

	      if (f)
		/* See the comment above x_compute_root_window_offset
		   for why this optimization is performed.  */
		xi_compute_root_window_offset (f, xev);

#ifdef HAVE_GTK3
	      menu_bar_p = (f && FRAME_X_OUTPUT (f)->menubar_widget
			    && xg_event_is_for_menubar (f, event));
	      if (f && FRAME_X_OUTPUT (f)->toolbar_widget)
		{
		  int scale = xg_get_scale (f);

		  GdkRectangle test_rect;
		  test_rect.x = xev->event_x / scale;
		  test_rect.y = xev->event_y / scale;
		  test_rect.width = 1;
		  test_rect.height = 1;

		  tool_bar_p = gtk_widget_intersect (FRAME_X_OUTPUT (f)->toolbar_widget,
						     &test_rect, NULL);
		}
#endif

#ifndef HAVE_EXT_TOOL_BAR
	      /* Is this a touch from a direct touch device that is in
		 the tool-bar?  */
	      if (device->direct_p
		  && WINDOWP (f->tool_bar_window)
		  && WINDOW_TOTAL_LINES (XWINDOW (f->tool_bar_window)))
		{
		  Lisp_Object window;
		  int x = xev->event_x;
		  int y = xev->event_y;

		  window = window_from_coordinates (f, x, y, 0, true, true, true);
		  /* Ignore button release events if the mouse
		     wasn't previously pressed on the tool bar.
		     We do this because otherwise selecting some
		     text with the mouse and then releasing it on
		     the tool bar doesn't stop selecting text,
		     since the tool bar eats the button up
		     event.  */
		  tool_bar_p = EQ (window, f->tool_bar_window);

		  /* If this touch has started in the tool bar, do not
		     send it to Lisp.  Instead, simulate a tool bar
		     click, releasing it once it goes away.  */

		  if (tool_bar_p)
		    {
		      /* Call note_mouse_highlight on the tool bar
			 item.  Otherwise, get_tool_bar_item will
			 return 1.

		         This is not necessary when mouse-highlight is
		         nil.  */

		      if (!NILP (Vmouse_highlight))
			{
			  note_mouse_highlight (f, x, y);

			  /* Always allow future mouse motion to
			     update the mouse highlight, no matter
			     where it is.  */
			  memset (&dpyinfo->last_mouse_glyph, 0,
				  sizeof dpyinfo->last_mouse_glyph);
			  dpyinfo->last_mouse_glyph_frame = f;
			}

		      handle_tool_bar_click_with_device (f, x, y, true, 0,
							 (source
							  ? source->name : Qt));

		      /* Flush any changes made by that to the front
			 buffer.  */
		      x_flush_dirty_back_buffer_on (f);

		      /* Record the device and the touch ID on the
			 frame.  That way, Emacs knows when to dismiss
			 the tool bar click later.  */

		      FRAME_OUTPUT_DATA (f)->tool_bar_touch_device
			= device->device_id;
		      FRAME_OUTPUT_DATA (f)->tool_bar_touch_id = xev->detail;

		      goto XI_OTHER;
		    }
		}
#endif

	      if (!menu_bar_p && !tool_bar_p)
		{
		  if (f && device->direct_p)
		    {
		      *finish = X_EVENT_DROP;

		      x_catch_errors (dpyinfo->display);

		      if (x_input_grab_touch_events)
			XIAllowTouchEvents (dpyinfo->display,
					    xev->deviceid,
					    xev->detail, xev->event,
					    XIAcceptTouch);

		      if (!x_had_errors_p (dpyinfo->display))
			{
			  local_detail
			    = xi_link_touch_point (device, xev->detail,
						   xev->event_x,
						   xev->event_y, f);

			  inev.ie.kind = TOUCHSCREEN_BEGIN_EVENT;
			  inev.ie.timestamp = xev->time;
			  XSETFRAME (inev.ie.frame_or_window, f);
			  XSETINT (inev.ie.x, lrint (xev->event_x));
			  XSETINT (inev.ie.y, lrint (xev->event_y));
			  XSETINT (inev.ie.arg, local_detail);

			  if (source)
			    inev.ie.device = source->name;
			}

		      x_uncatch_errors ();
		    }
#ifndef HAVE_GTK3
		  else if (x_input_grab_touch_events)
		    {
		      x_ignore_errors_for_next_request (dpyinfo, 0);
		      XIAllowTouchEvents (dpyinfo->display, xev->deviceid,
					  xev->detail, xev->event, XIRejectTouch);
		      x_stop_ignoring_errors (dpyinfo);
		    }
#endif
		}
	      else
		{
#ifdef HAVE_GTK3
		  bool was_waiting_for_input = waiting_for_input;
		  /* This hack was adopted from the NS port.  Whether
		     or not it is actually safe is a different story
		     altogether.  */
		  if (waiting_for_input)
		    waiting_for_input = 0;
		  set_frame_menubar (f, true);
		  waiting_for_input = was_waiting_for_input;
#endif
		}

	      goto XI_OTHER;
	    }

	  case XI_TouchOwnership:
	    {
	      struct xi_device_t *device;
	      struct xi_touch_point_t *touchpoint;
	      XITouchOwnershipEvent *event;

	      /* All grabbing clients have decided to reject ownership
		 of this touch sequence.  */

	      event  = (XITouchOwnershipEvent *) xi_event;
	      device = xi_device_from_id (dpyinfo, event->deviceid);

	      if (!device || device->use == XIMasterPointer)
		goto XI_OTHER;

	      touchpoint = xi_find_touch_point (device, event->touchid);

	      if (!touchpoint)
		goto XI_OTHER;

	      /* As a result, Emacs should complete whatever editing
		 operations result from this touch sequence.  */
	      touchpoint->ownership = TOUCH_OWNERSHIP_SELF;

	      goto XI_OTHER;
	    }

	  case XI_TouchUpdate:
	    {
	      struct xi_device_t *device, *source;
	      struct xi_touch_point_t *touchpoint;
	      Lisp_Object arg = Qnil;

	      /* If flags & TouchPendingEnd, the touch sequence has
		 already ended, but some grabbing clients remain
		 undecided as to whether they will obtain ownership of
		 the touch sequence.

	         Wait for them to make their decision, resulting in
	         TouchOwnership and TouchEnd events being sent.  */

	      if (xev->flags & XITouchPendingEnd)
		goto XI_OTHER;

	      device = xi_device_from_id (dpyinfo, xev->deviceid);
	      source = xi_device_from_id (dpyinfo, xev->sourceid);
	      x_display_set_last_user_time (dpyinfo, xev->time,
					    xev->send_event, true);

	      /* Don't process touch sequences from this device if
	         it's a master pointer.  Touch sequences aren't
	         canceled by the X server if a slave device is
	         detached, and master pointers may also represent
	         dependent touch devices.  */

	      if (!device || device->use == XIMasterPointer)
		goto XI_OTHER;

	      touchpoint = xi_find_touch_point (device, xev->detail);

	      if (!touchpoint
		  /* Don't send this event if nothing has changed
		     either.  */
		  || (touchpoint->x == lrint (xev->event_x)
		      && touchpoint->y == lrint (xev->event_y)))
		goto XI_OTHER;

	      touchpoint->x = lrint (xev->event_x);
	      touchpoint->y = lrint (xev->event_y);

	      f = x_window_to_frame (dpyinfo, xev->event);

	      if (f && device->direct_p)
		{
		  inev.ie.kind = TOUCHSCREEN_UPDATE_EVENT;
		  inev.ie.timestamp = xev->time;
		  XSETFRAME (inev.ie.frame_or_window, f);

		  for (touchpoint = device->touchpoints;
		       touchpoint; touchpoint = touchpoint->next)
		    {
		      if (touchpoint->frame == f)
			arg = Fcons (list3i (touchpoint->x, touchpoint->y,
					     touchpoint->local_detail),
				     arg);
		    }

		  if (source)
		    inev.ie.device = source->name;

		  inev.ie.arg = arg;
		}

	      goto XI_OTHER;
	    }

	  case XI_TouchEnd:
	    {
	      struct xi_device_t *device, *source;
	      int state;
	      EMACS_INT local_detail;

	      device = xi_device_from_id (dpyinfo, xev->deviceid);
	      source = xi_device_from_id (dpyinfo, xev->sourceid);
	      x_display_set_last_user_time (dpyinfo, xev->time,
					    xev->send_event, true);

	      /* Don't process touch sequences from this device if
	         it's a master pointer.  Touch sequences aren't
	         canceled by the X server if a slave device is
	         detached, and master pointers may also represent
	         dependent touch devices.  */

	      if (!device || device->use == XIMasterPointer)
		goto XI_OTHER;

	      state = xi_unlink_touch_point (xev->detail, device,
					     &local_detail);

	      if (state)
		{
		  f = x_window_to_frame (dpyinfo, xev->event);

		  if (f && device->direct_p)
		    {
		      inev.ie.kind = TOUCHSCREEN_END_EVENT;
		      inev.ie.timestamp = xev->time;
		      inev.ie.modifiers = state != 2;

		      XSETFRAME (inev.ie.frame_or_window, f);
		      XSETINT (inev.ie.x, lrint (xev->event_x));
		      XSETINT (inev.ie.y, lrint (xev->event_y));
		      XSETINT (inev.ie.arg, local_detail);

		      if (source)
			inev.ie.device = source->name;
		    }
		}

#ifndef HAVE_EXT_TOOL_BAR
	      /* Now see if the touchpoint was previously on the tool bar.
	         If it was, release the tool bar.  */

	      if (!f)
		f = x_window_to_frame (dpyinfo, xev->event);

	      if (f && (FRAME_OUTPUT_DATA (f)->tool_bar_touch_id
			== xev->detail))
		{
		  if (f->last_tool_bar_item != -1)
		    handle_tool_bar_click_with_device (f, xev->event_x,
						       xev->event_y,
						       false, 0,
						       (source
							? source->name
							: Qnil));

		  /* Cancel any outstanding mouse highlight.  */
		  note_mouse_highlight (f, -1, -1);
		  x_flush_dirty_back_buffer_on (f);

		  /* Now clear the tool bar device.  */
		  FRAME_OUTPUT_DATA (f)->tool_bar_touch_device = 0;
		}
#endif

	      goto XI_OTHER;
	    }

#endif

#ifdef HAVE_XINPUT2_4
	  case XI_GesturePinchBegin:
	  case XI_GesturePinchUpdate:
	    {
	      XIGesturePinchEvent *pev = (XIGesturePinchEvent *) xi_event;
	      struct xi_device_t *device, *source;

	      device = xi_device_from_id (dpyinfo, pev->deviceid);
	      source = xi_device_from_id (dpyinfo, pev->sourceid);
	      x_display_set_last_user_time (dpyinfo, pev->time,
					    pev->send_event, true);

	      if (!device || device->use != XIMasterPointer)
		goto XI_OTHER;

#ifdef HAVE_XWIDGETS
	      struct xwidget_view *xvw = xwidget_view_from_window (pev->event);

	      if (xvw)
		{
		  *finish = X_EVENT_DROP;
		  xwidget_pinch (xvw, pev);
		  goto XI_OTHER;
		}
#endif

	      any = x_window_to_frame (dpyinfo, pev->event);

	      if (any)
		{
		  if (pev->event == FRAME_X_WINDOW (any))
		    xi_compute_root_window_offset_pinch (any, pev);

		  inev.ie.kind = PINCH_EVENT;
		  inev.ie.modifiers
		    = x_x_to_emacs_modifiers (dpyinfo, pev->mods.effective);

		  XSETINT (inev.ie.x, lrint (pev->event_x));
		  XSETINT (inev.ie.y, lrint (pev->event_y));
		  XSETFRAME (inev.ie.frame_or_window, any);
		  inev.ie.arg = list4 (make_float (pev->delta_x),
				       make_float (pev->delta_y),
				       make_float (pev->scale),
				       make_float (pev->delta_angle));

		  if (source)
		    inev.ie.device = source->name;
		}

	      /* Once again GTK seems to crash when confronted by
		 events it doesn't understand.  */
	      *finish = X_EVENT_DROP;
	      goto XI_OTHER;
	    }

	  case XI_GesturePinchEnd:
	    {
#if defined HAVE_XWIDGETS
	      XIGesturePinchEvent *pev = (XIGesturePinchEvent *) xi_event;
	      struct xwidget_view *xvw = xwidget_view_from_window (pev->event);

	      if (xvw)
		xwidget_pinch (xvw, pev);
#endif
	      *finish = X_EVENT_DROP;
	      goto XI_OTHER;
	    }
#endif
	  default:
	    goto XI_OTHER;
	  }

      xi_done_keysym:
#ifdef HAVE_X_I18N
      if (f)
	{
	  struct window *w = XWINDOW (f->selected_window);
	  xic_set_preeditarea (w, w->cursor.x, w->cursor.y);

	  if (FRAME_XIC (f) && (FRAME_XIC_STYLE (f) & XIMStatusArea))
            xic_set_statusarea (f);
	}
#endif
	if (must_free_data)
	  XFreeEventData (dpyinfo->display, &event->xcookie);
	goto done_keysym;

      XI_OTHER:
	if (must_free_data)
	  XFreeEventData (dpyinfo->display, &event->xcookie);
	goto OTHER;
      }
#endif

    default:
#ifdef HAVE_XKB
      if (dpyinfo->supports_xkb
	  && event->type == dpyinfo->xkb_event_type)
	{
	  XkbEvent *xkbevent = (XkbEvent *) event;

	  if (xkbevent->any.xkb_type == XkbNewKeyboardNotify
	      || xkbevent->any.xkb_type == XkbMapNotify)
	    {
	      XkbRefreshKeyboardMapping (&xkbevent->map);

	      if (dpyinfo->xkb_desc)
		{
		  if (XkbGetUpdatedMap (dpyinfo->display,
					(XkbKeySymsMask
					 | XkbKeyTypesMask
					 | XkbModifierMapMask
					 | XkbVirtualModsMask),
					dpyinfo->xkb_desc) == Success)
		    XkbGetNames (dpyinfo->display, XkbAllNamesMask,
				 dpyinfo->xkb_desc);
		  else
		    {
		      XkbFreeKeyboard (dpyinfo->xkb_desc,
				       XkbAllComponentsMask, True);
		      dpyinfo->xkb_desc = NULL;
		    }
		}
	      else
		{
		  dpyinfo->xkb_desc = XkbGetMap (dpyinfo->display,
						 (XkbKeySymsMask
						  | XkbKeyTypesMask
						  | XkbModifierMapMask
						  | XkbVirtualModsMask),
						 XkbUseCoreKbd);

		  if (dpyinfo->xkb_desc)
		    XkbGetNames (dpyinfo->display, XkbAllNamesMask,
				 dpyinfo->xkb_desc);
		}

	      x_find_modifier_meanings (dpyinfo);
	    }
	  else if (x_dnd_in_progress
		   && xkbevent->any.xkb_type == XkbStateNotify)
	    x_dnd_keyboard_state = (xkbevent->state.mods
				    | xkbevent->state.ptr_buttons);
	}
#endif
#ifdef HAVE_XSHAPE
      if (dpyinfo->xshape_supported_p
	  && event->type == dpyinfo->xshape_event_base + ShapeNotify
	  && x_dnd_in_progress && x_dnd_use_toplevels
	  && dpyinfo == FRAME_DISPLAY_INFO (x_dnd_frame))
	{
#ifndef USE_GTK
	  XEvent xevent;
#endif
	  XShapeEvent *xse = (XShapeEvent *) event;
#if defined HAVE_XCB_SHAPE && defined HAVE_XCB_SHAPE_INPUT_RECTS
	  xcb_shape_get_rectangles_cookie_t bounding_rect_cookie;
	  xcb_shape_get_rectangles_reply_t *bounding_rect_reply;
	  xcb_rectangle_iterator_t bounding_rect_iterator;

	  xcb_shape_get_rectangles_cookie_t input_rect_cookie;
	  xcb_shape_get_rectangles_reply_t *input_rect_reply;
	  xcb_rectangle_iterator_t input_rect_iterator;

	  xcb_generic_error_t *error;
#else
	  XRectangle *rects;
	  int rc, ordering;
#endif

	  /* Somehow this really interferes with GTK's own processing
	     of ShapeNotify events.  Not sure what GTK uses them for,
	     but we cannot skip any of them here.  */
#ifndef USE_GTK
	  while (XPending (dpyinfo->display))
	    {
	      XNextEvent (dpyinfo->display, &xevent);

	      if (xevent.type == dpyinfo->xshape_event_base + ShapeNotify
		  && ((XShapeEvent *) &xevent)->window == xse->window)
		xse = (XShapeEvent *) &xevent;
	      else
		{
		  XPutBackEvent (dpyinfo->display, &xevent);
		  break;
		}
	    }
#endif

	  for (struct x_client_list_window *tem = x_dnd_toplevels; tem;
	       tem = tem->next)
	    {
	      if (tem->window == xse->window)
		{
		  if (tem->n_input_rects != -1)
		    xfree (tem->input_rects);
		  if (tem->n_bounding_rects != -1)
		    xfree (tem->bounding_rects);

		  tem->n_input_rects = -1;
		  tem->n_bounding_rects = -1;

#if defined HAVE_XCB_SHAPE && defined HAVE_XCB_SHAPE_INPUT_RECTS
		  bounding_rect_cookie = xcb_shape_get_rectangles (dpyinfo->xcb_connection,
								   (xcb_window_t) xse->window,
								   XCB_SHAPE_SK_BOUNDING);
		  if (dpyinfo->xshape_major > 1
		      || (dpyinfo->xshape_major == 1
			  && dpyinfo->xshape_minor >= 1))
		    input_rect_cookie
		      = xcb_shape_get_rectangles (dpyinfo->xcb_connection,
						  (xcb_window_t) xse->window,
						  XCB_SHAPE_SK_INPUT);

		  bounding_rect_reply = xcb_shape_get_rectangles_reply (dpyinfo->xcb_connection,
									bounding_rect_cookie,
									&error);

		  if (bounding_rect_reply)
		    {
		      bounding_rect_iterator
			= xcb_shape_get_rectangles_rectangles_iterator (bounding_rect_reply);
		      tem->n_bounding_rects = bounding_rect_iterator.rem + 1;
		      tem->bounding_rects = xmalloc (tem->n_bounding_rects
						     * sizeof *tem->bounding_rects);
		      tem->n_bounding_rects = 0;

		      for (; bounding_rect_iterator.rem; xcb_rectangle_next (&bounding_rect_iterator))
			{
			  tem->bounding_rects[tem->n_bounding_rects].x
			    = bounding_rect_iterator.data->x;
			  tem->bounding_rects[tem->n_bounding_rects].y
			    = bounding_rect_iterator.data->y;
			  tem->bounding_rects[tem->n_bounding_rects].width
			    = bounding_rect_iterator.data->width;
			  tem->bounding_rects[tem->n_bounding_rects].height
			    = bounding_rect_iterator.data->height;

			  tem->n_bounding_rects++;
			}

		      free (bounding_rect_reply);
		    }
		  else
		    free (error);

		  if (dpyinfo->xshape_major > 1
		      || (dpyinfo->xshape_major == 1
			  && dpyinfo->xshape_minor >= 1))
		    {
		      input_rect_reply = xcb_shape_get_rectangles_reply (dpyinfo->xcb_connection,
									 input_rect_cookie, &error);

		      if (input_rect_reply)
			{
			  input_rect_iterator
			    = xcb_shape_get_rectangles_rectangles_iterator (input_rect_reply);
			  tem->n_input_rects = input_rect_iterator.rem + 1;
			  tem->input_rects = xmalloc (tem->n_input_rects
						      * sizeof *tem->input_rects);
			  tem->n_input_rects = 0;

			  for (; input_rect_iterator.rem; xcb_rectangle_next (&input_rect_iterator))
			    {
			      tem->input_rects[tem->n_input_rects].x
				= input_rect_iterator.data->x;
			      tem->input_rects[tem->n_input_rects].y
				= input_rect_iterator.data->y;
			      tem->input_rects[tem->n_input_rects].width
				= input_rect_iterator.data->width;
			      tem->input_rects[tem->n_input_rects].height
				= input_rect_iterator.data->height;

			      tem->n_input_rects++;
			    }

			  free (input_rect_reply);
			}
		      else
			free (error);
		    }
#else
		  x_catch_errors (dpyinfo->display);
		  rects = XShapeGetRectangles (dpyinfo->display,
					       xse->window,
					       ShapeBounding,
					       &count, &ordering);
		  rc = x_had_errors_p (dpyinfo->display);
		  x_uncatch_errors_after_check ();

		  /* Does XShapeGetRectangles allocate anything upon an
		     error?  */
		  if (!rc)
		    {
		      tem->n_bounding_rects = count;
		      tem->bounding_rects
			= xmalloc (sizeof *tem->bounding_rects * count);
		      memcpy (tem->bounding_rects, rects,
			      sizeof *tem->bounding_rects * count);

		      XFree (rects);
		    }

#ifdef ShapeInput
		  if (dpyinfo->xshape_major > 1
		      || (dpyinfo->xshape_major == 1
			  && dpyinfo->xshape_minor >= 1))
		    {
		      x_catch_errors (dpyinfo->display);
		      rects = XShapeGetRectangles (dpyinfo->display,
						   xse->window, ShapeInput,
						   &count, &ordering);
		      rc = x_had_errors_p (dpyinfo->display);
		      x_uncatch_errors_after_check ();

		      /* Does XShapeGetRectangles allocate anything upon
			 an error?  */
		      if (!rc)
			{
			  tem->n_input_rects = count;
			  tem->input_rects
			    = xmalloc (sizeof *tem->input_rects * count);
			  memcpy (tem->input_rects, rects,
				  sizeof *tem->input_rects * count);

			  XFree (rects);
			}
		    }
#endif
#endif

		  /* Handle the common case where the input shape equals the
		     bounding shape.  */

		  if (tem->n_input_rects != -1
		      && tem->n_bounding_rects == tem->n_input_rects
		      && !memcmp (tem->bounding_rects, tem->input_rects,
				  tem->n_input_rects * sizeof *tem->input_rects))
		    {
		      xfree (tem->input_rects);
		      tem->n_input_rects = -1;
		    }

		  /* And the common case where there is no input rect and the
		     bounding rect equals the window dimensions.  */

		  if (tem->n_input_rects == -1
		      && tem->n_bounding_rects == 1
		      && tem->bounding_rects[0].width == tem->width
		      && tem->bounding_rects[0].height == tem->height
		      && tem->bounding_rects[0].x == -tem->border_width
		      && tem->bounding_rects[0].y == -tem->border_width)
		    {
		      xfree (tem->bounding_rects);
		      tem->n_bounding_rects = -1;
		    }

		  break;
		}
	    }
	}
#endif
#if defined HAVE_XRANDR && !defined USE_GTK
      if (dpyinfo->xrandr_supported_p
	  && (event->type == (dpyinfo->xrandr_event_base
			      + RRScreenChangeNotify)
	      || event->type == (dpyinfo->xrandr_event_base
				 + RRNotify)))
	{
	  Time timestamp;
	  Lisp_Object current_monitors;
	  XRRScreenChangeNotifyEvent *notify;

	  if (event->type == (dpyinfo->xrandr_event_base
			      + RRScreenChangeNotify))
	    XRRUpdateConfiguration ((XEvent *) event);

	  if (event->type == (dpyinfo->xrandr_event_base
			      + RRScreenChangeNotify))
	    {
	      notify = ((XRRScreenChangeNotifyEvent *) event);
	      timestamp = notify->timestamp;

	      /* Don't set screen dimensions if the notification is
		 for a different screen.  */
	      if (notify->root == dpyinfo->root_window)
		{
		  dpyinfo->screen_width = notify->width;
		  dpyinfo->screen_height = notify->height;
		  dpyinfo->screen_mm_width = notify->mwidth;
		  dpyinfo->screen_mm_height = notify->mheight;
		}
	    }
	  else
	    timestamp = 0;

	  if (x_find_monitors_changed_event (dpyinfo))
	    /* Don't store a MONITORS_CHANGED_EVENT if there is
	       already an undelivered event on the queue.  */
	    goto OTHER;

	  inev.ie.kind = MONITORS_CHANGED_EVENT;
	  inev.ie.timestamp = timestamp;
	  XSETTERMINAL (inev.ie.arg, dpyinfo->terminal);

	  /* Also don't do anything if the monitor configuration
	     didn't really change.  */

	  current_monitors
	    = Fx_display_monitor_attributes_list (inev.ie.arg);

	  if (!NILP (Fequal (current_monitors,
			     dpyinfo->last_monitor_attributes_list)))
	    inev.ie.kind = NO_EVENT;

	  dpyinfo->last_monitor_attributes_list = current_monitors;

	  if (x_dnd_in_progress && x_dnd_update_tooltip)
	    x_dnd_monitors = current_monitors;

	  if (inev.ie.kind != NO_EVENT)
	    x_dnd_update_tooltip_now ();
	}
#endif
#ifdef HAVE_XFIXES
      if (dpyinfo->xfixes_supported_p
	  && event->type == (dpyinfo->xfixes_event_base
			     + XFixesSelectionNotify)
	  && x_handle_selection_monitor_event (dpyinfo, event))
	/* GTK 3 crashes if an XFixesSelectionNotify arrives with a
	   window other than the root window, because it wants to know
	   the screen in order to determine the compositing manager
	   selection name.  (bug#58584) */
	*finish = X_EVENT_DROP;
#endif
    OTHER:
#ifdef USE_X_TOOLKIT
      if (*finish != X_EVENT_DROP)
	{
	  /* Ignore some obviously bogus ConfigureNotify events that
	     other clients have been known to send Emacs.
	     (bug#54051) */
	  if (event->type != ConfigureNotify
	      || (event->xconfigure.width != 0
		  && event->xconfigure.height != 0))
	    {
#if defined USE_X_TOOLKIT && defined HAVE_XINPUT2
	      XtDispatchEvent (use_copy ? &copy : (XEvent *) event);
#else
	      XtDispatchEvent ((XEvent *) event);
#endif
	    }
	}
#endif /* USE_X_TOOLKIT */
#if defined USE_GTK && !defined HAVE_GTK3 && defined HAVE_XINPUT2
      if (*finish != X_EVENT_DROP && copy)
	{
	  gtk_main_do_event (copy);
	  *finish = X_EVENT_DROP;
	}

      if (copy)
	gdk_event_free (copy);
#endif
    break;
    }

 done:
  if (inev.ie.kind != NO_EVENT)
    {
      kbd_buffer_store_buffered_event (&inev, hold_quit);
      count++;
    }

  if (do_help
      && !(hold_quit && hold_quit->kind != NO_EVENT))
    {
      Lisp_Object frame;

      if (f)
	XSETFRAME (frame, f);
      else
	frame = Qnil;

      if (do_help > 0)
	{
	  any_help_event_p = true;
#ifdef HAVE_XINPUT2
	  if (gen_help_device)
	    xi_handle_interaction (dpyinfo, f,
				   gen_help_device,
				   gen_help_time);
#endif
	  gen_help_event (help_echo_string, frame, help_echo_window,
			  help_echo_object, help_echo_pos);
	}
      else
	{
	  help_echo_string = Qnil;
	  gen_help_event (Qnil, frame, Qnil, Qnil, 0);
	}
      count++;
    }

#if defined HAVE_XINPUT2 || defined HAVE_XKB || defined HAVE_X_I18N
  SAFE_FREE ();
#endif

  return count;
}

/* Handles the XEvent EVENT on display DISPLAY.
   This is used for event loops outside the normal event handling,
   i.e. looping while a popup menu or a dialog is posted.

   Returns the value handle_one_xevent sets in the finish argument.  */

#ifdef USE_GTK
static int
#else
int
#endif
x_dispatch_event (XEvent *event, Display *display)
{
  struct x_display_info *dpyinfo;
  int finish = X_EVENT_NORMAL;

  dpyinfo = x_display_info_for_display (display);

  if (dpyinfo)
    {
      /* Block input before calling x_dispatch_event.  */
      block_input ();
      handle_one_xevent (dpyinfo, event, &finish, 0);
      unblock_input ();
    }

  return finish;
}

/* Read events coming from the X server.
   Return as soon as there are no more events to be read.

   Return the number of characters stored into the buffer,
   thus pretending to be `read' (except the characters we store
   in the keyboard buffer can be multibyte, so are not necessarily
   C chars).  */

static int
XTread_socket (struct terminal *terminal, struct input_event *hold_quit)
{
  int count = 0;
  bool event_found = false;
  struct x_display_info *dpyinfo = terminal->display_info.x;

  /* Don't allow XTread_socket to do anything if drag-and-drop is in
     progress.  If unblock_input causes XTread_socket to be called and
     read X events while the drag-and-drop event loop is in progress,
     things can go wrong very quick.

     When x_dnd_unwind_flag is true, the above doesn't apply, since
     the surrounding code takes special precautions to keep it safe.

     That doesn't matter for events from displays other than the
     display of the drag-and-drop operation, though.  */
  if (!x_dnd_unwind_flag
      && ((x_dnd_in_progress
	   && dpyinfo->display == FRAME_X_DISPLAY (x_dnd_frame))
	  || (x_dnd_waiting_for_finish
	      && dpyinfo->display == x_dnd_finish_display)))
    return 0;

  x_clean_failable_requests (dpyinfo);

  block_input ();

  /* For debugging, this gives a way to fake an I/O error.  */
  if (dpyinfo == XTread_socket_fake_io_error)
    {
      XTread_socket_fake_io_error = 0;
      x_io_error_quitter (dpyinfo->display);
    }

#ifndef USE_GTK
  while (XPending (dpyinfo->display))
    {
      int finish;
      XEvent event;

      XNextEvent (dpyinfo->display, &event);

#ifdef HAVE_X_I18N
      /* Filter events for the current X input method.  */
#ifdef HAVE_XINPUT2
      if (event.type != GenericEvent
	  || !dpyinfo->supports_xi2
	  || event.xgeneric.extension != dpyinfo->xi2_opcode)
	{
	  /* Input extension key events are filtered inside
	     handle_one_xevent.  */
#endif
	  if (x_filter_event (dpyinfo, &event))
	    continue;
#ifdef HAVE_XINPUT2
	}
#endif
#endif
      event_found = true;

      count += handle_one_xevent (dpyinfo, &event, &finish, hold_quit);

      if (finish == X_EVENT_GOTO_OUT)
	break;
    }

#else /* USE_GTK */

  /* For GTK we must use the GTK event loop.  But XEvents gets passed
     to our filter function above, and then to the big event switch.
     We use a bunch of globals to communicate with our filter function,
     that is kind of ugly, but it works.

     There is no way to do one display at the time, GTK just does events
     from all displays.  */

  while (gtk_events_pending ())
    {
      current_count = count;
      current_hold_quit = hold_quit;

      gtk_main_iteration ();

      count = current_count;
      current_count = -1;
      current_hold_quit = 0;

      if (current_finish == X_EVENT_GOTO_OUT)
        break;
    }

  /* Now see if `xg_pending_quit_event' was set.  */
  if (xg_pending_quit_event.kind != NO_EVENT)
    {
      /* Check that the frame is still valid.  It could have been
	 deleted between now and the time the event was recorded.  */
      if (FRAME_LIVE_P (XFRAME (xg_pending_quit_event.frame_or_window)))
	/* Store that event into hold_quit and clear the pending quit
	   event.  */
	*hold_quit = xg_pending_quit_event;

      /* If the frame is invalid, just clear the event as well.  */
      xg_pending_quit_event.kind = NO_EVENT;
    }
#endif /* USE_GTK */

  /* On some systems, an X bug causes Emacs to get no more events
     when the window is destroyed.  Detect that.  (1994.)  */
  if (! event_found)
    {
      /* Emacs and the X Server eats up CPU time if XNoOp is done every time.
	 One XNOOP in 100 loops will make Emacs terminate.
	 B. Bretthauer, 1994 */
      x_noop_count++;
      if (x_noop_count >= 100)
	{
	  x_noop_count=0;

	  if (next_noop_dpyinfo == 0)
	    next_noop_dpyinfo = x_display_list;

	  XNoOp (next_noop_dpyinfo->display);

	  /* Each time we get here, cycle through the displays now open.  */
	  next_noop_dpyinfo = next_noop_dpyinfo->next;
	}
    }

  /* If the focus was just given to an auto-raising frame,
     raise it now.  FIXME: handle more than one such frame.  */
  if (dpyinfo->x_pending_autoraise_frame)
    {
      x_raise_frame (dpyinfo->x_pending_autoraise_frame);
      dpyinfo->x_pending_autoraise_frame = NULL;
    }

  unblock_input ();

  return count;
}




/***********************************************************************
			     Text Cursor
 ***********************************************************************/

/* Set clipping for output in glyph row ROW.  W is the window in which
   we operate.  GC is the graphics context to set clipping in.

   If RECT_RETURN is non-NULL, return the clip rectangle within
   *RECT_RETURN.

   ROW may be a text row or, e.g., a mode line.  Text rows must be
   clipped to the interior of the window dedicated to text display,
   mode lines must be clipped to the whole window.  */

static void
x_clip_to_row (struct window *w, struct glyph_row *row,
	       enum glyph_row_area area, GC gc,
	       XRectangle *rect_return)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  XRectangle clip_rect;
  int window_x, window_y, window_width;

  window_box (w, area, &window_x, &window_y, &window_width, 0);

  clip_rect.x = window_x;
  clip_rect.y = WINDOW_TO_FRAME_PIXEL_Y (w, max (0, row->y));
  clip_rect.y = max (clip_rect.y, window_y);
  clip_rect.width = window_width;
  clip_rect.height = row->visible_height;

  x_set_clip_rectangles (f, gc, &clip_rect, 1);

  if (rect_return)
    *rect_return = clip_rect;
}


/* Draw a hollow box cursor on window W in glyph row ROW.  */

static void
x_draw_hollow_cursor (struct window *w, struct glyph_row *row)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Display *dpy = FRAME_X_DISPLAY (f);
  int x, y, wd, h;
  XGCValues xgcv;
  struct glyph *cursor_glyph;
  GC gc;

  /* Get the glyph the cursor is on.  If we can't tell because
     the current matrix is invalid or such, give up.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Compute frame-relative coordinates for phys cursor.  */
  get_phys_cursor_geometry (w, row, cursor_glyph, &x, &y, &h);
  wd = w->phys_cursor_width - 1;

  /* The foreground of cursor_gc is typically the same as the normal
     background color, which can cause the cursor box to be invisible.  */
  xgcv.foreground = f->output_data.x->cursor_pixel;
  xgcv.line_width = 1;
  if (dpyinfo->scratch_cursor_gc)
    XChangeGC (dpy, dpyinfo->scratch_cursor_gc, GCForeground | GCLineWidth, &xgcv);
  else
    dpyinfo->scratch_cursor_gc = XCreateGC (dpy, FRAME_X_DRAWABLE (f),
					    GCForeground | GCLineWidth, &xgcv);
  gc = dpyinfo->scratch_cursor_gc;

  /* When on R2L character, show cursor at the right edge of the
     glyph, unless the cursor box is as wide as the glyph or wider
     (the latter happens when x-stretch-cursor is non-nil).  */
  if ((cursor_glyph->resolved_level & 1) != 0
      && cursor_glyph->pixel_width > wd)
    {
      x += cursor_glyph->pixel_width - wd;
      if (wd > 0)
	wd -= 1;
    }
  /* Set clipping, draw the rectangle, and reset clipping again.  */
  x_clip_to_row (w, row, TEXT_AREA, gc, NULL);
  x_draw_rectangle (f, gc, x, y, wd, h - 1);
  x_reset_clip_rectangles (f, gc);
}


/* Draw a bar cursor on window W in glyph row ROW.

   Implementation note: One would like to draw a bar cursor with an
   angle equal to the one given by the font property XA_ITALIC_ANGLE.
   Unfortunately, I didn't find a font yet that has this property set.
   --gerd.  */

static void
x_draw_bar_cursor (struct window *w, struct glyph_row *row, int width, enum text_cursor_kinds kind)
{
  struct frame *f = XFRAME (w->frame);
  struct glyph *cursor_glyph;

  /* If cursor is out of bounds, don't draw garbage.  This can happen
     in mini-buffer windows when switching between echo area glyphs
     and mini-buffer.  */
  cursor_glyph = get_phys_cursor_glyph (w);
  if (cursor_glyph == NULL)
    return;

  /* Experimental avoidance of cursor on xwidget.  */
  if (cursor_glyph->type == XWIDGET_GLYPH)
    return;

  /* If on an image, draw like a normal cursor.  That's usually better
     visible than drawing a bar, esp. if the image is large so that
     the bar might not be in the window.  */
  if (cursor_glyph->type == IMAGE_GLYPH)
    {
      struct glyph_row *r;
      r = MATRIX_ROW (w->current_matrix, w->phys_cursor.vpos);
      draw_phys_cursor_glyph (w, r, DRAW_CURSOR);
    }
  else
    {
      Display *dpy = FRAME_X_DISPLAY (f);
      Drawable drawable = FRAME_X_DRAWABLE (f);
      GC gc = FRAME_DISPLAY_INFO (f)->scratch_cursor_gc;
      unsigned long mask = GCForeground | GCBackground | GCGraphicsExposures;
      struct face *face = FACE_FROM_ID (f, cursor_glyph->face_id);
      XGCValues xgcv;

      /* If the glyph's background equals the color we normally draw
	 the bars cursor in, the bar cursor in its normal color is
	 invisible.  Use the glyph's foreground color instead in this
	 case, on the assumption that the glyph's colors are chosen so
	 that the glyph is legible.  */
      if (face->background == f->output_data.x->cursor_pixel)
	xgcv.background = xgcv.foreground = face->foreground;
      else
	xgcv.background = xgcv.foreground = f->output_data.x->cursor_pixel;
      xgcv.graphics_exposures = False;
      xgcv.line_width = 1;

      mask |= GCLineWidth;

      if (gc)
	XChangeGC (dpy, gc, mask, &xgcv);
      else
	{
          gc = XCreateGC (dpy, drawable, mask, &xgcv);
	  FRAME_DISPLAY_INFO (f)->scratch_cursor_gc = gc;
	}

      x_clip_to_row (w, row, TEXT_AREA, gc, NULL);

      if (kind == BAR_CURSOR)
	{
	  int x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

	  if (width < 0)
	    width = FRAME_CURSOR_WIDTH (f);
	  width = min (cursor_glyph->pixel_width, width);

	  w->phys_cursor_width = width;

	  /* If the character under cursor is R2L, draw the bar cursor
	     on the right of its glyph, rather than on the left.  */
	  if ((cursor_glyph->resolved_level & 1) != 0)
	    x += cursor_glyph->pixel_width - width;

	  x_fill_rectangle (f, gc, x,
			    WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y),
			    width, row->height, false);
	}
      else /* HBAR_CURSOR */
	{
	  int dummy_x, dummy_y, dummy_h;
	  int x = WINDOW_TEXT_TO_FRAME_PIXEL_X (w, w->phys_cursor.x);

	  if (width < 0)
	    width = row->height;

	  width = min (row->height, width);

	  get_phys_cursor_geometry (w, row, cursor_glyph, &dummy_x,
				    &dummy_y, &dummy_h);

	  if ((cursor_glyph->resolved_level & 1) != 0
	      && cursor_glyph->pixel_width > w->phys_cursor_width - 1)
	    x += cursor_glyph->pixel_width - w->phys_cursor_width + 1;
	  x_fill_rectangle (f, gc, x,
			    WINDOW_TO_FRAME_PIXEL_Y (w, w->phys_cursor.y +
                                                     row->height - width),
                            w->phys_cursor_width - 1, width, false);
	}

      x_reset_clip_rectangles (f, gc);
    }
}


/* RIF: Define cursor CURSOR on frame F.  */

static void
x_define_frame_cursor (struct frame *f, Emacs_Cursor cursor)
{
  if (!f->pointer_invisible
      && f->output_data.x->current_cursor != cursor)
    XDefineCursor (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f), cursor);
  f->output_data.x->current_cursor = cursor;
}


/* RIF: Clear area on frame F.  */

static void
x_clear_frame_area (struct frame *f, int x, int y, int width, int height)
{
  x_clear_area (f, x, y, width, height);
}


/* RIF: Draw cursor on window W.  */

static void
x_draw_window_cursor (struct window *w, struct glyph_row *glyph_row, int x,
		      int y, enum text_cursor_kinds cursor_type,
		      int cursor_width, bool on_p, bool active_p)
{
#ifdef HAVE_X_I18N
  struct frame *f = XFRAME (WINDOW_FRAME (w));
#endif

  if (on_p)
    {
      w->phys_cursor_type = cursor_type;
      w->phys_cursor_on_p = true;

      if (glyph_row->exact_window_width_line_p
	  && (glyph_row->reversed_p
	      ? (w->phys_cursor.hpos < 0)
	      : (w->phys_cursor.hpos >= glyph_row->used[TEXT_AREA])))
	{
	  glyph_row->cursor_in_fringe_p = true;
	  draw_fringe_bitmap (w, glyph_row, glyph_row->reversed_p);
	}
      else
	{
	  switch (cursor_type)
	    {
	    case HOLLOW_BOX_CURSOR:
	      x_draw_hollow_cursor (w, glyph_row);
	      break;

	    case FILLED_BOX_CURSOR:
	      draw_phys_cursor_glyph (w, glyph_row, DRAW_CURSOR);
	      break;

	    case BAR_CURSOR:
	      x_draw_bar_cursor (w, glyph_row, cursor_width, BAR_CURSOR);
	      break;

	    case HBAR_CURSOR:
	      x_draw_bar_cursor (w, glyph_row, cursor_width, HBAR_CURSOR);
	      break;

	    case NO_CURSOR:
	      w->phys_cursor_width = 0;
	      break;

	    default:
	      emacs_abort ();
	    }
	}

#ifdef HAVE_X_I18N
      if (w == XWINDOW (f->selected_window))
	xic_set_preeditarea (w, x, y);
#endif
    }
}


/* Icons.  */

/* Make the x-window of frame F use the gnu icon bitmap.  */

static bool
x_bitmap_icon (struct frame *f, Lisp_Object file)
{
  ptrdiff_t bitmap_id;

  if (FRAME_X_WINDOW (f) == 0)
    return true;

  /* Free up our existing icon bitmap and mask if any.  */
  if (f->output_data.x->icon_bitmap > 0)
    image_destroy_bitmap (f, f->output_data.x->icon_bitmap);
  f->output_data.x->icon_bitmap = 0;

  if (STRINGP (file))
    {
#ifdef USE_GTK
      /* Use gtk_window_set_icon_from_file () if available,
	 It's not restricted to bitmaps */
      if (xg_set_icon (f, file))
	return false;
#endif /* USE_GTK */
      bitmap_id = image_create_bitmap_from_file (f, file);
      x_create_bitmap_mask (f, bitmap_id);
    }
  else
    {
      /* Create the GNU bitmap and mask if necessary.  */
      if (FRAME_DISPLAY_INFO (f)->icon_bitmap_id < 0)
	{
	  ptrdiff_t rc = -1;

#ifdef USE_GTK

          if (xg_set_icon (f, xg_default_icon_file)
              || xg_set_icon_from_xpm_data (f, gnu_xpm_bits))
            {
              FRAME_DISPLAY_INFO (f)->icon_bitmap_id = -2;
              return false;
            }

#elif defined (HAVE_XPM) && defined (HAVE_X_WINDOWS)
	  /* This allocates too many colors.  */
	  if ((FRAME_X_VISUAL_INFO (f)->class == TrueColor
	       || FRAME_X_VISUAL_INFO (f)->class == StaticColor
	       || FRAME_X_VISUAL_INFO (f)->class == StaticGray)
	      /* That pixmap needs about 240 colors, and we should
		 also leave some more space for other colors as
		 well.  */
	      || FRAME_X_VISUAL_INFO (f)->colormap_size >= (240 * 4))
	    {
	      rc = x_create_bitmap_from_xpm_data (f, gnu_xpm_bits);
	      if (rc != -1)
		FRAME_DISPLAY_INFO (f)->icon_bitmap_id = rc;
	    }
#endif

	  /* If all else fails, use the (black and white) xbm image. */
	  if (rc == -1)
	    {
              rc = image_create_bitmap_from_data (f,
                                                  (char *) gnu_xbm_bits,
                                                  gnu_xbm_width,
                                                  gnu_xbm_height);
	      if (rc == -1)
		return true;

	      FRAME_DISPLAY_INFO (f)->icon_bitmap_id = rc;
	      x_create_bitmap_mask (f, FRAME_DISPLAY_INFO (f)->icon_bitmap_id);
	    }
	}

      /* The first time we create the GNU bitmap and mask,
	 this increments the ref-count one extra time.
	 As a result, the GNU bitmap and mask are never freed.
	 That way, we don't have to worry about allocating it again.  */
      image_reference_bitmap (f, FRAME_DISPLAY_INFO (f)->icon_bitmap_id);

      bitmap_id = FRAME_DISPLAY_INFO (f)->icon_bitmap_id;
    }

  x_wm_set_icon_pixmap (f, bitmap_id);
  f->output_data.x->icon_bitmap = bitmap_id;

  return false;
}


/* Make the x-window of frame F use a rectangle with text.
   Use ICON_NAME as the text.  */

bool
x_text_icon (struct frame *f, const char *icon_name)
{
  if (FRAME_X_WINDOW (f) == 0)
    return true;

  {
    XTextProperty text;
    text.value = (unsigned char *) icon_name;
    text.encoding = XA_STRING;
    text.format = 8;
    text.nitems = strlen (icon_name);
    XSetWMIconName (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f), &text);
  }

  if (f->output_data.x->icon_bitmap > 0)
    image_destroy_bitmap (f, f->output_data.x->icon_bitmap);
  f->output_data.x->icon_bitmap = 0;
  x_wm_set_icon_pixmap (f, 0);

  return false;
}


struct x_error_message_stack
{
  /* Pointer to the error message of any error that was generated, or
     NULL.  */
  char *string;

  /* The display this error handler applies to.  */
  Display *dpy;

  /* A function to call upon an error if non-NULL.  */
  x_special_error_handler handler;

  /* Some data to pass to that handler function.  */
  void *handler_data;

  /* The previous handler in this stack.  */
  struct x_error_message_stack *prev;

  /* The first request that this error handler applies to.  Keeping
     track of this allows us to avoid an XSync yet still have errors
     for previously made requests be handled correctly.  */
  unsigned long first_request;
};

/* Stack of X error message handlers.  Whenever an error is generated
   on a display, look in this stack for an appropriate error handler,
   set its `string' to the error message and call its `handler' with
   `handler_data'.  If no handler applies to the error, don't catch
   it, and let it crash Emacs instead.

   This used to be a pointer to a string in which any error would be
   placed before 2006.  */
static struct x_error_message_stack *x_error_message;

/* The amount of items (depth) in that stack.  */
int x_error_message_count;

/* Compare various request serials while handling wraparound.  Treat a
   difference of more than X_ULONG_MAX / 2 as wraparound.

   Note that these functions truncate serials to 32 bits before
   comparison.  */

static bool
x_is_serial_more_than (unsigned int a, unsigned int b)
{
  if (a > b)
    return true;

  return (b - a > X_ULONG_MAX / 2);
}

static bool
x_is_serial_more_than_or_equal_to (unsigned int a, unsigned int b)
{
  if (a >= b)
    return true;

  return (b - a > X_ULONG_MAX / 2);
}

static bool
x_is_serial_less_than (unsigned int a, unsigned int b)
{
  if (a < b)
    return true;

  return (a - b > X_ULONG_MAX / 2);
}

static bool
x_is_serial_less_than_or_equal_to (unsigned int a, unsigned int b)
{
  if (a <= b)
    return true;

  return (a - b > X_ULONG_MAX / 2);
}

static struct x_error_message_stack *
x_find_error_handler (Display *dpy, XErrorEvent *event)
{
  struct x_error_message_stack *stack;

  stack = x_error_message;

  while (stack)
    {
      if (x_is_serial_more_than_or_equal_to (event->serial,
					     stack->first_request)
	  && dpy == stack->dpy)
	return stack;

      stack = stack->prev;
    }

  return NULL;
}

void
x_unwind_errors_to (int depth)
{
  while (x_error_message_count > depth)
    /* This is safe to call because we check whether or not
       x_error_message->dpy is still alive before calling XSync.  */
    x_uncatch_errors ();
}

#define X_ERROR_MESSAGE_SIZE 200

/* An X error handler which stores the error message in the first
   applicable handler in the x_error_message stack.  This is called
   from *x_error_handler if an x_catch_errors for DISPLAY is in
   effect.  */

static void
x_error_catcher (Display *display, XErrorEvent *event,
		 struct x_error_message_stack *stack)
{
  char buf[X_ERROR_MESSAGE_SIZE];

  XGetErrorText (display, event->error_code,
		 buf, X_ERROR_MESSAGE_SIZE);

  if (stack->string)
    xfree (stack->string);

  stack->string = xstrdup (buf);

  if (stack->handler)
    stack->handler (display, event, stack->string,
		    stack->handler_data);
}

/* Begin trapping X errors for display DPY.

   After calling this function, X protocol errors generated on DPY no
   longer cause Emacs to exit; instead, they are recorded in an error
   handler pushed onto the stack `x_error_message'.

   Calling x_check_errors signals an Emacs error if an X error has
   occurred since the last call to x_catch_errors or x_check_errors.

   Calling x_uncatch_errors resumes the normal error handling,
   skipping an XSync if the last request made is known to have been
   processed.  Calling x_uncatch_errors_after_check is similar, but
   always skips an XSync to the server, and should be used only
   immediately after x_had_errors_p or x_check_errors, or when it is
   known that no requests have been made since the last x_catch_errors
   call for DPY.

   There is no need to use this mechanism for ignoring errors from
   single asynchronous requests, such as sending a ClientMessage to a
   window that might no longer exist.  Use
   x_ignore_errors_for_next_request (paired with
   x_stop_ignoring_errors) instead.  */

void
x_catch_errors_with_handler (Display *dpy, x_special_error_handler handler,
			     void *handler_data)
{
  struct x_error_message_stack *data;

  data = xzalloc (sizeof *data);
  data->dpy = dpy;
  data->handler = handler;
  data->handler_data = handler_data;
  data->prev = x_error_message;
  data->first_request = XNextRequest (dpy);
  x_error_message = data;

  ++x_error_message_count;
}

void
x_catch_errors (Display *dpy)
{
  x_catch_errors_with_handler (dpy, NULL, NULL);
}

/* Return if errors for REQUEST should be ignored even if there is no
   error handler applied.  */
static struct x_failable_request *
x_request_can_fail (struct x_display_info *dpyinfo,
		    unsigned long request)
{
  struct x_failable_request *failable_requests;

  for (failable_requests = dpyinfo->failable_requests;
       failable_requests < dpyinfo->next_failable_request;
       failable_requests++)
    {
      if (x_is_serial_more_than_or_equal_to (request,
					     failable_requests->start)
	  && (!failable_requests->end
	      || x_is_serial_less_than_or_equal_to (request,
						    failable_requests->end)))
	return failable_requests;
    }

  return NULL;
}

/* Remove outdated request serials from
   dpyinfo->failable_requests.  */
static void
x_clean_failable_requests (struct x_display_info *dpyinfo)
{
  struct x_failable_request *first, *last;

  last = dpyinfo->next_failable_request;

  for (first = dpyinfo->failable_requests; first < last; first++)
    {
      if (x_is_serial_more_than (first->start,
				 LastKnownRequestProcessed (dpyinfo->display))
	  || !first->end
	  || x_is_serial_more_than (first->end,
				    LastKnownRequestProcessed (dpyinfo->display)))
	break;
    }

  if (first != last)
    memmove (&dpyinfo->failable_requests, first,
	     sizeof *first * (last - first));

  dpyinfo->next_failable_request = (dpyinfo->failable_requests
				    + (last - first));
}

/* Protect a section of X requests.

   Ignore errors generated by X requests made from now until
   `x_stop_ignoring_errors'.  Each call must be paired with a call to
   `x_stop_ignoring_errors', and recursive calls inside the protected
   section are not allowed.

   The advantage over x_catch_errors followed by
   x_uncatch_errors_after_check is that this function does not sync to
   catch errors if requests were made.  It should be used instead of
   those two functions for catching errors around requests that do not
   require a reply.

   If SELECTION_SERIAL is an arbitrary number greater than zero,
   x_select_handle_selection_error is called with the specified number
   after any errors within the protected section are received to
   delete the selection request that encountered errors.  */

void
x_ignore_errors_for_next_request (struct x_display_info *dpyinfo,
				  unsigned int selection_serial)
{
  struct x_failable_request *request, *max;
  unsigned long next_request;

  /* This code is not reentrant, so be sure nothing calls it
     recursively in response to input.  */
  block_input ();

#ifdef HAVE_GTK3
  /* GTK 3 tends to override our own error handler inside certain
     callbacks, which this can be called from.  Instead of trying to
     restore our own, add a trap for the following requests with
     GDK as well.  */
  GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (dpyinfo->display);
  if (gdpy)
    gdk_x11_display_error_trap_push (gdpy);
#endif

  if ((dpyinfo->next_failable_request
       != dpyinfo->failable_requests)
      && (dpyinfo->next_failable_request - 1)->end == 0)
    /* A new sequence should never be started before an old one
       finishes.  Use `x_catch_errors' to nest error handlers.  */
    emacs_abort ();

  request = dpyinfo->next_failable_request;
  max = dpyinfo->failable_requests + N_FAILABLE_REQUESTS;
  next_request = XNextRequest (dpyinfo->display);

  if (request >= max)
    {
      /* There is no point in making this extra sync if all requests
	 are known to have been fully processed.  */
      if ((LastKnownRequestProcessed (dpyinfo->display)
	   != next_request - 1))
	XSync (dpyinfo->display, False);

      x_clean_failable_requests (dpyinfo);
      request = dpyinfo->next_failable_request;
    }

  if (request >= max)
    /* A request should always be made immediately after calling this
       function.  */
    emacs_abort ();

  request->start = next_request;
  request->end = 0;
  request->selection_serial = selection_serial;

  dpyinfo->next_failable_request++;
}

void
x_stop_ignoring_errors (struct x_display_info *dpyinfo)
{
  struct x_failable_request *range;

  range = dpyinfo->next_failable_request - 1;
  range->end = XNextRequest (dpyinfo->display) - 1;

  /* Abort if no request was made since
     `x_ignore_errors_for_next_request'.  */

  if (x_is_serial_less_than (range->end, range->start))
    emacs_abort ();

#ifdef HAVE_GTK3
  GdkDisplay *gdpy = gdk_x11_lookup_xdisplay (dpyinfo->display);

  if (gdpy)
    gdk_x11_display_error_trap_pop_ignored (gdpy);
#endif

  unblock_input ();
}

/* Undo the last x_catch_errors call.
   DPY should be the display that was passed to x_catch_errors.

   This version should be used only if the immediately preceding
   X-protocol-related thing was x_check_errors or x_had_error_p, both
   of which issue XSync calls, so we don't need to re-sync here.  */

void
x_uncatch_errors_after_check (void)
{
  struct x_error_message_stack *tmp;

  block_input ();
  tmp = x_error_message;
  x_error_message = x_error_message->prev;
  --x_error_message_count;
  if (tmp->string)
    xfree (tmp->string);
  xfree (tmp);
  unblock_input ();
}

/* Undo the last x_catch_errors call.  */

void
x_uncatch_errors (void)
{
  struct x_error_message_stack *tmp;
  struct x_display_info *dpyinfo;

  /* In rare situations when running Emacs run in daemon mode,
     shutting down an emacsclient via delete-frame can cause
     x_uncatch_errors to be called when x_error_message is set to
     NULL.  */
  if (x_error_message == NULL)
    return;

  block_input ();

  dpyinfo = x_display_info_for_display (x_error_message->dpy);

  /* The display may have been closed before this function is called.
     Check if it is still open before calling XSync.  */
  if (dpyinfo != 0
      /* There is no point in making this extra sync if all requests
	 are known to have been fully processed.  */
      && (LastKnownRequestProcessed (x_error_message->dpy)
	  != XNextRequest (x_error_message->dpy) - 1)
      /* Likewise if no request was made since the trap was
	 installed.  */
      && (NextRequest (x_error_message->dpy)
	  > x_error_message->first_request))
    {
      XSync (x_error_message->dpy, False);
      x_clean_failable_requests (dpyinfo);
    }

  tmp = x_error_message;
  x_error_message = x_error_message->prev;
  --x_error_message_count;
  if (tmp->string)
    xfree (tmp->string);
  xfree (tmp);
  unblock_input ();
}

/* If any X protocol errors have arrived since the last call to
   x_catch_errors or x_check_errors, signal an Emacs error using
   sprintf (a buffer, FORMAT, the x error message text) as the text.  */

void
x_check_errors (Display *dpy, const char *format)
{
  struct x_display_info *dpyinfo;
  char *string;

  /* This shouldn't happen, since x_check_errors should be called
     immediately inside an x_catch_errors block.  */
  if (dpy != x_error_message->dpy)
    emacs_abort ();

  /* There is no point in making this extra sync if all requests
     are known to have been fully processed.  */
  if ((LastKnownRequestProcessed (dpy)
       != XNextRequest (dpy) - 1)
      && (NextRequest (dpy)
	  > x_error_message->first_request))
    XSync (dpy, False);

  dpyinfo = x_display_info_for_display (dpy);

  /* Clean the array of failable requests, since a sync happened.  */
  if (dpyinfo)
    x_clean_failable_requests (dpyinfo);

  if (x_error_message->string)
    {
      string = alloca (strlen (x_error_message->string) + 1);
      strcpy (string, x_error_message->string);

      error (format, string);
    }
}

/* Nonzero if any X protocol errors were generated since the last call
   to x_catch_errors on DPY.  */

bool
x_had_errors_p (Display *dpy)
{
  struct x_display_info *dpyinfo;

  /* This shouldn't happen, since x_check_errors should be called
     immediately inside an x_catch_errors block.  */
  if (dpy != x_error_message->dpy)
    emacs_abort ();

  /* Make sure to catch any errors incurred so far.  */
  if ((LastKnownRequestProcessed (dpy)
       != XNextRequest (dpy) - 1)
      && (NextRequest (dpy)
	  > x_error_message->first_request))
    XSync (dpy, False);

  dpyinfo = x_display_info_for_display (dpy);

  /* Clean the array of failable requests, since a sync happened.  */
  if (dpyinfo)
    x_clean_failable_requests (dpyinfo);

  return !!x_error_message->string;
}

/* Forget about any errors we have had, since we did x_catch_errors on
   DPY.  */

void
x_clear_errors (Display *dpy)
{
  /* This shouldn't happen, since x_check_errors should be called
     immediately inside an x_catch_errors block.  */
  if (dpy != x_error_message->dpy)
    emacs_abort ();

  xfree (x_error_message->string);
  x_error_message->string = NULL;
}

#if false
      /* See comment in unwind_to_catch why calling this is a bad
       * idea.  --lorentey   */
/* Close off all unclosed x_catch_errors calls.  */

void
x_fully_uncatch_errors (void)
{
  while (x_error_message)
    x_uncatch_errors ();
}
#endif

#if false
static unsigned int x_wire_count;

static int
x_trace_wire (Display *dpy)
{
  fprintf (stderr, "Lib call: %u\n", ++x_wire_count);
  return 0;
}
#endif


/************************************************************************
			  Handling X errors
 ************************************************************************/

/* Error message passed to x_connection_closed.  */

static char *error_msg;

/* Try to find a frame in Vframe_list, and make it the selected frame.
   `delete_frame' sometimes misses the initial frame for an unknown
   reason when Emacs is running as a background daemon.  */

static void
x_try_restore_frame (void)
{
  Lisp_Object tail, frame;

  FOR_EACH_FRAME (tail, frame)
    {
      if (!NILP (do_switch_frame (frame, 0, 1, Qnil)))
	return;
    }
}

/* Handle the loss of connection to display DPY.  ERROR_MESSAGE is
   the text of an error message that lead to the connection loss.  */

static void
x_connection_closed (Display *dpy, const char *error_message, bool ioerror)
{
  struct x_display_info *dpyinfo;
  Lisp_Object frame, tail;
  specpdl_ref idx = SPECPDL_INDEX ();
  Emacs_XIOErrorHandler io_error_handler;
  xm_drop_start_message dmsg;
  struct frame *f;
  Lisp_Object minibuf_frame, tmp;
  struct x_failable_request *failable;
  struct x_error_message_stack *stack;
  static Display *current_display;

  /* Prevent recursive calls of this function for the same display.
     This is because destroying a frame might still cause an IO error
     in some cases.  (bug#56528) */
  if (current_display == dpy)
    return;

  current_display = dpy;

  dpyinfo = x_display_info_for_display (dpy);
  error_msg = alloca (strlen (error_message) + 1);
  strcpy (error_msg, error_message);

  /* Inhibit redisplay while frames are being deleted. */
  specbind (Qinhibit_redisplay, Qt);

  /* If drag-and-drop is in progress, cancel drag-and-drop.  If DND
     frame's display is DPY, don't reset event masks or try to send
     responses to other programs because the display is going
     away.  */

  if (x_dnd_in_progress || x_dnd_waiting_for_finish)
    {
      if (!ioerror)
	{
	  /* Handle display disconnect errors here because this function
	     is not reentrant at this particular spot.  */
	  io_error_handler = XSetIOErrorHandler (x_dnd_io_error_handler);

	  if (!!sigsetjmp (x_dnd_disconnect_handler, 1)
	      && x_dnd_in_progress
	      && dpy == (x_dnd_waiting_for_finish
			 ? x_dnd_finish_display
			 : FRAME_X_DISPLAY (x_dnd_frame)))
	    {
	      /* Clean up drag and drop if the drag frame's display isn't
		 the one being disconnected.  */
	      f = x_dnd_frame;

	      if (x_dnd_last_seen_window != None
		  && x_dnd_last_protocol_version != -1)
		x_dnd_send_leave (x_dnd_frame,
				  x_dnd_last_seen_window,
				  x_dnd_last_seen_toplevel);
	      else if (x_dnd_last_seen_window != None
		       && !XM_DRAG_STYLE_IS_DROP_ONLY (x_dnd_last_motif_style)
		       && x_dnd_last_motif_style != XM_DRAG_STYLE_NONE
		       && x_dnd_motif_setup_p)
		{
		  dmsg.reason = XM_DRAG_REASON (XM_DRAG_ORIGINATOR_INITIATOR,
						XM_DRAG_REASON_DROP_START);
		  dmsg.byte_order = XM_BYTE_ORDER_CUR_FIRST;
		  dmsg.timestamp = FRAME_DISPLAY_INFO (f)->last_user_time;
		  dmsg.side_effects
		    = XM_DRAG_SIDE_EFFECT (xm_side_effect_from_action (FRAME_DISPLAY_INFO (f),
								       x_dnd_wanted_action),
					   XM_DROP_SITE_VALID, x_dnd_motif_operations,
					   XM_DROP_ACTION_DROP_CANCEL);
		  dmsg.x = 0;
		  dmsg.y = 0;
		  dmsg.index_atom = x_dnd_motif_atom;
		  dmsg.source_window = FRAME_X_WINDOW (f);

		  x_dnd_send_xm_leave_for_drop (FRAME_DISPLAY_INFO (f), f,
						x_dnd_last_seen_window, 0);
		  xm_send_drop_message (FRAME_DISPLAY_INFO (f), FRAME_X_WINDOW (f),
					x_dnd_last_seen_window, &dmsg);
		}
	    }

	  XSetIOErrorHandler (io_error_handler);
	}

      dpyinfo = x_display_info_for_display (dpy);

      x_dnd_last_seen_window = None;
      x_dnd_last_seen_toplevel = None;
      x_dnd_in_progress = false;
      x_dnd_waiting_for_finish = false;

      if (x_dnd_use_toplevels)
	x_dnd_free_toplevels (!ioerror);

      x_dnd_return_frame_object = NULL;
      x_dnd_movement_frame = NULL;
      x_dnd_wheel_frame = NULL;
      x_dnd_frame = NULL;
    }

  if (dpyinfo)
    {
      /* Protect display from being closed when we delete the last
         frame on it. */
      dpyinfo->reference_count++;
      dpyinfo->terminal->reference_count++;
      if (ioerror)
	dpyinfo->display = 0;
    }

  /* delete_frame can still try to read async input (even though we
     tell pass `noelisp'), because looking up the `delete-before'
     parameter calls Fassq which then calls maybe_quit.  So block
     input while deleting frames.  */
  block_input ();

  /* First delete frames whose mini-buffers are on frames
     that are on the dead display.  */
  FOR_EACH_FRAME (tail, frame)
    {
      /* Tooltip frames don't have these, so avoid crashing.  */

      if (FRAME_TOOLTIP_P (XFRAME (frame)))
	continue;

      minibuf_frame
	= WINDOW_FRAME (XWINDOW (FRAME_MINIBUF_WINDOW (XFRAME (frame))));

      if (FRAME_X_P (XFRAME (frame))
	  && FRAME_X_P (XFRAME (minibuf_frame))
	  && ! EQ (frame, minibuf_frame)
	  && FRAME_DISPLAY_INFO (XFRAME (minibuf_frame)) == dpyinfo)
	delete_frame (frame, Qnoelisp);
    }

  /* Now delete all remaining frames on the dead display.
     We are now sure none of these is used as the mini-buffer
     for another frame that we need to delete.  */
  FOR_EACH_FRAME (tail, frame)
    if (FRAME_X_P (XFRAME (frame))
	&& FRAME_DISPLAY_INFO (XFRAME (frame)) == dpyinfo)
      {
	/* Set this to t so that delete_frame won't get confused
	   trying to find a replacement.  */
	kset_default_minibuffer_frame (FRAME_KBOARD (XFRAME (frame)), Qt);
	delete_frame (frame, Qnoelisp);
      }

  /* If DPYINFO is null, this means we didn't open the display in the
     first place, so don't try to close it.  */
  if (dpyinfo)
    {
      /* We can not call XtCloseDisplay here because it calls XSync.
         XSync inside the error handler apparently hangs Emacs.  On
         current Xt versions, this isn't needed either.  */
#ifdef USE_GTK
      /* A long-standing GTK bug prevents proper disconnect handling
	 <https://gitlab.gnome.org/GNOME/gtk/issues/221>.  Once,
	 the resulting Glib error message loop filled a user's disk.
	 To avoid this, kill Emacs unconditionally on disconnect.  */
      shut_down_emacs (0, Qnil);
      fprintf (stderr, "%s\n\
When compiled with GTK, Emacs cannot recover from X disconnects.\n\
This is a GTK bug: https://gitlab.gnome.org/GNOME/gtk/issues/221\n\
For details, see etc/PROBLEMS.\n",
	       error_msg);
      emacs_abort ();
#endif /* USE_GTK */

      /* Indicate that this display is dead.  */
      dpyinfo->display = 0;

      dpyinfo->reference_count--;
      dpyinfo->terminal->reference_count--;
      if (dpyinfo->reference_count != 0)
        /* We have just closed all frames on this display. */
        emacs_abort ();

      /* This was the last terminal remaining, so print the error
	 message and associated error handlers and kill Emacs.  */
      if (dpyinfo->terminal == terminal_list
	  && !terminal_list->next_terminal)
	{
	  fprintf (stderr, "%s\n", error_msg);

	  if (!ioerror && dpyinfo)
	    {
	      /* Dump the list of error handlers for debugging
		 purposes if the list exists.  */

	      if ((dpyinfo->failable_requests
		   != dpyinfo->next_failable_request) || x_error_message)
		fprintf (stderr, "X error handlers currently installed:\n");

	      for (failable = dpyinfo->failable_requests;
		   failable < dpyinfo->next_failable_request;
		   ++failable)
		{
		  if (failable->end)
		    fprintf (stderr, "Ignoring errors between %lu to %lu\n",
			     failable->start, failable->end);
		  else
		    fprintf (stderr, "Ignoring errors from %lu onwards\n",
			     failable->start);
		}

	      for (stack = x_error_message; stack; stack = stack->prev)
		fprintf (stderr, "Trapping errors from %lu\n",
			 stack->first_request);
	    }
	}

      XSETTERMINAL (tmp, dpyinfo->terminal);
      Fdelete_terminal (tmp, Qnoelisp);
    }

  /* The initial "daemon" frame is sometimes not selected by
     `delete_frame' when Emacs is a background daemon.  */
  if (NILP (selected_frame))
    x_try_restore_frame ();

  unblock_input ();

  /* Sometimes another terminal is still alive, but deleting this
     terminal caused all frames to vanish.  In that case, simply kill
     Emacs, since the next redisplay will abort as there is no more
     selected frame.  (bug#56528) */
  if (terminal_list == 0 || NILP (selected_frame))
    Fkill_emacs (make_fixnum (70), Qnil);

  totally_unblock_input ();

  unbind_to (idx, Qnil);
  clear_waiting_for_input ();

  /* Here, we absolutely have to use a non-local exit (e.g. signal, throw,
     longjmp), because returning from this function would get us back into
     Xlib's code which will directly call `exit'.  */
  current_display = NULL;
  error ("%s", error_msg);
}

static void x_error_quitter (Display *, XErrorEvent *);

/* This is the first-level handler for X protocol errors.
   It calls x_error_quitter or x_error_catcher.  */

static int
x_error_handler (Display *display, XErrorEvent *event)
{
  struct x_error_message_stack *stack;
  struct x_display_info *dpyinfo;
  struct x_failable_request *fail, *last;

#if defined USE_GTK && defined HAVE_GTK3
  if ((event->error_code == BadMatch
       || event->error_code == BadWindow)
      && event->request_code == X_SetInputFocus)
    return 0;
#endif

  dpyinfo = x_display_info_for_display (display);

  if (dpyinfo)
    {
      fail = x_request_can_fail (dpyinfo, event->serial);

      if (fail)
	{
	  /* Now that this request sequence has been fully handled,
	     remove it from the list of requests that can fail.  */

	  if (event->serial == fail->end)
	    {
	      last = dpyinfo->next_failable_request;
	      memmove (&dpyinfo->failable_requests, fail,
		       sizeof *fail * (last - fail));
	      dpyinfo->next_failable_request = (dpyinfo->failable_requests
						+ (last - fail));
	    }

	  /* If a selection transfer is the cause of this error,
	     remove the selection transfer now.  */

	  if (fail->selection_serial)
	    {
	      x_handle_selection_error (fail->selection_serial,
					event);

	      /* Clear selection_serial to prevent
		 x_handle_selection_error from being called again if
		 any more requests within the protected section cause
		 errors to be reported.  */
	      fail->selection_serial = 0;
	    }

	  return 0;
	}
    }

  /* If we try to ungrab or grab a device that doesn't exist anymore
     (that happens a lot in xmenu.c), just ignore the error.  */

#ifdef HAVE_XINPUT2
  /* Handle errors from some specific XI2 requests here to avoid a
     sync in handle_one_xevent.  */
  if (dpyinfo && dpyinfo->supports_xi2
      && event->request_code == dpyinfo->xi2_opcode
      && (event->minor_code == X_XIGrabDevice
	  || event->minor_code == X_XIUngrabDevice
	  || event->minor_code == X_XIAllowEvents))
    return 0;
#endif

  stack = x_find_error_handler (display, event);

  if (stack)
    x_error_catcher (display, event, stack);
  else
    x_error_quitter (display, event);
  return 0;
}

/* This is the usual handler for X protocol errors.
   It kills all frames on the display that we got the error for.
   If that was the only one, it prints an error message and kills Emacs.  */

/* .gdbinit puts a breakpoint here, so make sure it is not inlined.  */

static void NO_INLINE
x_error_quitter (Display *display, XErrorEvent *event)
{
  char buf[256], buf1[800 + INT_STRLEN_BOUND (int)
		      + INT_STRLEN_BOUND (unsigned long)
		      + INT_STRLEN_BOUND (XID)
		      + INT_STRLEN_BOUND (int)];

  /* Ignore BadName errors.  They can happen because of fonts
     or colors that are not defined.  */

  if (event->error_code == BadName)
    return;

  /* Note that there is no real way portable across R3/R4 to get the
     original error handler.  */

  XGetErrorText (display, event->error_code, buf, sizeof (buf));
  sprintf (buf1, "X protocol error: %s on protocol request %d\n"
	   "Serial no: %lu\n"
	   "Failing resource ID (if any): 0x%lx\n"
	   "Minor code: %d\n"
	   "This is a bug!  Please report this to bug-gnu-emacs@gnu.org!\n",
	   buf, event->request_code, event->serial, event->resourceid,
	   event->minor_code);
  x_connection_closed (display, buf1, false);
}


/* This is the handler for X IO errors, always.
   It kills all frames on the display that we lost touch with.
   If that was the only one, it prints an error message and kills Emacs.  */

static int NO_INLINE
x_io_error_quitter (Display *display)
{
  char const *server = DisplayString (display);
  static char const fmt[] = "Connection lost to X server '%s'";
  USE_SAFE_ALLOCA;
  char *buf = SAFE_ALLOCA (sizeof fmt - sizeof "%s" + strlen (server) + 1);
  sprintf (buf, fmt, server);
  x_connection_closed (display, buf, true);
  SAFE_FREE ();

  return 0;
}


/* Changing the font of the frame.  */

/* Give frame F the font FONT-OBJECT as its default font.  The return
   value is FONT-OBJECT.  FONTSET is an ID of the fontset for the
   frame.  If it is negative, generate a new fontset from
   FONT-OBJECT.  */

static Lisp_Object
x_new_font (struct frame *f, Lisp_Object font_object, int fontset)
{
  struct font *font = XFONT_OBJECT (font_object);
  int unit, font_ascent, font_descent;

  if (fontset < 0)
    fontset = fontset_from_font (font_object);
  FRAME_FONTSET (f) = fontset;
  if (FRAME_FONT (f) == font)
    /* This font is already set in frame F.  There's nothing more to
       do.  */
    return font_object;

  FRAME_FONT (f) = font;
  FRAME_BASELINE_OFFSET (f) = font->baseline_offset;
  FRAME_COLUMN_WIDTH (f) = font->average_width;
  get_font_ascent_descent (font, &font_ascent, &font_descent);
  FRAME_LINE_HEIGHT (f) = font_ascent + font_descent;

#ifndef USE_X_TOOLKIT
  FRAME_MENU_BAR_HEIGHT (f) = FRAME_MENU_BAR_LINES (f) * FRAME_LINE_HEIGHT (f);
#endif
  /* We could use a more elaborate calculation here.  */
  FRAME_TAB_BAR_HEIGHT (f) = FRAME_TAB_BAR_LINES (f) * FRAME_LINE_HEIGHT (f);

  /* Compute character columns occupied by scrollbar.

     Don't do things differently for non-toolkit scrollbars
     (Bug#17163).  */
  unit = FRAME_COLUMN_WIDTH (f);
  if (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) > 0)
    FRAME_CONFIG_SCROLL_BAR_COLS (f)
      = (FRAME_CONFIG_SCROLL_BAR_WIDTH (f) + unit - 1) / unit;
  else
    FRAME_CONFIG_SCROLL_BAR_COLS (f) = (14 + unit - 1) / unit;


  /* Don't change the size of a tip frame; there's no point in doing it
     because it's done in Fx_show_tip, and it leads to problems because
     the tip frame has no widget.  */
  if (FRAME_X_WINDOW (f) != 0 && !FRAME_TOOLTIP_P (f))
    adjust_frame_size
      (f, FRAME_COLS (f) * FRAME_COLUMN_WIDTH (f),
       FRAME_LINES (f) * FRAME_LINE_HEIGHT (f), 3, false, Qfont);

#ifdef HAVE_X_I18N
  if (FRAME_XIC (f)
      && (FRAME_XIC_STYLE (f) & (XIMPreeditPosition | XIMStatusArea)))
    {
      block_input ();
      xic_set_xfontset (f, SSDATA (fontset_ascii (fontset)));
      unblock_input ();
    }
#endif

  return font_object;
}


/***********************************************************************
			   X Input Methods
 ***********************************************************************/

#ifdef HAVE_X_I18N

#ifdef HAVE_X11R6

/* HAVE_X11R6 means Xlib conforms to the R6 specification or later.
   HAVE_X11R6_XIM, OTOH, means that Emacs should try to use R6-style
   callback driven input method initialization.  They are separate
   because Sun apparently ships buggy Xlib with some versions of
   Solaris... */

#ifdef HAVE_X11R6_XIM

/* If preedit text is set on F, cancel preedit, free the text, and
   generate the appropriate events to cancel the preedit display.

   This is mainly useful when the connection to the IM server is
   dropped during preconversion.  */

static void
x_maybe_clear_preedit (struct frame *f)
{
  struct x_output *output;
  struct input_event ie;

  output = FRAME_X_OUTPUT (f);

  if (!output->preedit_chars)
    return;

  EVENT_INIT (ie);
  ie.kind = PREEDIT_TEXT_EVENT;
  ie.arg = Qnil;
  XSETFRAME (ie.frame_or_window, f);
  XSETINT (ie.x, 0);
  XSETINT (ie.y, 0);
  kbd_buffer_store_event (&ie);

  xfree (output->preedit_chars);

  output->preedit_size = 0;
  output->preedit_active = false;
  output->preedit_chars = NULL;
  output->preedit_caret = 0;
}

/* XIM destroy callback function, which is called whenever the
   connection to input method XIM dies.  CLIENT_DATA contains a
   pointer to the x_display_info structure corresponding to XIM.  */

static void
xim_destroy_callback (XIM xim, XPointer client_data, XPointer call_data)
{
  struct x_display_info *dpyinfo = (struct x_display_info *) client_data;
  Lisp_Object frame, tail;

  block_input ();

  /* No need to call XDestroyIC.. */
  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *f = XFRAME (frame);
      if (FRAME_X_P (f) && FRAME_DISPLAY_INFO (f) == dpyinfo)
	{
	  FRAME_XIC (f) = NULL;
          xic_free_xfontset (f);

	  /* Free the preedit text if necessary.  */
	  x_maybe_clear_preedit (f);
	}
    }

  /* No need to call XCloseIM.  */
  dpyinfo->xim = NULL;

  /* Also free IM values; those are allocated separately upon
     XGetIMValues.  */
  if (dpyinfo->xim_styles)
    XFree (dpyinfo->xim_styles);
  dpyinfo->xim_styles = NULL;
  unblock_input ();
}

#endif

#endif /* HAVE_X11R6 */

/* Open the connection to the XIM server on display DPYINFO.
   RESOURCE_NAME is the resource name Emacs uses.  */

static void
xim_open_dpy (struct x_display_info *dpyinfo, char *resource_name)
{
#ifdef HAVE_XIM
  XIM xim;
  const char *locale;

  if (dpyinfo->use_xim)
    {
      if (dpyinfo->xim)
	{
	  XCloseIM (dpyinfo->xim);

	  /* Free values left over from the last time the IM
	     connection was established.  */

	  if (dpyinfo->xim_styles)
	    XFree (dpyinfo->xim_styles);
	  dpyinfo->xim_styles = NULL;
	}

      xim = XOpenIM (dpyinfo->display, dpyinfo->rdb, resource_name,
		     emacs_class);
      dpyinfo->xim = xim;

      if (xim)
	{
#ifdef HAVE_X11R6_XIM
	  XIMCallback destroy;
#endif

	  /* Get supported styles and XIM values.  */
	  XGetIMValues (xim, XNQueryInputStyle, &dpyinfo->xim_styles, NULL);

#ifdef HAVE_X11R6_XIM
	  destroy.callback = xim_destroy_callback;
	  destroy.client_data = (XPointer)dpyinfo;
	  XSetIMValues (xim, XNDestroyCallback, &destroy, NULL);
#endif

	  locale = XLocaleOfIM (xim);

	  /* Now try to determine the coding system that should be
	     used.  locale is in Host Portable Character Encoding, and
	     as such can be passed to build_string as is.  */
	  dpyinfo->xim_coding = safe_calln (Vx_input_coding_function,
					    build_string (locale));
	}
    }
  else
#endif /* HAVE_XIM */
    dpyinfo->xim = NULL;
}


#ifdef HAVE_X11R6_XIM

/* XIM instantiate callback function, which is called whenever an XIM
   server is available.  DISPLAY is the display of the XIM.
   CLIENT_DATA contains a pointer to an xim_inst_t structure created
   when the callback was registered.  */

static void
xim_instantiate_callback (Display *display, XPointer client_data, XPointer call_data)
{
  struct xim_inst_t *xim_inst = (struct xim_inst_t *) client_data;
  struct x_display_info *dpyinfo = xim_inst->dpyinfo;

  if (x_dnd_in_progress)
    return;

  /* We don't support multiple XIM connections. */
  if (dpyinfo->xim)
    return;

  xim_open_dpy (dpyinfo, xim_inst->resource_name);

  /* Create XIC for the existing frames on the same display, as long
     as they have no XIC.  */
  if (dpyinfo->xim && dpyinfo->reference_count > 0)
    {
      Lisp_Object tail, frame;

      block_input ();
      FOR_EACH_FRAME (tail, frame)
	{
	  struct frame *f = XFRAME (frame);

	  if (FRAME_X_P (f)
              && FRAME_DISPLAY_INFO (f) == xim_inst->dpyinfo)
	    if (FRAME_XIC (f) == NULL)
	      {
		create_frame_xic (f);
		if (FRAME_XIC_STYLE (f) & XIMStatusArea)
		  xic_set_statusarea (f);
		struct window *w = XWINDOW (f->selected_window);
		xic_set_preeditarea (w, w->cursor.x, w->cursor.y);
	      }
	}

      unblock_input ();
    }
}

#endif /* HAVE_X11R6_XIM */


/* Open a connection to the XIM server on display DPYINFO.
   RESOURCE_NAME is the resource name for Emacs.  On X11R5, open the
   connection only at the first time.  On X11R6, open the connection
   in the XIM instantiate callback function.  */

static void
xim_initialize (struct x_display_info *dpyinfo, char *resource_name)
{
  dpyinfo->xim = NULL;
#ifdef HAVE_XIM
  if (dpyinfo->use_xim)
    {
#ifdef HAVE_X11R6_XIM
      struct xim_inst_t *xim_inst = xmalloc (sizeof *xim_inst);
      Bool ret;

      dpyinfo->xim_callback_data = xim_inst;
      xim_inst->dpyinfo = dpyinfo;
      xim_inst->resource_name = xstrdup (resource_name);

      /* The last argument is XPointer in XFree86 but (XPointer *) on
	 Tru64, at least, but the configure test doesn't work because
	 xim_instantiate_callback can either be XIMProc or XIDProc, so
	 just cast to void *.  */

      ret = XRegisterIMInstantiateCallback (dpyinfo->display,
					    dpyinfo->rdb,
					    xim_inst->resource_name,
					    emacs_class,
					    xim_instantiate_callback,
					    (void *) xim_inst);
      eassert (ret);
#else /* not HAVE_X11R6_XIM */
      xim_open_dpy (dpyinfo, resource_name);
#endif /* not HAVE_X11R6_XIM */
    }
#endif /* HAVE_XIM */
}


/* Close the connection to the XIM server on display DPYINFO.
   Unregister any IM instantiation callback previously installed,
   close the connection to the IM server if possible, and free any
   retrieved IM values.  */

static void
xim_close_dpy (struct x_display_info *dpyinfo)
{
#ifdef HAVE_XIM
#ifdef HAVE_X11R6_XIM
  struct xim_inst_t *xim_inst;
  Bool rc;

  /* If dpyinfo->xim_callback_data is not set, then IM support wasn't
     initialized, which can happen if Xlib doesn't understand the C
     locale being used.  */

  if (dpyinfo->xim_callback_data)
    {
      xim_inst = dpyinfo->xim_callback_data;

      if (dpyinfo->display)
	{
	  rc = XUnregisterIMInstantiateCallback (dpyinfo->display,
						 dpyinfo->rdb,
						 xim_inst->resource_name,
						 emacs_class,
						 xim_instantiate_callback,
						 (void *) xim_inst);
	  eassert (rc);
	}

      xfree (xim_inst->resource_name);
      xfree (xim_inst);
    }
#endif /* HAVE_X11R6_XIM */

  /* Now close the connection to the input method server.  This may
     access the display connection, and isn't safe if the display has
     already been closed.  */

  if (dpyinfo->display && dpyinfo->xim)
    XCloseIM (dpyinfo->xim);
  dpyinfo->xim = NULL;

  /* Free the list of XIM styles retrieved.  */

  if (dpyinfo->xim_styles)
    XFree (dpyinfo->xim_styles);
  dpyinfo->xim_styles = NULL;
#endif /* HAVE_XIM */
}

#endif /* not HAVE_X11R6_XIM */



/* Calculate the absolute position in frame F
   from its current recorded position values and gravity.  */

static void
x_calc_absolute_position (struct frame *f)
{
  int flags = f->size_hint_flags;
  struct frame *p = FRAME_PARENT_FRAME (f);

  /* We have nothing to do if the current position
     is already for the top-left corner.  */
  if (! ((flags & XNegative) || (flags & YNegative)))
    return;

  /* Treat negative positions as relative to the leftmost bottommost
     position that fits on the screen.  */
  if (flags & XNegative)
    {
      int width = FRAME_PIXEL_WIDTH (f);

      /* A frame that has been visible at least once should have outer
	 edges.  */
      if (f->output_data.x->has_been_visible && !p)
	{
	  Lisp_Object frame;
	  Lisp_Object edges = Qnil;

	  XSETFRAME (frame, f);
	  edges = Fx_frame_edges (frame, Qouter_edges);
	  if (!NILP (edges))
	    width = (XFIXNUM (Fnth (make_fixnum (2), edges))
		     - XFIXNUM (Fnth (make_fixnum (0), edges)));
	}

      if (p)
	f->left_pos = (FRAME_PIXEL_WIDTH (p) - width - 2 * f->border_width
		       + f->left_pos);
      else
	f->left_pos = (x_display_pixel_width (FRAME_DISPLAY_INFO (f))
		       - width + f->left_pos);

    }

  if (flags & YNegative)
    {
      int height = FRAME_PIXEL_HEIGHT (f);

#if defined USE_X_TOOLKIT && defined USE_MOTIF
      /* Something is fishy here.  When using Motif, starting Emacs with
	 `-g -0-0', the frame appears too low by a few pixels.

	 This seems to be so because initially, while Emacs is starting,
	 the column widget's height and the frame's pixel height are
	 different.  The column widget's height is the right one.  In
	 later invocations, when Emacs is up, the frame's pixel height
	 is right, though.

	 It's not obvious where the initial small difference comes from.
	 2000-12-01, gerd.  */

      XtVaGetValues (f->output_data.x->column_widget, XtNheight, &height, NULL);
#endif

      if (f->output_data.x->has_been_visible && !p)
	{
	  Lisp_Object frame;
	  Lisp_Object edges = Qnil;

	  XSETFRAME (frame, f);
	  if (NILP (edges))
	    edges = Fx_frame_edges (frame, Qouter_edges);
	  if (!NILP (edges))
	    height = (XFIXNUM (Fnth (make_fixnum (3), edges))
		      - XFIXNUM (Fnth (make_fixnum (1), edges)));
	}

      if (p)
	f->top_pos = (FRAME_PIXEL_HEIGHT (p) - height - 2 * f->border_width
		       + f->top_pos);
      else
	f->top_pos = (x_display_pixel_height (FRAME_DISPLAY_INFO (f))
		      - height + f->top_pos);
  }

  /* The left_pos and top_pos
     are now relative to the top and left screen edges,
     so the flags should correspond.  */
  f->size_hint_flags &= ~ (XNegative | YNegative);
}

/* CHANGE_GRAVITY is 1 when calling from Fset_frame_position,
   to really change the position, and 0 when calling from
   x_make_frame_visible (in that case, XOFF and YOFF are the current
   position values).  It is -1 when calling from gui_set_frame_parameters,
   which means, do adjust for borders but don't change the gravity.  */

static void
x_set_offset (struct frame *f, int xoff, int yoff, int change_gravity)
{
  int modified_top, modified_left;
#ifdef USE_GTK
  int scale = xg_get_scale (f);
#endif

  if (change_gravity > 0)
    {
      f->top_pos = yoff;
      f->left_pos = xoff;
      f->size_hint_flags &= ~ (XNegative | YNegative);
      if (xoff < 0)
	f->size_hint_flags |= XNegative;
      if (yoff < 0)
	f->size_hint_flags |= YNegative;
      f->win_gravity = NorthWestGravity;
    }

  x_calc_absolute_position (f);

  block_input ();
  x_wm_set_size_hint (f, 0, false);

#ifdef USE_GTK
  if (x_gtk_use_window_move)
    {
      /* When a position change was requested and the outer GTK widget
	 has been realized already, leave it to gtk_window_move to
	 DTRT and return.  Used for Bug#25851 and Bug#25943.  Convert
	 from X pixels to GTK scaled pixels.  */
      if (change_gravity != 0 && FRAME_GTK_OUTER_WIDGET (f))
	gtk_window_move (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
			 f->left_pos / scale, f->top_pos / scale);
      unblock_input ();
      return;
    }
#endif /* USE_GTK */

  modified_left = f->left_pos;
  modified_top = f->top_pos;

  if (change_gravity != 0 && FRAME_DISPLAY_INFO (f)->wm_type == X_WMTYPE_A)
    {
      /* Some WMs (twm, wmaker at least) has an offset that is smaller
         than the WM decorations.  So we use the calculated offset instead
         of the WM decoration sizes here (x/y_pixels_outer_diff).  */
      modified_left += FRAME_X_OUTPUT (f)->move_offset_left;
      modified_top += FRAME_X_OUTPUT (f)->move_offset_top;
    }

#ifdef USE_GTK
  /* Make sure we adjust for possible scaling.  */
  gtk_window_move (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)),
		   modified_left / scale, modified_top / scale);
#else
  XMoveWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
	       modified_left, modified_top);
#endif

  /* The following code is too slow over a latent network
     connection.  */
  if (NILP (Vx_lax_frame_positioning))
    {
      /* 'x_sync_with_move' is too costly for dragging child frames.  */
      if (!FRAME_PARENT_FRAME (f)
	  /* If no window manager exists, just calling XSync will be
	     sufficient to ensure that the window geometry has been
	     updated.  */
	  && NILP (Vx_no_window_manager))
	{
	  x_sync_with_move (f, f->left_pos, f->top_pos,
			    FRAME_DISPLAY_INFO (f)->wm_type == X_WMTYPE_UNKNOWN);

	  /* change_gravity is non-zero when this function is called from Lisp to
	     programmatically move a frame.  In that case, we call
	     x_check_expected_move to discover if we have a "Type A" or "Type B"
	     window manager, and, for a "Type A" window manager, adjust the position
	     of the frame.

	     We call x_check_expected_move if a programmatic move occurred, and
	     either the window manager type (A/B) is unknown or it is Type A but we
	     need to compute the top/left offset adjustment for this frame.  */

	  if (change_gravity != 0
	      && (FRAME_DISPLAY_INFO (f)->wm_type == X_WMTYPE_UNKNOWN
		  || (FRAME_DISPLAY_INFO (f)->wm_type == X_WMTYPE_A
		      && (FRAME_X_OUTPUT (f)->move_offset_left == 0
			  && FRAME_X_OUTPUT (f)->move_offset_top == 0))))
	    x_check_expected_move (f, modified_left, modified_top);
	}
      /* Instead, just wait for the last ConfigureWindow request to
	 complete.  No window manager is involved when moving child
	 frames.  */
      else
	XSync (FRAME_X_DISPLAY (f), False);
    }

  unblock_input ();
}

static Window
x_get_wm_check_window (struct x_display_info *dpyinfo)
{
  Window result;
  unsigned char *tmp_data = NULL;
  int rc, actual_format;
  unsigned long actual_size, bytes_remaining;
  Atom actual_type;

  rc = XGetWindowProperty (dpyinfo->display, dpyinfo->root_window,
			   dpyinfo->Xatom_net_supporting_wm_check,
                           0, 1, False, XA_WINDOW, &actual_type,
			   &actual_format, &actual_size,
                           &bytes_remaining, &tmp_data);

  if (rc != Success || actual_type != XA_WINDOW
      || actual_format != 32 || actual_size != 1)
    {
      if (tmp_data)
	XFree (tmp_data);

      return None;
    }

  result = *(Window *) tmp_data;
  XFree (tmp_data);

  return result;
}

/* Return true if _NET_SUPPORTING_WM_CHECK window exists and _NET_SUPPORTED
   on the root window for frame F contains ATOMNAME.
   This is how a WM check shall be done according to the Window Manager
   Specification/Extended Window Manager Hints at
   https://freedesktop.org/wiki/Specifications/wm-spec/.  */

bool
x_wm_supports_1 (struct x_display_info *dpyinfo, Atom want_atom)
{
  Atom actual_type;
  unsigned long actual_size, bytes_remaining;
  int i, rc, actual_format;
  bool ret;
  Window wmcheck_window;
  Window target_window = dpyinfo->root_window;
  int max_len = 65536;
  Display *dpy = dpyinfo->display;
  unsigned char *tmp_data = NULL;
  Atom target_type = XA_WINDOW;

  /* The user says there's no window manager, so take him up on
     it.  */
  if (!NILP (Vx_no_window_manager))
    return false;

  /* If the window system says Emacs is untrusted, there will be no
     way to send any information to the window manager, making any
     hints useless.  */
  if (dpyinfo->untrusted)
    return false;

  block_input ();

  x_catch_errors (dpy);

  wmcheck_window = dpyinfo->net_supported_window;

  if (wmcheck_window == None)
    wmcheck_window = x_get_wm_check_window (dpyinfo);

  if (!x_special_window_exists_p (dpyinfo, wmcheck_window))
    {
      if (dpyinfo->net_supported_window != None)
	{
	  dpyinfo->net_supported_window = None;
	  wmcheck_window = x_get_wm_check_window (dpyinfo);

	  if (!x_special_window_exists_p (dpyinfo, wmcheck_window))
	    {
	      x_uncatch_errors ();
	      unblock_input ();
	      return false;
	    }
	}
      else
	{
	  x_uncatch_errors ();
	  unblock_input ();
	  return false;
	}
    }

  if (dpyinfo->net_supported_window != wmcheck_window)
    {
      /* Window changed, reload atoms */
      if (dpyinfo->net_supported_atoms != NULL)
        XFree (dpyinfo->net_supported_atoms);
      dpyinfo->net_supported_atoms = NULL;
      dpyinfo->nr_net_supported_atoms = 0;
      dpyinfo->net_supported_window = 0;

      target_type = XA_ATOM;
      tmp_data = NULL;
      rc = XGetWindowProperty (dpy, target_window,
                               dpyinfo->Xatom_net_supported,
                               0, max_len, False, target_type,
                               &actual_type, &actual_format, &actual_size,
                               &bytes_remaining, &tmp_data);

      if (rc != Success || actual_type != XA_ATOM || x_had_errors_p (dpy))
        {
          if (tmp_data)
	    XFree (tmp_data);
          x_uncatch_errors ();
          unblock_input ();
          return false;
        }

      dpyinfo->net_supported_atoms = (Atom *) tmp_data;
      dpyinfo->nr_net_supported_atoms = actual_size;
      dpyinfo->net_supported_window = wmcheck_window;
    }

  ret = false;

  for (i = 0; !ret && i < dpyinfo->nr_net_supported_atoms; ++i)
    ret = dpyinfo->net_supported_atoms[i] == want_atom;

  x_uncatch_errors ();
  unblock_input ();

  return ret;
}

bool
x_wm_supports (struct frame *f, Atom want_atom)
{
  return x_wm_supports_1 (FRAME_DISPLAY_INFO (f),
			  want_atom);
}

static void
set_wm_state (Lisp_Object frame, bool add, Atom atom, Atom value)
{
  struct x_display_info *dpyinfo;
  XEvent msg;

  dpyinfo = FRAME_DISPLAY_INFO (XFRAME (frame));
  msg.xclient.type = ClientMessage;
  msg.xclient.window = FRAME_OUTER_WINDOW (XFRAME (frame));
  msg.xclient.message_type = dpyinfo->Xatom_net_wm_state;
  msg.xclient.format = 32;

  msg.xclient.data.l[0] = add ? 1 : 0;
  msg.xclient.data.l[1] = atom;
  msg.xclient.data.l[2] = value;
  msg.xclient.data.l[3] = 1; /* Source indication.  */
  msg.xclient.data.l[4] = 0;

  block_input ();
  XSendEvent (dpyinfo->display, dpyinfo->root_window,
	      False, (SubstructureRedirectMask
		      | SubstructureNotifyMask), &msg);
  unblock_input ();
}

void
x_set_sticky (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  Lisp_Object frame;
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  XSETFRAME (frame, f);

  set_wm_state (frame, !NILP (new_value),
                dpyinfo->Xatom_net_wm_state_sticky, None);
}

void
x_set_shaded (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  Lisp_Object frame;
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  XSETFRAME (frame, f);

  set_wm_state (frame, !NILP (new_value),
                dpyinfo->Xatom_net_wm_state_shaded, None);
}

/**
 * x_set_skip_taskbar:
 *
 * Set frame F's `skip-taskbar' parameter.  If non-nil, this should
 * remove F's icon from the taskbar associated with the display of F's
 * window-system window and inhibit switching to F's window via
 * <Alt>-<TAB>.  If nil, lift these restrictions.
 *
 * Some window managers may not honor this parameter.
 */
void
x_set_skip_taskbar (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  if (!EQ (new_value, old_value))
    {
#ifdef USE_GTK
      xg_set_skip_taskbar (f, new_value);
#else
      Lisp_Object frame;
      struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

      XSETFRAME (frame, f);
      set_wm_state (frame, !NILP (new_value),
		    dpyinfo->Xatom_net_wm_state_skip_taskbar, None);
#endif /* USE_GTK */
      FRAME_SKIP_TASKBAR (f) = !NILP (new_value);
    }
}

/**
 * x_set_z_group:
 *
 * Set frame F's `z-group' parameter.  If `above', F's window-system
 * window is displayed above all windows that do not have the `above'
 * property set.  If nil, F's window is shown below all windows that
 * have the `above' property set and above all windows that have the
 * `below' property set.  If `below', F's window is displayed below all
 * windows that do not have the `below' property set.
 *
 * Some window managers may not honor this parameter.
 *
 * Internally, this function also handles a value 'above-suspended'.
 * That value is used to temporarily remove F from the 'above' group
 * to make sure that it does not obscure a menu currently popped up.
 */
void
x_set_z_group (struct frame *f, Lisp_Object new_value, Lisp_Object old_value)
{
  /* We don't care about old_value.  The window manager might have
     reset the value without telling us.  */
  Lisp_Object frame;
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  XSETFRAME (frame, f);

  if (NILP (new_value))
    {
      set_wm_state (frame, false,
		    dpyinfo->Xatom_net_wm_state_above, None);
      set_wm_state (frame, false,
		    dpyinfo->Xatom_net_wm_state_below, None);
      FRAME_Z_GROUP (f) = z_group_none;
    }
  else if (EQ (new_value, Qabove))
    {
      set_wm_state (frame, true,
		    dpyinfo->Xatom_net_wm_state_above, None);
      set_wm_state (frame, false,
		    dpyinfo->Xatom_net_wm_state_below, None);
      FRAME_Z_GROUP (f) = z_group_above;
    }
  else if (EQ (new_value, Qbelow))
    {
      set_wm_state (frame, false,
		    dpyinfo->Xatom_net_wm_state_above, None);
      set_wm_state (frame, true,
		    dpyinfo->Xatom_net_wm_state_below, None);
      FRAME_Z_GROUP (f) = z_group_below;
    }
  else if (EQ (new_value, Qabove_suspended))
    {
      set_wm_state (frame, false,
		    dpyinfo->Xatom_net_wm_state_above, None);
      FRAME_Z_GROUP (f) = z_group_above_suspended;
    }
  else
    error ("Invalid z-group specification");
}


/* Return the current _NET_WM_STATE.
   SIZE_STATE is set to one of the FULLSCREEN_* values.
   Set *STICKY to the sticky state.

   Return true iff we are not hidden.  */

static bool
x_get_current_wm_state (struct frame *f,
                        Window window,
                        int *size_state,
                        bool *sticky,
			bool *shaded)
{
  unsigned long actual_size;
  int i;
  bool is_hidden = false;
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  long max_len = 65536;
  Atom target_type = XA_ATOM;
  /* If XCB is available, we can avoid three XSync calls.  */
#ifdef USE_XCB
  xcb_get_property_cookie_t prop_cookie;
  xcb_get_property_reply_t *prop;
  typedef xcb_atom_t reply_data_object;
#else
  Display *dpy = FRAME_X_DISPLAY (f);
  unsigned long bytes_remaining;
  int rc, actual_format;
  Atom actual_type;
  unsigned char *tmp_data = NULL;
  typedef Atom reply_data_object;
#endif
  reply_data_object *reply_data;
# if defined GCC_LINT || defined lint
  reply_data_object reply_data_dummy;
  reply_data = &reply_data_dummy;
# endif

  *sticky = false;
  *size_state = FULLSCREEN_NONE;
  *shaded = false;

  block_input ();

#ifdef USE_XCB
  prop_cookie = xcb_get_property (dpyinfo->xcb_connection, 0, window,
                                  dpyinfo->Xatom_net_wm_state,
                                  target_type, 0, max_len);
  prop = xcb_get_property_reply (dpyinfo->xcb_connection, prop_cookie, NULL);
  if (prop && prop->type == target_type)
    {
      int actual_bytes = xcb_get_property_value_length (prop);
      eassume (0 <= actual_bytes);
      actual_size = actual_bytes / sizeof *reply_data;
      reply_data = xcb_get_property_value (prop);
    }
  else
    {
      actual_size = 0;
      is_hidden = FRAME_ICONIFIED_P (f);
    }
#else
  x_catch_errors (dpy);
  rc = XGetWindowProperty (dpy, window, dpyinfo->Xatom_net_wm_state,
                           0, max_len, False, target_type,
                           &actual_type, &actual_format, &actual_size,
                           &bytes_remaining, &tmp_data);

  if (rc == Success && actual_type == target_type && ! x_had_errors_p (dpy))
    reply_data = (Atom *) tmp_data;
  else
    {
      actual_size = 0;
      is_hidden = FRAME_ICONIFIED_P (f);
    }

  x_uncatch_errors ();
#endif

  for (i = 0; i < actual_size; ++i)
    {
      Atom a = reply_data[i];
      if (a == dpyinfo->Xatom_net_wm_state_hidden)
	is_hidden = true;
      else if (a == dpyinfo->Xatom_net_wm_state_maximized_horz)
        {
          if (*size_state == FULLSCREEN_HEIGHT)
            *size_state = FULLSCREEN_MAXIMIZED;
          else
            *size_state = FULLSCREEN_WIDTH;
        }
      else if (a == dpyinfo->Xatom_net_wm_state_maximized_vert)
        {
          if (*size_state == FULLSCREEN_WIDTH)
            *size_state = FULLSCREEN_MAXIMIZED;
          else
            *size_state = FULLSCREEN_HEIGHT;
        }
      else if (a == dpyinfo->Xatom_net_wm_state_fullscreen)
        *size_state = FULLSCREEN_BOTH;
      else if (a == dpyinfo->Xatom_net_wm_state_sticky)
        *sticky = true;
      else if (a == dpyinfo->Xatom_net_wm_state_shaded)
	*shaded = true;
    }

#ifdef USE_XCB
  free (prop);
#else
  if (tmp_data) XFree (tmp_data);
#endif

  unblock_input ();
  return ! is_hidden;
}

/* Do fullscreen as specified in extended window manager hints */

static bool
do_ewmh_fullscreen (struct frame *f)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  bool have_net_atom = x_wm_supports (f, dpyinfo->Xatom_net_wm_state);
  int cur;
  bool dummy;

  x_get_current_wm_state (f, FRAME_OUTER_WINDOW (f), &cur, &dummy, &dummy);

  /* Some window managers don't say they support _NET_WM_STATE, but they do say
     they support _NET_WM_STATE_FULLSCREEN.  Try that also.  */
  if (!have_net_atom)
    have_net_atom = x_wm_supports (f, dpyinfo->Xatom_net_wm_state_fullscreen);

  if (have_net_atom && cur != f->want_fullscreen)
    {
      Lisp_Object frame;

      XSETFRAME (frame, f);

      /* Keep number of calls to set_wm_state as low as possible.
         Some window managers, or possible Gtk+, hangs when too many
         are sent at once.  */
      switch (f->want_fullscreen)
        {
        case FULLSCREEN_BOTH:
          if (cur != FULLSCREEN_BOTH)
            set_wm_state (frame, true, dpyinfo->Xatom_net_wm_state_fullscreen,
                          None);
          break;
        case FULLSCREEN_WIDTH:
	  if (x_frame_normalize_before_maximize && cur == FULLSCREEN_MAXIMIZED)
	    {
	      set_wm_state (frame, false,
			    dpyinfo->Xatom_net_wm_state_maximized_horz,
			    dpyinfo->Xatom_net_wm_state_maximized_vert);
	      set_wm_state (frame, true,
			    dpyinfo->Xatom_net_wm_state_maximized_horz, None);
	    }
	  else
	    {
	      if (cur == FULLSCREEN_BOTH || cur == FULLSCREEN_HEIGHT
		  || cur == FULLSCREEN_MAXIMIZED)
		set_wm_state (frame, false, dpyinfo->Xatom_net_wm_state_fullscreen,
			      dpyinfo->Xatom_net_wm_state_maximized_vert);
	      if (cur != FULLSCREEN_MAXIMIZED || x_frame_normalize_before_maximize)
		set_wm_state (frame, true,
			      dpyinfo->Xatom_net_wm_state_maximized_horz, None);
	    }
          break;
        case FULLSCREEN_HEIGHT:
	  if (x_frame_normalize_before_maximize && cur == FULLSCREEN_MAXIMIZED)
	    {
	      set_wm_state (frame, false,
			    dpyinfo->Xatom_net_wm_state_maximized_horz,
			    dpyinfo->Xatom_net_wm_state_maximized_vert);
	      set_wm_state (frame, true,
			    dpyinfo->Xatom_net_wm_state_maximized_vert, None);
	    }
	  else
	    {
	      if (cur == FULLSCREEN_BOTH || cur == FULLSCREEN_WIDTH
		  || cur == FULLSCREEN_MAXIMIZED)
		set_wm_state (frame, false, dpyinfo->Xatom_net_wm_state_fullscreen,
			      dpyinfo->Xatom_net_wm_state_maximized_horz);
	      if (cur != FULLSCREEN_MAXIMIZED || x_frame_normalize_before_maximize)
		set_wm_state (frame, true,
			      dpyinfo->Xatom_net_wm_state_maximized_vert, None);
	    }
          break;
        case FULLSCREEN_MAXIMIZED:
	  if (x_frame_normalize_before_maximize && cur == FULLSCREEN_BOTH)
	    {
	      set_wm_state (frame, false,
			    dpyinfo->Xatom_net_wm_state_fullscreen, None);
	      set_wm_state (frame, true,
			    dpyinfo->Xatom_net_wm_state_maximized_horz,
			    dpyinfo->Xatom_net_wm_state_maximized_vert);
	    }
	  else if (x_frame_normalize_before_maximize && cur == FULLSCREEN_WIDTH)
	    {
	      set_wm_state (frame, false,
			    dpyinfo->Xatom_net_wm_state_maximized_horz, None);
	      set_wm_state (frame, true,
			    dpyinfo->Xatom_net_wm_state_maximized_horz,
			    dpyinfo->Xatom_net_wm_state_maximized_vert);
	    }
	  else if (x_frame_normalize_before_maximize && cur == FULLSCREEN_HEIGHT)
	    {
	      set_wm_state (frame, false,
			    dpyinfo->Xatom_net_wm_state_maximized_vert, None);
	      set_wm_state (frame, true,
			    dpyinfo->Xatom_net_wm_state_maximized_horz,
			    dpyinfo->Xatom_net_wm_state_maximized_vert);
	    }
	  else
	    {
	      if (cur == FULLSCREEN_BOTH)
		set_wm_state (frame, false, dpyinfo->Xatom_net_wm_state_fullscreen,
			      None);
	      else if (cur == FULLSCREEN_HEIGHT)
		set_wm_state (frame, true,
			      dpyinfo->Xatom_net_wm_state_maximized_horz, None);
	      else if (cur == FULLSCREEN_WIDTH)
		set_wm_state (frame, true, None,
			      dpyinfo->Xatom_net_wm_state_maximized_vert);
	      else
		set_wm_state (frame, true,
			      dpyinfo->Xatom_net_wm_state_maximized_horz,
			      dpyinfo->Xatom_net_wm_state_maximized_vert);
	    }
          break;
        case FULLSCREEN_NONE:
          if (cur == FULLSCREEN_BOTH)
            set_wm_state (frame, false, dpyinfo->Xatom_net_wm_state_fullscreen,
			  None);
          else
            set_wm_state (frame, false,
			  dpyinfo->Xatom_net_wm_state_maximized_horz,
                          dpyinfo->Xatom_net_wm_state_maximized_vert);
        }

      f->want_fullscreen = FULLSCREEN_NONE;

    }

  return have_net_atom;
}

static void
XTfullscreen_hook (struct frame *f)
{
  if (!FRAME_VISIBLE_P (f))
    return;

  block_input ();
  x_check_fullscreen (f);
  unblock_input ();
}


static bool
x_handle_net_wm_state (struct frame *f, const XPropertyEvent *event)
{
  int value = FULLSCREEN_NONE;
  Lisp_Object lval;
  bool sticky = false, shaded = false;
  bool not_hidden = x_get_current_wm_state (f, event->window,
					    &value, &sticky,
					    &shaded);

  lval = Qnil;
  switch (value)
    {
    case FULLSCREEN_WIDTH:
      lval = Qfullwidth;
      break;
    case FULLSCREEN_HEIGHT:
      lval = Qfullheight;
      break;
    case FULLSCREEN_BOTH:
      lval = Qfullboth;
      break;
    case FULLSCREEN_MAXIMIZED:
      lval = Qmaximized;
      break;
    }

  store_frame_param (f, Qfullscreen, lval);
  store_frame_param (f, Qsticky, sticky ? Qt : Qnil);
  store_frame_param (f, Qshaded, shaded ? Qt : Qnil);

  return not_hidden;
}

/* Check if we need to resize the frame due to a fullscreen request.
   If so needed, resize the frame. */
static void
x_check_fullscreen (struct frame *f)
{
  Lisp_Object lval = Qnil;

  if (do_ewmh_fullscreen (f))
    return;

  if (f->output_data.x->parent_desc != FRAME_DISPLAY_INFO (f)->root_window)
    return; /* Only fullscreen without WM or with EWM hints (above). */

  /* Setting fullscreen to nil doesn't do anything.  We could save the
     last non-fullscreen size and restore it, but it seems like a
     lot of work for this unusual case (no window manager running).  */

  if (f->want_fullscreen != FULLSCREEN_NONE)
    {
      int width = FRAME_PIXEL_WIDTH (f), height = FRAME_PIXEL_HEIGHT (f);
      struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

      switch (f->want_fullscreen)
        {
          /* No difference between these two when there is no WM */
        case FULLSCREEN_MAXIMIZED:
          lval = Qmaximized;
          width = x_display_pixel_width (dpyinfo);
          height = x_display_pixel_height (dpyinfo);
          break;
        case FULLSCREEN_BOTH:
          lval = Qfullboth;
          width = x_display_pixel_width (dpyinfo);
          height = x_display_pixel_height (dpyinfo);
          break;
        case FULLSCREEN_WIDTH:
          lval = Qfullwidth;
          width = x_display_pixel_width (dpyinfo);
	  height = height + FRAME_MENUBAR_HEIGHT (f);
	  break;
        case FULLSCREEN_HEIGHT:
          lval = Qfullheight;
          height = x_display_pixel_height (dpyinfo);
	  break;
	default:
	  emacs_abort ();
        }

      x_wm_set_size_hint (f, 0, false);

      XResizeWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		     width, height);

      if (FRAME_VISIBLE_P (f))
	x_wait_for_event (f, ConfigureNotify);
      else
	change_frame_size (f, width, height, false, true, false);
    }

  /* `x_net_wm_state' might have reset the fullscreen frame parameter,
     restore it. */
  store_frame_param (f, Qfullscreen, lval);
}

/* This function is called by x_set_offset to determine whether the window
   manager interfered with the positioning of the frame.  Type A window
   managers position the surrounding window manager decorations a small
   amount above and left of the user-supplied position.  Type B window
   managers position the surrounding window manager decorations at the
   user-specified position.  If we detect a Type A window manager, we
   compensate by moving the window right and down by the proper amount.  */

static void
x_check_expected_move (struct frame *f, int expected_left, int expected_top)
{
  int current_left = 0, current_top = 0;

  /* x_real_positions returns the left and top offsets of the outermost
     window manager window around the frame.  */

  x_real_positions (f, &current_left, &current_top);

  if (current_left != expected_left || current_top != expected_top)
    {
      /* It's a "Type A" window manager. */

      int adjusted_left;
      int adjusted_top;

      FRAME_DISPLAY_INFO (f)->wm_type = X_WMTYPE_A;
      FRAME_X_OUTPUT (f)->move_offset_left = expected_left - current_left;
      FRAME_X_OUTPUT (f)->move_offset_top = expected_top - current_top;

      /* Now fix the mispositioned frame's location. */

      adjusted_left = expected_left + FRAME_X_OUTPUT (f)->move_offset_left;
      adjusted_top = expected_top + FRAME_X_OUTPUT (f)->move_offset_top;

      XMoveWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
                   adjusted_left, adjusted_top);

      x_sync_with_move (f, expected_left, expected_top, false);
    }
  else
    /* It's a "Type B" window manager.  We don't have to adjust the
       frame's position. */
      FRAME_DISPLAY_INFO (f)->wm_type = X_WMTYPE_B;
}


/* Wait for XGetGeometry to return up-to-date position information for a
   recently-moved frame.  Call this immediately after calling XMoveWindow.
   If FUZZY is non-zero, then LEFT and TOP are just estimates of where the
   frame has been moved to, so we use a fuzzy position comparison instead
   of an exact comparison.  */

static void
x_sync_with_move (struct frame *f, int left, int top, bool fuzzy)
{
  sigset_t emptyset;
  int count, current_left, current_top;
  struct timespec fallback;

  sigemptyset (&emptyset);
  count = 0;

  while (count++ < 50)
    {
      current_left = 0;
      current_top = 0;

      /* There is no need to call XSync (even when no window manager
	 is present) because x_real_positions already does that
	 implicitly.  */
      x_real_positions (f, &current_left, &current_top);

      if (fuzzy)
        {
          /* The left fuzz-factor is 10 pixels.  The top fuzz-factor is 40
             pixels.  */

          if (eabs (current_left - left) <= 10
	      && eabs (current_top - top) <= 40)
            return;
	}
      else if (current_left == left && current_top == top)
        return;
    }

  /* As a last resort, just wait 0.5 seconds and hope that XGetGeometry
     will then return up-to-date position info. */

  fallback = dtotimespec (0.5);

  /* This will hang if input is blocked, so use pselect to wait
     instead.  */
  if (input_blocked_p ())
    pselect (0, NULL, NULL, NULL, &fallback, &emptyset);
  else
    wait_reading_process_output (0, 500000000, 0, false, Qnil, NULL, 0);
}

/* Wait for an event on frame F matching EVENTTYPE.  */
void
x_wait_for_event (struct frame *f, int eventtype)
{
  if (!FLOATP (Vx_wait_for_event_timeout))
    return;

  int level = interrupt_input_blocked;
  fd_set fds;
  struct timespec tmo, tmo_at, time_now;
  int fd = ConnectionNumber (FRAME_X_DISPLAY (f));

  f->wait_event_type = eventtype;

  /* Default timeout is 0.1 second.  Hopefully not noticeable.  */
  double timeout = XFLOAT_DATA (Vx_wait_for_event_timeout);
  time_t timeout_seconds = (time_t) timeout;
  tmo = make_timespec
    (timeout_seconds, (long int) ((timeout - timeout_seconds)
                                  * 1000 * 1000 * 1000));
  tmo_at = timespec_add (current_timespec (), tmo);

  while (f->wait_event_type)
    {
      pending_signals = true;
      totally_unblock_input ();
      /* XTread_socket is called after unblock.  */
      block_input ();
      interrupt_input_blocked = level;

      FD_ZERO (&fds);
      FD_SET (fd, &fds);

      time_now = current_timespec ();
      if (timespec_cmp (tmo_at, time_now) < 0)
	break;

      tmo = timespec_sub (tmo_at, time_now);
      if (pselect (fd + 1, &fds, NULL, NULL, &tmo, NULL) == 0)
        break; /* Timeout */
    }

  f->wait_event_type = 0;
}


/* Change the size of frame F's X window to WIDTH/HEIGHT in the case F
   doesn't have a widget.  If CHANGE_GRAVITY, change to
   top-left-corner window gravity for this size change and subsequent
   size changes.  Otherwise leave the window gravity unchanged.  */

static void
x_set_window_size_1 (struct frame *f, bool change_gravity,
		     int width, int height)
{
  if (change_gravity)
    f->win_gravity = NorthWestGravity;
  x_wm_set_size_hint (f, 0, false);

  XResizeWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		 width, height + FRAME_MENUBAR_HEIGHT (f));

  /* We've set {FRAME,PIXEL}_{WIDTH,HEIGHT} to the values we hope to
     receive in the ConfigureNotify event; if we get what we asked
     for, then the event won't cause the screen to become garbaged, so
     we have to make sure to do it here.  */
  SET_FRAME_GARBAGED (f);

  /* The following code is too slow over a latent network
     connection, so skip it when the user says so.  */

  if (!NILP (Vx_lax_frame_positioning))
    return;

  /* Now, strictly speaking, we can't be sure that this is accurate,
     but the window manager will get around to dealing with the size
     change request eventually, and we'll hear how it went when the
     ConfigureNotify event gets here.

     We could just not bother storing any of this information here,
     and let the ConfigureNotify event set everything up, but that
     might be kind of confusing to the Lisp code, since size changes
     wouldn't be reported in the frame parameters until some random
     point in the future when the ConfigureNotify event arrives.

     Pass true for DELAY since we can't run Lisp code inside of
     a BLOCK_INPUT.  */

  /* But the ConfigureNotify may in fact never arrive, and then this is
     not right if the frame is visible.  Instead wait (with timeout)
     for the ConfigureNotify.  */
  if (FRAME_VISIBLE_P (f))
    {
      x_wait_for_event (f, ConfigureNotify);

      if (CONSP (frame_size_history))
	frame_size_history_extra
	  (f, build_string ("x_set_window_size_1, visible"),
	   FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f), width, height,
	   f->new_width, f->new_height);
    }
  else
    {
      if (CONSP (frame_size_history))
	frame_size_history_extra
	  (f, build_string ("x_set_window_size_1, invisible"),
	   FRAME_PIXEL_WIDTH (f), FRAME_PIXEL_HEIGHT (f), width, height,
	   f->new_width, f->new_height);

      /* Call adjust_frame_size right away as with GTK.  It might be
	 tempting to clear out f->new_width and f->new_height here.  */
      adjust_frame_size (f, FRAME_PIXEL_TO_TEXT_WIDTH (f, width),
			 FRAME_PIXEL_TO_TEXT_HEIGHT (f, height),
			 5, 0, Qx_set_window_size_1);
    }
}


/* Change the size of frame F's X window to WIDTH and HEIGHT pixels.  If
   CHANGE_GRAVITY, change to top-left-corner window gravity for this
   size change and subsequent size changes.  Otherwise we leave the
   window gravity unchanged.  */

void
x_set_window_size (struct frame *f, bool change_gravity,
		   int width, int height)
{
  block_input ();

#ifdef USE_GTK
  if (FRAME_GTK_WIDGET (f))
    xg_frame_set_char_size (f, width, height);
  else
    x_set_window_size_1 (f, change_gravity, width, height);
#else /* not USE_GTK */
  x_set_window_size_1 (f, change_gravity, width, height);
  x_clear_under_internal_border (f);
#endif /* not USE_GTK */

  /* If cursor was outside the new size, mark it as off.  */
  mark_window_cursors_off (XWINDOW (f->root_window));

  /* Clear out any recollection of where the mouse highlighting was,
     since it might be in a place that's outside the new frame size.
     Actually checking whether it is outside is a pain in the neck,
     so don't try--just let the highlighting be done afresh with new size.  */
  cancel_mouse_face (f);

  unblock_input ();

  do_pending_window_change (false);
}

static void
x_set_window_size_and_position_1 (struct frame *f, int width, int height)
{
  int x = f->left_pos;
  int y = f->top_pos;

  x_wm_set_size_hint (f, 0, false);

  XMoveResizeWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		     x, y, width, height + FRAME_MENUBAR_HEIGHT (f));

  SET_FRAME_GARBAGED (f);

  if (FRAME_VISIBLE_P (f))
    x_wait_for_event (f, ConfigureNotify);
  else
    /* Call adjust_frame_size right away as with GTK.  It might be
       tempting to clear out f->new_width and f->new_height here.  */
    adjust_frame_size (f, FRAME_PIXEL_TO_TEXT_WIDTH (f, width),
		       FRAME_PIXEL_TO_TEXT_HEIGHT (f, height),
		       5, 0, Qx_set_window_size_1);
}

void
x_set_window_size_and_position (struct frame *f, int width, int height)
{
  block_input ();

#ifdef USE_GTK
  if (FRAME_GTK_WIDGET (f))
    xg_frame_set_size_and_position (f, width, height);
  else
    x_set_window_size_and_position_1 (f, width, height);
#else /* not USE_GTK */
  x_set_window_size_and_position_1 (f, width, height);
#endif /* USE_GTK */

  x_clear_under_internal_border (f);

  /* If cursor was outside the new size, mark it as off.  */
  mark_window_cursors_off (XWINDOW (FRAME_ROOT_WINDOW (f)));

  /* Clear out any recollection of where the mouse highlighting was,
     since it might be in a place that's outside the new frame size.
     Actually checking whether it is outside is a pain in the neck,
     so don't try--just let the highlighting be done afresh with new size.  */
  cancel_mouse_face (f);

  unblock_input ();

  do_pending_window_change (false);
}

/* Move the mouse to position pixel PIX_X, PIX_Y relative to frame F.  */

void
frame_set_mouse_pixel_position (struct frame *f, int pix_x, int pix_y)
{
#ifdef HAVE_XINPUT2
  int deviceid;

  deviceid = FRAME_DISPLAY_INFO (f)->client_pointer_device;

  if (FRAME_DISPLAY_INFO (f)->supports_xi2
      && deviceid != -1)
    {
      block_input ();
      x_ignore_errors_for_next_request (FRAME_DISPLAY_INFO (f), 0);
      XIWarpPointer (FRAME_X_DISPLAY (f), deviceid, None,
		     FRAME_X_WINDOW (f), 0, 0, 0, 0, pix_x, pix_y);
      x_stop_ignoring_errors (FRAME_DISPLAY_INFO (f));
      unblock_input ();
    }
  else
#endif
    XWarpPointer (FRAME_X_DISPLAY (f), None, FRAME_X_WINDOW (f),
		  0, 0, 0, 0, pix_x, pix_y);
}

/* Raise frame F.  */

static void
x_raise_frame (struct frame *f)
{
  block_input ();

  if (FRAME_VISIBLE_P (f))
    {
      XRaiseWindow (FRAME_X_DISPLAY (f),
		    FRAME_OUTER_WINDOW (f));
      XFlush (FRAME_X_DISPLAY (f));
    }

  unblock_input ();
}

static void
x_lower_frame_1 (struct frame *f)
{
  Window *windows;
  Lisp_Object frame, tail;
  struct frame *sibling;

  windows = alloca (2 * sizeof *windows);

  /* Lowering a child frame leads to the window being put below any
     scroll bars on the parent.  To avoid that, restack the child
     frame below all of its siblings instead of just lowering it.  */

  FOR_EACH_FRAME (tail, frame)
    {
      sibling = XFRAME (frame);

      if (sibling == f)
	continue;

      if (FRAME_PARENT_FRAME (sibling)
	  != FRAME_PARENT_FRAME (f))
	continue;

      windows[0] = FRAME_OUTER_WINDOW (sibling);
      windows[1] = FRAME_OUTER_WINDOW (f);

      XRestackWindows (FRAME_X_DISPLAY (f), windows, 2);
    }
}

/* Lower frame F.  */

static void
x_lower_frame (struct frame *f)
{
  if (FRAME_PARENT_FRAME (f)
      && (FRAME_HAS_VERTICAL_SCROLL_BARS (FRAME_PARENT_FRAME (f))
	  || FRAME_HAS_HORIZONTAL_SCROLL_BARS (FRAME_PARENT_FRAME (f))))
    x_lower_frame_1 (f);
  else
    XLowerWindow (FRAME_X_DISPLAY (f),
		  FRAME_OUTER_WINDOW (f));

#ifdef HAVE_XWIDGETS
  /* Make sure any X windows owned by xwidget views of the parent
     still display below the lowered frame.  */

  if (FRAME_PARENT_FRAME (f))
    lower_frame_xwidget_views (FRAME_PARENT_FRAME (f));
#endif

  XFlush (FRAME_X_DISPLAY (f));
}

static void
XTframe_raise_lower (struct frame *f, bool raise_flag)
{
  if (raise_flag)
    x_raise_frame (f);
  else
    x_lower_frame (f);
}

/* Request focus with XEmbed */

static void
xembed_request_focus (struct frame *f)
{
  /* See XEmbed Protocol Specification at
     https://freedesktop.org/wiki/Specifications/xembed-spec/  */
  if (FRAME_VISIBLE_P (f))
    xembed_send_message (f, CurrentTime,
			 XEMBED_REQUEST_FOCUS, 0, 0, 0);
}

static Bool
server_timestamp_predicate (Display *display, XEvent *xevent,
			    XPointer arg)
{
  XID *args = (XID *) arg;

  if (xevent->type == PropertyNotify
      && xevent->xproperty.window == args[0]
      && xevent->xproperty.atom == args[1])
    return True;

  return False;
}

/* Get the server time.  The X server is guaranteed to deliver the
   PropertyNotify event, so there is no reason to use x_if_event.  */

static Time
x_get_server_time (struct frame *f)
{
  Atom property_atom;
  XEvent property_dummy;
  struct x_display_info *dpyinfo;
  XID client_data[2];
#if defined HAVE_XSYNC && !defined USE_GTK && defined HAVE_CLOCK_GETTIME
  uint_fast64_t current_monotonic_time;
#endif

  /* If the server time is the same as the monotonic time, avoid a
     roundtrip by using that instead.  */

#if defined HAVE_XSYNC && !defined USE_GTK && defined HAVE_CLOCK_GETTIME
  if (FRAME_DISPLAY_INFO (f)->server_time_monotonic_p)
    {
      current_monotonic_time = x_sync_current_monotonic_time ();

      if (current_monotonic_time)
	/* Truncate the time to CARD32.  */
	return (current_monotonic_time / 1000) & X_ULONG_MAX;
    }
#endif

  dpyinfo = FRAME_DISPLAY_INFO (f);
  property_atom = dpyinfo->Xatom_EMACS_SERVER_TIME_PROP;
  client_data[0] = FRAME_OUTER_WINDOW (f);
  client_data[1] = property_atom;

  XChangeProperty (dpyinfo->display, FRAME_OUTER_WINDOW (f),
		   property_atom, XA_ATOM, 32,
		   PropModeReplace,
		   (unsigned char *) &property_atom, 1);

  XIfEvent (dpyinfo->display, &property_dummy,
	    server_timestamp_predicate, (XPointer) client_data);

  return property_dummy.xproperty.time;
}

/* Activate frame with Extended Window Manager Hints */

static void
x_ewmh_activate_frame (struct frame *f)
{
  XEvent msg;
  struct x_display_info *dpyinfo;
  Time time;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (FRAME_VISIBLE_P (f))
    {
      /* See the documentation at
	 https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html
	 for more details on the format of this message.  */
      msg.xclient.type = ClientMessage;
      msg.xclient.window = FRAME_OUTER_WINDOW (f);
      msg.xclient.message_type = dpyinfo->Xatom_net_active_window;
      msg.xclient.format = 32;
      msg.xclient.data.l[0] = 1;
      msg.xclient.data.l[1] = dpyinfo->last_user_time;
      msg.xclient.data.l[2] = (!dpyinfo->x_focus_frame
			       ? None
			       : FRAME_OUTER_WINDOW (dpyinfo->x_focus_frame));
      msg.xclient.data.l[3] = 0;
      msg.xclient.data.l[4] = 0;

      /* No frame is currently focused on that display, so apply any
	 bypass for focus stealing prevention that the user has
	 specified.  */
      if (!dpyinfo->x_focus_frame)
	{
	  if (EQ (Vx_allow_focus_stealing, Qimitate_pager))
	    msg.xclient.data.l[0] = 2;
	  else if (EQ (Vx_allow_focus_stealing, Qnewer_time))
	    {
	      block_input ();
	      time = x_get_server_time (f);
#ifdef USE_GTK
	      x_set_gtk_user_time (f, time);
#endif
	      /* Temporarily override dpyinfo->x_focus_frame so the
		 user time property is set on the right window.  */
	      dpyinfo->x_focus_frame = f;
	      x_display_set_last_user_time (dpyinfo, time, true, true);
	      dpyinfo->x_focus_frame = NULL;
	      unblock_input ();

	      msg.xclient.data.l[1] = time;
	    }
	  else if (EQ (Vx_allow_focus_stealing, Qraise_and_focus))
	    {
	      time = x_get_server_time (f);

	      x_set_input_focus (FRAME_DISPLAY_INFO (f),
				 FRAME_OUTER_WINDOW (f),
				 time);
	      XRaiseWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f));

	      return;
	    }
	}

      XSendEvent (dpyinfo->display, dpyinfo->root_window,
		  False, (SubstructureRedirectMask
			  | SubstructureNotifyMask), &msg);
    }
}

static Lisp_Object
x_get_focus_frame (struct frame *f)
{
  Lisp_Object lisp_focus;

  struct frame *focus =  FRAME_DISPLAY_INFO (f)->x_focus_frame;

  if (!focus)
    return Qnil;

  XSETFRAME (lisp_focus, focus);
  return lisp_focus;
}

/* Return the toplevel parent of F, if it is a child frame.
   Otherwise, return NULL.  */

static struct frame *
x_get_toplevel_parent (struct frame *f)
{
  struct frame *parent;

  if (!FRAME_PARENT_FRAME (f))
    return NULL;

  parent = FRAME_PARENT_FRAME (f);

  while (FRAME_PARENT_FRAME (parent))
    parent = FRAME_PARENT_FRAME (parent);

  return parent;
}

static void
x_set_input_focus (struct x_display_info *dpyinfo, Window window,
		   Time time)
{
#ifdef HAVE_XINPUT2
  struct xi_device_t *device;
#endif

  /* Do the equivalent of XSetInputFocus with the specified window and
     time, but use the attachment to the device that Emacs has
     designated the client pointer on X Input Extension builds.
     Asynchronously trap errors around the generated XI_SetFocus or
     SetInputFocus request, in case the device has been destroyed or
     the window obscured.

     The revert_to will be set to RevertToParent for generated
     SetInputFocus requests.  */

#ifdef HAVE_XINPUT2
  if (dpyinfo->supports_xi2
      && dpyinfo->client_pointer_device != -1)
    {
      device = xi_device_from_id (dpyinfo, dpyinfo->client_pointer_device);

      /* The device is a master pointer.  Use its attachment, which
	 should be the master keyboard.  */

      if (device)
	{
	  eassert (device->use == XIMasterPointer);

	  x_ignore_errors_for_next_request (dpyinfo, 0);
	  XISetFocus (dpyinfo->display, device->attachment,
		      /* Note that the input extension
			 only supports RevertToParent-type
			 behavior.  */
		      window, time);
	  x_stop_ignoring_errors (dpyinfo);

	  return;
	}
    }
#endif

  /* Otherwise, use the pointer device that the X server says is the
     client pointer.  */
  x_ignore_errors_for_next_request (dpyinfo, 0);
  XSetInputFocus (dpyinfo->display, window, RevertToParent, time);
  x_stop_ignoring_errors (dpyinfo);
}

/* In certain situations, when the window manager follows a
   click-to-focus policy, there seems to be no way around calling
   XSetInputFocus to give another frame the input focus.

   In an ideal world, XSetInputFocus should generally be avoided so
   that applications don't interfere with the window manager's focus
   policy.  But I think it's okay to use when it's clearly done
   following a user-command.  */

static void
x_focus_frame (struct frame *f, bool noactivate)
{
  struct x_display_info *dpyinfo;
  Time time;

  dpyinfo = FRAME_DISPLAY_INFO (f);

  if (dpyinfo->untrusted)
    /* The X server ignores all input focus related requests from
       untrusted clients.  */
    return;

  /* The code below is not reentrant wrt to dpyinfo->x_focus_frame and
     friends being set.  */
  block_input ();

#ifdef HAVE_GTK3
  /* read_minibuf assumes that calling Fx_focus_frame on a frame that
     is already selected won't move the focus elsewhere, and thereby
     disrupt any focus redirection to e.g. a minibuffer frame that
     might be activated between that call being made and the
     consequent XI_FocusIn/Out events arriving.  This is true whether
     the focus is ultimately transferred back to the frame it was
     initially on or not.

     GTK 3 moves the keyboard focus to the edit widget's window
     whenever it receives a FocusIn event targeting the outer window.
     This operation gives rise to a FocusOut event that clears
     device->focus_frame, which in turn prompts xi_handle_focus_change
     to clear the display's focus frame.  The next FocusIn event
     destined for the same frame registers as a new focus, which
     cancels any focus redirection from that frame.

     To prevent this chain of events from disrupting focus redirection
     when the minibuffer is activated twice in rapid succession while
     configured to redirect focus to a minibuffer frame, ignore frames
     which hold the input focus and are connected to a minibuffer
     window.  (bug#65116)*/

  if (f == dpyinfo->x_focus_frame && !FRAME_HAS_MINIBUF_P (f))
    {
      unblock_input ();
      return;
    }
#endif /* HAVE_GTK3 */

  if (FRAME_X_EMBEDDED_P (f))
    /* For Xembedded frames, normally the embedder forwards key
       events.  See XEmbed Protocol Specification at
       https://freedesktop.org/wiki/Specifications/xembed-spec/  */
    xembed_request_focus (f);
  else
    {
      if (!noactivate
	  /* If F is override-redirect, use SetInputFocus instead.
	     Override-redirect frames are not subject to window
	     management.  */
	  && !FRAME_OVERRIDE_REDIRECT (f)
	  /* If F is a child frame, use SetInputFocus instead.  This
	     may not work if its parent is not activated.  */
	  && !FRAME_PARENT_FRAME (f)
	  /* If the focus is being transferred from a child frame to
	     its toplevel parent, also use SetInputFocus.  */
	  && (!dpyinfo->x_focus_frame
	      || (x_get_toplevel_parent (dpyinfo->x_focus_frame)
		  != f))
	  && x_wm_supports (f, dpyinfo->Xatom_net_active_window))
	{
	  /* When window manager activation is possible, use it
	     instead.  The window manager is expected to perform any
	     necessary actions such as raising the frame, moving it to
	     the current workspace, and mapping it, etc, before moving
	     input focus to the frame.  */
	  x_ewmh_activate_frame (f);
	  goto out;
	}

      if (NILP (Vx_no_window_manager))
	{
	  /* Use the last user time.  It is invalid to use CurrentTime
	     according to the ICCCM:

	       Clients that use a SetInputFocus request must set the
	       time field to the timestamp of the event that caused
	       them to make the attempt. [...] Note that clients must
	       not use CurrentTime in the time field.  */
	  time = dpyinfo->last_user_time;

	  /* Unless the focus doesn't belong to Emacs anymore and
	     `x-allow-focus-stealing' is set to Qnewer_time.  */
	  if (EQ (Vx_allow_focus_stealing, Qnewer_time)
	      && !dpyinfo->x_focus_frame)
	    time = x_get_server_time (f);

	  /* Ignore any BadMatch error this request might result in.
	     A BadMatch error can occur if the window was obscured
	     after the time of the last user interaction without
	     changing the last-focus-change-time.  */
	  x_set_input_focus (FRAME_DISPLAY_INFO (f), FRAME_OUTER_WINDOW (f),
			     time);
	}
      else
	x_set_input_focus (FRAME_DISPLAY_INFO (f), FRAME_OUTER_WINDOW (f),
			   /* But when no window manager is in use,
			      respecting the ICCCM doesn't really
			      matter.  */
			   CurrentTime);
    }

 out:
  unblock_input ();
}


/* XEmbed implementation.  */

#if defined USE_X_TOOLKIT || ! defined USE_GTK

/* XEmbed implementation.  */

#define XEMBED_VERSION 0

static void
xembed_set_info (struct frame *f, enum xembed_info flags)
{
  unsigned long data[2];
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  data[0] = XEMBED_VERSION;
  data[1] = flags;

  XChangeProperty (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
                   dpyinfo->Xatom_XEMBED_INFO, dpyinfo->Xatom_XEMBED_INFO,
		   32, PropModeReplace, (unsigned char *) data, 2);
}
#endif /* defined USE_X_TOOLKIT || ! defined USE_GTK */

static void
xembed_send_message (struct frame *f, Time t, enum xembed_message msg,
		     long int detail, long int data1, long int data2)
{
  XEvent event;

  event.xclient.type = ClientMessage;
  event.xclient.window = FRAME_X_OUTPUT (f)->parent_desc;
  event.xclient.message_type = FRAME_DISPLAY_INFO (f)->Xatom_XEMBED;
  event.xclient.format = 32;
  event.xclient.data.l[0] = t;
  event.xclient.data.l[1] = msg;
  event.xclient.data.l[2] = detail;
  event.xclient.data.l[3] = data1;
  event.xclient.data.l[4] = data2;

  /* XXX: the XEmbed spec tells us to trap errors around this request,
     but I don't understand why: there is no way for clients to
     survive the death of the parent anyway.  */

  x_ignore_errors_for_next_request (FRAME_DISPLAY_INFO (f), 0);
  XSendEvent (FRAME_X_DISPLAY (f), FRAME_X_OUTPUT (f)->parent_desc,
	      False, NoEventMask, &event);
  x_stop_ignoring_errors (FRAME_DISPLAY_INFO (f));
}

/* Change of visibility.  */

/* This tries to wait until the frame is really visible, depending on
   the value of Vx_wait_for_event_timeout.
   However, if the window manager asks the user where to position
   the frame, this will return before the user finishes doing that.
   The frame will not actually be visible at that time,
   but it will become visible later when the window manager
   finishes with it.  */

void
x_make_frame_visible (struct frame *f)
{
#ifndef USE_GTK
  struct x_display_info *dpyinfo;
  struct x_output *output;
#endif
  bool output_flushed;

  if (FRAME_PARENT_FRAME (f))
    {
      if (!FRAME_VISIBLE_P (f))
	{
	  block_input ();
#ifdef USE_GTK
	  gtk_widget_show_all (FRAME_GTK_OUTER_WIDGET (f));
	  XMoveWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		       f->left_pos, f->top_pos);
#else
	  XMapRaised (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f));
#endif
	  unblock_input ();

	  SET_FRAME_VISIBLE (f, true);
	  SET_FRAME_ICONIFIED (f, false);
	}
      return;
    }

  block_input ();

  gui_set_bitmap_icon (f);

#ifndef USE_GTK
  dpyinfo = FRAME_DISPLAY_INFO (f);
#endif

  if (! FRAME_VISIBLE_P (f))
    {
      /* We test asked_for_visible here to make sure we don't
         call x_set_offset a second time
         if we get to x_make_frame_visible a second time
	 before the window gets really visible.  */
      if (! FRAME_ICONIFIED_P (f)
	  && ! FRAME_X_EMBEDDED_P (f)
	  && ! f->output_data.x->asked_for_visible)
	x_set_offset (f, f->left_pos, f->top_pos, 0);

#ifndef USE_GTK
      output = FRAME_X_OUTPUT (f);
      x_update_frame_user_time_window (f);

      /* It's been a while since I wrote that code...  I don't
	 remember if it can leave `user_time_window' unset or not.  */
      if (output->user_time_window != None)
	{
	  if (dpyinfo->last_user_time)
	    XChangeProperty (dpyinfo->display, output->user_time_window,
			     dpyinfo->Xatom_net_wm_user_time,
			     XA_CARDINAL, 32, PropModeReplace,
			     (unsigned char *) &dpyinfo->last_user_time, 1);
	  else
	    XDeleteProperty (dpyinfo->display, output->user_time_window,
			     dpyinfo->Xatom_net_wm_user_time);
	}
#endif

      f->output_data.x->asked_for_visible = true;

      if (! EQ (Vx_no_window_manager, Qt))
	x_wm_set_window_state (f, NormalState);
#ifdef USE_X_TOOLKIT
      if (FRAME_X_EMBEDDED_P (f))
	xembed_set_info (f, XEMBED_MAPPED);
      else
	{
	  /* This was XtPopup, but that did nothing for an iconified frame.  */
	  XtMapWidget (f->output_data.x->widget);
	}
#else /* not USE_X_TOOLKIT */
#ifdef USE_GTK
      gtk_widget_show_all (FRAME_GTK_OUTER_WIDGET (f));
      gtk_window_deiconify (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));
#else
      if (FRAME_X_EMBEDDED_P (f))
	xembed_set_info (f, XEMBED_MAPPED);
      else
	XMapRaised (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif /* not USE_GTK */
#endif /* not USE_X_TOOLKIT */

      if (FRAME_X_EMBEDDED_P (f))
	{
	  SET_FRAME_VISIBLE (f, true);
	  SET_FRAME_ICONIFIED (f, false);
	}
    }

  /* Synchronize to ensure Emacs knows the frame is visible
     before we do anything else.  We do this loop with input not blocked
     so that incoming events are handled.  */
  {
    Lisp_Object frame;
    /* This must be before UNBLOCK_INPUT
       since events that arrive in response to the actions above
       will set it when they are handled.  */
    bool previously_visible = f->output_data.x->has_been_visible;

    XSETFRAME (frame, f);

    int original_left = f->left_pos;
    int original_top = f->top_pos;

    /* This must come after we set COUNT.  */
    unblock_input ();

    /* Keep track of whether or not the output buffer was flushed, to
       avoid any extra flushes.  */
    output_flushed = false;

    /* We unblock here so that arriving X events are processed.  */

    /* Now move the window back to where it was "supposed to be".
       But don't do it if the gravity is negative.
       When the gravity is negative, this uses a position
       that is 3 pixels too low.  Perhaps that's really the border width.

       Don't do this if the window has never been visible before,
       because the window manager may choose the position
       and we don't want to override it.  */

    if (!FRAME_VISIBLE_P (f)
	&& !FRAME_ICONIFIED_P (f)
	&& !FRAME_X_EMBEDDED_P (f)
	&& !FRAME_PARENT_FRAME (f)
	&& NILP (Vx_lax_frame_positioning)
	&& f->win_gravity == NorthWestGravity
	&& previously_visible)
      {
	Drawable rootw;
	int x, y;
	unsigned int width, height, border, depth;

	block_input ();

	/* On some window managers (such as FVWM) moving an existing
	   window, even to the same place, causes the window manager
	   to introduce an offset.  This can cause the window to move
	   to an unexpected location.  Check the geometry (a little
	   slow here) and then verify that the window is in the right
	   place.  If the window is not in the right place, move it
	   there, and take the potential window manager hit.  */
	XGetGeometry (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		      &rootw, &x, &y, &width, &height, &border, &depth);
	output_flushed = true;

	if (original_left != x || original_top != y)
	  XMoveWindow (FRAME_X_DISPLAY (f), FRAME_OUTER_WINDOW (f),
		       original_left, original_top);

	unblock_input ();
      }

    /* Try to wait for a MapNotify event (that is what tells us when a
       frame becomes visible).  Unless `x-lax-frame-positioning' is
       non-nil: there, that is a little slow.  */

#ifdef CYGWIN
    /* On Cygwin, which uses input polling, we need to force input to
       be read.  See
       https://lists.gnu.org/r/emacs-devel/2013-12/msg00351.html
       and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=24091#131.
       Fake an alarm signal to let the handler know that there's
       something to be read.

       It could be confusing if a real alarm arrives while processing
       the fake one.  Turn it off and let the handler reset it.  */
    int old_poll_suppress_count = poll_suppress_count;
    poll_suppress_count = 1;
    poll_for_input_1 ();
    poll_suppress_count = old_poll_suppress_count;
#endif

    if (!FRAME_VISIBLE_P (f)
	&& NILP (Vx_lax_frame_positioning))
      {
	if (CONSP (frame_size_history))
	  frame_size_history_plain
	    (f, build_string ("x_make_frame_visible"));

	x_wait_for_event (f, MapNotify);
	output_flushed = true;
      }

    if (!output_flushed)
      x_flush (f);
  }
}

/* Change from mapped state to withdrawn state.  */

/* Make the frame visible (mapped and not iconified).  */

void
x_make_frame_invisible (struct frame *f)
{
  Window window;

  /* Use the frame's outermost window, not the one we normally draw on.  */
  window = FRAME_OUTER_WINDOW (f);

  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_DISPLAY_INFO (f)->highlight_frame == f)
    FRAME_DISPLAY_INFO (f)->highlight_frame = 0;

  block_input ();

#ifdef HAVE_XINPUT2_2
  /* Remove any touch points associated with F.  */
  xi_unlink_touch_points (f);
#endif

  /* Before unmapping the window, update the WM_SIZE_HINTS property to claim
     that the current position of the window is user-specified, rather than
     program-specified, so that when the window is mapped again, it will be
     placed at the same location, without forcing the user to position it
     by hand again (they have already done that once for this window.)  */
  x_wm_set_size_hint (f, 0, true);

#ifdef USE_GTK
  if (FRAME_GTK_OUTER_WIDGET (f))
    gtk_widget_hide (FRAME_GTK_OUTER_WIDGET (f));
  else
#else
  if (FRAME_X_EMBEDDED_P (f))
    xembed_set_info (f, 0);
  else
#endif

    if (! XWithdrawWindow (FRAME_X_DISPLAY (f), window,
			   DefaultScreen (FRAME_X_DISPLAY (f))))
      {
	unblock_input ();
	error ("Can't notify window manager of window withdrawal");
      }

  /* Don't perform the synchronization if the network connection is
     slow, and the user says it is unwanted.  */
  if (NILP (Vx_lax_frame_positioning))
    XSync (FRAME_X_DISPLAY (f), False);

  /* We can't distinguish this from iconification
     just by the event that we get from the server.
     So we can't win using the usual strategy of letting
     FRAME_SAMPLE_VISIBILITY set this.  So do it by hand,
     and synchronize with the server to make sure we agree.  */
  SET_FRAME_VISIBLE (f, 0);
  SET_FRAME_ICONIFIED (f, false);

  if (CONSP (frame_size_history))
    frame_size_history_plain (f, build_string ("x_make_frame_invisible"));

  unblock_input ();
}

static void
x_make_frame_visible_invisible (struct frame *f, bool visible)
{
  if (visible)
    x_make_frame_visible (f);
  else
    x_make_frame_invisible (f);
}

Cursor
x_create_font_cursor (struct x_display_info *dpyinfo, int glyph)
{
  if (glyph <= 65535)
    return XCreateFontCursor (dpyinfo->display, glyph);

  /* x-pointer-invisible cannot fit in CARD16, and thus cannot be any
     existing cursor.  */
  return make_invisible_cursor (dpyinfo);
}


/* Change window state from mapped to iconified.  */

void
x_iconify_frame (struct frame *f)
{
#ifdef USE_X_TOOLKIT
  int result;
#endif

  /* Don't keep the highlight on an invisible frame.  */
  if (FRAME_DISPLAY_INFO (f)->highlight_frame == f)
    FRAME_DISPLAY_INFO (f)->highlight_frame = 0;

  if (FRAME_ICONIFIED_P (f))
    return;

  block_input ();

  gui_set_bitmap_icon (f);

#if defined (USE_GTK)
  if (FRAME_GTK_OUTER_WIDGET (f))
    {
      if (! FRAME_VISIBLE_P (f))
        gtk_widget_show_all (FRAME_GTK_OUTER_WIDGET (f));

      gtk_window_iconify (GTK_WINDOW (FRAME_GTK_OUTER_WIDGET (f)));
      SET_FRAME_VISIBLE (f, 0);
      SET_FRAME_ICONIFIED (f, true);
      unblock_input ();
      return;
    }
#endif

#ifdef USE_X_TOOLKIT

  if (! FRAME_VISIBLE_P (f))
    {
      if (! EQ (Vx_no_window_manager, Qt))
	x_wm_set_window_state (f, IconicState);
      /* This was XtPopup, but that did nothing for an iconified frame.  */
      XtMapWidget (f->output_data.x->widget);
      /* The server won't give us any event to indicate
	 that an invisible frame was changed to an icon,
	 so we have to record it here.  */
      SET_FRAME_VISIBLE (f, 0);
      SET_FRAME_ICONIFIED (f, true);
      unblock_input ();
      return;
    }

  result = XIconifyWindow (FRAME_X_DISPLAY (f),
			   XtWindow (f->output_data.x->widget),
			   DefaultScreen (FRAME_X_DISPLAY (f)));
  unblock_input ();

  if (!result)
    error ("Can't notify window manager of iconification");

  SET_FRAME_ICONIFIED (f, true);
  SET_FRAME_VISIBLE (f, 0);

  block_input ();
  XFlush (FRAME_X_DISPLAY (f));
  unblock_input ();
#else /* not USE_X_TOOLKIT */

  /* Make sure the X server knows where the window should be positioned,
     in case the user deiconifies with the window manager.  */
  if (! FRAME_VISIBLE_P (f)
      && ! FRAME_ICONIFIED_P (f)
      && ! FRAME_X_EMBEDDED_P (f))
    x_set_offset (f, f->left_pos, f->top_pos, 0);

  /* Since we don't know which revision of X we're running, we'll use both
     the X11R3 and X11R4 techniques.  I don't know if this is a good idea.  */

  /* X11R4: send a ClientMessage to the window manager using the
     WM_CHANGE_STATE type.  */
  {
    XEvent msg;

    msg.xclient.window = FRAME_X_WINDOW (f);
    msg.xclient.type = ClientMessage;
    msg.xclient.message_type = FRAME_DISPLAY_INFO (f)->Xatom_wm_change_state;
    msg.xclient.format = 32;
    msg.xclient.data.l[0] = IconicState;
    msg.xclient.data.l[1] = 0;
    msg.xclient.data.l[2] = 0;
    msg.xclient.data.l[3] = 0;
    msg.xclient.data.l[4] = 0;

    if (! XSendEvent (FRAME_X_DISPLAY (f),
		      FRAME_DISPLAY_INFO (f)->root_window,
		      False,
		      SubstructureRedirectMask | SubstructureNotifyMask,
		      &msg))
      {
	unblock_input ();
	error ("Can't notify window manager of iconification");
      }
  }

  /* X11R3: set the initial_state field of the window manager hints to
     IconicState.  */
  x_wm_set_window_state (f, IconicState);

  if (!FRAME_VISIBLE_P (f))
    {
      /* If the frame was withdrawn, before, we must map it.  */
      XMapRaised (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
    }

  SET_FRAME_ICONIFIED (f, true);
  SET_FRAME_VISIBLE (f, 0);

  XFlush (FRAME_X_DISPLAY (f));
  unblock_input ();
#endif /* not USE_X_TOOLKIT */
}


/* Free X resources of frame F.  */

void
x_free_frame_resources (struct frame *f)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);
  Mouse_HLInfo *hlinfo = &dpyinfo->mouse_highlight;
#ifdef USE_X_TOOLKIT
  Lisp_Object bar;
  struct scroll_bar *b;
#endif

  block_input ();

#ifdef HAVE_XINPUT2
  /* Remove any record of this frame being focused.  */
  xi_handle_delete_frame (dpyinfo, f);
#endif

#ifdef HAVE_XINPUT2_2
  /* Remove any touch points associated with F.  */
  xi_unlink_touch_points (f);
#endif

  /* We must free faces before destroying windows because some
     font-driver (e.g. xft) access a window while finishing a face.

     This function must be called to remove this frame's fontsets from
     Vfontset_list, and is itself responsible for not issuing X requests
     if the connection has already been terminated.  Otherwise, a future
     call to a function that iterates over all existing fontsets might
     crash, as they are not prepared to receive dead frames.
     (bug#66151) */
  free_frame_faces (f);

  /* If a display connection is dead, don't try sending more
     commands to the X server.  */
  if (dpyinfo->display)
    {
      /* Always exit with visible pointer to avoid weird issue
	 with Xfixes (Bug#17609).  */
      if (f->pointer_invisible)
	XTtoggle_invisible_pointer (f, 0);

      tear_down_x_back_buffer (f);

      if (f->output_data.x->icon_desc)
	XDestroyWindow (FRAME_X_DISPLAY (f), f->output_data.x->icon_desc);

#ifdef USE_X_TOOLKIT
      /* Explicitly destroy the scroll bars of the frame.  Without
	 this, we get "BadDrawable" errors from the toolkit later on,
	 presumably from expose events generated for the disappearing
	 toolkit scroll bars.  */
      for (bar = FRAME_SCROLL_BARS (f); !NILP (bar); bar = b->next)
	{
	  b = XSCROLL_BAR (bar);
	  x_scroll_bar_remove (b);
	}
#endif

#ifdef HAVE_X_I18N
      if (FRAME_XIC (f))
	free_frame_xic (f);
#endif

#ifdef USE_CAIRO
      x_cr_destroy_frame_context (f);
#endif
#ifdef USE_X_TOOLKIT
      if (f->output_data.x->widget)
	{
	  XtDestroyWidget (f->output_data.x->widget);
	  f->output_data.x->widget = NULL;
	}
      /* Tooltips don't have widgets, only a simple X window, even if
	 we are using a toolkit.  */
      else if (FRAME_X_WINDOW (f))
        XDestroyWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));

      free_frame_menubar (f);

      if (f->shell_position)
	xfree (f->shell_position);
#else  /* !USE_X_TOOLKIT */

#ifdef HAVE_XWIDGETS
      kill_frame_xwidget_views (f);
#endif

#ifdef USE_GTK
      xg_free_frame_widgets (f);
#endif /* USE_GTK */

      tear_down_x_back_buffer (f);
      if (FRAME_X_WINDOW (f))
	XDestroyWindow (FRAME_X_DISPLAY (f), FRAME_X_WINDOW (f));
#endif /* !USE_X_TOOLKIT */

#ifdef HAVE_XSYNC
      if (FRAME_X_BASIC_COUNTER (f) != None)
	XSyncDestroyCounter (FRAME_X_DISPLAY (f),
			     FRAME_X_BASIC_COUNTER (f));

      if (FRAME_X_EXTENDED_COUNTER (f) != None)
	XSyncDestroyCounter (FRAME_X_DISPLAY (f),
			     FRAME_X_EXTENDED_COUNTER (f));
#endif

      unload_color (f, FRAME_FOREGROUND_PIXEL (f));
      unload_color (f, FRAME_BACKGROUND_PIXEL (f));
      unload_color (f, f->output_data.x->cursor_pixel);
      unload_color (f, f->output_data.x->cursor_foreground_pixel);
      unload_color (f, f->output_data.x->border_pixel);
      unload_color (f, f->output_data.x->mouse_pixel);

      if (f->output_data.x->scroll_bar_background_pixel != -1)
	unload_color (f, f->output_data.x->scroll_bar_background_pixel);
      if (f->output_data.x->scroll_bar_foreground_pixel != -1)
	unload_color (f, f->output_data.x->scroll_bar_foreground_pixel);
#if defined (USE_LUCID) && defined (USE_TOOLKIT_SCROLL_BARS)
      /* Scrollbar shadow colors.  */
      if (f->output_data.x->scroll_bar_top_shadow_pixel != -1)
	unload_color (f, f->output_data.x->scroll_bar_top_shadow_pixel);
      if (f->output_data.x->scroll_bar_bottom_shadow_pixel != -1)
	unload_color (f, f->output_data.x->scroll_bar_bottom_shadow_pixel);
#endif /* USE_LUCID && USE_TOOLKIT_SCROLL_BARS */
      if (f->output_data.x->white_relief.pixel != -1)
	unload_color (f, f->output_data.x->white_relief.pixel);
      if (f->output_data.x->black_relief.pixel != -1)
	unload_color (f, f->output_data.x->black_relief.pixel);

      x_free_gcs (f);

      /* Free extra GCs allocated by x_setup_relief_colors.  */
      if (f->output_data.x->white_relief.gc)
	{
	  XFreeGC (dpyinfo->display, f->output_data.x->white_relief.gc);
	  f->output_data.x->white_relief.gc = 0;
	}
      if (f->output_data.x->black_relief.gc)
	{
	  XFreeGC (dpyinfo->display, f->output_data.x->black_relief.gc);
	  f->output_data.x->black_relief.gc = 0;
	}

      /* Free cursors.  */
      if (f->output_data.x->text_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->text_cursor);
      if (f->output_data.x->nontext_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->nontext_cursor);
      if (f->output_data.x->modeline_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->modeline_cursor);
      if (f->output_data.x->hand_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->hand_cursor);
      if (f->output_data.x->hourglass_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->hourglass_cursor);
      if (f->output_data.x->horizontal_drag_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->horizontal_drag_cursor);
      if (f->output_data.x->vertical_drag_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->vertical_drag_cursor);
      if (f->output_data.x->left_edge_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->left_edge_cursor);
      if (f->output_data.x->top_left_corner_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->top_left_corner_cursor);
      if (f->output_data.x->top_edge_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->top_edge_cursor);
      if (f->output_data.x->top_right_corner_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->top_right_corner_cursor);
      if (f->output_data.x->right_edge_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->right_edge_cursor);
      if (f->output_data.x->bottom_right_corner_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->bottom_right_corner_cursor);
      if (f->output_data.x->bottom_edge_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->bottom_edge_cursor);
      if (f->output_data.x->bottom_left_corner_cursor != 0)
	XFreeCursor (FRAME_X_DISPLAY (f), f->output_data.x->bottom_left_corner_cursor);

      /* Free sync fences.  */
#if defined HAVE_XSYNCTRIGGERFENCE && !defined USE_GTK && defined HAVE_CLOCK_GETTIME
      x_sync_free_fences (f);
#endif

#ifdef USE_TOOLKIT_SCROLL_BARS
      /* Since the frame was destroyed, we can no longer guarantee
	 that scroll bar events will be received.  Clear
	 protected_windows.  */
      dpyinfo->n_protected_windows = 0;
#endif
    }

#ifdef HAVE_GTK3
  if (FRAME_OUTPUT_DATA (f)->scrollbar_background_css_provider)
    g_object_unref (FRAME_OUTPUT_DATA (f)->scrollbar_background_css_provider);

  if (FRAME_OUTPUT_DATA (f)->scrollbar_foreground_css_provider)
    g_object_unref (FRAME_OUTPUT_DATA (f)->scrollbar_foreground_css_provider);
#endif

  if (f == dpyinfo->motif_drag_atom_owner)
    {
      dpyinfo->motif_drag_atom_owner = NULL;
      dpyinfo->motif_drag_atom = None;
    }

  if (f == dpyinfo->x_focus_frame)
    dpyinfo->x_focus_frame = 0;
  if (f == dpyinfo->x_focus_event_frame)
    dpyinfo->x_focus_event_frame = 0;
  if (f == dpyinfo->highlight_frame)
    dpyinfo->highlight_frame = 0;
  if (f == hlinfo->mouse_face_mouse_frame)
    reset_mouse_highlight (hlinfo);

  /* These two need to be freed now that they are used to compute the
     mouse position, I think.  */
  if (f == dpyinfo->last_mouse_motion_frame)
    dpyinfo->last_mouse_motion_frame = NULL;
  if (f == dpyinfo->last_mouse_frame)
    dpyinfo->last_mouse_frame = NULL;

#ifdef HAVE_XINPUT2
#if !defined USE_X_TOOLKIT && (!defined USE_GTK || defined HAVE_GTK3)
  /* Consider a frame being unfocused with no following FocusIn event
     while an older focus from another seat exists.  The client
     pointer should then revert to the other seat, so handle potential
     focus changes.  */

  if (dpyinfo->supports_xi2)
    xi_handle_focus_change (dpyinfo);
#endif /* !USE_X_TOOLKIT && (!USE_GTK || HAVE_GTK3) */
#endif /* HAVE_XINPUT2 */

  unblock_input ();
}


/* Destroy the X window of frame F.  */

static void
x_destroy_window (struct frame *f)
{
  struct x_display_info *dpyinfo = FRAME_DISPLAY_INFO (f);

  /* If a display connection is dead, don't try sending more
     commands to the X server.  */
  if (dpyinfo->display != 0)
    x_free_frame_resources (f);

  xfree (f->output_data.x->saved_menu_event);

#ifdef HAVE_X_I18N
  if (f->output_data.x->preedit_chars)
    xfree (f->output_data.x->preedit_chars);
#endif

#ifdef HAVE_XINPUT2
#ifdef HAVE_XINPUT2_1
  if (f->output_data.x->xi_masks)
    XFree (f->output_data.x->xi_masks);
#else
  /* This is allocated by us under very old versions of libXi; see
     `setup_xi_event_mask'.  */
  if (f->output_data.x->xi_masks)
    xfree (f->output_data.x->xi_masks);
#endif
#endif

  xfree (f->output_data.x);
  f->output_data.x = NULL;

  dpyinfo->reference_count--;
}

/* Intern NAME in DPYINFO, but check to see if the atom was already
   interned when the X connection was opened, and use that instead.

   If PREDEFINED_ONLY, return None if the atom was not interned during
   connection setup or is predefined.  */
Atom
x_intern_cached_atom (struct x_display_info *dpyinfo,
		      const char *name, bool predefined_only)
{
  int i;
  char *ptr;
  Atom *atom;

  /* Special atoms that depend on the screen number.  */
  char xsettings_atom_name[sizeof "_XSETTINGS_S%d" - 2
			   + INT_STRLEN_BOUND (int)];
  char cm_atom_name[sizeof "_NET_WM_CM_S%d" - 2
		    + INT_STRLEN_BOUND (int)];

  sprintf (xsettings_atom_name, "_XSETTINGS_S%d",
	   XScreenNumberOfScreen (dpyinfo->screen));
  sprintf (cm_atom_name, "_NET_WM_CM_S%d",
	   XScreenNumberOfScreen (dpyinfo->screen));

  if (!strcmp (name, xsettings_atom_name))
    return dpyinfo->Xatom_xsettings_sel;

  if (!strcmp (name, cm_atom_name))
    return dpyinfo->Xatom_NET_WM_CM_Sn;

  /* Now do some common predefined atoms.  */
  if (!strcmp (name, "PRIMARY"))
    return XA_PRIMARY;

  if (!strcmp (name, "SECONDARY"))
    return XA_SECONDARY;

  if (!strcmp (name, "STRING"))
    return XA_STRING;

  if (!strcmp (name, "INTEGER"))
    return XA_INTEGER;

  if (!strcmp (name, "ATOM"))
    return XA_ATOM;

  if (!strcmp (name, "WINDOW"))
    return XA_WINDOW;

  if (!strcmp (name, "DRAWABLE"))
    return XA_DRAWABLE;

  if (!strcmp (name, "BITMAP"))
    return XA_BITMAP;

  if (!strcmp (name, "CARDINAL"))
    return XA_CARDINAL;

  if (!strcmp (name, "COLORMAP"))
    return XA_COLORMAP;

  if (!strcmp (name, "CURSOR"))
    return XA_CURSOR;

  if (!strcmp (name, "FONT"))
    return XA_FONT;

  if (dpyinfo->motif_drag_atom != None
      && !strcmp (name, dpyinfo->motif_drag_atom_name))
    return dpyinfo->motif_drag_atom;

  for (i = 0; i < ARRAYELTS (x_atom_refs); ++i)
    {
      ptr = (char *) dpyinfo;

      if (!strcmp (x_atom_refs[i].name, name))
	{
	  atom = (Atom *) (ptr + x_atom_refs[i].offset);

	  return *atom;
	}
    }

  if (predefined_only)
    return None;

  return XInternAtom (dpyinfo->display, name, False);
}

/* Get the name of ATOM, but try not to make a request to the X
   server.  Whether or not a request to the X server happened is
   placed in NEED_SYNC.  */
char *
x_get_atom_name (struct x_display_info *dpyinfo, Atom atom,
		 bool *need_sync)
{
  char *dpyinfo_pointer, *name, *value, *buffer;
  int i;
  Atom ref_atom;

  dpyinfo_pointer = (char *) dpyinfo;
  value = NULL;

  if (need_sync)
    *need_sync = false;

  buffer = alloca (45 + INT_STRLEN_BOUND (int));

  switch (atom)
    {
    case XA_PRIMARY:
      return xstrdup ("PRIMARY");

    case XA_SECONDARY:
      return xstrdup ("SECONDARY");

    case XA_INTEGER:
      return xstrdup ("INTEGER");

    case XA_ATOM:
      return xstrdup ("ATOM");

    case XA_CARDINAL:
      return xstrdup ("CARDINAL");

    case XA_WINDOW:
      return xstrdup ("WINDOW");

    case XA_DRAWABLE:
      return xstrdup ("DRAWABLE");

    case XA_BITMAP:
      return xstrdup ("BITMAP");

    case XA_COLORMAP:
      return xstrdup ("COLORMAP");

    case XA_FONT:
      return xstrdup ("FONT");

    default:
      if (dpyinfo->motif_drag_atom
	  && atom == dpyinfo->motif_drag_atom)
	return xstrdup (dpyinfo->motif_drag_atom_name);

      if (atom == dpyinfo->Xatom_xsettings_sel)
	{
	  sprintf (buffer, "_XSETTINGS_S%d",
		   XScreenNumberOfScreen (dpyinfo->screen));
	  return xstrdup (buffer);
	}

      if (atom == dpyinfo->Xatom_NET_WM_CM_Sn)
	{
	  sprintf (buffer, "_NET_WM_CM_S%d",
		   XScreenNumberOfScreen (dpyinfo->screen));
	  return xstrdup (buffer);
	}

      for (i = 0; i < ARRAYELTS (x_atom_refs); ++i)
	{
	  ref_atom = *(Atom *) (dpyinfo_pointer
				+ x_atom_refs[i].offset);

	  if (atom == ref_atom)
	    return xstrdup (x_atom_refs[i].name);
	}

      name = XGetAtomName (dpyinfo->display, atom);

      if (need_sync)
	*need_sync = true;

      if (name)
	{
	  value = xstrdup (name);
	  XFree (name);
	}

      break;
    }

  return value;
}

/* Intern an array of atoms, and do so quickly, avoiding extraneous
   roundtrips to the X server.

   Avoid sending atoms that have already been found to the X server.
   This cannot do anything that will end up triggering garbage
   collection.  */

void
x_intern_atoms (struct x_display_info *dpyinfo, char **names, int count,
		Atom *atoms_return)
{
  int i, j, indices[256];
  char *new_names[256];
  Atom results[256], candidate;

  if (count > 256)
    /* Atoms array too big to inspect reasonably, just send it to the
       server and back.  */
    XInternAtoms (dpyinfo->display, new_names, count, False, atoms_return);
  else
    {
      for (i = 0, j = 0; i < count; ++i)
	{
	  candidate = x_intern_cached_atom (dpyinfo, names[i],
					    true);

	  if (candidate)
	    atoms_return[i] = candidate;
	  else
	    {
	      indices[j++] = i;
	      new_names[j - 1] = names[i];
	    }
	}

      if (!j)
	return;

      /* Now, get the results back from the X server.  */
      XInternAtoms (dpyinfo->display, new_names, j, False,
		    results);

      for (i = 0; i < j; ++i)
	atoms_return[indices[i]] = results[i];
    }
}

#ifndef USE_GTK

/* Set up XEmbed for F, and change its save set to handle the parent
   being destroyed.  */

bool
x_embed_frame (struct x_display_info *dpyinfo, struct frame *f)
{
  bool rc;

  x_catch_errors (dpyinfo->display);
  /* Catch errors; the target window might no longer exist.  */
  XReparentWindow (dpyinfo->display, FRAME_OUTER_WINDOW (f),
		   FRAME_OUTPUT_DATA (f)->parent_desc, 0, 0);
  rc = x_had_errors_p (dpyinfo->display);
  x_uncatch_errors_after_check ();

  if (rc)
    return false;

  return true;
}

#endif


/* Setting window manager hints.  */

/* Set the normal size hints for the window manager, for frame F.
   FLAGS is the flags word to use--or 0 meaning preserve the flags
   that the window now has.
   If USER_POSITION, set the USPosition
   flag (this is useful when FLAGS is 0).
   The GTK version is in gtkutils.c.  */

void
x_wm_set_size_hint (struct frame *f, long flags, bool user_position)
{
#ifndef USE_GTK
  XSizeHints size_hints;
  Window window = FRAME_OUTER_WINDOW (f);
#ifdef USE_X_TOOLKIT
  WMShellWidget shell;
#ifndef USE_MOTIF
  bool hints_changed;
#endif
#endif

  if (!window)
    return;

#ifdef USE_X_TOOLKIT
  if (f->output_data.x->widget)
    {
      /* Do this dance in xterm.c because some stuff is not as easily
	 available in widget.c.  */

      eassert (XtIsWMShell (f->output_data.x->widget));
      shell = (WMShellWidget) f->output_data.x->widget;

      if (flags)
	{
	  shell->wm.size_hints.flags &= ~(PPosition | USPosition);
	  shell->wm.size_hints.flags |= flags & (PPosition | USPosition);
	}

      if (user_position)
	{
	  shell->wm.size_hints.flags &= ~PPosition;
	  shell->wm.size_hints.flags |= USPosition;
	}

#ifndef USE_MOTIF
      hints_changed
	= widget_update_wm_size_hints (f->output_data.x->widget,
				       f->output_data.x->edit_widget);
#else
      widget_update_wm_size_hints (f->output_data.x->widget,
				   f->output_data.x->edit_widget);

      /* Do this all over again for the benefit of Motif, which always
	 knows better than the programmer.  */
      shell->wm.size_hints.flags &= ~(PPosition | USPosition);
      shell->wm.size_hints.flags |= flags & (PPosition | USPosition);

      if (user_position)
	{
	  shell->wm.size_hints.flags &= ~PPosition;
	  shell->wm.size_hints.flags |= USPosition;
	}
#endif

      /* Drill hints into Motif, since it keeps setting its own.  */
      size_hints.flags = shell->wm.size_hints.flags;
      size_hints.x = shell->wm.size_hints.x;
      size_hints.y = shell->wm.size_hints.y;
      size_hints.width = shell->wm.size_hints.width;
      size_hints.height = shell->wm.size_hints.height;
      size_hints.min_width = shell->wm.size_hints.min_width;
      size_hints.min_height = shell->wm.size_hints.min_height;
      size_hints.max_width = shell->wm.size_hints.max_width;
      size_hints.max_height = shell->wm.size_hints.max_height;
      size_hints.width_inc = shell->wm.size_hints.width_inc;
      size_hints.height_inc = shell->wm.size_hints.height_inc;
      size_hints.min_aspect.x = shell->wm.size_hints.min_aspect.x;
      size_hints.min_aspect.y = shell->wm.size_hints.min_aspect.y;
      size_hints.max_aspect.x = shell->wm.size_hints.max_aspect.x;
      size_hints.max_aspect.y = shell->wm.size_hints.max_aspect.y;
      size_hints.base_width = shell->wm.base_width;
      size_hints.base_height = shell->wm.base_height;
      size_hints.win_gravity = shell->wm.win_gravity;

#ifdef USE_MOTIF
      XSetWMNormalHints (XtDisplay (f->output_data.x->widget),
			 XtWindow (f->output_data.x->widget),
			 &size_hints);
#else
      /* In many cases, widget_update_wm_size_hints will not have
	 updated the size hints if only flags changed.  When that
	 happens, set the WM hints manually.  */

      if (!hints_changed)
	XSetWMNormalHints (XtDisplay (f->output_data.x->widget),
			   XtWindow (f->output_data.x->widget),
			   &size_hints);
#endif

      return;
    }
#endif

  /* Setting PMaxSize caused various problems.  */
  size_hints.flags = PResizeInc | PMinSize /* | PMaxSize */;

  size_hints.x = f->left_pos;
  size_hints.y = f->top_pos;

  size_hints.width = FRAME_PIXEL_WIDTH (f);
  size_hints.height = FRAME_PIXEL_HEIGHT (f);

  size_hints.width_inc = frame_resize_pixelwise ? 1 : FRAME_COLUMN_WIDTH (f);
  size_hints.height_inc = frame_resize_pixelwise ? 1 : FRAME_LINE_HEIGHT (f);

  size_hints.max_width = x_display_pixel_width (FRAME_DISPLAY_INFO (f))
    - FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, 0);
  size_hints.max_height = x_display_pixel_height (FRAME_DISPLAY_INFO (f))
    - FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, 0);

  /* Calculate the base and minimum sizes.  */
  {
    int base_width, base_height;

    base_width = FRAME_TEXT_COLS_TO_PIXEL_WIDTH (f, 0);
    base_height = FRAME_TEXT_LINES_TO_PIXEL_HEIGHT (f, 0);

    /* The window manager uses the base width hints to calculate the
       current number of rows and columns in the frame while
       resizing; min_width and min_height aren't useful for this
       purpose, since they might not give the dimensions for a
       zero-row, zero-column frame.  */

    size_hints.flags |= PBaseSize;
    size_hints.base_width = base_width;
    size_hints.base_height = base_height + FRAME_MENUBAR_HEIGHT (f);
    size_hints.min_width  = base_width;
    size_hints.min_height = base_height;
  }

  /* If we don't need the old flags, we don't need the old hint at all.  */
  if (flags)
    {
      size_hints.flags |= flags;
      goto no_read;
    }

  {
    XSizeHints hints;		/* Sometimes I hate X Windows... */
    long supplied_return;
    int value;

    value = XGetWMNormalHints (FRAME_X_DISPLAY (f), window, &hints,
			       &supplied_return);

    if (flags)
      size_hints.flags |= flags;
    else
      {
	if (value == 0)
	  hints.flags = 0;
	if (hints.flags & PSize)
	  size_hints.flags |= PSize;
	if (hints.flags & PPosition)
	  size_hints.flags |= PPosition;
	if (hints.flags & USPosition)
	  size_hints.flags |= USPosition;
	if (hints.flags & USSize)
	  size_hints.flags |= USSize;
      }
  }

 no_read:

#ifdef PWinGravity
  size_hints.win_gravity = f->win_gravity;
  size_hints.flags |= PWinGravity;

  if (user_position)
    {
      size_hints.flags &= ~ PPosition;
      size_hints.flags |= USPosition;
    }
#endif /* PWinGravity */

  XSetWMNormalHints (FRAME_X_DISPLAY (f), window, &size_hints);
#else
  xg_wm_set_size_hint (f, flags, user_position);
#endif /* USE_GTK */
}

/* Used for IconicState or NormalState */

static void
x_wm_set_window_state (struct frame *f, int state)
{
#ifdef USE_X_TOOLKIT
  Arg al[1];

  XtSetArg (al[0], XtNinitialState, state);
  XtSetValues (f->output_data.x->widget, al, 1);
#else /* not USE_X_TOOLKIT */
  Window window = FRAME_X_WINDOW (f);

  f->output_data.x->wm_hints.flags |= StateHint;
  f->output_data.x->wm_hints.initial_state = state;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);
#endif /* not USE_X_TOOLKIT */
}

static void
x_wm_set_icon_pixmap (struct frame *f, ptrdiff_t pixmap_id)
{
  Pixmap icon_pixmap, icon_mask;

#if !defined USE_X_TOOLKIT && !defined USE_GTK
  Window window = FRAME_OUTER_WINDOW (f);
#endif

  if (pixmap_id > 0)
    {
      icon_pixmap = image_bitmap_pixmap (f, pixmap_id);
      f->output_data.x->wm_hints.icon_pixmap = icon_pixmap;
      icon_mask = x_bitmap_mask (f, pixmap_id);
      f->output_data.x->wm_hints.icon_mask = icon_mask;
    }
  else
    {
      /* It seems there is no way to turn off use of an icon
	 pixmap.  */
      return;
    }


#ifdef USE_GTK
  {
    xg_set_frame_icon (f, icon_pixmap, icon_mask);
    return;
  }

#elif defined (USE_X_TOOLKIT) /* same as in x_wm_set_window_state.  */

  {
    Arg al[1];
    XtSetArg (al[0], XtNiconPixmap, icon_pixmap);
    XtSetValues (f->output_data.x->widget, al, 1);
    XtSetArg (al[0], XtNiconMask, icon_mask);
    XtSetValues (f->output_data.x->widget, al, 1);
  }

#else /* not USE_X_TOOLKIT && not USE_GTK */

  f->output_data.x->wm_hints.flags |= (IconPixmapHint | IconMaskHint);
  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);

#endif /* not USE_X_TOOLKIT && not USE_GTK */
}

void
x_wm_set_icon_position (struct frame *f, int icon_x, int icon_y)
{
  Window window = FRAME_OUTER_WINDOW (f);

  f->output_data.x->wm_hints.flags |= IconPositionHint;
  f->output_data.x->wm_hints.icon_x = icon_x;
  f->output_data.x->wm_hints.icon_y = icon_y;

  XSetWMHints (FRAME_X_DISPLAY (f), window, &f->output_data.x->wm_hints);
}


/***********************************************************************
				Fonts
 ***********************************************************************/

#ifdef GLYPH_DEBUG

/* Check that FONT is valid on frame F.  It is if it can be found in F's
   font table.  */

static void
x_check_font (struct frame *f, struct font *font)
{
  eassert (font != NULL && ! NILP (font->props[FONT_TYPE_INDEX]));
  if (font->driver->check)
    eassert (font->driver->check (f, font) == 0);
}

#endif /* GLYPH_DEBUG */


/***********************************************************************
                             Image Hooks
 ***********************************************************************/

static void
x_free_pixmap (struct frame *f, Emacs_Pixmap pixmap)
{
#ifdef USE_CAIRO
  if (pixmap)
    {
      xfree (pixmap->data);
      xfree (pixmap);
    }
#else
  XFreePixmap (FRAME_X_DISPLAY (f), pixmap);
#endif
}


/***********************************************************************
			    Initialization
 ***********************************************************************/

#ifdef USE_X_TOOLKIT
static XrmOptionDescRec emacs_options[] = {
  {(char *) "-geometry", (char *) ".geometry", XrmoptionSepArg, NULL},
  {(char *) "-iconic", (char *) ".iconic", XrmoptionNoArg, (XtPointer) "yes"},

  {(char *) "-internal-border-width",
   (char *) "*EmacsScreen.internalBorderWidth", XrmoptionSepArg, NULL},
  {(char *) "-ib", (char *) "*EmacsScreen.internalBorderWidth",
   XrmoptionSepArg, NULL},
  {(char *) "-T", (char *) "*EmacsShell.title", XrmoptionSepArg, NULL},
  {(char *) "-wn", (char *) "*EmacsShell.title", XrmoptionSepArg, NULL},
  {(char *) "-title", (char *) "*EmacsShell.title", XrmoptionSepArg, NULL},
  {(char *) "-iconname", (char *) "*EmacsShell.iconName",
   XrmoptionSepArg, NULL},
  {(char *) "-in", (char *) "*EmacsShell.iconName", XrmoptionSepArg, NULL},
  {(char *) "-mc", (char *) "*pointerColor", XrmoptionSepArg, NULL},
  {(char *) "-cr", (char *) "*cursorColor", XrmoptionSepArg, NULL}
};

/* Whether atimer for Xt timeouts is activated or not.  */

static bool x_timeout_atimer_activated_flag;

#endif /* USE_X_TOOLKIT */

static bool x_initialized;

/* Test whether two display-name strings agree up to the dot that separates
   the screen number from the server number.  */
static bool
same_x_server (const char *name1, const char *name2)
{
  bool seen_colon = false;
  Lisp_Object sysname = Fsystem_name ();
  if (! STRINGP (sysname))
    sysname = empty_unibyte_string;
  const char *system_name = SSDATA (sysname);
  ptrdiff_t system_name_length = SBYTES (sysname);
  ptrdiff_t length_until_period = 0;

  while (system_name[length_until_period] != 0
	 && system_name[length_until_period] != '.')
    length_until_period++;

  /* Treat `unix' like an empty host name.  */
  if (! strncmp (name1, "unix:", 5))
    name1 += 4;
  if (! strncmp (name2, "unix:", 5))
    name2 += 4;
  /* Treat this host's name like an empty host name.  */
  if (! strncmp (name1, system_name, system_name_length)
      && name1[system_name_length] == ':')
    name1 += system_name_length;
  if (! strncmp (name2, system_name, system_name_length)
      && name2[system_name_length] == ':')
    name2 += system_name_length;
  /* Treat this host's domainless name like an empty host name.  */
  if (! strncmp (name1, system_name, length_until_period)
      && name1[length_until_period] == ':')
    name1 += length_until_period;
  if (! strncmp (name2, system_name, length_until_period)
      && name2[length_until_period] == ':')
    name2 += length_until_period;

  for (; *name1 != '\0' && *name1 == *name2; name1++, name2++)
    {
      if (*name1 == ':')
	seen_colon = true;
      if (seen_colon && *name1 == '.')
	return true;
    }
  return (seen_colon
	  && (*name1 == '.' || *name1 == '\0')
	  && (*name2 == '.' || *name2 == '\0'));
}

/* Count number of set bits in mask and number of bits to shift to
   get to the first bit.  With MASK 0x7e0, *BITS is set to 6, and *OFFSET
   to 5.  */
static void
get_bits_and_offset (unsigned long mask, int *bits, int *offset)
{
  int nr = 0;
  int off = 0;

  while (!(mask & 1))
    {
      off++;
      mask >>= 1;
    }

  while (mask & 1)
    {
      nr++;
      mask >>= 1;
    }

  *offset = off;
  *bits = nr;
}

/* Return true iff display DISPLAY is available for use.
   But don't permanently open it, just test its availability.  */

bool
x_display_ok (const char *display)
{
  /* XOpenDisplay fails if it gets a signal.  Block SIGIO which may arrive.  */
  unrequest_sigio ();
  Display *dpy = XOpenDisplay (display);
  request_sigio ();
  if (!dpy)
    return false;
  XCloseDisplay (dpy);
  return true;
}

#ifdef USE_GTK
static void
my_log_handler (const gchar *log_domain, GLogLevelFlags log_level,
		const gchar *msg, gpointer user_data)
{
  if (!strstr (msg, "g_set_prgname"))
      fprintf (stderr, "%s-WARNING **: %s\n", log_domain, msg);
}
#endif

/* Current X display connection identifier.  Incremented for each next
   connection established.  */
static unsigned x_display_id;

#if defined HAVE_XINPUT2 && !defined HAVE_GTK3

/* Select for device change events on the root window of DPYINFO.
   These include device change and hierarchy change notifications.  */

static void
xi_select_hierarchy_events (struct x_display_info *dpyinfo)
{
  XIEventMask mask;
  ptrdiff_t l;
  unsigned char *m;

  l = XIMaskLen (XI_LASTEVENT);
  mask.mask = m = alloca (l);
  memset (m, 0, l);
  mask.mask_len = l;

  mask.deviceid = XIAllDevices;

  XISetMask (m, XI_PropertyEvent);
  XISetMask (m, XI_HierarchyChanged);
  XISetMask (m, XI_DeviceChanged);

  XISelectEvents (dpyinfo->display, dpyinfo->root_window,
		  &mask, 1);
}

#endif

#if defined HAVE_XINPUT2 && defined HAVE_GTK3

/* Look up whether or not GTK already initialized the X input
   extension.

   Value is 0 if GTK was not built with the input extension, or if it
   was explicitly disabled, 1 if GTK enabled the input extension and
   the version was successfully determined, and 2 if that information
   could not be determined.  */

static int
xi_check_toolkit (Display *display)
{
  GdkDisplay *gdpy;
  GdkDeviceManager *manager;

  gdpy = gdk_x11_lookup_xdisplay (display);
  eassume (gdpy);
  manager = gdk_display_get_device_manager (gdpy);

  if (!strcmp (G_OBJECT_TYPE_NAME (manager),
	       "GdkX11DeviceManagerXI2"))
    return 1;

  if (!strcmp (G_OBJECT_TYPE_NAME (manager),
	       "GdkX11DeviceManagerCore"))
    return 0;

  /* Something changed in GDK so this information is no longer
     available.  */

  return 2;
}

#endif

/* Open a connection to X display DISPLAY_NAME, and return the
   structure that describes the open display.  If obtaining the XCB
   connection or toolkit-specific display fails, return NULL.  Signal
   an error if opening the display itself failed.  */

struct x_display_info *
x_term_init (Lisp_Object display_name, char *xrm_option, char *resource_name)
{
  Display *dpy;
  XKeyboardState keyboard_state;
  struct terminal *terminal;
  struct x_display_info *dpyinfo;
  XrmDatabase xrdb;
  Lisp_Object tem, quit_keysym;
#ifdef USE_XCB
  xcb_connection_t *xcb_conn;
#endif
  static char const cm_atom_fmt[] = "_NET_WM_CM_S%d";
  char cm_atom_sprintf[sizeof cm_atom_fmt - 2 + INT_STRLEN_BOUND (int)];
#ifdef USE_GTK
  GdkDisplay *gdpy;
  GdkScreen *gscr;
#endif
#ifdef HAVE_XFIXES
  Lisp_Object lisp_name;
  int num_fast_selections;
  Atom selection_name;
#ifdef USE_XCB
  xcb_get_selection_owner_cookie_t *selection_cookies;
  xcb_get_selection_owner_reply_t *selection_reply;
  xcb_generic_error_t *selection_error;
#endif
#endif
  int i;

#if defined HAVE_XFIXES && defined USE_XCB
  USE_SAFE_ALLOCA;
#endif

  block_input ();

#ifdef USE_GTK
  bool was_initialized = x_initialized;
#endif /* USE_GTK */
  if (!x_initialized)
    {
      x_initialize ();
      x_initialized = true;
    }

#if defined USE_X_TOOLKIT || defined USE_GTK

  if (!x_display_ok (SSDATA (display_name)))
    error ("Display %s can't be opened", SSDATA (display_name));

#endif

#ifdef USE_GTK
  {
#define NUM_ARGV 10
    int argc;
    char *argv[NUM_ARGV];
    char **argv2 = argv;
    guint id;

    if (was_initialized)
      {
        xg_display_open (SSDATA (display_name), &dpy);
      }
    else
      {
        static char display_opt[] = "--display";
        static char name_opt[] = "--name";

        for (argc = 0; argc < NUM_ARGV; ++argc)
          argv[argc] = 0;

        argc = 0;
        argv[argc++] = initial_argv0;

        if (! NILP (display_name))
          {
            argv[argc++] = display_opt;
            argv[argc++] = SSDATA (display_name);
          }

        argv[argc++] = name_opt;
        argv[argc++] = resource_name;

        XSetLocaleModifiers ("");

        /* Work around GLib bug that outputs a faulty warning. See
           https://bugzilla.gnome.org/show_bug.cgi?id=563627.  */
        id = g_log_set_handler ("GLib", G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL
                                  | G_LOG_FLAG_RECURSION, my_log_handler, NULL);

        /* NULL window -> events for all windows go to our function.
           Call before gtk_init so Gtk+ event filters comes after our.  */
        gdk_window_add_filter (NULL, event_handler_gdk, NULL);

        gtk_disable_setlocale ();
        unrequest_sigio (); /* See comment in x_display_ok.  */
        gtk_init (&argc, &argv2);
        request_sigio ();

        g_log_remove_handler ("GLib", id);

        xg_initialize ();

	/* When Fontconfig is used, xg_initialize calls its initialization
	   function which in some versions of Fontconfig calls setlocale.  */
	fixup_locale ();

        dpy = DEFAULT_GDK_DISPLAY ();

#ifndef HAVE_GTK3
        /* Load our own gtkrc if it exists.  */
        {
          const char *file = "~/.emacs.d/gtkrc";
          Lisp_Object s, abs_file;

          s = build_string (file);
          abs_file = Fexpand_file_name (s, Qnil);

          if (! NILP (abs_file) && !NILP (Ffile_readable_p (abs_file)))
            gtk_rc_parse (SSDATA (abs_file));
        }
#endif

        XSetErrorHandler (x_error_handler);
        XSetIOErrorHandler (x_io_error_quitter);
      }
  }
#else /* not USE_GTK */
#ifdef USE_X_TOOLKIT
  /* weiner@footloose.sps.mot.com reports that this causes
     errors with X11R5:
	   X protocol error: BadAtom (invalid Atom parameter)
	   on protocol request 18skiloaf.
     So let's not use it until R6.  */
#ifdef HAVE_X11XTR6
  XtSetLanguageProc (NULL, NULL, NULL);
#endif

  {
    int argc = 0;
    char *argv[3];

    argv[0] = (char *) "";
    argc = 1;
    if (xrm_option)
      {
	argv[argc++] = (char *) "-xrm";
	argv[argc++] = xrm_option;
      }
    turn_on_atimers (false);
    unrequest_sigio ();  /* See comment in x_display_ok.  */
    dpy = XtOpenDisplay (Xt_app_con, SSDATA (display_name),
			 resource_name, EMACS_CLASS,
			 emacs_options, XtNumber (emacs_options),
			 &argc, argv);
    request_sigio ();
    turn_on_atimers (true);

#ifdef HAVE_X11XTR6
    /* I think this is to compensate for XtSetLanguageProc.  */
    fixup_locale ();
#endif
  }

#else /* not USE_X_TOOLKIT */
  XSetLocaleModifiers ("");
  unrequest_sigio ();  /* See comment in x_display_ok.  */
  dpy = XOpenDisplay (SSDATA (display_name));
  request_sigio ();
#endif /* not USE_X_TOOLKIT */
#endif /* not USE_GTK*/

  /* Detect failure.  */
  if (dpy == 0)
    {
#if !defined USE_X_TOOLKIT && !defined USE_GTK
      /* Avoid opening a display three times (once in dispextern.c
	 upon startup, once in x_display_ok, and once above) to
	 determine whether or not the display is alive on no toolkit
	 builds, where no toolkit initialization happens at all.  */

      error ("Display %s can't be opened", SSDATA (display_name));
#endif

      unblock_input ();

#if defined HAVE_XFIXES && defined USE_XCB
      SAFE_FREE ();
#endif
      return 0;
    }

#ifdef USE_XCB
  xcb_conn = XGetXCBConnection (dpy);
  if (!xcb_conn)
    {
#ifdef USE_GTK
      xg_display_close (dpy);
#else
#ifdef USE_X_TOOLKIT
      XtCloseDisplay (dpy);
#else
      XCloseDisplay (dpy);
#endif
#endif /* ! USE_GTK */

      unblock_input ();

#if defined HAVE_XFIXES && defined USE_XCB
      SAFE_FREE ();
#endif
      return 0;
    }
#endif

  /* Select for structure events on the root window, since this allows
     us to record changes to the size of the screen.  */

  XSelectInput (dpy, DefaultRootWindow (dpy), StructureNotifyMask);

  /* We have definitely succeeded.  Record the new connection.  */

  dpyinfo = xzalloc (sizeof *dpyinfo);
  terminal = x_create_terminal (dpyinfo);

  if (!NILP (Vx_detect_server_trust))
    {
      /* Detect whether or not the X server trusts this client, which
	 is done by making a SetKeyboardControl request and checking
	 for an Access error.  */
      XGrabServer (dpy);
      XGetKeyboardControl (dpy, &keyboard_state);

      x_catch_errors (dpy);

      /* At this point, the display is not on x_display_list, so
	 x_uncatch_errors won't sync.  However, that's okay because
	 x_had_errors_p will.  */

      if (keyboard_state.global_auto_repeat
	  == AutoRepeatModeOn)
	XAutoRepeatOn (dpy);
      else
	XAutoRepeatOff (dpy);

      if (x_had_errors_p (dpy))
	dpyinfo->untrusted = true;
      x_uncatch_errors_after_check ();
      XUngrabServer (dpy);
    }

  dpyinfo->next_failable_request = dpyinfo->failable_requests;

  {
    struct x_display_info *share;

    for (share = x_display_list; share; share = share->next)
      if (same_x_server (SSDATA (XCAR (share->name_list_element)),
			 SSDATA (display_name)))
	break;
    if (share)
      terminal->kboard = share->terminal->kboard;
    else
      {
	terminal->kboard = allocate_kboard (Qx);

	if (!BASE_EQ (XSYMBOL (Qvendor_specific_keysyms)->u.s.function,
		      Qunbound))
	  {
	    char *vendor = ServerVendor (dpy);

	    /* Temporarily hide the partially initialized terminal.
	       Use safe_call so that if a signal happens, a partially
	       initialized display (and display connection) is not
	       kept around.  */
	    terminal_list = terminal->next_terminal;
	    unblock_input ();
	    kset_system_key_alist (terminal->kboard,
				   safe_calln (Qvendor_specific_keysyms,
					       (vendor
						? build_string (vendor)
						: empty_unibyte_string)));
	    block_input ();
	    terminal->next_terminal = terminal_list;
	    terminal_list = terminal;
	  }

	/* Don't let the initial kboard remain current longer than necessary.
	   That would cause problems if a file loaded on startup tries to
	   prompt in the mini-buffer.  */
	if (current_kboard == initial_kboard)
	  current_kboard = terminal->kboard;
      }
    terminal->kboard->reference_count++;
  }

  /* Now look through Vx_quit_keysym for the quit keysym associated
     with this display.  */
  tem = Vx_quit_keysym;
  FOR_EACH_TAIL_SAFE (tem)
    {
      quit_keysym = XCAR (tem);

      /* Check if its car is a string and its cdr a valid keysym.
	 Skip if it is not.  */

      if (!CONSP (quit_keysym) || !FIXNUMP (XCDR (quit_keysym))
	  || !STRINGP (XCAR (quit_keysym)))
	continue;

      /* Check if this is the keysym to be used.  */

      if (strcmp (SSDATA (XCAR (quit_keysym)), ServerVendor (dpy)))
	continue;

      dpyinfo->quit_keysym = XFIXNUM (XCDR (quit_keysym));
    }

  /* Put this display on the chain.  */
  dpyinfo->next = x_display_list;
  x_display_list = dpyinfo;

  dpyinfo->name_list_element = Fcons (display_name, Qnil);
  dpyinfo->display = dpy;
  dpyinfo->connection = ConnectionNumber (dpyinfo->display);
#ifdef USE_XCB
  dpyinfo->xcb_connection = xcb_conn;
#endif

  /* https://lists.gnu.org/r/emacs-devel/2015-11/msg00194.html  */
  dpyinfo->smallest_font_height = 1;
  dpyinfo->smallest_char_width = 1;

  dpyinfo->color_names_size = 256;
  dpyinfo->color_names = xzalloc (dpyinfo->color_names_size
				  * sizeof *dpyinfo->color_names);
  dpyinfo->color_names_length = xzalloc (dpyinfo->color_names_size
					 * sizeof *dpyinfo->color_names_length);

  /* Set the name of the terminal. */
  terminal->name = xlispstrdup (display_name);

#if false
  XSetAfterFunction (dpyinfo->display, x_trace_wire);
#endif

  Lisp_Object system_name = Fsystem_name ();
  static char const title[] = "GNU Emacs";
  if (STRINGP (system_name))
    {
      static char const at[] = " at ";
      ptrdiff_t nbytes = sizeof (title) + sizeof (at);
      if (ckd_add (&nbytes, nbytes, SBYTES (system_name)))
	memory_full (SIZE_MAX);
      dpyinfo->x_id_name = xmalloc (nbytes);
      sprintf (dpyinfo->x_id_name, "%s%s%s", title, at, SDATA (system_name));
    }
  else
    {
      dpyinfo->x_id_name = xmalloc (sizeof (title));
      strcpy (dpyinfo->x_id_name, title);
    }

  dpyinfo->x_id = ++x_display_id;

#ifndef HAVE_XKB
  /* Figure out which modifier bits mean what.  */
  x_find_modifier_meanings (dpyinfo);
#endif

  /* Get the scroll bar cursor.  */
#ifdef USE_GTK
  /* We must create a GTK cursor, it is required for GTK widgets.  */
  dpyinfo->xg_cursor = xg_create_default_cursor (dpyinfo->display);
#endif /* USE_GTK */

  dpyinfo->vertical_scroll_bar_cursor
    = XCreateFontCursor (dpyinfo->display, XC_sb_v_double_arrow);

  dpyinfo->horizontal_scroll_bar_cursor
    = XCreateFontCursor (dpyinfo->display, XC_sb_h_double_arrow);

  xrdb = x_load_resources (dpyinfo->display, xrm_option,
			   resource_name, EMACS_CLASS);
#ifdef HAVE_XRMSETDATABASE
  XrmSetDatabase (dpyinfo->display, xrdb);
#else
  dpyinfo->display->db = xrdb;
#endif

#ifdef HAVE_XRENDER
  int event_base, error_base;
  dpyinfo->xrender_supported_p
    = XRenderQueryExtension (dpyinfo->display, &event_base, &error_base);

  if (dpyinfo->xrender_supported_p)
    dpyinfo->xrender_supported_p
      = XRenderQueryVersion (dpyinfo->display, &dpyinfo->xrender_major,
			     &dpyinfo->xrender_minor);
#endif

  /* This must come after XRenderQueryVersion! */
#ifdef HAVE_XCOMPOSITE
  int composite_event_base, composite_error_base;
  dpyinfo->composite_supported_p = XCompositeQueryExtension (dpyinfo->display,
							     &composite_event_base,
							     &composite_error_base);

  if (dpyinfo->composite_supported_p)
    dpyinfo->composite_supported_p
      = XCompositeQueryVersion (dpyinfo->display,
				&dpyinfo->composite_major,
				&dpyinfo->composite_minor);
#endif

#ifdef HAVE_XSHAPE
  dpyinfo->xshape_supported_p
    = XShapeQueryExtension (dpyinfo->display,
			    &dpyinfo->xshape_event_base,
			    &dpyinfo->xshape_error_base);

  if (dpyinfo->xshape_supported_p)
    dpyinfo->xshape_supported_p
      = XShapeQueryVersion (dpyinfo->display,
			    &dpyinfo->xshape_major,
			    &dpyinfo->xshape_minor);
#endif

  /* Put the rdb where we can find it in a way that works on
     all versions.  */
  dpyinfo->rdb = xrdb;

  dpyinfo->screen = ScreenOfDisplay (dpyinfo->display,
				     DefaultScreen (dpyinfo->display));
  select_visual (dpyinfo);
  dpyinfo->cmap = DefaultColormapOfScreen (dpyinfo->screen);
  dpyinfo->root_window = RootWindowOfScreen (dpyinfo->screen);
  dpyinfo->icon_bitmap_id = -1;
  dpyinfo->wm_type = X_WMTYPE_UNKNOWN;

  reset_mouse_highlight (&dpyinfo->mouse_highlight);

#ifdef HAVE_XRENDER
  if (dpyinfo->xrender_supported_p
      /* This could already have been initialized by
	 `select_visual'.  */
      && !dpyinfo->pict_format)
    dpyinfo->pict_format = XRenderFindVisualFormat (dpyinfo->display,
						    dpyinfo->visual);
#endif

#ifdef HAVE_XSYNC
  int xsync_event_base, xsync_error_base;
  dpyinfo->xsync_supported_p
    = XSyncQueryExtension (dpyinfo->display,
			   &xsync_event_base,
			   &xsync_error_base);

  if (dpyinfo->xsync_supported_p)
    dpyinfo->xsync_supported_p = XSyncInitialize (dpyinfo->display,
						  &dpyinfo->xsync_major,
						  &dpyinfo->xsync_minor);

  {
    AUTO_STRING (synchronizeResize, "synchronizeResize");
    AUTO_STRING (SynchronizeResize, "SynchronizeResize");

    Lisp_Object value = gui_display_get_resource (dpyinfo,
						  synchronizeResize,
						  SynchronizeResize,
						  Qnil, Qnil);

    if (STRINGP (value)
	&& (!strcmp (SSDATA (value), "false")
	    || !strcmp (SSDATA (value), "off")))
      dpyinfo->xsync_supported_p = false;
  }
#endif

#ifdef HAVE_XINERAMA
  int xin_event_base, xin_error_base;
  dpyinfo->xinerama_supported_p
    = XineramaQueryExtension (dpy, &xin_event_base, &xin_error_base);
#endif

  /* See if a private colormap is requested.  */
  if (dpyinfo->visual == DefaultVisualOfScreen (dpyinfo->screen))
    {
      if (dpyinfo->visual_info.class == PseudoColor)
	{
	  AUTO_STRING (privateColormap, "privateColormap");
	  AUTO_STRING (PrivateColormap, "PrivateColormap");
	  Lisp_Object value
	    = gui_display_get_resource (dpyinfo, privateColormap,
                                        PrivateColormap, Qnil, Qnil);
	  if (STRINGP (value)
	      && (!strcmp (SSDATA (value), "true")
		  || !strcmp (SSDATA (value), "on")))
	    dpyinfo->cmap = XCopyColormapAndFree (dpyinfo->display, dpyinfo->cmap);
	}
    }
  else
    dpyinfo->cmap = XCreateColormap (dpyinfo->display, dpyinfo->root_window,
                                     dpyinfo->visual, AllocNone);

  /* See if we can construct pixel values from RGB values.  */
  if (dpyinfo->visual_info.class == TrueColor)
    {
      get_bits_and_offset (dpyinfo->visual_info.red_mask,
                           &dpyinfo->red_bits, &dpyinfo->red_offset);
      get_bits_and_offset (dpyinfo->visual_info.blue_mask,
                           &dpyinfo->blue_bits, &dpyinfo->blue_offset);
      get_bits_and_offset (dpyinfo->visual_info.green_mask,
                           &dpyinfo->green_bits, &dpyinfo->green_offset);

#ifdef HAVE_XRENDER
      if (dpyinfo->pict_format)
	{
	  unsigned long channel_mask
	    = ((unsigned long) dpyinfo->pict_format->direct.alphaMask
	       << dpyinfo->pict_format->direct.alpha);

	  if (channel_mask)
	    get_bits_and_offset (channel_mask, &dpyinfo->alpha_bits,
				 &dpyinfo->alpha_offset);
	  dpyinfo->alpha_mask = channel_mask;
	}
      else
#endif
	{
	  XColor xc;
	  unsigned long alpha_mask;
	  xc.red = 65535;
	  xc.green = 65535;
	  xc.blue = 65535;

	  if (XAllocColor (dpyinfo->display,
			   dpyinfo->cmap, &xc) != 0)
	    {
	      alpha_mask = xc.pixel & ~(dpyinfo->visual_info.red_mask
					| dpyinfo->visual_info.blue_mask
					| dpyinfo->visual_info.green_mask);

	      if (alpha_mask)
		get_bits_and_offset (alpha_mask, &dpyinfo->alpha_bits,
				     &dpyinfo->alpha_offset);
	      dpyinfo->alpha_mask = alpha_mask;
	    }
	}
    }

#ifdef HAVE_XDBE
  dpyinfo->supports_xdbe = false;
  int xdbe_major;
  int xdbe_minor;
  if (XdbeQueryExtension (dpyinfo->display, &xdbe_major, &xdbe_minor))
    dpyinfo->supports_xdbe = true;
#endif

#ifdef USE_XCB
  xcb_screen_t *xcb_screen = NULL;
  xcb_screen_iterator_t iter;
  xcb_visualid_t wanted = { XVisualIDFromVisual (dpyinfo->visual) };
  xcb_depth_iterator_t depth_iter;
  xcb_visualtype_iterator_t visual_iter;

  int screen = DefaultScreen (dpyinfo->display);

  iter = xcb_setup_roots_iterator (xcb_get_setup (dpyinfo->xcb_connection));
  for (; iter.rem; --screen, xcb_screen_next (&iter))
    {
      if (!screen)
	xcb_screen = iter.data;
    }

  if (xcb_screen)
    {
      depth_iter = xcb_screen_allowed_depths_iterator (xcb_screen);
      for (; depth_iter.rem; xcb_depth_next (&depth_iter))
	{
	  visual_iter = xcb_depth_visuals_iterator (depth_iter.data);
	  for (; visual_iter.rem; xcb_visualtype_next (&visual_iter))
	    {
	      if (wanted == visual_iter.data->visual_id)
		{
		  dpyinfo->xcb_visual = visual_iter.data;
		  break;
		}
	    }
	}
    }
#endif

#ifdef HAVE_XINPUT2
  dpyinfo->supports_xi2 = false;
  int rc;
  int major = 2;
  int xi_first_event, xi_first_error;

#ifndef HAVE_GTK3
  {
    AUTO_STRING (disableInputExtension, "disableInputExtension");
    AUTO_STRING (DisableInputExtension, "DisableInputExtension");

    Lisp_Object value = gui_display_get_resource (dpyinfo,
						  disableInputExtension,
						  DisableInputExtension,
						  Qnil, Qnil);

    if (STRINGP (value)
	&& (!strcmp (SSDATA (value), "on")
	    || !strcmp (SSDATA (value), "true")))
      goto skip_xi_setup;
  }
#endif

#ifdef HAVE_XINPUT2_4
  int minor = 4;
#elif defined HAVE_XINPUT2_3 /* XInput 2.3 */
  int minor = 3;
#elif defined HAVE_XINPUT2_2 /* XInput 2.2 */
  int minor = 2;
#elif defined HAVE_XINPUT2_1 /* XInput 2.1 */
  int minor = 1;
#else /* Some old version of XI2 we're not interested in. */
  int minor = 0;
#endif

  dpyinfo->client_pointer_device = -1;

#ifdef HAVE_GTK3
  /* GTK gets a chance to request use of the input extension first.
     If we later try to enable it if GDK did not, then GTK will not
     get the resulting extension events.  */

  rc = xi_check_toolkit (dpyinfo->display);

  if (!rc)
    goto skip_xi_setup;
#endif

  if (XQueryExtension (dpyinfo->display, "XInputExtension",
		       &dpyinfo->xi2_opcode, &xi_first_event,
		       &xi_first_error))
    {
#ifdef HAVE_GTK3
      bool move_backwards = false;
      int original_minor = minor;

    query:

      /* Catch errors caused by GTK requesting a different version of
	 XInput 2 than what Emacs was built with.  Usually, the X
	 server tolerates these mistakes, but a BadValue error can
	 result if only one of GTK or Emacs wasn't built with support
	 for XInput 2.2.

	 To work around the first, it suffices to increase the minor
	 version until the X server is happy if the XIQueryVersion
	 request results in an error.  If that doesn't work, however,
	 then it's the latter, so decrease the minor until the version
	 that GTK requested is found.  */
#endif

      x_catch_errors (dpyinfo->display);

      rc = XIQueryVersion (dpyinfo->display, &major, &minor);

#ifdef HAVE_GTK3
      /* Increase the minor version until we find one the X
	 server agrees with.  If that didn't work, then
	 decrease the version until it either hits zero or
	 becomes agreeable to the X server.  */

      if (x_had_errors_p (dpyinfo->display))
	{
	  x_uncatch_errors_after_check ();

	  /* Since BadValue errors can't be generated if both the
	     prior and current requests specify a version of 2.2 or
	     later, this means the prior request specified a version
	     of the input extension less than 2.2.  */
	  if (minor >= 2)
	    {
	      move_backwards = true;
	      minor = original_minor;

	      if (--minor < 0)
		rc = BadRequest;
	      else
		goto query;
	    }
	  else
	    {
	      if (!move_backwards)
		{
		  minor++;
		  goto query;
		}

	      if (--minor < 0)
		rc = BadRequest;
	      else
		goto query;

	    }
	}
      else
	x_uncatch_errors_after_check ();

      /* But don't delude ourselves into thinking that we can use
	 features provided by a version of the input extension that
	 libXi itself doesn't support.  */

      if (minor > original_minor)
	minor = original_minor;
#else
      if (x_had_errors_p (dpyinfo->display))
	rc = BadRequest;

      x_uncatch_errors_after_check ();
#endif

      if (rc == Success)
	{
	  dpyinfo->supports_xi2 = true;
#ifndef HAVE_GTK3
	  /* Select for hierarchy events on the root window.  GTK 3.x
	     does this itself.  */
	  xi_select_hierarchy_events (dpyinfo);
#endif

	  dpyinfo->xi2_version = minor;
	  x_cache_xi_devices (dpyinfo);
	}
    }
 skip_xi_setup:
  ;
#endif

#if defined HAVE_XRANDR || defined USE_GTK
  Lisp_Object term;

  XSETTERMINAL (term, terminal);
#endif

#ifdef HAVE_XRANDR
  dpyinfo->xrandr_supported_p
    = XRRQueryExtension (dpy, &dpyinfo->xrandr_event_base,
			 &dpyinfo->xrandr_error_base);

#ifndef USE_GTK
  dpyinfo->last_monitor_attributes_list = Qnil;
#endif

  if (dpyinfo->xrandr_supported_p)
    {
      XRRQueryVersion (dpy, &dpyinfo->xrandr_major_version,
		       &dpyinfo->xrandr_minor_version);

#ifndef USE_GTK
      if (dpyinfo->xrandr_major_version == 1
	  && dpyinfo->xrandr_minor_version >= 2)
	{
	  XRRSelectInput (dpyinfo->display,
			  dpyinfo->root_window,
			  (RRScreenChangeNotifyMask
			   | RRCrtcChangeNotifyMask
			   | RROutputChangeNotifyMask
#ifdef USE_GTK
			   /* Emacs doesn't actually need this, but
			      GTK selects for it when the display is
			      initialized.  */
			   | RROutputPropertyNotifyMask
#endif
			   ));

	  dpyinfo->last_monitor_attributes_list
	    = Fx_display_monitor_attributes_list (term);
	}
#endif
    }
#endif

#ifdef USE_GTK
  dpyinfo->last_monitor_attributes_list
    = Fx_display_monitor_attributes_list (term);

  gdpy = gdk_x11_lookup_xdisplay (dpyinfo->display);
  gscr = gdk_display_get_default_screen (gdpy);

  g_signal_connect (G_OBJECT (gscr), "monitors-changed",
		    G_CALLBACK (x_monitors_changed_cb),
		    NULL);
#endif

#ifdef HAVE_XKB
  int xkb_major, xkb_minor, xkb_op, xkb_error_code;
  xkb_major = XkbMajorVersion;
  xkb_minor = XkbMinorVersion;

  if (XkbLibraryVersion (&xkb_major, &xkb_minor)
      && XkbQueryExtension (dpyinfo->display, &xkb_op, &dpyinfo->xkb_event_type,
			    &xkb_error_code, &xkb_major, &xkb_minor))
    {
      dpyinfo->supports_xkb = true;
      dpyinfo->xkb_desc = XkbGetMap (dpyinfo->display,
				     (XkbKeySymsMask
				      | XkbKeyTypesMask
				      | XkbModifierMapMask
				      | XkbVirtualModsMask),
				     XkbUseCoreKbd);

      if (dpyinfo->xkb_desc)
	XkbGetNames (dpyinfo->display, XkbAllNamesMask,
		     dpyinfo->xkb_desc);

      XkbSelectEvents (dpyinfo->display, XkbUseCoreKbd,
		       XkbNewKeyboardNotifyMask | XkbMapNotifyMask,
		       XkbNewKeyboardNotifyMask | XkbMapNotifyMask);
    }

  /* XFree86 extends XKBlib with a new Xlib control `ControlFallback',
     which enables a search for symbols designating ASCII characters
     within inactive groups during keycode translation when
     ControlMask is set.  Users find this behavior gratuitous, so
     disable it if present.  */

#ifdef XkbLC_ControlFallback
  XkbSetXlibControls (dpyinfo->display, XkbLC_ControlFallback, 0);
#endif /* XkbLC_ControlFallback */
#endif /* HAVE_XKB */

#ifdef HAVE_XFIXES
  int xfixes_error_base;
  dpyinfo->xfixes_supported_p
    = XFixesQueryExtension (dpyinfo->display,
			    &dpyinfo->xfixes_event_base,
			    &xfixes_error_base);

  if (dpyinfo->xfixes_supported_p)
    {
      if (!XFixesQueryVersion (dpyinfo->display, &dpyinfo->xfixes_major,
			       &dpyinfo->xfixes_minor))
	dpyinfo->xfixes_supported_p = false;
    }
#endif

#if defined USE_CAIRO || defined HAVE_XFT
  {
    /* If we are using Xft, the following precautions should be made:

       1. Make sure that the Xrender extension is added before the Xft one.
       Otherwise, the close-display hook set by Xft is called after the one
       for Xrender, and the former tries to re-add the latter.  This results
       in inconsistency of internal states and leads to X protocol error when
       one reconnects to the same X server (Bug#1696).

       2. Check dpi value in X resources.  It is better we use it as well,
       since Xft will use it, as will all Gnome applications.  If our real DPI
       is smaller or larger than the one Xft uses, our font will look smaller
       or larger than other for other applications, even if it is the same
       font name (monospace-10 for example).  */

    char *v = XGetDefault (dpyinfo->display, "Xft", "dpi");
    double d;
    if (v != NULL && sscanf (v, "%lf", &d) == 1)
      dpyinfo->resy = dpyinfo->resx = d;
  }
#endif

  if (dpyinfo->resy < 1)
    {
      int screen_number = XScreenNumberOfScreen (dpyinfo->screen);
      double pixels = DisplayHeight (dpyinfo->display, screen_number);
      double mm = DisplayHeightMM (dpyinfo->display, screen_number);
      /* Mac OS X 10.3's Xserver sometimes reports 0.0mm.  */
      dpyinfo->resy = (mm < 1) ? 100 : pixels * 25.4 / mm;
      pixels = DisplayWidth (dpyinfo->display, screen_number);
      mm = DisplayWidthMM (dpyinfo->display, screen_number);
      /* Mac OS X 10.3's Xserver sometimes reports 0.0mm.  */
      dpyinfo->resx = (mm < 1) ? 100 : pixels * 25.4 / mm;
    }

  sprintf (cm_atom_sprintf, cm_atom_fmt,
	   XScreenNumberOfScreen (dpyinfo->screen));

  {
    enum { atom_count = ARRAYELTS (x_atom_refs) };
    /* 1 for _XSETTINGS_SN.  */
    enum { total_atom_count = 2 + atom_count };
    Atom atoms_return[total_atom_count];
    char *atom_names[total_atom_count];
    static char const xsettings_fmt[] = "_XSETTINGS_S%d";
    char xsettings_atom_name[sizeof xsettings_fmt - 2
			     + INT_STRLEN_BOUND (int)];

    for (i = 0; i < atom_count; i++)
      atom_names[i] = (char *) x_atom_refs[i].name;

    /* Build _XSETTINGS_SN atom name.  */
    sprintf (xsettings_atom_name, xsettings_fmt,
	     XScreenNumberOfScreen (dpyinfo->screen));
    atom_names[i] = xsettings_atom_name;
    atom_names[i + 1] = cm_atom_sprintf;

    XInternAtoms (dpyinfo->display, atom_names, total_atom_count,
                  False, atoms_return);

    for (i = 0; i < atom_count; i++)
      *(Atom *) ((char *) dpyinfo + x_atom_refs[i].offset) = atoms_return[i];

    /* Manually copy last two atoms.  */
    dpyinfo->Xatom_xsettings_sel = atoms_return[i];
    dpyinfo->Xatom_NET_WM_CM_Sn = atoms_return[i + 1];
  }

#ifdef HAVE_XKB
  /* Figure out which modifier bits mean what.  */
  x_find_modifier_meanings (dpyinfo);
#endif

  dpyinfo->x_dnd_atoms_size = 16;
  dpyinfo->x_dnd_atoms = xmalloc (sizeof *dpyinfo->x_dnd_atoms
                                  * dpyinfo->x_dnd_atoms_size);
  dpyinfo->gray
    = XCreatePixmapFromBitmapData (dpyinfo->display, dpyinfo->root_window,
				   gray_bits, gray_width, gray_height,
				   1, 0, 1);

  dpyinfo->invisible_cursor = make_invisible_cursor (dpyinfo);
#if defined HAVE_XFIXES && XFIXES_VERSION >= 40000
  dpyinfo->fixes_pointer_blanking = (egetenv ("EMACS_XFIXES") != NULL);
#endif

  xsettings_initialize (dpyinfo);

  /* This is only needed for distinguishing keyboard and process input.  */
  if (dpyinfo->connection != 0)
    add_keyboard_wait_descriptor (dpyinfo->connection);

#ifdef F_SETOWN
  fcntl (dpyinfo->connection, F_SETOWN, getpid ());
#endif /* ! defined (F_SETOWN) */

  if (interrupt_input)
    init_sigio (dpyinfo->connection);

#ifdef USE_LUCID
  {
    XrmValue d, fr, to;
    Font font;
    XFontStruct *query_result;

    dpy = dpyinfo->display;
    d.addr = (XPointer) &dpy;
    d.size = sizeof (Display *);
    fr.addr = (char *) XtDefaultFont;
    fr.size = sizeof (XtDefaultFont);
    to.size = sizeof (Font *);
    to.addr = (XPointer) &font;
    x_catch_errors (dpy);
    if (!XtCallConverter (dpy, XtCvtStringToFont, &d, 1, &fr, &to, NULL))
      emacs_abort ();
    query_result = XQueryFont (dpy, font);

    /* Set the dialog font to some fallback (here, 9x15) if the font
       specified is invalid.  */
    if (x_had_errors_p (dpy) || !font)
      XrmPutLineResource (&xrdb, "Emacs.dialog.*.font: 9x15");

    /* Do not destroy the font struct returned above with XFreeFont;
       that also destroys the font, leading to X protocol errors at
       XtCloseDisplay.  Just free the font info structure.
       (Bug#18403) */
    XFreeFontInfo (NULL, query_result, 1);
    x_uncatch_errors ();
  }
#endif

  /* See if we should run in synchronous mode.  This is useful
     for debugging X code.  */
  {
    AUTO_STRING (synchronous, "synchronous");
    AUTO_STRING (Synchronous, "Synchronous");
    Lisp_Object value = gui_display_get_resource (dpyinfo, synchronous,
                                                  Synchronous, Qnil, Qnil);
    if (STRINGP (value)
	&& (!strcmp (SSDATA (value), "true")
	    || !strcmp (SSDATA (value), "on")))
      XSynchronize (dpyinfo->display, True);
  }

#ifdef HAVE_X_I18N
  {
    AUTO_STRING (useXIM, "useXIM");
    AUTO_STRING (UseXIM, "UseXIM");
    Lisp_Object value = gui_display_get_resource (dpyinfo, useXIM, UseXIM,
                                                  Qnil, Qnil);

    /* `USE_XIM' controls whether Emacs should use X input methods by
       default, not whether or not XIM is available.  */

#ifdef USE_XIM
    dpyinfo->use_xim = true;

    if (STRINGP (value)
	&& (!strcmp (SSDATA (value), "false")
	    || !strcmp (SSDATA (value), "off")))
      dpyinfo->use_xim = false;
#else /* !USE_XIM */
    dpyinfo->use_xim = false;

    if (STRINGP (value)
	&& (!strcmp (SSDATA (value), "true")
	    || !strcmp (SSDATA (value), "on")))
      dpyinfo->use_xim = true;
#endif /* USE_XIM */
  }

  {
    AUTO_STRING (inputStyle, "inputStyle");
    AUTO_STRING (InputStyle, "InputStyle");
    Lisp_Object value = gui_display_get_resource (dpyinfo, inputStyle, InputStyle,
						  Qnil, Qnil);

    if (STRINGP (value))
      {
	if (!strcmp (SSDATA (value), "callback"))
	  dpyinfo->preferred_xim_style = STYLE_CALLBACK;
	else if (!strcmp (SSDATA (value), "none"))
	  dpyinfo->preferred_xim_style = STYLE_NONE;
	else if (!strcmp (SSDATA (value), "overthespot"))
	  dpyinfo->preferred_xim_style = STYLE_OVERTHESPOT;
	else if (!strcmp (SSDATA (value), "offthespot"))
	  dpyinfo->preferred_xim_style = STYLE_OFFTHESPOT;
	else if (!strcmp (SSDATA (value), "root"))
	  dpyinfo->preferred_xim_style = STYLE_ROOT;
#ifdef USE_GTK
	else if (!strcmp (SSDATA (value), "native"))
	  dpyinfo->prefer_native_input = true;
#endif /* HAVE_GTK */
      }
  }

  /* Now that defaults have been set up, initialize input method
     support.  */

  /* Avoid initializing input methods if the X library does not
     support Emacs's locale.  When the current locale is not
     supported, decoding input method strings becomes undefined.  */
  if (XSupportsLocale ())
    xim_initialize (dpyinfo, resource_name);
#endif /* HAVE_X_I18N */

#ifdef HAVE_X_SM
  /* Only do this for the very first display in the Emacs session.
     Ignore X session management when Emacs was first started on a
     tty or started as a daemon.  */
  if (!dpyinfo->next && ! IS_DAEMON)
    x_session_initialize (dpyinfo);
#endif

#if defined USE_CAIRO || defined HAVE_XRENDER
  x_extension_initialize (dpyinfo);
#endif

#ifdef USE_TOOLKIT_SCROLL_BARS
  dpyinfo->protected_windows = xmalloc (sizeof (Lisp_Object) * 256);
  dpyinfo->n_protected_windows = 0;
  dpyinfo->protected_windows_max = 256;
#endif

#ifdef HAVE_XFIXES
  /* Initialize selection tracking for the selections in
     x-fast-selection-list.  */

  if (CONSP (Vx_fast_selection_list)
      && dpyinfo->xfixes_supported_p
      && dpyinfo->xfixes_major >= 1)
    {
      num_fast_selections = 0;
      tem = Vx_fast_selection_list;

      FOR_EACH_TAIL_SAFE (tem)
	{
	  if (!SYMBOLP (XCAR (tem)))
	    continue;

	  num_fast_selections++;
	}

      dpyinfo->n_monitored_selections = num_fast_selections;
      dpyinfo->selection_tracking_window
	= x_create_special_window (dpyinfo, dpyinfo->root_window);
      dpyinfo->monitored_selections
	= xmalloc (num_fast_selections
		   * sizeof *dpyinfo->monitored_selections);

      num_fast_selections = 0;
      tem = Vx_fast_selection_list;

      FOR_EACH_TAIL_SAFE (tem)
	{
	  lisp_name = XCAR (tem);

	  if (!SYMBOLP (lisp_name))
	    continue;

	  selection_name = symbol_to_x_atom (dpyinfo, lisp_name);
	  dpyinfo->monitored_selections[num_fast_selections++].name
	    = selection_name;
	  dpyinfo->monitored_selections[num_fast_selections - 1].owner
	    = X_INVALID_WINDOW;

	  /* Select for selection input.  */
	  XFixesSelectSelectionInput (dpyinfo->display,
				      dpyinfo->selection_tracking_window,
				      selection_name,
				      (XFixesSetSelectionOwnerNotifyMask
				       | XFixesSelectionClientCloseNotifyMask));
	}

#ifdef USE_XCB
      SAFE_NALLOCA (selection_cookies, 1, num_fast_selections);
#endif

      /* Now, ask for the current owners of all those selections.  */
      for (i = 0; i < num_fast_selections; ++i)
	{
#ifdef USE_XCB
	  selection_cookies[i]
	    = xcb_get_selection_owner (dpyinfo->xcb_connection,
				       dpyinfo->monitored_selections[i].name);
#else
	  dpyinfo->monitored_selections[i].owner
	    = XGetSelectionOwner (dpyinfo->display,
				  dpyinfo->monitored_selections[i].name);
#endif
	}

#ifdef USE_XCB
      for (i = 0; i < num_fast_selections; ++i)
	{
	  selection_reply
	    = xcb_get_selection_owner_reply (dpyinfo->xcb_connection,
					     selection_cookies[i],
					     &selection_error);

	  if (selection_reply)
	    {
	      dpyinfo->monitored_selections[i].owner
		= selection_reply->owner;
	      free (selection_reply);
	    }
	  else if (selection_error)
	    free (selection_error);
	}
#endif
    }
#endif

  unblock_input ();

#if defined HAVE_XFIXES && defined USE_XCB
  SAFE_FREE ();
#endif
  return dpyinfo;
}



/* Remove all the selection input events on the keyboard buffer
   intended for DPYINFO.  */

static void
x_delete_selection_requests (struct x_display_info *dpyinfo)
{
  union buffered_input_event *event;
  int moved_events;

  for (event = kbd_fetch_ptr; event != kbd_store_ptr;
       event = X_NEXT_KBD_EVENT (event))
    {
      if (event->kind == SELECTION_REQUEST_EVENT
	  || event->kind == SELECTION_CLEAR_EVENT)
	{
	  if (SELECTION_EVENT_DPYINFO (&event->sie) != dpyinfo)
	    continue;

	  /* Remove the event from the fifo buffer before processing;
	     otherwise swallow_events called recursively could see it
	     and process it again.  To do this, we move the events
	     between kbd_fetch_ptr and EVENT one slot to the right,
	     cyclically.  */

	  if (event < kbd_fetch_ptr)
	    {
	      memmove (kbd_buffer + 1, kbd_buffer,
		       (event - kbd_buffer) * sizeof *kbd_buffer);
	      kbd_buffer[0] = kbd_buffer[KBD_BUFFER_SIZE - 1];
	      moved_events = kbd_buffer + KBD_BUFFER_SIZE - 1 - kbd_fetch_ptr;
	    }
	  else
	    moved_events = event - kbd_fetch_ptr;

	  memmove (kbd_fetch_ptr + 1, kbd_fetch_ptr,
		   moved_events * sizeof *kbd_fetch_ptr);
	  kbd_fetch_ptr = X_NEXT_KBD_EVENT (kbd_fetch_ptr);

	  /* `detect_input_pending' will then recompute whether or not
	     pending input events exist.  */
	  input_pending = false;
	}
    }
}

/* Get rid of display DPYINFO, deleting all frames on it,
   and without sending any more commands to the X server.  */

static void
x_delete_display (struct x_display_info *dpyinfo)
{
  struct terminal *t;
  struct color_name_cache_entry *color_entry, *next_color_entry;
  int i;
  struct x_selection_request_event *ie, *last, *temp;

  /* Close all frames and delete the generic struct terminal for this
     X display.  */
  for (t = terminal_list; t; t = t->next_terminal)
    if (t->type == output_x_window && t->display_info.x == dpyinfo)
      {
#ifdef HAVE_X_SM
        /* Close X session management when we close its display.  */
        if (t->id == 1 && x_session_have_connection ())
          x_session_close ();
#endif
        delete_terminal (t);
        break;
      }

  /* Find any pending selection requests for this display and unchain
     them.  */

  last = NULL;

  for (ie = pending_selection_requests; ie; ie = ie->next)
    {
    again:

      if (SELECTION_EVENT_DPYINFO (&ie->se) == dpyinfo)
	{
	  if (last)
	    last->next = ie->next;

	  temp = ie;
	  ie = ie->next;
	  xfree (temp);

	  goto again;
	}

      last = ie;
    }

  /* Delete selection requests bound for dpyinfo from the keyboard
     buffer.  */
  x_delete_selection_requests (dpyinfo);

  /* And remove any outstanding selection transfers.  */
  x_remove_selection_transfers (dpyinfo);

  if (next_noop_dpyinfo == dpyinfo)
    next_noop_dpyinfo = dpyinfo->next;

  if (mouse_click_timeout_display == dpyinfo)
    mouse_click_timeout_display = NULL;

  if (x_display_list == dpyinfo)
    x_display_list = dpyinfo->next;
  else
    {
      struct x_display_info *tail;

      for (tail = x_display_list; tail; tail = tail->next)
	if (tail->next == dpyinfo)
	  tail->next = tail->next->next;
    }

  for (i = 0; i < dpyinfo->color_names_size; ++i)
    {
      for (color_entry = dpyinfo->color_names[i];
	   color_entry; color_entry = next_color_entry)
	{
	  next_color_entry = color_entry->next;

	  xfree (color_entry->name);
	  xfree (color_entry);
	}
    }

  if (dpyinfo->net_supported_atoms)
    XFree (dpyinfo->net_supported_atoms);

  xfree (dpyinfo->color_names);
  xfree (dpyinfo->color_names_length);
  xfree (dpyinfo->x_id_name);
  xfree (dpyinfo->x_dnd_atoms);
  xfree (dpyinfo->color_cells);
#ifdef HAVE_XFIXES
  if (dpyinfo->monitored_selections)
    xfree (dpyinfo->monitored_selections);
#endif
#ifdef USE_TOOLKIT_SCROLL_BARS
  xfree (dpyinfo->protected_windows);
#endif
#ifdef HAVE_XINPUT2
  if (dpyinfo->supports_xi2)
    x_free_xi_devices (dpyinfo);
#endif
  xfree (dpyinfo);
}

#ifdef USE_X_TOOLKIT

/* Atimer callback function for TIMER.  Called every 0.1s to process
   Xt timeouts, if needed.  We must avoid calling XtAppPending as
   much as possible because that function does an implicit XFlush
   that slows us down.  */

static void
x_process_timeouts (struct atimer *timer)
{
  block_input ();
  x_timeout_atimer_activated_flag = false;
  if (toolkit_scroll_bar_interaction || popup_activated ())
    {
      while (XtAppPending (Xt_app_con) & XtIMTimer)
	XtAppProcessEvent (Xt_app_con, XtIMTimer);
      /* Reactivate the atimer for next time.  */
      x_activate_timeout_atimer ();
    }
  unblock_input ();
}

/* Install an asynchronous timer that processes Xt timeout events
   every 0.1s as long as either `toolkit_scroll_bar_interaction' or
   `popup_activated_flag' (in xmenu.c) is set.  Make sure to call this
   function whenever these variables are set.  This is necessary
   because some widget sets use timeouts internally, for example the
   LessTif menu bar, or the Xaw3d scroll bar.  When Xt timeouts aren't
   processed, these widgets don't behave normally.  */

void
x_activate_timeout_atimer (void)
{
  block_input ();
  if (!x_timeout_atimer_activated_flag)
    {
      struct timespec interval = make_timespec (0, 100 * 1000 * 1000);
      start_atimer (ATIMER_RELATIVE, interval, x_process_timeouts, 0);
      x_timeout_atimer_activated_flag = true;
    }
  unblock_input ();
}

#endif /* USE_X_TOOLKIT */


/* Set up use of X before we make the first connection.  */

static struct redisplay_interface x_redisplay_interface =
  {
    x_frame_parm_handlers,
    gui_produce_glyphs,
    gui_write_glyphs,
    gui_insert_glyphs,
    gui_clear_end_of_line,
    x_scroll_run,
    x_after_update_window_line,
    NULL, /* update_window_begin */
    NULL, /* update_window_end   */
    x_flip_and_flush,
    gui_clear_window_mouse_face,
    gui_get_glyph_overhangs,
    gui_fix_overlapping_area,
    x_draw_fringe_bitmap,
#ifdef USE_CAIRO
    x_cr_define_fringe_bitmap,
    x_cr_destroy_fringe_bitmap,
#else
    0, /* define_fringe_bitmap */
    0, /* destroy_fringe_bitmap */
#endif
    x_compute_glyph_string_overhangs,
    x_draw_glyph_string,
    x_define_frame_cursor,
    x_clear_frame_area,
    x_clear_under_internal_border,
    x_draw_window_cursor,
    x_draw_vertical_window_border,
    x_draw_window_divider,
    x_shift_glyphs_for_insert, /* Never called; see comment in function.  */
    x_show_hourglass,
    x_hide_hourglass,
    x_default_font_parameter
  };


/* This function is called when the last frame on a display is deleted. */
void
x_delete_terminal (struct terminal *terminal)
{
  struct x_display_info *dpyinfo;
  struct frame *f;
  Lisp_Object tail, frame;

  dpyinfo = terminal->display_info.x;

  /* Protect against recursive calls.  delete_frame in
     delete_terminal calls us back when it deletes our last frame.  */
  if (!terminal->name)
    return;

  block_input ();

  /* Delete all remaining frames on the display that is going away.
     Otherwise, font backends assume the display is still up, and
     xftfont_end_for_frame crashes.  */
  FOR_EACH_FRAME (tail, frame)
    {
      f = XFRAME (frame);

      if (FRAME_LIVE_P (f) && f->terminal == terminal)
	/* Pass Qnoelisp rather than Qt.  */
	delete_frame (frame, Qnoelisp);
    }

#ifdef HAVE_X_I18N
  /* We must close our connection to the XIM server before closing the
     X display.  */
  xim_close_dpy (dpyinfo);
#endif

  /* Destroy all bitmap images created on the display.  */
  image_destroy_all_bitmaps (dpyinfo);

  /* Free the storage allocated to hold bitmap records.  */
  xfree (dpyinfo->bitmaps);

  /* In case someone decides to use `bitmaps' again... */
  dpyinfo->bitmaps = NULL;
  dpyinfo->bitmaps_last = 0;

  /* Normally, the display is available...  */
  if (dpyinfo->display)
    {
      XSetCloseDownMode (dpyinfo->display, DestroyAll);

      /* Delete the scratch cursor GC, should it exist.  */
      if (dpyinfo->scratch_cursor_gc)
	XFreeGC (dpyinfo->display, dpyinfo->scratch_cursor_gc);

      /* Get rid of any drag-and-drop operation that might be in
	 progress as well.  */
      if ((x_dnd_in_progress || x_dnd_waiting_for_finish)
	  && dpyinfo->display == (x_dnd_waiting_for_finish
				  ? x_dnd_finish_display
				  : FRAME_X_DISPLAY (x_dnd_frame)))
	{
	  x_dnd_last_seen_window = None;
	  x_dnd_last_seen_toplevel = None;
	  x_dnd_in_progress = false;
	  x_dnd_waiting_for_finish = false;

	  /* The display is going away, so there's no point in
	     de-selecting for input on the DND toplevels.  */
	  if (x_dnd_use_toplevels)
	    x_dnd_free_toplevels (false);

	  x_dnd_return_frame_object = NULL;
	  x_dnd_movement_frame = NULL;
	  x_dnd_wheel_frame = NULL;
	  x_dnd_frame = NULL;
	}

      /* Whether or not XCloseDisplay destroys the associated resource
	 database depends on the version of libX11.  To avoid both
	 crash and memory leak, we dissociate the database from the
	 display and then destroy dpyinfo->rdb ourselves.

	 Unfortunately, the above strategy does not work in some
	 situations due to a bug in newer versions of libX11: because
	 XrmSetDatabase doesn't clear the flag XlibDisplayDfltRMDB if
	 dpy->db is NULL, XCloseDisplay destroys the associated
	 database whereas it has not been created by XGetDefault
	 (Bug#21974 in freedesktop.org Bugzilla).  As a workaround, we
	 don't destroy the database here in order to avoid the crash
	 in the above situations for now, though that may cause memory
	 leaks in other situations.  */
#if false
#ifdef HAVE_XRMSETDATABASE
      XrmSetDatabase (dpyinfo->display, NULL);
#else
      dpyinfo->display->db = NULL;
#endif
      /* We used to call XrmDestroyDatabase from x_delete_display, but
	 some older versions of libX11 crash if we call it after
	 closing all the displays.  */
      XrmDestroyDatabase (dpyinfo->rdb);
#endif
#ifdef USE_GTK
      xg_display_close (dpyinfo->display);
#else
#ifdef USE_X_TOOLKIT
      XtCloseDisplay (dpyinfo->display);
#else
      XCloseDisplay (dpyinfo->display);
#endif
#endif /* ! USE_GTK */
      /* Do not close the connection here because it's already closed
	 by X(t)CloseDisplay (Bug#18403).  */
      dpyinfo->display = NULL;
    }

  /* ...but if called from x_connection_closed, the display may already
     be closed and dpyinfo->display was set to 0 to indicate that.  Since
     X server is most likely gone, explicit close is the only reliable
     way to continue and avoid Bug#19147.  */
  else if (dpyinfo->connection >= 0)
    emacs_close (dpyinfo->connection);

  /* Free the keyboard and modifier maps here; that is safe to do
     without a display, and not doing so leads to a lot of data being
     leaked upon IO error.  */

#ifdef HAVE_XKB
  if (dpyinfo->xkb_desc)
    XkbFreeKeyboard (dpyinfo->xkb_desc, XkbAllComponentsMask, True);
#endif

  if (dpyinfo->modmap)
    XFreeModifiermap (dpyinfo->modmap);

  /* No more input on this descriptor.  */
  delete_keyboard_wait_descriptor (dpyinfo->connection);
  /* Mark as dead. */
  dpyinfo->connection = -1;

  x_delete_display (dpyinfo);
  unblock_input ();
}

#ifdef HAVE_XINPUT2
static bool
x_have_any_grab (struct x_display_info *dpyinfo)
{
  int i;

  if (!dpyinfo->supports_xi2)
    return false;

  for (i = 0; i < dpyinfo->num_devices; ++i)
    {
      if (dpyinfo->devices[i].grab)
	return true;
    }

  return false;
}
#endif

/* Create a struct terminal, initialize it with the X11 specific
   functions and make DISPLAY->TERMINAL point to it.  */

static struct terminal *
x_create_terminal (struct x_display_info *dpyinfo)
{
  struct terminal *terminal;

  terminal = create_terminal (output_x_window, &x_redisplay_interface);

  terminal->display_info.x = dpyinfo;
  dpyinfo->terminal = terminal;

  /* kboard is initialized in x_term_init. */

  terminal->clear_frame_hook = x_clear_frame;
  terminal->ins_del_lines_hook = x_ins_del_lines;
  terminal->delete_glyphs_hook = x_delete_glyphs;
  terminal->ring_bell_hook = XTring_bell;
  terminal->toggle_invisible_pointer_hook = XTtoggle_invisible_pointer;
  terminal->update_begin_hook = x_update_begin;
  terminal->update_end_hook = x_update_end;
  terminal->read_socket_hook = XTread_socket;
  terminal->frame_up_to_date_hook = XTframe_up_to_date;
#ifdef HAVE_XDBE
  terminal->buffer_flipping_unblocked_hook = XTbuffer_flipping_unblocked_hook;
#endif
  terminal->defined_color_hook = x_defined_color;
  terminal->query_frame_background_color = x_query_frame_background_color;
  terminal->query_colors = x_query_colors;
  terminal->mouse_position_hook = XTmouse_position;
  terminal->get_focus_frame = x_get_focus_frame;
  terminal->focus_frame_hook = x_focus_frame;
  terminal->frame_rehighlight_hook = XTframe_rehighlight;
  terminal->frame_raise_lower_hook = XTframe_raise_lower;
  terminal->frame_visible_invisible_hook = x_make_frame_visible_invisible;
  terminal->fullscreen_hook = XTfullscreen_hook;
  terminal->iconify_frame_hook = x_iconify_frame;
  terminal->set_window_size_hook = x_set_window_size;
  terminal->set_window_size_and_position_hook = x_set_window_size_and_position;
  terminal->set_frame_offset_hook = x_set_offset;
  terminal->set_frame_alpha_hook = x_set_frame_alpha;
  terminal->set_new_font_hook = x_new_font;
  terminal->set_bitmap_icon_hook = x_bitmap_icon;
  terminal->implicit_set_name_hook = x_implicitly_set_name;
  terminal->menu_show_hook = x_menu_show;
#ifdef HAVE_EXT_MENU_BAR
  terminal->activate_menubar_hook = x_activate_menubar;
#endif
#if defined (USE_X_TOOLKIT) || defined (USE_GTK)
  terminal->popup_dialog_hook = xw_popup_dialog;
#endif
  terminal->change_tab_bar_height_hook = x_change_tab_bar_height;
#ifndef HAVE_EXT_TOOL_BAR
  terminal->change_tool_bar_height_hook = x_change_tool_bar_height;
#endif
  terminal->set_vertical_scroll_bar_hook = XTset_vertical_scroll_bar;
  terminal->set_horizontal_scroll_bar_hook = XTset_horizontal_scroll_bar;
  terminal->set_scroll_bar_default_width_hook = x_set_scroll_bar_default_width;
  terminal->set_scroll_bar_default_height_hook = x_set_scroll_bar_default_height;
  terminal->condemn_scroll_bars_hook = XTcondemn_scroll_bars;
  terminal->redeem_scroll_bar_hook = XTredeem_scroll_bar;
  terminal->judge_scroll_bars_hook = XTjudge_scroll_bars;
  terminal->get_string_resource_hook = x_get_string_resource;
  terminal->free_pixmap = x_free_pixmap;
  terminal->delete_frame_hook = x_destroy_window;
  terminal->delete_terminal_hook = x_delete_terminal;
  terminal->toolkit_position_hook = x_toolkit_position;
#ifdef HAVE_XINPUT2
  terminal->any_grab_hook = x_have_any_grab;
#endif
  /* Other hooks are NULL by default.  */

  return terminal;
}

static void
x_initialize (void)
{
  baud_rate = 19200;

  x_noop_count = 0;
  any_help_event_p = false;
  ignore_next_mouse_click_timeout = 0;
  mouse_click_timeout_display = NULL;

#ifdef USE_GTK
  current_count = -1;
#endif

  /* Try to use interrupt input; if we can't, then start polling.  */
  Fset_input_interrupt_mode (Qt);

#if THREADS_ENABLED
  /* This must be called before any other Xlib routines.  */
  if (XInitThreads () == 0)
    fputs ("Warning: An error occurred initializing X11 thread support!\n",
	   stderr);
#endif

#ifdef USE_X_TOOLKIT
  XtToolkitInitialize ();

  Xt_app_con = XtCreateApplicationContext ();

  /* Register a converter from strings to pixels, which uses
     Emacs's color allocation infrastructure.  */
  XtAppSetTypeConverter (Xt_app_con,
			 XtRString, XtRPixel, cvt_string_to_pixel,
			 cvt_string_to_pixel_args,
			 XtNumber (cvt_string_to_pixel_args),
			 XtCacheByDisplay, cvt_pixel_dtor);

  XtAppSetFallbackResources (Xt_app_con, Xt_default_resources);
#endif

#ifdef USE_TOOLKIT_SCROLL_BARS
#ifndef USE_GTK
  xaw3d_arrow_scroll = False;
  xaw3d_pick_top = True;
#endif
#endif

#ifdef USE_CAIRO
  gui_init_fringe (&x_redisplay_interface);
#endif

  /* Note that there is no real way portable across R3/R4 to get the
     original error handler.  */
  XSetErrorHandler (x_error_handler);
  XSetIOErrorHandler (x_io_error_quitter);
}

#ifdef HAVE_X_I18N

/* Notice that a change has occurred on F that requires its input
   method state to be reset.  */

static void
x_reset_conversion (struct frame *f)
{
  char *string;

  if (FRAME_XIC (f))
    {
      string = XmbResetIC (FRAME_XIC (f));

      /* string is actually any string that was being composed at the
	 time of the reset.  */

      if (string)
	XFree (string);
    }
}

/* Interface used to control input method ``text conversion''.  */

static struct textconv_interface text_conversion_interface =
  {
    x_reset_conversion,
  };

#endif

void
init_xterm (void)
{
#ifndef HAVE_XINPUT2
  /* Emacs can handle only core input events when built without XI2
     support, so make sure Gtk doesn't use Xinput or Xinput2
     extensions.  */
#ifndef HAVE_GTK3
  xputenv ("GDK_CORE_DEVICE_EVENTS=1");
#else
  gdk_disable_multidevice ();
#endif
#endif

#ifdef HAVE_X_I18N
  register_textconv_interface (&text_conversion_interface);
#endif
}

void
mark_xterm (void)
{
  Lisp_Object val;
#if defined HAVE_XINPUT2 || defined USE_TOOLKIT_SCROLL_BARS \
  || defined HAVE_XRANDR || defined USE_GTK || defined HAVE_X_I18N
  struct x_display_info *dpyinfo;
#if defined HAVE_XINPUT2 || defined USE_TOOLKIT_SCROLL_BARS
  int i;
#endif
#endif

  if (x_dnd_return_frame_object)
    {
      XSETFRAME (val, x_dnd_return_frame_object);
      mark_object (val);
    }

  if (x_dnd_movement_frame)
    {
      XSETFRAME (val, x_dnd_movement_frame);
      mark_object (val);
    }

  if (x_dnd_wheel_frame)
    {
      XSETFRAME (val, x_dnd_wheel_frame);
      mark_object (val);
    }

#if defined HAVE_XINPUT2 || defined USE_TOOLKIT_SCROLL_BARS \
  || defined HAVE_XRANDR || defined USE_GTK || defined HAVE_X_I18N
  for (dpyinfo = x_display_list; dpyinfo; dpyinfo = dpyinfo->next)
    {
#ifdef HAVE_XINPUT2
      for (i = 0; i < dpyinfo->num_devices; ++i)
	mark_object (dpyinfo->devices[i].name);
#endif
#ifdef USE_TOOLKIT_SCROLL_BARS
      for (i = 0; i < dpyinfo->n_protected_windows; ++i)
	mark_object (dpyinfo->protected_windows[i]);
#endif
#if defined HAVE_XRANDR || defined USE_GTK
      mark_object (dpyinfo->last_monitor_attributes_list);
#endif
#if defined HAVE_X_I18N
      mark_object (dpyinfo->xim_coding);
#endif
    }
#endif
}

/* Error handling functions for Lisp functions that expose X protocol
   requests.  They are mostly like `x_catch_errors' and friends, but
   respect `x-fast-protocol-requests'.  */

void
x_catch_errors_for_lisp (struct x_display_info *dpyinfo)
{
  if (!x_fast_protocol_requests)
    x_catch_errors (dpyinfo->display);
  else
    x_ignore_errors_for_next_request (dpyinfo, 0);
}

void
x_check_errors_for_lisp (struct x_display_info *dpyinfo,
			 const char *format)
{
  if (!x_fast_protocol_requests)
    x_check_errors (dpyinfo->display, format);
}

void
x_uncatch_errors_for_lisp (struct x_display_info *dpyinfo)
{
  if (!x_fast_protocol_requests)
    x_uncatch_errors ();
  else
    x_stop_ignoring_errors (dpyinfo);
}

/* Preserve the selections in LOST in another frame on DPYINFO.  LOST
   is a list of local selections that were lost, due to their frame
   being deleted.  */

void
x_preserve_selections (struct x_display_info *dpyinfo, Lisp_Object lost,
		       Lisp_Object current_owner)
{
  Lisp_Object tail, frame, new_owner;
  Time timestamp;
  Window *owners;
  Atom *names;
  ptrdiff_t nowners, counter;
  struct selection_input_event clear;
#ifdef USE_XCB
  xcb_get_selection_owner_cookie_t *cookies;
  xcb_generic_error_t *error;
  xcb_get_selection_owner_reply_t *reply;
#endif

  new_owner = Qnil;

  FOR_EACH_FRAME (tail, frame)
    {
      if (FRAME_X_P (XFRAME (frame))
	  && !EQ (frame, current_owner)
	  && FRAME_DISPLAY_INFO (XFRAME (frame)) == dpyinfo)
	{
	  new_owner = frame;
	  break;
	}
    }

  tail = lost;
  nowners = 0;

  FOR_EACH_TAIL_SAFE (tail)
    {
      Lisp_Object tem = XCAR (tail);
      ++nowners;

      /* The selection is really lost (since we cannot find a new
	 owner), so run the appropriate hooks.  */
      if (NILP (new_owner))
	CALLN (Frun_hook_with_args, Qx_lost_selection_functions,
	       XCAR (tem));
      else
	{
	  CONS_TO_INTEGER (XCAR (XCDR (XCDR (tem))), Time, timestamp);

	  /* This shouldn't be able to signal any errors, despite the
	     call to `x_check_errors' inside.  */
	  x_own_selection (XCAR (tem), XCAR (XCDR (tem)),
			   new_owner, XCAR (XCDR (XCDR (XCDR (XCDR (tem))))),
			   timestamp);
	}
    }

  if (!NILP (new_owner))
    {
      owners = alloca (sizeof *owners * nowners);
      names = alloca (sizeof *names * nowners);
#ifdef USE_XCB
      cookies = alloca (sizeof *cookies * nowners);
#endif

      tail = lost;
      nowners = 0;
      counter = 0;

      FOR_EACH_TAIL_SAFE (tail)
	{
	  Lisp_Object tem = XCAR (tail);

	  /* Now check if we still don't own that selection, which can
	     happen if another program set itself as the owner.  */
	  names[counter++] = symbol_to_x_atom (dpyinfo, XCAR (tem));

#ifndef USE_XCB
	  owners[nowners++] = XGetSelectionOwner (dpyinfo->display,
						  names[counter - 1]);
#else
	  cookies[nowners++]
	    = xcb_get_selection_owner (dpyinfo->xcb_connection,
				       names[counter - 1]);
	}

      nowners = 0;
      tail = lost;

      FOR_EACH_TAIL_SAFE (tail)
	{
	  Lisp_Object tem = XCAR (tail);

	  reply = xcb_get_selection_owner_reply (dpyinfo->xcb_connection,
						 cookies[nowners++], &error);
	  if (reply)
	    owners[nowners - 1] = reply->owner;
	  else
	    owners[nowners - 1] = None;

	  free (reply ? (void *) reply : (void *) error);
#endif

	  if (owners[nowners - 1] != FRAME_X_WINDOW (XFRAME (new_owner)))
	    {
	      /* Clear the local selection, since we know we don't own
		 it any longer.  */
	      CONS_TO_INTEGER (XCAR (XCDR (XCDR (tem))), Time, timestamp);

	      clear.kind = SELECTION_CLEAR_EVENT;

	      SELECTION_EVENT_DPYINFO (&clear) = dpyinfo;
	      SELECTION_EVENT_SELECTION (&clear) = names[nowners - 1];
	      SELECTION_EVENT_TIME (&clear) = timestamp;

	      x_handle_selection_event (&clear);
	    }
	}

      tail = lost;
      nowners = 0;

      FOR_EACH_TAIL_SAFE (tail)
	{
	  Lisp_Object tem = XCAR (tail);

	  /* If the selection isn't owned by us anymore, note that the
	     selection was lost.  */
	  if (owners[nowners++] != FRAME_X_WINDOW (XFRAME (new_owner)))
	    CALLN (Frun_hook_with_args, Qx_lost_selection_functions,
		   XCAR (tem));
	}
    }
}

/* Return a list of the keyboard modifier masks in DPYINFO.

   Value is a list of (HYPER SUPER ALT SHIFT-LOCK META), each element
   being the appropriate modifier mask.  */

Lisp_Object
x_get_keyboard_modifiers (struct x_display_info *dpyinfo)
{
  /* This sometimes happens when the function is called during display
     initialization, which can happen while obtaining vendor specific
     keysyms.  */

#ifdef HAVE_XKB
  if (!dpyinfo->xkb_desc && !dpyinfo->modmap)
    x_find_modifier_meanings (dpyinfo);
#else
  if (!dpyinfo->modmap)
    x_find_modifier_meanings (dpyinfo);
#endif

  return list5 (make_uint (dpyinfo->hyper_mod_mask),
		make_uint (dpyinfo->super_mod_mask),
		make_uint (dpyinfo->alt_mod_mask),
		make_uint (dpyinfo->shift_lock_mask),
		make_uint (dpyinfo->meta_mod_mask));
}

void
syms_of_xterm (void)
{
  x_error_message = NULL;
  PDUMPER_IGNORE (x_error_message);

  x_dnd_monitors = Qnil;
  staticpro (&x_dnd_monitors);

  x_dnd_action_symbol = Qnil;
  staticpro (&x_dnd_action_symbol);

  x_dnd_selection_alias_cell = Fcons (Qnil, Qnil);
  staticpro (&x_dnd_selection_alias_cell);

  x_dnd_unsupported_drop_data = Qnil;
  staticpro (&x_dnd_unsupported_drop_data);

  DEFSYM (Qvendor_specific_keysyms, "vendor-specific-keysyms");
  DEFSYM (Qlatin_1, "latin-1");
  DEFSYM (Qnow, "now");
  DEFSYM (Qx_dnd_targets_list, "x-dnd-targets-list");
  DEFSYM (Qx_auto_preserve_selections, "x-auto-preserve-selections");
  DEFSYM (Qexpose, "expose");
  DEFSYM (Qdont_save, "dont-save");

  DEFSYM (Qmouse_wheel_buttons, "mouse-wheel-buttons");
  DEFSYM (Qwheel_up,    "wheel-up");
  DEFSYM (Qwheel_down,  "wheel-down");
  DEFSYM (Qwheel_left,  "wheel-left");
  DEFSYM (Qwheel_right, "wheel-right");

#ifdef USE_GTK
  xg_default_icon_file = build_string ("icons/hicolor/scalable/apps/emacs.svg");
  staticpro (&xg_default_icon_file);

  DEFSYM (Qx_gtk_map_stock, "x-gtk-map-stock");
#endif

  DEFVAR_BOOL ("x-use-underline-position-properties",
	       x_use_underline_position_properties,
     doc: /* Non-nil means make use of UNDERLINE_POSITION font properties.
A value of nil means ignore them.  If you encounter fonts with bogus
UNDERLINE_POSITION font properties, set this to nil.  You can also use
`underline-minimum-offset' to override the font's UNDERLINE_POSITION for
small font display sizes.  */);
  x_use_underline_position_properties = true;
  DEFSYM (Qx_use_underline_position_properties,
	  "x-use-underline-position-properties");

  DEFVAR_BOOL ("x-underline-at-descent-line",
	       x_underline_at_descent_line,
     doc: /* Non-nil means to draw the underline at the same place as the descent line.
(If `line-spacing' is in effect, that moves the underline lower by
that many pixels.)
A value of nil means to draw the underline according to the value of the
variable `x-use-underline-position-properties', which is usually at the
baseline level.  The default value is nil.  */);
  x_underline_at_descent_line = false;
  DEFSYM (Qx_underline_at_descent_line, "x-underline-at-descent-line");

  DEFVAR_BOOL ("x-mouse-click-focus-ignore-position",
	       x_mouse_click_focus_ignore_position,
    doc: /* Non-nil means that a mouse click to focus a frame does not move point.
This variable is used only when the window manager requires that you
click on a frame to select it (give it focus).  In that case, a value
of nil, means that the selected window and cursor position changes to
reflect the mouse click position, while a non-nil value means that the
selected window or cursor position is preserved.

This option works by ignoring button press events for a given amount
of time after a frame might've been focused.  If it does not work for
you, try increasing the value of
`x-mouse-click-focus-ignore-time'.  */);
  x_mouse_click_focus_ignore_position = false;

  DEFVAR_INT ("x-mouse-click-focus-ignore-time", x_mouse_click_focus_ignore_time,
    doc: /* Number of milliseconds for which to ignore buttons after focus change.
This variable only takes effect if
`x-mouse-click-focus-ignore-position' is non-nil, and should be
adjusted if the default value does not work for whatever reason.  */);
  x_mouse_click_focus_ignore_time = 200;

  DEFVAR_LISP ("x-toolkit-scroll-bars", Vx_toolkit_scroll_bars,
    doc: /* Which toolkit scroll bars Emacs uses, if any.
A value of nil means Emacs doesn't use toolkit scroll bars.
With the X Window system, the value is a symbol describing the
X toolkit.  Possible values are: gtk, motif, xaw, or xaw3d.
With MS Windows, Haiku windowing or Nextstep, the value is t.
With Android, the value is nil, but that is because Emacs on
Android does not support scroll bars at all.  */);
#ifdef USE_TOOLKIT_SCROLL_BARS
#ifdef USE_MOTIF
  Vx_toolkit_scroll_bars = intern_c_string ("motif");
#elif defined HAVE_XAW3D
  Vx_toolkit_scroll_bars = intern_c_string ("xaw3d");
#elif USE_GTK
  Vx_toolkit_scroll_bars = intern_c_string ("gtk");
#else
  Vx_toolkit_scroll_bars = intern_c_string ("xaw");
#endif
#else
  Vx_toolkit_scroll_bars = Qnil;
#endif

  DEFSYM (Qmodifier_value, "modifier-value");
  DEFSYM (Qctrl, "ctrl");
  Fput (Qctrl, Qmodifier_value, make_fixnum (ctrl_modifier));
  DEFSYM (Qalt, "alt");
  Fput (Qalt, Qmodifier_value, make_fixnum (alt_modifier));
  DEFSYM (Qhyper, "hyper");
  Fput (Qhyper, Qmodifier_value, make_fixnum (hyper_modifier));
  DEFSYM (Qmeta, "meta");
  Fput (Qmeta, Qmodifier_value, make_fixnum (meta_modifier));
  DEFSYM (Qsuper, "super");
  Fput (Qsuper, Qmodifier_value, make_fixnum (super_modifier));
  DEFSYM (QXdndSelection, "XdndSelection");
  DEFSYM (Qx_selection_alias_alist, "x-selection-alias-alist");
  DEFSYM (Qimitate_pager, "imitate-pager");
  DEFSYM (Qnewer_time, "newer-time");
  DEFSYM (Qraise_and_focus, "raise-and-focus");
  DEFSYM (Qreally_fast, "really-fast");

  /* Referenced in gtkutil.c.  */
  DEFSYM (Qtheme_name, "theme-name");
  DEFSYM (Qfile_name_sans_extension, "file-name-sans-extension");

  DEFVAR_LISP ("x-ctrl-keysym", Vx_ctrl_keysym,
    doc: /* Which modifier value Emacs reports when Ctrl is depressed.
This should be one of the symbols `ctrl', `alt', `hyper', `meta', or
`super', representing a modifier to be reported for key events with the
Ctrl modifier (i.e. the keysym Ctrl_L or Ctrl_R) depressed, with nil or
any other value equivalent to `ctrl'.  */);
  Vx_ctrl_keysym = Qnil;

  DEFVAR_LISP ("x-alt-keysym", Vx_alt_keysym,
    doc: /* Which modifier value Emacs reports when Alt is depressed.
This should be one of the symbols `ctrl', `alt', `hyper', `meta', or
`super', representing a modifier to be reported for key events with the
Alt modifier (e.g. the keysym Alt_L or Alt_R, if the keyboard features a
dedicated key for Meta) depressed, with nil or any other value
equivalent to `alt'.  */);
  Vx_alt_keysym = Qnil;

  DEFVAR_LISP ("x-hyper-keysym", Vx_hyper_keysym,
    doc: /* Which modifier value Emacs reports when Hyper is depressed.
This should be one of the symbols `ctrl', `alt', `hyper', `meta', or
`super', representing a modifier to be reported for key events with the
Hyper modifier (i.e. the keysym Hyper_L or Hyper_R) depressed, with nil
or any other value equivalent to `hyper'.  */);
  Vx_hyper_keysym = Qnil;

  DEFVAR_LISP ("x-meta-keysym", Vx_meta_keysym,
    doc: /* Which modifier value Emacs reports when Meta is depressed.
This should be one of the symbols `ctrl', `alt', `hyper', `meta', or
`super', representing a modifier to be reported for key events with the
Meta modifier (e.g. the keysym Alt_L or Alt_R, when the keyboard does
not feature a dedicated key for Meta) depressed, with nil or any other
value equivalent to `meta'.  */);
  Vx_meta_keysym = Qnil;

  DEFVAR_LISP ("x-super-keysym", Vx_super_keysym,
    doc: /* Which modifier value Emacs reports when Super is depressed.
This should be one of the symbols `ctrl', `alt', `hyper', `meta', or
`super', representing a modifier to be reported for key events with the
Super modifier (i.e. the keysym Super_L or Super_R) depressed, with nil
or any other value equivalent to `super'.  */);
  Vx_super_keysym = Qnil;

  DEFVAR_LISP ("x-wait-for-event-timeout", Vx_wait_for_event_timeout,
    doc: /* How long to wait for X events.

Emacs will wait up to this many seconds to receive X events after
making changes which affect the state of the graphical interface.
Under some window managers this can take an indefinite amount of time,
so it is important to limit the wait.

If set to a non-float value, there will be no wait at all.  */);
  Vx_wait_for_event_timeout = make_float (0.1);

  DEFVAR_LISP ("x-keysym-table", Vx_keysym_table,
    doc: /* Hash table of character codes indexed by X keysym codes.  */);
  Vx_keysym_table = make_hash_table (&hashtest_eql, 900, Weak_None);

  DEFVAR_BOOL ("x-frame-normalize-before-maximize",
	       x_frame_normalize_before_maximize,
    doc: /* Non-nil means normalize frame before maximizing.
If this variable is t, Emacs first asks the window manager to give the
frame its normal size, and only then the final state, whenever changing
from a full-height, full-width or full-both state to the maximized one
or when changing from the maximized to the full-height or full-width
state.

Set this variable only if your window manager cannot handle the
transition between the various maximization states.  */);
  x_frame_normalize_before_maximize = false;

  DEFVAR_BOOL ("x-gtk-use-window-move", x_gtk_use_window_move,
    doc: /* Non-nil means rely on gtk_window_move to set frame positions.
If this variable is t (the default), the GTK build uses the function
gtk_window_move to set or store frame positions and disables some time
consuming frame position adjustments.  In newer versions of GTK, Emacs
always uses gtk_window_move and ignores the value of this variable.  */);
  x_gtk_use_window_move = true;

  DEFVAR_LISP ("x-scroll-event-delta-factor", Vx_scroll_event_delta_factor,
	       doc: /* A scale to apply to pixel deltas reported in scroll events.
This option is only effective when Emacs is built with XInput 2
support. */);
  Vx_scroll_event_delta_factor = make_float (1.0);

  DEFVAR_BOOL ("x-gtk-use-native-input", x_gtk_use_native_input,
	       doc: /* Non-nil means to use GTK for input method support.
This provides better support for some modern input methods, and is
only effective when Emacs is built with GTK.  */);
  x_gtk_use_native_input = false;

  DEFVAR_LISP ("x-set-frame-visibility-more-laxly",
	       x_set_frame_visibility_more_laxly,
    doc: /* Non-nil means set frame visibility more laxly.
If this is nil, Emacs is more strict when marking a frame as visible.
Since this may cause problems on some window managers, this variable can
be also set as follows: The value `focus-in' means to mark a frame as
visible also when a FocusIn event is received for it on GTK builds.  The
value `expose' means to mark a frame as visible also when an Expose
event is received for it on any X build.  The value `t' means to mark a
frame as visible in either of these two cases.

Note that any non-nil setting may cause invisible frames get erroneously
reported as iconified.  */);
  x_set_frame_visibility_more_laxly = Qnil;

  DEFVAR_BOOL ("x-input-grab-touch-events", x_input_grab_touch_events,
	       doc: /* Non-nil means to actively grab touch events.
This means touch sequences that are obtained through a passive grab on
an Emacs frame (or a parent window of such a frame) will reliably
continue to receive updates, but may cause crashes with some window
managers and/or external programs.  Changing this option is only
useful when other programs are making their own X requests pertaining
to the window hierarchy of an Emacs frame.  */);
  x_input_grab_touch_events = true;

  DEFVAR_BOOL ("x-dnd-fix-motif-leave", x_dnd_fix_motif_leave,
	       doc: /* Work around Motif bug during drag-and-drop.
When non-nil, Emacs will send a motion event containing impossible
coordinates to a Motif drop receiver when the mouse moves outside it
during a drag-and-drop session, to work around broken implementations
of Motif.  */);
  x_dnd_fix_motif_leave = true;

  DEFVAR_BOOL ("x-dnd-disable-motif-drag", x_dnd_disable_motif_drag,
	       doc: /* Disable the Motif drag protocol during DND.
This reduces network usage, but also means you can no longer scroll
around inside the Motif window underneath the cursor during
drag-and-drop.  */);
  x_dnd_disable_motif_drag = false;

  DEFVAR_LISP ("x-dnd-movement-function", Vx_dnd_movement_function,
    doc: /* Function called upon mouse movement on a frame during drag-and-drop.
It should either be nil, or accept two arguments FRAME and POSITION,
where FRAME is the frame the mouse is on top of, and POSITION is a
mouse position list.  */);
  Vx_dnd_movement_function = Qnil;

  DEFVAR_LISP ("x-dnd-wheel-function", Vx_dnd_wheel_function,
    doc: /* Function called upon wheel movement on a frame during drag-and-drop.
It should either be nil, or accept four arguments POSITION, BUTTON,
STATE and TIME, where POSITION is a mouse position list describing
where the wheel moved, BUTTON is the wheel button that was pressed,
STATE is the X modifier state at the time of the wheel movement, and
TIME is the X server time at which the wheel moved.  */);
  Vx_dnd_wheel_function = Qnil;

  DEFVAR_LISP ("x-dnd-unsupported-drop-function", Vx_dnd_unsupported_drop_function,
    doc: /* Function called when trying to drop on an unsupported window.
This function is called whenever the user tries to drop something on a
window that does not support either the XDND or Motif protocols for
drag-and-drop.  It should return a non-nil value if the drop was
handled by the function, and nil if it was not.  It should accept
several arguments TARGETS, X, Y, ACTION, WINDOW-ID, FRAME, TIME and
LOCAL-SELECTION, where TARGETS is the list of targets that was passed
to `x-begin-drag', WINDOW-ID is the numeric XID of the window that is
being dropped on, X and Y are the root window-relative coordinates
where the drop happened, ACTION is the action that was passed to
`x-begin-drag', FRAME is the frame which initiated the drag-and-drop
operation, TIME is the X server time when the drop happened, and
LOCAL-SELECTION is the contents of the `XdndSelection' when
`x-begin-drag' was run; its contents can be retrieved by calling the
function `x-get-local-selection'.

If a symbol is returned, then it will be used as the return value of
`x-begin-drag'.  */);
  Vx_dnd_unsupported_drop_function = Qnil;

  DEFVAR_INT ("x-color-cache-bucket-size", x_color_cache_bucket_size,
    doc: /* Max number of buckets allowed per display in the internal color cache.
Values less than 1 mean 128.  This option is for debugging only.  */);
  x_color_cache_bucket_size = 128;

  DEFVAR_LISP ("x-dnd-targets-list", Vx_dnd_targets_list,
    doc: /* List of drag-and-drop targets.
This variable contains the list of drag-and-drop selection targets
during a drag-and-drop operation, in the same format as the TARGET
argument to `x-begin-drag'.  */);
  Vx_dnd_targets_list = Qnil;

  DEFVAR_LISP ("x-dnd-native-test-function", Vx_dnd_native_test_function,
    doc: /* Function that determines return value of drag-and-drop on Emacs frames.
If the value is a function, `x-begin-drag' will call it with two
arguments, POS and ACTION, where POS is a mouse position list
that specifies the location of the drop, and ACTION is the
action specified by the caller of `x-begin-drag'.  The function
should return a symbol describing what to return from
`x-begin-drag' if the drop happens on an Emacs frame.

If the value is nil, or the function returns a value that is not
a symbol, a drop on an Emacs frame will be canceled.  */);
  Vx_dnd_native_test_function = Qnil;

  DEFVAR_BOOL ("x-dnd-preserve-selection-data", x_dnd_preserve_selection_data,
    doc: /* Preserve selection data after `x-begin-drag' returns.
This lets you inspect the contents of `XdndSelection' after a
drag-and-drop operation, which is useful when writing tests for
drag-and-drop code.  */);
  x_dnd_preserve_selection_data = false;

  DEFVAR_BOOL ("x-dnd-disable-motif-protocol", x_dnd_disable_motif_protocol,
    doc: /* Disable the Motif drag-and-drop protocols.
When non-nil, `x-begin-drag' will not drop onto any window that only
supports the Motif drag-and-drop protocols.  */);
  x_dnd_disable_motif_protocol = false;

  DEFVAR_BOOL ("x-dnd-use-unsupported-drop", x_dnd_use_unsupported_drop,
    doc: /* Enable the emulation of drag-and-drop based on the primary selection.
When nil, do not use the primary selection and synthetic mouse clicks
to emulate the drag-and-drop of `STRING', `UTF8_STRING',
`COMPOUND_TEXT' or `TEXT'.  */);
  x_dnd_use_unsupported_drop = true;

  DEFVAR_BOOL ("x-fast-protocol-requests", x_fast_protocol_requests,
    doc: /* Whether or not X protocol-related functions should wait for errors.
When this is nil, functions such as `x-delete-window-property',
`x-change-window-property' and `x-send-client-message' will wait for a
reply from the X server, and signal any errors that occurred while
executing the protocol request.  Otherwise, errors will be silently
ignored without waiting, which is generally faster.  */);
  x_fast_protocol_requests = false;

  DEFVAR_LISP ("x-auto-preserve-selections", Vx_auto_preserve_selections,
    doc: /* Whether or not to transfer selection ownership when deleting a frame.
When non-nil, deleting a frame that is currently the owner of a
selection will cause its ownership to be transferred to another frame
on the same display.

In addition, when this variable is a list, only preserve the
selections whose names are contained within.  */);
  Vx_auto_preserve_selections = list2 (QCLIPBOARD, QPRIMARY);

  DEFVAR_LISP ("x-input-coding-system", Vx_input_coding_system,
    doc: /* Coding system used for input from X input methods.
If a symbol and non-nil, this is the coding system that will be used
to decode input from X input methods.  It does not affect input from
GTK native input methods enabled through `x-gtk-use-native-input'.  */);
  Vx_input_coding_system = Qnil;

  DEFVAR_LISP ("x-input-coding-function", Vx_input_coding_function,
    doc: /* Function used to determine the coding system used by input methods.
It should accept a single argument, a string describing the locale of
the input method, and return a coding system that can decode keyboard
input generated by said input method.  */);
  Vx_input_coding_function = Qnil;

  DEFVAR_LISP ("x-fast-selection-list", Vx_fast_selection_list,
    doc: /* List of selections for which `x-selection-exists-p' should be fast.

List of selection names as atoms that will be monitored by Emacs for
ownership changes when the X server supports the XFIXES extension.
The result of the monitoring is then used by `x-selection-exists-p' to
avoid a server round trip, which is important as it is called while
updating the tool bar.  The value of this variable is only read upon
connection setup.  */);
  /* The default value of this variable is chosen so that updating the
     tool bar does not require a call to _XReply.  */
  Vx_fast_selection_list = list1 (QCLIPBOARD);

  DEFVAR_LISP ("x-allow-focus-stealing", Vx_allow_focus_stealing,
    doc: /* How to bypass window manager focus stealing prevention.

Some window managers prevent `x-focus-frame' from activating the given
frame when Emacs is in the background, which is especially prone to
cause problems when the Emacs server wants to activate itself.

In addition, when an old-fashioned (pre-EWMH) window manager is being
run and `x-no-window-manager' is nil, the X server will not let Emacs
focus itself if another program was focused after the last time Emacs
obtained the input focus.

This variable specifies the strategy used to activate frames when that
is the case, and has several valid values (any other value means to
not bypass window manager focus stealing prevention):

  - The symbol `imitate-pager', which means to pretend that Emacs is a
    pager.

  - The symbol `newer-time', which means to fetch the current time
    from the X server and use it to activate the frame.

  - The symbol `raise-and-focus', which means to raise the window and
    focus it manually.  */);
  Vx_allow_focus_stealing = Qnewer_time;

  DEFVAR_LISP ("x-use-fast-mouse-position", Vx_use_fast_mouse_position,
    doc: /* How to make `mouse-position' faster.

`mouse-position' and `mouse-pixel-position' default to querying the X
server for the window under the mouse pointer.  This results in
accurate results, but is also very slow when the X connection has
moderate to high latency.  Setting this variable to a non-nil value
makes Emacs query only for the position of the pointer, which is
usually faster.  Doing so improves the performance of dragging to
select text over slow X connections.

If that is still too slow, setting this variable to the symbol
`really-fast' will make Emacs return only cached values.  */);
  Vx_use_fast_mouse_position = Qnil;

  DEFVAR_LISP ("x-detect-server-trust", Vx_detect_server_trust,
    doc: /* If non-nil, Emacs should detect whether or not it is trusted by X.

If non-nil, Emacs will make an X request at connection startup that is
prohibited to untrusted clients under the X Security Extension and
check whether or not a resulting Access error is generated by the X
server.  If the X server reports the error, Emacs will disable certain
features that do not work for untrusted clients.  */);
  Vx_detect_server_trust = Qnil;

  DEFVAR_LISP ("x-lax-frame-positioning", Vx_lax_frame_positioning,
    doc: /* If non-nil, Emacs won't compensate for WM geometry behavior.

Setting this to non-nil is useful when the compensation proves to be
too slow, which is usually true when the X server is located over a
network connection with high latency.  Doing so will make frame
creation and placement faster at the cost of reducing the accuracy of
frame placement via frame parameters, `set-frame-position', and
`set-frame-size', along with the actual state of a frame after
`x_make_frame_invisible'.  */);
  Vx_lax_frame_positioning = Qnil;

  DEFVAR_LISP ("x-quit-keysym", Vx_quit_keysym,
    doc: /* Keysyms which will cause Emacs to quit if rapidly pressed twice.

This is used to support quitting on devices that do not have any kind
of physical keyboard, or where the physical keyboard is incapable of
entering `C-g'.

The value is an alist associating between strings, describing X server
vendor names, and a single number describing the keysym to use.  The
keysym to use for each display connection is determined upon
connection setup, and does not reflect further changes to this
variable.  */);
  Vx_quit_keysym = Qnil;
}
