;;; tpu-doc.el --- Documentation for TPU-edt

;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: Rob Riepel <riepel@networking.stanford.edu>
;; Maintainer: Rob Riepel <riepel@networking.stanford.edu>
;; Keywords: emulations

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;; This is documentation for the TPU-edt editor for GNU emacs.  Major
;; sections of this document are separated with lines that begin with
;; ";; %% <topic>", where <topic> is what is discussed in that section.


;; %% Contents

;;  % Introduction
;;  % Terminal Support
;;  % X-windows Support
;;  % Differences Between TPU-edt and the Real Thing
;;  % Starting TPU-edt
;;  % TPU-edt Default Editing Keypad, Control and Gold Key Bindings
;;  % Optional TPU-edt Extensions
;;  % Customizing TPU-edt using the Emacs Initialization File
;;  % Compiling TPU-edt
;;  % Regular expressions in TPU-edt
;;  % Etcetera


;; %% Introduction

;;    TPU-edt is based on tpu.el by Jeff Kowalski and Bob Covey.  TPU-edt
;;    endeavors to be even more like TPU's EDT emulation than the original
;;    tpu.el.  Considerable effort has been expended to that end.  Still,
;;    emacs is emacs and there are differences between TPU-edt and the
;;    real thing.  Please read the "Differences Between TPU-edt and the
;;    Real Thing" and "Starting TPU-edt" sections before running TPU-edt.


;; %% Terminal Support

;;    TPU-edt, like it's VMS cousin, works on VT-series terminals with
;;    DEC style keyboards.  VT terminal emulators, including xterm with
;;    the appropriate key translations, work just fine too.


;; %% X-windows Support

;;    Starting with version 19 of emacs, TPU-edt works with X-windows.
;;    This is accomplished through a TPU-edt X keymap.  The emacs lisp
;;    program tpu-mapper.el creates this map and stores it in a file.
;;    Tpu-mapper will be run automatically the first time you invoke
;;    the X-windows version of emacs, or you can run it by hand.  See
;;    the commentary in tpu-mapper.el for details.


;; %% Differences Between TPU-edt and the Real Thing (not Coke (r))

;;    Emacs (version 18.58) doesn't support text highlighting, so selected
;;    regions are not shown in inverse video.  Emacs uses the concept of
;;    "the mark".  The mark is set at one end of a selected region; the
;;    cursor is at the other.  The letter "M" appears in the mode line
;;    when the mark is set.  The native emacs command ^X^X (Control-X
;;    twice) exchanges the cursor with the mark; this provides a handy
;;    way to find the location of the mark.

;;    In TPU the cursor can be either bound or free.  Bound means the
;;    cursor cannot wander outside the text of the file being edited.
;;    Free means the arrow keys can move the cursor past the ends of
;;    lines.  Free is the default mode in TPU; bound is the only mode
;;    in EDT.  Bound is the only mode in the base version of TPU-edt;
;;    optional extensions add an approximation of free mode.

;;    Like TPU, emacs uses multiple buffers.  Some buffers are used to
;;    hold files you are editing; other "internal" buffers are used for
;;    emacs' own purposes (like showing you help).  Here are some commands
;;    for dealing with buffers.

;;	 Gold-B   moves to next buffer, including internal buffers
;;	 Gold-N   moves to next buffer containing a file
;;	 Gold-M   brings up a buffer menu (like TPU "show buffers")

;;    Emacs is very fond of throwing up new windows.  Dealing with all
;;    these windows can be a little confusing at first, so here are a few
;;    commands to that may help:

;;       Gold-Next_Scr  moves to the next window on the screen
;;       Gold-Prev_Scr  moves to the previous window on the screen
;;       Gold-TAB       also moves to the next window on the screen

;;	 Control-x 1    deletes all but the current window
;;	 Control-x 0    deletes the current window

;;    Note that the buffers associated with deleted windows still exist!

;;    Like TPU, TPU-edt has a "command" function, invoked with Gold-KP7 or
;;    Do.  Most of the commands available are emacs commands.  Some TPU
;;    commands are available, they are: replace, exit, quit, include, and
;;    Get (unfortunately, "get" is an internal emacs function, so we are
;;    stuck with "Get" - to make life easier, Get is available as Gold-g).

;;    Support for recall of commands, file names, and search strings was
;;    added to emacs in version 19.  For version 18 of emacs, optional
;;    extensions are available to add this recall capability (see "Optional
;;    TPU-edt Extensions" below).  The history of strings recalled in both
;;    versions of emacs differs slightly from TPU/edt, but it is still very
;;    convenient.

;;    Help is available!  The traditional help keys (Help and PF2) display
;;    a three page help file showing the default keypad layout, control key
;;    functions, and Gold key functions.  Pressing any key inside of help
;;    splits the screen and prints a description of the function of the
;;    pressed key.  Gold-PF2 invokes the native emacs help, with it's
;;    zillions of options.  Gold-Help shows all the current key bindings.

;;    Thanks to emacs, TPU-edt has some extensions that may make your life
;;    easier, or at least more interesting.  For example, Gold-r toggles
;;    TPU-edt rectangular mode.  In rectangular mode, Remove and Insert work
;;    on rectangles.  Likewise, Gold-* toggles TPU-edt regular expression
;;    mode.  In regular expression mode Find, Find Next, and the line-mode
;;    replace command work with regular expressions.  [A regular expression
;;    is a pattern that denotes a set of strings; like VMS wildcards.]

;;    Emacs also gives TPU-edt the undo and occur functions.  Undo does
;;    what it says; it undoes the last change.  Multiple undos in a row
;;    undo multiple changes.  For your convenience, undo is available on
;;    Gold-u.  Occur shows all the lines containing a specific string in
;;    another window.  Moving to that window, and typing ^C^C (Control-C
;;    twice) on a particular line moves you back to the original window
;;    at that line.  Occur is on Gold-o.

;;    Finally, as you edit, remember that all the power of emacs is at
;;    your disposal.  It really is a fantastic tool.  You may even want to
;;    take some time and read the emacs tutorial; perhaps not to learn the
;;    native emacs key bindings, but to get a feel for all the things
;;    emacs can do for you.  The emacs tutorial is available from the
;;    emacs help function: "Gold-PF2 t"


;; %% Starting TPU-edt

;;    In order to use TPU-edt, the TPU-edt editor definitions, contained
;;    in tpu-edt.el, need to be loaded when emacs is run.  This can be
;;    done in a couple of ways.  The first is by explicitly requesting
;;    loading of the TPU-edt emacs definition file on the command line:

;;        prompt> emacs -l /path/to/definitions/tpu-edt.el

;;    If TPU-edt is installed on your system, that is, if tpu-edt.el is in
;;    a directory like /usr/local/emacs/lisp, along with dozens of other
;;    .el files, you should be able to use the command:

;;        prompt> emacs -l tpu-edt

;;    If you like TPU-edt and want to use it all the time, you can load
;;    the TPU-edt definitions using the emacs initialization file, .emacs.
;;    Simply create a .emacs file in your home directory containing the
;;    line:

;;        (load "/path/to/definitions/tpu-edt")

;;    or, if (as above) TPU-edt is installed on your system:

;;        (load "tpu-edt")

;;    Once TPU-edt has been loaded, you will be using an editor with the
;;    interface shown in the next section (A section that is suitable for
;;    cutting out of this document and pasting next to your terminal!).


;; %% TPU-edt Default Editing Keypad, Control and Gold Key Bindings
;;
;;        _______________________    _______________________________
;;       | HELP  |      Do       |  |       |       |       |       |
;;       |KeyDefs|               |  |       |       |       |       |
;;       |_______|_______________|  |_______|_______|_______|_______|
;;        _______________________    _______________________________
;;       | Find  |Insert |Remove |  | Gold  | HELP  |FndNxt | Del L |
;;       |       |       |Sto Tex|  |  key  |E-Help | Find  |Undel L|
;;       |_______|_______|_______|  |_______|_______|_______|_______|
;;       |Select |Pre Scr|Nex Scr|  | Page  | Sect  |Append | Del W |
;;       | Reset |Pre Win|Nex Win|  |  Do   | Fill  |Replace|Undel W|
;;       |_______|_______|_______|  |_______|_______|_______|_______|
;;               |Move up|          |Forward|Reverse|Remove | Del C |
;;               |  Top  |          |Bottom |  Top  |Insert |Undel C|
;;        _______|_______|_______   |_______|_______|_______|_______|
;;       |Mov Lef|Mov Dow|Mov Rig|  | Word  |  EOL  | Char  |       |
;;       |StaOfLi|Bottom |EndOfLi|  |ChngCas|Del EOL|SpecIns| Enter |
;;       |_______|_______|_______|  |_______|_______|_______|       |
;;                                  |     Line      |Select | Subs  |
;;                                  |   Open Line   | Reset |       |
;;                                  |_______________|_______|_______|
;;     Control Characters
;;
;;    ^A  toggle insert and overwrite    ^L  insert page break
;;    ^B  recall                         ^R  remember, re-center
;;    ^E  end of line                    ^U  delete to beginning of line
;;    ^G  cancel current operation       ^V  quote
;;    ^H  beginning of line              ^W  refresh
;;    ^J  delete previous word           ^Z  exit
;;    ^K  learn                        ^X^X  exchange point and mark
;;
;;
;;     Gold-<key> Functions
;;     -----------------------------------------------------------------
;;     W     Write - save current buffer
;;     K     Kill buffer - abandon edits and delete buffer
;;
;;     E     Exit - save current buffer and ask about others
;;     X     eXit - save all modified buffers and exit
;;     Q     Quit - exit without saving anything
;;
;;     G     Get - load a file into a new edit buffer
;;     I     Include - include a file in this buffer
;;
;;     B     next Buffer - display the next buffer (all buffers)
;;     N     Next file buffer - display next buffer containing a file
;;     M     buffer Menu - display a list of all buffers
;;
;;     U     Undo - undo the last edit
;;     C     Recall - edit and possibly repeat previous commands
;;
;;     O     Occur - show following lines containing REGEXP
;;     S     Search and substitute - line mode REPLACE command
;;
;;     ?     Spell check - check spelling in a region or entire buffer
;;
;;     R     Toggle Rectangular mode for remove and insert
;;     *     Toggle regular expression mode for search and substitute
;;
;;     V     Show TPU-edt version
;;     -----------------------------------------------------------------


;; %% Optional TPU-edt Extensions

;;    Several optional packages have been included in this distribution
;;    of TPU-edt.  The following is a brief description of each package.
;;    See the {package}.el file for more detailed information and usage
;;    instructions.

;;      tpu-extras  -  TPU/edt scroll margins and free cursor mode.
;;      tpu-recall  -  String, file name, and command history.
;;      vt-control  -  VTxxx terminal width and keypad controls.

;;    Packages are normally loaded from the emacs initialization file
;;    (discussed below).  If a package is not installed in the emacs
;;    lisp directory, it can be loaded by specifying the complete path
;;    to the package file.  However, it is preferable to modify the
;;    emacs load-path variable to include the directory where packages
;;    are stored.  This way, packages can be loaded by name, just as if
;;    they were installed.  The first part of the sample .emacs file
;;    below shows how to make such a modification.


;; %% Customizing TPU-edt using the Emacs Initialization File

;; .emacs - a sample emacs initialization file

;; This is a sample emacs initialization file.  It shows how to invoke
;; TPU-edt, and how to customize it.

;; The load-path is where emacs looks for files to fulfill load requests.
;; If TPU-edt is not installed in a standard emacs directory, the load-path
;; should be updated to include the directory where the TPU-edt files are
;; stored.  Modify and un-comment the following section if TPU-ed is not
;; installed on your system - be sure to leave the double quotes!

;; (setq load-path
;;       (append (list (expand-file-name "/path/to/tpu-edt/files"))
;;               load-path))

;; Load TPU-edt
(load "tpu-edt")

;; Load the optional goodies - scroll margins, free cursor mode, command
;; and string recall.  But don't complain if the file aren't available.
(load "tpu-extras" t)
(load "tpu-recall" t)

;; Uncomment this line to set scroll margins 10% (top) and 15% (bottom).
;(and (fboundp 'tpu-set-scroll-margins) (tpu-set-scroll-margins "10%" "15%"))

;; Load the vtxxx terminal control functions, but don't complain if
;; if the file is not found.
(load "vt-control" t)

;; TPU-edt treats words like EDT; here's how to add word separators.
;; Note that backslash (\) and double quote (") are quoted with '\'.
(tpu-add-word-separators "]\\[-_,.\"=+()'/*#:!&;$")

;; Emacs is happy to save files without a final newline; other Unix programs
;; hate that!  This line will make sure that files end with newlines.
(setq require-final-newline t)

;; Emacs has the ability to automatically run code embedded in files
;; you edit.  This line makes emacs ask if you want to run the code.
(if tpu-emacs19-p (setq enable-local-variables "ask")
  (setq inhibit-local-variables t))

;; Emacs uses Control-s and Control-q.  Problems can occur when using emacs
;; on terminals that use these codes for flow control (Xon/Xoff flow control).
;; These lines disable emacs' use of these characters.
(global-unset-key "\C-s")
(global-unset-key "\C-q")

;; top, bottom, bol, eol seem like a waste of Gold-arrow functions.  The
;; following section re-maps up and down arrow keys to top and bottom of
;; screen, and left and right arrow keys to pan left and right (pan-left,
;; right moves the screen 16 characters left or right - try it, you'll
;; like it!).

;; Re-map the Gold-arrow functions
(define-key GOLD-CSI-map "A" 'tpu-beginning-of-window)          ; up-arrow
(define-key GOLD-CSI-map "B" 'tpu-end-of-window)                ; down-arrow
(define-key GOLD-CSI-map "C" 'tpu-pan-right)                    ; right-arrow
(define-key GOLD-CSI-map "D" 'tpu-pan-left)                     ; left-arrow
(define-key GOLD-SS3-map "A" 'tpu-beginning-of-window)          ; up-arrow
(define-key GOLD-SS3-map "B" 'tpu-end-of-window)                ; down-arrow
(define-key GOLD-SS3-map "C" 'tpu-pan-right)                    ; right-arrow
(define-key GOLD-SS3-map "D" 'tpu-pan-left)                     ; left-arrow

;; Re-map the Gold-arrow functions for X-windows TPU-edt (emacs version 19)
(cond
 ((and tpu-emacs19-p window-system)
  (define-key GOLD-map [up] 'tpu-beginning-of-window)           ; up-arrow
  (define-key GOLD-map [down] 'tpu-end-of-window)               ; down-arrow
  (define-key GOLD-map [right] 'tpu-pan-right)                  ; right-arrow
  (define-key GOLD-map [left] 'tpu-pan-left)))                  ; left-arrow

;; The emacs universal-argument function is very useful for native emacs
;; commands.  This line maps universal-argument to Gold-PF1
(define-key GOLD-SS3-map "P" 'universal-argument)               ; Gold-PF1

;; Make KP7 move by paragraphs, instead of pages.
(define-key SS3-map "w" 'tpu-paragraph)                         ; KP7

;; TPU-edt assumes you have the ispell spelling checker;
;; Un-comment this line if you don't.
;(setq tpu-have-spell nil)

;; Display the TPU-edt version.
(tpu-version)

;; End of .emacs - a sample emacs initialization file

;;    After initialization with the .emacs file shown above, the editing
;;    keys have been re-mapped to look like this:

;;        _______________________    _______________________________
;;       | HELP  |      Do       |  |       |       |       |       |
;;       |KeyDefs|               |  |       |       |       |       |
;;       |_______|_______________|  |_______|_______|_______|_______|
;;        _______________________    _______________________________
;;       | Find  |Insert |Remove |  | Gold  | HELP  |FndNxt | Del L |
;;       |       |       |Sto Tex|  | U Arg |E-Help | Find  |Undel L|
;;       |_______|_______|_______|  |_______|_______|_______|_______|
;;       |Select |Pre Scr|Nex Scr|  |Paragra| Sect  |Append | Del W |
;;       | Reset |Pre Win|Nex Win|  |  Do   | Fill  |Replace|Undel W|
;;       |_______|_______|_______|  |_______|_______|_______|_______|
;;               |Move up|          |Forward|Reverse|Remove | Del C |
;;               |Tscreen|          |Bottom |  Top  |Insert |Undel C|
;;        _______|_______|_______   |_______|_______|_______|_______|
;;       |Mov Lef|Mov Dow|Mov Rig|  | Word  |  EOL  | Char  |       |
;;       |PanLeft|Bscreen|PanRigh|  |ChngCas|Del EOL|SpecIns| Enter |
;;       |_______|_______|_______|  |_______|_______|_______|       |
;;                                  |     Line      |Select | Subs  |
;;                                  |   Open Line   | Reset |       |
;;                                  |_______________|_______|_______|

;;    Astute emacs hackers will realize that on systems where TPU-edt is
;;    installed, this documentation file can be loaded to produce the above
;;    editing keypad layout.  In fact, to get all the changes in the sample
;;    initialization file, you only need a one line initialization file:

;;        (load "tpu-doc")

;;    wow!


;; %% Compiling TPU-edt

;;    It is not necessary to compile (byte-compile in emacs parlance)
;;    TPU-edt to use it.  However, byte-compiled code loads and runs
;;    faster, and takes up less memory when loaded.  To byte compile
;;    TPU-edt, use the following command.

;;        emacs -batch -f batch-byte-compile tpu-edt.el

;;    This will produce a file named tpu-edt.elc.  This new file can be
;;    used in place of the original tpu-edt.el file.  In commands where
;;    the file type is not specified, emacs always attempts to use the
;;    byte-compiled version before resorting to the source.


;; %% Regular expressions in TPU-edt

;;    Gold-* toggles TPU-edt regular expression mode.  In regular expression
;;    mode, find, find next, replace, and substitute accept emacs regular
;;    expressions.  A complete list of emacs regular expressions can be
;;    found using the emacs "info" command (it's somewhat like the VMS help
;;    command).  Try the following sequence of commands:

;;        DO info             <enter info mode>
;;	  m regex             <select the "regular expression" topic>
;;	  m directives        <select the "directives" topic>

;;    Type "q" to quit out of info mode.

;;    There is a problem in regular expression mode when searching for
;;    empty strings, like beginning-of-line (^) and end-of-line ($).
;;    When searching for these strings, find-next may find the current
;;    string, instead of the next one.  This can cause global replace and
;;    substitute commands to loop forever in the same location.  For this
;;    reason, commands like

;;        replace "^" "> "       <add "> " to beginning of line>
;;	  replace "$" "00711"    <add "00711" to end of line>

;;    may not work properly.

;;    Commands like those above are very useful for adding text to the
;;    beginning or end of lines.  They might work on a line-by-line basis,
;;    but go into an infinite loop if the "all" response is specified.  If
;;    the goal is to add a string to the beginning or end of a particular
;;    set of lines TPU-edt provides functions to do this.

;;        Gold-^  Add a string at BOL in region or buffer
;;        Gold-$  Add a string at EOL in region or buffer

;;    There is also a TPU-edt interface to the native emacs string
;;    replacement commands.  Gold-/ invokes this command.  It accepts
;;    regular expressions if TPU-edt is in regular expression mode.  Given
;;    a repeat count, it will perform the replacement without prompting
;;    for confirmation.

;;    This command replaces empty strings correctly, however, it has its
;;    drawbacks.  As a native emacs command, it has a different interface
;;    than the emulated TPU commands.  Also, it works only in the forward
;;    direction, regardless of the current TPU-edt direction.


;; %% Etcetera

;;    That's TPU-edt in a nutshell...

;;    Please send any bug reports, feature requests, or cookies to the
;;    author, Rob Riepel, at the address shown by the tpu-version command
;;    (Gold-V).

;;    Share and enjoy...  Rob Riepel  7/93

;;; tpu-doc.el ends here
