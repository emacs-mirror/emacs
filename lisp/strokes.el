;;; strokes.el --- control Emacs through mouse strokes

;; Copyright (C) 1997, 2000, 2002 Free Software Foundation, Inc.

;; Author: David Bakhash <cadet@alum.mit.edu>
;; Maintainer: FSF
;; Keywords: lisp, mouse, extensions

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is the strokes package.  It is intended to allow the user to
;; control Emacs by means of mouse strokes.  Once strokes is loaded, you
;; can always get help be invoking `strokes-help':

;; > M-x strokes-help

;; and you can learn how to use the package.  A mouse stroke, for now,
;; can be defined as holding the middle button, for instance, and then
;; moving the mouse in whatever pattern you wish, which you have set
;; Emacs to understand as mapping to a given command.  For example, you
;; may wish the have a mouse stroke that looks like a capital `C' which
;; means `copy-region-as-kill'.  Treat strokes just like you do key
;; bindings.  For example, Emacs sets key bindings globally with the
;; `global-set-key' command.  Likewise, you can do

;; > M-x global-set-stroke

;; to interactively program in a stroke.  It would be wise to set the
;; first one to this very command, so that from then on, you invoke
;; `global-set-stroke' with a stroke.  likewise, there may eventually
;; be a `local-set-stroke' command, also analogous to `local-set-key'.

;; You can always unset the last stroke definition with the command

;; > M-x strokes-unset-last-stroke

;; and the last stroke that was added to `strokes-global-map' will be
;; removed.

;; Other analogies between strokes and key bindings are as follows:

;;    1) To describe a stroke binding, you can type

;;       > M-x describe-stroke

;;       analogous to `describe-key'.  It's also wise to have a stroke,
;;       like an `h', for help, or a `?', mapped to `describe-stroke'.

;;    2) stroke bindings are set internally through the Lisp function
;;       `define-stroke', similar to the `define-key' function.  some
;;       examples for a 3x3 stroke grid would be

;;       (define-stroke c-mode-stroke-map
;;                      '((0 . 0) (1 . 1) (2 . 2))
;;                      'kill-region)
;;       (define-stroke strokes-global-map
;;                      '((0 . 0) (0 . 1) (0 . 2) (1 . 2) (2 . 2))
;;                      'list-buffers)

;;       however, if you would probably just have the user enter in the
;;       stroke interactively and then set the stroke to whatever he/she
;;       entered. The Lisp function to interactively read a stroke is
;;       `strokes-read-stroke'.  This is especially helpful when you're
;;       on a fast computer that can handle a 9x9 stroke grid.

;;       NOTE: only global stroke bindings are currently implemented,
;;       however mode- and buffer-local stroke bindings may eventually
;;       be implemented in a future version.

;; The important variables to be aware of for this package are listed
;; below.  They can all be altered through the customizing package via

;; > M-x customize

;; and customizing the group named `strokes'.  You can also read
;; documentation on the variables there.

;; `strokes-minimum-match-score' (determines the threshold of error that
;; makes a stroke acceptable or unacceptable.  If your strokes aren't
;; matching, then you should raise this variable.

;; `strokes-grid-resolution' (determines the grid dimensions that you use
;; when defining/reading strokes.  The finer the grid your computer can
;; handle, the more you can do, but even a 3x3 grid is pretty cool.)
;; The default value (9) should be fine for most decent computers.
;; NOTE: This variable should not be set to a number less than 3.

;; `strokes-display-strokes-buffer' will allow you to hide the strokes
;; buffer when doing simple strokes.  This is a speedup for slow
;; computers as well as people who don't want to see their strokes.

;; If you find that your mouse is accelerating too fast, you can
;; execute the UNIX X command to slow it down.  A good possibility is

;; % xset m 5/4 8

;; which seems, heuristically, to work okay, without much disruption.

;; Whenever you load in the strokes package, you will be able to save
;; what you've done upon exiting Emacs.  You can also do

;; > M-x save-strokes

;; and it will save your strokes in ~/.strokes, or you may wish to change
;; this by setting the variable `strokes-file'.

;; Note that internally, all of the routines that are part of this
;; package are able to deal with complex strokes, as they are a superset
;; of simple strokes.  However, the default of this package will map
;; mouse button2 to the command `strokes-do-stroke', and NOT
;; `strokes-do-complex-stroke'.  If you wish to use complex strokes, you
;; will have to override this key mapping.  Complex strokes are terminated
;; with mouse button3.  The strokes package will not interfere with
;; `mouse-yank', but you may want to examine how this is done (see the
;; variable `strokes-click-command')

;; To get strokes to work as part of your your setup, then you'll have
;; put the strokes package in your load-path (preferably byte-compiled)
;; and then add the following to your .emacs file (or wherever
;; you put Emacs-specific startup preferences):

;;(and window-system
;;     (require 'strokes))

;; Once loaded, you can start stroking.  You can also toggle between
;; strokes mode by simple typing

;; > M-x strokes-mode

;; I am now in the process of porting this package to Emacs.  I also hope
;; that, with the help of others, this package will be useful in entering
;; in pictographic-like language text using the mouse (i.e. Korean).
;; Japanese and Chinese are a bit trickier, but I'm sure that with help
;; it can be done.  The next version will allow the user to enter strokes
;; which "remove the pencil from the paper" so to speak, so one character
;; can have multiple strokes.

;; You can read more about strokes at:

;; http://www.mit.edu/people/cadet/strokes-help.html

;; If you're interested in using strokes for writing English into Emacs
;; using strokes, then you'll want to read about it on the web page above
;; or just download from http://www.mit.edu/people/cadet/strokes-abc.el,
;; which is nothing but a file with some helper commands for inserting
;; alphanumerics and punctuation.

;; Great thanks to Rob Ristroph for his generosity in letting me use
;; his PC to develop this, Jason Johnson for his help in algorithms,
;; Euna Kim for her help in Korean, and massive thanks to the helpful
;; guys on the help instance on athena (zeno, jered, amu, gsstark,
;; ghudson, etc) Special thanks to Steve Baur, Kyle Jones, and Hrvoje
;; Niksic for all their help.  And special thanks to Dave Gillespie
;; for all the elisp help--he is responsible for helping me use the cl
;; macros at (near) max speed.

;; Tasks: (what I'm getting ready for future version)...
;; 2) use 'strokes-read-complex-stroke for korean, etc.
;; 4) buffer-local 'strokes-local-map, and mode-stroke-maps would be nice
;; 6) add some hooks, like `strokes-read-stroke-hook'
;; 7) See what people think of the factory settings.  Should I change
;;    them?  They're all pretty arbitrary in a way.  I guess they
;;    should be minimal, but computers are getting lots faster, and
;;    if I choose the defaults too conservatively, then strokes will
;;    surely disappoint some people on decent machines (until they
;;    figure out M-x customize).  I need feedback.
;; Other: I always have the most beta version of strokes, so if you
;;        want it just let me know.

;;; Code:

;;; Requirements and provisions...

(autoload 'mail-position-on-field "sendmail")
(eval-when-compile (require 'cl))

;;; Constants...

(defconst strokes-version "2.4-Emacs")

(defconst strokes-bug-address "cadet@alum.mit.edu")

(defconst strokes-lift :strokes-lift
  "Symbol representing a stroke lift event for complex strokes.
Complex strokes are those which contain two or more simple strokes.")

(defconst strokes-xpm-header "/* XPM */
static char * stroke_xpm[] = {
/* width height ncolors cpp [x_hot y_hot] */
\"33 33 9 1 26 23\",
/* colors */
\" 	c none s none\",
\"*	c #000000 s foreground\",
\"R	c #FFFF00000000\",
\"O	c #FFFF80000000\",
\"Y	c #FFFFFFFF0000\",
\"G	c #0000FFFF0000\",
\"B	c #00000000FFFF\",
\"P	c #FFFF0000FFFF\",
\".	c #45458B8B0000\",
/* pixels */\n"
  "The header to all xpm buffers created by strokes")

;;; user variables...

(defgroup strokes nil
  "Control Emacs through mouse strokes"
  :link '(url-link "http://www.mit.edu/people/cadet/strokes-help.html")
  :group 'mouse)

(defcustom strokes-modeline-string " Strokes"
  "*Modeline identification when strokes-mode is on \(default is \" Strokes\"\)."
  :type 'string
  :group 'strokes)

(defcustom strokes-character ?@
  "*Character used when drawing strokes in the strokes buffer.
\(The default is `@', which works well.\)"
  :type 'character
  :group 'strokes)

(defcustom strokes-minimum-match-score 1000
  "*Minimum score for a stroke to be considered a possible match.
Setting this variable to 0 would require a perfectly precise match.
The default value is 1000, but it's mostly dependent on how precisely
you manage to replicate your user-defined strokes.  It also depends on
the value of `strokes-grid-resolution', since a higher grid resolution
will correspond to more sample points, and thus more distance
measurements.  Usually, this is not a problem since you first set
`strokes-grid-resolution' based on what your computer seems to be able
to handle (though the defaults are usually more than sufficent), and
then you can set `strokes-minimum-match-score' to something that works
for you.  The only purpose of this variable is to insure that if you
do a bogus stroke that really doesn't match any of the predefined
ones, then strokes should NOT pick the one that came closest."
  :type 'integer
  :group 'strokes)

(defcustom strokes-grid-resolution 9
  "*Integer defining dimensions of the stroke grid.
The grid is a square grid, where STROKES-GRID-RESOLUTION defaults to
`9', making a 9x9 grid whose coordinates go from (0 . 0) on the top
left to ((STROKES-GRID-RESOLUTION - 1) . (STROKES-GRID-RESOLUTION - 1))
on the bottom right.  The greater the resolution, the more intricate
your strokes can be.
NOTE: This variable should be odd and MUST NOT be less than 3 and need
      not be greater than 33, which is the resolution of the pixmaps.
WARNING: Changing the value of this variable will gravely affect the
         strokes you have already programmed in.  You should try to
         figure out what it should be based on your needs and on how
         quick the particular platform(s) you're operating on, and
         only then start programming in your custom strokes."
  :type 'integer
  :group 'strokes)

(defcustom strokes-file (convert-standard-filename "~/.strokes")
  "*File containing saved strokes for stroke-mode (default is ~/.strokes)."
  :type 'file
  :group 'strokes)

(defcustom strokes-buffer-name " *strokes*"
  "The buffer that the strokes take place in (default is ` *strokes*')."
  :type 'string
  :group 'strokes)

(defcustom strokes-use-strokes-buffer t
  "*If non-nil, the strokes buffer is used and strokes are displayed.
If nil, strokes will be read the same, however the user will not be
able to see the strokes.  This be helpful for people who don't like
the delay in switching to the strokes buffer."
  :type 'boolean
  :group 'strokes)

(defcustom strokes-click-command 'mouse-yank-at-click
  "*Command to execute when stroke is actually a `click' event.
This is set to `mouse-yank-at-click' by default."
  :type 'function
  :group 'strokes)

;;; internal variables...

;; This is an internal variable, but we defcustom it so Customize can
;; use it.
;;;###autoload
(defcustom strokes-mode nil
  "Non-nil when `strokes' is globally enabled.
Setting this variable directly does not take effect.  Use either Customize
or M-x strokes-mode."
  :type 'boolean
  :set (lambda (symbol value)
	 (strokes-mode (or value 0)))
  :initialize 'custom-initialize-default
  :require 'strokes
  :version "21.1"
  :group 'strokes)

(defvar strokes-window-configuration nil
  "The special window configuration used when entering strokes.
This is set properly in the function `strokes-update-window-configuration'.")

(defvar strokes-last-stroke nil
  "Last stroke entered by the user.
Its value gets set every time the function
`strokes-fill-stroke' gets called,
since that is the best time to set the variable")

(defvar strokes-global-map '()
  "Association list of strokes and their definitions.
Each entry is (STROKE . COMMAND) where STROKE is itself a list of
coordinates (X . Y) where X and Y are lists of positions on the
normalized stroke grid, with the top left at (0 . 0).  COMMAND is the
corresponding interactive function")

(defvar strokes-load-hook nil
  "Function or functions to be called when `strokes' is loaded.")

;;; ### NOT IMPLEMENTED YET ###
;;(defvar edit-strokes-menu
;;  '("Edit-Strokes"
;;    ["Add stroke..." strokes-global-set-stroke t]
;;    ["Delete stroke..." strokes-edit-delete-stroke t]
;;    ["Change stroke"	strokes-smaller	t]
;;    ["Change definition"	strokes-larger	t]
;;    ["[Re]List Strokes chronologically"	strokes-list-strokes	t]
;;    ["[Re]List Strokes alphabetically"	strokes-list-strokes	t]
;;    ["Quit"		strokes-edit-quit		t]
;;    ))

;;; Macros...

(defmacro strokes-while-inhibiting-garbage-collector (&rest forms)
  "Execute FORMS without interference from the garbage collector."
  `(let ((gc-cons-threshold 134217727))
     ,@forms))

(defsubst strokes-click-p (stroke)
  "Non-nil if STROKE is really click."
  (< (length stroke) 2))

;;; old, but worked pretty good (just in case)...
;;(defmacro strokes-define-stroke (stroke-map stroke def)
;;  "Add STROKE to STROKE-MAP alist with given command DEF"
;;  (list 'if (list '< (list 'length stroke) 2)
;;	(list 'error
;;	      "That's a click, not a stroke.  See `strokes-click-command'")
;;	(list 'setq stroke-map (list 'cons (list 'cons stroke def)
;;				     (list 'remassoc stroke stroke-map)))))

(defsubst strokes-remassoc (key list)
  (let (elt)
    (while (setq elt (assoc key list))
      (setq list (delete elt list))))
  list)

(defmacro strokes-define-stroke (stroke-map stroke def)
  "Add STROKE to STROKE-MAP alist with given command DEF."
  `(if (strokes-click-p ,stroke)
       (error "That's a click, not a stroke; see `strokes-click-command'")
     (setq ,stroke-map (cons (cons ,stroke ,def)
			     (strokes-remassoc ,stroke ,stroke-map)))))

(defalias 'define-stroke 'strokes-define-stroke)

(defsubst strokes-square (x)
  "Returns the square of the number X"
  (* x x))

(defsubst strokes-distance-squared (p1 p2)
  "Gets the distance (squared) between to points P1 and P2.
P1 and P2 are cons cells in the form (X . Y)."
  (let ((x1 (car p1))
	(y1 (cdr p1))
	(x2 (car p2))
	(y2 (cdr p2)))
    (+ (strokes-square (- x2 x1))
       (strokes-square (- y2 y1)))))

;;; Advice for various functions...

;; I'd originally wanted to write a macro that would just take in the
;; generic functions which use mouse button2 in various modes.  Most of
;; them are identical in form: they take an event as the single argument
;; and then do their thing.  I tried writing a macro that looked
;; something like this, but failed.  Advice just ain't that easy.  The
;; one that bugged me the most was `Manual-follow-xref', because that had
;; &rest arguments, and I didn't know how to work around it in defadvice.
;; However, I was able to fix up most of the important modes (i.e. the
;; ones I use all the time).  One `bug' in the program that I just can't
;; seem to figure out is why I can only advise other button2 functions
;; successfully when the variable `strokes-use-strokes-buffer' is nil.  I
;; did all the save-excursion/save-window-excursion stuff SPECIFICALLY so
;; that using the strokes buffer or not would absolutely not affect any
;; other part of the program.  If someone can figure out how to make the
;; following advices work w/ regardless of that variable
;; `strokes-use-strokes-buffer', then that would be a great victory.  If
;; someone out there would be kind enough to make the commented code
;; below work, I'd be grateful.  By the way, I put the `protect' keywords
;; there to insure that if a stroke went bad, then
;; `strokes-click-command' would be set back.  If this isn't necessary,
;; then feel free to let me know.

;; For what follows, I really wanted something that would work like this:

;;(strokes-fix-button2 'vm-mouse-button-2)

;; Or even better, I could have simply done something like:

;;(mapcar 'strokes-fix-button2
;; 	  '(vm-mouse-button-2
;;          rmail-summary-mouse-goto-msg
;;	    <rest of them>))

;;; With help from Hans (author of advice.el)...
(defmacro strokes-fix-button2-command (command)
  "Fix COMMAND so that it can also work with strokes.
COMMAND must take one event argument.
Example of how one might fix up a command that's bound to button2
and which is an interactive funcion of one event argument:

\(strokes-fix-button2-command 'rmail-summary-mouse-goto-msg)"
  (let ((command (eval command)))
    `(progn
       (defadvice ,command (around strokes-fix-button2 compile preactivate)
         ,(format "Fix %s to work with strokes." command)
         (let ((strokes-click-command
                  ',(intern (format "ad-Orig-%s" command))))
             (strokes-do-stroke (ad-get-arg 0)))))))

(defvar strokes-insinuated nil)

(defun strokes-insinuate ()
  "Insinuate Emacs with strokes advices."
  (unless strokes-insinuated
    (strokes-fix-button2-command 'vm-mouse-button-2)
    (strokes-fix-button2-command 'rmail-summary-mouse-goto-msg)
    (strokes-fix-button2-command 'Buffer-menu-mouse-select)
    (strokes-fix-button2-command 'w3-widget-button-click)
    (strokes-fix-button2-command 'widget-image-button-press)
    (strokes-fix-button2-command 'Info-follow-clicked-node)
    (strokes-fix-button2-command 'compile-mouse-goto-error)
    (strokes-fix-button2-command 'gdbsrc-select-or-yank)
    (strokes-fix-button2-command 'hypropos-mouse-get-doc)
    (strokes-fix-button2-command 'gnus-mouse-pick-group)
    (strokes-fix-button2-command 'gnus-mouse-pick-article)
    (strokes-fix-button2-command 'gnus-article-push-button)
    (strokes-fix-button2-command 'dired-mouse-find-file)
    (strokes-fix-button2-command 'url-dired-find-file-mouse)
    (strokes-fix-button2-command 'dired-u-r-mouse-toggle)
    (strokes-fix-button2-command 'dired-u-w-mouse-toggle)
    (strokes-fix-button2-command 'dired-u-x-mouse-toggle)
    (strokes-fix-button2-command 'dired-g-r-mouse-toggle)
    (strokes-fix-button2-command 'dired-g-w-mouse-toggle)
    (strokes-fix-button2-command 'dired-g-x-mouse-toggle)
    (strokes-fix-button2-command 'dired-o-r-mouse-toggle)
    (strokes-fix-button2-command 'dired-o-w-mouse-toggle)
    (strokes-fix-button2-command 'isearch-yank-x-selection)
    (strokes-fix-button2-command 'occur-mode-mouse-goto)
    (strokes-fix-button2-command 'cvs-mouse-find-file)
    (setq strokes-insinuated t)))

;;; I can fix the customize widget button click, but then
;;; people will get confused when they try to customize
;;; strokes with the mouse and customize tells them that
;;; `strokes-click-command' is mapped to `ad-Orig-widget-button-click'
;;(strokes-fix-button2-command 'widget-button-click)

;;; without the advice, each advised function would look like...
;;(defadvice vm-mouse-button-2 (around vm-strokes activate protect)
;;  "Allow strokes to work in VM."
;;  (if strokes-use-strokes-buffer
;;      ;; then strokes is no good and we'll have to use the original
;;      ad-do-it
;;    ;; otherwise, we can make strokes work too...
;;    (let ((strokes-click-command 'ad-Orig-vm-mouse-button-2))
;;      (strokes-do-stroke (ad-get-arg 0)))))

;;; Functions...

(defsubst strokes-mouse-event-p (event)
  (and (consp event) (symbolp (car event))
       (or (eq (car event) 'mouse-movement)
	   (memq 'click (get (car event) 'event-symbol-elements))
	   (memq 'down (get (car event) 'event-symbol-elements))
	   (memq 'drag (get (car event) 'event-symbol-elements)))))

(defsubst strokes-button-press-event-p (event)
  (and (consp event) (symbolp (car event))
       (memq 'down (get (car event) 'event-symbol-elements))))

(defsubst strokes-button-release-event-p (event)
  (and (consp event) (symbolp (car event))
       (or (memq 'click (get (car event) 'event-symbol-elements))
	   (memq 'drag (get (car event) 'event-symbol-elements)))))

(defun strokes-event-closest-point-1 (window &optional line)
  "Return position of start of line LINE in WINDOW.
If LINE is nil, return the last position visible in WINDOW."
  (let* ((total (- (window-height window)
		   (if (window-minibuffer-p window)
		       0 1)))
	 (distance (or line total)))
    (save-excursion
      (goto-char (window-start window))
      (if (= (vertical-motion distance) distance)
	  (if (not line)
	      (forward-char -1)))
      (point))))

(defun strokes-event-closest-point (event &optional start-window)
  "Return the nearest position to where EVENT ended its motion.
This is computed for the window where EVENT's motion started,
or for window WINDOW if that is specified."
  (or start-window (setq start-window (posn-window (event-start event))))
  (if (eq start-window (posn-window (event-end event)))
      (if (eq (posn-point (event-end event)) 'vertical-line)
	  (strokes-event-closest-point-1 start-window
					 (cdr (posn-col-row (event-end event))))
	(if (eq (posn-point (event-end event)) 'mode-line)
	    (strokes-event-closest-point-1 start-window)
	  (posn-point (event-end event))))
    ;; EVENT ended in some other window.
    (let* ((end-w (posn-window (event-end event)))
	   (end-w-top)
	   (w-top (nth 1 (window-edges start-window))))
      (setq end-w-top
	    (if (windowp end-w)
		(nth 1 (window-edges end-w))
	      (/ (cdr (posn-x-y (event-end event)))
		 (frame-char-height end-w))))
      (if (>= end-w-top w-top)
	  (strokes-event-closest-point-1 start-window)
	(window-start start-window)))))

(defun strokes-lift-p (object)
  "Return non-nil if OBJECT is a stroke-lift."
  (eq object strokes-lift))

(defun strokes-unset-last-stroke ()
  "Undo the last stroke definition."
  (interactive)
  (let ((command (cdar strokes-global-map)))
    (if (y-or-n-p
	 (format "really delete last stroke definition, defined to `%s'? "
		 command))
	(progn
	  (setq strokes-global-map (cdr strokes-global-map))
	  (message "That stroke has been deleted"))
      (message "Nothing done"))))

;;;###autoload
(defun strokes-global-set-stroke (stroke command)
  "Interactively give STROKE the global binding as COMMAND.
Operated just like `global-set-key', except for strokes.
COMMAND is a symbol naming an interactively-callable function.  STROKE
is a list of sampled positions on the stroke grid as described in the
documentation for the `strokes-define-stroke' function."
  (interactive
   (list
    (and (or strokes-mode (strokes-mode t))
	 (strokes-read-complex-stroke
	  "Define a new stroke.  Draw with button1 (or 2).  End with button3..."))
    (read-command "command to map stroke to: ")))
  (strokes-define-stroke strokes-global-map stroke command))

;;;###autoload
(defalias 'global-set-stroke 'strokes-global-set-stroke)

;;(defun global-unset-stroke (stroke); FINISH THIS DEFUN!
;;  "delete all strokes matching STROKE from `strokes-global-map',
;; letting the user input
;; the stroke with the mouse"
;;  (interactive
;;   (list
;;    (strokes-read-stroke "Enter the stroke you want to delete...")))
;;  (strokes-define-stroke 'strokes-global-map stroke command))

(defun strokes-get-grid-position (stroke-extent position &optional grid-resolution)
  "Map POSITION to a new grid position based on its STROKE-EXTENT and GRID-RESOLUTION.
STROKE-EXTENT as a list \(\(XMIN . YMIN\) \(XMAX . YMAX\)\).
If POSITION is a `strokes-lift', then it is itself returned.
Optional GRID-RESOLUTION may be used in place of STROKES-GRID-RESOLUTION.
The grid is a square whose dimesion is [0,GRID-RESOLUTION)."
  (cond ((consp position)		; actual pixel location
	 (let ((grid-resolution (or grid-resolution strokes-grid-resolution))
	       (x (car position))
	       (y (cdr position))
	       (xmin (caar stroke-extent))
	       (ymin (cdar stroke-extent))
	       ;; the `1+' is there to insure that the
	       ;; formula evaluates correctly at the boundaries
	       (xmax (1+ (car (cadr stroke-extent))))
	       (ymax (1+ (cdr (cadr stroke-extent)))))
	   (cons (floor (* grid-resolution
			   (/ (float (- x xmin))
			      (- xmax xmin))))
		 (floor (* grid-resolution
			   (/ (float (- y ymin))
			      (- ymax ymin)))))))
	((strokes-lift-p position)	; stroke lift
	 strokes-lift)))

(defun strokes-get-stroke-extent (pixel-positions)
  "From a list of absolute PIXEL-POSITIONS, returns absolute spatial extent.
The return value is a list ((XMIN . YMIN) (XMAX . YMAX))."
  (if pixel-positions
      (let ((xmin (caar pixel-positions))
	    (xmax (caar pixel-positions))
	    (ymin (cdar pixel-positions))
	    (ymax (cdar pixel-positions))
	    (rest (cdr pixel-positions)))
	(while rest
	  (if (consp (car rest))
	      (let ((x (caar rest))
		    (y (cdar rest)))
		(if (< x xmin)
		    (setq xmin x))
		(if (> x xmax)
		    (setq xmax x))
		(if (< y ymin)
		    (setq ymin y))
		(if (> y ymax)
		    (setq ymax y))))
	  (setq rest (cdr rest)))
	(let ((delta-x (- xmax xmin))
	      (delta-y (- ymax ymin)))
	  (if (> delta-x delta-y)
	      (setq ymin (- ymin
			    (/ (- delta-x delta-y)
			       2))
		    ymax (+ ymax
			    (/ (- delta-x delta-y)
			       2)))
	    (setq xmin (- xmin
			  (/ (- delta-y delta-x)
			     2))
		  xmax (+ xmax
			  (/ (- delta-y delta-x)
			     2))))
	  (list (cons xmin ymin)
		(cons xmax ymax))))
    nil))

(defun strokes-eliminate-consecutive-redundancies (entries)
  "Returns a list with no consecutive redundant entries."
  ;; defun a grande vitesse grace a Dave G.
  (loop for element on entries
        if (not (equal (car element) (cadr element)))
        collect (car element)))
;;  (loop for element on entries
;;        nconc (if (not (equal (car el) (cadr el)))
;;                  (list (car el)))))
;; yet another (orig) way of doing it...
;;  (if entries
;;      (let* ((current (car entries))
;;	     (rest (cdr entries))
;;	     (non-redundant-list (list current))
;;	     (next nil))
;;	(while rest
;;	  (setq next (car rest))
;;	  (if (equal current next)
;;	      (setq rest (cdr rest))
;;	    (setq non-redundant-list (cons next non-redundant-list)
;;		  current next
;;		  rest (cdr rest))))
;;	(nreverse non-redundant-list))
;;    nil))

(defun strokes-renormalize-to-grid (positions &optional grid-resolution)
  "Map POSITIONS to a new grid whose dimensions are based on GRID-RESOLUTION.
POSITIONS is a list of positions and stroke-lifts.
Optional GRID-RESOLUTION may be used in place of STROKES-GRID-RESOLUTION.
The grid is a square whose dimesion is [0,GRID-RESOLUTION)."
  (or grid-resolution (setq grid-resolution strokes-grid-resolution))
  (let ((stroke-extent (strokes-get-stroke-extent positions)))
    (mapcar (function
	     (lambda (pos)
	       (strokes-get-grid-position stroke-extent pos grid-resolution)))
	    positions)))

(defun strokes-fill-stroke (unfilled-stroke &optional force)
  "Fill in missing grid locations in the list of UNFILLED-STROKE.
If FORCE is non-nil, then fill the stroke even if it's `stroke-click'.
NOTE: This is where the global variable `strokes-last-stroke' is set."
  (setq strokes-last-stroke		; this is global
	(if (and (strokes-click-p unfilled-stroke)
		 (not force))
	    unfilled-stroke
	  (loop for grid-locs on unfilled-stroke
		nconc (let* ((current (car grid-locs))
			     (current-is-a-point-p (consp current))
			     (next (cadr grid-locs))
			     (next-is-a-point-p (consp next))
			     (both-are-points-p (and current-is-a-point-p
						     next-is-a-point-p))
			     (x1 (and current-is-a-point-p
				      (car current)))
			     (y1 (and current-is-a-point-p
				      (cdr current)))
			     (x2 (and next-is-a-point-p
				      (car next)))
			     (y2 (and next-is-a-point-p
				      (cdr next)))
			     (delta-x (and both-are-points-p
					   (- x2 x1)))
			     (delta-y (and both-are-points-p
					   (- y2 y1)))
			     (slope (and both-are-points-p
					 (if (zerop delta-x)
					     nil ; undefined vertical slope
					   (/ (float delta-y)
					      delta-x)))))
			(cond ((not both-are-points-p)
			       (list current))
			      ((null slope) ; undefinded vertical slope
			       (if (>= delta-y 0)
				   (loop for y from y1 below y2
					 collect (cons x1 y))
				 (loop for y from y1 above y2
				       collect (cons x1 y))))
			      ((zerop slope) ; (= y1 y2)
			       (if (>= delta-x 0)
				   (loop for x from x1 below x2
					 collect (cons x y1))
				 (loop for x from x1 above x2
				       collect (cons x y1))))
			      ((>= (abs delta-x) (abs delta-y))
			       (if (> delta-x 0)
				   (loop for x from x1 below x2
					 collect (cons x
						       (+ y1
							  (round (* slope
								    (- x x1))))))
				 (loop for x from x1 above x2
				       collect (cons x
						     (+ y1
							(round (* slope
								  (- x x1))))))))
			      (t	; (< (abs delta-x) (abs delta-y))
			       (if (> delta-y 0)
				   (loop for y from y1 below y2
					 collect (cons (+ x1
							  (round (/ (- y y1)
								    slope)))
						       y))
				 (loop for y from y1 above y2
				       collect (cons (+ x1
							(round (/ (- y y1)
								  slope)))
						     y))))))))))

(defun strokes-rate-stroke (stroke1 stroke2)
  "Rates STROKE1 with STROKE2 and returns a score based on a distance metric.
Note: the rating is an error rating, and therefore, a return of 0
represents a perfect match.  Also note that the order of stroke
arguments is order-independent for the algorithm used here."
  (if (and stroke1 stroke2)
      (let ((rest1 (cdr stroke1))
	    (rest2 (cdr stroke2))
	    (err (strokes-distance-squared (car stroke1)
					   (car stroke2))))
	(while (and rest1 rest2)
	  (while (and (consp (car rest1))
		      (consp (car rest2)))
	    (setq err (+ err
			 (strokes-distance-squared (car rest1)
						   (car rest2)))
		  stroke1 rest1
		  stroke2 rest2
		  rest1 (cdr stroke1)
		  rest2 (cdr stroke2)))
	  (cond ((and (strokes-lift-p (car rest1))
		      (strokes-lift-p (car rest2)))
		 (setq rest1 (cdr rest1)
		       rest2 (cdr rest2)))
		((strokes-lift-p (car rest2))
		 (while (consp (car rest1))
		   (setq err (+ err
				(strokes-distance-squared (car rest1)
							  (car stroke2)))
			 rest1 (cdr rest1))))
		((strokes-lift-p (car rest1))
		 (while (consp (car rest2))
		   (setq err (+ err
				(strokes-distance-squared (car stroke1)
							  (car rest2)))
			 rest2 (cdr rest2))))))
	(if (null rest2)
	    (while (consp (car rest1))
	      (setq err (+ err
			   (strokes-distance-squared (car rest1)
						     (car stroke2)))
		    rest1 (cdr rest1))))
	(if (null rest1)
	    (while (consp (car rest2))
	      (setq err (+ err
			   (strokes-distance-squared (car stroke1)
						     (car rest2)))
		    rest2 (cdr rest2))))
	(if (or (strokes-lift-p (car rest1))
		(strokes-lift-p (car rest2)))
	    (setq err nil)
	  err))
    nil))

(defun strokes-match-stroke (stroke stroke-map)
  "Finds the best matching command of STROKE in STROKE-MAP.
Returns the corresponding match as (COMMAND . SCORE)."
  (if (and stroke stroke-map)
      (let ((score (strokes-rate-stroke stroke (caar stroke-map)))
	    (command (cdar stroke-map))
	    (map (cdr stroke-map)))
	(while map
	  (let ((newscore (strokes-rate-stroke stroke (caar map))))
	    (if (or (and newscore score (< newscore score))
		    (and newscore (null score)))
		(setq score newscore
		      command (cdar map)))
	    (setq map (cdr map))))
	(if score
	    (cons command score)
	  nil))
    nil))

;;;###autoload
(defun strokes-read-stroke (&optional prompt event)
  "Read a simple stroke (interactively) and return the stroke.
Optional PROMPT in minibuffer displays before and during stroke reading.
This function will display the stroke interactively as it is being
entered in the strokes buffer if the variable
`strokes-use-strokes-buffer' is non-nil.
Optional EVENT is acceptable as the starting event of the stroke"
  (save-excursion
    (let ((pix-locs nil)
	  (grid-locs nil)
	  (safe-to-draw-p nil))
      (if strokes-use-strokes-buffer
	  ;; switch to the strokes buffer and
	  ;; display the stroke as it's being read
	  (save-window-excursion
	    (set-window-configuration strokes-window-configuration)
	    (when prompt
	      (message prompt)
	      (setq event (read-event))
	      (or (strokes-button-press-event-p event)
		  (error "You must draw with the mouse")))
	    (unwind-protect
		(track-mouse
		  (or event (setq event (read-event)
				  safe-to-draw-p t))
		  (while (not (strokes-button-release-event-p event))
		    (if (strokes-mouse-event-p event)
			(let ((point (strokes-event-closest-point event)))
			  (if (and point safe-to-draw-p)
			      ;; we can draw that point
			      (progn
				(goto-char point)
				(subst-char-in-region point (1+ point) ?\  strokes-character))
			    ;; otherwise, we can start drawing the next time...
			    (setq safe-to-draw-p t))
			  (push (cdr (mouse-pixel-position))
				pix-locs)))
		    (setq event (read-event)))))
	    ;; protected
	    ;; clean up strokes buffer and then bury it.
	    (when (equal (buffer-name) strokes-buffer-name)
	      (subst-char-in-region (point-min) (point-max) strokes-character ?\ )
	      (goto-char (point-min))
	      (bury-buffer))))
      ;; Otherwise, don't use strokes buffer and read stroke silently
      (when prompt
	(message prompt)
	(setq event (read-event))
	(or (strokes-button-press-event-p event)
	    (error "You must draw with the mouse")))
      (track-mouse
	(or event (setq event (read-event)))
	(while (not (strokes-button-release-event-p event))
	  (if (strokes-mouse-event-p event)
	      (push (cdr (mouse-pixel-position))
		    pix-locs))
	  (setq event (read-event))))
      (setq grid-locs (strokes-renormalize-to-grid (nreverse pix-locs)))
      (strokes-fill-stroke (strokes-eliminate-consecutive-redundancies grid-locs)))))

;;;###autoload
(defun strokes-read-complex-stroke (&optional prompt event)
  "Read a complex stroke (interactively) and return the stroke.
Optional PROMPT in minibuffer displays before and during stroke reading.
Note that a complex stroke allows the user to pen-up and pen-down.  This
is implemented by allowing the user to paint with button1 or button2 and
then complete the stroke with button3.
Optional EVENT is acceptable as the starting event of the stroke"
  (save-excursion
    (save-window-excursion
      (set-window-configuration strokes-window-configuration)
      (let ((pix-locs nil)
	    (grid-locs nil))
	(if prompt
	    (while (not (strokes-button-press-event-p event))
	      (message prompt)
	      (setq event (read-event))))
	(unwind-protect
	    (track-mouse
	      (or event (setq event (read-event)))
	      (while (not (and (strokes-button-press-event-p event)
			       (eq 'mouse-3
				   (car (get (car event)
					     'event-symbol-elements)))))
		(while (not (strokes-button-release-event-p event))
		  (if (strokes-mouse-event-p event)
		      (let ((point (strokes-event-closest-point event)))
			(when point
			  (goto-char point)
			  (subst-char-in-region point (1+ point) ?\ strokes-character))
			(push (cdr (mouse-pixel-position))
			      pix-locs)))
		  (setq event (read-event)))
		(push strokes-lift pix-locs)
		(while (not (strokes-button-press-event-p event))
		  (setq event (read-event))))
	      ;; ### KLUDGE! ### sit and wait
	      ;; for some useless event to
	      ;; happen to fix the minibuffer bug.
	      (while (not (strokes-button-release-event-p (read-event))))
	      (setq pix-locs (nreverse (cdr pix-locs))
		    grid-locs (strokes-renormalize-to-grid pix-locs))
	      (strokes-fill-stroke
	       (strokes-eliminate-consecutive-redundancies grid-locs)))
	  ;; protected
	  (when (equal (buffer-name) strokes-buffer-name)
	    (subst-char-in-region (point-min) (point-max) strokes-character ?\ )
	    (goto-char (point-min))
	    (bury-buffer)))))))

(defun strokes-execute-stroke (stroke)
  "Given STROKE, execute the command which corresponds to it.
The command will be executed provided one exists for that stroke,
based on the variable `strokes-minimum-match-score'.
If no stroke matches, nothing is done and return value is nil."
  (let* ((match (strokes-match-stroke stroke strokes-global-map))
	 (command (car match))
	 (score (cdr match)))
    (cond ((strokes-click-p stroke)
	   ;; This is the case of a `click' type event.
	   ;; The `sit-for' is a minor frob that has to do with timing
	   ;; problems.  Without the `sit-for', mouse-yank will not
	   ;; yank at the proper location if the user opted for
	   ;; mouse-yank-at-point to be nil (i.e. mouse-yank takes
	   ;; place at pointer position).  The sit-for tells redisplay
	   ;; to be sure to wait for the `*strokes*' buffer to vanish
	   ;; from consideration when deciding on a point to be used
	   ;; for mouse-yank.
	   (sit-for 0)
	   (command-execute strokes-click-command))
	  ((and match (<= score strokes-minimum-match-score))
	   (message "%s" command)
	   (command-execute command))
	  ((null strokes-global-map)
	   (if (file-exists-p strokes-file)
	       (and (y-or-n-p
		     (format "No strokes loaded.  Load `%s'? "
			     strokes-file))
		    (strokes-load-user-strokes))
	     (error "No strokes defined; use `global-set-stroke'")))
	  (t
	   (error
	    "No stroke matches; see variable `strokes-minimum-match-score'")
	   nil))))

;;;###autoload
(defun strokes-do-stroke (event)
  "Read a simple stroke from the user and then execute its command.
This must be bound to a mouse event."
  (interactive "e")
  (or strokes-mode (strokes-mode t))
  (strokes-execute-stroke (strokes-read-stroke nil event)))

;;;###autoload
(defun strokes-do-complex-stroke (event)
  "Read a complex stroke from the user and then execute its command.
This must be bound to a mouse event."
  (interactive "e")
  (or strokes-mode (strokes-mode t))
  (strokes-execute-stroke (strokes-read-complex-stroke nil event)))

;;;###autoload
(defun strokes-describe-stroke (stroke)
  "Displays the command which STROKE maps to, reading STROKE interactively."
  (interactive
   (list
    (strokes-read-complex-stroke
     "Enter stroke to describe; end with button3...")))
  (let* ((match (strokes-match-stroke stroke strokes-global-map))
	 (command (or (and (strokes-click-p stroke)
			   strokes-click-command)
		      (car match)))
	 (score (cdr match)))
    (if (or (and match
		 (<= score strokes-minimum-match-score))
	    (and (strokes-click-p stroke)
		 strokes-click-command))
	(message "That stroke maps to `%s'" command)
      (message "That stroke is undefined"))
    (sleep-for 1)))			; helpful for recursive edits

;;;###autoload
(defalias 'describe-stroke 'strokes-describe-stroke)

;;;###autoload
(defun strokes-help ()
  "Get instructional help on using the `strokes' package."
  (interactive)
  (let ((helpdoc
	 "This is help for the strokes package.

If you find something wrong with strokes, or feel that it can be
improved in some way, then please feel free to email me:

David Bakhash <cadet@mit.edu>

or just do

M-x strokes-report-bug

------------------------------------------------------------

** Strokes...

The strokes package allows you to define strokes, made with
the mouse or other pointer device, that Emacs can interpret as
corresponding to commands, and then executes the commands.  It does
character recognition, so you don't have to worry about getting it
right every time.

Strokes also allows you to compose documents graphically.  You can
fully edit documents in Chinese, Japanese, etc. based on Emacs
strokes.  Once you've done so, you can ascii compress-and-encode them
and then safely save them for later use, send letters to friends
\(using Emacs, of course).  Strokes will later decode these documents,
extracting the strokes for editing use once again, so the editing
cycle can continue.

Strokes are easy to program and fun to use.  To start strokes going,
you'll want to put the following line in your .emacs file as mentioned
in the commentary to strokes.el.

This will load strokes when and only when you start Emacs on a window
system, with a mouse or other pointer device defined.

To toggle strokes-mode, you just do

> M-x strokes-mode

** Strokes for controlling the behavior of Emacs...

When you're ready to start defining strokes, just use the command

> M-x global-set-stroke

You will see a ` *strokes*' buffer which is waiting for you to enter in
your stroke.  When you enter in the stroke, you draw with button1 or
button2, and then end with button3.  Next, you enter in the command
which will be executed when that stroke is invoked.  Simple as that.
For now, try to define a stroke to copy a region.  This is a popular
edit command, so type

> M-x global-set-stroke

Then, in the ` *strokes*' buffer, draw the letter `C' (for `copy')
and then, when it asks you to enter the command to map that to, type

> copy-region-as-kill

That's about as hard as it gets.
Remember: paint with button1 or button2 and then end with button3.

If ever you want to know what a certain strokes maps to, then do

> M-x describe-stroke

and you can enter in any arbitrary stroke.  Remember: The strokes
package lets you program in simple and complex (multi-lift) strokes.
The only difference is how you *invoke* the two.  You will most likely
use simple strokes, as complex strokes were developed for
Chinese/Japanese/Korean.  So the middle mouse button (mouse-2) will
invoke the command `strokes-do-stroke' in buffers where button2 doesn't
already have a meaning other than its original, which is `mouse-yank'.
But don't worry: `mouse-yank' will still work with strokes.  See the
variable `strokes-click-command'.

If ever you define a stroke which you don't like, then you can unset
it with the command

> M-x strokes-unset-last-stroke

You can always get an idea of what your current strokes look like with
the command

> M-x strokes-list-strokes

Your strokes will be displayed in alphabetical order (based on command
names) and the beginning of each simple stroke will be marked by a
color dot.  Since you may have several simple strokes in a complex
stroke, the dot colors are arranged in the rainbow color sequence,
`ROYGBIV'.  If you want a listing of your strokes from most recent
down, then use a prefix argument:

> C-u M-x strokes-list-strokes

Your strokes are stored as you enter them.  They get saved in a file
called ~/.strokes, along with other strokes configuration variables.
You can change this location by setting the variable `strokes-file'.
You will be prompted to save them when you exit Emacs, or you can save
them with

> M-x save-strokes

Your strokes get loaded automatically when you enable `strokes-mode'.
You can also load in your user-defined strokes with

> M-x load-user-strokes

** Strokes for pictographic editing...

If you'd like to create graphical files with strokes, you'll have to
be running a version of Emacs with XPM support.  You use the
binding C-mouse-2 to start drawing your strokes.  These are just
complex strokes, and thus you continue drawing with mouse-1 or mouse-2 and
end with mouse-3-3.  Then the stroke image gets inserted into the
buffer.  You treat it like any other character, which you can copy,
paste, delete, move, etc.  The command which is bound to C-mouse-2 is
called `strokes-compose-complex-stroke'.  When all is done, you may
want to send the file, or save it.  This is done with

> M-x strokes-encode-buffer

Likewise, to decode the strokes from a strokes-encoded buffer you do

> M-x strokes-decode-buffer

** A few more important things...

o The command `strokes-do-complex-stroke' is invoked with M-mouse-2,
  so that you can execute complex strokes (i.e. with more than one lift)
  if preferred.

o Strokes are a bit computer-dependent in that they depend somewhat on
  the speed of the computer you're working on.  This means that you
  may have to tweak some variables.  You can read about them in the
  commentary of `strokes.el'.  Better to just use apropos and read their
  docstrings.  All variables/functions start with `strokes'.  The one
  variable which many people wanted to see was
  `strokes-use-strokes-buffer' which allows the user to use strokes
  silently--without displaying the strokes.  All variables can be set
  by customizing the group named `strokes' via the customization package:

  > M-x customize"))
    (with-output-to-temp-buffer "*Help with Strokes*"
      (princ helpdoc)
      (set-buffer standard-output)
      (help-mode)
      (print-help-return-message))))

(defun strokes-report-bug ()
  "Submit a bug report for strokes."
  (interactive)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     strokes-bug-address "Strokes"
     (cons
      'strokes-version
      (nconc
       (mapcar
	'intern
	(sort
	 (let (completion-ignore-case)
	   (all-completions "strokes-" obarray 'user-variable-p))
	 'string-lessp))
       (list 'reporter-version)))
     (function
      (lambda ()
	(save-excursion
	  (mail-position-on-field "subject")
	  (beginning-of-line)
	  (skip-chars-forward "^:\n")
	  (if (looking-at ": Strokes;")
	      (progn
		(goto-char (match-end 0))
		(delete-char -1)
		(insert " " strokes-version " bug:")))))))))

(defsubst strokes-fill-current-buffer-with-whitespace ()
  "Erase the contents of the current buffer and fill it with whitespace."
  (erase-buffer)
  (loop repeat (frame-height) do
	(insert-char ?\  (1- (frame-width)))
	(newline))
  (goto-char (point-min)))

(defun strokes-window-configuration-changed-p ()
  "Non-nil if the `strokes-window-configuration' frame properties changed.
This is based on the last time the `strokes-window-configuration was updated."
  (compare-window-configurations (current-window-configuration)
				 strokes-window-configuration))

(defun strokes-update-window-configuration ()
  "Ensure that `strokes-window-configuration' is up-to-date."
  (interactive)
  (let ((current-window (selected-window)))
    (cond ((or (window-minibuffer-p current-window)
	       (window-dedicated-p current-window))
	   ;; don't try to update strokes window configuration
	   ;; if window is dedicated or a minibuffer
	   nil)
	  ((or (interactive-p)
	       (not (buffer-live-p (get-buffer strokes-buffer-name)))
	       (null strokes-window-configuration))
	   ;; create `strokes-window-configuration' from scratch...
	   (save-excursion
	     (save-window-excursion
	       (get-buffer-create strokes-buffer-name)
	       (set-window-buffer current-window strokes-buffer-name)
	       (delete-other-windows)
	       (fundamental-mode)
	       (auto-save-mode 0)
	       (if (featurep 'font-lock)
		   (font-lock-mode 0))
	       (abbrev-mode 0)
	       (buffer-disable-undo (current-buffer))
	       (setq truncate-lines nil)
	       (strokes-fill-current-buffer-with-whitespace)
	       (setq strokes-window-configuration (current-window-configuration))
	       (bury-buffer))))
	  ((strokes-window-configuration-changed-p) ; simple update
	   ;; update the strokes-window-configuration for this
	   ;; specific frame...
	   (save-excursion
	     (save-window-excursion
	       (set-window-buffer current-window strokes-buffer-name)
	       (delete-other-windows)
	       (strokes-fill-current-buffer-with-whitespace)
	       (setq strokes-window-configuration (current-window-configuration))
	       (bury-buffer)))))))

;;;###autoload
(defun strokes-load-user-strokes ()
  "Load user-defined strokes from file named by `strokes-file'."
  (interactive)
  (cond ((and (file-exists-p strokes-file)
	      (file-readable-p strokes-file))
	 (load-file strokes-file))
	((interactive-p)
	 (error "Trouble loading user-defined strokes; nothing done"))
	(t
	 (message "No user-defined strokes, sorry"))))

;;;###autoload
(defalias 'load-user-strokes 'strokes-load-user-strokes)

(defun strokes-prompt-user-save-strokes ()
  "Save user-defined strokes to file named by `strokes-file'."
  (interactive)
  (save-excursion
    (let ((current strokes-global-map))
      (unwind-protect
	  (progn
	    (setq strokes-global-map nil)
	    (strokes-load-user-strokes)
	    (if (and (not (equal current strokes-global-map))
		     (or (interactive-p)
			 (yes-or-no-p "Save your strokes? ")))
		(progn
		  (require 'pp)		; pretty-print variables
		  (message "Saving strokes in %s..." strokes-file)
		  (get-buffer-create "*saved-strokes*")
		  (set-buffer "*saved-strokes*")
		  (erase-buffer)
		  (emacs-lisp-mode)
		  (goto-char (point-min))
		  (insert
		   ";;   -*- Syntax: Emacs-Lisp; Mode: emacs-lisp -*-\n")
		  (insert (format ";;; saved strokes for %s, as of %s\n\n"
				  (user-full-name)
				  (format-time-string "%B %e, %Y" nil)))
		  (message "Saving strokes in %s..." strokes-file)
		  (insert (format "(setq strokes-global-map '%s)"
				  (pp current)))
		  (message "Saving strokes in %s..." strokes-file)
		  (indent-region (point-min) (point-max) nil)
		  (write-region (point-min)
				(point-max)
				strokes-file))
	      (message "(no changes need to be saved)")))
	;; protected
	(if (get-buffer "*saved-strokes*")
	    (kill-buffer (get-buffer "*saved-strokes*")))
	(setq strokes-global-map current)))))

(defalias 'save-strokes 'strokes-prompt-user-save-strokes)

(defun strokes-toggle-strokes-buffer (&optional arg)
  "Toggle the use of the strokes buffer.
In other words, toggle the variabe `strokes-use-strokes-buffer'.
With ARG, use strokes buffer if and only if ARG is positive or true.
Returns value of `strokes-use-strokes-buffer'."
  (interactive "P")
  (setq strokes-use-strokes-buffer
	(if arg (> (prefix-numeric-value arg) 0)
	  (not strokes-use-strokes-buffer))))

(defun strokes-xpm-for-stroke (&optional stroke bufname b/w-only)
  "Create an xpm pixmap for the given STROKE in buffer ` *strokes-xpm*'.
If STROKE is not supplied, then `strokes-last-stroke' will be used.
Optional BUFNAME to name something else.
The pixmap will contain time information via rainbow dot colors
where each individual strokes begins.
Optional B/W-ONLY non-nil will create a mono pixmap, not intended
for trying to figure out the order of strokes, but rather for reading
the stroke as a character in some language."
  (interactive)
  (save-excursion
    (let ((buf (get-buffer-create (or bufname " *strokes-xpm*")))
	  (stroke (strokes-eliminate-consecutive-redundancies
		   (strokes-fill-stroke
		    (strokes-renormalize-to-grid (or stroke
						     strokes-last-stroke)
						 31))))
	  (lift-flag t)
	  (rainbow-chars (list ?R ?O ?Y ?G ?B ?P))) ; ROYGBIV w/o indigo
      (set-buffer buf)
      (erase-buffer)
      (insert strokes-xpm-header)
      (loop repeat 33 do
	    (insert ?\")
	    (insert-char ?\  33)
	    (insert "\",")
	    (newline)
	    finally
	    (forward-line -1)
	    (end-of-line)
	    (insert "}\n"))
      (loop for point in stroke
	    for x = (car-safe point)
	    for y = (cdr-safe point) do
	    (cond ((consp point)
		   ;; draw a point, and possibly a starting-point
		   (if (and lift-flag (not b/w-only))
		       ;; mark starting point with the appropriate color
		       (let ((char (or (car rainbow-chars) ?\.)))
			 (loop for i from 0 to 2 do
			       (loop for j from 0 to 2 do
				     (goto-line (+ 16 i y))	
				     (forward-char (+ 1 j x))
				     (delete-char 1)
				     (insert char)))
			 (setq rainbow-chars (cdr rainbow-chars)
			       lift-flag nil))
		     ;; Otherwise, just plot the point...
		     (goto-line (+ 17 y))	
		     (forward-char (+ 2 x))	
		     (subst-char-in-region (point) (1+ (point)) ?\  ?\*)))
		  ((strokes-lift-p point)
		   ;; a lift--tell the loop to X out the next point...
		   (setq lift-flag t))))
      (when (interactive-p)
	(pop-to-buffer " *strokes-xpm*")
	;;	(xpm-mode 1)
	(goto-char (point-min))
	(put-image (create-image (buffer-string) 'xpm t :ascent 100)
		   (line-end-position))))))

;;; Strokes Edit stuff... ### NOT IMLEMENTED YET ###

;;(defun strokes-edit-quit ()
;;  (interactive)
;;  (or (one-window-p t 0)
;;      (delete-window))
;;  (kill-buffer "*Strokes List*"))

;;(define-derived-mode edit-strokes-mode list-mode
;;  "Edit-Strokes"
;;  "Major mode for `edit-strokes' and `list-strokes' buffers.

;;Editing commands:

;;\\{edit-strokes-mode-map}"
;;  (setq truncate-lines nil
;;	auto-show-mode nil		; don't want problems here either
;;	mode-popup-menu edit-strokes-menu) ; what about extent-specific stuff?
;;  (and (featurep 'menubar)
;;       current-menubar
;;       (set (make-local-variable 'current-menubar)
;;	    (copy-sequence current-menubar))
;;       (add-submenu nil edit-strokes-menu)))

;;(let ((map edit-strokes-mode-map))
;;  (define-key map "<" 'beginning-of-buffer)
;;  (define-key map ">" 'end-of-buffer)
;;  ;;  (define-key map "c" 'strokes-copy-other-face)
;;  ;;  (define-key map "C" 'strokes-copy-this-face)
;;  ;;  (define-key map "s" 'strokes-smaller)
;;  ;;  (define-key map "l" 'strokes-larger)
;;  ;;  (define-key map "b" 'strokes-bold)
;;  ;;  (define-key map "i" 'strokes-italic)
;;  (define-key map "e" 'strokes-list-edit)
;;  ;;  (define-key map "f" 'strokes-font)
;;  ;;  (define-key map "u" 'strokes-underline)
;;  ;;  (define-key map "t" 'strokes-truefont)
;;  ;;  (define-key map "F" 'strokes-foreground)
;;  ;;  (define-key map "B" 'strokes-background)
;;  ;;  (define-key map "D" 'strokes-doc-string)
;;  (define-key map "a" 'strokes-global-set-stroke)
;;  (define-key map "d" 'strokes-list-delete-stroke)
;;  ;;  (define-key map "n" 'strokes-list-next)
;;  ;;  (define-key map "p" 'strokes-list-prev)
;;  ;;  (define-key map " " 'strokes-list-next)
;;  ;;  (define-key map "\C-?" 'strokes-list-prev)
;;  (define-key map "g" 'strokes-list-strokes) ; refresh display
;;  (define-key map "q" 'strokes-edit-quit)
;;  (define-key map [(control c) (control c)] 'bury-buffer))

;;;;;###autoload
;;(defun strokes-edit-strokes (&optional chronological strokes-map)
;;  ;; ### DEAL WITH THE 2nd ARGUMENT ISSUE! ###
;;  "Edit strokes in a pop-up buffer containing strokes and their definitions.
;;If STROKES-MAP is not given, `strokes-global-map' will be used instead.

;;Editing commands:

;;\\{edit-faces-mode-map}"
;;  (interactive "P")
;;  (pop-to-buffer (get-buffer-create "*Strokes List*"))
;;  (reset-buffer (current-buffer))	; handy function from minibuf.el
;;  (setq strokes-map (or strokes-map
;;			strokes-global-map
;;			(progn
;;			  (strokes-load-user-strokes)
;;			  strokes-global-map)))
;;  (or chronological
;;      (setq strokes-map (sort (copy-sequence strokes-map)
;;			      'strokes-alphabetic-lessp)))
;;  ;;  (push-window-configuration)
;;  (insert
;;   "Command                                     Stroke\n"
;;   "-------                                     ------")
;;  (loop for def in strokes-map
;;	for i from 0 to (1- (length strokes-map)) do
;;	(let ((stroke (car def))
;;	      (command-name (symbol-name (cdr def))))
;;	  (strokes-xpm-for-stroke stroke " *strokes-xpm*")
;;	  (newline 2)
;;	  (insert-char ?\  45)
;;	  (beginning-of-line)
;;	  (insert command-name)
;;	  (beginning-of-line)
;;	  (forward-char 45)
;;	  (set (intern (format "strokes-list-annotation-%d" i))
;;	       (make-annotation (make-glyph
;;				 (list
;;				  (vector 'xpm
;;					  :data (buffer-substring
;;						 (point-min " *strokes-xpm*")
;;						 (point-max " *strokes-xpm*")
;;						 " *strokes-xpm*"))
;;				  [string :data "[Stroke]"]))
;;				(point) 'text))
;;	  (set-annotation-data (symbol-value (intern (format "strokes-list-annotation-%d" i)))
;;			       def))
;;	finally do (kill-region (1+ (point)) (point-max)))
;;  (edit-strokes-mode)
;;  (goto-char (point-min)))

;;;;;###autoload
;;(defalias 'edit-strokes 'strokes-edit-strokes)

(eval-when-compile (defvar view-mode-map))

;;;###autoload
(defun strokes-list-strokes (&optional chronological strokes-map)
  "Pop up a buffer containing an alphabetical listing of strokes in STROKES-MAP.
With CHRONOLOGICAL prefix arg \(\\[universal-argument]\) list strokes
chronologically by command name.
If STROKES-MAP is not given, `strokes-global-map' will be used instead."
  (interactive "P")
  (setq strokes-map (or strokes-map
			strokes-global-map
			(progn
			  (strokes-load-user-strokes)
			  strokes-global-map)))
  (if (not chronological)
      ;; then alphabetize the strokes based on command names...
      (setq strokes-map (sort (copy-sequence strokes-map)
			      (function strokes-alphabetic-lessp))))
  (let ((config (current-window-configuration)))
    (set-buffer (get-buffer-create "*Strokes List*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert
     "Command                                     Stroke\n"
     "-------                                     ------")
    (loop for def in strokes-map do
	  (let ((stroke (car def))
		(command-name (symbol-name (cdr def))))
	    (strokes-xpm-for-stroke stroke " *strokes-xpm*")
	    (newline 2)
	    (insert-char ?\  45)
	    (beginning-of-line)
	    (insert command-name)
	    (beginning-of-line)
	    (forward-char 45)
	    (insert-image (create-image (with-current-buffer " *strokes-xpm*"
					  (buffer-string))
					'xpm t)))
	  finally do (kill-region (1+ (point)) (point-max)))
    (view-buffer "*Strokes List*" nil)
    (set (make-local-variable 'view-mode-map)
	 (let ((map (copy-keymap view-mode-map)))
	   (define-key map "q" `(lambda ()
				  (interactive)
				  (View-quit)
				  (set-window-configuration ,config)))
	   map))
    (goto-char (point-min))))

(defun strokes-alphabetic-lessp (stroke1 stroke2)
  "T iff command name for STROKE1 is less than STROKE2's in lexicographic order."
  (let ((command-name-1 (symbol-name (cdr stroke1)))
	(command-name-2 (symbol-name (cdr stroke2))))
    (string-lessp command-name-1 command-name-2)))

;;;###autoload
(defun strokes-mode (&optional arg)
  "Toggle strokes being enabled.
With ARG, turn strokes on if and only if ARG is positive or true.
Note that `strokes-mode' is a global mode.  Think of it as a minor
mode in all buffers when activated.
By default, strokes are invoked with mouse button-2.  You can define
new strokes with

> M-x global-set-stroke

To use strokes for pictographic editing, such as Chinese/Japanese, use
S-mouse-2, which draws strokes and inserts them.  Encode/decode your
strokes with

> M-x strokes-encode-buffer
> M-x strokes-decode-buffer"
  (interactive "P")
  (let ((on-p (if arg
		  (> (prefix-numeric-value arg) 0)
		(not strokes-mode))))
    (cond ((not (display-mouse-p))
	   (error "Can't use strokes without a mouse"))
	  (on-p				; turn on strokes
	   (strokes-insinuate)		; make sure defadvices are set
	   (and (file-exists-p strokes-file)
		(null strokes-global-map)
		(strokes-load-user-strokes))
	   (add-hook 'kill-emacs-query-functions
		     'strokes-prompt-user-save-strokes)
	   (add-hook 'select-frame-hook
		     'strokes-update-window-configuration)
	   (strokes-update-window-configuration)
	   (define-key global-map [(down-mouse-2)] 'strokes-do-stroke)
	   (define-key global-map [(meta down-mouse-2)]
	     'strokes-do-complex-stroke)
	   ;;	   (define-key global-map [(control down-mouse-2)] 'strokes-do-complex-stroke)
	   (ad-activate-regexp "^strokes-") ; advise button2 commands
	   (setq strokes-mode t))
	  (t				; turn off strokes
	   (if (get-buffer strokes-buffer-name)
	       (kill-buffer (get-buffer strokes-buffer-name)))
	   (remove-hook 'select-frame-hook
			'strokes-update-window-configuration)
	   (if (string-match "^strokes-" (symbol-name (key-binding [(down-mouse-2)])))
	       (define-key global-map [(down-mouse-2)] strokes-click-command))
	   (if (string-match "^strokes-" (symbol-name (key-binding [(meta down-mouse-2)])))
	       (global-unset-key [(meta down-mouse-2)]))
	   (if (string-match "^strokes-" (symbol-name (key-binding [(control down-mouse-2)])))
	       (global-unset-key [(control down-mouse-2)]))
	   ;;	   (if (string-match "^strokes-" (symbol-name (key-binding [(shift button2)])))	
	   ;;	       (global-unset-key [(shift button2)]))
	   (ad-deactivate-regexp "^strokes-") ; unadvise strokes-button2 commands
	   (setq strokes-mode nil))))
  (force-mode-line-update))

;;;; strokes-xpm stuff (later may be separate)...

;; This is the stuff that will eventuall be used for composing letters in
;; any language, compression, decompression, graphics, editing, etc.

(defface strokes-char-face '((t (:background "lightgray")))
  "Face for strokes characters."
  :version "21.1"
  :group 'strokes)

(put 'strokes 'char-table-extra-slots 0)
(defconst strokes-char-table (make-char-table 'strokes) ;
  "The table which stores values for the character keys.")
(aset strokes-char-table ?0 0)
(aset strokes-char-table ?1 1)
(aset strokes-char-table ?2 2)
(aset strokes-char-table ?3 3)
(aset strokes-char-table ?4 4)
(aset strokes-char-table ?5 5)
(aset strokes-char-table ?6 6)
(aset strokes-char-table ?7 7)
(aset strokes-char-table ?8 8)
(aset strokes-char-table ?9 9)
(aset strokes-char-table ?a 10)
(aset strokes-char-table ?b 11)
(aset strokes-char-table ?c 12)
(aset strokes-char-table ?d 13)
(aset strokes-char-table ?e 14)
(aset strokes-char-table ?f 15)
(aset strokes-char-table ?g 16)
(aset strokes-char-table ?h 17)
(aset strokes-char-table ?i 18)
(aset strokes-char-table ?j 19)
(aset strokes-char-table ?k 20)
(aset strokes-char-table ?l 21)
(aset strokes-char-table ?m 22)
(aset strokes-char-table ?n 23)
(aset strokes-char-table ?o 24)
(aset strokes-char-table ?p 25)
(aset strokes-char-table ?q 26)
(aset strokes-char-table ?r 27)
(aset strokes-char-table ?s 28)
(aset strokes-char-table ?t 29)
(aset strokes-char-table ?u 30)
(aset strokes-char-table ?v 31)
(aset strokes-char-table ?w 32)
(aset strokes-char-table ?x 33)
(aset strokes-char-table ?y 34)
(aset strokes-char-table ?z 35)
(aset strokes-char-table ?A 36)
(aset strokes-char-table ?B 37)
(aset strokes-char-table ?C 38)
(aset strokes-char-table ?D 39)
(aset strokes-char-table ?E 40)
(aset strokes-char-table ?F 41)
(aset strokes-char-table ?G 42)
(aset strokes-char-table ?H 43)
(aset strokes-char-table ?I 44)
(aset strokes-char-table ?J 45)
(aset strokes-char-table ?K 46)
(aset strokes-char-table ?L 47)
(aset strokes-char-table ?M 48)
(aset strokes-char-table ?N 49)
(aset strokes-char-table ?O 50)
(aset strokes-char-table ?P 51)
(aset strokes-char-table ?Q 52)
(aset strokes-char-table ?R 53)
(aset strokes-char-table ?S 54)
(aset strokes-char-table ?T 55)
(aset strokes-char-table ?U 56)
(aset strokes-char-table ?V 57)
(aset strokes-char-table ?W 58)
(aset strokes-char-table ?X 59)
(aset strokes-char-table ?Y 60)
(aset strokes-char-table ?Z 61)

(defconst strokes-base64-chars
  ;; I wanted to make this a vector of individual like (vector ?0
  ;; ?1 ?2 ...), but `concat' in XEmacs-20.* refuses to accept single
  ;; characters.
  (vector "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
	  "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
	  "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "B" "C" "D"
	  "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
	  "T" "U" "V" "W" "X" "Y" "Z")
;;  (vector [?0] [?1] [?2] [?3] [?4] [?5] [?6] [?7] [?8] [?9]
;;	  [?a] [?b] [?c] [?d] [?e] [?f] [?g] [?h] [?i] [?j]
;;	  [?k] [?l] [?m] [?n] [?o] [?p] [?q] [?r] [?s] [?t]
;;	  [?u] [?v] [?w] [?x] [?y] [?z]
;;	  [?A] [?B] [?C] [?D] [?E] [?F] [?G] [?H] [?I] [?J]
;;	  [?K] [?L] [?M] [?N] [?O] [?P] [?Q] [?R] [?S] [?T]
;;	  [?U] [?V] [?W] [?X] [?Y] [?Z])
  "Character vector for fast lookup of base-64 encoding of numbers in [0,61].")

(defsubst strokes-xpm-char-on-p (char)
  "Non-nil if CHAR represents an `on' bit in the xpm."
  (eq char ?*))

(defsubst strokes-xpm-char-bit-p (char)
  "Non-nil if CHAR represents an `on' or `off' bit in the xpm."
  (or (eq char ?\ )
      (eq char ?*)))

;;(defsubst strokes-xor (a b)  ### Should I make this an inline function? ###
;;  "T iff one and only one of A and B is non-nil; otherwise, returns nil.
;;NOTE: Don't use this as a numeric xor since it treats all non-nil
;;      values as t including `0' (zero)."
;;  (eq (null a) (not (null b))))

(defsubst strokes-xpm-encode-length-as-string (length)
  "Given some LENGTH in [0,62) do a fast lookup of it's encoding."
  (aref strokes-base64-chars length))
		   
(defsubst strokes-xpm-decode-char (character)
  "Given a CHARACTER, do a fast lookup to find its corresponding integer value."
  (aref strokes-char-table character))
		   
(defun strokes-xpm-to-compressed-string (&optional xpm-buffer)
  "Convert the xpm in XPM-BUFFER into a compressed string representing the stroke.
XPM-BUFFER is an optional argument, and defaults to `*strokes-xpm*'."
  (save-excursion
    (set-buffer (setq xpm-buffer (or xpm-buffer "*strokes-xpm*")))
    (goto-char (point-min))
    (search-forward "/* pixels */")	; skip past header junk
    (forward-char 2)
    ;; a note for below:
    ;; the `current-char' is the char being counted -- NOT the char at (point)
    ;; which happens to be called `char-at-point'
    (let ((compressed-string "+/")	; initialize the output
	  (count 0)			; keep a current count of
					; `current-char'
	  (last-char-was-on-p t)       	; last entered stream
					; represented `on' bits
	  (current-char-is-on-p nil)	; current stream represents `on' bits
	  (char-at-point (char-after)))	; read the first char
      (while (not (eq char-at-point ?})) ; a `}' denotes the
					; end of the pixmap
	(cond ((zerop count)		; must restart counting
	       ;; check to see if the `char-at-point' is an actual pixmap bit
	       (when (strokes-xpm-char-bit-p char-at-point)
		 (setq count 1
		       current-char-is-on-p (strokes-xpm-char-on-p char-at-point)))	       
	       (forward-char 1))
	      ((= count 61)		; maximum single char's
					; encoding length
	       (setq compressed-string (concat compressed-string
					       ;; add a zero-length
					       ;; encoding when
					       ;; necessary
					       (when (eq last-char-was-on-p
							 current-char-is-on-p)
						 ;; "0"
						 (strokes-xpm-encode-length-as-string 0))
					       (strokes-xpm-encode-length-as-string 61))
		     last-char-was-on-p current-char-is-on-p
		     count 0))		; note that we just set
					; count=0 and *don't* advance
					; (point)
	      ((strokes-xpm-char-bit-p char-at-point) ; an actual xpm bit
	       (if (eq current-char-is-on-p
		       (strokes-xpm-char-on-p char-at-point))
		   ;; yet another of the same bit-type, so we continue
		   ;; counting...
		   (progn
		     (incf count)
		     (forward-char 1))
		 ;; otherwise, it's the opposite bit-type, so we do a
		 ;; write and then restart count ### NOTE (for myself
		 ;; to be aware of) ### I really should advance
		 ;; (point) in this case instead of letting another
		 ;; iteration go through and letting the case: count=0
		 ;; take care of this stuff for me.  That's why
		 ;; there's no (forward-char 1) below.
		 (setq compressed-string (concat compressed-string
						 ;; add a zero-length
						 ;; encoding when
						 ;; necessary
						 (when (eq last-char-was-on-p
							   current-char-is-on-p)
						   ;; "0"
						   (strokes-xpm-encode-length-as-string 0))
						 (strokes-xpm-encode-length-as-string count))
		       count 0
		       last-char-was-on-p current-char-is-on-p)))
	      (t			; ELSE it's some other useless
					; char, like `"' or `,'
	       (forward-char 1)))
	(setq char-at-point (char-after)))
      (concat compressed-string
	      (when (> count 0)
		(concat (when (eq last-char-was-on-p
				  current-char-is-on-p)
			  ;; "0"
			  (strokes-xpm-encode-length-as-string 0))
			(strokes-xpm-encode-length-as-string count)))
	      "/"))))

;;;###autoload
(defun strokes-decode-buffer (&optional buffer force)
  "Decode stroke strings in BUFFER and display their corresponding glyphs.
Optional BUFFER defaults to the current buffer.
Optional FORCE non-nil will ignore the buffer's read-only status."
  (interactive)
  ;;  (interactive "*bStrokify buffer: ")
  (save-excursion
    (set-buffer (setq buffer (get-buffer (or buffer (current-buffer)))))
    (when (or (not buffer-read-only)
	      force
	      inhibit-read-only
	      (y-or-n-p
	       (format "Buffer %s is read-only.  Strokify anyway? " buffer)))
      (let ((inhibit-read-only t))
	(message "Strokifying %s..." buffer)
	(goto-char (point-min))
	(let (ext string image)
	  ;; The comment below is what i'd have to do if I wanted to
	  ;; deal with random newlines in the midst of the compressed
	  ;; strings.  If I do this, I'll also have to change
	  ;; `strokes-xpm-to-compress-string' to deal with the newline,
	  ;; and possibly other whitespace stuff.  YUCK!
	  ;;      (while (re-search-forward "\\+/\\(\\w\\|\\)+/" nil t nil (get-buffer buffer))
	  (while (with-current-buffer buffer
		   (when (re-search-forward "\\+/\\(\\w+\\)/" nil t nil)
		     (setq string (match-string 1))
		     (goto-char (match-end 0))
		     (replace-match " ")
		     t))
	    (strokes-xpm-for-compressed-string string " *strokes-xpm*")
	    (setq image (create-image (with-current-buffer " *strokes-xpm*"
					(buffer-string))
				      'xpm t))
	    (insert-image image
			  (propertize " "
				      'type 'stroke-glyph
				      'stroke-glyph image
				      'data string))))
	(message "Strokifying %s...done" buffer)))))

(defun strokes-encode-buffer (&optional buffer force)
  "Convert the glyphs in BUFFER to thier base-64 ASCII representations.
Optional BUFFER defaults to the current buffer.
Optional FORCE non-nil will ignore the buffer's read-only status."
  ;; ### NOTE !!! ### (for me)
  ;; For later on, you can/should make the inserted strings atomic
  ;; extents, so that the users have a clue that they shouldn't be
  ;; editing inside them.  Plus, if you make them extents, you can
  ;; very easily just hide the glyphs, so if you unstrokify, and the
  ;; restrokify, then those that already are glyphed don't need to be
  ;; re-calculated, etc.  It's just nicer that way.  The only things
  ;; to worry about is cleanup (i.e. do the glyphs get gc'd when the
  ;; buffer is killed?
  ;;  (interactive "*bUnstrokify buffer: ")
  (interactive)
  (save-excursion
    (set-buffer (setq buffer (or buffer (current-buffer))))
    (when (or (not buffer-read-only)
	      force
	      inhibit-read-only
	      (y-or-n-p
	       (format "Buffer %s is read-only.  Encode anyway? " buffer)))
      (message "Encoding strokes in %s..." buffer)
      ;;      (map-extents
      ;;       (lambda (ext buf)
      ;;	 (when (eq (extent-property ext 'type) 'stroke-glyph)
      ;;	   (goto-char (extent-start-position ext))
      ;;	   (delete-char 1)  ; ### What the hell do I do here? ###
      ;;	   (insert "+/" (extent-property ext 'data) "/")
      ;;       (delete-extent ext))))))
      (let ((inhibit-read-only t)
	    (start nil)
	    glyph)
	(while (or (and (bobp)
			(get-text-property (point) 'type))
		   (setq start (next-single-property-change (point) 'type)))
	  (when (eq 'stroke-glyph (get-text-property (point) 'type))
	    (goto-char start)
	    (setq start (point-marker)
		  glyph  (get-text-property start 'display))
	    (insert "+/" (get-text-property (point) 'data) ?/)
	    (delete-char 1)
	    (add-text-properties start (point)
				 (list 'type 'stroke-string
				       'face 'strokes-char-face
				       'stroke-glyph glyph
				       'display nil))))
	(message "Encoding strokes in %s...done" buffer)))))

(defun strokes-xpm-for-compressed-string (compressed-string &optional bufname)
  "Convert the stroke represented by COMPRESSED-STRING into an xpm.
Store xpm in buffer BUFNAME if supplied \(default is `*strokes-xpm*'\)"
  (save-excursion
    (or bufname (setq bufname "*strokes-xpm*"))
    (set-buffer (get-buffer-create bufname))
    (erase-buffer)
    (insert compressed-string)
    (goto-char (point-min))
    (let ((current-char-is-on-p nil))
      (while (not (eobp))
	(insert-char
	 (if current-char-is-on-p
	     ?*
	   ?\ )
	 (strokes-xpm-decode-char (char-after)))
	(delete-char 1)
	(setq current-char-is-on-p (not current-char-is-on-p)))
      (goto-char (point-min))
      (loop repeat 33 do
	    (insert ?\")
	    (forward-char 33)
	    (insert "\",\n"))
      (goto-char (point-min))
      (insert strokes-xpm-header))))

;;;###autoload
(defun strokes-compose-complex-stroke ()
  ;; ### NOTE !!! ###
  ;; Even though we have lexical scoping, it's somewhat ugly how I
  ;; pass around variables in the global name space.  I can/should
  ;; change this.
  "Read a complex stroke and insert its glyph into the current buffer."
  (interactive "*")
  (let ((strokes-grid-resolution 33))
    (strokes-read-complex-stroke)
    (strokes-xpm-for-stroke nil " *strokes-xpm*" t)
    (insert (strokes-xpm-to-compressed-string " *strokes-xpm*"))
    (strokes-decode-buffer)
    ;; strokes-decode-buffer does a save-excursion.
    (forward-char)))

(or (assq 'strokes-mode minor-mode-alist)
    (setq minor-mode-alist (cons (list 'strokes-mode strokes-modeline-string)
				 minor-mode-alist)))

(provide 'strokes)
(run-hooks 'strokes-load-hook)

;;; strokes.el ends here
