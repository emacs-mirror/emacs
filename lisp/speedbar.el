;;; speedbar.el --- quick access to files and tags in a frame

;;; Copyright (C) 1996, 97, 98, 99, 2000, 01 Free Software Foundation

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.11a
;; Keywords: file, tags, tools

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
;;
;;   The speedbar provides a frame in which files, and locations in
;; files are displayed.  These items can be clicked on with mouse-2
;; in order to make the last active frame display that file location.
;;
;; Starting Speedbar:
;;
;;   Simply type `M-x speedbar', and it will be autoloaded for you.

;;   If you want to choose it from a menu, such as "Tools", you can do this:
;;
;;   (define-key-after (lookup-key global-map [menu-bar tools])
;;      [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])
;;
;;   If you want to access speedbar using only the keyboard, do this:
;;
;;   (global-set-key [f4] 'speedbar-get-focus)
;;
;;   This will let you hit f4 (or whatever key you choose) to jump
;; focus to the speedbar frame.  Pressing it again will bring you back
;; to the attached frame.  Pressing RET or e to jump to a file
;; or tag will move you back to the attached frame.  The command
;; `speedbar-get-focus' will also create a speedbar frame if it does
;; not exist.
;;
;; Customizing Speedbar:
;;
;;   Once a speedbar frame is active, it takes advantage of idle time
;; to keep its contents updated.  The contents is usually a list of
;; files in the directory of the currently active buffer.  When
;; applicable, tags in the active file can be expanded.
;;
;;   To add new supported files types into speedbar, use the function
;; `speedbar-add-supported-extension'.  If speedbar complains that the
;; file type is not supported, that means there is no built in
;; support from imenu, and the etags part wasn't set up correctly.  You
;; may add elements to `speedbar-supported-extension-expressions' as long
;; as it is done before speedbar is loaded.
;;
;;   To prevent speedbar from following you into certain directories
;; use the function `speedbar-add-ignored-path-regexp' to add a new
;; regular expression matching a type of path.  You may add list
;; elements to `speedbar-ignored-path-expressions' as long as it is
;; done before speedbar is loaded.
;;
;;   To add new file types to imenu, see the documentation in the
;; file imenu.el that comes with Emacs.  To add new file types which
;; etags supports, you need to modify the variable
;; `speedbar-fetch-etags-parse-list'.
;;
;;    If the updates are going too slow for you, modify the variable
;; `speedbar-update-speed' to a longer idle time before updates.
;;
;;    If you navigate directories, you will probably notice that you
;; will navigate to a directory which is eventually replaced after
;; you go back to editing a file (unless you pull up a new file.)
;; The delay time before this happens is in
;; `speedbar-navigating-speed', and defaults to 10 seconds.
;;
;;    To enable mouse tracking with information in the minibuffer of
;; the attached frame, use the variable `speedbar-track-mouse-flag'.
;;
;;    Tag layout can be modified through `speedbar-tag-hierarchy-method',
;; which controls how tags are layed out.  It is actually a list of
;; functions that filter the data.  The default groups large tag lists
;; into sub-lists.  A long flat list can be used instead if needed.
;; Other filters can be easily added.
;;
;;    AUC-TEX users: The imenu tags for AUC-TEX mode doesn't work very
;; well.  Use the imenu keywords from tex-mode.el for better results.
;;
;; This file requires the library package assoc (association lists)
;;
;;; Developing for speedbar
;;
;; Adding a speedbar specialized display mode:
;;
;; Speedbar can be configured to create a special display for certain
;; modes that do not display traditional file/tag data.  Rmail, Info,
;; and the debugger are examples.  These modes can, however, benefit
;; from a speedbar style display in their own way.
;;
;; If your `major-mode' is `foo-mode', the only requirement is to
;; create a function called `foo-speedbar-buttons' which takes one
;; argument, BUFFER.  BUFFER will be the buffer speedbar wants filled.
;; In `foo-speedbar-buttons' there are several functions that make
;; building a speedbar display easy.  See the documentation for
;; `speedbar-with-writable' (needed because the buffer is usually
;; read-only) `speedbar-make-tag-line', `speedbar-insert-button', and
;; `speedbar-insert-generic-list'.  If you use
;; `speedbar-insert-generic-list', also read the doc for
;; `speedbar-tag-hierarchy-method' in case you wish to override it.
;; The macro `speedbar-with-attached-buffer' brings you back to the
;; buffer speedbar is displaying for.
;;
;; For those functions that make buttons, the "function" should be a
;; symbol that is the function to call when clicked on.  The "token"
;; is extra data you can pass along.  The "function" must take three
;; parameters.  They are (TEXT TOKEN INDENT).  TEXT is the text of the
;; button clicked on.  TOKEN is the data passed in when you create the
;; button.  INDENT is an indentation level, or 0.  You can store
;; indentation levels with `speedbar-make-tag-line' which creates a
;; line with an expander (eg.  [+]) and a text button.
;;
;; Some useful functions when writing expand functions, and click
;; functions are `speedbar-change-expand-button-char',
;; `speedbar-delete-subblock', and `speedbar-center-buffer-smartly'.
;; The variable `speedbar-power-click' is set to t in your functions
;; when the user shift-clicks.  This is an indication of anything from
;; refreshing cached data to making a buffer appear in a new frame.
;;
;; If you wish to add to the default speedbar menu for the case of
;; `foo-mode', create a variable `foo-speedbar-menu-items'.  This
;; should be a list compatible with the `easymenu' package.  It will
;; be spliced into the main menu.  (Available with click-mouse-3).  If
;; you wish to have extra key bindings in your special mode, create a
;; variable `foo-speedbar-key-map'.  Instead of using `make-keymap',
;; or `make-sparse-keymap', use the function
;; `speedbar-make-specialized-keymap'.  This lets you inherit all of
;; speedbar's default bindings with low overhead.
;;
;; Adding a speedbar top-level display mode:
;;
;; Unlike the specialized modes, there are no name requirements,
;; however the methods for writing a button display, menu, and keymap
;; are the same.  Once you create these items, you can call the
;; function `speedbar-add-expansion-list'.  It takes one parameter
;; which is a list element of the form (NAME MENU KEYMAP &rest
;; BUTTON-FUNCTIONS).  NAME is a string that will show up in the
;; Displays menu item.  MENU is a symbol containing the menu items to
;; splice in.  KEYMAP is a symbol holding the keymap to use, and
;; BUTTON-FUNCTIONS are the function names to call, in order, to create
;; the display.
;;  Another tweakable variable is `speedbar-stealthy-function-list'
;; which is of the form (NAME &rest FUNCTION ...).  NAME is the string
;; name matching `speedbar-add-expansion-list'.  (It does not need to
;; exist.). This provides additional display info which might be
;; time-consuming to calculate.
;;  Lastly, `speedbar-mode-functions-list' allows you to set special
;; function overrides.  At the moment very few functions may be
;; overridden, but more will be added as the need is discovered.

;;; TODO:
;; - More functions to create buttons and options
;; - Timeout directories we haven't visited in a while.

(require 'assoc)
(require 'easymenu)

(condition-case nil
    (require 'image)
  (error nil))

(defvar speedbar-xemacsp (string-match "XEmacs" emacs-version)
  "Non-nil if we are running in the XEmacs environment.")
(defvar speedbar-xemacs20p (and speedbar-xemacsp
				(>= emacs-major-version 20)))

;; customization stuff
(defgroup speedbar nil
  "File and tag browser frame."
  :group 'tags
  :group 'tools
  :group 'convenience
  :version "20.3")

(defgroup speedbar-faces nil
  "Faces used in speedbar."
  :prefix "speedbar-"
  :group 'speedbar
  :group 'faces)

(defgroup speedbar-vc nil
  "Version control display in speedbar."
  :prefix "speedbar-"
  :group 'speedbar)

;;; Code:
(defvar speedbar-initial-expansion-mode-alist
  '(("buffers" speedbar-buffer-easymenu-definition speedbar-buffers-key-map
     speedbar-buffer-buttons)
    ("quick buffers" speedbar-buffer-easymenu-definition speedbar-buffers-key-map
     speedbar-buffer-buttons-temp)
    ;; Files last, means first in the Displays menu
    ("files" speedbar-easymenu-definition-special speedbar-file-key-map
     speedbar-directory-buttons speedbar-default-directory-list)
    )
  "List of named expansion elements for filling the speedbar frame.
These expansion lists are only valid for regular files.  Special modes
still get to override this list on a mode-by-mode basis.  This list of
lists is of the form (NAME MENU KEYMAP FN1 FN2 ...).  NAME is a string
representing the types of things to be displayed.  MENU is an easymenu
structure used when in this mode.  KEYMAP is a local keymap to install
over the regular speedbar keymap.  FN1 ...  are functions that will be
called in order.  These functions will always get the default
directory to use passed in as the first parameter, and a 0 as the
second parameter.  The 0 indicates the uppermost indentation level.
They must assume that the cursor is at the position where they start
inserting buttons.")

(defvar speedbar-initial-expansion-list-name "files"
  "A symbol name representing the expansion list to use.
The expansion list `speedbar-initial-expansion-mode-alist' contains
the names and associated functions to use for buttons in speedbar.")

(defvar speedbar-previously-used-expansion-list-name "files"
  "Save the last expansion list method.
This is used for returning to a previous expansion list method when
the user is done with the current expansion list.")

(defvar speedbar-stealthy-function-list
  '(("files"
     speedbar-update-current-file speedbar-check-vc speedbar-check-objects)
    )
  "List of functions to periodically call stealthily.
This list is of the form:
 '( (\"NAME\" FUNCTION ...)
    ...)
where NAME is the name of the major display mode these functions are
for, and the remaining elements FUNCTION are functions to call in order.
Each function must return nil if interrupted, or t if completed.
Stealthy functions which have a single operation should always return
t.  Functions which take a long time should maintain a state (where
they are in their speedbar related calculations) and permit
interruption.  See `speedbar-check-vc' as a good example.")

(defvar speedbar-mode-functions-list
  '(("files" (speedbar-item-info . speedbar-files-item-info)
     (speedbar-line-path . speedbar-files-line-path))
    ("buffers" (speedbar-item-info . speedbar-buffers-item-info)
     (speedbar-line-path . speedbar-buffers-line-path))
    ("quick buffers" (speedbar-item-info . speedbar-buffers-item-info)
     (speedbar-line-path . speedbar-buffers-line-path))
    )
  "List of function tables to use for different major display modes.
It is not necessary to define any functions for a specialized mode.
This just provides a simple way of adding lots of customizations.
Each sublist is of the form:
  (\"NAME\" (FUNCTIONSYMBOL . REPLACEMENTFUNCTION) ...)
Where NAME is the name of the specialized mode.  The rest of the list
is a set of dotted pairs of the form FUNCTIONSYMBOL, which is the name
of a function you would like to replace, and REPLACEMENTFUNCTION,
which is a function you can call instead.  Not all functions can be
replaced this way.  Replaceable functions must provide that
functionality individually.")

(defcustom speedbar-mode-specific-contents-flag t
  "*Non-nil means speedbar will show special mode contents.
This permits some modes to create customized contents for the speedbar
frame."
  :group 'speedbar
  :type 'boolean)

(defvar speedbar-special-mode-expansion-list nil
  "Default function list for creating specialized button lists.
This list is set by modes that wish to have special speedbar displays.
The list is of function names.  Each function is called with one
parameter BUFFER, the originating buffer.  The current buffer is the
speedbar buffer.")

(defvar speedbar-special-mode-key-map nil
  "Default keymap used when identifying a specialized display mode.
This keymap is local to each buffer that wants to define special keybindings
effective when it's display is shown.")

(defcustom speedbar-visiting-file-hook nil
  "Hooks run when speedbar visits a file in the selected frame."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-visiting-tag-hook '(speedbar-highlight-one-tag-line)
  "Hooks run when speedbar visits a tag in the selected frame."
  :group 'speedbar
  :type 'hook
  :version "21.1"
  :options '(speedbar-highlight-one-tag-line
	     speedbar-recenter-to-top
	     speedbar-recenter
	     ))

(defcustom speedbar-load-hook nil
  "Hooks run when speedbar is loaded."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-reconfigure-keymaps-hook nil
  "Hooks run when the keymaps are regenerated."
  :group 'speedbar
  :version "21.1"
  :type 'hook)

(defcustom speedbar-show-unknown-files nil
  "*Non-nil show files we can't expand with a ? in the expand button.
nil means don't show the file in the list."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-update-speed
  (if speedbar-xemacsp
      (if speedbar-xemacs20p
	  2				; 1 is too obrusive in XEmacs
	5)				; when no idleness, need long delay
    1)
  "*Idle time in seconds needed before speedbar will update itself.
Updates occur to allow speedbar to display directory information
relevant to the buffer you are currently editing."
  :group 'speedbar
  :type 'integer)

;; When I moved to a repeating timer, I had the horrible missfortune
;; of loosing the ability for adaptive speed choice.  This update
;; speed currently causes long delays when it should have been turned off.
(defcustom speedbar-navigating-speed speedbar-update-speed
  "*Idle time to wait after navigation commands in speedbar are executed.
Navigation commands included expanding/contracting nodes, and moving
between different directories."
  :group 'speedbar
  :type 'integer)

(defcustom speedbar-frame-parameters '((minibuffer . nil)
				       (width . 20)
				       (border-width . 0)
				       (menu-bar-lines . 0)
				       (tool-bar-lines . 0)
				       (unsplittable . t))
  "*Parameters to use when creating the speedbar frame in Emacs.
Any parameter supported by a frame may be added.  The parameter `height'
will be initialized to the height of the frame speedbar is
attached to and added to this list before the new frame is initialized."
  :group 'speedbar
  :type '(repeat (sexp :tag "Parameter:")))

;; These values by Hrvoje Niksic <hniksic@srce.hr>
(defcustom speedbar-frame-plist
  '(minibuffer nil width 20 border-width 0
	       internal-border-width 0 unsplittable t
	       default-toolbar-visible-p nil has-modeline-p nil
	       menubar-visible-p nil)
  "*Parameters to use when creating the speedbar frame in XEmacs.
Parameters not listed here which will be added automatically are
`height' which will be initialized to the height of the frame speedbar
is attached to."
  :group 'speedbar
  :type '(repeat (group :inline t
			(symbol :tag "Property")
			(sexp :tag "Value"))))

(defcustom speedbar-use-imenu-flag (stringp (locate-library "imenu"))
  "*Non-nil means use imenu for file parsing.  nil to use etags.
XEmacs prior to 20.4 doesn't support imenu, therefore the default is to
use etags instead.  Etags support is not as robust as imenu support."
  :tag "Use Imenu for tags"
  :group 'speedbar
  :type 'boolean)

(defvar speedbar-dynamic-tags-function-list
  '((speedbar-fetch-dynamic-imenu . speedbar-insert-imenu-list)
    (speedbar-fetch-dynamic-etags . speedbar-insert-etags-list))
  "Set to a functions which will return and insert a list of tags.
Each element is of the form ( FETCH .  INSERT ) where FETCH
is a funciotn which takes one parameter (the file to tag) and returns a
list of tags.  The tag list can be of any form as long as the
corresponding insert method can handle it.  If it returns t, then an
error occured, and the next fetch routine is tried.
INSERT is a function which takes an INDENTation level, and a LIST of
tags to insert.  It will then create the speedbar buttons.")

(defcustom speedbar-track-mouse-flag t
  "*Non-nil means to display info about the line under the mouse."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-sort-tags nil
  "*If Non-nil, sort tags in the speedbar display.  *Obsolete*."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-tag-hierarchy-method
  '(speedbar-prefix-group-tag-hierarchy
    speedbar-trim-words-tag-hierarchy)
  "*List of hooks which speedbar will use to organize tags into groups.
Groups are defined as expandable meta-tags.  Imenu supports
such things in some languages, such as separating variables from
functions.  Each hook takes one argument LST, and may destructivly
create a new list of the same form.  LST is a list of elements of the
form:
  (ELT1 ELT2 ... ELTn)
where each ELT is of the form
  (TAG-NAME-STRING . NUMBER-OR-MARKER)
or
  (GROUP-NAME-STRING ELT1 EL2... ELTn)"
  :group 'speedbar
  :type 'hook
  :options '(speedbar-sort-tag-hierarchy
	     speedbar-trim-words-tag-hierarchy
	     speedbar-prefix-group-tag-hierarchy
	     speedbar-simple-group-tag-hierarchy)
  )

(defcustom speedbar-tag-group-name-minimum-length 4
  "*The minimum length of a prefix group name before expanding.
Thus, if the `speedbar-tag-hierarchy-method' includes `prefix-group'
and one such groups common characters is less than this number of
characters, then the group name will be changed to the form of:
  worda to wordb
instead of just
  word
This way we won't get silly looking listings."
  :group 'speedbar
  :type 'integer)

(defcustom speedbar-tag-split-minimum-length 20
  "*Minimum length before we stop trying to create sub-lists in tags.
This is used by all tag-hierarchy methods that break large lists into
sub-lists."
  :group 'speedbar
  :type 'integer)

(defcustom speedbar-tag-regroup-maximum-length 10
  "*Maximum length of submenus that are regrouped.
If the regrouping option is used, then if two or more short subgroups
are next to each other, then they are combined until this number of
items is reached."
  :group 'speedbar
  :type 'integer)

(defcustom speedbar-activity-change-focus-flag nil
  "*Non-nil means the selected frame will change based on activity.
Thus, if a file is selected for edit, the buffer will appear in the
selected frame and the focus will change to that frame."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-directory-button-trim-method 'span
  "*Indicates how the directory button will be displayed.
Possible values are:
 'span - span large directories over multiple lines.
 'trim - trim large directories to only show the last few.
 nil   - no trimming."
  :group 'speedbar
  :type '(radio (const :tag "Span large directories over mutiple lines."
		       span)
		(const :tag "Trim large directories to only show the last few."
		       trim)
		(const :tag "No trimming." nil)))

(defcustom speedbar-smart-directory-expand-flag t
  "*Non-nil means speedbar should use smart expansion.
Smart expansion only affects when speedbar wants to display a
directory for a file in the attached frame.  When smart expansion is
enabled, new directories which are children of a displayed directory
are expanded in the current framework.  If nil, then the current
hierarchy would be replaced with the new directory."
  :group 'speedbar
  :type 'boolean)

(defcustom speedbar-indentation-width 1
  "*When sub-nodes are expanded, the number of spaces used for indentation."
  :group 'speedbar
  :version "21.1"
  :type 'integer)

(defcustom speedbar-hide-button-brackets-flag nil
  "*Non-nil means speedbar will hide the brackets around the + or -."
  :group 'speedbar
  :version "21.1"
  :type 'boolean)

(defcustom speedbar-use-images (and (or (fboundp 'defimage)
					(fboundp 'make-image-specifier))
				    (if (fboundp 'display-graphic-p)
					(display-graphic-p)
				      window-system))
  "*Non nil if speedbar should display icons."
  :group 'speedbar
  :version "21.1"
  :type 'boolean)

(defcustom speedbar-before-popup-hook nil
  "*Hooks called before popping up the speedbar frame."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-before-delete-hook nil
  "*Hooks called before deleting the speedbar frame."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-mode-hook nil
  "*Hooks called after creating a speedbar buffer."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-timer-hook nil
  "*Hooks called after running the speedbar timer function."
  :group 'speedbar
  :type 'hook)

(defcustom speedbar-verbosity-level 1
  "*Verbosity level of the speedbar.  0 means say nothing.
1 means medium level verbosity.  2 and higher are higher levels of
verbosity."
  :group 'speedbar
  :type 'integer)

(defvar speedbar-indicator-separator " "
  "String separating file text from indicator characters.")

(defcustom speedbar-vc-do-check t
  "*Non-nil check all files in speedbar to see if they have been checked out.
Any file checked out is marked with `speedbar-vc-indicator'"
  :group 'speedbar-vc
  :type 'boolean)

(defvar speedbar-vc-indicator "*"
  "Text used to mark files which are currently checked out.
Other version control systems can be added by examining the function
`speedbar-vc-path-enable-hook' and `speedbar-vc-in-control-hook'.")

(defcustom speedbar-vc-path-enable-hook nil
  "*Return non-nil if the current path should be checked for Version Control.
Functions in this hook must accept one parameter which is the path
being checked."
  :group 'speedbar-vc
  :type 'hook)

(defcustom speedbar-vc-in-control-hook nil
  "*Return non-nil if the specified file is under Version Control.
Functions in this hook must accept two parameters.  The PATH of the
current file, and the FILENAME of the file being checked."
  :group 'speedbar-vc
  :type 'hook)

(defvar speedbar-vc-to-do-point nil
  "Local variable maintaining the current version control check position.")

(defcustom speedbar-obj-do-check t
  "*Non-nil check all files in speedbar to see if they have an object file.
Any file checked out is marked with `speedbar-obj-indicator', and the
marking is based on  `speedbar-obj-alist'"
  :group 'speedbar-vc
  :type 'boolean)

(defvar speedbar-obj-to-do-point nil
  "Local variable maintaining the current version control check position.")

(defvar speedbar-obj-indicator '("#" . "!")
  "Text used to mark files that have a corresponding hidden object file.
The car is for an up-to-date object.  The cdr is for an out of date object.
The expression `speedbar-obj-alist' defines who gets tagged.")

(defvar speedbar-obj-alist
  '(("\\.\\([cpC]\\|cpp\\|cc\\)$" . ".o")
    ("\\.el$" . ".elc")
    ("\\.java$" . ".class")
    ("\\.f\\(or\\|90\\|77\\)?$" . ".o")
    ("\\.tex$" . ".dvi")
    ("\\.texi$" . ".info"))
  "Alist of file extensions, and their corresponding object file type.")

(defvar speedbar-indicator-regex
  (concat (regexp-quote speedbar-indicator-separator)
	  "\\("
	  (regexp-quote speedbar-vc-indicator)
	  "\\|"
	  (regexp-quote (car speedbar-obj-indicator))
	  "\\|"
	  (regexp-quote (cdr speedbar-obj-indicator))
	  "\\)*")
  "Regular expression used when identifying files.
Permits stripping of indicator characters from a line.")

(defcustom speedbar-scanner-reset-hook nil
  "*Hook called whenever generic scanners are reset.
Set this to implement your own scanning / rescan safe functions with
state data."
  :group 'speedbar
  :type 'hook)

(defvar speedbar-ignored-modes nil
  "*List of major modes which speedbar will not switch directories for.")

(defun speedbar-extension-list-to-regex (extlist)
  "Takes EXTLIST, a list of extensions and transforms it into regexp.
All the preceding `.' are stripped for an optimized expression starting
with `.' followed by extensions, followed by full-filenames."
  (let ((regex1 nil) (regex2 nil))
    (while extlist
      (if (= (string-to-char (car extlist)) ?.)
	  (setq regex1 (concat regex1 (if regex1 "\\|" "")
			       (substring (car extlist) 1)))
	(setq regex2 (concat regex2 (if regex2 "\\|" "") (car extlist))))
      (setq extlist (cdr extlist)))
    ;; concat all the sub-exressions together, making sure all types
    ;; of parts exist during concatination.
    (concat "\\("
	    (if regex1 (concat "\\(\\.\\(" regex1 "\\)\\)") "")
	    (if (and regex1 regex2) "\\|" "")
	    (if regex2 (concat "\\(" regex2 "\\)") "")
	    "\\)$")))

(defvar speedbar-ignored-path-regexp nil
  "Regular expression matching paths speedbar will not switch to.
Created from `speedbar-ignored-path-expressions' with the function
`speedbar-extension-list-to-regex' (A misnamed function in this case.)
Use the function `speedbar-add-ignored-path-regexp', or customize the
variable `speedbar-ignored-path-expressions' to modify this variable.")

(defcustom speedbar-ignored-path-expressions
  '("[/\\]logs?[/\\]\\'")
  "*List of regular expressions matching directories speedbar will ignore.
They should included paths to directories which are notoriously very
large and take a long time to load in.  Use the function
`speedbar-add-ignored-path-regexp' to add new items to this list after
speedbar is loaded.  You may place anything you like in this list
before speedbar has been loaded."
  :group 'speedbar
  :type '(repeat (regexp :tag "Path Regexp"))
  :set (lambda (sym val)
	 (setq speedbar-ignored-path-expressions val
	       speedbar-ignored-path-regexp
	       (speedbar-extension-list-to-regex val))))

(defcustom speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\)\\'"
  "*Regular expression matching directories not to show in speedbar.
They should include commonly existing directories which are not
useful, such as version control."
  :group 'speedbar
  :type 'string)

(defvar speedbar-file-unshown-regexp
  (let ((nstr "") (noext completion-ignored-extensions))
    (while noext
      (setq nstr (concat nstr (regexp-quote (car noext)) "\\'"
			 (if (cdr noext) "\\|" ""))
	    noext (cdr noext)))
    ;;               backup      refdir      lockfile
    (concat nstr "\\|#[^#]+#$\\|\\.\\.?\\'\\|\\.#"))
  "*Regexp matching files we don't want displayed in a speedbar buffer.
It is generated from the variable `completion-ignored-extensions'")

;; this is dangerous to customize, because the defaults will probably
;; change in the future.
(defcustom speedbar-supported-extension-expressions
  (append '(".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?"
	    ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".f\\(90\\|77\\|or\\)?")
	  (if speedbar-use-imenu-flag
	      '(".ada" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g"
		;; html is not supported by default, but an imenu tags package
		;; is available.  Also, html files are nice to be able to see.
		".s?html"
		"[Mm]akefile\\(\\.in\\)?")))
  "*List of regular expressions which will match files supported by tagging.
Do not prefix the `.' char with a double \\ to quote it, as the period
will be stripped by a simplified optimizer when compiled into a
singular expression.  This variable will be turned into
`speedbar-file-regexp' for use with speedbar.  You should use the
function `speedbar-add-supported-extension' to add a new extension at
runtime, or use the configuration dialog to set it in your .emacs
file."
  :group 'speedbar
  :version "21.1"
  :type '(repeat (regexp :tag "Extension Regexp"))
  :set (lambda (sym val)
	 (setq speedbar-supported-extension-expressions val
	       speedbar-file-regexp (speedbar-extension-list-to-regex val)))
  )

(defvar speedbar-file-regexp
  (speedbar-extension-list-to-regex speedbar-supported-extension-expressions)
  "Regular expression matching files we know how to expand.
Created from `speedbar-supported-extension-expression' with the
function `speedbar-extension-list-to-regex'")

(defun speedbar-add-supported-extension (extension)
  "Add EXTENSION as a new supported extension for speedbar tagging.
This should start with a `.' if it is not a complete file name, and
the dot should NOT be quoted in with \\.  Other regular expression
matchers are allowed however.  EXTENSION may be a single string or a
list of strings."
  (interactive "sExtionsion: ")
  (if (not (listp extension)) (setq extension (list extension)))
  (while extension
    (if (member (car extension) speedbar-supported-extension-expressions)
	nil
      (setq speedbar-supported-extension-expressions
	    (cons (car extension) speedbar-supported-extension-expressions)))
    (setq extension (cdr extension)))
  (setq speedbar-file-regexp (speedbar-extension-list-to-regex
			      speedbar-supported-extension-expressions)))

(defun speedbar-add-ignored-path-regexp (path-expression)
  "Add PATH-EXPRESSION as a new ignored path for speedbar tracking.
This function will modify `speedbar-ignored-path-regexp' and add
PATH-EXPRESSION to `speedbar-ignored-path-expressions'."
  (interactive "sPath regex: ")
  (if (not (listp path-expression))
      (setq path-expression (list path-expression)))
  (while path-expression
    (if (member (car path-expression) speedbar-ignored-path-expressions)
	nil
      (setq speedbar-ignored-path-expressions
	    (cons (car path-expression) speedbar-ignored-path-expressions)))
    (setq path-expression (cdr path-expression)))
  (setq speedbar-ignored-path-regexp (speedbar-extension-list-to-regex
				      speedbar-ignored-path-expressions)))

;; If we don't have custom, then we set it here by hand.
(if (not (fboundp 'custom-declare-variable))
    (setq speedbar-file-regexp (speedbar-extension-list-to-regex
				speedbar-supported-extension-expressions)
	  speedbar-ignored-path-regexp (speedbar-extension-list-to-regex
					speedbar-ignored-path-expressions)))

(defvar speedbar-update-flag (and
			      (or (fboundp 'run-with-idle-timer)
				  (fboundp 'start-itimer)
				  (boundp 'post-command-idle-hook))
			      (if (fboundp 'display-graphic-p)
				  (display-graphic-p)
				window-system))
  "*Non-nil means to automatically update the display.
When this is nil then speedbar will not follow the attached frame's path.
When speedbar is active, use:

\\<speedbar-key-map> `\\[speedbar-toggle-updates]'

to toggle this value.")

(defvar speedbar-syntax-table nil
  "Syntax-table used on the speedbar.")

(if speedbar-syntax-table
    nil
  (setq speedbar-syntax-table (make-syntax-table))
  ;; turn off paren matching around here.
  (modify-syntax-entry ?\' " " speedbar-syntax-table)
  (modify-syntax-entry ?\" " " speedbar-syntax-table)
  (modify-syntax-entry ?( " " speedbar-syntax-table)
  (modify-syntax-entry ?) " " speedbar-syntax-table)
  (modify-syntax-entry ?{ " " speedbar-syntax-table)
  (modify-syntax-entry ?} " " speedbar-syntax-table)
  (modify-syntax-entry ?[ " " speedbar-syntax-table)
  (modify-syntax-entry ?] " " speedbar-syntax-table))

(defvar speedbar-key-map nil
  "Keymap used in speedbar buffer.")

(if speedbar-key-map
    nil
  (setq speedbar-key-map (make-keymap))
  (suppress-keymap speedbar-key-map t)

  ;; control
  (define-key speedbar-key-map "g" 'speedbar-refresh)
  (define-key speedbar-key-map "t" 'speedbar-toggle-updates)
  (define-key speedbar-key-map "q" 'speedbar-close-frame)
  (define-key speedbar-key-map "Q" 'delete-frame)

  ;; navigation
  (define-key speedbar-key-map "n" 'speedbar-next)
  (define-key speedbar-key-map "p" 'speedbar-prev)
  (define-key speedbar-key-map "\M-n" 'speedbar-restricted-next)
  (define-key speedbar-key-map "\M-p" 'speedbar-restricted-prev)
  (define-key speedbar-key-map "\C-\M-n" 'speedbar-forward-list)
  (define-key speedbar-key-map "\C-\M-p" 'speedbar-backward-list)
  (define-key speedbar-key-map " " 'speedbar-scroll-up)
  (define-key speedbar-key-map [delete] 'speedbar-scroll-down)

  ;; Short cuts I happen to find useful
  (define-key speedbar-key-map "r"
    (lambda () (interactive)
      (speedbar-change-initial-expansion-list
       speedbar-previously-used-expansion-list-name)))
  (define-key speedbar-key-map "b"
    (lambda () (interactive)
      (speedbar-change-initial-expansion-list "quick buffers")))
  (define-key speedbar-key-map "f"
    (lambda () (interactive)
      (speedbar-change-initial-expansion-list "files")))

  ;; Overrides
  (substitute-key-definition 'switch-to-buffer
			     'speedbar-switch-buffer-attached-frame
			     speedbar-key-map global-map)

  (if speedbar-xemacsp
      (progn
	;; mouse bindings so we can manipulate the items on each line
	(define-key speedbar-key-map 'button2 'speedbar-click)
	(define-key speedbar-key-map '(shift button2) 'speedbar-power-click)
	;; Info doc fix from Bob Weiner
	(if (featurep 'infodoc)
	    nil
	  (define-key speedbar-key-map 'button3 'speedbar-xemacs-popup-kludge))
	(define-key speedbar-key-map '(meta button3) 'speedbar-mouse-item-info)
	)

    ;; mouse bindings so we can manipulate the items on each line
    (define-key speedbar-key-map [down-mouse-1] 'speedbar-double-click)
    (define-key speedbar-key-map [mouse-2] 'speedbar-click)
    ;; This is the power click for new frames, or refreshing a cache
    (define-key speedbar-key-map [S-mouse-2] 'speedbar-power-click)
    ;; This adds a small unecessary visual effect
    ;;(define-key speedbar-key-map [down-mouse-2] 'speedbar-quick-mouse)
    (define-key speedbar-key-map [M-mouse-2] 'speedbar-mouse-item-info)

    (define-key speedbar-key-map [down-mouse-3] 'speedbar-emacs-popup-kludge)

    ;; This lets the user scroll as if we had a scrollbar... well maybe not
    (define-key speedbar-key-map [mode-line mouse-2] 'speedbar-mouse-hscroll)
    ;; another handy place users might click to get our menu.
    (define-key speedbar-key-map [mode-line down-mouse-1]
      'speedbar-emacs-popup-kludge)

    ;; We can't switch buffers with the buffer mouse menu.  Lets hack it.
    (define-key speedbar-key-map [C-down-mouse-1] 'speedbar-hack-buffer-menu)

    ;; Lastly, we want to track the mouse.  Play here
    (define-key speedbar-key-map [mouse-movement] 'speedbar-track-mouse)
   ))

(defun speedbar-make-specialized-keymap ()
  "Create a keymap for use w/ a speedbar major or minor display mode.
This basically creates a sparse keymap, and makes it's parent be
`speedbar-key-map'."
  (let ((k (make-sparse-keymap)))
    (set-keymap-parent k speedbar-key-map)
    k))

(defvar speedbar-file-key-map nil
  "Keymap used in speedbar buffer while files are displayed.")

(if speedbar-file-key-map
    nil
  (setq speedbar-file-key-map (speedbar-make-specialized-keymap))

  ;; Basic tree features
  (define-key speedbar-file-key-map "e" 'speedbar-edit-line)
  (define-key speedbar-file-key-map "\C-m" 'speedbar-edit-line)
  (define-key speedbar-file-key-map "+" 'speedbar-expand-line)
  (define-key speedbar-file-key-map "=" 'speedbar-expand-line)
  (define-key speedbar-file-key-map "-" 'speedbar-contract-line)

  ;; file based commands
  (define-key speedbar-file-key-map "U" 'speedbar-up-directory)
  (define-key speedbar-file-key-map "I" 'speedbar-item-info)
  (define-key speedbar-file-key-map "B" 'speedbar-item-byte-compile)
  (define-key speedbar-file-key-map "L" 'speedbar-item-load)
  (define-key speedbar-file-key-map "C" 'speedbar-item-copy)
  (define-key speedbar-file-key-map "D" 'speedbar-item-delete)
  (define-key speedbar-file-key-map "O" 'speedbar-item-object-delete)
  (define-key speedbar-file-key-map "R" 'speedbar-item-rename)
  )

(defvar speedbar-easymenu-definition-base
  (append
   '("Speedbar"
     ["Update" speedbar-refresh t]
     ["Auto Update" speedbar-toggle-updates
      :style toggle :selected speedbar-update-flag])
   (if (and (or (fboundp 'defimage)
		(fboundp 'make-image-specifier))
	    (if (fboundp 'display-graphic-p)
		(display-graphic-p)
	      window-system))
       (list
	["Use Images" speedbar-toggle-images
	 :style toggle :selected speedbar-use-images]))
   )
  "Base part of the speedbar menu.")

(defvar speedbar-easymenu-definition-special
  '(["Edit Item On Line" speedbar-edit-line t]
    ["Show All Files" speedbar-toggle-show-all-files
     :style toggle :selected speedbar-show-unknown-files]
    ["Expand File Tags" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Flush Cache & Expand" speedbar-flush-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract File Tags" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
;    ["Sort Tags" speedbar-toggle-sorting
;     :style toggle :selected speedbar-sort-tags]
    "----"
    ["File/Tag Information" speedbar-item-info t]
    ["Load Lisp File" speedbar-item-load
     (save-excursion
       (beginning-of-line)
       (looking-at "[0-9]+: *\\[[+-]\\] .+\\(\\.el\\)\\( \\|$\\)"))]
    ["Byte Compile File" speedbar-item-byte-compile
     (save-excursion
       (beginning-of-line)
       (looking-at "[0-9]+: *\\[[+-]\\] .+\\(\\.el\\)\\( \\|$\\)"))]
    ["Copy File" speedbar-item-copy
     (save-excursion (beginning-of-line) (looking-at "[0-9]+: *\\["))]
    ["Rename File" speedbar-item-rename
     (save-excursion (beginning-of-line) (looking-at "[0-9]+: *[[<]"))]
    ["Delete File" speedbar-item-delete
     (save-excursion (beginning-of-line) (looking-at "[0-9]+: *[[<]"))]
    ["Delete Object" speedbar-item-object-delete
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *\\[[+-]\\] [^ \n]+ \\*?[!#]$"))]
    )
  "Additional menu items while in file-mode.")
 
(defvar speedbar-easymenu-definition-trailer
  (append
   (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
       (list ["Customize..." speedbar-customize t]))
   (list
    ["Close" speedbar-close-frame t]
    ["Quit" delete-frame t] ))
  "Menu items appearing at the end of the speedbar menu.")

(defvar speedbar-desired-buffer nil
  "Non-nil when speedbar is showing buttons specific a special mode.
In this case it is the originating buffer.")
(defvar speedbar-buffer nil
  "The buffer displaying the speedbar.")
(defvar speedbar-frame nil
  "The frame displaying speedbar.")
(defvar speedbar-cached-frame nil
  "The frame that was last created, then removed from the display.")
(defvar speedbar-full-text-cache nil
  "The last open directory is saved in its entirety for ultra-fast switching.")
(defvar speedbar-timer nil
  "The speedbar timer used for updating the buffer.")
(defvar speedbar-attached-frame nil
  "The frame which started speedbar mode.
This is the frame from which all data displayed in the speedbar is
gathered, and in which files and such are displayed.")

(defvar speedbar-last-selected-file nil
  "The last file which was selected in speedbar buffer.")

(defvar speedbar-shown-directories nil
  "Maintain list of directories simultaneously open in the current speedbar.")

(defvar speedbar-directory-contents-alist nil
  "An association list of directories and their contents.
Each sublist was returned by `speedbar-file-lists'.  This list is
maintained to speed up the refresh rate when switching between
directories.")

(defvar speedbar-power-click nil
  "Never set this by hand.  Value is t when S-mouse activity occurs.")


;;; Compatibility
;;
(if (fboundp 'frame-parameter)

    (defalias 'speedbar-frame-parameter 'frame-parameter)
  
  (defun speedbar-frame-parameter (frame parameter)
    "Return FRAME's PARAMETER value."
    (cdr (assoc parameter (frame-parameters frame)))))

(if (fboundp 'make-overlay)
    (progn
      (defalias 'speedbar-make-overlay 'make-overlay)
      (defalias 'speedbar-overlay-put 'overlay-put)
      (defalias 'speedbar-delete-overlay 'delete-overlay)
      (defalias 'speedbar-overlay-start 'overlay-start)
      (defalias 'speedbar-overlay-end 'overlay-end)
      (defalias 'speedbar-mode-line-update 'force-mode-line-update))
  (defalias 'speedbar-make-overlay 'make-extent)
  (defalias 'speedbar-overlay-put 'set-extent-property)
  (defalias 'speedbar-delete-overlay 'delete-extent)
  (defalias 'speedbar-overlay-start 'extent-start)
  (defalias 'speedbar-overlay-end 'extent-end)
  (defalias 'speedbar-mode-line-update 'redraw-modeline))

;;; Mode definitions/ user commands
;;

;;;###autoload
(defalias 'speedbar 'speedbar-frame-mode)
;;;###autoload
(defun speedbar-frame-mode (&optional arg)
  "Enable or disable speedbar.  Positive ARG means turn on, negative turn off.
nil means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time.
`speedbar-before-popup-hook' is called before popping up the speedbar frame.
`speedbar-before-delete-hook' is called before the frame is deleted."
  (interactive "P")
  ;; toggle frame on and off.
  (if (not arg) (if (and (frame-live-p speedbar-frame)
			 (frame-visible-p speedbar-frame))
		    (setq arg -1) (setq arg 1)))
  ;; turn the frame off on neg number
  (if (and (numberp arg) (< arg 0))
      (progn
	(run-hooks 'speedbar-before-delete-hook)
	(if (and speedbar-frame (frame-live-p speedbar-frame))
	    (progn
	      (setq speedbar-cached-frame speedbar-frame)
	      (make-frame-invisible speedbar-frame)))
	(setq speedbar-frame nil)
	(speedbar-set-timer nil)
	;; Used to delete the buffer.  This has the annoying affect of
	;; preventing whatever took its place from ever appearing
	;; as the default after a C-x b was typed
	;;(if (bufferp speedbar-buffer)
	;;    (kill-buffer speedbar-buffer))
	)
    ;; Set this as our currently attached frame
    (setq speedbar-attached-frame (selected-frame))
    (run-hooks 'speedbar-before-popup-hook)
    ;; Get the frame to work in
    (if (frame-live-p speedbar-cached-frame)
	(progn
	  (setq speedbar-frame speedbar-cached-frame)
	  (make-frame-visible speedbar-frame)
	  ;; Get the buffer to play with
	  (speedbar-mode)
	  (select-frame speedbar-frame)
	  (if (not (eq (current-buffer) speedbar-buffer))
	      (switch-to-buffer speedbar-buffer))
	  (set-window-dedicated-p (selected-window) t)
	  (raise-frame speedbar-frame)
	  (speedbar-set-timer speedbar-update-speed)
	  )
      (if (frame-live-p speedbar-frame)
	  (raise-frame speedbar-frame)
	(setq speedbar-frame
	      (if speedbar-xemacsp
		  ;; Only guess height if it is not specified.
		  (if (member 'height speedbar-frame-plist)
		      (make-frame speedbar-frame-plist)
		    (make-frame (nconc (list 'height
					     (speedbar-needed-height))
				       speedbar-frame-plist)))
		(let* ((mh (speedbar-frame-parameter nil 'menu-bar-lines))
		       (cfx (speedbar-frame-parameter nil 'left))
		       (cfy (speedbar-frame-parameter nil 'top))
		       (cfw (frame-pixel-width))
		       (params
			;; Only add a guessed height if one is not specified
			;; in the input parameters.
			(if (assoc 'height speedbar-frame-parameters)
			    speedbar-frame-parameters
			  (append
			   speedbar-frame-parameters
			   (list (cons 'height (+ mh (frame-height)))))))
		       (frame
			(if (or (< emacs-major-version 20)
				(not (eq window-system 'x)))
			    (make-frame params)
			  (let ((x-pointer-shape x-pointer-top-left-arrow)
				(x-sensitive-text-pointer-shape
				 x-pointer-hand2))
			    (make-frame params)))))
		  ;; Position speedbar frame.
		  (if (or (not window-system) (eq window-system 'pc)
			  (assoc 'left speedbar-frame-parameters)
			  (assoc 'top speedbar-frame-parameters))
		      ;; Do no positioning if not on a windowing system,
		      ;; or if left/top were specified in the parameters.
		      frame
		    (let ((cfx
			   (if (not (consp cfx))
			       cfx
			     ;; If cfx is a list, that means we grow
			     ;; from a specific edge of the display.
			     ;; Convert that to the distance from the
			     ;; left side of the display.
			     (if (eq (car cfx) '-)
				 ;; A - means distance from the right edge
				 ;; of the display, or DW - cfx - framewidth
				 (- (x-display-pixel-width) (car (cdr cfx))
				    (frame-pixel-width))
			       (car (cdr cfx))))))
		      (modify-frame-parameters
		       frame
		       (list
			(cons
			 'left
			 ;; Decide which side to put it
			 ;; on.  200 is just a buffer
			 ;; for the left edge of the
			 ;; screen.  The extra 10 is just
			 ;; dressings for window decorations.
			 (let ((sfw (frame-pixel-width frame)))
			   (let ((left-guess (- cfx 10 sfw))
				 (right-guess (+ cfx cfw 5)))
			     (let ((left-margin left-guess)
				   (right-margin
				    (- (x-display-pixel-width)
				       right-guess 5 sfw)))
			       (cond ((>= left-margin 0) left-guess)
				     ((>= right-margin 0) right-guess)
				     ;; otherwise choose side we overlap less
				     ((> left-margin right-margin) 0)
				     (t (- (x-display-pixel-width) sfw 5)))))))
			(cons 'top cfy)))
		      frame)))))
	;; reset the selection variable
	(setq speedbar-last-selected-file nil)
	;; Put the buffer into the frame
	(save-window-excursion
	  ;; Get the buffer to play with
	  (speedbar-mode)
	  (select-frame speedbar-frame)
	  (switch-to-buffer speedbar-buffer)
	  (set-window-dedicated-p (selected-window) t))
	(if (and (or (null window-system) (eq window-system 'pc))
		 (fboundp 'set-frame-name))
	    (progn
	      (select-frame speedbar-frame)
	      (set-frame-name "Speedbar")))
	(speedbar-set-timer speedbar-update-speed)))))

;;;###autoload
(defun speedbar-get-focus ()
  "Change frame focus to or from the speedbar frame.
If the selected frame is not speedbar, then speedbar frame is
selected.  If the speedbar frame is active, then select the attached frame."
  (interactive)
  (if (eq (selected-frame) speedbar-frame)
      (if (frame-live-p speedbar-attached-frame)
	  (select-frame speedbar-attached-frame))
    ;; If updates are off, then refresh the frame (they want it now...)
    (if (not speedbar-update-flag)
	(let ((speedbar-update-flag t))
	  (speedbar-timer-fn)))
    ;; make sure we have a frame
    (if (not (frame-live-p speedbar-frame)) (speedbar-frame-mode 1))
    ;; go there
    (select-frame speedbar-frame)
    )
  (other-frame 0))

(defun speedbar-close-frame ()
  "Turn off a currently active speedbar."
  (interactive)
  (speedbar-frame-mode -1)
  (select-frame speedbar-attached-frame)
  (other-frame 0))

(defun speedbar-switch-buffer-attached-frame (&optional buffer)
  "Switch to BUFFER in speedbar's attached frame, and raise that frame.
This overrides the default behavior of `switch-to-buffer' which is
broken because of the dedicated speedbar frame."
  (interactive)
  ;; Assume we are in the speedbar frame.
  (speedbar-get-focus)
  ;; Now switch buffers
  (if buffer
      (switch-to-buffer buffer)
    (call-interactively 'switch-to-buffer nil nil)))

(defmacro speedbar-frame-width ()
  "Return the width of the speedbar frame in characters.
nil if it doesn't exist."
  '(frame-width speedbar-frame))

;; XEmacs function only.
(defun speedbar-needed-height (&optional frame)
  "The needed height for the tool bar FRAME (in characters)."
  (or frame (setq frame (selected-frame)))
  ;; The 1 is the missing modeline/minibuffer
  (+ 1 (/ (frame-pixel-height frame)
	  (face-height 'default frame))))

(defun speedbar-mode ()
  "Major mode for managing a display of directories and tags.
\\<speedbar-key-map>
The first line represents the default path of the speedbar frame.
Each directory segment is a button which jumps speedbar's default
directory to that path.  Buttons are activated by clicking `\\[speedbar-click]'.
In some situations using `\\[speedbar-power-click]' is a `power click' which will
rescan cached items, or pop up new frames.

Each line starting with <+> represents a directory.  Click on the <+>
to insert the directory listing into the current tree.  Click on the
<-> to retract that list.  Click on the directory name to go to that
directory as the default.

Each line starting with [+] is a file.  If the variable
`speedbar-show-unknown-files' is t, the lines starting with [?] are
files which don't have imenu support, but are not expressly ignored.
Files are completely ignored if they match `speedbar-file-unshown-regexp'
which is generated from `completion-ignored-extensions'.

Files with a `*' character after their name are files checked out of a
version control system.  (currently only RCS is supported.)  New
version control systems can be added by examining the documentation
for `speedbar-this-file-in-vc' and `speedbar-vc-check-dir-p'

Files with a `#' or `!' character after them are source files that
have an object file associated with them.  The `!' indicates that the
files is out of date.   You can control what source/object associations
exist through the variable `speedbar-obj-alist'.

Click on the [+] to display a list of tags from that file.  Click on
the [-] to retract the list.  Click on the file name to edit the file
in the attached frame.

If you open tags, you might find a node starting with {+}, which is a
category of tags.  Click the {+} to expand the category.  Jump-able
tags start with >.  Click the name of the tag to go to that position
in the selected file.

\\{speedbar-key-map}"
  ;; NOT interactive
  (save-excursion
    (setq speedbar-buffer (set-buffer (get-buffer-create " SPEEDBAR")))
    (kill-all-local-variables)
    (setq major-mode 'speedbar-mode)
    (setq mode-name "Speedbar")
    (set-syntax-table speedbar-syntax-table)
    (setq font-lock-keywords nil) ;; no font-locking please
    (setq truncate-lines t)
    (make-local-variable 'frame-title-format)
    (setq frame-title-format "Speedbar")
    ;; Set this up special just for the speedbar buffer
    ;; Terminal minibuffer stuff does not require this.
    (if (and window-system (not (eq window-system 'pc))
	     (null default-minibuffer-frame))
	(progn
	  (make-local-variable 'default-minibuffer-frame)
	  (setq default-minibuffer-frame speedbar-attached-frame)))
    ;; Correct use of `temp-buffer-show-function': Bob Weiner
    (if (and (boundp 'temp-buffer-show-hook)
	     (boundp 'temp-buffer-show-function))
	(progn (make-local-variable 'temp-buffer-show-hook)
	       (setq temp-buffer-show-hook temp-buffer-show-function)))
    (make-local-variable 'temp-buffer-show-function)
    (setq temp-buffer-show-function 'speedbar-temp-buffer-show-function)
    (if speedbar-xemacsp
	(progn
	  ;; Argh!  mouse-track-click-hook doesn't understand the
	  ;; make-local-hook conventions.
	  (make-local-variable 'mouse-track-click-hook)
	  (add-hook 'mouse-track-click-hook
		    (lambda (event count)
		      (if (/= (event-button event) 1)
			  nil		; Do normal operations.
			(cond ((eq count 1)
			       (speedbar-quick-mouse event))
			      ((or (eq count 2)
				   (eq count 3))
			       (speedbar-mouse-set-point event)
			       (speedbar-do-function-pointer)
			       (speedbar-quick-mouse event)))
			;; Don't do normal operations.
			t)))))
    (add-hook 'kill-buffer-hook (lambda () (let ((skilling (boundp 'skilling)))
					     (if skilling
						 nil
					       (if (eq (current-buffer)
						       speedbar-buffer)
						   (speedbar-frame-mode -1)))))
	      t t)
    (toggle-read-only 1)
    (speedbar-set-mode-line-format)
    (if speedbar-xemacsp
	(set (make-local-variable 'mouse-motion-handler)
	     'speedbar-track-mouse-xemacs)
      (if speedbar-track-mouse-flag
	  (set (make-local-variable 'track-mouse) t))	;this could be messy.
      (setq auto-show-mode nil))	;no auto-show for Emacs
    (run-hooks 'speedbar-mode-hook))
  (speedbar-update-contents)
  speedbar-buffer)

(defmacro speedbar-with-attached-buffer (&rest forms)
  "Execute FORMS in the attached frame's special buffer.
Optionally select that frame if necessary."
  `(save-selected-window
     (speedbar-set-timer speedbar-update-speed)
     (select-frame speedbar-attached-frame)
     ,@forms
     (speedbar-maybee-jump-to-attached-frame)))

(defun speedbar-message (fmt &rest args)
  "Like message, but for use in the speedbar frame.
Argument FMT is the format string, and ARGS are the arguments for message."
  (save-selected-window
    (select-frame speedbar-attached-frame)
    (apply 'message fmt args)))

(defun speedbar-y-or-n-p (prompt)
  "Like `y-or-n-p', but for use in the speedbar frame.
Argument PROMPT is the prompt to use."
  (save-selected-window
    (if (and default-minibuffer-frame (not (eq default-minibuffer-frame
					       speedbar-attached-frame)))
	(select-frame speedbar-attached-frame))
    (y-or-n-p prompt)))

(defun speedbar-show-info-under-mouse (&optional event)
  "Call the info function for the line under the mouse.
Optional EVENT is currently not used."
  (let ((pos (mouse-position)))  ; we ignore event until I use it later.
    (if (equal (car pos) speedbar-frame)
	(save-excursion
	  (save-window-excursion
	    (apply 'set-mouse-position pos)
	    (speedbar-item-info))))))

(defun speedbar-set-mode-line-format ()
  "Set the format of the mode line based on the current speedbar environment.
This gives visual indications of what is up.  It EXPECTS the speedbar
frame and window to be the currently active frame and window."
  (if (and (frame-live-p speedbar-frame)
	   (or (not speedbar-xemacsp)
	       (specifier-instance has-modeline-p)))
      (save-excursion
	(set-buffer speedbar-buffer)
	(let* ((w (or (speedbar-frame-width) 20))
	       (p1 "<<")
	       (p5 ">>")
	       (p3 (if speedbar-update-flag "SPEEDBAR" "SLOWBAR"))
	       (blank (- w (length p1) (length p3) (length p5)
			 (if line-number-mode 4 0)))
	       (p2 (if (> blank 0)
		       (make-string (/ blank 2) ? )
		     ""))
	       (p4 (if (> blank 0)
		       (make-string (+ (/ blank 2) (% blank 2)) ? )
		     ""))
	       (tf
		(if line-number-mode
		    (list (concat p1 p2 p3) '(line-number-mode " %3l")
			  (concat p4 p5))
		  (list (concat p1 p2 p3 p4 p5)))))
	  (if (not (equal mode-line-format tf))
	      (progn
		(setq mode-line-format tf)
		(speedbar-mode-line-update)))))))

(defun speedbar-temp-buffer-show-function (buffer)
  "Placed in the variable `temp-buffer-show-function' in `speedbar-mode'.
If a user requests help using \\[help-command] <Key> the temp BUFFER will be
redirected into a window on the attached frame."
  (if speedbar-attached-frame (select-frame speedbar-attached-frame))
  (pop-to-buffer buffer nil)
  (other-window -1)
  ;; Fix for using this hook on some platforms: Bob Weiner
  (cond ((not speedbar-xemacsp)
	 (run-hooks 'temp-buffer-show-hook))
	((fboundp 'run-hook-with-args)
	 (run-hook-with-args 'temp-buffer-show-hook buffer))
	((and (boundp 'temp-buffer-show-hook)
	      (listp temp-buffer-show-hook))
	 (mapcar (function (lambda (hook) (funcall hook buffer)))
		 temp-buffer-show-hook))))

(defvar speedbar-previous-menu nil
  "The menu before the last `speedbar-reconfigure-keymaps' was called.")

(defun speedbar-reconfigure-keymaps ()
  "Reconfigure the menu-bar in a speedbar frame.
Different menu items are displayed depending on the current display mode
and the existence of packages."
  (let ((md (append
	     speedbar-easymenu-definition-base
	     (if speedbar-shown-directories
		 ;; file display mode version
		 (speedbar-initial-menu)
	       (save-excursion
		 (select-frame speedbar-attached-frame)
		 (if (local-variable-p
		      'speedbar-easymenu-definition-special
		      (current-buffer))
		     ;; If bound locally, we can use it
		     speedbar-easymenu-definition-special)))
	     ;; Dynamic menu stuff
	     '("-")
	    (list (cons "Displays"
			(let ((displays nil)
			      (alist speedbar-initial-expansion-mode-alist))
			  (while alist
			    (setq displays
				  (cons
				   (vector
				    (capitalize (car (car alist)))
				    (list
				     'speedbar-change-initial-expansion-list
				     (car (car alist)))
				    t)
				   displays))
			    (setq alist (cdr alist)))
			  displays)))
	    ;; The trailer
	    speedbar-easymenu-definition-trailer))
	(localmap (save-excursion
		    (let ((cf (selected-frame)))
		      (prog2
			  (select-frame speedbar-attached-frame)
			  (if (local-variable-p
			       'speedbar-special-mode-key-map
			       (current-buffer))
			      speedbar-special-mode-key-map)
			(select-frame cf))))))
    (save-excursion
      (set-buffer speedbar-buffer)
      (use-local-map (or localmap
			 (speedbar-initial-keymap)
			 ;; This creates a small keymap we can glom the
			 ;; menu adjustments into.
			 (speedbar-make-specialized-keymap)))
      ;; Delete the old menu if applicable.
      (if speedbar-previous-menu (easy-menu-remove speedbar-previous-menu))
      (setq speedbar-previous-menu md)
      ;; Now add the new menu
      (if (not speedbar-xemacsp)
	  (easy-menu-define speedbar-menu-map (current-local-map)
			    "Speedbar menu" md)
	(easy-menu-add md (current-local-map))
	(set-buffer-menubar (list md))))
    (run-hooks 'speedbar-reconfigure-keymaps-hook)))


;;; User Input stuff
;;

;; XEmacs: this can be implemented using modeline keymaps, but there
;; is no use, as we have horizontal scrollbar (as the docstring
;; hints.)
(defun speedbar-mouse-hscroll (e)
  "Read a mouse event E from the mode line, and horizontally scroll.
If the mouse is being clicked on the far left, or far right of the
mode-line.  This is only useful for non-XEmacs"
  (interactive "e")
  (let* ((xp (car (nth 2 (car (cdr e)))))
	 (cpw (/ (frame-pixel-width)
		 (frame-width)))
	 (oc (1+ (/ xp cpw)))
	 )
    (cond ((< oc 3)
	   (scroll-left 2))
	  ((> oc (- (window-width) 3))
	   (scroll-right 2))
	  (t (speedbar-message
	      "Click on the edge of the modeline to scroll left/right")))
    ;;(speedbar-message "X: Pixel %d Char Pixels %d On char %d" xp cpw oc)
    ))

(defun speedbar-customize ()
  "Customize speedbar using the Custom package."
  (interactive)
  (let ((sf (selected-frame)))
    (select-frame speedbar-attached-frame)
    (customize-group 'speedbar)
    (select-frame sf))
  (speedbar-maybee-jump-to-attached-frame))

(defun speedbar-track-mouse (event)
  "For motion EVENT, display info about the current line."
  (interactive "e")
  (if (not speedbar-track-mouse-flag)
      nil
    (save-excursion
      (let ((char (nth 1 (car (cdr event)))))
	(if (not (numberp char))
	    (speedbar-message nil)
	  (goto-char char)
	  ;; (speedbar-message "%S" event)
	  (speedbar-item-info)
	  )))))

(defun speedbar-track-mouse-xemacs (event)
  "For motion EVENT, display info about the current line."
  (if (functionp (default-value 'mouse-motion-handler))
      (funcall (default-value 'mouse-motion-handler) event))
  (if speedbar-track-mouse-flag
      (save-excursion
	(save-window-excursion
	  (condition-case ()
	      (progn (mouse-set-point event)
		     ;; Prevent focus-related bugs.
		     (if (eq major-mode 'speedbar-mode)
			 (speedbar-item-info)))
	    (error nil))))))

;; In XEmacs, we make popup menus work on the item over mouse (as
;; opposed to where the point happens to be.)  We attain this by
;; temporarily moving the point to that place.
;;    Hrvoje Niksic <hniksic@srce.hr>
(defun speedbar-xemacs-popup-kludge (event)
  "Pop up a menu related to the clicked on item.
Must be bound to EVENT."
  (interactive "e")
  (select-frame speedbar-frame)
  (save-excursion
    (goto-char (event-closest-point event))
    (beginning-of-line)
    (forward-char (min 5 (- (save-excursion (end-of-line) (point))
			    (save-excursion (beginning-of-line) (point)))))
    (popup-mode-menu)
    ;; Wait for menu to bail out.  `popup-mode-menu' (and other popup
    ;; menu functions) return immediately.
    (let (new)
      (while (not (misc-user-event-p (setq new (next-event))))
	(dispatch-event new))
      (dispatch-event new))))

(defun speedbar-emacs-popup-kludge (e)
  "Pop up a menu related to the clicked on item.
Must be bound to event E."
  (interactive "e")
  (save-excursion
    (mouse-set-point e)
    ;; This gets the cursor where the user can see it.
    (if (not (bolp)) (forward-char -1))
    (sit-for 0)
    (if (< emacs-major-version 20)
	(mouse-major-mode-menu e)
      (mouse-major-mode-menu e nil))))

(defun speedbar-hack-buffer-menu (e)
  "Control mouse 1 is buffer menu.
This hack overrides it so that the right thing happens in the main
Emacs frame, not in the speedbar frame.
Argument E is the event causing this activity."
  (interactive "e")
  (let ((fn (lookup-key global-map (if speedbar-xemacsp
				              '(control button1)
				     [C-down-mouse-1])))
	(newbuff nil))
    (unwind-protect
	(save-excursion
	  (set-window-dedicated-p (selected-window) nil)
	  (call-interactively fn)
	  (setq newbuff (current-buffer)))
      (switch-to-buffer speedbar-buffer)
      (set-window-dedicated-p (selected-window) t))
    (if (not (eq newbuff speedbar-buffer))
	(speedbar-with-attached-buffer
	 (switch-to-buffer newbuff)))))

(defun speedbar-next (arg)
  "Move to the next ARGth line in a speedbar buffer."
  (interactive "p")
  (forward-line (or arg 1))
  (speedbar-item-info)
  (speedbar-position-cursor-on-line))

(defun speedbar-prev (arg)
  "Move to the previous ARGth line in a speedbar buffer."
  (interactive "p")
  (speedbar-next (if arg (- arg) -1)))

(defun speedbar-restricted-move (arg)
  "Move to the next ARGth line in a speedbar buffer at the same depth.
This means that movement is restricted to a subnode, and that siblings
of intermediate nodes are skipped."
  (if (not (numberp arg)) (signal 'wrong-type-argument (list arg 'numberp)))
  ;; First find the extent for which we are allowed to move.
  (let ((depth (save-excursion (beginning-of-line)
			       (if (looking-at "[0-9]+:")
				   (string-to-int (match-string 0))
				 0)))
	(crement (if (< arg 0) 1 -1)) ; decrement or increment
	(lastmatch (point)))
    (while (/= arg 0)
      (forward-line (- crement))
      (let ((subdepth (save-excursion (beginning-of-line)
			       (if (looking-at "[0-9]+:")
				   (string-to-int (match-string 0))
				 0))))
	(cond ((or (< subdepth depth)
		   (progn (end-of-line) (eobp))
		   (progn (beginning-of-line) (bobp)))
	       ;; We have reached the end of this block.
	       (goto-char lastmatch)
	       (setq arg 0)
	       (error "End of sub-list"))
	      ((= subdepth depth)
	       (setq lastmatch (point)
		     arg (+ arg crement))))))
    (speedbar-position-cursor-on-line)))

(defun speedbar-restricted-next (arg)
  "Move to the next ARGth line in a speedbar buffer at the same depth.
This means that movement is restricted to a subnode, and that siblings
of intermediate nodes are skipped."
  (interactive "p")
  (speedbar-restricted-move (or arg 1))
  (speedbar-item-info))


(defun speedbar-restricted-prev (arg)
  "Move to the previous ARGth line in a speedbar buffer at the same depth.
This means that movement is restricted to a subnode, and that siblings
of intermediate nodes are skipped."
  (interactive "p")
  (speedbar-restricted-move (if arg (- arg) -1))
  (speedbar-item-info))

(defun speedbar-navigate-list (arg)
  "Move across ARG groups of similarly typed items in speedbar.
Stop on the first line of the next type of item, or on the last or first item
if we reach a buffer boundary."
  (interactive "p")
  (beginning-of-line)
  (if (looking-at "[0-9]+: *[[<{][-+?][]>}] ")
      (let ((str (regexp-quote (match-string 0))))
	(while (looking-at str)
	  (speedbar-restricted-move arg)
	  (beginning-of-line))))
  (speedbar-position-cursor-on-line))

(defun speedbar-forward-list ()
  "Move forward over the current list.
A LIST in speedbar is a group of similarly typed items, such as directories,
files, or the directory button."
  (interactive)
  (speedbar-navigate-list 1)
  (speedbar-item-info))

(defun speedbar-backward-list ()
  "Move backward over the current list.
A LIST in speedbar is a group of similarly typed items, such as directories,
files, or the directory button."
  (interactive)
  (speedbar-navigate-list -1)
  (speedbar-item-info))

(defun speedbar-scroll-up (&optional arg)
  "Page down one screen-full of the speedbar, or ARG lines."
  (interactive "P")
  (scroll-up arg)
  (speedbar-position-cursor-on-line))

(defun speedbar-scroll-down (&optional arg)
  "Page up one screen-full of the speedbar, or ARG lines."
  (interactive "P")
  (scroll-down arg)
  (speedbar-position-cursor-on-line))

(defun speedbar-up-directory ()
  "Keyboard accelerator for moving the default directory up one.
Assumes that the current buffer is the speedbar buffer"
  (interactive)
  (setq default-directory (expand-file-name (concat default-directory "../")))
  (speedbar-update-contents))

;;; Speedbar file activity (aka creeping featurism)
;;
(defun speedbar-refresh ()
  "Refresh the current speedbar display, disposing of any cached data."
  (interactive)
  (let ((dl speedbar-shown-directories)
	(dm (and (boundp 'deactivate-mark) deactivate-mark)))
    (while dl
      (adelete 'speedbar-directory-contents-alist (car dl))
      (setq dl (cdr dl)))
    (if (<= 1 speedbar-verbosity-level)
	(speedbar-message "Refreshing speedbar..."))
    (speedbar-update-contents)
    (speedbar-stealthy-updates)
    ;; Reset the timer in case it got really hosed for some reason...
    (speedbar-set-timer speedbar-update-speed)
    (if (<= 1 speedbar-verbosity-level)
	(speedbar-message "Refreshing speedbar...done"))
    (if (boundp 'deactivate-mark) (setq deactivate-mark dm))))

(defun speedbar-item-load ()
  "Load the item under the cursor or mouse if it is a Lisp file."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if (and (file-exists-p f) (string-match "\\.el\\'" f))
	(if (and (file-exists-p (concat f "c"))
		 (speedbar-y-or-n-p (format "Load %sc? " f)))
	    ;; If the compiled version exists, load that instead...
	    (load-file (concat f "c"))
	  (load-file f))
      (error "Not a loadable file"))))

(defun speedbar-item-byte-compile ()
  "Byte compile the item under the cursor or mouse if it is a Lisp file."
  (interactive)
  (let ((f (speedbar-line-file))
	(sf (selected-frame)))
    (if (and (file-exists-p f) (string-match "\\.el\\'" f))
	(progn
	  (select-frame speedbar-attached-frame)
	  (byte-compile-file f nil)
	  (select-frame sf)
	  (speedbar-reset-scanners)))
    ))

(defun speedbar-mouse-item-info (event)
  "Provide information about what the user clicked on.
This should be bound to a mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (speedbar-item-info))

(defun speedbar-generic-item-info ()
  "Attempt to derive, and then display information about thils line item.
File style information is displayed with `speedbar-item-info'."
  (save-excursion
    (beginning-of-line)
    ;; Skip invisible number info.
    (if (looking-at "\\([0-9]+\\):") (goto-char (match-end 0)))
    ;; Skip items in "folder" type text characters.
    (if (looking-at "\\s-*[[<({].[]>)}] ") (goto-char (match-end 0)))
    ;; Get the text
    (speedbar-message "Text: %s" (buffer-substring-no-properties
				  (point) (progn (end-of-line) (point))))))

(defun speedbar-item-info ()
  "Display info in the mini-buffer about the button the mouse is over.
This function can be replaced in `speedbar-mode-functions-list' as
`speedbar-item-info'"
  (interactive)
  (let (message-log-max)
    (funcall (or (speedbar-fetch-replacement-function 'speedbar-item-info)
		 'speedbar-generic-item-info))))

(defun speedbar-item-info-file-helper (&optional filename)
  "Display info about a file that is on the current line.
nil if not applicable.  If FILENAME, then use that instead of reading
it from the speedbar buffer."
  (let* ((item (or filename (speedbar-line-file)))
	 (attr (if item (file-attributes item) nil)))
    (if (and item attr) (speedbar-message "%s %-6d %s" (nth 8 attr)
					  (nth 7 attr) item)
      nil)))

(defun speedbar-item-info-tag-helper ()
  "Display info about a tag that is on the current line.
nil if not applicable."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward " [-+=]?> \\([^\n]+\\)"
			   (save-excursion(end-of-line)(point)) t)
	(let ((tag (match-string 1))
	      (attr (speedbar-line-token))
	      (item nil))
	  (if (and (featurep 'semantic) (semantic-token-p attr))
	      (speedbar-message (semantic-summerize-nonterminal attr))
	    (looking-at "\\([0-9]+\\):")
	    (setq item (file-name-nondirectory (speedbar-line-path)))
	    (speedbar-message "Tag: %s  in %s" tag item)))
      (if (re-search-forward "{[+-]} \\([^\n]+\\)$"
			     (save-excursion(end-of-line)(point)) t)
	  (speedbar-message "Group of tags \"%s\"" (match-string 1))
	(if (re-search-forward " [+-]?[()|@] \\([^\n]+\\)$" nil t)
	    (let* ((detailtext (match-string 1))
		   (detail (or (speedbar-line-token) detailtext))
		  (parent (save-excursion
			    (beginning-of-line)
			    (let ((dep (if (looking-at "[0-9]+:")
					   (1- (string-to-int (match-string 0)))
					 0)))
			      (re-search-backward (concat "^"
							   (int-to-string dep)
							   ":")
						  nil t))
			    (if (looking-at "[0-9]+: +[-+=>]> \\([^\n]+\\)$")
				(speedbar-line-token)
			      nil))))
	      (if (and (featurep 'semantic) (semantic-token-p detail))
		  (speedbar-message
		   (semantic-summerize-nonterminal detail parent))
		(if parent
		    (speedbar-message "Detail: %s of tag %s" detail
				      (if (and (featurep 'semantic)
					       (semantic-token-p parent))
					  (semantic-token-name parent)
					parent))
		  (speedbar-message "Detail: %s" detail))))
	  nil)))))

(defun speedbar-files-item-info ()
  "Display info in the mini-buffer about the button the mouse is over."
  (if (not speedbar-shown-directories)
      (speedbar-generic-item-info)
    (or (speedbar-item-info-file-helper)
	(speedbar-item-info-tag-helper)
	(speedbar-generic-item-info))))

(defun speedbar-item-copy ()
  "Copy the item under the cursor.
Files can be copied to new names or places."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if (not f)	(error "Not a file"))
    (if (file-directory-p f)
	(error "Cannot copy directory")
      (let* ((rt (read-file-name (format "Copy %s to: "
					 (file-name-nondirectory f))
				 (file-name-directory f)))
	     (refresh (member (expand-file-name (file-name-directory rt))
			      speedbar-shown-directories)))
	;; Create the right file name part
	(if (file-directory-p rt)
	    (setq rt
		  (concat (expand-file-name rt)
			  (if (string-match "[/\\]$" rt) "" "/")
			  (file-name-nondirectory f))))
	(if (or (not (file-exists-p rt))
		(speedbar-y-or-n-p (format "Overwrite %s with %s? " rt f)))
	    (progn
	      (copy-file f rt t t)
	      ;; refresh display if the new place is currently displayed.
	      (if refresh
		  (progn
		    (speedbar-refresh)
		    (if (not (speedbar-goto-this-file rt))
			(speedbar-goto-this-file f))))
	      ))))))

(defun speedbar-item-rename ()
  "Rename the item under the cursor or mouse.
Files can be renamed to new names or moved to new directories."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if f
	(let* ((rt (read-file-name (format "Rename %s to: "
					   (file-name-nondirectory f))
				   (file-name-directory f)))
	       (refresh (member (expand-file-name (file-name-directory rt))
				speedbar-shown-directories)))
	  ;; Create the right file name part
	  (if (file-directory-p rt)
	      (setq rt
		    (concat (expand-file-name rt)
			    (if (string-match "[/\\]\\'" rt) "" "/")
			    (file-name-nondirectory f))))
	  (if (or (not (file-exists-p rt))
		  (speedbar-y-or-n-p (format "Overwrite %s with %s? " rt f)))
	      (progn
		(rename-file f rt t)
		;; refresh display if the new place is currently displayed.
		(if refresh
		    (progn
		      (speedbar-refresh)
		      (speedbar-goto-this-file rt)
		      )))))
      (error "Not a file"))))

(defun speedbar-item-delete ()
  "Delete the item under the cursor.  Files are removed from disk."
  (interactive)
  (let ((f (speedbar-line-file)))
    (if (not f) (error "Not a file"))
    (if (speedbar-y-or-n-p (format "Delete %s? " f))
	(progn
	  (if (file-directory-p f)
	      (delete-directory f)
	    (delete-file f))
	  (speedbar-message "Okie dokie..")
	  (let ((p (point)))
	    (speedbar-refresh)
	    (goto-char p))
	  ))
    ))

(defun speedbar-item-object-delete ()
  "Delete the object associated from the item under the cursor.
The file is removed from disk.  The object is determined from the
variable `speedbar-obj-alist'."
  (interactive)
  (let* ((f (speedbar-line-file))
	 (obj nil)
	 (oa speedbar-obj-alist))
    (if (not f) (error "Not a file"))
    (while (and oa (not (string-match (car (car oa)) f)))
      (setq oa (cdr oa)))
    (setq obj (concat (file-name-sans-extension f) (cdr (car oa))))
    (if (and oa (file-exists-p obj)
	     (speedbar-y-or-n-p (format "Delete %s? " obj)))
	(progn
	  (delete-file obj)
	  (speedbar-reset-scanners)))))

(defun speedbar-enable-update ()
  "Enable automatic updating in speedbar via timers."
  (interactive)
  (setq speedbar-update-flag t)
  (speedbar-set-mode-line-format)
  (speedbar-set-timer speedbar-update-speed))

(defun speedbar-disable-update ()
  "Disable automatic updating and stop consuming resources."
  (interactive)
  (setq speedbar-update-flag nil)
  (speedbar-set-mode-line-format)
  (speedbar-set-timer nil))

(defun speedbar-toggle-updates ()
  "Toggle automatic update for the speedbar frame."
  (interactive)
  (if speedbar-update-flag
      (speedbar-disable-update)
    (speedbar-enable-update)))

(defun speedbar-toggle-images ()
  "Toggle automatic update for the speedbar frame."
  (interactive)
  (setq speedbar-use-images (not speedbar-use-images))
  (speedbar-refresh))

(defun speedbar-toggle-sorting ()
  "Toggle automatic update for the speedbar frame."
  (interactive)
  (setq speedbar-sort-tags (not speedbar-sort-tags)))

(defun speedbar-toggle-show-all-files ()
  "Toggle display of files speedbar can not tag."
  (interactive)
  (setq speedbar-show-unknown-files (not speedbar-show-unknown-files))
  (speedbar-refresh))

;;; Utility functions
;;
(defun speedbar-set-timer (timeout)
  "Apply a timer with TIMEOUT, or remove a timer if TIMOUT is nil.
TIMEOUT is the number of seconds until the speedbar timer is called
again.  When TIMEOUT is nil, turn off all timeouts.
This function will also enable or disable the `vc-checkin-hook' used
to track file check ins, and will change the mode line to match
`speedbar-update-flag'."
  (cond
   ;; XEmacs
   (speedbar-xemacsp
    (if speedbar-timer
	(progn (delete-itimer speedbar-timer)
	       (setq speedbar-timer nil)))
    (if timeout
	(if (and speedbar-xemacsp
		 (or (>= emacs-major-version 20)
		     (>= emacs-minor-version 15)))
	    (setq speedbar-timer (start-itimer "speedbar"
					       'speedbar-timer-fn
					       timeout
					       timeout
					       t))
	  (setq speedbar-timer (start-itimer "speedbar"
					     'speedbar-timer-fn
					     timeout
					     nil)))))
   ;; Post 19.31 Emacs
   ((fboundp 'run-with-idle-timer)
    (if speedbar-timer
	(progn (cancel-timer speedbar-timer)
	       (setq speedbar-timer nil)))
    (if timeout
	(setq speedbar-timer
	      (run-with-idle-timer timeout t 'speedbar-timer-fn))))
   ;; Emacs 19.30 (Thanks twice: ptype@dra.hmg.gb)
   ((fboundp 'post-command-idle-hook)
    (if timeout
	(add-hook 'post-command-idle-hook 'speedbar-timer-fn)
      (remove-hook 'post-command-idle-hook 'speedbar-timer-fn)))
   ;; Older or other Emacsen with no timers.  Set up so that its
   ;; obvious this emacs can't handle the updates
   (t
    (setq speedbar-update-flag nil)))
  ;; Apply a revert hook that will reset the scanners.  We attach to revert
  ;; because most reverts occur during VC state change, and this lets our
  ;; VC scanner fix itself.
  (if timeout
      (add-hook 'after-revert-hook 'speedbar-reset-scanners)
    (remove-hook 'after-revert-hook 'speedbar-reset-scanners)
    )
  ;; change this if it changed for some reason
  (speedbar-set-mode-line-format))

(defmacro speedbar-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate FORMS."
  (list 'let '((inhibit-read-only t))
	(cons 'progn forms)))
(put 'speedbar-with-writable 'lisp-indent-function 0)

(defun speedbar-select-window (buffer)
  "Select a window in which BUFFER is shown.
If it is not shown, force it to appear in the default window."
  (let ((win (get-buffer-window buffer speedbar-attached-frame)))
    (if win
	(select-window win)
      (set-window-buffer (selected-window) buffer))))

(defun speedbar-insert-button (text face mouse function
				    &optional token prevline)
  "Insert TEXT as the next logical speedbar button.
FACE is the face to put on the button, MOUSE is the highlight face to use.
When the user clicks on TEXT, FUNCTION is called with the TOKEN parameter.
This function assumes that the current buffer is the speedbar buffer.
If PREVLINE, then put this button on the previous line.

This is a convenience function for special mode that create their own
specialized speedbar displays."
  (goto-char (point-max))
  (let ((start (point)))
    (if (/= (current-column) 0) (insert "\n"))
    (put-text-property start (point) 'invisible nil))
  (if prevline (progn (delete-char -1)
		      (insert " ") ;back up if desired...
		      (put-text-property (1- (point)) (point) 'invisible nil)))
  (let ((start (point)))
    (insert text)
    (speedbar-make-button start (point) face mouse function token))
  (let ((start (point)))
    (insert "\n")
    (put-text-property start (point) 'face nil)
    (put-text-property start (point) 'invisible nil)
    (put-text-property start (point) 'mouse-face nil)))

(defun speedbar-make-button (start end face mouse function &optional token)
  "Create a button from START to END, with FACE as the display face.
MOUSE is the mouse face.  When this button is clicked on FUNCTION
will be run with the TOKEN parameter (any Lisp object)"
  (put-text-property start end 'face face)
  (put-text-property start end 'mouse-face mouse)
  (put-text-property start end 'invisible nil)
  (if function (put-text-property start end 'speedbar-function function))
  (if token (put-text-property start end 'speedbar-token token))
  ;; So far the only text we have is less that 3 chars.
  (if (<= (- end start) 3)
      (speedbar-insert-image-button-maybe start (- end start)))
  )

;;; Initial Expansion list management
;;
(defun speedbar-initial-expansion-list ()
  "Return the current default expansion list.
This is based on `speedbar-initial-expansion-list-name' referencing
`speedbar-initial-expansion-mode-alist'."
  ;; cdr1 - name, cdr2 - menu
  (cdr (cdr (cdr (assoc speedbar-initial-expansion-list-name
			speedbar-initial-expansion-mode-alist)))))

(defun speedbar-initial-menu ()
  "Return the current default menu data.
This is based on `speedbar-initial-expansion-list-name' referencing
`speedbar-initial-expansion-mode-alist'."
  (symbol-value
   (car (cdr (assoc speedbar-initial-expansion-list-name
		    speedbar-initial-expansion-mode-alist)))))

(defun speedbar-initial-keymap ()
  "Return the current default menu data.
This is based on `speedbar-initial-expansion-list-name' referencing
`speedbar-initial-expansion-mode-alist'."
  (symbol-value
   (car (cdr (cdr (assoc speedbar-initial-expansion-list-name
			 speedbar-initial-expansion-mode-alist))))))

(defun speedbar-initial-stealthy-functions ()
  "Return a list of functions to call stealthily.
This is based on `speedbar-initial-expansion-list-name' referencing
`speedbar-stealthy-function-list'."
  (cdr (assoc speedbar-initial-expansion-list-name
	      speedbar-stealthy-function-list)))

(defun speedbar-add-expansion-list (new-list)
  "Add NEW-LIST to the list of expansion lists."
  (add-to-list 'speedbar-initial-expansion-mode-alist new-list))

(defun speedbar-change-initial-expansion-list (new-default)
  "Change speedbar's default expansion list to NEW-DEFAULT."
  (interactive
   (list
    (completing-read (format "Speedbar Mode (default %s): "
			     speedbar-previously-used-expansion-list-name)
		     speedbar-initial-expansion-mode-alist
		     nil t "" nil
		     speedbar-previously-used-expansion-list-name)))
  (setq speedbar-previously-used-expansion-list-name
	speedbar-initial-expansion-list-name
	speedbar-initial-expansion-list-name new-default)
  (speedbar-refresh)
  (speedbar-reconfigure-keymaps))

(defun speedbar-fetch-replacement-function (function)
  "Return a current mode specific replacement for function, or nil.
Scans `speedbar-mode-functions-list' first for the current mode, then
for FUNCTION."
  (cdr (assoc function
	      (cdr (assoc speedbar-initial-expansion-list-name
			  speedbar-mode-functions-list)))))

(defun speedbar-add-mode-functions-list (new-list)
  "Add NEW-LIST to the list of mode functions.
See `speedbar-mode-functions-list' for details."
  (add-to-list 'speedbar-mode-functions-list new-list))


;;; Special speedbar display management
;;
(defun speedbar-maybe-add-localized-support (buffer)
  "Quick check function called on BUFFERs by the speedbar timer function.
Maintains the value of local variables which control speedbars use
of the special mode functions."
  (or speedbar-special-mode-expansion-list
      (speedbar-add-localized-speedbar-support buffer)))

(defun speedbar-add-localized-speedbar-support (buffer)
  "Add localized speedbar support to BUFFER's mode if it is available."
  (interactive "bBuffer: ")
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (not (buffer-live-p buffer))
      nil
    (save-excursion
      (set-buffer buffer)
      (save-match-data
	(let ((ms (symbol-name major-mode)) v)
	  (if (not (string-match "-mode$" ms))
	      nil ;; do nothing to broken mode
	    (setq ms (substring ms 0 (match-beginning 0)))
	    (setq v (intern-soft (concat ms "-speedbar-buttons")))
	    (make-local-variable 'speedbar-special-mode-expansion-list)
	    (if (not v)
		(setq speedbar-special-mode-expansion-list t)
	      ;; If it is autoloaded, we need to load it now so that
	      ;; we have access to the varialbe -speedbar-menu-items.
	      ;; Is this XEmacs safe?
	      (let ((sf (symbol-function v)))
		(if (and (listp sf) (eq (car sf) 'autoload))
		    (load-library (car (cdr sf)))))
	      (setq speedbar-special-mode-expansion-list (list v))
	      (setq v (intern-soft (concat ms "-speedbar-key-map")))
	      (if (not v)
		  nil ;; don't add special keymap
		(make-local-variable 'speedbar-special-mode-key-map)
		(setq speedbar-special-mode-key-map
		      (symbol-value v)))
	      (setq v (intern-soft (concat ms "-speedbar-menu-items")))
	      (if (not v)
		  nil ;; don't add special menus
		(make-local-variable 'speedbar-easymenu-definition-special)
		(setq speedbar-easymenu-definition-special
		      (symbol-value v)))
	      )))))))

(defun speedbar-remove-localized-speedbar-support (buffer)
  "Remove any traces that BUFFER supports speedbar in a specialized way."
  (save-excursion
    (set-buffer buffer)
    (kill-local-variable 'speedbar-special-mode-expansion-list)
    (kill-local-variable 'speedbar-special-mode-key-map)
    (kill-local-variable 'speedbar-easymenu-definition-special)))

;;; File button management
;;
(defun speedbar-file-lists (directory)
  "Create file lists for DIRECTORY.
The car is the list of directories, the cdr is list of files not
matching ignored headers.  Cache any directory files found in
`speedbar-directory-contents-alist' and use that cache before scanning
the file-system"
  (setq directory (expand-file-name directory))
  ;; If in powerclick mode, then the directory we are getting
  ;; should be rescanned.
  (if speedbar-power-click
      (adelete 'speedbar-directory-contents-alist directory))
  ;; find the directory, either in the cache, or build it.
  (or (cdr-safe (assoc directory speedbar-directory-contents-alist))
      (let ((default-directory directory)
	    (dir (directory-files directory nil))
	    (dirs nil)
	    (files nil))
	(while dir
	  (if (not
	       (or (string-match speedbar-file-unshown-regexp (car dir))
		   (string-match speedbar-directory-unshown-regexp (car dir))))
	      (if (file-directory-p (car dir))
		  (setq dirs (cons (car dir) dirs))
		(setq files (cons (car dir) files))))
	  (setq dir (cdr dir)))
	(let ((nl (cons (nreverse dirs) (list (nreverse files)))))
	  (aput 'speedbar-directory-contents-alist directory nl)
	  nl))
      ))

(defun speedbar-directory-buttons (directory index)
  "Insert a single button group at point for DIRECTORY.
Each directory path part is a different button.  If part of the path
matches the user directory ~, then it is replaced with a ~.
INDEX is not used, but is required by the caller."
  (let* ((tilde (expand-file-name "~/"))
	 (dd (expand-file-name directory))
	 (junk (string-match (regexp-quote tilde) dd))
	 (displayme (if junk
			(concat "~/" (substring dd (match-end 0)))
		      dd))
	 (p (point)))
    (if (string-match "^~[/\\]?\\'" displayme) (setq displayme tilde))
    (insert displayme)
    (save-excursion
      (goto-char p)
      (while (re-search-forward "\\([^/\\]+\\)[/\\]" nil t)
	(speedbar-make-button (match-beginning 1) (match-end 1)
			      'speedbar-directory-face
			      'speedbar-highlight-face
			      'speedbar-directory-buttons-follow
			      (if (and (= (match-beginning 1) p)
				       (not (char-equal (char-after (+ p 1)) ?:)))
				  (expand-file-name "~/")  ;the tilde
				(buffer-substring-no-properties
				 p (match-end 0)))))
      ;; Nuke the beginning of the directory if it's too long...
      (cond ((eq speedbar-directory-button-trim-method 'span)
	     (beginning-of-line)
	     (let ((ww (or (speedbar-frame-width) 20)))
	       (move-to-column ww nil)
	       (while (>= (current-column) ww)
		 (re-search-backward "[/\\]" nil t)
		 (if (<= (current-column) 2)
		     (progn
		       (re-search-forward "[/\\]" nil t)
		       (if (< (current-column) 4)
			   (re-search-forward "[/\\]" nil t))
		       (forward-char -1)))
		 (if (looking-at "[/\\]?$")
		     (beginning-of-line)
		   (insert "/...\n ")
		   (move-to-column ww nil)))))
	    ((eq speedbar-directory-button-trim-method 'trim)
	     (end-of-line)
	     (let ((ww (or (speedbar-frame-width) 20))
		   (tl (current-column)))
	       (if (< ww tl)
		   (progn
		     (move-to-column (- tl ww))
		     (if (re-search-backward "[/\\]" nil t)
			 (progn
			   (delete-region (point-min) (point))
			   (insert "$")
			   )))))))
      )
    (if (string-match "\\`[/\\][^/\\]+[/\\]\\'" displayme)
	(progn
	  (insert "  ")
	  (let ((p (point)))
	    (insert "<root>")
	    (speedbar-make-button p (point)
				  'speedbar-directory-face
				  'speedbar-highlight-face
				  'speedbar-directory-buttons-follow
				  "/"))))
    (end-of-line)
    (insert-char ?\n 1 nil)))

(defun speedbar-make-tag-line (exp-button-type
			       exp-button-char exp-button-function
			       exp-button-data
			       tag-button tag-button-function tag-button-data
			       tag-button-face depth)
  "Create a tag line with EXP-BUTTON-TYPE for the small expansion button.
This is the button that expands or contracts a node (if applicable),
and EXP-BUTTON-CHAR the character in it (+, -, ?, etc).  EXP-BUTTON-FUNCTION
is the function to call if it's clicked on.  Button types are
'bracket, 'angle, 'curly, or nil.  EXP-BUTTON-DATA is extra data
attached to the text forming the expansion button.

Next, TAG-BUTTON is the text of the tag.  TAG-BUTTON-FUNCTION is the
function to call if clicked on, and TAG-BUTTON-DATA is the data to
attach to the text field (such a tag positioning, etc).
TAG-BUTTON-FACE is a face used for this type of tag.

Lastly, DEPTH shows the depth of expansion.

This function assumes that the cursor is in the speedbar window at the
position to insert a new item, and that the new item will end with a CR"
  (let ((start (point))
	(end (progn
	       (insert (int-to-string depth) ":")
	       (point)))
	(depthspacesize (* depth speedbar-indentation-width)))
    (put-text-property start end 'invisible t)
    (insert-char ?  depthspacesize nil)
    (put-text-property (- (point) depthspacesize) (point) 'invisible nil)
    (let* ((exp-button (cond ((eq exp-button-type 'bracket) "[%c]")
			     ((eq exp-button-type 'angle) "<%c>")
			     ((eq exp-button-type 'curly) "{%c}")
			     (t ">")))
	   (buttxt (format exp-button exp-button-char))
	   (start (point))
	   (end (progn (insert buttxt) (point)))
	   (bf (if exp-button-type 'speedbar-button-face nil))
	   (mf (if exp-button-function 'speedbar-highlight-face nil))
	   )
      (speedbar-make-button start end bf mf exp-button-function exp-button-data)
      (if speedbar-hide-button-brackets-flag
	  (progn
	    (put-text-property start (1+ start) 'invisible t)
	    (put-text-property end (1- end) 'invisible t)))
      )
    (insert-char ?  1 nil)
    (put-text-property (1- (point)) (point) 'invisible nil)
    (let ((start (point))
	  (end (progn (insert tag-button) (point))))
      (insert-char ?\n 1 nil)
      (put-text-property (1- (point)) (point) 'invisible nil)
      (speedbar-make-button start end tag-button-face
			    (if tag-button-function 'speedbar-highlight-face nil)
			    tag-button-function tag-button-data))
    ))
  
(defun speedbar-change-expand-button-char (char)
  "Change the expansion button character to CHAR for the current line."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward ":\\s-*.\\([-+?]\\)" (save-excursion (end-of-line)
								(point)) t)
	(speedbar-with-writable
	  (goto-char (match-beginning 1))
	  (delete-char 1)
	  (insert-char char 1 t)
	  (put-text-property (point) (1- (point)) 'invisible nil)
	  ;; make sure we fix the image on the text here.
	  (speedbar-insert-image-button-maybe (- (point) 2) 3)))))


;;; Build button lists
;;
(defun speedbar-insert-files-at-point (files level)
  "Insert list of FILES starting at point, and indenting all files to LEVEL.
Tag expandable items with a +, otherwise a ?.  Don't highlight ? as we
don't know how to manage them.  The input parameter FILES is a cons
cell of the form ( 'DIRLIST .  'FILELIST )"
  ;; Start inserting all the directories
  (let ((dirs (car files)))
    (while dirs
      (speedbar-make-tag-line 'angle ?+ 'speedbar-dired (car dirs)
			      (car dirs) 'speedbar-dir-follow nil
			      'speedbar-directory-face level)
      (setq dirs (cdr dirs))))
  (let ((lst (car (cdr files)))
	(case-fold-search t))
    (while lst
      (let* ((known (string-match speedbar-file-regexp (car lst)))
	     (expchar (if known ?+ ??))
	     (fn (if known 'speedbar-tag-file nil)))
	(if (or speedbar-show-unknown-files (/= expchar ??))
	    (speedbar-make-tag-line 'bracket expchar fn (car lst)
				    (car lst) 'speedbar-find-file nil
				    'speedbar-file-face level)))
      (setq lst (cdr lst)))))

(defun speedbar-default-directory-list (directory index)
  "Insert files for DIRECTORY with level INDEX at point."
  (speedbar-insert-files-at-point
   (speedbar-file-lists directory) index)
  (speedbar-reset-scanners)
  (if (= index 0)
      ;; If the shown files variable has extra directories, then
      ;; it is our responsibility to redraw them all
      ;; Luckilly, the nature of inserting items into this list means
      ;; that by reversing it, we can easilly go in the right order
      (let ((sf (cdr (reverse speedbar-shown-directories))))
	(setq speedbar-shown-directories
	      (list (expand-file-name default-directory)))
	;; exand them all as we find them
	(while sf
	  (if (speedbar-goto-this-file (car sf))
	      (progn
		(beginning-of-line)
		(if (looking-at "[0-9]+:[ ]*<")
		    (progn
		      (goto-char (match-end 0))
		      (speedbar-do-function-pointer)))))
	  (setq sf (cdr sf)))
	)))

(defun speedbar-sort-tag-hierarchy (lst)
  "Sort all elements of tag hierarchy LST."
  (sort (copy-alist lst)
	(lambda (a b) (string< (car a) (car b)))))

(defun speedbar-prefix-group-tag-hierarchy (lst)
  "Prefix group names for tag hierarchy LST."
  (let ((newlst nil)
	(sublst nil)
	(work-list nil)
	(junk-list nil)
	(short-group-list nil)
	(short-start-name nil)
	(short-end-name nil)
	(num-shorts-grouped 0)
	(bins (make-vector 256 nil))
	(diff-idx 0))
    ;; Break out sub-lists
    (while lst
      (if (and (listp (cdr-safe (car-safe lst)))
	       ;; This one is for bovine tokens
	       (not (symbolp (car-safe (cdr-safe (car-safe lst))))))
	  (setq newlst (cons (car lst) newlst))
	(setq sublst (cons (car lst) sublst)))
      (setq lst (cdr lst)))
    ;; Reverse newlst because it was made backwards.
    ;; Sublist doesn't need reversing because the act
    ;; of binning things will reverse it for us.
    (setq newlst (nreverse newlst))
    ;; Now, first find out how long our list is.  Never let a
    ;; list get-shorter than our minimum.
    (if (<= (length sublst) speedbar-tag-split-minimum-length)
	(setq work-list (nreverse sublst))
      (setq diff-idx (length (try-completion "" sublst)))
      ;; Sort the whole list into bins.
      (while sublst
	(let ((e (car sublst))
	      (s (car (car sublst))))
	  (cond ((<= (length s) diff-idx)
		 ;; 0 storage bin for shorty.
		 (aset bins 0 (cons e (aref bins 0))))
		(t
		 ;; stuff into a bin based on ascii value at diff
		 (aset bins (aref s diff-idx)
		       (cons e (aref bins (aref s diff-idx)))))))
	(setq sublst (cdr sublst)))
      ;; Go through all our bins  Stick singles into our
      ;; junk-list, everything else as sublsts in work-list.
      ;; If two neighboring lists are both small, make a grouped
      ;; group combinding those two sub-lists.
      (setq diff-idx 0)
      (while (> 256 diff-idx)
	(let ((l (nreverse;; Reverse the list since they are stuck in
		  ;; backwards.
		  (aref bins diff-idx))))
	  (if l
	      (let ((tmp (cons (try-completion "" l) l)))
		(if (or (> (length l) speedbar-tag-regroup-maximum-length)
			(> (+ (length l) (length short-group-list))
			   speedbar-tag-split-minimum-length))
		    (progn
		      ;; We have reached a longer list, so we
		      ;; must finish off a grouped group.
		      (cond
		       ((and short-group-list
			     (= (length short-group-list)
				num-shorts-grouped))
			;; All singles?  Junk list
			(setq junk-list (append short-group-list
						junk-list)))
		       ((= num-shorts-grouped 1)
			;; Only one short group?  Just stick it in
			;; there by itself.  Make a group, and find
			;; a subexpression
			(let ((subexpression (try-completion
					      "" short-group-list)))
			  (if (< (length subexpression)
				 speedbar-tag-group-name-minimum-length)
			      (setq subexpression
				    (concat short-start-name
					    " ("
					    (substring
					     (car (car short-group-list))
					     (length short-start-name))
					    ")")))
			  (setq work-list
				(cons (cons subexpression
					    short-group-list)
				      work-list))))
		       (short-group-list
			;; Multiple groups to be named in a special
			;; way by displaying the range over which we
			;; have grouped them.
			(setq work-list
			      (cons (cons (concat short-start-name
						  " to "
						  short-end-name)
					  (nreverse short-group-list))
				    work-list))))
		      ;; Reset short group list information every time.
		      (setq short-group-list nil
			    short-start-name nil
			    short-end-name nil
			    num-shorts-grouped 0)))
		;; Ok, now that we cleaned up the short-group-list,
		;; we can deal with this new list, to decide if it
		;; should go on one of these sub-lists or not.
		(if (< (length l) speedbar-tag-regroup-maximum-length)
		    (setq short-group-list (append short-group-list l)
			  num-shorts-grouped (1+ num-shorts-grouped)
			  short-end-name (car tmp)
			  short-start-name (if short-start-name
					       short-start-name
					     (car tmp)))
		  (setq work-list (cons tmp work-list))))))
	(setq diff-idx (1+ diff-idx))))
    ;; Did we run out of things?  Drop our new list onto the end.
    (cond
     ((and short-group-list (= (length short-group-list) num-shorts-grouped))
      ;; All singles?  Junk list
      (setq junk-list (append short-group-list junk-list)))
     ((= num-shorts-grouped 1)
      ;; Only one short group?  Just stick it in
      ;; there by itself.
      (setq work-list
	    (cons (cons (try-completion "" short-group-list)
			short-group-list)
		  work-list)))
     (short-group-list
      ;; Multiple groups to be named in a special
      ;; way by displaying the range over which we
      ;; have grouped them.
      (setq work-list
	    (cons (cons (concat short-start-name " to " short-end-name)
			short-group-list)
		  work-list))))
    ;; Reverse the work list nreversed when consing.
    (setq work-list (nreverse work-list))
    ;; Now, stick our new list onto the end of
    (if work-list
	(if junk-list
	    (append newlst work-list junk-list)
	  (append newlst work-list))
      (append  newlst junk-list))))

(defun speedbar-trim-words-tag-hierarchy (lst)
  "Trim all words in a tag hierarchy.
Base trimming information on word separators, and group names.
Argument LST is the list of tags to trim."
  (let ((newlst nil)
	(sublst nil)
	(trim-prefix nil)
	(trim-chars 0)
	(trimlst nil))
    (while lst
      (if (listp (cdr-safe (car-safe lst)))
	  (setq newlst (cons (car lst) newlst))
	(setq sublst (cons (car lst) sublst)))
      (setq lst (cdr lst)))
    ;; Get the prefix to trim by.  Make sure that we don't trim
    ;; off silly pieces, only complete understandable words.
    (setq trim-prefix (try-completion "" sublst))
    (if (or (= (length sublst) 1)
	    (not trim-prefix)
	    (not (string-match "\\(\\w+\\W+\\)+" trim-prefix)))
	(append (nreverse newlst) (nreverse sublst))
      (setq trim-prefix (substring trim-prefix (match-beginning 0)
				   (match-end 0)))
      (setq trim-chars (length trim-prefix))
      (while sublst
	(setq trimlst (cons
		       (cons (substring (car (car sublst)) trim-chars)
			     (cdr (car sublst)))
		       trimlst)
	      sublst (cdr sublst)))
      ;; Put the lists together
      (append (nreverse newlst) trimlst))))

(defun speedbar-simple-group-tag-hierarchy (lst)
  "Create a simple 'Tags' group with orphaned tags.
Argument LST is the list of tags to sort into groups."
  (let ((newlst nil)
	(sublst nil))
    (while lst
      (if (listp (cdr-safe (car-safe lst)))
	  (setq newlst (cons (car lst) newlst))
	(setq sublst (cons (car lst) sublst)))
      (setq lst (cdr lst)))
    (if (not newlst)
	(nreverse sublst)
      (setq newlst (cons (cons "Tags" (nreverse sublst)) newlst))
      (nreverse newlst))))

(defun speedbar-create-tag-hierarchy (lst)
  "Adjust the tag hierarchy in LST, and return it.
This uses `speedbar-tag-hierarchy-method' to determine how to adjust
the list."
  (let* ((f (save-excursion
	      (forward-line -1)
	      (speedbar-line-path)))
	 (methods (if (get-file-buffer f)
		      (save-excursion (set-buffer (get-file-buffer f))
				      speedbar-tag-hierarchy-method)
		    speedbar-tag-hierarchy-method))
	 (lst (if (fboundp 'copy-tree)
		  (copy-tree lst)
		lst)))
    (while methods
      (setq lst (funcall (car methods) lst)
	    methods (cdr methods)))
    lst))

(defun speedbar-insert-generic-list (level lst expand-fun find-fun)
  "At LEVEL, insert a generic multi-level alist LST.
Associations with lists get {+} tags (to expand into more nodes) and
those with positions just get a > as the indicator.  {+} buttons will
have the function EXPAND-FUN and the token is the CDR list.  The token
name will have the function FIND-FUN and not token."
  ;; Remove imenu rescan button
  (if (string= (car (car lst)) "*Rescan*")
      (setq lst (cdr lst)))
  ;; Adjust the list.
  (setq lst (speedbar-create-tag-hierarchy lst))
  ;; insert the parts
  (while lst
    (cond ((null (car-safe lst)) nil)	;this would be a separator
	  ((or (numberp (cdr-safe (car-safe lst)))
	       (markerp (cdr-safe (car-safe lst))))
	   (speedbar-make-tag-line nil nil nil nil ;no expand button data
				   (car (car lst)) ;button name
				   find-fun        ;function
				   (cdr (car lst)) ;token is position
				   'speedbar-tag-face
				   (1+ level)))
	  ((listp (cdr-safe (car-safe lst)))
	   (speedbar-make-tag-line 'curly ?+ expand-fun (cdr (car lst))
				   (car (car lst)) ;button name
				   nil nil 'speedbar-tag-face
				   (1+ level)))
	  (t (speedbar-message "Ooops!")))
    (setq lst (cdr lst))))

(defun speedbar-insert-imenu-list (indent lst)
  "At level INDENT, insert the imenu generated LST."
  (speedbar-insert-generic-list indent lst
				'speedbar-tag-expand
				'speedbar-tag-find))
				
(defun speedbar-insert-etags-list (indent lst)
  "At level INDENT, insert the etags generated LST."
  (speedbar-insert-generic-list indent lst
				'speedbar-tag-expand
				'speedbar-tag-find))

;;; Timed functions
;;
(defun speedbar-update-contents ()
  "Generically update the contents of the speedbar buffer."
  (interactive)
  ;; Set the current special buffer
  (setq speedbar-desired-buffer nil)
  ;; Check for special modes
  (speedbar-maybe-add-localized-support (current-buffer))
  ;; Choose the correct method of doodling.
  (if (and speedbar-mode-specific-contents-flag
	   (listp speedbar-special-mode-expansion-list)
	   speedbar-special-mode-expansion-list
	   (local-variable-p
	    'speedbar-special-mode-expansion-list
	    (current-buffer)))
      ;;(eq (get major-mode 'mode-class 'special)))
      (speedbar-update-special-contents)
    (speedbar-update-directory-contents)))

(defun speedbar-update-directory-contents ()
  "Update the contents of the speedbar buffer based on the current directory."
  (let ((cbd (expand-file-name default-directory))
	cbd-parent
	(funclst (speedbar-initial-expansion-list))
	(cache speedbar-full-text-cache)
	;; disable stealth during update
	(speedbar-stealthy-function-list nil)
	(use-cache nil)
	(expand-local nil)
	;; Because there is a bug I can't find just yet
	(inhibit-quit nil))
    (save-excursion
      (set-buffer speedbar-buffer)
      ;; If we are updating contents to where we are, then this is
      ;; really a request to update existing contents, so we must be
      ;; careful with our text cache!
      (if (member cbd speedbar-shown-directories)
	  (progn
	    (setq cache nil)
	    ;; If the current directory is not the last element in the dir
	    ;; list, then we ALSO need to zap the list of expanded directories
	    (if (/= (length (member cbd speedbar-shown-directories)) 1)
		(setq speedbar-shown-directories (list cbd))))

	;; Build cbd-parent, and see if THAT is in the current shown
	;; directories.  First, go through pains to get the parent directory
	(if (and speedbar-smart-directory-expand-flag
		 (save-match-data
		   (setq cbd-parent cbd)
		   (if (string-match "[/\\]$" cbd-parent)
		       (setq cbd-parent (substring cbd-parent 0
						   (match-beginning 0))))
		   (setq cbd-parent (file-name-directory cbd-parent)))
		 (member cbd-parent speedbar-shown-directories))
	    (setq expand-local t)

	  ;; If this directory is NOT in the current list of available
	  ;; paths, then use the cache, and set the cache to our new
	  ;; value.  Make sure to unhighlight the current file, or if we
	  ;; come back to this directory, it might be a different file
	  ;; and then we get a mess!
	  (if (> (point-max) 1)
	      (progn
		(speedbar-clear-current-file)
		(setq speedbar-full-text-cache
		      (cons speedbar-shown-directories (buffer-string)))))

	  ;; Check if our new directory is in the list of directories
	  ;; shown in the text-cache
	  (if (member cbd (car cache))
	      (setq speedbar-shown-directories (car cache)
		    use-cache t)
	    ;; default the shown directories to this list...
	    (setq speedbar-shown-directories (list cbd)))
	  ))
      (if (not expand-local) (setq speedbar-last-selected-file nil))
      (speedbar-with-writable
	(if (and expand-local
		 ;; Find this directory as a speedbar node.
		 (speedbar-path-line cbd))
	    ;; Open it.
	    (speedbar-expand-line)
	  (erase-buffer)
	  (cond (use-cache
		 (setq default-directory
		       (nth (1- (length speedbar-shown-directories))
			    speedbar-shown-directories))
		 (insert (cdr cache)))
		(t
		 (while funclst
		   (setq default-directory cbd)
		   (funcall (car funclst) cbd 0)
		   (setq funclst (cdr funclst))))))
	(goto-char (point-min)))))
  (speedbar-reconfigure-keymaps))

(defun speedbar-update-special-contents ()
  "Used the mode-specific variable to fill in the speedbar buffer.
This should only be used by modes classified as special."
  (let ((funclst speedbar-special-mode-expansion-list)
	(specialbuff (current-buffer)))
    (save-excursion
      (setq speedbar-desired-buffer specialbuff)
      (set-buffer speedbar-buffer)
      ;; If we are leaving a directory, cache it.
      (if (not speedbar-shown-directories)
	  ;; Do nothing
	  nil
	;; Clean up directory maintenance stuff
	(speedbar-clear-current-file)
	(setq speedbar-full-text-cache
	      (cons speedbar-shown-directories (buffer-string))
	      speedbar-shown-directories nil))
      ;; Now fill in the buffer with our newly found specialized list.
      (speedbar-with-writable
	(while funclst
	  ;; We do not erase the buffer because these functions may
	  ;; decide NOT to update themselves.
	  (funcall (car funclst) specialbuff)
	  (setq funclst (cdr funclst))))
      (goto-char (point-min))))
  (speedbar-reconfigure-keymaps))

(defun speedbar-timer-fn ()
  "Run whenever Emacs is idle to update the speedbar item."
  (if (not (and (frame-live-p speedbar-frame)
		(frame-live-p speedbar-attached-frame)))
      (speedbar-set-timer nil)
    ;; Save all the match data so that we don't mess up executing fns
    (save-match-data
      ;; Only do stuff if the frame is visible, not an icon, and if
      ;; it is currently flagged to do something.
      (if (and speedbar-update-flag
	       (frame-visible-p speedbar-frame)
	       (not (eq (frame-visible-p speedbar-frame) 'icon)))
	  (let ((af (selected-frame)))
	    (save-window-excursion
	      (select-frame speedbar-attached-frame)
	      ;; make sure we at least choose a window to
	      ;; get a good directory from
	      (if (window-minibuffer-p (selected-window))
		  nil
		;; Check for special modes
		(speedbar-maybe-add-localized-support (current-buffer))
		;; Update for special mode all the time!
		(if (and speedbar-mode-specific-contents-flag
			 (listp speedbar-special-mode-expansion-list)
			 speedbar-special-mode-expansion-list
			 (local-variable-p
			  'speedbar-special-mode-expansion-list
			  (current-buffer)))
		    ;;(eq (get major-mode 'mode-class 'special)))
		    (progn
		      (if (<= 2 speedbar-verbosity-level)
			  (speedbar-message
			   "Updating speedbar to special mode: %s..."
			   major-mode))
		      (speedbar-update-special-contents)
		      (if (<= 2 speedbar-verbosity-level)
			  (progn
			    (speedbar-message
			     "Updating speedbar to special mode: %s...done"
			     major-mode)
			    (speedbar-message nil))))
		  ;; Update all the contents if directories change!
		  (if (or (member (expand-file-name default-directory)
				  speedbar-shown-directories)
			  (and speedbar-ignored-path-regexp
			       (string-match
				speedbar-ignored-path-regexp
				(expand-file-name default-directory)))
			  (member major-mode speedbar-ignored-modes)
			  (eq af speedbar-frame)
			  (not (buffer-file-name)))
		      nil
		    (if (<= 1 speedbar-verbosity-level)
			(speedbar-message "Updating speedbar to: %s..."
				 default-directory))
		    (speedbar-update-directory-contents)
		    (if (<= 1 speedbar-verbosity-level)
			(progn
			  (speedbar-message "Updating speedbar to: %s...done"
				   default-directory)
			  (speedbar-message nil)))))
		(select-frame af)))
	    ;; Now run stealthy updates of time-consuming items
	    (speedbar-stealthy-updates)))
      ;; Now run the mouse tracking system
      (speedbar-show-info-under-mouse)))
  (run-hooks 'speedbar-timer-hook))


;;; Stealthy activities
;;
(defvar speedbar-stealthy-update-recurse nil
  "Recursion avoidance variable for stealthy update.")

(defun speedbar-stealthy-updates ()
  "For a given speedbar, run all items in the stealthy function list.
Each item returns t if it completes successfully, or nil if
interrupted by the user."
  (if (not speedbar-stealthy-update-recurse)
      (let ((l (speedbar-initial-stealthy-functions))
	    (speedbar-stealthy-update-recurse t))
	(unwind-protect
	    (speedbar-with-writable
	      (while (and l (funcall (car l)))
		;;(sit-for 0)
		(setq l (cdr l))))
	  ;;(speedbar-message "Exit with %S" (car l))
	  ))))

(defun speedbar-reset-scanners ()
  "Reset any variables used by functions in the stealthy list as state.
If new functions are added, their state needs to be updated here."
  (setq speedbar-vc-to-do-point t
	speedbar-obj-to-do-point t)
  (run-hooks 'speedbar-scanner-reset-hook)
  )

(defun speedbar-find-selected-file (file)
  "Goto the line where FILE is."
  (goto-char (point-min))
  (let ((m nil))
    (while (and (setq m (re-search-forward
			 (concat " \\(" (regexp-quote (file-name-nondirectory file))
				 "\\)\\(" speedbar-indicator-regex "\\)?\n")
			 nil t))
		(not (string= file
			      (concat
			       (speedbar-line-path
				(save-excursion
				  (goto-char (match-beginning 0))
				  (beginning-of-line)
				  (save-match-data
				    (looking-at "[0-9]+:")
				    (string-to-number (match-string 0)))))
			       (match-string 1))))))
    (if m
	(progn
	  (goto-char (match-beginning 1))
	  (match-string 1)))))

(defun speedbar-clear-current-file ()
  "Locate the file thought to be current, and remove its highlighting."
  (save-excursion
    (set-buffer speedbar-buffer)
    (if speedbar-last-selected-file
	(speedbar-with-writable
	  (if (speedbar-find-selected-file speedbar-last-selected-file)
	      (put-text-property (match-beginning 1)
				 (match-end 1)
				 'face
				 'speedbar-file-face))))))

(defun speedbar-update-current-file ()
  "Find the current file, and update our visuals to indicate its name.
This is specific to file names.  If the file name doesn't show up, but
it should be in the list, then the directory cache needs to be
updated."
  (let* ((lastf (selected-frame))
	 (newcfd (save-excursion
		   (select-frame speedbar-attached-frame)
		   (let ((rf (if (buffer-file-name)
				 (buffer-file-name)
			       nil)))
		     (select-frame lastf)
		     rf)))
	 (newcf (if newcfd newcfd))
	 (lastb (current-buffer))
	 (sucf-recursive (boundp 'sucf-recursive))
	 (case-fold-search t))
    (if (and newcf
	     ;; check here, that way we won't refresh to newcf until
	     ;; its been written, thus saving ourselves some time
	     (file-exists-p newcf)
	     (not (string= newcf speedbar-last-selected-file)))
	(progn
	  ;; It is important to select the frame, otherwise the window
	  ;; we want the cursor to move in will not be updated by the
	  ;; search-forward command.
	  (select-frame speedbar-frame)
	  ;; Remove the old file...
	  (speedbar-clear-current-file)
	  ;; now highlight the new one.
	  (set-buffer speedbar-buffer)
	  (speedbar-with-writable
	    (if (speedbar-find-selected-file newcf)
		;; put the property on it
		(put-text-property (match-beginning 1)
				   (match-end 1)
				   'face
				   'speedbar-selected-face)
	      ;; Oops, it's not in the list.  Should it be?
	      (if (and (string-match speedbar-file-regexp newcf)
		       (string= (file-name-directory newcfd)
				(expand-file-name default-directory)))
		  ;; yes, it is (we will ignore unknowns for now...)
		  (progn
		    (speedbar-refresh)
		    (if (speedbar-find-selected-file newcf)
			;; put the property on it
			(put-text-property (match-beginning 1)
					   (match-end 1)
					   'face
					   'speedbar-selected-face)))
		;; if it's not in there now, whatever...
		))
	    (setq speedbar-last-selected-file newcf))
	  (if (not sucf-recursive)
	      (progn
		(speedbar-center-buffer-smartly)
		(speedbar-position-cursor-on-line)
		))
	  (set-buffer lastb)
	  (select-frame lastf)
	  )))
  ;; return that we are done with this activity.
  t)

(defun speedbar-add-indicator (indicator-string &optional replace-this)
  "Add INDICATOR-STRING to the end of this speedbar line.
If INDICATOR-STRING is space, and REPLACE-THIS is a character, then
an the existing indicator is removed.  If there is already an
indicator, then do not add a space."
  (beginning-of-line)
  ;; The nature of the beast: Assume we are in "the right place"
  (end-of-line)
  (skip-chars-backward (concat " " speedbar-vc-indicator
			       (car speedbar-obj-indicator)
			       (cdr speedbar-obj-indicator)))
  (if (and (not (looking-at speedbar-indicator-regex))
	   (not (string= indicator-string " ")))
      (insert speedbar-indicator-separator))
  (speedbar-with-writable
    (save-excursion
      (if (and replace-this
	       (re-search-forward replace-this (save-excursion (end-of-line)
							       (point))
				  t))
	  (delete-region (match-beginning 0) (match-end 0))))
    (end-of-line)
    (if (not (string= " " indicator-string))
	(insert indicator-string))))

;; Load efs/ange-ftp only if compiling to remove byte-compiler warnings.
;; Steven L Baur <steve@xemacs.org> said this was important:
(eval-when-compile (or (featurep 'xemacs)
		       (condition-case () (require 'efs)
			 (error (require 'ange-ftp)))))

(defun speedbar-check-vc ()
  "Scan all files in a directory, and for each see if it's checked out.
See `speedbar-this-file-in-vc' and `speedbar-vc-check-dir-p' for how
to add more types of version control systems."
  ;; Check for to-do to be reset.  If reset but no RCS is available
  ;; then set to nil (do nothing) otherwise, start at the beginning
  (save-excursion
    (set-buffer speedbar-buffer)
    (if (and speedbar-vc-do-check (eq speedbar-vc-to-do-point t)
	     (speedbar-vc-check-dir-p default-directory)
	     (not (or (and (featurep 'ange-ftp)
			   (string-match
			    (car (if speedbar-xemacsp
				     ange-ftp-path-format
				   ange-ftp-name-format))
			    (expand-file-name default-directory)))
		      ;; efs support: Bob Weiner
		      (and (featurep 'efs)
			   (string-match
			    (car efs-path-regexp)
			    (expand-file-name default-directory))))))
	(setq speedbar-vc-to-do-point 0))
    (if (numberp speedbar-vc-to-do-point)
	(progn
	  (goto-char speedbar-vc-to-do-point)
	  (while (and (not (input-pending-p))
		      (re-search-forward "^\\([0-9]+\\):\\s-*\\[[+-]\\] "
					 nil t))
	    (setq speedbar-vc-to-do-point (point))
	    (if (speedbar-check-vc-this-line (match-string 1))
		(speedbar-add-indicator speedbar-vc-indicator
					(regexp-quote speedbar-vc-indicator))
	      (speedbar-add-indicator " "
				      (regexp-quote speedbar-vc-indicator))))
	  (if (input-pending-p)
	      ;; return that we are incomplete
	      nil
	    ;; we are done, set to-do to nil
	    (setq speedbar-vc-to-do-point nil)
	    ;; and return t
	    t))
      t)))

(defun speedbar-check-vc-this-line (depth)
  "Return t if the file on this line is check of of a version control system.
Parameter DEPTH is a string with the current depth of indentation of
the file being checked."
  (let* ((d (string-to-int depth))
	 (f (speedbar-line-path d))
	 (fn (buffer-substring-no-properties
	      ;; Skip-chars: thanks ptype@dra.hmg.gb
	      (point) (progn
			(skip-chars-forward "^ "
					    (save-excursion (end-of-line)
							    (point)))
			(point))))
	 (fulln (concat f fn)))
    (if (<= 2 speedbar-verbosity-level)
	(speedbar-message "Speedbar vc check...%s" fulln))
    (and (file-writable-p fulln)
	 (speedbar-this-file-in-vc f fn))))

(defun speedbar-vc-check-dir-p (path)
  "Return t if we should bother checking PATH for version control files.
This can be overloaded to add new types of version control systems."
  (or
   ;; Local RCS
   (file-exists-p (concat path "RCS/"))
   ;; Local SCCS
   (file-exists-p (concat path "SCCS/"))
   ;; Remote SCCS project
   (let ((proj-dir (getenv "PROJECTDIR")))
     (if proj-dir
	 (file-exists-p (concat proj-dir "/SCCS"))
       nil))
   ;; User extension
   (run-hook-with-args 'speedbar-vc-path-enable-hook path)
   ))

(defun speedbar-this-file-in-vc (path name)
  "Check to see if the file in PATH with NAME is in a version control system.
You can add new VC systems by overriding this function.  You can
optimize this function by overriding it and only doing those checks
that will occur on your system."
  (or
   ;; RCS file name
   (file-exists-p (concat path "RCS/" name ",v"))
   (file-exists-p (concat path "RCS/" name))
   ;; Local SCCS file name
   (file-exists-p (concat path "SCCS/s." name))
   ;; Remote SCCS file name
   (let ((proj-dir (getenv "PROJECTDIR")))
     (if proj-dir
         (file-exists-p (concat proj-dir "/SCCS/s." name))
       nil))
   ;; User extension
   (run-hook-with-args 'speedbar-vc-in-control-hook path name)
   ))

;; Objet File scanning
(defun speedbar-check-objects ()
  "Scan all files in a directory, and for each see if there is an object.
See `speedbar-check-obj-this-line' and `speedbar-obj-alist' for how
to add more object types."
  ;; Check for to-do to be reset.  If reset but no RCS is available
  ;; then set to nil (do nothing) otherwise, start at the beginning
  (save-excursion
    (set-buffer speedbar-buffer)
    (if (and speedbar-obj-do-check (eq speedbar-obj-to-do-point t))
	(setq speedbar-obj-to-do-point 0))
    (if (numberp speedbar-obj-to-do-point)
	(progn
	  (goto-char speedbar-obj-to-do-point)
	  (while (and (not (input-pending-p))
		      (re-search-forward "^\\([0-9]+\\):\\s-*\\[[+-]\\] "
					 nil t))
	    (setq speedbar-obj-to-do-point (point))
	    (let ((ind (speedbar-check-obj-this-line (match-string 1))))
	      (if (not ind) (setq ind " "))
	      (speedbar-add-indicator ind (concat
					   (car speedbar-obj-indicator)
					   "\\|"
					   (cdr speedbar-obj-indicator)))))
	  (if (input-pending-p)
	      ;; return that we are incomplete
	      nil
	    ;; we are done, set to-do to nil
	    (setq speedbar-obj-to-do-point nil)
	    ;; and return t
	    t))
      t)))

(defun speedbar-check-obj-this-line (depth)
  "Return t if the file on this line has an associated object.
Parameter DEPTH is a string with the current depth of indentation of
the file being checked."
  (let* ((d (string-to-int depth))
	 (f (speedbar-line-path d))
	 (fn (buffer-substring-no-properties
	      ;; Skip-chars: thanks ptype@dra.hmg.gb
	      (point) (progn
			(skip-chars-forward "^ "
					    (save-excursion (end-of-line)
							    (point)))
			(point))))
	 (fulln (concat f fn)))
    (if (<= 2 speedbar-verbosity-level)
	(speedbar-message "Speedbar obj check...%s" fulln))
    (let ((oa speedbar-obj-alist))
      (while (and oa (not (string-match (car (car oa)) fulln)))
	(setq oa (cdr oa)))
      (if (not (and oa (file-exists-p (concat (file-name-sans-extension fulln)
					      (cdr (car oa))))))
	  nil
	;; Find out if the object is out of date or not.
	(let ((date1 (nth 5 (file-attributes fulln)))
	      (date2 (nth 5 (file-attributes (concat
					      (file-name-sans-extension fulln)
                                              (cdr (car oa)))))))
	  (if (or (< (car date1) (car date2))
		  (and (= (car date1) (car date2))
		       (< (nth 1 date1) (nth 1 date2))))
	      (car speedbar-obj-indicator)
	    (cdr speedbar-obj-indicator)))))))

;;; Clicking Activity
;;
(defun speedbar-mouse-set-point (e)
  "Set POINT based on event E.
Handle clicking on images in XEmacs."
  (if (and (fboundp 'event-over-glyph-p) (event-over-glyph-p e))
      ;; We are in XEmacs, and clicked on a picture
      (let ((ext (event-glyph-extent e)))
	;; This position is back inside the extent where the
	;; junk we pushed into the property list lives.
	(if (extent-end-position ext)
	    (goto-char (1- (extent-end-position ext)))
	  (mouse-set-point e)))
    ;; We are not in XEmacs, OR we didn't click on a picture.
    (mouse-set-point e)))

(defun speedbar-quick-mouse (e)
  "Since mouse events are strange, this will keep the mouse nicely positioned.
This should be bound to mouse event E."
  (interactive "e")
  (speedbar-mouse-set-point e)
  (speedbar-position-cursor-on-line)
  )

(defun speedbar-position-cursor-on-line ()
  "Position the cursor on a line."
  (let ((oldpos (point)))
    (beginning-of-line)
    (if (looking-at "[0-9]+:\\s-*..?.? ")
	(goto-char (1- (match-end 0)))
      (goto-char oldpos))))

(defun speedbar-power-click (e)
  "Activate any speedbar button as a power click.
A power click will dispose of cached data (if available) or bring a buffer
up into a different window.
This should be bound to mouse event E."
  (interactive "e")
  (let ((speedbar-power-click t))
    (speedbar-click e)))

(defun speedbar-click (e)
  "Activate any speedbar buttons where the mouse is clicked.
This must be bound to a mouse event.  A button is any location of text
with a mouse face that has a text property called `speedbar-function'.
This should be bound to mouse event E."
  (interactive "e")
  (speedbar-mouse-set-point e)
  (speedbar-do-function-pointer)
  (speedbar-quick-mouse e))

(defun speedbar-double-click (e)
  "Activate any speedbar buttons where the mouse is clicked.
This must be bound to a mouse event.  A button is any location of text
with a mouse face that has a text property called `speedbar-function'.
This should be bound to mouse event E."
  (interactive "e")
  ;; Emacs only.  XEmacs handles this via `mouse-track-click-hook'.
  (cond ((eq (car e) 'down-mouse-1)
	 (speedbar-mouse-set-point e))
	((eq (car e) 'mouse-1)
	 (speedbar-quick-mouse e))
	((or (eq (car e) 'double-down-mouse-1)
	     (eq (car e) 'triple-down-mouse-1))
	 (speedbar-mouse-set-point e)
	 (speedbar-do-function-pointer)
	 (speedbar-quick-mouse e))))

(defun speedbar-do-function-pointer ()
  "Look under the cursor and examine the text properties.
From this extract the file/tag name, token, indentation level and call
a function if appropriate"
  (let* ((fn (get-text-property (point) 'speedbar-function))
	 (tok (get-text-property (point) 'speedbar-token))
	 ;; The 1-,+ is safe because scaning starts AFTER the point
	 ;; specified.  This lets the search include the character the
	 ;; cursor is on.
	 (tp (previous-single-property-change
	      (1+ (point)) 'speedbar-function))
	 (np (next-single-property-change
	      (point) 'speedbar-function))
	 (txt (buffer-substring-no-properties (or tp (point-min))
					      (or np (point-max))))
	 (dent (save-excursion (beginning-of-line)
			       (string-to-number
				(if (looking-at "[0-9]+")
				    (buffer-substring-no-properties
				    (match-beginning 0) (match-end 0))
				  "0")))))
    ;;(speedbar-message "%S:%S:%S:%s" fn tok txt dent)
    (and fn (funcall fn txt tok dent)))
  (speedbar-position-cursor-on-line))

;;; Reading info from the speedbar buffer
;;
(defun speedbar-line-text (&optional p)
  "Retrieve the text after prefix junk for the current line.
Optional argument P is where to start the search from."
  (save-excursion
    (if p (goto-char p))
    (beginning-of-line)
    (if (looking-at (concat
		     "\\([0-9]+\\): *[[<{][-+?][]>}] \\([^ \n]+\\)\\("
		     speedbar-indicator-regex "\\)?"))
	(match-string 2)
      nil)))

(defun speedbar-line-token (&optional p)
  "Retrieve the token information after the prefix junk for the current line.
Optional argument P is where to start the search from."
  (save-excursion
    (if p (goto-char p))
    (beginning-of-line)
    (if (looking-at (concat
		     "\\([0-9]+\\): *[[<{]?[-+?=][]>}@()|] \\([^ \n]+\\)\\("
		     speedbar-indicator-regex "\\)?"))
	(progn
	  (goto-char (match-beginning 2))
	  (get-text-property (point) 'speedbar-token))
      nil)))

(defun speedbar-line-file (&optional p)
  "Retrieve the file or whatever from the line at P point.
The return value is a string representing the file.  If it is a
directory, then it is the directory name."
  (save-match-data
    (let ((f (speedbar-line-text p)))
      (if f
	  (let* ((depth (string-to-int (match-string 1)))
		 (path (speedbar-line-path depth)))
	    (if (file-exists-p (concat path f))
		(concat path f)
	      nil))
	nil))))

(defun speedbar-goto-this-file (file)
  "If FILE is displayed, goto this line and return t.
Otherwise do not move and return nil."
  (let ((path (substring (file-name-directory (expand-file-name file))
			 (length (expand-file-name default-directory))))
	(dest (point)))
    (save-match-data
      (goto-char (point-min))
      ;; scan all the directories
      (while (and path (not (eq path t)))
	(if (string-match "^[/\\]?\\([^/\\]+\\)" path)
	    (let ((pp (match-string 1 path)))
	      (if (save-match-data
		    (re-search-forward (concat "> " (regexp-quote pp) "$")
				       nil t))
		  (setq path (substring path (match-end 1)))
		(setq path nil)))
	  (setq path t)))
      ;; find the file part
      (if (or (not path) (string= (file-name-nondirectory file) ""))
	  ;; only had a dir part
	  (if path
	      (progn
		(speedbar-position-cursor-on-line)
		t)
	    (goto-char dest) nil)
	;; find the file part
	(let ((nd (file-name-nondirectory file)))
	  (if (re-search-forward
	       (concat "] \\(" (regexp-quote nd)
		       "\\)\\(" speedbar-indicator-regex "\\)$")
	       nil t)
	      (progn
		(speedbar-position-cursor-on-line)
		t)
	    (goto-char dest)
	    nil))))))

(defun speedbar-line-path (&optional depth)
  "Retrieve the pathname associated with the current line.
This may require traversing backwards from DEPTH and combining the default
directory with these items.  This function is replaceable in
`speedbar-mode-functions-list' as `speedbar-line-path'"
  (let ((rf (speedbar-fetch-replacement-function 'speedbar-line-path)))
    (if rf (funcall rf depth) default-directory)))
      
(defun speedbar-files-line-path (&optional depth)
  "Retrieve the pathname associated with the current line.
This may require traversing backwards from DEPTH and combining the default
directory with these items."
  (save-excursion
    (save-match-data
      (if (not depth)
	  (progn
	    (beginning-of-line)
	    (looking-at "^\\([0-9]+\\):")
	    (setq depth (string-to-int (match-string 1)))))
      (let ((path nil))
	(setq depth (1- depth))
	(while (/= depth -1)
	  (if (not (re-search-backward (format "^%d:" depth) nil t))
	      (error "Error building path of tag")
	    (cond ((looking-at "[0-9]+:\\s-*<->\\s-+\\([^\n]+\\)$")
		   (setq path (concat (buffer-substring-no-properties
				       (match-beginning 1) (match-end 1))
				      "/"
				      path)))
		  ((looking-at "[0-9]+:\\s-*[-]\\s-+\\([^\n]+\\)$")
		   ;; This is the start of our path.
		   (setq path (buffer-substring-no-properties
			       (match-beginning 1) (match-end 1))))))
	  (setq depth (1- depth)))
	(if (and path
		 (string-match (concat speedbar-indicator-regex "$")
			       path))
	    (setq path (substring path 0 (match-beginning 0))))
	(concat default-directory path)))))

(defun speedbar-path-line (path)
  "Position the cursor on the line specified by PATH."
  (save-match-data
    (if (string-match "[/\\]$" path)
	(setq path (substring path 0 (match-beginning 0))))
    (let ((nomatch t) (depth 0)
	  (fname (file-name-nondirectory path))
	  (pname (file-name-directory path)))
      (if (not (member pname speedbar-shown-directories))
	  (error "Internal Error: File %s not shown in speedbar" path))
      (goto-char (point-min))
      (while (and nomatch
		  (re-search-forward
		   (concat "[]>] \\(" (regexp-quote fname)
			   "\\)\\(" speedbar-indicator-regex "\\)?$")
		   nil t))
	(beginning-of-line)
	(looking-at "\\([0-9]+\\):")
	(setq depth (string-to-int (match-string 0))
	      nomatch (not (string= pname (speedbar-line-path depth))))
	(end-of-line))
      (beginning-of-line)
      (not nomatch))))

(defun speedbar-edit-line ()
  "Edit whatever tag or file is on the current speedbar line."
  (interactive)
  (or (save-excursion
	(beginning-of-line)
	;; If this fails, then it is a non-standard click, and as such,
	;; perfectly allowed.
	(if (re-search-forward "[]>?}] [^ ]"
			       (save-excursion (end-of-line) (point))
			       t)
	    (speedbar-do-function-pointer)
	  nil))
      (speedbar-do-function-pointer)))

(defun speedbar-expand-line (&optional arg)
  "Expand the line under the cursor.
With universal argument ARG, flush cached data."
  (interactive "P")
  (beginning-of-line)
  (let ((speedbar-power-click arg))
    (condition-case nil
	(progn
	  (re-search-forward ":\\s-*.\\+. "
			     (save-excursion (end-of-line) (point)))
	  (forward-char -2)
	  (speedbar-do-function-pointer))
      (error (speedbar-position-cursor-on-line)))))
  
(defun speedbar-flush-expand-line ()
  "Expand the line under the cursor and flush any cached information."
  (interactive)
  (speedbar-expand-line 1))
  
(defun speedbar-contract-line ()
  "Contract the line under the cursor."
  (interactive)
  (beginning-of-line)
  (condition-case nil
      (progn
	(re-search-forward ":\\s-*.-. "
			   (save-excursion (end-of-line) (point)))
	(forward-char -2)
	(speedbar-do-function-pointer))
    (error (speedbar-position-cursor-on-line))))

(if speedbar-xemacsp
    (defalias 'speedbar-mouse-event-p 'button-press-event-p)
  (defun speedbar-mouse-event-p (event)
    "Return t if the event is a mouse related event"
    ;; And Emacs does it this way
    (if (and (listp event)
	     (member (event-basic-type event)
		     '(mouse-1 mouse-2 mouse-3)))
	t
      nil)))

(defun speedbar-maybee-jump-to-attached-frame ()
  "Jump to the attached frame ONLY if this was not a mouse event."
  (if (or (not (speedbar-mouse-event-p last-input-event))
	  speedbar-activity-change-focus-flag)
      (progn
	(select-frame speedbar-attached-frame)
	(other-frame 0))))

(defun speedbar-find-file (text token indent)
  "Speedbar click handler for filenames.
TEXT, the file will be displayed in the attached frame.
TOKEN is unused, but required by the click handler.  INDENT is the
current indentation level."
  (let ((cdd (speedbar-line-path indent)))
    (speedbar-find-file-in-frame (concat cdd text))
    (speedbar-stealthy-updates)
    (run-hooks 'speedbar-visiting-file-hook)
    ;; Reset the timer with a new timeout when cliking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer speedbar-update-speed))
  (speedbar-maybee-jump-to-attached-frame))

(defun speedbar-dir-follow (text token indent)
  "Speedbar click handler for directory names.
Clicking a directory will cause the speedbar to list files in the
the subdirectory TEXT.  TOKEN is an unused requirement.  The
subdirectory chosen will be at INDENT level."
  (setq default-directory
	(concat (expand-file-name (concat (speedbar-line-path indent) text))
		"/"))
  ;; Because we leave speedbar as the current buffer,
  ;; update contents will change directory without
  ;; having to touch the attached frame.  Turn off smart expand just
  ;; in case.
  (let ((speedbar-smart-directory-expand-flag nil))
    (speedbar-update-contents))
  (speedbar-set-timer speedbar-navigating-speed)
  (setq speedbar-last-selected-file nil)
  (speedbar-stealthy-updates))

(defun speedbar-delete-subblock (indent)
  "Delete text from point to indentation level INDENT or greater.
Handles end-of-sublist smartly."
  (speedbar-with-writable
    (save-excursion
      (end-of-line) (forward-char 1)
      (let ((start (point)))
	(while (and (looking-at "^\\([0-9]+\\):")
		    (> (string-to-int (match-string 1)) indent)
		    (not (eobp)))
	  (forward-line 1)
	  (beginning-of-line))
	(delete-region start (point))))))

(defun speedbar-dired (text token indent)
  "Speedbar click handler for directory expand button.
Clicking this button expands or contracts a directory.  TEXT is the
button clicked which has either a + or -.  TOKEN is the directory to be
expanded.  INDENT is the current indentation level."
  (cond ((string-match "+" text)	;we have to expand this dir
	 (setq speedbar-shown-directories
	       (cons (expand-file-name
		      (concat (speedbar-line-path indent) token "/"))
		     speedbar-shown-directories))
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-reset-scanners)
	 (save-excursion
	   (end-of-line) (forward-char 1)
	   (speedbar-with-writable
	     (speedbar-default-directory-list
	      (concat (speedbar-line-path indent) token "/")
	      (1+ indent)))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-reset-scanners)
	 (let ((oldl speedbar-shown-directories)
	       (newl nil)
	       (td (expand-file-name
		    (concat (speedbar-line-path indent) token))))
	   (while oldl
	     (if (not (string-match (concat "^" (regexp-quote td)) (car oldl)))
		 (setq newl (cons (car oldl) newl)))
	     (setq oldl (cdr oldl)))
	   (setq speedbar-shown-directories (nreverse newl)))
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent)
	 )
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly)
  (setq speedbar-last-selected-file nil)
  (save-excursion (speedbar-stealthy-updates)))

(defun speedbar-directory-buttons-follow (text token indent)
  "Speedbar click handler for default directory buttons.
TEXT is the button clicked on.  TOKEN is the directory to follow.
INDENT is the current indentation level and is unused."
  (if (string-match "^[A-z]:$" token)
      (setq default-directory (concat token (char-to-string directory-sep-char)))
    (setq default-directory token))
  ;; Because we leave speedbar as the current buffer,
  ;; update contents will change directory without
  ;; having to touch the attached frame.
  (speedbar-update-contents)
  (speedbar-set-timer speedbar-navigating-speed))

(defun speedbar-tag-file (text token indent)
  "The cursor is on a selected line.  Expand the tags in the specified file.
The parameter TEXT and TOKEN are required, where TEXT is the button
clicked, and TOKEN is the file to expand.  INDENT is the current
indentation level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (let* ((fn (expand-file-name (concat (speedbar-line-path indent)
					      token)))
		(mode nil)
		(lst (speedbar-fetch-dynamic-tags fn)))
	   ;; if no list, then remove expando button
	   (if (not lst)
	       (speedbar-change-expand-button-char ??)
	     (speedbar-change-expand-button-char ?-)
	     (speedbar-with-writable
	       (save-excursion
		 (end-of-line) (forward-char 1)
		 (funcall (car lst) indent (cdr lst)))))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun speedbar-tag-find (text token indent)
  "For the tag TEXT in a file TOKEN, goto that position.
INDENT is the current indentation level."
  (let ((file (speedbar-line-path indent)))
    (speedbar-find-file-in-frame file)
    (save-excursion (speedbar-stealthy-updates))
    ;; Reset the timer with a new timeout when cliking a file
    ;; in case the user was navigating directories, we can cancel
    ;; that other timer.
    (speedbar-set-timer speedbar-update-speed)
    (goto-char token)
    (run-hooks 'speedbar-visiting-tag-hook)
    ;;(recenter)
    (speedbar-maybee-jump-to-attached-frame)
    ))

(defun speedbar-tag-expand (text token indent)
  "Expand a tag sublist.  Imenu will return sub-lists of specialized tag types.
Etags does not support this feature.  TEXT will be the button
string.  TOKEN will be the list, and INDENT is the current indentation
level."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (speedbar-insert-generic-list indent token 'speedbar-tag-expand
					   'speedbar-tag-find))))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

;;; Loading files into the attached frame.
;;
(defun speedbar-find-file-in-frame (file)
  "This will load FILE into the speedbar attached frame.
If the file is being displayed in a different frame already, then raise that
frame instead."
  (let* ((buff (find-file-noselect file))
	 (bwin (get-buffer-window buff 0)))
    (if bwin
	(progn
	  (select-window bwin)
	  (raise-frame (window-frame bwin)))
      (if speedbar-power-click
	  (let ((pop-up-frames t)) (select-window (display-buffer buff)))
	(select-frame speedbar-attached-frame)
	(switch-to-buffer buff))))
 )

;;; Centering Utility
;;
(defun speedbar-center-buffer-smartly ()
  "Recenter a speedbar buffer so the current indentation level is all visible.
This assumes that the cursor is on a file, or tag of a file which the user is
interested in."
  (if (<= (count-lines (point-min) (point-max))
	  (1- (window-height (selected-window))))
      ;; whole buffer fits
      (let ((cp (point)))
	(goto-char (point-min))
	(recenter 0)
	(goto-char cp))
    ;; too big
    (let (depth start end exp p)
      (save-excursion
	(beginning-of-line)
	(setq depth (if (looking-at "[0-9]+")
			(string-to-int (buffer-substring-no-properties
					(match-beginning 0) (match-end 0)))
		      0))
	(setq exp (format "^%d:\\s-*[[{<]\\([?+-]\\)[]>}]" depth)))
      (save-excursion
	(end-of-line)
	(if (re-search-backward exp nil t)
	    (setq start (point))
	  (setq start (point-min)))
	(save-excursion			;Not sure about this part.
	  (end-of-line)
	  (setq p (point))
	  (while (and (not (re-search-forward exp nil t))
		      (>= depth 0))
	    (setq depth (1- depth))
	    (setq exp (format "^%d:\\s-*[[{<]\\([?+-]\\)[]>}]" depth)))
	  (if (/= (point) p)
	      (setq end (point))
	    (setq end (point-max)))))
      ;; Now work out the details of centering
      (let ((nl (count-lines start end))
	    (cp (point)))
	(if (> nl (window-height (selected-window)))
	    ;; We can't fit it all, so just center on cursor
	    (progn (goto-char start)
		   (recenter 1))
	  ;; we can fit everything on the screen, but...
	  (if (and (pos-visible-in-window-p start (selected-window))
		   (pos-visible-in-window-p end (selected-window)))
	      ;; we are all set!
	      nil
	    ;; we need to do something...
	    (goto-char start)
	    (let ((newcent (/ (- (window-height (selected-window)) nl) 2))
		  (lte (count-lines start (point-max))))
	      (if (and (< (+ newcent lte) (window-height (selected-window)))
		       (> (- (window-height (selected-window)) lte 1)
			  newcent))
		  (setq newcent (- (window-height (selected-window))
				   lte 1)))
	      (recenter newcent))))
	(goto-char cp)))))


;;; Tag Management -- List of expanders:
;;
(defun speedbar-fetch-dynamic-tags (file)
  "Return a list of tags generated dynamically from FILE.
This uses the entries in `speedbar-dynamic-tags-function-list'
to find the proper tags.  It is up to each of those individual
functions to do caching and flushing if appropriate."
  (save-excursion
    (set-buffer (find-file-noselect file))
    ;; If there is a buffer-local value of
    ;; speedbar-dynamic-tags-function-list, it will now be available.
    (let ((dtf speedbar-dynamic-tags-function-list)
	  (ret t))
      (while (and (eq ret t) dtf)
	(setq ret
	      (if (fboundp (car (car dtf)))
		  (funcall (car (car dtf)) (buffer-file-name))
		t))
	(if (eq ret t)
	    (setq dtf (cdr dtf))))
      (if (eq ret t)
	  ;; No valid tag list, return nil
	  nil
	;; We have some tags.  Return the list with the insert fn
	;; prepended
	(cons (cdr (car dtf)) ret)))))

;;; Tag Management -- Imenu
;;
(if (not speedbar-use-imenu-flag)

    nil

(eval-when-compile (if (locate-library "imenu") (require 'imenu)))

(defun speedbar-fetch-dynamic-imenu (file)
  "Load FILE into a buffer, and generate tags using Imenu.
Returns the tag list, or t for an error."
  ;; Load this AND compile it in
  (require 'imenu)
  (if speedbar-power-click (setq imenu--index-alist nil))
  (condition-case nil
      (let ((index-alist (imenu--make-index-alist t)))
	(if speedbar-sort-tags
	    (sort (copy-alist index-alist)
		  (lambda (a b) (string< (car a) (car b))))
	  index-alist))
    (error t)))
)

;;; Tag Management -- etags  (old XEmacs compatibility part)
;;
(defvar speedbar-fetch-etags-parse-list
  '(;; Note that java has the same parse-group as c
    ("\\.\\([cChH]\\|c\\+\\+\\|cpp\\|cc\\|hh\\|java\\)\\'" .
     speedbar-parse-c-or-c++tag)
    ("^\\.emacs$\\|.\\(el\\|l\\|lsp\\)\\'" .
     "def[^i]+\\s-+\\(\\(\\w\\|[-_]\\)+\\)\\s-*\C-?")
;    ("\\.\\([fF]\\|for\\|FOR\\|77\\|90\\)\\'" .
;      speedbar-parse-fortran77-tag)
    ("\\.tex\\'" . speedbar-parse-tex-string)
    ("\\.p\\'" .
     "\\(\\(FUNCTION\\|function\\|PROCEDURE\\|procedure\\)\\s-+\\([a-zA-Z0-9_.:]+\\)\\)\\s-*(?^?")
    )
  "Associations of file extensions and expressions for extracting tags.
To add a new file type, you would want to add a new association to the
list, where the car is the file match, and the cdr is the way to
extract an element from the tags output.  If the output is complex,
use a function symbol instead of regexp.  The function should expect
to be at the beginning of a line in the etags buffer.

This variable is ignored if `speedbar-use-imenu-flag' is non-nil.")

(defvar speedbar-fetch-etags-command "etags"
  "*Command used to create an etags file.

This variable is ignored if `speedbar-use-imenu-flag' is t")

(defvar speedbar-fetch-etags-arguments '("-D" "-I" "-o" "-")
  "*List of arguments to use with `speedbar-fetch-etags-command'.
This creates an etags output buffer.  Use `speedbar-toggle-etags' to
modify this list conveniently.

This variable is ignored if `speedbar-use-imenu-flag' is t")

(defun speedbar-toggle-etags (flag)
  "Toggle FLAG in `speedbar-fetch-etags-arguments'.
FLAG then becomes a member of etags command line arguments.  If flag
is \"sort\", then toggle the value of `speedbar-sort-tags'.  If its
value is \"show\" then toggle the value of
`speedbar-show-unknown-files'.

  This function is a convenience function for XEmacs menu created by
Farzin Guilak <farzin@protocol.com>"
  (interactive)
  (cond
   ((equal flag "sort")
    (setq speedbar-sort-tags (not speedbar-sort-tags)))
   ((equal flag "show")
    (setq speedbar-show-unknown-files (not speedbar-show-unknown-files)))
   ((or (equal flag "-C")
	(equal flag "-S")
	(equal flag "-D"))
    (if (member flag speedbar-fetch-etags-arguments)
	(setq speedbar-fetch-etags-arguments
	      (delete flag speedbar-fetch-etags-arguments))
      (add-to-list 'speedbar-fetch-etags-arguments flag)))
   (t nil)))

(defun speedbar-fetch-dynamic-etags (file)
  "For FILE, run etags and create a list of symbols extracted.
Each symbol will be associated with its line position in FILE."
  (let ((newlist nil))
    (unwind-protect
	(save-excursion
	  (if (get-buffer "*etags tmp*")
	      (kill-buffer "*etags tmp*"))	;kill to clean it up
	  (if (<= 1 speedbar-verbosity-level)
	      (speedbar-message "Fetching etags..."))
	  (set-buffer (get-buffer-create "*etags tmp*"))
	  (apply 'call-process speedbar-fetch-etags-command nil
		 (current-buffer) nil
		 (append speedbar-fetch-etags-arguments (list file)))
	  (goto-char (point-min))
	  (if (<= 1 speedbar-verbosity-level)
	      (speedbar-message "Fetching etags..."))
	  (let ((expr
		 (let ((exprlst speedbar-fetch-etags-parse-list)
		       (ans nil))
		   (while (and (not ans) exprlst)
		     (if (string-match (car (car exprlst)) file)
			 (setq ans (car exprlst)))
		     (setq exprlst (cdr exprlst)))
		   (cdr ans))))
	    (if expr
		(let (tnl)
		  (set-buffer (get-buffer-create "*etags tmp*"))
		  (while (not (save-excursion (end-of-line) (eobp)))
		    (save-excursion
		      (setq tnl (speedbar-extract-one-symbol expr)))
		    (if tnl (setq newlist (cons tnl newlist)))
		    (forward-line 1)))
	      (speedbar-message
	       "Sorry, no support for a file of that extension"))))
      )
    (if speedbar-sort-tags
	(sort newlist (lambda (a b) (string< (car a) (car b))))
      (reverse newlist))))

;; This bit donated by Farzin Guilak <farzin@protocol.com> but I'm not
;; sure it's needed with the different sorting method.
;;
;(defun speedbar-clean-etags()
;  "Removes spaces before the ^? character, and removes `#define',
;return types, etc. preceding tags.  This ensures that the sort operation
;works on the tags, not the return types."
;  (save-excursion
;    (goto-char (point-min))
;    (while
;	(re-search-forward "(?[ \t](?\C-?" nil t)
;      (replace-match "\C-?" nil nil))
;    (goto-char (point-min))
;    (while
;	(re-search-forward "\\(.*[ \t]+\\)\\([^ \t\n]+.*\C-?\\)" nil t)
;      (delete-region (match-beginning 1) (match-end 1)))))

(defun speedbar-extract-one-symbol (expr)
  "At point, return nil, or one alist in the form: (SYMBOL .  POSITION)
The line should contain output from etags.  Parse the output using the
regular expression EXPR"
  (let* ((sym (if (stringp expr)
		  (if (save-excursion
			(re-search-forward expr (save-excursion
						  (end-of-line)
						  (point)) t))
		      (buffer-substring-no-properties (match-beginning 1)
						      (match-end 1)))
		(funcall expr)))
	 (pos (let ((j (re-search-forward "[\C-?\C-a]\\([0-9]+\\),\\([0-9]+\\)"
					  (save-excursion
					    (end-of-line)
					    (point))
					  t)))
		(if (and j sym)
		    (1+ (string-to-int (buffer-substring-no-properties
					(match-beginning 2)
					(match-end 2))))
		  0))))
    (if (/= pos 0)
	(cons sym pos)
      nil)))

(defun speedbar-parse-c-or-c++tag ()
  "Parse a c or c++ tag, which tends to be a little complex."
  (save-excursion
    (let ((bound (save-excursion (end-of-line) (point))))
      (cond ((re-search-forward "\C-?\\([^\C-a]+\\)\C-a" bound t)
	     (buffer-substring-no-properties (match-beginning 1)
					     (match-end 1)))
	    ((re-search-forward "\\<\\([^ \t]+\\)\\s-+new(" bound t)
	     (buffer-substring-no-properties (match-beginning 1)
					     (match-end 1)))
	    ((re-search-forward "\\<\\([^ \t(]+\\)\\s-*(\C-?" bound t)
	     (buffer-substring-no-properties (match-beginning 1)
					     (match-end 1)))
	    (t nil))
      )))

(defun speedbar-parse-tex-string ()
  "Parse a Tex string.  Only find data which is relevant."
  (save-excursion
    (let ((bound (save-excursion (end-of-line) (point))))
      (cond ((re-search-forward "\\(\\(sub\\)*section\\|chapter\\|cite\\)\\s-*{[^\C-?}]*}?" bound t)
	     (buffer-substring-no-properties (match-beginning 0)
					     (match-end 0)))
	    (t nil)))))


;;; BUFFER DISPLAY mode.
;;
(defvar speedbar-buffers-key-map nil
  "Keymap used when in the buffers display mode.")

(if speedbar-buffers-key-map
    nil
  (setq speedbar-buffers-key-map (speedbar-make-specialized-keymap))

  ;; Basic tree features
  (define-key speedbar-buffers-key-map "e" 'speedbar-edit-line)
  (define-key speedbar-buffers-key-map "\C-m" 'speedbar-edit-line)
  (define-key speedbar-buffers-key-map "+" 'speedbar-expand-line)
  (define-key speedbar-buffers-key-map "=" 'speedbar-expand-line)
  (define-key speedbar-buffers-key-map "-" 'speedbar-contract-line)

  ;; Buffer specific keybindings
  (define-key speedbar-buffers-key-map "k" 'speedbar-buffer-kill-buffer)
  (define-key speedbar-buffers-key-map "r" 'speedbar-buffer-revert-buffer)

  )

(defvar speedbar-buffer-easymenu-definition
  '(["Jump to buffer" speedbar-edit-line t]
    ["Expand File Tags" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Flush Cache & Expand" speedbar-flush-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract File Tags" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    ["Kill Buffer" speedbar-buffer-kill-buffer
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    ["Revert Buffer" speedbar-buffer-revert-buffer
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    )
  "Menu item elements shown when displaying a buffer list.")

(defun speedbar-buffer-buttons (directory zero)
  "Create speedbar buttons based on the buffers currently loaded.
DIRECTORY is the path to the currently active buffer, and ZERO is 0."
  (speedbar-buffer-buttons-engine nil))

(defun speedbar-buffer-buttons-temp (directory zero)
  "Create speedbar buttons based on the buffers currently loaded.
DIRECTORY is the path to the currently active buffer, and ZERO is 0."
  (speedbar-buffer-buttons-engine t))

(defun speedbar-buffer-buttons-engine (temp)
  "Create speedbar buffer buttons.
If TEMP is non-nil, then clicking on a buffer restores the previous display."
  (insert "Active Buffers:\n")
  (let ((bl (buffer-list)))
    (while bl
      (if (string-match "^[ *]" (buffer-name (car bl)))
	  nil
	(let* ((known (string-match speedbar-file-regexp
				    (buffer-name (car bl))))
	       (expchar (if known ?+ ??))
	       (fn (if known 'speedbar-tag-file nil))
	       (fname (save-excursion (set-buffer (car bl))
				      (buffer-file-name))))
	  (speedbar-make-tag-line 'bracket expchar fn
				  (if fname (file-name-nondirectory fname))
				  (buffer-name (car bl))
				  'speedbar-buffer-click temp
				  'speedbar-file-face 0)))
      (setq bl (cdr bl)))
    (setq bl (buffer-list))
    (insert "Scratch Buffers:\n")
    (while bl
      (if (not (string-match "^\\*" (buffer-name (car bl))))
	  nil
	(if (eq (car bl) speedbar-buffer)
	    nil
	  (speedbar-make-tag-line 'bracket ?? nil nil
				  (buffer-name (car bl))
				  'speedbar-buffer-click temp
				  'speedbar-file-face 0)))
      (setq bl (cdr bl)))
    (setq bl (buffer-list))
    (insert "Hidden Buffers:\n")
    (while bl
      (if (not (string-match "^ " (buffer-name (car bl))))
	  nil
	(if (eq (car bl) speedbar-buffer)
	    nil
	  (speedbar-make-tag-line 'bracket ?? nil nil
				  (buffer-name (car bl))
				  'speedbar-buffer-click temp
				  'speedbar-file-face 0)))
      (setq bl (cdr bl)))))

(defun speedbar-buffers-item-info ()
  "Display information about the current buffer on the current line."
  (or (speedbar-item-info-tag-helper)
      (let* ((item (speedbar-line-text))
	     (buffer (if item (get-buffer item) nil)))
	(and buffer
	     (speedbar-message "%s%s %S %d %s"
			       (if (buffer-modified-p buffer) "* " "")
			       item
			       (save-excursion (set-buffer buffer) major-mode)
			       (save-excursion (set-buffer buffer)
					       (buffer-size))
			       (or (buffer-file-name buffer) "<No file>"))))))

(defun speedbar-buffers-line-path (&optional depth)
  "Fetch the full path to the file (buffer) specified on the current line.
Optional argument DEPTH specifies the current depth of the back search."
  (save-excursion
    (end-of-line)
    (let ((start (point)))
      ;; Buffers are always at level 0
      (if (not (re-search-backward "^0:" nil t))
	  nil
	(let* ((bn (speedbar-line-text))
	       (buffer (if bn (get-buffer bn))))
	  (if buffer
	      (if (save-excursion
		    (end-of-line)
		    (eq start (point)))
		  (file-name-directory (buffer-file-name buffer))
		(buffer-file-name buffer))))))))

(defun speedbar-buffer-click (text token indent)
  "When the users clicks on a buffer-button in speedbar.
TEXT is the buffer's name, TOKEN and INDENT are unused."
  (if speedbar-power-click
      (let ((pop-up-frames t)) (select-window (display-buffer text)))
    (select-frame speedbar-attached-frame)
    (switch-to-buffer text)
    (if token (speedbar-change-initial-expansion-list
	       speedbar-previously-used-expansion-list-name))))

(defun speedbar-buffer-kill-buffer ()
  "Kill the buffer the cursor is on in the speedbar buffer."
  (interactive)
  (or (save-excursion
	(beginning-of-line)
	;; If this fails, then it is a non-standard click, and as such,
	;; perfectly allowed.
	(if (re-search-forward "[]>?}] [^ ]"
			       (save-excursion (end-of-line) (point))
			       t)
	    (let ((text (progn
			  (forward-char -1)
			  (buffer-substring (point) (save-excursion
						      (end-of-line)
						      (point))))))
	      (if (and (get-buffer text)
		       (speedbar-y-or-n-p (format "Kill buffer %s? " text)))
		  (kill-buffer text))
	      (speedbar-refresh))))))

(defun speedbar-buffer-revert-buffer ()
  "Revert the buffer the cursor is on in the speedbar buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    ;; If this fails, then it is a non-standard click, and as such,
    ;; perfectly allowed
    (if (re-search-forward "[]>?}] [^ ]"
			   (save-excursion (end-of-line) (point))
			   t)
	(let ((text (progn
		      (forward-char -1)
		      (buffer-substring (point) (save-excursion
						  (end-of-line)
						  (point))))))
	  (if (get-buffer text)
	      (progn
		(set-buffer text)
		(revert-buffer t)))))))


;;; Useful hook values and such.
;;
(defvar speedbar-highlight-one-tag-line nil
  "Overlay used for highlighting the most recently jumped to tag line.")

(defun speedbar-highlight-one-tag-line ()
  "Highlight the current line, unhighlighting a previously jumped to line."
  (speedbar-unhighlight-one-tag-line)
  (setq speedbar-highlight-one-tag-line
	(speedbar-make-overlay (save-excursion (beginning-of-line) (point))
			       (save-excursion (end-of-line)
					       (forward-char 1)
					       (point))))
  (speedbar-overlay-put speedbar-highlight-one-tag-line 'face
			'speedbar-highlight-face)
  (add-hook 'pre-command-hook 'speedbar-unhighlight-one-tag-line)
  )

(defun speedbar-unhighlight-one-tag-line ()
  "Unhighlight the currently highlight line."
  (if speedbar-highlight-one-tag-line
      (progn
	(speedbar-delete-overlay speedbar-highlight-one-tag-line)
	(setq speedbar-highlight-one-tag-line nil)))
  (remove-hook 'pre-command-hook 'speedbar-unhighlight-one-tag-line))

(defun speedbar-recenter-to-top ()
  "Recenter the current buffer so POINT is on the top of the window."
  (recenter 1))

(defun speedbar-recenter ()
  "Recenter the current buffer so POINT is in the center of the window."
  (recenter (/ (window-height (selected-window)) 2)))


;;; Color loading section.
;;
(defface speedbar-button-face '((((class color) (background light))
				 (:foreground "green4"))
				(((class color) (background dark))
				 (:foreground "green3")))
  "Face used for +/- buttons."
  :group 'speedbar-faces)

(defface speedbar-file-face '((((class color) (background light))
			       (:foreground "cyan4"))
			      (((class color) (background dark))
			       (:foreground "cyan"))
			      (t (:weight bold)))
  "Face used for file names."
  :group 'speedbar-faces)

(defface speedbar-directory-face '((((class color) (background light))
				    (:foreground "blue4"))
				   (((class color) (background dark))
				    (:foreground "light blue")))
  "Faced used for directory names."
  :group 'speedbar-faces)
(defface speedbar-tag-face '((((class color) (background light))
			      (:foreground "brown"))
			     (((class color) (background dark))
			      (:foreground "yellow")))
  "Face used for displaying tags."
  :group 'speedbar-faces)

(defface speedbar-selected-face '((((class color) (background light))
				    (:foreground "red" :underline t))
				  (((class color) (background dark))
				   (:foreground "red" :underline t))
				  (t (:underline t)))
  "Face used to underline the file in the active window."
  :group 'speedbar-faces)

(defface speedbar-highlight-face '((((class color) (background light))
				    (:background "green"))
				   (((class color) (background dark))
				    (:background "sea green"))
				   (((class grayscale monochrome)
				     (background light))
				    (:background "black"))
				   (((class grayscale monochrome)
				     (background dark))
				    (:background "white")))
  "Face used for highlighting buttons with the mouse."
  :group 'speedbar-faces)


;;; Image loading and inlining
;;

;;; Some images if defimage is available:
(eval-when-compile

(if (fboundp 'defimage)
    (defalias 'defimage-speedbar 'defimage)

  (if (not (fboundp 'make-glyph))
      
(defmacro defimage-speedbar (variable imagespec docstring)
  "Don't bother loading up an image...
Argument VARIABLE is the varible to define.
Argument IMAGESPEC is the list defining the image to create.
Argument DOCSTRING is the documentation for VARIABLE."
  `(defvar ,variable nil ,docstring))

;; ELSE
(defun speedbar-find-image-on-load-path (image)
  "Find the image file IMAGE on the load path."
  (let ((l load-path)
	(r nil))
    (while (and l (not r))
      (if (file-exists-p (concat (car l) "/" image))
	  (setq r (concat (car l) "/" image)))
      (setq l (cdr l)))
    r))

(defun speedbar-convert-emacs21-imagespec-to-xemacs (spec)
  "Convert the Emacs21 Image SPEC into an XEmacs image spec."
  (let* ((sl (car spec))
	 (itype (nth 1 sl))
	 (ifile (nth 3 sl)))
    (vector itype ':file (speedbar-find-image-on-load-path ifile))))

(defmacro defimage-speedbar (variable imagespec docstring)
  "Devine VARIABLE as an image if `defimage' is not available..
IMAGESPEC is the image data, and DOCSTRING is documentation for the image."
  `(defvar ,variable
     ;; The Emacs21 version of defimage looks just like the XEmacs image
     ;; specifier, except that it needs a :type keyword.  If we line
     ;; stuff up right, we can use this cheat to support XEmacs specifiers.
     (condition-case nil
	 (make-glyph
	  (make-image-specifier
	   (speedbar-convert-emacs21-imagespec-to-xemacs (quote ,imagespec)))
	  'buffer)
       (error nil))
     ,docstring))

)))

(defimage-speedbar speedbar-directory-plus
  ((:type xpm :file "sb-dir-plus.xpm" :ascent center))
  "Image used for closed directories with stuff in them.")

(defimage-speedbar speedbar-directory-minus
  ((:type xpm :file "sb-dir-minus.xpm" :ascent center))
  "Image used for open directories with stuff in them.")

(defimage-speedbar speedbar-page-plus
  ((:type xpm :file "sb-pg-plus.xpm" :ascent center))
  "Image used for closed files with stuff in them.")

(defimage-speedbar speedbar-page-minus
  ((:type xpm :file "sb-pg-minus.xpm" :ascent center))
  "Image used for open files with stuff in them.")

(defimage-speedbar speedbar-page
  ((:type xpm :file "sb-pg.xpm" :ascent center))
  "Image used for files that can't be opened.")

(defimage-speedbar speedbar-tag
  ((:type xpm :file "sb-tag.xpm" :ascent center))
  "Image used for tags.")

(defimage-speedbar speedbar-tag-plus
  ((:type xpm :file "sb-tag-plus.xpm" :ascent center))
  "Image used for closed tag groups.")

(defimage-speedbar speedbar-tag-minus
  ((:type xpm :file "sb-tag-minus.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-tag-gt
  ((:type xpm :file "sb-tag-gt.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-tag-v
  ((:type xpm :file "sb-tag-v.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-tag-type
  ((:type xpm :file "sb-tag-type.xpm" :ascent center))
  "Image used for open tag groups.")

(defimage-speedbar speedbar-mail
  ((:type xpm :file "sb-mail.xpm" :ascent center))
  "Image used for open tag groups.")

(defvar speedbar-expand-image-button-alist
  '(("<+>" . speedbar-directory-plus)
    ("<->" . speedbar-directory-minus)
    ("[+]" . speedbar-page-plus)
    ("[-]" . speedbar-page-minus)
    ("[?]" . speedbar-page)
    ("{+}" . speedbar-tag-plus)
    ("{-}" . speedbar-tag-minus)
    ("<M>" . speedbar-mail)
    (" =>" . speedbar-tag)
    (" +>" . speedbar-tag-gt)
    (" ->" . speedbar-tag-v)
    (">" . speedbar-tag)
    ("@" . speedbar-tag-type)
    ("  @" . speedbar-tag-type)
    )
  "List of text and image associations.")

(defun speedbar-insert-image-button-maybe (start length)
  "Insert an image button based on text starting at START for LENGTH chars.
If buttontext is unknown, just insert that text.
If we have an image associated with it, use that image."
  (if speedbar-use-images
      (let* ((bt (buffer-substring start (+ length start)))
	     (a (assoc bt speedbar-expand-image-button-alist)))
	;; Regular images (created with `insert-image' are intangible
	;; which (I suppose) make them more compatible with XEmacs 21.
	;; Unfortunatly, there is a giant pile o code dependent on the
	;; underlying text.  This means if we leave it tangible, then I
	;; don't have to change said giant piles o code.
	(if (and a (symbol-value (cdr a)))
	    (if (featurep 'xemacs)
		(add-text-properties (+ start (length bt)) start
				     (list 'end-glyph (symbol-value (cdr a))
					   'rear-nonsticky (list 'display)
					   'invisible t
					   'detachable t))
	      (add-text-properties start (+ start (length bt))
				   (list 'display (symbol-value (cdr a))
					 'rear-nonsticky (list 'display))))
	  ;(message "Bad text [%s]" (buffer-substring start (+ start length)))
	  ))))


;; some edebug hooks
(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec speedbar-with-writable def-body)))

(provide 'speedbar)

;; run load-time hooks
(run-hooks 'speedbar-load-hook)

;;; speedbar.el ends here
