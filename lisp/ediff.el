;;; ediff.el --- a comprehensive visual interface to diff & patch
;;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>
;; Created: February 2, 1994
;; Keywords: comparing, merging, patching, version control.

(defconst ediff-version "2.19" "The current version of Ediff")
(defconst ediff-date "March 14, 1995" "Date of last update")  

;; LCD Archive Entry:
;; ediff|Michael Kifer|kifer@cs.sunysb.edu|
;; A comprehensive visual interface to diff and patch|
;; 14-March-95|2.19|~/packages/ediff.shar.Z|


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

;;; Commentary:
;;  ----------

;; Never read that diff output again!
;; Apply patch selectively, like a pro!
;; Merge with ease!

;; This package provides a convenient way of simultaneous browsing through
;; the differences between a pair (or a tripple) of files or buffers.  The
;; files being compared, file-A, file-B, and file-C (if applicable) are
;; shown in separate windows (side by side, one above the another, or in
;; separate frames), and the differences are highlighted as you step
;; through them.  You can also copy difference regions from one buffer to
;; another (and recover old differences if you change your mind).

;; In addition, Ediff can apply a patch to a file and then let you step
;; though both files, the patched and the original one, simultaneously,
;; difference-by-difference.  You can even apply a patch right out of a
;; mail buffer, i.e., patches received by mail don't even have to be saved.
;; Since Ediff lets you copy differences between buffers, you can, in
;; effect, apply patches selectively (i.e., you can copy a difference
;; region from file_orig to file, thereby undoing any particular patch that
;; you don't like).

;; This package builds upon the ideas borrowed from emerge.el and
;; several Ediff's functions are adaptations from emerge.el. 
;; Much of the functionality of Ediff is also influenced by emerge.el.

;; The present version of Ediff supersedes Emerge. It provides a superior
;; user interface and has many features not found in Emerge. In particular,
;; it can do patching and 2-way and 3-way file comparison in addition to
;; merging.

;; Ediff is aware of version control, which lets the user compare
;; files with their older versions. Ediff can also work with remote and
;; compressed files. Details are given below.


;;; Remarks: 
;;  -------

;;  1. Ediff is heavily dependent on the new features of Emacs 19.
;;     It won't run under Emacs 18 at all.
;;  2. If running XEmacs, Ediff requires at least version 19.9.
;;  3. The function ediff-revision requires the version of vc.el that comes
;;     with Emacs 19.22 and XEmacs 19.10 and later, or rcs.el version 1.67 
;;     or later. See "Version control support", below.


;;; Installation and use:
;;  ---------------------

;; The user can invoke Ediff interactively using the following functions:
;;
;;  	    ediff-files   	    	    	 - compare two files
;;  	    ediff   	    	    	    	 - alias for ediff-files
;;  	    ediff-buffers   	    	    	 - compare two buffers
;;
;;  	    ediff-files3   	    	    	 - compare three files
;;  	    ediff3   	    	    	    	 - alias for ediff-files3
;;  	    ediff-buffers3   	    	    	 - compare three buffers
;;
;;	    ediff-windows			 - compare windows
;;	    ediff-small-regions			 - compare small regions
;;	    ediff-large-regions			 - compare large regions
;;
;;  	    ediff-revision 	    	    	 - compare buffer & version
;;
;;  	    ediff-patch-file	    	    	 - patch file then compare
;;  	    epatch  	    	    	    	 - alias for ediff-patch-file
;;  	    ediff-patch-buffer	    	    	 - patch buffer then compare
;;  	    epatch-buffer   	    	    	 - alias for ediff-patch-buffer
;;
;;          ediff-merge-files			 - merge two files
;;          ediff-merge				 - alias for ediff-merge-files
;;	    ediff-merge-files-with-ancestor	 - same but with ancestor
;;	    ediff-merge-with-ancestor		 - alias for the above
;;	    ediff-merge-buffers			 - merge two buffers
;;	    ediff-merge-buffers-with-ancestor	 - same but with ancestor
;;	    ediff-merge-revisions	    	 - same but with ancestor
;;	    ediff-merge-revisions-with-ancestor	 - same but with ancestor
;;
;;
;;
;; To use Ediff, put this in your .emacs file:
;;
;;  (autoload 'ediff-buffers "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff  "ediff"  "Visual interface to diff" t)
;;  (autoload 'ediff-files "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff-buffers3 "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff3  "ediff3"  "Visual interface to diff" t)
;;  (autoload 'ediff-files3 "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff-merge "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff-merge-files "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff-merge-files-with-ancestor "ediff"
;;                                             "Visual interface to diff" t)
;;  (autoload 'ediff-merge-with-ancestor "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff-merge-buffers "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff-merge-buffers-with-ancestor "ediff"
;;                                             "Visual interface to diff" t)
;;  (autoload 'ediff-merge-revisions "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff-merge-revisions-with-ancestor "ediff"
;;                                             "Visual interface to diff" t)
;;  (autoload 'ediff-windows "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff-small-regions "ediff" "Visual interface to diff" t)
;;  (autoload 'ediff-large-regions "ediff" "Visual interface to diff" t)
;;  (autoload 'epatch  "ediff"  "Visual interface to patch" t)
;;  (autoload 'ediff-patch-file "ediff" "Visual interface to patch" t)
;;  (autoload 'ediff-patch-buffer "ediff" "Visual interface to patch" t)
;;  (autoload 'epatch-buffer "ediff" "Visual interface to patch" t)
;;  (autoload 'ediff-revision "ediff"
;;  	    	    	"Interface to diff & version control" t) 
;;
;;
;; If you want Ediff to be loaded from the very beginning, you should have
;;
;;  (require 'ediff)
;;
;; in your .emacs file.  This way it is also easier to figure out changes
;; to the default Ediff setting, if such changes become necessary --- see
;; Customization.
;;
;; All the above functions use the diff program to find different
;; regions. They process diff output and display it to the user in a
;; convenient form.
;;
;; The functions ediff-files, ediff-buffers, ediff-files3, ediff-buffers3 
;; first display the coarse, line-based difference regions, as commonly
;; found by the diff program.
;; Since diff may report fairly large chunks of text as being
;; different even though the difference may be contained in a few words or
;; even in the white space or line breaks, Ediff will further refine the
;; regions to indicate which exact words differ. If the only difference is
;; in the white space and line breaks, Ediff will say so.
;;
;; The functions ediff-windows, ediff-small-regions and ediff-large-regions
;; do comparison on parts of buffers (which must already exist).
;; Since ediff-windows  and ediff-small-regions are intended for relatively
;; small segments of the buffers, comparison is done on the
;; word-basis rather than line basis. No refinement is necessary in this
;; case. This technique is effective only for relatively small
;; regions (perhaps, up to 100 lines), as these functions have a relatively
;; slow startup.
;; To compare large regions, use ediff-large-regions. In this mode, Ediff
;; displays differences as it would if invoked via ediff-files or
;; ediff-buffers.
;;
;; The functions ediff-patch-file and ediff-patch-buffer apply a patch
;; to a file or a buffer and then run Ediff on these buffers, displaying
;; the difference regions.
;;
;; Finally, for files under version control, ediff-revisions will compare a
;; file to one of its versions.

;;; Compilation
;;  -----------
;;
;; When you byte-compile Ediff, you will get some warnings about functions
;; being undefined.  These can be safely ignored.
;;

;;; Customization:
;;  --------------

;; Hooks:
;; -----
;; If you don't like the default setting, you can change it through the
;; various variables and hooks.  In particular, the following hooks are
;; available: 

;;	    ediff-load-hooks
;;          ediff-keymap-setup-hooks
;;  	    ediff-before-setup-windows-hooks
;;  	    ediff-after-setup-windows-hooks
;;	    ediff-before-setup-control-frame-hooks
;;	    ediff-after-setup-control-frame-hooks
;;  	    ediff-startup-hooks
;;  	    ediff-select-hooks
;;  	    ediff-unselect-hooks
;;  	    ediff-suspend-hooks
;;  	    ediff-quit-hooks
;;  	    ediff-prepare-buffer-hooks
;;          ediff-display-help-hooks

;; The hooks in ediff-load-hooks can be used to change defaults after Ediff
;; is loaded. The hooks in ediff-keymap-setup-hooks can be used to alter
;; bindings in Ediff's keymap. These hooks are called right after the
;; default bindings are set.
;;
;; The hooks in ediff-before/after-setup-windows-hooks,
;; ediff-suspend-hooks, and ediff-quit-hooks can be used to save and then
;; restore whatever window configuration you want.

;; Note that, by default, ediff-quit-hooks is set to a function,
;; ediff-cleanup-mess, which cleans after Ediff, as appropriate in most
;; cases. It is rather unlikely that the user will want to change
;; it. However, the user may want add other hooks to ediff-quit-hooks,
;; either before or after ediff-cleanup-mess (see the documentation for
;; add-hook on how to do this).  One should be aware that hooks executing
;; before ediff-cleanup-mess start in ediff-control-buffer; they should
;; also leave ediff-control-buffer as the current buffer. Hooks that are
;; executed after ediff-cleanup-mess will have either buffer A or buffer B
;; as the current buffer.

;; If you are using packages such as mode-line.el to alter
;; the buffer identification field in the mode line, you may have to
;; restore this field after exiting Ediff by calling an appropriate
;; function from a hook in ediff-quit-hooks (in case of mode-line.el, the
;; function to call would be mode-line-abbreviate-buffer-identification).
;; This should be done from a hook that runs before ediff-default-quit-hook,
;; since such hooks run from within ediff-control-buffer, where buffers
;; A, B, and C can be accessed via the variables ediff-buffer-A,
;; ediff-buffer-B, ediff-buffer-C.

;; The hooks ediff-before/after-setup-control-frame-hooks can be used to
;; change how and where Ediff Control Panel is displayed, when it is
;; displayed in a separate frame. 

;; However, be aware that many variables that drive Ediff are local to
;; Ediff Control Panel, which requires special care in writing these hooks.
;; Take a look at ediff-default-suspend-hook and ediff-default-quit-hook to
;; see what's involved.

;; The hooks in ediff-prepare-buffer-hooks are executed for each Ediff
;; buffer (A, B, C) right after these buffers are arranged.  Ediff runs the
;; hooks in ediff-display-help-hooks each time after setting up the help
;; message.  Finally, ediff-mode-hooks are run just after ediff-mode is set
;; up in the control buffer. This is done before any windows or frames are
;; created. One can use it to set local variables that determine the
;; look of the display.

;; Quick help:
;; ----------
;;
;;  Ediff provides quick help using its control panel window. Since this
;;  window takes a fair share of the screen real estate, you can toggle it
;;  off by hitting `?'. The control window will then shrink to just one
;;  line and a mode line, displaying a short help message. The variable 
;;
;;	ediff-prefer-long-help-message
;;
;;  Tells Ediff whether the user wants the short message initially or the
;;  long one. By default, it is set to nil, meaning that the short message
;;  will be shown on startup. Set this to t, if you want the long message
;;  initially. 
;;  If you want to change the appearance of the help message on a
;;  per-buffer basis, you must use ediff-startup-hooks to change the value
;;  of ediff-help-message, which is a variable local to ediff-control-buffer.

;; Window and frame configuration:
;; -------------------------------

;; In a non-windowing display, Ediff sets things up in one frame, splitting
;; it between a small control window and the windows for file-A, file-B,
;; and file-C. The split between these latter windows can be horizontal or
;; vertical, which can be changed interactively by hitting `|' while the
;; cursor is in the control window.
;;
;; On a window display, Ediff sets up a dedicated frame for Ediff Control
;; Panel and then it would choose windows as follows: If one of the buffers
;; is invisible, it will be displayed in the currently selected frame.  If
;; a buffer is visible, it will be displayed in the frame it is visible.
;; If, according to the above criteria, the two buffers fall into the same
;; frame, then be it---the frame will be shared by the two.  The same
;; algorithm works when you hit `C-l' (ediff-recenter), `p'
;; (ediff-previous-difference), `n', etc.
;;
;; Thus, you can compare files in one frame or in different frames.
;; The former is done by default, while the latter can be achieved by
;; arranging files A, B (and C, if applicable) to be seen in different
;; frames.  Ediff respects these arrangements, automatically adapting
;; itself to the multi-frame mode.

;; Ediff uses the variables
;;
;;    ediff-control-frame-parameters
;;    ediff-control-frame-position-function
;;
;; to set up its control panels. The user can change or augment
;; ediff-control-frame-parameters including the font, color, etc. The X
;; resource name of Ediff Control Panel frames is `Ediff'. Under X-windows,
;; you can use this name to set up preferences in your ~/.Xdefaults
;; (~/.xrdb, or whatever is in use). Usually this is preferable to changing
;; ediff-control-frame-parameters directly.  For instance, you can specify
;; in ~/.Xdefaults where the control frame is to be sitting on the screen
;; using the resource 
;;
;;    Ediff*geometry
;;
;; In general, any X resource pertaining the control frame can be reached
;; via the prefix `Ediff*'.
;;
;; The prefered way of specifying the position of the control frame is by
;; setting the variable ediff-control-frame-position-function to be a
;; function to be called in order to determine the desired location for the
;; control frame. The default value of this variable is
;; `ediff-make-frame-position'. This function places the control frame in
;; the vicinity of the North-East corner of the frame displaying buffer A.
;; A pair of variables,
;;
;;    ediff-narrow-control-frame-leftward-shift
;;    ediff-wide-control-frame-rightward-shift
;;    ediff-control-frame-upward-shift
;;
;; can be used to adjust the location produced by ediff-make-frame-position.
;; The first variable specifies the number of characters for shifting
;; the control frame from the rightmost edge of frame A when the control
;; frame is displayed as a small window. 
;; The second variable specifies the rightward shift of the control frame
;; from the left edge of frame A when the control frame shows the full
;; menu of options.
;; The third variable specifies the number of pixels for the upward shift
;; of the control frame.
;;
;; If you truly and absolutely dislike the way Ediff sets up windows and if
;; you can't customize this via frame parameters, the last resort is to
;; rewrite the function `ediff-setup-windows'.  However, we believe that
;; detaching Ediff Control Panel from the rest and making it into a
;; separate frame offers an important opportunity by allowing you to
;; iconify that frame. Under Emacs, the icon will usually accept all of the
;; Ediff commands, but will free up valuable real estate on your screen
;; (this may depend on the window manager, though). Iconifying won't do any
;; good under XEmacs since XEmacs icons do not seem to be sensitive to
;; keyboard input.  The saving grace is that, even if not iconified, the
;; control frame is very small, smaller than some icons, so it doesn't take
;; much space in any case.
;;
;; The variable 
;;
;;      ediff-prefer-iconified-control-frame
;;
;; if t, will cause the control frame to become iconified automatically when
;; the help message is toggled off. This saves valuable real estate on the
;; screen. Toggling help back will deiconify the control frame.
;;
;; To start ediff with an iconified Control Panel, you should set the above
;; to t and ediff-prefer-long-help-message to nil.

;; The variable
;;
;;	ediff-window-setup-function
;;
;; Controls the way windows are setup. The above multiframe setup is
;; achieved via ediff-setup-windows-multiframe function, which is a default
;; on windowing displays (except for XEmacs 19.10 and earlier, which has a
;; bug that breaks the multiframe display). The plain setup, one where all
;; windows are always in one frame, is done via ediff-setup-windows-plain,
;; which is the default on a non-windowing display (or in an xterm window).
;; In fact, under Emacs, you can switch freely between these two setups by
;; executing the command `ediff-toggle-multiframe'. However, don't try to
;; do it under XEmacs, as it gets thoroughly confused if you switch from
;; multiframe setup to plain setup within the same Ediff session. 

;; If you don't like either of these setups, write your own function. See
;; the documentation for ediff-window-setup-function for the basic
;; guidelines. However, writing window setups is not easy, so, before
;; embarking on this job, you may want to take a close look at
;; ediff-setup-windows-plain and ediff-setup-windows-multiframe.

;;  The user can run multiple Ediff sessions at once, by invoking it several
;;  times without exiting the previous Ediff sessions. Different sessions
;;  may even operate on the same pair of files.  So, in principle, it is
;;  possible to do, say, pairwise comparison of three (or more) different
;;  files.  Each session would have its own Ediff Control Panel and all the
;;  regarding a particular session is local to the associated control panel
;;  buffer.  You can switch between sessions by suspending one session and
;;  then switching to another control panel. (Different control panel
;;  buffers are distinguished by a numerical suffix, e.g., Ediff Control
;;  Panel<3>.)  Thus, if you would like to compare three files pairwise,
;;  you can do this by preparing three different frames, each with its data
;;  buffer to be compared.  (No, I am not saying that such a 3way
;;  comparison is very easy to do.)
;;
;; If you need to conduct multiple Ediff sessions on the same file, one
;; thing should be kept in mind: each time you invoke Ediff on a buffer that
;; already participates in another Ediff session, that buffer should not
;; have any ASCII Ediff flags in it. (Highlighting with faces is OK.)  If
;; flags are not removed, difference overlays won't be set correctly
;; for the second invocation of Ediff.  The simplest way to remove ASCII
;; flags from an Ediff buffer is to hit `h' and thus switch to highlighting
;; with faces (unhighlighting on a dumb terminal).

;;  Remote and Compressed Files
;;   ---------------------------

;;  Ediff will work with remote, compressed, and encrypted files. Ediff
;;  supports ange-ftp.el, jka-compr.el, uncompress.el and crypt++.el, but
;;  it may work with other similar packages as well. This
;;  means that you can compare files residing on another machine, or you
;;  can apply a patch to a file on another machine (even the patch itself
;;  can be a remote file!).
;;
;;  When patching compressed or remote files, Ediff doesn't rename the
;;  source file into source-file-name_orig (unlike what `patch' would
;;  usually do). Instead, the source file retains its name and the result
;;  of applying the patch is placed in a temporary file that has the suffix
;;  `_patched'.  Generally, this applies to files that are handled using
;;  black magic, such as special file handlers (ange-ftp and some
;;  compression and encryption packages all use this method).
;;
;;  Regular files are treated by `patch' in the usual manner, i.e., the
;;  original is renamed into source-name_orig and the result of the patch
;;  is placed into the file source-name. (Ediff uses `_orig' instead of
;;  the usual `.orig' for compatibility with systems like VMS.)

;;
;; Selective browsing: Control over stepping through difference regions
;; -------------------------------------------------------------------- 
;;
;; Sometimes it is convenient to be able to step through only some
;; difference regions, those that satisfy certain conditions and to ignore
;; all others. The commands `#f' and `#h' let the user specify regular
;; expressions to control the way Ediff skips to the next or previous
;; difference. Typing `#f' lets one specify of regular expressions,
;; regexp-A, regexp-B, and regexp-C.
;; Ediff will then start stepping only through those difference regions where
;; the region in buffer A matches regexp-A and/or the region in buffer B
;; matches regexp-B, etc. Whether `and' or `or' should be used depends on
;; how the user responds to a prompt.
;; Similarly, using `#h', one specifies expressions that match difference
;; regions to be ignored while stepping through the differences. That is, if
;; the buffer A part matches regexp-A, the buffer B part matches regexp B
;; and (if applicable) buffer-C part matches regexp-C, then the region will
;; be ignored by ediff-next-difference and ediff-previous-difference commands.
;;
;;  Hitting `#f' and `#h' toggles this feature on/off.
;;
;; Note that selective browsing affects only ediff-next-difference and
;; ediff-previous-difference, i.e., the commands invoked by typing n/SPC
;; and p/DEL. You can still jump directly (using `j' or `ga/gb/gc') to any
;; numbered  difference. Also, it should be understood, that #f and #h do
;; not change the position of the point in the buffers. The effect of these
;; commands is seen only when the user types `n' or `p', i.e., when
;; Ediff is told to jump to the next or previous difference.
;;
;; Users can supply their own functions that specify how Ediff should do
;; selective browsing. To change the default Ediff function, add a function to
;; ediff-load-hooks which will do the following assignments:
;;
;;  	(fset ediff-hide-regexp-matches 'your-hide-function) 
;;  	(fset ediff-focus-on-regexp-matches 'your-focus-function)
;;
;; Useful hints: To specify a regexp that matches everything, don't simply
;; type RET in response to a prompt. Typing RET tells Ediff to accept the
;; default value, which may not be what you want. Instead, one should enter
;; something like `^' or `$' --- which would match every line. 
;;
;; If the user doesn't remember if selective browsing is in effect and
;; which regexps are being used, the status command, `i', will supply
;; the requisite information.
;;
;; In addition to the ability to ignore regions that match regular
;; expressions, Ediff can be ordered to start skipping over certain
;; `inessential' regions. This is controlled by the variable
;;
;;      ediff-ignore-similar-regions
;;
;; which, if set to t, will cause Ediff to skip over difference regions
;; that has been found similar, i.e., where the only differences are those
;; in the white space and newlines.
;;
;; Note: In order for this feature to work, auto-refining of difference
;; regions must be on, since otherwise Ediff won't know if there are no
;; fine differences between regions. Under X, auto-refining is a default,
;; but it is nixed on a dumb terminal or in an Xterm window. Therefore, in
;; a non-windowing environment, the user must explicitly turn
;; auto-refining on (e.g., by typing `@').
;;
;; CAUTION: If many inessential regions appear in a row, Ediff may take a
;; long time to jump to the next region because it has to compute fine
;; differences of all intermediate regions.
;;
;;
;; Highlighting difference regions
;; -------------------------------
;; The second group of Ediff variables that could be changed, if you so
;; wish, is: 
;;
;;  	    ediff-before-flag-bol
;;  	    ediff-after-flag-eol
;;  	    ediff-before-flag-mol
;;  	    ediff-after-flag-mol
;;
;;  	    ediff-current-diff-face-A
;;  	    ediff-current-diff-face-B
;;  	    ediff-current-diff-face-C
;;  	    ediff-fine-diff-face-A
;;  	    ediff-fine-diff-face-B
;;  	    ediff-fine-diff-face-C
;;  	    ediff-even-diff-face-A
;;  	    ediff-even-diff-face-B
;;  	    ediff-even-diff-face-C
;;  	    ediff-odd-diff-face-A
;;  	    ediff-odd-diff-face-B
;;  	    ediff-odd-diff-face-C
;
;; The first four are ASCII strings that mark the beginning and the end of
;; the differences found in files A, B, and C. Ediff uses different flags
;; to highlight regions that begin/end at the beginning of a line or in a
;; middle of a line.

;; The rest are the faces used to highlight text on X displays.  On X
;; displays, Ediff uses ediff-current-diff-face-A/B/C to highlight the
;; current difference region. 
;;
;; The faces ediff-fine-diff-face-A/B/C
;; are used to show the fine differences between the current differences
;; regions in buffers A, B, and C, respectively.
;;
;; Non-current difference regions are displayed in alternating
;; faces: ediff-even/odd-diff-face-A/B/C.   The odd and the even
;; faces are actually identical on monochrome displays, because it is
;; rather poor in what you can do on such a display. So, I chose to use
;; italics to highlight other differences. Any ideas would be welcome.
;; There are two ways to change the default setting for highlighting faces:
;; either change the variables, as in
;;
;; (setq ediff-current-diff-face-A 'bold-italic)
;;
;; or
;;
;; (setq ediff-current-diff-face-A
;;  	 (copy-face 'bold-italic 'ediff-current-diff-face-A))
;;
;; or by selectively modifying the defaults:
;;
;; (add-hook 'ediff-load-hooks
;;   (function (lambda () 
;;                (set-face-foreground ediff-current-diff-face-B "blue")
;;                (set-face-background ediff-current-diff-face-B "red")
;;                (make-face-italic ediff-current-diff-face-B))))
;;
;; You may also want to take a look at how the above faces are defined in
;; Ediff. 
;;
;; Note: it is not recommended to use `internal-get-face' (or `get-face' in
;;  	 XEmacs) when defining faces for Ediff, since this may cause
;;  	 problems when there are several frames with different font sizes.
;;       Instead, use copy-face or set/make-face-* as shown above.
;;
;; The last variable in this group,
;;
;;  	    ediff-highlight-all-diffs
;;
;; indicates whether---on a window system---the user wants differences to be
;; marked using ASCII strings (like on a dumb terminal) or using colors and
;; highlighting. Normally, Ediff highlights all differences, but the selected
;; difference is highlighted more visibly. One can cycle through various
;; modes of highlighting by hitting `h'. By default, Ediff starts in the
;; mode where all difference regions are highlighted. If you prefer to
;; start in the mode where unselected differences are not highlighted, you
;; should set ediff-highlight-all-diffs to nil. 
;; You will still be able to turn on highlighting of all differences by
;; hitting `h'.
;;
;; If you want to change the above variables, they must be set
;; BEFORE Ediff is loaded. 
;;
;; Note: Ediff lets you switch between the two types of highlighting.  That
;; is you can switch, interactively, from highlighting using faces to
;; highlighting using ASCII flags, and back.  Of course, toggling has
;; effect only on a window system.  On a dumb terminal or in an xterm
;; window, the only available option is highlighting with ASCII flags.
;;
;; Selective display
;; -----------------
;; If buffers being compared are narrowed at the time of invocation of Ediff,
;; ediff-buffers will preserve the narrowing range. However, if ediff-files
;; is invoked on the files visited by these buffers, narrowing will be
;; turned off, since we assume that the user wants to compare the entire files.
;;
;; Invocation of ediff-small/large-regions and ediff-windows will cause
;; Ediff to set new narrowing ranges. However, the old ranges are preserved
;; and will be returned to after quitting or by hitting `%'. 
;;
;; Two variables control the behavior of ediff-windows,
;; ediff-small-regions, and ediff-large-regions with respect to narrowing:
;;
;;	ediff-start-narrowed
;;	ediff-quit-widened
;;
;; If ediff-start-narrowed is t, then Ediff will narrow display to the
;; appropriate range if it is invoked as ediff-windows or
;; ediff-small/large-regions.
;; If it is nil, then narrowing will not take place. However, the user can
;; still toggle narrowing on and off by typing `%'.
;; Similarly, ediff-quit-widened controls whether Ediff should restore
;; the visibility range that existed before the current invocation.
;;
;;
;; Refinement of difference regions
;; --------------------------------
;; Ediff has variables that control the way fine differences are
;; highlighted. This feature lets the user highlight the exact words that 
;; make the difference regions in comparison buffers different. This process
;; ignores spaces, tabs, and newlines.
;;
;;  	    ediff-auto-refine
;;  	    ediff-auto-refine-limit
;;
;; By default, `ediff-auto-refine' is `on', which means that fine differences
;; within regions will be highlighted automatically. On a slow system, this
;; feature may be undesirable. In any case, the user can always toggle
;; auto-refining on/off/nix by hitting `@'. When auto-refining is off, fine
;; differences will be shown only for regions for which these differences
;; have been computed and saved before. If auto-refining is nixed, fine
;; differences will not be shown at all. Hitting `*' will compute and
;; display fine differences for the current difference region regardless of
;; whether auto-refining is on, off, or nixed. 
;; If auto-refining is on, the variable `ediff-auto-refine-limit' limits
;; the size of the regions to be auto-refined. This variable guards against
;; possible slow-down that may be caused by an extraordinary large
;; difference region.
;;
;; However, the user can always force region refining by typing `*'.
;;
;; Sometimes, when a difference region has too many differences between the
;; variants, highlighting of fine differences stands in the way, especially
;; on color displays. If that is the case, the user can invoke `*' with a
;; negative prefix argument, which would unhighlight fine diffs for the
;; current region.
;;
;; To unhighlight fine differences in all diff regions, use the command
;; `@'. Repeated typing of this key cycles through three different states:
;; auto-refining, no-auto-refining, and unhighlighting of all fine
;; differences.
;;
;; The variable
;;
;;  	    ediff-forward-word-function
;;
;; allows the user to control how fine differences are computed.
;; The value must be a lisp function that determines how the
;; current difference region should be split into words. 
;;
;; Fine diferences are computed by first splitting the current difference
;; region into words and then passing this along to
;; `ediff-diff-program'. For the default ediff-forward-word-function,
;; `ediff-forward-word', a word is a string consisting of letters, `-', or
;; `_', a string of punctuation symbols, a string of digits, or a string
;; consisting of symbols that are neither space, nor a letter.
;;
;; Patch and diff programs
;; -----------------------
;; The next group of variables determines the programs to be used for
;; applying patches and for computing the main difference regions (not the
;; fine difference regions):
;;
;;  	    ediff-patch-program
;;  	    ediff-patch-options
;;  	    ediff-diff-program
;;  	    ediff-diff-options
;;  	    ediff-diff3-program
;;  	    ediff-diff3-options
;;
;; Warning about VMS: The output from VMS DIFF is not yet supported.
;; Instead, make sure some implementation of Unix diff on VMS is used.
;;
;; These specify the functions that produce differences and do patching.
;; The *-options variables specify which options to pass to these programs.
;; It is unlikely that you would want to change these.
;; However, sometimes you may want to tell diff to ignore spaces and
;; such. Use the option '-w' for that.
;; Diff has several other useful options (type 'man diff' to find out). 
;;
;; However, Ediff doesn't let you use the option '-c', as it doesn't
;; recognize this format yet. However, if you need to save the output from
;; diff in a special form, Ediff lets you specify ``custom'' diff format
;; using the following two variables:
;;
;;  	    ediff-custom-diff-program
;;  	    ediff-custom-diff-options
;;
;; The output generated by ediff-custom-diff-program (which doesn't even
;; have to be a Unix-style diff!) is not used by Ediff, except that you can
;; save if using ediff-save-buffer function (normally bound to `wd' key
;; sequence).
;; However, Ediff is not the preferred way of producing diff output in
;; Emacs, unless you also intend to use Ediff to browse through the diff'ed
;; files.  This is because diff.el (M-x diff), which also comes with Emacs,
;; is much faster in yielding the output of diff, while Ediff consumes many
;; resources.

;; Support for diff3 and merging
;; -----------------------------

;; Ediff supports 3way comparison via the functions `ediff-files3' and
;; `ediff-buffers3'. The interface is the same as for 2-way comparison.
;; In 3-way comparison and merging, Ediff indicates if any two difference
;; regions are identical. For instance, if the current region in buffer A
;; is the same as the region in buffer C, then the mode line of buffer A will
;; display [=diff(C)] and the mode line of buffer C will display [=diff(A)]. 
;;
;; Merging is done according to the following algorithm.
;;
;; If a diff region in one of the buffers, say B, differs from the ancestor
;; while the region in the other buffer, A, doesn't, then the merge buffer,
;; C, gets the B's region. Similarly when buffer A's region differs from
;; the ancestor and B's doesn't.
;;
;; If both regions, A and B, differ from the ancestor, then Ediff chooses
;; according to the value of
;;
;;    ediff-default-variant
;;
;; If the value is `default-A' then A's region is chosen. If it is
;; `default-B' then B's region is chosen. If the value of the above
;; variable is `combined' then the region in buffer C will look like this:
;;
;;    #ifdef NEW  /* variant A */
;;    diff region from buffer A
;;    #else  /* variant B */
;;    diff region from buffer B
;;    #endif  /* NEW */
;;
;; The actual strings that separate the regions copied from bufer A and B
;; are controled by the variable
;;
;;    ediff-combination-pattern
;;
;; which must be a list of three strings.
;;
;; In addition to the state of the difference, during merging Ediff
;; displays the state of the merge for each region. If a difference came
;; from buffer A by default (because both regions A and B were different
;; from the ancestor and ediff-default-variant was set to `default-A')
;; then [=diff(A) default-A] is displayed in the mode line.  If the
;; difference in buffer C came, say, from buffer B because the diff region
;; in that buffer differs from the ancestor, but the region in buffer A
;; doesn't (if merging with an ancestor) then [=diff(B) prefer-B] is
;; displayed. The indicators default-A/B and prefer-A/B are inspired by
;; emerge.el and have the same meaning. 
;;
;; Another indicator of the state of merge is `combined'. It appears
;; with any difference region in buffer C that was obtained by combining
;; the difference regions in buffers A and B as explained above.
;;
;; Note that the state-of-difference indicators `=diff(A)' and `=diff(B)'
;; above are not redundant, even in the present of a state-of-merge
;; indicator, as the two serve different purposes. For instance, if the
;; mode line displays [=diff(B) prefer(B)] and you copy a diff region from
;; buffer A to buffer C then `=diff(B)' will change to `diff-A' and the
;; mode line will display [=diff(A) prefer-B]. 
;; This indicates that the difference region in buffer C is identical to
;; that in buffer A, but originally buffer C's region came from buffer B.
;; This is useful to know because the original diff region in buffer C can
;; be recovered by typing `r'.
;;
;; Ediff never changes the state-of-merge indicator, except as a result of
;; the `!' command (see below), in which case the indicator is lost.
;; On the other hand, the state-of-difference indicator is changed
;; automatically by the copying/recovery commands, `a', `b', `r', `+'.
;;
;; If Ediff is asked to recompute differences via the command `!', the
;; information about origins of the regions in the merge buffer (default-A,
;; prefer-B, or combined) will be lost. This is because recomputing 
;; differences in this case means running diff3 on buffers A, B, and the
;; merge buffer, not on the ancestor buffer. (It makes no sense to
;; recompute differences with the ancestor, since Ediff assumes that the user
;; doesn't edit buffers A and B, but he may have edited buffer C, and these
;; changes are to be preserved.)  Since some difference regions
;; may disappear as a result of editing in buffer C and others may arise,
;; there is generally no simple way to tell where the various regions
;; in the merge buffer came from.
;;
;; In 3-way comparison, Ediff tries to disregard regions consisting of
;; white space only as much as possible. For instance, if, say, the current
;; region in buffer A consists of the white space only (or if it is empty),
;; Ediff will not take it into account for the purpose of computing fine
;; differences. The result is that Ediff can provide more visual
;; information regarding the actual fine differences in the non-white
;; regions B and C. Moreover, if the regions in buffers B and C differ in
;; the white space only, then a message to this effect will be displayed.
;;
;; In merging, the variable
;;
;;	ediff-merge-window-share
;;
;; controls the split between window C (the window for the merge-buffer)
;; and the windows for buffers A and B. The default is 0.5. To make the
;; merge-buffer window smaller, reduce this amount. It is not recommended
;; to increase the size of the merge-window to more than half the frame
;; (i.e., to increase the default value of ediff-merge-window-share),
;; since it is then hard to see the contents of buffers A and B.
;;
;; The user can temporarily shrink the merge window to just one line by
;; typing `s'. This change is temporary, until Ediff finds a reason to
;; redraw the screen. Typing `s' again will restore the original window size.
;;
;; With a positive prefix argument, this command will make the merge window
;; slightly taller. This change will hold throughout the current Ediff
;; session. With `-' or a negative prefix argument, the command `s' makes
;; the merge window slightly shorter. This change also holds through the
;; entire current Ediff session.
;;
;; Ediff lets the user automatically skip regions where one of the buffer's
;; regions is prefered because it disagrees with the ancestor, while the
;; other buffer agrees with the ancestor. In this case, Ediff displays only
;; the difference regions where the changes made to the original clash with
;; each other. The variable that controls this behavior is
;;
;;	ediff-show-clashes-only
;;
;; The value of this variable can be toggled interactively, by typing `$'.
;; Note that this variable controls only how Ediff chooses the
;; next/previous difference to show. The user can still jump directly to
;; any difference using the command `j' (with prefix argument specifying
;; the difference number).

;; Version control support
;; -----------------------
;; Ediff supports version control via vc.el (in the standard
;; distribution of Emacs 19) and rcs.el. The latter is a package written by 
;; Sebastian Kremer <sk@thp.Uni-Koeln.DE>, which is available in
;;
;;         ftp.cs.buffalo.edu:pub/Emacs/rcs.tar.Z
;;         ftp.uni-koeln.de:/pub/gnu/emacs/rcs.tar.Z
;;
;; To specify which version control package you are using, set the variable
;; ediff-version-control-package, e.g.,
;;	(setq ediff-version-control-package 'rcs)
;; The default, is `vc'.
;; Note: both packages provide access to RCS, but only vc.el comes standard
;; with Emacs and XEmacs.
;; For files under revision control, one key (usually `=') is bound to the
;; function ediff-revision, which runs Ediff on the current buffer and one
;; of its versions. Use the variable 
;;
;;	   ediff-revision-key
;;
;; if you want to change this binding, e.g., (setq ediff-revision-key "\C-cD")

;;
;; Mode line
;; ---------
;;
;; When Ediff is running, the mode line of Ediff Control Panel buffer
;; displays the current difference being displayed and the total number of
;; difference regions in the two files. 
;;
;; The mode line of the buffers being compared displays the type of the
;; buffer (`A:' or `B:') and (usually) the file name. Ediff is trying to be
;; intelligent in choosing mode line buffer identification. In particular,
;; it works well with uniquify.el and mode-line.el packages (which improve
;; on the default way in which Emacs displays buffer identification).
;; If you don't like the way Ediff identifies its buffers, there is always
;; ediff-prepare-buffer-hooks, which can be used to modify the mode line.
;;
;; Miscellaneous
;; -------------
;; The last batch of variables that can be modified is
;;
;;  	    ediff-split-window-function
;;  	    ediff-merge-split-window-function
;;          ediff-make-wide-display-function
;;  	    ediff-use-last-dir
;;  	    ediff-no-emacs-help-in-control-buffer
;;  	    ediff-toggle-read-only-function

;; ediff-split-window-function controls the way you want the window be
;; split between file-A and file-B (and file-C, if applicable).  It
;; defaults to vertical split, but you can set it to
;; split-window-horizontally, if you want.
;; The variable ediff-merge-split-window-function controls how windows are
;; split between buffers A and B in merging jobs.

;; Ediff lets you toggle the way
;; windows are split, so you can try different settings interactively.
;; Note: if file-A and file-B (and file-C, if applicable) are in different
;; frames, windows are not split, regardless of the value
;; ediff-split-window-function.  Instead, other windows on these frames are
;; deleted and Ediff starts displaying file-A/B/C using these
;; frames, one file per frame.  You can then switch to one-frame mode
;; simply by hiding one of the buffers A/B/C.
;;
;; Note that if Ediff detects that the two buffers it compares are residing in
;; separate frames, it assumes that the user wants them to be so displayed
;; and stops splitting windows.  Instead, it will arrange each buffer to
;; occupy its own frame (possibly shared with Ediff's help window).
;;
;; The user can swap the windows in which buffers are displayed by typing `~'.
;; Furthermore, the user can toggle wide/regular display by typing
;; `m'. This is particularly useful when files are compared side-by-side.
;; By default, the display is widened without changing its height. However,
;; the user can set the variable
;;
;;    ediff-make-wide-display-function
;;
;; to contain the name of a function to be called to widen the frame in
;; which to display the buffers. See the documentation string for
;; `ediff-make-wide-display-function' for details. It is also recommended
;; to look into how the default function, `ediff-make-wide-display' is
;; written.
;;
;;
;; The variable ediff-use-last-dir controls the way Ediff presents the
;; default directory when it prompts the user for files to compare.  If nil,
;; Ediff will use the default directory of the current buffer when it
;; prompts the user for file names.  Otherwise, it will use the
;; directories it had previously used for file-A/B/C.
;;
;; The variable ediff-no-emacs-help-in-control-buffer, if set to t, makes C-h
;; behave like the DEL key, i.e., it will move you back to the previous
;; difference rather than invoking help.  This is useful when, in an xterm
;; window or on a dumb terminal, the Backspace key is bound to C-h and is
;; positioned more conveniently than the DEL key.
;;
;; The variable ediff-toggle-read-only-function can be used to change the
;; way Ediff toggles the read-only property in its buffers.
;; By default, Ediff uses toggle-read-only. For files under version
;; control, Ediff first tries to check the files out.


;;; Commands
;;  --------

;; All Ediff commands are displayed in a help window, unless you hit '?' to
;; shrink it to just one line.  You can redisplay the help window by hitting
;; '?' again.
;;
;; Many Ediff commands take numeric prefix arguments.  For instance, if you
;; hit a number, N, and then `j' (ediff-jump-to-difference), Ediff will
;; take you to Nth difference.  Hitting a number, N, and then `ab'
;; (ediff-diff-to-diff) will copy Nth difference from buffer A to buffer B.
;; Hitting `ba' does copying in the other direction. Likewise, `ca' would
;; copy from buffer C to buffer A (if buffer C exists, of course).
;; Likewise, a number, N, followed by `ra' will restore the Nth difference
;; region in buffer A (if it was previously saved as a result of copying
;; from buffer B to A). 
;;
;; Without the prefix argument, all commands operate on the current
;; difference region.
;;
;; The total number of differences and the current difference number are
;; always displayed in the mode line of the control window. 
;;
;; If, after making changes to buffers A, B, or C, you decide to save them,
;; it is recommended to use `ediff-save-buffer', which is bound to `wa', `wb',
;; and `wc' (`wa will save buffer A, `wb' saves buffer B, etc.).
;;
;; Typing `wd' saves the diff output in a file. 

;; The command `s' is used only for merging. It allows the user to shrink
;; window C to its minimal size, thereby exposing as much of buffers A and
;; B as possible. 
;; This command is intended only for temporary viewing. Therefore, Ediff
;; will restore the original window size for buffer C whenever window
;; configuration is changed by the user (on toggling help, split,
;; etc.). However, recentering and jumping to a difference doesn't affect
;; window C. Typing `s' again restores the original size of the merge
;; window.
;;
;; With a positive prefix argument, the command `s' makes the merge
;; window, window C, slightly taller. With `-' or a negative prefix
;; argument, `s' makes window C slightly shorter.
;;
;; While browsing through differences in the merge mode, one may discover
;; that the default variant was chosen inappropriately, which means that
;; the user will have to do a lot of copying manually. To facilitate this,
;; there is a command, bound to `&', which will cause Ediff to start
;; merging anew beginning with the current difference and using the
;; alternative default variant (the user is asked to type in the new
;; default for merging, which can be either `default-A', `default-B', or
;; `combined'.
;;
;; Such repeated merging affects only difference regions that have
;; default-A/B status, and only if they were not changed with respect to
;; their originals.
;;
;; Another command that is used for merging only is `+'. Its effect is to
;; combine the current difference regions of buffers A and B and put the
;; combination into the merge buffer. See `ediff-combine-diffs' and
;; `ediff-combination-pattern' for details.
;;
;; There is also one command the is not bound to any key:
;;
;;     ediff-revert-buffers-then-recompute-diffs
;;
;; It is useful when, after making changes, you decided to make a fresh
;; start, or if at some point you changed the files being compared but want
;; to discard any changes to comparison buffers that were done since then.
;; This command will ask for confirmation before reverting files. With a
;; prefix argument, it will revert files without asking.

;;; Heavy-duty customization:
;;  -------------------------

;; Some users need to customize Ediff in rather sophisticated ways, which
;; requires different defaults for different kinds of files (e.g., SGML, etc.).
;; Ediff supports this kind of customization is several ways.
;; First, most customization variables are buffer-local. Those that aren't
;; are usually accessible from within Ediff Control Panel, so one can make
;; thel local to the panel by calling make-local-variable from within
;; ediff-startup-hooks.
;; Second, there is now a new optional (6-th) argument to ediff-setup,
;; which has the form ( (var-name-1 . val-1) (var-name-2 . val-2) ...).
;; The function ediff-setup will set the variables on the list to the
;; respective values in the ediff control buffer. This is an easy way to
;; throw in custom variables (which usually should be buffer-local) that
;; can then be tested in various hooks.
;; Make sure the variable ediff-job-name and ediff-word-mode are set
;; properly in this case, as some things in Ediff depend on this.
;; Finally, if custom-tailored help messages are desired, Ediff has
;; ediff-brief-help-message-custom and ediff-long-help-message-custom,
;; which are local variables that can be either set to
;; a function that returns a string.


;;; Bugs:
;;  -----

;;  1. The undo command doesn't restore deleted regions well. That is, if
;;  you delete all characters in a difference region and then invoke
;;  `undo', the reinstated text will most likely be inserted outside of
;;  what Ediff thinks is the current difference region. (This problem
;;  doesn't seem to exist with XEmacs.)
;;
;;  If at any point you feel that difference regions are no longer correct,
;;  you can hit '!' to recompute the differences.

;;  2. Emacs 19.xx, where xx < 23, has several bugs related to overlays and
;;  faces. Somethimes, these may cause highlighting of the refinements or
;;  of the unselected differences to disappear. Hitting `!' will bring them
;;  back.  In version 19.23 and later, these problems no longer occur.

;;  3. On a monochrome display, the repertoire of faces with which to
;;  highlight fine differences is limited. By default, Ediff is using
;;  underlining. However, if the region is already underlied by some other
;;  overlays, there is no simple way to temporarily remove that residual
;;  underlining. This problem occurs when a buffer is highlighted with
;;  hilit19.el or font-lock.el packages. If this residual highlighting gets
;;  in the way, you can do the following. Both font-lock.el and hilit19.el
;;  provide commands for unhighlighting buffers. You can either place these
;;  commands in `ediff-prepare-buffer-hooks' (which will unhighlight every
;;  buffer used by Ediff) or you can execute them interactively, at any time
;;  and on any buffer.

;;  4. In XEmacs (statically linked with Motif libraries), emerge.el
;;  and hence Ediff won't start, unless you set (setq scrollbar-width 0).
;;  This is a Motif-related bug, I was told.

;;  5. XEmacs 19.11 (and, probably, earlier versions) has trouble
;;  positioning the point withing Ediff buffers on Ediff's startup.
;;  This doesn't get in the way, though, since when the user start looking
;;  at the diff regions, they are positioned correctly. It seems that
;;  XEmacs doesn't have enough time to redisplay windows---it does this
;;  correctly when it is told to redisplay (sit-for 0).

;;  6. It seems that XEmacs icons are insensitive to keyboard events. This
;;  deprives XEmacs users from being able to iconify Ediff's control panel,
;;  thereby saving space onthe screen. Also, it seems that XEmacs doesn't
;;  let one create minibuffer-less frames, which leaves Ediff control Panel
;;  with a useless minibuffer. The "unsplittable" property is also ignored
;;  in XEmacs, some further minor annoyances are possible.

;;  7. XEmacs (19.11 and below) doesn't let one create minibufferless
;;  frames. This causes the problem that messages are displayed in a small
;;  control frame window, when help is toggled off. Ediff overcomes this by
;;  setting synchronize-minibuffers to t, which causes all messages to be
;;  displayed in all minibuffers. If you detest this, set
;;  synchronize-minibuffers to nil after quitting Ediff.


;;; Change Log:
;;  ----------

;; Thu Feb  3, 1994 

;;     Fixed a bug in ediff-setup-windows that caused control window to
;;     appear in a wrong place when split-window-keep-point is nil
;;     (Thanks to Kevin Broadey <KevinB@bartley.demon.co.uk>.)
;;
;;     Added mechanism for using faces instead of before/after flags.  This
;;     looks much better on an X display, especially on a color one.
;;     (Thanks to Boris Goldowsky <boris@cs.rochester.edu> for the code
;;     that led to ediff-highlight-diff.
;;     Also, thanks to Kevin Esler <esler@ch.hp.com> for suggestions
;;     regarding highlighting differences on X displays.)
;;
;;     Added functions to apply patches.
;;     (Thanks to Kevin Broadey <KevinB@bartley.demon.co.uk> for this
;;     suggestion.)

;; Fri Feb  4, 1994 

;;     Added mechanism for toggling vertical/horizontal window split.
;;     (Inspired by a suggestion from Allan Gottlieb
;;     <gottlieb@allan.ultra.nyu.edu> -- thanks.)
;;
;;     Added mechanism for toggling between highlighting using faces and
;;     highlighting using ASCII flags.
;;
;;     Fixed a problem with undo.  Now, Ediff has smartened up and doesn't
;;     keep undo info on ASCII flags inserted in buffer-A and buffer-B.
;;     So, if you edit the files while browsing through them, undo behaves
;;     as you would expect, i.e., faces/flags don't get in the way.

;; Sun Feb  6, 1994 

;;     Added horizontal scrolling.  Added ediff-position-region to ensure
;;     that difference regions in buffer-A and buffer-B are aligned with
;;     each other.  Disabled ediff-toggle-split when buffers are displayed
;;     in different frames.
;;
;;     Added toggle-window help (Suggested by Boris Goldowsky
;;     <boris@cs.rochester.edu>.)
;;     Added functions to copy differences from one buffer to another and to
;;     recover old differences.
;;     Added prefix arguments to ediff-next-difference and
;;     ediff-previous-difference.

;; Tue Feb  8, 1994

;;     Replaced text properties with overlays.  Fixed ediff-setup-windows.
;;     Added ediff-save-buffer to local-write-file-hooks to prevent user
;;     from saving corrupted states. (Thanks to <boris@cs.rochester.edu>
;;     for suggestion.)  Instead, Ediff now has a pair of functions for
;;     safe saving of buffers. 
;;     Changed ediff-read-file-name to be more intuitive on ediff-files.
;;     Added ediff-prepare-buffer-hooks. (Thanks to Kevin Esler
;;     <esler@ch.hp.com> for the idea.)
;;
;;     Cleanups in ediff-patch-file.  Protected ediff-copy-diff against
;;     a bug that Emacs has in kill-region.
;;
;;     Added support for Lemacs. (Thanks to Alastair Burt
;;     <burt@dfki.uni-kl.de> for coercing Ediff into working under Lemacs.)
;;     Added ediff-kill-buffer-carefully and other suggestions by Boris
;;     Goldowsky <boris@cs.rochester.edu>.
;;     Refined the protection against interference with highlighting caused
;;     by Hilit19.  Added the variable ediff-third-party-highlighting.
;;     Added mechanisn for unhighlighting regions highlighted with Hilit19
;;     before hightlighting them with Ediff's overlays. (And for
;;     rehighlighting them with Hilit19, when the current difference moves on.)

;; Sun Feb 13, 1994

;;     Added ediff-place-flags-in-buffer and ediff-remote-exit, which are
;;     modifications of Emerge's similar functions.  The difference is that
;;     in Ediff they make ediff-before-flag and ediff-after-flag into
;;     read-only regions, so the user can't change them by mistake.
;;
;;     Adopted a suggestion by Boris Goldowsky <boris@cs.rochester.edu>
;;     that led to a more elegant treatment of faces.
;;
;;     Added protection against interference with Font-Lock highlighting
;;     similar to that of Hilit19's protection.

;; Tue Feb 15, 1994

;;     Deleted spurious (auto-save-mode 1) in ediff-control-buffer, which
;;     was causing this buffer to be auto-saved for no good reason.
;;     Added read-only protection to ediff-before/after-flags in Lemacs.
;;     (Thanks to Alastair Burt <burt@dfki.uni-kl.de> for help in testing.)
;;
;;     Further fixes in the XEmacs part.  Changed highlighted region in
;;     ediff-highlight-diff so that an extra character will be highlighted
;;     only if a difference is empty (thereby allowing the user to see where an
;;     insertion or a deletion has taken place).
;;
;;     Simplified interaction with other highlighting packages by giving
;;     Ediff overlays the highest priority. (Taking a cue from
;;     ediff-highlight-diff-lemacs written by Alastair Burt
;;     <burt@dfki.uni-kl.de>.) Zapped ediff-third-party-highlighting
;;     variable and hooks that were previously used to
;;     unhighlight/rehighlight buffers when hilit19/font-lock are on.

;; Fri Feb 18, 1994

;;     Added a bit more sophistication to ediff-read-file-name.  Now,
;;     ediff-files remembers both, the file-A and the file-B directories.
;;     They are offered as defaults when ediff-use-last-dir is set to t.

;; Fri Feb 22, 1994

;;     Added ediff-before-change-guard to remove ASCII highlighting when
;;     the user attempts to change buffer-A/B.  This is needed because
;;     otherwise the undo info may become screwed up in those buffers.
;;     Hitting `h' (ediff-toggle-hilit) on a dumb terminal will toggle
;;     between ASCII highlighting and no highlighting.

;; Fri Feb 24, 1994

;;     Fixed problems with multiple Ediff sessions running simultaneously.

;; Tue Mar 1, 1994

;;     Added vc-ediff, the Ediff interface to vc.el. (Thanks to Eric
;;     Freudenthal <freudent@jan.ultra.nyu.edu> for contributing this
;;     function.) 

;; Sun Mar 6, 1994

;;     Added rcs-ediff, an Ediff interface to RCS via rcs.el. (Thanks to
;;     Alastair Burt  <burt@dfki.uni-kl.de>.)
;;     Some minor improvements.

;; Tue March 15, 1994

;;     Fixed a buglet in defining ediff-current-diff-face-A/B.
;;     (Thanks to Job Ganzevoort  <Job.Ganzevoort@cwi.nl>.) 

;; Tue March 22, 1994

;;     Fixed a bug with ediffing narrowed buffers, reported by Kevin
;;     Broadey <KevinB@bartley.demon.co.uk>.
;;     Made Ediff to work with files that have incomplete last line.
;;     Made Ediff execute diff and patch using Bourne Shell, which
;;     should eliminate problems with $prompt that some people had.

;; Thu March 24, 1994

;;     Achieved quadratic speedup in the size of the file by replacing the
;;     slow goto-line by forward-line.
;;     Converted demarkation of difference regions
;;     from markers to overlays.  This will later allow us to highlight all
;;     diffs, not just the current one.

;; Wed March 30, 1994

;;     Under X, Ediff now highlights all differences in dim colors and the
;;     current difference in bright colors. Improved XEmacs support.
;;     Changed toggle hilit to cycle through 3 states: highlighting all
;;     diffs, highlighting only the current diff, and highlighting using
;;     ASCII flags.
;;     Added support for difference regions that are not full lines.

;; Fri April 1, 1994

;;     Fixed bugs related to writing buffers A and B.
;;     Added commands `ga', `gb' to jump directly to the closest diff in
;;     buffer A and B, respectively.

;; Fri April 11, 1994

;;     Added `ediff-update-diffs', a function that lets the user recompute
;;     difference regions after extensive editing done to buffers A and B
;;     (bound to `!').

;; Wed April 13, 1994

;;     Added the new feature: refining the current difference region.
;;     This would highlight the precise differences between the regions in
;;     buffer A and B. (A way to implement this was suggested by Boris
;;     Goldowsky <boris@cs.rochester.edu>.)
;;
;;     Fixed Ediff to be immune to several different versions of rcs.el
;;     that are currently in distribution.

;; Thu April 14, 1994

;;     Ediff now respects X resources for the faces it uses. It no longer
;;     barks when the colormap has no colors it is using; or when face
;;     fonts can't be italicized, etc.

;; Fri April 15, 1994

;;     Changed `ediff-setup-windows' to minimize the need to delete and
;;     create windows. Now jumps faster from diff to diff.
;;     Added Ediff to the File menu on the menu bar (FSF's version).

;; Mon April 18, 1994

;;     Fixed to work with OS/2's PM-Emacs.

;; Thu April 21, 1994

;;     Lemacs' menus added (thanks to Alastair Burt for the help).

;; Wed April 28, 1994

;;     Fixed ediff-keep-window-config (thanks to Norbert Kiesel 
;;     <norbert@i3.informatik.rwth-aachen.de>), ediff-shell and
;;     ediff-protect-metachars (thanks to Richard Stanton
;;     <stanton@haas.berkeley.edu>). Made access to difference
;;     overlays structure-independent, making it less bug-prone.
;;     Patched ediff-read-file-name to work more intuitively with directory
;;     names (thanks to Kevin Broadey <KevinB@bartley.demon.co.uk>).

;; Mon May 2, 1994

;;     Added `ediff-frame-has-menubar' to guard against the possibility that
;;     the current frame has no menu bar.

;; Fri May 6, 1994

;;     Fixed buglet in vc-ediff (thanks to Ray Nickson <nickson@cs.uq.oz.au>).

;; Wed May 18, 1994

;;     Modified ediff-read-file-name to not put long file names in the
;;     default prompt area, as suggested by KevinB@bartley.demon.co.uk.
;;     Applied patch supplied by burt@dfki.uni-kl.de, fixing a problem with
;;     ediff-diff-to-diff in Lemacs.

;; Tue May 31, 1994

;;     Added ediff-forward-word-function (as suggested by Job Ganzevoort
;;     <Job.Ganzevoort@cwi.nl>). Modified ediff-default-quit-hook so it
;;     will clean things up in a more satisfactory way.

;; Thu Jun 2, 1994

;;     Added `ediff-toggle-regexp-match', which allows the user to step
;;     through only those difference regions that match some regexp; or,
;;     vice versa, to skip over regions that match a regexp. (This feature
;;     was suggested by Andy Scott <ascott@pcocd2.intel.com>.)
;;     Added ediff-eval-in-buffer, which is a modified emerge-eval-in-buffer.
;;     The function ediff-status-info, bound to `i', now replaces and extends
;;     ediff-file-names and ediff-line-numbers, which were bound to `f'
;;     and `i', respectively.


;; Wed Jun 10, 1994

;;     Improved `ediff-read-file-name' and `ediff-buffers' so they are now
;;     providing more intuitive defaults. Modified `ediff-read-file-name'
;;     so it won't cause problems under OS/2.

;; Fri Jun 24, 1994

;;     Modified ediff-find-file, ediff-files-internal, and made
;;     emerge-verify-file-buffer into ediff-verify-file-buffer so that
;;     Ediff will work correctly with remote and compressed
;;     files. (Suggested by Sandy Rutherford <sandy@ibm550.sissa.it>.)

;; Fri Jun 28, 1994

;;     Fixed ediff-patch-files to work with remote and compressed files.

;; Wed July 20, 1994

;;     Changed menu bar items per RMS's suggestion. Changed odd/even faces
;;     in Lemacs to italic.  Changed ediff-*-face-* variables so that they
;;     will contain names of faces instead of the face internal
;;     representation.  (Copy-face works better with face names than with
;;     face internal representation.  With face internal representation, if
;;     a face vector mentions a font explicitly, copy-face may attempt to
;;     copy this font, which would cause an error if the font has a wrong
;;     size for one of the existing frames.)  Improved the way
;;     mode-line-buffer-identification is set in ediff-setup so that Ediff
;;     will accommodate the way buffers are identified in mode-line.el and
;;     uniquify.el.

;; Fri August 5, 1994

;;     Ediff can now automatically skip over regions that differ only in
;;     the white space and line breaks. This is controled with the variable
;;     `ediff-ignore-similar-regions' and can be toggled on/off by typing
;;     `##'.

;; Mon August 8, 1994

;;     If ediff-save-buffer is invoked with `wd', it'll save the diff
;;     output in a file.

;; Wed August 24, 1994

;;     Fixed ediff-toggle-read-only and ediff-patch-file so that they will
;;     check out version-controled files before modifying them. This will
;;     permit checking the modified versions back in. In earlier
;;     versions, such modifications could be lost, unless the user takes
;;     special care of preserving them.

;; Tue August 30, 1994

;;     Added ediff-submit-report.
;;     Introduced ediff-revision as a uniform way of calling vc.el and
;;     rcs.el. This is controled by ediff-version-control-package
;;     variable. Functions vc-ediff, rcs-ediff are replaced by their
;;     internal versions.
;;     Added ediff-find-file-name-handler function to smooth out the
;;     transition from Emacs 19.22/XEmacs 19.9 to 19.23/19/10

;; Thu September 1, 1994

;;     Made ediff-overlay-put and ediff-move-overlay into bona fide
;;     functions (rather than fset symbols). These now check if overlay's
;;     buffer is alive. If not, overlay is deleted. This overcomes some of
;;     the problems with Lemacs.

;; Thu September 8, 1994

;;     Added ediff-revision-key, ediff-load-version-control and streamlined
;;     vc/rcs-ediff[-internal].  Eliminated dependency on emerge.el.

;; Fri September 23, 1994

;;     Added ediff-windows and ediff-regions.
;;     Changed ediff-setup-windows and related procedures to create
;;     a separate dedicated control frame for each invocation of Ediff.

;; Tue September 27, 1994

;;     Added redraw-display everywhere before creating or deleting
;;     frames. It appears that this cures Emacs' bug where it trashes some
;;     fonts which leads to crashes. Also, some code cleanups and bug fixes.

;; Fri September 30, 1994

;;     Fixed ediff-update-diffs so it'll work correctly with
;;     ediff-windows and ediff-regions. Added narrowing and widening to the
;;     suite of commands available for ediff-windows and ediff-regions.

;; Fri October 7, 1994

;;     Changed ediff-setup-windows to funcall the actual window setting
;;     function, which is either ediff-setup-windows-multiframe or
;;     ediff-setup-windows-plain. Changed all temp file names to use `_'
;;     instead of `.'. Presumably, this makes VMS happier.
;;     Ported to VMS (thanks to Richard Levitte <levitte@e.kth.se>).
;;     Added ediff-prefer-long-help-message and changed defaults.
;;     Fighting with XEmacs bugs. Made it possible to switch between plain
;;     and multiframe display easier, but XEmacs is still getting confused
;;     at times. Added ediff-check-version and ediff-set-help-message.
;;     Made more sensible temp file names, which is important when
;;     generating context diffs. Added ediff-make-frame-position and
;;     ediff-control-frame-position-function.

;; Wed October 12, 1994

;;     ediff-window-visible-p now makes a call to ediff-frame-visible-p
;;     only when window-system is non-nil. Rearranged the block of fset's
;;     so that the wrong things won't be defined when window-system is nil.
;;     Added ediff-revert-buffers-then-recompute-diffs function.
;;     Removed flag-argument from a background call to shell-command.
;;     Added ediff-shell-command to enable custom diff execute in the
;;     background and insert output in ediff-custom-diff-buffer.
;;     Added ediff-shell-command-filter, because XEmacs doesn't have it.
;;     Made ediff-revision set up ediff-job-name to `ediff-revision'.
;;     This enables users do ediff-revision-specific actions on exiting
;;     Ediff using ediff-quit-hooks.
;;     Reshaffled some fset's so that functions for checking color and
;;     faces won't be touched on a non-windowing display.

;; Thu October 20, 1994

;;     Modified ediff-make-fine-diffs so that no fine diffs are computed if
;;     one of the diff regions is empty. Saves time and also works around
;;     the buggy diff program in AIX.
;;     ediff no longer loads the version control package
;;     automatically---only when ediff-revision is called.
;;     Enabled focusing/hiding diff regions that match both or just one of the
;;     regexps. 
;;     Changed ediff-regions to ediff-small-regions. Added ediff-large-regions.
;;     Modified ediff-next/previous-difference to work right when both
;;     skipping regexp matches and skipping similar regions is enabled.
;;     Fixed bugs in positioning the control frame.

;; Fri October 28, 1994

;;     Fixed bugs in ediff-next/previous-difference, ediff-set-visible-region
;;     Changed/added ediff-word-[1-4].

;; Tue November 1, 1994

;;     Made ediff-revision delete the temporary version files it creates.

;; Tue November 29, 1994

;;     Added ediff-swap-buffers. Split ediff-difference-vector into
;;     ediff-difference-vector-A and ediff-difference-vector-B, which
;;     allowed to factor out a lot of stuff.
;;     Made the code buffer-C ready.

;; Thu December 1, 1994

;;     Lotsa bug fixes. Further rationalized the code.
;;     Added ediff-display-help-hooks, ediff-mode-hooks.
;;     Replaced almost identical ediff-scroll-up/down with
;;     ediff-scroll-vertically.
;;     Replaced almost identical ediff-scroll-left/right with
;;     ediff-scroll-horizontally.
;;     Made the code buffer-C ready.

;; Thu December 8, 1994

;;     Added ediff-toggle-wide-display. In plain display, help message is
;;     now centered correctly.

;; Fri December 9, 1994

;;     Added ediff-toggle-multiframe (it doesn't work with XEmacs for some
;;     reason). Fixed ediff-pop-diff and ediff-copy-diff, so that they will
;;     invoke auto-refining, if necessary.

;; Mon December 12, 1994

;;     Modified ediff-toggle-wide-display so it would funcall
;;     ediff-make-wide-display-function.

;; Tue December 13, 1994

;;     Ediff now chooses its surrogate minibuffer from frame A.

;; Mon December 20, 1994

;;     Added ediff-keymap-setup-hooks.

;; Fri December 23, 1994

;;     Changed the representation in ediff-killed-diffs-alist so that
;;     ediff-save-diff-region and ediff-pop-diff won't be confused when the
;;     user swaps buffers.

;; Mon December 26, 1994

;;     Placated OS/2 by making Ediff to synchronize the call to startup
;;     hooks with the acynchronous process that computes custom diffs.
;;     This has no effect on Unix installations (and VMS?), but
;;     may increase Ediff's startup time under OS/2 by 1 or 2 seconds.

;; Thu December 29, 1994

;;     ediff-recenter now deactivates the mark, so that transient mark mode
;;     highlighting won't interfere with Ediff's highlighting.
;;     Also, ediff-recenter tries to not deiconify control frame, if it is
;;     not needed.

;; Fri December 30, 1994

;;     Small bugs. Worked around the OS/2 bug where
;;     modify-frame-parameters has no effect on iconified frames.  Ediff
;;     now remembers the iconification status of the control frame and uses
;;     it as a preferred way of displaying it when help is toggled
;;     off. (This can be observed only in Emacs (not XEmacs) and only if
;;     the window manager lets icons accept keyboard input.)

;; Tue January 3, 1995

;;     Some futher work on incorporating buffer C in ediff-extract-diffs,
;;     ediff-focus/hide-on-regexp-matches, and
;;     ediff-setup-windows-plain/multiframe. The preceding two functions
;;     now dispatch the appropriate setup function depending on whether the
;;     current job is comparison or merge.

;; Wed January 4, 1995

;;     Made it work under Emacs built without the X support.

;; Fri January 6, 1995

;;     Slightly changed the prompting behavior of ediff-files. Bug fix in
;;     mode-line-buffer-identification.

;; Wed January 18, 1995

;;     Added 3way comparison and support for diff3. Ported to NeXTStep
;;     
;; Fri January 20, 1995

;;     Added ediff-merge-files, ediff-merge-buffers,
;;     ediff-merge-files-with-ancestor, ediff-merge-buffers-with-ancestor,
;;     ediff-merge-revisions, ediff-merge-revisions-with-ancestor.

;; Tue January 24, 1995

;;     Bug fixes. Split into several files.

;; Thu January 26, 1995

;;     `*' is now bound to ediff-make-or-kill-fine-diffs. This lets the
;;     user to kill fine diffs for the current region (by providing a
;;     negative prefix arg), if there are so many of them as to hamper
;;     the viewing.

;; Mon January 30, 1995

;;     Changed ediff-selective-display to ediff-set-visible-region,
;;     ediff-toggle-selective-display to ediff-toggle-narrow-region.
;;     Ediff now turns selective display off before starting. Restores 
;;     selective display on exit.
;;     In 2-way comparison, `a' now copies to buf `b' and `b' to buf `a'.
;;     Previously, the bindings were `ab' and `ba'.

;; Wed February 1, 1995

;;     Added ediff-undo-selective-display (thanks to Stig <stig@inse.com>).
;;     Rearranged autoloads. Renamed ediff-entry.el into ediff.el and
;;     ediff.el into ediff-util.el.

;; Fri February 3

;;     Added ediff-toggle-show-clashes-only, which is bound to `$'.

;; Fri February 19

;;     Some minor patches from Stig. Also, made ediff-xemacs-p and
;;     ediff-*-job into variables for better performance.
;;     In ediff-setup, diff regions are now computed after buffers A/B/C
;;     are set up. Previously, it didn't work right with selective display.
;;     Also, added ediff-profile to time Ediff commands and
;;     ediff-debug-info for civilized display of the difference vectors
;;     (and possibly more in the future).

;; Tue March 14

;;	Fixed ediff-diff-at-point.


;;; TO DO:
;;  ------
;;
;; 1. Add support for multiple sessions. (At present, one can run
;; multiple Ediff sessions, but they won't be related.) The idea is to
;; have vars local to each control buffer, which will tell which buffer is
;; the next and which is the previous one. The user could then go forward
;; and backward by typing C-SPC and C-DEL (or C-n and C-p).
;; This will probably entail some minor modifications to ediff-setup.
;; The primary use of this feature would be comparing directories of
;; similarly named files and multi-file patch. For the latter, Ediff will
;; have to parse patches to extract the names of files.


;;; Acknowledgements:

;; Special thanks to Alastair Burt <burt@dfki.uni-kl.de>, Kevin Broadey
;; <KevinB@bartley.demon.co.uk>, Harald Boegeholz
;; <hwb@machnix.mathematik.uni-stuttgart.de>, Jin S. Choi" <jin@atype.com>,
;;  Eric Eide <eeide@asylum.cs.utah.edu>, Kevin Esler <esler@ch.hp.com>, Robert
;;  Estes <estes@ece.ucdavis.edu>, Eric Freudenthal
;;  <freudent@jan.ultra.nyu.edu>, 
;; Job Ganzevoort <Job.Ganzevoort@cwi.nl>, Boris Goldowsky
;; <boris@cs.rochester.edu>, Allan Gottlieb <gottlieb@allan.ultra.nyu.edu>,
;; Xiaoli Huang <hxl@epic.com>, Larry Gouge <larry@itginc.com>,
;; irvine@lks.csi.com, jaffe@chipmunk.cita.utoronto.ca, David Karr
;; <dkarr@nmo.gtegsc.com>, Norbert Kiesel
;; <norbert@i3.informatik.rwth-aachen.de>, Fritz Knabe <Fritz.Knabe@ecrc.de>,
;; Heinz Knutzen <hk@informatik.uni-kiel.d400.de>, Ken Laprade
;; <laprade@dw3f.ess.harris.com>, Richard Levitte
;; <levitte@e.kth.se>, Martin Maechler <maechler@stat.math.ethz.ch>,
;; Richard Mlynarik <mly@adoc.xerox.com>, Chris Murphy
;; <murphycm@sun.aston.ac.uk>, Eyvind Ness <Eyvind.Ness@hrp.no>, Ray Nickson
;; <nickson@cs.uq.oz.au>, Paul Raines <raines@slac.stanford.edu>, Tibor
;; Polgar <tlp00@spg.amdahl.com>, C.S. Roberson <roberson@aur.alcatel.com>,
;; Kevin Rodgers <kevin.rodgers@ihs.com>, Sandy Rutherford
;; <sandy@ibm550.sissa.it>, Heribert Schuetz <schuetz@ecrc.de>, Andy Scott
;; <ascott@pcocd2.intel.com>, Axel Seibert
;; <axel@tumbolia.ppp.informatik.uni-muenchen.de>, Richard Stallman
;; <rms@gnu.ai.mit.edu>, Richard Stanton <stanton@haas.berkeley.edu>,
;; Ake Stenhoff <etxaksf@aom.ericsson.se>,
;; Stig <stig@hackvan.com>, Peter Stout <Peter_Stout@cs.cmu.edu>,
;; Raymond Toy <toy@rtp.ericsson.se>,
;; and Ilya Zakharevich <ilya@math.ohio-state.edu> 
;; for contributing ideas, patches, and bug reports.
;;
;; Thanks also to many others who felt obliged to drop a thank you note.


;;; Code:

(require 'ediff-init)

(defvar ediff-version-control-package 'vc
  "Version control package used.
Currently, Ediff supports vc.el and rcs.el. Set this to `rcs' if you have
rcs.el and want to use it instead of the standard vc.el.

Note: both packages provide access to RCS, but only vc.el comes with Emacs
distribution.")

(defvar ediff-revision-key "="
  "Key to which `ediff-revision' is to be bound.")
  
(defvar ediff-use-last-dir nil
  "*If t, Ediff uses previous directory as default when reading file name.")
  
(defvar ediff-last-dir-A nil
  "Last directory used by an Ediff command for file-A.")
(defvar ediff-last-dir-B nil
  "Last directory used by an Ediff command for file-B.")
(defvar ediff-last-dir-C nil
  "Last directory used by an Ediff command for file-C.")
(defvar ediff-last-dir-ancestor nil
  "Last directory used by an Ediff command for the ancestor file.")
(defvar ediff-last-dir-patch nil
  "Last directory used by an Ediff command for file to patch.")

;;; Patching

;;;###autoload
(defun ediff-patch-file (source-filename &optional startup-hooks)
  "Run Ediff by patching FILE-TP-PATCH."
  (interactive 
   (list (ediff-read-file-name "File to patch"
			       (if ediff-use-last-dir
				   ediff-last-dir-patch
				 default-directory)
			       nil)))
  
  (setq source-filename (expand-file-name source-filename))
  (ediff-get-patch-buffer (file-name-directory source-filename))
  
  (let* ((backup-extension 
	  ;; if the user specified a -b option, extract the backup
	  ;; extension from there; else use `_orig'
	  (substring ediff-patch-options
		     (if (string-match "-b[ \t]+" ediff-patch-options)
			 (match-end 0) 0)
		     (if (string-match "-b[ \t]+[^ \t]+" ediff-patch-options)
			 (match-end 0) 0)))
	 (shell-file-name ediff-shell)
	 ;; ediff-find-file may use a temp file to do the patch
	 ;; so, we save source-filename and true-source-filename as a var
	 ;; that initially is source-filename but may be changed to a temp
	 ;; file for the purpose of patching.
	 (true-source-filename source-filename)
	 (target-filename source-filename)
	 target-buf buf-to-patch file-name-magic-p)
	  
    ;; if the user didn't specify a backup extension, use _orig
    (if (string= backup-extension "")
	(setq backup-extension "_orig"))
					
    ;; Make a temp file, if source-filename has a magic file handler (or if
    ;; it is handled via auto-mode-alist and similar magic).
    ;; Check if there is a buffer visiting source-filename and if they are in
    ;; synch; arrange for the deletion of temp file.
    (ediff-find-file 'true-source-filename 'buf-to-patch
		     'ediff-last-dir-patch 'startup-hooks)

    ;; Check if source file name has triggered black magic, such as file name
    ;; handlers or auto mode alist, and make a note of it.
    ;; true-source-filename should be either the original name or a
    ;; temporary file where we put the after-product of the file handler.
    (setq file-name-magic-p (not (equal (file-truename true-source-filename)
					(file-truename source-filename))))
    
    ;; Checkout orig file, if necessary so that the patched file could be
    ;; checked back in.
    (ediff-toggle-read-only buf-to-patch)
    
    (ediff-eval-in-buffer ediff-patch-diagnostics
      (message "Applying patch ... ")(sit-for 0)
      ;; always pass patch the -f option, so it won't ask any questions
      (shell-command-on-region 
       (point-min) (point-max)
       (format "%s -f %s -b %s %s"
	       ediff-patch-program ediff-patch-options
	       backup-extension
	       (expand-file-name true-source-filename))
       t))
    (message "Applying patch ... done")(sit-for 0)
    (switch-to-buffer ediff-patch-diagnostics)
    (sit-for 0) ; synchronize
    
    (or (file-exists-p (concat true-source-filename backup-extension))
	(error "Patch failed or didn't modify the original file"))
  
    ;; If black magic is involved, apply patch to a temp copy of the
    ;; file. Otherwise, apply patch to the orig copy.  If patch is applied
    ;; to temp copy, we name the result old-name_patched for local files
    ;; and temp-copy_patched for remote files. The orig file name isn't
    ;; changed, and the temp copy of the original is later deleted.
    ;; Without magic, the original file is renamed (usually into
    ;; old-name_orig) and the result of patching will have the same name as
    ;; the original.
    (if (not file-name-magic-p)
	(ediff-eval-in-buffer buf-to-patch
	  (set-visited-file-name (concat source-filename backup-extension))
	  (set-buffer-modified-p nil))
      
      ;; Black magic in effect.
      ;; If orig file was remote, put the patched file in the temp directory.
      ;; If orig file is local, put the patched file in the directory of
      ;; the orig file.
      (setq target-filename
	    (concat
	     (if (ediff-file-remote-p (file-truename source-filename))
		 true-source-filename
	       source-filename)
	     "_patched"))
      
      (rename-file true-source-filename target-filename t)
      
      ;; arrange that the temp copy of orig will be deleted
      (rename-file (concat true-source-filename backup-extension)
		   true-source-filename t))
    
    ;; make orig buffer read-only
    (setq startup-hooks
	  (cons 'ediff-toggle-read-only-patch-orig startup-hooks))
    
    ;; set up a buf for the patched file
    (ediff-eval-in-buffer
	(setq target-buf (find-file-noselect target-filename))
      ;; files to be patched are always checked out first
      (setq ediff-file-checked-out-flag t))
    
    (ediff-buffers buf-to-patch target-buf startup-hooks 'epatch)
  
    (bury-buffer ediff-patch-diagnostics)
    (message "Patch diagnostics available in buffer %s"
	     (buffer-name ediff-patch-diagnostics))))
  
(defun ediff-toggle-read-only-patch-orig ()
  "Used as a startup hook to set `_orig' patch file read-only."
  (ediff-toggle-read-only ediff-buffer-A))

;;;###autoload
(defalias 'epatch 'ediff-patch-file)
;;;###autoload
(defalias 'epatch-buffer 'ediff-patch-buffer)

;;; Compare files/buffers

;;;###autoload
(defun ediff-files (file-A file-B &optional startup-hooks)
  "Run Ediff on a pair of files, FILE-A and FILE-B."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B f)
     (list (setq f (ediff-read-file-name "File A to compare" dir-A nil))
	   (ediff-read-file-name "File B to compare" 
				 (setq dir-B
				       (if ediff-use-last-dir
					   ediff-last-dir-B 
					 (file-name-directory f)))
				 (progn
				   (setq file-name-history
					 (cons (abbreviate-file-name
						(expand-file-name
						 (file-name-nondirectory f)
						 dir-B))
					       file-name-history))
				   f))
	   )))
  (ediff-files-internal file-A 
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			nil ; file-C
			startup-hooks
			'ediff-files))
  
;;;###autoload
(defun ediff-files3 (file-A file-B file-C &optional startup-hooks)
  "Run Ediff on three files, FILE-A, FILE-B, and FILE-C."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B dir-C f ff)
     (list (setq f (ediff-read-file-name "File A to compare" dir-A nil))
	   (setq ff (ediff-read-file-name "File B to compare" 
					  (setq dir-B
						(if ediff-use-last-dir
						    ediff-last-dir-B
						  (file-name-directory f)))
					  (progn
					    (setq file-name-history
						  (cons
						   (abbreviate-file-name
						    (expand-file-name
						     (file-name-nondirectory f)
						     dir-B))
						   file-name-history))
					    f)))
	   (ediff-read-file-name "File C to compare" 
				 (setq dir-C (if ediff-use-last-dir
						 ediff-last-dir-C
					       (file-name-directory ff)))
				 (progn
				   (setq file-name-history
					 (cons (abbreviate-file-name
						(expand-file-name
						 (file-name-nondirectory ff)
						 dir-C))
					       file-name-history))
				   ff))
	   )))
  (ediff-files-internal file-A 
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			(if (file-directory-p file-C)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-C)
			  file-C)
			startup-hooks
			'ediff-files3))

;;;###autoload
(defalias 'ediff3 'ediff-files3)


(defun ediff-find-file (file-var buffer-name &optional last-dir hooks-var)
  "Visit FILE and arrange its buffer to Ediff's liking. 
FILE is actually a variable symbol that must contain a true file name.
BUFFER-NAME is a variable symbol, which will get the buffer object into which
FILE is read.  LAST-DIR is the directory variable symbol where FILE's
directory name should be returned. HOOKS is a variable symbol that will be
assigned the hook to be executed after `ediff-startup' is finished.
`ediff-find-file' arranges that the temp files it might create will be
deleted."
  (let* ((file (symbol-value file-var))
	 (file-magic (ediff-find-file-name-handler file))
	 (temp-file-name-prefix (file-name-nondirectory file)))
    (if (not (file-readable-p file))
	(error "File `%s' does not exist or is not readable" file))
	
    ;; some of the command, below, require full file name
    (setq file (expand-file-name file))
  
    ;; Record the directory of the file
    (if last-dir
	(set last-dir (expand-file-name (file-name-directory file))))
    
    ;; Setup the buffer
    (set buffer-name (find-file-noselect file))
  
    (ediff-eval-in-buffer (symbol-value buffer-name)
      (widen) ; Make sure the entire file is seen
      (cond (file-magic  ;; file has handler, such as jka-compr-handler or
	     ;; ange-ftp-hook-function--arrange for temp file
	     (ediff-verify-file-buffer 'magic)
	     (setq file (ediff-make-temp-file temp-file-name-prefix))
	     (set hooks-var (cons (` (lambda () (delete-file (, file))))
				  (symbol-value hooks-var))))
	    ;; file processed via auto-mode-alist, a la uncompress.el
	    ((not (equal (file-truename file)
			 (file-truename (buffer-file-name))))
	     (setq file (ediff-make-temp-file temp-file-name-prefix))
	     (set hooks-var (cons (` (lambda () (delete-file (, file))))
				  (symbol-value hooks-var))))
	    (t ;; plain file---just check that the file matches the buffer
	     (ediff-verify-file-buffer))))
    (set file-var file)))

(defun ediff-files-internal (file-A file-B file-C startup-hooks job-name)
  (let (buf-A buf-B buf-C)
    (message "Reading file %s ... " file-A)(sit-for 0)
    (ediff-find-file 'file-A 'buf-A 'ediff-last-dir-A 'startup-hooks)
    (message "Reading file %s ... " file-B)(sit-for 0)
    (ediff-find-file 'file-B 'buf-B 'ediff-last-dir-B 'startup-hooks)
    (if (and (stringp file-C) (not ediff-merge-job))
	(progn
	  (message "Reading file %s ... " file-C)(sit-for 0)
	  (ediff-find-file
	   'file-C 'buf-C
	   (if (eq job-name 'ediff-merge-files-with-ancestor)
	       'ediff-last-dir-ancestor 'ediff-last-dir-C)
	   'startup-hooks)))
    (ediff-setup buf-A file-A
		 buf-B file-B
		 buf-C file-C
		 startup-hooks
		 (list (cons 'ediff-job-name job-name)))))
  

;;;###autoload
(defalias 'ediff 'ediff-files)


;;;###autoload
(defun ediff-buffers (buffer-A buffer-B &optional startup-hooks job-name)
  "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B."
  (interactive 
   (let (bf)
     (list (setq bf (read-buffer "Buffer A to compare: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Buffer B to compare: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  
  (or job-name (setq job-name 'ediff-buffers))
  (ediff-buffers-internal buffer-A buffer-B nil startup-hooks job-name))
      
;;;###autoload
(defun ediff-buffers3 (buffer-A buffer-B buffer-C
				 &optional startup-hooks job-name)
  "Run Ediff on three buffers, BUFFER-A, BUFFER-B, and BUFFER-C."
  (interactive 
   (let (bf bff)
     (list (setq bf (read-buffer "Buffer A to compare: "
				 (ediff-other-buffer "") t))
	   (setq bff (read-buffer "Buffer B to compare: "
				  (progn
				    ;; realign buffers so that two visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer bf))
				  t))
	   (read-buffer "Buffer C to compare: "
				  (progn
				    ;; realign buffers so that three visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer (list bf bff)))
				  t)
	   )))
  
  (or job-name (setq job-name 'ediff-buffers3))
  (ediff-buffers-internal buffer-A buffer-B buffer-C startup-hooks job-name))
      

			
(defun ediff-buffers-internal (buf-A buf-B buf-C startup-hooks job-name)
  (let* ((buf-A-file-name (buffer-file-name (get-buffer buf-A)))
	 (buf-B-file-name (buffer-file-name (get-buffer buf-B)))
	 (buf-C-is-alive (ediff-buffer-live-p buf-C))
	 (buf-C-file-name (if buf-C-is-alive
			      (buffer-file-name (get-buffer buf-B))))
	 file-A file-B file-C)
    (if (not (ediff-buffer-live-p buf-A))
	(error "Buffer %S doesn't exist" buf-A))
    (if (not (ediff-buffer-live-p buf-B))
	(error "Buffer %S doesn't exist" buf-B))
    (let ((ediff-job-name job-name))
      (if (and ediff-3way-comparison-job
	       (not buf-C-is-alive))
	  (error "Buffer %S doesn't exist" buf-C)))
    (if (stringp buf-A-file-name)
	(setq buf-A-file-name (file-name-nondirectory buf-A-file-name)))
    (if (stringp buf-B-file-name)
	(setq buf-B-file-name (file-name-nondirectory buf-B-file-name)))
    (if (stringp buf-C-file-name)
	(setq buf-C-file-name (file-name-nondirectory buf-C-file-name)))
	
    ;; these three need to be evaluated in their buffers, since
    ;; ediff-make-temp-file checks the current buffer when assigning file
    ;; names
    (ediff-eval-in-buffer buf-A
      (setq file-A (ediff-make-temp-file buf-A-file-name)))
    (ediff-eval-in-buffer buf-B
      (setq file-B (ediff-make-temp-file buf-B-file-name)))
    (if buf-C-is-alive
	(ediff-eval-in-buffer buf-C
	  (setq file-C (ediff-make-temp-file buf-C-file-name))))
	  
    (ediff-setup (get-buffer buf-A) file-A
		 (get-buffer buf-B) file-B
		 (if buf-C-is-alive (get-buffer buf-C))
		 file-C
		 (cons (` (lambda ()
			    (delete-file (, file-A))
			    (delete-file (, file-B))
			    (if (stringp (, file-C)) (delete-file (, file-C)))
			    ))
		       startup-hooks)
		 (list (cons 'ediff-job-name job-name))
		 )))
		 


;;; Compare regions and windows

;;;###autoload
(defun ediff-windows (dumb-mode &optional wind-A wind-B startup-hooks)
  "Compare WIND-A and WIND-B, which are selected by clicking.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A."

  (interactive "P")
      
  (if (or dumb-mode (not window-system))
      (setq wind-A (ediff-get-next-window wind-A nil)
	    wind-B (ediff-get-next-window wind-B wind-A))
    (setq wind-A (ediff-get-window-by-clicking wind-A nil 1)
	  wind-B (ediff-get-window-by-clicking wind-B wind-A 2)))
      
  (let ((buffer-A (window-buffer wind-A))
	(buffer-B (window-buffer wind-B))
	beg-A end-A beg-B end-B)
    
    (save-excursion
      (save-window-excursion
	(sit-for 0) ; synch before using window-start/end -- a precaution
	(select-window wind-A)
	(setq beg-A (window-start)
	      end-A (window-end))
	(select-window wind-B)
	(setq beg-B (window-start)
	      end-B (window-end))))
    (ediff-regions-internal
     buffer-A beg-A end-A buffer-B beg-B end-B
     startup-hooks 'ediff-windows 'word-mode)))
     
;;;###autoload
(defun ediff-small-regions (buffer-A buffer-B &optional startup-hooks)
  "Run Ediff on a pair of regions in two different buffers.
Regions \(i.e., point and mark\) are assumed to be set in advance.
This function is effective only for relatively small regions, up to 200
lines. For large regions, use `ediff-large-regions'."
  (interactive 
   (let (bf)
     (list (setq bf (read-buffer "Region's A buffer: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Region's B buffer: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (if (not (ediff-buffer-live-p buffer-A))
      (error "Buffer %S doesn't exist" buffer-A))
  (if (not (ediff-buffer-live-p buffer-B))
      (error "Buffer %S doesn't exist" buffer-B))
  
  
  (let (reg-A-beg reg-A-end reg-B-beg reg-B-end)
    (save-excursion
      (set-buffer buffer-A)
      (setq reg-A-beg (region-beginning)
	    reg-A-end (region-end))
      (set-buffer buffer-B)
      (setq reg-B-beg (region-beginning)
	    reg-B-end (region-end)))
	    
    (ediff-regions-internal
     (get-buffer buffer-A) reg-A-beg reg-A-end
     (get-buffer buffer-B) reg-B-beg reg-B-end
     startup-hooks 'ediff-small-regions 'word-mode)))
     
;;;###autoload
(defun ediff-large-regions (buffer-A buffer-B &optional startup-hooks)
  "Run Ediff on a pair of regions in two different buffers.
Regions \(i.e., point and mark\) are assumed to be set in advance.
Each region is enlarged to contain full lines.
This function is effective for large regions, over 100-200
lines. For small regions, use `ediff-small-regions'."
  (interactive 
   (let (bf)
     (list (setq bf (read-buffer "Region A's buffer: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Region B's buffer: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  (if (not (ediff-buffer-live-p buffer-A))
      (error "Buffer %S doesn't exist" buffer-A))
  (if (not (ediff-buffer-live-p buffer-B))
      (error "Buffer %S doesn't exist" buffer-B))
  
  (let (reg-A-beg reg-A-end reg-B-beg reg-B-end)
    (save-excursion
      (set-buffer buffer-A)
      (setq reg-A-beg (region-beginning)
	    reg-A-end (region-end))
      ;; enlarge the region to hold full lines
      (goto-char reg-A-beg) 
      (beginning-of-line)
      (setq reg-A-beg (point))
      (goto-char reg-A-end) 
      (end-of-line)
      (or (eobp) (forward-char)) ; include the newline char
      (setq reg-A-end (point))
      
      (set-buffer buffer-B)
      (setq reg-B-beg (region-beginning)
	    reg-B-end (region-end))
      ;; enlarge the region to hold full lines
      (goto-char reg-A-beg) 
      (goto-char reg-B-beg) 
      (beginning-of-line)
      (setq reg-B-beg (point))
      (goto-char reg-B-end) 
      (end-of-line)
      (or (eobp) (forward-char)) ; include the newline char
      (setq reg-B-end (point))
      ) ; save excursion
	    
    (ediff-regions-internal
     (get-buffer buffer-A) reg-A-beg reg-A-end
     (get-buffer buffer-B) reg-B-beg reg-B-end
     startup-hooks 'ediff-large-regions nil))) ; no word mode
	
;; compare region beg-A to end-A of buffer-A
;; to regions beg-B -- end-B in buffer-B. 
(defun ediff-regions-internal (buffer-A beg-A end-A buffer-B beg-B end-B
					startup-hooks job-name word-mode)
  (let ((tmp-buffer (get-buffer-create ediff-tmp-buffer))
	overl-A overl-B
	file-A file-B)
	
    ;; in case beg/end-A/B aren't markers--make them into markers
    (ediff-eval-in-buffer buffer-A
      (setq beg-A (move-marker (make-marker) beg-A)
	    end-A (move-marker (make-marker) end-A)))
    (ediff-eval-in-buffer buffer-B
      (setq beg-B (move-marker (make-marker) beg-B)
	    end-B (move-marker (make-marker) end-B)))
	
    (if (and (eq buffer-A buffer-B)
	     (or (and (< beg-A end-B) (<= beg-B beg-A))   ; b-B b-A e-B
		 (and (< beg-B end-A) (<= end-A end-B)))) ; b-B e-A e-B
	(progn
	  (with-output-to-temp-buffer ediff-msg-buffer
	    (princ "
You have requested to compare overlapping regions of the same buffer.

In this case, Ediff's highlighting may be confusing---in the same window,
you may see highlighted regions that belong to different regions.

Continue anyway? (y/n) "))

	  (if (y-or-n-p "Continue anyway? ")
	      ()
	    (error "%S aborted" job-name))))
	    
    ;; make file-A
    (if word-mode
	(ediff-wordify beg-A end-A buffer-A tmp-buffer)
      (ediff-copy-to-buffer beg-A end-A buffer-A tmp-buffer))
    (ediff-eval-in-buffer tmp-buffer
      (setq file-A (ediff-make-temp-file "regA")))
    
    ;; make file-B
    (if word-mode
	(ediff-wordify beg-B end-B buffer-B tmp-buffer)
      (ediff-copy-to-buffer beg-B end-B buffer-B tmp-buffer))
    (ediff-eval-in-buffer tmp-buffer
      (setq file-B (ediff-make-temp-file "regB")))
     
    (setq overl-A (ediff-make-bullet-proof-overlay beg-A end-A buffer-A))
    (setq overl-B (ediff-make-bullet-proof-overlay beg-B end-B buffer-B))
    (ediff-setup buffer-A file-A
		 buffer-B file-B
		 nil nil	    ; buffer & file C
		 (cons (` (lambda ()
			    (delete-file (, file-A))
			    (delete-file (, file-B))))
		       startup-hooks)
		 (list (cons 'ediff-word-mode  word-mode)
		       (cons 'ediff-narrow-bounds (list overl-A overl-B))
		       (cons 'ediff-job-name job-name))
		 )
    ))
    
 
;;; Merge files and buffers
  
;;;###autoload
(defalias 'ediff-merge 'ediff-merge-files)
  
(defsubst ediff-merge-on-startup ()
  (ediff-do-merge 0)
  (ediff-eval-in-buffer ediff-buffer-C
    (set-buffer-modified-p nil)))

;;;###autoload
(defun ediff-merge-files (file-A file-B &optional startup-hooks)
  "Merge two files without ancestor."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B f)
     (list (setq f (ediff-read-file-name "File A to merge" dir-A nil))
	   (ediff-read-file-name "File B to merge" 
				 (setq dir-B
				       (if ediff-use-last-dir
					   ediff-last-dir-B 
					 (file-name-directory f)))
				 (progn
				   (setq file-name-history
					 (cons (abbreviate-file-name
						(expand-file-name
						 (file-name-nondirectory f)
						 dir-B))
					       file-name-history))
				   f))
	   )))
  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (ediff-files-internal file-A 
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			  nil ; file-C
			  startup-hooks
			  'ediff-merge-files))
			  
;;;###autoload
(defun ediff-merge-files-with-ancestor (file-A file-B file-ancestor
					       &optional startup-hooks)
  "Merge two files with ancestor."
  (interactive
   (let ((dir-A (if ediff-use-last-dir
		    ediff-last-dir-A
		  default-directory))
	 dir-B dir-ancestor f ff)
     (list (setq f (ediff-read-file-name "File A to merge" dir-A nil))
	   (setq ff (ediff-read-file-name "File B to merge" 
					  (setq dir-B
						(if ediff-use-last-dir
						    ediff-last-dir-B 
						  (file-name-directory f)))
					  (progn
					    (setq file-name-history
						  (cons
						   (abbreviate-file-name
						    (expand-file-name
						     (file-name-nondirectory f)
						     dir-B))
						   file-name-history))
					    f)))
	   (ediff-read-file-name "Ancestor file" 
				 (setq dir-ancestor
				       (if ediff-use-last-dir
					   ediff-last-dir-ancestor
					 (file-name-directory ff)))
				 (progn
				   (setq file-name-history
					 (cons (abbreviate-file-name
						(expand-file-name
						 (file-name-nondirectory ff)
						 dir-ancestor))
					       file-name-history))
				   ff))
	   )))
  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (ediff-files-internal file-A 
			(if (file-directory-p file-B)
			    (expand-file-name
			     (file-name-nondirectory file-A) file-B)
			  file-B)
			  file-ancestor
			  startup-hooks
			  'ediff-merge-files-with-ancestor))
			  
;;;###autoload
(defalias 'ediff-merge-with-ancestor 'ediff-merge-files-with-ancestor)
			  
;;;###autoload
(defun ediff-merge-buffers (buffer-A buffer-B &optional startup-hooks job-name)
  "Merge buffers without ancestor."
  (interactive 
   (let (bf)
     (list (setq bf (read-buffer "Buffer A to merge: "
				 (ediff-other-buffer "") t))
	   (read-buffer "Buffer B to merge: "
			(progn
			  ;; realign buffers so that two visible bufs will be
			  ;; at the top
			  (save-window-excursion (other-window 1))
			  (ediff-other-buffer bf))
			t))))
  
  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (or job-name (setq job-name 'ediff-merge-buffers))
  (ediff-buffers-internal
   buffer-A buffer-B nil startup-hooks job-name))
   
;;;###autoload
(defun ediff-merge-buffers-with-ancestor (buffer-A 
					  buffer-B buffer-ancestor
					  &optional startup-hooks job-name)
  "Merge buffers with ancestor."
  (interactive 
   (let (bf bff)
     (list (setq bf (read-buffer "Buffer A to merge: "
				 (ediff-other-buffer "") t))
	   (setq bff (read-buffer "Buffer B to merge: "
				  (progn
				    ;; realign buffers so that two visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer bf))
				  t))
	   (read-buffer "Ancestor buffer: "
				  (progn
				    ;; realign buffers so that three visible
				    ;; bufs will be at the top
				    (save-window-excursion (other-window 1))
				    (ediff-other-buffer (list bf bff)))
				  t)
	   )))
  
  (setq startup-hooks (cons 'ediff-merge-on-startup startup-hooks))
  (or job-name (setq job-name 'ediff-merge-buffers-with-ancestor))
  (ediff-buffers-internal
   buffer-A buffer-B buffer-ancestor startup-hooks job-name))
      

;;;###autoload
(defun ediff-merge-revisions (rev1 rev2 &optional startup-hooks)
  "Run Ediff by merging two revisions of a file.
The file is the one visited by the current buffer."
  (interactive
   "sVersion 1 to merge (default is the latest version): \nsVersion 2 to merge (default is the latest version): ")
  (ediff-load-version-control)
  (let (buf1 buf2)
    (if (eq ediff-version-control-package 'vc)
	 (progn
	   (save-excursion
	     (vc-version-other-window rev1)
	     (setq buf1 (current-buffer)))
	   (save-excursion
	     (vc-version-other-window rev2)
	     (setq buf2 (current-buffer)))
	   (setq startup-hooks 
		 (list (` (lambda () 
			    (delete-file (, (buffer-file-name buf1)))
			    (delete-file (, (buffer-file-name buf2))))))))
      (setq buf1 (rcs-ediff-view-revision rev1)
	    buf2 (rcs-ediff-view-revision rev2)))
    (ediff-merge-buffers buf1 buf2 startup-hooks 'ediff-merge-revisions)))
    

;;;###autoload
(defun ediff-merge-revisions-with-ancestor (rev1
					    rev2 ancestor-rev
					    &optional startup-hooks)
  "Run Ediff by merging with ancestor of two revisions of a file.
The file is the one visited by the current buffer."
  (interactive
   "sVersion 1 to merge (default: the latest version): \nsVersion 2 to merge (default: the latest version): \nsAncestor version (default: the latest version): ")
  (ediff-load-version-control)
  (let (buf1 buf2 ancestor-buf)
    (if (eq ediff-version-control-package 'vc)
	 (progn
	   (save-excursion
	     (vc-version-other-window rev1)
	     (setq buf1 (current-buffer)))
	   (save-excursion
	     (vc-version-other-window rev2)
	     (setq buf2 (current-buffer)))
	   (save-excursion
	     (vc-version-other-window ancestor-rev)
	     (setq ancestor-buf (current-buffer)))
	   (setq startup-hooks 
		 (list (` (lambda () 
			    (delete-file (, (buffer-file-name buf1)))
			    (delete-file (, (buffer-file-name buf2)))
			    (delete-file (, (buffer-file-name ancestor-buf)))
			    )))))
      (setq buf1 (rcs-ediff-view-revision rev1)
	    buf2 (rcs-ediff-view-revision rev2)
	    ancestor-buf (rcs-ediff-view-revision ancestor-rev)))
    (ediff-merge-buffers-with-ancestor
     buf1 buf2 ancestor-buf
     startup-hooks 'ediff-merge-revisions-with-ancestor)))
     
     
;;; Apply patch
          
    
;;;###autoload
(defun ediff-patch-buffer (buffer-name &optional startup-hooks)		  
  "Run Ediff by patching BUFFER-NAME."
  (interactive "bBuffer to patch: ")
  
  (let* ((file-buffer (get-buffer buffer-name))
	 (file-name (if file-buffer (buffer-file-name  file-buffer))))
    (if (not file-name)
	(error "Buffer %s doesn't exist or doesn't visit any file.  Why patch?"
	       file-name))
    
    (ediff-patch-file file-name startup-hooks)))


(defun ediff-get-patch-buffer (dir)
  "Obtain patch buffer.  If patch is already in a buffer---use it.
Else, read patch file into a new buffer."
  (if (y-or-n-p "Is the patch file already in a buffer? ")
      (setq ediff-patch-buf
	    (get-buffer (read-buffer "Patch buffer name: " nil t))) ;must match
    (setq ediff-patch-buf
	  (find-file-noselect (read-file-name "Patch file name: " dir))))
  
  ;; secure the patch buffer against accidental changes
  (ediff-eval-in-buffer ediff-patch-buf
    (setq buffer-read-only t))
   
  (setq ediff-patch-diagnostics
	(get-buffer-create "*ediff patch diagnostics*"))
  (ediff-eval-in-buffer
      ediff-patch-diagnostics
    (insert-buffer ediff-patch-buf))
  )


      


;;; Versions Control functions      
      
;;;###autoload
(defun ediff-revision (revision)
  "Call `vc.el' or `rcs.el' depending on `ediff-version-control-package'.
This function is introduced to provide a uniform interface to version
control packages from Ediff."
  (interactive "sVersion to Ediff with (default: the latest version): ")
  (ediff-load-version-control)
  (funcall
   (intern (format "%S-ediff-internal" ediff-version-control-package))
   revision))
   
;; Backward compatibility
;;;###autoload
(defun vc-ediff ()
  (interactive)
  (beep 1)
  (with-output-to-temp-buffer ediff-msg-buffer
    (princ "
You have invoked an obsolete function `vc-ediff' or `rcs-ediff'.
Please use `M-x ediff-revision' instead.

Also, please check the variables `ediff-version-control-package'
and `ediff-revision-key' for customization.")))

(defalias 'rcs-ediff 'vc-ediff)
   
;; Test if version control package is loaded and load if not
;; Is SILENT is non-nil, don't report error if package is not found.
(defun ediff-load-version-control (&optional silent)
  (or (featurep ediff-version-control-package)
      (if (locate-library (symbol-name ediff-version-control-package))
	  (progn
	    (message "") ; kill the message from `locate-library'
	    (require ediff-version-control-package)
	    (define-key
	      (cond ((eq ediff-version-control-package 'vc) vc-prefix-map)
		    ((eq ediff-version-control-package 'rcs) global-map)
		    (t  global-map))
	      ediff-revision-key 'ediff-revision))
	(or silent
	    (error "Version control package %S.el not found. Use vc.el instead" 
		   ediff-version-control-package)))))
  
      
;; Note: this function will work only with Emacs 19.22 or later.
(defun vc-ediff-internal (rev)
  "Run Ediff on version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (let ((newvers (current-buffer)))
    (vc-version-other-window rev)
    ;; current-buffer is now supposed to contain the old version
    ;; in another window
    ;; We delete the temp file that was created by vc.el for the old
    ;; version
    (ediff-buffers (current-buffer) newvers
		   (list (` (lambda () (delete-file (, (buffer-file-name))))))
		   'ediff-revision)
    ))
    
(defun rcs-ediff-view-revision (&optional rev)
  "View previous RCS revision of current file.
With prefix argument, prompts for a revision name." 
  (interactive (list (if current-prefix-arg 
			 (read-string "Revision: "))))
  (let* ((filename (buffer-file-name (current-buffer)))
	 (switches (append '("-p")
			   (if rev (list (concat "-r" rev)) nil)))
	 (buff (concat (file-name-nondirectory filename) ".~" rev "~")))
    (message "Working ...")
    (setq filename (expand-file-name filename))
    (with-output-to-temp-buffer buff
      (let ((output-buffer (ediff-rcs-get-output-buffer filename buff)))
	(delete-windows-on output-buffer)
	(save-excursion
	  (set-buffer output-buffer)
	  (apply 'call-process "co" nil t nil
		 ;; -q: quiet (no diagnostics)
		 (append switches rcs-default-co-switches
			 (list "-q" filename))))) 
      (message "")
      buff)))    
      
(defun ediff-rcs-get-output-buffer (file name)
  ;; Get a buffer for RCS output for FILE, make it writable and clean it up.
  ;; Optional NAME is name to use instead of `*RCS-output*'.
  ;; This is a modified version from rcs.el v1.1. I use it here to make
  ;; Ediff immune to changes in rcs.el
  (let* ((default-major-mode 'fundamental-mode) ; no frills!
	 (buf (get-buffer-create name)))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil
	    default-directory (file-name-directory (expand-file-name file)))
      (erase-buffer))
    buf))

(defun rcs-ediff-internal (rev)
  "Run Ediff on the current buffer, comparing it with previous RCS revision."
  (let ((newvers (current-buffer))
	(oldvers (rcs-ediff-view-revision rev)))
	
    ;; rcs.el doesn't create temp version files, so we don't have to delete
    ;; anything in startup hooks to ediff-buffers
    (ediff-buffers oldvers newvers nil 'ediff-revision)
    ))

;;; Menu bar

;;; This is split in several parts to avoid
;;; making a line in loaddefs.el that is too long for patch.
;;; Note that autoload.el currently looks for cookies
;;; only at top level in the file.
;;; So I moved these to top level.  But the conditionals on
;;; purify-flag make these no-ops when you load ediff.
;;; They only do something in loaddefs.el.

;;;###autoload
(if purify-flag
    ;; explicit string-match, as ediff-xemacs-p is not defined at build time
    (if (string-match "\\(Lucid\\|Xemacs\\)" emacs-version)
	()
      (defvar menu-bar-epatch-menu (make-sparse-keymap "Epatch"))
      (fset 'menu-bar-epatch-menu (symbol-value 'menu-bar-epatch-menu))
      (defvar menu-bar-ediff-merge-menu (make-sparse-keymap "Ediff merge"))
      (fset 'menu-bar-ediff-merge-menu 
	    (symbol-value 'menu-bar-ediff-merge-menu))
      (defvar menu-bar-ediff-menu (make-sparse-keymap "Ediff"))
      (fset 'menu-bar-ediff-menu (symbol-value 'menu-bar-ediff-menu))
      ))


;;;###autoload
(if purify-flag
    ;; explicit string-match, as ediff-xemacs-p is not defined at build time
    (if (string-match "\\(Lucid\\|Xemacs\\)" emacs-version)
	()
      (define-key menu-bar-ediff-menu [ediff-revision]
	'("File with Revision ..." . ediff-revision))
      (define-key menu-bar-ediff-menu [ediff-large-regions]
	'("Large Regions ..." . ediff-large-regions))
      (define-key menu-bar-ediff-menu [ediff-small-regions]
	'("Small Regions ..." . ediff-small-regions))
      (define-key menu-bar-ediff-menu [ediff-windows]
	'("Windows ..." . ediff-windows))
      ))
     
;;;###autoload
(if purify-flag
    ;; explicit string-match, as ediff-xemacs-p is not defined at build time
    (if (string-match "\\(Lucid\\|Xemacs\\)" emacs-version)
	()
      (define-key menu-bar-ediff-menu [ediff-buffers3]
	'("Three Buffers ..." . ediff-buffers3))
      (define-key menu-bar-ediff-menu [ediff-files3]
	'("Three Files ..." . ediff-files3))
      (define-key menu-bar-ediff-menu [ediff-buffers]
	'("Two Buffers ..." . ediff-buffers))
      (define-key menu-bar-ediff-menu [ediff-files]
	'("Two Files ..." . ediff-files))
      ))

;;;###autoload
(if purify-flag
    ;; explicit string-match, as ediff-xemacs-p is not defined at build time
    (if (string-match "\\(Lucid\\|Xemacs\\)" emacs-version)
	()
      (define-key
	menu-bar-ediff-merge-menu [ediff-merge-revisions-with-ancestor]
	'("Revisions with Ancestor ..." . ediff-merge-revisions-with-ancestor))
      (define-key menu-bar-ediff-merge-menu [ediff-merge-revisions]
	'("Revisions ..." . ediff-merge-revisions))
      (define-key menu-bar-ediff-merge-menu [ediff-merge-buffers-with-ancestor]
	'("Buffers with Ancestor ..." . ediff-merge-buffers-with-ancestor))
      (define-key menu-bar-ediff-merge-menu [ediff-merge-buffers]
	'("Buffers ..." . ediff-merge-buffers))
      (define-key menu-bar-ediff-merge-menu [ediff-merge-files-with-ancestor]
	'("Files with Ancestor ..." . ediff-merge-files-with-ancestor))
      (define-key menu-bar-ediff-merge-menu [ediff-merge-files]
	'("Files ..." . ediff-merge-files))
      ))
      
;;;###autoload
(if purify-flag
    ;; explicit string-match, as ediff-xemacs-p is not defined at build time
    (if (string-match "\\(Lucid\\|Xemacs\\)" emacs-version)
	()
      (define-key menu-bar-epatch-menu [ediff-patch-buffer]
	'("To a Buffer ..." . ediff-patch-buffer))
      (define-key menu-bar-epatch-menu [ediff-patch-file]
	'("To a File ..." . ediff-patch-file))
      ))
	

;;;###autoload
(if purify-flag
    ;; explicit string-match, as ediff-xemacs-p is not defined at build time
    (if (string-match "\\(Lucid\\|Xemacs\\)" emacs-version)
	(progn
	  (defvar ediff-menu
	    '(""
	      ["Two Files ..."  ediff-files t]
	      ["Two Buffers ..." ediff-buffers t]
	      ["Three Files ..."  ediff-files3 t]
	      ["Three Buffers ..." ediff-buffers3 t]
	      ["Windows ..." ediff-windows t]
	      ["Small Regions ..." ediff-small-regions t]
	      ["Large Regions ..." ediff-large-regions t]
	      ["File with Revision ..."  ediff-revision t]))
	  (defvar ediff-merge-menu
	    '(""
	      ["Files ..."  ediff-merge-files t]
	      ["Files with Ancestor ..." ediff-merge-files-with-ancestor t]
	      ["Buffers ..."  ediff-merge-buffers t]
	      ["Buffers with Ancestor ..."
	       ediff-merge-buffers-with-ancestor t]
	      ["Revisions ..."  ediff-merge-revisions t]
	      ["Revisions with Ancestor ..."
	       ediff-merge-revisions-with-ancestor t]))
	  (defvar epatch-menu
	    '(""
	      ["To a file ..."  ediff-patch-file t]
	      ["To a buffer ..." ediff-patch-buffer t]))
	  (add-menu '("File") "Compare" 
		    ediff-menu
		    "New Frame")
	  (add-menu '("File") "Merge" 
		    ediff-merge-menu
		    "New Frame")
	  (add-menu '("File") "Apply Patch" 
		    epatch-menu
		    "New Frame")
	  ;; Display a solid horizontal line 
	  (add-menu-item '("File") "---" nil nil "New Screen")
	  )))


(provide 'ediff)
(require 'ediff-util)

;;; ediff.el ends here
