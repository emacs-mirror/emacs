;;; ediff.el --- a comprehensive visual interface to diff & patch
;;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.sunysb.edu>
;; Created: February 2, 1994
;; Keywords: comparing, merging, patching, version control.

(defconst ediff-version "2.26" "The current version of Ediff")
(defconst ediff-date "June 3, 1995" "Date of last update")  

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
;; the differences between a pair (or a triple) of files or buffers.  The
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

;; Ediff is aware of version control, which lets the user compare
;; files with their older versions. Ediff can also work with remote and
;; compressed files. Details are given below.

;; This package builds upon the ideas borrowed from emerge.el and
;; several Ediff's functions are adaptations from emerge.el. 
;; Much of the functionality of Ediff is also influenced by emerge.el.

;; The present version of Ediff supersedes Emerge. It provides a superior
;; user interface and has many features not found in Emerge. In particular,
;; it can do patching and 2-way and 3-way file comparison in addition to
;; merging.



;;; Compilation
;;  -----------
;;
;; When you byte-compile Ediff, you will get some warnings about functions
;; being undefined.  These can be safely ignored.
;;
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

;;  2. On a monochrome display, the repertoire of faces with which to
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
;;

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
;;     <Job.Ganzevoort@cwi.nl>).

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

;;     ediff-window-visible-p now makes a call to frame-visible-p
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

;;     Added ediff-toggle-multiframe.
;;     Fixed ediff-pop-diff and ediff-copy-diff, so that they will
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

;; Tue March 18

;;	Fixed ediff-diff-at-point and ediff-toggle-multiframe.
;;	Added ediff-destroy-control-frame, ediff-window-display-p. The latter
;;      replaces window-system in many cases. Needed because in XEmacs 19.12
;;      window-system returns 'tty on a tty display.
;;	Converted xemacs *screen* nomenclature to *frame*.
;;	Made ediff-patch-buffer cope with buffers that don't visit any file.
;;	Fixed ediff-toggle-read-only so it knows the difference between
;;	version-controlled files and others. It also knows whether we are using
;;	patch or not.
;;	Renamed ediff-windows to ediff-windows-wordwise, added
;;	ediff-windows-linewise. Changed ediff-small/large-regions to
;;      ediff-regions-wordwise/linewise 

;; Tue May 2

;;	Added ediff-documentation. Fixes for XEmacs 19.12.
;;	Merge buffer now assumes the major mode of ediff-default-variant.

;; Mon May 31, 1995

;;     Ediff-revision now takes a prefix argument. Can compare two versions of
;;     the same file. Cleaned up ediff-make-control-frame.
;;     Fixed a bug in ediff-get-visible-buffer-window.
;;     Added ediff-cleanup-hooks, ediff-janitor.
;;     ediff-cleanup-hooks is called before ediff-quit-hooks.


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
;; Karl Heuer <kwzh@gnu.ai.mit.edu>, <irvine@lks.csi.com>,
;; <jaffe@chipmunk.cita.utoronto.ca>, David Karr
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

(defvar ediff-revision-key nil
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
(defun ediff-patch-file (source-filename &optional startup-hooks job-name)
  "Run Ediff by patching FILE-TP-PATCH."
  ;; This now returns the control buffer
  (interactive 
   (list (ediff-read-file-name "File to patch"
			       (if ediff-use-last-dir
				   ediff-last-dir-patch
				 default-directory)
			       nil)))
  
  (setq source-filename (expand-file-name source-filename))
  (ediff-get-patch-buffer
   (if (eq job-name 'ediff-patch-buffer)
       (ediff-eval-in-buffer (get-file-buffer source-filename)
	 default-directory)
     (file-name-directory source-filename)))
  
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
	 target-buf buf-to-patch file-name-magic-p ctl-buf)
	  
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
    
    ;; Checkout orig file, if necessary, so that the patched file could be
    ;; checked back in.
    (if (ediff-file-checked-in-p (buffer-file-name buf-to-patch))
	(ediff-toggle-read-only buf-to-patch))
    
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
	  (cons 'ediff-set-read-only-in-buf-A startup-hooks))
    
    ;; set up a buf for the patched file
    (setq target-buf (find-file-noselect target-filename))
    
    (setq ctl-buf
	  (ediff-buffers-internal
	   buf-to-patch target-buf nil
	   startup-hooks '(or job-name ediff-patch-file)))
  
    (bury-buffer ediff-patch-diagnostics)
    (message "Patch diagnostics are available in buffer %s"
	     (buffer-name ediff-patch-diagnostics))
    ctl-buf))
  
(defun ediff-set-read-only-in-buf-A ()
  "Used as a startup hook to set `_orig' patch file read-only."
  (ediff-eval-in-buffer ediff-buffer-A
    (toggle-read-only 1)))

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
	 (file-magic (find-file-name-handler file 'find-file-noselect))
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
(defun ediff-windows-wordwise (dumb-mode &optional wind-A wind-B startup-hooks)
  "Compare WIND-A and WIND-B, which are selected by clicking, wordwise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A."
  (interactive "P")
  (ediff-windows dumb-mode wind-A wind-B
		 startup-hooks 'ediff-windows-wordwise 'word-mode))
		 
;;;###autoload
(defun ediff-windows-linewise (dumb-mode &optional wind-A wind-B startup-hooks)
  "Compare WIND-A and WIND-B, which are selected by clicking, linewise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A."
  (interactive "P")
  (ediff-windows dumb-mode wind-A wind-B
		 startup-hooks 'ediff-windows-linewise nil))
      
;; Compare WIND-A and WIND-B, which are selected by clicking.
;; With prefix argument, DUMB-MODE, or on a non-windowing display,
;; works as follows:
;; If WIND-A is nil, use selected window.
;; If WIND-B is nil, use window next to WIND-A.
(defun ediff-windows (dumb-mode wind-A wind-B startup-hooks job-name word-mode)
  (if (or dumb-mode (not (ediff-window-display-p)))
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
     startup-hooks job-name word-mode)))
     
;;;###autoload
(defun ediff-regions-wordwise (buffer-A buffer-B &optional startup-hooks)
  "Run Ediff on a pair of regions in two different buffers.
Regions \(i.e., point and mark\) are assumed to be set in advance.
This function is effective only for relatively small regions, up to 200
lines. For large regions, use `ediff-regions-linewise'."
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
     startup-hooks 'ediff-regions-wordwise 'word-mode)))
     
;;;###autoload
(defun ediff-regions-linewise (buffer-A buffer-B &optional startup-hooks)
  "Run Ediff on a pair of regions in two different buffers.
Regions \(i.e., point and mark\) are assumed to be set in advance.
Each region is enlarged to contain full lines.
This function is effective for large regions, over 100-200
lines. For small regions, use `ediff-regions-wordwise'."
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
     startup-hooks 'ediff-regions-linewise nil))) ; no word mode
	
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
  
  (let* ((buf-to-patch (get-buffer buffer-name))
	 (file-name-ok (if buf-to-patch (buffer-file-name  buf-to-patch)))
	 (buf-mod-status (buffer-modified-p buf-to-patch))
	 default-dir file-name ctl-buf)
    (if file-name-ok
	(setq file-name file-name-ok)
      (ediff-eval-in-buffer buffer-name
	(setq default-dir default-directory)
	(setq file-name (ediff-make-temp-file))
	(set-visited-file-name file-name)
	(setq buffer-auto-save-file-name nil) ; don't create auto-save file
	(rename-buffer buffer-name) ; don't confuse the user with new buf name
	(set-buffer-modified-p nil)
	(set-visited-file-modtime) ; sync buffer and temp file
	(setq default-directory default-dir)
	))
    
    (setq ctl-buf
	  (ediff-patch-file file-name startup-hooks 'ediff-patch-buffer))
    
    (if file-name-ok
	()
      (ediff-eval-in-buffer ctl-buf
	(delete-file (buffer-file-name ediff-buffer-A))
	(delete-file (buffer-file-name ediff-buffer-B))
	(ediff-eval-in-buffer ediff-buffer-A
	  (if default-dir (setq default-directory default-dir))
	  (set-visited-file-name nil)
	  (rename-buffer buffer-name)
	  (set-buffer-modified-p buf-mod-status))
	(ediff-eval-in-buffer ediff-buffer-B
	  (setq buffer-auto-save-file-name nil) ; don't create auto-save file
	  (if default-dir (setq default-directory default-dir))
	  (set-visited-file-name nil)
	  (rename-buffer (ediff-unique-buffer-name 
			  (concat buffer-name "_patched") ""))
	  (set-buffer-modified-p t))))
    ))


(defun ediff-get-patch-buffer (dir)
  "Obtain patch buffer.  If patch is already in a buffer---use it.
Else, read patch file into a new buffer."
  (if (y-or-n-p "Is the patch file already in a buffer? ")
      (setq ediff-patch-buf
	    (get-buffer (read-buffer "Patch buffer name: " nil t))) ;must match
    (setq ediff-patch-buf
	  (find-file-noselect (read-file-name "Patch file name: " dir))))
  
  (setq ediff-patch-diagnostics
	(get-buffer-create "*ediff patch diagnostics*"))
  (ediff-eval-in-buffer ediff-patch-diagnostics
    (insert-buffer ediff-patch-buf)))


      


;;; Versions Control functions      
      
;;;###autoload
(defun ediff-revision (arg)
  "Call `vc.el' or `rcs.el' depending on `ediff-version-control-package'.
Without prefix argument, compares the current buffer with an older version.
With prefix argument, compares two older versions of the current buffer."
  (interactive "P")
  (let (rev1 rev2)
    (if arg
	(setq rev1
	      (read-string
	       "This buffer's version-1 to compare (default: the latest version): ")
	      rev2
	      (read-string "This buffer's version-2 to compare (default: the latest version): "))
      (setq rev1
	    (read-string "Version to compare the current buffer with (default: the latest version): ")))
    (ediff-load-version-control)
    (funcall
     (intern (format "%S-ediff-internal" ediff-version-control-package))
     rev1 rev2)
    ))
   
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
	    (if ediff-revision-key
		(define-key
		  (cond ((eq ediff-version-control-package 'vc) vc-prefix-map)
			((eq ediff-version-control-package 'rcs) global-map)
			(t  global-map))
		  ediff-revision-key 'ediff-revision)))
	(or silent
	    (error "Version control package %S.el not found. Use vc.el instead"
		   ediff-version-control-package)))))
  
      
(defun vc-ediff-internal (rev1 &optional rev2)
  "Run Ediff on versions of the current buffer.
If both REV1 and REV2 are given then these two versions are compared.
If only REV1 is given then the current buffer is compared against version REV1.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (let ((curbuf (current-buffer))
	(curwind (selected-window))
	file1 file2
	rev1buf rev2buf)
    (vc-version-other-window rev1)
    (setq rev1buf (current-buffer)
	  file1 (buffer-file-name))
    (select-window curwind)
    (if (not (stringp rev2))
	(setq rev2buf curbuf)
      (vc-version-other-window rev2)
      (setq rev2buf (current-buffer)
	    file2 (buffer-file-name)))
    (ediff-buffers
     rev1buf rev2buf
     (list (` (lambda ()
		(delete-file (, file1))
		(if (, file2) (delete-file (, file2)))
		)))
     'ediff-revision)))
    
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

(defun rcs-ediff-internal (rev1 &optional rev2)
  "Run Ediff on the current buffer, comparing it with previous RCS revision."
  (let ((rev2buf (if (stringp rev2)
		     (rcs-ediff-view-revision rev2)
		   (current-buffer)))
	(rev1buf (rcs-ediff-view-revision rev1)))
	
    ;; rcs.el doesn't create temp version files, so we don't have to delete
    ;; anything in startup hooks to ediff-buffers
    (ediff-buffers rev1buf rev2buf nil 'ediff-revision)
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

;;;   These must be placed in menu-bar.el in Emacs
;;
;;      (define-key menu-bar-tools-menu [epatch]
;;	'("Apply Patch" . menu-bar-epatch-menu))
;;      (define-key menu-bar-tools-menu [ediff-merge]
;;	'("Merge" . menu-bar-ediff-merge-menu))
;;      (define-key menu-bar-tools-menu [ediff]
;;	'("Compare" . menu-bar-ediff-menu))


;;;###autoload
(if purify-flag
    ;; explicit string-match, as ediff-xemacs-p is not defined at build time
    (if (string-match "\\(Lucid\\|Xemacs\\)" emacs-version)
	()
      (define-key menu-bar-ediff-menu [ediff-revision]
	'("File with Revision ..." . ediff-revision))
      (define-key menu-bar-file-menu [separator-ediff-files] '("--"))
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
      (define-key menu-bar-file-menu [separator-ediff-regions] '("--"))
      (define-key menu-bar-ediff-menu [ediff-regions-linewise]
	'("Regions Line-by-line ..." . ediff-regions-linewise))
      (define-key menu-bar-ediff-menu [ediff-regions-wordwise]
	'("Regions Word-by-word ..." . ediff-regions-wordwise))
      (define-key menu-bar-file-menu [separator-ediff-windows] '("--"))
      (define-key menu-bar-ediff-menu [ediff-windows-linewise]
	'("Windows Line-by-line ..." . ediff-windows-linewise))
      (define-key menu-bar-ediff-menu [ediff-windows-wordwise]
	'("Windows Word-by-word ..." . ediff-windows-wordwise))
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
      (define-key menu-bar-file-menu [separator-ediff-merge] '("--"))
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
	    '("Compare"
	      ["Two Files ..."  ediff-files t]
	      ["Two Buffers ..." ediff-buffers t]
	      ["Three Files ..."  ediff-files3 t]
	      ["Three Buffers ..." ediff-buffers3 t]
	      "---"
	      ["File with Revision ..."  ediff-revision t]
	      "---"
	      ["Windows Word-by-word ..." ediff-windows-wordwise t]
	      ["Windows Line-by-line ..." ediff-windows-linewise t]
	      "---"
	      ["Regions Word-by-word ..." ediff-regions-wordwise t]
	      ["Regions Line-by-line ..." ediff-regions-linewise t]))
	  (defvar ediff-merge-menu
	    '("Merge"
	      ["Files ..."  ediff-merge-files t]
	      ["Files with Ancestor ..." ediff-merge-files-with-ancestor t]
	      ["Buffers ..."  ediff-merge-buffers t]
	      ["Buffers with Ancestor ..."
	       ediff-merge-buffers-with-ancestor t]
	      "---"
	      ["Revisions ..."  ediff-merge-revisions t]
	      ["Revisions with Ancestor ..."
	       ediff-merge-revisions-with-ancestor t]))
	  (defvar epatch-menu
	    '("Apply Patch"
	      ["To a file ..."  ediff-patch-file t]
	      ["To a buffer ..." ediff-patch-buffer t]))
	  (add-submenu '("Tools") ediff-menu "VC")
	  (add-submenu '("Tools") ediff-merge-menu "VC")
	  (add-submenu '("Tools") epatch-menu "VC")
	  ;; Display a solid horizontal line 
	  (add-menu-button '("Tools") ["---" nil nil] "VC")
	  )))


(provide 'ediff)
(require 'ediff-util)

;;; ediff.el ends here
