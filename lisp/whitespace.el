;;; whitespace.el --- minor mode to visualize TAB, (HARD) SPACE, NEWLINE

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: data, wp
;; Version: 9.2
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package is a minor mode to visualize blanks (TAB, (HARD) SPACE
;; and NEWLINE).
;;
;; whitespace uses two ways to visualize blanks: faces and display
;; table.
;;
;; * Faces are used to highlight the background with a color.
;;   whitespace uses font-lock to highlight blank characters.
;;
;; * Display table changes the way a character is displayed, that is,
;;   it provides a visual mark for characters, for example, at the end
;;   of line (?\xB6), at SPACEs (?\xB7) and at TABs (?\xBB).
;;
;; The `whitespace-style' and `whitespace-chars' variables are used to
;; select which way should be used to visualize blanks.
;;
;; Note that when whitespace is turned on, whitespace saves the
;; font-lock state, that is, if font-lock is on or off.  And
;; whitespace restores the font-lock state when it is turned off.  So,
;; if whitespace is turned on and font-lock is off, whitespace also
;; turns on the font-lock to highlight blanks, but the font-lock will
;; be turned off when whitespace is turned off.  Thus, turn on
;; font-lock before whitespace is on, if you want that font-lock
;; continues on after whitespace is turned off.
;;
;; When whitespace is on, it takes care of highlighting some special
;; characters over the default mechanism of `nobreak-char-display'
;; (which see) and `show-trailing-whitespace' (which see).
;;
;; There are two ways of using whitespace: local and global.
;;
;; * Local whitespace affects only the current buffer.
;;
;; * Global whitespace affects all current and future buffers.  That
;;   is, if you turn on global whitespace and then create a new
;;   buffer, the new buffer will also have whitespace on.  The
;;   `whitespace-global-modes' variable controls which major-mode will
;;   be automagically turned on.
;;
;; You can mix the local and global usage without any conflict.  But
;; local whitespace has priority over global whitespace.  Whitespace
;; mode is active in a buffer if you have enabled it in that buffer or
;; if you have enabled it globally.
;;
;; When global and local whitespace are on:
;;
;; * if local whitespace is turned off, whitespace is turned off for
;;   the current buffer only.
;;
;; * if global whitespace is turned off, whitespace continues on only
;;   in the buffers in which local whitespace is on.
;;
;; To use whitespace, insert in your ~/.emacs:
;;
;;    (require 'whitespace-mode)
;;
;; Or autoload at least one of the commands`whitespace-mode',
;; `whitespace-toggle-options', `global-whitespace-mode' or
;; `global-whitespace-toggle-options'.  For example:
;;
;;    (autoload 'whitespace-mode           "whitespace"
;;      "Toggle whitespace visualization."        t)
;;    (autoload 'whitespace-toggle-options "whitespace"
;;      "Toggle local `whitespace-mode' options." t)
;;
;; whitespace was inspired by:
;;
;;    whitespace.el            Rajesh Vaidheeswarran <rv@gnu.org>
;;	Warn about and clean bogus whitespaces in the file
;;	(inspired the idea to warn and clean some blanks)
;;	This was the original `whitespace.el' which was replaced by
;;	`blank-mode.el'.  And later `blank-mode.el' was renamed to
;;	`whitespace.el'.
;;
;;    show-whitespace-mode.el  Aurelien Tisne <aurelien.tisne@free.fr>
;;       Simple mode to highlight whitespaces
;;       (inspired the idea to use font-lock)
;;
;;    whitespace-mode.el       Lawrence Mitchell <wence@gmx.li>
;;       Major mode for editing Whitespace
;;       (inspired the idea to use display table)
;;
;;    visws.el                 Miles Bader <miles@gnu.org>
;;       Make whitespace visible
;;       (handle display table, his code was modified, but the main
;;       idea was kept)
;;
;;
;; Using whitespace
;; ----------------
;;
;; There is no problem if you mix local and global minor mode usage.
;;
;; * LOCAL whitespace:
;;    + To toggle whitespace options locally, type:
;;
;;         M-x whitespace-toggle-options RET
;;
;;    + To activate whitespace locally, type:
;;
;;         C-u 1 M-x whitespace-mode RET
;;
;;    + To deactivate whitespace locally, type:
;;
;;         C-u 0 M-x whitespace-mode RET
;;
;;    + To toggle whitespace locally, type:
;;
;;         M-x whitespace-mode RET
;;
;; * GLOBAL whitespace:
;;    + To toggle whitespace options globally, type:
;;
;;         M-x global-whitespace-toggle-options RET
;;
;;    + To activate whitespace globally, type:
;;
;;         C-u 1 M-x global-whitespace-mode RET
;;
;;    + To deactivate whitespace globally, type:
;;
;;         C-u 0 M-x global-whitespace-mode RET
;;
;;    + To toggle whitespace globally, type:
;;
;;         M-x global-whitespace-mode RET
;;
;; There are also the following useful commands:
;;
;; `whitespace-cleanup'
;;    Cleanup some blank problems in all buffer or at region.
;;
;; `whitespace-cleanup-region'
;;    Cleanup some blank problems at region.
;;
;; `whitespace-buffer'
;;    Turn on `whitespace-mode' forcing some settings.
;;
;; The problems, which are cleaned up, are:
;;
;; 1. empty lines at beginning of buffer.
;; 2. empty lines at end of buffer.
;;    If `whitespace-chars' includes the value `empty', remove all
;;    empty lines at beginning and/or end of buffer.
;;
;; 3. 8 or more SPACEs at beginning of line.
;;    If `whitespace-chars' includes the value `indentation', replace
;;    8 or more SPACEs at beginning of line by TABs.
;;
;; 4. SPACEs before TAB.
;;    If `whitespace-chars' includes the value `space-before-tab',
;;    replace SPACEs by TABs.
;;
;; 5. SPACEs or TABs at end of line.
;;    If `whitespace-chars' includes the value `trailing', remove all
;;    SPACEs or TABs at end of line."
;;
;; 6. 8 or more SPACEs after TAB.
;;    If `whitespace-chars' includes the value `space-after-tab',
;;    replace SPACEs by TABs.
;;
;;
;; Hooks
;; -----
;;
;; whitespace has the following hook variables:
;;
;; `whitespace-mode-hook'
;;    It is evaluated always when whitespace is turned on locally.
;;
;; `global-whitespace-mode-hook'
;;    It is evaluated always when whitespace is turned on globally.
;;
;; `whitespace-load-hook'
;;    It is evaluated after whitespace package is loaded.
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of whitespace options, please,
;; see the options declaration in the code for a long documentation.
;;
;; `whitespace-style'		Specify the visualization style.
;;
;; `whitespace-chars'		Specify which kind of blank is
;;				visualized.
;;
;; `whitespace-space'		Face used to visualize SPACE.
;;
;; `whitespace-hspace'		Face used to visualize HARD SPACE.
;;
;; `whitespace-tab'		Face used to visualize TAB.
;;
;; `whitespace-newline'		Face used to visualize NEWLINE char
;;				mapping.
;;
;; `whitespace-trailing'	Face used to visualize trailing
;;				blanks.
;;
;; `whitespace-line'		Face used to visualize "long" lines.
;;
;; `whitespace-space-before-tab'	Face used to visualize SPACEs
;;					before TAB.
;;
;; `whitespace-indentation'	Face used to visualize 8 or more
;;				SPACEs at beginning of line.
;;
;; `whitespace-empty'		Face used to visualize empty lines at
;;				beginning and/or end of buffer.
;;
;; `whitespace-space-after-tab'	Face used to visualize 8 or more
;;				SPACEs after TAB.
;;
;; `whitespace-space-regexp'	Specify SPACE characters regexp.
;;
;; `whitespace-hspace-regexp'	Specify HARD SPACE characters regexp.
;;
;; `whitespace-tab-regexp'	Specify TAB characters regexp.
;;
;; `whitespace-trailing-regexp'	Specify trailing characters regexp.
;;
;; `whitespace-space-before-tab-regexp'	Specify SPACEs before TAB
;;					regexp.
;;
;; `whitespace-indentation-regexp'	Specify regexp for 8 or more
;;					SPACEs at beginning of line.
;;
;; `whitespace-empty-at-bob-regexp'	Specify regexp for empty lines
;;					at beginning of buffer.
;;
;; `whitespace-empty-at-eob-regexp'	Specify regexp for empty lines
;;					at end of buffer.
;;
;; `whitespace-space-after-tab-regexp'	Specify regexp for 8 or more
;;					SPACEs after TAB.
;;
;; `whitespace-line-column'	Specify column beyond which the line
;;				is highlighted.
;;
;; `whitespace-display-mappings'	Specify an alist of mappings
;;					for displaying characters.
;;
;; `whitespace-global-modes'	Modes for which global
;;				`whitespace-mode' is automagically
;;				turned on.
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to nschum (EmacsWiki) for the idea about highlight "long"
;; lines tail.  See EightyColumnRule (EmacsWiki).
;;
;; Thanks to Juri Linkov <juri@jurta.org> for suggesting:
;;    * `define-minor-mode'.
;;    * `global-whitespace-*' name for global commands.
;;
;; Thanks to Robert J. Chassell <bob@gnu.org> for doc fix and testing.
;;
;; Thanks to Drew Adams <drew.adams@oracle.com> for toggle commands
;; suggestion.
;;
;; Thanks to Antti Kaihola <antti.kaihola@linux-aktivaattori.org> for
;; helping to fix `find-file-hooks' reference.
;;
;; Thanks to Andreas Roehler <andreas.roehler@easy-emacs.de> for
;; indicating defface byte-compilation warnings.
;;
;; Thanks to TimOCallaghan (EmacsWiki) for the idea about highlight
;; "long" lines.  See EightyColumnRule (EmacsWiki).
;;
;; Thanks to Yanghui Bian <yanghuibian@gmail.com> for indicating a new
;; newline character mapping.
;;
;; Thanks to Pete Forman <pete.forman@westgeo.com> for indicating
;; whitespace-mode.el on XEmacs.
;;
;; Thanks to Miles Bader <miles@gnu.org> for handling display table via
;; visws.el (his code was modified, but the main idea was kept).
;;
;; Thanks to:
;;    Rajesh Vaidheeswarran <rv@gnu.org>	(original) whitespace.el
;;    Aurelien Tisne <aurelien.tisne@free.fr>	show-whitespace-mode.el
;;    Lawrence Mitchell <wence@gmx.li>		whitespace-mode.el
;;    Miles Bader <miles@gnu.org>		visws.el
;; And to all people who contributed with them.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User Variables:


;;; Interface to the command system


(defgroup whitespace nil
  "Visualize blanks (TAB, (HARD) SPACE and NEWLINE)."
  :link '(emacs-library-link :tag "Source Lisp File" "whitespace.el")
  :version "23.1"
  :group 'wp
  :group 'data)


(defcustom whitespace-style '(mark color)
  "*Specify the visualization style.

It's a list containing some or all of the following values:

   mark		display mappings are visualized.

   color	faces are visualized.

Any other value is ignored.

If nil, don't visualize TABs, (HARD) SPACEs and NEWLINEs.

See also `whitespace-display-mappings' for documentation."
  :type '(repeat :tag "Style of Blank"
		 (choice :tag "Style of Blank"
			 (const :tag "Display Table" mark)
			 (const :tag "Faces" color)))
  :group 'whitespace)


(defcustom whitespace-chars
  '(tabs spaces trailing lines space-before-tab newline
	 indentation empty space-after-tab)
  "*Specify which kind of blank is visualized.

It's a list containing some or all of the following values:

   trailing		trailing blanks are visualized.

   tabs			TABs are visualized.

   spaces		SPACEs and HARD SPACEs are visualized.

   lines		lines whose have columns beyond
			`whitespace-line-column' are highlighted.
			Whole line is highlighted.
			It has precedence over
			`lines-tail' (see below).

   lines-tail		lines whose have columns beyond
			`whitespace-line-column' are highlighted.
			But only the part of line which goes
			beyond `whitespace-line-column' column.
			It has effect only if `lines' (see above)
			is not present in `whitespace-chars'.

   space-before-tab	SPACEs before TAB are visualized.

   newline		NEWLINEs are visualized.

   indentation		8 or more SPACEs at beginning of line are
			visualized.

   empty		empty lines at beginning and/or end of buffer
			are visualized.

   space-after-tab	8 or more SPACEs after a TAB are visualized.

Any other value is ignored.

If nil, don't visualize TABs, (HARD) SPACEs and NEWLINEs.

Used when `whitespace-style' includes the value `color'.
Used also when `whitespace-chars' includes `newline',
and `whitespace-style' includes `mark'."
  :type '(repeat :tag "Kind of Blank"
		 (choice :tag "Kind of Blank"
			 (const :tag "Trailing TABs, SPACEs and HARD SPACEs"
				trailing)
			 (const :tag "SPACEs and HARD SPACEs" spaces)
			 (const :tag "TABs" tabs)
			 (const :tag "Lines" lines)
			 (const :tag "SPACEs before TAB"
				space-before-tab)
			 (const :tag "NEWLINEs" newline)
			 (const :tag "Indentation SPACEs" indentation)
			 (const :tag "Empty Lines At BOB And/Or EOB"
				empty)
			 (const :tag "SPACEs after TAB"
				space-after-tab)))
  :group 'whitespace)


(defcustom whitespace-space 'whitespace-space
  "*Symbol face used to visualize SPACE.

Used when `whitespace-style' includes the value `color'."
  :type 'face
  :group 'whitespace)


(defface whitespace-space
  '((((class color) (background dark))
     (:background "grey20"      :foreground "aquamarine3"))
    (((class color) (background light))
     (:background "LightYellow" :foreground "aquamarine3"))
    (t (:inverse-video t)))
  "Face used to visualize SPACE."
  :group 'whitespace)


(defcustom whitespace-hspace 'whitespace-hspace
  "*Symbol face used to visualize HARD SPACE.

Used when `whitespace-style' includes the value `color'."
  :type 'face
  :group 'whitespace)


(defface whitespace-hspace		; 'nobreak-space
  '((((class color) (background dark))
     (:background "grey24"        :foreground "aquamarine3"))
    (((class color) (background light))
     (:background "LemonChiffon3" :foreground "aquamarine3"))
    (t (:inverse-video t)))
  "Face used to visualize HARD SPACE."
  :group 'whitespace)


(defcustom whitespace-tab 'whitespace-tab
  "*Symbol face used to visualize TAB.

Used when `whitespace-style' includes the value `color'."
  :type 'face
  :group 'whitespace)


(defface whitespace-tab
  '((((class color) (background dark))
     (:background "grey22" :foreground "aquamarine3"))
    (((class color) (background light))
     (:background "beige"  :foreground "aquamarine3"))
    (t (:inverse-video t)))
  "Face used to visualize TAB."
  :group 'whitespace)


(defcustom whitespace-newline 'whitespace-newline
  "*Symbol face used to visualize NEWLINE char mapping.

See `whitespace-display-mappings'.

Used when `whitespace-style' includes the values `mark'
and `color', and `whitespace-chars' includes `newline'."
  :type 'face
  :group 'whitespace)


(defface whitespace-newline
  '((((class color) (background dark))
     (:background "grey26" :foreground "aquamarine3" :bold t))
    (((class color) (background light))
     (:background "linen"  :foreground "aquamarine3" :bold t))
    (t (:bold t :underline t)))
  "Face used to visualize NEWLINE char mapping.

See `whitespace-display-mappings'."
  :group 'whitespace)


(defcustom whitespace-trailing 'whitespace-trailing
  "*Symbol face used to visualize traling blanks.

Used when `whitespace-style' includes the value `color'."
  :type 'face
  :group 'whitespace)


(defface whitespace-trailing		; 'trailing-whitespace
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "red1" :foreground "yellow" :bold t)))
  "Face used to visualize trailing blanks."
  :group 'whitespace)


(defcustom whitespace-line 'whitespace-line
  "*Symbol face used to visualize \"long\" lines.

See `whitespace-line-column'.

Used when `whitespace-style' includes the value `color'."
  :type 'face
  :group 'whitespace)


(defface whitespace-line
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "gray20" :foreground "violet")))
  "Face used to visualize \"long\" lines.

See `whitespace-line-column'."
  :group 'whitespace)


(defcustom whitespace-space-before-tab 'whitespace-space-before-tab
  "*Symbol face used to visualize SPACEs before TAB.

Used when `whitespace-style' includes the value `color'."
  :type 'face
  :group 'whitespace)


(defface whitespace-space-before-tab
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "DarkOrange" :foreground "firebrick")))
  "Face used to visualize SPACEs before TAB."
  :group 'whitespace)


(defcustom whitespace-indentation 'whitespace-indentation
  "*Symbol face used to visualize 8 or more SPACEs at beginning of line.

Used when `whitespace-style' includes the value `color'."
  :type 'face
  :group 'whitespace)


(defface whitespace-indentation
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "yellow" :foreground "firebrick")))
  "Face used to visualize 8 or more SPACEs at beginning of line."
  :group 'whitespace)


(defcustom whitespace-empty 'whitespace-empty
  "*Symbol face used to visualize empty lines at beginning and/or end of buffer.

Used when `whitespace-style' includes the value `color'."
  :type 'face
  :group 'whitespace)


(defface whitespace-empty
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "yellow" :foreground "firebrick")))
  "Face used to visualize empty lines at beginning and/or end of buffer."
  :group 'whitespace)


(defcustom whitespace-space-after-tab 'whitespace-space-after-tab
  "*Symbol face used to visualize 8 or more SPACEs after TAB.

Used when `whitespace-style' includes the value `color'."
  :type 'face
  :group 'whitespace)


(defface whitespace-space-after-tab
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "yellow" :foreground "firebrick")))
  "Face used to visualize 8 or more SPACEs after TAB."
  :group 'whitespace)


(defcustom whitespace-hspace-regexp
  "\\(\\(\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\)+\\)"
  "*Specify HARD SPACE characters regexp.

If you're using `mule' package, there may be other characters besides:

   \"\\xA0\"   \"\\x8A0\"   \"\\x920\"   \"\\xE20\"   \"\\xF20\"

that should be considered HARD SPACE.

Here are some examples:

   \"\\\\(^\\xA0+\\\\)\"		\
visualize only leading HARD SPACEs.
   \"\\\\(\\xA0+$\\\\)\"		\
visualize only trailing HARD SPACEs.
   \"\\\\(^\\xA0+\\\\|\\xA0+$\\\\)\"	\
visualize leading and/or trailing HARD SPACEs.
   \"\\t\\\\(\\xA0+\\\\)\\t\"		\
visualize only HARD SPACEs between TABs.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `whitespace-style' includes the value `color',
and `whitespace-chars' includes `spaces'."
  :type '(regexp :tag "HARD SPACE Chars")
  :group 'whitespace)


(defcustom whitespace-space-regexp "\\( +\\)"
  "*Specify SPACE characters regexp.

If you're using `mule' package, there may be other characters
besides \" \" that should be considered SPACE.

Here are some examples:

   \"\\\\(^ +\\\\)\"		visualize only leading SPACEs.
   \"\\\\( +$\\\\)\"		visualize only trailing SPACEs.
   \"\\\\(^ +\\\\| +$\\\\)\"	\
visualize leading and/or trailing SPACEs.
   \"\\t\\\\( +\\\\)\\t\"	visualize only SPACEs between TABs.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `whitespace-style' includes the value `color',
and `whitespace-chars' includes `spaces'."
  :type '(regexp :tag "SPACE Chars")
  :group 'whitespace)


(defcustom whitespace-tab-regexp "\\(\t+\\)"
  "*Specify TAB characters regexp.

If you're using `mule' package, there may be other characters
besides \"\\t\" that should be considered TAB.

Here are some examples:

   \"\\\\(^\\t+\\\\)\"		visualize only leading TABs.
   \"\\\\(\\t+$\\\\)\"		visualize only trailing TABs.
   \"\\\\(^\\t+\\\\|\\t+$\\\\)\"	\
visualize leading and/or trailing TABs.
   \" \\\\(\\t+\\\\) \"	visualize only TABs between SPACEs.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `whitespace-style' includes the value `color',
and `whitespace-chars' includes `tabs'."
  :type '(regexp :tag "TAB Chars")
  :group 'whitespace)


(defcustom whitespace-trailing-regexp
  "\t\\| \\|\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20"
  "*Specify trailing characters regexp.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

NOTE: DO NOT enclose by \\\\( and \\\\) the elements to highlight.
      `whitespace-mode' surrounds this regexp by \"\\\\(\\\\(\" and
      \"\\\\)+\\\\)$\".

Used when `whitespace-style' includes the value `color',
and `whitespace-chars' includes `trailing'."
  :type '(regexp :tag "Trailing Chars")
  :group 'whitespace)


(defcustom whitespace-space-before-tab-regexp "\\( +\\)\t"
  "*Specify SPACEs before TAB regexp.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `whitespace-style' includes the value `color',
and `whitespace-chars' includes `space-before-tab'."
  :type '(regexp :tag "SPACEs Before TAB")
  :group 'whitespace)


(defcustom whitespace-indentation-regexp
  "^\t*\\(\\( \\{8\\}\\)+\\)[^\n\t]"
  "*Specify regexp for 8 or more SPACEs at beginning of line.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `whitespace-style' includes the value `color',
and `whitespace-chars' includes `indentation'."
  :type '(regexp :tag "Indentation SPACEs")
  :group 'whitespace)


(defcustom whitespace-empty-at-bob-regexp "\\`\\(\\([ \t]*\n\\)+\\)"
  "*Specify regexp for empty lines at beginning of buffer.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `whitespace-style' includes the value `color',
and `whitespace-chars' includes `empty'."
  :type '(regexp :tag "Empty Lines At Beginning Of Buffer")
  :group 'whitespace)


(defcustom whitespace-empty-at-eob-regexp "^\\([ \t\n]+\\)\\'"
  "*Specify regexp for empty lines at end of buffer.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `whitespace-style' includes the value `color',
and `whitespace-chars' includes `empty'."
  :type '(regexp :tag "Empty Lines At End Of Buffer")
  :group 'whitespace)


(defcustom whitespace-space-after-tab-regexp "\t\\(\\( \\{8\\}\\)+\\)"
  "*Specify regexp for 8 or more SPACEs after TAB.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `whitespace-style' includes the value `color',
and `whitespace-chars' includes `space-after-tab'."
  :type '(regexp :tag "SPACEs After TAB")
  :group 'whitespace)


(defcustom whitespace-line-column 80
  "*Specify column beyond which the line is highlighted.

Used when `whitespace-style' includes the value `color',
and `whitespace-chars' includes `lines' or `lines-tail'."
  :type '(integer :tag "Line Length")
  :group 'whitespace)


;; Hacked from `visible-whitespace-mappings' in visws.el
(defcustom whitespace-display-mappings
  ;; Due to limitations of glyph representation, the char code can not
  ;; be above ?\x1FFFF.  Probably, this will be fixed after Emacs
  ;; unicode merging.
  '(
    (?\     [?\xB7]       [?.])		; space - centered dot
    (?\xA0  [?\xA4]       [?_])		; hard space - currency
    (?\x8A0 [?\x8A4]      [?_])		; hard space - currency
    (?\x920 [?\x924]      [?_])		; hard space - currency
    (?\xE20 [?\xE24]      [?_])		; hard space - currency
    (?\xF20 [?\xF24]      [?_])		; hard space - currency
    ;; NEWLINE is displayed using the face `whitespace-newline'
    (?\n    [?$ ?\n])			; end-of-line - dollar sign
    ;; (?\n    [?\u21B5 ?\n] [?$ ?\n])	; end-of-line - downwards arrow
    ;; (?\n    [?\xB6 ?\n]   [?$ ?\n])	; end-of-line - pilcrow
    ;; (?\n    [?\x8AF ?\n]  [?$ ?\n])	; end-of-line - overscore
    ;; (?\n    [?\x8AC ?\n]  [?$ ?\n])	; end-of-line - negation
    ;; (?\n    [?\x8B0 ?\n]  [?$ ?\n])	; end-of-line - grade
    ;;
    ;; WARNING: the mapping below has a problem.
    ;; When a TAB occupies exactly one column, it will display the
    ;; character ?\xBB at that column followed by a TAB which goes to
    ;; the next TAB column.
    ;; If this is a problem for you, please, comment the line below.
    (?\t    [?\xBB ?\t]   [?\\ ?\t])	; tab - left quote mark
    )
  "*Specify an alist of mappings for displaying characters.

Each element has the following form:

   (CHAR VECTOR...)

Where:

CHAR	is the character to be mapped.

VECTOR	is a vector of characters to be displayed in place of CHAR.
	The first display vector that can be displayed is used;
	if no display vector for a mapping can be displayed, then
	that character is displayed unmodified.

The NEWLINE character is displayed using the face given by
`whitespace-newline' variable.  The characters in the vector to
be displayed will not have this face applied if the character
code is above #x1FFFF.

Used when `whitespace-style' includes the value `mark'."
  :type '(repeat
	  (list :tag "Character Mapping"
		(character :tag "Char")
		(repeat :inline t :tag "Vector List"
			(vector :tag ""
				(repeat :inline t
					:tag "Vector Characters"
					(character :tag "Char"))))))
  :group 'whitespace)


(defcustom whitespace-global-modes t
  "*Modes for which global `whitespace-mode' is automagically turned on.

Global `whitespace-mode' is controlled by the command
`global-whitespace-mode'.

If nil, means no modes have `whitespace-mode' automatically
turned on.

If t, all modes that support `whitespace-mode' have it
automatically turned on.

Else it should be a list of `major-mode' symbol names for which
`whitespace-mode' should be automatically turned on.  The sense
of the list is negated if it begins with `not'.  For example:

   (c-mode c++-mode)

means that `whitespace-mode' is turned on for buffers in C and
C++ modes only."
  :type '(choice (const :tag "None" nil)
		 (const :tag "All" t)
		 (set :menu-tag "Mode Specific" :tag "Modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t
			      (symbol :tag "Mode"))))
  :group 'whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Local mode


;;;###autoload
(define-minor-mode whitespace-mode
  "Toggle whitespace minor mode visualization (\"ws\" on modeline).

If ARG is null, toggle whitespace visualization.
If ARG is a number greater than zero, turn on visualization;
otherwise, turn off visualization.
Only useful with a windowing system."
  :lighter    " ws"
  :init-value nil
  :global     nil
  :group      'whitespace
  (cond
   (noninteractive			; running a batch job
    (setq whitespace-mode nil))
   (whitespace-mode			; whitespace-mode on
    (whitespace-turn-on))
   (t					; whitespace-mode off
    (whitespace-turn-off))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Global mode


(define-minor-mode global-whitespace-mode
  "Toggle whitespace global minor mode visualization (\"WS\" on modeline).

If ARG is null, toggle whitespace visualization.
If ARG is a number greater than zero, turn on visualization;
otherwise, turn off visualization.
Only useful with a windowing system."
  :lighter    " WS"
  :init-value nil
  :global     t
  :group      'whitespace
  (cond
   (noninteractive			; running a batch job
    (setq global-whitespace-mode nil))
   (global-whitespace-mode		; global-whitespace-mode on
    (save-excursion
      (add-hook 'find-file-hook 'whitespace-turn-on-if-enabled t)
      (dolist (buffer (buffer-list))	; adjust all local mode
	(set-buffer buffer)
	(unless whitespace-mode
	  (whitespace-turn-on-if-enabled)))))
   (t					; global-whitespace-mode off
    (save-excursion
      (remove-hook 'find-file-hook 'whitespace-turn-on-if-enabled)
      (dolist (buffer (buffer-list))	; adjust all local mode
	(set-buffer buffer)
	(unless whitespace-mode
	  (whitespace-turn-off)))))))


(defun whitespace-turn-on-if-enabled ()
  (when (cond
	 ((eq whitespace-global-modes t))
	 ((listp whitespace-global-modes)
	  (if (eq (car-safe whitespace-global-modes) 'not)
	      (not (memq major-mode (cdr whitespace-global-modes)))
	    (memq major-mode whitespace-global-modes)))
	 (t nil))
    (let (inhibit-quit)
      ;; Don't turn on whitespace mode if...
      (or
       ;; ...we don't have a display (we're running a batch job)
       noninteractive
       ;; ...or if the buffer is invisible (name starts with a space)
       (eq (aref (buffer-name) 0) ?\ )
       ;; ...or if the buffer is temporary (name starts with *)
       (and (eq (aref (buffer-name) 0) ?*)
	    ;; except the scratch buffer.
	    (not (string= (buffer-name) "*scratch*")))
       ;; Otherwise, turn on whitespace mode.
       (whitespace-turn-on)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Toggle


(defconst whitespace-chars-value-list
  '(tabs
    spaces
    trailing
    space-before-tab
    lines
    lines-tail
    newline
    indentation
    empty
    space-after-tab
    )
  "List of valid `whitespace-chars' values.")


(defconst whitespace-style-value-list
  '(color
    mark
    )
  "List of valid `whitespace-style' values.")


(defconst whitespace-toggle-option-alist
  '((?t . tabs)
    (?s . spaces)
    (?r . trailing)
    (?b . space-before-tab)
    (?l . lines)
    (?L . lines-tail)
    (?n . newline)
    (?i . indentation)
    (?e . empty)
    (?a . space-after-tab)
    (?c . color)
    (?m . mark)
    (?x . whitespace-chars)
    (?z . whitespace-style)
    )
  "Alist of toggle options.

Each element has the form:

   (CHAR . SYMBOL)

Where:

CHAR	is a char which the user will have to type.

SYMBOL	is a valid symbol associated with CHAR.
	See `whitespace-chars-value-list' and
	`whitespace-style-value-list'.")


(defvar whitespace-active-chars nil
  "Used to save locally `whitespace-chars' value.")
(make-variable-buffer-local 'whitespace-active-chars)

(defvar whitespace-active-style nil
  "Used to save locally `whitespace-style' value.")
(make-variable-buffer-local 'whitespace-active-style)


;;;###autoload
(defun whitespace-toggle-options (arg)
  "Toggle local `whitespace-mode' options.

If local whitespace-mode is off, toggle the option given by ARG
and turn on local whitespace-mode.

If local whitespace-mode is on, toggle the option given by ARG
and restart local whitespace-mode.

Interactively, it reads one of the following chars:

  CHAR	MEANING
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   b	toggle SPACEs before TAB visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   n	toggle NEWLINE visualization
   i	toggle indentation SPACEs visualization
   e	toggle empty line at bob and/or eob visualization
   a	toggle SPACEs after TAB visualization
   c	toggle color faces
   m	toggle visual mark
   x	restore `whitespace-chars' value
   z	restore `whitespace-style' value
   ?	display brief help

Non-interactively, ARG should be a symbol or a list of symbols.
The valid symbols are:

   tabs			toggle TAB visualization
   spaces		toggle SPACE and HARD SPACE visualization
   trailing		toggle trailing blanks visualization
   space-before-tab	toggle SPACEs before TAB visualization
   lines		toggle \"long lines\" visualization
   lines-tail		toggle \"long lines\" tail visualization
   newline		toggle NEWLINE visualization
   indentation		toggle indentation SPACEs visualization
   empty		toggle empty line at bob and/or eob visualization
   space-after-tab	toggle SPACEs after TAB visualization
   color		toggle color faces
   mark			toggle visual mark
   whitespace-chars	restore `whitespace-chars' value
   whitespace-style	restore `whitespace-style' value

Only useful with a windowing system."
  (interactive (whitespace-interactive-char t))
  (let ((whitespace-chars
	 (whitespace-toggle-list
	  t arg whitespace-active-chars whitespace-chars
	  'whitespace-chars whitespace-chars-value-list))
	(whitespace-style
	 (whitespace-toggle-list
	  t arg whitespace-active-style whitespace-style
	  'whitespace-style whitespace-style-value-list)))
    (whitespace-mode 0)
    (whitespace-mode 1)))


(defvar whitespace-toggle-chars nil
  "Used to toggle the global `whitespace-chars' value.")
(defvar whitespace-toggle-style nil
  "Used to toggle the global `whitespace-style' value.")


;;;###autoload
(defun global-whitespace-toggle-options (arg)
  "Toggle global `whitespace-mode' options.

If global whitespace-mode is off, toggle the option given by ARG
and turn on global whitespace-mode.

If global whitespace-mode is on, toggle the option given by ARG
and restart global whitespace-mode.

Interactively, it accepts one of the following chars:

  CHAR	MEANING
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   b	toggle SPACEs before TAB visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   n	toggle NEWLINE visualization
   i	toggle indentation SPACEs visualization
   e	toggle empty line at bob and/or eob visualization
   a	toggle SPACEs after TAB visualization
   c	toggle color faces
   m	toggle visual mark
   x	restore `whitespace-chars' value
   z	restore `whitespace-style' value
   ?	display brief help

Non-interactively, ARG should be a symbol or a list of symbols.
The valid symbols are:

   tabs			toggle TAB visualization
   spaces		toggle SPACE and HARD SPACE visualization
   trailing		toggle trailing blanks visualization
   space-before-tab	toggle SPACEs before TAB visualization
   lines		toggle \"long lines\" visualization
   lines-tail		toggle \"long lines\" tail visualization
   newline		toggle NEWLINE visualization
   indentation		toggle indentation SPACEs visualization
   empty		toggle empty line at bob and/or eob visualization
   space-after-tab	toggle SPACEs after TAB visualization
   color		toggle color faces
   mark			toggle visual mark
   whitespace-chars	restore `whitespace-chars' value
   whitespace-style	restore `whitespace-style' value

Only useful with a windowing system."
  (interactive (whitespace-interactive-char nil))
  (let ((whitespace-chars
	 (whitespace-toggle-list
	  nil arg whitespace-toggle-chars whitespace-chars
	  'whitespace-chars whitespace-chars-value-list))
	(whitespace-style
	 (whitespace-toggle-list
	  nil arg whitespace-toggle-style whitespace-style
	  'whitespace-style whitespace-style-value-list)))
    (setq whitespace-toggle-chars whitespace-chars
	  whitespace-toggle-style whitespace-style)
    (global-whitespace-mode 0)
    (global-whitespace-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Cleanup


;;;###autoload
(defun whitespace-cleanup ()
  "Cleanup some blank problems in all buffer or at region.

It usually applies to the whole buffer, but in transient mark
mode when the mark is active, it applies to the region.  It also
applies to the region when it is not in transiente mark mode, the
mark is active and \\[universal-argument] was pressed just before calling
`whitespace-cleanup' interactively.

See also `whitespace-cleanup-region'.

The problems cleaned up are:

1. empty lines at beginning of buffer.
2. empty lines at end of buffer.
   If `whitespace-chars' includes the value `empty', remove all
   empty lines at beginning and/or end of buffer.

3. 8 or more SPACEs at beginning of line.
   If `whitespace-chars' includes the value `indentation', replace
   8 or more SPACEs at beginning of line by TABs.

4. SPACEs before TAB.
   If `whitespace-chars' includes the value `space-before-tab',
   replace SPACEs by TABs.

5. SPACEs or TABs at end of line.
   If `whitespace-chars' includes the value `trailing', remove all
   SPACEs or TABs at end of line.

6. 8 or more SPACEs after TAB.
   If `whitespace-chars' includes the value `space-after-tab',
   replace SPACEs by TABs."
  (interactive "@*")
  (if (and (or transient-mark-mode
	       current-prefix-arg)
	   mark-active)
      ;; region active
      ;; problems 1 and 2 are not handled in region
      ;; problem 3: 8 or more SPACEs at bol
      ;; problem 4: SPACEs before TAB
      ;; problem 5: SPACEs or TABs at eol
      ;; problem 6: 8 or more SPACEs after TAB
      (whitespace-cleanup-region (region-beginning) (region-end))
    ;; whole buffer
    (save-excursion
      (save-match-data
	;; problem 1: empty lines at bob
	;; problem 2: empty lines at eob
	;; action: remove all empty lines at bob and/or eob
	(when (memq 'empty whitespace-chars)
	  (let (overwrite-mode)		; enforce no overwrite
	    (goto-char (point-min))
	    (when (re-search-forward
		   whitespace-empty-at-bob-regexp nil t)
	      (delete-region (match-beginning 1) (match-end 1)))
	    (when (re-search-forward
		   whitespace-empty-at-eob-regexp nil t)
	      (delete-region (match-beginning 1) (match-end 1)))))))
    ;; problem 3: 8 or more SPACEs at bol
    ;; problem 4: SPACEs before TAB
    ;; problem 5: SPACEs or TABs at eol
    ;; problem 6: 8 or more SPACEs after TAB
    (whitespace-cleanup-region (point-min) (point-max))))


;;;###autoload
(defun whitespace-cleanup-region (start end)
  "Cleanup some blank problems at region.

The problems cleaned up are:

1. 8 or more SPACEs at beginning of line.
   If `whitespace-chars' includes the value `indentation', replace
   8 or more SPACEs at beginning of line by TABs.

2. SPACEs before TAB.
   If `whitespace-chars' includes the value `space-before-tab',
   replace SPACEs by TABs.

3. SPACEs or TABs at end of line.
   If `whitespace-chars' includes the value `trailing', remove all
   SPACEs or TABs at end of line.

4. 8 or more SPACEs after TAB.
   If `whitespace-chars' includes the value `space-after-tab',
   replace SPACEs by TABs."
  (interactive "@*r")
  (let ((rstart           (min start end))
	(rend             (copy-marker (max start end)))
	(tab-width        8)		; assure TAB width
	(indent-tabs-mode t)		; always insert TABs
	overwrite-mode			; enforce no overwrite
	tmp)
    (save-excursion
      (save-match-data
	;; problem 1: 8 or more SPACEs at bol
	;; action: replace 8 or more SPACEs at bol by TABs
	(when (memq 'indentation whitespace-chars)
	  (goto-char rstart)
	  (while (re-search-forward
		  whitespace-indentation-regexp rend t)
	    (setq tmp (current-indentation))
	    (delete-horizontal-space)
	    (unless (eolp)
	      (indent-to tmp))))
	;; problem 3: SPACEs or TABs at eol
	;; action: remove all SPACEs or TABs at eol
	(when (memq 'trailing whitespace-chars)
	  (let ((regexp (concat "\\(\\(" whitespace-trailing-regexp
				"\\)+\\)$")))
	    (goto-char rstart)
	    (while (re-search-forward regexp rend t)
	      (delete-region (match-beginning 1) (match-end 1)))))
	;; problem 4: 8 or more SPACEs after TAB
	;; action: replace 8 or more SPACEs by TABs
	(when (memq 'space-after-tab whitespace-chars)
	  (whitespace-replace-spaces-by-tabs
	   rstart rend whitespace-space-after-tab-regexp))
	;; problem 2: SPACEs before TAB
	;; action: replace SPACEs before TAB by TABs
	(when (memq 'space-before-tab whitespace-chars)
	  (whitespace-replace-spaces-by-tabs
	   rstart rend whitespace-space-before-tab-regexp))))
    (set-marker rend nil)))		; point marker to nowhere


(defun whitespace-replace-spaces-by-tabs (rstart rend regexp)
  "Replace all SPACEs by TABs matched by REGEXP between RSTART and REND."
  (goto-char rstart)
  (while (re-search-forward regexp rend t)
    (goto-char (match-beginning 1))
    (let* ((scol (current-column))
	   (ecol (save-excursion
		   (goto-char (match-end 1))
		   (current-column))))
      (delete-region (match-beginning 1) (match-end 1))
      (insert-char ?\t
		   (/ (- (- ecol (% ecol 8))  ; prev end col
			 (- scol (% scol 8))) ; prev start col
		      8)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User command - old whitespace compatibility


;;;###autoload
(defun whitespace-buffer ()
  "Turn on `whitespace-mode' forcing some settings.

It forces `whitespace-style' to have `color'.

It also forces `whitespace-chars' to have:

   trailing
   indentation
   space-before-tab
   empty
   space-after-tab

So, it is possible to visualize the following problems:

   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   indentation		3. 8 or more SPACEs at beginning of line.
   space-before-tab	4. SPACEs before TAB.
   trailing		5. SPACEs or TABs at end of line.
   space-after-tab	6. 8 or more SPACEs after TAB.

See `whitespace-chars' and `whitespace-style' for documentation.
See also `whitespace-cleanup' and `whitespace-cleanup-region' for
cleaning up these problems."
  (interactive)
  (whitespace-mode 0)			; assure is off
  ;; keep original values
  (let ((whitespace-style (copy-sequence whitespace-style))
	(whitespace-chars (copy-sequence whitespace-chars)))
    ;; adjust options for whitespace bogus blanks
    (add-to-list 'whitespace-style 'color)
    (mapc #'(lambda (option)
	      (add-to-list 'whitespace-chars option))
	  '(trailing
	    indentation
	    space-before-tab
	    empty
	    space-after-tab))
    (whitespace-mode 1)))		; turn on


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal functions


(defvar whitespace-font-lock-mode nil
  "Used to remember whether a buffer had font lock mode on or not.")
(make-variable-buffer-local 'whitespace-font-lock-mode)

(defvar whitespace-font-lock nil
  "Used to remember whether a buffer initially had font lock on or not.")
(make-variable-buffer-local 'whitespace-font-lock)

(defvar whitespace-font-lock-keywords nil
  "Used to save locally `font-lock-keywords' value.")
(make-variable-buffer-local 'whitespace-font-lock-keywords)


(defconst whitespace-help-text
  "\
      whitespace-mode toggle options:

 []  t - toggle TAB visualization
 []  s - toggle SPACE and HARD SPACE visualization
 []  r - toggle trailing blanks visualization
 []  b - toggle SPACEs before TAB visualization
 []  l - toggle \"long lines\" visualization
 []  L - toggle \"long lines\" tail visualization
 []  n - toggle NEWLINE visualization
 []  i - toggle indentation SPACEs visualization
 []  e - toggle empty line at bob and/or eob visualization
 []  a - toggle SPACEs after TAB visualization

 []  c - toggle color faces
 []  m - toggle visual mark

      x - restore `whitespace-chars' value
      z - restore `whitespace-style' value

      ? - display this text\n\n"
  "Text for whitespace toggle options.")


(defconst whitespace-help-buffer-name "*Whitespace Toggle Options*"
  "The buffer name for whitespace toggle options.")


(defun whitespace-insert-option-mark (the-list the-value)
  "Insert the option mark ('X' or ' ') in toggle options buffer."
  (forward-line 1)
  (dolist (sym  the-list)
    (forward-line 1)
    (forward-char 2)
    (insert (if (memq sym the-value) "X" " "))))


(defun whitespace-help-on (chars style)
  "Display the whitespace toggle options."
  (unless (get-buffer whitespace-help-buffer-name)
    (delete-other-windows)
    (let ((buffer (get-buffer-create whitespace-help-buffer-name)))
      (save-excursion
	(set-buffer buffer)
	(erase-buffer)
	(insert whitespace-help-text)
	(goto-char (point-min))
	(whitespace-insert-option-mark
	 whitespace-chars-value-list chars)
	(whitespace-insert-option-mark
	 whitespace-style-value-list style)
	(goto-char (point-min))
	(set-buffer-modified-p nil)
	(let ((size (- (window-height)
		       (max window-min-height
			    (1+ (count-lines (point-min)
					     (point-max)))))))
	  (when (<= size 0)
	    (kill-buffer buffer)
	    (error "Frame height is too small; \
can't split window to display whitespace toggle options"))
	  (set-window-buffer (split-window nil size) buffer))))))


(defun whitespace-help-off ()
  "Remove the buffer and window of the whitespace toggle options."
  (let ((buffer (get-buffer whitespace-help-buffer-name)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))


(defun whitespace-interactive-char (local-p)
  "Interactive function to read a char and return a symbol.

If LOCAL-P is non-nil, it uses a local context; otherwise, it
uses a global context.

It accepts one of the following chars:

  CHAR	MEANING
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   b	toggle SPACEs before TAB visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   n	toggle NEWLINE visualization
   i	toggle indentation SPACEs visualization
   e	toggle empty line at bob and/or eob visualization
   a	toggle SPACEs after TAB visualization
   c	toggle color faces
   m	toggle visual mark
   x	restore `whitespace-chars' value
   z	restore `whitespace-style' value
   ?	display brief help

See also `whitespace-toggle-option-alist'."
  (let* ((is-off (not (if local-p
			  whitespace-mode
			global-whitespace-mode)))
	 (chars  (cond (is-off  whitespace-chars) ; use default value
		       (local-p whitespace-active-chars)
		       (t       whitespace-toggle-chars)))
	 (style  (cond (is-off  whitespace-style) ; use default value
		       (local-p whitespace-active-style)
		       (t       whitespace-toggle-style)))
	 (prompt
	  (format "Whitespace Toggle %s (type ? for further options)-"
		  (if local-p "Local" "Global")))
	 ch sym)
    ;; read a valid option and get the corresponding symbol
    (save-window-excursion
      (condition-case data
	  (progn
	    (while
		;; while condition
		(progn
		  (setq ch (read-char prompt))
		  (not
		   (setq sym
			 (cdr
			  (assq ch whitespace-toggle-option-alist)))))
	      ;; while body
	      (if (eq ch ?\?)
		  (whitespace-help-on chars style)
		(ding)))
	    (whitespace-help-off)
	    (message " "))		; clean echo area
	;; handler
	((quit error)
	 (whitespace-help-off)
	 (error (error-message-string data)))))
    (list sym)))			; return the apropriate symbol


(defun whitespace-toggle-list (local-p arg the-list default-list
				       sym-restore sym-list)
  "Toggle options in THE-LIST based on list ARG.

If LOCAL-P is non-nil, it uses a local context; otherwise, it
uses a global context.

ARG is a list of options to be toggled.

THE-LIST is a list of options.  This list will be toggled and the
resultant list will be returned.

DEFAULT-LIST is the default list of options.  It is used to
restore the options in THE-LIST.

SYM-RESTORE is the symbol which indicates to restore the options
in THE-LIST.

SYM-LIST is a list of valid options, used to check if the ARG's
options are valid."
  (unless (if local-p whitespace-mode global-whitespace-mode)
    (setq the-list default-list))
  (setq the-list (copy-sequence the-list)) ; keep original list
  (dolist (sym (if (listp arg) arg (list arg)))
    (cond
     ;; restore default values
     ((eq sym sym-restore)
      (setq the-list default-list))
     ;; toggle valid values
     ((memq sym sym-list)
      (setq the-list (if (memq sym the-list)
			 (delq sym the-list)
		       (cons sym the-list))))))
  the-list)


(defun whitespace-turn-on ()
  "Turn on whitespace visualization."
  (setq whitespace-active-style (if (listp whitespace-style)
				    whitespace-style
				  (list whitespace-style)))
  (setq whitespace-active-chars (if (listp whitespace-chars)
				    whitespace-chars
				  (list whitespace-chars)))
  (when (memq 'color whitespace-active-style)
    (whitespace-color-on))
  (when (memq 'mark  whitespace-active-style)
    (whitespace-display-char-on)))


(defun whitespace-turn-off ()
  "Turn off whitespace visualization."
  (when (memq 'color whitespace-active-style)
    (whitespace-color-off))
  (when (memq 'mark  whitespace-active-style)
    (whitespace-display-char-off)))


(defun whitespace-color-on ()
  "Turn on color visualization."
  (when whitespace-active-chars
    (unless whitespace-font-lock
      (setq whitespace-font-lock t
	    whitespace-font-lock-keywords
	    (copy-sequence font-lock-keywords)))
    ;; turn off font lock
    (setq whitespace-font-lock-mode font-lock-mode)
    (font-lock-mode 0)
    ;; add whitespace-mode color into font lock
    (when (memq 'spaces whitespace-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs
	(list whitespace-space-regexp  1 whitespace-space  t)
	;; Show HARD SPACEs
	(list whitespace-hspace-regexp 1 whitespace-hspace t))
       t))
    (when (memq 'tabs whitespace-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show TABs
	(list whitespace-tab-regexp 1 whitespace-tab t))
       t))
    (when (memq 'trailing whitespace-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show trailing blanks
	(list (concat "\\(\\(" whitespace-trailing-regexp "\\)+\\)$")
	      1 whitespace-trailing t))
       t))
    (when (or (memq 'lines      whitespace-active-chars)
	      (memq 'lines-tail whitespace-active-chars))
      (font-lock-add-keywords
       nil
       (list
	;; Show "long" lines
	(list
	 (format
	  "^\\([^\t\n]\\{%s\\}\\|[^\t\n]\\{0,%s\\}\t\\)\\{%d\\}%s\\(.+\\)$"
	  tab-width (1- tab-width)
	  (/ whitespace-line-column tab-width)
	  (let ((rem (% whitespace-line-column tab-width)))
	    (if (zerop rem)
		""
	      (format ".\\{%d\\}" rem))))
	 (if (memq 'lines whitespace-active-chars)
	     0				; whole line
	   2)				; line tail
	 whitespace-line t))
       t))
    (when (memq 'space-before-tab whitespace-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs before TAB
	(list whitespace-space-before-tab-regexp
	      1 whitespace-space-before-tab t))
       t))
    (when (memq 'indentation whitespace-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show indentation SPACEs
	(list whitespace-indentation-regexp
	      1 whitespace-indentation t))
       t))
    (when (memq 'empty whitespace-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show empty lines at beginning of buffer
	(list whitespace-empty-at-bob-regexp
	      1 whitespace-empty t))
       t)
      (font-lock-add-keywords
       nil
       (list
	;; Show empty lines at end of buffer
	(list whitespace-empty-at-eob-regexp
	      1 whitespace-empty t))
       t))
    (when (memq 'space-after-tab whitespace-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs after TAB
	(list whitespace-space-after-tab-regexp
	      1 whitespace-space-after-tab t))
       t))
    ;; now turn on font lock and highlight blanks
    (font-lock-mode 1)))


(defun whitespace-color-off ()
  "Turn off color visualization."
  (when whitespace-active-chars
    ;; turn off font lock
    (font-lock-mode 0)
    (when whitespace-font-lock
      (setq whitespace-font-lock nil
	    font-lock-keywords   whitespace-font-lock-keywords))
    ;; restore original font lock state
    (font-lock-mode whitespace-font-lock-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hacked from visws.el (Miles Bader <miles@gnu.org>)


(defvar whitespace-display-table nil
  "Used to save a local display table.")
(make-variable-buffer-local 'whitespace-display-table)

(defvar whitespace-display-table-was-local nil
  "Used to remember whether a buffer initially had a local display table.")
(make-variable-buffer-local 'whitespace-display-table-was-local)


(defsubst whitespace-char-valid-p (char)
  ;; This check should be improved!!!
  (or (< char 256)
      (characterp char)))


(defun whitespace-display-vector-p (vec)
  "Return true if every character in vector VEC can be displayed."
  (let ((i (length vec)))
    (when (> i 0)
      (while (and (>= (setq i (1- i)) 0)
		  (whitespace-char-valid-p (aref vec i))))
      (< i 0))))


(defun whitespace-display-char-on ()
  "Turn on character display mapping."
  (when whitespace-display-mappings
    (let (vecs vec)
      ;; Remember whether a buffer has a local display table.
      (unless whitespace-display-table-was-local
	(setq whitespace-display-table-was-local t
	      whitespace-display-table
	      (copy-sequence buffer-display-table)))
      (unless buffer-display-table
	(setq buffer-display-table (make-display-table)))
      (dolist (entry whitespace-display-mappings)
	(setq vecs (cdr entry))
	;; Get a displayable mapping.
	(while (and vecs
		    (not (whitespace-display-vector-p (car vecs))))
	  (setq vecs (cdr vecs)))
	;; Display a valid mapping.
	(when vecs
	  (setq vec (copy-sequence (car vecs)))
	  (cond
	   ;; Any char except newline
	   ((not (eq (car entry) ?\n))
	    (aset buffer-display-table (car entry) vec))
	   ;; Newline char - display it
	   ((memq 'newline whitespace-active-chars)
	    ;; Only insert face bits on NEWLINE char mapping to avoid
	    ;; obstruction of other faces like TABs and (HARD) SPACEs
	    ;; faces, font-lock faces, etc.
	    (when (memq 'color whitespace-active-style)
	      (dotimes (i (length vec))
		;; Due to limitations of glyph representation, the char
		;; code can not be above ?\x1FFFF.  Probably, this will
		;; be fixed after Emacs unicode merging.
		(or (eq (aref vec i) ?\n)
		    (> (aref vec i) #x1FFFF)
		    (aset vec i
			  (make-glyph-code (aref vec i)
					   whitespace-newline)))))
	    ;; Display mapping
	    (aset buffer-display-table (car entry) vec))
	   ;; Newline char - don't display it
	   (t
	    ;; Do nothing
	    )))))))


(defun whitespace-display-char-off ()
  "Turn off character display mapping."
  (and whitespace-display-mappings
       whitespace-display-table-was-local
       (setq whitespace-display-table-was-local nil
	     buffer-display-table whitespace-display-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun whitespace-unload-function ()
  "Unload the Whitespace library."
  (let (whitespace-mode) ;; so g-w-m thinks it is nil in all buffers
    (global-whitespace-mode -1))
  ;; continue standard unloading
  nil)

(provide 'whitespace)


(run-hooks 'whitespace-load-hook)


;; arch-tag: 1b1e2500-dbd4-4a26-8f7a-5a5edfd3c97e
;;; whitespace.el ends here
