;;; blank-mode.el --- minor mode to visualize TAB, (HARD) SPACE, NEWLINE

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: data, wp
;; Version: 8.0
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
;; blank-mode uses two ways to visualize blanks: faces and display
;; table.
;;
;; * Faces are used to highlight the background with a color.
;;   blank-mode uses font-lock to highlight blank characters.
;;
;; * Display table changes the way a character is displayed, that is,
;;   it provides a visual mark for characters, for example, at the end
;;   of line (?\xB6), at SPACEs (?\xB7) and at TABs (?\xBB).
;;
;; The `blank-style' and `blank-chars' variables are used to select
;; which way should be used to visualize blanks.
;;
;; Note that when blank-mode is turned on, blank-mode saves the
;; font-lock state, that is, if font-lock is on or off.  And
;; blank-mode restores the font-lock state when it is turned off.  So,
;; if blank-mode is turned on and font-lock is off, blank-mode also
;; turns on the font-lock to highlight blanks, but the font-lock will
;; be turned off when blank-mode is turned off.  Thus, turn on
;; font-lock before blank-mode is on, if you want that font-lock
;; continues on after blank-mode is turned off.
;;
;; When blank-mode is on, it takes care of highlighting some special
;; characters over the default mechanism of `nobreak-char-display'
;; (which see) and `show-trailing-whitespace' (which see).
;;
;; There are two ways of using blank-mode: local and global.
;;
;; * Local blank-mode affects only the current buffer.
;;
;; * Global blank-mode affects all current and future buffers.  That
;;   is, if you turn on global blank-mode and then create a new
;;   buffer, the new buffer will also have blank-mode on.  The
;;   `blank-global-modes' variable controls which major-mode will be
;;   automagically turned on.
;;
;; You can mix the local and global usage without any conflict.  But
;; local blank-mode has priority over global blank-mode.  Blank mode
;; is active in a buffer if you have enabled it in that buffer or if
;; you have enabled it globally.
;;
;; When global and local blank-mode are on:
;;
;; * if local blank-mode is turned off, blank-mode is turned off for
;;   the current buffer only.
;;
;; * if global blank-mode is turned off, blank-mode continues on only
;;   in the buffers in which local blank-mode is on.
;;
;; To use blank-mode, insert in your ~/.emacs:
;;
;;    (require 'blank-mode)
;;
;; Or autoload at least one of the commands`blank-mode',
;; `blank-toggle-options', `global-blank-mode' or
;; `global-blank-toggle-options'.  For example:
;;
;;    (autoload 'blank-mode                  "blank-mode"
;;      "Toggle blank visualization."          t)
;;    (autoload 'blank-toggle-options        "blank-mode"
;;      "Toggle local `blank-mode' options."   t)
;;
;; blank-mode was inspired by:
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
;; Using blank-mode
;; ----------------
;;
;; There is no problem if you mix local and global minor mode usage.
;;
;; * LOCAL blank-mode:
;;    + To toggle blank-mode options locally, type:
;;
;;         M-x blank-toggle-options RET
;;
;;    + To activate blank-mode locally, type:
;;
;;         C-u 1 M-x blank-mode RET
;;
;;    + To deactivate blank-mode locally, type:
;;
;;         C-u 0 M-x blank-mode RET
;;
;;    + To toggle blank-mode locally, type:
;;
;;         M-x blank-mode RET
;;
;; * GLOBAL blank-mode:
;;    + To toggle blank-mode options globally, type:
;;
;;         M-x global-blank-toggle-options RET
;;
;;    + To activate blank-mode globally, type:
;;
;;         C-u 1 M-x global-blank-mode RET
;;
;;    + To deactivate blank-mode globally, type:
;;
;;         C-u 0 M-x global-blank-mode RET
;;
;;    + To toggle blank-mode globally, type:
;;
;;         M-x global-blank-mode RET
;;
;;
;; Hooks
;; -----
;;
;; blank-mode has the following hook variables:
;;
;; `blank-mode-hook'
;;    It is evaluated always when blank-mode is turned on locally.
;;
;; `global-blank-mode-hook'
;;    It is evaluated always when blank-mode is turned on globally.
;;
;; `blank-load-hook'
;;    It is evaluated after blank-mode package is loaded.
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of blank-mode options, please,
;; see the options declaration in the code for a long documentation.
;;
;; `blank-style'		Specify the visualization style.
;;
;; `blank-chars'		Specify which kind of blank is
;;				visualized.
;;
;; `blank-space-face'		Face used to visualize SPACE.
;;
;; `blank-hspace-face'		Face used to visualize HARD SPACE.
;;
;; `blank-tab-face'		Face used to visualize TAB.
;;
;; `blank-newline-face'		Face used to visualize NEWLINE char
;;				mapping.
;;
;; `blank-trailing-face'	Face used to visualize trailing
;;				blanks.
;;
;; `blank-line-face'		Face used to visualize "long" lines.
;;
;; `blank-space-before-tab-face'	Face used to visualize SPACEs
;;					before TAB.
;;
;; `blank-space-regexp'		Specify SPACE characters regexp.
;;
;; `blank-hspace-regexp'	Specify HARD SPACE characters regexp.
;;
;; `blank-tab-regexp'		Specify TAB characters regexp.
;;
;; `blank-trailing-regexp'	Specify trailing characters regexp.
;;
;; `blank-space-before-tab-regexp'	Specify SPACEs before TAB
;;					regexp.
;;
;; `blank-line-length'		Specify length beyond which the line
;;				is highlighted.
;;
;; `blank-display-mappings'	Specify an alist of mappings for
;;				displaying characters.
;;
;; `blank-global-modes'		Modes for which global `blank-mode' is
;;				automagically turned on.
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Juri Linkov <juri@jurta.org> for suggesting:
;;    * `define-minor-mode'.
;;    * `global-blank-*' name for global commands.
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
;; "long" lines. See EightyColumnRule (EmacsWiki).
;;
;; Thanks to Yanghui Bian <yanghuibian@gmail.com> for indicating a new
;; newline character mapping.
;;
;; Thanks to Pete Forman <pete.forman@westgeo.com> for indicating
;; whitespace-mode on XEmacs.
;;
;; Thanks to Miles Bader <miles@gnu.org> for handling display table via
;; visws.el (his code was modified, but the main idea was kept).
;;
;; Thanks to:
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


(defgroup blank nil
  "Visualize blanks (TAB, (HARD) SPACE and NEWLINE)."
  :link '(emacs-library-link :tag "Source Lisp File" "blank-mode.el")
  :version "22.2"
  :group 'wp
  :group 'data)


(defcustom blank-style '(mark color)
  "*Specify the visualization style.

It's a list which element value can be:

   mark		display mappings are visualized.

   color	faces are visualized.

Any other value is ignored.

If nil, don't visualize TABs, (HARD) SPACEs and NEWLINEs.

See also `blank-display-mappings' for documentation."
  :type '(repeat :tag "Style of Blank"
		 (choice :tag "Style of Blank"
			 (const :tag "Display Table" mark)
			 (const :tag "Faces" color)))
  :group 'blank)


(defcustom blank-chars
  '(tabs spaces trailing lines space-before-tab newline)
  "*Specify which kind of blank is visualized.

It's a list which element value can be:

   trailing		trailing blanks are visualized.

   tabs			TABs are visualized.

   spaces		SPACEs and HARD SPACEs are visualized.

   lines		lines whose length is greater than
			`blank-line-length' are highlighted.

   space-before-tab	SPACEs before TAB are visualized.

   newline		NEWLINEs are visualized.

Any other value is ignored.

If nil, don't visualize TABs, (HARD) SPACEs and NEWLINEs.

Used when `blank-style' has `color' as an element.
If `blank-chars' has `newline' as an element, used when `blank-style'
has `mark' as an element."
  :type '(repeat :tag "Kind of Blank"
		 (choice :tag "Kind of Blank"
			 (const :tag "Trailing TABs, SPACEs and HARD SPACEs"
				trailing)
			 (const :tag "SPACEs and HARD SPACEs" spaces)
			 (const :tag "TABs" tabs)
			 (const :tag "Lines" lines)
			 (const :tag "SPACEs before TAB"
				space-before-tab)
			 (const :tag "NEWLINEs" newline)))
  :group 'blank)


(defcustom blank-space-face 'blank-space-face
  "*Symbol face used to visualize SPACE.

Used when `blank-style' has `color' as an element."
  :type 'face
  :group 'blank)


(defface blank-space-face
  '((((class color) (background dark))
     (:background "grey20"      :foreground "aquamarine3"))
    (((class color) (background light))
     (:background "LightYellow" :foreground "aquamarine3"))
    (t (:inverse-video t)))
  "Face used to visualize SPACE."
  :group 'blank)


(defcustom blank-hspace-face 'blank-hspace-face
  "*Symbol face used to visualize HARD SPACE.

Used when `blank-style' has `color' as an element."
  :type 'face
  :group 'blank)


(defface blank-hspace-face		; 'nobreak-space
  '((((class color) (background dark))
     (:background "grey24"        :foreground "aquamarine3"))
    (((class color) (background light))
     (:background "LemonChiffon3" :foreground "aquamarine3"))
    (t (:inverse-video t)))
  "Face used to visualize HARD SPACE."
  :group 'blank)


(defcustom blank-tab-face 'blank-tab-face
  "*Symbol face used to visualize TAB.

Used when `blank-style' has `color' as an element."
  :type 'face
  :group 'blank)


(defface blank-tab-face
  '((((class color) (background dark))
     (:background "grey22" :foreground "aquamarine3"))
    (((class color) (background light))
     (:background "beige"  :foreground "aquamarine3"))
    (t (:inverse-video t)))
  "Face used to visualize TAB."
  :group 'blank)


(defcustom blank-newline-face 'blank-newline-face
  "*Symbol face used to visualize NEWLINE char mapping.

See `blank-display-mappings'.

Used when `blank-style' has `mark' and `color' as elements
and `blank-chars' has `newline' as an element."
  :type 'face
  :group 'blank)


(defface blank-newline-face
  '((((class color) (background dark))
     (:background "grey26" :foreground "aquamarine3" :bold t))
    (((class color) (background light))
     (:background "linen"  :foreground "aquamarine3" :bold t))
    (t (:bold t :underline t)))
  "Face used to visualize NEWLINE char mapping.

See `blank-display-mappings'."
  :group 'blank)


(defcustom blank-trailing-face 'blank-trailing-face
  "*Symbol face used to visualize traling blanks.

Used when `blank-style' has `color' as an element."
  :type 'face
  :group 'blank)


(defface blank-trailing-face		; 'trailing-whitespace
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "red1" :foreground "yellow" :bold t)))
  "Face used to visualize trailing blanks."
  :group 'blank)


(defcustom blank-line-face 'blank-line-face
  "*Symbol face used to visualize \"long\" lines.

See `blank-line-length'.

Used when `blank-style' has `color' as an element."
  :type 'face
  :group 'blank)


(defface blank-line-face
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "gray20" :foreground "violet")))
  "Face used to visualize \"long\" lines.

See `blank-line-length'."
  :group 'blank)


(defcustom blank-space-before-tab-face 'blank-space-before-tab-face
  "*Symbol face used to visualize SPACEs before TAB.

Used when `blank-style' has `color' as an element."
  :type 'face
  :group 'blank)


(defface blank-space-before-tab-face
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "DarkOrange" :foreground "firebrick")))
  "Face used to visualize SPACEs before TAB."
  :group 'blank)


(defcustom blank-hspace-regexp
  "\\(\\(\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\)+\\)"
  "*Specify HARD SPACE characters regexp.

If you're using `mule' package, it may exist other characters besides:

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

Used when `blank-style' has `color' as an element, and
`blank-chars' has `spaces' as an element."
  :type '(regexp :tag "HARD SPACE Chars")
  :group 'blank)


(defcustom blank-space-regexp "\\( +\\)"
  "*Specify SPACE characters regexp.

If you're using `mule' package, it may exist other characters
besides \" \" that should be considered SPACE.

Here are some examples:

   \"\\\\(^ +\\\\)\"		visualize only leading SPACEs.
   \"\\\\( +$\\\\)\"		visualize only trailing SPACEs.
   \"\\\\(^ +\\\\| +$\\\\)\"	\
visualize leading and/or trailing SPACEs.
   \"\\t\\\\( +\\\\)\\t\"	visualize only SPACEs between TABs.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `blank-style' has `color' as an element, and
`blank-chars' has `spaces' as an element."
  :type '(regexp :tag "SPACE Chars")
  :group 'blank)


(defcustom blank-tab-regexp "\\(\t+\\)"
  "*Specify TAB characters regexp.

If you're using `mule' package, it may exist other characters
besides \"\\t\" that should be considered TAB.

Here are some examples:

   \"\\\\(^\\t+\\\\)\"		visualize only leading TABs.
   \"\\\\(\\t+$\\\\)\"		visualize only trailing TABs.
   \"\\\\(^\\t+\\\\|\\t+$\\\\)\"	\
visualize leading and/or trailing TABs.
   \" \\\\(\\t+\\\\) \"	visualize only TABs between SPACEs.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `blank-style' has `color' as an element, and
`blank-chars' has `tabs' as an element."
  :type '(regexp :tag "TAB Chars")
  :group 'blank)


(defcustom blank-trailing-regexp
  "\t\\| \\|\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20"
  "*Specify trailing characters regexp.

If you're using `mule' package, it may exist other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

NOTE: DO NOT enclose by \\\\( and \\\\) the elements to highlight.
      `blank-mode' surrounds this regexp by \"\\\\(\\\\(\" and
      \"\\\\)+\\\\)$\".

Used when `blank-style' has `color' as an element, and
`blank-chars' has `trailing' as an element."
  :type '(regexp :tag "Trailing Chars")
  :group 'blank)


(defcustom blank-space-before-tab-regexp "\\( +\\)\t"
  "*Specify SPACEs before TAB regexp.

If you're using `mule' package, it may exist other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `blank-style' has `color' as an element, and
`blank-chars' has `space-before-tab' as an element."
  :type '(regexp :tag "SPACEs Before TAB")
  :group 'blank)


(defcustom blank-line-length 80
  "*Specify length beyond which the line is highlighted.

Used when `blank-style' has `color' as an element, and
`blank-chars' has `lines' as an element."
  :type '(integer :tag "Line Length")
  :group 'blank)


;; Hacked from `visible-whitespace-mappings' in visws.el
(defcustom blank-display-mappings
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
    ;; NEWLINE is displayed using the face `blank-newline-face'
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
`blank-newline-face' variable.  The characters in the vector to be
displayed will not have this face applied if the character code is
above #x1FFFF.

Used when `blank-style' has `mark' as an element."
  :type '(repeat
	  (list :tag "Character Mapping"
		(character :tag "Char")
		(repeat :inline t :tag "Vector List"
			(vector :tag ""
				(repeat :inline t
					:tag "Vector Characters"
					(character :tag "Char"))))))
  :group 'blank)


(defcustom blank-global-modes t
  "*Modes for which global `blank-mode' is automagically turned on.

Global `blank-mode' is controlled by the command `global-blank-mode'.

If nil, means no modes have `blank-mode' automatically turned on.
If t, all modes that support `blank-mode' have it automatically
turned on.
Else it should be a list of `major-mode' symbol names for
which `blank-mode' should be automatically turned on.  The sense
of the list is negated if it begins with `not'.  For example:

   (c-mode c++-mode)

means that `blank-mode' is turned on for buffers in C and C++
modes only."
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (set :menu-tag "mode specific" :tag "modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t
			      (symbol :tag "mode"))))
  :group 'blank)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Local mode


;;;###autoload
(define-minor-mode blank-mode
  "Toggle blank minor mode visualization (\"bl\" on modeline).

If ARG is null, toggle blank visualization.
If ARG is a number greater than zero, turn on visualization;
otherwise, turn off visualization.
Only useful with a windowing system."
  :lighter    " bl"
  :init-value nil
  :global     nil
  :group      'blank
  (cond
   (noninteractive			; running a batch job
    (setq blank-mode nil))
   (blank-mode				; blank-mode on
    (blank-turn-on))
   (t					; blank-mode off
    (blank-turn-off))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Global mode


(define-minor-mode global-blank-mode
  "Toggle blank global minor mode visualization (\"BL\" on modeline).

If ARG is null, toggle blank visualization.
If ARG is a number greater than zero, turn on visualization;
otherwise, turn off visualization.
Only useful with a windowing system."
  :lighter    " BL"
  :init-value nil
  :global     t
  :group      'blank
  (cond
   (noninteractive			; running a batch job
    (setq global-blank-mode nil))
   (global-blank-mode			; global-blank-mode on
    (save-excursion
      (if (boundp 'find-file-hook)
	  (add-hook 'find-file-hook 'blank-turn-on-if-enabled t)
	(add-hook 'find-file-hooks 'blank-turn-on-if-enabled t))
      (dolist (buffer (buffer-list))	; adjust all local mode
	(set-buffer buffer)
	(unless blank-mode
	  (blank-turn-on-if-enabled)))))
   (t					; global-blank-mode off
    (save-excursion
      (if (boundp 'find-file-hook)
	  (remove-hook 'find-file-hook 'blank-turn-on-if-enabled)
	(remove-hook 'find-file-hooks 'blank-turn-on-if-enabled))
      (dolist (buffer (buffer-list))	; adjust all local mode
	(set-buffer buffer)
	(unless blank-mode
	  (blank-turn-off)))))))


(defun blank-turn-on-if-enabled ()
  (when (cond
	 ((eq blank-global-modes t))
	 ((listp blank-global-modes)
	  (if (eq (car-safe blank-global-modes) 'not)
	      (not (memq major-mode (cdr blank-global-modes)))
	    (memq major-mode blank-global-modes)))
	 (t nil))
    (let (inhibit-quit)
      ;; Don't turn on blank mode if...
      (or
       ;; ...we don't have a display (we're running a batch job)
       noninteractive
       ;; ...or if the buffer is invisible (name starts with a space)
       (eq (aref (buffer-name) 0) ?\ )
       ;; ...or if the buffer is temporary (name starts with *)
       (and (eq (aref (buffer-name) 0) ?*)
	    ;; except the scratch buffer.
	    (not (string= (buffer-name) "*scratch*")))
       ;; Otherwise, turn on blank mode.
       (blank-turn-on)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Toggle


(defconst blank-chars-value-list
  '(tabs
    spaces
    trailing
    space-before-tab
    lines
    newline
    )
  "List of valid `blank-chars' values.")


(defconst blank-style-value-list
  '(color
    mark
    )
  "List of valid `blank-style' values.")


(defconst blank-toggle-option-alist
  '((?t . tabs)
    (?s . spaces)
    (?r . trailing)
    (?b . space-before-tab)
    (?l . lines)
    (?n . newline)
    (?c . color)
    (?m . mark)
    (?x . blank-chars)
    (?z . blank-style)
    )
  "Alist of toggle options.

Each element has the form:

   (CHAR . SYMBOL)

Where:

CHAR	is a char which the user will have to type.

SYMBOL	is a valid symbol associated with CHAR.
	See `blank-chars-value-list' and `blank-style-value-list'.")


(defvar blank-active-chars nil
  "Used to save locally `blank-chars' value.")
(make-variable-buffer-local 'blank-active-chars)

(defvar blank-active-style nil
  "Used to save locally `blank-style' value.")
(make-variable-buffer-local 'blank-active-style)


;;;###autoload
(defun blank-toggle-options (arg)
  "Toggle local `blank-mode' options.

If local blank-mode is off, toggle the option given by ARG and
turn on local blank-mode.

If local blank-mode is on, toggle the option given by ARG and
restart local blank-mode.

Interactively, it reads one of the following chars:

  CHAR	MEANING
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   b	toggle SPACEs before TAB visualization
   l	toggle \"long lines\" visualization
   n	toggle NEWLINE visualization
   c	toggle color faces
   m	toggle visual mark
   x	restore `blank-chars' value
   z	restore `blank-style' value
   ?	display brief help

Non-interactively, ARG should be a symbol or a list of symbols.
The valid symbols are:

   tabs			toggle TAB visualization
   spaces		toggle SPACE and HARD SPACE visualization
   trailing		toggle trailing blanks visualization
   space-before-tab	toggle SPACEs before TAB visualization
   lines		toggle \"long lines\" visualization
   newline		toggle NEWLINE visualization
   color		toggle color faces
   mark			toggle visual mark
   blank-chars		restore `blank-chars' value
   blank-style		restore `blank-style' value

Only useful with a windowing system."
  (interactive (blank-interactive-char t))
  (let ((blank-chars
	 (blank-toggle-list t arg blank-active-chars blank-chars
			    'blank-chars blank-chars-value-list))
	(blank-style
	 (blank-toggle-list t arg blank-active-style blank-style
			    'blank-style blank-style-value-list)))
    (blank-mode 0)
    (blank-mode 1)))


(defvar blank-toggle-chars nil
  "Used to toggle the global `blank-chars' value.")
(defvar blank-toggle-style nil
  "Used to toggle the global `blank-style' value.")


;;;###autoload
(defun global-blank-toggle-options (arg)
  "Toggle global `blank-mode' options.

If global blank-mode is off, toggle the option given by ARG and
turn on global blank-mode.

If global blank-mode is on, toggle the option given by ARG and
restart global blank-mode.

Interactively, it reads one of the following chars:

  CHAR	MEANING
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   b	toggle SPACEs before TAB visualization
   l	toggle \"long lines\" visualization
   n	toggle NEWLINE visualization
   c	toggle color faces
   m	toggle visual mark
   x	restore `blank-chars' value
   z	restore `blank-style' value
   ?	display brief help

Non-interactively, ARG should be a symbol or a list of symbols.
The valid symbols are:

   tabs			toggle TAB visualization
   spaces		toggle SPACE and HARD SPACE visualization
   trailing		toggle trailing blanks visualization
   space-before-tab	toggle SPACEs before TAB visualization
   lines		toggle \"long lines\" visualization
   newline		toggle NEWLINE visualization
   color		toggle color faces
   mark			toggle visual mark
   blank-chars		restore `blank-chars' value
   blank-style		restore `blank-style' value

Only useful with a windowing system."
  (interactive (blank-interactive-char nil))
  (let ((blank-chars
	 (blank-toggle-list nil arg blank-toggle-chars blank-chars
			    'blank-chars blank-chars-value-list))
	(blank-style
	 (blank-toggle-list nil arg blank-toggle-style blank-style
			    'blank-style blank-style-value-list)))
    (setq blank-toggle-chars blank-chars
	  blank-toggle-style blank-style)
    (global-blank-mode 0)
    (global-blank-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal functions


(defvar blank-font-lock-mode nil
  "Used to remember whether a buffer had font lock mode on or not.")
(make-variable-buffer-local 'blank-font-lock-mode)

(defvar blank-font-lock nil
  "Used to remember whether a buffer initially had font lock on or not.")
(make-variable-buffer-local 'blank-font-lock)

(defvar blank-font-lock-keywords nil
  "Used to save locally `font-lock-keywords' value.")
(make-variable-buffer-local 'blank-font-lock-keywords)


(defconst blank-help-text
  "\
      blank-mode toggle options:

 []  t - toggle TAB visualization
 []  s - toggle SPACE and HARD SPACE visualization
 []  r - toggle trailing blanks visualization
 []  b - toggle SPACEs before TAB visualization
 []  l - toggle \"long lines\" visualization
 []  n - toggle NEWLINE visualization

 []  c - toggle color faces
 []  m - toggle visual mark

      x - restore `blank-chars' value
      z - restore `blank-style' value

      ? - display this text\n\n"
  "Text for blank toggle options.")


(defconst blank-help-buffer-name "*Blank Toggle Options*"
  "The buffer name for blank toggle options.")


(defun blank-insert-option-mark (the-list the-value)
  "Insert the option mark ('X' or ' ') in toggle options buffer."
  (forward-line 1)
  (dolist (sym  the-list)
    (forward-line 1)
    (forward-char 2)
    (insert (if (memq sym the-value) "X" " "))))


(defun blank-help-on (chars style)
  "Display the blank toggle options."
  (unless (get-buffer blank-help-buffer-name)
    (delete-other-windows)
    (let ((buffer (get-buffer-create blank-help-buffer-name)))
      (save-excursion
	(set-buffer buffer)
	(erase-buffer)
	(insert blank-help-text)
	(goto-char (point-min))
	(blank-insert-option-mark blank-chars-value-list chars)
	(blank-insert-option-mark blank-style-value-list style)
	(goto-char (point-min))
	(set-buffer-modified-p nil)
	(let ((size (- (window-height)
		       (max window-min-height
			    (1+ (count-lines (point-min) (point-max)))))))
	  (when (<= size 0)
	    (kill-buffer buffer)
	    (error "Frame height is too small; \
can't split window to display blank toggle options"))
	  (set-window-buffer (split-window nil size) buffer))))))


(defun blank-help-off ()
  "Remove the buffer and window of the blank toggle options."
  (let ((buffer (get-buffer blank-help-buffer-name)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))


(defun blank-interactive-char (local-p)
  "Interactive function to read a char and return a symbol.

If LOCAL-P is non-nil, it uses a local context; otherwise, it
uses a global context.

It reads one of the following chars:

  CHAR	MEANING
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   b	toggle SPACEs before TAB visualization
   l	toggle \"long lines\" visualization
   n	toggle NEWLINE visualization
   c	toggle color faces
   m	toggle visual mark
   x	restore `blank-chars' value
   z	restore `blank-style' value
   ?	display brief help

See also `blank-toggle-option-alist'."
  (let* ((is-off (not (if local-p blank-mode global-blank-mode)))
	 (chars  (cond (is-off  blank-chars) ; use default value
		       (local-p blank-active-chars)
		       (t       blank-toggle-chars)))
	 (style  (cond (is-off  blank-style) ; use default value
		       (local-p blank-active-style)
		       (t       blank-toggle-style)))
	 (prompt
	  (format "Blank Toggle %s (type ? for further options)-"
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
			 (cdr (assq ch blank-toggle-option-alist)))))
	      ;; while body
	      (if (eq ch ?\?)
		  (blank-help-on chars style)
		(ding)))
	    (blank-help-off)
	    (message " "))		; clean echo area
	;; handler
	((quit error)
	 (blank-help-off)
	 (error (error-message-string data)))))
    (list sym)))			; return the apropriate symbol


(defun blank-toggle-list (local-p arg the-list default-list
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
  (unless (if local-p blank-mode global-blank-mode)
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


(defun blank-turn-on ()
  "Turn on blank visualization."
  (setq blank-active-style (if (listp blank-style)
			       blank-style
			     (list blank-style)))
  (setq blank-active-chars (if (listp blank-chars)
			       blank-chars
			     (list blank-chars)))
  (and (memq 'color blank-active-style)
       (blank-color-on))
  (and (memq 'mark  blank-active-style)
       (blank-display-char-on)))


(defun blank-turn-off ()
  "Turn off blank visualization."
  (and (memq 'color blank-active-style)
       (blank-color-off))
  (and (memq 'mark  blank-active-style)
       (blank-display-char-off)))


(defun blank-color-on ()
  "Turn on color visualization."
  (when blank-active-chars
    (unless blank-font-lock
      (setq blank-font-lock t
	    blank-font-lock-keywords
	    (copy-sequence font-lock-keywords)))
    ;; turn off font lock
    (setq blank-font-lock-mode font-lock-mode)
    (font-lock-mode 0)
    ;; add blank-mode color into font lock
    (when (memq 'spaces blank-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs
	(list blank-space-regexp  1 blank-space-face  t)
	;; Show HARD SPACEs
	(list blank-hspace-regexp 1 blank-hspace-face t))
       t))
    (when (memq 'tabs blank-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show TABs
	(list blank-tab-regexp 1 blank-tab-face t))
       t))
    (when (memq 'trailing blank-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show trailing blanks
	(list (concat "\\(\\(" blank-trailing-regexp "\\)+\\)$")
	      1 blank-trailing-face t))
       t))
    (when (memq 'lines blank-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show "long" lines
	(list (concat "^\\(.\\{" (int-to-string blank-line-length)
		      ",\\}\\)$")
	      1 blank-line-face t))
       t))
    (when (memq 'space-before-tab blank-active-chars)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs before TAB
	(list blank-space-before-tab-regexp
	      1 blank-space-before-tab-face t))
       t))
    ;; now turn on font lock and highlight blanks
    (font-lock-mode 1)))


(defun blank-color-off ()
  "Turn off color visualization."
  (when blank-active-chars
    (when blank-font-lock
      (setq blank-font-lock nil
	    font-lock-keywords blank-font-lock-keywords))
    ;; turn off font lock
    (font-lock-mode 0)
    ;; restore original font lock state
    (font-lock-mode blank-font-lock-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hacked from visws.el (Miles Bader <miles@gnu.org>)


(defvar blank-display-table nil
  "Used to save a local display table.")
(make-variable-buffer-local 'blank-display-table)

(defvar blank-display-table-was-local nil
  "Used to remember whether a buffer initially had a local display table or not.")
(make-variable-buffer-local 'blank-display-table-was-local)


(defsubst blank-char-valid-p (char)
  ;; This check should be improved!!!
  (or (< char 256)
      (char-valid-p char)))


(defun blank-legal-display-vector-p (vec)
  "Return true if every character in vector VEC can be displayed."
  (let ((i (length vec)))
    (when (> i 0)
      (while (and (>= (setq i (1- i)) 0)
		  (blank-char-valid-p (aref vec i))))
      (< i 0))))


(defun blank-display-char-on ()
  "Turn on character display mapping."
  (when blank-display-mappings
    (let (vecs vec)
      ;; Remember whether a buffer has a local display table.
      (unless blank-display-table-was-local
	(setq blank-display-table-was-local t
	      blank-display-table
	      (copy-sequence buffer-display-table)))
      (unless buffer-display-table
	(setq buffer-display-table (make-display-table)))
      (dolist (entry blank-display-mappings)
	(setq vecs (cdr entry))
	;; Get a displayable mapping.
	(while (and vecs
		    (not (blank-legal-display-vector-p (car vecs))))
	  (setq vecs (cdr vecs)))
	;; Display a valid mapping.
	(when vecs
	  (setq vec (copy-sequence (car vecs)))
	  (cond
	   ;; Any char except newline
	   ((not (eq (car entry) ?\n))
	    (aset buffer-display-table (car entry) vec))
	   ;; Newline char - display it
	   ((memq 'newline blank-active-chars)
	    ;; Only insert face bits on NEWLINE char mapping to avoid
	    ;; obstruction of other faces like TABs and (HARD) SPACEs
	    ;; faces, font-lock faces, etc.
	    (when (memq 'color blank-active-style)
	      (dotimes (i (length vec))
		;; Due to limitations of glyph representation, the char
		;; code can not be above ?\x1FFFF.  Probably, this will
		;; be fixed after Emacs unicode merging.
		(or (eq (aref vec i) ?\n)
		    (> (aref vec i) #x1FFFF)
		    (aset vec i (make-glyph-code (aref vec i)
						 blank-newline-face)))))
	    ;; Display mapping
	    (aset buffer-display-table (car entry) vec))
	   ;; Newline char - don't display it
	   (t
	    ;; Do nothing
	    )))))))


(defun blank-display-char-off ()
  "Turn off character display mapping."
  (and blank-display-mappings
       blank-display-table-was-local
       (setq blank-display-table-was-local nil
	     buffer-display-table          blank-display-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'blank-mode)


(run-hooks 'blank-load-hook)


;; arch-tag: 1b1e2500-dbd4-4a26-8f7a-5a5edfd3c97e
;;; blank-mode.el ends here
