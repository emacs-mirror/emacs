;;; whitespace.el --- minor mode to visualize TAB, (HARD) SPACE, NEWLINE -*- lexical-binding: t -*-

;; Copyright (C) 2000-2026 Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
;; Keywords: data, text
;; Version: 13.2.2
;; URL: https://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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
;; The `whitespace-style' variable selects which way blanks are
;; visualized.
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
;; The trailing spaces are not highlighted while point is at end of line.
;; Also the spaces at beginning of buffer are not highlighted while point is at
;; beginning of buffer; and the spaces at end of buffer are not highlighted
;; while point is at end of buffer.
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
;; `whitespace-newline-mode'
;;    Toggle NEWLINE minor mode visualization ("nl" on mode line).
;;
;; `global-whitespace-newline-mode'
;;    Toggle NEWLINE global minor mode visualization ("NL" on mode line).
;;
;; `whitespace-page-delimiters-mode'
;;    Display page delimiters characters as horizontal lines ("pd" on mode line).
;;
;; `whitespace-report'
;;    Report some blank problems in buffer.
;;
;; `whitespace-report-region'
;;    Report some blank problems in a region.
;;
;; `whitespace-cleanup'
;;    Cleanup some blank problems in all buffer or at region.
;;    See the function's docstring for more information.
;;
;; `whitespace-cleanup-region'
;;    Cleanup some blank problems at region.
;;
;;
;; Options
;; -------
;;
;; Whitespace's behavior can be changed with `M-x customize-group
;; whitespace', which see for the full list of options.
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
;; Acknowledgments
;; ---------------
;;
;; Thanks to felix (EmacsWiki) for keeping highlight when switching between
;; major modes on a file.
;;
;; Thanks to David Reitter <david.reitter@gmail.com> for suggesting a
;; `whitespace-newline' initialization with low contrast relative to
;; the background color.
;;
;; Thanks to Stephen Deasey <sdeasey@gmail.com> for the
;; `indent-tabs-mode' usage suggestion.
;;
;; Thanks to Eric Cooper <ecc@cmu.edu> for the suggestion to have hook
;; actions when buffer is written as the original whitespace package
;; had.
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
;; Thanks to Tim O'Callaghan (EmacsWiki) for the idea about highlight
;; "long" lines.  See EightyColumnRule (EmacsWiki).
;;
;; Thanks to Yanghui Bian <yanghuibian@gmail.com> for indicating a new
;; NEWLINE character mapping.
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

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User Variables:


;;; Interface to the command system


(defgroup whitespace nil
  "Visualize blanks (TAB, (HARD) SPACE and NEWLINE)."
  :link '(emacs-library-link :tag "Source Lisp File" "whitespace.el")
  :version "23.1"
  :group 'convenience)


(defcustom whitespace-style
  '(face
    tabs spaces trailing lines space-before-tab newline
    indentation empty space-after-tab
    space-mark tab-mark newline-mark
    missing-newline-at-eof)
  "Determine the kinds of whitespace are visualized.

The value is a list containing one or more of the following symbols:

   face                 visualize by using faces (see below).

   trailing             visualize trailing blanks via faces.
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   tabs                 visualize TABs via faces.
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   spaces               visualize SPACEs and HARD SPACEs via
                        faces.
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   lines                highlight lines which have columns beyond
                        `whitespace-line-column' via faces.
                        Whole line is highlighted.
                        This has precedence over `lines-tail' and
                        `lines-char' (see below).
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   lines-tail           highlighted lines which have columns beyond
                        `whitespace-line-column' via faces.
                        Only the part of line which goes beyond
                        `whitespace-line-column' column.
                        This has effect only if `lines' (see above)
                        is NOT present in `whitespace-style',
                        and if `face' (see above) IS present in
                        `whitespace-style'.

   lines-char           lines which have columns beyond
                        `whitespace-line-column' are highlighted via
                        putting a face on the first character that goes
                        beyond the `whitespace-line-column' column.
                        It has effect only if `lines' or
                        `lines-tail' (see above) is not present
                        in `whitespace-style' and if `face' (see
                        above) is present in `whitespace-style'.

   newline              visualize NEWLINEs via faces.
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   missing-newline-at-eof visualize missing newline at the end of
                        the file via faces.
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   page-delimiters       visualize page-break delimiter characters (^L)
                         as horizontal lines.
                         This has effect only if `face' (see above)
                         is present in `whitespace-style'.

   empty                visualize empty lines at beginning and/or
                        end of buffer via faces.
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   indentation::tab     visualize `tab-width' or more SPACEs at
                        beginning of line via faces.
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   indentation::space   visualize TABs at beginning of line via
                        faces.
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   indentation          visualize `tab-width' or more SPACEs at
                        beginning of line, if `indent-tabs-mode' (which
                        see) is non-nil; otherwise, visualize TABs
                        at beginning of line via faces.
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   big-indent           visualize big indentations via faces.
                        This has effect only if `face' (see above)
                        is present in `whitespace-style'.

   space-after-tab::tab         visualize `tab-width' or more SPACEs
                                after a TAB via faces.
                                This has effect only if `face' (see above)
                                is present in `whitespace-style'.

   space-after-tab::space       visualize TABs when `tab-width' or
                                more SPACEs occur after a TAB, via
                                faces.
                                This has effect only if `face' (see above)
                                is present in `whitespace-style'.

   space-after-tab              visualize `tab-width' or more SPACEs
                                after a TAB, if `indent-tabs-mode'
                                (which see) is non-nil; otherwise,
                                visualize the TABs via faces.
                                This has effect only if `face' (see above)
                                is present in `whitespace-style'.

   space-before-tab::tab        visualize SPACEs before TAB via faces.
                                This has effect only if `face' (see above)
                                is present in `whitespace-style'.

   space-before-tab::space      visualize TABs when SPACEs occur
                                before TAB, via faces.
                                This has effect only if `face' (see above)
                                is present in `whitespace-style'.

   space-before-tab             visualize SPACEs before TAB, if
                                `indent-tabs-mode' (which see) is
                                non-nil; otherwise, visualize TABs
                                via faces.
                                This has effect only if `face' (see above)
                                is present in `whitespace-style'.

   space-mark           visualize SPACEs and HARD SPACEs via
                        display table.

   tab-mark             visualize TABs via display table.

   newline-mark         visualize NEWLINEs via display table.

Any other value is ignored.

If nil, don't visualize TABs, (HARD) SPACEs and NEWLINEs via faces and
via display table.

There is an evaluation order for some values, if they are
included in `whitespace-style' list.  For example, if
indentation, indentation::tab and/or indentation::space are
included in `whitespace-style' list, the evaluation order is:

 * For indentation:
   1. indentation
   2. indentation::tab
   3. indentation::space

 * For SPACEs after TABs:
   1. space-after-tab
   2. space-after-tab::tab
   3. space-after-tab::space

 * For SPACEs before TABs:
   1. space-before-tab
   2. space-before-tab::tab
   3. space-before-tab::space

For example, if `indentation' and `indentation::space' are
included in `whitespace-style', the `indentation' value is used
instead of the `indentation::space' value.

One reason to not use faces to visualize spaces (i.e., not
include `face' in `whitespace-style') is to use `whitespace-mode'
only for cleaning up a buffer.  See `whitespace-cleanup' and
`whitespace-cleanup-region'.

See also `whitespace-display-mappings' for documentation."
  :type '(set :tag "Kind of Blank"
              (const :tag "(Face) Face visualization" face)
              (const :tag "(Face) Trailing TABs, SPACEs and HARD SPACEs"
                     trailing)
              (const :tag "(Face) TABs" tabs)
              (const :tag "(Face) SPACEs and HARD SPACEs" spaces)
              (const :tag "(Face) Lines" lines)
              (const :tag "(Face) Lines, only overlong part" lines-tail)
              (const :tag "(Face) Lines, only first character" lines-char)
              (const :tag "(Face) NEWLINEs" newline)
              (const :tag "(Face) Missing newlines at EOB"
                     missing-newline-at-eof)
              (const :tag "(Face) Page delimiters" page-delimiters)
              (const :tag "(Face) Empty Lines At BOB And/Or EOB" empty)
              (const :tag "(Face) Indentation SPACEs" indentation::tab)
              (const :tag "(Face) Indentation TABs"
                     indentation::space)
              (const :tag "(Face) Indentation TABs or SPACEs" indentation)
              (const :tag "(Face) Too much line indentation" big-indent)
              (const :tag "(Face) SPACEs after TAB: SPACEs"
                     space-after-tab::tab)
              (const :tag "(Face) SPACEs after TAB: TABs"
                     space-after-tab::space)
              (const :tag "(Face) SPACEs after TAB" space-after-tab)
              (const :tag "(Face) SPACEs before TAB: SPACEs"
                     space-before-tab::tab)
              (const :tag "(Face) SPACEs before TAB: TABs"
                     space-before-tab::space)
              (const :tag "(Face) SPACEs before TAB" space-before-tab)
              (const :tag "(Mark) SPACEs and HARD SPACEs" space-mark)
              (const :tag "(Mark) TABs" tab-mark)
              (const :tag "(Mark) NEWLINEs" newline-mark))
  :version "31.1")

(defvar whitespace-space 'whitespace-space
  "Symbol face used to visualize SPACE.
Used when `whitespace-style' includes the value `spaces'.")
(make-obsolete-variable 'whitespace-space "use the face instead." "24.4")


(defface whitespace-space
  '((((class color) (background dark))
     :background "grey20"      :foreground "darkgray")
    (((class color) (background light))
     :background "LightYellow" :foreground "lightgray")
    (t :inverse-video t))
  "Face used to visualize SPACE.

See `whitespace-space-regexp'.")


(defvar whitespace-hspace 'whitespace-hspace
  "Symbol face used to visualize HARD SPACE.
Used when `whitespace-style' includes the value `spaces'.")
(make-obsolete-variable 'whitespace-hspace "use the face instead." "24.4")

(defface whitespace-hspace		; 'nobreak-space
  '((((class color) (background dark))
     :background "grey24"        :foreground "darkgray")
    (((class color) (background light))
     :background "LemonChiffon3" :foreground "lightgray")
    (t :inverse-video t))
  "Face used to visualize HARD SPACE.

See `whitespace-hspace-regexp'.")


(defvar whitespace-tab 'whitespace-tab
  "Symbol face used to visualize TAB.
Used when `whitespace-style' includes the value `tabs'.")
(make-obsolete-variable 'whitespace-tab
                        "customize the face `whitespace-tab' instead." "24.4")

(defface whitespace-tab
  '((((class color) (background dark))
     :background "grey22" :foreground "darkgray")
    (((class color) (background light))
     :background "beige"  :foreground "lightgray")
    (t :inverse-video t))
  "Face used to visualize TAB.

See `whitespace-tab-regexp'.")


(defvar whitespace-newline 'whitespace-newline
  "Symbol face used to visualize NEWLINE char mapping.
See `whitespace-display-mappings'.
Used when `whitespace-style' includes the values `newline-mark'
and `newline'.")
(make-obsolete-variable 'whitespace-newline "use the face instead." "24.4")

(defface whitespace-newline
  '((default :weight normal)
    (((class color) (background dark)) :foreground "darkgray")
    (((class color) (min-colors 88) (background light)) :foreground "lightgray")
    ;; Displays with 16 colors use lightgray as background, so using a
    ;; lightgray foreground makes the newline mark invisible.
    (((class color) (background light)) :foreground "brown")
    (t :underline t))
  "Face used to visualize NEWLINE char mapping.

See `whitespace-display-mappings'.")


(defvar whitespace-trailing 'whitespace-trailing
  "Symbol face used to visualize trailing blanks.
Used when `whitespace-style' includes the value `trailing'.")
(make-obsolete-variable 'whitespace-trailing "use the face instead." "24.4")

(defface whitespace-trailing		; 'trailing-whitespace
  '((default :weight bold)
    (((class mono)) :inverse-video t :underline t)
    (t :background "red1" :foreground "yellow"))
  "Face used to visualize trailing blanks.

See `whitespace-trailing-regexp'.")


(defvar whitespace-line 'whitespace-line
  "Symbol face used to visualize \"long\" lines.
See `whitespace-line-column'.
Used when `whitespace-style' includes the value `line'.")
(make-obsolete-variable 'whitespace-line "use the face instead." "24.4")

(defface whitespace-line
  '((((class mono)) :inverse-video t :weight bold :underline t)
    (t :background "gray20" :foreground "violet"))
  "Face used to visualize \"long\" lines.

See `whitespace-line-column'.")


(defvar whitespace-space-before-tab 'whitespace-space-before-tab
  "Symbol face used to visualize SPACEs before TAB.
Used when `whitespace-style' includes the value `space-before-tab'.")
(make-obsolete-variable 'whitespace-space-before-tab
                        "use the face instead." "24.4")

(defface whitespace-space-before-tab
  '((((class mono)) :inverse-video t :weight bold :underline t)
    (t :background "DarkOrange" :foreground "firebrick"))
  "Face used to visualize SPACEs before TAB.

See `whitespace-space-before-tab-regexp'.")


(defvar whitespace-indentation 'whitespace-indentation
  "Symbol face used to visualize `tab-width' or more SPACEs at beginning of line.
Used when `whitespace-style' includes the value `indentation'.")
(make-obsolete-variable 'whitespace-indentation "use the face instead." "24.4")

(defface whitespace-indentation
  '((((class mono)) :inverse-video t :weight bold :underline t)
    (t :background "yellow" :foreground "firebrick"))
  "Face used to visualize `tab-width' or more SPACEs at beginning of line.

See `whitespace-indentation-regexp'.")

(defface whitespace-big-indent
  '((((class mono)) :inverse-video t :weight bold :underline t)
    (t :background "red" :foreground "firebrick"))
  "Face used to visualize big indentation.

See `whitespace-big-indent-regexp'.")

(defface whitespace-missing-newline-at-eof
  '((((class mono)) :inverse-video t :weight bold :underline t)
    (t :background "#d0d040" :foreground "black"))
  "Face used to visualize missing newline at the end of the file.")

(defvar whitespace-empty 'whitespace-empty
  "Symbol face used to visualize empty lines at beginning and/or end of buffer.
Used when `whitespace-style' includes the value `empty'.")
(make-obsolete-variable 'whitespace-empty "use the face instead." "24.4")

(defface whitespace-empty
  '((((class mono)) :inverse-video t :weight bold :underline t)
    (t :background "yellow" :foreground "firebrick" :extend t))
  "Face used to visualize empty lines at beginning and/or end of buffer.

See `whitespace-empty-at-bob-regexp' and `whitespace-empty-at-eob-regexp.")


(defvar whitespace-space-after-tab 'whitespace-space-after-tab
  "Symbol face used to visualize `tab-width' or more SPACEs after TAB.
Used when `whitespace-style' includes the value `space-after-tab'.")
(make-obsolete-variable 'whitespace-space-after-tab
                        "use the face instead." "24.4")

(defface whitespace-space-after-tab
  '((((class mono)) :inverse-video t :weight bold :underline t)
    (t :background "yellow" :foreground "firebrick"))
  "Face used to visualize `tab-width' or more SPACEs after TAB.

See `whitespace-space-after-tab-regexp'.")

(defface whitespace-page-delimiter
  '((((supports :underline (:color foreground-color  :style double-line)))
     :underline (:color foreground-color :style double-line)
     :height 0.1 :extend t :inherit shadow)
    (((supports :strike-through t))
     :height 0.1 :strike-through t :extend t :inherit shadow)
    (t :height 0.1 :extend t :inherit shadow :inverse-video t))
  "Face used to visualize page delimiter characters."
  :version "31.1")

(defcustom whitespace-hspace-regexp
  "\\(\u00A0+\\)"
  "Regexp to match HARD SPACE characters that should be visualized.

The HARD SPACE characters are highlighted using the `whitespace-hspace' face.
Here are some examples:

   \"\\\\(^\\xA0+\\\\)\"		\
visualize only leading HARD SPACEs.
   \"\\\\(\\xA0+$\\\\)\"		\
visualize only trailing HARD SPACEs.
   \"\\\\(^\\xA0+\\\\|\\xA0+$\\\\)\"	\
visualize leading and/or trailing HARD SPACEs.
   \"\\t\\\\(\\xA0+\\\\)\\t\"		\
visualize only HARD SPACEs between TABs.

NOTE: Always enclose the elements to highlight in \\\\(...\\\\).
      Use exactly one pair of enclosing \\\\( and \\\\).

This variable is used when `whitespace-style' includes `spaces'."
  :type '(regexp :tag "HARD SPACE Chars"))


(defcustom whitespace-space-regexp "\\( +\\)"
  "Regexp to match SPACE characters that should be visualized.

The SPACE characters are highlighted using the `whitespace-space' face.
By default only ASCII SPACE character is visualized, but if you
are typing in some non-Latin language, there may be other
characters besides \" \" that should be considered SPACE.

Here are some examples:

   \"\\\\(^ +\\\\)\"		visualize only leading SPACEs.
   \"\\\\( +$\\\\)\"		visualize only trailing SPACEs.
   \"\\\\(^ +\\\\| +$\\\\)\"	\
visualize leading and/or trailing SPACEs.
   \"\\t\\\\( +\\\\)\\t\"	visualize only SPACEs between TABs.

NOTE: Always enclose the elements to highlight in \\\\(...\\\\).
      Use exactly one pair of enclosing \\\\( and \\\\).

This variable is used when `whitespace-style' includes `spaces'."
  :type '(regexp :tag "SPACE Chars"))


(defcustom whitespace-tab-regexp "\\(\t+\\)"
  "Regexp to match TAB characters that should be visualized.

The TAB characters are highlighted using the `whitespace-tab' face.
By default only ASCII TAB character is visualized, but if you
are typing in some non-Latin language, there may be other
characters besides \" \" that should be considered a TAB.

Here are some examples:

   \"\\\\(^\\t+\\\\)\"		visualize only leading TABs.
   \"\\\\(\\t+$\\\\)\"		visualize only trailing TABs.
   \"\\\\(^\\t+\\\\|\\t+$\\\\)\"	\
visualize leading and/or trailing TABs.
   \" \\\\(\\t+\\\\) \"	visualize only TABs between SPACEs.

NOTE: Always enclose the elements to highlight in \\\\(...\\\\).
      Use exactly one pair of enclosing \\\\( and \\\\).

This variable is used when `whitespace-style' includes `tabs'."
  :type '(regexp :tag "TAB Chars"))


(defcustom whitespace-trailing-regexp
  "\\([\t \u00A0]+\\)$"
  "Regexp to match trailing characters that should be visualized.

The trailing characters are highlighted using the `whitespace-trailing' face.
There may be other characters besides:

   \" \"  \"\\t\"  \"\\u00A0\"

that should be considered blank.

NOTE: Always enclose the elements to highlight in \"\\\\(\"...\"\\\\)$\".
      Use exactly one pair of enclosing elements above.

This variable is used when `whitespace-style' includes `trailing'."
  :type '(regexp :tag "Trailing Chars"))


(defcustom whitespace-space-before-tab-regexp "\\( +\\)\\(\t+\\)"
  "Regexp to match SPACEs before TAB that should be visualized.

The SPACE characters are highlighted using the `whitespace-space-before-tab'
face.
This variable is used when `whitespace-style' includes
`space-before-tab', `space-before-tab::tab' or `space-before-tab::space'."
  :type '(regexp :tag "SPACEs Before TAB"))


(defcustom whitespace-indentation-regexp
  '("^\t*\\(\\( \\{%d\\}\\)+\\)[^\n\t]"
    . "^ *\\(\t+\\).")
  "Regexps to match indentation whitespace that should be visualized.

The value should be a cons whose car specifies the regexp to match
visualization of SPACEs, and the cdr specifies the regexp to match
visualization of TABs.

The indentation characters are highlighted using the `whitespace-indentation'
face.
This variable is used when `whitespace-style' includes `indentation',
`indentation::tab' or  `indentation::space'."
  :type '(cons (string :tag "Indentation SPACEs")
	       (regexp :tag "Indentation TABs")))


(defcustom whitespace-empty-at-bob-regexp "\\`\\([ \t\n]*\\(?:\n\\|$\\)\\)"
  "Regexp to match empty lines at beginning of buffer that should be visualized.

The empty lines are highlighted using the `whitespace-empty' face.
This variable is used when `whitespace-style' includes `empty'."
  :type '(regexp :tag "Empty Lines At Beginning Of Buffer"))


(defcustom whitespace-empty-at-eob-regexp "^\\([ \t\n]+\\)\\'"
  "Regexp to match empty lines at end of buffer that should be visualized.

The empty lines are highlighted using the `whitespace-empty' face.
This variable is used when `whitespace-style' includes `empty'."
  :type '(regexp :tag "Empty Lines At End Of Buffer"))


(defcustom whitespace-space-after-tab-regexp
  '("\t+\\(\\( \\{%d,\\}\\)+\\)"
    . "\\(\t+\\) \\{%d,\\}")
  "Regexps to match multiple SPACEs after TAB that should be visualized.

The SPACE and TAB characters will be visualized if there at least
as many SPACEs as `tab-width' after a TAB.
The value should be a cons whose car is used for SPACEs visualization
and whose cdr is used for TABs visualization.

The SPACE characters are highlighted using the `whitespace-space-after-tab'
face.
This variable is used when `whitespace-style' includes `space-after-tab',
`space-after-tab::tab' or `space-after-tab::space'."
  :type '(cons (string :tag "SPACEs After TAB")
	       string))

(defcustom whitespace-big-indent-regexp
  "^\\(\\(?:\t\\{4,\\}\\| \\{32,\\}\\)[\t ]*\\)"
  "Regexp to match big indentation at BOL that should be visualized.

The indentation characters are highlighted using the `whitespace-big-indent'
face.
If you're using non-Latin languages, there may be other characters
besides \"\\t\" that should be considered a TAB.

NOTE: Always enclose the elements to highlight in \\\\(...\\\\).
      Use exactly one pair of enclosing \\\\( and \\\\).

This variable is used when `whitespace-style' includes `big-indent'."
  :version "25.1"
  :type '(regexp :tag "Detect too much indentation at the beginning of a line"))


(defcustom whitespace-line-column 80
  "Column beyond which the line is highlighted.

The value must be an integer or nil.  If nil, use the value
of the `fill-column' variable.

The characters beyond the column specified by this variable are
highlighted using the `whitespace-line' face.

This variable is used when `whitespace-style' includes `lines',
`lines-tail' or `lines-char'."
  :type '(choice :tag "Line Length Limit"
		 (integer :tag "Line Length")
		 (const :tag "Use fill-column" nil))
  :safe  #'integerp)


;; Hacked from `visible-whitespace-mappings' in visws.el
(defcustom whitespace-display-mappings
  '(
    (space-mark   ?\     [?·]     [?.])		; space - middle dot
    (space-mark   ?\xA0  [?¤]     [?_])		; hard space - currency sign
    ;; NEWLINE is displayed using the face `whitespace-newline'
    (newline-mark ?\n    [?$ ?\n])			; eol - dollar sign
    ;; (newline-mark ?\n    [?↵ ?\n] [?$ ?\n])	; eol - downwards arrow
    ;; (newline-mark ?\n    [?¶ ?\n] [?$ ?\n])	; eol - pilcrow
    ;; (newline-mark ?\n    [?¯ ?\n]  [?$ ?\n])	; eol - overscore
    ;; (newline-mark ?\n    [?¬ ?\n]  [?$ ?\n])	; eol - negation
    ;; (newline-mark ?\n    [?° ?\n]  [?$ ?\n])	; eol - degrees
    ;;
    ;; WARNING: the mapping below has a problem.
    ;; When a TAB occupies exactly one column, it will display the
    ;; character ?\xBB at that column followed by a TAB which goes to
    ;; the next TAB column.
    ;; If this is a problem for you, please, comment the line below.
    (tab-mark     ?\t    [?» ?\t] [?\\ ?\t])	; tab - right guillemet
    )
  "Alist of mappings for displaying characters.

Each element has the following form:

   (KIND CHAR VECTOR...)

Where:

KIND    is the kind of character.
        It can be one of the following symbols:

        tab-mark        for TAB character

        space-mark      for SPACE or HARD SPACE character

        newline-mark    for NEWLINE character

CHAR    is the character to be mapped.

VECTOR  is a vector of characters to be displayed in place of CHAR.
        The first vector that can be displayed by the terminal is used;
        if no display vector for a mapping can be displayed, then
        that character is displayed unmodified.

The NEWLINE character is displayed using the face given by
`whitespace-newline' variable.

This variable is used when `whitespace-style' includes `tab-mark',
`space-mark' or `newline-mark'."
  :type '(repeat
	  (list :tag "Character Mapping"
		(choice :tag "Char Kind"
			(const :tag "Tab" tab-mark)
			(const :tag "Space" space-mark)
			(const :tag "Newline" newline-mark))
		(character :tag "Char")
		(repeat :inline t :tag "Vector List"
			(vector :tag ""
				(repeat :inline t
					:tag "Vector Characters"
					(character :tag "Char")))))))


(defcustom whitespace-global-modes t
  "Modes for which global `whitespace-mode' is automatically turned on.

Global `whitespace-mode' is controlled by the command
`global-whitespace-mode'.

If nil, no modes have `whitespace-mode' automatically turned on.

If t, all modes that support `whitespace-mode' have it
automatically turned on.

Else it should be a list of `major-mode' symbol names for which
`whitespace-mode' should be automatically turned on.  The sense
of the list is negated if it begins with `not'.  For example:

   (c-mode c++-mode)

means that `whitespace-mode' is turned on for buffers in C and
C++ modes only."
  :type '(choice :tag "Global Modes"
		 (const :tag "None" nil)
		 (const :tag "All" t)
		 (set :menu-tag "Mode Specific" :tag "Modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t
			      (symbol :tag "Mode")))))


(defcustom whitespace-action nil
  "Specify which action is taken when a buffer is visited or written.

The value is a list containing one or more of the following symbols:

   nil                  no action is taken.

   cleanup              always cleanup any bogus whitespace when local
                        whitespace is turned on.
                        See `whitespace-cleanup' and
                        `whitespace-cleanup-region'.

   report-on-bogus      always report if there is any bogus whitespace
                        when local whitespace is turned on.

   auto-cleanup         cleanup any bogus whitespace when buffer is
                        written.
                        See `whitespace-cleanup' and
                        `whitespace-cleanup-region'.

   abort-on-bogus       signal an error when writing the buffer if there is
                        any bogus whitespace in the buffer.

   warn-if-read-only    give a warning if `cleanup' or `auto-cleanup'
                        is included in `whitespace-action' and the
                        buffer is read-only.

Any other value is treated as nil."
  :type '(choice :tag "Actions"
		 (const :tag "None" nil)
		 (repeat :tag "Action List"
		  (choice :tag "Action"
			  (const :tag "Cleanup When On" cleanup)
			  (const :tag "Report On Bogus" report-on-bogus)
			  (const :tag "Auto Cleanup" auto-cleanup)
			  (const :tag "Abort On Bogus" abort-on-bogus)
			  (const :tag "Warn If Read-Only" warn-if-read-only)))))

(defvar whitespace--page-delimiters-keyword
  `((,(lambda (bound)
        (re-search-forward (concat page-delimiter "\n") bound t))
     0
     (prog1 nil
       (put-text-property (match-beginning 0) (1- (match-end 0)) 'display " ")
       (add-text-properties (match-beginning 0) (match-end 0)
                            '( face whitespace-page-delimiter
                               display-line-numbers-disable t)))))
  "Used to add page delimiters keywords to `whitespace-font-lock-keywords'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Local mode


;;;###autoload
(define-minor-mode whitespace-mode
  "Toggle whitespace visualization (Whitespace mode).

See also `whitespace-style', `whitespace-newline' and
`whitespace-display-mappings'.

This mode uses a number of faces to visualize the whitespace; see
the customization group `whitespace' for details."
  :lighter    " ws"
  (cond
   (noninteractive			; running a batch job
    (setq whitespace-mode nil))
   (whitespace-mode			; whitespace-mode on
    (whitespace-turn-on)
    (whitespace-action-when-on))
   (t					; whitespace-mode off
    (whitespace-turn-off))))


;;;###autoload
(define-minor-mode whitespace-newline-mode
  "Toggle newline visualization (Whitespace Newline mode).

Use `whitespace-newline-mode' only for NEWLINE visualization
exclusively.  For other visualizations, including NEWLINE
visualization together with (HARD) SPACEs and/or TABs, please,
use `whitespace-mode'.

See also `whitespace-newline' and `whitespace-display-mappings'."
  :lighter    " nl"
  (let ((whitespace-style '(face newline-mark newline)))
    (whitespace-mode (if whitespace-newline-mode
			 1 -1)))
  ;; sync states (running a batch job)
  (setq whitespace-newline-mode whitespace-mode))

;;;###autoload
(define-minor-mode whitespace-page-delimiters-mode
  "Display page-break delimiter characters as horizontal lines."
  :lighter " pd"
  :group 'whitespace
  (let ((whitespace-style '(face page-delimiters)))
    (whitespace-mode (if whitespace-page-delimiters-mode
                         1 -1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Global mode


;;;###autoload
(define-globalized-minor-mode global-whitespace-mode
  whitespace-mode
  whitespace-turn-on-if-enabled
  :init-value nil)

(defvar whitespace-enable-predicate
  (lambda ()
    (and (cond
          ((eq whitespace-global-modes t))
          ((listp whitespace-global-modes)
           (if (eq (car-safe whitespace-global-modes) 'not)
               (not (derived-mode-p (cdr whitespace-global-modes)))
             (derived-mode-p whitespace-global-modes)))
          (t nil))
         ;; ...we have a display (not running a batch job)
         (not noninteractive)
         ;; ...the buffer is not internal (name starts with a space)
         (not (eq (aref (buffer-name) 0) ?\ ))
         ;; ...the buffer is not special (name starts with *)
         (or (not (eq (aref (buffer-name) 0) ?*))
             ;; except the scratch buffer.
             (string= (buffer-name) "*scratch*"))))
  "Predicate to decide which buffers obey `global-whitespace-mode'.
This function is called with no argument and should return non-nil
if the current buffer should obey `global-whitespace-mode'.
This variable is normally modified via `add-function'.")

(defun whitespace-turn-on-if-enabled ()
  (when (funcall whitespace-enable-predicate)
    (whitespace-mode)))

;;;###autoload
(define-minor-mode global-whitespace-newline-mode
  "Toggle global newline visualization (Global Whitespace Newline mode).

Use `global-whitespace-newline-mode' only for NEWLINE
visualization exclusively.  For other visualizations, including
NEWLINE visualization together with (HARD) SPACEs and/or TABs,
please use `global-whitespace-mode'.

See also `whitespace-newline' and `whitespace-display-mappings'."
  :lighter    " NL"
  :global     t
  (let ((whitespace-style '(newline-mark newline)))
    (global-whitespace-mode (if global-whitespace-newline-mode
                                1 -1))
    ;; sync states (running a batch job)
    (setq global-whitespace-newline-mode global-whitespace-mode)))
(make-obsolete 'global-whitespace-newline-mode
               "use `global-whitespace-mode' with `whitespace-style' set to `(newline-mark newline)' instead."
               "28.1")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Toggle


(defconst whitespace-style-value-list
  '(face
    tabs
    spaces
    trailing
    page-delimiters
    lines
    lines-tail
    lines-char
    newline
    empty
    indentation
    indentation::tab
    indentation::space
    big-indent
    space-after-tab
    space-after-tab::tab
    space-after-tab::space
    space-before-tab
    space-before-tab::tab
    space-before-tab::space
    help-newline       ; value used by `whitespace-insert-option-mark'
    tab-mark
    space-mark
    newline-mark
    )
  "List of valid `whitespace-style' values.")


(defconst whitespace-toggle-option-alist
  '((?f    . face)
    (?t    . tabs)
    (?s    . spaces)
    (?p    . page-delimiters)
    (?r    . trailing)
    (?l    . lines)
    (?L    . lines-tail)
    (?\C-l . lines-char)
    (?n    . newline)
    (?e    . empty)
    (?\C-i . indentation)
    (?I    . indentation::tab)
    (?i    . indentation::space)
    (?\C-t . big-indent)
    (?\C-a . space-after-tab)
    (?A    . space-after-tab::tab)
    (?a    . space-after-tab::space)
    (?\C-b . space-before-tab)
    (?B    . space-before-tab::tab)
    (?b    . space-before-tab::space)
    (?T    . tab-mark)
    (?S    . space-mark)
    (?N    . newline-mark)
    (?x    . whitespace-style)
    )
  "Alist of toggle options.

Each element has the form:

   (CHAR . SYMBOL)

Where:

CHAR	is a char which the user will have to type.

SYMBOL	is a valid symbol associated with CHAR.
	See `whitespace-style-value-list'.")


(defvar-local whitespace-active-style nil
  "Used to save locally `whitespace-style' value.")

(defvar-local whitespace-point (point)
  "Used to save locally current point value.
Used by function `whitespace-trailing-regexp' (which see).")
(defvar-local whitespace-point--used nil
  "Region whose highlighting depends on `whitespace-point'.")

(defvar-local whitespace-bob-marker nil
  "Position of the buffer's first non-empty line.
This marker is positioned at the beginning of the first line in
the buffer that contains a non-space character.  If no such line
exists, this is positioned at the end of the buffer (which could
be after `whitespace-eob-marker' if the buffer contains nothing
but empty lines).")

(defvar-local whitespace-eob-marker nil
  "Position after the buffer's last non-empty line.
This marker is positioned at the beginning of the first line
immediately following the last line in the buffer that contains a
non-space character.  If no such line exists, this is positioned
at the beginning of the buffer (which could be before
`whitespace-bob-marker' if the buffer contains nothing but empty
lines).")

(defvar-local whitespace-buffer-changed nil
  "Used to indicate locally if buffer changed.
Used by `whitespace-post-command-hook' and `whitespace-buffer-changed'
functions (which see).")


;;;###autoload
(defun whitespace-toggle-options (arg)
  "Toggle local `whitespace-mode' options.

If local whitespace-mode is off, toggle the option given by ARG
and turn on local whitespace-mode.

If local whitespace-mode is on, toggle the option given by ARG
and restart local whitespace-mode.

Interactively, it reads one of the following chars:

  CHAR	MEANING
  (VIA FACES)
   f	toggle face visualization
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   p	toggle page delimiters visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   n	toggle NEWLINE visualization
   e	toggle empty line at bob and/or eob visualization
   C-i	toggle indentation SPACEs visualization (via `indent-tabs-mode')
   I	toggle indentation SPACEs visualization
   i	toggle indentation TABs visualization
   C-t	toggle big indentation visualization
   C-a	toggle SPACEs after TAB visualization (via `indent-tabs-mode')
   A	toggle SPACEs after TAB: SPACEs visualization
   a	toggle SPACEs after TAB: TABs visualization
   C-b	toggle SPACEs before TAB visualization (via `indent-tabs-mode')
   B	toggle SPACEs before TAB: SPACEs visualization
   b	toggle SPACEs before TAB: TABs visualization

  (VIA DISPLAY TABLE)
   T	toggle TAB visualization
   S	toggle SPACEs before TAB visualization
   N	toggle NEWLINE visualization

   x	restore `whitespace-style' value
   ?	display brief help

Non-interactively, ARG should be a symbol or a list of symbols.
The valid symbols are:

   face			toggle face visualization
   tabs			toggle TAB visualization
   spaces		toggle SPACE and HARD SPACE visualization
   trailing		toggle trailing blanks visualization
   page-delimiters	toggle page delimiters visualization
   lines		toggle \"long lines\" visualization
   lines-tail		toggle \"long lines\" tail visualization
   newline		toggle NEWLINE visualization
   empty		toggle empty line at bob and/or eob visualization
   indentation		toggle indentation SPACEs visualization
   indentation::tab	toggle indentation SPACEs visualization
   indentation::space	toggle indentation TABs visualization
   big-indent		toggle big indentation visualization
   space-after-tab		toggle SPACEs after TAB visualization
   space-after-tab::tab		toggle SPACEs after TAB: SPACEs visualization
   space-after-tab::space	toggle SPACEs after TAB: TABs visualization
   space-before-tab		toggle SPACEs before TAB visualization
   space-before-tab::tab	toggle SPACEs before TAB: SPACEs visualization
   space-before-tab::space	toggle SPACEs before TAB: TABs visualization

   tab-mark		toggle TAB visualization
   space-mark		toggle SPACEs before TAB visualization
   newline-mark		toggle NEWLINE visualization

   whitespace-style	restore `whitespace-style' value

See `whitespace-style' and `indent-tabs-mode' for documentation."
  (interactive (whitespace-interactive-char t))
  (let ((whitespace-style
	 (whitespace-toggle-list t arg whitespace-active-style)))
    (whitespace-mode 0)
    (whitespace-mode 1)))


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
  (VIA FACES)
   f	toggle face visualization
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   p	toggle page delimiters visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   C-l	toggle \"long lines\" one character visualization
   n	toggle NEWLINE visualization
   e	toggle empty line at bob and/or eob visualization
   C-i	toggle indentation SPACEs visualization (via `indent-tabs-mode')
   I	toggle indentation SPACEs visualization
   i	toggle indentation TABs visualization
   C-t	toggle big indentation visualization
   C-a	toggle SPACEs after TAB visualization (via `indent-tabs-mode')
   A	toggle SPACEs after TAB: SPACEs visualization
   a	toggle SPACEs after TAB: TABs visualization
   C-b	toggle SPACEs before TAB visualization (via `indent-tabs-mode')
   B	toggle SPACEs before TAB: SPACEs visualization
   b	toggle SPACEs before TAB: TABs visualization

  (VIA DISPLAY TABLE)
   T	toggle TAB visualization
   S	toggle SPACEs before TAB visualization
   N	toggle NEWLINE visualization

   x	restore `whitespace-style' value
   ?	display brief help

Non-interactively, ARG should be a symbol or a list of symbols.
The valid symbols are:

   face			toggle face visualization
   tabs			toggle TAB visualization
   spaces		toggle SPACE and HARD SPACE visualization
   trailing		toggle trailing blanks visualization
   page-delimiters	toggle page delimiters visualization
   lines		toggle \"long lines\" visualization
   lines-tail		toggle \"long lines\" tail visualization
   lines-char		toggle \"long lines\" one character visualization
   newline		toggle NEWLINE visualization
   empty		toggle empty line at bob and/or eob visualization
   indentation		toggle indentation SPACEs visualization
   indentation::tab	toggle indentation SPACEs visualization
   indentation::space	toggle indentation TABs visualization
   big-indent		toggle big indentation visualization
   space-after-tab		toggle SPACEs after TAB visualization
   space-after-tab::tab		toggle SPACEs after TAB: SPACEs visualization
   space-after-tab::space	toggle SPACEs after TAB: TABs visualization
   space-before-tab		toggle SPACEs before TAB visualization
   space-before-tab::tab	toggle SPACEs before TAB: SPACEs visualization
   space-before-tab::space	toggle SPACEs before TAB: TABs visualization

   tab-mark		toggle TAB visualization
   space-mark		toggle SPACEs before TAB visualization
   newline-mark		toggle NEWLINE visualization

   whitespace-style	restore `whitespace-style' value

See `whitespace-style' and `indent-tabs-mode' for documentation."
  (interactive (whitespace-interactive-char nil))
  (let ((whitespace-style
	 (whitespace-toggle-list nil arg whitespace-toggle-style)))
    (setq whitespace-toggle-style whitespace-style)
    (global-whitespace-mode 0)
    (global-whitespace-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Cleanup


;;;###autoload
(defun whitespace-cleanup ()
  "Cleanup some blank problems in all buffer or at region.

It usually applies to the whole buffer, but in transient mark
mode when the mark is active, it applies to the region.  It also
applies to the region when it is not in transient mark mode, the
mark is active and \\[universal-argument] was pressed just before
calling `whitespace-cleanup' interactively.

See also `whitespace-cleanup-region'.

The problems cleaned up are:

1. empty lines at beginning of buffer.
2. empty lines at end of buffer.
   If `whitespace-style' includes the value `empty', remove all
   empty lines at beginning and/or end of buffer.

3. `tab-width' or more SPACEs at beginning of line.
   If `whitespace-style' includes the value `indentation':
   replace `tab-width' or more SPACEs at beginning of line by
   TABs, if `indent-tabs-mode' is non-nil; otherwise, replace TABs by
   SPACEs.
   If `whitespace-style' includes the value `indentation::tab',
   replace `tab-width' or more SPACEs at beginning of line by TABs.
   If `whitespace-style' includes the value `indentation::space',
   replace TABs by SPACEs.

4. SPACEs before TAB.
   If `whitespace-style' includes the value `space-before-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-before-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-before-tab::space', replace TABs by SPACEs.

5. SPACEs or TABs at end of line.
   If `whitespace-style' includes the value `trailing', remove
   all SPACEs or TABs at end of line.

6. `tab-width' or more SPACEs after TAB.
   If `whitespace-style' includes the value `space-after-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-after-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-after-tab::space', replace TABs by SPACEs.

See `whitespace-style', `indent-tabs-mode' and `tab-width' for
documentation."
  (interactive "@")
  (cond
   ;; read-only buffer
   (buffer-read-only
    (whitespace-warn-read-only "cleanup"))
   ;; region active
   ((and (or transient-mark-mode
	     current-prefix-arg)
	 mark-active)
    ;; PROBLEMs 1 and 2 are not handled in region
    ;; PROBLEM 3: `tab-width' or more SPACEs at bol
    ;; PROBLEM 4: SPACEs before TAB
    ;; PROBLEM 5: SPACEs or TABs at eol
    ;; PROBLEM 6: `tab-width' or more SPACEs after TAB
    (whitespace-cleanup-region (region-beginning) (region-end)))
   ;; whole buffer
   (t
    (save-excursion
      ;; PROBLEM 1: empty lines at bob
      ;; PROBLEM 2: empty lines at eob
      ;; ACTION: remove all empty lines at bob and/or eob
      (when (memq 'empty whitespace-style)
        (let (overwrite-mode)		; enforce no overwrite
          (goto-char (point-min))
          (when (looking-at whitespace-empty-at-bob-regexp)
            (delete-region (match-beginning 1) (match-end 1)))
          (when (re-search-forward
                 whitespace-empty-at-eob-regexp nil t)
            (delete-region (match-beginning 1) (match-end 1))))))
    ;; PROBLEM 3: `tab-width' or more SPACEs at bol
    ;; PROBLEM 4: SPACEs before TAB
    ;; PROBLEM 5: SPACEs or TABs at eol
    ;; PROBLEM 6: `tab-width' or more SPACEs after TAB
    (whitespace-cleanup-region (point-min) (point-max)))))


;;;###autoload
(defun whitespace-cleanup-region (start end)
  "Cleanup some blank problems at region.

The problems cleaned up are:

1. `tab-width' or more SPACEs at beginning of line.
   If `whitespace-style' includes the value `indentation':
   replace `tab-width' or more SPACEs at beginning of line by TABs,
   if `indent-tabs-mode' is non-nil; otherwise, replace TABs by
   SPACEs.
   If `whitespace-style' includes the value `indentation::tab',
   replace `tab-width' or more SPACEs at beginning of line by TABs.
   If `whitespace-style' includes the value `indentation::space',
   replace TABs by SPACEs.

2. SPACEs before TAB.
   If `whitespace-style' includes the value `space-before-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-before-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-before-tab::space', replace TABs by SPACEs.

3. SPACEs or TABs at end of line.
   If `whitespace-style' includes the value `trailing', remove
   all SPACEs or TABs at end of line.

4. `tab-width' or more SPACEs after TAB.
   If `whitespace-style' includes the value `space-after-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-after-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-after-tab::space', replace TABs by SPACEs.

5. missing newline at end of file.
   If `whitespace-style' includes the value `missing-newline-at-eof',
   and the cleanup region includes the end of file, add a final newline
   if it is not there already.

See `whitespace-style', `indent-tabs-mode' and `tab-width' for
documentation."
  (interactive "@r")
  (if buffer-read-only
      ;; read-only buffer
      (whitespace-warn-read-only "cleanup region")
    ;; non-read-only buffer
    (let ((rstart           (min start end))
	  (rend             (copy-marker (max start end)))
	  overwrite-mode		; enforce no overwrite
	  tmp)
      (save-excursion
        ;; PROBLEM 1: `tab-width' or more SPACEs at bol
        (cond
         ;; ACTION: replace `tab-width' or more SPACEs at bol by TABs, if
         ;; `indent-tabs-mode' is non-nil; otherwise, replace TABs
         ;; by SPACEs.
         ((memq 'indentation whitespace-style)
          (let ((regexp (whitespace-indentation-regexp)))
            (goto-char rstart)
            (while (re-search-forward regexp rend t)
              (setq tmp (current-indentation))
              (goto-char (match-beginning 0))
              (delete-horizontal-space)
              (unless (eolp)
                (indent-to tmp)))))
         ;; ACTION: replace `tab-width' or more SPACEs at bol by TABs.
         ((memq 'indentation::tab whitespace-style)
          (whitespace-replace-action
           'tabify rstart rend
           (whitespace-indentation-regexp 'tab) 0))
         ;; ACTION: replace TABs by SPACEs.
         ((memq 'indentation::space whitespace-style)
          (whitespace-replace-action
           'untabify rstart rend
           (whitespace-indentation-regexp 'space) 0)))
        ;; PROBLEM 3: SPACEs or TABs at eol
        ;; ACTION: remove all SPACEs or TABs at eol
        (when (memq 'trailing whitespace-style)
          (whitespace-replace-action
           'delete-region rstart rend
           whitespace-trailing-regexp 1))
        ;; PROBLEM 4: `tab-width' or more SPACEs after TAB
        (cond
         ;; ACTION: replace `tab-width' or more SPACEs by TABs, if
         ;; `indent-tabs-mode' is non-nil; otherwise, replace TABs
         ;; by SPACEs.
         ((memq 'space-after-tab whitespace-style)
          (whitespace-replace-action
           (if indent-tabs-mode 'tabify 'untabify)
           rstart rend (whitespace-space-after-tab-regexp) 1))
         ;; ACTION: replace `tab-width' or more SPACEs by TABs.
         ((memq 'space-after-tab::tab whitespace-style)
          (whitespace-replace-action
           'tabify rstart rend
           (whitespace-space-after-tab-regexp 'tab) 1))
         ;; ACTION: replace TABs by SPACEs.
         ((memq 'space-after-tab::space whitespace-style)
          (whitespace-replace-action
           'untabify rstart rend
           (whitespace-space-after-tab-regexp 'space) 1)))
        ;; PROBLEM 2: SPACEs before TAB
        (cond
         ;; ACTION: replace SPACEs before TAB by TABs, if
         ;; `indent-tabs-mode' is non-nil; otherwise, replace TABs
         ;; by SPACEs.
         ((memq 'space-before-tab whitespace-style)
          (whitespace-replace-action
           (if indent-tabs-mode 'tabify 'untabify)
           rstart rend whitespace-space-before-tab-regexp
           (if indent-tabs-mode 0 2)))
         ;; ACTION: replace SPACEs before TAB by TABs.
         ((memq 'space-before-tab::tab whitespace-style)
          (whitespace-replace-action
           'tabify rstart rend
           whitespace-space-before-tab-regexp 0))
         ;; ACTION: replace TABs by SPACEs.
         ((memq 'space-before-tab::space whitespace-style)
          (whitespace-replace-action
           'untabify rstart rend
           whitespace-space-before-tab-regexp 2)))
        ;; PROBLEM 5: missing newline at end of file
        (and (memq 'missing-newline-at-eof whitespace-style)
             (> (point-max) (point-min))
             (= (point-max) (without-restriction (point-max)))
             (/= (char-before (point-max)) ?\n)
             (not (and (eq selective-display t)
                       (= (char-before (point-max)) ?\r)))
             (goto-char (point-max))
             (ignore-errors (insert "\n"))))
      (set-marker rend nil))))		; point marker to nowhere


(defun whitespace-replace-action (action rstart rend regexp index)
  "Do ACTION in the string matched by REGEXP between RSTART and REND.

INDEX is the level group matched by REGEXP and used by ACTION.

See also `tab-width'."
  (goto-char rstart)
  (while (re-search-forward regexp rend t)
    (goto-char (match-end index))
    (funcall action (match-beginning index) (match-end index))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User command - report


(defun whitespace-regexp (regexp &optional kind)
  "Return REGEXP depending on `indent-tabs-mode'."
  (format
   (cond
    ((or (eq kind 'tab)
         indent-tabs-mode)
     (car regexp))
    ((or (eq kind 'space)
         (not indent-tabs-mode))
     (cdr regexp)))
   tab-width))


(defun whitespace-indentation-regexp (&optional kind)
  "Return the indentation regexp depending on `indent-tabs-mode'."
  (whitespace-regexp whitespace-indentation-regexp kind))


(defun whitespace-space-after-tab-regexp (&optional kind)
  "Return the space-after-tab regexp depending on `indent-tabs-mode'."
  (whitespace-regexp whitespace-space-after-tab-regexp kind))


(defconst whitespace-report-list
  (list
   (cons 'empty                   whitespace-empty-at-bob-regexp)
   (cons 'empty                   whitespace-empty-at-eob-regexp)
   (cons 'trailing                whitespace-trailing-regexp)
   (cons 'indentation             nil)
   (cons 'indentation::tab        nil)
   (cons 'indentation::space      nil)
   (cons 'space-before-tab        whitespace-space-before-tab-regexp)
   (cons 'space-before-tab::tab   whitespace-space-before-tab-regexp)
   (cons 'space-before-tab::space whitespace-space-before-tab-regexp)
   (cons 'space-after-tab         nil)
   (cons 'space-after-tab::tab    nil)
   (cons 'space-after-tab::space  nil)
   )
   "List of whitespace bogus symbol and corresponding regexp.")


(defconst whitespace-report-text
  '( ;; `indent-tabs-mode' has non-nil value
    "\
 Whitespace Report

 Current Setting                       Whitespace Problem

 empty                    []     []  empty lines at beginning of buffer
 empty                    []     []  empty lines at end of buffer
 trailing                 []     []  SPACEs or TABs at end of line
 indentation              []     []  >= `tab-width' SPACEs at beginning of line
 indentation::tab         []     []  >= `tab-width' SPACEs at beginning of line
 indentation::space       []     []  TABs at beginning of line
 space-before-tab         []     []  SPACEs before TAB
 space-before-tab::tab    []     []  SPACEs before TAB: SPACEs
 space-before-tab::space  []     []  SPACEs before TAB: TABs
 space-after-tab          []     []  >= `tab-width' SPACEs after TAB
 space-after-tab::tab     []     []  >= `tab-width' SPACEs after TAB: SPACEs
 space-after-tab::space   []     []  >= `tab-width' SPACEs after TAB: TABs

 indent-tabs-mode =
 tab-width        = \n\n"
    . ;; `indent-tabs-mode' has nil value
    "\
 Whitespace Report

 Current Setting                       Whitespace Problem

 empty                    []     []  empty lines at beginning of buffer
 empty                    []     []  empty lines at end of buffer
 trailing                 []     []  SPACEs or TABs at end of line
 indentation              []     []  TABs at beginning of line
 indentation::tab         []     []  >= `tab-width' SPACEs at beginning of line
 indentation::space       []     []  TABs at beginning of line
 space-before-tab         []     []  SPACEs before TAB
 space-before-tab::tab    []     []  SPACEs before TAB: SPACEs
 space-before-tab::space  []     []  SPACEs before TAB: TABs
 space-after-tab          []     []  >= `tab-width' SPACEs after TAB
 space-after-tab::tab     []     []  >= `tab-width' SPACEs after TAB: SPACEs
 space-after-tab::space   []     []  >= `tab-width' SPACEs after TAB: TABs

 indent-tabs-mode =
 tab-width        = \n\n")
  "Text for whitespace bogus report.

It is a cons of strings, where the car part is used when
`indent-tabs-mode' is non-nil, and the cdr part is used when
`indent-tabs-mode' is nil.")


(defconst whitespace-report-buffer-name "*Whitespace Report*"
  "The buffer name for whitespace bogus report.")


;;;###autoload
(defun whitespace-report (&optional force report-if-bogus)
  "Report some whitespace problems in buffer.

Perform `whitespace-report-region' on the current buffer."
  (interactive (list current-prefix-arg))
  (whitespace-report-region (point-min) (point-max)
			    force report-if-bogus))


;;;###autoload
(defun whitespace-report-region (start end &optional force report-if-bogus)
  "Report some whitespace problems in a region.

Return nil if there is no whitespace problem; otherwise, return
non-nil.

If FORCE is non-nil or \\[universal-argument] was pressed just
before calling `whitespace-report-region' interactively, it
forces all classes of whitespace problem to be considered
significant.

If REPORT-IF-BOGUS is t, it reports only when there are any
whitespace problems in buffer; if it is `never', it does not
report problems.

Report if some of the following whitespace problems exist:

* If `indent-tabs-mode' is non-nil:
   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   trailing		3. SPACEs or TABs at end of line.
   indentation		4. line starts with `tab-width' or more SPACEs.
   space-before-tab	5. SPACEs before TAB.
   space-after-tab	6. `tab-width' or more SPACEs after TAB.

* If `indent-tabs-mode' is nil:
   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   trailing		3. SPACEs or TABs at end of line.
   indentation		4. TABS at beginning of line.
   space-before-tab	5. SPACEs before TAB.
   space-after-tab	6. `tab-width' or more SPACEs after TAB.

See `whitespace-style' for documentation.
See also `whitespace-cleanup' and `whitespace-cleanup-region' for
cleaning up these problems."
  (interactive "r")
  (setq force (or current-prefix-arg force))
  (save-excursion
    (let* ((has-bogus nil)
           (rstart    (min start end))
           (rend      (max start end))
           ;; Fall back to whitespace-style so we can run before
           ;; the mode is active.
           (style     (copy-sequence
                       (or whitespace-active-style whitespace-style)))
           (bogus-list
            (mapcar
             (lambda (option)
               (when force
                 (push (car option) style))
               (goto-char rstart)
               (let ((regexp
                      (cond
                       ((eq (car option) 'indentation)
                        (whitespace-indentation-regexp))
                       ((eq (car option) 'indentation::tab)
                        (whitespace-indentation-regexp 'tab))
                       ((eq (car option) 'indentation::space)
                        (whitespace-indentation-regexp 'space))
                       ((eq (car option) 'space-after-tab)
                        (whitespace-space-after-tab-regexp))
                       ((eq (car option) 'space-after-tab::tab)
                        (whitespace-space-after-tab-regexp 'tab))
                       ((eq (car option) 'space-after-tab::space)
                        (whitespace-space-after-tab-regexp 'space))
                       ((eq (car option) 'missing-newline-at-eof)
                        ".\\'")
                       (t
                        (cdr option)))))
                 (when (re-search-forward regexp rend t)
                   (unless has-bogus
                     (setq has-bogus (memq (car option) style)))
                   t)))
             whitespace-report-list)))
      (when (pcase report-if-bogus ('nil t) ('never nil) (_ has-bogus))
        (whitespace-kill-buffer whitespace-report-buffer-name)
        ;; `indent-tabs-mode' may be local to current buffer
        ;; `tab-width' may be local to current buffer
        (let ((ws-indent-tabs-mode indent-tabs-mode)
              (ws-tab-width tab-width))
          (with-current-buffer (get-buffer-create
                                whitespace-report-buffer-name)
            (let ((inhibit-read-only t))
              (special-mode)
              (erase-buffer)
              (insert (if ws-indent-tabs-mode
                          (car whitespace-report-text)
                        (cdr whitespace-report-text)))
              (goto-char (point-min))
              (forward-line 3)
              (dolist (option whitespace-report-list)
                (forward-line 1)
                (whitespace-mark-x
                 27 (memq (car option) style))
                (whitespace-mark-x 7 (car bogus-list))
                (setq bogus-list (cdr bogus-list)))
              (forward-line 1)
              (whitespace-insert-value ws-indent-tabs-mode)
              (whitespace-insert-value ws-tab-width)
              (when has-bogus
                (goto-char (point-max))
                (insert (substitute-command-keys
                         " Type \\[whitespace-cleanup]")
                        " to cleanup the buffer.\n\n"
                        (substitute-command-keys
                         " Type \\[whitespace-cleanup-region]")
                        " to cleanup a region.\n\n"))
              (whitespace-display-window (current-buffer))))))
      has-bogus)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal functions


(defvar-local whitespace-font-lock-keywords nil
  "Used to save the value `whitespace-color-on' adds to `font-lock-keywords'.")


(defconst whitespace-help-text
  "\
 Whitespace Toggle Options                  | scroll up  :  SPC   or > |
                                            | scroll down:  M-SPC or < |
 FACES                                      \\__________________________/
 []  f   - toggle face visualization
 []  t   - toggle TAB visualization
 []  s   - toggle SPACE and HARD SPACE visualization
 []  r   - toggle trailing blanks visualization
 []  l   - toggle \"long lines\" visualization
 []  L   - toggle \"long lines\" tail visualization
 []  C-l - toggle \"long lines\" one character visualization
 []  n   - toggle NEWLINE visualization
 []  e   - toggle empty line at bob and/or eob visualization
 []  C-i - toggle indentation SPACEs visualization (via `indent-tabs-mode')
 []  I   - toggle indentation SPACEs visualization
 []  i   - toggle indentation TABs visualization
 []  C-t - toggle big indentation visualization
 []  C-a - toggle SPACEs after TAB visualization (via `indent-tabs-mode')
 []  A   - toggle SPACEs after TAB: SPACEs visualization
 []  a   - toggle SPACEs after TAB: TABs visualization
 []  C-b - toggle SPACEs before TAB visualization (via `indent-tabs-mode')
 []  B   - toggle SPACEs before TAB: SPACEs visualization
 []  b   - toggle SPACEs before TAB: TABs visualization

 DISPLAY TABLE
 []  T - toggle TAB visualization
 []  S - toggle SPACE and HARD SPACE visualization
 []  N - toggle NEWLINE visualization

      x - restore `whitespace-style' value

      ? - display this text\n\n"
  "Text for whitespace toggle options.")


(defconst whitespace-help-buffer-name "*Whitespace Toggle Options*"
  "The buffer name for whitespace toggle options.")


(defun whitespace-insert-value (value)
  "Insert VALUE at column 20 of next line."
  (forward-line 1)
  (move-to-column 20 t)
  (insert (format "%s" value)))


(defun whitespace-mark-x (nchars condition)
  "Insert the mark (`X' or ` ') after NCHARS depending on CONDITION."
  (forward-char nchars)
  (insert (if condition "X" " ")))


(defun whitespace-insert-option-mark (the-list the-value)
  "Insert the option mark (`X' or ` ') in toggle options buffer."
  (goto-char (point-min))
  (forward-line 2)
  (dolist (sym  the-list)
    (if (eq sym 'help-newline)
	(forward-line 2)
      (forward-line 1)
      (whitespace-mark-x 2 (memq sym the-value)))))


(defun whitespace-help-on (style)
  "Display the whitespace toggle options."
  (unless (get-buffer whitespace-help-buffer-name)
    (delete-other-windows)
    (let ((buffer (get-buffer-create whitespace-help-buffer-name)))
      (with-current-buffer buffer
	(erase-buffer)
	(insert whitespace-help-text)
	(whitespace-insert-option-mark
	 whitespace-style-value-list style)
	(whitespace-display-window buffer)))))


(defun whitespace-display-window (buffer)
  "Display BUFFER, preferably below the selected window."
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (let ((window (display-buffer
	         buffer
	         `((display-buffer-reuse-window
		    display-buffer-below-selected)))))
    (shrink-window-if-larger-than-buffer window)))

(defun whitespace-kill-buffer (buffer-name)
  "Kill buffer BUFFER-NAME and windows related with it."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))


(defun whitespace-help-off ()
  "Remove the buffer and window of the whitespace toggle options."
  (whitespace-kill-buffer whitespace-help-buffer-name))


(defun whitespace-help-scroll (&optional up)
  "Scroll help window, if it exists.

If UP is non-nil, scroll up; otherwise, scroll down."
  (condition-case nil
      (let ((buffer (get-buffer whitespace-help-buffer-name)))
	(if buffer
	    (with-selected-window (get-buffer-window buffer)
	      (if up
		  (scroll-up 3)
		(scroll-down 3)))
	  (ding)))
    ;; handler
    ((error)
     ;; just ignore error
     )))


(defun whitespace-interactive-char (local-p)
  "Interactive function to read a char and return a symbol.

If LOCAL-P is non-nil, it uses a local context; otherwise, it
uses a global context.

It accepts one of the following chars:

  CHAR	MEANING
  (VIA FACES)
   f	toggle face visualization
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   C-l	toggle \"long lines\" one character visualization
   n	toggle NEWLINE visualization
   e	toggle empty line at bob and/or eob visualization
   C-i	toggle indentation SPACEs visualization (via `indent-tabs-mode')
   I	toggle indentation SPACEs visualization
   i	toggle indentation TABs visualization
   C-a	toggle SPACEs after TAB visualization (via `indent-tabs-mode')
   A	toggle SPACEs after TAB: SPACEs visualization
   a	toggle SPACEs after TAB: TABs visualization
   C-b	toggle SPACEs before TAB visualization (via `indent-tabs-mode')
   B	toggle SPACEs before TAB: SPACEs visualization
   b	toggle SPACEs before TAB: TABs visualization

  (VIA DISPLAY TABLE)
   T	toggle TAB visualization
   S	toggle SPACE and HARD SPACE visualization
   N	toggle NEWLINE visualization

   x	restore `whitespace-style' value
   ?	display brief help

See also `whitespace-toggle-option-alist'."
  (let* ((is-off (not (if local-p
			  whitespace-mode
			global-whitespace-mode)))
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
	      (cond
	       ((eq ch ?\?)   (whitespace-help-on style))
	       ((eq ch ?\ )   (whitespace-help-scroll t))
	       ((eq ch ?\M- ) (whitespace-help-scroll))
	       ((eq ch ?>)    (whitespace-help-scroll t))
	       ((eq ch ?<)    (whitespace-help-scroll))
	       (t             (ding))))
	    (whitespace-help-off)
	    (message " "))		; clean echo area
	;; handler
	((quit error)
	 (whitespace-help-off)
	 (error (error-message-string data)))))
    (list sym)))			; return the appropriate symbol


(defun whitespace-toggle-list (local-p arg the-list)
  "Toggle options in THE-LIST based on list ARG.

If LOCAL-P is non-nil, it uses a local context; otherwise, it
uses a global context.

ARG is a list of options to be toggled.

THE-LIST is a list of options.  This list will be toggled and the
resultant list will be returned."
  (unless (if local-p whitespace-mode global-whitespace-mode)
    (setq the-list whitespace-style))
  (setq the-list (copy-sequence the-list)) ; keep original list
  (dolist (sym (if (listp arg) arg (list arg)))
    (cond
     ;; ignore help value
     ((eq sym 'help-newline))
     ;; restore default values
     ((eq sym 'whitespace-style)
      (setq the-list whitespace-style))
     ;; toggle valid values
     ((memq sym whitespace-style-value-list)
      (setq the-list (if (memq sym the-list)
			 (delq sym the-list)
		       (cons sym the-list))))))
  the-list)


(defvar-local whitespace-display-table nil
  "Used to save a local display table.")

(defvar-local whitespace-display-table-was-local nil
  "Used to remember whether a buffer initially had a local display table.")

(defun whitespace-turn-on ()
  "Turn on whitespace visualization."
  ;; prepare local hooks
  (add-hook 'write-file-functions #'whitespace-write-file-hook nil t)
  ;; create whitespace local buffer environment
  (setq-local whitespace-font-lock-keywords nil)
  (setq-local whitespace-display-table nil)
  (setq-local whitespace-display-table-was-local nil)
  (setq-local whitespace-active-style
              (if (listp whitespace-style)
	          whitespace-style
	        (list whitespace-style)))
  ;; turn on whitespace
  (when whitespace-active-style
    (whitespace-color-on)
    (whitespace-display-char-on)))


(defun whitespace-turn-off ()
  "Turn off whitespace visualization."
  (remove-hook 'write-file-functions #'whitespace-write-file-hook t)
  (when whitespace-active-style
    (whitespace-color-off)
    (whitespace-display-char-off)))


(defun whitespace-style-face-p ()
  "Return t if there is some visualization via face."
  (and (memq 'face whitespace-active-style)
       (or (memq 'tabs                    whitespace-active-style)
	   (memq 'spaces                  whitespace-active-style)
	   (memq 'trailing                whitespace-active-style)
	   (memq 'lines                   whitespace-active-style)
	   (memq 'lines-tail              whitespace-active-style)
	   (memq 'lines-char              whitespace-active-style)
	   (memq 'newline                 whitespace-active-style)
           (memq 'page-delimiters         whitespace-active-style)
	   (memq 'empty                   whitespace-active-style)
	   (memq 'indentation             whitespace-active-style)
	   (memq 'indentation::tab        whitespace-active-style)
	   (memq 'indentation::space      whitespace-active-style)
	   (memq 'big-indent              whitespace-active-style)
	   (memq 'space-after-tab         whitespace-active-style)
	   (memq 'space-after-tab::tab    whitespace-active-style)
	   (memq 'space-after-tab::space  whitespace-active-style)
	   (memq 'space-before-tab        whitespace-active-style)
	   (memq 'space-before-tab::tab   whitespace-active-style)
	   (memq 'space-before-tab::space whitespace-active-style))
       t))


(defun whitespace--clone ()
  "Hook function run after `make-indirect-buffer' and `clone-buffer'."
  (when (whitespace-style-face-p)
    (setq-local whitespace-bob-marker
                (copy-marker (marker-position whitespace-bob-marker)
                             (marker-insertion-type whitespace-bob-marker)))
    (setq-local whitespace-eob-marker
                (copy-marker (marker-position whitespace-eob-marker)
                             (marker-insertion-type whitespace-eob-marker)))))


(defun whitespace-color-on ()
  "Turn on color visualization."
  (when (whitespace-style-face-p)
    ;; save current point and refontify when necessary
    (setq-local whitespace-point (point))
    (setq whitespace-point--used
          (let ((ol (make-overlay (point) (point) nil nil t)))
            (delete-overlay ol) ol))
    (setq-local whitespace-bob-marker (point-min-marker))
    (setq-local whitespace-eob-marker (point-max-marker))
    (whitespace--update-bob-eob)
    (setq-local whitespace-buffer-changed nil)
    (add-hook 'post-command-hook #'whitespace-post-command-hook nil t)
    (add-hook 'after-change-functions #'whitespace--update-bob-eob
              ;; The -1 ensures that it runs before any
              ;; `font-lock-mode' hook functions.
              -1 t)
    (add-hook 'clone-buffer-hook #'whitespace--clone nil t)
    (add-hook 'clone-indirect-buffer-hook #'whitespace--clone nil t)
    ;; Add whitespace-mode color into font lock.
    (setq
     whitespace-font-lock-keywords
     `(
       (whitespace-point--flush-used)
       ,@(when (memq 'spaces whitespace-active-style)
           ;; Show SPACEs.
           `((,whitespace-space-regexp 1 whitespace-space t)
             ;; Show HARD SPACEs.
             (,whitespace-hspace-regexp 1 whitespace-hspace t)))
       ,@(when (memq 'tabs whitespace-active-style)
           ;; Show TABs.
           `((,whitespace-tab-regexp 1 whitespace-tab t)))
       ,@(when (memq 'trailing whitespace-active-style)
           ;; Show trailing blanks.
           `((,#'whitespace-trailing-regexp 1 whitespace-trailing t)))
       ,@(when (or (memq 'lines      whitespace-active-style)
                   (memq 'lines-tail whitespace-active-style)
                   (memq 'lines-char whitespace-active-style))
           ;; Show "long" lines.
           `((,#'whitespace-lines-regexp
              ,(cond
                ;; whole line
                ((memq 'lines whitespace-active-style) 0)
                ;; line tail
                ((memq 'lines-tail whitespace-active-style) 2)
                ;; first overflowing character
                ((memq 'lines-char whitespace-active-style) 3))
              whitespace-line prepend)))
       ,@(when (memq 'page-delimiters whitespace-active-style)
           (unless (and (memq 'display font-lock-extra-managed-props)
                        (memq 'display-line-numbers-disable font-lock-extra-managed-props))
             (setq-local font-lock-extra-managed-props
                         `(,@font-lock-extra-managed-props display display-line-numbers-disable)))
           ;; Show page delimiters characters
           whitespace--page-delimiters-keyword)
       ,@(when (or (memq 'space-before-tab whitespace-active-style)
                   (memq 'space-before-tab::tab whitespace-active-style)
                   (memq 'space-before-tab::space whitespace-active-style))
           `((,whitespace-space-before-tab-regexp
              ,(cond
                ((memq 'space-before-tab whitespace-active-style)
                 ;; Show SPACEs before TAB (indent-tabs-mode).
                 (if indent-tabs-mode 1 2))
                ((memq 'space-before-tab::tab whitespace-active-style)
                 1)
                ((memq 'space-before-tab::space whitespace-active-style)
                 2))
              whitespace-space-before-tab t)))
       ,@(when (or (memq 'indentation whitespace-active-style)
                   (memq 'indentation::tab whitespace-active-style)
                   (memq 'indentation::space whitespace-active-style))
           `((,#'whitespace--indentation-matcher
              1 whitespace-indentation t)))
       ,@(when (memq 'big-indent whitespace-active-style)
           ;; Show big indentation.
           `((,whitespace-big-indent-regexp 1 'whitespace-big-indent t)))
       ,@(when (memq 'empty whitespace-active-style)
           ;; Show empty lines at beginning of buffer.
           `((,#'whitespace--empty-at-bob-matcher
              0 whitespace-empty t)
             ;; Show empty lines at end of buffer.
             (,#'whitespace--empty-at-eob-matcher
              0 whitespace-empty t)))
       ,@(when (or (memq 'space-after-tab whitespace-active-style)
                   (memq 'space-after-tab::tab whitespace-active-style)
                   (memq 'space-after-tab::space whitespace-active-style))
           `((,(cond
                ((memq 'space-after-tab whitespace-active-style)
                 ;; Show SPACEs after TAB (indent-tabs-mode).
                 (whitespace-space-after-tab-regexp))
                ((memq 'space-after-tab::tab whitespace-active-style)
                 ;; Show SPACEs after TAB (SPACEs).
                 (whitespace-space-after-tab-regexp 'tab))
                ((memq 'space-after-tab::space whitespace-active-style)
                 ;; Show SPACEs after TAB (TABs).
                 (whitespace-space-after-tab-regexp 'space)))
              1 whitespace-space-after-tab t)))
       ,@(when (memq 'missing-newline-at-eof whitespace-active-style)
           ;; Show missing newline.
           `((".\\'" 0
              ;; Don't mark the end of the buffer if point is there --
              ;; it probably means that the user is typing something
              ;; at the end of the buffer.
              (and (/= whitespace-point (point-max))
                   'whitespace-missing-newline-at-eof)
              prepend)))))
    (font-lock-add-keywords nil whitespace-font-lock-keywords 'append)
    (font-lock-flush)))


(defun whitespace-color-off ()
  "Turn off color visualization."
  ;; turn off font lock
  (kill-local-variable 'whitespace-point--used)
  (when (whitespace-style-face-p)
    (remove-hook 'post-command-hook #'whitespace-post-command-hook t)
    (remove-hook 'after-change-functions #'whitespace--update-bob-eob
                 t)
    (remove-hook 'clone-buffer-hook #'whitespace--clone t)
    (remove-hook 'clone-indirect-buffer-hook #'whitespace--clone t)
    (font-lock-remove-keywords nil whitespace-font-lock-keywords)
    (font-lock-flush)))

(defun whitespace-point--used (start end)
  (let ((ostart (overlay-start whitespace-point--used)))
    (if ostart
        (move-overlay whitespace-point--used
                      (min start ostart)
                      (max end (overlay-end whitespace-point--used)))
      (move-overlay whitespace-point--used start end))))

(defun whitespace-point--flush-used (limit)
  (let ((ostart (overlay-start whitespace-point--used)))
    ;; Strip parts of whitespace-point--used we're about to refresh.
    (when ostart
      (let ((oend (overlay-end whitespace-point--used)))
        (if (<= (point) ostart)
            (if (<= oend limit)
                (delete-overlay whitespace-point--used)
              (move-overlay whitespace-point--used limit oend)))
        (if (<= oend limit)
            (move-overlay whitespace-point--used ostart (point))))))
  nil)

(defun whitespace-trailing-regexp (limit)
  "Match trailing spaces which do not contain the point at end of line."
  (let ((status t))
    (while (if (re-search-forward whitespace-trailing-regexp limit t)
	       (when (= whitespace-point (match-end 1)) ; Loop if point at eol.
                 (whitespace-point--used (match-beginning 0) (match-end 0))
                 t)
	     (setq status nil)))		  ;; end of buffer
    status))

(defun whitespace-lines-regexp (limit)
  (re-search-forward
   (let ((line-column (or whitespace-line-column fill-column)))
     (format
      "^\\([^\t\n]\\{%s\\}\\|[^\t\n]\\{0,%s\\}\t\\)\\{%d\\}%s\\(?2:\\(?3:.\\).*\\)$"
      tab-width
      (1- tab-width)
      (/ line-column tab-width)
      (let ((rem (% line-column tab-width)))
        (if (zerop rem)
            ""
          (format ".\\{%d\\}" rem)))))
   limit t))

(defun whitespace--empty-at-bob-matcher (limit)
  "Match empty/space-only lines at beginning of buffer (BoB).
Match does not extend past position LIMIT.  For improved UX, the
line containing `whitespace-point' and subsequent lines are
excluded from the match.  (The idea is that the user might be
about to start typing, and if they do, that line and any
following empty lines will no longer be BoB empty lines.
Highlighting those lines can be distracting.)"
  (let ((p (point))
        (e (min whitespace-bob-marker limit
                ;; EoB marker will be before BoB marker if the buffer
                ;; has nothing but empty lines.
                whitespace-eob-marker
                (save-excursion (goto-char whitespace-point)
                                (line-beginning-position)))))
    (when (= p (point-min))
      (with-silent-modifications
        ;; See the comment in `whitespace--update-bob-eob' for why
        ;; this text property is added here.
        (put-text-property (point-min) whitespace-bob-marker
                           'font-lock-multiline t)))
    (when (< p e)
      (set-match-data (list p e))
      (goto-char e))))

(defsubst whitespace--looking-back (regexp)
  (save-excursion
    (when (/= 0 (skip-chars-backward " \t\n"))
      (unless (bolp)
	(forward-line 1))
      (looking-at regexp))))

(defun whitespace--empty-at-eob-matcher (limit)
  "Match empty/space-only lines at end of buffer (EoB).
Match does not extend past position LIMIT.  For improved UX, the
line containing `whitespace-point' and preceding lines are
excluded from the match.  (The idea is that the user might be
about to start typing, and if they do, that line and previous
empty lines will no longer be EoB empty lines.  Highlighting
those lines can be distracting.)"
  (when (= limit (point-max))
    (with-silent-modifications
      ;; See the comment in `whitespace--update-bob-eob' for why this
      ;; text property is added here.
      (put-text-property whitespace-eob-marker limit
                         'font-lock-multiline t)))
  (let ((b (max (point) whitespace-eob-marker
                whitespace-bob-marker ; See comment in the bob func.
                (save-excursion (goto-char whitespace-point)
                                (forward-line 1)
                                (point)))))
    (when (< b limit)
      (set-match-data (list b limit))
      (goto-char limit))))

(defun whitespace-post-command-hook ()
  "Save current point into `whitespace-point' variable.
Also refontify when necessary."
  (when (or (not (eq whitespace-point (point)))
            whitespace-buffer-changed)
    (when (and (not whitespace-buffer-changed)
               (memq 'empty whitespace-active-style))
      ;; No need to handle the `whitespace-buffer-changed' case here
      ;; because that is taken care of by the `font-lock-multiline'
      ;; text property.
      (when (<= (min (point) whitespace-point) whitespace-bob-marker)
        (font-lock-flush (point-min) whitespace-bob-marker))
      (when (>= (max (point) whitespace-point) whitespace-eob-marker)
        (font-lock-flush whitespace-eob-marker (point-max))))
    (setq-local whitespace-buffer-changed nil)
    (setq whitespace-point (point))	; current point position
    (let ((refontify (or (and (eolp) ; It is at end of line ...
                              ;; ... with trailing SPACE or TAB
                              (or (memq (preceding-char) '(?\s ?\t)))
                              (line-beginning-position))
                         (and (memq 'missing-newline-at-eof
                                    ;; If user requested to highlight
                                    ;; EOB without a newline...
                                    whitespace-active-style)
                              ;; ...and the buffer is not empty...
                              (not (= (point-min) (point-max)))
                              (= (point-max) (without-restriction (point-max)))
                              ;; ...and no newline at EOB...
                              (not (eq (char-before (point-max)) ?\n))
                              ;; ...then refontify the last character in
                              ;; the buffer
                              (max (1- (point-max)) (point-min)))))
          (ostart (overlay-start whitespace-point--used)))
      (cond
       ((not refontify)
        ;; New point does not affect highlighting: just refresh the
        ;; highlighting of old point, if needed.
        (when ostart
          (font-lock-flush ostart
                           (overlay-end whitespace-point--used))
          (delete-overlay whitespace-point--used)))
       ((not ostart)
        ;; Old point did not affect highlighting, but new one does: refresh the
        ;; highlighting of new point.
        (font-lock-flush (min refontify (point)) (max refontify (point))))
       ((save-excursion
          (goto-char ostart)
          (setq ostart (line-beginning-position))
          (and (<= ostart (max refontify (point)))
               (progn
                 (goto-char (overlay-end whitespace-point--used))
                 (let ((oend (line-beginning-position 2)))
                   (<= (min refontify (point)) oend)))))
        ;; The old point highlighting and the new point highlighting
        ;; cover a contiguous region: do a single refresh.
        (font-lock-flush (min refontify (point) ostart)
                         (max refontify (point)
                              (overlay-end whitespace-point--used)))
        (delete-overlay whitespace-point--used))
       (t
        (font-lock-flush (min refontify (point))
                         (max refontify (point)))
        (font-lock-flush ostart (overlay-end whitespace-point--used))
        (delete-overlay whitespace-point--used))))))

(defun whitespace--indentation-matcher (limit)
  "Indentation matcher for `font-lock-keywords'.
This matcher is a function instead of a static regular expression
so that the next call to `font-lock-flush' picks up any changes
to `indent-tabs-mode' and `tab-width'."
  (re-search-forward
   (whitespace-indentation-regexp
    (cond
     ((memq 'indentation whitespace-active-style) nil)
     ((memq 'indentation::tab whitespace-active-style) 'tab)
     ((memq 'indentation::space whitespace-active-style) 'space)))
   limit t))

(defun whitespace--variable-watcher (_symbol _newval _op buffer)
  "Variable watcher that calls `font-lock-flush' for BUFFER."
  (when buffer
    (with-current-buffer buffer
      (when whitespace-mode
        (font-lock-flush)))))

(defun whitespace--update-bob-eob (&optional beg end &rest _)
  "Update `whitespace-bob-marker' and `whitespace-eob-marker'.
Also apply `font-lock-multiline' text property.  If BEG and END
are non-nil, assume that only characters in that range have
changed since the last call to this function (for optimization
purposes)."
  (setq whitespace-buffer-changed t)
  (when (memq 'empty whitespace-active-style)
    ;; When a line is changed, `font-lock-mode' normally limits
    ;; re-processing to only the changed line.  That behavior is
    ;; problematic for highlighting `empty' lines because adding or
    ;; deleting a character might affect lines before or after the
    ;; change.  To address this, all `empty' lines are marked with a
    ;; non-nil `font-lock-multiline' text property.  This forces
    ;; `font-lock-mode' to re-process all of the lines whenever
    ;; there's an edit within any one of them.
    ;;
    ;; The text property must be set on `empty' lines twice per
    ;; relevant change:
    ;;
    ;;   1. Before the change.  This is necessary to ensure that
    ;;      previously highlighted lines become un-highlighted if
    ;;      necessary.  The text property must be added after the
    ;;      previous `font-lock-mode' run (the run in reaction to the
    ;;      previous change) because `font-lock-mode' clears the text
    ;;      property when it runs.
    ;;
    ;;   2. After the change, but before `font-lock-mode' reacts to
    ;;      the change.  This is necessary to ensure that new `empty'
    ;;      lines become highlighted.
    ;;
    ;; This hook function is responsible for #2, while the
    ;; `whitespace--empty-at-bob-matcher' and
    ;; `whitespace--empty-at-eob-matcher' functions are responsible
    ;; for #1.  (Those functions run after `font-lock-mode' clears the
    ;; text property and before the next change.)
    (save-excursion
      (save-restriction
        (widen)
        (let ((inhibit-read-only t))
          (when (or (null beg)
                    (<= beg (save-excursion
                              (goto-char whitespace-bob-marker)
                              ;; Any change in the first non-`empty'
                              ;; line, even if it's not the first
                              ;; character in the line, can potentially
                              ;; cause subsequent lines to become
                              ;; classified as `empty' (e.g., delete the
                              ;; "x" from " x").
                              (forward-line 1)
                              (point))))
            (goto-char (point-min))
            (set-marker whitespace-bob-marker (point))
            (save-match-data
              (when (looking-at whitespace-empty-at-bob-regexp)
                (set-marker whitespace-bob-marker (match-end 1))
                (with-silent-modifications
                  (put-text-property (match-beginning 1) (match-end 1)
                                     'font-lock-multiline t)))))
          (when (or (null end)
                    (>= end (save-excursion
                              (goto-char whitespace-eob-marker)
                              ;; See above comment for the BoB case.
                              (forward-line -1)
                              (point))))
            (goto-char (point-max))
            (set-marker whitespace-eob-marker (point))
            (save-match-data
              (when (whitespace--looking-back
                     whitespace-empty-at-eob-regexp)
                (set-marker whitespace-eob-marker (match-beginning 1))
                (with-silent-modifications
                  (put-text-property (match-beginning 1) (match-end 1)
                                     'font-lock-multiline t))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hacked from visws.el (Miles Bader <miles@gnu.org>)


(defun whitespace-style-mark-p ()
  "Return t if there is some visualization via display table."
  (and (or (memq 'tab-mark     whitespace-active-style)
           (memq 'space-mark   whitespace-active-style)
           (memq 'newline-mark whitespace-active-style))
       t))


(defsubst whitespace-char-valid-p (char)
  ;; This check should be improved!!!
  (or (< char 256)
      (characterp char)))


(defun whitespace-display-vector-p (vec)
  "Return non-nil if every character in vector VEC can be displayed."
  (let ((i (length vec)))
    (when (> i 0)
      (while (and (>= (setq i (1- i)) 0)
		  (whitespace-char-valid-p (glyph-char (aref vec i)))))
      (< i 0))))


(defun whitespace-display-char-on ()
  "Turn on character display mapping."
  (when (and whitespace-display-mappings
	     (whitespace-style-mark-p))
    (let (vecs vec)
      ;; Remember whether a buffer has a local display table.
      (unless whitespace-display-table-was-local
	(setq whitespace-display-table-was-local t)
        ;; Save the old table so we can restore it when
        ;; `whitespace-mode' is switched off again.
        (when whitespace-mode
	  (setq whitespace-display-table
	        (copy-sequence buffer-display-table)))
	;; Assure `buffer-display-table' is unique
	;; when two or more windows are visible.
	(setq buffer-display-table
	      (copy-sequence (or buffer-display-table
                                 standard-display-table))))
      (unless buffer-display-table
	(setq buffer-display-table (make-display-table)))
      (dolist (entry whitespace-display-mappings)
	;; check if it is to display this mark
	(when (memq (car entry) whitespace-style)
	  ;; Get a displayable mapping.
	  (setq vecs (cddr entry))
	  (while (and vecs
		      (not (whitespace-display-vector-p (car vecs))))
	    (setq vecs (cdr vecs)))
	  ;; Display a valid mapping.
	  (when vecs
	    (setq vec (copy-sequence (car vecs)))
	    ;; NEWLINE char
	    (when (and (eq (cadr entry) ?\n)
		       (memq 'newline whitespace-active-style))
	      ;; Only insert face bits on NEWLINE char mapping to avoid
	      ;; obstruction of other faces like TABs and (HARD) SPACEs
	      ;; faces, font-lock faces, etc.
	      (dotimes (i (length vec))
		(or (eq (aref vec i) ?\n)
		    (aset vec i
			  (make-glyph-code (aref vec i)
					   'whitespace-newline)))))
	    ;; Display mapping
	    (aset buffer-display-table (cadr entry) vec)))))))


(defun whitespace-display-char-off ()
  "Turn off character display mapping."
  (and whitespace-display-mappings
       (whitespace-style-mark-p)
       whitespace-display-table-was-local
       (setq whitespace-display-table-was-local nil
	     buffer-display-table whitespace-display-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hook


(defun whitespace-action-when-on ()
  "Action to be taken always when local whitespace is turned on."
  (cond ((memq 'cleanup whitespace-action)
	 (whitespace-cleanup))
	((memq 'report-on-bogus whitespace-action)
	 (whitespace-report nil t))))


(defun whitespace-write-file-hook ()
  "Action to be taken when buffer is written.
It should be added buffer-locally to `write-file-functions'."
  (cond ((memq 'auto-cleanup whitespace-action)
	 (whitespace-cleanup))
	((memq 'abort-on-bogus whitespace-action)
	 (when (whitespace-report nil t)
	   (error "Abort write due to whitespace problems in %s"
		  (buffer-name)))))
  nil)					; continue hook processing


(defun whitespace-warn-read-only (msg)
  "Warn if buffer is read-only."
  (when (memq 'warn-if-read-only whitespace-action)
    (message "Can't %s: %s is read-only" msg (buffer-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar whitespace--watched-vars
  '(fill-column indent-tabs-mode tab-width whitespace-line-column))

(dolist (var whitespace--watched-vars)
  (add-variable-watcher var #'whitespace--variable-watcher))

(defun whitespace-unload-function ()
  "Unload the whitespace library."
  (dolist (var whitespace--watched-vars)
    (remove-variable-watcher var #'whitespace--variable-watcher))
  (global-whitespace-mode -1)
  ;; be sure all local whitespace mode is turned off
  (save-current-buffer
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (whitespace-mode -1)))
  nil)					; continue standard unloading


(provide 'whitespace)

(make-obsolete-variable 'whitespace-load-hook
                        "use `with-eval-after-load' instead." "28.1")
(run-hooks 'whitespace-load-hook)

;;; whitespace.el ends here
