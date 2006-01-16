;;; texinfo.el --- major mode for editing Texinfo files -*- coding: iso-2022-7bit -*-

;; Copyright (C) 1985, 1988, 1989, 1990, 1991, 1992, 1993, 1996, 1997,
;;   2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Robert J. Chassell
;; Date:   [See date below for texinfo-version]
;; Maintainer: FSF
;; Keywords: maint, tex, docs

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Todo:

;; - facemenu support.
;; - command completion.

;;; Commentary:

;;; Code:

(or (fboundp 'defgroup)
    (defmacro defgroup (&rest ignore) nil))

(or (fboundp 'defcustom)
    (defmacro defcustom (var value doc &rest ignore)
      `(defvar ,var ,value ,doc)))

(eval-when-compile (require 'tex-mode) (require 'cl))
(defvar outline-heading-alist)

(defgroup texinfo nil
  "Texinfo Mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'docs)

;;;###autoload
(defcustom texinfo-open-quote "``"
  "*String inserted by typing \\[texinfo-insert-quote] to open a quotation."
  :type 'string
  :group 'texinfo)

;;;###autoload
(defcustom texinfo-close-quote "''"
  "*String inserted by typing \\[texinfo-insert-quote] to close a quotation."
  :type 'string
  :group 'texinfo)

(defcustom texinfo-mode-hook nil
  "Normal hook run when entering Texinfo mode."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'texinfo)


;;; Autoloads:

(autoload 'makeinfo-region
  "makeinfo"
  "Make Info file from region of current Texinfo file, and switch to it.

This command does not offer the `next-error' feature since it would
apply to a temporary file, not the original; use the `makeinfo-buffer'
command to gain use of `next-error'."
  t nil)

(autoload 'makeinfo-buffer
  "makeinfo"
  "Make Info file from current buffer.

Use the \\[next-error] command to move to the next error
\(if there are errors\)."
  t nil)

(autoload 'kill-compilation
  "compile"
  "Kill the process made by the \\[compile] command."
  t nil)

(autoload 'makeinfo-recenter-compilation-buffer
  "makeinfo"
  "Redisplay `*compilation*' buffer so most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  t nil)

(autoload 'texinfo-update-node
  "texnfo-upd"
  "Without any prefix argument, update the node in which point is located.
Non-nil argument (prefix, if interactive) means update the nodes in the
marked region.

The functions for creating or updating nodes and menus, and their
keybindings, are:

    `texinfo-update-node' (&optional region-p)    \\[texinfo-update-node]
    `texinfo-every-node-update' ()                \\[texinfo-every-node-update]
    `texinfo-sequential-node-update' (&optional region-p)

    `texinfo-make-menu' (&optional region-p)      \\[texinfo-make-menu]
    `texinfo-all-menus-update' ()                 \\[texinfo-all-menus-update]
    `texinfo-master-menu' ()

    `texinfo-indent-menu-description' (column &optional region-p)

The `texinfo-column-for-description' variable specifies the column to
which menu descriptions are indented. Its default value is 32."
  t nil)

(autoload 'texinfo-every-node-update
  "texnfo-upd"
  "Update every node in a Texinfo file."
  t nil)

(autoload 'texinfo-sequential-node-update
  "texnfo-upd"
  "Update one node (or many) in a Texinfo file with sequential pointers.

This function causes the `Next' or `Previous' pointer to point to the
immediately preceding or following node, even if it is at a higher or
lower hierarchical level in the document.  Continually pressing `n' or
`p' takes you straight through the file.

Without any prefix argument, update the node in which point is located.
Non-nil argument (prefix, if interactive) means update the nodes in the
marked region.

This command makes it awkward to navigate among sections and
subsections; it should be used only for those documents that are meant
to be read like a novel rather than a reference, and for which the
Info `g*' command is inadequate."
  t nil)

(autoload 'texinfo-make-menu
  "texnfo-upd"
  "Without any prefix argument, make or update a menu.
Make the menu for the section enclosing the node found following point.

Non-nil argument (prefix, if interactive) means make or update menus
for nodes within or part of the marked region.

Whenever a menu exists, and is being updated, the descriptions that
are associated with node names in the pre-existing menu are
incorporated into the new menu.  Otherwise, the nodes' section titles
are inserted as descriptions."
  t nil)

(autoload 'texinfo-all-menus-update
  "texnfo-upd"
  "Update every regular menu in a Texinfo file.
Remove pre-existing master menu, if there is one.

If called with a non-nil argument, this function first updates all the
nodes in the buffer before updating the menus."
  t nil)

(autoload 'texinfo-master-menu
  "texnfo-upd"
  "Make a master menu for a whole Texinfo file.
Non-nil argument (prefix, if interactive) means first update all
existing nodes and menus.  Remove pre-existing master menu, if there is one.

This function creates a master menu that follows the top node.  The
master menu includes every entry from all the other menus.  It
replaces any existing ordinary menu that follows the top node.

If called with a non-nil argument, this function first updates all the
menus in the buffer (incorporating descriptions from pre-existing
menus) before it constructs the master menu.

The function removes the detailed part of an already existing master
menu.  This action depends on the pre-existing master menu using the
standard `texinfo-master-menu-header'.

The master menu has the following format, which is adapted from the
recommendation in the Texinfo Manual:

   * The first part contains the major nodes in the Texinfo file: the
     nodes for the chapters, chapter-like sections, and the major
     appendices.  This includes the indices, so long as they are in
     chapter-like sections, such as unnumbered sections.

   * The second and subsequent parts contain a listing of the other,
     lower level menus, in order.  This way, an inquirer can go
     directly to a particular node if he or she is searching for
     specific information.

Each of the menus in the detailed node listing is introduced by the
title of the section containing the menu."
  t nil)

(autoload 'texinfo-indent-menu-description
  "texnfo-upd"
  "Indent every description in menu following point to COLUMN.
Non-nil argument (prefix, if interactive) means indent every
description in every menu in the region.  Does not indent second and
subsequent lines of a multi-line description."
  t nil)

(autoload 'texinfo-insert-node-lines
  "texnfo-upd"
  "Insert missing `@node' lines in region of Texinfo file.
Non-nil argument (prefix, if interactive) means also to insert the
section titles as node names; and also to insert the section titles as
node names in pre-existing @node lines that lack names."
  t nil)

(autoload 'texinfo-start-menu-description
  "texnfo-upd"
  "In this menu entry, insert the node's section title as a description.
Position point at beginning of description ready for editing.
Do not insert a title if the line contains an existing description.

You will need to edit the inserted text since a useful description
complements the node name rather than repeats it as a title does."
  t nil)

(autoload 'texinfo-multiple-files-update
  "texnfo-upd"
  "Update first node pointers in each file included in OUTER-FILE;
create or update main menu in the outer file that refers to such nodes.
This does not create or update menus or pointers within the included files.

With optional MAKE-MASTER-MENU argument (prefix arg, if interactive),
insert a master menu in OUTER-FILE.  This does not create or update
menus or pointers within the included files.

With optional UPDATE-EVERYTHING argument (numeric prefix arg, if
interactive), update all the menus and all the `Next', `Previous', and
`Up' pointers of all the files included in OUTER-FILE before inserting
a master menu in OUTER-FILE.

The command also updates the `Top' level node pointers of OUTER-FILE.

Notes:

  * this command does NOT save any files--you must save the
    outer file and any modified, included files.

  * except for the `Top' node, this command does NOT handle any
    pre-existing nodes in the outer file; hence, indices must be
    enclosed in an included file.

Requirements:

  * each of the included files must contain exactly one highest
    hierarchical level node,
  * this highest node must be the first node in the included file,
  * each highest hierarchical level node must be of the same type.

Thus, normally, each included file contains one, and only one,
chapter."
  t nil)


;;; Code:

;;; Don't you dare insert any `require' calls at top level in this file--rms.

(defvar texinfo-section-list
  '(("top" 1)
    ("chapter" 2)
    ("section" 3)
    ("subsection" 4)
    ("subsubsection" 5)
    ("unnumbered" 2)
    ("unnumberedsec" 3)
    ("unnumberedsubsec" 4)
    ("unnumberedsubsubsec" 5)
    ("appendix" 2)
    ("appendixsec" 3)
    ("appendixsection" 3)
    ("appendixsubsec" 4)
    ("appendixsubsubsec" 5)
    ("majorheading" 2)
    ("chapheading" 2)
    ("heading" 3)
    ("subheading" 4)
    ("subsubheading" 5))
  "Alist of sectioning commands and their relative level.")

;;; Syntax table

(defvar texinfo-mode-syntax-table nil)

(if texinfo-mode-syntax-table
    nil
  (setq texinfo-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" "." texinfo-mode-syntax-table)
  (modify-syntax-entry ?\\ "." texinfo-mode-syntax-table)
  (modify-syntax-entry ?@ "\\" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\^q "\\" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" texinfo-mode-syntax-table)
  (modify-syntax-entry ?{ "(}" texinfo-mode-syntax-table)
  (modify-syntax-entry ?} "){" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\n ">" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\' "w" texinfo-mode-syntax-table))

;; Written by Wolfgang Bangerth <zcg51122@rpool1.rus.uni-stuttgart.de>
;; To override this example, set either `imenu-generic-expression'
;; or `imenu-create-index-function'.
(defvar texinfo-imenu-generic-expression
  '((nil "^@\\(node\\|anchor\\)[ \t]+\\([^,\n]*\\)" 2)
    ("Chapters" "^@chapter[ \t]+\\(.*\\)$" 1))
  "Imenu generic expression for Texinfo mode.  See `imenu-generic-expression'.")

(defvar texinfo-font-lock-syntactic-keywords
  '(("\\(@\\)c\\(omment\\)?\\>" (1 "<"))
    ("^\\(@\\)ignore\\>" (1 "< b"))
    ("^@end ignore\\(\n\\)" (1 "> b")))
  "Syntactic keywords to catch comment delimiters in `texinfo-mode'.")

(defconst texinfo-environments
  '("cartouche" "copying" "defcv" "deffn" "defivar" "defmac"
    "defmethod" "defop" "defopt" "defspec" "deftp" "deftypefn"
    "deftypefun" "deftypevar" "deftypevr" "defun" "defvar"
    "defvr" "description" "detailmenu" "direntry" "display"
    "documentdescription" "enumerate" "example" "flushleft"
    "flushright" "format" "ftable" "group" "ifclear" "ifset"
    "ifhtml" "ifinfo" "ifnothtml" "ifnotinfo" "ifnotplaintext"
    "ifnottex" "ifplaintext" "iftex" "ignore" "itemize" "lisp"
    "macro" "menu" "multitable" "quotation" "smalldisplay"
    "smallexample" "smallformat" "smalllisp" "table" "tex"
    "titlepage" "verbatim" "vtable")
  "List of Texinfo environments.")

(defconst texinfo-environment-regexp
  (concat "^@" (regexp-opt (cons "end" texinfo-environments) t) "\\>")
  "Regexp for environment-like Texinfo list commands.
Subexpression 1 is what goes into the corresponding `@end' statement.")

(defface texinfo-heading
  '((t (:inherit font-lock-function-name-face)))
  "Face used for section headings in `texinfo-mode'."
  :group 'texinfo)
;; backward-compatibility alias
(put 'texinfo-heading-face 'face-alias 'texinfo-heading)
(defvar texinfo-heading-face 'texinfo-heading)

(defvar texinfo-font-lock-keywords
  `(;; All but the first had an OVERRIDE of t.
    ;; It didn't seem to be any better, and it's slower--simon.
    ;; Robert J. Chassell <bob@gnu.org> says remove this line.
    ;;("\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
    ("@\\([a-zA-Z]+\\|[^ \t\n]\\)" 1 font-lock-keyword-face) ;commands
    ("^\\*\\([^\n:]*\\)" 1 font-lock-function-name-face t) ;menu items
    ("@\\(emph\\|i\\|sc\\){\\([^}]+\\)" 2 'italic)
    ("@\\(strong\\|b\\){\\([^}]+\\)" 2 'bold)
    ("@\\(kbd\\|key\\|url\\|uref\\){\\([^}]+\\)" 2 font-lock-string-face)
    ;; The following two groups have an OVERRIDE of `keep' because
    ;; their arguments frequently include a @@, and we don't want that
    ;; to overwrite the normal fontification of the argument.
    ("@\\(file\\|email\\){\\([^}]+\\)" 2 font-lock-string-face keep)
    ("@\\(samp\\|code\\|var\\|math\\|env\\|command\\|option\\){\\([^}]+\\)"
     2 font-lock-variable-name-face keep)
    ("@\\(cite\\|x?ref\\|pxref\\|dfn\\|inforef\\){\\([^}]+\\)"
     2 font-lock-constant-face)
    ("@\\(anchor\\){\\([^}]+\\)" 2 font-lock-type-face)
    ("@\\(dmn\\|acronym\\|value\\){\\([^}]+\\)" 2 font-lock-builtin-face)
    ("@\\(end\\|itemx?\\) +\\(.+\\)" 2 font-lock-keyword-face keep)
    ;; (,texinfo-environment-regexp
    ;;  1 (texinfo-clone-environment (match-beginning 1) (match-end 1)) keep)
    (,(concat "^@" (regexp-opt (mapcar 'car texinfo-section-list) t)
	       ".*\n") 0 texinfo-heading-face t))
  "Additional expressions to highlight in Texinfo mode.")

(defun texinfo-clone-environment (start end)
  (let ((endp nil))
    (save-excursion
      (ignore-errors
	(goto-char start)
	(when (looking-at "end\\Sw+\\(\\sw+\\)")
	  (setq endp t start (match-beginning 1) end (match-end 1)))
	(unless (get-char-property start 'text-clones)
	  (if endp
	      (texinfo-last-unended-begin)
	    (forward-word 1)
	    (texinfo-next-unmatched-end))
	  (skip-syntax-forward "^w")
	  (when (looking-at
		 (concat (regexp-quote (buffer-substring start end)) "\\>"))
	    (text-clone-create start end 'spread "\\w*")))))))


;;; Keybindings
(defvar texinfo-mode-map nil)

;;; Keys common both to Texinfo mode and to TeX shell.

(defun texinfo-define-common-keys (keymap)
  "Define the keys both in Texinfo mode and in the texinfo-tex-shell."
  (define-key keymap "\C-c\C-t\C-k"    'tex-kill-job)
  (define-key keymap "\C-c\C-t\C-x"    'texinfo-quit-job)
  (define-key keymap "\C-c\C-t\C-l"    'tex-recenter-output-buffer)
  (define-key keymap "\C-c\C-t\C-d"    'texinfo-delete-from-print-queue)
  (define-key keymap "\C-c\C-t\C-q"    'tex-show-print-queue)
  (define-key keymap "\C-c\C-t\C-p"    'texinfo-tex-print)
  (define-key keymap "\C-c\C-t\C-v"    'texinfo-tex-view)
  (define-key keymap "\C-c\C-t\C-i"    'texinfo-texindex)

  (define-key keymap "\C-c\C-t\C-r"    'texinfo-tex-region)
  (define-key keymap "\C-c\C-t\C-b"    'texinfo-tex-buffer))

;; Mode documentation displays commands in reverse order
;; from how they are listed in the texinfo-mode-map.

(if texinfo-mode-map
    nil
  (setq texinfo-mode-map (make-sparse-keymap))

  ;; bindings for `texnfo-tex.el'
  (texinfo-define-common-keys texinfo-mode-map)

  (define-key texinfo-mode-map "\"" 'texinfo-insert-quote)

  ;; bindings for `makeinfo.el'
  (define-key texinfo-mode-map "\C-c\C-m\C-k" 'kill-compilation)
  (define-key texinfo-mode-map "\C-c\C-m\C-l"
    'makeinfo-recenter-compilation-buffer)
  (define-key texinfo-mode-map "\C-c\C-m\C-r" 'makeinfo-region)
  (define-key texinfo-mode-map "\C-c\C-m\C-b" 'makeinfo-buffer)

  ;; bindings for `texinfmt.el'
  (define-key texinfo-mode-map "\C-c\C-e\C-r"    'texinfo-format-region)
  (define-key texinfo-mode-map "\C-c\C-e\C-b"    'texinfo-format-buffer)

  ;; AUCTeX-like bindings
  (define-key texinfo-mode-map "\e\r"		'texinfo-insert-@item)

  ;; bindings for updating nodes and menus

  (define-key texinfo-mode-map "\C-c\C-um"   'texinfo-master-menu)

  (define-key texinfo-mode-map "\C-c\C-u\C-m"   'texinfo-make-menu)
  (define-key texinfo-mode-map "\C-c\C-u\C-n"   'texinfo-update-node)
  (define-key texinfo-mode-map "\C-c\C-u\C-e"   'texinfo-every-node-update)
  (define-key texinfo-mode-map "\C-c\C-u\C-a"   'texinfo-all-menus-update)

  (define-key texinfo-mode-map "\C-c\C-s"     'texinfo-show-structure)

  (define-key texinfo-mode-map "\C-c}"          'up-list)
  (define-key texinfo-mode-map "\C-c]"          'up-list)
  (define-key texinfo-mode-map "\C-c{"		'texinfo-insert-braces)

  ;; bindings for inserting strings
  (define-key texinfo-mode-map "\C-c\C-o"     'texinfo-insert-block)
  (define-key texinfo-mode-map "\C-c\C-c\C-d" 'texinfo-start-menu-description)
  (define-key texinfo-mode-map "\C-c\C-c\C-s" 'texinfo-insert-@strong)
  (define-key texinfo-mode-map "\C-c\C-c\C-e" 'texinfo-insert-@emph)

  (define-key texinfo-mode-map "\C-c\C-cv"    'texinfo-insert-@var)
  (define-key texinfo-mode-map "\C-c\C-cu"    'texinfo-insert-@uref)
  (define-key texinfo-mode-map "\C-c\C-ct"    'texinfo-insert-@table)
  (define-key texinfo-mode-map "\C-c\C-cs"    'texinfo-insert-@samp)
  (define-key texinfo-mode-map "\C-c\C-cq"    'texinfo-insert-@quotation)
  (define-key texinfo-mode-map "\C-c\C-co"    'texinfo-insert-@noindent)
  (define-key texinfo-mode-map "\C-c\C-cn"    'texinfo-insert-@node)
  (define-key texinfo-mode-map "\C-c\C-cm"    'texinfo-insert-@email)
  (define-key texinfo-mode-map "\C-c\C-ck"    'texinfo-insert-@kbd)
  (define-key texinfo-mode-map "\C-c\C-ci"    'texinfo-insert-@item)
  (define-key texinfo-mode-map "\C-c\C-cf"    'texinfo-insert-@file)
  (define-key texinfo-mode-map "\C-c\C-cx"    'texinfo-insert-@example)
  (define-key texinfo-mode-map "\C-c\C-ce"    'texinfo-insert-@end)
  (define-key texinfo-mode-map "\C-c\C-cd"    'texinfo-insert-@dfn)
  (define-key texinfo-mode-map "\C-c\C-cc"    'texinfo-insert-@code))

(easy-menu-define texinfo-mode-menu
  texinfo-mode-map
  "Menu used for `texinfo-mode'."
  '("Texinfo"
    ["Insert block"	texinfo-insert-block	t]
    ;; ["Insert node"	texinfo-insert-@node	t]
    "----"
    ["Update All"	(lambda () (interactive) (texinfo-master-menu t))
     :keys "\\[universal-argument] \\[texinfo-master-menu]"]
    ["Update every node" texinfo-every-node-update t]
    ["Update node"	texinfo-update-node	t]
    ["Make Master menu"	texinfo-master-menu	t]
    ["Make menu"	texinfo-make-menu	t]
    ["Update all menus"	texinfo-all-menus-update t]
    "----"
    ["Show structure"	texinfo-show-structure	t]
    ["Format region"	texinfo-format-region	t]
    ["Format buffer"	texinfo-format-buffer	t]
    ["Makeinfo region"	makeinfo-region		t]
    ["Makeinfo buffer"	makeinfo-buffer		t]))


(defun texinfo-filter (section list)
  (let (res)
    (dolist (x list) (if (eq section (cadr x)) (push (car x) res)))
    res))

(defvar texinfo-chapter-level-regexp
  (regexp-opt (texinfo-filter 2 texinfo-section-list))
  "Regular expression matching just the Texinfo chapter level headings.")

;;; Texinfo mode

;;;###autoload
(define-derived-mode texinfo-mode text-mode "Texinfo"
  "Major mode for editing Texinfo files.

  It has these extra commands:
\\{texinfo-mode-map}

  These are files that are used as input for TeX to make printed manuals
and also to be turned into Info files with \\[makeinfo-buffer] or
the `makeinfo' program.  These files must be written in a very restricted and
modified version of TeX input format.

  Editing commands are like text-mode except that the syntax table is
set up so expression commands skip Texinfo bracket groups.  To see
what the Info version of a region of the Texinfo file will look like,
use \\[makeinfo-region], which runs `makeinfo' on the current region.

  You can show the structure of a Texinfo file with \\[texinfo-show-structure].
This command shows the structure of a Texinfo file by listing the
lines with the @-sign commands for @chapter, @section, and the like.
These lines are displayed in another window called the *Occur* window.
In that window, you can position the cursor over one of the lines and
use \\[occur-mode-goto-occurrence], to jump to the corresponding spot
in the Texinfo file.

  In addition, Texinfo mode provides commands that insert various
frequently used @-sign commands into the buffer.  You can use these
commands to save keystrokes.  And you can insert balanced braces with
\\[texinfo-insert-braces] and later use the command \\[up-list] to
move forward past the closing brace.

Also, Texinfo mode provides functions for automatically creating or
updating menus and node pointers.  These functions

  * insert the `Next', `Previous' and `Up' pointers of a node,
  * insert or update the menu for a section, and
  * create a master menu for a Texinfo source file.

Here are the functions:

    texinfo-update-node                \\[texinfo-update-node]
    texinfo-every-node-update          \\[texinfo-every-node-update]
    texinfo-sequential-node-update

    texinfo-make-menu                  \\[texinfo-make-menu]
    texinfo-all-menus-update           \\[texinfo-all-menus-update]
    texinfo-master-menu

    texinfo-indent-menu-description (column &optional region-p)

The `texinfo-column-for-description' variable specifies the column to
which menu descriptions are indented.

Passed an argument (a prefix argument, if interactive), the
`texinfo-update-node' and `texinfo-make-menu' functions do their jobs
in the region.

To use the updating commands, you must structure your Texinfo file
hierarchically, such that each `@node' line, with the exception of the
Top node, is accompanied by some kind of section line, such as an
`@chapter' or `@section' line.

If the file has a `top' node, it must be called `top' or `Top' and
be the first node in the file.

Entering Texinfo mode calls the value of `text-mode-hook', and then the
value of `texinfo-mode-hook'."
  (set (make-local-variable 'page-delimiter)
       (concat
	"^@node [ \t]*[Tt]op\\|^@\\("
	texinfo-chapter-level-regexp
	"\\)\\>"))
  (make-local-variable 'require-final-newline)
  (setq require-final-newline mode-require-final-newline)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	(concat "\b\\|@[a-zA-Z]*[ \n]\\|" paragraph-separate))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "\b\\|@[a-zA-Z]*[ \n]\\|" paragraph-start))
  (make-local-variable 'sentence-end-base)
  (setq sentence-end-base
	"\\(@\\(end\\)?dots{}\\|[.?!]\\)[]\"'$B!I$,1r}(B)}]*")
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'fill-column)
  (setq fill-column 70)
  (make-local-variable 'comment-start)
  (setq comment-start "@c ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "@c +\\|@comment +")
  (make-local-variable 'words-include-escapes)
  (setq words-include-escapes t)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression texinfo-imenu-generic-expression)
  (setq imenu-case-fold-search nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(texinfo-font-lock-keywords nil nil nil backward-paragraph
				     (font-lock-syntactic-keywords
				      . texinfo-font-lock-syntactic-keywords)))
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  ;; Outline settings.
  (set (make-local-variable 'outline-heading-alist)
       ;; We should merge outline-heading-alist and texinfo-section-list
       ;; but in the mean time, let's just generate one from the other.
       (mapcar (lambda (x) (cons (concat "@" (car x)) (cadr x)))
	       texinfo-section-list))
  (set (make-local-variable 'outline-regexp)
       (concat (regexp-opt (mapcar 'car outline-heading-alist) t)
	       "\\>"))

  (make-local-variable 'tex-start-of-header)
  (setq tex-start-of-header "%\\*\\*start")
  (make-local-variable 'tex-end-of-header)
  (setq tex-end-of-header "%\\*\\*end")
  (make-local-variable 'tex-first-line-header-regexp)
  (setq tex-first-line-header-regexp "^\\\\input")
  (make-local-variable 'tex-trailer)
  (setq tex-trailer "@bye\n")

  ;; Prevent filling certain lines, in addition to ones specified
  ;; by the user.
  (let ((prevent-filling "^@\\(def\\|multitable\\)"))
    (set (make-local-variable 'auto-fill-inhibit-regexp)
	 (if (null auto-fill-inhibit-regexp)
	     prevent-filling
	   (concat auto-fill-inhibit-regexp "\\|" prevent-filling)))))



;;; Insert string commands

(defvar texinfo-block-default "example")

(define-skeleton texinfo-insert-block
  "Create a matching pair @<cmd> .. @end <cmd> at point.
Puts point on a blank line between them."
  (setq texinfo-block-default
	(completing-read (format "Block name [%s]: " texinfo-block-default)
			 texinfo-environments
			 nil nil nil nil texinfo-block-default))
  \n "@" str \n _ \n "@end " str \n)

(defun texinfo-inside-macro-p (macro &optional bound)
  "Non-nil if inside a macro matching the regexp MACRO."
  (condition-case nil
      (save-excursion
	(save-restriction
	  (narrow-to-region bound (point))
	  (while (progn
		   (up-list -1)
		   (not (condition-case nil
			    (save-excursion
			      (backward-sexp 1)
			      (looking-at macro))
			  (scan-error nil)))))
	  t))
    (scan-error nil)))

(defun texinfo-inside-env-p (env &optional bound)
  "Non-nil if inside an environment matching the regexp @ENV."
  (save-excursion
    (and (re-search-backward (concat "@\\(end\\s +\\)?" env) bound t)
	 (not (match-end 1)))))

(defvar texinfo-enable-quote-macros "@\\(code\\|samp\\|kbd\\)\\>")
(defvar texinfo-enable-quote-envs '("example\\>" "lisp\\>"))
(defun texinfo-insert-quote (&optional arg)
  "Insert the appropriate quote mark for Texinfo.
Usually inserts the value of `texinfo-open-quote' (normally ``) or
`texinfo-close-quote' (normally ''), depending on the context.
With prefix argument or inside @code or @example, inserts a plain \"."
  (interactive "*P")
  (let ((top (or (save-excursion (re-search-backward "@node\\>" nil t))
		 (point-min))))
    (if (or arg
	    (= (preceding-char) ?\\)
	    (save-excursion
	      (backward-char (length texinfo-open-quote))
	      (when (or (looking-at texinfo-open-quote)
			(looking-at texinfo-close-quote))
		(delete-char (length texinfo-open-quote))
		t))
	    (texinfo-inside-macro-p texinfo-enable-quote-macros top)
	    (let ((in-env nil))
	      (dolist (env texinfo-enable-quote-envs in-env)
		(if (texinfo-inside-env-p env top)
		    (setq in-env t)))))
	(self-insert-command (prefix-numeric-value arg))
      (insert
       (if (memq (char-syntax (preceding-char)) '(?\( ?> ?\s))
	   texinfo-open-quote
	 texinfo-close-quote)))))

;; The following texinfo-insert-@end command not only inserts a SPC
;; after the @end, but tries to find out what belongs there.  It is
;; not very smart: it does not understand nested lists.

(defun texinfo-last-unended-begin ()
  (while (and (re-search-backward texinfo-environment-regexp)
	      (looking-at "@end"))
    (texinfo-last-unended-begin)))

(defun texinfo-next-unmatched-end ()
  (while (and (re-search-forward texinfo-environment-regexp)
	      (save-excursion
		(goto-char (match-beginning 0))
		(not (looking-at "@end"))))
    (texinfo-next-unmatched-end)))

(defun texinfo-insert-@end ()
  "Insert the matching `@end' for the last Texinfo command that needs one."
  (interactive)
  (let ((string
	 (ignore-errors
	   (save-excursion
	     (texinfo-last-unended-begin)
	     (match-string 1)))))
    (insert "@end ")
    (if string (insert string "\n"))))

;; The following insert commands accept a prefix arg N, which is the
;; number of words (actually s-exprs) that should be surrounded by
;; braces.  Thus you can first paste a variable name into a .texinfo
;; buffer, then say C-u 1 C-c C-c v at the beginning of the just
;; pasted variable name to put @var{...} *around* the variable name.
;; Operate on previous word or words with negative arg.

;; These commands use texinfo-insert-@-with-arg
(defun texinfo-insert-@-with-arg (string &optional arg)
  (if arg
      (progn
        (setq arg (prefix-numeric-value arg))
        (if (< arg 0)
            (progn
              (skip-chars-backward " \t\n\r\f")
              (save-excursion
                (forward-sexp arg)
                (insert "@" string "{"))
              (insert "}"))
          (skip-chars-forward " \t\n\r\f")
          (insert "@" string "{")
          (forward-sexp arg)
          (insert "}")))
    (insert "@" string "{}")
    (backward-char)))

(defun texinfo-insert-braces ()
  "Make a pair of braces and be poised to type inside of them.
Use \\[up-list] to move forward out of the braces."
  (interactive)
  (insert "{}")
  (backward-char))

(defun texinfo-insert-@code (&optional arg)
  "Insert a `@code{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "code" arg))

(defun texinfo-insert-@dfn (&optional arg)
  "Insert a `@dfn{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "dfn" arg))

(defun texinfo-insert-@email (&optional arg)
  "Insert a `@email{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "email" arg))

(defun texinfo-insert-@emph (&optional arg)
  "Insert a `@emph{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "emph" arg))

(defun texinfo-insert-@example ()
  "Insert the string `@example' in a Texinfo buffer."
  (interactive)
  (insert "@example\n"))

(defun texinfo-insert-@file (&optional arg)
  "Insert a `@file{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "file" arg))

(defun texinfo-insert-@item ()
  "Insert the string `@item' in a Texinfo buffer.
If in a table defined by @table, follow said string with a space.
Otherwise, follow with a newline."
  (interactive)
  (insert "@item"
	  (if (equal (ignore-errors
		      (save-excursion
			(texinfo-last-unended-begin)
			(match-string 1)))
		     "table")
	      ?\s
	    ?\n)))

(defun texinfo-insert-@kbd (&optional arg)
  "Insert a `@kbd{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "kbd" arg))

(defun texinfo-insert-@node ()
  "Insert the string `@node' in a Texinfo buffer.
Insert a comment on the following line indicating the order of
arguments to @node.  Insert a carriage return after the comment line.
Leave point after `@node'."
  (interactive)
  (insert "@node \n@comment  node-name,  next,  previous,  up\n")
  (forward-line -2)
  (forward-char 6))

(defun texinfo-insert-@noindent ()
  "Insert the string `@noindent' in a Texinfo buffer."
  (interactive)
  (insert "@noindent\n"))

(defun texinfo-insert-@quotation ()
  "Insert the string `@quotation' in a Texinfo buffer."
  (interactive)
  (insert "@quotation\n"))

(defun texinfo-insert-@samp (&optional arg)
  "Insert a `@samp{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "samp" arg))

(defun texinfo-insert-@strong (&optional arg)
  "Insert a `@strong{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "strong" arg))

(defun texinfo-insert-@table ()
  "Insert the string `@table' in a Texinfo buffer."
  (interactive)
  (insert "@table "))

(defun texinfo-insert-@var (&optional arg)
  "Insert a `@var{}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "var" arg))

(defun texinfo-insert-@uref (&optional arg)
  "Insert a `@uref{}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "uref" arg))
(defalias 'texinfo-insert-@url 'texinfo-insert-@uref)

;;; Texinfo file structure

(defun texinfo-show-structure (&optional nodes-too)
  "Show the structure of a Texinfo file.
List the lines in the file that begin with the @-sign commands for
@chapter, @section, and the like.

With optional argument (prefix if interactive), list both the lines
with @-sign commands for @chapter, @section, and the like, and list
@node lines.

Lines with structuring commands beginning in them are displayed in
another buffer named `*Occur*'.  In that buffer, you can move point to
one of those lines and then use
\\<occur-mode-map>\\[occur-mode-goto-occurrence],
to jump to the corresponding spot in the Texinfo source file."

  (interactive "P")
  ;; First, remember current location
  (let (current-location)
    (save-excursion
      (end-of-line)            ; so as to find section on current line
      (if (re-search-backward
           ;; do not require `texinfo-section-types-regexp' in texnfo-upd.el
           "^@\\(chapter \\|sect\\|subs\\|subh\\|unnum\\|major\\|chapheading \\|heading \\|appendix\\)"
           nil t)
          (setq current-location
                (progn
                  (beginning-of-line)
                  (buffer-substring (point) (progn (end-of-line) (point)))))
        ;; else point is located before any section command.
        (setq current-location "tex")))
    ;; Second, create and format an *Occur* buffer
    (save-excursion
      (goto-char (point-min))
      (occur (concat "^\\(?:" (if nodes-too "@node\\>\\|")
		     outline-regexp "\\)")))
    (pop-to-buffer "*Occur*")
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (flush-lines "-----")
      ;; Now format the "*Occur*" buffer to show the structure.
      ;; Thanks to ceder@signum.se (Per Cederqvist)
      (goto-char (point-max))
      (let (level)
        (while (re-search-backward "^ *[0-9]*:@\\(\\sw+\\)" nil 0)
          (goto-char (1- (match-beginning 1)))
          (setq level
                (or (cadr (assoc (match-string 1) texinfo-section-list)) 2))
          (indent-to-column (+ (current-column) (* 4 (- level 2))))
          (beginning-of-line))))
    ;; Third, go to line corresponding to location in source file
    ;; potential bug: two exactly similar `current-location' lines ...
    (goto-char (point-min))
    (re-search-forward current-location nil t)
    (beginning-of-line)
    ))


;;; The  tex  and  print  function definitions:

(defcustom texinfo-texi2dvi-command "texi2dvi"
  "*Command used by `texinfo-tex-buffer' to run TeX and texindex on a buffer."
  :type 'string
  :group 'texinfo)

(defcustom texinfo-tex-command "tex"
  "*Command used by `texinfo-tex-region' to run TeX on a region."
  :type 'string
  :group 'texinfo)

(defcustom texinfo-texindex-command "texindex"
  "*Command used by `texinfo-texindex' to sort unsorted index files."
  :type 'string
  :group 'texinfo)

(defcustom texinfo-delete-from-print-queue-command "lprm"
  "*Command string used to delete a job from the line printer queue.
Command is used by \\[texinfo-delete-from-print-queue] based on
number provided by a previous \\[tex-show-print-queue]
command."
  :type 'string
  :group 'texinfo)

(defvar texinfo-tex-trailer "@bye"
  "String appended after a region sent to TeX by `texinfo-tex-region'.")

(defun texinfo-tex-region (beg end)
  "Run TeX on the current region.
This works by writing a temporary file (`tex-zap-file') in the directory
that is the value of `tex-directory', then running TeX on that file.

The first line of the buffer is copied to the
temporary file; and if the buffer has a header, it is written to the
temporary file before the region itself.  The buffer's header is all lines
between the strings defined by `tex-start-of-header' and `tex-end-of-header'
inclusive.  The header must start in the first 100 lines.

The value of `texinfo-tex-trailer' is appended to the temporary file after the region."
  (interactive "r")
  (require 'tex-mode)
  (let ((tex-command texinfo-tex-command)
	(tex-trailer texinfo-tex-trailer))
    (tex-region beg end)))

(defun texinfo-tex-buffer ()
  "Run TeX on visited file, once or twice, to make a correct `.dvi' file."
  (interactive)
  (require 'tex-mode)
  (let ((tex-command texinfo-texi2dvi-command)
	;; Disable tex-start-options-string.  texi2dvi would not
	;; understand anything specified here.
	(tex-start-options-string ""))
    (tex-buffer)))

(defun texinfo-texindex ()
  "Run `texindex' on unsorted index files.
The index files are made by \\[texinfo-tex-region] or \\[texinfo-tex-buffer].
This runs the shell command defined by `texinfo-texindex-command'."
  (interactive)
  (require 'tex-mode)
  (tex-send-command texinfo-texindex-command (concat tex-zap-file ".??"))
  ;; alternatively
  ;; (send-string "tex-shell"
  ;;              (concat texinfo-texindex-command
  ;;                      " " tex-zap-file ".??" "\n"))
  (tex-recenter-output-buffer nil))

(defun texinfo-tex-print ()
  "Print `.dvi' file made by \\[texinfo-tex-region] or \\[texinfo-tex-buffer].
This runs the shell command defined by `tex-dvi-print-command'."
  (interactive)
  (require 'tex-mode)
  (tex-print))

(defun texinfo-tex-view ()
  "View `.dvi' file made by \\[texinfo-tex-region] or \\[texinfo-tex-buffer].
This runs the shell command defined by `tex-dvi-view-command'."
  (interactive)
  (require 'tex-mode)
  (tex-view))

(defun texinfo-quit-job ()
  "Quit currently running TeX job, by sending an `x' to it."
  (interactive)
  (if (not (get-process "tex-shell"))
      (error "No TeX shell running"))
  (tex-send-command "x"))
;; alternatively:
;; save-excursion
;;   (set-buffer (get-buffer "*tex-shell*"))
;;   (goto-char (point-max))
;;   (insert "x")
;;   (comint-send-input)

(defun texinfo-delete-from-print-queue (job-number)
  "Delete job from the line printer spooling queue.
You are prompted for the job number (use a number shown by a previous
\\[tex-show-print-queue] command)."
  (interactive "nPrinter job number for deletion: ")
  (require 'tex-mode)
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (tex-send-command texinfo-delete-from-print-queue-command job-number)
  ;; alternatively
  ;; (send-string "tex-shell"
  ;;              (concat
  ;;               texinfo-delete-from-print-queue-command
  ;;               " "
  ;;               job-number"\n"))
  (tex-recenter-output-buffer nil))

(provide 'texinfo)

;; arch-tag: 005d7c38-43b9-4b7d-aa1d-aea69bae73e1
;;; texinfo.el ends here
