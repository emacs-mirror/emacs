;;;; texinfo.el--major mode for editing Texinfo files.
;; Copyright (C) 1985, '88, '89, 
;;                '90, '91, '92, '93 Free Software Foundation, Inc.

;;; Author: Robert J. Chassell          
;;; Maintainer: FSF

;;; This file is part of GNU Emacs.

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
menu.  This action depends on the pre-exisitng master menu using the
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

;;; Syntax table

(defvar texinfo-mode-syntax-table nil)

(if texinfo-mode-syntax-table
    nil
  (setq texinfo-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" " " texinfo-mode-syntax-table)
  (modify-syntax-entry ?\\ " " texinfo-mode-syntax-table)
  (modify-syntax-entry ?@ "\\" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\^q "\\" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" texinfo-mode-syntax-table)
  (modify-syntax-entry ?{ "(}" texinfo-mode-syntax-table)
  (modify-syntax-entry ?} "){" texinfo-mode-syntax-table)
  (modify-syntax-entry ?\' "w" texinfo-mode-syntax-table))

(defvar texinfo-font-lock-keywords
  (list
   "@\\(@\\|[^}\t \n{]+\\)"					;commands
   '("^\\(@c\\|@comment\\)[ \t].*$" . font-lock-comment-face)	;comments
   '("^\\(*.*\\)[\t ]*$" 1 font-lock-function-name-face t)	;menu items
   '("@\\(emph\\|strong\\|b\\|i\\){\\([^}]+\\)" 2 font-lock-comment-face t)
   '("@\\(file\\|kbd\\|key\\){\\([^}]+\\)" 2 font-lock-string-face t)
   '("@\\(samp\\|code\\|var\\){\\([^}]+\\)" 2 font-lock-function-name-face t)
   '("@\\(xref\\|pxref\\){\\([^}]+\\)" 2 font-lock-keyword-face t)
   '("@end *\\([a-zA-Z0-9]+\\)[ \t]*$" 1 font-lock-function-name-face t)
   '("@item \\(.*\\)$" 1 font-lock-function-name-face t)
   '("\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
   )
  "Additional expressions to highlight in TeXinfo mode.")

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

  ;; bindings for `makeinfo.el'
  (define-key texinfo-mode-map "\C-c\C-m\C-k" 'kill-compilation)
  (define-key texinfo-mode-map "\C-c\C-m\C-l" 
    'makeinfo-recenter-compilation-buffer)
  (define-key texinfo-mode-map "\C-c\C-m\C-r" 'makeinfo-region)
  (define-key texinfo-mode-map "\C-c\C-m\C-b" 'makeinfo-buffer)

  ; Bindings for texinfmt.el.
  (define-key texinfo-mode-map "\C-c\C-e\C-r"    'texinfo-format-region)
  (define-key texinfo-mode-map "\C-c\C-e\C-b"    'texinfo-format-buffer)

  ;; bindings for updating nodes and menus

  (define-key texinfo-mode-map "\C-c\C-um"   'texinfo-master-menu)

  (define-key texinfo-mode-map "\C-c\C-u\C-m"   'texinfo-make-menu)
  (define-key texinfo-mode-map "\C-c\C-u\C-n"   'texinfo-update-node)
  (define-key texinfo-mode-map "\C-c\C-u\C-e"   'texinfo-every-node-update)
  (define-key texinfo-mode-map "\C-c\C-u\C-a"   'texinfo-all-menus-update)

  (define-key texinfo-mode-map "\C-c\C-s"     'texinfo-show-structure)

  (define-key texinfo-mode-map "\C-c}"    	'up-list)
  (define-key texinfo-mode-map "\C-c{"    	'texinfo-insert-braces)

  ;; bindings for inserting strings

  (define-key texinfo-mode-map "\C-c\C-c\C-d" 'texinfo-start-menu-description)

  (define-key texinfo-mode-map "\C-c\C-cv"    'texinfo-insert-@var)
  (define-key texinfo-mode-map "\C-c\C-ct"    'texinfo-insert-@table)
  (define-key texinfo-mode-map "\C-c\C-cs"    'texinfo-insert-@samp)
  (define-key texinfo-mode-map "\C-c\C-co"    'texinfo-insert-@noindent)
  (define-key texinfo-mode-map "\C-c\C-cn"    'texinfo-insert-@node)
  (define-key texinfo-mode-map "\C-c\C-ck"    'texinfo-insert-@kbd)
  (define-key texinfo-mode-map "\C-c\C-ci"    'texinfo-insert-@item)
  (define-key texinfo-mode-map "\C-c\C-cf"    'texinfo-insert-@file)
  (define-key texinfo-mode-map "\C-c\C-cx"    'texinfo-insert-@example)
  (define-key texinfo-mode-map "\C-c\C-ce"    'texinfo-insert-@end)
  (define-key texinfo-mode-map "\C-c\C-cd"    'texinfo-insert-@dfn)
  (define-key texinfo-mode-map "\C-c\C-cc"    'texinfo-insert-@code))


;;; Texinfo mode

(defvar texinfo-chapter-level-regexp 
  "chapter\\|unnumbered \\|appendix \\|majorheading\\|chapheading"
  "Regular expression matching Texinfo chapter-level headings.
This does not match `@node' and does not match the `@top' command.")

;;;###autoload
(defun texinfo-mode ()
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

Entering Texinfo mode calls the value of text-mode-hook, and then the
value of texinfo-mode-hook."
  (interactive)
  (text-mode)
  (setq mode-name "Texinfo")
  (setq major-mode 'texinfo-mode)
  (use-local-map texinfo-mode-map)
  (set-syntax-table texinfo-mode-syntax-table)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter 
        (concat 
         "^@node [ \t]*[Tt]op\\|^@\\(" 
         texinfo-chapter-level-regexp 
         "\\)"))
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat "\b\\|@[a-zA-Z]*[ \n]\\|" paragraph-separate))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "\b\\|@[a-zA-Z]*[ \n]\\|" paragraph-start))
  (make-local-variable 'fill-column)
  (setq fill-column 72)
  (make-local-variable 'comment-start)
  (setq comment-start "@c ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "@c +")
  (make-local-variable 'words-include-escapes)
  (setq words-include-escapes t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(texinfo-font-lock-keywords))
  (make-local-variable 'tex-start-of-header)
  (setq tex-start-of-header "%**start")
  (make-local-variable 'tex-end-of-header)
  (setq tex-end-of-header "%**end")
  (run-hooks 'text-mode-hook 'texinfo-mode-hook))


;;; Insert string commands

(defconst texinfo-environment-regexp
  "^@\\(f?table\\|enumerate\\|itemize\\|ifinfo\\|iftex\\|ifset\\|ifclear\
\\|example\\|quotation\\|lisp\\|smallexample\\|smalllisp\\|display\\|format\
\\|flushleft\\|flushright\\|ignore\\|group\\|tex\\|cartouche\\|menu\
\\|titlepage\\|end\\|def[a-z]*[a-wyz]\\>\\)"
  "Regexp for environment-like Texinfo list commands.
Subexpression 1 is what goes into the corresponding `@end' statement.")

(defun texinfo-insert-@end ()
  "Insert the matching `@end' for the last Texinfo command that needs one."
  (interactive)
  (let ((depth 1) string)
    (save-excursion
      (while (and (> depth 0)
		  (re-search-backward texinfo-environment-regexp nil t)
	(if (looking-at "@end")
	    (setq depth (1+ depth))
	  (setq depth (1- depth)))))
      (looking-at texinfo-environment-regexp)
      (if (zerop depth)
	  (setq string
		(buffer-substring (match-beginning 1)
				  (match-end 1)))))
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
  "Insert the string `@item' in a Texinfo buffer."
  (interactive)
  (insert "@item")
  (newline))

(defun texinfo-insert-@kbd (&optional arg)
  "Insert a `@kbd{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "kbd" arg))

(defun texinfo-insert-@node ()
  "Insert the string `@node' in a Texinfo buffer.
This also inserts on the following line a comment indicating
the order of arguments to @node."
  (interactive)
  (insert "@node \n@comment  node-name,  next,  previous,  up")
  (forward-line -1)
  (forward-char 6))

(defun texinfo-insert-@noindent ()
  "Insert the string `@noindent' in a Texinfo buffer."
  (interactive)
  (insert "@noindent\n"))

(defun texinfo-insert-@samp (&optional arg)
  "Insert a `@samp{...}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "samp" arg))

(defun texinfo-insert-@table (&optional arg)
  "Insert the string `@table' in a Texinfo buffer."
  (interactive "P")
  (insert "@table "))

(defun texinfo-insert-@var (&optional arg)
  "Insert a `@var{}' command in a Texinfo buffer.
A numeric argument says how many words the braces should surround.
The default is not to surround any existing words with the braces."
  (interactive "P")
  (texinfo-insert-@-with-arg "var" arg))

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
one of those lines and then use \\<occur-mode-map>\\[occur-mode-goto-occurrence], 
to jump to the corresponding spot in the Texinfo source file."

  (interactive "P")
  (require 'texnfo-upd)
  (save-excursion 
    (goto-char (point-min))
    (if nodes-too
        (occur (concat "\\(^@node\\)\\|" texinfo-section-types-regexp))
      (occur texinfo-section-types-regexp)))
  (pop-to-buffer "*Occur*")
  (goto-char (point-min))
  (flush-lines "-----")
  ;; Now format the "*Occur*" buffer to show the structure.
  ;; Thanks to ceder@signum.se (Per Cederqvist)
  (goto-char (point-max))
  (let ((margin 5))
    (while (re-search-backward "^ *[0-9]*:" nil 0)
      (re-search-forward ":")
      (setq margin
            (cond
             ((looking-at
               (concat "@\\(" texinfo-chapter-level-regexp "\\)")) 5)
             ;; ((looking-at "@chapter ") 5)
             ;; ((looking-at "@unnumbered ") 5)
             ;; ((looking-at "@appendix ") 5)
             ;; ((looking-at "@majorheading ") 5)
             ;; ((looking-at "@chapheading ") 5)

             ((looking-at
               (concat "@\\(" texinfo-section-level-regexp "\\)")) 9)
             ;; ((looking-at "@section ") 9)
             ;; ((looking-at "@unnumberedsec ") 9)
             ;; ((looking-at "@appendixsec ") 9)
             ;; ((looking-at "@heading ") 9)

             ((looking-at 
               (concat "@\\(" texinfo-subsection-level-regexp "\\)")) 13)
             ;; ((looking-at "@subsection ") 13)
             ;; ((looking-at "@unnumberedsubsec ") 13)
             ;; ((looking-at "@appendixsubsec ") 13)
             ;; ((looking-at "@subheading ") 13)

             ((looking-at 
               (concat "@\\(" texinfo-subsubsection-level-regexp "\\)")) 17)
             ;; ((looking-at "@subsubsection ") 17)
             ;; ((looking-at "@unnumberedsubsubsec ") 17)
             ;; ((looking-at "@appendixsubsubsec ") 17)
             ;; ((looking-at "@subsubheading ") 17)
             (t margin)))
      (indent-to-column margin)
      (beginning-of-line))))

;;; The  tex  and  print  function definitions:

(defvar texinfo-texi2dvi-command "texi2dvi"
  "*Command used by `texinfo-tex-buffer' to run TeX and texindex on a buffer.")

(defvar texinfo-tex-command "tex"
  "*Command used by `texinfo-tex-region' to run TeX on a region.")

(defvar texinfo-texindex-command "texindex"
  "*Command used by `texinfo-texindex' to sort unsorted index files.")

(defvar texinfo-delete-from-print-queue-command "lprm"
  "*Command string used to delete a job from the line printer queue.
Command is used by \\[texinfo-delete-from-print-queue] based on
number provided by a previous \\[tex-show-print-queue]
command.")

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
  (if (get-buffer "*tex-shell*")
      (tex-kill-job)
    (tex-start-shell))
  (or tex-zap-file (setq tex-zap-file (make-temp-name "#tz")))
  (let ((tex-out-file (concat tex-zap-file ".tex"))
	(temp-buffer (get-buffer-create " tex-Output-Buffer"))
	(zap-directory
         (file-name-as-directory (expand-file-name tex-directory))))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(forward-line 100)
	(let ((search-end (point))
	      (hbeg (point-min)) (hend (point-min))
	      (default-directory zap-directory))
	  (goto-char (point-min))
          
          ;; Copy first line, the `\input texinfo' line, to temp file
	  (write-region (point) 
                        (save-excursion (end-of-line) (point))
                        tex-out-file nil nil)
          
          ;; Don't copy first line twice if region includes it.
          (forward-line 1)
          (if (< beg (point)) (setq beg (point)))
          
          ;; Initialize the temp file with either the header or nothing
          (if (search-forward tex-start-of-header search-end t)
              (progn
                (beginning-of-line)
                (setq hbeg (point))	; Mark beginning of header.
                (if (search-forward tex-end-of-header nil t)
                    (progn (beginning-of-line)
                           (setq hend (point)))	; Mark end of header.
                  (setq hbeg (point-min))))) ; Else no header.
          
          ;; Copy  header  to temp file.
          (write-region (min hbeg beg) hend tex-out-file t nil)
          
          ;; Copy  region  to temp file.
          (write-region (max beg hend) end tex-out-file t nil))
        
        ;; This is  a kludge to insert the tex-trailer into the tex-out-file.
        ;;  We have to create a special buffer in which to insert
        ;;  the tex-trailer first because there is no function with
        ;;  which to append a literal string directly to a file.
        (let ((local-tex-trailer texinfo-tex-trailer))
          (set-buffer temp-buffer)
          (erase-buffer)
          ;; make sure trailer isn't hidden by a comment
          (insert-string "\n")
          (if local-tex-trailer (insert-string local-tex-trailer))
          (tex-set-buffer-directory temp-buffer zap-directory)
          (write-region (point-min) (point-max) tex-out-file t nil))

;;; The following is sufficient in Emacs 19.
;;;	(write-region (concat "\n" texinfo-tex-trailer) nil
;;;		      tex-out-file t nil)
	))
    
    (tex-set-buffer-directory "*tex-shell*" zap-directory)
    (tex-send-command tex-shell-cd-command zap-directory)
    (tex-send-command texinfo-tex-command tex-out-file))
  (tex-recenter-output-buffer 0))

(defun texinfo-tex-buffer ()
  "Run TeX on visited file, once or twice, to make a correct `.dvi' file."
  (interactive)

  ;; Make sure TeX shell is running.
  (require 'tex-mode)
  (if (get-buffer "*tex-shell*")
      (quit-process (get-process "tex-shell") t)
    (tex-start-shell))

  (cond ((null buffer-file-name)
         (error "Buffer not visiting any file!"))
        ((buffer-modified-p)
         (error "Buffer has been modified since last saved!")))

  (setq tex-zap-file buffer-file-name)

  (tex-send-command tex-shell-cd-command (file-name-directory tex-zap-file))

  (tex-send-command texinfo-texi2dvi-command tex-zap-file)

  (tex-recenter-output-buffer 0))

(defun texinfo-texindex ()
  "Run `texindex' on unsorted index files.
The index files are made by \\[texinfo-tex-region] or \\[texinfo-tex-buffer].
This runs the shell command defined by `texinfo-texindex-command'."
  (interactive)
  (require 'tex-mode)
  (tex-send-command texinfo-texindex-command (concat tex-zap-file ".??"))
  (tex-recenter-output-buffer nil))

(defun texinfo-tex-print ()
  "Print `.dvi' file made by \\[texinfo-tex-region] or \\[texinfo-tex-buffer].
This runs the shell command defined by `tex-dvi-print-command'."
  (interactive)
  (require 'tex-mode)
  (tex-send-command tex-dvi-print-command (concat tex-zap-file ".dvi"))
  (tex-recenter-output-buffer nil))

(defun texinfo-quit-job ()
  "Quit currently running TeX job, by sending an `x' to it."
  (interactive)
  (if (not (get-process "tex-shell"))
      (error "No TeX shell running"))
  (tex-send-command "x"))

(defun texinfo-delete-from-print-queue (job-number)
  "Delete job from the line printer spooling queue.
You are prompted for the job number (use a number shown by a previous
\\[texinfo-show-print-queue] command)."
  (interactive "nPrinter job number for deletion: ")
  (require 'tex-mode)
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (tex-send-command texinfo-delete-from-print-queue-command job-number)
  (tex-recenter-output-buffer nil))

(provide 'texinfo)

;;; texinfo.el ends here
