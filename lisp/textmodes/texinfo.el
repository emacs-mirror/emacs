;;;; texinfo.el ---- major mode for editing Texinfo files.

;; Copyright (C) 1985, 1988, 1989, 1990 Free Software Foundation, Inc.

;; Author: Bob Chassell <bob@gnu.ai.mit.edu>
;; Version: 2.00
;; Keywords: maint, tex, doc, wp

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

;;; Code:

(require 'texnfo-upd)
(require 'tex-mode)
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

(defvar texinfo-mode-map nil)

;; Mode documentation displays commands in reverse order 
;; from how they are listed in the texinfo-mode-map.
(if texinfo-mode-map
    nil
  (setq texinfo-mode-map (make-sparse-keymap))

  (define-key texinfo-mode-map "\C-c\C-t\C-k"    'tex-kill-job)
  (define-key texinfo-mode-map "\C-c\C-t\C-l"    'tex-recenter-output-buffer)
  (define-key texinfo-mode-map "\C-c\C-t\C-q"    'tex-show-print-queue)
  (define-key texinfo-mode-map "\C-c\C-t\C-p"    'texinfo-tex-print)
;;  (define-key texinfo-mode-map "\C-c\C-t\C-i"    'texinfo-texindex)
  (define-key texinfo-mode-map "\C-c\C-t\C-t"    'texinfo-tex-buffer)
  (define-key texinfo-mode-map "\C-c\C-t\C-r"    'texinfo-tex-region)

  (define-key texinfo-mode-map "\C-c\C-i\C-r"    'texinfo-format-region)
  (define-key texinfo-mode-map "\C-c\C-i\C-b"    'texinfo-format-buffer)

  (define-key texinfo-mode-map "\C-c\C-u\C-m"   'texinfo-make-menu)
  (define-key texinfo-mode-map "\C-c\C-u\C-n"   'texinfo-update-node)
  (define-key texinfo-mode-map "\C-c\C-u\C-e"   'texinfo-every-node-update)
  (define-key texinfo-mode-map "\C-c\C-u\C-a"   'texinfo-all-menus-update)

  (define-key texinfo-mode-map "\C-c\C-s"     'texinfo-show-structure)

  (define-key texinfo-mode-map "\""           'tex-insert-quote)
  (define-key texinfo-mode-map "\C-c}"          'up-list)
  (define-key texinfo-mode-map "\C-c{"          'texinfo-insert-braces)

  (define-key texinfo-mode-map "\C-c\C-cv"    'texinfo-insert-@var)
  (define-key texinfo-mode-map "\C-c\C-cs"    'texinfo-insert-@samp)
  (define-key texinfo-mode-map "\C-c\C-co"    'texinfo-insert-@noindent)
  (define-key texinfo-mode-map "\C-c\C-cn"    'texinfo-insert-@node)
  (define-key texinfo-mode-map "\C-c\C-ck"    'texinfo-insert-@kbd)
  (define-key texinfo-mode-map "\C-c\C-ci"    'texinfo-insert-@item)
  (define-key texinfo-mode-map "\C-c\C-cx"    'texinfo-insert-@example)
  (define-key texinfo-mode-map "\C-c\C-ce"    'texinfo-insert-@end-example)
  (define-key texinfo-mode-map "\C-c\C-cd"    'texinfo-insert-@dfn)
  (define-key texinfo-mode-map "\C-c\C-cc"    'texinfo-insert-@code))

(defun texinfo-insert-@var ()
  "Insert the string @var in a texinfo buffer."
  (interactive)
  (insert "@var{}")
  (backward-char))

(defun texinfo-insert-@samp ()
  "Insert the string @samp in a texinfo buffer."
  (interactive)
  (insert "@samp{}")
  (backward-char))

(defun texinfo-insert-@noindent ()
  "Insert the string @noindent in a texinfo buffer."
  (interactive)
  (insert "@noindent\n"))

(defun texinfo-insert-@node ()
  "Insert the string @node in a texinfo buffer, 
along with a comment indicating the arguments to @node."
  (interactive)
  (insert "@node \n@comment  node-name,  next,  previous,  up")
  (forward-line -1)
  (forward-char 6))

(defun texinfo-insert-@kbd ()
  "Insert the string @kbd in a texinfo buffer."
  (interactive)
  (insert "@kbd{}")
  (backward-char))

(defun texinfo-insert-@item ()
  "Insert the string @item in a texinfo buffer."
  (interactive)
  (insert "@item")
  (newline))

(defun texinfo-insert-@example ()
  "Insert the string @example in a texinfo buffer."
  (interactive)
  (insert "@example\n"))

(defun texinfo-insert-@end-example ()
  "Insert the string @end example in a texinfo buffer."
  (interactive)
  (insert "@end example\n"))

(defun texinfo-insert-@dfn ()
  "Insert the string @dfn in a texinfo buffer."
  (interactive)
  (insert "@dfn{}")
  (backward-char))

(defun texinfo-insert-@code ()
  "Insert the string @code in a texinfo buffer."
  (interactive)
  (insert "@code{}")
  (backward-char))

(defun texinfo-insert-braces ()
  "Make a pair of braces and be poised to type inside of them.
Use \\[up-list] to move forward out of the braces."
  (interactive)
  (insert "{}")
  (backward-char))

;;;###autoload
(defun texinfo-mode ()
  "Major mode for editing Texinfo files.

  It has these extra commands:
\\{texinfo-mode-map}

  These are files that are used as input for TeX to make printed manuals
and also to be turned into Info files by \\[texinfo-format-buffer] or
`makeinfo'.  These files must be written in a very restricted and
modified version of TeX input format.

  Editing commands are like text-mode except that the syntax table is
set up so expression commands skip Texinfo bracket groups.  To see
what the Info version of a region of the Texinfo file will look like,
use \\[texinfo-format-region].  This command runs Info on the current region
of the Texinfo file and formats it properly.

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
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter texinfo-chapter-level-regexp)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat "^\b\\|^@[a-zA-Z]*[ \n]\\|" paragraph-separate))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^\b\\|^@[a-zA-Z]*[ \n]\\|" paragraph-start))
  (make-local-variable 'fill-column)
  (setq fill-column 72)
  (make-local-variable 'comment-start)
  (setq comment-start "@c ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "@c +")
  (make-local-variable 'words-include-escapes)
  (setq words-include-escapes t)
  (make-local-variable 'tex-start-of-header)
  (setq tex-start-of-header "%**start of header")
  (make-local-variable 'tex-end-of-header)
  (setq tex-end-of-header "%**end of header")
  (make-local-variable 'tex-trailer)
  (setq tex-trailer "@bye\n")
  (run-hooks 'text-mode-hook 'texinfo-mode-hook))


;;; Texinfo file structure

; The following is defined in `texnfo-upd.el'
; (defvar texinfo-section-types-regexp
;   "^@\\(chapter \\|sect\\|sub\\|unnum\\|major\\|heading \\|appendix\\)"
;   "Regexp matching chapter, section, other headings (but not the top node).")

(defun texinfo-show-structure (&optional nodes-too) 
  "Show the structure of a Texinfo file.
List the lines in the file that begin with the @-sign commands for
@chapter, @section, and the like.

With optional argument (prefix if interactive), list both the lines
with @-sign commands for @chapter, @section, and the like, and list
@node lines.

Lines with structuring commands beginning in them are displayed in
another window called the *Occur* window.  In that window, you can
position the cursor over one of the lines and use
\\[occur-mode-goto-occurrence], 
to jump to the corresponding spot in the Texinfo file."

  (interactive "P")
  (save-excursion 
    (goto-char (point-min))
    (if nodes-too
        (occur (concat "\\(^@node\\)\\|" texinfo-section-types-regexp))
      (occur texinfo-section-types-regexp)))
  (pop-to-buffer "*Occur*")
  (goto-char (point-min))
  (flush-lines "-----"))


;;; texinfo mode tex and hardcopy printing commands.

;; These commands are for running tex on a region of a texinfo file in
;; GNU Emacs, or on the whole buffer, and for printing the resulting
;; .dvi file.  The three commands are:

; texinfo-tex-region    to run tex on the current region.
; texinfo-tex-buffer    to run tex on the current buffer.
; texinfo-tex-print     to print the .dvi file made by tex

;;; Other useful functions

; These functions are provided by `tex-mode.el' but are bound to keys
; in texinfo mode.

; tex-kill-job                  to kill the currently running tex job
; tex-recenter-output-buffer    to redisplay tex job output buffer
; tex-show-print-queue          to show the print queue

; Various variables are provided by `tex-mode.el'

; tex mode variable         Default Value 

; tex-dvi-print-command         "lpr -d"
; tex-directory                 "/tmp/"   
; tex-show-queue-command        "lpq"     
; tex-shell-cd-command          "cd"      
; tex-zap-file                  nil   (created  as needed)


;;; The  tex  and  print  function definitions:

(defvar texinfo-tex-command "texi2dvi"
  "*Command used by  texinfo-tex-region  to run tex on a region.")

;;(defvar texinfo-texindex-command "texindex"
;;  "*Command used by  texinfo-texindex  to sort unsorted index files.")

(defun texinfo-tex-region (beg end)
  "Run tex on the current region.
A temporary file (`tex-zap-file') is written in directory `tex-directory', and
tex is run in that directory.  The first line of the file is copied to the
temporary file; and if the buffer has a header, it is written to the
temporary file before the region itself.  The buffer's header is all lines
between the strings defined by `tex-start-of-header' and `tex-end-of-header'
inclusive.  The header must start in the first 100 lines.  The value of
tex-trailer is appended to the temporary file after the region."
  (interactive "r")
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
        (let ((local-tex-trailer tex-trailer))
          (set-buffer temp-buffer)
          (erase-buffer)
          ;; make sure trailer isn't hidden by a comment
          (insert-string "\n")
          (if local-tex-trailer (insert-string local-tex-trailer))
          (set-buffer-directory temp-buffer zap-directory)
          (write-region (point-min) (point-max) tex-out-file t nil))))
    
    (set-buffer-directory "*tex-shell*" zap-directory)
    (send-string "tex-shell" (concat tex-shell-cd-command " "
                                     zap-directory "\n"))
    (send-string "tex-shell" (concat texinfo-tex-command " "
                                     tex-out-file "\n")))
  (tex-recenter-output-buffer 0))

(defun texinfo-tex-buffer ()
  "Run tex on current buffer.  
See \\[texinfo-tex-region] for more information."
  (interactive)
  (texinfo-tex-region (point-min) (point-max)))

;;(defun texinfo-texindex ()
;;  "Run texindex on unsorted index files.
;;The index files are made by \\[texinfo-tex-region] or \\[texinfo-tex-buffer].
;;Runs the shell command defined by `texinfo-texindex-command'."
;;  (interactive)
;;  (send-string "tex-shell"
;;	       (concat texinfo-texindex-command
;;                       " " tex-zap-file ".??" "\n"))
;;  (tex-recenter-output-buffer nil))

(defun texinfo-tex-print ()
  "Print .dvi file made by \\[texinfo-tex-region] or \\[texinfo-tex-buffer].
Runs the shell command defined by `tex-dvi-print-command'."
  (interactive)
  (send-string "tex-shell"
	       (concat tex-dvi-print-command
                       " " tex-zap-file ".dvi" "\n"))
  (tex-recenter-output-buffer nil))

(provide 'texinfo)

;;; texinfo.el ends here
