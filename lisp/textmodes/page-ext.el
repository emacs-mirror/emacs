;;; page-ext.el --- page handling commands

;;; Copyright (C) 1990 Free Software Foundation

;; Author: Robert J. Chassell <bob@gnu.ai.mit.edu>

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

;;; You may use these commands to handle an address list or other
;;; small data base.

;;; Change Log:

;;; Change Log ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Version 0.043
;;; 24 May 1990 - When the cursor is at the end of the pages directory
;;; buffer (which is empty), a `C-c C-c' (pages-directory-goto)
;;; command now takes you to the end of the buffer.
;;;
;;; Version 0.042
;;; 16 May 1990 - Since people often handle address and other files
;;; differently, variable `pages-directory-for-addresses-narrowing-p'
;;; now specifies whether `pages-directory-goto' should narrow
;;; addresses buffer to entry to which it goes.
;;; `pages-directory-buffer-narrowing-p' continues to control
;;; narrowing of pages buffer.
;;;
;;; `add-new-page' documentation string now explains
;;; that the value of the inserted page-delimiter is a `^L'.
;;;
;;; `pages-directory-previous-regexp' definition reworded.
;;;
;;; Removed unneeded defvar for `pages-directory-buffer'.
;;;
;;; Version 0.041 
;;; 14 May 1990 - `pages-last-search' bound to nil initially.
;;; Remove unnecessary lines from `search-pages' definition.
;;;
;;; Version 0.04 
;;; 18 Mar 1990 - `pages-directory' creates a directory for only the
;;; accessible portion of the buffer; it does not automatically widen
;;; the buffer.
;;;  
;;; However, `pages-directory-for-addresses' does widen the addresses'
;;; buffer before constructing the addresses' directory.
;;;
;;; Version 0.032
;;; 20 Feb 1990 - `pages-directory-for-addresses' no longer copies
;;; first line of addresses directory to kill-ring
;;;
;;; Remove `(kill-all-local-variables)' line from
;;; `pages-directory-address-mode' so Emacs will not be told to forget
;;; the name of the file containing the addresses!
;;;
;;; Version 0.031
;;; 15 Feb 1990 - `pages-directory-goto' no longer erroneously selects
;;; the entry on the following line when the cursor is at the end of
;;; the line, but selects the entry on which the cursor rests.  
;;;
;;; `pages-directory-address-mode' now sets local variables and enables
;;; `describe-mode' to describe Addresses Directory mode.
;;;
;;; `pages-directory-for-addresses' now sets the buffer-modifed flag
;;; for the Addresses Directory to nil.
;;;
;;; The documentation string for both `pages-directory-mode' and
;;; `pages-directory-address-mode' now provide a lookup for the
;;; `pages-directory-goto' keybinding.
;;;
;;; Version 0.03
;;; 10 Feb 1990 - Incorporated a specialized extension of the
;;; `pages-directory' command called `pages-directory-for-addresses'
;;; and bound it to ctl-x-ctl-p-map "d" for integration with other
;;; page functions.  This function finds a file, creates a directory
;;; for it using the `pages-directory' command, and displays the
;;; directory.  It is primarily for lists of addresses and the like.
;;;
;;; The difference between this and the `pages-directory' command is
;;; that the `pages-directory-for-addresses' command presumes a
;;; default addresses file (although you may optionally specify a file
;;; name) and it switches you to the directory for the file, but the
;;; `pages-directory' command creates a directory for the current
;;; buffer, and pops to the directory in another window.
;;;
;;; `pages-directory' now places the cursor over the header line of
;;; the page in which point was located in the pages buffer.
;;;
;;; New `set-page-delimiter' command sets  the buffer local value of
;;; the page-delimiter variable.  With prefix arg, resets function to
;;; original value.  (Quicker to use than `edit-options'.)
;;;
;;; Version 0.02
;;; 9 Feb 1990 - `pages-directory' now displays the
;;; first line that contains a non-blank character that follows the
;;; `page-delimiter'; this may be the rest of the line that contains
;;; the `page-delimiter' or a line following.  (In most instances, the
;;; line containing a non-blank character is a line of text.)
;;; Modification includes changes to `pages-copy-header-and-position'.
;;;
;;; Each directory created by `pages-directory' now possesses a name
;;; derived on the name of the pages buffer.  Consequently, you may
;;; create several different directories, one for each pages buffer.
;;;
;;; `sort-pages-in-region' no longers requires the text to start on
;;; the line immediately following the line containing the
;;; page-delimiter.
;;;
;;; `pages-directory-goto' no longer narrows to the page
;;; automatically.  Instead, if you wish it to narrow to the page, set
;;; variable pages-directory-buffer-narrowing-p to a non-nil value.
;;; Default is nil; this is an experiment to see whether it is useful
;;; to see the surrounding context.
;;;
;;; Version 0.011
;;; 2 Feb 1990 - `add-new-page': removed extraneous space.
;;;
;;; Version 0.01 
;;; 28 Jan 1990 - Initial definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;; Summary

; The current page commands are:

;     forward-page         C-x ]
;     backward-page        C-x [
;     narrow-to-page       C-x p
;     count-lines-page     C-x l
;     mark-page            C-x C-p  (change this to C-x C-p C-m)
;     sort-pages           not bound
;     what-page            not bound

; The new page handling commands all use `C-x C-p' as a prefix.  This
; means that the key binding for `mark-page' must be changed.
; Otherwise, no other changes are made to the current commands or
; their bindings.

; New page handling commands:

;     next-page                        C-x C-p C-n
;     previous-page                    C-x C-p C-p
;     search-pages                     C-x C-p C-s
;     add-new-page                     C-x C-p C-a
;     sort-pages-buffer                C-x C-p s      
;     set-page-delimiter               C-x C-p C-l
;     pages-directory                  C-x C-p C-d   
;     pages-directory-for-addresses    C-x C-p d
;         goto-page                    C-c C-c


;;;; Using the page commands
; 
; The page commands are helpful in several different contexts.  For
; example, programmers often divide source files into sections using the
; `page-delimiter'; you can use the `pages-directory' command to list
; the sections.

; You may change the buffer local value of the `page-delimiter' with
; the `set-page-delimiter' command.  This command is bound to `C-x C-p
; C-l' The command prompts you for a new value for the page-delimiter.
; Called with a prefix-arg, the command resets the value of the
; page-delimiter to its original value.


;;;; Handling an address list or small data base

; You may use the page commands to handle an address list or other
; small data base.  Put each address or entry on its own page.  The
; first line of text in each page is a `header line' and is listed by
; the `pages-directory' or `pages-directory-for-addresses' command.

; Specifically:
; 
;   1. Begin each entry with a `page-delimiter' (which is, by default,
;      `^L' at the beginning of the line).
; 
;   2. The first line of text in each entry is the `heading line'; it
;      will appear in the pages-directory-buffer which is constructed
;      using the `C-x C-p C-d' (pages-directory) command or the `C-x
;      C-p d' (pages-directory-for-addresses) command.
; 
;      The heading line may be on the same line as the page-delimiter
;      or it may follow after.  It is the first non-blank line on the
;      page.  Conventionally, the heading line is placed on the line
;      immediately following the line containing page-delimiter.
;
;   3. Follow the heading line with the body of the entry.  The body
;      extends up to the next `page-delimiter'.  The body may be of any
;      length.  It is conventional to place a blank line after the last
;      line of the body.

; For example, a file might look like this:
; 
;     FSF
;     Free Software Foundation
;     675 Massachusetts Avenue
;     Cambridge, MA 02139  USA
;     (617) 876-3296
;     gnu@prep.ai.mit.edu
; 
;     
;     House Subcommittee on Intellectual Property,
;     U.S. House of Representatives,
;     Washington, DC  20515
;     
;     Congressional committee concerned with permitting or preventing
;     monopolistic restictions on the use of software technology
; 
;     
;     George Lakoff
;     ``Women, Fire, and Dangerous Things:
;     What Categories Reveal about the Mind''
;     1987, Univ. of Chicago Press
; 
;     About philosophy, Whorfian effects, and linguistics.
; 
;      
;     OBI   (On line text collection.)
;     Open Book Initiative
;     c/o Software Tool & Die
;     1330 Beacon St, Brookline, MA 02146 USA
;     (617) 739-0202 
;     obi@world.std.com

; In this example, the heading lines are:
;
;     FSF
;     House Subcommittee on Intellectual Property
;     George Lakoff
;     OBI (On line text collection.)

; The `C-x C-p s' (sort-pages-buffer) command sorts the entries in the
; buffer alphabetically.

; You may use any of the page commands, including the `next-page',
; `previous-page', `add-new-page', `mark-page', and `search-pages'
; commands.

; You may use either the `C-x C-p d' (pages-directory-for-addresses)
; or the `C-x C-p C-d' (pages-directory) command to construct and
; dislay a directory of all the heading lines.

; In the directory, you may position the cursor over a heading line
; and type `C-c C-c' to go to the entry to which it refers in the
; pages buffer.

; When used in conjunction with the `pages-directory-for-addresses'
; command, the `C-c C-c' (pages-directory-goto) command narrows to the
; entry to which it goes.  But, when used in conjunction with the
; `pages-directory' command, the `C-c C-c' (pages-directory-goto)
; command does not narrow to the entry, but widens the buffer so you
; can see the context surrounding the entry.

; If you wish, you may create several different directories,
; one for each different buffer.

;; `pages-directory-for-addresses' in detail

; The `pages-directory-for-addresses' assumes a default addresses
; file.  You do not need to specify the addresses file but merely type
; `C-x C-p d' from any buffer.  The command finds the file, constructs
; a directory for it, and switches you to the directory.  If you call
; the command with a prefix arg, `C-u C-x C-p d', it prompts you for a
; file name.

;; `pages-directory' in detail

; Call the `pages-directory' from the buffer for which you want a
; directory created; it creates a directory for the buffer and pops
; you to the directory.

; The `pages-directory' command has several options:

;   Called with a prefix arg, `C-u C-x C-p C-d', the `pages-directory'
;   prompts you for a regular expression and only lists only those
;   header lines that are part of pages that contain matches to the
;   regexp.  In the example above, `C-u C-x C-p C-d 617 RET' would
;   match the telephone area code of the first and fourth entries, so
;   only the header lines of those two entries would appear in the
;   pages-directory-buffer.
; 
;   Called with a numeric argument, the `pages-directory' command
;   lists the number of lines in each page.  This is helpful when you
;   are printing hardcopy.  

;   Called with a negative numeric argument, the `pages-directory'
;   command lists the lengths of pages whose contents match a regexp.


;;;; Key bindings for page handling functions

(global-unset-key "\C-x\C-p")

(defvar ctl-x-ctl-p-map (make-sparse-keymap)
  "Keymap for subcommands of C-x C-p, which are for page handling.")

(define-key ctl-x-map "\C-p" 'ctl-x-ctl-p-prefix)
(fset 'ctl-x-ctl-p-prefix ctl-x-ctl-p-map)

(define-key ctl-x-ctl-p-map "\C-n" 'next-page)
(define-key ctl-x-ctl-p-map "\C-p" 'previous-page)
(define-key ctl-x-ctl-p-map "\C-a" 'add-new-page)
(define-key ctl-x-ctl-p-map "\C-m" 'mark-page)
(define-key ctl-x-ctl-p-map "\C-s" 'search-pages)
(define-key ctl-x-ctl-p-map "s"    'sort-pages-buffer)
(define-key ctl-x-ctl-p-map "\C-l" 'set-page-delimiter)
(define-key ctl-x-ctl-p-map "\C-d" 'pages-directory)
(define-key ctl-x-ctl-p-map "d"    'pages-directory-for-addresses)


;;;; Page movement function definitions

(defun next-page (&optional count)
  "Move to the next page bounded by the `page-delimiter' variable.
With arg (prefix if interactive), move that many pages."
  (interactive "p")
  (or count (setq count 1))
  (widen)
  ;; Cannot use forward-page because of problems at page boundaries.
  (while (and (> count 0) (not (eobp)))
    (if (re-search-forward page-delimiter nil t)
        nil
      (goto-char (point-max)))
    (setq count (1- count)))
  ;; If COUNT is negative, we want to go back -COUNT + 1 page boundaries.
  ;; The first page boundary we reach is the top of the current page,
  ;; which doesn't count.
  (while (and (< count 1) (not (bobp)))
    (if (re-search-backward page-delimiter nil t)
	(goto-char (match-beginning 0))
      (goto-char (point-min)))
    (setq count (1+ count)))
  (narrow-to-page)
  (goto-char (point-min))
  (recenter 0))

(defun previous-page (&optional count)
  "Move to the previous page bounded by the `page-delimiter' variable.
With arg (prefix if interactive), move that many pages."
  (interactive "p")
  (or count (setq count 1))
  (next-page (- count)))


;;;; Adding and searching pages

(defun add-new-page (header-line)
  "Insert new page at point; prompt for header line.
Page begins with a `^L' as the page-delimiter.  
Point is left in the body of page."
  (interactive "sHeader line: ")
  (widen)
  (insert (format "\n\n%s\n\n" header-line))
  ;; don't renarrow; stay unnarrowed to see context
  (forward-line -1))

(defvar pages-last-search nil
  "Value of last regexp searched for.  Initially, nil.")

(defun search-pages (regexp)
  "Search for REGEXP, starting from point, and narrow to page it is in."
  (interactive (list
                (read-string
                 (format "Search for `%s' (end with RET): "
                         (or pages-last-search "regexp")))))
  (if (equal regexp "")
      (setq regexp pages-last-search)
    (setq pages-last-search regexp))
  (widen)
  (re-search-forward regexp)
  (narrow-to-page))


;;;; Sorting pages

(autoload 'sort-subr "sort" "Primary function for sorting." t nil)

(defun sort-pages-in-region (reverse beg end)
  "Sort pages in region alphabetically.  Prefix arg means reverse order.  

Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort)."

;;; This sort function handles ends of pages differently than
;;; `sort-pages' and works better with lists of addresses and similar
;;; files.

  (interactive "P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    ;;; `sort-subr' takes three arguments
    (sort-subr reverse

               ;; NEXTRECFUN is called with point at the end of the
               ;; previous record. It moves point to the start of the
               ;; next record.
	       (function (lambda ()
                           (re-search-forward page-delimiter nil t)
                           (skip-chars-forward " \t\n")
                           ))

               ;; ENDRECFUN is is called with point within the record.
               ;; It should move point to the end of the record.
	       (function (lambda ()
                           (if (re-search-forward
                                page-delimiter
                                nil
                                t)
                               (goto-char (match-beginning 0))
                             (goto-char (point-max))))))))

(defun sort-pages-buffer (&optional reverse)
  "Sort pages alphabetically in buffer.  Prefix arg means reverse order. 
\(Non-nil arg if not interactive.\)"

  (interactive "P")
  (or reverse (setq reverse nil))
  (widen)
  (let ((beginning (point-min))
        (end (point-max)))
    (sort-pages-in-region reverse beginning end)))


;;;; Pages directory ancillary definitions

(defvar pages-directory-buffer-narrowing-p nil
  "*If non-nil, `pages-directory-goto' narrows pages buffer to entry.")

(defvar pages-directory-previous-regexp nil
  "Value of previous regexp used by `pages-directory'.
\(This regular expression may be used to select only those pages that
contain matches to the regexp.\)")

(defvar pages-buffer nil
  "The buffer for which the pages-directory function creates the directory.")

(defvar pages-directory-prefix "*Directory for:"
  "Prefix of name of temporary buffer for pages-directory.")

(defvar pages-pos-list nil
  "List containing the positions of the pages in the pages-buffer.")

(defvar pages-directory-map nil
  "Keymap for the pages-directory-buffer.")

(if pages-directory-map
    ()
  (setq pages-directory-map (make-sparse-keymap))
  (define-key pages-directory-map "\C-c\C-c"
    'pages-directory-goto))

(defun set-page-delimiter (regexp reset-p)
  "Set buffer local value of page-delimiter to REGEXP.
Called interactively with a prefix argument, reset `page-delimiter' to
its original value.

In a program, non-nil second arg causes first arg to be ignored and
resets the page-delimiter to the original value."
  
  (interactive
   (if current-prefix-arg
       (list original-page-delimiter nil)
     (list (read-string "Set page-delimiter to regexp: " page-delimiter)
           nil)))
  (make-local-variable 'original-page-delimiter)
  (make-local-variable 'page-delimiter)
  (setq original-page-delimiter
        (or original-page-delimiter page-delimiter))
  (if (not reset-p)
      (setq page-delimiter regexp)
    (setq page-delimiter original-page-delimiter))
  (if (interactive-p)
      (message "The value of `page-delimiter' is now: %s" page-delimiter)))


;;;; Pages directory main definitions

(defun pages-directory
  (pages-list-all-headers-p count-lines-p &optional regexp)
  "Display a directory of the page headers in a temporary buffer.
A header is the first non-blank line after the page-delimiter.
\\[pages-directory-mode]
You may move point to one of the lines in the temporary buffer,
then use \\<pages-directory-goto> to go to the same line in the pages buffer.

In interactive use:

    1. With no prefix arg, display all headers.

    2. With prefix arg, display the headers of only those pages that
       contain matches to a regular expression for which you are
       prompted.

    3. With numeric prefix arg, for every page, print the number of
       lines within each page.

    4. With negative numeric prefix arg, for only those pages that
       match a regular expression, print the number of lines within
       each page.

When called from a program, non-nil first arg means list all headers;
non-nil second arg means print numbers of lines in each page; if first
arg is nil, optional third arg is regular expression.

If the buffer is narrowed, the `pages-directory' command creates a
directory for only the accessible portion of the buffer."
  
  (interactive
   (cond ((not current-prefix-arg)
          (list t nil nil))
         ((listp current-prefix-arg) 
          (list nil
                nil
                (read-string
                 (format "Select according to `%s' (end with RET): "
                         (or pages-directory-previous-regexp "regexp")))))
         ((> (prefix-numeric-value current-prefix-arg) 0)
          (list t t nil))
         ((< (prefix-numeric-value current-prefix-arg) 0)          
          (list nil
                t
                (read-string
                 (format "Select according to `%s' (end with RET): "
                         (or pages-directory-previous-regexp "regexp")))))))
  
  (if (equal regexp "")
      (setq regexp pages-directory-previous-regexp)
    (setq pages-directory-previous-regexp regexp))
  
  (if (interactive-p)
      (message "Creating directory for: %s "
               (buffer-name)))
  
  (let ((buffer (current-buffer))
        (pages-directory-buffer
         (concat pages-directory-prefix " " (buffer-name) " "))
        (linenum 1)
        (pages-buffer-original-position (point))
        (pages-buffer-original-page 0))
    
    ;; `with-output-to-temp-buffer' binds the value of the variable
    ;; `standard-output' to the buffer named as its first argument,
    ;; but does not switch to that buffer.
    (with-output-to-temp-buffer pages-directory-buffer
      (save-excursion
        (set-buffer standard-output)
        (pages-directory-mode)
        (insert
         "==== Pages Directory: use `C-c C-c' to go to page under cursor. ====" ?\n)
        (setq pages-buffer buffer)
        (setq pages-pos-list nil))
      
      (if pages-list-all-headers-p
          
          ;; 1. If no prefix argument, list all headers
          (save-excursion
            (goto-char (point-min))
            
            ;; (a) Point is at beginning of buffer; but the first
            ;;     page may not begin with a page-delimiter
            (save-restriction
              ;; If page delimiter is at beginning of buffer, skip it
              (if (and (save-excursion
                         (re-search-forward page-delimiter nil t))
                       (= 1 (match-beginning 0)))
                  (goto-char (match-end 0)))
              (narrow-to-page)
              (pages-copy-header-and-position count-lines-p))
            
            ;; (b) Search within pages buffer for next page-delimiter
            (while (re-search-forward page-delimiter nil t)
              (pages-copy-header-and-position count-lines-p)))
        
        ;; 2. Else list headers whose pages match regexp.
        (save-excursion
          ;; REMOVED  save-restriction  AND  widen  FROM HERE
          (goto-char (point-min))
          
          ;; (a) Handle first page
          (save-restriction
            (narrow-to-page)
            ;; search for selection regexp
            (if (save-excursion (re-search-forward regexp nil t))
                (pages-copy-header-and-position count-lines-p)))
          
          ;; (b) Search for next page-delimiter
          (while (re-search-forward page-delimiter nil t)
            (save-restriction
              (narrow-to-page)
              ;; search for selection regexp
              (if (save-excursion (re-search-forward regexp nil t))
                  (pages-copy-header-and-position count-lines-p)
                )))))
      
      (set-buffer standard-output)
      ;; Put positions in increasing order to go with buffer.
      (setq pages-pos-list (nreverse pages-pos-list))
      (if (interactive-p)
          (message "%d matching lines in: %s"
                   (length pages-pos-list) (buffer-name buffer))))
    (pop-to-buffer pages-directory-buffer)
    (sit-for 0)  ; otherwise forward-line fails if N > window height.
    (forward-line (if (= 0 pages-buffer-original-page)
                      1
                    pages-buffer-original-page))))

(defun pages-copy-header-and-position (count-lines-p)
  "Copy page header and its position to the Pages Directory.
Only arg non-nil, count lines in page and insert before header.
Used by `pages-directory' function."
  
  (let (position line-count)

    (if count-lines-p
        (save-excursion
          (save-restriction
            (narrow-to-page)
            (setq line-count (count-lines (point-min) (point-max))))))

    ;; Keep track of page for later cursor positioning
    (if (<= (point) pages-buffer-original-position)
        (setq pages-buffer-original-page
              (1+ pages-buffer-original-page)))
    
    (save-excursion
      ;; go to first non-blank char after the page-delimiter
      (skip-chars-forward " \t\n")     
      ;; set the marker here; this the place to which the
      ;; `pages-directory-goto' command will go 
      (setq position (make-marker))
      (set-marker position (point))
      (let ((start (point))
            (end (save-excursion (end-of-line) (point))))
        ;; change to directory buffer
        (set-buffer standard-output)
        ;; record page position 
        (setq pages-pos-list (cons position pages-pos-list))
        ;; insert page header
        (insert-buffer-substring buffer start end))
      
      (if count-lines-p
          (save-excursion
            (beginning-of-line)
            (insert (format "%3d: " line-count))))
      
      (terpri))
    (forward-line 1)))

(defun pages-directory-mode ()
  "Mode for handling the pages-directory buffer.

Move point to one of the lines in this buffer, then use \\[pages-directory-goto] to go
to the same line in the pages buffer."

  (kill-all-local-variables)
  (use-local-map pages-directory-map)
  (setq major-mode 'pages-directory-mode)
  (setq mode-name "Pages-Directory")
  (make-local-variable 'pages-buffer)
  (make-local-variable 'pages-pos-list)
  (make-local-variable 'pages-directory-buffer-narrowing-p))

(defun pages-directory-goto ()
  "Go to the corresponding line in the pages buffer."

;;; This function is mostly a copy of `occur-mode-goto-occurrence'

  (interactive)
  (if (or (not pages-buffer)
	  (not (buffer-name pages-buffer)))
      (progn
	(setq pages-buffer nil
	      pages-pos-list nil)
	(error "Buffer in which pages were found is deleted.")))
  (beginning-of-line)
  (let* ((pages-number (1- (count-lines (point-min) (point))))
	 (pos (nth pages-number pages-pos-list))
         (end-of-directory-p (eobp))
         (narrowing-p  pages-directory-buffer-narrowing-p))
    (pop-to-buffer pages-buffer)
    (widen)
    (if end-of-directory-p
        (goto-char (point-max))
      (goto-char (marker-position pos)))
    (if narrowing-p (narrow-to-page))))


;;;; The `pages-directory-for-addresses' function and ancillary code

(defvar pages-addresses-file-name "~/addresses"
  "*Standard name for file of addresses.  Entries separated by `page-delimiter'.
Used by `pages-directory-for-addresses' function.")

(defvar pages-directory-for-addresses-narrowing-p t
  "*If non-nil, `pages-directory-goto' narrows addresses buffer to entry.")

(defun pages-directory-for-addresses (&optional filename)
  "Find addresses file and display its directory.
By default, create and display directory of `pages-addresses-file-name'.
Optional argument is FILENAME.  In interactive use, with prefix
argument, prompt for file name and provide completion.

Move point to one of the lines in the displayed directory,
then use C-c C-c to go to the same line in the addresses buffer."

  (interactive
   (list (if current-prefix-arg
             (read-file-name "Filename: " pages-addresses-file-name))))

  (if (interactive-p)
      (message "Creating directory for: %s "
               (or filename pages-addresses-file-name)))
  (if (file-exists-p (or filename pages-addresses-file-name))
      (progn
        (set-buffer
         (find-file-noselect
          (expand-file-name
           (or filename pages-addresses-file-name))))
        (widen)
        (pages-directory t nil nil)
        (pages-directory-address-mode)
        (setq pages-directory-buffer-narrowing-p 
              pages-directory-for-addresses-narrowing-p)
        (delete-other-windows)
        (save-excursion
          (goto-char (point-min))
          (delete-region (point) (save-excursion (end-of-line) (point)))
          (insert
           "=== Address List Directory: use `C-c C-c' to go to page under cursor. ===")
          (set-buffer-modified-p nil)
          ))
    (error "No addresses file found!")))

(defun pages-directory-address-mode ()
  "Mode for handling the Addresses Directory buffer.

Move point to one of the lines in this buffer, then use C-c C-c to go
to the same line in the pages buffer."

  (use-local-map pages-directory-map)
  (setq major-mode 'pages-directory-address-mode)
  (setq mode-name "Addresses Directory")
  (make-local-variable 'pages-buffer)
  (make-local-variable 'pages-pos-list)
  (make-local-variable 'pages-directory-buffer-narrowing-p))

;;; page-ext.el ends here

