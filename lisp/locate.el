;;; locate.el --- interface to the locate command

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Peter Breton <pbreton@i-kinetics.com>

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

;; Search a database of files and use dired commands on
;; the result. 
;;

;;;;; Building a database of files ;;;;;;;;;
;; 
;; You can create a simple files database with a port of the Unix find command
;; and one of the various Windows NT various scheduling utilities,
;; for example the AT command from the NT Resource Kit, WinCron which is
;; included with Microsoft FrontPage, or the shareware NTCron program.
;;
;; To set up a function which searches the files database, do something
;; like this:
;; 
;; (defvar locate-fcodes-file       (concat my-home "/fcodes"))
;; (defvar locate-make-command-line 'nt-locate-make-command-line)
;; 
;; (defun nt-locate-make-command-line (arg)
;;  (cons "grep"
;;        (mapconcat 'identity
;; 		  (list "-i" arg locate-fcodes-file)
;; 		  " ")))
;;
;;;;;;;; ADVICE For dired-make-relative: ;;;;;;;;;
;; 
;; For certain dired commands to work right, you should also include the
;; following in your _emacs/.emacs:
;; 
;; (defadvice dired-make-relative (before set-no-error activate)
;;   "For locate mode and Windows, don't return errors"
;;   (if (and (eq   major-mode  'locate-mode)
;; 	   (memq system-type (list 'windows-nt 'ms-dos)))
;;       (ad-set-arg 2 t)
;;     ))
;;
;; Otherwise, dired-make-relative will give error messages like
;; "FILENAME: not in directory tree growing at /"

;;; Commentary:
;;
;; Locate.el provides an interface to a program which searches a
;; database of file names. By default, this program is the GNU locate
;; command, but it could also be the BSD-style find command, or even a
;; user specified command.
;;
;; To use the BSD-style "fast find", or any other shell command of the
;; form 
;;
;;   SHELLPROGRAM  Name-to-find
;;
;; set the variable locate-command in your .emacs file.
;;
;;   To use a more complicated expression, create a function which 
;; takes a string (the name to find) as input and returns a cons
;; pair: the car should be the command to be executed, the cdr
;; should be the arguments, concatenated into a string (including
;; the name to find). Then do
;;
;; (setq locate-make-command-line 'my-locate-command-line) 
;;
;; in your .emacs, using the name of your function in place of
;; my-locate-command-line
;;
;; You should make sure that whichever command you use works correctly
;; from a shell prompt. GNU locate and BSD find expect the file databases
;; to either be in standard places or located via environment variables.
;; If the latter, make sure these environment variables are set in
;; your emacs process
;;
;; Locate-mode assumes that each line output from the locate-command
;; consists exactly of a file name, possibly preceded or trailed by
;; whitespace. If your file database has other information on the line (for
;; example, the file size), you will need to redefine the function 
;; locate-get-file-positions to return a list consisting of the first
;; character in the file name and the last character in the file name.
;;
;; To use locate-mode, simply type M-x locate and then the string
;; you wish to find. You can use almost all of the dired commands in
;; the resulting *Locate* buffer.  It is worth noting that your commands
;; do not, of course, affect the file database. For example, if you
;; compress a file in the locate buffer, the actual file will be
;; compressed, but the entry in the file database will not be
;; affected. Consequently, the database and the filesystem will be out
;; of sync until the next time the database is updated
;;
;; The command locate-with-filter keeps only lines matching a
;; regular expression; this is often useful to constrain a big search.
;;

;;; Code:

(eval-when-compile
  (require 'dired))

;; Variables
(defvar locate-command "locate"
  "*The executable program used to search a database of files.")

(defvar locate-history-list nil
  "The history list used by the \\[locate] command.")

(defvar locate-make-command-line 'locate-default-make-command-line
  "*Function used to create the locate command line.")

(defvar locate-buffer-name "*Locate*"
  "*Name of the buffer to show results from the \\[locate] command.")

(defvar locate-fcodes-file nil
  "*Database of filenames.")

(defvar locate-mouse-face 'highlight
  "*Face used to highlight locate entries.")

(defvar locate-header-face 'region
  "*Face used to highlight the locate header.")

(defvar locate-current-filter nil)

;; Functions

(defun locate-default-make-command-line (search-string)
  (cons locate-command search-string))

;;;### autoload
(defun locate (search-string &optional filter)
  "Run the \\[locate] command, putting results in `*Locate*' buffer."
  (interactive
      (list (read-from-minibuffer "Locate: " nil nil
		  nil 'locate-history-list)))
  (let* ((pop-up-windows 1)
	 (locate-cmd-list (funcall locate-make-command-line search-string))
	 (locate-cmd (car locate-cmd-list))
	 (locate-cmd-args (cdr locate-cmd-list))
	 (locate-proc)
	)
    
    ;; Find the Locate buffer
    (if (not (string-equal (buffer-name) locate-buffer-name))
       (switch-to-buffer-other-window locate-buffer-name))

    (locate-mode)
    (erase-buffer)

    (setq locate-current-filter filter)

    (call-process locate-cmd nil t nil locate-cmd-args)
    (if filter
	(locate-filter-output filter))

    (locate-do-setup)
 )
)

;;;### autoload
(defun locate-with-filter (search-string filter)
  "Run the locate command with a filter."
  (interactive
      (list (read-from-minibuffer "Locate: " nil nil
		  nil 'locate-history-list)
            (read-from-minibuffer "Filter: " nil nil
		  nil 'grep-history)))
  (locate search-string filter))

(defun locate-filter-output (filter)
  "Filter output from the locate command."
  (goto-char (point-min))
  (delete-non-matching-lines (regexp-quote filter)))

(defvar locate-mode-map nil
  "Local keymap for Locate mode buffers.")
(if locate-mode-map
    nil

   (require 'dired)

   (setq locate-mode-map (copy-keymap dired-mode-map))

   ;; Undefine Useless Dired Menu bars
   (define-key locate-mode-map [menu-bar Dired]   'undefined)
   (define-key locate-mode-map [menu-bar subdir]  'undefined)

   (define-key locate-mode-map [menu-bar mark executables] 'undefined)
   (define-key locate-mode-map [menu-bar mark directory]   'undefined)
   (define-key locate-mode-map [menu-bar mark directories] 'undefined)
   (define-key locate-mode-map [menu-bar mark symlinks]    'undefined)

   (define-key locate-mode-map [mouse-2] 'mouse-locate-view-file)
   (define-key locate-mode-map "\C-ct"   'locate-tags)

   (define-key locate-mode-map "U"       'dired-unmark-all-files-no-query)
)

;; This variable is used to indent the lines and then to search for
;; the file name
(defconst locate-filename-indentation 4
 "The amount of indentation for each file.")

(defun locate-get-file-positions ()
  (save-excursion
     (end-of-line)
     (let ((eol (point)))
       (beginning-of-line)

       ;; Assumes names end at the end of the line
       (forward-char locate-filename-indentation)
       (list (point) eol))))

;; From SQL-mode
(defun current-line ()
  "Return the current line number, as an integer."
  (interactive)
  (+ (count-lines (point-min) (point))
     (if (eq (current-column) 0)
	 1
       0)))

(defun locate-get-filename ()
  (let ((pos    (locate-get-file-positions))
	(lineno (current-line)))
    (and (not (eq lineno 1)) 
	 (not (eq lineno 2)) 
	 (buffer-substring (elt pos 0) (elt pos 1)))))

(defun mouse-locate-view-file (event)
  "In Locate mode, view a file, using the mouse."
  (interactive "@e") 
  (save-excursion
    (goto-char (posn-point (event-start event)))
      (view-file (locate-get-filename))))

;; Define a mode for locate
;; Default directory is set to "/" so that dired commands, which
;; expect to be in a tree, will work properly
(defun locate-mode ()
  "Major mode for the `*Locate*' buffer made by \\[locate]."
  (kill-all-local-variables)
  (use-local-map             locate-mode-map)
  (setq major-mode          'locate-mode
        mode-name           "Locate"
        default-directory   "/"
	dired-subdir-alist  (list (cons "/" (point-min-marker))))
  (make-local-variable 'dired-move-to-filename-regexp)
  (setq dired-move-to-filename-regexp
	(make-string locate-filename-indentation ?\ ))
  (make-local-variable 'dired-actual-switches)
  (setq dired-actual-switches "")
  (make-local-variable 'dired-permission-flags-regexp)
  (setq dired-permission-flags-regexp "^\\(    \\)")
  (run-hooks 'locate-mode-hook))

(defun locate-do-setup ()
  (let ((search-string (car locate-history-list)))
    (goto-char (point-min))
    (save-excursion
      
      ;; Nothing returned from locate command?
      (if (eobp)
	  (progn
	    (kill-buffer locate-buffer-name)
	    (delete-window)
	    (if locate-current-filter
		(error "Locate: no match for %s in database using filter %s"
		       search-string locate-current-filter)
	    (error "Locate: no match for %s in database" search-string))))

      (locate-insert-header search-string)
      
      (while (not (eobp))
	(insert-char ?\  locate-filename-indentation t)
	(locate-set-properties)
	(forward-line 1)))))

(defun locate-set-properties ()
  (save-excursion
    (let ((pos (locate-get-file-positions)))
      (add-text-properties (elt pos 0) (elt pos 1)
			   (list 'mouse-face locate-mouse-face)))))

(defun locate-insert-header (search-string)
  (let ((locate-format-string "Matches for %s")
	(locate-regexp-match
	 (concat " *Matches for \\(" (regexp-quote search-string) "\\)"))
	(locate-format-args (list search-string))
	)
  
    (if locate-fcodes-file
	(setq locate-format-string
	      (concat locate-format-string " in %s")
	      locate-regexp-match
	      (concat locate-regexp-match
		      " in \\("
		      (regexp-quote locate-fcodes-file)
		      "\\)")
	      locate-format-args
	      (append (list locate-fcodes-file) locate-format-args)))

    (if locate-current-filter
	(setq locate-format-string
	      (concat locate-format-string " using filter %s")
	      locate-regexp-match
	      (concat locate-regexp-match
		      " using filter "
		      "\\("
		      (regexp-quote locate-current-filter)
		      "\\)")
	      locate-format-args
	      (append (list locate-current-filter) locate-format-args)))
	
    (setq locate-format-string
	  (concat locate-format-string ": \n\n")
	  locate-regexp-match
	  (concat locate-regexp-match ": \n"))
    
    (insert (apply 'format locate-format-string (reverse locate-format-args)))
    
    (save-excursion
      (goto-char (point-min))
      (if (not (looking-at locate-regexp-match))
	  nil
	(add-text-properties (match-beginning 1) (match-end 1)
			     (list 'face locate-header-face))
	(and (match-beginning 2)
	     (add-text-properties (match-beginning 2) (match-end 2)
				  (list 'face locate-header-face)))
	(and (match-beginning 3)
	     (add-text-properties (match-beginning 3) (match-end 3)
				  (list 'face locate-header-face)))
	))))

(defun locate-tags ()
  "Visit a tags table in `*Locate*' mode."
  (interactive)
  (let ((tags-table (locate-get-filename)))
   (if (y-or-n-p (format "Visit tags table %s? " tags-table)) 
     (visit-tags-table tags-table)
    nil)))

(provide 'locate)

;;; locate.el ends here
