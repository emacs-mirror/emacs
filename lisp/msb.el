;;; msb.el --- Customizable buffer-selection with multiple menus.
;; Copyright (C) 1993, 1994 Lars Lindberg <Lars.Lindberg@sypro.cap.se>
;;
;; Author: Lars Lindberg <Lars.Lindberg@sypro.cap.se>
;; Created: 8 Oct 1993
;; $Revision: 3.21 $
;; $Date: 1994/12/22 07:58:27 $
;; Keywords: mouse buffer menu 
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; msb|Lars Lindberg|Lars.Lindberg@sypro.cap.se|
;; Choose buffer with the mouse.
;; $Date: 1994/12/22 07:58:27 $|$Revision: 3.21 $|~/packages/msb.el.Z|

;;; Commentary:
;;
;; Purpose of this package:
;;   1. Offer a function for letting the user choose buffer,
;;      not necessarily for switching to it.
;;   2. Make a better mouse-buffer-menu.
;;
;; Installation:
;;   (require 'msb)
;;   Note! You now use msb instead of mouse-buffer-menu.
;;
;;   Now try c-mouse-down-1 (Press <CTRL> and mouse button 1 at the
;;   same time).
;;
;; Customization:
;;   Look at the variable 'msb-menu-cond' for deciding what menus you
;;   want. It's not that hard to customize, despite my not-so-good
;;   doc-string. Feel free to send me a better doc-string.
;;   There are some constants for you to try here:
;;   msb--few-menus
;;   msb--very-many-menus (default)
;;   
;;   Look at the variable 'msb-item-handling-function' for customization
;;   of the appearance of every menu item. Try for instance setting
;;   it to 'msb-alon-item-handler.
;;   
;;   Look at the variable 'msb-item-sort-function' for customization
;;   of sorting the menus. Set it to t for instance, which means no
;;   sorting - you will get latest used buffer first.
;;
;;   Also check out the variable 'msb-display-invisible-buffers-p'

;; Known bugs:
;; - `msb' does not work on a non-X-toolkit Emacs.
;; Future enhancements:
;; - [Mattes] had a suggestion about sorting files by extension.
;;   I (Lars Lindberg) think this case could be solved if msb.el was
;;   rewritten to handle more dynamic splitting.  It's now completely
;;   static, depending on the menu-cond.  If the splitting could also
;;   be done by a user-defined function a lot of cases would be
;;   solved.
;; - [Jim] suggested that the Frame menu became a part of the buffer menu.

;;; Change Log
;; 3.21 22/12-94
;;	Fixed bug that occured in non X-toolkit versions of Emacs.
;;	[Chalupsky] pointed out that a global `save-match-data' is
;;	necessary.
;;	Bug found.  Thanks [kifer].
;; 3.16 20/12-94
;;	Added separators to the menu.  New variable `msb-separator-diff'.
;;	New variable `msb-files-by-directory-sort-key'.
;;	Removed `msb--many-menus.'
;;	Fixed bugs.
;; 3.13 20/12-94
;;	Menu fix for non-X-toolkit Emacsen and new "process"
;;	menu. Thanks [jim].
;;      Bug for 'files-by-type'.
;;	Restored the call to `msb-after-load-hooks'.  Thanks [larry].
;;      Major fixes by [Ake].
;;      Menu-bar buffer-menu now has menu-toggle at top level.
;;  3.6 16/12-94
;;	Added variable `msb-max-file-menu-items'.
;;	Removed a large part of the change log.
;;	Found bug.
;;  3.3 16/12-94
;;      Found bugs.
;;  3.1 16/12-94
;;      Now has two types of menus - "files by directory" and "files
;;      by type".
;;      Added variable `msb-files-by-directory'.
;;      Fixed a number of bugs for older versions.
;;  2.11 16/12-94
;;      Added 'no-multi to msb-menu-cond.
;;      Added possibility to shift the menu leftwards. Thanks [kifer].
;;  2.8 15/12-94
;;      Now aware of earlier versions of Emacs that doesn't have the
;;      function `frame-or-buffer-changed-p' or the variable
;;      `menu-bar-update-hook'.  Thanks [will].
;;  2.7 14/12-94
;;	Better installation.
;;  2.6 14/12-94
;;	Now only makes up the menu when necessary.
;;	Added menu-bar support.
;;	Now handles errors in msb-menu-cond better. Thanks [jaalto].
;;      Added MH-awareness. Thanks [kifer].
;;      Added autoload statements.
;;  2.3 8/12-94
;;  	Now uses RCS version numbering for msb.el version number.
;;  	Submitted this to LCD.

;;; Thanks goes to
;;  [msb] - Mark Brader <msb@sq.com>
;;  [Chalupsky] - Hans Chalupsky <hans@cs.Buffalo.EDU>
;;  [jim] - Jim Berry <m1jhb00@FRB.GOV>
;;  [larry] - Larry Rosenberg <ljr@ictv.com>
;;  [will] - Will Henney <will@astroscu.unam.mx>
;;  [jaalto] - Jari Aalto <jaalto@tre.tele.nokia.fi>
;;  [kifer] - Michael Kifer <kifer@sbkifer.cs.sunysb.edu>
;;  [Gael] - Gael Marziou <gael@gnlab030.grenoble.hp.com>
;;  [Gillespie] - Dave Gillespie <daveg@thymus.synaptics.com>
;;  [Alon] - Alon Albert <alon@milcse.rtsg.mot.com>
;;  [KevinB] - Kevin Broadey, <KevinB@bartley.demon.co.uk>
;;  [Ake] - Ake Stenhof <ake@cadpoint.se>
;;  [RMS] - Richard Stallman <rms@gnu.ai.mit.edu>
;;  [Fisk] - Steve Fisk <fisk@medved.bowdoin.edu>

;;; Code:

(require 'cl)

;;;
;;; Some example constants to be used for 'msb-menu-cond'. See that
;;; variable for more information. Please note that if the condition
;;; returns 'multi, then the buffer can appear in several menus.
;;;
(defconst msb--few-menus
  '(((and (boundp 'server-buffer-clients)
	  server-buffer-clients
	  'multi)
     3030
     "Clients (%d)")
    ((and msb-display-invisible-buffers-p
	  (msb-invisible-buffer-p)
	  'multi)
     3090
     "Invisible buffers (%d)")
    ((eq major-mode 'dired-mode)
     2010
     "Dired (%d)"
     msb-dired-item-handler
     msb-sort-by-directory)
    ((eq major-mode 'Man-mode)
     4090
     "Manuals (%d)")
    ((eq major-mode 'w3-mode)
     4020
     "WWW (%d)")
    ((or (memq major-mode '(rmail-mode vm-summary-mode vm-mode mail-mode))
	 (memq major-mode '(mh-letter-mode
			    mh-show-mode
			    mh-folder-mode))	 
	 (memq major-mode '(gnus-summary-mode
			    news-reply-mode
			    gnus-group-mode
			    gnus-article-mode
			    gnus-kill-file-mode
			    gnus-browse-killed-mode)))
     4010
     "Mail (%d)")
    ((not buffer-file-name)
     4099
     "Buffers (%d)")
    ('no-multi
     1099
     "Files (%d)")))

(defconst msb--very-many-menus
  '(((and (boundp 'server-buffer-clients)
	  server-buffer-clients
	  'multi)
     1010
     "Clients (%d)")
    ((and (boundp 'vc-mode) vc-mode 'multi)
     1020
     "Version Control (%d)")
    ((and buffer-file-name
	  (buffer-modified-p)
	  'multi)
     1030
     "Changed files (%d)")
    ((and (get-buffer-process (current-buffer))
	  'multi)
     1040
     "Processes (%d)")
    ((and msb-display-invisible-buffers-p
	  (msb-invisible-buffer-p)
	  'multi)
     1090
     "Invisible buffers (%d)") 
    ((eq major-mode 'dired-mode)
     2010
     "Dired (%d)"
     ;; Note this different menu-handler
     msb-dired-item-handler
     ;; Also note this item-sorter
     msb-sort-by-directory)
    ((eq major-mode 'Man-mode)
     4030
     "Manuals (%d)")
    ((eq major-mode 'w3-mode)
     4020
     "WWW (%d)")
    ((or (memq major-mode '(rmail-mode vm-summary-mode vm-mode mail-mode))
	 (memq major-mode '(mh-letter-mode
			    mh-show-mode
			    mh-folder-mode))	 
	 (memq major-mode '(gnus-summary-mode
			    news-reply-mode
			    gnus-group-mode
			    gnus-article-mode
			    gnus-kill-file-mode
			    gnus-browse-killed-mode)))
     4010
     "Mail (%d)")
    ;; Catchup for all non-file buffers
    ((and (not buffer-file-name)
	  'no-multi)
     4099
     "Other non-file buffers (%d)")
    ((and (string-match "/\\.[^/]*$" buffer-file-name)
	  'multi)
     3090
     "Hidden Files (%d)")
    ((memq major-mode '(c-mode c++-mode))
     3010
     "C/C++ Files (%d)")
    ((eq major-mode 'emacs-lisp-mode)
     3020
     "Elisp Files (%d)")
    ((eq major-mode 'latex-mode)
     3030
     "LaTex Files (%d)")
    ('no-multi
     3099
     "Other files (%d)")))

;; msb--many-menus is obsolete
(defvar msb--many-menus msb--very-many-menus)

;;;
;;; Customizable variables
;;;

(defvar msb-separator-diff 100
  "*Non-nil means use separators.
The separators will appear between all menus that have a sorting key that differs by this value or more.")

(defvar msb-files-by-directory-sort-key 0
  "*The sort key for files sorted by directory")

(defvar msb-max-menu-items 25
  "*The maximum number of items in a menu.
If this variable is set to 15 for instance, then the 15 latest used
buffer that fits in a certain submenu will appear in that submenu.
Nil means no limit.")

(defvar msb-max-file-menu-items 10
  "*The maximum number of items from different directories.

When the menu is of type 'file by directory', this is the maximum
number of buffers that are clumped togehter from different
directories.

If the value is not a number, then the value 10 is used.")

(defvar msb-most-recently-used-sort-key -1010
  "*Where should the menu with the most recently used buffers be placed?")

(defvar msb-display-most-recently-used t
  "*How many buffers should be in the most-recently-used menu.
No buffers at all if less than 1 or nil.
T means use the value of `msb-max-menu-items' in the way it is defined.")

(defvar msb-most-recently-used-title "Most recently used (%d)"
  "*The title for the most-recently-used menu.")
  
(defvar msb-horizontal-shift-function '(lambda () 0)
  "*Function that specifies a number of pixels by which the top menu should
be shifted leftwards.")

(defvar msb-display-invisible-buffers-p nil
  "*Show invisible buffers or not.
Non-nil means that the buffer menu should include buffers that have
names that starts with a space character.")

(defvar msb-item-handling-function 'msb-item-handler
  "*The appearance of a buffer menu.

The default function to call for handling the appearance of a menu
item. It should take to arguments, BUFFER and MAX-BUFFER-NAME-LENGTH,
where the latter is the max length of all buffer names.
When the function is called, BUFFER is the current buffer.
This function is called for items in the variable 'msb-menu-cond' that
have nil as ITEM-HANDLING-FUNCTION. See 'msb-menu-cond' for more
information.")

(defvar msb-item-sort-function 'msb-sort-by-name
  "*The order of items in a buffer menu.
The default function to call for handling the order of items in a menu
item. This function is called like a sort function. The items
look like (ITEM-NAME . BUFFER).
ITEM-NAME is the name of the item that will appear in the menu.
BUFFER is the buffer, this is not necessarily the current buffer.

Set this to nil or t if you don't want any sorting (faster).")

(defvar msb-files-by-directory nil
  "*Non-nil means that files should be sorted by directory instead of
the groups in msb-menu-cond.")

(defvar msb-menu-cond msb--very-many-menus
  "*List of criterias for splitting the mouse buffer menu.
The elements in the list should be of this type:
 (CONDITION MENU-SORT-KEY MENU-TITLE ITEM-HANDLING-FN ITEM-SORT-FN).

When making the split, the buffers are tested one by one against the
CONDITION, just like a lisp cond: When hitting a true condition, the
other criterias are *not* tested and the buffer name will appear in
the menu with the menu-title corresponding to the true condition.

If the condition returns the symbol 'multi, then the buffer will be
added to this menu *and* tested for other menus too.  If it returns
'no-multi, then the buffer will only be added if it hasn't been added
to any other menu.

During this test, the buffer in question is the current buffer, and
the test is surrounded by calls to `save-excursion' and
`save-match-data'

The categories are sorted by MENU-SORT-KEY. Smaller keys are on
top. nil means don't display this menu.

MENU-TITLE is really a format. If you add %d in it, the %d is replaced
with the number of items in that menu.

ITEM-HANDLING-FN, is optional. If it is supplied and is a
function, than it is used for displaying the items in that particular
buffer menu, otherwise the function pointed out by
'msb-item-handling-function' is used.

ITEM-SORT-FN, is also optional.
If it is not supplied, the function pointed out by
'msb-item-sort-function' is used.
If it is nil, then no sort takes place and the buffers are presented
in least-recently-used order.
If it is t, then no sort takes place and the buffers are presented in
most-recently-used order.
If it is supplied and non-nil and not t than it is used for sorting
the items in that particular buffer menu.

Note1: There should always be a 'catch-all' as last element,
in this list. That is an element like (t TITLE ITEM-HANDLING-FUNCTION).
Note2: A buffer menu appears only if it has at least one buffer in it.
Note3: If you have a CONDITION that can't be evaluated you will get an
error every time you do \\[msb].")

(defvar msb-after-load-hooks nil
  "Hooks to be run after the msb package has been loaded.")

;;;
;;; Internal variables
;;;

;; The last calculated menu.
(defvar msb--last-buffer-menu nil)

;; If this is non-nil, then it is a string that describes the error.
(defvar msb--error nil)

;;;
;;; Some example function to be used for 'msb-item-sort-function'.
;;;
(defun msb-item-handler (buffer &optional maxbuf)
  "Create one string item, concerning BUFFER, for the buffer menu.
The item looks like:
*% <buffer-name>
The '*' appears only if the buffer is marked as modified.
The '%' appears only if the buffer is read-only.
Optional second argument MAXBUF is completely ignored."
  (let ((name (buffer-name))
	(modified (if (buffer-modified-p) "*" " "))
	(read-only (if buffer-read-only "%" " ")))
    (format "%s%s %s" modified read-only name)))


(eval-when-compile (require 'dired))

;; 'dired' can be called with a list of the form (directory file1 file2 ...)
;; which causes 'dired-directory' to be in the same form.
(defun msb--dired-directory ()
  (cond ((stringp dired-directory)
	 (abbreviate-file-name (expand-file-name dired-directory)))
	((consp dired-directory)
	 (abbreviate-file-name (expand-file-name (car dired-directory))))
	(t
	 (error "Unknown type of 'dired-directory' in buffer %s"
		(buffer-name)))))

(defun msb-dired-item-handler (buffer &optional maxbuf)
  "Create one string item, concerning a dired BUFFER, for the buffer menu.
The item looks like:
*% <buffer-name>
The '*' appears only if the buffer is marked as modified.
The '%' appears only if the buffer is read-only.
Optional second argument MAXBUF is completely ignored."
  (let ((name (msb--dired-directory))
	(modified (if (buffer-modified-p) "*" " "))
	(read-only (if buffer-read-only "%" " ")))
    (format "%s%s %s" modified read-only name)))

(defun msb-alon-item-handler (buffer maxbuf)
  "Create one string item for the buffer menu.
The item looks like:
<buffer-name> *%# <file-name>
The '*' appears only if the buffer is marked as modified.
The '%' appears only if the buffer is read-only.
The '#' appears only version control file (SCCS/RCS)."
  (format (format "%%%ds  %%s%%s%%s  %%s" maxbuf)
          (buffer-name buffer)
          (if (buffer-modified-p) "*" " ")
          (if buffer-read-only "%" " ")
          (if (and (boundp 'vc-mode) vc-mode) "#" " ")
          (or buffer-file-name "")))

;;;
;;; Some example function to be used for 'msb-item-handling-function'.
;;;
(defun msb-sort-by-name (item1 item2)
  "Sorts the items depending on their buffer-name
An item look like (NAME . BUFFER)."
  (string-lessp (buffer-name (cdr item1))
		(buffer-name (cdr item2))))


(defun msb-sort-by-directory (item1 item2)
  "Sorts the items depending on their directory. Made for dired.
An item look like (NAME . BUFFER)."
  (string-lessp (save-excursion (set-buffer (cdr item1)) (msb--dired-directory))
		(save-excursion (set-buffer (cdr item2)) (msb--dired-directory))))

;;;
;;; msb
;;;
;;; This function can be used instead of (mouse-buffer-menu EVENT)
;;; function in "mouse.el".
;;; 
(defun msb (event)
  "Pop up several menus of buffers for selection with the mouse.
This command switches buffers in the window that you clicked on, and
selects that window.

See the function 'mouse-select-buffer' and the variable
'msb-menu-cond' for more information about how the menus are split."
  (interactive "e")
  (let ((buffer (mouse-select-buffer event))
	(window (posn-window (event-start event))))
    (cond
     (buffer
      (or (framep window) (select-window window))
      (switch-to-buffer (car (cdr buffer))))))
  nil)

;;;
;;; Some supportive functions
;;;
(defun msb-invisible-buffer-p (&optional buffer)
  "Return t if optional BUFFER is an \"invisible\" buffer.
If the argument is left out or nil, then the current buffer is considered."
  (and (> (length (buffer-name buffer)) 0)
       (eq ?\ (aref (buffer-name buffer) 0))))

;; Strip one hierarcy level from the end of PATH.
(defun msb--strip-path (path)
  (save-match-data
    (if (string-match "\\(.+\\)/[^/]+$" path)
	(substring path (match-beginning 1) (match-end 1))
      "/")))

;; Create an alist with all buffers from LIST that lies under the same
;; directory will be in the same item as the directory string as
;;'((PATH1 . (BUFFER-1 BUFFER-2 ...)) (PATH2 . (BUFFER-K BUFFER-K+1...)) ...)
(defun msb--init-file-alist (list)
  (let ((buffer-alist
	 (sort (mapcan
		(function
		 (lambda (buffer)
		   (let ((file-name (buffer-file-name buffer)))
		     (when file-name
		       (list (cons (msb--strip-path file-name) buffer))))))
		list)
	       (function (lambda (item1 item2)
			   (string< (car item1) (car item2)))))))
    ;; Make alist that looks like
    ;;'((PATH1 . (BUFFER-1 BUFFER-2 ...)) (PATH2 . (BUFFER-K)) ...)
    (let ((path nil)
	  (buffers nil)
	  (result nil))
      (append
       (mapcan (function
	       (lambda (item)
		 (cond
		  ((and path
			msb-max-menu-items
			(< (length buffers) msb-max-menu-items)
			(string= path (car item)))
		   (push (cdr item) buffers)
		   nil)
		  (t
		   (when path
		     (setq result (cons path buffers)))
		   (setq path (car item))
		   (setq buffers (list (cdr item)))
		   (and result (list result))))))
	      buffer-alist)
       (list (cons path buffers))))))

;; Choose file-menu with respect to directory for every buffer in LIST.
(defun msb--choose-file-menu (list)
  (let ((buffer-alist (msb--init-file-alist list))
	(final-list nil)
	(max-clumped-together (if (numberp msb-max-file-menu-items)
				  msb-max-file-menu-items
				10))
	(top-found-p nil)
	(last-path nil)
	first rest path buffers)
    (setq first (car buffer-alist))
    (setq rest (cdr buffer-alist))
    (setq path (car first))
    (setq buffers (cdr first))
    (while rest
      (let ((found-p nil)
	    (tmp-rest rest)
	    new-path item)
	(setq item (car tmp-rest))
	(while (and tmp-rest
		    (<= (length buffers) max-clumped-together)
		    (>= (length (car item)) (length path))
		    (string= path (substring (car item) 0 (length path))))
	  (setq found-p t)
	  (setq buffers (append buffers (cdr item)))
	  (setq tmp-rest (cdr tmp-rest))
	  (setq item (car tmp-rest)))
	(cond
	 ((> (length buffers) max-clumped-together)
	  (setq last-path (car first))
	  (when top-found-p
	    (setq first (cons (concat (car first) "/...")
			      (cdr first)))
	    (setq top-found-p nil))
	  (push first final-list)
	  (setq first (car rest)
		rest (cdr rest))
	  (setq path (car first)
		buffers (cdr first)))
	 (t
	  (when found-p
	    (setq top-found-p t)
	    (setq first (cons path buffers)
		  rest tmp-rest))
	  (setq path (msb--strip-path path)
		buffers (cdr first))
	  (when (and last-path
		     (or (and (>= (length path) (length last-path))
			      (string= last-path
				       (substring path 0 (length last-path))))
			 (and (< (length path) (length last-path))
			      (string= path
				       (substring last-path 0 (length path))))))
			 
	    (when top-found-p
	      (setq first (cons (concat (car first) "/...")
				(cdr first)))
	      (setq top-found-p nil))
	    (push first final-list)
	    (setq first (car rest)
		  rest (cdr rest))
	    (setq path (car first)
		buffers (cdr first)))))))
    (when top-found-p
      (setq first (cons (concat (car first)
				(if (string-match "/$" (car first))
				    "..."
				  "/..."))
			(cdr first)))
      (setq top-found-p nil))
    (push first final-list)
    (nreverse final-list)))

;; Create a vector as:
;; [BUFFER-LIST-VARIABLE CONDITION MENU-SORT-KEY MENU-TITLE ITEM-HANDLER SORTER)
;; from an element in 'msb-menu-cond'. See that variable for a
;; description of it's elements.
(defun msb--create-function-info (menu-cond-elt)
  (let* ((list-symbol (make-symbol "-msb-buffer-list"))
	 (tmp-ih (and (> (length menu-cond-elt) 3)
		      (nth 3 menu-cond-elt)))
	 (item-handler (if (and tmp-ih (fboundp tmp-ih))
			   tmp-ih
			 msb-item-handling-function))
	 (tmp-s (if (> (length menu-cond-elt) 4)
		    (nth 4 menu-cond-elt)
		  msb-item-sort-function))
	 (sorter (if (or (fboundp tmp-s)
			 (null tmp-s)
			 (eq tmp-s 't))
		    tmp-s
		   msb-item-sort-function)))
    (when (< (length menu-cond-elt) 3)
      (error "Wrong format of msb-menu-cond."))
    (when (and (> (length menu-cond-elt) 3)
	       (not (fboundp tmp-ih)))
      (signal 'invalid-function (list tmp-ih)))
    (when (and (> (length menu-cond-elt) 4)
	       tmp-s
	       (not (fboundp tmp-s))
	       (not (eq tmp-s 't)))
      (signal 'invalid-function (list tmp-s)))
    (set list-symbol '())
    (vector list-symbol			;BUFFER-LIST-VARIABLE
	    (nth 0 menu-cond-elt)	;CONDITION
	    (nth 1 menu-cond-elt)	;SORT-KEY
	    (nth 2 menu-cond-elt)	;MENU-TITLE
	    item-handler		;ITEM-HANDLER
	    sorter)			;SORTER
    ))

;; This defsubst is only used in `msb--choose-menu' below.  It was
;; pulled out merely to make the code somewhat clearer. The indention
;; level was too big.
(defsubst msb--collect (function-info-vector)
  (let ((result nil)
	(multi-flag nil)
	function-info-list)
    (setq function-info-list
	  (loop for fi
		across function-info-vector
		if (and (setq result
			      (eval (aref fi 1))) ;Test CONDITION
			(not (and (eq result 'no-multi)
				  multi-flag))
			(progn (when (eq result 'multi)
				 (setq multi-flag t))
			       t)
			(or (not msb-max-menu-items)
			    (< (length (eval (aref fi 0)))
			       msb-max-menu-items)))
		collect fi
		until (and result
			   (not (eq result 'multi)))))
    (when (and (not function-info-list)
	       (not result))
      (error "No catch-all in msb-menu-cond!"))
    function-info-list))

;; Adds BUFFER to the menu depicted by FUNCTION-INFO
;; All side-effects.  Adds an element of type '(BUFFER-TITLE . BUFFER)
;; to the buffer-list variable in function-info.
(defun msb--add-to-menu (buffer function-info max-buffer-name-length)
  (let ((list-symbol (aref function-info 0))) ;BUFFER-LIST-VARIABLE
    ;; Here comes the hairy side-effect!
    (set list-symbol
	 (cons (cons (funcall (aref function-info 4) ;ITEM-HANDLER
			      buffer
			      max-buffer-name-length)
		     buffer)
	       (eval list-symbol)))))
  
;; Selects the appropriate menu for BUFFER.
;; This is all side-effects, folks!
;; This should be optimized.
(defsubst msb--choose-menu (buffer function-info-vector max-buffer-name-length)
  (unless (and (not msb-display-invisible-buffers-p)
	       (msb-invisible-buffer-p buffer))
    (condition-case nil
	(save-excursion
	  (set-buffer buffer)
	  ;; Menu found. Add to this menu
	  (mapc (function
		 (lambda (function-info)
		   (msb--add-to-menu buffer function-info max-buffer-name-length)))
		(msb--collect function-info-vector)))
      (error (unless msb--error
	       (setq msb--error
		     (format
		      "Variable `msb-menu-cond': Error for buffer \"%s\"."
		      (buffer-name buffer)))
	       (error msb--error))))))

;; Return (SORT-KEY TITLE . BUFFER-LIST) or nil if the
;; buffer-list is empty.
(defun msb--create-sort-item (function-info)
  (let ((buffer-list (eval (aref function-info 0))))
    (when buffer-list
      (let ((sorter (aref function-info 5)) ;SORTER
	    (sort-key (aref function-info 2))) ;MENU-SORT-KEY
	(when sort-key
	  (cons sort-key	
		(cons (format (aref function-info 3) ;MENU-TITLE
			      (length buffer-list))
		      (cond
		       ((null sorter)
			buffer-list)
		       ((eq sorter 't)
			(nreverse buffer-list))
		       (t
			(sort buffer-list sorter))))))))))

;; Returns a list on the form ((TITLE . BUFFER-LIST)) for
;; the most recently used buffers.
(defun msb--most-recently-used-menu (max-buffer-name-length)
  (when (and msb-display-most-recently-used
	     (or (not (numberp msb-display-most-recently-used))
		 (> msb-display-most-recently-used 0)))
    (let* ((max-in-menu
	    (if (numberp msb-display-most-recently-used)
		msb-display-most-recently-used
	      msb-max-menu-items))

	   (most-recently-used
	    (loop with n = 0
		  for buffer in (cdr (buffer-list))
		  if (save-excursion
		       (set-buffer buffer)
		       (and (not (msb-invisible-buffer-p))
			    (not (eq major-mode 'dired-mode))))
		  collect (save-excursion
			    (set-buffer buffer)
			    (cons (funcall msb-item-handling-function
					   buffer
					   max-buffer-name-length)
				  buffer))
		  and do (incf n)
		  until (and max-in-menu (>= n max-in-menu)))))
      (cons (if (stringp msb-most-recently-used-title)
		(format msb-most-recently-used-title
			(length most-recently-used))
	      (signal 'wrong-type-argument (list msb-most-recently-used-title)))
	    most-recently-used))))

(defun msb--create-buffer-menu-2 ()
  (let ((max-buffer-name-length 0)
	file-buffers
	function-info-vector)
    ;; Calculate the longest buffer name.
    (mapc
     (function
      (lambda (buffer)
	(if (or msb-display-invisible-buffers-p
		(not (msb-invisible-buffer-p)))
	    (setq max-buffer-name-length
		  (max max-buffer-name-length
		       (length (buffer-name buffer)))))))
     (buffer-list))
    ;; Make a list with elements of type
    ;; (BUFFER-LIST-VARIABLE
    ;;  CONDITION
    ;;  MENU-SORT-KEY
    ;;  MENU-TITLE
    ;;  ITEM-HANDLER
    ;;  SORTER)
    ;; Uses "function-global" variables:
    ;; function-info-vector
    (setq function-info-vector
	  (apply (function vector)
		 (mapcar (function msb--create-function-info)
			 msb-menu-cond)))
    ;; Split the buffer-list into several lists; one list for each
    ;; criteria. This is the most critical part with respect to time.
    (mapc (function (lambda (buffer)
		      (cond ((and msb-files-by-directory
				  (buffer-file-name buffer))
			     (push buffer file-buffers))
			    (t
			     (msb--choose-menu buffer
					       function-info-vector
					       max-buffer-name-length)))))
	  (buffer-list))
    (when file-buffers
      (setq file-buffers
	    (mapcar (function
		     (lambda (buffer-list)
		       (cons msb-files-by-directory-sort-key
			     (cons (car buffer-list)
				   (sort
				    (mapcar (function
					     (lambda (buffer)
					       (cons (buffer-name buffer)
						     buffer)))
					    (cdr buffer-list))
				    (function
				     (lambda (item1 item2)
				       (string< (car item1) (car item2)))))))))
		     (msb--choose-file-menu file-buffers))))
    ;; Now make the menu - a list of (TITLE . BUFFER-LIST)
    (let* ((buffers (buffer-list))
	   menu
	   (most-recently-used
	    (msb--most-recently-used-menu max-buffer-name-length))
	   (others (append file-buffers
			   (loop for elt
			 across function-info-vector
			 for value = (msb--create-sort-item elt)
			 if value collect value))))
      (setq menu
	    (mapcar 'cdr		;Remove the SORT-KEY
		    ;; Sort the menus - not the items.
		    (msb--add-separators
		    (sort
		     ;; Get a list of (SORT-KEY TITLE . BUFFER-LIST)
		     ;; Also sorts the items within the menus.
		     (if (cdr most-recently-used)
			 (cons
			  ;; Add most recent used buffers
			  (cons msb-most-recently-used-sort-key
				most-recently-used)
			  others)
		       others)
		     (function (lambda (elt1 elt2)
				 (< (car elt1) (car elt2))))))))
      ;; Now make it a keymap menu
      (append
       '(keymap "Select Buffer")
       (msb--make-keymap-menu menu)
       (when msb-separator-diff
	 (list (list 'separator "---")))
       (list (cons 'toggle 
		   (cons
		   (if msb-files-by-directory
		       "*Files by type*"
		     "*Files by directory*")
		   'msb--toggle-menu-type)))))))

(defun msb--create-buffer-menu  ()
  (save-match-data
    (save-excursion
      (msb--create-buffer-menu-2))))

;;;
;;; Multi purpose function for selecting a buffer with the mouse.
;;; 
(defun msb--toggle-menu-type ()
  (interactive)
  (setq msb-files-by-directory (not msb-files-by-directory))
  (menu-bar-update-buffers t))

(defun mouse-select-buffer (event)
  "Pop up several menus of buffers, for selection with the mouse.
Returns the selected buffer or nil if no buffer is selected.

The way the buffers are splitted is conveniently handled with the
variable 'msb-menu-cond'."
  ;; Popup the menu and return the selected buffer.
  (when (or msb--error
	    (not msb--last-buffer-menu)
	    (not (fboundp 'frame-or-buffer-changed-p))
	    (frame-or-buffer-changed-p))
    (setq msb--error nil)
    (setq msb--last-buffer-menu (msb--create-buffer-menu)))
  (let ((position event))
    (when (and (fboundp 'posn-x-y)
	       (fboundp 'posn-window))
      (let ((posX (car (posn-x-y (event-start event))))
	    (posY (cdr (posn-x-y (event-start event))))
	    (posWind (posn-window (event-start event)))
	    name)
	;; adjust position
	(setq posX (- posX (funcall msb-horizontal-shift-function))
	      position (list (list posX posY) posWind))))
    (setq name (x-popup-menu position msb--last-buffer-menu))
    ;; If toggle bring up the
    (cond
     ((eq (car name) 'toggle)
	(msb--toggle-menu-type)
	(mouse-select-buffer event))
     ((and (numberp (car name))
	   (null (cdr name)))
      (let ((msb--last-buffer-menu (nthcdr 3 (assq (car name) msb--last-buffer-menu))))
	(mouse-select-buffer event)))
      ((and (stringp (car name))
	   (null (cdr name)))
      (cons nil name))
      (t
       name))))
		    
;; Add separators
(defun msb--add-separators (sorted-list)
  (cond
   ((or (not msb-separator-diff)
	(not (numberp msb-separator-diff)))
    sorted-list)
   (t
    (let ((last-key nil))
      (mapcan
       (function
	(lambda (item)
	  (cond
	   ((and msb-separator-diff
		 last-key 
		 (> (- (car item) last-key)
		    msb-separator-diff))
	    (setq last-key (car item))
	    (list (cons last-key 'separator)
		  item))
	   (t
	    (setq last-key (car item))
	    (list item)))))
       sorted-list)))))

(defun msb--make-keymap-menu (raw-menu)
  (let ((end (cons '(nil) 'menu-bar-select-buffer))
	(mcount 0))
    (mapcar
     (function
      (lambda (sub-menu)
	(cond 
	 ((eq 'separator sub-menu)
	  (list 'separator "---"))
	 (t
	  (append (list (incf mcount) (car sub-menu)
			'keymap (car sub-menu))
		  (mapcar (function
			   (lambda (item)
			     (let ((string (car item))
				   (buffer (cdr item)))
			       (cons (buffer-name buffer)
				     (cons string end)))))
		   (cdr sub-menu)))))))
     raw-menu)))

(defun menu-bar-update-buffers (&optional arg)
  ;; If user discards the Buffers item, play along.
  (when (and (lookup-key (current-global-map) [menu-bar buffer])
	     (or (not (fboundp 'frame-or-buffer-changed-p))
		 (frame-or-buffer-changed-p)
		 arg))
    (let ((buffers (buffer-list))
	  (frames (frame-list))
	  buffers-menu frames-menu)
      ;; If requested, list only the N most recently selected buffers.
      (when (and (integerp buffers-menu-max-size)
		 (> buffers-menu-max-size 1)
		 (> (length buffers) buffers-menu-max-size))
	(setcdr (nthcdr buffers-menu-max-size buffers) nil))
      ;; Make the menu of buffers proper.
      (setq msb--last-buffer-menu (msb--create-buffer-menu))
      (setq buffers-menu msb--last-buffer-menu)
      ;; Make a Frames menu if we have more than one frame.
      (if (cdr frames)
	  (setq frames-menu
		(cons "Select Frame"
		      (mapcar
		       (function
			(lambda (frame)
			  (nconc
			   (list frame
				 (cdr (assq 'name
					    (frame-parameters frame)))
				 (cons nil nil))
			   'menu-bar-select-frame)))
		       frames))))
      (when frames-menu
	(setq frames-menu (cons 'keymap frames-menu)))
      (define-key (current-global-map) [menu-bar buffer]
	(cons "Buffers"
	      (if (and buffers-menu frames-menu)
		  (list 'keymap "Buffers and Frames"
			(cons 'buffers (cons "Buffers" buffers-menu))
			(cons 'frames (cons "Frames" frames-menu)))
		(or buffers-menu frames-menu 'undefined)))))))

(when (and (boundp 'menu-bar-update-hook)
	   (not (fboundp 'frame-or-buffer-changed-p)))
  (defvar msb--buffer-count 0)
  (defun frame-or-buffer-changed-p ()
    (let ((count (length (buffer-list))))
      (when (/= count msb--buffer-count)
        (setq msb--buffer-count count)
        t))))

(unless (or (not (boundp 'menu-bar-update-hook))
	    (memq 'menu-bar-update-buffers menu-bar-update-hook))
    (add-hook 'menu-bar-update-hook 'menu-bar-update-buffers))

(and (fboundp 'mouse-buffer-menu)
     (substitute-key-definition 'mouse-buffer-menu 'msb (current-global-map)))

(provide 'msb)
(eval-after-load 'msb (run-hooks 'msb-after-load-hooks))
;;; msb.el ends here
