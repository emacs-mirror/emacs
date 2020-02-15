;;; files-x.el --- extended file handling commands

;; Copyright (C) 2009-2020 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@jurta.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: files
;; Package: emacs

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

;; This file defines additional infrequently used file- and
;; directory-handling commands that should not be in files.el
;; to not make the dumped image bigger.

;;; Code:

(eval-when-compile (require 'subr-x)) ; for string-trim-right


;;; Commands to add/delete file-local/directory-local variables.

(defun read-file-local-variable (prompt)
  "Read file-local variable using PROMPT and completion.
Intended to be used in the `interactive' spec of
`add-file-local-variable', `delete-file-local-variable',
`add-dir-local-variable', `delete-dir-local-variable'."
  (let* ((default (variable-at-point))
         (default (and (symbolp default) (boundp default)
		       (symbol-name default)))
         (variable
	  (completing-read
	   (if default
	       (format "%s (default %s): " prompt default)
	     (format "%s: " prompt))
	   obarray
	   (lambda (sym)
	     (or (custom-variable-p sym)
                 (get sym 'safe-local-variable)
		 (memq sym '(mode eval coding unibyte))))
	   nil nil nil default nil)))
    (and (stringp variable) (intern variable))))

(defun read-file-local-variable-value (variable)
  "Read value of file-local VARIABLE using completion.
Intended to be used in the `interactive' spec of
`add-file-local-variable' and `add-dir-local-variable'."
  (cond
   ((eq variable 'mode)
    (let* ((default (and (symbolp major-mode) (symbol-name major-mode)))
           (value
            (completing-read
             (if default
                 (format "Add %s with value (default %s): " variable default)
               (format "Add %s with value: " variable))
             obarray
             (lambda (sym)
               (string-match-p "-mode\\'" (symbol-name sym)))
             nil nil nil default nil)))
      (and (stringp value)
           (intern (replace-regexp-in-string "-mode\\'" "" value)))))
   ((eq variable 'eval)
    (read--expression (format "Add %s with expression: " variable)))
   ((eq variable 'coding)
    (let ((default (and (symbolp buffer-file-coding-system)
                        (symbol-name buffer-file-coding-system))))
      (read-coding-system
       (if default
           (format "Add %s with value (default %s): " variable default)
         (format "Add %s with value: " variable))
       default)))
   (t
    (let ((default (format "%S"
                           (cond ((eq variable 'unibyte) t)
                                 ((boundp variable)
                                  (symbol-value variable)))))
          (minibuffer-completing-symbol t))
      (read-from-minibuffer (format "Add %s with value: " variable)
                            nil read-expression-map t
                            'set-variable-value-history
			    default)))))

(defun read-file-local-variable-mode ()
  "Read per-directory file-local variable's mode using completion.
Intended to be used in the `interactive' spec of
`add-dir-local-variable', `delete-dir-local-variable'."
  (let* ((default (and (symbolp major-mode) (symbol-name major-mode)))
	 (mode
	  (completing-read
	   (if default
	       (format "Mode or subdirectory (default %s): " default)
	     (format "Mode or subdirectory: "))
	   obarray
	   (lambda (sym)
	     (and (string-match-p "-mode\\'" (symbol-name sym))
		  (not (or (memq sym minor-mode-list)
                           (string-match-p "-minor-mode\\'"
                                           (symbol-name sym))))))
	   nil nil nil default nil)))
    (cond
     ((equal mode "nil") nil)
     ((and (stringp mode) (fboundp (intern mode))) (intern mode))
     (t mode))))

(defun modify-file-local-variable-message (variable value op)
  (let* ((not-value (make-symbol ""))
	 (old-value (cond ((eq variable 'mode)
			   major-mode)
			  ((eq variable 'coding)
			   buffer-file-coding-system)
			  (t (if (and (symbolp variable)
				      (boundp variable))
				 (symbol-value variable)
			       not-value))))
	 (new-value (if (eq op 'delete)
			(cond ((eq variable 'mode)
			       (default-value 'major-mode))
			      ((eq variable 'coding)
			       (default-value 'buffer-file-coding-system))
			      (t (if (and (symbolp variable)
					  (default-boundp variable))
				     (default-value variable)
				   not-value)))
		      (cond ((eq variable 'mode)
			     (let ((string (format "%S" value)))
			       (if (string-match-p "-mode\\'" string)
				   value
				 (intern (concat string "-mode")))))
			    (t value)))))
    (when (or (eq old-value not-value)
	      (eq new-value not-value)
	      (not (equal old-value new-value)))
      (message "%s" (substitute-command-keys
		     "For this change to take effect revisit file using \\[revert-buffer]")))))

(defun modify-file-local-variable (variable value op &optional interactive)
  "Modify file-local VARIABLE in Local Variables depending on operation OP.

If OP is `add-or-replace' then delete all existing settings of
VARIABLE (except `mode' and `eval') and add a new file-local VARIABLE
with VALUE to the Local Variables list.

If there is no Local Variables list in the current file buffer and OP
is not `delete' then this function adds the first line containing the
string `Local Variables:' and the last line containing the string `End:'.

If OP is `delete' then delete all existing settings of VARIABLE
from the Local Variables list ignoring the input argument VALUE."
  (catch 'exit
    (let ((beg (point)) end replaced-pos)
      (unless enable-local-variables
	(throw 'exit (message "File-local variables are disabled")))

      ;; Look for "Local variables:" line in last page.
      (widen)
      (goto-char (point-max))
      (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)

      ;; Add "Local variables:" list if not found.
      (unless (let ((case-fold-search t))
		(search-forward "Local Variables:" nil t))

	;; Don't add "Local variables:" list for the deletion operation.
	(when (eq op 'delete)
	  (throw 'exit (progn (goto-char beg)
			      (message "Local Variables not found"))))

	(goto-char (point-max))
	(let ((comment-style 'plain)
	      (comment-start (or comment-start ";; ")))
	  (comment-region
	   (prog1 (setq beg (point))
	     (insert "\nLocal Variables:\nEnd:\n"))
	   (point)))

	(unless (let ((case-fold-search t))
		  (goto-char beg)
		  (search-forward "Local Variables:" nil t))
	  (throw 'exit (message "Can't add file-local variables"))))

      ;; prefix is what comes before "local variables:" in its line.
      ;; suffix is what comes after "local variables:" in its line.
      (let* ((prefix (buffer-substring (line-beginning-position)
				       (match-beginning 0)))
	     (suffix (buffer-substring (point) (line-end-position)))
	     (prefix-re (concat "^" (regexp-quote prefix)))
	     (suffix-re (concat (regexp-quote suffix) "$")))

	;; Find or add missing "End:".
	(forward-line 1)
	(setq beg (point))
	(save-excursion
	  (unless (let ((case-fold-search t))
		    (re-search-forward
		     (concat prefix-re "[ \t]*End:[ \t]*" suffix-re)
		     nil t))
	    (save-excursion
	      (insert (format "%sEnd:%s\n" prefix suffix))))
	  (beginning-of-line)
	  (setq end (point-marker)))

	;; Find and delete all existing variable/value pairs.
	(when (member op '(add-or-replace delete))
	  (if (and (eq op 'add-or-replace) (memq variable '(mode eval)))
	      (goto-char end)
	    (goto-char beg)
	    (while (re-search-forward
		    (format "%s%S:.*%s" prefix-re variable suffix-re) end t)
	      (delete-region (match-beginning 0) (1+ (match-end 0)))
	      (setq replaced-pos (point)))))

	;; Add a new variable/value pair.  Add `mode' to the start, add new
	;; variable to the end, and add a replaced variable to its last location.
	(when (eq op 'add-or-replace)
	  (cond
	   ((eq variable 'mode) (goto-char beg))
	   ((null replaced-pos) (goto-char end))
	   (replaced-pos (goto-char replaced-pos)))
	  (insert (format "%s%S: %S%s\n" prefix variable value suffix))))

      (when interactive
	(modify-file-local-variable-message variable value op)))))

;;;###autoload
(defun add-file-local-variable (variable value &optional interactive)
  "Add file-local VARIABLE with its VALUE to the Local Variables list.

This command deletes all existing settings of VARIABLE (except `mode'
and `eval') and adds a new file-local VARIABLE with VALUE to the
Local Variables list.

If there is no Local Variables list in the current file buffer
then this function adds the first line containing the string
`Local Variables:' and the last line containing the string `End:'."
  (interactive
   (let ((variable (read-file-local-variable "Add file-local variable")))
     ;; Error before reading value.
     (if (equal variable 'lexical-binding)
	 (user-error "The `%s' variable must be set at the start of the file"
		     variable))
     (list variable (read-file-local-variable-value variable) t)))
  (if (equal variable 'lexical-binding)
      (user-error "The `%s' variable must be set at the start of the file"
                  variable))
  (modify-file-local-variable variable value 'add-or-replace interactive))

;;;###autoload
(defun delete-file-local-variable (variable &optional interactive)
  "Delete all settings of file-local VARIABLE from the Local Variables list."
  (interactive
   (list (read-file-local-variable "Delete file-local variable") t))
  (modify-file-local-variable variable nil 'delete interactive))

(defun modify-file-local-variable-prop-line (variable value op &optional interactive)
  "Modify file-local VARIABLE in the -*- line depending on operation OP.

If OP is `add-or-replace' then delete all existing settings of
VARIABLE (except `mode' and `eval') and add a new file-local VARIABLE
with VALUE to the -*- line.

If there is no -*- line at the beginning of the current file buffer
and OP is not `delete' then this function adds the -*- line.

If OP is `delete' then delete all existing settings of VARIABLE
from the -*- line ignoring the input argument VALUE."
  (catch 'exit
    (let ((beg (point)) end replaced-pos)
      (unless enable-local-variables
	(throw 'exit (message "File-local variables are disabled")))

      ;; Find the -*- line at the beginning of the current buffer.
      (widen)
      (goto-char (point-min))
      (setq end (set-auto-mode-1))

      (if end
	  (setq beg (point-marker) end (copy-marker end))

	;; Add the -*- line if not found.
	;; Don't add the -*- line for the deletion operation.
	(when (eq op 'delete)
	  (throw 'exit (progn (goto-char beg)
			      (message "The -*- line not found"))))

	(goto-char (point-min))

	;; Skip interpreter magic line "#!" or XML declaration.
	(when (or (looking-at file-auto-mode-skip)
		  (looking-at "<\\?xml[^>\n]*>$"))
	  (forward-line 1))

	(let ((comment-style 'plain)
	      (comment-start (or comment-start ";;; "))
	      (line-beg (line-beginning-position))
	      (ce nil))
	  (comment-normalize-vars)
	  ;; If the first line contains a comment.
	  (if (save-excursion
		(and (looking-at comment-start-skip)
		     (goto-char (match-end 0))
		     (re-search-forward comment-end-skip)
		     (goto-char (match-beginning 0))
		     ;; Still on the same line?
		     (equal line-beg (line-beginning-position))
		     (setq ce (point))))
	      ;; Add local variables to the end of the existing comment.
	      (progn
		(goto-char ce)
		(insert "  -*-")
		(setq beg (point-marker))
		(setq end (point-marker))
		(insert "-*-"))
	    ;; Otherwise, add a new comment before the first line.
	    (comment-region
	     (prog1 (point)
	       (insert "-*-")
	       (setq beg (point-marker))
	       (setq end (point-marker))
	       (insert "-*-\n"))
	     (point)))))

      (cond
       ((looking-at "[ \t]*\\([^ \t\n\r:;]+\\)\\([ \t]*-\\*-\\)")
	;; Simple form: "-*- MODENAME -*-".
	(if (eq variable 'mode)
	    ;; Replace or delete MODENAME
	    (progn
	      (when (member op '(add-or-replace delete))
		(delete-region (match-beginning 1) (match-end 1)))
	      (when (eq op 'add-or-replace)
		(goto-char (match-beginning 1))
		(insert (format "%S" value))))
	  ;; Else, turn `MODENAME' into `mode:MODENAME'
	  ;; and add `VARIABLE: VALUE;'
	  (goto-char (match-beginning 2))
	  (insert (format "; %S: %S; " variable value))
	  (goto-char (match-beginning 1))
	  (insert " mode: ")))

       (t
	;; Hairy form: '-*-' [ <variable> ':' <value> ';' ]* '-*-'
	;; Find and delete all existing variable/value pairs.
	(when (member op '(add-or-replace delete))
	  (if (and (eq op 'add-or-replace) (memq variable '(mode eval)))
	      (goto-char end)
	    (goto-char beg)
	    (while (< (point) end)
	      (or (looking-at "[ \t]*\\([^ \t\n:]+\\)[ \t]*:[ \t]*")
		  (throw 'exit (message "Malformed -*- line")))
	      (goto-char (match-end 0))
	      (let ((key (intern (match-string 1))))
                (save-restriction
                  (narrow-to-region (point) end)
                  (let ((read-circle nil))
                    (read (current-buffer))))
		(skip-chars-forward " \t;")
		(when (eq key variable)
		  (delete-region (match-beginning 0) (point))
		  (setq replaced-pos (point)))))))
	;; Add a new variable/value pair.  Add `mode' to the start, add new
	;; variable to the end, and add a replaced variable to its last location.
	(when (eq op 'add-or-replace)
	  (cond
	   ((eq variable 'mode) (goto-char beg))
	   ((null replaced-pos) (goto-char end))
	   (replaced-pos (goto-char replaced-pos)))
          (if (and (save-excursion
                     (skip-chars-backward " \t")
                     (not (eq (char-before) ?\;)))
		   (not (equal (point) (marker-position beg)))
		   ;; When existing `-*- -*-' is empty, beg > end.
		   (not (> (marker-position beg) (marker-position end))))
	      (insert ";"))
	  (unless (eq (char-before) ?\s) (insert " "))
	  (insert (format "%S: %S;" variable value))
	  (unless (eq (char-after) ?\s) (insert " ")))))

      (when interactive
	(modify-file-local-variable-message variable value op)))))

;;;###autoload
(defun add-file-local-variable-prop-line (variable value &optional interactive)
  "Add file-local VARIABLE with its VALUE to the -*- line.

This command deletes all existing settings of VARIABLE (except `mode'
and `eval') and adds a new file-local VARIABLE with VALUE to
the -*- line.

If there is no -*- line at the beginning of the current file buffer
then this function adds it."
  (interactive
   (let ((variable (read-file-local-variable "Add -*- file-local variable")))
     (list variable (read-file-local-variable-value variable) t)))
  (modify-file-local-variable-prop-line variable value 'add-or-replace interactive))

;;;###autoload
(defun delete-file-local-variable-prop-line (variable &optional interactive)
  "Delete all settings of file-local VARIABLE from the -*- line."
  (interactive
   (list (read-file-local-variable "Delete -*- file-local variable") t))
  (modify-file-local-variable-prop-line variable nil 'delete interactive))

(defvar auto-insert) ; from autoinsert.el

(defun modify-dir-local-variable (mode variable value op)
  "Modify directory-local VARIABLE in .dir-locals.el depending on operation OP.

If OP is `add-or-replace' then delete all existing settings of
VARIABLE (except `mode' and `eval') and add a new directory-local VARIABLE
with VALUE to the MODE alist where MODE can be a mode name symbol or
a subdirectory name.

If .dir-locals.el was not found and OP is not `delete' then create
this file in the current directory.

If OP is `delete' then delete all existing settings of VARIABLE
from the MODE alist ignoring the input argument VALUE."
  (catch 'exit
    (unless enable-local-variables
      (throw 'exit (message "Directory-local variables are disabled")))
    (let* ((dir-or-cache (and (buffer-file-name)
                              (not (file-remote-p (buffer-file-name)))
                              (dir-locals-find-file (buffer-file-name))))
           (variables-file
            ;; If there are several .dir-locals, the user probably
            ;; wants to edit the last one (the highest priority).
            (cond ((stringp dir-or-cache)
                   (car (last (dir-locals--all-files dir-or-cache))))
                  ((consp dir-or-cache)	; result from cache
                   ;; If cache element has an mtime, assume it came
                   ;; from a file.  Otherwise, assume it was set
                   ;; directly.
                   (if (nth 2 dir-or-cache)
                       (car (last (dir-locals--all-files (car dir-or-cache))))
                     (cadr dir-or-cache)))
                  ;; Try to make a proper file-name.
                  (t (expand-file-name dir-locals-file))))
           variables)
      ;; I can't be bothered to handle this case right now.
      ;; Dir locals were set directly from a class.  You need to
      ;; directly modify the class in dir-locals-class-alist.
      (and variables-file (not (stringp variables-file))
	   (throw 'exit (message "Directory locals were not set from a file")))
      ;; Don't create ".dir-locals.el" for the deletion operation.
      (and (eq op 'delete)
	   (or (not variables-file)
	       (not (file-exists-p variables-file)))
	   (throw 'exit (message "No .dir-locals.el file was found")))
      (let ((auto-insert nil))
	(find-file variables-file))
      (widen)
      (goto-char (point-min))

      ;; Read alist of directory-local variables.
      (ignore-errors
	(delete-region
	 (prog1 (point)
	   (setq variables (let ((read-circle nil))
			     (read (current-buffer)))))
	 (point)))

      ;; Add or replace variable in alist of directory-local variables.
      (let ((mode-assoc (assoc mode variables)))
	(if mode-assoc
	    (setq variables
		  (cons (cons mode
			      (if (eq op 'delete)
				  (assq-delete-all variable (cdr mode-assoc))
				(cons
				 (cons variable value)
				 (if (memq variable '(mode eval))
				     (cdr mode-assoc)
				   (assq-delete-all variable (cdr mode-assoc))))))
			(assoc-delete-all mode variables)))
	  (setq variables
		(cons `(,mode . ((,variable . ,value)))
		      variables))))

      ;; Invalidate cache (may be needed if this .dir-locals.el file
      ;; will be written with the same timestamp as is already present
      ;; in the cache, see bug#13860).
      (setq dir-locals-directory-cache
            (assoc-delete-all (file-name-directory variables-file)
                              dir-locals-directory-cache))

      ;; Insert modified alist of directory-local variables.
      (insert ";;; Directory Local Variables\n")
      (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
      (princ (dir-locals-to-string
              (sort variables
		    (lambda (a b)
		      (cond
		       ((null (car a)) t)
		       ((null (car b)) nil)
		       ((and (symbolp (car a)) (stringp (car b))) t)
		       ((and (symbolp (car b)) (stringp (car a))) nil)
		       (t (string< (car a) (car b)))))))
             (current-buffer))
      (goto-char (point-min))
      (indent-sexp))))

(defun dir-locals-to-string (variables)
  "Output alists of VARIABLES to string in dotted pair notation syntax."
  (format "(%s)" (mapconcat
                  (lambda (mode-variables)
                    (format "(%S . %s)"
                            (car mode-variables)
                            (format "(%s)" (mapconcat
                                            (lambda (variable-value)
                                              (format "(%S . %s)"
                                                      (car variable-value)
                                                      (string-trim-right
                                                       (pp-to-string
                                                        (cdr variable-value)))))
                                            (cdr mode-variables) "\n"))))
                  variables "\n")))

;;;###autoload
(defun add-dir-local-variable (mode variable value)
  "Add directory-local VARIABLE with its VALUE and MODE to .dir-locals.el."
  (interactive
   (let (variable)
     (list
      (read-file-local-variable-mode)
      (setq variable (read-file-local-variable "Add directory-local variable"))
      (read-file-local-variable-value variable))))
  (modify-dir-local-variable mode variable value 'add-or-replace))

;;;###autoload
(defun delete-dir-local-variable (mode variable)
  "Delete all MODE settings of file-local VARIABLE from .dir-locals.el."
  (interactive
   (list
    (read-file-local-variable-mode)
    (read-file-local-variable "Delete directory-local variable")))
  (modify-dir-local-variable mode variable nil 'delete))

;;;###autoload
(defun copy-file-locals-to-dir-locals ()
  "Copy file-local variables to .dir-locals.el."
  (interactive)
  (dolist (elt file-local-variables-alist)
    (unless (assq (car elt) dir-local-variables-alist)
      (add-dir-local-variable major-mode (car elt) (cdr elt)))))

;;;###autoload
(defun copy-dir-locals-to-file-locals ()
  "Copy directory-local variables to the Local Variables list."
  (interactive)
  (dolist (elt dir-local-variables-alist)
    (add-file-local-variable (car elt) (cdr elt))))

;;;###autoload
(defun copy-dir-locals-to-file-locals-prop-line ()
  "Copy directory-local variables to the -*- line."
  (interactive)
  (dolist (elt dir-local-variables-alist)
    (add-file-local-variable-prop-line (car elt) (cdr elt))))


;;; connection-local variables.

;;;###autoload
(defvar enable-connection-local-variables t
  "Non-nil means enable use of connection-local variables.")

(defvar connection-local-variables-alist nil
  "Alist of connection-local variable settings in the current buffer.
Each element in this list has the form (VAR . VALUE), where VAR
is a connection-local variable (a symbol) and VALUE is its value.
The actual value in the buffer may differ from VALUE, if it is
changed by the user.")
(make-variable-buffer-local 'connection-local-variables-alist)
(setq ignored-local-variables
      (cons 'connection-local-variables-alist ignored-local-variables))

(defvar connection-local-profile-alist nil
  "Alist mapping connection profiles to variable lists.
Each element in this list has the form (PROFILE VARIABLES).
PROFILE is the name of a connection profile (a symbol).
VARIABLES is a list that declares connection-local variables for
PROFILE.  An element in VARIABLES is an alist whose elements are
of the form (VAR . VALUE).")

(defvar connection-local-criteria-alist nil
  "Alist mapping connection criteria to connection profiles.
Each element in this list has the form (CRITERIA PROFILES).
CRITERIA is a plist identifying a connection and the application
using this connection.  Property names might be `:application',
`:protocol', `:user' and `:machine'.  The property value of
`:application' is a symbol, all other property values are
strings.  All properties are optional; if CRITERIA is nil, it
always applies.
PROFILES is a list of connection profiles (symbols).")

(defsubst connection-local-normalize-criteria (criteria)
  "Normalize plist CRITERIA according to properties.
Return a reordered plist."
  (apply
   'append
   (mapcar
    (lambda (property)
      (when (and (plist-member criteria property) (plist-get criteria property))
        (list property (plist-get criteria property))))
    '(:application :protocol :user :machine))))

(defsubst connection-local-get-profiles (criteria)
  "Return the connection profiles list for CRITERIA.
CRITERIA is a plist identifying a connection and the application
using this connection, see `connection-local-criteria-alist'."
  (let (profiles)
    (dolist (crit-alist connection-local-criteria-alist)
      (let ((crit criteria)
            (match t))
        (while (and crit match)
          (when (plist-member (car crit-alist) (car crit))
            (setq match (equal (plist-get (car crit-alist) (car crit))
                               (plist-get criteria (car crit)))))
          (setq crit (cddr crit)))
        (when match
          (setq profiles (append profiles (cdr crit-alist))))))
    (delete-dups profiles)))

;;;###autoload
(defun connection-local-set-profiles (criteria &rest profiles)
  "Add PROFILES for CRITERIA.
CRITERIA is a plist identifying a connection and the application
using this connection, see `connection-local-criteria-alist'.
PROFILES are the names of connection profiles (a symbol).

When a connection to a remote server is opened and CRITERIA
matches to that server, the connection-local variables from
PROFILES are applied to the corresponding process buffer.  The
variables for a connection profile are defined using
`connection-local-set-profile-variables'."
  (unless (listp criteria)
    (error "Wrong criteria `%s'" criteria))
  (dolist (profile profiles)
    (unless (assq profile connection-local-profile-alist)
      (error "No such connection profile `%s'" (symbol-name profile))))
  (let* ((criteria (connection-local-normalize-criteria criteria))
         (slot (assoc criteria connection-local-criteria-alist)))
    (if slot
        (setcdr slot (delete-dups (append (cdr slot) profiles)))
      (setq connection-local-criteria-alist
            (cons (cons criteria (delete-dups profiles))
		  connection-local-criteria-alist)))))

(defsubst connection-local-get-profile-variables (profile)
  "Return the connection-local variable list for PROFILE."
  (cdr (assq profile connection-local-profile-alist)))

;;;###autoload
(defun connection-local-set-profile-variables (profile variables)
  "Map the symbol PROFILE to a list of variable settings.
VARIABLES is a list that declares connection-local variables for
the connection profile.  An element in VARIABLES is an alist
whose elements are of the form (VAR . VALUE).

When a connection to a remote server is opened, the server's
connection profiles are found.  A server may be assigned a
connection profile using `connection-local-set-profiles'.  Then
variables are set in the server's process buffer according to the
VARIABLES list of the connection profile.  The list is processed
in order."
  (setf (alist-get profile connection-local-profile-alist) variables))

(defun hack-connection-local-variables (criteria)
  "Read connection-local variables according to CRITERIA.
Store the connection-local variables in buffer local
variable`connection-local-variables-alist'.

This does nothing if `enable-connection-local-variables' is nil."
  (when enable-connection-local-variables
    ;; Filter connection profiles.
    (dolist (profile (connection-local-get-profiles criteria))
      ;; Loop over variables.
      (dolist (variable (connection-local-get-profile-variables profile))
        (unless (assq (car variable) connection-local-variables-alist)
          (push variable connection-local-variables-alist))))
    ;; Push them to `file-local-variables-alist'.  Connection-local
    ;; variables do not appear from external files.  So we can regard
    ;; them as safe.
    (let ((enable-local-variables :all))
      (hack-local-variables-filter connection-local-variables-alist nil))))

;;;###autoload
(defun hack-connection-local-variables-apply (criteria)
 "Apply connection-local variables identified by CRITERIA.
Other local variables, like file-local and dir-local variables,
will not be changed."
 (hack-connection-local-variables criteria)
 (let ((file-local-variables-alist
        (copy-tree connection-local-variables-alist)))
   (hack-local-variables-apply)))

(defsubst connection-local-criteria-for-default-directory ()
  "Return a connection-local criteria, which represents `default-directory'."
  (when (file-remote-p default-directory)
    `(:application tramp
       :protocol ,(file-remote-p default-directory 'method)
       :user     ,(file-remote-p default-directory 'user)
       :machine  ,(file-remote-p default-directory 'host))))

;;;###autoload
(defmacro with-connection-local-variables (&rest body)
  "Apply connection-local variables according to `default-directory'.
Execute BODY, and unwind connection-local variables."
  (declare (debug t))
  `(if (file-remote-p default-directory)
       (let ((enable-connection-local-variables t)
             (old-buffer-local-variables (buffer-local-variables))
	     connection-local-variables-alist)
	 (hack-connection-local-variables-apply
	  (connection-local-criteria-for-default-directory))
	 (unwind-protect
             (progn ,@body)
	   ;; Cleanup.
	   (dolist (variable connection-local-variables-alist)
	     (let ((elt (assq (car variable) old-buffer-local-variables)))
	       (if elt
		   (set (make-local-variable (car elt)) (cdr elt))
		 (kill-local-variable (car variable)))))))
     ;; No connection-local variables to apply.
     ,@body))



(provide 'files-x)

;;; files-x.el ends here
