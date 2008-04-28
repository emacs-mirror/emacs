;;; org-remember.el --- Fast note taking in Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the system to take fast notes with Org-mode.
;; This system is used together with John Wiegleys `remember.el'.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org)

(declare-function remember "remember" (&optional initial))
(declare-function remember-buffer-desc "remember" ())
(declare-function remember-finalize "remember" ())
(defvar remember-save-after-remembering)
(defvar remember-data-file)
(defvar remember-register)
(defvar remember-buffer)
(defvar remember-handler-functions)
(defvar remember-annotation-functions)

(defgroup org-remember nil
  "Options concerning interaction with remember.el."
  :tag "Org Remember"
  :group 'org)

(defcustom org-remember-store-without-prompt t
  "Non-nil means, `C-c C-c' stores remember note without further promts.
In this case, you need `C-u C-c C-c' to get the prompts for
note file and headline.
When this variable is nil, `C-c C-c' give you the prompts, and
`C-u C-c C-c' trigger the fasttrack."
  :group 'org-remember
  :type 'boolean)

(defcustom org-remember-interactive-interface 'refile
  "The interface to be used for interactive filing of remember notes.
This is only used when the interactive mode for selecting a filing
location is used (see the variable `org-remember-store-without-prompt').
Allowed vaues are:
outline                  The interface shows an outline of the relevant file
                         and the correct heading is found by moving through
                         the outline or by searching with incremental search.
outline-path-completion  Headlines in the current buffer are offered via
                         completion.
refile                   Use the refile interface, and offer headlines,
                         possibly from different buffers."
  :group 'org-remember
  :type '(choice
	  (const :tag "Refile" refile)
	  (const :tag "Outline" outline)
	  (const :tag "Outline-path-completion" outline-path-completion)))

(defcustom org-remember-default-headline ""
  "The headline that should be the default location in the notes file.
When filing remember notes, the cursor will start at that position.
You can set this on a per-template basis with the variable
`org-remember-templates'."
  :group 'org-remember
  :type 'string)

(defcustom org-remember-templates nil
  "Templates for the creation of remember buffers.
When nil, just let remember make the buffer.
When not nil, this is a list of 5-element lists.  In each entry, the first
element is the name of the template, which should be a single short word.
The second element is a character, a unique key to select this template.
The third element is the template.

The fourth element is optional and can specify a destination file for
remember items created with this template.  The default file is given
by `org-default-notes-file'.  If the file name is not an absolute path,
it will be interpreted relative to `org-directory'.

An optional fifth element can specify the headline in that file that should
be offered first when the user is asked to file the entry.  The default
headline is given in the variable `org-remember-default-headline'.

An optional sixth element specifies the contexts in which the user can
select the template.  This element can be either a list of major modes
or a function.  `org-remember' will first check whether the function
returns `t' or if we are in any of the listed major modes, and select
the template accordingly.

The template specifies the structure of the remember buffer.  It should have
a first line starting with a star, to act as the org-mode headline.
Furthermore, the following %-escapes will be replaced with content:

  %^{prompt}  Prompt the user for a string and replace this sequence with it.
              A default value and a completion table ca be specified like this:
              %^{prompt|default|completion2|completion3|...}
  %t          time stamp, date only
  %T          time stamp with date and time
  %u, %U      like the above, but inactive time stamps
  %^t         like %t, but prompt for date.  Similarly %^T, %^u, %^U
              You may define a prompt like %^{Please specify birthday}t
  %n          user name (taken from `user-full-name')
  %a          annotation, normally the link created with org-store-link
  %i          initial content, the region active.  If %i is indented,
              the entire inserted text will be indented as well.
  %c          content of the clipboard, or current kill ring head
  %^g         prompt for tags, with completion on tags in target file
  %^G         prompt for tags, with completion all tags in all agenda files
  %:keyword   specific information for certain link types, see below
  %[pathname] insert the contents of the file given by `pathname'
  %(sexp)     evaluate elisp `(sexp)' and replace with the result
  %!          Store this note immediately after filling the template

  %?          After completing the template, position cursor here.

Apart from these general escapes, you can access information specific to the
link type that is created.  For example, calling `remember' in emails or gnus
will record the author and the subject of the message, which you can access
with %:author and %:subject, respectively.  Here is a complete list of what
is recorded for each link type.

Link type          |  Available information
-------------------+------------------------------------------------------
bbdb               |  %:type %:name %:company
vm, wl, mh, rmail  |  %:type %:subject %:message-id
                   |  %:from %:fromname %:fromaddress
                   |  %:to   %:toname   %:toaddress
                   |  %:fromto (either \"to NAME\" or \"from NAME\")
gnus               |  %:group, for messages also all email fields
w3, w3m            |  %:type %:url
info               |  %:type %:file %:node
calendar           |  %:type %:date"
  :group 'org-remember
  :get (lambda (var) ; Make sure all entries have at least 5 elements
	 (mapcar (lambda (x)
		   (if (not (stringp (car x))) (setq x (cons "" x)))
		   (cond ((= (length x) 4) (append x '("")))
			 ((= (length x) 3) (append x '("" "")))
			 (t x)))
		 (default-value var)))
  :type '(repeat
	  :tag "enabled"
	  (list :value ("" ?a "\n" nil nil nil)
		(string :tag "Name")
		(character :tag "Selection Key")
		(string :tag "Template")
		(choice
		 (file :tag "Destination file")
		 (const :tag "Prompt for file" nil))
		(choice
		 (string :tag "Destination headline")
		 (const :tag "Selection interface for heading"))
		(choice
		 (const :tag "Use by default" nil)
		 (const :tag "Use in all contexts" t)
		 (repeat :tag "Use only if in major mode"
			 (symbol :tag "Major mode"))
		 (function :tag "Perform a check against function")))))

(defvar annotation) ; from remember.el, dynamically scoped in `remember-mode'
(defvar initial)    ; from remember.el, dynamically scoped in `remember-mode'

;;;###autoload
(defun org-remember-insinuate ()
  "Setup remember.el for use wiht Org-mode."
  (require 'remember)
  (setq remember-annotation-functions '(org-remember-annotation))
  (setq remember-handler-functions '(org-remember-handler))
  (add-hook 'remember-mode-hook 'org-remember-apply-template))

;;;###autoload
(defun org-remember-annotation ()
  "Return a link to the current location as an annotation for remember.el.
If you are using Org-mode files as target for data storage with
remember.el, then the annotations should include a link compatible with the
conventions in Org-mode.  This function returns such a link."
  (org-store-link nil))

(defconst org-remember-help
"Select a destination location for the note.
UP/DOWN=headline   TAB=cycle visibility  [Q]uit   RET/<left>/<right>=Store
RET on headline   -> Store as sublevel entry to current headline
RET at beg-of-buf -> Append to file as level 2 headline
<left>/<right>    -> before/after current headline, same headings level")

(defvar org-remember-previous-location nil)
(defvar org-force-remember-template-char) ;; dynamically scoped

;; Save the major mode of the buffer we called remember from
(defvar org-select-template-temp-major-mode nil)

;; Temporary store the buffer where remember was called from
(defvar org-select-template-original-buffer nil)

(defun org-select-remember-template (&optional use-char)
  (when org-remember-templates
    (let* ((pre-selected-templates
	    (mapcar
	     (lambda (tpl)
	       (let ((ctxt (nth 5 tpl))
		     (mode org-select-template-temp-major-mode)
		     (buf org-select-template-original-buffer))
		 (and (or (not ctxt) (eq ctxt t)
			  (and (listp ctxt) (memq mode ctxt))
			  (and (functionp ctxt)
			       (with-current-buffer buf
				 ;; Protect the user-defined function from error
				 (condition-case nil (funcall ctxt) (error nil)))))
		      tpl)))
	     org-remember-templates))
	   ;; If no template at this point, add the default templates:
	   (pre-selected-templates1
	    (if (not (delq nil pre-selected-templates))
		(mapcar (lambda(x) (if (not (nth 5 x)) x))
			org-remember-templates)
	      pre-selected-templates))
	   ;; Then unconditionnally add template for any contexts
	   (pre-selected-templates2
	    (append (mapcar (lambda(x) (if (eq (nth 5 x) t) x))
			    org-remember-templates)
		    (delq nil pre-selected-templates1)))
	   (templates (mapcar (lambda (x)
				(if (stringp (car x))
				    (append (list (nth 1 x) (car x)) (cddr x))
				  (append (list (car x) "") (cdr x))))
			      (delq nil pre-selected-templates2)))
	   (char (or use-char
		     (cond
		      ((= (length templates) 1)
		       (caar templates))
		      ((and (boundp 'org-force-remember-template-char)
			    org-force-remember-template-char)
		       (if (stringp org-force-remember-template-char)
			   (string-to-char org-force-remember-template-char)
			 org-force-remember-template-char))
		      (t
		       (message "Select template: %s"
				(mapconcat
				 (lambda (x)
				   (cond
				    ((not (string-match "\\S-" (nth 1 x)))
				     (format "[%c]" (car x)))
				    ((equal (downcase (car x))
					    (downcase (aref (nth 1 x) 0)))
				     (format "[%c]%s" (car x)
					     (substring (nth 1 x) 1)))
				    (t (format "[%c]%s" (car x) (nth 1 x)))))
				 templates " "))
		       (let ((inhibit-quit t) (char0 (read-char-exclusive)))
			 (when (equal char0 ?\C-g)
			   (jump-to-register remember-register)
			   (kill-buffer remember-buffer))
			 char0))))))
      (cddr (assoc char templates)))))

(defun org-get-x-clipboard (value)
  "Get the value of the x clibboard, in a way that also works with XEmacs."
  (if (eq window-system 'x)
      (let ((x (if org-xemacs-p
		   (org-no-warnings (get-selection-no-error value))
		 (and (fboundp 'x-selection-value)
		      (x-selection-value value)))))
	(and (> (length x) 0) (set-text-properties 0 (length x) nil x) x))))

;;;###autoload
(defun org-remember-apply-template (&optional use-char skip-interactive)
  "Initialize *remember* buffer with template, invoke `org-mode'.
This function should be placed into `remember-mode-hook' and in fact requires
to be run from that hook to function properly."
  (if org-remember-templates
      (let* ((entry (org-select-remember-template use-char))
	     (tpl (car entry))
	     (plist-p (if org-store-link-plist t nil))
	     (file (if (and (nth 1 entry) (stringp (nth 1 entry))
			    (string-match "\\S-" (nth 1 entry)))
		       (nth 1 entry)
		     org-default-notes-file))
	     (headline (nth 2 entry))
	     (v-c (and (> (length kill-ring) 0) (current-kill 0)))
	     (v-x (or (org-get-x-clipboard 'PRIMARY)
		      (org-get-x-clipboard 'CLIPBOARD)
		      (org-get-x-clipboard 'SECONDARY)))
	     (v-t (format-time-string (car org-time-stamp-formats) (org-current-time)))
	     (v-T (format-time-string (cdr org-time-stamp-formats) (org-current-time)))
	     (v-u (concat "[" (substring v-t 1 -1) "]"))
	     (v-U (concat "[" (substring v-T 1 -1) "]"))
	     ;; `initial' and `annotation' are bound in `remember'
	     (v-i (if (boundp 'initial) initial))
	     (v-a (if (and (boundp 'annotation) annotation)
		      (if (equal annotation "[[]]") "" annotation)
		    ""))
	     (clipboards (remove nil (list v-i
					   (org-get-x-clipboard 'PRIMARY)
					   (org-get-x-clipboard 'CLIPBOARD)
					   (org-get-x-clipboard 'SECONDARY)
					   v-c)))
	     (v-A (if (and v-a
			   (string-match "\\[\\(\\[.*?\\]\\)\\(\\[.*?\\]\\)?\\]" v-a))
		      (replace-match "[\\1[%^{Link description}]]" nil nil v-a)
		    v-a))
	     (v-n user-full-name)
	     (org-startup-folded nil)
	     org-time-was-given org-end-time-was-given x
	     prompt completions char time pos default histvar)
	(when (and file (not (file-name-absolute-p file)))
	  (setq file (expand-file-name file org-directory)))
	(setq org-store-link-plist
	      (append (list :annotation v-a :initial v-i)
		      org-store-link-plist))
	(unless tpl (setq tpl "") (message "No template") (ding) (sit-for 1))
	(erase-buffer)
	(insert (substitute-command-keys
		 (format
"## Filing location: Select interactively, default, or last used:
## %s  to select file and header location interactively.
## %s  \"%s\" -> \"* %s\"
## C-u C-u C-c C-c  \"%s\" -> \"* %s\"
## To switch templates, use `\\[org-remember]'.  To abort use `C-c C-k'.\n\n"
		  (if org-remember-store-without-prompt "    C-u C-c C-c" "        C-c C-c")
		  (if org-remember-store-without-prompt "        C-c C-c" "    C-u C-c C-c")
		  (abbreviate-file-name (or file org-default-notes-file))
		  (or headline "")
		  (or (car org-remember-previous-location) "???")
		  (or (cdr org-remember-previous-location) "???"))))
	(insert tpl) (goto-char (point-min))
	;; Simple %-escapes
	(while (re-search-forward "%\\([tTuUaiAcx]\\)" nil t)
	  (when (and initial (equal (match-string 0) "%i"))
	    (save-match-data
	      (let* ((lead (buffer-substring
			    (point-at-bol) (match-beginning 0))))
		(setq v-i (mapconcat 'identity
				     (org-split-string initial "\n")
				     (concat "\n" lead))))))
	  (replace-match
	   (or (eval (intern (concat "v-" (match-string 1)))) "")
	   t t))

	;; %[] Insert contents of a file.
	(goto-char (point-min))
	(while (re-search-forward "%\\[\\(.+\\)\\]" nil t)
	  (let ((start (match-beginning 0))
		(end (match-end 0))
		(filename (expand-file-name (match-string 1))))
	    (goto-char start)
	    (delete-region start end)
	    (condition-case error
		(insert-file-contents filename)
	      (error (insert (format "%%![Couldn't insert %s: %s]"
				     filename error))))))
	;; %() embedded elisp
	(goto-char (point-min))
	(while (re-search-forward "%\\((.+)\\)" nil t)
	  (goto-char (match-beginning 0))
	  (let ((template-start (point)))
	    (forward-char 1)
	    (let ((result
		   (condition-case error
		       (eval (read (current-buffer)))
		     (error (format "%%![Error: %s]" error)))))
	      (delete-region template-start (point))
	      (insert result))))

	;; From the property list
	(when plist-p
	  (goto-char (point-min))
	  (while (re-search-forward "%\\(:[-a-zA-Z]+\\)" nil t)
	    (and (setq x (or (plist-get org-store-link-plist
					(intern (match-string 1))) ""))
		 (replace-match x t t))))

	;; Turn on org-mode in the remember buffer, set local variables
	(org-mode)
	(org-set-local 'org-finish-function 'org-remember-finalize)
	(if (and file (string-match "\\S-" file) (not (file-directory-p file)))
	    (org-set-local 'org-default-notes-file file))
	(if (and headline (stringp headline) (string-match "\\S-" headline))
	    (org-set-local 'org-remember-default-headline headline))
	;; Interactive template entries
	(goto-char (point-min))
	(while (re-search-forward "%^\\({\\([^}]*\\)}\\)?\\([gGuUtTCL]\\)?" nil t)
	  (setq char (if (match-end 3) (match-string 3))
		prompt (if (match-end 2) (match-string 2)))
	  (goto-char (match-beginning 0))
	  (replace-match "")
	  (setq completions nil default nil)
	  (when prompt
	    (setq completions (org-split-string prompt "|")
		  prompt (pop completions)
		  default (car completions)
		  histvar (intern (concat
				   "org-remember-template-prompt-history::"
				   (or prompt "")))
		  completions (mapcar 'list completions)))
	  (cond
	   ((member char '("G" "g"))
	    (let* ((org-last-tags-completion-table
		    (org-global-tags-completion-table
		     (if (equal char "G") (org-agenda-files) (and file (list file)))))
		   (org-add-colon-after-tag-completion t)
		   (ins (completing-read
			 (if prompt (concat prompt ": ") "Tags: ")
			 'org-tags-completion-function nil nil nil
			 'org-tags-history)))
	      (setq ins (mapconcat 'identity
				  (org-split-string ins (org-re "[^[:alnum:]_@]+"))
				  ":"))
	      (when (string-match "\\S-" ins)
		(or (equal (char-before) ?:) (insert ":"))
		(insert ins)
		(or (equal (char-after) ?:) (insert ":")))))
	   ((equal char "C")
	    (cond ((= (length clipboards) 1) (insert (car clipboards)))
		  ((> (length clipboards) 1)
		   (insert (read-string "Clipboard/kill value: "
					(car clipboards) '(clipboards . 1)
					(car clipboards))))))
	   ((equal char "L")
	    (cond ((= (length clipboards) 1)
		   (org-insert-link 0 (car clipboards)))
		  ((> (length clipboards) 1)
		   (org-insert-link 0 (read-string "Clipboard/kill value: "
						   (car clipboards)
						   '(clipboards . 1)
						   (car clipboards))))))
	   (char
	    (setq org-time-was-given (equal (upcase char) char))
	    (setq time (org-read-date (equal (upcase char) "U") t nil
				      prompt))
	    (org-insert-time-stamp time org-time-was-given
				   (member char '("u" "U"))
				   nil nil (list org-end-time-was-given)))
	   (t
	    (insert (org-completing-read
		     (concat (if prompt prompt "Enter string")
			     (if default (concat " [" default "]"))
			     ": ")
		     completions nil nil nil histvar default)))))
	(goto-char (point-min))
	(if (re-search-forward "%\\?" nil t)
	    (replace-match "")
	  (and (re-search-forward "^[^#\n]" nil t) (backward-char 1))))
    (org-mode)
    (org-set-local 'org-finish-function 'org-remember-finalize))
  (when (save-excursion
	  (goto-char (point-min))
	  (re-search-forward "%!" nil t))
    (replace-match "")
    (add-hook 'post-command-hook 'org-remember-finish-immediately 'append)))

(defun org-remember-finish-immediately ()
  "File remember note immediately.
This should be run in `post-command-hook' and will remove itself
from that hook."
  (remove-hook 'post-command-hook 'org-remember-finish-immediately)
  (when org-finish-function
    (funcall org-finish-function)))

(defvar org-clock-marker) ; Defined below
(defun org-remember-finalize ()
  "Finalize the remember process."
  (unless (fboundp 'remember-finalize)
    (defalias 'remember-finalize 'remember-buffer))
  (when (and org-clock-marker
	     (equal (marker-buffer org-clock-marker) (current-buffer)))
    ;; FIXME: test this, this is w/o notetaking!
    (let (org-log-note-clock-out) (org-clock-out)))
  (when buffer-file-name
    (save-buffer)
    (setq buffer-file-name nil))
  (remember-finalize))

;;;###autoload
(defun org-remember (&optional goto org-force-remember-template-char)
  "Call `remember'.  If this is already a remember buffer, re-apply template.
If there is an active region, make sure remember uses it as initial content
of the remember buffer.

When called interactively with a `C-u' prefix argument GOTO, don't remember
anything, just go to the file/headline where the selected template usually
stores its notes.  With a double prefix arg `C-u C-u', go to the last
note stored by remember.

Lisp programs can set ORG-FORCE-REMEMBER-TEMPLATE-CHAR to a character
associated with a template in `org-remember-templates'."
  (interactive "P")
  (cond
   ((equal goto '(4)) (org-go-to-remember-target))
   ((equal goto '(16)) (org-remember-goto-last-stored))
   (t
    ;; set temporary variables that will be needed in
    ;; `org-select-remember-template'
    (setq org-select-template-temp-major-mode major-mode)
    (setq org-select-template-original-buffer (current-buffer))
    (if (eq org-finish-function 'org-remember-finalize)
	(progn
	  (when (< (length org-remember-templates) 2)
	    (error "No other template available"))
	  (erase-buffer)
	  (let ((annotation (plist-get org-store-link-plist :annotation))
		(initial (plist-get org-store-link-plist :initial)))
	    (org-remember-apply-template))
	  (message "Press C-c C-c to remember data"))
      (if (org-region-active-p)
	  (org-do-remember (buffer-substring (point) (mark)))
	(org-do-remember))))))

(defun org-remember-goto-last-stored ()
  "Go to the location where the last remember note was stored."
  (interactive)
  (bookmark-jump "org-remember-last-stored")
  (message "This is the last note stored by remember"))

(defun org-go-to-remember-target (&optional template-key)
  "Go to the target location of a remember template.
The user is queried for the template."
  (interactive)
  (let* (org-select-template-temp-major-mode
	 (entry (org-select-remember-template template-key))
	 (file (nth 1 entry))
	 (heading (nth 2 entry))
	 visiting)
    (unless (and file (stringp file) (string-match "\\S-" file))
      (setq file org-default-notes-file))
    (when (and file (not (file-name-absolute-p file)))
      (setq file (expand-file-name file org-directory)))
    (unless (and heading (stringp heading) (string-match "\\S-" heading))
      (setq heading org-remember-default-headline))
    (setq visiting (org-find-base-buffer-visiting file))
    (if (not visiting) (find-file-noselect file))
    (switch-to-buffer (or visiting (get-file-buffer file)))
    (widen)
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "^\\*+[ \t]+" (regexp-quote heading)
		 (org-re "\\([ \t]+:[[:alnum:]@_:]*\\)?[ \t]*$"))
	 nil t)
	(goto-char (match-beginning 0))
      (error "Target headline not found: %s" heading))))

;;;###autoload
(defun org-remember-handler ()
  "Store stuff from remember.el into an org file.
First prompts for an org file.  If the user just presses return, the value
of `org-default-notes-file' is used.
Then the command offers the headings tree of the selected file in order to
file the text at a specific location.
You can either immediately press RET to get the note appended to the
file, or you can use vertical cursor motion and visibility cycling (TAB) to
find a better place.  Then press RET or <left> or <right> in insert the note.

Key      Cursor position   Note gets inserted
-----------------------------------------------------------------------------
RET      buffer-start      as level 1 heading at end of file
RET      on headline       as sublevel of the heading at cursor
RET      no heading        at cursor position, level taken from context.
			   Or use prefix arg to specify level manually.
<left>   on headline       as same level, before current heading
<right>  on headline       as same level, after current heading

So the fastest way to store the note is to press RET RET to append it to
the default file.  This way your current train of thought is not
interrupted, in accordance with the principles of remember.el.
You can also get the fast execution without prompting by using
C-u C-c C-c to exit the remember buffer.  See also the variable
`org-remember-store-without-prompt'.

Before being stored away, the function ensures that the text has a
headline, i.e. a first line that starts with a \"*\".  If not, a headline
is constructed from the current date and some additional data.

If the variable `org-adapt-indentation' is non-nil, the entire text is
also indented so that it starts in the same column as the headline
\(i.e. after the stars).

See also the variable `org-reverse-note-order'."
  (goto-char (point-min))
  (while (looking-at "^[ \t]*\n\\|^##.*\n")
    (replace-match ""))
  (goto-char (point-max))
  (beginning-of-line 1)
  (while (looking-at "[ \t]*$\\|##.*")
    (delete-region (1- (point)) (point-max))
    (beginning-of-line 1))
  (catch 'quit
    (if org-note-abort (throw 'quit nil))
    (let* ((txt (buffer-substring (point-min) (point-max)))
	   (fastp (org-xor (equal current-prefix-arg '(4))
			   org-remember-store-without-prompt))
	   (file (cond
		  (fastp org-default-notes-file)
		  ((and (eq org-remember-interactive-interface 'refile)
			org-refile-targets)
		   org-default-notes-file)
		  ((not (and (equal current-prefix-arg '(16))
			     org-remember-previous-location))
		   (org-get-org-file))))
	   (heading org-remember-default-headline)
	   (visiting (and file (org-find-base-buffer-visiting file)))
	   (org-startup-folded nil)
	   (org-startup-align-all-tables nil)
	   (org-goto-start-pos 1)
	   spos exitcmd level indent reversed)
      (if (and (equal current-prefix-arg '(16)) org-remember-previous-location)
	  (setq file (car org-remember-previous-location)
		heading (cdr org-remember-previous-location)
		fastp t))
      (setq current-prefix-arg nil)
      (if (string-match "[ \t\n]+\\'" txt)
	  (setq txt (replace-match "" t t txt)))
      ;; Modify text so that it becomes a nice subtree which can be inserted
      ;; into an org tree.
      (let* ((lines (split-string txt "\n"))
	     first)
	(setq first (car lines) lines (cdr lines))
	(if (string-match "^\\*+ " first)
	    ;; Is already a headline
	    (setq indent nil)
	  ;; We need to add a headline:  Use time and first buffer line
	  (setq lines (cons first lines)
		first (concat "* " (current-time-string)
			      " (" (remember-buffer-desc) ")")
		indent "  "))
	(if (and org-adapt-indentation indent)
	    (setq lines (mapcar
			 (lambda (x)
			   (if (string-match "\\S-" x)
			       (concat indent x) x))
			 lines)))
	(setq txt (concat first "\n"
			  (mapconcat 'identity lines "\n"))))
      (if (string-match "\n[ \t]*\n[ \t\n]*\\'" txt)
	  (setq txt (replace-match "\n\n" t t txt))
	(if (string-match "[ \t\n]*\\'" txt)
	    (setq txt (replace-match "\n" t t txt))))
      ;; Put the modified text back into the remember buffer, for refile.
      (erase-buffer)
      (insert txt)
      (goto-char (point-min))
      (when (and (eq org-remember-interactive-interface 'refile)
		 (not fastp))
	(org-refile nil (or visiting (find-file-noselect file)))
	(throw 'quit t))
      ;; Find the file
      (if (not visiting) (find-file-noselect file))
      (with-current-buffer (or visiting (get-file-buffer file))
	(unless (org-mode-p)
	  (error "Target files for remember notes must be in Org-mode"))
	(save-excursion
	  (save-restriction
	    (widen)
	    (and (goto-char (point-min))
		 (not (re-search-forward "^\\* " nil t))
		 (insert "\n* " (or heading "Notes") "\n"))
	    (setq reversed (org-notes-order-reversed-p))

	    ;; Find the default location
	    (when (and heading (stringp heading) (string-match "\\S-" heading))
	      (goto-char (point-min))
	      (if (re-search-forward
		   (concat "^\\*+[ \t]+" (regexp-quote heading)
			   (org-re "\\([ \t]+:[[:alnum:]@_:]*\\)?[ \t]*$"))
		   nil t)
		  (setq org-goto-start-pos (match-beginning 0))
		(when fastp
		  (goto-char (point-max))
		  (unless (bolp) (newline))
		  (insert "* " heading "\n")
		  (setq org-goto-start-pos (point-at-bol 0)))))

	    ;; Ask the User for a location, using the appropriate interface
	    (cond
	     (fastp (setq spos org-goto-start-pos
			  exitcmd 'return))
	     ((eq org-remember-interactive-interface 'outline)
	      (setq spos (org-get-location (current-buffer)
					   org-remember-help)
		    exitcmd (cdr spos)
		    spos (car spos)))
	     ((eq org-remember-interactive-interface 'outline-path-completion)
	      (let ((org-refile-targets '((nil . (:maxlevel . 10))))
		    (org-refile-use-outline-path t))
		(setq spos (org-refile-get-location "Heading: ")
		      exitcmd 'return
		      spos (nth 3 spos))))
	     (t (error "This should not happen")))
	    (if (not spos) (throw 'quit nil)) ; return nil to show we did
					; not handle this note
	    (goto-char spos)
	    (cond ((org-on-heading-p t)
		   (org-back-to-heading t)
		   (setq level (funcall outline-level))
		   (cond
		    ((eq exitcmd 'return)
		     ;; sublevel of current
		     (setq org-remember-previous-location
			   (cons (abbreviate-file-name file)
				 (org-get-heading 'notags)))
		     (if reversed
			 (outline-next-heading)
		       (org-end-of-subtree t)
		       (if (not (bolp))
			   (if (looking-at "[ \t]*\n")
			       (beginning-of-line 2)
			     (end-of-line 1)
			     (insert "\n"))))
		     (bookmark-set "org-remember-last-stored")
		     (org-paste-subtree (org-get-valid-level level 1) txt))
		    ((eq exitcmd 'left)
		     ;; before current
		     (bookmark-set "org-remember-last-stored")
		     (org-paste-subtree level txt))
		    ((eq exitcmd 'right)
		     ;; after current
		     (org-end-of-subtree t)
		     (bookmark-set "org-remember-last-stored")
		     (org-paste-subtree level txt))
		    (t (error "This should not happen"))))

		  ((and (bobp) (not reversed))
		   ;; Put it at the end, one level below level 1
		   (save-restriction
		     (widen)
		     (goto-char (point-max))
		     (if (not (bolp)) (newline))
		     (bookmark-set "org-remember-last-stored")
		     (org-paste-subtree (org-get-valid-level 1 1) txt)))

		  ((and (bobp) reversed)
		   ;; Put it at the start, as level 1
		   (save-restriction
		     (widen)
		     (goto-char (point-min))
		     (re-search-forward "^\\*+ " nil t)
		     (beginning-of-line 1)
		     (bookmark-set "org-remember-last-stored")
		     (org-paste-subtree 1 txt)))
		  (t
		   ;; Put it right there, with automatic level determined by
		   ;; org-paste-subtree or from prefix arg
		   (bookmark-set "org-remember-last-stored")
		   (org-paste-subtree
		    (if (numberp current-prefix-arg) current-prefix-arg)
		    txt)))
	    (when remember-save-after-remembering
	      (save-buffer)
	      (if (not visiting) (kill-buffer (current-buffer)))))))))

  t)    ;; return t to indicate that we took care of this note.


(defun org-do-remember (&optional initial)
  "Call remember."
  (remember initial))

(provide 'org-remember)

;;; org-remember.el ends here

;; arch-tag: 497f30d0-4bc3-4097-8622-2d27ac5f2698
