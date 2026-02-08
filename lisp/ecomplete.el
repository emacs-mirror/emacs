;;; ecomplete.el --- electric completion of addresses and the like  -*- lexical-binding:t -*-

;; Copyright (C) 2006-2026 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: mail

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

;; ecomplete stores matches in a file that looks like this:
;;
;; ((mail
;;  ("larsi@gnus.org" 38154 1516109510 "Lars Ingebrigtsen <larsi@gnus.org>")
;;  ("kfogel@red-bean.com" 10 1516065455 "Karl Fogel <kfogel@red-bean.com>")
;;  ...
;;  ))
;;
;; That is, it's an alist map where the key is the "type" of match (so
;; that you can have one list of things for `mail' and one for, say,
;; `twitter').  In each of these sections you then have a list where
;; each item is on the form
;;
;; (KEY TIMES-USED LAST-TIME-USED STRING)
;;
;; If you call `ecomplete-display-matches', it will then display all
;; items that match STRING.  KEY is unique and is used to identify the
;; item, and is used for updates.  For instance, if given the above
;; data, you call
;;
;; (ecomplete-add-item "larsi@gnus.org" 'mail "Lars Magne Ingebrigtsen <larsi@gnus.org>")
;;
;; the "larsi@gnus.org" entry will then be updated with that new STRING.

;; The interface functions are `ecomplete-add-item' and
;; `ecomplete-display-matches', while `ecomplete-setup' should be
;; called to read the .ecompleterc file, and `ecomplete-save' are
;; called to save the file.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup ecomplete nil
  "Electric completion of email addresses and the like."
  :group 'mail)

(defcustom ecomplete-database-file
  (locate-user-emacs-file "ecompleterc" "~/.ecompleterc")
  "The name of the file to store the ecomplete data."
  :type 'file)

(defcustom ecomplete-database-file-coding-system 'iso-2022-7bit
  ;; FIXME: We should transition to `utf-8-emacs-unix' somehow!
  "Coding system used for writing the ecomplete database file."
  :type '(symbol :tag "Coding system"))

(defcustom ecomplete-sort-predicate #'ecomplete-decay
  "Predicate to use when sorting matched ecomplete candidates.
The predicate is called with two arguments that represent the
completion.  Each argument is a list where the first element is
the times the completion has been used, the second is the
timestamp of the most recent usage, and the third item is the
string that was matched."
  :type '(radio (function-item :tag "Sort by usage and newness" ecomplete-decay)
		(function-item :tag "Sort by times used" ecomplete-usage)
		(function-item :tag "Sort by newness" ecomplete-newness)
		(function :tag "Other")))

(defcustom ecomplete-auto-select nil
  "Whether `ecomplete-display-matches' should automatically select a sole option."
  :type 'boolean
  :version "29.1")

(defcustom ecomplete-filter-regexp nil
  "Regular expression of addresses that should not be stored by ecomplete."
  :type '(choice (const :tag "None" nil)
                 (regexp :tag "Regexp"))
  :version "29.1")

;;; Internal variables.

(defvar ecomplete-database nil)

;;;###autoload
(defun ecomplete-setup ()
  "Read the .ecompleterc file."
  (when (file-exists-p ecomplete-database-file)
    (with-temp-buffer
      (let ((coding-system-for-read ecomplete-database-file-coding-system))
	(insert-file-contents ecomplete-database-file)
	(setq ecomplete-database (read (current-buffer)))))))

(defun ecomplete-add-item (type key text &optional force)
  "Add item TEXT of TYPE to the database, using KEY as the identifier.
By default, the longest version of TEXT will be preserved, but if
FORCE is non-nil, use TEXT exactly as is."
  (unless ecomplete-database (ecomplete-setup))
  (unless (and ecomplete-filter-regexp
               (string-match-p ecomplete-filter-regexp key))
    (let ((elems (assq type ecomplete-database))
          (now (time-convert nil 'integer))
          entry)
      (unless elems
        (push (setq elems (list type)) ecomplete-database))
      (if (setq entry (assoc key (cdr elems)))
          (pcase-let ((`(,_key ,count ,_time ,oldtext) entry))
            (setcdr entry (list (1+ count) now
                                ;; Preserve the "more complete" text.
                                (if (or force
                                        (>= (length text) (length oldtext)))
                                    text
                                  oldtext))))
        (nconc elems (list (list key 1 now text)))))))

(defun ecomplete--remove-item (type key)
  "Remove the element of TYPE and KEY from the ecomplete database."
  (unless ecomplete-database
    (ecomplete-setup))
  (let ((elems (assq type ecomplete-database)))
    (unless elems
      (user-error "No elements of type %s" type))
    (let ((entry (assoc key elems)))
      (unless entry
        (user-error "No entry with key %s" key))
      (setcdr elems (delq entry (cdr elems))))))

(defun ecomplete-get-item (type key)
  "Return the text for the item identified by KEY of the required TYPE."
  (assoc key (cdr (assq type ecomplete-database))))

(defun ecomplete-save ()
  "Write the .ecompleterc file."
  ;; If the database is empty, it might be because we haven't called
  ;; `ecomplete-setup', so better not save at all, lest we lose the real
  ;; database!
  (when ecomplete-database
    (with-temp-buffer
      (let ((coding-system-for-write ecomplete-database-file-coding-system))
        (insert "(")
        (cl-loop for (type . elems) in ecomplete-database
	         do
	         (insert (format "(%s\n" type))
	         (dolist (entry elems)
	           (prin1 entry (current-buffer))
	           (insert "\n"))
	         (insert ")\n"))
	(insert ")")
	(write-region (point-min) (point-max)
		      ecomplete-database-file nil 'silent)))))

(defun ecomplete-get-matches (type match)
  (let* ((elems (cdr (assq type ecomplete-database)))
	 (match (regexp-quote match))
	 (candidates
	  (sort
	   (cl-loop for (_key count time text) in elems
		    when (string-match match text)
		    collect (list count time text))
           ecomplete-sort-predicate)))
    (when (> (length candidates) 10)
      (setcdr (nthcdr 10 candidates) nil))
    (unless (zerop (length candidates))
      (with-temp-buffer
	(dolist (candidate candidates)
	  (insert (caddr candidate) "\n"))
	(goto-char (point-min))
	(put-text-property (point) (1+ (point)) 'ecomplete t)
	(while (re-search-forward match nil t)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face 'isearch))
	(buffer-string)))))

(defun ecomplete-display-matches (type word &optional choose)
  "Display the top-rated elements TYPE that match WORD.
If CHOOSE, allow the user to choose interactively between the
matches.

Auto-select when `ecomplete-auto-select' is
non-nil and there is only a single completion option available."
  (let* ((matches (ecomplete-get-matches type word))
         (match-list (and matches (split-string matches "\n")))
         (max-lines (and matches (- (length match-list) 2)))
	 (line 0)
	 (message-log-max nil)
	 command highlight)
    (if (not matches)
	(progn
	  (message "No ecomplete matches")
	  nil)
      (if (not choose)
	  (progn
	    (message "%s" matches)
	    nil)
        (if (and ecomplete-auto-select
                 max-lines
                 (zerop max-lines))
            ;; Auto-select when only one option is available.
            (nth 0 match-list)
          ;; Interactively choose from the filtered completions.
	  (let ((local-map (make-sparse-keymap))
                (prev-func (lambda () (setq line (max (1- line) 0))))
                (next-func (lambda () (setq line (min (1+ line) max-lines))))
	        selected)
	    (define-key local-map (kbd "RET")
                        (lambda () (setq selected (nth line match-list))))
	    (define-key local-map (kbd "M-n") next-func)
	    (define-key local-map (kbd "<down>") next-func)
	    (define-key local-map (kbd "M-p") prev-func)
	    (define-key local-map (kbd "<up>") prev-func)
	    (let ((overriding-local-map local-map))
              (setq highlight (ecomplete-highlight-match-line matches line))
	      (while (and (null selected)
			  (setq command (read-key-sequence highlight))
			  (lookup-key local-map command))
	        (apply (key-binding command) nil)
	        (setq highlight (ecomplete-highlight-match-line matches line))))
	    (message (or selected "Abort"))
            selected))))))

(defun ecomplete-highlight-match-line (matches line)
  (with-temp-buffer
    (insert matches)
    (goto-char (point-min))
    (forward-line line)
    (save-restriction
      (narrow-to-region (point) (line-end-position))
      (while (not (eobp))
	;; Put the 'region face on any characters on this line that
	;; aren't already highlighted.
	(unless (get-text-property (point) 'face)
	  (put-text-property (point) (1+ (point)) 'face 'highlight))
	(forward-char 1)))
    (buffer-string)))

(defun ecomplete-usage (l1 l2)
  (> (car l1) (car l2)))

(defun ecomplete-newness (l1 l2)
  (> (cadr l1) (cadr l2)))

(defun ecomplete-decay (l1 l2)
  (> (ecomplete-decay-1 l1) (ecomplete-decay-1 l2)))

(defun ecomplete-decay-1 (elem)
  ;; We subtract 5% from the item for each week it hasn't been used.
  (/ (car elem)
     (expt 1.05 (/ (float-time (time-since (cadr elem)))
                   (* 7 24 60 60)))))

;; `ecomplete-get-matches' uses substring matching, so also use the `substring'
;; style by default.
(add-to-list 'completion-category-defaults
             '(ecomplete (styles basic substring)))

(defun ecomplete-completion-table (type)
  "Return a completion-table suitable for TYPE."
  (lambda (string pred action)
    (pcase action
      (`(boundaries . ,_) nil)
      ('metadata `(metadata (category . ecomplete)
                            (display-sort-function . ,#'identity)
                            (cycle-sort-function . ,#'identity)))
      (_
       (let* ((elems (cdr (assq type ecomplete-database)))
	      (candidates
	       (mapcar (lambda (x) (nth 2 x))
                       (sort
	                (cl-loop for x in elems
		                 when (string-prefix-p string (nth 3 x)
                                                       completion-ignore-case)
		                 collect (cdr x))
                        ecomplete-sort-predicate))))
         (complete-with-action action candidates string pred))))))

(defun ecomplete--prompt-type ()
  (unless ecomplete-database
    (ecomplete-setup))
  (if (length= ecomplete-database 1)
      (caar ecomplete-database)
    (completing-read "Item type to edit: "
                     (mapcar #'car ecomplete-database)
                     nil t)))

(defun ecomplete-edit ()
  "Prompt for an ecomplete item and allow editing it."
  (interactive)
  (let* ((type (ecomplete--prompt-type))
         (data (cdr (assq type ecomplete-database)))
         (key (completing-read "Key to edit: " data nil t))
         (new (read-string "New value (empty to remove): "
                           (nth 3 (assoc key data)))))
    (if (zerop (length new))
        (progn
          (ecomplete--remove-item type key)
          (message "Removed %s" key))
      (ecomplete-add-item type key new t)
      (message "Updated %s to %s" key new))
    (ecomplete-save)))

(defun ecomplete-remove ()
  "Remove from the ecomplete database the entries matching a regexp.
Prompt for the regexp to match the database entries to be removed."
  (interactive)
  (let* ((type (ecomplete--prompt-type))
         (data (cdr (assq type ecomplete-database)))
         (match (read-regexp (format "Remove %s keys matching (regexp): "
                                     type)))
         (elems (seq-filter (lambda (elem)
                              (string-match-p match (car elem)))
                            data)))
    (if (length= elems 0)
        (message "No matching entries for %s" match)
      (when (yes-or-no-p (format "Delete %s matching ecomplete %s? "
                                 (length elems)
                                 (if (length= elems 1)
                                     "entry"
                                   "entries")))
        (dolist (elem elems)
          (ecomplete--remove-item type (car elem)))
        (ecomplete-save)
        (message "Deleted entries")))))

(provide 'ecomplete)

;;; ecomplete.el ends here
