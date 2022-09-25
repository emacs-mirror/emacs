;;; gnus-mlspl.el --- a group params-based mail splitting mechanism  -*- lexical-binding: t; -*-

;; Copyright (C) 1998-2022 Free Software Foundation, Inc.

;; Author: Alexandre Oliva <oliva@lsd.ic.unicamp.br>
;; Keywords: news, mail

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

;;; Code:

(require 'gnus)
(require 'gnus-sum)
(require 'gnus-group)
(require 'nnmail)

(defvar gnus-group-split-updated-hook nil
  "Hook called just after `nnmail-split-fancy' is updated by
`gnus-group-split-update'.")

(defvar gnus-group-split-default-catch-all-group "mail.misc"
  "Group name (or arbitrary fancy split) with default splitting rules.
Used by `gnus-group-split' and `gnus-group-split-update' as a fallback
split, in case none of the group-based splits matches.")

;;;###autoload
(defun gnus-group-split-setup (&optional auto-update catch-all)
  "Set up the split for `nnmail-split-fancy'.
Sets things up so that nnmail-split-fancy is used for mail
splitting, and defines the variable nnmail-split-fancy according with
group parameters.

If AUTO-UPDATE is non-nil (prefix argument accepted, if called
interactively), it makes sure nnmail-split-fancy is re-computed before
getting new mail, by adding `gnus-group-split-update' to
`gnus-get-top-new-news-hook'.

A non-nil CATCH-ALL replaces the current value of
`gnus-group-split-default-catch-all-group'.  This variable is only used
by gnus-group-split-update, and only when its CATCH-ALL argument is
nil.  This argument may contain any fancy split, that will be added as
the last split in a `|' split produced by `gnus-group-split-fancy',
unless overridden by any group marked as a catch-all group.  Typical
uses are as simple as the name of a default mail group, but more
elaborate fancy splits may also be useful to split mail that doesn't
match any of the group-specified splitting rules.  See
`gnus-group-split-fancy' for details."
  (interactive "P")
  (setq nnmail-split-methods 'nnmail-split-fancy)
  (when catch-all
    (setq gnus-group-split-default-catch-all-group catch-all))
  (add-hook
   (if auto-update
       'gnus-get-top-new-news-hook
     ;; Split updating requires `gnus-newsrc-hashtb' to be
     ;; initialized; the read newsrc hook is the only hook that comes
     ;; after initialization, but before checking for new news.
     'gnus-read-newsrc-el-hook)
   #'gnus-group-split-update))

;;;###autoload
(defun gnus-group-split-update (&optional catch-all)
  "Computes `nnmail-split-fancy' from group params and CATCH-ALL.
It does this by calling (gnus-group-split-fancy nil nil CATCH-ALL).

If CATCH-ALL is nil, `gnus-group-split-default-catch-all-group' is used
instead.  This variable is set by `gnus-group-split-setup'."
  (interactive)
  (setq nnmail-split-fancy
	(gnus-group-split-fancy
	 nil (null nnmail-crosspost)
	 (or catch-all gnus-group-split-default-catch-all-group)))
  (run-hooks 'gnus-group-split-updated-hook))

;;;###autoload
(defun gnus-group-split ()
  "Use information from group parameters in order to split mail.
See `gnus-group-split-fancy' for more information.

`gnus-group-split' is a valid value for `nnmail-split-methods'."
  (let (nnmail-split-fancy)
    (gnus-group-split-update)
    (nnmail-split-fancy)))

;;;###autoload
(defun gnus-group-split-fancy
  (&optional groups no-crosspost catch-all)
  "Uses information from group parameters in order to split mail.
It can be embedded into `nnmail-split-fancy' lists with the SPLIT

\(: gnus-group-split-fancy GROUPS NO-CROSSPOST CATCH-ALL)

GROUPS may be a regular expression or a list of group names, that will
be used to select candidate groups.  If it is omitted or nil, all
existing groups are considered.

if NO-CROSSPOST is omitted or nil, a & split will be returned,
otherwise, a | split, that does not allow crossposting, will be
returned.

For each selected group, a SPLIT is composed like this: if SPLIT-SPEC
is specified, this split is returned as-is (unless it is nil: in this
case, the group is ignored).  Otherwise, if TO-ADDRESS, TO-LIST and/or
EXTRA-ALIASES are specified, a regexp that matches any of them is
constructed (extra-aliases may be a list).  Additionally, if
SPLIT-REGEXP is specified, the regexp will be extended so that it
matches this regexp too, and if SPLIT-EXCLUDE is specified, RESTRICT
clauses will be generated.

If CATCH-ALL is nil, no catch-all handling is performed, regardless of
catch-all marks in group parameters.  Otherwise, if there is no
selected group whose SPLIT-REGEXP matches the empty string, nor is
there a selected group whose SPLIT-SPEC is `catch-all', this fancy
split (say, a group name) will be appended to the returned SPLIT list,
as the last element of a `|' SPLIT.

For example, given the following group parameters:

nnml:mail.bar:
\((to-address . \"bar@femail.com\")
 (split-regexp . \".*@femail\\\\.com\"))
nnml:mail.foo:
\((to-list . \"foo@nowhere.gov\")
 (extra-aliases \"foo@localhost\" \"foo-redist@home\")
 (split-exclude \"bugs-foo\" \"rambling-foo\")
 (admin-address . \"foo-request@nowhere.gov\"))
nnml:mail.others:
\((split-spec . catch-all))

Calling (gnus-group-split-fancy nil nil \"mail.others\") returns:

\(| (& (any \"\\\\(bar@femail\\\\.com\\\\|.*@femail\\\\.com\\\\)\"
	   \"mail.bar\")
      (any \"\\\\(foo@nowhere\\\\.gov\\\\|foo@localhost\\\\|foo-redist@home\\\\)\"
	   - \"bugs-foo\" - \"rambling-foo\" \"mail.foo\"))
   \"mail.others\")"
  (let ((group-names (if (and (listp groups)
                             (not (null groups)))
			 groups
		       (delete-dups
			(delq nil
			      (mapcar
			       (lambda (info)
				 (let ((group (gnus-info-group info)))
				   (if (or (not groups)
					   (and (stringp groups)
						(string-match groups group)))
				       group)))
			       (append gnus-newsrc-alist gnus-parameters))))))
       split)
    (dolist (group group-names)
      (let ((params (gnus-group-find-parameter group)))
       ;; Skip groups without param (or nonexistent)
       (when (not (null params))
         (let ((split-spec (assoc 'split-spec params)) group-clean)
           ;; Remove backend from group name
           (setq group-clean (string-search ":" group))
	    (setq group-clean
		  (if group-clean
		      (substring group (1+ group-clean))
		    group))
	    (if split-spec
		(when (setq split-spec (cdr split-spec))
		  (if (eq split-spec 'catch-all)
		      ;; Emit catch-all only when requested
		      (when catch-all
			(setq catch-all group-clean))
		    ;; Append split-spec to the main split
		    (push split-spec split)))
	      ;; Let's deduce split-spec from other params
	      (let ((to-address (cdr (assoc 'to-address params)))
		    (to-list (cdr (assoc 'to-list params)))
		    (extra-aliases (cdr (assoc 'extra-aliases params)))
		    (split-regexp (cdr (assoc 'split-regexp params)))
		    (split-exclude (cdr (assoc 'split-exclude params)))
		    (match-list (cdr (assoc 'match-list params))))
		(when (or to-address to-list extra-aliases split-regexp)
		  ;; regexp-quote to-address, to-list and extra-aliases
		  ;; and add them all to split-regexp
		  (setq split-regexp
			(concat
			 "\\("
			 (mapconcat
			  #'identity
			  (append
			   (and to-address (list (regexp-quote to-address)))
			   (and to-list (list (regexp-quote to-list)))
			   (and extra-aliases
				(if (listp extra-aliases)
				    (mapcar #'regexp-quote extra-aliases)
				  (list extra-aliases)))
			   (and split-regexp (list split-regexp)))
			  "\\|")
			 "\\)"))
		  ;; Now create the new SPLIT
		  (let ((split-regexp-with-list-ids
			 (string-replace "@" "[@.]" split-regexp))
			(exclude
			 ;; Generate RESTRICTs for SPLIT-EXCLUDEs.
			 (if (listp split-exclude)
			     (apply #'append
				    (mapcar (lambda (arg) (list '- arg))
					    split-exclude))
			   (list '- split-exclude))))

		    (if match-list
			;; Match RFC2919 IDs or mail addresses
			(push (append
			       (list 'list split-regexp-with-list-ids)
			       exclude
			       (list group-clean))
			      split)
		      (push (append
			     (list 'any split-regexp)
			     exclude
			     (list group-clean))
			    split)))
		  ;; If it matches the empty string, it is a catch-all
		  (when (string-match split-regexp "")
		    (setq catch-all nil)))))))))
    ;; Add catch-all if not crossposting
    (if (and catch-all no-crosspost)
	(push catch-all split))
    ;; Move it to the tail, while arranging that SPLITs appear in the
    ;; same order as groups.
    (setq split (reverse split))
    ;; Decide whether to accept cross-postings or not.
    (push (if no-crosspost '| '&) split)
    ;; Even if we can cross-post, catch-all should not get
    ;; cross-posts.
    (if (and catch-all (not no-crosspost))
	(setq split (list '| split catch-all)))
    split))

(provide 'gnus-mlspl)

;;; gnus-mlspl.el ends here
