;;; uniquify.el --- unique buffer names dependent on file name

;; Copyright (c) 1989, 1995, 1996, 1997 Free Software Foundation, Inc.

;; Author: Dick King <king@reasoning.com>
;; Maintainer: Michael Ernst <mernst@alum.mit.edu>
;; Created: 15 May 86

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

;; Emacs's standard method for making buffer names unique adds <2>, <3>,
;; etc. to the end of (all but one of) the buffers.  This file replaces
;; that behavior, for buffers visiting files and dired buffers, with a
;; uniquification that adds parts of the file name until the buffer names
;; are unique.  For instance, buffers visiting /u/mernst/tmp/Makefile and
;; /usr/projects/zaphod/Makefile would be named Makefile|tmp and
;; Makefile|zaphod, respectively (instead of Makefile and Makefile<2>).
;; Other buffer name styles are also available.

;; To use this file, do (require 'uniquify)
;; and set uniquify-buffer-name-style to one of its non-nil alternative values.

;; For other options, see "User-visible variables", below.

;; A version of uniquify.el that works under Emacs 18, Emacs 19, XEmacs,
;; and InfoDock is available from the maintainer.

;; Doesn't work under NT when backslash is used as a path separator (forward
;;   slash path separator works fine).  To fix, check system-type against
;;   'windows-nt, write a routine that breaks paths down into components.
;;   (Surprisingly, there isn't one built in.)

;;; Change Log:

;; Originally by Dick King <king@reasoning.com> 15 May 86
;; Converted for Emacs 18 by Stephen Gildea <gildea@lcs.mit.edu>
;; Make uniquify-min-dir-content 0 truly non-invasive.  gildea 23 May 89
;; Some cleanup.  uniquify-min-dir-content default 0.  gildea 01 Jun 89
;; Don't rename to "".  Michael Ernst <mernst@theory.lcs.mit.edu> 15 Jun 94
;; Add kill-buffer-hook.  Kenneth Manheimer <ken.manheimer@nist.gov> 09 May 95
;; Add advice for rename-buffer and create-file-buffer, handle dired buffers,
;;  kill-buffer-rationalize-buffer-names-p, documentation.  mernst 24 May 95
;; Remove free variables, fix typos.  mernst 5 Jun 95
;; Efficiently support Emacs 19.27 & earlier.  ken.manheimer, mernst 10 Jun 95
;; Rename user options to "uniquify-...", add uniquify-reverse-dir-content-p,
;;  add uniquify-ask-about-buffer-names-p.  king, mernst 13 Jun 95
;; Prefix functions by "uniquify-..."; rename mnemonic-buffer-names to
;;  uniquify-buffer-name-style; add 'forward and 'post-forward-angle-brackets
;;  styles; remove uniquify-reverse-dir-content-p; add
;;  uniquify-trailing-separator-p.  mernst 4 Aug 95
;; Don't call expand-file-name on nil.  mernst 7 Jan 96
;; Check whether list-buffers-directory is bound.  mernst 11 Oct 96
;; Ignore non-file non-dired buffers. Colin Rafferty <craffert@ml.com> 3 Mar 97
;; Use last component, not "", for file name of directories.  mernst 27 Jun 97
;; Use directory-file-name; code cleanup.  mernst 6 Sep 97
;; Add uniquify-ignore-buffers-re.
;;  Andre Srinivasan <andre@visigenic.com> 9 Sep 97

;; Valuable feedback was provided by
;; Paul Smith <psmith@baynetworks.com>,
;; Alastair Burt <burt@dfki.uni-kl.de>,
;; Bob Weiner <weiner@footloose.sps.mot.com>,
;; Albert L. Ting <alt@vlibs.com>,
;; gyro@reasoning.com,
;; Bryan O'Sullivan <bos@eng.sun.com>.


;;; Code:

(provide 'uniquify)

;;; User-visible variables

(defgroup uniquify nil
  "Unique buffer names dependent on file name"
  :group 'applications)


(defcustom uniquify-buffer-name-style nil
  "*If non-nil, buffer names are uniquified with parts of directory name.
The value determines the buffer name style and is one of `forward',
`reverse', `post-forward' (the default), or `post-forward-angle-brackets'.
For example, files `/foo/bar/mumble/name' and `/baz/quux/mumble/name'
would have the following buffer names in the various styles:
  forward        bar/mumble/name  quux/mumble/name
  reverse        name\\mumble\\bar  name\\mumble\\quux
  post-forward   name|bar/mumble  name|quux/mumble
  post-forward-angle-brackets   name<bar/mumble>  name<quux/mumble>
  nil            name  name<2>"
  :type '(radio (const forward)
		(const reverse)
		(const post-forward)
		(const post-forward-angle-brackets)
		(const :tag "standard Emacs behavior (nil)" nil))
  :require 'uniquify
  :group 'uniquify)

(defcustom uniquify-after-kill-buffer-p nil
  "*If non-nil, rerationalize buffer names after a buffer has been killed.
This can be dangerous if Emacs Lisp code is keeping track of buffers by their
names (rather than keeping pointers to the buffers themselves)."
  :type 'boolean
  :group 'uniquify)

(defcustom uniquify-ask-about-buffer-names-p nil
  "*If non-nil, permit user to choose names for buffers with same base file.
If the user chooses to name a buffer, uniquification is preempted and no
other buffer names are changed."
  :type 'boolean
  :group 'uniquify)

(defcustom uniquify-ignore-buffers-re nil
  "*Regular expression matching buffer names that should not be uniquified.
For instance, set this to \"^draft-[0-9]+$\" to avoid having uniquify rename
draft buffers even if `uniquify-after-kill-buffer-p' is non-nil and the
visited file name isn't the same as that of the buffer."
  :type '(choice (const :tag "Uniquify all buffers" nil) regexp)
  :group 'uniquify)

(defcustom uniquify-min-dir-content 0
  "*Minimum number of directory name components included in buffer name."
  :type 'integer
  :group 'uniquify)

(defcustom uniquify-separator nil
  "*String separator for buffer name components.
When `uniquify-buffer-name-style' is `post-forward', separates
base file name from directory part in buffer names (default \"|\").
When `uniquify-buffer-name-style' is `reverse', separates all
file name components (default \"\\\")."
  :type '(choice (const nil) string)
  :group 'uniquify)

(defcustom uniquify-trailing-separator-p nil
  "*If non-nil, add a file name separator to dired buffer names.
If `uniquify-buffer-name-style' is `forward', add the separator at the end;
if it is `reverse', add the separator at the beginning; otherwise, this
variable is ignored."
  :type 'boolean
  :group 'uniquify)


;;; Utilities

(defmacro uniquify-push (item list)
  `(setq ,list (cons ,item ,list)))

;; For directories, return the last component, not the empty string.
(defun uniquify-file-name-nondirectory (file-name)
  (file-name-nondirectory (directory-file-name file-name)))

;; uniquify-fix-list data structure
(defmacro uniquify-fix-item-base (a)
  `(car ,a))
(defmacro uniquify-fix-item-filename (a)
  `(car (cdr ,a)))
(defmacro uniquify-fix-item-buffer (a)
  `(car (cdr (cdr ,a))))
;; Not a macro: passed to mapcar.
(defun uniquify-fix-item-unrationalized-buffer (item)
  (or (car (cdr (cdr (cdr item)))) nil))	;maybe better in the future

(defun uniquify-fix-item-filename-lessp (fixlist1 fixlist2)
  (uniquify-filename-lessp (uniquify-fix-item-filename fixlist1)
			   (uniquify-fix-item-filename fixlist2)))

;; Internal variables used free
(defvar uniquify-non-file-buffer-names nil)
(defvar uniquify-possibly-resolvable nil)

;;; Main entry point.

(defun uniquify-rationalize-file-buffer-names (&optional newbuffile newbuf)
  "Makes file buffer names unique by adding segments from file name.
If `uniquify-min-dir-content' > 0, always pulls that many
file name elements.  Arguments cause only a subset of buffers to be renamed."
  (interactive)
  (let (fix-list
	uniquify-non-file-buffer-names)
    (let ((buffers (buffer-list)))
      (while buffers
	(let* ((buffer (car buffers))
	       (bfn (if (eq buffer newbuf)
                        (and newbuffile
			     (expand-file-name
			      (if (file-directory-p newbuffile)
				  (directory-file-name newbuffile)
			       newbuffile)))
		      (uniquify-buffer-file-name buffer)))
	       (rawname (and bfn (uniquify-file-name-nondirectory bfn)))
	       (deserving (and rawname
			       (not (and uniquify-ignore-buffers-re
					 (string-match uniquify-ignore-buffers-re
						       (buffer-name buffer))))
			       (or (not newbuffile)
				   (equal rawname
					  (uniquify-file-name-nondirectory newbuffile))))))
	  (if deserving
	      (uniquify-push (list rawname bfn buffer nil) fix-list)
	    (uniquify-push (list (buffer-name buffer))
			   uniquify-non-file-buffer-names)))
	(setq buffers (cdr buffers))))
    ;; selects buffers whose names may need changing, and others that
    ;; may conflict.
    (setq fix-list
	  (sort fix-list 'uniquify-fix-item-filename-lessp))
    ;; bringing conflicting names together
    (uniquify-rationalize-a-list fix-list uniquify-min-dir-content)
    (mapcar 'uniquify-fix-item-unrationalized-buffer fix-list)))

;; uniquify's version of buffer-file-name; result never contains trailing slash
(defun uniquify-buffer-file-name (buffer)
  "Return name of file BUFFER is visiting, or nil if none.
Works on dired buffers and ordinary file-visiting buffers, but no others."
  (or (buffer-file-name buffer)
      (and (featurep 'dired)
      (save-excursion
	(set-buffer buffer)
	(and
	 (eq major-mode 'dired-mode)	; do nothing if not a dired buffer
	 (if (boundp 'list-buffers-directory) ; XEmacs mightn't define this
	     (and list-buffers-directory
		  (directory-file-name list-buffers-directory))
	   ;; don't use default-directory if dired-directory is nil
	   (and dired-directory
		(expand-file-name
		 (directory-file-name
		  (if (consp dired-directory)
		      (car dired-directory)
		    dired-directory))))))))))

;; This examines the filename components in reverse order.
(defun uniquify-filename-lessp (s1 s2)
  (let ((s1f (uniquify-file-name-nondirectory s1))
	(s2f (uniquify-file-name-nondirectory s2)))
    (and (not (equal s2f ""))
	 (or (string-lessp s1f s2f)
	     (and (equal s1f s2f)
		  (let ((s1d (file-name-directory s1))
			(s2d (file-name-directory s2)))
		    (and (not (<= (length s2d) 1))
			 (or (<= (length s1d) 1)
			     (uniquify-filename-lessp
			      (substring s1d 0 -1)
			      (substring s2d 0 -1))))))))))

(defun uniquify-rationalize-a-list (fix-list depth)
  (let (conflicting-sublist	; all elements have the same proposed name
	(old-name "")
	proposed-name uniquify-possibly-resolvable)
    (while fix-list
      (let ((item (car fix-list)))
	(setq proposed-name (uniquify-get-proposed-name item depth))
	(if (not (equal proposed-name old-name))
	    (progn
	      (uniquify-rationalize-conflicting-sublist
	       conflicting-sublist old-name depth)
	      (setq conflicting-sublist nil)))
	(uniquify-push item conflicting-sublist)
	(setq old-name proposed-name))
      (setq fix-list (cdr fix-list)))
    (uniquify-rationalize-conflicting-sublist
     conflicting-sublist old-name depth)))

(defun uniquify-get-proposed-name (item depth)
  (let (index
	(extra-string "")
	(n depth)
	(base (uniquify-fix-item-base item))
	(fn (uniquify-fix-item-filename item)))
    (while (and (> n 0)
		(setq index (string-match
			     (concat "\\(^\\|/[^/]*\\)/"
				     (regexp-quote extra-string)
				     (regexp-quote base)
				     "\\'")
			     fn)))
      (setq extra-string (substring fn
				    (if (zerop index) 0 (1+ index))
				    ;; (- (length base)) fails for base = "".
				    ;; Equivalently, we could have used
				    ;; (apply 'substring ...
				    ;;        (and (not (string= "" base))
				    ;;             (list (- (length base)))))
				    (- (length fn) (length base)))
	    n (1- n)))
    (if (zerop n) (setq uniquify-possibly-resolvable t))


    ;; Distinguish directories by adding extra separator.
    (if (and uniquify-trailing-separator-p
	     (file-directory-p fn)
	     (not (string-equal base "")))
	(cond ((eq uniquify-buffer-name-style 'forward)
	       (setq base (concat base "/")))
	      ((eq uniquify-buffer-name-style 'reverse)
	       (setq base (concat (or uniquify-separator "\\") base)))))

    ;; Trim trailing separator on directory part
    (if (and (not (string-equal extra-string ""))
	     (or (eq uniquify-buffer-name-style 'post-forward)
		 (eq uniquify-buffer-name-style 'post-forward-angle-brackets)))
	(setq extra-string (substring extra-string 0
				      (- (length extra-string) 1))))

    (cond ((string-equal extra-string "")
	   base)
	  ((string-equal base "")
	   extra-string)
	  ((eq uniquify-buffer-name-style 'forward)
	   (concat extra-string base))
	  ((eq uniquify-buffer-name-style 'reverse)
	   (concat base (uniquify-reverse-components extra-string)))
	  ((eq uniquify-buffer-name-style 'post-forward)
	   (concat base (or uniquify-separator "|") extra-string))
	  ((eq uniquify-buffer-name-style 'post-forward-angle-brackets)
	   (concat base "<" extra-string ">"))
	  (t (error "Bad value for uniquify-buffer-name-style: %s"
		    uniquify-buffer-name-style)))))


;; Deal with conflicting-sublist, all of whose elements have identical
;; "base" components.
(defun uniquify-rationalize-conflicting-sublist (conflicting-sublist old-name depth)
  (or (null conflicting-sublist)
      (and (null (cdr conflicting-sublist))
	   (not (assoc old-name uniquify-non-file-buffer-names))
	   (or (and (not (string= old-name ""))
		    (uniquify-rename-buffer (car conflicting-sublist) old-name))
	       t))
      (if uniquify-possibly-resolvable
	  (uniquify-rationalize-a-list conflicting-sublist (1+ depth)))))

(defun uniquify-rename-buffer (item newname)
  (let ((buffer (uniquify-fix-item-buffer item)))
    (if (not (equal newname (buffer-name buffer)))
	(let ((unset (current-buffer))
	      ;; avoid hooks on rename-buffer
	      (uniquify-buffer-name-style nil))
	  (set-buffer buffer)
	  (rename-buffer newname)
	  (set-buffer unset))))
  (rplaca (nthcdr 3 item) t))

(defun uniquify-reverse-components (instring)
  (let ((sofar '())
	(cursor 0)
	(len (length instring))
	(sep (or uniquify-separator "\\")))
    (while (< cursor len)
      (if (= (aref instring cursor) ?/)
          (setq sofar (cons sep sofar)
                cursor (1+ cursor))
        (let ((first-slash (or (string-match "/" instring cursor) len)))
          (setq sofar (cons (substring instring cursor first-slash) sofar)
                cursor first-slash))))
    (apply (function concat) sofar)))


;;; Hooks from the rest of Emacs

;; The logical place to put all this code is in generate-new-buffer-name.
;; It's written in C, so we would add a generate-new-buffer-name-function
;; which, if non-nil, would be called instead of the C.  One problem with
;; that is that generate-new-buffer-name takes a potential buffer name as
;; its argument -- not other information, such as what file the buffer will
;; visit.

;; The below solution works because generate-new-buffer-name is called
;; only by rename-buffer (which, as of 19.29, is never called from C) and
;; generate-new-buffer, which is called only by Lisp functions
;; create-file-buffer and rename-uniquely.  Rename-uniquely generally
;; isn't used for buffers visiting files, so it's sufficient to hook
;; rename-buffer and create-file-buffer.  (Setting find-file-hooks isn't
;; sufficient.)

(defadvice rename-buffer (after rename-buffer-uniquify activate)
  "Uniquify buffer names with parts of directory name."
  (if (and uniquify-buffer-name-style
	   ;; UNIQUE argument
	   (ad-get-arg 1))
      (progn
	(if uniquify-after-kill-buffer-p
	    ;; call with no argument; rationalize vs. old name as well as new
	    (uniquify-rationalize-file-buffer-names)
	  ;; call with argument: rationalize vs. new name only
	  (uniquify-rationalize-file-buffer-names
	   (uniquify-buffer-file-name (current-buffer)) (current-buffer)))
	(setq ad-return-value (buffer-name (current-buffer))))))

(defadvice create-file-buffer (after create-file-buffer-uniquify activate)
  "Uniquify buffer names with parts of directory name."
  (if uniquify-buffer-name-style
      (uniquify-rationalize-file-buffer-names (ad-get-arg 0) ad-return-value)))

;; Buffer deletion
;; Rerationalize after a buffer is killed, to reduce coinciding buffer names.
;; This mechanism uses `kill-buffer-hook', which runs *before* deletion.
;; That means that the kill-buffer-hook function cannot just delete the
;; buffer -- it has to set something to do the rationalization *later*.
;; It actually puts another function on `post-command-hook'.  This other
;; function runs the rationalization and then removes itself from the hook.
;; Is there a better way to accomplish this?
;; (This ought to set some global variables so the work is done only for
;; buffers with names similar to the deleted buffer.  -MDE)

(defun delay-uniquify-rationalize-file-buffer-names ()
  "Add `delayed-uniquify-rationalize-file-buffer-names' to `post-command-hook'.
For use on, eg, `kill-buffer-hook', to rationalize *after* buffer deletion."
  (if (and uniquify-buffer-name-style
	   uniquify-after-kill-buffer-p)
      (add-hook 'post-command-hook
		'delayed-uniquify-rationalize-file-buffer-names)))

(defun delayed-uniquify-rationalize-file-buffer-names ()
  "Rerationalize buffer names and remove self from `post-command-hook'.
See also `delay-rationalize-file-buffer-names' for hook setter."
  (uniquify-rationalize-file-buffer-names)
  (remove-hook 'post-command-hook
	       'delayed-uniquify-rationalize-file-buffer-names))

(add-hook 'kill-buffer-hook 'delay-uniquify-rationalize-file-buffer-names)

;;; uniquify.el ends here
