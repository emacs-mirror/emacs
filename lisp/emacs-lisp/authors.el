;;; authors.el --- utility for maintaining Emacs' AUTHORS file

;; Copyright (C) 2000 Free Software Foundation, Inc.

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

;; Use M-x authors RET to create an *Authors* buffer that can used as
;; or merged with Emacs' AUTHORS file.

;;; Code:

(defconst authors-many-files 20
  "Maximum number of files for which to print individual information.
If an author has modified more files, only a single entry is
printed telling how many files he changed, instead of listing each
file individually.")

(defconst authors-aliases
  '(("eliz" . "Eli Zaretskii")
    ("Richard Stallman" . "Richard M. Stallman")
    ("Richard M. Stallman,,," . "Richard M. Stallman")
    ("Richard Stallmao" . "Richard M. Stallman")
    ("rms@gnu.org" . "Richard M. Stallman")
    ("NIIBE Yutaka" . "Yutaka NIIBE")
    ("(saw@cebaf.gov)" . "Stephen A. Wood")
    ("(pmr@legacy.pajato.com)" . "Paul Reilly")
    ("(Eric Youngdale at youngdale@v6550c.nrl.navy.mil)" . "Eric Youngdale")
    ("<Daniel.Pfeiffer@Informatik.START.db.de>" . "Daniel Pfeiffer")
    ("<Daniel.Pfeiffer@Informatik.START.dbp.de>" . "Daniel Pfeiffer") 
    ("(afs@hplb.hpl.hp.com)" . "ignore")
    ("<Use-Author-Address-Header@\\[127.1\\]>" . "ignore")
    ("Code Extracted" . "ignore")
    ("Fsf" . "ignore")
    ("David M. Koppelman, Koppel@Ee.Lsu.Edu" . "David M. Koppelman")
    ("jka@ece.cmu.edu" . "Jay K. Adams")
    ("Per Abhiddenware; you can redistribute it and/or modify" . "Per Abrahamsen")
    ("Andrw Innes" . "Andrew Innes")
    ("Barry Warsaw" . "Barry A. Warsaw")
    ("Barry A. Warsaw, Century Computing, Inc." . "Barry A. Warsaw")
    ("Barry A. Warsaw, ITB" . "Barry A. Warsaw")
    ("Ken'ichi Handa" . "Kenichi Handa")
    ("Bob Chassell" . "Robert J. Chassell")
    ("SL Baur" . "Steven L. Baur")
    ("Steven L Baur" . "Steven L. Baur")
    ("eggert" . "Paul Eggert")
    ("voelker" . "Geoff Voelker")
    ("rms" . "Richard M. Stallman")
    ("Edward M Reingold" . "Edward M. Reingold")
    ("Eric Ludlam" . "Eric M. Ludlam")
    ("Eric Raymond" . "Eric S. Raymond")
    ("Francois Pinard" . "François Pinard")
    ("Fred Pierresteguy" . "Frederic Pierresteguy")
    ("Hallvard B Furuseth" . "Hallvard B. Furuseth")
    ("ISO-2022-JP" . "ignore")
    ("Jens-Ulrik Petersen" . "Jens-Ulrik Holger Petersen")
    ("Christoph.Wedler@sap.com" . "Christoph Wedler")
    ("Jonathan Kamens" . "Jonathan I. Kamens")
    ("Kim Storm" . "Kim F. Storm")
    ("Marcus Daniels" . "Marcus G. Daniels")
    ("Michael I Bushnell" . "Michael I. Bushnell")
    ("Michael I. Bushnell, P/Bsg" . "Michael I. Bushnell")
    ("Reingold Edward M" . "Edward M. Reingold")
    ("Roland B Roberts" . "Roland B. Roberts")
    ("Sam Shteingold" . "Sam Steingold")
    ("Kenichi HANDA" . "Kenichi Handa"))
  "Alist of author aliases.

Each entry is of the form (REGEXP . ALIAS).  If an author's name
matches REGEXP, use ALIAS instead.  The special alias \"ignore\" means
ignore that author.")


(defun authors-add (author file action table)
  "Record that AUTHOR worked on FILE.
ACTION is a keyword symbol describing what he did.  Record file,
author and what he did in hash table TABLE.  See the description of
`authors-scan-change-log' for the structure of the hash table."
  (let* ((value (gethash author table))
	 (entry (assoc file value)))
    (if (null entry)
	(puthash author (cons (list file action) value) table)
      (unless (memq action entry)
	(nconc entry (list action))))))


(defun authors-process-lines (program &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
Signal an error if the program returns with a non-zero exit status."
  (with-temp-buffer
    (let ((status (apply 'call-process program nil (current-buffer) nil args)))
      (unless (eq status 0)
	(error "%s exited with status %s" program status))
      (goto-char (point-min))
      (let (lines)
	(while (not (eobp))
	  (setq lines (cons (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position))
			    lines))
	  (forward-line 1))
	(nreverse lines)))))


(defun authors-canonical-author-name (author)
  "Return a canonicalized form of AUTHOR, an author name.
If AUTHOR has an alias, use that.  Remove email addresses.  Capitalize
words in the author's name."
  (let ((aliases authors-aliases))
    (while aliases
      (when (string-match (car (car aliases)) author)
	(setq author (cdr (car aliases))
	      aliases nil))
      (setq aliases (cdr aliases))))
  (setq author (replace-regexp-in-string "[ \t]*[(<].*$" "" author))
  (setq author (replace-regexp-in-string "^[ \t]+" "" author))
  (setq author (replace-regexp-in-string "[ \t]+$" "" author))
  (capitalize author))


(defun authors-scan-change-log (file table)
  "Scan change log FILE for author information.

For each change mentioned in the log, add an entry to hash table TABLE
under the author's canonical name.

Keys of TABLE are author names.  Values are alists of entries (FILE
ACTION...).  FILE is one file the author worked on.  The rest of the
entry is a list of keyword symbols describing what he did with the
file.

:wrote		means the author wrote the file
:changed	means he changed the file."
  
  (let* ((enable-local-variables t)
	 (enable-local-eval t)
	 (existing-buffer (get-file-buffer file))
	 (buffer (find-file-noselect file))
	 author)
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "^[0-9]\\|^[ \t]+\\* " nil t)
	  (beginning-of-line)
	  (cond ((looking-at "^[0-9]+-[0-9]+-[0-9]+")
		 (skip-chars-forward " \t+:0-9-")
		 (setq author (buffer-substring-no-properties
			       (point) (line-end-position)))
		 (setq author (authors-canonical-author-name author))
		 (forward-line 1))
		((looking-at "^[ \t]+\\*")
		 (let ((line (buffer-substring-no-properties
			      (match-end 0) (line-end-position))))
		   (while (and (not (string-match ":" line))
			       (forward-line 1)
			       (not (looking-at ":\\|^[ \t]*$")))
		     (setq line (concat line
					(buffer-substring-no-properties
					 (line-beginning-position)
					 (line-end-position)))))
		   (when (string-match ":" line)
		     (setq line (substring line 0 (match-beginning 0)))
		     (setq line (replace-regexp-in-string "[[(<{].*$" "" line))
		     (setq line (replace-regexp-in-string "," "" line))
		     (dolist (file (split-string line))
		       (setq file (file-name-nondirectory file))
		       ;(message "%s changed %s" author file)
		       (authors-add author file :changed table)))
		   (forward-line 1)))))))
    (unless existing-buffer
      (kill-buffer buffer))))


(defun authors-scan-el (file table)
  "Scan Lisp file FILE for author information.
TABLE is a hash table to add author information to."
  (let* ((existing-buffer (get-file-buffer file))
	 (enable-local-variables t)
	 (enable-local-eval t)
	 (buffer (find-file-noselect file)))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (and (re-search-forward
		     "^;+[ \t]*\\(Author\\|Commentary\\):[ \t]*" nil t)
		    (not (string= (match-string 1) "Commentary")))
	  ;; Some entries contain a year range in front of the
	  ;; author's name.
	  (skip-chars-forward "-0-9 \t")
	  (let ((author (buffer-substring-no-properties
			 (point) (line-end-position))))
	    (setq author (authors-canonical-author-name author))
	    (setq file (file-name-nondirectory file))
	    (authors-add author file :wrote table)))))
    (unless existing-buffer
      (kill-buffer buffer))))


(defun authors-print (author changes)
  "Insert information about AUTHOR's work on Emacs into the current buffer.
CHANGES is an alist of entries (FILE ACTION...), as produced by
`authors-scan-change-log'."
  (unless (equal author "Ignore")
    (let ((nchanged 0))
    (dolist (change changes)
      (let ((actions (cdr change))
	    (file (car change)))
	(if (memq :wrote actions)
	    (insert author " (wrote) " file "\n")
	  (setq nchanged (1+ nchanged)))))
    (if (> nchanged authors-many-files)
	(insert author " (changed) [changes in more than "
		(int-to-string authors-many-files) " files]\n")
      (dolist (change changes)
	(let ((actions (cdr change))
	      (file (car change)))
	  (unless (memq :wrote actions)
	    (insert author " (changed) " file "\n"))))))))


;;;###autoload
(defun authors (root)
  "Extract author information from change logs and Lisp source files.
ROOT is the root directory under which to find the files.  If called
interactively, ROOT is read from the minibuffer.  Result is a
buffer *Authors* containing authorship information."
  (interactive "DEmacs source directory: ")
  (let ((logs (authors-process-lines "find" root "-name" "ChangeLog*"))
	(table (make-hash-table :test 'equal))
	(buffer-name "*Authors*"))
    (setq root (expand-file-name root))
    (unless (file-exists-p (expand-file-name "src/emacs.c" root))
      (error "Not the root directory of Emacs: %s" root))
    (dolist (log logs)
      (when (string-match "ChangeLog\\(.[0-9]+\\)?$" log)
	(message "Scanning %s..." log)
	(authors-scan-change-log log table)))
    (let ((els (authors-process-lines "find" root "-name" "*.el")))
      (dolist (file els)
	(message "Scanning %s..." file)
	(authors-scan-el file table)))
    (set-buffer (get-buffer-create buffer-name))
    (erase-buffer)
    (maphash #'authors-print table)
    (sort-lines nil (point-min) (point-max))
    (pop-to-buffer buffer-name)))
  

;; authors.el ends here
