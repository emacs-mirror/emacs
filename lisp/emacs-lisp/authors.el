;;; authors.el --- utility for maintaining Emacs' AUTHORS file

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Gerd Moellmann <gerd@gnu.org>
;; Maintainer: FSF
;; Keywords: maint

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
    ("W{\L}Odek Bzyl" . "Wlodzimierz Bzyl")
    ("Kenneth Manheimer" . "Ken Manheimer")
    ("Kenichi HANDA" . "Kenichi Handa")
    ("Jay Adams" . "Jay R. Adams")
    ("Joe Arceneaux" . "Josef Arceneaux")
    ("K. Berry" . "Karl Berry")
    ("Michael Ernst" . "Michael D. Ernst")
    ("Dave Gillespie" . "David Gillespie")
    ("Shane Hartman" . "K. Shane Hartman")
    ("Francesco Potorti`" . "Francesco Potorti")
    ("Roland Roberts" . "Roland B. Roberts")
    ("David Smith" . "David M. Smith")
    )
  "Alist of author aliases.

Each entry is of the form (REGEXP . ALIAS).  If an author's name
matches REGEXP, use ALIAS instead.  The special alias \"ignore\" means
ignore that author.")


(defvar authors-public-domain-files
  '("auto-show\\.el"
    "form-d2\\.el"
    "emerge\\.el"
    "unused\\.el"
    "vi\\.el"
    "feedmail\\.el"
    "mailpost\\.el"
    "hanoi\\.el"
    "meese\\.el"
    "studly\\.el"
    "modula2\\.el")
  "List of regexps matching files for which the FSF doesn't need papers.")


(defvar authors-obsolete-files-regexps
  '("vc-\\*\\.el$"
    "spec.txt$"
    "vc-\\(rcs\\|cvs\\|sccs\\)-hooks\\.el$")
  "List of regexps matching obsolete files.
Changes to files matching one of the regexps in this list are not
listed.")


(defconst authors-fixed-entries
  '(("Joe Arceneax" :wrote "xrdb.c")
    ("Blitz Product Development Corporation" :wrote "ispell.el")
    ("Frank Bresz" :wrote "diff.el")
    ("David M. Brown" :wrote "array.el")
    ("Gary Byers" :changed "xenix.h")
    ("Shawn M. Carey" :wrote "freebsd.h")
    ("Eric Decker"  :changed "hp9000s800.h" "hpux.h" "sysdep.c")
    ("Lawrence R. Dodd" :wrote "dired-x.el")
    ("Viktor Dukhovni" :wrote "unexsunos4.c")
    ("Paul Eggert" :wrote "rcs2log" "vcdiff")
    ("Fred Fish" :changed "unexec.c")
    ("Tim Fleehart" :wrote "makefile.nt")
    ("Keith Gabryelski" :wrote "hexl.c")
    ("Kevin Gallagher" :wrote "flow-ctrl.el")
    ("Howard Gayle" :wrote "disp-table.el" "iso-syntax.el" "casetab.c")
    ("Stephen Gildea" :wrote "refcard.tex" "mh-funcs.el" "mh-pick.el")
    ("David Gillespie" :wrote "cl.texinfo")
    ("Hewlett-Packard" :changed "emacsclient.c" "emacsserver.c"
     "server.el" "keyboard.c")
    ("Thomas Horsley" :wrote "cxux.h" "cxux7.h")
    ("Indiana University Foundation" :changed "buffer.c" "buffer.h"
     "indent.c" "search.c" "xdisp.c" "region-cache.c" "region-cache.h")
    ("International Business Machines" :changed "emacs.c" "fileio.c"
     "ibmrt.h" "process.c" "sysdep.c" "unexec.c" "ibmrt-aix.h")
    ("Ishikawa Chiaki" :changed "aviion.h" "dgux.h")
    ("Michael K. Johnson" :changed "configure.in" "emacs.c" "intel386.h"
     "mem-limits.h" "process.c" "template.h" "sysdep.c" "syssignal.h" "systty.h" "unexec.c"
      "ymakefile" "linux.h")
    ("Kyle E. Jones" :wrote "mldrag.el")
    ("Kenry Kautz" :wrote "bib-mode.el")
    ("Joseph M. Kelsey" :changed "fileio.c" "vms-pwd.h" "vmsfns.c" "dir.h"
     "uaf.h")
    ("Sam Kendall" :changed "etags.c" "etags.el")
    ("Richard King" :wrote "backquote.el" "userlock.el" "filelock.c")
    ("Larry Kolodney" :wrote "cvtmail.c")
    ("Sebastian Kremer" :changed "add-log.el")
    ("Mark Lambert" :changed "process.c" "process.h")
    ("Aaron Larson" :changed "bibtex.el")
    ("James R. Larus" :wrote "mh-e.el")
    ("Lars Lindberg" :changed "dabbrev.el" :wrote "imenu.el")
    ("Lucid, Inc." :changed "bytecode.c" "byte-opt.el" "byte-run.el"
     "bytecomp.el" "delsel.el" "disass.el" "faces.el" "font-lock.el"
     "lmenu.el" "lselect.el" "mailabbrev.el" "select.el" "xfaces.c"
     "xselect.c")
    ("MCC" :changed "etags.c" "emacsclient.c" "emacsserver.c" "movemail.c"
     "rmail.el" "rmailedit.el" "rmailkwd.el"
     "rmailmsc.el" "rmailout.el" "rmailsum.el" "scribe.el"
     "server.el" "lisp.h" "sysdep.c" "unexec.c" :wrote "xmenu.c")
    ("Niall Mansfield" :changed "etags.c")
    ("Brian Marick" :wrote "hideif.el")
    ("Marko Kohtala" :changed "info.el")
    ("Sidney Markowitz" :changed "doctor.el")
    ("Richard Mlynarik" :wrote "env.c" "ehelp.el")
    ("Mosur Mohan" :changed "etags.c")
    ("Jeff Morgenthaler" :changed "flow-ctrl.el" "vt200.el" "vt201.el"
     "vt220.el" "vt240.el")
    ("Motorola" :changed "buff-menu.el")
    ("Hiroshi Nakano" :changed "ralloc.c")
    ("Sundar Narasimhan" :changed "rnewspost.el")
    ("NeXT, Inc." :wrote "unexnext.c")
    ("Mark Neale" :changed "fortran.el")
    ("Martin Neitzel" :changed "sc.el")
    ("Andrew Oram" :changed "miscellaneous changes to files in man/"
     "man/calendar.texi")
    ("Frederic Pierresteguy" :wrote "widget.c")
    ("Michael D. Prange" :changed "tex-mode.el")
    ("Paul Reilly" :wrote "gux5-4r2.h" "dgux5-4r3.h")
    ("Roland B Roberts" :changed "files.el" "sort.el" "vmsproc.el"
     "buffer.h" "callproc.c" "dired.c" "process.c" "sysdep.c" "systty.h"
     "vmspaths.h" "build.com" "compile.com" "kepteditor.com" "precomp.com"
     :wrote "logout.com" "mailemacs.com")
    ("Guillermo J. Rozas" :wrote "fakemail.c")
    ("Wolfgang Rupprecht" :changed "lisp-mode.el" "loadup.el"
     "sort.el" "alloc.c" "callint.c"
     "config.h.in" "crt0.c" "data.c" "fns.c"
     "lisp.h" "lread.c" "sun3.h"
     "print.c" "ymakefile" :wrote "float-sup.el" "floatfns.c")
    ("Schlumberger Technology Corporation" :changed "gud.el")
    ("Gregor Schmid" :wrote "tcl-mode.el")
    ("Rainer Schoepf" :wrote "alpha.h" "unexalpha.c")
    ("William Sommerfeld" :wrote "emacsclient.c" "emacsserver.c" "scribe.el")
    ("Leigh Stoller" :changed "emacsclient.c" "emacsserver.c" "server.el")
    ("Steve Strassman" :wrote "spook.el")
    ("Shinichirou Sugou" :changed "etags.c")
    ("Sun Microsystems, Inc" :changed "emacsclient.c" "emacsserver.c"
     "server.el" :wrote "emacs.icon" "emacstool.1" "emacstool.c" "sun-curs.el"
     "sun-fns.el" "sun-mouse.el" "sun.el" "sunfns.c")
    ("Kayvan Sylvan" :changed "sc.el")
    ("Spencer Thomas" :changed "emacsclient.c" "emacsserver.c" "server.el"
     "dabbrev.el" "unexec.c" "tcp.c" "gnus.texi")
    ("Jonathan Vail" :changed "vc.el")
    ("James Van Artsdalen" :changed "usg5-4.h" "unexec.c")
    ("Geoff Voelker" :wrote "src/makefile.nt" "lisp/makefile.nt" "winnt.el"
     "nt.c" "nt.h" "ntheap.c" "ntheap.h" "ntinevt.c"
     "ntproc.c" "ntterm.c" "windowsnt.h")
    ("Morten Welinder" :wrote "dosfns.c" "[many MSDOS files]" "msdos.h")
    ("Pace Willisson" :wrote "ispell.el")
    ("Garrett Wollman" :changed "sendmail.el")
    ("Dale Worley" :changed "mail-extr.el")
    ("Jamie Zawinski" :changed "bytecode.c" :wrote "disass.el" "tar-mode.el"))
  "Actions taken from the original, manually (un)maintained AUTHORS file.")


(defun authors-add-fixed-entries (table)
  "Add actions from `authors-fixed-entries' to TABLE."
  (dolist (entry authors-fixed-entries)
    (let ((author (car entry))
	  action)
      (dolist (item (cdr entry))
	(if (symbolp item)
	    (setq action item)
	  (authors-add author item action table))))))

	   
(defun authors-obsolete-file-p (file)
  "Return non-nil if FILE is obsolete.
FILE is considered obsolete if it matches on of the regular expressions
from `authors-obsolete-files-regexps'."
  (let (obsolete-p
	(regexps authors-obsolete-files-regexps))
    (while (and regexps (not obsolete-p))
      (setq obsolete-p (string-match (car regexps) file)
	    regexps (cdr regexps)))
    obsolete-p))


(defun authors-add (author file action table)
  "Record that AUTHOR worked on FILE.
ACTION is a keyword symbol describing what he did.  Record file,
author and what he did in hash table TABLE.  See the description of
`authors-scan-change-log' for the structure of the hash table."
  (unless (or (authors-obsolete-file-p file)
	      (equal author ""))
    (let* ((value (gethash author table))
	   (entry (assoc file value)))
      (if (null entry)
	  (puthash author (cons (list file action) value) table)
	(unless (memq action entry)
	  (nconc entry (list action)))))))


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


(defun authors-public-domain-p (file)
  "Return t if FILE is a file that was put in public domain."
  (let ((public-domain-p nil)
	(list authors-public-domain-files))
    (while (and list (not public-domain-p))
      (when (string-match (car list) file)
	(setq public-domain-p t))
      (setq list (cdr list)))
    public-domain-p))


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
	    (progn
	      (insert author " (wrote) " file)
	      (when (authors-public-domain-p file)
		(insert " (public domain)"))
	      (insert "\n"))
	  (setq nchanged (1+ nchanged)))))
    (if (> nchanged authors-many-files)
	(insert author " (changed) [more than "
		(int-to-string authors-many-files) " files]\n")
      (dolist (change changes)
	(let ((actions (cdr change))
	      (file (car change)))
	  (unless (memq :wrote actions)
	    (insert author " (changed) " file "\n"))))))))


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
    (authors-add-fixed-entries table)
    (unless (file-exists-p (expand-file-name "src/emacs.c" root))
      (error "Not the root directory of Emacs: %s" root))
    (dolist (log logs)
      (when (and (string-match "ChangeLog\\(.[0-9]+\\)?$" log)
		 (not (string-match "/lispref/" log)))
	(message "Scanning %s..." log)
	(authors-scan-change-log log table)))
    (let ((els (authors-process-lines "find" root "-name" "*.el")))
      (dolist (file els)
	(message "Scanning %s..." file)
	(authors-scan-el file table)))
    (set-buffer (get-buffer-create buffer-name))
    (erase-buffer)
    (set-buffer-file-coding-system 'iso-2022-7bit)
    (maphash #'authors-print table)
    (sort-lines nil (point-min) (point-max))
    (insert "\nLocal" " Variables:\ncoding: iso-2022-7bit\nEnd:\n")
    (unless noninteractive
      (pop-to-buffer buffer-name))))


(defun batch-update-authors ()
  "Produce an AUTHORS file.
Call this function in batch mode with two command line arguments FILE
and ROOT.  FILE is the file to write, ROOT is the root directory of
the Emacs source tree, from which to build the file."
  (unless noninteractive
    (error "`batch-update-authors' is to be used only with -batch"))
  (when (/= (length command-line-args-left) 2)
    (error "Call `batch-update-authors' with the name of the file to write"))
  (let* ((file (pop command-line-args-left))
	 (root (pop command-line-args-left)))
    (authors root)
    (write-file file)))

;; authors.el ends here
