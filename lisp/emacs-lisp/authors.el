;;; authors.el --- utility for maintaining Emacs' AUTHORS file -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

;; Author: Gerd Moellmann <gerd@gnu.org>
;; Maintainer: Kim F. Storm <storm@cua.dk>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Use M-x authors RET to create an *Authors* buffer that can used as
;; or merged with Emacs' AUTHORS file.

;;; Code:

(defvar authors-coding-system 'iso-2022-7bit
  "Coding system used in the AUTHORS file.")

(defconst authors-many-files 20
  "Maximum number of files for which to print individual information.
If an author has modified more files, only the names of the most
frequently modified files are printed and a count of the additional
files.")

(defconst authors-aliases
  '(
    ("Andrew Csillag" "Drew Csillag")
    ("Anna M. Bigatti" "Anna Bigatti")
    ("Barry A. Warsaw" "Barry A. Warsaw, Century Computing, Inc."
     "Barry A. Warsaw, ITB" "Barry Warsaw")
    ("Bj,Av(Brn Torkelsson" "Bjorn Torkelsson")
    ("Brian Fox" "Brian J. Fox")
    ("Christoph Wedler" "Christoph.Wedler@sap.com")
    ("Daniel Pfeiffer" "<Daniel.Pfeiffer@Informatik.START.db.de>"
     "<Daniel.Pfeiffer@Informatik.START.dbp.de>")
    ("David Gillespie" "Dave Gillespie")
    ("David K,Ae(Bgedal" "David K..edal")
    ("David M. Koppelman" "David M. Koppelman, Koppel@Ee.Lsu.Edu"
     "David Koppelman")
    ("David M. Smith" "David Smith" "David M Smith")
    ("Ed L. Cashin" "Ed L Cashin")
    ("Edward M. Reingold" "Ed Reingold" "Edward M Reingold"
     "Reingold Edward M")
    ("Eli Zaretskii" "eliz")
    ("Emilio C. Lopes" "Emilio Lopes")
    ("Era Eriksson" "Era@Iki.Fi")
    ("Eric M. Ludlam" "Eric Ludlam")
    ("Eric S. Raymond" "Eric Raymond")
    ("Eric Youngdale" "(Eric Youngdale at youngdale@v6550c.nrl.navy.mil)")
    ("Francis J. Wright" "Dr Francis J. Wright" "Francis Wright")
    ("Fran,Ag(Bois Pinard" "Francois Pinard")
    ("Francesco Potort,Al(B" "Francesco Potorti" "Francesco Potorti`")
    ("Frederic Pierresteguy" "Fred Pierresteguy")
    ("Geoff Voelker" "voelker")
    ("Gerd M,Av(Bllmann" "Gerd Moellmann")
    ("Hallvard B. Furuseth" "Hallvard B Furuseth")
    ("Hrvoje Nik,B9(Bi,Bf(B" "Hrvoje Niksic")
    (nil "(afs@hplb.hpl.hp.com)")
    (nil "<Use-Author-Address-Header@\\[127.1\\]>")
    (nil "Code Extracted")
    (nil "\\`FSF")
    (nil "ISO-2022-JP")
    ("Jaeyoun Chung" "Jae-youn Chung" "Jae-you Chung" "Chung Jae-youn")
    ("Jan Dj,Ad(Brv" "Jan D." "Jan Djarv")
    ("Jay K. Adams" "jka@ece.cmu.edu" "Jay Adams")
    ("J,Ai(Br,At(Bme Marant" "J,bi(Br,bt(Bme Marant" "Jerome Marant")
    ("Jens-Ulrik Holger Petersen" "Jens-Ulrik Petersen")
    ("Jeremy Bertram Maitin-Shepard" "Jeremy Maitin-Shepard")
    ("Johan Bockg,Ae(Brd" "Johan Bockgard")
    ("John W. Eaton" "John Eaton")
    ("Jonathan I. Kamens" "Jonathan Kamens")
    ("Joseph Arceneaux" "Joe Arceneaux")
    ("Juan Le,As(Bn Lahoz Garc,Am(Ba" "Juan-Leon Lahoz Garcia")
    ("K. Shane Hartman" "Shane Hartman")
    ("Kai Gro,A_(Bjohann" "Kai Grossjohann" "Kai Gro,b_(Bjohann"
     "Kai.Grossjohann@Cs.Uni-Dortmund.De"
     "Kai.Grossjohann@Gmx.Net")
    ("Karl Berry" "K. Berry")
    ("K,Aa(Broly L$,1 q(Brentey" "K,Aa(Broly L,Bu(Brentey" "L$,1 q(Brentey K,Aa(Broly")
    ("Kazushi Marukawa" "Kazushi")
    ("Ken Manheimer" "Kenneth Manheimer")
    ("Kenichi Handa" "Ken'ichi Handa" "Kenichi HANDA")
    ("Kevin Greiner" "Kevin J. Greiner")
    ("Kim F. Storm" "Kim Storm")
    ("Kyle Jones" "Kyle E. Jones")
    ("Marcus G. Daniels" "Marcus Daniels")
    ("Mark D. Baushke" "Mark D Baushke")
    ("Agust,Am(Bn Mart,Am(Bn" "Agustin Martin")
    ("Martin Lorentzon" "Martin Lorentzson")
    ("Matt Swift" "Matthew Swift")
    ("Michael R. Mauger" "Michael Mauger")
    ("Michael D. Ernst" "Michael Ernst")
    ("Micha,Ak(Bl Cadilhac" "Michael Cadilhac")
    ("Michael I. Bushnell" "Michael I Bushnell" "Michael I. Bushnell, P/Bsg")
    ("Mikio Nakajima" "Nakajima Mikio")
    ("Paul Eggert" "eggert")
    ("Paul Reilly" "(pmr@legacy.pajato.com)")
    ("Pavel Jan,Bm(Bk" "Pavel Jan,Am(Bk Ml." "Pavel Jan,Am(Bk" "Pavel@Janik.Cz")
    ("Per Abrahamsen" "Per Abhiddenware")
    ("Peter S. Galbraith" "Peter Galbraith")
    ("Peter Runestig" "Peter 'luna' Runestig")
    ("Peter S. Galbraith" "Peter S Galbraith")
    ("Richard M. Stallman" "Richard M. Stallman,,," "Richard Stallman"
     "rms" "rms@gnu.org")
    ("Robert J. Chassell" "Bob Chassell")
    ("Roland B. Roberts" "Roland B Roberts" "Roland Roberts")
    ("Rui-Tao Dong" "Rui-Tao Dong ~{6-Hpln~}")
    ("Sacha Chua" "Sandra Jean Chua")
    ("Sam Steingold" "Sam Shteingold")
    ("Satyaki Das" "Indexed search by Satyaki Das")
    ("Stefan Monnier" "Stefan")
    ("Stephen A. Wood" "(saw@cebaf.gov)")
    ("Steven L. Baur" "SL Baur" "Steven L Baur")
    ("Takaaki Ota" "Tak Ota")
    ("Takahashi Naoto" "Naoto Takahashi")
    ("Teodor Zlatanov" "Ted Zlatanov")
    ("Torbj,Av(Brn Axelsson" "Torbjvrn Axelsson")
    ("Torbj,Av(Brn Einarsson" "Torbj.*rn Einarsson")
    ("Toru Tomabechi" "Toru Tomabechi,")
    ("Vincent Del Vecchio" "Vince Del Vecchio")
    ("William M. Perry" "Bill Perry")
    ("Wlodzimierz Bzyl" "W.*dek Bzyl")
    ("Yutaka NIIBE" "NIIBE Yutaka")
    )
  "Alist of author aliases.

Each entry is of the form (REALNAME REGEXP...).  If an author's name
matches one of the REGEXPs, use REALNAME instead.
If REALNAME is nil, ignore that author.")


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

(defconst authors-ignored-files
  '("external-lisp"
    "lock" "share-lib" "local-lisp"
    "noleim-Makefile.in"
    "NEWS" "PROBLEMS" "FAQ" "AUTHORS" "FOR-RELEASE" "TODO")
  "List of files and directories to ignore.
Changes to files in this list are not listed.")

(defconst authors-fixed-entries
  '(("Richard M. Stallman" :wrote "[The original GNU Emacs and numerous files]")
    ("Joseph Arceneaux" :wrote "xrdb.c")
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
    ("Kyle Jones" :wrote "mldrag.el")
    ("Henry Kautz" :wrote "bib-mode.el")
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
    ("Roland B. Roberts" :changed "files.el" "sort.el" "vmsproc.el"
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
    ("Dale R. Worley" :changed "mail-extr.el")
    ("Jamie Zawinski" :changed "bytecode.c" :wrote "disass.el" "tar-mode.el"))
  "Actions taken from the original, manually (un)maintained AUTHORS file.")


(defconst authors-valid-file-names
  '("aclocal.m4"
    "makedist.bat"
    "make-delta")
  "File names which are valid, but no longer exists (or cannot be
found) in the repository.")

(defconst authors-renamed-files-alist
  '(("nt.c" . "w32.c") ("nt.h" . "w32.h")
    ("ntheap.c" . "w32heap.c") ("ntheap.h" . "w32heap.h")
    ("ntinevt.c" . "w32inevt.c") ("ntinevt.h" . "w32inevt.h")
    ("ntproc.c" . "w32proc.c")
    ("w32console.c" . "w32term.c")
    ("unexnt.c" . "unexw32.c")
    ("s/windowsnt.h" . "s/ms-w32.h")
    ("config.emacs" . "configure")
    ("GETTING.GNU.SOFTWARE" . "FTP")
    ("leim-Makefile" . "leim/Makefile")
    ("leim-Makefile.in" . "leim/Makefile.in")
    ("emacs-lisp/testcover-ses.el" . "tcover-ses.el")
    ("emacs-lisp/testcover-unsafep.el" . "tcover-unsafep.el")
    ("INSTALL-CVS" . "INSTALL.CVS")
    )
  "Alist of files which have been renamed during their lifetime.
Elements are (OLDNAME . NEWNAME).")

(defconst authors-renamed-files-regexps
  '(("^m/m-\\(.*\\.h\\)$" . "m/\\1")
    ("^m-\\(.*\\.h\\)$" . "\\1")
    ("^s/s-\\(.*\\.h\\)$" . "s/\\1")
    ("^s-\\(.*\\.h\\)$" . "\\1")
    ("^s/[-.a-zA-Z0-9_]+\\.h$" . t)
    ("\\(.*\\)\\.cmd$" . "\\1.bat")
    ("\\.bat$" . t)
    ("\\.[ch]$" . t)
    ("\\.el$" . t)
    ("\\.ps$" . t)
    ("\\.texi?$" . t)
    ("\\.texinfo$" . t)
    ("\\.xml?$" . t)
    ("\\.x[pb]m$" . t)
    ("\\.[xp]bm$" . t)
    ("^paths\\." . t)
    ("^install\\." . t)
    )
  "List regexps and rewriting rules for renamed files.
Elements are (REGEXP . REPLACE).  If REPLACE is a string, the file
name matching REGEXP is replaced by REPLACE using `replace-string'.
Otherwise, the file name is accepted as is.")

(defvar authors-checked-files-alist)
(defvar authors-invalid-file-names)

(defun authors-canonical-file-name (file log-file pos author)
  "Return canonical file name for FILE found in LOG-FILE at POS for AUTHOR.
Checks whether FILE is a valid (existing) file name, has been renamed,
or is on the list of removed files.  Returns the non-diretory part of
the file name."
  (let ((entry (assoc file authors-checked-files-alist))
	relname
	valid)
    (if entry
	(cdr entry)
      (setq relname (file-name-nondirectory file))
      (if (or (member relname authors-valid-file-names)
	      (file-exists-p file)
	      (file-exists-p relname)
	      (file-exists-p (concat "etc/" relname)))
	  (setq valid relname)
	(setq valid (assoc file authors-renamed-files-alist))
	(if valid
	    (setq valid (cdr valid))
	  (let ((rules authors-renamed-files-regexps))
	    (while rules
	      (if (string-match (car (car rules)) file)
		  (setq valid (if (stringp (cdr (car rules)))
				  (file-name-nondirectory
				   (replace-match (cdr (car rules)) t nil file))
				relname)
			rules nil))
	      (setq rules (cdr rules))))))
      (setq authors-checked-files-alist
	    (cons (cons file valid) authors-checked-files-alist))
      (unless (or valid
		  (string-match "[*]" file)
		  (string-match "^[0-9.]+$" file))
	(setq authors-invalid-file-names
	      (cons (format "%s:%d: unrecognized `%s' for %s"
			    log-file
			    (1+ (count-lines (point-min) pos))
			    file author)
		    authors-invalid-file-names)))
      valid)))

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
FILE is considered obsolete if it matches one of the regular expressions
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
  (unless (or (member file authors-ignored-files)
	      (authors-obsolete-file-p file)
	      (equal author ""))
    (let* ((value (gethash author table))
	   (entry (assoc file value))
	   slot)
      (if (null entry)
	  (puthash author (cons (list file (cons action 1)) value) table)
	(if (setq slot (assoc action (cdr entry)))
	    (setcdr slot (1+ (cdr slot)))
	  (nconc entry (list (cons action 1))))))))


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
  (let* ((aliases authors-aliases)
	 regexps realname)
    (while aliases
      (setq realname (car (car aliases))
	    regexps (cdr (car aliases))
	    aliases (cdr aliases))
      (while regexps
	(if (string-match (car regexps) author)
	    (setq author realname
		  regexps nil
		  aliases nil)
	  (setq regexps (cdr regexps))))))
  (when author
    (setq author (replace-regexp-in-string "[ \t]*[(<].*$" "" author))
    (setq author (replace-regexp-in-string "\`[ \t]+" "" author))
    (setq author (replace-regexp-in-string "[ \t]+$" "" author))
    (setq author (replace-regexp-in-string "[ \t]+" " " author))
    (unless (string-match "[-, \t]" author)
      (setq author ""))
    (capitalize author)))

(defun authors-scan-change-log (log-file table)
  "Scan change log LOG-FILE for author information.

For each change mentioned in the log, add an entry to hash table TABLE
under the author's canonical name.

Keys of TABLE are author names.  Values are alists of entries (FILE
\(ACTION . COUNT) ...).  FILE is one file the author worked on.  The
rest of the entry is a list of keyword symbols describing what he did
with the file and the number of each action.

:wrote		means the author wrote the file
:changed	means he changed the file COUNT times."

  (let* ((enable-local-variables t)
	 (enable-local-eval t)
	 (existing-buffer (get-file-buffer log-file))
	 (buffer (find-file-noselect log-file))
	 author file pos)
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "^[0-9]\\|^[ \t]+\\* " nil t)
	  (beginning-of-line)
	  (setq pos (point))
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
		       (when (setq file (authors-canonical-file-name file log-file pos author))
			 ;;(message "%s changed %s" author file)
			 (authors-add author file :changed table))))
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

(defvar authors-author-list)

(defun authors-add-to-author-list (author changes)
  "Insert information about AUTHOR's work on Emacs into `authors-author-list'.
CHANGES is an alist of entries (FILE (ACTION . COUNT) ...), as produced by
`authors-scan-change-log'.
The element added to `authors-author-list' is (AUTHOR WROTE CHANGED), where
WROTE and CHANGED are lists of the files written and changed by AUTHOR."
  (when author
    (let ((nchanged 0)
	  wrote-list
	  changed-list)
      (dolist (change changes)
	(let ((actions (cdr change))
	      (file (car change))
	      slot)
	  (if (assq :wrote actions)
	      (setq wrote-list
		    (cons
		     (if (authors-public-domain-p file)
			 (concat file " (public domain)")
		       file)
		     wrote-list))
	    (setq changed-list
		  (cons (cons file (cdr (assq :changed actions)))
			changed-list)))))
      (if wrote-list
	  (setq wrote-list (sort wrote-list 'string-lessp)))
      (when changed-list
	(setq changed-list (sort changed-list
				 (lambda (a b)
				   (if (= (cdr a) (cdr b))
				       (string-lessp (car a) (car b))
				     (> (cdr a) (cdr b))))))
	(setq nchanged (length changed-list))
	(setq changed-list (mapcar 'car changed-list)))
      (if (> (- nchanged authors-many-files) 2)
	  (setcdr (nthcdr authors-many-files changed-list)
		  (list (format "and %d other files" (- nchanged authors-many-files)))))
      (setq authors-author-list
	    (cons (list author wrote-list changed-list)
		  authors-author-list)))))

(defun authors (root)
  "Extract author information from change logs and Lisp source files.
ROOT is the root directory under which to find the files.  If called
interactively, ROOT is read from the minibuffer.
Result is a buffer *Authors* containing authorship information, and a
buffer *Authors Errors* containing references to unknown files."
  (interactive "DEmacs source directory: ")
  (setq root (expand-file-name root))
  (let ((logs (authors-process-lines "find" root "-name" "ChangeLog*"))
	(table (make-hash-table :test 'equal))
	(buffer-name "*Authors*")
	authors-checked-files-alist
	authors-invalid-file-names)
    (authors-add-fixed-entries table)
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
    (message "Generating buffer %s..." buffer-name)
    (set-buffer (get-buffer-create buffer-name))
    (erase-buffer)
    (set-buffer-file-coding-system authors-coding-system)
    (insert
"Many people have contributed code included in the Free Software
Foundation's distribution of GNU Emacs.  To show our appreciation for
their public spirit, we list here in alphabetical order a condensed
list of their contributions.\n")
    (let (authors-author-list a)
      (maphash #'authors-add-to-author-list table)
      (setq authors-author-list
	    (sort authors-author-list
		  (lambda (a b) (string-lessp (car a) (car b)))))
      (dolist (a authors-author-list)
	(let ((author (car a))
	      (wrote (nth 1 a))
	      (changed (nth 2 a))
	      file)
	(insert "\n" author ": ")
	(when wrote
	  (insert "wrote")
	  (dolist (file wrote)
	    (if (> (+ (current-column) (length file)) 72)
	      (insert "\n "))
	    (insert " " file))
	  (insert "\n"))
	(when changed
	  (if wrote
	      (insert "and "))
	  (insert "changed")
	  (dolist (file changed)
	    (if (> (+ (current-column) (length file)) 72)
		(insert "\n "))
	    (insert " " file))
	  (insert "\n")))))
    (insert "\nLocal" " Variables:\ncoding: "
	    (symbol-name authors-coding-system) "\nEnd:\n")
    (message "Generating buffer %s... done" buffer-name)
    (unless noninteractive
      (when authors-invalid-file-names
	(with-current-buffer (get-buffer-create "*Authors Errors*")
	  (erase-buffer)
	  (set-buffer-file-coding-system authors-coding-system)
	  (insert "Unrecognized file entries found:\n\n")
	  (mapcar (lambda (f) (if (not (string-match "^[A-Za-z]+$" f)) (insert f "\n")))
		  (sort authors-invalid-file-names 'string-lessp))
	  (goto-char (point-min))
	  (compilation-mode)
	  (message "Errors were found.  See buffer %s" (buffer-name))))
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

(provide 'authors)

;;; arch-tag: 659d5900-5ff2-43b0-954c-a315cc1e4dc1
;;; authors.el ends here
