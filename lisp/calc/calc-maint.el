;;; calc-maint.el --- maintenance routines for Calc

;; Copyright (C) 1990, 1991, 1992, 1993, 2001 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Colin Walters <walters@debian.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:

;;; Code:

(defun calc-compile ()
  "Compile all parts of Calc.
Unix usage:
     emacs -batch -l calc-maint -f calc-compile"
  (interactive)
  (if (equal (user-full-name) "David Gillespie")
      (load "~/lisp/newbytecomp"))
  (setq byte-compile-verbose t)
  (if noninteractive
      (let ((old-message (symbol-function 'message))
	    (old-write-region (symbol-function 'write-region))
	    (comp-was-func nil)
	    (comp-len 0))
	(unwind-protect
	    (progn
	      (fset 'message (symbol-function 'calc-compile-message))
	      (fset 'write-region (symbol-function 'calc-compile-write-region))
	      (calc-do-compile))
	  (fset 'message old-message)
	  (fset 'write-region old-write-region)))
    (calc-do-compile)))

(defun calc-do-compile ()
  (let ((make-backup-files nil)
	(changed-rules nil)
	(changed-units nil)
	(message-bug (string-match "^18.\\([0-4][0-9]\\|5[0-6]\\)"
				   emacs-version)))
    (setq max-lisp-eval-depth (max 400 max-lisp-eval-depth))
    ;; Enable some irrelevant warnings to avoid compiler bug in 19.29:
    (setq byte-compile-warnings (and (string-match "^19.29" emacs-version)
				     '(obsolete)))

    ;; Make sure we're in the right directory.
    (find-file "calc.el")
    (if (= (buffer-size) 0)
	(error "This command must be used in the Calc source directory"))

    ;; Make sure current directory is in load-path.
    (setq load-path (cons default-directory load-path))
    (load "calc-macs.el" nil t t)
    (provide 'calc)
    (provide 'calc-ext)

    ;; Compile all the source files.
    (let ((files (append
		  '("calc.el" "calc-ext.el")
		  (sort (directory-files
			 default-directory nil
			 "\\`calc-.[^x].*\\.el\\'")
			'string<))))
      (while files
	(if (file-newer-than-file-p (car files) (concat (car files) "c"))
	    (progn
	      (if (string-match "calc-rules" (car files))
		  (setq changed-rules t))
	      (if (string-match "calc-units" (car files))
		  (setq changed-units t))
	      (or message-bug (message ""))
	      (byte-compile-file (car files)))
	  (message "File %s is up to date" (car files)))
	(if (string-match "calc\\(-ext\\)?.el" (car files))
	    (load (concat (car files) "c") nil t t))
	(setq files (cdr files))))

    (if (or changed-units changed-rules)
	(condition-case err
	    (progn

	      ;; Pre-build the units table.
	      (if (and changed-units
		       (not (string-match "Lucid" emacs-version)))
		  (progn
		    (or message-bug (message ""))
		    (save-excursion
		      (calc-create-buffer)
		      (math-build-units-table))
		    (find-file "calc-units.elc")
		    (goto-char (point-max))
		    (insert "\n(setq math-units-table '"
			    (prin1-to-string math-units-table)
			    ")\n")
		    (save-buffer)))

	      ;; Pre-build rewrite rules for j D, j M, etc.
	      (if (and changed-rules (not (string-match "^19" emacs-version)))
		  (let ((rules nil))
		    (or message-bug (message ""))
		    (find-file "calc-rules.elc")
		    (goto-char (point-min))
		    (while (re-search-forward "defun calc-\\([A-Za-z]*Rules\\)"
					      nil t)
		      (setq rules (cons (buffer-substring (match-beginning 1)
							  (match-end 1))
					rules)))
		    (goto-char (point-min))
		    (re-search-forward "\n(defun calc-[A-Za-z]*Rules")
		    (beginning-of-line)
		    (delete-region (point) (point-max))
		    (mapcar (function
			     (lambda (v)
			       (let* ((vv (intern (concat "var-" v)))
				      (val (save-excursion
					     (calc-create-buffer)
					     (calc-var-value vv))))
				 (insert "\n(defun calc-" v " () '"
					 (prin1-to-string val) ")\n"))))
			    (sort rules 'string<))
		    (save-buffer))))
	  (error (message "Unable to pre-build tables %s" err))))
    (message "Done.  Don't forget to install with \"make public\" or \"make private\"")))

(defun calc-compile-message (fmt &rest args)
  (cond ((and (= (length args) 2)
	      (stringp (car args))
	      (string-match ".elc?\\'" (car args))
	      (symbolp (nth 1 args)))
	 (let ((name (symbol-name (nth 1 args))))
	   (princ (if comp-was-func ", " "  "))
	   (if (and comp-was-func (eq (string-match comp-was-func name) 0))
	       (setq name (substring name (1- (length comp-was-func))))
	     (setq comp-was-func (if (string-match "\\`[a-zA-Z]+-" name)
				     (substring name 0 (match-end 0))
				   " ")))
	   (if (> (+ comp-len (length name)) 75)
	       (progn
		 (princ "\n  ")
		 (setq comp-len 0)))
	   (princ name)
	   (send-string-to-terminal "")  ; cause an fflush(stdout)
	   (setq comp-len (+ comp-len 2 (length name)))))
	((and (setq comp-was-func nil
		    comp-len 0)
	      (= (length args) 1)
	      (stringp (car args))
	      (string-match ".elc?\\'" (car args)))
	 (unless (string-match "Saving file %s..." fmt)
	   (funcall old-message fmt (file-name-nondirectory (car args)))))
	((string-match "\\(Preparing\\|Building\\).*\\.\\.\\.$" fmt)
	 (send-string-to-terminal (apply 'format fmt args)))
	((string-match "\\(Preparing\\|Building\\).*\\.\\.\\. *done$" fmt)
	 (send-string-to-terminal "done\n"))
	(t (apply old-message fmt args))))

(defun calc-compile-write-region (start end filename &optional append visit &rest rest)
  (if (eq visit t)
      (set-buffer-auto-saved))
  (if (and (string-match "\\.elc" filename)
	   (= start (point-min))
	   (= end (point-max)))
      (save-excursion
	(goto-char (point-min))
	(if (search-forward "\n(require (quote calc-macs))\n" nil t)
	    (replace-match ""))
	(setq end (point-max))))
  (apply old-write-region start end filename append 'quietly rest)
  (message "Wrote %s" filename)
  nil)



(defun calc-split-tutorial (&optional force)
  (interactive "P")
  (calc-split-manual force 1))


(defun calc-split-reference (&optional force)
  (interactive "P")
  (calc-split-manual force 2))


(defun calc-split-manual (&optional force part)
  "Split the Calc manual into separate Tutorial and Reference manuals.
Use this if your TeX installation is too small-minded to handle
calc.texinfo all at once.
Usage:  C-x C-f calc.texinfo RET
        M-x calc-split-manual RET"
  (interactive "P")
  (or (let ((case-fold-search t))
	(string-match "calc\\.texinfo" (buffer-name)))
      force
      (error "This command should be used in the calc.texinfo buffer"))
  (let ((srcbuf (current-buffer))
	tutpos refpos endpos (maxpos (point-max)))
    (goto-char 1)
    (search-forward "@c [tutorial]")
    (beginning-of-line)
    (setq tutpos (point))
    (search-forward "@c [reference]")
    (beginning-of-line)
    (setq refpos (point))
    (search-forward "@c [end]")
    (beginning-of-line)
    (setq endpos (point))
    (or (eq part 2)
	(progn
	  (find-file "calctut.tex")
	  (erase-buffer)
	  (insert-buffer-substring srcbuf 1 refpos)
	  (insert-buffer-substring srcbuf endpos maxpos)
	  (calc-split-volume "I" "ref" "Tutorial" "Reference")
	  (save-buffer)))
    (or (eq part 1)
	(progn
	  (find-file "calcref.tex")
	  (erase-buffer)
	  (insert-buffer-substring srcbuf 1 tutpos)
	  (insert "\n@tex\n\\global\\advance\\chapno by 1\n@end tex\n")
	  (insert-buffer-substring srcbuf refpos maxpos)
	  (calc-split-volume "II" "tut" "Reference" "Tutorial")
	  (save-buffer)))
    (switch-to-buffer srcbuf)
    (goto-char 1))
  (message (cond ((eq part 1) "Wrote file calctut.tex")
		 ((eq part 2) "Wrote file calcref.tex")
		 (t "Wrote files calctut.tex and calcref.tex"))))

(defun calc-split-volume (number fix name other-name)
  (goto-char 1)
  (search-forward "@c [title]\n")
  (search-forward "Manual")
  (delete-backward-char 6)
  (insert name)
  (search-forward "@c [volume]\n")
  (insert "@sp 1\n@center Volume " number ": " name "\n")
  (let ((pat (format "@c \\[fix-%s \\(.*\\)\\]\n" fix)))
    (while (re-search-forward pat nil t)
      (let ((topic (buffer-substring (match-beginning 1) (match-end 1))))
	(re-search-forward "@\\(p?xref\\){[^}]*}")
	(let ((cmd (buffer-substring (match-beginning 1) (match-end 1))))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (if (equal cmd "pxref") "see" "See")
		  " ``" topic "'' in @emph{the Calc "
		  other-name "}")))))
  (goto-char 1)
  (while (search-forward "@c [when-split]\n" nil t)
    (while (looking-at "@c ")
      (delete-char 3)
      (forward-line 1)))
  (goto-char 1)
  (while (search-forward "@c [not-split]\n" nil t)
    (while (not (looking-at "@c"))
      (insert "@c ")
      (forward-line 1))))


(defun calc-inline-summary ()
  "Make a special \"calcsum.tex\" file to be used with main manual."
  (calc-split-summary nil t))

(defun calc-split-summary (&optional force in-line)
  "Make a special \"calcsum.tex\" file with just the Calc summary."
  (interactive "P")
  (or (let ((case-fold-search t))
	(string-match "calc\\.texinfo" (buffer-name)))
      force
      (error "This command should be used in the calc.texinfo buffer"))
  (let ((srcbuf (current-buffer))
	begpos sumpos endpos midpos)
    (goto-char 1)
    (search-forward "{Calc Manual}")
    (backward-char 1)
    (delete-backward-char 6)
    (insert "Summary")
    (search-forward "@c [begin]")
    (beginning-of-line)
    (setq begpos (point))
    (search-forward "@c [summary]")
    (beginning-of-line)
    (setq sumpos (point))
    (search-forward "@c [end-summary]")
    (beginning-of-line)
    (setq endpos (point))
    (find-file "calcsum.tex")
    (erase-buffer)
    (insert-buffer-substring srcbuf 1 begpos)
    (insert "@tex\n"
	    "\\global\\advance\\appendixno2\n"
	    "\\gdef\\xref#1.{See ``#1.''}\n")
    (setq midpos (point))
    (insert "@end tex\n")
    (insert-buffer-substring srcbuf sumpos endpos)
    (insert "@bye\n")
    (goto-char 1)
    (if (search-forward "{. a b c" nil t)
	(replace-match "{... a b c"))
    (goto-char 1)
    (if in-line
	(let ((buf (current-buffer))
	      (page nil))
	  (find-file "calc.aux")
	  (if (> (buffer-size) 0)
	      (progn
		(goto-char 1)
		(re-search-forward "{Summary-pg}{\\([0-9]+\\)}")
		(setq page (string-to-int (buffer-substring (match-beginning 1)
							    (match-end 1))))))
	  (switch-to-buffer buf)
	  (if page
	      (progn
		(message "Adjusting starting page number to %d" page)
		(goto-char midpos)
		(insert (format "\\global\\pageno=%d\n" page)))
	    (message "Unable to find page number from calc.aux")))
      (if (search-forward "@c smallbook" nil t)
	  (progn   ; activate "smallbook" format for compactness
	    (beginning-of-line)
	    (forward-char 1)
	    (delete-char 2))))
    (let ((buf (current-buffer)))
      (find-file "calc.ky")
      (if (> (buffer-size) 0)
	  (let ((ibuf (current-buffer)))
	    (message "Mixing in page numbers from Key Index (calc.ky)")
	    (switch-to-buffer buf)
	    (goto-char 1)
	    (search-forward "notes at the end")
	    (insert "; the number in italics is\n"
		    "the page number where the command is described")
	    (while (re-search-forward
		    "@r{.*@: *\\([^ ]\\(.*[^ ]\\)?\\) *@:.*@:.*@:\\(.*\\)@:.*}"
		    nil t)
	      (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
		    (pos (match-beginning 3))
		    num)
		(set-buffer ibuf)
		(goto-char 1)
		(let ((p '( ( "I H " . "H I " )  ; oops!
			    ( "@@ ' \"" . "@@" ) ( "h m s" . "@@" )
			    ( "\\\\" . "{\\tt\\indexbackslash }" )
			    ( "_" . "{\\_}" )
			    ( "\\^" . "{\\tt\\hat}" )
			    ( "<" . "{\\tt\\less}" )
			    ( ">" . "{\\tt\\gtr}" )
			    ( "\"" ) ( "@{" ) ( "@}" )
			    ( "~" ) ( "|" ) ( "@@" )
			    ( "\\+" . "{\\tt\\char43}" )
			    ( "# l" . "# L" )
			    ( "I f I" . "f I" ) ( "I f Q" . "f Q" )
			    ( "V &" . "&" ) ( "C-u " . "" ) ))
		      (case-fold-search nil))
		  (while p
		    (if (string-match (car (car p)) key)
			(setq key (concat (substring key 0 (match-beginning 0))
					  (or (cdr (car p))
					      (format "{\\tt\\char'%03o}"
						      (aref key (1- (match-end
								     0)))))
					  (substring key (match-end 0)))))
		    (setq p (cdr p)))
		  (setq num (and (search-forward (format "\\entry {%s}{" key)
						 nil t)
				 (looking-at "[0-9]+")
				 (buffer-substring (point) (match-end 0)))))
		(set-buffer buf)
		(goto-char pos)
		(insert "@pgref{" (or num "") "}")))
	    (goto-char midpos)
	    (insert "\\gdef\\pgref#1{\\hbox to 2em{\\indsl\\hss#1}\\ \\ }\n"))
	(message
	 "Unable to find Key Index (calc.ky); no page numbers inserted"))
      (switch-to-buffer buf))
    (save-buffer))
  (message "Wrote file calcsum.tex"))



(defun calc-public-autoloads ()
  "Modify the public \"default\" file to contain the necessary autoload and
global-set-key commands for Calc."
  (interactive)
  (let ((home default-directory)
	(p load-path)
	instbuf name)
    (while (and p
		(not (file-exists-p
		      (setq name (expand-file-name "default" (car p)))))
		(not (file-exists-p
		      (setq name (expand-file-name "default.el" (car p))))))
      (setq p (cdr p)))
    (unless p
      (error "Unable to find \"default\" file.  Create one and try again"))
    (find-file name)
    (if buffer-read-only (error "No write permission for \"%s\"" buffer-file-name))
    (goto-char (point-max))
    (calc-add-autoloads home "calc-public-autoloads")))

(defun calc-private-autoloads ()
  "Modify the user's \".emacs\" file to contain the necessary autoload and
global-set-key commands for Calc."
  (interactive)
  (let ((home default-directory))
    (find-file "~/.emacs")
    (goto-char (point-max))
    (calc-add-autoloads home "calc-private-autoloads")))

(defun calc-add-autoloads (home cmd)
  (barf-if-buffer-read-only)
  (let (top)
    (if (and (re-search-backward ";;; Commands added by calc-.*-autoloads"
				 nil t)
	     (setq top (point))
	     (search-forward ";;; End of Calc autoloads" nil t))
	(progn
	  (forward-line 1)
	  (message "(Removing previous autoloads)")
	  (delete-region top (point)))
      (insert "\n\n")
      (backward-char 1)))
  (insert ";;; Commands added by " cmd " on "
	  (current-time-string) ".
\(autoload 'calc-dispatch	   \"calc\" \"Calculator Options\" t)
\(autoload 'full-calc		   \"calc\" \"Full-screen Calculator\" t)
\(autoload 'full-calc-keypad	   \"calc\" \"Full-screen X Calculator\" t)
\(autoload 'calc-eval		   \"calc\" \"Use Calculator from Lisp\")
\(autoload 'defmath		   \"calc\" nil t t)
\(autoload 'calc			   \"calc\" \"Calculator Mode\" t)
\(autoload 'quick-calc		   \"calc\" \"Quick Calculator\" t)
\(autoload 'calc-keypad		   \"calc\" \"X windows Calculator\" t)
\(autoload 'calc-embedded	   \"calc\" \"Use Calc inside any buffer\" t)
\(autoload 'calc-embedded-activate  \"calc\" \"Activate =>'s in buffer\" t)
\(autoload 'calc-grab-region	   \"calc\" \"Grab region of Calc data\" t)
\(autoload 'calc-grab-rectangle	   \"calc\" \"Grab rectangle of data\" t)
\(setq load-path (nconc load-path (list \"" (directory-file-name home) "\")))
\(global-set-key \"\\e#\" 'calc-dispatch)
;;; End of Calc autoloads.\n")
  (let ((trim-versions-without-asking t))
    (save-buffer)))

;;; calc-maint.el ends here
