;;; mailabbrev.el --- abbrev-expansion of mail aliases.

;;; Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;; Maintainer: Jamie Zawinski <jwz@lucid.com>
;; Created: 19 Oct 90
;; Keywords: mail

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This file ensures that, when the point is in a To:, CC:, BCC:, or From: 
;;; field, word-abbrevs are defined for each of your mail aliases.  These
;;; aliases will be defined from your .mailrc file (or the file specified by
;;; the MAILRC environment variable) if it exists.  Your mail aliases will
;;; expand any time you type a word-delimiter at the end of an abbreviation.
;;;
;;; What you see is what you get: no abbreviations will be expanded after you
;;; have sent the mail, unlike the old system.  This means you don't suffer
;;; the annoyance of having the system do things behind your back -- if an
;;; address you typed is going to be rewritten, you know it immediately,
;;; instead of after the mail has been sent and it's too late to do anything
;;; about it.  You will never again be screwed because you forgot to delete an
;;; old alias from your .mailrc when a new local user arrives and is given a
;;; userid which conflicts with one of your aliases, for example.
;;;
;;; Your mail alias abbrevs will be in effect only when the point is in an
;;; appropriate header field.  When in the body of the message, or other
;;; header fields, the mail aliases will not expand.  Rather, the normal
;;; mode-specific abbrev table (mail-mode-abbrev-table) will be used if 
;;; defined.  So if you use mail-mode specific abbrevs, this code will not
;;; adversely affect you.  You can control which header fields the abbrevs
;;; are used in by changing the variable mail-abbrev-mode-regexp.
;;;
;;; If auto-fill mode is on, abbrevs will wrap at commas instead of at word
;;; boundaries; also, header continuation-lines will be properly indented.
;;;
;;; You can also insert a mail alias with mail-interactive-insert-alias
;;; (bound to C-c C-a), which prompts you for an alias (with completion)
;;; and inserts its expansion at point.
;;;
;;; This file fixes a bug in the old system which prohibited your .mailrc
;;; file from having lines like
;;;
;;;     alias someone "John Doe <doe@quux.com>"
;;;
;;; That is, if you want an address to have embedded spaces, simply surround it
;;; with double-quotes.  This is necessary because the format of the .mailrc
;;; file bogusly uses spaces as address delimiters.  The following line defines
;;; an alias which expands to three addresses:
;;;
;;;     alias foobar addr-1 addr-2 "address three <addr-3>"
;;;
;;; (This is bogus because mail-delivery programs want commas, not spaces,
;;; but that's what the file format is, so we have to live with it.)
;;;
;;; If you like, you can call the function define-mail-abbrev to define your
;;; mail aliases instead of using a .mailrc file.  When you call it in this
;;; way, addresses are separated by commas.
;;;
;;; CAVEAT: This works on most Sun systems; I have been told that some versions
;;; of /bin/mail do not understand double-quotes in the .mailrc file.  So you
;;; should make sure your version does before including verbose addresses like
;;; this.  One solution to this, if you are on a system whose /bin/mail doesn't
;;; work that way, (and you still want to be able to /bin/mail to send mail in
;;; addition to emacs) is to define minimal aliases (without full names) in
;;; your .mailrc file, and use define-mail-abbrev to redefine them when sending
;;; mail from emacs; this way, mail sent from /bin/mail will work, and mail
;;; sent from emacs will be pretty.
;;;
;;; Aliases in the mailrc file may be nested.  If you define aliases like
;;;     alias group1 fred ethel
;;;     alias group2 larry curly moe
;;;     alias everybody group1 group2
;;; Then when you type "everybody" on the To: line, it will be expanded to
;;;     fred, ethyl, larry, curly, moe
;;;
;;; Aliases may also contain forward references; the alias of "everybody" can
;;; precede the aliases of "group1" and "group2".
;;;
;;; This code also understands the "source" .mailrc command, for reading
;;; aliases from some other file as well.
;;;
;;; Aliases may contain hyphens, as in "alias foo-bar foo@bar"; word-abbrevs
;;; normally cannot contain hyphens, but this code works around that for the
;;; specific case of mail-alias word-abbrevs.
;;;
;;; To read in the contents of another .mailrc-type file from emacs, use the
;;; command Meta-X merge-mail-abbrevs.  The rebuild-mail-abbrevs command is
;;; similar, but will delete existing aliases first.
;;;
;;; If you would like your aliases to be expanded when you type M-> or ^N to
;;; move out of the mail-header into the message body (instead of having to
;;; type SPC at the end of the abbrev before moving away) then you can do
;;;
;;;	(define-key mail-mode-map "\C-n" 'mail-abbrev-next-line)
;;;	(define-key mail-mode-map "\M->" 'mail-abbrev-end-of-buffer)
;;;
;;; If you want multiple addresses separated by a string other than ", " then
;;; you can set the variable mail-alias-separator-string to it.  This has to
;;; be a comma bracketed by whitespace if you want any kind of reasonable
;;; behaviour.
;;;
;;; Thanks to Harald Hanche-Olsen, Michael Ernst, David Loeffler, and
;;; Noah Friedman for suggestions and bug reports.

;;; To use this package, do (add-hook 'mail-setup-hook 'mail-abbrevs-setup).

;;; Code:

(require 'sendmail)

;; originally defined in sendmail.el - used to be an alist, now is a table.
(defvar mail-abbrevs nil
  "Word-abbrev table of mail address aliases.
If this is nil, it means the aliases have not yet been initialized and
should be read from the .mailrc file.  (This is distinct from there being
no aliases, which is represented by this being a table with no entries.)")

;;;###autoload
(defun mail-abbrevs-setup ()
  (if (and (not (vectorp mail-abbrevs))
	   (file-exists-p mail-personal-alias-file))
      (build-mail-abbrevs))
  (make-local-hook 'pre-abbrev-expand-hook)
  (add-hook 'pre-abbrev-expand-hook 'sendmail-pre-abbrev-expand-hook
	    nil t)
  (abbrev-mode 1))

;;;###autoload
(defun build-mail-abbrevs (&optional file recursivep)
  "Read mail aliases from personal mail alias file and set `mail-abbrevs'.
By default this is the file specified by `mail-personal-alias-file'."
  (setq file (expand-file-name (or file mail-personal-alias-file)))
  (if (vectorp mail-abbrevs)
      nil
    (setq mail-abbrevs nil)
    (define-abbrev-table 'mail-abbrevs '()))
  (message "Parsing %s..." file)
  (let ((buffer nil)
	(obuf (current-buffer)))
    (unwind-protect
	(progn
	  (setq buffer (generate-new-buffer "mailrc"))
	  (buffer-disable-undo buffer)
	  (set-buffer buffer)
	  (cond ((get-file-buffer file)
		 (insert (save-excursion
			   (set-buffer (get-file-buffer file))
			   (buffer-substring (point-min) (point-max)))))
		((not (file-exists-p file)))
		(t (insert-file-contents file)))
	  ;; Don't lose if no final newline.
	  (goto-char (point-max))
	  (or (eq (preceding-char) ?\n) (newline))
	  (goto-char (point-min))
	  ;; Delete comments from the file
	  (while (search-forward "# " nil t)
	    (let ((p (- (point) 2)))
	      (end-of-line)
	      (delete-region p (point))))
	  (goto-char (point-min))
	  ;; handle "\\\n" continuation lines
	  (while (not (eobp))
	    (end-of-line)
	    (if (= (preceding-char) ?\\)
		(progn (delete-char -1) (delete-char 1) (insert ?\ ))
	        (forward-char 1)))
	  (goto-char (point-min))
	  (while (re-search-forward
		  "^\\(a\\(lias\\)?\\|g\\(roup\\)?\\|source\\)[ \t]+" nil t)
	    (beginning-of-line)
	    (if (looking-at "source[ \t]+\\([^ \t\n]+\\)")
		(progn
		  (end-of-line)
		  (build-mail-abbrevs
		   (substitute-in-file-name
		    (buffer-substring (match-beginning 1) (match-end 1)))
		   t))
	      (re-search-forward "[ \t]+\\([^ \t\n]+\\)")
	      (let* ((name (buffer-substring
			    (match-beginning 1) (match-end 1)))
		     (start (progn (skip-chars-forward " \t") (point))))
		(end-of-line)
;		(message "** %s \"%s\"" name (buffer-substring start (point)))(sit-for 1)
		(define-mail-abbrev
		    name
		    (buffer-substring start (point))
		    t))))
	  ;; Resolve forward references in .mailrc file.
	  ;; This would happen automatically before the first abbrev was
	  ;; expanded, but why not do it now.
	  (or recursivep (mail-resolve-all-aliases))
	  mail-abbrevs)
      (if buffer (kill-buffer buffer))
      (set-buffer obuf)))
    (message "Parsing %s... done" file))

(defvar mail-alias-separator-string ", "
  "*A string inserted between addresses in multi-address mail aliases.
This has to contain a comma, so \", \" is a reasonable value.  You might 
also want something like \",\\n    \" to get each address on its own line.")

;; define-mail-abbrev sets this flag, which causes mail-resolve-all-aliases
;; to be called before expanding abbrevs if it's necessary.
(defvar mail-abbrev-aliases-need-to-be-resolved t)

;; originally defined in mailalias.el ; build-mail-abbrevs calls this with
;; stuff parsed from the .mailrc file.
;;
;;;###autoload
(defun define-mail-abbrev (name definition &optional from-mailrc-file)
  "Define NAME as a mail alias abbrev that translates to DEFINITION.
If DEFINITION contains multiple addresses, separate them with commas."
  ;; When this is called from build-mail-abbrevs, the third argument is
  ;; true, and we do some evil space->comma hacking like /bin/mail does.
  (interactive "sDefine mail alias: \nsDefine %s as mail alias for: ")
  ;; Read the defaults first, if we have not done so.
  (if (vectorp mail-abbrevs)
      nil
    (setq mail-abbrevs nil)
    (define-abbrev-table 'mail-abbrevs '())
    (if (file-exists-p mail-personal-alias-file)
	(build-mail-abbrevs)))
  ;; strip garbage from front and end
  (if (string-match "\\`[ \t\n,]+" definition)
      (setq definition (substring definition (match-end 0))))
  (if (string-match "[ \t\n,]+\\'" definition)
      (setq definition (substring definition 0 (match-beginning 0))))
  (let* ((result '())
	 (L (length definition))
	 (start (if (> L 0) 0))
	 end)
    (while start
      ;; If we're reading from the mailrc file, then addresses are delimited
      ;; by spaces, and addresses with embedded spaces must be surrounded by
      ;; double-quotes.  Otherwise, addresses are separated by commas.
      (if from-mailrc-file
	  (if (eq ?\" (aref definition start))
	      (setq start (1+ start)
		    end (string-match "\"[ \t,]*" definition start))
	    (setq end (string-match "[ \t,]+" definition start)))
	(setq end (string-match "[ \t\n,]*,[ \t\n,]*" definition start)))
      (setq result (cons (substring definition start end) result))
      (setq start (and end
		       (/= (match-end 0) L)
		       (match-end 0))))
    (setq definition (mapconcat (function identity)
				(nreverse result)
				mail-alias-separator-string)))
  (setq mail-abbrev-aliases-need-to-be-resolved t)
  (setq name (downcase name))
  ;; use an abbrev table instead of an alist for mail-abbrevs.
  (let ((abbrevs-changed abbrevs-changed))  ; protect this from being changed.
    (define-abbrev mail-abbrevs name definition 'mail-abbrev-expand-hook)))


(defun mail-resolve-all-aliases ()
  "Resolve all forward references in the mail aliases table."
  (if mail-abbrev-aliases-need-to-be-resolved
      (progn
;;	(message "Resolving mail aliases...")
	(if (vectorp mail-abbrevs)
	    (mapatoms (function mail-resolve-all-aliases-1) mail-abbrevs))
	(setq mail-abbrev-aliases-need-to-be-resolved nil)
;;	(message "Resolving mail aliases... done.")
	)))

(defun mail-resolve-all-aliases-1 (sym &optional so-far)
  (if (memq sym so-far)
      (error "mail alias loop detected: %s"
	     (mapconcat 'symbol-name (cons sym so-far) " <- ")))
  (let ((definition (and (boundp sym) (symbol-value sym))))
    (if definition
	(let ((result '())
	      (start 0))
	  (while start
	    (let ((end (string-match "[ \t\n]*,[, \t\n]*" definition start)))
	      (setq result (cons (substring definition start end) result)
		    start (and end (match-end 0)))))
	  (setq definition
		(mapconcat (function (lambda (x)
			     (or (mail-resolve-all-aliases-1
				   (intern-soft (downcase x) mail-abbrevs)
				   (cons sym so-far))
				 x)))
			   (nreverse result)
			   mail-alias-separator-string))
	  (set sym definition))))
  (symbol-value sym))


(defun mail-abbrev-expand-hook ()
  "For use as the fourth arg to `define-abbrev'.
After expanding a mail-abbrev, if Auto Fill mode is on and we're past the
fill-column, break the line at the previous comma, and indent the next line."
  (save-excursion
    (let ((p (point))
	  bol comma fp)
      (beginning-of-line)
      (setq bol (point))
      (goto-char p)
      (while (and auto-fill-function
		  (>= (current-column) fill-column)
		  (search-backward "," bol t))
	(setq comma (point))
	(forward-char 1)		; Now we are just past the comma.
	(insert "\n")
	(delete-horizontal-space)
 	(setq p (point))
	(indent-relative)
	(setq fp (buffer-substring p (point)))
	;; Go to the end of the new line.
	(end-of-line)
	(if (> (current-column) fill-column)
	    ;; It's still too long; do normal auto-fill.
	    (let ((fill-prefix (or fp "\t")))
	      (do-auto-fill)))
	;; Resume the search.
	(goto-char comma)
	))))

;;; Syntax tables and abbrev-expansion

(defvar mail-abbrev-mode-regexp 
  "^\\(Resent-\\)?\\(To\\|From\\|CC\\|BCC\\|Reply-to\\):"
  "*Regexp to select mail-headers in which mail abbrevs should be expanded.
This string will be handed to `looking-at' with point at the beginning
of the current line; if it matches, abbrev mode will be turned on, otherwise
it will be turned off.  (You don't need to worry about continuation lines.)
This should be set to match those mail fields in which you want abbreviations
turned on.")

(defvar mail-mode-syntax-table (copy-syntax-table text-mode-syntax-table)
  "The syntax table which is used in send-mail mode message bodies.")

(defvar mail-mode-header-syntax-table
  (let ((tab (copy-syntax-table text-mode-syntax-table)))
    ;; This makes the characters "@%!._-" be considered symbol-constituents
    ;; but not word-constituents, so forward-sexp will move you over an
    ;; entire address, but forward-word will only move you over a sequence
    ;; of alphanumerics.  (Clearly the right thing.)
    (modify-syntax-entry ?@ "_" tab)
    (modify-syntax-entry ?% "_" tab)
    (modify-syntax-entry ?! "_" tab)
    (modify-syntax-entry ?. "_" tab)
    (modify-syntax-entry ?_ "_" tab)
    (modify-syntax-entry ?- "_" tab)
    (modify-syntax-entry ?< "(>" tab)
    (modify-syntax-entry ?> ")<" tab)
    tab)
  "The syntax table used in send-mail mode when in a mail-address header.
`mail-mode-syntax-table' is used when the cursor is in the message body or in
non-address headers.")

(defvar mail-abbrev-syntax-table
  (let* ((tab (copy-syntax-table mail-mode-header-syntax-table))
	 (i (1- (length tab)))
	 (_ (aref (standard-syntax-table) ?_))
	 (w (aref (standard-syntax-table) ?w)))
    (while (>= i 0)
      (if (= (aref tab i) _) (aset tab i w))
      (setq i (1- i)))
    tab)
  "The syntax-table used for abbrev-expansion purposes; this is not actually
made the current syntax table of the buffer, but simply controls the set of
characters which may be a part of the name of a mail alias.")


(defun mail-abbrev-in-expansion-header-p ()
  "Whether point is in a mail-address header field."
  (let ((case-fold-search t))
    (and ;;
         ;; we are on an appropriate header line...
     (save-excursion
       (beginning-of-line)
       ;; skip backwards over continuation lines.
       (while (and (looking-at "^[ \t]")
		   (not (= (point) (point-min))))
	 (forward-line -1))
       ;; are we at the front of an appropriate header line?
       (looking-at mail-abbrev-mode-regexp))
     ;;
     ;; ...and we are before the mail-header-separator
     (< (point)
	(save-excursion
	  (goto-char (point-min))
	  (search-forward (concat "\n" mail-header-separator "\n")
			  nil 0)
	  (point))))))

(defvar mail-mode-abbrev-table) ; quiet the compiler

(defun sendmail-pre-abbrev-expand-hook ()
  (and (and mail-abbrevs (not (eq mail-abbrevs t)))
       (if (mail-abbrev-in-expansion-header-p)
	   (progn
	     ;;
	     ;; We are in a To: (or CC:, or whatever) header, and
	     ;; should use word-abbrevs to expand mail aliases.

	     ;; Before anything else, resolve aliases if they need it.
	     (and mail-abbrev-aliases-need-to-be-resolved
		  (mail-resolve-all-aliases))

	     ;; Now proceed with the abbrev section.
	     ;;   -  First, install the mail-abbrevs as the word-abbrev table.
	     ;;   -  Then install the mail-abbrev-syntax-table, which
	     ;;      temporarily marks all of the
	     ;;      non-alphanumeric-atom-characters (the "_"
	     ;;      syntax ones) as being normal word-syntax.  We do this
	     ;;      because the C code for expand-abbrev only works on words,
	     ;;      and we want these characters to be considered words for
	     ;;      the purpose of abbrev expansion.
	     ;;   -  Then we call expand-abbrev again, recursively, to do
	     ;;      the abbrev expansion with the above syntax table.
	     ;;   -  Then we do a trick which tells the expand-abbrev frame
	     ;;      which invoked us to not continue (and thus not
	     ;;      expand twice.) This means that any abbrev expansion
	     ;;      will happen as a result of this function's call to
	     ;;      expand-abbrev, and not as a result of the call to
	     ;;      expand-abbrev which invoked *us*.
	     ;;   -  Then we set the syntax table to
	     ;;      mail-mode-header-syntax-table, which doesn't have
	     ;;      anything to do with abbrev expansion, but
	     ;;      is just for the user's convenience (see its doc string.)
	     ;;

	     (setq local-abbrev-table mail-abbrevs)

	     ;; If the character just typed was non-alpha-symbol-syntax,
	     ;; then don't expand the abbrev now (that is, don't expand
	     ;; when the user types -.)  Check the character's syntax in
	     ;; the mail-mode-header-syntax-table.

	     (set-syntax-table mail-mode-header-syntax-table)
	     (or (and (integerp last-command-char)
		      (eq (char-syntax last-command-char) ?_))
		 (let ((pre-abbrev-expand-hook nil)) ; That's us; don't loop.
		   ;; Use this table so that abbrevs can have hyphens in them.
		   (set-syntax-table mail-abbrev-syntax-table)
		   (expand-abbrev)
		   ;; Now set it back to what it was before.
		   (set-syntax-table mail-mode-header-syntax-table)))
	     (setq abbrev-start-location (point-max) ; This is the trick.
		   abbrev-start-location-buffer (current-buffer)))

	 ;; We're not in a mail header where mail aliases should
	 ;; be expanded, then use the normal mail-mode abbrev table
	 ;; (if any) and the normal mail-mode syntax table.

	 (setq local-abbrev-table (and (boundp 'mail-mode-abbrev-table)
				       mail-mode-abbrev-table))
	 (set-syntax-table mail-mode-syntax-table))
       ))

;;; utilities

(defun merge-mail-abbrevs (file)
  "Merge mail aliases from the given file with existing ones."
  (interactive (list
		(let ((insert-default-directory t)
		      (default-directory (expand-file-name "~/"))
		      (def mail-personal-alias-file))
		  (read-file-name
		    (format "Read additional aliases from file: (default %s) "
			    def)
		    default-directory
		    (expand-file-name def default-directory)
		    t))))
  (build-mail-abbrevs file))

(defun rebuild-mail-abbrevs (file)
  "Rebuild all the mail aliases from the given file."
  (interactive (list
		(let ((insert-default-directory t)
		      (default-directory (expand-file-name "~/"))
		      (def mail-personal-alias-file))
		  (read-file-name
		   (format "Read mail aliases from file: (default %s) " def)
		   default-directory
		   (expand-file-name def default-directory)
		   t))))
  (setq mail-abbrevs nil)
  (build-mail-abbrevs file))

(defun mail-interactive-insert-alias (&optional alias)
  "Prompt for and insert a mail alias."
  (interactive (progn
		(if (not (vectorp mail-abbrevs)) (mail-abbrevs-setup))
		(list (completing-read "Expand alias: " mail-abbrevs nil t))))
  (if (not (vectorp mail-abbrevs)) (mail-abbrevs-setup))
  (insert (or (and alias (symbol-value (intern-soft alias mail-abbrevs))) "")))

(defun mail-abbrev-next-line (&optional arg)
  "Expand any mail abbrev, then move cursor vertically down ARG lines.
If there is no character in the target line exactly under the current column,
the cursor is positioned after the character in that line which spans this
column, or at the end of the line if it is not long enough.
If there is no line in the buffer after this one,
a newline character is inserted to create a line
and the cursor moves to that line.

The command \\[set-goal-column] can be used to create
a semipermanent goal column to which this command always moves.
Then it does not try to move vertically.  This goal column is stored
in `goal-column', which is nil when there is none.

If you are thinking of using this in a Lisp program, consider
using `forward-line' instead.  It is usually easier to use
and more reliable (no dependence on goal column, etc.)."
  (interactive "p")
  (if (looking-at "[ \t]*\n") (expand-abbrev))
  (setq this-command 'next-line)
  (next-line arg))

(defun mail-abbrev-end-of-buffer (&optional arg)
  "Expand any mail abbrev, then move point to end of buffer.
Leave mark at previous position.
With arg N, put point N/10 of the way from the true end.

Don't use this command in Lisp programs!
\(goto-char (point-max)) is faster and avoids clobbering the mark."
  (interactive "P")
  (if (looking-at "[ \t]*\n") (expand-abbrev))
  (setq this-command 'end-of-buffer)
  (end-of-buffer arg))

(define-key mail-mode-map "\C-c\C-a" 'mail-interactive-insert-alias)

;;(define-key mail-mode-map "\C-n" 'mail-abbrev-next-line)
;;(define-key mail-mode-map "\M->" 'mail-abbrev-end-of-buffer)

(provide 'mailabbrev)
