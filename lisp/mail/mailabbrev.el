;;; Abbrev-expansion of mail aliases.
;;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.
;;; Created: 19 oct 90, Jamie Zawinski <jwz@lucid.com>
;;; Last change 15-dec-91. jwz

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This file ensures that, when the point is in a To:, CC:, BCC:, or From: 
;;; field, word-abbrevs are defined for each of your mail aliases.  These
;;; aliases will be defined from your .mailrc file (or the file specified by
;;; the MAILRC environment variable) if it exists.  Providing abbrev-mode is
;;; on in your send-mail buffer, your mail aliases will expand any time you
;;; type a word-delimiter at the end of an abbreviation.
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
;;; To use this code, do something like
;;;
;;;    (setq mail-mode-hook '(lambda () (require 'mail-abbrevs)))
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
;;; If you like, you can call the function define-mail-alias to define your
;;; mail-aliases instead of using a .mailrc file.  When you call it in this
;;; way, addresses are seperated by commas.
;;;
;;; CAVEAT: This works on most Sun systems; I have been told that some versions
;;; of /bin/mail do not understand double-quotes in the .mailrc file.  So you
;;; should make sure your version does before including verbose addresses like
;;; this.  One solution to this, if you are on a system whose /bin/mail doesn't
;;; work that way, (and you still want to be able to /bin/mail to send mail in
;;; addition to emacs) is to define minimal aliases (without full names) in
;;; your .mailrc file, and use define-mail-alias to redefine them when sending
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
;;; preceed the aliases of "group1" and "group2".
;;;
;;; This code also understands the "source" .mailrc command, for reading
;;; aliases from some other file as well.
;;;
;;; To read in the contents of another .mailrc-type file from emacs, use the
;;; command Meta-X merge-mail-aliases.  The rebuild-mail-aliases command is
;;; similar, but will delete existing aliases first.
;;;
;;; If you want multiple addresses seperated by a string other than ", " then
;;; you can set the variable mail-alias-seperator-string to it.  This has to
;;; be a comma bracketed by whitespace if you want any kind of reasonable
;;; behaviour.
;;;
;;; Thanks to Harald Hanche-Olsen, Michael Ernst, David Loeffler, and
;;; Noah Friedman for suggestions and bug reports.

(require 'sendmail)

(defvar mail-abbrev-mailrc-file nil
  "Name of file with mail aliases.   If nil, ~/.mailrc is used.")

(defmacro mail-abbrev-mailrc-file ()
  '(or mail-abbrev-mailrc-file
       (setq mail-abbrev-mailrc-file
	     (or (getenv "MAILRC") "~/.mailrc"))))

;; originally defined in sendmail.el - used to be an alist, now is a table.
(defvar mail-aliases nil
  "Word-abbrev table of mail address aliases.
If this is nil, it means the aliases have not yet been initialized and
should be read from the .mailrc file.  (This is distinct from there being
no aliases, which is represented by this being a table with no entries.)")

(defun mail-aliases-setup ()
  (if (and (not (vectorp mail-aliases))
	   (file-exists-p (mail-abbrev-mailrc-file)))
      (build-mail-aliases))
  (make-local-variable 'pre-abbrev-expand-hook)
  (setq pre-abbrev-expand-hook
    (cond ((and (listp pre-abbrev-expand-hook)
		(not (eq 'lambda (car pre-abbrev-expand-hook))))
	   (cons 'sendmail-pre-abbrev-expand-hook pre-abbrev-expand-hook))
	  (t
	   (list 'sendmail-pre-abbrev-expand-hook pre-abbrev-expand-hook))))
  (abbrev-mode 1))

;;; Originally defined in mailalias.el.  Changed to call define-mail-alias
;;; with an additional argument.
(defun build-mail-aliases (&optional file recursivep)
  "Read mail aliases from .mailrc and set mail-aliases."
  (setq file (expand-file-name (or file (mail-abbrev-mailrc-file))))
  (if (vectorp mail-aliases)
      nil
    (setq mail-aliases nil)
    (define-abbrev-table 'mail-aliases '()))
  (message "Parsing %s ..." file)
  (let ((buffer nil)
	(obuf (current-buffer)))
    (unwind-protect
	(progn
	  (setq buffer (generate-new-buffer "mailrc"))
	  (buffer-flush-undo buffer)
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
		  "^\\(a\\(lias\\|\\)\\|g\\(roup\\)\\|source\\)[ \t]+" nil t)
	    (beginning-of-line)
	    (if (looking-at "source[ \t]+\\([^ \t\n]+\\)")
		(progn
		  (end-of-line)
		  (build-mail-aliases
		   (buffer-substring (match-beginning 1) (match-end 1)) t))
	      (re-search-forward "[ \t]+\\([^ \t\n]+\\)")
	      (let* ((name (buffer-substring
			    (match-beginning 1) (match-end 1)))
		     (start (progn (skip-chars-forward " \t") (point))))
		(end-of-line)
;		(message "** %s \"%s\"" name (buffer-substring start (point)))(sit-for 1)
		(define-mail-alias
		    name
		    (buffer-substring start (point))
		    t))))
	  ;; Resolve forward references in .mailrc file.
	  ;; This would happen automatically before the first abbrev was
	  ;; expanded, but why not do it now.
	  (or recursivep (mail-resolve-all-aliases))
	  mail-aliases)
      (if buffer (kill-buffer buffer))
      (set-buffer obuf)))
    (message "Parsing %s ... done" file))

(defvar mail-alias-seperator-string ", "
  "*A string inserted between addresses in multi-address mail aliases.
This has to contain a comma, so \", \" is a reasonable value.  You might 
also want something like \",\\n    \" to get each address on its own line.")

;; define-mail-alias sets this flag, which causes mail-resolve-all-aliases
;; to be called before expanding abbrevs if it's necessary.
(defvar mail-abbrev-aliases-need-to-be-resolved t)

;; originally defined in mailalias.el ; build-mail-aliases calls this with
;; stuff parsed from the .mailrc file.
;;
(defun define-mail-alias (name definition &optional from-mailrc-file)
  "Define NAME as a mail-alias that translates to DEFINITION.
If DEFINITION contains multiple addresses, seperate them with commas."
  ;; When this is called from build-mail-aliases, the third argument is
  ;; true, and we do some evil space->comma hacking like /bin/mail does.
  (interactive "sDefine mail alias: \nsDefine %s as mail alias for: ")
  ;; Read the defaults first, if we have not done so.
  (if (vectorp mail-aliases)
      nil
    (setq mail-aliases nil)
    (define-abbrev-table 'mail-aliases '())
    (if (file-exists-p (mail-abbrev-mailrc-file))
	(build-mail-aliases)))
  ;; strip garbage from front and end
  (if (string-match "\\`[ \t\n,]+" definition)
      (setq definition (substring definition (match-end 0))))
  (if (string-match "[ \t\n,]+\\'" definition)
      (setq definition (substring definition 0 (match-beginning 0))))
  (let ((result '())
	(start 0)
	(L (length definition))
	end)
    (while start
      ;; If we're reading from the mailrc file, then addresses are delimited
      ;; by spaces, and addresses with embedded spaces must be surrounded by
      ;; double-quotes.  Otherwise, addresses are seperated by commas.
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
				mail-alias-seperator-string)))
  (setq mail-abbrev-aliases-need-to-be-resolved t)
  (setq name (downcase name))
  ;; use an abbrev table instead of an alist for mail-aliases.
  (let ((abbrevs-changed abbrevs-changed))  ; protect this from being changed.
    (define-abbrev mail-aliases name definition 'mail-abbrev-expand-hook)))


(defun mail-resolve-all-aliases ()
  "Resolve all forward references in the mail aliases table."
  (if mail-abbrev-aliases-need-to-be-resolved
      (progn
;;	(message "Resolving mail aliases...")
	(if (vectorp mail-aliases)
	    (mapatoms (function mail-resolve-all-aliases-1) mail-aliases))
	(setq mail-abbrev-aliases-need-to-be-resolved nil)
;;	(message "Resolving mail aliases... done.")
	)))

(defun mail-resolve-all-aliases-1 (sym)
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
				   (intern-soft x mail-aliases))
				 x)))
			   (nreverse result)
			   mail-alias-seperator-string))
	  (set sym definition))))
  (symbol-value sym))


(defun mail-abbrev-expand-hook ()
  "For use as the fourth arg to define-abbrev.
  After expanding a mail-abbrev, if fill-mode is on and we're past the 
fill-column, break the line at the previous comma, and indent the next
line."
  (save-excursion
    (let ((p (point))
	  bol)
      (if (and (if (boundp 'auto-fill-function)
		   auto-fill-function
		 auto-fill-hook)
	       (>= (current-column) fill-column))
	  (progn
	    (beginning-of-line)
	    (setq bol (point))
	    (goto-char p)
	    (if (search-backward "," bol t)
		(progn
		  (forward-char 1)
		  (insert "\n   ")))
	    (if (> (current-column) fill-column)
		(let ((fill-prefix "    "))
		  (do-auto-fill)))
	    )))))


(defun mail-interactive-insert-alias (&optional alias)
  "Prompt for and insert a mail alias."
  (interactive (list (completing-read "Expand alias: " mail-aliases nil t)))
  (insert (or (and alias (symbol-value (intern-soft alias mail-aliases))) "")))

(define-key mail-mode-map "\C-c\C-a" 'mail-interactive-insert-alias)


(defvar mail-abbrev-mode-regexp "^\\(To\\|From\\|CC\\|BCC\\):"
  "*Regexp to select mail-headers in which mail-aliases should be expanded.
This string it will be handed to `looking-at' with the point at the beginning
of the current line; if it matches, abbrev mode will be turned on, otherwise
it will be turned off.  (You don't need to worry about continuation lines.)
This should be set to match those mail fields in which you want abbreviations
turned on.")

(defvar mail-mode-syntax-table (copy-syntax-table text-mode-syntax-table)
  "The syntax table which is current in send-mail mode.")

(defvar mail-mode-header-syntax-table
  (let ((tab (copy-syntax-table text-mode-syntax-table)))
    ;; This makes the caracters "@%!._-" be considered symbol-consituents
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
    ;; I hate this more than you can possibly imagine.
    ;; Do this if you want to have aliases with hyphens in them.  This causes
    ;; hyphens to be considered word-syntax, so forward-word will not stop at
    ;; hyphens.
    ;;(modify-syntax-entry ?- "w" tab)
    tab)
  "The syntax table used when the cursor is in a mail-address header.
mail-mode-syntax-table is used when the cursor is not in an address header.")


(defun sendmail-pre-abbrev-expand-hook ()
  (if mail-abbrev-aliases-need-to-be-resolved
      (mail-resolve-all-aliases))
  (if (and mail-aliases (not (eq mail-aliases t)))
      (let ((case-fold-search t))
	(if (and ;;
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
		      (point))))
	    ;; install the mail-aliases abbrev and syntax tables...
	    (progn
	      (setq local-abbrev-table mail-aliases)
	      (set-syntax-table mail-mode-header-syntax-table))
	  ;; or install the normal mail-mode abbrev table (likely empty).
	  (progn
	    (setq local-abbrev-table (and (boundp 'mail-mode-abbrev-table)
					  mail-mode-abbrev-table))
	    (set-syntax-table mail-mode-syntax-table))))))


(defun merge-mail-aliases (file)
  "Merge mail aliases from the given file with existing ones."
  (interactive (list
		(let ((insert-default-directory t)
		      (default-directory (expand-file-name "~/"))
		      (def (mail-abbrev-mailrc-file)))
		  (read-file-name
		    (format "Read additional aliases from file: (default %s) "
			    def)
		    default-directory
		    (expand-file-name def default-directory)
		    t))))
  (build-mail-aliases file))

(defun rebuild-mail-aliases (file)
  "Rebuild all the mail aliases from the given file."
  (interactive (list
		(let ((insert-default-directory t)
		      (default-directory (expand-file-name "~/"))
		      (def (mail-abbrev-mailrc-file)))
		  (read-file-name
		   (format "Read mail aliases from file: (default %s) " def)
		   default-directory
		   (expand-file-name def default-directory)
		   t))))
  (setq mail-aliases nil)
  (build-mail-aliases file))
  

;;; Patching it in:
;;; Remove the entire file mailalias.el
;;; Remove the definition of mail-aliases from sendmail.el
;;; Add a call to mail-aliases-setup to mail-setup in sendmail.el
;;; Remove the call to expand-mail-aliases from sendmail-send-it in sendmail.el
;;; Remove the autoload of expand-mail-aliases from sendmail.el
;;; Remove the autoload of build-mail-aliases from sendmail.el
;;; Add an autoload of define-mail-alias

(fmakunbound 'expand-mail-aliases)

(provide 'mail-abbrevs)

