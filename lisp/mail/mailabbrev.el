;;; mailabbrev.el --- abbrev-expansion of mail aliases

;; Copyright (C) 1985, 86, 87, 92, 93, 96, 1997, 2000, 2002
;;	Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>, now <jwz@jwz.org>
;; Maintainer: FSF
;; Created: 19 Oct 90
;; Keywords: mail

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

;; This file ensures that, when the point is in a To:, CC:, BCC:, or From:
;; field, word-abbrevs are defined for each of your mail aliases.  These
;; aliases will be defined from your .mailrc file (or the file specified by
;; the MAILRC environment variable) if it exists.  Your mail aliases will
;; expand any time you type a word-delimiter at the end of an abbreviation.
;;
;; What you see is what you get: if mailabbrev is in use when you type
;; a name, and the name does not expand, you know it is not an abbreviation.
;; However, if you yank abbreviations into the headers
;; in a way that bypasses the check for abbreviations,
;; they are expanded (but not visibly) when you send the message.
;;
;; Your mail alias abbrevs will be in effect only when the point is in an
;; appropriate header field.  When in the body of the message, or other
;; header fields, the mail aliases will not expand.  Rather, the normal
;; mode-specific abbrev table will be used if
;; defined.  So if you use mail-mode specific abbrevs, this code will not
;; adversely affect you.  You can control which header fields the abbrevs
;; are used in by changing the variable mail-abbrev-mode-regexp.
;;
;; If auto-fill mode is on, abbrevs will wrap at commas instead of at word
;; boundaries; also, header continuation-lines will be properly indented.
;;
;; You can also insert a mail alias with mail-abbrev-insert-alias
;; (bound to C-c C-a), which prompts you for an alias (with completion)
;; and inserts its expansion at point.
;;
;; This file fixes a bug in the old system which prohibited your .mailrc
;; file from having lines like
;;
;;     alias someone "John Doe <doe@quux.com>"
;;
;; That is, if you want an address to have embedded spaces, simply surround it
;; with double-quotes.  This is necessary because the format of the .mailrc
;; file bogusly uses spaces as address delimiters.  The following line defines
;; an alias which expands to three addresses:
;;
;;     alias foobar addr-1 addr-2 "address three <addr-3>"
;;
;; (This is bogus because mail-delivery programs want commas, not spaces,
;; but that's what the file format is, so we have to live with it.)
;;
;; If you like, you can call the function define-mail-abbrev to define your
;; mail aliases instead of using a .mailrc file.  When you call it in this
;; way, addresses are separated by commas.
;;
;; CAVEAT: This works on most Sun systems; I have been told that some versions
;; of /bin/mail do not understand double-quotes in the .mailrc file.  So you
;; should make sure your version does before including verbose addresses like
;; this.  One solution to this, if you are on a system whose /bin/mail doesn't
;; work that way, (and you still want to be able to /bin/mail to send mail in
;; addition to emacs) is to define minimal aliases (without full names) in
;; your .mailrc file, and use define-mail-abbrev to redefine them when sending
;; mail from emacs; this way, mail sent from /bin/mail will work, and mail
;; sent from emacs will be pretty.
;;
;; Aliases in the mailrc file may be nested.  If you define aliases like
;;     alias group1 fred ethel
;;     alias group2 larry curly moe
;;     alias everybody group1 group2
;; Then when you type "everybody" on the To: line, it will be expanded to
;;     fred, ethyl, larry, curly, moe
;;
;; Aliases may also contain forward references; the alias of "everybody" can
;; precede the aliases of "group1" and "group2".
;;
;; This code also understands the "source" .mailrc command, for reading
;; aliases from some other file as well.
;;
;; Aliases may contain hyphens, as in "alias foo-bar foo@bar"; word-abbrevs
;; normally cannot contain hyphens, but this code works around that for the
;; specific case of mail-alias word-abbrevs.
;;
;; To read in the contents of another .mailrc-type file from emacs, use the
;; command Meta-X merge-mail-abbrevs.  The rebuild-mail-abbrevs command is
;; similar, but will delete existing aliases first.
;;
;; If you would like your aliases to be expanded when you type M-> or ^N to
;; move out of the mail-header into the message body (instead of having to
;; type SPC at the end of the abbrev before moving away) then you can do
;;
;;  (add-hook
;;   'mail-mode-hook
;;   (lambda ()
;;      (substitute-key-definition 'next-line 'mail-abbrev-next-line
;;				 mail-mode-map global-map)
;;      (substitute-key-definition 'end-of-buffer 'mail-abbrev-end-of-buffer
;;				 mail-mode-map global-map)))
;;
;; If you want multiple addresses separated by a string other than ", " then
;; you can set the variable mail-alias-separator-string to it.  This has to
;; be a comma bracketed by whitespace if you want any kind of reasonable
;; behaviour.
;;
;; Thanks to Harald Hanche-Olsen, Michael Ernst, David Loeffler, and
;; Noah Friedman for suggestions and bug reports.

;; To use this package, do (add-hook 'mail-mode-hook 'mail-abbrevs-setup).

;;; Code:

(eval-when-compile
  (require 'sendmail))

(defgroup mail-abbrev nil
  "Expand mail aliases as abbrevs, in certain mail headers."
  :group 'abbrev-mode)

(defcustom mail-abbrevs-mode nil
  "*Non-nil means expand mail aliases as abbrevs, in certain message headers."
  :type 'boolean
  :group 'mail-abbrev
  :require 'mailabbrev
  :set (lambda (symbol value)
	 (setq mail-abbrevs-mode value)
	 (if value (mail-abbrevs-enable) (mail-abbrevs-disable)))
  :initialize 'custom-initialize-default
  :version "20.3")

(defcustom mail-abbrevs-only nil
  "*Non-nil means only mail abbrevs should expand automatically.
Other abbrevs expand only when you explicitly use `expand-abbrev'."
  :type 'boolean
  :group 'mail-abbrev)

;; originally defined in sendmail.el - used to be an alist, now is a table.
(defvar mail-abbrevs nil
  "Word-abbrev table of mail address aliases.
If this is nil, it means the aliases have not yet been initialized and
should be read from the .mailrc file.  (This is distinct from there being
no aliases, which is represented by this being a table with no entries.)")

(defvar mail-abbrev-modtime nil
  "The modification time of your mail alias file when it was last examined.")

(defun mail-abbrevs-sync-aliases ()
  (if (file-exists-p mail-personal-alias-file)
      (let ((modtime (nth 5 (file-attributes mail-personal-alias-file))))
	(if (not (equal mail-abbrev-modtime modtime))
	    (progn
	      (setq mail-abbrev-modtime modtime)
	      (build-mail-abbrevs))))))

;;;###autoload
(defun mail-abbrevs-setup ()
  "Initialize use of the `mailabbrev' package."
  (if (and (not (vectorp mail-abbrevs))
	   (file-exists-p mail-personal-alias-file))
      (progn
	(setq mail-abbrev-modtime
	      (nth 5 (file-attributes mail-personal-alias-file)))
	(build-mail-abbrevs)))
  (mail-abbrevs-sync-aliases)
  (add-hook 'pre-abbrev-expand-hook 'sendmail-pre-abbrev-expand-hook
	    nil t)
  (abbrev-mode 1))

(defun mail-abbrevs-enable ()
  (add-hook 'mail-mode-hook 'mail-abbrevs-setup))

(defun mail-abbrevs-disable ()
  "Turn off use of the `mailabbrev' package."
  (remove-hook 'mail-mode-hook 'mail-abbrevs-setup)
  (abbrev-mode (if (default-value 'abbrev-mode) 1 -1)))

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
	  (setq buffer (generate-new-buffer " mailrc"))
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
    (define-abbrev mail-abbrevs name definition 'mail-abbrev-expand-hook 0 t)))


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
  ;; Disable abbrev mode to avoid recursion in indent-relative expanding
  ;; part of the abbrev expansion as an abbrev itself.
  (let ((abbrev-mode nil))
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
	  )))))

;;; Syntax tables and abbrev-expansion

(defvar mail-abbrev-mode-regexp
  "^\\(Resent-\\)?\\(To\\|From\\|CC\\|BCC\\|Reply-to\\):"
  "*Regexp to select mail-headers in which mail abbrevs should be expanded.
This string will be handed to `looking-at' with point at the beginning
of the current line; if it matches, abbrev mode will be turned on, otherwise
it will be turned off.  (You don't need to worry about continuation lines.)
This should be set to match those mail fields in which you want abbreviations
turned on.")

(defvar mail-abbrev-syntax-table nil
  "The syntax-table used for abbrev-expansion purposes.
This is not actually made the current syntax table of the buffer, but
simply controls the set of characters which may be a part of the name
of a mail alias.  The value is set up, buffer-local, when first needed.")

(defun mail-abbrev-make-syntax-table ()
  (make-local-variable 'mail-abbrev-syntax-table)
  (unless mail-abbrev-syntax-table
    (let ((tab (copy-syntax-table (syntax-table)))
	  (_ (aref (standard-syntax-table) ?_))
	  (w (aref (standard-syntax-table) ?w)))
      (map-char-table
       (function (lambda (key value)
		   (if (equal value _)
		       (set-char-table-range tab key w))))
       tab)
      (modify-syntax-entry ?@ "w" tab)
      (setq mail-abbrev-syntax-table tab))))

(defun mail-abbrev-in-expansion-header-p ()
  "Whether point is in a mail-address header field."
  (let ((case-fold-search t))
    (and ;;
         ;; we are on an appropriate header line...
     (save-excursion
       (unless (eobp) (forward-char 1))
       (re-search-backward "^[^ \t]" nil 'move)
       ;; are we at the front of an appropriate header line?
       (looking-at mail-abbrev-mode-regexp))
     ;;
     ;; ...and are we in the headers?
     (< (point)
	(save-restriction
	  (widen)
	  (save-excursion
	    (rfc822-goto-eoh)
	    (point)))))))

(defun sendmail-pre-abbrev-expand-hook ()
  (and (and mail-abbrevs (not (eq mail-abbrevs t)))
       (if (mail-abbrev-in-expansion-header-p)

	   ;; We are in a To: (or CC:, or whatever) header, and
	   ;; should use word-abbrevs to expand mail aliases.
	   (let ((local-abbrev-table mail-abbrevs)
		 (old-syntax-table (syntax-table)))

	     ;; Before anything else, resolve aliases if they need it.
	     (and mail-abbrev-aliases-need-to-be-resolved
		  (mail-resolve-all-aliases))

	     ;; Now proceed with the abbrev section.
	     ;;   -  We already installed mail-abbrevs as the abbrev table.
	     ;;   -  Then install the mail-abbrev-syntax-table, which
	     ;;      temporarily marks all of the
	     ;;      non-alphanumeric-atom-characters (the "_"
	     ;;      syntax ones) as being normal word-syntax.  We do this
	     ;;      because the C code for expand-abbrev only works on words,
	     ;;      and we want these characters to be considered words for
	     ;;      the purpose of abbrev expansion.
	     ;;   -  Then we call expand-abbrev again, recursively, to do
	     ;;      the abbrev expansion with the above syntax table.
	     ;;   -  Restore the previous syntax table.
	     ;;   -  Then we do a trick which tells the expand-abbrev frame
	     ;;      which invoked us to not continue (and thus not
	     ;;      expand twice.) This means that any abbrev expansion
	     ;;      will happen as a result of this function's call to
	     ;;      expand-abbrev, and not as a result of the call to
	     ;;      expand-abbrev which invoked *us*.

	     (mail-abbrev-make-syntax-table)

	     ;; If the character just typed was non-alpha-symbol-syntax,
	     ;; then don't expand the abbrev now (that is, don't expand
	     ;; when the user types -.)  Check the character's syntax in
	     ;; the usual syntax table.

	     (or (and (integerp last-command-char)
		      (eq (char-syntax last-command-char) ?_))
		 (let ((pre-abbrev-expand-hook nil)) ; That's us; don't loop.
		   ;; Use this table so that abbrevs can have hyphens in them.
		   (set-syntax-table mail-abbrev-syntax-table)
		   (unwind-protect
		       (expand-abbrev)
		     ;; Now set it back to what it was before.
		     (set-syntax-table old-syntax-table))))
	     (setq abbrev-start-location (point-max) ; This is the trick.
		   abbrev-start-location-buffer (current-buffer)))

	 (if (or (not mail-abbrevs-only)
		 (eq this-command 'expand-abbrev))
	     ;; We're not in a mail header where mail aliases should
	     ;; be expanded, then use the normal mail-mode abbrev table
	     ;; (if any) and the normal mail-mode syntax table.
	     nil
	   ;; This is not a mail abbrev, and we should not expand it.
	   ;; This kludge stops expand-abbrev from doing anything.
	   (setq abbrev-start-location (point-max)
		 abbrev-start-location-buffer (current-buffer))))
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

(defun rebuild-mail-abbrevs (&optional file)
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
  (if (null file)
      (setq file buffer-file-name))
  (setq mail-abbrevs nil)
  (build-mail-abbrevs file))

(defun mail-abbrev-insert-alias (&optional alias)
  "Prompt for and insert a mail alias."
  (interactive (progn
		(if (not (vectorp mail-abbrevs)) (mail-abbrevs-setup))
		(list (completing-read "Expand alias: " mail-abbrevs nil t))))
  (if (not (vectorp mail-abbrevs)) (mail-abbrevs-setup))
  (insert (or (and alias (symbol-value (intern-soft alias mail-abbrevs))) ""))
  (mail-abbrev-expand-hook))

(defun mail-abbrev-complete-alias ()
  "Perform completion on alias preceding point."
  ;; Based on lisp.el:lisp-complete-symbol
  (interactive)
  (mail-abbrev-make-syntax-table)
  (let* ((end (point))
	 (syntax-table (syntax-table))
	 (beg (unwind-protect
		  (save-excursion
		    (set-syntax-table mail-abbrev-syntax-table)
		    (backward-word 1)
		    (point))
		(set-syntax-table syntax-table)))
	 (alias (buffer-substring beg end))
	 (completion (try-completion alias mail-abbrevs)))
    (cond ((eq completion t)
	   (message "%s" alias))	; confirm
	  ((null completion)
	   (error "[Can't complete \"%s\"]" alias)) ; (message ...) (ding)
	  ((not (string= completion alias))
	   (delete-region beg end)
	   (insert completion))
	  (t (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list
		(prog2
		    (message "Making completion list...")
		    (all-completions alias mail-abbrevs)
		  (message "Making completion list...done"))))))))

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

(eval-after-load "sendmail"
  '(progn
     (define-key mail-mode-map "\C-c\C-a" 'mail-abbrev-insert-alias)
     (define-key mail-mode-map "\e\t"	; like lisp-complete-symbol
       'mail-abbrev-complete-alias)))

;;(define-key mail-mode-map "\C-n" 'mail-abbrev-next-line)
;;(define-key mail-mode-map "\M->" 'mail-abbrev-end-of-buffer)

(provide 'mailabbrev)

(if mail-abbrevs-mode
    (mail-abbrevs-enable))

;;; mailabbrev.el ends here
