;;; mailalias.el --- expand mailing address aliases defined in ~/.mailrc.

;; Copyright (C) 1985, 1987 Free Software Foundation, Inc.

;; Maintainer: FSF
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Basic functions for defining and expanding mail aliases.
;; These seal off the interface to the alias-definition parts of a
;; .mailrc file formatted for BSD's Mail or USL's mailx.

;;; Code:

(require 'sendmail)

;; Called from sendmail-send-it, or similar functions,
;; only if some mail aliases are defined.
(defun expand-mail-aliases (beg end &optional exclude)
  "Expand all mail aliases in suitable header fields found between BEG and END.
Suitable header fields are `To', `From', `CC' and `BCC', `Reply-to', and
their `Resent-' variants.

Optional second arg EXCLUDE may be a regular expression defining text to be
removed from alias expansions."
  (sendmail-synch-aliases)
  (if (eq mail-aliases t)
      (progn (setq mail-aliases nil) (build-mail-aliases)))
  (goto-char beg)
  (setq end (set-marker (make-marker) end))
  (let ((case-fold-search nil))
    (while (let ((case-fold-search t))
	     (re-search-forward "^\\(Resent-\\)?\\(To\\|From\\|CC\\|BCC\\|Reply-to\\):" end t))
      (skip-chars-forward " \t")
      (let ((beg1 (point))
	    end1 pos epos seplen
	    ;; DISABLED-ALIASES records aliases temporarily disabled
	    ;; while we scan text that resulted from expanding those aliases.
	    ;; Each element is (ALIAS . TILL-WHEN), where TILL-WHEN
	    ;; is where to reenable the alias (expressed as number of chars
	    ;; counting from END1).
	    (disabled-aliases nil))
	(re-search-forward "^[^ \t]" end 'move)
	(beginning-of-line)
	(skip-chars-backward " \t\n")
	(setq end1 (point-marker))
	(goto-char beg1)
	(while (< (point) end1)
	  (setq pos (point))
	  ;; Reenable any aliases which were disabled for ranges
	  ;; that we have passed out of.
	  (while (and disabled-aliases (> pos (- end1 (cdr (car disabled-aliases)))))
	    (setq disabled-aliases (cdr disabled-aliases)))
	  ;; EPOS gets position of end of next name;
	  ;; SEPLEN gets length of whitespace&separator that follows it.
	  (if (re-search-forward "[ \t]*[\n,][ \t]*" end1 t)
	      (setq epos (match-beginning 0)
		    seplen (- (point) epos))
	    (setq epos (marker-position end1) seplen 0))
	  (let (translation
		(string (buffer-substring-no-properties pos epos)))
	    (if (and (not (assoc string disabled-aliases))
		     (setq translation
			   (cdr (assoc string mail-aliases))))
		(progn
		  ;; This name is an alias.  Disable it.
		  (setq disabled-aliases (cons (cons string (- end1 epos))
					       disabled-aliases))
		  ;; Replace the alias with its expansion
		  ;; then rescan the expansion for more aliases.
		  (goto-char pos)
		  (insert translation)
		  (if exclude
 		      (let ((regexp
			     (concat "\\b\\(" exclude "\\)\\b"))
			    (end (point-marker)))
			(goto-char pos)
			(while (re-search-forward regexp end t)
			  (replace-match ""))
			(goto-char end)))
		  (delete-region (point) (+ (point) (- epos pos)))
		  (goto-char pos))
	      ;; Name is not an alias.  Skip to start of next name.
	      (goto-char epos)
	      (forward-char seplen))))
	(set-marker end1 nil)))
    (set-marker end nil)))

;; Called by mail-setup, or similar functions, only if the file specified
;; by mail-personal-alias-file (usually `~/.mailrc') exists.
(defun build-mail-aliases (&optional file)
  "Read mail aliases from personal aliases file and set `mail-aliases'.
By default, this is the file specified by `mail-personal-alias-file'."
  (setq file (expand-file-name (or file mail-personal-alias-file)))
  (let ((buffer nil)
	(obuf (current-buffer)))
    (unwind-protect
	(progn
	  (setq buffer (generate-new-buffer "mailrc"))
	  (buffer-disable-undo buffer)
	  (set-buffer buffer)
	  (while file
	    (cond ((get-file-buffer file)
		   (insert (save-excursion
			     (set-buffer (get-file-buffer file))
			     (buffer-substring (point-min) (point-max)))))
		  ((file-exists-p file) (insert-file-contents file))
		  ((file-exists-p (setq file (concat "~/" file)))
		   (insert-file-contents file))
		  (t (setq file nil)))
	    ;; Don't lose if no final newline.
	    (goto-char (point-max))
	    (or (eq (preceding-char) ?\n) (newline))
	    (goto-char (point-min))
	    ;; handle "\\\n" continuation lines
	    (while (not (eobp))
	      (end-of-line)
	      (if (= (preceding-char) ?\\)
		  (progn (delete-char -1) (delete-char 1) (insert ?\ ))
	        (forward-char 1)))
	    (goto-char (point-min))
	    ;; handle `source' directives -- Eddy/1994/May/25
	    (cond ((re-search-forward "^source[ \t]+" nil t)
		   (re-search-forward "\\S-+")
		   (setq file
			 (buffer-substring (match-beginning 0) (match-end 0)))
		   (beginning-of-line)
		   (insert "# ") ; to ensure we don't re-process this file
		   (beginning-of-line))
		  (t (setq file nil))))
	  (goto-char (point-min))
	  (while (or (re-search-forward "^a\\(lias\\|\\)[ \t]+" nil t)
		     (re-search-forward "^g\\(roup\\|\\)[ \t]+" nil t))
	    (re-search-forward "[^ \t]+")
	    (let* ((name (buffer-substring (match-beginning 0) (match-end 0)))
		   (start (progn (skip-chars-forward " \t") (point))))
	      (end-of-line)
	      (define-mail-alias
		name
		(buffer-substring start (point))
		t)))
	  mail-aliases)
      (if buffer (kill-buffer buffer))
      (set-buffer obuf))))

;; Always autoloadable in case the user wants to define aliases
;; interactively or in .emacs.
;;;###autoload
(defun define-mail-alias (name definition &optional from-mailrc-file)
  "Define NAME as a mail alias that translates to DEFINITION.
This means that sending a message to NAME will actually send to DEFINITION.
DEFINITION can be one or more mail addresses separated by spaces.
An address can contain spaces if it is quoted with double-quotes."
  (interactive "sDefine mail alias: \nsDefine %s as mail alias for: ")
  ;; Read the defaults first, if we have not done so.
  (sendmail-synch-aliases)
  (if (eq mail-aliases t)
      (progn
	(setq mail-aliases nil)
	(if (file-exists-p mail-personal-alias-file)
	    (build-mail-aliases))))
  ;; strip garbage from front and end
  (if (string-match "\\`[ \t\n,]+" definition)
      (setq definition (substring definition (match-end 0))))
  (if (string-match "[ \t\n,]+\\'" definition)
      (setq definition (substring definition 0 (match-beginning 0))))
  (let ((result '())
	;; If DEFINITION is null string, avoid looping even once.
	(start (and (not (equal definition "")) 0))
	(L (length definition))
	end tem)
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
				", "))
    (setq tem (assoc name mail-aliases))
    (if tem
	(rplacd tem definition)
      (setq mail-aliases (cons (cons name definition) mail-aliases)))))

(provide 'mailalias)

;;; mailalias.el ends here
