;;; iso-acc.el --- minor mode providing electric accent keys

;; Copyright (C) 1993, 1994, 1996 Free Software Foundation, Inc.

;; Author: Johan Vromans
;; Maintainer: FSF
;; Keywords: i18n

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

;; Function `iso-accents-mode' activates a minor mode in which
;; typewriter "dead keys" are emulated.  The purpose of this emulation
;; is to provide a simple means for inserting accented characters
;; according to the ISO-8859-1 character set.
;;
;; In `iso-accents-mode', pseudo accent characters are used to
;; introduce accented keys.  The pseudo-accent characters are:
;;
;;   '  (minute)    -> grave accent
;;   `  (backtick)  -> acute accent
;;   "  (second)    -> diaeresis
;;   ^  (caret)     -> circumflex
;;   ~  (tilde)     -> tilde over the character
;;   /  (slash)     -> slash through the character.
;;                  Also:  /A is A-with-ring and /E is AE ligature.
;;
;; The action taken depends on the key that follows the pseudo accent.
;; In general: 
;;
;;   pseudo-accent + appropriate letter -> accented letter
;;   pseudo-accent + space -> pseudo-accent
;;   pseudo-accent + pseudo-accent -> accent (if available)
;;   pseudo-accent + other -> pseudo-accent + other
;;
;; If the pseudo-accent is followed by anything else than a 
;; self-insert-command, the dead-key code is terminated, the
;; pseudo-accent inserted 'as is' and the bell is rung to signal this.
;;
;; Function `iso-accents-mode' can be used to enable the iso accents
;; minor mode, or disable it.

;; If you want only some of these characters to serve as accents,
;; add a language to `iso-languages' which specifies the accent characters
;; that you want, then select the language with `iso-accents-customize'.

;;; Code:

(provide 'iso-acc)

(defvar iso-languages
  '(("portuguese"
     (?' (?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
	 (?C . ?\307) (?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363)
	 (?u . ?\372) (?c . ?\347) (?\  . ?'))
     (?` (?A . ?\300) (?a . ?\340) (?\  . ?`))
     (?^ (?A . ?\302) (?E . ?\312) (?O . ?\324) (?a . ?\342) (?e . ?\352)
	 (?o . ?\364) (?\  . ?^))
     (?\" (?U . ?\334) (?u . ?\374) (?\  . ?\"))
     (?\~ (?A . ?\303) (?O . ?\325) (?a . ?\343) (?o . ?\365) (?\  . ?\~)))
    
    ("irish"
     (?' (?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
	 (?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363) (?u . ?\372)
	 (?\  . ?')))
    
    ("french"
     (?' (?E . ?\311) (?C . ?\307) (?e . ?\351) (?c . ?\347) (?\  . ?'))
     (?` (?A . ?\300) (?E . ?\310) (?U . ?\331)
         (?a . ?\340) (?e . ?\350) (?u . ?\371) (?\  . ?`))
     (?^ (?A . ?\302) (?E . ?\312) (?I . ?\316) (?O . ?\324) (?U . ?\333)
	 (?a . ?\342) (?e . ?\352) (?i . ?\356) (?o . ?\364) (?u . ?\373)
	 (?\  . ?^))
     (?\" (?E . ?\313) (?I . ?\317)  
          (?e . ?\353) (?i . ?\357) (?\  . ?\"))
     (?\~ (?< . ?\253) (?> . ?\273) (?C . ?\307) (?c . ?\347) (?\  . ?\~)))
    
    ("latin-2"
     (?' (?A . ?\301) (?C . ?\306) (?D . ?\320) (?E . ?\311) (?I . ?\315)
	 (?L . ?\305) (?N . ?\321) (?O . ?\323) (?R . ?\300) (?S . ?\246)
	 (?U . ?\332) (?Y . ?\335) (?Z . ?\254) (?a . ?\341) (?c . ?\346)
	 (?d . ?\360) (?e . ?\351) (?i . ?\355) (?l . ?\345) (?n . ?\361)
	 (?o . ?\363) (?r . ?\340) (?s . ?\266) (?u . ?\372) (?y . ?\375)
	 (?z . ?\274) (?' . ?\264) (?\  . ?')) 
     (?` (?A . ?\241) (?C . ?\307) (?E . ?\312) (?L . ?\243) (?S . ?\252)
	 (?T . ?\336) (?Z . ?\257) (?a . ?\261) (?l . ?\263) (?c . ?\347)
	 (?e . ?\352) (?s . ?\272) (?t . ?\376) (?z . ?\277) (?` . ?\252)
	 (?. . ?\377) (?\  . ?`))
     (?^ (?A . ?\302) (?O . ?\324) (?a . ?\342) (?o . ?\364)
	 (?^ . ?^)			; no special code?
	 (?\  . ?^))
     (?\" (?A . ?\304) (?E . ?\313) (?O . ?\326) (?U . ?\334) (?a . ?\344)
	  (?e . ?\353) (?o . ?\366) (?s . ?\337) (?u . ?\374) (?\" . ?\250)
	  (?\  . ?\"))
     (?\~ (?A . ?\303) (?C . ?\310) (?D . ?\317) (?L . ?\245) (?N . ?\322)
	  (?O . ?\325) (?R . ?\330) (?S . ?\251) (?T . ?\253) (?U . ?\333)
	  (?Z . ?\256) (?a . ?\323) (?c . ?\350) (?d . ?\357) (?l . ?\265)
	  (?n . ?\362) (?o . ?\365) (?r . ?\370) (?s . ?\271) (?t . ?\273)
	  (?u . ?\373) (?z . ?\276)
	  (?v . ?\242)			; v accent
	  (?\~ . ?\242)			; v accent
	  (?\. . ?\270)			; cedilla accent
	  (?\  . ?\~)))

    ("latin-1"
     (?' (?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
	 (?Y . ?\335) (?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363)
	 (?u . ?\372) (?y . ?\375) (?' . ?\264) (?\  . ?'))      
     (?` (?A . ?\300) (?E . ?\310) (?I . ?\314) (?O . ?\322) (?U . ?\331)
	 (?a . ?\340) (?e . ?\350) (?i . ?\354) (?o . ?\362) (?u . ?\371)
	 (?` . ?`) (?\  . ?`))
     (?^ (?A . ?\302) (?E . ?\312) (?I . ?\316) (?O . ?\324) (?U . ?\333)
	 (?a . ?\342) (?e . ?\352) (?i . ?\356) (?o . ?\364) (?u . ?\373)
	 (?^ . ?^) (?\  . ?^))
     (?\" (?A . ?\304) (?E . ?\313) (?I . ?\317) (?O . ?\326) (?U . ?\334)
	  (?a . ?\344) (?e . ?\353) (?i . ?\357) (?o . ?\366) (?s . ?\337)
	  (?u . ?\374) (?y . ?\377) (?\" . ?\250) (?\  . ?\"))
     (?\~ (?A . ?\303) (?C . ?\307) (?D . ?\320) (?N . ?\321) (?O . ?\325)
	  (?T . ?\336) (?a . ?\343) (?c . ?\347) (?d . ?\360) (?n . ?\361)
	  (?o . ?\365) (?t . ?\376) (?> . ?\273) (?< . ?\253) (?\~ . ?\270)
	  (?! . ?\241) (?? . ?\277)
	  (?\  . ?\~))
     (?\/ (?A . ?\305) (?E . ?\306) (?O . ?\330) (?a . ?\345) (?e . ?\346)
	  (?o . ?\370) (?\/ . ?\260) (?\  . ?\/))))
  "List of language-specific customizations for the ISO Accents mode.

Each element of the list is of the form

    (LANGUAGE
     (PSEUDO-ACCENT MAPPINGS)
     (PSEUDO-ACCENT MAPPINGS)
     ...)

LANGUAGE is a string naming the language.
PSEUDO-ACCENT is a char specifying an accent key.
MAPPINGS are cons cells of the form (CHAR . ISO-CHAR).

The net effect is that the key sequence PSEUDO-ACCENT CHAR is mapped
to ISO-CHAR on input.")

(defvar iso-language nil
  "Language for which ISO Accents mode is currently customized.
Change it with the `iso-accents-customize' function.")

(defvar iso-accents-list nil
  "Association list for ISO accent combinations, for the chosen language.")

(defvar iso-accents-mode nil
  "*Non-nil enables ISO Accents mode.
Setting this variable makes it local to the current buffer.
See the function `iso-accents-mode'.")
(make-variable-buffer-local 'iso-accents-mode)

(defvar iso-accents-enable '(?' ?` ?^ ?\" ?~ ?/)
  "*List of accent keys that become prefixes in ISO Accents mode.
The default is (?' ?` ?^ ?\" ?~ ?/), which contains all the supported
accent keys.  If you set this variable to a list in which some of those
characters are missing, the missing ones do not act as accents.

Note that if you specify a language with `iso-accents-customize',
that can also turn off certain prefixes (whichever ones are not needed in
the language you choose).")

(defun iso-accents-accent-key (prompt)
  "Modify the following character by adding an accent to it."
  ;; Pick up the accent character.
  (if (and iso-accents-mode
	   (memq last-input-char iso-accents-enable))
      (iso-accents-compose prompt)
    (char-to-string last-input-char)))

(defun iso-accents-compose (prompt)
  (let* ((first-char last-input-char)
	 (list (assq first-char iso-accents-list))
	 ;; Wait for the second key and look up the combination.
	 (second-char (if (or prompt
			      (not (eq (key-binding "a")
				       'self-insert-command))
			      ;; Not at start of a key sequence.
			      (> (length (this-single-command-keys)) 1)
			      ;; Called from anything but the command loop.
			      this-command)
			  (progn
			    (message "%s%c"
				     (or prompt "Compose with ")
				     first-char)
			    (read-event))
			(insert first-char)
			(prog1 (read-event)
			  (delete-region (1- (point)) (point)))))
	 (entry (cdr (assq second-char list))))
    (if entry
	;; Found it: return the mapped char
	(vector entry)
      ;; Otherwise, advance and schedule the second key for execution.
      (setq unread-command-events (list second-char))
      (vector first-char))))

;; It is a matter of taste if you want the minor mode indicated
;; in the mode line...
;; If so, uncomment the next four lines.
;; (or (assq 'iso-accents-mode minor-mode-map-alist)
;;     (setq minor-mode-alist
;; 	  (append minor-mode-alist
;; 		  '((iso-accents-mode " ISO-Acc")))))

;;;###autoload
(defun iso-accents-mode (&optional arg)
  "Toggle ISO Accents mode, in which accents modify the following letter.
This permits easy insertion of accented characters according to ISO-8859-1.
When Iso-accents mode is enabled, accent character keys
\(`, ', \", ^, / and ~) do not self-insert; instead, they modify the following
letter key so that it inserts an ISO accented letter.

You can customize ISO Accents mode to a particular language
with the command `iso-accents-customize'.

Special combinations: ~c gives a c with cedilla,
~d gives an Icelandic eth (d with dash).
~t gives an Icelandic thorn.
\"s gives German sharp s.
/a gives a with ring.
/e gives an a-e ligature.
~< and ~> give guillemots.
~! gives an inverted exclamation mark.
~? gives an inverted question mark.

With an argument, a positive argument enables ISO Accents mode, 
and a negative argument disables it."

  (interactive "P")

  (if (if arg
	  ;; Negative arg means switch it off.
	  (<= (prefix-numeric-value arg) 0)
	;; No arg means toggle.
	iso-accents-mode)
      (setq iso-accents-mode nil)

    ;; Enable electric accents.
    (setq iso-accents-mode t)))

(defun iso-accents-customize (language)
  "Customize the ISO accents machinery for a particular language.
It selects the customization based on the specifications in the
`iso-languages' variable."
  (interactive (list (completing-read "Language: " iso-languages nil t)))
  (let ((table (assoc language iso-languages))
	all-accents tail)
    (if (not table)
	(error "Unknown language '%s'" language)
      (setq iso-language language
	    iso-accents-list (cdr table))
      (if key-translation-map
	  (substitute-key-definition
	   'iso-accents-accent-key nil key-translation-map)
	(setq key-translation-map (make-sparse-keymap)))
      ;; Set up translations for all the characters that are used as
      ;; accent prefixes in this language.
      (setq tail iso-accents-list)
      (while tail
	(define-key key-translation-map (vector (car (car tail)))
	  'iso-accents-accent-key)
	(setq tail (cdr tail))))))

(defun iso-accentuate (start end)
  "Convert two-character sequences in region into accented characters.
Noninteractively, this operates on text from START to END.
This uses the same conversion that ISO Accents mode uses for type-in."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (forward-char 1)
      (let (entry)
	(while (< (point) end)
	  (if (and (memq (preceding-char) iso-accents-enable)
		   (setq entry (cdr (assq (following-char) (assq (preceding-char) iso-accents-list)))))
	      (progn
		(forward-char -1)
		(delete-char 2)
		(insert entry)
		(setq end (1- end)))
	    (forward-char 1)))))))

(defun iso-accent-rassoc-unit (value alist)
  (let (elt acc)
    (while (and alist (not elt))
      (setq acc (car (car alist))
	    elt (car (rassq value (cdr (car alist))))
	    alist (cdr alist)))
    (if elt
	(cons acc elt))))

(defun iso-unaccentuate (start end)
  "Convert accented characters in the region into two-character sequences.
Noninteractively, this operates on text from START to END.
This uses the opposite of the conversion done by ISO Accents mode for type-in."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (entry)
	(while (< (point) end)
	  (if (and (> (following-char) 127)
		   (setq entry (iso-accent-rassoc-unit (following-char)
						       iso-accents-list)))
	      (progn
		(delete-char 1)
		(insert (car entry) (cdr entry))
		(setq end (1+ end)))
	    (forward-char 1)))))))

(defun iso-deaccentuate (start end)
  "Convert accented characters in the region into unaccented characters.
Noninteractively, this operates on text from START to END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (entry)
	(while (< (point) end)
	  (if (and (> (following-char) 127)
		   (setq entry (iso-accent-rassoc-unit (following-char)
						       iso-accents-list)))
	      (progn
		(delete-char 1)
		(insert (cdr entry)))
	    (forward-char 1)))))))

;; Set up the default settings.
(iso-accents-customize "latin-1")

;; Use Iso-Accents mode in the minibuffer
;; if it was in use in the previous buffer.
(defun iso-acc-minibuf-setup ()
  (setq iso-accents-mode
	(save-excursion
	  (set-buffer (window-buffer minibuffer-scroll-window))
	  iso-accents-mode)))

(add-hook 'minibuf-setup-hook 'iso-acc-minibuf-setup)

;;; iso-acc.el ends here
