;;; iso-acc.el -- minor mode providing electric accent keys
;;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: Johan Vromans <jv@mh.nl>
;; Version: 1.7 (modified)
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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
;; set iso-accents-enable to the list of characters that should be special.

;;; Code:

(provide 'iso-acc)

(defvar iso-accents-list
  '(((?' ?A) ?\301)
    ((?' ?E) ?\311)
    ((?' ?I) ?\315)
    ((?' ?O) ?\323)
    ((?' ?U) ?\332)
    ((?' ?Y) ?\335)
    ((?' ?a) ?\341)
    ((?' ?e) ?\351)
    ((?' ?i) ?\355)
    ((?' ?o) ?\363)
    ((?' ?u) ?\372)
    ((?' ?y) ?\375)
    ((?' ?') ?\264)
    ((?' ? ) ?')
    ((?` ?A) ?\300)
    ((?` ?E) ?\310)
    ((?` ?I) ?\314)
    ((?` ?O) ?\322)
    ((?` ?U) ?\331)
    ((?` ?a) ?\340)
    ((?` ?e) ?\350)
    ((?` ?i) ?\354)
    ((?` ?o) ?\362)
    ((?` ?u) ?\371)
    ((?` ? ) ?`)
    ((?` ?`) ?`)		; no special code?
    ((?^ ?A) ?\302)
    ((?^ ?E) ?\312)
    ((?^ ?I) ?\316)
    ((?^ ?O) ?\324)
    ((?^ ?U) ?\333)
    ((?^ ?a) ?\342)
    ((?^ ?e) ?\352)
    ((?^ ?i) ?\356)
    ((?^ ?o) ?\364)
    ((?^ ?u) ?\373)
    ((?^ ? ) ?^)
    ((?^ ?^) ?^)		; no special code?
    ((?\" ?A) ?\304)
    ((?\" ?E) ?\313)
    ((?\" ?I) ?\317)
    ((?\" ?O) ?\326)
    ((?\" ?U) ?\334)
    ((?\" ?a) ?\344)
    ((?\" ?e) ?\353)
    ((?\" ?i) ?\357)
    ((?\" ?o) ?\366)
    ((?\" ?s) ?\337)
    ((?\" ?u) ?\374)
    ((?\" ?y) ?\377)
    ((?\" ? ) ?\")
    ((?\" ?\") ?\250)
    ((?\~ ?A) ?\303)
    ((?\~ ?C) ?\307)
    ((?\~ ?D) ?\320)
    ((?\~ ?N) ?\321)
    ((?\~ ?O) ?\325)
    ((?\~ ?T) ?\336)
    ((?\~ ?a) ?\343)
    ((?\~ ?c) ?\347)
    ((?\~ ?d) ?\360)
    ((?\~ ?n) ?\361)
    ((?\~ ?o) ?\365)
    ((?\~ ?t) ?\376)
    ((?\~ ?>) ?\273)
    ((?\~ ?<) ?\253)
    ((?\~ ?!) ?\241)  ;; Inverted exclamation mark
    ((?\~ ??) ?\277)  ;; Inverted question mark
    ((?\~ ?\ ) ?\~)
    ((?\~ ?\~) ?\270) ;; cedilla accent
    ((?\/ ?A) ?\305) ;; A-with-ring (Norwegian and Danish)
    ((?\/ ?E) ?\306) ;; AE-ligature (Norwegian and Danish)
    ((?\/ ?O) ?\330)
    ((?\/ ?a) ?\345) ;; a-with-ring (Norwegian and Danish)
    ((?\/ ?e) ?\346) ;; ae-ligature (Norwegian and Danish)
    ((?\/ ?o) ?\370)
    ((?\/ ?\ ) ?\/)
    ((?\/ ?\/) ?\260) ;; ring accent (actually degree sign?)
    )
  "Association list for ISO accent combinations.")

(defvar iso-accents-mode nil
  "*Non-nil enables ISO Accents mode.
Setting this variable makes it local to the current buffer.
See function `iso-accents-mode'.")
(make-variable-buffer-local 'iso-accents-mode)

(defun iso-accents-accent-key (prompt)
  "Modify the following character by adding an accent to it."
  ;; Pick up the accent character.
  (if iso-accents-mode
      (iso-accents-compose prompt)
    (char-to-string last-input-char)))

(defun iso-accents-compose-key (prompt)
  "Modify the following character by adding an accent to it."
  ;; Pick up the accent character.
  (let ((combined (iso-accents-compose prompt)))
    (if unread-command-events
	(let ((unread unread-command-events))
	  (setq unread-command-events nil)
	  (error "Characters %s and %s cannot be composed"
		 (single-key-description (aref combined 0))
		 (single-key-description (car unread)))))
    combined))

(defun iso-accents-compose (prompt)
  (let* ((first-char last-input-char)
	 ;; Wait for the second key and look up the combination.
	 (second-char (if (or prompt
			      (not (eq (key-binding "a")
				       'self-insert-command)))
			  (progn
			    (message "%s%c"
				     (or prompt "Compose with ")
				     first-char)
			    (read-event))
			(insert first-char)
			(prog1 (read-event)
			  (delete-region (1- (point)) (point)))))
	 (entry (assoc (list first-char second-char) iso-accents-list)))
    (if entry
	;; Found it: delete the first character and insert the combination.
	(concat (list (nth 1 entry)))
      ;; Otherwise, advance and schedule the second key for execution.
      (setq unread-command-events (list second-char))
      (vector first-char))))

(defvar iso-accents-enable '(?' ?` ?^ ?\" ?~ ?/)
  "*List of accent keys that become prefixes in ISO Accents mode.
The default is (?' ?` ?^ ?\" ?~ ?/), which contains all the supported
accent keys.  For certain languages, you might want to remove some of
those characters that are not actually used.")

(or key-translation-map (setq key-translation-map (make-sparse-keymap)))
;; For sequences starting with an accent character,
;; use a function that tests iso-accents-mode.
(if (memq ?' iso-accents-enable)
    (define-key key-translation-map "'"  'iso-accents-accent-key))
(if (memq ?` iso-accents-enable)
    (define-key key-translation-map "`"  'iso-accents-accent-key))
(if (memq ?^ iso-accents-enable)
    (define-key key-translation-map "^"  'iso-accents-accent-key))
(if (memq ?\" iso-accents-enable)
    (define-key key-translation-map "\"" 'iso-accents-accent-key))
(if (memq ?~ iso-accents-enable)
    (define-key key-translation-map "~" 'iso-accents-accent-key))
(if (memq ?/ iso-accents-enable)
    (define-key key-translation-map "/" 'iso-accents-accent-key))

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

The variable `iso-accents-enable' specifies the list of characters to
enable as accents.  If you don't need all of them, remove the ones you
don't need from that list.

Special combinations: ~c gives a c with cedilla,
~d gives an Icelandic eth (d with dash).
~t gives an Icelandic thorn.
\"s gives German sharp s.
/a gives a with ring.
/e gives an a-e ligature.
~< and ~> give guillemets.
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

;;; iso-acc.el ends here
