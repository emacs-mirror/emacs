;;; iso-acc.el -- minor mode providing electric accent keys
;;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Johan Vromans <jv@mh.nl>
;; Version: 1.7

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

;; Function `iso-accents-mode' activates a minor mode
;; (`iso-accents-minor-mode') in which typewriter "dead keys" are
;; emulated.  The purpose of this emulation is to provide a simple
;; means for inserting accented characters according to the ISO-8859-1
;; character set.
;;
;; In `iso-accents-minor-mode', pseudo accent characters are used to
;; introduce accented keys.  The pseudo-accent characterss are:
;;
;;   '  (minute)    -> grave accent
;;   `  (backtick)  -> acute accent
;;   "  (second)    -> diaeresis
;;   ^  (caret)     -> circonflexe
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

;;; Code:

(provide 'iso-acc)

(defvar iso-accents-list
  '(((?' ?A) ?\301)
    ((?' ?E) ?\311)
    ((?' ?I) ?\315)
    ((?' ?O) ?\323)
    ((?' ?U) ?\332)
    ((?' ?a) ?\341)
    ((?' ?e) ?\351)
    ((?' ?i) ?\355)
    ((?' ?o) ?\363)
    ((?' ?u) ?\372)
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
    ((?` ?A) ?\302)
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
    ((?\" ?u) ?\374)
    ((?\" ? ) ?\")
    ((?\" ?\") ?\250)
    )
  "Association list for ISO accent combinations.")

(defun iso-accents-accent-key ()
  "Modify the following character by adding an accent to it."
  (interactive)

  ;; Pick up the accent character.
  (let ((first-char last-command-char))

    ;; Display it and backup.
    (insert first-char)
    (backward-char 1)

    ;; Wait for the second key and look up the combination in the list.
    (let* ((second-char (read-event))
	   (entry (assoc (list first-char second-char) iso-accents-list)))
      (if entry
	  ;; Found it: delete the first character and insert the combination.
	  (progn
	    (delete-char 1)
	    (insert (car (cdr entry))))

	;; Otherwise, advance and schedule the second key for execution.
	(forward-char 1)
	(setq unread-command-events (list second-char))

	;; If it is not a self-insert-command, ring the terminal bell.
	(or (eq (key-binding (make-vector 1 second-char)) 'self-insert-command)
	    (beep 1))))))

(defvar iso-accents-minor-mode nil
  "*Non-nil enables ISO-accents mode.
Setting this variable makes it local to the current buffer.
See `iso-accents-mode'.")
(make-variable-buffer-local 'iso-accents-minor-mode)

;; A minor mode map `iso-accents-prefix-map' is used to activate the
;; dead key handling depending on the value of iso-accents-minor-mode.
(defvar iso-accents-prefix-map nil
  "Keymap for ISO-accents minor mode.")

;; Create the minor-mode keymap, if needed.
(or iso-accents-prefix-map
    (progn
      (setq iso-accents-prefix-map (make-sparse-keymap))
      (define-key iso-accents-prefix-map "'"  'iso-accents-dead-key)
      (define-key iso-accents-prefix-map "`"  'iso-accents-dead-key)
      (define-key iso-accents-prefix-map "^"  'iso-accents-dead-key)
      (define-key iso-accents-prefix-map "\"" 'iso-accents-dead-key)))

;; Add the dead key minor mode map to the minor mode maps.
(or (assq 'iso-accents-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'iso-accents-minor-mode iso-accents-prefix-map)
		minor-mode-map-alist)))

;; It is a matter of taste if you want the minor mode indicated
;; in the mode line...
;; If so, uncomment the next four lines.
;; (or (assq 'iso-accents-minor-mode minor-mode-map-alist)
;;     (setq minor-mode-alist
;; 	  (append minor-mode-alist
;; 		  '((iso-accents-minor-mode " ISO-Acc")))))

;;;###autoload
(defun iso-accents-mode (&optional arg)
  "Toggle a minor mode in which accents modify the following letter.
This permits easy insertion of accented characters according to ISO-8859-1.
When Iso-accents mode is enabled, accent character keys
\(', \", ^ and ~) do not self-insert; instead, they modify the following
letter key so that it inserts an ISO accented letter.

With an argument, a positive argument enables ISO-accents mode, 
and a negative argument disables it."

  (interactive "P")

  (if (if arg
	  ;; Negative arg means switch it off.
	  (<= (prefix-numeric-value arg) 0)
	;; No arg means toggle.
	iso-accents-minor-mode)
      (setq iso-accents-minor-mode nil)

    ;; Enable electric accents.
    (setq iso-accents-minor-mode t)))

;;; iso-acc.el ends here
