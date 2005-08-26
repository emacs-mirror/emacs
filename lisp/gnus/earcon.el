;;; earcon.el --- Sound effects for messages

;; Copyright (C) 1996, 2000, 2001, 2003 Free Software Foundation

;; Author: Steven L. Baur <steve@miranova.com>

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
;; This file provides access to sound effects in Gnus.

;;; Code:

(eval-when-compile (require 'cl))
(require 'gnus)
(require 'gnus-audio)
(require 'gnus-art)

(defgroup earcon nil
  "Turn ** sounds ** into noise."
  :group 'gnus-visual)

(defcustom earcon-prefix "**"
  "*String denoting the start of an earcon."
  :type 'string
  :group 'earcon)

(defcustom earcon-suffix "**"
  "String denoting the end of an earcon."
  :type 'string
  :group 'earcon)

(defcustom earcon-regexp-alist
  '(("boring" 1 "Boring.au")
    ("evil[ \t]+laugh" 1 "Evil_Laugh.au")
    ("gag\\|puke" 1 "Puke.au")
    ("snicker" 1 "Snicker.au")
    ("meow" 1 "catmeow.wav")
    ("sob\\|boohoo" 1 "cry.wav")
    ("drum[ \t]*roll" 1 "drumroll.au")
    ("blast" 1 "explosion.au")
    ("flush\\|plonk!*" 1 "flush.au")
    ("kiss" 1 "kiss.wav")
    ("tee[ \t]*hee" 1 "laugh.au")
    ("shoot" 1 "shotgun.wav")
    ("yawn" 1 "snore.wav")
    ("cackle" 1 "witch.au")
    ("yell\\|roar" 1 "yell2.au")
    ("whoop-de-doo" 1 "whistle.au"))
  "*A list of regexps to map earcons to real sounds."
  :type '(repeat (list regexp
		       (integer :tag "Match")
		       (string :tag "Sound")))
  :group 'earcon)
(defvar earcon-button-marker-list nil)
(make-variable-buffer-local 'earcon-button-marker-list)

;;; FIXME!! clone of code from gnus-vis.el FIXME!!
(defun earcon-article-push-button (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `earcon-callback' property,
call it with the value of the `earcon-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (let* ((pos (posn-point (event-start event)))
	 (data (get-text-property pos 'earcon-data))
	 (fun (get-text-property pos 'earcon-callback)))
    (if fun (funcall fun data))))

(defun earcon-article-press-button ()
  "Check text at point for a callback function.
If the text at point has a `earcon-callback' property,
call it with the value of the `earcon-data' text property."
  (interactive)
  (let* ((data (get-text-property (point) 'earcon-data))
	 (fun (get-text-property (point) 'earcon-callback)))
    (if fun (funcall fun data))))

(defun earcon-article-prev-button (n)
  "Move point to N buttons backward.
If N is negative, move forward instead."
  (interactive "p")
  (earcon-article-next-button (- n)))

(defun earcon-article-next-button (n)
  "Move point to N buttons forward.
If N is negative, move backward instead."
  (interactive "p")
  (let ((function (if (< n 0) 'previous-single-property-change
		    'next-single-property-change))
	(inhibit-point-motion-hooks t)
	(backward (< n 0))
	(limit (if (< n 0) (point-min) (point-max))))
    (setq n (abs n))
    (while (and (not (= limit (point)))
		(> n 0))
      ;; Skip past the current button.
      (when (get-text-property (point) 'earcon-callback)
	(goto-char (funcall function (point) 'earcon-callback nil limit)))
      ;; Go to the next (or previous) button.
      (gnus-goto-char (funcall function (point) 'earcon-callback nil limit))
      ;; Put point at the start of the button.
      (when (and backward (not (get-text-property (point) 'earcon-callback)))
	(goto-char (funcall function (point) 'earcon-callback nil limit)))
      ;; Skip past intangible buttons.
      (when (get-text-property (point) 'intangible)
	(incf n))
      (decf n))
    (unless (zerop n)
      (gnus-message 5 "No more buttons"))
    n))

(defun earcon-article-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (and (boundp gnus-article-button-face)
       gnus-article-button-face
       (gnus-overlay-put (gnus-make-overlay from to)
			 'face gnus-article-button-face))
  (gnus-add-text-properties
   from to
   (nconc (and gnus-article-mouse-face
	       (list gnus-mouse-face-prop gnus-article-mouse-face))
	  (list 'gnus-callback fun)
	  (and data (list 'gnus-data data)))))

(defun earcon-button-entry ()
  ;; Return the first entry in `gnus-button-alist' matching this place.
  (let ((alist earcon-regexp-alist)
	(case-fold-search t)
	(entry nil))
    (while alist
      (setq entry (pop alist))
      (if (looking-at (car entry))
	  (setq alist nil)
	(setq entry nil)))
    entry))

(defun earcon-button-push (marker)
  ;; Push button starting at MARKER.
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char marker)
    (let* ((entry (earcon-button-entry))
	   (inhibit-point-motion-hooks t)
	   (fun 'gnus-audio-play)
	   (args (list (nth 2 entry))))
      (cond
       ((fboundp fun)
	(apply fun args))
       ((and (boundp fun)
	     (fboundp (symbol-value fun)))
	(apply (symbol-value fun) args))
       (t
	(gnus-message 1 "You must define `%S' to use this button"
		      (cons fun args)))))))

;;; FIXME!! clone of code from gnus-vis.el FIXME!!

;;;###interactive
(defun earcon-region (beg end)
  "Play Sounds in the region between point and mark."
  (interactive "r")
  (earcon-buffer (current-buffer) beg end))

;;;###interactive
(defun earcon-buffer (&optional buffer st nd)
  (interactive)
  (save-excursion
    ;; clear old markers.
    (if (boundp 'earcon-button-marker-list)
	(while earcon-button-marker-list
	  (set-marker (pop earcon-button-marker-list) nil))
      (setq earcon-button-marker-list nil))
    (and buffer (set-buffer buffer))
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist earcon-regexp-alist)
	  beg entry regexp)
      (goto-char (point-min))
      (setq beg (point))
      (while (setq entry (pop alist))
	(setq regexp (concat (regexp-quote earcon-prefix)
			     ".*\\("
			     (car entry)
			     "\\).*"
			     (regexp-quote earcon-suffix)))
	(goto-char beg)
	(while (re-search-forward regexp nil t)
	  (let* ((start (and entry (match-beginning 1)))
		 (end (and entry (match-end 1)))
		 (from (match-beginning 1)))
	    (earcon-article-add-button
	     start end 'earcon-button-push
	     (car (push (set-marker (make-marker) from)
			earcon-button-marker-list)))
	    (gnus-audio-play (caddr entry))))))))

;;;###autoload
(defun gnus-earcon-display ()
  "Play sounds in message buffers."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char (point-min))
    ;; Skip headers
    (unless (search-forward "\n\n" nil t)
      (goto-char (point-max)))
    (sit-for 0)
    (earcon-buffer (current-buffer) (point))))

;;;***

(provide 'earcon)

(run-hooks 'earcon-load-hook)

;;; arch-tag: 844dfeea-980c-4ed0-907f-a30bf139691c
;;; earcon.el ends here
