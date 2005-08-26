;;; smerge-mode.el --- Minor mode to resolve diff3 conflicts

;; Copyright (C) 1999, 2000, 2001, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: revision-control merge diff3 cvs conflict

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

;; Provides a lightweight alternative to emerge/ediff.
;; To use it, simply add to your .emacs the following lines:
;;
;;   (autoload 'smerge-mode "smerge-mode" nil t)
;;
;; you can even have it turned on automatically with the following
;; piece of code in your .emacs:
;;
;;   (defun sm-try-smerge ()
;;     (save-excursion
;;   	 (goto-char (point-min))
;;   	 (when (re-search-forward "^<<<<<<< " nil t)
;;   	   (smerge-mode 1))))
;;   (add-hook 'find-file-hook 'sm-try-smerge t)

;;; Todo:

;; - if requested, ask the user whether he wants to call ediff right away

;;; Code:

(eval-when-compile (require 'cl))


;;; The real definition comes later.
(defvar smerge-mode)

(defgroup smerge ()
  "Minor mode to resolve diff3 conflicts."
  :group 'tools
  :prefix "smerge-")

(defcustom smerge-diff-buffer-name "*vc-diff*"
  "Buffer name to use for displaying diffs."
  :group 'smerge
  :type '(choice
	  (const "*vc-diff*")
	  (const "*cvs-diff*")
	  (const "*smerge-diff*")
	  string))

(defcustom smerge-diff-switches
  (append '("-d" "-b")
	  (if (listp diff-switches) diff-switches (list diff-switches)))
  "*A list of strings specifying switches to be passed to diff.
Used in `smerge-diff-base-mine' and related functions."
  :group 'smerge
  :type '(repeat string))

(defcustom smerge-auto-leave t
  "*Non-nil means to leave `smerge-mode' when the last conflict is resolved."
  :group 'smerge
  :type 'boolean)

(defface smerge-mine
  '((((min-colors 88) (background light))
     (:foreground "blue1"))
    (((background light))
     (:foreground "blue"))
    (((min-colors 88) (background dark))
     (:foreground "cyan1"))
    (((background dark))
     (:foreground "cyan")))
  "Face for your code."
  :group 'smerge)
;; backward-compatibility alias
(put 'smerge-mine-face 'face-alias 'smerge-mine)
(defvar smerge-mine-face 'smerge-mine)

(defface smerge-other
  '((((background light))
     (:foreground "darkgreen"))
    (((background dark))
     (:foreground "lightgreen")))
  "Face for the other code."
  :group 'smerge)
;; backward-compatibility alias
(put 'smerge-other-face 'face-alias 'smerge-other)
(defvar smerge-other-face 'smerge-other)

(defface smerge-base
  '((((min-colors 88) (background light))
     (:foreground "red1"))
    (((background light))
     (:foreground "red"))
    (((background dark))
     (:foreground "orange")))
  "Face for the base code."
  :group 'smerge)
;; backward-compatibility alias
(put 'smerge-base-face 'face-alias 'smerge-base)
(defvar smerge-base-face 'smerge-base)

(defface smerge-markers
  '((((background light))
     (:background "grey85"))
    (((background dark))
     (:background "grey30")))
  "Face for the conflict markers."
  :group 'smerge)
;; backward-compatibility alias
(put 'smerge-markers-face 'face-alias 'smerge-markers)
(defvar smerge-markers-face 'smerge-markers)

(easy-mmode-defmap smerge-basic-map
  `(("n" . smerge-next)
    ("p" . smerge-prev)
    ("r" . smerge-resolve)
    ("a" . smerge-keep-all)
    ("b" . smerge-keep-base)
    ("o" . smerge-keep-other)
    ("m" . smerge-keep-mine)
    ("E" . smerge-ediff)
    ("\C-m" . smerge-keep-current)
    ("=" . ,(make-sparse-keymap "Diff"))
    ("=<" "base-mine" . smerge-diff-base-mine)
    ("=>" "base-other" . smerge-diff-base-other)
    ("==" "mine-other" . smerge-diff-mine-other))
  "The base keymap for `smerge-mode'.")

(defcustom smerge-command-prefix "\C-c^"
  "Prefix for `smerge-mode' commands."
  :group 'smerge
  :type '(choice (string "\e") (string "\C-c^") (string "") string))

(easy-mmode-defmap smerge-mode-map
  `((,smerge-command-prefix . ,smerge-basic-map))
  "Keymap for `smerge-mode'.")

(defvar smerge-check-cache nil)
(make-variable-buffer-local 'smerge-check-cache)
(defun smerge-check (n)
  (condition-case nil
      (let ((state (cons (point) (buffer-modified-tick))))
	(unless (equal (cdr smerge-check-cache) state)
	  (smerge-match-conflict)
	  (setq smerge-check-cache (cons (match-data) state)))
	(nth (* 2 n) (car smerge-check-cache)))
    (error nil)))

(easy-menu-define smerge-mode-menu smerge-mode-map
  "Menu for `smerge-mode'."
  '("SMerge"
    ["Next" smerge-next :help "Go to next conflict"]
    ["Previous" smerge-prev :help "Go to previous conflict"]
    "--"
    ["Keep All" smerge-keep-all :help "Keep all three versions"
     :active (smerge-check 1)]
    ["Keep Current" smerge-keep-current :help "Use current (at point) version"
     :active (and (smerge-check 1) (> (smerge-get-current) 0))]
    "--"
    ["Revert to Base" smerge-keep-base :help "Revert to base version"
     :active (smerge-check 2)]
    ["Keep Other" smerge-keep-other :help "Keep `other' version"
     :active (smerge-check 3)]
    ["Keep Yours" smerge-keep-mine :help "Keep your version"
     :active (smerge-check 1)]
    "--"
    ["Diff Base/Mine" smerge-diff-base-mine
     :help "Diff `base' and `mine' for current conflict"
     :active (smerge-check 2)]
    ["Diff Base/Other" smerge-diff-base-other
     :help "Diff `base' and `other' for current conflict"
     :active (smerge-check 2)]
    ["Diff Mine/Other" smerge-diff-mine-other
     :help "Diff `mine' and `other' for current conflict"
     :active (smerge-check 1)]
    "--"
    ["Invoke Ediff" smerge-ediff
     :help "Use Ediff to resolve the conflicts"
     :active (smerge-check 1)]
    ["Auto Resolve" smerge-resolve
     :help "Try auto-resolution heuristics"
     :active (smerge-check 1)]
    ["Combine" smerge-combine-with-next
     :help "Combine current conflict with next"
     :active (smerge-check 1)]
    ))

(easy-menu-define smerge-context-menu nil
  "Context menu for mine area in `smerge-mode'."
  '(nil
    ["Keep Current" smerge-keep-current :help "Use current (at point) version"]
    ["Kill Current" smerge-kill-current :help "Remove current (at point) version"]
    ["Keep All" smerge-keep-all :help "Keep all three versions"]
    "---"
    ["More..." (popup-menu smerge-mode-menu) :help "Show full SMerge mode menu"]
    ))

(defconst smerge-font-lock-keywords
  '((smerge-find-conflict
     (1 smerge-mine-face prepend t)
     (2 smerge-base-face prepend t)
     (3 smerge-other-face prepend t)
     ;; FIXME: `keep' doesn't work right with syntactic fontification.
     (0 smerge-markers-face keep)
     (4 nil t t)
     (5 nil t t)))
  "Font lock patterns for `smerge-mode'.")

(defconst smerge-begin-re "^<<<<<<< \\(.*\\)\n")
(defconst smerge-end-re "^>>>>>>> .*\n")
(defconst smerge-base-re "^||||||| .*\n")
(defconst smerge-other-re "^=======\n")

(defvar smerge-conflict-style nil
  "Keep track of which style of conflict is in use.
Can be nil if the style is undecided, or else:
- `diff3-E'
- `diff3-A'")

;; Compiler pacifiers
(defvar font-lock-mode)
(defvar font-lock-keywords)

;;;;
;;;; Actual code
;;;;

;; Define smerge-next and smerge-prev
(easy-mmode-define-navigation smerge smerge-begin-re "conflict")

(defconst smerge-match-names ["conflict" "mine" "base" "other"])

(defun smerge-ensure-match (n)
  (unless (match-end n)
    (error "No `%s'" (aref smerge-match-names n))))

(defun smerge-auto-leave ()
  (when (and smerge-auto-leave
	     (save-excursion (goto-char (point-min))
			     (not (re-search-forward smerge-begin-re nil t))))
    (smerge-mode -1)))


(defun smerge-keep-all ()
  "Concatenate all versions."
  (interactive)
  (smerge-match-conflict)
  (let ((mb2 (or (match-beginning 2) (point-max)))
	(me2 (or (match-end 2) (point-min))))
    (delete-region (match-end 3) (match-end 0))
    (delete-region (max me2 (match-end 1)) (match-beginning 3))
    (if (and (match-end 2) (/= (match-end 1) (match-end 3)))
	(delete-region (match-end 1) (match-beginning 2)))
    (delete-region (match-beginning 0) (min (match-beginning 1) mb2))
    (smerge-auto-leave)))

(defun smerge-keep-n (n)
  ;; We used to use replace-match, but that did not preserve markers so well.
  (delete-region (match-end n) (match-end 0))
  (delete-region (match-beginning 0) (match-beginning n)))

(defun smerge-combine-with-next ()
  "Combine the current conflict with the next one."
  (interactive)
  (smerge-match-conflict)
  (let ((ends nil))
    (dolist (i '(3 2 1 0))
      (push (if (match-end i) (copy-marker (match-end i) t)) ends))
    (setq ends (apply 'vector ends))
    (goto-char (aref ends 0))
    (if (not (re-search-forward smerge-begin-re nil t))
	(error "No next conflict")
      (smerge-match-conflict)
      (let ((match-data (mapcar (lambda (m) (if m (copy-marker m)))
				(match-data))))
	;; First copy the in-between text in each alternative.
	(dolist (i '(1 2 3))
	  (when (aref ends i)
	    (goto-char (aref ends i))
	    (insert-buffer-substring (current-buffer)
				     (aref ends 0) (car match-data))))
	(delete-region (aref ends 0) (car match-data))
	;; Then move the second conflict's alternatives into the first.
	(dolist (i '(1 2 3))
	  (set-match-data match-data)
	  (when (and (aref ends i) (match-end i))
	    (goto-char (aref ends i))
	    (insert-buffer-substring (current-buffer)
				     (match-beginning i) (match-end i))))
	(delete-region (car match-data) (cadr match-data))
	;; Free the markers.
	(dolist (m match-data) (if m (move-marker m nil)))
	(mapc (lambda (m) (if m (move-marker m nil))) ends)))))

(defvar smerge-resolve-function
  (lambda () (error "Don't know how to resolve"))
  "Mode-specific merge function.
The function is called with no argument and with the match data set
according to `smerge-match-conflict'.")

(defvar smerge-text-properties
  `(help-echo "merge conflict: mouse-3 shows a menu"
    ;; mouse-face highlight
    keymap (keymap (down-mouse-3 . smerge-popup-context-menu))))

(defun smerge-remove-props (&optional beg end)
  (remove-text-properties
   (or beg (match-beginning 0))
   (or end (match-end 0))
   smerge-text-properties))

(defun smerge-popup-context-menu (event)
  "Pop up the Smerge mode context menu under mouse."
  (interactive "e")
  (if (and smerge-mode
	   (save-excursion (posn-set-point (event-end event)) (smerge-check 1)))
      (progn
	(posn-set-point (event-end event))
	(smerge-match-conflict)
	(let ((i (smerge-get-current))
	      o)
	  (if (<= i 0)
	      ;; Out of range
	      (popup-menu smerge-mode-menu)
	    ;; Install overlay.
	    (setq o (make-overlay (match-beginning i) (match-end i)))
	    (unwind-protect
		(progn
		  (overlay-put o 'face 'highlight)
		  (sit-for 0)		;Display the new highlighting.
		  (popup-menu smerge-context-menu))
	      ;; Delete overlay.
	      (delete-overlay o)))))
    ;; There's no conflict at point, the text-props are just obsolete.
    (save-excursion
      (let ((beg (re-search-backward smerge-end-re nil t))
	    (end (re-search-forward smerge-begin-re nil t)))
	(smerge-remove-props (or beg (point-min)) (or end (point-max)))
	(push event unread-command-events)))))

(defun smerge-resolve ()
  "Resolve the conflict at point intelligently.
This relies on mode-specific knowledge and thus only works in
some major modes.  Uses `smerge-resolve-function' to do the actual work."
  (interactive)
  (smerge-match-conflict)
  (smerge-remove-props)
  (cond
   ;; Trivial diff3 -A non-conflicts.
   ((and (eq (match-end 1) (match-end 3))
	 (eq (match-beginning 1) (match-beginning 3)))
    ;; FIXME: Add "if [ diff -b MINE OTHER ]; then select OTHER; fi"
    (smerge-keep-n 3))
   ((and (match-end 2)
	 ;; FIXME: Add "diff -b BASE MINE | patch OTHER".
	 ;; FIXME: Add "diff -b BASE OTHER | patch MINE".
	 nil)
    )
   ((and (not (match-end 2))
	 ;; FIXME: Add "diff -b"-based refinement.
	 nil)
    )
   (t
    ;; Mode-specific conflict resolution.
    (funcall smerge-resolve-function)))
  (smerge-auto-leave))

(defun smerge-keep-base ()
  "Revert to the base version."
  (interactive)
  (smerge-match-conflict)
  (smerge-ensure-match 2)
  (smerge-remove-props)
  (smerge-keep-n 2)
  (smerge-auto-leave))

(defun smerge-keep-other ()
  "Use \"other\" version."
  (interactive)
  (smerge-match-conflict)
  ;;(smerge-ensure-match 3)
  (smerge-remove-props)
  (smerge-keep-n 3)
  (smerge-auto-leave))

(defun smerge-keep-mine ()
  "Keep your version."
  (interactive)
  (smerge-match-conflict)
  ;;(smerge-ensure-match 1)
  (smerge-remove-props)
  (smerge-keep-n 1)
  (smerge-auto-leave))

(defun smerge-get-current ()
  (let ((i 3))
    (while (or (not (match-end i))
	       (< (point) (match-beginning i))
	       (>= (point) (match-end i)))
      (decf i))
    i))

(defun smerge-keep-current ()
  "Use the current (under the cursor) version."
  (interactive)
  (smerge-match-conflict)
  (let ((i (smerge-get-current)))
    (if (<= i 0) (error "Not inside a version")
      (smerge-remove-props)
      (smerge-keep-n i)
      (smerge-auto-leave))))

(defun smerge-kill-current ()
  "Remove the current (under the cursor) version."
  (interactive)
  (smerge-match-conflict)
  (let ((i (smerge-get-current)))
    (if (<= i 0) (error "Not inside a version")
      (smerge-remove-props)
      (let ((left nil))
	(dolist (n '(3 2 1))
	  (if (and (match-end n) (/= (match-end n) (match-end i)))
	      (push n left)))
	(if (and (cdr left)
		 (/= (match-end (car left)) (match-end (cadr left))))
	    (ding)			;We don't know how to do that.
	  (smerge-keep-n (car left))
	  (smerge-auto-leave))))))

(defun smerge-diff-base-mine ()
  "Diff 'base' and 'mine' version in current conflict region."
  (interactive)
  (smerge-diff 2 1))

(defun smerge-diff-base-other ()
  "Diff 'base' and 'other' version in current conflict region."
  (interactive)
  (smerge-diff 2 3))

(defun smerge-diff-mine-other ()
  "Diff 'mine' and 'other' version in current conflict region."
  (interactive)
  (smerge-diff 1 3))

(defun smerge-match-conflict ()
  "Get info about the conflict.  Puts the info in the `match-data'.
The submatches contain:
 0:  the whole conflict.
 1:  your code.
 2:  the base code.
 3:  other code.
An error is raised if not inside a conflict."
  (save-excursion
    (condition-case nil
	(let* ((orig-point (point))

	       (_ (forward-line 1))
	       (_ (re-search-backward smerge-begin-re))

	       (start (match-beginning 0))
	       (mine-start (match-end 0))
	       (filename (or (match-string 1) ""))

	       (_ (re-search-forward smerge-end-re))
	       (_ (assert (< orig-point (match-end 0))))

	       (other-end (match-beginning 0))
	       (end (match-end 0))

	       (_ (re-search-backward smerge-other-re start))

	       (mine-end (match-beginning 0))
	       (other-start (match-end 0))

	       base-start base-end)

	  ;; handle the various conflict styles
	  (cond
	   ((save-excursion
	      (goto-char mine-start)
	      (re-search-forward smerge-begin-re end t))
	    ;; There's a nested conflict and we're after the the beginning
	    ;; of the outer one but before the beginning of the inner one.
	    (error "There is a nested conflict"))

	   ((re-search-backward smerge-base-re start t)
	    ;; a 3-parts conflict
	    (set (make-local-variable 'smerge-conflict-style) 'diff3-A)
	    (setq base-end mine-end)
	    (setq mine-end (match-beginning 0))
	    (setq base-start (match-end 0)))

	   ((string= filename (file-name-nondirectory
			       (or buffer-file-name "")))
	    ;; a 2-parts conflict
	    (set (make-local-variable 'smerge-conflict-style) 'diff3-E))

	   ((and (not base-start)
		 (or (eq smerge-conflict-style 'diff3-A)
		     (equal filename "ANCESTOR")
		     (string-match "\\`[.0-9]+\\'" filename)))
	    ;; a same-diff conflict
	    (setq base-start mine-start)
	    (setq base-end   mine-end)
	    (setq mine-start other-start)
	    (setq mine-end   other-end)))

	  (let ((inhibit-read-only t)
		(inhibit-modification-hooks t)
		(m (buffer-modified-p)))
	    (unwind-protect
		(add-text-properties start end smerge-text-properties)
	      (restore-buffer-modified-p m)))

	  (store-match-data (list start end
				  mine-start mine-end
				  base-start base-end
				  other-start other-end
				  (when base-start (1- base-start)) base-start
				  (1- other-start) other-start))
	  t)
      (search-failed (error "Point not in conflict region")))))

(defun smerge-find-conflict (&optional limit)
  "Find and match a conflict region.  Intended as a font-lock MATCHER.
The submatches are the same as in `smerge-match-conflict'.
Returns non-nil if a match is found between the point and LIMIT.
The point is moved to the end of the conflict."
  (when (re-search-forward smerge-begin-re limit t)
    (condition-case err
	(progn
	  (smerge-match-conflict)
	  (goto-char (match-end 0)))
      (error (smerge-find-conflict limit)))))

(defun smerge-diff (n1 n2)
  (smerge-match-conflict)
  (smerge-ensure-match n1)
  (smerge-ensure-match n2)
  (let ((name1 (aref smerge-match-names n1))
	(name2 (aref smerge-match-names n2))
	;; Read them before the match-data gets clobbered.
	(beg1 (match-beginning n1))
	(end1 (match-end n1))
	(beg2 (match-beginning n2))
	(end2 (match-end n2))
	(file1 (make-temp-file "smerge1"))
	(file2 (make-temp-file "smerge2"))
	(dir default-directory)
	(file (file-relative-name buffer-file-name))
	(coding-system-for-read buffer-file-coding-system))
    (write-region beg1 end1 file1 nil 'nomessage)
    (write-region beg2 end2 file2 nil 'nomessage)
    (unwind-protect
	(with-current-buffer (get-buffer-create smerge-diff-buffer-name)
	  (setq default-directory dir)
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (let ((status
		   (apply 'call-process diff-command nil t nil
			  (append smerge-diff-switches
				  (list "-L" (concat name1 "/" file)
					"-L" (concat name2 "/" file)
					file1 file2)))))
	      (if (eq status 0) (insert "No differences found.\n"))))
	  (goto-char (point-min))
	  (diff-mode)
	  (display-buffer (current-buffer) t))
      (delete-file file1)
      (delete-file file2))))

;; compiler pacifiers
(defvar smerge-ediff-windows)
(defvar smerge-ediff-buf)
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)

;;;###autoload
(defun smerge-ediff (&optional name-mine name-other name-base)
  "Invoke ediff to resolve the conflicts.
NAME-MINE, NAME-OTHER, and NAME-BASE, if non-nil, are used for the
buffer names."
  (interactive)
  (let* ((buf (current-buffer))
	 (mode major-mode)
	 ;;(ediff-default-variant 'default-B)
	 (config (current-window-configuration))
	 (filename (file-name-nondirectory buffer-file-name))
	 (mine (generate-new-buffer
		(or name-mine (concat "*" filename " MINE*"))))
	 (other (generate-new-buffer
		 (or name-other (concat "*" filename " OTHER*"))))
	 base)
    (with-current-buffer mine
      (buffer-disable-undo)
      (insert-buffer-substring buf)
      (goto-char (point-min))
      (while (smerge-find-conflict)
	(when (match-beginning 2) (setq base t))
	(smerge-keep-n 1))
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (funcall mode))

    (with-current-buffer other
      (buffer-disable-undo)
      (insert-buffer-substring buf)
      (goto-char (point-min))
      (while (smerge-find-conflict)
	(smerge-keep-n 3))
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (funcall mode))

    (when base
      (setq base (generate-new-buffer
		  (or name-base (concat "*" filename " BASE*"))))
      (with-current-buffer base
	(buffer-disable-undo)
	(insert-buffer-substring buf)
	(goto-char (point-min))
	(while (smerge-find-conflict)
	  (if (match-end 2)
	      (smerge-keep-n 2)
	    (delete-region (match-beginning 0) (match-end 0))))
	(buffer-enable-undo)
	(set-buffer-modified-p nil)
	(funcall mode)))

    ;; the rest of the code is inspired from vc.el
    ;; Fire up ediff.
    (set-buffer
     (if base
	 (ediff-merge-buffers-with-ancestor mine other base)
	  ;; nil 'ediff-merge-revisions-with-ancestor buffer-file-name)
       (ediff-merge-buffers mine other)))
        ;; nil 'ediff-merge-revisions buffer-file-name)))

    ;; Ediff is now set up, and we are in the control buffer.
    ;; Do a few further adjustments and take precautions for exit.
    (set (make-local-variable 'smerge-ediff-windows) config)
    (set (make-local-variable 'smerge-ediff-buf) buf)
    (set (make-local-variable 'ediff-quit-hook)
	 (lambda ()
	   (let ((buffer-A ediff-buffer-A)
		 (buffer-B ediff-buffer-B)
		 (buffer-C ediff-buffer-C)
		 (buffer-Ancestor ediff-ancestor-buffer)
		 (buf smerge-ediff-buf)
		 (windows smerge-ediff-windows))
	     (ediff-cleanup-mess)
	     (with-current-buffer buf
	       (erase-buffer)
	       (insert-buffer buffer-C)
	       (kill-buffer buffer-A)
	       (kill-buffer buffer-B)
	       (kill-buffer buffer-C)
	       (when (bufferp buffer-Ancestor) (kill-buffer buffer-Ancestor))
	       (set-window-configuration windows)
	       (message "Conflict resolution finished; you may save the buffer")))))
    (message "Please resolve conflicts now; exit ediff when done")))


;;;###autoload
(define-minor-mode smerge-mode
  "Minor mode to simplify editing output from the diff3 program.
\\{smerge-mode-map}"
  :group 'smerge :lighter " SMerge"
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (set (make-local-variable 'font-lock-multiline) t)
    (save-excursion
      (if smerge-mode
	  (font-lock-add-keywords nil smerge-font-lock-keywords 'append)
	(font-lock-remove-keywords nil smerge-font-lock-keywords))
      (goto-char (point-min))
      (while (smerge-find-conflict)
	(save-excursion
	  (font-lock-fontify-region (match-beginning 0) (match-end 0) nil))))))


(provide 'smerge-mode)

;; arch-tag: 605c8d1e-e43d-4943-a6f3-1bcc4333e690
;;; smerge-mode.el ends here
