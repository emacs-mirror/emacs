;; Calculator for GNU Emacs, part II [calc-embed.el]
;; Copyright (C) 1990, 1991, 1992, 1993, 2001 Free Software Foundation, Inc.
;; Written by Dave Gillespie, daveg@synaptics.com.

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



;; This file is autoloaded from calc-ext.el.
(require 'calc-ext)

(require 'calc-macs)

(defun calc-Need-calc-embed () nil)


(defun calc-show-plain (n)
  (interactive "P")
  (calc-wrapper
   (calc-set-command-flag 'renum-stack)
   (message (if (calc-change-mode 'calc-show-plain n nil t)
		"Including \"plain\" formulas in Calc Embedded mode."
	      "Omitting \"plain\" formulas in Calc Embedded mode.")))
)




;;; Things to do for Embedded Mode:
;;; 
;;;  Detect and strip off unexpected labels during reading.
;;;
;;;  Get calc-grab-region to use math-read-big-expr.
;;;  If calc-show-plain, main body should have only righthand side of => expr.
;;;  Handle tabs that have crept into embedded formulas.
;;;  After "switching to new formula", home cursor to that formula.
;;;  Do something like \evalto ... \to for \gets operators.
;;;


(defvar calc-embedded-modes nil)
(defvar calc-embedded-globals nil)
(defvar calc-embedded-active nil)

(make-variable-buffer-local 'calc-embedded-all-active)
(make-variable-buffer-local 'calc-embedded-some-active)


(defvar calc-embedded-open-formula "\\`\\|^\n\\|\\$\\$?\\|\\\\\\[\\|^\\\\begin.*\n\\|^@.*\n\\|^\\.EQ.*\n\\|\\\\(\\|^%\n\\|^\\.\\\\\"\n"
  "*A regular expression for the opening delimiter of a formula used by
calc-embedded.")

(defvar calc-embedded-close-formula "\\'\\|\n$\\|\\$\\$?\\|\\\\]\\|^\\\\end.*\n\\|^@.*\n\\|^\\.EN.*\n\\|\\\\)\\|\n%\n\\|^\\.\\\\\"\n"
  "*A regular expression for the closing delimiter of a formula used by
calc-embedded.")

(defvar calc-embedded-open-word "^\\|[^-+0-9.eE]"
  "*A regular expression for the opening delimiter of a formula used by
calc-embedded-word.")

(defvar calc-embedded-close-word "$\\|[^-+0-9.eE]"
  "*A regular expression for the closing delimiter of a formula used by
calc-embedded-word.")

(defvar calc-embedded-open-plain "%%% "
  "*A string which is the opening delimiter for a \"plain\" formula.
If calc-show-plain mode is enabled, this is inserted at the front of
each formula.")

(defvar calc-embedded-close-plain " %%%\n"
  "*A string which is the closing delimiter for a \"plain\" formula.
See calc-embedded-open-plain.")

(defvar calc-embedded-open-new-formula "\n\n"
  "*A string which is inserted at front of formula by calc-embedded-new-formula.")

(defvar calc-embedded-close-new-formula "\n\n"
  "*A string which is inserted at end of formula by calc-embedded-new-formula.")

(defvar calc-embedded-announce-formula "%Embed\n\\(% .*\n\\)*"
  "*A regular expression which is sure to be followed by a calc-embedded formula." )

(defvar calc-embedded-open-mode "% "
  "*A string which should precede calc-embedded mode annotations.
This is not required to be present for user-written mode annotations.")

(defvar calc-embedded-close-mode "\n"
  "*A string which should follow calc-embedded mode annotations.
This is not required to be present for user-written mode annotations.")


(defconst calc-embedded-mode-vars '(("precision" . calc-internal-prec)
				    ("word-size" . calc-word-size)
				    ("angles" . calc-angle-mode)
				    ("symbolic" . calc-symbolic-mode)
				    ("matrix" . calc-matrix-mode)
				    ("fractions" . calc-prefer-frac)
				    ("complex" . calc-complex-mode)
				    ("simplify" . calc-simplify-mode)
				    ("language" . the-language)
				    ("plain" . calc-show-plain)
				    ("break" . calc-line-breaking)
				    ("justify" . the-display-just)
				    ("left-label" . calc-left-label)
				    ("right-label" . calc-right-label)
				    ("radix" . calc-number-radix)
				    ("leading-zeros" . calc-leading-zeros)
				    ("grouping" . calc-group-digits)
				    ("group-char" . calc-group-char)
				    ("point-char" . calc-point-char)
				    ("frac-format" . calc-frac-format)
				    ("float-format" . calc-float-format)
				    ("complex-format" . calc-complex-format)
				    ("hms-format" . calc-hms-format)
				    ("date-format" . calc-date-format)
				    ("matrix-justify" . calc-matrix-just)
				    ("full-vectors" . calc-full-vectors)
				    ("break-vectors" . calc-break-vectors)
				    ("vector-commas" . calc-vector-commas)
				    ("vector-brackets" . calc-vector-brackets)
				    ("matrix-brackets" . calc-matrix-brackets)
				    ("strings" . calc-display-strings)
))


;;; Format of calc-embedded-info vector:
;;;    0   Editing buffer.
;;;    1   Calculator buffer.
;;;    2   Top of current formula (marker).
;;;    3   Bottom of current formula (marker).
;;;    4   Top of current formula's delimiters (marker).
;;;    5   Bottom of current formula's delimiters (marker).
;;;    6   String representation of current formula.
;;;    7   Non-nil if formula is embedded within a single line.
;;;    8   Internal representation of current formula.
;;;    9   Variable assigned by this formula, or nil.
;;;   10   List of variables upon which this formula depends.
;;;   11   Evaluated value of the formula, or nil.
;;;   12   Mode settings for current formula.
;;;   13   Local mode settings for current formula.
;;;   14   Permanent mode settings for current formula.
;;;   15   Global mode settings for editing buffer.


;;; calc-embedded-active is an a-list keyed on buffers; each cdr is a
;;; sorted list of calc-embedded-infos in that buffer.  We do this
;;; rather than using buffer-local variables because the latter are
;;; thrown away when a buffer changes major modes.


(defun calc-do-embedded (arg end obeg oend)
  (if calc-embedded-info

      ;; Turn embedded mode off or switch to a new buffer.
      (cond ((eq (current-buffer) (aref calc-embedded-info 1))
	     (let ((calcbuf (current-buffer))
		   (buf (aref calc-embedded-info 0)))
	       (calc-embedded-original-buffer t)
	       (calc-embedded nil)
	       (switch-to-buffer calcbuf)))

	    ((eq (current-buffer) (aref calc-embedded-info 0))
	     (let* ((info calc-embedded-info)
		    (mode calc-embedded-modes))
	       (save-excursion
		 (set-buffer (aref info 1))
		 (if (and (> (calc-stack-size) 0)
			  (equal (calc-top 1 'full) (aref info 8)))
		     (let ((calc-no-refresh-evaltos t))
		       (if (calc-top 1 'sel)
			   (calc-unselect 1))
		       (calc-embedded-set-modes
			(aref info 15) (aref info 12) (aref info 14))
		       (let ((calc-embedded-info nil))
			 (calc-wrapper (calc-pop-stack))))
		   (calc-set-mode-line)))
	       (setq calc-embedded-info nil
		     mode-line-buffer-identification (car mode)
		     truncate-lines (nth 2 mode)
		     buffer-read-only nil)
	       (use-local-map (nth 1 mode))
	       (set-buffer-modified-p (buffer-modified-p))
	       (or calc-embedded-quiet
		   (message "Back to %s mode." mode-name))))

	    (t
	     (if (buffer-name (aref calc-embedded-info 0))
		 (save-excursion
		   (set-buffer (aref calc-embedded-info 0))
		   (or (y-or-n-p (format "Cancel Calc Embedded mode in buffer %s? "
					 (buffer-name)))
		       (keyboard-quit))
		   (calc-embedded nil)))
	     (calc-embedded arg end obeg oend)))

    ;; Turn embedded mode on.
    (calc-plain-buffer-only)
    (let ((modes (list mode-line-buffer-identification
		       (current-local-map)
		       truncate-lines))
	  top bot outer-top outer-bot
	  info chg ident)
      (barf-if-buffer-read-only)
      (or calc-embedded-globals
	  (calc-find-globals))
      (setq info (calc-embedded-make-info (point) nil t arg end obeg oend))
      (if (eq (car-safe (aref info 8)) 'error)
	  (progn
	    (goto-char (nth 1 (aref info 8)))
	    (error (nth 2 (aref info 8)))))
      (let ((mode-line-buffer-identification mode-line-buffer-identification)
	    (calc-embedded-info info)
	    (calc-embedded-no-reselect t))
	(calc-wrapper
	 (let* ((okay nil)
		(calc-no-refresh-evaltos t))
	   (setq chg (calc-embedded-set-modes
		      (aref info 15) (aref info 12) (aref info 13)))
	   (if (aref info 8)
	       (calc-push (calc-normalize (aref info 8)))
	     (calc-alg-entry)))
	 (setq calc-undo-list nil
	       calc-redo-list nil
	       ident mode-line-buffer-identification)))
      (setq calc-embedded-info info
	    calc-embedded-modes modes
	    mode-line-buffer-identification ident
	    truncate-lines t
	    buffer-read-only t)
      (set-buffer-modified-p (buffer-modified-p))
      (use-local-map calc-mode-map)
      (setq calc-no-refresh-evaltos nil)
      (and chg calc-any-evaltos (calc-wrapper (calc-refresh-evaltos)))
      (or (eq calc-embedded-quiet t)
	  (message "Embedded Calc mode enabled.  %s to return to normal."
		   (if calc-embedded-quiet
		       "Type `M-# x'"
		     "Give this command again")))))
  (scroll-down 0)    ; fix a bug which occurs when truncate-lines is changed.
)
(setq calc-embedded-quiet nil)


(defun calc-embedded-select (arg)
  (interactive "P")
  (calc-embedded arg)
  (and calc-embedded-info
       (eq (car-safe (aref calc-embedded-info 8)) 'calcFunc-evalto)
       (calc-select-part 1))
  (and calc-embedded-info
       (or (eq (car-safe (aref calc-embedded-info 8)) 'calcFunc-assign)
	   (and (eq (car-safe (aref calc-embedded-info 8)) 'calcFunc-evalto)
		(eq (car-safe (nth 1 (aref calc-embedded-info 8)))
		    'calcFunc-assign)))
       (calc-select-part 2))
)


(defun calc-embedded-update-formula (arg)
  (interactive "P")
  (if arg
      (let ((entry (assq (current-buffer) calc-embedded-active)))
	(while (setq entry (cdr entry))
	  (and (eq (car-safe (aref (car entry) 8)) 'calcFunc-evalto)
	       (or (not (consp arg))
		   (and (<= (aref (car entry) 2) (region-beginning))
			(>= (aref (car entry) 3) (region-end))))
	       (save-excursion
		 (calc-embedded-update (car entry) 14 t t)))))
    (if (and calc-embedded-info
	     (eq (current-buffer) (aref calc-embedded-info 0))
	     (>= (point) (aref calc-embedded-info 4))
	     (<= (point) (aref calc-embedded-info 5)))
	(calc-evaluate 1)
      (let* ((opt (point))
	     (info (calc-embedded-make-info (point) nil t))
	     (pt (- opt (aref info 4))))
	(or (eq (car-safe (aref info 8)) 'error)
	    (progn
	      (save-excursion
		(calc-embedded-update info 14 'eval t))
	      (goto-char (+ (aref info 4) pt)))))))
)


(defun calc-embedded-edit (arg)
  (interactive "P")
  (let ((info (calc-embedded-make-info (point) nil t arg))
	str)
    (if (eq (car-safe (aref info 8)) 'error)
	(progn
	  (goto-char (nth 1 (aref info 8)))
	  (error (nth 2 (aref info 8)))))
    (calc-wrapper
     (setq str (math-showing-full-precision
		(math-format-nice-expr (aref info 8) (frame-width))))
     (calc-edit-mode (list 'calc-embedded-finish-edit info))
     (insert str "\n")))
  (calc-show-edit-buffer)
)

(defun calc-embedded-finish-edit (info)
  (let ((buf (current-buffer))
	(str (buffer-substring (point) (point-max)))
	(start (point))
	pos)
    (switch-to-buffer calc-original-buffer)
    (let ((val (save-excursion
		 (set-buffer (aref info 1))
		 (let ((calc-language nil)
		       (math-expr-opers math-standard-opers))
		   (math-read-expr str)))))
      (if (eq (car-safe val) 'error)
	  (progn
	    (switch-to-buffer buf)
	    (goto-char (+ start (nth 1 val)))
	    (error (nth 2 val))))
      (calc-embedded-original-buffer t info)
      (aset info 8 val)
      (calc-embedded-update info 14 t t)))
)

(defun calc-do-embedded-activate (arg cbuf)
  (calc-plain-buffer-only)
  (if arg
      (calc-embedded-forget))
  (calc-find-globals)
  (if (< (prefix-numeric-value arg) 0)
      (message "Deactivating %s for Calc Embedded mode." (buffer-name))
    (message "Activating %s for Calc Embedded mode..." (buffer-name))
    (save-excursion
      (let* ((active (assq (current-buffer) calc-embedded-active))
	     (info active)
	     (pat " := \\| \\\\gets \\| => \\| \\\\evalto "))
	(if calc-embedded-announce-formula
	    (setq pat (format "%s\\|\\(%s\\)"
			      pat calc-embedded-announce-formula)))
	(while (setq info (cdr info))
	  (or (equal (buffer-substring (aref (car info) 2) (aref (car info) 3))
		     (aref (car info) 6))
	      (setcdr active (delq (car info) (cdr active)))))
	(goto-char (point-min))
	(while (re-search-forward pat nil t)
	  (if (looking-at calc-embedded-open-formula)
	      (goto-char (match-end 1)))
	  (setq info (calc-embedded-make-info (point) cbuf nil))
	  (or (eq (car-safe (aref info 8)) 'error)
	      (goto-char (aref info 5))))))
    (message "Activating %s for Calc Embedded mode...done" (buffer-name)))
  (calc-embedded-active-state t)
)

(defun calc-plain-buffer-only ()
  (if (memq major-mode '(calc-mode calc-trail-mode calc-edit-mode))
      (error "This command should be used in a normal editing buffer"))
)

(defun calc-embedded-active-state (state)
  (or (assq 'calc-embedded-all-active minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(calc-embedded-all-active " Active")
		  (cons '(calc-embedded-some-active " ~Active")
			minor-mode-alist))))
  (let ((active (assq (current-buffer) calc-embedded-active)))
    (or (cdr active)
	(setq state nil)))
  (and (eq state 'more) calc-embedded-all-active (setq state t))
  (setq calc-embedded-all-active (eq state t)
	calc-embedded-some-active (not (memq state '(nil t))))
  (set-buffer-modified-p (buffer-modified-p))
)


(defun calc-embedded-original-buffer (switch &optional info)
  (or info (setq info calc-embedded-info))
  (or (buffer-name (aref info 0))
      (progn
	(error "Calc embedded mode: Original buffer has been killed")))
  (if switch
      (set-buffer (aref info 0)))
)

(defun calc-embedded-word ()
  (interactive)
  (calc-embedded '(4))
)

(defun calc-embedded-mark-formula (&optional body-only)
  "Put point at the beginning of this Calc formula, mark at the end.
This normally marks the whole formula, including surrounding delimiters.
With any prefix argument, marks only the formula itself."
  (interactive "P")
  (and (eq major-mode 'calc-mode)
       (error "This command should be used in a normal editing buffer"))
  (let (top bot outer-top outer-bot)
    (save-excursion
      (calc-embedded-find-bounds body-only))
    (push-mark (if body-only bot outer-bot) t)
    (goto-char (if body-only top outer-top)))
)

(defun calc-embedded-find-bounds (&optional plain)
  ;; (while (and (bolp) (eq (following-char) ?\n))
  ;;  (forward-char 1))
  (and (eolp) (bolp) (not (eq (char-after (- (point) 2)) ?\n))
       (forward-char -1))
  (let ((home (point)))
    (or (and (looking-at calc-embedded-open-formula)
	     (not (looking-at calc-embedded-close-formula)))
	(re-search-backward calc-embedded-open-formula nil t)
	(error "Can't find start of formula"))
    (and (eq (preceding-char) ?\$)  ; backward search for \$\$? won't back
	 (eq (following-char) ?\$)  ; up over a second $, so do it by hand.
	 (forward-char -1))
    (setq outer-top (point))
    (goto-char (match-end 0))
    (if (eq (following-char) ?\n)
	(forward-char 1))
    (or (bolp)
	(while (eq (following-char) ?\ )
	  (forward-char 1)))
    (or (eq plain 'plain)
	(if (looking-at (regexp-quote calc-embedded-open-plain))
	    (progn
	      (goto-char (match-end 0))
	      (search-forward calc-embedded-close-plain))))
    (setq top (point))
    (or (re-search-forward calc-embedded-close-formula nil t)
	(error "Can't find end of formula"))
    (if (< (point) home)
	(error "Not inside a formula"))
    (and (eq (following-char) ?\n) (not (bolp))
	 (forward-char 1))
    (setq outer-bot (point))
    (goto-char (match-beginning 0))
    (if (eq (preceding-char) ?\n)
	(backward-char 1))
    (or (eolp)
	(while (eq (preceding-char) ?\ )
	  (backward-char 1)))
    (setq bot (point)))
)

(defun calc-embedded-kill-formula ()
  "Kill the formula surrounding point.
If Calc Embedded mode was active, this deactivates it.
The formula (including its surrounding delimiters) is saved in the kill ring.
The command \\[yank] can retrieve it from there."
  (interactive)
  (and calc-embedded-info
       (calc-embedded nil))
  (calc-embedded-mark-formula)
  (kill-region (point) (mark))
  (pop-mark)
)

(defun calc-embedded-copy-formula-as-kill ()
  "Save the formula surrounding point as if killed, but don't kill it."
  (interactive)
  (save-excursion
    (calc-embedded-mark-formula)
    (copy-region-as-kill (point) (mark))
    (pop-mark))
)

(defun calc-embedded-duplicate ()
  (interactive)
  (let ((already calc-embedded-info)
	top bot outer-top outer-bot new-top)
    (if calc-embedded-info
	(progn
	  (setq top (+ (aref calc-embedded-info 2))
		bot (+ (aref calc-embedded-info 3))
		outer-top (+ (aref calc-embedded-info 4))
		outer-bot (+ (aref calc-embedded-info 5)))
	  (calc-embedded nil))
      (calc-embedded-find-bounds))
    (goto-char outer-bot)
    (insert "\n")
    (setq new-top (point))
    (insert-buffer-substring (current-buffer) outer-top outer-bot)
    (goto-char (+ new-top (- top outer-top)))
    (let ((calc-embedded-quiet (if already t 'x)))
      (calc-embedded (+ new-top (- top outer-top))
		     (+ new-top (- bot outer-top))
		     new-top
		     (+ new-top (- outer-bot outer-top)))))
)

(defun calc-embedded-next (arg)
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (let* ((active (cdr (assq (current-buffer) calc-embedded-active)))
	 (p active)
	 (num (length active)))
    (or active
	(error "No active formulas in buffer"))
    (cond ((= arg 0))
	  ((= arg -1)
	   (if (<= (point) (aref (car active) 3))
	       (goto-char (aref (nth (1- num) active) 2))
	     (while (and (cdr p)
			 (> (point) (aref (nth 1 p) 3)))
	       (setq p (cdr p)))
	     (goto-char (aref (car p) 2))))
	  ((< arg -1)
	   (calc-embedded-next -1)
	   (calc-embedded-next (+ (* num 1000) arg 1)))
	  (t
	   (setq arg (1+ (% (1- arg) num)))
	   (while (and p (>= (point) (aref (car p) 2)))
	     (setq p (cdr p)))
	   (while (> (setq arg (1- arg)) 0)
	     (setq p (if p (cdr p) (cdr active))))
	   (goto-char (aref (car (or p active)) 2)))))
)

(defun calc-embedded-previous (arg)
  (interactive "p")
  (calc-embedded-next (- (prefix-numeric-value arg)))
)

(defun calc-embedded-new-formula ()
  (interactive)
  (and (eq major-mode 'calc-mode)
       (error "This command should be used in a normal editing buffer"))
  (if calc-embedded-info
      (calc-embedded nil))
  (let (top bot outer-top outer-bot)
    (if (and (eq (preceding-char) ?\n)
	     (string-match "\\`\n" calc-embedded-open-new-formula))
	(progn
	  (setq outer-top (1- (point)))
	  (forward-char -1)
	  (insert (substring calc-embedded-open-new-formula 1)))
      (setq outer-top (point))
      (insert calc-embedded-open-new-formula))
    (setq top (point))
    (insert " ")
    (setq bot (point))
    (insert calc-embedded-close-new-formula)
    (if (and (eq (following-char) ?\n)
	     (string-match "\n\\'" calc-embedded-close-new-formula))
	(delete-char 1))
    (setq outer-bot (point))
    (goto-char top)
    (let ((calc-embedded-quiet 'x))
      (calc-embedded top bot outer-top outer-bot)))
)

(defun calc-embedded-forget ()
  (interactive)
  (setq calc-embedded-active (delq (assq (current-buffer) calc-embedded-active)
				   calc-embedded-active))
  (calc-embedded-active-state nil)
)


(defun calc-embedded-set-modes (gmodes modes local-modes &optional temp)
  (let ((the-language (calc-embedded-language))
	(the-display-just (calc-embedded-justify))
	(v gmodes)
	(changed nil)
	found value)
    (while v
      (or (symbolp (car v))
	  (and (setq found (assq (car (car v)) modes))
	       (not (eq (cdr found) 'default)))
	  (and (setq found (assq (car (car v)) local-modes))
	       (not (eq (cdr found) 'default)))
	  (progn
	    (if (eq (setq value (cdr (car v))) 'default)
		(setq value (cdr (assq (car (car v)) calc-mode-var-list))))
	    (equal (symbol-value (car (car v))) value))
	  (progn
	    (setq changed t)
	    (if temp (setq prev-modes (cons (cons (car (car v))
						  (symbol-value (car (car v))))
					    prev-modes)))
	    (set (car (car v)) value)))
      (setq v (cdr v)))
    (setq v modes)
    (while v
      (or (and (setq found (assq (car (car v)) local-modes))
	       (not (eq (cdr found) 'default)))
	  (eq (setq value (cdr (car v))) 'default)
	  (equal (symbol-value (car (car v))) value)
	  (progn
	    (setq changed t)
	    (if temp (setq prev-modes (cons (cons (car (car v))
						  (symbol-value (car (car v))))
					    prev-modes)))
	    (set (car (car v)) value)))
      (setq v (cdr v)))
    (setq v local-modes)
    (while v
      (or (eq (setq value (cdr (car v))) 'default)
	  (equal (symbol-value (car (car v))) value)
	  (progn
	    (setq changed t)
	    (if temp (setq prev-modes (cons (cons (car (car v))
						  (symbol-value (car (car v))))
					    prev-modes)))
	    (set (car (car v)) value)))
      (setq v (cdr v)))
    (and changed (not (eq temp t))
	 (progn
	   (calc-embedded-set-justify the-display-just)
	   (calc-embedded-set-language the-language)))
    (and changed (not temp)
	 (progn
	   (setq calc-full-float-format (list (if (eq (car calc-float-format)
						      'fix)
						  'float
						(car calc-float-format))
					      0))
	   (calc-refresh)))
    changed)
)

(defun calc-embedded-language ()
  (if calc-language-option
      (list calc-language calc-language-option)
    calc-language)
)

(defun calc-embedded-set-language (lang)
  (let ((option nil))
    (if (consp lang)
	(setq option (nth 1 lang)
	      lang (car lang)))
    (or (and (eq lang calc-language)
	     (equal option calc-language-option))
	(calc-set-language lang option t)))
)

(defun calc-embedded-justify ()
  (if calc-display-origin
      (list calc-display-just calc-display-origin)
    calc-display-just)
)

(defun calc-embedded-set-justify (just)
  (if (consp just)
      (setq calc-display-origin (nth 1 just)
	    calc-display-just (car just))
    (setq calc-display-just just
	  calc-display-origin nil))
)


(defun calc-find-globals ()
  (interactive)
  (and (eq major-mode 'calc-mode)
       (error "This command should be used in a normal editing buffer"))
  (make-local-variable 'calc-embedded-globals)
  (let ((case-fold-search nil)
	(modes nil)
	(save-pt (point))
	found value)
    (goto-char (point-min))
    (while (re-search-forward "\\[calc-global-mode: *\\([-a-z]+\\): *\\(\"\\([^\"\n\\]\\|\\\\.\\)*\"\\|[- ()a-zA-Z0-9]+\\)\\]" nil t)
      (and (setq found (assoc (buffer-substring (match-beginning 1)
						(match-end 1))
			      calc-embedded-mode-vars))
	   (or (assq (cdr found) modes)
	       (setq modes (cons (cons (cdr found)
				       (car (read-from-string
					     (buffer-substring
					      (match-beginning 2)
					      (match-end 2)))))
				 modes)))))
    (setq calc-embedded-globals (cons t modes))
    (goto-char save-pt))
)

(defun calc-embedded-find-modes ()
  (let ((case-fold-search nil)
	(save-pt (point))
	(no-defaults t)
	(modes nil)
	(emodes nil)
	(pmodes nil)
	found value)
    (while (and no-defaults (search-backward "[calc-" nil t))
      (forward-char 6)
      (or (and (looking-at "mode: *\\([-a-z]+\\): *\\(\"\\([^\"\n\\]\\|\\\\.\\)*\"\\|[- ()a-zA-Z0-9]+\\)]")
	       (setq found (assoc (buffer-substring (match-beginning 1)
						    (match-end 1))
				  calc-embedded-mode-vars))
	       (or (assq (cdr found) modes)
		   (setq modes (cons (cons (cdr found)
					   (car (read-from-string
						 (buffer-substring
						  (match-beginning 2)
						  (match-end 2)))))
				     modes))))
	  (and (looking-at "perm-mode: *\\([-a-z]+\\): *\\(\"\\([^\"\n\\]\\|\\\\.\\)*\"\\|[- ()a-zA-Z0-9]+\\)]")
	       (setq found (assoc (buffer-substring (match-beginning 1)
						    (match-end 1))
				  calc-embedded-mode-vars))
	       (or (assq (cdr found) pmodes)
		   (setq pmodes (cons (cons (cdr found)
					    (car (read-from-string
						  (buffer-substring
						   (match-beginning 2)
						   (match-end 2)))))
				      pmodes))))
	  (and (looking-at "edit-mode: *\\([-a-z]+\\): *\\(\"\\([^\"\n\\]\\|\\\\.\\)*\"\\|[- ()a-zA-Z0-9]+\\)]")
	       (setq found (assoc (buffer-substring (match-beginning 1)
						    (match-end 1))
				  calc-embedded-mode-vars))
	       (or (assq (cdr found) emodes)
		   (setq emodes (cons (cons (cdr found)
					    (car (read-from-string
						  (buffer-substring
						   (match-beginning 2)
						   (match-end 2)))))
				      emodes))))
	  (and (looking-at "defaults]")
	       (setq no-defaults nil)))
      (backward-char 6))
    (goto-char save-pt)
    (list modes emodes pmodes))
)


(defun calc-embedded-make-info (point cbuf fresh &optional
				      top bot outer-top outer-bot)
  (let* ((bufentry (assq (current-buffer) calc-embedded-active))
	 (found bufentry)
	 (force (and fresh top))
	 (fixed top)
	 (new-info nil)
	 info str)
    (or found
	(setq found (list (current-buffer))
	      calc-embedded-active (cons found calc-embedded-active)))
    (while (and (cdr found)
		(> point (aref (car (cdr found)) 3)))
      (setq found (cdr found)))
    (if (and (cdr found)
	     (>= point (aref (nth 1 found) 2)))
	(setq info (nth 1 found))
      (setq info (make-vector 16 nil)
	    new-info t
	    fresh t)
      (aset info 0 (current-buffer))
      (aset info 1 (or cbuf (save-excursion
			      (calc-create-buffer)
			      (current-buffer)))))
    (if (and (integerp top) (not bot))  ; started with a user-supplied argument
	(progn
	  (if (= (setq arg (prefix-numeric-value arg)) 0)
	      (progn
		(aset info 2 (copy-marker (region-beginning)))
		(aset info 3 (copy-marker (region-end))))
	    (aset info (if (> arg 0) 2 3) (point-marker))
	    (forward-line arg)
	    (aset info (if (> arg 0) 3 2) (point-marker)))
	  (aset info 4 (copy-marker (aref info 2)))
	  (aset info 5 (copy-marker (aref info 3))))
      (if (aref info 4)
	  (setq top (aref info 2)
		fixed top)
	(if (consp top)
	    (let ((calc-embedded-open-formula calc-embedded-open-word)
		  (calc-embedded-close-formula calc-embedded-close-word))
	      (calc-embedded-find-bounds 'plain))
	  (or top
	      (calc-embedded-find-bounds 'plain)))
	(aset info 2 (copy-marker (min top bot)))
	(aset info 3 (copy-marker (max top bot)))
	(aset info 4 (copy-marker (or outer-top (aref info 2))))
	(aset info 5 (copy-marker (or outer-bot (aref info 3))))))
    (goto-char (aref info 2))
    (if new-info
	(progn
	  (or (bolp) (aset info 7 t))
	  (goto-char (aref info 3))
	  (or (bolp) (eolp) (aset info 7 t))))
    (if fresh
	(let ((modes (calc-embedded-find-modes)))
	  (aset info 12 (car modes))
	  (aset info 13 (nth 1 modes))
	  (aset info 14 (nth 2 modes))))
    (aset info 15 calc-embedded-globals)
    (setq str (buffer-substring (aref info 2) (aref info 3)))
    (if (or force
	    (not (equal str (aref info 6))))
	(if (and fixed (aref info 6))
	    (progn
	      (aset info 4 nil)
	      (calc-embedded-make-info point cbuf nil)
	      (setq new-info nil))
	  (let* ((open-plain calc-embedded-open-plain)
		 (close-plain calc-embedded-close-plain)
		 (pref-len (length open-plain))
		 (vars-used nil)
		 suff-pos val temp)
	    (save-excursion
	      (set-buffer (aref info 1))
	      (calc-embedded-set-modes (aref info 15)
				       (aref info 12) (aref info 14))
	      (if (and (> (length str) pref-len)
		       (equal (substring str 0 pref-len) open-plain)
		       (setq suff-pos (string-match (regexp-quote close-plain)
						    str pref-len)))
		  (setq val (math-read-plain-expr
			     (substring str pref-len suff-pos)))
		(if (string-match "[^ \t\n]" str)
		    (setq pref-len 0
			  val (math-read-big-expr str))
		  (setq val nil))))
	    (if (eq (car-safe val) 'error)
		(setq val (list 'error
				(+ (aref info 2) pref-len (nth 1 val))
				(nth 2 val))))
	    (aset info 6 str)
	    (aset info 8 val)
	    (setq temp val)
	    (if (eq (car-safe temp) 'calcFunc-evalto)
		(setq temp (nth 1 temp))
	      (if (eq (car-safe temp) 'error)
		  (if new-info
		      (setq new-info nil)
		    (setcdr found (delq info (cdr found)))
		    (calc-embedded-active-state 'less))))
	    (aset info 9 (and (eq (car-safe temp) 'calcFunc-assign)
			      (nth 1 temp)))
	    (if (memq (car-safe val) '(calcFunc-evalto calcFunc-assign))
		(calc-embedded-find-vars val))
	    (aset info 10 vars-used)
	    (aset info 11 nil))))
    (if new-info
	(progn
	  (setcdr found (cons info (cdr found)))
	  (calc-embedded-active-state 'more)))
    info)
)

(defun calc-embedded-find-vars (x)
  (cond ((Math-primp x)
	 (and (eq (car-safe x) 'var)
	      (not (assoc x vars-used))
	      (setq vars-used (cons (list x) vars-used))))
	((eq (car x) 'calcFunc-evalto)
	 (calc-embedded-find-vars (nth 1 x)))
	((eq (car x) 'calcFunc-assign)
	 (calc-embedded-find-vars (nth 2 x)))
	(t
	 (and (eq (car x) 'calcFunc-subscr)
	      (eq (car-safe (nth 1 x)) 'var)
	      (Math-primp (nth 2 x))
	      (not (assoc x vars-used))
	      (setq vars-used (cons (list x) vars-used)))
	 (while (setq x (cdr x))
	   (calc-embedded-find-vars (car x)))))
)


(defun calc-embedded-evaluate-expr (x)
  (let ((vars-used (aref calc-embedded-info 10)))
    (or vars-used (calc-embedded-find-vars x))
    (if vars-used
	(let ((active (assq (aref calc-embedded-info 0) calc-embedded-active))
	      (args nil))
	  (save-excursion
	    (calc-embedded-original-buffer t)
	    (or active
		(progn
		  (calc-embedded-activate)
		  (setq active (assq (aref calc-embedded-info 0)
				     calc-embedded-active))))
	    (while vars-used
	      (calc-embedded-eval-get-var (car (car vars-used)) active)
	      (setq vars-used (cdr vars-used))))
	  (calc-embedded-subst x))
      (calc-normalize (math-evaluate-expr-rec x))))
)

(defun calc-embedded-subst (x)
  (if (and (eq (car-safe x) 'calcFunc-evalto) (cdr x))
      (let ((rhs (calc-embedded-subst (nth 1 x))))
	(list 'calcFunc-evalto
	      (nth 1 x)
	      (if (eq (car-safe rhs) 'calcFunc-assign) (nth 2 rhs) rhs)))
    (if (and (eq (car-safe x) 'calcFunc-assign) (= (length x) 3))
	(list 'calcFunc-assign
	      (nth 1 x)
	      (calc-embedded-subst (nth 2 x)))
      (calc-normalize (math-evaluate-expr-rec (math-multi-subst-rec x)))))
)

(defun calc-embedded-eval-get-var (var base)
  (let ((entry base)
	(point (aref calc-embedded-info 2))
	(last nil)
	val)
    (while (and (setq entry (cdr entry))
		(or (not (equal var (aref (car entry) 9)))
		    (and (> point (aref (car entry) 3))
			 (setq last entry)))))
    (if last
	(setq entry last))
    (if entry
	(progn
	  (setq entry (car entry))
	  (if (equal (buffer-substring (aref entry 2) (aref entry 3))
		     (aref entry 6))
	      (progn
		(or (aref entry 11)
		    (save-excursion
		      (calc-embedded-update entry 14 t nil)))
		(setq val (aref entry 11))
		(if (eq (car-safe val) 'calcFunc-evalto)
		    (setq val (nth 2 val)))
		(if (eq (car-safe val) 'calcFunc-assign)
		    (setq val (nth 2 val)))
		(setq args (cons (cons var val) args)))
	    (calc-embedded-activate)
	    (calc-embedded-eval-get-var var base)))))
)


(defun calc-embedded-update (info which need-eval need-display
				  &optional str entry old-val)
  (let* ((prev-modes nil)
	 (open-plain calc-embedded-open-plain)
	 (close-plain calc-embedded-close-plain)
	 (vars-used nil)
	 (evalled nil)
	 (val (aref info 8))
	 (old-eval (aref info 11)))
    (or old-val (setq old-val val))
    (if (eq (car-safe val) 'calcFunc-evalto)
	(setq need-display t))
    (unwind-protect
	(progn
	  (set-buffer (aref info 1))
	  (and which
	       (calc-embedded-set-modes (aref info 15) (aref info 12)
					(aref info which)
					(if need-display 'full t)))
	  (if (memq (car-safe val) '(calcFunc-evalto calcFunc-assign))
	      (calc-embedded-find-vars val))
	  (if need-eval
	      (let ((calc-embedded-info info))
		(setq val (math-evaluate-expr val)
		      evalled val)))
	  (if (or (eq need-eval 'eval) (eq (car-safe val) 'calcFunc-evalto))
	      (aset info 8 val))
	  (aset info 9 nil)
	  (aset info 10 vars-used)
	  (aset info 11 nil)
	  (if (or need-display (eq (car-safe val) 'calcFunc-evalto))
	      (let ((extra (if (eq calc-language 'big) 1 0)))
		(or entry (setq entry (list val 1 nil)))
		(or str (progn
			  (setq str (let ((calc-line-numbering nil))
				      (math-format-stack-value entry)))
			  (if (eq calc-language 'big)
			      (setq str (substring str 0 -1)))))
		(and calc-show-plain
		     (setq str (concat open-plain
				       (math-showing-full-precision
					(math-format-flat-expr val 0))
				       close-plain
				       str)))
		(save-excursion
		  (calc-embedded-original-buffer t info)
		  (or (equal str (aref info 6))
		      (let ((delta (- (aref info 5) (aref info 3)))
			    (buffer-read-only nil))
			(goto-char (aref info 2))
			(delete-region (point) (aref info 3))
			(and (> (nth 1 entry) (1+ extra))
			     (aref info 7)
			     (progn
			       (aset info 7 nil)
			       (delete-horizontal-space)
			       (insert "\n\n")
			       (delete-horizontal-space)
			       (backward-char 1)))
			(insert str)
			(set-marker (aref info 3) (point))
			(set-marker (aref info 5) (+ (point) delta))
			(aset info 6 str))))))
	  (if (eq (car-safe val) 'calcFunc-evalto)
	      (progn
		(setq evalled (nth 2 val)
		      val (nth 1 val))))
	  (if (eq (car-safe val) 'calcFunc-assign)
	      (progn
		(aset info 9 (nth 1 val))
		(aset info 11 (or evalled
				  (let ((calc-embedded-info info))
				    (math-evaluate-expr (nth 2 val)))))
		(or (equal old-eval (aref info 11))
		    (calc-embedded-var-change (nth 1 val) (aref info 0))))
	    (if (eq (car-safe old-val) 'calcFunc-evalto)
		(setq old-val (nth 1 old-val)))
	    (if (eq (car-safe old-val) 'calcFunc-assign)
		(calc-embedded-var-change (nth 1 old-val) (aref info 0)))))
      (set-buffer (aref info 1))
      (while prev-modes
	(cond ((eq (car (car prev-modes)) 'the-language)
	       (if need-display
		   (calc-embedded-set-language (cdr (car prev-modes)))))
	      ((eq (car (car prev-modes)) 'the-display-just)
	       (if need-display
		   (calc-embedded-set-justify (cdr (car prev-modes)))))
	      (t
	       (set (car (car prev-modes)) (cdr (car prev-modes)))))
	(setq prev-modes (cdr prev-modes)))))
)




;;; These are hooks called by the main part of Calc.

(defun calc-embedded-select-buffer ()
  (if (eq (current-buffer) (aref calc-embedded-info 0))
      (let ((info calc-embedded-info)
	    horiz vert)
	(if (and (or (< (point) (aref info 4))
		     (> (point) (aref info 5)))
		 (not calc-embedded-no-reselect))
	    (let ((calc-embedded-quiet t))
	      (message "(Switching Calc Embedded mode to new formula.)")
	      (calc-embedded nil)
	      (calc-embedded nil)))
	(setq horiz (max (min (current-column) (- (point) (aref info 2))) 0)
	      vert (if (<= (aref info 2) (point))
		       (- (count-lines (aref info 2) (point))
			  (if (bolp) 0 1))
		     0))
	(set-buffer (aref info 1))
	(if calc-show-plain
	    (if (= vert 0)
		(setq horiz 0)
	      (setq vert (1- vert))))
	(calc-cursor-stack-index 1)
	(if calc-line-numbering
	    (setq horiz (+ horiz 4)))
	(if (> vert 0)
	    (forward-line vert))
	(forward-char (min horiz
			   (- (point-max) (point)))))
    (calc-select-buffer))
)
(setq calc-embedded-no-reselect nil)

(defun calc-embedded-finish-command ()
  (let ((buf (current-buffer))
	horiz vert)
    (save-excursion
      (set-buffer (aref calc-embedded-info 1))
      (if (> (calc-stack-size) 0)
	  (let ((pt (point))
		(col (current-column))
		(bol (bolp)))
	    (calc-cursor-stack-index 0)
	    (if (< pt (point))
		(progn
		  (calc-cursor-stack-index 1)
		  (if (>= pt (point))
		      (progn
			(setq horiz (- col (if calc-line-numbering 4 0))
			      vert (- (count-lines (point) pt)
				      (if bol 0 1)))
			(if calc-show-plain
			    (setq vert (max 1 (1+ vert))))))))
	    (goto-char pt))))
    (if horiz
	(progn
	  (set-buffer (aref calc-embedded-info 0))
	  (goto-char (aref calc-embedded-info 2))
	  (if (> vert 0)
	      (forward-line vert))
	  (forward-char (max horiz 0))
	  (set-buffer buf))))
)

(defun calc-embedded-stack-change ()
  (or calc-executing-macro
      (save-excursion
	(set-buffer (aref calc-embedded-info 1))
	(let* ((info calc-embedded-info)
	       (extra-line (if (eq calc-language 'big) 1 0))
	       (the-point (point))
	       (empty (= (calc-stack-size) 0))
	       (entry (if empty
			  (list '(var empty var-empty) 1 nil)
			(calc-top 1 'entry)))
	       (old-val (aref info 8))
	       top bot str)
	  (if empty
	      (setq str "empty")
	    (save-excursion
	      (calc-cursor-stack-index 1)
	      (setq top (point))
	      (calc-cursor-stack-index 0)
	      (setq bot (- (point) extra-line))
	      (setq str (buffer-substring top (- bot 1))))
	    (if calc-line-numbering
		(let ((pos 0))
		  (setq str (substring str 4))
		  (while (setq pos (string-match "\n...." str pos))
		    (setq str (concat (substring str 0 (1+ pos))
				      (substring str (+ pos 5)))
			  pos (1+ pos))))))
	  (calc-embedded-original-buffer t)
	  (aset info 8 (car entry))
	  (calc-embedded-update info 13 nil t str entry old-val))))
)

(defun calc-embedded-mode-line-change ()
  (let ((str mode-line-buffer-identification))
    (save-excursion
      (calc-embedded-original-buffer t)
      (setq mode-line-buffer-identification str)
      (set-buffer-modified-p (buffer-modified-p))))
)

(defun calc-embedded-modes-change (vars)
  (if (eq (car vars) 'calc-language) (setq vars '(the-language)))
  (if (eq (car vars) 'calc-display-just) (setq vars '(the-display-just)))
  (while (and vars
	      (not (rassq (car vars) calc-embedded-mode-vars)))
    (setq vars (cdr vars)))
  (if (and vars calc-mode-save-mode (not (eq calc-mode-save-mode 'save)))
      (save-excursion
	(let* ((save-mode calc-mode-save-mode)
	       (header (if (eq save-mode 'local)
			   "calc-mode:"
			 (format "calc-%s-mode:" save-mode)))
	       (the-language (calc-embedded-language))
	       (the-display-just (calc-embedded-justify))
	       (values (mapcar 'symbol-value vars))
	       (num (cond ((eq save-mode 'local) 12)
			  ((eq save-mode 'edit) 13)
			  ((eq save-mode 'perm) 14)
			  (t nil)))
	       base limit mname mlist)
	  (calc-embedded-original-buffer t)
	  (save-excursion
	    (if (eq save-mode 'global)
		(setq base (point-max)
		      limit (point-min)
		      mlist calc-embedded-globals)
	      (goto-char (aref calc-embedded-info 4))
	      (beginning-of-line)
	      (setq base (point)
		    limit (max (- (point) 1000) (point-min))
		    mlist (and num (aref calc-embedded-info num)))
	      (and (re-search-backward
		    (format "\\(%s\\)[^\001]*\\(%s\\)\\|\\[calc-defaults]"
			    calc-embedded-open-formula
			    calc-embedded-close-formula) limit t)
		   (setq limit (point))))
	    (while vars
	      (goto-char base)
	      (if (setq mname (car (rassq (car vars)
					  calc-embedded-mode-vars)))
		  (let ((buffer-read-only nil)
			(found (assq (car vars) mlist)))
		    (if found
			(setcdr found (car values))
		      (setq mlist (cons (cons (car vars) (car values)) mlist))
		      (if num
			  (aset calc-embedded-info num mlist)
			(if (eq save-mode 'global)
			    (setq calc-embedded-globals mlist))))
		    (if (re-search-backward
			 (format "\\[%s *%s: *\\(\"\\([^\"\n\\]\\|\\\\.\\)*\"\\|[- ()a-zA-Z0-9]+\\)]"
				 header mname)
			 limit t)
			(progn
			  (goto-char (match-beginning 1))
			  (delete-region (point) (match-end 1))
			  (insert (prin1-to-string (car values))))
		      (goto-char base)
		      (insert-before-markers
		       calc-embedded-open-mode
		       "[" header " " mname ": "
		       (prin1-to-string (car values)) "]"
		       calc-embedded-close-mode))))
	      (setq vars (cdr vars)
		    values (cdr values)))))))
)

(defun calc-embedded-var-change (var &optional buf)
  (if (symbolp var)
      (setq var (list 'var
		      (if (string-match "\\`var-.+\\'"
					(symbol-name var))
			  (intern (substring (symbol-name var) 4))
			var)
		      var)))
  (save-excursion
    (let ((manual (not calc-auto-recompute))
	  (bp calc-embedded-active)
	  (first t))
      (if buf (setq bp (memq (assq buf bp) bp)))
      (while bp
	(let ((calc-embedded-no-reselect t)
	      (p (and (buffer-name (car (car bp)))
		      (cdr (car bp)))))
	  (while p
	    (if (assoc var (aref (car p) 10))
		(if manual
		    (if (aref (car p) 11)
			(progn
			  (aset (car p) 11 nil)
			  (if (aref (car p) 9)
			      (calc-embedded-var-change (aref (car p) 9)))))
		  (set-buffer (aref (car p) 0))
		  (if (equal (buffer-substring (aref (car p) 2)
					       (aref (car p) 3))
			     (aref (car p) 6))
		      (let ((calc-embedded-info nil))
			(or calc-embedded-quiet
			    (message "Recomputing..."))
			(setq first nil)
			(calc-wrapper
			 (set-buffer (aref (car p) 0))
			 (calc-embedded-update (car p) 14 t nil)))
		    (setcdr (car bp) (delq (car p) (cdr (car bp))))
		    (message
		     "(Tried to recompute but formula was changed or missing.)"))))
	    (setq p (cdr p))))
	(setq bp (if buf nil (cdr bp))))
      (or first calc-embedded-quiet (message ""))))
)





