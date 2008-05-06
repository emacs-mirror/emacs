;;; gnus-salt.el --- alternate summary mode interfaces for Gnus

;; Copyright (C) 1996, 1997, 1998, 1999, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-sum)
(require 'gnus-win)

;;;
;;; gnus-pick-mode
;;;

(defvar gnus-pick-mode nil
  "Minor mode for providing a pick-and-read interface in Gnus
summary buffers.")

(defcustom gnus-pick-display-summary nil
  "*Display summary while reading."
  :type 'boolean
  :group 'gnus-summary-pick)

(defcustom gnus-pick-mode-hook nil
  "Hook run in summary pick mode buffers."
  :type 'hook
  :group 'gnus-summary-pick)

(when (featurep 'xemacs)
  (add-hook 'gnus-pick-mode-hook 'gnus-xmas-pick-menu-add))

(defcustom gnus-mark-unpicked-articles-as-read nil
  "*If non-nil, mark all unpicked articles as read."
  :type 'boolean
  :group 'gnus-summary-pick)

(defcustom gnus-pick-elegant-flow t
  "If non-nil, `gnus-pick-start-reading' runs
 `gnus-summary-next-group' when no articles have been picked."
  :type 'boolean
  :group 'gnus-summary-pick)

(defcustom gnus-summary-pick-line-format
  "%-5P %U\%R\%z\%I\%(%[%4L: %-23,23n%]%) %s\n"
  "*The format specification of the lines in pick buffers.
It accepts the same format specs that `gnus-summary-line-format' does."
  :type 'string
  :group 'gnus-summary-pick)

;;; Internal variables.

(defvar gnus-pick-mode-map nil)

(unless gnus-pick-mode-map
  (setq gnus-pick-mode-map (make-sparse-keymap))

  (gnus-define-keys gnus-pick-mode-map
    " " gnus-pick-next-page
    "u" gnus-pick-unmark-article-or-thread
    "." gnus-pick-article-or-thread
    gnus-down-mouse-2 gnus-pick-mouse-pick-region
    "\r" gnus-pick-start-reading))

(defun gnus-pick-make-menu-bar ()
  (unless (boundp 'gnus-pick-menu)
    (easy-menu-define
      gnus-pick-menu gnus-pick-mode-map ""
      '("Pick"
	("Pick"
	 ["Article" gnus-summary-mark-as-processable t]
	 ["Thread" gnus-uu-mark-thread t]
	 ["Region" gnus-uu-mark-region t]
	 ["Regexp" gnus-uu-mark-by-regexp t]
	 ["Buffer" gnus-uu-mark-buffer t])
	("Unpick"
	 ["Article" gnus-summary-unmark-as-processable t]
	 ["Thread" gnus-uu-unmark-thread t]
	 ["Region" gnus-uu-unmark-region t]
	 ["Regexp" gnus-uu-unmark-by-regexp t]
	 ["Buffer" gnus-summary-unmark-all-processable t])
	["Start reading" gnus-pick-start-reading t]
	["Switch pick mode off" gnus-pick-mode gnus-pick-mode]))))

(defun gnus-pick-mode (&optional arg)
  "Minor mode for providing a pick-and-read interface in Gnus summary buffers.

\\{gnus-pick-mode-map}"
  (interactive "P")
  (when (eq major-mode 'gnus-summary-mode)
    (if (not (set (make-local-variable 'gnus-pick-mode)
		  (if (null arg) (not gnus-pick-mode)
		    (> (prefix-numeric-value arg) 0))))
	(remove-hook 'gnus-message-setup-hook 'gnus-pick-setup-message)
      ;; Make sure that we don't select any articles upon group entry.
      (set (make-local-variable 'gnus-auto-select-first) nil)
      ;; Change line format.
      (setq gnus-summary-line-format gnus-summary-pick-line-format)
      (setq gnus-summary-line-format-spec nil)
      (gnus-update-format-specifications nil 'summary)
      (gnus-update-summary-mark-positions)
      (add-hook 'gnus-message-setup-hook 'gnus-pick-setup-message)
      (set (make-local-variable 'gnus-summary-goto-unread) 'never)
      ;; Set up the menu.
      (when (gnus-visual-p 'pick-menu 'menu)
	(gnus-pick-make-menu-bar))
      (add-minor-mode 'gnus-pick-mode " Pick" gnus-pick-mode-map)
      (gnus-run-hooks 'gnus-pick-mode-hook))))

(defun gnus-pick-setup-message ()
  "Make Message do the right thing on exit."
  (when (and (gnus-buffer-live-p gnus-summary-buffer)
	     (with-current-buffer gnus-summary-buffer
	       gnus-pick-mode))
    (message-add-action
     `(gnus-configure-windows ,gnus-current-window-configuration t)
     'send 'exit 'postpone 'kill)))

(defvar gnus-pick-line-number 1)
(defun gnus-pick-line-number ()
  "Return the current line number."
  (if (bobp)
      (setq gnus-pick-line-number 1)
    (incf gnus-pick-line-number)))

(defun gnus-pick-start-reading (&optional catch-up)
  "Start reading the picked articles.
If given a prefix, mark all unpicked articles as read."
  (interactive "P")
  (if gnus-newsgroup-processable
      (progn
	(gnus-summary-limit-to-articles nil)
	(when (or catch-up gnus-mark-unpicked-articles-as-read)
	  (gnus-summary-limit-mark-excluded-as-read))
	(gnus-summary-first-article)
	(gnus-configure-windows
	 (if gnus-pick-display-summary 'article 'pick) t))
    (if gnus-pick-elegant-flow
	(progn
	  (when (or catch-up gnus-mark-unpicked-articles-as-read)
	    (gnus-summary-catchup nil t))
	  (if (gnus-group-quit-config gnus-newsgroup-name)
	      (gnus-summary-exit)
	    (gnus-summary-next-group)))
      (error "No articles have been picked"))))

(defun gnus-pick-goto-article (arg)
  "Go to the article number indicated by ARG.
If ARG is an invalid article number, then stay on current line."
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      (when (zerop (forward-line (1- (prefix-numeric-value arg))))
	(setq pos (point))))
    (if (not pos)
	(gnus-error 2 "No such line: %s" arg)
      (goto-char pos))))

(defun gnus-pick-article (&optional arg)
  "Pick the article on the current line.
If ARG, pick the article on that line instead."
  (interactive "P")
  (when arg
    (gnus-pick-goto-article arg))
  (gnus-summary-mark-as-processable 1))

(defun gnus-pick-article-or-thread (&optional arg)
  "If `gnus-thread-hide-subtree' is t, then pick the thread on the current line.
Otherwise pick the article on the current line.
If ARG, pick the article/thread on that line instead."
  (interactive "P")
  (when arg
    (gnus-pick-goto-article arg))
  (if gnus-thread-hide-subtree
      (progn
	(save-excursion
	  (gnus-uu-mark-thread))
	(forward-line 1))
    (gnus-summary-mark-as-processable 1)))

(defun gnus-pick-unmark-article-or-thread (&optional arg)
  "If `gnus-thread-hide-subtree' is t, then unmark the thread on current line.
Otherwise unmark the article on current line.
If ARG, unmark thread/article on that line instead."
  (interactive "P")
  (when arg
    (gnus-pick-goto-article arg))
  (if gnus-thread-hide-subtree
      (save-excursion
	(gnus-uu-unmark-thread))
    (gnus-summary-unmark-as-processable 1)))

(defun gnus-pick-mouse-pick (e)
  (interactive "e")
  (mouse-set-point e)
  (save-excursion
    (gnus-summary-mark-as-processable 1)))

(defun gnus-pick-mouse-pick-region (start-event)
  "Pick articles that the mouse is dragged over.
This must be bound to a button-down mouse event."
  (interactive "e")
  (mouse-minibuffer-check start-event)
  (let* ((echo-keystrokes 0)
	 (start-posn (event-start start-event))
	 (start-point (posn-point start-posn))
         (start-line (1+ (count-lines (point-min) start-point)))
	 (start-window (posn-window start-posn))
	 (bounds (gnus-window-edges start-window))
	 (top (nth 1 bounds))
	 (bottom (if (window-minibuffer-p start-window)
		     (nth 3 bounds)
		   ;; Don't count the mode line.
		   (1- (nth 3 bounds))))
	 (click-count (1- (event-click-count start-event))))
    (setq mouse-selection-click-count click-count)
    (setq mouse-selection-click-count-buffer (current-buffer))
    (mouse-set-point start-event)
   ;; In case the down click is in the middle of some intangible text,
    ;; use the end of that text, and put it in START-POINT.
    (when (< (point) start-point)
      (goto-char start-point))
    (gnus-pick-article)
    (setq start-point (point))
    ;; end-of-range is used only in the single-click case.
    ;; It is the place where the drag has reached so far
    ;; (but not outside the window where the drag started).
    (let (event end end-point (end-of-range (point)))
      (track-mouse
	(while (progn
		 (setq event (cdr (gnus-read-event-char)))
		 (or (mouse-movement-p event)
		     (eq (car-safe event) 'switch-frame)))
	  (if (eq (car-safe event) 'switch-frame)
	      nil
	    (setq end (event-end event)
		  end-point (posn-point end))

	    (cond
	     ;; Are we moving within the original window?
	     ((and (eq (posn-window end) start-window)
		   (integer-or-marker-p end-point))
	      ;; Go to START-POINT first, so that when we move to END-POINT,
	      ;; if it's in the middle of intangible text,
	      ;; point jumps in the direction away from START-POINT.
	      (goto-char start-point)
	      (goto-char end-point)
	      (gnus-pick-article)
	      ;; In case the user moved his mouse really fast, pick
	      ;; articles on the line between this one and the last one.
	      (let* ((this-line (1+ (count-lines (point-min) end-point)))
		     (min-line (min this-line start-line))
		     (max-line (max this-line start-line)))
		;; Why not use `forward-line'?  --Stef
		(while (< min-line max-line)
		  (goto-line min-line)
		  (gnus-pick-article)
		  (setq min-line (1+ min-line)))
		(setq start-line this-line))
	      (when (zerop (% click-count 3))
		(setq end-of-range (point))))
	     (t
	      (let ((mouse-row (cdr (cdr (mouse-position)))))
		(cond
		 ((null mouse-row))
		 ((< mouse-row top)
		  (mouse-scroll-subr start-window (- mouse-row top)))
		 ((>= mouse-row bottom)
		  (mouse-scroll-subr start-window
				     (1+ (- mouse-row bottom)))))))))))
      (when (consp event)
	(let ((fun (key-binding (vector (car event)))))
	  ;; Run the binding of the terminating up-event, if possible.
       ;; In the case of a multiple click, it gives the wrong results,
	  ;; because it would fail to set up a region.
	  (when nil
      ;; (and (= (mod mouse-selection-click-count 3) 0) (fboundp fun))
       ;; In this case, we can just let the up-event execute normally.
	    (let ((end (event-end event)))
	      ;; Set the position in the event before we replay it,
	      ;; because otherwise it may have a position in the wrong
	      ;; buffer.
	      (setcar (cdr end) end-of-range)
	      ;; Delete the overlay before calling the function,
	     ;; because delete-overlay increases buffer-modified-tick.
	      (push event unread-command-events))))))))

(defun gnus-pick-next-page ()
  "Go to the next page.  If at the end of the buffer, start reading articles."
  (interactive)
  (let ((scroll-in-place nil))
    (condition-case nil
	(scroll-up)
      (end-of-buffer (gnus-pick-start-reading)))))

;;;
;;; gnus-binary-mode
;;;

(defvar gnus-binary-mode nil
  "Minor mode for providing a binary group interface in Gnus summary buffers.")

(defvar gnus-binary-mode-hook nil
  "Hook run in summary binary mode buffers.")

(defvar gnus-binary-mode-map nil)

(unless gnus-binary-mode-map
  (setq gnus-binary-mode-map (make-sparse-keymap))

  (gnus-define-keys
      gnus-binary-mode-map
    "g" gnus-binary-show-article))

(defun gnus-binary-make-menu-bar ()
  (unless (boundp 'gnus-binary-menu)
    (easy-menu-define
      gnus-binary-menu gnus-binary-mode-map ""
      '("Pick"
	["Switch binary mode off" gnus-binary-mode t]))))

(defun gnus-binary-mode (&optional arg)
  "Minor mode for providing a binary group interface in Gnus summary buffers."
  (interactive "P")
  (when (eq major-mode 'gnus-summary-mode)
    (make-local-variable 'gnus-binary-mode)
    (setq gnus-binary-mode
	  (if (null arg) (not gnus-binary-mode)
	    (> (prefix-numeric-value arg) 0)))
    (when gnus-binary-mode
      ;; Make sure that we don't select any articles upon group entry.
      (make-local-variable 'gnus-auto-select-first)
      (setq gnus-auto-select-first nil)
      (make-local-variable 'gnus-summary-display-article-function)
      (setq gnus-summary-display-article-function 'gnus-binary-display-article)
      ;; Set up the menu.
      (when (gnus-visual-p 'binary-menu 'menu)
	(gnus-binary-make-menu-bar))
      (add-minor-mode 'gnus-binary-mode " Binary" gnus-binary-mode-map)
      (gnus-run-hooks 'gnus-binary-mode-hook))))

(defun gnus-binary-display-article (article &optional all-header)
  "Run ARTICLE through the binary decode functions."
  (when (gnus-summary-goto-subject article)
    (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
      (gnus-uu-decode-uu))))

(defun gnus-binary-show-article (&optional arg)
  "Bypass the binary functions and show the article."
  (interactive "P")
  (let (gnus-summary-display-article-function)
    (gnus-summary-show-article arg)))

;;;
;;; gnus-tree-mode
;;;

(defcustom gnus-tree-line-format "%(%[%3,3n%]%)"
  "Format of tree elements."
  :type 'string
  :group 'gnus-summary-tree)

(defcustom gnus-tree-minimize-window t
  "If non-nil, minimize the tree buffer window.
If a number, never let the tree buffer grow taller than that number of
lines."
  :type '(choice boolean
		 integer)
  :group 'gnus-summary-tree)

(defcustom gnus-selected-tree-face 'modeline
  "*Face used for highlighting selected articles in the thread tree."
  :type 'face
  :group 'gnus-summary-tree)

(defvar gnus-tree-brackets '((?\[ . ?\]) (?\( . ?\))
			     (?\{ . ?\}) (?< . ?>))
  "Brackets used in tree nodes.")

(defvar gnus-tree-parent-child-edges '(?- ?\\ ?|)
  "Characters used to connect parents with children.")

(defcustom gnus-tree-mode-line-format "Gnus: %%b %S %Z"
  "*The format specification for the tree mode line."
  :type 'string
  :group 'gnus-summary-tree)

(defcustom gnus-generate-tree-function 'gnus-generate-vertical-tree
  "*Function for generating a thread tree.
Two predefined functions are available:
`gnus-generate-horizontal-tree' and `gnus-generate-vertical-tree'."
  :type '(radio (function-item gnus-generate-vertical-tree)
		(function-item gnus-generate-horizontal-tree)
		(function :tag "Other" nil))
  :group 'gnus-summary-tree)

(defcustom gnus-tree-mode-hook nil
  "*Hook run in tree mode buffers."
  :type 'hook
  :group 'gnus-summary-tree)

(when (featurep 'xemacs)
  (add-hook 'gnus-tree-mode-hook 'gnus-xmas-tree-menu-add)
  (add-hook 'gnus-tree-mode-hook 'gnus-xmas-switch-horizontal-scrollbar-off))


;;; Internal variables.

(defvar gnus-tree-line-format-alist
  `((?n gnus-tmp-name ?s)
    (?f gnus-tmp-from ?s)
    (?N gnus-tmp-number ?d)
    (?\[ gnus-tmp-open-bracket ?c)
    (?\] gnus-tmp-close-bracket ?c)
    (?s gnus-tmp-subject ?s)))

(defvar gnus-tree-mode-line-format-alist gnus-summary-mode-line-format-alist)

(defvar gnus-tree-mode-line-format-spec nil)
(defvar gnus-tree-line-format-spec nil)

(defvar gnus-tree-node-length nil)
(defvar gnus-selected-tree-overlay nil)

(defvar gnus-tree-displayed-thread nil)
(defvar gnus-tree-inhibit nil)

(defvar gnus-tree-mode-map nil)
(put 'gnus-tree-mode 'mode-class 'special)

(unless gnus-tree-mode-map
  (setq gnus-tree-mode-map (make-keymap))
  (suppress-keymap gnus-tree-mode-map)
  (gnus-define-keys
      gnus-tree-mode-map
    "\r" gnus-tree-select-article
    gnus-mouse-2 gnus-tree-pick-article
    "\C-?" gnus-tree-read-summary-keys
    "h" gnus-tree-show-summary

    "\C-c\C-i" gnus-info-find-node)

  (substitute-key-definition
   'undefined 'gnus-tree-read-summary-keys gnus-tree-mode-map))

(defun gnus-tree-make-menu-bar ()
  (unless (boundp 'gnus-tree-menu)
    (easy-menu-define
      gnus-tree-menu gnus-tree-mode-map ""
      '("Tree"
	["Select article" gnus-tree-select-article t]))))

(defun gnus-tree-mode ()
  "Major mode for displaying thread trees."
  (interactive)
  (gnus-set-format 'tree-mode)
  (gnus-set-format 'tree t)
  (when (gnus-visual-p 'tree-menu 'menu)
    (gnus-tree-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq mode-name "Tree")
  (setq major-mode 'gnus-tree-mode)
  (use-local-map gnus-tree-mode-map)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (save-excursion
    (gnus-set-work-buffer)
    (gnus-tree-node-insert (make-mail-header "") nil)
    (setq gnus-tree-node-length (1- (point))))
  (gnus-run-mode-hooks 'gnus-tree-mode-hook))

(defun gnus-tree-read-summary-keys (&optional arg)
  "Read a summary buffer key sequence and execute it."
  (interactive "P")
  (unless gnus-tree-inhibit
    (let ((buf (current-buffer))
	  (gnus-tree-inhibit t)
	  win)
      (set-buffer gnus-article-buffer)
      (gnus-article-read-summary-keys arg nil t)
      (when (setq win (get-buffer-window buf))
	(select-window win)
	(when gnus-selected-tree-overlay
	  (goto-char (or (gnus-overlay-end gnus-selected-tree-overlay) 1)))
	(gnus-tree-minimize)))))

(defun gnus-tree-show-summary ()
  "Reconfigure windows to show summary buffer."
  (interactive)
  (if (not (gnus-buffer-live-p gnus-summary-buffer))
      (error "There is no summary buffer for this tree buffer")
    (gnus-configure-windows 'article)
    (gnus-summary-goto-subject gnus-current-article)))

(defun gnus-tree-select-article (article)
  "Select the article under point, if any."
  (interactive (list (gnus-tree-article-number)))
  (let ((buf (current-buffer)))
    (when article
      (with-current-buffer gnus-summary-buffer
	(gnus-summary-goto-article article))
      (select-window (get-buffer-window buf)))))

(defun gnus-tree-pick-article (e)
  "Select the article under the mouse pointer."
  (interactive "e")
  (mouse-set-point e)
  (gnus-tree-select-article (gnus-tree-article-number)))

(defun gnus-tree-article-number ()
  (get-text-property (point) 'gnus-number))

(defun gnus-tree-article-region (article)
  "Return a cons with BEG and END of the article region."
  (let ((pos (text-property-any
	      (point-min) (point-max) 'gnus-number article)))
    (when pos
      (cons pos (next-single-property-change pos 'gnus-number)))))

(defun gnus-tree-goto-article (article)
  (let ((pos (text-property-any
	      (point-min) (point-max) 'gnus-number article)))
    (when pos
      (goto-char pos))))

(defun gnus-tree-recenter ()
  "Center point in the tree window."
  (let ((selected (selected-window))
	(tree-window (gnus-get-buffer-window gnus-tree-buffer t)))
    (when tree-window
      (select-window tree-window)
      (when gnus-selected-tree-overlay
	(goto-char (or (gnus-overlay-end gnus-selected-tree-overlay) 1)))
      (let* ((top (cond ((< (window-height) 4) 0)
			((< (window-height) 7) 1)
			(t 2)))
	     (height (1- (window-height)))
	     (bottom (save-excursion (goto-char (point-max))
				     (forward-line (- height))
				     (point))))
      ;; Set the window start to either `bottom', which is the biggest
	;; possible valid number, or the second line from the top,
	;; whichever is the least.
	(set-window-start
	 tree-window (min bottom (save-excursion
				   (forward-line (- top)) (point)))))
      (select-window selected))))

(defun gnus-get-tree-buffer ()
  "Return the tree buffer properly initialized."
  (with-current-buffer (gnus-get-buffer-create gnus-tree-buffer)
    (unless (eq major-mode 'gnus-tree-mode)
      (gnus-tree-mode))
    (current-buffer)))

(defun gnus-tree-minimize ()
  (when (and gnus-tree-minimize-window
	     (not (one-window-p)))
    (let ((windows 0)
	  tot-win-height)
      (walk-windows (lambda (window) (incf windows)))
      (setq tot-win-height
	    (- (frame-height)
	       (* window-min-height (1- windows))
	       2))
      (let* ((window-min-height 2)
	     (height (count-lines (point-min) (point-max)))
	     (min (max (1- window-min-height) height))
	     (tot (if (numberp gnus-tree-minimize-window)
		      (min gnus-tree-minimize-window min)
		    min))
	     (win (get-buffer-window (current-buffer)))
	     (wh (and win (1- (window-height win)))))
	(setq tot (min tot tot-win-height))
	(when (and win
		   (not (eq tot wh)))
	  (let ((selected (selected-window)))
	    (when (ignore-errors (select-window win))
	      (enlarge-window (- tot wh))
	      (select-window selected))))))))

;;; Generating the tree.

(defun gnus-tree-node-insert (header sparse &optional adopted)
  (let* ((dummy (stringp header))
	 (header (if (vectorp header) header
		   (progn
		     (setq header (make-mail-header "*****"))
		     (mail-header-set-number header 0)
		     (mail-header-set-lines header 0)
		     (mail-header-set-chars header 0)
		     header)))
	 (gnus-tmp-from (mail-header-from header))
	 (gnus-tmp-subject (mail-header-subject header))
	 (gnus-tmp-number (mail-header-number header))
	 (gnus-tmp-name
	  (cond
	   ((string-match "(.+)" gnus-tmp-from)
	    (substring gnus-tmp-from
		       (1+ (match-beginning 0)) (1- (match-end 0))))
	   ((string-match "<[^>]+> *$" gnus-tmp-from)
	    (let ((beg (match-beginning 0)))
	      (or (and (string-match "^\"[^\"]*\"" gnus-tmp-from)
		       (substring gnus-tmp-from (1+ (match-beginning 0))
				  (1- (match-end 0))))
		  (substring gnus-tmp-from 0 beg))))
	   ((memq gnus-tmp-number sparse)
	    "***")
	   (t gnus-tmp-from)))
	 (gnus-tmp-open-bracket
	  (cond ((memq gnus-tmp-number sparse)
		 (caadr gnus-tree-brackets))
		(dummy (caaddr gnus-tree-brackets))
		(adopted (car (nth 3 gnus-tree-brackets)))
		(t (caar gnus-tree-brackets))))
	 (gnus-tmp-close-bracket
	  (cond ((memq gnus-tmp-number sparse)
		 (cdadr gnus-tree-brackets))
		(adopted (cdr (nth 3 gnus-tree-brackets)))
		(dummy
		 (cdaddr gnus-tree-brackets))
		(t (cdar gnus-tree-brackets))))
	 (buffer-read-only nil)
	 beg end)
    (gnus-add-text-properties
     (setq beg (point))
     (setq end (progn (eval gnus-tree-line-format-spec) (point)))
     (list 'gnus-number gnus-tmp-number))
    (when (or t (gnus-visual-p 'tree-highlight 'highlight))
      (gnus-tree-highlight-node gnus-tmp-number beg end))))

(defun gnus-tree-highlight-node (article beg end)
  "Highlight current line according to `gnus-summary-highlight'."
  (let ((list gnus-summary-highlight)
	face)
    (with-current-buffer gnus-summary-buffer
      (let* ((score (or (cdr (assq article gnus-newsgroup-scored))
			gnus-summary-default-score 0))
	     (default gnus-summary-default-score)
	     (default-high gnus-summary-default-high-score)
	     (default-low gnus-summary-default-low-score)
             (uncached (memq article gnus-newsgroup-undownloaded))
             (downloaded (not uncached))
	     (mark (or (gnus-summary-article-mark article) gnus-unread-mark)))
	;; Eval the cars of the lists until we find a match.
	(while (and list
		    (not (eval (caar list))))
	  (setq list (cdr list)))))
    (unless (eq (setq face (cdar list)) (get-text-property beg 'face))
      (gnus-put-text-property-excluding-characters-with-faces
       beg end 'face
       (if (boundp face) (symbol-value face) face)))))

(defun gnus-tree-indent (level)
  (insert (make-string (1- (* (1+ gnus-tree-node-length) level)) ? )))

(defvar gnus-tmp-limit)
(defvar gnus-tmp-sparse)
(defvar gnus-tmp-indent)

(defun gnus-generate-tree (thread)
  "Generate a thread tree for THREAD."
  (with-current-buffer (gnus-get-tree-buffer)
    (let ((buffer-read-only nil)
	  (gnus-tmp-indent 0))
      (erase-buffer)
      (funcall gnus-generate-tree-function thread 0)
      (gnus-set-mode-line 'tree)
      (goto-char (point-min))
      (gnus-tree-minimize)
      (gnus-tree-recenter)
      (let ((selected (selected-window)))
	(when (gnus-get-buffer-window (set-buffer gnus-tree-buffer) t)
	  (select-window (gnus-get-buffer-window (set-buffer gnus-tree-buffer) t))
	  (gnus-horizontal-recenter)
	  (select-window selected))))))

(defun gnus-generate-horizontal-tree (thread level &optional dummyp adopted)
  "Generate a horizontal tree."
  (let* ((dummy (stringp (car thread)))
	 (do (or dummy
		 (and (car thread)
		      (memq (mail-header-number (car thread))
			    gnus-tmp-limit))))
	 col beg)
    (if (not do)
	;; We don't want this article.
	(setq thread (cdr thread))
      (if (not (bolp))
	  ;; Not the first article on the line, so we insert a "-".
	  (insert (car gnus-tree-parent-child-edges))
	;; If the level isn't zero, then we insert some indentation.
	(unless (zerop level)
	  (gnus-tree-indent level)
	  (insert (cadr gnus-tree-parent-child-edges))
	  (setq col (- (setq beg (point)) (point-at-bol) 1))
	  ;; Draw "|" lines upwards.
	  (while (progn
		   (forward-line -1)
		   (forward-char col)
		   (eq (char-after) ? ))
	    (delete-char 1)
	    (insert (caddr gnus-tree-parent-child-edges)))
	  (goto-char beg)))
      (setq dummyp nil)
      ;; Insert the article node.
      (gnus-tree-node-insert (pop thread) gnus-tmp-sparse adopted))
    (if (null thread)
	;; End of the thread, so we go to the next line.
	(unless (bolp)
	  (insert "\n"))
      ;; Recurse downwards in all children of this article.
      (while thread
	(gnus-generate-horizontal-tree
	 (pop thread) (if do (1+ level) level)
	 (or dummyp dummy) dummy)))))

(defsubst gnus-tree-indent-vertical ()
  (let ((len (- (* (1+ gnus-tree-node-length) gnus-tmp-indent)
		(- (point) (point-at-bol)))))
    (when (> len 0)
      (insert (make-string len ? )))))

(defsubst gnus-tree-forward-line (n)
  (while (>= (decf n) 0)
    (unless (zerop (forward-line 1))
      (end-of-line)
      (insert "\n")))
  (end-of-line))

(defun gnus-generate-vertical-tree (thread level &optional dummyp adopted)
  "Generate a vertical tree."
  (let* ((dummy (stringp (car thread)))
	 (do (or dummy
		 (and (car thread)
		      (memq (mail-header-number (car thread))
			    gnus-tmp-limit))))
	 beg)
    (if (not do)
	;; We don't want this article.
	(setq thread (cdr thread))
      (if (not (save-excursion (beginning-of-line) (bobp)))
	  ;; Not the first article on the line, so we insert a "-".
	  (progn
	    (gnus-tree-indent-vertical)
	    (insert (make-string (/ gnus-tree-node-length 2) ? ))
	    (insert (caddr gnus-tree-parent-child-edges))
	    (gnus-tree-forward-line 1))
	;; If the level isn't zero, then we insert some indentation.
	(unless (zerop gnus-tmp-indent)
	  (gnus-tree-forward-line (1- (* 2 level)))
	  (gnus-tree-indent-vertical)
	  (delete-char -1)
	  (insert (cadr gnus-tree-parent-child-edges))
	  (setq beg (point))
	  (forward-char -1)
	  ;; Draw "-" lines leftwards.
	  (while (and (not (bobp))
		      (eq (char-after (1- (point))) ? ))
	    (delete-char -1)
	    (insert (car gnus-tree-parent-child-edges))
	    (forward-char -1))
	  (goto-char beg)
	  (gnus-tree-forward-line 1)))
      (setq dummyp nil)
      ;; Insert the article node.
      (gnus-tree-indent-vertical)
      (gnus-tree-node-insert (pop thread) gnus-tmp-sparse adopted)
      (gnus-tree-forward-line 1))
    (if (null thread)
	;; End of the thread, so we go to the next line.
	(progn
	  (goto-char (point-min))
	  (end-of-line)
	  (incf gnus-tmp-indent))
      ;; Recurse downwards in all children of this article.
      (while thread
	(gnus-generate-vertical-tree
	 (pop thread) (if do (1+ level) level)
	 (or dummyp dummy) dummy)))))

;;; Interface functions.

(defun gnus-possibly-generate-tree (article &optional force)
  "Generate the thread tree for ARTICLE if it isn't displayed already."
  (when (with-current-buffer gnus-summary-buffer
	  (and gnus-use-trees
	       gnus-show-threads
	       (vectorp (gnus-summary-article-header article))))
    (save-excursion
      (let ((top (with-current-buffer gnus-summary-buffer
		   (gnus-cut-thread
		    (gnus-remove-thread
		     (mail-header-id
		      (gnus-summary-article-header article))
		     t))))
	    (gnus-tmp-limit gnus-newsgroup-limit)
	    (gnus-tmp-sparse gnus-newsgroup-sparse))
	(when (or force
		  (not (eq top gnus-tree-displayed-thread)))
	  (gnus-generate-tree top)
	  (setq gnus-tree-displayed-thread top))))))

(defun gnus-tree-open (group)
  (gnus-get-tree-buffer))

(defun gnus-tree-close (group)
  (gnus-kill-buffer gnus-tree-buffer))

(defun gnus-tree-perhaps-minimize ()
  (when (and gnus-tree-minimize-window
	     (get-buffer gnus-tree-buffer))
    (with-current-buffer gnus-tree-buffer
      (gnus-tree-minimize))))

(defun gnus-highlight-selected-tree (article)
  "Highlight the selected article in the tree."
  (let ((buf (current-buffer))
	region)
    (set-buffer gnus-tree-buffer)
    (when (setq region (gnus-tree-article-region article))
      (when (or (not gnus-selected-tree-overlay)
		(gnus-extent-detached-p gnus-selected-tree-overlay))
	;; Create a new overlay.
	(gnus-overlay-put
	 (setq gnus-selected-tree-overlay
	       (gnus-make-overlay (point-min) (1+ (point-min))))
	 'face gnus-selected-tree-face))
      ;; Move the overlay to the article.
      (gnus-move-overlay
       gnus-selected-tree-overlay (goto-char (car region)) (cdr region))
      (gnus-tree-minimize)
      (gnus-tree-recenter)
      (let ((selected (selected-window)))
	(when (gnus-get-buffer-window (set-buffer gnus-tree-buffer) t)
	  (select-window (gnus-get-buffer-window (set-buffer gnus-tree-buffer) t))
	  (gnus-horizontal-recenter)
	  (select-window selected))))
;; If we remove this save-excursion, it updates the wrong mode lines?!?
    (with-current-buffer gnus-tree-buffer
      (gnus-set-mode-line 'tree))
    (set-buffer buf)))

(defun gnus-tree-highlight-article (article face)
  (with-current-buffer (gnus-get-tree-buffer)
    (let (region)
      (when (setq region (gnus-tree-article-region article))
	(gnus-put-text-property (car region) (cdr region) 'face face)
	(set-window-point
	 (gnus-get-buffer-window (current-buffer) t) (cdr region))))))

;;;
;;; gnus-carpal
;;;

(defvar gnus-carpal-group-buffer-buttons
  '(("next" . gnus-group-next-unread-group)
    ("prev" . gnus-group-prev-unread-group)
    ("read" . gnus-group-read-group)
    ("select" . gnus-group-select-group)
    ("catch-up" . gnus-group-catchup-current)
    ("new-news" . gnus-group-get-new-news-this-group)
    ("toggle-sub" . gnus-group-unsubscribe-current-group)
    ("subscribe" . gnus-group-unsubscribe-group)
    ("kill" . gnus-group-kill-group)
    ("yank" . gnus-group-yank-group)
    ("describe" . gnus-group-describe-group)
    "list"
    ("subscribed" . gnus-group-list-groups)
    ("all" . gnus-group-list-all-groups)
    ("killed" . gnus-group-list-killed)
    ("zombies" . gnus-group-list-zombies)
    ("matching" . gnus-group-list-matching)
    ("post" . gnus-group-post-news)
    ("mail" . gnus-group-mail)
    ("local" . (lambda () (interactive) (gnus-group-news 0)))
    ("rescan" . gnus-group-get-new-news)
    ("browse-foreign" . gnus-group-browse-foreign)
    ("exit" . gnus-group-exit)))

(defvar gnus-carpal-summary-buffer-buttons
  '("mark"
    ("read" . gnus-summary-mark-as-read-forward)
    ("tick" . gnus-summary-tick-article-forward)
    ("clear" . gnus-summary-clear-mark-forward)
    ("expirable" . gnus-summary-mark-as-expirable)
    "move"
    ("scroll" . gnus-summary-next-page)
    ("next-unread" . gnus-summary-next-unread-article)
    ("prev-unread" . gnus-summary-prev-unread-article)
    ("first" . gnus-summary-first-unread-article)
    ("best" . gnus-summary-best-unread-article)
    "article"
    ("headers" . gnus-summary-toggle-header)
    ("uudecode" . gnus-uu-decode-uu)
    ("enter-digest" . gnus-summary-enter-digest-group)
    ("fetch-parent" . gnus-summary-refer-parent-article)
    "mail"
    ("move" . gnus-summary-move-article)
    ("copy" . gnus-summary-copy-article)
    ("respool" . gnus-summary-respool-article)
    "threads"
    ("lower" . gnus-summary-lower-thread)
    ("kill" . gnus-summary-kill-thread)
    "post"
    ("post" . gnus-summary-post-news)
    ("local" . gnus-summary-news-other-window)
    ("mail" . gnus-summary-mail-other-window)
    ("followup" . gnus-summary-followup-with-original)
    ("reply" . gnus-summary-reply-with-original)
    ("cancel" . gnus-summary-cancel-article)
    "misc"
    ("exit" . gnus-summary-exit)
    ("fed-up" . gnus-summary-catchup-and-goto-next-group)))

(defvar gnus-carpal-server-buffer-buttons
  '(("add" . gnus-server-add-server)
    ("browse" . gnus-server-browse-server)
    ("list" . gnus-server-list-servers)
    ("kill" . gnus-server-kill-server)
    ("yank" . gnus-server-yank-server)
    ("copy" . gnus-server-copy-server)
    ("exit" . gnus-server-exit)))

(defvar gnus-carpal-browse-buffer-buttons
  '(("subscribe" . gnus-browse-unsubscribe-current-group)
    ("exit" . gnus-browse-exit)))

(defvar gnus-carpal-group-buffer "*Carpal Group*")
(defvar gnus-carpal-summary-buffer "*Carpal Summary*")
(defvar gnus-carpal-server-buffer "*Carpal Server*")
(defvar gnus-carpal-browse-buffer "*Carpal Browse*")

(defvar gnus-carpal-attached-buffer nil)

(defvar gnus-carpal-mode-hook nil
  "*Hook run in carpal mode buffers.")

(defvar gnus-carpal-button-face 'bold
  "*Face used on carpal buttons.")

(defvar gnus-carpal-header-face 'bold-italic
  "*Face used on carpal buffer headers.")

(defvar gnus-carpal-mode-map nil)
(put 'gnus-carpal-mode 'mode-class 'special)

(if gnus-carpal-mode-map
    nil
  (setq gnus-carpal-mode-map (make-keymap))
  (suppress-keymap gnus-carpal-mode-map)
  (define-key gnus-carpal-mode-map " " 'gnus-carpal-select)
  (define-key gnus-carpal-mode-map "\r" 'gnus-carpal-select)
  (define-key gnus-carpal-mode-map gnus-mouse-2 'gnus-carpal-mouse-select))

(defun gnus-carpal-mode ()
  "Major mode for clicking buttons.

All normal editing commands are switched off.
\\<gnus-carpal-mode-map>
The following commands are available:

\\{gnus-carpal-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-line-modified (cdr gnus-mode-line-modified))
  (setq major-mode 'gnus-carpal-mode)
  (setq mode-name "Gnus Carpal")
  (setq mode-line-process nil)
  (use-local-map gnus-carpal-mode-map)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (make-local-variable 'gnus-carpal-attached-buffer)
  (gnus-run-mode-hooks 'gnus-carpal-mode-hook))

(defun gnus-carpal-setup-buffer (type)
  (let ((buffer (symbol-value (intern (format "gnus-carpal-%s-buffer" type)))))
    (if (get-buffer buffer)
	()
      (with-current-buffer (gnus-get-buffer-create buffer)
	(gnus-carpal-mode)
	(setq gnus-carpal-attached-buffer
	      (intern (format "gnus-%s-buffer" type)))
	(let ((buttons (symbol-value
			(intern (format "gnus-carpal-%s-buffer-buttons"
					type))))
	      (buffer-read-only nil)
	      button)
	  (while buttons
	    (setq button (car buttons)
		  buttons (cdr buttons))
	    (if (stringp button)
		(set-text-properties
		 (point)
		 (prog2 (insert button) (point) (insert " "))
		 (list 'face gnus-carpal-header-face))
	      (set-text-properties
	       (point)
	       (prog2 (insert (car button)) (point) (insert " "))
	       (list 'gnus-callback (cdr button)
		     'face gnus-carpal-button-face
		     gnus-mouse-face-prop 'highlight))))
	  (let ((fill-column (- (window-width) 2)))
	    (fill-region (point-min) (point-max)))
	  (set-window-point (get-buffer-window (current-buffer))
			    (point-min)))))))

(defun gnus-carpal-select ()
  "Select the button under point."
  (interactive)
  (let ((func (get-text-property (point) 'gnus-callback)))
    (if (null func)
	()
      (pop-to-buffer (symbol-value gnus-carpal-attached-buffer))
      (call-interactively func))))

(defun gnus-carpal-mouse-select (event)
  "Select the button under the mouse pointer."
  (interactive "e")
  (mouse-set-point event)
  (gnus-carpal-select))

;;; Allow redefinition of functions.
(gnus-ems-redefine)

(provide 'gnus-salt)

;; arch-tag: 35449164-77b3-4398-bcbd-a2e3e998f810
;;; gnus-salt.el ends here
