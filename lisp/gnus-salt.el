;;; gnus-salt.el --- alternate summary mode interfaces for Gnus
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>

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

;;; Code:

(require 'gnus)
(eval-when-compile (require 'cl))

;;;
;;; gnus-pick-mode
;;;

(defvar gnus-pick-mode nil
  "Minor mode for providing a pick-and-read interface in Gnus summary buffers.")

(defvar gnus-pick-display-summary nil
  "*Display summary while reading.")

(defvar gnus-pick-mode-hook nil
  "Hook run in summary pick mode buffers.")

;;; Internal variables.

(defvar gnus-pick-mode-map nil)

(unless gnus-pick-mode-map
  (setq gnus-pick-mode-map (make-sparse-keymap))

  (gnus-define-keys
   gnus-pick-mode-map
   "t" gnus-uu-mark-thread
   "T" gnus-uu-unmark-thread
   " " gnus-summary-mark-as-processable
   "u" gnus-summary-unmark-as-processable
   "U" gnus-summary-unmark-all-processable
   "v" gnus-uu-mark-over
   "r" gnus-uu-mark-region
   "R" gnus-uu-unmark-region
   "e" gnus-uu-mark-by-regexp
   "E" gnus-uu-mark-by-regexp
   "b" gnus-uu-mark-buffer
   "B" gnus-uu-unmark-buffer
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
	["Regexp" gnus-uu-mark-regexp t]
	["Buffer" gnus-uu-mark-buffer t])
       ("Unpick"
	["Article" gnus-summary-unmark-as-processable t]
	["Thread" gnus-uu-unmark-thread t]
	["Region" gnus-uu-unmark-region t]
	["Regexp" gnus-uu-unmark-regexp t]
	["Buffer" gnus-uu-unmark-buffer t])
       ["Start reading" gnus-pick-start-reading t]
       ["Switch pick mode off" gnus-pick-mode gnus-pick-mode]))))

(defun gnus-pick-mode (&optional arg)
  "Minor mode for providing a pick-and-read interface in Gnus summary buffers.

\\{gnus-pick-mode-map}"
  (interactive "P")
  (when (eq major-mode 'gnus-summary-mode)
    (make-local-variable 'gnus-pick-mode)
    (setq gnus-pick-mode 
	  (if (null arg) (not gnus-pick-mode)
	    (> (prefix-numeric-value arg) 0)))
    (when gnus-pick-mode
      ;; Make sure that we don't select any articles upon group entry.
      (make-local-variable 'gnus-auto-select-first)
      (setq gnus-auto-select-first nil)
      ;; Set up the menu.
      (when (and menu-bar-mode
		 (gnus-visual-p 'pick-menu 'menu))
	(gnus-pick-make-menu-bar))
      (unless (assq 'gnus-pick-mode minor-mode-alist)
	(push '(gnus-pick-mode " Pick") minor-mode-alist))
      (unless (assq 'gnus-pick-mode minor-mode-map-alist)
	(push (cons 'gnus-pick-mode gnus-pick-mode-map)
	      minor-mode-map-alist))
      (run-hooks 'gnus-pick-mode-hook))))

(defun gnus-pick-start-reading (&optional catch-up)
  "Start reading the picked articles.
If given a prefix, mark all unpicked articles as read."
  (interactive "P")
  (unless gnus-newsgroup-processable
    (error "No articles have been picked"))
  (gnus-summary-limit-to-articles nil)
  (when catch-up
    (gnus-summary-limit-mark-excluded-as-read))
  (gnus-summary-first-unread-article)
  (gnus-configure-windows (if gnus-pick-display-summary 'article 'pick) t))


;;;
;;; gnus-binary-mode
;;;

(defvar gnus-binary-mode nil
  "Minor mode for provind a binary group interface in Gnus summary buffers.")

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
      (when (and menu-bar-mode
		 (gnus-visual-p 'binary-menu 'menu))
	(gnus-binary-make-menu-bar))
      (unless (assq 'gnus-binary-mode minor-mode-alist)
	(push '(gnus-binary-mode " Binary") minor-mode-alist))
      (unless (assq 'gnus-binary-mode minor-mode-map-alist)
	(push (cons 'gnus-binary-mode gnus-binary-mode-map)
	      minor-mode-map-alist))
      (run-hooks 'gnus-binary-mode-hook))))

(defun gnus-binary-display-article (article &optional all-header)
  "Run ARTICLE through the binary decode functions."
  (when (gnus-summary-goto-subject article)
    (let ((gnus-view-pseudos 'automatic))
      (gnus-uu-decode-uu))))

(defun gnus-binary-show-article (&optional arg)
  "Bypass the binary functions and show the article."
  (interactive "P")
  (let (gnus-summary-display-article-function)
    (gnus-summary-show-article arg)))

;;;
;;; gnus-tree-mode
;;;

(defvar gnus-tree-line-format "%(%[%3,3n%]%)"
  "Format of tree elements.")

(defvar gnus-tree-minimize-window t
  "If non-nil, minimize the tree buffer window.
If a number, never let the tree buffer grow taller than that number of
lines.")

(defvar gnus-selected-tree-face 'modeline
  "*Face used for highlighting selected articles in the thread tree.")

(defvar gnus-tree-brackets '((?\[ . ?\]) (?\( . ?\))
			     (?\{ . ?\}) (?< . ?>))
  "Brackets used in tree nodes.")

(defvar gnus-tree-parent-child-edges '(?- ?\\ ?|)
  "Charaters used to connect parents with children.")

(defvar gnus-tree-mode-line-format "Gnus: %%b %S %Z"
  "*The format specification for the tree mode line.")

(defvar gnus-generate-tree-function 'gnus-generate-vertical-tree
  "*Function for generating a thread tree.
Two predefined functions are available:
`gnus-generate-horizontal-tree' and `gnus-generate-vertical-tree'.")

(defvar gnus-tree-mode-hook nil
  "*Hook run in tree mode buffers.")

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
  (setq gnus-tree-mode-line-format-spec 
	(gnus-parse-format gnus-tree-mode-line-format 
			   gnus-summary-mode-line-format-alist))
  (setq gnus-tree-line-format-spec 
	(gnus-parse-format gnus-tree-line-format 
			   gnus-tree-line-format-alist t))
  (when (and menu-bar-mode
	     (gnus-visual-p 'tree-menu 'menu))
    (gnus-tree-make-menu-bar))
  (kill-all-local-variables)
  (gnus-simplify-mode-line)
  (setq mode-name "Tree")
  (setq major-mode 'gnus-tree-mode)
  (use-local-map gnus-tree-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (save-excursion
    (gnus-set-work-buffer)
    (gnus-tree-node-insert (make-mail-header "") nil)
    (setq gnus-tree-node-length (1- (point))))
  (run-hooks 'gnus-tree-mode-hook))

(defun gnus-tree-read-summary-keys (&optional arg)
  "Read a summary buffer key sequence and execute it."
  (interactive "P")
  (let ((buf (current-buffer))
	win)
    (gnus-article-read-summary-keys arg nil t)
    (when (setq win (get-buffer-window buf))
      (select-window win)
      (when gnus-selected-tree-overlay
	(goto-char (or (gnus-overlay-end gnus-selected-tree-overlay) 1)))
      (gnus-tree-minimize))))

(defun gnus-tree-select-article (article)
  "Select the article under point, if any."
  (interactive (list (gnus-tree-article-number)))
  (let ((buf (current-buffer)))
    (when article
      (save-excursion
	(set-buffer gnus-summary-buffer)
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
  (let ((pos (text-property-any (point-min) (point-max) 'gnus-number article)))
    (when pos
      (cons pos (next-single-property-change pos 'gnus-number)))))

(defun gnus-tree-goto-article (article)
  (let ((pos (text-property-any (point-min) (point-max) 'gnus-number article)))
    (when pos
      (goto-char pos))))

(defun gnus-tree-recenter ()
  "Center point in the tree window."
  (let ((selected (selected-window))
	(tree-window (get-buffer-window gnus-tree-buffer t)))
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
  (save-excursion
    (set-buffer (get-buffer-create gnus-tree-buffer))
    (unless (eq major-mode 'gnus-tree-mode)
      (gnus-add-current-to-buffer-list)
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
	    (select-window win)
	    (enlarge-window (- tot wh))
	    (select-window selected)))))))

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
    (save-excursion
      (set-buffer gnus-summary-buffer)
      (let* ((score (or (cdr (assq article gnus-newsgroup-scored))
			gnus-summary-default-score 0))
	     (default gnus-summary-default-score)
	     (mark (or (gnus-summary-article-mark article) gnus-unread-mark)))
	;; Eval the cars of the lists until we find a match.
	(while (and list
		    (not (eval (caar list))))
	  (setq list (cdr list)))))
    (unless (eq (setq face (cdar list)) (get-text-property beg 'face))
      (gnus-put-text-property 
       beg end 'face 
       (if (boundp face) (symbol-value face) face)))))

(defun gnus-tree-indent (level)
  (insert (make-string (1- (* (1+ gnus-tree-node-length) level)) ? )))

(defvar gnus-tmp-limit)
(defvar gnus-tmp-sparse)
(defvar gnus-tmp-indent)

(defun gnus-generate-tree (thread)
  "Generate a thread tree for THREAD."
  (save-excursion
    (set-buffer (gnus-get-tree-buffer))
    (let ((buffer-read-only nil)
	  (gnus-tmp-indent 0))
      (erase-buffer)
      (funcall gnus-generate-tree-function thread 0)
      (gnus-set-mode-line 'tree)
      (goto-char (point-min))
      (gnus-tree-minimize)
      (gnus-tree-recenter)
      (let ((selected (selected-window)))
	(when (get-buffer-window (set-buffer gnus-tree-buffer) t)
	  (select-window (get-buffer-window (set-buffer gnus-tree-buffer) t))
	  (gnus-horizontal-recenter)
	  (select-window selected))))))

(defun gnus-generate-horizontal-tree (thread level &optional dummyp adopted)
  "Generate a horizontal tree."
  (let* ((dummy (stringp (car thread)))
	 (do (or dummy
		 (memq (mail-header-number (car thread)) gnus-tmp-limit)))
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
	  (setq col (- (setq beg (point)) (gnus-point-at-bol) 1))
	  ;; Draw "|" lines upwards.
	  (while (progn
		   (forward-line -1)
		   (forward-char col)
		   (= (following-char) ? ))
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
		(- (point) (gnus-point-at-bol)))))
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
		 (memq (mail-header-number (car thread)) gnus-tmp-limit)))
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
	  ;; Draw "-" lines leftwards.
	  (while (progn
		   (forward-char -2)
		   (= (following-char) ? ))
	    (delete-char 1)
	    (insert (car gnus-tree-parent-child-edges)))
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
  (when (save-excursion
	  (set-buffer gnus-summary-buffer)
	  (and gnus-use-trees
	       (vectorp (gnus-summary-article-header article))))
    (save-excursion
      (let ((top (save-excursion
		   (set-buffer gnus-summary-buffer)
		   (gnus-cut-thread
		    (gnus-remove-thread 
		     (mail-header-id 
		      (gnus-summary-article-header article)) t))))
	    (gnus-tmp-limit gnus-newsgroup-limit)
	    (gnus-tmp-sparse gnus-newsgroup-sparse))
	(when (or force
		  (not (eq top gnus-tree-displayed-thread)))
	  (gnus-generate-tree top)
	  (setq gnus-tree-displayed-thread top))))))

(defun gnus-tree-open (group)
  (gnus-get-tree-buffer))

(defun gnus-tree-close (group)
  ;(gnus-kill-buffer gnus-tree-buffer)
  )

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
	 (setq gnus-selected-tree-overlay (gnus-make-overlay 1 2))
	 'face gnus-selected-tree-face))
      ;; Move the overlay to the article.
      (gnus-move-overlay 
       gnus-selected-tree-overlay (goto-char (car region)) (cdr region))
      (gnus-tree-minimize)
      (gnus-tree-recenter)
      (let ((selected (selected-window)))
	(when (get-buffer-window (set-buffer gnus-tree-buffer) t)
	  (select-window (get-buffer-window (set-buffer gnus-tree-buffer) t))
	  (gnus-horizontal-recenter)
	  (select-window selected))))
    ;; If we remove this save-excursion, it updates the wrong mode lines?!?
    (save-excursion
      (set-buffer gnus-tree-buffer)
      (gnus-set-mode-line 'tree))
    (set-buffer buf)))

(defun gnus-tree-highlight-article (article face)
  (save-excursion
    (set-buffer (gnus-get-tree-buffer))
    (let (region)
      (when (setq region (gnus-tree-article-region article))
	(gnus-put-text-property (car region) (cdr region) 'face face)
	(set-window-point 
	 (get-buffer-window (current-buffer) t) (cdr region))))))

;;; Allow redefinition of functions.
(gnus-ems-redefine)

(provide 'gnus-salt)

;;; gnus-salt.el ends here
