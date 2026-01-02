;;; paren.el --- highlight matching paren  -*- lexical-binding:t -*-

;; Copyright (C) 1993, 1996, 2001-2026 Free Software Foundation, Inc.

;; Author: rms@gnu.org
;; Maintainer: emacs-devel@gnu.org
;; Keywords: languages, faces

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Put this into your ~/.emacs:

;;  (show-paren-mode t)

;; It will display highlighting on whatever paren matches the one
;; before or after point.

;;; Code:

(defgroup paren-showing nil
  "Showing (un)matching of parens and expressions."
  :prefix "show-paren-"
  :group 'paren-matching)

(defcustom show-paren-style 'parenthesis
  "Style used when showing a matching paren.
Valid styles are `parenthesis' (meaning show the matching paren),
`expression' (meaning show the entire expression enclosed by the paren) and
`mixed' (meaning show the matching paren if it is visible, and the expression
otherwise)."
  :type '(choice (const parenthesis) (const expression) (const mixed)))

(defcustom show-paren-delay 0.125
  "Time in seconds to delay before showing a matching paren.
If you change this without using customize while `show-paren-mode' is
active, you must toggle the mode off and on again for this to take effect."
  :type '(number :tag "seconds")
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
	 (if (not show-paren-mode)
	     (set sym val)
	   (show-paren-mode -1)
	   (set sym val)
	   (show-paren-mode 1))))

(defcustom show-paren-priority 1000
  "Priority of paren highlighting overlays."
  :type 'integer
  :version "21.1")

(defcustom show-paren-ring-bell-on-mismatch nil
  "If non-nil, beep if mismatched paren is detected."
  :type 'boolean
  :version "20.3")

(defcustom show-paren-when-point-inside-paren nil
  "If non-nil, show parens when point is just inside one.
This will only be done when point isn't also just outside a paren."
  :type 'boolean
  :version "25.1")

(defcustom show-paren-when-point-in-periphery nil
  "If non-nil, show parens when point is in the line's periphery.
The periphery is at the beginning or end of a line or in any
whitespace there."
  :type 'boolean
  :version "25.1")

(defcustom show-paren-highlight-openparen t
  "Non-nil turns on openparen highlighting when matching forward.
When nil, and point stands just before an open paren, the paren
is not highlighted, the cursor being regarded as adequate to mark
its position."
  :type 'boolean)

(defcustom show-paren-context-when-offscreen nil
  "If non-nil, show context around the opening paren if it is offscreen.
The context is usually the line that contains the openparen,
except if the openparen is on its own line, in which case the
context includes the previous nonblank line.

By default, the context is shown in the echo area.

If set to the symbol `overlay', the context is shown in an
overlay at the top-left of the window.

If set to the symbol `child-frame', the context is shown in a
child frame at the top-left of the window.  You might want to
customize the `child-frame-border' face (especially the
background color) to give the child frame a distinguished border.
On non-graphical frames, the context is shown in the echo area."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "In echo area" t)
                 (const :tag "In overlay" overlay)
                 (const :tag "In child-frame" child-frame))
  :version "29.1")

(defcustom show-paren-not-in-comments-or-strings nil
  "If non-nil, do not highlight the parens inside comments and strings.
If set to `all', never highlight parens inside comments and strings.
If set to `on-mismatch', do not highlight mismatched parens inside
comments and strings.
If set to nil (the default), always highlight parens wherever they are."
  :type '(choice
          (const :tag "Never highlight parens in comments and strings" all)
          (const
           :tag "Don't highlight mismatched parens in comments and strings"
           on-mismatch)
          (const :tag "Always highlight parens" nil))
  :version "31.1")

(defvar show-paren--idle-timer nil)
(defvar show-paren--overlay
  (let ((ol (make-overlay (point) (point) nil t))) (delete-overlay ol) ol)
  "Overlay used to highlight the matching paren.")
(defvar show-paren--overlay-1
  (let ((ol (make-overlay (point) (point) nil t))) (delete-overlay ol) ol)
  "Overlay used to highlight the paren at point.")

(defcustom show-paren-predicate '(not (derived-mode . special-mode))
  "Whether to use `show-paren-mode' in a buffer.
The default is to enable the mode in all buffers that don't
derive from `special-mode', which means that it's on (by default)
in all editing buffers.
The predicate is passed as argument to `buffer-match-p', which see."
  :type 'buffer-predicate
  :safe #'booleanp
  :version "29.1")

;;;###autoload
(define-minor-mode show-paren-mode
  "Toggle visualization of matching parens (Show Paren mode).

When enabled, any matching parenthesis is highlighted in `show-paren-style'
after `show-paren-delay' seconds of Emacs idle time.

Also see `show-paren-predicate', which controls which buffers
this mode is enabled in.

This is a global minor mode.  To toggle the mode in a single buffer,
use `show-paren-local-mode'."
  :global t :group 'paren-showing
  :initialize #'custom-initialize-after-file-load
  :init-value t
  ;; Enable or disable the mechanism.
  ;; First get rid of the old idle timer.
  (when show-paren--idle-timer
    (cancel-timer show-paren--idle-timer)
    (setq show-paren--idle-timer nil))
  (setq show-paren--idle-timer (run-with-idle-timer
                                show-paren-delay t
                                #'show-paren-function))
  (unless show-paren-mode
    (show-paren--delete-overlays)))

(defun show-paren--delete-overlays ()
  (delete-overlay show-paren--overlay)
  (delete-overlay show-paren--overlay-1))

;;;###autoload
(define-minor-mode show-paren-local-mode
  "Toggle `show-paren-mode' only in this buffer."
  :variable ((show-paren--enabled-p)
             .
             (lambda (val) (setq-local show-paren-mode val)))
  (cond
   ((eq show-paren-mode (default-value 'show-paren-mode))
    (unless show-paren-mode
      (show-paren--delete-overlays)
      (kill-local-variable 'show-paren-mode)))
   ((not (default-value 'show-paren-mode))
    ;; Locally enabled, but globally disabled.
    (show-paren-mode 1)                ; Setup the timer.
    (setq-default show-paren-mode nil) ; But keep it globally disabled.
    )
   (t ;; Locally disabled only.
    (show-paren--delete-overlays))))

(defun show-paren--unescaped-p (pos)
  "Determine whether the paren after POS is unescaped."
  (save-excursion
    (goto-char pos)
    (= (logand (skip-syntax-backward "/\\") 1) 0)))

(defun show-paren--categorize-paren (pos)
  "Determine whether the character after POS has paren syntax,
and if so, return a cons (DIR . OUTSIDE), where DIR is 1 for an
open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  If the character isn't a
paren, or it is an escaped paren, return nil."
  (cond
   ((and (eq (syntax-class (syntax-after pos)) 4)
	 (show-paren--unescaped-p pos))
    (cons 1 pos))
   ((and (eq (syntax-class (syntax-after pos)) 5)
	 (show-paren--unescaped-p pos))
    (cons -1 (1+ pos)))))

(defun show-paren--locate-near-paren ()
  "Locate an unescaped paren \"near\" point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  Otherwise return nil."
  (let* ((ind-pos (save-excursion (back-to-indentation) (point)))
	 (eol-pos
	  (save-excursion
	    (end-of-line) (skip-chars-backward " \t" ind-pos) (point)))
	 (before (show-paren--categorize-paren (1- (point))))
	 (after (show-paren--categorize-paren (point))))
    (cond
     ;; Point is immediately outside a paren.
     ((eq (car before) -1) before)
     ((eq (car after) 1) after)
     ;; Point is immediately inside a paren.
     ((and show-paren-when-point-inside-paren before))
     ((and show-paren-when-point-inside-paren after))
     ;; Point is in the whitespace before the code.
     ((and show-paren-when-point-in-periphery
	   (<= (point) ind-pos))
      (or (show-paren--categorize-paren ind-pos)
	  (show-paren--categorize-paren (1- eol-pos))))
     ;; Point is in the whitespace after the code.
     ((and show-paren-when-point-in-periphery
	   (>= (point) eol-pos))
      (show-paren--categorize-paren (1- eol-pos))))))

(defvar show-paren-data-function #'show-paren--default
  "Function to find the opener/closer \"near\" point and its match.
The function is called with no argument and should return either nil
if there's no opener/closer near point, or a list of the form
\(HERE-BEG HERE-END THERE-BEG THERE-END MISMATCH)
Where HERE-BEG..HERE-END is expected to be near point.")

(defun show-paren--default ()
  "Find the opener/closer near point and its match.

It is the default value of `show-paren-data-function'."
  (let* ((temp (show-paren--locate-near-paren))
	 (dir (car temp))
	 (outside (cdr temp))
         ;; If we're inside a comment, then we probably want to blink
         ;; a matching parentheses in the comment.  So don't ignore
         ;; comments in that case.
         (parse-sexp-ignore-comments
          (if (ppss-comment-depth (syntax-ppss))
              nil
            parse-sexp-ignore-comments))
	 pos mismatch here-beg here-end)
    ;;
    ;; Find the other end of the sexp.
    (when dir
      (setq here-beg (if (eq dir 1) outside (1- outside))
	    here-end (if (eq dir 1) (1+ outside) outside))
      (save-restriction
	;; Determine the range within which to look for a match.
	(when blink-matching-paren-distance
	  (narrow-to-region
	   (max (point-min) (- (point) blink-matching-paren-distance))
	   (min (point-max) (+ (point) blink-matching-paren-distance))))
	;; Scan across one sexp within that range.
	;; Errors or nil mean there is a mismatch.
	(condition-case ()
	    (setq pos (scan-sexps outside dir))
	  (error (setq pos t mismatch t)))
	;; Move back the other way and verify we get back to the
	;; starting point.  If not, these two parens don't really match.
	;; Maybe the one at point is escaped and doesn't really count,
	;; or one is inside a comment.
	(when (integerp pos)
	  (unless (condition-case ()
		      (eq outside (scan-sexps pos (- dir)))
		    (error nil))
	    (setq pos nil)))
	;; If found a "matching" paren, see if it is the right
	;; kind of paren to match the one we started at.
	(if (not (integerp pos))
	    (if mismatch (list here-beg here-end nil nil t))
	  (let ((beg (min pos outside)) (end (max pos outside)))
	    (unless (eq (syntax-class (syntax-after beg)) 8)
	      (setq mismatch
		    (not (or (eq (char-before end)
				 ;; This can give nil.
				 (cdr (syntax-after beg)))
			     (eq (char-after beg)
				 ;; This can give nil.
				 (cdr (syntax-after (1- end))))
			     ;; The cdr might hold a new paren-class
			     ;; info rather than a matching-char info,
			     ;; in which case the two CDRs should match.
			     (eq (cdr (syntax-after (1- end)))
				 (cdr (syntax-after beg)))))))
	    (list here-beg here-end
		  (if (= dir 1) (1- pos) pos)
		  (if (= dir 1) pos (1+ pos))
		  mismatch)))))))

(defvar show-paren--context-child-frame nil)

(defun show-paren--context-child-frame-redirect-focus ()
  "Redirect focus from child frame."
  (redirect-frame-focus
   show-paren--context-child-frame
   (frame-parent show-paren--context-child-frame)))

(defun show-paren--context-child-frame-buffer (text)
  (with-current-buffer
      (get-buffer-create " *show-paren context*")
    ;; Redirect focus to parent.
    (add-hook 'pre-command-hook
              #'show-paren--delete-context-child-frame
              nil t)
    ;; Use an empty keymap.
    (use-local-map (make-keymap))
    (dolist (var '((mode-line-format . nil)
                   (header-line-format . nil)
                   (tab-line-format . nil)
                   (tab-bar-format . nil) ;; Emacs 28 tab-bar-format
                   (frame-title-format . "")
                   (truncate-lines . t)
                   (cursor-in-non-selected-windows . nil)
                   (cursor-type . nil)
                   (show-trailing-whitespace . nil)
                   (display-line-numbers . nil)
                   (left-fringe-width . nil)
                   (right-fringe-width . nil)
                   (left-margin-width . 0)
                   (right-margin-width . 0)
                   (fringes-outside-margins . 0)
                   (buffer-read-only . t)))
      (set (make-local-variable (car var)) (cdr var)))
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))
    (current-buffer)))

(defvar show-paren--context-child-frame-parameters
  `((visibility . nil)
    (width . 0) (height . 0)
    (min-width . t) (min-height . t)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (border-width . 0)
    (child-frame-border-width . 1)
    (left-fringe . 0)
    (right-fringe . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (no-other-window . t)
    (no-delete-other-windows . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t)))

(defun show-paren--delete-context-child-frame ()
  (when show-paren--context-child-frame
    (delete-frame show-paren--context-child-frame)
    (setq show-paren--context-child-frame nil))
  (remove-hook 'post-command-hook
               #'show-paren--delete-context-child-frame))

(defun show-paren--show-context-in-child-frame (text)
  "Show TEXT in a child-frame at the top-left of the current window."
  (let ((minibuffer (minibuffer-window (window-frame)))
        (buffer (show-paren--context-child-frame-buffer text))
        (x (window-pixel-left))
        (y (window-pixel-top))
        (window-min-height 1)
        (window-min-width 1)
        after-make-frame-functions)
    (show-paren--delete-context-child-frame)
    (setq show-paren--context-child-frame
          (make-frame
           `((parent-frame . ,(window-frame))
             (minibuffer . ,minibuffer)
             ,@show-paren--context-child-frame-parameters)))
    (let ((win (frame-root-window show-paren--context-child-frame)))
      (set-window-buffer win buffer)
      (set-window-dedicated-p win t)
      (set-frame-size show-paren--context-child-frame
                      (string-width text)
                      (length (string-lines text)))
      (set-frame-position show-paren--context-child-frame x y)
      (make-frame-visible show-paren--context-child-frame)
      (add-hook 'post-command-hook
                #'show-paren--delete-context-child-frame))))

(defvar-local show-paren--context-overlay nil)

(defun show-paren--delete-context-overlay ()
  (when show-paren--context-overlay
    (delete-overlay show-paren--context-overlay)
    (setq show-paren--context-overlay nil))
  (remove-hook 'post-command-hook #'show-paren--delete-overlays
               'local))

(defun show-paren--show-context-in-overlay (text)
  "Show TEXT in an overlay at the top-left of the current window."
  (setq text (replace-regexp-in-string "\n" " " text))
  (show-paren--delete-context-overlay)
  (let* ((beg (window-start))
         (end (save-excursion
                (goto-char beg)
                (line-end-position))))
    (setq show-paren--context-overlay (make-overlay beg end)))
  (overlay-put show-paren--context-overlay 'display text)
  ;; Use the (default very high) `show-paren-priority' ensuring that
  ;; not other overlays shine through (bug#59527).
  (overlay-put show-paren--context-overlay 'priority
               show-paren-priority)
  (overlay-put show-paren--context-overlay
               'face `(:box
                       ( :line-width (1 . -1)
                         :color ,(face-attribute 'shadow :foreground))))
  (add-hook 'post-command-hook #'show-paren--delete-context-overlay
            nil 'local))

;; The last position of point for which `show-paren-function' was
;; called.  We track it in order to C-g away a context overlay or
;; child-frame without having it pop up again after
;; `show-paren-delay'.
(defvar-local show-paren--last-pos nil)

(defun show-paren--enabled-p ()
  (and show-paren-mode
       ;; If we're using `show-paren-local-mode', then
       ;; always heed the value.
       (or (local-variable-p 'show-paren-mode)
           ;; If not, check that the predicate matches.
           (buffer-match-p show-paren-predicate (current-buffer)))))

(defun show-paren-function ()
  "Highlight the parentheses until the next input arrives."
  (let ((data (and (show-paren--enabled-p)
                   (funcall show-paren-data-function))))
    (if (not data)
        (progn
          ;; If show-paren-mode is nil in this buffer or if not at a paren that
          ;; has a match, turn off any previous paren highlighting.
          (delete-overlay show-paren--overlay)
          (delete-overlay show-paren--overlay-1)
          (setq show-paren--last-pos (point)))

      ;; Found something to highlight.
      (catch 'sp-exit
        (let* ((here-beg (nth 0 data))
               (here-end (nth 1 data))
               (there-beg (nth 2 data))
               (there-end (nth 3 data))
               (mismatch (nth 4 data))
               (highlight-expression
                (or (eq show-paren-style 'expression)
                    (and there-beg
                         (eq show-paren-style 'mixed)
                         (let ((closest (if (< there-beg here-beg)
                                            (1- there-end) (1+ there-beg))))
                           (not (pos-visible-in-window-p closest))))))
               (face
                (cond
                 (mismatch
                  (if (and (eq show-paren-not-in-comments-or-strings 'on-mismatch)
                           (save-excursion
                             (syntax-ppss-context (syntax-ppss here-beg))))
                      (throw 'sp-exit nil))
                  (if show-paren-ring-bell-on-mismatch
                      (beep))
                  'show-paren-mismatch)
                 (highlight-expression 'show-paren-match-expression)
                 (t 'show-paren-match))))
          (if (and (eq show-paren-not-in-comments-or-strings 'all)
                   (save-excursion
                     (syntax-ppss-context (syntax-ppss here-beg))))
              (throw 'sp-exit nil))
          ;;
          ;; If matching backwards, highlight the closeparen
          ;; before point as well as its matching open.
          ;; If matching forward, and the openparen is unbalanced,
          ;; highlight the paren at point to indicate misbalance.
          ;; Otherwise, turn off any such highlighting.
          (if (or (not here-beg)
                  (and (not show-paren-highlight-openparen)
                       (> here-end (point))
                       (<= here-beg (point))
                       (integerp there-beg)))
              (delete-overlay show-paren--overlay-1)
            (move-overlay show-paren--overlay-1
                          here-beg here-end (current-buffer))
            ;; Always set the overlay face, since it varies.
            (overlay-put show-paren--overlay-1 'priority show-paren-priority)
            (overlay-put show-paren--overlay-1 'face face))
          ;;
          ;; Turn on highlighting for the matching paren, if found.
          ;; If it's an unmatched paren, turn off any such highlighting.
          (if (not there-beg)
              (delete-overlay show-paren--overlay)
            (if highlight-expression
                (move-overlay show-paren--overlay
                              (if (< there-beg here-beg) here-end here-beg)
                              (if (< there-beg here-beg) there-beg there-end)
                              (current-buffer))
              (move-overlay show-paren--overlay
                            there-beg there-end (current-buffer)))
            ;; If `show-paren-context-when-offscreen' is non-nil and
            ;; point is at a closing paren, show the context around the
            ;; opening paren.
            (let ((openparen (min here-beg there-beg)))
              (when (and show-paren-context-when-offscreen
                         (not (eql show-paren--last-pos (point)))
                         (< there-beg here-beg)
                         ;; Either OPENPAREN position is fully visible...
                         (not (or (pos-visible-in-window-p openparen)
                                  (let ((dfh4 (* 0.25 (default-font-height)))
                                        (part
                                         (pos-visible-in-window-p openparen
                                                                  nil t)))
                                    ;; ...or partially visible, and the
                                    ;; invisible part is less than 1/4th
                                    ;; of the default font height
                                    (and (>= (length part) 4)
                                         (< (nth 2 part) dfh4)
                                         (< (nth 3 part) dfh4))))))
                (let ((context (blink-paren-open-paren-line-string
                                openparen))
                      (message-log-max nil))
                  (cond
                   ((eq show-paren-context-when-offscreen 'child-frame)
                    (show-paren--show-context-in-child-frame context))
                   ((eq show-paren-context-when-offscreen 'overlay)
                    (show-paren--show-context-in-overlay context))
                   (show-paren-context-when-offscreen
                    (minibuffer-message "Matches %s" context))))))
            (setq show-paren--last-pos (point))
            ;; Always set the overlay face, since it varies.
            (overlay-put show-paren--overlay 'priority show-paren-priority)
            (overlay-put show-paren--overlay 'face face)))))))

(provide 'paren)

;;; paren.el ends here
