;;; completions-highlight.el --- highlight and natural move throw *Completions* buffer -*- lexical-binding: t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Jimmy Aguilar Mena <spacibba at aol dot com>
;; Created: Aug 2020 Jimmy Aguilar Mena spacibba@aol.com
;; Keywords: help, abbrev

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

;; Enabling this package implements more dynamic interaction with the
;; *Completions* buffer to give the user a similar experience than
;; interacting with Zle from zsh shell.  This basically means:

;; 0. When tab is pressed in the minibuffer the *Completions* buffer
;; is shown as usual.

;; 1.1 If the completion list is too large then a second tab just
;; scrolls the list.

;; 1.2 If all the completion candidates are visible then a second tab
;; highlights the first candidate and completed in the minibuffer.
;; (selected)

;; 2. Every time tab is pressed the next horizontal completion (on the
;; right) is selected.

;; 3. When a candidate is highlighted arrow keys also selects the next
;; candidate in the arrow direction.  The arrow produces the same
;; result either in the minibuffer or in *Completions* window.

;; The package intents to implement such functionalities without using
;; hacks or complex functions, using the default Emacs *Completions*
;; infrastructure.  The main advantage is that it is not needed to
;; switch to/from *Completions* buffer to select a candidate from the
;; list with arrow keys.


;;; Code:

(require 'simple)
(require 'minibuffer)

(defgroup completions-highlight nil
  "Highlight candidates in completions buffer."
  :version "28.1"
  :group 'completion)

(defcustom completions-highlight-autoselect nil
  "Select first candidate without extra tab.

When this variable is nil an extra tab is required to select and
highlight the first candidate in the *Completions* buffer.  When
the value is non-nil the candidate is selected every time the
buffer is shown and updated."
  :type 'boolean
  :group 'completions-highlight
  :version "28.1")

(defface completions-highlight
  '((t :inherit highlight :extend t))
  "Default face for highlighting the current line in Hl-Line mode."
  :version "28.1"
  :group 'completions-highlight)

(defvar completions-highlight-overlay (make-overlay 0 0)
  "Overlay to use when `completion-highlight-mode' is enabled.")

(defvar minibuffer-tab-through-completions-function-save nil
  "Saves the the original value of completion-in-minibuffer-scroll-window.")

;; *Completions* side commands
(defun completions-highlight-this-completion ()
  "Highlight the completion under point or near."
  (next-completion -1)
  (next-completion 1)
  (completions-highlight-select-near))

(defun completions-highlight-select-near ()
  "Move to and highlight closer item in the completion list."
  (interactive "p")

  (cond
   ((eobp) (next-completion -1))
   ((bobp) (next-completion 1)))

  (let* ((obeg (point))
         (oend (next-single-property-change obeg 'mouse-face nil (point-max)))
         (choice (buffer-substring-no-properties obeg oend)))

    (move-overlay completions-highlight-overlay obeg oend)
    (minibuffer-completion-set-suffix choice)))

(defsubst completions-highlight-completions-visible-p ()
  "Return t if *Completions* is visible."
  (and (window-live-p minibuffer-scroll-window)
       (eq t (frame-visible-p (window-frame minibuffer-scroll-window)))))

;; Minibuffer side commands
(defmacro with-minibuffer-scroll-window (&rest body)
  "Execute BODY in *Completions* buffer and return to `minibuffer'.
The command is only executed if the `minibuffer-scroll-window' is
alive and active."
  `(and (completions-highlight-completions-visible-p)
	(with-selected-window minibuffer-scroll-window
          (ignore-errors ,@body)
          (run-hooks 'post-command-hook))))

(defun minibuffer-next-completion (n)
  "Execute `next-completion' in *Completions*.
The argument N is passed directly to
`next-completion', the command is executed
in another window, but cursor stays in minibuffer."
  (interactive "p")
  (with-minibuffer-scroll-window
   (next-completion n)))

(defun minibuffer-previous-completion (n)
  "Execute `previous-completion' in *Completions*.
The argument N is passed directly to `previous-completion', the
command is executed in another window, but cursor stays in
minibuffer."
  (interactive "p")
  (with-minibuffer-scroll-window
   (previous-completion n)))

(defun minibuffer-next-line-completion (n)
  "Execute `next-line' in *Completions*.
The argument N is passed directly to `next-line', the command is
executed in another window, but cursor stays in minibuffer."
  (interactive "p")
  (with-minibuffer-scroll-window (next-line n)))

(defun minibuffer-previous-line-completion (n)
  "Execute `previous-line' in *Completions*.
The argument N is passed directly to `previous-line', the command
is executed in another window, but cursor stays in minibuffer."
  (interactive "p")
  (with-minibuffer-scroll-window (previous-line n)))

;; General commands
(defun completions-highlight--set-suffix (choice)
  "Set CHOICE suffix to current completion.
It uses `completion-base-position' to determine the cursor
position.  If choice is the empty string the command removes the
suffix."
  (let* ((obase-position completion-base-position)
         (minibuffer-window (active-minibuffer-window))
         (minibuffer-buffer (window-buffer minibuffer-window))
         (completion-no-auto-exit t))

    (with-selected-window minibuffer-window
      (let* ((prompt-end (minibuffer-prompt-end))
	     (cursor-pos (if obase-position
			     (cadr obase-position)
			   (choose-completion-guess-base-position choice)))
	     (prefix-len (- cursor-pos prompt-end))
	     (suffix (if (< prefix-len (length choice))
			 (substring choice prefix-len)
		       ""))
	     (suffix-len (string-width suffix)))

        (choose-completion-string suffix minibuffer-buffer
                                  (list cursor-pos (point-max)))
        (add-face-text-property cursor-pos (+ cursor-pos suffix-len) 'shadow)
        (goto-char cursor-pos)))))

(defun completions-highlight--clear-suffix()
  "Clear completion suffix if set."
  (completions-highlight--set-suffix ""))

(defvar completions-highlight-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-must-match-map)
    (define-key map [backtab] #'minibuffer-previous-completion)
    (define-key map [right] #'minibuffer-next-completion)
    (define-key map [left] #'minibuffer-previous-completion)
    (define-key map [down] #'minibuffer-next-line-completion)
    (define-key map [up] #'minibuffer-previous-line-completion)
    map)
  "Keymap used in minibuffer while *Completions* is active.")

(defvar completions-highlight-completions-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map completion-list-mode-map)
    (define-key map "\C-g" #'quit-window)
    map)
  "Keymap used in *Completions* while highlighting candidates.")

(defun completions-highlight--minibuffer-tab-through-completions ()
  "Default action in `minibuffer-scroll-window' WINDOW.
This is called when *Completions* window is already visible and
should be assigned to completion-in-minibuffer-scroll-window."
  (let ((window minibuffer-scroll-window))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
	  (if (pos-visible-in-window-p (point-min) window)
	      ;; If all completions are shown point-min and point-max
	      ;; are both visible.  Then do the highlight.
	      (minibuffer-next-completion 1)
	    ;; Else the buffer is too long, so better just scroll it to
	    ;; the beginning as default behavior.
	    (set-window-start window (point-min) nil))
	;; Then point-max is not visible the buffer is too long and we
	;; can scroll.
	(with-selected-window window (scroll-up))))))

(defun completions-highlight-setup ()
  "Function to call when enabling the `completion-highlight-mode' mode.
It is called when showing the *Completions* buffer."
  (delete-overlay completions-highlight-overlay)

  (with-current-buffer standard-output
    (when (string= (buffer-name) "*Completions*")

      (add-hook 'pre-command-hook #'completions-highlight--clear-suffix nil t)
      (add-hook 'post-command-hook #'completions-highlight--this-completion nil t)

      ;; Add completions-highlight-completions-map to *Completions*
      (use-local-map (make-composed-keymap
                      completions-highlight-completions-map (current-local-map)))

      ;; Autoselect candidate if enabled
      (when completions-highlight-autoselect
        (with-selected-window (get-buffer-window (current-buffer) 0)
          (next-completion 1)
          (completions-highlight-select-near)))))

  (add-hook 'pre-command-hook #'completions-highlight--clear-suffix nil t)

  ;; Add completions-highlight-minibuffer-map bindings to minibuffer
  (use-local-map (make-composed-keymap
                  completions-highlight-minibuffer-map (current-local-map))))

;;;###autoload
(define-minor-mode completions-highlight-mode
  "Completion highlight mode to enable candidates highlight in the minibuffer."
  :global t
  :group 'minibuffer

  (if completions-highlight-mode
      (progn
        (overlay-put completions-highlight-overlay 'face 'completions-highlight)

        (setq minibuffer-tab-through-completions-function-save
	      minibuffer-tab-through-completions-function)

	(setq minibuffer-tab-through-completions-function
	      #'completions-highlight--minibuffer-tab-through-completions)

	(add-hook 'completion-setup-hook #'completions-highlight-setup t))

    ;; Restore the default completion-in-minibuffer-scroll-window
    (setq minibuffer-tab-through-completions-function
	  minibuffer-tab-through-completions-function-save)

    (remove-hook 'completion-setup-hook #'completions-highlight-setup)))

(provide 'completions-highlight)
;;; completions-highlight.el ends here
