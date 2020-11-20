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

(defcustom completions-highlight-set-suffix t
  "Insert completion candidate in minibuffer

When this variable is nil the completions will be highlighted but
not inserted in the minibuffer."
  :type 'boolean
  :group 'completions-highlight
  :version "28.1")

(defcustom completions-highlight-tab-no-scroll nil
  "When press tab with too many candidates go to next or scroll.

When non-nil tab always go to next completions independently of
the *Completions* buffer size.  When this variable is nil tab
scrolls the *Completions* buffer if there are too many candidates
otherwise it goes to the next completion. "
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
(defun completions-highlight-select-near ()
  "Move to and highlight closer item in the completion list."
  (interactive "p")

  ;; Try to find the closest completion if not in one
  (unless (get-text-property (point) 'mouse-face)
    (next-completion -1)
    (next-completion 1)

    (cond
     ((eobp) (next-completion -1))
     ((bobp) (next-completion 1))))

  (let* ((obeg (point))
         (oend (next-single-property-change obeg 'mouse-face nil (point-max)))
         (choice (buffer-substring-no-properties obeg oend)))

    (move-overlay completions-highlight-overlay obeg oend)
    (when completions-highlight-set-suffix
      (completions-highlight--set-suffix choice))))

(defsubst completions-highlight-completions-visible-p ()
  "Return t if *Completions* is visible."
  (and (window-live-p minibuffer-scroll-window)
       (eq t (frame-visible-p (window-frame minibuffer-scroll-window)))))

(defun completions-highlight-from-minibuffer (&optional command)
  (interactive)
  (and (completions-highlight-completions-visible-p)
       (with-selected-window minibuffer-scroll-window
         (when-let ((command (or command
                                 (lookup-key (current-active-maps)
                                             (this-single-command-keys))
                                 (lookup-key (current-active-maps)
                                             (lookup-key local-function-key-map
                                                         (this-single-command-keys))))))
           (call-interactively command)
           (run-hooks 'post-command-hook)))))

;; Maybe this may be done with an advise?
(defun minibuffer-choose-completion ()
  "Execute `choose-completion' in *Completions*."
  (interactive)
  (if (and (completions-highlight-completions-visible-p)
           (overlay-buffer completions-highlight-overlay))
      (call-interactively #'completions-highlight-from-minibuffer)
    (minibuffer-complete-and-exit)))

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
    (dolist (key '(up down left right backtab))
      (define-key map `[(,key)] #'completions-highlight-from-minibuffer))

    (define-key map [remap minibuffer-complete-and-exit] #'minibuffer-choose-completion)
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
      (if completions-highlight-tab-no-scroll
          (completions-highlight-from-minibuffer #'next-completion)
        (if (pos-visible-in-window-p (point-max) window) ;; scroll o go to next
	    (if (pos-visible-in-window-p (point-min) window)
	        ;; If all completions are shown point-min and point-max
	        ;; are both visible.  Then do the highlight.
	        (completions-highlight-from-minibuffer #'next-completion)
	      ;; Else the buffer is too long, so better just scroll it to
	      ;; the beginning as default behavior.
	      (set-window-start window (point-min) nil))
	  ;; Then point-max is not visible the buffer is too long and we
	  ;; can scroll.
	  (with-selected-window window (scroll-up)))))))

(defun completions-highlight-setup ()
  "Function to call when enabling the `completion-highlight-mode' mode.
It is called when showing the *Completions* buffer."
  (delete-overlay completions-highlight-overlay)

  (with-current-buffer standard-output
    (when (string= (buffer-name) "*Completions*")

      (add-hook 'pre-command-hook #'completions-highlight--clear-suffix nil t)
      (add-hook 'post-command-hook #'completions-highlight-select-near nil t)

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
