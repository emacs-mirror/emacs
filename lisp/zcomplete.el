;;; zcomplete.el --- highlight and natural move throw *Completions* buffer -*- lexical-binding: t -*-

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

(defgroup zcomplete nil
  "Highlight candidates in completions buffer."
  :version "28.1"
  :group 'completion)

(defcustom zcomplete-set-suffix t
  "Insert completion candidate in minibuffer

When this variable is nil the completions will be highlighted but
not inserted in the minibuffer."
  :type 'boolean
  :group 'zcomplete
  :version "28.1")

(defcustom zcomplete-tab-no-scroll nil
  "When press tab with too many candidates go to next or scroll.

When non-nil tab always go to next completions independently of
the *Completions* buffer size.  When this variable is nil tab
scrolls the *Completions* buffer if there are too many candidates
otherwise it goes to the next completion. "
  :type 'boolean
  :group 'zcomplete
  :version "28.1")

(defface zcomplete
  '((t :inherit highlight :extend t))
  "Default face for highlighting the current line in Hl-Line mode."
  :version "28.1"
  :group 'zcomplete)

(defvar zcomplete-overlay (make-overlay 0 0)
  "Overlay to use when `completion-highlight-mode' is enabled.")

;; *Completions* side commands
(defun zcomplete-select-near ()
  "Move to and highlight closer item in the completion list."
  (interactive "p")
  (let ((point (point))
        (pmin (point-min))
        (pmax (point-max))
        prev next choice)

    ;; Try to find the closest completion if not in one
    (if (get-text-property point 'mouse-face)
        (unless isearch-mode ;; assert we are in the beginning
          (next-completion -1)
          (next-completion 1))

      (setq prev (previous-single-property-change (min pmax (1+ point)) 'mouse-face nil pmin))
      (setq next (next-single-property-change point 'mouse-face nil pmax))
      (if (or (eobp)
              (< (- point prev) (- next point)))
          (next-completion -1)
        (next-completion 1)))

    ;; Select region
    (setq point (point))
    (setq next (next-single-property-change point 'mouse-face nil (point-max)))
    (setq choice (buffer-substring-no-properties point next))

    (move-overlay zcomplete-overlay point next)
    (zcomplete--set-suffix choice)))

;; General commands
(defun zcomplete--set-suffix (choice)
  "Set CHOICE suffix to current completion.
It uses `completion-base-position' to determine the cursor
position.  If choice is the empty string the command removes the
suffix."
  (when zcomplete-set-suffix
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
          (goto-char cursor-pos))))))

(defvar zcomplete-completions-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'choose-completion)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [down-mouse-2] nil)
    (define-key map "\C-m" 'choose-completion)
    (define-key map "\e\e\e" 'delete-completion-window)
    (define-key map [left] 'previous-completion)
    (define-key map [right] 'next-completion)
    (define-key map [?\t] 'next-completion)
    (define-key map [backtab] 'previous-completion)
    (define-key map "\C-g" #'quit-window)
    map)
  "Keymap used in *Completions* while highlighting candidates.")

(defun zcomplete--minibuffer-hook ()
  "Close *Completions* buffer when the command is not in the map."
  (zcomplete--set-suffix "")
  (unless (lookup-key minibuffer-local-must-match-map
                      (this-single-command-keys))
    (minibuffer-hide-completions)))

(defun zcomplete--completions-pre-hook ()
  "Close *Completions* buffer when the command is not in the map."
  (zcomplete--set-suffix "")
  (when (eq this-command 'self-insert-command)
    (call-interactively #'quit-window)))

(defun zcomplete--hack (data context signal)
  "Alternative to command-error-default-function.
This will exit the *Completions* if the error is buffer-read-only."
  (if (eq (car data) 'buffer-read-only)
      (call-interactively #'quit-window)
    (command-error-default-function data context signal)))

(defun zcomplete--completions-setup-hook ()
  "Function to call when enabling the `completion-highlight-mode' mode.
It is called when showing the *Completions* buffer."
  (delete-overlay zcomplete-overlay)

  ;; Add zcomplete-minibuffer-map bindings to minibuffer
  (add-hook 'pre-command-hook #'zcomplete--minibuffer-hook nil t)

  ;; After this commands are for Completions
  (call-interactively #'switch-to-completions)
  (add-hook 'pre-command-hook #'zcomplete--completions-pre-hook nil t)
  (add-hook 'post-command-hook #'zcomplete-select-near nil t)

  (setq-local command-error-function #'zcomplete--hack)
  (setq-local mode-line-format nil)
  (use-local-map zcomplete-completions-map)

  ;; Autoselect candidate if enabled
  (zcomplete-select-near))

;;;###autoload
(define-minor-mode zcomplete-mode
  "Completion highlight mode to enable candidates highlight in the minibuffer."
  :global t
  :group 'minibuffer

  (if zcomplete-mode
      (progn
        (overlay-put zcomplete-overlay 'face 'zcomplete)
	(add-hook 'completion-setup-hook #'zcomplete--completions-setup-hook t))

    (remove-hook 'completion-setup-hook #'zcomplete--completions-setup-hook)))

(provide 'zcomplete)
;;; zcomplete.el ends here
