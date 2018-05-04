;;; text-mode.el --- text mode, and its idiosyncratic commands

;; Copyright (C) 1985, 1992, 1994, 2001-2018 Free Software Foundation,
;; Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: wp
;; Package: emacs

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

;; This package provides the fundamental text mode documented in the
;; Emacs user's manual.

;;; Code:

;; Normally non-nil defaults for hooks are bad, but since this file is
;; preloaded it's ok/better, and avoids this showing up in customize-rogue.
(defcustom text-mode-hook '(text-mode-hook-identify)
  "Normal hook run when entering Text mode and many related modes."
  :type 'hook
  :options '(turn-on-auto-fill turn-on-flyspell)
  :group 'text)

(defvar text-mode-variant nil
  "Non-nil if this buffer's major mode is a variant of Text mode.
Use (derived-mode-p \\='text-mode) instead.")

(defvar text-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\\ ".   " st)
    ;; We add `p' so that M-c on 'hello' leads to 'Hello' rather than 'hello'.
    (modify-syntax-entry ?' "w p" st)
    ;; UAX #29 says HEBREW PUNCTUATION GERESH behaves like a letter
    ;; for the purposes of finding word boundaries.
    (modify-syntax-entry #x5f3 "w   ") ; GERESH
    ;; UAX #29 says HEBREW PUNCTUATION GERSHAYIM should not be a word
    ;; boundary when surrounded by letters.  Our infrastructure for
    ;; finding a word boundary doesn't support 3-character
    ;; definitions, so for now simply make this a word-constituent
    ;; character.  This leaves a problem of having GERSHAYIM at the
    ;; beginning or end of a word, where it should be a boundary;
    ;; FIXME.
    (modify-syntax-entry #x5f4 "w   ") ; GERSHAYIM
    ;; These all should not be a word boundary when between letters,
    ;; according to UAX #29, so they again are prone to the same
    ;; problem as GERSHAYIM; FIXME.
    (modify-syntax-entry #xb7 "w   ")	; MIDDLE DOT
    (modify-syntax-entry #x2027 "w   ")	; HYPHENATION POINT
    (modify-syntax-entry #xff1a "w   ")	; FULLWIDTH COLON
    st)
  "Syntax table used while in `text-mode'.")

(defvar text-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\t" 'ispell-complete-word)
    (define-key map [menu-bar text]
      (cons "Text" (make-sparse-keymap "Text")))
    (bindings--define-key map [menu-bar text toggle-text-mode-auto-fill]
      '(menu-item "Auto Fill" toggle-text-mode-auto-fill
                  :button (:toggle . (memq 'turn-on-auto-fill text-mode-hook))
                  :help "Automatically fill text while typing in text modes (Auto Fill mode)"))
    (bindings--define-key map [menu-bar text paragraph-indent-minor-mode]
      '(menu-item "Paragraph Indent" paragraph-indent-minor-mode
                  :button (:toggle . (bound-and-true-p paragraph-indent-minor-mode))
                  :help "Toggle paragraph indent minor mode"))
    (bindings--define-key map [menu-bar text sep] menu-bar-separator)
    (bindings--define-key map [menu-bar text center-region]
      '(menu-item "Center Region" center-region
                  :help "Center the marked region"
                  :enable (region-active-p)))
    (bindings--define-key map [menu-bar text center-paragraph]
      '(menu-item "Center Paragraph" center-paragraph
                  :help "Center the current paragraph"))
    (bindings--define-key map [menu-bar text center-line]
      '(menu-item "Center Line" center-line
                  :help "Center the current line"))
    map)
  "Keymap for `text-mode'.
Many other modes, such as `mail-mode', `outline-mode' and `indented-text-mode',
inherit all the commands defined in this map.")


(define-derived-mode text-mode nil "Text"
  "Major mode for editing text written for humans to read.
In this mode, paragraphs are delimited only by blank or white lines.
You can thus get the full benefit of adaptive filling
 (see the variable `adaptive-fill-mode').
\\{text-mode-map}
Turning on Text mode runs the normal hook `text-mode-hook'."
  (set (make-local-variable 'text-mode-variant) t)
  (set (make-local-variable 'require-final-newline)
       mode-require-final-newline)
  (set (make-local-variable 'indent-line-function) 'indent-relative))

(define-derived-mode paragraph-indent-text-mode text-mode "Parindent"
  "Major mode for editing text, with leading spaces starting a paragraph.
In this mode, you do not need blank lines between paragraphs
when the first line of the following paragraph starts with whitespace.
`paragraph-indent-minor-mode' provides a similar facility as a minor mode.
Special commands:
\\{text-mode-map}
Turning on Paragraph-Indent Text mode runs the normal hooks
`text-mode-hook' and `paragraph-indent-text-mode-hook'."
  :abbrev-table nil :syntax-table nil
  (paragraph-indent-minor-mode))

(define-minor-mode paragraph-indent-minor-mode
  "Minor mode for editing text, with leading spaces starting a paragraph.
In this mode, you do not need blank lines between paragraphs when the
first line of the following paragraph starts with whitespace, as with
`paragraph-indent-text-mode'.
Turning on Paragraph-Indent minor mode runs the normal hook
`paragraph-indent-text-mode-hook'."
  :initial-value nil
  ;; Change the definition of a paragraph start.
  (let ((ps-re "[ \t\n\f]\\|"))
    (if (eq t (compare-strings ps-re nil nil
                               paragraph-start nil (length ps-re)))
        (if (not paragraph-indent-minor-mode)
            (set (make-local-variable 'paragraph-start)
                 (substring paragraph-start (length ps-re))))
      (if paragraph-indent-minor-mode
          (set (make-local-variable 'paragraph-start)
               (concat ps-re paragraph-start)))))
  ;; Change the indentation function.
  (if paragraph-indent-minor-mode
      (add-function :override (local 'indent-line-function)
                    #'indent-to-left-margin)
    (remove-function (local 'indent-line-function)
                     #'indent-to-left-margin)))

(defalias 'indented-text-mode 'text-mode)

;; This can be made a no-op once all modes that use text-mode-hook
;; are "derived" from text-mode.  (As of 2015/04, and probably well before,
;; the only one I can find that doesn't so derive is rmail-edit-mode.)
(defun text-mode-hook-identify ()
  "Mark that this mode has run `text-mode-hook'.
This is how `toggle-text-mode-auto-fill' knows which buffers to operate on."
  (set (make-local-variable 'text-mode-variant) t))

(defun toggle-text-mode-auto-fill ()
  "Toggle whether to use Auto Fill in Text mode and related modes.
This command affects all buffers that use modes related to Text mode,
both existing buffers and buffers that you subsequently create."
  (interactive)
  (let ((enable-mode (not (memq 'turn-on-auto-fill text-mode-hook))))
    (if enable-mode
	(add-hook 'text-mode-hook 'turn-on-auto-fill)
      (remove-hook 'text-mode-hook 'turn-on-auto-fill))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(if (or (derived-mode-p 'text-mode) text-mode-variant)
	    (auto-fill-mode (if enable-mode 1 0)))))
    (message "Auto Fill %s in Text modes"
	     (if enable-mode "enabled" "disabled"))))


(define-key facemenu-keymap "\eS" 'center-paragraph)

(defun center-paragraph ()
  "Center each nonblank line in the paragraph at or after point.
See `center-line' for more info."
  (interactive)
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)))
      (backward-paragraph)
      (center-region (point) end))))

(defun center-region (from to)
  "Center each nonblank line starting in the region.
See `center-line' for more info."
  (interactive "r")
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (while (not (eobp))
	(or (save-excursion (skip-chars-forward " \t") (eolp))
	    (center-line))
	(forward-line 1)))))

(define-key facemenu-keymap "\es" 'center-line)

(defun center-line (&optional nlines)
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation so that it equals
the distance between the end of the text and `fill-column'.
The argument NLINES says how many lines to center."
  (interactive "P")
  (if nlines (setq nlines (prefix-numeric-value nlines)))
  (while (not (eq nlines 0))
    (save-excursion
      (let ((lm (current-left-margin))
	    line-length)
	(beginning-of-line)
	(delete-horizontal-space)
	(end-of-line)
	(delete-horizontal-space)
	(setq line-length (current-column))
	(if (> (- fill-column lm line-length) 0)
	    (indent-line-to
	     (+ lm (/ (- fill-column lm line-length) 2))))))
    (cond ((null nlines)
	   (setq nlines 0))
	  ((> nlines 0)
	   (setq nlines (1- nlines))
	   (forward-line 1))
	  ((< nlines 0)
	   (setq nlines (1+ nlines))
	   (forward-line -1)))))

(provide 'text-mode)

;;; text-mode.el ends here
