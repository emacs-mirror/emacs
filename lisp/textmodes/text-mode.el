;;; text-mode.el --- text mode, and its idiosyncratic commands  -*- lexical-binding: t -*-

;; Copyright (C) 1985, 1992, 1994, 2001-2026 Free Software Foundation,
;; Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: text
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
  :options '(turn-on-auto-fill flyspell-mode)
  :version "31.1"
  :group 'text)

(defvar text-mode-variant nil
  "Non-nil if this buffer's major mode is a variant of Text mode.")
(make-obsolete-variable 'text-mode-variant
                        "\
don't set it, and instead of testing its value use `derived-mode-p'." "27.1")

;; Actually defined in textconv.c.
(defvar text-conversion-style)

(defvar text-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\\ ".   " st)
    ;; We add `p' so that M-c on 'hello' leads to 'Hello' rather than 'hello'.
    (modify-syntax-entry ?' "w p" st)
    ;; UAX #29 says HEBREW PUNCTUATION GERESH behaves like a letter
    ;; for the purposes of finding word boundaries.
    (modify-syntax-entry #x5f3 "w   " st) ; GERESH
    ;; UAX #29 says HEBREW PUNCTUATION GERSHAYIM should not be a word
    ;; boundary when surrounded by letters.  Our infrastructure for
    ;; finding a word boundary doesn't support 3-character
    ;; definitions, so for now simply make this a word-constituent
    ;; character.  This leaves a problem of having GERSHAYIM at the
    ;; beginning or end of a word, where it should be a boundary;
    ;; FIXME.
    (modify-syntax-entry #x5f4 "w   " st) ; GERSHAYIM
    ;; These all should not be a word boundary when between letters,
    ;; according to UAX #29, so they again are prone to the same
    ;; problem as GERSHAYIM; FIXME.
    (modify-syntax-entry #xb7 "w   " st)   ; MIDDLE DOT
    (modify-syntax-entry #x2027 "w   " st) ; HYPHENATION POINT
    (modify-syntax-entry #xff1a "w   " st) ; FULLWIDTH COLON
    st)
  "Syntax table used while in `text-mode'.")

(defvar-keymap text-mode-map
  :doc "Keymap for `text-mode'.
Many other modes, such as `mail-mode' and `outline-mode', inherit
all the commands defined in this map.")

(defcustom text-mode-ispell-word-completion 'completion-at-point
  "How Text mode provides Ispell word completion.

By default, this option is set to `completion-at-point', which
means that Text mode adds an Ispell word completion function to
`completion-at-point-functions'.  Any other non-nil value says to
bind M-TAB directly to `ispell-complete-word' instead.  If this
is nil, Text mode neither binds M-TAB to `ispell-complete-word'
nor does it extend `completion-at-point-functions'."
  :group 'text
  :type '(choice (const completion-at-point) boolean)
  :version "30.1"
  :set (lambda (sym val)
         (if (and (set sym val)
                  (not (eq val 'completion-at-point)))
             (keymap-set text-mode-map "C-M-i" #'ispell-complete-word)
           (keymap-unset text-mode-map "C-M-i" t))))

(easy-menu-define text-mode-menu text-mode-map
  "Menu for `text-mode'."
  '("Text"
    ["Center Line" center-line
     :help "Center the current line"]
    ["Center Paragraph" center-paragraph
     :help "Center the current paragraph"]
    ["Center Region" center-region
     :help "Center the marked region"
     :enable (region-active-p)]
    "---"
    ["Paragraph Indent" paragraph-indent-minor-mode
     :help "Toggle paragraph indent minor mode"
     :style toggle
     :selected (bound-and-true-p paragraph-indent-minor-mode)]
    ["Auto Fill" toggle-text-mode-auto-fill
     :help "Automatically fill text while typing in text modes (Auto Fill mode)"
     :style toggle
     :selected (memq 'turn-on-auto-fill text-mode-hook)]))

(defun text-mode-context-menu (menu click)
  "Populate MENU with text selection commands at CLICK."

  (when (thing-at-mouse click 'word)
    (define-key-after menu [select-region mark-word]
      `(menu-item "Word"
                  ,(lambda (e) (interactive "e") (mark-thing-at-mouse e 'word))
                  :help "Mark the word at click for a subsequent cut/copy")
      'mark-whole-buffer))
  (define-key-after menu [select-region mark-sentence]
    `(menu-item "Sentence"
                ,(lambda (e) (interactive "e") (mark-thing-at-mouse e 'sentence))
                :help "Mark the sentence at click for a subsequent cut/copy")
    'mark-whole-buffer)
  (define-key-after menu [select-region mark-paragraph]
    `(menu-item "Paragraph"
                ,(lambda (e) (interactive "e") (mark-thing-at-mouse e 'paragraph))
                :help "Mark the paragraph at click for a subsequent cut/copy")
    'mark-whole-buffer)

  menu)


(define-derived-mode text-mode nil "Text"
  "Major mode for editing text written for humans to read.
In this mode, paragraphs are delimited only by blank or white lines.
You can thus get the full benefit of adaptive filling
 (see the variable `adaptive-fill-mode').
\\{text-mode-map}
Turning on Text mode runs the normal hook `text-mode-hook'."
  (setq-local text-mode-variant t)
  (setq-local require-final-newline mode-require-final-newline)

  ;; Enable text conversion in this buffer.
  (setq-local text-conversion-style t)
  (add-hook 'context-menu-functions 'text-mode-context-menu 10 t)
  (when (eq text-mode-ispell-word-completion 'completion-at-point)
    (add-hook 'completion-at-point-functions #'ispell-completion-at-point 10 t)))

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
    (if (string-prefix-p ps-re paragraph-start)
        (if (not paragraph-indent-minor-mode)
            (setq-local paragraph-start
                        (substring paragraph-start (length ps-re))))
      (if paragraph-indent-minor-mode
          (setq-local paragraph-start (concat ps-re paragraph-start)))))
  ;; Change the indentation function.
  (if paragraph-indent-minor-mode
      (add-function :override (local 'indent-line-function)
                    #'indent-to-left-margin)
    (remove-function (local 'indent-line-function)
                     #'indent-to-left-margin)))

;; This can be made a no-op once all modes that use text-mode-hook
;; are "derived" from text-mode.  (As of 2015/04, and probably well before,
;; the only one I can find that doesn't so derive is rmail-edit-mode.)
(defun text-mode-hook-identify ()
  "Mark that this mode has run `text-mode-hook'.
This is how `toggle-text-mode-auto-fill' knows which buffers to operate on."
  (setq-local text-mode-variant t))

(defun toggle-text-mode-auto-fill ()
  "Toggle whether to use Auto Fill in Text mode and related modes.
This command affects all buffers that use modes related to Text mode,
both existing buffers and buffers that you subsequently create."
  (interactive)
  (let ((enable-mode (not (memq 'turn-on-auto-fill text-mode-hook))))
    (if enable-mode
        (add-hook 'text-mode-hook #'turn-on-auto-fill)
      (remove-hook 'text-mode-hook #'turn-on-auto-fill))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(if (or (derived-mode-p 'text-mode) text-mode-variant)
	    (auto-fill-mode (if enable-mode 1 0)))))
    (message "Auto Fill %s in Text modes"
	     (if enable-mode "enabled" "disabled"))))


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
            space)
	(beginning-of-line)
	(delete-horizontal-space)
	(end-of-line)
	(delete-horizontal-space)
        (setq space (- fill-column lm (current-column)))
        (if (> space 0)
            (indent-line-to (+ lm (/ space 2))))))
    (cond ((null nlines)
	   (setq nlines 0))
	  ((> nlines 0)
	   (setq nlines (1- nlines))
	   (forward-line 1))
	  ((< nlines 0)
	   (setq nlines (1+ nlines))
	   (forward-line -1)))))

;; Actually defined in track-changes.el.
(defvar track-changes-undo-only)
(declare-function track-changes-register "track-changes"
                  ( signal &optional &key nobefore disjoint immediate))
(declare-function track-changes-unregister "track-changes" (id))
(declare-function track-changes-fetch "track-changes" (id func))

(defvar-local center-line-mode--track-changes nil)

(defun center-line-mode--track-changes-signal (tracker)
  (track-changes-fetch
   tracker
   #'center-line-mode--track-changes-function))

(defun center-line-mode--track-changes-function (beg end _before)
  (unless track-changes-undo-only
    (save-excursion
      (let ((beg-line (line-number-at-pos beg))
            (end-line (line-number-at-pos end))
            (should-center-last-line-p
             (progn
               (goto-char end)
               (null
                (or (bolp)
                    (and (eolp)
                         (looking-back "[\r\n\t ]" (1- (point)))))))))
        (goto-char beg)
        (dotimes (_ (- end-line beg-line)) ; all but last line
          (unless (and (bolp) (eolp))
            (center-line))
          (forward-line 1))
        (when should-center-last-line-p
          (center-line)))))
  ;; Disregard our own changes.
  (track-changes-fetch center-line-mode--track-changes #'ignore))

(define-minor-mode center-line-mode
  "Minor mode for keeping modified lines centered horizontally.
Calls `center-line' on each line of the modified region to center the
text within the width specified by `fill-column'."
  :lighter " Center-Line"
  (require 'track-changes)
  (if center-line-mode
      (setq center-line-mode--track-changes
            (track-changes-register
             #'center-line-mode--track-changes-signal
             :nobefore t))
    (when center-line-mode--track-changes
      (track-changes-unregister center-line-mode--track-changes))))

(define-obsolete-function-alias 'indented-text-mode #'text-mode "29.1")



(defvar text-mode--fullwidth-table nil)

(defun text-mode--get-fullwidth-table ()
  "Return translation table for converting half-width characters to fullwidth."
  (or (and (char-table-p text-mode--fullwidth-table)
           text-mode--fullwidth-table)
      ;; Create the translation table.
      (let ((tbl (make-char-table 'translation-table))
            (rev-tbl (make-char-table 'translation-table))
            (ch ?!))
        (while (<= ch ?~)
          ;; ! -> ！, 0 -> ０, A -> Ａ, etc.
          (aset tbl ch (+ ch #xFEE0))
          (aset rev-tbl (+ ch #xFEE0) ch)
          (setq ch (1+ ch)))
        ;; SPC -> U+3000 IDEOGRAPHIC SPACE
        (aset tbl ?\  #x3000)
        (aset rev-tbl #x3000 ?\ )
        (set-char-table-extra-slot tbl 0 rev-tbl)
        (set-char-table-extra-slot tbl 1 1)
        (set-char-table-extra-slot rev-tbl 1 1)
        (put 'text-mode--fullwidth-table 'translation-table tbl)
        (setq text-mode--fullwidth-table tbl)
        tbl)))

(defun fullwidth-region (from to)
  "Convert ASCII characters in the region to their fullwidth variants.
This converts 1 to １, A to Ａ, etc.
When called from Lisp, FROM and TO are character positions that define
the region in which to convert characters."
  (interactive "r")
  (translate-region from to
                    (text-mode--get-fullwidth-table)))

(defun halfwidth-region (from to)
  "Convert fullwidth characters in the region to their ASCII variants.
This converts １ to 1, Ａ to A, etc.
When called from Lisp, FROM and TO are character positions that define
the region in which to convert characters."
  (interactive "r")
  (translate-region from to
                    (char-table-extra-slot (text-mode--get-fullwidth-table)
                                           0)))

(defun fullwidth-word (arg)
  "Convert fullwidth characters in word at point, moving over the word.
This converts fullwidth characters to their ASCII variants:
１ to 1, Ａ to A, etc.
With numerical argument ARG, convert that many words starting from point.
With negative argument, convert previous words, but do not move point.
If point is in the middle of a word, the part of that word before point
is ignored when converting forward, and the part of that word after
point is ignored when converting backward."
  (interactive "p")
  (let* ((pt (point-marker))
         (beg pt)
         (end (progn
                (forward-word arg)
                (point))))
    (fullwidth-region beg end)
    (or (> arg 0) (goto-char pt))))

(defun halfwidth-word (arg)
  "Convert characters in word at point to fullwidth, moving over the word.
This converts ASCII characters to their fullwidth variants:
1 to １, A to Ａ, etc.
With numerical argument ARG, convert that many words starting from point.
With negative argument, convert previous words, but do not move point.
If point is in the middle of a word, the part of that word before point
is ignored when converting forward, and the part of that word after
point is ignored when converting backward."
  (interactive "p")
  (let* ((pt (point-marker))
         (beg pt)
         (end (progn
                (forward-word arg)
                (point))))
    (halfwidth-region beg end)
    (or (> arg 0) (goto-char pt))))

(provide 'text-mode)

;;; text-mode.el ends here
