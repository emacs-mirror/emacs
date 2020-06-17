;;; icomplete.el --- minibuffer completion incremental feedback -*- lexical-binding: t -*-

;; Copyright (C) 1992-1994, 1997, 1999, 2001-2020 Free Software
;; Foundation, Inc.

;; Author: Ken Manheimer <ken dot manheimer at gmail...>
;; Created: Mar 1993 Ken Manheimer, klm@nist.gov - first release to usenet
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

;; Enabling this package implements a more fine-grained minibuffer
;; completion feedback scheme.  Prospective completions are concisely
;; indicated within the minibuffer itself, with each successive
;; keystroke.

;; See `icomplete-completions' docstring for a description of the
;; icomplete display format.

;; See the `icomplete-minibuffer-setup-hook' docstring for a means to
;; customize icomplete setup for interoperation with other
;; minibuffer-oriented packages.

;; To enable/disable icomplete mode, use the `icomplete-mode' function.

;; Thanks to everyone for their suggestions for refinements of this
;; package.  I particularly have to credit Michael Cook, who
;; implemented an incremental completion style in his 'iswitch'
;; functions that served as a model for icomplete.  Some other
;; contributors: Noah Friedman (restructuring as minor mode), Colin
;; Rafferty (lemacs reconciliation), Lars Lindberg, RMS, and others.

;; klm.

;;; Code:

(require 'rfn-eshadow) ; rfn-eshadow-overlay

(defgroup icomplete nil
  "Show completions dynamically in minibuffer."
  :prefix "icomplete-"
  :link '(info-link "(emacs)Icomplete")
  :group 'minibuffer)

(defcustom icomplete-separator " | "
  "String used by Icomplete to separate alternatives in the minibuffer."
  :type 'string
  :version "24.4")

(defcustom icomplete-hide-common-prefix t
  "When non-nil, hide common prefix from completion candidates.
When nil, show candidates in full."
  :type 'boolean
  :version "24.4")

(defvar icomplete-tidy-shadowed-file-names nil
  "If non-nil, automatically delete superfluous parts of file names.
For example, if the user types ~/ after a long path name,
everything preceding the ~/ is discarded so the interactive
selection process starts again from the user's $HOME.")

(defcustom icomplete-show-matches-on-no-input nil
  "When non-nil, show completions when first prompting for input."
  :type 'boolean
  :version "24.4")

(defcustom icomplete-with-completion-tables t
  "Specialized completion tables with which Icomplete should operate.
If this is t, Icomplete operates on all tables.
Otherwise this should be a list of the completion tables (e.g.,
`internal-complete-buffer') on which Icomplete should operate."
  ;; Prior to 24.4, not a user-option, default '(internal-complete-buffer).
  :version "24.4"
  :type '(choice (const :tag "All" t)
		 (repeat function)))

(defface icomplete-first-match '((t :weight bold))
  "Face used by Icomplete for highlighting first match."
  :version "24.4")

;;;_* User Customization variables
(defcustom icomplete-prospects-height 2
  ;; We used to compute how many lines 100 characters would take in
  ;; the current window width, but the return value of `window-width'
  ;; is unreliable on startup (e.g., if we're in daemon mode), so now
  ;; we simply base the default value on an 80 column window.
  "Maximum number of lines to use in the minibuffer."
  :type 'integer
  :version "26.1")

(defcustom icomplete-compute-delay .3
  "Completions-computation stall, used only with large-number completions.
See `icomplete-delay-completions-threshold'."
  :type 'number)

(defcustom icomplete-delay-completions-threshold 400
  "Pending-completions number over which to apply `icomplete-compute-delay'."
  :type 'integer)

(defcustom icomplete-max-delay-chars 3
  "Maximum number of initial chars to apply `icomplete-compute-delay'."
  :type 'integer)

(defvar icomplete-in-buffer nil
  "If non-nil, also use Icomplete when completing in non-mini buffers.")

(defcustom icomplete-minibuffer-setup-hook nil
  "Icomplete-specific customization of minibuffer setup.

This hook is run during minibuffer setup if Icomplete is active.
It is intended for use in customizing Icomplete for interoperation
with other features and packages.  For instance:

  (add-hook \\='icomplete-minibuffer-setup-hook
	     (lambda () (setq-local max-mini-window-height 3)))

will constrain Emacs to a maximum minibuffer height of 3 lines when
icompletion is occurring."
  :type 'hook
  :group 'icomplete)


;;;_* Initialization

;;;_ + Internal Variables
;;;_  = icomplete-eoinput nil
(defvar icomplete-overlay (make-overlay (point-min) (point-min) nil t t)
  "Overlay used to display the list of completions.")

(defun icomplete-pre-command-hook ()
 (let ((non-essential t))
   (icomplete-tidy)))

(defun icomplete-post-command-hook ()
  (let ((non-essential t)) ;E.g. don't prompt for password!
    (icomplete-exhibit)))

(defvar icomplete-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\M-\t] 'icomplete-force-complete)
    (define-key map [?\C-j]  'icomplete-force-complete-and-exit)
    (define-key map [?\C-.]  'icomplete-forward-completions)
    (define-key map [?\C-,]  'icomplete-backward-completions)
    map)
  "Keymap used by `icomplete-mode' in the minibuffer.")

(defun icomplete-force-complete-and-exit ()
  "Complete the minibuffer with the longest possible match and exit.
Use the first of the matches if there are any displayed, and use
the default otherwise."
  (interactive)
  ;; This function is tricky.  The mandate is to "force", meaning we
  ;; should take the first possible valid completion for the input.
  ;; However, if there is no input and we can prove that that
  ;; coincides with the default, it is much faster to just call
  ;; `minibuffer-complete-and-exit'.  Otherwise, we have to call
  ;; `minibuffer-force-complete-and-exit', which needs the full
  ;; completion set and is potentially slow and blocking.  Do the
  ;; latter if:
  (if (or
       ;; there's some input, meaning the default in off the table by
       ;; definition; OR
       (> (icomplete--field-end) (icomplete--field-beg))
       ;; there's no input, but there's also no minibuffer default
       ;; (and the user really wants to see completions on no input,
       ;; meaning he expects a "force" to be at least attempted); OR
       (and (not minibuffer-default)
            icomplete-show-matches-on-no-input)
       ;; there's no input but the full completion set has been
       ;; calculated, This causes the first cached completion to
       ;; be taken (i.e. the one that the user sees highlighted)
       completion-all-sorted-completions)
      (minibuffer-force-complete-and-exit)
    ;; Otherwise take the faster route...
    (minibuffer-complete-and-exit)))

(defun icomplete-force-complete ()
  "Complete the icomplete minibuffer."
  (interactive)
  ;; We're not at all interested in cycling here (bug#34077).
  (minibuffer-force-complete nil nil 'dont-cycle))

(defun icomplete-forward-completions ()
  "Step forward completions by one entry.
Second entry becomes the first and can be selected with
`icomplete-force-complete-and-exit'."
  (interactive)
  (let* ((beg (icomplete--field-beg))
         (end (icomplete--field-end))
         (comps (completion-all-sorted-completions beg end))
	 (last (last comps)))
    (when comps
      (setcdr last (cons (car comps) (cdr last)))
      (completion--cache-all-sorted-completions beg end (cdr comps)))))

(defun icomplete-backward-completions ()
  "Step backward completions by one entry.
Last entry becomes the first and can be selected with
`icomplete-force-complete-and-exit'."
  (interactive)
  (let* ((beg (icomplete--field-beg))
         (end (icomplete--field-end))
         (comps (completion-all-sorted-completions beg end))
	 (last-but-one (last comps 2))
	 (last (cdr last-but-one)))
    (when (consp last)		      ; At least two elements in comps
      (setcdr last-but-one (cdr last))
      (push (car last) comps)
      (completion--cache-all-sorted-completions beg end comps))))

;;; Helpers for `fido-mode' (or `ido-mode' emulation)
;;;
(defun icomplete-fido-kill ()
  "Kill line or current completion, like `ido-mode'.
If killing to the end of line make sense, call `kill-line',
otherwise kill the currently selected completion candidate.
Exactly what killing entails is dependent on the things being
completed.  If completing files, it means delete the file.  If
completing buffers it means kill the buffer.  Both actions
require user confirmation."
  (interactive)
  (let ((end (icomplete--field-end)))
    (if (< (point) end)
        (call-interactively 'kill-line)
      (let* ((all (completion-all-sorted-completions))
             (thing (car all))
             (action
              (pcase (icomplete--category)
                (`buffer
                 (lambda ()
                   (when (yes-or-no-p (concat "Kill buffer " thing "? "))
                     (kill-buffer thing))))
                (`file
                 (lambda ()
                   (let* ((dir (file-name-directory (icomplete--field-string)))
                          (path (expand-file-name thing dir)))
                     (when (yes-or-no-p (concat "Delete file " path "? "))
                       (delete-file path) t)))))))
        (when (let (;; Allow `yes-or-no-p' to work and don't let it
                    ;; `icomplete-exhibit' anything.
                    (enable-recursive-minibuffers t)
                    (icomplete-mode nil))
                (funcall action))
          (completion--cache-all-sorted-completions
           (icomplete--field-beg)
           (icomplete--field-end)
           (cdr all)))
        (message nil)))))

(defun icomplete-fido-delete-char ()
  "Delete char or maybe call `dired', like `ido-mode'."
  (interactive)
  (let ((end (icomplete--field-end)))
    (if (or (< (point) end) (not (eq (icomplete--category) 'file)))
        (call-interactively 'delete-char)
      (dired (file-name-directory (icomplete--field-string)))
      (exit-minibuffer))))

(defun icomplete-fido-ret ()
  "Exit minibuffer or enter directory, like `ido-mode'."
  (interactive)
  (let* ((dir (and (eq (icomplete--category) 'file)
                   (file-name-directory (icomplete--field-string))))
         (current (car completion-all-sorted-completions))
         (probe (and dir current
                     (expand-file-name (directory-file-name current) dir))))
    (cond ((and probe (file-directory-p probe) (not (string= current "./")))
           (icomplete-force-complete))
          (t
           (icomplete-force-complete-and-exit)))))

(defun icomplete-fido-exit (force)
  "Attempt to exit minibuffer immediately with current input.
Unless FORCE is non-nil (interactively with a prefix argument),
honour a non-nil REQUIRE-MATCH argument to `completing-read' by
trying to complete as much as possible and disallowing the exit
if that doesn't produce a completion match."
  (interactive "P")
  (if (and (not force) minibuffer--require-match)
      (minibuffer-complete-and-exit)
    (exit-minibuffer)))

(defun icomplete-fido-backward-updir ()
  "Delete char before or go up directory, like `ido-mode'."
  (interactive)
  (if (and (eq (char-before) ?/)
           (eq (icomplete--category) 'file))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))
    (call-interactively 'backward-delete-char)))

(defvar icomplete-fido-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k") 'icomplete-fido-kill)
    (define-key map (kbd "C-d") 'icomplete-fido-delete-char)
    (define-key map (kbd "RET") 'icomplete-fido-ret)
    (define-key map (kbd "C-m") 'icomplete-fido-ret)
    (define-key map (kbd "DEL") 'icomplete-fido-backward-updir)
    (define-key map (kbd "M-j") 'icomplete-fido-exit)
    (define-key map (kbd "C-s") 'icomplete-forward-completions)
    (define-key map (kbd "C-r") 'icomplete-backward-completions)
    (define-key map (kbd "<right>") 'icomplete-forward-completions)
    (define-key map (kbd "<left>") 'icomplete-backward-completions)
    (define-key map (kbd "C-.") 'icomplete-forward-completions)
    (define-key map (kbd "C-,") 'icomplete-backward-completions)
    map)
  "Keymap used by `fido-mode' in the minibuffer.")

(defun icomplete--fido-mode-setup ()
  "Setup `fido-mode''s minibuffer."
  (when (and icomplete-mode (icomplete-simple-completing-p))
    (use-local-map (make-composed-keymap icomplete-fido-mode-map
                                         (current-local-map)))
    (setq-local icomplete-tidy-shadowed-file-names t
                icomplete-show-matches-on-no-input t
                icomplete-hide-common-prefix nil
                completion-styles '(flex)
                completion-flex-nospace nil
                completion-category-defaults nil
                completion-ignore-case t
                read-buffer-completion-ignore-case t
                read-file-name-completion-ignore-case t)))

;;;###autoload
(define-minor-mode fido-mode
  "An enhanced `icomplete-mode' that emulates `ido-mode'.

This global minor mode makes minibuffer completion behave
more like `ido-mode' than regular `icomplete-mode'."
  :global t :group 'icomplete
  (remove-hook 'minibuffer-setup-hook #'icomplete-minibuffer-setup)
  (remove-hook 'minibuffer-setup-hook #'icomplete--fido-mode-setup)
  (when fido-mode
    (icomplete-mode -1)
    (setq icomplete-mode t)
    (add-hook 'minibuffer-setup-hook #'icomplete-minibuffer-setup)
    (add-hook 'minibuffer-setup-hook #'icomplete--fido-mode-setup)))

;;;_ > icomplete-mode (&optional prefix)
;;;###autoload
(define-minor-mode icomplete-mode
  "Toggle incremental minibuffer completion (Icomplete mode).

When this global minor mode is enabled, typing in the minibuffer
continuously displays a list of possible completions that match
the string you have typed.  See `icomplete-completions' for a
description of how prospective completions are displayed.

For more information, see Info node `(emacs)Icomplete'.
For options you can set, `\\[customize-group] icomplete'.

You can use the following key bindings to navigate and select
completions:

\\{icomplete-minibuffer-map}"
  :global t :group 'icomplete
  (remove-hook 'minibuffer-setup-hook #'icomplete-minibuffer-setup)
  (remove-hook 'completion-in-region-mode-hook #'icomplete--in-region-setup)
  (when icomplete-mode
    (fido-mode -1)
    (when icomplete-in-buffer
      (add-hook 'completion-in-region-mode-hook #'icomplete--in-region-setup))
    (add-hook 'minibuffer-setup-hook #'icomplete-minibuffer-setup)))

(defun icomplete--completion-table ()
  (if (window-minibuffer-p) minibuffer-completion-table
    (or (nth 2 completion-in-region--data)
	(message "In %S (w=%S): %S"
		 (current-buffer) (selected-window) (window-minibuffer-p)))))
(defun icomplete--completion-predicate ()
  (if (window-minibuffer-p) minibuffer-completion-predicate
    (nth 3 completion-in-region--data)))
(defun icomplete--field-string ()
  (if (window-minibuffer-p) (minibuffer-contents)
    (buffer-substring-no-properties
     (nth 0 completion-in-region--data)
     (nth 1 completion-in-region--data))))
(defun icomplete--field-beg ()
  (if (window-minibuffer-p) (minibuffer-prompt-end)
    (nth 0 completion-in-region--data)))
(defun icomplete--field-end ()
  (if (window-minibuffer-p) (point-max)
    (nth 1 completion-in-region--data)))
(defun icomplete--category ()
  (let* ((beg (icomplete--field-beg))
         (md (completion--field-metadata beg)))
    (alist-get 'category (cdr md))))

;;;_ > icomplete-simple-completing-p ()
(defun icomplete-simple-completing-p ()
  "Non-nil if current window is a minibuffer that's doing simple completion.

Conditions are:
   the selected window is a minibuffer,
   and not in the middle of macro execution,
   and the completion table is not a function (which would
       indicate some non-standard, non-simple completion mechanism,
       like file-name and other custom-func completions),
   and `icomplete-with-completion-tables' doesn't restrict completion."

  (unless executing-kbd-macro
    (let ((table (icomplete--completion-table)))
      (and table
           (or (not (functionp table))
               (eq icomplete-with-completion-tables t)
               (member table icomplete-with-completion-tables))))))

;;;_ > icomplete-minibuffer-setup ()
(defun icomplete-minibuffer-setup ()
  "Run in minibuffer on activation to establish incremental completion.
Usually run by inclusion in `minibuffer-setup-hook'."
  (when (and icomplete-mode (icomplete-simple-completing-p))
    (set (make-local-variable 'completion-show-inline-help) nil)
    (use-local-map (make-composed-keymap icomplete-minibuffer-map
    					 (current-local-map)))
    (add-hook 'pre-command-hook  #'icomplete-pre-command-hook  nil t)
    (add-hook 'post-command-hook #'icomplete-post-command-hook nil t)
    (run-hooks 'icomplete-minibuffer-setup-hook)))

(defvar icomplete--in-region-buffer nil)

(defun icomplete--in-region-setup ()
  (when (or (not completion-in-region-mode)
	    (and icomplete--in-region-buffer
		 (not (eq icomplete--in-region-buffer (current-buffer)))))
    (with-current-buffer (or icomplete--in-region-buffer (current-buffer))
      (setq icomplete--in-region-buffer nil)
      (delete-overlay icomplete-overlay)
      (kill-local-variable 'completion-show-inline-help)
      (remove-hook 'pre-command-hook  'icomplete-pre-command-hook  t)
      (remove-hook 'post-command-hook 'icomplete-post-command-hook t)
      (message nil)))
  (when (and completion-in-region-mode
	     icomplete-mode (icomplete-simple-completing-p))
    (setq icomplete--in-region-buffer (current-buffer))
    (set (make-local-variable 'completion-show-inline-help) nil)
    (let ((tem (assq 'completion-in-region-mode
		     minor-mode-overriding-map-alist)))
      (unless (memq icomplete-minibuffer-map (cdr tem))
	(setcdr tem (make-composed-keymap icomplete-minibuffer-map
					  (cdr tem)))))
    (add-hook 'pre-command-hook  'icomplete-pre-command-hook  nil t)
    (add-hook 'post-command-hook 'icomplete-post-command-hook nil t)))

(defun icomplete--sorted-completions ()
  (or completion-all-sorted-completions
      (cl-loop
       with beg = (icomplete--field-beg)
       with end = (icomplete--field-end)
       with all = (completion-all-sorted-completions beg end)
       for fn in (cond ((and minibuffer-default
                             (stringp minibuffer-default) ; bug#38992
                             (= (icomplete--field-end) (icomplete--field-beg)))
                        ;; When we have a non-nil string default and
                        ;; no input whatsoever: we want to make sure
                        ;; that default is bubbled to the top so that
                        ;; `icomplete-force-complete-and-exit' will
                        ;; select it (do that even if the match
                        ;; doesn't match the completion perfectly.
                        `(,(lambda (comp)
                             (equal minibuffer-default comp))
                          ,(lambda (comp)
                             (string-prefix-p minibuffer-default comp))))
                       ((and fido-mode
                             (not minibuffer-default)
                             (eq (icomplete--category) 'file))
                        ;; `fido-mode' has some extra file-sorting
                        ;; semantics even if there isn't a default,
                        ;; which is to bubble "./" to the top if it
                        ;; exists.  This makes M-x dired RET RET go to
                        ;; the directory of current file, which is
                        ;; what vanilla Emacs and `ido-mode' both do.
                        `(,(lambda (comp)
                             (string= "./" comp)))))
       thereis (cl-loop
                for l on all
                while (consp (cdr l))
                for comp = (cadr l)
                when (funcall fn comp)
                do (setf (cdr l) (cddr l))
                and return
                (completion--cache-all-sorted-completions beg end (cons comp all)))
       finally return all)))




;;;_* Completion

;;;_ > icomplete-tidy ()
(defun icomplete-tidy ()
  "Remove completions display (if any) prior to new user input.
Should be run in on the minibuffer `pre-command-hook'.
See `icomplete-mode' and `minibuffer-setup-hook'."
  (delete-overlay icomplete-overlay))

;;;_ > icomplete-exhibit ()
(defun icomplete-exhibit ()
  "Insert Icomplete completions display.
Should be run via minibuffer `post-command-hook'.
See `icomplete-mode' and `minibuffer-setup-hook'."
  (when (and icomplete-mode
             (icomplete-simple-completing-p)) ;Shouldn't be necessary.
    (let ((saved-point (point)))
      (save-excursion
        (goto-char (point-max))
                                        ; Insert the match-status information:
        (when (and (or icomplete-show-matches-on-no-input
                       (> (icomplete--field-end) (icomplete--field-beg)))
                   (or
                    ;; Don't bother with delay after certain number of chars:
                    (> (- (point) (icomplete--field-beg))
                       icomplete-max-delay-chars)
                    ;; Don't delay if the completions are known.
                    completion-all-sorted-completions
                    ;; Don't delay if alternatives number is small enough:
                    (and (sequencep (icomplete--completion-table))
                         (< (length (icomplete--completion-table))
                            icomplete-delay-completions-threshold))
                    ;; Delay - give some grace time for next keystroke, before
                    ;; embarking on computing completions:
                    (sit-for icomplete-compute-delay)))
          (when (and
                 icomplete-tidy-shadowed-file-names
                 (eq (icomplete--category) 'file)
                 rfn-eshadow-overlay (overlay-buffer rfn-eshadow-overlay)
                 (eq this-command 'self-insert-command)
                 (= saved-point (icomplete--field-end))
                 (or (>= (- (point) (overlay-end rfn-eshadow-overlay)) 2)
                     (eq ?/ (char-before (- (point) 2)))))
            (delete-region (overlay-start rfn-eshadow-overlay)
                           (overlay-end rfn-eshadow-overlay)) )
          (let* ((field-string (icomplete--field-string))
                 ;; Not sure why, but such requests seem to come
                 ;; every once in a while.  It's not fully
                 ;; deterministic but `C-x C-f M-DEL M-DEL ...'
                 ;; seems to trigger it fairly often!
                 (while-no-input-ignore-events '(selection-request))
                 (text (while-no-input
                         (icomplete-completions
                          field-string
                          (icomplete--completion-table)
                          (icomplete--completion-predicate)
                          (if (window-minibuffer-p)
                              (eq minibuffer--require-match t)))))
                 (buffer-undo-list t)
                 deactivate-mark)
            ;; Do nothing if while-no-input was aborted.
            (when (stringp text)
              (move-overlay icomplete-overlay (point) (point) (current-buffer))
              ;; The current C cursor code doesn't know to use the overlay's
              ;; marker's stickiness to figure out whether to place the cursor
              ;; before or after the string, so let's spoon-feed it the pos.
              (put-text-property 0 1 'cursor t text)
              (overlay-put icomplete-overlay 'after-string text))))))))

;;;_ > icomplete-completions (name candidates predicate require-match)
(defun icomplete-completions (name candidates predicate require-match)
  "Identify prospective candidates for minibuffer completion.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
one of (), [], or {} pairs.  The choice of brackets is as follows:

  (...) - a single prospect is identified and matching is enforced,
  [...] - a single prospect is identified but matching is optional, or
  {...} - multiple prospects, separated by commas, are indicated, and
          further input is required to distinguish a single one.

If there are multiple possibilities, `icomplete-separator' separates them.

The displays for unambiguous matches have ` [Matched]' appended
\(whether complete or not), or ` [No matches]', if no eligible
matches exist."
  (let* ((ignored-extension-re
          (and minibuffer-completing-file-name
               icomplete-with-completion-tables
               completion-ignored-extensions
               (concat "\\(?:\\`\\.\\./\\|"
                       (regexp-opt completion-ignored-extensions)
                       "\\)\\'")))
         (minibuffer-completion-table candidates)
	 (minibuffer-completion-predicate
          (if ignored-extension-re
              (lambda (cand)
                (and (not (string-match ignored-extension-re cand))
                     (or (null predicate)
                         (funcall predicate cand))))
            predicate))
	 (md (completion--field-metadata (icomplete--field-beg)))
	 (comps (icomplete--sorted-completions))
         (last (if (consp comps) (last comps)))
         (base-size (cdr last))
         (open-bracket (if require-match "(" "["))
         (close-bracket (if require-match ")" "]")))
    ;; `concat'/`mapconcat' is the slow part.
    (if (not (consp comps))
	(progn ;;(debug (format "Candidates=%S field=%S" candidates name))
	  (format " %sNo matches%s" open-bracket close-bracket))
      (if last (setcdr last nil))
      (let* ((most-try
              (if (and base-size (> base-size 0))
                  (completion-try-completion
                   name candidates predicate (length name) md)
                ;; If the `comps' are 0-based, the result should be
                ;; the same with `comps'.
                (completion-try-completion
                 name comps nil (length name) md)))
	     (most (if (consp most-try) (car most-try)
                     (if most-try (car comps) "")))
             ;; Compare name and most, so we can determine if name is
             ;; a prefix of most, or something else.
	     (compare (compare-strings name nil nil
				       most nil nil completion-ignore-case))
	     (ellipsis (if (char-displayable-p ?…) "…" "..."))
	     (determ (unless (or (eq t compare) (eq t most-try)
				 (= (setq compare (1- (abs compare)))
				    (length most)))
		       (concat open-bracket
			       (cond
				((= compare (length name))
                                 ;; Typical case: name is a prefix.
				 (substring most compare))
                                ;; Don't bother truncating if it doesn't gain
                                ;; us at least 2 columns.
				((< compare (+ 2 (string-width ellipsis))) most)
				(t (concat ellipsis (substring most compare))))
			       close-bracket)))
	     ;;"-prospects" - more than one candidate
	     (prospects-len (+ (string-width
				(or determ (concat open-bracket close-bracket)))
			       (string-width icomplete-separator)
			       (+ 2 (string-width ellipsis)) ;; take {…} into account
			       (string-width (buffer-string))))
             (prospects-max
              ;; Max total length to use, including the minibuffer content.
              (* (+ icomplete-prospects-height
                    ;; If the minibuffer content already uses up more than
                    ;; one line, increase the allowable space accordingly.
                    (/ prospects-len (window-width)))
                 (window-width)))
             ;; Find the common prefix among `comps'.
             ;; We can't use the optimization below because its assumptions
             ;; aren't always true, e.g. when completion-cycling (bug#10850):
             ;; (if (eq t (compare-strings (car comps) nil (length most)
             ;; 			 most nil nil completion-ignore-case))
             ;;     ;; Common case.
             ;;     (length most)
             ;; Else, use try-completion.
	     (prefix (when icomplete-hide-common-prefix
		       (try-completion "" comps)))
             (prefix-len
	      (and (stringp prefix)
                   ;; Only hide the prefix if the corresponding info
                   ;; is already displayed via `most'.
                   (string-prefix-p prefix most t)
                   (length prefix))) ;;)
	     prospects comp limit)
	(if (or (eq most-try t) (not (consp (cdr comps))))
	    (setq prospects nil)
	  (when (member name comps)
	    ;; NAME is complete but not unique.  This scenario poses
	    ;; following UI issues:
	    ;;
	    ;; - When `icomplete-hide-common-prefix' is non-nil, NAME
	    ;;   is stripped empty.  This would make the entry
	    ;;   inconspicuous.
	    ;;
	    ;; - Due to sorting of completions, NAME may not be the
	    ;;   first of the prospects and could be hidden deep in
	    ;;   the displayed string.
	    ;;
	    ;; - Because of `icomplete-prospects-height' , NAME may
	    ;;   not even be displayed to the user.
	    ;;
	    ;; To circumvent all the above problems, provide a visual
	    ;; cue to the user via an "empty string" in the try
	    ;; completion field.
	    (setq determ (concat open-bracket "" close-bracket)))
	  ;; Compute prospects for display.
	  (while (and comps (not limit))
	    (setq comp
		  (if prefix-len (substring (car comps) prefix-len) (car comps))
		  comps (cdr comps))
	    (setq prospects-len
                  (+ (string-width comp)
		     (string-width icomplete-separator)
		     prospects-len))
	    (if (< prospects-len prospects-max)
		(push comp prospects)
	      (setq limit t))))
	(setq prospects (nreverse prospects))
	;; Decorate first of the prospects.
	(when prospects
	  (let ((first (copy-sequence (pop prospects))))
	    (put-text-property 0 (length first)
			       'face 'icomplete-first-match first)
	    (push first prospects)))
        ;; Restore the base-size info, since completion-all-sorted-completions
        ;; is cached.
        (if last (setcdr last base-size))
	(if prospects
	    (concat determ
		    "{"
		    (mapconcat 'identity prospects icomplete-separator)
		    (and limit (concat icomplete-separator ellipsis))
		    "}")
	  (concat determ " [Matched]"))))))

;;; Iswitchb compatibility

;; We moved Iswitchb to `obsolete' in 24.4, but autoloads in files in
;; `obsolete' aren't obeyed (since that would encourage people to keep using
;; those packages, oblivious to their obsolescence).  Given the fact that
;; Iswitchb was very popular, we decided to keep its autoload for a bit longer,
;; so we moved it here.

;;;###autoload(when (locate-library "obsolete/iswitchb")
;;;###autoload  (autoload 'iswitchb-mode "iswitchb" "Toggle Iswitchb mode." t)
;;;###autoload  (make-obsolete 'iswitchb-mode
;;;###autoload    "use `icomplete-mode' or `ido-mode' instead." "24.4"))

;;;_* Provide
(provide 'icomplete)

;;_* Local emacs vars.
;;Local variables:
;;allout-layout: (-2 :)
;;End:

;;; icomplete.el ends here
