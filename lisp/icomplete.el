;;; icomplete.el --- minibuffer completion incremental feedback -*- lexical-binding: t -*-

;; Copyright (C) 1992-1994, 1997, 1999, 2001-2022 Free Software
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
(require 'simple) ; max-mini-window-lines
(require 'cl-lib)

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
  "When non-nil, show completions when first prompting for input.
This means to show completions even when the current minibuffer contents
is the same as was the initial input after minibuffer activation.
This also means that if you traverse the list of completions with
commands like `C-.' and just hit RET without typing any
characters, the match under point will be chosen instead of the
default."
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

(defcustom icomplete-matches-format "%s/%s "
  "Format of the current/total number of matches for the prompt prefix."
  :version "28.1"
  :type '(choice (const :tag "No prefix" nil)
                 (string :tag "Prefix format string")))

(defface icomplete-first-match '((t :weight bold))
  "Face used by Icomplete for highlighting first match."
  :version "24.4")

(defface icomplete-selected-match '((t :inherit highlight))
  "Face used by `icomplete-vertical-mode' for the selected candidate."
  :version "28.1")

(defface icomplete-section '((t :inherit shadow :slant italic))
  "Face used by `icomplete-vertical-mode' for the section title."
  :version "28.1")

;;;_* User Customization variables
(defcustom icomplete-prospects-height 2
  ;; We used to compute how many lines 100 characters would take in
  ;; the current window width, but the return value of `window-width'
  ;; is unreliable on startup (e.g., if we're in daemon mode), so now
  ;; we simply base the default value on an 80 column window.
  "Maximum number of lines to use in the minibuffer."
  :type 'integer
  :version "26.1")

(defcustom icomplete-compute-delay .15
  "Completions-computation stall, used only with large-number completions.
See `icomplete-delay-completions-threshold'."
  :type 'number)

(defcustom icomplete-delay-completions-threshold 400
  "Pending-completions number over which to apply `icomplete-compute-delay'."
  :type 'integer)

(defcustom icomplete-max-delay-chars 2
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

(defvar icomplete--initial-input nil
  "Initial input in the minibuffer when `icomplete-mode' was activated.
Used to implement the option `icomplete-show-matches-on-no-input'.")

(defun icomplete-post-command-hook ()
  (let ((non-essential t)) ;E.g. don't prompt for password!
    (icomplete-exhibit)))

(defvar icomplete-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\M-\t] 'icomplete-force-complete)
    (define-key map [remap minibuffer-complete-and-exit] 'icomplete-ret)
    (define-key map [?\C-j]  'icomplete-force-complete-and-exit)
    (define-key map [?\C-.]  'icomplete-forward-completions)
    (define-key map [?\C-,]  'icomplete-backward-completions)
    map)
  "Keymap used by `icomplete-mode' in the minibuffer.")

(defun icomplete-ret ()
  "Exit minibuffer for icomplete."
  (interactive)
  (if (and icomplete-show-matches-on-no-input
           (car completion-all-sorted-completions)
           (equal (icomplete--field-string) icomplete--initial-input))
      (icomplete-force-complete-and-exit)
    (minibuffer-complete-and-exit)))

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
       (not (equal (icomplete--field-string) icomplete--initial-input))
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

;; Apropos `icomplete-scroll', we implement "scrolling icomplete"
;; within classic icomplete, which is "rotating", by contrast.
;;
;; The two variables supporing this are
;; `icomplete--scrolled-completions' and `icomplete--scrolled-past'.
;; They come into play when:
;;
;; - The user invokes commands `icomplete-forward-completions' and
;;   `icomplete-backward-completions', thus "manually" scrolling to a
;;   given position;
;;
;; - The user re-filters a selection that had already been manually
;;   scrolled.  The system attempts to keep the previous selection
;;   stable in the face of the new filtering.  This is mostly done in
;;   `icomplete--render-vertical'.
;;
(defvar icomplete-scroll nil
  "If non-nil, scroll candidates list instead of rotating it.")
(defvar icomplete--scrolled-completions nil
  "If non-nil, tail of completions list manually scrolled to.")
(defvar icomplete--scrolled-past nil
  "If non-nil, reverse tail of completions scrolled past.")

(defun icomplete-forward-completions ()
  "Step forward completions by one entry.
Second entry becomes the first and can be selected with
`icomplete-force-complete-and-exit'.
Return non-nil iff something was stepped."
  (interactive)
  (let* ((beg (icomplete--field-beg))
         (end (icomplete--field-end))
         (comps (completion-all-sorted-completions beg end)))
    (when (consp (cdr comps))
      (cond (icomplete-scroll
             (push (pop comps) icomplete--scrolled-past)
             (setq icomplete--scrolled-completions comps))
            (t
             (let ((last (last comps)))
               (setcdr (last comps) (cons (pop comps) (cdr last))))))
      (completion--cache-all-sorted-completions beg end comps))))

(defun icomplete-backward-completions ()
  "Step backward completions by one entry.
Last entry becomes the first and can be selected with
`icomplete-force-complete-and-exit'.
Return non-nil iff something was stepped."
  (interactive)
  (let* ((beg (icomplete--field-beg))
         (end (icomplete--field-end))
         (comps (completion-all-sorted-completions beg end))
         last-but-one)
    (prog1
        (cond ((and icomplete-scroll icomplete--scrolled-past)
               (push (pop icomplete--scrolled-past) comps)
               (setq icomplete--scrolled-completions comps))
              ((and (not icomplete-scroll)
                    (consp (cdr (setq last-but-one (last comps 2)))))
               ;; At least two elements in comps
               (push (car (cdr last-but-one)) comps)
               (setcdr last-but-one (cdr (cdr last-but-one)))))
      (completion--cache-all-sorted-completions beg end comps))))

(defun icomplete-vertical-goto-first ()
  "Go to first completions entry when `icomplete-scroll' is non-nil."
  (interactive)
  (unless icomplete-scroll (error "Only works with `icomplete-scroll'"))
  (while (icomplete-backward-completions)))

(defun icomplete-vertical-goto-last ()
  "Go to last completions entry when `icomplete-scroll' is non-nil."
  (interactive)
  (unless icomplete-scroll (error "Only works with `icomplete-scroll'"))
  (while (icomplete-forward-completions)))

;;;_* Helpers for `fido-mode' (or `ido-mode' emulation)

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
             (cat (icomplete--category))
             (action
              (cl-case cat
                (buffer
                 (lambda ()
                   (when (yes-or-no-p (concat "Kill buffer " thing "? "))
                     (kill-buffer thing))))
                ((project-file file)
                 (lambda ()
                   (let* ((dir (file-name-directory (icomplete--field-string)))
                          (path (expand-file-name thing dir)))
                     (when (yes-or-no-p (concat "Delete file " path "? "))
                       (delete-file path) t))))
                (t
                 (error "Sorry, don't know how to kill things for `%s'" cat)))))
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
                     (expand-file-name (directory-file-name current)
                                       (substitute-env-vars dir)))))
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
                icomplete-scroll (not (null icomplete-vertical-mode))
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
    (setq-local icomplete--initial-input (icomplete--field-string))
    (setq-local completion-show-inline-help nil)
    (setq icomplete--scrolled-completions nil)
    (use-local-map (make-composed-keymap icomplete-minibuffer-map
    					 (current-local-map)))
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
      (remove-hook 'post-command-hook 'icomplete-post-command-hook t)
      (message nil)))
  (when (and completion-in-region-mode
	     icomplete-mode (icomplete-simple-completing-p))
    (setq icomplete--in-region-buffer (current-buffer))
    (setq-local completion-show-inline-help nil)
    (let ((tem (assq 'completion-in-region-mode
		     minor-mode-overriding-map-alist)))
      (unless (memq icomplete-minibuffer-map (cdr tem))
	(setcdr tem (make-composed-keymap icomplete-minibuffer-map
					  (cdr tem)))))
    (add-hook 'post-command-hook 'icomplete-post-command-hook nil t)))

(defun icomplete--sorted-completions ()
  (or completion-all-sorted-completions
      (cl-loop
       initially (setq icomplete--scrolled-past nil) ; Invalidate scrolled state
       with beg = (icomplete--field-beg)
       with end = (icomplete--field-end)
       with all = (completion-all-sorted-completions beg end)
       ;; Icomplete mode re-sorts candidates, bubbling the default to
       ;; top if it's found somewhere down the list.  This loop's
       ;; iteration variable, `fn' iterates through these "bubble up
       ;; predicates" which may vary depending on specific
       ;; `completing-read' invocations, described below:
       for fn in (cond ((and minibuffer-default
                             (stringp minibuffer-default) ; bug#38992
                             (equal (icomplete--field-string) icomplete--initial-input))
                        ;; Here, we have a non-nil string default and
                        ;; no input whatsoever.  We want to make sure
                        ;; that the default is bubbled to the top so
                        ;; that `icomplete-force-complete-and-exit'
                        ;; will select it.  We want to do that even if
                        ;; the match doesn't match the completion
                        ;; perfectly.
                        ;;
                        `(;; The first predicate ensures that:
                          ;;
                          ;; (completing-read "thing? " '("foo" "bar")
                          ;;                  nil nil nil nil "bar")
                          ;;
                          ;; Has "bar" at the top, so RET will select
                          ;; it, as desired.
                          ,(lambda (comp)
                             (equal minibuffer-default comp))
                          ;; Why do we need this second predicate?
                          ;; Because that'll make things like M-x man
                          ;; RET RET, when invoked with point on the
                          ;; "bar" word, behave correctly.  There, the
                          ;; default doesn't quite match any
                          ;; candidate. So:
                          ;;
                          ;; (completing-read "Man entry? " '("foo(1)" "bar(1)")
                          ;;                  nil nil nil nil "bar")
                          ;;
                          ;; Will place "bar(1)" on top, and RET will
                          ;; select it -- again, as desired.
                          ;;
                          ;; FIXME: it's arguable that this second
                          ;; behaviour should be a property of the
                          ;; completion table and not the completion
                          ;; frontend such as we have done
                          ;; here. However, it seems generically
                          ;; useful for a very broad spectrum of
                          ;; cases.
                          ,(lambda (comp)
                             (string-prefix-p minibuffer-default comp))))
                       ((and fido-mode
                             (not minibuffer-default)
                             (eq (icomplete--category) 'file))
                        ;; When there isn't a default, `fido-mode'
                        ;; specifically also has some extra
                        ;; file-sorting semantics inherited from Ido.
                        ;; Those make the directory "./" bubble to the
                        ;; top (if it exists).  This makes M-x dired
                        ;; RET RET go to the directory of current
                        ;; file, which is non-Icomplete vanilla Emacs
                        ;; and `ido-mode' both do.
                        `(,(lambda (comp)
                             (string= "./" comp)))))
       ;; After we have setup the predicates, look for a completion
       ;; matching one of them and bubble up it, destructively on
       ;; `completion-all-sorted-completions' (unless that completion
       ;; happens to be already on top).
       thereis (or
                (and (funcall fn (car all)) all)
                (cl-loop
                 for l on all
                 while (consp (cdr l))
                 for comp = (cadr l)
                 when (funcall fn comp)
                 do (setf (cdr l) (cddr l))
                 and return
                 (completion--cache-all-sorted-completions beg end (cons comp all))))
       finally return all)))

(defvar icomplete-vertical-mode-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") 'icomplete-forward-completions)
    (define-key map (kbd "C-p") 'icomplete-backward-completions)
    (define-key map (kbd "<down>") 'icomplete-forward-completions)
    (define-key map (kbd "<up>") 'icomplete-backward-completions)
    (define-key map (kbd "M-<") 'icomplete-vertical-goto-first)
    (define-key map (kbd "M->") 'icomplete-vertical-goto-last)
    map)
  "Keymap used by `icomplete-vertical-mode' in the minibuffer.")

(defun icomplete--vertical-minibuffer-setup ()
  "Setup the minibuffer for vertical display of completion candidates."
  (use-local-map (make-composed-keymap icomplete-vertical-mode-minibuffer-map
                                       (current-local-map)))
  (setq-local icomplete-hide-common-prefix nil
              ;; Ask `icomplete-completions' to return enough completions candidates.
              icomplete-prospects-height 25
              redisplay-adhoc-scroll-in-resize-mini-windows nil))

;;;###autoload
(define-minor-mode icomplete-vertical-mode
  "Toggle vertical candidate display in `icomplete-mode' or `fido-mode'.

If none of these modes are on, turn on `icomplete-mode'.

As many completion candidates as possible are displayed, depending on
the value of `max-mini-window-height', and the way the mini-window is
resized depends on `resize-mini-windows'."
  :global t
  (remove-hook 'icomplete-minibuffer-setup-hook
               #'icomplete--vertical-minibuffer-setup)
  (when icomplete-vertical-mode
    (unless icomplete-mode
      (icomplete-mode 1))
    (add-hook 'icomplete-minibuffer-setup-hook
              #'icomplete--vertical-minibuffer-setup)))

;;;###autoload
(define-minor-mode fido-vertical-mode
  "Toggle vertical candidate display in `fido-mode'.
When turning on, if non-vertical `fido-mode' is off, turn it on.
If it's on, just add the vertical display."
  :global t
  (icomplete-vertical-mode -1)
  (when fido-vertical-mode
    (unless fido-mode (fido-mode 1))
    (icomplete-vertical-mode 1)))




;;;_* Completion

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
                       (not (equal (icomplete--field-string)
                                   icomplete--initial-input)))
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
                           (overlay-end rfn-eshadow-overlay)))
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
              (move-overlay icomplete-overlay (point-min) (point) (current-buffer))
              ;; The current C cursor code doesn't know to use the overlay's
              ;; marker's stickiness to figure out whether to place the cursor
              ;; before or after the string, so let's spoon-feed it the pos.
              (put-text-property 0 1 'cursor t text)
              (overlay-put
               icomplete-overlay 'before-string
               (and icomplete-scroll
                    icomplete-matches-format
                    (let* ((past (length icomplete--scrolled-past))
                           (current (1+ past))
                           (total (+ past (safe-length
                                           completion-all-sorted-completions))))
                      (format icomplete-matches-format current total))))
              (overlay-put icomplete-overlay 'after-string text))))))))

(defun icomplete--augment (md prospects)
  "Augment completion strings in PROSPECTS with completion metadata MD.
Return a list of strings (COMP PREFIX SUFFIX SECTION).  PREFIX
and SUFFIX, if non-nil, are obtained from `affixation-function' or
`annotation-function' metadata.  SECTION is obtained from
`group-function'.  Consecutive `equal' sections are avoided.
COMP is the element in PROSPECTS or a transformation also given
by `group-function''s second \"transformation\" protocol."
  (let* ((aff-fun (or (completion-metadata-get md 'affixation-function)
                      (plist-get completion-extra-properties :affixation-function)))
         (ann-fun (or (completion-metadata-get md 'annotation-function)
                      (plist-get completion-extra-properties :annotation-function)))
         (grp-fun (and completions-group
                       (completion-metadata-get md 'group-function)))
         (annotated
          (cond (aff-fun
           (funcall aff-fun prospects))
          (ann-fun
           (mapcar
            (lambda (comp)
              (let ((suffix (or (funcall ann-fun comp) "")))
                (list comp ""
                      ;; The default completion UI adds the
                      ;; `completions-annotations' face if no
                      ;; other faces are present.
                      (if (text-property-not-all 0 (length suffix) 'face nil suffix)
                          suffix
                        (propertize suffix 'face 'completions-annotations)))))
            prospects))
          (t (mapcar #'list prospects)))))
    (if grp-fun
        (cl-loop with section = nil
                 for (c prefix suffix) in annotated
                 for selectedp = (get-text-property 0 'icomplete-selected c)
                 for tr = (propertize (or (funcall grp-fun c t) c)
                                      'icomplete-selected selectedp)
                 if (not (equal section (setq section (funcall grp-fun c nil))))
                 collect (list tr prefix suffix section)
                 else collect (list tr prefix suffix ))
      annotated)))

(cl-defun icomplete--render-vertical
    (comps md &aux scroll-above scroll-below
           (total-space ; number of mini-window lines available
            (1- (min
                 icomplete-prospects-height
                 (truncate (max-mini-window-lines) 1)))))
  ;; Welcome to loopapalooza!
  ;;
  ;; First, be mindful of `icomplete-scroll' and manual scrolls.  If
  ;; `icomplete--scrolled-completions' and `icomplete--scrolled-past'
  ;; are:
  ;;
  ;; - both nil, there is no manual scroll;
  ;; - both non-nil, there is a healthy manual scroll that doesn't need
  ;;   to be readjusted (user just moved around the minibuffer, for
  ;;   example)l
  ;; - non-nil and nil, respectively, a refiltering took place and we
  ;;   may need to readjust them to the new filtered `comps'.
  (when (and icomplete-scroll
             icomplete--scrolled-completions
             (null icomplete--scrolled-past))
    (cl-loop with preds
             for (comp . rest) on comps
             when (equal comp (car icomplete--scrolled-completions))
             do
             (setq icomplete--scrolled-past preds
                   comps (cons comp rest))
             (completion--cache-all-sorted-completions
              (icomplete--field-beg)
              (icomplete--field-end)
              comps)
             and return nil
             do (push comp preds)
             finally (setq icomplete--scrolled-completions nil)))
  ;; Then, in this pretty ugly loop, collect completions to display
  ;; above and below the selected one, considering scrolling
  ;; positions.
  (cl-loop with preds = icomplete--scrolled-past
           with succs = (cdr comps)
           with space-above = (- total-space
                                 1
                                 (cl-loop for (_ . r) on comps
                                          repeat (truncate total-space 2)
                                          while (listp r)
                                          count 1))
           repeat total-space
           for neighbour = nil
           if (and preds (> space-above 0)) do
           (push (setq neighbour (pop preds)) scroll-above)
           (cl-decf space-above)
           else if (consp succs) collect
           (setq neighbour (pop succs)) into scroll-below-aux
           while neighbour
           finally (setq scroll-below scroll-below-aux))
  ;; Halfway there...
  (let* ((selected (propertize (car comps) 'icomplete-selected t))
         (chosen (append scroll-above (list selected) scroll-below))
         (tuples (icomplete--augment md chosen))
         max-prefix-len max-comp-len lines nsections)
    (add-face-text-property 0 (length selected)
                            'icomplete-selected-match 'append selected)
    ;; Figure out parameters for horizontal spacing
    (cl-loop
     for (comp prefix) in tuples
     maximizing (length prefix) into max-prefix-len-aux
     maximizing (length comp) into max-comp-len-aux
     finally (setq max-prefix-len max-prefix-len-aux
                   max-comp-len max-comp-len-aux))
    ;; Serialize completions and section titles into a list
    ;; of lines to render
    (cl-loop
     for (comp prefix suffix section) in tuples
     when section
     collect (propertize section 'face 'icomplete-section) into lines-aux
     and count 1 into nsections-aux
     when (get-text-property 0 'icomplete-selected comp)
     do (add-face-text-property 0 (length comp)
                                'icomplete-selected-match 'append comp)
     collect (concat prefix
                     (make-string (- max-prefix-len (length prefix)) ? )
                     comp
                     (make-string (- max-comp-len (length comp)) ? )
                     suffix)
     into lines-aux
     finally (setq lines lines-aux
                   nsections nsections-aux))
    ;; Kick out some lines from the beginning due to extra sections.
    ;; This hopes to keep the selected entry more or less in the
    ;; middle of the dropdown-like widget when `icomplete-scroll' is
    ;; t.  Funky, but at least I didn't use `cl-loop'
    (setq lines
          (nthcdr
           (cond ((<= (length lines) total-space) 0)
                 ((> (length scroll-above) (length scroll-below)) nsections)
                 (t (min (ceiling nsections 2) (length scroll-above))))
           lines))
    ;; At long last, render final string return value.  This may still
    ;; kick out lines at the end.
    (concat " \n"
            (cl-loop for l in lines repeat total-space concat l concat "\n"))))

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
         (open-bracket (if require-match "(" "["))
         (close-bracket (if require-match ")" "]")))
    ;; `concat'/`mapconcat' is the slow part.
    (if (not (consp comps))
	(progn ;;(debug (format "Candidates=%S field=%S" candidates name))
	  (format " %sNo matches%s" open-bracket close-bracket))
      (if icomplete-vertical-mode
	  (icomplete--render-vertical comps md)
        (let* ((last (if (consp comps) (last comps)))
               ;; Save the "base size" encoded in `comps' then
               ;; removing making `comps' a proper list.
               (base-size (prog1 (cdr last)
                            (if last (setcdr last nil))))
               (most-try
                ;; icomplete-hide-common-prefix logic is used
                ;; unconditionally when there is single match.
                (when (or icomplete-hide-common-prefix (not (cdr comps)))
                  (if (and base-size (> base-size 0))
                      (completion-try-completion
                       name candidates predicate (length name) md)
                    ;; If the `comps' are 0-based, the result should be
                    ;; the same with `comps'.
                    (completion-try-completion
                     name comps nil (length name) md))))
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
               ;;                        most nil nil completion-ignore-case))
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
          (prog1
              (if (or (eq most-try t) (and (not icomplete-scroll)
                                           (not (consp (cdr comps)))))
                  (concat determ " [Matched]")
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
                    (setq limit t)))
                (setq prospects (nreverse prospects))
                ;; Decorate first of the prospects.
                (when prospects
                  (let ((first (copy-sequence (pop prospects))))
                    (put-text-property 0 (length first)
                                       'face 'icomplete-first-match first)
                    (push first prospects)))
                (concat determ
                        "{"
                        (mapconcat 'identity prospects icomplete-separator)
                        (concat (and limit (concat icomplete-separator ellipsis))
                                "}")))
            ;; Restore the base-size info, since completion-all-sorted-completions
            ;; is cached.
            (if last (setcdr last base-size))))))))

;;;_* Iswitchb compatibility

;; We moved Iswitchb to `obsolete' in 24.4, but autoloads in files in
;; `obsolete' aren't obeyed (since that would encourage people to keep using
;; those packages, oblivious to their obsolescence).  Given the fact that
;; Iswitchb was very popular, we decided to keep its autoload for a bit longer,
;; so we moved it here.

;;;###autoload(when (locate-library "obsolete/iswitchb")
;;;###autoload  (autoload 'iswitchb-mode "iswitchb" "Toggle Iswitchb mode." t)
;;;###autoload  (make-obsolete 'iswitchb-mode
;;;###autoload    "use `icomplete-mode' or `ido-mode' instead." "24.4"))

(provide 'icomplete)

;;;_* Local emacs vars.
;;Local variables:
;;allout-layout: (-2 :)
;;End:

;;; icomplete.el ends here
