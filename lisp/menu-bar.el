;;; menu-bar.el --- define a default menu bar.

;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

;; Author: RMS
;; Maintainer: FSF
;; Keywords: internal

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

;; Avishai Yacobi suggested some menu rearrangements.

;;; Code:

;;; User options:

(defcustom buffers-menu-max-size 10
  "*Maximum number of entries which may appear on the Buffers menu.
If this is 10, then only the ten most-recently-selected buffers are shown.
If this is nil, then all buffers are shown.
A large number or nil slows down menu responsiveness."
  :type '(choice integer
		 (const :tag "All" nil))
  :group 'mouse)

;; Don't clobber an existing menu-bar keymap, to preserve any menu-bar key
;; definitions made in loaddefs.el.
(or (lookup-key global-map [menu-bar])
    (define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))
(defvar menu-bar-help-menu (make-sparse-keymap "Help"))

;; Force Help item to come last, after the major mode's own items.
;; The symbol used to be called `help', but that gets confused with the
;; help key.
(setq menu-bar-final-items '(help-menu))

(define-key global-map [menu-bar help-menu] (cons "Help" menu-bar-help-menu))
(defvar menu-bar-search-menu (make-sparse-keymap "Search"))
(define-key global-map [menu-bar search] (cons "Search" menu-bar-search-menu))
(defvar menu-bar-edit-menu (make-sparse-keymap "Edit"))
(define-key global-map [menu-bar edit] (cons "Edit" menu-bar-edit-menu))
(defvar menu-bar-tools-menu (make-sparse-keymap "Tools"))
(define-key global-map [menu-bar tools] (cons "Tools" menu-bar-tools-menu))
(defvar menu-bar-files-menu (make-sparse-keymap "Files"))
(define-key global-map [menu-bar files] (cons "Files" menu-bar-files-menu))

;; This alias is for compatibility with 19.28 and before.
(defvar menu-bar-file-menu menu-bar-files-menu)

(defvar vc-menu-map (make-sparse-keymap "Version Control"))

(define-key menu-bar-tools-menu [gdb] '("Debugger..." . gdb))
(define-key menu-bar-tools-menu [compile] '("Compile..." . compile))
(define-key menu-bar-tools-menu [grep] '("Search Files..." . grep))

(define-key menu-bar-tools-menu [separator-1]
  '("--"))

(define-key menu-bar-tools-menu [calendar] '("Display Calendar" . calendar))
(define-key menu-bar-tools-menu [speedbar]
  '("Display Speedbar" . speedbar-frame-mode))
(define-key menu-bar-tools-menu [compose-mail] '("Send Mail" . compose-mail))
(define-key menu-bar-tools-menu [rmail] '("Read Mail" . rmail))
(define-key menu-bar-tools-menu [gnus] '("Read Net News" . gnus))

(define-key menu-bar-tools-menu [separator-vc]
  '("--"))

(define-key menu-bar-tools-menu [vc]
  (cons "Version Control" vc-menu-map))

(define-key menu-bar-tools-menu [separator-compare]
  '("--"))

(define-key menu-bar-tools-menu [ediff-misc]
  '("Ediff Miscellanea" . menu-bar-ediff-misc-menu))
(define-key menu-bar-tools-menu [epatch]
  '("Apply Patch" . menu-bar-epatch-menu))
(define-key menu-bar-tools-menu [ediff-merge]
  '("Merge" . menu-bar-ediff-merge-menu))
(define-key menu-bar-tools-menu [compare]
  '("Compare" . menu-bar-ediff-menu))

(define-key menu-bar-tools-menu [separator-print]
  '("--"))

(defvar menu-bar-print-menu (make-sparse-keymap "Print"))

(define-key menu-bar-print-menu [ps-print-region]
  '("Postscript Print Region" . ps-print-region-with-faces))
(define-key menu-bar-print-menu [ps-print-buffer]
  '("Postscript Print Buffer" . ps-print-buffer-with-faces))
(define-key menu-bar-print-menu [separator-ps-print]
  '("--"))
(define-key menu-bar-print-menu [print-region]
  '("Print Region" . print-region))
(define-key menu-bar-print-menu [print-buffer]
  '("Print Buffer" . print-buffer))

(define-key menu-bar-tools-menu [print]
  (cons "Print" menu-bar-print-menu))

(put 'print-region 'menu-enable 'mark-active)
(put 'ps-print-region-with-faces 'menu-enable 'mark-active)

(define-key menu-bar-files-menu [exit-emacs]
  '("Exit Emacs" . save-buffers-kill-emacs))

(define-key menu-bar-files-menu [separator-exit]
  '("--"))

(define-key menu-bar-files-menu [one-window]
  '("One Window" . delete-other-windows))

(define-key menu-bar-files-menu [split-window]
  '("Split Window" . split-window-vertically))

(if (fboundp 'delete-frame)
    (progn
      ;; Don't use delete-frame as event name
      ;; because that is a special event.
      (define-key menu-bar-files-menu [delete-this-frame]
	'("Delete Frame" . delete-frame))
      (define-key menu-bar-files-menu [make-frame-on-display]
	'("Open New Display..." . make-frame-on-display))
      (define-key menu-bar-files-menu [make-frame]
	'("Make New Frame" . make-frame-command))))

(define-key menu-bar-files-menu [separator-buffers]
  '("--"))

(define-key menu-bar-files-menu [kill-buffer]
  '("Kill Current Buffer" . kill-this-buffer))
(define-key menu-bar-files-menu [insert-file]
  '("Insert File..." . insert-file))
(define-key menu-bar-files-menu [recover-session]
  '("Recover Session..." . recover-session))
(define-key menu-bar-files-menu [revert-buffer]
  '("Revert Buffer" . revert-buffer))
(define-key menu-bar-files-menu [write-file]
  '("Save Buffer As..." . write-file))
(define-key menu-bar-files-menu [save-buffer] '("Save Buffer" . save-buffer))
(define-key menu-bar-files-menu [dired] '("Open Directory..." . dired))
(define-key menu-bar-files-menu [open-file] '("Open File..." . find-file))

(put 'recover-session 'menu-enable
     '(and auto-save-list-file-prefix
	   (directory-files
	    (file-name-directory auto-save-list-file-prefix)
	    nil
	    (concat "\\`"
		    (regexp-quote (file-name-nondirectory
				   auto-save-list-file-prefix)))
	    t)))

(defun nonincremental-search-forward (string)
  "Read a string and search for it nonincrementally."
  (interactive "sSearch for string: ")
  (if (equal string "")
      (search-forward (car search-ring))
    (isearch-update-ring string nil)
    (search-forward string)))

(defun nonincremental-search-backward (string)
  "Read a string and search backward for it nonincrementally."
  (interactive "sSearch for string: ")
  (if (equal string "")
      (search-backward (car search-ring))
    (isearch-update-ring string nil)
    (search-backward string)))

(defun nonincremental-re-search-forward (string)
  "Read a regular expression and search for it nonincrementally."
  (interactive "sSearch for regexp: ")
  (if (equal string "")
      (re-search-forward (car regexp-search-ring))
    (isearch-update-ring string t)
    (re-search-forward string)))

(defun nonincremental-re-search-backward (string)
  "Read a regular expression and search backward for it nonincrementally."
  (interactive "sSearch for regexp: ")
  (if (equal string "")
      (re-search-backward (car regexp-search-ring))
    (isearch-update-ring string t)
    (re-search-backward string)))

(defun nonincremental-repeat-search-forward ()
  "Search forward for the previous search string."
  (interactive)
  (if (null search-ring)
      (error "No previous search"))
  (search-forward (car search-ring)))

(defun nonincremental-repeat-search-backward ()
  "Search backward for the previous search string."
  (interactive)
  (if (null search-ring)
      (error "No previous search"))
  (search-backward (car search-ring)))

(defun nonincremental-repeat-re-search-forward ()
  "Search forward for the previous regular expression."
  (interactive)
  (if (null regexp-search-ring)
      (error "No previous search"))
  (re-search-forward (car regexp-search-ring)))

(defun nonincremental-repeat-re-search-backward ()
  "Search backward for the previous regular expression."
  (interactive)
  (if (null regexp-search-ring)
      (error "No previous search"))
  (re-search-backward (car regexp-search-ring)))

(define-key menu-bar-search-menu [query-replace-regexp]
  '("Query Replace Regexp..." . query-replace-regexp))
(define-key menu-bar-search-menu [query-replace]
  '("Query Replace..." . query-replace))
(define-key menu-bar-search-menu [find-tag]
  '("Find Tag..." . find-tag))
(define-key menu-bar-search-menu [bookmark]
  '("Bookmarks" . menu-bar-bookmark-map))

(define-key menu-bar-search-menu [separator-search]
  '("--"))

(define-key menu-bar-search-menu [repeat-regexp-back]
  '("Repeat Regexp Backwards" . nonincremental-repeat-re-search-backward))
(define-key menu-bar-search-menu [repeat-search-back]
  '("Repeat Backwards" . nonincremental-repeat-search-backward))
(define-key menu-bar-search-menu [repeat-regexp-fwd]
  '("Repeat Regexp" . nonincremental-repeat-re-search-forward))
(define-key menu-bar-search-menu [repeat-search-fwd]
  '("Repeat Search" . nonincremental-repeat-search-forward))

(define-key menu-bar-search-menu [separator-repeat]
  '("--"))

(define-key menu-bar-search-menu [re-search-backward]
  '("Regexp Search Backwards..." . nonincremental-re-search-backward))
(define-key menu-bar-search-menu [search-backward]
  '("Search Backwards..." . nonincremental-search-backward))
(define-key menu-bar-search-menu [re-search-forward]
  '("Regexp Search..." . nonincremental-re-search-forward))
(define-key menu-bar-search-menu [search-forward]
  '("Search..." . nonincremental-search-forward))

(define-key menu-bar-edit-menu [spell] '("Spell" . ispell-menu-map))
(define-key menu-bar-edit-menu [fill] '("Fill" . fill-region))
(define-key menu-bar-edit-menu [props] '("Text Properties" . facemenu-menu))

(define-key menu-bar-edit-menu [separator-edit]
  '("--"))

(define-key menu-bar-edit-menu [clear] '("Clear" . delete-region))

(defvar yank-menu (cons "Select Yank" nil))
(fset 'yank-menu (cons 'keymap yank-menu))
(define-key menu-bar-edit-menu [select-paste] '("Select and Paste" . yank-menu))
(define-key menu-bar-edit-menu [paste] '("Paste" . yank))
(define-key menu-bar-edit-menu [copy] '("Copy" . menu-bar-kill-ring-save))
(define-key menu-bar-edit-menu [cut] '("Cut" . kill-region))
(define-key menu-bar-edit-menu [undo] '("Undo" . undo))

(defun menu-bar-kill-ring-save (beg end)
  (interactive "r")
  (if (mouse-region-match)
      (message "Selecting a region with the mouse does `copy' automatically")
    (kill-ring-save beg end)))

(put 'fill-region 'menu-enable '(and mark-active (not buffer-read-only)))
(put 'kill-region 'menu-enable '(and mark-active (not buffer-read-only)))
(put 'menu-bar-kill-ring-save 'menu-enable 'mark-active)
(put 'yank 'menu-enable '(and (x-selection-exists-p) (not buffer-read-only)))
(put 'yank-menu 'menu-enable '(and (cdr yank-menu) (not buffer-read-only)))
(put 'delete-region 'menu-enable '(and mark-active
				       (not buffer-read-only)
				       (not (mouse-region-match))))
(put 'undo 'menu-enable '(and (not buffer-read-only)
			      (if (eq last-command 'undo)
				  pending-undo-list
				(consp buffer-undo-list))))
(put 'query-replace 'menu-enable '(not buffer-read-only))
(put 'query-replace-regexp 'menu-enable '(not buffer-read-only))

(autoload 'ispell-menu-map "ispell" nil t 'keymap)

;; These are alternative definitions for the cut, paste and copy
;; menu items.  Use them if your system expects these to use the clipboard.

(put 'clipboard-kill-region 'menu-enable 'mark-active)
(put 'clipboard-kill-ring-save 'menu-enable 'mark-active)
(put 'clipboard-yank 'menu-enable
     '(or (x-selection-exists-p) (x-selection-exists-p 'CLIPBOARD)))

(defun clipboard-yank ()
  "Insert the clipboard contents, or the last stretch of killed text."
  (interactive)
  (let ((x-select-enable-clipboard t))
    (yank)))

(defun clipboard-kill-ring-save (beg end)
  "Copy region to kill ring, and save in the X clipboard."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (kill-ring-save beg end)))

(defun clipboard-kill-region (beg end)
  "Kill the region, and save it in the X clipboard."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (kill-region beg end)))

(defun menu-bar-enable-clipboard ()
  "Make CUT, PASTE and COPY (keys and menu bar items) use the clipboard.
Do the same for the keys of the same name."
  (interactive)
  ;; We can't use constant list structure here because it becomes pure,
  ;; and because it gets modified with cache data.
  (define-key menu-bar-edit-menu [paste]
    (cons "Paste" 'clipboard-yank))
  (define-key menu-bar-edit-menu [copy]
    (cons "Copy" 'clipboard-kill-ring-save))
  (define-key menu-bar-edit-menu [cut]
    (cons "Cut" 'clipboard-kill-region))

  (define-key global-map [f20] 'clipboard-kill-region)
  (define-key global-map [f16] 'clipboard-kill-ring-save)
  (define-key global-map [f18] 'clipboard-yank)
  ;; X11R6 versions
  (define-key global-map [cut] 'clipboard-kill-region)
  (define-key global-map [copy] 'clipboard-kill-ring-save)
  (define-key global-map [paste] 'clipboard-yank))


;;; Menu support

(defvar menu-bar-custom-menu (make-sparse-keymap "Customize"))

(define-key menu-bar-custom-menu [customize-apropos-groups]
  '("Apropos Groups..." . customize-apropos-groups))
(define-key menu-bar-custom-menu [customize-apropos-faces]
  '("Apropos Faces..." . customize-apropos-faces))
(define-key menu-bar-custom-menu [customize-apropos-options]
  '("Apropos Options..." . customize-apropos-options))
(define-key menu-bar-custom-menu [customize-apropos]
  '("Apropos..." . customize-apropos))
(define-key menu-bar-custom-menu [separator-2]
  '("--"))
(define-key menu-bar-custom-menu [customize-group]
  '("Specific Group..." . customize-group))
(define-key menu-bar-custom-menu [customize-face]
  '("Specific Face..." . customize-face))
(define-key menu-bar-custom-menu [customize-option]
  '("Specific Option..." . customize-option))
(define-key menu-bar-custom-menu [customize-changed-options]
  '("Changed Options..." . customize-changed-options))
(define-key menu-bar-custom-menu [separator-3]
  '("--"))
(define-key menu-bar-custom-menu [customize-browse]
  '("Browse Customization Groups" . customize-browse))
(define-key menu-bar-custom-menu [customize]
  '("Top-level Customization Group" . customize))

;; Options menu
(defvar menu-bar-options-menu (make-sparse-keymap "Global Options"))

(defmacro menu-bar-make-toggle (name variable doc message &rest body)
  `(progn
     (defun ,name ()
       ,(concat doc ".")
       (interactive)
       (if ,(if body `(progn . ,body)
	      `(setq ,variable (not ,variable)))
	   (message ,message "enabled")
	 (message ,message "disabled")))
     '(menu-item ,doc ,name .
                 (:button (:toggle . (and (boundp ',variable) ,variable))))))

(define-key menu-bar-options-menu [debug-on-quit]
  (menu-bar-make-toggle toggle-debug-on-quit debug-on-quit
			"Debug on Quit" "Debug on Quit %s"))
(define-key menu-bar-options-menu [debug-on-error]
  (menu-bar-make-toggle toggle-debug-on-error debug-on-error
			"Debug on Error" "Debug on Error %s"))
(define-key menu-bar-options-menu [options-separator]
  '("--"))
(define-key menu-bar-options-menu [save-place]
  (menu-bar-make-toggle toggle-save-place-globally save-place
			"Save Place in Files between Sessions"
			"Saving place in files %s"
                        (require 'saveplace)
			(setq-default save-place (not (default-value save-place)))))
(define-key menu-bar-options-menu [uniquify]
  (menu-bar-make-toggle toggle-uniquify-buffer-names uniquify-buffer-name-style
			"Use Directory Names in Buffer Names"
			"Directory name in buffer names (uniquify) %s"
			(require 'uniquify)
			(setq uniquify-buffer-name-style
			      (if (not uniquify-buffer-name-style)
				  'forward))))
(define-key menu-bar-options-menu [case-fold-search]
  (menu-bar-make-toggle toggle-case-fold-search case-fold-search
			"Case folding in searches"
			"Case folding in searches %s"))
(define-key menu-bar-options-menu [truncate-lines]
  (menu-bar-make-toggle
   toggle-truncate-lines truncate-lines
   "Line Truncation" "Line Truncation %s"
   (prog1 (setq truncate-lines (not truncate-lines))
     (set-buffer-modified-p (buffer-modified-p)))))
(define-key menu-bar-options-menu [transient-mark-mode]
  (menu-bar-make-toggle toggle-transient-mark-mode transient-mark-mode
			"Transient Mark Mode (highlights region)"
			"Transient Mark mode %s"))
(define-key menu-bar-options-menu [toggle-auto-compression]
  '(menu-item "Automatic File De/compression"
             auto-compression-mode .
             (:button (:toggle . (rassq 'jka-compr-handler
                                        file-name-handler-alist)))))
(define-key menu-bar-options-menu [auto-fill-mode]
  '(menu-item "Auto Fill (word wrap) in Text modes"
              toggle-text-mode-auto-fill .
              (:button (:toggle . (member 'turn-on-auto-fill text-mode-hook)))))
(define-key menu-bar-options-menu [toggle-global-lazy-font-lock-mode]
  (menu-bar-make-toggle toggle-global-lazy-font-lock-mode global-font-lock-mode
			"Global Font Lock (highlights syntax)"
			"Global Font Lock mode %s"
			;; Make sure a support mode is used;
			;; otherwise Font Lock will be too slow.
			(require 'font-lock)
			(if (not global-font-lock-mode)
			    (or font-lock-support-mode
				(setq font-lock-support-mode 'lazy-lock-mode)))
			(global-font-lock-mode)))

(defvar menu-bar-describe-menu (make-sparse-keymap "Describe"))

(define-key menu-bar-describe-menu [describe-variable]
  '("Describe Variable..." . describe-variable))
(define-key menu-bar-describe-menu [describe-function]
  '("Describe Function..." . describe-function))
(define-key menu-bar-describe-menu [describe-key]
  '("Describe Key..." . describe-key))
(define-key menu-bar-describe-menu [list-keybindings]
  '("List Key Bindings" . describe-bindings))
(define-key menu-bar-describe-menu [apropos-variables]
  '("Apropos Variables..." . apropos-variable))
(define-key menu-bar-describe-menu [apropos-commands]
  '("Apropos Commands..." . apropos-command))
(define-key menu-bar-describe-menu [describe-mode]
  '("Describe Buffer Modes" . describe-mode))

(defvar menu-bar-manuals-menu (make-sparse-keymap "Manuals"))

(define-key menu-bar-manuals-menu [man]
  '("Read Man Page..." . manual-entry))
(define-key menu-bar-manuals-menu [sep2]
  '("--"))
(define-key menu-bar-manuals-menu [key]
  '("Find Key in Manual" . Info-goto-emacs-key-command-node))
(define-key menu-bar-manuals-menu [command]
  '("Find Command in Manual" . Info-goto-emacs-command-node))
(define-key menu-bar-manuals-menu [info]
  '("Browse Manuals with Info" . info))
(define-key menu-bar-manuals-menu [sep1]
  '("--"))
(define-key menu-bar-manuals-menu [emacs-problems]
  '("Emacs Known Problems" . view-emacs-problems))
(define-key menu-bar-manuals-menu [emacs-faq]
  '("Emacs FAQ" . view-emacs-FAQ))
(define-key menu-bar-manuals-menu [emacs-news]
  '("Emacs News" . view-emacs-news))

(define-key menu-bar-help-menu [describe-no-warranty]
  '("(Non)Warranty" . describe-no-warranty))
(define-key menu-bar-help-menu [describe-copying]
  '("Copying Conditions" . describe-copying))
(define-key menu-bar-help-menu [describe-distribution]
  '("Getting New Versions" . describe-distribution))
(define-key menu-bar-help-menu [emacs-version]
  '("Show Version" . emacs-version))
(define-key menu-bar-help-menu [report-emacs-bug]
  '("Send Bug Report..." . report-emacs-bug))
(define-key menu-bar-help-menu [sep2]
  '("--"))
(define-key menu-bar-help-menu [finder-by-keyword]
  '("Find Emacs Packages..." . finder-by-keyword))
(define-key menu-bar-help-menu [describe]
  (cons "Describe" menu-bar-describe-menu))
(define-key menu-bar-help-menu [manuals]
  (cons "Manuals" menu-bar-manuals-menu))
(define-key menu-bar-help-menu [emacs-tutorial]
  '("Emacs Tutorial" . help-with-tutorial))
(define-key menu-bar-help-menu [sep1]
  '("--"))
(define-key menu-bar-help-menu [options]
  (cons "Options" menu-bar-options-menu))
(define-key menu-bar-help-menu [customize]
  (cons "Customize" menu-bar-custom-menu))

(defun kill-this-buffer ()	; for the menubar
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun kill-this-buffer-enabled-p ()
  (let ((count 0)
	(buffers (buffer-list)))
    (while buffers
      (or (string-match "^ " (buffer-name (car buffers)))
	  (setq count (1+ count)))
      (setq buffers (cdr buffers)))
    (and (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
	 (> count 1))))

(put 'kill-this-buffer 'menu-enable '(kill-this-buffer-enabled-p))

(put 'save-buffer 'menu-enable
     '(and (buffer-modified-p)
	   (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))

(put 'write-file 'menu-enable
     '(not (window-minibuffer-p (frame-selected-window menu-updating-frame))))

(put 'find-file 'menu-enable
     '(not (window-minibuffer-p (frame-selected-window menu-updating-frame))))

(put 'dired 'menu-enable
     '(not (window-minibuffer-p (frame-selected-window menu-updating-frame))))

(put 'insert-file 'menu-enable
     '(not (window-minibuffer-p (frame-selected-window menu-updating-frame))))

(put 'revert-buffer 'menu-enable
     '(or revert-buffer-function revert-buffer-insert-file-contents-function
	  (and (buffer-file-name)
	       (or (buffer-modified-p)
		   (not (verify-visited-file-modtime (current-buffer)))))))

;; Permit deleting frame if it would leave a visible or iconified frame.
(put 'delete-frame 'menu-enable
     '(delete-frame-enabled-p))

(defun delete-frame-enabled-p ()
  "Return non-nil if `delete-frame' should be enabled in the menu bar."
  (let ((frames (frame-list))
	(count 0))
    (while frames
      (if (frame-visible-p (car frames))
	  (setq count (1+ count)))
      (setq frames (cdr frames)))
    (> count 1)))

(put 'advertised-undo 'menu-enable
     '(and (not (eq t buffer-undo-list))
	   (if (eq last-command 'undo)
	       (and (boundp 'pending-undo-list)
		    pending-undo-list)
	     buffer-undo-list)))

(defcustom yank-menu-length 20
  "*Maximum length to display in the yank-menu."
  :type 'integer
  :group 'mouse)

(defun menu-bar-update-yank-menu (string old)
  (let ((front (car (cdr yank-menu)))
	(menu-string (if (<= (length string) yank-menu-length)
			 string
		       (concat
			(substring string 0 (/ yank-menu-length 2))
			"..."
			(substring string (- (/ yank-menu-length 2)))))))
    ;; Don't let the menu string be all dashes
    ;; because that has a special meaning in a menu.
    (if (string-match "\\`-+\\'" menu-string)
	(setq menu-string (concat menu-string " ")))
    ;; If we're supposed to be extending an existing string, and that
    ;; string really is at the front of the menu, then update it in place.
    (if (and old (or (eq old (car front))
		     (string= old (car front))))
	(progn
	  (setcar front string)
	  (setcar (cdr front) menu-string))
      (setcdr yank-menu
	      (cons
	       (cons string (cons menu-string 'menu-bar-select-yank))
	       (cdr yank-menu)))))
  (if (> (length (cdr yank-menu)) kill-ring-max)
      (setcdr (nthcdr kill-ring-max yank-menu) nil)))

(put 'menu-bar-select-yank 'apropos-inhibit t)
(defun menu-bar-select-yank ()
  (interactive "*")
  (push-mark (point))
  (insert last-command-event))

;; This definition is just to show what this looks like.
;; It gets overridden below when menu-bar-update-buffers is called.
(define-key global-map [menu-bar buffer]
  (cons "Buffers" (make-sparse-keymap "Buffers")))

(defvar list-buffers-directory nil)

(defvar menu-bar-update-buffers-maxbuf)

(defun menu-bar-select-buffer ()
  (interactive)
  (switch-to-buffer last-command-event))

(defun menu-bar-select-frame ()
  (interactive)
  (make-frame-visible last-command-event)
  (raise-frame last-command-event)
  (select-frame last-command-event))

(defun menu-bar-update-buffers-1 (elt)
  (cons (format
	 (format "%%%ds  %%s%%s  %%s" menu-bar-update-buffers-maxbuf)
	 (cdr elt)
	 (if (buffer-modified-p (car elt))
	     "*" " ")
	 (save-excursion
	   (set-buffer (car elt))
	   (if buffer-read-only "%" " "))
	 (let ((file
		(or (buffer-file-name (car elt))
		    (save-excursion
		      (set-buffer (car elt))
		      list-buffers-directory)
		    "")))
	   (setq file (or (file-name-directory file)
			  ""))
	   (if (> (length file) 20)
	       (setq file (concat "..." (substring file -17))))
	   file))
	(car elt)))

(defvar menu-bar-buffers-menu-list-buffers-entry nil)

(defun menu-bar-update-buffers ()
  ;; If user discards the Buffers item, play along.
  (and (lookup-key (current-global-map) [menu-bar buffer])
       (frame-or-buffer-changed-p)
       (let ((buffers (buffer-list))
	     (frames (frame-list))
	     (maxlen 0)
	     buffers-menu frames-menu)
	 ;; If requested, list only the N most recently selected buffers.
	 (if (and (integerp buffers-menu-max-size)
		  (> buffers-menu-max-size 1))
	     (if (> (length buffers) buffers-menu-max-size)
		 (setcdr (nthcdr buffers-menu-max-size buffers) nil)))

	 ;; Make the menu of buffers proper.
	 (setq buffers-menu
	       (cons "Select Buffer"
		     (let* ((buffer-list
			     (mapcar 'list buffers))
			    tail
			    (menu-bar-update-buffers-maxbuf 0)
			    alist
			    head)
		       ;; Put into each element of buffer-list
		       ;; the name for actual display,
		       ;; perhaps truncated in the middle.
		       (setq tail buffer-list)
		       (while tail
			 (let ((name (buffer-name (car (car tail)))))
			   (setcdr (car tail)
				   (if (> (length name) 27)
				       (concat (substring name 0 12)
					       "..."
					       (substring name -12))
				     name)))
			 (setq tail (cdr tail)))
		       ;; Compute the maximum length of any name.
		       (setq tail buffer-list)
		       (while tail
			 (or (eq ?\ (aref (cdr (car tail)) 0))
			     (setq menu-bar-update-buffers-maxbuf
				   (max menu-bar-update-buffers-maxbuf
					(length (cdr (car tail))))))
			 (setq tail (cdr tail)))
		       ;; Set ALIST to an alist of the form
		       ;; ITEM-STRING . BUFFER
		       (setq tail buffer-list)
		       (while tail
			 (let ((elt (car tail)))
			   (or (eq ?\ (aref (cdr elt) 0))
			       (setq alist (cons
					    (menu-bar-update-buffers-1 elt)
					    alist)))
			   (and alist (> (length (car (car alist))) maxlen)
				(setq maxlen (length (car (car alist))))))
			 (setq tail (cdr tail)))
		       (setq alist (nreverse alist))
		       ;; Make the menu item for list-buffers
		       ;; or reuse the one we already have.
		       ;; The advantage in reusing one
		       ;; is that it already has the keyboard equivalent
		       ;; cached, so we save the time to look that up again.
		       (or menu-bar-buffers-menu-list-buffers-entry
			   (setq menu-bar-buffers-menu-list-buffers-entry
				 (cons
				  'list-buffers
				  (cons
				   ""
				   'list-buffers))))
		       ;; Update the item string for menu's new width.
		       (setcar (cdr menu-bar-buffers-menu-list-buffers-entry)
			       (concat (make-string (max (- (/ maxlen 2) 8) 0)
						    ?\ )
				       "List All Buffers"))
		       ;; Now make the actual list of items,
		       ;; ending with the list-buffers item.
		       (nconc (mapcar '(lambda (pair)
					 ;; This is somewhat risque, to use
					 ;; the buffer name itself as the event
					 ;; type to define, but it works.
					 ;; It would not work to use the buffer
					 ;; since a buffer as an event has its
					 ;; own meaning.
					 (nconc (list (buffer-name (cdr pair))
						      (car pair)
						      (cons nil nil))
						'menu-bar-select-buffer))
				      alist)
			      (list menu-bar-buffers-menu-list-buffers-entry)))))


	 ;; Make a Frames menu if we have more than one frame.
	 (if (cdr frames)
	     (let ((name (concat (make-string (max (- (/ maxlen 2) 3) 0)
					      ?\ )
				 "Frames"))
		   (frames-menu
		    (cons 'keymap
			  (cons "Select Frame"
				(mapcar '(lambda (frame)
					   (nconc (list frame
							(cdr (assq 'name
								   (frame-parameters frame)))
							(cons nil nil))
						  'menu-bar-select-frame))
					frames)))))
	       ;; Put it underneath the Buffers menu.
	       (setq buffers-menu (cons (cons 'frames (cons name frames-menu))
					buffers-menu))))
	 (if buffers-menu
	     (setq buffers-menu (cons 'keymap buffers-menu)))
	 (define-key (current-global-map) [menu-bar buffer]
	   (cons "Buffers" buffers-menu)))))

(add-hook 'menu-bar-update-hook 'menu-bar-update-buffers)

(menu-bar-update-buffers)

;; this version is too slow
;;;(defun format-buffers-menu-line (buffer)
;;;  "Returns a string to represent the given buffer in the Buffer menu.
;;;nil means the buffer shouldn't be listed.  You can redefine this."
;;;  (if (string-match "\\` " (buffer-name buffer))
;;;      nil
;;;    (save-excursion
;;;     (set-buffer buffer)
;;;     (let ((size (buffer-size)))
;;;       (format "%s%s %-19s %6s %-15s %s"
;;;	       (if (buffer-modified-p) "*" " ")
;;;	       (if buffer-read-only "%" " ")
;;;	       (buffer-name)
;;;	       size
;;;	       mode-name
;;;	       (or (buffer-file-name) ""))))))

;;; Set up a menu bar menu for the minibuffer.

(mapcar
 (function
  (lambda (map)
    (define-key map [menu-bar minibuf]
      (cons "Minibuf" (make-sparse-keymap "Minibuf")))))
 (list minibuffer-local-ns-map
       minibuffer-local-must-match-map
       minibuffer-local-isearch-map
       minibuffer-local-map
       minibuffer-local-completion-map))

(mapcar
 (function
  (lambda (map)
    (define-key map [menu-bar minibuf ?\?]
      '("List Completions" . minibuffer-completion-help))
    (define-key map [menu-bar minibuf space]
      '("Complete Word" . minibuffer-complete-word))
    (define-key map [menu-bar minibuf tab]
      '("Complete" . minibuffer-complete))
    ))
 (list minibuffer-local-must-match-map
       minibuffer-local-completion-map))

(mapcar
 (function
  (lambda (map)
    (define-key map [menu-bar minibuf quit]
      '("Quit" . keyboard-escape-quit))
    (define-key map [menu-bar minibuf return]
      '("Enter" . exit-minibuffer))
    ))
 (list minibuffer-local-ns-map
       minibuffer-local-must-match-map
       minibuffer-local-isearch-map
       minibuffer-local-map
       minibuffer-local-completion-map))

(defcustom menu-bar-mode nil
  "Toggle display of a menu bar on each frame.
Setting this variable directly does not take effect;
use either \\[customize] or the function `menu-bar-mode'."
  :set (lambda (symbol value)
	 (menu-bar-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'frames)

(defun menu-bar-mode (flag)
  "Toggle display of a menu bar on each frame.
This command applies to all frames that exist and frames to be
created in the future.
With a numeric argument, if the argument is positive,
turn on menu bars; otherwise, turn off menu bars."
 (interactive "P")

  ;; Make menu-bar-mode and default-frame-alist consistent.
  (let ((default (assq 'menu-bar-lines default-frame-alist)))
    (if default
	(setq menu-bar-mode (not (eq (cdr default) 0)))
      (setq default-frame-alist
	    (cons (cons 'menu-bar-lines (if menu-bar-mode 1 0))
		  default-frame-alist))))

  ;; Toggle or set the mode, according to FLAG.
 (setq menu-bar-mode (if (null flag) (not menu-bar-mode)
		       (> (prefix-numeric-value flag) 0)))

 ;; Apply it to default-frame-alist.
 (let ((parameter (assq 'menu-bar-lines default-frame-alist)))
   (if (consp parameter)
       (setcdr parameter (if menu-bar-mode 1 0))
     (setq default-frame-alist
	   (cons (cons 'menu-bar-lines (if menu-bar-mode 1 0))
		 default-frame-alist))))

 ;; Apply it to existing frames.
 (let ((frames (frame-list)))
   (while frames
     (let ((height (cdr (assq 'height (frame-parameters (car frames))))))
       (modify-frame-parameters (car frames)
				(list (cons 'menu-bar-lines
					  (if menu-bar-mode 1 0))))
       (modify-frame-parameters (car frames)
				(list (cons 'height height))))
     (setq frames (cdr frames)))))

(provide 'menu-bar)

;;; menu-bar.el ends here
