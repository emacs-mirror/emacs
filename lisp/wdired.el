;;; wdired.el --- Rename files editing their names in dired buffers

;; Copyright (C) 2001, 2004  Free Software Foundation, Inc.

;; Filename: wdired.el
;; Author: Juan León Lahoz García <juan-leon.lahoz@tecsidel.es>
;; Version: 1.91
;; Keywords: dired, environment, files, renaming

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; wdired.el (the "w" is for writable) provides an alternative way of
;; renaming files.
;;
;; Have you ever wished to use C-x r t (string-rectangle), M-%
;; (query-replace), M-c (capitalize-word), etc. to change the name of
;; the files in a "dired" buffer? Now you can do this. All the power
;; of emacs commands are available to renaming files!
;; 
;; This package provides a function that makes the filenames of a a
;; dired buffer editable, by changing the buffer mode (which inhibits
;; all of the commands of dired mode). Here you can edit the names of
;; one or more files and directories, and when you press C-c C-c, the
;; renaming takes effect and you are back to dired mode.
;;
;; Another things you can do with wdired:
;;
;; - To move files to another directory (by typing their path,
;;   absolute or relative, as a part of the new filename).
;;
;; - To change the target of symbolic links.
;;
;; - To change the permission bits of the filenames (in systems with a
;;   working unix-alike `dired-chmod-program'). See and customize the
;;   variable `wdired-allow-to-change-permissions'. To change a single
;;   char (toggling between its two more usual values) you can press
;;   the space bar over it or left-click the mouse. To set any char to
;;   an specific value (this includes the SUID, SGID and STI bits) you
;;   can use the key labeled as the letter you want. Please note that
;;   permissions of the links cannot be changed in that way, because
;;   the change would affect to their targets, and this would not be
;;   WYSIWYG :-).
;;
;; - To mark files for deletion, by deleting their whole filename.
;;
;; I do not have a URL to hang wdired, but you can use the one below
;; to find the latest version:
;;
;; http://groups.google.com/groups?as_ugroup=gnu.emacs.sources&as_q=wdired

;;; Installation:

;; Add this file (byte-compiling it is recommended) to your load-path.
;; Then add one of these set of lines (or similar ones) to your config:
;;
;; This is the easy way:
;;
;; (require 'wdired)
;; (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
;;
;; This is recommended way for faster emacs startup time and lower
;; memory consumption, but remind to add these lines before dired.el
;; gets loaded (i.e., near the beginning of your .emacs file):
;;
;; (autoload 'wdired-change-to-wdired-mode "wdired")
;; (add-hook 'dired-load-hook
;;           '(lambda ()
;;              (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
;;              (define-key dired-mode-map
;;                [menu-bar immediate wdired-change-to-wdired-mode]
;;                '("Edit File Names" . wdired-change-to-wdired-mode))))
;;
;;
;; Type "M-x customize-group RET wdired" if you want make changes to
;; the default behavior.

;;; Usage:

;; Then, you can start editing the names of the files by typing "r"
;; (or whatever key you choose, or M-x wdired-change-to-wdired-mode).
;; Use C-c C-c when finished or C-c C-k to abort. You can use also the
;; menu options: in dired mode, "Edit File Names" under "Immediate".
;; While editing the names, a new submenu "WDired" is available at top
;; level. You can customize the behavior of this package from this
;; menu.

;;; Change Log:

;; From 1.9 to 1.91
;;
;; - Fixed a bug (introduced in 1.9) so now files can be marked for
;;   deletion again, by deleting their whole filename.

;; From 1.8 to 1.9
;;
;; - Another alternative way of editing permissions allowed, see
;;   `wdired-allow-to-change-permissions' for details.
;;
;; - Now wdired doesn't rely on regexp so much. As a consequence of
;;   this, you can add newlines to filenames and symlinks targets
;;   (although this is not very usual, IMHO). Please note that dired
;;   (at least in Emacs 21.1 and previous) does not work very well
;;   with filenames with newlines in them, so RET is deactivated in
;;   wdired mode. But you can activate it if you want.
;;
;; - Now `upcase-word' `capitalize-word' and `downcase-word' are not
;;   advised to work better with wdired mode, but the keys bound to
;;   them use wdired versions of those commands.
;;
;; - Now "undo" actions are not inherited from wdired mode when
;;   changing to dired mode.
;;
;; - Code and documentation cleanups.
;;
;; - Fixed a bug that was making wdired to fail on users with
;;   `dired-backup-overwrite' set to t.
;;
;; - C-c C-[ now abort changes.

;; From 1.7 to 1.8
;;
;; - Now permission (access-control) bits of the files can be changed.
;;   Please see the commentary section and the custom variable
;;   `wdired-allow-to-change-permissions' for details.
;;
;; - Added another possible value for the variable
;;   `wdired-always-move-to-filename-beginning', useful to change
;;   permission bits of several files without the cursor jumping to
;;   filenames when changing lines.

;; From 0.1 to 1.7

;; - I've moved the list of changes to another file, because it was
;;   huge. Ask me for it or search older versions in google.

;;; TODO:

;; - Make it to work in XEmacs. Any volunteer?

;;; Code:

(eval-when-compile
  (require 'advice)
  (defvar dired-backup-overwrite) ; Only in emacs 20.x this is a custom var
  (set (make-local-variable 'byte-compile-dynamic) t))

(eval-and-compile
  (require 'dired)
  (autoload 'dired-do-create-files-regexp "dired-aux")
  (autoload 'dired-call-process "dired-aux"))

(defgroup wdired nil
  "Mode to rename files by editing their names in dired buffers."
  :group 'dired)

(defcustom wdired-use-interactive-rename nil
  "*If t, confirmation is required before actually rename the files.
Confirmation is required also for overwriting files.  If nil, no
confirmation is required for change the file names, and the variable
`wdired-is-ok-overwrite' is used to see if it is ok to overwrite files
without asking."
  :type 'boolean
  :group 'wdired)

(defcustom wdired-is-ok-overwrite nil
  "*If non-nil the renames can overwrite files without asking. 
This variable is used only if `wdired-use-interactive-rename' is nil."
  :type 'boolean
  :group 'wdired)

(defcustom wdired-always-move-to-filename-beginning nil
  "*If t the \"up\" and \"down\" movement is done as in dired mode.
That is, always move the point to the beginning of the filename at line.

If `sometimes, only move to the beginning of filename if the point is
before it, and `track-eol' is honored.  This behavior is very handy
when editing several filenames.

If nil, \"up\" and \"down\" movement is done as in any other buffer."
  :type '(choice (const :tag "As in any other mode" nil)
		 (const :tag "Smart cursor placement" sometimes)
		 (other :tag "As in dired mode" t))
  :group 'wdired)

(defcustom wdired-advise-functions t
  "*If t some editing commands are advised when wdired is loaded.
The advice only has effect in wdired mode.  These commands are
`query-replace' `query-replace-regexp' `replace-string', and the
advice makes them to ignore read-only regions, so no attempts to
modify these regions are done by them, and so they don't end
prematurely.

Setting this to nil does not unadvise the functions, if they are
already advised, but new Emacs will not advise them."
  :type 'boolean
  :group 'wdired)

(defcustom wdired-allow-to-redirect-links t
  "*If non-nil, the target of the symbolic links can be changed also.
In systems without symbolic links support, this variable has no effect
at all."
  :type 'boolean
  :group 'wdired)

(defcustom wdired-allow-to-change-permissions nil
  "*If non-nil, the permissions bits of the files can be changed also.

If t, to change a single bit, put the cursor over it and press the
space bar, or left click over it.  You can also hit the letter you want
to set: if this value is allowed, the character in the buffer will be
changed.  Anyway, the point is advanced one position, so, for example,
you can keep the \"x\" key pressed to give execution permissions to
everybody to that file.

If `advanced, the bits are freely editable.  You can use
`string-rectangle', `query-replace', etc.  You can put any value (even
newlines), but if you want your changes to be useful, you better put a
intelligible value.

Anyway, the real change of the permissions is done with the external
program `dired-chmod-program', which must exist."
  :type '(choice (const :tag "Not allowed" nil)
                 (const :tag "Toggle/set bits" t)
		 (other :tag "Bits freely editable" advanced))
  :group 'wdired)

(defvar wdired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'wdired-finish-edit)
    (define-key map "\C-c\C-c" 'wdired-finish-edit)
    (define-key map "\C-c\C-k" 'wdired-abort-changes)
    (define-key map "\C-c\C-[" 'wdired-abort-changes)
    (define-key map "\C-m"     'wdired-newline)
    (define-key map "\C-j"     'wdired-newline)
    (define-key map "\C-o"     'wdired-newline)
    (define-key map [up]       'wdired-previous-line)
    (define-key map "\C-p"     'wdired-previous-line)
    (define-key map [down]     'wdired-next-line)
    (define-key map "\C-n"     'wdired-next-line)

    (define-key map [menu-bar wdired]
      (cons "WDired" (make-sparse-keymap "WDired")))
    (define-key map [menu-bar wdired wdired-customize]
      '("Options" . wdired-customize))
    (define-key map [menu-bar wdired dashes]
      '("--"))
    (define-key map [menu-bar wdired wdired-abort-changes]
      '("Abort Changes" . wdired-abort-changes))
    (define-key map [menu-bar wdired wdired-finish-edit]
      '("Commit Changes" . wdired-finish-edit))
    ;; FIXME: Use the new remap trick.
    (substitute-key-definition 'upcase-word 'wdired-upcase-word
			       map global-map)
    (substitute-key-definition 'capitalize-word 'wdired-capitalize-word
			       map global-map)
    (substitute-key-definition 'downcase-word 'wdired-downcase-word
			       map global-map)
    map))

(defvar wdired-mode-hook nil
  "Hook run when changing to wdired mode.")

;; Local variables (put here to avoid compilation gripes)
(defvar wdired-col-perm) ;; Column where the permission bits start
(defvar wdired-old-content)


(defun wdired-mode ()
  "\\<wdired-mode-map>File Names Editing mode.

Press \\[wdired-finish-edit] to make the changes to take effect and
exit.  To abort the edit, use \\[wdired-abort-changes].

In this mode you can edit the names of the files, the target of the
links and the permission bits of the files.  You can `customize-group'
wdired.

Editing things out of the filenames, or adding or deleting lines is
not allowed, because the rest of the buffer is read-only."
  (interactive)
  (error "This mode can be enabled only by `wdired-change-to-wdired-mode'"))
(put 'wdired-mode 'mode-class 'special)


;;;###autoload
(defun wdired-change-to-wdired-mode ()
  "Put a dired buffer in a mode in which filenames are editable.
In this mode the names of the files can be changed, and after
typing C-c C-c the files and directories in disk are renamed.

See `wdired-mode'."
  (interactive)
  (set (make-local-variable 'wdired-old-content)
       (buffer-substring (point-min) (point-max)))
  (use-local-map wdired-mode-map)
  (force-mode-line-update)
  (setq buffer-read-only nil)
  (dired-unadvertise default-directory)
  (add-hook 'kill-buffer-hook 'wdired-check-kill-buffer nil t)
  (setq major-mode 'wdired-mode)
  (setq mode-name "Edit filenames")
  (setq revert-buffer-function 'wdired-revert)
  ;; I temp disable undo for performance: since I'm going to clear the
  ;; undo list, it can save more than a 9% of time with big
  ;; directories because setting properties modify the undo-list.
  (buffer-disable-undo)
  (wdired-preprocess-files)
  (if wdired-allow-to-change-permissions
      (wdired-preprocess-perms))
  (if (and wdired-allow-to-redirect-links (fboundp 'make-symbolic-link))
      (wdired-preprocess-symlinks))
  (buffer-enable-undo) ; Performance hack. See above.
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (run-hooks 'wdired-mode-hook)
  (message (substitute-command-keys "Press \\[wdired-finish-edit] when finished")))


;; Protect the buffer so only the filenames can be changed, and put
;; properties so filenames (old and new) can be easily found.
(defun wdired-preprocess-files ()
  (put-text-property 1 2 'front-sticky t)
  (save-excursion
    (goto-char (point-min))
    (let ((b-protection (point))
	  filename)
      (while (not (eobp))
	(setq filename (dired-get-filename nil t))
        (when (and filename
		   (not (member (file-name-nondirectory filename) '("." ".."))))
	  (dired-move-to-filename)
	  (put-text-property (- (point) 2) (1- (point)) 'old-name filename)
	  (put-text-property b-protection (1- (point)) 'read-only t)
	  (setq b-protection (dired-move-to-end-of-filename t)))
	(put-text-property (point) (1+ (point)) 'end-name t)
        (forward-line))
      (put-text-property b-protection (point-max) 'read-only t))))

;; This code is a copy of some dired-get-filename lines.
(defsubst wdired-normalize-filename (file)
  (setq file
	;; FIXME: shouldn't we check for a `b' argument or somesuch before
	;; doing such unquoting?  --Stef
	(read (concat
	       "\"" (replace-regexp-in-string
		     "\\([^\\]\\|\\`\\)\"" "\\1\\\\\"" file)
	       "\"")))
  (and file buffer-file-coding-system
       (not file-name-coding-system)
       (not default-file-name-coding-system)
       (setq file (encode-coding-string file buffer-file-coding-system)))
  file)

(defun wdired-get-filename (&optional no-dir old)
  "Return the filename at line.
Similar to `dired-get-filename' but it doesn't rely on regexps.  It
relies on wdired buffer's properties.  Optional arg NO-DIR with value
non-nil means don't include directory.  Optional arg OLD with value
non-nil means return old filename."
  ;; FIXME: Use dired-get-filename's new properties.
  (let (beg end file)
    (save-excursion
      (setq end (progn (end-of-line) (point)))
      (beginning-of-line)
      (setq beg (next-single-property-change (point) 'old-name nil end))
      (unless (eq beg end)
	(if old
	    (setq file (get-text-property beg 'old-name))
	  (setq end (next-single-property-change (1+ beg) 'end-name))
	  (setq file (buffer-substring-no-properties (+ 2 beg) end)))
	(and file (setq file (wdired-normalize-filename file))))
      (if (or no-dir old)
	  file
	(and file (> (length file) 0)
             (concat (dired-current-directory) file))))))


(defun wdired-change-to-dired-mode ()
  "Change the mode back to dired."
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max)
			    '(read-only nil local-map nil)))
  (put-text-property 1 2 'front-sticky nil)
  (use-local-map dired-mode-map)
  (force-mode-line-update)
  (setq buffer-read-only t)
  (setq major-mode 'dired-mode)
  (setq mode-name "Dired")
  (dired-advertise)
  (remove-hook 'kill-buffer-hook 'wdired-check-kill-buffer t)
  (setq revert-buffer-function 'dired-revert))


(defun wdired-abort-changes ()
  "Abort changes and return to dired mode."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert wdired-old-content))
  (wdired-change-to-dired-mode)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (message "Changes aborted"))

(defun wdired-finish-edit ()
  "Actually rename files based on your editing in the Dired buffer."
  (interactive)
  (wdired-change-to-dired-mode)
  (let ((overwrite (or wdired-is-ok-overwrite 1))
	(changes nil)
	(files-deleted nil)
	(errors 0)
	file-ori file-new tmp-value)
    (save-excursion
      (if (and wdired-allow-to-redirect-links
	       (fboundp 'make-symbolic-link))
	  (progn
	    (setq tmp-value (wdired-do-symlink-changes))
	    (setq errors (cdr tmp-value))
	    (setq changes (car tmp-value))))
      (if (and wdired-allow-to-change-permissions
	       (boundp 'wdired-col-perm)) ; could have been changed
	  (progn
	    (setq tmp-value (wdired-do-perm-changes))
	    (setq errors (+ errors (cdr tmp-value)))
	    (setq changes (or changes (car tmp-value)))))
      (goto-char (point-max))
      (while (not (bobp))
	(setq file-ori (wdired-get-filename nil t))
	(if file-ori
	    (setq file-new (wdired-get-filename)))
	(if (and file-ori (not (equal file-new file-ori)))
	    (progn
	      (setq changes t)
	      (if (not file-new) ;empty filename!
		  (setq files-deleted (cons file-ori files-deleted))
		(progn
		  (setq file-new (substitute-in-file-name file-new))
		  (if wdired-use-interactive-rename
		      (wdired-search-and-rename file-ori file-new)
		    (condition-case err
			(let ((dired-backup-overwrite nil))
			  (dired-rename-file file-ori file-new
					     overwrite))
		      (error
		       (setq errors (1+ errors))
		       (dired-log (concat "Rename `" file-ori "' to `"
					  file-new "' failed:\n%s\n")
				  err))))))))
	(forward-line -1)))
    (if changes
        (revert-buffer) ;The "revert" is necessary to re-sort the buffer
      (let ((buffer-read-only nil))
	(remove-text-properties (point-min) (point-max)
				'(old-name nil end-name nil old-link nil
					   end-link nil end-perm nil
					   old-perm nil perm-changed nil))
	(message "(No changes to be performed)")))
    (if files-deleted
        (wdired-flag-for-deletion files-deleted))
    (if (> errors 0)
        (dired-log-summary (format "%d rename actions failed" errors) nil)))
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil))

;; Renames a file, searching it in a modified dired buffer, in order
;; to be able to use `dired-do-create-files-regexp' and get its
;; "benefits"
(defun wdired-search-and-rename (filename-ori filename-new)
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (let ((exit-while nil)
	  curr-filename)
      (while (not exit-while)
        (setq curr-filename (wdired-get-filename))
        (if (and curr-filename
                 (equal (substitute-in-file-name curr-filename) filename-new))
            (progn
              (setq exit-while t)
              (let ((inhibit-read-only t))
                (dired-move-to-filename)
                (search-forward (wdired-get-filename t) nil t)
                (replace-match (file-name-nondirectory filename-ori) t t))
              (dired-do-create-files-regexp
               (function dired-rename-file)
               "Move" 1 ".*" filename-new nil t))
          (progn
            (forward-line -1)
            (beginning-of-line)
            (setq exit-while (= 1 (point)))))))))

;; marks a list of files for deletion
(defun wdired-flag-for-deletion (filenames-ori)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (member (dired-get-filename nil t) filenames-ori)
          (dired-flag-file-deletion 1)
	(forward-line)))))

(defun wdired-customize ()
  "Customize wdired options."
  (interactive)
  (customize-apropos "wdired" 'groups))

(defun wdired-revert (&optional arg noconfirm)
  "Discard changes in the buffer and update the changes in the disk."
  (wdired-change-to-dired-mode)
  (revert-buffer)
  (wdired-change-to-wdired-mode))

(defun wdired-check-kill-buffer ()
  ;; FIXME: Can't we use the normal mechanism for that?  --Stef
  (if (and
       (buffer-modified-p)
       (not (y-or-n-p "Buffer changed. Discard changes and kill buffer? ")))
      (error nil)))

(defun wdired-next-line (arg)
  "Move down lines then position at filename or the current column.
See `wdired-always-move-to-filename-beginning'.  Optional prefix ARG
says how many lines to move; default is one line."
  (interactive "p")
  (next-line arg)
  (if (or (eq wdired-always-move-to-filename-beginning t)
	  (and wdired-always-move-to-filename-beginning
	       (< (current-column)
		  (save-excursion (dired-move-to-filename)
				  (current-column)))))
      (dired-move-to-filename)))

(defun wdired-previous-line (arg)
  "Move up lines then position at filename or the current column.
See `wdired-always-move-to-filename-beginning'.  Optional prefix ARG
says how many lines to move; default is one line."
  (interactive "p")
  (previous-line arg)
  (if (or (eq wdired-always-move-to-filename-beginning t)
	  (and wdired-always-move-to-filename-beginning
	       (< (current-column)
		  (save-excursion (dired-move-to-filename)
				  (current-column)))))
      (dired-move-to-filename)))

;; dired doesn't works well with newlines, so ...
(defun wdired-newline ()
  "Do nothing."
  (interactive))

;; Put the needed properties to allow the user to change links' targets
(defun wdired-preprocess-symlinks ()
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at dired-re-sym)
            (progn
              (re-search-forward " -> \\(.*\\)$")
	      (put-text-property (- (match-beginning 1) 2)
				 (1- (match-beginning 1)) 'old-link
				 (match-string-no-properties 1))
              (put-text-property (match-end 1) (1+ (match-end 1)) 'end-link t)
	      (put-text-property (1- (match-beginning 1))
				 (match-end 1) 'read-only nil)))
        (forward-line)
	(beginning-of-line)))))


(defun wdired-get-previous-link (&optional old move)
  "Return the next symlink target.
If OLD, return the old target.  If MOVE, move point before it."
  (let (beg end target)
    (setq beg (previous-single-property-change (point) 'old-link nil))
    (if beg
	(progn
	  (if old
	      (setq target (get-text-property (1- beg) 'old-link))
	    (setq end (next-single-property-change beg 'end-link))
	    (setq target (buffer-substring-no-properties (1+ beg) end)))
	  (if move (goto-char (1- beg)))))
    (and target (wdired-normalize-filename target))))



;; Perform the changes in the target of the changed links.
(defun wdired-do-symlink-changes()
  (let ((changes nil)
	(errors 0)
	link-to-ori link-to-new link-from)
    (goto-char (point-max))
    (while (setq link-to-new (wdired-get-previous-link))
      (setq link-to-ori (wdired-get-previous-link t t))
      (setq link-from (wdired-get-filename nil t))
      (if (not (equal link-to-new link-to-ori))
          (progn
            (setq changes t)
            (if (equal link-to-new "") ;empty filename!
                (setq link-to-new "/dev/null"))
	    (condition-case err
		(progn 
		  (delete-file link-from)
		  (make-symbolic-link
		   (substitute-in-file-name link-to-new) link-from))
		  (error
		   (setq errors (1+ errors))
		   (dired-log (concat "Link `" link-from "' to `"
				      link-to-new "' failed:\n%s\n")
			      err))))))
    (cons changes errors)))

;; Perform a "case command" skipping read-only words.
(defun wdired-xcase-word (command arg)
  (if (< arg 0)
      (funcall command arg)
    (progn
      (while (> arg 0)
	(condition-case err
	    (progn
	      (funcall command 1)
	      (setq arg (1- arg)))
	  (error
	   (if (not (forward-word 1))
	       (setq arg 0))))))))

(defun wdired-downcase-word (arg)
  "Wdired version of `downcase-word'.
Like original function but it skips read-only words."
  (interactive "p")
  (wdired-xcase-word 'downcase-word arg))

(defun wdired-upcase-word (arg)
  "Wdired version of `upcase-word'.
Like original function but it skips read-only words."
  (interactive "p")
  (wdired-xcase-word 'upcase-word arg))

(defun wdired-capitalize-word (arg)
  "Wdired version of `capitalize-word'.
Like original function but it skips read-only words."
  (interactive "p")
  (wdired-xcase-word 'capitalize-word arg))

;; The following code is related to advice some interactive functions
;; to make some editing commands in wdired mode not to fail trying to
;; change read-only text. Notice that some advises advice and unadvise
;; them-self to another functions: search-forward and
;; re-search-forward. This is to keep these functions advised only
;; when is necessary. Since they are built-in commands used heavily in
;; lots of places, to have it permanently advised would cause some
;; performance loss.


(defun wdired-add-skip-in-replace (command)
  "Advice COMMAND to skip matches while they have read-only properties.
This is useful to avoid \"read-only\" errors in search and replace
commands.  This advice only has effect in wdired mode."
  (eval
    `(defadvice ,command (around wdired-discard-read-only activate)
       ,(format "Make %s to work better with wdired,\n%s."  command
		"skipping read-only matches when invoked without argument")
       ad-do-it
       (if (eq major-mode 'wdired-mode)
	   (while (and ad-return-value
		       (text-property-any
			(max 1 (1- (match-beginning 0))) (match-end 0)
			'read-only t))
	     ad-do-it))
       ad-return-value)))


(defun wdired-add-replace-advice (command)
  "Advice COMMAND to skip matches while they have read-only properties.
This is useful to avoid \"read-only\" errors in search and replace
commands.  This advice only has effect in wdired mode."
  (eval
   `(defadvice ,command (around wdired-grok-read-only activate)
       ,(format "Make %s to work better with wdired,\n%s."  command
		"skipping read-only matches when invoked without argument")
       (if (eq major-mode 'wdired-mode)
           (progn
             (wdired-add-skip-in-replace 'search-forward)
             (wdired-add-skip-in-replace 're-search-forward)
             (unwind-protect 
                 ad-do-it
               (progn
                 (ad-remove-advice 'search-forward
                                   'around 'wdired-discard-read-only)
                 (ad-remove-advice 're-search-forward
                                   'around 'wdired-discard-read-only)
                 (ad-update 'search-forward)
                 (ad-update 're-search-forward))))
         ad-do-it)
       ad-return-value)))


(if wdired-advise-functions
    (progn
      (mapcar 'wdired-add-replace-advice
              '(query-replace query-replace-regexp replace-string))))


;; The following code deals with changing the access bits (or
;; permissions) of the files.

(defvar wdired-perm-mode-map nil)
(unless wdired-perm-mode-map
  (setq wdired-perm-mode-map (copy-keymap wdired-mode-map))
  (define-key wdired-perm-mode-map " " 'wdired-toggle-bit)
  (define-key wdired-perm-mode-map "r" 'wdired-set-bit)
  (define-key wdired-perm-mode-map "w" 'wdired-set-bit)
  (define-key wdired-perm-mode-map "x" 'wdired-set-bit)
  (define-key wdired-perm-mode-map "-" 'wdired-set-bit)
  (define-key wdired-perm-mode-map "S" 'wdired-set-bit)
  (define-key wdired-perm-mode-map "s" 'wdired-set-bit)
  (define-key wdired-perm-mode-map "T" 'wdired-set-bit)
  (define-key wdired-perm-mode-map "t" 'wdired-set-bit)
  (define-key wdired-perm-mode-map "s" 'wdired-set-bit)
  (define-key wdired-perm-mode-map "l" 'wdired-set-bit)
  (define-key wdired-perm-mode-map [down-mouse-1] 'wdired-mouse-toggle-bit))

;; Put a local-map to the permission bits of the files, and store the
;; original name and permissions as a property
(defun wdired-preprocess-perms()
  (let ((inhibit-read-only t)
	filename)
    (set (make-local-variable 'wdired-col-perm) nil)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(if (and (not (looking-at dired-re-sym))
		 (setq filename (wdired-get-filename)))
	    (progn
	      (re-search-forward dired-re-perms)
	      (or wdired-col-perm
		  (setq wdired-col-perm (- (current-column) 9)))
	      (if (eq wdired-allow-to-change-permissions 'advanced)
		  (put-text-property (match-beginning 0) (match-end 0)
				     'read-only nil)
		(put-text-property (1+ (match-beginning 0)) (match-end 0)
				   'local-map wdired-perm-mode-map))
	      (put-text-property (match-end 0) (1+ (match-end 0)) 'end-perm t)
	      (put-text-property (match-beginning 0) (1+ (match-beginning 0))
				 'old-perm (match-string-no-properties 0))))
        (forward-line)
	(beginning-of-line)))))

(defun wdired-perm-allowed-in-pos (char pos)
  (cond
   ((= char ?-)          t)
   ((= char ?r)          (= (% pos 3) 0))
   ((= char ?w)          (= (% pos 3) 1))
   ((= char ?x)          (= (% pos 3) 2))
   ((memq char '(?s ?S)) (memq pos '(2 5)))
   ((memq char '(?t ?T)) (= pos 8))
   ((= char ?l)          (= pos 5))))

(defun wdired-set-bit ()
  "Set a permission bit character."
  (interactive)
  (if (wdired-perm-allowed-in-pos last-command-char
                                  (- (current-column) wdired-col-perm))
      (let ((new-bit (char-to-string last-command-char))
            (inhibit-read-only t)
	    (pos-prop (- (point) (- (current-column) wdired-col-perm))))
        (put-text-property 0 1 'local-map wdired-perm-mode-map new-bit)
        (put-text-property 0 1 'read-only t new-bit)
        (insert new-bit)
        (delete-char 1)
	(put-text-property pos-prop (1- pos-prop) 'perm-changed t))
    (forward-char 1)))

(defun wdired-toggle-bit()
  "Toggle the permission bit at point."
  (interactive)
  (let ((inhibit-read-only t)
	(new-bit "-")
	(pos-prop (- (point) (- (current-column) wdired-col-perm))))
    (if (eq (char-after (point)) ?-)
	(setq new-bit	
	      (if (= (% (- (current-column) wdired-col-perm) 3) 0) "r"
		(if (= (% (- (current-column) wdired-col-perm) 3) 1) "w"
		  "x"))))
    (put-text-property 0 1 'local-map wdired-perm-mode-map new-bit)
    (put-text-property 0 1 'read-only t new-bit)
    (insert new-bit)
    (delete-char 1)
    (put-text-property pos-prop (1- pos-prop) 'perm-changed t)))

(defun wdired-mouse-toggle-bit (event)
  "Toggle the permission bit that was left clicked."
  (interactive "e")
  (mouse-set-point event)
  (wdired-toggle-bit))

;; Allowed chars for 4000 bit are Ss  in position 3
;; Allowed chars for 2000 bit are Ssl in position 6
;; Allowed chars for 1000 bit are Tt  in position 9
(defun wdired-perms-to-number (perms)
  (let ((nperm 0777))
    (if (= (elt perms 1) ?-) (setq nperm (- nperm 400)))
    (if (= (elt perms 2) ?-) (setq nperm (- nperm 200)))
    (let ((p-bit (elt perms 3)))
      (if (memq p-bit '(?- ?S)) (setq nperm (- nperm 100)))
      (if (memq p-bit '(?s ?S)) (setq nperm (+ nperm 4000))))
    (if (= (elt perms 4) ?-) (setq nperm (- nperm 40)))
    (if (= (elt perms 5) ?-) (setq nperm (- nperm 20)))
    (let ((p-bit (elt perms 6)))
      (if (memq p-bit '(?- ?S ?l)) (setq nperm (- nperm 10)))
      (if (memq p-bit '(?s ?S ?l)) (setq nperm (+ nperm 2000))))
    (if (= (elt perms 7) ?-) (setq nperm (- nperm 4)))
    (if (= (elt perms 8) ?-) (setq nperm (- nperm 2)))
    (let ((p-bit (elt perms 9)))
      (if (memq p-bit '(?- ?T)) (setq nperm (- nperm 1)))
      (if (memq p-bit '(?t ?T)) (setq nperm (+ nperm 1000))))
    nperm))

;; Perform the changes in the permissions of the files that have
;; changed.
(defun wdired-do-perm-changes ()
  (let ((changes nil)
	(errors 0)
	(prop-wanted (if (eq wdired-allow-to-change-permissions 'advanced)
			 'old-perm 'perm-changed))
	filename perms-ori perms-new perm-tmp)
    (goto-char (next-single-property-change (point-min) prop-wanted
					    nil (point-max)))
    (while (not (eobp))
      (setq perms-ori (get-text-property (point) 'old-perm))
      (setq perms-new (buffer-substring-no-properties
		       (point) (next-single-property-change (point) 'end-perm)))
      (if (not (equal perms-ori perms-new))
	  (progn
	    (setq changes t)
	    (setq filename (wdired-get-filename nil t))
	    (if (= (length perms-new) 10)
		(progn
		  (setq perm-tmp
			(int-to-string (wdired-perms-to-number perms-new)))
		  (if (not (equal 0 (dired-call-process dired-chmod-program
				     t perm-tmp filename)))
		      (progn
			(setq errors (1+ errors))
			(dired-log (concat dired-chmod-program " " perm-tmp
					   " `" filename "' failed\n\n")))))
	    (setq errors (1+ errors))
	    (dired-log (concat "Cannot parse permission `" perms-new
			       "' for file `" filename "'\n\n")))))
      (goto-char (next-single-property-change (1+ (point)) prop-wanted
					      nil (point-max))))
    (cons changes errors)))

(provide 'wdired)

;; arch-tag: bc00902e-526f-4305-bc7f-8862a559184f
;;; wdired.el ends here
