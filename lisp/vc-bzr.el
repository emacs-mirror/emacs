;;; vc-bzr.el --- VC backend for the bzr revision control system

;; Copyright (C) 2006, 2007  Free Software Foundation, Inc.

;; NOTE: THIS IS A MODIFIED VERSION OF Dave Love's vc-bzr.el,
;; which you can find at: http://www.loveshack.ukfsn.org/emacs/vc-bzr.el
;; I could not get in touch with Dave Love by email, so 
;; I am releasing my changes separately. -- Riccardo

;; Author: Dave Love <fx@gnu.org>, Riccardo Murri <riccardo.murri@gmail.com>
;; Keywords: tools
;; Created: Sept 2006
;; Version: 2007-05-24
;; URL: http://launchpad.net/vc-bzr

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; NOTE: THIS IS A MODIFIED VERSION OF Dave Love's vc-bzr.el,
;; which you can find at: http://www.loveshack.ukfsn.org/emacs/vc-bzr.el

;; See <URL:http://bazaar-vcs.org/> concerning bzr.

;; Load this library to register bzr support in VC.  It covers basic VC 
;; functionality, but was only lightly exercised with a few Emacs/bzr
;; version combinations, namely those current on the authors' PCs.
;; See various Fixmes below.


;; Known bugs
;; ==========

;; When edititing a symlink and *both* the symlink and its target
;; are bzr-versioned, `vc-bzr` presently runs `bzr status` on the
;; symlink, thereby not detecting whether the actual contents
;; (that is, the target contents) are changed.  
;; See https://bugs.launchpad.net/vc-bzr/+bug/116607

;; For an up-to-date list of bugs, please see:
;;   https://bugs.launchpad.net/vc-bzr/+bugs


;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'vc))                        ; for vc-exec-after

(defgroup vc-bzr nil
  "VC bzr backend."
;;   :version "22"
  :group 'vc)

(defcustom vc-bzr-program "bzr"
  "Name of the bzr command (excluding any arguments)."
  :group 'vc-bzr
  :type 'string)

;; Fixme: there's probably no call for this.
(defcustom vc-bzr-program-args nil
  "List of global arguments to pass to `vc-bzr-program'."
  :group 'vc-bzr
  :type '(repeat string))

(defcustom vc-bzr-diff-switches nil
  "String/list of strings specifying extra switches for bzr diff under VC."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List" :value ("") string))
  :group 'vc-bzr)

(defvar vc-bzr-version nil
  "Internal use.")

;; Could be used for compatibility checks if bzr changes.
(defun vc-bzr-version ()
  "Return a three-numeric element list with components of the bzr version.
This is of the form (X Y Z) for revision X.Y.Z.  The elements are zero
if running `vc-bzr-program' doesn't produce the expected output."
  (or vc-bzr-version
      (setq vc-bzr-version
            (let ((s (shell-command-to-string
                      (concat (shell-quote-argument vc-bzr-program)
                              " --version"))))
              (if (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$" s)
                  (list (string-to-number (match-string 1 s))
                        (string-to-number (match-string 2 s))
                        (string-to-number (match-string 3 s)))
                '(0 0 0))))))

(defun vc-bzr-at-least-version (vers)
  "Return t if the bzr command reports being a least version VERS.
First argument VERS is a list of the form (X Y Z), as returned by `vc-bzr-version'."
  (version-list-<= vers (vc-bzr-version)))

;; since v0.9, bzr supports removing the progress indicators
;; by setting environment variable BZR_PROGRESS_BAR to "none".
(defun vc-bzr-command (bzr-command buffer okstatus file &rest args)
  "Wrapper round `vc-do-command' using `vc-bzr-program' as COMMAND.
Invoke the bzr command adding `BZR_PROGRESS_BAR=none' to the environment."
  (let ((process-environment
         (list* "BZR_PROGRESS_BAR=none" ; Suppress progress output (bzr >=0.9)
                "LC_ALL=C"              ; Force English output
                process-environment))
        ;; bzr may attempt some kind of user interaction if its stdin/stdout
        ;; is connected to a PTY; therefore, ask Emacs to use a pipe to
        ;; communicate with it.
        ;; This is redundant because vc-do-command does it already.  --Stef
        (process-connection-type nil))
    (apply 'vc-do-command buffer okstatus vc-bzr-program
           file bzr-command (append vc-bzr-program-args args))))
  
(unless (vc-bzr-at-least-version '(0 9))
  ;; For older versions, we fall back to washing the log buffer
  ;; when all output has been gathered.
  (defun vc-bzr-post-command-function (command file flags)
    "`vc-post-command-functions' function to remove progress messages."
    ;; Note that using this requires that the vc command is run
    ;; synchronously.  Otherwise, the ^Ms in the leading progress
    ;; message on stdout cause the stream to be interpreted as having
    ;; DOS line endings, losing the ^Ms, so the search fails.  I don't
    ;; know how this works under Windows.
    (when (equal command vc-bzr-program)
      (save-excursion
        (goto-char (point-min))
        (if (looking-at "^\\(\r.*\r\\)[^\r]+$")
            (replace-match "" nil nil nil 1)))
      (save-excursion
        (goto-char (point-min))
        ;; This is inserted by bzr 0.11 `log', at least
        (while (looking-at "read knit.*\n")
          (replace-match "")))))

  (add-hook 'vc-post-command-functions 'vc-bzr-post-command-function))

;;;###autoload
(defconst vc-bzr-admin-dirname ".bzr")    ; FIXME: "_bzr" on w32?

;;;###autoload (defun vc-bzr-registered (file)
;;;###autoload   (if (vc-find-root file vc-bzr-admin-dirname)
;;;###autoload       (progn
;;;###autoload         (load "vc-bzr")
;;;###autoload         (vc-bzr-registered file))))

(defun vc-bzr-root-dir (file)
  "Return the root directory in the hierarchy above FILE.
Return nil if there isn't one."
  (vc-find-root file vc-bzr-admin-dirname))

(defun vc-bzr-registered (file)
  "Return non-nil if FILE is registered with bzr."
  (if (vc-bzr-root-dir file) ; Short cut.
      (vc-bzr-state file)))                    ; Expensive.

(defun vc-bzr-buffer-nonblank-p (&optional buffer)
  "Return non-nil if BUFFER contains any non-blank characters."
  (or (> (buffer-size buffer) 0)
      (save-excursion
        (set-buffer (or buffer (current-buffer)))
        (goto-char (point-min))
        (re-search-forward "[^ \t\n]" (point-max) t))))

(defconst vc-bzr-state-words
  "added\\|ignored\\|modified\\|removed\\|renamed\\|unknown"
  "Regexp matching file status words as reported in `bzr' output.")

;; FIXME:  Also get this in a non-registered sub-directory.
(defun vc-bzr-state (file)
  (with-temp-buffer
    (cd (file-name-directory file))
    (let ((ret (vc-bzr-command "status" t 255 file))
          (state 'up-to-date))
      ;; the only secure status indication in `bzr status' output
      ;; is a couple of lines following the pattern::
      ;;   | <status>:
      ;;   |   <file name>
      ;; if the file is up-to-date, we get no status report from `bzr',
      ;; so if the regexp search for the above pattern fails, we consider
      ;; the file to be up-to-date.
      (goto-char (point-min))
      (when
          (re-search-forward 
           (concat "^\\(" vc-bzr-state-words "\\):[ \t\n]+" 
                   (file-name-nondirectory file) "[ \t\n]*$")
           (point-max) t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (goto-char start)
          (setq state
                (cond 
                 ((not (equal ret 0)) nil)
                 ((looking-at "added\\|renamed\\|modified\\|removed") 'edited)
                 ((looking-at "unknown\\|ignored") nil)))
          ;; erase the status text that matched
          (delete-region start end)))
      (when (vc-bzr-buffer-nonblank-p)
        ;; "bzr" will output some warnings and informational messages
        ;; to the user to stderr; due to Emacs' `vc-do-command' (and,
        ;; it seems, `start-process' itself), we cannot catch stderr
        ;; and stdout into different buffers.  So, if there's anything
        ;; left in the buffer after removing the above status
        ;; keywords, let us just presume that any other message from
        ;; "bzr" is a user warning, and display it.
        (message "Warnings in `bzr' output: %s" 
               (buffer-substring (point-min) (point-max))))
      (when state
        (vc-file-setprop file 'vc-workfile-version
                         (vc-bzr-workfile-version file))
        (vc-file-setprop file 'vc-state state))
      state)))

(defun vc-bzr-workfile-unchanged-p (file)
  (eq 'up-to-date (vc-bzr-state file)))

(defun vc-bzr-workfile-version (file)
  (with-temp-buffer
    (vc-bzr-command "revno" t 0 file)
    (goto-char 1)
    (buffer-substring 1 (line-end-position))))

(defun vc-bzr-checkout-model (file)
  'implicit)

(defun vc-bzr-register (file &optional rev comment)
  "Register FILE under bzr.
Signal an error unless REV is nil.
COMMENT is ignored."
  (if rev (error "Can't register explicit version with bzr"))
  (vc-bzr-command "add" nil 0 file))

;; Could run `bzr status' in the directory and see if it succeeds, but
;; that's relatively expensive.
(defalias 'vc-bzr-responsible-p 'vc-bzr-root-dir
  "Return non-nil if FILE is (potentially) controlled by bzr.
The criterion is that there is a `.bzr' directory in the same
or a superior directory.")

(defun vc-bzr-could-register (file)
  "Return non-nil if FILE could be registered under bzr."
  (and (vc-bzr-responsible-p file)      ; shortcut
       (condition-case ()
           (with-temp-buffer
             (vc-bzr-command "add" t 0 file "--dry-run")
             ;; The command succeeds with no output if file is
             ;; registered (in bzr 0.8).
             (goto-char 1)
             (looking-at "added "))
         (error))))

(defun vc-bzr-unregister (file)
  "Unregister FILE from bzr."
  (vc-bzr-command "remove" nil 0 file))

(defun vc-bzr-checkin (file rev comment)
  "Check FILE in to bzr with log message COMMENT.
REV non-nil gets an error."
  (if rev (error "Can't check in a specific version with bzr"))
  (vc-bzr-command "commit" nil 0 file "-m" comment))

(defun vc-bzr-checkout (file &optional editable rev destfile)
  "Checkout revision REV of FILE from bzr to DESTFILE.
EDITABLE is ignored."
  (unless destfile
    (setq destfile (vc-version-backup-file-name file rev)))
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary))
  (with-temp-file destfile
    (if rev
        (vc-bzr-command "cat" t 0 file "-r" rev)
      (vc-bzr-command "cat" t 0 file)))))

(defun vc-bzr-revert (file &optional contents-done)
  (unless contents-done
    (with-temp-buffer (vc-bzr-command "revert" t 'async file))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-current-tag-function)

(define-derived-mode vc-bzr-log-view-mode log-view-mode "Bzr-Log-View"
  (remove-hook 'log-view-mode-hook 'vc-bzr-log-view-mode) ;Deactivate the hack.
  (require 'add-log)
  ;; Don't have file markers, so use impossible regexp.
  (set (make-local-variable 'log-view-file-re) "\\'\\`")
  (set (make-local-variable 'log-view-message-re)
       "^ *-+\n *\\(?:revno: \\([0-9]+\\)\\|merged: .+\\)")
  (set (make-local-variable 'log-view-font-lock-keywords)
       ;; log-view-font-lock-keywords is careful to use the buffer-local
       ;; value of log-view-message-re only since Emacs-23.
       (append `((,log-view-message-re . 'log-view-message-face))
               ;; log-view-font-lock-keywords
               '(("^ *committer: \
\\([^<(]+?\\)[  ]*[(<]\\([[:alnum:]_.-]+@[[:alnum:]_.-]+\\)[>)]"
                  (1 'change-log-name)
                  (2 'change-log-email))
                 ("^ *timestamp: \\(.*\\)" (1 'change-log-date-face))))))

(defun vc-bzr-print-log (file &optional buffer) ; get buffer arg in Emacs 22
  "Get bzr change log for FILE into specified BUFFER."
  ;; Fixme: This might need the locale fixing up if things like `revno'
  ;; got localized, but certainly it shouldn't use LC_ALL=C.
  ;; NB.  Can't be async -- see `vc-bzr-post-command-function'.
  (vc-bzr-command "log" buffer 0 file)
  ;; FIXME: Until Emacs-23, VC was missing a hook to sort out the mode for
  ;; the buffer, or at least set the regexps right.
  (unless (fboundp 'vc-default-log-view-mode)
    (add-hook 'log-view-mode-hook 'vc-bzr-log-view-mode)))

(defun vc-bzr-show-log-entry (version)
  "Find entry for patch name VERSION in bzr change log buffer."
  (goto-char (point-min))
  (let (case-fold-search)
    (if (re-search-forward (concat "^-+\nrevno: " version "$") nil t)
        (beginning-of-line 0)
      (goto-char (point-min)))))

;; Fixem: vc-bzr-wash-log

(autoload 'vc-diff-switches-list "vc" nil nil t)

(defun vc-bzr-diff (file &optional rev1 rev2 buffer)
  "VC bzr backend for diff."
  (let ((working (vc-workfile-version file)))
    (if (and (equal rev1 working) (not rev2))
        (setq rev1 nil))
    (if (and (not rev1) rev2)
        (setq rev1 working))
    ;; NB.  Can't be async -- see `vc-bzr-post-command-function'.
    ;; bzr diff produces condition code 1 for some reason.
    (apply #'vc-bzr-command "diff" (or buffer "*vc-diff*") 1 file
           "--diff-options" (mapconcat 'identity (vc-diff-switches-list bzr)
                                       " ")
           (when rev1
             (if rev2
                 (list "-r" (format "%s..%s" rev1 rev2))
               (list "-r" rev1))))))

(defalias 'vc-bzr-diff-tree 'vc-bzr-diff)

;; Fixme: implement vc-bzr-dir-state, vc-bzr-dired-state-info

;; Fixme: vc-{next,previous}-version need fixing in vc.el to deal with
;; straight integer versions.

(defun vc-bzr-delete-file (file)
  "Delete FILE and delete it in the bzr repository."
  (condition-case ()
      (delete-file file)
    (file-error nil))
  (vc-bzr-command "remove" nil 0 file))

(defun vc-bzr-rename-file (old new)
  "Rename file from OLD to NEW using `bzr mv'."
  (vc-bzr-command "mv" nil 0 new old))

(defvar vc-bzr-annotation-table nil
  "Internal use.")
(make-variable-buffer-local 'vc-bzr-annotation-table)

(defun vc-bzr-annotate-command (file buffer &optional version)
  "Prepare BUFFER for `vc-annotate' on FILE.
Each line is tagged with the revision number, which has a `help-echo'
property containing author and date information."
  (apply #'vc-bzr-command "annotate" buffer 0 file "-l" "--all"
         (if version (list "-r" version)))
  (with-current-buffer buffer
    ;; Store the tags for the annotated source lines in a hash table
    ;; to allow saving space by sharing the text properties.
    (setq vc-bzr-annotation-table (make-hash-table :test 'equal))
    (goto-char (point-min))
    (while (re-search-forward "^\\( *[0-9]+\\) \\(.+\\) +\\([0-9]\\{8\\}\\) |"
                              nil t)
      (let* ((rev (match-string 1))
             (author (match-string 2))
             (date (match-string 3))
             (key (match-string 0))
             (tag (gethash key vc-bzr-annotation-table)))
        (unless tag
          (save-match-data
            (string-match " +\\'" author)
            (setq author (substring author 0 (match-beginning 0))))
          (setq tag (propertize rev 'help-echo (concat "Author: " author
                                                       ", date: " date)
                                'mouse-face 'highlight))
          (puthash key tag vc-bzr-annotation-table))
        (replace-match "")
        (insert tag " |")))))

;; Definition from Emacs 22
(unless (fboundp 'vc-annotate-convert-time)
(defun vc-annotate-convert-time (time)
  "Convert a time value to a floating-point number of days.
The argument TIME is a list as returned by `current-time' or
`encode-time', only the first two elements of that list are considered."
  (/ (+ (* (float (car time)) (lsh 1 16)) (cadr time)) 24 3600)))

(defun vc-bzr-annotate-time ()
  (when (re-search-forward "^ *[0-9]+ |" nil t)
    (let ((prop (get-text-property (line-beginning-position) 'help-echo)))
      (string-match "[0-9]+\\'" prop)
      (vc-annotate-convert-time
       (encode-time 0 0 0
                    (string-to-number (substring (match-string 0 prop) 6 8))
                    (string-to-number (substring (match-string 0 prop) 4 6))
                    (string-to-number (substring (match-string 0 prop) 0 4))
                    )))))

(defun vc-bzr-annotate-extract-revision-at-line ()
  "Return revision for current line of annoation buffer, or nil.
Return nil if current line isn't annotated."
  (save-excursion
    (beginning-of-line)
    (if (looking-at " *\\([0-9]+\\) | ")
        (match-string-no-properties 1))))

;; Not needed for Emacs 22
(defun vc-bzr-annotate-difference (point)
  (let ((next-time (vc-bzr-annotate-time)))
    (if next-time
        (- (vc-annotate-convert-time (current-time)) next-time))))

;; FIXME: `bzr root' will return the real path to the repository root,
;; that is, it can differ from the buffer's current directory name
;; if there are any symbolic links.
(defun vc-bzr-root (dir)
  "Return the root directory of the bzr repository containing DIR."
  ;; Cache technique copied from vc-arch.el.
  (or (vc-file-getprop dir 'bzr-root)
      (vc-file-setprop
       dir 'bzr-root
       (substring 
	(shell-command-to-string (concat vc-bzr-program " root " dir)) 0 -1))))

;; TODO: it would be nice to mark the conflicted files in  VC Dired,
;; and implement a command to run ediff and `bzr resolve' once the 
;; changes have been merged.
(defun vc-bzr-dir-state (dir &optional localp)
  "Find the VC state of all files in DIR.
Optional argument LOCALP is always ignored."
  (let ((bzr-root-directory (vc-bzr-root dir))
        (at-start t)
        current-bzr-state current-vc-state)
    ;; Check that DIR is a bzr repository.
    (unless (file-name-absolute-p bzr-root-directory)
      (error "Cannot find bzr repository for directory `%s'" dir))
    ;; `bzr ls --versioned' lists all versioned files;
    ;; assume they are up-to-date, unless we are given
    ;; evidence of the contrary.
    (setq at-start t)
    (with-temp-buffer
      (vc-bzr-command "ls" t 0 nil "--versioned" "--non-recursive")
      (goto-char (point-min))
      (while (or at-start
                 (eq 0 (forward-line)))
        (setq at-start nil)
        (let ((file (expand-file-name
                     (buffer-substring-no-properties 
                      (line-beginning-position) (line-end-position))
                     bzr-root-directory)))
          (vc-file-setprop file 'vc-state 'up-to-date)
          ;; XXX: is this correct? what happens if one 
          ;; mixes different SCMs in the same dir?
          (vc-file-setprop file 'vc-backend 'BZR))))
    ;; `bzr status' reports on added/modified/renamed and unknown/ignored files
    (setq at-start t)
    (with-temp-buffer 
      (vc-bzr-command "status" t 0 nil)
      (goto-char (point-min))
      (while (or at-start
                 (eq 0 (forward-line)))
        (setq at-start nil)
        (cond 
         ((looking-at "^added") 
          (setq current-vc-state 'edited)
          (setq current-bzr-state 'added))
         ((looking-at "^modified") 
          (setq current-vc-state 'edited)
          (setq current-bzr-state 'modified))
         ((looking-at "^renamed") 
          (setq current-vc-state 'edited)
          (setq current-bzr-state 'renamed))
         ((looking-at "^\\(unknown\\|ignored\\)")
          (setq current-vc-state nil)
          (setq current-bzr-state 'not-versioned))
         ((looking-at "  ")
          ;; file names are indented by two spaces
          (when current-vc-state
            (let ((file (expand-file-name
                         (buffer-substring-no-properties
                          (match-end 0) (line-end-position))
                         bzr-root-directory)))
              (vc-file-setprop file 'vc-state current-vc-state)
              (vc-file-setprop file 'vc-bzr-state current-bzr-state)
              (when (eq 'added current-bzr-state)
                (vc-file-setprop file 'vc-workfile-version "0"))))
          (when (eq 'not-versioned current-bzr-state)
            (let ((file (expand-file-name
                         (buffer-substring-no-properties
                          (match-end 0) (line-end-position))
                         bzr-root-directory)))
              (vc-file-setprop file 'vc-backend 'none)
              (vc-file-setprop file 'vc-state nil))))
         (t
          ;; skip this part of `bzr status' output
          (setq current-vc-state nil)
          (setq current-bzr-state nil)))))))

(defun vc-bzr-dired-state-info (file)
  "Bzr-specific version of `vc-dired-state-info'."
  (if (eq 'edited (vc-state file))
      (let ((bzr-state (vc-file-getprop file 'vc-bzr-state)))
        (if bzr-state
            (concat "(" (symbol-name bzr-state) ")")
          ;; else fall back to default vc representation
          (vc-default-dired-state-info 'BZR file)))))

;; In case of just `(load "vc-bzr")', but that's probably the wrong
;; way to do it.
(add-to-list 'vc-handled-backends 'BZR)

(eval-after-load "vc"
  '(add-to-list 'vc-directory-exclusion-list ".bzr" t))

(defconst vc-bzr-unload-hook
  (lambda ()
    (setq vc-handled-backends (delq 'BZR vc-handled-backends))
    (remove-hook 'vc-post-command-functions 'vc-bzr-post-command-function)))

(provide 'vc-bzr)
;; arch-tag: 8101bad8-4e92-4e7d-85ae-d8e08b4e7c06
;;; vc-bzr.el ends here
