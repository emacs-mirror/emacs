;;; dired-x.el --- Sebastian Kremer's Extra DIRED hacked up for GNU Emacs19

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;;	Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Maintainer: Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Version: 2.37+
;; Date: 1994/08/18 19:27:42
;; Keywords: dired extensions

;; Copyright (C) 1993, 1994 Free Software Foundation

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This is Sebastian Kremer's excellent dired-x.el (Dired Extra), version
;;; 1.191, hacked up for GNU Emacs 19.  Redundant or conflicting material
;;; has been removed or renamed in order to work properly with dired of
;;; GNU Emacs 19.  All suggestions or comments are most welcomed.

;;;  
;;; Please, PLEASE, *PLEASE* see the info pages.
;;; 
 
;;; BUGS: Type M-x dired-x-submit-report and a report will be generated.

;;; INSTALLATION: In your ~/.emacs,
;;;
;;; (add-hook 'dired-load-hook
;;;           (function (lambda ()
;;;                       (load "dired-x")
;;;                       ;; Set global variables here.  For example:
;;;                       ;; (setq dired-guess-shell-gnutar "gtar")
;;;                       )))
;;; (add-hook 'dired-mode-hook
;;;           (function (lambda ()
;;;                       ;; Set buffer-local variables here.  For example:
;;;                       ;; (setq dired-omit-files-p t)
;;;                       )))
;;;
;;; At load time dired-x.el will install itself, redefine some functions, and
;;; bind some dired keys.  *Please* see the info pages for more details.

;;; CAUTION: If you are using a version of GNU Emacs earlier than 19.20 than
;;; you may have to edit dired.el.  The copy of dired.el in GNU Emacs versions
;;; earlier than 19.20 incorrectly had the call to run-hooks *before* the call
;;; to provide.  In such a case, it is possible that byte-compiling and/or
;;; loading dired can cause an infinite loop.  To prevent this, make sure the
;;; line of code
;;;  
;;;         (run-hooks 'dired-load-hook) 
;;;  
;;; is the *last* executable line in the file dired.el.  That is, make sure it
;;; comes *after* the line
;;;  
;;;         (provide 'dired) 
;;; 
;;; *Please* see the info pages for more details. 

;;; User defined variables:
;;;
;;;      dired-bind-vm
;;;      dired-vm-read-only-folders
;;;      dired-bind-jump
;;;      dired-bind-info
;;;      dired-bind-man
;;;      dired-x-hands-off-my-keys 
;;;      dired-find-subdir
;;;      dired-enable-local-variables
;;;      dired-local-variables-file
;;;      dired-guess-shell-gnutar
;;;      dired-guess-shell-gzip-quiet
;;;      dired-guess-shell-znew-switches
;;;      dired-guess-shell-alist-user
;;;      dired-clean-up-buffers-too
;;;      dired-omit-files-p
;;;      dired-omit-files
;;;      dired-omit-extensions
;;;
;;; To find out more about these variables, load this file, put your cursor at
;;; the end of any of the variable names, and hit C-h v [RET].  *Please* see
;;; the info pages for more details.

;;; When loaded this code redefines the following functions of GNU Emacs
;;;
;;;   Function                         Found in this file of GNU Emacs
;;;   --------                         -------------------------------
;;;   dired-clean-up-after-deletion    ../lisp/dired.el
;;;   dired-find-buffer-nocreate       ../lisp/dired.el
;;;   dired-initial-position           ../lisp/dired.el
;;;   dired-up-directory               ../lisp/dired.el
;;;
;;;   dired-add-entry                  ../lisp/dired-aux.el
;;;   dired-read-shell-command         ../lisp/dired-aux.el
;;;
;;; One drawback is that dired-x.el will load dired-aux.el as soon as dired is
;;; loaded.  Thus, the advantage of separating out non-essential dired stuff
;;; into dired-aux.el and only loading when necessary will be lost.  Please
;;; note also that some of the comments in dired.el and dired-aux.el are
;;; Kremer's that referred to the old dired-x.el.  This now should be referring
;;; to this program.  (This is also a good reason to call this dired-x.el
;;; instead of dired-x19.el.)


;;;; Code:

;;; LOAD.

;;; This is a no-op if dired-x is being loaded via `dired-load-hook'.  It is
;;; here in case the user has autoloaded dired-x via the dired-jump key binding
;;; (instead of autoloading to dired as is suggested in the info-pages).

(require 'dired)

;;; We will redefine some functions and also need some macros so we need to
;;; load dired stuff of GNU Emacs.

(require 'dired-aux)

;;;; User-defined variables.

(defvar dired-bind-vm nil
  "*t says \"V\" in dired-mode will `dired-vm', otherwise \"V\" is `dired-rmail'.
Also, RMAIL files contain -*- rmail -*- at the top so \"f\",
`dired-advertised-find-file', will run rmail.")

(defvar dired-bind-jump t
  "*t says bind `dired-jump' to C-x C-j, otherwise do not.")

(defvar dired-bind-man t
  "*t says bind `dired-man' to \"N\" in dired-mode, otherwise do not.")

(defvar dired-bind-info t
  "*t says bind `dired-info' to \"I\" in dired-mode, otherwise do not.")

(defvar dired-vm-read-only-folders nil
  "*If t, \\[dired-vm] will visit all folders read-only.
If neither nil nor t, e.g. the symbol `if-file-read-only', only
files not writable by you are visited read-only.

Read-only folders only work in VM 5, not in VM 4.")

(defvar dired-omit-files-p nil
  "*If non-nil, \"uninteresting\" files are not listed (buffer-local).
Use \\[dired-omit-toggle] to toggle its value.
Uninteresting files are those whose filenames match regexp `dired-omit-files',
plus those ending with extensions in `dired-omit-extensions'.")
(make-variable-buffer-local 'dired-omit-files-p)

(defvar dired-omit-files "^#\\|^\\.$\\|^\\.\\.$"
  "*Filenames matching this regexp will not be displayed.
This only has effect when `dired-omit-files-p' is t.  See interactive function
`dired-omit-toggle' \(\\[dired-omit-toggle]\) and variable
`dired-omit-extensions'.  The default is to omit  `.', `..', and auto-save
files.")

(defvar dired-find-subdir nil           ; t is pretty near to DWIM...
  "*If non-nil, Dired always finds a directory in a buffer of its own.
If nil, Dired finds the directory as a subdirectory in some other buffer
if it is present as one.

If there are several Dired buffers for a directory, the most recently
used is chosen.

Dired avoids switching to the current buffer, so that if you have
a normal and a wildcard buffer for the same directory, C-x d RET will
toggle between those two.")

(defvar dired-enable-local-variables t
  "*Control use of local-variables lists in dired.
The value can be t, nil or something else.
A value of t means local-variables lists are obeyed;
nil means they are ignored; anything else means query.

This temporarily overrides the value of `enable-local-variables' when listing
a directory.  See also `dired-local-variables-file'.")

(defvar dired-guess-shell-gnutar nil
  "*If non-nil, name of GNU tar executable (e.g., \"tar\" or \"gtar\") and `z'
switch will be used for compressed or gzip'ed tar files.  If no GNU tar, set
to nil: a pipe using `zcat' or `gunzip -c' will be used.")

(defvar dired-guess-shell-gzip-quiet t
  "*non-nil says pass -q to gzip overriding verbose GZIP environment.")

(defvar dired-guess-shell-znew-switches nil
  "*If non-nil, then string of switches passed to `znew', example: \"-K\"")

(defvar dired-clean-up-buffers-too t
  "*t says offer to kill buffers visiting files and dirs deleted in dired.")

;;;; KEY BINDINGS.

(define-key dired-mode-map "\M-o" 'dired-omit-toggle)
(define-key dired-mode-map "\M-(" 'dired-mark-sexp)
(define-key dired-mode-map "\M-!" 'dired-smart-shell-command)
(define-key dired-mode-map "T" 'dired-do-toggle)
(define-key dired-mode-map "w" 'dired-copy-filename-as-kill)
(define-key dired-mode-map "\M-g" 'dired-goto-file)
(define-key dired-mode-map "\M-G" 'dired-goto-subdir)
(define-key dired-mode-map "F" 'dired-do-find-marked-files)
(define-key dired-mode-map "Y"  'dired-do-relsymlink)
(define-key dired-mode-map "%Y" 'dired-do-relsymlink-regexp)
(define-key dired-mode-map "V" 'dired-do-run-mail)

(if dired-bind-man
    (define-key dired-mode-map "N" 'dired-man))

(if dired-bind-info
    (define-key dired-mode-map "I" 'dired-info))

;;; GLOBAL BINDING.
(if dired-bind-jump
    (progn
      (define-key global-map "\C-x\C-j" 'dired-jump)
      (define-key global-map "\C-x4\C-j" 'dired-jump-other-window)))


;;;; Install into appropriate hooks.

(add-hook 'dired-mode-hook 'dired-extra-startup)
(add-hook 'dired-after-readin-hook 'dired-omit-expunge)

(defun dired-extra-startup ()
  "Automatically put on dired-mode-hook to get extra dired features:
\\<dired-mode-map>

  \\[dired-do-run-mail]\t-- run mail on folder (see `dired-bind-vm')
  \\[dired-info]\t-- run info on file
  \\[dired-man]\t-- run man on file
  \\[dired-do-find-marked-files]\t-- visit all marked files simultaneously
  \\[dired-omit-toggle]\t-- toggle omitting of files
  \\[dired-do-toggle]\t-- toggle marks
  \\[dired-mark-sexp]\t-- mark by lisp expression
  \\[dired-copy-filename-as-kill]\t-- copy the file or subdir names into the kill ring.
  \t   You can feed it to other commands using \\[yank].

For more features, see variables

  dired-bind-vm
  dired-bind-jump
  dired-bind-info
  dired-bind-man
  dired-vm-read-only-folders
  dired-omit-files-p
  dired-omit-files
  dired-omit-extensions
  dired-find-subdir
  dired-enable-local-variables
  dired-local-variables-file
  dired-guess-shell-gnutar
  dired-guess-shell-gzip-quiet
  dired-guess-shell-znew-switches
  dired-guess-shell-alist-user
  dired-clean-up-buffers-too

See also functions

  dired-flag-extension
  dired-virtual
  dired-jump
  dired-man
  dired-vm
  dired-rmail
  dired-info
  dired-do-find-marked-files
"
  (interactive)

  ;; These must be done in each new dired buffer.
  (dired-hack-local-variables)
  (dired-omit-startup))


;;;; BUFFER CLEANING.

;;; REDEFINE.
(defun dired-clean-up-after-deletion (fn)

  ;; Clean up after a deleted file or directory FN.
  ;; Remove expanded subdir of deleted dir, if any.
  (save-excursion (and (cdr dired-subdir-alist)
                       (dired-goto-subdir fn)
                       (dired-kill-subdir)))

  ;; Offer to kill buffer of deleted file FN.
  (if dired-clean-up-buffers-too
      (progn
        (let ((buf (get-file-buffer fn)))
          (and buf
               (funcall (function y-or-n-p)
                        (format "Kill buffer of %s, too? "
                                (file-name-nondirectory fn)))
               (save-excursion ; you never know where kill-buffer leaves you
                 (kill-buffer buf))))
        (let ((buf-list (dired-buffers-for-dir (expand-file-name fn)))
              (buf nil))
          (and buf-list
               (y-or-n-p (format "Kill dired buffer%s of %s, too? "
                                 (dired-plural-s (length buf-list))
                                 (file-name-nondirectory fn)))
               (while buf-list
                 (save-excursion (kill-buffer (car buf-list)))
                 (setq buf-list (cdr buf-list)))))))
  ;; Anything else?
  )


;;;; EXTENSION MARKING FUNCTIONS.

;;; Mark files with some extension.
(defun dired-mark-extension (extension &optional marker-char)
  "Mark all files with a certain extension for use in later commands.
A `.' is not automatically prepended to the string entered."
  ;; EXTENSION may also be a list of extensions instead of a single one.
  ;; Optional MARKER-CHAR is marker to use.
  (interactive "sMarking extension: \nP")
  (or (listp extension)
      (setq extension (list extension)))
  (dired-mark-files-regexp
   (concat ".";; don't match names with nothing but an extension
           "\\("
           (mapconcat 'regexp-quote extension "\\|")
           "\\)$")
   marker-char))

(defun dired-flag-extension (extension)
  "In dired, flag all files with a certain extension for deletion.
A `.' is *not* automatically prepended to the string entered."
  (interactive "sFlagging extension: ")
  (dired-mark-extension extension dired-del-marker))

;;; Define some unpopular file extensions.  Used for cleaning and omitting.

(defvar dired-patch-unclean-extensions
  '(".rej" ".orig")
  "List of extensions of dispensable files created by the `patch' program.")

(defvar dired-tex-unclean-extensions
  '(".toc" ".log" ".aux");; these are already in completion-ignored-extensions
  "List of extensions of dispensable files created by TeX.")

(defvar dired-latex-unclean-extensions
  '(".idx" ".lof" ".lot" ".glo")
  "List of extensions of dispensable files created by LaTeX.")

(defvar dired-bibtex-unclean-extensions
  '(".blg" ".bbl")
  "List of extensions of dispensable files created by BibTeX.")

(defvar dired-texinfo-unclean-extensions
  '(".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs"
    ".tp" ".tps" ".vr" ".vrs")
  "List of extensions of dispensable files created by texinfo.")

(defun dired-clean-patch ()
  "Flag dispensable files created by patch for deletion.
See variable `dired-patch-unclean-extensions'."
  (interactive)
  (dired-flag-extension dired-patch-unclean-extensions))

(defun dired-clean-tex ()
  "Flag dispensable files created by [La]TeX etc. for deletion.
See variables `dired-texinfo-unclean-extensions',
`dired-latex-unclean-extensions', `dired-bibtex-unclean-extensions' and
`dired-texinfo-unclean-extensions'."
  (interactive)
  (dired-flag-extension (append dired-texinfo-unclean-extensions
                                dired-latex-unclean-extensions
                                dired-bibtex-unclean-extensions
                                dired-tex-unclean-extensions)))

(defun dired-very-clean-tex ()
  "Flag dispensable files created by [La]TeX *and* \".dvi\" for deletion.
See variables `dired-texinfo-unclean-extensions',
`dired-latex-unclean-extensions', `dired-bibtex-unclean-extensions' and
`dired-texinfo-unclean-extensions'."
  (interactive)
  (dired-flag-extension (append dired-texinfo-unclean-extensions
                                dired-latex-unclean-extensions
                                dired-bibtex-unclean-extensions
                                dired-tex-unclean-extensions
                                (list ".dvi"))))

;;;; JUMP.

;;;###autoload
(defun dired-jump (&optional other-window)
  "Jump to dired buffer corresponding to current buffer.
If in a file, dired the current directory and move to file's line.
If in dired already, pop up a level and goto old directory's line.
In case the proper dired file line cannot be found, refresh the dired
buffer and try again."
  (interactive "P")
  (let* ((file buffer-file-name)
         (dir (if file (file-name-directory file) default-directory)))
    (if (eq major-mode 'dired-mode)
        (progn
          (setq dir (dired-current-directory))
          (dired-up-directory other-window)
          (or (dired-goto-file dir)
              ;; refresh and try again
              (progn
                (dired-insert-subdir (file-name-directory dir))
                (dired-goto-file dir))))
      (if other-window
          (dired-other-window dir)
        (dired dir))
      (if file
          (or (dired-goto-file file)
              ;; Toggle omitting, if necessary, and try again.
              (progn
                (dired-omit-toggle t)
                (dired-goto-file file))
              ;; refresh and try again
              (progn
                (dired-insert-subdir (file-name-directory file))
                (dired-goto-file file)))))))

(defun dired-jump-other-window ()
  "Like \\[dired-jump] (dired-jump) but in other window."
  (interactive)
  (dired-jump t))

;;; REDEFINE.
;;; This replaces the version in dired.el
;;; It simply adds the OTHER-WINDOW option to the one in dired.el.
(defun dired-up-directory (&optional other-window)
  "Run dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Finds in current window or in other window with optional OTHER-WINDOW.
Creates a buffer if necessary."
  (interactive "P")
  (let* ((dir (dired-current-directory))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (if other-window
              (dired-other-window up)
            (dired up))
          (dired-goto-file dir)))))


;;;; TOGGLE.
;;; Toggle marked files with unmarked files.

(defun dired-do-toggle ()
  "Toggle marks.
That is, currently marked files become unmarked and vice versa.
Files marked with other flags (such as `D') are not affected.
`.' and `..' are never toggled.
As always, hidden subdirs are not affected."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (while (not (eobp))
        (or (dired-between-files)
            (looking-at dired-re-dot)
            ;; use subst instead of insdel because it does not move
            ;; the gap and thus should be faster and because
            ;; other characters are left alone automatically
            (apply 'subst-char-in-region
                   (point) (1+ (point))
                   (if (eq ?\040 (following-char)) ; SPC
                       (list ?\040 dired-marker-char)
                     (list dired-marker-char ?\040))))
        (forward-line 1)))))


;;;; COPY NAMES OF MARKED FILES INTO KILL-RING.

(defun dired-copy-filename-as-kill (&optional arg)
  "Copy names of marked (or next ARG) files into the kill ring.
The names are separated by a space.
With a zero prefix arg, use the complete pathname of each marked file.
With \\[universal-argument], use the relative pathname of each marked file.

If on a subdir headerline, use subdirname instead; prefix arg is ignored
in this case.

You can then feed the file name(s) to other commands with \\[yank]."
  (interactive "P")
  (let ((string
         (or (dired-get-subdir)
             (mapconcat (function identity)
                        (if arg
                            (cond ((zerop (prefix-numeric-value arg))
                                   (dired-get-marked-files))
                                  ((integerp arg)
                                   (dired-get-marked-files 'no-dir arg))
                                  (t    ; else a raw arg
                                   (dired-get-marked-files t)))
                          (dired-get-marked-files 'no-dir))
                        " "))))
    (kill-new string)
    (message "%s" string)))


;;;; OMITTING.

;;; Enhanced omitting of lines from directory listings.
;;; Marked files are never omitted.

;; should probably get rid of this and always use 'no-dir.
;; sk 28-Aug-1991 09:37
(defvar dired-omit-localp 'no-dir
  "The LOCALP argument dired-omit-expunge passes to dired-get-filename.
If it is 'no-dir, omitting is much faster, but you can only match
against the basename of the file.  Set it to nil if you need to match the
whole pathname.")

;; \017=^O for Omit - other packages can chose other control characters.
(defvar dired-omit-marker-char ?\017
  "Temporary marker used by by dired-omit.
Should never be used as marker by the user or other packages.")

(defun dired-omit-startup ()
  (or (assq 'dired-omit-files-p minor-mode-alist)
      (setq minor-mode-alist
            (append '((dired-omit-files-p " Omit")) minor-mode-alist))))

(defun dired-omit-toggle (&optional flag)
  "Toggle omitting files matching `dired-omit-files' and `dired-omit-extensions'.
With an arg, and if omitting was off, don't toggle and just mark the
  files but don't actually omit them.
With an arg, and if omitting was on, turn it off but don't refresh the buffer."
  (interactive "P")
  (if flag
      (if dired-omit-files-p
          (setq dired-omit-files-p (not dired-omit-files-p))
        (dired-mark-unmarked-files (dired-omit-regexp) nil nil
                                   dired-omit-localp))
    ;; no FLAG
    (setq dired-omit-files-p (not dired-omit-files-p))
    (if (not dired-omit-files-p)
        (revert-buffer)
      ;; this will mention how many were omitted:
      (dired-omit-expunge))))

(defvar dired-omit-extensions
  (append completion-ignored-extensions
          dired-latex-unclean-extensions
          dired-bibtex-unclean-extensions
          dired-texinfo-unclean-extensions)
  "If non-nil, a list of extensions \(strings\) to omit from Dired listings.  
Defaults to elements of `completion-ignored-extensions',
`dired-latex-unclean-extensions', `dired-bibtex-unclean-extensions', and
`dired-texinfo-unclean-extensions'.  

See interactive function `dired-omit-toggle' \(\\[dired-omit-toggle]\) and
variables `dired-omit-files-p' and `dired-omit-files'.")

(defun dired-omit-expunge (&optional regexp)
  "Erases all unmarked files matching REGEXP.
Does nothing if global variable `dired-omit-files-p' is nil.
If REGEXP is nil or not specified, uses `dired-omit-files', and also omits
  filenames ending in `dired-omit-extensions'.
If REGEXP is the empty string, this function is a no-op.

This functions works by temporarily binding `dired-marker-char' to
`dired-omit-marker-char' and calling `dired-do-kill-lines'."
  (interactive "sOmit files (regexp): ")
  (if dired-omit-files-p
      (let ((omit-re (or regexp (dired-omit-regexp)))
            (old-modified-p (buffer-modified-p))
            count)
        (or (string= omit-re "")
            (let ((dired-marker-char dired-omit-marker-char))
              (message "Omitting...")
              (if (dired-mark-unmarked-files omit-re nil nil dired-omit-localp)
                  (progn
                    (setq count (dired-do-kill-lines nil "Omitted %d line%s."))
                    ;; Force an update of modeline.
                    (set-buffer-modified-p (buffer-modified-p)))
                (message "(Nothing to omit)"))))
        ;; Try to preserve modified state of buffer.  So `%*' doesn't appear
        ;; in mode-line of omitted buffers.
        (set-buffer-modified-p (and old-modified-p 
                                    (save-excursion
                                      (goto-char (point-min))
                                      (re-search-forward dired-re-mark nil t))))
        count)))

(defun dired-omit-regexp ()
  (concat (if dired-omit-files (concat "\\(" dired-omit-files "\\)") "")
          (if (and dired-omit-files dired-omit-extensions) "\\|" "")
          (if dired-omit-extensions
              (concat ".";; a non-extension part should exist
                      "\\("
                      (mapconcat 'regexp-quote dired-omit-extensions "\\|")
                      "\\)$")
            "")))

;; Returns t if any work was done, nil otherwise.
(defun dired-mark-unmarked-files (regexp msg &optional unflag-p localp)
  "Marks unmarked files matching REGEXP, displaying MSG.
REGEXP is matched against the complete pathname.
Does not re-mark files which already have a mark.
With prefix argument, unflag all those files.
Second optional argument LOCALP is as in `dired-get-filename'."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\  dired-marker-char)))
    (dired-mark-if
     (and
      ;; not already marked
      (looking-at " ")
      ;; uninteresting
      (let ((fn (dired-get-filename localp t)))
        (and fn (string-match regexp fn))))
     msg)))

;;; REDEFINE.
(defun dired-omit-new-add-entry (filename &optional marker-char)
  ;; This redefines dired-aux.el's dired-add-entry to avoid calling ls for
  ;; files that are going to be omitted anyway.
  (if dired-omit-files-p
      ;; perhaps return t without calling ls
      (let ((omit-re (dired-omit-regexp)))
        (if (or (string= omit-re "")
                (not
                 (string-match omit-re
                               (cond
                                ((eq 'no-dir dired-omit-localp)
                                 filename)
                                ((eq t dired-omit-localp)
                                 (dired-make-relative filename))
                                (t
                                 (dired-make-absolute
                                  filename
                                  (file-name-directory filename)))))))
            ;; if it didn't match, go ahead and add the entry
            (dired-omit-old-add-entry filename marker-char)
          ;; dired-add-entry returns t for success, perhaps we should
          ;; return file-exists-p
          t))
    ;; omitting is not turned on at all
    (dired-omit-old-add-entry filename marker-char)))

;;; REDEFINE.
;;; Redefine dired-aux.el's version of `dired-add-entry'
;;; Save old defun if not already done:
(or (fboundp 'dired-omit-old-add-entry)
    (fset 'dired-omit-old-add-entry (symbol-function 'dired-add-entry)))
;; Redefine it.
(fset 'dired-add-entry 'dired-omit-new-add-entry)


;;;; VIRTUAL DIRED MODE.

;;; For browsing `ls -lR' listings in a dired-like fashion.

(fset 'virtual-dired 'dired-virtual)
(defun dired-virtual (dirname &optional switches)
  "Put this buffer into Virtual Dired mode.

In Virtual Dired mode, all commands that do not actually consult the
filesystem will work.

This is useful if you want to peruse and move around in an ls -lR
output file, for example one you got from an ftp server.  With
ange-ftp, you can even dired a directory containing an ls-lR file,
visit that file and turn on virtual dired mode.  But don't try to save
this file, as dired-virtual indents the listing and thus changes the
buffer.

If you have save a Dired buffer in a file you can use \\[dired-virtual] to
resume it in a later session.

Type \\<dired-mode-map>\\[revert-buffer] in the
Virtual Dired buffer and answer `y' to convert the virtual to a real
dired buffer again.  You don't have to do this, though: you can relist
single subdirs using \\[dired-do-redisplay].
"

  ;; DIRNAME is the top level directory of the buffer.  It will become
  ;; its `default-directory'.  If nil, the old value of
  ;; default-directory is used.

  ;; Optional SWITCHES are the ls switches to use.

  ;; Shell wildcards will be used if there already is a `wildcard'
  ;; line in the buffer (thus it is a saved Dired buffer), but there
  ;; is no other way to get wildcards.  Insert a `wildcard' line by
  ;; hand if you want them.

  (interactive
   (list (read-string "Virtual Dired directory: " (dired-virtual-guess-dir))))
  (goto-char (point-min))
  (or (looking-at "  ")
      ;; if not already indented, do it now:
      (indent-region (point-min) (point-max) 2))
  (or dirname (setq dirname default-directory))
  (setq dirname (expand-file-name (file-name-as-directory dirname)))
  (setq default-directory dirname)      ; contains no wildcards
  (let ((wildcard (save-excursion
                    (goto-char (point-min))
                    (forward-line 1)
                    (and (looking-at "^  wildcard ")
                         (buffer-substring (match-end 0)
                                           (progn (end-of-line) (point)))))))
  (if wildcard
        (setq dirname (expand-file-name wildcard default-directory))))
  ;; If raw ls listing (not a saved old dired buffer), give it a
  ;; decent subdir headerline:
  (goto-char (point-min))
  (or (looking-at dired-subdir-regexp)
      (dired-insert-headerline default-directory))
  (dired-mode dirname (or switches dired-listing-switches))
  (setq mode-name "Virtual Dired"
        revert-buffer-function 'dired-virtual-revert)
  (set (make-local-variable 'dired-subdir-alist) nil)
  (dired-build-subdir-alist)
  (goto-char (point-min))
  (dired-initial-position dirname))

(defun dired-virtual-guess-dir ()

  ;; Guess and return appropriate working directory of this buffer,
  ;; assumed to be in Dired or ls -lR format.
  ;; The guess is based upon buffer contents.
  ;; If nothing could be guessed, returns nil.

  (let ((regexp "^\\(  \\)?\\([^ \n\r]*\\)\\(:\\)[\n\r]")
        (subexpr 2))
    (goto-char (point-min))
    (cond ((looking-at regexp)
           ;; If a saved dired buffer, look to which dir and
           ;; perhaps wildcard it belongs:
           (let ((dir (buffer-substring (match-beginning subexpr)
                                        (match-end subexpr))))
             (file-name-as-directory dir)))
          ;; Else no match for headerline found.  It's a raw ls listing.
          ;; In raw ls listings the directory does not have a headerline
          ;; try parent of first subdir, if any
          ((re-search-forward regexp nil t)
           (file-name-directory
            (directory-file-name
             (file-name-as-directory
              (buffer-substring (match-beginning subexpr)
                                (match-end subexpr))))))
          (t                            ; if all else fails
           nil))))


(defun dired-virtual-revert (&optional arg noconfirm)
  (if (not
       (y-or-n-p "Cannot revert a Virtual Dired buffer - switch to Real Dired mode? "))
      (error "Cannot revert a Virtual Dired buffer.")
    (setq mode-name "Dired"
          revert-buffer-function 'dired-revert)
    (revert-buffer)))

;; A zero-arg version of dired-virtual.
;; You need my modified version of set-auto-mode for the
;; `buffer-contents-mode-alist'.
;; Or you use infer-mode.el and infer-mode-alist, same syntax.
(defun dired-virtual-mode ()
  "Put current buffer into virtual dired mode (see `dired-virtual').
Useful on `buffer-contents-mode-alist' (which see) with the regexp

    \"^  \\(/[^ /]+\\)/?+:$\"

to put saved dired buffers automatically into virtual dired mode.

Also useful for `auto-mode-alist' (which see) like this:

  \(setq auto-mode-alist (cons '(\"[^/]\\.dired\\'\" . dired-virtual-mode)
                              auto-mode-alist)\)"
  (interactive)
  (dired-virtual (dired-virtual-guess-dir)))


;;;; SMART SHELL.

;;; An Emacs buffer can have but one working directory, stored in the
;;; buffer-local variable `default-directory'.  A Dired buffer may have
;;; several subdirectories inserted, but still has but one working directory:
;;; that of the top level Dired directory in that buffer.  For some commands
;;; it is appropriate that they use the current Dired directory instead of
;;; `default-directory', e.g., `find-file' and `compile'.  This is a general
;;; mechanism is provided for special handling of the working directory in
;;; special major modes.

;; It's easier to add to this alist than redefine function
;; default-directory while keeping the old information.
(defconst default-directory-alist
  '((dired-mode . (if (fboundp 'dired-current-directory)
                      (dired-current-directory)
                    default-directory)))
  "Alist of major modes and their opinion on default-directory, as a
lisp expression to evaluate.  A resulting value of nil is ignored in
favor of default-directory.")

(defun default-directory ()
  "Usage like variable `default-directory', but knows about the special
cases in variable `default-directory-alist' (which see)."
  (or (eval (cdr (assq major-mode default-directory-alist)))
      default-directory))

(defun dired-smart-shell-command (cmd &optional insert)
  "Like function `shell-command', but in the current Tree Dired directory."
  (interactive "sShell command: \nP")
  (let ((default-directory (default-directory)))
    (shell-command cmd insert)))


;;;; LOCAL VARIABLES FOR DIRED BUFFERS.

;;; Brief Description:
;;;
;;; * `dired-extra-startup' is part of the `dired-mode-hook'.
;;;
;;; * `dired-extra-startup' calls `dired-hack-local-variables'
;;;
;;; * `dired-hack-local-variables' checks the value of
;;;   `dired-local-variables-file'
;;;
;;; * Check if `dired-local-variables-file' is a non-nil string and is a
;;;   filename found in the directory of the Dired Buffer being created.
;;;
;;; * If `dired-local-variables-file' satisfies the above, then temporarily
;;;   include it in the Dired Buffer at the bottom.
;;;
;;; * Set `enable-local-variables' temporarily to the user variable
;;;   `dired-enable-local-variables' and run `hack-local-variables' on the
;;;   Dired Buffer.

(defvar dired-local-variables-file ".dired"
  "Filename, as string, containing local dired buffer variables to be hacked.
If this file found in current directory, then it will be inserted into dired
buffer and `hack-local-variables' will be run.  See Emacs Info pages for more
information on local variables.  See also `dired-enable-local-variables'.")

(defun dired-hack-local-variables ()
  "Evaluate local variables in `dired-local-variables-file' for dired buffer."
  (if (and dired-local-variables-file
           (stringp dired-local-variables-file)
           (file-exists-p dired-local-variables-file))
      (let ((opoint (point-max))
            buffer-read-only
            ;; In case user has `enable-local-variables' set to nil we
            ;; override it locally with dired's variable.
            (enable-local-variables dired-enable-local-variables))
        ;; Insert 'em.
        (save-excursion
          (goto-char opoint)
          (insert "\^L\n")
          (insert-file-contents dired-local-variables-file))
        ;; Hack 'em.
        (let ((buffer-file-name dired-local-variables-file))
          (hack-local-variables))
        ;; Make sure that the modeline shows the proper information.
        (dired-sort-set-modeline)
        ;; Delete this stuff: `eobp' is used to find last subdir by dired.el.
        (delete-region opoint (point-max)))))

(defun dired-omit-here-always ()
  "Creates `dired-local-variables-file' for omitting and reverts directory.
Sets dired-omit-file-p to t in a local variables file that is readable by
dired."
  (interactive)
  (if (file-exists-p dired-local-variables-file)
      (message "File `./%s' already exists." dired-local-variables-file)

    ;; Create `dired-local-variables-file'.
    (save-excursion
      (set-buffer (get-buffer-create " *dot-dired*"))
      (erase-buffer)
      (insert "Local Variables:\ndired-omit-files-p: t\nEnd:\n")
      (write-file dired-local-variables-file)
      (kill-buffer (current-buffer)))

    ;; Run extra-hooks and revert directory.
    (dired-extra-startup)
    (dired-revert)))


;;;; GUESS SHELL COMMAND.

;;; Brief Description:
;;;
;;; `dired-do-shell-command' is bound to `!' by dired.el.
;;;
;;; * Redefine `dired-do-shell-command' so it calls
;;;   `dired-guess-shell-command'.
;;;
;;; * `dired-guess-shell-command' calls `dired-guess-default' with list of
;;;    marked files.
;;;
;;; * Parse `dired-guess-shell-alist-user' and
;;;   `dired-guess-shell-alist-default' (in that order) for the first REGEXP
;;;   that matches the first file in the file list.
;;;
;;; * If the REGEXP matches all the entries of the file list then evaluate
;;;   COMMAND, which is either a string or a Lisp expression returning a
;;;   string.  COMMAND may be a list of commands.
;;;
;;; * Return this command to `dired-guess-shell-command' which prompts user
;;;   with it.  The list of commands are temporaily put into the history list.
;;;   If a command is used successfully then it is stored permanently in
;;;   `dired-shell-command-history'.

;;; Guess what shell command to apply to a file.
(defvar dired-shell-command-history nil
  "History list for commands that read dired-shell commands.")

;;; Default list of shell commands.

;;; NOTE: Use `gunzip -c' instead of `zcat' on `.gz' files.  Some do not
;;; install GNU zip's version of zcat.

(defvar dired-guess-shell-alist-default
  (list
   (list "\\.tar$" '(if dired-guess-shell-gnutar
                        (concat dired-guess-shell-gnutar " xvf")
                      "tar xvf"))

   ;; REGEXPS for compressed archives must come before the .Z rule to
   ;; be recognized:
   (list "\\.tar\\.Z$"
         ;; Untar it.
         '(if dired-guess-shell-gnutar
              (concat dired-guess-shell-gnutar " zxvf")
            (concat "zcat * | tar xvf -"))
         ;; Optional conversion to gzip format.
         '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
                  " " dired-guess-shell-znew-switches))

   ;; gzip'ed archives
   (list "\\.tar\\.g?z$"
         '(if dired-guess-shell-gnutar
              (concat dired-guess-shell-gnutar " zxvf")
            (concat "gunzip -qc * | tar xvf -"))
         ;; Optional decompression.
         '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q" "")))

   '("\\.shar.Z$" "zcat * | unshar")
   '("\\.shar.g?z$" "gunzip -qc * | unshar")

   '("\\.ps$" "ghostview" "xv" "lpr")
   (list "\\.ps.g?z$" "gunzip -qc * | ghostview -"
         ;; Optional decompression.
         '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.ps.Z$" "zcat * | ghostview -"
         ;; Optional conversion to gzip format.
         '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
                  " " dired-guess-shell-znew-switches))
   '("\\.patch$" "cat * | patch")
   '("\\.patch.g?z$" "gunzip -qc * | patch")
   (list "\\.patch.Z$" "zcat * | patch"
         ;; Optional conversion to gzip format.
         '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
                  " " dired-guess-shell-znew-switches))

   '("\\.dvi$" "xdvi" "dvips")          ; preview and printing
   '("\\.au$" "play")                   ; play Sun audiofiles
   '("\\.mpg$" "mpeg_play")
   '("\\.uu$" "uudecode")               ; for uudecoded files
   '("\\.hqx$" "mcvert")
   '("\\.sh$" "sh")                     ; execute shell scripts
   '("\\.xbm$" "bitmap")                ; view X11 bitmaps
   '("\\.gp$" "gnuplot")
   '("\\.p[bgpn]m$" "xv")
   '("\\.gif$" "xv")                    ; view gif pictures
   '("\\.tif$" "xv")
   '("\\.jpg$" "xv")
   '("\\.fig$" "xfig")                  ; edit fig pictures
   '("\\.out$" "xgraph")                ; for plotting purposes.
   '("\\.tex$" "latex" "tex")
   '("\\.texi\\(nfo\\)?$" "makeinfo" "texi2dvi")

   ;; Some other popular archivers.
   '("\\.zoo$" "zoo x//")
   '("\\.zip$" "unzip")
   '("\\.lzh$" "lharc x")
   '("\\.arc$" "arc x")
   '("\\.shar$" "unshar")

   ;; Compression.
   (list "\\.g?z$" '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.Z$" "uncompress"
         ;; Optional conversion to gzip format.
         '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
                  " " dired-guess-shell-znew-switches))
   )

  "Default alist used for shell command guessing.
See `dired-guess-shell-alist-user'")

(defvar dired-guess-shell-alist-user nil
  "User-defined alist of rules for suggested commands. These rules take
precedence over the predefined rules in the variable
`dired-guess-shell-alist-default' (to which they are prepended).

Each element of this list looks like

    \(REGEXP COMMAND...\)

where each COMMAND can either be a string or a lisp expression that evaluates
to a string.  If several COMMANDs are given, the first one will be the default
and the rest will be added temporarily to the history and can be retrieved
with \\[previous-history-element] (M-p) .

You can set this variable in your ~/.emacs.  For example, to add rules for
`.foo' and `.bar' files, write

 \(setq dired-guess-shell-alist-user
       (list (list \"\\\\.foo$\" \"FOO-COMMAND\");; fixed rule
              ;; possibly more rules ...
              (list \"\\\\.bar$\";; rule with condition test
                    '(if condition
                          \"BAR-COMMAND-1\"
                        \"BAR-COMMAND-2\")))\)
")

(defun dired-guess-default (files)

  ;; Guess a shell commands for FILES.  Return command or list of commands.
  ;; See `dired-guess-shell-alist-user'.

  (let* ((case-fold-search nil) ; case-sensitive matching
         ;; Prepend the user's alist to the default alist.
         (alist (append dired-guess-shell-alist-user
                        dired-guess-shell-alist-default))
         (file (car files))
         (flist (cdr files))
         elt regexp cmds)

    ;; Find the first match in the alist for first file in FILES.
    (while alist
      (setq elt (car alist)
            regexp (car elt)
            alist (cdr alist))
      (if (string-match regexp file)
          (setq cmds (cdr elt)
                alist nil)))

    ;; If more than one file, see if all of FILES match regular expression.
    (while (and flist
                (string-match regexp (car flist)))
      (setq flist (cdr flist)))

    ;; If flist is still non-nil, then do not guess since this means that not
    ;; all the files in FILES were matched by the regexp.
    (setq cmds (and (not flist) cmds))

    ;; Return commands or nil if flist is still non-nil.
    ;; Evaluate the commands in order that any logical testing will be done.
    (cond ((not (cdr cmds))
           (eval (car cmds))) ; single command
          (t
           (mapcar (function eval) cmds)))))

(defun dired-guess-shell-command (prompt files)

  ;; Ask user with PROMPT for a shell command, guessing a default from FILES.

  (let ((default (dired-guess-default files))
        default-list old-history val (failed t))

    (if (null default)
        ;; Nothing to guess
        (read-from-minibuffer prompt nil nil nil 'dired-shell-command-history)

      ;; Save current history list
      (setq old-history dired-shell-command-history)

      (if (listp default)

          ;; More than one guess
          (setq default-list default
                default (car default)
                prompt (concat
                        prompt
                        (format "{%d guesses} " (length default-list))))

        ;; Just one guess
        (setq default-list (list default)))

      ;; Push all guesses onto history so that they can be retrieved with M-p
      ;; and put the first guess in the prompt but not in the initial value.
      (setq dired-shell-command-history
            (append default-list dired-shell-command-history)
            prompt (concat prompt (format "[%s] " default)))

      ;; The unwind-protect returns VAL, and we too.
      (unwind-protect
          ;; BODYFORM
          (progn
            (setq val (read-from-minibuffer prompt nil nil nil
                                            'dired-shell-command-history)
                  failed nil)
            ;; If we got a return, then use default.
            (if (equal val "")
                (setq val default))
            val)

        ;; UNWINDFORMS
        ;; Undo pushing onto the history list so that an aborted
        ;; command doesn't get the default in the next command.
        (setq dired-shell-command-history old-history)
        (if (not failed)
            (or (equal val (car-safe dired-shell-command-history))
                (setq dired-shell-command-history
                      (cons val dired-shell-command-history))))))))


;;; REDEFINE.
;;; Redefine dired-aux.el's version:
(defun dired-read-shell-command (prompt arg files)
;;  "Read a dired shell command prompting with PROMPT (using read-string).
;;ARG is the prefix arg and may be used to indicate in the prompt which
;;  files are affected.
;;This is an extra function so that you can redefine it, e.g., to use gmhist."
  (dired-mark-pop-up
   nil 'shell files
   'dired-guess-shell-command
   (format prompt (dired-mark-prompt arg files)) ; PROMPT
   files))                                       ; FILES


;;;; RELATIVE SYMBOLIC LINKS.

(defvar dired-keep-marker-relsymlink ?S
  "See variable `dired-keep-marker-move'.")

(defun dired-make-relative-symlink (file1 file2 &optional ok-if-already-exists)
  "Three arguments: FILE1 FILE2 &optional OK-IF-ALREADY-EXISTS
Make a symbolic link (pointing to FILE1) in FILE2.
The link is relative (if possible), for example

    \"/vol/tex/bin/foo\" \"/vol/local/bin/foo\"

results in

    \"../../tex/bin/foo\" \"/vol/local/bin/foo\"
"
  (interactive "FRelSymLink: \nFRelSymLink %s: \np")
  (let (name1 name2 len1 len2 (index 0) sub)
    (setq file1 (expand-file-name file1)
          file2 (expand-file-name file2)
          len1 (length file1)
          len2 (length file2))
    ;; Find common initial pathname components:
    (let (next)
      (while (and (setq next (string-match "/" file1 index))
                  (setq next (1+ next))
                  (< next (min len1 len2))
                  ;; For the comparison, both substrings must end in
                  ;; `/', so NEXT is *one plus* the result of the
                  ;; string-match.
                  ;; E.g., consider the case of linking "/tmp/a/abc"
                  ;; to "/tmp/abc" erronously giving "/tmp/a" instead
                  ;; of "/tmp/" as common initial component
                  (string-equal (substring file1 0 next)
                                (substring file2 0 next)))
        (setq index next))
      (setq name2 file2
            sub (substring file1 0 index)
            name1 (substring file1 index)))
    (if (string-equal sub "/")
        ;; No common initial pathname found
        (setq name1 file1)
      ;; Else they have a common parent directory
      (let ((tem (substring file2 index))
            (start 0)
            (count 0))
        ;; Count number of slashes we must compensate for ...
        (while (setq start (string-match "/" tem start))
          (setq count (1+ count)
                start (1+ start)))
        ;; ... and prepend a "../" for each slash found:
        (while (> count 0)
          (setq count (1- count)
                name1 (concat "../" name1)))))
    (make-symbolic-link
     (directory-file-name name1)        ; must not link to foo/
                                        ; (trailing slash!)
     name2 ok-if-already-exists)))

(defun dired-do-relsymlink (&optional arg)
   "Relative symlink all marked (or next ARG) files into a directory,
or make a relative symbolic link to the current file.
This creates relative symbolic links like

    foo -> ../bar/foo

not absolute ones like

    foo -> /ugly/path/that/may/change/any/day/bar/foo"
  (interactive "P")
  (dired-do-create-files 'relsymlink (function dired-make-relative-symlink)
                           "RelSymLink" arg dired-keep-marker-relsymlink))

(defun dired-do-relsymlink-regexp (regexp newname &optional whole-path)
  "RelSymlink all marked files containing REGEXP to NEWNAME.
See functions `dired-do-rename-regexp' and `dired-do-relsymlink'
for more info."
  (interactive (dired-mark-read-regexp "RelSymLink"))
  (dired-do-create-files-regexp
   (function dired-make-relative-symlink)
   "RelSymLink" nil regexp newname whole-path dired-keep-marker-relsymlink))


;;;; VISIT ALL MARKED FILES SIMULTANEOUSLY.

;;; Brief Description:
;;;
;;; `dired-do-find-marked-files' is bound to `F' by dired-x.el.
;;;
;;; * Use `dired-get-marked-files' to collect the marked files in the current
;;;   Dired Buffer into a list of filenames `FILE-LIST'.
;;;
;;; * Pass FILE-LIST to `dired-simultaneous-find-file' all with
;;;   `dired-do-find-marked-files''s prefix argument NOSELECT.
;;;
;;; * `dired-simultaneous-find-file' runs through FILE-LIST decrementing the
;;;   list each time.
;;;
;;; * If NOSELECT is non-nil then just run `find-file-noselect'  on each
;;;   element of FILE-LIST.
;;;
;;; * If NOSELECT is nil then calculate the `size' of the window for each file
;;;   by dividing the `window-height' by length of FILE-LIST.  Thus, `size' is
;;;   cognizant of the window-configuration.
;;;
;;; * If `size' is too small abort, otherwise run `find-file' on each element
;;;   of FILE-LIST giving each a window of height `size'.

(defun dired-do-find-marked-files (&optional noselect)
  "Find all marked files displaying all of them simultaneously.
With optional NOSELECT just find files but do not select them.

The current window is split across all files marked, as evenly as possible.
Remaining lines go to bottom-most window.  The number of files that can be
displayed this way is restricted by the height of the current window and
`window-min-height'.

To keep dired buffer displayed, type \\[split-window-vertically] first.
To display just marked files, type \\[delete-other-windows] first."

  (interactive "P")
  (dired-simultaneous-find-file (dired-get-marked-files) noselect))

(defun dired-simultaneous-find-file (file-list noselect)

  ;; Visit all files in FILE-LIST and display them simultaneously.  The
  ;; current window is split across all files in FILE-LIST, as evenly as
  ;; possible.  Remaining lines go to the bottom-most window.  The number of
  ;; files that can be displayed this way is restricted by the height of the
  ;; current window and the variable `window-min-height'.  With non-nil
  ;; NOSELECT the files are merely found but not selected.

  ;; We don't make this function interactive because it is usually too clumsy
  ;; to specify FILE-LIST interactively unless via dired.

  (let (size)

    (if noselect
        ;; Do not select the buffer.
        (find-file-noselect (car file-list))

      ;; We will have to select the buffer.  Calculate and check window size.
      (setq size (/ (window-height) (length file-list)))
      (or (<= window-min-height size)
          (error "Too many files to visit simultaneously.  Try C-u prefix."))
      (find-file (car file-list)))

    ;; Decrement.
    (setq file-list (cdr file-list))

    (while file-list

      (if noselect
          ;; Do not select the buffer.
          (find-file-noselect (car file-list))

        ;; Vertically split off a window of desired size.  Upper window will
        ;; have SIZE lines.  Select lower (larger) window.  We split it again.
        (select-window (split-window nil size))
        (find-file (car file-list)))

      ;; Decrement.
      (setq file-list (cdr file-list)))))


;;;; MISCELLANEOUS COMMANDS.

;;; Run man on files.

(defun dired-man ()
  "Run man on this file.  Display old buffer if buffer name matches filename.
Uses ../lisp/man.el of \\[manual-entry] fame."
  (interactive)
  (require 'man)
  (let ((file (dired-get-filename))
        (manual-program "nroff -man -h"))
    (Man-getpage-in-background file)))

;;; Run Info on files.

(defun dired-info ()
  "Run info on this file."
  (interactive)
  (info (dired-get-filename)))

;;; Run mail on mail folders.

;;; (and (not (fboundp 'vm-visit-folder))
;;;      (defun vm-visit-folder (file &optional arg)
;;;        nil))

(defun dired-vm (&optional read-only)
  "Run VM on this file.
With prefix arg, visit folder read-only (this requires at least VM 5).
See also variable `dired-vm-read-only-folders'."
  (interactive "P")
  (let ((dir (dired-current-directory))
        (fil (dired-get-filename)))
    ;; take care to supply 2nd arg only if requested - may still run VM 4!
    (cond (read-only (vm-visit-folder fil t))
          ((eq t dired-vm-read-only-folders) (vm-visit-folder fil t))
          ((null dired-vm-read-only-folders) (vm-visit-folder fil))
          (t (vm-visit-folder fil (not (file-writable-p fil)))))
    ;; so that pressing `v' inside VM does prompt within current directory:
    (set (make-local-variable 'vm-folder-directory) dir)))

(defun dired-rmail ()
  "Run RMAIL on this file."
  (interactive)
  (rmail (dired-get-filename)))

(defun dired-do-run-mail ()
  "If `dired-bind-vm' is t, then function `dired-vm', otherwise `dired-rmail'."
  (interactive)
  (if dired-bind-vm
      ;; Read mail folder using vm.
      (dired-vm)
    ;; Read mail folder using rmail.
    (dired-rmail)))


;;;; MISCELLANEOUS INTERNAL FUNCTIONS.

(or (fboundp 'dired-old-find-buffer-nocreate)
    (fset 'dired-old-find-buffer-nocreate
          (symbol-function 'dired-find-buffer-nocreate)))

;;; REDEFINE.
;;; Redefines dired.el's version of `dired-find-buffer-nocreate'
(defun dired-find-buffer-nocreate (dirname)
  (if dired-find-subdir
      (let* ((cur-buf (current-buffer))
             (buffers (nreverse
		       (dired-buffers-for-dir (expand-file-name dirname))))
             (cur-buf-matches (and (memq cur-buf buffers)
                                   ;; wildcards must match, too:
                                   (equal dired-directory dirname))))
        ;; We don't want to switch to the same buffer---
        (setq buffers (delq cur-buf buffers));;need setq with delq
        (or (car (sort buffers (function dired-buffer-more-recently-used-p)))
            ;; ---unless it's the only possibility:
            (and cur-buf-matches cur-buf)))
    (dired-old-find-buffer-nocreate dirname)))

;; This should be a builtin
(defun dired-buffer-more-recently-used-p (buffer1 buffer2)
  "Return t if BUFFER1 is more recently used than BUFFER2."
  (if (equal buffer1 buffer2)
      nil
    (let ((more-recent nil)
          (list (buffer-list)))
      (while (and list
                  (not (setq more-recent (equal buffer1 (car list))))
                  (not (equal buffer2 (car list))))
        (setq list (cdr list)))
      more-recent)))

;;; Same thing as `dired-buffers-for-dir' of dired.el? - lrd 11/23/93
;;; (defun dired-buffers-for-dir-exact (dir)
;;; ;; Return a list of buffers that dired DIR (a directory or wildcard)
;;; ;; at top level, or as subdirectory.
;;; ;; Top level matches must match the wildcard part too, if any.
;;; ;; The list is in reverse order of buffer creation, most recent last.
;;; ;; As a side effect, killed dired buffers for DIR are removed from
;;; ;; dired-buffers.
;;;   (let ((alist dired-buffers) result elt)
;;;     (while alist
;;;       (setq elt (car alist)
;;;             alist (cdr alist))
;;;       (let ((buf (cdr elt)))
;;;         (if (buffer-name buf)
;;;             ;; Top level must match exactly against dired-directory in
;;;             ;; case one of them is a wildcard.
;;;             (if (or (equal dir (save-excursion (set-buffer buf)
;;;                                                dired-directory))
;;;                     (assoc dir (save-excursion (set-buffer buf)
;;;                                                dired-subdir-alist)))
;;;                 (setq result (cons buf result)))
;;;           ;; else buffer is killed - clean up:
;;;           (setq dired-buffers (delq elt dired-buffers)))))
;;;     result))

;;; REDEFINE.
;;; Redefines dired.el's version of `dired-initial-position'
(defun dired-initial-position (dirname)
  (end-of-line)
  (if dired-find-subdir (dired-goto-subdir dirname)) ; new
  (if dired-trivial-filenames (dired-goto-next-nontrivial-file)))


;; Does anyone use this? - lrd 6/29/93.
(defun dired-mark-sexp (predicate &optional unflag-p)
  "Mark files for which PREDICATE returns non-nil.
With a prefix arg, unflag those files instead.

PREDICATE is a lisp expression that can refer to the following symbols:

    inode  [integer] the inode of the file (only for ls -i output)
    s      [integer] the size of the file for ls -s output
                     (ususally in blocks or, with -k, in KByte)
    mode   [string]  file permission bits, e.g. \"-rw-r--r--\"
    nlink  [integer] number of links to file
    uid    [string]  owner
    gid    [string]  group  (If the gid is not displayed by ls,
                     this will still be set (to the same as uid))
    size   [integer] file size in bytes
    time   [string]  the time that ls displays, e.g. \"Feb 12 14:17\"
    name   [string]  the name of the file
    sym    [string]  if file is a symbolic link, the linked-to name, else \"\"

For example, use

        (equal 0 size)

to mark all zero length files."
  ;; Using sym="" instead of nil avoids the trap of
  ;; (string-match "foo" sym) into which a user would soon fall.
  ;; Give `equal' instead of `=' in the example, as this works on
  ;; integers and strings.
  (interactive "xMark if (lisp expr): \nP")
  (message "%s" predicate)
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char))
        inode s mode nlink uid gid size time name sym)
    (dired-mark-if
     (save-excursion
       (and
        ;; Sets vars
        ;;                inode s mode nlink uid gid size time name sym

        ;; according to current file line.  Returns t for success, nil if
        ;; there is no file line.  Upon success, all variables are set, either
        ;; to nil or the appropriate value, so they need not be initialized.
        ;; Moves point within the current line.
        (if (dired-move-to-filename)
            (let (pos
                  (mode-len 10) ; length of mode string
                  ;; like in dired.el, but with subexpressions \1=inode, \2=s:
                  (dired-re-inode-size "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?"))
              (beginning-of-line)
              (forward-char 2)
              (if (looking-at dired-re-inode-size)
                  (progn
                    (goto-char (match-end 0))
                    (setq inode (string-to-int (buffer-substring (match-beginning 1)
                                                                 (match-end 1)))
                          s (string-to-int (buffer-substring (match-beginning 2)
                                                             (match-end 2)))))
                (setq inode nil
                      s nil))
              (setq mode (buffer-substring (point) (+ mode-len (point))))
              (forward-char mode-len)
              (setq nlink (read (current-buffer)))
              (setq uid (buffer-substring (point) (progn (forward-word 1) (point))))
              (re-search-forward "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)")
              (goto-char (match-beginning 1))
              (forward-char -1)
              (setq size (string-to-int (buffer-substring (save-excursion
                                                            (backward-word 1)
                                                            (setq pos (point)))
                                                          (point))))
              (goto-char pos)
              (backward-word 1)
              ;; if no gid is displayed, gid will be set to uid
              ;; but user will then not reference it anyway in PREDICATE.
              (setq gid (buffer-substring (save-excursion (forward-word 1) (point))
                                          (point))
                    time (buffer-substring (match-beginning 1)
                                           (1- (dired-move-to-filename)))
                    name (buffer-substring (point)
                                           (or (dired-move-to-end-of-filename t)
                                               (point)))
                    sym  (progn
                           (if (looking-at " -> ")
                               (buffer-substring (progn (forward-char 4) (point))
                                                 (progn (end-of-line) (point)))
                             "")))
              t)
          nil)
                      (eval predicate)))
     (format "'%s file" predicate))))


;;;; FIND FILE AT POINT.

(defvar dired-x-hands-off-my-keys t
  "*t means don't bind `dired-x-find-file' over `find-file' on keyboard.
Similarly for `dired-x-find-file-other-window' over `find-file-other-window'.
If you change this variable after dired-x.el is loaded then do
\\[dired-x-bind-find-file].")

;;; Bind `dired-x-find-file{-other-window}' over wherever
;;; `find-file{-other-window}' is bound?
(defun dired-x-bind-find-file ()
  "Bind `dired-x-find-file' in place of `find-file' \(or reverse\).
Similarly for `dired-x-find-file-other-window' and `find-file-other-window'.
Binding direction based on `dired-x-hands-off-my-keys'.
This function part of `after-init-hook'."
  (interactive)
  (if (interactive-p)
      (setq dired-x-hands-off-my-keys
            (not (y-or-n-p "Bind dired-x-find-file over find-file? "))))
  (cond ((not dired-x-hands-off-my-keys)
         (substitute-key-definition 'find-file
                                    'dired-x-find-file
                                    (current-global-map))
         (substitute-key-definition 'find-file-other-window
                                    'dired-x-find-file-other-window
                                    (current-global-map)))
        (t
         (substitute-key-definition 'dired-x-find-file
                                    'find-file
                                    (current-global-map))
         (substitute-key-definition 'dired-x-find-file-other-window
                                    'find-file-other-window
                                    (current-global-map))))
  ;; Clear mini-buffer.
  (message nil))

;;; Now call it so binding is correct and put on `after-init-hook' in case
;;; user changes binding.
(dired-x-bind-find-file)
(add-hook 'after-init-hook 'dired-x-bind-find-file)

(defun dired-x-find-file (filename)
  "Edit file FILENAME.
May create a new window, or reuse an existing one.
See the function `display-buffer'.

Identical to `find-file' except when called interactively, with a prefix arg
\(e.g., \\[universal-argument]\), in which case it guesses filename near
point.  Useful for editing file mentioned in buffer you are viewing, or to
test if that file exists.  Use minibuffer after snatching filename."
  (interactive (list (read-filename-at-point "Find file: ")))
  (find-file (expand-file-name filename)))

(defun dired-x-find-file-other-window (filename)
  "Edit file FILENAME, in another window.
May create a new window, or reuse an existing one.
See the function `display-buffer'.

Identical to `find-file-other-window' except when called interactively, with a
prefix arg \(e.g., \\[universal-argument]\), in which case it guesses filename
near point.  Useful for editing file mentioned in buffer you are viewing, or
to test if that file exists.  Use minibuffer after snatching filename."
  (interactive (list (read-filename-at-point "Find file: ")))
  (find-file-other-window (expand-file-name filename)))

;;; Internal functions.
(defun dired-filename-at-point ()

  ;; Get the filename closest to point, but do not change position.  Has a
  ;; preference for looking backward when not directly on a symbol.  Not
  ;; perfect - point must be in middle of or end of filename.

  (let ((filename-chars ".a-zA-Z0-9---_/:$+")
        (bol (save-excursion (beginning-of-line) (point)))
        (eol (save-excursion (end-of-line) (point)))
        start end filename)

    (save-excursion
      ;; First see if just past a filename.
      (if (not (eobp))
          (if (looking-at "[] \t\n[{}()]") ; whitespace or some parens
              (progn
                (skip-chars-backward " \n\t\r({[]})")
                (if (not (bobp))
                    (backward-char 1)))))

      (if (string-match (concat "[" filename-chars "]")
                        (char-to-string (following-char)))
          (progn
            (skip-chars-backward filename-chars)
            (setq start (point))
            (if (string-match "[/~]" (char-to-string (preceding-char)))
                (setq start (1- start)))
            (skip-chars-forward filename-chars))

        (error "No file found around point!"))

      ;; Return string.
      (expand-file-name (buffer-substring start (point))))))

(defun read-filename-at-point (prompt)
  ;;; Returns filename prompting with PROMPT with completion.  If
  ;;; `current-prefix-arg' is non-nil, uses name at point as guess.
  (if current-prefix-arg
      (let ((guess (dired-filename-at-point)))
        (read-file-name prompt
                        (file-name-directory guess)
                        guess
                        nil (file-name-nondirectory guess)))
    (read-file-name prompt default-directory)))


;;;; BUG REPORTS

;;; This section is provided for reports.  It uses Barry A. Warsaw's
;;; reporter.el which is bundled with GNU Emacs v19.

(defconst dired-x-version "2.37"
  "Revision number of dired-x.el -- dired extra for GNU Emacs v19.
Type \\[dired-x-submit-report] to send a bug report.  Available via anonymous
ftp in

   /roebling.poly.edu:/pub/packages/dired-x.tar.gz")

(defconst dired-x-help-address "dodd@roebling.poly.edu"
  "Address(es) accepting submission of reports on dired-x.el.")

(defconst dired-x-maintainer "Larry"
  "First name(s) of people accepting submission of reports on dired-x.el.")

(defconst dired-x-file "dired-x.el"
  "Name of file containing emacs lisp code.")

(defconst dired-x-variable-list
  (list
   'dired-bind-vm
   'dired-vm-read-only-folders
   'dired-bind-jump
   'dired-bind-info
   'dired-bind-man
   'dired-find-subdir
   'dired-enable-local-variables
   'dired-local-variables-file
   'dired-guess-shell-gnutar
   'dired-guess-shell-gzip-quiet
   'dired-guess-shell-znew-switches
   'dired-guess-shell-alist-user
   'dired-clean-up-buffers-too
   'dired-omit-files-p
   'dired-omit-files
   'dired-omit-extensions
   )
  "List of variables to be appended to reports sent by `dired-x-submit-report.'")

(defun dired-x-submit-report ()
  "Submit via reporter.el a bug report on program.  Send report on `dired-x-file'
version `dired-x-version,' to `dired-x-maintainer' at address `dired-x-help-address'
listing variables `dired-x-variable-list' in the message."
  (interactive)

  ;; In case we can't find reporter...
  (condition-case err
      (progn
        ;; Get it if we can.
        (require 'reporter)

        (reporter-submit-bug-report
         dired-x-help-address                           ; address
         (concat dired-x-file " (" dired-x-version ")") ; pkgname
         dired-x-variable-list                          ; varlist
         nil nil                                        ; pre-/post-hooks
         (concat dired-x-maintainer ",")))              ; salutation

    ;; ...fail gracefully.
    (error
     (beep)
     (message "Sorry, reporter.el not found."))))


;; As Barry Warsaw would say: "This might be useful..."
(provide 'dired-x)

;;; dired-x.el ends here
