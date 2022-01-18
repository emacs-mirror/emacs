;;; dired-x.el --- extra Dired functionality  -*- lexical-binding:t -*-

;; Copyright (C) 1993-1994, 1997, 2001-2022 Free Software Foundation,
;; Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;;	Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Maintainer: Romain Francoise <rfrancoise@gnu.org>
;; Keywords: dired extensions files
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

;; This is based on Sebastian Kremer's excellent dired-x.el (Dired Extra),
;; version 1.191, adapted for GNU Emacs.  See the `dired-x' Info manual.

;; At load time dired-x.el will install itself and bind some dired keys.
;; Some dired.el and dired-aux.el functions have extra features if
;; dired-x is loaded.

;; User customization: M-x customize-group RET dired-x RET.

;; *Please* see the `dired-x' Info manual for more details.


;;; Code:

;; This is a no-op if dired-x is being loaded via `dired-load-hook',
;; but maybe not if a dired-x function is being autoloaded.
(require 'dired)


;;; User-defined variables

(defgroup dired-x nil
  "Extended directory editing (dired-x)."
  :group 'dired)

(defgroup dired-keys nil
  "Dired keys customizations."
  :prefix "dired-"
  :group 'dired-x)

(defcustom dired-bind-vm nil
  "Non-nil means \"V\" runs `dired-vm', otherwise \"V\" runs `dired-rmail'.
RMAIL files in the old Babyl format (used before Emacs 23.1)
contain \"-*- rmail -*-\" at the top, so `dired-find-file'
will run `rmail' on these files.  New RMAIL files use the standard
mbox format, and so cannot be distinguished in this way."
  :type 'boolean
  :group 'dired-keys)

(defvar dired-bind-jump t)
(make-obsolete-variable 'dired-bind-jump "not used." "28.1")

(defcustom dired-bind-man t
  "Non-nil means bind `dired-man' to \"N\" in Dired, otherwise do not.
Setting this variable directly after dired-x is loaded has no effect -
use \\[customize]."
  :type 'boolean
  :set (lambda (sym val)
         (if (set sym val)
             (define-key dired-mode-map "N" 'dired-man)
           (if (eq 'dired-man (lookup-key dired-mode-map "N"))
               (define-key dired-mode-map "N" nil))))
  :group 'dired-keys)

(defcustom dired-bind-info t
  "Non-nil means bind `dired-info' to \"I\" in Dired, otherwise do not.
Setting this variable directly after dired-x is loaded has no effect -
use \\[customize]."
  :type 'boolean
  :set (lambda (sym val)
         (if (set sym val)
             (define-key dired-mode-map "I" 'dired-info)
           (if (eq 'dired-info (lookup-key dired-mode-map "I"))
               (define-key dired-mode-map "I" nil))))
  :group 'dired-keys)

(defcustom dired-vm-read-only-folders nil
  "If non-nil, \\[dired-vm] will visit all folders read-only.
If neither nil nor t, e.g. the symbol `if-file-read-only', only
files not writable by you are visited read-only."
  :type '(choice (const :tag "off" nil)
		 (const :tag "on" t)
		 (other :tag "non-writable only" if-file-read-only))
  :group 'dired-x)

(defcustom dired-omit-size-limit 30000
  "Maximum size for the \"omitting\" feature.
If nil, there is no maximum size."
  :type '(choice (const :tag "no maximum" nil) integer)
  :group 'dired-x)

(defcustom dired-omit-case-fold 'filesystem
  "Determine whether \"omitting\" patterns are case-sensitive.
When nil, always be case-sensitive; when t, always be
case-insensitive; the default value, `filesystem', causes case
folding to be used on case-insensitive filesystems only."
  :type '(choice (const :tag "Always case-sensitive" nil)
		 (const :tag "Always case-insensitive" t)
		 (const :tag "According to filesystem" filesystem))
  :group 'dired-x
  :version "26.1")

(declare-function file-name-case-insensitive-p "fileio.c" (filename))
(defun dired-omit-case-fold-p (dir)
  "Non-nil if `dired-omit-mode' should be case-insensitive in DIR."
  (if (eq dired-omit-case-fold 'filesystem)
      (file-name-case-insensitive-p dir)
    dired-omit-case-fold))

;;;###autoload
(define-minor-mode dired-omit-mode
  "Toggle omission of uninteresting files in Dired (Dired-Omit mode).

Dired-Omit mode is a buffer-local minor mode.  When enabled in a
Dired buffer, Dired does not list files whose filenames match
regexp `dired-omit-files', nor files ending with extensions in
`dired-omit-extensions'.

To enable omitting in every Dired buffer, you can put this in
your init file:

  (add-hook \\='dired-mode-hook (lambda () (dired-omit-mode)))

See Info node `(dired-x) Omitting Variables' for more information."
  :group 'dired-x
  (if dired-omit-mode
      ;; This will mention how many lines were omitted:
      (let ((dired-omit-size-limit nil)) (dired-omit-expunge))
    (revert-buffer)))

(put 'dired-omit-mode 'safe-local-variable 'booleanp)

(defcustom dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'"
  "Filenames matching this regexp will not be displayed.
This only has effect when `dired-omit-mode' is t.  See interactive function
`dired-omit-mode' (\\[dired-omit-mode]) and variable
`dired-omit-extensions'.  The default is to omit  `.', `..', auto-save
files and lock files."
  :type 'regexp
  :group 'dired-x)

(defcustom dired-omit-verbose t
  "When non-nil, show messages when omitting files.
When nil, don't show messages."
  :version "24.1"
  :type 'boolean
  :group 'dired-x)

(defcustom dired-find-subdir nil           ; t is pretty near to DWIM...
  "If non-nil, Dired always finds a directory in a buffer of its own.
If nil, Dired finds the directory as a subdirectory in some other buffer
if it is present as one.

If there are several Dired buffers for a directory, the most recently
used is chosen.

Dired avoids switching to the current buffer, so that if you have
a normal and a wildcard buffer for the same directory, \\[dired] will
toggle between those two."
  :type 'boolean
  :group 'dired-x)

(defcustom dired-guess-shell-gnutar
  (catch 'found
    (dolist (exe '("tar" "gtar"))
      (if (with-temp-buffer
            (ignore-errors (call-process exe nil t nil "--version"))
            (and (re-search-backward "GNU tar" nil t) t))
          (throw 'found exe))))
  "If non-nil, name of GNU tar executable.
\(E.g., \"tar\" or \"gtar\").  The `z' switch will be used with it for
compressed or gzip'ed tar files.  If you don't have GNU tar, set this
to nil: a pipe using `zcat' or `gunzip -c' will be used."
  ;; Changed from system-type test to testing --version output.
  ;; Maybe test --help for -z instead?
  :version "24.1"
  :type '(choice (const :tag "Not GNU tar" nil)
		 (string :tag "Command name"))
  :group 'dired-x)

(defcustom dired-guess-shell-gzip-quiet t
  "Non-nil says pass -q to gzip overriding verbose GZIP environment."
  :type 'boolean
  :group 'dired-x)

(defcustom dired-guess-shell-znew-switches nil
  "If non-nil, then string of switches passed to `znew', example: \"-K\"."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Switches"))
  :group 'dired-x)

(defcustom dired-clean-up-buffers-too t
  "Non-nil means offer to kill buffers visiting files and dirs deleted in Dired."
  :type 'boolean
  :group 'dired-x)

(defcustom dired-clean-confirm-killing-deleted-buffers t
  "If nil, don't ask whether to kill buffers visiting deleted files."
  :version "26.1"
  :type 'boolean
  :group 'dired-x)


;;; Key bindings

(when (keymapp (lookup-key dired-mode-map "*"))
  (define-key dired-mode-map "*(" 'dired-mark-sexp)
  (define-key dired-mode-map "*O" 'dired-mark-omitted)
  (define-key dired-mode-map "*." 'dired-mark-extension))

(when (keymapp (lookup-key dired-mode-map "%"))
  (define-key dired-mode-map "%Y" 'dired-do-relsymlink-regexp))

(define-key dired-mode-map "\C-x\M-o" 'dired-omit-mode)
(define-key dired-mode-map "\M-(" 'dired-mark-sexp)
(define-key dired-mode-map "\M-!" 'dired-smart-shell-command)
(define-key dired-mode-map "\M-G" 'dired-goto-subdir)
(define-key dired-mode-map "F" 'dired-do-find-marked-files)
(define-key dired-mode-map "Y"  'dired-do-relsymlink)
(define-key dired-mode-map "V" 'dired-do-run-mail)


;;; Menu bindings

(when-let ((menu (lookup-key dired-mode-map [menu-bar])))
  (easy-menu-add-item menu '("Operate")
                      ["Find Files" dired-do-find-marked-files
                       :help "Find current or marked files"]
                      "Shell Command...")
  (easy-menu-add-item menu '("Operate")
                      ["Relative Symlink to..." dired-do-relsymlink
                       :visible (fboundp 'make-symbolic-link)
                       :help "Make relative symbolic links for current or \
marked files"]
                      "Hardlink to...")
  (easy-menu-add-item menu '("Mark")
                      ["Flag Extension..." dired-flag-extension
                       :help "Flag files with a certain extension for deletion"]
                      "Mark Executables")
  (easy-menu-add-item menu '("Mark")
                      ["Mark Extension..." dired-mark-extension
                       :help "Mark files with a certain extension"]
                      "Unmark All")
  (easy-menu-add-item menu '("Mark")
                      ["Mark Omitted" dired-mark-omitted
                       :help "Mark files matching `dired-omit-files' \
and `dired-omit-extensions'"]
                      "Unmark All")
  (easy-menu-add-item menu '("Regexp")
                      ["Relative Symlink..." dired-do-relsymlink-regexp
                       :visible (fboundp 'make-symbolic-link)
                       :help "Make relative symbolic links for files \
matching regexp"]
                      "Hardlink...")
  (easy-menu-add-item menu '("Immediate")
                      ["Omit Mode" dired-omit-mode
                       :style toggle :selected dired-omit-mode
                       :help "Enable or disable omitting \"uninteresting\" \
files"]
                      "Refresh"))


;;; Install into appropriate hooks

(add-hook 'dired-mode-hook 'dired-extra-startup)
(add-hook 'dired-after-readin-hook 'dired-omit-expunge)

(defun dired-extra-startup ()
  "Automatically put on `dired-mode-hook' to get extra Dired features:
\\<dired-mode-map>
  \\[dired-do-run-mail]\t-- run mail on folder (see `dired-bind-vm')
  \\[dired-info]\t-- run info on file
  \\[dired-man]\t-- run man on file
  \\[dired-do-find-marked-files]\t-- visit all marked files simultaneously
  \\[dired-omit-mode]\t-- toggle omitting of files
  \\[dired-mark-sexp]\t-- mark by Lisp expression

To see the options you can set, use \\[customize-group] RET dired-x RET.
See also the functions:
  `dired-flag-extension'
  `dired-virtual'
  `dired-man'
  `dired-vm'
  `dired-rmail'
  `dired-info'
  `dired-do-find-marked-files'"
  (interactive)
  ;; These must be done in each new dired buffer.
  (dired-omit-startup))


;;; Extension marking functions

(defun dired--mark-suffix-interactive-spec ()
  (let* ((default
           (let ((file (dired-get-filename nil t)))
             (when file
               (file-name-extension file))))
         (suffix
          (read-string (format-prompt
                        "%s extension" default
                        (if (equal current-prefix-arg '(4))
                            "UNmarking"
                          "Marking"))
                       nil nil default))
         (marker
          (pcase current-prefix-arg
            ('(4) ?\s)
            ('(16)
             (let* ((dflt (char-to-string dired-marker-char))
                    (input (read-string
                            (format-prompt "Marker character to use" dflt)
                            nil nil dflt)))
               (aref input 0)))
            (_ dired-marker-char))))
    (list suffix marker)))

;; Mark files with some extension.
(defun dired-mark-extension (extension &optional marker-char)
  "Mark all files with a certain EXTENSION for use in later commands.
A `.' is automatically prepended to EXTENSION when not present.
EXTENSION may also be a list of extensions instead of a single one.
Optional MARKER-CHAR is marker to use.
Interactively, ask for EXTENSION.
Prefixed with one \\[universal-argument], unmark files instead.
Prefixed with two \\[universal-argument]'s, prompt for MARKER-CHAR and mark files with it."
  (interactive (dired--mark-suffix-interactive-spec))
  (unless (listp extension)
    (setq extension (list extension)))
  (dired-mark-files-regexp
   (concat ".";; don't match names with nothing but an extension
           "\\("
           (mapconcat
            (lambda (x)
              (regexp-quote
               (if (string-prefix-p "." x) x (concat "." x))))
            extension "\\|")
           "\\)$")
   marker-char))

;; Mark files ending with some suffix.
(defun dired-mark-suffix (suffix &optional marker-char)
  "Mark all files with a certain SUFFIX for use in later commands.
A `.' is *not* automatically prepended to the string entered;  see
also `dired-mark-extension', which is similar but automatically
prepends `.' when not present.
SUFFIX may also be a list of suffixes instead of a single one.
Optional MARKER-CHAR is marker to use.
Interactively, ask for SUFFIX.
Prefixed with one \\[universal-argument], unmark files instead.
Prefixed with two \\[universal-argument]'s, prompt for MARKER-CHAR and mark files with it."
  (interactive (dired--mark-suffix-interactive-spec))
  (unless (listp suffix)
    (setq suffix (list suffix)))
  (dired-mark-files-regexp
   (concat ".";; don't match names with nothing but an extension
           "\\("
           (mapconcat 'regexp-quote suffix "\\|")
           "\\)$")
   marker-char))

(defun dired-flag-extension (extension)
  "In Dired, flag all files with a certain EXTENSION for deletion.
A `.' is *not* automatically prepended to the string entered."
  (interactive "sFlagging extension: ")
  (dired-mark-extension extension dired-del-marker))

;; Define some unpopular file extensions.  Used for cleaning and omitting.

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
See variables `dired-tex-unclean-extensions',
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


;;; Omitting

;; Enhanced omitting of lines from directory listings.
;; Marked files are never omitted.

;; should probably get rid of this and always use 'no-dir.
;; sk 28-Aug-1991 09:37
(defvar dired-omit-localp 'no-dir
  "The LOCALP argument `dired-omit-expunge' passes to `dired-get-filename'.
If it is `no-dir', omitting is much faster, but you can only match
against the non-directory part of the file name.  Set it to nil if you
need to match the entire file name.")

;; \017=^O for Omit - other packages can choose other control characters.
(defvar dired-omit-marker-char ?\017
  "Temporary marker used by Dired-Omit.
Should never be used as marker by the user or other packages.")

(defun dired-omit-startup ()
  (or (assq 'dired-omit-mode minor-mode-alist)
      (setq minor-mode-alist
            (append '((dired-omit-mode
		       (:eval (if (eq major-mode 'dired-mode)
				  " Omit" ""))))
		    minor-mode-alist))))

(defun dired-mark-omitted ()
  "Mark files matching `dired-omit-files' and `dired-omit-extensions'."
  (interactive)
  (let ((dired-omit-mode nil)) (revert-buffer)) ;; Show omitted files
  (dired-mark-unmarked-files (dired-omit-regexp) nil nil dired-omit-localp
                             (dired-omit-case-fold-p (if (stringp dired-directory)
                                                         dired-directory
                                                       (car dired-directory)))))

(defcustom dired-omit-extensions
  (append completion-ignored-extensions
          dired-latex-unclean-extensions
          dired-bibtex-unclean-extensions
          dired-texinfo-unclean-extensions)
  "If non-nil, a list of extensions (strings) to omit from Dired listings.
Defaults to elements of `completion-ignored-extensions',
`dired-latex-unclean-extensions', `dired-bibtex-unclean-extensions', and
`dired-texinfo-unclean-extensions'.

See interactive function `dired-omit-mode' (\\[dired-omit-mode]) and
variables `dired-omit-mode' and `dired-omit-files'."
  :type '(repeat string)
  :group 'dired-x)

(defun dired-omit-expunge (&optional regexp)
  "Erases all unmarked files matching REGEXP.
Does nothing if global variable `dired-omit-mode' is nil, or if called
  non-interactively and buffer is bigger than `dired-omit-size-limit'.
If REGEXP is nil or not specified, uses `dired-omit-files', and also omits
  filenames ending in `dired-omit-extensions'.
If REGEXP is the empty string, this function is a no-op.

This functions works by temporarily binding `dired-marker-char' to
`dired-omit-marker-char' and calling `dired-do-kill-lines'."
  (interactive "sOmit files (regexp): ")
  (if (and dired-omit-mode
           (or (called-interactively-p 'interactive)
               (not dired-omit-size-limit)
               (< (buffer-size) dired-omit-size-limit)
	       (progn
		 (when dired-omit-verbose
		   (message "Not omitting: directory larger than %d characters."
			    dired-omit-size-limit))
		 (setq dired-omit-mode nil)
		 nil)))
      (let ((omit-re (or regexp (dired-omit-regexp)))
            (old-modified-p (buffer-modified-p))
            count)
        (or (string= omit-re "")
            (let ((dired-marker-char dired-omit-marker-char))
              (when dired-omit-verbose (message "Omitting..."))
              (if (dired-mark-unmarked-files omit-re nil nil dired-omit-localp
                                             (dired-omit-case-fold-p (if (stringp dired-directory)
                                                                         dired-directory
                                                                       (car dired-directory))))
                  (progn
                    (setq count (dired-do-kill-lines
				 nil
				 (if dired-omit-verbose "Omitted %d line%s." "")))
                    (force-mode-line-update))
                (when dired-omit-verbose (message "(Nothing to omit)")))))
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
(defun dired-mark-unmarked-files (regexp msg &optional unflag-p localp case-fold-p)
  "Mark unmarked files matching REGEXP, displaying MSG.
REGEXP is matched against the entire file name.  When called
interactively, prompt for REGEXP.
With prefix argument, unflag all those files.
Optional fourth argument LOCALP is as in `dired-get-filename'.
Optional fifth argument CASE-FOLD-P specifies the value of
`case-fold-search' used for matching REGEXP.
If the region is active in Transient Mark mode, operate only on
files in the active region if `dired-mark-region' is non-nil."
  (interactive
   (list (read-regexp
	  "Mark unmarked files matching regexp (default all): "
          nil 'dired-regexp-history)
	 nil current-prefix-arg nil))
  (let ((dired-marker-char (if unflag-p ?\s dired-marker-char)))
    (dired-mark-if
     (and
      (if unflag-p
          ;; Already marked.
          (not (= (following-char) ?\s))
        ;; Not already marked.
        (= (following-char) ?\s))
      ;; Interesting.
      (let ((fn (dired-get-filename localp t))
            ;; Match patterns case-insensitively on case-insensitive
            ;; systems
            (case-fold-search case-fold-p))
        (and fn (string-match-p regexp fn))))
     msg)))


;;; Virtual dired mode

;; For browsing `ls -lR' listings in a dired-like fashion.

(defalias 'virtual-dired 'dired-virtual)
(defun dired-virtual (dirname &optional switches)
  "Put this Dired buffer into Virtual Dired mode.

In Virtual Dired mode, all commands that do not actually consult the
filesystem will work.

This is useful if you want to peruse and move around in an ls -lR
output file, for example one you got from an ftp server.  With
ange-ftp, you can even Dired a directory containing an ls-lR file,
visit that file and turn on Virtual Dired mode.  But don't try to save
this file, as `dired-virtual' indents the listing and thus changes the
buffer.

If you have saved a Dired buffer in a file you can use \\[dired-virtual] to
resume it in a later session.

Type \\<dired-mode-map>\\[revert-buffer] \
in the Virtual Dired buffer and answer `y' to convert
the virtual to a real Dired buffer again.  You don't have to do this, though:
you can relist single subdirs using \\[dired-do-redisplay]."

  ;; DIRNAME is the top level directory of the buffer.  It will become
  ;; its `default-directory'.  If nil, the old value of
  ;; default-directory is used.

  ;; Optional SWITCHES are the ls switches to use.

  ;; Shell wildcards will be used if there already is a `wildcard'
  ;; line in the buffer (thus it is a saved Dired buffer), but there
  ;; is no other way to get wildcards.  Insert a `wildcard' line by
  ;; hand if you want them.

  (interactive
   (list (read-directory-name "Virtual Dired directory: "
                              nil (dired-virtual-guess-dir))))
  (goto-char (point-min))
  (or (looking-at-p "  ")
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
                                           (line-end-position))))))
    (if wildcard
        (setq dirname (expand-file-name wildcard default-directory))))
  ;; If raw ls listing (not a saved old dired buffer), give it a
  ;; decent subdir headerline:
  (goto-char (point-min))
  (or (looking-at-p dired-subdir-regexp)
      (insert "  "
	      (directory-file-name (file-name-directory default-directory))
	      ":\n"))
  (dired-mode dirname (or switches dired-listing-switches))
  (setq mode-name "Virtual Dired"
        revert-buffer-function 'dired-virtual-revert)
  (setq-local dired-subdir-alist nil)
  (dired-build-subdir-alist)
  (goto-char (point-min))
  (dired-initial-position dirname))

(defun dired-virtual-guess-dir ()
  "Guess and return appropriate working directory of this buffer.
The buffer is assumed to be in Dired or ls -lR format.  The guess is
based upon buffer contents.  If nothing could be guessed, returns
nil."

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


(defun dired-virtual-revert (&optional _arg _noconfirm)
  (if (not
       (y-or-n-p "Cannot revert a Virtual Dired buffer - switch to Real Dired mode? "))
      (error "Cannot revert a Virtual Dired buffer")
    (setq mode-name "Dired"
          revert-buffer-function 'dired-revert)
    (revert-buffer)))

;; A zero-arg version of dired-virtual.
(defun dired-virtual-mode ()
  "Put current buffer into Virtual Dired mode (see `dired-virtual').
Useful on `magic-mode-alist' with the regexp

  \"^  \\\\(/[^ /]+\\\\)+/?:$\"

to put saved Dired buffers automatically into Virtual Dired mode.

Also useful for `auto-mode-alist' like this:

  (add-to-list \\='auto-mode-alist
               \\='(\"[^/]\\\\.dired\\\\\\='\" . dired-virtual-mode))"
  (interactive)
  (dired-virtual (dired-virtual-guess-dir)))


;;; Smart shell

;; An Emacs buffer can have but one working directory, stored in the
;; buffer-local variable `default-directory'.  A Dired buffer may have
;; several subdirectories inserted, but still has but one working directory:
;; that of the top level Dired directory in that buffer.  For some commands
;; it is appropriate that they use the current Dired directory instead of
;; `default-directory', e.g., `find-file' and `compile'.  This is a general
;; mechanism is provided for special handling of the working directory in
;; special major modes.

(defun dired-smart-shell-command (command &optional output-buffer error-buffer)
  "Like function `shell-command', but in the current Virtual Dired directory."
  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
			(cond
			 (buffer-file-name (file-relative-name buffer-file-name))
			 ((eq major-mode 'dired-mode) (dired-get-filename t t))))
    current-prefix-arg
    shell-command-default-error-buffer))
  (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                    (dired-current-directory))
                               default-directory)))
    (shell-command command output-buffer error-buffer)))


;;; Guess shell command

;; Brief Description:
;;
;; * `dired-do-shell-command' is bound to `!' by dired.el.
;;
;; * `dired-guess-shell-command' provides smarter defaults for
;;    dired-aux.el's `dired-read-shell-command'.
;;
;; * `dired-guess-shell-command' calls `dired-guess-default' with list of
;;    marked files.
;;
;; * Parse `dired-guess-shell-alist-user' and
;;   `dired-guess-shell-alist-default' (in that order) for the first REGEXP
;;   that matches the first file in the file list.
;;
;; * If the REGEXP matches all the entries of the file list then evaluate
;;   COMMAND, which is either a string or a Lisp expression returning a
;;   string.  COMMAND may be a list of commands.
;;
;; * Return this command to `dired-guess-shell-command' which prompts user
;;   with it.  The list of commands is put into the list of default values.
;;   If a command is used successfully then it is stored permanently in
;;   `dired-shell-command-history'.

;; Guess what shell command to apply to a file.
(defvar dired-shell-command-history nil
  "History list for commands that read dired-shell commands.")

;; Default list of shell commands.

;; NOTE: Use `gunzip -c' instead of `zcat' on `.gz' files.  Some do not
;; install GNU zip's version of zcat.

(autoload 'Man-support-local-filenames "man")
(autoload 'vc-responsible-backend "vc")

(defvar dired-guess-shell-alist-default
  (list
   (list "\\.tar\\'"
	 '(if dired-guess-shell-gnutar
	      (concat dired-guess-shell-gnutar " xvf")
	    "tar xvf")
	 ;; Extract files into a separate subdirectory
	 '(if dired-guess-shell-gnutar
	      (concat "mkdir " (file-name-sans-extension file)
		      "; " dired-guess-shell-gnutar " -C "
		      (file-name-sans-extension file) " -xvf")
	    (concat "mkdir " (file-name-sans-extension file)
		    "; tar -C " (file-name-sans-extension file) " -xvf"))
	 ;; List archive contents.
	 '(if dired-guess-shell-gnutar
	      (concat dired-guess-shell-gnutar " tvf")
	    "tar tvf"))

   ;; REGEXPS for compressed archives must come before the .Z rule to
   ;; be recognized:
   (list "\\.tar\\.Z\\'"
	 ;; Untar it.
	 '(if dired-guess-shell-gnutar
	      (concat dired-guess-shell-gnutar " zxvf")
	    (concat "zcat * | tar xvf -"))
	 ;; Optional conversion to gzip format.
	 '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
		  " " dired-guess-shell-znew-switches))

   ;; gzip'ed archives
   (list "\\.t\\(ar\\.\\)?gz\\'"
	 '(if dired-guess-shell-gnutar
	      (concat dired-guess-shell-gnutar " zxvf")
	    (concat "gunzip -qc * | tar xvf -"))
	 ;; Extract files into a separate subdirectory
	 '(if dired-guess-shell-gnutar
	      (concat "mkdir " (file-name-sans-extension file)
		      "; " dired-guess-shell-gnutar " -C "
		      (file-name-sans-extension file) " -zxvf")
	    (concat "mkdir " (file-name-sans-extension file)
		    "; gunzip -qc * | tar -C "
		    (file-name-sans-extension file) " -xvf -"))
	 ;; Optional decompression.
	 '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q" ""))
	 ;; List archive contents.
	 '(if dired-guess-shell-gnutar
	      (concat dired-guess-shell-gnutar " ztvf")
	    (concat "gunzip -qc * | tar tvf -")))

   ;; bzip2'ed archives
   (list "\\.t\\(ar\\.bz2\\|bz\\)\\'"
	 "bunzip2 -c * | tar xvf -"
	 ;; Extract files into a separate subdirectory
	 '(concat "mkdir " (file-name-sans-extension file)
		  "; bunzip2 -c * | tar -C "
		  (file-name-sans-extension file) " -xvf -")
	 ;; Optional decompression.
	 "bunzip2")

   ;; xz'ed archives
   (list "\\.t\\(ar\\.\\)?xz\\'"
	 "unxz -c * | tar xvf -"
	 ;; Extract files into a separate subdirectory
	 '(concat "mkdir " (file-name-sans-extension file)
		  "; unxz -c * | tar -C "
		  (file-name-sans-extension file) " -xvf -")
	 ;; Optional decompression.
	 "unxz")

   '("\\.shar\\.Z\\'" "zcat * | unshar")
   '("\\.shar\\.g?z\\'" "gunzip -qc * | unshar")

   '("\\.e?ps\\'" "ghostview" "xloadimage" "lpr")
   (list "\\.e?ps\\.g?z\\'" "gunzip -qc * | ghostview -"
	 ;; Optional decompression.
	 '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.e?ps\\.Z\\'" "zcat * | ghostview -"
	 ;; Optional conversion to gzip format.
	 '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
		  " " dired-guess-shell-znew-switches))

   (list "\\.patch\\'"
         '(if (eq (ignore-errors (vc-responsible-backend default-directory)) 'Git)
              "cat * | git apply"
            "cat * | patch"))
   (list "\\.patch\\.g?z\\'" "gunzip -qc * | patch"
	 ;; Optional decompression.
	 '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.patch\\.Z\\'" "zcat * | patch"
	 ;; Optional conversion to gzip format.
	 '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
		  " " dired-guess-shell-znew-switches))

   ;; The following four extensions are useful with dired-man ("N" key)
   ;; FIXME "man ./" does not work with dired-do-shell-command,
   ;; because there seems to be no way for us to modify the filename,
   ;; only the command.  Hmph.  `dired-man' works though.
   (list "\\.\\(?:[0-9]\\|man\\)\\'"
         '(let ((loc (Man-support-local-filenames)))
            (cond ((eq loc 'man-db) "man -l")
                  ((eq loc 'man) "man ./")
                  (t
                   "cat * | tbl | nroff -man -h | col -b"))))
   (list "\\.\\(?:[0-9]\\|man\\)\\.g?z\\'"
         '(let ((loc (Man-support-local-filenames)))
            (cond ((eq loc 'man-db)
                   "man -l")
                  ((eq loc 'man)
                   "man ./")
                  (t "gunzip -qc * | tbl | nroff -man -h | col -b")))
	 ;; Optional decompression.
	 '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.[0-9]\\.Z\\'"
         '(let ((loc (Man-support-local-filenames)))
            (cond ((eq loc 'man-db) "man -l")
                  ((eq loc 'man) "man ./")
                  (t "zcat * | tbl | nroff -man -h | col -b")))
	 ;; Optional conversion to gzip format.
	 '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
		  " " dired-guess-shell-znew-switches))
   '("\\.pod\\'" "perldoc" "pod2man * | nroff -man")

   '("\\.dvi\\'" "xdvi" "dvips")	; preview and printing
   '("\\.au\\'" "play")			; play Sun audiofiles
   '("\\.mpe?g\\'\\|\\.avi\\'" "xine -p")
   '("\\.ogg\\'" "ogg123")
   '("\\.mp3\\'" "mpg123")
   '("\\.wav\\'" "play")
   '("\\.uu\\'" "uudecode")		; for uudecoded files
   '("\\.hqx\\'" "mcvert")
   '("\\.sh\\'" "sh")			; execute shell scripts
   '("\\.xbm\\'" "bitmap")		; view X11 bitmaps
   '("\\.gp\\'" "gnuplot")
   '("\\.p[bgpn]m\\'" "xloadimage")
   '("\\.gif\\'" "xloadimage")		; view gif pictures
   '("\\.tif\\'" "xloadimage")
   '("\\.png\\'" "display")		; xloadimage 4.1 doesn't grok PNG
   '("\\.jpe?g\\'" "xloadimage")
   '("\\.fig\\'" "xfig")		; edit fig pictures
   '("\\.out\\'" "xgraph")		; for plotting purposes.
   '("\\.tex\\'" "latex" "tex")
   '("\\.texi\\(nfo\\)?\\'" "makeinfo" "texi2dvi")
   '("\\.pdf\\'" "xpdf")
   '("\\.doc\\'" "antiword" "strings")
   '("\\.rpm\\'" "rpm -qilp" "rpm -ivh")
   '("\\.dia\\'" "dia")
   '("\\.mgp\\'" "mgp")

   ;; Some other popular archivers.
   (list "\\.zip\\'" "unzip" "unzip -l"
	 ;; Extract files into a separate subdirectory
	 '(concat "unzip" (if dired-guess-shell-gzip-quiet " -q")
		  " -d " (file-name-sans-extension file)))
   '("\\.zoo\\'" "zoo x//")
   '("\\.lzh\\'" "lharc x")
   '("\\.arc\\'" "arc x")
   '("\\.shar\\'" "unshar")
   '("\\.rar\\'" "unrar x")
   '("\\.7z\\'" "7z x")

   ;; Compression.
   (list "\\.g?z\\'" '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.dz\\'" "dictunzip")
   (list "\\.bz2\\'" "bunzip2")
   (list "\\.xz\\'" "unxz")
   (list "\\.Z\\'" "uncompress"
	 ;; Optional conversion to gzip format.
	 '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
		  " " dired-guess-shell-znew-switches))

   '("\\.sign?\\'" "gpg --verify"))

  "Default alist used for shell command guessing.
See `dired-guess-shell-alist-user'.")

(defcustom dired-guess-shell-alist-user nil
  "User-defined alist of rules for suggested commands.
These rules take precedence over the predefined rules in the variable
`dired-guess-shell-alist-default' (to which they are prepended).

Each element of this list looks like

    (REGEXP COMMAND...)

COMMAND will be used if REGEXP matches the file to be processed.
If several files are to be processed, REGEXP has to match all the
files.

Each COMMAND can either be a string or a Lisp expression that evaluates
to a string.  If this expression needs to consult the name of the file for
which the shell commands are being requested, it can access that file name
as the variable `file'.

If several COMMANDs are given, the first one will be the default
and the rest will be added temporarily to the history and can be retrieved
with `previous-history-element' (\\<minibuffer-mode-map>\\[previous-history-element]).

The variable `dired-guess-shell-case-fold-search' controls whether
REGEXP is matched case-sensitively."
  :group 'dired-x
  :type '(alist :key-type regexp :value-type (repeat sexp)))

(defcustom dired-guess-shell-case-fold-search t
  "If non-nil, `dired-guess-shell-alist-default' and
`dired-guess-shell-alist-user' are matched case-insensitively."
  :group 'dired-x
  :type 'boolean)

(defun dired-guess-default (files)
  "Return a shell command, or a list of commands, appropriate for FILES.
See `dired-guess-shell-alist-user'."
  (let* ((case-fold-search dired-guess-shell-case-fold-search)
         (programs
          (delete-dups
           (mapcar
            (lambda (command)
              (eval command `((file . ,(car files)))))
            (seq-reduce
             #'append
             (mapcar #'cdr
                     (seq-filter (lambda (elem)
                                   (seq-every-p
                                    (lambda (file)
                                      (string-match-p (car elem) file))
                                    files))
                                 (append dired-guess-shell-alist-user
                                         dired-guess-shell-alist-default)))
             nil)))))
    (if (length= programs 1)
        (car programs)
      programs)))

(defun dired-guess-shell-command (prompt files)
  "Ask user with PROMPT for a shell command, guessing a default from FILES."
  (let ((default (dired-guess-default files))
        default-list val)
    (if (null default)
        ;; Nothing to guess
        (read-shell-command prompt nil 'dired-shell-command-history)
      (setq prompt (replace-regexp-in-string ": $" " " prompt))
      (if (listp default)
          ;; More than one guess
          (setq default-list default
                default (car default)
                prompt (concat
                        prompt
                        (format "{%d guesses} " (length default-list))))
        ;; Just one guess
        (setq default-list (list default)))
      ;; Put the first guess in the prompt but not in the initial value.
      (setq prompt (concat prompt (format "[%s]: " default)))
      ;; All guesses can be retrieved with M-n
      (setq val (read-shell-command prompt nil
                                    'dired-shell-command-history
                                    default-list))
      ;; If we got a return, then return default.
      (if (equal val "") default val))))


;;; Relative symbolic links

(declare-function make-symbolic-link "fileio.c")

(defvar dired-keep-marker-relsymlink ?S
  "See variable `dired-keep-marker-move'.")

(defun dired-make-relative-symlink (file1 file2 &optional ok-if-already-exists)
  "Make a symbolic link (pointing to FILE1) in FILE2.
The link is relative (if possible), for example

    \"/vol/tex/bin/foo\" \"/vol/local/bin/foo\"

results in

    \"../../tex/bin/foo\" \"/vol/local/bin/foo\""
  (interactive "FRelSymLink: \nFRelSymLink %s: \np")
  (let (name1 name2 len1 len2 (index 0) sub)
    (setq file1 (expand-file-name file1)
          file2 (expand-file-name file2)
          len1 (length file1)
          len2 (length file2))
    ;; Find common initial file name components:
    (let (next)
      (while (and (setq next (string-search "/" file1 index))
                  (< (setq next (1+ next)) (min len1 len2))
                  ;; For the comparison, both substrings must end in
                  ;; `/', so NEXT is *one plus* the result of the
                  ;; string-search.
                  ;; E.g., consider the case of linking "/tmp/a/abc"
                  ;; to "/tmp/abc" erroneously giving "/tmp/a" instead
                  ;; of "/tmp/" as common initial component
                  (string-equal (substring file1 0 next)
                                (substring file2 0 next)))
        (setq index next))
      (setq name2 file2
            sub (substring file1 0 index)
            name1 (substring file1 index)))
    (if (string-equal sub "/")
        ;; No common initial file name found
        (setq name1 file1)
      ;; Else they have a common parent directory
      (let ((tem (substring file2 index))
            (start 0)
            (count 0))
        ;; Count number of slashes we must compensate for ...
        (while (setq start (string-search "/" tem start))
          (setq count (1+ count)
                start (1+ start)))
        ;; ... and prepend a "../" for each slash found:
        (dotimes (_ count)
          (setq name1 (concat "../" name1)))))
    (make-symbolic-link
     (directory-file-name name1)        ; must not link to foo/
                                        ; (trailing slash!)
     name2 ok-if-already-exists)))

(autoload 'dired-do-create-files "dired-aux")

;;;###autoload
(defun dired-do-relsymlink (&optional arg)
  "Relative symlink all marked (or next ARG) files into a directory.
Otherwise make a relative symbolic link to the current file.
This creates relative symbolic links like

    foo -> ../bar/foo

not absolute ones like

    foo -> /ugly/file/name/that/may/change/any/day/bar/foo

For absolute symlinks, use \\[dired-do-symlink]."
  (interactive "P")
  (dired-do-create-files 'relsymlink #'dired-make-relative-symlink
                         "RelSymLink" arg dired-keep-marker-relsymlink))

(autoload 'dired-mark-read-regexp "dired-aux")
(autoload 'dired-do-create-files-regexp "dired-aux")

(defun dired-do-relsymlink-regexp (regexp newname &optional arg whole-name)
  "RelSymlink all marked files containing REGEXP to NEWNAME.
See functions `dired-do-rename-regexp' and `dired-do-relsymlink'
for more info."
  (interactive (dired-mark-read-regexp "RelSymLink"))
  (dired-do-create-files-regexp
   #'dired-make-relative-symlink
   "RelSymLink" arg regexp newname whole-name dired-keep-marker-relsymlink))


;;; Visit all marked files simultaneously

;; Brief Description:
;;
;; `dired-do-find-marked-files' is bound to `F' by dired-x.el.
;;
;; * Use `dired-get-marked-files' to collect the marked files in the current
;;   Dired Buffer into a list of filenames `FILE-LIST'.
;;
;; * Pass FILE-LIST to `dired-simultaneous-find-file' all with
;;   `dired-do-find-marked-files''s prefix argument NOSELECT.
;;
;; * `dired-simultaneous-find-file' runs through FILE-LIST decrementing the
;;   list each time.
;;
;; * If NOSELECT is non-nil then just run `find-file-noselect' on each
;;   element of FILE-LIST.
;;
;; * If NOSELECT is nil then calculate the `size' of the window for each file
;;   by dividing the `window-height' by length of FILE-LIST.  Thus, `size' is
;;   cognizant of the window-configuration.
;;
;; * If `size' is too small abort, otherwise run `find-file' on each element
;;   of FILE-LIST giving each a window of height `size'.

(defun dired-do-find-marked-files (&optional noselect)
  "Find all marked files displaying all of them simultaneously.
With optional NOSELECT just find files but do not select them.

The current window is split across all files marked, as evenly as possible.
Remaining lines go to bottom-most window.  The number of files that can be
displayed this way is restricted by the height of the current window and
`window-min-height'.

To keep Dired buffer displayed, type \\[split-window-below] first.
To display just marked files, type \\[delete-other-windows] first."
  (interactive "P")
  (dired-simultaneous-find-file (dired-get-marked-files nil nil nil nil t)
                                noselect))

(defun dired-simultaneous-find-file (file-list noselect)
  "Visit all files in FILE-LIST and display them simultaneously.
The current window is split across all files in FILE-LIST, as evenly as
possible.  Remaining lines go to the bottom-most window.  The number of
files that can be displayed this way is restricted by the height of the
current window and the variable `window-min-height'.  With non-nil
NOSELECT the files are merely found but not selected."
  ;; We don't make this function interactive because it is usually too clumsy
  ;; to specify FILE-LIST interactively unless via dired.
  (let (size)
    (if noselect
        ;; Do not select the buffer.
        (find-file-noselect (car file-list))
      ;; We will have to select the buffer.  Calculate and check window size.
      (setq size (/ (window-height) (length file-list)))
      (or (<= window-min-height size)
          (error "Too many files to visit simultaneously.  Try C-u prefix"))
      (find-file (car file-list)))
    ;; Decrement.
    (dolist (file (cdr file-list))
      (if noselect
          ;; Do not select the buffer.
          (find-file-noselect file)
        ;; Vertically split off a window of desired size.  Upper window will
        ;; have SIZE lines.  Select lower (larger) window.  We split it again.
        (select-window (split-window nil size))
        (find-file file)))))


;;; Miscellaneous commands

;; Run man on files.

(declare-function Man-getpage-in-background "man" (topic))

(defvar manual-program) ; from man.el

(defun dired-man ()
  "Run `man' on this file."
  ;; Used also to say: "Display old buffer if buffer name matches filename."
  ;; but I have no idea what that means.
  (interactive)
  (require 'man)
  (let* ((file (dired-get-filename))
         (manual-program (string-replace "*" "%s"
                          (dired-guess-shell-command
                           "Man command: " (list file)))))
    (Man-getpage-in-background file)))

;; Run Info on files.

(defun dired-info ()
  "Run `info' on this file."
  (interactive)
  (info (dired-get-filename)))

;; Run mail on mail folders.

(declare-function vm-visit-folder "ext:vm" (folder &optional read-only))
(defvar vm-folder-directory)

(defun dired-vm (&optional read-only)
  "Run VM on this file.
With optional prefix argument, visits the folder read-only.
Otherwise obeys the value of `dired-vm-read-only-folders'."
  (interactive "P")
  (let ((dir (dired-current-directory))
        (fil (dired-get-filename)))
    (vm-visit-folder fil (or read-only
                             (eq t dired-vm-read-only-folders)
                             (and dired-vm-read-only-folders
                                  (not (file-writable-p fil)))))
    ;; So that pressing `v' inside VM does prompt within current directory:
    (setq-local vm-folder-directory dir)))

(defun dired-rmail ()
  "Run RMAIL on this file."
  (interactive)
  (rmail (dired-get-filename)))

(defun dired-do-run-mail ()
  "Visit the current file as a mailbox, using VM or RMAIL.
Prompt for confirmation first; if the user says yes, call
`dired-vm' if `dired-bind-vm' is non-nil, `dired-rmail'
otherwise."
  (interactive)
  (let ((file (dired-get-filename t)))
    (if dired-bind-vm
	(if (y-or-n-p (format-message
		       "Visit `%s' as a mail folder with VM?" file))
	    (dired-vm))
      ;; Read mail folder using rmail.
      (if (y-or-n-p (format-message
		     "Visit `%s' as a mailbox with RMAIL?" file))
	  (dired-rmail)))))


;;; Miscellaneous internal functions

;; This should be a builtin
(defun dired-buffer-more-recently-used-p (buffer1 buffer2)
  "Return t if BUFFER1 is more recently used than BUFFER2.
Considers buffers closer to the car of `buffer-list' to be more recent."
  (and (not (equal buffer1 buffer2))
       (memq buffer1 (buffer-list))
       (not (memq buffer1 (memq buffer2 (buffer-list))))))

;; Needed if ls -lh is supported and also for GNU ls -ls.
(defun dired-x--string-to-number (str)
  "Like `string-to-number' but recognize a trailing unit prefix.
For example, 2K is expanded to 2048.0.  The caller should make
sure that a trailing letter in STR is one of BKkMGTPEZY."
  (let* ((val (string-to-number str))
         (u (unless (zerop val)
              (aref str (1- (length str))))))
    (when (and u (> u ?9))
      (when (= u ?k)
        (setq u ?K))
      (let ((units '(?B ?K ?M ?G ?T ?P ?E ?Z ?Y)))
        (while (and units (/= (pop units) u))
          (setq val (* 1024.0 val)))))
    val))

(defun dired-mark-sexp (predicate &optional unflag-p)
  "Mark files for which PREDICATE returns non-nil.
With a prefix arg, unmark or unflag those files instead.

PREDICATE is a lisp expression that can refer to the following symbols:

    inode  [integer] the inode of the file (only for ls -i output)
    s      [integer] the size of the file for ls -s output
                     (usually in blocks or, with -k, in KByte)
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

to mark all zero length files.

There's an ambiguity when a single integer not followed by a unit
prefix precedes the file mode: It is then parsed as inode number
and not as block size (this always works for GNU coreutils ls).

Another limitation is that the uid field is needed for the
function to work correctly.  In particular, the field is not
present for some values of `ls-lisp-emulation'.

This function operates only on the buffer content and does not
refer at all to the underlying file system.  Contrast this with
`find-dired', which might be preferable for the task at hand.
If the region is active in Transient Mark mode, mark files
only in the active region if `dired-mark-region' is non-nil."
  ;; Using sym="" instead of nil avoids the trap of
  ;; (string-match "foo" sym) into which a user would soon fall.
  ;; Give `equal' instead of `=' in the example, as this works on
  ;; integers and strings.
  (interactive
   (list (read--expression
          (format "%s if (lisp expr): "
                  (if current-prefix-arg
                      "UNmark"
                    "Mark")))
         current-prefix-arg))
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
        (dired-move-to-filename)
        (let ((mode-len 10) ; length of mode string
	      ;; like in dired.el, but with subexpressions \1=inode, \2=s:
	      ;; GNU ls -hs suffixes the block count with a unit and
	      ;; prints it as a float, FreeBSD does neither.
	      (dired-re-inode-size "\\=\\s *\\([0-9]+\\s +\\)?\
\\(?:\\([0-9]+\\(?:\\.[0-9]*\\)?[BkKMGTPEZY]?\\)? ?\\)"))
	  (beginning-of-line)
	  (forward-char 2)
	  (search-forward-regexp dired-re-inode-size nil t)
          ;; XXX Might be a size not followed by a unit prefix.
          ;; We could set s to inode if it were otherwise nil,
          ;; with a similar reasoning as below for setting gid to uid,
          ;; but it would be even more whimsical.
	  (setq inode (when (match-string 1)
			(string-to-number (match-string 1))))
	  (setq s (when (match-string 2)
		    (dired-x--string-to-number (match-string 2))))
          (setq mode (buffer-substring (point) (+ mode-len (point))))
          (forward-char mode-len)
          ;; Skip any extended attributes marker ("." or "+").
          (or (= (following-char) ?\s)
              (forward-char 1))
          (setq nlink (read (current-buffer)))
          ;; Karsten Wenger <kw@cis.uni-muenchen.de> fixed uid.
          ;; Another issue is that GNU ls -n right-justifies numerical
          ;; UIDs and GIDs, while FreeBSD left-justifies them, so
          ;; don't rely on a specific whitespace layout.  Both of them
          ;; right-justify all other numbers, though.
          ;; XXX Return a number if the uid or gid seems to be
          ;; numerical?
          (setq uid (buffer-substring (progn
                                        (skip-chars-forward " \t")
                                        (point))
                                      (progn
                                        (skip-chars-forward "^ \t")
                                        (point))))
	  (dired-move-to-filename)
          (save-excursion
            (setq time
                  ;; The regexp below tries to match from the last
                  ;; digit of the size field through a space after the
                  ;; date.  Also, dates may have different formats
                  ;; depending on file age, so the date column need
                  ;; not be aligned to the right.
                  (buffer-substring (save-excursion
                                      (skip-chars-backward " \t")
                                      (point))
                                    (progn
                                      (re-search-backward
                                       directory-listing-before-filename-regexp)
                                      (skip-chars-forward "^ \t")
                                      (1+ (point))))
                  size (dired-x--string-to-number
                        ;; We know that there's some kind of number
                        ;; before point because the regexp search
                        ;; above succeeded.  I don't think it's worth
                        ;; doing an extra check for leading garbage.
                        (buffer-substring (point)
                                          (progn
                                            (skip-chars-backward "^ \t")
                                            (point))))
                  ;; If no gid is displayed, gid will be set to uid
                  ;; but the user will then not reference it anyway in
                  ;; PREDICATE.
                  gid (buffer-substring (progn
                                          (skip-chars-backward " \t")
                                          (point))
                                        (progn
                                          (skip-chars-backward "^ \t")
                                          (point)))))
	  (setq name (buffer-substring (point)
				       (or
					(dired-move-to-end-of-filename t)
					(point)))
		sym (if (looking-at " -> ")
			(buffer-substring (progn (forward-char 4) (point))
					  (line-end-position))
		      ""))
          t)
        (eval predicate
              `((inode . ,inode)
                (s . ,s)
                (mode . ,mode)
                (nlink . ,nlink)
                (uid . ,uid)
                (gid . ,gid)
                (size . ,size)
                (time . ,time)
                (name . ,name)
                (sym . ,sym)))))
     (format "'%s file" predicate))))


;;; Find file at point

(defcustom dired-x-hands-off-my-keys t
  "Non-nil means don't remap `find-file' to `dired-x-find-file'.
Similarly for `find-file-other-window' and `dired-x-find-file-other-window'.
If you change this variable without using \\[customize] after `dired-x.el'
is loaded then call \\[dired-x-bind-find-file]."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (set symbol value)
         (dired-x-bind-find-file))
  :group 'dired-x)

(defun dired-x-bind-find-file ()
  "Bind `dired-x-find-file' in place of `find-file' (or vice-versa).
Similarly for `dired-x-find-file-other-window' and `find-file-other-window'.
Binding direction based on `dired-x-hands-off-my-keys'."
  (interactive)
  (if (called-interactively-p 'interactive)
      (setq dired-x-hands-off-my-keys
            (not (y-or-n-p "Bind dired-x-find-file over find-file? "))))
  (unless dired-x-hands-off-my-keys
    (define-key (current-global-map) [remap find-file]
      'dired-x-find-file)
    (define-key (current-global-map) [remap find-file-other-window]
      'dired-x-find-file-other-window)))

;; Now call it so binding is correct.  This could go in the :initialize
;; slot, but then dired-x-bind-find-file has to be defined before the
;; defcustom, and we get free variable warnings.
(dired-x-bind-find-file)

(defun dired-x-find-file (filename)
  "Edit file FILENAME.
Like `find-file', except that when called interactively with a
prefix argument, it offers the filename near point as a default."
  (interactive (list (dired-x-read-filename-at-point "Find file: ")))
  (find-file filename))

(defun dired-x-find-file-other-window (filename)
  "Edit file FILENAME, in another window.
Like `find-file-other-window', except that when called interactively with
a prefix argument, when it offers the filename near point as a default."
  (interactive (list (dired-x-read-filename-at-point "Find file: ")))
  (find-file-other-window filename))


;;; Internal functions

;; Fixme: This should probably use `thing-at-point'.  -- fx
(define-obsolete-function-alias 'dired-filename-at-point
  #'dired-x-guess-file-name-at-point "28.1")
(defun dired-x-guess-file-name-at-point ()
  "Return the filename closest to point, expanded.
Point should be in or after a filename."
  (save-excursion
    ;; First see if just past a filename.
    (or (eobp)                             ; why?
        (when (looking-at-p "[] \t\n[{}()]") ; whitespace or some parens
          (skip-chars-backward " \n\t\r({[]})")
          (or (bobp) (backward-char 1))))
    (let ((filename-chars "-.[:alnum:]_/:$+@")
          start prefix)
      (if (looking-at-p (format "[%s]" filename-chars))
          (progn
            (skip-chars-backward filename-chars)
            (setq start (point)
                  prefix
                  ;; This is something to do with ange-ftp filenames.
                  ;; It convert foo@bar to /foo@bar.
                  ;; But when does the former occur in dired buffers?
		  (and (string-match-p
			"^\\w+@"
			(buffer-substring start (line-end-position)))
		       "/"))
            (if (string-match-p "[/~]" (char-to-string (preceding-char)))
                (setq start (1- start)))
            (skip-chars-forward filename-chars))
        (error "No file found around point!"))
      ;; Return string.
      (expand-file-name (concat prefix (buffer-substring start (point)))))))

(defun dired-x-read-filename-at-point (prompt)
  "Return filename prompting with PROMPT with completion.
If `current-prefix-arg' is non-nil, uses name at point as guess."
  (if current-prefix-arg
      (let ((guess (dired-x-guess-file-name-at-point)))
        (read-file-name prompt
                        (file-name-directory guess)
                        guess
                        nil (file-name-nondirectory guess)))
    (read-file-name prompt default-directory)))

(define-obsolete-function-alias 'read-filename-at-point
  'dired-x-read-filename-at-point "24.1") ; is this even needed?


;;; Epilog

(define-obsolete-function-alias 'dired-x-submit-report 'report-emacs-bug "24.1")


;; As Barry Warsaw would say: "This might be useful..."
(provide 'dired-x)

;; Local Variables:
;; generated-autoload-file: "dired-loaddefs.el"
;; End:

;;; dired-x.el ends here
