;;; dired.el --- directory-browsing commands

;; Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;; Maintainer: FSF

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

;; This is a major mode for directory browsing and editing.  It is
;; documented in the Emacs manual.

;; Rewritten in 1990/1991 to add tree features, file marking and
;; sorting by Sebastian Kremer <sk@thp.uni-koeln.de>.
;; Finished up by rms in 1992.

;;; Code:

;;; Customizable variables

;;;###autoload
(defvar dired-listing-switches "-al"
  "*Switches passed to `ls' for dired.  MUST contain the `l' option.
May contain all other options that don't contradict `-l';
may contain even `F', `b', `i' and `s'.  See also the variable
`dired-ls-F-marks-symlinks' concerning the `F' switch.")

; Don't use absolute paths as /bin should be in any PATH and people
; may prefer /usr/local/gnu/bin or whatever.  However, chown is
; usually not in PATH.

;;;###autoload
(defvar dired-chown-program
  (if (memq system-type '(hpux dgux usg-unix-v irix linux))
      "chown" "/etc/chown")
  "Name of chown command (usually `chown' or `/etc/chown').")

;;;###autoload
(defvar dired-ls-F-marks-symlinks nil
  "*Informs dired about how `ls -lF' marks symbolic links.
Set this to t if `insert-directory-program' with `-lF' marks the symbolic link
itself with a trailing @ (usually the case under Ultrix).

Example: if `ln -s foo bar; ls -F bar' gives `bar -> foo', set it to
nil (the default), if it gives `bar@ -> foo', set it to t.

Dired checks if there is really a @ appended.  Thus, if you have a
marking `ls' program on one host and a non-marking on another host, and
don't care about symbolic links which really end in a @, you can
always set this variable to t.")

;;;###autoload
(defvar dired-trivial-filenames "^\\.\\.?$\\|^#"
  "*Regexp of files to skip when finding first file of a directory.
A value of nil means move to the subdir line.
A value of t means move to first file.")

;;;###autoload
(defvar dired-keep-marker-rename t
  ;; Use t as default so that moved files "take their markers with them".
  "*Controls marking of renamed files.
If t, files keep their previous marks when they are renamed.
If a character, renamed files (whether previously marked or not)
are afterward marked with that character.")

;;;###autoload
(defvar dired-keep-marker-copy ?C
  "*Controls marking of copied files.
If t, copied files are marked if and as the corresponding original files were.
If a character, copied files are unconditionally marked with that character.")

;;;###autoload
(defvar dired-keep-marker-hardlink ?H
  "*Controls marking of newly made hard links.
If t, they are marked if and as the files linked to were marked.
If a character, new links are unconditionally marked with that character.")

;;;###autoload
(defvar dired-keep-marker-symlink ?Y
  "*Controls marking of newly made symbolic links.
If t, they are marked if and as the files linked to were marked.
If a character, new links are unconditionally marked with that character.")

;;;###autoload
(defvar dired-dwim-target nil
  "*If non-nil, dired tries to guess a default target directory.
This means: if there is a dired buffer displayed in the next window,
use its current subdir, instead of the current subdir of this dired buffer.

The target is used in the prompt for file copy, rename etc.")

;;;###autoload
(defvar dired-copy-preserve-time t
  "*If non-nil, Dired preserves the last-modified time in a file copy.
\(This works on only some systems.)")

(defvar dired-font-lock-keywords
  '(;; Put directory headers in italics.
    ("^  \\(/.+\\)$" 1 font-lock-type-face)
    ;; Put symlinks in bold italics.
    ("\\([^ ]+\\) -> [^ ]+$" . font-lock-function-name-face)
    ;; Put marks in bold.
    ("^\\([^ ]\\).*$" 1 font-lock-keyword-face t)
    ;; Put files that are subdirectories in bold.
    ("^..d.* \\([^ ]+\\)$" 1 font-lock-keyword-face))
  "Additional expressions to highlight in Dired mode.")

;;; Hook variables

(defvar dired-load-hook nil
  "Run after loading dired.
You can customize key bindings or load extensions with this.")

(defvar dired-mode-hook nil
  "Run at the very end of dired-mode.")

(defvar dired-before-readin-hook nil
  "This hook is run before a dired buffer is read in (created or reverted).")

(defvar dired-after-readin-hook nil
  "Hook run after each time a file or directory is read by Dired.
After each listing of a file or directory, this hook is run
with the buffer narrowed to the listing.")
;; Note this can't simply be run inside function `dired-ls' as the hook
;; functions probably depend on the dired-subdir-alist to be OK.

;;; Internal variables

(defvar dired-marker-char ?*		; the answer is 42
  ;; so that you can write things like
  ;; (let ((dired-marker-char ?X))
  ;;    ;; great code using X markers ...
  ;;    )
  ;; For example, commands operating on two sets of files, A and B.
  ;; Or marking files with digits 0-9.  This could implicate
  ;; concentric sets or an order for the marked files.
  ;; The code depends on dynamic scoping on the marker char.
  "In Dired, the current mark character.
This is what the `do' commands look for and what the `mark' commands store.")

(defvar dired-del-marker ?D
  "Character used to flag files for deletion.")

(defvar dired-shrink-to-fit
  t
;; I see no reason ever to make this nil -- rms.
;;  (> baud-rate search-slow-speed)
  "Non-nil means Dired shrinks the display buffer to fit the marked files.")

(defvar dired-flagging-regexp nil);; Last regexp used to flag files.

(defvar dired-file-version-alist)

(defvar dired-directory nil
  "The directory name or shell wildcard that was used as argument to `ls'.
Local to each dired buffer.  May be a list, in which case the car is the
directory name and the cdr is the actual files to list.")

(defvar dired-actual-switches nil
  "The value of `dired-listing-switches' used to make this buffer's text.")

(defvar dired-re-inode-size "[0-9 \t]*"
  "Regexp for optional initial inode and file size as made by `ls -i -s'.")

;; These regexps must be tested at beginning-of-line, but are also
;; used to search for next matches, so neither omitting "^" nor
;; replacing "^" by "\n" (to make it slightly faster) will work.

(defvar dired-re-mark "^[^ \n]")
;; "Regexp matching a marked line.
;; Important: the match ends just after the marker."
(defvar dired-re-maybe-mark "^. ")
(defvar dired-re-dir (concat dired-re-maybe-mark dired-re-inode-size "d"))
(defvar dired-re-sym (concat dired-re-maybe-mark dired-re-inode-size "l"))
(defvar dired-re-exe;; match ls permission string of an executable file
  (mapconcat (function
	      (lambda (x)
		(concat dired-re-maybe-mark dired-re-inode-size x)))
	     '("-[-r][-w][xs][-r][-w].[-r][-w]."
	       "-[-r][-w].[-r][-w][xs][-r][-w]."
	       "-[-r][-w].[-r][-w].[-r][-w][xst]")
	     "\\|"))
(defvar dired-re-perms "[-bcdlps][-r][-w].[-r][-w].[-r][-w].")
(defvar dired-re-dot "^.* \\.\\.?$")

;; The subdirectory names in this list are expanded.
(defvar dired-subdir-alist nil
  "Association list of subdirectories and their buffer positions.
Each subdirectory has an element: (DIRNAME . STARTMARKER).
The order of elements is the reverse of the order in the buffer.
In simple cases, this list contains one element.")

(defvar dired-subdir-regexp "^. \\([^\n\r]+\\)\\(:\\)[\n\r]"
  "Regexp matching a maybe hidden subdirectory line in `ls -lR' output.
Subexpression 1 is the subdirectory proper, no trailing colon.
The match starts at the beginning of the line and ends after the end
of the line (\\n or \\r).
Subexpression 2 must end right before the \\n or \\r.")


;;; Macros must be defined before they are used, for the byte compiler.

;; Mark all files for which CONDITION evals to non-nil.
;; CONDITION is evaluated on each line, with point at beginning of line.
;; MSG is a noun phrase for the type of files being marked.
;; It should end with a noun that can be pluralized by adding `s'.
;; Return value is the number of files marked, or nil if none were marked.
(defmacro dired-mark-if (predicate msg)
  (` (let (buffer-read-only count)
       (save-excursion
	 (setq count 0)
	 (if (, msg) (message "Marking %ss..." (, msg)))
	 (goto-char (point-min))
	 (while (not (eobp))
	   (if (, predicate)
	       (progn
		 (delete-char 1)
		 (insert dired-marker-char)
		 (setq count (1+ count))))
	   (forward-line 1))
	 (if (, msg) (message "%s %s%s %s%s."
			  count
			  (, msg)
			  (dired-plural-s count)
			  (if (eq dired-marker-char ?\040) "un" "")
			  (if (eq dired-marker-char dired-del-marker)
			      "flagged" "marked"))))
       (and (> count 0) count))))

(defmacro dired-map-over-marks (body arg &optional show-progress)
;;  "Macro: Perform BODY with point somewhere on each marked line
;;and return a list of BODY's results.
;;If no marked file could be found, execute BODY on the current line.
;;  If ARG is an integer, use the next ARG (or previous -ARG, if ARG<0)
;;  files instead of the marked files.
;;  In that case point is dragged along.  This is so that commands on
;;  the next ARG (instead of the marked) files can be chained easily.
;;  If ARG is otherwise non-nil, use current file instead.
;;If optional third arg SHOW-PROGRESS evaluates to non-nil,
;;   redisplay the dired buffer after each file is processed.
;;No guarantee is made about the position on the marked line.
;;  BODY must ensure this itself if it depends on this.
;;Search starts at the beginning of the buffer, thus the car of the list
;;  corresponds to the line nearest to the buffer's bottom.  This
;;  is also true for (positive and negative) integer values of ARG.
;;BODY should not be too long as it is expanded four times."
;;
;;Warning: BODY must not add new lines before point - this may cause an
;;endless loop.
;;This warning should not apply any longer, sk  2-Sep-1991 14:10.
  (` (prog1
	 (let (buffer-read-only case-fold-search found results)
	   (if (, arg)
	       (if (integerp (, arg))
		   (progn;; no save-excursion, want to move point.
		     (dired-repeat-over-lines
		      (, arg)
		      (function (lambda ()
				  (if (, show-progress) (sit-for 0))
				  (setq results (cons (, body) results)))))
		     (if (< (, arg) 0)
			 (nreverse results)
		       results))
		 ;; non-nil, non-integer ARG means use current file:
		 (list (, body)))
	     (let ((regexp (dired-marker-regexp)) next-position)
	       (save-excursion
		 (goto-char (point-min))
		 ;; remember position of next marked file before BODY
		 ;; can insert lines before the just found file,
		 ;; confusing us by finding the same marked file again
		 ;; and again and...
		 (setq next-position (and (re-search-forward regexp nil t)
					  (point-marker))
		       found (not (null next-position)))
		 (while next-position
		   (goto-char next-position)
		   (if (, show-progress) (sit-for 0))
		   (setq results (cons (, body) results))
		   ;; move after last match
		   (goto-char next-position)
		   (forward-line 1)
		   (set-marker next-position nil)
		   (setq next-position (and (re-search-forward regexp nil t)
					    (point-marker)))))
	       (if found
		   results
		 (list (, body))))))
       ;; save-excursion loses, again
       (dired-move-to-filename))))

(defun dired-get-marked-files (&optional localp arg)
  "Return the marked files' names as list of strings.
The list is in the same order as the buffer, that is, the car is the
  first marked file.
Values returned are normally absolute pathnames.
Optional arg LOCALP as in `dired-get-filename'.
Optional second argument ARG forces to use other files.  If ARG is an
  integer, use the next ARG files.  If ARG is otherwise non-nil, use
  current file.  Usually ARG comes from the current prefix arg."
  (save-excursion
    (nreverse (dired-map-over-marks (dired-get-filename localp) arg))))


;; Function dired-ls is redefinable for VMS, ange-ftp, Prospero or
;; other special applications.

;; The dired command

(defun dired-read-dir-and-switches (str)
  ;; For use in interactive.
  (reverse (list
	    (if current-prefix-arg
		(read-string "Dired listing switches: "
			     dired-listing-switches))
	    (read-file-name (format "Dired %s(directory): " str)
			    nil default-directory nil))))

;;;###autoload (define-key ctl-x-map "d" 'dired)
;;;###autoload
(defun dired (dirname &optional switches)
  "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Optional second argument SWITCHES specifies the `ls' options used.
\(Interactively, use a prefix argument to be able to specify SWITCHES.)
Dired displays a list of files in DIRNAME (which may also have
shell wildcards appended to select certain files).  If DIRNAME is a cons,
its first element is taken as the directory name and the resr as an explicit
list of files to make directory entries for.
\\<dired-mode-map>\
You can move around in it with the usual commands.
You can flag files for deletion with \\[dired-flag-file-deletion] and then
delete them by typing \\[dired-do-flagged-delete].
Type \\[describe-mode] after entering dired for more info.

If DIRNAME is already in a dired buffer, that buffer is used without refresh."
  ;; Cannot use (interactive "D") because of wildcards.
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer (dired-noselect dirname switches)))

;;;###autoload (define-key ctl-x-4-map "d" 'dired-other-window)
;;;###autoload
(defun dired-other-window (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but selects in another window."
  (interactive (dired-read-dir-and-switches "in other window "))
  (switch-to-buffer-other-window (dired-noselect dirname switches)))

;;;###autoload (define-key ctl-x-5-map "d" 'dired-other-frame)
;;;###autoload
(defun dired-other-frame (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (switch-to-buffer-other-frame (dired-noselect dirname switches)))

;;;###autoload
(defun dired-noselect (dir-or-list &optional switches)
  "Like `dired' but returns the dired buffer as value, does not select it."
  (or dir-or-list (setq dir-or-list default-directory))
  ;; This loses the distinction between "/foo/*/" and "/foo/*" that
  ;; some shells make:
  (let (dirname)
    (if (consp dir-or-list)
	(setq dirname (car dir-or-list))
      (setq dirname dir-or-list))
    (setq dirname (abbreviate-file-name
		   (expand-file-name (directory-file-name dirname))))
    (if (file-directory-p dirname)
	(setq dirname (file-name-as-directory dirname)))
    (if (consp dir-or-list)
	(setq dir-or-list (cons dirname (cdr dir-or-list)))
      (setq dir-or-list dirname))
    (dired-internal-noselect dir-or-list switches)))

;; Separate function from dired-noselect for the sake of dired-vms.el.
(defun dired-internal-noselect (dir-or-list &optional switches)
  ;; If there is an existing dired buffer for DIRNAME, just leave
  ;; buffer as it is (don't even call dired-revert).
  ;; This saves time especially for deep trees or with ange-ftp.
  ;; The user can type `g'easily, and it is more consistent with find-file.
  ;; But if SWITCHES are given they are probably different from the
  ;; buffer's old value, so call dired-sort-other, which does
  ;; revert the buffer.
  ;; A pity we can't possibly do "Directory has changed - refresh? "
  ;; like find-file does.
  (let* ((dirname (if (consp dir-or-list) (car dir-or-list) dir-or-list))
	 (buffer (dired-find-buffer-nocreate dir-or-list))
	 ;; note that buffer already is in dired-mode, if found
	 (new-buffer-p (not buffer))
	 (old-buf (current-buffer)))
    (or buffer
	(let ((default-major-mode 'fundamental-mode))
	  ;; We don't want default-major-mode to run hooks and set auto-fill
	  ;; or whatever, now that dired-mode does not
	  ;; kill-all-local-variables any longer.
	  (setq buffer (create-file-buffer (directory-file-name dirname)))))
    (set-buffer buffer)
    (if (not new-buffer-p)		; existing buffer ...
	(if switches			; ... but new switches
	    (dired-sort-other switches)	; this calls dired-revert
	  ;; If directory has changed on disk, offer to revert.
	  (if (let ((attributes (file-attributes dirname))
		    (modtime (visited-file-modtime)))
		(or (eq modtime 0)
		    (not (eq (car attributes) t))
		    (and (= (car (nth 5 attributes)) (car modtime))
			 (= (nth 1 (nth 5 attributes)) (cdr modtime)))))
	      nil
	    (message "Directory has changed on disk; type `g' to update Dired")))
      ;; Else a new buffer
      (setq default-directory
	    (if (file-directory-p dirname)
		dirname
	      (file-name-directory dirname)))
      (or switches (setq switches dired-listing-switches))
      (dired-mode dirname switches)
      ;; default-directory and dired-actual-switches are set now
      ;; (buffer-local), so we can call dired-readin:
      (let ((failed t))
	(unwind-protect
	    (progn (dired-readin dir-or-list buffer)
		   (setq failed nil))
	  ;; dired-readin can fail if parent directories are inaccessible.
	  ;; Don't leave an empty buffer around in that case.
	  (if failed (kill-buffer buffer))))
      ;; No need to narrow since the whole buffer contains just
      ;; dired-readin's output, nothing else.  The hook can
      ;; successfully use dired functions (e.g. dired-get-filename)
      ;; as the subdir-alist has been built in dired-readin.
      (run-hooks 'dired-after-readin-hook)
      (goto-char (point-min))
      (dired-initial-position dirname))
    (set-buffer old-buf)
    buffer))

;; This differs from dired-buffers-for-dir in that it does not consider
;; subdirs of default-directory and searches for the first match only
(defun dired-find-buffer-nocreate (dirname)
  (let (found (blist (buffer-list)))
    (while blist
      (save-excursion
        (set-buffer (car blist))
	(if (and (eq major-mode 'dired-mode)
		 (equal dired-directory dirname))
	    (setq found (car blist)
		  blist nil)
	  (setq blist (cdr blist)))))
    found))


;; Read in a new dired buffer

;; dired-readin differs from dired-insert-subdir in that it accepts
;; wildcards, erases the buffer, and builds the subdir-alist anew
;; (including making it buffer-local and clearing it first).
(defun dired-readin (dir-or-list buffer)
  ;; default-directory and dired-actual-switches must be buffer-local
  ;; and initialized by now.
  ;; Thus we can test (equal default-directory dirname) instead of
  ;; (file-directory-p dirname) and save a filesystem transaction.
  ;; Also, we can run this hook which may want to modify the switches
  ;; based on default-directory, e.g. with ange-ftp to a SysV host
  ;; where ls won't understand -Al switches.
  (let (dirname)
    (if (consp dir-or-list)
	(setq dirname (car dir-or-list))
      (setq dirname dir-or-list))
    (setq dirname (expand-file-name dirname))
    (if (consp dir-or-list)
	(setq dir-or-list (cons dirname (cdr dir-or-list))))
    (run-hooks 'dired-before-readin-hook)
    (save-excursion
      (message "Reading directory %s..." dirname)
      (set-buffer buffer)
      (let (buffer-read-only (failed t))
	(widen)
	(erase-buffer)
	(dired-readin-insert dir-or-list)
	(indent-rigidly (point-min) (point-max) 2)
	;; We need this to make the root dir have a header line as all
	;; other subdirs have:
	(goto-char (point-min))
	(dired-insert-headerline default-directory)
	;; can't run dired-after-readin-hook here, it may depend on the subdir
	;; alist to be OK.
	)
      (message "Reading directory %s...done" dirname)
      ;; Must first make alist buffer local and set it to nil because
      ;; dired-build-subdir-alist will call dired-clear-alist first
      (set (make-local-variable 'dired-subdir-alist) nil)
      (dired-build-subdir-alist)
      (let ((attributes (file-attributes dirname)))
	(if (eq (car attributes) t)
	    (set-visited-file-modtime (nth 5 attributes))))
      (set-buffer-modified-p nil))))

;; Subroutines of dired-readin

(defun dired-readin-insert (dir-or-list)
  ;; Just insert listing for the passed-in directory or
  ;; directory-and-file list, assuming a clean buffer.
  (let (dirname)
    (if (consp dir-or-list)
	(setq dirname (car dir-or-list))
      (setq dirname dir-or-list))
    ;; Expand before comparing in case one or both have been abbreviated.
    (if (and (equal (expand-file-name default-directory)
		    (expand-file-name dirname))
	     (not (consp dir-or-list)))
	;; If we are reading a whole single directory...
	(dired-insert-directory dir-or-list dired-actual-switches nil t)
      (if (not (file-readable-p
		(directory-file-name (file-name-directory dirname))))
	  (error "Directory %s inaccessible or nonexistent" dirname)
	;; Else assume it contains wildcards,
	;; unless it is an explicit list of files.
	(dired-insert-directory dir-or-list dired-actual-switches
				(not (listp dir-or-list)))
	(save-excursion		;; insert wildcard instead of total line:
	  (goto-char (point-min))
	  (insert "wildcard " (file-name-nondirectory dirname) "\n"))))))

(defun dired-insert-directory (dir-or-list switches &optional wildcard full-p)
  ;; Do the right thing whether dir-or-list is atomic or not.  If it is,
  ;; inset all files listed in the cdr (the car is the passed-in directory
  ;; list).
  (let ((opoint (point))
	end)
    (if (consp dir-or-list)
	;; In this case, use the file names in the cdr
	;; exactly as originally given to dired-noselect.
	(mapcar
	 (function (lambda (x) (insert-directory x switches wildcard full-p)))
	 (cdr dir-or-list))
      ;; Expand the file name here because it may have been abbreviated
      ;; in dired-noselect.
      (insert-directory (expand-file-name dir-or-list) switches wildcard full-p))
    ;; Quote certain characters, unless ls quoted them for us.
    (cond ((not (string-match "b" dired-actual-switches))
	   (setq end (point-marker))
	   (goto-char opoint)
	   (while (search-forward "\\" end t)
	     (replace-match "\\\\" nil t))
	   (goto-char opoint)
	   (while (search-forward "\^m" end t)
	     (replace-match "\\015" nil t))
	   (set-marker end nil)))
    (dired-insert-set-properties opoint (point)))
  (setq dired-directory dir-or-list))

;; Make the file names highlight when the mouse is on them.
(defun dired-insert-set-properties (beg end)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (if (dired-move-to-filename)
	  (put-text-property (point)
			     (save-excursion
			       (dired-move-to-end-of-filename)
			       (point))
			     'mouse-face 'highlight))
      (forward-line 1))))

(defun dired-insert-headerline (dir);; also used by dired-insert-subdir
  ;; Insert DIR's headerline with no trailing slash, exactly like ls
  ;; would, and put cursor where dired-build-subdir-alist puts subdir
  ;; boundaries.
  (save-excursion (insert "  " (directory-file-name dir) ":\n")))


;; Reverting a dired buffer

(defun dired-revert (&optional arg noconfirm)
  ;; Reread the dired buffer.  Must also be called after
  ;; dired-actual-switches have changed.
  ;; Should not fail even on completely garbaged buffers.
  ;; Preserves old cursor, marks/flags, hidden-p.
  (widen)				; just in case user narrowed
  (let ((opoint (point))
	(ofile (dired-get-filename nil t))
	(mark-alist nil)		; save marked files
	(hidden-subdirs (dired-remember-hidden))
	(old-subdir-alist (cdr (reverse dired-subdir-alist))) ; except pwd
	(case-fold-search nil)		; we check for upper case ls flags
	buffer-read-only)
    (goto-char (point-min))
    (setq mark-alist;; only after dired-remember-hidden since this unhides:
	  (dired-remember-marks (point-min) (point-max)))
    ;; treat top level dir extra (it may contain wildcards)
    (dired-uncache
     (if (consp dired-directory) (car dired-directory) dired-directory))
    (dired-readin dired-directory (current-buffer))
    (let ((dired-after-readin-hook nil))
      ;; don't run that hook for each subdir...
      (dired-insert-old-subdirs old-subdir-alist))
    (dired-mark-remembered mark-alist)	; mark files that were marked
    ;; ... run the hook for the whole buffer, and only after markers
    ;; have been reinserted (else omitting in dired-x would omit marked files)
    (run-hooks 'dired-after-readin-hook)	; no need to narrow
    (or (and ofile (dired-goto-file ofile)) ; move cursor to where it
	(goto-char opoint))		; was before
    (dired-move-to-filename)
    (save-excursion			; hide subdirs that were hidden
      (mapcar (function (lambda (dir)
			  (if (dired-goto-subdir dir)
			      (dired-hide-subdir 1))))
	      hidden-subdirs)))
  ;; outside of the let scope
;;; Might as well not override the user if the user changed this.
;;;  (setq buffer-read-only t)
  )

;; Subroutines of dired-revert
;; Some of these are also used when inserting subdirs.

(defun dired-remember-marks (beg end)
  ;; Return alist of files and their marks, from BEG to END.
  (if selective-display			; must unhide to make this work.
      (let (buffer-read-only)
	(subst-char-in-region beg end ?\r ?\n)))
  (let (fil chr alist)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward dired-re-mark end t)
	(if (setq fil (dired-get-filename nil t))
	    (setq chr (preceding-char)
		  alist (cons (cons fil chr) alist)))))
    alist))

;; Mark all files remembered in ALIST.
;; Each element of ALIST looks like (FILE . MARKERCHAR).
(defun dired-mark-remembered (alist)
  (let (elt fil chr)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    fil (car elt)
	    chr (cdr elt))
      (if (dired-goto-file fil)
	  (save-excursion
	    (beginning-of-line)
	    (delete-char 1)
	    (insert chr))))))

;; Return a list of names of subdirs currently hidden.
(defun dired-remember-hidden ()
  (let ((l dired-subdir-alist) dir pos result)
    (while l
      (setq dir (car (car l))
	    pos (cdr (car l))
	    l (cdr l))
      (goto-char pos)
      (skip-chars-forward "^\r\n")
      (if (eq (following-char) ?\r)
	  (setq result (cons dir result))))
    result))

;; Try to insert all subdirs that were displayed before,
;; according to the former subdir alist OLD-SUBDIR-ALIST.
(defun dired-insert-old-subdirs (old-subdir-alist)
  (or (string-match "R" dired-actual-switches)
      (let (elt dir)
	(while old-subdir-alist
	  (setq elt (car old-subdir-alist)
		old-subdir-alist (cdr old-subdir-alist)
		dir (car elt))
	  (condition-case ()
	      (progn
		(dired-uncache dir)
		(dired-insert-subdir dir))
	    (error nil))))))

;; Remove directory DIR from any directory cache.
(defun dired-uncache (dir)
  (let ((handler (find-file-name-handler dir 'dired-uncache)))
    (if handler
	(funcall handler 'dired-uncache dir))))

;; dired mode key bindings and initialization

(defvar dired-mode-map nil "Local keymap for dired-mode buffers.")
(if dired-mode-map
    nil
  ;; This looks ugly when substitute-command-keys uses C-d instead d:
  ;;  (define-key dired-mode-map "\C-d" 'dired-flag-file-deletion)

  (setq dired-mode-map (make-keymap))
  (suppress-keymap dired-mode-map)
  (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file-other-window)
  ;; Commands to mark or flag certain categories of files
  (define-key dired-mode-map "#" 'dired-flag-auto-save-files)
  (define-key dired-mode-map "*" 'dired-mark-executables)
  (define-key dired-mode-map "." 'dired-clean-directory)
  (define-key dired-mode-map "/" 'dired-mark-directories)
  (define-key dired-mode-map "@" 'dired-mark-symlinks)
  (define-key dired-mode-map "~" 'dired-flag-backup-files)
  ;; Upper case keys (except !) for operating on the marked files
  (define-key dired-mode-map "A" 'dired-do-tags-search)
  (define-key dired-mode-map "C" 'dired-do-copy)
  (define-key dired-mode-map "B" 'dired-do-byte-compile)
  (define-key dired-mode-map "D" 'dired-do-delete)
  (define-key dired-mode-map "G" 'dired-do-chgrp)
  (define-key dired-mode-map "H" 'dired-do-hardlink)
  (define-key dired-mode-map "L" 'dired-do-load)
  (define-key dired-mode-map "M" 'dired-do-chmod)
  (define-key dired-mode-map "O" 'dired-do-chown)
  (define-key dired-mode-map "P" 'dired-do-print)
  (define-key dired-mode-map "Q" 'dired-do-tags-query-replace)
  (define-key dired-mode-map "R" 'dired-do-rename)
  (define-key dired-mode-map "S" 'dired-do-symlink)
  (define-key dired-mode-map "X" 'dired-do-shell-command)
  (define-key dired-mode-map "Z" 'dired-do-compress)
  (define-key dired-mode-map "!" 'dired-do-shell-command)
  ;; Comparison commands
  (define-key dired-mode-map "=" 'dired-diff)
  (define-key dired-mode-map "\M-=" 'dired-backup-diff)
  ;; Tree Dired commands
  (define-key dired-mode-map "\M-\C-?" 'dired-unmark-all-files)
  (define-key dired-mode-map "\M-\C-d" 'dired-tree-down)
  (define-key dired-mode-map "\M-\C-u" 'dired-tree-up)
  (define-key dired-mode-map "\M-\C-n" 'dired-next-subdir)
  (define-key dired-mode-map "\M-\C-p" 'dired-prev-subdir)
  ;; move to marked files
  (define-key dired-mode-map "\M-{" 'dired-prev-marked-file)
  (define-key dired-mode-map "\M-}" 'dired-next-marked-file)
  ;; Make all regexp commands share a `%' prefix:
  ;; We used to get to the submap via a symbol dired-regexp-prefix,
  ;; but that seems to serve little purpose, and copy-keymap
  ;; does a better job without it.
  (define-key dired-mode-map "%" nil)
  (define-key dired-mode-map "%u" 'dired-upcase)
  (define-key dired-mode-map "%l" 'dired-downcase)
  (define-key dired-mode-map "%d" 'dired-flag-files-regexp)
  (define-key dired-mode-map "%m" 'dired-mark-files-regexp)
  (define-key dired-mode-map "%r" 'dired-do-rename-regexp)
  (define-key dired-mode-map "%C" 'dired-do-copy-regexp)
  (define-key dired-mode-map "%H" 'dired-do-hardlink-regexp)
  (define-key dired-mode-map "%R" 'dired-do-rename-regexp)
  (define-key dired-mode-map "%S" 'dired-do-symlink-regexp)
  ;; Lower keys for commands not operating on all the marked files
  (define-key dired-mode-map "c" 'dired-change-marks)
  (define-key dired-mode-map "d" 'dired-flag-file-deletion)
  (define-key dired-mode-map "e" 'dired-find-file)
  (define-key dired-mode-map "f" 'dired-find-file)
  (define-key dired-mode-map "\C-m" 'dired-advertised-find-file)
  (define-key dired-mode-map "g" 'revert-buffer)
  (define-key dired-mode-map "h" 'describe-mode)
  (define-key dired-mode-map "i" 'dired-maybe-insert-subdir)
  (define-key dired-mode-map "k" 'dired-do-kill-lines)
  (define-key dired-mode-map "l" 'dired-do-redisplay)
  (define-key dired-mode-map "m" 'dired-mark)
  (define-key dired-mode-map "n" 'dired-next-line)
  (define-key dired-mode-map "o" 'dired-find-file-other-window)
  (define-key dired-mode-map "\C-o" 'dired-display-file)
  (define-key dired-mode-map "p" 'dired-previous-line)
  (define-key dired-mode-map "q" 'dired-quit)
  (define-key dired-mode-map "s" 'dired-sort-toggle-or-edit)
  (define-key dired-mode-map "u" 'dired-unmark)
  (define-key dired-mode-map "v" 'dired-view-file)
  (define-key dired-mode-map "x" 'dired-do-flagged-delete)
  (define-key dired-mode-map "+" 'dired-create-directory)
  ;; moving
  (define-key dired-mode-map "<" 'dired-prev-dirline)
  (define-key dired-mode-map ">" 'dired-next-dirline)
  (define-key dired-mode-map "^" 'dired-up-directory)
  (define-key dired-mode-map " "  'dired-next-line)
  (define-key dired-mode-map "\C-n" 'dired-next-line)
  (define-key dired-mode-map "\C-p" 'dired-previous-line)
  (define-key dired-mode-map [down] 'dired-next-line)
  (define-key dired-mode-map [up] 'dired-previous-line)
  ;; hiding
  (define-key dired-mode-map "$" 'dired-hide-subdir)
  (define-key dired-mode-map "\M-$" 'dired-hide-all)
  ;; misc
  (define-key dired-mode-map "?" 'dired-summary)
  (define-key dired-mode-map "\177" 'dired-unmark-backward)
  (define-key dired-mode-map "\C-_" 'dired-undo)
  (define-key dired-mode-map "\C-xu" 'dired-undo)
  )

;; Make menu bar items.

;; Get rid of the Edit menu bar item to save space.
(define-key dired-mode-map [menu-bar edit] 'undefined)

(define-key dired-mode-map [menu-bar subdir]
  (cons "Subdir" (make-sparse-keymap "Subdir")))

(define-key dired-mode-map [menu-bar subdir hide-all]
  '("Hide All" . dired-hide-all))
(define-key dired-mode-map [menu-bar subdir hide-subdir]
  '("Hide Subdir" . dired-hide-subdir))
(define-key dired-mode-map [menu-bar subdir tree-down]
  '("Tree Down" . dired-tree-down))
(define-key dired-mode-map [menu-bar subdir tree-up]
  '("Tree Up" . dired-tree-up))
(define-key dired-mode-map [menu-bar subdir up]
  '("Up Directory" . dired-up-directory))
(define-key dired-mode-map [menu-bar subdir prev-subdir]
  '("Prev Subdir" . dired-prev-subdir))
(define-key dired-mode-map [menu-bar subdir next-subdir]
  '("Next Subdir" . dired-next-subdir))
(define-key dired-mode-map [menu-bar subdir prev-dirline]
  '("Prev Dirline" . dired-prev-dirline))
(define-key dired-mode-map [menu-bar subdir next-dirline]
  '("Next Dirline" . dired-next-dirline))
(define-key dired-mode-map [menu-bar subdir insert]
  '("Insert This Subdir" . dired-maybe-insert-subdir))

(define-key dired-mode-map [menu-bar immediate]
  (cons "Immediate" (make-sparse-keymap "Immediate")))

(define-key dired-mode-map [menu-bar immediate backup-diff]
  '("Compare with Backup" . dired-backup-diff))
(define-key dired-mode-map [menu-bar immediate diff]
  '("Diff" . dired-diff))
(define-key dired-mode-map [menu-bar immediate view]
  '("View This File" . dired-view-file))
(define-key dired-mode-map [menu-bar immediate display]
  '("Display in Other Window" . dired-display-file))
(define-key dired-mode-map [menu-bar immediate find-file-other-window]
  '("Find in Other Window" . dired-find-file-other-window))
(define-key dired-mode-map [menu-bar immediate find-file]
  '("Find This File" . dired-find-file))
(define-key dired-mode-map [menu-bar immediate create-directory]
  '("Create Directory..." . dired-create-directory))

(define-key dired-mode-map [menu-bar regexp]
  (cons "Regexp" (make-sparse-keymap "Regexp")))

(define-key dired-mode-map [menu-bar regexp downcase]
  '("Downcase" . dired-downcase))
(define-key dired-mode-map [menu-bar regexp upcase]
  '("Upcase" . dired-upcase))
(define-key dired-mode-map [menu-bar regexp hardlink]
  '("Hardlink..." . dired-do-hardlink-regexp))
(define-key dired-mode-map [menu-bar regexp symlink]
  '("Symlink..." . dired-do-symlink-regexp))
(define-key dired-mode-map [menu-bar regexp rename]
  '("Rename..." . dired-do-rename-regexp))
(define-key dired-mode-map [menu-bar regexp copy]
  '("Copy..." . dired-do-copy-regexp))
(define-key dired-mode-map [menu-bar regexp flag]
  '("Flag..." . dired-flag-files-regexp))
(define-key dired-mode-map [menu-bar regexp mark]
  '("Mark..." . dired-mark-files-regexp))

(define-key dired-mode-map [menu-bar mark]
  (cons "Mark" (make-sparse-keymap "Mark")))

(define-key dired-mode-map [menu-bar mark prev]
  '("Previous Marked" . dired-prev-marked-file))
(define-key dired-mode-map [menu-bar mark next]
  '("Next Marked" . dired-next-marked-file))
(define-key dired-mode-map [menu-bar mark marks]
  '("Change Marks..." . dired-change-marks))
(define-key dired-mode-map [menu-bar mark unmark-all]
  '("Unmark All" . dired-unmark-all-files-no-query))
(define-key dired-mode-map [menu-bar mark symlinks]
  '("Mark Symlinks" . dired-mark-symlinks))
(define-key dired-mode-map [menu-bar mark directories]
  '("Mark Directories" . dired-mark-directories))
(define-key dired-mode-map [menu-bar mark directory]
  '("Mark Old Backups" . dired-clean-directory))
(define-key dired-mode-map [menu-bar mark executables]
  '("Mark Executables" . dired-mark-executables))
(define-key dired-mode-map [menu-bar mark backup-files]
  '("Flag Backup Files" . dired-flag-backup-files))
(define-key dired-mode-map [menu-bar mark auto-save-files]
  '("Flag Auto-save Files" . dired-flag-auto-save-files))
(define-key dired-mode-map [menu-bar mark deletion]
  '("Flag" . dired-flag-file-deletion))
(define-key dired-mode-map [menu-bar mark unmark]
  '("Unmark" . dired-unmark))
(define-key dired-mode-map [menu-bar mark mark]
  '("Mark" . dired-mark))

(define-key dired-mode-map [menu-bar operate]
  (cons "Operate" (make-sparse-keymap "Operate")))

(define-key dired-mode-map [menu-bar operate tags-query-replace]
  '("Query Replace in Files..." . dired-do-tags-query-replace))
(define-key dired-mode-map [menu-bar operate tags-search]
  '("Search Files..." . dired-do-tags-query-replace))
(define-key dired-mode-map [menu-bar operate chown]
  '("Change Owner..." . dired-do-chown))
(define-key dired-mode-map [menu-bar operate chgrp]
  '("Change Group..." . dired-do-chgrp))
(define-key dired-mode-map [menu-bar operate chmod]
  '("Change Mode..." . dired-do-chmod))
(define-key dired-mode-map [menu-bar operate load]
  '("Load" . dired-do-load))
(define-key dired-mode-map [menu-bar operate compile]
  '("Byte-compile" . dired-do-byte-compile))
(define-key dired-mode-map [menu-bar operate compress]
  '("Compress" . dired-do-compress))
(define-key dired-mode-map [menu-bar operate print]
  '("Print" . dired-do-print))
(define-key dired-mode-map [menu-bar operate hardlink]
  '("Hardlink to..." . dired-do-hardlink))
(define-key dired-mode-map [menu-bar operate symlink]
  '("Symlink to..." . dired-do-symlink))
(define-key dired-mode-map [menu-bar operate command]
  '("Shell Command..." . dired-do-shell-command))
(define-key dired-mode-map [menu-bar operate delete]
  '("Delete" . dired-do-delete))
(define-key dired-mode-map [menu-bar operate rename]
  '("Rename to..." . dired-do-rename))
(define-key dired-mode-map [menu-bar operate copy]
  '("Copy to..." . dired-do-copy))

;; Dired mode is suitable only for specially formatted data.
(put 'dired-mode 'mode-class 'special)

(defun dired-mode (&optional dirname switches)
  "\
Mode for \"editing\" directory listings.
In dired, you are \"editing\" a list of the files in a directory and
  \(optionally) its subdirectories, in the format of `ls -lR'.
  Each directory is a page: use \\[backward-page] and \\[forward-page] to move pagewise.
\"Editing\" means that you can run shell commands on files, visit,
  compress, load or byte-compile them, change their file attributes
  and insert subdirectories into the same buffer.  You can \"mark\"
  files for later commands or \"flag\" them for deletion, either file
  by file or all files matching certain criteria.
You can move using the usual cursor motion commands.\\<dired-mode-map>
Letters no longer insert themselves.  Digits are prefix arguments.
Instead, type \\[dired-flag-file-deletion] to flag a file for Deletion.
Type \\[dired-mark] to Mark a file or subdirectory for later commands.
  Most commands operate on the marked files and use the current file
  if no files are marked.  Use a numeric prefix argument to operate on
  the next ARG (or previous -ARG if ARG<0) files, or just `1'
  to operate on the current file only.  Prefix arguments override marks.
  Mark-using commands display a list of failures afterwards.  Type \\[dired-summary]
  to see why something went wrong.
Type \\[dired-unmark] to Unmark a file or all files of a subdirectory.
Type \\[dired-unmark-backward] to back up one line and unflag.
Type \\[dired-do-flagged-delete] to eXecute the deletions requested.
Type \\[dired-advertised-find-file] to Find the current line's file
  (or dired it in another buffer, if it is a directory).
Type \\[dired-find-file-other-window] to find file or dired directory in Other window.
Type \\[dired-maybe-insert-subdir] to Insert a subdirectory in this buffer.
Type \\[dired-do-rename] to Rename a file or move the marked files to another directory.
Type \\[dired-do-copy] to Copy files.
Type \\[dired-sort-toggle-or-edit] to toggle sorting by name/date or change the `ls' switches.
Type \\[revert-buffer] to read all currently expanded directories again.
  This retains all marks and hides subdirs again that were hidden before.
SPC and DEL can be used to move down and up by lines.

If dired ever gets confused, you can either type \\[revert-buffer] \
to read the
directories again, type \\[dired-do-redisplay] \
to relist a single or the marked files or a
subdirectory, or type \\[dired-build-subdir-alist] to parse the buffer
again for the directory tree.

Customization variables (rename this buffer and type \\[describe-variable] on each line
for more info):

  dired-listing-switches
  dired-trivial-filenames
  dired-shrink-to-fit
  dired-marker-char
  dired-del-marker
  dired-keep-marker-rename
  dired-keep-marker-copy
  dired-keep-marker-hardlink
  dired-keep-marker-symlink

Hooks (use \\[describe-variable] to see their documentation):

  dired-before-readin-hook
  dired-after-readin-hook
  dired-mode-hook
  dired-load-hook

Keybindings:
\\{dired-mode-map}"
  ;; Not to be called interactively (e.g. dired-directory will be set
  ;; to default-directory, which is wrong with wildcards).
  (kill-all-local-variables)
  (use-local-map dired-mode-map)
  (dired-advertise)			; default-directory is already set
  (setq major-mode 'dired-mode
	mode-name "Dired"
;;	case-fold-search nil
	buffer-read-only t
	selective-display t		; for subdirectory hiding
	mode-line-buffer-identification '("Dired: %17b"))
  (set (make-local-variable 'revert-buffer-function)
       (function dired-revert))
  (set (make-local-variable 'page-delimiter)
       "\n\n")
  (set (make-local-variable 'dired-directory)
       (or dirname default-directory))
  ;; list-buffers uses this to display the dir being edited in this buffer.
  (set (make-local-variable 'list-buffers-directory)
       (expand-file-name dired-directory))
  (set (make-local-variable 'dired-actual-switches)
       (or switches dired-listing-switches))
  (set (make-local-variable 'font-lock-defaults) '(dired-font-lock-keywords t))
  (dired-sort-other dired-actual-switches t)
  (run-hooks 'dired-mode-hook))

;; Idiosyncratic dired commands that don't deal with marks.

(defun dired-quit ()
  "Bury the current dired buffer."
  (interactive)
  (bury-buffer))

(defun dired-summary ()
  "Summarize basic Dired commands and show recent Dired errors."
  (interactive)
  (dired-why)
  ;>> this should check the key-bindings and use substitute-command-keys if non-standard
  (message
   "d-elete, u-ndelete, x-punge, f-ind, o-ther window, R-ename, C-opy, h-elp"))

(defun dired-undo ()
  "Undo in a dired buffer.
This doesn't recover lost files, it just undoes changes in the buffer itself.
You can use it to recover marks, killed lines or subdirs.
In the latter case, you have to do \\[dired-build-subdir-alist] to
parse the buffer again."
  (interactive)
  (let (buffer-read-only)
    (undo)))

(defun dired-next-line (arg)
  "Move down lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (next-line arg)
  (dired-move-to-filename))

(defun dired-previous-line (arg)
  "Move up lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (previous-line arg)
  (dired-move-to-filename))

(defun dired-next-dirline (arg &optional opoint)
  "Goto ARG'th next directory file line."
  (interactive "p")
  (or opoint (setq opoint (point)))
  (if (if (> arg 0)
	  (re-search-forward dired-re-dir nil t arg)
	(beginning-of-line)
	(re-search-backward dired-re-dir nil t (- arg)))
      (dired-move-to-filename)		; user may type `i' or `f'
    (goto-char opoint)
    (error "No more subdirectories")))

(defun dired-prev-dirline (arg)
  "Goto ARG'th previous directory file line."
  (interactive "p")
  (dired-next-dirline (- arg)))

(defun dired-up-directory ()
  "Run dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary."
  (interactive)
  (let* ((dir (dired-current-directory))
	 (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
	;; Only try dired-goto-subdir if buffer has more than one dir.
	(and (cdr dired-subdir-alist)
	     (dired-goto-subdir up))
	(progn
	  (dired 
up)
	  (dired-goto-file dir)))))

;; Force `f' rather than `e' in the mode doc:
(defalias 'dired-advertised-find-file 'dired-find-file)
(defun dired-find-file ()
  "In dired, visit the file or directory named on this line."
  (interactive)
  (find-file (file-name-sans-versions (dired-get-filename) t)))

(defun dired-mouse-find-file-other-window (event)
  "In dired, visit the file or directory name you click on."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-file-other-window (file-name-sans-versions file t))))

(defun dired-view-file ()
  "In dired, examine a file in view mode, returning to dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer."
  (interactive)
  (if (file-directory-p (dired-get-filename))
      (or (and (cdr dired-subdir-alist)
	       (dired-goto-subdir (dired-get-filename)))
	  (dired (dired-get-filename)))
    (view-file (dired-get-filename))))

(defun dired-find-file-other-window ()
  "In dired, visit this file or directory in another window."
  (interactive)
  (find-file-other-window (file-name-sans-versions (dired-get-filename) t)))

(defun dired-display-file ()
  "In dired, display this file or directory in another window."
  (interactive)
  (let ((file (file-name-sans-versions (dired-get-filename) t)))
    (display-buffer (find-file-noselect file))))

;;; Functions for extracting and manipulating file names in dired buffers.

(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
Optional arg LOCALP with value `no-dir' means don't include directory
  name in result.  A value of t means construct name relative to
  `default-directory', which still may contain slashes if in a subdirectory.
Optional arg NO-ERROR-IF-NOT-FILEP means return nil if no filename on
  this line, otherwise an error occurs."
  (let (case-fold-search file p1 p2)
    (save-excursion
      (if (setq p1 (dired-move-to-filename (not no-error-if-not-filep)))
	  (setq p2 (dired-move-to-end-of-filename no-error-if-not-filep))))
    ;; nil if no file on this line, but no-error-if-not-filep is t:
    (if (setq file (and p1 p2 (buffer-substring p1 p2)))
	(progn
	  ;; Get rid of the mouse-face property that file names have.
	  (set-text-properties 0 (length file) nil file)
	  ;; Unquote names quoted by ls or by dired-insert-directory.
	  ;; Using read to unquote is much faster than substituting
	  ;; \007 (4 chars) -> ^G  (1 char) etc. in a lisp loop.
	  (setq file
		(read
		 (concat "\""
			 ;; some ls -b don't escape quotes, argh!
			 ;; This is not needed for GNU ls, though.
			 (or (dired-string-replace-match
			      "\\([^\\]\\)\"" file "\\1\\\\\"")
			     file)
			 "\"")))))
    (if (eq localp 'no-dir)
	file
      (and file (concat (dired-current-directory localp) file)))))

;; Cloning replace-match to work on strings instead of in buffer:
;; The FIXEDCASE parameter of replace-match is not implemented.
(defun dired-string-replace-match (regexp string newtext
					  &optional literal global)
  "Replace first match of REGEXP in STRING with NEWTEXT.
If it does not match, nil is returned instead of the new string.
Optional arg LITERAL means to take NEWTEXT literally.
Optional arg GLOBAL means to replace all matches."
  (if global
        (let ((result "") (start 0) mb me)
	  (while (string-match regexp string start)
	    (setq mb (match-beginning 0)
		  me (match-end 0)
		  result (concat result
				 (substring string start mb)
				 (if literal
				     newtext
				   (dired-expand-newtext string newtext)))
		  start me))
	  (if mb			; matched at least once
	      (concat result (substring string start))
	    nil))
    ;; not GLOBAL
    (if (not (string-match regexp string 0))
	nil
      (concat (substring string 0 (match-beginning 0))
	      (if literal newtext (dired-expand-newtext string newtext))
	      (substring string (match-end 0))))))

(defun dired-make-absolute (file &optional dir)
  ;;"Convert FILE (a pathname relative to DIR) to an absolute pathname."
  ;; We can't always use expand-file-name as this would get rid of `.'
  ;; or expand in / instead default-directory if DIR=="".
  ;; This should be good enough for ange-ftp, but might easily be
  ;; redefined (for VMS?).
  ;; It should be reasonably fast, though, as it is called in
  ;; dired-get-filename.
  (concat (or dir default-directory) file))

(defun dired-make-relative (file &optional dir no-error)
  ;;"Convert FILE (an absolute pathname) to a pathname relative to DIR.
  ;; Else error (unless NO-ERROR is non-nil, then FILE is returned unchanged)
  ;;DIR defaults to default-directory."
  ;; DIR must be file-name-as-directory, as with all directory args in
  ;; Emacs Lisp code.
  (or dir (setq dir default-directory))
  ;; This case comes into play if default-directory is set to
  ;; use ~.
  (if (and (> (length dir) 0) (= (aref dir 0) ?~))
      (setq dir (expand-file-name dir)))
  (if (string-match (concat "^" (regexp-quote dir)) file)
      (substring file (match-end 0))
    (if no-error
	file
      (error "%s: not in directory tree growing at %s" file dir))))

;;; Functions for finding the file name in a dired buffer line.

(defvar dired-move-to-filename-regexp
  "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ ]+[0-9]+ [ 0-9][0-9][:0-9][0-9][ 0-9] "
  "Regular expression to match a month abbreviation followed by a number.")

;; Move to first char of filename on this line.
;; Returns position (point) or nil if no filename on this line."
(defun dired-move-to-filename (&optional raise-error eol)
  ;; This is the UNIX version.
  (or eol (setq eol (progn (end-of-line) (point))))
  (beginning-of-line)
  (if (re-search-forward dired-move-to-filename-regexp eol t)
      (goto-char (match-end 0))
    (if raise-error
	(error "No file on this line"))))

(defun dired-move-to-end-of-filename (&optional no-error)
  ;; Assumes point is at beginning of filename,
  ;; thus the rwx bit re-search-backward below will succeed in *this*
  ;; line if at all.  So, it should be called only after
  ;; (dired-move-to-filename t).
  ;; On failure, signals an error (with non-nil NO-ERROR just returns nil).
  ;; This is the UNIX version.
  (let (opoint file-type executable symlink hidden case-fold-search used-F eol)
    ;; case-fold-search is nil now, so we can test for capital F:
    (setq used-F (string-match "F" dired-actual-switches)
	  opoint (point)
          eol (save-excursion (end-of-line) (point))
	  hidden (and selective-display
		      (save-excursion (search-forward "\r" eol t))))
    (if hidden
	nil
      (save-excursion;; Find out what kind of file this is:
	;; Restrict perm bits to be non-blank,
	;; otherwise this matches one char to early (looking backward):
	;; "l---------" (some systems make symlinks that way)
	;; "----------" (plain file with zero perms)
	(if (re-search-backward
	     "\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)"
	     nil t)
	    (setq file-type (char-after (match-beginning 1))
		  symlink (eq file-type ?l)
		  ;; Only with -F we need to know whether it's an executable
		  executable (and
			      used-F
			      (string-match
			       "[xst]";; execute bit set anywhere?
			       (concat
				(buffer-substring (match-beginning 2)
						  (match-end 2))
				(buffer-substring (match-beginning 3)
						  (match-end 3))
				(buffer-substring (match-beginning 4)
						  (match-end 4))))))
	  (or no-error (error "No file on this line"))))
      ;; Move point to end of name:
      (if symlink
	  (if (search-forward " ->" eol t)
	      (progn
		(forward-char -3)
		(and used-F
		     dired-ls-F-marks-symlinks
		     (eq (preceding-char) ?@);; did ls really mark the link?
		     (forward-char -1))))
	(goto-char eol);; else not a symbolic link
	;; ls -lF marks dirs, sockets and executables with exactly one
	;; trailing character. (Executable bits on symlinks ain't mean
	;; a thing, even to ls, but we know it's not a symlink.)
	(and used-F
	     (or (memq file-type '(?d ?s))
		 executable)
	     (forward-char -1))))
    (or no-error
	(not (eq opoint (point)))
	(error (if hidden
		   (substitute-command-keys
		    "File line is hidden, type \\[dired-hide-subdir] to unhide")
		 "No file on this line")))
    (if (eq opoint (point))
	nil
      (point))))


;; Keeping Dired buffers in sync with the filesystem and with each other

(defvar dired-buffers nil
  ;; Enlarged by dired-advertise
  ;; Queried by function dired-buffers-for-dir. When this detects a
  ;; killed buffer, it is removed from this list.
  "Alist of expanded directories and their associated dired buffers.")

(defun dired-buffers-for-dir (dir)
;; Return a list of buffers that dired DIR (top level or in-situ subdir).
;; The list is in reverse order of buffer creation, most recent last.
;; As a side effect, killed dired buffers for DIR are removed from
;; dired-buffers.
  (setq dir (file-name-as-directory dir))
  (let ((alist dired-buffers) result elt buf)
    (while alist
      (setq elt (car alist)
	    buf (cdr elt))
      (if (buffer-name buf)
	  (if (dired-in-this-tree dir (car elt))
	      (if (assoc dir (save-excursion
			       (set-buffer buf)
			       dired-subdir-alist))
		  (setq result (cons buf result))))
	;; else buffer is killed - clean up:
	(setq dired-buffers (delq elt dired-buffers)))
      (setq alist (cdr alist)))
    result))

(defun dired-advertise ()
  ;;"Advertise in variable `dired-buffers' that we dired `default-directory'."
  ;; With wildcards we actually advertise too much.
  (let ((expanded-default (expand-file-name default-directory)))
    (if (memq (current-buffer) (dired-buffers-for-dir expanded-default))
	t				; we have already advertised ourselves
      (setq dired-buffers
	    (cons (cons expanded-default (current-buffer))
		  dired-buffers)))))

(defun dired-unadvertise (dir)
  ;; Remove DIR from the buffer alist in variable dired-buffers.
  ;; This has the effect of removing any buffer whose main directory is DIR.
  ;; It does not affect buffers in which DIR is a subdir.
  ;; Removing is also done as a side-effect in dired-buffer-for-dir.
  (setq dired-buffers
	(delq (assoc (expand-file-name dir) dired-buffers) dired-buffers)))

;; Tree Dired

;;; utility functions

(defun dired-in-this-tree (file dir)
  ;;"Is FILE part of the directory tree starting at DIR?"
  (let (case-fold-search)
    (string-match (concat "^" (regexp-quote dir)) file)))

(defun dired-normalize-subdir (dir)
  ;; Prepend default-directory to DIR if relative path name.
  ;; dired-get-filename must be able to make a valid filename from a
  ;; file and its directory DIR.
  (file-name-as-directory
   (if (file-name-absolute-p dir)
       dir
     (expand-file-name dir default-directory))))

(defun dired-get-subdir ()
  ;;"Return the subdir name on this line, or nil if not on a headerline."
  ;; Look up in the alist whether this is a headerline.
  (save-excursion
    (let ((cur-dir (dired-current-directory)))
      (beginning-of-line)		; alist stores b-o-l positions
      (and (zerop (- (point)
		     (dired-get-subdir-min (assoc cur-dir
						  dired-subdir-alist))))
	   cur-dir))))

;(defun dired-get-subdir-min (elt)
;  (cdr elt))
;; can't use macro,  must be redefinable for other alist format in dired-nstd.
(defalias 'dired-get-subdir-min 'cdr)

(defun dired-get-subdir-max (elt)
  (save-excursion
    (goto-char (dired-get-subdir-min elt))
    (dired-subdir-max)))

(defun dired-clear-alist ()
  (while dired-subdir-alist
    (set-marker (dired-get-subdir-min (car dired-subdir-alist)) nil)
    (setq dired-subdir-alist (cdr dired-subdir-alist))))

(defun dired-subdir-index (dir)
  ;; Return an index into alist for use with nth
  ;; for the sake of subdir moving commands.
  (let (found (index 0) (alist dired-subdir-alist))
    (while alist
      (if (string= dir (car (car alist)))
	  (setq alist nil found t)
	(setq alist (cdr alist) index (1+ index))))
    (if found index nil)))

(defun dired-next-subdir (arg &optional no-error-if-not-found no-skip)
  "Go to next subdirectory, regardless of level."
  ;; Use 0 arg to go to this directory's header line.
  ;; NO-SKIP prevents moving to end of header line, returning whatever
  ;; position was found in dired-subdir-alist.
  (interactive "p")
  (let ((this-dir (dired-current-directory))
	pos index)
    ;; nth with negative arg does not return nil but the first element
    (setq index (- (dired-subdir-index this-dir) arg))
    (setq pos (if (>= index 0)
		  (dired-get-subdir-min (nth index dired-subdir-alist))))
    (if pos
	(progn
	  (goto-char pos)
	  (or no-skip (skip-chars-forward "^\n\r"))
	  (point))
      (if no-error-if-not-found
	  nil				; return nil if not found
	(error "%s directory" (if (> arg 0) "Last" "First"))))))

(defun dired-build-subdir-alist ()
  "Build `dired-subdir-alist' by parsing the buffer.
Returns the new value of the alist."
  (interactive)
  (dired-clear-alist)
  (save-excursion
    (let ((count 0)
	  (buffer-read-only nil)
	  new-dir-name)
      (goto-char (point-min))
      (setq dired-subdir-alist nil)
      (while (and (re-search-forward dired-subdir-regexp nil t)
		  ;; Avoid taking a file name ending in a colon
		  ;; as a subdir name.
		  (not (save-excursion
			 (goto-char (match-beginning 0))
			 (beginning-of-line)
			 (forward-char 2)
			 (save-match-data (looking-at dired-re-perms)))))
	(save-excursion
	  (goto-char (match-beginning 1))
	  (setq new-dir-name
		(expand-file-name (buffer-substring (point) (match-end 1))))
	  (delete-region (point) (match-end 1))
	  (insert new-dir-name))
	(setq count (1+ count))
	(dired-alist-add-1 new-dir-name
			   ;; Place a sub directory boundary between lines.
			   (save-excursion
			     (goto-char (match-beginning 0))
			     (beginning-of-line)
			     (point-marker))))
      (if (> count 1)
	  (message "Buffer includes %d directories" count))
      ;; We don't need to sort it because it is in buffer order per
      ;; constructionem.  Return new alist:
      dired-subdir-alist)))

(defun dired-alist-add-1 (dir new-marker)
  ;; Add new DIR at NEW-MARKER.  Don't sort.
  (setq dired-subdir-alist
	(cons (cons (dired-normalize-subdir dir) new-marker)
	      dired-subdir-alist)))

(defun dired-goto-next-nontrivial-file ()
  ;; Position point on first nontrivial file after point.
  (dired-goto-next-file);; so there is a file to compare with
  (if (stringp dired-trivial-filenames)
      (while (and (not (eobp))
		  (string-match dired-trivial-filenames
				(file-name-nondirectory
				 (or (dired-get-filename nil t) ""))))
	(forward-line 1)
	(dired-move-to-filename))))

(defun dired-goto-next-file ()
  (let ((max (1- (dired-subdir-max))))
    (while (and (not (dired-move-to-filename)) (< (point) max))
      (forward-line 1))))

(defun dired-goto-file (file)
  "Go to file line of FILE in this dired buffer."
  ;; Return value of point on success, else nil.
  ;; FILE must be an absolute pathname.
  ;; Loses if FILE contains control chars like "\007" for which ls
  ;; either inserts "?" or "\\007" into the buffer, so we won't find
  ;; it in the buffer.
  (interactive
   (prog1				; let push-mark display its message
       (list (expand-file-name
	      (read-file-name "Goto file: "
			      (dired-current-directory))))
     (push-mark)))
  (setq file (directory-file-name file)) ; does no harm if no directory
  (let (found case-fold-search dir)
    (setq dir (or (file-name-directory file)
		  (error "Need absolute pathname for %s" file)))
    (save-excursion
      ;; The hair here is to get the result of dired-goto-subdir
      ;; without really calling it if we don't have any subdirs.
      (if (if (string= dir (expand-file-name default-directory))
	      (goto-char (point-min))
	    (and (cdr dired-subdir-alist)
		 (dired-goto-subdir dir)))
	  (let ((base (file-name-nondirectory file))
		(boundary (dired-subdir-max)))
	    (while (and (not found)
			;; filenames are preceded by SPC, this makes
			;; the search faster (e.g. for the filename "-"!).
 			(search-forward (concat " " base) boundary 'move))
	      ;; Match could have BASE just as initial substring or
	      ;; or in permission bits or date or
	      ;; not be a proper filename at all:
	      (if (equal base (dired-get-filename 'no-dir t))
		    ;; Must move to filename since an (actually
		    ;; correct) match could have been elsewhere on the
		    ;; ;; line (e.g. "-" would match somewhere in the
		    ;; permission bits).
		  (setq found (dired-move-to-filename))
		;; If this isn't the right line, move forward to avoid
		;; trying this line again.
		(forward-line 1))))))
    (and found
	 ;; return value of point (i.e., FOUND):
	 (goto-char found))))

(defun dired-initial-position (dirname)
  ;; Where point should go in a new listing of DIRNAME.
  ;; Point assumed at beginning of new subdir line.
  ;; You may redefine this function as you wish, e.g. like in dired-x.el.
  (end-of-line)
  (if dired-trivial-filenames (dired-goto-next-nontrivial-file)))

;; These are hooks which make tree dired work.
;; They are in this file because other parts of dired need to call them.
;; But they don't call the rest of tree dired unless there are subdirs loaded.

;; This function is called for each retrieved filename.
;; It could stand to be faster, though it's mostly function call
;; overhead.  Avoiding the function call seems to save about 10% in
;; dired-get-filename.  Make it a defsubst?
(defun dired-current-directory (&optional localp)
  "Return the name of the subdirectory to which this line belongs.
This returns a string with trailing slash, like `default-directory'.
Optional argument means return a file name relative to `default-directory'."
  (let ((here (point))
	(alist (or dired-subdir-alist
		   ;; probably because called in a non-dired buffer
		   (error "No subdir-alist in %s" (current-buffer))))
	elt dir)
    (while alist
      (setq elt (car alist)
	    dir (car elt)
	    ;; use `<=' (not `<') as subdir line is part of subdir
	    alist (if (<= (dired-get-subdir-min elt) here)
		      nil		; found
		    (cdr alist))))
    (if localp
	(dired-make-relative dir default-directory)
      dir)))

;; Subdirs start at the beginning of their header lines and end just
;; before the beginning of the next header line (or end of buffer).

(defun dired-subdir-max ()
  (save-excursion
    (if (or (null (cdr dired-subdir-alist)) (not (dired-next-subdir 1 t t)))
	(point-max)
      (point))))

;; Deleting files

(defun dired-do-flagged-delete ()
  "In dired, delete the files flagged for deletion."
  (interactive)
  (let* ((dired-marker-char dired-del-marker)
	 (regexp (dired-marker-regexp))
	 case-fold-search)
    (if (save-excursion (goto-char (point-min))
			(re-search-forward regexp nil t))
	(dired-internal-do-deletions
	 ;; this can't move point since ARG is nil
	 (dired-map-over-marks (cons (dired-get-filename) (point))
			       nil)
	 nil)
      (message "(No deletions requested)"))))

(defun dired-do-delete (&optional arg)
  "Delete all marked (or next ARG) files."
  ;; This is more consistent with the file marking feature than
  ;; dired-do-flagged-delete.
  (interactive "P")
  (dired-internal-do-deletions
   ;; this may move point if ARG is an integer
   (dired-map-over-marks (cons (dired-get-filename) (point))
			 arg)
   arg))

(defvar dired-deletion-confirmer 'yes-or-no-p) ; or y-or-n-p?

(defun dired-internal-do-deletions (l arg)
  ;; L is an alist of files to delete, with their buffer positions.
  ;; ARG is the prefix arg.
  ;; Filenames are absolute (VMS needs this for logical search paths).
  ;; (car L) *must* be the *last* (bottommost) file in the dired buffer.
  ;; That way as changes are made in the buffer they do not shift the
  ;; lines still to be changed, so the (point) values in L stay valid.
  ;; Also, for subdirs in natural order, a subdir's files are deleted
  ;; before the subdir itself - the other way around would not work.
  (let ((files (mapcar (function car) l))
	(count (length l))
	(succ 0))
    ;; canonicalize file list for pop up
    (setq files (nreverse (mapcar (function dired-make-relative) files)))
    (if (dired-mark-pop-up
	 " *Deletions*" 'delete files dired-deletion-confirmer
	 (format "Delete %s " (dired-mark-prompt arg files)))
	(save-excursion
	  (let (failures);; files better be in reverse order for this loop!
	    (while l
	      (goto-char (cdr (car l)))
	      (let (buffer-read-only)
		(condition-case err
		    (let ((fn (car (car l))))
		      ;; This test is equivalent to
		      ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
		      ;; but more efficient
		      (if (eq t (car (file-attributes fn)))
			  (delete-directory fn)
			(delete-file fn))
		      ;; if we get here, removing worked
		      (setq succ (1+ succ))
		      (message "%s of %s deletions" succ count)
		      (delete-region (progn (beginning-of-line) (point))
				     (progn (forward-line 1) (point)))
		      (dired-clean-up-after-deletion fn))
		  (error;; catch errors from failed deletions
		   (dired-log "%s\n" err)
		   (setq failures (cons (car (car l)) failures)))))
	      (setq l (cdr l)))
	    (if (not failures)
		(message "%d deletion%s done" count (dired-plural-s count))
	      (dired-log-summary
	       (format "%d of %d deletion%s failed"
		       (length failures) count
		       (dired-plural-s count))
	       failures))))
      (message "(No deletions performed)")))
  (dired-move-to-filename))

;; This is a separate function for the sake of dired-x.el.
(defun dired-clean-up-after-deletion (fn)
  ;; Clean up after a deleted file or directory FN.
  (save-excursion (and (cdr dired-subdir-alist)
		       (dired-goto-subdir fn)
		       (dired-kill-subdir))))

;; Confirmation

(defun dired-marker-regexp ()
  (concat "^" (regexp-quote (char-to-string dired-marker-char))))

(defun dired-plural-s (count)
  (if (= 1 count) "" "s"))

(defun dired-mark-prompt (arg files)
  ;; Return a string for use in a prompt, either the current file
  ;; name, or the marker and a count of marked files.
  (let ((count (length files)))
    (if (= count 1)
	(car files)
      ;; more than 1 file:
      (if (integerp arg)
	  ;; abs(arg) = count
	  ;; Perhaps this is nicer, but it also takes more screen space:
	  ;;(format "[%s %d files]" (if (> arg 0) "next" "previous")
	  ;;                        count)
	  (format "[next %d files]" arg)
	(format "%c [%d files]" dired-marker-char count)))))

(defun dired-pop-to-buffer (buf)
  ;; Pop up buffer BUF.
  ;; If dired-shrink-to-fit is t, make its window fit its contents.
  (if (not dired-shrink-to-fit)
      (pop-to-buffer (get-buffer-create buf))
    ;; let window shrink to fit:
    (let ((window (selected-window))
	  target-lines w2)
      (cond ;; if split-window-threshold is enabled, use the largest window
            ((and (> (window-height (setq w2 (get-largest-window)))
		     split-height-threshold)
		  (= (frame-width) (window-width w2)))
	     (setq window w2))
	    ;; if the least-recently-used window is big enough, use it
	    ((and (> (window-height (setq w2 (get-lru-window)))
		     (* 2 window-min-height))
		  (= (frame-width) (window-width w2)))
	     (setq window w2)))
      (save-excursion
	(set-buffer buf)
	(goto-char (point-max))
	(skip-chars-backward "\n\r\t ")
	(setq target-lines (count-lines (point-min) (point)))
	;; Don't forget to count the last line.
	(if (not (bolp))
	    (setq target-lines (1+ target-lines))))
      (if (<= (window-height window) (* 2 window-min-height))
	  ;; At this point, every window on the frame is too small to split.
	  (setq w2 (display-buffer buf))
	(setq w2 (split-window window
		  (max window-min-height
		       (- (window-height window)
			  (1+ (max window-min-height target-lines)))))))
      (set-window-buffer w2 buf)
      (if (< (1- (window-height w2)) target-lines)
	  (progn
	    (select-window w2)
	    (enlarge-window (- target-lines (1- (window-height w2))))))
      (set-window-start w2 1)
      )))

(defvar dired-no-confirm nil
;;  "If non-nil, list of symbols for commands dired should not confirm.
;;It can be a sublist of
;;
;;  '(byte-compile chgrp chmod chown compress copy delete hardlink load
;;    move print shell symlink uncompress)"
  )

(defun dired-mark-pop-up (bufname op-symbol files function &rest args)
  ;;"Args BUFNAME OP-SYMBOL FILES FUNCTION &rest ARGS.
  ;;Return FUNCTION's result on ARGS after popping up a window (in a buffer
  ;;named BUFNAME, nil gives \" *Marked Files*\") showing the marked
  ;;files.  Uses function `dired-pop-to-buffer' to do that.
  ;; FUNCTION should not manipulate files.
  ;; It should only read input (an argument or confirmation).
  ;;The window is not shown if there is just one file or
  ;; OP-SYMBOL is a member of the list in `dired-no-confirm'.
  ;;FILES is the list of marked files."
  (or bufname (setq bufname  " *Marked Files*"))
  (if (or (memq op-symbol dired-no-confirm)
	  (= (length files) 1))
      (apply function args)
    (save-excursion
      (set-buffer (get-buffer-create bufname))
      (erase-buffer)
      (dired-format-columns-of-files files)
      (remove-text-properties (point-min) (point-max) '(mouse-face)))
    (save-window-excursion
      (dired-pop-to-buffer bufname)
      (apply function args))))

(defun dired-format-columns-of-files (files)
  ;; Files should be in forward order for this loop.
  ;; i.e., (car files) = first file in buffer.
  ;; Returns the number of lines used.
  (let* ((maxlen (+ 2 (apply 'max (mapcar 'length files))))
	 (width (- (window-width (selected-window)) 2))
	 (columns (max 1 (/ width maxlen)))
	 (nfiles (length files))
	 (rows (+ (/ nfiles columns)
		  (if (zerop (% nfiles columns)) 0 1)))
	 (i 0)
	 (j 0))
    (setq files (nconc (copy-sequence files) ; fill up with empty fns
		       (make-list (- (* columns rows) nfiles) "")))
    (setcdr (nthcdr (1- (length files)) files) files) ; make circular
    (while (< j rows)
      (while (< i columns)
	(indent-to (* i maxlen))
	(insert (car files))
	(setq files (nthcdr rows files)
	      i (1+ i)))
      (insert "\n")
      (setq i 0
	    j (1+ j)
	    files (cdr files)))
    rows))

;; Commands to mark or flag file(s) at or near current line.

(defun dired-repeat-over-lines (arg function)
  ;; This version skips non-file lines.
  (let ((pos (make-marker)))
    (beginning-of-line)
    (while (and (> arg 0) (not (eobp)))
      (setq arg (1- arg))
      (beginning-of-line)
      (while (and (not (eobp)) (dired-between-files)) (forward-line 1))
      (save-excursion
	(forward-line 1)
	(move-marker pos (1+ (point))))
      (save-excursion (funcall function))
      ;; Advance to the next line--actually, to the line that *was* next.
      ;; (If FUNCTION inserted some new lines in between, skip them.)
      (goto-char pos))
    (while (and (< arg 0) (not (bobp)))
      (setq arg (1+ arg))
      (forward-line -1)
      (while (and (not (bobp)) (dired-between-files)) (forward-line -1))
      (beginning-of-line)
      (save-excursion (funcall function)))
    (move-marker pos nil)
    (dired-move-to-filename)))

(defun dired-between-files ()
  ;; Point must be at beginning of line
  ;; Should be equivalent to (save-excursion (not (dired-move-to-filename)))
  ;; but is about 1.5..2.0 times as fast. (Actually that's not worth it)
  (or (looking-at "^$\\|^. *$\\|^. total\\|^. wildcard")
      (and (looking-at dired-subdir-regexp)
	   (save-excursion (not (dired-move-to-filename))))))

(defun dired-next-marked-file (arg &optional wrap opoint)
  "Move to the next marked file, wrapping around the end of the buffer."
  (interactive "p\np")
  (or opoint (setq opoint (point)));; return to where interactively started
  (if (if (> arg 0)
	  (re-search-forward dired-re-mark nil t arg)
	(beginning-of-line)
	(re-search-backward dired-re-mark nil t (- arg)))
      (dired-move-to-filename)
    (if (null wrap)
	(progn
	  (goto-char opoint)
	  (error "No next marked file"))
      (message "(Wraparound for next marked file)")
      (goto-char (if (> arg 0) (point-min) (point-max)))
      (dired-next-marked-file arg nil opoint))))

(defun dired-prev-marked-file (arg &optional wrap)
  "Move to the previous marked file, wrapping around the end of the buffer."
  (interactive "p\np")
  (dired-next-marked-file (- arg) wrap))

(defun dired-file-marker (file)
  ;; Return FILE's marker, or nil if unmarked.
  (save-excursion
    (and (dired-goto-file file)
	 (progn
	   (beginning-of-line)
	   (if (not (equal ?\040 (following-char)))
	       (following-char))))))

(defun dired-mark-files-in-region (start end)
  (let (buffer-read-only)
    (if (> start end)
	(error "start > end"))
    (goto-char start)			; assumed at beginning of line
    (while (< (point) end)
      ;; Skip subdir line and following garbage like the `total' line:
      (while (and (< (point) end) (dired-between-files))
	(forward-line 1))
      (if (and (not (looking-at dired-re-dot))
	       (dired-get-filename nil t))
	  (progn
	    (delete-char 1)
	    (insert dired-marker-char)))
      (forward-line 1))))

(defun dired-mark (arg)
  "Mark the current (or next ARG) files.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks
and \\[dired-unmark] on a subdir to remove the marks in
this subdir."
  (interactive "P")
  (if (and (cdr dired-subdir-alist) (dired-get-subdir))
      (save-excursion (dired-mark-subdir-files))
    (let (buffer-read-only)
      (dired-repeat-over-lines
       (prefix-numeric-value arg)
       (function (lambda () (delete-char 1) (insert dired-marker-char)))))))

(defun dired-unmark (arg)
  "Unmark the current (or next ARG) files.
If looking at a subdir, unmark all its files except `.' and `..'."
  (interactive "P")
  (let ((dired-marker-char ?\040))
    (dired-mark arg)))

(defun dired-flag-file-deletion (arg)
  "In dired, flag the current line's file for deletion.
With prefix arg, repeat over several lines.

If on a subdir headerline, mark all its files except `.' and `..'."
  (interactive "P")
  (let ((dired-marker-char dired-del-marker))
    (dired-mark arg)))

(defun dired-unmark-backward (arg)
  "In dired, move up lines and remove deletion flag there.
Optional prefix ARG says how many lines to unflag; default is one line."
  (interactive "p")
  (dired-unmark (- arg)))

;;; Commands to mark or flag files based on their characteristics or names.

(defvar dired-regexp-history nil
  "History list of regular expressions used in Dired commands.")

(defun dired-read-regexp (prompt)
  (read-from-minibuffer prompt nil nil nil 'dired-regexp-history))

(defun dired-mark-files-regexp (regexp &optional marker-char)
  "Mark all files matching REGEXP for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$' for
object files--just `.o' will mark more than you might think."
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    " files (regexp): "))
	 (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename nil t)))
	    (and fn (string-match regexp (file-name-nondirectory fn)))))
     "matching file")))

(defun dired-flag-files-regexp (regexp)
  "In dired, flag all files containing the specified REGEXP for deletion.
The match is against the non-directory part of the filename.  Use `^'
  and `$' to anchor matches.  Exclude subdirs by hiding them.
`.' and `..' are never flagged."
  (interactive (list (dired-read-regexp "Flag for deletion (regexp): ")))
  (dired-mark-files-regexp regexp dired-del-marker))

(defun dired-mark-symlinks (unflag-p)
  "Mark all symbolic links.
With prefix argument, unflag all those files."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
    (dired-mark-if (looking-at dired-re-sym) "symbolic link")))

(defun dired-mark-directories (unflag-p)
  "Mark all directory file lines except `.' and `..'.
With prefix argument, unflag all those files."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
    (dired-mark-if (and (looking-at dired-re-dir)
			(not (looking-at dired-re-dot)))
		   "directory file")))

(defun dired-mark-executables (unflag-p)
  "Mark all executable files.
With prefix argument, unflag all those files."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
    (dired-mark-if (looking-at dired-re-exe) "executable file")))

;; dired-x.el has a dired-mark-sexp interactive command: mark
;; files for which PREDICATE returns non-nil.

(defun dired-flag-auto-save-files (&optional unflag-p)
  "Flag for deletion files whose names suggest they are auto save files.
A prefix argument says to unflag those files instead."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\040 dired-del-marker)))
    (dired-mark-if
     ;; It is less than general to check for # here,
     ;; but it's the only way this runs fast enough.
     (and (save-excursion (end-of-line)
                          (or
                           (eq (preceding-char) ?#)
                           ;; Handle executables in case of -F option.
                           ;; We need not worry about the other kinds
                           ;; of markings that -F makes, since they won't
                           ;; appear on real auto-save files.
                           (if (eq (preceding-char) ?*)
                               (progn
                                 (forward-char -1)
                                 (eq (preceding-char) ?#)))))
	  (not (looking-at dired-re-dir))
	  (let ((fn (dired-get-filename t t)))
	    (if fn (auto-save-file-name-p
		    (file-name-nondirectory fn)))))
     "auto save file")))

(defun dired-flag-backup-files (&optional unflag-p)
  "Flag all backup files (names ending with `~') for deletion.
With prefix argument, unflag these files."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\040 dired-del-marker)))
    (dired-mark-if
     ;; It is less than general to check for ~ here,
     ;; but it's the only way this runs fast enough.
     (and (save-excursion (end-of-line)
			  (or
			   (eq (preceding-char) ?~)
			   ;; Handle executables in case of -F option.
			   ;; We need not worry about the other kinds
			   ;; of markings that -F makes, since they won't
			   ;; appear on real backup files.
			   (if (eq (preceding-char) ?*)
			       (progn
				 (forward-char -1)
				 (eq (preceding-char) ?~)))))
	  (not (looking-at dired-re-dir))
	  (let ((fn (dired-get-filename t t)))
	    (if fn (backup-file-name-p fn))))
     "backup file")))

(defun dired-change-marks (&optional old new)
  "Change all OLD marks to NEW marks.
OLD and NEW are both characters used to mark files."
  (interactive
   (let* ((cursor-in-echo-area t)
	  (old (progn (message "Change (old mark): ") (read-char)))
	  (new (progn (message  "Change %c marks to (new mark): " old)
		      (read-char))))
     (list old new)))
  (if (or (eq old ?\r) (eq new ?\r))
      (ding)
    (let ((string (format "\n%c" old))
	  (buffer-read-only))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward string nil t)
	  (subst-char-in-region (match-beginning 0)
				(match-end 0) old new))))))

(defun dired-unmark-all-files-no-query ()
  "Remove all marks from all files in the Dired buffer."
  (interactive)
  (dired-unmark-all-files ?\r))

(defun dired-unmark-all-files (mark &optional arg)
  "Remove a specific mark (or any mark) from every file.
After this command, type the mark character to remove, 
or type RET to remove all marks.
With prefix arg, query for each marked file.
Type \\[help-command] at that time for help."
  (interactive "cRemove marks (RET means all): \nP")
  (save-excursion
    (let* ((count 0)
	   buffer-read-only case-fold-search query
	   (string (format "\n%c" mark))
	   (help-form "\
Type SPC or `y' to unmark one file, DEL or `n' to skip to next,
`!' to unmark all remaining files with no more questions."))
      (goto-char (point-min))
      (while (if (eq mark ?\r)
		 (re-search-forward dired-re-mark nil t)
	       (search-forward string nil t))
	(if (or (not arg)
		(dired-query 'query "Unmark file `%s'? "
			     (dired-get-filename t)))
	    (progn (subst-char-in-region (1- (point)) (point)
					 (preceding-char) ?\ )
		   (setq count (1+ count)))))
      (message (if (= count 1) "1 mark removed"
		 "%d marks removed")
	       count))))

;; Logging failures operating on files, and showing the results.

(defvar dired-log-buffer "*Dired log*")

(defun dired-why ()
  "Pop up a buffer with error log output from Dired.
A group of errors from a single command ends with a formfeed.
Thus, use \\[backward-page] to find the beginning of a group of errors."
  (interactive)
  (if (get-buffer dired-log-buffer)
      (let ((owindow (selected-window))
	    (window (display-buffer (get-buffer dired-log-buffer))))
	(unwind-protect
	    (progn
	      (select-window window)
	      (goto-char (point-max))
	      (recenter -1))
	  (select-window owindow)))))

(defun dired-log (log &rest args)
  ;; Log a message or the contents of a buffer.
  ;; If LOG is a string and there are more args, it is formatted with
  ;; those ARGS.  Usually the LOG string ends with a \n.
  ;; End each bunch of errors with (dired-log t): this inserts
  ;; current time and buffer, and a \f (formfeed).
  (let ((obuf (current-buffer)))
    (unwind-protect			; want to move point
	(progn
	  (set-buffer (get-buffer-create dired-log-buffer))
	  (goto-char (point-max))
	  (let (buffer-read-only)
	    (cond ((stringp log)
		   (insert (if args
			       (apply (function format) log args)
			     log)))
		  ((bufferp log)
		   (insert-buffer log))
		  ((eq t log)
		   (insert "\n\t" (current-time-string)
			   "\tBuffer `" (buffer-name obuf) "'\n\f\n")))))
      (set-buffer obuf))))

(defun dired-log-summary (string failures)
  (message (if failures "%s--type ? for details (%s)"
	     "%s--type ? for details")
	   string failures)
  ;; Log a summary describing a bunch of errors.
  (dired-log (concat "\n" string))
  (dired-log t))

;;; Sorting

;; Most ls can only sort by name or by date (with -t), nothing else.
;; GNU ls sorts on size with -S, on extension with -X, and unsorted with -U.
;; So anything that does not contain these is sort "by name".

(defvar dired-ls-sorting-switches "SXU"
  "String of `ls' switches (single letters) except `t' that influence sorting.")

(defvar dired-sort-by-date-regexp
  (concat "^-[^" dired-ls-sorting-switches
	  "]*t[^" dired-ls-sorting-switches "]*$")
  "Regexp recognized by dired to set `by date' mode.")

(defvar dired-sort-by-name-regexp
  (concat "^-[^t" dired-ls-sorting-switches "]+$")
  "Regexp recognized by dired to set `by name' mode.")

(defun dired-sort-set-modeline ()
  ;; Set modeline display according to dired-actual-switches.
  ;; Modeline display of "by name" or "by date" guarantees the user a
  ;; match with the corresponding regexps.  Non-matching switches are
  ;; shown literally.
  (setq mode-name
	(let (case-fold-search)
	  (cond ((string-match dired-sort-by-name-regexp dired-actual-switches)
		 "Dired by name")
		((string-match dired-sort-by-date-regexp dired-actual-switches)
		 "Dired by date")
		(t
		 (concat "Dired " dired-actual-switches)))))
  ;; update mode line:
  (set-buffer-modified-p (buffer-modified-p)))

(defun dired-sort-toggle-or-edit (&optional arg)
  "Toggle between sort by date/name and refresh the dired buffer.
With a prefix argument you can edit the current listing switches instead."
  (interactive "P")
  (if arg
      (dired-sort-other
       (read-string "ls switches (must contain -l): " dired-actual-switches))
    (dired-sort-toggle)))

(defun dired-sort-toggle ()
  ;; Toggle between sort by date/name.  Reverts the buffer.
  (setq dired-actual-switches
	(let (case-fold-search)
	  (concat
	   "-l"
	   (dired-replace-in-string (concat "[-lt"
					    dired-ls-sorting-switches "]")
				    ""
				    dired-actual-switches)
	   (if (string-match (concat "[t" dired-ls-sorting-switches "]")
			     dired-actual-switches)
	       ""
	     "t"))))
  (dired-sort-set-modeline)
  (revert-buffer))

(defun dired-replace-in-string (regexp newtext string)
  ;; Replace REGEXP with NEWTEXT everywhere in STRING and return result.
  ;; NEWTEXT is taken literally---no \\DIGIT escapes will be recognized.
  (let ((result "") (start 0) mb me)
    (while (string-match regexp string start)
      (setq mb (match-beginning 0)
	    me (match-end 0)
	    result (concat result (substring string start mb) newtext)
	    start me))
    (concat result (substring string start))))

(defun dired-sort-other (switches &optional no-revert)
  ;; Specify new ls SWITCHES for current dired buffer.  Values matching
  ;; `dired-sort-by-date-regexp' or `dired-sort-by-name-regexp' set the
  ;; minor mode accordingly, others appear literally in the mode line.
  ;; With optional second arg NO-REVERT, don't refresh the listing afterwards.
  (setq dired-actual-switches switches)
  (dired-sort-set-modeline)
  (or no-revert (revert-buffer)))

;; To make this file smaller, the less common commands
;; go in a separate file.  But autoload them here
;; to make the separation invisible.

(autoload 'dired-diff "dired-aux"
  "Compare file at point with file FILE using `diff'.
FILE defaults to the file at the mark.
The prompted-for file is the first file given to `diff'."
  t)

(autoload 'dired-backup-diff "dired-aux"
  "Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'."
  t)

(autoload 'dired-clean-directory "dired-aux"
  "Flag numerical backups for deletion.
Spares `dired-kept-versions' latest versions, and `kept-old-versions' oldest.
Positive prefix arg KEEP overrides `dired-kept-versions';
Negative prefix arg KEEP overrides `kept-old-versions' with KEEP made positive.

To clear the flags on these files, you can use \\[dired-flag-backup-files]
with a prefix argument."
  t)

(autoload 'dired-do-chmod "dired-aux"
  "Change the mode of the marked (or next ARG) files.
This calls chmod, thus symbolic modes like `g+w' are allowed."
  t)

(autoload 'dired-do-chgrp "dired-aux"
  "Change the group of the marked (or next ARG) files."
  t)

(autoload 'dired-do-chown "dired-aux"
  "Change the owner of the marked (or next ARG) files."
  t)

(autoload 'dired-do-print "dired-aux"
  "Print the marked (or next ARG) files.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default."
  t)

(autoload 'dired-do-shell-command "dired-aux"
  "Run a shell command COMMAND on the marked files.
If no files are marked or a specific numeric prefix arg is given,
the next ARG files are used.  Just \\[universal-argument] means the current file.
The prompt mentions the file(s) or the marker, as appropriate.

If there is output, it goes to a separate buffer.

Normally the command is run on each file individually.
However, if there is a `*' in the command then it is run
just once with the entire file list substituted there.

No automatic redisplay of dired buffers is attempted, as there's no
telling what files the command may have changed.  Type
\\[dired-do-redisplay] to redisplay the marked files.

The shell command has the top level directory as working directory, so
output files usually are created there instead of in a subdir."
  t)

(autoload 'dired-do-kill-lines "dired-aux"
  "Kill all marked lines (not the files).
With a prefix arg, kill all lines not marked or flagged."
  t)

(autoload 'dired-do-compress "dired-aux"
  "Compress or uncompress marked (or next ARG) files."
  t)

(autoload 'dired-do-byte-compile "dired-aux"
  "Byte compile marked (or next ARG) Emacs Lisp files."
  t)

(autoload 'dired-do-load "dired-aux"
  "Load the marked (or next ARG) Emacs Lisp files."
  t)

(autoload 'dired-do-redisplay "dired-aux"
  "Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing."
  t)

(autoload 'dired-string-replace-match "dired-aux"
  "Replace first match of REGEXP in STRING with NEWTEXT.
If it does not match, nil is returned instead of the new string.
Optional arg LITERAL means to take NEWTEXT literally.
Optional arg GLOBAL means to replace all matches."
  t)

(autoload 'dired-create-directory "dired-aux"
  "Create a directory called DIRECTORY."
  t)

(autoload 'dired-do-copy "dired-aux"
  "Copy all marked (or next ARG) files, or copy the current file.
Thus, a zero prefix argument copies nothing.  But it toggles the
variable `dired-copy-preserve-time' (which see)."
  t)

(autoload 'dired-do-symlink "dired-aux"
  "Make symbolic links to current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new symbolic links are made in that directory
with the same names that the files currently have."
  t)

(autoload 'dired-do-hardlink "dired-aux"
  "Add names (hard links) current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new hard links are made in that directory
with the same names that the files currently have."
  t)

(autoload 'dired-do-rename "dired-aux"
  "Rename current file or all marked (or next ARG) files.
When renaming just the current file, you specify the new name.
When renaming multiple or marked files, you specify a directory."
  t)

(autoload 'dired-do-rename-regexp "dired-aux"
  "Rename marked files containing REGEXP to NEWNAME.
As each match is found, the user must type a character saying
  what to do with it.  For directions, type \\[help-command] at that time.
NEWNAME may contain \\=\\<n> or \\& as in `query-replace-regexp'.
REGEXP defaults to the last regexp used.
With a zero prefix arg, renaming by regexp affects the complete
  pathname - usually only the non-directory part of file names is used
  and changed."
  t)

(autoload 'dired-do-copy-regexp "dired-aux"
  "Copy all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  t)

(autoload 'dired-do-hardlink-regexp "dired-aux"
  "Hardlink all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  t)

(autoload 'dired-do-symlink-regexp "dired-aux"
  "Symlink all marked files containing REGEXP to NEWNAME.
See function `dired-rename-regexp' for more info."
  t)

(autoload 'dired-upcase "dired-aux"
  "Rename all marked (or next ARG) files to upper case."
  t)

(autoload 'dired-downcase "dired-aux"
  "Rename all marked (or next ARG) files to lower case."
  t)

(autoload 'dired-maybe-insert-subdir "dired-aux"
  "Insert this subdirectory into the same dired buffer.
If it is already present, just move to it (type \\[dired-do-redisplay] to refresh),
  else inserts it at its natural place (as `ls -lR' would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output."
  t)

(autoload 'dired-next-subdir "dired-aux"
  "Go to next subdirectory, regardless of level."
  t)

(autoload 'dired-prev-subdir "dired-aux"
  "Go to previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line."
  t)

(autoload 'dired-goto-subdir "dired-aux"
  "Go to end of header line of DIR in this dired buffer.
Return value of point on success, otherwise return nil.
The next char is either \\n, or \\r if DIR is hidden."
  t)

(autoload 'dired-mark-subdir-files "dired-aux"
  "Mark all files except `.' and `..'."
  t)

(autoload 'dired-kill-subdir "dired-aux"
  "Remove all lines of current subdirectory.
Lower levels are unaffected."
  t)

(autoload 'dired-tree-up "dired-aux"
  "Go up ARG levels in the dired tree."
  t)

(autoload 'dired-tree-down "dired-aux"
  "Go down in the dired tree."
  t)

(autoload 'dired-hide-subdir "dired-aux"
  "Hide or unhide the current subdirectory and move to next directory.
Optional prefix arg is a repeat factor.
Use \\[dired-hide-all] to (un)hide all directories."
  t)

(autoload 'dired-hide-all "dired-aux"
  "Hide all subdirectories, leaving only their header lines.
If there is already something hidden, make everything visible again.
Use \\[dired-hide-subdir] to (un)hide a particular subdirectory."
  t)

(if (eq system-type 'vax-vms)
    (load "dired-vms"))

(provide 'dired)

(run-hooks 'dired-load-hook)		; for your customizations

;;; dired.el ends here
