;;; info.el --- info package for Emacs

;; Copyright (C) 1985, 86, 92, 93, 94, 95, 96, 97, 98, 99, 2000, 2001
;;  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help

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

;;; Commentary:

;; Note that nowadays we expect info files to be made using makeinfo.

;;; Code:

(eval-when-compile (require 'jka-compr))

(defgroup info nil
  "Info subsystem"
  :group 'help
  :group 'docs)


(defvar Info-history nil
  "List of info nodes user has visited.
Each element of list is a list (FILENAME NODENAME BUFFERPOS).")

(defcustom Info-enable-edit nil
  "*Non-nil means the \\<Info-mode-map>\\[Info-edit] command in Info can edit the current node.
This is convenient if you want to write info files by hand.
However, we recommend that you not do this.
It is better to write a Texinfo file and generate the Info file from that,
because that gives you a printed manual as well."
  :type 'boolean
  :group 'info)

(defvar Info-enable-active-nodes nil
  "Non-nil allows Info to execute Lisp code associated with nodes.
The Lisp code is executed when the node is selected.")
(put 'Info-enable-active-nodes 'risky-local-variable t)

(defface info-node
  '((((class color) (background light)) (:foreground "brown" :weight bold :slant italic))
    (((class color) (background dark)) (:foreground "white" :weight bold :slant italic))
    (t (:weight bold :slant italic)))
  "Face for Info node names."
  :group 'info)

(defface info-menu-5
  '((((class color)) (:foreground "red1"))
    (t (:underline t)))
  "Face for the fifth and nineth `*' in an Info menu."
  :group 'info)

(defface info-xref
  '((((class color) (background light)) (:foreground "magenta4" :weight bold))
    (((class color) (background dark)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Face for Info cross-references."
  :group 'info)

(defcustom Info-fontify-maximum-menu-size 100000
  "*Maximum size of menu to fontify if `Info-fontify' is non-nil."
  :type 'integer
  :group 'info)

(defcustom Info-use-header-line t
  "*Non-nil means to put the beginning-of-node links in an emacs header-line.
A header-line does not scroll with the rest of the buffer."
  :type 'boolean
  :group 'info)

(defface info-header-xref
  '((t (:inherit info-xref)))
  "Face for Info cross-references in a node header."
  :group 'info)

(defface info-header-node
  '((t (:inherit info-node)))
  "Face for Info nodes in a node header."
  :group 'info)

(defvar Info-directory-list nil
  "List of directories to search for Info documentation files.
nil means not yet initialized.  In this case, Info uses the environment
variable INFOPATH to initialize it, or `Info-default-directory-list'
if there is no INFOPATH variable in the environment.

When `Info-directory-list' is initialized from the value of
`Info-default-directory-list', and Emacs is installed in one of the
standard directories, the directory of Info files that come with Emacs
is put last (so that local Info files override standard ones).

When `Info-directory-list' is initialized from the value of
`Info-default-directory-list', and Emacs is not installed in one
of the standard directories, the first element of the resulting
list is the directory where Emacs installs the Info files that
come with it.  This is so that Emacs's own manual, which suits the
version of Emacs you are using, will always be found first.  This
is useful when you install an experimental version of Emacs without
removing the standard installation.

If you want to override the order of directories in
`Info-default-directory-list', set INFOPATH in the environment.

If you run the Emacs executable from the `src' directory in the Emacs
source tree, and INFOPATH is not defined, the `info' directory in the
source tree is used as the first element of `Info-directory-list', in
place of the installation Info directory.  This is useful when you run
a version of Emacs without installing it.")

(defcustom Info-additional-directory-list nil
  "List of additional directories to search for Info documentation files.
These directories are searched after those in `Info-directory-list', and
they are not searched for merging the `dir' file."
  :type '(repeat directory)
  :group 'info)

(defcustom Info-scroll-prefer-subnodes t
  "*If non-nil, \\<Info-mode-map>\\[Info-scroll-up] in a menu visits subnodes.
If this is non-nil, and you scroll far enough in a node that its menu
appears on the screen, the next \\<Info-mode-map>\\[Info-scroll-up]
moves to a subnode indicated by the following menu item.  This means
that you visit a subnode before getting to the end of the menu.

Setting this option to nil results in behavior similar to the stand-alone
Info reader program, which visits the first subnode from the menu only
when you hit the end of the current node."
  :type 'boolean
  :group 'info)

(defcustom Info-mode-hook '(turn-on-font-lock)
  "Hooks run when `info-mode' is called."
  :type 'hook
  :group 'info)

(defvar Info-current-file nil
  "Info file that Info is now looking at, or nil.
This is the name that was specified in Info, not the actual file name.
It doesn't contain directory names or file name extensions added by Info.
Can also be t when using `Info-on-current-buffer'.")

(defvar Info-current-subfile nil
  "Info subfile that is actually in the *info* buffer now.
nil if current info file is not split into subfiles.")

(defvar Info-current-node nil
  "Name of node that Info is now looking at, or nil.")

(defvar Info-tag-table-marker nil
  "Marker pointing at beginning of current Info file's tag table.
Marker points nowhere if file has no tag table.")

(defvar Info-tag-table-buffer nil
  "Buffer used for indirect tag tables.")

(defvar Info-current-file-completions nil
  "Cached completion list for current Info file.")

(defvar Info-index-alternatives nil
  "List of possible matches for last `Info-index' command.")

(defvar Info-standalone nil
  "Non-nil if Emacs was started solely as an Info browser.")

(defvar Info-suffix-list
  ;; The MS-DOS list should work both when long file names are
  ;; supported (Windows 9X), and when only 8+3 file names are available.
  (if (eq system-type 'ms-dos)
      '( (".gz"      . "gunzip")
	 (".z"       . "gunzip")
	 (".bz2"     . ("bzip2" "-dc"))
	 (".inz"     . "gunzip")
	 (".igz"     . "gunzip")
	 (".info.Z"  . "gunzip")
	 (".info.gz" . "gunzip")
	 ("-info.Z"  . "gunzip")
	 ("-info.gz" . "gunzip")
	 ("/index.gz". "gunzip")
	 ("/index.z" . "gunzip")
	 (".inf"     . nil)
	 (".info"    . nil)
	 ("-info"    . nil)
	 ("/index"   . nil)
	 (""         . nil))
    '( (".info.Z".    "uncompress")
       (".info.Y".    "unyabba")
       (".info.gz".   "gunzip")
       (".info.z".    "gunzip")
       (".info.bz2" . ("bzip2" "-dc"))
       (".info".      nil)
       ("-info.Z".   "uncompress")
       ("-info.Y".   "unyabba")
       ("-info.gz".  "gunzip")
       ("-info.bz2" . ("bzip2" "-dc"))
       ("-info.z".   "gunzip")
       ("-info".     nil)
       ("/index.Z".   "uncompress")
       ("/index.Y".   "unyabba")
       ("/index.gz".  "gunzip")
       ("/index.z".   "gunzip")
       ("/index.bz2". ("bzip2" "-dc"))
       ("/index".     nil)
       (".Z".         "uncompress")
       (".Y".         "unyabba")
       (".gz".        "gunzip")
       (".z".         "gunzip")
       (".bz2" .      ("bzip2" "-dc"))
       ("".           nil)))
  "List of file name suffixes and associated decoding commands.
Each entry should be (SUFFIX . STRING); the file is given to
the command as standard input.

STRING may be a list of strings.  In that case, the first element is
the command name, and the rest are arguments to that command.

If STRING is nil, no decoding is done.
Because the SUFFIXes are tried in order, the empty string should
be last in the list.")

;; Concatenate SUFFIX onto FILENAME.  SUFFIX should start with a dot.
;; First, on MS-DOS with no long file names support, delete some of
;; the extension in FILENAME to make room.
(defun info-insert-file-contents-1 (filename suffix lfn)
  (if lfn	; long file names are supported
      (concat filename suffix)
    (let* ((sans-exts (file-name-sans-extension filename))
	   ;; How long is the extension in FILENAME (not counting the dot).
	   (ext-len (max 0 (- (length filename) (length sans-exts) 1)))
	   ext-left)
      ;; SUFFIX starts with a dot.  If FILENAME already has one,
      ;; get rid of the one in SUFFIX (unless suffix is empty).
      (or (and (<= ext-len 0)
	       (not (eq (aref filename (1- (length filename))) ?.)))
	  (= (length suffix) 0)
	  (setq suffix (substring suffix 1)))
      ;; How many chars of that extension should we keep?
      (setq ext-left (min ext-len (max 0 (- 3 (length suffix)))))
      ;; Get rid of the rest of the extension, and add SUFFIX.
      (concat (substring filename 0 (- (length filename)
				       (- ext-len ext-left)))
	      suffix))))

(defun info-file-exists-p (filename)
  (and (file-exists-p filename)
       (not (file-directory-p filename))))

(defun info-insert-file-contents (filename &optional visit)
  "Insert the contents of an info file in the current buffer.
Do the right thing if the file has been compressed or zipped."
  (let* ((tail Info-suffix-list)
	 (lfn (or (not (fboundp 'msdos-long-file-names))
		  (msdos-long-file-names)))
	 (check-short (and (fboundp 'msdos-long-file-names)
			   lfn))
	 fullname decoder done)
    (if (info-file-exists-p filename)
	;; FILENAME exists--see if that name contains a suffix.
	;; If so, set DECODE accordingly.
	(progn
	  (while (and tail
		      (not (string-match
			    (concat (regexp-quote (car (car tail))) "$")
			    filename)))
	    (setq tail (cdr tail)))
	  (setq fullname filename
		decoder (cdr (car tail))))
      ;; Try adding suffixes to FILENAME and see if we can find something.
      (while (and tail (not done))
	(setq fullname (info-insert-file-contents-1 filename
						    (car (car tail)) lfn))
	(if (info-file-exists-p fullname)
	    (setq done t
		  ;; If we found a file with a suffix, set DECODER
		  ;; according to the suffix.
		  decoder (cdr (car tail)))
	  ;; When the MS-DOS port runs on Windows, we need to check
	  ;; the short variant of a long file name as well.
	  (when check-short
	    (setq fullname (info-insert-file-contents-1 filename
							(car (car tail)) nil))
	    (if (info-file-exists-p fullname)
		(setq done t
		      decoder (cdr (car tail))))))
	(setq tail (cdr tail)))
      (or tail
	  (error "Can't find %s or any compressed version of it" filename)))
    ;; check for conflict with jka-compr
    (if (and (featurep 'jka-compr)
	     (jka-compr-installed-p)
	     (jka-compr-get-compression-info fullname))
	(setq decoder nil))
    (if decoder
	(progn
	  (insert-file-contents-literally fullname visit)
	  (let ((buffer-read-only nil)
		(coding-system-for-write 'no-conversion)
		(default-directory (or (file-name-directory fullname)
				       default-directory)))
	    (or (consp decoder)
		(setq decoder (list decoder)))
	    (apply 'call-process-region (point-min) (point-max)
		   (car decoder) t t nil (cdr decoder))))
      (insert-file-contents fullname visit))))

(defun info-initialize ()
  "Initialize `Info-directory-list', if that hasn't been done yet."
  (unless Info-directory-list
    (let ((path (getenv "INFOPATH"))
	  (source (expand-file-name "info/" source-directory))
	  (sibling (if installation-directory
		       (expand-file-name "info/" installation-directory)
		     (if invocation-directory
			 (let ((infodir (expand-file-name
					 "../info/"
					 invocation-directory)))
			   (if (file-exists-p infodir)
			       infodir
			     (setq infodir (expand-file-name
					    "../../../info/"
					    invocation-directory))
			     (and (file-exists-p infodir)
				  infodir))))))
	  alternative)
      (setq Info-directory-list
	    (prune-directory-list
	     (if path
		 (split-string path (regexp-quote path-separator))
	       (if (and sibling (file-exists-p sibling))
		   ;; Uninstalled, Emacs builddir != srcdir.
		   (setq alternative sibling)
		 ;; Uninstalled, builddir == srcdir
		 (setq alternative source))
	       (if (or (member alternative Info-default-directory-list)
		       ;; On DOS/NT, we use movable executables always,
		       ;; and we must always find the Info dir at run time.
		       (if (memq system-type '(ms-dos windows-nt))
			   nil
			 ;; Use invocation-directory for Info
			 ;; only if we used it for exec-directory also.
			 (not (string= exec-directory
				       (expand-file-name "lib-src/"
							 installation-directory))))
		       (not (file-exists-p alternative)))
		   Info-default-directory-list
		 ;; `alternative' contains the Info files that came with this
		 ;; version, so we should look there first.  `Info-insert-dir'
		 ;; currently expects to find `alternative' first on the list.
		 (cons alternative
		       (reverse (cdr (reverse Info-default-directory-list)))))))))))

;;;###autoload
(defun info-other-window (&optional file)
  "Like `info' but show the Info buffer in another window."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Info file name: " nil nil t))))
  (let (same-window-buffer-names)
    (info file)))

;;;###autoload (add-hook 'same-window-buffer-names "*info*")

;;;###autoload
(defun info (&optional file)
  "Enter Info, the documentation browser.
Optional argument FILE specifies the file to examine;
the default is the top-level directory of Info.
Called from a program, FILE may specify an Info node of the form
`(FILENAME)NODENAME'.

In interactive use, a prefix argument directs this command
to read a file name from the minibuffer.

The search path for Info files is in the variable `Info-directory-list'.
The top-level Info directory is made by combining all the files named `dir'
in all the directories in that path."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Info file name: " nil nil t))))
  (if file
      (progn
	(pop-to-buffer "*info*")
	;; If argument already contains parentheses, don't add another set
	;; since the argument will then be parsed improperly.  This also
	;; has the added benefit of allowing node names to be included
	;; following the parenthesized filename.
	(if (and (stringp file) (string-match "(.*)" file))
	    (Info-goto-node file)
	  (Info-goto-node (concat "(" file ")"))))
    (if (get-buffer "*info*")
	(pop-to-buffer "*info*")
      (Info-directory))))

;;;###autoload
(defun info-standalone ()
  "Run Emacs as a standalone Info reader.
Usage:  emacs -f info-standalone [filename]
In standalone mode, \\<Info-mode-map>\\[Info-exit] exits Emacs itself."
  (setq Info-standalone t)
  (if (and command-line-args-left
	   (not (string-match "^-" (car command-line-args-left))))
      (condition-case err
	  (progn
	    (info (car command-line-args-left))
	    (setq command-line-args-left (cdr command-line-args-left)))
	(error (send-string-to-terminal
		(format "%s\n" (if (eq (car-safe err) 'error)
				   (nth 1 err) err)))
	       (save-buffers-kill-emacs)))
    (info)))

;; See if the accessible portion of the buffer begins with a node
;; delimiter, and the node header line which follows matches REGEXP.
;; Typically, this test will be followed by a loop that examines the
;; rest of the buffer with (search-forward "\n\^_"), and it's a pity
;; to have the overhead of this special test inside the loop.

;; This function changes match-data, but supposedly the caller might
;; want to use the results of re-search-backward.

;; The return value is the value of point at the beginning of matching
;; REGEXP, if the function succeeds, nil otherwise.
(defun Info-node-at-bob-matching (regexp)
  (and (bobp)				; are we at beginning of buffer?
       (looking-at "\^_")		; does it begin with node delimiter?
       (let (beg)
	 (forward-line 1)
	 (setq beg (point))
	 (forward-line 1)		; does the line after delimiter match REGEXP?
	 (re-search-backward regexp beg t))))

(defun Info-find-node (filename nodename &optional no-going-back)
  "Go to an info node specified as separate FILENAME and NODENAME.
NO-GOING-BACK is non-nil if recovering from an error in this function;
it says do not attempt further (recursive) error recovery."
  (info-initialize)
  ;; Convert filename to lower case if not found as specified.
  ;; Expand it.
  (if (stringp filename)
      (let (temp temp-downcase found)
        (setq filename (substitute-in-file-name filename))
        (if (string= (downcase filename) "dir")
            (setq found t)
          (let ((dirs (if (string-match "^\\./" filename)
                          ;; If specified name starts with `./'
                          ;; then just try current directory.
                          '("./")
                        (if (file-name-absolute-p filename)
                            ;; No point in searching for an
                            ;; absolute file name
                            '(nil)
                          (if Info-additional-directory-list
                              (append Info-directory-list
                                      Info-additional-directory-list)
                            Info-directory-list)))))
            ;; Search the directory list for file FILENAME.
            (while (and dirs (not found))
              (setq temp (expand-file-name filename (car dirs)))
              (setq temp-downcase
                    (expand-file-name (downcase filename) (car dirs)))
              ;; Try several variants of specified name.
              (let ((suffix-list Info-suffix-list)
		    (lfn (or (not (fboundp 'msdos-long-file-names))
			     (msdos-long-file-names))))
                (while (and suffix-list (not found))
                  (cond ((info-file-exists-p
                          (info-insert-file-contents-1
                           temp (car (car suffix-list)) lfn))
                         (setq found temp))
                        ((info-file-exists-p
                          (info-insert-file-contents-1
                           temp-downcase (car (car suffix-list)) lfn))
                         (setq found temp-downcase))
			((and (fboundp 'msdos-long-file-names)
			      lfn
			      (info-file-exists-p
			       (info-insert-file-contents-1
				temp (car (car suffix-list)) nil)))
			 (setq found temp)))
                  (setq suffix-list (cdr suffix-list))))
              (setq dirs (cdr dirs)))))
        (if found
            (setq filename found)
          (error "Info file %s does not exist" filename))))
  ;; Record the node we are leaving.
  (if (and Info-current-file (not no-going-back))
      (setq Info-history
            (cons (list Info-current-file Info-current-node (point))
                  Info-history)))
  ;; Go into info buffer.
  (or (eq major-mode 'Info-mode) (pop-to-buffer "*info*"))
  (Info-find-node-2 filename nodename no-going-back))

(defun Info-on-current-buffer (&optional nodename)
  "Use the `Info-mode' to browse the current info buffer.
If a prefix arg is provided, it queries for the NODENAME which
else defaults to \"Top\"."
  (interactive
   (list (if current-prefix-arg
	     (completing-read "Node name: " (Info-build-node-completions)
			      nil t "Top"))))
  (unless nodename (setq nodename "Top"))
  (info-initialize)
  (Info-mode)
  (set (make-local-variable 'Info-current-file) t)
  (Info-find-node-2 nil nodename))

(defun Info-find-in-tag-table-1 (marker regexp case-fold)
  "Find a node in a tag table.
MARKER specifies the buffer and position to start searching at.
REGEXP is a regular expression matching nodes or references.  Its first
group should match `Node:' or `Ref:'.
CASE-FOLD t means search for a case-insensitive match.
If a match was found, value is a list (FOUND-ANCHOR POS MODE), where
FOUND-ANCHOR is non-nil if a `Ref:' was matched, POS is the position
where the match was found, and MODE is `major-mode' of the buffer in
which the match was found."
  (let ((case-fold-search case-fold)
	found-mode guesspos found-anchor)
    (save-excursion
      (set-buffer (marker-buffer marker))
      (goto-char marker)
    
      ;; Search tag table
      (beginning-of-line)
      (when (re-search-forward regexp nil t)
	(list (string-equal "Ref:" (match-string 1))
	      (+ (point-min) (read (current-buffer)))
	      major-mode)))))

(defun Info-find-in-tag-table (marker regexp)
  "Find a node in a tag table.
MARKER specifies the buffer and position to start searching at.
REGEXP is a regular expression matching nodes or references.  Its first
group should match `Node:' or `Ref:'.
If a match was found, value is a list (FOUND-ANCHOR POS MODE), where
FOUND-ANCHOR is non-nil if a `Ref:' was matched, POS is the position
where the match was found, and MODE is `major-mode' of the buffer in
which the match was found.
This function tries to find a case-sensitive match first, then a
case-insensitive match is tried."
  (let ((result (Info-find-in-tag-table-1 marker regexp nil)))
    (when (null (car result))
      (setq result (Info-find-in-tag-table-1 marker regexp t)))
    result))

(defun Info-find-node-in-buffer-1 (regexp case-fold)
  "Find a node or anchor in the current buffer.
REGEXP is a regular expression matching nodes or references.  Its first
group should match `Node:' or `Ref:'.
CASE-FOLD t means search for a case-insensitive match.
Value is the position at which a match was found, or nil if not found."
  (let ((case-fold-search case-fold)
	found)
    (save-excursion
      (when (Info-node-at-bob-matching regexp)
	(setq found (point)))
      (while (and (not found)
		  (search-forward "\n\^_" nil t))
	(forward-line 1)
	(let ((beg (point)))
	  (forward-line 1)
	  (when (re-search-backward regexp beg t)
	    (beginning-of-line)
	    (setq found (point)))))
      found)))
		  
(defun Info-find-node-in-buffer (regexp)
  "Find a node or anchor in the current buffer.
REGEXP is a regular expression matching nodes or references.  Its first
group should match `Node:' or `Ref:'.
Value is the position at which a match was found, or nil if not found.
This function looks for a case-sensitive match first.  If none is found,
a case-insensitive match is tried."
  (or (Info-find-node-in-buffer-1 regexp nil)
      (Info-find-node-in-buffer-1 regexp t)))
  
(defun Info-find-node-2 (filename nodename &optional no-going-back)
  (buffer-disable-undo (current-buffer))
  (or (eq major-mode 'Info-mode)
      (Info-mode))
  (widen)
  (setq Info-current-node nil)
  (unwind-protect
      (let ((case-fold-search t)
	    anchorpos)
        ;; Switch files if necessary
        (or (null filename)
            (equal Info-current-file filename)
            (let ((buffer-read-only nil))
              (setq Info-current-file nil
                    Info-current-subfile nil
                    Info-current-file-completions nil
                    buffer-file-name nil)
              (erase-buffer)
              (if (eq filename t)
                  (Info-insert-dir)
                (info-insert-file-contents filename nil)
                (setq default-directory (file-name-directory filename)))
              (set-buffer-modified-p nil)
              ;; See whether file has a tag table.  Record the location if yes.
              (goto-char (point-max))
              (forward-line -8)
              ;; Use string-equal, not equal, to ignore text props.
              (if (not (or (string-equal nodename "*")
                           (not
                            (search-forward "\^_\nEnd tag table\n" nil t))))
                  (let (pos)
                    ;; We have a tag table.  Find its beginning.
                    ;; Is this an indirect file?
                    (search-backward "\nTag table:\n")
                    (setq pos (point))
                    (if (save-excursion
                          (forward-line 2)
                          (looking-at "(Indirect)\n"))
                        ;; It is indirect.  Copy it to another buffer
                        ;; and record that the tag table is in that buffer.
                        (let ((buf (current-buffer))
                              (tagbuf
                               (or Info-tag-table-buffer
                                   (generate-new-buffer " *info tag table*"))))
                          (setq Info-tag-table-buffer tagbuf)
                          (save-excursion
                            (set-buffer tagbuf)
                            (buffer-disable-undo (current-buffer))
                            (setq case-fold-search t)
                            (erase-buffer)
                            (insert-buffer-substring buf))
                          (set-marker Info-tag-table-marker
                                      (match-end 0) tagbuf))
                      (set-marker Info-tag-table-marker pos)))
                (set-marker Info-tag-table-marker nil))
              (setq Info-current-file
                    (if (eq filename t) "dir" filename))))
        ;; Use string-equal, not equal, to ignore text props.
        (if (string-equal nodename "*")
            (progn (setq Info-current-node nodename)
                   (Info-set-mode-line))
          ;; Possibilities:
          ;;
          ;; 1. Anchor found in tag table
          ;; 2. Anchor *not* in tag table
          ;;
          ;; 3. Node found in tag table
          ;; 4. Node *not* found in tag table, but found in file
          ;; 5. Node *not* in tag table, and *not* in file
          ;;
          ;; *Or* the same, but in an indirect subfile.

          ;; Search file for a suitable node.
	  (let ((guesspos (point-min))
		(regexp (concat "\\(Node:\\|Ref:\\) *\\("
				(if (stringp nodename)
				    (regexp-quote nodename)
				  "")
				"\\) *[,\t\n\177]"))
		(nodepos nil))

	    (catch 'foo
	      
	      ;; First, search a tag table, if any
	      (when (marker-position Info-tag-table-marker)
		(let* ((m Info-tag-table-marker)
		       (found (Info-find-in-tag-table m regexp)))
		  
		  (when found
		    ;; FOUND is (ANCHOR POS MODE).
		    (setq guesspos (nth 1 found))
		    
		    ;; If this is an indirect file, determine which
		    ;; file really holds this node and read it in.
		    (unless (eq (nth 2 found) 'Info-mode)
		      ;; Note that the current buffer must be the
		      ;; *info* buffer on entry to
		      ;; Info-read-subfile.  Thus the hackery above.
		      (setq guesspos (Info-read-subfile guesspos)))

		    ;; Handle anchor
		    (when (nth 0 found)
		      (goto-char (setq anchorpos guesspos))
		      (throw 'foo t)))))

	      ;; Else we may have a node, which we search for:
	      (goto-char (max (point-min)
			      (- (byte-to-position guesspos) 1000)))
	      
	      ;; Now search from our advised position (or from beg of
	      ;; buffer) to find the actual node.  First, check
	      ;; whether the node is right where we are, in case the
	      ;; buffer begins with a node.
	      (let ((pos (Info-find-node-in-buffer regexp)))
		(when pos
		  (goto-char pos)
		  (throw 'foo t))
		(error "No such anchor in tag table or node in tag table or file: %s"
		       nodename)))

	    (Info-select-node)
	    (goto-char (or anchorpos (point-min))))))
    ;; If we did not finish finding the specified node,
    ;; go back to the previous one.
    (or Info-current-node no-going-back (null Info-history)
        (let ((hist (car Info-history)))
          (setq Info-history (cdr Info-history))
          (Info-find-node (nth 0 hist) (nth 1 hist) t)
          (goto-char (nth 2 hist))))))

;; Cache the contents of the (virtual) dir file, once we have merged
;; it for the first time, so we can save time subsequently.
(defvar Info-dir-contents nil)

;; Cache for the directory we decided to use for the default-directory
;; of the merged dir text.
(defvar Info-dir-contents-directory nil)

;; Record the file attributes of all the files from which we
;; constructed Info-dir-contents.
(defvar Info-dir-file-attributes nil)

(defvar Info-dir-file-name nil)

;; Construct the Info directory node by merging the files named `dir'
;; from various directories.  Set the *info* buffer's
;; default-directory to the first directory we actually get any text
;; from.
(defun Info-insert-dir ()
  (if (and Info-dir-contents Info-dir-file-attributes
	   ;; Verify that none of the files we used has changed
	   ;; since we used it.
	   (eval (cons 'and
		       (mapcar (lambda (elt)
				 (let ((curr (file-attributes
					      ;; Handle symlinks
					      (file-truename (car elt)))))

				   ;; Don't compare the access time.
				   (if curr (setcar (nthcdr 4 curr) 0))
				   (setcar (nthcdr 4 (cdr elt)) 0)
				   (equal (cdr elt) curr)))
			       Info-dir-file-attributes))))
      (progn
	(insert Info-dir-contents)
	(goto-char (point-min)))
    (let ((dirs (if Info-additional-directory-list
                              (append Info-directory-list
                                      Info-additional-directory-list)
                            Info-directory-list))
	  ;; Bind this in case the user sets it to nil.
	  (case-fold-search t)
	  ;; This is set non-nil if we find a problem in some input files.
	  problems
	  buffers buffer others nodes dirs-done)

      (setq Info-dir-file-attributes nil)

      ;; Search the directory list for the directory file.
      (while dirs
	(let ((truename (file-truename (expand-file-name (car dirs)))))
	  (or (member truename dirs-done)
	      (member (directory-file-name truename) dirs-done)
	      ;; Try several variants of specified name.
	      ;; Try upcasing, appending `.info', or both.
	      (let* (file
		     (attrs
		      (or
		       (progn (setq file (expand-file-name "dir" truename))
			      (file-attributes file))
		       (progn (setq file (expand-file-name "DIR" truename))
			      (file-attributes file))
		       (progn (setq file (expand-file-name "dir.info" truename))
			      (file-attributes file))
		       (progn (setq file (expand-file-name "DIR.INFO" truename))
			      (file-attributes file)))))
		(setq dirs-done
		      (cons truename
			    (cons (directory-file-name truename)
				  dirs-done)))
		(if attrs
		    (save-excursion
		      (or buffers
			  (message "Composing main Info directory..."))
		      (set-buffer (generate-new-buffer " info dir"))
		      (condition-case nil
			  (progn
			    (insert-file-contents file)
			    (make-local-variable 'Info-dir-file-name)
			    (setq Info-dir-file-name file)
			    (setq buffers (cons (current-buffer) buffers)
				  Info-dir-file-attributes
				  (cons (cons file attrs)
					Info-dir-file-attributes)))
			(error (kill-buffer (current-buffer))))))))
	  (or (cdr dirs) (setq Info-dir-contents-directory
			       (file-name-as-directory (car dirs))))
	  (setq dirs (cdr dirs))))

      (or buffers
	  (error "Can't find the Info directory node"))
      ;; Distinguish the dir file that comes with Emacs from all the
      ;; others.  Yes, that is really what this is supposed to do.
      ;; The definition of `Info-directory-list' puts it first on that
      ;; list and so last in `buffers' at this point.
      (setq buffer (car (last buffers))
	    others (delq buffer buffers))

      ;; Insert the entire original dir file as a start; note that we've
      ;; already saved its default directory to use as the default
      ;; directory for the whole concatenation.
      (insert-buffer buffer)

      ;; Look at each of the other buffers one by one.
      (while others
	(let ((other (car others))
	      ;; Bind this in case the user sets it to nil.
	      (case-fold-search t)
	      this-buffer-nodes)
	  ;; In each, find all the menus.
	  (save-excursion
	    (set-buffer other)
	    (goto-char (point-min))
	    ;; Find each menu, and add an elt to NODES for it.
	    (while (re-search-forward "^\\* Menu:" nil t)
	      (let (beg nodename end)
		(forward-line 1)
		(while (and (eolp) (not (eobp)))
		  (forward-line 1))
		(setq beg (point))
		(or (search-backward "\n\^_" nil 'move)
		    (looking-at "\^_")
		    (signal 'search-failed (list "\n\^_")))
		(search-forward "Node: ")
		(setq nodename (Info-following-node-name))
		(search-forward "\n\^_" nil 'move)
		(beginning-of-line)
		(setq end (point))
		(setq this-buffer-nodes
		      (cons (list nodename other beg end)
			    this-buffer-nodes))))
	    (if (assoc-ignore-case "top" this-buffer-nodes)
		(setq nodes (nconc this-buffer-nodes nodes))
	      (setq problems t)
	      (message "No `top' node in %s" Info-dir-file-name))))
	(setq others (cdr others)))
      ;; Add to the main menu a menu item for each other node.
      (let ((case-fold-search t)
	    (re-search-forward "^\\* Menu:")))
      (forward-line 1)
      (let ((menu-items '("top"))
	    (nodes nodes)
	    (case-fold-search t)
	    (end (save-excursion (search-forward "\^_" nil t) (point))))
	(while nodes
	  (let ((nodename (car (car nodes))))
	    (save-excursion
	      (or (member (downcase nodename) menu-items)
		  (re-search-forward (concat "^\\* +"
					     (regexp-quote nodename)
					     "::")
				     end t)
		  (progn
		    (insert "* " nodename "::" "\n")
		    (setq menu-items (cons nodename menu-items))))))
	  (setq nodes (cdr nodes))))
      ;; Now take each node of each of the other buffers
      ;; and merge it into the main buffer.
      (while nodes
	(let ((case-fold-search t)
	      (nodename (car (car nodes))))
	  (goto-char (point-min))
	  ;; Find the like-named node in the main buffer.
	  (if (re-search-forward (concat "^\^_.*\n.*Node: "
					 (regexp-quote nodename)
					 "[,\n\t]")
				 nil t)
	      (progn
		(search-forward "\n\^_" nil 'move)
		(beginning-of-line)
		(insert "\n"))
	    ;; If none exists, add one.
	    (goto-char (point-max))
	    (insert "\^_\nFile: dir\tNode: " nodename "\n\n* Menu:\n\n"))
	  ;; Merge the text from the other buffer's menu
	  ;; into the menu in the like-named node in the main buffer.
	  (apply 'insert-buffer-substring (cdr (car nodes))))
	(setq nodes (cdr nodes)))
      ;; Kill all the buffers we just made.
      (while buffers
	(kill-buffer (car buffers))
	(setq buffers (cdr buffers)))
      (goto-char (point-min))
      (if problems
	  (message "Composing main Info directory...problems encountered, see `*Messages*'")
	(message "Composing main Info directory...done")))
    (setq Info-dir-contents (buffer-string)))
  (setq default-directory Info-dir-contents-directory))

;; Note that on entry to this function the current-buffer must be the
;; *info* buffer; not the info tags buffer.
(defun Info-read-subfile (nodepos)
  ;; NODEPOS is either a position (in the Info file as a whole,
  ;; not relative to a subfile) or the name of a subfile.
  (let (lastfilepos
	lastfilename)
    (if (numberp nodepos)
	(save-excursion
	  (set-buffer (marker-buffer Info-tag-table-marker))
	  (goto-char (point-min))
	  (or (looking-at "\^_")
	      (search-forward "\n\^_"))
	  (forward-line 2)
	  (catch 'foo
	    (while (not (looking-at "\^_"))
	      (if (not (eolp))
		  (let ((beg (point))
			thisfilepos thisfilename)
		    (search-forward ": ")
		    (setq thisfilename  (buffer-substring beg (- (point) 2)))
		    (setq thisfilepos (+ (point-min) (read (current-buffer))))
		    ;; read in version 19 stops at the end of number.
		    ;; Advance to the next line.
		    (forward-line 1)
		    (if (> thisfilepos nodepos)
			(throw 'foo t))
		    (setq lastfilename thisfilename)
		    (setq lastfilepos thisfilepos))
		(forward-line 1)))))
      (setq lastfilename nodepos)
      (setq lastfilepos 0))
    ;; Assume previous buffer is in Info-mode.
    ;; (set-buffer (get-buffer "*info*"))
    (or (equal Info-current-subfile lastfilename)
	(let ((buffer-read-only nil))
	  (setq buffer-file-name nil)
	  (widen)
	  (erase-buffer)
	  (info-insert-file-contents lastfilename)
	  (set-buffer-modified-p nil)
	  (setq Info-current-subfile lastfilename)))
    ;; Widen in case we are in the same subfile as before.
    (widen)
    (goto-char (point-min))
    (if (looking-at "\^_")
	(forward-char 1)
      (search-forward "\n\^_"))
    (if (numberp nodepos)
	(+ (- nodepos lastfilepos) (point)))))

(defvar Info-header-line nil
  "If the info node header is hidden, the text of the header.")
(put 'Info-header-line 'risky-local-variable t)

(defun Info-select-node ()
"Select the info node that point is in.
Bind this in case the user sets it to nil."
  (let ((case-fold-search t))
    (save-excursion
      ;; Find beginning of node.
      (if (search-backward "\n\^_" nil 'move)
	  (forward-line 2)
	(if (looking-at "\^_")
	    (forward-line 1)
	  (signal 'search-failed (list "\n\^_"))))
      ;; Get nodename spelled as it is in the node.
      (re-search-forward "Node:[ \t]*")
      (setq Info-current-node
	    (buffer-substring-no-properties (point)
					    (progn
					      (skip-chars-forward "^,\t\n")
					      (point))))
      (Info-set-mode-line)
      ;; Find the end of it, and narrow.
      (beginning-of-line)
      (let (active-expression)
	;; Narrow to the node contents
	(narrow-to-region (point)
			  (if (re-search-forward "\n[\^_\f]" nil t)
			      (prog1
				  (1- (point))
				(if (looking-at "[\n\^_\f]*execute: ")
				    (progn
				      (goto-char (match-end 0))
				      (setq active-expression
					    (read (current-buffer))))))
			    (point-max)))
	(if Info-enable-active-nodes (eval active-expression))
	(Info-fontify-node)
	(if Info-use-header-line
	    (Info-setup-header-line)
	  (setq Info-header-line nil)
	  (setq header-line-format nil)) ; so the header line isn't displayed
	(run-hooks 'Info-selection-hook)))))

(defun Info-set-mode-line ()
  (setq mode-line-buffer-identification
	(nconc (propertized-buffer-identification "%b")
	       (list
		(concat " ("
			(file-name-nondirectory
			 (if (stringp Info-current-file)
			     Info-current-file
			   (or buffer-file-name "")))
			") "
			(or Info-current-node ""))))))

;; Skip the node header and make it into a header-line.  This function
;; should be called when the node is already narrowed.
(defun Info-setup-header-line ()
  (goto-char (point-min))
  (let* ((case-fold-search t)
	 (header-end (save-excursion (forward-line 1) (1- (point))))
	 ;; If we find neither Next: nor Prev: link, show the entire
	 ;; node header.  Otherwise, don't show the File: and Node:
	 ;; parts, to avoid wasting precious space on information that
	 ;; is available in the mode line.
	 (header-beg (if (re-search-forward
			  "\\(next\\|prev[ious]*\\): "
			  header-end t)
			 (match-beginning 1)
		       (point))))
    (set (make-local-variable 'Info-header-line)
	 (buffer-substring header-beg header-end))
    (setq header-line-format 'Info-header-line)
;;; It is useful to be able to copy the links line out of the buffer
;;; with M-w.
;;;    (narrow-to-region (1+ header-end) (point-max))
    ))

;; Go to an info node specified with a filename-and-nodename string
;; of the sort that is found in pointers in nodes.

(defun Info-goto-node (nodename &optional fork)
  "Go to info node named NODENAME.  Give just NODENAME or (FILENAME)NODENAME.
If NODENAME is of the form (FILENAME)NODENAME, the node is in the Info file
FILENAME; otherwise, NODENAME should be in the current Info file (or one of
its sub-files).
Completion is available, but only for node names in the current Info file.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive (list (Info-read-node-name "Go to node: ") current-prefix-arg))
  (info-initialize)
  (if fork
      (set-buffer
       (clone-buffer (concat "*info-" (if (stringp fork) fork nodename) "*") t)))
  (let (filename)
    (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)"
		  nodename)
    (setq filename (if (= (match-beginning 1) (match-end 1))
		       ""
		     (substring nodename (match-beginning 2) (match-end 2)))
	  nodename (substring nodename (match-beginning 3) (match-end 3)))
    (let ((trim (string-match "\\s *\\'" filename)))
      (if trim (setq filename (substring filename 0 trim))))
    (let ((trim (string-match "\\s *\\'" nodename)))
      (if trim (setq nodename (substring nodename 0 trim))))
    (if transient-mark-mode (deactivate-mark))
    (Info-find-node (if (equal filename "") nil filename)
		    (if (equal nodename "") "Top" nodename))))

(defvar Info-read-node-completion-table)

;; This function is used as the "completion table" while reading a node name.
;; It does completion using the alist in Info-read-node-completion-table
;; unless STRING starts with an open-paren.
(defun Info-read-node-name-1 (string predicate code)
  (cond
   ;; First complete embedded file names.
   ((string-match "\\`([^)]*\\'" string)
    (let ((file (substring string 1)))
      (cond
       ((eq code nil)
	(let ((comp (try-completion file 'locate-file-completion
				    (cons Info-directory-list
					  (mapcar 'car Info-suffix-list)))))
	  (cond
	   ((eq comp t) (concat string ")"))
	   (comp (concat "(" comp)))))
       ((eq code t) (all-completions file 'locate-file-completion
				     (cons Info-directory-list
					   (mapcar 'car Info-suffix-list))))
       (t nil))))
   ;; If a file name was given, then any node is fair game.
   ((string-match "\\`(" string)
    (cond
     ((eq code nil) string)
     ((eq code t) nil)
     (t t)))
   ;; Otherwise use Info-read-node-completion-table.
   ((eq code nil)
    (try-completion string Info-read-node-completion-table predicate))
   ((eq code t)
    (all-completions string Info-read-node-completion-table predicate))
   (t
    (test-completion string Info-read-node-completion-table predicate))))

(defun Info-read-node-name (prompt &optional default)
  (let* ((completion-ignore-case t)
	 (Info-read-node-completion-table (Info-build-node-completions))
	 (nodename (completing-read prompt 'Info-read-node-name-1 nil t)))
    (if (equal nodename "")
	(or default
	    (Info-read-node-name prompt))
      nodename)))

(defun Info-build-node-completions ()
  (or Info-current-file-completions
      (let ((compl nil)
	    ;; Bind this in case the user sets it to nil.
	    (case-fold-search t)
	    (node-regexp "Node: *\\([^,\n]*\\) *[,\n\t]"))
	(save-excursion
	  (save-restriction
	    (if (marker-buffer Info-tag-table-marker)
		(let ((marker Info-tag-table-marker))
		  (set-buffer (marker-buffer marker))
		  (widen)
		  (goto-char marker)
		  (while (re-search-forward "\n\\(Node\\|Ref\\): \\(.*\\)\177" nil t)
		    (setq compl
			  (cons (list (match-string-no-properties 2))
				compl))))
	      (widen)
	      (goto-char (point-min))
	      ;; If the buffer begins with a node header, process that first.
	      (if (Info-node-at-bob-matching node-regexp)
		  (setq compl (list (match-string-no-properties 1))))
	      ;; Now for the rest of the nodes.
	      (while (search-forward "\n\^_" nil t)
		(forward-line 1)
		(let ((beg (point)))
		  (forward-line 1)
		  (if (re-search-backward node-regexp beg t)
		      (setq compl
			    (cons (list (match-string-no-properties 1))
				  compl))))))))
	(setq compl (cons '("*") compl))
	(set (make-local-variable 'Info-current-file-completions) compl))))

(defun Info-restore-point (hl)
  "If this node has been visited, restore the point value when we left."
  (while hl
    (if (and (equal (nth 0 (car hl)) Info-current-file)
	     ;; Use string-equal, not equal, to ignore text props.
	     (string-equal (nth 1 (car hl)) Info-current-node))
	(progn
	  (goto-char (nth 2 (car hl)))
	  (setq hl nil))		;terminate the while at next iter
      (setq hl (cdr hl)))))

(defvar Info-search-history nil
  "The history list for `Info-search'.")

(defun Info-search (regexp)
  "Search for REGEXP, starting from point, and select node it's found in."
  (interactive (list (read-string
		      (if Info-search-history
			  (format "Regexp search (default `%s'): "
				  (car Info-search-history))
			"Regexp search: ")
		      nil 'Info-search-history)))
  (when transient-mark-mode
    (deactivate-mark))
  (when (equal regexp "")
    (setq regexp (car Info-search-history)))
  (when regexp
    (let ((found ()) current
	  (onode Info-current-node)
	  (ofile Info-current-file)
	  (opoint (point))
	  (ostart (window-start))
	  (osubfile Info-current-subfile))
      (save-excursion
	(save-restriction
	  (widen)
	  (if (null Info-current-subfile)
	      (progn (re-search-forward regexp) (setq found (point)))
	    (condition-case err
		(progn (re-search-forward regexp) (setq found (point)))
	      (search-failed nil)))))
      (if (not found)			;can only happen in subfile case -- else would have erred
	  (unwind-protect
	      (let ((list ()))
		(save-excursion
		  (set-buffer (marker-buffer Info-tag-table-marker))
		  (goto-char (point-min))
		  (search-forward "\n\^_\nIndirect:")
		  (save-restriction
		    (narrow-to-region (point)
				      (progn (search-forward "\n\^_")
					     (1- (point))))
		    (goto-char (point-min))
		    (search-forward (concat "\n" osubfile ": "))
		    (beginning-of-line)
		    (while (not (eobp))
		      (re-search-forward "\\(^.*\\): [0-9]+$")
		      (goto-char (+ (match-end 1) 2))
		      (setq list (cons (cons (read (current-buffer))
					     (match-string-no-properties 1))
				       list))
		      (goto-char (1+ (match-end 0))))
		    (setq list (nreverse list)
			  current (car (car list))
			  list (cdr list))))
		(while list
		  (message "Searching subfile %s..." (cdr (car list)))
		  (Info-read-subfile (car (car list)))
		  (setq list (cdr list))
		  (if (re-search-forward regexp nil t)
		      (setq found (point) list ())))
		(if found
		    (message "")
		  (signal 'search-failed (list regexp))))
	    (if (not found)
		(progn (Info-read-subfile osubfile)
		       (goto-char opoint)
		       (Info-select-node)
		       (set-window-start (selected-window) ostart)))))
      (widen)
      (goto-char found)
      (Info-select-node)
      ;; Use string-equal, not equal, to ignore text props.
      (or (and (string-equal onode Info-current-node)
	       (equal ofile Info-current-file))
	  (setq Info-history (cons (list ofile onode opoint)
				   Info-history))))))

(defun Info-extract-pointer (name &optional errorname)
  "Extract the value of the node-pointer named NAME.
If there is none, use ERRORNAME in the error message;
if ERRORNAME is nil, just return nil.
Bind this in case the user sets it to nil."
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(when Info-header-line
	  ;; expose the header line in the buffer
	  (widen)
	  (forward-line -1))
	(let ((bound (point)))
	  (forward-line 1)
	  (cond ((re-search-backward (concat name ":") bound t)
		 (goto-char (match-end 0))
		 (Info-following-node-name))
		((not (eq errorname t))
		 (error "Node has no %s"
			(capitalize (or errorname name))))))))))

(defun Info-following-node-name (&optional allowedchars)
  "Return the node name in the buffer following point.
ALLOWEDCHARS, if non-nil, goes within [...] to make a regexp
saying which chars may appear in the node name."
  (skip-chars-forward " \t")
  (buffer-substring-no-properties
   (point)
   (progn
     (while (looking-at (concat "[" (or allowedchars "^,\t\n") "]"))
       (skip-chars-forward (concat (or allowedchars "^,\t\n") "("))
       (if (looking-at "(")
	   (skip-chars-forward "^)")))
     (skip-chars-backward " ")
     (point))))

(defun Info-next ()
  "Go to the next node of this node."
  (interactive)
  (Info-goto-node (Info-extract-pointer "next")))

(defun Info-prev ()
  "Go to the previous node of this node."
  (interactive)
  (Info-goto-node (Info-extract-pointer "prev[ious]*" "previous")))

(defun Info-up (&optional same-file)
  "Go to the superior node of this node.
If SAME-FILE is non-nil, do not move to a different Info file."
  (interactive)
  (let ((node (Info-extract-pointer "up")))
    (and (or same-file (not (stringp Info-current-file)))
	 (string-match "^(" node)
	 (error "Up node is in another Info file"))
    (Info-goto-node node))
  (Info-restore-point Info-history))

(defun Info-last ()
  "Go back to the last node visited."
  (interactive)
  (or Info-history
      (error "This is the first Info node you looked at"))
  (let (filename nodename opoint)
    (setq filename (car (car Info-history)))
    (setq nodename (car (cdr (car Info-history))))
    (setq opoint (car (cdr (cdr (car Info-history)))))
    (setq Info-history (cdr Info-history))
    (Info-find-node filename nodename)
    (setq Info-history (cdr Info-history))
    (goto-char opoint)))

;;;###autoload
(defun Info-directory ()
  "Go to the Info directory node."
  (interactive)
  (Info-find-node "dir" "top"))

(defun Info-follow-reference (footnotename)
  "Follow cross reference named FOOTNOTENAME to the node it refers to.
FOOTNOTENAME may be an abbreviation of the reference name."
  (interactive
   (let ((completion-ignore-case t)
	 (case-fold-search t)
	 completions default alt-default (start-point (point)) str i bol eol)
     (save-excursion
       ;; Store end and beginning of line.
       (end-of-line)
       (setq eol (point))
       (beginning-of-line)
       (setq bol (point))

       (goto-char (point-min))
       (while (re-search-forward "\\*note[ \n\t]*\\([^:]*\\):" nil t)
	 (setq str (buffer-substring-no-properties
		    (match-beginning 1)
		    (1- (point))))
	 ;; See if this one should be the default.
	 (and (null default)
	      (<= (match-beginning 0) start-point)
	      (<= start-point (point))
	      (setq default t))
	 ;; See if this one should be the alternate default.
	 (and (null alt-default)
	      (and (<= bol (match-beginning 0))
		   (<= (point) eol))
	      (setq alt-default t))
	 (setq i 0)
	 (while (setq i (string-match "[ \n\t]+" str i))
	   (setq str (concat (substring str 0 i) " "
			     (substring str (match-end 0))))
	   (setq i (1+ i)))
	 ;; Record as a completion and perhaps as default.
	 (if (eq default t) (setq default str))
	 (if (eq alt-default t) (setq alt-default str))
	 ;; Don't add this string if it's a duplicate.
	 ;; We use a loop instead of "(assoc str completions)" because
	 ;; we want to do a case-insensitive compare.
	 (let ((tail completions)
	       (tem (downcase str)))
	   (while (and tail
		       (not (string-equal tem (downcase (car (car tail))))))
	     (setq tail (cdr tail)))
	   (or tail
	       (setq completions
		     (cons (cons str nil)
			   completions))))))
     ;; If no good default was found, try an alternate.
     (or default
	 (setq default alt-default))
     ;; If only one cross-reference found, then make it default.
     (if (eq (length completions) 1)
         (setq default (car (car completions))))
     (if completions
	 (let ((input (completing-read (if default
					   (concat
					    "Follow reference named: (default "
					    default ") ")
					 "Follow reference named: ")
				       completions nil t)))
	   (list (if (equal input "")
		     default input)))
       (error "No cross-references in this node"))))

  (unless footnotename
    (error "No reference was specified"))

  (let (target beg i (str (concat "\\*note " (regexp-quote footnotename)))
	       (case-fold-search t))
    (while (setq i (string-match " " str i))
      (setq str (concat (substring str 0 i) "[ \t\n]+" (substring str (1+ i))))
      (setq i (+ i 6)))
    (save-excursion
      (goto-char (point-min))
      (or (re-search-forward str nil t)
	  (error "No cross-reference named %s" footnotename))
      (goto-char (+ (match-beginning 0) 5))
      (setq target
	    (Info-extract-menu-node-name "Bad format cross reference" t)))
    (while (setq i (string-match "[ \t\n]+" target i))
      (setq target (concat (substring target 0 i) " "
			   (substring target (match-end 0))))
      (setq i (+ i 1)))
    (Info-goto-node target)))

(defun Info-extract-menu-node-name (&optional errmessage multi-line)
  (skip-chars-forward " \t\n")
  (let ((beg (point))
	str i)
    (skip-chars-forward "^:")
    (forward-char 1)
    (setq str
	  (if (looking-at ":")
	      (buffer-substring-no-properties beg (1- (point)))
	    (skip-chars-forward " \t\n")
	    (Info-following-node-name (if multi-line "^.,\t" "^.,\t\n"))))
    (replace-regexp-in-string "[ \n]+" " " str)))

;; No one calls this.
;;(defun Info-menu-item-sequence (list)
;;  (while list
;;    (Info-menu (car list))
;;    (setq list (cdr list))))

(defvar Info-complete-menu-buffer)
(defvar Info-complete-next-re nil)
(defvar Info-complete-cache nil)

(defun Info-complete-menu-item (string predicate action)
  ;; This uses two dynamically bound variables:
  ;; - `Info-complete-menu-buffer' which contains the buffer in which
  ;; is the menu of items we're trying to complete.
  ;; - `Info-complete-next-re' which, if non-nil, indicates that we should
  ;; also look for menu items in subsequent nodes as long as those
  ;; nodes' names match `Info-complete-next-re'.  This feature is currently
  ;; only used for completion in Info-index.
  (save-excursion
    (set-buffer Info-complete-menu-buffer)
    (let ((completion-ignore-case t)
	  (case-fold-search t)
	  (orignode Info-current-node)
	  nextnode)
      (goto-char (point-min))
      (search-forward "\n* Menu:")
      (if (not (memq action '(nil t)))
	  (re-search-forward
	   (concat "\n\\* +" (regexp-quote string) ":") nil t)
	(let ((pattern (concat "\n\\* +\\("
			       (regexp-quote string)
			       "[^:\t\n]*\\):"))
	      completions)
	  ;; Check the cache.
	  (if (and (equal (nth 0 Info-complete-cache) Info-current-file)
		   (equal (nth 1 Info-complete-cache) Info-current-node)
		   (equal (nth 2 Info-complete-cache) Info-complete-next-re)
		   (let ((prev (nth 3 Info-complete-cache)))
		     (eq t (compare-strings string 0 (length prev)
					    prev 0 nil t))))
	      ;; We can reuse the previous list.
	      (setq completions (nth 4 Info-complete-cache))
	    ;; The cache can't be used.
	    (while
		(progn
		  (while (re-search-forward pattern nil t)
		    (push (match-string-no-properties 1)
			  completions))
		  ;; Check subsequent nodes if applicable.
		  (and Info-complete-next-re
		       (setq nextnode (Info-extract-pointer "next" t))
		       (string-match Info-complete-next-re nextnode)))
	      (Info-goto-node nextnode))
	    ;; Go back to the start node (for the next completion).
	    (unless (equal Info-current-node orignode)
	      (Info-goto-node orignode))
	    ;; Update the cache.
	    (setq Info-complete-cache
		  (list Info-current-file Info-current-node
			Info-complete-next-re string completions)))
	  (if action
	      (all-completions string completions predicate)
	    (try-completion string completions predicate)))))))


(defun Info-menu (menu-item &optional fork)
  "Go to the node pointed to by the menu item named (or abbreviated) MENU-ITEM.
The menu item should one of those listed in the current node's menu.
Completion is allowed, and the default menu item is the one point is on.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new info buffer.  If FORK is a string, it is the name to use for the
new buffer."
  (interactive
   (let ((completions '())
	 ;; If point is within a menu item, use that item as the default
	 (default nil)
	 (p (point))
	 beg
	 (last nil)
	 (case-fold-search t))
     (save-excursion
       (goto-char (point-min))
       (if (not (search-forward "\n* menu:" nil t))
	   (error "No menu in this node"))
       (setq beg (point))
       (and (< (point) p)
	    (save-excursion
	      (goto-char p)
	      (end-of-line)
	      (if (re-search-backward "\n\\* +\\([^:\t\n]*\\):" beg t)
		  (setq default (match-string-no-properties 1))))))
     (let ((item nil))
       (while (null item)
	 (setq item (let ((completion-ignore-case t)
			  (Info-complete-menu-buffer (current-buffer)))
		      (completing-read (if default
					   (format "Menu item (default %s): "
						   default)
					 "Menu item: ")
				       'Info-complete-menu-item nil t)))
	 ;; we rely on the fact that completing-read accepts an input
	 ;; of "" even when the require-match argument is true and ""
	 ;; is not a valid possibility
	 (if (string= item "")
	     (if default
		 (setq item default)
	       ;; ask again
	       (setq item nil))))
       (list item current-prefix-arg))))
  ;; there is a problem here in that if several menu items have the same
  ;; name you can only go to the node of the first with this command.
  (Info-goto-node (Info-extract-menu-item menu-item) (if fork menu-item)))

(defun Info-extract-menu-item (menu-item)
  (setq menu-item (regexp-quote menu-item))
  (let ((case-fold-search t))
    (save-excursion
      (let ((case-fold-search t))
	(goto-char (point-min))
	(or (search-forward "\n* menu:" nil t)
	    (error "No menu in this node"))
	(or (re-search-forward (concat "\n\\* +" menu-item ":") nil t)
	    (re-search-forward (concat "\n\\* +" menu-item) nil t)
	    (error "No such item in menu"))
	(beginning-of-line)
	(forward-char 2)
	(Info-extract-menu-node-name)))))

;; If COUNT is nil, use the last item in the menu.
(defun Info-extract-menu-counting (count)
  (let ((case-fold-search t))
    (save-excursion
      (let ((case-fold-search t))
	(goto-char (point-min))
	(or (search-forward "\n* menu:" nil t)
	    (error "No menu in this node"))
	(if count
	    (or (search-forward "\n* " nil t count)
		(error "Too few items in menu"))
	  (while (search-forward "\n* " nil t)
	    nil))
	(Info-extract-menu-node-name)))))

(defun Info-nth-menu-item ()
  "Go to the node of the Nth menu item.
N is the digit argument used to invoke this command."
  (interactive)
  (Info-goto-node
   (Info-extract-menu-counting
    (- (aref (this-command-keys) (1- (length (this-command-keys)))) ?0))))

(defun Info-top-node ()
  "Go to the Top node of this file."
  (interactive)
  (Info-goto-node "Top"))

(defun Info-final-node ()
  "Go to the final node in this file."
  (interactive)
  (Info-goto-node "Top")
  (let ((Info-history nil)
	(case-fold-search t))
    ;; Go to the last node in the menu of Top.
    (Info-goto-node (Info-extract-menu-counting nil))
    ;; If the last node in the menu is not last in pointer structure,
    ;; move forward until we can't go any farther.
    (while (Info-forward-node t t) nil)
    ;; Then keep moving down to last subnode, unless we reach an index.
    (while (and (not (string-match "\\<index\\>" Info-current-node))
		(save-excursion (search-forward "\n* Menu:" nil t)))
      (Info-goto-node (Info-extract-menu-counting nil)))))

(defun Info-forward-node (&optional not-down no-error)
  "Go forward one node, considering all nodes as forming one sequence."
  (interactive)
  (goto-char (point-min))
  (forward-line 1)
  (let ((case-fold-search t))
    ;; three possibilities, in order of priority:
    ;;     1. next node is in a menu in this node (but not in an index)
    ;;     2. next node is next at same level
    ;;     3. next node is up and next
    (cond ((and (not not-down)
		(save-excursion (search-forward "\n* menu:" nil t))
		(not (string-match "\\<index\\>" Info-current-node)))
	   (Info-goto-node (Info-extract-menu-counting 1))
	   t)
	  ((save-excursion
	     (save-restriction
	       (let (limit)
		 (when Info-header-line
		   (goto-char (point-min))
		   (widen)
		   (forward-line -1)
		   (setq limit (point))
		   (forward-line 1))
		 (search-backward "next:" limit t))))
	   (Info-next)
	   t)
	  ((and (save-excursion
		  (save-restriction
		    (let (limit)
		      (when Info-header-line
			(goto-char (point-min))
			(widen)
			(forward-line -1)
			(setq limit (point))
			(forward-line 1))
		      (search-backward "up:" limit t))))
		;; Use string-equal, not equal, to ignore text props.
		(not (string-equal (downcase (Info-extract-pointer "up"))
				   "top")))
	   (let ((old-node Info-current-node))
	     (Info-up)
	     (let (Info-history success)
	       (unwind-protect
		   (setq success (Info-forward-node t no-error))
		 (or success (Info-goto-node old-node))))))
	  (no-error nil)
	  (t (error "No pointer forward from this node")))))

(defun Info-backward-node ()
  "Go backward one node, considering all nodes as forming one sequence."
  (interactive)
  (let ((prevnode (Info-extract-pointer "prev[ious]*" t))
	(upnode (Info-extract-pointer "up" t))
	(case-fold-search t))
    (cond ((and upnode (string-match "(" upnode))
	   (error "First node in file"))
	  ((and upnode (or (null prevnode)
			   ;; Use string-equal, not equal,
			   ;; to ignore text properties.
			   (string-equal (downcase prevnode)
					 (downcase upnode))))
	   (Info-up))
	  (prevnode
	   ;; If we move back at the same level,
	   ;; go down to find the last subnode*.
	   (Info-prev)
	   (let (Info-history)
	     (while (and (not (string-match "\\<index\\>" Info-current-node))
			 (save-excursion (search-forward "\n* Menu:" nil t)))
	       (Info-goto-node (Info-extract-menu-counting nil)))))
	  (t
	   (error "No pointer backward from this node")))))

(defun Info-exit ()
  "Exit Info by selecting some other buffer."
  (interactive)
  (if Info-standalone
      (save-buffers-kill-emacs)
    (quit-window)))

(defun Info-next-menu-item ()
  (interactive)
  ;; Bind this in case the user sets it to nil.
  (let* ((case-fold-search t)
	 (node
	  (save-excursion
	    (forward-line -1)
	    (search-forward "\n* menu:" nil t)
	    (and (search-forward "\n* " nil t)
		 (Info-extract-menu-node-name)))))
    (if node (Info-goto-node node)
      (error "No more items in menu"))))

(defun Info-last-menu-item ()
  (interactive)
  (save-excursion
    (forward-line 1)
    ;; Bind this in case the user sets it to nil.
    (let* ((case-fold-search t)
	   (beg (save-excursion
		  (and (search-backward "\n* menu:" nil t)
		       (point)))))
      (or (and beg (search-backward "\n* " beg t))
	  (error "No previous items in menu")))
    (Info-goto-node (save-excursion
		      (goto-char (match-end 0))
		      (Info-extract-menu-node-name)))))

(defmacro Info-no-error (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(t))) '(error nil)))

(defun Info-next-preorder ()
  "Go to the next subnode or the next node, or go up a level."
  (interactive)
  (cond ((Info-no-error (Info-next-menu-item)))
	((Info-no-error (Info-next)))
	((Info-no-error (Info-up t))
	 ;; Since we have already gone thru all the items in this menu,
	 ;; go up to the end of this node.
	 (goto-char (point-max))
	 ;; Since logically we are done with the node with that menu,
	 ;; move on from it.
	 (Info-next-preorder))
	(t
	 (error "No more nodes"))))

(defun Info-last-preorder ()
  "Go to the last node, popping up a level if there is none."
  (interactive)
  (cond ((Info-no-error
	  (Info-last-menu-item)
	  ;; If we go down a menu item, go to the end of the node
	  ;; so we can scroll back through it.
	  (goto-char (point-max)))
	 ;; Keep going down, as long as there are nested menu nodes.
	 (while (Info-no-error
		 (Info-last-menu-item)
		 ;; If we go down a menu item, go to the end of the node
		 ;; so we can scroll back through it.
		 (goto-char (point-max))))
	 (recenter -1))
	((and (Info-no-error (Info-extract-pointer "prev"))
	      (not (equal (Info-extract-pointer "up")
			  (Info-extract-pointer "prev"))))
	 (Info-no-error (Info-prev))
	 (goto-char (point-max))
	 (while (Info-no-error
		 (Info-last-menu-item)
		 ;; If we go down a menu item, go to the end of the node
		 ;; so we can scroll back through it.
		 (goto-char (point-max))))
	 (recenter -1))
	((Info-no-error (Info-up t))
	 (goto-char (point-min))
	 (let ((case-fold-search t))
	   (or (search-forward "\n* Menu:" nil t)
	       (goto-char (point-max)))))
	(t (error "No previous nodes"))))

(defun Info-scroll-up ()
  "Scroll one screenful forward in Info, considering all nodes as one sequence.
Once you scroll far enough in a node that its menu appears on the screen
but after point, the next scroll moves into its first subnode, unless
`Info-scroll-prefer-subnodes' is nil.

When you scroll past the end of a node, that goes to the next node if
`Info-scroll-prefer-subnodes' is non-nil and to the first subnode otherwise;
if this node has no successor, it moves to the parent node's successor,
and so on.  If `Info-scroll-prefer-subnodes' is non-nil and point is inside
the menu of a node, it moves to subnode indicated by the following menu
item.  (That case won't normally result from this command, but can happen
in other ways.)"

  (interactive)
  (if (or (< (window-start) (point-min))
	  (> (window-start) (point-max)))
      (set-window-start (selected-window) (point)))
  (let* ((case-fold-search t)
	 (virtual-end (save-excursion
			(goto-char (point-min))
			(if (and Info-scroll-prefer-subnodes
				 (search-forward "\n* Menu:" nil t))
			    (point)
			  (point-max)))))
    (if (or (< virtual-end (window-start))
	    (pos-visible-in-window-p virtual-end))
	(cond
	 (Info-scroll-prefer-subnodes (Info-next-preorder))
	 ((Info-no-error (Info-goto-node (Info-extract-menu-counting 1))))
	 (t (Info-next-preorder)))
      (scroll-up))))

(defun Info-scroll-down ()
  "Scroll one screenful back in Info, considering all nodes as one sequence.
If point is within the menu of a node, and `Info-scroll-prefer-subnodes'
is non-nil, this goes to its last subnode.  When you scroll past the
beginning of a node, that goes to the previous node or back up to the
parent node."
  (interactive)
  (if (or (< (window-start) (point-min))
	  (> (window-start) (point-max)))
      (set-window-start (selected-window) (point)))
  (let* ((case-fold-search t)
	 (current-point (point))
	 (virtual-end
	  (and Info-scroll-prefer-subnodes
	       (save-excursion
		 (beginning-of-line)
		 (setq current-point (point))
		 (goto-char (point-min))
		 (search-forward "\n* Menu:"
				 current-point
				 t)))))
    (if (or virtual-end 
	    (pos-visible-in-window-p (point-min) nil t))
	(Info-last-preorder)
      (scroll-down))))

(defun Info-next-reference (&optional recur)
  "Move cursor to the next cross-reference or menu item in the node."
  (interactive)
  (let ((pat "\\*note[ \n\t]*\\([^:]*\\):\\|^\\* .*:")
	(old-pt (point))
	(case-fold-search t))
    (or (eobp) (forward-char 1))
    (or (re-search-forward pat nil t)
	(progn
	  (goto-char (point-min))
	  (or (re-search-forward pat nil t)
	      (progn
		(goto-char old-pt)
		(error "No cross references in this node")))))
    (goto-char (match-beginning 0))
    (if (looking-at "\\* Menu:")
	(if recur
	    (error "No cross references in this node")
	  (Info-next-reference t)))))

(defun Info-prev-reference (&optional recur)
  "Move cursor to the previous cross-reference or menu item in the node."
  (interactive)
  (let ((pat "\\*note[ \n\t]*\\([^:]*\\):\\|^\\* .*:")
	(old-pt (point))
	(case-fold-search t))
    (or (re-search-backward pat nil t)
	(progn
	  (goto-char (point-max))
	  (or (re-search-backward pat nil t)
	      (progn
		(goto-char old-pt)
		(error "No cross references in this node")))))
    (goto-char (match-beginning 0))
    (if (looking-at "\\* Menu:")
	(if recur
	    (error "No cross references in this node")
	  (Info-prev-reference t)))))

(defun Info-goto-index ()
  (Info-goto-node "Top")
  (or (search-forward "\n* menu:" nil t)
      (error "No index"))
  (or (re-search-forward "\n\\* \\(.*\\<Index\\>\\)" nil t)
      (error "No index"))
  (goto-char (match-beginning 1))
  ;; Protect Info-history so that the current node (Top) is not added to it.
  (let ((Info-history nil))
    (Info-goto-node (Info-extract-menu-node-name))))

(defun Info-index (topic)
  "Look up a string TOPIC in the index for this file.
The index is defined as the first node in the top level menu whose
name contains the word \"Index\", plus any immediately following
nodes whose names also contain the word \"Index\".
If there are no exact matches to the specified topic, this chooses
the first match which is a case-insensitive substring of a topic.
Use the `,' command to see the other matches.
Give a blank topic name to go to the Index node itself."
  (interactive
   (list
    (let ((Info-complete-menu-buffer (clone-buffer))
	  (Info-complete-next-re "\\<Index\\>"))
      (unwind-protect
	  (with-current-buffer Info-complete-menu-buffer
	    (Info-goto-index)
	    (completing-read "Index topic: " 'Info-complete-menu-item))
	(kill-buffer Info-complete-menu-buffer)))))
  (let ((orignode Info-current-node)
	(rnode nil)
	(pattern (format "\n\\* +\\([^\n:]*%s[^\n:]*\\):[ \t]*\\([^.\n]*\\)\\.[ \t]*\\([0-9]*\\)"
			 (regexp-quote topic)))
	node
	(case-fold-search t))
    (Info-goto-index)
    (or (equal topic "")
	(let ((matches nil)
	      (exact nil)
	      ;; We bind Info-history to nil for internal node-switches so
	      ;; that we don't put junk in the history.  In the first
	      ;; Info-goto-index call, above, we do update the history
	      ;; because that is what the user's previous node choice into it.
	      (Info-history nil)
	      found)
	  (while
	      (progn
		(goto-char (point-min))
		(while (re-search-forward pattern nil t)
		  (push (list (match-string-no-properties 1)
			      (match-string-no-properties 2)
			      Info-current-node
			      (string-to-int (concat "0"
						     (match-string 3))))
			matches))
		(and (setq node (Info-extract-pointer "next" t))
		     (string-match "\\<Index\\>" node)))
	    (Info-goto-node node))
	  (or matches
	      (progn
		(Info-goto-node orignode)
		(error "No `%s' in index" topic)))
	  ;; Here it is a feature that assoc is case-sensitive.
	  (while (setq found (assoc topic matches))
	    (setq exact (cons found exact)
		  matches (delq found matches)))
	  (setq Info-index-alternatives (nconc exact (nreverse matches)))
	  (Info-index-next 0)))))

(defun Info-index-next (num)
  "Go to the next matching index item from the last `i' command."
  (interactive "p")
  (or Info-index-alternatives
      (error "No previous `i' command"))
  (while (< num 0)
    (setq num (+ num (length Info-index-alternatives))))
  (while (> num 0)
    (setq Info-index-alternatives
	  (nconc (cdr Info-index-alternatives)
		 (list (car Info-index-alternatives)))
	  num (1- num)))
  (Info-goto-node (nth 1 (car Info-index-alternatives)))
  (if (> (nth 3 (car Info-index-alternatives)) 0)
      (forward-line (nth 3 (car Info-index-alternatives)))
    (forward-line 3)			; don't search in headers
    (let ((name (car (car Info-index-alternatives))))
      (Info-find-index-name name)))
  (message "Found `%s' in %s.  %s"
	   (car (car Info-index-alternatives))
	   (nth 2 (car Info-index-alternatives))
	   (if (cdr Info-index-alternatives)
	       "(Press `,' for more)"
	     "(Only match)")))

(defun Info-find-index-name (name)
  "Move point to the place within the current node where NAME is defined."
  (let ((case-fold-search t))
    (if (or (re-search-forward (format
				"[a-zA-Z]+: %s\\( \\|$\\)"
				(regexp-quote name)) nil t)
	    (search-forward (format "`%s'" name) nil t)
	    (and (string-match "\\`.*\\( (.*)\\)\\'" name)
		 (search-forward
		  (format "`%s'" (substring name 0 (match-beginning 1)))
		  nil t))
	    (search-forward name nil t))
	(beginning-of-line)
      (goto-char (point-min)))))

(defun Info-undefined ()
  "Make command be undefined in Info."
  (interactive)
  (ding))

(defun Info-help ()
  "Enter the Info tutorial."
  (interactive)
  (delete-other-windows)
  (Info-find-node "info"
		  (if (< (window-height) 23)
		      "Help-Small-Screen"
		    "Help")))

(defun Info-summary ()
  "Display a brief summary of all Info commands."
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*Help*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (documentation 'Info-mode))
    (help-mode)
    (goto-char (point-min))
    (let (ch flag)
      (while (progn (setq flag (not (pos-visible-in-window-p (point-max))))
		    (message (if flag "Type Space to see more"
			       "Type Space to return to Info"))
		    (if (not (eq ?\  (setq ch (read-event))))
			(progn (setq unread-command-events (list ch)) nil)
		      flag))
	(scroll-up)))
    (bury-buffer "*Help*")))

(defun Info-get-token (pos start all &optional errorstring)
  "Return the token around POS.
POS must be somewhere inside the token
START is a regular expression which will match the
    beginning of the tokens delimited string
ALL is a regular expression with a single
    parenthesized subpattern which is the token to be
    returned.  E.g. '{\(.*\)}' would return any string
    enclosed in braces around POS.
ERRORSTRING optional fourth argument, controls action on no match
    nil: return nil
    t: beep
    a string: signal an error, using that string."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char pos)
      ;; First look for a match for START that goes across POS.
      (while (and (not (bobp)) (> (point) (- pos (length start)))
		  (not (looking-at start)))
	(forward-char -1))
      ;; If we did not find one, search back for START
      ;; (this finds only matches that end at or before POS).
      (or (looking-at start)
	  (progn
	    (goto-char pos)
	    (re-search-backward start (max (point-min) (- pos 200)) 'yes)))
      (let (found)
	(while (and (re-search-forward all (min (point-max) (+ pos 200)) 'yes)
		    (not (setq found (and (<= (match-beginning 0) pos)
					  (> (match-end 0) pos))))))
	(if (and found (<= (match-beginning 0) pos)
		 (> (match-end 0) pos))
	    (match-string-no-properties 1)
	  (cond ((null errorstring)
		 nil)
		((eq errorstring t)
		 (beep)
		 nil)
		(t
		 (error "No %s around position %d" errorstring pos))))))))

(defun Info-mouse-follow-nearest-node (click)
  "\\<Info-mode-map>Follow a node reference near point.
Like \\[Info-menu], \\[Info-follow-reference], \\[Info-next], \\[Info-prev] or \\[Info-up] command, depending on where you click.
At end of the node's text, moves to the next node, or up if none."
  (interactive "e")
  (mouse-set-point click)
  (and (not (Info-try-follow-nearest-node))
       (save-excursion (forward-line 1) (eobp))
       (Info-next-preorder)))

(defun Info-follow-nearest-node ()
  "\\<Info-mode-map>Follow a node reference near point.
Like \\[Info-menu], \\[Info-follow-reference], \\[Info-next], \\[Info-prev] or \\[Info-up] command, depending on where point is.
If no reference to follow, moves to the next node, or up if none."
  (interactive)
  (or (Info-try-follow-nearest-node)
      (Info-next-preorder)))

;; Common subroutine.
(defun Info-try-follow-nearest-node ()
  "Follow a node reference near point.  Return non-nil if successful."
  (let (node)
    (cond
     ((setq node (Info-get-token (point) "\\*note[ \n]"
				 "\\*note[ \n]\\([^:]*\\):"))
      (Info-follow-reference node))
     ((setq node (Info-get-token (point) "\\* +" "\\* +\\([^:]*\\)::"))
      (Info-goto-node node))
     ((Info-get-token (point) "\\* +" "\\* +\\([^:]*\\):")
      (beginning-of-line)
      (forward-char 2)
      (setq node (Info-extract-menu-node-name))
      (Info-goto-node node))
     ((setq node (Info-get-token (point) "Up: " "Up: \\([^,\n\t]*\\)"))
      (Info-goto-node node))
     ((setq node (Info-get-token (point) "Next: " "Next: \\([^,\n\t]*\\)"))
      (Info-goto-node node))
     ((setq node (Info-get-token (point) "File: " "File: \\([^,\n\t]*\\)"))
      (Info-goto-node "Top"))
     ((setq node (Info-get-token (point) "Prev: " "Prev: \\([^,\n\t]*\\)"))
      (Info-goto-node node)))
    node))

(defvar Info-mode-map nil
  "Keymap containing Info commands.")
(if Info-mode-map
    nil
  (setq Info-mode-map (make-keymap))
  (suppress-keymap Info-mode-map)
  (define-key Info-mode-map "." 'beginning-of-buffer)
  (define-key Info-mode-map " " 'Info-scroll-up)
  (define-key Info-mode-map "\C-m" 'Info-follow-nearest-node)
  (define-key Info-mode-map "\t" 'Info-next-reference)
  (define-key Info-mode-map "\e\t" 'Info-prev-reference)
  (define-key Info-mode-map "1" 'Info-nth-menu-item)
  (define-key Info-mode-map "2" 'Info-nth-menu-item)
  (define-key Info-mode-map "3" 'Info-nth-menu-item)
  (define-key Info-mode-map "4" 'Info-nth-menu-item)
  (define-key Info-mode-map "5" 'Info-nth-menu-item)
  (define-key Info-mode-map "6" 'Info-nth-menu-item)
  (define-key Info-mode-map "7" 'Info-nth-menu-item)
  (define-key Info-mode-map "8" 'Info-nth-menu-item)
  (define-key Info-mode-map "9" 'Info-nth-menu-item)
  (define-key Info-mode-map "0" 'undefined)
  (define-key Info-mode-map "?" 'Info-summary)
  (define-key Info-mode-map "]" 'Info-forward-node)
  (define-key Info-mode-map "[" 'Info-backward-node)
  (define-key Info-mode-map "<" 'Info-top-node)
  (define-key Info-mode-map ">" 'Info-final-node)
  (define-key Info-mode-map "b" 'beginning-of-buffer)
  (define-key Info-mode-map "c" 'Info-copy-current-node-name)
  (define-key Info-mode-map "d" 'Info-directory)
  (define-key Info-mode-map "e" 'Info-edit)
  (define-key Info-mode-map "f" 'Info-follow-reference)
  (define-key Info-mode-map "g" 'Info-goto-node)
  (define-key Info-mode-map "h" 'Info-help)
  (define-key Info-mode-map "i" 'Info-index)
  (define-key Info-mode-map "l" 'Info-last)
  (define-key Info-mode-map "m" 'Info-menu)
  (define-key Info-mode-map "n" 'Info-next)
  (define-key Info-mode-map "p" 'Info-prev)
  (define-key Info-mode-map "q" 'Info-exit)
  (define-key Info-mode-map "s" 'Info-search)
  ;; For consistency with Rmail.
  (define-key Info-mode-map "\M-s" 'Info-search)
  (define-key Info-mode-map "\M-n" 'clone-buffer)
  (define-key Info-mode-map "t" 'Info-top-node)
  (define-key Info-mode-map "u" 'Info-up)
  (define-key Info-mode-map "," 'Info-index-next)
  (define-key Info-mode-map "\177" 'Info-scroll-down)
  (define-key Info-mode-map [mouse-2] 'Info-mouse-follow-nearest-node)
  )

(defun Info-check-pointer (item)
  "Non-nil if ITEM is present in this node."
  (condition-case nil
      (Info-extract-pointer item)
    (error nil)))

(easy-menu-define
 Info-mode-menu Info-mode-map
 "Menu for info files."
 '("Info"
   ["Up" Info-up :active (Info-check-pointer "up")
    :help "Go up in the Info tree"]
   ["Next" Info-next :active (Info-check-pointer "next")
    :help "Go to the next node"]
   ["Previous" Info-prev :active (Info-check-pointer "prev[ious]*")
    :help "Go to the previous node"]
   ["Backward" Info-backward-node
    :help "Go backward one node, considering all as a sequence"]
   ["Forward" Info-forward-node
    :help "Go forward one node, considering all as a sequence"]
   ["Beginning" beginning-of-buffer
    :help "Go to beginning of this node"]
   ["Top" Info-top-node
    :help "Go to top node of file"]
   ["Final Node" Info-final-node
    :help "Go to final node in this file"]
   ("Menu Item" ["You should never see this" report-emacs-bug t])
   ("Reference" ["You should never see this" report-emacs-bug t])
   ["Search..." Info-search
    :help "Search for regular expression in this Info file"]
   ["Go to Node..." Info-goto-node
    :help "Go to a named node"]
   ["Last" Info-last :active Info-history
    :help "Go to the last node you were at"]
   ("Index..."
    ["Lookup a String" Info-index
     :help "Look for a string in the index items"]
    ["Next Matching Item" Info-index-next
     :help "Look for another occurrence of previous item"])
   ["Edit" Info-edit :help "Edit contents of this node"
    :active Info-enable-edit]
   ["Copy Node Name" Info-copy-current-node-name
    :help "Copy the name of the current node into the kill ring"]
   ["Exit" Info-exit :help "Stop reading Info"]))


(defvar info-tool-bar-map
  (if (display-graphic-p)
      (let ((map (make-sparse-keymap)))
	(tool-bar-local-item-from-menu 'Info-exit "close" map Info-mode-map)
	(tool-bar-local-item-from-menu 'Info-prev "left_arrow" map Info-mode-map)
	(tool-bar-local-item-from-menu 'Info-next "right_arrow" map Info-mode-map)
	(tool-bar-local-item-from-menu 'Info-up "up_arrow" map Info-mode-map)
	(tool-bar-local-item-from-menu 'Info-last "undo" map Info-mode-map)
	(tool-bar-local-item-from-menu 'Info-top-node "home" map Info-mode-map)
	(tool-bar-local-item-from-menu 'Info-index "index" map Info-mode-map)
	(tool-bar-local-item-from-menu 'Info-goto-node "jump_to" map Info-mode-map)
	(tool-bar-local-item-from-menu 'Info-search "search" map Info-mode-map)
	map)))

(defvar Info-menu-last-node nil)
;; Last node the menu was created for.
;; Value is a list, (FILE-NAME NODE-NAME).

(defun Info-menu-update ()
  "Update the Info menu for the current node."
  (condition-case nil
      (if (or (not (eq major-mode 'Info-mode))
	      (equal (list Info-current-file Info-current-node)
		     Info-menu-last-node))
	  ()
	;; Update menu menu.
	(let* ((Info-complete-menu-buffer (current-buffer))
	       (items (nreverse (condition-case nil
				    (Info-complete-menu-item "" nil t)
				  (error nil))))
	       entries current
	       (number 0))
	  (while (and items (< number 9))
	    (setq current (car items)
		  items (cdr items)
		  number (1+ number))
	    (setq entries (cons `[,current
				  (Info-menu ,current)
				  :keys ,(format "%d" number)]
				entries)))
	  (if items
	      (setq entries (cons ["Other..." Info-menu t] entries)))
	  (or entries
	      (setq entries (list ["No menu" nil nil] nil :active)))
	  (easy-menu-change '("Info") "Menu Item" (nreverse entries)))
	;; Update reference menu.  Code stolen from `Info-follow-reference'.
	(let ((items nil)
	      str i entries current
	      (number 0)
	      (case-fold-search t))
	  (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "\\*note[ \n\t]*\\([^:]*\\):" nil t)
	      (setq str (buffer-substring
			 (match-beginning 1)
			 (1- (point))))
	      (setq i 0)
	      (while (setq i (string-match "[ \n\t]+" str i))
		(setq str (concat (substring str 0 i) " "
				  (substring str (match-end 0))))
		(setq i (1+ i)))
	      (setq items
		    (cons str items))))
	  (while (and items (< number 9))
	    (setq current (car items)
		  items (cdr items)
		  number (1+ number))
	    (setq entries (cons `[,current
				  (Info-follow-reference ,current)
				  t]
				entries)))
	  (if items
	      (setq entries (cons ["Other..." Info-follow-reference t]
				  entries)))
	  (or entries
	      (setq entries (list ["No references" nil nil] nil :active)))
	  (easy-menu-change '("Info") "Reference" (nreverse entries)))
	;; Update last seen node.
	(setq Info-menu-last-node (list Info-current-file Info-current-node)))
    ;; Try to avoid entering infinite beep mode in case of errors.
    (error (ding))))


(defun Info-copy-current-node-name ()
  "Put the name of the current info node into the kill ring.
The name of the info file is prepended to the node name in parentheses."
  (interactive)
  (unless Info-current-node
    (error "No current info node"))
  (kill-new
   (concat "("
	   (file-name-nondirectory
	    (if (stringp Info-current-file)
		Info-current-file
	      (or buffer-file-name "")))
	   ")"
	   Info-current-node)))


;; Info mode is suitable only for specially formatted data.
(put 'Info-mode 'mode-class 'special)
(put 'Info-mode 'no-clone-indirect t)

(defun Info-mode ()
  "Info mode provides commands for browsing through the Info documentation tree.
Documentation in Info is divided into \"nodes\", each of which discusses
one topic and contains references to other nodes which discuss related
topics.  Info has commands to follow the references and show you other nodes.

\\<Info-mode-map>\
\\[Info-help]	Invoke the Info tutorial.
\\[Info-exit]	Quit Info: reselect previously selected buffer.

Selecting other nodes:
\\[Info-mouse-follow-nearest-node]
	Follow a node reference you click on.
	  This works with menu items, cross references, and
	  the \"next\", \"previous\" and \"up\", depending on where you click.
\\[Info-follow-nearest-node]	Follow a node reference near point, like \\[Info-mouse-follow-nearest-node].
\\[Info-next]	Move to the \"next\" node of this node.
\\[Info-prev]	Move to the \"previous\" node of this node.
\\[Info-up]	Move \"up\" from this node.
\\[Info-menu]	Pick menu item specified by name (or abbreviation).
	  Picking a menu item causes another node to be selected.
\\[Info-directory]	Go to the Info directory node.
\\[Info-follow-reference]	Follow a cross reference.  Reads name of reference.
\\[Info-last]	Move to the last node you were at.
\\[Info-index]	Look up a topic in this file's Index and move to that node.
\\[Info-index-next]	(comma) Move to the next match from a previous `i' command.
\\[Info-top-node]	Go to the Top node of this file.
\\[Info-final-node]	Go to the final node in this file.
\\[Info-backward-node]	Go backward one node, considering all nodes as forming one sequence.
\\[Info-forward-node]	Go forward one node, considering all nodes as forming one sequence.

Moving within a node:
\\[Info-scroll-up]	Normally, scroll forward a full screen.
	  Once you scroll far enough in a node that its menu appears on the
	  screen but after point, the next scroll moves into its first
	  subnode.  When after all menu items (or if there is no menu),
	  move up to the parent node.
\\[Info-scroll-down]	Normally, scroll backward.  If the beginning of the buffer is
	  already visible, try to go to the previous menu entry, or up
	  if there is none.
\\[beginning-of-buffer]	Go to beginning of node.

Advanced commands:
\\[Info-exit]	Quit Info: reselect previously selected buffer.
\\[Info-edit]	Edit contents of selected node.
1	Pick first item in node's menu.
2, 3, 4, 5   Pick second ... fifth item in node's menu.
\\[Info-goto-node]	Move to node specified by name.
	  You may include a filename as well, as (FILENAME)NODENAME.
\\[universal-argument] \\[info]	Move to new Info file with completion.
\\[Info-search]	Search through this Info file for specified regexp,
	  and select the node in which the next occurrence is found.
\\[Info-next-reference]	Move cursor to next cross-reference or menu item.
\\[Info-prev-reference]	Move cursor to previous cross-reference or menu item."
  (kill-all-local-variables)
  (setq major-mode 'Info-mode)
  (setq mode-name "Info")
  (setq tab-width 8)
  (use-local-map Info-mode-map)
  (add-hook 'activate-menubar-hook 'Info-menu-update nil t)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq case-fold-search t)
  (setq buffer-read-only t)
  (make-local-variable 'Info-current-file)
  (make-local-variable 'Info-current-subfile)
  (make-local-variable 'Info-current-node)
  (make-local-variable 'Info-tag-table-marker)
  (setq Info-tag-table-marker (make-marker))
  (make-local-variable 'Info-tag-table-buffer)
  (setq Info-tag-table-buffer nil)
  (set (make-local-variable 'font-lock-category-alist)
       '((info-menu-header . info-menu-header)
	 (info-header-node . info-header-node)
	 (info-header-xref . info-header-xref)
	 (Info-title-1-face . Info-title-1-face)
	 (Info-title-2-face . Info-title-2-face)
	 (Info-title-3-face . Info-title-3-face)
	 (Info-title-4-face . Info-title-4-face)
	 (info-menu-5 . info-menu-5)
	 (info-xref . info-xref)))
  (make-local-variable 'Info-history)
  (make-local-variable 'Info-index-alternatives)
  (set (make-local-variable 'tool-bar-map) info-tool-bar-map)
  ;; This is for the sake of the invisible text we use handling titles.
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (add-hook 'clone-buffer-hook 'Info-clone-buffer-hook nil t)
  (Info-set-mode-line)
  (run-hooks 'Info-mode-hook))

(defun Info-clone-buffer-hook ()
  (when (bufferp Info-tag-table-buffer)
    (setq Info-tag-table-buffer
	  (with-current-buffer Info-tag-table-buffer (clone-buffer))))
  (let ((m Info-tag-table-marker))
    (when (markerp m)
      (setq Info-tag-table-marker
	    (if (and (marker-position m) (bufferp Info-tag-table-buffer))
		(with-current-buffer Info-tag-table-buffer
		  (copy-marker (marker-position m)))
	      (make-marker))))))

(defvar Info-edit-map (let ((map (make-sparse-keymap)))
			(set-keymap-parent map text-mode-map)
			(define-key map "\C-c\C-c" 'Info-cease-edit)
			map)
  "Local keymap used within `e' command of Info.")

;; Info-edit mode is suitable only for specially formatted data.
(put 'Info-edit-mode 'mode-class 'special)

(defun Info-edit-mode ()
  "Major mode for editing the contents of an Info node.
Like text mode with the addition of `Info-cease-edit'
which returns to Info mode for browsing.
\\{Info-edit-map}"
  (use-local-map Info-edit-map)
  (setq major-mode 'Info-edit-mode)
  (setq mode-name "Info Edit")
  (kill-local-variable 'mode-line-buffer-identification)
  (setq buffer-read-only nil)
  (force-mode-line-update)
  (buffer-enable-undo (current-buffer))
  (run-hooks 'Info-edit-mode-hook))

(defun Info-edit ()
  "Edit the contents of this Info node.
Allowed only if variable `Info-enable-edit' is non-nil."
  (interactive)
  (or Info-enable-edit
      (error "Editing info nodes is not enabled"))
  (Info-edit-mode)
  (message "%s" (substitute-command-keys
		 "Editing: Type \\<Info-edit-map>\\[Info-cease-edit] to return to info")))

(defun Info-cease-edit ()
  "Finish editing Info node; switch back to Info proper."
  (interactive)
  ;; Do this first, so nothing has changed if user C-g's at query.
  (and (buffer-modified-p)
       (y-or-n-p "Save the file? ")
       (save-buffer))
  (use-local-map Info-mode-map)
  (setq major-mode 'Info-mode)
  (setq mode-name "Info")
  (Info-set-mode-line)
  (setq buffer-read-only t)
  (force-mode-line-update)
  (and (marker-position Info-tag-table-marker)
       (buffer-modified-p)
       (message "Tags may have changed.  Use Info-tagify if necessary")))

(defvar Info-file-list-for-emacs
  '("ediff" "eudc" "forms" "gnus" "info" ("mh" . "mh-e")
    "sc" "message" ("dired" . "dired-x") "viper" "vip" "idlwave"
    ("c" . "ccmode") ("c++" . "ccmode") ("objc" . "ccmode")
    ("java" . "ccmode") ("idl" . "ccmode") ("pike" . "ccmode")
    ("skeleton" . "autotype") ("auto-insert" . "autotype")
    ("copyright" . "autotype") ("executable" . "autotype")
    ("time-stamp" . "autotype") ("quickurl" . "autotype")
    ("tempo" . "autotype") ("hippie-expand" . "autotype")
    ("cvs" . "pcl-cvs") ("ada" . "ada-mode") "calc"
    ("calcAlg" . "calc") ("calcDigit" . "calc") ("calcVar" . "calc")
    "ebrowse" "eshell" "cl" "reftex" "speedbar" "widget" "woman"
    ("mail-header" . "emacs-mime") ("mail-content" . "emacs-mime")
    ("mail-encode" . "emacs-mime") ("mail-decode" . "emacs-mime")
    ("rfc2045" . "emacs-mime")
    ("rfc2231" . "emacs-mime")  ("rfc2047" . "emacs-mime")
    ("rfc2045" . "emacs-mime") ("rfc1843" . "emacs-mime")
    ("ietf-drums" . "emacs-mime")  ("quoted-printable" . "emacs-mime")
    ("binhex" . "emacs-mime") ("uudecode" . "emacs-mime")
    ("mailcap" . "emacs-mime") ("mm" . "emacs-mime")
    ("mml" . "emacs-mime"))
  "List of Info files that describe Emacs commands.
An element can be a file name, or a list of the form (PREFIX . FILE)
where PREFIX is a name prefix and FILE is the file to look in.
If the element is just a file name, the file name also serves as the prefix.")

(defun Info-find-emacs-command-nodes (command)
  "Return a list of locations documenting COMMAND.
The `info-file' property of COMMAND says which Info manual to search.
If COMMAND has no property, the variable `Info-file-list-for-emacs'
defines heuristics for which Info manual to try.
The locations are of the format used in `Info-history', i.e.
\(FILENAME NODENAME BUFFERPOS\)."
  (let ((where '())
	(cmd-desc (concat "^\\* +" (regexp-quote (symbol-name command))
			  "\\( <[0-9]+>\\)?:\\s *\\(.*\\)\\.$"))
	(info-file "emacs"))		;default
    ;; Determine which info file this command is documented in.
    (if (get command 'info-file)
	(setq info-file (get command 'info-file))
      ;; If it doesn't say explicitly, test its name against
      ;; various prefixes that we know.
      (let ((file-list Info-file-list-for-emacs))
	(while file-list
	  (let* ((elt (car file-list))
		 (name (if (consp elt)
			   (car elt)
			 elt))
		 (file (if (consp elt) (cdr elt) elt))
		 (regexp (concat "\\`" (regexp-quote name)
				 "\\(\\'\\|-\\)")))
	    (if (string-match regexp (symbol-name command))
		(setq info-file file file-list nil))
	    (setq file-list (cdr file-list))))))
    (Info-find-node info-file "Top")
    (or (and (search-forward "\n* menu:" nil t)
	     (re-search-forward "\n\\* \\(.*\\<Index\\>\\)" nil t))
	(error "Info file `%s' appears to lack an index" info-file))
    (goto-char (match-beginning 1))
    ;; Bind Info-history to nil, to prevent the index nodes from
    ;; getting into the node history.
    (let ((Info-history nil)
	  (exact nil)
	  node found)
      (Info-goto-node (Info-extract-menu-node-name))
      (while
	  (progn
	    (goto-char (point-min))
	    (while (re-search-forward cmd-desc nil t)
	      (setq where
		    (cons (list Info-current-file
				(match-string-no-properties 2)
				0)
			  where)))
	    (and (setq node (Info-extract-pointer "next" t))
		 (string-match "\\<Index\\>" node)))
	(Info-goto-node node)))
    where))

;;;###autoload
(defun Info-goto-emacs-command-node (command)
  "Go to the Info node in the Emacs manual for command COMMAND.
The command is found by looking up in Emacs manual's indices
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'."
  (interactive "CFind documentation for command: ")
  (or (commandp command)
      (signal 'wrong-type-argument (list 'commandp command)))
  (let ((where (Info-find-emacs-command-nodes command)))
    (if where
	(let ((num-matches (length where)))
	  ;; Get Info running, and pop to it in another window.
	  (save-window-excursion
	    (info))
	  ;; FIXME It would be cool if this could use a buffer other
	  ;; than *info*.
	  (pop-to-buffer "*info*")
	  ;; Bind Info-history to nil, to prevent the last Index node
	  ;; visited by Info-find-emacs-command-nodes from being
	  ;; pushed onto the history.
	  (let ((Info-history nil))
	    (Info-find-node (car (car where))
			    (car (cdr (car where)))))
	  (if (> num-matches 1)
	      (progn
		;; (car where) will be pushed onto Info-history
		;; when/if they go to another node.  Put the other
		;; nodes that were found on the history.
		(setq Info-history (nconc (cdr where) Info-history))
		(message "Found %d other entr%s.  Use %s to see %s."
			 (1- num-matches)
			 (if (> num-matches 2) "ies" "y")
			 (substitute-command-keys "\\[Info-last]")
			 (if (> num-matches 2) "them" "it")))))
      (error "Couldn't find documentation for %s" command))))

;;;###autoload
(defun Info-goto-emacs-key-command-node (key)
  "Go to the node in the Emacs manual which describes the command bound to KEY.
KEY is a string.
Interactively, if the binding is `execute-extended-command', a command is read.
The command is found by looking up in Emacs manual's indices
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'."
  (interactive "kFind documentation for key: ")
  (let ((command (key-binding key)))
    (cond ((null command)
	   (message "%s is undefined" (key-description key)))
	  ((and (interactive-p)
		(eq command 'execute-extended-command))
	   (Info-goto-emacs-command-node
	    (read-command "Find documentation for command: ")))
	  (t
	   (Info-goto-emacs-command-node command)))))

(defface Info-title-1-face
  '((((type tty pc) (class color)) (:foreground "yellow" :weight bold))
    (t (:height 1.2 :inherit Info-title-2-face)))
  "Face for Info titles at level 1."
  :group 'info)

(defface Info-title-2-face
  '((((type tty pc) (class color)) (:foreground "lightblue" :weight bold))
    (t (:height 1.2 :inherit Info-title-3-face)))
  "Face for Info titles at level 2."
  :group 'info)

(defface Info-title-3-face
  '((((type tty pc) (class color)) (:weight bold))
    (t (:height 1.2 :inherit Info-title-4-face)))
  "Face for Info titles at level 3."
  :group 'info)

(defface Info-title-4-face
  '((((type tty pc) (class color)) (:weight bold))
    (t (:weight bold :inherit variable-pitch)))
  "Face for Info titles at level 4."
  :group 'info)

(defface info-menu-header
  '((((type tty pc))
     :underline t
     :weight bold)
    (t
     :inherit variable-pitch
     :weight bold))
  "Face for headers in Info menus."
  :group 'info)

(defun Info-fontify-menu-headers ()
  "Add the face `info-menu-header' to any header before a menu entry."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\\* Menu:" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
			 'category 'info-menu-header)
      (while (re-search-forward "\n\n\\([^*\n ].*\\)\n\n?[*]" nil t)
	(put-text-property (match-beginning 1) (match-end 1)
			   'category 'info-menu-header)))))

(defun Info-fontify-node ()
  ;; Only fontify the node if it hasn't already been done.  [We pass in
  ;; LIMIT arg to `next-property-change' because it seems to search past
  ;; (point-max).]
  (unless (and (< (next-property-change (point-min) nil (point-max))
		  (point-max))
	       ;; But do put the text properties if the local-map property
	       ;; is inconsistent with Info-use-header-line's value.
	       (eq
		(= (next-single-property-change
		    (point-min) 'local-map nil (point-max))
		   (point-max))
		(null Info-use-header-line)))
    (save-excursion
      (let ((buffer-read-only nil)
	    (case-fold-search t))
	(goto-char (point-min))
	(when (looking-at "^File: [^,: \t]+,?[ \t]+")
	  (goto-char (match-end 0))
	  (while (looking-at "[ \t]*\\([^:, \t\n]+\\):[ \t]+\\([^:,\t\n]+\\),?")
	    (goto-char (match-end 0))
	    (let* ((nbeg (match-beginning 2))
		   (nend (match-end 2))
		   (tbeg (match-beginning 1))
		   (tag (buffer-substring tbeg (match-end 1))))
	      (if (string-equal tag "Node")
		  (put-text-property nbeg nend 'category 'info-header-node)
		(put-text-property nbeg nend 'category 'info-header-xref)
		(put-text-property tbeg nend 'mouse-face 'highlight)
		(put-text-property tbeg nend
				   'help-echo
				   (concat "Go to node "
					   (buffer-substring nbeg nend)))
		;; Don't bind mouse events on the header line if we
		;; aren't going to display the header line.
		(when Info-use-header-line
		  (let ((fun (cdr (assoc tag '(("Prev" . Info-prev)
					       ("Next" . Info-next)
					       ("Up" . Info-up))))))
		    (when fun
		      (let ((keymap (make-sparse-keymap)))
			(define-key keymap [header-line mouse-1] fun)
			(define-key keymap [header-line mouse-2] fun)
			(put-text-property tbeg nend 'local-map keymap)))))
		(if (not Info-use-header-line)
		    ;; In case they switched Info-use-header-line off
		    ;; in the middle of an Info session, some text
		    ;; properties may have been left lying around from
		    ;; past visits of this node.  Remove them.
		    (remove-text-properties tbeg nend '(local-map nil)))
		  ))))
	(goto-char (point-min))
	(while (re-search-forward "\n\\([^ \t\n].+\\)\n\\(\\*+\\|=+\\|-+\\|\\.+\\)$"
				  nil t)
	  (let* ((c (preceding-char))
		 (category
		  (cond ((= c ?*) 'Info-title-1-face)
			((= c ?=) 'Info-title-2-face)
			((= c ?-) 'Info-title-3-face)
			(t        'Info-title-4-face))))
	    (put-text-property (match-beginning 1) (match-end 1)
			       'category category))
	  ;; This is a serious problem for trying to handle multiple
	  ;; frame types at once.  We want this text to be invisible
	  ;; on frames that can display the font above.
	  (when (memq (framep (selected-frame)) '(x pc w32 mac))
	    (add-text-properties (match-beginning 2) (1+ (match-end 2))
				 '(invisible t intangible t))))
	(goto-char (point-min))
	(while (re-search-forward "\\*Note[ \n\t]+\\([^:]*\\):" nil t)
	  (if (= (char-after (1- (match-beginning 0))) ?\") ; hack
	      nil
	    (add-text-properties (match-beginning 1) (match-end 1)
				 '(category info-xref
				   mouse-face highlight
				   help-echo "mouse-2: go to this node"))))
	(goto-char (point-min))
	(if (and (search-forward "\n* Menu:" nil t)
		 (not (string-match "\\<Index\\>" Info-current-node))
		 ;; Don't take time to annotate huge menus
		 (< (- (point-max) (point)) Info-fontify-maximum-menu-size))
	    (let ((n 0))
	      (while (re-search-forward "^\\* +\\([^:\t\n]*\\):" nil t)
		(setq n (1+ n))
		(if (zerop (% n 3)) ; visual aids to help with 1-9 keys
		    (put-text-property (match-beginning 0)
				       (1+ (match-beginning 0))
				       'category 'info-menu-5))
		(add-text-properties (match-beginning 1) (match-end 1)
				     '(category info-xref
				       mouse-face highlight
				       help-echo "mouse-2: go to this node")))))
	(Info-fontify-menu-headers)
	(set-buffer-modified-p nil)))))


;; When an Info buffer is killed, make sure the associated tags buffer
;; is killed too.
(defun Info-kill-buffer ()
  (and (eq major-mode 'Info-mode)
       Info-tag-table-buffer
       (kill-buffer Info-tag-table-buffer)))

(add-hook 'kill-buffer-hook 'Info-kill-buffer)

;;; Speedbar support:
;; These functions permit speedbar to display the "tags" in the
;; current info node.
(eval-when-compile (require 'speedbar))

(defvar Info-speedbar-key-map nil
  "Keymap used when in the info display mode.")

(defun Info-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance Info."
  (if Info-speedbar-key-map
      nil
    (setq Info-speedbar-key-map (speedbar-make-specialized-keymap))

    ;; Basic tree features
    (define-key Info-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key Info-speedbar-key-map "\C-m" 'speedbar-edit-line)
    (define-key Info-speedbar-key-map "+" 'speedbar-expand-line)
    (define-key Info-speedbar-key-map "-" 'speedbar-contract-line)
    )

  (speedbar-add-expansion-list '("Info" Info-speedbar-menu-items
				 Info-speedbar-key-map
				 Info-speedbar-hierarchy-buttons)))

(defvar Info-speedbar-menu-items
  '(["Browse Node" speedbar-edit-line t]
    ["Expand Node" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract Node" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    )
  "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (Info-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'Info-install-speedbar-variables))

;;; Info hierarchy display method
;;;###autoload
(defun Info-speedbar-browser ()
  "Initialize speedbar to display an info node browser.
This will add a speedbar major display mode."
  (interactive)
  (require 'speedbar)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Now, throw us into Info mode on speedbar.
  (speedbar-change-initial-expansion-list "Info")
  )

(eval-when-compile (defvar speedbar-attached-frame))

(defun Info-speedbar-hierarchy-buttons (directory depth &optional node)
  "Display an Info directory hierarchy in speedbar.
DIRECTORY is the current directory in the attached frame.
DEPTH is the current indentation depth.
NODE is an optional argument that is used to represent the
specific node to expand."
  (if (and (not node)
	   (save-excursion (goto-char (point-min))
			   (let ((case-fold-search t))
			     (looking-at "Info Nodes:"))))
      ;; Update our "current node" maybe?
      nil
    ;; We cannot use the generic list code, that depends on all leaves
    ;; being known at creation time.
    (if (not node)
	(speedbar-with-writable (insert "Info Nodes:\n")))
    (let ((completions nil)
	  (cf (selected-frame)))
      (select-frame speedbar-attached-frame)
      (save-window-excursion
	(setq completions
	      (Info-speedbar-fetch-file-nodes (or node '"(dir)top"))))
      (select-frame cf)
      (if completions
	  (speedbar-with-writable
	   (while completions
	     (speedbar-make-tag-line 'bracket ?+ 'Info-speedbar-expand-node
				     (cdr (car completions))
				     (car (car completions))
				     'Info-speedbar-goto-node
				     (cdr (car completions))
				     'info-xref depth)
	     (setq completions (cdr completions)))
	   t)
	nil))))

(defun Info-speedbar-goto-node (text node indent)
  "When user clicks on TEXT, go to an info NODE.
The INDENT level is ignored."
  (select-frame speedbar-attached-frame)
  (let* ((buff (or (get-buffer "*info*")
		   (progn (info) (get-buffer "*info*"))))
	 (bwin (get-buffer-window buff 0)))
    (if bwin
	(progn
	  (select-window bwin)
	  (raise-frame (window-frame bwin)))
      (if speedbar-power-click
	  (let ((pop-up-frames t)) (select-window (display-buffer buff)))
	(select-frame speedbar-attached-frame)
	(switch-to-buffer buff)))
    (let ((junk (string-match "^(\\([^)]+\\))\\([^.]+\\)$" node))
	  (file (match-string 1 node))
	  (node (match-string 2 node)))
      (Info-find-node file node)
      ;; If we do a find-node, and we were in info mode, restore
      ;; the old default method.  Once we are in info mode, it makes
      ;; sense to return to whatever method the user was using before.
      (if (string= speedbar-initial-expansion-list-name "Info")
	  (speedbar-change-initial-expansion-list
	   speedbar-previously-used-expansion-list-name)))))

(defun Info-speedbar-expand-node (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is data related to this node (NAME . FILE).
INDENT is the current indentation depth."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (if (speedbar-with-writable
	      (save-excursion
		(end-of-line) (forward-char 1)
		(Info-speedbar-hierarchy-buttons nil (1+ indent) token)))
	     (speedbar-change-expand-button-char ?-)
	   (speedbar-change-expand-button-char ??)))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun Info-speedbar-fetch-file-nodes (nodespec)
  "Fetch the subnodes from the info NODESPEC.
NODESPEC is a string of the form: (file)node.
Optional THISFILE represends the filename of"
  (save-excursion
    ;; Set up a buffer we can use to fake-out Info.
    (set-buffer (get-buffer-create "*info-browse-tmp*"))
    (if (not (equal major-mode 'Info-mode))
	(Info-mode))
    ;; Get the node into this buffer
    (let ((junk (string-match "^(\\([^)]+\\))\\([^.]+\\)$" nodespec))
	  (file (match-string 1 nodespec))
	  (node (match-string 2 nodespec)))
      (Info-find-node file node))
    ;; Scan the created buffer
    (goto-char (point-min))
    (let ((completions nil)
	  (case-fold-search t)
	  (thisfile (progn (string-match "^(\\([^)]+\\))" nodespec)
			   (match-string 1 nodespec))))
      ;; Always skip the first one...
      (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
      (while (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
	(let ((name (match-string 1)))
	  (if (looking-at " *\\(([^)]+)[^.\n]+\\)\\.")
	      (setq name (cons name (match-string 1)))
	    (if (looking-at " *\\(([^)]+)\\)\\.")
		(setq name (cons name (concat (match-string 1) "Top")))
	      (if (looking-at " \\([^.]+\\).")
		  (setq name
			(cons name (concat "(" thisfile ")" (match-string 1))))
		(setq name (cons name (concat "(" thisfile ")" name))))))
	  (setq completions (cons name completions))))
      (nreverse completions))))

;;; Info mode node listing
(defun Info-speedbar-buttons (buffer)
  "Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for."
  (if (save-excursion (goto-char (point-min))
		      (let ((case-fold-search t))
			(not (looking-at "Info Nodes:"))))
      (erase-buffer))
  (Info-speedbar-hierarchy-buttons nil 0)
  )

(dolist (mess '("^Node has no Previous$"
		"^No menu in this node$"
		"^Node has no Next$"
                "^No cross-references in this node^"
                search-failed
		"^No \".*\" in index$"))
  (add-to-list 'debug-ignored-errors mess))

(provide 'info)

;;; info.el ends here
