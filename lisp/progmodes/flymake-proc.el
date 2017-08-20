;;; flymake-proc.el --- Flymake for external syntax checker processes  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2017 Free Software Foundation, Inc.

;; Author:  Pavel Kobyakov <pk_at_work@yahoo.com>
;; Maintainer: Leo Liu <sdl.web@gmail.com>
;; Version: 0.3
;; Keywords: c languages tools

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Flymake is a minor Emacs mode performing on-the-fly syntax checks.
;;
;; This file contains the most original implementation of flymake's
;; main source of on-the-fly diagnostic info, the external syntax
;; checker backend.
;;
;;; Bugs/todo:

;; - Only uses "Makefile", not "makefile" or "GNUmakefile"
;;   (from http://bugs.debian.org/337339).

;;; Code:

(require 'flymake)

(defcustom flymake-compilation-prevents-syntax-check t
  "If non-nil, don't start syntax check if compilation is running."
  :group 'flymake
  :type 'boolean)

(defcustom flymake-xml-program
  (if (executable-find "xmlstarlet") "xmlstarlet" "xml")
  "Program to use for XML validation."
  :type 'file
  :group 'flymake
  :version "24.4")

(defcustom flymake-master-file-dirs '("." "./src" "./UnitTest")
  "Dirs where to look for master files."
  :group 'flymake
  :type '(repeat (string)))

(defcustom flymake-master-file-count-limit 32
  "Max number of master files to check."
  :group 'flymake
  :type 'integer)

(defcustom flymake-allowed-file-name-masks
  '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init)
    ("\\.xml\\'" flymake-xml-init)
    ("\\.html?\\'" flymake-xml-init)
    ("\\.cs\\'" flymake-simple-make-init)
    ("\\.p[ml]\\'" flymake-perl-init)
    ("\\.php[345]?\\'" flymake-php-init)
    ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
    ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
    ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
    ("\\.tex\\'" flymake-simple-tex-init)
    ("\\.idl\\'" flymake-simple-make-init)
    ;; ("\\.cpp\\'" 1)
    ;; ("\\.java\\'" 3)
    ;; ("\\.h\\'" 2 ("\\.cpp\\'" "\\.c\\'")
    ;; ("[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*[/\\]*\\)\\(%s\\)\"" 1 2))
    ;; ("\\.idl\\'" 1)
    ;; ("\\.odl\\'" 1)
    ;; ("[0-9]+\\.tex\\'" 2 ("\\.tex\\'")
    ;; ("[ \t]*\\input[ \t]*{\\(.*\\)\\(%s\\)}" 1 2 ))
    ;; ("\\.tex\\'" 1)
    )
  "Files syntax checking is allowed for.
This is an alist with elements of the form:
  REGEXP INIT [CLEANUP [NAME]]
REGEXP is a regular expression that matches a file name.
INIT is the init function to use.
CLEANUP is the cleanup function to use, default `flymake-simple-cleanup'.
NAME is the file name function to use, default `flymake-get-real-file-name'."
  :group 'flymake
  :type '(alist :key-type (regexp :tag "File regexp")
                :value-type
                (list :tag "Handler functions"
                      (function :tag "Init function")
                      (choice :tag "Cleanup function"
                              (const :tag "flymake-simple-cleanup" nil)
                              function)
                      (choice :tag "Name function"
                              (const :tag "flymake-get-real-file-name" nil)
                              function))))

(defvar flymake-processes nil
  "List of currently active flymake processes.")

(defvar-local flymake-output-residual nil)

(defun flymake-get-file-name-mode-and-masks (file-name)
  "Return the corresponding entry from `flymake-allowed-file-name-masks'."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (let ((fnm flymake-allowed-file-name-masks)
	(mode-and-masks nil))
    (while (and (not mode-and-masks) fnm)
      (if (string-match (car (car fnm)) file-name)
	  (setq mode-and-masks (cdr (car fnm))))
      (setq fnm (cdr fnm)))
    (flymake-log 3 "file %s, init=%s" file-name (car mode-and-masks))
    mode-and-masks))

(defun flymake-can-syntax-check-file (file-name)
  "Determine whether we can syntax check FILE-NAME.
Return nil if we cannot, non-nil if we can."
  (if (flymake-get-init-function file-name) t nil))

(defun flymake-get-init-function (file-name)
  "Return init function to be used for the file."
  (let* ((init-f  (nth 0 (flymake-get-file-name-mode-and-masks file-name))))
    ;;(flymake-log 0 "calling %s" init-f)
    ;;(funcall init-f (current-buffer))
    init-f))

(defun flymake-get-cleanup-function (file-name)
  "Return cleanup function to be used for the file."
  (or (nth 1 (flymake-get-file-name-mode-and-masks file-name))
      'flymake-simple-cleanup))

(defun flymake-get-real-file-name-function (file-name)
  (or (nth 2 (flymake-get-file-name-mode-and-masks file-name))
      'flymake-get-real-file-name))

(defvar flymake-find-buildfile-cache (make-hash-table :test #'equal))

(defun flymake-get-buildfile-from-cache (dir-name)
  "Look up DIR-NAME in cache and return its associated value.
If DIR-NAME is not found, return nil."
  (gethash dir-name flymake-find-buildfile-cache))

(defun flymake-add-buildfile-to-cache (dir-name buildfile)
  "Associate DIR-NAME with BUILDFILE in the buildfile cache."
  (puthash dir-name buildfile flymake-find-buildfile-cache))

(defun flymake-clear-buildfile-cache ()
  "Clear the buildfile cache."
  (clrhash flymake-find-buildfile-cache))

(defun flymake-find-buildfile (buildfile-name source-dir-name)
  "Find buildfile starting from current directory.
Buildfile includes Makefile, build.xml etc.
Return its file name if found, or nil if not found."
  (or (flymake-get-buildfile-from-cache source-dir-name)
      (let* ((file (locate-dominating-file source-dir-name buildfile-name)))
        (if file
            (progn
              (flymake-log 3 "found buildfile at %s" file)
              (flymake-add-buildfile-to-cache source-dir-name file)
              file)
          (progn
            (flymake-log 3 "buildfile for %s not found" source-dir-name)
            nil)))))

(defun flymake-fix-file-name (name)
  "Replace all occurrences of `\\' with `/'."
  (when name
    (setq name (expand-file-name name))
    (setq name (abbreviate-file-name name))
    (setq name (directory-file-name name))
    name))

(defun flymake-same-files (file-name-one file-name-two)
  "Check if FILE-NAME-ONE and FILE-NAME-TWO point to same file.
Return t if so, nil if not."
  (equal (flymake-fix-file-name file-name-one)
	 (flymake-fix-file-name file-name-two)))

;; This is bound dynamically to pass a parameter to a sort predicate below
(defvar flymake-included-file-name)

(defun flymake-find-possible-master-files (file-name master-file-dirs masks)
  "Find (by name and location) all possible master files.

Name is specified by FILE-NAME and location is specified by
MASTER-FILE-DIRS.  Master files include .cpp and .c for .h.
Files are searched for starting from the .h directory and max
max-level parent dirs.  File contents are not checked."
  (let* ((dirs master-file-dirs)
	 (files  nil)
	 (done   nil))

    (while (and (not done) dirs)
      (let* ((dir (expand-file-name (car dirs) (file-name-directory file-name)))
	     (masks masks))
	(while (and (file-exists-p dir) (not done) masks)
	  (let* ((mask        (car masks))
		 (dir-files   (directory-files dir t mask)))

	    (flymake-log 3 "dir %s, %d file(s) for mask %s"
			 dir (length dir-files) mask)
	    (while (and (not done) dir-files)
	      (when (not (file-directory-p (car dir-files)))
		(setq files (cons (car dir-files) files))
		(when (>= (length files) flymake-master-file-count-limit)
		  (flymake-log 3 "master file count limit (%d) reached" flymake-master-file-count-limit)
		  (setq done t)))
	      (setq dir-files (cdr dir-files))))
	  (setq masks (cdr masks))))
      (setq dirs (cdr dirs)))
    (when files
      (let ((flymake-included-file-name (file-name-nondirectory file-name)))
	(setq files (sort files 'flymake-master-file-compare))))
    (flymake-log 3 "found %d possible master file(s)" (length files))
    files))

(defun flymake-master-file-compare (file-one file-two)
  "Compare two files specified by FILE-ONE and FILE-TWO.
This function is used in sort to move most possible file names
to the beginning of the list (File.h -> File.cpp moved to top)."
  (and (equal (file-name-sans-extension flymake-included-file-name)
	      (file-name-base file-one))
       (not (equal file-one file-two))))

(defvar flymake-check-file-limit 8192
  "Maximum number of chars to look at when checking possible master file.
Nil means search the entire file.")

(defun flymake-check-patch-master-file-buffer
    (master-file-temp-buffer
     master-file-name patched-master-file-name
     source-file-name patched-source-file-name
     include-dirs regexp)
  "Check if MASTER-FILE-NAME is a master file for SOURCE-FILE-NAME.
If yes, patch a copy of MASTER-FILE-NAME to include PATCHED-SOURCE-FILE-NAME
instead of SOURCE-FILE-NAME.

For example, foo.cpp is a master file if it includes foo.h.

When a buffer for MASTER-FILE-NAME exists, use it as a source
instead of reading master file from disk."
  (let* ((source-file-nondir (file-name-nondirectory source-file-name))
         (source-file-extension (file-name-extension source-file-nondir))
         (source-file-nonext (file-name-sans-extension source-file-nondir))
         (found                     nil)
	 (inc-name                  nil)
	 (search-limit              flymake-check-file-limit))
    (setq regexp
          (format regexp	; "[ \t]*#[ \t]*include[ \t]*\"\\(.*%s\\)\""
                  ;; Hack for tex files, where \include often excludes .tex.
                  ;; Maybe this is safe generally.
                  (if (and (> (length source-file-extension) 1)
                           (string-equal source-file-extension "tex"))
                      (format "%s\\(?:\\.%s\\)?"
                              (regexp-quote source-file-nonext)
                              (regexp-quote source-file-extension))
                    (regexp-quote source-file-nondir))))
    (unwind-protect
        (with-current-buffer master-file-temp-buffer
          (if (or (not search-limit)
                  (> search-limit (point-max)))
              (setq search-limit (point-max)))
          (flymake-log 3 "checking %s against regexp %s"
                       master-file-name regexp)
          (goto-char (point-min))
          (while (and (< (point) search-limit)
                      (re-search-forward regexp search-limit t))
            (let ((match-beg   (match-beginning 1))
                  (match-end   (match-end 1)))

              (flymake-log 3 "found possible match for %s" source-file-nondir)
              (setq inc-name (match-string 1))
              (and (> (length source-file-extension) 1)
                   (string-equal source-file-extension "tex")
                   (not (string-match (format "\\.%s\\'" source-file-extension)
                                      inc-name))
                   (setq inc-name (concat inc-name "." source-file-extension)))
              (when (eq t (compare-strings
                           source-file-nondir nil nil
                           inc-name (- (length inc-name)
                                       (length source-file-nondir)) nil))
                (flymake-log 3 "inc-name=%s" inc-name)
                (when (flymake-check-include source-file-name inc-name
                                             include-dirs)
                  (setq found t)
                  ;;  replace-match is not used here as it fails in
                  ;; XEmacs with 'last match not a buffer' error as
                  ;; check-includes calls replace-in-string
                  (flymake-replace-region
                   match-beg match-end
                   (file-name-nondirectory patched-source-file-name))))
              (forward-line 1)))
          (when found
            (flymake-save-buffer-in-file patched-master-file-name)))
      ;;+(flymake-log 3 "killing buffer %s"
      ;;                (buffer-name master-file-temp-buffer))
      (kill-buffer master-file-temp-buffer))
    ;;+(flymake-log 3 "check-patch master file %s: %s" master-file-name found)
    (when found
      (flymake-log 2 "found master file %s" master-file-name))
    found))

;;; XXX: remove
(defun flymake-replace-region (beg end rep)
  "Replace text in BUFFER in region (BEG END) with REP."
  (save-excursion
    (goto-char end)
    ;; Insert before deleting, so as to better preserve markers's positions.
    (insert rep)
    (delete-region beg end)))

(defun flymake-read-file-to-temp-buffer (file-name)
  "Insert contents of FILE-NAME into newly created temp buffer."
  (let* ((temp-buffer (get-buffer-create (generate-new-buffer-name (concat "flymake:" (file-name-nondirectory file-name))))))
    (with-current-buffer temp-buffer
      (insert-file-contents file-name))
    temp-buffer))

(defun flymake-copy-buffer-to-temp-buffer (buffer)
  "Copy contents of BUFFER into newly created temp buffer."
  (with-current-buffer
      (get-buffer-create (generate-new-buffer-name
                          (concat "flymake:" (buffer-name buffer))))
    (insert-buffer-substring buffer)
    (current-buffer)))

(defun flymake-check-include (source-file-name inc-name include-dirs)
  "Check if SOURCE-FILE-NAME can be found in include path.
Return t if it can be found via include path using INC-NAME."
  (if (file-name-absolute-p inc-name)
      (flymake-same-files source-file-name inc-name)
    (while (and include-dirs
                (not (flymake-same-files
                      source-file-name
                      (concat (file-name-directory source-file-name)
                              "/" (car include-dirs)
                              "/" inc-name))))
      (setq include-dirs (cdr include-dirs)))
    include-dirs))

(defun flymake-find-buffer-for-file (file-name)
  "Check if there exists a buffer visiting FILE-NAME.
Return t if so, nil if not."
  (let ((buffer-name (get-file-buffer file-name)))
    (if buffer-name
	(get-buffer buffer-name))))

(defun flymake-create-master-file (source-file-name patched-source-file-name get-incl-dirs-f create-temp-f masks include-regexp)
  "Save SOURCE-FILE-NAME with a different name.
Find master file, patch and save it."
  (let* ((possible-master-files     (flymake-find-possible-master-files source-file-name flymake-master-file-dirs masks))
	 (master-file-count         (length possible-master-files))
	 (idx                       0)
	 (temp-buffer               nil)
	 (master-file-name          nil)
	 (patched-master-file-name  nil)
	 (found                     nil))

    (while (and (not found) (< idx master-file-count))
      (setq master-file-name (nth idx possible-master-files))
      (setq patched-master-file-name (funcall create-temp-f master-file-name "flymake_master"))
      (if (flymake-find-buffer-for-file master-file-name)
	  (setq temp-buffer (flymake-copy-buffer-to-temp-buffer (flymake-find-buffer-for-file master-file-name)))
	(setq temp-buffer (flymake-read-file-to-temp-buffer master-file-name)))
      (setq found
	    (flymake-check-patch-master-file-buffer
	     temp-buffer
	     master-file-name
	     patched-master-file-name
	     source-file-name
	     patched-source-file-name
	     (funcall get-incl-dirs-f (file-name-directory master-file-name))
	     include-regexp))
      (setq idx (1+ idx)))
    (if found
	(list master-file-name patched-master-file-name)
      (progn
	(flymake-log 3 "none of %d master file(s) checked includes %s" master-file-count
		     (file-name-nondirectory source-file-name))
	nil))))

(defun flymake-save-buffer-in-file (file-name)
  "Save the entire buffer contents into file FILE-NAME.
Create parent directories as needed."
  (make-directory (file-name-directory file-name) 1)
  (write-region nil nil file-name nil 566)
  (flymake-log 3 "saved buffer %s in file %s" (buffer-name) file-name))

(defun flymake-process-filter (process output)
  "Parse OUTPUT and highlight error lines.
It's flymake process filter."
  (let ((source-buffer (process-buffer process)))

    (flymake-log 3 "received %d byte(s) of output from process %d"
                 (length output) (process-id process))
    (when (buffer-live-p source-buffer)
      (with-current-buffer source-buffer
        (flymake-parse-output-and-residual output)))))

(defun flymake-process-sentinel (process _event)
  "Sentinel for syntax check buffers."
  (when (memq (process-status process) '(signal exit))
    (let* ((exit-status       (process-exit-status process))
           (command           (process-command process))
           (source-buffer     (process-buffer process))
           (cleanup-f         (flymake-get-cleanup-function (buffer-file-name source-buffer))))

      (flymake-log 2 "process %d exited with code %d"
                   (process-id process) exit-status)
      (condition-case err
          (progn
            (flymake-log 3 "cleaning up using %s" cleanup-f)
            (when (buffer-live-p source-buffer)
              (with-current-buffer source-buffer
                (funcall cleanup-f)))

            (delete-process process)
            (setq flymake-processes (delq process flymake-processes))

            (when (buffer-live-p source-buffer)
              (with-current-buffer source-buffer

                (flymake-parse-residual)
                (flymake-post-syntax-check exit-status command)
                (setq flymake-is-running nil))))
        (error
         (let ((err-str (format "Error in process sentinel for buffer %s: %s"
                                source-buffer (error-message-string err))))
           (flymake-log 0 err-str)
           (with-current-buffer source-buffer
             (setq flymake-is-running nil))))))))

(defun flymake-post-syntax-check (exit-status command)
  (let ((err-count (flymake-get-err-count flymake-new-err-info "e"))
        (warn-count (flymake-get-err-count flymake-new-err-info "w")))
    (if (equal 0 exit-status)
        (flymake-report flymake-new-err-info)
      (if flymake-check-was-interrupted
          (flymake-report-status nil "") ;; STOPPED
        (if (and (zerop err-count) (zerop warn-count))
            (flymake-report-fatal-status "CFGERR"
                                         (format "Configuration error has occurred while running %s" command))
          (flymake-report flymake-new-err-info))))
    (setq flymake-new-err-info nil)))


(defun flymake-parse-output-and-residual (output)
  "Split OUTPUT into lines, merge in residual if necessary."
  (let* ((buffer-residual     flymake-output-residual)
         (total-output        (if buffer-residual (concat buffer-residual output) output))
         (lines-and-residual  (flymake-split-output total-output))
         (lines               (nth 0 lines-and-residual))
         (new-residual        (nth 1 lines-and-residual)))
    (setq flymake-output-residual new-residual)
    (setq flymake-new-err-info
          (flymake-parse-err-lines
           flymake-new-err-info lines))))

(defvar-local flymake-new-err-info nil
  "Same as `flymake-err-info', effective when a syntax check is in progress.")

(defun flymake-parse-residual ()
  "Parse residual if it's non empty."
  (when flymake-output-residual
    (setq flymake-new-err-info
          (flymake-parse-err-lines
           flymake-new-err-info
           (list flymake-output-residual)))
    (setq flymake-output-residual nil)))

(defun flymake-parse-err-lines (err-info-list lines)
  "Parse err LINES, store info in ERR-INFO-LIST."
  (let* ((count              (length lines))
	 (idx                0)
	 (line-err-info      nil)
	 (real-file-name     nil)
	 (source-file-name   buffer-file-name)
	 (get-real-file-name-f (flymake-get-real-file-name-function source-file-name)))

    (while (< idx count)
      (setq line-err-info (flymake-parse-line (nth idx lines)))
      (when line-err-info
	(setq real-file-name (funcall get-real-file-name-f
                                      (flymake-ler-file line-err-info)))
	(setq line-err-info (flymake-ler-set-full-file line-err-info real-file-name))

	(when (flymake-same-files real-file-name source-file-name)
	  (setq line-err-info (flymake-ler-set-file line-err-info nil))
	  (setq err-info-list (flymake-add-err-info err-info-list line-err-info))))
      (flymake-log 3 "parsed `%s', %s line-err-info" (nth idx lines) (if line-err-info "got" "no"))
      (setq idx (1+ idx)))
    err-info-list))

(defun flymake-split-output (output)
  "Split OUTPUT into lines.
Return last one as residual if it does not end with newline char.
Returns ((LINES) RESIDUAL)."
  (when (and output (> (length output) 0))
    (let* ((lines (split-string output "[\n\r]+" t))
	   (complete (equal "\n" (char-to-string (aref output (1- (length output))))))
	   (residual nil))
      (when (not complete)
	(setq residual (car (last lines)))
	(setq lines (butlast lines)))
      (list lines residual))))

(defun flymake-reformat-err-line-patterns-from-compile-el (original-list)
  "Grab error line patterns from ORIGINAL-LIST in compile.el format.
Convert it to flymake internal format."
  (let* ((converted-list '()))
    (dolist (item original-list)
      (setq item (cdr item))
      (let ((regexp (nth 0 item))
	    (file (nth 1 item))
	    (line (nth 2 item))
	    (col (nth 3 item)))
	(if (consp file)	(setq file (car file)))
	(if (consp line)	(setq line (car line)))
	(if (consp col)	(setq col (car col)))

	(when (not (functionp line))
	  (setq converted-list (cons (list regexp file line col) converted-list)))))
    converted-list))

(require 'compile)

(defvar flymake-err-line-patterns ; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
  (append
   '(
     ;; MS Visual C++ 6.0
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(\\(error\\|warning\\|fatal error\\) \\(C[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; jikes
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:[0-9]+: \\(\\(Error\\|Warning\\|Caution\\|Semantic Error\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; MS midl
     ("midl[ ]*:[ ]*\\(command line error .*\\)"
      nil nil nil 1)
     ;; MS C#
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\),[0-9]+): \\(\\(error\\|warning\\|fatal error\\) \\(CS[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; perl
     ("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)
     ;; PHP
     ("\\(?:Parse\\|Fatal\\) error: \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)" 2 3 nil 1)
     ;; LaTeX warnings (fileless) ("\\(LaTeX \\(Warning\\|Error\\): .*\\) on input line \\([0-9]+\\)" 20 3 nil 1)
     ;; ant/javac.  Note this also matches gcc warnings!
     (" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\):\\([0-9]+\\)\\(?::[0-9]+\\)?:[ \t\n]*\\(.+\\)"
      2 4 nil 5))
   ;; compilation-error-regexp-alist)
   (flymake-reformat-err-line-patterns-from-compile-el compilation-error-regexp-alist-alist))
  "Patterns for matching error/warning lines.  Each pattern has the form
\(REGEXP FILE-IDX LINE-IDX COL-IDX ERR-TEXT-IDX).
Use `flymake-reformat-err-line-patterns-from-compile-el' to add patterns
from compile.el")

(define-obsolete-variable-alias 'flymake-warning-re 'flymake-warning-predicate "24.4")
(defvar flymake-warning-predicate "^[wW]arning"
  "Predicate matching against error text to detect a warning.
Takes a single argument, the error's text and should return non-nil
if it's a warning.
Instead of a function, it can also be a regular expression.")

(defun flymake-parse-line (line)
  "Parse LINE to see if it is an error or warning.
Return its components if so, nil otherwise."
  (let ((raw-file-name nil)
	(line-no 0)
	(err-type "e")
	(err-text nil)
	(patterns flymake-err-line-patterns)
	(matched nil))
    (while (and patterns (not matched))
      (when (string-match (car (car patterns)) line)
	(let* ((file-idx (nth 1 (car patterns)))
	       (line-idx (nth 2 (car patterns))))

	  (setq raw-file-name (if file-idx (match-string file-idx line) nil))
	  (setq line-no       (if line-idx (string-to-number
                                            (match-string line-idx line)) 0))
	  (setq err-text      (if (> (length (car patterns)) 4)
				  (match-string (nth 4 (car patterns)) line)
				(flymake-patch-err-text
                                 (substring line (match-end 0)))))
	  (if (null err-text)
              (setq err-text "<no error text>")
            (when (cond ((stringp flymake-warning-predicate)
                         (string-match flymake-warning-predicate err-text))
                        ((functionp flymake-warning-predicate)
                         (funcall flymake-warning-predicate err-text)))
              (setq err-type "w")))
	  (flymake-log
           3 "parse line: file-idx=%s line-idx=%s file=%s line=%s text=%s"
           file-idx line-idx raw-file-name line-no err-text)
	  (setq matched t)))
      (setq patterns (cdr patterns)))
    (if matched
	(flymake-ler-make-ler raw-file-name line-no err-type err-text)
      ())))

(defun flymake-get-project-include-dirs-imp (basedir)
  "Include dirs for the project current file belongs to."
  (if (flymake-get-project-include-dirs-from-cache basedir)
      (progn
	(flymake-get-project-include-dirs-from-cache basedir))
    ;;else
    (let* ((command-line  (concat "make -C "
				  (shell-quote-argument basedir)
				  " DUMPVARS=INCLUDE_DIRS dumpvars"))
	   (output        (shell-command-to-string command-line))
	   (lines         (split-string output "\n" t))
	   (count         (length lines))
	   (idx           0)
	   (inc-dirs      nil))
      (while (and (< idx count) (not (string-match "^INCLUDE_DIRS=.*" (nth idx lines))))
	(setq idx (1+ idx)))
      (when (< idx count)
	(let* ((inc-lines  (split-string (nth idx lines) " *-I" t))
	       (inc-count  (length inc-lines)))
	  (while (> inc-count 0)
	    (when (not (string-match "^INCLUDE_DIRS=.*" (nth (1- inc-count) inc-lines)))
	      (push (replace-regexp-in-string "\"" "" (nth (1- inc-count) inc-lines)) inc-dirs))
	    (setq inc-count (1- inc-count)))))
      (flymake-add-project-include-dirs-to-cache basedir inc-dirs)
      inc-dirs)))

(defvar flymake-get-project-include-dirs-function #'flymake-get-project-include-dirs-imp
  "Function used to get project include dirs, one parameter: basedir name.")

(defun flymake-get-project-include-dirs (basedir)
  (funcall flymake-get-project-include-dirs-function basedir))

(defun flymake-get-system-include-dirs ()
  "System include dirs - from the `INCLUDE' env setting."
  (let* ((includes (getenv "INCLUDE")))
    (if includes (split-string includes path-separator t) nil)))

(defvar flymake-project-include-dirs-cache (make-hash-table :test #'equal))

(defun flymake-get-project-include-dirs-from-cache (base-dir)
  (gethash base-dir flymake-project-include-dirs-cache))

(defun flymake-add-project-include-dirs-to-cache (base-dir include-dirs)
  (puthash base-dir include-dirs flymake-project-include-dirs-cache))

(defun flymake-clear-project-include-dirs-cache ()
  (clrhash flymake-project-include-dirs-cache))

(defun flymake-get-include-dirs (base-dir)
  "Get dirs to use when resolving local file names."
  (let* ((include-dirs (append '(".") (flymake-get-project-include-dirs base-dir) (flymake-get-system-include-dirs))))
    include-dirs))

;; (defun flymake-restore-formatting ()
;;   "Remove any formatting made by flymake."
;;   )

;; (defun flymake-get-program-dir (buffer)
;;   "Get dir to start program in."
;;   (unless (bufferp buffer)
;;     (error "Invalid buffer"))
;;   (with-current-buffer buffer
;;     default-directory))

(defun flymake-safe-delete-file (file-name)
  (when (and file-name (file-exists-p file-name))
    (delete-file file-name)
    (flymake-log 1 "deleted file %s" file-name)))

(defun flymake-safe-delete-directory (dir-name)
  (condition-case nil
      (progn
	(delete-directory dir-name)
	(flymake-log 1 "deleted dir %s" dir-name))
    (error
     (flymake-log 1 "Failed to delete dir %s, error ignored" dir-name))))

(defun flymake-start-syntax-check ()
  "Start syntax checking for current buffer."
  (interactive)
  (flymake-log 3 "flymake is running: %s" flymake-is-running)
  (when (and (not flymake-is-running)
             (flymake-can-syntax-check-file buffer-file-name))
    (when (or (not flymake-compilation-prevents-syntax-check)
              (not (flymake-compilation-is-running))) ;+ (flymake-rep-ort-status buffer "COMP")
      (flymake-clear-buildfile-cache)
      (flymake-clear-project-include-dirs-cache)

      (setq flymake-check-was-interrupted nil)
      (setq flymake-check-start-time (float-time))

      (let* ((source-file-name  buffer-file-name)
             (init-f (flymake-get-init-function source-file-name))
             (cleanup-f (flymake-get-cleanup-function source-file-name))
             (cmd-and-args (funcall init-f))
             (cmd          (nth 0 cmd-and-args))
             (args         (nth 1 cmd-and-args))
             (dir          (nth 2 cmd-and-args)))
        (if (not cmd-and-args)
            (progn
              (flymake-log 0 "init function %s for %s failed, cleaning up" init-f source-file-name)
              (funcall cleanup-f))
          (progn
            (setq flymake-last-change-time nil)
            (flymake-start-syntax-check-process cmd args dir)))))))

(defun flymake-start-syntax-check-process (cmd args dir)
"Start syntax check process."
(condition-case err
    (let* ((process
            (let ((default-directory (or dir default-directory)))
              (when dir
                (flymake-log 3 "starting process on dir %s" dir))
              (apply 'start-file-process
                     "flymake-proc" (current-buffer) cmd args))))
      (set-process-sentinel process 'flymake-process-sentinel)
      (set-process-filter process 'flymake-process-filter)
      (set-process-query-on-exit-flag process nil)
      (push process flymake-processes)

      (setq flymake-is-running t)
      (setq flymake-last-change-time nil)

      (flymake-report-status nil "*")
      (flymake-log 2 "started process %d, command=%s, dir=%s"
                   (process-id process) (process-command process)
                   default-directory)
      process)
  (error
   (let* ((err-str
           (format-message
            "Failed to launch syntax check process `%s' with args %s: %s"
            cmd args (error-message-string err)))
          (source-file-name buffer-file-name)
          (cleanup-f        (flymake-get-cleanup-function source-file-name)))
     (flymake-log 0 err-str)
     (funcall cleanup-f)
     (flymake-report-fatal-status "PROCERR" err-str)))))

(defun flymake-kill-process (proc)
  "Kill process PROC."
  (kill-process proc)
  (let* ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(setq flymake-check-was-interrupted t))))
  (flymake-log 1 "killed process %d" (process-id proc)))

(defun flymake-stop-all-syntax-checks ()
  "Kill all syntax check processes."
  (interactive)
  (while flymake-processes
    (flymake-kill-process (pop flymake-processes))))

(defun flymake-compilation-is-running ()
  (and (boundp 'compilation-in-progress)
       compilation-in-progress))

(defun flymake-compile ()
  "Kill all flymake syntax checks, start compilation."
  (interactive)
  (flymake-stop-all-syntax-checks)
  (call-interactively 'compile))

;;;; general init-cleanup and helper routines
(defun flymake-create-temp-inplace (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((ext (file-name-extension file-name))
	 (temp-name (file-truename
		     (concat (file-name-sans-extension file-name)
			     "_" prefix
			     (and ext (concat "." ext))))))
    (flymake-log 3 "create-temp-inplace: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun flymake-create-temp-with-folder-structure (file-name _prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))

  (let* ((dir       (file-name-directory file-name))
         ;; Not sure what this slash-pos is all about, but I guess it's just
         ;; trying to remove the leading / of absolute file names.
	 (slash-pos (string-match "/" dir))
	 (temp-dir  (expand-file-name (substring dir (1+ slash-pos))
                                      temporary-file-directory)))

    (file-truename (expand-file-name (file-name-nondirectory file-name)
                                     temp-dir))))

(defun flymake-delete-temp-directory (dir-name)
  "Attempt to delete temp dir created by `flymake-create-temp-with-folder-structure', do not fail on error."
  (let* ((temp-dir    temporary-file-directory)
	 (suffix      (substring dir-name (1+ (length temp-dir)))))

    (while (> (length suffix) 0)
      (setq suffix (directory-file-name suffix))
      ;;+(flymake-log 0 "suffix=%s" suffix)
      (flymake-safe-delete-directory
       (file-truename (expand-file-name suffix temp-dir)))
      (setq suffix (file-name-directory suffix)))))

(defvar-local flymake-temp-source-file-name nil)
(defvar-local flymake-master-file-name nil)
(defvar-local flymake-temp-master-file-name nil)
(defvar-local flymake-base-dir nil)

(defun flymake-init-create-temp-buffer-copy (create-temp-f)
  "Make a temporary copy of the current buffer, save its name in buffer data and return the name."
  (let*  ((source-file-name       buffer-file-name)
	  (temp-source-file-name  (funcall create-temp-f source-file-name "flymake")))

    (flymake-save-buffer-in-file temp-source-file-name)
    (setq flymake-temp-source-file-name temp-source-file-name)
    temp-source-file-name))

(defun flymake-simple-cleanup ()
  "Do cleanup after `flymake-init-create-temp-buffer-copy'.
Delete temp file."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (setq flymake-last-change-time nil))

(defun flymake-get-real-file-name (file-name-from-err-msg)
  "Translate file name from error message to \"real\" file name.
Return full-name.  Names are real, not patched."
  (let* ((real-name		nil)
	 (source-file-name	buffer-file-name)
	 (master-file-name	flymake-master-file-name)
	 (temp-source-file-name	flymake-temp-source-file-name)
	 (temp-master-file-name	flymake-temp-master-file-name)
	 (base-dirs
          (list flymake-base-dir
                (file-name-directory source-file-name)
                (if master-file-name (file-name-directory master-file-name))))
	 (files (list (list source-file-name       source-file-name)
                      (list temp-source-file-name  source-file-name)
                      (list master-file-name       master-file-name)
                      (list temp-master-file-name  master-file-name))))

    (when (equal 0 (length file-name-from-err-msg))
      (setq file-name-from-err-msg source-file-name))

    (setq real-name (flymake-get-full-patched-file-name file-name-from-err-msg base-dirs files))
    ;; if real-name is nil, than file name from err msg is none of the files we've patched
    (if (not real-name)
	(setq real-name (flymake-get-full-nonpatched-file-name file-name-from-err-msg base-dirs)))
    (if (not real-name)
	(setq real-name file-name-from-err-msg))
    (setq real-name (flymake-fix-file-name real-name))
    (flymake-log 3 "get-real-file-name: file-name=%s real-name=%s" file-name-from-err-msg real-name)
    real-name))

(defun flymake-get-full-patched-file-name (file-name-from-err-msg base-dirs files)
  (let* ((base-dirs-count  (length base-dirs))
	 (file-count       (length files))
	 (real-name        nil))

    (while (and (not real-name) (> base-dirs-count 0))
      (setq file-count (length files))
      (while (and (not real-name) (> file-count 0))
	(let* ((this-dir        (nth (1- base-dirs-count) base-dirs))
	       (this-file       (nth 0 (nth (1- file-count) files)))
	       (this-real-name  (nth 1 (nth (1- file-count) files))))
	  ;;+(flymake-log 0 "this-dir=%s this-file=%s this-real=%s msg-file=%s" this-dir this-file this-real-name file-name-from-err-msg)
	  (when (and this-dir this-file (flymake-same-files
					 (expand-file-name file-name-from-err-msg this-dir)
					 this-file))
	    (setq real-name this-real-name)))
	(setq file-count (1- file-count)))
      (setq base-dirs-count (1- base-dirs-count)))
    real-name))

(defun flymake-get-full-nonpatched-file-name (file-name-from-err-msg base-dirs)
  (let* ((real-name  nil))
    (if (file-name-absolute-p file-name-from-err-msg)
	(setq real-name file-name-from-err-msg)
      (let* ((base-dirs-count  (length base-dirs)))
	(while (and (not real-name) (> base-dirs-count 0))
	  (let* ((full-name (expand-file-name file-name-from-err-msg
					      (nth (1- base-dirs-count) base-dirs))))
	    (if (file-exists-p full-name)
		(setq real-name full-name))
	    (setq base-dirs-count (1- base-dirs-count))))))
    real-name))

(defun flymake-init-find-buildfile-dir (source-file-name buildfile-name)
  "Find buildfile, store its dir in buffer data and return its dir, if found."
  (let* ((buildfile-dir
          (flymake-find-buildfile buildfile-name
                                  (file-name-directory source-file-name))))
    (if buildfile-dir
        (setq flymake-base-dir buildfile-dir)
      (flymake-log 1 "no buildfile (%s) for %s" buildfile-name source-file-name)
      (flymake-report-fatal-status
       "NOMK" (format "No buildfile (%s) found for %s"
                      buildfile-name source-file-name)))))

(defun flymake-init-create-temp-source-and-master-buffer-copy (get-incl-dirs-f create-temp-f master-file-masks include-regexp)
  "Find master file (or buffer), create its copy along with a copy of the source file."
  (let* ((source-file-name       buffer-file-name)
	 (temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f))
	 (master-and-temp-master (flymake-create-master-file
				  source-file-name temp-source-file-name
				  get-incl-dirs-f create-temp-f
				  master-file-masks include-regexp)))

    (if (not master-and-temp-master)
	(progn
	  (flymake-log 1 "cannot find master file for %s" source-file-name)
          (flymake-report-status "!" "")	; NOMASTER
          nil)
      (setq flymake-master-file-name (nth 0 master-and-temp-master))
      (setq flymake-temp-master-file-name (nth 1 master-and-temp-master)))))

(defun flymake-master-cleanup ()
  (flymake-simple-cleanup)
  (flymake-safe-delete-file flymake-temp-master-file-name))

;;;; make-specific init-cleanup routines
(defun flymake-get-syntax-check-program-args (source-file-name base-dir use-relative-base-dir use-relative-source get-cmd-line-f)
  "Create a command line for syntax check using GET-CMD-LINE-F."
  (funcall get-cmd-line-f
           (if use-relative-source
               (file-relative-name source-file-name base-dir)
             source-file-name)
           (if use-relative-base-dir
               (file-relative-name base-dir
                                   (file-name-directory source-file-name))
             base-dir)))

(defun flymake-get-make-cmdline (source base-dir)
  (list "make"
	(list "-s"
	      "-C"
	      base-dir
	      (concat "CHK_SOURCES=" source)
	      "SYNTAX_CHECK_MODE=1"
	      "check-syntax")))

(defun flymake-get-ant-cmdline (source base-dir)
  (list "ant"
	(list "-buildfile"
	      (concat base-dir "/" "build.xml")
	      (concat "-DCHK_SOURCES=" source)
	      "check-syntax")))

(defun flymake-simple-make-init-impl (create-temp-f use-relative-base-dir use-relative-source build-file-name get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
	 (source-file-name   buffer-file-name)
	 (buildfile-dir      (flymake-init-find-buildfile-dir source-file-name build-file-name)))
    (if buildfile-dir
	(let* ((temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f)))
	  (setq args (flymake-get-syntax-check-program-args temp-source-file-name buildfile-dir
							    use-relative-base-dir use-relative-source
							    get-cmdline-f))))
    args))

(defun flymake-simple-make-init ()
  (flymake-simple-make-init-impl 'flymake-create-temp-inplace t t "Makefile" 'flymake-get-make-cmdline))

(defun flymake-master-make-init (get-incl-dirs-f master-file-masks include-regexp)
  "Create make command line for a source file checked via master file compilation."
  (let* ((make-args nil)
	 (temp-master-file-name (flymake-init-create-temp-source-and-master-buffer-copy
                                 get-incl-dirs-f 'flymake-create-temp-inplace
				 master-file-masks include-regexp)))
    (when temp-master-file-name
      (let* ((buildfile-dir (flymake-init-find-buildfile-dir temp-master-file-name "Makefile")))
	(if  buildfile-dir
	    (setq make-args (flymake-get-syntax-check-program-args
			     temp-master-file-name buildfile-dir nil nil 'flymake-get-make-cmdline)))))
    make-args))

(defun flymake-find-make-buildfile (source-dir)
  (flymake-find-buildfile "Makefile" source-dir))

;;;; .h/make specific
(defun flymake-master-make-header-init ()
  (flymake-master-make-init
   'flymake-get-include-dirs
   '("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'")
   "[ \t]*#[ \t]*include[ \t]*\"\\([[:word:]0-9/\\_.]*%s\\)\""))

;;;; .java/make specific
(defun flymake-simple-make-java-init ()
  (flymake-simple-make-init-impl 'flymake-create-temp-with-folder-structure nil nil "Makefile" 'flymake-get-make-cmdline))

(defun flymake-simple-ant-java-init ()
  (flymake-simple-make-init-impl 'flymake-create-temp-with-folder-structure nil nil "build.xml" 'flymake-get-ant-cmdline))

(defun flymake-simple-java-cleanup ()
  "Cleanup after `flymake-simple-make-java-init' -- delete temp file and dirs."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (flymake-delete-temp-directory
     (file-name-directory flymake-temp-source-file-name))))

;;;; perl-specific init-cleanup routines
(defun flymake-perl-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc " local-file))))

;;;; php-specific init-cleanup routines
(defun flymake-php-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local-file "-l"))))

;;;; tex-specific init-cleanup routines
(defun flymake-get-tex-args (file-name)
  ;;(list "latex" (list "-c-style-errors" file-name))
  (list "texify" (list "--pdf" "--tex-option=-c-style-errors" file-name)))

(defun flymake-simple-tex-init ()
  (flymake-get-tex-args (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace)))

;; Perhaps there should be a buffer-local variable flymake-master-file
;; that people can set to override this stuff.  Could inherit from
;; the similar AUCTeX variable.
(defun flymake-master-tex-init ()
  (let* ((temp-master-file-name (flymake-init-create-temp-source-and-master-buffer-copy
                                 'flymake-get-include-dirs-dot 'flymake-create-temp-inplace
				 '("\\.tex\\'")
				 "[ \t]*\\in\\(?:put\\|clude\\)[ \t]*{\\(.*%s\\)}")))
    (when temp-master-file-name
      (flymake-get-tex-args temp-master-file-name))))

(defun flymake-get-include-dirs-dot (_base-dir)
  '("."))

;;;; xml-specific init-cleanup routines
(defun flymake-xml-init ()
  (list flymake-xml-program
        (list "val" (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))))

(provide 'flymake-proc)
;;; flymake-proc.el ends here
