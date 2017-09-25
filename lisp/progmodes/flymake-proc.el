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

(defcustom flymake-proc-compilation-prevents-syntax-check t
  "If non-nil, don't start syntax check if compilation is running."
  :group 'flymake
  :type 'boolean)

(defcustom flymake-proc-xml-program
  (if (executable-find "xmlstarlet") "xmlstarlet" "xml")
  "Program to use for XML validation."
  :type 'file
  :group 'flymake
  :version "24.4")

(defcustom flymake-proc-master-file-dirs '("." "./src" "./UnitTest")
  "Dirs where to look for master files."
  :group 'flymake
  :type '(repeat (string)))

(defcustom flymake-proc-master-file-count-limit 32
  "Max number of master files to check."
  :group 'flymake
  :type 'integer)

(defcustom flymake-proc-allowed-file-name-masks
  '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-proc-simple-make-init)
    ("\\.xml\\'" flymake-proc-xml-init)
    ("\\.html?\\'" flymake-proc-xml-init)
    ("\\.cs\\'" flymake-proc-simple-make-init)
    ("\\.p[ml]\\'" flymake-proc-perl-init)
    ("\\.php[345]?\\'" flymake-proc-php-init)
    ("\\.h\\'" flymake-proc-master-make-header-init flymake-proc-master-cleanup)
    ("\\.java\\'" flymake-proc-simple-make-java-init flymake-proc-simple-java-cleanup)
    ("[0-9]+\\.tex\\'" flymake-proc-master-tex-init flymake-proc-master-cleanup)
    ("\\.tex\\'" flymake-proc-simple-tex-init)
    ("\\.idl\\'" flymake-proc-simple-make-init)
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
CLEANUP is the cleanup function to use, default `flymake-proc-simple-cleanup'.
NAME is the file name function to use, default `flymake-proc-get-real-file-name'."
  :group 'flymake
  :type '(alist :key-type (regexp :tag "File regexp")
                :value-type
                (list :tag "Handler functions"
                      (function :tag "Init function")
                      (choice :tag "Cleanup function"
                              (const :tag "flymake-proc-simple-cleanup" nil)
                              function)
                      (choice :tag "Name function"
                              (const :tag "flymake-proc-get-real-file-name" nil)
                              function))))

(defvar-local flymake-proc--process nil
  "Currently active flymake process for a buffer, if any.")

(defvar flymake-proc--processes nil
  "List of currently active flymake processes.")

(defvar flymake-proc--report-fn nil
  "If bound, function used to report back to flymake's UI.")

(defun flymake-proc--get-file-name-mode-and-masks (file-name)
  "Return the corresponding entry from `flymake-proc-allowed-file-name-masks'."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (let ((fnm flymake-proc-allowed-file-name-masks)
	(mode-and-masks nil))
    (while (and (not mode-and-masks) fnm)
      (if (string-match (car (car fnm)) file-name)
	  (setq mode-and-masks (cdr (car fnm))))
      (setq fnm (cdr fnm)))
    (flymake-log 3 "file %s, init=%s" file-name (car mode-and-masks))
    mode-and-masks))

(defun flymake-proc--get-init-function (file-name)
  "Return init function to be used for the file."
  (let* ((init-f  (nth 0 (flymake-proc--get-file-name-mode-and-masks file-name))))
    ;;(flymake-log 0 "calling %s" init-f)
    ;;(funcall init-f (current-buffer))
    init-f))

(defun flymake-proc--get-cleanup-function (file-name)
  "Return cleanup function to be used for the file."
  (or (nth 1 (flymake-proc--get-file-name-mode-and-masks file-name))
      'flymake-proc-simple-cleanup))

(defun flymake-proc--get-real-file-name-function (file-name)
  (or (nth 2 (flymake-proc--get-file-name-mode-and-masks file-name))
      'flymake-proc-get-real-file-name))

(defvar flymake-proc--find-buildfile-cache (make-hash-table :test #'equal))

(defun flymake-proc--get-buildfile-from-cache (dir-name)
  "Look up DIR-NAME in cache and return its associated value.
If DIR-NAME is not found, return nil."
  (gethash dir-name flymake-proc--find-buildfile-cache))

(defun flymake-proc--add-buildfile-to-cache (dir-name buildfile)
  "Associate DIR-NAME with BUILDFILE in the buildfile cache."
  (puthash dir-name buildfile flymake-proc--find-buildfile-cache))

(defun flymake-proc--clear-buildfile-cache ()
  "Clear the buildfile cache."
  (clrhash flymake-proc--find-buildfile-cache))

(defun flymake-proc--find-buildfile (buildfile-name source-dir-name)
  "Find buildfile starting from current directory.
Buildfile includes Makefile, build.xml etc.
Return its file name if found, or nil if not found."
  (or (flymake-proc--get-buildfile-from-cache source-dir-name)
      (let* ((file (locate-dominating-file source-dir-name buildfile-name)))
        (if file
            (progn
              (flymake-log 3 "found buildfile at %s" file)
              (flymake-proc--add-buildfile-to-cache source-dir-name file)
              file)
          (progn
            (flymake-log 3 "buildfile for %s not found" source-dir-name)
            nil)))))

(defun flymake-proc--fix-file-name (name)
  "Replace all occurrences of `\\' with `/'."
  (when name
    (setq name (expand-file-name name))
    (setq name (abbreviate-file-name name))
    (setq name (directory-file-name name))
    name))

(defun flymake-proc--same-files (file-name-one file-name-two)
  "Check if FILE-NAME-ONE and FILE-NAME-TWO point to same file.
Return t if so, nil if not."
  (equal (flymake-proc--fix-file-name file-name-one)
	 (flymake-proc--fix-file-name file-name-two)))

;; This is bound dynamically to pass a parameter to a sort predicate below
(defvar flymake-proc--included-file-name)

(defun flymake-proc--find-possible-master-files (file-name master-file-dirs masks)
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
		(when (>= (length files) flymake-proc-master-file-count-limit)
		  (flymake-log 3 "master file count limit (%d) reached" flymake-proc-master-file-count-limit)
		  (setq done t)))
	      (setq dir-files (cdr dir-files))))
	  (setq masks (cdr masks))))
      (setq dirs (cdr dirs)))
    (when files
      (let ((flymake-proc--included-file-name (file-name-nondirectory file-name)))
	(setq files (sort files 'flymake-proc--master-file-compare))))
    (flymake-log 3 "found %d possible master file(s)" (length files))
    files))

(defun flymake-proc--master-file-compare (file-one file-two)
  "Compare two files specified by FILE-ONE and FILE-TWO.
This function is used in sort to move most possible file names
to the beginning of the list (File.h -> File.cpp moved to top)."
  (and (equal (file-name-sans-extension flymake-proc--included-file-name)
	      (file-name-base file-one))
       (not (equal file-one file-two))))

(defvar flymake-proc-check-file-limit 8192
  "Maximum number of chars to look at when checking possible master file.
Nil means search the entire file.")

(defun flymake-proc--check-patch-master-file-buffer
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
	 (search-limit              flymake-proc-check-file-limit))
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
                (when (flymake-proc--check-include source-file-name inc-name
                                             include-dirs)
                  (setq found t)
                  ;;  replace-match is not used here as it fails in
                  ;; XEmacs with 'last match not a buffer' error as
                  ;; check-includes calls replace-in-string
                  (flymake-proc--replace-region
                   match-beg match-end
                   (file-name-nondirectory patched-source-file-name))))
              (forward-line 1)))
          (when found
            (flymake-proc--save-buffer-in-file patched-master-file-name)))
      ;;+(flymake-log 3 "killing buffer %s"
      ;;                (buffer-name master-file-temp-buffer))
      (kill-buffer master-file-temp-buffer))
    ;;+(flymake-log 3 "check-patch master file %s: %s" master-file-name found)
    (when found
      (flymake-log 2 "found master file %s" master-file-name))
    found))

;;; XXX: remove
(defun flymake-proc--replace-region (beg end rep)
  "Replace text in BUFFER in region (BEG END) with REP."
  (save-excursion
    (goto-char end)
    ;; Insert before deleting, so as to better preserve markers's positions.
    (insert rep)
    (delete-region beg end)))

(defun flymake-proc--read-file-to-temp-buffer (file-name)
  "Insert contents of FILE-NAME into newly created temp buffer."
  (let* ((temp-buffer (get-buffer-create (generate-new-buffer-name (concat "flymake:" (file-name-nondirectory file-name))))))
    (with-current-buffer temp-buffer
      (insert-file-contents file-name))
    temp-buffer))

(defun flymake-proc--copy-buffer-to-temp-buffer (buffer)
  "Copy contents of BUFFER into newly created temp buffer."
  (with-current-buffer
      (get-buffer-create (generate-new-buffer-name
                          (concat "flymake:" (buffer-name buffer))))
    (insert-buffer-substring buffer)
    (current-buffer)))

(defun flymake-proc--check-include (source-file-name inc-name include-dirs)
  "Check if SOURCE-FILE-NAME can be found in include path.
Return t if it can be found via include path using INC-NAME."
  (if (file-name-absolute-p inc-name)
      (flymake-proc--same-files source-file-name inc-name)
    (while (and include-dirs
                (not (flymake-proc--same-files
                      source-file-name
                      (concat (file-name-directory source-file-name)
                              "/" (car include-dirs)
                              "/" inc-name))))
      (setq include-dirs (cdr include-dirs)))
    include-dirs))

(defun flymake-proc--find-buffer-for-file (file-name)
  "Check if there exists a buffer visiting FILE-NAME.
Return t if so, nil if not."
  (let ((buffer-name (get-file-buffer file-name)))
    (if buffer-name
	(get-buffer buffer-name))))

(defun flymake-proc--create-master-file (source-file-name patched-source-file-name get-incl-dirs-f create-temp-f masks include-regexp)
  "Save SOURCE-FILE-NAME with a different name.
Find master file, patch and save it."
  (let* ((possible-master-files     (flymake-proc--find-possible-master-files source-file-name flymake-proc-master-file-dirs masks))
	 (master-file-count         (length possible-master-files))
	 (idx                       0)
	 (temp-buffer               nil)
	 (master-file-name          nil)
	 (patched-master-file-name  nil)
	 (found                     nil))

    (while (and (not found) (< idx master-file-count))
      (setq master-file-name (nth idx possible-master-files))
      (setq patched-master-file-name (funcall create-temp-f master-file-name "flymake_master"))
      (if (flymake-proc--find-buffer-for-file master-file-name)
	  (setq temp-buffer (flymake-proc--copy-buffer-to-temp-buffer (flymake-proc--find-buffer-for-file master-file-name)))
	(setq temp-buffer (flymake-proc--read-file-to-temp-buffer master-file-name)))
      (setq found
	    (flymake-proc--check-patch-master-file-buffer
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

(defun flymake-proc--save-buffer-in-file (file-name)
  "Save the entire buffer contents into file FILE-NAME.
Create parent directories as needed."
  (make-directory (file-name-directory file-name) 1)
  (write-region nil nil file-name nil 566)
  (flymake-log 3 "saved buffer %s in file %s" (buffer-name) file-name))

(defun flymake-proc--diagnostics-for-pattern (proc pattern)
  (cl-flet ((guess-type
             (pred message)
             (cond ((null message)
                    :error)
                   ((stringp pred)
                    (if (string-match pred message)
                        :warning
                      :error))
                   ((functionp pred)
                    (let ((probe (funcall pred message)))
                      (cond ((assoc-default probe
                                            flymake-diagnostic-types-alist)
                             probe)
                            (probe
                             :warning)
                            (t
                             :error)))))))
    (condition-case err
        (cl-loop
         with (regexp file-idx line-idx col-idx message-idx) = pattern
         while (search-forward-regexp regexp nil t)
         for fname = (and file-idx (match-string file-idx))
         for message = (and message-idx (match-string message-idx))
         for line-string = (and line-idx (match-string line-idx))
         for line-number = (and line-string
                                (string-to-number line-string))
         for col-string = (and col-idx (match-string col-idx))
         for col-number = (and col-string
                               (string-to-number col-string))
         for full-file = (with-current-buffer (process-buffer proc)
                           (and fname
                                (funcall
                                 (flymake-proc--get-real-file-name-function
                                  fname)
                                 fname)))
         for buffer = (and full-file
                           (find-buffer-visiting full-file))
         if (eq buffer (process-buffer proc))
         collect (with-current-buffer buffer
                   (pcase-let ((`(,beg . ,end)
                                (flymake-diag-region line-number col-number)))
                     (flymake-make-diagnostic
                      buffer beg end
                      (guess-type flymake-proc-diagnostic-type-pred message)
                      message)))
         else
         do (flymake-log 2 "No buffer found for diagnosed file %s" fname))
      (error
       (flymake-log 1 "Error parsing process output for pattern %s: %s"
                    pattern err)
       nil))))

(defun flymake-proc--process-filter (proc string)
  "Parse STRING and collect diagnostics info."
  (flymake-log 3 "received %d byte(s) of output from process %d"
               (length string) (process-id proc))
  (let ((output-buffer (process-get proc 'flymake-proc--output-buffer))
        (flymake-proc--report-fn
         (process-get proc 'flymake-proc--report-fn)))
    (when (and (buffer-live-p (process-buffer proc))
               output-buffer)
      (with-current-buffer output-buffer
        (let ((moving (= (point) (process-mark proc)))
              (inhibit-read-only t)
              (unprocessed-mark
               (or (process-get proc 'flymake-proc--unprocessed-mark)
                   (set-marker (make-marker) (point-min)))))
          (save-excursion
            ;; Insert the text, advancing the process marker.
            (goto-char (process-mark proc))
            (insert string)
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc)))

          ;; check for new diagnostics
          ;;
          (save-excursion
            (goto-char unprocessed-mark)
            (dolist (pattern flymake-proc-err-line-patterns)
              (let ((new (flymake-proc--diagnostics-for-pattern proc pattern)))
                (process-put
                 proc
                 'flymake-proc--collected-diagnostics
                 (append new
                         (process-get proc
                                      'flymake-proc--collected-diagnostics)))))
            (process-put proc 'flymake-proc--unprocessed-mark
                         (point-marker))))))))

(defun flymake-proc--process-sentinel (proc _event)
  "Sentinel for syntax check buffers."
  (when (memq (process-status proc) '(signal exit))
    (let* ((exit-status   (process-exit-status proc))
           (command       (process-command proc))
           (source-buffer (process-buffer proc))
           (flymake-proc--report-fn (process-get proc
                                                 'flymake-proc--report-fn))
           (cleanup-f (flymake-proc--get-cleanup-function
                       (buffer-file-name source-buffer)))
           (diagnostics (process-get
                         proc
                         'flymake-proc--collected-diagnostics))
           (interrupted (process-get proc 'flymake-proc--interrupted)))
      (flymake-log 2 "process %d exited with code %d"
                   (process-id proc) exit-status)
      (unwind-protect
          (when (buffer-live-p source-buffer)
            (flymake-log 3 "cleaning up using %s" cleanup-f)
            (with-current-buffer source-buffer
              (funcall cleanup-f)
              (cond ((equal 0 exit-status)
                     (funcall flymake-proc--report-fn diagnostics))
                    (interrupted
                     (flymake-proc--panic :stopped interrupted))
                    ((null diagnostics)
                     ;; non-zero exit but no errors is strange
                     (flymake-proc--panic
                      :configuration-error
                      (format "Command %s errored, but no diagnostics"
                              command)))
                    (diagnostics
                     (funcall flymake-proc--report-fn diagnostics)))))
        (delete-process proc)
        (setq flymake-proc--processes
              (delq proc flymake-proc--processes))
        (unless (> flymake-log-level 2)
          (kill-buffer (process-get proc 'flymake-proc--output-buffer)))))))

(defun flymake-proc--panic (problem explanation)
  "Tell flymake UI about a fatal PROBLEM with this backend.
May only be called in a dynamic environment where
`flymake-proc--dynamic-report-fn' is bound"
  (flymake-log 0 "%s: %s" problem explanation)
  (if (and (boundp 'flymake-proc--report-fn)
           flymake-proc--report-fn)
      (funcall flymake-proc--report-fn :panic
               :explanation (format "%s: %s" problem explanation))
    (error "Trouble telling flymake-ui about problem %s(%s)"
                   problem explanation)))

(defun flymake-proc-reformat-err-line-patterns-from-compile-el (original-list)
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

(defvar flymake-proc-err-line-patterns ; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
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
     (" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?:[ \t\n]*\\(.+\\)"
      2 4 5 6))
   ;; compilation-error-regexp-alist)
   (flymake-proc-reformat-err-line-patterns-from-compile-el compilation-error-regexp-alist-alist))
  "Patterns for matching error/warning lines.  Each pattern has the form
\(REGEXP FILE-IDX LINE-IDX COL-IDX ERR-TEXT-IDX).
Use `flymake-proc-reformat-err-line-patterns-from-compile-el' to add patterns
from compile.el")

(define-obsolete-variable-alias 'flymake-warning-re 'flymake-proc-diagnostic-type-pred "26.1")
(defvar flymake-proc-diagnostic-type-pred
  'flymake-proc-default-guess
  "Predicate matching against diagnostic text to detect its type.
Takes a single argument, the diagnostic's text and should return
a value suitable for indexing
`flymake-diagnostic-types-alist' (which see). If the returned
value is nil, a type of `error' is assumed. For some backward
compatibility, if a non-nil value is returned that that doesn't
index that alist, a type of `:warning' is assumed.

Instead of a function, it can also be a string, a regular
expression. A match indicates `:warning' type, otherwise
`:error'")

(defun flymake-proc-default-guess (text)
  "Guess if TEXT means a warning, a note or an error."
  (cond ((string-match "^[wW]arning" text)
         :warning)
        ((string-match "^[nN]ote" text)
         :note)
        (t
         :error)))

(defun flymake-proc-get-project-include-dirs-imp (basedir)
  "Include dirs for the project current file belongs to."
  (if (flymake-proc--get-project-include-dirs-from-cache basedir)
      (progn
	(flymake-proc--get-project-include-dirs-from-cache basedir))
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
      (flymake-proc--add-project-include-dirs-to-cache basedir inc-dirs)
      inc-dirs)))

(defvar flymake-proc-get-project-include-dirs-function #'flymake-proc-get-project-include-dirs-imp
  "Function used to get project include dirs, one parameter: basedir name.")

(defun flymake-proc--get-project-include-dirs (basedir)
  (funcall flymake-proc-get-project-include-dirs-function basedir))

(defun flymake-proc--get-system-include-dirs ()
  "System include dirs - from the `INCLUDE' env setting."
  (let* ((includes (getenv "INCLUDE")))
    (if includes (split-string includes path-separator t) nil)))

(defvar flymake-proc--project-include-dirs-cache (make-hash-table :test #'equal))

(defun flymake-proc--get-project-include-dirs-from-cache (base-dir)
  (gethash base-dir flymake-proc--project-include-dirs-cache))

(defun flymake-proc--add-project-include-dirs-to-cache (base-dir include-dirs)
  (puthash base-dir include-dirs flymake-proc--project-include-dirs-cache))

(defun flymake-proc--clear-project-include-dirs-cache ()
  (clrhash flymake-proc--project-include-dirs-cache))

(defun flymake-proc-get-include-dirs (base-dir)
  "Get dirs to use when resolving local file names."
  (let* ((include-dirs (append '(".") (flymake-proc--get-project-include-dirs base-dir) (flymake-proc--get-system-include-dirs))))
    include-dirs))

;; (defun flymake-proc--restore-formatting ()
;;   "Remove any formatting made by flymake."
;;   )

;; (defun flymake-proc--get-program-dir (buffer)
;;   "Get dir to start program in."
;;   (unless (bufferp buffer)
;;     (error "Invalid buffer"))
;;   (with-current-buffer buffer
;;     default-directory))

(defun flymake-proc--safe-delete-file (file-name)
  (when (and file-name (file-exists-p file-name))
    (delete-file file-name)
    (flymake-log 1 "deleted file %s" file-name)))

(defun flymake-proc--safe-delete-directory (dir-name)
  (condition-case nil
      (progn
	(delete-directory dir-name)
	(flymake-log 1 "deleted dir %s" dir-name))
    (error
     (flymake-log 1 "Failed to delete dir %s, error ignored" dir-name))))


(defun flymake-proc-start-syntax-check (report-fn &optional interactive)
  "Start syntax checking for current buffer."
  ;; Interactively, behave as if flymake had invoked us through its
  ;; `flymake-diagnostic-functions' with a suitable ID so flymake can
  ;; clean up consistently
  (interactive (list (flymake-make-report-fn 'flymake-proc-start-syntax-check)
                     t))
  (cond
   ((process-live-p flymake-proc--process)
    (when interactive
      (user-error
       "There's already a flymake process running in this buffer")))
   ((and buffer-file-name
         ;; Since we write temp files in current dir, there's no point
         ;; trying if the directory is read-only (bug#8954).
         (file-writable-p (file-name-directory buffer-file-name))
         (or (not flymake-proc-compilation-prevents-syntax-check)
             (not (flymake-proc--compilation-is-running))))
    (let ((init-f (flymake-proc--get-init-function buffer-file-name)))
      (unless init-f (error "Can find a suitable init function"))
      (flymake-proc--clear-buildfile-cache)
      (flymake-proc--clear-project-include-dirs-cache)

      (let* ((flymake-proc--report-fn report-fn)
             (cleanup-f (flymake-proc--get-cleanup-function buffer-file-name))
             (cmd-and-args (funcall init-f))
             (cmd          (nth 0 cmd-and-args))
             (args         (nth 1 cmd-and-args))
             (dir          (nth 2 cmd-and-args)))
        (cond ((not cmd-and-args)
               (progn
                 (flymake-log 0 "init function %s for %s failed, cleaning up"
                              init-f buffer-file-name)
                 (funcall cleanup-f)))
              (t
               (setq flymake-last-change-time nil)
               (flymake-proc--start-syntax-check-process cmd
                                                         args
                                                         dir)
               t)))))))

(defun flymake-proc--start-syntax-check-process (cmd args dir)
  "Start syntax check process."
  (condition-case err
      (let* ((process
              (let ((default-directory (or dir default-directory)))
                (when dir
                  (flymake-log 3 "starting process on dir %s" dir))
                (make-process :name "flymake-proc"
                              :buffer (current-buffer)
                              :command (cons cmd args)
                              :noquery t
                              :filter 'flymake-proc--process-filter
                              :sentinel 'flymake-proc--process-sentinel))))
        (process-put process 'flymake-proc--output-buffer
                     (generate-new-buffer
                      (format " *flymake output for %s*" (current-buffer))))
        (process-put process 'flymake-proc--report-fn
                     flymake-proc--report-fn)

        (setq-local flymake-proc--process process)
        (push process flymake-proc--processes)

        (setq flymake-is-running t)
        (setq flymake-last-change-time nil)

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
            (cleanup-f        (flymake-proc--get-cleanup-function source-file-name)))
       (flymake-log 0 err-str)
       (funcall cleanup-f)
       (flymake-proc--panic :make-process-error err-str)))))

(defun flymake-proc-stop-all-syntax-checks (&optional reason)
  "Kill all syntax check processes."
  (interactive (list "Interrupted by user"))
  (mapc (lambda (proc)
          (kill-process proc)
          (process-put proc 'flymake-proc--interrupted reason)
          (flymake-log 2 "killed process %d" (process-id proc)))
        flymake-proc--processes))

(defun flymake-proc--compilation-is-running ()
  (and (boundp 'compilation-in-progress)
       compilation-in-progress))

(defun flymake-proc-compile ()
  "Kill all flymake syntax checks, start compilation."
  (interactive)
  (flymake-proc-stop-all-syntax-checks "Stopping for proper compilation")
  (call-interactively 'compile))

;;;; general init-cleanup and helper routines
(defun flymake-proc-create-temp-inplace (file-name prefix)
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

(defun flymake-proc-create-temp-with-folder-structure (file-name _prefix)
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

(defun flymake-proc--delete-temp-directory (dir-name)
  "Attempt to delete temp dir created by `flymake-proc-create-temp-with-folder-structure', do not fail on error."
  (let* ((temp-dir    temporary-file-directory)
	 (suffix      (substring dir-name (1+ (length temp-dir)))))

    (while (> (length suffix) 0)
      (setq suffix (directory-file-name suffix))
      ;;+(flymake-log 0 "suffix=%s" suffix)
      (flymake-proc--safe-delete-directory
       (file-truename (expand-file-name suffix temp-dir)))
      (setq suffix (file-name-directory suffix)))))

(defvar-local flymake-proc--temp-source-file-name nil)
(defvar-local flymake-proc--master-file-name nil)
(defvar-local flymake-proc--temp-master-file-name nil)
(defvar-local flymake-proc--base-dir nil)

(defun flymake-proc-init-create-temp-buffer-copy (create-temp-f)
  "Make a temporary copy of the current buffer, save its name in buffer data and return the name."
  (let*  ((source-file-name       buffer-file-name)
	  (temp-source-file-name  (funcall create-temp-f source-file-name "flymake")))

    (flymake-proc--save-buffer-in-file temp-source-file-name)
    (setq flymake-proc--temp-source-file-name temp-source-file-name)
    temp-source-file-name))

(defun flymake-proc-simple-cleanup ()
  "Do cleanup after `flymake-proc-init-create-temp-buffer-copy'.
Delete temp file."
  (flymake-proc--safe-delete-file flymake-proc--temp-source-file-name)
  (setq flymake-last-change-time nil))

(defun flymake-proc-get-real-file-name (file-name-from-err-msg)
  "Translate file name from error message to \"real\" file name.
Return full-name.  Names are real, not patched."
  (let* ((real-name		nil)
	 (source-file-name	buffer-file-name)
	 (master-file-name	flymake-proc--master-file-name)
	 (temp-source-file-name	flymake-proc--temp-source-file-name)
	 (temp-master-file-name	flymake-proc--temp-master-file-name)
	 (base-dirs
          (list flymake-proc--base-dir
                (file-name-directory source-file-name)
                (if master-file-name (file-name-directory master-file-name))))
	 (files (list (list source-file-name       source-file-name)
                      (list temp-source-file-name  source-file-name)
                      (list master-file-name       master-file-name)
                      (list temp-master-file-name  master-file-name))))

    (when (equal 0 (length file-name-from-err-msg))
      (setq file-name-from-err-msg source-file-name))

    (setq real-name (flymake-proc--get-full-patched-file-name file-name-from-err-msg base-dirs files))
    ;; if real-name is nil, than file name from err msg is none of the files we've patched
    (if (not real-name)
	(setq real-name (flymake-proc--get-full-nonpatched-file-name file-name-from-err-msg base-dirs)))
    (if (not real-name)
	(setq real-name file-name-from-err-msg))
    (setq real-name (flymake-proc--fix-file-name real-name))
    (flymake-log 3 "get-real-file-name: file-name=%s real-name=%s" file-name-from-err-msg real-name)
    real-name))

(defun flymake-proc--get-full-patched-file-name (file-name-from-err-msg base-dirs files)
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
	  (when (and this-dir this-file (flymake-proc--same-files
					 (expand-file-name file-name-from-err-msg this-dir)
					 this-file))
	    (setq real-name this-real-name)))
	(setq file-count (1- file-count)))
      (setq base-dirs-count (1- base-dirs-count)))
    real-name))

(defun flymake-proc--get-full-nonpatched-file-name (file-name-from-err-msg base-dirs)
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

(defun flymake-proc--init-find-buildfile-dir (source-file-name buildfile-name)
  "Find buildfile, store its dir in buffer data and return its dir, if found."
  (let* ((buildfile-dir
          (flymake-proc--find-buildfile buildfile-name
                                        (file-name-directory source-file-name))))
    (if buildfile-dir
        (setq flymake-proc--base-dir buildfile-dir)
      (flymake-log 1 "no buildfile (%s) for %s" buildfile-name source-file-name)
      (flymake-proc--panic
       "NOMK" (format "No buildfile (%s) found for %s"
                      buildfile-name source-file-name)))))

(defun flymake-proc--init-create-temp-source-and-master-buffer-copy (get-incl-dirs-f create-temp-f master-file-masks include-regexp)
  "Find master file (or buffer), create its copy along with a copy of the source file."
  (let* ((source-file-name       buffer-file-name)
	 (temp-source-file-name  (flymake-proc-init-create-temp-buffer-copy create-temp-f))
	 (master-and-temp-master (flymake-proc--create-master-file
				  source-file-name temp-source-file-name
				  get-incl-dirs-f create-temp-f
				  master-file-masks include-regexp)))

    (if (not master-and-temp-master)
	(progn
	  (flymake-log 1 "cannot find master file for %s" source-file-name)
          (flymake-proc--panic "NOMASTER" "")	; NOMASTER
          nil)
      (setq flymake-proc--master-file-name (nth 0 master-and-temp-master))
      (setq flymake-proc--temp-master-file-name (nth 1 master-and-temp-master)))))

(defun flymake-proc-master-cleanup ()
  (flymake-proc-simple-cleanup)
  (flymake-proc--safe-delete-file flymake-proc--temp-master-file-name))

;;;; make-specific init-cleanup routines
(defun flymake-proc--get-syntax-check-program-args (source-file-name base-dir use-relative-base-dir use-relative-source get-cmd-line-f)
  "Create a command line for syntax check using GET-CMD-LINE-F."
  (funcall get-cmd-line-f
           (if use-relative-source
               (file-relative-name source-file-name base-dir)
             source-file-name)
           (if use-relative-base-dir
               (file-relative-name base-dir
                                   (file-name-directory source-file-name))
             base-dir)))

(defun flymake-proc-get-make-cmdline (source base-dir)
  (list "make"
	(list "-s"
	      "-C"
	      base-dir
	      (concat "CHK_SOURCES=" source)
	      "SYNTAX_CHECK_MODE=1"
	      "check-syntax")))

(defun flymake-proc-get-ant-cmdline (source base-dir)
  (list "ant"
	(list "-buildfile"
	      (concat base-dir "/" "build.xml")
	      (concat "-DCHK_SOURCES=" source)
	      "check-syntax")))

(defun flymake-proc-simple-make-init-impl (create-temp-f use-relative-base-dir use-relative-source build-file-name get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
	 (source-file-name   buffer-file-name)
	 (buildfile-dir      (flymake-proc--init-find-buildfile-dir source-file-name build-file-name)))
    (if buildfile-dir
	(let* ((temp-source-file-name  (flymake-proc-init-create-temp-buffer-copy create-temp-f)))
	  (setq args (flymake-proc--get-syntax-check-program-args temp-source-file-name buildfile-dir
							    use-relative-base-dir use-relative-source
							    get-cmdline-f))))
    args))

(defun flymake-proc-simple-make-init ()
  (flymake-proc-simple-make-init-impl 'flymake-proc-create-temp-inplace t t "Makefile" 'flymake-proc-get-make-cmdline))

(defun flymake-proc-master-make-init (get-incl-dirs-f master-file-masks include-regexp)
  "Create make command line for a source file checked via master file compilation."
  (let* ((make-args nil)
	 (temp-master-file-name (flymake-proc--init-create-temp-source-and-master-buffer-copy
                                 get-incl-dirs-f 'flymake-proc-create-temp-inplace
				 master-file-masks include-regexp)))
    (when temp-master-file-name
      (let* ((buildfile-dir (flymake-proc--init-find-buildfile-dir temp-master-file-name "Makefile")))
	(if  buildfile-dir
	    (setq make-args (flymake-proc--get-syntax-check-program-args
			     temp-master-file-name buildfile-dir nil nil 'flymake-proc-get-make-cmdline)))))
    make-args))

(defun flymake-proc--find-make-buildfile (source-dir)
  (flymake-proc--find-buildfile "Makefile" source-dir))

;;;; .h/make specific
(defun flymake-proc-master-make-header-init ()
  (flymake-proc-master-make-init
   'flymake-proc-get-include-dirs
   '("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'")
   "[ \t]*#[ \t]*include[ \t]*\"\\([[:word:]0-9/\\_.]*%s\\)\""))

;;;; .java/make specific
(defun flymake-proc-simple-make-java-init ()
  (flymake-proc-simple-make-init-impl 'flymake-proc-create-temp-with-folder-structure nil nil "Makefile" 'flymake-proc-get-make-cmdline))

(defun flymake-proc-simple-ant-java-init ()
  (flymake-proc-simple-make-init-impl 'flymake-proc-create-temp-with-folder-structure nil nil "build.xml" 'flymake-proc-get-ant-cmdline))

(defun flymake-proc-simple-java-cleanup ()
  "Cleanup after `flymake-proc-simple-make-java-init' -- delete temp file and dirs."
  (flymake-proc--safe-delete-file flymake-proc--temp-source-file-name)
  (when flymake-proc--temp-source-file-name
    (flymake-proc--delete-temp-directory
     (file-name-directory flymake-proc--temp-source-file-name))))

;;;; perl-specific init-cleanup routines
(defun flymake-proc-perl-init ()
  (let* ((temp-file   (flymake-proc-init-create-temp-buffer-copy
                       'flymake-proc-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc " local-file))))

;;;; php-specific init-cleanup routines
(defun flymake-proc-php-init ()
  (let* ((temp-file   (flymake-proc-init-create-temp-buffer-copy
                       'flymake-proc-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local-file "-l"))))

;;;; tex-specific init-cleanup routines
(defun flymake-proc--get-tex-args (file-name)
  ;;(list "latex" (list "-c-style-errors" file-name))
  (list "texify" (list "--pdf" "--tex-option=-c-style-errors" file-name)))

(defun flymake-proc-simple-tex-init ()
  (flymake-proc--get-tex-args (flymake-proc-init-create-temp-buffer-copy 'flymake-proc-create-temp-inplace)))

;; Perhaps there should be a buffer-local variable flymake-master-file
;; that people can set to override this stuff.  Could inherit from
;; the similar AUCTeX variable.
(defun flymake-proc-master-tex-init ()
  (let* ((temp-master-file-name (flymake-proc--init-create-temp-source-and-master-buffer-copy
                                 'flymake-proc-get-include-dirs-dot 'flymake-proc-create-temp-inplace
				 '("\\.tex\\'")
				 "[ \t]*\\in\\(?:put\\|clude\\)[ \t]*{\\(.*%s\\)}")))
    (when temp-master-file-name
      (flymake-proc--get-tex-args temp-master-file-name))))

(defun flymake-proc--get-include-dirs-dot (_base-dir)
  '("."))

;;;; xml-specific init-cleanup routines
(defun flymake-proc-xml-init ()
  (list flymake-proc-xml-program
        (list "val" (flymake-proc-init-create-temp-buffer-copy
                     'flymake-proc-create-temp-inplace))))


;;;; Hook onto flymake-ui
(add-to-list 'flymake-diagnostic-functions
             'flymake-proc-start-syntax-check)


;;;;

(progn
  (define-obsolete-variable-alias 'flymake-compilation-prevents-syntax-check
    'flymake-proc-compilation-prevents-syntax-check "26.1"
    "If non-nil, don't start syntax check if compilation is running.")
  (define-obsolete-variable-alias 'flymake-xml-program
    'flymake-proc-xml-program "26.1"
    "Program to use for XML validation.")
  (define-obsolete-variable-alias 'flymake-master-file-dirs
    'flymake-proc-master-file-dirs "26.1"
    "Dirs where to look for master files.")
  (define-obsolete-variable-alias 'flymake-master-file-count-limit
    'flymake-proc-master-file-count-limit "26.1"
    "Max number of master files to check.")
  (define-obsolete-variable-alias 'flymake-allowed-file-name-masks
    'flymake-proc-allowed-file-name-masks "26.1"
    "Files syntax checking is allowed for.
This is an alist with elements of the form:
  REGEXP INIT [CLEANUP [NAME]]
REGEXP is a regular expression that matches a file name.
INIT is the init function to use.
CLEANUP is the cleanup function to use, default `flymake-simple-cleanup'.
NAME is the file name function to use, default `flymake-get-real-file-name'.")
  (define-obsolete-variable-alias 'flymake-processes
    'flymake-proc--processes "26.1"
    "List of currently active flymake processes.")
  (define-obsolete-function-alias 'flymake-get-file-name-mode-and-masks
    'flymake-proc--get-file-name-mode-and-masks "26.1"
    "Return the corresponding entry from ‘flymake-allowed-file-name-masks’.")
  (define-obsolete-function-alias 'flymake-get-init-function
    'flymake-proc--get-init-function "26.1"
    "Return init function to be used for the file.")
  (define-obsolete-function-alias 'flymake-get-cleanup-function
    'flymake-proc--get-cleanup-function "26.1"
    "Return cleanup function to be used for the file.")
  (define-obsolete-function-alias 'flymake-get-real-file-name-function
    'flymake-proc--get-real-file-name-function "26.1"
    nil)
  (define-obsolete-variable-alias 'flymake-find-buildfile-cache
    'flymake-proc--find-buildfile-cache "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-get-buildfile-from-cache
    'flymake-proc--get-buildfile-from-cache "26.1"
    "Look up DIR-NAME in cache and return its associated value.
If DIR-NAME is not found, return nil.")
  (define-obsolete-function-alias 'flymake-add-buildfile-to-cache
    'flymake-proc--add-buildfile-to-cache "26.1"
    "Associate DIR-NAME with BUILDFILE in the buildfile cache.")
  (define-obsolete-function-alias 'flymake-clear-buildfile-cache
    'flymake-proc--clear-buildfile-cache "26.1"
    "Clear the buildfile cache.")
  (define-obsolete-function-alias 'flymake-find-buildfile
    'flymake-proc--find-buildfile "26.1"
    "Find buildfile starting from current directory.
Buildfile includes Makefile, build.xml etc.
Return its file name if found, or nil if not found.")
  (define-obsolete-function-alias 'flymake-fix-file-name
    'flymake-proc--fix-file-name "26.1"
    "Replace all occurrences of ‘\\’ with ‘/’.")
  (define-obsolete-function-alias 'flymake-same-files
    'flymake-proc--same-files "26.1"
    "Check if FILE-NAME-ONE and FILE-NAME-TWO point to same file.
Return t if so, nil if not.

(fn FILE-NAME-ONE FILE-NAME-TWO)")
  (define-obsolete-variable-alias 'flymake-included-file-name\)
    'flymake-proc--included-file-name\) "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-find-possible-master-files
    'flymake-proc--find-possible-master-files "26.1"
    "Find (by name and location) all possible master files.

Name is specified by FILE-NAME and location is specified by
MASTER-FILE-DIRS.  Master files include .cpp and .c for .h.
Files are searched for starting from the .h directory and max
max-level parent dirs.  File contents are not checked.")
  (define-obsolete-function-alias 'flymake-master-file-compare
    'flymake-proc--master-file-compare "26.1"
    "Compare two files specified by FILE-ONE and FILE-TWO.
This function is used in sort to move most possible file names
to the beginning of the list (File.h -> File.cpp moved to top).")
  (define-obsolete-variable-alias 'flymake-check-file-limit
    'flymake-proc-check-file-limit "26.1"
    "Maximum number of chars to look at when checking possible master file.
Nil means search the entire file.")
  (define-obsolete-function-alias 'flymake-check-patch-master-file-buffer
    'flymake-proc--check-patch-master-file-buffer "26.1"
    "Check if MASTER-FILE-NAME is a master file for SOURCE-FILE-NAME.
If yes, patch a copy of MASTER-FILE-NAME to include PATCHED-SOURCE-FILE-NAME
instead of SOURCE-FILE-NAME.

For example, foo.cpp is a master file if it includes foo.h.

When a buffer for MASTER-FILE-NAME exists, use it as a source
instead of reading master file from disk.")
  (define-obsolete-function-alias 'flymake-replace-region
    'flymake-proc--replace-region "26.1"
    "Replace text in BUFFER in region (BEG END) with REP.")
  (define-obsolete-function-alias 'flymake-read-file-to-temp-buffer
    'flymake-proc--read-file-to-temp-buffer "26.1"
    "Insert contents of FILE-NAME into newly created temp buffer.")
  (define-obsolete-function-alias 'flymake-copy-buffer-to-temp-buffer
    'flymake-proc--copy-buffer-to-temp-buffer "26.1"
    "Copy contents of BUFFER into newly created temp buffer.")
  (define-obsolete-function-alias 'flymake-check-include
    'flymake-proc--check-include "26.1"
    "Check if SOURCE-FILE-NAME can be found in include path.
Return t if it can be found via include path using INC-NAME.")
  (define-obsolete-function-alias 'flymake-find-buffer-for-file
    'flymake-proc--find-buffer-for-file "26.1"
    "Check if there exists a buffer visiting FILE-NAME.
Return t if so, nil if not.")
  (define-obsolete-function-alias 'flymake-create-master-file
    'flymake-proc--create-master-file "26.1"
    "Save SOURCE-FILE-NAME with a different name.
Find master file, patch and save it.")
  (define-obsolete-function-alias 'flymake-save-buffer-in-file
    'flymake-proc--save-buffer-in-file "26.1"
    "Save the entire buffer contents into file FILE-NAME.
Create parent directories as needed.")
  (define-obsolete-function-alias 'flymake-process-filter
    'flymake-proc--process-filter "26.1"
    "Parse OUTPUT and highlight error lines.
It’s flymake process filter.")
  (define-obsolete-function-alias 'flymake-process-sentinel
    'flymake-proc--process-sentinel "26.1"
    "Sentinel for syntax check buffers.")
  (define-obsolete-function-alias 'flymake-post-syntax-check
    'flymake-proc--post-syntax-check "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-reformat-err-line-patterns-from-compile-el
    'flymake-proc-reformat-err-line-patterns-from-compile-el "26.1"
    "Grab error line patterns from ORIGINAL-LIST in compile.el format.
Convert it to flymake internal format.")
  (define-obsolete-variable-alias 'flymake-err-line-patterns
    'flymake-proc-err-line-patterns "26.1"
    "Patterns for matching error/warning lines.  Each pattern has the form
(REGEXP FILE-IDX LINE-IDX COL-IDX ERR-TEXT-IDX).
Use `flymake-reformat-err-line-patterns-from-compile-el' to add patterns
from compile.el")
  (define-obsolete-function-alias 'flymake-parse-line
    'flymake-proc-parse-line "26.1"
    "Parse LINE to see if it is an error or warning.
Return its components if so, nil otherwise.")
  (define-obsolete-function-alias 'flymake-get-project-include-dirs-imp
    'flymake-proc--get-project-include-dirs-imp "26.1"
    "Include dirs for the project current file belongs to.")
  (define-obsolete-variable-alias 'flymake-get-project-include-dirs-function
    'flymake-proc--get-project-include-dirs-function "26.1"
    "Function used to get project include dirs, one parameter: basedir name.")
  (define-obsolete-function-alias 'flymake-get-project-include-dirs
    'flymake-proc--get-project-include-dirs "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-get-system-include-dirs
    'flymake-proc--get-system-include-dirs "26.1"
    "System include dirs - from the ‘INCLUDE’ env setting.")
  (define-obsolete-variable-alias 'flymake-project-include-dirs-cache
    'flymake-proc--project-include-dirs-cache "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-add-project-include-dirs-to-cache
    'flymake-proc--add-project-include-dirs-to-cache "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-clear-project-include-dirs-cache
    'flymake-proc--clear-project-include-dirs-cache "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-get-include-dirs
    'flymake-proc-get-include-dirs "26.1"
    "Get dirs to use when resolving local file names.")
  (define-obsolete-variable-alias 'flymake-restore-formatting
    'flymake-proc--restore-formatting "26.1"
    nil)
  (define-obsolete-variable-alias 'flymake-get-program-dir
    'flymake-proc--get-program-dir "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-safe-delete-file
    'flymake-proc--safe-delete-file "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-safe-delete-directory
    'flymake-proc--safe-delete-directory "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-start-syntax-check
    'flymake-proc-start-syntax-check "26.1"
    "Start syntax checking for current buffer.")
  (define-obsolete-function-alias 'flymake-stop-all-syntax-checks
    'flymake-proc-stop-all-syntax-checks "26.1"
    "Kill all syntax check processes.")
  (define-obsolete-function-alias 'flymake-compilation-is-running
    'flymake-proc--compilation-is-running "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-compile
    'flymake-proc-compile "26.1"
    "Kill all flymake syntax checks, start compilation.")
  (define-obsolete-function-alias 'flymake-create-temp-inplace
    'flymake-proc-create-temp-inplace "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-create-temp-with-folder-structure
    'flymake-proc-create-temp-with-folder-structure "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-delete-temp-directory
    'flymake-proc--delete-temp-directory "26.1"
    "Attempt to delete temp dir created by ‘flymake-create-temp-with-folder-structure’, do not fail on error.")
  (define-obsolete-variable-alias 'flymake-temp-source-file-name
    'flymake-proc--temp-source-file-name "26.1"
    nil)
  (define-obsolete-variable-alias 'flymake-master-file-name
    'flymake-proc--master-file-name "26.1"
    nil)
  (define-obsolete-variable-alias 'flymake-temp-master-file-name
    'flymake-proc--temp-master-file-name "26.1"
    nil)
  (define-obsolete-variable-alias 'flymake-base-dir
    'flymake-proc--base-dir "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-init-create-temp-buffer-copy
    'flymake-proc-init-create-temp-buffer-copy "26.1"
    "Make a temporary copy of the current buffer, save its name in buffer data and return the name.")
  (define-obsolete-function-alias 'flymake-simple-cleanup
    'flymake-proc-simple-cleanup "26.1"
    "Do cleanup after ‘flymake-init-create-temp-buffer-copy’.
Delete temp file.")
  (define-obsolete-function-alias 'flymake-get-real-file-name
    'flymake-proc-get-real-file-name "26.1"
    "Translate file name from error message to \"real\" file name.
Return full-name.  Names are real, not patched.")
  (define-obsolete-function-alias 'flymake-get-full-patched-file-name
    'flymake-proc--get-full-patched-file-name "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-get-full-nonpatched-file-name
    'flymake-proc--get-full-nonpatched-file-name "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-init-find-buildfile-dir
    'flymake-proc--init-find-buildfile-dir "26.1"
    "Find buildfile, store its dir in buffer data and return its dir, if found.")
  (define-obsolete-function-alias 'flymake-init-create-temp-source-and-master-buffer-copy
    'flymake-proc--init-create-temp-source-and-master-buffer-copy "26.1"
    "Find master file (or buffer), create its copy along with a copy of the source file.")
  (define-obsolete-function-alias 'flymake-master-cleanup
    'flymake-proc-master-cleanup "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-get-syntax-check-program-args
    'flymake-proc--get-syntax-check-program-args "26.1"
    "Create a command line for syntax check using GET-CMD-LINE-F.")
  (define-obsolete-function-alias 'flymake-get-make-cmdline
    'flymake-proc-get-make-cmdline "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-get-ant-cmdline
    'flymake-proc-get-ant-cmdline "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-simple-make-init-impl
    'flymake-proc-simple-make-init-impl "26.1"
    "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy.")
  (define-obsolete-function-alias 'flymake-simple-make-init
    'flymake-proc-simple-make-init "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-master-make-init
    'flymake-proc-master-make-init "26.1"
    "Create make command line for a source file checked via master file compilation.")
  (define-obsolete-function-alias 'flymake-find-make-buildfile
    'flymake-proc--find-make-buildfile "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-master-make-header-init
    'flymake-proc-master-make-header-init "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-simple-make-java-init
    'flymake-proc-simple-make-java-init "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-simple-ant-java-init
    'flymake-proc-simple-ant-java-init "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-simple-java-cleanup
    'flymake-proc-simple-java-cleanup "26.1"
    "Cleanup after ‘flymake-simple-make-java-init’ -- delete temp file and dirs.")
  (define-obsolete-function-alias 'flymake-perl-init
    'flymake-proc-perl-init "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-php-init
    'flymake-proc-php-init "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-get-tex-args
    'flymake-proc--get-tex-args "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-simple-tex-init
    'flymake-proc-simple-tex-init "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-master-tex-init
    'flymake-proc-master-tex-init "26.1"
    nil)
  (define-obsolete-function-alias 'flymake-xml-init
    'flymake-proc-xml-init "26.1"
    nil))



(provide 'flymake-proc)
;;; flymake-proc.el ends here
