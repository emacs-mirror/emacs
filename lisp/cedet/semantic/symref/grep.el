;;; semantic/symref/grep.el --- Symref implementation using find/grep

;; Copyright (C) 2008-2017 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; Implement the symref tool API using the external tools find/grep.
;;
;; The symref GREP tool uses grep in a project to find symbol references.
;; This is a lowest-common-denominator tool with sucky performance that
;; can be used in small projects to find symbol references.

(require 'semantic/symref)
(require 'grep)

;;; Code:

;;; GREP
;;;###autoload
(defclass semantic-symref-tool-grep (semantic-symref-tool-baseclass)
  (
   )
  "A symref tool implementation using grep.
This tool uses EDE to find he root of the project, then executes
find-grep in the project.  The output is parsed for hits
and those hits returned.")

(defvar semantic-symref-filepattern-alist
  '((c-mode "*.[ch]")
    (c++-mode "*.[chCH]" "*.[ch]pp" "*.cc" "*.hh")
    (html-mode "*.s?html" "*.php")
    (ruby-mode "*.r[bu]" "*.rake" "*.gemspec" "*.erb" "*.haml"
               "Rakefile" "Thorfile" "Capfile" "Guardfile" "Vagrantfile")
    (perl-mode "*.pl" "*.PL")
    (cperl-mode "*.pl" "*.PL")
    (lisp-interaction-mode "*.el" "*.ede" ".emacs" "_emacs")
    )
  "List of major modes and file extension pattern.
See find -name man page for format.")

(defun semantic-symref-derive-find-filepatterns (&optional mode)
  ;; FIXME: This should be moved to grep.el, where it could be used
  ;; for "C-u M-x grep" as well.
  "Derive a list of file patterns for the current buffer.
Looks first in `semantic-symref-filepattern-alist'.  If it is not
there, it then looks in `auto-mode-alist', and attempts to derive something
from that.
Optional argument MODE specifies the `major-mode' to test."
  ;; First, try the filepattern alist.
  (let* ((mode (or mode major-mode))
	 (pat (cdr (assoc mode semantic-symref-filepattern-alist))))
    (when (not pat)
      ;; No hit, try auto-mode-alist.
      (dolist (X auto-mode-alist)
	(when (and (eq (cdr X) mode)
                   ;; Only take in simple patterns, so try to convert this one.
                   (string-match "\\\\\\.\\([^\\'>]+\\)\\\\'" (car X)))
          (push (concat "*." (match-string 1 (car X))) pat))))
    ;; Convert the list into some find-flags.
    (if (null pat)
        (error "Customize `semantic-symref-filepattern-alist' for %S"
               major-mode)
      (let ((args `("-name" ,(car pat))))
        (if (null (cdr args))
            args
          `("(" ,@args
            ,@(mapcan (lambda (s) `("-o" "-name" ,s)) pat)
            ")"))))))

(defvar grepflags)
(defvar greppattern)

(defvar semantic-symref-grep-expand-keywords
  (condition-case nil
      (let* ((kw (copy-alist grep-expand-keywords))
	     (C (assoc "<C>" kw))
	     (R (assoc "<R>" kw)))
	(setcdr C 'grepflags)
	(setcdr R 'greppattern)
	kw)
    (error nil))
  "Grep expand keywords used when expanding templates for symref.")

(defun semantic-symref-grep-use-template (rootdir filepattern flags pattern)
  "Use the grep template expand feature to create a grep command.
ROOTDIR is the root location to run the `find' from.
FILEPATTERN is a string representing find flags for searching file patterns.
GREPFLAGS are flags passed to grep, such as -n or -l.
GREPPATTERN is the pattern used by grep."
  ;; We have grep-compute-defaults.  Let's use it.
  (grep-compute-defaults)
  (let* ((grepflags flags)
         (greppattern pattern)
         (grep-expand-keywords semantic-symref-grep-expand-keywords)
	 (cmd (grep-expand-template
               (if (memq system-type '(windows-nt ms-dos))
                   ;; grep-find uses '--color=always' on MS-Windows
                   ;; because it wants the colorized output, to show
                   ;; it to the user.  By contrast, here we don't show
                   ;; the output, and the SGR escapes get in the way
                   ;; of parsing the output.
                   (replace-regexp-in-string "--color=always" ""
                                             grep-find-template t t)
                 grep-find-template)
               greppattern
               filepattern
               rootdir)))
    ;; http://debbugs.gnu.org/20719
    (when (string-match "find \\(\\.\\)" cmd)
      (setq cmd (replace-match rootdir t t cmd 1)))
    ;;(message "New command: %s" cmd)
    cmd))

(defcustom semantic-symref-grep-shell shell-file-name
  "The shell command to use for executing find/grep.
This shell should support pipe redirect syntax."
  :group 'semantic
  :type 'string)

(cl-defmethod semantic-symref-perform-search ((tool semantic-symref-tool-grep))
  "Perform a search with Grep."
  ;; Grep doesn't support some types of searches.
  (let ((st (oref tool :searchtype)))
    (when (not (memq st '(symbol regexp)))
      (error "Symref impl GREP does not support searchtype of %s" st))
    )
  ;; Find the root of the project, and do a find-grep...
  (let* (;; Find the file patterns to use.
	 (rootdir (semantic-symref-calculate-rootdir))
	 (filepatterns (semantic-symref-derive-find-filepatterns))
         (filepattern (mapconcat #'shell-quote-argument filepatterns " "))
	 ;; Grep based flags.
	 (grepflags (cond ((eq (oref tool :resulttype) 'file)
                           "-l ")
                          ((eq (oref tool :searchtype) 'regexp)
                           "-nE ")
                          (t "-n ")))
	 (greppat (shell-quote-argument
                   (cond ((eq (oref tool :searchtype) 'regexp)
                          (oref tool searchfor))
                         (t
                          ;; Can't use the word boundaries: Grep
                          ;; doesn't always agrees with the language
                          ;; syntax on those.
                          (format "\\(^\\|\\W\\)%s\\(\\W\\|$\\)"
                                  (oref tool searchfor))))))
	 ;; Misc
	 (b (get-buffer-create "*Semantic SymRef*"))
	 (ans nil)
	 )

    (with-current-buffer b
      (erase-buffer)
      (setq default-directory rootdir)

      (if (not (fboundp 'grep-compute-defaults))

	  ;; find . -type f -print0 | xargs -0 -e grep -nH -e
	  ;; Note : I removed -e as it is not posix, nor necessary it seems.

	  (let ((cmd (concat "find " default-directory " -type f " filepattern " -print0 "
			     "| xargs -0 grep -H " grepflags "-e " greppat)))
	    ;;(message "Old command: %s" cmd)
	    (call-process semantic-symref-grep-shell nil b nil
                          shell-command-switch cmd)
	    )
	(let ((cmd (semantic-symref-grep-use-template rootdir filepattern grepflags greppat)))
	  (call-process semantic-symref-grep-shell nil b nil
                        shell-command-switch cmd))
	))
    (setq ans (semantic-symref-parse-tool-output tool b))
    ;; Return the answer
    ans))

(cl-defmethod semantic-symref-parse-tool-output-one-line ((tool semantic-symref-tool-grep))
  "Parse one line of grep output, and return it as a match list.
Moves cursor to end of the match."
  (pcase-let
      ((`(,grep-re ,file-group ,line-group . ,_) (car (grep-regexp-alist))))
    (cond ((eq (oref tool :resulttype) 'file)
	   ;; Search for files
	   (when (re-search-forward "^\\([^\n]+\\)$" nil t)
	     (match-string 1)))
          ((eq (oref tool :resulttype) 'line-and-text)
           (when (re-search-forward grep-re nil t)
             (list (string-to-number (match-string line-group))
                   (match-string file-group)
                   (buffer-substring-no-properties (point) (line-end-position)))))
	  (t
	   (when (re-search-forward grep-re nil t)
	     (cons (string-to-number (match-string line-group))
		   (match-string file-group))
	     )))))

(provide 'semantic/symref/grep)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/symref/grep"
;; End:

;;; semantic/symref/grep.el ends here
