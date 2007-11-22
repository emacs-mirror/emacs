;;; check-declare.el --- Check declare-function statements

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Glenn Morris <rgm@gnu.org>
;; Keywords: lisp, tools, maint

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The byte-compiler often warns about undefined functions that you
;; know will actually be defined when it matters.  The `declare-function'
;; statement allows you to suppress these warnings.  This package
;; checks that all such statements in a file or directory are accurate.
;; The entry points are `check-declare-file' and `check-declare-directory'.

;;; TODO:

;; 1. Handle defstructs (eg uniquify-item-base in desktop.el).

;;; Code:

(defconst check-declare-warning-buffer "*Check Declarations Warnings*"
  "Name of buffer used to display any `check-declare' warnings.")

(defun check-declare-scan (file)
  "Scan FILE for `declare-function' calls.
Return a list with elements of the form (FNFILE FN ARGLIST), where
ARGLIST may be absent.  This claims that FNFILE defines FN, with ARGLIST."
  (let ((m (format "Scanning %s..." file))
        alist fnfile fn)
    (message "%s" m)
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward
              "^[ \t]*(declare-function[ \t]+\\(\\S-+\\)[ \t]+\
\"\\(\\S-+\\)\"" nil t)
        (setq fn (match-string 1)
              fnfile (match-string 2))
        (or (file-name-absolute-p fnfile)
            (setq fnfile
                  (expand-file-name fnfile
                                    ;; .c files are assumed to be
                                    ;; relative to the Emacs src/ directory.
                                    (if (string-equal
                                         "c" (file-name-extension fnfile))
                                        (expand-file-name "src"
                                                          source-directory)
                                      (file-name-directory file)))))
        (setq alist (cons
                     (list fnfile fn
                           (progn
                             (skip-chars-forward " \t\n")
                             ;; Use `t' to distinguish no arglist
                             ;; specified from an empty one.
                             (if (looking-at "\\((\\|nil\\)")
                                 (read (current-buffer))
                               t)))
                     alist))))
    (message "%sdone" m)
    alist))

(autoload 'byte-compile-arglist-signature "bytecomp")

(defun check-declare-verify (fnfile fnlist)
  "Check that FNFILE contains function definitions matching FNLIST.
Each element of FNLIST has the form (FILE FN ARGLIST), where
ARGLIST is optional.  This means FILE claimed FN was defined in
FNFILE with the specified ARGLIST.  Returns nil if all claims are
found to be true, otherwise a list of errors with elements of the form
\(FILE FN TYPE), where TYPE is a string giving details of the error."
  (let ((m (format "Checking %s..." fnfile))
        (cflag (string-equal "c" (file-name-extension fnfile)))
        re fn sig siglist arglist type errlist minargs maxargs)
    (message "%s" m)
    (or cflag
        (file-exists-p fnfile)
        (setq fnfile (concat fnfile ".el")))
    (if (file-exists-p fnfile)
        (with-temp-buffer
          (insert-file-contents fnfile)
          ;; defsubst's don't _have_ to be known at compile time.
          (setq re (format (if cflag
                               "^[ \t]*\\(DEFUN\\)[ \t]*([ \t]*\"%s\""
                             "^[ \t]*(\\(def\\(?:un\\|subst\\|\
ine-derived-mode\\|ine-minor-mode\\|alias[ \t]+'\\)\\)\
\[ \t]*%s\\([ \t;]+\\|$\\)")
                           (regexp-opt (mapcar 'cadr fnlist) t)))
          (while (re-search-forward re nil t)
            (skip-chars-forward " \t\n")
            (setq fn (match-string 2)
                  ;; (min . max) for a fixed number of arguments, or
                  ;; arglists with optional elements.
                  ;; (min) for arglists with &rest.
                  sig (cond (cflag
                             (re-search-forward "," nil t 3)
                             (skip-chars-forward " \t\n")
                             ;; Assuming minargs and maxargs on same line.
                             (when (looking-at "\\([0-9]+\\)[ \t]*,[ \t]*\
\\([0-9]+\\|MANY\\|UNEVALLED\\)")
                               (setq minargs (string-to-number (match-string 1))
                                     maxargs (match-string 2))
                               (cons minargs (unless (string-match "[^0-9]"
                                                                   maxargs)
                                               (string-to-number maxargs)))))
                            ((string-equal (match-string 1)
                                           "define-derived-mode")
                             '(0 . 0))
                            ((string-equal (match-string 1)
                                           "define-minor-mode")
                             '(0 . 1))
                            ;; Can't easily check alias arguments.
                            ((string-equal (match-string 1)
                                           "defalias")
                             t)
                            (t
                             (if (looking-at "\\((\\|nil\\)")
                                 (byte-compile-arglist-signature
                                  (read (current-buffer))))))
                  ;; alist of functions and arglist signatures.
                  siglist (cons (cons fn sig) siglist)))))
    (dolist (e fnlist)
      (setq arglist (nth 2 e)
            type
            (if re                   ; re non-nil means found a file
                (if (setq sig (assoc (cadr e) siglist))
                    ;; Recall we use t to mean no arglist specified,
                    ;; to distinguish from an empty arglist.
                    (unless (or (eq arglist t)
                                (eq sig t))
                      (unless (equal (byte-compile-arglist-signature arglist)
                                     (cdr sig))
                        "arglist mismatch"))
                  "function not found")
              "file not found"))
      (when type
        (setq errlist (cons (list (car e) (cadr e) type) errlist))))
    (message "%s%s" m (if errlist "problems found" "OK"))
    errlist))

(defun check-declare-sort (alist)
  "Sort a list with elements FILE (FNFILE ...).
Returned list has elements FNFILE (FILE ...)."
  (let (file fnfile rest sort a)
    (dolist (e alist)
      (setq file (car e))
      (dolist (f (cdr e))
        (setq fnfile (car f)
              rest (cdr f))
        (if (setq a (assoc fnfile sort))
            (setcdr a (append (cdr a) (list (cons file rest))))
          (setq sort (cons (list fnfile (cons file rest)) sort)))))
    sort))

(defun check-declare-warn (file fn fnfile type)
  "Warn that FILE made a false claim about FN in FNFILE.
TYPE is a string giving the nature of the error.  Warning is displayed in
`check-declare-warning-buffer'."
  (display-warning 'check-declare
                   (format "%s said `%s' was defined in %s: %s"
                           (file-name-nondirectory file) fn
                           (file-name-nondirectory fnfile)
                           type)
                   nil check-declare-warning-buffer))

(defun check-declare-files (&rest files)
  "Check veracity of all `declare-function' statements in FILES.
Return a list of any errors found."
  (let (alist err errlist)
    (dolist (file files)
      (setq alist (cons (cons file (check-declare-scan file)) alist)))
    ;; Sort so that things are ordered by the files supposed to
    ;; contain the defuns.
    (dolist (e (check-declare-sort alist))
      (if (setq err (check-declare-verify (car e) (cdr e)))
          (setq errlist (cons (cons (car e) err) errlist))))
    (if (get-buffer check-declare-warning-buffer)
        (kill-buffer check-declare-warning-buffer))
    ;; Sort back again so that errors are ordered by the files
    ;; containing the declare-function statements.
    (dolist (e (check-declare-sort errlist))
        (dolist (f (cdr e))
          (check-declare-warn (car e) (cadr f) (car f) (nth 2 f))))
    errlist))

;;;###autoload
(defun check-declare-file (file)
  "Check veracity of all `declare-function' statements in FILE.
See `check-declare-directory' for more information."
  (interactive "fFile to check: ")
  (or (file-exists-p file)
      (error "File `%s' not found" file))
  (let ((m (format "Checking %s..." file))
        errlist)
    (message "%s" m)
    (setq errlist (check-declare-files file))
    (message "%s%s" m (if errlist "problems found" "OK"))
    errlist))

;;;###autoload
(defun check-declare-directory (root)
  "Check veracity of all `declare-function' statements under directory ROOT.
Returns non-nil if any false statements are found.  For this to
work correctly, the statements must adhere to the format
described in the documentation of `declare-function'."
  (interactive "DDirectory to check: ")
  (or (file-directory-p (setq root (expand-file-name root)))
      (error "Directory `%s' not found" root))
  (let ((m "Checking `declare-function' statements...")
        (m2 "Finding files with declarations...")
        errlist files)
    (message "%s" m)
    (message "%s" m2)
    (setq files (process-lines "find" root "-name" "*.el"
                                 "-exec" "grep" "-l"
                                 "^[ 	]*(declare-function" "{}" ";"))
    (message "%s%d found" m2 (length files))
    (when files
      (setq errlist (apply 'check-declare-files files))
      (message "%s%s" m (if errlist "problems found" "OK"))
      errlist)))

(provide 'check-declare)

;; arch-tag: a4d6cdc4-deb7-4502-b327-0e4ef3d82d96
;;; check-declare.el ends here.
