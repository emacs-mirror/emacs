;;; check-declare.el --- Check declare-function statements

;; Copyright (C) 2007-2017 Free Software Foundation, Inc.

;; Author: Glenn Morris <rgm@gnu.org>
;; Keywords: lisp, tools, maint

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

;; The byte-compiler often warns about undefined functions that you
;; know will actually be defined when it matters.  The `declare-function'
;; statement allows you to suppress these warnings.  This package
;; checks that all such statements in a file or directory are accurate.
;; The entry points are `check-declare-file' and `check-declare-directory'.

;; For more information, see Info node `(elisp)Declaring Functions'.

;;; TODO:

;; 1. Warn about functions marked as obsolete, eg
;; password-read-and-add in smime.el.
;; 2. defmethod, defclass argument checking.
;; 3. defclass also defines -p and -child-p.

;;; Code:

(defconst check-declare-warning-buffer "*Check Declarations Warnings*"
  "Name of buffer used to display any `check-declare' warnings.")

(defun check-declare-locate (file basefile)
  "Return the relative name of FILE.
Expands files with a \".c\" or \".m\" extension relative to the Emacs
\"src/\" directory.  Otherwise, `locate-library' searches for FILE.
If that fails, expands FILE relative to BASEFILE's directory part.
The returned file might not exist.  If FILE has an \"ext:\" prefix, so does
the result."
  (let ((ext (string-match "^ext:" file))
        tfile)
    (if ext
        (setq file (substring file 4)))
    (setq file
          (if (member (file-name-extension file) '("c" "m"))
              (expand-file-name file (expand-file-name "src" source-directory))
            (if (setq tfile (locate-library file))
                (progn
                  (setq tfile
                        (replace-regexp-in-string "\\.elc\\'" ".el" tfile))
                  (if (and (not (file-exists-p tfile))
                           (file-exists-p (concat tfile ".gz")))
                      (concat tfile ".gz")
                    tfile))
              (setq tfile (expand-file-name file
                                            (file-name-directory basefile)))
              (if (or (file-exists-p tfile)
                      (string-match "\\.el\\'" tfile))
                  tfile
                (concat tfile ".el")))))
    (setq file (file-relative-name file))
    (if ext (concat "ext:" file)
      file)))

(defun check-declare-scan (file)
  "Scan FILE for `declare-function' calls.
Return a list with elements of the form (FNFILE FN ARGLIST FILEONLY),
where only the first two elements need be present.  This claims that FNFILE
defines FN, with ARGLIST.  FILEONLY non-nil means only check that FNFILE
exists, not that it defines FN.  This is for function definitions that we
don't know how to recognize (e.g. some macros)."
  (let (alist)
    (with-temp-buffer
      (insert-file-contents file)
      ;; FIXME we could theoretically be inside a string.
      (while (re-search-forward "^[ \t]*\\((declare-function\\)[ \t\n]" nil t)
        (let ((pos (match-beginning 1)))
          (goto-char pos)
          (let ((form (ignore-errors (read (current-buffer))))
                len fn formfile fnfile arglist fileonly)
            (if (and
                 ;; Exclude element of byte-compile-initial-macro-environment.
                 (or (listp (cdr form)) (setq form nil))
                 (> (setq len (length form)) 2)
                 (< len 6)
                 (setq formfile (nth 2 form))
                 (symbolp (setq fn (cadr form)))
                 (setq fn (symbol-name fn)) ; later we use as a search string
                 (stringp formfile)
                 (setq fnfile (check-declare-locate formfile file))
                 ;; Use t to distinguish unspecified arglist from empty one.
                 (or (eq t (setq arglist (if (> len 3)
                                             (nth 3 form)
                                           t)))
                     (listp arglist))
                 (symbolp (setq fileonly (nth 4 form))))
                (setq alist (cons (list fnfile fn arglist fileonly) alist))
              (when form
                (check-declare-warn file (or fn "unknown function")
                                    (if (stringp formfile) formfile
                                      "unknown file")
                                    "Malformed declaration"
                                    (line-number-at-pos pos))))))))
    alist))

(autoload 'byte-compile-arglist-signature "bytecomp")

(defgroup check-declare nil
  "Check declare-function statements."
  :group 'tools)

(defcustom check-declare-ext-errors nil
  "When non-nil, warn about functions not found in :ext."
  :version "25.1"
  :type 'boolean)

(defun check-declare-verify (fnfile fnlist)
  "Check that FNFILE contains function definitions matching FNLIST.
Each element of FNLIST has the form (FILE FN ARGLIST FILEONLY), where
only the first two elements need be present.  This means FILE claimed FN
was defined in FNFILE with the specified ARGLIST.  FILEONLY non-nil means
to only check that FNFILE exists, not that it actually defines FN.

Returns nil if all claims are found to be true, otherwise a list
of errors with elements of the form \(FILE FN TYPE), where TYPE
is a string giving details of the error."
  (let ((cflag (member (file-name-extension fnfile) '("c" "m")))
        (ext (string-match "^ext:" fnfile))
        re fn sig siglist arglist type errlist minargs maxargs)
    (if ext
        (setq fnfile (substring fnfile 4)))
    (if (file-regular-p fnfile)
        (with-temp-buffer
          (insert-file-contents fnfile)
          ;; defsubst's don't _have_ to be known at compile time.
          (setq re (format (if cflag
                               "^[ \t]*\\(DEFUN\\)[ \t]*([ \t]*\"%s\""
                             "^[ \t]*(\\(fset[ \t]+'\\|\
cl-def\\(?:generic\\|method\\)\\|\
def\\(?:un\\|subst\\|foo\\|method\\|class\\|\
ine-\\(?:derived\\|generic\\|\\(?:global\\(?:ized\\)?-\\)?minor\\)-mode\\|\
\\(?:ine-obsolete-function-\\)?alias[ \t]+'\\|\
ine-overloadable-function\\)\\)\
[ \t]*%s\\([ \t;]+\\|$\\)")
                           (regexp-opt (mapcar 'cadr fnlist) t)))
          (while (re-search-forward re nil t)
            (skip-chars-forward " \t\n")
            (setq fn (match-string 2)
                  type (match-string 1)
                  ;; (min . max) for a fixed number of arguments, or
                  ;; arglists with optional elements.
                  ;; (min) for arglists with &rest.
                  ;; sig = 'err means we could not find an arglist.
                  sig (cond (cflag
                             (or
                              (when (search-forward "," nil t 3)
                                (skip-chars-forward " \t\n")
                                ;; Assuming minargs and maxargs on same line.
                                (when (looking-at "\\([0-9]+\\)[ \t]*,[ \t]*\
\\([0-9]+\\|MANY\\|UNEVALLED\\)")
                                  (setq minargs (string-to-number
                                                 (match-string 1))
                                        maxargs (match-string 2))
                                  (cons minargs (unless (string-match "[^0-9]"
                                                                      maxargs)
                                                 (string-to-number
                                                  maxargs)))))
                              'err))
                            ((string-match
                              "\\`define-\\(derived\\|generic\\)-mode\\'"
                              type)
                             '(0 . 0))
                            ((string-match
                              "\\`define\\(-global\\(ized\\)?\\)?-minor-mode\\'"
                              type)
                             '(0 . 1))
                            ;; Prompt to update.
                            ((string-match
                              "\\`define-obsolete-function-alias\\>"
                              type)
                             'obsolete)
                            ;; Can't easily check arguments in these cases.
                            ((string-match "\\`\\(def\\(alias\\|class\\)\\|\
fset\\|\\(?:cl-\\)?defmethod\\)\\>" type)
                             t)
                            ((looking-at "\\((\\|nil\\)")
                             (byte-compile-arglist-signature
                              (read (current-buffer))))
                            (t
                             'err))
                  ;; alist of functions and arglist signatures.
                  siglist (cons (cons fn sig) siglist)))))
    (dolist (e fnlist)
      (setq arglist (nth 2 e)
            type
            (if (not re)
                (when (or check-declare-ext-errors (not ext))
                  "file not found")
              (if (not (setq sig (assoc (cadr e) siglist)))
                  (unless (nth 3 e)     ; fileonly
                    "function not found")
                (setq sig (cdr sig))
                (cond ((eq sig 'obsolete) ; check even when no arglist specified
                       "obsolete alias")
                      ;; arglist t means no arglist specified, as
                      ;; opposed to an empty arglist.
                      ((eq arglist t) nil)
                      ((eq sig t) nil) ; eg defalias - can't check arguments
                      ((eq sig 'err)
                       "arglist not found") ; internal error
                      ((not (equal (byte-compile-arglist-signature
                                    arglist)
                                   sig))
                       "arglist mismatch")))))
      (when type
        (setq errlist (cons (list (car e) (cadr e) type) errlist))))
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

(defun check-declare-warn (file fn fnfile type &optional line)
  "Warn that FILE made a false claim about FN in FNFILE.
TYPE is a string giving the nature of the error.
Optional LINE is the claim's line number; otherwise, search for the claim.
Display warning in `check-declare-warning-buffer'."
  (let ((warning-prefix-function
         (lambda (level entry)
	   (insert (format "%s:%d:" (file-relative-name file) (or line 0)))
           entry))
        (warning-fill-prefix "    "))
    (unless line
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (when (and (not line)
                   (re-search-forward
                    (format "(declare-function[ \t\n]+%s" fn) nil t))
          (goto-char (match-beginning 0))
          (setq line (line-number-at-pos)))))
    (display-warning 'check-declare
                     (format-message "said `%s' was defined in %s: %s"
                                     fn (file-relative-name fnfile) type)
                     nil check-declare-warning-buffer)))

(declare-function compilation-forget-errors "compile" ())

(defun check-declare-files (&rest files)
  "Check veracity of all `declare-function' statements in FILES.
Return a list of any errors found."
  (if (get-buffer check-declare-warning-buffer)
      (kill-buffer check-declare-warning-buffer))
  (let ((buf (get-buffer-create check-declare-warning-buffer))
        alist err errlist)
    (with-current-buffer buf
      (unless (derived-mode-p 'compilation-mode)
        (compilation-mode))
      (setq mode-line-process
            '(:propertize ":run" face compilation-mode-line-run))
      (let ((inhibit-read-only t))
        (insert "\f\n"))
      (compilation-forget-errors))
    (dolist (file files)
      (setq alist (cons (cons file (check-declare-scan file)) alist)))
    ;; Sort so that things are ordered by the files supposed to
    ;; contain the defuns.
    (dolist (e (check-declare-sort alist))
      (if (setq err (check-declare-verify (car e) (cdr e)))
          (setq errlist (cons (cons (car e) err) errlist))))
    (setq errlist (nreverse errlist))
    ;; Sort back again so that errors are ordered by the files
    ;; containing the declare-function statements.
    (dolist (e (check-declare-sort errlist))
      (dolist (f (cdr e))
        (check-declare-warn (car e) (cadr f) (car f) (nth 2 f))))
    (with-current-buffer buf
      (setq mode-line-process
            '(:propertize ":exit" face compilation-mode-line-run))
      (force-mode-line-update))
    errlist))

;;;###autoload
(defun check-declare-file (file)
  "Check veracity of all `declare-function' statements in FILE.
See `check-declare-directory' for more information."
  (interactive "fFile to check: ")
  (or (file-exists-p file)
      (error "File `%s' not found" file))
  (check-declare-files file))

;;;###autoload
(defun check-declare-directory (root)
  "Check veracity of all `declare-function' statements under directory ROOT.
Returns non-nil if any false statements are found."
  (interactive "DDirectory to check: ")
  (setq root (directory-file-name (file-relative-name root)))
  (or (file-directory-p root)
      (error "Directory `%s' not found" root))
  (let ((files (process-lines find-program root
                              "-name" "*.el"
                              "-exec" grep-program
                              "-l" "^[ \t]*(declare-function" "{}" "+")))
    (when files
      (apply #'check-declare-files files))))

(provide 'check-declare)

;;; check-declare.el ends here.
