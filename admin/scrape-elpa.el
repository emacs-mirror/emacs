;;; scrape-elpa.el --- Collect ELPA package suggestions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2026  Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines an administrative command to update the
;; `package-autosuggest' database.

;;; Code:

(require 'rx)

(defun scrape-elpa--safe-eval (exp &optional vars)
  "Manually evaluate EXP without potentially dangerous side-effects.
The optional argument VARS may be an alist mapping symbols to values,
used when evaluating variables.  The evaluation function is not meant to
be comprehensive, but just to handle the kinds of expressions that
`scrape-elpa' expects to encounter."
  (pcase-exhaustive exp
    ;; special handling for macros
    (`(rx . ,body) (rx-to-string `(: . ,body) t))
    ;; quoting and quasi-quoting
    (`',x x)
    (`(purecopy ,x) x)
    ((and (guard (eq '\` (car-safe exp))) (let `(,car . ,cdr) (cadr exp)))
     (cons
      (if (eq (car-safe car) '\,) (scrape-elpa--safe-eval (cadr car) vars) car)
      (if (eq (car-safe cdr) '\,) (scrape-elpa--safe-eval (cadr cdr) vars) cdr)))
    ;; allow calling `side-effect-free' functions
    (`(,(and (pred symbolp) (pred (get _ 'side-effect-free)) fn) . ,args)
     (apply fn (mapcar #'scrape-elpa--safe-eval args)))
    ;; self-evaluating forms
    ((pred macroexp-const-p) exp)
    ;; variable evaluation
    ((pred symbolp)
     (let ((ent (assq exp vars)))
       (if ent (cdr ent) (signal 'void-variable exp))))))

(defun scrape-elpa (&rest directories)
  "Scrape autoload files in DIRECTORIES for package suggestions.
This file will automatically update \"package-autosuggest.eld\", but not
save it.  You should invoke this command with built GNU ELPA and NonGNU
ELPA checkouts (i.e. having run \"make autoloads\" in both directories).
Please review the results before updating the autosuggest database!"
  (interactive (completing-read-multiple
                "ELPA directories to scrape: "
                #'completion-file-name-table
                #'file-directory-p))
  (with-current-buffer
      (find-file (expand-file-name "package-autosuggest.eld" data-directory))
    (erase-buffer)
    (lisp-data-mode)
    (insert ";; The contents of this file are loaded into `package--autosuggest-database'.
;; were automatically generate by scraping ELPA for auto-loaded
;; code using the `scrape-elpa' command from admin/scrape-elpa.el.  Please do not
;; update this file manually!

")
    (fill-paragraph)
    (insert "(")
    (let ((standard-output (current-buffer)))
      (dolist-with-progress-reporter
          (file (mapcan
                 (lambda (dir)
                   (directory-files-recursively
                    dir "-autoloads\\.el\\'"))
                 directories))
          "Scraping files..."
        (and-let* (((string-match "/\\([^/]+?\\)-autoloads\\.el\\'" file))
                   (pkg (intern (match-string 1 file)))
                   (vars (list '(#:nihil)))
                   (inhibit-message t))
          (with-temp-buffer
            (insert-file-contents file)
            (condition-case nil
                (while t
                  (dolist (exp (macroexp-unprogn (read (current-buffer))))
                    (pcase exp
                      (`(defconst ,(and (pred symbolp) var) ,val . ,_)
                       (catch 'ignore
                         (push
                          (cons var (condition-case err
                                        (scrape-elpa--safe-eval val vars)
                                      (t (message "Failed to evaluate %S: %S in %S" exp err vars)
                                         (throw 'ignore nil))))
                          vars)))
                      (`(add-to-list
                         ',(and (or 'interpreter-mode-alist
                                    'magic-mode-alist
                                    'auto-mode-alist)
                                variable)
                         ,(let `(,(and (pred stringp) regexp) .
                                 ,(and (pred symbolp) mode))
                            (condition-case err
                                (scrape-elpa--safe-eval _ vars)
                              (t (message "Failed to evaluate %S: %S in %S" exp err vars)
                                 nil))))
                       (terpri)
                       (prin1 (append (list pkg variable regexp)
                                      (and (not (eq pkg mode)) (list mode)))))
                      (`(add-to-list
                         ',(or 'interpreter-mode-alist
                               'magic-mode-alist
                               'auto-mode-alist)
                         _)
                       (_ (message "Skipped over %S" exp))))))
              (end-of-file nil))))))
    (insert "\n)\n")))

(provide 'scrape-elpa)
;;; scrape-elpa.el ends here
