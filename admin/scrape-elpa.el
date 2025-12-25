;;; scrape-elpa.el --- Collect ELPA package suggestions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

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
    (insert ";; The contents of this file are loaded into `package-autosuggest-database'
;; and were automatically generate by scraping ELPA for auto-loaded
;; code using the `scrape-elpa' command.  Please avoid updating this
;; file manually!

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
                   (inhibit-message t))
          (with-temp-buffer
            (insert-file-contents file)
            (condition-case nil
                (while t
                  (dolist (exp (macroexp-unprogn (read (current-buffer))))
                    (pcase exp
                      (`(add-to-list
                         ',(and (or 'interpreter-mode-alist
                                    'magic-mode-alist
                                    'auto-mode-alist)
                                variable)
                         '(,(and (pred stringp) regexp) .
                           ,(and (pred symbolp) mode)))
                       (terpri)
                       (prin1 (append (list pkg variable regexp)
                                      (and (not (eq pkg mode)) (list mode))))))))
              (end-of-file nil))))))
    (insert "\n)\n")
    (save-mark-and-excursion
      (mark-sexp -1)
      (sort-lines nil (region-beginning) (region-end)))))

(provide 'scrape-elpa)
;;; scrape-elpa.el ends here
