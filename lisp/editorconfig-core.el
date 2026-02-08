;;; editorconfig-core.el --- EditorConfig Core library  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2026 Free Software Foundation, Inc.

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Package: editorconfig

;; See
;; https://github.com/editorconfig/editorconfig-emacs/graphs/contributors or
;; https://github.com/editorconfig/editorconfig-emacs/blob/master/CONTRIBUTORS
;; for the list of contributors.

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

;; This library is one implementation of EditorConfig Core, which parses
;; .editorconfig files and returns properties for given files.
;; This can be used in place of, for example, editorconfig-core-c.


;; Use from EditorConfig Emacs Plugin

;; Emacs plugin (v0.5 or later) can utilize this implementation.
;; By default, the plugin first search for any EditorConfig executable,
;; and fallback to this library if not found.
;; If you always want to use this library, add following lines to your init.el:

;; Functions

;; editorconfig-core-get-properties-hash (&optional file confname)

;; Get EditorConfig properties for FILE.

;; If FILE is not given, use currently visiting file.
;; Give CONFNAME for basename of config file other than .editorconfig.

;; This functions returns hash table of properties' values.

;;; Code:

(require 'cl-lib)

(require 'editorconfig-core-handle)

(eval-when-compile
  (require 'subr-x))


(defun editorconfig-core--get-handles (dir confname &optional result)
  "Get list of EditorConfig handlers for DIR from CONFNAME.

In the resulting list, the handle for root config file comes first, and the
nearest comes last.
The list may contains nil when no file was found for directories.
RESULT is used internally and normally should not be used."
  (setq dir (expand-file-name dir))
  (let ((handle (editorconfig-core-handle (concat (file-name-as-directory dir)
                                                  confname)))
        (parent (file-name-directory (directory-file-name dir))))
    (if (or (string= parent dir)
            (and handle (editorconfig-core-handle-root-p handle)))
        (cl-remove-if-not #'identity (cons handle result))
      (editorconfig-core--get-handles parent
                                      confname
                                      (cons handle result)))))

(defun editorconfig-core-get-nearest-editorconfig (directory)
  "Return path to .editorconfig file that is closest to DIRECTORY."
  (when-let* ((handle (car (last
                           (editorconfig-core--get-handles directory
                                                           ".editorconfig")))))
    (editorconfig-core-handle-path handle)))

(defun editorconfig-core--hash-merge (into update)
  "Merge two hashes INTO and UPDATE.

This is a destructive function, hash INTO will be modified.
When the same key exists in both two hashes, values of UPDATE takes precedence."
  (maphash (lambda (key value) (puthash key value into)) update)
  into)

(defun editorconfig-core-get-properties-hash (&optional file confname)
  "Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.

This function is almost same as `editorconfig-core-get-properties', but returns
hash object instead."
  (setq file
        (expand-file-name (or file
                              buffer-file-name
                              (error "FILE is not given and `buffer-file-name' is nil"))))
  (setq confname (or confname ".editorconfig"))
  (let ((result (make-hash-table)))
    (dolist (handle (editorconfig-core--get-handles (file-name-directory file)
                                                    confname))
      (editorconfig-core--hash-merge result
                                     (editorconfig-core-handle-get-properties-hash handle
                                                                                   file)))

    ;; Downcase known boolean values
    ;; FIXME: Why not do that in `editorconfig-core-handle--parse-file'?
    (dolist (key '( end_of_line indent_style indent_size insert_final_newline
                    trim_trailing_whitespace charset))
      (when-let* ((val (gethash key result)))
        (puthash key (downcase val) result)))

    result))

(provide 'editorconfig-core)
;;; editorconfig-core.el ends here
