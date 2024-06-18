;;; editorconfig-core.el --- EditorConfig Core library in Emacs Lisp  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2024 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>

;; See
;; https://github.com/editorconfig/editorconfig-emacs/graphs/contributors
;; or the CONTRIBUTORS file for the list of contributors.

;; This file is part of EditorConfig Emacs Plugin.

;; EditorConfig Emacs Plugin is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; EditorConfig Emacs Plugin is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; EditorConfig Emacs Plugin. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library is one implementation of EditorConfig Core, which parses
;; .editorconfig files and returns properties for given files.
;; This can be used in place of, for example, editorconfig-core-c.


;; Use from EditorConfig Emacs Plugin

;; Emacs plugin (v0.5 or later) can utilize this implementation.
;; By default, the plugin first search for any EditorConfig executable,
;; and fallback to this library if not found.
;; If you always want to use this library, add following lines to your init.el:

;;     (setq editorconfig-get-properties-function
;;           'editorconfig-core-get-properties-hash)


;; Functions

;; editorconfig-core-get-properties (&optional file confname confversion)

;; Get EditorConfig properties for FILE.

;; If FILE is not given, use currently visiting file.
;; Give CONFNAME for basename of config file other than .editorconfig.
;; If need to specify config format version, give CONFVERSION.

;; This functions returns alist of properties.  Each element will look like
;; (KEY . VALUE) .


;; editorconfig-core-get-properties-hash (&optional file confname confversion)

;; Get EditorConfig properties for FILE.

;; This function is almost same as `editorconfig-core-get-properties', but
;; returns hash object instead.

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
        (cl-remove-if-not 'identity (cons handle result))
      (editorconfig-core--get-handles parent
                                      confname
                                      (cons handle result)))))

;;;###autoload
(defun editorconfig-core-get-nearest-editorconfig (directory)
  "Return path to .editorconfig file that is closest to DIRECTORY."
  (when-let* ((handle (car (last
                           (editorconfig-core--get-handles directory
                                                           ".editorconfig")))))
    (editorconfig-core-handle-path handle)))

;;;###autoload
(defun editorconfig-core-get-properties (&optional file confname confversion)
  "Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This function returns an alist of properties.  Each element will
look like (KEY . VALUE)."
  (let ((hash (editorconfig-core-get-properties-hash file confname confversion))
        (result nil))
    (maphash (lambda (key value)
               (add-to-list 'result (cons (symbol-name key) value)))
             hash)
    result))

(defun editorconfig-core--hash-merge (into update)
  "Merge two hashes INTO and UPDATE.

This is a destructive function, hash INTO will be modified.
When the same key exists in both two hashes, values of UPDATE takes precedence."
  (maphash (lambda (key value) (puthash key value into)) update)
  into)

;;;###autoload
(defun editorconfig-core-get-properties-hash (&optional file confname confversion)
  "Get EditorConfig properties for FILE.
If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This function is almost same as `editorconfig-core-get-properties', but returns
hash object instead."
  (setq file
        (expand-file-name (or file
                              buffer-file-name
                              (error "FILE is not given and `buffer-file-name' is nil"))))
  (setq confname (or confname ".editorconfig"))
  (setq confversion (or confversion "0.12.0"))
  (let ((result (make-hash-table)))
    (dolist (handle (editorconfig-core--get-handles (file-name-directory file)
                                                    confname))
      (editorconfig-core--hash-merge result
                                     (editorconfig-core-handle-get-properties-hash handle
                                                                                   file)))

    ;; Downcase known boolean values
    (dolist (key '( end_of_line indent_style indent_size insert_final_newline
                    trim_trailing_whitespace charset))
      (when-let* ((val (gethash key result)))
        (puthash key (downcase val) result)))

    ;; Add indent_size property
    (let ((v-indent-size (gethash 'indent_size result))
          (v-indent-style (gethash 'indent_style result)))
      (when (and (not v-indent-size)
                 (string= v-indent-style "tab")
                 ;; If VERSION < 0.9.0, indent_size should have no default value
                 (version<= "0.9.0"
                            confversion))
        (puthash 'indent_size
                 "tab"
                 result)))
    ;; Add tab_width property
    (let ((v-indent-size (gethash 'indent_size result))
          (v-tab-width (gethash 'tab_width result)))
      (when (and v-indent-size
                 (not v-tab-width)
                 (not (string= v-indent-size "tab")))
        (puthash 'tab_width v-indent-size result)))
    ;; Update indent-size property
    (let ((v-indent-size (gethash 'indent_size result))
          (v-tab-width (gethash 'tab_width result)))
      (when (and v-indent-size
                 v-tab-width
                 (string= v-indent-size "tab"))
        (puthash 'indent_size v-tab-width result)))

    result))

(provide 'editorconfig-core)
;;; editorconfig-core.el ends here
