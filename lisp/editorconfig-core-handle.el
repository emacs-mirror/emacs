;;; editorconfig-core-handle.el --- Handle Class for EditorConfig File  -*- lexical-binding: t -*-

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

;; Handle structures for EditorConfig config file.  This library is used
;; internally from editorconfig-core.el .

;;; Code:

(require 'cl-lib)

(require 'editorconfig-fnmatch)

(defvar editorconfig-core-handle--cache-hash
  (make-hash-table :test 'equal)
  "Hash of EditorConfig filename and its `editorconfig-core-handle' instance.")

(cl-defstruct editorconfig-core-handle-section
  "Structure representing one section in a .editorconfig file.

Slots:

`name'
  String of section name (glob string).

`props'
  Alist of properties: (KEY . VALUE)."
  (name nil)
  (props nil))

(defun editorconfig-core-handle-section-get-properties (section file dir)
  "Return properties alist when SECTION name match FILE.

DIR should be the directory where .editorconfig file which has SECTION lives.
IF not match, return nil."
  (when (editorconfig-core-handle--fnmatch-p
         file (editorconfig-core-handle-section-name section) dir)
    (editorconfig-core-handle-section-props section)))

(cl-defstruct editorconfig-core-handle
  "Structure representing an .editorconfig file.

Slots:
`top-props'
  Alist of top properties like ((\"root\" . \"true\"))

`sections'
  List of `editorconfig-core-handle-section' structure objects.

`mtime'
  Last modified time of .editorconfig file.

`path'
  Absolute path to .editorconfig file.'
"
  (top-props nil)
  (sections nil)
  (mtime nil)
  (path nil))


(defun editorconfig-core-handle (conf)
  "Return EditorConfig handle for CONF, which should be a file path.

If CONF does not exist return nil."
  (when (file-readable-p conf)
    (let ((cached (gethash conf editorconfig-core-handle--cache-hash))
          (mtime (nth 5 (file-attributes conf))))
      (if (and cached
               (equal (editorconfig-core-handle-mtime cached) mtime))
          cached
        (let ((parsed (editorconfig-core-handle--parse-file conf)))
          (puthash conf
                   (make-editorconfig-core-handle :top-props (plist-get parsed :top-props)
                                                  :sections (plist-get parsed :sections)
                                                  :mtime mtime
                                                  :path conf)
                   editorconfig-core-handle--cache-hash))))))

(defun editorconfig-core-handle-root-p (handle)
  "Return non-nil if HANDLE represent root EditorConfig file.

If HANDLE is nil return nil."
  (when handle
    (string-equal "true"
                  (downcase (or (cdr (assoc "root"
                                            (editorconfig-core-handle-top-props handle)))
                                "")))))

(defun editorconfig-core-handle-get-properties (handle file)
  "Return list of alist of properties from HANDLE for FILE.
The list returned will be ordered by the lines they appear.

If HANDLE is nil return nil."
  (when handle
    (let ((dir (file-name-directory (editorconfig-core-handle-path handle))))
      (cl-loop for section in (editorconfig-core-handle-sections handle)
               for props = (editorconfig-core-handle-section-get-properties section
                                                                            file
                                                                            dir)
               when props collect (copy-alist props)))))
(make-obsolete 'editorconfig-core-handle-get-properties
               'editorconfig-core-handle-get-properties-hash
               "0.8.0")


(defun editorconfig-core-handle-get-properties-hash (handle file)
  "Return hash of properties from HANDLE for FILE.

If HANDLE is nil return nil."
  (when handle
    (let ((hash (make-hash-table))
          (dir (file-name-directory (editorconfig-core-handle-path
                                     handle))))
      (dolist (section (editorconfig-core-handle-sections handle))
        (cl-loop for (key . value) in (editorconfig-core-handle-section-get-properties section file dir)
                 do (puthash (intern key) value hash)))
      hash)))

(defun editorconfig-core-handle--fnmatch-p (name pattern dir)
  "Return non-nil if NAME match PATTERN.
If pattern has slash, pattern should be relative to DIR.

This function is a fnmatch with a few modification for EditorConfig usage."
  (if (string-match-p "/" pattern)
      (let ((pattern (replace-regexp-in-string "^/" "" pattern))
            (dir (file-name-as-directory dir)))
        (editorconfig-fnmatch-p name (concat dir pattern)))
    (editorconfig-fnmatch-p name (concat "**/" pattern))))

(defsubst editorconfig-core-handle--string-trim (str)
  "Remove leading and trailing whitespaces from STR."
  (replace-regexp-in-string "[[:space:]]+\\'"
                            ""
                            (replace-regexp-in-string "\\`[[:space:]]+"
                                                      ""
                                                      str)))

(defun editorconfig-core-handle--parse-file (conf)
  "Parse EditorConfig file CONF.

This function returns cons of its top properties alist and
alist of patterns and its properties alist.
The list returned will be ordered by the lines they appear.

If CONF is not found return nil."
  (when (file-readable-p conf)
    (with-temp-buffer
      ;; NOTE: Use this instead of insert-file-contents-literally to enable
      ;; code conversion
      (insert-file-contents conf)
      (goto-char (point-min))
      (let ((point-max (point-max))
            (sections ())
            (top-props nil)

            ;; String of current line
            (line "")
            ;; nil when pattern not appeared yet, "" when pattern is empty ("[]")
            (pattern nil)
            ;; Alist of properties for current PATTERN
            (props ())

            ;; Current line num
            (current-line-number 1))
        (while (not (eq (point) point-max))
          (setq line
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))
          (setq line
                (replace-regexp-in-string "\\(^\\| \\)\\(#\\|;\\).*$"
                                          ""
                                          (editorconfig-core-handle--string-trim line)))

          (cond
           ((string-equal "" line)
            nil)

           ;; Start of section
           ((string-match "^\\[\\(.*\\)\\]$"
                          line)
            (when pattern
              (setq sections
                    `(,@sections ,(make-editorconfig-core-handle-section
                                   :name pattern
                                   :props props)))
              (setq pattern nil)
              (setq props nil))
            (setq pattern (match-string 1 line)))

           (t
            (let ((idx (string-match "=\\|:" line)))
              (unless idx
                (error "Error while reading config file: %s:%d:\n    %s\n"
                       conf current-line-number line))
              (let ((key (downcase (editorconfig-core-handle--string-trim
                                    (substring line 0 idx))))
                    (value (editorconfig-core-handle--string-trim
                            (substring line (1+ idx)))))
                (when (and (< (length key) 51)
                           (< (length value) 256))
                  (if pattern
                      (when (< (length pattern) 4097)
                        (setq props
                              `(,@props (,key . ,value))))
                    (setq top-props
                          `(,@top-props (,key . ,value)))))))))
          (setq current-line-number (1+ current-line-number))
          (goto-char (point-min))
          (forward-line (1- current-line-number)))
        (when pattern
          (setq sections
                `(,@sections ,(make-editorconfig-core-handle-section
                               :name pattern
                               :props props))))
        (list :top-props top-props
              :sections sections)))))

(provide 'editorconfig-core-handle)
;;; editorconfig-core-handle.el ends here
