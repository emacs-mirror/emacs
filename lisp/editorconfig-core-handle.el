;;; editorconfig-core-handle.el --- Handle Class for EditorConfig File  -*- lexical-binding: t -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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

(defun editorconfig-core-handle-section-get-properties (section file)
  "Return properties alist when SECTION name match FILE.

FILE should be a relative file name, relative to the directory where
the `.editorconfig' file which has SECTION lives.
If not match, return nil."
  (when (editorconfig-core-handle--fnmatch-p
         file (editorconfig-core-handle-section-name section))
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
  Absolute path to .editorconfig file."
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
          (puthash conf parsed editorconfig-core-handle--cache-hash))))))

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
  (declare (obsolete editorconfig-core-handle-get-properties-hash "0.8.0"))
  (when handle
    (let* ((dir (file-name-directory (editorconfig-core-handle-path handle)))
           (file (file-relative-name file dir)))
      (cl-loop for section in (editorconfig-core-handle-sections handle)
               for props = (editorconfig-core-handle-section-get-properties
                            section file)
               when props collect (copy-alist props)))))


(defun editorconfig-core-handle-get-properties-hash (handle file)
  "Return hash of properties from HANDLE for FILE.

If HANDLE is nil return nil."
  (when handle
    (let* ((hash (make-hash-table))
           (dir (file-name-directory (editorconfig-core-handle-path handle)))
           (file (file-relative-name file dir)))
      (dolist (section (editorconfig-core-handle-sections handle))
        (cl-loop for (key . value) in (editorconfig-core-handle-section-get-properties section file)
                 do (puthash (intern key) value hash)))
      hash)))

(defun editorconfig-core-handle--fnmatch-p (name pattern)
  "Return non-nil if NAME match PATTERN.
If pattern has slash, pattern should be relative to DIR.

This function is a fnmatch with a few modification for EditorConfig usage."
  (if (string-match-p "/" pattern)
      (let ((pattern (replace-regexp-in-string "\\`/" "" pattern)))
        (editorconfig-fnmatch-p name pattern))
    ;; The match is not "anchored" so it can be either in the current dir or
    ;; in a subdir.  Contrary to Zsh patterns, editorconfig's `**/foo' does
    ;; not match `foo', so we need to split the problem into two matches.
    (or (editorconfig-fnmatch-p name pattern)
        (editorconfig-fnmatch-p name (concat "**/" pattern)))))

(defun editorconfig-core-handle--parse-file (conf)
  "Parse EditorConfig file CONF.

This function returns a `editorconfig-core-handle'.
If CONF is not found return nil."
  (when (file-readable-p conf)
    (with-temp-buffer
      ;; NOTE: Use this instead of insert-file-contents-literally to enable
      ;; code conversion
      (insert-file-contents conf)
      (goto-char (point-min))
      (let ((sections ())
            (top-props nil)

            ;; nil when pattern not appeared yet, "" when pattern is empty ("[]")
            (pattern nil)
            ;; Alist of properties for current PATTERN
            (props ()))
        (while (not (eobp))
          (skip-chars-forward " \t\f")
          (cond
           ((looking-at "\\(?:[#;].*\\)?$")
            nil)

           ;; Start of section
           ((looking-at "\\[\\(.*\\)\\][ \t]*\\(?:[#;].*\\)?$")
            (let ((newpattern (match-string 1)))
              (when pattern
                (push (make-editorconfig-core-handle-section
                       :name pattern
                       :props (nreverse props))
                      sections))
              (setq props nil)
              (setq pattern newpattern)))

           ((looking-at "\\([^=: \n\t][^=:\n]*\\)[ \t]*[=:][ \t]*\\(.*?\\)[ \t]*$")
            (let ((key (downcase (string-trim (match-string 1))))
                  (value (match-string 2)))
              (if pattern
                  (push `(,key . ,value)
                        props)
                (push `(,key . ,value)
                      top-props))))

           (t (error "Error while reading config file: %s:%d:\n    %s\n"
                     conf (line-number-at-pos)
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))))
          (forward-line 1))
        (when pattern
          (push (make-editorconfig-core-handle-section
                 :name pattern
                 :props (nreverse props))
                sections))
        (make-editorconfig-core-handle
         :top-props (nreverse top-props)
         :sections (nreverse sections)
         :mtime (nth 5 (file-attributes conf))
         :path conf)))))

(provide 'editorconfig-core-handle)
;;; editorconfig-core-handle.el ends here
