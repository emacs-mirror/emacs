;;; editorconfig-fnmatch.el --- Glob pattern matching in Emacs lisp  -*- lexical-binding: t -*-

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

;; editorconfig-fnmatch.el provides a fnmatch implementation with a few
;; extensions.
;; The main usage of this library is glob pattern matching for EditorConfig, but
;; it can also act solely.

;; editorconfig-fnmatch-p (name pattern)

;; Test whether NAME match PATTERN.

;; PATTERN should be a shell glob pattern, and some zsh-like wildcard matchings
;; can be used:

;; *           Matches any string of characters, except path separators (/)
;; **          Matches any string of characters
;; ?           Matches any single character
;; [name]      Matches any single character in name
;; [^name]     Matches any single character not in name
;; {s1,s2,s3}  Matches any of the strings given (separated by commas)
;; {min..max}  Matches any number between min and max


;; This library is a port from editorconfig-core-py library.
;; https://github.com/editorconfig/editorconfig-core-py/blob/master/editorconfig/fnmatch.py

;;; Code:

(require 'cl-lib)

(defvar editorconfig-fnmatch--cache-hashtable
  nil
  "Cache of shell pattern and its translation.")
;; Clear cache on file reload
(setq editorconfig-fnmatch--cache-hashtable
      (make-hash-table :test 'equal))


(defconst editorconfig-fnmatch--left-brace-regexp
  "\\(^\\|[^\\]\\){"
  "Regular expression for left brace ({).")

(defconst editorconfig-fnmatch--right-brace-regexp
  "\\(^\\|[^\\]\\)}"
  "Regular expression for right brace (}).")


(defconst editorconfig-fnmatch--numeric-range-regexp
  "\\([+-]?[0-9]+\\)\\.\\.\\([+-]?[0-9]+\\)"
  "Regular expression for numeric range (like {-3..+3}).")

(defun editorconfig-fnmatch--match-num (regexp string)
  "Return how many times REGEXP is found in STRING."
  (let ((num 0))
    ;; START arg does not work as expected in this case
    (while (string-match regexp string)
      (setq num (1+ num)
            string (substring string (match-end 0))))
    num))

;;;###autoload
(defun editorconfig-fnmatch-p (string pattern)
  "Test whether STRING match PATTERN.

Matching ignores case if `case-fold-search' is non-nil.

PATTERN should be a shell glob pattern, and some zsh-like wildcard matchings can
be used:

*           Matches any string of characters, except path separators (/)
**          Matches any string of characters
?           Matches any single character
[name]      Matches any single character in name
[^name]     Matches any single character not in name
{s1,s2,s3}  Matches any of the strings given (separated by commas)
{min..max}  Matches any number between min and max"
  (string-match (editorconfig-fnmatch-translate pattern)
                string))

;;(editorconfig-fnmatch-translate "{a,{-3..3}}.js")
;;(editorconfig-fnmatch-p "1.js" "{a,{-3..3}}.js")

(defun editorconfig-fnmatch-translate (pattern)
  "Translate a shell PATTERN to a regular expression.

Translation result will be cached, so same translation will not be done twice."
  (let ((cached (gethash pattern
                         editorconfig-fnmatch--cache-hashtable)))
    (or cached
        (puthash pattern
                 (editorconfig-fnmatch--do-translate pattern)
                 editorconfig-fnmatch--cache-hashtable))))


(defun editorconfig-fnmatch--do-translate (pattern &optional nested)
  "Translate a shell PATTERN to a regular expression.

Set NESTED to t when this function is called from itself.

This function is called from `editorconfig-fnmatch-translate', when no cached
translation is found for PATTERN."
  (let ((index 0)
        (length (length pattern))
        (brace-level 0)
        (in-brackets nil)
        ;; List of strings of resulting regexp
        (result ())
        (is-escaped nil)
        (matching-braces (= (editorconfig-fnmatch--match-num
                             editorconfig-fnmatch--left-brace-regexp
                             pattern)
                            (editorconfig-fnmatch--match-num
                             editorconfig-fnmatch--right-brace-regexp
                             pattern)))

        current-char
        pos
        has-slash
        has-comma
        num-range)

    (while (< index length)
      (if (and (not is-escaped)
               (string-match "[^]\\*?[{},/\\-]+"
                             ;;(string-match "[^]\\*?[{},/\\-]+" "?.a")
                             pattern
                             index)
               (eq index (match-beginning 0)))
          (setq result `(,@result ,(regexp-quote (match-string 0 pattern)))
                index (match-end 0)
                is-escaped nil)

        (setq current-char (aref pattern index)
              index (1+ index))

        (cl-case current-char
          (?*
           (setq pos index)
           (if (and (< pos length)
                    (= (aref pattern pos) ?*))
               (setq result `(,@result ".*"))
             (setq result `(,@result "[^/]*"))))

          (??
           (setq result `(,@result "[^/]")))

          (?\[
           (if in-brackets
               (setq result `(,@result "\\["))
             (if (= (aref pattern index) ?/)
                 ;; Slash after an half-open bracket
                 (setq result `(,@result "\\[/")
                       index (+ index 1))
               (setq pos index
                     has-slash nil)
               (while (and (< pos length)
                           (not (= (aref pattern pos) ?\]))
                           (not has-slash))
                 (if (and (= (aref pattern pos) ?/)
                          (not (= (aref pattern (- pos 1)) ?\\)))
                     (setq has-slash t)
                   (setq pos (1+ pos))))
               (if has-slash
                   (setq result `(,@result ,(concat "\\["
                                                    (substring pattern
                                                               index
                                                               (1+ pos))
                                                    "\\]"))
                         index (+ pos 2))
                 (if (and (< index length)
                          (memq (aref pattern index)
                                '(?! ?^)))
                     (setq index (1+ index)
                           result `(,@result "[^"))
                   (setq result `(,@result "[")))
                 (setq in-brackets t)))))

          (?-
           (if in-brackets
               (setq result `(,@result "-"))
             (setq result `(,@result "\\-"))))

          (?\]
           (setq result `(,@result "]")
                 in-brackets nil))

          (?{
           (setq pos index
                 has-comma nil)
           (while (and (or (and (< pos length)
                                (not (= (aref pattern pos) ?})))
                           is-escaped)
                       (not has-comma))
             (if (and (eq (aref pattern pos) ?,)
                      (not is-escaped))
                 (setq has-comma t)
               (setq is-escaped (and (eq (aref pattern pos)
                                         ?\\)
                                     (not is-escaped))
                     pos (1+ pos))))
           (if (and (not has-comma)
                    (< pos length))
               (let ((pattern-sub (substring pattern index pos)))
                 (setq num-range (string-match editorconfig-fnmatch--numeric-range-regexp
                                               pattern-sub))
                 (if num-range
                     (let ((number-start (string-to-number (match-string 1
                                                                         pattern-sub)))
                           (number-end (string-to-number (match-string 2
                                                                       pattern-sub))))
                       (setq result `(,@result ,(concat "\\(?:"
                                                        (mapconcat 'number-to-string
                                                                   (cl-loop for i from number-start to number-end
                                                                            collect i)
                                                                   "\\|")
                                                        "\\)"))))
                   (let ((inner (editorconfig-fnmatch--do-translate pattern-sub t)))
                     (setq result `(,@result ,(format "{%s}" inner)))))
                 (setq index (1+ pos)))
             (if matching-braces
                 (setq result `(,@result "\\(?:")
                       brace-level (1+ brace-level))
               (setq result `(,@result "{")))))

          (?,
           (if (and (> brace-level 0)
                    (not is-escaped))
               (setq result `(,@result "\\|"))
             (setq result `(,@result "\\,"))))

          (?}
           (if (and (> brace-level 0)
                    (not is-escaped))
               (setq result `(,@result "\\)")
                     brace-level (- brace-level 1))
             (setq result `(,@result "}"))))

          (?/
           (if (and (<= (+ index 3) (length pattern))
                    (string= (substring pattern index (+ index 3)) "**/"))
               (setq result `(,@result "\\(?:/\\|/.*/\\)")
                     index (+ index 3))
             (setq result `(,@result "/"))))

          (t
           (unless (= current-char ?\\)
             (setq result `(,@result ,(regexp-quote (char-to-string current-char)))))))

        (if (= current-char ?\\)
            (progn (when is-escaped
                     (setq result `(,@result "\\\\")))
                   (setq is-escaped (not is-escaped)))
          (setq is-escaped nil))))
    (unless nested
      (setq result `("^" ,@result "\\'")))
    (apply #'concat result)))

(provide 'editorconfig-fnmatch)
;;; editorconfig-fnmatch.el ends here
