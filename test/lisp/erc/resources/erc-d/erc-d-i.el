;;; erc-d-i.el --- IRC helpers for ERC test server -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (erc-d-i-message (:conc-name erc-d-i-message.))
  "Identical to `erc-response'.
When member `compat' is nil, it means the raw message was decoded as
UTF-8 text before parsing, which is nonstandard."
  (unparsed "" :type string)
  (sender "" :type string)
  (command "" :type string)
  (command-args nil :type (list-of string))
  (contents "" :type string)
  (tags nil :type (list-of (cons symbol string)))
  (compat t :type boolean))

(defconst erc-d-i--tag-escapes
  '((";" . "\\:") (" " . "\\s") ("\\" . "\\\\") ("\r" . "\\r") ("\n" . "\\n")))

;; These are not mirror inverses; unescaping may drop stranded or
;; misplaced backslashes.

(defconst erc-d-i--tag-escaped-regexp (rx (or ?\; ?\  ?\\ ?\r ?\n)))

(defconst erc-d-i--tag-unescaped-regexp
  (rx (or "\\:" "\\s" "\\\\" "\\r" "\\n"
          (seq "\\" (or string-end (not (or ":" "s" "n" "r" "\\")))))))

(defun erc-d-i--unescape-tag-value (str)
  "Undo substitution of char placeholders in raw tag value STR."
  (replace-regexp-in-string erc-d-i--tag-unescaped-regexp
                            (lambda (s)
                              (or (car (rassoc s erc-d-i--tag-escapes))
                                  (substring s 1)))
                            str t t))

(defun erc-d-i--escape-tag-value (str)
  "Swap out banned chars in tag value STR with message representation."
  (replace-regexp-in-string erc-d-i--tag-escaped-regexp
                            (lambda (s)
                              (cdr (assoc s erc-d-i--tag-escapes)))
                            str t t))

(defconst erc-d-i--invalid-tag-regexp (rx (any "\0\7\r\n; ")))

(defun erc-d-i--validate-tags (raw)
  "Validate tags portion of some RAW incoming message.
RAW must not have a leading \"@\" or a trailing space.  The spec says
validation shouldn't be performed on keys and that undecodeable values
or ones with illegal (unescaped) chars may be dropped.  This does not
respect any of that.  Its purpose is to catch bad input created by us."
  (unless (> 4094 (string-bytes raw))
    ;; 417 ERR_INPUTTOOLONG Input line was too long
    (error "Message tags exceed 4094 bytes: %S" raw))
  (let (tags
        (tag-strings (split-string raw ";")))
    (dolist (s tag-strings (nreverse tags))
      (let* ((m (if (>= emacs-major-version 28)
                    (string-search "=" s)
                  (string-match-p "=" s)))
             (key (if m (substring s 0 m) s))
             (val (when-let* (m ; check first, like (m), but shadow
                              (v (substring s (1+ m)))
                              ((not (string-equal v ""))))
                    (when (string-match-p erc-d-i--invalid-tag-regexp v)
                      (error "Bad tag: %s" s))
                    (thread-first v
                                  (decode-coding-string 'utf-8 t)
                                  (erc-d-i--unescape-tag-value)))))
        (when (string-empty-p key)
          (error "Tag missing key: %S" s))
        (setf (alist-get (intern key) tags) val)))))

(defun erc-d-i--parse-message (s &optional decode)
  "Parse string S into `erc-d-i-message' object.
With DECODE, decode as UTF-8 text."
  (when (string-suffix-p "\r\n" s)
    (error "Unstripped message encountered"))
  (when decode
    (setq s (decode-coding-string s 'utf-8 t)))
  (let ((mes (make-erc-d-i-message :unparsed s :compat (not decode)))
        tokens)
    (when-let* (((not (string-empty-p s)))
                ((eq ?@ (aref s 0)))
                (m (string-match " " s))
                (u (substring s 1 m)))
      (setf (erc-d-i-message.tags mes) (erc-d-i--validate-tags u)
            s (substring s (1+ m))))
    (if-let* ((m (string-search " :" s))
              (other-toks (split-string (substring s 0 m) " " t))
              (rest (substring s (+ 2 m))))
        (setf (erc-d-i-message.contents mes) rest
              tokens (nconc other-toks (list rest)))
      (setf tokens (split-string s " " t " ")
            (erc-d-i-message.contents mes) (car (last tokens))))
    (when (and tokens (eq ?: (aref (car tokens) 0)))
      (setf (erc-d-i-message.sender mes) (substring (pop tokens) 1)))
    (setf (erc-d-i-message.command mes) (or (pop tokens) "")
          (erc-d-i-message.command-args mes) tokens)
    mes))

(provide 'erc-d-i)
;;; erc-d-i.el ends here
