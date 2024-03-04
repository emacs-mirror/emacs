;;; mule-tests.el --- unit tests for mule.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2024 Free Software Foundation, Inc.

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

;; Unit tests for lisp/international/mule.el.

;;; Code:

(require 'ert-x)                        ;For `ert-simulate-keys'.

(ert-deftest find-auto-coding--bug27391 ()
  "Check that Bug#27391 is fixed."
  (with-temp-buffer
    (insert "\n[comment]: # ( Local Variables: )\n"
            "[comment]: # ( coding: utf-8	)\n"
            "[comment]: # ( End:		)\n")
    (goto-char (point-min))
    (should (equal (let ((auto-coding-alist ())
                         (auto-coding-regexp-alist ())
                         (auto-coding-functions ()))
                     (find-auto-coding "" (buffer-size)))
                   '(utf-8 . :coding)))))

(ert-deftest mule-cmds-tests--encode-ebcdic ()
  (should (equal (encode-coding-char ?a 'ebcdic-int) "\201"))
  (should (not (multibyte-string-p (encode-coding-char ?a 'utf-8)))))

(ert-deftest mule-cmds--test-universal-coding-system-argument ()
  (should (equal "ccccccccccccccccab"
                 (let ((enable-recursive-minibuffers t))
                   (ert-simulate-keys
                       (kbd "C-x RET c u t f - 8 RET C-u C-u c a b RET")
                     (read-string "prompt:"))))))

;;Bug#65997, ensure that old-names haven't overridden new names.
(ert-deftest mule-cmds-tests--ucs-names-old-name-override ()
  (let (code-points)
    (dotimes (u (1+ (max-char 'ucs)))
      (when-let* ((name (get-char-code-property u 'name))
                  (c (char-from-name name)))
        (when (and (not (<= #xD800 u #xDFFF))
                   (not (= c u)))
          (push (format "%X" u) code-points))))
    (setq code-points (nreverse code-points))
    (should (null code-points))))

;; Bug#65997, ensure that all codepoints with names are in '(ucs-names)'.
(ert-deftest mule-cmds-tests--ucs-names-missing-names ()
  (let (code-points)
    (dotimes (u (1+ (max-char 'ucs)))
      (when-let ((name (get-char-code-property u 'name)))
        (when (and (not (<= #xD800 u #xDFFF))
                   (not (<= #x18800 u #x18AFF))
                   (not (char-from-name name)))
          (push (format "%X" u) code-points))))
    (setq code-points (nreverse code-points))
    (should (null code-points))))

(ert-deftest mule-utf-7 ()
  ;; utf-7 and utf-7-imap are not ASCII-compatible.
  (should-not (coding-system-get 'utf-7 :ascii-compatible-p))
  (should-not (coding-system-get 'utf-7-imap :ascii-compatible-p))
  ;; Invariant ASCII subset.
  (let ((s (apply #'string (append (number-sequence #x20 #x25)
                                   (number-sequence #x27 #x7e)))))
    (should (equal (encode-coding-string s 'utf-7-imap) s))
    (should (equal (decode-coding-string s 'utf-7-imap) s)))
  ;; Escaped ampersand.
  (should (equal (encode-coding-string "a&bcd" 'utf-7-imap) "a&-bcd"))
  (should (equal (decode-coding-string "a&-bcd" 'utf-7-imap) "a&bcd"))
  ;; Ability to encode Unicode.
  (should (equal (check-coding-systems-region "あ" nil '(utf-7-imap)) nil))
  (should (equal (encode-coding-string "あ" 'utf-7-imap) "&MEI-"))
  (should (equal (decode-coding-string "&MEI-" 'utf-7-imap) "あ")))

(ert-deftest mule-hz ()
  ;; The chinese-hz encoding is not ASCII compatible.
  (should-not (coding-system-get 'chinese-hz :ascii-compatible-p)))

;;; Testing `sgml-html-meta-auto-coding-function'.

(defconst sgml-html-meta-pre "<!doctype html><html><head>"
  "The beginning of a minimal HTML document.")

(defconst sgml-html-meta-post "</head></html>"
  "The end of a minimal HTML document.")

(defun sgml-html-meta-run (coding-system)
  "Run `sgml-html-meta-auto-coding-function' on a minimal HTML.
When CODING-SYSTEM is not nil, insert it, wrapped in a '<meta>'
element.  When CODING-SYSTEM contains HTML meta characters or
white space, insert it as-is, without additional formatting.  Use
the variables `sgml-html-meta-pre' and `sgml-html-meta-post' to
provide HTML fragments.  Some tests override those variables."
  (with-temp-buffer
    (insert sgml-html-meta-pre
            (cond ((not coding-system)
                   "")
                  ((string-match "[<>'\"\n ]" coding-system)
                   coding-system)
                  (t
                   (format "<meta charset='%s'>" coding-system)))
            sgml-html-meta-post)
    (goto-char (point-min))
    (sgml-html-meta-auto-coding-function (- (point-max) (point-min)))))

(ert-deftest sgml-html-meta-utf-8 ()
  "Baseline: UTF-8."
  (should (eq 'utf-8 (coding-system-base (sgml-html-meta-run "utf-8")))))

(ert-deftest sgml-html-meta-windows-hebrew ()
  "A non-Unicode charset."
  (should (eq 'windows-1255 (sgml-html-meta-run "windows-1255"))))

(ert-deftest sgml-html-meta-none ()
  (should (eq nil (sgml-html-meta-run nil))))

(ert-deftest sgml-html-meta-unknown-coding ()
  (should (eq nil (sgml-html-meta-run "XXX"))))

(ert-deftest sgml-html-meta-no-pre ()
  "Without the prefix, so not HTML."
  (let ((sgml-html-meta-pre ""))
    (should (eq nil (sgml-html-meta-run "utf-8")))))

(ert-deftest sgml-html-meta-no-post-less-than-10lines ()
  "No '</head>', detect charset in the first 10 lines."
  (let ((sgml-html-meta-post ""))
    (should (eq 'utf-8 (coding-system-base
                        (sgml-html-meta-run
                         (concat "\n\n\n\n\n\n\n\n\n"
                                 "<meta charset='utf-8'>")))))))

(ert-deftest sgml-html-meta-no-post-10lines ()
  "No '</head>', do not detect charset after the first 10 lines."
  (let ((sgml-html-meta-post ""))
    (should (eq nil (sgml-html-meta-run
                     (concat "\n\n\n\n\n\n\n\n\n\n"
                             "<meta charset='utf-8'>"))))))

(ert-deftest sgml-html-meta-utf-8-with-bom ()
  "Requesting 'UTF-8' does not override `utf-8-with-signature'.
Check fix for Bug#20623."
  (let ((buffer-file-coding-system 'utf-8-with-signature))
    (should (eq 'utf-8-with-signature (sgml-html-meta-run "utf-8")))))

;; Stop "Local Variables" above causing confusion when visiting this file.


;;; mule-tests.el ends here
