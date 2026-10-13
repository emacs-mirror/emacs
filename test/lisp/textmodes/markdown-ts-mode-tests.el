;;; markdown-ts-mode-tests.el --- Tests for Tree-sitter-based Markdown mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'treesit)
(require 'url-vars)
(require 'markdown-ts-mode)

(ert-deftest markdown-ts-mode-test-decode-destination ()
  "Link destinations are unbracketed, unescaped and entity-decoded.
Escapes and references are processed in a single pass, numeric
references out of range become U+FFFD, and only HTML entity names are
decoded."
  (dolist (case '(("<my file.md>"        . "my file.md")
                  ("<https://gnu.org>"   . "https://gnu.org")
                  ("features\\.md"       . "features.md")
                  ("<READ\\ME.md>"       . "READ\\ME.md")
                  ("README.md\\#L45"     . "README.md#L45")
                  ("README&#46;md"       . "README.md")
                  ("&#x52;EADME.md"      . "README.md")
                  ("&#X52;EADME.md"      . "README.md")
                  ("README&period;md"    . "README.md")
                  ("&ell;.md"            . "ℓ.md")
                  ("README&bogus;md"     . "README&bogus;md")
                  ("README&smiley;md"    . "README&smiley;md")
                  ("README\\&#46;md"     . "README&#46;md")
                  ("a\\&amp;b.md"        . "a&amp;b.md")
                  ("a&amp;#46;b.md"      . "a&#46;b.md")
                  ("x&#0;.md"            . "x\uFFFD.md")
                  ("x&#xD800;.md"        . "x\uFFFD.md")
                  ("x&#x110000;.md"      . "x\uFFFD.md")
                  ("x&#9999999;.md"      . "x\uFFFD.md")
                  ("x&#99999999;.md"     . "x&#99999999;.md")))
    (should (equal case
                   (cons (car case)
                         (markdown-ts--decode-destination (car case)))))))

(ert-deftest markdown-ts-mode-test-url-to-file ()
  "Local file names are percent-decoded as UTF-8; invalid escapes stay."
  (dolist (case '(("README%2Emd"    . "README.md")
                  ("f%C3%B6o.txt"   . "föo.txt")
                  ("100%25.md"      . "100%.md")
                  ("bad%zz.md"      . "bad%zz.md")))
    (should (equal case
                   (cons (car case)
                         (markdown-ts--url-to-file (car case)))))))

(ert-deftest markdown-ts-mode-test-link-dispatch ()
  "Link buttons dispatch on the destination.
Schemes are case-insensitive and 2 to 32 characters long, so a drive
letter like \"C:\" is a file name."
  (dolist (case '(("#heading"                   . fragment)
                  ("mailto:user@gnu.org"        . mail)
                  ("MAILTO:user@gnu.org"        . mail)
                  ("https://www.gnu.org"        . browse)
                  ("HTTPS://www.gnu.org"        . browse)
                  ("svn+ssh://example.com/repo" . browse)
                  ("C:/foo.md"                  . file)
                  ("README.md#L45"              . file)))
    (let* ((called nil)
           (url-mail-command (lambda (_) (setq called 'mail))))
      (cl-letf (((symbol-function 'markdown-ts--goto-fragment)
                 (lambda (_) (setq called 'fragment)))
                ((symbol-function 'browse-url)
                 (lambda (&rest _) (setq called 'browse)))
                ((symbol-function 'markdown-ts--follow-file-link)
                 (lambda (_) (setq called 'file))))
        (with-temp-buffer
          (insert "x")
          (markdown-ts--make-link-button 1 2 (car case))
          (button-activate (button-at 1))))
      (should (equal case
                     (cons (car case)
                           called))))))

(ert-deftest markdown-ts-mode-test-follow-file-link ()
  "A file link splits at the first \"#\" before percent-decoding.
An empty fragment is ignored, and \"%23\" stays part of the file name."
  (dolist (case '(("README.md"        "README.md" nil)
                  ("README.md#L45"    "README.md" "L45")
                  ("README.md#usage"  "README.md" "usage")
                  ("x.el#"            "x.el"      nil)
                  ("a%23b.md"         "a#b.md"    nil)
                  ("%52EADME.md#L45"  "README.md" "L45")))
    (let (file frag)
      (cl-letf (((symbol-function 'find-file)
                 (lambda (f &rest _) (setq file f)))
                ((symbol-function 'markdown-ts--goto-fragment)
                 (lambda (f) (setq frag f))))
        (markdown-ts--follow-file-link (car case)))
      (should (equal case
                     (list (car case)
                           file
                           frag))))))

(ert-deftest markdown-ts-mode-test-goto-line-fragment ()
  "\"L<n>\", \"L<n>-L<m>\" and \"L<n>-<m>\" go to line n.
Only upper-case \"L\" counts: \"l3\" is a heading slug, not a line."
  (let ((buffer-lines 50))
    (dolist (case `(("L45"     . 45)
                    ("L45-L60" . 45)
                    ("L45-60"  . 45)
                    ("L999"    . ,(1+ buffer-lines)) ; Beyond buffer length.
                    ("l3"      . 1)
                    ("L"       . 1)))
      (with-temp-buffer
        (dotimes (i buffer-lines)
          (insert (format "line %d\n" (1+ i))))
        (goto-char (point-min))
        (let ((inhibit-message t))
          (markdown-ts--goto-fragment (car case)))
        (should (equal case
                       (cons (car case)
                             (line-number-at-pos))))))))

(ert-deftest markdown-ts-mode-test-goto-heading-fragment ()
  "Fragments jump to the heading with that slug or explicit id.
\"#L3\" goes to line 3 even when a heading has the slug \"l3\"."
  (skip-unless (and (treesit-ready-p 'markdown t)
                    (treesit-ready-p 'markdown-inline t)))
  (ert-with-test-buffer (:selected t)
    (insert "# Heading Level 1\n"
            "\n"
            "text\n"
            "\n"
            "## Custom {#custom-id}\n"
            "\n"
            "### L3\n")
    (markdown-ts-mode)
    (let ((inhibit-message t))
      (dolist (case '(("heading-level-1" . 1)
                      ("custom-id"       . 5)
                      ("l3"              . 7)
                      ("L3"              . 3)))
        (goto-char (point-max))
        (markdown-ts--goto-fragment (car case))
        (should (equal case
                       (cons (car case)
                             (line-number-at-pos)))))
      (should-error (markdown-ts--goto-fragment "nope")
                    :type 'user-error))))

(provide 'markdown-ts-mode-tests)

;;; markdown-ts-mode-tests.el ends here
