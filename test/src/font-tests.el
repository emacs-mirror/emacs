;;; font-tests.el --- Test suite for font-related functions.

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

;; Author: Chong Yidong <cyd@stupidchicken.com>
;; Keywords:       internal
;; Human-Keywords: internal

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

;; Type M-x test-font-parse RET to generate the test buffer.

;;; Code:

(require 'ert)

(defvar font-parse-tests--data
  `((" " ,(intern " ") nil nil nil nil)
    ("Monospace" Monospace nil nil nil nil)
    ("Monospace Serif" ,(intern "Monospace Serif") nil nil nil nil)
    ("Foo1" Foo1 nil nil nil nil)
    ("12" nil 12.0 nil nil nil)
    ("12 " ,(intern "12 ") nil nil nil nil)
    ;; Fontconfig format
    ("Foo:" Foo nil nil nil nil)
    ("Foo-8" Foo 8.0 nil nil nil)
    ("Foo-18:" Foo 18.0 nil nil nil)
    ("Foo-18:light" Foo 18.0 light nil nil)
    ("Foo 10:weight=bold" ,(intern "Foo 10") nil bold nil nil)
    ("Foo-12:weight=bold" Foo 12.0 bold nil nil)
    ("Foo 8-20:slant=oblique" ,(intern "Foo 8") 20.0 nil oblique nil)
    ("Foo:light:roman" Foo nil light roman nil)
    ("Foo:italic:roman" Foo nil nil roman nil)
    ("Foo 12:light:oblique" ,(intern "Foo 12") nil light oblique nil)
    ("Foo-12:demibold:oblique" Foo 12.0 demibold oblique nil)
    ("Foo:black:proportional" Foo nil black nil 0)
    ("Foo-10:black:proportional" Foo 10.0 black nil 0)
    ("Foo:weight=normal" Foo nil normal nil nil)
    ("Foo:weight=bold" Foo nil bold nil nil)
    ("Foo:weight=bold:slant=italic" Foo nil bold italic)
    ("Foo:weight=bold:slant=italic:mono" Foo nil bold italic 100)
    ("Foo-10:demibold:slant=normal" Foo 10.0 demibold normal nil)
    ("Foo 11-16:oblique:weight=bold" ,(intern "Foo 11") 16.0 bold oblique nil)
    ("Foo:oblique:randomprop=randomtag:weight=bold" Foo nil bold oblique nil)
    ("Foo:randomprop=randomtag:bar=baz" Foo nil nil nil nil)
    ("Foo Book Light:bar=baz" ,(intern "Foo Book Light") nil nil nil nil)
    ("Foo Book Light 10:bar=baz" ,(intern "Foo Book Light 10") nil nil nil nil)
    ("Foo Book Light-10:bar=baz" ,(intern "Foo Book Light") 10.0 nil nil nil)
    ;; GTK format
    ("Oblique" nil nil nil oblique nil)
    ("Bold 17" nil 17.0 bold nil nil)
    ("17 Bold" ,(intern "17") nil bold nil nil)
    ("Book Oblique 2" nil 2.0 book oblique nil)
    ("Bar 7" Bar 7.0 nil nil nil)
    ("Bar Ultra-Light" Bar nil ultra-light nil nil)
    ("Bar Light 8" Bar 8.0 light nil nil)
    ("Bar Book Medium 9" Bar 9.0 medium nil nil)
    ("Bar Semi-Bold Italic 10" Bar 10.0 semi-bold italic nil)
    ("Bar Semi-Condensed Bold Italic 11" Bar 11.0 bold italic nil)
    ("Foo 10 11" ,(intern "Foo 10") 11.0 nil nil nil)
    ("Foo 1985 Book" ,(intern "Foo 1985") nil book nil nil)
    ("Foo 1985 A Book" ,(intern "Foo 1985 A") nil book nil nil)
    ("Foo 1 Book 12" ,(intern "Foo 1") 12.0 book nil nil)
    ("Foo A Book 12 A" ,(intern "Foo A Book 12 A") nil nil nil nil)
    ("Foo 1985 Book 12 Oblique" ,(intern "Foo 1985 Book 12") nil nil oblique nil)
    ("Foo 1985 Book 12 Italic 10" ,(intern "Foo 1985 Book 12") 10.0 nil italic nil)
    ("Foo Book Bar 6 Italic" ,(intern "Foo Book Bar 6") nil nil italic nil)
    ("Foo Book Bar Bold" ,(intern "Foo Book Bar") nil bold nil nil))
  "List of font names parse data.
Each element should have the form
   (NAME FAMILY SIZE WEIGHT SLANT SPACING)
where NAME is the name to parse, and the remainder are the
expected font properties from parsing NAME.")

(defun font-parse-check (name prop expected)
  (let ((result (font-get (font-spec :name name) prop)))
    (if (and (symbolp result) (symbolp expected))
	(eq result expected)
      (equal result expected))))

(put 'font-parse-check 'ert-explainer 'font-parse-explain)

(defun font-parse-explain (name prop expected)
  (let ((result (font-get (font-spec :name name) prop))
	(propname (symbol-name prop)))
    (format "Parsing `%s': expected %s `%s', got `%s'."
	    name (substring propname 1) expected
	    (font-get (font-spec :name name) prop))))

(ert-deftest font-parse-tests ()
  "Test parsing of Fontconfig-style and GTK-style font names."
  (dolist (test font-parse-tests--data)
    (let* ((name (nth 0 test)))
      (should (font-parse-check name :family (nth 1 test)))
      (should (font-parse-check name :size   (nth 2 test)))
      (should (font-parse-check name :weight (nth 3 test)))
      (should (font-parse-check name :slant  (nth 4 test)))
      (should (font-parse-check name :spacing (nth 5 test))))))


(defun test-font-parse ()
  "Test font name parsing."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Font Pase Test*"))
  (setq show-trailing-whitespace nil)
  (let ((pass-face '((t :foreground "green")))
	(fail-face '((t :foreground "red"))))
    (dolist (test font-parse-tests--data)
      (let* ((name (nth 0 test))
	     (fs (font-spec :name name))
	     (family  (font-get fs :family))
	     (size    (font-get fs :size))
	     (weight  (font-get fs :weight))
	     (slant   (font-get fs :slant))
	     (spacing (font-get fs :spacing)))
	(insert name)
	(if (> (current-column) 20)
	    (insert "\n"))
	(indent-to-column 21)
	(insert (propertize (symbol-name family)
			    'face (if (eq family (nth 1 test))
				      pass-face
				    fail-face)))
	(indent-to-column 40)
	(insert (propertize (format "%s" size)
			    'face (if (equal size (nth 2 test))
				      pass-face
				    fail-face)))
	(indent-to-column 48)
	(insert (propertize (format "%s" weight)
			    'face (if (eq weight (nth 3 test))
				      pass-face
				    fail-face)))
	(indent-to-column 60)
	(insert (propertize (format "%s" slant)
			    'face (if (eq slant (nth 4 test))
				      pass-face
				    fail-face)))
	(indent-to-column 69)
	(insert (propertize (format "%s" spacing)
			    'face (if (eq spacing (nth 5 test))
				      pass-face
				    fail-face)))
	(insert "\n"))))
  (goto-char (point-min)))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'font-tests)
;;; font-tests.el ends here.
