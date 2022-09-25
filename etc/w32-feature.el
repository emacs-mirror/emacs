;;; w32-feature.el --- Check Availability of Emacs Features  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Author: Phillip Lord <phillip.lord@russet.org.uk>

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

;; This file provides tests for various features of Emacs. It is
;; designed to check whether bundled binary distributions of Emacs on
;; windows are fully functional.

;; By default is checks whether the features that we are expect to be
;; available on Emacs for Windows are reported to be available. It
;; should be possible to run these tests from a distributed version of
;; Emacs.

;; In addition, it provides a single command
;; `w32-feature-load-tests'. If the full source repository of Emacs is
;; available, this will load selected files from the repository which
;; test these features.

;;; Code:
(require 'ert)

(defvar w32-feature-core-tests nil)

(ert-deftest feature-optimization ()
  (should
   (string-match-p "CFLAGS=-O2" system-configuration-options)))

(ert-deftest feature-harfbuzz ()
  (should
   (eq
    'harfbuzz
    (car (frame-parameter nil 'font-backend)))))

(ert-deftest feature-gnutls ()
  (should (gnutls-available-p)))

(add-to-list 'w32-feature-core-tests "lisp/net/gnutls-tests.el")

(ert-deftest feature-zlib ()
  (should (zlib-available-p)))

(add-to-list 'w32-feature-core-tests "src/decompress-tests.el")

(ert-deftest feature-thread ()
  (should (fboundp 'make-thread)))

(add-to-list 'w32-feature-core-tests "lisp/thread-tests.el")

(ert-deftest feature-json ()
  (should
   (fboundp 'json-serialize)))

(add-to-list 'w32-feature-core-tests "src/json-tests.el")

(ert-deftest feature-gmp ()
  (should
   (string-match-p "GMP" system-configuration-features)))

(ert-deftest feature-module ()
  (should (fboundp 'module-load)))

(ert-deftest feature-libxml ()
  (should (libxml-available-p)))

(add-to-list 'w32-feature-core-tests "src/xml-tests.el")

(ert-deftest feature-lcms2 ()
  (should (lcms2-available-p)))

(add-to-list 'w32-feature-core-tests "src/lcms-tests.el")

(ert-deftest feature-xpm ()
  (should (image-type-available-p 'xpm)))

(ert-deftest feature-gif ()
  (should (image-type-available-p 'gif)))

(ert-deftest feature-png ()
  (should (image-type-available-p 'png)))

(add-to-list 'w32-feature-core-tests "lisp/image-file-tests.el")

(ert-deftest feature-jpeg ()
  (should (image-type-available-p 'jpeg)))

(ert-deftest feature-tiff ()
  (should (image-type-available-p 'tiff)))

(ert-deftest feature-svg ()
  (should (image-type-available-p 'svg)))

(defun w32-feature-load-tests (dir)
  (interactive "D")
  (mapc
   (lambda(f)
     (load-file (concat dir "test/" f)))
   w32-feature-core-tests))

;;; feature.el ends here
