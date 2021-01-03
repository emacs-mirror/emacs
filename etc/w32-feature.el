;;; w32-feature.el --- Check Availability of Emacs Features  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

;;; Code:
(require 'ert)

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

(ert-deftest feature-zlib ()
  (should (zlib-available-p)))

(ert-deftest feature-thread ()
  (should (fboundp 'make-thread)))

(ert-deftest feature-json ()
  (should
   (fboundp 'json-serialize)))

(ert-deftest feature-gmp ()
  (should
   (string-match-p "GMP" system-configuration-features)))

(ert-deftest feature-module ()
  (should (fboundp 'module-load)))

(ert-deftest feature-libxml ()
  (should (libxml-available-p)))

(ert-deftest feature-lcms2 ()
  (should (lcms2-available-p)))

(ert-deftest feature-xpm ()
  (should (image-type-available-p 'xpm)))

(ert-deftest feature-gif ()
  (should (image-type-available-p 'gif)))

(ert-deftest feature-png ()
  (should (image-type-available-p 'png)))

(ert-deftest feature-xpm ()
  (should (image-type-available-p 'xpm)))

(ert-deftest feature-jpeg ()
  (should (image-type-available-p 'jpeg)))

(ert-deftest feature-tiff ()
  (should (image-type-available-p 'tiff)))

(ert-deftest feature-svg ()
  (should (image-type-available-p 'svg)))
;;; feature.el ends here
