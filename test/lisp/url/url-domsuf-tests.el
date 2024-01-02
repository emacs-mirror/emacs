;;; url-domsuf-tests.el --- Tests for url-domsuf.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

(require 'url-domsuf)
(require 'ert)

(defun url-domsuf-tests--run ()
  (should-not (url-domsuf-cookie-allowed-p "com"))
  (should (url-domsuf-cookie-allowed-p "foo.bar.bd"))
  (should-not (url-domsuf-cookie-allowed-p "bar.bd"))
  (should-not (url-domsuf-cookie-allowed-p "co.uk"))
  (should (url-domsuf-cookie-allowed-p "foo.bar.hokkaido.jo"))
  (should-not (url-domsuf-cookie-allowed-p "bar.yokohama.jp"))
  (should (url-domsuf-cookie-allowed-p "city.yokohama.jp")))

(ert-deftest url-domsuf-test-cookie-allowed-p ()
  "Run the domsuf tests without need for parsing a file."
  (let ((url-domsuf-domains '(("com")
                              ("bar.bd")
                              ("co.uk")
                              ("bar.yokohama.jp"))))
    (url-domsuf-tests--run)))

(ert-deftest url-domsuf-test-cookie-allowed-p/and-parse ()
  "Run the domsuf tests, but also parse the file."
  :tags '(:expensive-test)
  (url-domsuf-tests--run))

(provide 'url-domsuf-tests)

;;; url-domsuf-tests.el ends here
