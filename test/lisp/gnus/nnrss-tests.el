;;; nnrss-tests.el --- tests for gnus/nnrss.el    -*- lexical-binding:t -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

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
(require 'nnrss)

(ert-deftest test-nnrss-normalize ()
  (should (equal (nnrss-normalize-date "2004-09-17T05:09:49.001+00:00")
                 "Fri, 17 Sep 2004 05:09:49 +0000")))

(defconst test-nnrss-xml
  '((rss
     ((version . "2.0")
      (xmlns:dc . "http://purl.org/dc/elements/1.1/"))
     (channel
      ((xmlns:content . "http://purl.org/rss/1.0/modules/content/"))))))

(ert-deftest test-nnrss-namespace-top ()
  (should (equal (nnrss-get-namespace-prefix
                  test-nnrss-xml "http://purl.org/dc/elements/1.1/")
                 "dc:")))
(ert-deftest test-nnrss-namespace-inner ()
  (should (equal (nnrss-get-namespace-prefix
                  test-nnrss-xml "http://purl.org/rss/1.0/modules/content/")
                 "content:")))

;;; nnrss-tests.el ends here
