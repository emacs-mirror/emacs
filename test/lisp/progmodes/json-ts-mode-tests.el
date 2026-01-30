;;; json-ts-mode-tests.el --- Tests for json-ts-mode.el  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Tests for json-ts-mode.

;;; Code:

(require 'ert)
(require 'treesit)
(require 'json-ts-mode)

(ert-deftest json-ts-mode-test-path-at-point ()
  "Test `json-ts--get-path-at-node' and `json-ts--path-to-jq'."
  (skip-unless (treesit-language-available-p 'json))
  (with-temp-buffer
    (json-ts-mode)
    (insert "{\"a\": [1, {\"b\": 2}, 3]}")

    ;; Point at '1' (index 0 of array 'a')
    (goto-char (point-min))
    (search-forward "1")
    (backward-char)
    (should (equal (json-ts--path-to-jq (json-ts--get-path-at-node (treesit-node-at (point))))
                   ".a[0]"))

    ;; Point at '2' (key 'b' inside object at index 1)
    (goto-char (point-min))
    (search-forward "2")
    (backward-char)
    (should (equal (json-ts--path-to-jq (json-ts--get-path-at-node (treesit-node-at (point))))
                   ".a[1].b"))

    ;; Point at '3' (index 2 of array 'a')
    (goto-char (point-min))
    (search-forward "3")
    (backward-char)
    (should (equal (json-ts--path-to-jq (json-ts--get-path-at-node (treesit-node-at (point))))
                   ".a[2]"))))

(ert-deftest json-ts-mode-test-path-at-point-complex-keys ()
  "Test path generation with complex keys."
  (skip-unless (treesit-language-available-p 'json))
  (with-temp-buffer
    (json-ts-mode)
    (insert "{\"key.with.dot\": {\"key with space\": 1}}")

    (goto-char (point-min))
    (search-forward "1")
    (backward-char)
    (should (equal (json-ts--path-to-jq (json-ts--get-path-at-node (treesit-node-at (point))))
                   "[\"key.with.dot\"][\"key with space\"]"))))

(ert-deftest json-ts-mode-test-jq-path-keys ()
  "Test `json-ts--path-to-jq' with various key formats."
  (should (equal (json-ts--path-to-jq '("v123")) ".v123"))
  (should (equal (json-ts--path-to-jq '("-123")) "[\"-123\"]"))
  (should (equal (json-ts--path-to-jq '("v_v")) ".v_v"))
  (should (equal (json-ts--path-to-jq '("123")) "[\"123\"]"))
  (should (equal (json-ts--path-to-jq '("_123")) "._123"))
  (should (equal (json-ts--path-to-jq '("1v2")) "[\"1v2\"]")))

(ert-deftest json-ts-mode-test-path-to-python ()
  "Test `json-ts--path-to-python'."
  (should (equal (json-ts--path-to-python '("a" 0 "b"))
                 "[\"a\"][0][\"b\"]")))

(provide 'json-ts-mode-tests)
;;; json-ts-mode-tests.el ends here
