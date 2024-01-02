;;; bug-reference-tests.el --- Tests for bug-reference.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'bug-reference)
(require 'ert)

(defun test--get-github-entry (url)
  (and (string-match
	(car (bug-reference--build-forge-setup-entry
              "github.com" 'github "https"))
        url)
       (match-string 1 url)))

(defun test--get-gitlab-entry (url)
  (and (string-match
	(car (bug-reference--build-forge-setup-entry
              "gitlab.com" 'gitlab "https"))
        url)
       (match-string 1 url)))

(defun test--get-gitea-entry (url)
  (and (string-match
	(car (bug-reference--build-forge-setup-entry
              "gitea.com" 'gitea "https"))
        url)
       (match-string 1 url)))

(ert-deftest test-github-entry ()
  (should
   (equal
    (test--get-github-entry "git@github.com:larsmagne/csid.git")
    "larsmagne/csid"))
  (should
   (equal
    (test--get-github-entry "git@github.com:larsmagne/csid")
    "larsmagne/csid"))
  (should
   (equal
    (test--get-github-entry "https://github.com/magit/magit.git")
    "magit/magit"))
  (should
   (equal
    (test--get-github-entry "https://github.com/magit/magit.git/")
    "magit/magit"))
  (should
   (equal
    (test--get-github-entry "https://github.com/magit/magit")
    "magit/magit"))
  (should
   (equal
    (test--get-github-entry "https://github.com/magit/magit/")
    "magit/magit")))

(ert-deftest test-gitlab-entry ()
  (should
   (equal
    (test--get-gitlab-entry "git@gitlab.com:larsmagne/csid.git")
    "larsmagne/csid"))
  (should
   (equal
    (test--get-gitlab-entry "git@gitlab.com:larsmagne/csid")
    "larsmagne/csid"))
  (should
   (equal
    (test--get-gitlab-entry "https://gitlab.com/magit/magit.git")
    "magit/magit"))
  (should
   (equal
    (test--get-gitlab-entry "https://gitlab.com/magit/magit.git/")
    "magit/magit"))
  (should
   (equal
    (test--get-gitlab-entry "https://gitlab.com/magit/magit")
    "magit/magit"))
  (should
   (equal
    (test--get-gitlab-entry "https://gitlab.com/magit/magit/")
    "magit/magit")))

(ert-deftest test-gitea-entry ()
  (should
   (equal
    (test--get-gitea-entry "git@gitea.com:larsmagne/csid.git")
    "larsmagne/csid"))
  (should
   (equal
    (test--get-gitea-entry "git@gitea.com:larsmagne/csid")
    "larsmagne/csid"))
  (should
   (equal
    (test--get-gitea-entry "https://gitea.com/magit/magit.git")
    "magit/magit"))
  (should
   (equal
    (test--get-gitea-entry "https://gitea.com/magit/magit.git/")
    "magit/magit"))
  (should
   (equal
    (test--get-gitea-entry "https://gitea.com/magit/magit")
    "magit/magit"))
  (should
   (equal
    (test--get-gitea-entry "https://gitea.com/magit/magit/")
    "magit/magit")))

;;; bug-reference-tests.el ends here
