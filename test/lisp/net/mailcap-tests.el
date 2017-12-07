;;; mailcap-tests.el --- tests for mailcap.el -*- lexical-binding: t -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Mark Oteiza <mvoteiza@udel.edu>

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

(require 'ert)
(require 'mailcap)

(defconst mailcap-tests-data-dir
  (expand-file-name "test/data/mailcap" source-directory))

(defconst mailcap-tests-path
  (expand-file-name "mime.types" mailcap-tests-data-dir)
  "String used as PATH argument of `mailcap-parse-mimetypes'.")

(defconst mailcap-tests-mime-extensions (copy-alist mailcap-mime-extensions))

(defconst mailcap-tests-path-extensions
  '((".wav" . "audio/x-wav")
    (".flac" . "audio/flac")
    (".opus" . "audio/ogg"))
  "Alist of MIME associations in `mailcap-tests-path'.")

(ert-deftest mailcap-mimetypes-parsed-p ()
  (should (null mailcap-mimetypes-parsed-p)))

(ert-deftest mailcap-parse-empty-path ()
  "If PATH is empty, this should be a noop."
  (mailcap-parse-mimetypes "file/that/should/not/exist" t)
  (should mailcap-mimetypes-parsed-p)
  (should (equal mailcap-mime-extensions mailcap-tests-mime-extensions)))

(ert-deftest mailcap-parse-path ()
  (let ((mimetypes (getenv "MIMETYPES")))
    (unwind-protect
        (progn
          (setenv "MIMETYPES" mailcap-tests-path)
          (mailcap-parse-mimetypes nil t))
      (setenv "MIMETYPES" mimetypes)))
  (should (equal mailcap-mime-extensions
                 (append mailcap-tests-path-extensions
                         mailcap-tests-mime-extensions)))
  ;; Already parsed this, should be a noop
  (mailcap-parse-mimetypes mailcap-tests-path)
  (should (equal mailcap-mime-extensions
                 (append mailcap-tests-path-extensions
                         mailcap-tests-mime-extensions))))

;;; mailcap-tests.el ends here
