;;; epg-config-tests.el --- Test suite for epg.el  -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert)
(require 'epg-config)

(ert-deftest epg-config-test-epg-find-configuration ()
  (skip-unless (executable-find "gpg2"))
  (should (assq 'version (epg-find-configuration 'OpenPGP))))

(ert-deftest epg-config-test-epg-find-configuration/unknown-protocol ()
  (should-error (epg-find-configuration 'does-not-exist)))

(ert-deftest epg-config-test-epg-check-configuration ()
  (should (epg-check-configuration '((version . "1.0")) "0.9"))
  (should (epg-check-configuration '((version . "1.0")) "1.0"))
  (should-error (epg-check-configuration '((version . "1.0")) "1.1"))
  (should-error (epg-check-configuration '((version . "1.0")) 'foo))
  (should-error (epg-check-configuration '((version . "1.0")) "foo")))

(ert-deftest epg-config-test-epg-required-version-p ()
  (skip-unless (executable-find "gpg2"))
  (should (epg-required-version-p 'OpenPGP "1.0")))

(provide 'epg-config-tests)

;;; epg-config-tests.el ends here
