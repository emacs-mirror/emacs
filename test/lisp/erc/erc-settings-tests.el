;;; erc-settings-tests.el --- Tests for erc-settings  -*- lexical-binding:t -*-

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

;;; Code:

(require 'erc-settings)
(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-tests-common)))

;; This test does some Custom type checking via `setopt'.
(ert-deftest erc-settings ()
  (when (< emacs-major-version 29) (ert-skip "Needs `setopt'"))

  (cl-letf* ((erc-settings ())
             ((symbol-function 'set-default) #'set)
             ((symbol-function 'warn) #'error))

    (setopt erc-settings `(((or (network . foonet)
                                ,(rx bot "127.0.0.1:"))
                            (erc-foo 1))
                           ((and ,(rx bot "#emacs")
                                 (not ,(rx bot "#emacs-devel")))
                            (erc-foo 2)
                            (erc-bar 3))))

    ;; Realistic predicates and bindings.
    (setopt erc-settings '(((and (id . foonet)
                                 (not erc-server-process-alive))
                            (erc-prompt "ERC! ")
                            (erc-sasl-user :nick))
                           (erc-open-server-buffer-p
                            (erc-autojoin-channels-alist (("." "#chan"))))
                           ((or "#chan" "#spam" erc-query-buffer-p)
                            (erc-modules `(keep-place-indicator ,@erc-modules)
                                         :eval))))

    ;; Network can also be a regexp.
    (setopt erc-settings `(((or ,(rx bot (| "localhost" "127.0.0.1") ":")
                                (network . foonet))
                            (erc-foo 1))
                           ((or ,(rx bot "#emacs") "#erc")
                            (erc-foo 2)))))

  (should (equal erc-settings ())))

(ert-deftest erc-settings--gather-bindings ()

  ;; Match by network only.
  (with-current-buffer (get-buffer-create "foonet")
    (setq erc-network 'foonet)
    (let ((erc-settings '(((or (network . barnet) ignore nil))
                          ((and (network . foonet) always t)
                           (erc-foo 1) (erc-bar 2))
                          ((network . baznet)))))
      (should (equal '((erc-foo 1) (erc-bar 2))
                     (erc-settings--gather-bindings (current-buffer)))))
    (kill-buffer))

  ;; Multiple regexps.
  (with-current-buffer (get-buffer-create "localhost:6697")
    (let ((erc-settings '(((network . barnet))
                          ("\\`localhost"
                           (erc-foo 1))
                          ("."
                           (erc-bar 2)))))
      (should (equal '((erc-foo 1) (erc-bar 2))
                     (erc-settings--gather-bindings (current-buffer))))
      (kill-buffer)))

  ;; One regexp, one network.
  (with-current-buffer (get-buffer-create "foonet")
    (setq erc-network 'foonet)
    (let ((erc-settings `((,(rx (| "foonet" "barnet"))
                           (erc-foo 1))
                          ((and (network . foonet) (name . "foonet"))
                           (erc-bar 2)))))
      (should (equal '((erc-foo 1) (erc-bar 2))
                     (erc-settings--gather-bindings (current-buffer)))))
    (kill-buffer)))

(ert-deftest erc-settings--extract-ids ()
  (should-not (erc-settings--extract-ids))

  ;; Does not deduplicate.
  (let ((erc-settings '(((id . a)
                         (erc-foo 0))
                        ("."
                         (erc-foo 1))
                        ((and t nil)
                         (erc-foo 2))
                        ((and ignore (id . b))
                         (erc-foo 3))
                        ((or (not (id . c)) (and (id . d) ignore))
                         (erc-foo 4))
                        ((not (id . a))
                         (erc-foo 5)))))
    (should (equal (erc-settings--extract-ids) '(a b c d a)))))

;;; erc-settings-tests.el ends here
