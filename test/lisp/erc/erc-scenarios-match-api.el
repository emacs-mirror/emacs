;;; erc-scenarios-match-api.el --- `erc-match-functions' scenarios -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-match)
(require 'erc-stamp)

(defun erc-scenarios-match-api--test-fn (&rest plist)
  (apply #'erc-match
         :predicate #'always
         :handler (lambda (m)
                    (let ((body (erc-match-get-message-body m))
                          (cmd (format (if (numberp (erc-match-command m))
                                           "%03i"
                                         "%s")
                                       (erc-match-command m)))
                          (nick (or (erc-match-nick m) "?")))
                      (with-current-buffer "*erc-match test matches*"
                        (save-excursion
                          (goto-char (point-max))
                          (insert cmd " " nick ": " body "\n")))))
         plist))

(defun erc-scenarios-match-api--custom-match-functions ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "match/functions")
       (dumb-server (erc-d-run "localhost" t 'custom))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.1)
       (erc-autojoin-channels-alist '((foonet "#chan")))
       (match-buffer (get-buffer-create "*erc-match test matches*"))
       (erc-match-functions
        (cons #'erc-scenarios-match-api--test-fn erc-match-functions))
       ;; Shadow this date stamp instead of mocking the time function.
       ;; The version numbers in the QUIT message don't need this
       ;; because they're printed verbatim from the server response.
       (erc-message-english-s329 "%c was created on @@DATESTAMP@@"))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :full-name "tester"
                                :user "tester"
                                :nick "tester")
        (funcall expect 5 "debug")))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
      (funcall expect 10 "<alice> There were none principal")
      (erc-scenarios-common-say "/query bob"))

    (with-current-buffer "bob"
      (erc-scenarios-common-say "hi")
      (funcall expect 10 "<bob> As much as ever Coriolanus did"))

    (with-current-buffer "foonet"
      (erc-scenarios-common-say "/msg NickServ help identify")
      (funcall expect 10 "End of NickServ HELP"))

    (with-current-buffer "#chan"
      (funcall expect 10 "<alice> bob: Thus men may grow wiser")
      (erc-scenarios-common-say "/quit")
      (funcall expect 10 "quit: Quit:"))

    (with-current-buffer "foonet"
      (funcall expect 10 "==> ERROR"))

    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "match/functions/custom-match-log"
                         (ert-resource-directory)))
      (let ((expected (buffer-string)))
        (with-current-buffer "*erc-match test matches*"
          (should (equal expected (buffer-string))))))))

;; These tests primarily ensure that the various fields of the
;; `erc-match' object, like `spkr-beg', as well as associated utilities,
;; like `erc-match-get-message-body', work as expected.  It defines a
;; custom `erc-match-functions' member that prints a summary of every
;; displayed message to its own buffer.  That buffer's contents appear
;; in the file resources/match/functions/custom-match-log.

(ert-deftest erc-scenarios-match-api--custom-functions/basic ()
  :tags '(:expensive-test)
  (erc-scenarios-match-api--custom-match-functions))

(ert-deftest erc-scenarios-match-api--custom-functions/fill-wrap ()
  :tags '(:expensive-test)
  (let ((erc-modules (cons 'fill-wrap erc-modules)))
    (erc-scenarios-match-api--custom-match-functions)))

(ert-deftest erc-scenarios-match-api--custom-functions/left-stamps ()
  :tags '(:expensive-test)
  (let ((erc-insert-timestamp-function #'erc-insert-timestamp-left)
        (erc-timestamp-only-if-changed-flag nil))
    (erc-scenarios-match-api--custom-match-functions)))

(ert-deftest erc-scenarios-match-api--custom-functions/left-stamps/fill-wrap ()
  :tags '(:expensive-test)
  (let ((erc-insert-timestamp-function #'erc-insert-timestamp-left)
        (erc-timestamp-only-if-changed-flag nil)
        (erc-modules (cons 'fill-wrap erc-modules)))
    (erc-scenarios-match-api--custom-match-functions)))

;;; erc-scenarios-match-api.el ends here
