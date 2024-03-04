;;; erc-scenarios-join-auth-source.el --- join-auth-source scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

;; TODO add another test with autojoin and channel keys

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(ert-deftest erc-scenarios-join-auth-source--network ()
  :tags '(:expensive-test)
  (should erc-auth-source-join-function)
  (erc-scenarios-common-with-cleanup
      ((entries
        '("machine 127.0.0.1 port %d login \"#foo\" password spam"
          "machine irc.foonet.org port %d login tester password fake"
          "machine irc.foonet.org login \"#spam\" password secret"
          "machine foonet port %d login dummy password fake"
          "machine 127.0.0.1 port %d login dummy password changeme"))
       (erc-scenarios-common-dialog "join/auth-source")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       (ents (mapcar (lambda (fmt) (format fmt port)) entries))
       (netrc-file (make-temp-file "auth-source-test" nil nil
                                   (string-join ents "\n")))
       (auth-sources (list netrc-file))
       (auth-source-do-cache nil)
       (expect (erc-d-t-make-expecter))
       (erc-scenarios-common-extra-teardown (lambda ()
                                              (delete-file netrc-file))))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "dummy"
                                :full-name "dummy")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (erc-d-t-wait-for 8 (eq erc-network 'foonet))
        (funcall expect 10 "user modes")
        (erc-scenarios-common-say "/JOIN #spam")))

    (ert-info ("Join #spam")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
        (funcall expect 10 "#spam was created on")))))

;;; erc-scenarios-join-auth-source.el ends here
