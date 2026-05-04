;;; erc-scenarios-log-options.el --- erc-log options scenarios -*- lexical-binding: t -*-

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

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-log)

;; This checks that logs are only inserted in new buffers.
(defun erc-scenarios-log-options--insert-on-open (test-fn)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "join/reconnect")
       (dumb-server (erc-d-run "localhost" t 'foonet 'foonet-again))
       (tempdir (make-temp-file "erc-tests-log." t nil nil))
       (erc-log-channels-directory tempdir)
       (erc-modules `(log ,@erc-modules))
       (erc-timestamp-format-left "\n[@@DATE__STAMP@@]\n")
       (port (process-contact dumb-server :service))
       (erc-server-auto-reconnect t)
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter))
       ;; Bind these so they'll be killed on teardown.
       (server-log-buffer (get-buffer-create "*erc-log FooNet*"))
       (chan-log-buffer (get-buffer-create "*erc-log #chan*"))
       (uh-suffix (format "!tester@127.0.0.1:%d.txt" port)))

    (let ((file (concat "127.0.0.1:" (number-to-string port) uh-suffix)))
      (with-temp-file (expand-file-name file tempdir)
        (insert "\n@@OLD__BEG@@\n" "file: " file "\n@@OLD__END@@\n\n")))

    (with-temp-file (expand-file-name (concat "foonet" uh-suffix) tempdir)
      (insert "\n@@OLD__BEG@@\n"
              "file: " (concat "foonet" uh-suffix)
              "\n@@OLD__END@@\n\n"))

    (with-temp-file (expand-file-name (concat "#chan" uh-suffix) tempdir)
      (insert "\n@@OLD__BEG@@\n"
              "file: " (concat "#chan" uh-suffix)
              "\n@@OLD__END@@\n\n"))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (funcall expect 10 "debug mode")))

    (ert-info ("#chan populated")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "@@DATE__STAMP@@")
        (funcall expect 10 "<alice> tester, welcome")))

    (ert-info ("Reconnect")
      (with-current-buffer "FooNet"
        (funcall expect 10 "Connection failed!")
        (funcall expect 10 "Reconnecting")
        (funcall expect 10 "Welcome")
        (funcall expect 10 "debug mode")))

    (with-current-buffer "#chan"
      (funcall expect -0.01 "@@DATE__STAMP@@")
      (funcall expect 10 "<alice> bob: Well, this"))

    (with-current-buffer "FooNet"
      (erc-scenarios-common-say "/quit")
      (funcall expect 10 "ERROR"))

    ;; Ensure no redundant logging.
    (with-current-buffer "FooNet"
      (let ((file (erc-current-logfile (current-buffer))))
        (with-current-buffer server-log-buffer
          (insert-file-contents file)
          (funcall expect 1 "@@DATE__STAMP@@")
          (funcall expect -0.01 "@@DATE__STAMP@@")
          ;; Full output again on reconnect.
          (funcall expect 1 "*** Welcome to the foonet")
          (funcall expect 1 "debug mode")
          (funcall expect 1 "*** Connection failed!")
          (funcall expect 1 "*** Welcome to the foonet")
          (funcall expect 1 "debug mode"))))

    (with-current-buffer "#chan"
      (let ((file (erc-current-logfile (current-buffer))))
        (with-current-buffer chan-log-buffer
          (insert-file-contents file)
          (funcall expect 1 "@@OLD__BEG@@")
          (funcall expect -0.01 "@@OLD__BEG@@") ; only appearance
          (funcall expect 1 "file: #chan")
          (funcall expect 1 "@@OLD__END@@")
          (funcall expect 1 "@@DATE__STAMP@@")
          (funcall expect -0.01 "@@DATE__STAMP@@")
          ;; Full output again on reconnect.
          (funcall expect 1 "*** You have joined channel #chan")
          (funcall expect 1 "<alice> tester, welcome!")
          (funcall expect -0.01 "@@DATE__STAMP@@")
          (funcall expect 1 "*** You have joined channel #chan")
          (funcall expect 1 "<alice> bob: Well, this is the forest"))))

    (funcall test-fn expect)
    (erc-log-mode -1)

    (if noninteractive
        (delete-directory tempdir :recursive)
      (add-hook 'kill-emacs-hook
                (lambda () (delete-directory tempdir :recursive))))))

;; This shows that the legacy default value of t ends up inserting
;; existing log content on reconnect as well, leading to redundant
;; insertions.
(ert-deftest erc-scenarios-log-options--insert-on-open/default ()
  (let ((erc-log-insert-log-on-open t))
    (erc-scenarios-log-options--insert-on-open
     (lambda (expect)

       (with-current-buffer "*erc-log FooNet*"
         (funcall expect 1 "@@OLD__BEG@@" (point-min))
         (funcall expect -0.01 "@@OLD__BEG@@") ; not repeated
         (funcall expect 1 "file: foonet")
         (funcall expect 1 "@@OLD__END@@")
         (funcall expect 1 "@@DATE__STAMP@@"))

       ;; For server buffers, the file name changes because the buffer
       ;; is renamed after the network is announced during the initial
       ;; session.
       (with-current-buffer "FooNet"

         (funcall expect 1 "@@OLD__BEG@@" (point-min))
         ;; Existing log contents inserted once per connection (most
         ;; recent first).
         (funcall expect 1 "file: foonet")
         (funcall expect 1 "@@OLD__END@@")

         ;; Insertion from initial connection last.
         (funcall expect 1 "@@OLD__BEG@@")
         (funcall expect 1 "file: 127.0.0.1")
         (funcall expect -0.01 "@@OLD__BEG@@")
         (funcall expect 1 "@@OLD__END@@")

         (funcall expect 1 "@@DATE__STAMP@@"))

       (ert-info ("Repeated in #chan")
         (with-current-buffer "#chan"
           (funcall expect 1 "@@OLD__BEG@@" (point-min))
           (funcall expect 1 "file: #chan")
           (funcall expect 1 "@@OLD__END@@")

           ;; Existing log is indeed repeated in full once per connection.
           (funcall expect 1 "@@OLD__BEG@@")
           (funcall expect -0.01 "@@OLD__BEG@@")
           (funcall expect 1 "file: #chan")
           (funcall expect 1 "@@OLD__END@@")

           (funcall expect 1 "@@DATE__STAMP@@")))))))

(ert-deftest erc-scenarios-log-options--insert-on-open/target-p ()
  (let ((erc-log-insert-log-on-open #'erc-log-new-target-buffer-p))
    (erc-scenarios-log-options--insert-on-open
     (lambda (expect)

       (ert-info ("Absent from server buffer")
         (with-current-buffer "FooNet"
           ;; No insertions in the server buffer.
           (funcall expect -0.01 "@@OLD__BEG@@" (point-min))
           (funcall expect 1 "@@DATE__STAMP@@")))

       (ert-info ("Once in server log")
         ;; No redundancies in the server's log file, though previously
         ;; existing content is obviously present.
         (with-current-buffer "*erc-log FooNet*"
           (funcall expect 1 "@@OLD__BEG@@" (point-min))
           (funcall expect -0.01 "@@OLD__BEG@@") ; no repeats
           (funcall expect 1 "file: foonet")
           (funcall expect 1 "@@OLD__END@@")
           (funcall expect 1 "@@DATE__STAMP@@")))

       ;; Also, as asserted in the fixture body, the associated log file
       ;; for #chan has no redundancy.
       (ert-info ("Not repeated in #chan")
         (with-current-buffer "#chan"
           (funcall expect 1 "@@OLD__BEG@@" (point-min))
           (funcall expect 1 "file: #chan")
           (funcall expect 1 "@@OLD__END@@")

           ;; Existing scrollback only inserted at most once per buffer.
           (funcall expect -0.01 "@@OLD__BEG@@")
           (funcall expect 1 "@@DATE__STAMP@@")))))))

(require 'erc-log)

;;; erc-scenarios-log-options.el ends here
