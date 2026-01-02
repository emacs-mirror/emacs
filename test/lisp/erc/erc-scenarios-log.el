;;; erc-scenarios-log.el --- erc-log scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
(require 'erc-truncate)

(defvar erc-timestamp-format-left)

(ert-deftest erc-scenarios-log--kill-hook ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/bouncer-history")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (tempdir (make-temp-file "erc-tests-log." t nil nil))
       (erc-log-channels-directory tempdir)
       (erc-modules (cons 'log erc-modules))
       (port (process-contact dumb-server :service))
       (logfile (expand-file-name (format "#chan!tester@127.0.0.1:%d.txt" port)
                                  tempdir))
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "foonet")))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
      (funcall expect 10 "was created on")
      (funcall expect 10 "please your lordship")
      (with-current-buffer "foonet"
        (delete-process erc-server-process)
        (funcall expect 5 "failed"))
      (should-not (file-exists-p logfile))
      (kill-buffer)
      (should (file-exists-p logfile)))

    (with-temp-buffer
      (insert-file-contents logfile)
      (funcall expect 1 "You have joined")
      (funcall expect 1 "Playback Complete.")
      (funcall expect 1 "please your lordship"))

    (erc-log-mode -1)
    (if noninteractive
        (delete-directory tempdir :recursive)
      (add-hook 'kill-emacs-hook
                (lambda () (delete-directory tempdir :recursive))))))

;; These next tests show that, in addition to truncating the buffer,
;; /CLEAR also syncs the log.  They differ from the tests further below
;; involving the `truncate' module in that, here, the upper truncation
;; boundary doesn't reside on an `erc--msg' char but rather on a newline
;; (the final one before `erc-insert-marker').  This was initially done
;; to safeguard `erc-last-saved-position' because `erc-insert-marker'
;; originally had a nil insertion type.  This staggered alignment means
;; truncation resulting from a /CLEAR actually demands more twiddling
;; and care than that triggered by the `truncate' module.
(ert-deftest erc-scenarios-log--cmd-clear/date-stamps ()
  :tags '(:expensive-test)
  (require 'erc-stamp)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/bouncer-history")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (tempdir (make-temp-file "erc-tests-log." t nil nil))
       (erc-scenarios-common-extra-teardown
        (and noninteractive
             (lambda ()
               (run-at-time 0 nil #'delete-directory tempdir :recursive))))
       (erc-log-channels-directory tempdir)
       (erc-modules (cons 'log erc-modules))
       (erc-timestamp-format-left "\n[%a %b %e %Y @@STAMP@@]\n")
       (port (process-contact dumb-server :service))
       (logfile (expand-file-name (format "#chan!tester@127.0.0.1:%d.txt" port)
                                  tempdir))
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter)))

    (unless noninteractive
      (add-hook 'kill-emacs-hook
                (lambda () (delete-directory tempdir :recursive))))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "foonet")))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
      (funcall expect 10 "@@STAMP@@")
      (funcall expect 10 "Grows, lives")
      (should-not (file-exists-p logfile))
      (goto-char (point-max))
      (erc-scenarios-common-say "/clear")
      (should (file-exists-p logfile))
      (funcall expect 10 "please your lordship")
      (ert-info ("Buffer truncated")
        (funcall expect 10 "@@STAMP@@" (goto-char (point-min))) ; reset
        ;; Requisite two blank lines plus date stamp are present.
        (should (string-prefix-p "\n\n\n[" (buffer-string)))
        (funcall expect -0.1 "Grows, lives")
        (funcall expect 1 "For these two")
        ;; Stamp resides just before `erc-last-saved-position'.
        (should (looking-back (rx "]\n<bob> alice: For these two")))
        (should (= erc-last-saved-position (1- (pos-bol))))))

    (ert-info ("Current contents saved")
      (with-temp-buffer
        (insert-file-contents logfile)
        (funcall expect 1 "@@STAMP@@")
        (funcall expect 1 "You have joined")
        (funcall expect 1 "Playback Complete.")
        (funcall expect 1 "Grows, lives")
        (funcall expect -0.001 "alice: For these two hours")))

    (ert-info ("Remainder saved, timestamp printed when option non-nil")
      (with-current-buffer "foonet"
        (delete-process erc-server-process)
        (funcall expect 5 "failed"))
      (kill-buffer "#chan")
      (with-temp-buffer
        (insert-file-contents logfile)
        (funcall expect 1 "@@STAMP@@")
        (funcall expect 1 "Grows, lives")
        (funcall expect -0.01 "@@STAMP@@")
        (forward-line 1) ; no blank, no timestamp
        (should (looking-at (rx "<bob> alice: For these two hours,")))
        (funcall expect 1 "please your lordship")))

    (erc-log-mode -1)))

(ert-deftest erc-scenarios-log--cmd-clear/left-stamps ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/bouncer-history")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (tempdir (make-temp-file "erc-tests-log." t nil nil))
       (erc-scenarios-common-extra-teardown
        (and noninteractive
             (lambda ()
               (run-at-time 0 nil #'delete-directory tempdir :recursive))))
       (erc-log-channels-directory tempdir)
       (erc-modules (cons 'log erc-modules))
       (erc-insert-timestamp-function #'erc-insert-timestamp-left)
       (erc-timestamp-only-if-changed-flag nil)
       (port (process-contact dumb-server :service))
       (logfile (expand-file-name (format "#chan!tester@127.0.0.1:%d.txt" port)
                                  tempdir))
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter)))

    (unless noninteractive
      (add-hook 'kill-emacs-hook
                (lambda () (delete-directory tempdir :recursive))))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "foonet")))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
      (funcall expect 10 "Grows, lives")
      (should (string-prefix-p "\n\n[" (buffer-string)))
      (should-not (file-exists-p logfile))
      (goto-char (point-max))
      (erc-scenarios-common-say "/clear")
      (should (file-exists-p logfile))
      (funcall expect 10 "please your lordship")

      ;; During truncation, `erc--clear-function' inserts exactly two
      ;; blanks, regardless of the following content.
      (ert-info ("Buffer truncated")
        (funcall expect -0.1 "Grows, lives")
        (funcall expect 1 "For these two" (goto-char (point-min)))
        (should (string-prefix-p "\n\n[" (buffer-string)))
        (should (looking-back (rx "]<bob> alice: For these two")))
        (should (= erc-last-saved-position 2))))

    (ert-info ("Current contents saved")
      (with-temp-buffer
        (insert-file-contents logfile)
        (should (string-prefix-p "[" (buffer-string)))
        (funcall expect 1 "]*** You have joined")
        (funcall expect 1 "Playback Complete.")
        (funcall expect 1 "]<alice> bob: Grows, lives")
        (funcall expect -0.001 "<bob> alice: For these two hours")))

    (ert-info ("Remainder saved, timestamp printed when option non-nil")
      (with-current-buffer "foonet"
        (delete-process erc-server-process)
        (funcall expect 5 "failed"))
      (kill-buffer "#chan")
      (with-temp-buffer
        (insert-file-contents logfile)
        (funcall expect 1 "]<alice> bob: Grows, lives")
        (forward-line 1) ; no blank, no timestamp
        (should (looking-at (rx "[" (+ (in ":0-9"))
                                "]<bob> alice: For these two hours,")))
        (funcall expect 1 "]<alice> bob: As't please your lordship")))

    (erc-log-mode -1)))

(defun erc-scenarios-log--truncate (assert-truncation assert-log)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/bouncer-history")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (tempdir (make-temp-file "erc-tests-log." t nil nil))
       (erc-log-channels-directory tempdir)
       (erc-modules (cons 'truncate (cons 'log erc-modules)))
       (erc-max-buffer-size 512)
       (erc-truncate-padding-size 512)
       (port (process-contact dumb-server :service))
       (logchan (expand-file-name (format "#chan!tester@127.0.0.1:%d.txt" port)
                                  tempdir))
       (logserv (expand-file-name
                 (format "127.0.0.1:%d!tester@127.0.0.1:%d.txt" port port)
                 tempdir))
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter)))

    (unless noninteractive
      (add-hook 'kill-emacs-hook
                (lambda () (delete-directory tempdir :recursive))))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (should-not (file-exists-p logserv))
        (should-not (file-exists-p logchan))
        ;; Verify that truncation actually happens where it should.
        (funcall assert-truncation expect)
        (should (file-exists-p logserv))))

    (ert-info ("Log file ahead of truncation point")
      ;; Log contains lines still present in buffer.
      (with-temp-buffer
        (insert-file-contents logserv)
        (funcall expect 10 "*** MAXLIST=beI:60")))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
      (funcall expect 10 "please your lordship")
      (should (file-exists-p logchan))
      (funcall expect -0.1 "[07:04:37] alice: Here," (point-min)))

    (ert-info ("Log ahead of truncation point")
      (with-temp-buffer
        (insert-file-contents logchan)
        (funcall expect 1 "You have joined")
        ;; No unwanted duplicates.
        (funcall expect 1 "<bob> [07:04:37] alice: Here,")
        (funcall expect -0.001 "<bob> [07:04:37] alice: Here,")
        (funcall expect 1 "<alice> [07:04:42] bob: By my troth")
        (funcall expect -0.001 "<alice> [07:04:42] bob: By my troth")
        (funcall expect 1 "I will grant it")
        (funcall assert-log expect)))

    (erc-log-mode -1)
    (erc-truncate-mode -1)
    (when noninteractive (delete-directory tempdir :recursive))))

(ert-deftest erc-scenarios-log--truncate ()
  :tags '(:expensive-test :unstable)
  (erc-scenarios-log--truncate

   (lambda (expect)
     (funcall expect 10 "*** MAXLIST=beI:60")
     (should (= (pos-bol) 22))
     ;; Exactly two + 1 (for date stamp) newlines preserved.
     (should (string-prefix-p "\n\n\n[" (buffer-string))))

   (lambda (expect)
     (funcall expect -0.001 "loathed enemy"))))

(ert-deftest erc-scenarios-log--truncate/left-stamps ()
  :tags '(:expensive-test :unstable)
  (let ((erc-insert-timestamp-function #'erc-insert-timestamp-left)
        (erc-timestamp-only-if-changed-flag nil))

    (erc-scenarios-log--truncate

     (lambda (expect)
       ;; Exactly two leading newlines preserved.
       (funcall expect 10
                '(: "\n\n[" (= 5 (in "0-9:")) "]*** There are 0 users")))

     (lambda (expect)
       (funcall expect 1 "loathed enemy")
       (funcall expect -0.001 "please your lordship")))))

(defvar erc-insert-timestamp-function)
(declare-function erc-insert-timestamp-left "erc-stamp" (string))

(ert-deftest erc-scenarios-log--save-buffer-in-logs/truncate-on-save ()
  :tags '(:expensive-test)
  (with-suppressed-warnings ((obsolete erc-truncate-buffer-on-save))
    (erc-scenarios-common-with-cleanup
        ((erc-scenarios-common-dialog "base/assoc/bouncer-history")
         (dumb-server (erc-d-run "localhost" t 'foonet))
         (tempdir (make-temp-file "erc-tests-log." t nil nil))
         (erc-log-channels-directory tempdir)
         (erc-modules (cons 'log erc-modules))
         (port (process-contact dumb-server :service))
         (erc-truncate-buffer-on-save t)
         (logchan (expand-file-name (format "#chan!tester@127.0.0.1:%d.txt"
                                            port)
                                    tempdir))
         (erc-server-flood-penalty 0.1)
         (erc-insert-timestamp-function #'erc-insert-timestamp-left)
         (expect (erc-d-t-make-expecter)))

      (unless noninteractive
        (add-hook 'kill-emacs-hook
                  (lambda () (delete-directory tempdir :recursive))))

      (ert-info ("Connect to foonet")
        (with-current-buffer (erc :server "127.0.0.1"
                                  :port port
                                  :nick "tester"
                                  :password "foonet:changeme"
                                  :full-name "tester")
          (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
        (funcall expect 10 "<someone> [07:04:10] hi everyone")
        (should-not (file-exists-p logchan))
        ;; Simulate an M-x erc-save-buffer-in-logs RET
        (cl-letf (((symbol-function 'called-interactively-p) #'always))
          (call-interactively #'erc-save-buffer-in-logs))
        (should (file-exists-p logchan))
        (funcall expect 10 "<alice> bob: As't please your lordship")
        (erc-save-buffer-in-logs)
        ;; Not truncated when called by lisp code.
        (should (> (buffer-size) 400)))

      (ert-info ("No double entries")
        (with-temp-buffer
          (insert-file-contents logchan)
          (funcall expect 0.1 "hi everyone")
          (funcall expect -0.1 "hi everyone")
          (funcall expect 0.1 "Playback Complete")
          (funcall expect -0.1 "Playback Complete")
          (funcall expect 10 "<alice> bob: As't")))

      (erc-log-mode -1)
      (when noninteractive (delete-directory tempdir :recursive)))))

(ert-deftest erc-scenarios-log--write-after-insert ()
  :tags '(:expensive-test :unstable)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/bouncer-history")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (tempdir (make-temp-file "erc-tests-log." t nil nil))
       (erc-log-channels-directory tempdir)
       (erc-modules `(truncate log ,@erc-modules))
       (erc-max-buffer-size 512)
       (erc-log-write-after-insert t)
       (erc-timestamp-format-left "\n[@@DATE__STAMP@@]\n")
       (erc-truncate-padding-size 512)
       (port (process-contact dumb-server :service))
       (logchan (expand-file-name (format "#chan!tester@127.0.0.1:%d.txt" port)
                                  tempdir))
       (logserv (expand-file-name
                 (format "127.0.0.1:%d!tester@127.0.0.1:%d.txt" port port)
                 tempdir))
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter)))

    (unless noninteractive
      (add-hook 'kill-emacs-hook
                (lambda () (delete-directory tempdir :recursive))))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        ;; File already exists because `erc-log-write-after-insert' is
        ;; non-nil.
        (should (file-exists-p logserv))
        (should-not (file-exists-p logchan))
        ;; Verify that truncation actually happens where it should.
        (funcall expect 10 "*** MAXLIST=beI:60")
        (should (= (pos-bol) 22))
        ;; Exactly two + 1 (for date stamp) newlines preserved.
        (should (string-prefix-p "\n\n\n[" (buffer-string)))))

    (ert-info ("Log file ahead of truncation point")
      ;; Log contains lines still present in buffer.
      (with-temp-buffer
        (insert-file-contents logserv)
        (funcall expect 10 "@@DATE__STAMP@@")
        (funcall expect 10 "*** MAXLIST=beI:60")))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
      (funcall expect 10 "@@DATE__STAMP@@")
      (funcall expect 10 "please your lordship")
      (should (file-exists-p logchan))
      (funcall expect -0.1 "[07:04:37] alice: Here," (point-min)))

    (ert-info ("Log ahead of truncation point")
      (with-temp-buffer
        (insert-file-contents logchan)
        (funcall expect 10 "@@DATE__STAMP@@")
        (funcall expect 1 "You have joined")
        ;; No unwanted duplicates.
        (funcall expect 1 "<bob> [07:04:37] alice: Here,")
        (funcall expect -0.001 "<bob> [07:04:37] alice: Here,")
        (funcall expect 1 "<alice> [07:04:42] bob: By my troth")
        (funcall expect -0.001 "<alice> [07:04:42] bob: By my troth")
        (funcall expect 1 "I will grant it")

        ;; Writes happen instantly because `erc-log-write-after-insert'
        ;; is non-nil.  Compare to `erc-scenarios-log--truncate' above.
        (funcall expect 1 "loathed enemy")))

    (erc-log-mode -1)
    (erc-truncate-mode -1)
    (when noninteractive (delete-directory tempdir :recursive))))

;;; erc-scenarios-log.el ends here
