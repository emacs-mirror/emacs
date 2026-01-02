;;; erc-dcc-tests.el --- Tests for erc-dcc  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'ert-x)
(require 'erc-dcc)
(require 'erc-pcomplete)

(ert-deftest erc-dcc-ctcp-query-send-regexp ()
  (let ((s "DCC SEND \"file name\" 2130706433 9899 1405135128"))
    (should (string-match erc-dcc-ctcp-query-send-regexp s))
    (should-not (match-string 2 s))
    (should (string= "file name" (match-string 1 s)))
    (should (string= "SEND" (match-string 6 s))))
  (let ((s "DCC SEND \"file \\\" name\" 2130706433 9899 1405135128"))
    (should (string-match erc-dcc-ctcp-query-send-regexp s))
    (should-not (match-string 2 s))
    (should (string= "SEND" (match-string 6 s)))
    (should (string= "file \" name"
                     (erc-dcc-unquote-filename (match-string 1 s)))))
  (let ((s "DCC SEND filename 2130706433 9899 1405135128"))
    (should (string-match erc-dcc-ctcp-query-send-regexp s))
    (should (string= "filename" (match-string 2 s)))
    (should (string= "2130706433" (match-string 3 s)))
    (should (string= "9899" (match-string 4 s)))
    (should (string= "1405135128" (match-string 5 s))))
  (let ((s "DCC TSEND filename 2130706433 9899 1405135128"))
    (should (string-match erc-dcc-ctcp-query-send-regexp s))
    (should (string= "TSEND" (match-string 6 s)))))

;; This also indirectly tests base functionality for
;; `erc-dcc-do-LIST-command'

(defun erc-dcc-tests--dcc-handle-ctcp-send (turbo)
  (let (erc-send-completed-hook
        erc-insert-modify-hook
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
    (with-current-buffer (get-buffer-create "fake-server")
      (erc-mode)
      (setq erc-server-process
            (start-process "fake" (current-buffer) "sleep" "10")
            erc-server-current-nick "dummy")
      (erc--initialize-markers (point) nil)
      (set-process-query-on-exit-flag erc-server-process nil)
      (should-not erc-dcc-list)
      (erc-ctcp-query-DCC erc-server-process
                          "tester"
                          "~tester"
                          "fake.irc"
                          "dummy"
                          (concat "DCC " (if turbo "TSEND" "SEND")
                                  " foo 2130706433 9899 1405135128"))
      (should-not (cdr erc-dcc-list))
      (should (equal (plist-put (car erc-dcc-list) :parent 'fake)
                     `(:nick "tester!~tester@fake.irc"
                             :type GET
                             :peer nil
                             :parent fake
                             :ip "127.0.0.1"
                             :port "9899"
                             :file "foo"
                             :size 1405135128
                             :turbo ,(and turbo t)
                             :secure nil)))
      (goto-char (point-min))
      (should (search-forward "file foo offered by tester" nil t))
      (erc-dcc-do-LIST-command erc-server-process)
      (should (search-forward-regexp (concat
                                      "GET +no +1405135128 +foo"
                                      (and turbo " +(T)") "$")
                                     nil t))
      (when noninteractive
        (kill-buffer))))
  ;; `erc-dcc-list' is global; must leave it empty
  (should erc-dcc-list)
  (setq erc-dcc-list nil))

(ert-deftest erc-dcc-handle-ctcp-send--base ()
  (erc-dcc-tests--dcc-handle-ctcp-send nil))

(ert-deftest erc-dcc-handle-ctcp-send--turbo ()
  (erc-dcc-tests--dcc-handle-ctcp-send t))

(defun erc-dcc-tests--erc-dcc-do-GET-command (file &optional sep nuh)
  (unless nuh (setq nuh "tester!~tester@fake.irc"))
  (with-temp-buffer
    (let* ((proc (start-process "fake" (current-buffer) "sleep" "10"))
           (elt (list :nick nuh
                      :type 'GET
                      :peer nil
                      :parent proc
                      :ip "127.0.0.1"
                      :port "9899"
                      :file file
                      :size 1405135128))
           (nic (erc-extract-nick nuh))
           (erc-dcc-list (list elt))
           ;;
           erc-accidental-paste-threshold-seconds
           erc-insert-modify-hook erc-send-completed-hook
           erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook
           calls)
      (erc-mode)
      (setq erc-server-process proc
            erc-server-current-nick "dummy")
      (erc--initialize-markers (point) nil)
      (set-process-query-on-exit-flag proc nil)
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _) file))
                ((symbol-function 'erc-dcc-get-file)
                 (lambda (&rest r) (push r calls))))
        (goto-char (point-max))

        (ert-info ("No turbo")
          (should-not (plist-member elt :turbo))
          (goto-char erc-input-marker)
          (insert "/dcc GET " nic " " (or sep "") (prin1-to-string file))
          (erc-send-current-line)
          (should-not (plist-member (car erc-dcc-list) :turbo))
          (should (equal (pop calls) (list elt file proc))))

        (ert-info ("Arg turbo in pos 2")
          (should-not (plist-member elt :turbo))
          (goto-char erc-input-marker)
          (insert "/dcc GET -t " nic " " (or sep "") (prin1-to-string file))
          (erc-send-current-line)
          (should (eq t (plist-get (car erc-dcc-list) :turbo)))
          (should (equal (pop calls) (list elt file proc))))

        (ert-info ("Arg turbo in pos 4")
          (setq elt (plist-put elt :turbo nil)
                erc-dcc-list (list elt))
          (goto-char erc-input-marker)
          (insert "/dcc GET " nic " -t " (or sep "") (prin1-to-string file))
          (erc-send-current-line)
          (should (eq t (plist-get (car erc-dcc-list) :turbo)))
          (should (equal (pop calls) (list elt file proc))))

        (ert-info ("Arg turbo in pos 6")
          (setq elt (plist-put elt :turbo nil)
                erc-dcc-list (list elt))
          (goto-char erc-input-marker)
          (insert "/dcc GET " nic " " (prin1-to-string file) " -t" (or sep ""))
          (erc-send-current-line)
          (should (eq (if sep nil t) (plist-get (car erc-dcc-list) :turbo)))
          (should (equal (pop calls) (if sep nil (list elt file proc)))))))))

(ert-deftest erc-dcc-do-GET-command ()
  (erc-dcc-tests--erc-dcc-do-GET-command "foo.bin")
  (erc-dcc-tests--erc-dcc-do-GET-command "foo - file.bin")
  (erc-dcc-tests--erc-dcc-do-GET-command "foo -t file.bin")
  (erc-dcc-tests--erc-dcc-do-GET-command "-t" "-- ")

  ;; Regression involving pipe character in nickname.
  (let ((nuh "test|r!~test|r@fake.irc"))
    (erc-dcc-tests--erc-dcc-do-GET-command "foo.bin" nil nuh)
    (erc-dcc-tests--erc-dcc-do-GET-command "foo - file.bin" nil nuh)
    (erc-dcc-tests--erc-dcc-do-GET-command "foo -t file.bin" nil nuh)
    (erc-dcc-tests--erc-dcc-do-GET-command "-t" "-- " nuh)))

(defun erc-dcc-tests--pcomplete-common (test-fn &optional file)
  (with-current-buffer (get-buffer-create "*erc-dcc-do-GET-command*")
    (let* ((inhibit-message noninteractive)
           (proc (start-process "fake" (current-buffer) "sleep" "10"))
           (elt (list :nick "tester!~tester@fake.irc"
                      :type 'GET
                      :peer nil
                      :parent proc
                      :ip "127.0.0.1"
                      :port "9899"
                      :file (or file "foo.bin")
                      :size 1405135128))
           ;;
           erc-accidental-paste-threshold-seconds
           erc-insert-modify-hook erc-send-completed-hook
           erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
      (erc-mode)
      (pcomplete-erc-setup)
      (add-hook 'erc-complete-functions #'erc-pcompletions-at-point 0 t)
      (setq erc-server-process proc
            erc-input-marker (make-marker)
            erc-insert-marker (make-marker)
            erc-server-current-nick "dummy")
      (setq-local erc-dcc-list (list elt)) ; for interactive noodling
      (set-process-query-on-exit-flag proc nil)
      (goto-char (point-max))
      (set-marker erc-insert-marker (point-max))
      (erc-display-prompt)
      (goto-char erc-input-marker)
      (funcall test-fn))
    (when noninteractive
      (kill-buffer))))

(ert-deftest pcomplete/erc-mode/DCC--get-basic ()
  (erc-dcc-tests--pcomplete-common
   (lambda ()
     (insert "/dcc get ")
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get tester" nil t)))
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get tester foo.bin" nil t))))))

(ert-deftest pcomplete/erc-mode/DCC--get-quoted ()
  (erc-dcc-tests--pcomplete-common
   (lambda ()
     (insert "/dcc get ")
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get tester" nil t)))
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get tester \"foo bar.bin\"" nil t))))
   "foo bar.bin"))

(ert-deftest pcomplete/erc-mode/DCC--get-1flag ()
  (erc-dcc-tests--pcomplete-common
   (lambda ()
     (goto-char erc-input-marker)
     (delete-region (point) (point-max))
     (insert "/dcc get -")
     (call-interactively #'completion-at-point)
     (with-current-buffer "*Completions*"
       (goto-char (point-min))
       (search-forward "-s")
       (search-forward "-t"))
     (insert "s ")
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get -s tester" nil t)))
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get -s tester foo.bin" nil t))))))

(ert-deftest pcomplete/erc-mode/DCC--get-2flags ()
  (erc-dcc-tests--pcomplete-common
   (lambda ()
     (goto-char erc-input-marker)
     (delete-region (point) (point-max))
     (insert "/dcc get -")
     (call-interactively #'completion-at-point)
     (with-current-buffer "*Completions*"
       (goto-char (point-min))
       (search-forward "-s")
       (search-forward "-t"))
     (insert "s -")
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get -s -t " nil t)))
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get -s -t tester" nil t)))
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get -s -t tester foo.bin" nil t))))))

(ert-deftest pcomplete/erc-mode/DCC--get-2flags-reverse ()
  (erc-dcc-tests--pcomplete-common
   (lambda ()
     (goto-char erc-input-marker)
     (delete-region (point) (point-max))
     (insert "/dcc get -")
     (call-interactively #'completion-at-point)
     (with-current-buffer "*Completions*"
       (goto-char (point-min))
       (search-forward "-s")
       (search-forward "-t"))
     (insert "t -")
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get -t -s " nil t)))
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get -t -s tester" nil t)))
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get -t -s tester foo.bin" nil t))))))

(ert-deftest pcomplete/erc-mode/DCC--get-sep ()
  (erc-dcc-tests--pcomplete-common
   (lambda ()
     (insert "/dcc get ")
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get tester" nil t)))
     (insert "-")
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get tester -- " nil t)))
     (call-interactively #'completion-at-point)
     (save-excursion
       (beginning-of-line)
       (should (search-forward "/dcc get tester -- -t" nil t))))
   "-t"))

;;; erc-dcc-tests.el ends here
