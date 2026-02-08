;;; erc-match-tests.el --- Tests for erc-match.  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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
(require 'erc-match)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-tests-common)))


(ert-deftest erc-add-entry-to-list ()
  ;; These tests currently freeze in Android.
  (when (featurep 'android)
    (ert-skip "Freezes on Android as of 31.0.50"))

  (let ((erc-pals '("z"))
        (inhibit-message noninteractive)
        (erc-match-quote-when-adding 'ask))

    (ert-info ("Default (ask)")
      (ert-simulate-keys "\t\ry\r"
        (erc-add-entry-to-list 'erc-pals "?" '((".")) nil)
        (should (equal (pop erc-pals) "\\.")))

      (ert-info ("Inverted")
        (ert-simulate-keys "\t\ry\r"
          (erc-add-entry-to-list 'erc-pals "?" '((".")) nil)
          (should (equal (pop erc-pals) "\\."))))

      (ert-info ("Skipped")
        (ert-simulate-keys "\t\r"
          (erc-add-entry-to-list 'erc-pals "?" '(("x")) nil)
          (should (equal (pop erc-pals) "x")))))

    (ert-info ("Verbatim")
      (setq erc-match-quote-when-adding nil)
      (ert-simulate-keys "\t\r"
        (erc-add-entry-to-list 'erc-pals "?" '((".")) nil)
        (should (equal (pop erc-pals) ".")))

      (ert-info ("Inverted")
        (ert-simulate-keys "\t\r"
          (erc-add-entry-to-list 'erc-pals "?" '((".")) t)
          (should (equal (pop erc-pals) "\\.")))))

    (ert-info ("Quoted")
      (setq erc-match-quote-when-adding t)
      (ert-simulate-keys "\t\r"
        (erc-add-entry-to-list 'erc-pals "?" '((".")) nil)
        (should (equal (pop erc-pals) "\\.")))

      (ert-info ("Inverted")
        (ert-simulate-keys "\t\r"
          (erc-add-entry-to-list 'erc-pals "?" '((".")) t)
          (should (equal (pop erc-pals) ".")))))

    (should (equal erc-pals '("z")))))

(ert-deftest erc-pals ()
  (with-temp-buffer
    (setq erc-server-process (start-process "true" (current-buffer) "true")
          erc-server-users (make-hash-table :test #'equal))
    (set-process-query-on-exit-flag erc-server-process nil)
    (erc-add-server-user "FOO[m]" (make-erc-server-user :nickname "foo[m]"))
    (erc-add-server-user "tester" (make-erc-server-user :nickname "tester"))

    (let ((erc-match-quote-when-adding t)
          (inhibit-message noninteractive)
          erc-pals calls rvs)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest r) (push r calls) (pop rvs))))

        (ert-info ("`erc-add-pal'")
          (push "foo[m]" rvs)
          (ert-simulate-command '(erc-add-pal))
          (should (equal (cadr (pop calls)) '(("tester") ("foo[m]"))))
          (should (equal erc-pals '("foo\\[m]"))))

        (ert-info ("`erc-match-pal-p'")
          (should (erc-match-pal-p "FOO[m]!~u@example.net" nil)))

        (ert-info ("`erc-delete-pal'")
          (push "foo\\[m]" rvs)
          (ert-simulate-command '(erc-delete-pal))
          (should (equal (cadr (pop calls)) '(("foo\\[m]"))))
          (should-not erc-pals))

        (ert-info ("`erc-add-pal' verbatim")
          (push "foo[m]" rvs)
          (ert-simulate-command '(erc-add-pal (4)))
          (should (equal (cadr (pop calls)) '(("tester") ("foo[m]"))))
          (should (equal erc-pals '("foo[m]"))))))))

(ert-deftest erc-fools ()
  (with-temp-buffer
    (setq erc-server-process (start-process "true" (current-buffer) "true")
          erc-server-users (make-hash-table :test #'equal))
    (set-process-query-on-exit-flag erc-server-process nil)
    (erc-add-server-user "FOO[m]" (make-erc-server-user :nickname "foo[m]"))
    (erc-add-server-user "tester" (make-erc-server-user :nickname "tester"))

    (let ((erc-match-quote-when-adding t)
          (inhibit-message noninteractive)
          erc-fools calls rvs)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest r) (push r calls) (pop rvs))))

        (ert-info ("`erc-add-fool'")
          (push "foo[m]" rvs)
          (ert-simulate-command '(erc-add-fool))
          (should (equal (cadr (pop calls)) '(("tester") ("foo[m]"))))
          (should (equal erc-fools '("foo\\[m]"))))

        (ert-info ("`erc-match-fool-p'")
          (should (erc-match-fool-p "FOO[m]!~u@example.net" ""))
          (should (erc-match-fool-p "tester!~u@example.net" "FOO[m]: die")))

        (ert-info ("`erc-delete-fool'")
          (push "foo\\[m]" rvs)
          (ert-simulate-command '(erc-delete-fool))
          (should (equal (cadr (pop calls)) '(("foo\\[m]"))))
          (should-not erc-fools))

        (ert-info ("`erc-add-fool' verbatim")
          (push "foo[m]" rvs)
          (ert-simulate-command '(erc-add-fool (4)))
          (should (equal (cadr (pop calls)) '(("tester") ("foo[m]"))))
          (should (equal erc-fools '("foo[m]"))))))))

(ert-deftest erc-keywords ()
  (let ((erc-match-quote-when-adding t)
        (inhibit-message noninteractive)
        erc-keywords calls rvs)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest r) (push r calls) (pop rvs))))

      (ert-info ("`erc-add-keyword'")
        (push "[cit. needed]" rvs)
        (ert-simulate-command '(erc-add-keyword))
        (should (equal (cadr (pop calls)) nil))
        (should (equal erc-keywords '("\\[cit\\. needed]"))))

      (ert-info ("`erc-match-keyword-p'")
        (should (erc-match-keyword-p nil "is pretty [cit. needed]")))

      (ert-info ("`erc-delete-keyword'")
        (push "\\[cit\\. needed]" rvs)
        (ert-simulate-command '(erc-delete-keyword))
        (should (equal (cadr (pop calls)) '(("\\[cit\\. needed]"))))
        (should-not erc-keywords))

      (ert-info ("`erc-add-keyword' verbatim")
        (push "[...]" rvs)
        (ert-simulate-command '(erc-add-keyword (4)))
        (should (equal (cadr (pop calls)) nil))
        (should (equal erc-keywords '("[...]")))))))

(ert-deftest erc-dangerous-hosts ()
  (let ((erc-match-quote-when-adding t)
        (inhibit-message noninteractive)
        erc-dangerous-hosts calls rvs)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest r) (push r calls) (pop rvs))))

      (ert-info ("`erc-add-dangerous-host'")
        (push "example.net" rvs)
        (ert-simulate-command '(erc-add-dangerous-host))
        (should (equal (cadr (pop calls)) nil))
        (should (equal erc-dangerous-hosts '("example\\.net"))))

      (ert-info ("`erc-match-dangerous-host-p'")
        (should (erc-match-dangerous-host-p "FOO[m]!~u@example.net" nil)))

      (ert-info ("`erc-delete-dangerous-host'")
        (push "example\\.net" rvs)
        (ert-simulate-command '(erc-delete-dangerous-host))
        (should (equal (cadr (pop calls)) '(("example\\.net"))))
        (should-not erc-dangerous-hosts))

      (ert-info ("`erc-add-dangerous-host' verbatim")
        (push "example.net" rvs)
        (ert-simulate-command '(erc-add-dangerous-host (4)))
        (should (equal (cadr (pop calls)) nil))
        (should (equal erc-dangerous-hosts '("example.net")))))))

(defun erc-match-tests--assert-face-absent (face end)
  "Ensure FACE is absent from point until pos or substring END."
  (when (stringp end)
    (save-excursion
      (search-forward end)
      (setq end (1- (match-beginning 0)))))
  (ert-info ((format "Face %S absent throughout: %S" face
                     (buffer-substring-no-properties (point) end)))
    (while (<= (point) end)
      (ert-info ((format "Looking at: (%d %c)" (char-after) (char-after)))
        (let ((val (ensure-list (get-text-property (point) 'font-lock-face))))
          (should-not (memq face val))))
      (forward-char))))

(defun erc-match-tests--assert-face-present (face end)
  "Ensure FACE is present from point until pos or substring END."
  (when (stringp end)
    (save-excursion
      (search-forward end)
      (setq end (1- (match-beginning 0)))))
  (ert-info ((format "Face %S appears throughout: %S" face
                     (buffer-substring-no-properties (point) end)))
    (while (<= (point) end)
      (ert-info ((format "Looking at: (%d %c)" (char-after) (char-after)))
        (let ((val (ensure-list (get-text-property (point) 'font-lock-face))))
          (should (eq face (car val)))))
      (forward-char))))

(defun erc-match-tests--assert-speaker-highlighted (nick face)
  (search-forward (concat "<" nick ">"))
  (goto-char (pos-bol))
  (should (= (char-after) ?<))
  (should (equal (get-text-property (point) 'font-lock-face)
                 'erc-default-face))

  (ert-info ((format "Nick in <%s> highlighted" nick))
    (forward-char)
    (erc-match-tests--assert-face-present face "> "))

  (should (= (char-after) ?>)))

(defun erc-match-tests--assert-speaker-only-highlighted (nick face)
  (erc-match-tests--assert-speaker-highlighted nick face)
  (ert-info ("Remaining text in line not highlighted")
    (erc-match-tests--assert-face-absent face (pos-eol))))

(defun erc-match-tests--perform (test)
  (erc-tests-common-make-server-buf)
  (setq erc-server-current-nick "tester")
  (with-current-buffer (erc--open-target "#chan")
    (funcall test))
  (when noninteractive
    (erc-tests-common-kill-buffers)))

;; The `nick' highlight type only covers a matching sender's speaker
;; tag.  It does not do any highlighting for pal/fool/dangerous-host
;; mentions.  While `current-nick' and `keyword' categories match
;; against a message's content, the speaker's nick is still highlighted
;; (in the corresponding face) when a match occurs.
(defun erc-match-tests--hl-type-nick (face &optional test)
  (should (eq erc-current-nick-highlight-type 'keyword))
  (should (eq erc-keyword-highlight-type 'keyword))

  (erc-match-tests--perform
   (lambda ()
     (erc-tests-common-add-cmem "bob")
     (erc-tests-common-add-cmem "alice")
     ;; Change highlight type for match categories `keyword' and
     ;; `current-nick' to `nick'.
     (let ((erc-current-nick-highlight-type 'nick)
           (erc-keyword-highlight-type 'nick)
           (erc-keywords '("thing")))
       (erc-tests-common-simulate-privmsg "bob" "hi alice")
       (erc-tests-common-simulate-privmsg "alice" "hi bob")
       (erc-tests-common-simulate-privmsg "bob" "hi tester")
       (erc-tests-common-simulate-privmsg "bob" "something blue"))
     (goto-char (point-min))

     ;; A sender's nick appears in `erc-{pals,fools,dangerous-hosts}',
     ;; so the nick portion of their speaker tag alone is highlighted.
     (erc-match-tests--assert-speaker-only-highlighted "bob" face)

     ;; A non-matching sender mentions a would-be match (if message
     ;; bodies were considered), and the nick portion of their speaker
     ;; tag is *not* highlighted.
     (search-forward "<alice>")
     (goto-char (pos-bol))
     (erc-match-tests--assert-face-absent face (pos-eol))

     ;; A matching sender mentions our own nick ("tester"), and their
     ;; speaker's nick is highlighted in `erc-current-nick-face' instead
     ;; of the normal category face (e.g., `erc-pal-face').  This
     ;; happens because the implementation applies highlighting for
     ;; non-NUH-based categories (`keyword' and `current-nick') after
     ;; sender-based ones.
     (should (looking-at (rx "<bob>")))
     (erc-match-tests--assert-speaker-only-highlighted
      "bob" 'erc-current-nick-face)

     ;; A matching sender mentions keyword "tester", and their speaker's
     ;; nick is highlighted in `erc-keyword-face' instead of the normal
     ;; category face for the same reason mentioned above.
     (should (looking-at (rx "<bob>")))
     (erc-match-tests--assert-speaker-only-highlighted
      "bob" 'erc-keyword-face)

     (when test
       (funcall test)))))

(defun erc-match-tests--hl-type-nick/mention (face)
  (erc-match-tests--hl-type-nick
   face
   (lambda ()
     (erc-tests-common-simulate-privmsg "alice" "bob: one")
     (erc-tests-common-simulate-privmsg "alice" "bob, two")
     (erc-tests-common-simulate-privmsg "alice" "three, bob.")

     (search-forward "<alice> bob: one")
     (goto-char (pos-bol))
     (erc-match-tests--assert-speaker-only-highlighted "alice" face)

     (search-forward "<alice> bob, two")
     (goto-char (pos-bol))
     (erc-match-tests--assert-speaker-only-highlighted "alice" face)

     (search-forward "<alice> three, bob.")
     (goto-char (pos-bol))
     (erc-match-tests--assert-speaker-only-highlighted "alice" face))))

(ert-deftest erc-match-message/pal/nick ()
  (should (eq erc-pal-highlight-type 'nick))
  (let ((erc-pals (list "bob")))
    (erc-match-tests--hl-type-nick 'erc-pal-face)))

(ert-deftest erc-match-message/fool/nick ()
  (should (eq erc-fool-highlight-type 'nick))
  (let ((erc-fools (list "bob")))
    (erc-match-tests--hl-type-nick/mention 'erc-fool-face)))

(ert-deftest erc-match-message/dangerous-host/nick ()
  (should (eq erc-dangerous-host-highlight-type 'nick))
  (let ((erc-dangerous-hosts (list "bob")))
    (erc-match-tests--hl-type-nick 'erc-dangerous-host-face)))

(defun erc-match-tests--hl-type-message (face)
  (should (eq erc-current-nick-highlight-type 'keyword))
  (should (eq erc-keyword-highlight-type 'keyword))

  (erc-match-tests--perform
   (lambda ()
     (erc-tests-common-add-cmem "bob")
     (erc-tests-common-add-cmem "alice")
     ;; Change highlight type for categories `keyword' and
     ;; `current-nick' to `message'.
     (let ((erc-current-nick-highlight-type 'message)
           (erc-keyword-highlight-type 'message)
           (erc-keywords '("thing")))
       (erc-tests-common-simulate-privmsg "bob" "hi alice")
       (erc-tests-common-simulate-privmsg "alice" "hi bob")
       (erc-tests-common-simulate-privmsg "bob" "hi tester")
       (erc-tests-common-simulate-privmsg "bob" "something blue"))
     (goto-char (point-min))

     ;; Message body portion appears in `erc-{pals,fools,dangerous-hosts}'.
     ;; But the speaker portion is not highlighted by `match'.
     (erc-match-tests--assert-face-absent face "hi alice")
     (erc-match-tests--assert-face-present face
                                           (+ (point) (length "hi alice") -1))

     ;; A non-matching sender mentions a would-be match (if message
     ;; bodies were considered), but nothing is highlighted.
     (search-forward "<alice>")
     (goto-char (pos-bol))
     (erc-match-tests--assert-face-absent face (pos-eol))

     ;; A matching sender mentions our own nick ("tester"), and the
     ;; message body is highlighted in `erc-current-nick-face' instead
     ;; of the normal category face (e.g., `erc-pal-face').
     (should (looking-at (rx "<bob>")))
     (save-excursion (erc-match-tests--assert-face-absent face "hi tester"))
     (erc-match-tests--assert-face-absent 'erc-current-nick-face "hi tester")
     (erc-match-tests--assert-face-present 'erc-current-nick-face (pos-eol))

     ;; A matching sender mentions keyword "thing", and the message body
     ;; is highlighted in `erc-keyword-face' instead of the normal
     ;; category face.
     (should (looking-at (rx "<bob>")))
     (save-excursion (erc-match-tests--assert-face-absent face "something"))
     (erc-match-tests--assert-face-absent 'erc-keyword-face "something")
     (erc-match-tests--assert-face-present 'erc-keyword-face (pos-eol)))))

(ert-deftest erc-match-message/pal/message ()
  (should (eq erc-pal-highlight-type 'nick))
  (let ((erc-pals (list "bob"))
        (erc-pal-highlight-type 'message))
    (erc-match-tests--hl-type-message 'erc-pal-face)))

(ert-deftest erc-match-message/fool/message ()
  (should (eq erc-fool-highlight-type 'nick))
  (let ((erc-fools (list "bob"))
        (erc-fool-highlight-type 'message))
    (erc-match-tests--hl-type-message 'erc-fool-face)))

(ert-deftest erc-match-message/dangerous-host/message ()
  (should (eq erc-dangerous-host-highlight-type 'nick))
  (let ((erc-dangerous-hosts (list "bob"))
        (erc-dangerous-host-highlight-type 'message))
    (erc-match-tests--hl-type-message 'erc-dangerous-host-face)))

(defun erc-match-tests--hl-type-all (face)
  (should (eq erc-current-nick-highlight-type 'keyword))
  (should (eq erc-keyword-highlight-type 'keyword))

  (erc-match-tests--perform
   (lambda ()
     (erc-tests-common-add-cmem "bob")
     (erc-tests-common-add-cmem "alice")
     ;; Change highlight type for categories `current-nick' and
     ;; `keyword' to `all'.
     (let ((erc-current-nick-highlight-type 'all)
           (erc-keyword-highlight-type 'all)
           (erc-keywords '("thing")))
       (erc-tests-common-simulate-privmsg "bob" "hi alice")
       (erc-tests-common-simulate-privmsg "alice" "hi bob")
       (erc-tests-common-simulate-privmsg "bob" "hi tester")
       (erc-tests-common-simulate-privmsg "bob" "something blue"))
     (goto-char (point-min))

     ;; Entire message, including speaker appears in a speaker-based
     ;; face `erc-{pals,fools,dangerous-hosts}'.
     (search-forward "<bob>")
     (goto-char (pos-bol))
     (erc-match-tests--assert-face-present
      face (+ (point) (length "<bob> hi alice") -1))

     ;; A non-matching sender mentions a would-be match (if message
     ;; bodies were considered), but nothing is highlighted.
     (search-forward "<alice>")
     (goto-char (pos-bol))
     (erc-match-tests--assert-face-absent face (pos-eol))

     ;; A matching sender mentions our own nick ("tester"), and the
     ;; entire message, including the speaker portion, is highlighted in
     ;; `erc-current-nick-face' instead of the normal category face
     ;; (e.g., `erc-pal-face').
     (should (looking-at (rx "<bob>")))
     (save-excursion (erc-match-tests--assert-face-absent face (pos-eol)))
     (erc-match-tests--assert-face-present 'erc-current-nick-face (pos-eol))

     ;; A matching sender mentions keyword "thing", and the entire
     ;; message is highlighted in `erc-keyword-face' instead of the
     ;; normal category face.
     (should (looking-at (rx "<bob>")))
     (save-excursion (erc-match-tests--assert-face-absent face (pos-eol)))
     (erc-match-tests--assert-face-present 'erc-keyword-face (pos-eol)))))

(ert-deftest erc-match-message/pal/all ()
  (should (eq erc-pal-highlight-type 'nick))
  (let ((erc-pals (list "bob"))
        (erc-pal-highlight-type 'all))
    (erc-match-tests--hl-type-all 'erc-pal-face)))

(ert-deftest erc-match-message/fool/all ()
  (should (eq erc-fool-highlight-type 'nick))
  (let ((erc-fools (list "bob"))
        (erc-fool-highlight-type 'all))
    (erc-match-tests--hl-type-all 'erc-fool-face)))

(ert-deftest erc-match-message/dangerous-host/all ()
  (should (eq erc-dangerous-host-highlight-type 'nick))
  (let ((erc-dangerous-hosts (list "bob"))
        (erc-dangerous-host-highlight-type 'all))
    (erc-match-tests--hl-type-all 'erc-dangerous-host-face)))

(defun erc-match-tests--hl-type-nick-or-keyword ()
  (should (eq erc-current-nick-highlight-type 'keyword))

  (erc-match-tests--perform
   (lambda ()
     (erc-tests-common-add-cmem "bob")
     (erc-tests-common-add-cmem "alice")
     ;; Change highlight type for category `current-nick' from the
     ;; default to `nick-or-keyword'.
     (let ((erc-current-nick-highlight-type 'nick-or-keyword))
       (erc-tests-common-simulate-line
        ":irc.foonet.org 353 tester = #chan :bob tester alice")
       (erc-tests-common-simulate-line
        ":irc.foonet.org 366 tester #chan :End of NAMES list")
       (erc-tests-common-simulate-privmsg "bob" "hi tester"))
     (goto-char (point-min))

     ;; An initial NAMES burst arrives.  Its sender is "irc.foonet.org",
     ;; so `match' skips the "nick" half of `nick-or-keyword' and
     ;; considers the input non-NUH-based (because a host name alone
     ;; can't be a real user).  IOW, it pretends the option's value is
     ;; `keyword', and highlights all occurrences in the message body.
     (search-forward "*** Users on #chan: bob tester")
     (goto-char (pos-bol))
     (erc-match-tests--assert-face-absent 'erc-current-nick-face "tester")
     (erc-match-tests--assert-face-present 'erc-current-nick-face
                                           (+ (point) (length "tester") -1))
     (erc-match-tests--assert-face-absent 'erc-current-nick-face (pos-eol))

     ;; Someone mentions our nick ("tester"), and only their speaker
     ;; tag's nick is highlighted in `erc-current-nick-face' because
     ;; that speaker is a real server user.
     (search-forward "<bob>")
     (goto-char (pos-bol))
     (should-not (get-text-property (point) 'erc-current-nick-face))
     (forward-char)
     (erc-match-tests--assert-face-present 'erc-current-nick-face
                                           "> hi tester")
     (erc-match-tests--assert-face-absent 'erc-current-nick-face
                                          (+ (point) (length "hi tester"))))))

(ert-deftest erc-match-message/current-nick/nick-or-keyword ()
  (erc-match-tests--hl-type-nick-or-keyword))

(defun erc-match-tests--hl-type-keyword ()
  (should (eq erc-keyword-highlight-type 'keyword))

  (erc-match-tests--perform
   (lambda ()
     (erc-tests-common-add-cmem "bob")
     (erc-tests-common-add-cmem "imamodel")
     (erc-tests-common-add-cmem "ModerNerd")

     (let ((erc-keywords '("mode")))
       (erc-tests-common-simulate-line
        ":irc.foonet.org 353 tester = #chan :bob imamodel ModerNerd tester")
       (erc-tests-common-simulate-line
        ":irc.foonet.org 366 tester #chan :End of NAMES list")
       (erc-tests-common-simulate-line
        ":irc.foonet.org 324 tester #chan +Cnt")
       (erc-tests-common-simulate-line
        ":irc.foonet.org 329 tester #chan 1703579802")
       (erc-tests-common-simulate-privmsg "bob" "imamodel: spam a la mode!")
       (erc-tests-common-simulate-privmsg "imamodel" "hi bob"))

     (goto-char (point-min))

     ;; All occurrences highlighted in a non-user-based message.
     (search-forward "*** Users on #chan:")
     (goto-char (pos-bol))
     (erc-match-tests--assert-face-absent 'erc-keyword-face "model ")
     (erc-match-tests--assert-face-present 'erc-keyword-face "l ")
     (erc-match-tests--assert-face-absent 'erc-keyword-face "Mode")
     (erc-match-tests--assert-face-present 'erc-keyword-face "rNerd")
     (erc-match-tests--assert-face-absent 'erc-keyword-face (pos-eol))

     ;; Formatted text matched against rather than original message.
     (search-forward "*** #chan modes:")
     (goto-char (pos-bol))
     (erc-match-tests--assert-face-absent 'erc-keyword-face "modes:")
     (erc-match-tests--assert-face-present 'erc-keyword-face "s: +Cnt")
     (erc-match-tests--assert-face-absent 'erc-keyword-face (pos-eol))

     ;; All occurrences highlighted in a user-based message.
     (search-forward "<bob>")
     (goto-char (pos-bol))
     (erc-match-tests--assert-face-absent 'erc-keyword-face "model")
     (erc-match-tests--assert-face-present 'erc-keyword-face "l: spam")
     (erc-match-tests--assert-face-absent 'erc-keyword-face "mode!")
     (erc-match-tests--assert-face-present 'erc-keyword-face "!")
     (erc-match-tests--assert-face-absent 'erc-keyword-face (pos-eol))

     ;; Matching speaker ignored.
     (search-forward "<imamodel>")
     (goto-char (pos-bol))
     (erc-match-tests--assert-face-absent 'erc-keyword-face (pos-eol)))))

(ert-deftest erc-match-message/keyword/keyword ()
  (erc-match-tests--hl-type-keyword))

(defun erc-match-tests--log-matches ()
  (let ((erc-log-matches-flag t)
        (erc-timestamp-format "[@@TS@@]")
        (inhibit-message noninteractive))
    (erc-match-tests--hl-type-keyword)
    (with-current-buffer "*scratch*"
      (ert-simulate-keys "\t\r"
        (erc-go-to-log-matches-buffer))
      (should (equal (buffer-name) "ERC Keywords"))
      (goto-char (point-min))
      (should (equal (buffer-string) "\
 == Type \"q\" to dismiss messages ==
[@@TS@@]<Server:353:#chan> *** Users on #chan: bob imamodel ModerNerd tester
[@@TS@@]<Server:324:#chan> *** #chan modes: +Cnt
[@@TS@@]<bob:#chan> imamodel: spam a la mode!
"))
      (when noninteractive
        (kill-buffer)))))

(ert-deftest erc-log-matches ()
  (erc-match-tests--log-matches))


;;; erc-match-tests.el ends here
