;;; erc-stamp-tests.el --- Tests for erc-stamp.  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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
(require 'erc-stamp)
(require 'erc-goodies) ; for `erc-make-read-only'

;; These display-oriented tests are brittle because many factors
;; influence how text properties are applied.  We should just
;; rework these into full scenarios.

(defun erc-stamp-tests--insert-right (test)
  (let ((val (list 0 0))
        (erc-insert-modify-hook '(erc-add-timestamp))
        (erc-insert-post-hook '(erc-make-read-only)) ; see comment above
        (erc-timestamp-only-if-changed-flag nil)
        ;;
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (advice-add 'erc-format-timestamp :filter-args
                (lambda (args) (cons (cl-incf (cadr val) 60) (cdr args)))
                '((name . ert-deftest--erc-timestamp-use-align-to)))

    (with-current-buffer (get-buffer-create "*erc-stamp-tests--insert-right*")
      (erc-mode)
      (erc-munge-invisibility-spec)
      (erc--initialize-markers (point) nil)
      (setq erc-server-process (start-process "p" (current-buffer)
                                              "sleep" "1"))
      (set-process-query-on-exit-flag erc-server-process nil)

      (funcall test)

      (when noninteractive
        (kill-buffer)))

    (advice-remove 'erc-format-timestamp
                   'ert-deftest--erc-timestamp-use-align-to)))

(defun erc-stamp-tests--use-align-to--nil (compat)
  (erc-stamp-tests--insert-right
   (lambda ()

     (ert-info ("nil, normal")
       (let ((erc-timestamp-use-align-to nil))
         (erc-display-message nil 'notice (current-buffer) "begin"))
       (goto-char (point-min))
       (should (search-forward-regexp
                (rx "begin" (+ "\t") (* " ") "[") nil t))
       ;; Field includes intervening spaces
       (should (eql ?n (char-before (field-beginning (point)))))
       ;; Timestamp extends to the end of the line
       (should (eql ?\n (char-after (field-end (point))))))

     ;; The option `erc-timestamp-right-column' is normally nil by
     ;; default, but it's a convenient stand in for a sufficiently
     ;; small `erc-fill-column' (we can force a line break without
     ;; involving that module).
     (should-not erc-timestamp-right-column)

     (ert-info ("nil, overlong (hard wrap)")
       (let ((erc-timestamp-use-align-to nil)
             (erc-timestamp-right-column 20))
         (erc-display-message nil 'notice (current-buffer)
                              "twenty characters"))
       (should (search-forward-regexp (rx bol (+ "\t") (* " ") "[") nil t))
       ;; Field includes leading whitespace.
       (should (eql (if compat ?\[ ?\n)
                    (char-after (field-beginning (point)))))
       ;; Timestamp extends to the end of the line.
       (should (eql ?\n (char-after (field-end (point)))))))))

(ert-deftest erc-timestamp-use-align-to--nil ()
  (ert-info ("Field starts on stamp text (compat)")
    (let ((erc-stamp--omit-properties-on-folded-lines t))
      (erc-stamp-tests--use-align-to--nil 'compat)))
  (ert-info ("Field includes leaidng white space")
    (erc-stamp-tests--use-align-to--nil nil)))

(defun erc-stamp-tests--use-align-to--t (compat)
  (erc-stamp-tests--insert-right
   (lambda ()

     (ert-info ("t, normal")
       (let ((erc-timestamp-use-align-to t))
         (let ((msg (erc-format-privmessage "bob" "msg one" nil t)))
           (erc-display-message nil nil (current-buffer) msg)))
       (goto-char (point-min))
       ;; Exactly two spaces, one from format, one added by erc-stamp.
       (should (search-forward "msg one [" nil t))
       ;; Field covers space between.
       (should (eql ?e (char-before (field-beginning (point)))))
       (should (eql ?\n (char-after (field-end (point))))))

     (ert-info ("t, overlong (hard wrap)")
       (let ((erc-timestamp-use-align-to t)
             (erc-timestamp-right-column 20))
         (let ((msg (erc-format-privmessage "bob" "tttt wwww oooo" nil t)))
           (erc-display-message nil nil (current-buffer) msg)))
       ;; Indented to pos (this is arguably a bug).
       (should (search-forward-regexp (rx bol (+ "\t") (* " ") "[") nil t))
       ;; Field includes leading space.
       (should (eql (if compat ?\[ ?\n) (char-after (field-beginning (point)))))
       (should (eql ?\n (char-after (field-end (point)))))))))

(ert-deftest erc-timestamp-use-align-to--t ()
  (ert-info ("Field starts on stamp text (compat)")
    (let ((erc-stamp--omit-properties-on-folded-lines t))
      (erc-stamp-tests--use-align-to--t 'compat)))
  (ert-info ("Field includes leaidng white space")
    (erc-stamp-tests--use-align-to--t nil)))

(ert-deftest erc-timestamp-use-align-to--integer ()
  (erc-stamp-tests--insert-right
   (lambda ()

     (ert-info ("integer, normal")
       (let ((erc-timestamp-use-align-to 1))
         (let ((msg (erc-format-privmessage "bob" "msg one" nil t)))
           (erc-display-message nil nil (current-buffer) msg)))
       (goto-char (point-min))
       ;; Space not added because included in format string.
       (should (search-forward "msg one [" nil t))
       ;; Field covers space between.
       (should (eql ?e (char-before (field-beginning (point)))))
       (should (eql ?\n (char-after (field-end (point))))))

     (ert-info ("integer, overlong (hard wrap)")
       (let ((erc-timestamp-use-align-to 1)
             (erc-timestamp-right-column 20))
         (let ((msg (erc-format-privmessage "bob" "tttt wwww oooo" nil t)))
           (erc-display-message nil nil (current-buffer) msg)))
       ;; No hard wrap
       (should (search-forward "oooo [" nil t))
       ;; Field starts at leading space.
       (should (eql ?\s (char-after (field-beginning (point)))))
       (should (eql ?\n (char-after (field-end (point)))))))))

(ert-deftest erc-stamp--display-margin-mode--right ()
  (erc-stamp-tests--insert-right
   (lambda ()
     (erc-stamp--display-margin-mode +1)

     (ert-info ("margin, normal")
       (let ((erc-timestamp-use-align-to 'margin))
         (let ((msg (erc-format-privmessage "bob" "msg one" nil t)))
           (put-text-property 0 (length msg) 'wrap-prefix 10 msg)
           (erc-display-message nil nil (current-buffer) msg)))
       (goto-char (point-min))
       ;; Space not added (treated as opaque string).
       (should (search-forward "msg one[" nil t))
       ;; Field covers stamp alone
       (should (eql ?e (char-before (field-beginning (point)))))
       ;; Vanity props extended
       (should (get-text-property (field-beginning (point)) 'wrap-prefix))
       (should (get-text-property (1+ (field-beginning (point))) 'wrap-prefix))
       (should (get-text-property (1- (field-end (point))) 'wrap-prefix))
       (should (eql ?\n (char-after (field-end (point))))))

     (ert-info ("margin, overlong (hard wrap)")
       (let ((erc-timestamp-use-align-to 'margin)
             (erc-timestamp-right-column 20))
         (let ((msg (erc-format-privmessage "bob" "tttt wwww oooo" nil t)))
           (erc-display-message nil nil (current-buffer) msg)))
       ;; No hard wrap
       (should (search-forward "oooo[" nil t))
       ;; Field starts at format string (right bracket)
       (should (eql ?\[ (char-after (field-beginning (point)))))
       (should (eql ?\n (char-after (field-end (point)))))))))

;; This concerns a proposed partial reversal of the changes resulting
;; from:
;;
;;   24.1.50; Wrong behavior of move-end-of-line in ERC (Bug#11706)
;;
;; Perhaps core behavior has changed since this bug was reported, but
;; C-e stopping one char short of EOL no longer seems a problem.
;; However, invoking C-n (`next-line') exhibits a similar effect.
;; When point is in a stamp or near the beginning of a line, issuing a
;; C-n puts point one past the start of the message (i.e., two chars
;; beyond the timestamp's closing "]".  Dropping the invisible
;; property when timestamps are hidden does indeed prevent this, but
;; it's also a lasting commitment.  The docs mention that it's
;; pointless to pair the old `intangible' property with `invisible'
;; and suggest users look at `cursor-intangible-mode'.  Turning off
;; the latter does indeed do the trick as does decrementing the end of
;; the `cursor-intangible' interval so that, in addition to C-n
;; working, a C-f from before the timestamp doesn't overshoot.  This
;; appears to be the case whether `erc-hide-timestamps' is enabled or
;; not, but it may be inadvisable for some reason (a hack) and
;; therefore warrants further investigation.
;;
;; Note some striking omissions here:
;;
;;   1. a lack of `fill' module integration (we simulate it by
;;      making lines short enough to not wrap)
;;   2. functions like `line-move' behave differently when
;;      `noninteractive'
;;   3. no actual test assertions involving `cursor-sensor' movement
;;      even though that's a huge ingredient

(ert-deftest erc-timestamp-intangible--left ()
  (let ((erc-timestamp-only-if-changed-flag nil)
        (erc-timestamp-intangible t) ; default changed to nil in 2014
        (erc-hide-timestamps t)
        (erc-insert-timestamp-function 'erc-insert-timestamp-left)
        (erc-server-process (start-process "true" (current-buffer) "true"))
        (erc-insert-modify-hook '(erc-make-read-only erc-add-timestamp))
        msg
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
    (should (not cursor-sensor-inhibit))
    (set-process-query-on-exit-flag erc-server-process nil)
    (erc-mode)
    (with-current-buffer (get-buffer-create "*erc-timestamp-intangible*")
      (erc-mode)
      (erc--initialize-markers (point) nil)
      (erc-munge-invisibility-spec)
      (erc-display-message nil 'notice (current-buffer) "Welcome")
      ;;
      ;; Pretend `fill' is active and that these lines are
      ;; folded. Otherwise, there's an annoying issue on wrapped lines
      ;; (when visual-line-mode is off and stamps are visible) where
      ;; C-e sends you to the end of the previous line.
      (setq msg "Lorem ipsum dolor sit amet")
      (erc-display-message nil nil (current-buffer)
                           (erc-format-privmessage "alyssa" msg nil t))
      (erc-display-message nil 'notice (current-buffer) "Home")
      (goto-char (point-min))

      ;; EOL is actually EOL (Bug#11706)

      (ert-info ("Notice before stamp, C-e") ; first line/stamp
        (should (search-forward "Welcome" nil t))
        (ert-simulate-command '(erc-bol))
        (should (looking-at (rx "[")))
        (let ((end (pos-eol))) ; `line-end-position' fails because fields
          (ert-simulate-command '(move-end-of-line 1))
          (should (= end (point)))))

      (ert-info ("Privmsg before stamp, C-e")
        (should (search-forward "Lorem" nil t))
        (goto-char (pos-bol))
        (should (looking-at (rx "[")))
        (let ((end (pos-eol)))
          (ert-simulate-command '(move-end-of-line 1))
          (should (= end (point)))))

      (ert-info ("Privmsg first line, C-e")
        (goto-char (pos-bol))
        (should (search-forward "ipsum" nil t))
        (let ((end (pos-eol)))
          (ert-simulate-command '(move-end-of-line 1))
          (should (= end (point)))))

      (when noninteractive
        (kill-buffer)))))

(ert-deftest erc-echo-timestamp ()
  :tags (and (null (getenv "CI")) '(:unstable))

  (should-not erc-echo-timestamps)
  (should-not erc-stamp--last-stamp)
  (insert (propertize "abc" 'erc-timestamp 433483200))
  (goto-char (point-min))
  (let ((inhibit-message t)
        (erc-echo-timestamp-format "%Y-%m-%d %H:%M:%S %Z")
        (erc-echo-timestamp-zone (list (* 60 60 -4) "EDT")))

    ;; No-op when non-interactive and option is nil
    (should-not (erc--echo-ts-csf nil nil 'entered))
    (should-not erc-stamp--last-stamp)

    ;; Non-interactive (cursor sensor function)
    (let ((erc-echo-timestamps t))
      (should (equal (erc--echo-ts-csf nil nil 'entered)
                     "1983-09-27 00:00:00 EDT")))
    (should (= 433483200 erc-stamp--last-stamp))

    ;; Interactive
    (should (equal (call-interactively #'erc-echo-timestamp)
                   "1983-09-27 00:00:00 EDT"))
    ;; Interactive with zone
    (let ((current-prefix-arg '(4)))
      (should (member (call-interactively #'erc-echo-timestamp)
                      '("1983-09-27 04:00:00 GMT"
                        "1983-09-27 04:00:00 UTC"))))
    (let ((current-prefix-arg -7))
      (should (equal (call-interactively #'erc-echo-timestamp)
                     "1983-09-26 21:00:00 -07")))))

;;; erc-stamp-tests.el ends here
