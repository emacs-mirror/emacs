;;; erc-scenarios-match.el --- Misc `erc-match' scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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

(eval-when-compile
  (require 'erc-join)
  (require 'erc-match))

(require 'erc-stamp)
(require 'erc-fill)

;; This defends against a regression in which all matching by the
;; `erc-match-message' fails when `erc-add-timestamp' precedes it in
;; `erc-insert-modify-hook'.  Basically, `erc-match-message' used to
;; expect an `erc-parsed' text property on the first character in a
;; message, which doesn't exist, when the message content is prefixed
;; by a leading timestamp.

(ert-deftest erc-scenarios-match--stamp-left-current-nick ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (dumb-server (erc-d-run "localhost" t 'unexpected-disconnect))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.1)
       (erc-insert-timestamp-function 'erc-insert-timestamp-left)
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :full-name "tester"
                                :nick "tester")
        ;; Module `timestamp' follows `match' in insertion hooks.
        (should (memq 'erc-add-timestamp
                      (memq 'erc-match-message erc-insert-modify-hook)))
        ;; The "match type" is `current-nick'.
        (funcall expect 5 "tester")
        (should (eq (get-text-property (1- (point)) 'font-lock-face)
                    'erc-current-nick-face))))))

;; When hacking on tests that use this fixture, it's best to run it
;; interactively, and check for wierdness before and after doing
;; M-: (remove-from-invisibility-spec 'erc-match) RET.
(defun erc-scenarios-match--invisible-stamp (hiddenp visiblep)
  (unless noninteractive
    (kill-new "(remove-from-invisibility-spec 'erc-match)"))

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "join/legacy")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.1)
       (erc-timestamp-only-if-changed-flag nil)
       (erc-fools '("bob"))
       (erc-text-matched-hook '(erc-hide-fools))
       (erc-autojoin-channels-alist '((FooNet "#chan")))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :full-name "tester"
                                :password "changeme"
                                :nick "tester")
        ;; Module `timestamp' follows `match' in insertion hooks.
        (should (memq 'erc-add-timestamp
                      (memq 'erc-match-message erc-insert-modify-hook)))
        (funcall expect 5 "This server is in debug mode")))

    (ert-info ("Ensure lines featuring \"bob\" are invisible")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (should (funcall expect 10 "<bob> tester, welcome!"))
        (ert-info ("<bob> tester, welcome!") (funcall hiddenp))

        ;; Alice's is the only one visible.
        (should (funcall expect 10 "<alice> tester, welcome!"))
        (ert-info ("<alice> tester, welcome!") (funcall visiblep))

        (should (funcall expect 10 "<bob> alice: But, as it seems"))
        (ert-info ("<bob> alice: But, as it seems") (funcall hiddenp))

        (should (funcall expect 10 "<alice> bob: Well, this is the forest"))
        (ert-info ("<alice> bob: Well, this is the forest") (funcall hiddenp))

        (should (funcall expect 10 "<alice> bob: And will you"))
        (ert-info ("<alice> bob: And will you") (funcall hiddenp))

        (should (funcall expect 10 "<bob> alice: Live, and be prosperous"))
        (ert-info ("<bob> alice: Live, and be prosperous") (funcall hiddenp))

        (should (funcall expect 10 "ERC>"))
        (should-not (get-text-property (pos-bol) 'invisible))
        (should-not (get-text-property (point) 'invisible))))))

;; This asserts that when stamps appear before a message, registered
;; invisibility properties owned by modules span the entire message.
(ert-deftest erc-scenarios-match--stamp-left-fools-invisible ()
  :tags '(:expensive-test)
  (let ((erc-insert-timestamp-function #'erc-insert-timestamp-left))
    (erc-scenarios-match--invisible-stamp

     (lambda ()
       ;; This is a time-stamped message.
       (should (eq (field-at-pos (pos-bol)) 'erc-timestamp))

       ;; Leading stamp has combined `invisible' property value.
       (should (equal (get-text-property (pos-bol) 'invisible)
                      '(timestamp erc-match)))

       ;; Message proper has the `invisible' property `erc-match'.
       (let ((msg-beg (next-single-property-change (pos-bol) 'invisible)))
         (should (eq (get-text-property msg-beg 'invisible) 'erc-match))
         (should (>= (next-single-property-change msg-beg 'invisible nil)
                     (pos-eol)))))

     (lambda ()
       ;; This is a time-stamped message.
       (should (eq (field-at-pos (pos-bol)) 'erc-timestamp))
       (should (get-text-property (pos-bol) 'invisible))

       ;; The entire message proper is visible.
       (let ((msg-beg (next-single-property-change (pos-bol) 'invisible)))
         (should
          (= (next-single-property-change msg-beg 'invisible nil (pos-eol))
             (pos-eol))))))))

(defun erc-scenarios-match--find-eol ()
  (save-excursion
    (goto-char (next-single-property-change (point) 'erc-command))
    (pos-eol)))

;; In most cases, `erc-hide-fools' makes line endings invisible.
(ert-deftest erc-scenarios-match--stamp-right-fools-invisible ()
  :tags '(:expensive-test)
  (let ((erc-insert-timestamp-function #'erc-insert-timestamp-right))
    (erc-scenarios-match--invisible-stamp

     (lambda ()
       (let ((end (erc-scenarios-match--find-eol)))
         ;; The end of the message is a newline.
         (should (= ?\n (char-after end)))

         ;; Every message has a trailing time stamp.
         (should (eq (field-at-pos (1- end)) 'erc-timestamp))

         ;; Stamps have a combined `invisible' property value.
         (should (equal (get-text-property (1- end) 'invisible)
                        '(timestamp erc-match)))

         ;; The final newline is hidden by `match', not `stamps'
         (should (equal (get-text-property end 'invisible) 'erc-match))

         ;; The message proper has the `invisible' property `erc-match',
         ;; and it starts after the preceding newline.
         (should (eq (get-text-property (pos-bol) 'invisible) 'erc-match))

         ;; It ends just before the timestamp.
         (let ((msg-end (next-single-property-change (pos-bol) 'invisible)))
           (should (equal (get-text-property msg-end 'invisible)
                          '(timestamp erc-match)))

           ;; Stamp's `invisible' property extends throughout the stamp
           ;; and ends before the trailing newline.
           (should (= (next-single-property-change msg-end 'invisible) end)))))

     (lambda ()
       (let ((end (erc-scenarios-match--find-eol)))
         ;; This message has a time stamp like all the others.
         (should (eq (field-at-pos (1- end)) 'erc-timestamp))

         ;; The entire message proper is visible.
         (should-not (get-text-property (pos-bol) 'invisible))
         (let ((inv-beg (next-single-property-change (pos-bol) 'invisible)))
           (should (eq (get-text-property inv-beg 'invisible)
                       'timestamp))))))))

;; This asserts that when `erc-fill-wrap-mode' is enabled, ERC hides
;; the preceding message's line ending.
(ert-deftest erc-scenarios-match--stamp-right-invisible-fill-wrap ()
  :tags '(:expensive-test)
  (let ((erc-insert-timestamp-function #'erc-insert-timestamp-right)
        (erc-fill-function #'erc-fill-wrap))
    (erc-scenarios-match--invisible-stamp

     (lambda ()
       ;; Every message has a trailing time stamp.
       (should (eq (field-at-pos (1- (pos-eol))) 'erc-timestamp))

       ;; Stamps appear in the right margin.
       (should (equal (car (get-text-property (1- (pos-eol)) 'display))
                      '(margin right-margin)))

       ;; Stamps have a combined `invisible' property value.
       (should (equal (get-text-property (1- (pos-eol)) 'invisible)
                      '(timestamp erc-match)))

       ;; The message proper has the `invisible' property `erc-match',
       ;; which starts at the preceding newline...
       (should (eq (get-text-property (1- (pos-bol)) 'invisible) 'erc-match))

       ;; ... and ends just before the timestamp.
       (let ((msgend (next-single-property-change (1- (pos-bol)) 'invisible)))
         (should (equal (get-text-property msgend 'invisible)
                        '(timestamp erc-match)))

         ;; The newline before `erc-insert-marker' is still visible.
         (should-not (get-text-property (pos-eol) 'invisible))
         (should (= (next-single-property-change msgend 'invisible)
                    (pos-eol)))))

     (lambda ()
       ;; This message has a time stamp like all the others.
       (should (eq (field-at-pos (1- (pos-eol))) 'erc-timestamp))

       ;; Unlike hidden messages, the preceding newline is visible.
       (should-not (get-text-property (1- (pos-bol)) 'invisible))

       ;; The entire message proper is visible.
       (let ((inv-beg (next-single-property-change (1- (pos-bol)) 'invisible)))
         (should (eq (get-text-property inv-beg 'invisible) 'timestamp)))))))

(ert-deftest erc-scenarios-match--stamp-both-invisible-fill-static ()
  :tags '(:expensive-test)
  (should (eq erc-insert-timestamp-function
              #'erc-insert-timestamp-left-and-right))

  ;; Rewind the clock to known date artificially.
  (let ((erc-stamp--current-time 704591940)
        (erc-stamp--tz t)
        (erc-fill-function #'erc-fill-static)
        (bob-utterance-counter 0))

    (erc-scenarios-match--invisible-stamp

     (lambda ()
       (ert-info ("Baseline check")
         ;; False date printed initially before anyone speaks.
         (when (zerop bob-utterance-counter)
           (save-excursion
             (goto-char (point-min))
             (search-forward "[Wed Apr 29 1992]")
             (search-forward "[23:59]"))))

       (ert-info ("Line endings in Bob's messages are invisible")
         ;; The message proper has the `invisible' property `erc-match'.
         (should (eq (get-text-property (pos-bol) 'invisible) 'erc-match))
         (let* ((mbeg (next-single-property-change (pos-bol) 'erc-command))
                (mend (next-single-property-change mbeg 'erc-command)))

           (if (/= 1 bob-utterance-counter)
               (should-not (field-at-pos mend))
             ;; For Bob's stamped message, check newline after stamp.
             (should (eq (field-at-pos mend) 'erc-timestamp))
             (setq mend (field-end mend)))

           ;; The `erc-timestamp' property spans entire messages,
           ;; including stamps and filled text, which makes for
           ;; convenient traversal when `erc-stamp-mode' is enabled.
           (should (get-text-property (pos-bol) 'erc-timestamp))
           (should (= (next-single-property-change (pos-bol) 'erc-timestamp)
                      mend))

           ;; Line ending has the `invisible' property `erc-match'.
           (should (= (char-after mend) ?\n))
           (should (eq (get-text-property mend'invisible) 'erc-match))))

       ;; Only the message right after Alice speaks contains stamps.
       (when (= 1 bob-utterance-counter)

         (ert-info ("Date stamp occupying previous line is invisible")
           (save-excursion
             (forward-line -1)
             (goto-char (pos-bol))
             (should (looking-at (rx "[Mon May  4 1992]")))
             ;; Date stamp has a combined `invisible' property value
             ;; that extends until the start of the message proper.
             (should (equal (get-text-property (point) 'invisible)
                            '(timestamp erc-match)))
             (should (= (next-single-property-change (point) 'invisible)
                        (1+ (pos-eol))))))

         (ert-info ("Folding preserved despite invisibility")
           ;; Message has a trailing time stamp, but it's been folded
           ;; over to the next line.
           (should-not (eq (field-at-pos (1- (pos-eol))) 'erc-timestamp))
           (save-excursion
             (forward-line)
             (should (eq (field-at-pos (1- (pos-eol))) 'erc-timestamp)))

           ;; Stamp invisibility starts where message's ends.
           (let ((msgend (next-single-property-change (pos-bol) 'invisible)))
             ;; Stamp has a combined `invisible' property value.
             (should (equal (get-text-property msgend 'invisible)
                            '(timestamp erc-match)))

             ;; Combined `invisible' property spans entire timestamp.
             (should (= (next-single-property-change msgend 'invisible)
                        (save-excursion (forward-line) (pos-eol)))))))

       (cl-incf bob-utterance-counter))

     ;; Alice.
     (lambda ()
       ;; Set clock ahead a week or so.
       (setq erc-stamp--current-time 704962800)

       ;; This message has no time stamp and is completely visible.
       (should-not (eq (field-at-pos (1- (pos-eol))) 'erc-timestamp))
       (should-not (next-single-property-change (pos-bol) 'invisible))))))

;;; erc-scenarios-match.el ends here
