;;; erc-scenarios-match.el --- Misc `erc-match' scenarios -*- lexical-binding: t -*-

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
                      (memq 'erc-match-message
                            (default-value 'erc-insert-modify-hook))))
        ;; The "match type" is `current-nick'.
        (funcall expect 5 "tester")
        (should (eq (get-text-property (1- (point)) 'font-lock-face)
                    'erc-current-nick-face))))))

;; When hacking on tests that use this fixture, it's best to run it
;; interactively, and visually inspect the output with various
;; combinations of:
;;
;;   M-x erc-match-toggle-hidden-fools RET
;;   M-x erc-toggle-timestamps RET
;;
(defun erc-scenarios-match--invisible-stamp (hiddenp visiblep)
  (unless noninteractive
    (push "erc-match-toggle-hidden-fools" extended-command-history)
    (push "erc-toggle-timestamps" extended-command-history))

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
                      (memq 'erc-match-message
                            (default-value 'erc-insert-modify-hook))))
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
                      '(match-fools timestamp)))

       ;; Message proper has the `invisible' property `match-fools'.
       (let ((msg-beg (next-single-property-change (pos-bol) 'invisible)))
         (should (eq (get-text-property msg-beg 'invisible) 'match-fools))
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

;; In most cases, `erc-hide-fools' makes line endings invisible.
(defun erc-scenarios-match--stamp-right-fools-invisible ()
  (let ((erc-insert-timestamp-function #'erc-insert-timestamp-right))
    (erc-scenarios-match--invisible-stamp

     (lambda ()
       (pcase-let ((`(,beg . ,end) (erc--get-inserted-msg-bounds)))
         ;; The end of the message is a newline.
         (should (= ?\n (char-after end)))

         ;; Every message has a trailing time stamp.
         (should (eq (field-at-pos (1- end)) 'erc-timestamp))

         ;; Stamps have a combined `invisible' property value.
         (should (equal (get-text-property (1- end) 'invisible)
                        '(match-fools timestamp)))

         ;; The final newline is hidden by `match', not `stamps'
         (with-suppressed-warnings ((obsolete erc-legacy-invisible-bounds-p))
           (if erc-legacy-invisible-bounds-p
               (should (eq (get-text-property end 'invisible) 'match-fools))
             (should (eq (get-text-property beg 'invisible) 'match-fools))
             (should-not (get-text-property end 'invisible))))

         ;; The message proper has the `invisible' property `match-fools',
         ;; and it starts after the preceding newline.
         (should (eq (get-text-property (pos-bol) 'invisible) 'match-fools))

         ;; It ends just before the timestamp.
         (let ((msg-end (next-single-property-change (pos-bol) 'invisible)))
           (should (equal (get-text-property msg-end 'invisible)
                          '(match-fools timestamp)))

           ;; Stamp's `invisible' property extends throughout the stamp
           ;; and ends before the trailing newline.
           (should (= (next-single-property-change msg-end 'invisible) end)))))

     (lambda ()
       (let ((end (erc--get-inserted-msg-end (point))))
         ;; This message has a time stamp like all the others.
         (should (eq (field-at-pos (1- end)) 'erc-timestamp))

         ;; The entire message proper is visible.
         (should-not (get-text-property (pos-bol) 'invisible))
         (let ((inv-beg (next-single-property-change (pos-bol) 'invisible)))
           (should (eq (get-text-property inv-beg 'invisible)
                       'timestamp))))))))

(ert-deftest erc-scenarios-match--stamp-right-fools-invisible ()
  :tags '(:expensive-test)
  (erc-scenarios-match--stamp-right-fools-invisible))

(ert-deftest erc-scenarios-match--stamp-right-fools-invisible--nooffset ()
  :tags '(:expensive-test)
  (with-suppressed-warnings ((obsolete erc-legacy-invisible-bounds-p))
    (should-not erc-legacy-invisible-bounds-p)
    (let ((erc-legacy-invisible-bounds-p t))
      (erc-scenarios-match--stamp-right-fools-invisible))))

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
                      '(match-fools timestamp)))

       ;; The message proper has the `invisible' property `match-fools',
       ;; which starts at the preceding newline...
       (should (eq (get-text-property (1- (pos-bol)) 'invisible) 'match-fools))

       ;; ... and ends just before the timestamp.
       (let ((msgend (next-single-property-change (1- (pos-bol)) 'invisible)))
         (should (equal (get-text-property msgend 'invisible)
                        '(match-fools timestamp)))

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

(defun erc-scenarios-match--fill-wrap-stamp-dedented-p (point)
  (pcase (get-text-property point 'line-prefix)
    (`(space :width (- erc-fill--wrap-value (,n)))
     (if (display-graphic-p) (< 100 n 200) (< 10 n 30)))
    (`(space :width (- erc-fill--wrap-value ,n))
     (< 10 n 30))))

(ert-deftest erc-scenarios-match--hide-fools/stamp-both/fill-wrap ()

  ;; Rewind the clock to known date artificially.  We should probably
  ;; use a ticks/hz cons on 29+.
  (let ((erc-stamp--current-time 704591940)
        (erc-stamp--tz t)
        (erc-fill-function #'erc-fill-wrap)
        (bob-utterance-counter 0))

    (erc-scenarios-match--invisible-stamp

     (lambda ()
       (ert-info ("Baseline check")
         ;; False date printed initially before anyone speaks.
         (when (zerop bob-utterance-counter)
           (save-excursion
             (goto-char (point-min))
             (search-forward "[Wed Apr 29 1992]")
             ;; First stamp in a buffer is not invisible from previous
             ;; newline (before stamp's own leading newline).
             (should (= 4 (match-beginning 0)))
             (should (get-text-property 3 'invisible))
             (should-not (get-text-property 2 'invisible))
             (should (erc-scenarios-match--fill-wrap-stamp-dedented-p 4))
             (search-forward "[23:59]"))))

       (ert-info ("Line endings in Bob's messages are invisible")
         ;; The message proper has the `invisible' property `match-fools'.
         (should (eq (get-text-property (pos-bol) 'invisible) 'match-fools))
         (pcase-let ((`(,mbeg . ,mend) (erc--get-inserted-msg-bounds)))
           (should (= (char-after mend) ?\n))
           (should-not (field-at-pos mend))
           (should-not (field-at-pos mbeg))

           (when (= bob-utterance-counter 1)
             (let ((right-stamp (field-end mbeg)))
               (should (eq 'erc-timestamp (field-at-pos right-stamp)))
               (should (= mend (field-end right-stamp)))
               (should (eq (field-at-pos (1- mend)) 'erc-timestamp))))

           ;; The `erc--ts' property is present in prop stack.
           (should (get-text-property (pos-bol) 'erc--ts))
           (should-not (next-single-property-change (1+ (pos-bol)) 'erc--ts))

           ;; Line ending has the `invisible' property `match-fools'.
           (should (eq (get-text-property mbeg 'invisible) 'match-fools))
           (should-not (get-text-property mend 'invisible))))

       ;; Only the message right after Alice speaks contains stamps.
       (when (= 1 bob-utterance-counter)

         (ert-info ("Date stamp occupying previous line is invisible")
           (should (eq 'match-fools (get-text-property (point) 'invisible)))
           (save-excursion
             (forward-line -1)
             (goto-char (pos-bol))
             (should (looking-at (rx "[Mon May  4 1992]")))
             (ert-info ("Stamp's NL `invisible' as fool, not timestamp")
               (let ((end (match-end 0)))
                 (should (eq (char-after end) ?\n))
                 (should (eq 'timestamp
                             (get-text-property (1- end) 'invisible)))
                 (should (eq 'match-fools
                             (get-text-property end 'invisible)))))
             (should (erc-scenarios-match--fill-wrap-stamp-dedented-p (point)))
             ;; Date stamp has a combined `invisible' property value
             ;; that starts at the previous message's trailing newline
             ;; and extends until the start of the message proper.
             (should (equal ?\n (char-before (point))))
             (should (equal ?\n (char-before (1- (point)))))
             (let ((val (get-text-property (- (point) 2) 'invisible)))
               (should (equal val 'timestamp))
               (should (= (text-property-not-all (- (point) 2) (point-max)
                                                 'invisible val)
                          (pos-eol))))))

         (ert-info ("Current message's RHS stamp is hidden")
           ;; Right stamp has `match-fools' property.
           (save-excursion
             (should-not (field-at-pos (point)))
             (should (eq (field-at-pos (1- (pos-eol))) 'erc-timestamp)))

           ;; Stamp invisibility starts where message's ends.
           (let ((msgend (next-single-property-change (pos-bol) 'invisible)))
             ;; Stamp has a combined `invisible' property value.
             (should (equal (get-text-property msgend 'invisible)
                            '(match-fools timestamp)))

             ;; Combined `invisible' property spans entire timestamp.
             (should (= (next-single-property-change msgend 'invisible)
                        (pos-eol))))))

       (cl-incf bob-utterance-counter))

     ;; Alice.
     (lambda ()
       ;; Set clock ahead a week or so.
       (setq erc-stamp--current-time 704962800)

       ;; This message has no time stamp and is completely visible.
       (should-not (eq (field-at-pos (1- (pos-eol))) 'erc-timestamp))
       (should-not (next-single-property-change (pos-bol) 'invisible))))))

;; This asserts that speaker hiding by `erc-fill-wrap-merge' doesn't
;; take place after a series of hidden fool messages with an
;; intervening outgoing message followed immediately by a non-fool
;; message from the last non-hidden speaker (other than the user).
(ert-deftest erc-scenarios-match--hide-fools/stamp-both/fill-wrap/speak ()

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "match/fools")
       (erc-stamp--current-time 704591940)
       (dumb-server (erc-d-run "localhost" t 'fill-wrap))
       (erc-stamp--tz t)
       (erc-fill-function #'erc-fill-wrap)
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
                      (memq 'erc-match-message
                            (default-value 'erc-insert-modify-hook))))
        (funcall expect 5 "This server is in debug mode")))

    (ert-info ("Ensure lines featuring \"bob\" are invisible")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (should (funcall expect 10 "<alice> None better than"))
        (should (funcall expect 10 "<alice> bob: Still we went"))
        (should (funcall expect 10 "<bob> alice: Give me your hand"))
        (erc-scenarios-common-say "hey")
        (should (funcall expect 10 "<bob> You have paid the heavens"))
        (should (funcall expect 10 "<alice> bob: In the sick air"))
        (should (funcall expect 10 "<alice> The web of our life"))

        ;; Regression (see leading comment).
        (should-not (equal "" (get-text-property (pos-bol) 'display)))

        ;; No remaining meta-data positions, no more timestamps.
        (should-not (next-single-property-change (1+ (pos-bol)) 'erc--ts))
        ;; No remaining invisible messages.
        (should-not (text-property-not-all (pos-bol) erc-insert-marker
                                           'invisible nil))

        (should (funcall expect 10 "ERC>"))
        (should-not (get-text-property (pos-bol) 'invisible))
        (should-not (get-text-property (point) 'invisible))))))

(defun erc-scenarios-match--stamp-both-invisible-fill-static (assert-ds)
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
         ;; The message proper has the `invisible' property `match-fools'.
         (should (eq (get-text-property (pos-bol) 'invisible) 'match-fools))
         (pcase-let ((`(,mbeg . ,mend) (erc--get-inserted-msg-bounds)))

           (should (= (char-after mend) ?\n))
           (should-not (field-at-pos mbeg))
           (should-not (field-at-pos mend))
           (when (= 1 bob-utterance-counter)
             ;; For Bob's stamped message, check newline after stamp.
             (should (eq (field-at-pos (field-end mbeg)) 'erc-timestamp))
             (should (eq (field-at-pos (1- mend)) 'erc-timestamp)))

           ;; The `erc--ts' property is present in the message's
           ;; width 1 prop collection at its first char.
           (should (get-text-property (pos-bol) 'erc--ts))
           (should-not (next-single-property-change (1+ (pos-bol)) 'erc--ts))

           ;; Line ending has the `invisible' property `match-fools'.
           (should (= (char-after mend) ?\n))
           (with-suppressed-warnings ((obsolete erc-legacy-invisible-bounds-p))
             (if erc-legacy-invisible-bounds-p
                 (should (eq (get-text-property mend 'invisible) 'match-fools))
               (should (eq (get-text-property mbeg 'invisible) 'match-fools))
               (should-not (get-text-property mend 'invisible))))))

       ;; Only the message right after Alice speaks contains stamps.
       (when (= 1 bob-utterance-counter)

         (ert-info ("Date stamp occupying previous line is invisible")
           (save-excursion
             (forward-line -1)
             (goto-char (pos-bol))
             (should (looking-at (rx "[Mon May  4 1992]")))
             (should (= ?\n (char-after (- (point) 2)))) ; welcome!\n
             (funcall assert-ds))) ; "assert date stamp"

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
                            '(match-fools timestamp)))

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

;; FIXME explain why these next two fail on FreeBSD 14.2 (Bug#74722).
(ert-deftest erc-scenarios-match--stamp-both-invisible-fill-static ()
  :tags `(:expensive-test ,@(and (eq system-type 'berkeley-unix) '(:unstable)))
  (erc-scenarios-match--stamp-both-invisible-fill-static

   (lambda ()
     ;; Date stamp has an `invisible' property that starts from the
     ;; newline delimiting the current and previous messages and
     ;; extends until the stamp's final newline.  It is not combined
     ;; with the old value, `match-fools'.
     (let ((delim-pos (- (point) 2)))
       (should (equal 'timestamp (get-text-property delim-pos 'invisible)))
       ;; Stamp-only invisibility ends before its last newline.
       (should (= (text-property-not-all delim-pos (point-max)
                                         'invisible 'timestamp)
                  (match-end 0))))))) ; pos-eol

(ert-deftest erc-scenarios-match--stamp-both-invisible-fill-static--nooffset ()
  :tags `(:expensive-test ,@(and (eq system-type 'berkeley-unix) '(:unstable)))
  (with-suppressed-warnings ((obsolete erc-legacy-invisible-bounds-p))
    (should-not erc-legacy-invisible-bounds-p)

    (let ((erc-legacy-invisible-bounds-p t))
      (erc-scenarios-match--stamp-both-invisible-fill-static

       (lambda ()
         ;; Date stamp has an `invisible' property that covers its
         ;; format string exactly.  It is not combined with the old
         ;; value, `match-fools'.
         (let ((delim-prev (- (point) 2)))
           (should-not (get-text-property delim-prev 'invisible))
           (should (eq 'erc-timestamp (field-at-pos (point))))
           (should (= (next-single-property-change delim-prev 'invisible)
                      (field-beginning (point))))
           (should (equal 'timestamp
                          (get-text-property (1- (point)) 'invisible)))
           ;; Field stops before final newline because the date stamp
           ;; is (now, as of ERC 5.6) its own standalone message.
           (should (= ?\n (char-after (field-end (point)))))
           ;; Stamp-only invisibility includes last newline.
           (should (= (text-property-not-all (1- (point)) (point-max)
                                             'invisible 'timestamp)
                      (1+ (field-end (point)))))))))))

;;; erc-scenarios-match.el ends here
