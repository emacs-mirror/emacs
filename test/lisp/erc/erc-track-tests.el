;;; erc-track-tests.el --- Tests for erc-track.  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Author: Vivek Dasmohapatra <vivek@etla.org>

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

(require 'erc-track)
(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-tests-common)))


(ert-deftest erc-track--shorten-aggressive-nil ()
  "Test non-aggressive erc track buffer name shortening."
  (let (erc-track-shorten-aggressively)
    (should
     (equal (erc-unique-channel-names '("#emacs" "#vi" "#electronica" "#folk")
                                      '("#emacs" "#vi"))
            '("#em" "#vi")))
    (should
     (equal (erc-unique-channel-names '("#linux-de" "#linux-fr")
                                      '("#linux-de" "#linux-fr"))
            '("#linux-de" "#linux-fr")))
    (should
     (equal (erc-unique-channel-names
             '("#dunnet" "#lisp" "#sawfish" "#fsf" "#guile" "#testgnome"
               "#gnu" "#fsbot" "#hurd" "#hurd-bunny" "#emacs")
             '("#hurd-bunny" "#hurd" "#sawfish" "#lisp"))
            '("#hurd-" "#hurd" "#s" "#l")))
    (should
     (equal (erc-unique-substrings '("#emacs" "#vi" "#electronica" "#folk"))
            '("#em" "#vi" "#el" "#f")))
    (should
     (equal (erc-unique-channel-names
             '("#emacs" "#burse" "+linux.de" "#starwars"
               "#bitlbee" "+burse" "#ratpoison")
             '("+linux.de" "#starwars" "#burse"))
            '("+l" "#s" "#bu")))
    (should
     (equal (erc-unique-channel-names '("fsbot" "#emacs" "deego") '("fsbot"))
            '("fs")))
    (should
     (equal (erc-unique-channel-names '("fsbot" "#emacs" "deego")
                                      '("fsbot")
                                      (lambda (s) (> (length s) 4)) 1)
            '("f")))
    (should
     (equal (erc-unique-channel-names '("fsbot" "#emacs" "deego")
                                      '("fsbot")
                                      (lambda (s) (> (length s) 4)) 2)
            '("fs")))
    (should
     (equal (erc-unique-channel-names '("deego" "#hurd" "#hurd-bunny" "#emacs")
                                      '("#hurd" "#hurd-bunny"))
            '("#hurd" "#hurd-")))
    (should
     (and
      (equal (erc-unique-substring-1 "abc" '("ab" "abcd")) "abcd")
      (not   (erc-unique-substring-1 "a" '("xyz" "xab")))
      (equal (erc-unique-substrings '("abc" "xyz" "xab")) '("abc" "xyz" "xab"))
      (equal (erc-unique-substrings '("abc" "abcdefg")) '("abc" "abcd")))) ))

(ert-deftest erc-track--shorten-aggressive-t ()
  "Test aggressive erc track buffer name shortening."
  (let ((erc-track-shorten-aggressively t))
    (should
     (equal (erc-unique-channel-names '("#emacs" "#vi" "#electronica" "#folk")
                                      '("#emacs" "#vi"))
            '("#em" "#v")))
    (should
     (equal (erc-unique-channel-names '("#linux-de" "#linux-fr")
                                      '("#linux-de" "#linux-fr"))
            '("#linux-d" "#linux-f")))
    (should
     (equal (erc-unique-substrings '("#emacs" "#vi" "#electronica" "#folk"))
            '("#em" "#v" "#el" "#f")))
    (should
     (and
      (equal (erc-unique-substring-1 "abc" '("ab" "abcd")) "abcd")
      (not   (erc-unique-substring-1 "a" '("xyz" "xab")))
      (equal (erc-unique-substrings '("abc" "xyz" "xab")) '("ab" "xy" "xa"))
      (equal (erc-unique-substrings '("abc" "abcdefg")) '("abc" "abcd")))) ))

(ert-deftest erc-track--shorten-aggressive-max ()
  "Test maximally aggressive erc track buffer name shortening."
  (let ((erc-track-shorten-aggressively 'max))
    (should
     (equal (erc-unique-channel-names '("#emacs" "#vi" "#electronica" "#folk")
                                      '("#emacs" "#vi"))
            '("#e" "#v"))) ))

(ert-deftest erc-track--shortened-names ()
  (let (erc-track--shortened-names
        erc-track--shortened-names-current-hash
        results)

    (with-memoization (erc-track--shortened-names-get
                       '("apple" "banana" "cherries"))
      '("a" "b" "c"))
    (should (integerp (car erc-track--shortened-names)))
    (should (equal (cdr erc-track--shortened-names) '("a" "b" "c")))
    (push erc-track--shortened-names results)

    ;; Redundant call doesn't run.
    (with-memoization (erc-track--shortened-names-get
                       '("apple" "banana" "cherries"))
      (should-not 'run)
      '("a" "b" "c"))
    (should (equal erc-track--shortened-names (car results)))

    ;; Change in environment or context forces run.
    (with-temp-buffer
      (with-memoization (erc-track--shortened-names-get
                         '("apple" "banana" "cherries"))
        '("x" "y" "z")))
    (should (and (integerp (car erc-track--shortened-names))
                 (/= (car erc-track--shortened-names) (caar results))))
    (should (equal (cdr erc-track--shortened-names) '("x" "y" "z")))
    (push erc-track--shortened-names results)

    (with-memoization (erc-track--shortened-names-get
                       '("apple" "banana" "cherries"))
      '("1" "2" "3"))
    (should (and (integerp (car erc-track--shortened-names))
                 (/= (car erc-track--shortened-names) (caar results))))
    (should (equal (cdr erc-track--shortened-names) '("1" "2" "3")))))

(ert-deftest erc-track--erc-faces-in ()
  "`erc-faces-in' should pick up both 'face and 'font-lock-face properties."
  (let ((str0 (copy-sequence "is bold"))
        (str1 (copy-sequence "is bold")))
    ;; Turn on Font Lock mode: this initialize `char-property-alias-alist'
    ;; to '((face font-lock-face)).  Note that `font-lock-mode' don't
    ;; turn on the mode if the test is run on batch mode or if the
    ;; buffer name starts with ?\s (Bug#23954).
    (unless font-lock-mode (font-lock-default-function 1))
    (put-text-property 3 (length str0) 'font-lock-face
                       '(bold erc-current-nick-face) str0)
    (put-text-property 3 (length str1) 'face
                       '(bold erc-current-nick-face) str1)
    (should (erc-faces-in str0))
    (should (erc-faces-in str1)) ))

;; This simulates an alternating bold/non-bold [#c] in the mode-line,
;; i.e., an `erc-modified-channels-alist' that vacillates between
;;
;;   ((#<buffer #chan> 42 . erc-default-face))
;;
;; and
;;
;;   ((#<buffer #chan> 42 erc-nick-default-face erc-default-face))
;;
;; This is a fairly typical scenario where consecutive messages
;; feature speaker and addressee button highlighting and otherwise
;; plain message bodies.  This mapping of phony to real faces
;; describes the picture in 5.6:
;;
;;   `1': (erc-button erc-default-face)                 ; URL
;;   `2': (erc-nick-default-face erc-default-face)      ; mention
;;   `3': erc-default-face                              ; body
;;   `_': (erc-nick-default-face erc-nick-default-face) ; speaker
;;
;; The `_' represents a commonly occurring face (a <speaker>) that's
;; not present in either option's default (standard) value.  It's a
;; no-op from the POV of `erc-track-select-mode-line-face'.

(ert-deftest erc-track-select-mode-line-face ()

  ;; Observed (see key above).
  (let ((erc-track-faces-priority-list '(1 2 3))
        (erc-track-faces-normal-list   '(1 2 3)))

    (should (equal 2 (erc-track-select-mode-line-face 3 '(2 _ 3))))
    (should (equal 2 (erc-track-select-mode-line-face 2 '(2 _ 3))))
    (should (equal 3 (erc-track-select-mode-line-face 2 '(_ 3))))
    (should (equal 2 (erc-track-select-mode-line-face 3 '(2 3))))
    (should (equal 3 (erc-track-select-mode-line-face 2 '(3))))

    (should (equal 1 (erc-track-select-mode-line-face 1 '(2 1 3))))
    (should (equal 1 (erc-track-select-mode-line-face 1 '(1 3))))
    (should (equal 1 (erc-track-select-mode-line-face 1 '(1 3 2))))
    (should (equal 1 (erc-track-select-mode-line-face 1 '(3 1)))))

  ;; When the current face outranks all new faces and doesn't appear
  ;; among them, it's eligible to be replaced with a fellow "normal"
  ;; from those new faces.  But if it does appear among them, it's
  ;; never replaced.
  (let ((erc-track-faces-priority-list '(a b))
        (erc-track-faces-normal-list   '(a b)))

    (should (equal 'a (erc-track-select-mode-line-face 'a '(b a))))
    (should (equal 'a (erc-track-select-mode-line-face 'a '(a b))))
    (should (equal 'a (erc-track-select-mode-line-face 'b '(b a))))
    (should (equal 'a (erc-track-select-mode-line-face 'b '(a b))))

    (should (equal 'a (erc-track-select-mode-line-face 'b '(a))))
    (should (equal 'b (erc-track-select-mode-line-face 'a '(b)))))

  ;; The ordering of the "normal" list doesn't matter.
  (let ((erc-track-faces-priority-list '(a b))
        (erc-track-faces-normal-list   '(b a)))

    (should (equal 'a (erc-track-select-mode-line-face 'a '(b a))))
    (should (equal 'a (erc-track-select-mode-line-face 'a '(a b))))
    (should (equal 'a (erc-track-select-mode-line-face 'b '(b a))))
    (should (equal 'a (erc-track-select-mode-line-face 'b '(a b))))))

(defun erc-track-tests--select-mode-line-face (ranked normals cases)
  (setq normals (map-into (mapcar (lambda (f) (cons f t)) normals)
                          '(hash-table :test equal)))

  (setq ranked (cons (map-into (mapcar (let ((i 0))
                                         (lambda (f) (cons f (cl-incf i))))
                                       ranked)
                               '(hash-table :test equal))
                     ranked))

  (pcase-dolist (`(,want ,cur-face ,new-faces) cases)

    (ert-info ((format "Observed: {cur: %S, new: %S, want: %S}"
                       cur-face new-faces want))
      (setq new-faces (cons (map-into
                             (mapcar (lambda (f) (cons f t)) new-faces)
                             '(hash-table :test equal))
                            (reverse new-faces)))
      (should (equal want (erc-track--select-mode-line-face
                           cur-face new-faces ranked normals))))))

;; The main difference between these variants is that with the above,
;; when given alternating lines like
;;
;;  CUR      NEW                          CHOICE
;;   text     (mention $speaker text)  =>   mention
;;   mention  ($speaker text)          =>   text
;;
;; we see the effect of alternating faces in the indicator.  But when
;; given consecutive lines with a similar composition, like
;;
;;   text     (mention $speaker text)  =>   mention
;;   text     (mention $speaker text)  =>   mention
;;
;; we lose the effect.  With the variant below, we get
;;
;;   text     (mention $speaker text)  =>   mention
;;   text     (mention $speaker text)  =>   text
;;

(ert-deftest erc-track--select-mode-line-face ()
  (should-not erc-track-ignore-normal-contenders-p)

  ;; These are the same test cases from the previous test.  The syntax
  ;; is (expected cur-face new-faces).
  (erc-track-tests--select-mode-line-face
   '(1 2 3) '(1 2 3)
   '((2 3 (2 _ 3))
     (3 2 (2 _ 3))
     (3 2 (_ 3))
     (2 3 (2 3))
     (3 2 (3))
     (2 1 (2 1 3))
     (3 1 (1 3))
     (2 1 (1 3 2))
     (3 1 (3 1))))

  (erc-track-tests--select-mode-line-face
   '(a b) '(a b)
   '((b a (b a))
     (b a (a b))
     (a b (b a))
     (a b (a b))
     (a b (a))
     (b a (b))))

  (erc-track-tests--select-mode-line-face
   '(a b) '(b a)
   '((b a (b a))
     (b a (a b))
     (a b (b a))
     (a b (a b)))))

(ert-deftest erc-track--collect-faces-in ()
  (with-current-buffer (get-buffer-create "*erc-track--get-faces-in*")
    (erc-tests-common-prep-for-insertion)
    (goto-char (point-min))
    (skip-chars-forward "\n")

    (let ((ts #("[04:37]"
                0 1 ( erc--msg 0 field erc-timestamp
                      font-lock-face erc-timestamp-face)
                1 7 ( field erc-timestamp
                      font-lock-face erc-timestamp-face)))
          bounds)

      (with-silent-modifications

        (push (list (point)) bounds)
        (insert ; JOIN
         ts "      " ; initial `fill' indentation lacks properties
         #("*** You have joined channel #chan" 0 33
           (font-lock-face erc-notice-face))
         "\n")
        (setcdr (car bounds) (point))

        (push (list (point)) bounds)
        (insert ; 353
         ts "      "
         #("*** Users on #chan: bob alice dummy tester"
           0 30 (font-lock-face erc-notice-face)
           30 35 (font-lock-face erc-current-nick-face)
           35 42 (font-lock-face erc-notice-face))
         "\n" #("                 @fsbot" ; but intervening HAS properties
                0 23 (font-lock-face erc-notice-face)))
        (setcdr (car bounds) (point))

        (push (list (point)) bounds)
        (insert ; PRIVMSG
         "\n" ts "  "
         #("<alice> bob: Thou canst not come to me: I come to"
           0 1 (font-lock-face erc-default-face)
           ;; erc-dangerous-host-face -> erc-nicks-alice-face (undefined)
           1 6 (font-lock-face (erc-dangerous-host-face erc-nick-default-face))
           6 8 (font-lock-face erc-default-face)
           ;; erc-pal-face -> erc-nicks-bob-face (undefined)
           8 11 (font-lock-face (erc-pal-face erc-default-face))
           11 49 (font-lock-face erc-default-face))
         "\n" #("                 thee."
                0 22 (font-lock-face erc-default-face))
         "\n")
        (setcdr (car bounds) (point)))

      (goto-char (point-max))
      (should (equal (setq bounds (nreverse bounds))
                     '((3 . 50) (50 . 129) (129 . 212))))

      ;; For these result assertions, the insertion order of the table
      ;; elements should mirror that of the consed lists.

      ;; Baseline
      (narrow-to-region 1 3)
      (let ((result (erc-track--collect-faces-in)))
        (should-not (map-pairs (car result)))
        (should-not (cdr result)))

      ;; JOIN
      (narrow-to-region (car (nth 0 bounds)) (cdr (nth 0 bounds)))
      (let ((result (erc-track--collect-faces-in)))
        (should (seq-set-equal-p
                 (map-pairs (car result)) '((erc-timestamp-face . t)
                                            (erc-notice-face . t))))
        (should (equal (cdr result) '(erc-notice-face erc-timestamp-face))))

      ;; 353
      (narrow-to-region (car (nth 1 bounds)) (cdr (nth 1 bounds)))
      (let ((result (erc-track--collect-faces-in)))
        (should (seq-set-equal-p (map-pairs (car result))
                                 '((erc-timestamp-face . t)
                                   (erc-notice-face . t)
                                   (erc-current-nick-face . t))))
        (should (equal (cdr result) '(erc-current-nick-face
                                      erc-notice-face
                                      erc-timestamp-face))))

      ;; PRIVMSG
      (narrow-to-region (car (nth 2 bounds)) (cdr (nth 2 bounds)))
      (let ((result (erc-track--collect-faces-in)))
        (should (seq-set-equal-p
                 (map-pairs (car result))
                 '((erc-timestamp-face . t)
                   (erc-default-face . t)
                   ((erc-dangerous-host-face erc-nick-default-face) . t)
                   ((erc-pal-face erc-default-face) . t))))
        (should (equal (cdr result)
                       '((erc-pal-face erc-default-face)
                         (erc-dangerous-host-face erc-nick-default-face)
                         erc-default-face
                         erc-timestamp-face))))

      ;; Entire buffer.
      (narrow-to-region (car (nth 0 bounds)) erc-insert-marker)
      (let ((result (erc-track--collect-faces-in)))
        (should (seq-set-equal-p
                 (map-pairs (car result))
                 '((erc-timestamp-face . t)
                   (erc-notice-face . t)
                   (erc-current-nick-face . t)
                   (erc-default-face . t)
                   ((erc-dangerous-host-face erc-nick-default-face) . t)
                   ((erc-pal-face erc-default-face) . t))))
        (should (equal (cdr result)
                       '((erc-pal-face erc-default-face)
                         (erc-dangerous-host-face erc-nick-default-face)
                         erc-default-face
                         erc-current-nick-face
                         erc-notice-face
                         erc-timestamp-face)))))

    (widen)
    (when noninteractive
      (kill-buffer))))

(defun erc-track-tests--modified-channels/baseline (set-faces)
  ;; Simulate a JOIN, PART, etc. that's displayed in `erc-notice-face'.
  (funcall set-faces '(erc-notice-face))
  (erc-track-modified-channels)
  (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                 '(1 . erc-notice-face)))

  ;; Someone speaks, and the mode-line face goes from ERC's generic
  ;; "notice" face, `erc-notice-face', to the first face in the
  ;; inserted message that outranks it, which happens to be the
  ;; `button' module's composite face for buttonized speakers:
  ;; (erc-button-nick-default-face erc-nick-default-face).  It
  ;; outranks both the previous occupant, `erc-notice-face', and its
  ;; one cohabitant in the message text, `erc-default-face', in
  ;; `erc-track-faces-priority-list'.  Note that in the following
  ;; list, `erc-default-face' appears first because it's used for the
  ;; opening speaker bracket "<".  The timestamp appears last because
  ;; it's a right-sided stamp appended to the message body.
  (funcall set-faces '(erc-timestamp-face
                       (erc-button-nick-default-face erc-nick-default-face)
                       erc-default-face))
  (erc-track-modified-channels)
  (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                 '(2 erc-button-nick-default-face erc-nick-default-face)))

  ;; The speaker speaks again immediately, and the segment changes to
  ;; `erc-default-face', which appears later in the message, as
  ;; normal body text.  This happens because both `erc-default-face'
  ;; and (erc-button-nick-default-face erc-nick-default-face) appear
  ;; in `erc-track-faces-normal-list', meaning the lower-ranked
  ;; former can replace the higher-ranked latter in the mode-line for
  ;; the purpose of indicating channel activity.
  (funcall set-faces '(erc-timestamp-face
                       (erc-button-nick-default-face erc-nick-default-face)
                       erc-default-face))
  (erc-track-modified-channels)
  (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                 '(3 . erc-default-face)))

  ;; Note: if (erc-button-nick-default-face erc-nick-default-face)
  ;; were removed from `erc-track-faces-priority-list' but kept in
  ;; `erc-track-faces-normal-list', then replaying the sequence would
  ;; result in the previous two results being switched:
  ;; `erc-default-face' would replace `erc-notice-face' before being
  ;; replaced by the buttonized composite.

  ;; The speaker speaks yet again, and the segment goes back to the
  ;; higher ranking face.
  (funcall set-faces '(erc-timestamp-face
                       (erc-button-nick-default-face erc-nick-default-face)
                       erc-default-face))
  (erc-track-modified-channels)
  (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                 '(4 erc-button-nick-default-face erc-nick-default-face)))

  ;; Finally, another notice arrives.  Although lower ranked, it also
  ;; appears in `erc-track-faces-normal-list' and so is eligible to
  ;; replace the incumbent.
  (funcall set-faces '(erc-notice-face))
  (erc-track-modified-channels)
  (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                 '(5 . erc-notice-face))))

(ert-deftest erc-track-modified-channels/baseline ()
  (erc-tests-common-track-modified-channels
   #'erc-track-tests--modified-channels/baseline))

(ert-deftest erc-track-modified-channels/baseline/mention ()
  (erc-tests-common-track-modified-channels
   (lambda (set-faces)
     ;; Note: these messages don't have timestamps.

     ;; Simulate a JOIN, PART, etc. that's displayed in `erc-notice-face'.
     (funcall set-faces '(erc-notice-face))
     (erc-track-modified-channels)
     (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                    '(1 . erc-notice-face)))

     ;; Someone speaks, mentioning someone else, and the mode-line
     ;; changes to (erc-button-nick-default-face erc-nick-default-face)
     ;; rather than (erc-button-nick-default-face erc-default-face)
     ;; based on their rankings in `erc-track-faces-priority-list'.
     (funcall set-faces '((erc-button-nick-default-face erc-default-face)
                          (erc-button-nick-default-face erc-nick-default-face)
                          erc-default-face))
     (erc-track-modified-channels)
     (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                    '(2 erc-button-nick-default-face erc-nick-default-face)))

     ;; Someone else speaks, again with a mention and additional body text.
     (funcall set-faces '((erc-button-nick-default-face erc-default-face)
                          (erc-button-nick-default-face erc-nick-default-face)
                          erc-default-face))
     (erc-track-modified-channels)
     (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                    '(3 erc-button-nick-default-face erc-default-face)))

     ;; And yet again, which results in the indicator going back to one.
     (funcall set-faces '((erc-button-nick-default-face erc-default-face)
                          (erc-button-nick-default-face erc-nick-default-face)
                          erc-default-face))
     (erc-track-modified-channels)
     (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                    '(4 erc-button-nick-default-face erc-nick-default-face)))

     ;; Finally, another notice arrives.
     (funcall set-faces '(erc-notice-face))
     (erc-track-modified-channels)
     (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                    '(5 . erc-notice-face))))))

;; The compat-oriented option `erc-track-ignore-normal-contenders-p'
;; blinds track to `erc-track-faces-normal-list' for certain consecutive
;; messages with an identical face makeup.
(ert-deftest erc-track-modified-channels/baseline/ignore ()
  (let ((erc-track-ignore-normal-contenders-p t))
    (erc-tests-common-track-modified-channels
     (lambda (set-faces)
       ;; Simulate a JOIN, PART, etc. that's displayed in `erc-notice-face'.
       (funcall set-faces '(erc-notice-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(1 . erc-notice-face)))

       ;; Someone speaks, and the mode-line indicator's face changes to
       ;; that of a buttonized speaker.
       (funcall set-faces
                '(erc-timestamp-face
                  (erc-button-nick-default-face erc-nick-default-face)
                  erc-default-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(2 erc-button-nick-default-face erc-nick-default-face)))

       ;; The speaker speaks again immediately, and the segment doesn't
       ;; change.
       (funcall set-faces
                '(erc-timestamp-face
                  (erc-button-nick-default-face erc-nick-default-face)
                  erc-default-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(3 erc-button-nick-default-face erc-nick-default-face)))

       ;; Finally, another notice arrives.
       (funcall set-faces '(erc-notice-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(4 . erc-notice-face)))))))

;; Compat-oriented option `erc-track-ignore-normal-contenders-p'.
(ert-deftest erc-track-modified-channels/baseline/mention/ignore ()
  (let ((erc-track-ignore-normal-contenders-p t))
    (erc-tests-common-track-modified-channels
     (lambda (set-faces)

       ;; Simulate a JOIN, PART, etc. that's displayed in `erc-notice-face'.
       (funcall set-faces '(erc-notice-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(1 . erc-notice-face)))

       ;; Someone speaks, and the mode-line indicator's face changes to
       ;; that of a buttonized speaker.
       (funcall set-faces
                '((erc-button-nick-default-face erc-default-face)
                  (erc-button-nick-default-face erc-nick-default-face)
                  erc-default-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(2 erc-button-nick-default-face erc-nick-default-face)))

       ;; Someone else speaks, again with a mention and additional body
       ;; text, but the indicator stays the same.
       (funcall set-faces
                '((erc-button-nick-default-face erc-default-face)
                  (erc-button-nick-default-face erc-nick-default-face)
                  erc-default-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(3 erc-button-nick-default-face erc-nick-default-face)))

       ;; Finally, another notice arrives.
       (funcall set-faces '(erc-notice-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(4 . erc-notice-face)))))))

;; Option `erc-track-priority-faces-only' does not affect the behavior
;; of the baseline "normals" scenario because all faces appear in
;; `erc-track-faces-priority-list'.
(ert-deftest erc-track-modified-channels/priority-only-all/baseline ()
  (let ((erc-track-priority-faces-only 'all))
    (erc-tests-common-track-modified-channels
     #'erc-track-tests--modified-channels/baseline)))

;; This test simulates a common configuration that combines an
;; `erc-track-faces-priority-list' removed of `erc-notice-face' with
;; `erc-track-priority-faces-only' being `all'.  It also features in the
;; sample configuration in ERC's manual.
(ert-deftest erc-track-modified-channels/priority-only-all/sans-notice ()
  (let ((erc-track-priority-faces-only 'all)
        (erc-track-faces-priority-list
         (remq 'erc-notice-face erc-track-faces-priority-list)))

    (erc-tests-common-track-modified-channels
     (lambda (set-faces)
       ;; Note: these messages don't have timestamps.

       ;; Simulate a message normally displayed in `erc-notice-face',
       ;; which has been removed from `erc-track-faces-priority-list'.
       (funcall set-faces '(erc-notice-face))
       (erc-track-modified-channels)
       (should-not (alist-get (current-buffer) erc-modified-channels-alist))

       ;; Someone speaks, mentioning someone else, and the mode-line
       ;; changes to the buttonized speaker face rather than the
       ;; buttonized mention face, due to their respective ranks.
       (funcall set-faces
                '((erc-button-nick-default-face erc-default-face)
                  (erc-button-nick-default-face erc-nick-default-face)
                  erc-default-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(1 erc-button-nick-default-face erc-nick-default-face)))

       ;; Someone else speaks, again with a mention and additional body text.
       (funcall set-faces
                '((erc-button-nick-default-face erc-default-face)
                  (erc-button-nick-default-face erc-nick-default-face)
                  erc-default-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(2 erc-button-nick-default-face erc-default-face)))

       ;; And yet again, which results in the indicator going back to one.
       (funcall set-faces
                '((erc-button-nick-default-face erc-default-face)
                  (erc-button-nick-default-face erc-nick-default-face)
                  erc-default-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(3 erc-button-nick-default-face erc-nick-default-face)))

       ;; Finally, another notice arrives, which is ignored.
       (funcall set-faces '(erc-notice-face))
       (erc-track-modified-channels)
       (should (equal (alist-get (current-buffer) erc-modified-channels-alist)
                      '(3 erc-button-nick-default-face
                          erc-nick-default-face)))))))

;;; erc-track-tests.el ends here
