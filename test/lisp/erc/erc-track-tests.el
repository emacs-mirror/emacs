;;; erc-track-tests.el --- Tests for erc-track.  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2023 Free Software Foundation, Inc.

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

(require 'ert)
(require 'erc-track)

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
  (pcase-dolist (`(,want ,cur-face ,new-faces) cases)

    (ert-info ((format "Observed: {cur: %S, new: %S, want: %S}"
                       cur-face new-faces want))
      (setq new-faces (cons (map-into
                             (mapcar (lambda (f) (cons f t)) new-faces)
                             '(hash-table :test equal))
                            (reverse new-faces)))
      (should (equal want (funcall #'erc-track--select-mode-line-face
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

;;; erc-track-tests.el ends here
