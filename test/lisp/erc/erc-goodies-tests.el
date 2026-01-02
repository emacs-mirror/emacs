;;; erc-goodies-tests.el --- Tests for erc-goodies  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
(require 'erc-goodies)

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-tests-common)))

(defun erc-goodies-tests--assert-face (beg end-str present &optional absent)
  (setq beg (+ beg (point-min)))
  (let ((end (+ beg (1- (length end-str)))))
    (ert-info ((format "beg: %S, end-str: %S" beg end-str))
      (while (and beg (< beg end))
        (let* ((val (get-text-property beg 'font-lock-face))
               (ft (flatten-tree (ensure-list val))))
          (ert-info ((format "looking-at: %S, val: %S"
                             (buffer-substring-no-properties beg end)
                             val))
            (dolist (p (ensure-list present))
              (if (consp p)
                  (should (member p val))
                (should (memq p ft))))
            (dolist (a (ensure-list absent))
              (if (consp a)
                  (should-not (member a val))
                (should-not (memq a ft)))))
          (setq beg (text-property-not-all beg (point-max)
                                           'font-lock-face val)))))))

;; These are from the "Examples" section of
;; https://modern.ircdocs.horse/formatting.html

(ert-deftest erc-controls-highlight--examples ()
  (should (eq t erc-interpret-controls-p))
  (let ((erc-insert-modify-hook '(erc-controls-highlight))
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
    (with-current-buffer (get-buffer-create "#chan")
      (erc-mode)
      (setq-local erc-interpret-mirc-color t)
      (erc--initialize-markers (point) nil)

      (let* ((m "I love \C-c3IRC!\C-c It is the \C-c7best protocol ever!")
             (msg (erc-format-privmessage "bob" m nil t)))
        (erc-display-message nil nil (current-buffer) msg))
      (forward-line -1)
      (should (search-forward "<bob> " nil t))
      (save-restriction
        (narrow-to-region (point) (pos-eol))
        (erc-goodies-tests--assert-face
         0 "I love" 'erc-default-face 'fg:erc-color-face3)
        (erc-goodies-tests--assert-face
         7 " IRC!" 'fg:erc-color-face3)
        (erc-goodies-tests--assert-face
         11 " It is the " 'erc-default-face 'fg:erc-color-face7)
        (erc-goodies-tests--assert-face
         22 "best protocol ever!" 'fg:erc-color-face7))

      (let* ((m "This is a \C-]\C-c13,9cool \C-cmessage")
             (msg (erc-format-privmessage "alice" m nil t)))
        (erc-display-message nil nil (current-buffer) msg))
      (should (search-forward "<alice> " nil t))
      (save-restriction
        (narrow-to-region (point) (pos-eol))
        (erc-goodies-tests--assert-face
         0 "this is a " 'erc-default-face 'erc-italic-face)
        (erc-goodies-tests--assert-face
         10 "cool " '(erc-italic-face fg:erc-color-face13 bg:erc-color-face9))
        (erc-goodies-tests--assert-face
         15 "message" 'erc-italic-face
         '(fg:erc-color-face13 bg:erc-color-face9)))

      (let* ((m "IRC \C-bis \C-c4,12so \C-cgreat\C-o!")
             (msg (erc-format-privmessage "bob" m nil t)))
        (erc-display-message nil nil (current-buffer) msg))
      (should (search-forward "<bob> " nil t))
      (save-restriction
        (narrow-to-region (point) (pos-eol))
        (erc-goodies-tests--assert-face
         0 "IRC " 'erc-default-face 'erc-bold-face)
        (erc-goodies-tests--assert-face
         4 "is " 'erc-bold-face '(fg:erc-color-face4 bg:erc-color-face12))
        (erc-goodies-tests--assert-face
         7 "so " '(erc-bold-face fg:erc-color-face4 bg:erc-color-face12))
        (erc-goodies-tests--assert-face
         10 "great" 'erc-bold-face '(fg:erc-color-face4 bg:erc-color-face12))
        (erc-goodies-tests--assert-face
         15 "!" 'erc-default-face 'erc-bold-face))

      (let* ((m (concat "Rules: Don't spam 5\C-c13,8,6\C-c,7,8, "
                        "and especially not \C-b9\C-b\C-]!"))
             (msg (erc-format-privmessage "alice" m nil t)))
        (erc-display-message nil nil (current-buffer) msg))
      (should (search-forward "<alice> " nil t))
      (save-restriction
        (narrow-to-region (point) (pos-eol))
        (erc-goodies-tests--assert-face
         0 "Rules: Don't spam 5" 'erc-default-face
         '(fg:erc-color-face13 bg:erc-color-face8))
        (erc-goodies-tests--assert-face
         19 ",6" '(fg:erc-color-face13 bg:erc-color-face8))
        (erc-goodies-tests--assert-face
         21 ",7,8, and especially not " 'erc-default-face
         '(fg:erc-color-face13 bg:erc-color-face8 erc-bold-face))
        (erc-goodies-tests--assert-face
         44 "9" 'erc-bold-face 'erc-italic-face)
        (erc-goodies-tests--assert-face
         45 "!" 'erc-italic-face 'erc-bold-face))

      (when noninteractive
        (kill-buffer)))))

;; Like the test above, this is most intuitive when run interactively.
;; Hovering over the redacted area should reveal its underlying text
;; in a high-contrast face.

(ert-deftest erc-controls-highlight--spoilers ()
  (should (eq t erc-interpret-controls-p))
  (erc-tests-common-make-server-buf)
  (with-current-buffer (erc--open-target "#chan")
    (setq-local erc-interpret-mirc-color t)
    (let* ((raw (concat "BEGIN "
                        "\C-c0,0 WhiteOnWhite "
                        "\C-c1,1 BlackOnBlack "
                        "\C-c99,99 Default "
                        "\C-o END"))
           (msg (erc-format-privmessage "bob" raw nil t)))
      (erc-display-message nil nil (current-buffer) msg))
    (forward-line -1)
    (should (search-forward "<bob> " nil t))
    (save-restriction
      ;; Narrow to EOL or start of right-side stamp.
      (narrow-to-region (point) (line-end-position))
      (save-excursion
        (search-forward "WhiteOn")
        (should (eq (get-text-property (point) 'mouse-face)
                    'erc-spoiler-face))
        (search-forward "BlackOn")
        (should (eq (get-text-property (point) 'mouse-face)
                    'erc-spoiler-face)))
      ;; Start with ERC default face.
      (erc-goodies-tests--assert-face
       0 "BEGIN " 'erc-default-face
       '(fg:erc-color-face0 bg:erc-color-face0))
      ;; Masked in all white.
      (erc-goodies-tests--assert-face
       6 "WhiteOnWhite" '(fg:erc-color-face0 bg:erc-color-face0)
       '(fg:erc-color-face1 bg:erc-color-face1))
      ;; Masked in all black.
      (erc-goodies-tests--assert-face
       20 "BlackOnBlack" '(fg:erc-color-face1 bg:erc-color-face1) nil)
      ;; Explicit "default" code ignored.
      (erc-goodies-tests--assert-face
       34 "Default" '(erc-default-face)
       '(fg:erc-color-face1 bg:erc-color-face1))
      (erc-goodies-tests--assert-face
       43 "END" 'erc-default-face nil)))
  (when noninteractive
    (erc-tests-common-kill-buffers)))

(ert-deftest erc-controls-highlight--inverse ()
  (should (eq t erc-interpret-controls-p))
  (erc-tests-common-make-server-buf)
  (with-current-buffer (erc--open-target "#chan")
    (setq-local erc-interpret-mirc-color t)
    (defvar erc-fill-column)
    (let* ((erc-fill-column 90)
           (raw (concat "BEGIN "
                        "\C-c3,13 GreenOnPink "
                        "\C-v PinkOnGreen "
                        "\C-c99,99 ReversedDefault "
                        "\C-v NormalDefault "
                        "\C-o END"))
           (msg (erc-format-privmessage "bob" raw nil t)))
      (erc-display-message nil nil (current-buffer) msg))
    (forward-line -1)
    (should (search-forward "<bob> " nil t))
    (save-restriction
      ;; Narrow to EOL or start of right-side stamp.
      (narrow-to-region (point) (line-end-position))
      ;; Baseline.
      (erc-goodies-tests--assert-face
       0 "BEGIN " 'erc-default-face
       '(fg:erc-color-face0 bg:erc-color-face0))
      ;; Normal fg/bg combo.
      (erc-goodies-tests--assert-face
       6 "GreenOnPink" '(fg:erc-color-face3 bg:erc-color-face13)
       '(erc-inverse-face))
      ;; Reverse of previous, so former-bg on former-fg.
      (erc-goodies-tests--assert-face
       19 "PinkOnGreen"
       '(erc-inverse-face fg:erc-color-face3 bg:erc-color-face13)
       nil)
      ;; The inverse of `default' because reverse still in effect.
      (erc-goodies-tests--assert-face
       32 "ReversedDefault" '(erc-inverse-face erc-default-face)
       '(fg:erc-color-face3 bg:erc-color-face13))
      (erc-goodies-tests--assert-face
       49 "NormalDefault" '(erc-default-face)
       '(erc-inverse-face fg:erc-color-face1 bg:erc-color-face1))
      (erc-goodies-tests--assert-face
       64 "END" 'erc-default-face
       '(fg:erc-color-face0 bg:erc-color-face0))))
  (when noninteractive
    (erc-tests-common-kill-buffers)))

;; This is meant to assert two behavioral properties:
;;
;; 1) The background is preserved when only a new foreground is
;;    defined, in accordance with this bit from the spec: "If only the
;;    foreground color is set, the background color stays the same."
;;    https://modern.ircdocs.horse/formatting#color
;;
;; 2) The same holds true for a new, lone foreground of 99.  Rather
;;    than prepend `erc-default-face', this causes the removal of an
;;    existing foreground face and likewise doesn't clobber the
;;    existing background.
(ert-deftest erc-controls-highlight/default-foreground ()
  (should (eq t erc-interpret-controls-p))
  (erc-tests-common-make-server-buf)
  (with-current-buffer (erc--open-target "#chan")
    (setq-local erc-interpret-mirc-color t)
    (defvar erc-fill-column)
    (let ((erc-fill-column 90))
      (erc-display-message nil nil (current-buffer)
                           (erc-format-privmessage
                            "bob" (concat "BEGIN "
                                          "\C-c03,08 GreenOnYellow "
                                          "\C-c99 BlackOnYellow "
                                          "\C-o END")
                            nil t)))
    (forward-line -1)
    (should (search-forward "<bob> " nil t))
    (should (erc-tests-common-equal-with-props
             (erc--remove-text-properties
              (buffer-substring (point) (line-end-position)))
             #("BEGIN  GreenOnYellow  BlackOnYellow  END"
               0 6 (font-lock-face erc-default-face)
               6 21 (font-lock-face (fg:erc-color-face3
                                     bg:erc-color-face8
                                     erc-default-face))
               21 36 (font-lock-face (bg:erc-color-face8
                                      erc-default-face))
               36 40 (font-lock-face (erc-default-face)))))
    (should (search-forward "BlackOnYellow"))
    (let ((faces (get-text-property (point) 'font-lock-face)))
      (should (equal (face-background (car faces) nil (cdr faces))
                     "yellow")))

    ;; Redefine background color alongside default foreground.
    (let ((erc-fill-column 90))
      (erc-display-message nil nil (current-buffer)
                           (erc-format-privmessage
                            "bob" (concat "BEGIN "
                                          "\C-c03,08 GreenOnYellow "
                                          "\C-c99,07 BlackOnOrange "
                                          "\C-o END")
                            nil t)))
    (should (search-forward "<bob> " nil t))
    (should (erc-tests-common-equal-with-props
             (erc--remove-text-properties
              (buffer-substring (point) (line-end-position)))
             #("BEGIN  GreenOnYellow  BlackOnOrange  END"
               0 6 (font-lock-face erc-default-face)
               6 21 (font-lock-face (fg:erc-color-face3
                                     bg:erc-color-face8
                                     erc-default-face))
               21 36 (font-lock-face (bg:erc-color-face7
                                      erc-default-face))
               36 40 (font-lock-face (erc-default-face)))))
    (should (search-forward "BlackOnOrange"))
    (let ((faces (get-text-property (point) 'font-lock-face)))
      (should (equal (face-background (car faces) nil (cdr faces))
                     "orange")))) ; as opposed to white or black
  (when noninteractive
    (erc-tests-common-kill-buffers)))

;; This merely asserts our current interpretation of "default faces":
;; that they reflect the foreground and background exhibited by normal
;; chat messages before any control-code formatting is applied (rather
;; than, e.g., some sort of negation or no-op).
(ert-deftest erc-controls-highlight/default-background ()
  (should (eq t erc-interpret-controls-p))
  (erc-tests-common-make-server-buf)
  (with-current-buffer (erc--open-target "#chan")
    (setq-local erc-interpret-mirc-color t)
    (defvar erc-fill-column)
    (let ((erc-fill-column 90))
      (erc-display-message nil nil (current-buffer)
                           (erc-format-privmessage
                            "bob" (concat "BEGIN "
                                          "\C-c03,08 GreenOnYellow "
                                          "\C-c05,99 BrownOnWhite "
                                          "\C-o END")
                            nil t)))
    (forward-line -1)
    (should (search-forward "<bob> " nil t))
    (should (erc-tests-common-equal-with-props
             (erc--remove-text-properties
              (buffer-substring (point) (line-end-position)))
             #("BEGIN  GreenOnYellow  BrownOnWhite  END"
               0 6 (font-lock-face erc-default-face)
               6 21 (font-lock-face (fg:erc-color-face3
                                     bg:erc-color-face8
                                     erc-default-face))
               21 35 (font-lock-face (fg:erc-color-face5
                                      erc-default-face))
               35 39 (font-lock-face (erc-default-face)))))
    ;; Ensure the background is white or black, rather than yellow.
    (should (search-forward "BrownOnWhite"))
    (let ((faces (get-text-property (point) 'font-lock-face)))
      (should (equal (face-background (car faces) nil `(,@(cdr faces) default))
                     (face-background 'default)))))
  (when noninteractive
    (erc-tests-common-kill-buffers)))

(defvar erc-goodies-tests--motd
  ;; This is from ergo's MOTD
  '((":- - this is \2bold text\17.")
    (":- - this is \35italics text\17.")
    (":- - this is \0034red\3 and \0032blue\3 text.")
    (":- - this is \0034,12red text with a light blue background\3.")
    (":- - this is a normal escaped dollarsign: $")
    (":- ")
    (":- "
     "\0031,0 00 \0030,1 01 \0030,2 02 \0030,3 03 "
     "\0031,4 04 \0030,5 05 \0030,6 06 \0031,7 07 ")
    (":- "
     "\0031,8 08 \0031,9 09 \0030,10 10 \0031,11 11 "
     "\0030,12 12 \0031,13 13 \0031,14 14 \0031,15 15 ")
    (":- ")
    (":- "
     "\0030,16 16 \0030,17 17 \0030,18 18 \0030,19 19 "
     "\0030,20 20 \0030,21 21 \0030,22 22 \0030,23 23 "
     "\0030,24 24 \0030,25 25 \0030,26 26 \0030,27 27 ")
    (":- "
     "\0030,28 28 \0030,29 29 \0030,30 30 \0030,31 31 "
     "\0030,32 32 \0030,33 33 \0030,34 34 \0030,35 35 "
     "\0030,36 36 \0030,37 37 \0030,38 38 \0030,39 39 ")
    (":- "
     "\0030,40 40 \0030,41 41 \0030,42 42 \0030,43 43 "
     "\0030,44 44 \0030,45 45 \0030,46 46 \0030,47 47 "
     "\0030,48 48 \0030,49 49 \0030,50 50 \0030,51 51 ")
    (":- "
     "\0030,52 52 \0030,53 53 \0031,54 54 \0031,55 55 "
     "\0031,56 56 \0031,57 57 \0031,58 58 \0030,59 59 "
     "\0030,60 60 \0030,61 61 \0030,62 62 \0030,63 63 ")
    (":- "
     "\0030,64 64 \0031,65 65 \0031,66 66 \0031,67 67 "
     "\0031,68 68 \0031,69 69 \0031,70 70 \0031,71 71 "
     "\0030,72 72 \0030,73 73 \0030,74 74 \0030,75 75 ")
    (":- "
     "\0031,76 76 \0031,77 77 \0031,78 78 \0031,79 79 "
     "\0031,80 80 \0031,81 81 \0031,82 82 \0031,83 83 "
     "\0031,84 84 \0031,85 85 \0031,86 86 \0031,87 87 ")
    (":- "
     "\0030,88 88 \0030,89 89 \0030,90 90 \0030,91 91 "
     "\0030,92 92 \0030,93 93 \0030,94 94 \0030,95 95 "
     "\0031,96 96 \0031,97 97 \0031,98 98 \399,99 99 ")
    (":- ")))

(ert-deftest erc-controls-highlight--motd ()
  (should (eq t erc-interpret-controls-p))
  (let ((erc-insert-modify-hook '(erc-controls-highlight))
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
    (with-current-buffer (get-buffer-create "#chan")
      (erc-mode)
      (setq-local erc-interpret-mirc-color t)
      (erc--initialize-markers (point) nil)

      (dolist (parts erc-goodies-tests--motd)
        (erc-display-message nil 'notice (current-buffer) (string-join parts)))

      ;; Spot check
      (goto-char (point-min))
      (should (search-forward " 16 " nil t))
      (save-restriction
        (narrow-to-region (point) (pos-eol))
        (erc-goodies-tests--assert-face
         0 " 17 " '(fg:erc-color-face0 (:background "#472100")))
        (erc-goodies-tests--assert-face
         4 " 18 " '(fg:erc-color-face0 (:background "#474700"))
         '((:background "#472100"))))

      (should (search-forward " 71 " nil t))
      (save-restriction
        (narrow-to-region (point) (pos-eol))
        (erc-goodies-tests--assert-face
         0 " 72 " '(fg:erc-color-face0 (:background "#5959ff")))
        (erc-goodies-tests--assert-face
         4 " 73 " '(fg:erc-color-face0 (:background "#c459ff"))
         '((:background "#5959ff"))))

      (goto-char (point-min))
      (when noninteractive
        (kill-buffer)))))


;; Among other things, this test also asserts that a local module's
;; minor-mode toggle is allowed to disable its mode variable as
;; needed.

(defun erc-goodies-tests--assert-kp-indicator-on ()
  (should erc--keep-place-indicator-overlay)
  (should (memq 'erc--keep-place-indicator-on-window-buffer-change
                window-buffer-change-functions))
  (should (memq 'erc-keep-place erc-insert-pre-hook))
  (should (eq erc-keep-place-mode
              (not (local-variable-p 'erc-insert-pre-hook)))))

(defun erc-goodies-tests--assert-kp-indicator-off ()
  (should-not (local-variable-p 'erc-insert-pre-hook))
  (should-not (memq 'erc--keep-place-indicator-on-window-buffer-change
                    window-buffer-change-functions))
  (should-not erc--keep-place-indicator-overlay))

(defun erc-goodies-tests--kp-indicator-populate ()
  (erc-display-message nil 'notice (current-buffer)
                       "This buffer is for text that is not saved")
  (erc-display-message nil 'notice (current-buffer)
                       "and for lisp evaluation")
  (should (search-forward "saved" nil t))
  (erc-keep-place-move nil)
  (goto-char erc-input-marker))

(defun erc-goodies-tests--keep-place-indicator (test)
  (erc-keep-place-mode -1)
  (with-current-buffer (erc-tests-common-make-server-buf
                        "*erc-keep-place-indicator-mode*")
    (let (erc-connect-pre-hook
          erc-modules)

      (ert-info ("Clean slate")
        (erc-goodies-tests--assert-kp-indicator-off)
        (should-not erc-keep-place-mode)
        (should-not (memq 'keep-place erc-modules)))

      (funcall test))

    (when noninteractive
      (erc-keep-place-indicator-mode -1)
      (erc-keep-place-mode -1)
      (should-not (member 'erc-keep-place
                          (default-value 'erc-insert-pre-hook)))
      (should-not (local-variable-p 'erc-insert-pre-hook))
      (erc-tests-common-kill-buffers))))

(ert-deftest erc-keep-place-indicator-mode--no-global ()
  (erc-goodies-tests--keep-place-indicator
   (lambda ()

     (ert-info ("Value t")
       (should (eq erc-keep-place-indicator-buffer-type t))
       (erc-keep-place-indicator-mode +1)
       (erc-goodies-tests--assert-kp-indicator-on)
       (goto-char (point-min)))

     (erc-keep-place-indicator-mode -1)
     (erc-goodies-tests--assert-kp-indicator-off)

     (ert-info ("Value `target'")
       (let ((erc-keep-place-indicator-buffer-type 'target))
         ;; No-op because server buffer.
         (erc-keep-place-indicator-mode +1)
         (erc-goodies-tests--assert-kp-indicator-off)
         ;; Spoof target buffer (no longer no-op).
         (setq erc--target (erc--target-from-string "#chan"))
         (erc-keep-place-indicator-mode +1)
         (erc-goodies-tests--assert-kp-indicator-on)))

     (erc-keep-place-indicator-mode -1)
     (erc-goodies-tests--assert-kp-indicator-off)

     (ert-info ("Value `server'")
       (let ((erc-keep-place-indicator-buffer-type 'server))
         (erc-keep-place-indicator-mode +1)
         (erc-goodies-tests--assert-kp-indicator-off)
         (setq erc--target nil)
         (erc-keep-place-indicator-mode +1)
         (erc-goodies-tests--assert-kp-indicator-on)))

     ;; Populate buffer
     (erc-goodies-tests--kp-indicator-populate)

     (ert-info ("Indicator survives reconnect")
       (let ((erc--server-reconnecting (buffer-local-variables)))
         (cl-letf (((symbol-function 'erc-server-connect) #'ignore))
           (erc-open "localhost" 6667 "tester" "Tester" 'connect
                     nil nil nil nil nil "tester" nil)))
       (erc-goodies-tests--assert-kp-indicator-on)
       (should (= (point) erc-input-marker))
       (goto-char (overlay-start erc--keep-place-indicator-overlay))
       (should (looking-at (rx "*** This buffer is for text")))))))

(ert-deftest erc-keep-place-indicator-mode--global ()
  (erc-goodies-tests--keep-place-indicator
   (lambda ()

     (push 'keep-place erc-modules)

     (ert-info ("Value t")
       (should (eq erc-keep-place-indicator-buffer-type t))
       (erc-keep-place-indicator-mode +1)
       (erc-goodies-tests--assert-kp-indicator-on)
       ;; Local module activates global `keep-place'.
       (should erc-keep-place-mode)
       ;; Does not register local version of hook (otherwise would run
       ;; twice).
       (should-not (local-variable-p 'erc-insert-pre-hook))
       (goto-char (point-min)))

     (erc-keep-place-indicator-mode -1)
     (erc-goodies-tests--assert-kp-indicator-off)
     (should erc-keep-place-mode)
     (should (member 'erc-keep-place erc-insert-pre-hook))

     (ert-info ("Value `target'")
       (let ((erc-keep-place-indicator-buffer-type 'target))
         ;; No-op because server buffer.
         (erc-keep-place-indicator-mode +1)
         (erc-goodies-tests--assert-kp-indicator-off)
         ;; Does not interfere with global activation state.
         (should erc-keep-place-mode)
         (should (member 'erc-keep-place erc-insert-pre-hook))
         ;; Morph into a target buffer (no longer no-op).
         (setq erc--target (erc--target-from-string "#chan"))
         (erc-keep-place-indicator-mode +1)
         (erc-goodies-tests--assert-kp-indicator-on)
         ;; Does not register local version of hook.
         (should-not (local-variable-p 'erc-insert-pre-hook))))

     (erc-keep-place-indicator-mode -1)
     (erc-goodies-tests--assert-kp-indicator-off)
     (should erc-keep-place-mode)
     (should (member 'erc-keep-place erc-insert-pre-hook))

     (ert-info ("Value `server'")
       (let ((erc-keep-place-indicator-buffer-type 'server))
         ;; No-op because we're now a target buffer.
         (erc-keep-place-indicator-mode +1)
         (erc-goodies-tests--assert-kp-indicator-off)
         (should erc-keep-place-mode)
         (should (member 'erc-keep-place erc-insert-pre-hook))
         ;; Back to server.
         (setq erc--target nil)
         (erc-keep-place-indicator-mode +1)
         (erc-goodies-tests--assert-kp-indicator-on)
         (should-not (local-variable-p 'erc-insert-pre-hook))))

     (ert-info ("Local adapts to global toggle")
       (erc-keep-place-mode -1)
       (should-not (member 'erc-keep-place
                           (default-value 'erc-insert-pre-hook)))
       (should (member 'erc-keep-place erc-insert-pre-hook))
       (erc-goodies-tests--assert-kp-indicator-on)
       (erc-keep-place-mode +1)
       (should (member 'erc-keep-place (default-value 'erc-insert-pre-hook)))
       (should-not (local-variable-p 'erc-insert-pre-hook))
       (erc-goodies-tests--assert-kp-indicator-on))

     ;; Populate buffer
     (erc-goodies-tests--kp-indicator-populate)

     (ert-info ("Indicator survives reconnect")
       (let ((erc--server-reconnecting (buffer-local-variables)))
         (cl-letf (((symbol-function 'erc-server-connect) #'ignore))
           (erc-open "localhost" 6667 "tester" "Tester" 'connect
                     nil nil nil nil nil "tester" nil)))
       (erc-goodies-tests--assert-kp-indicator-on)
       (should erc-keep-place-mode)
       (should (member 'erc-keep-place erc-insert-pre-hook))
       (should (= (point) erc-input-marker))
       (goto-char (overlay-start erc--keep-place-indicator-overlay))
       (should (looking-at (rx "*** This buffer is for text")))))))

(ert-deftest erc--get-inserted-msg-beg/readonly ()
  (erc-tests-common-assert-get-inserted-msg-readonly-with
   #'erc-tests-common-assert-get-inserted-msg/basic
   (lambda (arg) (should (= 3 (erc--get-inserted-msg-beg arg))))))

(ert-deftest erc--get-inserted-msg-beg/truncated/readonly ()
  (erc-tests-common-assert-get-inserted-msg-readonly-with
   #'erc-tests-common-assert-get-inserted-msg/truncated
   (lambda (arg) (should (= 1 (erc--get-inserted-msg-beg arg))))))

(ert-deftest erc--get-inserted-msg-end/readonly ()
  (erc-tests-common-assert-get-inserted-msg-readonly-with
   #'erc-tests-common-assert-get-inserted-msg/basic
   (lambda (arg) (should (= 11 (erc--get-inserted-msg-end arg))))))

(ert-deftest erc--get-inserted-msg-bounds/readonly ()
  (erc-tests-common-assert-get-inserted-msg-readonly-with
   #'erc-tests-common-assert-get-inserted-msg/basic
   (lambda (arg)
     (should (equal '(3 . 11) (erc--get-inserted-msg-bounds arg))))))


;;; erc-goodies-tests.el ends here
