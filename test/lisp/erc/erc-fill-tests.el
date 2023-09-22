;;; erc-fill-tests.el --- Tests for erc-fill  -*- lexical-binding:t -*-

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

;; FIXME these tests are brittle and error prone.  Replace with
;; scenarios.

;;; Code:
(require 'ert-x)
(require 'erc-fill)

(defvar erc-fill-tests--buffers nil)
(defvar erc-fill-tests--time-vals (lambda () 0))

(defun erc-fill-tests--insert-privmsg (speaker &rest msg-parts)
  (declare (indent 1))
  (let* ((msg (erc-format-privmessage speaker
                                      (apply #'concat msg-parts) nil t))
         ;; (erc--msg-prop-overrides '((erc-msg . msg) (erc-cmd . PRIVMSG)))
         (parsed (make-erc-response :unparsed msg :sender speaker
                                    :command "PRIVMSG"
                                    :command-args (list "#chan" msg)
                                    :contents msg)))
    (erc-display-message parsed nil (current-buffer) msg)))

(defun erc-fill-tests--wrap-populate (test)
  (let ((original-window-buffer (window-buffer (selected-window)))
        (erc-stamp--tz t)
        (erc-fill-function 'erc-fill-wrap)
        (pre-command-hook pre-command-hook)
        (inhibit-message noninteractive)
        erc-insert-post-hook
        extended-command-history
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
    (cl-letf (((symbol-function 'erc-stamp--current-time)
               (lambda () (funcall erc-fill-tests--time-vals)))
              ((symbol-function 'erc-server-connect)
               (lambda (&rest _)
                 (setq erc-server-process
                       (start-process "sleep" (current-buffer) "sleep" "1"))
                 (set-process-query-on-exit-flag erc-server-process nil))))
      (with-current-buffer
          (car (push (erc-open "localhost" 6667 "tester" "Tester" 'connect
                               nil nil nil nil nil "tester" 'foonet)
                     erc-fill-tests--buffers))
        (setq erc-network 'foonet
              erc-server-connected t)
        (with-current-buffer (erc--open-target "#chan")
          (set-window-buffer (selected-window) (current-buffer))

          (erc-update-channel-member
           "#chan" "alice" "alice" t nil nil nil nil nil "fake" "~u" nil nil t)

          (erc-update-channel-member
           "#chan" "bob" "bob" t nil nil nil nil nil "fake" "~u" nil nil t)

          (erc-display-message
           nil 'notice (current-buffer)
           (concat "This server is in debug mode and is logging all user I/O. "
                   "If you do not wish for everything you send to be readable "
                   "by the server owner(s), please disconnect."))

          (erc-fill-tests--insert-privmsg "alice"
            "bob: come, you are a tedious fool: to the purpose. "
            "What was done to Elbow's wife, that he hath cause to complain of?"
            " Come me to what was done to her.")

          ;; Introduce an artificial gap in properties `line-prefix' and
          ;; `wrap-prefix' and later ensure they're not incremented twice.
          (save-excursion
            (forward-line -1)
            (search-forward "? ")
            (with-silent-modifications
              (remove-text-properties (1- (point)) (point)
                                      '(line-prefix t wrap-prefix t))))

          (erc-fill-tests--insert-privmsg "bob"
            "alice: Either your unparagoned mistress is dead, "
            "or she's outprized by a trifle.")

          ;; Defend against non-local exits from `ert-skip'
          (unwind-protect
              (funcall test)
            (when set-transient-map-timer
              (timer-event-handler set-transient-map-timer))
            (set-window-buffer (selected-window) original-window-buffer)
            (when noninteractive
              (while-let ((buf (pop erc-fill-tests--buffers)))
                (kill-buffer buf))
              (kill-buffer))))))))

(defun erc-fill-tests--wrap-check-prefixes (&rest prefixes)
  ;; Check that prefix props are applied over correct intervals.
  (save-excursion
    (goto-char (point-min))
    (dolist (prefix prefixes)
      (should (search-forward prefix nil t))
      (should (get-text-property (pos-bol) 'line-prefix))
      (should (get-text-property (1- (pos-eol)) 'line-prefix))
      (should-not (get-text-property (pos-eol) 'line-prefix))
      ;; Spans entire line uninterrupted.
      (let* ((val (get-text-property (pos-bol) 'line-prefix))
             (end (text-property-not-all (pos-bol) (point-max)
                                         'line-prefix val)))
        (when (and (/= end (pos-eol)) (= ?? (char-before end)))
          (setq end (text-property-not-all (1+ end) (point-max)
                                           'line-prefix val)))
        (should (eq end (pos-eol))))
      (should (equal (get-text-property (pos-bol) 'wrap-prefix)
                     '(space :width erc-fill--wrap-value)))
      (should-not (get-text-property (pos-eol) 'wrap-prefix))
      (should (equal (get-text-property (1- (pos-eol)) 'wrap-prefix)
                     '(space :width erc-fill--wrap-value))))))

;; Set this variable to t to generate new snapshots after carefully
;; reviewing the output of *each* snapshot (not just first and last).
;; Obviously, only run one test at a time.
(defvar erc-fill-tests--save-p nil)

;; On graphical displays, echo .graphic >> .git/info/exclude
(defvar erc-fill-tests--graphic-dir "fill/snapshots/.graphic")

(defun erc-fill-tests--compare (name)
  (let* ((dir (expand-file-name (if (display-graphic-p)
                                    erc-fill-tests--graphic-dir
                                  "fill/snapshots/")
                                (ert-resource-directory)))
         (expect-file (file-name-with-extension (expand-file-name name dir)
                                                "eld"))
         (erc--own-property-names
          (seq-difference `(font-lock-face ,@erc--own-property-names)
                          '(field display wrap-prefix line-prefix)
                          #'eq))
         (print-circle t)
         (print-escape-newlines t)
         (print-escape-nonascii t)
         (got (erc--remove-text-properties
               (buffer-substring (point-min) erc-insert-marker)))
         (repr (string-replace "erc-fill--wrap-value"
                               (number-to-string erc-fill--wrap-value)
                               (prin1-to-string got))))
    (with-current-buffer (generate-new-buffer name)
      (push (current-buffer) erc-fill-tests--buffers)
      (with-silent-modifications
        (insert (setq got (read repr))))
      (erc-mode))
    (if erc-fill-tests--save-p
        (with-temp-file expect-file
          (insert repr))
      (if (file-exists-p expect-file)
          ;; Ensure string-valued properties, like timestamps, aren't
          ;; recursive (signals `max-lisp-eval-depth' exceeded).
          (named-let assert-equal
              ((latest (read repr))
               (expect (read (with-temp-buffer
                               (insert-file-contents-literally expect-file)
                               (buffer-string)))))
            (pcase latest
              ((or "" 'nil) t)
              ((pred stringp)
               (should (equal-including-properties latest expect))
               (let ((latest-intervals (object-intervals latest))
                     (expect-intervals (object-intervals expect)))
                 (while-let ((l-iv (pop latest-intervals))
                             (x-iv (pop expect-intervals))
                             (l-tab (map-into (nth 2 l-iv) 'hash-table))
                             (x-tab (map-into (nth 2 x-iv) 'hash-table)))
                   (pcase-dolist (`(,l-k . ,l-v) (map-pairs l-tab))
                     (assert-equal l-v (gethash l-k x-tab))
                     (remhash l-k x-tab))
                   (should (zerop (hash-table-count x-tab))))))
              ((pred sequencep)
               (assert-equal (seq-first latest) (seq-first expect))
               (assert-equal (seq-rest latest) (seq-rest expect)))
              (_ (should (equal latest expect)))))
        (message "Snapshot file missing: %S" expect-file)))))

;; To inspect variable pitch, set `erc-mode-hook' to
;;
;;   (lambda () (face-remap-add-relative 'default :family "Sans Serif"))
;;
;; or similar.

(ert-deftest erc-fill-wrap--monospace ()
  :tags '(:unstable)
  (unless (>= emacs-major-version 29)
    (ert-skip "Emacs version too low, missing `buffer-text-pixel-size'"))

  (erc-fill-tests--wrap-populate

   (lambda ()
     (should (= erc-fill--wrap-value 27))
     (erc-fill-tests--wrap-check-prefixes "*** " "<alice> " "<bob> ")
     (erc-fill-tests--compare "monospace-01-start")

     (ert-info ("Shift right by one (plus)")
       ;; Args are all `erc-fill-wrap-nudge' +1 because interactive "p"
       (ert-with-message-capture messages
         ;; M-x erc-fill-wrap-nudge RET =
         (ert-simulate-command '(erc-fill-wrap-nudge 2))
         (should (string-match (rx "for further adjustment") messages)))
       (should (= erc-fill--wrap-value 29))
       (erc-fill-tests--wrap-check-prefixes "*** " "<alice> " "<bob> ")
       (erc-fill-tests--compare "monospace-02-right"))

     (ert-info ("Shift left by five")
       ;; "M-x erc-fill-wrap-nudge RET -----"
       (ert-simulate-command '(erc-fill-wrap-nudge -4))
       (should (= erc-fill--wrap-value 25))
       (erc-fill-tests--wrap-check-prefixes "*** " "<alice> " "<bob> ")
       (erc-fill-tests--compare "monospace-03-left"))

     (ert-info ("Reset")
       ;; M-x erc-fill-wrap-nudge RET 0
       (ert-simulate-command '(erc-fill-wrap-nudge 0))
       (should (= erc-fill--wrap-value 27))
       (erc-fill-tests--wrap-check-prefixes "*** " "<alice> " "<bob> ")
       (erc-fill-tests--compare "monospace-04-reset")))))

(ert-deftest erc-fill-wrap--merge ()
  :tags '(:unstable)
  (unless (>= emacs-major-version 29)
    (ert-skip "Emacs version too low, missing `buffer-text-pixel-size'"))

  (erc-fill-tests--wrap-populate

   (lambda ()
     (erc-update-channel-member
      "#chan" "Dummy" "Dummy" t nil nil nil nil nil "fake" "~u" nil nil t)

     ;; Set this here so that the first few messages are from 1970
     (let ((erc-fill-tests--time-vals (lambda () 1680332400)))
       (erc-fill-tests--insert-privmsg "bob" "zero.")
       (erc-fill-tests--insert-privmsg "alice" "one.")
       (erc-fill-tests--insert-privmsg "alice" "two.")
       (erc-fill-tests--insert-privmsg "bob" "three.")
       (erc-fill-tests--insert-privmsg "bob" "four.")
       (erc-fill-tests--insert-privmsg "Dummy" "five.")
       (erc-fill-tests--insert-privmsg "Dummy" "six."))

     (should (= erc-fill--wrap-value 27))
     (erc-fill-tests--wrap-check-prefixes
      "*** " "<alice> " "<bob> "
      "<bob> " "<alice> " "<alice> " "<bob> " "<bob> " "<Dummy> " "<Dummy> ")
     (erc-fill-tests--compare "merge-01-start")

     (ert-info ("Shift right by one (plus)")
       (ert-simulate-command '(erc-fill-wrap-nudge 2))
       (should (= erc-fill--wrap-value 29))
       (erc-fill-tests--wrap-check-prefixes
        "*** " "<alice> " "<bob> "
        "<bob> " "<alice> " "<alice> " "<bob> " "<bob> " "<Dummy> " "<Dummy> ")
       (erc-fill-tests--compare "merge-02-right")))))

(ert-deftest erc-fill-wrap--merge-action ()
  :tags '(:unstable)
  (unless (>= emacs-major-version 29)
    (ert-skip "Emacs version too low, missing `buffer-text-pixel-size'"))

  (erc-fill-tests--wrap-populate

   (lambda ()
     ;; Set this here so that the first few messages are from 1970
     (let ((erc-fill-tests--time-vals (lambda () 1680332400)))
       (erc-fill-tests--insert-privmsg "bob" "zero.")

       (erc-process-ctcp-query
        erc-server-process
        (make-erc-response
         :unparsed ":bob!~u@fake PRIVMSG #chan :\1ACTION one\1"
         :sender "bob!~u@fake" :command "PRIVMSG"
         :command-args '("#chan" "\1ACTION one\1") :contents "\1ACTION one\1")
        "bob" "~u" "fake")

       (erc-fill-tests--insert-privmsg "bob" "two.")

       ;; Compat switch to opt out of overhanging speaker.
       (let (erc-fill--wrap-action-dedent-p)
         (erc-process-ctcp-query
          erc-server-process
          (make-erc-response
           :unparsed ":bob!~u@fake PRIVMSG #chan :\1ACTION three\1"
           :sender "bob!~u@fake" :command "PRIVMSG"
           :command-args '("#chan" "\1ACTION three\1")
           :contents "\1ACTION three\1")
          "bob" "~u" "fake"))

       (erc-fill-tests--insert-privmsg "bob" "four."))

     (should (= erc-fill--wrap-value 27))
     (erc-fill-tests--wrap-check-prefixes
      "*** " "<alice> " "<bob> " "<bob> " "* bob " "<bob> " "* " "<bob> ")
     (erc-fill-tests--compare "merge-wrap-01"))))

(ert-deftest erc-fill-line-spacing ()
  :tags '(:unstable)
  (unless (>= emacs-major-version 29)
    (ert-skip "Emacs version too low, missing `buffer-text-pixel-size'"))

  (let ((erc-fill-line-spacing 0.5))
    (erc-fill-tests--wrap-populate
     (lambda ()
       (erc-fill-tests--insert-privmsg "bob" "This buffer is for text.")
       (erc-display-message nil 'notice (current-buffer) "one two three")
       (erc-display-message nil 'notice (current-buffer) "four five six")
       (erc-fill-tests--insert-privmsg "bob" "Somebody stop me")
       (erc-fill-tests--compare "spacing-01-mono")))))

(ert-deftest erc-fill-wrap-visual-keys--body ()
  :tags '(:unstable)
  (erc-fill-tests--wrap-populate

   (lambda ()
     (ert-info ("Value: non-input")
       (should (eq erc-fill--wrap-visual-keys 'non-input))
       (goto-char (point-min))
       (should (search-forward "that he hath" nil t))
       (execute-kbd-macro "\C-a")
       (should-not (looking-at (rx "<alice> ")))
       (execute-kbd-macro "\C-e")
       (should (search-backward "tedious fool" nil t))
       (should-not (looking-back "done to her\\."))
       (forward-char)
       (execute-kbd-macro "\C-e")
       (should (search-forward "done to her." nil t)))

     (ert-info ("Value: nil")
       (execute-kbd-macro "\C-ca")
       (should-not erc-fill--wrap-visual-keys)
       (goto-char (point-min))
       (should (search-forward "in debug mode" nil t))
       (execute-kbd-macro "\C-a")
       (should (looking-at (rx "*** ")))
       (execute-kbd-macro "\C-e")
       (should (eql ?\] (char-before (point)))))

     (ert-info ("Value: t")
       (execute-kbd-macro "\C-ca")
       (should (eq erc-fill--wrap-visual-keys t))
       (goto-char (point-min))
       (should (search-forward "that he hath" nil t))
       (execute-kbd-macro "\C-a")
       (should-not (looking-at (rx "<alice> ")))
       (should (search-backward "tedious fool" nil t))
       (execute-kbd-macro "\C-e")
       (should-not (looking-back (rx "done to her\\.")))
       (should (search-forward "done to her." nil t))
       (execute-kbd-macro "\C-a")
       (should-not (looking-at (rx "<alice> ")))))))

(ert-deftest erc-fill-wrap-visual-keys--prompt ()
  :tags '(:unstable)
  (erc-fill-tests--wrap-populate

   (lambda ()
     (set-window-buffer (selected-window) (current-buffer))
     (goto-char erc-input-marker)
     (insert "This buffer is for text that is not saved, and for Lisp "
             "evaluation.  To create a file, visit it with C-x C-f and "
             "enter text in its buffer.")

     (ert-info ("Value: non-input")
       (should (eq erc-fill--wrap-visual-keys 'non-input))
       (execute-kbd-macro "\C-a")
       (should (looking-at "This buffer"))
       (execute-kbd-macro "\C-e")
       (should (looking-back "its buffer\\."))
       (execute-kbd-macro "\C-a")
       (execute-kbd-macro "\C-k")
       (should (eobp)))

     (ert-info ("Value: nil") ; same
       (execute-kbd-macro "\C-ca")
       (should-not erc-fill--wrap-visual-keys)
       (execute-kbd-macro "\C-y")
       (should (looking-back "its buffer\\."))
       (execute-kbd-macro "\C-a")
       (should (looking-at "This buffer"))
       (execute-kbd-macro "\C-k")
       (should (eobp)))

     (ert-info ("Value: non-input")
       (execute-kbd-macro "\C-ca")
       (should (eq erc-fill--wrap-visual-keys t))
       (execute-kbd-macro "\C-y")
       (execute-kbd-macro "\C-a")
       (should-not (looking-at "This buffer"))
       (execute-kbd-macro "\C-p")
       (should-not (looking-back "its buffer\\."))
       (should (search-forward "its buffer." nil t))
       (should (search-backward "ERC> " nil t))
       (execute-kbd-macro "\C-a")))))

(ert-deftest erc-fill--left-hand-stamps ()
  :tags '(:unstable)
  (unless (>= emacs-major-version 29)
    (ert-skip "Emacs version too low, missing `buffer-text-pixel-size'"))

  (let ((erc-timestamp-only-if-changed-flag nil)
        (erc-insert-timestamp-function #'erc-insert-timestamp-left))
    (erc-fill-tests--wrap-populate
     (lambda ()
       (should (= 8 left-margin-width))
       (pcase-let ((`((margin left-margin) ,displayed)
                    (get-text-property erc-insert-marker 'display)))
         (should (equal-including-properties
                  displayed #("    ERC>" 4 8
                              ( read-only t
                                front-sticky t
                                field erc-prompt
                                erc-prompt t
                                rear-nonsticky t
                                font-lock-face erc-prompt-face)))))
       (erc-fill-tests--compare "stamps-left-01")

       (ert-info ("Shrink left margin by 1 col")
         (erc-stamp--adjust-margin -1)
         (with-silent-modifications (erc--refresh-prompt))
         (should (= 7 left-margin-width))
         (pcase-let ((`((margin left-margin) ,displayed)
                      (get-text-property erc-insert-marker 'display)))
           (should (equal-including-properties
                    displayed #("   ERC>" 3 7
                                ( read-only t
                                  front-sticky t
                                  field erc-prompt
                                  erc-prompt t
                                  rear-nonsticky t
                                  font-lock-face erc-prompt-face))))))))))

;;; erc-fill-tests.el ends here
