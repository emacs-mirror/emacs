;;; term-tests.el --- tests for term.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2019-2026 Free Software Foundation, Inc.

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
(require 'ert)
(require 'term)
(eval-when-compile (require 'cl-lib))

(defvar term-height)                    ; Number of lines in window.
(defvar term-width)                     ; Number of columns in window.

(defconst yellow-fg-props
  `(:foreground ,(face-foreground 'term-color-yellow nil 'default)))
(defconst yellow-bg-props
  `(:background ,(face-background 'term-color-yellow nil 'default)))
(defconst bright-yellow-fg-props
  `(:foreground ,(face-foreground 'term-color-bright-yellow nil 'default)))
(defconst bright-yellow-bg-props
  `(:background ,(face-background 'term-color-bright-yellow nil 'default)))
(defconst custom-color-fg-props
  `(:foreground "#87FFFF"))

(defconst ansi-test-strings
  `(("\e[33mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face `(,yellow-fg-props)))
    ("\e[43mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face `(,yellow-bg-props)))
    ("\e[93mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face `(,bright-yellow-fg-props)))
    ("\e[103mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face `(,bright-yellow-bg-props)))
    ("\e[1;33mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,yellow-fg-props term-bold))
     ,(propertize "Hello World" 'font-lock-face
                  `(,bright-yellow-fg-props term-bold)))
    ("\e[33;1mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,yellow-fg-props term-bold))
     ,(propertize "Hello World" 'font-lock-face
                  `(,bright-yellow-fg-props term-bold)))
    ("\e[1m\e[33mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,yellow-fg-props term-bold))
     ,(propertize "Hello World" 'font-lock-face
                  `(,bright-yellow-fg-props term-bold)))
    ("\e[33m\e[1mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,yellow-fg-props term-bold))
     ,(propertize "Hello World" 'font-lock-face
                  `(,bright-yellow-fg-props term-bold)))
    ("\e[38;5;3;1mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,yellow-fg-props term-bold))
     ,(propertize "Hello World" 'font-lock-face
                  `(,bright-yellow-fg-props term-bold)))
    ("\e[38;5;123;1mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,custom-color-fg-props term-bold)))
    ("\e[38;2;135;255;255;1mHello World\e[0m"
     ,(propertize "Hello World" 'font-lock-face
                  `(,custom-color-fg-props term-bold)))))

(defun term-test-screen-from-input (width height input &optional return-var)
  (with-temp-buffer
    (term-mode)
    ;; Keep dimensions independent from window size.
    (remove-function (local 'window-adjust-process-window-size-function)
                     'term-maybe-reset-size)
    (term-exec (current-buffer) "test" "cat" nil nil)
    (term-char-mode)
    (setq term-width width)
    (setq term-height height)
    ;; Pass input directly to `term-emulate-terminal', it's easier to
    ;; control chunking, and we don't have to worry about wrestling
    ;; with stty settings.
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Don't get stuck when we close the buffer.
      (set-process-query-on-exit-flag proc nil)
      (if (consp input)
                (mapc (lambda (input) (term-emulate-terminal proc input)) input)
              (term-emulate-terminal proc input))
      (if return-var (buffer-local-value return-var (current-buffer))
        (buffer-substring (point-min) (point-max))))))

(ert-deftest term-simple-lines ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let ((str "\
first line\r
next line\r\n"))
    (should (equal (term-test-screen-from-input 40 12 str)
                   (string-replace "\r" "" str)))))

(ert-deftest term-carriage-return ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let ((str "\
first line\r_next line\r\n"))
    (should (equal (term-test-screen-from-input 40 12 str)
                   "_next line\n"))))

(ert-deftest term-line-wrap ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (should (string-match-p
           ;; Don't be strict about trailing whitespace.
           "\\`a\\{40\\}\na\\{20\\} *\\'"
           (term-test-screen-from-input 40 12 (make-string 60 ?a))))
  ;; Again, but split input into chunks.
  (should (string-match-p
           "\\`a\\{40\\}\na\\{20\\} *\\'"
           (term-test-screen-from-input 40 12 (let ((str (make-string 30 ?a)))
                                                (list str str))))))

(ert-deftest term-line-wrap-no-auto-margins ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let* ((width 40)
         (line (cl-loop for i upfrom 0 to 60
                     collect (+ ?a (% i 26)) into chars
                     finally return (apply #'string chars)))
         (expected (concat (substring line 0 (1- width))
                           (substring line (1- (length line)))))
         (rmam "\e[?7l"))
    (should
     (equal (term-test-screen-from-input width 12 (concat rmam line))
            expected))
    ;; Again, but split input into chunks.
    (should (equal
             (term-test-screen-from-input
              width 12
              (cl-loop
                    with step = 3
                    with n = (length line)
                    for i upfrom 0 below n by step
                    collect (substring line i (min n (+ i step))) into parts
                    finally return (cons rmam parts)))
             expected))))

(ert-deftest term-colors ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (pcase-dolist (`(,str ,expected) ansi-test-strings)
    (let ((result (term-test-screen-from-input 40 12 str)))
      (should (equal result expected))
      (should (equal (text-properties-at 0 result)
                     (text-properties-at 0 expected))))))

(ert-deftest term-colors-bold-is-bright ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let ((ansi-color-bold-is-bright t))
    (pcase-dolist (`(,str ,expected ,bright-expected) ansi-test-strings)
      (let ((expected (or bright-expected expected))
            (result (term-test-screen-from-input 40 12 str)))
        (should (equal result expected))
        (should (equal (text-properties-at 0 result)
                       (text-properties-at 0 expected)))))))

(ert-deftest term-cursor-movement ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  ;; Absolute positioning.
  (should (equal "ab\ncd"
                 (term-test-screen-from-input
                  40 12 (concat "\e[2;2Hd"
                                "\e[2;1Hc"
                                "\e[1;2Hb"
                                "\e[1;1Ha"))))
  ;; Send one byte at a time.
  (should (equal "ab\ncd"
                 (term-test-screen-from-input
                  40 12 (split-string (concat "\e[2;2Hd"
                                              "\e[2;1Hc"
                                              "\e[1;2Hb"
                                              "\e[1;1Ha") "" t))))
  (should (equal "abcde    j"
                 (term-test-screen-from-input
                  10 12 '("abcdefghij"
                          "\e[H"  ;move back to point-min
                          "abcde"
                          "    j"))))

  ;; Relative positioning.
  (should (equal "ab\ncd"
                 (term-test-screen-from-input
                  40 12 (concat "\e[B\e[Cd"
                                "\e[D\e[Dc"
                                "\e[Ab"
                                "\e[D\e[Da")))))

(ert-deftest term-scrolling-region ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (should (equal "\
line3
line4
line5
line6
"
                 (term-test-screen-from-input
                  40 12 "\e[1;5r\
line1\r
line2\r
line3\r
line4\r
line5\r
line6\r
")))

  ;; test reverse scrolling
  (should (equal "line1
line7
line6
line2
line5"
                 (term-test-screen-from-input 40 5
                                              '("\e[0;0H"
                                                "\e[J"
                                                "line1\r
line2\r
line3\r
line4\r
line5"
                                                "\e[2;4r"
                                                "\e[2;0H"
                                                "\e[2;0H"
                                                "\eMline6"
                                                "\e[2;0H"
                                                "\eMline7"))))

  ;; test scrolling down
  (should (equal "line1
line3
line4
line7
line5"
                 (term-test-screen-from-input 40 5
                                              '("\e[0;0H"
                                                "\e[J"
                                                "line1\r
line2\r
line3\r
line4\r
line5"
                                                "\e[2;4r"
                                                "\e[2;0H"
                                                "\e[4;5H"
                                                "\n\rline7"))))

  ;; setting the scroll region end beyond the max height should not
  ;; turn on term-scroll-with-delete
  (should (equal "line1
line2
line3
line4
line5
line6
line7"
                 (term-test-screen-from-input 40 5
                                                      '("\e[1;10r"
                                                        "line1\r
line2\r
line3\r
line4\r
line5\r
line6\r
line7"))))


  ;; resetting the terminal should set the scroll region end to (1- term-height).
  (should (equal "
line1
line2
line3
line4
"
                 (term-test-screen-from-input 40 5
                                                      '("\e[1;10r"
                                                        "\ec" ;reset
                                                        "line1\r
line2\r
line3\r
line4\r
line5"
                                                        "\e[1;1H"
                                                        "\e[L"))))

  ;; scroll region should be limited to the (1- term-height).  Note,
  ;; this fixes an off by one error when comparing the scroll region
  ;; end with term-height.
  (should (equal "
line1
line2
line3
line4
"
                 (term-test-screen-from-input 40 5
                                              '("\e[1;6r"
                                                "line1\r
line2\r
line3\r
line4\r
line5"
                                                "\e[1;1H" ;go back to home
                                                "\e[L"    ;insert a new line at the top
                                                ))))

  ;; setting the scroll region to the entire height should not turn on
  ;; term-scroll-with-delete
  (should (equal "line1
line2
line3
line4
line5
line6"
                 (term-test-screen-from-input 40 5
                                                      '("\e[1;5r"
                                                        "line1\r
line2\r
line3\r
line4\r
line5\r
line6"))))

  ;; reset should reset term-scroll-with-delete
  (should (equal "line1
line2
line3
line4
line5
line6
line7"
                 (term-test-screen-from-input 40 5
                                              '("\e[2;5r" ;set the region
                                                "\ec" ;reset
                                                "line1\r
line2\r
line3\r
line4\r
line5\r
line6\r
line7")))))

(ert-deftest term-set-directory ()
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let ((term-ansi-at-user (user-real-login-name)))
    (should (equal (term-test-screen-from-input
                    40 12 "\eAnSiTc /foo/\n" 'default-directory)
                   "/foo/"))
    ;; Split input (Bug#17231).
    (should (equal (term-test-screen-from-input
                    40 12 (list "\eAnSiTc /f" "oo/\n") 'default-directory)
                   "/foo/"))))

(ert-deftest term-line-wrapping-then-motion ()
  "Make sure we reset the line-wrapping state after moving cursor.
A real-life example is the default zsh prompt which writes spaces
to the end of line (triggering line-wrapping state), and then
sends a carriage return followed by another space to overwrite
the first character of the line."
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let* ((width 10)
         (strs (list "x" (make-string (1- width) ?_)
                     "\r_")))
    (should (equal (term-test-screen-from-input width 12 strs)
                   (make-string width ?_)))))

(ert-deftest term-to-margin ()
  "Test cursor movement at the scroll margin.
This is a reduced example from GNU nano's initial screen."
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (let* ((width 10)
         (x (make-string width ?x))
         (y (make-string width ?y)))
    (should (equal (term-test-screen-from-input
                    width 3
                    `("\e[1;3r"       ; Setup 3 line scrolling region.
                      "\e[2;1H"       ; Move to 2nd last line.
                      ,x              ; Fill with 'x'.
                      "\r\e[1B"       ; Next line.
                      ,y))            ; Fill with 'y'.
                   (concat "\n" x "\n" y)))
    ;; Same idea, but moving upwards.
    (should (equal (term-test-screen-from-input
                    width 3
                    `("\e[1;3r" "\e[2;1H" ,x "\r\e[1A" ,y))
                   (concat y "\n" x)))))

(ert-deftest term-decode-partial () ;; Bug#25288.
  "Test multibyte characters sent into multiple chunks."
  ;; Set `locale-coding-system' so test will be deterministic.
  (let ((locale-coding-system 'utf-8-unix))
    (should (equal "шшш" (term-test-screen-from-input
                          40 1 '("\321" "\210\321\210\321\210"))))
    (should (equal "шшш" (term-test-screen-from-input
                          40 1 '("\321\210\321" "\210\321\210"))))
    (should (equal "шшш" (term-test-screen-from-input
                          40 1 '("\321\210\321\210\321" "\210"))))))

(ert-deftest term-undecodable-input () ;; Bug#29918.
  "Undecodable bytes should be passed through without error."
  (let* ((locale-coding-system 'utf-8-unix) ; As above.
         (bytes "\376\340\360\370.")
         (string (decode-coding-string bytes locale-coding-system)))
    (should (equal string
                   (term-test-screen-from-input
                    40 1 bytes)))))

(ert-deftest term-ignore-osc ()
  ;; BEL-terminated OSC sequence
  (should (equal "test"
                 (term-test-screen-from-input
                  40 1 "te\e]0;window title\ast")))
  ;; ESC \-terminated OSC sequence
  (should (equal "test"
                 (term-test-screen-from-input
                  40 1 "te\e]0;window title\e\\st")))
  ;; Long OSC sequence split into multiple chunks
  (should (equal "test"
                 (term-test-screen-from-input
                  40 1 '("te\e]0;win" "dow " " title\ast"))))
  ;; OSC sequence that start and ends with the chunk
  (should (equal "test"
                 (term-test-screen-from-input
                  40 1 '("te" "\e]0;window " "title\a" "st"))))

  ;; Invalid control characters break out of the OSC sequence, for
  ;; safety.
  (should (equal "tetitlest"
                 (term-test-screen-from-input
                  40 1 '("te\e]0;window\x05title\ast"))))

  (let ((locale-coding-system 'utf-8-unix))
    ;; An OSC sequence with multibyte UTF-8 characters.  This is not
    ;; exactly standard-compliant, but too common not to support.
    (should (equal "test"
                   (term-test-screen-from-input
                    40 1 "te\e]0;\xce\xb1\xce\xb2\e\\st")))))

(ert-deftest term-handle-osc ()
  (let* ((captured nil)
         (handler (lambda (code text)
                    (push (cons code text)
                          captured)))
         (term-osc-handlers `(("2" . ,handler)
                              ("1994" . ,handler))))

    ;; Send OSC sequences to handler
    (should (equal "test"
                   (term-test-screen-from-input
                    40 1 "te\e]2;foo\as\e]1994;bar\at")))
    (should (equal '(("2" . "foo")
                     ("1994" . "bar"))
                   (nreverse captured)))

    ;; OSC sequences and code can be chunked
    (setq captured nil)
    (should (equal "test"
                   (term-test-screen-from-input
                    40 1 `("te\e]2;chunked fo"
                           "o\as\e]19"
                           "94;chunked ba"
                           "r\at"))))
    (should (equal '(("2" . "chunked foo")
                     ("1994" . "chunked bar"))
                   (nreverse captured)))

    ;; OSC sequences can contain multibyte characters
    (let ((locale-coding-system 'utf-8-unix))
      (setq captured nil)
      (should (equal "test"
                     (term-test-screen-from-input
                      40 1 "te\e]2;\xce\xb1\xce\xb2\e\\st")))
      (should (equal '(("2" . "\u03b1\u03b2")) captured)))

    ;; Ignore unhandled and invalid OSC sequences
    (setq captured nil)
    (should (equal
             "test"
             (term-test-screen-from-input
              40 1 "t\e]3;unhandled\aest")))
    (should-not captured)
    (should (equal
             "test"
             (term-test-screen-from-input
              40 1 "t\e]2missing semicolon\aest")))
    (should-not captured)

    (should (equal
             "test"
             (term-test-screen-from-input
              40 1 "t\e]2;not ended\003est")))
    (should-not captured)))

(ert-deftest term-call-ansi-osc-handlers ()
  (let* ((captured nil)
         (osc-handler (lambda (code text)
                        (push (list 'osc code text)
                              captured)))
         (term-handler (lambda (code text)
                         (push (list 'term code text)
                               captured)))
         (ansi-osc-handlers `(("1" . ,osc-handler)
                              ("2" . ,osc-handler)
                              ("3" . ,osc-handler)))
         (term-osc-handlers `(("2" . ,term-handler)
                              ("3" . nil))))

    (should
     (equal
      "test"
      (term-test-screen-from-input
       40 1 (concat
             "te"
             "\e]1;a\a" ;; sent to osc-handler
             "\e]2;b\a" ;; sent to term-handler
             "\e]3;c\a" ;; ignored; disabled in term
             "\e]4;d\a" ;; ignored; not registered
             "st"))))
    (should
     (equal
      '((osc "1" "a")
        (term "2" "b"))
      (nreverse captured)))))

(provide 'term-tests)

;;; term-tests.el ends here
