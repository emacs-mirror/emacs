;;; help-tests.el --- Tests for help.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022 Free Software Foundation, Inc.

;; Author: Juanma Barranquero <lekktu@gmail.com>
;;         Eli Zaretskii <eliz@gnu.org>
;;         Stefan Kangas <stefankangas@gmail.com>
;; Keywords: help, internal

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
(eval-when-compile (require 'cl-lib))
(require 'text-property-search) ; for `text-property-search-forward'

(ert-deftest help-split-fundoc-SECTION ()
  "Test new optional arg SECTION."
  (let* ((doc "Doc first line.\nDoc second line.")
         (usg "\n\n(fn ARG1 &optional ARG2)")
         (full (concat doc usg))
         (usage "(t ARG1 &optional ARG2)"))
    ;; Docstring has both usage and doc
    (should (equal (help-split-fundoc full t nil)    `(,usage . ,doc)))
    (should (equal (help-split-fundoc full t t)      `(,usage . ,doc)))
    (should (equal (help-split-fundoc full t 'usage)  usage))
    (should (equal (help-split-fundoc full t 'doc)    doc))
    ;; Docstring has no usage, only doc
    (should (equal (help-split-fundoc doc t nil)      nil))
    (should (equal (help-split-fundoc doc t t)       `(nil . ,doc)))
    (should (equal (help-split-fundoc doc t 'usage)  nil))
    (should (equal (help-split-fundoc doc t 'doc)    doc))
    ;; Docstring is only usage, no doc
    (should (equal (help-split-fundoc usg t nil)     `(,usage . nil)))
    (should (equal (help-split-fundoc usg t t)       `(,usage . nil)))
    (should (equal (help-split-fundoc usg t 'usage)  usage))
    (should (equal (help-split-fundoc usg t 'doc)    nil))
    ;; Docstring is null
    (should (equal (help-split-fundoc nil t nil)     nil))
    (should (equal (help-split-fundoc nil t t)       '(nil)))
    (should (equal (help-split-fundoc nil t 'usage)  nil))
    (should (equal (help-split-fundoc nil t 'doc)    nil))))


;;; substitute-command-keys

(defmacro with-substitute-command-keys-test (&rest body)
  `(cl-flet* ((test
               (lambda (orig result)
                 (should (equal (substitute-command-keys orig)
                                result))))
              (test-re
               (lambda (orig regexp)
                 (should (string-match (concat "^" regexp "$")
                                       (substitute-command-keys orig))))))
     ,@body))

(ert-deftest help-tests-substitute-command-keys/no-change ()
  (with-substitute-command-keys-test
   (test "foo" "foo")
   (test "\\invalid-escape" "\\invalid-escape")))

(ert-deftest help-tests-substitute-command-keys/commands ()
  (with-substitute-command-keys-test
   (test "foo \\[goto-char]" "foo M-g c")
   (test "\\[next-line]" "C-n")
   (test "\\[next-line]\n\\[next-line]" "C-n\nC-n")
   (test "\\[next-line]\\[previous-line]" "C-nC-p")
   (test "\\[next-line]\\=\\[previous-line]" "C-n\\[previous-line]")
   ;; Allow any style of quotes, since the terminal might not support
   ;; UTF-8.  Same thing is done below.
   (test-re "\\[next-line]`foo'" "C-n[`'‘]foo['’]")
   (test "\\[emacs-version]" "M-x emacs-version")
   (test "\\[emacs-version]\\[next-line]" "M-x emacs-versionC-n")
   (test-re "\\[emacs-version]`foo'" "M-x emacs-version[`'‘]foo['’]")))

(ert-deftest help-tests-substitute-command-keys/keymaps ()
  (with-substitute-command-keys-test
   (test "\\{minibuffer-local-must-match-map}"
         "\
key             binding
---             -------

C-g		abort-minibuffers
TAB		minibuffer-complete
C-j		minibuffer-complete-and-exit
RET		minibuffer-complete-and-exit
ESC		Prefix Command
SPC		minibuffer-complete-word
?		minibuffer-completion-help
C-<tab>		file-cache-minibuffer-complete
<XF86Back>	previous-history-element
<XF86Forward>	next-history-element
<down>		next-line-or-history-element
<next>		next-history-element
<prior>		switch-to-completions
<up>		previous-line-or-history-element

M-g		Prefix Command
M-v		switch-to-completions

M-g ESC		Prefix Command

M-<		minibuffer-beginning-of-buffer
M-n		next-history-element
M-p		previous-history-element
M-r		previous-matching-history-element
M-s		next-matching-history-element

M-g M-c		switch-to-completions

")))

(ert-deftest help-tests-substitute-command-keys/keymap-change ()
  (with-substitute-command-keys-test
   (test "\\<minibuffer-local-must-match-map>\\[abort-recursive-edit]" "C-]")
   (test "\\<emacs-lisp-mode-map>\\[eval-defun]" "C-M-x")))

(defvar help-tests-remap-map
  (let ((map (make-keymap)))
    (define-key map (kbd "x") 'foo)
    (define-key map (kbd "y") 'bar)
    (define-key map [remap foo] 'bar)
    map))

(ert-deftest help-tests-substitute-command-keys/remap ()
  (should (equal (substitute-command-keys "\\<help-tests-remap-map>\\[foo]") "y"))
  (should (equal (substitute-command-keys "\\<help-tests-remap-map>\\[bar]") "y")))

(ert-deftest help-tests-substitute-command-keys/undefined-map ()
  (with-substitute-command-keys-test
   (test-re "\\{foobar-map}"
            "\nUses keymap [`'‘]foobar-map['’], which is not currently defined.\n")))

(ert-deftest help-tests-substitute-command-keys/quotes ()
 (with-substitute-command-keys-test
  (let ((text-quoting-style 'curve))
    (test "quotes ‘like this’" "quotes ‘like this’")
    (test "`x'" "‘x’")
    (test "`" "‘")
    (test "'" "’")
    (test "\\`" "\\‘"))
  (let ((text-quoting-style 'straight))
    (test "quotes `like this'" "quotes 'like this'")
    (test "`x'" "'x'")
    (test "`" "'")
    (test "'" "'")
    (test "\\`" "\\'"))
  (let ((text-quoting-style 'grave))
    (test "quotes `like this'" "quotes `like this'")
    (test "`x'" "`x'")
    (test "`" "`")
    (test "'" "'")
    (test "\\`" "\\`"))))

(ert-deftest help-tests-substitute-command-keys/literals ()
  (with-substitute-command-keys-test
   (test "foo \\=\\[goto-char]" "foo \\[goto-char]")
   (test "foo \\=\\=" "foo \\=")
   (test "\\=\\=" "\\=")
   (test "\\=\\[" "\\[")
   (let ((text-quoting-style 'curve))
     (test "\\=`x\\='" "`x'"))
   (let ((text-quoting-style 'straight))
     (test "\\=`x\\='" "`x'"))
   (let ((text-quoting-style 'grave))
     (test "\\=`x\\='" "`x'"))))

(ert-deftest help-tests-substitute-command-keys/no-change ()
  (with-substitute-command-keys-test
   (test "\\[foobar" "\\[foobar")
   (test "\\=" "\\=")))

(ert-deftest help-tests-substitute-command-keys/multibyte ()
  ;; Cannot use string= here, as that compares unibyte and multibyte
  ;; strings not equal.
  (should (compare-strings
           (substitute-command-keys "\200 \\[goto-char]") nil nil
           "\200 M-g c" nil nil)))

(ert-deftest help-tests-substitute-command-keys/apropos ()
  (save-window-excursion
    (apropos "foo")
    (switch-to-buffer "*Apropos*")
    (goto-char (point-min))
    (should (looking-at "Type RET on"))))

(defvar help-tests-major-mode-map
  (let ((map (make-keymap)))
    (define-key map "x" 'foo-original)
    (define-key map "1" 'foo-range)
    (define-key map "2" 'foo-range)
    (define-key map "3" 'foo-range)
    (define-key map "4" 'foo-range)
    (define-key map (kbd "C-e") 'foo-something)
    (define-key map '[F1] 'foo-function-key1)
    (define-key map "(" 'short-range)
    (define-key map ")" 'short-range)
    (define-key map "a" 'foo-other-range)
    (define-key map "b" 'foo-other-range)
    (define-key map "c" 'foo-other-range)
    map))

(define-derived-mode help-tests-major-mode nil
  "Major mode for testing shadowing.")

(defvar help-tests-minor-mode-map
  (let ((map (make-keymap)))
    (define-key map "x" 'foo-shadow)
    (define-key map (kbd "C-e") 'foo-shadow)
    map))

(define-minor-mode help-tests-minor-mode
  "Minor mode for testing shadowing.")

(ert-deftest help-tests-substitute-command-keys/add-key-face ()
  (should (equal (substitute-command-keys "\\[next-line]")
                 (propertize "C-n"
                             'face 'help-key-binding
                             'font-lock-face 'help-key-binding))))

(ert-deftest help-tests-substitute-command-keys/add-key-face-listing ()
  (with-temp-buffer
    (insert (substitute-command-keys "\\{help-tests-minor-mode-map}"))
    (goto-char (point-min))
    (text-property-search-forward 'face 'help-key-binding)
    (should (looking-at "C-e"))
    ;; Don't fontify trailing whitespace.
    (should-not (get-text-property (+ (point) 3) 'face))
    (text-property-search-forward 'face 'help-key-binding)
    (should (looking-at "x"))
    (should-not (get-text-property (+ (point) 1) 'face))))

(ert-deftest help-tests-substitute-command-keys/test-mode ()
  (with-substitute-command-keys-test
   (with-temp-buffer
     (help-tests-major-mode)
     (test "\\{help-tests-major-mode-map}"
           "\
key             binding
---             -------

( .. )		short-range
1 .. 4		foo-range
a .. c		foo-other-range

C-e		foo-something
x		foo-original
<F1>		foo-function-key1

"))))

(ert-deftest help-tests-substitute-command-keys/shadow ()
  (with-substitute-command-keys-test
   (with-temp-buffer
     (help-tests-major-mode)
     (help-tests-minor-mode)
     (test "\\{help-tests-major-mode-map}"
           "\
key             binding
---             -------

( .. )		short-range
1 .. 4		foo-range
a .. c		foo-other-range

C-e		foo-something
  (this binding is currently shadowed)
x		foo-original
  (this binding is currently shadowed)
<F1>		foo-function-key1

"))))

(ert-deftest help-tests-substitute-command-keys/command-remap ()
  (with-substitute-command-keys-test
   (let ((help-tests-major-mode-map (make-keymap))) ; Protect from changes.
    (with-temp-buffer
      (help-tests-major-mode)
      (define-key help-tests-major-mode-map [remap foo] 'bar)
      (test "\\{help-tests-major-mode-map}"
            "\
key             binding
---             -------

<remap>		Prefix Command

<remap> <foo>	bar

")))))

(ert-deftest help-tests-describe-map-tree/no-menu-t ()
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (map '(keymap . ((1 . foo)
                           (menu-bar keymap
                                     (foo menu-item "Foo" foo
                                          :enable mark-active
                                          :help "Help text"))))))
      (describe-map-tree map nil nil nil nil t nil nil nil)
      (should (equal (buffer-string) "key             binding
---             -------

C-a		foo

")))))

(ert-deftest help-tests-describe-map-tree/no-menu-nil ()
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (map '(keymap . ((1 . foo)
                           (menu-bar keymap
                                     (foo menu-item "Foo" foo
                                          :enable mark-active
                                          :help "Help text"))))))
      (describe-map-tree map nil nil nil nil nil nil nil nil)
      (should (equal (buffer-string) "key             binding
---             -------

C-a		foo
<menu-bar>	Prefix Command

<menu-bar> <foo>		foo

")))))

(ert-deftest help-tests-describe-map-tree/mention-shadow-t ()
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (map '(keymap . ((1 . foo)
                           (2 . bar))))
          (shadow-maps '((keymap . ((1 . baz))))))
      (describe-map-tree map t shadow-maps nil nil t nil nil t)
      (should (equal (buffer-string) "key             binding
---             -------

C-a		foo
  (this binding is currently shadowed)
C-b		bar

")))))

(ert-deftest help-tests-describe-map-tree/mention-shadow-nil ()
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (map '(keymap . ((1 . foo)
                           (2 . bar))))
          (shadow-maps '((keymap . ((1 . baz))))))
      (describe-map-tree map t shadow-maps nil nil t nil nil nil)
      (should (equal (buffer-string) "key             binding
---             -------

C-b		bar

")))))

(ert-deftest help-tests-describe-map-tree/partial-t ()
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (map '(keymap . ((1 . foo)
                           (2 . undefined)))))
      (describe-map-tree map t nil nil nil nil nil nil nil)
      (should (equal (buffer-string) "key             binding
---             -------

C-a		foo

")))))

(ert-deftest help-tests-describe-map-tree/partial-nil ()
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (map '(keymap . ((1 . foo)
                           (2 . undefined)))))
      (describe-map-tree map nil nil nil nil nil nil nil nil)
      (should (equal (buffer-string) "key             binding
---             -------

C-a		foo
C-b		undefined

")))))

(defvar help-tests--was-in-buffer nil)

(ert-deftest help-substitute-command-keys/menu-filter-in-correct-buffer ()
  "Evaluate menu-filter in the original buffer.  See Bug#39149."
  (unwind-protect
      (progn
        (define-key global-map (kbd "C-c C-l r")
          `(menu-item "2" identity
                      :filter ,(lambda (cmd)
                                 (setq help-tests--was-in-buffer
                                       (current-buffer))
                                 cmd)))
        (with-temp-buffer
          (substitute-command-keys "\\[identity]")
          (should (eq help-tests--was-in-buffer
                      (current-buffer)))))
    (setq help-tests--was-in-buffer nil)
    (define-key global-map (kbd "C-c C-l r") nil)
    (define-key global-map (kbd "C-c C-l") nil)))

(ert-deftest help-substitute-command-keys/preserves-text-properties ()
  "Check that we preserve text properties (Bug#17052)."
  (should (equal (substitute-command-keys
                  (propertize "foo \\[save-buffer]" 'face 'bold))
                 (propertize "foo C-x C-s" 'face 'bold))))

(provide 'help-tests)

;;; help-tests.el ends here
