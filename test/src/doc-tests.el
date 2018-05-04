;;; doc-tests.el --- Tests for doc.c

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

;; Author: Eli Zaretskii <eliz@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)

(ert-deftest doc-test-substitute-command-keys ()
  ;; Bindings.
  (should (string= (substitute-command-keys "foo \\[goto-char]") "foo M-g c"))
  ;; Cannot use string= here, as that compares unibyte and multibyte
  ;; strings not equal.
  (should (compare-strings
           (substitute-command-keys "\200 \\[goto-char]") nil nil
           "\200 M-g c" nil nil))
  ;; Literals.
  (should (string= (substitute-command-keys "foo \\=\\[goto-char]")
                   "foo \\[goto-char]"))
  (should (string= (substitute-command-keys "foo \\=\\=")
                   "foo \\="))
  ;; Keymaps.
  (should (string= (substitute-command-keys
                    "\\{minibuffer-local-must-match-map}")
                   "\
key             binding
---             -------

C-g		abort-recursive-edit
TAB		minibuffer-complete
C-j		minibuffer-complete-and-exit
RET		minibuffer-complete-and-exit
ESC		Prefix Command
SPC		minibuffer-complete-word
?		minibuffer-completion-help
<C-tab>		file-cache-minibuffer-complete
<XF86Back>	previous-history-element
<XF86Forward>	next-history-element
<down>		next-line-or-history-element
<next>		next-history-element
<prior>		switch-to-completions
<up>		previous-line-or-history-element

M-v		switch-to-completions

M-n		next-history-element
M-p		previous-history-element
M-r		previous-matching-history-element
M-s		next-matching-history-element

"))
  (should (string=
           (substitute-command-keys
            "\\<minibuffer-local-must-match-map>\\[abort-recursive-edit]")
           "C-g"))
  ;; Allow any style of quotes, since the terminal might not support
  ;; UTF-8.
  (should (string-match
           "\nUses keymap [`‘']foobar-map['’], which is not currently defined.\n"
            (substitute-command-keys "\\{foobar-map}")))
  ;; Quotes.
  (should (let ((text-quoting-style 'grave))
            (string= (substitute-command-keys "quotes `like this'")
                      "quotes `like this'")))
  (should (let ((text-quoting-style 'grave))
            (string= (substitute-command-keys "quotes ‘like this’")
                      "quotes ‘like this’")))
  (should (let ((text-quoting-style 'straight))
            (string= (substitute-command-keys "quotes `like this'")
                     "quotes 'like this'")))
  ;; Bugs.
  (should (string= (substitute-command-keys "\\[foobar") "\\[foobar"))
  (should (string= (substitute-command-keys "\\=") "\\="))
  )

(provide 'doc-tests)
;;; doc-tests.el ends here
