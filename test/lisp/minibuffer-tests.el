;;; minibuffer-tests.el --- Tests for completion functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2024 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

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

;;

;;; Code:

(require 'ert)
(require 'ert-x)

(eval-when-compile (require 'cl-lib))

(ert-deftest completion-test1 ()
  (with-temp-buffer
    (cl-flet* ((test/completion-table (_string _pred action)
                                      (if (eq action 'lambda)
                                          nil
                                        "test: "))
               (test/completion-at-point ()
                                         (list (copy-marker (point-min))
                                               (copy-marker (point))
                                               #'test/completion-table)))
      (let ((completion-at-point-functions (list #'test/completion-at-point)))
        (insert "TEST")
        (completion-at-point)
        (should (equal (buffer-string)
                       "test: "))))))

(ert-deftest completion-table-with-predicate-test ()
  (let ((full-collection
         '("apple"                      ; Has A.
           "beet"                       ; Has B.
           "banana"                     ; Has A & B.
           "cherry"                     ; Has neither.
           ))
        (no-A (lambda (x) (not (string-match-p "a" x))))
        (no-B (lambda (x) (not (string-match-p "b" x)))))
    (should
     (member "cherry"
             (completion-table-with-predicate
              full-collection no-A t "" no-B t)))
    (should-not
     (member "banana"
             (completion-table-with-predicate
              full-collection no-A t "" no-B t)))
    ;; "apple" should still match when strict is nil.
    (should (eq t (try-completion
                   "apple"
                   (apply-partially
                    'completion-table-with-predicate
                    full-collection no-A nil)
                   no-B)))
    ;; "apple" should still match when strict is nil and pred2 is nil
    ;; (Bug#27841).
    (should (eq t (try-completion
                   "apple"
                   (apply-partially
                    'completion-table-with-predicate
                    full-collection no-A nil))))))

(ert-deftest completion-table-subvert-test ()
  (let* ((origtable '("A-hello" "A-there"))
         (subvtable (completion-table-subvert origtable "B" "A")))
    (should (equal (try-completion "B-hel" subvtable)
                   "B-hello"))
    (should (equal (all-completions "B-hel" subvtable) '("-hello")))
    (should (test-completion "B-hello" subvtable))
    (should (equal (completion-boundaries "B-hel" subvtable
                                          nil "suffix")
                   '(1 . 6)))))

(ert-deftest completion-table-test-quoting ()
  (let ((process-environment
         `("CTTQ1=ed" "CTTQ2=et/" ,@process-environment))
        (default-directory (ert-resource-directory)))
    (pcase-dolist (`(,input ,output)
                   '(
                     ;; Test that $ in files is properly $$ quoted.
                     ("data/m-cttq" "data/minibuffer-test-cttq$$tion")
                     ;; Test that $$ in input is properly unquoted.
                     ("data/m-cttq$$t" "data/minibuffer-test-cttq$$tion")
                     ;; Test that env-vars are preserved.
                     ("lisp/c${CTTQ1}et/se-u" "lisp/c${CTTQ1}et/semantic-utest")
                     ("lisp/ced${CTTQ2}se-u" "lisp/ced${CTTQ2}semantic-utest")
                     ;; Test that env-vars don't prevent partial-completion.
                     ;; FIXME: Ideally we'd like to keep the ${CTTQ}!
                     ("lis/c${CTTQ1}/se-u" "lisp/cedet/semantic-utest")
                     ))
      (should (equal (completion-try-completion input
                                                #'completion--file-name-table
                                                nil (length input))
                     (cons output (length output)))))))

(ert-deftest completion--insert-strings-faces ()
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" "suffix1")))
    (should (equal (get-text-property 12 'face) '(completions-annotations))))
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" #("suffix1" 0 7 (face shadow)))))
    (should (equal (get-text-property 12 'face) 'shadow)))
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" "prefix1" "suffix1")))
    (should (equal (get-text-property 19 'face) nil)))
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" "prefix1" #("suffix1" 0 7 (face shadow)))))
    (should (equal (get-text-property 19 'face) 'shadow))))

(ert-deftest completion-pcm--optimize-pattern ()
  (should (equal (completion-pcm--optimize-pattern '("buf" point "f"))
                 '("buf" point "f")))
  (should (equal (completion-pcm--optimize-pattern '(any "" any))
                 '(any))))

(defun test-completion-all-sorted-completions (base def history-var history-list)
  (with-temp-buffer
    (insert base)
    (cl-letf (((symbol-function #'minibufferp) (lambda (&rest _) t)))
      (let ((completion-styles '(basic))
            (completion-category-defaults nil)
            (completion-category-overrides nil)
            (minibuffer-history-variable history-var)
            (minibuffer-history history-list)
            (minibuffer-default def)
            (minibuffer-completion-table
             (lambda (str pred action)
               (pcase action
                 (`(boundaries . ,_) `(boundaries ,(length base) . 0))
                 (_ (complete-with-action
                     action
                     '("epsilon" "alpha" "gamma" "beta" "delta")
                     (substring str (length base)) pred))))))
        (completion-all-sorted-completions)))))

(ert-deftest completion-all-sorted-completions ()
  ;; No base, disabled history, no default
  (should (equal (test-completion-all-sorted-completions
                  "" nil t nil)
                 `("beta" "alpha" "delta" "gamma" "epsilon" . 0)))
  ;; No base, disabled history, default string
  (should (equal (test-completion-all-sorted-completions
                  "" "gamma" t nil)
                 `("gamma" "beta" "alpha" "delta" "epsilon" . 0)))
  ;; No base, empty history, default string
  (should (equal (test-completion-all-sorted-completions
                  "" "gamma" 'minibuffer-history nil)
                 `("gamma" "beta" "alpha" "delta" "epsilon" . 0)))
  ;; No base, empty history, default list
  (should (equal (test-completion-all-sorted-completions
                  "" '("gamma" "zeta") 'minibuffer-history nil)
                 `("gamma" "beta" "alpha" "delta" "epsilon" . 0)))
  ;; No base, history, default string
  (should (equal (test-completion-all-sorted-completions
                  "" "gamma" 'minibuffer-history '("other" "epsilon" "delta"))
                 `("gamma" "epsilon" "delta" "beta" "alpha"  . 0)))
  ;; Base, history, default string
  (should (equal (test-completion-all-sorted-completions
                  "base/" "base/gamma" 'minibuffer-history
                  '("some/alpha" "base/epsilon" "base/delta"))
                 `("gamma" "epsilon" "delta" "beta" "alpha"  . 5)))
  ;; Base, history, default string
  (should (equal (test-completion-all-sorted-completions
                  "base/" "gamma" 'minibuffer-history
                  '("some/alpha" "base/epsilon" "base/delta"))
                 `("epsilon" "delta" "beta" "alpha" "gamma"  . 5))))

(defun completion--pcm-score (comp)
  "Get `completion-score' from COMP."
  (get-text-property 0 'completion-score comp))

(defun completion--pcm-first-difference-pos (comp)
  "Get `completions-first-difference' from COMP."
  (cl-loop for pos = (next-single-property-change 0 'face comp)
           then (next-single-property-change pos 'face comp)
           while pos
           when (eq (get-text-property pos 'face comp)
                    'completions-first-difference)
           return pos))

(ert-deftest completion-pcm-test-1 ()
  ;; Point is at end, this does not match anything
  (should (null
           (completion-pcm-all-completions
            "foo" '("hello" "world" "barfoobar") nil 3))))

(ert-deftest completion-pcm-test-2 ()
  ;; Point is at beginning, this matches "barfoobar"
  (should (equal
           (car (completion-pcm-all-completions
                 "foo" '("hello" "world" "barfoobar") nil 0))
           "barfoobar")))

(ert-deftest completion-pcm-test-3 ()
  ;; Full match!
  (should (eql
           (completion--pcm-score
            (car (completion-pcm-all-completions
                  "R" '("R" "hello") nil 1)))
           1.0)))

(ert-deftest completion-pcm-test-4 ()
  ;; One fourth of a match and no match due to point being at the end
  (should (eql
           (completion--pcm-score
            (car (completion-pcm-all-completions
                  "RO" '("RaOb") nil 1)))
           (/ 1.0 4.0)))
  (should (null
           (completion-pcm-all-completions
            "RO" '("RaOb") nil 2))))

(ert-deftest completion-pcm-test-5 ()
  ;; Since point is at the beginning, there is nothing that can really
  ;; be typed anymore
  (should (null
           (completion--pcm-first-difference-pos
            (car (completion-pcm-all-completions
                  "f" '("few" "many") nil 0))))))

(ert-deftest completion-pcm-test-6 ()
  ;; Wildcards and delimiters work
  (should (equal
           (car (completion-pcm-all-completions
                 "li-pac*" '("list-packages") nil 7))
           "list-packages"))
  (should (null
           (car (completion-pcm-all-completions
                 "li-pac*" '("do-not-list-packages") nil 7)))))

(ert-deftest completion-substring-test-1 ()
  ;; One third of a match!
  (should (equal
           (car (completion-substring-all-completions
                 "foo" '("hello" "world" "barfoobar") nil 3))
           "barfoobar"))
  (should (eql
           (completion--pcm-score
            (car (completion-substring-all-completions
                  "foo" '("hello" "world" "barfoobar") nil 3)))
           (/ 1.0 3.0))))

(ert-deftest completion-substring-test-2 ()
  ;; Full match!
  (should (eql
           (completion--pcm-score
            (car (completion-substring-all-completions
                  "R" '("R" "hello") nil 1)))
           1.0)))

(ert-deftest completion-substring-test-3 ()
  ;; Substring match
  (should (equal
           (car (completion-substring-all-completions
                 "custgroup" '("customize-group") nil 4))
           "customize-group"))
  (should (null
           (car (completion-substring-all-completions
                 "custgroup" '("customize-group") nil 5)))))

(ert-deftest completion-substring-test-4 ()
  ;; `completions-first-difference' should be at the right place
  (should (eql
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjobstabby" "many") nil 1)))
           4))
  (should (null
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjabstabby" "many") nil 1)))))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjabstabby" "many") nil 3)))
           6)))

(ert-deftest completion-flex-test-1 ()
  ;; Fuzzy match
  (should (equal
           (car (completion-flex-all-completions
                 "foo" '("hello" "world" "fabrobazo") nil 3))
           "fabrobazo")))

(ert-deftest completion-flex-test-2 ()
  ;; Full match!
  (should (eql
           (completion--pcm-score
            (car (completion-flex-all-completions
                  "R" '("R" "hello") nil 1)))
           1.0)))

(ert-deftest completion-flex-test-3 ()
  ;; Another fuzzy match, but more of a "substring" one
  (should (equal
           (car (completion-flex-all-completions
                 "custgroup" '("customize-group-other-window") nil 4))
           "customize-group-other-window"))
  ;; `completions-first-difference' should be at the right place
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-flex-all-completions
                  "custgroup" '("customize-group-other-window") nil 4)))
           4))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-flex-all-completions
                  "custgroup" '("customize-group-other-window") nil 9)))
           15)))


(defmacro completing-read-with-minibuffer-setup (collection &rest body)
  (declare (indent 1) (debug (collection body)))
  `(catch 'result
     (minibuffer-with-setup-hook
         (lambda ()
           (let ((redisplay-skip-initial-frame nil)
                 (executing-kbd-macro nil)) ; Don't skip redisplay
             (throw 'result (progn . ,body))))
       (let ((executing-kbd-macro t)) ; Force the real minibuffer
         (completing-read "Prompt: " ,collection)))))

(ert-deftest completion-auto-help-test ()
  (let (messages)
    (cl-letf* (((symbol-function 'minibuffer-message)
                (lambda (message &rest args)
                  (push (apply #'format-message message args) messages))))
      (let ((completion-auto-help nil))
        (completing-read-with-minibuffer-setup
            '("a" "ab" "ac")
          (execute-kbd-macro (kbd "a TAB TAB"))
          (should (equal (car messages) "Complete, but not unique"))
          (should-not (get-buffer-window "*Completions*" 0))
          (execute-kbd-macro (kbd "b TAB"))
          (should (equal (car messages) "Sole completion"))))
      (let ((completion-auto-help t))
        (completing-read-with-minibuffer-setup
            '("a" "ab" "ac")
          (execute-kbd-macro (kbd "a TAB TAB"))
          (should (get-buffer-window "*Completions*" 0))
          (execute-kbd-macro (kbd "b TAB"))
          (should (equal (car messages) "Sole completion"))))
      (let ((completion-auto-help 'visible))
        (completing-read-with-minibuffer-setup
         '("a" "ab" "ac" "achoo")
         (execute-kbd-macro (kbd "a TAB TAB"))
         (should (get-buffer-window "*Completions*" 0))
         (execute-kbd-macro (kbd "ch TAB"))
         (should (equal (car messages) "Sole completion")))))))

(ert-deftest completion-auto-select-test ()
  (let ((completion-auto-select t))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (execute-kbd-macro (kbd "a TAB"))
      (should (and (get-buffer-window "*Completions*" 0)
                   (eq (current-buffer) (get-buffer "*Completions*"))))
      (execute-kbd-macro (kbd "TAB TAB TAB"))
      (should (and (get-buffer-window "*Completions*" 0)
                   (eq (current-buffer) (get-buffer " *Minibuf-1*"))))
      (execute-kbd-macro (kbd "S-TAB"))
      (should (and (get-buffer-window "*Completions*" 0)
                   (eq (current-buffer) (get-buffer "*Completions*"))))))
  (let ((completion-auto-select 'second-tab))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (execute-kbd-macro (kbd "a TAB"))
      (should (and (get-buffer-window "*Completions*" 0)
                   (not (eq (current-buffer) (get-buffer "*Completions*")))))
      (execute-kbd-macro (kbd "TAB TAB"))
      (should (eq (current-buffer) (get-buffer "*Completions*"))))))

(ert-deftest completion-auto-wrap-test ()
  (let ((completion-auto-wrap nil))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (insert "a")
      (minibuffer-completion-help)
      (switch-to-completions)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (next-completion 2)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      ;; Fixed in bug#54374
      (next-completion 5)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (previous-completion 5)
      (should (equal "aa" (get-text-property (point) 'completion--string)))))
  (let ((completion-auto-wrap t))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (insert "a")
      (minibuffer-completion-help)
      (switch-to-completions)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (next-completion 2)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (next-completion 1)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (previous-completion 1)
      (should (equal "ac" (get-text-property (point) 'completion--string))))))

(ert-deftest completions-header-format-test ()
  (let ((completion-show-help nil)
        (completions-header-format nil))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (insert "a")
      (minibuffer-completion-help)
      (switch-to-completions)
      ;; Fixed in bug#55430
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (next-completion 2)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (previous-completion 2)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      ;; Fixed in bug#54374
      (previous-completion 1)
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      (next-completion 1)
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      ;; Fixed in bug#55430
      (execute-kbd-macro (kbd "C-u RET"))
      (should (equal (minibuffer-contents) "aa")))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      ;; Fixed in bug#55289
      (execute-kbd-macro (kbd "a M-<up> M-<down>"))
      (should (equal (minibuffer-contents) "aa")))))

(ert-deftest completions-affixation-navigation-test ()
  (let ((completion-extra-properties
         '(:affixation-function
           (lambda (completions)
             (mapcar (lambda (c)
                       (list c "prefix " " suffix"))
                     completions)))))
    (completing-read-with-minibuffer-setup
        '("aa" "ab" "ac")
      (insert "a")
      (minibuffer-completion-help)
      (switch-to-completions)
      (should (equal 'highlight (get-text-property (point) 'mouse-face)))
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (let ((completion-auto-wrap t))
        (next-completion 3))
      (should (equal 'highlight (get-text-property (point) 'mouse-face)))
      (should (equal "aa" (get-text-property (point) 'completion--string)))
      (let ((completion-auto-wrap nil))
        (next-completion 3))
      (should (equal 'highlight (get-text-property (point) 'mouse-face)))
      (should (equal "ac" (get-text-property (point) 'completion--string)))
      ;; Fixed in bug#54374
      (goto-char (1- (point-max)))
      (should-not (equal 'highlight (get-text-property (point) 'mouse-face)))
      (execute-kbd-macro (kbd "C-u RET"))
      (should (equal (minibuffer-contents) "ac")))))

(provide 'minibuffer-tests)
;;; minibuffer-tests.el ends here
