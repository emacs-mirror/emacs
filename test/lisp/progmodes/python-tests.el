;;; python-tests.el --- Test suite for python.el

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

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
(require 'python)

;; Dependencies for testing:
(require 'electric)
(require 'hideshow)
(require 'tramp-sh)


(defmacro python-tests-with-temp-buffer (contents &rest body)
  "Create a `python-mode' enabled temp buffer with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((python-indent-guess-indent-offset nil))
       (python-mode)
       (insert ,contents)
       (goto-char (point-min))
       ,@body)))

(defmacro python-tests-with-temp-file (contents &rest body)
  "Create a `python-mode' enabled file with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  ;; temp-file never actually used for anything?
  `(let* ((temp-file (make-temp-file "python-tests" nil ".py"))
          (buffer (find-file-noselect temp-file))
          (python-indent-guess-indent-offset nil))
     (unwind-protect
         (with-current-buffer buffer
           (python-mode)
           (insert ,contents)
           (goto-char (point-min))
           ,@body)
       (and buffer (kill-buffer buffer))
       (delete-file temp-file))))

(defun python-tests-look-at (string &optional num restore-point)
  "Move point at beginning of STRING in the current buffer.
Optional argument NUM defaults to 1 and is an integer indicating
how many occurrences must be found, when positive the search is
done forwards, otherwise backwards.  When RESTORE-POINT is
non-nil the point is not moved but the position found is still
returned.  When searching forward and point is already looking at
STRING, it is skipped so the next STRING occurrence is selected."
  (let* ((num (or num 1))
         (starting-point (point))
         (string (regexp-quote string))
         (search-fn (if (> num 0) #'re-search-forward #'re-search-backward))
         (deinc-fn (if (> num 0) #'1- #'1+))
         (found-point))
    (prog2
        (catch 'exit
          (while (not (= num 0))
            (when (and (> num 0)
                       (looking-at string))
              ;; Moving forward and already looking at STRING, skip it.
              (forward-char (length (match-string-no-properties 0))))
            (and (not (funcall search-fn string nil t))
                 (throw 'exit t))
            (when (> num 0)
              ;; `re-search-forward' leaves point at the end of the
              ;; occurrence, move back so point is at the beginning
              ;; instead.
              (forward-char (- (length (match-string-no-properties 0)))))
            (setq
             num (funcall deinc-fn num)
             found-point (point))))
        found-point
      (and restore-point (goto-char starting-point)))))

(defun python-tests-self-insert (char-or-str)
  "Call `self-insert-command' for chars in CHAR-OR-STR."
  (let ((chars
         (cond
          ((characterp char-or-str)
           (list char-or-str))
          ((stringp char-or-str)
           (string-to-list char-or-str))
          ((not
            (cl-remove-if #'characterp char-or-str))
           char-or-str)
          (t (error "CHAR-OR-STR must be a char, string, or list of char")))))
    (mapc
     (lambda (char)
       (let ((last-command-event char))
         (call-interactively 'self-insert-command)))
     chars)))

(defun python-tests-visible-string (&optional min max)
  "Return the buffer string excluding invisible overlays.
Argument MIN and MAX delimit the region to be returned and
default to `point-min' and `point-max' respectively."
  (let* ((min (or min (point-min)))
         (max (or max (point-max)))
         (buffer (current-buffer))
         (buffer-contents (buffer-substring-no-properties min max))
         (overlays
          (sort (overlays-in min max)
                (lambda (a b)
                  (let ((overlay-end-a (overlay-end a))
                        (overlay-end-b (overlay-end b)))
                    (> overlay-end-a overlay-end-b))))))
    (with-temp-buffer
      (insert buffer-contents)
      (dolist (overlay overlays)
        (if (overlay-get overlay 'invisible)
            (delete-region (overlay-start overlay)
                           (overlay-end overlay))))
      (buffer-substring-no-properties (point-min) (point-max)))))


;;; Tests for your tests, so you can test while you test.

(ert-deftest python-tests-look-at-1 ()
  "Test forward movement."
  (python-tests-with-temp-buffer
   "Lorem ipsum dolor sit amet, consectetur adipisicing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna
aliqua."
   (let ((expected (save-excursion
                     (dotimes (i 3)
                       (re-search-forward "et" nil t))
                     (forward-char -2)
                     (point))))
     (should (= (python-tests-look-at "et" 3 t) expected))
     ;; Even if NUM is bigger than found occurrences the point of last
     ;; one should be returned.
     (should (= (python-tests-look-at "et" 6 t) expected))
     ;; If already looking at STRING, it should skip it.
     (dotimes (i 2) (re-search-forward "et"))
     (forward-char -2)
     (should (= (python-tests-look-at "et") expected)))))

(ert-deftest python-tests-look-at-2 ()
  "Test backward movement."
  (python-tests-with-temp-buffer
   "Lorem ipsum dolor sit amet, consectetur adipisicing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna
aliqua."
   (let ((expected
          (save-excursion
            (re-search-forward "et" nil t)
            (forward-char -2)
            (point))))
     (dotimes (i 3)
       (re-search-forward "et" nil t))
     (should (= (python-tests-look-at "et" -3 t) expected))
     (should (= (python-tests-look-at "et" -6 t) expected)))))


;;; Bindings


;;; Python specialized rx


;;; Font-lock and syntax

(ert-deftest python-syntax-after-python-backspace ()
  ;; `python-indent-dedent-line-backspace' garbles syntax
  :expected-result :failed
  (python-tests-with-temp-buffer
      "\"\"\""
    (goto-char (point-max))
    (python-indent-dedent-line-backspace 1)
    (should (string= (buffer-string) "\"\""))
    (should (null (nth 3 (syntax-ppss))))))


;;; Indentation

;; See: http://www.python.org/dev/peps/pep-0008/#indentation

(ert-deftest python-indent-pep8-1 ()
  "First pep8 case."
  (python-tests-with-temp-buffer
   "# Aligned with opening delimiter
foo = long_function_name(var_one, var_two,
                         var_three, var_four)
"
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "foo = long_function_name(var_one, var_two,")
   (should (eq (car (python-indent-context)) :after-comment))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "var_three, var_four)")
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 25))))

(ert-deftest python-indent-pep8-2 ()
  "Second pep8 case."
  (python-tests-with-temp-buffer
   "# More indentation included to distinguish this from the rest.
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    print (var_one)
"
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "def long_function_name(")
   (should (eq (car (python-indent-context)) :after-comment))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "var_one, var_two, var_three,")
   (should (eq (car (python-indent-context))
               :inside-paren-newline-start-from-block))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "var_four):")
   (should (eq (car (python-indent-context))
               :inside-paren-newline-start-from-block))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "print (var_one)")
   (should (eq (car (python-indent-context))
               :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-pep8-3 ()
  "Third pep8 case."
  (python-tests-with-temp-buffer
   "# Extra indentation is not necessary.
foo = long_function_name(
  var_one, var_two,
  var_three, var_four)
"
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "foo = long_function_name(")
   (should (eq (car (python-indent-context)) :after-comment))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "var_one, var_two,")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "var_three, var_four)")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-base-case ()
  "Check base case does not trigger errors."
  (python-tests-with-temp-buffer
   "

"
   (goto-char (point-min))
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))))

(ert-deftest python-indent-after-comment-1 ()
  "The most simple after-comment case that shouldn't fail."
  (python-tests-with-temp-buffer
   "# Contents will be modified to correct indentation
class Blag(object):
    def _on_child_complete(self, child_future):
        if self.in_terminal_state():
            pass
        # We only complete when all our async children have entered a
    # terminal state. At that point, if any child failed, we fail
# with the exception with which the first child failed.
"
   (python-tests-look-at "# We only complete")
   (should (eq (car (python-indent-context)) :after-block-end))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "# terminal state")
   (should (eq (car (python-indent-context)) :after-comment))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "# with the exception")
   (should (eq (car (python-indent-context)) :after-comment))
   ;; This one indents relative to previous block, even given the fact
   ;; that it was under-indented.
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "# terminal state" -1)
   ;; It doesn't hurt to check again.
   (should (eq (car (python-indent-context)) :after-comment))
   (python-indent-line)
   (should (= (current-indentation) 8))
   (python-tests-look-at "# with the exception")
   (should (eq (car (python-indent-context)) :after-comment))
   ;; Now everything should be lined up.
   (should (= (python-indent-calculate-indentation) 8))))

(ert-deftest python-indent-after-comment-2 ()
  "Test after-comment in weird cases."
  (python-tests-with-temp-buffer
   "# Contents will be modified to correct indentation
def func(arg):
    # I don't do much
    return arg
    # This comment is badly indented because the user forced so.
    # At this line python.el wont dedent, user is always right.

comment_wins_over_ender = True

# yeah, that.
"
   (python-tests-look-at "# I don't do much")
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "return arg")
   ;; Comment here just gets ignored, this line is not a comment so
   ;; the rules won't apply here.
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "# This comment is badly indented")
   (should (eq (car (python-indent-context)) :after-block-end))
   ;; The return keyword do make indentation lose a level...
   (should (= (python-indent-calculate-indentation) 0))
   ;; ...but the current indentation was forced by the user.
   (python-tests-look-at "# At this line python.el wont dedent")
   (should (eq (car (python-indent-context)) :after-comment))
   (should (= (python-indent-calculate-indentation) 4))
   ;; Should behave the same for blank lines: potentially a comment.
   (forward-line 1)
   (should (eq (car (python-indent-context)) :after-comment))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "comment_wins_over_ender")
   ;; The comment won over the ender because the user said so.
   (should (eq (car (python-indent-context)) :after-comment))
   (should (= (python-indent-calculate-indentation) 4))
   ;; The indentation calculated fine for the assignment, but the user
   ;; choose to force it back to the first column.  Next line should
   ;; be aware of that.
   (python-tests-look-at "# yeah, that.")
   (should (eq (car (python-indent-context)) :after-line))
   (should (= (python-indent-calculate-indentation) 0))))

(ert-deftest python-indent-after-comment-3 ()
  "Test after-comment in buggy case."
  (python-tests-with-temp-buffer
   "
class A(object):

    def something(self, arg):
        if True:
            return arg

    # A comment

    @adecorator
    def method(self, a, b):
        pass
"
   (python-tests-look-at "@adecorator")
   (should (eq (car (python-indent-context)) :after-comment))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-inside-paren-1 ()
  "The most simple inside-paren case that shouldn't fail."
  (python-tests-with-temp-buffer
   "
data = {
    'key':
    {
        'objlist': [
            {
                'pk': 1,
                'name': 'first',
            },
            {
                'pk': 2,
                'name': 'second',
            }
        ]
    }
}
"
   (python-tests-look-at "data = {")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "'key':")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "{")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "'objlist': [")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "{")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 12))
   (python-tests-look-at "'pk': 1,")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 16))
   (python-tests-look-at "'name': 'first',")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 16))
   (python-tests-look-at "},")
   (should (eq (car (python-indent-context))
               :inside-paren-at-closing-nested-paren))
   (should (= (python-indent-calculate-indentation) 12))
   (python-tests-look-at "{")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 12))
   (python-tests-look-at "'pk': 2,")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 16))
   (python-tests-look-at "'name': 'second',")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 16))
   (python-tests-look-at "}")
   (should (eq (car (python-indent-context))
               :inside-paren-at-closing-nested-paren))
   (should (= (python-indent-calculate-indentation) 12))
   (python-tests-look-at "]")
   (should (eq (car (python-indent-context))
               :inside-paren-at-closing-nested-paren))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "}")
   (should (eq (car (python-indent-context))
               :inside-paren-at-closing-nested-paren))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "}")
   (should (eq (car (python-indent-context)) :inside-paren-at-closing-paren))
   (should (= (python-indent-calculate-indentation) 0))))

(ert-deftest python-indent-inside-paren-2 ()
  "Another more compact paren group style."
  (python-tests-with-temp-buffer
   "
data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}
"
   (python-tests-look-at "data = {")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "'objlist': [")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "{'pk': 1,")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "'name': 'first'},")
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 9))
   (python-tests-look-at "{'pk': 2,")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "'name': 'second'}")
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 9))
   (python-tests-look-at "]")
   (should (eq (car (python-indent-context))
               :inside-paren-at-closing-nested-paren))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "}}")
   (should (eq (car (python-indent-context))
               :inside-paren-at-closing-nested-paren))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "}")
   (should (eq (car (python-indent-context)) :inside-paren-at-closing-paren))
   (should (= (python-indent-calculate-indentation) 0))))

(ert-deftest python-indent-inside-paren-3 ()
  "The simplest case possible."
  (python-tests-with-temp-buffer
   "
data = ('these',
        'are',
        'the',
        'tokens')
"
   (python-tests-look-at "data = ('these',")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 8))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 8))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 8))))

(ert-deftest python-indent-inside-paren-4 ()
  "Respect indentation of first column."
  (python-tests-with-temp-buffer
   "
data = [ [ 'these', 'are'],
         ['the', 'tokens' ] ]
"
   (python-tests-look-at "data = [ [ 'these', 'are'],")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 9))))

(ert-deftest python-indent-inside-paren-5 ()
  "Test when :inside-paren initial parens are skipped in context start."
  (python-tests-with-temp-buffer
   "
while ((not some_condition) and
       another_condition):
    do_something_interesting(
        with_some_arg)
"
   (python-tests-look-at "while ((not some_condition) and")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 7))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 8))))

(ert-deftest python-indent-inside-paren-6 ()
  "This should be aligned.."
  (python-tests-with-temp-buffer
   "
CHOICES = (('some', 'choice'),
           ('another', 'choice'),
           ('more', 'choices'))
"
   (python-tests-look-at "CHOICES = (('some', 'choice'),")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 11))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 11))))

(ert-deftest python-indent-inside-paren-7 ()
  "Test for Bug#21762."
  (python-tests-with-temp-buffer
   "import re as myre\nvar = [\n"
   (goto-char (point-max))
   ;; This signals an error if the test fails
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))))

(ert-deftest python-indent-after-block-1 ()
  "The most simple after-block case that shouldn't fail."
  (python-tests-with-temp-buffer
   "
def foo(a, b, c=True):
"
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (goto-char (point-max))
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-after-block-2 ()
  "A weird (malformed) multiline block statement."
  (python-tests-with-temp-buffer
   "
def foo(a, b, c={
    'a':
}):
"
   (goto-char (point-max))
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-after-block-3 ()
  "A weird (malformed) sample, usually found in python shells."
  (python-tests-with-temp-buffer
   "
In [1]:
def func():
pass

In [2]:
something
"
   (python-tests-look-at "pass")
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "something")
   (end-of-line)
   (should (eq (car (python-indent-context)) :after-line))
   (should (= (python-indent-calculate-indentation) 0))))

(ert-deftest python-indent-after-async-block-1 ()
  "Test PEP492 async def."
  (python-tests-with-temp-buffer
   "
async def foo(a, b, c=True):
"
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (goto-char (point-max))
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-after-async-block-2 ()
  "Test PEP492 async with."
  (python-tests-with-temp-buffer
   "
async with foo(a) as mgr:
"
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (goto-char (point-max))
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-after-async-block-3 ()
  "Test PEP492 async for."
  (python-tests-with-temp-buffer
   "
async for a in sequencer():
"
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (goto-char (point-max))
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-after-backslash-1 ()
  "The most common case."
  (python-tests-with-temp-buffer
   "
from foo.bar.baz import something, something_1 \\
    something_2 something_3, \\
    something_4, something_5
"
   (python-tests-look-at "from foo.bar.baz import something, something_1")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "something_2 something_3,")
   (should (eq (car (python-indent-context)) :after-backslash-first-line))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "something_4, something_5")
   (should (eq (car (python-indent-context)) :after-backslash))
   (should (= (python-indent-calculate-indentation) 4))
   (goto-char (point-max))
   (should (eq (car (python-indent-context)) :after-line))
   (should (= (python-indent-calculate-indentation) 0))))

(ert-deftest python-indent-after-backslash-2 ()
  "A pretty extreme complicated case."
  (python-tests-with-temp-buffer
   "
objects = Thing.objects.all() \\
                       .filter(
                           type='toy',
                           status='bought'
                       ) \\
                       .aggregate(
                           Sum('amount')
                       ) \\
                       .values_list()
"
   (python-tests-look-at "objects = Thing.objects.all()")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at ".filter(")
   (should (eq (car (python-indent-context))
               :after-backslash-dotted-continuation))
   (should (= (python-indent-calculate-indentation) 23))
   (python-tests-look-at "type='toy',")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 27))
   (python-tests-look-at "status='bought'")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 27))
   (python-tests-look-at ") \\")
   (should (eq (car (python-indent-context)) :inside-paren-at-closing-paren))
   (should (= (python-indent-calculate-indentation) 23))
   (python-tests-look-at ".aggregate(")
   (should (eq (car (python-indent-context))
               :after-backslash-dotted-continuation))
   (should (= (python-indent-calculate-indentation) 23))
   (python-tests-look-at "Sum('amount')")
   (should (eq (car (python-indent-context)) :inside-paren-newline-start))
   (should (= (python-indent-calculate-indentation) 27))
   (python-tests-look-at ") \\")
   (should (eq (car (python-indent-context)) :inside-paren-at-closing-paren))
   (should (= (python-indent-calculate-indentation) 23))
   (python-tests-look-at ".values_list()")
   (should (eq (car (python-indent-context))
               :after-backslash-dotted-continuation))
   (should (= (python-indent-calculate-indentation) 23))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :after-line))
   (should (= (python-indent-calculate-indentation) 0))))

(ert-deftest python-indent-after-backslash-3 ()
  "Backslash continuation from block start."
  (python-tests-with-temp-buffer
   "
with open('/path/to/some/file/you/want/to/read') as file_1, \\
     open('/path/to/some/file/being/written', 'w') as file_2:
    file_2.write(file_1.read())
"
   (python-tests-look-at
    "with open('/path/to/some/file/you/want/to/read') as file_1, \\")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at
    "open('/path/to/some/file/being/written', 'w') as file_2")
   (should (eq (car (python-indent-context))
               :after-backslash-block-continuation))
   (should (= (python-indent-calculate-indentation) 5))
   (python-tests-look-at "file_2.write(file_1.read())")
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-after-backslash-4 ()
  "Backslash continuation from assignment."
  (python-tests-with-temp-buffer
   "
super_awful_assignment = some_calculation() and \\
    another_calculation() and \\
    some_final_calculation()
"
   (python-tests-look-at
    "super_awful_assignment = some_calculation() and \\")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "another_calculation() and \\")
   (should (eq (car (python-indent-context))
               :after-backslash-assignment-continuation))
   (should (= (python-indent-calculate-indentation) python-indent-offset))
   (python-tests-look-at "some_final_calculation()")
   (should (eq (car (python-indent-context)) :after-backslash))
   (should (= (python-indent-calculate-indentation) python-indent-offset))))

(ert-deftest python-indent-after-backslash-5 ()
  "Dotted continuation bizarre example."
  (python-tests-with-temp-buffer
   "
def delete_all_things():
    Thing \\
        .objects.all() \\
                .delete()
"
   (python-tests-look-at "Thing \\")
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at ".objects.all() \\")
   (should (eq (car (python-indent-context)) :after-backslash-first-line))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at ".delete()")
   (should (eq (car (python-indent-context))
               :after-backslash-dotted-continuation))
   (should (= (python-indent-calculate-indentation) 16))))

(ert-deftest python-indent-block-enders-1 ()
  "Test de-indentation for pass keyword."
  (python-tests-with-temp-buffer
   "
Class foo(object):

    def bar(self):
        if self.baz:
            return (1,
                    2,
                    3)

        else:
            pass
"
   (python-tests-look-at "3)")
   (forward-line 1)
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "pass")
   (forward-line 1)
   (should (eq (car (python-indent-context)) :after-block-end))
   (should (= (python-indent-calculate-indentation) 8))))

(ert-deftest python-indent-block-enders-2 ()
  "Test de-indentation for return keyword."
  (python-tests-with-temp-buffer
   "
Class foo(object):
    '''raise lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do

    eiusmod tempor incididunt ut labore et dolore magna aliqua.
    '''
    def bar(self):
        \"return (1, 2, 3).\"
        if self.baz:
            return (1,
                    2,
                    3)
"
   (python-tests-look-at "def")
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "if")
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "return")
   (should (= (python-indent-calculate-indentation) 12))
   (goto-char (point-max))
   (should (eq (car (python-indent-context)) :after-block-end))
   (should (= (python-indent-calculate-indentation) 8))))

(ert-deftest python-indent-block-enders-3 ()
  "Test de-indentation for continue keyword."
  (python-tests-with-temp-buffer
   "
for element in lst:
    if element is None:
        continue
"
   (python-tests-look-at "if")
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "continue")
   (should (= (python-indent-calculate-indentation) 8))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :after-block-end))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-block-enders-4 ()
  "Test de-indentation for break keyword."
  (python-tests-with-temp-buffer
   "
for element in lst:
    if element is None:
        break
"
   (python-tests-look-at "if")
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "break")
   (should (= (python-indent-calculate-indentation) 8))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :after-block-end))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-block-enders-5 ()
  "Test de-indentation for raise keyword."
  (python-tests-with-temp-buffer
   "
for element in lst:
    if element is None:
        raise ValueError('Element cannot be None')
"
   (python-tests-look-at "if")
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "raise")
   (should (= (python-indent-calculate-indentation) 8))
   (forward-line 1)
   (should (eq (car (python-indent-context)) :after-block-end))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-dedenters-1 ()
  "Test de-indentation for the elif keyword."
  (python-tests-with-temp-buffer
   "
if save:
    try:
        write_to_disk(data)
    finally:
        cleanup()
        elif
"
   (python-tests-look-at "elif\n")
   (should (eq (car (python-indent-context)) :at-dedenter-block-start))
   (should (= (python-indent-calculate-indentation) 0))
   (should (= (python-indent-calculate-indentation t) 0))))

(ert-deftest python-indent-dedenters-2 ()
  "Test de-indentation for the else keyword."
  (python-tests-with-temp-buffer
   "
if save:
    try:
        write_to_disk(data)
    except IOError:
        msg = 'Error saving to disk'
        message(msg)
        logger.exception(msg)
    except Exception:
        if hide_details:
            logger.exception('Unhandled exception')
            else
    finally:
        data.free()
"
   (python-tests-look-at "else\n")
   (should (eq (car (python-indent-context)) :at-dedenter-block-start))
   (should (= (python-indent-calculate-indentation) 8))
   (python-indent-line t)
   (should (= (python-indent-calculate-indentation t) 4))
   (python-indent-line t)
   (should (= (python-indent-calculate-indentation t) 0))
   (python-indent-line t)
   (should (= (python-indent-calculate-indentation t) 8))))

(ert-deftest python-indent-dedenters-3 ()
  "Test de-indentation for the except keyword."
  (python-tests-with-temp-buffer
   "
if save:
    try:
        write_to_disk(data)
        except
"
   (python-tests-look-at "except\n")
   (should (eq (car (python-indent-context)) :at-dedenter-block-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-indent-line t)
   (should (= (python-indent-calculate-indentation t) 4))))

(ert-deftest python-indent-dedenters-4 ()
  "Test de-indentation for the finally keyword."
  (python-tests-with-temp-buffer
   "
if save:
    try:
        write_to_disk(data)
        finally
"
   (python-tests-look-at "finally\n")
   (should (eq (car (python-indent-context)) :at-dedenter-block-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-indent-line t)
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-dedenters-5 ()
  "Test invalid levels are skipped in a complex example."
  (python-tests-with-temp-buffer
   "
if save:
    try:
        write_to_disk(data)
    except IOError:
        msg = 'Error saving to disk'
        message(msg)
        logger.exception(msg)
    finally:
        if cleanup:
            do_cleanup()
        else
"
   (python-tests-look-at "else\n")
   (should (eq (car (python-indent-context)) :at-dedenter-block-start))
   (should (= (python-indent-calculate-indentation) 8))
   (should (= (python-indent-calculate-indentation t) 0))
   (python-indent-line t)
   (should (= (python-indent-calculate-indentation t) 8))))

(ert-deftest python-indent-dedenters-6 ()
  "Test indentation is zero when no opening block for dedenter."
  (python-tests-with-temp-buffer
   "
try:
    # if save:
        write_to_disk(data)
        else
"
   (python-tests-look-at "else\n")
   (should (eq (car (python-indent-context)) :at-dedenter-block-start))
   (should (= (python-indent-calculate-indentation) 0))
   (should (= (python-indent-calculate-indentation t) 0))))

(ert-deftest python-indent-dedenters-7 ()
  "Test indentation case from Bug#15163."
  (python-tests-with-temp-buffer
   "
if a:
    if b:
        pass
    else:
        pass
        else:
"
   (python-tests-look-at "else:" 2)
   (should (eq (car (python-indent-context)) :at-dedenter-block-start))
   (should (= (python-indent-calculate-indentation) 0))
   (should (= (python-indent-calculate-indentation t) 0))))

(ert-deftest python-indent-dedenters-8 ()
  "Test indentation for Bug#18432."
  (python-tests-with-temp-buffer
   "
if (a == 1 or
    a == 2):
    pass
elif (a == 3 or
a == 4):
"
   (python-tests-look-at "elif (a == 3 or")
   (should (eq (car (python-indent-context)) :at-dedenter-block-start))
   (should (= (python-indent-calculate-indentation) 0))
   (should (= (python-indent-calculate-indentation t) 0))
   (python-tests-look-at "a == 4):\n")
   (should (eq (car (python-indent-context)) :inside-paren))
   (should (= (python-indent-calculate-indentation) 6))
   (python-indent-line)
   (should (= (python-indent-calculate-indentation t) 4))
   (python-indent-line t)
   (should (= (python-indent-calculate-indentation t) 0))
   (python-indent-line t)
   (should (= (python-indent-calculate-indentation t) 6))))

(ert-deftest python-indent-inside-string-1 ()
  "Test indentation for strings."
  (python-tests-with-temp-buffer
   "
multiline = '''
bunch
of
lines
'''
"
   (python-tests-look-at "multiline = '''")
   (should (eq (car (python-indent-context)) :no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "bunch")
   (should (eq (car (python-indent-context)) :inside-string))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "of")
   (should (eq (car (python-indent-context)) :inside-string))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "lines")
   (should (eq (car (python-indent-context)) :inside-string))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "'''")
   (should (eq (car (python-indent-context)) :inside-string))
   (should (= (python-indent-calculate-indentation) 0))))

(ert-deftest python-indent-inside-string-2 ()
  "Test indentation for docstrings."
  (python-tests-with-temp-buffer
   "
def fn(a, b, c=True):
    '''docstring
    bunch
        of
    lines
    '''
"
   (python-tests-look-at "'''docstring")
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "bunch")
   (should (eq (car (python-indent-context)) :inside-docstring))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "of")
   (should (eq (car (python-indent-context)) :inside-docstring))
   ;; Any indentation deeper than the base-indent must remain unmodified.
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "lines")
   (should (eq (car (python-indent-context)) :inside-docstring))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "'''")
   (should (eq (car (python-indent-context)) :inside-docstring))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-inside-string-3 ()
  "Test indentation for nested strings."
  (python-tests-with-temp-buffer
   "
def fn(a, b, c=True):
    some_var = '''
    bunch
    of
    lines
    '''
"
   (python-tests-look-at "some_var = '''")
   (should (eq (car (python-indent-context)) :after-block-start))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "bunch")
   (should (eq (car (python-indent-context)) :inside-string))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "of")
   (should (eq (car (python-indent-context)) :inside-string))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "lines")
   (should (eq (car (python-indent-context)) :inside-string))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "'''")
   (should (eq (car (python-indent-context)) :inside-string))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-electric-comma-inside-multiline-string ()
  "Test indentation ...."
  (python-tests-with-temp-buffer
   "
a = (
    '''\
- foo,
- bar
'''
"
   (python-tests-look-at "- bar")
   (should (eq (car (python-indent-context)) :inside-string))
   (goto-char (line-end-position))
   (python-tests-self-insert ",")
   (should (= (current-indentation) 0))))

(ert-deftest python-indent-electric-comma-after-multiline-string ()
  "Test indentation ...."
  (python-tests-with-temp-buffer
   "
a = (
    '''\
- foo,
- bar'''
"
   (python-tests-look-at "- bar'''")
   (should (eq (car (python-indent-context)) :inside-string))
   (goto-char (line-end-position))
   (python-tests-self-insert ",")
   (should (= (current-indentation) 0))))

(ert-deftest python-indent-electric-colon-1 ()
  "Test indentation case from Bug#18228."
  (python-tests-with-temp-buffer
   "
def a():
    pass

def b()
"
   (python-tests-look-at "def b()")
   (goto-char (line-end-position))
   (python-tests-self-insert ":")
   (should (= (current-indentation) 0))))

(ert-deftest python-indent-electric-colon-2 ()
  "Test indentation case for dedenter."
  (python-tests-with-temp-buffer
   "
if do:
    something()
    else
"
   (python-tests-look-at "else")
   (goto-char (line-end-position))
   (python-tests-self-insert ":")
   (should (= (current-indentation) 0))))

(ert-deftest python-indent-electric-colon-3 ()
  "Test indentation case for multi-line dedenter."
  (python-tests-with-temp-buffer
   "
if do:
    something()
    elif (this
          and
          that)
"
   (python-tests-look-at "that)")
   (goto-char (line-end-position))
   (python-tests-self-insert ":")
   (python-tests-look-at "elif" -1)
   (should (= (current-indentation) 0))
   (python-tests-look-at "and")
   (should (= (current-indentation) 6))
   (python-tests-look-at "that)")
   (should (= (current-indentation) 6))))

(ert-deftest python-indent-electric-colon-4 ()
  "Test indentation case where there is one more-indented previous open block."
  (python-tests-with-temp-buffer
   "
def f():
    if True:
        a = 5

        if True:
            a = 10

        b = 3

else
"
   (python-tests-look-at "else")
   (goto-char (line-end-position))
   (python-tests-self-insert ":")
   (python-tests-look-at "else" -1)
   (should (= (current-indentation) 4))))

(ert-deftest python-indent-region-1 ()
  "Test indentation case from Bug#18843."
  (let ((contents "
def foo ():
    try:
        pass
    except:
        pass
"))
    (python-tests-with-temp-buffer
     contents
     (python-indent-region (point-min) (point-max))
     (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      contents)))))

(ert-deftest python-indent-region-2 ()
  "Test region indentation on comments."
  (let ((contents "
def f():
    if True:
        pass

# This is
# some multiline
# comment
"))
    (python-tests-with-temp-buffer
     contents
     (python-indent-region (point-min) (point-max))
     (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      contents)))))

(ert-deftest python-indent-region-3 ()
  "Test region indentation on comments."
  (let ((contents "
def f():
    if True:
        pass
# This is
# some multiline
# comment
")
        (expected "
def f():
    if True:
        pass
    # This is
    # some multiline
    # comment
"))
    (python-tests-with-temp-buffer
     contents
     (python-indent-region (point-min) (point-max))
     (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      expected)))))

(ert-deftest python-indent-region-4 ()
  "Test region indentation block starts, dedenters and enders."
  (let ((contents "
def f():
    if True:
a  = 5
    else:
            a = 10
    return a
")
        (expected "
def f():
    if True:
        a  = 5
    else:
        a = 10
    return a
"))
    (python-tests-with-temp-buffer
     contents
     (python-indent-region (point-min) (point-max))
     (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      expected)))))

(ert-deftest python-indent-region-5 ()
  "Test region indentation for docstrings."
  (let ((contents "
def f():
'''
this is
        a multiline
string
'''
    x = \\
        '''
this is an arbitrarily
    indented multiline
 string
'''
")
        (expected "
def f():
    '''
    this is
        a multiline
    string
    '''
    x = \\
        '''
this is an arbitrarily
    indented multiline
 string
'''
"))
    (python-tests-with-temp-buffer
     contents
     (python-indent-region (point-min) (point-max))
     (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      expected)))))


;;; Mark

(ert-deftest python-mark-defun-1 ()
  """Test `python-mark-defun' with point at defun symbol start."""
  (python-tests-with-temp-buffer
   "
def foo(x):
    return x

class A:
   pass

class B:

    def __init__(self):
       self.b = 'b'

    def fun(self):
       return self.b

class C:
   '''docstring'''
"
   (let ((transient-mark-mode t)
         (expected-mark-beginning-position
          (progn
            (python-tests-look-at "class A:")
            (1- (point))))
         (expected-mark-end-position-1
          (save-excursion
            (python-tests-look-at "pass")
            (forward-line)
            (point)))
         (expected-mark-end-position-2
          (save-excursion
            (python-tests-look-at "return self.b")
            (forward-line)
            (point)))
         (expected-mark-end-position-3
          (save-excursion
            (python-tests-look-at "'''docstring'''")
            (forward-line)
            (point))))
     ;; Select class A only, with point at bol.
     (python-mark-defun 1)
     (should (= (point) expected-mark-beginning-position))
     (should (= (marker-position (mark-marker))
                expected-mark-end-position-1))
     ;; expand to class B, start position should remain the same.
     (python-mark-defun 1)
     (should (= (point) expected-mark-beginning-position))
     (should (= (marker-position (mark-marker))
                expected-mark-end-position-2))
     ;; expand to class C, start position should remain the same.
     (python-mark-defun 1)
     (should (= (point) expected-mark-beginning-position))
     (should (= (marker-position (mark-marker))
                expected-mark-end-position-3)))))

(ert-deftest python-mark-defun-2 ()
  """Test `python-mark-defun' with point at nested defun symbol start."""
  (python-tests-with-temp-buffer
   "
def foo(x):
    return x

class A:
   pass

class B:

    def __init__(self):
       self.b = 'b'

    def fun(self):
       return self.b

class C:
   '''docstring'''
"
   (let ((transient-mark-mode t)
         (expected-mark-beginning-position
          (progn
            (python-tests-look-at "def __init__(self):")
            (1- (line-beginning-position))))
         (expected-mark-end-position-1
          (save-excursion
            (python-tests-look-at "self.b = 'b'")
            (forward-line)
            (point)))
         (expected-mark-end-position-2
          (save-excursion
            (python-tests-look-at "return self.b")
            (forward-line)
            (point)))
         (expected-mark-end-position-3
          (save-excursion
            (python-tests-look-at "'''docstring'''")
            (forward-line)
            (point))))
     ;; Select B.__init only, with point at its start.
     (python-mark-defun 1)
     (should (= (point) expected-mark-beginning-position))
     (should (= (marker-position (mark-marker))
                expected-mark-end-position-1))
     ;; expand to B.fun, start position should remain the same.
     (python-mark-defun 1)
     (should (= (point) expected-mark-beginning-position))
     (should (= (marker-position (mark-marker))
                expected-mark-end-position-2))
     ;; expand to class C, start position should remain the same.
     (python-mark-defun 1)
     (should (= (point) expected-mark-beginning-position))
     (should (= (marker-position (mark-marker))
                expected-mark-end-position-3)))))

(ert-deftest python-mark-defun-3 ()
  """Test `python-mark-defun' with point inside defun symbol."""
  (python-tests-with-temp-buffer
   "
def foo(x):
    return x

class A:
   pass

class B:

    def __init__(self):
       self.b = 'b'

    def fun(self):
       return self.b

class C:
   '''docstring'''
"
   (let ((expected-mark-beginning-position
          (progn
            (python-tests-look-at "def fun(self):")
            (python-tests-look-at "(self):")
            (1- (line-beginning-position))))
         (expected-mark-end-position
          (save-excursion
            (python-tests-look-at "return self.b")
            (forward-line)
            (point))))
     ;; Should select B.fun, despite point is inside the defun symbol.
     (python-mark-defun 1)
     (should (= (point) expected-mark-beginning-position))
     (should (= (marker-position (mark-marker))
                expected-mark-end-position)))))


;;; Navigation

(ert-deftest python-nav-beginning-of-defun-1 ()
  (python-tests-with-temp-buffer
   "
def decoratorFunctionWithArguments(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decoratorFunctionWithArguments('arg1', 'arg2')
    def func(a, b, c=True):
        pass
    '''

    def wwrap(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap
"
   (python-tests-look-at "return wrap")
   (should (= (save-excursion
                (python-nav-beginning-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "def wrapped_f(*args):" -1)
                (beginning-of-line)
                (point))))
   (python-tests-look-at "def wrapped_f(*args):" -1)
   (should (= (save-excursion
                (python-nav-beginning-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "def wwrap(f):" -1)
                (beginning-of-line)
                (point))))
   (python-tests-look-at "def wwrap(f):" -1)
   (should (= (save-excursion
                (python-nav-beginning-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "def decoratorFunctionWithArguments" -1)
                (beginning-of-line)
                (point))))))

(ert-deftest python-nav-beginning-of-defun-2 ()
  (python-tests-with-temp-buffer
   "
class C(object):

    def m(self):
        self.c()

        def b():
            pass

        def a():
            pass

    def c(self):
        pass
"
   ;; Nested defuns, are handled with care.
   (python-tests-look-at "def c(self):")
   (should (= (save-excursion
                (python-nav-beginning-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "def m(self):" -1)
                (beginning-of-line)
                (point))))
   ;; Defuns on same levels should be respected.
   (python-tests-look-at "def a():" -1)
   (should (= (save-excursion
                (python-nav-beginning-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "def b():" -1)
                (beginning-of-line)
                (point))))
   ;; Jump to a top level defun.
   (python-tests-look-at "def b():" -1)
   (should (= (save-excursion
                (python-nav-beginning-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "def m(self):" -1)
                (beginning-of-line)
                (point))))
   ;; Jump to a top level defun again.
   (python-tests-look-at "def m(self):" -1)
   (should (= (save-excursion
                (python-nav-beginning-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "class C(object):" -1)
                (beginning-of-line)
                (point))))))

(ert-deftest python-nav-beginning-of-defun-3 ()
  (python-tests-with-temp-buffer
   "
class C(object):

    async def m(self):
        return await self.c()

    async def c(self):
        pass
"
   (python-tests-look-at "self.c()")
   (should (= (save-excursion
                (python-nav-beginning-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "async def m" -1)
                (beginning-of-line)
                (point))))))

(ert-deftest python-nav-end-of-defun-1 ()
  (python-tests-with-temp-buffer
   "
class C(object):

    def m(self):
        self.c()

        def b():
            pass

        def a():
            pass

    def c(self):
        pass
"
   (should (= (save-excursion
                (python-tests-look-at "class C(object):")
                (python-nav-end-of-defun)
                (point))
              (save-excursion
                (point-max))))
   (should (= (save-excursion
                (python-tests-look-at "def m(self):")
                (python-nav-end-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "def c(self):")
                (forward-line -1)
                (point))))
   (should (= (save-excursion
                (python-tests-look-at "def b():")
                (python-nav-end-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "def b():")
                (forward-line 2)
                (point))))
   (should (= (save-excursion
                (python-tests-look-at "def c(self):")
                (python-nav-end-of-defun)
                (point))
              (save-excursion
                (point-max))))))

(ert-deftest python-nav-end-of-defun-2 ()
  (python-tests-with-temp-buffer
   "
def decoratorFunctionWithArguments(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decoratorFunctionWithArguments('arg1', 'arg2')
    def func(a, b, c=True):
        pass
    '''

    def wwrap(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap
"
   (should (= (save-excursion
                (python-tests-look-at "def decoratorFunctionWithArguments")
                (python-nav-end-of-defun)
                (point))
              (save-excursion
                (point-max))))
   (should (= (save-excursion
                (python-tests-look-at "@decoratorFunctionWithArguments")
                (python-nav-end-of-defun)
                (point))
              (save-excursion
                (point-max))))
   (should (= (save-excursion
                (python-tests-look-at "def wwrap(f):")
                (python-nav-end-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "return wwrap")
                (line-beginning-position))))
   (should (= (save-excursion
                (python-tests-look-at "def wrapped_f(*args):")
                (python-nav-end-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "return wrapped_f")
                (line-beginning-position))))
   (should (= (save-excursion
                (python-tests-look-at "f(*args)")
                (python-nav-end-of-defun)
                (point))
              (save-excursion
                (python-tests-look-at "return wrapped_f")
                (line-beginning-position))))))

(ert-deftest python-nav-backward-defun-1 ()
  (python-tests-with-temp-buffer
   "
class A(object): # A

    def a(self): # a
        pass

    def b(self): # b
        pass

    class B(object): # B

        class C(object): # C

            def d(self): # d
                pass

            # def e(self): # e
            #     pass

    def c(self): # c
        pass

    # def d(self): # d
    #     pass
"
   (goto-char (point-max))
   (should (= (save-excursion (python-nav-backward-defun))
              (python-tests-look-at "    def c(self): # c" -1)))
   (should (= (save-excursion (python-nav-backward-defun))
              (python-tests-look-at "            def d(self): # d" -1)))
   (should (= (save-excursion (python-nav-backward-defun))
              (python-tests-look-at "        class C(object): # C" -1)))
   (should (= (save-excursion (python-nav-backward-defun))
              (python-tests-look-at "    class B(object): # B" -1)))
   (should (= (save-excursion (python-nav-backward-defun))
              (python-tests-look-at "    def b(self): # b" -1)))
   (should (= (save-excursion (python-nav-backward-defun))
              (python-tests-look-at "    def a(self): # a" -1)))
   (should (= (save-excursion (python-nav-backward-defun))
              (python-tests-look-at "class A(object): # A" -1)))
   (should (not (python-nav-backward-defun)))))

(ert-deftest python-nav-backward-defun-2 ()
  (python-tests-with-temp-buffer
   "
def decoratorFunctionWithArguments(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decoratorFunctionWithArguments('arg1', 'arg2')
    def func(a, b, c=True):
        pass
    '''

    def wwrap(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap
"
   (goto-char (point-max))
   (should (= (save-excursion (python-nav-backward-defun))
              (python-tests-look-at "        def wrapped_f(*args):" -1)))
   (should (= (save-excursion (python-nav-backward-defun))
              (python-tests-look-at "    def wwrap(f):" -1)))
   (should (= (save-excursion (python-nav-backward-defun))
              (python-tests-look-at "def decoratorFunctionWithArguments(arg1, arg2, arg3):" -1)))
   (should (not (python-nav-backward-defun)))))

(ert-deftest python-nav-backward-defun-3 ()
  (python-tests-with-temp-buffer
   "
'''
    def u(self):
        pass

    def v(self):
        pass

    def w(self):
        pass
'''

class A(object):
    pass
"
   (goto-char (point-min))
   (let ((point (python-tests-look-at "class A(object):")))
     (should (not (python-nav-backward-defun)))
     (should (= point (point))))))

(ert-deftest python-nav-forward-defun-1 ()
  (python-tests-with-temp-buffer
   "
class A(object): # A

    def a(self): # a
        pass

    def b(self): # b
        pass

    class B(object): # B

        class C(object): # C

            def d(self): # d
                pass

            # def e(self): # e
            #     pass

    def c(self): # c
        pass

    # def d(self): # d
    #     pass
"
   (goto-char (point-min))
   (should (= (save-excursion (python-nav-forward-defun))
              (python-tests-look-at "(object): # A")))
   (should (= (save-excursion (python-nav-forward-defun))
              (python-tests-look-at "(self): # a")))
   (should (= (save-excursion (python-nav-forward-defun))
              (python-tests-look-at "(self): # b")))
   (should (= (save-excursion (python-nav-forward-defun))
              (python-tests-look-at "(object): # B")))
   (should (= (save-excursion (python-nav-forward-defun))
              (python-tests-look-at "(object): # C")))
   (should (= (save-excursion (python-nav-forward-defun))
              (python-tests-look-at "(self): # d")))
   (should (= (save-excursion (python-nav-forward-defun))
              (python-tests-look-at "(self): # c")))
   (should (not (python-nav-forward-defun)))))

(ert-deftest python-nav-forward-defun-2 ()
  (python-tests-with-temp-buffer
   "
def decoratorFunctionWithArguments(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decoratorFunctionWithArguments('arg1', 'arg2')
    def func(a, b, c=True):
        pass
    '''

    def wwrap(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap
"
   (goto-char (point-min))
   (should (= (save-excursion (python-nav-forward-defun))
              (python-tests-look-at "(arg1, arg2, arg3):")))
   (should (= (save-excursion (python-nav-forward-defun))
              (python-tests-look-at "(f):")))
   (should (= (save-excursion (python-nav-forward-defun))
              (python-tests-look-at "(*args):")))
   (should (not (python-nav-forward-defun)))))

(ert-deftest python-nav-forward-defun-3 ()
  (python-tests-with-temp-buffer
   "
class A(object):
    pass

'''
    def u(self):
        pass

    def v(self):
        pass

    def w(self):
        pass
'''
"
   (goto-char (point-min))
   (let ((point (python-tests-look-at "(object):")))
     (should (not (python-nav-forward-defun)))
     (should (= point (point))))))

(ert-deftest python-nav-beginning-of-statement-1 ()
  (python-tests-with-temp-buffer
   "
v1 = 123 + \\
     456 + \\
     789
v2 = (value1,
      value2,

      value3,
      value4)
v3 = ('this is a string'

      'that is continued'
      'between lines'
      'within a paren',
      # this is a comment, yo
      'continue previous line')
v4 = '''
a very long
string
'''
"
   (python-tests-look-at "v2 =")
   (python-util-forward-comment -1)
   (should (= (save-excursion
                (python-nav-beginning-of-statement)
                (point))
              (python-tests-look-at "v1 =" -1 t)))
   (python-tests-look-at "v3 =")
   (python-util-forward-comment -1)
   (should (= (save-excursion
                (python-nav-beginning-of-statement)
                (point))
              (python-tests-look-at "v2 =" -1 t)))
   (python-tests-look-at "v4 =")
   (python-util-forward-comment -1)
   (should (= (save-excursion
                (python-nav-beginning-of-statement)
                (point))
              (python-tests-look-at "v3 =" -1 t)))
   (goto-char (point-max))
   (python-util-forward-comment -1)
   (should (= (save-excursion
                (python-nav-beginning-of-statement)
                (point))
              (python-tests-look-at "v4 =" -1 t)))))

(ert-deftest python-nav-end-of-statement-1 ()
  (python-tests-with-temp-buffer
   "
v1 = 123 + \\
     456 + \\
     789
v2 = (value1,
      value2,

      value3,
      value4)
v3 = ('this is a string'

      'that is continued'
      'between lines'
      'within a paren',
      # this is a comment, yo
      'continue previous line')
v4 = '''
a very long
string
'''
"
   (python-tests-look-at "v1 =")
   (should (= (save-excursion
                (python-nav-end-of-statement)
                (point))
              (save-excursion
                (python-tests-look-at "789")
                (line-end-position))))
   (python-tests-look-at "v2 =")
   (should (= (save-excursion
                (python-nav-end-of-statement)
                (point))
              (save-excursion
                (python-tests-look-at "value4)")
                (line-end-position))))
   (python-tests-look-at "v3 =")
   (should (= (save-excursion
                (python-nav-end-of-statement)
                (point))
              (save-excursion
                (python-tests-look-at
                 "'continue previous line')")
                (line-end-position))))
   (python-tests-look-at "v4 =")
   (should (= (save-excursion
                (python-nav-end-of-statement)
                (point))
              (save-excursion
                (goto-char (point-max))
                (python-util-forward-comment -1)
                (point))))))

(ert-deftest python-nav-forward-statement-1 ()
  (python-tests-with-temp-buffer
   "
v1 = 123 + \\
     456 + \\
     789
v2 = (value1,
      value2,

      value3,
      value4)
v3 = ('this is a string'

      'that is continued'
      'between lines'
      'within a paren',
      # this is a comment, yo
      'continue previous line')
v4 = '''
a very long
string
'''
"
   (python-tests-look-at "v1 =")
   (should (= (save-excursion
                (python-nav-forward-statement)
                (point))
              (python-tests-look-at "v2 =")))
   (should (= (save-excursion
                (python-nav-forward-statement)
                (point))
              (python-tests-look-at "v3 =")))
   (should (= (save-excursion
                (python-nav-forward-statement)
                (point))
              (python-tests-look-at "v4 =")))
   (should (= (save-excursion
                (python-nav-forward-statement)
                (point))
              (point-max)))))

(ert-deftest python-nav-backward-statement-1 ()
  (python-tests-with-temp-buffer
   "
v1 = 123 + \\
     456 + \\
     789
v2 = (value1,
      value2,

      value3,
      value4)
v3 = ('this is a string'

      'that is continued'
      'between lines'
      'within a paren',
      # this is a comment, yo
      'continue previous line')
v4 = '''
a very long
string
'''
"
   (goto-char (point-max))
   (should (= (save-excursion
                (python-nav-backward-statement)
                (point))
              (python-tests-look-at "v4 =" -1)))
   (should (= (save-excursion
                (python-nav-backward-statement)
                (point))
              (python-tests-look-at "v3 =" -1)))
   (should (= (save-excursion
                (python-nav-backward-statement)
                (point))
              (python-tests-look-at "v2 =" -1)))
   (should (= (save-excursion
                (python-nav-backward-statement)
                (point))
              (python-tests-look-at "v1 =" -1)))))

(ert-deftest python-nav-backward-statement-2 ()
  :expected-result :failed
  (python-tests-with-temp-buffer
   "
v1 = 123 + \\
     456 + \\
     789
v2 = (value1,
      value2,

      value3,
      value4)
"
   ;; FIXME: For some reason `python-nav-backward-statement' is moving
   ;; back two sentences when starting from 'value4)'.
   (goto-char (point-max))
   (python-util-forward-comment -1)
   (should (= (save-excursion
                (python-nav-backward-statement)
                (point))
              (python-tests-look-at "v2 =" -1 t)))))

(ert-deftest python-nav-beginning-of-block-1 ()
  (python-tests-with-temp-buffer
   "
def decoratorFunctionWithArguments(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decoratorFunctionWithArguments('arg1', 'arg2')
    def func(a, b, c=True):
        pass
    '''

    def wwrap(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap
"
   (python-tests-look-at "return wwrap")
   (should (= (save-excursion
                (python-nav-beginning-of-block)
                (point))
              (python-tests-look-at "def decoratorFunctionWithArguments" -1)))
   (python-tests-look-at "print 'Inside wwrap()'")
   (should (= (save-excursion
                (python-nav-beginning-of-block)
                (point))
              (python-tests-look-at "def wwrap(f):" -1)))
   (python-tests-look-at "print 'After f(*args)'")
   (end-of-line)
   (should (= (save-excursion
                (python-nav-beginning-of-block)
                (point))
              (python-tests-look-at "def wrapped_f(*args):" -1)))
   (python-tests-look-at "return wrapped_f")
   (should (= (save-excursion
                (python-nav-beginning-of-block)
                (point))
              (python-tests-look-at "def wwrap(f):" -1)))))

(ert-deftest python-nav-end-of-block-1 ()
  (python-tests-with-temp-buffer
   "
def decoratorFunctionWithArguments(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decoratorFunctionWithArguments('arg1', 'arg2')
    def func(a, b, c=True):
        pass
    '''

    def wwrap(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap
"
   (python-tests-look-at "def decoratorFunctionWithArguments")
   (should (= (save-excursion
                (python-nav-end-of-block)
                (point))
              (save-excursion
                (goto-char (point-max))
                (python-util-forward-comment -1)
                (point))))
   (python-tests-look-at "def wwrap(f):")
   (should (= (save-excursion
                (python-nav-end-of-block)
                (point))
              (save-excursion
                (python-tests-look-at "return wrapped_f")
                (line-end-position))))
   (end-of-line)
   (should (= (save-excursion
                (python-nav-end-of-block)
                (point))
              (save-excursion
                (python-tests-look-at "return wrapped_f")
                (line-end-position))))
   (python-tests-look-at "f(*args)")
   (should (= (save-excursion
                (python-nav-end-of-block)
                (point))
              (save-excursion
                (python-tests-look-at "print 'After f(*args)'")
                (line-end-position))))))

(ert-deftest python-nav-forward-block-1 ()
  "This also accounts as a test for `python-nav-backward-block'."
  (python-tests-with-temp-buffer
   "
if request.user.is_authenticated():
    # def block():
    #     pass
    try:
        profile = request.user.get_profile()
    except Profile.DoesNotExist:
        profile = Profile.objects.create(user=request.user)
    else:
        if profile.stats:
            profile.recalculate_stats()
        else:
            profile.clear_stats()
    finally:
        profile.views += 1
        profile.save()
"
   (should (= (save-excursion (python-nav-forward-block))
              (python-tests-look-at "if request.user.is_authenticated():")))
   (should (= (save-excursion (python-nav-forward-block))
              (python-tests-look-at "try:")))
   (should (= (save-excursion (python-nav-forward-block))
              (python-tests-look-at "except Profile.DoesNotExist:")))
   (should (= (save-excursion (python-nav-forward-block))
              (python-tests-look-at "else:")))
   (should (= (save-excursion (python-nav-forward-block))
              (python-tests-look-at "if profile.stats:")))
   (should (= (save-excursion (python-nav-forward-block))
              (python-tests-look-at "else:")))
   (should (= (save-excursion (python-nav-forward-block))
              (python-tests-look-at "finally:")))
   ;; When point is at the last block, leave it there and return nil
   (should (not (save-excursion (python-nav-forward-block))))
   ;; Move backwards, and even if the number of moves is less than the
   ;; provided argument return the point.
   (should (= (save-excursion (python-nav-forward-block -10))
              (python-tests-look-at
               "if request.user.is_authenticated():" -1)))))

(ert-deftest python-nav-forward-sexp-1 ()
  (python-tests-with-temp-buffer
   "
a()
b()
c()
"
   (python-tests-look-at "a()")
   (python-nav-forward-sexp)
   (should (looking-at "$"))
   (should (save-excursion
             (beginning-of-line)
             (looking-at "a()")))
   (python-nav-forward-sexp)
   (should (looking-at "$"))
   (should (save-excursion
             (beginning-of-line)
             (looking-at "b()")))
   (python-nav-forward-sexp)
   (should (looking-at "$"))
   (should (save-excursion
             (beginning-of-line)
             (looking-at "c()")))
   ;; The default behavior when next to a paren should do what lisp
   ;; does and, otherwise `blink-matching-open' breaks.
   (python-nav-forward-sexp -1)
   (should (looking-at "()"))
   (should (save-excursion
             (beginning-of-line)
             (looking-at "c()")))
   (end-of-line)
   ;; Skipping parens should jump to `bolp'
   (python-nav-forward-sexp -1 nil t)
   (should (looking-at "c()"))
   (forward-line -1)
   (end-of-line)
   ;; b()
   (python-nav-forward-sexp -1)
   (should (looking-at "()"))
   (python-nav-forward-sexp -1)
   (should (looking-at "b()"))
   (end-of-line)
   (python-nav-forward-sexp -1 nil t)
   (should (looking-at "b()"))
   (forward-line -1)
   (end-of-line)
   ;; a()
   (python-nav-forward-sexp -1)
   (should (looking-at "()"))
   (python-nav-forward-sexp -1)
   (should (looking-at "a()"))
   (end-of-line)
   (python-nav-forward-sexp -1 nil t)
   (should (looking-at "a()"))))

(ert-deftest python-nav-forward-sexp-2 ()
  (python-tests-with-temp-buffer
   "
def func():
    if True:
        aaa = bbb
        ccc = ddd
        eee = fff
    return ggg
"
   (python-tests-look-at "aa =")
   (python-nav-forward-sexp)
   (should (looking-at " = bbb"))
   (python-nav-forward-sexp)
   (should (looking-at "$"))
   (should (save-excursion
             (back-to-indentation)
             (looking-at "aaa = bbb")))
   (python-nav-forward-sexp)
   (should (looking-at "$"))
   (should (save-excursion
             (back-to-indentation)
             (looking-at "ccc = ddd")))
   (python-nav-forward-sexp)
   (should (looking-at "$"))
   (should (save-excursion
             (back-to-indentation)
             (looking-at "eee = fff")))
   (python-nav-forward-sexp)
   (should (looking-at "$"))
   (should (save-excursion
             (back-to-indentation)
             (looking-at "return ggg")))
   (python-nav-forward-sexp -1)
   (should (looking-at "def func():"))))

(ert-deftest python-nav-forward-sexp-3 ()
  (python-tests-with-temp-buffer
   "
from some_module import some_sub_module
from another_module import another_sub_module

def another_statement():
    pass
"
   (python-tests-look-at "some_module")
   (python-nav-forward-sexp)
   (should (looking-at " import"))
   (python-nav-forward-sexp)
   (should (looking-at " some_sub_module"))
   (python-nav-forward-sexp)
   (should (looking-at "$"))
   (should
    (save-excursion
      (back-to-indentation)
      (looking-at
       "from some_module import some_sub_module")))
   (python-nav-forward-sexp)
   (should (looking-at "$"))
   (should
    (save-excursion
      (back-to-indentation)
      (looking-at
       "from another_module import another_sub_module")))
   (python-nav-forward-sexp)
   (should (looking-at "$"))
   (should
    (save-excursion
      (back-to-indentation)
      (looking-at
       "pass")))
   (python-nav-forward-sexp -1)
   (should (looking-at "def another_statement():"))
   (python-nav-forward-sexp -1)
   (should (looking-at "from another_module import another_sub_module"))
   (python-nav-forward-sexp -1)
   (should (looking-at "from some_module import some_sub_module"))))

(ert-deftest python-nav-forward-sexp-safe-1 ()
  (python-tests-with-temp-buffer
   "
profile = Profile.objects.create(user=request.user)
profile.notify()
"
   (python-tests-look-at "profile =")
   (python-nav-forward-sexp-safe 1)
   (should (looking-at "$"))
   (beginning-of-line 1)
   (python-tests-look-at "user=request.user")
   (python-nav-forward-sexp-safe -1)
   (should (looking-at "(user=request.user)"))
   (python-nav-forward-sexp-safe -4)
   (should (looking-at "profile ="))
   (python-tests-look-at "user=request.user")
   (python-nav-forward-sexp-safe 3)
   (should (looking-at ")"))
   (python-nav-forward-sexp-safe 1)
   (should (looking-at "$"))
   (python-nav-forward-sexp-safe 1)
   (should (looking-at "$"))))

(ert-deftest python-nav-up-list-1 ()
  (python-tests-with-temp-buffer
   "
def f():
    if True:
        return [i for i in range(3)]
"
   (python-tests-look-at "3)]")
   (python-nav-up-list)
   (should (looking-at "]"))
   (python-nav-up-list)
   (should (looking-at "$"))))

(ert-deftest python-nav-backward-up-list-1 ()
  :expected-result :failed
  (python-tests-with-temp-buffer
   "
def f():
    if True:
        return [i for i in range(3)]
"
   (python-tests-look-at "3)]")
   (python-nav-backward-up-list)
   (should (looking-at "(3)\\]"))
   (python-nav-backward-up-list)
   (should (looking-at
            "\\[i for i in range(3)\\]"))
   ;; FIXME: Need to move to beginning-of-statement.
   (python-nav-backward-up-list)
   (should (looking-at
            "return \\[i for i in range(3)\\]"))
   (python-nav-backward-up-list)
   (should (looking-at "if True:"))
   (python-nav-backward-up-list)
   (should (looking-at "def f():"))))

(ert-deftest python-indent-dedent-line-backspace-1 ()
  "Check de-indentation on first call.  Bug#18319."
  (python-tests-with-temp-buffer
   "
if True:
    x ()
    if False:
"
   (python-tests-look-at "if False:")
   (call-interactively #'python-indent-dedent-line-backspace)
   (should (zerop (current-indentation)))
   ;; XXX: This should be a call to `undo' but it's triggering errors.
   (insert "    ")
   (should (= (current-indentation) 4))
   (call-interactively #'python-indent-dedent-line-backspace)
   (should (zerop (current-indentation)))))

(ert-deftest python-indent-dedent-line-backspace-2 ()
  "Check de-indentation with tabs.  Bug#19730."
  (let ((tab-width 8))
    (python-tests-with-temp-buffer
     "
if x:
\tabcdefg
"
     (python-tests-look-at "abcdefg")
     (goto-char (line-end-position))
     (call-interactively #'python-indent-dedent-line-backspace)
     (should
      (string= (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))
               "\tabcdef")))))

(ert-deftest python-indent-dedent-line-backspace-3 ()
  "Paranoid check of de-indentation with tabs.  Bug#19730."
  (let ((tab-width 8))
    (python-tests-with-temp-buffer
     "
if x:
\tif y:
\t abcdefg
"
     (python-tests-look-at "abcdefg")
     (goto-char (line-end-position))
     (call-interactively #'python-indent-dedent-line-backspace)
     (should
      (string= (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))
               "\t abcdef"))
     (back-to-indentation)
     (call-interactively #'python-indent-dedent-line-backspace)
     (should
      (string= (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))
               "\tabcdef"))
     (call-interactively #'python-indent-dedent-line-backspace)
     (should
      (string= (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))
               "    abcdef"))
     (call-interactively #'python-indent-dedent-line-backspace)
     (should
      (string= (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))
               "abcdef")))))

(ert-deftest python-bob-infloop-avoid ()
  "Test that strings at BOB don't confuse syntax analysis.  Bug#24905"
  (python-tests-with-temp-buffer
      " \"\n"
    (goto-char (point-min))
    (call-interactively 'font-lock-fontify-buffer)))


;;; Shell integration

(defvar python-tests-shell-interpreter "python")

(ert-deftest python-shell-get-process-name-1 ()
  "Check process name calculation sans `buffer-file-name'."
  (python-tests-with-temp-buffer
   ""
   (should (string= (python-shell-get-process-name nil)
                    python-shell-buffer-name))
   (should (string= (python-shell-get-process-name t)
                    (format "%s[%s]" python-shell-buffer-name (buffer-name))))))

(ert-deftest python-shell-get-process-name-2 ()
  "Check process name calculation with `buffer-file-name'."
  (python-tests-with-temp-file
   ""
   ;; `buffer-file-name' is non-nil but the dedicated flag is nil and
   ;; should be respected.
   (should (string= (python-shell-get-process-name nil)
                    python-shell-buffer-name))
   (should (string=
            (python-shell-get-process-name t)
            (format "%s[%s]" python-shell-buffer-name (buffer-name))))))

(ert-deftest python-shell-internal-get-process-name-1 ()
  "Check the internal process name is buffer-unique sans `buffer-file-name'."
  (python-tests-with-temp-buffer
   ""
   (should (string= (python-shell-internal-get-process-name)
                    (format "%s[%s]" python-shell-internal-buffer-name (buffer-name))))))

(ert-deftest python-shell-internal-get-process-name-2 ()
  "Check the internal process name is buffer-unique with `buffer-file-name'."
  (python-tests-with-temp-file
   ""
   (should (string= (python-shell-internal-get-process-name)
                    (format "%s[%s]" python-shell-internal-buffer-name (buffer-name))))))

(ert-deftest python-shell-calculate-pythonpath-1 ()
  "Test PYTHONPATH calculation."
  (let ((process-environment '("PYTHONPATH=/path0"))
        (python-shell-extra-pythonpaths '("/path1" "/path2")))
    (should (string= (python-shell-calculate-pythonpath)
                     (concat "/path1" path-separator
                             "/path2" path-separator "/path0")))))

(ert-deftest python-shell-calculate-pythonpath-2 ()
  "Test existing paths are moved to front."
  (let ((process-environment
         (list (concat "PYTHONPATH=/path0" path-separator "/path1")))
        (python-shell-extra-pythonpaths '("/path1" "/path2")))
    (should (string= (python-shell-calculate-pythonpath)
                     (concat "/path1" path-separator
                             "/path2" path-separator "/path0")))))

(ert-deftest python-shell-calculate-process-environment-1 ()
  "Test `python-shell-process-environment' modification."
  (let* ((python-shell-process-environment
          '("TESTVAR1=value1" "TESTVAR2=value2"))
         (process-environment (python-shell-calculate-process-environment)))
    (should (equal (getenv "TESTVAR1") "value1"))
    (should (equal (getenv "TESTVAR2") "value2"))))

(ert-deftest python-shell-calculate-process-environment-2 ()
  "Test `python-shell-extra-pythonpaths' modification."
  (let* ((process-environment process-environment)
         (original-pythonpath (setenv "PYTHONPATH" "/path0"))
         (python-shell-extra-pythonpaths '("/path1" "/path2"))
         (process-environment (python-shell-calculate-process-environment)))
    (should (equal (getenv "PYTHONPATH")
                   (concat "/path1" path-separator
                           "/path2" path-separator "/path0")))))

(ert-deftest python-shell-calculate-process-environment-3 ()
  "Test `python-shell-virtualenv-root' modification."
  (let* ((python-shell-virtualenv-root "/env")
         (process-environment
          (let ((process-environment process-environment))
            (setenv "PYTHONHOME" "/home")
            (setenv "VIRTUAL_ENV")
            (python-shell-calculate-process-environment))))
    (should (not (getenv "PYTHONHOME")))
    (should (string= (getenv "VIRTUAL_ENV") "/env"))))

(ert-deftest python-shell-calculate-process-environment-4 ()
  "Test PYTHONUNBUFFERED when `python-shell-unbuffered' is non-nil."
  (let* ((python-shell-unbuffered t)
         (process-environment
          (let ((process-environment process-environment))
            (setenv "PYTHONUNBUFFERED")
            (python-shell-calculate-process-environment))))
    (should (string= (getenv "PYTHONUNBUFFERED") "1"))))

(ert-deftest python-shell-calculate-process-environment-5 ()
  "Test PYTHONUNBUFFERED when `python-shell-unbuffered' is nil."
  (let* ((python-shell-unbuffered nil)
         (process-environment
          (let ((process-environment process-environment))
            (setenv "PYTHONUNBUFFERED")
            (python-shell-calculate-process-environment))))
    (should (not (getenv "PYTHONUNBUFFERED")))))

(ert-deftest python-shell-calculate-process-environment-6 ()
  "Test PYTHONUNBUFFERED=1 when `python-shell-unbuffered' is nil."
  (let* ((python-shell-unbuffered nil)
         (process-environment
          (let ((process-environment process-environment))
            (setenv "PYTHONUNBUFFERED" "1")
            (python-shell-calculate-process-environment))))
    ;; User default settings must remain untouched:
    (should (string= (getenv "PYTHONUNBUFFERED") "1"))))

(ert-deftest python-shell-calculate-process-environment-7 ()
  "Test no side-effects on `process-environment'."
  (let* ((python-shell-process-environment
          '("TESTVAR1=value1" "TESTVAR2=value2"))
         (python-shell-virtualenv-root (or (getenv "VIRTUAL_ENV") "/env"))
         (python-shell-unbuffered t)
         (python-shell-extra-pythonpaths'("/path1" "/path2"))
         (original-process-environment (copy-sequence process-environment)))
    (python-shell-calculate-process-environment)
    (should (equal process-environment original-process-environment))))

(ert-deftest python-shell-calculate-process-environment-8 ()
  "Test no side-effects on `tramp-remote-process-environment'."
  (let* ((default-directory "/ssh::/example/dir/")
         (python-shell-process-environment
          '("TESTVAR1=value1" "TESTVAR2=value2"))
         (python-shell-virtualenv-root "/env")
         (python-shell-unbuffered t)
         (python-shell-extra-pythonpaths'("/path1" "/path2"))
         (original-process-environment
          (copy-sequence tramp-remote-process-environment)))
    (python-shell-calculate-process-environment)
    (should (equal tramp-remote-process-environment original-process-environment))))

(ert-deftest python-shell-calculate-exec-path-1 ()
  "Test `python-shell-exec-path' modification."
  (let* ((exec-path '("/path0"))
         (python-shell-exec-path '("/path1" "/path2"))
         (new-exec-path (python-shell-calculate-exec-path)))
    (should (equal new-exec-path '("/path1" "/path2" "/path0")))))

(ert-deftest python-shell-calculate-exec-path-2 ()
  "Test `python-shell-virtualenv-root' modification."
  (let* ((exec-path '("/path0"))
         (python-shell-virtualenv-root "/env")
         (new-exec-path (python-shell-calculate-exec-path)))
    (should (equal new-exec-path
                   (list (expand-file-name "/env/bin") "/path0")))))

(ert-deftest python-shell-calculate-exec-path-3 ()
  "Test complete `python-shell-virtualenv-root' modification."
  (let* ((exec-path '("/path0"))
         (python-shell-exec-path '("/path1" "/path2"))
         (python-shell-virtualenv-root "/env")
         (new-exec-path (python-shell-calculate-exec-path)))
    (should (equal new-exec-path
                   (list (expand-file-name "/env/bin")
                         "/path1" "/path2" "/path0")))))

(ert-deftest python-shell-calculate-exec-path-4 ()
  "Test complete `python-shell-virtualenv-root' with remote."
  (let* ((default-directory "/ssh::/example/dir/")
         (python-shell-remote-exec-path '("/path0"))
         (python-shell-exec-path '("/path1" "/path2"))
         (python-shell-virtualenv-root "/env")
         (new-exec-path (python-shell-calculate-exec-path)))
    (should (equal new-exec-path
                   (list (expand-file-name "/env/bin")
                         "/path1" "/path2" "/path0")))))

(ert-deftest python-shell-calculate-exec-path-5 ()
  "Test no side-effects on `exec-path'."
  (let* ((exec-path '("/path0"))
         (python-shell-exec-path '("/path1" "/path2"))
         (python-shell-virtualenv-root "/env")
         (original-exec-path (copy-sequence exec-path)))
    (python-shell-calculate-exec-path)
    (should (equal exec-path original-exec-path))))

(ert-deftest python-shell-calculate-exec-path-6 ()
  "Test no side-effects on `python-shell-remote-exec-path'."
  (let* ((default-directory "/ssh::/example/dir/")
         (python-shell-remote-exec-path '("/path0"))
         (python-shell-exec-path '("/path1" "/path2"))
         (python-shell-virtualenv-root "/env")
         (original-exec-path (copy-sequence python-shell-remote-exec-path)))
    (python-shell-calculate-exec-path)
    (should (equal python-shell-remote-exec-path original-exec-path))))

(ert-deftest python-shell-with-environment-1 ()
  "Test environment with local `default-directory'."
  (let* ((exec-path '("/path0"))
         (python-shell-exec-path '("/path1" "/path2"))
         (original-exec-path exec-path)
         (python-shell-virtualenv-root "/env"))
    (python-shell-with-environment
     (should (equal exec-path
                    (list (expand-file-name "/env/bin")
                          "/path1" "/path2" "/path0")))
      (should (not (getenv "PYTHONHOME")))
      (should (string= (getenv "VIRTUAL_ENV") "/env")))
    (should (equal exec-path original-exec-path))))

(ert-deftest python-shell-with-environment-2 ()
  "Test environment with remote `default-directory'."
  (let* ((default-directory "/ssh::/example/dir/")
         (python-shell-remote-exec-path '("/remote1" "/remote2"))
         (python-shell-exec-path '("/path1" "/path2"))
         (tramp-remote-process-environment '("EMACS=t"))
         (original-process-environment (copy-sequence tramp-remote-process-environment))
         (python-shell-virtualenv-root "/env"))
    (python-shell-with-environment
      (should (equal (python-shell-calculate-exec-path)
                     (list (expand-file-name "/env/bin")
                           "/path1" "/path2" "/remote1" "/remote2")))
      (let ((process-environment (python-shell-calculate-process-environment)))
        (should (not (getenv "PYTHONHOME")))
        (should (string= (getenv "VIRTUAL_ENV") "/env"))
        (should (equal tramp-remote-process-environment process-environment))))
    (should (equal tramp-remote-process-environment original-process-environment))))

(ert-deftest python-shell-with-environment-3 ()
  "Test `python-shell-with-environment' is idempotent."
  (let* ((python-shell-extra-pythonpaths '("/example/dir/"))
         (python-shell-exec-path '("path1" "path2"))
         (python-shell-virtualenv-root "/home/user/env")
         (single-call
          (python-shell-with-environment
            (list exec-path process-environment)))
         (nested-call
          (python-shell-with-environment
            (python-shell-with-environment
              (list exec-path process-environment)))))
    (should (equal single-call nested-call))))

(ert-deftest python-shell-make-comint-1 ()
  "Check comint creation for global shell buffer."
  (skip-unless (executable-find python-tests-shell-interpreter))
  ;; The interpreter can get killed too quickly to allow it to clean
  ;; up the tempfiles that the default python-shell-setup-codes create,
  ;; so it leaves tempfiles behind, which is a minor irritation.
  (let* ((python-shell-setup-codes nil)
         (python-shell-interpreter
          (executable-find python-tests-shell-interpreter))
         (proc-name (python-shell-get-process-name nil))
         (shell-buffer
          (python-tests-with-temp-buffer
           "" (python-shell-make-comint
               (python-shell-calculate-command) proc-name)))
         (process (get-buffer-process shell-buffer)))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag process nil)
          (should (process-live-p process))
          (with-current-buffer shell-buffer
            (should (eq major-mode 'inferior-python-mode))
            (should (string= (buffer-name) (format "*%s*" proc-name)))))
      (kill-buffer shell-buffer))))

(ert-deftest python-shell-make-comint-2 ()
  "Check comint creation for internal shell buffer."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let* ((python-shell-setup-codes nil)
         (python-shell-interpreter
          (executable-find python-tests-shell-interpreter))
         (proc-name (python-shell-internal-get-process-name))
         (shell-buffer
          (python-tests-with-temp-buffer
           "" (python-shell-make-comint
               (python-shell-calculate-command) proc-name nil t)))
         (process (get-buffer-process shell-buffer)))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag process nil)
          (should (process-live-p process))
          (with-current-buffer shell-buffer
            (should (eq major-mode 'inferior-python-mode))
            (should (string= (buffer-name) (format " *%s*" proc-name)))))
      (kill-buffer shell-buffer))))

(ert-deftest python-shell-make-comint-3 ()
  "Check comint creation with overridden python interpreter and args.
The command passed to `python-shell-make-comint' as argument must
locally override global values set in `python-shell-interpreter'
and `python-shell-interpreter-args' in the new shell buffer."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let* ((python-shell-setup-codes nil)
         (python-shell-interpreter "interpreter")
         (python-shell-interpreter-args "--some-args")
         (proc-name (python-shell-get-process-name nil))
         (interpreter-override
          (concat (executable-find python-tests-shell-interpreter) " " "-i"))
         (shell-buffer
          (python-tests-with-temp-buffer
           "" (python-shell-make-comint interpreter-override proc-name nil)))
         (process (get-buffer-process shell-buffer)))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag process nil)
          (should (process-live-p process))
          (with-current-buffer shell-buffer
            (should (eq major-mode 'inferior-python-mode))
            (should (file-equal-p
                     python-shell-interpreter
                     (executable-find python-tests-shell-interpreter)))
            (should (string= python-shell-interpreter-args "-i"))))
      (kill-buffer shell-buffer))))

(ert-deftest python-shell-make-comint-4 ()
  "Check shell calculated prompts regexps are set."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let* ((process-environment process-environment)
         (python-shell-setup-codes nil)
         (python-shell-interpreter
          (executable-find python-tests-shell-interpreter))
         (python-shell-interpreter-args "-i")
         (python-shell--prompt-calculated-input-regexp nil)
         (python-shell--prompt-calculated-output-regexp nil)
         (python-shell-prompt-detect-enabled t)
         (python-shell-prompt-input-regexps '("extralargeinputprompt" "sml"))
         (python-shell-prompt-output-regexps '("extralargeoutputprompt" "sml"))
         (python-shell-prompt-regexp "in")
         (python-shell-prompt-block-regexp "block")
         (python-shell-prompt-pdb-regexp "pdf")
         (python-shell-prompt-output-regexp "output")
         (startup-code (concat "import sys\n"
                               "sys.ps1 = 'py> '\n"
                               "sys.ps2 = '..> '\n"
                               "sys.ps3 = 'out '\n"))
         (startup-file (python-shell--save-temp-file startup-code))
         (proc-name (python-shell-get-process-name nil))
         (shell-buffer
          (progn
            (setenv "PYTHONSTARTUP" startup-file)
            (python-tests-with-temp-buffer
             "" (python-shell-make-comint
                 (python-shell-calculate-command) proc-name nil))))
         (process (get-buffer-process shell-buffer)))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag process nil)
          (should (process-live-p process))
          (with-current-buffer shell-buffer
            (should (eq major-mode 'inferior-python-mode))
            (should (string=
                     python-shell--prompt-calculated-input-regexp
                     (concat "^\\(extralargeinputprompt\\|\\.\\.> \\|"
                             "block\\|py> \\|pdf\\|sml\\|in\\)")))
            (should (string=
                     python-shell--prompt-calculated-output-regexp
                     "^\\(extralargeoutputprompt\\|output\\|out \\|sml\\)"))))
      (delete-file startup-file)
      (kill-buffer shell-buffer))))

(ert-deftest python-shell-get-process-1 ()
  "Check dedicated shell process preference over global."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (python-tests-with-temp-file
      ""
    (let* ((python-shell-setup-codes nil)
           (python-shell-interpreter
            (executable-find python-tests-shell-interpreter))
           (global-proc-name (python-shell-get-process-name nil))
           (dedicated-proc-name (python-shell-get-process-name t))
           (global-shell-buffer
            (python-shell-make-comint
             (python-shell-calculate-command) global-proc-name))
           (dedicated-shell-buffer
            (python-shell-make-comint
             (python-shell-calculate-command) dedicated-proc-name))
           (global-process (get-buffer-process global-shell-buffer))
           (dedicated-process (get-buffer-process dedicated-shell-buffer)))
      (unwind-protect
          (progn
            (set-process-query-on-exit-flag global-process nil)
            (set-process-query-on-exit-flag dedicated-process nil)
            ;; Prefer dedicated if global also exists.
            (should (equal (python-shell-get-process) dedicated-process))
            (kill-buffer dedicated-shell-buffer)
            ;; If there's only global, use it.
            (should (equal (python-shell-get-process) global-process))
            (kill-buffer global-shell-buffer)
            ;; No buffer available.
            (should (not (python-shell-get-process))))
        (ignore-errors (kill-buffer global-shell-buffer))
        (ignore-errors (kill-buffer dedicated-shell-buffer))))))

(ert-deftest python-shell-internal-get-or-create-process-1 ()
  "Check internal shell process creation fallback."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (python-tests-with-temp-file
   ""
   (should (not (process-live-p (python-shell-internal-get-process-name))))
   (let* ((python-shell-interpreter
           (executable-find python-tests-shell-interpreter))
          (internal-process-name (python-shell-internal-get-process-name))
          (internal-process (python-shell-internal-get-or-create-process))
          (internal-shell-buffer (process-buffer internal-process)))
     (unwind-protect
         (progn
           (set-process-query-on-exit-flag internal-process nil)
           (should (equal (process-name internal-process)
                          internal-process-name))
           (should (equal internal-process
                          (python-shell-internal-get-or-create-process)))
           ;; Assert the internal process is not a user process
           (should (not (python-shell-get-process)))
           (kill-buffer internal-shell-buffer))
       (ignore-errors (kill-buffer internal-shell-buffer))))))

(ert-deftest python-shell-prompt-detect-1 ()
  "Check prompt autodetection."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let ((process-environment process-environment))
    ;; Ensure no startup file is enabled
    (setenv "PYTHONSTARTUP" "")
    (should python-shell-prompt-detect-enabled)
    (should (equal (python-shell-prompt-detect) '(">>> " "... " "")))))

(ert-deftest python-shell-prompt-detect-2 ()
  "Check prompt autodetection with startup file.  Bug#17370."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let* ((process-environment process-environment)
         (startup-code (concat "import sys\n"
                               "sys.ps1 = 'py> '\n"
                               "sys.ps2 = '..> '\n"
                               "sys.ps3 = 'out '\n"))
         (startup-file (python-shell--save-temp-file startup-code)))
    (unwind-protect
        (progn
          ;; Ensure startup file is enabled
          (setenv "PYTHONSTARTUP" startup-file)
          (should python-shell-prompt-detect-enabled)
          (should (equal (python-shell-prompt-detect) '("py> " "..> " "out "))))
      (ignore-errors (delete-file startup-file)))))

(ert-deftest python-shell-prompt-detect-3 ()
  "Check prompts are not autodetected when feature is disabled."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let ((process-environment process-environment)
        (python-shell-prompt-detect-enabled nil))
    ;; Ensure no startup file is enabled
    (should (not python-shell-prompt-detect-enabled))
    (should (not (python-shell-prompt-detect)))))

(ert-deftest python-shell-prompt-detect-4 ()
  "Check warning is shown when detection fails."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let* ((process-environment process-environment)
         ;; Trigger failure by removing prompts in the startup file
         (startup-code (concat "import sys\n"
                               "sys.ps1 = ''\n"
                               "sys.ps2 = ''\n"
                               "sys.ps3 = ''\n"))
         (startup-file (python-shell--save-temp-file startup-code)))
    (unwind-protect
        (progn
          (kill-buffer (get-buffer-create "*Warnings*"))
          (should (not (get-buffer "*Warnings*")))
          (setenv "PYTHONSTARTUP" startup-file)
          (should python-shell-prompt-detect-failure-warning)
          (should python-shell-prompt-detect-enabled)
          (should (not (python-shell-prompt-detect)))
          (should (get-buffer "*Warnings*")))
      (ignore-errors (delete-file startup-file)))))

(ert-deftest python-shell-prompt-detect-5 ()
  "Check disabled warnings are not shown when detection fails."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let* ((process-environment process-environment)
         (startup-code (concat "import sys\n"
                               "sys.ps1 = ''\n"
                               "sys.ps2 = ''\n"
                               "sys.ps3 = ''\n"))
         (startup-file (python-shell--save-temp-file startup-code))
         (python-shell-prompt-detect-failure-warning nil))
    (unwind-protect
        (progn
          (kill-buffer (get-buffer-create "*Warnings*"))
          (should (not (get-buffer "*Warnings*")))
          (setenv "PYTHONSTARTUP" startup-file)
          (should (not python-shell-prompt-detect-failure-warning))
          (should python-shell-prompt-detect-enabled)
          (should (not (python-shell-prompt-detect)))
          (should (not (get-buffer "*Warnings*"))))
      (ignore-errors (delete-file startup-file)))))

(ert-deftest python-shell-prompt-detect-6 ()
  "Warnings are not shown when detection is disabled."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let* ((process-environment process-environment)
         (startup-code (concat "import sys\n"
                               "sys.ps1 = ''\n"
                               "sys.ps2 = ''\n"
                               "sys.ps3 = ''\n"))
         (startup-file (python-shell--save-temp-file startup-code))
         (python-shell-prompt-detect-failure-warning t)
         (python-shell-prompt-detect-enabled nil))
    (unwind-protect
        (progn
          (kill-buffer (get-buffer-create "*Warnings*"))
          (should (not (get-buffer "*Warnings*")))
          (setenv "PYTHONSTARTUP" startup-file)
          (should python-shell-prompt-detect-failure-warning)
          (should (not python-shell-prompt-detect-enabled))
          (should (not (python-shell-prompt-detect)))
          (should (not (get-buffer "*Warnings*"))))
      (ignore-errors (delete-file startup-file)))))

(ert-deftest python-shell-prompt-validate-regexps-1 ()
  "Check `python-shell-prompt-input-regexps' are validated."
  (let* ((python-shell-prompt-input-regexps '("\\("))
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              (format-message
               "Invalid regexp \\( in `python-shell-prompt-input-regexps'")))))

(ert-deftest python-shell-prompt-validate-regexps-2 ()
  "Check `python-shell-prompt-output-regexps' are validated."
  (let* ((python-shell-prompt-output-regexps '("\\("))
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              (format-message
               "Invalid regexp \\( in `python-shell-prompt-output-regexps'")))))

(ert-deftest python-shell-prompt-validate-regexps-3 ()
  "Check `python-shell-prompt-regexp' is validated."
  (let* ((python-shell-prompt-regexp "\\(")
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              (format-message
               "Invalid regexp \\( in `python-shell-prompt-regexp'")))))

(ert-deftest python-shell-prompt-validate-regexps-4 ()
  "Check `python-shell-prompt-block-regexp' is validated."
  (let* ((python-shell-prompt-block-regexp "\\(")
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              (format-message
               "Invalid regexp \\( in `python-shell-prompt-block-regexp'")))))

(ert-deftest python-shell-prompt-validate-regexps-5 ()
  "Check `python-shell-prompt-pdb-regexp' is validated."
  (let* ((python-shell-prompt-pdb-regexp "\\(")
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              (format-message
               "Invalid regexp \\( in `python-shell-prompt-pdb-regexp'")))))

(ert-deftest python-shell-prompt-validate-regexps-6 ()
  "Check `python-shell-prompt-output-regexp' is validated."
  (let* ((python-shell-prompt-output-regexp "\\(")
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              (format-message
               "Invalid regexp \\( in `python-shell-prompt-output-regexp'")))))

(ert-deftest python-shell-prompt-validate-regexps-7 ()
  "Check default regexps are valid."
  ;; should not signal error
  (python-shell-prompt-validate-regexps))

(ert-deftest python-shell-prompt-set-calculated-regexps-1 ()
  "Check regexps are validated."
  (let* ((python-shell-prompt-output-regexp '("\\("))
         (python-shell--prompt-calculated-input-regexp nil)
         (python-shell--prompt-calculated-output-regexp nil)
         (python-shell-prompt-detect-enabled nil)
         (error-data (should-error (python-shell-prompt-set-calculated-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              (format-message
               "Invalid regexp \\( in `python-shell-prompt-output-regexp'")))))

(ert-deftest python-shell-prompt-set-calculated-regexps-2 ()
  "Check `python-shell-prompt-input-regexps' are set."
  (let* ((python-shell-prompt-input-regexps '("my" "prompt"))
         (python-shell-prompt-output-regexps '(""))
         (python-shell-prompt-regexp "")
         (python-shell-prompt-block-regexp "")
         (python-shell-prompt-pdb-regexp "")
         (python-shell-prompt-output-regexp "")
         (python-shell--prompt-calculated-input-regexp nil)
         (python-shell--prompt-calculated-output-regexp nil)
         (python-shell-prompt-detect-enabled nil))
    (python-shell-prompt-set-calculated-regexps)
    (should (string= python-shell--prompt-calculated-input-regexp
                     "^\\(prompt\\|my\\|\\)"))))

(ert-deftest python-shell-prompt-set-calculated-regexps-3 ()
  "Check `python-shell-prompt-output-regexps' are set."
  (let* ((python-shell-prompt-input-regexps '(""))
         (python-shell-prompt-output-regexps '("my" "prompt"))
         (python-shell-prompt-regexp "")
         (python-shell-prompt-block-regexp "")
         (python-shell-prompt-pdb-regexp "")
         (python-shell-prompt-output-regexp "")
         (python-shell--prompt-calculated-input-regexp nil)
         (python-shell--prompt-calculated-output-regexp nil)
         (python-shell-prompt-detect-enabled nil))
    (python-shell-prompt-set-calculated-regexps)
    (should (string= python-shell--prompt-calculated-output-regexp
                     "^\\(prompt\\|my\\|\\)"))))

(ert-deftest python-shell-prompt-set-calculated-regexps-4 ()
  "Check user defined prompts are set."
  (let* ((python-shell-prompt-input-regexps '(""))
         (python-shell-prompt-output-regexps '(""))
         (python-shell-prompt-regexp "prompt")
         (python-shell-prompt-block-regexp "block")
         (python-shell-prompt-pdb-regexp "pdb")
         (python-shell-prompt-output-regexp "output")
         (python-shell--prompt-calculated-input-regexp nil)
         (python-shell--prompt-calculated-output-regexp nil)
         (python-shell-prompt-detect-enabled nil))
    (python-shell-prompt-set-calculated-regexps)
    (should (string= python-shell--prompt-calculated-input-regexp
                     "^\\(prompt\\|block\\|pdb\\|\\)"))
    (should (string= python-shell--prompt-calculated-output-regexp
                     "^\\(output\\|\\)"))))

(ert-deftest python-shell-prompt-set-calculated-regexps-5 ()
  "Check order of regexps (larger first)."
  (let* ((python-shell-prompt-input-regexps '("extralargeinputprompt" "sml"))
         (python-shell-prompt-output-regexps '("extralargeoutputprompt" "sml"))
         (python-shell-prompt-regexp "in")
         (python-shell-prompt-block-regexp "block")
         (python-shell-prompt-pdb-regexp "pdf")
         (python-shell-prompt-output-regexp "output")
         (python-shell--prompt-calculated-input-regexp nil)
         (python-shell--prompt-calculated-output-regexp nil)
         (python-shell-prompt-detect-enabled nil))
    (python-shell-prompt-set-calculated-regexps)
    (should (string= python-shell--prompt-calculated-input-regexp
                     "^\\(extralargeinputprompt\\|block\\|pdf\\|sml\\|in\\)"))
    (should (string= python-shell--prompt-calculated-output-regexp
                     "^\\(extralargeoutputprompt\\|output\\|sml\\)"))))

(ert-deftest python-shell-prompt-set-calculated-regexps-6 ()
  "Check detected prompts are included `regexp-quote'd."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let* ((python-shell-prompt-input-regexps '(""))
         (python-shell-prompt-output-regexps '(""))
         (python-shell-prompt-regexp "")
         (python-shell-prompt-block-regexp "")
         (python-shell-prompt-pdb-regexp "")
         (python-shell-prompt-output-regexp "")
         (python-shell--prompt-calculated-input-regexp nil)
         (python-shell--prompt-calculated-output-regexp nil)
         (python-shell-prompt-detect-enabled t)
         (process-environment process-environment)
         (startup-code (concat "import sys\n"
                               "sys.ps1 = 'p.> '\n"
                               "sys.ps2 = '..> '\n"
                               "sys.ps3 = 'o.t '\n"))
         (startup-file (python-shell--save-temp-file startup-code)))
    (unwind-protect
        (progn
          (setenv "PYTHONSTARTUP" startup-file)
          (python-shell-prompt-set-calculated-regexps)
          (should (string= python-shell--prompt-calculated-input-regexp
                           "^\\(\\.\\.> \\|p\\.> \\|\\)"))
          (should (string= python-shell--prompt-calculated-output-regexp
                           "^\\(o\\.t \\|\\)")))
      (ignore-errors (delete-file startup-file)))))

(ert-deftest python-shell-buffer-substring-1 ()
  "Selecting a substring of the whole buffer must match its contents."
  (python-tests-with-temp-buffer
   "
class Foo(models.Model):
    pass


class Bar(models.Model):
    pass
"
   (should (string= (buffer-string)
                    (python-shell-buffer-substring (point-min) (point-max))))))

(ert-deftest python-shell-buffer-substring-2 ()
  "Main block should be removed if NOMAIN is non-nil."
  (python-tests-with-temp-buffer
   "
class Foo(models.Model):
    pass

class Bar(models.Model):
    pass

if __name__ == \"__main__\":
    foo = Foo()
    print (foo)
"
   (should (string= (python-shell-buffer-substring (point-min) (point-max) t)
                    "
class Foo(models.Model):
    pass

class Bar(models.Model):
    pass




"))))

(ert-deftest python-shell-buffer-substring-3 ()
  "Main block should be removed if NOMAIN is non-nil."
  (python-tests-with-temp-buffer
   "
class Foo(models.Model):
    pass

if __name__ == \"__main__\":
    foo = Foo()
    print (foo)

class Bar(models.Model):
    pass
"
   (should (string= (python-shell-buffer-substring (point-min) (point-max) t)
                    "
class Foo(models.Model):
    pass





class Bar(models.Model):
    pass
"))))

(ert-deftest python-shell-buffer-substring-4 ()
  "Coding cookie should be added for substrings."
  (python-tests-with-temp-buffer
   "# coding: latin-1

class Foo(models.Model):
    pass

if __name__ == \"__main__\":
    foo = Foo()
    print (foo)

class Bar(models.Model):
    pass
"
   (should (string= (python-shell-buffer-substring
                     (python-tests-look-at "class Foo(models.Model):")
                     (progn (python-nav-forward-sexp) (point)))
                    "# -*- coding: latin-1 -*-

class Foo(models.Model):
    pass"))))

(ert-deftest python-shell-buffer-substring-5 ()
  "The proper amount of blank lines is added for a substring."
  (python-tests-with-temp-buffer
   "# coding: latin-1

class Foo(models.Model):
    pass

if __name__ == \"__main__\":
    foo = Foo()
    print (foo)

class Bar(models.Model):
    pass
"
   (should (string= (python-shell-buffer-substring
                     (python-tests-look-at "class Bar(models.Model):")
                     (progn (python-nav-forward-sexp) (point)))
                    "# -*- coding: latin-1 -*-








class Bar(models.Model):
    pass"))))

(ert-deftest python-shell-buffer-substring-6 ()
  "Handle substring with coding cookie in the second line."
  (python-tests-with-temp-buffer
   "
# coding: latin-1

class Foo(models.Model):
    pass

if __name__ == \"__main__\":
    foo = Foo()
    print (foo)

class Bar(models.Model):
    pass
"
   (should (string= (python-shell-buffer-substring
                     (python-tests-look-at "# coding: latin-1")
                     (python-tests-look-at "if __name__ == \"__main__\":"))
                    "# -*- coding: latin-1 -*-


class Foo(models.Model):
    pass

"))))

(ert-deftest python-shell-buffer-substring-7 ()
  "Ensure first coding cookie gets precedence."
  (python-tests-with-temp-buffer
   "# coding: utf-8
# coding: latin-1

class Foo(models.Model):
    pass

if __name__ == \"__main__\":
    foo = Foo()
    print (foo)

class Bar(models.Model):
    pass
"
   (should (string= (python-shell-buffer-substring
                     (python-tests-look-at "# coding: latin-1")
                     (python-tests-look-at "if __name__ == \"__main__\":"))
                    "# -*- coding: utf-8 -*-


class Foo(models.Model):
    pass

"))))

(ert-deftest python-shell-buffer-substring-8 ()
  "Ensure first coding cookie gets precedence when sending whole buffer."
  (python-tests-with-temp-buffer
   "# coding: utf-8
# coding: latin-1

class Foo(models.Model):
    pass
"
   (should (string= (python-shell-buffer-substring (point-min) (point-max))
                    "# coding: utf-8


class Foo(models.Model):
    pass
"))))

(ert-deftest python-shell-buffer-substring-9 ()
  "Check substring starting from `point-min'."
  (python-tests-with-temp-buffer
   "# coding: utf-8

class Foo(models.Model):
    pass

class Bar(models.Model):
    pass
"
   (should (string= (python-shell-buffer-substring
                     (point-min)
                     (python-tests-look-at "class Bar(models.Model):"))
                    "# coding: utf-8

class Foo(models.Model):
    pass

"))))

(ert-deftest python-shell-buffer-substring-10 ()
  "Check substring from partial block."
  (python-tests-with-temp-buffer
   "
def foo():
    print ('a')
"
   (should (string= (python-shell-buffer-substring
                     (python-tests-look-at "print ('a')")
                     (point-max))
                    "if True:

    print ('a')
"))))

(ert-deftest python-shell-buffer-substring-11 ()
  "Check substring from partial block and point within indentation."
  (python-tests-with-temp-buffer
   "
def foo():
    print ('a')
"
   (should (string= (python-shell-buffer-substring
                     (progn
                       (python-tests-look-at "print ('a')")
                       (backward-char 1)
                       (point))
                     (point-max))
                    "if True:

    print ('a')
"))))

(ert-deftest python-shell-buffer-substring-12 ()
  "Check substring from partial block and point in whitespace."
  (python-tests-with-temp-buffer
   "
def foo():

        # Whitespace

    print ('a')
"
   (should (string= (python-shell-buffer-substring
                     (python-tests-look-at "# Whitespace")
                     (point-max))
                    "if True:


        # Whitespace

    print ('a')
"))))



;;; Shell completion

(ert-deftest python-shell-completion-native-interpreter-disabled-p-1 ()
  (let* ((python-shell-completion-native-disabled-interpreters (list "pypy"))
         (python-shell-interpreter "/some/path/to/bin/pypy"))
    (should (python-shell-completion-native-interpreter-disabled-p))))




;;; PDB Track integration


;;; Symbol completion


;;; Fill paragraph


;;; Skeletons


;;; FFAP


;;; Code check


;;; Eldoc

(ert-deftest python-eldoc--get-symbol-at-point-1 ()
  "Test paren handling."
  (python-tests-with-temp-buffer
   "
map(xx
map(codecs.open('somefile'
"
   (python-tests-look-at "ap(xx")
   (should (string= (python-eldoc--get-symbol-at-point) "map"))
   (goto-char (line-end-position))
   (should (string= (python-eldoc--get-symbol-at-point) "map"))
   (python-tests-look-at "('somefile'")
   (should (string= (python-eldoc--get-symbol-at-point) "map"))
   (goto-char (line-end-position))
   (should (string= (python-eldoc--get-symbol-at-point) "codecs.open"))))

(ert-deftest python-eldoc--get-symbol-at-point-2 ()
  "Ensure self is replaced with the class name."
  (python-tests-with-temp-buffer
   "
class TheClass:

    def some_method(self, n):
        return n

    def other(self):
        return self.some_method(1234)

"
   (python-tests-look-at "self.some_method")
   (should (string= (python-eldoc--get-symbol-at-point)
                    "TheClass.some_method"))
   (python-tests-look-at "1234)")
   (should (string= (python-eldoc--get-symbol-at-point)
                    "TheClass.some_method"))))

(ert-deftest python-eldoc--get-symbol-at-point-3 ()
  "Ensure symbol is found when point is at end of buffer."
  (python-tests-with-temp-buffer
   "
some_symbol

"
   (goto-char (point-max))
   (should (string= (python-eldoc--get-symbol-at-point)
                    "some_symbol"))))

(ert-deftest python-eldoc--get-symbol-at-point-4 ()
  "Ensure symbol is found when point is at whitespace."
  (python-tests-with-temp-buffer
   "
some_symbol   some_other_symbol
"
   (python-tests-look-at "  some_other_symbol")
   (should (string= (python-eldoc--get-symbol-at-point)
                    "some_symbol"))))


;;; Imenu

(ert-deftest python-imenu-create-index-1 ()
  (python-tests-with-temp-buffer
   "
class Foo(models.Model):
    pass


class Bar(models.Model):
    pass


def decorator(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decorator('arg1', 'arg2')
    def func(a, b, c=True):
        pass
    '''

    def wrap(f):
        print ('wrap')
        def wrapped_f(*args):
            print ('wrapped_f')
            print ('Decorator arguments:', arg1, arg2, arg3)
            f(*args)
            print ('called f(*args)')
        return wrapped_f
    return wrap


class Baz(object):

    def a(self):
        pass

    def b(self):
        pass

    class Frob(object):

        def c(self):
            pass

        async def d(self):
            pass
"
   (goto-char (point-max))
   (should (equal
            (list
             (cons "Foo (class)" (copy-marker 2))
             (cons "Bar (class)" (copy-marker 38))
             (list
              "decorator (def)"
              (cons "*function definition*" (copy-marker 74))
              (list
               "wrap (def)"
               (cons "*function definition*" (copy-marker 254))
               (cons "wrapped_f (def)" (copy-marker 294))))
             (list
              "Baz (class)"
              (cons "*class definition*" (copy-marker 519))
              (cons "a (def)" (copy-marker 539))
              (cons "b (def)" (copy-marker 570))
              (list
               "Frob (class)"
               (cons "*class definition*" (copy-marker 601))
               (cons "c (def)" (copy-marker 626))
               (cons "d (async def)" (copy-marker 665)))))
            (python-imenu-create-index)))))

(ert-deftest python-imenu-create-index-2 ()
  (python-tests-with-temp-buffer
   "
class Foo(object):
    def foo(self):
        def foo1():
            pass

    def foobar(self):
        pass
"
   (goto-char (point-max))
   (should (equal
            (list
             (list
              "Foo (class)"
              (cons "*class definition*" (copy-marker 2))
              (list
               "foo (def)"
               (cons "*function definition*" (copy-marker 21))
               (cons "foo1 (def)" (copy-marker 40)))
              (cons "foobar (def)"  (copy-marker 78))))
            (python-imenu-create-index)))))

(ert-deftest python-imenu-create-index-3 ()
  (python-tests-with-temp-buffer
   "
class Foo(object):
    def foo(self):
        def foo1():
            pass
        def foo2():
            pass
"
   (goto-char (point-max))
   (should (equal
            (list
             (list
              "Foo (class)"
              (cons "*class definition*" (copy-marker 2))
              (list
               "foo (def)"
               (cons "*function definition*" (copy-marker 21))
               (cons "foo1 (def)" (copy-marker 40))
               (cons "foo2 (def)" (copy-marker 77)))))
            (python-imenu-create-index)))))

(ert-deftest python-imenu-create-index-4 ()
  (python-tests-with-temp-buffer
   "
class Foo(object):
    class Bar(object):
        def __init__(self):
            pass

        def __str__(self):
            pass

    def __init__(self):
            pass
"
   (goto-char (point-max))
   (should (equal
            (list
             (list
              "Foo (class)"
              (cons "*class definition*" (copy-marker 2))
              (list
               "Bar (class)"
               (cons "*class definition*" (copy-marker 21))
               (cons "__init__ (def)" (copy-marker 44))
               (cons "__str__ (def)" (copy-marker 90)))
              (cons "__init__ (def)" (copy-marker 135))))
            (python-imenu-create-index)))))

(ert-deftest python-imenu-create-flat-index-1 ()
  (python-tests-with-temp-buffer
   "
class Foo(models.Model):
    pass


class Bar(models.Model):
    pass


def decorator(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decorator('arg1', 'arg2')
    def func(a, b, c=True):
        pass
    '''

    def wrap(f):
        print ('wrap')
        def wrapped_f(*args):
            print ('wrapped_f')
            print ('Decorator arguments:', arg1, arg2, arg3)
            f(*args)
            print ('called f(*args)')
        return wrapped_f
    return wrap


class Baz(object):

    def a(self):
        pass

    def b(self):
        pass

    class Frob(object):

        def c(self):
            pass

        async def d(self):
            pass
"
   (goto-char (point-max))
   (should (equal
            (list (cons "Foo" (copy-marker 2))
                  (cons "Bar" (copy-marker 38))
                  (cons "decorator" (copy-marker 74))
                  (cons "decorator.wrap" (copy-marker 254))
                  (cons "decorator.wrap.wrapped_f" (copy-marker 294))
                  (cons "Baz" (copy-marker 519))
                  (cons "Baz.a" (copy-marker 539))
                  (cons "Baz.b" (copy-marker 570))
                  (cons "Baz.Frob" (copy-marker 601))
                  (cons "Baz.Frob.c" (copy-marker 626))
                  (cons "Baz.Frob.d" (copy-marker 665)))
            (python-imenu-create-flat-index)))))

(ert-deftest python-imenu-create-flat-index-2 ()
  (python-tests-with-temp-buffer
   "
class Foo(object):
    class Bar(object):
        def __init__(self):
            pass

        def __str__(self):
            pass

    def __init__(self):
            pass
"
   (goto-char (point-max))
   (should (equal
            (list
             (cons "Foo" (copy-marker 2))
             (cons "Foo.Bar" (copy-marker 21))
             (cons "Foo.Bar.__init__" (copy-marker 44))
             (cons "Foo.Bar.__str__" (copy-marker 90))
             (cons "Foo.__init__"  (copy-marker 135)))
            (python-imenu-create-flat-index)))))


;;; Misc helpers

(ert-deftest python-info-current-defun-1 ()
  (python-tests-with-temp-buffer
   "
def foo(a, b):
"
   (forward-line 1)
   (should (string= "foo" (python-info-current-defun)))
   (should (string= "def foo" (python-info-current-defun t)))
   (forward-line 1)
   (should (not (python-info-current-defun)))
   (indent-for-tab-command)
   (should (string= "foo" (python-info-current-defun)))
   (should (string= "def foo" (python-info-current-defun t)))))

(ert-deftest python-info-current-defun-2 ()
  (python-tests-with-temp-buffer
   "
class C(object):

    def m(self):
        if True:
            return [i for i in range(3)]
        else:
            return []

        def b():
            do_b()

        def a():
            do_a()

    def c(self):
        do_c()
"
   (forward-line 1)
   (should (string= "C" (python-info-current-defun)))
   (should (string= "class C" (python-info-current-defun t)))
   (python-tests-look-at "return [i for ")
   (should (string= "C.m" (python-info-current-defun)))
   (should (string= "def C.m" (python-info-current-defun t)))
   (python-tests-look-at "def b():")
   (should (string= "C.m.b" (python-info-current-defun)))
   (should (string= "def C.m.b" (python-info-current-defun t)))
   (forward-line 2)
   (indent-for-tab-command)
   (python-indent-dedent-line-backspace 1)
   (should (string= "C.m" (python-info-current-defun)))
   (should (string= "def C.m" (python-info-current-defun t)))
   (python-tests-look-at "def c(self):")
   (forward-line -1)
   (indent-for-tab-command)
   (should (string= "C.m.a" (python-info-current-defun)))
   (should (string= "def C.m.a" (python-info-current-defun t)))
   (python-indent-dedent-line-backspace 1)
   (should (string= "C.m" (python-info-current-defun)))
   (should (string= "def C.m" (python-info-current-defun t)))
   (python-indent-dedent-line-backspace 1)
   (should (string= "C" (python-info-current-defun)))
   (should (string= "class C" (python-info-current-defun t)))
   (python-tests-look-at "def c(self):")
   (should (string= "C.c" (python-info-current-defun)))
   (should (string= "def C.c" (python-info-current-defun t)))
   (python-tests-look-at "do_c()")
   (should (string= "C.c" (python-info-current-defun)))
   (should (string= "def C.c" (python-info-current-defun t)))))

(ert-deftest python-info-current-defun-3 ()
  (python-tests-with-temp-buffer
   "
def decoratorFunctionWithArguments(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decoratorFunctionWithArguments('arg1', 'arg2')
    def func(a, b, c=True):
        pass
    '''

    def wwrap(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap
"
   (python-tests-look-at "def wwrap(f):")
   (forward-line -1)
   (should (not (python-info-current-defun)))
   (indent-for-tab-command 1)
   (should (string= (python-info-current-defun)
                    "decoratorFunctionWithArguments"))
   (should (string= (python-info-current-defun t)
                    "def decoratorFunctionWithArguments"))
   (python-tests-look-at "def wrapped_f(*args):")
   (should (string= (python-info-current-defun)
                    "decoratorFunctionWithArguments.wwrap.wrapped_f"))
   (should (string= (python-info-current-defun t)
                    "def decoratorFunctionWithArguments.wwrap.wrapped_f"))
   (python-tests-look-at "return wrapped_f")
   (should (string= (python-info-current-defun)
                    "decoratorFunctionWithArguments.wwrap"))
   (should (string= (python-info-current-defun t)
                    "def decoratorFunctionWithArguments.wwrap"))
   (end-of-line 1)
   (python-tests-look-at "return wwrap")
   (should (string= (python-info-current-defun)
                    "decoratorFunctionWithArguments"))
   (should (string= (python-info-current-defun t)
                    "def decoratorFunctionWithArguments"))))

(ert-deftest python-info-current-symbol-1 ()
  (python-tests-with-temp-buffer
   "
class C(object):

    def m(self):
        self.c()

    def c(self):
        print ('a')
"
   (python-tests-look-at "self.c()")
   (should (string= "self.c" (python-info-current-symbol)))
   (should (string= "C.c" (python-info-current-symbol t)))))

(ert-deftest python-info-current-symbol-2 ()
  (python-tests-with-temp-buffer
   "
class C(object):

    class M(object):

        def a(self):
            self.c()

        def c(self):
            pass
"
   (python-tests-look-at "self.c()")
   (should (string= "self.c" (python-info-current-symbol)))
   (should (string= "C.M.c" (python-info-current-symbol t)))))

(ert-deftest python-info-current-symbol-3 ()
  "Keywords should not be considered symbols."
  :expected-result :failed
  (python-tests-with-temp-buffer
   "
class C(object):
    pass
"
   ;; FIXME: keywords are not symbols.
   (python-tests-look-at "class C")
   (should (not (python-info-current-symbol)))
   (should (not (python-info-current-symbol t)))
   (python-tests-look-at "C(object)")
   (should (string= "C" (python-info-current-symbol)))
   (should (string= "class C" (python-info-current-symbol t)))))

(ert-deftest python-info-statement-starts-block-p-1 ()
  (python-tests-with-temp-buffer
   "
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    print (var_one)
"
   (python-tests-look-at "def long_function_name")
   (should (python-info-statement-starts-block-p))
   (python-tests-look-at "print (var_one)")
   (python-util-forward-comment -1)
   (should (python-info-statement-starts-block-p))))

(ert-deftest python-info-statement-starts-block-p-2 ()
  (python-tests-with-temp-buffer
   "
if width == 0 and height == 0 and \\
   color == 'red' and emphasis == 'strong' or \\
   highlight > 100:
    raise ValueError('sorry, you lose')
"
   (python-tests-look-at "if width == 0 and")
   (should (python-info-statement-starts-block-p))
   (python-tests-look-at "raise ValueError(")
   (python-util-forward-comment -1)
   (should (python-info-statement-starts-block-p))))

(ert-deftest python-info-statement-ends-block-p-1 ()
  (python-tests-with-temp-buffer
   "
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    print (var_one)
"
   (python-tests-look-at "print (var_one)")
   (should (python-info-statement-ends-block-p))))

(ert-deftest python-info-statement-ends-block-p-2 ()
  (python-tests-with-temp-buffer
   "
if width == 0 and height == 0 and \\
   color == 'red' and emphasis == 'strong' or \\
   highlight > 100:
    raise ValueError(
'sorry, you lose'

)
"
   (python-tests-look-at "raise ValueError(")
   (should (python-info-statement-ends-block-p))))

(ert-deftest python-info-beginning-of-statement-p-1 ()
  (python-tests-with-temp-buffer
   "
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    print (var_one)
"
   (python-tests-look-at "def long_function_name")
   (should (python-info-beginning-of-statement-p))
   (forward-char 10)
   (should (not (python-info-beginning-of-statement-p)))
   (python-tests-look-at "print (var_one)")
   (should (python-info-beginning-of-statement-p))
   (goto-char (line-beginning-position))
   (should (not (python-info-beginning-of-statement-p)))))

(ert-deftest python-info-beginning-of-statement-p-2 ()
  (python-tests-with-temp-buffer
   "
if width == 0 and height == 0 and \\
   color == 'red' and emphasis == 'strong' or \\
   highlight > 100:
    raise ValueError(
'sorry, you lose'

)
"
   (python-tests-look-at "if width == 0 and")
   (should (python-info-beginning-of-statement-p))
   (forward-char 10)
   (should (not (python-info-beginning-of-statement-p)))
   (python-tests-look-at "raise ValueError(")
   (should (python-info-beginning-of-statement-p))
   (goto-char (line-beginning-position))
   (should (not (python-info-beginning-of-statement-p)))))

(ert-deftest python-info-end-of-statement-p-1 ()
  (python-tests-with-temp-buffer
   "
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    print (var_one)
"
   (python-tests-look-at "def long_function_name")
   (should (not (python-info-end-of-statement-p)))
   (end-of-line)
   (should (not (python-info-end-of-statement-p)))
   (python-tests-look-at "print (var_one)")
   (python-util-forward-comment -1)
   (should (python-info-end-of-statement-p))
   (python-tests-look-at "print (var_one)")
   (should (not (python-info-end-of-statement-p)))
   (end-of-line)
   (should (python-info-end-of-statement-p))))

(ert-deftest python-info-end-of-statement-p-2 ()
  (python-tests-with-temp-buffer
   "
if width == 0 and height == 0 and \\
   color == 'red' and emphasis == 'strong' or \\
   highlight > 100:
    raise ValueError(
'sorry, you lose'

)
"
   (python-tests-look-at "if width == 0 and")
   (should (not (python-info-end-of-statement-p)))
   (end-of-line)
   (should (not (python-info-end-of-statement-p)))
   (python-tests-look-at "raise ValueError(")
   (python-util-forward-comment -1)
   (should (python-info-end-of-statement-p))
   (python-tests-look-at "raise ValueError(")
   (should (not (python-info-end-of-statement-p)))
   (end-of-line)
   (should (not (python-info-end-of-statement-p)))
   (goto-char (point-max))
   (python-util-forward-comment -1)
   (should (python-info-end-of-statement-p))))

(ert-deftest python-info-beginning-of-block-p-1 ()
  (python-tests-with-temp-buffer
   "
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    print (var_one)
"
   (python-tests-look-at "def long_function_name")
   (should (python-info-beginning-of-block-p))
   (python-tests-look-at "var_one, var_two, var_three,")
   (should (not (python-info-beginning-of-block-p)))
   (python-tests-look-at "print (var_one)")
   (should (not (python-info-beginning-of-block-p)))))

(ert-deftest python-info-beginning-of-block-p-2 ()
  (python-tests-with-temp-buffer
   "
if width == 0 and height == 0 and \\
   color == 'red' and emphasis == 'strong' or \\
   highlight > 100:
    raise ValueError(
'sorry, you lose'

)
"
   (python-tests-look-at "if width == 0 and")
   (should (python-info-beginning-of-block-p))
   (python-tests-look-at "color == 'red' and emphasis")
   (should (not (python-info-beginning-of-block-p)))
   (python-tests-look-at "raise ValueError(")
   (should (not (python-info-beginning-of-block-p)))))

(ert-deftest python-info-end-of-block-p-1 ()
  (python-tests-with-temp-buffer
   "
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    print (var_one)
"
   (python-tests-look-at "def long_function_name")
   (should (not (python-info-end-of-block-p)))
   (python-tests-look-at "var_one, var_two, var_three,")
   (should (not (python-info-end-of-block-p)))
   (python-tests-look-at "var_four):")
   (end-of-line)
   (should (not (python-info-end-of-block-p)))
   (python-tests-look-at "print (var_one)")
   (should (not (python-info-end-of-block-p)))
   (end-of-line 1)
   (should (python-info-end-of-block-p))))

(ert-deftest python-info-end-of-block-p-2 ()
  (python-tests-with-temp-buffer
   "
if width == 0 and height == 0 and \\
   color == 'red' and emphasis == 'strong' or \\
   highlight > 100:
    raise ValueError(
'sorry, you lose'

)
"
   (python-tests-look-at "if width == 0 and")
   (should (not (python-info-end-of-block-p)))
   (python-tests-look-at "color == 'red' and emphasis == 'strong' or")
   (should (not (python-info-end-of-block-p)))
   (python-tests-look-at "highlight > 100:")
   (end-of-line)
   (should (not (python-info-end-of-block-p)))
   (python-tests-look-at "raise ValueError(")
   (should (not (python-info-end-of-block-p)))
   (end-of-line 1)
   (should (not (python-info-end-of-block-p)))
   (goto-char (point-max))
   (python-util-forward-comment -1)
   (should (python-info-end-of-block-p))))

(ert-deftest python-info-dedenter-opening-block-position-1 ()
  (python-tests-with-temp-buffer
      "
if request.user.is_authenticated():
    try:
        profile = request.user.get_profile()
    except Profile.DoesNotExist:
        profile = Profile.objects.create(user=request.user)
    else:
        if profile.stats:
            profile.recalculate_stats()
        else:
            profile.clear_stats()
    finally:
        profile.views += 1
        profile.save()
"
    (python-tests-look-at "try:")
    (should (not (python-info-dedenter-opening-block-position)))
    (python-tests-look-at "except Profile.DoesNotExist:")
    (should (= (python-tests-look-at "try:" -1 t)
               (python-info-dedenter-opening-block-position)))
    (python-tests-look-at "else:")
    (should (= (python-tests-look-at "except Profile.DoesNotExist:" -1 t)
               (python-info-dedenter-opening-block-position)))
    (python-tests-look-at "if profile.stats:")
    (should (not (python-info-dedenter-opening-block-position)))
    (python-tests-look-at "else:")
    (should (= (python-tests-look-at "if profile.stats:" -1 t)
               (python-info-dedenter-opening-block-position)))
    (python-tests-look-at "finally:")
    (should (= (python-tests-look-at "else:" -2 t)
               (python-info-dedenter-opening-block-position)))))

(ert-deftest python-info-dedenter-opening-block-position-2 ()
  (python-tests-with-temp-buffer
      "
if request.user.is_authenticated():
    profile = Profile.objects.get_or_create(user=request.user)
    if profile.stats:
        profile.recalculate_stats()

data = {
    'else': 'do it'
}
    'else'
"
    (python-tests-look-at "'else': 'do it'")
    (should (not (python-info-dedenter-opening-block-position)))
    (python-tests-look-at "'else'")
    (should (not (python-info-dedenter-opening-block-position)))))

(ert-deftest python-info-dedenter-opening-block-position-3 ()
  (python-tests-with-temp-buffer
      "
if save:
    try:
        write_to_disk(data)
    except IOError:
        msg = 'Error saving to disk'
        message(msg)
        logger.exception(msg)
    except Exception:
        if hide_details:
            logger.exception('Unhandled exception')
            else
    finally:
        data.free()
"
    (python-tests-look-at "try:")
    (should (not (python-info-dedenter-opening-block-position)))

    (python-tests-look-at "except IOError:")
    (should (= (python-tests-look-at "try:" -1 t)
               (python-info-dedenter-opening-block-position)))

    (python-tests-look-at "except Exception:")
    (should (= (python-tests-look-at "except IOError:" -1 t)
               (python-info-dedenter-opening-block-position)))

    (python-tests-look-at "if hide_details:")
    (should (not (python-info-dedenter-opening-block-position)))

    ;; check indentation modifies the detected opening block
    (python-tests-look-at "else")
    (should (= (python-tests-look-at "if hide_details:" -1 t)
               (python-info-dedenter-opening-block-position)))

    (indent-line-to 8)
    (should (= (python-tests-look-at "if hide_details:" -1 t)
               (python-info-dedenter-opening-block-position)))

    (indent-line-to 4)
    (should (= (python-tests-look-at "except Exception:" -1 t)
               (python-info-dedenter-opening-block-position)))

    (indent-line-to 0)
    (should (= (python-tests-look-at "if save:" -1 t)
               (python-info-dedenter-opening-block-position)))))

(ert-deftest python-info-dedenter-opening-block-positions-1 ()
  (python-tests-with-temp-buffer
      "
if save:
    try:
        write_to_disk(data)
    except IOError:
        msg = 'Error saving to disk'
        message(msg)
        logger.exception(msg)
    except Exception:
        if hide_details:
            logger.exception('Unhandled exception')
            else
    finally:
        data.free()
"
    (python-tests-look-at "try:")
    (should (not (python-info-dedenter-opening-block-positions)))

    (python-tests-look-at "except IOError:")
    (should
     (equal (list
             (python-tests-look-at "try:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (python-tests-look-at "except Exception:")
    (should
     (equal (list
             (python-tests-look-at "except IOError:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (python-tests-look-at "if hide_details:")
    (should (not (python-info-dedenter-opening-block-positions)))

    ;; check indentation does not modify the detected opening blocks
    (python-tests-look-at "else")
    (should
     (equal (list
             (python-tests-look-at "if hide_details:" -1 t)
             (python-tests-look-at "except Exception:" -1 t)
             (python-tests-look-at "if save:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (indent-line-to 8)
    (should
     (equal (list
             (python-tests-look-at "if hide_details:" -1 t)
             (python-tests-look-at "except Exception:" -1 t)
             (python-tests-look-at "if save:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (indent-line-to 4)
    (should
     (equal (list
             (python-tests-look-at "if hide_details:" -1 t)
             (python-tests-look-at "except Exception:" -1 t)
             (python-tests-look-at "if save:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (indent-line-to 0)
    (should
     (equal (list
             (python-tests-look-at "if hide_details:" -1 t)
             (python-tests-look-at "except Exception:" -1 t)
             (python-tests-look-at "if save:" -1 t))
            (python-info-dedenter-opening-block-positions)))))

(ert-deftest python-info-dedenter-opening-block-positions-2 ()
  "Test detection of opening blocks for elif."
  (python-tests-with-temp-buffer
      "
if var:
    if var2:
        something()
    elif var3:
        something_else()
        elif
"
    (python-tests-look-at "elif var3:")
    (should
     (equal (list
             (python-tests-look-at "if var2:" -1 t)
             (python-tests-look-at "if var:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (python-tests-look-at "elif\n")
    (should
     (equal (list
             (python-tests-look-at "elif var3:" -1 t)
             (python-tests-look-at "if var:" -1 t))
            (python-info-dedenter-opening-block-positions)))))

(ert-deftest python-info-dedenter-opening-block-positions-3 ()
  "Test detection of opening blocks for else."
  (python-tests-with-temp-buffer
      "
try:
    something()
except:
    if var:
        if var2:
            something()
        elif var3:
            something_else()
            else

if var4:
    while var5:
        var4.pop()
        else

    for value in var6:
        if value > 0:
            print value
            else
"
    (python-tests-look-at "else\n")
    (should
     (equal (list
             (python-tests-look-at "elif var3:" -1 t)
             (python-tests-look-at "if var:" -1 t)
             (python-tests-look-at "except:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (python-tests-look-at "else\n")
    (should
     (equal (list
             (python-tests-look-at "while var5:" -1 t)
             (python-tests-look-at "if var4:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (python-tests-look-at "else\n")
    (should
     (equal (list
             (python-tests-look-at "if value > 0:" -1 t)
             (python-tests-look-at "for value in var6:" -1 t)
             (python-tests-look-at "if var4:" -1 t))
            (python-info-dedenter-opening-block-positions)))))

(ert-deftest python-info-dedenter-opening-block-positions-4 ()
  "Test detection of opening blocks for except."
  (python-tests-with-temp-buffer
      "
try:
    something()
except ValueError:
    something_else()
    except
"
    (python-tests-look-at "except ValueError:")
    (should
     (equal (list (python-tests-look-at "try:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (python-tests-look-at "except\n")
    (should
     (equal (list (python-tests-look-at "except ValueError:" -1 t))
            (python-info-dedenter-opening-block-positions)))))

(ert-deftest python-info-dedenter-opening-block-positions-5 ()
  "Test detection of opening blocks for finally."
  (python-tests-with-temp-buffer
      "
try:
    something()
    finally

try:
    something_else()
except:
    logger.exception('something went wrong')
    finally

try:
    something_else_else()
except Exception:
    logger.exception('something else went wrong')
else:
    print ('all good')
    finally
"
    (python-tests-look-at "finally\n")
    (should
     (equal (list (python-tests-look-at "try:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (python-tests-look-at "finally\n")
    (should
     (equal (list (python-tests-look-at "except:" -1 t))
            (python-info-dedenter-opening-block-positions)))

    (python-tests-look-at "finally\n")
    (should
     (equal (list (python-tests-look-at "else:" -1 t))
            (python-info-dedenter-opening-block-positions)))))

(ert-deftest python-info-dedenter-opening-block-message-1 ()
  "Test dedenters inside strings are ignored."
  (python-tests-with-temp-buffer
      "'''
try:
    something()
except:
    logger.exception('something went wrong')
'''
"
    (python-tests-look-at "except\n")
    (should (not (python-info-dedenter-opening-block-message)))))

(ert-deftest python-info-dedenter-opening-block-message-2 ()
  "Test except keyword."
  (python-tests-with-temp-buffer
      "
try:
    something()
except:
    logger.exception('something went wrong')
"
    (python-tests-look-at "except:")
    (should (string=
             "Closes try:"
             (substring-no-properties
              (python-info-dedenter-opening-block-message))))
    (end-of-line)
    (should (string=
             "Closes try:"
             (substring-no-properties
              (python-info-dedenter-opening-block-message))))))

(ert-deftest python-info-dedenter-opening-block-message-3 ()
  "Test else keyword."
  (python-tests-with-temp-buffer
      "
try:
    something()
except:
    logger.exception('something went wrong')
else:
    logger.debug('all good')
"
    (python-tests-look-at "else:")
    (should (string=
             "Closes except:"
             (substring-no-properties
              (python-info-dedenter-opening-block-message))))
    (end-of-line)
    (should (string=
             "Closes except:"
             (substring-no-properties
              (python-info-dedenter-opening-block-message))))))

(ert-deftest python-info-dedenter-opening-block-message-4 ()
  "Test finally keyword."
  (python-tests-with-temp-buffer
      "
try:
    something()
except:
    logger.exception('something went wrong')
else:
    logger.debug('all good')
finally:
    clean()
"
    (python-tests-look-at "finally:")
    (should (string=
             "Closes else:"
             (substring-no-properties
              (python-info-dedenter-opening-block-message))))
    (end-of-line)
    (should (string=
             "Closes else:"
             (substring-no-properties
              (python-info-dedenter-opening-block-message))))))

(ert-deftest python-info-dedenter-opening-block-message-5 ()
  "Test elif keyword."
  (python-tests-with-temp-buffer
      "
if a:
    something()
elif b:
"
    (python-tests-look-at "elif b:")
    (should (string=
             "Closes if a:"
             (substring-no-properties
              (python-info-dedenter-opening-block-message))))
    (end-of-line)
    (should (string=
             "Closes if a:"
             (substring-no-properties
              (python-info-dedenter-opening-block-message))))))


(ert-deftest python-info-dedenter-statement-p-1 ()
  "Test dedenters inside strings are ignored."
  (python-tests-with-temp-buffer
      "'''
try:
    something()
except:
    logger.exception('something went wrong')
'''
"
    (python-tests-look-at "except\n")
    (should (not (python-info-dedenter-statement-p)))))

(ert-deftest python-info-dedenter-statement-p-2 ()
  "Test except keyword."
  (python-tests-with-temp-buffer
      "
try:
    something()
except:
    logger.exception('something went wrong')
"
    (python-tests-look-at "except:")
    (should (= (point) (python-info-dedenter-statement-p)))
    (end-of-line)
    (should (= (save-excursion
                 (back-to-indentation)
                 (point))
               (python-info-dedenter-statement-p)))))

(ert-deftest python-info-dedenter-statement-p-3 ()
  "Test else keyword."
  (python-tests-with-temp-buffer
      "
try:
    something()
except:
    logger.exception('something went wrong')
else:
    logger.debug('all good')
"
    (python-tests-look-at "else:")
    (should (= (point) (python-info-dedenter-statement-p)))
    (end-of-line)
    (should (= (save-excursion
                 (back-to-indentation)
                 (point))
               (python-info-dedenter-statement-p)))))

(ert-deftest python-info-dedenter-statement-p-4 ()
  "Test finally keyword."
  (python-tests-with-temp-buffer
      "
try:
    something()
except:
    logger.exception('something went wrong')
else:
    logger.debug('all good')
finally:
    clean()
"
    (python-tests-look-at "finally:")
    (should (= (point) (python-info-dedenter-statement-p)))
    (end-of-line)
    (should (= (save-excursion
                 (back-to-indentation)
                 (point))
               (python-info-dedenter-statement-p)))))

(ert-deftest python-info-dedenter-statement-p-5 ()
  "Test elif keyword."
  (python-tests-with-temp-buffer
      "
if a:
    something()
elif b:
"
    (python-tests-look-at "elif b:")
    (should (= (point) (python-info-dedenter-statement-p)))
    (end-of-line)
    (should (= (save-excursion
                 (back-to-indentation)
                 (point))
               (python-info-dedenter-statement-p)))))

(ert-deftest python-info-line-ends-backslash-p-1 ()
  (python-tests-with-temp-buffer
   "
objects = Thing.objects.all() \\
                       .filter(
                           type='toy',
                           status='bought'
                       ) \\
                       .aggregate(
                           Sum('amount')
                       ) \\
                       .values_list()
"
   (should (python-info-line-ends-backslash-p 2)) ; .filter(...
   (should (python-info-line-ends-backslash-p 3))
   (should (python-info-line-ends-backslash-p 4))
   (should (python-info-line-ends-backslash-p 5))
   (should (python-info-line-ends-backslash-p 6)) ; ) \...
   (should (python-info-line-ends-backslash-p 7))
   (should (python-info-line-ends-backslash-p 8))
   (should (python-info-line-ends-backslash-p 9))
   (should (not (python-info-line-ends-backslash-p 10))))) ; .values_list()...

(ert-deftest python-info-beginning-of-backslash-1 ()
  (python-tests-with-temp-buffer
   "
objects = Thing.objects.all() \\
                       .filter(
                           type='toy',
                           status='bought'
                       ) \\
                       .aggregate(
                           Sum('amount')
                       ) \\
                       .values_list()
"
   (let ((first 2)
         (second (python-tests-look-at ".filter("))
         (third (python-tests-look-at ".aggregate(")))
     (should (= first (python-info-beginning-of-backslash 2)))
     (should (= second (python-info-beginning-of-backslash 3)))
     (should (= second (python-info-beginning-of-backslash 4)))
     (should (= second (python-info-beginning-of-backslash 5)))
     (should (= second (python-info-beginning-of-backslash 6)))
     (should (= third (python-info-beginning-of-backslash 7)))
     (should (= third (python-info-beginning-of-backslash 8)))
     (should (= third (python-info-beginning-of-backslash 9)))
     (should (not (python-info-beginning-of-backslash 10))))))

(ert-deftest python-info-continuation-line-p-1 ()
  (python-tests-with-temp-buffer
   "
if width == 0 and height == 0 and \\
   color == 'red' and emphasis == 'strong' or \\
   highlight > 100:
    raise ValueError(
'sorry, you lose'

)
"
   (python-tests-look-at "if width == 0 and height == 0 and")
   (should (not (python-info-continuation-line-p)))
   (python-tests-look-at "color == 'red' and emphasis == 'strong' or")
   (should (python-info-continuation-line-p))
   (python-tests-look-at "highlight > 100:")
   (should (python-info-continuation-line-p))
   (python-tests-look-at "raise ValueError(")
   (should (not (python-info-continuation-line-p)))
   (python-tests-look-at "'sorry, you lose'")
   (should (python-info-continuation-line-p))
   (forward-line 1)
   (should (python-info-continuation-line-p))
   (python-tests-look-at ")")
   (should (python-info-continuation-line-p))
   (forward-line 1)
   (should (not (python-info-continuation-line-p)))))

(ert-deftest python-info-block-continuation-line-p-1 ()
  (python-tests-with-temp-buffer
   "
if width == 0 and height == 0 and \\
   color == 'red' and emphasis == 'strong' or \\
   highlight > 100:
    raise ValueError(
'sorry, you lose'

)
"
   (python-tests-look-at "if width == 0 and")
   (should (not (python-info-block-continuation-line-p)))
   (python-tests-look-at "color == 'red' and emphasis == 'strong' or")
   (should (= (python-info-block-continuation-line-p)
              (python-tests-look-at "if width == 0 and" -1 t)))
   (python-tests-look-at "highlight > 100:")
   (should (not (python-info-block-continuation-line-p)))))

(ert-deftest python-info-block-continuation-line-p-2 ()
  (python-tests-with-temp-buffer
   "
def foo(a,
        b,
        c):
    pass
"
   (python-tests-look-at "def foo(a,")
   (should (not (python-info-block-continuation-line-p)))
   (python-tests-look-at "b,")
   (should (= (python-info-block-continuation-line-p)
              (python-tests-look-at "def foo(a," -1 t)))
   (python-tests-look-at "c):")
   (should (not (python-info-block-continuation-line-p)))))

(ert-deftest python-info-assignment-statement-p-1 ()
  (python-tests-with-temp-buffer
   "
data = foo(), bar() \\
       baz(), 4 \\
       5, 6
"
   (python-tests-look-at "data = foo(), bar()")
   (should (python-info-assignment-statement-p))
   (should (python-info-assignment-statement-p t))
   (python-tests-look-at "baz(), 4")
   (should (python-info-assignment-statement-p))
   (should (not (python-info-assignment-statement-p t)))
   (python-tests-look-at "5, 6")
   (should (python-info-assignment-statement-p))
   (should (not (python-info-assignment-statement-p t)))))

(ert-deftest python-info-assignment-statement-p-2 ()
  (python-tests-with-temp-buffer
   "
data = (foo(), bar()
        baz(), 4
        5, 6)
"
   (python-tests-look-at "data = (foo(), bar()")
   (should (python-info-assignment-statement-p))
   (should (python-info-assignment-statement-p t))
   (python-tests-look-at "baz(), 4")
   (should (python-info-assignment-statement-p))
   (should (not (python-info-assignment-statement-p t)))
   (python-tests-look-at "5, 6)")
   (should (python-info-assignment-statement-p))
   (should (not (python-info-assignment-statement-p t)))))

(ert-deftest python-info-assignment-statement-p-3 ()
  (python-tests-with-temp-buffer
   "
data '=' 42
"
   (python-tests-look-at "data '=' 42")
   (should (not (python-info-assignment-statement-p)))
   (should (not (python-info-assignment-statement-p t)))))

(ert-deftest python-info-assignment-continuation-line-p-1 ()
  (python-tests-with-temp-buffer
   "
data = foo(), bar() \\
       baz(), 4 \\
       5, 6
"
   (python-tests-look-at "data = foo(), bar()")
   (should (not (python-info-assignment-continuation-line-p)))
   (python-tests-look-at "baz(), 4")
   (should (= (python-info-assignment-continuation-line-p)
              (python-tests-look-at "foo()," -1 t)))
   (python-tests-look-at "5, 6")
   (should (not (python-info-assignment-continuation-line-p)))))

(ert-deftest python-info-assignment-continuation-line-p-2 ()
  (python-tests-with-temp-buffer
   "
data = (foo(), bar()
        baz(), 4
        5, 6)
"
   (python-tests-look-at "data = (foo(), bar()")
   (should (not (python-info-assignment-continuation-line-p)))
   (python-tests-look-at "baz(), 4")
   (should (= (python-info-assignment-continuation-line-p)
              (python-tests-look-at "(foo()," -1 t)))
   (python-tests-look-at "5, 6)")
   (should (not (python-info-assignment-continuation-line-p)))))

(ert-deftest python-info-looking-at-beginning-of-defun-1 ()
  (python-tests-with-temp-buffer
   "
def decorat0r(deff):
    '''decorates stuff.

    @decorat0r
    def foo(arg):
        ...
    '''
    def wrap():
        deff()
    return wwrap
"
   (python-tests-look-at "def decorat0r(deff):")
   (should (python-info-looking-at-beginning-of-defun))
   (python-tests-look-at "def foo(arg):")
   (should (not (python-info-looking-at-beginning-of-defun)))
   (python-tests-look-at "def wrap():")
   (should (python-info-looking-at-beginning-of-defun))
   (python-tests-look-at "deff()")
   (should (not (python-info-looking-at-beginning-of-defun)))))

(ert-deftest python-info-current-line-comment-p-1 ()
  (python-tests-with-temp-buffer
   "
# this is a comment
foo = True  # another comment
'#this is a string'
if foo:
    # more comments
    print ('bar') # print bar
"
   (python-tests-look-at "# this is a comment")
   (should (python-info-current-line-comment-p))
   (python-tests-look-at "foo = True  # another comment")
   (should (not (python-info-current-line-comment-p)))
   (python-tests-look-at "'#this is a string'")
   (should (not (python-info-current-line-comment-p)))
   (python-tests-look-at "# more comments")
   (should (python-info-current-line-comment-p))
   (python-tests-look-at "print ('bar') # print bar")
   (should (not (python-info-current-line-comment-p)))))

(ert-deftest python-info-current-line-empty-p ()
  (python-tests-with-temp-buffer
   "
# this is a comment

foo = True  # another comment
"
   (should (python-info-current-line-empty-p))
   (python-tests-look-at "# this is a comment")
   (should (not (python-info-current-line-empty-p)))
   (forward-line 1)
   (should (python-info-current-line-empty-p))))

(ert-deftest python-info-docstring-p-1 ()
  "Test module docstring detection."
  (python-tests-with-temp-buffer
   "# -*- coding: utf-8 -*-
#!/usr/bin/python

'''
Module Docstring Django style.
'''
u'''Additional module docstring.'''
'''Not a module docstring.'''
"
   (python-tests-look-at "Module Docstring Django style.")
   (should (python-info-docstring-p))
   (python-tests-look-at "u'''Additional module docstring.'''")
   (should (python-info-docstring-p))
   (python-tests-look-at "'''Not a module docstring.'''")
   (should (not (python-info-docstring-p)))))

(ert-deftest python-info-docstring-p-2 ()
  "Test variable docstring detection."
  (python-tests-with-temp-buffer
   "
variable = 42
U'''Variable docstring.'''
'''Additional variable docstring.'''
'''Not a variable docstring.'''
"
   (python-tests-look-at "Variable docstring.")
   (should (python-info-docstring-p))
   (python-tests-look-at "u'''Additional variable docstring.'''")
   (should (python-info-docstring-p))
   (python-tests-look-at "'''Not a variable docstring.'''")
   (should (not (python-info-docstring-p)))))

(ert-deftest python-info-docstring-p-3 ()
  "Test function docstring detection."
  (python-tests-with-temp-buffer
   "
def func(a, b):
    r'''
    Function docstring.

    onetwo style.
    '''
    R'''Additional function docstring.'''
    '''Not a function docstring.'''
    return a + b
"
   (python-tests-look-at "Function docstring.")
   (should (python-info-docstring-p))
   (python-tests-look-at "R'''Additional function docstring.'''")
   (should (python-info-docstring-p))
   (python-tests-look-at "'''Not a function docstring.'''")
   (should (not (python-info-docstring-p)))))

(ert-deftest python-info-docstring-p-4 ()
  "Test class docstring detection."
  (python-tests-with-temp-buffer
   "
class Class:
    ur'''
    Class docstring.

    symmetric style.
    '''
    uR'''
    Additional class docstring.
    '''
    '''Not a class docstring.'''
    pass
"
   (python-tests-look-at "Class docstring.")
   (should (python-info-docstring-p))
   (python-tests-look-at "uR'''")  ;; Additional class docstring
   (should (python-info-docstring-p))
   (python-tests-look-at "'''Not a class docstring.'''")
   (should (not (python-info-docstring-p)))))

(ert-deftest python-info-docstring-p-5 ()
  "Test class attribute docstring detection."
  (python-tests-with-temp-buffer
   "
class Class:
    attribute = 42
    Ur'''
    Class attribute docstring.

    pep-257 style.

    '''
    UR'''
    Additional class attribute docstring.
    '''
    '''Not a class attribute docstring.'''
    pass
"
   (python-tests-look-at "Class attribute docstring.")
   (should (python-info-docstring-p))
   (python-tests-look-at "UR'''")  ;; Additional class attr docstring
   (should (python-info-docstring-p))
   (python-tests-look-at "'''Not a class attribute docstring.'''")
   (should (not (python-info-docstring-p)))))

(ert-deftest python-info-docstring-p-6 ()
  "Test class method docstring detection."
  (python-tests-with-temp-buffer
   "
class Class:

    def __init__(self, a, b):
        self.a = a
        self.b = b

    def __call__(self):
        '''Method docstring.

        pep-257-nn style.
        '''
        '''Additional method docstring.'''
        '''Not a method docstring.'''
        return self.a + self.b
"
   (python-tests-look-at "Method docstring.")
   (should (python-info-docstring-p))
   (python-tests-look-at "'''Additional method docstring.'''")
   (should (python-info-docstring-p))
   (python-tests-look-at "'''Not a method docstring.'''")
   (should (not (python-info-docstring-p)))))

(ert-deftest python-info-encoding-from-cookie-1 ()
  "Should detect it on first line."
  (python-tests-with-temp-buffer
   "# coding=latin-1

foo = True  # another comment
"
   (should (eq (python-info-encoding-from-cookie) 'latin-1))))

(ert-deftest python-info-encoding-from-cookie-2 ()
  "Should detect it on second line."
  (python-tests-with-temp-buffer
   "
# coding=latin-1

foo = True  # another comment
"
   (should (eq (python-info-encoding-from-cookie) 'latin-1))))

(ert-deftest python-info-encoding-from-cookie-3 ()
  "Should not be detected on third line (and following ones)."
  (python-tests-with-temp-buffer
   "

# coding=latin-1
foo = True  # another comment
"
   (should (not (python-info-encoding-from-cookie)))))

(ert-deftest python-info-encoding-from-cookie-4 ()
  "Should detect Emacs style."
  (python-tests-with-temp-buffer
   "# -*- coding: latin-1 -*-

foo = True  # another comment"
   (should (eq (python-info-encoding-from-cookie) 'latin-1))))

(ert-deftest python-info-encoding-from-cookie-5 ()
  "Should detect Vim style."
  (python-tests-with-temp-buffer
   "# vim: set fileencoding=latin-1 :

foo = True  # another comment"
   (should (eq (python-info-encoding-from-cookie) 'latin-1))))

(ert-deftest python-info-encoding-from-cookie-6 ()
  "First cookie wins."
  (python-tests-with-temp-buffer
   "# -*- coding: iso-8859-1 -*-
# vim: set fileencoding=latin-1 :

foo = True  # another comment"
   (should (eq (python-info-encoding-from-cookie) 'iso-8859-1))))

(ert-deftest python-info-encoding-from-cookie-7 ()
  "First cookie wins."
  (python-tests-with-temp-buffer
   "# vim: set fileencoding=latin-1 :
# -*- coding: iso-8859-1 -*-

foo = True  # another comment"
   (should (eq (python-info-encoding-from-cookie) 'latin-1))))

(ert-deftest python-info-encoding-1 ()
  "Should return the detected encoding from cookie."
  (python-tests-with-temp-buffer
   "# vim: set fileencoding=latin-1 :

foo = True  # another comment"
   (should (eq (python-info-encoding) 'latin-1))))

(ert-deftest python-info-encoding-2 ()
  "Should default to utf-8."
  (python-tests-with-temp-buffer
   "# No encoding for you

foo = True  # another comment"
   (should (eq (python-info-encoding) 'utf-8))))


;;; Utility functions

(ert-deftest python-util-goto-line-1 ()
  (python-tests-with-temp-buffer
   (concat
    "# a comment
# another comment
def foo(a, b, c):
    pass" (make-string 20 ?\n))
   (python-util-goto-line 10)
   (should (= (line-number-at-pos) 10))
   (python-util-goto-line 20)
   (should (= (line-number-at-pos) 20))))

(ert-deftest python-util-clone-local-variables-1 ()
  (let ((buffer (generate-new-buffer
                 "python-util-clone-local-variables-1"))
        (varcons
         '((python-fill-docstring-style . django)
           (python-shell-interpreter . "python")
           (python-shell-interpreter-args . "manage.py shell")
           (python-shell-prompt-regexp . "In \\[[0-9]+\\]: ")
           (python-shell-prompt-output-regexp . "Out\\[[0-9]+\\]: ")
           (python-shell-extra-pythonpaths "/home/user/pylib/")
           (python-shell-completion-setup-code
            . "from IPython.core.completerlib import module_completion")
           (python-shell-completion-string-code
            . "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
           (python-shell-virtualenv-root
            . "/home/user/.virtualenvs/project"))))
    (with-current-buffer buffer
      (kill-all-local-variables)
      (dolist (ccons varcons)
        (set (make-local-variable (car ccons)) (cdr ccons))))
    (python-tests-with-temp-buffer
     ""
     (python-util-clone-local-variables buffer)
     (dolist (ccons varcons)
       (should
        (equal (symbol-value (car ccons)) (cdr ccons)))))
    (kill-buffer buffer)))

(ert-deftest python-util-strip-string-1 ()
  (should (string= (python-util-strip-string "\t\r\n    str") "str"))
  (should (string= (python-util-strip-string "str \n\r") "str"))
  (should (string= (python-util-strip-string "\t\r\n    str \n\r ") "str"))
  (should
   (string= (python-util-strip-string "\n str \nin \tg \n\r") "str \nin \tg"))
  (should (string= (python-util-strip-string "\n \t \n\r ") ""))
  (should (string= (python-util-strip-string "") "")))

(ert-deftest python-util-forward-comment-1 ()
  (python-tests-with-temp-buffer
   (concat
    "# a comment
# another comment
     # bad indented comment
# more comments" (make-string 9999 ?\n))
   (python-util-forward-comment 1)
   (should (= (point) (point-max)))
   (python-util-forward-comment -1)
   (should (= (point) (point-min)))))

(ert-deftest python-util-valid-regexp-p-1 ()
  (should (python-util-valid-regexp-p ""))
  (should (python-util-valid-regexp-p python-shell-prompt-regexp))
  (should (not (python-util-valid-regexp-p "\\("))))


;;; Electricity

(ert-deftest python-parens-electric-indent-1 ()
  (let ((eim electric-indent-mode))
    (unwind-protect
        (progn
          (python-tests-with-temp-buffer
           "
from django.conf.urls import patterns, include, url

from django.contrib import admin

from myapp import views


urlpatterns = patterns('',
    url(r'^$', views.index
)
"
           (electric-indent-mode 1)
           (python-tests-look-at "views.index")
           (end-of-line)

           ;; Inserting commas within the same line should leave
           ;; indentation unchanged.
           (python-tests-self-insert ",")
           (should (= (current-indentation) 4))

           ;; As well as any other input happening within the same
           ;; set of parens.
           (python-tests-self-insert " name='index')")
           (should (= (current-indentation) 4))

           ;; But a comma outside it, should trigger indentation.
           (python-tests-self-insert ",")
           (should (= (current-indentation) 23))

           ;; Newline indents to the first argument column
           (python-tests-self-insert "\n")
           (should (= (current-indentation) 23))

           ;; All this input must not change indentation
           (indent-line-to 4)
           (python-tests-self-insert "url(r'^/login$', views.login)")
           (should (= (current-indentation) 4))

           ;; But this comma does
           (python-tests-self-insert ",")
           (should (= (current-indentation) 23))))
      (or eim (electric-indent-mode -1)))))

(ert-deftest python-triple-quote-pairing ()
  (let ((epm electric-pair-mode))
    (unwind-protect
        (progn
          (python-tests-with-temp-buffer
           "\"\"\n"
           (or epm (electric-pair-mode 1))
           (goto-char (1- (point-max)))
           (python-tests-self-insert ?\")
           (should (string= (buffer-string)
                            "\"\"\"\"\"\"\n"))
           (should (= (point) 4)))
          (python-tests-with-temp-buffer
           "\n"
           (python-tests-self-insert (list ?\" ?\" ?\"))
           (should (string= (buffer-string)
                            "\"\"\"\"\"\"\n"))
           (should (= (point) 4)))
          (python-tests-with-temp-buffer
           "\"\n\"\"\n"
           (goto-char (1- (point-max)))
           (python-tests-self-insert ?\")
           (should (= (point) (1- (point-max))))
           (should (string= (buffer-string)
                            "\"\n\"\"\"\n"))))
      (or epm (electric-pair-mode -1)))))


;;; Hideshow support

(ert-deftest python-hideshow-hide-levels-1 ()
  "Should hide all methods when called after class start."
  (let ((enabled hs-minor-mode))
    (unwind-protect
        (progn
          (python-tests-with-temp-buffer
           "
class SomeClass:

    def __init__(self, arg, kwarg=1):
        self.arg = arg
        self.kwarg = kwarg

    def filter(self, nums):
        def fn(item):
            return item in [self.arg, self.kwarg]
        return filter(fn, nums)

    def __str__(self):
        return '%s-%s' % (self.arg, self.kwarg)
"
           (hs-minor-mode 1)
           (python-tests-look-at "class SomeClass:")
           (forward-line)
           (hs-hide-level 1)
           (should
            (string=
             (python-tests-visible-string)
             "
class SomeClass:

    def __init__(self, arg, kwarg=1):
    def filter(self, nums):
    def __str__(self):"))))
      (or enabled (hs-minor-mode -1)))))

(ert-deftest python-hideshow-hide-levels-2 ()
  "Should hide nested methods and parens at end of defun."
  (let ((enabled hs-minor-mode))
    (unwind-protect
        (progn
          (python-tests-with-temp-buffer
           "
class SomeClass:

    def __init__(self, arg, kwarg=1):
        self.arg = arg
        self.kwarg = kwarg

    def filter(self, nums):
        def fn(item):
            return item in [self.arg, self.kwarg]
        return filter(fn, nums)

    def __str__(self):
        return '%s-%s' % (self.arg, self.kwarg)
"
           (hs-minor-mode 1)
           (python-tests-look-at "def fn(item):")
           (hs-hide-block)
           (should
            (string=
             (python-tests-visible-string)
             "
class SomeClass:

    def __init__(self, arg, kwarg=1):
        self.arg = arg
        self.kwarg = kwarg

    def filter(self, nums):
        def fn(item):
        return filter(fn, nums)

    def __str__(self):
        return '%s-%s' % (self.arg, self.kwarg)
"))))
      (or enabled (hs-minor-mode -1)))))


(ert-deftest python-tests--python-nav-end-of-statement--infloop ()
  "Checks that `python-nav-end-of-statement' doesn't infloop in a
buffer with overlapping strings."
  (python-tests-with-temp-buffer "''' '\n''' ' '\n"
    (syntax-propertize (point-max))
    ;; Create a situation where strings nominally overlap.  This
    ;; shouldn't happen in practice, but apparently it can happen when
    ;; a package calls `syntax-ppss' in a narrowed buffer during JIT
    ;; lock.
    (put-text-property 4 5 'syntax-table (string-to-syntax "|"))
    (remove-text-properties 8 9 '(syntax-table nil))
    (goto-char 4)
    (setq-local syntax-propertize-function nil)
    ;; The next form should not infloop.  We have to disable
    ;; ‘debug-on-error’ so that ‘cl-assert’ doesn’t call the debugger.
    (should-error (let ((debug-on-error nil))
                    (python-nav-end-of-statement)))
    (should (eolp))))


(provide 'python-tests)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; python-tests.el ends here
