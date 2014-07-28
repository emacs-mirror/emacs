;;; python-tests.el --- Test suite for python.el

;; Copyright (C) 2013-2014 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'python)

(defmacro python-tests-with-temp-buffer (contents &rest body)
  "Create a `python-mode' enabled temp buffer with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (python-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defmacro python-tests-with-temp-file (contents &rest body)
  "Create a `python-mode' enabled file with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  ;; temp-file never actually used for anything?
  `(let* ((temp-file (make-temp-file "python-tests" nil ".py"))
          (buffer (find-file-noselect temp-file)))
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
   (should (eq (car (python-indent-context)) 'no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "foo = long_function_name(var_one, var_two,")
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "var_three, var_four)")
   (should (eq (car (python-indent-context)) 'inside-paren))
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
   (should (eq (car (python-indent-context)) 'no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "def long_function_name(")
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "var_one, var_two, var_three,")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "var_four):")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "print (var_one)")
   (should (eq (car (python-indent-context)) 'after-beginning-of-block))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-pep8-3 ()
  "Third pep8 case."
  (python-tests-with-temp-buffer
   "# Extra indentation is not necessary.
foo = long_function_name(
  var_one, var_two,
  var_three, var_four)
"
   (should (eq (car (python-indent-context)) 'no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "foo = long_function_name(")
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "var_one, var_two,")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "var_three, var_four)")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 4))))

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
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "# terminal state")
   (should (eq (car (python-indent-context)) 'after-comment))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "# with the exception")
   (should (eq (car (python-indent-context)) 'after-comment))
   ;; This one indents relative to previous block, even given the fact
   ;; that it was under-indented.
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "# terminal state" -1)
   ;; It doesn't hurt to check again.
   (should (eq (car (python-indent-context)) 'after-comment))
   (python-indent-line)
   (should (= (current-indentation) 8))
   (python-tests-look-at "# with the exception")
   (should (eq (car (python-indent-context)) 'after-comment))
   ;; Now everything should be lined up.
   (should (= (python-indent-calculate-indentation) 8))))

(ert-deftest python-indent-after-comment-2 ()
  "Test after-comment in weird cases."
  (python-tests-with-temp-buffer
   "# Contents will be modified to correct indentation
def func(arg):
    # I don't do much
    return arg
    # This comment is badly indented just because.
    # But we won't mess with the user in this line.

now_we_do_mess_cause_this_is_not_a_comment = 1

# yeah, that.
"
   (python-tests-look-at "# I don't do much")
   (should (eq (car (python-indent-context)) 'after-beginning-of-block))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "return arg")
   ;; Comment here just gets ignored, this line is not a comment so
   ;; the rules won't apply here.
   (should (eq (car (python-indent-context)) 'after-beginning-of-block))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "# This comment is badly")
   (should (eq (car (python-indent-context)) 'after-line))
   ;; The return keyword moves indentation backwards 4 spaces, but
   ;; let's assume this comment was placed there because the user
   ;; wanted to (manually adding spaces or whatever).
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "# but we won't mess")
   (should (eq (car (python-indent-context)) 'after-comment))
   (should (= (python-indent-calculate-indentation) 4))
   ;; Behave the same for blank lines: potentially a comment.
   (forward-line 1)
   (should (eq (car (python-indent-context)) 'after-comment))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "now_we_do_mess")
   ;; Here is where comment indentation starts to get ignored and
   ;; where the user can't freely indent anymore.
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "# yeah, that.")
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))))

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
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "'key':")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "{")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "'objlist': [")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "{")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 12))
   (python-tests-look-at "'pk': 1,")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 16))
   (python-tests-look-at "'name': 'first',")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 16))
   (python-tests-look-at "},")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 12))
   (python-tests-look-at "{")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 12))
   (python-tests-look-at "'pk': 2,")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 16))
   (python-tests-look-at "'name': 'second',")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 16))
   (python-tests-look-at "}")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 12))
   (python-tests-look-at "]")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "}")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "}")
   (should (eq (car (python-indent-context)) 'inside-paren))
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
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "'objlist': [")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "{'pk': 1,")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "'name': 'first'},")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 9))
   (python-tests-look-at "{'pk': 2,")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 8))
   (python-tests-look-at "'name': 'second'}")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 9))
   (python-tests-look-at "]")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "}}")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "}")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 0))))

(ert-deftest python-indent-after-block-1 ()
  "The most simple after-block case that shouldn't fail."
  (python-tests-with-temp-buffer
   "
def foo(a, b, c=True):
"
   (should (eq (car (python-indent-context)) 'no-indent))
   (should (= (python-indent-calculate-indentation) 0))
   (goto-char (point-max))
   (should (eq (car (python-indent-context)) 'after-beginning-of-block))
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
   (should (eq (car (python-indent-context)) 'after-beginning-of-block))
   (should (= (python-indent-calculate-indentation) 4))))

(ert-deftest python-indent-after-backslash-1 ()
  "The most common case."
  (python-tests-with-temp-buffer
   "
from foo.bar.baz import something, something_1 \\\\
    something_2 something_3, \\\\
    something_4, something_5
"
   (python-tests-look-at "from foo.bar.baz import something, something_1")
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at "something_2 something_3,")
   (should (eq (car (python-indent-context)) 'after-backslash))
   (should (= (python-indent-calculate-indentation) 4))
   (python-tests-look-at "something_4, something_5")
   (should (eq (car (python-indent-context)) 'after-backslash))
   (should (= (python-indent-calculate-indentation) 4))
   (goto-char (point-max))
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))))

(ert-deftest python-indent-after-backslash-2 ()
  "A pretty extreme complicated case."
  (python-tests-with-temp-buffer
   "
objects = Thing.objects.all() \\\\
                       .filter(
                           type='toy',
                           status='bought'
                       ) \\\\
                       .aggregate(
                           Sum('amount')
                       ) \\\\
                       .values_list()
"
   (python-tests-look-at "objects = Thing.objects.all()")
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))
   (python-tests-look-at ".filter(")
   (should (eq (car (python-indent-context)) 'after-backslash))
   (should (= (python-indent-calculate-indentation) 23))
   (python-tests-look-at "type='toy',")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 27))
   (python-tests-look-at "status='bought'")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 27))
   (python-tests-look-at ") \\\\")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 23))
   (python-tests-look-at ".aggregate(")
   (should (eq (car (python-indent-context)) 'after-backslash))
   (should (= (python-indent-calculate-indentation) 23))
   (python-tests-look-at "Sum('amount')")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 27))
   (python-tests-look-at ") \\\\")
   (should (eq (car (python-indent-context)) 'inside-paren))
   (should (= (python-indent-calculate-indentation) 23))
   (python-tests-look-at ".values_list()")
   (should (eq (car (python-indent-context)) 'after-backslash))
   (should (= (python-indent-calculate-indentation) 23))
   (forward-line 1)
   (should (eq (car (python-indent-context)) 'after-line))
   (should (= (python-indent-calculate-indentation) 0))))

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
    (should (eq (car (python-indent-context)) 'dedenter-statement))
    (should (= (python-indent-calculate-indentation) 0))
    (should (equal (python-indent-calculate-levels) '(0)))))

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
    (should (eq (car (python-indent-context)) 'dedenter-statement))
    (should (= (python-indent-calculate-indentation) 8))
    (should (equal (python-indent-calculate-levels) '(0 4 8)))))

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
    (should (eq (car (python-indent-context)) 'dedenter-statement))
    (should (= (python-indent-calculate-indentation) 4))
    (should (equal (python-indent-calculate-levels) '(4)))))

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
    (should (eq (car (python-indent-context)) 'dedenter-statement))
    (should (= (python-indent-calculate-indentation) 4))
    (should (equal (python-indent-calculate-levels) '(4)))))

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
    (should (eq (car (python-indent-context)) 'dedenter-statement))
    (should (= (python-indent-calculate-indentation) 8))
    (should (equal (python-indent-calculate-levels) '(0 8)))))

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
    (should (eq (car (python-indent-context)) 'dedenter-statement))
    (should (= (python-indent-calculate-indentation) 0))
    (should (equal (python-indent-calculate-levels) '(0)))))

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
    (should (eq (car (python-indent-context)) 'dedenter-statement))
    (should (= (python-indent-calculate-indentation) 0))
    (should (equal (python-indent-calculate-levels) '(0)))))


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
v1 = 123 + \
     456 + \
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
v1 = 123 + \
     456 + \
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
v1 = 123 + \
     456 + \
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
v1 = 123 + \
     456 + \
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
v1 = 123 + \
     456 + \
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
   ;; Movement next to a paren should do what lisp does and
   ;; unfortunately It can't change, because otherwise
   ;; `blink-matching-open' breaks.
   (python-nav-forward-sexp -1)
   (should (looking-at "()"))
   (should (save-excursion
             (beginning-of-line)
             (looking-at "c()")))
   (python-nav-forward-sexp -1)
   (should (looking-at "c()"))
   (python-nav-forward-sexp -1)
   (should (looking-at "b()"))
   (python-nav-forward-sexp -1)
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


;;; Shell integration

(defvar python-tests-shell-interpreter "python")

(ert-deftest python-shell-get-process-name-1 ()
  "Check process name calculation on different scenarios."
  (python-tests-with-temp-buffer
      ""
    (should (string= (python-shell-get-process-name nil)
                     python-shell-buffer-name))
    ;; When the `current-buffer' doesn't have `buffer-file-name', even
    ;; if dedicated flag is non-nil should not include its name.
    (should (string= (python-shell-get-process-name t)
                     python-shell-buffer-name)))
  (python-tests-with-temp-file
      ""
    ;; `buffer-file-name' is non-nil but the dedicated flag is nil and
    ;; should be respected.
    (should (string= (python-shell-get-process-name nil)
                     python-shell-buffer-name))
    (should (string=
             (python-shell-get-process-name t)
             (format "%s[%s]" python-shell-buffer-name buffer-file-name)))))

(ert-deftest python-shell-internal-get-process-name-1 ()
  "Check the internal process name is config-unique."
  (let* ((python-shell-interpreter python-tests-shell-interpreter)
         (python-shell-interpreter-args "")
         (python-shell-prompt-regexp ">>> ")
         (python-shell-prompt-block-regexp "[.][.][.] ")
         (python-shell-setup-codes "")
         (python-shell-process-environment "")
         (python-shell-extra-pythonpaths "")
         (python-shell-exec-path "")
         (python-shell-virtualenv-path "")
         (expected (python-tests-with-temp-buffer
                       "" (python-shell-internal-get-process-name))))
    ;; Same configurations should match.
    (should
     (string= expected
              (python-tests-with-temp-buffer
                  "" (python-shell-internal-get-process-name))))
    (let ((python-shell-interpreter-args "-B"))
      ;; A minimal change should generate different names.
      (should
       (not (string=
             expected
             (python-tests-with-temp-buffer
                 "" (python-shell-internal-get-process-name))))))))

(ert-deftest python-shell-parse-command-1 ()
  "Check the command to execute is calculated correctly.
Using `python-shell-interpreter' and
`python-shell-interpreter-args'."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (let ((python-shell-interpreter (executable-find
                                   python-tests-shell-interpreter))
        (python-shell-interpreter-args "-B"))
    (should (string=
             (format "%s %s"
                     python-shell-interpreter
                     python-shell-interpreter-args)
             (python-shell-parse-command)))))

(ert-deftest python-shell-calculate-process-environment-1 ()
  "Test `python-shell-process-environment' modification."
  (let* ((original-process-environment process-environment)
         (python-shell-process-environment
          '("TESTVAR1=value1" "TESTVAR2=value2"))
         (process-environment
          (python-shell-calculate-process-environment)))
    (should (equal (getenv "TESTVAR1") "value1"))
    (should (equal (getenv "TESTVAR2") "value2"))))

(ert-deftest python-shell-calculate-process-environment-2 ()
  "Test `python-shell-extra-pythonpaths' modification."
  (let* ((original-process-environment process-environment)
         (original-pythonpath (getenv "PYTHONPATH"))
         (paths '("path1" "path2"))
         (python-shell-extra-pythonpaths paths)
         (process-environment
          (python-shell-calculate-process-environment)))
    (should (equal (getenv "PYTHONPATH")
                   (concat
                    (mapconcat 'identity paths path-separator)
                    path-separator original-pythonpath)))))

(ert-deftest python-shell-calculate-process-environment-3 ()
  "Test `python-shell-virtualenv-path' modification."
  (let* ((original-process-environment process-environment)
         (original-path (or (getenv "PATH") ""))
         (python-shell-virtualenv-path
          (directory-file-name user-emacs-directory))
         (process-environment
          (python-shell-calculate-process-environment)))
    (should (not (getenv "PYTHONHOME")))
    (should (string= (getenv "VIRTUAL_ENV") python-shell-virtualenv-path))
    (should (equal (getenv "PATH")
                   (format "%s/bin%s%s"
                           python-shell-virtualenv-path
                           path-separator original-path)))))

(ert-deftest python-shell-calculate-exec-path-1 ()
  "Test `python-shell-exec-path' modification."
  (let* ((original-exec-path exec-path)
         (python-shell-exec-path '("path1" "path2"))
         (exec-path (python-shell-calculate-exec-path)))
    (should (equal
             exec-path
             (append python-shell-exec-path
                     original-exec-path)))))

(ert-deftest python-shell-calculate-exec-path-2 ()
  "Test `python-shell-exec-path' modification."
  (let* ((original-exec-path exec-path)
         (python-shell-virtualenv-path
          (directory-file-name user-emacs-directory))
         (exec-path (python-shell-calculate-exec-path)))
    (should (equal
             exec-path
             (append (cons
                      (format "%s/bin" python-shell-virtualenv-path)
                      original-exec-path))))))

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
               (python-shell-parse-command) proc-name)))
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
               (python-shell-parse-command) proc-name nil t)))
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
            (should (string= python-shell-interpreter
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
                 (python-shell-parse-command) proc-name nil))))
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
             (python-shell-parse-command) global-proc-name))
           (dedicated-shell-buffer
            (python-shell-make-comint
             (python-shell-parse-command) dedicated-proc-name))
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

(ert-deftest python-shell-get-or-create-process-1 ()
  "Check shell dedicated process creation."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (python-tests-with-temp-file
   ""
   (let* ((python-shell-interpreter
           (executable-find python-tests-shell-interpreter))
          (use-dialog-box)
          (dedicated-process-name (python-shell-get-process-name t))
          (dedicated-process
           (python-shell-get-or-create-process python-shell-interpreter t))
          (dedicated-shell-buffer (process-buffer dedicated-process)))
     (unwind-protect
         (progn
           (set-process-query-on-exit-flag dedicated-process nil)
           ;; should be dedicated.
           (should (equal (process-name dedicated-process)
                          dedicated-process-name))
           (kill-buffer dedicated-shell-buffer)
           ;; Check there are no processes for current buffer.
           (should (not (python-shell-get-process))))
       (ignore-errors (kill-buffer dedicated-shell-buffer))))))

(ert-deftest python-shell-get-or-create-process-2 ()
  "Check shell global process creation."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (python-tests-with-temp-file
   ""
   (let* ((python-shell-interpreter
           (executable-find python-tests-shell-interpreter))
          (use-dialog-box)
          (process-name (python-shell-get-process-name nil))
          (process
           (python-shell-get-or-create-process python-shell-interpreter))
          (shell-buffer (process-buffer process)))
     (unwind-protect
         (progn
           (set-process-query-on-exit-flag process nil)
           ;; should be global.
           (should (equal (process-name process) process-name))
           (kill-buffer shell-buffer)
           ;; Check there are no processes for current buffer.
           (should (not (python-shell-get-process))))
       (ignore-errors (kill-buffer dedicated-shell-buffer))))))

(ert-deftest python-shell-get-or-create-process-3 ()
  "Check shell dedicated/global process preference."
  (skip-unless (executable-find python-tests-shell-interpreter))
  (python-tests-with-temp-file
   ""
   (let* ((python-shell-interpreter
           (executable-find python-tests-shell-interpreter))
          (use-dialog-box)
          (dedicated-process-name (python-shell-get-process-name t))
          (global-process)
          (dedicated-process))
     (unwind-protect
         (progn
           ;; Create global process
           (run-python python-shell-interpreter nil)
           (setq global-process (get-buffer-process "*Python*"))
           (should global-process)
           (set-process-query-on-exit-flag global-process nil)
           ;; Create dedicated process
           (run-python python-shell-interpreter t)
           (setq dedicated-process (get-process dedicated-process-name))
           (should dedicated-process)
           (set-process-query-on-exit-flag dedicated-process nil)
           ;; Prefer dedicated.
           (should (equal (python-shell-get-or-create-process)
                          dedicated-process))
           ;; Kill the dedicated so the global takes over.
           (kill-buffer (process-buffer dedicated-process))
           ;; Detect global.
           (should (equal (python-shell-get-or-create-process) global-process))
           ;; Kill the global.
           (kill-buffer (process-buffer global-process))
           ;; Check there are no processes for current buffer.
           (should (not (python-shell-get-process))))
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
              "Invalid regexp \\( in `python-shell-prompt-input-regexps'"))))

(ert-deftest python-shell-prompt-validate-regexps-2 ()
  "Check `python-shell-prompt-output-regexps' are validated."
  (let* ((python-shell-prompt-output-regexps '("\\("))
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              "Invalid regexp \\( in `python-shell-prompt-output-regexps'"))))

(ert-deftest python-shell-prompt-validate-regexps-3 ()
  "Check `python-shell-prompt-regexp' is validated."
  (let* ((python-shell-prompt-regexp "\\(")
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              "Invalid regexp \\( in `python-shell-prompt-regexp'"))))

(ert-deftest python-shell-prompt-validate-regexps-4 ()
  "Check `python-shell-prompt-block-regexp' is validated."
  (let* ((python-shell-prompt-block-regexp "\\(")
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              "Invalid regexp \\( in `python-shell-prompt-block-regexp'"))))

(ert-deftest python-shell-prompt-validate-regexps-5 ()
  "Check `python-shell-prompt-pdb-regexp' is validated."
  (let* ((python-shell-prompt-pdb-regexp "\\(")
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              "Invalid regexp \\( in `python-shell-prompt-pdb-regexp'"))))

(ert-deftest python-shell-prompt-validate-regexps-6 ()
  "Check `python-shell-prompt-output-regexp' is validated."
  (let* ((python-shell-prompt-output-regexp "\\(")
         (error-data (should-error (python-shell-prompt-validate-regexps)
                                   :type 'user-error)))
    (should
     (string= (cadr error-data)
              "Invalid regexp \\( in `python-shell-prompt-output-regexp'"))))

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
              "Invalid regexp \\( in `python-shell-prompt-output-regexp'"))))

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


;;; Shell completion


;;; PDB Track integration


;;; Symbol completion


;;; Fill paragraph


;;; Skeletons


;;; FFAP


;;; Code check


;;; Eldoc


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
               (cons "c (def)" (copy-marker 626)))))
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
                  (cons "Baz.Frob.c" (copy-marker 626)))
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
if width == 0 and height == 0 and \\\\
   color == 'red' and emphasis == 'strong' or \\\\
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
if width == 0 and height == 0 and \\\\
   color == 'red' and emphasis == 'strong' or \\\\
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
if width == 0 and height == 0 and \\\\
   color == 'red' and emphasis == 'strong' or \\\\
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
if width == 0 and height == 0 and \\\\
   color == 'red' and emphasis == 'strong' or \\\\
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
if width == 0 and height == 0 and \\\\
   color == 'red' and emphasis == 'strong' or \\\\
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
if width == 0 and height == 0 and \\\\
   color == 'red' and emphasis == 'strong' or \\\\
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
objects = Thing.objects.all() \\\\
                       .filter(
                           type='toy',
                           status='bought'
                       ) \\\\
                       .aggregate(
                           Sum('amount')
                       ) \\\\
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
objects = Thing.objects.all() \\\\
                       .filter(
                           type='toy',
                           status='bought'
                       ) \\\\
                       .aggregate(
                           Sum('amount')
                       ) \\\\
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
if width == 0 and height == 0 and \\\\
   color == 'red' and emphasis == 'strong' or \\\\
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
if width == 0 and height == 0 and \\\\
   color == 'red' and emphasis == 'strong' or \\\\
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

(ert-deftest python-info-assignment-continuation-line-p-1 ()
  (python-tests-with-temp-buffer
   "
data = foo(), bar() \\\\
       baz(), 4 \\\\
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
           (python-shell-virtualenv-path
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
  (require 'electric)
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
  (require 'electric)
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


(provide 'python-tests)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; python-tests.el ends here
