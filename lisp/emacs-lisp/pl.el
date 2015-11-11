;;; pl --- Combinator parsing library for Emacs, similar to Haskell's Parsec

;; Copyright (C) 2010-2015 Free Software Foundation, Inc.

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 19 Mar 2015
;; Keywords: languages, lisp, internal, parsing, indentation

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The compatibility version of this library for Emacsen pre-25 will
;; be maintained at https://github.com/jwiegley/emacs-pl

;; This is a parsing library in the spirit of Haskell's parsec.

;; There are a few parsers, whose job is to inspect whatever is at the current
;; buffer position, and return zero or more details regarding what was found:

;;     pl-ch                Match a single character
;;     pl-str               Match a string
;;     pl-re                Match a regular expression
;;     pl-num               Match an integer or floating-point number

;; Other possibilities include: inspecting text properties, overlays, etc.

;; If the parser succeeds, it returns the object matched (a string by default),
;; and advances point to the next position after the match.  Keywords may be
;; given to return other details:

;;     :beg                 Beginning of the match
;;     :end                 End of the match
;;     :group N             A particular regexp group
;;     :props               All properties within the matched region
;;     :nil                 Return `nil` (same as using `ignore`)

;; If a parser fails, it throws the exception `failed`.  This is caught by the
;; macro `pl-try`, which returns `nil` upon encountering the exception.  This
;; makes it possible to build certain combinators out of these few parts:

;;     pl-or                Return result from first successful parser
;;     pl-and               Return last result, if all parsers succeed
;;     pl-until             If the parse fails, advance cursor position by
;;                          one character and try again.  Keywords can
;;                          change the advance amount.

;; Note that even though a parse may fail, and thus return no value, any
;; side-effects that occur during the course of the parse will of course be
;; retained.  This can be used to good effect, by continuing an action for as
;; long as a parse succeeds:

;; (pl-parse
;;   (while t
;;      (delete-region (pl-str "<xml>" :beg)
;;                     (pl-until
;;                       (pl-str "</xml>" :end)))))

;; This will delete blocks demarcated by `<xml>` and `</xml>`, for as long as
;; such blocks continue to occur contiguously to one another.

(eval-when-compile (require 'cl-lib))

(defgroup pl nil
  "Combinator parsing library for Emacs, similar to Haskell's Parsec"
  :group 'development)

(defun pl-ch (ch &rest args)
  (if (char-equal (char-after) ch)
      (prog1
          (cond
           ((memq :nil args) nil)
           ((memq :beg args)
            (point))
           ((memq :end args)
            (1+ (point)))
           (t
            (char-to-string ch)))
        (forward-char 1))
    (throw 'failed nil)))

(defun pl-re (regexp &rest args)
  (if (looking-at regexp)
      (prog1
          (cond
           ((memq :nil args) nil)
           ((memq :beg args)
            (match-beginning 0))
           ((memq :end args)
            (match-end 0))
           ((memq :group args)
            (let ((group
                   (loop named outer for arg on args
                         when (eq (car arg) :group) do
                         (return-from outer (cadr arg)))))
              (if group
                  (match-string group)
                (error "Unexpected regexp :group %s" group))))
           (t
            (match-string 0)))
        (goto-char (match-end 0)))
    (throw 'failed nil)))

(defsubst pl-str (str &rest args)
  (pl-re (regexp-quote str)))

(defsubst pl-num (num &rest args)
  (pl-re (regexp-quote (number-to-string num))))

(defmacro pl-or (&rest parsers)
  (let ((outer-sym (make-symbol "outer"))
        (parser-sym (make-symbol "parser")))
    `(loop named ,outer-sym for ,parser-sym in ',parsers
           finally (throw 'failed nil) do
           (catch 'failed
             (return-from ,outer-sym (eval ,parser-sym))))))

(defmacro pl-try (&rest forms)
  `(catch 'failed ,@forms))

(defalias 'pl-and 'progn)
(defalias 'pl-parse 'pl-try
  "Evaluate some FORMS that define the grammar and act on it.

FORMS are simply S-expressions that typically get evaluated in
the current buffer.  For example:

  (pl-parse
    (delete-region (pl-str \"<xml>\" :beg)
                   (pl-until
                     (pl-str \"</xml>\" :end))))

For other constructs, such as returning the result of every
parser as a list, just combine parsers with regular Lisp
forms (`pl-parse' is just a synonym for `pl-try'):

  (pl-parse
    (list (pl-str \"Hello\") (pl-str \"World\")))
")

(defmacro pl-until (parser &optional &key skip)
  `(catch 'done
     (while (not (eobp))
       (catch 'failed
         (throw 'done ,parser))
       ,(if skip
            `(,skip 1)
          `(forward-char 1)))))

(defmacro pl-many (&rest parsers)
  (let ((final-sym (make-symbol "final"))
        (result-sym (make-symbol "result"))
        (parsers-sym (make-symbol "parsers")))
    `(let ((,parsers-sym ',parsers)
           ,result-sym
           ,final-sym)
       (while (and ,parsers-sym
                   (setq ,result-sym
                         (catch 'failed
                           (list (eval (car ,parsers-sym))))))
         (push (car ,result-sym) ,final-sym)
         (setq ,parsers-sym (cdr ,parsers-sym)))
       (nreverse ,final-sym))))

(provide 'pl)

;;; pl.el ends here
