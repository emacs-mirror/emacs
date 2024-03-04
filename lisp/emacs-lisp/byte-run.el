;;; byte-run.el --- byte-compiler support for inlining  -*- lexical-binding: t -*-

;; Copyright (C) 1992, 2001-2024 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;;	Hallvard Furuseth <hbf@ulrik.uio.no>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

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

;; interface to selectively inlining functions.
;; This only happens when source-code optimization is turned on.

;;; Code:

(defvar byte-run--ssp-seen nil
  "Which conses/vectors/records have been processed in strip-symbol-positions?
The value is a hash table, the keys being the elements and the values being t.

The purpose of this is to detect circular structures.")

(defalias 'byte-run--strip-list
  #'(lambda (arg)
      "Strip the positions from symbols with position in the list ARG.
This is done by destructively modifying ARG.  Return ARG."
      (let ((a arg))
        (while
            (and
             (null (gethash a byte-run--ssp-seen))
             (progn
               (puthash a t byte-run--ssp-seen)
               (cond
                ((symbol-with-pos-p (car a))
                 (setcar a (bare-symbol (car a))))
                ((consp (car a))
                 (byte-run--strip-list (car a)))
                ((or (vectorp (car a)) (recordp (car a)))
                 (byte-run--strip-vector/record (car a)))
                ((hash-table-p (car a))
                 (byte-run--strip-hash-table (car a))))
               (consp (cdr a))))
          (setq a (cdr a)))
        (cond
         ((symbol-with-pos-p (cdr a))
          (setcdr a (bare-symbol (cdr a))))
         ((or (vectorp (cdr a)) (recordp (cdr a)))
          (byte-run--strip-vector/record (cdr a))))
        arg)))

(defalias 'byte-run--strip-vector/record
  #'(lambda (arg)
      "Strip the positions from symbols with position in the vector/record ARG.
This is done by destructively modifying ARG.  Return ARG."
      (if (null (gethash arg byte-run--ssp-seen))
        (let ((len (length arg))
              (i 0)
              elt)
          (puthash arg t byte-run--ssp-seen)
          (while (< i len)
            (setq elt (aref arg i))
            (cond
             ((symbol-with-pos-p elt)
              (aset arg i (bare-symbol elt)))
             ((consp elt)
              (byte-run--strip-list elt))
             ((or (vectorp elt) (recordp elt))
              (byte-run--strip-vector/record elt))
             ((hash-table-p elt)
              (byte-run--strip-hash-table elt)))
            (setq i (1+ i)))))
      arg))

(defalias 'byte-run--strip-hash-table
  #'(lambda (arg)
      "Strip the positions from symbols with position in the hash table ARG.
This is done by destructively modifying ARG.  Return ARG."
      (maphash
       (lambda (key value)
         (when (symbol-with-pos-p key)
           (let ((symbols-with-pos-enabled t))
             (remhash key arg))
             (setq key (bare-symbol key)))
         (puthash key
                  (cond
                   ((symbol-with-pos-p value)
                    (bare-symbol value))
                   ((consp value)
                    (byte-run--strip-list value))
                   ((or (vectorp value) (recordp value))
                    (byte-run--strip-vector/record value))
                   ((hash-table-p value)
                    (byte-run--strip-hash-table value))
                   (t value))
                  arg))
       arg)))

(defalias 'byte-run-strip-symbol-positions
  #'(lambda (arg)
      "Strip all positions from symbols in ARG.
This modifies destructively then returns ARG.

ARG is any Lisp object, but is usually a list or a vector or a
record, containing symbols with position."
      (setq byte-run--ssp-seen (make-hash-table :test 'eq))
      (cond
       ((symbol-with-pos-p arg)
        (bare-symbol arg))
       ((consp arg)
        (byte-run--strip-list arg))
       ((or (vectorp arg) (recordp arg))
        (byte-run--strip-vector/record arg))
       ((hash-table-p arg)
        (byte-run--strip-hash-table arg))
       (t arg))))

(defalias 'byte-run--report-hash-table
  #'(lambda (name arg)
      "Report the positions from symbols with position in the hash table ARG.
This is done by destructively modifying ARG.  Return ARG."
      (maphash
       (lambda (key value)
         (when (symbol-with-pos-p key)
           (message "SWP key %S in %S, name %S" key arg name))
         (cond
          ((symbol-with-pos-p value)
           (message "SWP in %S, key %S, value %S, name %S"
                    arg key value name))
          ((consp value)
           (byte-run--report-list name value))
          ((or (vectorp value) (recordp value))
           (byte-run--report-vector/record name value))
          ((hash-table-p value)
           (byte-run--report-hash-table name value))
          (t value)))
       arg)
       arg))

(defalias 'byte-run--report-vector/record
  #'(lambda (name arg)
      "Report the positions from symbols with position in the vector/record ARG.
This is done by destructively modifying ARG.  Return ARG."
      (if (null (gethash arg byte-run--ssp-seen))
        (let ((len (length arg))
              (i 0)
              elt)
          (puthash arg t byte-run--ssp-seen)
          (while (< i len)
            (setq elt (aref arg i))
            (cond
             ((symbol-with-pos-p elt)
              (message "SWP in %S, elt %S,  %S, name %S"
                       arg i elt name))
             ((consp elt)
              (byte-run--report-list name elt))
             ((or (vectorp elt) (recordp elt))
              (byte-run--report-vector/record name elt))
             ((hash-table-p elt)
              (byte-run--report-hash-table name elt)))
            (setq i (1+ i)))))
      arg))

(defalias 'byte-run--report-list
  #'(lambda (name arg)
      "Report positions in symbols with position in the list ARG.
This is done by destructively modifying ARG.  Return ARG."
      (let ((a arg))
        (while
            (and
             (null (gethash a byte-run--ssp-seen))
             (progn
               (puthash a t byte-run--ssp-seen)
               (cond
                ((symbol-with-pos-p (car a))
                 (message "SWP in %S,  %S" name (car a)))
                ((consp (car a))
                 (byte-run--report-list name (car a)))
                ((or (vectorp (car a)) (recordp (car a)))
                 (byte-run--report-vector/record name (car a)))
                ((hash-table-p (car a))
                 (byte-run--report-hash-table name (car a))))
               (consp (cdr a))))
          (setq a (cdr a)))
        (cond
         ((symbol-with-pos-p (cdr a))
          (message "SWP in %S,  %S" name (cdr a)))
         ;; ((or (vectorp (cdr a)) (recordp (cdr a)))
         ;;  (byte-run--strip-vector/record (cdr a)))
         )
        arg)))

(defalias 'byte-run-report-symbol-positions
  #'(lambda (name arg)
      "Report NAME for any symbols with position in ARG.

ARG is any Lisp object, but is usually a list or a vector or a
record, containing symbols with position."
      (setq byte-run--ssp-seen (make-hash-table :test 'eq))
      (cond
       ((symbol-with-pos-p arg)
        (message "SWP in %S,  %S" name arg))
       ((consp arg)
        (byte-run--report-list name arg))
       ((or (vectorp arg) (recordp arg))
        (byte-run--report-vector/record name arg))
       ((hash-table-p arg)
        (byte-run--report-hash-table name arg))
       (t arg))))

(defalias 'byte-run--posify-hash-table
  #'(lambda (form)
      "Posify any lambda forms still unposified in the hash table FORM.
The original FORM is not changed.  Return a changed copy of FORM or FORM."
      (if (null (gethash form byte-run--ssp-seen))
          (let ((new (copy-hash-table form))
                changed)
            (progn
              (puthash form t byte-run--ssp-seen)
              (maphash
               (lambda (key value)
                 (if (null (eq value
                               (setq value
                                     (cond
                                      ((consp value)
                                       (byte-run--posify-list value))
                                      ((or (vectorp value) (recordp value))
                                       (byte-run--posify-vector/record value))
                                      ((hash-table-p value)
                                       (byte-run--posify-hash-table value))
                                      (t value)))))
                     (setq changed t))
                 (puthash key value new))
               form)
              (if changed new form)))
        form)))

(defalias 'byte-run--posify-vector/record
  #'(lambda (form)
      "Posify any lambda forms still unposified in the vector/record FORM.
The original FORM is not changed.  Return a changed copy of FORM, or FORM."
      (if (null (gethash form byte-run--ssp-seen))
        (let* ((len (length form))
               (new (if (vectorp form)
                        (make-vector len nil)
                      (make-record (aref form 0) (1- len) nil)))
               (i 0)
               changed elt)
          (puthash form t byte-run--ssp-seen)
          (while (< i len)
            (setq elt (aref form i))
            (if (null (eq elt
                          (setq elt
                                (cond
                                 ((consp elt)
                                  (byte-run--posify-list elt))
                                 ((or (vectorp elt) (recordp elt))
                                  (byte-run--posify-vector/record elt))
                                 ((hash-table-p elt)
                                  (byte-run--posify-hash-table elt))
                                 (t elt)))))
                (setq changed t))
            (aset new i elt)
            (setq i (1+ i)))
          (if changed new form))
        form)))

(defalias 'byte-run--posify-list
  #'(lambda (form)
      "Posify any lambda forms still unposified in the the list FORM.
This original FORM is not changed.  Return a changed copy of FORM or FORM."
      (let ((a form) changed elt new)
        (while (and (null (gethash a byte-run--ssp-seen))
                    (consp a)
                    (null (and (symbol-with-pos-p (car a))
                               (eq (bare-symbol (car a)) 'lambda))))
          (progn
            (puthash a t byte-run--ssp-seen)
            (setq elt (car a))
            (if (null
                 (eq elt
                     (setq elt
                           (cond
                            ((consp elt)
                             (byte-run--posify-list elt))
                            ((or (vectorp elt) (recordp elt))
                             (byte-run--posify-vector/record elt))
                            ((hash-table-p elt)
                             (byte-run--posify-hash-table elt))
                            (t elt)))))
                (setq changed t))
            (setq new (cons elt new))
            (setq a (cdr a))))

        (cond
         ((gethash a byte-run--ssp-seen)
          (if changed (nconc (nreverse new) a) form))
         ((null a)
          (if changed (nreverse new) form))
         ((or (vectorp a) (recordp a))
          (if (or (null (eq a (setq a (byte-run--posify-vector/record a))))
                  changed)
              (cons (nreverse new) a)
            form))
         ((hash-table-p a)
          (if (or (null (eq a (setq a (byte-run--posify-hash-table a))))
                  changed)
              (cons (nreverse new) a)
            form))
         ((and (symbol-with-pos-p (car-safe a))
               (eq (bare-symbol (car a)) 'lambda))
          (nconc (nreverse new)
                 (let ((stripped (byte-run-posify-lambda-form
                                  a (symbol-with-pos-pos (car a)))))
                   (setcar stripped 'lambda) ; Strip the position.
                   (byte-run--posify-list stripped))))
         (t (if changed (cons (nreverse new) a) form))))))

(defalias 'byte-run-posify-all-lambdas
  #'(lambda (form)
      "Posify any lambda forms still unposified in FORM.

FORM is any Lisp object, but is usually a list or a vector or a
record, containing symbols with position.  Return FORM, possibly
destructively modified."
      (setq byte-run--ssp-seen (make-hash-table :test 'eq))
      (cond
       ((consp form)
        (byte-run--posify-list form))
       ((or (vectorp form) (recordp form))
        (byte-run--posify-vector/record form))
       ((hash-table-p form)
        (byte-run--posify-hash-table form))
       (t form))))


(defalias 'byte-run--strip-lambda-doc-list
  #'(lambda (form)
      "Strip any doc string from all lambdas in the cons FORM, and return it."
      (let ((a form))
        (while
            (and
             (null (gethash a byte-run--ssp-seen))
             (progn
               (puthash a t byte-run--ssp-seen)
               (cond
                ((and (eq (car a) 'lambda)
                      (listp (car-safe (cdr a)))
                      (stringp (car-safe (cdr-safe (cdr a)))))
                 (setcdr (cdr a) (cdr (cdr (cdr a)))))
                ((consp (car a))
                 (byte-run--strip-lambda-doc-list (car a)))
                ((or (vectorp (car a)) (recordp (car a)))
                 (byte-run--strip-lambda-doc-vector/record (car a)))
                ((hash-table-p (car a))
                 (byte-run--strip-lambda-doc-hash-table (car a))))
               (consp (cdr a))))
          (setq a (cdr a)))
        (cond
         ((or (vectorp (cdr a)) (recordp (cdr a)))
          (byte-run--strip-lambda-doc-vector/record (cdr a)))
         ((hash-table-p (cdr a))
          (byte-run--strip-lambda-doc-hash-table (cdr a))))
        form)))

(defalias 'byte-run--strip-lambda-doc-vector/record
  #'(lambda (form)
      "Strip any doc string from all lambdas in the vector/record FORM.
Return the possibly changed FORM."
      (if (null (gethash form byte-run--ssp-seen))
          (let ((len (length form))
                (i 0)
                elt)
            (puthash form t byte-run--ssp-seen)
            (while (< i len)
              (setq elt (aref form i))
              (cond
               ((consp elt)
                (byte-run--strip-lambda-doc-list elt))
               ((or (vectorp elt) (recordp elt))
                (byte-run--strip-lambda-doc-vector/record elt))
               ((hash-table-p elt)
                (byte-run--strip-lambda-doc-hash-table elt)))
              (setq i (1+ i)))))
      form))

(defalias 'byte-run--strip-lambda-doc-hash-table
  #'(lambda (form)
      "Strip any doc string from all lambdas in the hash table FORM.
Return the possibly changed FORM."
      (if (null (gethash form byte-run--ssp-seen))
          (puthash form t byte-run--ssp-seen)
          (maphash
           (lambda (_key value)
             ;; Disregard the possibility of lambdas in `key's.
             (cond
              ((consp value)
               (byte-run--strip-lambda-doc-list value))
              ((or (vectorp value) (recordp value))
               (byte-run--strip-lambda-doc-vector/record value))
              ((hash-table-p value)
               (byte-run--strip-lambda-doc-hash-table value))))
           form))
      form))

(defalias 'byte-run-strip-lambda-doc
  #'(lambda (form)
      "Strip any doc string from all lambdas forms contained in FORM.
FORM can be any Lisp object.  A lambda form is something like
\(lambda (args) \"Optional doc string\" ...).  The change is done
destructively to the original FORM.  Return the possibly altered
FORM."
      (setq byte-run--ssp-seen (make-hash-table :test 'eq))
      (let ((symbols-with-pos-enabled t))
        (cond
         ((symbolp form) form)
         ((consp form)
          (byte-run--strip-lambda-doc-list form))
         ((or (vectorp form) (recordp form))
          (byte-run--strip-lambda-doc-vector/record form))
         ((hash-table-p form)
          (byte-run--strip-lambda-doc-hash-table form))
         (t form)))))

(defalias 'byte-run-valid-doc-string
  #'(lambda (str)
      "Return non-nil if STR is a valid doc string.
Otherwise return nil.
If STR is a string, or of the form (:documentation \"...\"), the return
value is t.  If STR is a doc string (:documentation ...) generated at
run time, return the symbol `var'."
      (if (stringp str)
          t
        (if (eq (car-safe str) ':documentation)
            (if (stringp (car-safe (cdr str)))
                t
              'var)
          nil))))

(defalias 'byte-run-posify-doc-string
  #'(lambda (doc-string &optional lambda-pos)
      "Prefix a doc string with defining position information.
DOC-STRING is the existing doc string, or if nil, the new doc
string is created from scratch.  If DOC-STRING is a
cons, (:documentation ....), the new structure will be `concat'ed
onto it.  LAMBDA-POS when non-nil is the position of the symbol
`lambda' for which the new doc string is being created.  It
should be a fixnum.  Return the new (or unaltered) doc string.
If DOC-STRING already has position information, return the string
unchanged."
      (if (cond
           ((stringp doc-string)
            (string-match "\\`;POS\036\001\001\001" doc-string))
           ((and (consp doc-string)
                 (eq (car-safe doc-string) ':documentation)
                 (stringp (car-safe (cdr doc-string))))
            (string-match "\\`;POS\036\001\001\001" (car (cdr doc-string)))))
          doc-string
        (let ((pos-string
             (concat
              ";POS"
              ;; (let ((version ; See comments in `byte-compile-insert-header'.
              ;; (if (zerop emacs-minor-version)
              ;;     (1- emacs-major-version)
              ;; emacs-major-version));)
              "\036"       ; Hard coded version 30, for now.  FIXME!!!
              ;; (cl-assert (and (> version 13) (< version 128)))
              ;; (string version))
              "\001\001\001 ["
              (if defining-symbol
                  (symbol-name (bare-symbol defining-symbol))
                "nil")
              " "
              (cond
               ((bufferp read-stream)
                (let ((name (format "%s" (buffer-name read-stream))))
                  (string-replace " " "\\ " name)))
               ;; What about reading from Fload, when we don't have a
               ;; buffer as such?  FIXME!!!  STOUGH, 2023-12-15.
               ((stringp read-stream)   ; A file name
                read-stream)
               (t                       ; ?minibuffer
                "nil"))                 ;; )
              " "
              (if (symbol-with-pos-p defining-symbol)
                  (format "%d" (symbol-with-pos-pos defining-symbol))
                "nil")
              " "
              (if (numberp lambda-pos)
                  (format "%d" lambda-pos)
                "nil")
              "]\n")))
        (cond
         ((null doc-string) pos-string)
         ((stringp doc-string) (concat pos-string doc-string))
         ((and (consp doc-string) (eq (car doc-string) ':documentation))
          (list (car doc-string)
                (cond ((stringp (car (cdr doc-string)))
                       (concat pos-string (car (cdr doc-string))))
                      ((consp (car (cdr doc-string)))
                       (list 'concat pos-string
                             (car (cdr doc-string))))
                      (t (list 'concat pos-string
                               (car (cdr doc-string)))))))
         (t ; Oclosure type info in the doc string position.  Deal with this
            ; sometime (2024-02-26).
          doc-string
          ))))))

(defalias 'byte-run-posify-lambda-form
  #'(lambda (form position)
      "Put position structure on the lambda form FORM.
POSITION is one position that will be used, the other coming from
`defining-symbol'.

The modification of FORM will be done by creating a new list
form."
      (let* ((bare-ds (bare-symbol defining-symbol))
             (cand-doc-string (nth 2 form))
             (doc-string
              (and (byte-run-valid-doc-string cand-doc-string)
                   cand-doc-string))
             (already-posified
              (and doc-string
                   (cond
                    ((stringp doc-string)
                     (string-match "^;POS\036\001\001\001" doc-string))
                    ((stringp (car-safe (cdr-safe doc-string)))
                     (string-match "^;POS\036\001\001\001"
                                   (car (cdr doc-string))))
;;;; STOUGH TO AMEND WHEN APPROPRIATE, 2023-12-17
                    (t t) ; For (:documentation 'symbol), in oclosures.
;;;; END OF STOUGH
                         )))
             (empty-body-allowed
              (and bare-ds (get bare-ds 'empty-body-allowed)))
             (insert (or (null doc-string)
                         (and (null empty-body-allowed)
                              (null (nthcdr 3 form))))))

        (if (and (null already-posified)
                 (>= (length form) 2))
            (let ((new-doc-string (byte-run-posify-doc-string
                                   doc-string
                                   position)))
              (nconc (take 2 form)
                     (list new-doc-string)
                     (nthcdr (if insert 2 3) form)))
          form))))

(defalias 'function-put
  ;; We don't want people to just use `put' because we can't conveniently
  ;; hook into `put' to remap old properties to new ones.  But for now, there's
  ;; no such remapping, so we just call `put'.
  #'(lambda (function prop value)
      "Set FUNCTION's property PROP to VALUE.
The namespace for PROP is shared with symbols.
So far, FUNCTION can only be a symbol, not a lambda expression."
      (put (bare-symbol function) prop value)))
(function-put 'defmacro 'doc-string-elt 3)
(function-put 'defmacro 'lisp-indent-function 2)

;; We define macro-declaration-alist here because it is needed to
;; handle declarations in macro definitions and this is the first file
;; loaded by loadup.el that uses declarations in macros.  We specify
;; the values as named aliases so that `describe-variable' prints
;; something useful; cf. Bug#40491.  We can only use backquotes inside
;; the lambdas and not for those properties that are used by functions
;; loaded before backquote.el.

(defalias 'byte-run--set-advertised-calling-convention
  #'(lambda (f _args arglist when)
      (list 'set-advertised-calling-convention
            (list 'quote f) (list 'quote arglist) (list 'quote when))))

(defalias 'byte-run--set-obsolete
  #'(lambda (f _args new-name when)
      (list 'make-obsolete
            (list 'quote f) (list 'quote new-name) when)))

(defalias 'byte-run--set-interactive-only
  #'(lambda (f _args instead)
      (list 'function-put (list 'quote f)
            ''interactive-only (list 'quote instead))))

(defalias 'byte-run--set-pure
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''pure (list 'quote val))))

(defalias 'byte-run--set-side-effect-free
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''side-effect-free (list 'quote val))))

(defalias 'byte-run--set-important-return-value
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''important-return-value (list 'quote val))))

(put 'compiler-macro 'edebug-declaration-spec
     '(&or symbolp ("lambda" &define lambda-list lambda-doc def-body)))

(defalias 'byte-run--set-compiler-macro
  #'(lambda (f args compiler-function)
      (if (null (eq (car-safe compiler-function) 'lambda))
          `(eval-and-compile
             (function-put ',f 'compiler-macro #',compiler-function))
        (let ((cfname (intern (concat (symbol-name f) "--anon-cmacro")))
              ;; Avoid cadr/cddr so we can use `compiler-macro' before
              ;; defining cadr/cddr.
              (data (cdr compiler-function)))
          `(progn
             (eval-and-compile
               (function-put ',f 'compiler-macro #',cfname))
             ;; Don't autoload the compiler-macro itself, since the
             ;; macroexpander will find this file via `f's autoload,
             ;; if needed.
             :autoload-end
             (eval-and-compile
               (defun ,cfname (,@(car data) ,@args)
                 (ignore ,@(delq '&rest (delq '&optional (copy-sequence args))))
                 ,@(cdr data))))))))

(defalias 'byte-run--set-doc-string
  #'(lambda (f _args pos)
      (list 'function-put (list 'quote f)
            ''doc-string-elt (if (numberp pos)
                                 pos
                               (list 'quote pos)))))

(defalias 'byte-run--set-indent
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''lisp-indent-function (if (numberp val)
                                       val
                                     (list 'quote val)))))

(defalias 'byte-run--set-speed
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''speed (list 'quote val))))

(defalias 'byte-run--set-completion
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''completion-predicate (list 'function val))))

(defalias 'byte-run--set-modes
  #'(lambda (f _args &rest val)
      (list 'function-put (list 'quote f)
            ''command-modes (list 'quote val))))

(defalias 'byte-run--set-interactive-args
  #'(lambda (f args &rest val)
      (setq args (remove '&optional (remove '&rest args)))
      (list 'function-put (list 'quote f)
            ''interactive-args
            (list
             'quote
             (mapcar
              (lambda (elem)
                (cons
                 (seq-position args (car elem))
                 (cadr elem)))
              val)))))

(defalias 'byte-run--extract-sym-from-form
  #'(lambda (form args)
      "Return the \"first\" arg in ARGS which occurs in list FORM.
ARGS has the shape of an arglist from a lambda form, FORM is a list
which is capable of being evaluated.  If an arg isn't found, return
nil."
      (cond
       ((symbolp form) (car (memq form args)))
       ((consp form)
        (let ((tail (cdr form)) sym
              )
          (while (and tail
                      (null
                       (setq sym
                             (byte-run--extract-sym-from-form
                              (car tail) args))))
            (setq tail (cdr tail)))
          sym)))))

(defalias 'byte-run--doc-n
  #'(lambda (doc-spec args)
      "Return the \"index\" of DOC-SPEC in a member of ARGS.
This index is the number of cdr's in DOC-SPEC which is of the form
(car (cdr (cdr ..... arg))), where arg is a member of the list ARGS.
(nth index arg) would give the same result as DOC-SPEC.  If
DOC-SPEC is not of the above form, return nil."
      (let ((n 0))
        (and (eq (car-safe doc-spec) 'car)
             (progn
               (while
                   (progn
                     (setq doc-spec (car-safe (cdr doc-spec)))
                     (eq (car-safe doc-spec) 'cdr))
                 (setq n (1+ n)))
               (memq doc-spec args))
             n))))

(defalias 'byte-run--posify-defining-form
  #'(lambda (f args &rest bit-specs)
      "Create code to posify an invocation of F with arglist ARGS.

This handler makes the invoking macro a defining macro.  The
`defining-symbol' declare clause which invokes this function specifies
which symbol the macro will be defining, where to find its doc string,
and whether to insert a new doc string if the macro invocation
would result in an empty body.  It is used to make macros such as `defun'
and `cl-defstruct' defining macros.

It returns a list of two Lisp forms: (i) to set the `byte-run-defined-form'
property on the macro being defined; (ii) to cause the defining macro to
posify the forms it defines.

BIT-SPECS, the cdr of the `defining-symbol' declare clause consists of three
elements: (i) A Lisp form, usually a symbol, which specifies how to get what
the macro will use as the new name being defined; (ii) The location of the
doc string in ARGS.  This is either a symbol (an element of ARGS) or of the form
(car (cdr (cdr ..... arg))), with zero or more `cdr's.  These may not be
abbreviated to (cadr arg), etc.  This element is optional when there is a
&rest (or &body) form in ARGS.  In this case the doc string is assumed to be
the first &rest element; (iii) An optional `t' element.  If present, it means
that if the form being defined by the macro would have an empty body, NOT to
assume that a doc string is also the value of the form.  See `cl-defstruct' for
an example of its use."
      (let* ((def-spec (car bit-specs))
             (rest-stuff
              (or (memq '&rest args)
                  (memq '&body args))) ; YUCK!
             (body-spec (and (eq (length rest-stuff) 2)
                             (car (cdr rest-stuff))))
             (doc-spec
              (cond ((and body-spec
                          (memq (car (cdr bit-specs)) '(nil t)))
                     (list 'car body-spec)) ; Default
                    ((symbolp (car (cdr bit-specs)))
                     (car (cdr bit-specs)))
                    ((byte-run--doc-n (car (cdr bit-specs)) args)
                     (car (cdr bit-specs)))
                    (t (error "Invalid arguments to defining-form declare clause in %s" f))))
             ;; We want to insert a new doc string if `doc-spec' is an
             ;; &optional parameter just before the &rest arg, and its
             ;; run time value isn't a string (e.g. a keyword in
             ;; `define-derived-mode' signifying a missing doc string).
             (can-insert-doc-before-rest
              (and rest-stuff
                   (symbolp doc-spec)
                   (memq doc-spec (memq '&optional args))
                   (eq (cdr (memq doc-spec args)) rest-stuff)))
             (empty-body-flag (and (memq t bit-specs) ; Empty body allowed?
                                   (eq (length (memq t bit-specs)) 1)))
             (def-arg-sym (byte-run--extract-sym-from-form def-spec args))
             (doc-arg-sym
              (byte-run--extract-sym-from-form doc-spec args))
             (doc-n (and (consp doc-spec)
                         (byte-run--doc-n doc-spec args)))
             (after-doc-spec (and doc-n
                                  (list 'nthcdr (1+ doc-n) doc-arg-sym)))
             (def-index
              (let ((i 1) (rest-args args))
                (while (and rest-args (null (eq (car rest-args) def-arg-sym)))
                  (progn (setq rest-args (cdr rest-args)) (setq i (1+ i))))
                (and rest-args i))))

        (if (null (and def-index doc-arg-sym))
            (error "Invalid arguments to defining-form declare clause in %s"
                   f))

        (cons
         (list 'function-put (list 'quote f)
               ''byte-run-defined-form
               def-index)
         (list
          'progn
          (list 'or 'defining-symbol
                (list 'setq 'defining-symbol def-spec))

          (list 'let*
                (list
                 (list 'old-ds
                       (list 'and (list 'byte-run-valid-doc-string doc-spec)
                             doc-spec))
                 (list 'new-ds (list 'byte-run-posify-doc-string 'old-ds)))
                ;; Strip the symbol position from the name being defined.
                (list 'if '(null byte-compile-in-progress)
                      (list 'setq def-arg-sym
                            (list 'byte-run-strip-symbol-positions
                                  def-arg-sym)))
                ;; Strip the symbol position from the name in the
                ;; original form.
                (list 'if (list 'and 'cur-evalled-macro-form
                                (list 'null 'byte-compile-in-progress))
                      (list
                       'let
                       (list
                        (list 'stripped-arg
                              (list 'byte-run-strip-symbol-positions
                                    (list 'nth def-index
                                          'cur-evalled-macro-form))))
                       (list 'setcar (list 'nthcdr def-index
                                           'cur-evalled-macro-form)
                             'stripped-arg)))
                (if empty-body-flag
                    (list 'put def-spec ''empty-body-allowed t)
                  (list 'progn))
                ;; Replace the old doc string with the new, or
                ;; insert the new.
                (cond
                 (can-insert-doc-before-rest
                  (list 'if (list 'byte-run-valid-doc-string 'old-ds)
                        (list 'setq doc-spec 'new-ds)
                        ;; If `doc-spec' isn't a string, it's part of the body.
                        (list 'setq body-spec
                              (list 'cons doc-spec body-spec))
                        (list 'setq doc-spec 'new-ds)))
                 ((symbolp doc-spec)
                  (list 'setq doc-spec 'new-ds))
                 (t
                   (list
                    'setq doc-arg-sym
                    (list
                     'nconc
                     (list 'take doc-n doc-arg-sym)
                     (list
                      'cond
                      ;; doc-string present and a non-nil (cdr body):
                      (list (list 'and (list 'byte-run-valid-doc-string
                                             doc-spec)
                                  after-doc-spec)
                            (list 'list 'new-ds))
                      ;; Single string, both doc string and return value:
                      (list (list 'byte-run-valid-doc-string doc-spec)
                            (if empty-body-flag
                                (list 'list 'new-ds)
                              (list 'list 'new-ds 'old-ds)))
                      ;; Neither doc string nor return value:
                      (list (list 'null (list 'nthcdr doc-n doc-arg-sym))
                            (if empty-body-flag
                                (list 'list 'new-ds)
                              (list 'list 'new-ds ''nil)))
                      ;; No doc string, but a non-nil body, not a string.
                      (list t
                            (list 'list 'new-ds doc-spec)))
                     after-doc-spec))))))))))

(put 'byte-run--posify-defining-form 'byte-run-pre-form t)

;; Add any new entries to info node `(elisp)Declare Form'.
(defvar defun-declarations-alist
  (list
   (list 'advertised-calling-convention
         #'byte-run--set-advertised-calling-convention)
   (list 'obsolete #'byte-run--set-obsolete)
   (list 'interactive-only #'byte-run--set-interactive-only)
   ;; FIXME: Merge `pure' and `side-effect-free'.
   (list 'pure #'byte-run--set-pure
         "If non-nil, the compiler can replace calls with their return value.
This may shift errors from run-time to compile-time.")
   (list 'side-effect-free #'byte-run--set-side-effect-free
         "If non-nil, calls can be ignored if their value is unused.
If `error-free', drop calls even if `byte-compile-delete-errors' is nil.")
   (list 'important-return-value #'byte-run--set-important-return-value
         "If non-nil, warn about calls not using the returned value.")
   (list 'compiler-macro #'byte-run--set-compiler-macro)
   (list 'doc-string #'byte-run--set-doc-string)
   (list 'indent #'byte-run--set-indent)
   (list 'speed #'byte-run--set-speed)
   (list 'completion #'byte-run--set-completion)
   (list 'modes #'byte-run--set-modes)
   (list 'interactive-args #'byte-run--set-interactive-args))
  "List associating function properties to their macro expansion.
Each element of the list takes the form (PROP FUN) where FUN is
a function.  For each (PROP . VALUES) in a function's declaration,
the FUN corresponding to PROP is called with the function name,
the function's arglist, and the VALUES and should return the code to use
to set this property.

This is used by `declare'.")

(defalias 'byte-run--set-debug
  #'(lambda (name _args spec)
      (list 'progn :autoload-end
	    (list 'put (list 'quote name)
		  ''edebug-form-spec (list 'quote spec)))))

(defalias 'byte-run--set-no-font-lock-keyword
  #'(lambda (name _args val)
      (list 'function-put (list 'quote name)
	    ''no-font-lock-keyword (list 'quote val))))

(defalias 'byte-run--parse-body
  #'(lambda (body allow-interactive)
      "Decompose BODY into (DOCSTRING DECLARE INTERACTIVE BODY-REST WARNINGS)."
      (let* ((top body)
             (docstring nil)
             (declare-form nil)
             (interactive-form nil)
             (warnings nil)
             (non-bare-doc nil)
             (warn #'(lambda (msg form)
                       (push (macroexp-warn-and-return
                              (format-message msg) nil nil t form)
                             warnings))))
        (while
            (and body
                 (let* ((form (car body))
                        (head (car-safe form)))
                   (cond
                    ((and (null docstring)
                          (stringp form) (null (cdr body)))
                     ;; The doc string is also the defun's return value.
                     (setq docstring form)
                     nil)     ; Don't remove the doc string from BODY.
                    ((or (and (stringp form) (cdr body))
                         (eq head :documentation))
                     (cond
                      (non-bare-doc
                       (funcall warn "More than one doc string" top))
                      (declare-form
                       (funcall warn "Doc string after `declare'" declare-form))
                      (interactive-form
                       (funcall warn "Doc string after `interactive'"
                                interactive-form))
                      ((string-match "\\`;POS\36\1\1\1 \\[[^]]+]\n\\'"
                                     (cond
                                      ((stringp form)
                                       form)
                                      ((stringp (car (cdr form)))
                                       (car (cdr form)))
                                      (t "")))
                       (setq docstring form))
                      (t (setq docstring form)
                         (setq non-bare-doc t)))
                     t)
                    ((eq head 'declare)
                     (cond
                      (declare-form
                       (funcall warn "More than one `declare' form" form))
                      (interactive-form
                       (funcall warn "`declare' after `interactive'" form))
                      (t (setq declare-form form)))
                     t)
                    ((eq head 'interactive)
                     (cond
                      ((null allow-interactive)
                       (funcall warn "No `interactive' form allowed here" form))
                      (interactive-form
                       (funcall warn "More than one `interactive' form" form))
                      (t (setq interactive-form form)))
                     t))))
          (setq body (cdr body)))
        (list docstring declare-form interactive-form body warnings))))

(defalias 'byte-run--parse-declarations
  #'(lambda (name arglist clauses construct declarations-alist)
      (let* ((cl-decls nil)
             (actions
              (mapcar
               #'(lambda (x)
                   (let ((f (cdr (assq (car x) declarations-alist))))
                     (cond
                      ((and f (symbolp (car f))
                            (get (car f) 'byte-run-pre-form))
                       (let ((res (apply (car f) name arglist (cdr x))))
                         (setq cl-decls (cons (cdr res) cl-decls))
                         (car res)))
                      (f (apply (car f) name arglist (cdr x)))
                      ;; Yuck!!
                      ((and (featurep 'cl)
                            (memq (car x)  ;C.f. cl--do-proclaim.
                                  '(special inline notinline optimize warn)))
                       (push (list 'declare x) cl-decls)
                       nil)
                      (t
                       (macroexp-warn-and-return
                        (format-message "Unknown %s property `%S'"
                                        construct (car x))
                        nil nil nil (car x))))))
               clauses)))
        (cons actions cl-decls))))

(defvar macro-declarations-alist
  (cons
   (list 'defining-symbol #'byte-run--posify-defining-form)
   (cons
    (list 'debug #'byte-run--set-debug)
    (cons
     (list 'no-font-lock-keyword #'byte-run--set-no-font-lock-keyword)
     defun-declarations-alist)))
  "List associating properties of macros to their macro expansion.
Each element of the list takes the form (PROP FUN) where FUN is a function.
For each (PROP . VALUES) in a macro's declaration, the FUN corresponding
to PROP is called with the macro name, the macro's arglist, and the VALUES
and should return the code to use to set this property.

This is used by `declare'.")

(defalias 'defmacro
  (cons
   'macro
   #'(lambda (name arglist &rest body)
       "Define NAME as a macro.
When the macro is called, as in (NAME ARGS...),
the function (lambda ARGLIST BODY...) is applied to
the list ARGS... as it appears in the expression,
and the result should be a form to be evaluated instead of the original.
DECL is a declaration, optional, of the form (declare DECLS...) where
DECLS is a list of elements of the form (PROP . VALUES).  These are
interpreted according to `macro-declarations-alist'.

\(fn NAME ARGLIST [DOCSTRING] [DECL] BODY...)"
       (if (null defining-symbol) ; For, e.g., components of cl-defstruct's;
           (setq defining-symbol name)) ; they must get the original symbol.
       (let*
           ((old-ds
             (or (and (stringp (car body)) (car body))
                 (and (eq (car-safe (car body)) ':documentation)
                      (car body))))
            (new-ds (byte-run-posify-doc-string old-ds)))
         ;; Replace the old doc string with the new, or insert the new.
         (setq body
               (cond
                ((and (stringp (car body)) (cdr body))
                 (cons new-ds (cdr body)))
                ((stringp (car body)) (list new-ds old-ds))
                ((null (car body)) (list new-ds 'nil))
                (t (cons new-ds body)))))
       (let* ((parse (byte-run--parse-body body nil))
              (docstring
                 (nth 0 parse))
              (declare-form (nth 1 parse))
              (body (or (nth 3 parse)
                        '(nil)))
              (warnings (nth 4 parse))
              (declarations
               (and declare-form (byte-run--parse-declarations
                                  name arglist (cdr declare-form) 'macro
                                  macro-declarations-alist))))
         (setq body (nconc warnings body))
         (if (and (null byte-compile-in-progress)
                  (symbol-with-pos-p name))
             (setq name (bare-symbol name)))
         (setq body (nconc (cdr declarations) body))
         (if docstring
             (setq body (cons docstring body)))
         (if (null body)
             (setq body '(nil)))
         (let* ((fun (list 'function (cons 'lambda (cons arglist body))))
	        (def (list 'defalias
		           (list 'quote name)
		           (list 'cons ''macro fun))))
           (if declarations
	       (cons 'prog1 (cons def (car declarations)))
	     def))))))
(function-put 'defmacro 'byte-run-defined-form 1)

;; Now that we defined defmacro we can use it!
(defmacro defun (name arglist &rest body)
  "Define NAME as a function.
The definition is (lambda ARGLIST [DOCSTRING] [INTERACTIVE] BODY...).
DECL is a declaration, optional, of the form (declare DECLS...) where
DECLS is a list of elements of the form (PROP . VALUES).  These are
interpreted according to `defun-declarations-alist'.
INTERACTIVE is an optional `interactive' specification.

\(fn NAME ARGLIST [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (doc-string 3) (indent 2)
           (defining-symbol name))
  (or name (error "Cannot define '%s' as a function" name))
  (if (null
       (and (listp arglist)
            (null (delq t (mapcar #'symbolp arglist)))))
      (error "Malformed arglist: %s" arglist))
  (let* ((parse (byte-run--parse-body body t))
         (docstring
          (nth 0 parse))
         (declare-form (nth 1 parse))
         (interactive-form (nth 2 parse))
         (body
          (or (nth 3 parse)
              '(nil)))
         (warnings (nth 4 parse))
         (declarations
          (and declare-form (byte-run--parse-declarations
                             name arglist (cdr declare-form) 'defun
                             defun-declarations-alist))))
    (setq body (nconc warnings body))
    (setq body (nconc (cdr declarations) body))
    (if interactive-form
        (setq body (cons interactive-form body)))
    (if docstring
        (setq body (cons docstring body)))
    (if (null body)
        (setq body '(nil)))
    (let ((def (list 'defalias
                     (list 'quote name)
                     (list 'function
                           (cons 'lambda
                                 (cons arglist body))))))
      (if declarations
          (cons 'prog1 (cons def (car declarations)))
        def))))


(defun byte-run--fun-doc-pos/macro (fun)
  "FUN should be a function form.  Is it a lambda, a closure, a macro?
Return a cons of (DOC-POS . MAC), where DOC-POS is the position
in FUN of the doc string (if any), depending on what FUN is,
and MAC is `macro' if additionally FUN is a macro, else nil.

If it's something else, return nil."
  (if (consp fun)
      (let ((mac (and (eq (car-safe fun) 'macro) 'macro)))
        (if (eq mac 'macro)
            (setq fun (cdr fun)))
        (if (consp fun)
            (let ((doc-pos (cond ((eq (car fun) 'lambda) 2)
                                 ((eq (car fun) 'closure) 3)
                                 (t nil))))
              (and doc-pos (cons doc-pos mac)))))))

(defun byte-run--fun-get-string (fun doc-pos/m)
  "Get the doc string (or nil) from function form FUN.
DOC-POS/M is a cons of FUN's doc string position and whether it's
a macro.

Return the doc sring."
  (if (cdr doc-pos/m)
      (setq fun (cdr fun)))
  (and (stringp (nth (car doc-pos/m) fun))
       (nth (car doc-pos/m) fun)))

(defun byte-run--fun-put-new-string (fun doc-string doc-pos/m)
  "Put a new doc string into FUN.
FUN is an interpreted function form, DOC-STRING is the new doc
string, including any position information on DOC-POS/M is a cons
of FUN's DOC-POS and whether it's a macro.

Create and return a new form rather than altering the old one."
  (if (cdr doc-pos/m)
      (setq fun (cdr fun)))
  (let* ((doc-pos (car doc-pos/m))
         (insert (null (stringp (nth doc-pos fun)))))
    (nconc (take doc-pos fun)
         (list doc-string)
         (nthcdr (if insert doc-pos (1+ doc-pos)) fun))))

(defun byte-run--fun-get-lambda-pos (fun doc-pos/m)
  "Get the position (if any) of the lambda symbol from FUN.
FUN is a function form, DOC-POS/M is a cons of FUN's DOC-POS and
whether it's a macro.

Return the position of the lambda or closure symbol from FUN."
  (if (cdr doc-pos/m)
      (setq fun (cdr fun)))
  (and (symbol-with-pos-p (car fun))
       (symbol-with-pos-pos (car fun))))

(defun byte-run-strip-pos-info (string)
  "Remove the POS info, if any, from STRING, returning what's left.
STRING may be nil.

If no changes are made, return the original STRING.  If there are
no characters other than the POS info, return nil instead."
  (if string
      (let (start index)
        (while
            (and (setq index (string-match ";POS.\001\001\001 " string start))
                 (string-match "\n" string index))
          (setq start (match-end 0)))
        (cond
         ((and start (< start (length string)))
          (substring string start))
         ((and start (eq start (length string)))
          nil)
         ((null index) string)))))

(defun byte-run-posify-existing-defaliases-1 (sym)
  "Sub function of `byte-run-posify-existing-defaliases'."
  (let ((defining-symbol (get sym 'byte-run--early-defalias))) ; Symbol with pos.
    (if defining-symbol
        (let* ((fun (symbol-function sym))
               (doc-pos/m (byte-run--fun-doc-pos/macro fun)))
          (if doc-pos/m
              (let*
                  ((lambda-pos (byte-run--fun-get-lambda-pos fun doc-pos/m))
                   (old-doc-string (byte-run--fun-get-string fun doc-pos/m))
                   (bare-doc-string (byte-run-strip-pos-info old-doc-string))
                   (new-doc-string (byte-run-posify-doc-string bare-doc-string
                                                               lambda-pos)))
                (byte-run--fun-put-new-string fun new-doc-string doc-pos/m)))))))

(defun byte-run-posify-existing-defaliases ()
  "Create the position structure in the doc strings of existing functions.
At the same time, strip the positions from the defining symbol and the
lambda."
  ;; This function should be run with `symbols-with-pos-enabled'
  ;; non-nil.  We can't use a lambda form here, since it would have a
  ;; position on the lambda symbol.
  (mapatoms #'byte-run-posify-existing-defaliases-1))

(defun byte-run-posify-existing-lambdas ()
  "Create the position structure in the doc strings of existing lambdas.
At the same time, strip the positions from the defining symbol and
the lambda."
  (let ((tail early-lambda-lists))
    (while tail
      (let* ((elt (car tail))  ; (((lambda (..) ...) #<symbol lambda at N>
                               ; #<symbol foo at M>))
             (pointer (car elt))           ; ((lambda (..) ...))
             (form-elt (car (cdr elt))) ; #<symbol lambda at N>
             (defining-symbol (car (cdr (cdr elt)))))
        (setcar (car pointer) form-elt)
        (if (null defining-symbol)
            (message "byte-run-posify-existing-lambdas: null defining-symbol")
          (let ((form1 (byte-run-posify-lambda-form
                        (car pointer) ;; form
                        (and (symbol-with-pos-p (car pointer))
                             (symbol-with-pos-pos (car pointer))))))
               (setcar pointer
                       form1)
            (if (null byte-compile-in-progress)
                (setcar (car pointer) 'lambda))))) ; Strip any position.
      (setq tail (cdr tail)))))

;; Redefined in byte-opt.el.
;; This was undocumented and unused for decades.
(defalias 'inline 'progn
  "Like `progn', but when compiled inline top-level function calls in body.
You don't need this.  (See bytecomp.el commentary for more details.)

\(fn BODY...)")

;;; Interface to inline functions.

;; (defmacro proclaim-inline (&rest fns)
;;   "Cause the named functions to be open-coded when called from compiled code.
;; They will only be compiled open-coded when byte-compile-optimize is true."
;;   (cons 'eval-and-compile
;; 	(mapcar (lambda (x)
;; 		   (or (memq (get x 'byte-optimizer)
;; 			     '(nil byte-compile-inline-expand))
;; 		       (error
;; 			"%s already has a byte-optimizer, can't make it inline"
;; 			x))
;; 		   (list 'put (list 'quote x)
;; 			 ''byte-optimizer ''byte-compile-inline-expand))
;; 		fns)))

;; (defmacro proclaim-notinline (&rest fns)
;;   "Cause the named functions to no longer be open-coded."
;;   (cons 'eval-and-compile
;; 	(mapcar (lambda (x)
;; 		   (if (eq (get x 'byte-optimizer) 'byte-compile-inline-expand)
;; 		       (put x 'byte-optimizer nil))
;; 		   (list 'if (list 'eq (list 'get (list 'quote x) ''byte-optimizer)
;; 				   ''byte-compile-inline-expand)
;; 			 (list 'put x ''byte-optimizer nil)))
;; 		fns)))

(defmacro defsubst (name arglist &rest body)
  "Define an inline function.  The syntax is just like that of `defun'.

\(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
  (declare (debug defun) (doc-string 3) (indent 2)
           (defining-symbol name))
  (let ((bare-name (bare-symbol name)))
  (or (memq (get bare-name
                 'byte-optimizer)
	    '(nil byte-compile-inline-expand))
      (error "`%s' is a primitive" name))
  `(prog1
       (defun ,name ,arglist ,@body)
     (eval-and-compile
       ;; Never native-compile defsubsts as we need the byte
       ;; definition in `byte-compile-unfold-bcf' to perform the
       ;; inlining (Bug#42664, Bug#43280, Bug#44209).
       ,(byte-run--set-speed name nil -1)
       (put ',bare-name
            'byte-optimizer 'byte-compile-inline-expand)))))

(defvar advertised-signature-table (make-hash-table :test 'eq :weakness 'key))

(defun set-advertised-calling-convention (function signature _when)
  "Set the advertised SIGNATURE of FUNCTION.
This will allow the byte-compiler to warn the programmer when she uses
an obsolete calling convention.  WHEN specifies since when the calling
convention was modified."
  (puthash (indirect-function function) signature
           advertised-signature-table))

(defun get-advertised-calling-convention (function)
  "Get the advertised SIGNATURE of FUNCTION.
Return t if there isn't any."
  (gethash function advertised-signature-table t))

(defun byte-run--constant-obsolete-warning (obsolete-name)
  (if (memq obsolete-name '(nil t))
      (error "Can't make `%s' obsolete; did you forget a quote mark?"
             obsolete-name)))

(defun make-obsolete (obsolete-name current-name when)
  "Make the byte-compiler warn that function OBSOLETE-NAME is obsolete.
OBSOLETE-NAME should be a function name or macro name (a symbol).

The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message
\(it should end with a period, and not start with a capital).
WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (byte-run--constant-obsolete-warning obsolete-name)
  (put obsolete-name 'byte-obsolete-info
       ;; The second entry used to hold the `byte-compile' handler, but
       ;; is not used any more nowadays.
       (purecopy (list current-name nil when)))
  obsolete-name)

(defmacro define-obsolete-function-alias ( obsolete-name current-name when
                                           &optional docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"28.1\" \
\"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"28.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4) (indent defun))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))

(defun make-obsolete-variable ( obsolete-name current-name when
                                &optional access-type)
  "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message.
WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number.
ACCESS-TYPE if non-nil should specify the kind of access that will trigger
  obsolescence warnings; it can be either `get' or `set'."
  (byte-run--constant-obsolete-warning obsolete-name)
  (put obsolete-name 'byte-obsolete-variable
       (purecopy (list current-name access-type when)))
  obsolete-name)

(defmacro define-obsolete-variable-alias ( obsolete-name current-name when
                                           &optional docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.

This macro evaluates all its parameters, and both OBSOLETE-NAME
and CURRENT-NAME should be symbols, so a typical usage would look like:

  (define-obsolete-variable-alias \\='foo-thing \\='bar-thing \"28.1\")

This macro uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
  (declare (doc-string 4) (indent defun))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     ;; See Bug#4706.
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

;; FIXME This is only defined in this file because the variable- and
;; function- versions are too.  Unlike those two, this one is not used
;; by the byte-compiler (would be nice if it could warn about obsolete
;; faces, but it doesn't really do anything special with faces).
;; It only really affects M-x describe-face output.
(defmacro define-obsolete-face-alias (obsolete-face current-face when)
  "Make OBSOLETE-FACE a face alias for CURRENT-FACE and mark it obsolete.
WHEN should be a string indicating when the face was first made
obsolete, for example a date or a release number."
  `(progn
     (put ,obsolete-face 'face-alias ,current-face)
     ;; Used by M-x describe-face.
     (put ,obsolete-face 'obsolete-face (or (purecopy ,when) t))))

(defmacro dont-compile (&rest body)
  "Like `progn', but the body always runs interpreted (not compiled).
If you think you need this, you're probably making a mistake somewhere."
  (declare (debug t) (indent 0) (obsolete nil "24.4"))
  (list 'eval (list 'quote (if (cdr body) (cons 'progn body) (car body)))))


;; interface to evaluating things at compile time and/or load time
;; these macro must come after any uses of them in this file, as their
;; definition in the file overrides the magic definitions on the
;; byte-compile-macro-environment.

(defmacro eval-when-compile (&rest body)
  "Like `progn', but evaluates the body at compile time if you're compiling.
Thus, the result of the body appears to the compiler as a quoted
constant.  In interpreted code, this is entirely equivalent to
`progn', except that the value of the expression may be (but is
not necessarily) computed at load time if eager macro expansion
is enabled."
  (declare (debug (&rest def-form)) (indent 0))
  (list 'quote (eval (cons 'progn body) lexical-binding)))

(defmacro eval-and-compile (&rest body)
  "Like `progn', but evaluates the body at compile time and at load time.
In interpreted code, this is entirely equivalent to `progn',
except that the value of the expression may be (but is not
necessarily) computed at load time if eager macro expansion is
enabled."
  (declare (debug (&rest def-form)) (indent 0))
  ;; When the byte-compiler expands code, this macro is not used, so we're
  ;; either about to run `body' (plain interpretation) or we're doing eager
  ;; macroexpansion.
  (list 'quote (eval (cons 'progn body) lexical-binding)))

(defun with-no-warnings (&rest body)
  "Like `progn', but prevents compiler warnings in the body."
  (declare (indent 0))
  ;; The implementation for the interpreter is basically trivial.
  (car (last body)))

(defmacro with-suppressed-warnings (warnings &rest body)
  "Like `progn', but prevents compiler WARNINGS in BODY.

WARNINGS is an association list where the first element of each
item is a warning type, and the rest of the elements in each item
are symbols they apply to.  For instance, if you want to suppress
byte compilation warnings about the two obsolete functions `foo'
and `bar', as well as the function `zot' being called with the
wrong number of parameters, say

\(with-suppressed-warnings ((obsolete foo bar)
                           (callargs zot))
  (foo (bar))
  (zot 1 2))

The warnings that can be suppressed are a subset of the warnings
in `byte-compile-warning-types'; see the variable
`byte-compile-warnings' for a fuller explanation of the warning
types.  The types that can be suppressed with this macro are
`free-vars', `callargs', `redefine', `obsolete',
`interactive-only', `lexical', `ignored-return-value', `constants',
`suspicious', `empty-body' and `mutate-constant'."
  ;; Note: during compilation, this definition is overridden by the one in
  ;; byte-compile-initial-macro-environment.
  (declare (debug (sexp body)) (indent 1))
  (if (null (and (featurep 'macroexp)
                (boundp 'byte-compile--suppressed-warnings)))
      ;; If `macroexp' is not yet loaded, we're in the middle of
      ;; bootstrapping, so better risk emitting too many warnings
      ;; than risk breaking the bootstrap.
      `(progn ,@body)
    ;; We need to let-bind byte-compile--suppressed-warnings here, so as to
    ;; silence warnings emitted during macro-expansion performed outside of
    ;; byte-compilation.
    (let ((byte-compile--suppressed-warnings
           (append warnings byte-compile--suppressed-warnings)))
      (macroexpand-all (macroexp-progn body)
                       macroexpand-all-environment))))

(defun byte-run--unescaped-character-literals-warning ()
  "Return a warning about unescaped character literals.
If there were any unescaped character literals in the last form
read, return an appropriate warning message as a string.
Otherwise, return nil.  For internal use only."
  ;; This is called from lread.c and therefore needs to be preloaded.
  (if lread--unescaped-character-literals
      (let ((sorted (sort lread--unescaped-character-literals #'<)))
        (format "unescaped character literals %s detected, %s expected!"
                (mapconcat (lambda (char) (format-message "`?%c'" char))
                           sorted ", ")
                (mapconcat (lambda (char) (format-message "`?\\%c'" char))
                           sorted ", ")))))

(defun byte-compile-info (string &optional message type)
  "Format STRING in a way that looks pleasing in the compilation output.
If MESSAGE, output the message, too.

If TYPE, it should be a string that says what the information
type is.  This defaults to \"INFO\"."
  (let ((string (format "  %-9s%s" (or type "INFO") string)))
    (when message
      (message "%s" string))
    string))

(defun byte-compile-info-string (&rest args)
  "Format ARGS in a way that looks pleasing in the compilation output."
  (declare (obsolete byte-compile-info "28.1"))
  (byte-compile-info (apply #'format args)))

(defun byte-compile-info-message (&rest args)
  "Message format ARGS in a way that looks pleasing in the compilation output."
  (declare (obsolete byte-compile-info "28.1"))
  (byte-compile-info (apply #'format args) t))


;; I nuked this because it's not a good idea for users to think of using it.
;; These options are a matter of installation preference, and have nothing to
;; with particular source files; it's a mistake to suggest to users
;; they should associate these with particular source files.
;; There is hardly any reason to change these parameters, anyway.
;; --rms.

;; (put 'byte-compiler-options 'lisp-indent-function 0)
;; (defmacro byte-compiler-options (&rest args)
;;   "Set some compilation-parameters for this file.  This will affect only the
;; file in which it appears; this does nothing when evaluated, and when loaded
;; from a .el file.
;;
;; Each argument to this macro must be a list of a key and a value.
;;
;;   Keys:		  Values:		Corresponding variable:
;;
;;   verbose	  t, nil		byte-compile-verbose
;;   optimize	  t, nil, source, byte	byte-compile-optimize
;;   warnings	  list of warnings	byte-compile-warnings
;; 		      Valid elements: (callargs redefine free-vars unresolved)
;;   file-format	  emacs18, emacs19	byte-compile-compatibility
;;
;; For example, this might appear at the top of a source file:
;;
;;     (byte-compiler-options
;;       (optimize t)
;;       (warnings (- free-vars))		; Don't warn about free variables
;;       (file-format emacs19))"
;;   nil)

;;; byte-run.el ends here
