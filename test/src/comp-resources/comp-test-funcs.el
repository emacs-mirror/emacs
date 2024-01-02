;;; comp-test-funcs.el --- compilation unit tested by comp-tests.el -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

;; Author: Andrea Corallo <acorallo@gnu.org>

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

(defvar comp-tests-var1 3)

(defun comp-tests-varref-f ()
  comp-tests-var1)

(defun comp-tests-list-f ()
  (list 1 2 3))
(defun comp-tests-list2-f (a b c)
  (list a b c))
(defun comp-tests-car-f (x)
  ;; Bcar
  (car x))
(defun comp-tests-cdr-f (x)
  ;; Bcdr
  (cdr x))
(defun comp-tests-car-safe-f (x)
  ;; Bcar_safe
  (car-safe x))
(defun comp-tests-cdr-safe-f (x)
  ;; Bcdr_safe
  (cdr-safe x))

(defun comp-tests-cons-car-f ()
  (car (cons 1 2)))
(defun comp-tests-cons-cdr-f (x)
  (cdr (cons 'foo x)))

(defun comp-tests-hint-fixnum-f (n)
  (1+ (comp-hint-fixnum n)))

(defun comp-tests-hint-cons-f (c)
  (car (comp-hint-cons c)))

(defun comp-tests-varset0-f ()
  (setq comp-tests-var1 55))
(defun comp-tests-varset1-f ()
  (setq comp-tests-var1 66)
  4)

(defun comp-tests-length-f ()
  (length '(1 2 3)))

(defun comp-tests-aref-aset-f ()
  (let ((vec (make-vector 3 0)))
    (aset vec 2 100)
    (aref vec 2)))

(defvar comp-tests-var2 3)
(defun comp-tests-symbol-value-f ()
  (symbol-value 'comp-tests-var2))

(defun comp-tests-concat-f (x)
  (concat "a" "b" "c" "d"
          (concat "a" "b" "c" (concat "a" "b" (concat "foo" x)))))

(defun comp-tests-ffuncall-callee-f (x y z)
  (list x y z))

(defun comp-tests-ffuncall-callee-optional-f (a b &optional c d)
  (list a b c d))

(defun comp-tests-ffuncall-callee-rest-f (a b &rest c)
  (list a b c))

(defun comp-tests-ffuncall-callee-more8-f (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10)
  ;; More then 8 args.
  (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10))

(defun comp-tests-ffuncall-callee-more8-rest-f (p1 p2 p3 p4 p5 p6 p7 p8 p9 &rest p10)
  ;; More then 8 args.
  (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10))

(defun comp-tests-ffuncall-native-f ()
  "Call a primitive with no dedicate op."
  (make-vector 1 nil))

(defun comp-tests-ffuncall-native-rest-f ()
  "Call a primitive with no dedicate op with &rest."
  (vector 1 2 3))

(defun comp-tests-ffuncall-apply-many-f (x)
  (apply #'list x))

(defun comp-tests-ffuncall-lambda-f (x)
  (let ((fun (lambda (x)
               (1+ x))))
    (funcall fun x)))

(defun comp-tests-jump-table-1-f (x)
  (pcase x
    ('x 'a)
    ('y 'b)
    (_ 'c)))

(defun comp-tests-jump-table-2-f (x)
  (pcase x
    ("aaa" 'a)
    ("bbb" 'b)))

(defun comp-tests-conditionals-1-f (x)
  ;; Generate goto-if-nil
  (if x 1 2))
(defun comp-tests-conditionals-2-f (x)
  ;; Generate goto-if-nil-else-pop
  (when x
    1340))

(defun comp-tests-fixnum-1-minus-f (x)
  ;; Bsub1
  (1- x))
(defun comp-tests-fixnum-1-plus-f (x)
  ;; Badd1
  (1+ x))
(defun comp-tests-fixnum-minus-f (x)
  ;; Bnegate
  (- x))

(defun comp-tests-eqlsign-f (x y)
  ;; Beqlsign
  (= x y))
(defun comp-tests-gtr-f (x y)
  ;; Bgtr
  (> x y))
(defun comp-tests-lss-f (x y)
  ;; Blss
  (< x y))
(defun comp-tests-les-f (x y)
  ;; Bleq
  (<= x y))
(defun comp-tests-geq-f (x y)
  ;; Bgeq
  (>= x y))

(defun comp-tests-setcar-f (x y)
  (setcar x y)
  x)
(defun comp-tests-setcdr-f (x y)
  (setcdr x y)
  x)

(defun comp-bubble-sort-f (list)
  (let ((i (length list)))
    (while (> i 1)
      (let ((b list))
        (while (cdr b)
          (when (< (cadr b) (car b))
            (setcar b (prog1 (cadr b)
                        (setcdr b (cons (car b) (cddr b))))))
          (setq b (cdr b))))
      (setq i (1- i)))
    list))

(defun comp-tests-consp-f (x)
  ;; Bconsp
  (consp x))
(defun comp-tests-setcar2-f (x)
  ;; Bsetcar
  (setcar x 3))

(defun comp-tests-integerp-f (x)
  ;; Bintegerp
  (integerp x))
(defun comp-tests-numberp-f (x)
  ;; Bnumberp
  (numberp x))

(defun comp-tests-discardn-f (_x)
  ;; BdiscardN
  (1+ (let ((a 1)
            (_b)
            (_c))
        a)))
(defun comp-tests-insertn-f (a b c d)
  ;; Binsert
  (insert a b c d))

(defun comp-tests-err-arith-f ()
  (/ 1 0))
(defun comp-tests-err-foo-f ()
  (error "Foo"))

(defun comp-tests-condition-case-0-f ()
  ;; Bpushhandler Bpophandler
  (condition-case
      err
      (comp-tests-err-arith-f)
    (arith-error (concat "arith-error "
                         (error-message-string err)
                         " caught"))
    (error (concat "error "
                   (error-message-string err)
                   " caught"))))
(defun comp-tests-condition-case-1-f ()
  ;; Bpushhandler Bpophandler
  (condition-case
      err
      (comp-tests-err-foo-f)
    (arith-error (concat "arith-error "
                         (error-message-string err)
                         " caught"))
    (error (concat "error "
                   (error-message-string err)
                   " caught"))))
(defun comp-tests-catch-f (f)
  (catch 'foo
    (funcall f)))
(defun comp-tests-throw-f (x)
  (throw 'foo x))

(defun comp-tests-buff0-f ()
  (with-temp-buffer
    (insert "foo")
    (buffer-string)))

(defun comp-tests-lambda-return-f ()
  (lambda (x) (1+ x)))

(defun comp-tests-fib-f (n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (comp-tests-fib-f (- n 1))
	      (comp-tests-fib-f (- n 2))))))

(defmacro comp-tests-macro-m (x)
  x)

(defun comp-tests-string-trim-f (url)
  (string-trim url))

(defun comp-tests-trampoline-removal-f ()
  (make-hash-table))

(defun comp-tests-signal-f ()
  (signal 'foo t))

(defun comp-tests-func-call-removal-f ()
  (let ((a 10)
	(b 3))
    (% a b)))

(defun comp-tests-doc-f ()
  "A nice docstring."
  t)

(defun comp-test-interactive-form0-f (dir)
  (interactive "D")
  dir)

(defun comp-test-interactive-form1-f (x y)
  (interactive '(1 2))
  (+ x y))

(defun comp-test-interactive-form2-f ()
  (interactive))

(defun comp-test-40187-2-f ()
  'foo)

(defalias 'comp-test-40187-1-f (symbol-function 'comp-test-40187-2-f))

(defun comp-test-40187-2-f ()
  'bar)

(defun comp-test-speed--1-f ()
  (declare (speed -1))
  3)

(defun comp-test-42360-f (str end-column
		              &optional start-column padding ellipsis
                              ellipsis-text-property)
  ;; From `truncate-string-to-width'.  A large enough function to
  ;; potentially use all registers and that is modifying local
  ;; variables inside condition-case.
  (let ((str-len (length str))
        (_str-width 14)
        (_ellipsis-width 3)
	(idx 0)
	(column 0)
	(head-padding "") (tail-padding "")
	ch last-column last-idx from-idx)
    (condition-case nil
	(while (< column start-column)
	  (setq ch (aref str idx)
		column (+ column (char-width ch))
		idx (1+ idx)))
      (args-out-of-range (setq idx str-len)))
    (if (< column start-column)
	(if padding (make-string end-column padding) "")
      (when (and padding (> column start-column))
	(setq head-padding (make-string (- column start-column) padding)))
      (setq from-idx idx)
      (when (>= end-column column)
	(condition-case nil
	    (while (< column end-column)
	      (setq last-column column
		    last-idx idx
		    ch (aref str idx)
		    column (+ column (char-width ch))
		    idx (1+ idx)))
	  (args-out-of-range (setq idx str-len)))
	(when (> column end-column)
	  (setq column last-column
		idx last-idx))
	(when (and padding (< column end-column))
	  (setq tail-padding (make-string (- end-column column) padding))))
      (if (and ellipsis-text-property
               (not (equal ellipsis ""))
               idx)
	  (concat head-padding
                  (substring str from-idx idx)
	          (propertize (substring str idx) 'display (or ellipsis "")))
        (concat head-padding (substring str from-idx idx)
	        tail-padding ellipsis)))))

(defun comp-test-primitive-advice-f (x y)
  (declare (speed 2))
  (+ x y))

(defun comp-test-primitive-redefine-f (x y)
  (declare (speed 2))
  (- x y))

(defsubst comp-test-defsubst-f ()
  t)

(defvar comp-test-and-3-var 1)
(defun comp-test-and-3-f (x)
  (and (atom x)
       comp-test-and-3-var
       2))

(defun comp-test-copy-insn-f (insn)
  ;; From `comp-copy-insn'.
  (if (consp insn)
      (let (result)
	(while (consp insn)
	  (let ((newcar (car insn)))
	    (if (or (consp (car insn)) (comp-mvar-p (car insn)))
		(setf newcar (comp-copy-insn (car insn))))
	    (push newcar result))
	  (setf insn (cdr insn)))
	(nconc (nreverse result)
               (if (comp-mvar-p insn) (comp-copy-insn insn) insn)))
    (if (comp-mvar-p insn)
        (copy-comp-mvar insn)
      insn)))

(defun comp-test-cond-rw-1-1-f ())

(defun comp-test-cond-rw-1-2-f ()
  (let ((it (comp-test-cond-rw-1-1-f))
	(key 't))
    (if (or (equal it key)
	    (eq key t))
	it
      nil)))

(defun comp-test-44968-f (start end)
  (let ((dirlist)
        (dir (expand-file-name start))
        (end (expand-file-name end)))
    (while (not (or (equal dir (car dirlist))
                    (file-equal-p dir end)))
      (push dir dirlist)
      (setq dir (directory-file-name (file-name-directory dir))))
    (nreverse dirlist)))

(defun comp-test-45342-f (n)
  (pcase n
    (1 " ➊") (2 " ➋") (3 " ➌") (4 " ➍") (5 " ➎") (6 " ➏")
    (7 " ➐") (8 " ➑") (9 " ➒") (10 " ➓") (_ "")))

(defun comp-test-assume-double-neg-f (collection value)
  ;; Reduced from `auth-source-search-collection'.
  (when (atom collection)
    (setq collection (list collection)))
  (or (eq value t)
      ;; value is (not (member t))
      (eq collection value)
      ;; collection is t, not (member t)!
      (member value collection)))

(defun comp-test-assume-in-loop-1-f (arg)
  ;; Reduced from `comint-delim-arg'.
  (let ((args nil)
	(pos 0)
	(len (length arg)))
    (while (< pos len)
      (let ((start pos))
	(while (< pos len)
	  (setq pos (1+ pos)))
	(setq args (cons (substring arg start pos) args))))
    args))

(defun comp-test-45376-1-f ()
  ;; Reduced from `eshell-ls-find-column-lengths'.
  (let* (res
	 (len 2)
	 (i 0)
	 (j 0))
    (while (< j len)
      (if (= i len)
	  (setq i 0))
      (setq res (cons i res)
	    j (1+ j)
	    i (1+ i)))
    res))

(defun comp-test-45376-2-f ()
  ;; Also reduced from `eshell-ls-find-column-lengths'.
  (let* ((x 1)
	 res)
    (while x
      (let* ((y 4)
	     (i 0))
	(while (> y 0)
	  (when (= i x)
	    (setq i 0))
	  (setf res (cons i res))
	  (setq y (1- y)
		i (1+ i)))
	(if (>= x 3)
	    (setq x nil)
	  (setq x (1+ x)))))
    res))

(defun comp-test-not-cons-f (x)
  ;; Reduced from `cl-copy-list'.
  (if (consp x)
      (print x)
    (car x)))

(defun comp-test-45576-f ()
  ;; Reduced from `eshell-find-alias-function'.
  (let ((sym (intern-soft "eval")))
    (if (and (functionp sym)
	     '(eshell-ls eshell-pred eshell-prompt eshell-script
			 eshell-term eshell-unix))
	sym)))

(defun comp-test-45635-f (&rest args)
  ;; Reduced from `set-face-attribute'.
  (let ((spec args)
	family)
    (while spec
      (cond ((eq (car spec) :family)
	     (setq family (cadr spec))))
      (setq spec (cddr spec)))
    (when (and (stringp family)
	       (string-match "\\([^-]*\\)-\\([^-]*\\)" family))
      (setq family (match-string 2 family)))
    (when (or (stringp family)
	      (eq family 'unspecified))
      family)))

;; This function doesn't have a doc string on purpose.
(defun comp-test-46670-1-f (_)
  "foo")

(defun comp-test-46670-2-f (s)
  (and (equal (comp-test-46670-1-f (length s)) s)
       s))

(cl-defun comp-test-46824-1-f ()
  (let ((next-repos '(1)))
    (while t
      (let ((_recipe (car next-repos)))
        (cl-block loop
          (while t
            (let ((err
                   (condition-case e
                       (progn
                         (setq next-repos
                               (cdr next-repos))
                         (cl-return-from loop))
                     (error e))))
              (format "%S"
                      (error-message-string err))))))
      (cl-return-from comp-test-46824-1-f))))

(defun comp-test-47868-1-f ()
  " ")

(defun comp-test-47868-2-f ()
  #(" " 0 1 (face font-lock-keyword-face)))

(defun comp-test-47868-3-f ()
  " ")

(defun comp-test-47868-4-f ()
  #(" " 0 1 (face font-lock-keyword-face)))

(defun comp-test-48029-nonascii-žžž-f (arg)
  (when arg t))


;;;;;;;;;;;;;;;;;;;;
;; Tromey's tests ;;
;;;;;;;;;;;;;;;;;;;;

;; Test Bconsp.
(defun comp-test-consp (x) (consp x))

;; Test Blistp.
(defun comp-test-listp (x) (listp x))

;; Test Bstringp.
(defun comp-test-stringp (x) (stringp x))

;; Test Bsymbolp.
(defun comp-test-symbolp (x) (symbolp x))

;; Test Bintegerp.
(defun comp-test-integerp (x) (integerp x))

;; Test Bnumberp.
(defun comp-test-numberp (x) (numberp x))

;; Test Badd1.
(defun comp-test-add1 (x) (1+ x))

;; Test Bsub1.
(defun comp-test-sub1 (x) (1- x))

;; Test Bneg.
(defun comp-test-negate (x) (- x))

;; Test Bnot.
(defun comp-test-not (x) (not x))

;; Test Bbobp, Beobp, Bpoint, Bpoint_min, Bpoint_max.
(defun comp-test-bobp () (bobp))
(defun comp-test-eobp () (eobp))
(defun comp-test-point () (point))
(defun comp-test-point-min () (point-min))
(defun comp-test-point-max () (point-max))

;; Test Bcar and Bcdr.
(defun comp-test-car (x) (car x))
(defun comp-test-cdr (x) (cdr x))

;; Test Bcar_safe and Bcdr_safe.
(defun comp-test-car-safe (x) (car-safe x))
(defun comp-test-cdr-safe (x) (cdr-safe x))

;; Test Beq.
(defun comp-test-eq (x y) (eq x y))

;; Test Bgotoifnil.
(defun comp-test-if (x y) (if x x y))

;; Test Bgotoifnilelsepop.
(defun comp-test-and (x y) (and x y))

;; Test Bgotoifnonnilelsepop.
(defun comp-test-or (x y) (or x y))

;; Test Bsave_excursion.
(defun comp-test-save-excursion ()
  (save-excursion
    (insert "XYZ")))

;; Test Bcurrent_buffer.
(defun comp-test-current-buffer () (current-buffer))

;; Test Bgtr.
(defun comp-test-> (a b)
  (> a b))

;; Test Bpushcatch.
(defun comp-test-catch (&rest l)
  (catch 'done
    (dolist (v l)
      (when (> v 23)
        (throw 'done v)))))

;; Test Bmemq.
(defun comp-test-memq (val list)
  (memq val list))

;; Test BlistN.
(defun comp-test-listN (x)
  (list x x x x x x x x x x x x x x x x))

;; Test BconcatN.
(defun comp-test-concatN (x)
  (concat x x x x x x))

;; Test optional and rest arguments.
(defun comp-test-opt-rest (a &optional b &rest c)
  (list a b c))

;; Test for too many arguments.
(defun comp-test-opt (a &optional b)
  (cons a b))

;; Test for unwind-protect.
(defvar comp-test-up-val nil)
(defun comp-test-unwind-protect (fun)
  (setq comp-test-up-val nil)
  (unwind-protect
      (progn
        (setq comp-test-up-val 23)
        (funcall fun)
        (setq comp-test-up-val 24))
    (setq comp-test-up-val 999)))

;; Non tested functions that proved just to be difficult to compile.

(defun comp-test-callee (_ __) t)
(defun comp-test-silly-frame1 (x)
  ;; Check robustness against dead code.
  (cl-case x
    (0 (comp-test-callee
        (pcase comp-tests-var1
          (1 1)
          (2 2))
        3))))

(defun comp-test-silly-frame2 (_token)
  ;; Check robustness against dead code.
  (while c
    (cl-case c
      (?< 1)
      (?> 2))))

(defun comp-test-big-interactive (filename &optional force arg load)
  "Check non trivial interactive form using `byte-recompile-file'."
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (derived-mode-p 'emacs-lisp-mode)
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (list (read-file-name (if current-prefix-arg
			       "Byte compile file: "
			     "Byte recompile file: ")
			   file-dir file-name nil)
	   current-prefix-arg)))
  (let ((dest (byte-compile-dest-file filename))
        ;; Expand now so we get the current buffer's defaults
        (filename (expand-file-name filename)))
    (if (if (file-exists-p dest)
            ;; File was already compiled
            ;; Compile if forced to, or filename newer
            (or force
                (file-newer-than-file-p filename dest))
          (and arg
               (or (eq 0 arg)
                   (y-or-n-p (concat "Compile "
                                     filename "? ")))))
        (progn
          (if (and noninteractive (not byte-compile-verbose))
              (message "Compiling %s..." filename))
          (byte-compile-file filename))
      (when load
	(load (if (file-exists-p dest) dest filename)))
      'no-byte-compile)))

(defun comp-test-no-return-1 (x)
  (while x
   (error "Foo")))

(defun comp-test-no-return-2 (x)
  (cond
   ((eql x '2) t)
   ((error "Bar") nil)))

(defun comp-test-no-return-3 ())
(defun comp-test-no-return-4 (x)
  (when x
    (error "Foo")
    (while (comp-test-no-return-3)
      (comp-test-no-return-3))))

(defun comp-test-=-nan (x)
  (when (= x 0.0e+NaN)
    x))

(defun comp-test-=-infinity (x)
  (when (= x 1.0e+INF)
    x))

(provide 'comp-test-funcs)

;;; comp-test-funcs.el ends here
