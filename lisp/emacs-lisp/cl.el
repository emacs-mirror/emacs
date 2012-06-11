;;; cl.el --- Compatibility aliases for the old CL library.

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: extensions

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

;; This is a compatibility file which provides the old names provided by CL
;; before we cleaned up its namespace usage.

;;; Code:

(require 'cl-lib)
(require 'macroexp)

;; (defun cl--rename ()
;;   (let ((vdefs ())
;;         (fdefs ())
;;         (case-fold-search nil)
;;         (files '("cl.el" "cl-macs.el" "cl-seq.el" "cl-extra.el")))
;;     (dolist (file files)
;;       (with-current-buffer (find-file-noselect file)
;;         (goto-char (point-min))
;;         (while (re-search-forward
;;                 "^(\\(def[^ \t\n]*\\) +'?\\(\\(\\sw\\|\\s_\\)+\\)" nil t)
;;           (let ((name (match-string-no-properties 2))
;;                 (type (match-string-no-properties 1)))
;;             (unless (string-match-p "\\`cl-" name)
;;               (cond
;;                ((member type '("defvar" "defconst"))
;;                 (unless (member name vdefs) (push name vdefs)))
;;                ((member type '("defun" "defsubst" "defalias" "defmacro"))
;;                 (unless (member name fdefs) (push name fdefs)))
;;                ((member type '("def-edebug-spec" "defsetf" "define-setf-method"
;;                                "define-compiler-macro"))
;;                 nil)
;;                (t (error "Unknown type %S" type))))))))
;;     (let ((re (concat "\\_<" (regexp-opt (append vdefs fdefs)) "\\_>"))
;;           (conflicts ()))
;;       (dolist (file files)
;;         (with-current-buffer (find-file-noselect file)
;;           (goto-char (point-min))
;;           (while (re-search-forward re nil t)
;;             (replace-match "cl-\\&"))
;;           (save-buffer))))
;;     (with-current-buffer (find-file-noselect "cl-rename.el")
;;       (dolist (def vdefs)
;;         (insert (format "(defvaralias '%s 'cl-%s)\n" def def)))
;;       (dolist (def fdefs)
;;         (insert (format "(defalias '%s 'cl-%s)\n" def def)))
;;       (save-buffer))))

;; (defun cl--unrename ()
;;   ;; Taken from "Naming Conventions" node of the doc.
;;   (let* ((names '(defun* defsubst* defmacro* function* member*
;;                          assoc* rassoc* get* remove* delete*
;;                          mapcar* sort* floor* ceiling* truncate*
;;                          round* mod* rem* random*))
;;          (files '("cl.el" "cl-lib.el" "cl-macs.el" "cl-seq.el" "cl-extra.el"))
;;          (re (concat "\\_<cl-" (regexp-opt (mapcar #'symbol-name names))
;;                      "\\_>")))
;;     (dolist (file files)
;;       (with-current-buffer (find-file-noselect file)
;;         (goto-char (point-min))
;;         (while (re-search-forward re nil t)
;;           (delete-region (1- (point)) (point)))
;;         (save-buffer)))))
(dolist (var '(
               ;; loop-result-var
               ;; loop-result
               ;; loop-initially
               ;; loop-finally
               ;; loop-bindings
               ;; loop-args
               ;; bind-inits
               ;; bind-block
               ;; lambda-list-keywords
               float-negative-epsilon
               float-epsilon
               least-negative-normalized-float
               least-positive-normalized-float
               least-negative-float
               least-positive-float
               most-negative-float
               most-positive-float
               ;; custom-print-functions
               ))
  (defvaralias var (intern (format "cl-%s" var))))

(dolist (fun '(
               (get* . cl-get)
               (random* . cl-random)
               (rem* . cl-rem)
               (mod* . cl-mod)
               (round* . cl-round)
               (truncate* . cl-truncate)
               (ceiling* . cl-ceiling)
               (floor* . cl-floor)
               (rassoc* . cl-rassoc)
               (assoc* . cl-assoc)
               (member* . cl-member)
               (delete* . cl-delete)
               (remove* . cl-remove)
               (defsubst* . cl-defsubst)
               (sort* . cl-sort)
               (function* . cl-function)
               (defmacro* . cl-defmacro)
               (defun* . cl-defun)
               (mapcar* . cl-mapcar)

               remprop
               getf
               tailp
               list-length
               nreconc
               revappend
               concatenate
               subseq
               random-state-p
               make-random-state
               signum
               isqrt
               lcm
               gcd
               notevery
               notany
               every
               some
               mapcon
               mapcan
               mapl
               maplist
               map
               equalp
               coerce
               tree-equal
               nsublis
               sublis
               nsubst-if-not
               nsubst-if
               nsubst
               subst-if-not
               subst-if
               subsetp
               nset-exclusive-or
               set-exclusive-or
               nset-difference
               set-difference
               nintersection
               intersection
               nunion
               union
               rassoc-if-not
               rassoc-if
               assoc-if-not
               assoc-if
               member-if-not
               member-if
               merge
               stable-sort
               search
               mismatch
               count-if-not
               count-if
               count
               position-if-not
               position-if
               position
               find-if-not
               find-if
               find
               nsubstitute-if-not
               nsubstitute-if
               nsubstitute
               substitute-if-not
               substitute-if
               substitute
               delete-duplicates
               remove-duplicates
               delete-if-not
               delete-if
               remove-if-not
               remove-if
               replace
               fill
               reduce
               compiler-macroexpand
               define-compiler-macro
               assert
               check-type
               typep
               deftype
               defstruct
               define-modify-macro
               callf2
               callf
               letf*
               letf
               rotatef
               shiftf
               remf
               psetf
               setf
               get-setf-method
               defsetf
               (define-setf-method . cl-define-setf-expander)
               define-setf-expander
               declare
               the
               locally
               multiple-value-setq
               multiple-value-bind
               symbol-macrolet
               macrolet
               flet
               progv
               psetq
               do-all-symbols
               do-symbols
               dotimes
               dolist
               do*
               do
               loop
               return-from
               return
               block
               etypecase
               typecase
               ecase
               case
               load-time-value
               eval-when
               destructuring-bind
               gentemp
               gensym
               pairlis
               acons
               subst
               adjoin
               copy-list
               ldiff
               list*
               cddddr
               cdddar
               cddadr
               cddaar
               cdaddr
               cdadar
               cdaadr
               cdaaar
               cadddr
               caddar
               cadadr
               cadaar
               caaddr
               caadar
               caaadr
               caaaar
               cdddr
               cddar
               cdadr
               cdaar
               caddr
               cadar
               caadr
               caaar
               tenth
               ninth
               eighth
               seventh
               sixth
               fifth
               fourth
               third
               endp
               rest
               second
               first
               svref
               copy-seq
               evenp
               oddp
               minusp
               plusp
               floatp-safe
               declaim
               proclaim
               nth-value
               multiple-value-call
               multiple-value-apply
               multiple-value-list
               values-list
               values
               pushnew
               push
               pop
               decf
               incf
               ))
  (let ((new (if (consp fun) (prog1 (cdr fun) (setq fun (car fun)))
               (intern (format "cl-%s" fun)))))
    (defalias fun new)
    ;; If `cl-foo' is declare inline, then make `foo' inline as well, and
    ;; similarly.  Same for edebug specifications, indent rules and
    ;; doc-string position.
    ;; FIXME: For most of them, we should instead follow aliases
    ;; where applicable.
    (dolist (prop '(byte-optimizer doc-string-elt edebug-form-spec
                    lisp-indent-function))
      (if (get new prop)
        (put fun prop (get new prop))))))

(defvar cl-closure-vars nil)
(defvar cl--function-convert-cache nil)

(defun cl--function-convert (f)
  "Special macro-expander for special cases of (function F).
The two cases that are handled are:
- closure-conversion of lambda expressions for `lexical-let'.
- renaming of F when it's a function defined via `cl-labels' or `labels'."
  (require 'cl-macs)
  (declare-function cl--expr-contains-any "cl-macs" (x y))
  (cond
   ;; ¡¡Big Ugly Hack!! We can't use a compiler-macro because those are checked
   ;; *after* handling `function', but we want to stop macroexpansion from
   ;; being applied infinitely, so we use a cache to return the exact `form'
   ;; being expanded even though we don't receive it.
   ((eq f (car cl--function-convert-cache)) (cdr cl--function-convert-cache))
   ((eq (car-safe f) 'lambda)
    (let ((body (mapcar (lambda (f)
                          (macroexpand-all f macroexpand-all-environment))
                        (cddr f))))
      (if (and cl-closure-vars
               (cl--expr-contains-any body cl-closure-vars))
          (let* ((new (mapcar 'cl-gensym cl-closure-vars))
                 (sub (cl-pairlis cl-closure-vars new)) (decls nil))
            (while (or (stringp (car body))
                       (eq (car-safe (car body)) 'interactive))
              (push (list 'quote (pop body)) decls))
            (put (car (last cl-closure-vars)) 'used t)
            `(list 'lambda '(&rest --cl-rest--)
                   ,@(cl-sublis sub (nreverse decls))
                   (list 'apply
                         (list 'quote
                               #'(lambda ,(append new (cadr f))
                                   ,@(cl-sublis sub body)))
                         ,@(nconc (mapcar (lambda (x) `(list 'quote ,x))
                                          cl-closure-vars)
                                  '((quote --cl-rest--))))))
        (let* ((newf `(lambda ,(cadr f) ,@body))
               (res `(function ,newf)))
          (setq cl--function-convert-cache (cons newf res))
          res))))
   (t
    (let ((found (assq f macroexpand-all-environment)))
      (if (and found (ignore-errors
                       (eq (cadr (cl-caddr found)) 'cl-labels-args)))
          (cadr (cl-caddr (cl-cadddr found)))
        (let ((res `(function ,f)))
          (setq cl--function-convert-cache (cons f res))
          res))))))

(defmacro lexical-let (bindings &rest body)
  "Like `let', but lexically scoped.
The main visible difference is that lambdas inside BODY will create
lexical closures as in Common Lisp.
\n(fn BINDINGS BODY)"
  (declare (indent 1) (debug let))
  (let* ((cl-closure-vars cl-closure-vars)
	 (vars (mapcar (function
			(lambda (x)
			  (or (consp x) (setq x (list x)))
			  (push (make-symbol (format "--cl-%s--" (car x)))
				cl-closure-vars)
			  (set (car cl-closure-vars) [bad-lexical-ref])
			  (list (car x) (cadr x) (car cl-closure-vars))))
		       bindings))
	 (ebody
	  (macroexpand-all
           `(cl-symbol-macrolet
                ,(mapcar (lambda (x)
                           `(,(car x) (symbol-value ,(cl-caddr x))))
                         vars)
              ,@body)
	   (cons (cons 'function #'cl--function-convert)
                 macroexpand-all-environment))))
    (if (not (get (car (last cl-closure-vars)) 'used))
        ;; Turn (let ((foo (cl-gensym)))
        ;;        (set foo <val>) ...(symbol-value foo)...)
        ;; into (let ((foo <val>)) ...(symbol-value 'foo)...).
        ;; This is good because it's more efficient but it only works with
        ;; dynamic scoping, since with lexical scoping we'd need
        ;; (let ((foo <val>)) ...foo...).
	`(progn
           ,@(mapcar (lambda (x) `(defvar ,(cl-caddr x))) vars)
           (let ,(mapcar (lambda (x) (list (cl-caddr x) (cadr x))) vars)
           ,(cl-sublis (mapcar (lambda (x)
                              (cons (cl-caddr x)
                                    `',(cl-caddr x)))
                            vars)
                    ebody)))
      `(let ,(mapcar (lambda (x)
                       (list (cl-caddr x)
                             `(make-symbol ,(format "--%s--" (car x)))))
                     vars)
         (cl-setf ,@(apply #'append
                        (mapcar (lambda (x)
                                  (list `(symbol-value ,(cl-caddr x)) (cadr x)))
                                vars)))
         ,ebody))))

(defmacro lexical-let* (bindings &rest body)
  "Like `let*', but lexically scoped.
The main visible difference is that lambdas inside BODY, and in
successive bindings within BINDINGS, will create lexical closures
as in Common Lisp.  This is similar to the behavior of `let*' in
Common Lisp.
\n(fn BINDINGS BODY)"
  (declare (indent 1) (debug let))
  (if (null bindings) (cons 'progn body)
    (setq bindings (reverse bindings))
    (while bindings
      (setq body (list `(lexical-let (,(pop bindings)) ,@body))))
    (car body)))

;; This should really have some way to shadow 'byte-compile properties, etc.
;;;###autoload
(defmacro flet (bindings &rest body)
  "Make temporary function definitions.
This is an analogue of `let' that operates on the function cell of FUNC
rather than its value cell.  The FORMs are evaluated with the specified
function definitions in place, then the definitions are undone (the FUNCs
go back to their previous definitions, or lack thereof).

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)"
  (declare (indent 1) (debug cl-flet))
  `(cl-letf* ,(mapcar
            (lambda (x)
              (if (or (and (fboundp (car x))
                           (eq (car-safe (symbol-function (car x))) 'macro))
                      (cdr (assq (car x) macroexpand-all-environment)))
                  (error "Use `labels', not `flet', to rebind macro names"))
              (let ((func `(cl-function
                            (lambda ,(cadr x)
                              (cl-block ,(car x) ,@(cddr x))))))
                (when (cl--compiling-file)
                  ;; Bug#411.  It would be nice to fix this.
                  (and (get (car x) 'byte-compile)
                       (error "Byte-compiling a redefinition of `%s' \
will not work - use `labels' instead" (symbol-name (car x))))
                  ;; FIXME This affects the rest of the file, when it
                  ;; should be restricted to the flet body.
                  (and (boundp 'byte-compile-function-environment)
                       (push (cons (car x) (eval func))
                             byte-compile-function-environment)))
                (list `(symbol-function ',(car x)) func)))
            bindings)
     ,@body))

(defmacro labels (bindings &rest body)
  "Make temporary function bindings.
This is like `flet', except the bindings are lexical instead of dynamic.
Unlike `flet', this macro is fully compliant with the Common Lisp standard.

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)"
  (declare (indent 1) (debug cl-flet))
  (let ((vars nil) (sets nil) (newenv macroexpand-all-environment))
    (dolist (binding bindings)
      ;; It's important that (not (eq (symbol-name var1) (symbol-name var2)))
      ;; because these var's *names* get added to the macro-environment.
      (let ((var (make-symbol (format "--cl-%s--" (car binding)))))
	(push var vars)
	(push `(cl-function (lambda . ,(cdr binding))) sets)
	(push var sets)
	(push (cons (car binding)
                    `(lambda (&rest cl-labels-args)
                       (cl-list* 'funcall ',var
                                 cl-labels-args)))
              newenv)))
    (macroexpand-all `(lexical-let ,vars (setq ,@sets) ,@body) newenv)))

;;; Additional compatibility code
;; For names that were clean but really aren't needed any more.

(define-obsolete-function-alias 'cl-macroexpand 'macroexpand "24.2")
(define-obsolete-variable-alias 'cl-macro-environment
  'macroexpand-all-environment "24.2")
(define-obsolete-function-alias 'cl-macroexpand-all 'macroexpand-all "24.2")

;;; Hash tables.
;; This is just kept for compatibility with code byte-compiled by Emacs-20.

;; No idea if this might still be needed.
(defun cl-not-hash-table (x &optional y &rest z)
  (signal 'wrong-type-argument (list 'cl-hash-table-p (or y x))))
(make-obsolete 'cl-not-hash-table nil "24.2")

(defvar cl-builtin-gethash (symbol-function 'gethash))
(make-obsolete-variable 'cl-builtin-gethash nil "24.2")
(defvar cl-builtin-remhash (symbol-function 'remhash))
(make-obsolete-variable 'cl-builtin-remhash nil "24.2")
(defvar cl-builtin-clrhash (symbol-function 'clrhash))
(make-obsolete-variable 'cl-builtin-clrhash nil "24.2")
(defvar cl-builtin-maphash (symbol-function 'maphash))

(make-obsolete-variable 'cl-builtin-maphash nil "24.2")
(define-obsolete-function-alias 'cl-map-keymap 'map-keymap "24.2")
(define-obsolete-function-alias 'cl-copy-tree 'copy-tree "24.2")
(define-obsolete-function-alias 'cl-gethash 'gethash "24.2")
(define-obsolete-function-alias 'cl-puthash 'puthash "24.2")
(define-obsolete-function-alias 'cl-remhash 'remhash "24.2")
(define-obsolete-function-alias 'cl-clrhash 'clrhash "24.2")
(define-obsolete-function-alias 'cl-maphash 'maphash "24.2")
(define-obsolete-function-alias 'cl-make-hash-table 'make-hash-table "24.2")
(define-obsolete-function-alias 'cl-hash-table-p 'hash-table-p "24.2")
(define-obsolete-function-alias 'cl-hash-table-count 'hash-table-count "24.2")

(defun cl-maclisp-member (item list)
  (declare (obsolete member "24.2"))
  (while (and list (not (equal item (car list)))) (setq list (cdr list)))
  list)

;; FIXME: More candidates: define-modify-macro, define-setf-expander.

(provide 'cl)
;;; cl.el ends here
