;;; cl-generic.el --- CLOS-style generic functions for Elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;; This implements the most of CLOS's multiple-dispatch generic functions.
;; To use it you need either (require 'cl-generic) or (require 'cl-lib).
;; The main entry points are: `cl-defgeneric' and `cl-defmethod'.

;; Missing elements:
;; - We don't support make-method, call-method, define-method-combination.
;;   CLOS's define-method-combination is IMO overly complicated, and it suffers
;;   from a significant problem: the method-combination code returns a sexp
;;   that needs to be `eval'uated or compiled.  IOW it requires run-time
;;   code generation.  Given how rarely method-combinations are used,
;;   I just provided a cl-generic-method-combination-function, which
;;   people can use if they are really desperate for such functionality.
;; - In defgeneric we don't support the options:
;;   declare, :method-combination, :generic-function-class, :method-class,
;;   :method.
;; Added elements:
;; - We support aliases to generic functions.
;; - The kind of thing on which to dispatch can be extended.
;;   There is support in this file for dispatch on:
;;   - (eql <val>)
;;   - plain old types
;;   - type of CL structs
;;   eieio-core adds dispatch on:
;;   - class of eieio objects
;;   - actual class argument, using the syntax (subclass <class>).
;; - cl-generic-method-combination-function (i.s.o define-method-combination).
;; - cl-generic-call-method (which replaces make-method and call-method).

;; Efficiency considerations: overall, I've made an effort to make this fairly
;; efficient for the expected case (e.g. no constant redefinition of methods).
;; - Generic functions which do not dispatch on any argument are implemented
;;   optimally (just as efficient as plain old functions).
;; - Generic functions which only dispatch on one argument are fairly efficient
;;   (not a lot of room for improvement, I think).
;; - Multiple dispatch is implemented rather naively.  There's an extra `apply'
;;   function call for every dispatch; we don't optimize each dispatch
;;   based on the set of candidate methods remaining; we don't optimize the
;;   order in which we performs the dispatches either;  If/when this
;;   becomes a problem, we can try and optimize it.
;; - call-next-method could be made more efficient, but isn't too terrible.

;;; Code:

;; Note: For generic functions that dispatch on several arguments (i.e. those
;; which use the multiple-dispatch feature), we always use the same "tagcodes"
;; and the same set of arguments on which to dispatch.  This works, but is
;; often suboptimal since after one dispatch, the remaining dispatches can
;; usually be simplified, or even completely skipped.

;; TODO/FIXME:
;; - WIBNI we could use something like
;;   (add-function :before (cl-method-function (cl-find-method ...)) ...)

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'pcase))

(defvar cl-generic-tagcode-function
  (lambda (type _name)
    (if (eq type t) '(0 . 'cl--generic-type)
      (error "Unknown specializer %S" type)))
  "Function to get the Elisp code to extract the tag on which we dispatch.
Takes a \"parameter-specializer-name\" and a variable name, and returns
a pair (PRIORITY . CODE) where CODE is an Elisp expression that should be
used to extract the \"tag\" (from the object held in the named variable)
that should uniquely determine if we have a match
\(i.e. the \"tag\" is the value that will be used to dispatch to the proper
method(s)).
Such \"tagcodes\" will be or'd together.
PRIORITY is an integer from 0 to 100 which is used to sort the tagcodes
in the `or'.  The higher the priority, the more specific the tag should be.
More specifically, if PRIORITY is N and we have two objects X and Y
whose tag (according to TAGCODE) is `eql', then it should be the case
that for all other (PRIORITY . TAGCODE) where PRIORITY ≤ N, then
\(eval TAGCODE) for X is `eql' to (eval TAGCODE) for Y.")

(defvar cl-generic-tag-types-function
  (lambda (tag) (if (eq tag 'cl--generic-type) '(t)))
  "Function to get the list of types that a given \"tag\" matches.
They should be sorted from most specific to least specific.")

(cl-defstruct (cl--generic-method
               (:constructor nil)
               (:constructor cl--generic-method-make
                (specializers qualifiers uses-cnm function))
               (:predicate nil))
  (specializers nil :read-only t :type list)
  (qualifiers   nil :read-only t :type (list-of atom))
  ;; USES-CNM is a boolean indicating if FUNCTION expects an extra argument
  ;; holding the next-method.
  (uses-cnm     nil :read-only t :type boolean)
  (function     nil :read-only t :type function))

(cl-defstruct (cl--generic
               (:constructor nil)
               (:constructor cl--generic-make
                (name &optional dispatches method-table))
               (:predicate nil))
  (name nil :type symbol :read-only t)  ;Pointer back to the symbol.
  ;; `dispatches' holds a list of (ARGNUM . TAGCODES) where ARGNUM is the index
  ;; of the corresponding argument and TAGCODES is a list of (PRIORITY . EXP)
  ;; where the EXPs are expressions (to be `or'd together) to compute the tag
  ;; on which to dispatch and PRIORITY is the priority of each expression to
  ;; decide in which order to sort them.
  ;; The most important dispatch is last in the list (and the least is first).
  (dispatches nil :type (list-of (cons natnum (list-of tagcode))))
  (method-table nil :type (list-of cl--generic-method)))

(defmacro cl--generic (name)
  `(get ,name 'cl--generic))

(defun cl-generic-ensure-function (name)
  (let (generic
        (origname name))
    (while (and (null (setq generic (cl--generic name)))
                (fboundp name)
                (symbolp (symbol-function name)))
      (setq name (symbol-function name)))
    (unless (or (not (fboundp name))
                (autoloadp (symbol-function name))
                (and (functionp name) generic))
      (error "%s is already defined as something else than a generic function"
             origname))
    (if generic
        (cl-assert (eq name (cl--generic-name generic)))
      (setf (cl--generic name) (setq generic (cl--generic-make name)))
      (defalias name (cl--generic-make-function generic)))
    generic))

(defun cl--generic-setf-rewrite (name)
  (let* ((setter (intern (format "cl-generic-setter--%s" name)))
         (exp `(unless (eq ',setter (get ',name 'cl-generic-setter))
                 ;; (when (get ',name 'gv-expander)
                 ;;   (error "gv-expander conflicts with (setf %S)" ',name))
                 (setf (get ',name 'cl-generic-setter) ',setter)
                 (gv-define-setter ,name (val &rest args)
                   (cons ',setter (cons val args))))))
    ;; Make sure `setf' can be used right away, e.g. in the body of the method.
    (eval exp t)
    (cons setter exp)))

;;;###autoload
(defmacro cl-defgeneric (name args &rest options-and-methods)
  "Create a generic function NAME.
DOC-STRING is the base documentation for this class.  A generic
function has no body, as its purpose is to decide which method body
is appropriate to use.  Specific methods are defined with `cl-defmethod'.
With this implementation the ARGS are currently ignored.
OPTIONS-AND-METHODS currently understands:
- (:documentation DOCSTRING)
- (declare DECLARATIONS)"
  (declare (indent 2) (doc-string 3))
  (let* ((docprop (assq :documentation options-and-methods))
         (doc (cond ((stringp (car-safe options-and-methods))
                     (pop options-and-methods))
                    (docprop
                     (prog1
                         (cadr docprop)
                       (setq options-and-methods
                             (delq docprop options-and-methods))))))
         (declarations (assq 'declare options-and-methods)))
    (when declarations
      (setq options-and-methods
            (delq declarations options-and-methods)))
    `(progn
       ,(when (eq 'setf (car-safe name))
          (pcase-let ((`(,setter . ,code) (cl--generic-setf-rewrite
                                           (cadr name))))
            (setq name setter)
            code))
       ,@(mapcar (lambda (declaration)
                   (let ((f (cdr (assq (car declaration)
                                       defun-declarations-alist))))
                     (cond
                      (f (apply (car f) name args (cdr declaration)))
                      (t (message "Warning: Unknown defun property `%S' in %S"
                                  (car declaration) name)
                         nil))))
                 (cdr declarations))
       (defalias ',name
         (cl-generic-define ',name ',args ',options-and-methods)
         ,(help-add-fundoc-usage doc args)))))

(defun cl--generic-mandatory-args (args)
  (let ((res ()))
    (while (not (memq (car args) '(nil &rest &optional &key)))
      (push (pop args) res))
    (nreverse res)))

;;;###autoload
(defun cl-generic-define (name args options-and-methods)
  (let ((generic (cl-generic-ensure-function name))
        (mandatory (cl--generic-mandatory-args args))
        (apo (assq :argument-precedence-order options-and-methods)))
    (setf (cl--generic-dispatches generic) nil)
    (when apo
      (dolist (arg (cdr apo))
        (let ((pos (memq arg mandatory)))
          (unless pos (error "%S is not a mandatory argument" arg))
          (push (list (- (length mandatory) (length pos)))
                (cl--generic-dispatches generic)))))
    (setf (cl--generic-method-table generic) nil)
    (cl--generic-make-function generic)))

(defmacro cl-generic-current-method-specializers ()
  "List of (VAR . TYPE) where TYPE is var's specializer.
This macro can only be used within the lexical scope of a cl-generic method."
  (error "cl-generic-current-method-specializers used outside of a method"))

(eval-and-compile         ;Needed while compiling the cl-defmethod calls below!
  (defun cl--generic-fgrep (vars sexp)    ;Copied from pcase.el.
    "Check which of the symbols VARS appear in SEXP."
    (let ((res '()))
      (while (consp sexp)
        (dolist (var (cl--generic-fgrep vars (pop sexp)))
          (unless (memq var res) (push var res))))
      (and (memq sexp vars) (not (memq sexp res)) (push sexp res))
      res))

  (defun cl--generic-lambda (args body)
    "Make the lambda expression for a method with ARGS and BODY."
    (let ((plain-args ())
          (specializers nil)
          (mandatory t))
      (dolist (arg args)
        (push (pcase arg
                ((or '&optional '&rest '&key) (setq mandatory nil) arg)
                ((and `(,name . ,type) (guard mandatory))
                 (push (cons name (car type)) specializers)
                 name)
                (_ arg))
              plain-args))
      (setq plain-args (nreverse plain-args))
      (let ((fun `(cl-function (lambda ,plain-args ,@body)))
            (macroenv (cons `(cl-generic-current-method-specializers
                              . ,(lambda () specializers))
                            macroexpand-all-environment)))
        (require 'cl-lib)        ;Needed to expand `cl-flet' and `cl-function'.
        ;; First macroexpand away the cl-function stuff (e.g. &key and
        ;; destructuring args, `declare' and whatnot).
        (pcase (macroexpand fun macroenv)
          (`#'(lambda ,args . ,body)
           (let* ((parsed-body (macroexp-parse-body body))
                  (cnm (make-symbol "cl--cnm"))
                  (nmp (make-symbol "cl--nmp"))
                  (nbody (macroexpand-all
                          `(cl-flet ((cl-call-next-method ,cnm)
                                     (cl-next-method-p ,nmp))
                             ,@(cdr parsed-body))
                          macroenv))
                  ;; FIXME: Rather than `grep' after the fact, the
                  ;; macroexpansion should directly set some flag when cnm
                  ;; is used.
                  ;; FIXME: Also, optimize the case where call-next-method is
                  ;; only called with explicit arguments.
                  (uses-cnm (cl--generic-fgrep (list cnm nmp) nbody)))
             (cons (not (not uses-cnm))
                   `#'(lambda (,@(if uses-cnm (list cnm)) ,@args)
                        ,@(delq nil (car parsed-body))
                        ,(if (not (memq nmp uses-cnm))
                             nbody
                           `(let ((,nmp (lambda ()
                                          (cl--generic-isnot-nnm-p ,cnm))))
                              ,nbody))))))
          (f (error "Unexpected macroexpansion result: %S" f)))))))


;;;###autoload
(defmacro cl-defmethod (name args &rest body)
  "Define a new method for generic function NAME.
I.e. it defines the implementation of NAME to use for invocations where the
value of the dispatch argument matches the specified TYPE.
The dispatch argument has to be one of the mandatory arguments, and
all methods of NAME have to use the same argument for dispatch.
The dispatch argument and TYPE are specified in ARGS where the corresponding
formal argument appears as (VAR TYPE) rather than just VAR.

The optional second argument QUALIFIER is a specifier that
modifies how the method is combined with other methods, including:
   :before  - Method will be called before the primary
   :after   - Method will be called after the primary
   :around  - Method will be called around everything else
The absence of QUALIFIER means this is a \"primary\" method.

Other than a type, TYPE can also be of the form `(eql VAL)' in
which case this method will be invoked when the argument is `eql' to VAL.

\(fn NAME [QUALIFIER] ARGS &rest [DOCSTRING] BODY)"
  (declare (doc-string 3) (indent 2)
           (debug
            (&define                    ; this means we are defining something
             [&or name ("setf" :name setf name)]
             ;; ^^ This is the methods symbol
             [ &optional keywordp ]     ; this is key :before etc
             list                       ; arguments
             [ &optional stringp ]      ; documentation string
             def-body)))                ; part to be debugged
  (let ((qualifiers nil)
        (setfizer (if (eq 'setf (car-safe name))
                      ;; Call it before we call cl--generic-lambda.
                      (cl--generic-setf-rewrite (cadr name)))))
    (while (not (listp args))
      (push args qualifiers)
      (setq args (pop body)))
    (pcase-let* ((`(,uses-cnm . ,fun) (cl--generic-lambda args body)))
      `(progn
         ,(when setfizer
            (setq name (car setfizer))
            (cdr setfizer))
         ,(and (get name 'byte-obsolete-info)
               (or (not (fboundp 'byte-compile-warning-enabled-p))
                   (byte-compile-warning-enabled-p 'obsolete))
               (let* ((obsolete (get name 'byte-obsolete-info)))
                 (macroexp--warn-and-return
                  (macroexp--obsolete-warning name obsolete "generic function")
                  nil)))
         ;; You could argue that `defmethod' modifies rather than defines the
         ;; function, so warnings like "not known to be defined" are fair game.
         ;; But in practice, it's common to use `cl-defmethod'
         ;; without a previous `cl-defgeneric'.
         (declare-function ,name "")
         (cl-generic-define-method ',name ',qualifiers ',args
                                   ,uses-cnm ,fun)))))

(defun cl--generic-member-method (specializers qualifiers methods)
  (while
      (and methods
           (let ((m (car methods)))
             (not (and (equal (cl--generic-method-specializers m) specializers)
                       (equal (cl--generic-method-qualifiers m) qualifiers)))))
    (setq methods (cdr methods)))
  methods)

;;;###autoload
(defun cl-generic-define-method (name qualifiers args uses-cnm function)
  (let* ((generic (cl-generic-ensure-function name))
         (mandatory (cl--generic-mandatory-args args))
         (specializers
          (mapcar (lambda (arg) (if (consp arg) (cadr arg) t)) mandatory))
         (method (cl--generic-method-make
                  specializers qualifiers uses-cnm function))
         (mt (cl--generic-method-table generic))
         (me (cl--generic-member-method specializers qualifiers mt))
         (dispatches (cl--generic-dispatches generic))
         (i 0))
    (dolist (specializer specializers)
      (let* ((tagcode (funcall cl-generic-tagcode-function specializer 'arg))
             (x (assq i dispatches)))
        (unless x
          (setq x (list i (funcall cl-generic-tagcode-function t 'arg)))
          (setf (cl--generic-dispatches generic)
                (setq dispatches (cons x dispatches))))
        (unless (member tagcode (cdr x))
          (setf (cdr x)
                (nreverse (sort (cons tagcode (cdr x))
                                #'car-less-than-car))))
        (setq i (1+ i))))
    (if me (setcar me method)
      (setf (cl--generic-method-table generic) (cons method mt)))
    (cl-pushnew `(cl-defmethod . (,(cl--generic-name generic) . ,specializers))
                current-load-list :test #'equal)
    (let ((gfun (cl--generic-make-function generic))
          ;; Prevent `defalias' from recording this as the definition site of
          ;; the generic function.
          current-load-list)
      ;; For aliases, cl--generic-name gives us the actual name.
      (defalias (cl--generic-name generic) gfun))))

(defmacro cl--generic-with-memoization (place &rest code)
  (declare (indent 1) (debug t))
  (gv-letplace (getter setter) place
    `(or ,getter
         ,(macroexp-let2 nil val (macroexp-progn code)
            `(progn
               ,(funcall setter val)
               ,val)))))

(defvar cl--generic-dispatchers (make-hash-table :test #'equal))

(defun cl--generic-get-dispatcher (tagcodes dispatch-arg)
  (cl--generic-with-memoization
      (gethash (cons dispatch-arg tagcodes) cl--generic-dispatchers)
    (let ((lexical-binding t)
          (tag-exp `(or ,@(mapcar #'cdr
				  ;; Minor optimization: since this tag-exp is
				  ;; only used to lookup the method-cache, it
				  ;; doesn't matter if the default value is some
				  ;; constant or nil.
				  (if (macroexp-const-p (car (last tagcodes)))
				      (butlast tagcodes)
				    tagcodes))))
          (extraargs ()))
      (dotimes (_ dispatch-arg)
        (push (make-symbol "arg") extraargs))
      (byte-compile
       `(lambda (generic dispatches-left)
          (let ((method-cache (make-hash-table :test #'eql)))
            (lambda (,@extraargs arg &rest args)
              (apply (cl--generic-with-memoization
                         (gethash ,tag-exp method-cache)
                       (cl--generic-cache-miss
                        generic ',dispatch-arg dispatches-left
                        (list ,@(mapcar #'cdr tagcodes))))
                     ,@extraargs arg args))))))))

(defun cl--generic-make-function (generic)
  (let* ((dispatches (cl--generic-dispatches generic))
         (dispatch
          (progn
            (while (and dispatches
                        (member (cdar dispatches)
                                '(nil ((0 . 'cl--generic-type)))))
              (setq dispatches (cdr dispatches)))
            (pop dispatches))))
    (if (null dispatch)
        (cl--generic-build-combined-method
         (cl--generic-name generic)
	 (cl--generic-method-table generic))
      (let ((dispatcher (cl--generic-get-dispatcher
                         (cdr dispatch) (car dispatch))))
        (funcall dispatcher generic dispatches)))))

(defvar cl-generic-method-combination-function
  #'cl--generic-standard-method-combination
  "Function to build the effective method.
Called with 2 arguments: NAME and METHOD-ALIST.
It should return an effective method, i.e. a function that expects the same
arguments as the methods, and calls those methods in some appropriate order.
NAME is the name (a symbol) of the corresponding generic function.
METHOD-ALIST is a list of elements (QUALIFIERS . METHODS) where
QUALIFIERS is a list of qualifiers, and METHODS is a list of the selected
methods for that qualifier list.
The METHODS lists are sorted from most generic first to most specific last.
The function can use `cl-generic-call-method' to create functions that call those
methods.")

(defvar cl--generic-combined-method-memoization
  (make-hash-table :test #'equal :weakness 'value)
  "Table storing previously built combined-methods.
This is particularly useful when many different tags select the same set
of methods, since this table then allows us to share a single combined-method
for all those different tags in the method-cache.")

(defun cl--generic-build-combined-method (generic-name methods)
  (cl--generic-with-memoization
      (gethash (cons generic-name methods)
               cl--generic-combined-method-memoization)
    (let ((mets-by-qual ()))
      (dolist (method methods)
        (let* ((qualifiers (cl--generic-method-qualifiers method))
               (x (assoc qualifiers mets-by-qual)))
          ;; FIXME: sadly, alist-get only uses `assq' and we need `assoc'.
          ;;(push (cdr qm) (alist-get qualifiers mets-by-qual)))
          (if x
              (push method (cdr x))
            (push (list qualifiers method) mets-by-qual))))
      (funcall cl-generic-method-combination-function
               generic-name mets-by-qual))))

(defun cl--generic-no-next-method-function (generic method)
  (lambda (&rest args)
    (apply #'cl-no-next-method generic method args)))

(defun cl-generic-call-method (generic-name method &optional fun)
  "Return a function that calls METHOD.
FUN is the function that should be called when METHOD calls
`call-next-method'."
  (if (not (cl--generic-method-uses-cnm method))
      (cl--generic-method-function method)
    (let ((met-fun (cl--generic-method-function method))
          (next (or fun (cl--generic-no-next-method-function
                         generic-name method))))
      (lambda (&rest args)
        (apply met-fun
               ;; FIXME: This sucks: passing just `next' would
               ;; be a lot more efficient than the lambda+apply
               ;; quasi-η, but we need this to implement the
               ;; "if call-next-method is called with no
               ;; arguments, then use the previous arguments".
               (lambda (&rest cnm-args)
                 (apply next (or cnm-args args)))
               args)))))

(defun cl--generic-standard-method-combination (generic-name mets-by-qual)
  (dolist (x mets-by-qual)
    (unless (member (car x) '(() (:after) (:before) (:around)))
      (error "Unsupported qualifiers in function %S: %S" generic-name (car x))))
  (cond
   ((null mets-by-qual)
    (lambda (&rest args)
      (apply #'cl-no-applicable-method generic-name args)))
   ((null (alist-get nil mets-by-qual))
    (lambda (&rest args)
      (apply #'cl-no-primary-method generic-name args)))
   (t
    (let* ((fun nil)
           (ab-call (lambda (m) (cl-generic-call-method generic-name m)))
           (before
            (mapcar ab-call (reverse (cdr (assoc '(:before) mets-by-qual)))))
           (after (mapcar ab-call (cdr (assoc '(:after) mets-by-qual)))))
      (dolist (method (cdr (assoc nil mets-by-qual)))
        (setq fun (cl-generic-call-method generic-name method fun)))
      (when (or after before)
        (let ((next fun))
          (setq fun (lambda (&rest args)
                      (dolist (bf before)
                        (apply bf args))
                      (prog1
                          (apply next args)
                        (dolist (af after)
                          (apply af args)))))))
      (dolist (method (cdr (assoc '(:around) mets-by-qual)))
        (setq fun (cl-generic-call-method generic-name method fun)))
      fun))))

(defconst cl--generic-nnm-sample (cl--generic-no-next-method-function t t))
(defconst cl--generic-cnm-sample
  (funcall (cl--generic-build-combined-method
            nil (list (cl--generic-method-make () () t #'identity)))))

(defun cl--generic-isnot-nnm-p (cnm)
  "Return non-nil if CNM is the function that calls `cl-no-next-method'."
  ;; ¡Big Gross Ugly Hack!
  ;; `next-method-p' just sucks, we should let it die.  But EIEIO did support
  ;; it, and some packages use it, so we need to support it.
  (catch 'found
    (cl-assert (function-equal cnm cl--generic-cnm-sample))
    (if (byte-code-function-p cnm)
        (let ((cnm-constants (aref cnm 2))
              (sample-constants (aref cl--generic-cnm-sample 2)))
          (dotimes (i (length sample-constants))
            (when (function-equal (aref sample-constants i)
                                  cl--generic-nnm-sample)
              (throw 'found
                     (not (function-equal (aref cnm-constants i)
                                          cl--generic-nnm-sample))))))
      (cl-assert (eq 'closure (car-safe cl--generic-cnm-sample)))
      (let ((cnm-env (cadr cnm)))
        (dolist (vb (cadr cl--generic-cnm-sample))
          (when (function-equal (cdr vb) cl--generic-nnm-sample)
            (throw 'found
                   (not (function-equal (cdar cnm-env)
                                        cl--generic-nnm-sample))))
          (setq cnm-env (cdr cnm-env)))))
    (error "Haven't found no-next-method-sample in cnm-sample")))

(defun cl--generic-cache-miss (generic dispatch-arg dispatches-left tags)
  (let ((types (apply #'append (mapcar cl-generic-tag-types-function tags)))
        (methods '()))
    (dolist (method (cl--generic-method-table generic))
      (let* ((specializer (or (nth dispatch-arg
                                   (cl--generic-method-specializers method))
                              t))
             (m (member specializer types)))
        (when m
          (push (cons (length m) method) methods))))
    ;; Sort the methods, most specific first.
    ;; It would be tempting to sort them once and for all in the method-table
    ;; rather than here, but the order might depend on the actual argument
    ;; (e.g. for multiple inheritance with defclass).
    (setq methods (nreverse (mapcar #'cdr (sort methods #'car-less-than-car))))
    (cl--generic-make-function (cl--generic-make (cl--generic-name generic)
                                                 dispatches-left methods))))

;;; Define some pre-defined generic functions, used internally.

(define-error 'cl-no-method "No method for %S")
(define-error 'cl-no-next-method "No next method for %S" 'cl-no-method)
(define-error 'cl-no-primary-method "No primary method for %S" 'cl-no-method)
(define-error 'cl-no-applicable-method "No applicable method for %S"
  'cl-no-method)

(cl-defgeneric cl-no-next-method (generic method &rest args)
  "Function called when `cl-call-next-method' finds no next method.")
(cl-defmethod cl-no-next-method (generic method &rest args)
  (signal 'cl-no-next-method `(,generic ,method ,@args)))

(cl-defgeneric cl-no-applicable-method (generic &rest args)
  "Function called when a method call finds no applicable method.")
(cl-defmethod cl-no-applicable-method (generic &rest args)
  (signal 'cl-no-applicable-method `(,generic ,@args)))

(cl-defgeneric cl-no-primary-method (generic &rest args)
  "Function called when a method call finds no primary method.")
(cl-defmethod cl-no-primary-method (generic &rest args)
  (signal 'cl-no-primary-method `(,generic ,@args)))

(defun cl-call-next-method (&rest _args)
  "Function to call the next applicable method.
Can only be used from within the lexical body of a primary or around method."
  (error "cl-call-next-method only allowed inside primary and around methods"))

(defun cl-next-method-p ()
  "Return non-nil if there is a next method.
Can only be used from within the lexical body of a primary or around method."
  (declare (obsolete "make sure there's always a next method, or catch `cl-no-next-method' instead" "25.1"))
  (error "cl-next-method-p only allowed inside primary and around methods"))

;;;###autoload
(defun cl-find-method (generic qualifiers specializers)
  (car (cl--generic-member-method
        specializers qualifiers
        (cl--generic-method-table (cl--generic generic)))))

(defalias 'cl-method-qualifiers 'cl--generic-method-qualifiers)

;;; Add support for describe-function

(defun cl--generic-search-method (met-name)
  (let ((base-re (concat "(\\(?:cl-\\)?defmethod[ \t]+"
                         (regexp-quote (format "%s" (car met-name)))
			 "\\_>")))
    (or
     (re-search-forward
      (concat base-re "[^&\"\n]*"
              (mapconcat (lambda (specializer)
                           (regexp-quote
                            (format "%S" (if (consp specializer)
                                             (nth 1 specializer) specializer))))
                         (remq t (cdr met-name))
                         "[ \t\n]*)[^&\"\n]*"))
      nil t)
     (re-search-forward base-re nil t))))


(with-eval-after-load 'find-func
  (defvar find-function-regexp-alist)
  (add-to-list 'find-function-regexp-alist
               `(cl-defmethod . ,#'cl--generic-search-method)))

(defun cl--generic-method-info (method)
  (let* ((specializers (cl--generic-method-specializers method))
         (qualifiers   (cl--generic-method-qualifiers method))
         (uses-cnm     (cl--generic-method-uses-cnm method))
         (function     (cl--generic-method-function method))
         (args (help-function-arglist function 'names))
         (docstring (documentation function))
         (qual-string
          (if (null qualifiers) ""
            (cl-assert (consp qualifiers))
            (let ((s (prin1-to-string qualifiers)))
              (concat (substring s 1 -1) " "))))
         (doconly (if docstring
                      (let ((split (help-split-fundoc docstring nil)))
                        (if split (cdr split) docstring))))
         (combined-args ()))
    (if uses-cnm (setq args (cdr args)))
    (dolist (specializer specializers)
      (let ((arg (if (eq '&rest (car args))
                     (intern (format "arg%d" (length combined-args)))
                   (pop args))))
        (push (if (eq specializer t) arg (list arg specializer))
              combined-args)))
    (setq combined-args (append (nreverse combined-args) args))
    (list qual-string combined-args doconly)))

(add-hook 'help-fns-describe-function-functions #'cl--generic-describe)
(defun cl--generic-describe (function)
  (let ((generic (if (symbolp function) (cl--generic function))))
    (when generic
      (require 'help-mode)              ;Needed for `help-function-def' button!
      (save-excursion
        (insert "\n\nThis is a generic function.\n\n")
        (insert (propertize "Implementations:\n\n" 'face 'bold))
        ;; Loop over fanciful generics
        (dolist (method (cl--generic-method-table generic))
          (let* ((info (cl--generic-method-info method)))
            ;; FIXME: Add hyperlinks for the types as well.
            (insert (format "%s%S" (nth 0 info) (nth 1 info)))
            (let* ((met-name (cons function
                                   (cl--generic-method-specializers method)))
                   (file (find-lisp-object-file-name met-name 'cl-defmethod)))
              (when file
                (insert " in `")
                (help-insert-xref-button (help-fns-short-filename file)
                                         'help-function-def met-name file
                                         'cl-defmethod)
                (insert "'.\n")))
            (insert "\n" (or (nth 2 info) "Undocumented") "\n\n")))))))

;;; Support for (eql <val>) specializers.

(defvar cl--generic-eql-used (make-hash-table :test #'eql))

(add-function :before-until cl-generic-tagcode-function
              #'cl--generic-eql-tagcode)
(defun cl--generic-eql-tagcode (type name)
  (when (eq (car-safe type) 'eql)
    (puthash (cadr type) type cl--generic-eql-used)
    `(100 . (gethash ,name cl--generic-eql-used))))

(add-function :before-until cl-generic-tag-types-function
              #'cl--generic-eql-tag-types)
(defun cl--generic-eql-tag-types (tag)
  (if (eq (car-safe tag) 'eql) (list tag)))

;;; Support for cl-defstructs specializers.

(add-function :before-until cl-generic-tagcode-function
              #'cl--generic-struct-tagcode)

(defun cl--generic-struct-tag (name)
  `(and (vectorp ,name)
        (> (length ,name) 0)
        (let ((tag (aref ,name 0)))
          (if (eq (symbol-function tag) :quick-object-witness-check)
              tag))))

(defun cl--generic-struct-tagcode (type name)
  (and (symbolp type)
       (get type 'cl-struct-type)
       (or (null (car (get type 'cl-struct-type)))
           (error "Can't dispatch on cl-struct %S: type is %S"
                  type (car (get type 'cl-struct-type))))
       (or (equal '(cl-tag-slot) (car (get type 'cl-struct-slots)))
           (error "Can't dispatch on cl-struct %S: no tag in slot 0"
                  type))
       ;; It's tempting to use (and (vectorp ,name) (aref ,name 0))
       ;; but that would suffer from some problems:
       ;; - the vector may have size 0.
       ;; - when called on an actual vector (rather than an object), we'd
       ;;   end up returning an arbitrary value, possibly colliding with
       ;;   other tagcode's values.
       ;; - it can also result in returning all kinds of irrelevant
       ;;   values which would end up filling up the method-cache with
       ;;   lots of irrelevant/redundant entries.
       ;; FIXME: We could speed this up by introducing a dedicated
       ;; vector type at the C level, so we could do something like
       ;; (and (vector-objectp ,name) (aref ,name 0))
       `(50 . ,(cl--generic-struct-tag name))))

(add-function :before-until cl-generic-tag-types-function
              #'cl--generic-struct-tag-types)
(defun cl--generic-struct-tag-types (tag)
  ;; FIXME: cl-defstruct doesn't make it easy for us.
  (and (symbolp tag)
       ;; A method call shouldn't itself mess with the match-data.
       (string-match-p "\\`cl-struct-\\(.*\\)" (symbol-name tag))
       (let ((types (list (intern (substring (symbol-name tag) 10)))))
         (while (get (car types) 'cl-struct-include)
           (push (get (car types) 'cl-struct-include) types))
         (push 'cl-structure-object types) ;The "parent type" of all cl-structs.
         (nreverse types))))

;;; Dispatch on "system types".

(defconst cl--generic-typeof-types
  ;; Hand made from the source code of `type-of'.
  '((integer number) (symbol) (string array sequence) (cons list sequence)
    ;; Markers aren't `numberp', yet they are accepted wherever integers are
    ;; accepted, pretty much.
    (marker) (overlay) (float number) (window-configuration)
    (process) (window) (subr) (compiled-function) (buffer)
    (char-table array sequence)
    (bool-vector array sequence)
    (frame) (hash-table) (font-spec) (font-entity) (font-object)
    (vector array sequence)
    ;; Plus, hand made:
    (null symbol list sequence)
    (list sequence)
    (array sequence)
    (sequence)
    (number)))

(add-function :before-until cl-generic-tagcode-function
              #'cl--generic-typeof-tagcode)
(defun cl--generic-typeof-tagcode (type name)
  ;; FIXME: Add support for other types accepted by `cl-typep' such
  ;; as `character', `atom', `face', `function', ...
  (and (assq type cl--generic-typeof-types)
       (progn
         (if (memq type '(vector array sequence))
             (message "`%S' also matches CL structs and EIEIO classes" type))
         ;; FIXME: We could also change `type-of' to return `null' for nil.
         `(10 . (if ,name (type-of ,name) 'null)))))

(add-function :before-until cl-generic-tag-types-function
              #'cl--generic-typeof-types)
(defun cl--generic-typeof-types (tag)
  (and (symbolp tag)
       (assq tag cl--generic-typeof-types)))

;;; Just for kicks: dispatch on major-mode
;;
;; Here's how you'd use it:
;;   (cl-defmethod foo ((x (major-mode text-mode)) y z) ...)
;; And then
;;     (foo 'major-mode toto titi)
;;
;; FIXME: Better would be to do that via dispatch on an "implicit argument".
;; E.g. (cl-defmethod foo (y z &context (major-mode text-mode)) ...)

;; (defvar cl--generic-major-modes (make-hash-table :test #'eq))
;;
;; (add-function :before-until cl-generic-tagcode-function
;;               #'cl--generic-major-mode-tagcode)
;; (defun cl--generic-major-mode-tagcode (type name)
;;   (if (eq 'major-mode (car-safe type))
;;       `(50 . (if (eq ,name 'major-mode)
;;                  (cl--generic-with-memoization
;;                      (gethash major-mode cl--generic-major-modes)
;;                    `(cl--generic-major-mode . ,major-mode))))))
;;
;; (add-function :before-until cl-generic-tag-types-function
;;               #'cl--generic-major-mode-types)
;; (defun cl--generic-major-mode-types (tag)
;;   (when (eq (car-safe tag) 'cl--generic-major-mode)
;;     (if (eq tag 'fundamental-mode) '(fundamental-mode t)
;;       (let ((types `((major-mode ,(cdr tag)))))
;;         (while (get (car types) 'derived-mode-parent)
;;           (push (list 'major-mode (get (car types) 'derived-mode-parent))
;;                 types))
;;         (unless (eq 'fundamental-mode (car types))
;;           (push '(major-mode fundamental-mode) types))
;;         (nreverse types)))))

;; Local variables:
;; generated-autoload-file: "cl-loaddefs.el"
;; End:

(provide 'cl-generic)
;;; cl-generic.el ends here
