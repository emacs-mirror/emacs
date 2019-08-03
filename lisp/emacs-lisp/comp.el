;;; comp.el --- compilation of Lisp code into native code -*- lexical-binding: t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Keywords: lisp
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
;; This code is an attempt to make the pig fly.
;; Or, to put it another way to make a Carrera out of a turbocharged VW Bug.

;;; Code:

(require 'bytecomp)
(require 'cl-lib)
(require 'cl-extra)
(require 'subr-x)

(defgroup comp nil
  "Emacs Lisp native compiler."
  :group 'lisp)

(defconst comp-debug t)

;; FIXME these has to be removed
(defvar comp-speed 2)
(defvar byte-compile-lap-output)

(defvar comp-pass nil
  "Every pass has the right to bind what it likes here.")

(defconst comp-passes '(comp-spill-lap
                        comp-limplify)
  "Passes to be executed in order.")

(defconst comp-known-ret-types '((Fcons . cons))
  "Alist used for type propagation.")

(defconst comp-mostly-pure-funcs
  '(% * + - / /= 1+ 1- < <= = > >= cons list % concat logand logcount logior
      lognot logxor regexp-opt regexp-quote string-to-char string-to-syntax
      symbol-name)
  "Functions on witch we do constant propagation."
  ;;  Is it acceptable to move into the compile time functions that are
  ;; allocating memory? (these are technically not side effect free)
)

(eval-when-compile
  (defconst comp-op-stack-info
    (cl-loop with h = (make-hash-table)
	     for k across byte-code-vector
	     for v across byte-stack+-info
	     when k
	     do (puthash k v h)
	     finally return h)
    "Hash table lap-op -> stack adjustment."))

(cl-defstruct comp-args
  (min nil :type number
       :documentation "Minimum number of arguments allowed.")
  (max nil
       :documentation "Maximum number of arguments allowed.
To be used when ncall-conv is nil.")
  (ncall-conv nil :type boolean
              :documentation "If t the signature is:
(ptrdiff_t nargs, Lisp_Object *args)."))

(cl-defstruct (comp-block (:copier nil))
  "A basic block."
  ;; The first two slots are used during limplification.
  (sp nil
      :documentation "When non nil indicates the sp value while entering
into it.")
  (closed nil :type 'boolean
          :documentation "If the block was already closed.")
  (insns () :type list
         :documentation "List of instructions."))

(cl-defstruct (comp-func (:copier nil))
  "LIMPLE representation of a function."
  (symbol-name nil
               :documentation "Function symbol's name.")
  (c-func-name nil :type 'string
               :documentation "The function name in the native world.")
  (func nil
        :documentation "Original form.")
  (byte-func nil
             :documentation "Byte compiled version.")
  (lap () :type list
       :documentation "Lap assembly representation.")
  (args nil :type 'comp-args)
  (frame-size nil :type 'number)
  (blocks (make-hash-table) :type 'hash-table
          :documentation "Key is the basic block symbol value is a comp-block
structure.")
  (lap-block (make-hash-table :test #'equal) :type 'hash-table
             :documentation "Key value to convert from LAP label number to
LIMPLE basic block.")
  (ssa-cnt -1 :type 'number
              :documentation "Counter to create ssa limple vars."))

(cl-defstruct (comp-mvar (:copier nil) (:constructor make--comp-mvar))
  "A meta-variable being a slot in the meta-stack."
  (id nil :type number
     :documentation "SSA number.")
  (slot nil :type fixnum
        :documentation "Slot position.")
  (const-vld nil
             :documentation "Valid signal for the following slot.")
  (constant nil
            :documentation "When const-vld non nil this is used for constant
 propagation.")
  (type nil
        :documentation "When non nil is used for type propagation."))

(cl-defstruct (comp-limplify (:copier nil))
  "Support structure used during limplification."
  (sp 0 :type 'fixnum
      :documentation "Current stack pointer while walking LAP.")
  (frame nil :type 'vector
         :documentation "Meta-stack used to flat LAP.")
  (block-name nil :type 'symbol
    :documentation "Current basic block name."))

(defun comp-pretty-print-func (func)
  "Pretty print function FUNC in the current buffer."
  (insert (format "\n\n Function: %s" (comp-func-symbol-name func)))
  (cl-loop for block-name being each hash-keys of (comp-func-blocks func)
           using (hash-value bb)
           do (progn
                (insert (concat "\n<" (symbol-name block-name) ">"))
                (cl-prettyprint (comp-block-insns bb)))))


;;; spill-lap pass specific code.

(defun comp-c-func-name (symbol-function)
  "Given SYMBOL-FUNCTION return a name suitable for the native code."
  ;; Unfortunatelly not all symbol names are valid as C function names...
  ;; Nassi's algorithm here:
  (let* ((orig-name (symbol-name symbol-function))
         (crypted (cl-loop with str = (make-string (* 2 (length orig-name)) 0)
	                   for j from 0 by 2
	                   for i across orig-name
	                   for byte = (format "%x" i)
	                   do (aset str j (aref byte 0))
	                   do (aset str (1+ j) (aref byte 1))
	                   finally return str))
         (human-readable (replace-regexp-in-string
                          "-" "_" orig-name))
         (human-readable (replace-regexp-in-string
                          (rx (not (any "0-9a-z_"))) "" human-readable)))
    (concat "F" crypted "_" human-readable)))

(defun comp-decrypt-lambda-list (x)
  "Decript lambda list X."
  (let ((rest (not (= (logand x 128) 0)))
        (mandatory (logand x 127))
        (nonrest (ash x -8)))
    (if (and (null rest)
             (< nonrest 9)) ;; SUBR_MAX_ARGS
        (make-comp-args :min mandatory
                        :max nonrest)
      (make-comp-args :min mandatory
                      :ncall-conv t))))

(defun comp-spill-lap (func)
  "Byte compile and spill the LAP rapresentation for FUNC."
  (let (byte-compile-lap-output)
    (setf (comp-func-byte-func func)
          (byte-compile (comp-func-symbol-name func)))
    (when comp-debug
      (cl-prettyprint byte-compile-lap-output))
    (let ((lambda-list (aref (comp-func-byte-func func) 0)))
      (if (fixnump lambda-list)
          (setf (comp-func-args func)
                (comp-decrypt-lambda-list (aref (comp-func-byte-func func) 0)))
        (error "Can't native compile a non lexical scoped function")))
    (setf (comp-func-lap func) byte-compile-lap-output)
    (setf (comp-func-frame-size func) (aref (comp-func-byte-func func) 3))
    func))


;;; Limplification pass specific code.

;; Special vars used during limplifications
(defvar comp-block)
(defvar comp-func)

;; (defun comp-opt-call (inst)
;;   "Optimize if possible a side-effect-free call in INST."
;;   (cl-destructuring-bind (_ f &rest args) inst
;;     (when (and (member f comp-mostly-pure-funcs)
;;                (cl-every #'identity (mapcar #'comp-mvar-const-vld args)))
;;       (apply f (mapcar #'comp-mvar-constant args)))))

(defun comp-new-frame (size)
  "Return a clean frame of meta variables of size SIZE."
  (let ((v (make-vector size nil)))
    (cl-loop for i below size
             do (aset v i (make-comp-mvar :slot i)))
    v))

(cl-defun make-comp-mvar (&key slot (constant nil const-vld) type)
  (make--comp-mvar :id (cl-incf (comp-func-ssa-cnt comp-func))
                   :slot slot :const-vld const-vld :constant constant
                   :type type))

(defmacro comp-sp ()
  "Current stack pointer."
  '(comp-limplify-sp comp-pass))

(defmacro comp-with-sp (sp &rest body)
  "Execute BODY setting the stack pointer to SP.
Restore the original value afterwards."
  (declare (debug (form body))
           (indent defun))
  (let ((sym (gensym)))
    `(let ((,sym (comp-sp)))
       (setf (comp-sp) ,sp)
       (progn ,@body)
       (setf (comp-sp) ,sym))))

(defmacro comp-slot-n (n)
  "Slot N into the meta-stack."
  (declare (debug (form)))
  `(aref (comp-limplify-frame comp-pass) ,n))

(defmacro comp-slot ()
  "Current slot into the meta-stack pointed by sp."
  '(comp-slot-n (comp-sp)))

(defmacro comp-slot-next ()
  "Slot into the meta-stack pointed by sp + 1."
  '(comp-slot-n (1+ (comp-sp))))

(defun comp-emit (insn)
  "Emit INSN into current basic block."
  (push insn (comp-block-insns comp-block)))

(defun comp-emit-set-call (call)
  "Emit CALL assigning the result the the current slot frame.
If the calle function is known to have a return type propagate it."
  (cl-assert call)
  (setf (comp-slot)
        (make-comp-mvar :slot (comp-sp)
                        :type (when (> comp-speed 0)
                                (alist-get (cadr call)
                                           comp-known-ret-types))))
  (comp-emit (list 'set (comp-slot) call)))

(defmacro comp-emit-set-call-subr (subr-name sp-delta &optional c-fun-name)
  "Emit a call for SUBR-NAME using C-FUN-NAME.
SP-DELTA is the stack adjustment.
If C-FUN-NAME is nil it will be guessed from SUBR-NAME."
  (let ((subr (symbol-function subr-name))
        (subr-str (symbol-name subr-name))
        (nargs (1+ (- sp-delta))))
    (cl-assert (subrp subr) nil
               "%s not a subr" subr-str)
      (let* ((arity (subr-arity subr))
             (minarg (car arity))
             (maxarg (cdr arity)))
        (unless c-fun-name
          (setq c-fun-name
                (intern (concat "F"
                                (replace-regexp-in-string
                                 "-" "_"
                                 subr-str)))))
        (cl-assert (not (eq maxarg 'unevalled)) nil
                   "%s contains unevalled arg" subr-name)
        (if (eq maxarg 'many)
            ;; callref case.
            `(comp-emit-set-call (list 'callref ',c-fun-name ,nargs (comp-sp)))
          ;; Normal call.
          (cl-assert (and (>= maxarg nargs) (<= minarg nargs))
                     (nargs maxarg minarg)
                     "Incoherent stack adjustment %d, maxarg %d minarg %d")
          `(let* ((c-fun-name ',c-fun-name)
                  (slots (cl-loop for i from 0 below ,maxarg
                                  collect (comp-slot-n (+ i (comp-sp))))))
             (comp-emit-set-call `(call ,c-fun-name ,@slots)))))))

(defun comp-copy-slot (src-n &optional dst-n)
  "Set slot number DST-N to slot number SRC-N as source.
If DST-N is specified use it otherwise assume it to be the current slot."
  (comp-with-sp (if dst-n dst-n (comp-sp))
    (let ((src-slot (comp-slot-n src-n)))
      (cl-assert src-slot)
      ;; FIXME id should encrease here.
      (setf (comp-slot)
            (copy-sequence src-slot))
      (setf (comp-mvar-slot (comp-slot)) (comp-sp))
      (comp-emit (list 'set (comp-slot) src-slot)))))

(defun comp-emit-annotation (str)
  "Emit annotation STR."
  (comp-emit `(comment ,str)))

(defun comp-emit-set-const (val)
  "Set constant VAL to current slot."
  (setf (comp-slot) (make-comp-mvar :slot (comp-sp)
                                    :constant val))
  (comp-emit (list 'setimm (comp-slot) val)))

(defun comp-mark-block-closed ()
  "Mark current basic block as closed."
  (setf (comp-block-closed (gethash (comp-limplify-block-name comp-pass)
                                    (comp-func-blocks comp-func)))
        t))

(defun comp-emit-jump (target)
  "Emit an unconditional branch to block TARGET."
  (comp-emit (list 'jump target))
  (comp-mark-block-closed))

(defun comp-emit-block (block-name)
  "Emit basic block BLOCK-NAME."
  (let ((blocks (comp-func-blocks comp-func)))
    ;; In case does not exist register it into comp-func-blocks.
    (unless (gethash block-name blocks)
      (puthash block-name
               (make-comp-block :sp (comp-sp))
               blocks))
    ;; If we are abandoning an non closed basic block close it with a fall
    ;; through.
    (when (and (not (eq block-name 'entry))
               (not (comp-block-closed
                     (gethash (comp-limplify-block-name comp-pass)
                              blocks))))
      (comp-emit-jump block-name))
    ;; Set this a currently compiled block.
    (setf comp-block (gethash block-name blocks))
    ;; Every new block we are forced to wipe out all the frame.
    ;; This will be optimized by proper flow analysis.
    (setf (comp-limplify-frame comp-pass)
          (comp-new-frame (comp-func-frame-size comp-func)))
    ;; If we are landing here form a recorded branch adjust sp accordingly.
    (setf (comp-sp)
          (comp-block-sp (gethash block-name blocks)))
    (setf (comp-limplify-block-name comp-pass) block-name)))

(defun comp-emit-cond-jump (a b target-offset lap-label negated)
  "Emit a conditional jump to LAP-LABEL when A and B satisfy EQ.
TARGET-OFFSET is the positive offset on the SP when branching to the target
block.
If NEGATED non nil negate the tested condition."
  (let ((blocks (comp-func-blocks comp-func))
        (bb (comp-new-block-sym))) ;; Fall through block
    (puthash bb
	     (make-comp-block :sp (comp-sp))
	     blocks)
    (let ((target (comp-lap-to-limple-bb lap-label)))
      (comp-emit (if negated
		     (list 'cond-jump a b target bb)
		   (list 'cond-jump a b bb target)))
      (puthash target
	       (make-comp-block :sp (+ target-offset (comp-sp)))
	       blocks)
      (comp-mark-block-closed))
    (comp-emit-block bb)))

(defun comp-stack-adjust (n)
  "Move sp by N."
  (cl-incf (comp-sp) n))

(defun comp-limplify-listn (n)
  "Limplify list N."
  (comp-with-sp (+ (comp-sp) n -1)
    (comp-emit-set-call `(call Fcons
                               ,(comp-slot)
                               ,(make-comp-mvar :constant nil))))
  (cl-loop for sp from (+ (comp-sp) n -2) downto (comp-sp)
           do (comp-with-sp sp
                (comp-emit-set-call `(call Fcons
                                           ,(comp-slot)
                                           ,(comp-slot-next))))))

(defun comp-new-block-sym ()
  "Return a symbol naming the next new basic block."
  (intern (format "bb_%s" (hash-table-count (comp-func-blocks comp-func)))))

(defun comp-lap-to-limple-bb (n)
  "Given the LAP label N return the limple basic block."
  (let ((hash (comp-func-lap-block comp-func)))
    (if-let ((bb (gethash n hash)))
        ;; If was already created return it.
        bb
      (let ((name (comp-new-block-sym)))
        (puthash n name hash)
        name))))

(defun comp-emit-handler (guarded-label handler-type)
  "Emit a non local exit handler for GUARDED-LABEL of type HANDLER-TYPE."
  (let ((blocks (comp-func-blocks comp-func))
             (guarded-bb (comp-new-block-sym)))
         (puthash guarded-bb
	          (make-comp-block :sp (comp-sp))
	          blocks)
         (let ((handler-bb (comp-lap-to-limple-bb guarded-label)))
           (comp-emit (list 'push-handler (comp-slot-next)
                            handler-type
                            handler-bb
                            guarded-bb))
           (puthash handler-bb
	            (make-comp-block :sp (1+ (comp-sp)))
	            blocks)
           (comp-mark-block-closed)
           (comp-emit-block guarded-bb))))

(defun comp-emit-switch (var m-hash)
  "Emit a limple for a lap jump table given VAR and M-HASH."
  (cl-assert (comp-mvar-const-vld m-hash))
  (cl-loop for test being each hash-keys of (comp-mvar-constant m-hash)
           using (hash-value target-label)
           for m-test = (make-comp-mvar :constant test)
           do (comp-emit-cond-jump var m-test 0 target-label nil)))

(defmacro comp-op-case (&rest cases)
  "Expand CASES into the corresponding pcase.
This is responsible for generating the proper stack adjustment when known and
the annotation emission."
  (declare (debug (body))
           (indent defun))
  (cl-labels ((op-to-fun (x)
                 ;; Given the LAP op strip "byte-" to have the subr name.
                 (intern (replace-regexp-in-string "byte-" "" x)))
              (body-eff (body op-name sp-delta)
                 ;; Given the original body BODY compute the effective one.
                 ;; When BODY is auto guess function name form the LAP bytecode
                 ;; name. Othewise expect lname fnname.
                 (pcase (car body)
                   ('auto
                    (list `(comp-emit-set-call-subr
                            ,(op-to-fun op-name)
                            ,sp-delta)))
                   ((pred symbolp)
                    (list `(comp-emit-set-call-subr
                            ,(car body)
                            ,sp-delta
                            ,(cadr body))))
                   (_ body))))
    `(pcase op
       ,@(cl-loop for (op . body) in cases
		  for sp-delta = (gethash op comp-op-stack-info)
                  for op-name = (symbol-name op)
		  if body
		    collect `(',op
                              ;; Log all LAP ops except the TAG one.
                              ,(unless (eq op 'TAG)
                                 `(comp-emit-annotation
                                   ,(concat "LAP op " op-name)))
                              ;; Emit the stack adjustment if present.
                              ,(when (and sp-delta (not (eq 0 sp-delta)))
			         `(comp-stack-adjust ,sp-delta))
                              ,@(body-eff body op-name sp-delta))
                  else
		    collect `(',op (error ,(concat "Unsupported LAP op "
                                                 op-name))))
       (_ (error "Unexpected LAP op %s" (symbol-name op))))))

(defun comp-limplify-lap-inst (insn)
  "Limplify LAP instruction INSN pushng it in the proper basic block."
  (let ((op (car insn))
        (arg (if (consp (cdr insn))
                 (cadr insn)
               (cdr insn))))
    (comp-op-case
      (TAG
       (comp-emit-block (comp-lap-to-limple-bb arg)))
      (byte-stack-ref
       (comp-copy-slot (- (comp-sp) arg 1)))
      (byte-varref
       (comp-emit-set-call `(call Fsymbol_value ,(make-comp-mvar
                                                  :constant arg))))
      (byte-varset
       (comp-emit `(call set_internal
                         ,(make-comp-mvar :constant arg)
                         ,(comp-slot))))
      (byte-varbind
       (comp-emit `(call specbind
                         ,(make-comp-mvar :constant arg)
                         ,(comp-slot-next))))
      (byte-call
       (comp-stack-adjust (- arg))
       (comp-emit-set-call `(callref Ffuncall ,(1+ arg) ,(comp-sp))))
      (byte-unbind
       (comp-emit `(call unbind_to
                         ,(make-comp-mvar :constant arg)
                         ,(make-comp-mvar :constant nil))))
      (byte-pophandler
       (comp-emit '(pop-handler)))
      (byte-pushconditioncase
       (comp-emit-handler (cl-third insn) 'condition-case))
      (byte-pushcatch
       (comp-emit-handler (cl-third insn) 'catcher))
      (byte-nth auto)
      (byte-symbolp auto)
      (byte-consp auto)
      (byte-stringp auto)
      (byte-listp auto)
      (byte-eq auto)
      (byte-memq auto)
      (byte-not null Fnull)
      (byte-car auto)
      (byte-cdr auto)
      (byte-cons auto)
      (byte-list1
       (comp-limplify-listn 1))
      (byte-list2
       (comp-limplify-listn 2))
      (byte-list3
       (comp-limplify-listn 3))
      (byte-list4
       (comp-limplify-listn 4))
      (byte-length auto)
      (byte-aref auto)
      (byte-aset auto)
      (byte-symbol-value auto)
      (byte-symbol-function auto)
      (byte-set auto)
      (byte-fset auto)
      (byte-get auto)
      (byte-substring auto)
      (byte-concat2
       (comp-emit-set-call `(callref Fconcat 2 ,(comp-sp))))
      (byte-concat3
       (comp-emit-set-call `(callref Fconcat 3 ,(comp-sp))))
      (byte-concat4
       (comp-emit-set-call `(callref Fconcat 4 ,(comp-sp))))
      (byte-sub1 1- Fsub1)
      (byte-add1 1+ Fadd1)
      (byte-eqlsign = Feqlsign)
      (byte-gtr > Fgtr)
      (byte-lss < Flss)
      (byte-leq <= Fleq)
      (byte-geq >= Fgeq)
      (byte-diff - Fminus)
      (byte-negate - Fminus)
      (byte-plus + Fplus)
      (byte-max auto)
      (byte-min auto)
      (byte-mult * Ftimes)
      (byte-point auto)
      (byte-goto-char auto)
      (byte-insert auto)
      (byte-point-max auto)
      (byte-point-min auto)
      (byte-char-after auto)
      (byte-following-char auto)
      (byte-preceding-char auto)
      (byte-current-column auto)
      (byte-indent-to auto)
      (byte-scan-buffer-OBSOLETE)
      (byte-eolp auto)
      (byte-eobp auto)
      (byte-bolp auto)
      (byte-bobp auto)
      (byte-current-buffer auto)
      (byte-set-buffer auto)
      (byte-save-current-buffer)
      (byte-set-mark-OBSOLETE)
      (byte-interactive-p-OBSOLETE)
      (byte-forward-char auto)
      (byte-forward-word auto)
      (byte-skip-chars-forward auto)
      (byte-skip-chars-backward auto)
      (byte-forward-line auto)
      (byte-char-syntax auto)
      (byte-buffer-substring auto)
      (byte-delete-region auto)
      (byte-narrow-to-region)
      (byte-widen)
      (byte-end-of-line auto)
      (byte-constant2)
      (byte-goto
       (comp-emit-jump (comp-lap-to-limple-bb (cl-third insn))))
      (byte-goto-if-nil
       (comp-emit-cond-jump (comp-slot-next) (make-comp-mvar :constant nil) 0
                            (cl-third insn) nil))
      (byte-goto-if-not-nil
       (comp-emit-cond-jump (comp-slot-next) (make-comp-mvar :constant nil) 0
                            (cl-third insn) t))
      (byte-goto-if-nil-else-pop
       (comp-emit-cond-jump (comp-slot-next) (make-comp-mvar :constant nil) 1
                            (cl-third insn) nil))
      (byte-goto-if-not-nil-else-pop
       (comp-emit-cond-jump (comp-slot-next) (make-comp-mvar :constant nil) 1
                            (cl-third insn) t))
      (byte-return
       (comp-emit (list 'return (comp-slot-next)))
       (comp-mark-block-closed))
      (byte-discard 'pass)
      (byte-dup
       (comp-copy-slot (1- (comp-sp))))
      (byte-save-excursion)
      (byte-save-window-excursion-OBSOLETE)
      (byte-save-restriction)
      (byte-catch)
      (byte-unwind-protect)
      (byte-condition-case)
      (byte-temp-output-buffer-setup-OBSOLETE)
      (byte-temp-output-buffer-show-OBSOLETE)
      (byte-unbind-all)
      (byte-set-marker auto)
      (byte-match-beginning auto)
      (byte-match-end auto)
      (byte-upcase auto)
      (byte-downcase auto)
      (byte-string= string-equal Fstring_equal)
      (byte-string< string-lessp Fstring_lessp)
      (byte-equal auto)
      (byte-nthcdr auto)
      (byte-elt auto)
      (byte-member auto)
      (byte-assq auto)
      (byte-nreverse auto)
      (byte-setcar auto)
      (byte-setcdr auto)
      (byte-car-safe auto)
      (byte-cdr-safe auto)
      (byte-nconc auto)
      (byte-quo / Fquo)
      (byte-rem % Frem)
      (byte-numberp auto)
      (byte-integerp auto)
      (byte-listN
       (comp-stack-adjust (- 1 arg))
       (comp-emit-set-call `(callref Flist ,arg ,(comp-sp))))
      (byte-concatN
       (comp-stack-adjust (- 1 arg))
       (comp-emit-set-call `(callref Fconcat ,arg ,(comp-sp))))
      (byte-insertN
       (comp-stack-adjust (- 1 arg))
       (comp-emit-set-call `(callref Finsert ,arg ,(comp-sp))))
      (byte-stack-set
       (comp-with-sp (1+ (comp-sp))
         (comp-copy-slot (comp-sp) (- (comp-sp) arg))))
      (byte-stack-set2)
      (byte-discardN
       (comp-stack-adjust (- arg)))
      (byte-switch
       (comp-emit-switch (comp-slot-next) (comp-slot-n (+ 2 (comp-sp)))))
      (byte-constant
       (comp-emit-set-const arg))
      (byte-discardN-preserve-tos
       (comp-stack-adjust (- arg))
       (comp-copy-slot (+ arg (comp-sp)))))))

(defun comp-limplify (func)
  "Given FUNC compute its LIMPLE ir."
  (let* ((frame-size (comp-func-frame-size func))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :sp -1
                     :frame (comp-new-frame frame-size)))
         (comp-block ()))
    ;; Prologue
    (comp-emit-block 'entry)
    (comp-emit-annotation (concat "Lisp function: "
                                  (symbol-name (comp-func-symbol-name func))))
    (cl-loop for i below (comp-args-min (comp-func-args func))
             do (progn
                  (cl-incf (comp-sp))
                  (comp-emit `(setpar ,(comp-slot) ,i))))
    ;; Body
    (comp-emit-block 'bb_1)
    (mapc #'comp-limplify-lap-inst (comp-func-lap func))
    ;; Reverse insns into all basic blocks.
    (cl-loop for bb being the hash-value in (comp-func-blocks func)
             do (setf (comp-block-insns bb)
                      (reverse (comp-block-insns bb))))
    (when comp-debug
      (comp-pretty-print-func func))
    func))


;;; Entry points.

(declare-function comp-init-ctxt "comp.c")
(declare-function comp-release-ctxt "comp.c")
(declare-function comp-add-func-to-ctxt "comp.c")
(declare-function comp-compile-and-load-ctxt "comp.c")

(defun native-compile (func-symbol-name)
  "FUNC-SYMBOL-NAME is the function name to be compiled into native code."
  (if-let ((f (symbol-function func-symbol-name)))
      (progn
        (when (byte-code-function-p f)
          (error "Can't native compile an already bytecompiled function"))
        (let ((func (make-comp-func :symbol-name func-symbol-name
                                    :func f
                                    :c-func-name (comp-c-func-name
                                                  func-symbol-name))))
          (mapc (lambda (pass)
                  (funcall pass func))
                comp-passes)
          ;; Once we have the final LIMPLE we jump into C.
          (comp-init-ctxt)
          (comp-add-func-to-ctxt func)
          (comp-compile-and-load-ctxt)
          (comp-release-ctxt)))
    (error "Trying to native compile something not a function")))

(provide 'comp)

;;; comp.el ends here
