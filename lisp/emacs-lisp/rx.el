;;; rx.el --- S-exp notation for regexps            -*- lexical-binding: t -*-

;; Copyright (C) 2001-2026 Free Software Foundation, Inc.

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

;; This facility allows writing regexps in a sexp-based language
;; instead of strings.  Regexps in the `rx' notation are easier to
;; read, write and maintain; they can be indented and commented in a
;; natural way, and are easily composed by program code.
;; The translation to string regexp is done by a macro and does not
;; incur any extra processing during run time.  Example:
;;
;;  (rx bos (or (not "^")
;;              (seq "^" (or " *" "["))))
;;
;; => "\\`\\(?:[^^]\\|\\^\\(?: \\*\\|\\[\\)\\)"
;;
;; The notation is much influenced by and retains some compatibility with
;; Olin Shivers's SRE, with concessions to Emacs regexp peculiarities,
;; and the older Emacs package Sregex.

;;; Legacy syntax still accepted by rx:
;;
;; These are constructs from earlier rx and sregex implementations
;; that were mistakes, accidents or just not very good ideas in hindsight.

;; Obsolete: accepted but not documented
;;
;; Obsolete                     Preferred
;; --------------------------------------------------------
;; (not word-boundary)          not-word-boundary
;; (not-syntax X)               (not (syntax X))
;; not-wordchar                 (not wordchar)
;; (not-char ...)               (not (any ...))
;; any                          nonl, not-newline    -- warning since Emacs 31
;; (repeat N FORM)              (= N FORM)
;; (syntax CHARACTER)           (syntax NAME)
;; (syntax CHAR-SYM)      [1]   (syntax NAME)
;; unibyte                      ascii
;; multibyte                    nonascii
;; --------------------------------------------------------
;; [1]  where CHAR-SYM is a symbol with single-character name

;; Obsolescent: accepted and documented but discouraged
;;
;; Obsolescent                    Preferred
;; --------------------------------------------------------
;; (and ...)                      (seq ...), (: ...), (sequence ...)
;; anything                       anychar
;; minimal-match, maximal-match   lazy ops: ??, *?, +?

;; FIXME: Prepare a phase-out by emitting compile-time warnings about
;; more of the legacy constructs above.

;;; Code:


;; The `rx--translate...' functions below return (REGEXP . PRECEDENCE),
;; where REGEXP is a list of string expressions that will be
;; concatenated into a regexp, and PRECEDENCE is one of
;;
;;  t    -- can be used as argument to postfix operators (eg. "a")
;;  seq  -- can be concatenated in sequence with other seq or higher (eg. "ab")
;;  lseq -- can be concatenated to the left of rseq or higher (eg. "^a")
;;  rseq -- can be concatenated to the right of lseq or higher (eg. "a$")
;;  nil  -- can only be used in alternatives (eg. "a\\|b")
;;
;; They form a lattice:
;;
;;           t          highest precedence
;;           |
;;          seq
;;         /   \
;;      lseq   rseq
;;         \   /
;;          nil         lowest precedence


(defconst rx--char-classes
  '((digit         . digit)
    (numeric       . digit)
    (num           . digit)
    (control       . cntrl)
    (cntrl         . cntrl)
    (hex-digit     . xdigit)
    (hex           . xdigit)
    (xdigit        . xdigit)
    (blank         . blank)
    (graphic       . graph)
    (graph         . graph)
    (printing      . print)
    (print         . print)
    (alphanumeric  . alnum)
    (alnum         . alnum)
    (letter        . alpha)
    (alphabetic    . alpha)
    (alpha         . alpha)
    (ascii         . ascii)
    (nonascii      . nonascii)
    (lower         . lower)
    (lower-case    . lower)
    (punctuation   . punct)
    (punct         . punct)
    (space         . space)
    (whitespace    . space)
    (white         . space)
    (upper         . upper)
    (upper-case    . upper)
    (word          . word)
    (wordchar      . word)
    (unibyte       . unibyte)
    (multibyte     . multibyte))
  "Alist mapping rx symbols to character classes.
Most of the names are from SRE.")

(defvar rx-constituents nil
  "Alist of old-style rx extensions, for compatibility.
For new code, use `rx-define', `rx-let' or `rx-let-eval'.

Each element is (SYMBOL . DEF).

If DEF is a symbol, then SYMBOL is an alias of DEF.

If DEF is a string, then SYMBOL is a plain rx symbol defined as the
   regexp string DEF.

If DEF is a list on the form (FUN MIN-ARGS MAX-ARGS PRED), then
   SYMBOL is an rx form with at least MIN-ARGS and at most
   MAX-ARGS arguments.  If MAX-ARGS is nil, then there is no upper limit.
   FUN is a function taking the entire rx form as single argument
   and returning the translated regexp string.
   If PRED is non-nil, it is a predicate that all actual arguments must
   satisfy.")

(make-obsolete-variable
 'rx-constituents
 "use `rx-let', `rx-let-eval', or `rx-define' instead."
 ;; Effectively obsolete since Emacs 27 but only formally declared
 ;; obsolete in Emacs 30.
 "30.1")

(defvar rx--local-definitions nil
  "Alist of dynamic local rx definitions.
Each entry is:
 (NAME DEF)      -- NAME is an rx symbol defined as the rx form DEF.
 (NAME ARGS DEF) -- NAME is an rx form with arglist ARGS, defined
                    as the rx form DEF (which can contain members of ARGS).")

(defsubst rx--lookup-def (name)
  "Current definition of NAME: (DEF) or (ARGS DEF), or nil if none."
  (or (cdr (assq name rx--local-definitions))
      (get name 'rx-definition)))

(defun rx--expand-def-form (form)
  "List FORM expanded (once) if a user-defined construct; otherwise nil."
  (let ((op (car form)))
    (and (symbolp op)
         (let ((def (rx--lookup-def op)))
           (and def
                (if (cdr def)
                    (rx--expand-template op (cdr form) (nth 0 def) (nth 1 def))
                  (error "Not an `rx' form definition: %s" op)))))))

(defun rx--expand-def-symbol (symbol)
  "SYM expanded (once) if a user-defined name; otherwise nil."
  (let ((def (rx--lookup-def symbol)))
    (and def
         (if (cdr def)
             (error "Not an `rx' symbol definition: %s" symbol)
           (car def)))))

(defun rx--translate-symbol (sym)
  "Translate an rx symbol.  Return (REGEXP . PRECEDENCE)."
  (pcase sym
    ;; Use `list' instead of a quoted list to wrap the strings here,
    ;; since the return value may be mutated.
    ((or 'nonl 'not-newline)      (cons (list ".") t))
    ((or 'anychar 'anything)      (cons (list "[^z-a]") t))
    ('unmatchable                 (rx--empty))
    ((or 'bol 'line-start)        (cons (list "^") 'lseq))
    ((or 'eol 'line-end)          (cons (list "$") 'rseq))
    ((or 'bos 'string-start 'bot 'buffer-start) (cons (list "\\`") t))
    ((or 'eos 'string-end   'eot 'buffer-end)   (cons (list "\\'") t))
    ('point                       (cons (list "\\=") t))
    ((or 'bow 'word-start)        (cons (list "\\<") t))
    ((or 'eow 'word-end)          (cons (list "\\>") t))
    ('word-boundary               (cons (list "\\b") t))
    ('not-word-boundary           (cons (list "\\B") t))
    ('symbol-start                (cons (list "\\_<") t))
    ('symbol-end                  (cons (list "\\_>") t))
    ('not-wordchar                (rx--translate '(not wordchar)))
    ('any
     (when (and (macroexp-compiling-p)
                (byte-compile-warning-enabled-p 'obsolete 'any))
       (byte-compile-warn-x
        sym (concat "`any' in rx is obsolete and means `not-newline';"
                    " did you mean `anychar'?")))
     (rx--translate-symbol 'nonl))
    (_
     (cond
      ((let ((class (cdr (assq sym rx--char-classes))))
         (and class (cons (list (concat "[[:" (symbol-name class) ":]]")) t))))

      ((let ((expanded (rx--expand-def-symbol sym)))
         (and expanded (rx--translate expanded))))

      ;; For compatibility with old rx.
      ((let ((entry (assq sym rx-constituents)))
         (and entry (rx--translate-compat-symbol-entry entry))))

      (t (error "Unknown rx symbol `%s'" sym))))))

(defun rx--enclose (left-str rexp right-str)
  "Bracket REXP by LEFT-STR and RIGHT-STR."
  (append (list left-str) rexp (list right-str)))

(defun rx--bracket (rexp)
  (rx--enclose "\\(?:" rexp "\\)"))

(defun rx--sequence (left right)
  "Return the sequence (concatenation) of two translated items,
each on the form (REGEXP . PRECEDENCE), returning (REGEXP . PRECEDENCE)."
  ;; Concatenation rules:
  ;;  seq  ++ seq  -> seq
  ;;  lseq ++ seq  -> lseq
  ;;  seq  ++ rseq -> rseq
  ;;  lseq ++ rseq -> nil
  (cond ((not (car left)) right)
        ((not (car right)) left)
        (t
         (let ((l (if (memq (cdr left) '(nil rseq))
                      (cons (rx--bracket (car left)) t)
                    left))
               (r (if (memq (cdr right) '(nil lseq))
                      (cons (rx--bracket (car right)) t)
                    right)))
           (cons (append (car l) (car r))
                 (if (eq (cdr l) 'lseq)
                     (if (eq (cdr r) 'rseq)
                         nil                   ; lseq ++ rseq
                       'lseq)                  ; lseq ++ seq
                   (if (eq (cdr r) 'rseq)
                       'rseq                   ; seq ++ rseq
                     'seq)))))))               ; seq ++ seq

(defun rx--translate-seq (body)
  "Translate a sequence of zero or more rx items.
Return (REGEXP . PRECEDENCE)."
  (if body
      (let* ((items (mapcar #'rx--translate body))
             (result (car items)))
        (dolist (item (cdr items))
          (setq result (rx--sequence result item)))
        result)
    (cons nil 'seq)))

(defun rx--empty ()
  "Regexp that never matches anything."
  (cons (list regexp-unmatchable) 'seq))

(defun rx--foldl (f x l)
  "(F (F (F X L0) L1) L2) ...
Left-fold the list L, starting with X, by the binary function F."
  (while l
    (setq x (funcall f x (car l)))
    (setq l (cdr l)))
  x)

;; FIXME: flatten nested `or' patterns when performing char-pattern combining.
;; The only reason for not flattening is to ensure regexp-opt processing
;; (which we do for entire `or' patterns, not subsequences), but we
;; obviously want to translate
;;   (or "a" space (or "b" (+ nonl) word) "c")
;;   -> (or (in "ab" space) (+ nonl) (in "c" word))

;; FIXME: normalize `seq', both the construct and implicit sequences,
;; so that they are flattened, adjacent strings concatenated, and
;; empty strings removed. That would give more opportunities for regexp-opt:
;;  (or "a" (seq "ab" (seq "c" "d") "")) -> (or "a" "abcd")

;; FIXME: Since `rx--normalize-char-pattern' recurses through `or', `not' and
;; `intersection', we may end up normalising subtrees multiple times
;; which wastes time (but should be idempotent).
;; One way to avoid this is to aggressively normalize the entire tree
;; before translating anything at all, but we must then recurse through
;; all constructs and probably copy them.
;; Such normalization could normalize synonyms, eliminate `minimal-match'
;; and `maximal-match' and convert affected `1+' to either `+' or `+?' etc.
;; We would also consolidate the user-def lookup, both modern and legacy,
;; in one place.

(defun rx--normalize-char-pattern (form)
  "Normalize FORM as a pattern matching a single-character.
Characters become strings, `any' forms and character classes become
`rx--char-alt' forms, user-definitions and `eval' forms are expanded,
and `or', `not' and `intersection' forms are normalized recursively.

A `rx--char-alt' form is shaped (rx--char-alt INTERVALS . CLASSES)
where INTERVALS is a sorted list of disjoint nonadjacent intervals,
each a cons of characters, and CLASSES an unordered list of unique
name-normalized character classes."
  (defvar rx--builtin-forms)
  (defvar rx--builtin-symbols)
  (cond ((consp form)
         (let ((op (car form))
               (body (cdr form)))
           (cond ((memq op '(or |))
                  ;; Normalize the constructor to `or' and the args recursively.
                  (cons 'or (mapcar #'rx--normalize-char-pattern body)))
                 ;; Convert `any' forms and char classes now so that we
                 ;; don't need to do it later on.
                 ((memq op '(any in char))
                  (cons 'rx--char-alt (rx--parse-any body)))
                 ((memq op '(not intersection))
                  (cons op (mapcar #'rx--normalize-char-pattern body)))
                 ((eq op 'eval)
                  (rx--normalize-char-pattern (rx--expand-eval body)))
                 ((memq op rx--builtin-forms) form)
                 ((let ((expanded (rx--expand-def-form form)))
                    (and expanded
                         (rx--normalize-char-pattern expanded))))
                 (t form))))
        ;; FIXME: Should we expand legacy definitions from
        ;; `rx-constituents' here as well?
        ((symbolp form)
         (cond ((let ((class (assq form rx--char-classes)))
                  (and class
                       `(rx--char-alt nil . (,(cdr class))))))
               ((memq form rx--builtin-symbols) form)
               ((let ((expanded (rx--expand-def-symbol form)))
                  (and expanded
                       (rx--normalize-char-pattern expanded))))
               (t form)))
        ((characterp form)
         (char-to-string form))
        (t form)))

(defun rx--char-alt-union (a b)
  "Union of the (INTERVALS . CLASSES) pairs A and B."
  (let* ((a-cl (cdr a))
         (b-cl (cdr b))
         (classes (if (and a-cl b-cl)
                      (let ((acc a-cl))
                        (dolist (c b-cl)
                          (unless (memq c a-cl)
                            (push c acc)))
                        acc)
                    (or a-cl b-cl))))
    (cons (rx--interval-set-union (car a) (car b)) classes)))

(defun rx--intersection-intervals (forms)
  "Intersection of the normalized FORMS, as an interval set."
  (rx--foldl #'rx--interval-set-intersection '((0 . #x3fffff))
             (mapcar (lambda (x)
                       (let ((char (rx--reduce-to-char-alt x)))
                         (if (and char (null (cdr char)))
                             (car char)
                           (error "Cannot be used in rx intersection: %S"
                                  (rx--human-readable x)))))
                     forms)))

(defun rx--reduce-to-char-alt (form)
  "Transform FORM into (INTERVALS . CLASSES) or nil if not possible.
Process `or', `intersection' and `not'.
FORM must be normalized (from `rx--normalize-char-pattern')."
  (cond
   ((stringp form)
    (and (= (length form) 1)
         (let ((c (aref form 0)))
           (list (list (cons c c))))))
   ((consp form)
    (let ((head (car form)))
      (cond
       ;; FIXME: Transform `digit', `xdigit', `cntrl', `ascii', `nonascii'
       ;; to ranges? That would allow them to be negated and intersected.
       ((eq head 'rx--char-alt) (cdr form))
       ((eq head 'not)
        (unless (= (length form) 2)
          (error "rx `not' form takes exactly one argument"))
        (let ((arg (rx--reduce-to-char-alt (cadr form))))
          ;; Only interval sets without classes are closed under complement.
          (and arg (null (cdr arg))
               (list (rx--interval-set-complement (car arg))))))
       ((eq head 'or)
        (let ((args (cdr form)))
          (let ((acc '(nil)))  ; union identity
            (while (and args
                        (let ((char (rx--reduce-to-char-alt (car args))))
                          (setq acc (and char (rx--char-alt-union acc char)))))
              (setq args (cdr args)))
            acc)))
       ((eq head 'intersection)
        (list (rx--intersection-intervals (cdr form))))
       )))
   ((memq form '(nonl not-newline any))
    '(((0 . 9) (11 . #x3fffff))))
   ((memq form '(anychar anything))
    '(((0 . #x3fffff))))
   ;; FIXME: A better handling of `unmatchable' would be:
   ;;   * (seq ... unmatchable ...) -> unmatchable
   ;;   * any or-pattern branch that is `unmatchable' is deleted
   ;;   * (REPEAT unmatchable) -> "", if REPEAT accepts 0 repetitions
   ;;   * (REPEAT unmatchable) -> unmatchable, otherwise
   ;; if it's worth the trouble (probably not).
   ((eq form 'unmatchable)
    '(nil))
   ))

(defun rx--optimize-or-args (args)
  "Optimize `or' arguments.  Return a new rx form.
Each element of ARGS should have been normalized using
`rx--normalize-char-pattern'."
  (if (null args)
      ;; No arguments.
      '(rx--char-alt nil . nil)         ; FIXME: not `unmatchable'?
    ;; Join consecutive single-char branches into a char alt where possible.
    ;; Ideally we should collect all single-char branches but that might
    ;; alter matching order in some cases.
    (let ((branches nil)
          (prev-char nil))
      (while args
        (let* ((item (car args))
               (item-char (rx--reduce-to-char-alt item)))
          (if item-char
              (setq prev-char (if prev-char
                                  (rx--char-alt-union prev-char item-char)
                                item-char))
            (when prev-char
              (push (cons 'rx--char-alt prev-char) branches)
              (setq prev-char nil))
            (push item branches)))
        (setq args (cdr args)))
      (when prev-char
        (push (cons 'rx--char-alt prev-char) branches))
      (if (cdr branches)
          (cons 'or (nreverse branches))
        (car branches)))))

(defun rx--all-string-branches-p (forms)
  "Whether FORMS are all strings or `or' forms with the same property."
  (all (lambda (x) (or (stringp x)
                       (and (eq (car-safe x) 'or)
                            (rx--all-string-branches-p (cdr x)))))
       forms))

(defun rx--collect-or-strings (forms)
  "All strings from FORMS, which are strings or `or' forms."
  (mapcan (lambda (form)
            (if (stringp form)
                (list form)
              ;; must be an `or' form
              (rx--collect-or-strings (cdr form))))
          forms))

;; TODO: Write a more general rx-level factoriser to replace
;; `regexp-opt' for our purposes.  It would handle non-literals:
;;
;;    (or "ab" (: "a" space) "bc" (: "b" (+ digit)))
;; -> (or (: "a" (in "b" space)) (: "b" (or "c" (+ digit))))
;;
;; As a minor side benefit we would get less useless bracketing.
;; The main problem is how to deal with matching order, which `regexp-opt'
;; alters in its own way.

(defun rx--translate-or (body)
  "Translate an or-pattern of zero or more rx items.
Return (REGEXP . PRECEDENCE)."
  (cond
   ((null body)                    ; No items: a never-matching regexp.
    (rx--empty))
   ((null (cdr body))              ; Single item.
    (rx--translate (car body)))
   (t
    (let ((args (mapcar #'rx--normalize-char-pattern body)))
      (if (rx--all-string-branches-p args)
          ;; All branches are strings: use `regexp-opt'.
          (cons (list (regexp-opt (rx--collect-or-strings args) nil))
                t)
        (let ((form (rx--optimize-or-args args)))
          (if (eq (car-safe form) 'or)
              (let ((branches (cdr form)))
                (cons (append (car (rx--translate (car branches)))
                              (mapcan (lambda (item)
                                        (cons "\\|" (car (rx--translate item))))
                                      (cdr branches)))
                      nil))
            (rx--translate form))))))))

(defun rx--string-to-intervals (str)
  "Decode STR as intervals: A-Z becomes (?A . ?Z), and the single
character X becomes (?X . ?X).  Return the intervals in a list."
  ;; We could just do string-to-multibyte on the string and work with
  ;; that instead of this `decode-char' workaround.
  (let ((decode-char
         (if (multibyte-string-p str)
             #'identity
           #'unibyte-char-to-multibyte))
        (len (length str))
        (i 0)
        (intervals nil))
    (while (< i len)
      (cond ((and (< i (- len 2))
                  (= (aref str (1+ i)) ?-))
             ;; Range.
             (let ((start (funcall decode-char (aref str i)))
                   (end   (funcall decode-char (aref str (+ i 2)))))
               (cond ((and (<= start #x7f) (>= end #x3fff80))
                      ;; Ranges between ASCII and raw bytes are split to
                      ;; avoid having them absorb Unicode characters
                      ;; caught in-between.
                      (push (cons start #x7f) intervals)
                      (push (cons #x3fff80 end) intervals))
                     ((<= start end)
                      (push (cons start end) intervals))
                     (t
                      (error "Invalid rx `any' range: %s"
                             (substring str i (+ i 3)))))
               (setq i (+ i 3))))
            (t
             ;; Single character.
             (let ((char (funcall decode-char (aref str i))))
               (push (cons char char) intervals))
             (setq i (+ i 1)))))
    intervals))

(defun rx--condense-intervals (intervals)
  "Merge adjacent and overlapping intervals by mutation, preserving the order.
INTERVALS is a list of (START . END) with START ≤ END, sorted by START."
  (let ((tail intervals)
        d)
    (while (setq d (cdr tail))
      (if (>= (cdar tail) (1- (caar d)))
          (progn
            (setcdr (car tail) (max (cdar tail) (cdar d)))
            (setcdr tail (cdr d)))
        (setq tail d)))
    intervals))

(defun rx--parse-any (body)
  "Parse arguments of an (any ...) construct.
Return (INTERVALS . CLASSES), where INTERVALS is a sorted list of
disjoint nonadjacent intervals (each a cons of chars), and CLASSES
a list of named character classes in the order they occur in BODY."
  (let ((classes nil)
        (strings nil)
        (conses nil))
    ;; Collect strings, conses and characters, and classes in separate bins.
    (dolist (arg body)
      (cond ((stringp arg)
             (push arg strings))
            ((and (consp arg)
                  (characterp (car arg))
                  (characterp (cdr arg))
                  (<= (car arg) (cdr arg)))
             ;; Copy the cons, in case we need to modify it.
             (push (cons (car arg) (cdr arg)) conses))
            ((characterp arg)
             (push (cons arg arg) conses))
            ((and (symbolp arg)
                  (let ((class (cdr (assq arg rx--char-classes))))
                    (and class
                         (or (memq class classes)
                             (progn (push class classes) t))))))
            (t (error "Invalid rx `any' argument: %s" arg))))
    (cons (rx--condense-intervals
           (sort (append conses
                         (mapcan #'rx--string-to-intervals strings))
                 :key #'car :in-place t))
          (nreverse classes))))

(defun rx--generate-alt (negated intervals classes)
  "Generate a character alternative.  Return (REGEXP . PRECEDENCE).
If NEGATED is non-nil, negate the result; INTERVALS is a sorted
list of disjoint intervals and CLASSES a list of named character
classes."
  ;; No, this is not pretty code.  You try doing it in a way that is both
  ;; elegant and efficient.  Or just one of the two.  I dare you.

  ;; Detect whether the interval set is better described in
  ;; complemented form.  This is not just a matter of aesthetics: any
  ;; range that straddles the char-raw boundary will be mutilated by the
  ;; regexp engine.  Ranges from ASCII to raw bytes will exclude the
  ;; all non-ASCII non-raw bytes, and ranges from non-ASCII Unicode
  ;; to raw bytes are ignored.
  (when (and (not classes)
             ;; Any interval set covering #x3fff7f should be negated.
             (any (lambda (iv) (<= (car iv) #x3fff7f (cdr iv)))
                  intervals))
    (setq negated (not negated))
    (setq intervals (rx--interval-set-complement intervals)))
  (cond
   ;; Single character.
   ((and intervals (eq (caar intervals) (cdar intervals))
         (null (cdr intervals))
         (null classes))
    (let ((ch (caar intervals)))
      (if negated
          (if (eq ch ?\n)
              ;; Single negated newline.
              (rx--translate-symbol 'nonl)
            ;; Single negated character (other than newline).
            (cons (list (string ?\[ ?^ ch ?\])) t))
        ;; Single literal character.
        (cons (list (regexp-quote (char-to-string ch))) t))))

   ;; Empty set (or any char).
   ((and (null intervals) (null classes))
    (if negated
        (rx--translate-symbol 'anychar)
      (rx--empty)))

   ;; More than one character, or at least one class.
   (t
    (let ((dash nil) (caret nil))
      ;; Move ] and range ]-x to the start.
      (let ((rbrac-l (assq ?\] intervals)))
        (when rbrac-l
          (setq intervals (cons rbrac-l (remq rbrac-l intervals)))))

      ;; Split x-] and move the lone ] to the start.
      (let ((rbrac-r (rassq ?\] intervals)))
        (when (and rbrac-r (not (eq (car rbrac-r) ?\])))
          (setcdr rbrac-r ?\\)
          (setq intervals (cons '(?\] . ?\]) intervals))))

      ;; Split ,-- (which would end up as ,- otherwise).
      (let ((dash-r (rassq ?- intervals)))
        (when (eq (car dash-r) ?,)
          (setcdr dash-r ?,)
          (setq dash "-")))

      ;; Remove - (lone or at start of interval)
      (let ((dash-l (assq ?- intervals)))
        (when dash-l
          (if (eq (cdr dash-l) ?-)
              (setq intervals (remq dash-l intervals))   ; Remove lone -
            (setcar dash-l ?.))                          ; Reduce --x to .-x
          (setq dash "-")))

      ;; Deal with leading ^ and range ^-x in non-negated set.
      (when (and (eq (caar intervals) ?^)
                 (not negated))
        (if (eq (cdar intervals) ?^)
            ;; single leading ^
            (if (or (cdr intervals) classes)
                ;; something else to put before the ^
                (progn
                  (setq intervals (cdr intervals))   ; remove lone ^
                  (setq caret "^"))                  ; put ^ (almost) last
              ;; nothing else but a lone -
              (setq intervals (cons '(?- . ?-) intervals))  ; move - first
              (setq dash nil))
          ;; split ^-x to _-x^
          (setq intervals `((?_ . ,(cdar intervals)) (?^ . ?^)
                            . ,(cdr intervals)))))

      (cons
       (list
        (concat
         "["
         (and negated "^")
         (mapconcat (lambda (iv)
                      (cond ((eq (car iv) (cdr iv))
                             (char-to-string (car iv)))
                            ((eq (1+ (car iv)) (cdr iv))
                             (string (car iv) (cdr iv)))
                            ;; Ranges that go between normal chars and raw bytes
                            ;; must be split to avoid being mutilated
                            ;; by Emacs's regexp parser.
                            ((<= (car iv) #x3fff7f (cdr iv))
                             (string (car iv) ?- #x3fff7f
                                     #x3fff80 ?- (cdr iv)))
                            (t
                             (string (car iv) ?- (cdr iv)))))
                    intervals)
         (mapconcat (lambda (cls) (format "[:%s:]" cls)) classes)
         caret                          ; ^ or nothing
         dash                           ; - or nothing
         "]"))
       t)))))

(defun rx--translate-char-alt (negated body)
  "Translate a (rx--char-alt ...) construct.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense."
  (rx--generate-alt negated (car body) (cdr body)))

(defun rx--translate-any (negated body)
  "Translate an (any ...) construct.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense."
  (let ((parsed (rx--parse-any body)))
    (rx--generate-alt negated (car parsed) (cdr parsed))))

;; TODO: Consider turning `not' into a variadic operator, following SRE:
;; (not A B) = (not (or A B)) = (intersection (not A) (not B)), and
;; (not) = anychar.
;; Maybe allow singleton characters as arguments.

(defun rx--translate-not (negated body)
  "Translate a (not ...) construct.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense (thus making it positive)."
  (unless (and body (null (cdr body)))
    (error "rx `not' form takes exactly one argument"))
  (let ((arg (rx--normalize-char-pattern (car body))))
    (pcase arg
      (`(not . ,args)
       (rx--translate-not      (not negated) args))
      (`(syntax . ,args)
       (rx--translate-syntax   (not negated) args))
      (`(category . ,args)
       (rx--translate-category (not negated) args))
      ('word-boundary                     ; legacy syntax
       (rx--translate-symbol (if negated 'word-boundary 'not-word-boundary)))
      (_ (let ((char (rx--reduce-to-char-alt arg)))
           (if char
               (rx--generate-alt (not negated) (car char) (cdr char))
             (error "Illegal argument to rx `not': %S"
                    (rx--human-readable arg))))))))

(defun rx--interval-set-complement (ivs)
  "Complement of the interval set IVS."
  (let ((compl nil)
        (c 0))
    (dolist (iv ivs)
      (when (< c (car iv))
        (push (cons c (1- (car iv))) compl))
      (setq c (1+ (cdr iv))))
    (when (< c (max-char))
      (push (cons c (max-char)) compl))
    (nreverse compl)))

(defun rx--interval-set-intersection (ivs-a ivs-b)
  "Intersection of the interval sets IVS-A and IVS-B."
  (let ((isect nil))
    (while (and ivs-a ivs-b)
      (let ((a (car ivs-a))
            (b (car ivs-b)))
        (cond
         ((< (cdr a) (car b)) (setq ivs-a (cdr ivs-a)))
         ((> (car a) (cdr b)) (setq ivs-b (cdr ivs-b)))
         (t
          (push (cons (max (car a) (car b))
                      (min (cdr a) (cdr b)))
                isect)
          (setq ivs-a (cdr ivs-a))
          (setq ivs-b (cdr ivs-b))
          (cond ((< (cdr a) (cdr b))
                 (push (cons (1+ (cdr a)) (cdr b))
                       ivs-b))
                ((> (cdr a) (cdr b))
                 (push (cons (1+ (cdr b)) (cdr a))
                       ivs-a)))))))
    (nreverse isect)))

(defun rx--interval-set-union (ivs-a ivs-b)
  "Union of the interval sets IVS-A and IVS-B."
  (let ((union nil))
    (while (and ivs-a ivs-b)
      (let ((a (car ivs-a))
            (b (car ivs-b)))
        (cond
         ((< (1+ (cdr a)) (car b))      ; a before b, not adacent
          (push a union)
          (setq ivs-a (cdr ivs-a)))
         ((< (1+ (cdr b)) (car a))      ; b before a, not adacent
          (push b union)
          (setq ivs-b (cdr ivs-b)))
         (t                             ; a and b adjacent or overlap
          (setq ivs-a (cdr ivs-a))
          (setq ivs-b (cdr ivs-b))
          (if (< (cdr a) (cdr b))
              (push (cons (min (car a) (car b))
                          (cdr b))
                    ivs-b)
            (push (cons (min (car a) (car b))
                        (cdr a))
                  ivs-a))))))
    (nconc (nreverse union) (or ivs-a ivs-b))))

(defun rx--human-readable (form)
  "Turn FORM into something that is more human-readable, for error messages."
  ;; FIXME: Should we produce a string instead?
  ;; That way we wouldn't have problems with ? and ??, and we could escape
  ;; single chars.
  ;; We could steal `xr--rx-to-string' and just file off the serials.
  (let ((recurse (lambda (op skip)
                   (cons op (append (take skip (cdr form))
                                    (mapcar #'rx--human-readable
                                            (nthcdr skip (cdr form))))))))
  (pcase form
    ;; strings are more readable than numbers for single chars
    ((pred characterp) (char-to-string form))
    ;; resugar `rx--char-alt'
    (`(rx--char-alt ((,c . ,c)) . nil)
     (char-to-string form))
    (`(rx--char-alt nil . (,class))
     class)
    ;; TODO: render in complemented form if more readable that way?
    (`(rx--char-alt ,ivs . ,classes)
     (let ((strings (mapcan (lambda (iv)
                              (let ((beg (car iv))
                                    (end (cdr iv)))
                                (cond
                                 ;; single char
                                 ((eq beg end)
                                  (list (string beg)))
                                 ;; two chars
                                 ((eq end (1+ beg))
                                  (list (string beg) (string end)))
                                 ;; first char is hyphen
                                 ((eq beg ?-)
                                  (cons (string "-")
                                        (if (eq end (+ ?- 2))
                                            (list (string (1+ ?-) end))
                                          (list (string (1+ ?-) ?- end)))))
                                 ;; other range
                                 (t (list (string beg ?- end))))))
                            ivs)))
       `(any ,@strings ,@classes)))
    ;; avoid numbers as ops
    (`(?  . ,_) (funcall recurse '\? 0))
    (`(??  . ,_) (funcall recurse '\?? 0))
    ;; recurse on arguments
    (`(repeat ,_ ,_) (funcall recurse (car form) 1))
    (`(,(or '** 'repeat) . ,_) (funcall recurse (car form) 2))
    (`(,(or '= '>= 'group-n 'submatch-n) . ,_) (funcall recurse (car form) 1))
    (`(,(or 'backref 'syntax 'not-syntax 'category
            'eval 'regex 'regexp 'literal)
       . ,_)
     form)
    (`(,_ . ,_) (funcall recurse (car form) 0))
    (_ form))))

(defun rx--translate-intersection (negated body)
  "Translate an (intersection ...) construct.  Return (REGEXP . PRECEDENCE).
If NEGATED, negate the sense."
  (rx--generate-alt negated (rx--intersection-intervals
                             (mapcar #'rx--normalize-char-pattern body))
                    nil))

(defun rx--atomic-regexp (item)
  "ITEM is (REGEXP . PRECEDENCE); return a regexp of precedence t."
  (if (eq (cdr item) t)
      (car item)
    (rx--bracket (car item))))

(defun rx--translate-counted-repetition (min-count max-count body)
  (let ((operand (rx--translate-seq body)))
    (if (car operand)
        (cons (append
               (rx--atomic-regexp operand)
               (list (concat "\\{"
                             (number-to-string min-count)
                             (cond ((null max-count) ",")
                                   ((< min-count max-count)
                                    (concat "," (number-to-string max-count))))
                             "\\}")))
              t)
      operand)))

(defun rx--check-repeat-arg (name min-args body)
  (unless (>= (length body) min-args)
    (error "rx `%s' requires at least %d argument%s"
           name min-args (if (= min-args 1) "" "s")))
  ;; There seems to be no reason to disallow zero counts.
  (unless (natnump (car body))
    (error "rx `%s' first argument must be nonnegative" name)))

(defun rx--translate-bounded-repetition (name body)
  (let ((min-count (car body))
        (max-count (cadr body))
        (items (cddr body)))
    (unless (and (natnump min-count)
                 (natnump max-count)
                 (<= min-count max-count))
      (error "rx `%s' range error" name))
    (rx--translate-counted-repetition min-count max-count items)))

(defun rx--translate-repeat (body)
  (rx--check-repeat-arg 'repeat 2 body)
  (if (= (length body) 2)
      (rx--translate-counted-repetition (car body) (car body) (cdr body))
    (rx--translate-bounded-repetition 'repeat body)))

(defun rx--translate-** (body)
  (rx--check-repeat-arg '** 2 body)
  (rx--translate-bounded-repetition '** body))

(defun rx--translate->= (body)
  (rx--check-repeat-arg '>= 1 body)
  (rx--translate-counted-repetition (car body) nil (cdr body)))

(defun rx--translate-= (body)
  (rx--check-repeat-arg '= 1 body)
  (rx--translate-counted-repetition (car body) (car body) (cdr body)))

(defvar rx--greedy t)

(defun rx--translate-rep (op-string greedy body)
  "Translate a repetition; OP-STRING is one of \"*\", \"+\" or \"?\".
GREEDY is a boolean.  Return (REGEXP . PRECEDENCE)."
  (let ((operand (rx--translate-seq body)))
    (if (car operand)
        (cons (append (rx--atomic-regexp operand)
                      (list (concat op-string (unless greedy "?"))))
              ;; The result has precedence seq to avoid (? (* "a")) -> "a*?"
              'seq)
      operand)))

(defun rx--control-greedy (greedy body)
  "Translate the sequence BODY with greediness GREEDY.
Return (REGEXP . PRECEDENCE)."
  (let ((rx--greedy greedy))
    (rx--translate-seq body)))

(defun rx--translate-group (body)
  "Translate the `group' form.  Return (REGEXP . PRECEDENCE)."
  (cons (rx--enclose "\\("
                     (car (rx--translate-seq body))
                     "\\)")
        t))

(defun rx--translate-group-n (body)
  "Translate the `group-n' form.  Return (REGEXP . PRECEDENCE)."
  (unless (and (integerp (car body)) (> (car body) 0))
    (error "rx `group-n' requires a positive number as first argument"))
  (cons (rx--enclose (concat "\\(?" (number-to-string (car body)) ":")
                     (car (rx--translate-seq (cdr body)))
                     "\\)")
        t))

(defun rx--translate-backref (body)
  "Translate the `backref' form.  Return (REGEXP . PRECEDENCE)."
  (unless (and (= (length body) 1) (integerp (car body)) (<= 1 (car body) 9))
    (error "rx `backref' requires an argument in the range 1..9"))
  (cons (list "\\" (number-to-string (car body))) t))

(defconst rx--syntax-codes
  '((whitespace         . ?-)           ; SPC also accepted
    (punctuation        . ?.)
    (word               . ?w)           ; W also accepted
    (symbol             . ?_)
    (open-parenthesis   . ?\()
    (close-parenthesis  . ?\))
    (expression-prefix  . ?\')
    (string-quote       . ?\")
    (paired-delimiter   . ?$)
    (escape             . ?\\)
    (character-quote    . ?/)
    (comment-start      . ?<)
    (comment-end        . ?>)
    (string-delimiter   . ?|)
    (comment-delimiter  . ?!)))

(defun rx--translate-syntax (negated body)
  "Translate the `syntax' form.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "rx `syntax' form takes exactly one argument"))
  (let* ((sym (car body))
         (syntax (cdr (assq sym rx--syntax-codes))))
    (unless syntax
      (cond
       ;; Syntax character directly (sregex compatibility)
       ((and (characterp sym) (rassq sym rx--syntax-codes))
        (setq syntax sym))
       ;; Syntax character as symbol (sregex compatibility)
       ((symbolp sym)
        (let ((name (symbol-name sym)))
          (when (= (length name) 1)
            (let ((char (string-to-char name)))
              (when (rassq char rx--syntax-codes)
                (setq syntax char)))))))
      (unless syntax
        (error "Unknown rx syntax name `%s'" sym)))
    ;; Produce \w and \W instead of \sw and \Sw, for smaller size.
    (cons (list (if (eq syntax ?w)
                    (string ?\\ (if negated ?W ?w))
                  (string ?\\ (if negated ?S ?s) syntax)))
          t)))

(defconst rx--categories
  '((space-for-indent           . ?\s)
    (base                       . ?.)
    (consonant                  . ?0)
    (base-vowel                 . ?1)
    (upper-diacritical-mark     . ?2)
    (lower-diacritical-mark     . ?3)
    (tone-mark                  . ?4)
    (symbol                     . ?5)
    (digit                      . ?6)
    (vowel-modifying-diacritical-mark . ?7)
    (vowel-sign                 . ?8)
    (semivowel-lower            . ?9)
    (not-at-end-of-line         . ?<)
    (not-at-beginning-of-line   . ?>)
    (alpha-numeric-two-byte     . ?A)
    (chinese-two-byte           . ?C)
    (greek-two-byte             . ?G)
    (japanese-hiragana-two-byte . ?H)
    (indian-two-byte            . ?I)
    (japanese-katakana-two-byte . ?K)
    (strong-left-to-right       . ?L)
    (korean-hangul-two-byte     . ?N)
    (strong-right-to-left       . ?R)
    (cyrillic-two-byte          . ?Y)
    (combining-diacritic        . ?^)
    (ascii                      . ?a)
    (arabic                     . ?b)
    (chinese                    . ?c)
    (ethiopic                   . ?e)
    (greek                      . ?g)
    (korean                     . ?h)
    (indian                     . ?i)
    (japanese                   . ?j)
    (japanese-katakana          . ?k)
    (latin                      . ?l)
    (lao                        . ?o)
    (tibetan                    . ?q)
    (japanese-roman             . ?r)
    (thai                       . ?t)
    (vietnamese                 . ?v)
    (hebrew                     . ?w)
    (cyrillic                   . ?y)
    (can-break                  . ?|)))

(defun rx--translate-category (negated body)
  "Translate the `category' form.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "rx `category' form takes exactly one argument"))
  (let* ((arg (car body))
         (category
          (cond ((symbolp arg)
                 (let ((cat (assq arg rx--categories)))
                   (unless cat
                     (error "Unknown rx category `%s'" arg))
                   (cdr cat)))
                ((characterp arg) arg)
                (t (error "Invalid rx `category' argument `%s'" arg)))))
    (cons (list (string ?\\ (if negated ?C ?c) category))
          t)))

(defvar rx--delayed-evaluation nil
  "Whether to allow certain forms to be evaluated at runtime.")

(defun rx--translate-literal (body)
  "Translate the `literal' form.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "rx `literal' form takes exactly one argument"))
  (let ((arg (car body)))
    (cond ((stringp arg)
           (cons (list (regexp-quote arg)) (if (= (length arg) 1) t 'seq)))
          (rx--delayed-evaluation
           (cons (list (list 'regexp-quote arg)) 'seq))
          (t (error "rx `literal' form with non-string argument")))))

(defun rx--expand-eval (body)
  "Expand `eval' arguments.  Return a new rx form."
  (unless (and body (null (cdr body)))
    (error "rx `eval' form takes exactly one argument"))
  (eval (car body) lexical-binding))

(defun rx--translate-eval (body)
  "Translate the `eval' form.  Return (REGEXP . PRECEDENCE)."
  (rx--translate (rx--expand-eval body)))

(defvar rx--regexp-atomic-regexp nil)

(defun rx--translate-regexp (body)
  "Translate the `regexp' form.  Return (REGEXP . PRECEDENCE)."
  (unless (and body (null (cdr body)))
    (error "rx `regexp' form takes exactly one argument"))
  (let ((arg (car body)))
    (cond ((stringp arg)
           ;; Generate the regexp when needed, since rx isn't
           ;; necessarily present in the byte-compilation environment.
           (unless rx--regexp-atomic-regexp
             (setq rx--regexp-atomic-regexp
                   ;; Match atomic (precedence t) regexps: may give
                   ;; false negatives but no false positives, assuming
                   ;; the target string is syntactically correct.
                   (rx-to-string
                    '(seq
                      bos
                      (or (seq "["
                               (opt "^")
                               (opt "]")
                               (* (or (seq "[:" (+ (any "a-z")) ":]")
                                      (not "]")))
                               "]")
                          (not (any "*+?^$[\\"))
                          (seq "\\"
                               (or anychar
                                   (seq (any "sScC_") anychar)
                                   (seq "("
                                        (* (or (not "\\")
                                               (seq "\\" (not ")"))))
                                        "\\)"))))
                      eos)
                    t)))
           (cons (list arg)
                 (if (string-match-p rx--regexp-atomic-regexp arg) t nil)))
          (rx--delayed-evaluation
           (cons (list arg) nil))
          (t (error "rx `regexp' form with non-string argument")))))

(defun rx--translate-compat-form (def form)
  "Translate a compatibility form from `rx-constituents'.
DEF is the definition tuple.  Return (REGEXP . PRECEDENCE)."
  (let* ((fn (nth 0 def))
         (min-args (nth 1 def))
         (max-args (nth 2 def))
         (predicate (nth 3 def))
         (nargs (1- (length form))))
    (when (< nargs min-args)
      (error "The `%s' form takes at least %d argument(s)"
             (car form) min-args))
    (when (and max-args (> nargs max-args))
      (error "The `%s' form takes at most %d argument(s)"
             (car form) max-args))
    (when (and predicate (not (all predicate (cdr form))))
      (error "The `%s' form requires arguments satisfying `%s'"
             (car form) predicate))
    (let ((regexp (funcall fn form)))
      (unless (stringp regexp)
        (error "The `%s' form did not expand to a string" (car form)))
      (cons (list regexp) nil))))

(defun rx--translate-compat-symbol-entry (entry)
  "Translate a compatibility symbol definition for ENTRY.
Return (REGEXP . PRECEDENCE) or nil if none."
  (and (progn
         (while (and entry (not (stringp (cdr entry))))
           (setq entry
                 (if (symbolp (cdr entry))
                     ;; Alias for another entry.
                     (assq (cdr entry) rx-constituents)
                   ;; Wrong type, try further down the list.
                   (assq (car entry)
                         (cdr (memq entry rx-constituents))))))
         entry)
       (cons (list (cdr entry)) nil)))

(defun rx--translate-compat-form-entry (orig-form entry)
  "Translate a compatibility ORIG-FORM definition for ENTRY.
Return (REGEXP . PRECEDENCE) or nil if none."
  (and (progn
         (while (and entry (not (consp (cdr entry))))
           (setq entry
                 (if (symbolp (cdr entry))
                     ;; Alias for another entry.
                     (assq (cdr entry) rx-constituents)
                   ;; Wrong type, try further down the list.
                   (assq (car entry)
                         (cdr (memq entry rx-constituents))))))
         entry)
       (rx--translate-compat-form (cdr entry) orig-form)))

(defun rx--substitute (bindings form)
  "Substitute BINDINGS in FORM.  BINDINGS is an alist of (NAME . VALUES)
where VALUES is a list to splice into FORM wherever NAME occurs.
Return the substitution result wrapped in a list, since a single value
can expand to any number of values."
  (cond ((symbolp form)
         (let ((binding (assq form bindings)))
           (if binding
               (cdr binding)
             (list form))))
        ((consp form)
         (if (listp (cdr form))
             ;; Proper list.  We substitute variables even in the head
             ;; position -- who knows, might be handy one day.
             (list (mapcan (lambda (x) (copy-sequence
                                        (rx--substitute bindings x)))
                           form))
           ;; Cons pair (presumably an interval).
           (let ((first (rx--substitute bindings (car form)))
                 (second (rx--substitute bindings (cdr form))))
             (if (and first (not (cdr first))
                      second (not (cdr second)))
                 (list (cons (car first) (car second)))
               (error
                "Cannot substitute a &rest parameter into a dotted pair")))))
        (t (list form))))

;; FIXME: Consider adding extensions in Lisp macro style, where
;; arguments are passed unevaluated to code that returns the rx form
;; to use.  Example:
;;
;;   (rx-let ((radix-digit (radix)
;;             :lisp (list 'any (cons ?0 (+ ?0 (eval radix) -1)))))
;;     (rx (radix-digit (+ 5 3))))
;; =>
;;   "[0-7]"
;;
;; While this would permit more powerful extensions, it's unclear just
;; how often they would be used in practice.  Let's wait until there is
;; demand for it.

;; FIXME: An alternative binding syntax would be
;;
;;   (NAME RXs...)
;; and
;;   ((NAME ARGS...) RXs...)
;;
;; which would have two minor advantages: multiple RXs with implicit
;; `seq' in the definition, and the arglist is no longer an optional
;; element in the middle of the list.  On the other hand, it's less
;; like traditional lisp arglist constructs (defun, defmacro).
;; Since it's a Scheme-like syntax, &rest parameters could be done using
;; dotted lists:
;;  (rx-let (((name arg1 arg2 . rest) ...definition...)) ...)

(defun rx--expand-template (op values arglist template)
  "Return TEMPLATE with variables in ARGLIST replaced with VALUES."
  (let ((bindings nil)
        (value-tail values)
        (formals arglist))
    (while formals
      (pcase (car formals)
        ('&rest
         (unless (cdr formals)
           (error
            "Expanding rx def `%s': missing &rest parameter name" op))
         (push (cons (cadr formals) value-tail) bindings)
         (setq formals nil)
         (setq value-tail nil))
        (name
         (unless value-tail
           (error
            "Expanding rx def `%s': too few arguments (got %d, need %s%d)"
            op (length values)
            (if (memq '&rest arglist) "at least " "")
            (- (length arglist) (length (memq '&rest arglist)))))
         (push (cons name (list (car value-tail))) bindings)
         (setq value-tail (cdr value-tail))))
      (setq formals (cdr formals)))
    (when value-tail
      (error
       "Expanding rx def `%s': too many arguments (got %d, need %d)"
       op (length values) (length arglist)))
    (let ((subst (rx--substitute bindings template)))
      (if (and subst (not (cdr subst)))
          (car subst)
        (error "Expanding rx def `%s': must result in a single value" op)))))

(defun rx--translate-form (form)
  "Translate an rx form (list structure).  Return (REGEXP . PRECEDENCE)."
  (let ((body (cdr form)))
    (pcase (car form)
      ((or 'seq : 'and 'sequence) (rx--translate-seq body))
      ((or 'or '|)              (rx--translate-or body))
      ((or 'any 'in 'char)      (rx--translate-any nil body))
      ('rx--char-alt            (rx--translate-char-alt nil body))
      ('not-char                (rx--translate-any t body))
      ('not                     (rx--translate-not nil body))
      ('intersection            (rx--translate-intersection nil body))

      ('repeat                  (rx--translate-repeat body))
      ('=                       (rx--translate-= body))
      ('>=                      (rx--translate->= body))
      ('**                      (rx--translate-** body))

      ((or 'zero-or-more '0+)           (rx--translate-rep "*" rx--greedy body))
      ((or 'one-or-more '1+)            (rx--translate-rep "+" rx--greedy body))
      ((or 'zero-or-one 'opt 'optional) (rx--translate-rep "?" rx--greedy body))

      ('*                       (rx--translate-rep "*" t body))
      ('+                       (rx--translate-rep "+" t body))
      ((or '\? ?\s)             (rx--translate-rep "?" t body))

      ('*?                      (rx--translate-rep "*" nil body))
      ('+?                      (rx--translate-rep "+" nil body))
      ((or '\?? ??)             (rx--translate-rep "?" nil body))

      ('minimal-match           (rx--control-greedy nil body))
      ('maximal-match           (rx--control-greedy t   body))

      ((or 'group 'submatch)     (rx--translate-group body))
      ((or 'group-n 'submatch-n) (rx--translate-group-n body))
      ('backref                  (rx--translate-backref body))

      ('syntax                  (rx--translate-syntax nil body))
      ('not-syntax              (rx--translate-syntax t body))
      ('category                (rx--translate-category nil body))

      ('literal                 (rx--translate-literal body))
      ('eval                    (rx--translate-eval body))
      ((or 'regexp 'regex)      (rx--translate-regexp body))

      (op
       (cond
        ((not (symbolp op)) (error "Bad rx operator `%S'" op))

        ((let ((expanded (rx--expand-def-form form)))
           (and expanded
                (rx--translate expanded))))

        ;; For compatibility with old rx.
        ((let ((entry (assq op rx-constituents)))
           (and entry (rx--translate-compat-form-entry form entry))))

        (t (error "Unknown rx form `%s'" op)))))))

(defconst rx--builtin-forms
  '(seq sequence : and or | any in char not-char not intersection
    repeat = >= **
    zero-or-more 0+ *
    one-or-more 1+ +
    zero-or-one opt optional \?
    *? +? \??
    minimal-match maximal-match
    group submatch group-n submatch-n backref
    syntax not-syntax category
    literal eval regexp regex)
  "List of built-in rx function-like symbols.")

(defconst rx--builtin-symbols
  (append '(nonl not-newline any anychar anything unmatchable
            bol eol line-start line-end
            bos eos string-start string-end
            bow eow word-start word-end
            symbol-start symbol-end
            point word-boundary not-word-boundary not-wordchar)
          (mapcar #'car rx--char-classes))
  "List of built-in rx variable-like symbols.")

(defconst rx--builtin-names
  (append rx--builtin-forms rx--builtin-symbols)
  "List of built-in rx names.  These cannot be redefined by the user.")

;; Declare Lisp indentation rules for constructs that take 1 or 2
;; parameters before a body of RX forms.
;; (`>=' and `=' are omitted because they are more likely to be used
;; as Lisp functions than RX constructs; `repeat' is a `defcustom' type.)
(put 'group-n 'lisp-indent-function 1)
(put 'submatch-n 'lisp-indent-function 1)
(put '** 'lisp-indent-function 2)


(defun rx--translate (item)
  "Translate the rx-expression ITEM.  Return (REGEXP . PRECEDENCE)."
  (cond
   ((stringp item)
    (if (= (length item) 0)
        (cons nil 'seq)
      (cons (list (regexp-quote item)) (if (= (length item) 1) t 'seq))))
   ((characterp item)
    (cons (list (regexp-quote (char-to-string item))) t))
   ((symbolp item)
    (rx--translate-symbol item))
   ((consp item)
    (rx--translate-form item))
   (t (error "Bad rx expression: %S" item))))


;;;###autoload
(defun rx-to-string (form &optional no-group)
  "Translate FORM from `rx' sexp syntax into a string regexp.
The arguments to `literal' and `regexp' forms inside FORM must be
constant strings.
If NO-GROUP is non-nil, don't bracket the result in a non-capturing
group.

For extending the `rx' notation in FORM, use `rx-define' or `rx-let-eval'."
  (declare (important-return-value t))
  (let* ((item (rx--translate form))
         (exprs (if no-group
                    (car item)
                  (rx--atomic-regexp item))))
    (apply #'concat exprs)))

(defun rx--to-expr (form)
  "Translate the rx-expression FORM to a Lisp expression yielding a regexp."
  (let* ((rx--local-definitions
          ;; Retrieve local definitions from the macroexpansion environment.
          ;; (It's unclear whether the previous value of `rx--local-definitions'
          ;; should be included, and if so, in which order.)
          (cdr (assq :rx-locals macroexpand-all-environment)))
         (rx--delayed-evaluation t)
         (elems (car (rx--translate form)))
         (args nil))
    ;; Merge adjacent strings.
    (while elems
      (let ((strings nil))
        (while (and elems (stringp (car elems)))
          (push (car elems) strings)
          (setq elems (cdr elems)))
        (let ((s (apply #'concat (nreverse strings))))
          (unless (zerop (length s))
            (push s args))))
      (when elems
        (push (car elems) args)
        (setq elems (cdr elems))))
    (cond ((null args) "")                             ; 0 args
          ((cdr args) (cons 'concat (nreverse args)))  ; ≥2 args
          (t (car args)))))                            ; 1 arg


;;;###autoload
(defmacro rx (&rest regexps)
  "Translate regular expressions REGEXPS in sexp form to a regexp string.
Each argument is one of the forms below; RX is a subform, and RX... stands
for zero or more RXs.  For details, see Info node `(elisp) Rx Notation'.
See `rx-to-string' for the corresponding function.

STRING         Match a literal string.
CHAR           Match a literal character.

(seq RX...)    Match the RXs in sequence.  Alias: :, sequence, and.
(or RX...)     Match one of the RXs.  Alias: |.

(zero-or-more RX...) Match RXs zero or more times.  Alias: 0+.
(one-or-more RX...)  Match RXs one or more times.  Alias: 1+.
(zero-or-one RX...)  Match RXs or the empty string.  Alias: opt, optional.
(* RX...)       Match RXs zero or more times; greedy.
(+ RX...)       Match RXs one or more times; greedy.
(? RX...)       Match RXs or the empty string; greedy.
(*? RX...)      Match RXs zero or more times; non-greedy.
(+? RX...)      Match RXs one or more times; non-greedy.
(?? RX...)      Match RXs or the empty string; non-greedy.
(= N RX...)     Match RXs exactly N times.
(>= N RX...)    Match RXs N or more times.
(** N M RX...)  Match RXs N to M times.  Alias: repeat.
(minimal-match RX)  Match RX, with zero-or-more, one-or-more, zero-or-one
                and aliases using non-greedy matching.
(maximal-match RX)  Match RX, with zero-or-more, one-or-more, zero-or-one
                and aliases using greedy matching, which is the default.

(any SET...)    Match a character from one of the SETs.  Each SET is a
                character, a string, a range as string \"A-Z\" or cons
                (?A . ?Z), or a character class (see below).  Alias: in, char.
(not CHARSPEC)  Match one character not matched by CHARSPEC.  CHARSPEC
                can be a character, single-char string, (any ...), (or ...),
                (intersection ...), (syntax ...), (category ...),
                or a character class.
(intersection CHARSET...) Match all CHARSETs.
                CHARSET is (any...), (not...), (or...) or (intersection...),
                a character or a single-char string.
not-newline     Match any character except a newline.  Alias: nonl.
anychar         Match any character.  Alias: anything.
unmatchable     Never match anything at all.

CHARCLASS       Match a character from a character class.  One of:
 alpha, alphabetic, letter   Alphabetic characters (defined by Unicode).
 alnum, alphanumeric         Alphabetic or decimal digit chars (Unicode).
 digit, numeric, num         0-9.
 xdigit, hex-digit, hex      0-9, A-F, a-f.
 cntrl, control              ASCII codes 0-31.
 blank                       Horizontal whitespace (Unicode).
 space, whitespace, white    Chars with whitespace syntax.
 lower, lower-case           Lower-case chars, from current case table.
 upper, upper-case           Upper-case chars, from current case table.
 graph, graphic              Graphic characters (Unicode).
 print, printing             Whitespace or graphic (Unicode).
 punct, punctuation          Not control, space, letter or digit (ASCII);
                              not word syntax (non-ASCII).
 word, wordchar              Characters with word syntax.
 ascii                       ASCII characters (codes 0-127).
 nonascii                    Non-ASCII characters (but not raw bytes).

(syntax SYNTAX)  Match a character with syntax SYNTAX, being one of:
  whitespace, punctuation, word, symbol, open-parenthesis,
  close-parenthesis, expression-prefix, string-quote,
  paired-delimiter, escape, character-quote, comment-start,
  comment-end, string-delimiter, comment-delimiter

(category CAT)   Match a character in category CAT, being one of:
  space-for-indent, base, consonant, base-vowel,
  upper-diacritical-mark, lower-diacritical-mark, tone-mark, symbol,
  digit, vowel-modifying-diacritical-mark, vowel-sign,
  semivowel-lower, not-at-end-of-line, not-at-beginning-of-line,
  alpha-numeric-two-byte, chinese-two-byte, greek-two-byte,
  japanese-hiragana-two-byte, indian-two-byte,
  japanese-katakana-two-byte, strong-left-to-right,
  korean-hangul-two-byte, strong-right-to-left, cyrillic-two-byte,
  combining-diacritic, ascii, arabic, chinese, ethiopic, greek,
  korean, indian, japanese, japanese-katakana, latin, lao,
  tibetan, japanese-roman, thai, vietnamese, hebrew, cyrillic,
  can-break

Zero-width assertions: these all match the empty string in specific places.
 line-start         At the beginning of a line.  Alias: bol.
 line-end           At the end of a line.  Alias: eol.
 string-start       At the start of the string or buffer.
                     Alias: buffer-start, bos, bot.
 string-end         At the end of the string or buffer.
                     Alias: buffer-end, eos, eot.
 point              At point.
 word-start         At the beginning of a word.  Alias: bow.
 word-end           At the end of a word.  Alias: eow.
 word-boundary      At the beginning or end of a word.
 not-word-boundary  Not at the beginning or end of a word.
 symbol-start       At the beginning of a symbol.
 symbol-end         At the end of a symbol.

(group RX...)  Match RXs and define a capture group.  Alias: submatch.
(group-n N RX...) Match RXs and define capture group N.  Alias: submatch-n.
(backref N)    Match the text that capture group N matched.

(literal EXPR) Match the literal string from evaluating EXPR at run time.
(regexp EXPR)  Match the string regexp from evaluating EXPR at run time.
(eval EXPR)    Match the rx sexp from evaluating EXPR at macro-expansion
                (compile) time.

Additional constructs can be defined using `rx-define' and `rx-let',
which see.

\(fn REGEXPS...)"
  (rx--to-expr (cons 'seq regexps)))

(defun rx--make-binding (name tail)
  "Make a definitions entry out of TAIL.
TAIL is on the form ([ARGLIST] DEFINITION)."
  (unless (symbolp name)
    (error "Bad `rx' definition name: %S" name))
  ;; FIXME: Consider using a hash table or symbol property, for speed.
  (when (memq name rx--builtin-names)
    (error "Cannot redefine built-in rx name `%s'" name))
  (pcase tail
    (`(,def)
     (list def))
    (`(,args ,def)
     (unless (and (listp args) (all #'symbolp args))
       (error "Bad argument list for `rx' definition %s: %S" name args))
     (list args def))
    (_ (error "Bad `rx' definition of %s: %S" name tail))))

(defun rx--make-named-binding (bindspec)
  "Make a definitions entry out of BINDSPEC.
BINDSPEC is on the form (NAME [ARGLIST] DEFINITION)."
  (unless (consp bindspec)
    (error "Bad `rx-let' binding: %S" bindspec))
  (cons (car bindspec)
        (rx--make-binding (car bindspec) (cdr bindspec))))

(defun rx--extend-local-defs (bindspecs)
  (append (mapcar #'rx--make-named-binding bindspecs)
          rx--local-definitions))

;;;###autoload
(defmacro rx-let-eval (bindings &rest body)
  "Evaluate BODY with local BINDINGS for `rx-to-string'.
BINDINGS, after evaluation, is a list of definitions each on the form
(NAME [(ARGS...)] RX), in effect for calls to `rx-to-string'
in BODY.

For bindings without an ARGS list, NAME is defined as an alias
for the `rx' expression RX.  Where ARGS is supplied, NAME is
defined as an `rx' form with ARGS as argument list.  The
parameters are bound from the values in the (NAME ...) form and
are substituted in RX.  ARGS can contain `&rest' parameters,
whose values are spliced into RX where the parameter name occurs.

Any previous definitions with the same names are shadowed during
the expansion of BODY only.
For extensions when using the `rx' macro, use `rx-let'.
To make global rx extensions, use `rx-define'.
For more details, see Info node `(elisp) Extending Rx'.

\(fn BINDINGS BODY...)"
  (declare (indent 1) (debug (form body)))
  ;; FIXME: this way, `rx--extend-local-defs' may need to be autoloaded.
  `(let ((rx--local-definitions (rx--extend-local-defs ,bindings)))
     ,@body))

;;;###autoload
(defmacro rx-let (bindings &rest body)
  "Evaluate BODY with local BINDINGS for `rx'.
BINDINGS is an unevaluated list of bindings each on the form
(NAME [(ARGS...)] RX).
They are bound lexically and are available in `rx' expressions in
BODY only.

For bindings without an ARGS list, NAME is defined as an alias
for the `rx' expression RX.  Where ARGS is supplied, NAME is
defined as an `rx' form with ARGS as argument list.  The
parameters are bound from the values in the (NAME ...) form and
are substituted in RX.  ARGS can contain `&rest' parameters,
whose values are spliced into RX where the parameter name occurs.

Any previous definitions with the same names are shadowed during
the expansion of BODY only.
For local extensions to `rx-to-string', use `rx-let-eval'.
To make global rx extensions, use `rx-define'.
For more details, see Info node `(elisp) Extending Rx'.

\(fn BINDINGS BODY...)"
  (declare (indent 1) (debug (sexp body)))
  (let ((prev-locals (cdr (assq :rx-locals macroexpand-all-environment)))
        (new-locals (mapcar #'rx--make-named-binding bindings)))
    (macroexpand-all (cons 'progn body)
                     (cons (cons :rx-locals (append new-locals prev-locals))
                           macroexpand-all-environment))))

;;;###autoload
(defmacro rx-define (name &rest definition)
  "Define NAME as a global `rx' definition.
If the ARGS list is omitted, define NAME as an alias for the `rx'
expression RX.

If the ARGS list is supplied, define NAME as an `rx' form with
ARGS as argument list.  The parameters are bound from the values
in the (NAME ...) form and are substituted in RX.
ARGS can contain `&rest' parameters, whose values are spliced
into RX where the parameter name occurs.

Any previous global definition of NAME is overwritten with the new one.
To make local rx extensions, use `rx-let' for `rx',
`rx-let-eval' for `rx-to-string'.
For more details, see Info node `(elisp) Extending Rx'.

\(fn NAME [(ARGS...)] RX)"
  (declare (indent defun))
  `(eval-and-compile
     (put ',name 'rx-definition ',(rx--make-binding name definition))
     ',name))

;; During `rx--pcase-transform', list of defined variables in right-to-left
;; order.
(defvar rx--pcase-vars)

;; FIXME: The rewriting strategy for pcase works so-so with extensions;
;; definitions cannot expand to `let' or named `backref'.  If this ever
;; becomes a problem, we can handle those forms in the ordinary parser,
;; using a dynamic variable for activating the augmented forms.

(defun rx--pcase-transform (rx)
  "Transform RX, an rx-expression augmented with `let' and named `backref',
into a plain rx-expression, collecting names into `rx--pcase-vars'."
  (pcase rx
    (`(let ,name . ,body)
     (let* ((index (length (memq name rx--pcase-vars)))
            (i (if (zerop index)
                   (length (push name rx--pcase-vars))
                 index)))
       `(group-n ,i ,(rx--pcase-transform (cons 'seq body)))))
    ((and `(backref ,ref)
          (guard (symbolp ref)))
     (let ((index (length (memq ref rx--pcase-vars))))
       (when (zerop index)
         (error "rx `backref' variable must be one of: %s"
                (mapconcat #'symbol-name rx--pcase-vars " ")))
       `(backref ,index)))
    ((and `(,head . ,rest)
          (guard (and (or (symbolp head) (memq head '(?\s ??)))
                      (not (memq head '(literal regexp regex eval))))))
     (cons head (mapcar #'rx--pcase-transform rest)))
    (_ rx)))

(defun rx--reduce-right (f l)
  "Right-reduction on L by F.  L must be non-empty."
  (if (cdr l)
      (funcall f (car l) (rx--reduce-right f (cdr l)))
    (car l)))

;;;###autoload
(pcase-defmacro rx (&rest regexps)
  "A pattern that matches strings against `rx' REGEXPS in sexp form.
REGEXPS are interpreted as in `rx'.  The pattern matches any
string that is a match for REGEXPS, as if by `string-match'.

In addition to the usual `rx' syntax, REGEXPS can contain the
following constructs:

  (let REF RX...)  binds the symbol REF to a submatch that matches
                   the regular expressions RX.  REF is bound in
                   CODE to the string of the submatch or nil, but
                   can also be used in `backref'.
  (backref REF)    matches whatever the submatch REF matched.
                   REF can be a number, as usual, or a name
                   introduced by a previous (let REF ...)
                   construct."
  (let* ((rx--pcase-vars nil)
         (regexp (rx--to-expr (rx--pcase-transform (cons 'seq regexps)))))
    `(and (pred stringp)
          ,(pcase (length rx--pcase-vars)
            (0
             ;; No variables bound: a single predicate suffices.
             `(pred (string-match ,regexp)))
            (1
             ;; Create a match value that on a successful regexp match
             ;; is the submatch value, 0 on failure.  We can't use nil
             ;; for failure because it is a valid submatch value.
             `(app (lambda (s)
                     (if (string-match ,regexp s)
                         (match-string 1 s)
                       0))
                   (and ,(car rx--pcase-vars) (pred (not numberp)))))
            (nvars
             ;; Pack the submatches into a dotted list which is then
             ;; immediately destructured into individual variables again.
             ;; This is of course slightly inefficient.
             ;; A dotted list is used to reduce the number of conses
             ;; to create and take apart.
             `(app (lambda (s)
                     (and (string-match ,regexp s)
                          ,(rx--reduce-right
                            (lambda (a b) `(cons ,a ,b))
                            (mapcar (lambda (i) `(match-string ,i s))
                                    (number-sequence 1 nvars)))))
                   ,(list '\`
                          (rx--reduce-right
                           #'cons
                           (mapcar (lambda (name) (list '\, name))
                                   (reverse rx--pcase-vars))))))))))

;; Obsolete internal symbol, used in old versions of the `flycheck' package.
(define-obsolete-function-alias 'rx-submatch-n #'rx-to-string "27.1")

(provide 'rx)

;;; rx.el ends here
