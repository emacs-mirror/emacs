;;; elb-smie.el --- C major mode based on SMIE  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 1.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Copy of the 2021 version of `sm-c-mode'.

;;; Code:

(require 'cl-lib)
(require 'smie)

(defgroup elb-smie-mode nil
  "Major mode to edit C code, based on SMIE."
  :group 'programming)

(defcustom elb-smie-indent-basic 2
  "Basic step of indentation.
Typically 2 for GNU style and `tab-width' for Linux style."
  :type 'integer)

(defcustom elb-smie-indent-braces t
  "If nil, braces in if/while/... are aligned with the if/while/...
Else, they're indented by `elb-smie-indent-basic' columns.
For braces placed at the end of lines (which SMIE calls \"hanging\"), it makes
no difference."
  :type 'boolean)

;;; Handling CPP directives.

(defsubst elb-smie--cpp-inside-p (ppss)
  (eq 2 (nth 7 ppss)))

(eval-and-compile
  (defconst elb-smie--cpp-regexp "^[ \t]*\\(\\(#\\)[ \t]*\\([a-z]+\\)\\)"))

(defconst elb-smie--cpp-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defun elb-smie--cpp-goto-end (ppss &optional limit)
  (cl-assert (elb-smie--cpp-inside-p ppss))
  (let (found)
    (while
        (and (setq found (re-search-forward "\\(?:\\\\\\\\\\)*\n" limit 'move))
             ;; We could also check (nth 5 ppss) to figure out if we're
             ;; after a backslash, but this is a very common case, so it's good
             ;; to avoid calling parse-partial-sexp for that.
             (or (eq ?\\ (char-before (match-beginning 0)))
                 (with-syntax-table elb-smie--cpp-syntax-table
                   (nth 4 (parse-partial-sexp (1+ (nth 8 ppss)) (point)))))))
    found))

(defvar syntax-ppss-cache)
(defvar syntax-ppss-last)

(defun elb-smie--cpp-fontify-syntactically (ppss)
  ;; FIXME: ¡¡BIG UGLY HACK!!
  ;; Copied from font-lock.el's font-lock-fontify-syntactically-region.
  (cl-assert (> (point) (nth 8 ppss)))
  (save-excursion
    (save-restriction
      (elb-smie--cpp-goto-end ppss)
      (narrow-to-region (1+ (nth 8 ppss)) (point))
      ;; FIXME: We should add some "with-local-syntax-ppss" macro to
      ;; encapsulate this.
      (let ((syntax-propertize-function nil)
            (syntax-ppss-cache nil)
            (syntax-ppss-last nil))
        (font-lock-fontify-syntactically-region (point-min) (point-max))))))

(defun elb-smie--cpp-syntax-propertize (end)
  (let ((ppss (syntax-ppss))
        found)
    (when (elb-smie--cpp-inside-p ppss)
      (while
          (and (setq found (re-search-forward "\\(\\\\\\\\\\)*\n" end 'move))
               (or (eq ?\\ (char-before (match-beginning 0)))
                   (with-syntax-table elb-smie--cpp-syntax-table
                     (nth 4 (parse-partial-sexp (1+ (nth 8 ppss)) (point)))))))
      (when found
        (let* ((ppss-in
                (save-excursion
                  (parse-partial-sexp (1+ (nth 8 ppss)) (1- (point)))))
               ;; Put the end before a closing //...\n comment so as to avoid
               ;; a bug in back_comment.  The problem is that back_comment
               ;; otherwise will see "// <...> <...> \n" and will consider the
               ;; CPP pseudo-comments as nested within the //...\n comment.
               (end (if (and (nth 4 ppss-in)        ;Inside a comment.
                             (null (nth 7 ppss-in)) ;A style `a' comment.
                             (memq (char-before (nth 8 ppss-in)) '(?\s ?\t)))
                        (nth 8 ppss-in)
                      (point))))
          (put-text-property (1- end) end
                             'syntax-table (string-to-syntax "> c")))))))

;;;; Indenting CPP directives.

(defcustom elb-smie-indent-cpp-basic 1
  "Indent step for CPP directives.
If non-zero, CPP directives are indented according to CPP depth.
E.g. a #define nested within 2 #ifs will be turned into \"#  define\"."
  :type 'integer)

(defun elb-smie--cpp-prev (tok)
  (let ((offset nil))
    (while
        (when (re-search-backward elb-smie--cpp-regexp nil t)
          (pcase (cons tok (match-string 3))
            (`(,_ . "endif") (elb-smie--cpp-prev "endif"))
            ((or `(,(or "endif" "else" "elif") . ,(or "if" "ifdef" "ifndef"))
                 `(,(or "else" "elif") . "elif"))
             (setq offset 0))
            (`(,(or "endif" "else" "elif") . ,_) nil)
            (`(,_ . ,(or "if" "ifdef" "ifndef" "elif" "else"))
             (setq offset elb-smie-indent-cpp-basic))
            (_ (setq offset 0)))
          (not offset)))
    (when offset
      (goto-char (match-beginning 3))
      (+ offset (current-column)))))


(defun elb-smie--cpp-indent-line (&optional _arg)
  ;; FIXME: Also align the terminating \, if any.
  (when (> elb-smie-indent-cpp-basic 0)
    (let* ((pos (point-marker))
           (beg)
           (indent
            (save-excursion
              (forward-line 0)
              (when (looking-at elb-smie--cpp-regexp)
                (setq beg (match-beginning 3))
                (or (elb-smie--cpp-prev (match-string 3)) 0)))))
      (when indent
        (let ((before (<= pos beg)))
          (goto-char beg)
          (unless (= (current-column) indent)
            (skip-chars-backward " \t")
            (delete-region (point)
                           (progn (skip-chars-forward " \t") (point)))
            (indent-to indent))
          (unless before (goto-char pos)))))))

;;;; Indenting inside CPP #define.

(defconst elb-smie--cpp-smie-indent-functions
  ;; FIXME: Don't just align line after #define with the "d"!
  (mapcar
   (lambda (f)
     (cond
      ((eq f #'smie-indent-comment-inside) #'elb-smie--cpp-indent-comment-inside)
      ;; ((eq f #'smie-indent-exps) #'elb-smie--cpp-indent-exps)
      (t f)))
   (default-value 'smie-indent-functions)))

(defun elb-smie--cpp-indent-comment-inside ()
  (let ((ppss (syntax-ppss)))
    (when (nth 4 ppss)
      ;; Indicate where's the comment start.
      `(noindent . ,(nth 8 ppss)))))

(defun elb-smie--cpp-smie-indent ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((elb-smie--cpp-inside-p ppss)
      (save-restriction
        (narrow-to-region (nth 8 ppss) (point-max))
        (let ((indent
               (let ((smie-indent-functions elb-smie--cpp-smie-indent-functions)
                     (syntax-ppss-cache nil)
                     (syntax-ppss-last nil)
                     (parse-sexp-lookup-properties nil))
                 (smie-indent-calculate))))
          (if (not (eq 'noindent (car-safe indent)))
              (if (integerp indent)
                  (max (funcall smie-rules-function :elem 'basic) indent)
                indent)
            ;; We can't just return `noindent' if we're inside a comment,
            ;; because the indent.el code would then be similarly confused,
            ;; thinking the `noindent' is because we're inside the cpp
            ;; pseudo-comment, and would hence align the code with the content
            ;; of the psuedo-comment rather than the nested real comment!
            ;;
            ;; FIXME: Copy&paste from indent--default-inside-comment.
            ;; FIXME: This will always re-indent inside these comments, even
            ;; during indent-region.
            (save-excursion
              (forward-line -1)
              (skip-chars-forward " \t")
              (when (< (1- (point)) (cdr indent) (line-end-position))
                (goto-char (cdr indent))
                (when (looking-at comment-start-skip)
                  (goto-char (match-end 0))))
              (current-column))))))

     ((equal (syntax-after (point)) (string-to-syntax "< c")) 0)
     )))

;;; Syntax table

(defvar elb-smie-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    st))

(defun elb-smie-syntax-propertize (start end)
  (goto-char start)
  (elb-smie--cpp-syntax-propertize end)
  (funcall
   (syntax-propertize-rules
    (elb-smie--cpp-regexp
     (2 (prog1 "< c"
          (when (and (equal (match-string 3) "include")
                     (looking-at "[ \t]*\\(<\\)[^>\n]*\\(>\\)"))
            (put-text-property (match-beginning 1) (match-end 1)
                               'syntax-table (string-to-syntax "|"))
            (put-text-property (match-beginning 2) (match-end 2)
                               'syntax-table (string-to-syntax "|")))
          (elb-smie--cpp-syntax-propertize end))))
    ;; Handle // comments that span multiple lines via \\\n!
    ("\\\\\\(\n\\)"
     (1 (let ((ppss (save-excursion (syntax-ppss (match-beginning 0)))))
          (when (and (nth 4 ppss)        ;Within a comment
                     (null (nth 7 ppss)) ;Within a // comment
                     (save-excursion     ;The \ is not itself escaped
                       (goto-char (match-beginning 0))
                       (zerop (mod (skip-chars-backward "\\\\") 2))))
            (string-to-syntax "."))))))
   (point) end))

(defun elb-smie-syntactic-face-function (ppss)
  (if (elb-smie--cpp-inside-p ppss)
      (prog1 nil (elb-smie--cpp-fontify-syntactically ppss))
    (funcall (default-value 'font-lock-syntactic-face-function) ppss)))

;;; SMIE support

(defconst elb-smie-paren-block-keywords '("if" "while" "for" "switch"))

(defconst elb-smie-smie-precedence-table
  '((assoc ";")
    ;; Compiled from https://en.wikipedia.org/wiki/Operators_in_C_and_C++.
    (assoc ",")                         ;1
    ;; (nonassoc "throw")
    (nonassoc "=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "^=" "|=") ;2
    ;; (nonassoc "?" ":") ;; Better handle it in the BNF.
    (assoc "||")                        ;3
    (assoc "&&")                        ;4
    (assoc "|")                         ;5
    (assoc "^")                         ;6
    ;; (assoc "&") ;; Binary and.  Confused with address-of.
    (nonassoc "==" "!=")                ;7
    (nonassoc "<" "<=" ">" ">=")        ;8
    (nonassoc "<<" ">>")                ;9
    (assoc "+" "-")                     ;10
    (assoc "/" "* mult" "%")            ;11
    ;; (nonassoc ".*" "->*")            ;12   ;; Only C++
    ;; (nonassoc "++" "--" "+" "-" "!" "~" "(type)" "*" "&"
    ;;          "sizeof" "new" "delete");13  ;; All prefix.
    (left "." "->") ;; "++" "--" suffixes, "()", "[]", "typeid", "*_cast". ;14
    ;; (noassoc "::") ;; Only C++
    ))

(defconst elb-smie-smie-grammar
  ;; `((:smie-closer-alist ("{" . "}")) ("{" (39) 0) ("}" 0 (40)) ("else" 27 26) ("," 38 38) ("do" (41) 22) ("while" (42) 23) ("for" (43) 24) (";" 11 11) ("if" (44) 25))
  (let ((grm
         (smie-prec2->grammar
          (smie-merge-prec2s
           (smie-bnf->prec2
            '((decls ("typedef" decl) ("extern" decl)
                     (decls ";" decls))
              (decl)
              (id)
              (insts ("{" insts "}")
                     (insts ";" insts)
                     ("return" exp)
                     ("goto" exp)
                     (":label")
                     ("case" subexp ": case")
                     ("else" exp-if))
              (exp-if ("if" exp) ("do" exp) ("while" exp) ("switch" exp) ("for" exp)
                      (exp))
              (exp ("(" exp ")") (exp "," exp) (subexp "?" exp ":" exp))
              (subexp (subexp "||" subexp))
              ;; Some of the precedence table deals with pre/postfixes, which
              ;; smie-precs->prec2 can't handle, so handle it here instead.
              (exp11 (exp12) (exp11 "/" exp11))
              (exp12 (exp13))           ;C++ only.
              (exp13 (exp14) ("++ prefix" exp13) ("-- prefix" exp13)
                     ("!" exp13) ("~" exp13) ("&" exp13) ("* deref" exp13))
              (exp14 (id) (exp14 "++ postfix") (exp14 "-- postfix")
                     (exp14 "->" id) (exp14 "." id)))
            '((assoc ";") (assoc ",") (nonassoc "?" ":"))
            elb-smie-smie-precedence-table)
           (smie-precs->prec2 elb-smie-smie-precedence-table)
           (smie-precs->prec2 '((nonassoc ";") (nonassoc ":")))))))
    ;; SMIE gives (":label" 261 262), but really this could just as well be
    ;; (":label" nil nil) because labels don't have any argument to their left
    ;; or right.  They're like both openers and closers at the same time.
    (mapcar (lambda (x)
              (if (equal (car-safe x) ":label")
                  ;; Rather than (":label" (n1) (n2)) we use
                  ;; (":label" (n1) n2) because SMIE otherwise complains:
                  ;; cl--assertion-failed((numberp (funcall op-forw toklevels)))
                  ;; in smie-next-sexp.
                  `(,(nth 0 x) (,(nth 1 x)) ,(nth 2 x)) x))
            grm)))

;; (defun elb-smie--:-discriminate ()
;;   (save-excursion
;;     (and (null (smie-backward-sexp))
;;          (let ((prev (smie-indent-backward-token)))
;;            (cond
;;             ((equal prev "case" ) ": case")
;;             ((member prev '(";" "{" "}")) ":-label")
;;             (t ":"))))))

(defconst elb-smie-smie-operator-regexp
  (let ((ops '()))
    (pcase-dolist (`(,token . ,_) elb-smie-smie-grammar)
      (when (and (stringp token) (string-match "\\`[^ [:alnum:](){}]+" token))
        (push (match-string 0 token) ops)))
    (regexp-opt ops)))

(defun elb-smie-smie-forward-token ()
  (forward-comment (point-max))
  (let ((tok (if (looking-at elb-smie-smie-operator-regexp)
                 (progn (goto-char (match-end 0)) (match-string 0))
               (smie-default-forward-token))))
    (cond
     ((and (equal tok "") (looking-at "\\\\\n"))
      (goto-char (match-end 0))
      (elb-smie-smie-forward-token))
     ((member tok '(":" "*"))
      (save-excursion (elb-smie-smie-backward-token)))
     ((looking-at "[ \t]*:")
      (if (not (equal (save-excursion (elb-smie-smie-forward-token)) ":label"))
          tok
        (looking-at "[ \t]*:")
        (goto-char (match-end 0)) ":label"))
     (t tok))))


(defun elb-smie-smie-backward-token ()
  (forward-comment (- (point)))
  (let ((tok (if (looking-back elb-smie-smie-operator-regexp (- (point) 3) t)
                 (progn (goto-char (match-beginning 0)) (match-string 0))
               (smie-default-backward-token))))
    (cond
     ((and (equal tok "") (looking-at "\n"))
      (let ((pos (point)))
        (if (not (= 0 (mod (skip-chars-backward "\\\\") 2)))
            (elb-smie-smie-backward-token)
          (goto-char pos)
          tok)))
     ((equal tok "*") (elb-smie-smie--*-token))
     ((equal tok ":")
      (let ((pos1 (point))
            (prev (elb-smie-smie-backward-token)))
        (if (zerop (length prev))
            (progn (goto-char pos1) tok)
          (let ((pos2 (point)))
            (pcase (car (smie-indent-backward-token))
              ("case" (goto-char pos1) ": case")
              ((or ";" "{" "}") (goto-char pos2) ":label")
              (_ (goto-char pos1) tok))))))
     (t tok))))

(defun elb-smie--prev-token ()
  (car (smie-indent-backward-token)))

(defun elb-smie--else-to-if ()
  (let ((pos (point)))
    (unless (equal (elb-smie--prev-token) ";")
      (goto-char pos))
    (while
        (pcase (smie-backward-sexp)
          (`(,_ ,pos "if") (goto-char pos) nil) ;Found it!
          (`(,_ ,_ ";") nil)                    ;Can't find it!
          (`(,_ ,pos "else") (goto-char pos) (elb-smie--else-to-if) t)
          (`(,_ ,pos "while")
           (goto-char pos) (unless (elb-smie--while-to-do) (goto-char pos)) t)
          (`(t . ,_) nil)               ;Can't find it!
          (`(,_ ,pos . ,_) (goto-char pos) t)
          (`nil t)))))

(defun elb-smie--while-to-do ()
  "Jump to the matching `do' and return non-nil, if any.  Return nil otherwise."
  (pcase (elb-smie--prev-token)
    ("}"
     ;; The easy case!
     (forward-char 1) (backward-sexp 1)
     (equal (elb-smie--prev-token) "do"))
    (";"
     (let ((found-do nil))
       (while
           (pcase (smie-backward-sexp)
             (`(,_ ,pos "do") (goto-char pos) (setq found-do t) nil)
             (`(,_ ,_ ";") nil)         ;Can't find it!
             (`(,_ ,pos "else") (goto-char pos) (elb-smie--else-to-if) t)
             (`(,_ ,pos "while")
              (goto-char pos) (unless (elb-smie--while-to-do) (goto-char pos)) t)
             (`(t . ,_) nil)            ;Can't find it!
             (`(,_ ,pos . ,_) (goto-char pos) t)
             (`nil (or (not (looking-at "{"))
                       (smie-rule-prev-p "=")))))
       found-do))))

(defun elb-smie--skip-labels (max)
  (while
      (let ((start (point)))
        (pcase (elb-smie-smie-forward-token)
          ("case"
           (smie-forward-sexp "case")
           (forward-comment (point-max))
           (if (>= (point) max) (progn (goto-char start) nil)
             t))
          (":label"
           (forward-comment (point-max))
           (if (>= (point) max) (progn (goto-char start) nil)
             t))
          (_ (goto-char start) nil)))))

(defun elb-smie--boi (&optional inner)
  "Jump to the beginning-of-instruction.
By default for things like nested ifs, it jumps to the outer if, but
if INNER is non-nil, it stops at the innermost one."
  (while
      (let ((pos (point)))
        (pcase (smie-backward-sexp)
          (`(,_ ,_ ";") nil)            ;Found it!
          (`(,_ ,pos "else") (goto-char pos) (elb-smie--else-to-if) t)
          (`(,_ ,pos "while")
           (goto-char pos) (unless (elb-smie--while-to-do) (goto-char pos)) t)
          (`(,(pred numberp) ,pos . ,_) (goto-char pos) t)
          ((or `nil `(nil . ,_))
           (if (and (or (not (looking-at "{"))
                        (smie-rule-prev-p "="))
                    (not (bobp)))
               t
             (goto-char pos) nil))
          (`(,_ ,_ ,(or "(" "{" "[")) nil) ;Found it!
          (`(,_ ,pos ,(and tok
                           (guard (when inner
                                    (or (member tok elb-smie-paren-block-keywords)
                                        (equal tok "do"))))))
           (ignore tok)
           (goto-char pos) nil) ;Found it!
          (`(t ,(pred (eq (point-min))) . ,_) nil)
          (`(,_ ,pos . ,_) (goto-char pos) t)))))

;; (defun elb-smie--if-tail-to-head ()
;;   (pcase (elb-smie--prev-token)
;;     (")"
;;      (forward-char 1) (backward-sexp 1)
;;      (pcase (elb-smie--prev-token)
;;        ("if" nil)
;;        ((or "while" "for") (elb-smie--if-tail-to-head))))
;;     ("do" (elb-smie--if-tail-to-head))))

(defun elb-smie--boe (tok)
  (let ((start (point))
        (res (smie-backward-sexp tok)))
    (when (member (nth 2 res) '("if" "while" "do" "for" "else"))
      (when (member (nth 2 res) '("if" "for"))
        (let ((forward-sexp-function nil))
          (forward-sexp 1))
        (forward-comment (point-max)))
      (when (looking-at "{")
        (let ((forward-sexp-function nil))
          (forward-sexp 1))
        (forward-comment (point-max)))
      (if (> (point) start) (goto-char start)))))

(defun elb-smie-smie--*-token ()
  (save-excursion
    (let ((pos (point)))
      (pcase (car (smie-indent-backward-token))
        (")"
         ;; Can be a multiplication (as in "(a+b)*c"), or a deref
         ;; (as in "if (stop) *a = 0;")
         (if (and (goto-char (nth 1 (syntax-ppss)))
                  (eq ?\( (char-after))
                  (member (smie-default-backward-token) '("if" "for")))
             "* deref"
           "* mult"))
        ("]" "* mult")                         ;Multiplication.
        ((or "(" "[" "{" "}") "* deref")
        (`nil
         (goto-char pos)
         (let ((res nil))
           (while (not (or res (bobp)))
             (pcase (smie-backward-sexp)
               (`(,_ ,_ ,(or ";" "{")) (setq res "* deref"))
               ((and `nil (guard (looking-at "{"))) (setq res "* deref"))
               (`(,left ,_ ,op)
                (if (and (numberp left)
                         (numberp (nth 2 (assoc op smie-grammar)))
                         (< (nth 2 (assoc op smie-grammar))
                            (nth 1 (assoc "* mult" smie-grammar))))
                    (smie-backward-sexp 'halfsexp)
                  (setq res "* mult")))))
           (or res "* mult")))
        (_ "* mult")))))

(defun elb-smie-smie-hanging-eolp ()
  (let ((start (point))
        (prev (smie-indent-backward-token)))
    (if (and (not (numberp (nth 1 prev)))
             (save-excursion (equal (elb-smie-smie-backward-token) ";")))
        ;; Treat instructions that start after ";" as always "hanging".
        (end-of-line)
      (goto-char start)))
  (skip-chars-forward " \t")
  (or (eolp)
      (forward-comment (point-max))
      (and (looking-at "\\\\\n")
           (goto-char (match-end 0)))))

(defvar elb-smie-smie--inhibit-case/label-rule nil)

(defun elb-smie--smie-virtual ()
  (if (and (smie-indent--bolp)
           (not (save-excursion
                  (member (elb-smie-smie-forward-token)
                          '("case" ":label")))))
      (current-column)
    (let ((elb-smie-smie--inhibit-case/label-rule t))
      (smie-indent-calculate))))

(defun elb-smie-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) elb-smie-indent-basic)
    (`(:list-intro . ";")
     (save-excursion
       (forward-char 1)
       (if (and (null (smie-forward-sexp))
                ;; FIXME: Handle \\\n as well!
                (progn (forward-comment (point-max))
                       (looking-at "(")))
           nil
         t)))
    (`(:before . "else")
     (save-excursion
       (elb-smie--else-to-if)
       `(column . ,(smie-indent-virtual))))
    (`(:before . "while")
     (save-excursion
       (when (elb-smie--while-to-do)
         `(column . ,(smie-indent-virtual)))))
    (`(:before . ,(or "=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "^=" "|="))
     (save-excursion
       (elb-smie--boe token)
       `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                      (smie-indent-virtual)))))
    (`(:before . "if")
     (when (and (not (smie-rule-bolp)) (smie-rule-prev-p "else"))
       (save-excursion
         (smie-indent-backward-token)
         `(column . ,(elb-smie--smie-virtual)))))
    ;; (`(:after . ,(or "=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "^=" "|="))
    ;;  (funcall smie-rules-function :elem 'basic))
    (`(:before . "{")
     (cond
      ((smie-rule-prev-p "=") nil)      ;Not a block of instructions!
      ((save-excursion
         (let ((pos (point)))
           (elb-smie--boi 'inner) (elb-smie--skip-labels (point-max))
           (let ((tok (save-excursion (elb-smie-smie-forward-token))))
             (cond
              ((or (equal tok "typedef")
                   (and (member tok '("enum" "struct"))
                        ;; Make sure that the {...} is about this struct/enum,
                        ;; as opposed to "struct foo *get_foo () {...}"!
                        (save-excursion
                          (smie-indent-forward-token)
                          (smie-indent-forward-token)
                          (forward-comment (point-max))
                          (>= (point) pos))))
               `(column . ,(+ (if (save-excursion
                                    (goto-char pos)
                                    (smie-rule-hanging-p))
                                  0
                                (funcall smie-rules-function :elem 'basic))
                              (smie-indent-virtual))))
              ((and (member tok '("enum" "struct"))
                    ;; Make sure that the {...} is about this struct/enum, as
                    ;; opposed to "struct foo *get_foo () {...}"!
                    (save-excursion
                      (smie-indent-forward-token)
                      (smie-indent-forward-token)
                      (forward-comment (point-max))
                      (>= (point) pos)))
               `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                              (smie-indent-virtual))))
              ((or (member tok elb-smie-paren-block-keywords)
                   (equal tok "do"))
               nil)
              ((save-excursion
                 (goto-char pos)
                 (when (and (> (car (syntax-ppss)) 0)
                            (equal ")" (car (smie-indent-backward-token))))
                   (up-list -1)
                   `(column . ,(elb-smie--smie-virtual)))))
              ((>= (point) pos) nil)
              (t `(column . ,(smie-indent-virtual))))))))
      ((smie-rule-hanging-p)
       (cond
        ((smie-rule-prev-p "do" "else")
         (smie-indent-backward-token))
        ((smie-rule-prev-p ")")
         (smie-backward-sexp)
         (smie-indent-backward-token))
        (t (elb-smie--boi 'inner)))
       `(column . ,(elb-smie--smie-virtual)))
      (t
       (let ((pos (point)))
         (pcase (elb-smie--prev-token)
           ((or "do" "else")
            (cond
             (elb-smie-indent-braces
              `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                             (smie-indent-virtual))))))
           (")" nil)
           (_ (goto-char pos) (elb-smie--boi)
              (if (< (point) pos)
                  `(column . ,(elb-smie--smie-virtual)))))))))
    (`(:before . "(")
     (save-excursion
       (let ((res (smie-backward-sexp)))
         (pcase res
           (`nil
            (if (looking-at "(")
                ;; (unless (save-excursion
                ;;           (member (elb-smie-smie-backward-token)
                ;;                   elb-smie-paren-block-keywords))
                ;;   `(column . ,(elb-smie--smie-virtual)))
                nil
              `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                             (elb-smie--smie-virtual)))))))))
    (`(:after . "else")
     (save-excursion
       (funcall smie-rules-function :elem 'basic)))
    (`(:after . ")")
     (save-excursion
       (let ((_ (progn (forward-char 1) (backward-sexp 1)))
             (pos (point))
             (prev (elb-smie-smie-backward-token)))
         (cond
          ((member prev elb-smie-paren-block-keywords)
           `(column . ,(+ (funcall smie-rules-function :elem 'basic)
                          (smie-indent-virtual))))
          ((and (looking-at "[[:alnum:]_]+(")
                (save-excursion
                  (forward-line 0)
                  (and (bobp) (looking-at elb-smie--cpp-regexp))))
           ;; Will be bumped up presumably by the "max" in
           ;; elb-smie--cpp-smie-indent.
           `(column . 0))
          (t (goto-char pos) `(column . ,(elb-smie--smie-virtual)))))))
    (`(:after . "}")
     (save-excursion
       (forward-char 1) (backward-sexp 1)
       (elb-smie--boi)
       `(column . ,(elb-smie--smie-virtual))))
    (`(:after . ";")
     (save-excursion
       (elb-smie--boi)
       `(column . ,(elb-smie--smie-virtual))))
    (`(:after . ":label")
     ;; Yuck!
     `(column . ,(elb-smie--smie-virtual)))
    (`(:after . ": case")
     ;; Yuck!
     (save-excursion
       (smie-backward-sexp ": case")
       `(column . ,(elb-smie--smie-virtual))))
    (`(:after . "* deref") `(column . ,(elb-smie--smie-virtual)))
    ((and `(:before . ":label") (guard (not elb-smie-smie--inhibit-case/label-rule)))
     (let ((ppss (syntax-ppss)))
       (when (nth 1 ppss)
         (save-excursion
           (goto-char (nth 1 ppss))
           `(column . ,(smie-indent-virtual))))))
    ((and `(:before . "case") (guard (not elb-smie-smie--inhibit-case/label-rule)))
     (catch 'found
       (dolist (pos (reverse (nth 9 (syntax-ppss))))
         (save-excursion
           (goto-char pos)
           (and (looking-at "{")
                (null (car-safe (smie-backward-sexp)))
                (equal "switch" (elb-smie-smie-backward-token))
                (goto-char pos)
                (throw 'found `(column . ,(smie-indent-virtual))))))))))

;;; Backslash alignment

(defvar-local elb-smie--bs-changed nil)

(defun elb-smie--bs-after-change (beg end _len)
  (unless undo-in-progress
    (if (null elb-smie--bs-changed)
        (setq elb-smie--bs-changed (cons beg end))
      (cl-callf (lambda (x) (min x beg)) (car elb-smie--bs-changed))
      (cl-callf (lambda (x) (max x end)) (cdr elb-smie--bs-changed)))))

(defun elb-smie--bs-realign ()
  (when elb-smie--bs-changed
    (elb-smie--bs-realign-1 (car elb-smie--bs-changed) (cdr elb-smie--bs-changed))
    (setq elb-smie--bs-changed nil)))

(defcustom elb-smie-backslash-max-align-column 78
  "Maximum column to align backslashes.
Past this column, we do not try to align the backslashes."
  :type 'integer)

(defun elb-smie--bs-current-column ()
  (let ((col (current-column)))
    (if (> col elb-smie-backslash-max-align-column)
        0 col)))

(defun elb-smie--bs-realign-1 (from to)
  (save-excursion
    (goto-char from)
    (end-of-line)
    (unless (zerop (mod (skip-chars-backward "\\\\") 2))
      (skip-chars-backward " \t")
      (setq from (point))
      (let ((col (elb-smie--bs-current-column))
            start end)
        (while
            (progn (setq start (point))
                   (end-of-line 0)
                   (and (< (point) start)
                        (not (zerop (mod (skip-chars-backward "\\\\") 2)))))
          (unless (>= col elb-smie-backslash-max-align-column)
            (skip-chars-backward " \t")
            (setq col (max (elb-smie--bs-current-column) col))))
        (goto-char from)
        (while
            (progn (setq end (point))
                   (end-of-line 2)
                   (and (> (line-beginning-position) end)
                        (not (zerop (mod (skip-chars-backward "\\\\") 2)))))
          (unless (>= col elb-smie-backslash-max-align-column)
            (skip-chars-backward " \t")
            (setq col (max (elb-smie--bs-current-column) col))))
        (goto-char to)
        (beginning-of-line)
        (unless (or (> (point) end)     ;Don't realign if we changed outside!
                    (<= end start))     ;A lone \
          
          (setq col (1+ col))         ;Add a space before the backslashes.
          (goto-char end)
          (end-of-line)
          (while (>= (point) start)
            (cl-assert (eq (char-before) ?\\))
            (forward-char -1)
            (let ((curcol (current-column)))
              (cond
               ((> col curcol) (indent-to col))
               ((< col curcol)
                (skip-chars-backward " \t")
                (unless (> (current-column) col)
                  (move-to-column col t)
                  (delete-region (point) (1- (line-end-position)))))))
            (end-of-line 0)))))))
            
;;; Font-lock support

(defconst elb-smie--comment-regexp
  "/\\(?:/.*\n\\|\\*[^*]*\\(?:\\*+[^/*][^*]*\\)*\\*+/\\)")

(defconst elb-smie--defun-regexp
  (let* ((spc0 (concat "\\(?:\n?[ \t]\\|" elb-smie--comment-regexp "\\)*"))
         (spc1 (concat "\n?[ \t]" spc0))
         (id "\\(?:\\sw\\|\\s_\\)+"))
    (cl-flet ((repeat (repetition &rest res)
                      (concat "\\(?:" (apply #'concat res) "\\)"
                              (pcase repetition
                                ((pred symbolp) (symbol-name repetition))
                                (1 "")))))
      (concat
       "^\\(?:"
       (repeat '* "\\*" spc0)                               ;Pointer symbols.
       (repeat '* id (repeat 1 spc1 "\\|" spc0 "\\*" spc0)) ;Type(s).
       "\\(" id "\\)[ \t\n]*("                              ;Function name.
       "\\|"
       "[ \t]*#[ \t]*define[ \t]+\\(?1:" id "\\)("
       "\\)"))))

(defconst elb-smie-font-lock-keywords
  `((,elb-smie--cpp-regexp (1 font-lock-preprocessor-face))
    ("\\_<\\(?:true\\|false\\)\\_>" (0 font-lock-constant-face))
    ("\\_<\\(case\\)\\_>[ \t]*\\([^: \t]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face))
    ("\\(?:[{};]\\(\\)\\|^\\)[ \t]*\\([[:alpha:]_][[:alnum:]_]*\\)[ \t]*:"
     (2 (if (or (match-beginning 1)
                (save-excursion (equal ":label" (elb-smie-smie-backward-token))))
            font-lock-constant-face)))
    (,(let ((kws (delq nil (mapcar (lambda (x)
                                     (setq x (car x))
                                     (and (stringp x)
                                          (string-match "\\`[a-z]" x)
                                          x))
                                   elb-smie-smie-grammar))))
        (concat "\\_<" (regexp-opt
                        (append
                         ;; Elements not in SMIE's grammar.  Either because
                         ;; they're uninteresting from a parsing point of view,
                         ;; or because SMIE's parsing engine can't handle them
                         ;; even poorly.
                         '("break" "continue" "struct" "enum" "union" "static")
                         ;; "case" already handled above.
                         (delete "case" kws)))
                "\\_>"))
     (0 font-lock-keyword-face))
    (,elb-smie--defun-regexp
     (1
      (prog1 font-lock-function-name-face
        (if (< (match-beginning 0) (line-beginning-position))
            (put-text-property (match-beginning 0) (match-end 0)
                               'font-lock-multiline t)))))))

(defconst elb-smie--def-regexp
  (let ((spc0 (concat "\\(?:[ \t\n]\\|" elb-smie--comment-regexp "\\)*"))
        (id "\\(?:\\sw\\|\\s_\\)+"))
    (concat elb-smie--defun-regexp
            "\\|"
            "\\_<\\(?1:\\(?:struct\\|enum\\)[ \t]+" id "\\)" spc0 "{")))

;;;###autoload
(define-derived-mode elb-smie-mode prog-mode "smC"
  "C editing mode based on SMIE."
  ;; (setq-local font-lock-support-mode nil) ;; To help debugging.
  (setq-local comment-start "/* ")
  (setq-local comment-end " */")
  (setq-local parse-sexp-lookup-properties t)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local syntax-propertize-function #'elb-smie-syntax-propertize)
  (setq-local font-lock-defaults '(elb-smie-font-lock-keywords))
  (setq-local font-lock-syntactic-face-function #'elb-smie-syntactic-face-function)
  (smie-setup elb-smie-smie-grammar #'elb-smie-smie-rules
              :backward-token #'elb-smie-smie-backward-token
              :forward-token #'elb-smie-smie-forward-token)
  ;; FIXME: The stock SMIE forward-sexp-function is not good enough here, since
  ;; our grammar is much too poor.  We should setup another function instead
  ;; (or ideally teach SMIE to use it).
  (kill-local-variable 'forward-sexp-function)
  (add-hook 'smie-indent-functions #'elb-smie--cpp-smie-indent nil t)
  (add-function :after (local 'indent-line-function)
                #'elb-smie--cpp-indent-line)
  (setq-local smie--hanging-eolp-function #'elb-smie-smie-hanging-eolp)
  ;; Backslash auto-realign.
  (add-hook 'after-change-functions #'elb-smie--bs-after-change nil t)
  (add-hook 'post-command-hook #'elb-smie--bs-realign nil t)
  (setq-local imenu-generic-expression `((nil ,elb-smie--def-regexp 1))))

;;; The actual benchmark

(defun elb-smie-entry ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name
                           "../resources/xmenu.c" elb-bench-directory))
    (elb-smie-mode)
    (dotimes (_ 5)
      (indent-region (point-min) (point-max)))))

(provide 'elb-smie)
;;; elb-smie.el ends here
