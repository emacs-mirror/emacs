;;; cfengine3.el --- mode for editing Cfengine 3 files

;; Copyright (C) 2001-2011  Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: languages

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

;; Supports only cfengine 3, unlike the older cfengine.el which
;; supports 1.x and 2.x.

;; Possible customization for auto-mode selection:

;; (push '(("^cfagent.conf\\'" . cfengine3-mode)) auto-mode-alist)
;; (push '(("^cf\\." . cfengine3-mode)) auto-mode-alist)
;; (push '(("\\.cf\\'" . cfengine3-mode)) auto-mode-alist)

;;; Code:

(defgroup cfengine3 ()
  "Editing CFEngine 3 files."
  :group 'languages)

(defcustom cfengine3-indent 2
  "*Size of a CFEngine 3 indentation step in columns."
  :group 'cfengine3
  :type 'integer)

(eval-and-compile
  (defconst cfengine3-defuns
    (mapcar
     'symbol-name
     '(bundle body))
    "List of the CFEngine 3.x defun headings.")

  (defconst cfengine3-defuns-regex
    (regexp-opt cfengine3-defuns t)
    "Regex to match the CFEngine 3.x defuns.")

  (defconst cfengine3-class-selector-regex "\\([[:alnum:]_().&|!]+\\)::")

  (defconst cfengine3-category-regex "\\([[:alnum:]_]+\\):")

  (defconst cfengine3-vartypes
    (mapcar
     'symbol-name
     '(string int real slist ilist rlist irange rrange counter))
    "List of the CFEngine 3.x variable types."))

(defvar cfengine3-font-lock-keywords
  `(
    (,(concat "^[ \t]*" cfengine3-class-selector-regex)
     1 font-lock-keyword-face)
    (,(concat "^[ \t]*" cfengine3-category-regex)
     1 font-lock-builtin-face)
    ;; Variables, including scope, e.g. module.var
    ("[@$](\\([[:alnum:]_.]+\\))" 1 font-lock-variable-name-face)
    ("[@$]{\\([[:alnum:]_.]+\\)}" 1 font-lock-variable-name-face)
    ;; Variable definitions.
    ("\\<\\([[:alnum:]_]+\\)[ \t]*=[ \t]*(" 1 font-lock-variable-name-face)

    ;; CFEngine 3.x faces
    ;; defuns
    (,(concat "\\<" cfengine3-defuns-regex "\\>"
              "[ \t]+\\<\\([[:alnum:]_]+\\)\\>"
              "[ \t]+\\<\\([[:alnum:]_]+\\)\\((\\([^)]*\\))\\)?")
    (1 font-lock-builtin-face)
    (2 font-lock-constant-name-face)
    (3 font-lock-function-name-face)
    (5 font-lock-variable-name-face))
    ;; variable types
    (,(concat "\\<" (eval-when-compile (regexp-opt cfengine3-vartypes t)) "\\>")
     1 font-lock-type-face)))

(defun cfengine3-beginning-of-defun ()
  "`beginning-of-defun' function for Cfengine 3 mode.
Treats body/bundle blocks as defuns."
  (unless (<= (current-column) (current-indentation))
    (end-of-line))
  (if (re-search-backward (concat "^[ \t]*" cfengine3-defuns-regex "\\>") nil t)
      (beginning-of-line)
    (goto-char (point-min)))
  t)

(defun cfengine3-end-of-defun ()
  "`end-of-defun' function for Cfengine 3 mode.
Treats body/bundle blocks as defuns."
  (end-of-line)
  (if (re-search-forward (concat "^[ \t]*" cfengine3-defuns-regex "\\>") nil t)
      (beginning-of-line)
    (goto-char (point-max)))
  t)

(defun cfengine3-indent-line ()
  "Indent a line in Cfengine mode.
Intended as the value of `indent-line-function'."
  (let ((pos (- (point-max) (point)))
        parse)
    (save-restriction
      (narrow-to-defun)
      (back-to-indentation)
      (setq parse (parse-partial-sexp (point-min) (point)))
      (message "%S" parse)
      (cond
       ;; body/bundle blocks start at 0
       ((looking-at (concat cfengine3-defuns-regex "\\>"))
        (indent-line-to 0))
       ;; categories are indented one step
       ((looking-at (concat cfengine3-category-regex "[ \t]*$"))
        (indent-line-to cfengine3-indent))
       ;; class selectors are indented two steps
       ((looking-at (concat cfengine3-class-selector-regex "[ \t]*$"))
        (indent-line-to (* 2 cfengine3-indent)))
       ;; Outdent leading close brackets one step.
       ((or (eq ?\} (char-after))
            (eq ?\) (char-after)))
        (condition-case ()
            (indent-line-to (save-excursion
                              (forward-char)
                              (backward-sexp)
                              (current-column)))
          (error nil)))
       ;; inside a string and it starts before this line
       ((and (nth 3 parse)
             (< (nth 8 parse) (save-excursion (beginning-of-line) (point))))
        (indent-line-to 0))
       ;; inside a defun, but not a nested list (depth is 1)
       ((= 1 (nth 0 parse))
        (indent-line-to (* (+ 2 (nth 0 parse)) cfengine3-indent)))
       ;; Inside brackets/parens: indent to start column of non-comment
       ;; token on line following open bracket or by one step from open
       ;; bracket's column.
       ((condition-case ()
            (progn (indent-line-to (save-excursion
                                     (backward-up-list)
                                     (forward-char)
                                     (skip-chars-forward " \t")
                                     (cond
                                      ((looking-at "[^\n#]")
                                       (current-column))
                                      ((looking-at "[^\n#]")
                                       (current-column))
                                      (t
                                       (skip-chars-backward " \t")
                                       (+ (current-column) -1
                                          cfengine3-indent)))))
                   t)
          (error nil)))
       ;; Else don't indent.
       (t (indent-line-to 0))))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

;; (defvar cfengine3-smie-grammar
;;   (smie-prec2->grammar
;;    (smie-merge-prec2s
;;    (smie-bnf->prec2
;;     '((token)
;;       (decls (decls "body" decls)
;;              (decls "bundle" decls))
;;       (insts (token ":" insts)))
;;     '((assoc "body" "bundle")))
;;    (smie-precs->prec2
;;     '((right ":")
;;       (right "::")
;;       (assoc ";")
;;       (assoc ",")
;;       (right "=>"))))))

;; (defun cfengine3-smie-rules (kind token)
;;   (pcase (cons kind token)
;;     (`(:elem . basic) 2)
;;     (`(:list-intro . ,(or `"body" `"bundle")) t)
;;     (`(:after . ":") 2)
;;     (`(:after . "::") 2)))

;; (defun cfengine3-show-all-tokens ()
;;   (interactive)
;;   (goto-char (point-min))
;;   (while (not (eobp))
;;     (let* ((p (point))
;;            (token (funcall smie-forward-token-function)))
;;       (delete-region p (point))
;;       (insert-before-markers token)
;;       (forward-char))))

;; (defun cfengine3-line-classes ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (let* ((todo (buffer-substring (point)
;;                                    (save-excursion (end-of-line) (point))))
;;            (original (concat (loop for c across todo
;;                                    collect (char-syntax c)))))
;;       (format "%s\n%s" original todo))))

;; (defun cfengine3-show-all-classes ()
;;   (interactive)
;;   (goto-char (point-min))
;;   (while (not (eobp))
;;     (let ((repl (cfengine3-line-classes)))
;;       (kill-line)
;;       (insert repl)
;;       (insert "\n"))))

;; specification: blocks
;; blocks: block | blocks block;
;; block:                 bundle typeid blockid bundlebody
;;                      | bundle typeid blockid usearglist bundlebody
;;                      | body typeid blockid bodybody
;;                      | body typeid blockid usearglist bodybody;

;; typeid: id
;; blockid: id
;; usearglist: '(' aitems ')';
;; aitems: aitem | aitem ',' aitems |;
;; aitem: id

;; bundlebody: '{' statements '}'
;; statements: statement | statements statement;
;; statement: category | classpromises;

;; bodybody: '{' bodyattribs '}'
;; bodyattribs: bodyattrib | bodyattribs bodyattrib;
;; bodyattrib: class | selections;
;; selections: selection | selections selection;
;; selection: id ASSIGN rval ';' ;

;; classpromises: classpromise | classpromises classpromise;
;; classpromise: class | promises;
;; promises: promise | promises promise;
;; category: CATEGORY
;; promise: promiser ARROW rval constraints ';' | promiser constraints ';';
;; constraints: constraint | constraints ',' constraint |;
;; constraint: id ASSIGN rval;
;; class: CLASS
;; id: ID
;; rval: ID | QSTRING | NAKEDVAR | list | usefunction
;; list: '{' litems '}' ;
;; litems: litem | litem ',' litems |;
;; litem: ID | QSTRING | NAKEDVAR | list | usefunction

;; functionid: ID | NAKEDVAR
;; promiser: QSTRING
;; usefunction: functionid givearglist
;; givearglist: '(' gaitems ')'
;; gaitems: gaitem | gaitems ',' gaitem |;
;; gaitem: ID | QSTRING | NAKEDVAR | list | usefunction

;; # from lexer:

;; bundle: "bundle"
;; body: "body"
;; COMMENT    #[^\n]*
;; NAKEDVAR   [$@][(][a-zA-Z0-9_\200-\377.]+[)]|[$@][{][a-zA-Z0-9_\200-\377.]+[}]
;; ID: [a-zA-Z0-9_\200-\377]+
;; ASSIGN: "=>"
;; ARROW: "->"
;; QSTRING: \"((\\\")|[^"])*\"|\'((\\\')|[^'])*\'|`[^`]*`
;; CLASS: [.|&!()a-zA-Z0-9_\200-\377]+::
;; CATEGORY: [a-zA-Z_]+:

;;;###autoload
(define-derived-mode cfengine3-mode prog-mode "CFEngine3"
  "Major mode for editing cfengine input.
There are no special keybindings by default.

Action blocks are treated as defuns, i.e. \\[beginning-of-defun] moves
to the action header."
  (modify-syntax-entry ?# "<" cfengine3-mode-syntax-table)
  (modify-syntax-entry ?\n ">#" cfengine3-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" cfengine3-mode-syntax-table)
  ;; variable substitution:
  (modify-syntax-entry ?$ "." cfengine3-mode-syntax-table)
  ;; Doze path separators:
  (modify-syntax-entry ?\\ "." cfengine3-mode-syntax-table)
  ;; Otherwise, syntax defaults seem OK to give reasonable word
  ;; movement.

  ;; (smie-setup cfengine3-smie-grammar #'cfengine3-smie-rules)
  ;;             ;; :forward-token  #'cfengine3-smie-forward-token
  ;;             ;; :backward-token #'cfengine3-smie-backward-token)
  ;; (set (make-local-variable 'smie-indent-basic) 'cfengine3-indent)

  (set (make-local-variable 'parens-require-spaces) nil)
  (set (make-local-variable 'comment-start)  "# ")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(?:^\\|[^\\\\\n]\\)\\(?:\\\\\\\\\\)*\\)#+[ \t]*")
  (set (make-local-variable 'indent-line-function) #'cfengine3-indent-line)
  (setq font-lock-defaults
        '(cfengine3-font-lock-keywords nil nil nil beginning-of-defun))
  ;; Fixme: set the args of functions in evaluated classes to string
  ;; syntax, and then obey syntax properties.
  (set (make-local-variable 'syntax-propertize-function)
       ;; In the main syntax-table, \ is marked as a punctuation, because
       ;; of its use in DOS-style directory separators.  Here we try to
       ;; recognize the cases where \ is used as an escape inside strings.
       (syntax-propertize-rules ("\\(\\(?:\\\\\\)+\\)\"" (1 "\\"))))

  ;; use defuns as the essential syntax block
  (set (make-local-variable 'beginning-of-defun-function)
       #'cfengine3-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'cfengine3-end-of-defun)

  ;; Like Lisp mode.  Without this, we lose with, say,
  ;; `backward-up-list' when there's an unbalanced quote in a
  ;; preceding comment.
  (set (make-local-variable 'parse-sexp-ignore-comments) t))

(provide 'cfengine3)

;;; cfengine3.el ends here
