;;; octave.el --- editing octave source files under emacs   -*- lexical-binding: t; -*-

;; Copyright (C) 1997, 2001-2013 Free Software Foundation, Inc.

;; Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
;;	   John Eaton <jwe@octave.org>
;; Maintainer: FSF
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

;; This package provides emacs support for octave.  It defines a major
;; mode for editing octave code and contains code for interacting with
;; an inferior octave process using comint.

;; See the documentation of `octave-mode' and `run-octave' for further
;; information on usage and customization.

;;; Code:
(require 'comint)

(defgroup octave nil
  "Editing Octave code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(define-obsolete-function-alias 'octave-submit-bug-report
  'report-emacs-bug "24.4")

(define-abbrev-table 'octave-abbrev-table nil
  "Abbrev table for Octave's reserved words.
Used in `octave-mode' and `inferior-octave-mode' buffers.")

(defvar octave-comment-char ?#
  "Character to start an Octave comment.")

(defvar octave-comment-start
  (string octave-comment-char ?\s)
  "String to insert to start a new Octave in-line comment.")

(defvar octave-comment-start-skip "\\s<+\\s-*"
  "Regexp to match the start of an Octave comment up to its body.")

(defvar octave-begin-keywords
  '("do" "for" "function" "if" "switch" "try" "unwind_protect" "while"))

(defvar octave-else-keywords
  '("case" "catch" "else" "elseif" "otherwise" "unwind_protect_cleanup"))

(defvar octave-end-keywords
  '("endfor" "endfunction" "endif" "endswitch" "end_try_catch"
    "end_unwind_protect" "endwhile" "until" "end"))

(defvar octave-reserved-words
  (append octave-begin-keywords
	  octave-else-keywords
	  octave-end-keywords
	  '("break" "continue" "end" "global" "persistent" "return"))
  "Reserved words in Octave.")

(defvar octave-text-functions
  '("casesen" "cd" "chdir" "clear" "diary" "dir" "document" "echo"
    "edit_history" "format" "help" "history" "hold"
    "load" "ls" "more" "run_history" "save" "type"
    "which" "who" "whos")
  "Text functions in Octave.")

(defvar octave-function-header-regexp
  (concat "^\\s-*\\_<\\(function\\)\\_>"
	  "\\([^=;\n]*=[ \t]*\\|[ \t]*\\)\\(\\(?:\\w\\|\\s_\\)+\\)\\_>")
  "Regexp to match an Octave function header.
The string `function' and its name are given by the first and third
parenthetical grouping.")


(defvar octave-font-lock-keywords
  (list
   ;; Fontify all builtin keywords.
   (cons (concat "\\_<\\("
		 (regexp-opt (append octave-reserved-words
                                     octave-text-functions))
		 "\\)\\_>")
	 'font-lock-keyword-face)
   ;; Note: 'end' also serves as the last index in an indexing expression.
   ;; Ref: http://www.mathworks.com/help/matlab/ref/end.html
   '("\\_<end\\_>" (0 (save-excursion
                        (condition-case nil
                            (progn
                              (goto-char (match-beginning 0))
                              (backward-up-list)
                              (unless (eq (char-after) ?\()
                                font-lock-keyword-face))
                          (error font-lock-keyword-face)))
                      t))
   ;; Fontify all builtin operators.
   (cons "\\(&\\||\\|<=\\|>=\\|==\\|<\\|>\\|!=\\|!\\)"
	 (if (boundp 'font-lock-builtin-face)
	     'font-lock-builtin-face
	   'font-lock-preprocessor-face))
   ;; Fontify all function declarations.
   (list octave-function-header-regexp
	 '(1 font-lock-keyword-face)
	 '(3 font-lock-function-name-face nil t)))
  "Additional Octave expressions to highlight.")

(defun octave-syntax-propertize-function (start end)
  (goto-char start)
  (octave-syntax-propertize-sqs end)
  (funcall (syntax-propertize-rules
            ;; Try to distinguish the string-quotes from the transpose-quotes.
            ("[[({,; ]\\('\\)"
             (1 (prog1 "\"'" (octave-syntax-propertize-sqs end)))))
           (point) end))

(defun octave-syntax-propertize-sqs (end)
  "Propertize the content/end of single-quote strings."
  (when (eq (nth 3 (syntax-ppss)) ?\')
    ;; A '..' string.
    (when (re-search-forward
           "\\(?:\\=\\|[^']\\)\\(?:''\\)*\\('\\)\\($\\|[^']\\)" end 'move)
      (goto-char (match-beginning 2))
      (when (eq (char-before (match-beginning 1)) ?\\)
        ;; Backslash cannot escape a single quote.
        (put-text-property (1- (match-beginning 1)) (match-beginning 1)
                           'syntax-table (string-to-syntax ".")))
      (put-text-property (match-beginning 1) (match-end 1)
                         'syntax-table (string-to-syntax "\"'")))))


(defvar octave-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\n" 'octave-indent-new-comment-line)
    (define-key map "\M-\C-q" 'octave-indent-defun)
    (define-key map "\C-c\C-p" 'octave-previous-code-line)
    (define-key map "\C-c\C-n" 'octave-next-code-line)
    (define-key map "\C-c\C-a" 'octave-beginning-of-line)
    (define-key map "\C-c\C-e" 'octave-end-of-line)
    (define-key map [remap down-list] 'smie-down-list)
    (define-key map "\C-c\M-\C-h" 'octave-mark-block)
    (define-key map "\C-c]" 'smie-close-block)
    (define-key map "\C-c/" 'smie-close-block)
    (define-key map "\C-c;" 'octave-update-function-file-comment)
    (define-key map "\C-c\C-f" 'octave-insert-defun)
    (define-key map "\C-c\C-il" 'octave-send-line)
    (define-key map "\C-c\C-ib" 'octave-send-block)
    (define-key map "\C-c\C-if" 'octave-send-defun)
    (define-key map "\C-c\C-ir" 'octave-send-region)
    (define-key map "\C-c\C-is" 'octave-show-process-buffer)
    (define-key map "\C-c\C-iq" 'octave-hide-process-buffer)
    (define-key map "\C-c\C-ik" 'octave-kill-process)
    (define-key map "\C-c\C-i\C-l" 'octave-send-line)
    (define-key map "\C-c\C-i\C-b" 'octave-send-block)
    (define-key map "\C-c\C-i\C-f" 'octave-send-defun)
    (define-key map "\C-c\C-i\C-r" 'octave-send-region)
    (define-key map "\C-c\C-i\C-s" 'octave-show-process-buffer)
    (define-key map "\C-c\C-i\C-q" 'octave-hide-process-buffer)
    (define-key map "\C-c\C-i\C-k" 'octave-kill-process)
    map)
  "Keymap used in Octave mode.")



(easy-menu-define octave-mode-menu octave-mode-map
  "Menu for Octave mode."
  '("Octave"
    ("Lines"
      ["Previous Code Line"	octave-previous-code-line t]
      ["Next Code Line"		octave-next-code-line t]
      ["Begin of Continuation"	octave-beginning-of-line t]
      ["End of Continuation"	octave-end-of-line t]
      ["Split Line at Point"	octave-indent-new-comment-line t])
    ("Blocks"
      ["Mark Block"		octave-mark-block t]
      ["Close Block"		smie-close-block t])
    ("Functions"
      ["Indent Function"	octave-indent-defun t]
      ["Insert Function"	octave-insert-defun t]
      ["Update function file comment" octave-update-function-file-comment t])
    "-"
    ("Debug"
      ["Send Current Line"	octave-send-line t]
      ["Send Current Block"	octave-send-block t]
      ["Send Current Function"	octave-send-defun t]
      ["Send Region"		octave-send-region t]
      ["Show Process Buffer"	octave-show-process-buffer t]
      ["Hide Process Buffer"	octave-hide-process-buffer t]
      ["Kill Process"		octave-kill-process t])
    "-"
    ["Indent Line"		indent-according-to-mode t]
    ["Complete Symbol"		completion-at-point t]
    ["Toggle Auto-Fill Mode"	auto-fill-mode
     :style toggle :selected auto-fill-function]
    "-"
    ["Describe Octave Mode"	describe-mode t]
    ["Lookup Octave Index"	info-lookup-symbol t]
    ["Customize Octave" (customize-group 'octave) t]
    "-"
    ["Submit Bug Report"	report-emacs-bug t]))

(defvar octave-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\r " "  table)
    (modify-syntax-entry ?+ "."   table)
    (modify-syntax-entry ?- "."   table)
    (modify-syntax-entry ?= "."   table)
    (modify-syntax-entry ?* "."   table)
    (modify-syntax-entry ?/ "."   table)
    (modify-syntax-entry ?> "."   table)
    (modify-syntax-entry ?< "."   table)
    (modify-syntax-entry ?& "."   table)
    (modify-syntax-entry ?| "."   table)
    (modify-syntax-entry ?! "."   table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\' "."  table)
    ;; Was "w" for abbrevs, but now that it's not necessary any more,
    (modify-syntax-entry ?\` "."  table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?. "_"   table)
    (modify-syntax-entry ?_ "_"   table)
    ;; The "b" flag only applies to the second letter of the comstart
    ;; and the first letter of the comend, i.e. the "4b" below is ineffective.
    ;; If we try to put `b' on the single-line comments, we get a similar
    ;; problem where the % and # chars appear as first chars of the 2-char
    ;; comend, so the multi-line ender is also turned into style-b.
    ;; So we need the new "c" comment style.
    (modify-syntax-entry ?\% "< 13"  table)
    (modify-syntax-entry ?\# "< 13"  table)
    (modify-syntax-entry ?\{ "(} 2c"  table)
    (modify-syntax-entry ?\} "){ 4c"  table)
    (modify-syntax-entry ?\n ">"  table)
    table)
  "Syntax table in use in `octave-mode' buffers.")

(defcustom octave-font-lock-texinfo-comment t
  "Control whether to highlight the texinfo comment block."
  :type 'boolean
  :group 'octave
  :version "24.4")

(defcustom octave-blink-matching-block t
  "Control the blinking of matching Octave block keywords.
Non-nil means show matching begin of block when inserting a space,
newline or semicolon after an else or end keyword."
  :type 'boolean
  :group 'octave)

(defcustom octave-block-offset 2
  "Extra indentation applied to statements in Octave block structures."
  :type 'integer
  :group 'octave)

(defvar octave-block-comment-start
  (concat (make-string 2 octave-comment-char) " ")
  "String to insert to start a new Octave comment on an empty line.")

(defcustom octave-continuation-offset 4
  "Extra indentation applied to Octave continuation lines."
  :type 'integer
  :group 'octave)

(eval-and-compile
  (defconst octave-continuation-marker-regexp "\\\\\\|\\.\\.\\."))

(defvar octave-continuation-regexp
  (concat "[^#%\n]*\\(" octave-continuation-marker-regexp
          "\\)\\s-*\\(\\s<.*\\)?$"))

;; Char \ is considered a bad decision for continuing a line.
(defconst octave-continuation-string "..."
  "Character string used for Octave continuation lines.")

(defvar octave-mode-imenu-generic-expression
  (list
   ;; Functions
   (list nil octave-function-header-regexp 3))
  "Imenu expression for Octave mode.  See `imenu-generic-expression'.")

(defcustom octave-mode-hook nil
  "Hook to be run when Octave mode is started."
  :type 'hook
  :group 'octave)

(defcustom octave-send-show-buffer t
  "Non-nil means display `inferior-octave-buffer' after sending to it."
  :type 'boolean
  :group 'octave)

(defcustom octave-send-line-auto-forward t
  "Control auto-forward after sending to the inferior Octave process.
Non-nil means always go to the next Octave code line after sending."
  :type 'boolean
  :group 'octave)

(defcustom octave-send-echo-input t
  "Non-nil means echo input sent to the inferior Octave process."
  :type 'boolean
  :group 'octave)


;;; SMIE indentation

(require 'smie)

(defconst octave-operator-table
  '((assoc ";" "\n") (assoc ",") ; The doc claims they have equal precedence!?
    (right "=" "+=" "-=" "*=" "/=")
    (assoc "&&") (assoc "||") ; The doc claims they have equal precedence!?
    (assoc "&") (assoc "|")   ; The doc claims they have equal precedence!?
    (nonassoc "<" "<=" "==" ">=" ">" "!=" "~=")
    (nonassoc ":")                      ;No idea what this is.
    (assoc "+" "-")
    (assoc "*" "/" "\\" ".\\" ".*" "./")
    (nonassoc "'" ".'")
    (nonassoc "++" "--" "!" "~")        ;And unary "+" and "-".
    (right "^" "**" ".^" ".**")
    ;; It's not really an operator, but for indentation purposes it
    ;; could be convenient to treat it as one.
    (assoc "...")))

(defconst octave-smie-bnf-table
  '((atom)
    ;; We can't distinguish the first element in a sequence with
    ;; precedence grammars, so we can't distinguish the condition
    ;; if the `if' from the subsequent body, for example.
    ;; This has to be done later in the indentation rules.
    (exp (exp "\n" exp)
         ;; We need to mention at least one of the operators in this part
         ;; of the grammar: if the BNF and the operator table have
         ;; no overlap, SMIE can't know how they relate.
         (exp ";" exp)
         ("try" exp "catch" exp "end_try_catch")
         ("try" exp "catch" exp "end")
         ("unwind_protect" exp
          "unwind_protect_cleanup" exp "end_unwind_protect")
         ("unwind_protect" exp "unwind_protect_cleanup" exp "end")
         ("for" exp "endfor")
         ("for" exp "end")
         ("do" exp "until" atom)
         ("while" exp "endwhile")
         ("while" exp "end")
         ("if" exp "endif")
         ("if" exp "else" exp "endif")
         ("if" exp "elseif" exp "else" exp "endif")
         ("if" exp "elseif" exp "elseif" exp "else" exp "endif")
         ("if" exp "elseif" exp "elseif" exp "else" exp "end")
         ("switch" exp "case" exp "endswitch")
         ("switch" exp "case" exp "otherwise" exp "endswitch")
         ("switch" exp "case" exp "case" exp "otherwise" exp "endswitch")
         ("switch" exp "case" exp "case" exp "otherwise" exp "end")
         ("function" exp "endfunction")
         ("function" exp "end"))
    ;; (fundesc (atom "=" atom))
    ))

(defconst octave-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2 octave-smie-bnf-table
                     '((assoc "\n" ";")))

    (smie-precs->prec2 octave-operator-table))))

;; Tokenizing needs to be refined so that ";;" is treated as two
;; tokens and also so as to recognize the \n separator (and
;; corresponding continuation lines).

(defconst octave-operator-regexp
  (regexp-opt (apply 'append (mapcar 'cdr octave-operator-table))))

(defun octave-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (not (eq (char-before) ?\;)) ;Coalesce ";" and "\n".
           (> pos (line-end-position))
           (if (looking-back octave-continuation-marker-regexp (- (point) 3))
               (progn
                 (goto-char (match-beginning 0))
                 (forward-comment (- (point)))
                 nil)
             t)
           ;; Ignore it if it's within parentheses.
           (let ((ppss (syntax-ppss)))
             (not (and (nth 1 ppss)
                       (eq ?\( (char-after (nth 1 ppss)))))))
      (skip-chars-forward " \t")
      ;; Why bother distinguishing \n and ;?
      ";") ;;"\n"
     ((and (looking-back octave-operator-regexp (- (point) 3) 'greedy)
           ;; Don't mistake a string quote for a transpose.
           (not (looking-back "\\s\"" (1- (point)))))
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     (t
      (smie-default-backward-token)))))

(defun octave-smie-forward-token ()
  (skip-chars-forward " \t")
  (when (looking-at (eval-when-compile
                      (concat "\\(" octave-continuation-marker-regexp
                              "\\)[ \t]*\\($\\|[%#]\\)")))
    (goto-char (match-end 1))
    (forward-comment 1))
  (cond
   ((and (looking-at "$\\|[%#]")
         ;; Ignore it if it's within parentheses or if the newline does not end
         ;; some preceding text.
         (prog1 (and (not (smie-rule-bolp))
		     (let ((ppss (syntax-ppss)))
		       (not (and (nth 1 ppss)
				 (eq ?\( (char-after (nth 1 ppss)))))))
           (forward-comment (point-max))))
    ;; Why bother distinguishing \n and ;?
    ";") ;;"\n"
   ((looking-at ";[ \t]*\\($\\|[%#]\\)")
    ;; Combine the ; with the subsequent \n.
    (goto-char (match-beginning 1))
    (forward-comment 1)
    ";")
   ((and (looking-at octave-operator-regexp)
         ;; Don't mistake a string quote for a transpose.
         (not (looking-at "\\s\"")))
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   (t
    (smie-default-forward-token))))

(defun octave-smie-rules (kind token)
  (pcase (cons kind token)
    ;; We could set smie-indent-basic instead, but that would have two
    ;; disadvantages:
    ;; - changes to octave-block-offset wouldn't take effect immediately.
    ;; - edebug wouldn't show the use of this variable.
    (`(:elem . basic) octave-block-offset)
    ;; Since "case" is in the same BNF rules as switch..end, SMIE by default
    ;; aligns it with "switch".
    (`(:before . "case") (if (not (smie-rule-sibling-p)) octave-block-offset))
    (`(:after . ";")
     (if (smie-rule-parent-p "function" "if" "while" "else" "elseif" "for"
                             "otherwise" "case" "try" "catch" "unwind_protect"
                             "unwind_protect_cleanup")
         (smie-rule-parent octave-block-offset)
       ;; For (invalid) code between switch and case.
       ;; (if (smie-parent-p "switch") 4)
       0))))

(defvar electric-layout-rules)

;;;###autoload
(define-derived-mode octave-mode prog-mode "Octave"
  "Major mode for editing Octave code.

Octave is a high-level language, primarily intended for numerical
computations.  It provides a convenient command line interface
for solving linear and nonlinear problems numerically.  Function
definitions can also be stored in files and used in batch mode."
  :abbrev-table octave-abbrev-table

  (smie-setup octave-smie-grammar #'octave-smie-rules
              :forward-token  #'octave-smie-forward-token
              :backward-token #'octave-smie-backward-token)
  (setq-local smie-indent-basic 'octave-block-offset)

  (setq-local smie-blink-matching-triggers
              (cons ?\; smie-blink-matching-triggers))
  (unless octave-blink-matching-block
    (remove-hook 'post-self-insert-hook #'smie-blink-matching-open 'local))

  (setq-local electric-indent-chars
              (cons ?\; electric-indent-chars))
  ;; IIUC matlab-mode takes the opposite approach: it makes RET insert
  ;; a ";" at those places where it's correct (i.e. outside of parens).
  (setq-local electric-layout-rules '((?\; . after)))

  (setq-local comment-start octave-comment-start)
  (setq-local comment-end "")
  ;; Don't set it here: it's not really a property of the language,
  ;; just a personal preference of the author.
  ;; (setq-local comment-column 32)
  (setq-local comment-start-skip "\\s<+\\s-*")
  (setq-local comment-add 1)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local paragraph-start (concat "\\s-*$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'octave-fill-paragraph)
  ;; FIXME: Why disable it?
  ;; (setq-local adaptive-fill-regexp nil)
  ;; Again, this is not a property of the language, don't set it here.
  ;; (setq fill-column 72)
  (setq-local normal-auto-fill-function 'octave-auto-fill)

  (setq font-lock-defaults '(octave-font-lock-keywords))

  (setq-local syntax-propertize-function #'octave-syntax-propertize-function)

  (setq imenu-generic-expression octave-mode-imenu-generic-expression)
  (setq imenu-case-fold-search nil)

  (add-hook 'completion-at-point-functions
            'octave-completion-at-point-function nil t)
  (add-hook 'before-save-hook 'octave-sync-function-file-names nil t)
  (setq-local beginning-of-defun-function 'octave-beginning-of-defun)
  (and octave-font-lock-texinfo-comment (octave-font-lock-texinfo-comment))

  (easy-menu-add octave-mode-menu))


(defcustom inferior-octave-program "octave"
  "Program invoked by `inferior-octave'."
  :type 'string
  :group 'octave)

(defcustom inferior-octave-buffer "*Inferior Octave*"
  "Name of buffer for running an inferior Octave process."
  :type 'string
  :group 'octave)

(defcustom inferior-octave-prompt
  "\\(^octave\\(\\|.bin\\|.exe\\)\\(-[.0-9]+\\)?\\(:[0-9]+\\)?\\|^debug\\|^\\)>+ "
  "Regexp to match prompts for the inferior Octave process."
  :type 'regexp
  :group 'octave)

(defcustom inferior-octave-prompt-read-only comint-prompt-read-only
  "If non-nil, the Octave prompt is read only.
See `comint-prompt-read-only' for details."
  :type 'boolean
  :group 'octave
  :version "24.4")

(defcustom inferior-octave-startup-file nil
  "Name of the inferior Octave startup file.
The contents of this file are sent to the inferior Octave process on
startup."
  :type '(choice (const :tag "None" nil)
		 file)
  :group 'octave)

(defcustom inferior-octave-startup-args nil
  "List of command line arguments for the inferior Octave process.
For example, for suppressing the startup message and using `traditional'
mode, set this to (\"-q\" \"--traditional\")."
  :type '(repeat string)
  :group 'octave)

(defcustom inferior-octave-mode-hook nil
  "Hook to be run when Inferior Octave mode is started."
  :type 'hook
  :group 'octave)

(defvar inferior-octave-process nil)

(defvar inferior-octave-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\M-?" 'comint-dynamic-list-filename-completions)
    (define-key map "\C-c\C-l" 'inferior-octave-dynamic-list-input-ring)
    (define-key map [menu-bar inout list-history]
      '("List Input History" . inferior-octave-dynamic-list-input-ring))
    map)
  "Keymap used in Inferior Octave mode.")

(defvar inferior-octave-mode-syntax-table
  (let ((table (make-syntax-table octave-mode-syntax-table)))
    table)
  "Syntax table in use in inferior-octave-mode buffers.")

(defvar inferior-octave-font-lock-keywords
  (list
   (cons inferior-octave-prompt 'font-lock-type-face))
  ;; Could certainly do more font locking in inferior Octave ...
  "Additional expressions to highlight in Inferior Octave mode.")


;;; Compatibility functions
(if (not (fboundp 'comint-line-beginning-position))
    ;; comint-line-beginning-position is defined in Emacs 21
    (defun comint-line-beginning-position ()
      "Returns the buffer position of the beginning of the line, after any prompt.
The prompt is assumed to be any text at the beginning of the line matching
the regular expression `comint-prompt-regexp', a buffer local variable."
      (save-excursion (comint-bol nil) (point))))


(defvar inferior-octave-output-list nil)
(defvar inferior-octave-output-string nil)
(defvar inferior-octave-receive-in-progress nil)

(define-obsolete-variable-alias 'inferior-octave-startup-hook
  'inferior-octave-mode-hook "24.4")

(defvar inferior-octave-has-built-in-variables nil
  "Non-nil means that Octave has built-in variables.")

(defvar inferior-octave-dynamic-complete-functions
  '(inferior-octave-completion-at-point comint-filename-completion)
  "List of functions called to perform completion for inferior Octave.
This variable is used to initialize `comint-dynamic-complete-functions'
in the Inferior Octave buffer.")

(defvar info-lookup-mode)

(define-derived-mode inferior-octave-mode comint-mode "Inferior Octave"
  "Major mode for interacting with an inferior Octave process."
  :abbrev-table octave-abbrev-table
  (setq comint-prompt-regexp inferior-octave-prompt)

  (setq-local comment-start octave-comment-start)
  (setq-local comment-end "")
  (setq comment-column 32)
  (setq-local comment-start-skip octave-comment-start-skip)

  (setq font-lock-defaults '(inferior-octave-font-lock-keywords nil nil))

  (setq info-lookup-mode 'octave-mode)

  (setq comint-input-ring-file-name
	(or (getenv "OCTAVE_HISTFILE") "~/.octave_hist")
	comint-input-ring-size (or (getenv "OCTAVE_HISTSIZE") 1024))
  (setq-local comint-dynamic-complete-functions
              inferior-octave-dynamic-complete-functions)
  (setq-local comint-prompt-read-only inferior-octave-prompt-read-only)
  (add-hook 'comint-input-filter-functions
	'inferior-octave-directory-tracker nil t)
  (comint-read-input-ring t))

;;;###autoload
(defun inferior-octave (&optional arg)
  "Run an inferior Octave process, I/O via `inferior-octave-buffer'.
This buffer is put in Inferior Octave mode.  See `inferior-octave-mode'.

Unless ARG is non-nil, switches to this buffer.

The elements of the list `inferior-octave-startup-args' are sent as
command line arguments to the inferior Octave process on startup.

Additional commands to be executed on startup can be provided either in
the file specified by `inferior-octave-startup-file' or by the default
startup file, `~/.emacs-octave'."
  (interactive "P")
  (let ((buffer inferior-octave-buffer))
    (get-buffer-create buffer)
    (if (comint-check-proc buffer)
	()
      (with-current-buffer buffer
	(comint-mode)
	(inferior-octave-startup)
	(inferior-octave-mode)))
    (if (not arg)
	(pop-to-buffer buffer))))

;;;###autoload
(defalias 'run-octave 'inferior-octave)

(defun inferior-octave-startup ()
  "Start an inferior Octave process."
  (let ((proc (comint-exec-1
	       (substring inferior-octave-buffer 1 -1)
	       inferior-octave-buffer
	       inferior-octave-program
	       (append (list "-i" "--no-line-editing")
		       inferior-octave-startup-args))))
    (set-process-filter proc 'inferior-octave-output-digest)
    (setq comint-ptyp process-connection-type
	  inferior-octave-process proc
	  inferior-octave-output-list nil
	  inferior-octave-output-string nil
	  inferior-octave-receive-in-progress t)

    ;; This may look complicated ... However, we need to make sure that
    ;; we additional startup code only AFTER Octave is ready (otherwise,
    ;; output may be mixed up).  Hence, we need to digest the Octave
    ;; output to see when it issues a prompt.
    (while inferior-octave-receive-in-progress
      (accept-process-output inferior-octave-process))
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (insert-before-markers
     (concat
      (if (not (bobp)) "\n")
      (if inferior-octave-output-list
	  (concat (mapconcat
		   'identity inferior-octave-output-list "\n")
		  "\n"))))

     ;; Find out whether Octave has built-in variables.
     (inferior-octave-send-list-and-digest
      (list "exist \"LOADPATH\"\n"))
     (setq inferior-octave-has-built-in-variables
 	  (string-match "101$" (car inferior-octave-output-list)))

    ;; An empty secondary prompt, as e.g. obtained by '--braindead',
    ;; means trouble.
    (inferior-octave-send-list-and-digest (list "PS2\n"))
    (if (string-match "\\(PS2\\|ans\\) = *$" (car inferior-octave-output-list))
 	(inferior-octave-send-list-and-digest
 	 (list (if inferior-octave-has-built-in-variables
 		   "PS2 = \"> \"\n"
 		 "PS2 (\"> \");\n"))))

    ;; O.k., now we are ready for the Inferior Octave startup commands.
    (let* (commands
	   (program (file-name-nondirectory inferior-octave-program))
	   (file (or inferior-octave-startup-file
			  (concat "~/.emacs-" program))))
      (setq commands
	    (list "more off;\n"
		  (if (not (string-equal
			    inferior-octave-output-string ">> "))
		      (if inferior-octave-has-built-in-variables
			  "PS1=\"\\\\s> \";\n"
			"PS1 (\"\\\\s> \");\n"))
		  (if (file-exists-p file)
		      (format "source (\"%s\");\n" file))))
      (inferior-octave-send-list-and-digest commands))
    (insert-before-markers
     (concat
      (if inferior-octave-output-list
	  (concat (mapconcat
		   'identity inferior-octave-output-list "\n")
		  "\n"))
      inferior-octave-output-string))

    ;; And finally, everything is back to normal.
    (set-process-filter proc 'inferior-octave-output-filter)
    ;; Just in case, to be sure a cd in the startup file
    ;; won't have detrimental effects.
    (inferior-octave-resync-dirs)))

(defun inferior-octave-completion-table ()
  (completion-table-dynamic
   (lambda (command)
     (inferior-octave-send-list-and-digest
      (list (concat "completion_matches (\"" command "\");\n")))
     (sort (delete-dups inferior-octave-output-list)
           'string-lessp))))

(defun inferior-octave-completion-at-point ()
  "Return the data to complete the Octave symbol at point."
  (let* ((end (point))
	 (start
	  (save-excursion
	    (skip-syntax-backward "w_" (comint-line-beginning-position))
            (point))))
    (when (> end start)
      (list start end (inferior-octave-completion-table)))))

(define-obsolete-function-alias 'inferior-octave-complete
  'completion-at-point "24.1")

(defun inferior-octave-dynamic-list-input-ring ()
  "List the buffer's input history in a help buffer."
  ;; We cannot use `comint-dynamic-list-input-ring', because it replaces
  ;; "completion" by "history reference" ...
  (interactive)
  (if (or (not (ring-p comint-input-ring))
          (ring-empty-p comint-input-ring))
      (message "No history")
    (let ((history nil)
          (history-buffer " *Input History*")
          (index (1- (ring-length comint-input-ring)))
          (conf (current-window-configuration)))
      ;; We have to build up a list ourselves from the ring vector.
      (while (>= index 0)
        (setq history (cons (ring-ref comint-input-ring index) history)
              index (1- index)))
      ;; Change "completion" to "history reference"
      ;; to make the display accurate.
      (with-output-to-temp-buffer history-buffer
        (display-completion-list history)
        (set-buffer history-buffer))
      (message "Hit space to flush")
      (let ((ch (read-event)))
        (if (eq ch ?\ )
            (set-window-configuration conf)
          (setq unread-command-events (list ch)))))))

(defun inferior-octave-strip-ctrl-g (string)
  "Strip leading `^G' character.
If STRING starts with a `^G', ring the bell and strip it."
  (if (string-match "^\a" string)
      (progn
        (ding)
        (setq string (substring string 1))))
  string)

(defun inferior-octave-output-filter (proc string)
  "Standard output filter for the inferior Octave process.
Ring Emacs bell if process output starts with an ASCII bell, and pass
the rest to `comint-output-filter'."
  (comint-output-filter proc (inferior-octave-strip-ctrl-g string)))

(defun inferior-octave-output-digest (_proc string)
  "Special output filter for the inferior Octave process.
Save all output between newlines into `inferior-octave-output-list', and
the rest to `inferior-octave-output-string'."
  (setq string (concat inferior-octave-output-string string))
  (while (string-match "\n" string)
    (setq inferior-octave-output-list
	  (append inferior-octave-output-list
		  (list (substring string 0 (match-beginning 0))))
	  string (substring string (match-end 0))))
  (if (string-match inferior-octave-prompt string)
      (setq inferior-octave-receive-in-progress nil))
  (setq inferior-octave-output-string string))

(defun inferior-octave-send-list-and-digest (list)
  "Send LIST to the inferior Octave process and digest the output.
The elements of LIST have to be strings and are sent one by one.  All
output is passed to the filter `inferior-octave-output-digest'."
  (let* ((proc inferior-octave-process)
	 (filter (process-filter proc))
	 string)
    (set-process-filter proc 'inferior-octave-output-digest)
    (setq inferior-octave-output-list nil)
    (unwind-protect
	(while (setq string (car list))
	  (setq inferior-octave-output-string nil
		inferior-octave-receive-in-progress t)
	  (comint-send-string proc string)
	  (while inferior-octave-receive-in-progress
	    (accept-process-output proc))
	  (setq list (cdr list)))
      (set-process-filter proc filter))))

(defun inferior-octave-directory-tracker (string)
  "Tracks `cd' commands issued to the inferior Octave process.
Use \\[inferior-octave-resync-dirs] to resync if Emacs gets confused."
  (cond
   ((string-match "^[ \t]*cd[ \t;]*$" string)
    (cd "~"))
   ((string-match "^[ \t]*cd[ \t]+\\([^ \t\n;]*\\)[ \t\n;]*" string)
    (cd (substring string (match-beginning 1) (match-end 1))))))

(defun inferior-octave-resync-dirs ()
  "Resync the buffer's idea of the current directory.
This command queries the inferior Octave process about its current
directory and makes this the current buffer's default directory."
  (interactive)
  (inferior-octave-send-list-and-digest '("disp (pwd ())\n"))
  (cd (car inferior-octave-output-list)))


;;; Miscellaneous useful functions

(defun octave-in-comment-p ()
  "Return non-nil if point is inside an Octave comment."
  (nth 4 (syntax-ppss)))

(defun octave-in-string-p ()
  "Return non-nil if point is inside an Octave string."
  (nth 3 (syntax-ppss)))

(defun octave-in-string-or-comment-p ()
  "Return non-nil if point is inside an Octave string or comment."
  (nth 8 (syntax-ppss)))

(defun octave-looking-at-kw (regexp)
  "Like `looking-at', but sets `case-fold-search' nil."
  (let ((case-fold-search nil))
    (looking-at regexp)))

(defun octave-maybe-insert-continuation-string ()
  (if (or (octave-in-comment-p)
	  (save-excursion
	    (beginning-of-line)
	    (looking-at octave-continuation-regexp)))
      nil
    (delete-horizontal-space)
    (insert (concat " " octave-continuation-string))))

(defun octave-function-file-p ()
  "Return non-nil if the first token is \"function\".
The value is (START END NAME-START NAME-END) of the function."
  (save-excursion
    (goto-char (point-min))
    (when (equal (funcall smie-forward-token-function) "function")
      (forward-word -1)
      (let* ((start (point))
             (end (progn (forward-sexp 1) (point)))
             (name (when (progn
                           (goto-char start)
                           (re-search-forward octave-function-header-regexp
                                              end t))
                     (list (match-beginning 3) (match-end 3)))))
        (cons start (cons end name))))))

;; Like forward-comment but stop at non-comment blank
(defun octave-skip-comment-forward (limit)
  (let ((ppss (syntax-ppss)))
    (if (nth 4 ppss)
        (goto-char (nth 8 ppss))
      (goto-char (or (comment-search-forward limit t) (point)))))
  (while (and (< (point) limit) (looking-at-p "\\s<"))
    (forward-comment 1)))

;;; First non-copyright comment block
(defun octave-function-file-comment ()
  "Beginning and end positions of the function file comment."
  (save-excursion
    (goto-char (point-min))
    ;; Copyright block: octave/libinterp/parse-tree/lex.ll around line 1634
    (while (save-excursion
             (when (comment-search-forward (point-max) t)
               (when (eq (char-after) ?\{) ; case of block comment
                 (forward-char 1))
               (skip-syntax-forward "-")
               (let ((case-fold-search t))
                 (looking-at-p "\\(?:copyright\\|author\\)\\_>"))))
      (octave-skip-comment-forward (point-max)))
    (let ((beg (comment-search-forward (point-max) t)))
      (when beg
        (goto-char beg)
        (octave-skip-comment-forward (point-max))
        (list beg (point))))))

(defun octave-sync-function-file-names ()
  "Ensure function name agree with function file name.
See Info node `(octave)Function Files'."
  (interactive)
  (when buffer-file-name
    (pcase-let ((`(,start ,_end ,name-start ,name-end)
                 (octave-function-file-p)))
      (when (and start name-start)
        (let* ((func (buffer-substring name-start name-end))
               (file (file-name-sans-extension
                      (file-name-nondirectory buffer-file-name)))
               (help-form (format "\
a: Use function name `%s'
b: Use file name `%s'
q: Don't fix\n" func file))
               (c (unless (equal file func)
                    (save-window-excursion
                      (help-form-show)
                      (read-char-choice
                       "Which name to use? (a/b/q) " '(?a ?b ?q))))))
          (pcase c
            (`?a (let ((newname (expand-file-name
                                 (concat func (file-name-extension
                                               buffer-file-name t)))))
                   (when (or (not (file-exists-p newname))
                             (yes-or-no-p
                              (format "Target file %s exists; proceed? " newname)))
                     (when (file-exists-p buffer-file-name)
                       (rename-file buffer-file-name newname t))
                     (set-visited-file-name newname))))
            (`?b (save-excursion
                   (goto-char name-start)
                   (delete-region name-start name-end)
                   (insert file)))))))))

(defun octave-update-function-file-comment (beg end)
  "Query replace function names in function file comment."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (or (octave-function-file-comment)
           (error "No function file comment found")))))
  (save-excursion
    (let* ((bounds (or (octave-function-file-p)
                       (error "Not in a function file buffer")))
           (func (if (cddr bounds)
                     (apply #'buffer-substring (cddr bounds))
                   (error "Function name not found")))
           (old-func (progn
                       (goto-char beg)
                       (when (re-search-forward
                              "[=}]\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>"
                              (min (line-end-position 4) end)
                              t)
                         (match-string 1))))
           (old-func (read-string (format (if old-func
                                              "Name to replace (default %s): "
                                            "Name to replace: ")
                                          old-func)
                                  nil nil old-func)))
      (if (and func old-func (not (equal func old-func)))
          (perform-replace old-func func 'query
                           nil 'delimited nil nil beg end)
        (message "Function names match")))))

;; Adapted from texinfo-font-lock-keywords
(defvar octave-texinfo-font-lock-keywords
  `(("@\\([a-zA-Z]+\\|[^ \t\n]\\)" 1 font-lock-keyword-face prepend) ;commands
    ("^\\*\\([^\n:]*\\)" 1 font-lock-function-name-face prepend) ;menu items
    ("@\\(emph\\|i\\|sc\\){\\([^}]+\\)" 2 'italic prepend)
    ("@\\(strong\\|b\\){\\([^}]+\\)" 2 'bold prepend)
    ("@\\(kbd\\|key\\|url\\|uref\\){\\([^}]+\\)"
     2 font-lock-string-face prepend)
    ("@\\(file\\|email\\){\\([^}]+\\)" 2 font-lock-string-face prepend)
    ("@\\(samp\\|code\\|var\\|math\\|env\\|command\\|option\\){\\([^}]+\\)"
     2 font-lock-variable-name-face prepend)
    ("@\\(cite\\|x?ref\\|pxref\\|dfn\\|inforef\\){\\([^}]+\\)"
     2 font-lock-constant-face prepend)
    ("@\\(anchor\\){\\([^}]+\\)" 2 font-lock-type-face prepend)
    ("@\\(dmn\\|acronym\\|value\\){\\([^}]+\\)"
     2 font-lock-builtin-face prepend)
    ("@\\(end\\|itemx?\\) +\\(.+\\)" 2 font-lock-keyword-face prepend))
  "Additional keywords to highlight in texinfo comment block.")

(defface octave-function-comment-block
  '((t (:inherit font-lock-doc-face)))
  "Face used to highlight function comment block."
  :group 'octave)

(defun octave-font-lock-texinfo-comment ()
  (font-lock-add-keywords
   nil
   '(((lambda (limit)
        (while (and (search-forward "-*- texinfo -*-" limit t)
                    (octave-in-comment-p))
          (let ((beg (nth 8 (syntax-ppss)))
                (end (progn
                       (octave-skip-comment-forward (point-max))
                       (point))))
            (put-text-property beg end 'font-lock-multiline t)
            (font-lock-prepend-text-property
             beg end 'face 'octave-function-comment-block)
            (dolist (kw octave-texinfo-font-lock-keywords)
              (goto-char beg)
              (while (re-search-forward (car kw) end 'move)
                (font-lock-apply-highlight (cdr kw))))))
        nil)))
   'append))


;;; Indentation

(defun octave-indent-new-comment-line ()
  "Break Octave line at point, continuing comment if within one.
If within code, insert `octave-continuation-string' before breaking the
line.  If within a string, signal an error.
The new line is properly indented."
  (interactive)
  (delete-horizontal-space)
  (cond
   ((octave-in-comment-p)
    (indent-new-comment-line))
   ((octave-in-string-p)
    (error "Cannot split a code line inside a string"))
   (t
    (insert (concat " " octave-continuation-string))
    (reindent-then-newline-and-indent))))

(defun octave-indent-defun ()
  "Properly indent the Octave function which contains point."
  (interactive)
  (save-excursion
    (mark-defun)
    (message "Indenting function...")
    (indent-region (point) (mark) nil))
  (message "Indenting function...done."))


;;; Motion
(defun octave-next-code-line (&optional arg)
  "Move ARG lines of Octave code forward (backward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc))
      (while (and (= n 0)
		  (looking-at "\\s-*\\($\\|\\s<\\)"))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))

(defun octave-previous-code-line (&optional arg)
  "Move ARG lines of Octave code backward (forward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (octave-next-code-line (- arg)))

(defun octave-beginning-of-line ()
  "Move point to beginning of current Octave line.
If on an empty or comment line, go to the beginning of that line.
Otherwise, move backward to the beginning of the first Octave code line
which is not inside a continuation statement, i.e., which does not
follow a code line ending in `...' or `\\', or is inside an open
parenthesis list."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "\\s-*\\($\\|\\s<\\)"))
      (while (or (condition-case nil
		     (progn
		       (up-list -1)
		       (beginning-of-line)
		       t)
		   (error nil))
		 (and (or (looking-at "\\s-*\\($\\|\\s<\\)")
			  (save-excursion
			    (if (zerop (octave-previous-code-line))
				(looking-at octave-continuation-regexp))))
		      (zerop (forward-line -1)))))))

(defun octave-end-of-line ()
  "Move point to end of current Octave line.
If on an empty or comment line, go to the end of that line.
Otherwise, move forward to the end of the first Octave code line which
does not end in `...' or `\\' or is inside an open parenthesis list."
  (interactive)
  (end-of-line)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "\\s-*\\($\\|\\s<\\)"))
      ()
    (while (or (condition-case nil
		   (progn
		     (up-list 1)
		     (end-of-line)
		     t)
		 (error nil))
	       (and (save-excursion
		      (beginning-of-line)
		      (or (looking-at "\\s-*\\($\\|\\s<\\)")
			  (looking-at octave-continuation-regexp)))
		    (zerop (forward-line 1)))))
    (end-of-line)))

(defun octave-mark-block ()
  "Put point at the beginning of this Octave block, mark at the end.
The block marked is the one that contains point or follows point."
  (interactive)
  (if (and (looking-at "\\sw\\|\\s_")
           (looking-back "\\sw\\|\\s_" (1- (point))))
      (skip-syntax-forward "w_"))
  (unless (or (looking-at "\\s(")
              (save-excursion
                (let* ((token (funcall smie-forward-token-function))
                       (level (assoc token smie-grammar)))
                  (and level (not (numberp (cadr level)))))))
    (backward-up-list 1))
  (mark-sexp))

(defun octave-beginning-of-defun (&optional arg)
  "Move backward to the beginning of an Octave function.
With positive ARG, do it that many times.  Negative argument -N means
move forward to Nth following beginning of a function.
Returns t unless search stops at the beginning or end of the buffer."
  (let* ((arg (or arg 1))
	 (inc (if (> arg 0) 1 -1))
	 (found nil)
         (case-fold-search nil))
    (and (not (eobp))
	 (not (and (> arg 0) (looking-at "\\_<function\\_>")))
	 (skip-syntax-forward "w"))
    (while (and (/= arg 0)
		(setq found
		      (re-search-backward "\\_<function\\_>" inc)))
      (unless (octave-in-string-or-comment-p)
        (setq arg (- arg inc))))
    (if found
	(progn
	  (and (< inc 0) (goto-char (match-beginning 0)))
	  t))))


;;; Filling
(defun octave-auto-fill ()
  "Perform auto-fill in Octave mode.
Returns nil if no feasible place to break the line could be found, and t
otherwise."
  (let (fc give-up)
    (if (or (null (setq fc (current-fill-column)))
	    (save-excursion
	      (beginning-of-line)
	      (and auto-fill-inhibit-regexp
		   (octave-looking-at-kw auto-fill-inhibit-regexp))))
	nil				; Can't do anything
      (if (and (not (octave-in-comment-p))
	       (> (current-column) fc))
	  (setq fc (- fc (+ (length octave-continuation-string) 1))))
      (while (and (not give-up) (> (current-column) fc))
	(let* ((opoint (point))
	       (fpoint
		(save-excursion
		  (move-to-column (+ fc 1))
		  (skip-chars-backward "^ \t\n")
		  ;; If we're at the beginning of the line, break after
		  ;; the first word
		  (if (bolp)
		      (re-search-forward "[ \t]" opoint t))
		  ;; If we're in a comment line, don't break after the
		  ;; comment chars
		  (if (save-excursion
			(skip-syntax-backward " <")
			(bolp))
		      (re-search-forward "[ \t]" (line-end-position)
					 'move))
		  ;; If we're not in a comment line and just ahead the
		  ;; continuation string, don't break here.
		  (if (and (not (octave-in-comment-p))
			   (looking-at
			    (concat "\\s-*"
				    (regexp-quote
				     octave-continuation-string)
				    "\\s-*$")))
		      (end-of-line))
		  (skip-chars-backward " \t")
		  (point))))
	  (if (save-excursion
		(goto-char fpoint)
		(not (or (bolp) (eolp))))
	      (let ((prev-column (current-column)))
		(if (save-excursion
		      (skip-chars-backward " \t")
		      (= (point) fpoint))
		    (progn
		      (octave-maybe-insert-continuation-string)
		      (indent-new-comment-line t))
		  (save-excursion
		    (goto-char fpoint)
		    (octave-maybe-insert-continuation-string)
		    (indent-new-comment-line t)))
		(if (>= (current-column) prev-column)
		    (setq give-up t)))
	    (setq give-up t))))
      (not give-up))))

(defun octave-fill-paragraph (&optional _arg)
  "Fill paragraph of Octave code, handling Octave comments."
  ;; FIXME: difference with generic fill-paragraph:
  ;; - code lines are only split, never joined.
  ;; - \n that end comments are never removed.
  ;; - insert continuation marker when splitting code lines.
  (interactive "P")
  (save-excursion
    (let ((end (progn (forward-paragraph) (copy-marker (point) t)))
          (beg (progn
                 (forward-paragraph -1)
                 (skip-chars-forward " \t\n")
                 (beginning-of-line)
                 (point)))
          (cfc (current-fill-column))
          comment-prefix)
      (goto-char beg)
      (while (< (point) end)
        (condition-case nil
            (indent-according-to-mode)
          (error nil))
        (move-to-column cfc)
        ;; First check whether we need to combine non-empty comment lines
        (if (and (< (current-column) cfc)
                 (octave-in-comment-p)
                 (not (save-excursion
                        (beginning-of-line)
                        (looking-at "^\\s-*\\s<+\\s-*$"))))
            ;; This is a nonempty comment line which does not extend
            ;; past the fill column.  If it is followed by a nonempty
            ;; comment line with the same comment prefix, try to
            ;; combine them, and repeat this until either we reach the
            ;; fill-column or there is nothing more to combine.
            (progn
              ;; Get the comment prefix
              (save-excursion
                (beginning-of-line)
                (while (and (re-search-forward "\\s<+")
                            (not (octave-in-comment-p))))
                (setq comment-prefix (match-string 0)))
              ;; And keep combining ...
              (while (and (< (current-column) cfc)
                          (save-excursion
                            (forward-line 1)
                            (and (looking-at
                                  (concat "^\\s-*"
                                          comment-prefix
                                          "\\S<"))
                                 (not (looking-at
                                       (concat "^\\s-*"
                                               comment-prefix
                                               "\\s-*$"))))))
                (delete-char 1)
                (re-search-forward comment-prefix)
                (delete-region (match-beginning 0) (match-end 0))
                (fixup-whitespace)
                (move-to-column cfc))))
        ;; We might also try to combine continued code lines>  Perhaps
        ;; some other time ...
        (skip-chars-forward "^ \t\n")
        (delete-horizontal-space)
        (if (or (< (current-column) cfc)
                (and (= (current-column) cfc) (eolp)))
            (forward-line 1)
          (if (not (eolp)) (insert " "))
          (or (octave-auto-fill)
              (forward-line 1))))
      t)))


;;; Completions

(defun octave-completion-at-point-function ()
  "Find the text to complete and the corresponding table."
  (let* ((beg (save-excursion (skip-syntax-backward "w_") (point)))
         (end (point)))
    (if (< beg (point))
        ;; Extend region past point, if applicable.
        (save-excursion (skip-syntax-forward "w_")
                        (setq end (point))))
    (list beg end (or (and inferior-octave-process
                           (process-live-p inferior-octave-process)
                           (inferior-octave-completion-table))
                      (append octave-reserved-words
                              octave-text-functions)))))

(define-obsolete-function-alias 'octave-complete-symbol
  'completion-at-point "24.1")

;;; Electric characters && friends
(define-skeleton octave-insert-defun
  "Insert an Octave function skeleton.
Prompt for the function's name, arguments and return values (to be
entered without parens)."
  (let* ((defname (file-name-sans-extension (buffer-name)))
         (name (read-string (format "Function name (default %s): " defname)
                            nil nil defname))
         (args (read-string "Arguments: "))
         (vals (read-string "Return values: ")))
    (format "%s%s (%s)"
            (cond
             ((string-equal vals "") vals)
             ((string-match "[ ,]" vals) (concat "[" vals "] = "))
             (t (concat vals " = ")))
            name
            args))
  \n octave-block-comment-start "usage: " str \n
  octave-block-comment-start '(delete-horizontal-space) \n
  octave-block-comment-start '(delete-horizontal-space) \n
  "function " > str \n
  _ \n
  "endfunction" > \n)

;;; Communication with the inferior Octave process
(defun octave-kill-process ()
  "Kill inferior Octave process and its buffer."
  (interactive)
  (if inferior-octave-process
      (progn
	(process-send-string inferior-octave-process "quit;\n")
	(accept-process-output inferior-octave-process)))
  (if inferior-octave-buffer
      (kill-buffer inferior-octave-buffer)))

(defun octave-show-process-buffer ()
  "Make sure that `inferior-octave-buffer' is displayed."
  (interactive)
  (if (get-buffer inferior-octave-buffer)
      (display-buffer inferior-octave-buffer)
    (message "No buffer named %s" inferior-octave-buffer)))

(defun octave-hide-process-buffer ()
  "Delete all windows that display `inferior-octave-buffer'."
  (interactive)
  (if (get-buffer inferior-octave-buffer)
      (delete-windows-on inferior-octave-buffer)
    (message "No buffer named %s" inferior-octave-buffer)))

(defun octave-send-region (beg end)
  "Send current region to the inferior Octave process."
  (interactive "r")
  (inferior-octave t)
  (let ((proc inferior-octave-process)
	(string (buffer-substring-no-properties beg end))
	line)
    (with-current-buffer inferior-octave-buffer
      (setq inferior-octave-output-list nil)
      (while (not (string-equal string ""))
	(if (string-match "\n" string)
	    (setq line (substring string 0 (match-beginning 0))
		  string (substring string (match-end 0)))
	  (setq line string string ""))
	(setq inferior-octave-receive-in-progress t)
	(inferior-octave-send-list-and-digest (list (concat line "\n")))
	(while inferior-octave-receive-in-progress
	  (accept-process-output proc))
	(insert-before-markers
	 (mapconcat 'identity
		    (append
		     (if octave-send-echo-input (list line) (list ""))
		     (mapcar 'inferior-octave-strip-ctrl-g
			     inferior-octave-output-list)
		     (list inferior-octave-output-string))
		    "\n")))))
  (if octave-send-show-buffer
      (display-buffer inferior-octave-buffer)))

(defun octave-send-block ()
  "Send current Octave block to the inferior Octave process."
  (interactive)
  (save-excursion
    (octave-mark-block)
    (octave-send-region (point) (mark))))

(defun octave-send-defun ()
  "Send current Octave function to the inferior Octave process."
  (interactive)
  (save-excursion
    (mark-defun)
    (octave-send-region (point) (mark))))

(defun octave-send-line (&optional arg)
  "Send current Octave code line to the inferior Octave process.
With positive prefix ARG, send that many lines.
If `octave-send-line-auto-forward' is non-nil, go to the next unsent
code line."
  (interactive "P")
  (or arg (setq arg 1))
  (if (> arg 0)
      (let (beg end)
	(beginning-of-line)
	(setq beg (point))
	(octave-next-code-line (- arg 1))
	(end-of-line)
	(setq end (point))
	(if octave-send-line-auto-forward
	    (octave-next-code-line 1))
	(octave-send-region beg end))))

(defun octave-eval-print-last-sexp ()
  "Evaluate Octave sexp before point and print value into current buffer."
  (interactive)
  (inferior-octave t)
  (let ((standard-output (current-buffer))
	(print-escape-newlines nil)
	(opoint (point)))
    (terpri)
    (prin1
     (save-excursion
       (forward-sexp -1)
       (inferior-octave-send-list-and-digest
	(list (concat (buffer-substring-no-properties (point) opoint)
		      "\n")))
       (mapconcat 'identity inferior-octave-output-list "\n")))
    (terpri)))


(provide 'octave)
;;; octave.el ends here
