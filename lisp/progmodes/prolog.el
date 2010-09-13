;;; prolog.el --- major mode for editing and running Prolog under Emacs

;; Copyright (C) 1986, 1987, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008, 2009, 2010  Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
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

;; This package provides a major mode for editing Prolog.  It knows
;; about Prolog syntax and comments, and can send regions to an inferior
;; Prolog interpreter process.  Font locking is tuned towards GNU Prolog.

;;; Code:

(defvar comint-prompt-regexp)
(defvar comint-process-echoes)
(require 'smie)

(defgroup prolog nil
  "Major mode for editing and running Prolog under Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)


(defcustom prolog-program-name
  (let ((names '("prolog" "gprolog" "swipl")))
    (while (and names
		(not (executable-find (car names))))
      (setq names (cdr names)))
    (or (car names) "prolog"))
  "Program name for invoking an inferior Prolog with `run-prolog'."
  :type 'string
  :group 'prolog)

(defcustom prolog-consult-string "reconsult(user).\n"
  "(Re)Consult mode (for C-Prolog and Quintus Prolog). "
  :type 'string
  :group 'prolog)

(defcustom prolog-compile-string "compile(user).\n"
  "Compile mode (for Quintus Prolog)."
  :type 'string
  :group 'prolog)

(defcustom prolog-eof-string "end_of_file.\n"
  "String that represents end of file for Prolog.
When nil, send actual operating system end of file."
  :type 'string
  :group 'prolog)

(defcustom prolog-indent-width 4
  "Level of indentation in Prolog buffers."
  :type 'integer
  :group 'prolog)

(defvar prolog-font-lock-keywords
  '(("\\(#[<=]=>\\|:-\\)\\|\\(#=\\)\\|\\(#[#<>\\/][=\\/]*\\|!\\)"
     0 font-lock-keyword-face)
    ("\\<\\(is\\|write\\|nl\\|read_\\sw+\\)\\>"
     1 font-lock-keyword-face)
    ("^\\(\\sw+\\)\\s-*\\((\\(.+\\))\\)*"
     (1 font-lock-function-name-face)
     (3 font-lock-variable-name-face)))
  "Font-lock keywords for Prolog mode.")

(defvar prolog-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    table))

(defvar prolog-mode-abbrev-table nil)
(define-abbrev-table 'prolog-mode-abbrev-table ())

(defconst prolog-smie-op-levels
  ;; Rather than construct the operator levels table from the BNF,
  ;; we directly provide the operator precedences from GNU Prolog's
  ;; manual.  The only problem is that GNU Prolog's manual uses
  ;; precedence levels in the opposite sense (higher numbers bind less
  ;; tightly) than SMIE, so we use negative numbers.
  '(("." -10000 -10000)
    (":-" -1200 -1200)
    ("-->" -1200 -1200)
    (";" -1100 -1100)
    ("->" -1050 -1050)
    ("," -1000 -1000)
    ("\\+" -900 -900)
    ("=" -700 -700)
    ("\\=" -700 -700)
    ("=.." -700 -700)
    ("==" -700 -700)
    ("\\==" -700 -700)
    ("@<" -700 -700)
    ("@=<" -700 -700)
    ("@>" -700 -700)
    ("@>=" -700 -700)
    ("is" -700 -700)
    ("=:=" -700 -700)
    ("=\\=" -700 -700)
    ("<" -700 -700)
    ("=<" -700 -700)
    (">" -700 -700)
    (">=" -700 -700)
    (":" -600 -600)
    ("+" -500 -500)
    ("-" -500 -500)
    ("/\\" -500 -500)
    ("\\/" -500 -500)
    ("*" -400 -400)
    ("/" -400 -400)
    ("//" -400 -400)
    ("rem" -400 -400)
    ("mod" -400 -400)
    ("<<" -400 -400)
    (">>" -400 -400)
    ("**" -200 -200)
    ("^" -200 -200)
    ;; Prefix
    ;; ("+" 200 200)
    ;; ("-" 200 200)
    ;; ("\\" 200 200)
    )
  "Precedence levels of infix operators.")

(defconst prolog-smie-indent-rules
  '((":-")
    ("->"))
  "Prolog indentation rules.")

(defun prolog-mode-variables ()
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat "%%\\|$\\|" page-delimiter)) ;'%%..'
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression '((nil "^\\sw+" 0)))
  (smie-setup prolog-smie-op-levels prolog-smie-indent-rules)
  (set (make-local-variable 'forward-sexp-function)
       'smie-forward-sexp-command)
  (set (make-local-variable 'smie-indent-basic) prolog-indent-width)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(?:%+\\|/\\*+\\)[ \t]*")
  (make-local-variable 'comment-end-skip)
  (setq comment-end-skip "[ \t]*\\(\n\\|\\*+/\\)")
  (make-local-variable 'comment-column)
  (setq comment-column 48))

(defvar prolog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e\C-x" 'prolog-consult-region)
    (define-key map "\C-c\C-l" 'inferior-prolog-load-file)
    (define-key map "\C-c\C-z" 'switch-to-prolog)
    map))

(easy-menu-define prolog-mode-menu prolog-mode-map "Menu for Prolog mode."
  ;; Mostly copied from scheme-mode's menu.
  ;; Not tremendously useful, but it's a start.
  '("Prolog"
    ["Indent line" indent-according-to-mode t]
    ["Indent region" indent-region t]
    ["Comment region" comment-region t]
    ["Uncomment region" uncomment-region t]
    "--"
    ["Run interactive Prolog session" run-prolog t]
    ))

;;;###autoload
(define-derived-mode prolog-mode prog-mode "Prolog"
  "Major mode for editing Prolog code for Prologs.
Blank lines and `%%...' separate paragraphs.  `%'s start comments.
Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of `prolog-mode-hook'
if that value is non-nil."
  (prolog-mode-variables)
  (set (make-local-variable 'comment-add) 1)
  (setq font-lock-defaults '(prolog-font-lock-keywords
                             nil nil nil
                             beginning-of-line)))

(defun end-of-prolog-clause ()
  "Go to end of clause in this line."
  (beginning-of-line 1)
  (let* ((eolpos (save-excursion (end-of-line) (point))))
    (if (re-search-forward comment-start-skip eolpos 'move)
	(goto-char (match-beginning 0)))
    (skip-chars-backward " \t")))

;;;
;;; Inferior prolog mode
;;;
(defvar inferior-prolog-mode-map
  (let ((map (make-sparse-keymap)))
    ;; This map will inherit from `comint-mode-map' when entering
    ;; inferior-prolog-mode.
    (define-key map [remap self-insert-command]
      'inferior-prolog-self-insert-command)
    map))

(defvar inferior-prolog-mode-syntax-table prolog-mode-syntax-table)
(defvar inferior-prolog-mode-abbrev-table prolog-mode-abbrev-table)

(defvar inferior-prolog-error-regexp-alist
  ;; GNU Prolog used to not follow the GNU standard format.
  '(("^\\(.*?\\):\\([0-9]+\\) error: .*(char:\\([0-9]+\\)" 1 2 3)
    gnu))

(declare-function comint-mode "comint")
(declare-function comint-send-string "comint" (process string))
(declare-function comint-send-region "comint" (process start end))
(declare-function comint-send-eof "comint" ())
(defvar compilation-error-regexp-alist)

(define-derived-mode inferior-prolog-mode comint-mode "Inferior Prolog"
  "Major mode for interacting with an inferior Prolog process.

The following commands are available:
\\{inferior-prolog-mode-map}

Entry to this mode calls the value of `prolog-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`prolog-mode-hook' is called after `comint-mode-hook'.

You can send text to the inferior Prolog from other buffers using the commands
`process-send-region', `process-send-string' and \\[prolog-consult-region].

Commands:
Tab indents for Prolog; with argument, shifts rest
 of expression rigidly with the current line.
Paragraphs are separated only by blank lines and '%%'.
'%'s start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops. \\[comint-quit-subjob] sends quit signal."
  (setq comint-prompt-regexp "^| [ ?][- ] *")
  (set (make-local-variable 'compilation-error-regexp-alist)
       inferior-prolog-error-regexp-alist)
  (compilation-shell-minor-mode)
  (prolog-mode-variables))

(defvar inferior-prolog-buffer nil)

(defvar inferior-prolog-flavor 'unknown
  "Either a symbol or a buffer position offset by one.
If a buffer position, the flavor has not been determined yet and
it is expected that the process's output has been or will
be inserted at that position plus one.")

(defun inferior-prolog-run (&optional name)
  (with-current-buffer (make-comint "prolog" (or name prolog-program-name))
    (inferior-prolog-mode)
    (setq-default inferior-prolog-buffer (current-buffer))
    (make-local-variable 'inferior-prolog-buffer)
    (when (and name (not (equal name prolog-program-name)))
      (set (make-local-variable 'prolog-program-name) name))
    (set (make-local-variable 'inferior-prolog-flavor)
         ;; Force re-detection.
         (let* ((proc (get-buffer-process (current-buffer)))
                (pmark (and proc (marker-position (process-mark proc)))))
           (cond
            ((null pmark) (1- (point-min)))
            ;; The use of insert-before-markers in comint.el together with
            ;; the potential use of comint-truncate-buffer in the output
            ;; filter, means that it's difficult to reliably keep track of
            ;; the buffer position where the process's output started.
            ;; If possible we use a marker at "start - 1", so that
            ;; insert-before-marker at `start' won't shift it.  And if not,
            ;; we fall back on using a plain integer.
            ((> pmark (point-min)) (copy-marker (1- pmark)))
            (t (1- pmark)))))
    (add-hook 'comint-output-filter-functions
              'inferior-prolog-guess-flavor nil t)))

(defun inferior-prolog-process (&optional dontstart)
  (or (and (buffer-live-p inferior-prolog-buffer)
           (get-buffer-process inferior-prolog-buffer))
      (unless dontstart
        (inferior-prolog-run)
        ;; Try again.
        (inferior-prolog-process))))

(defun inferior-prolog-guess-flavor (&optional ignored)
  (save-excursion
    (goto-char (1+ inferior-prolog-flavor))
    (setq inferior-prolog-flavor
          (cond
           ((looking-at "GNU Prolog") 'gnu)
           ((looking-at "Welcome to SWI-Prolog") 'swi)
           ((looking-at ".*\n") 'unknown) ;There's at least one line.
           (t inferior-prolog-flavor))))
  (when (symbolp inferior-prolog-flavor)
    (remove-hook 'comint-output-filter-functions
                 'inferior-prolog-guess-flavor t)
    (if (eq inferior-prolog-flavor 'gnu)
        (set (make-local-variable 'comint-process-echoes) t))))

;;;###autoload
(defalias 'run-prolog 'switch-to-prolog)
;;;###autoload
(defun switch-to-prolog (&optional name)
  "Run an inferior Prolog process, input and output via buffer *prolog*.
With prefix argument \\[universal-prefix], prompt for the program to use."
  (interactive
   (list (when current-prefix-arg
           (let ((proc (inferior-prolog-process 'dontstart)))
             (if proc
                 (if (yes-or-no-p "Kill current process before starting new one? ")
                     (kill-process proc)
                   (error "Abort")))
             (read-string "Run Prolog: " prolog-program-name)))))
  (unless (inferior-prolog-process 'dontstart)
    (inferior-prolog-run name))
  (pop-to-buffer inferior-prolog-buffer))

(defun inferior-prolog-self-insert-command ()
  "Insert the char in the buffer or pass it directly to the process."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (marker-position (process-mark proc)))))
    (if (and (eq inferior-prolog-flavor 'gnu)
             pmark
             (null current-prefix-arg)
             (eobp)
             (eq (point) pmark)
             (save-excursion
               (goto-char (- pmark 3))
               (looking-at " \\? ")))
        ;; This is GNU prolog waiting to know whether you want more answers
        ;; or not (or abort, etc...).  The answer is a single char, not
        ;; a line, so pass this char directly rather than wait for RET to
        ;; send a whole line.
        (comint-send-string proc (string last-command-event))
      (call-interactively 'self-insert-command))))

(defun prolog-consult-region (compile beg end)
  "Send the region to the Prolog process made by \"M-x run-prolog\".
If COMPILE (prefix arg) is not nil, use compile mode rather than consult mode."
  (interactive "P\nr")
  (let ((proc (inferior-prolog-process)))
    (comint-send-string proc
                        (if compile prolog-compile-string
                          prolog-consult-string))
    (comint-send-region proc beg end)
    (comint-send-string proc "\n")		;May be unnecessary
    (if prolog-eof-string
        (comint-send-string proc prolog-eof-string)
      (with-current-buffer (process-buffer proc)
        (comint-send-eof))))) ;Send eof to prolog process.

(defun prolog-consult-region-and-go (compile beg end)
  "Send the region to the inferior Prolog, and switch to *prolog* buffer.
If COMPILE (prefix arg) is not nil, use compile mode rather than consult mode."
  (interactive "P\nr")
  (prolog-consult-region compile beg end)
  (pop-to-buffer inferior-prolog-buffer))

;; inferior-prolog-mode uses the autoloaded compilation-shell-minor-mode.
(declare-function compilation-forget-errors "compile" ())

(defun inferior-prolog-load-file ()
  "Pass the current buffer's file to the inferior prolog process."
  (interactive)
  (save-buffer)
  (let ((file buffer-file-name)
        (proc (inferior-prolog-process)))
    (with-current-buffer (process-buffer proc)
      (compilation-forget-errors)
      (comint-send-string proc (concat "['" (file-relative-name file) "'].\n"))
      (pop-to-buffer (current-buffer)))))

(provide 'prolog)

;; arch-tag: f3ec6748-1272-4ab6-8826-c50cb1607636
;;; prolog.el ends here
