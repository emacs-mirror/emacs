;;; modula2.el --- Modula-2 editing support package

;; Author: Michael Schmidt <michael@pbinfo.UUCP> 
;;	Tom Perrine <Perrin@LOGICON.ARPA>
;; Keywords: languages

;; The authors distributed this without a copyright notice
;; back in 1988, so it is in the public domain.  The original included
;; the following credit:

;; Author Mick Jordan
;; amended Peter Robinson

;;; Commentary:

;; A major mode for editing Modula-2 code.  It provides convenient abbrevs
;; for Modula-2 keywords, knows about the standard layout rules, and supports
;; a native compile command.

;;; Code:

;;; Added by Tom Perrine (TEP)
(defvar m2-mode-syntax-table nil
  "Syntax table in use in Modula-2 buffers.")

(defvar m2-compile-command "m2c"
  "Command to compile Modula-2 programs")

(defvar m2-link-command "m2l"
  "Command to link Modula-2 programs")

(defvar m2-link-name nil
  "Name of the executable.")


(if m2-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\( ". 1" table)
    (modify-syntax-entry ?\) ". 4" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq m2-mode-syntax-table table)))

;;; Added by TEP
(defvar m2-mode-map nil
  "Keymap used in Modula-2 mode.")

(if m2-mode-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\^i" 'm2-tab)
    (define-key map "\C-cb" 'm2-begin)
    (define-key map "\C-cc" 'm2-case)
    (define-key map "\C-cd" 'm2-definition)
    (define-key map "\C-ce" 'm2-else)
    (define-key map "\C-cf" 'm2-for)
    (define-key map "\C-ch" 'm2-header)
    (define-key map "\C-ci" 'm2-if)
    (define-key map "\C-cm" 'm2-module)
    (define-key map "\C-cl" 'm2-loop)
    (define-key map "\C-co" 'm2-or)
    (define-key map "\C-cp" 'm2-procedure)
    (define-key map "\C-c\C-w" 'm2-with)
    (define-key map "\C-cr" 'm2-record)
    (define-key map "\C-cs" 'm2-stdio)
    (define-key map "\C-ct" 'm2-type)
    (define-key map "\C-cu" 'm2-until)
    (define-key map "\C-cv" 'm2-var)
    (define-key map "\C-cw" 'm2-while)
    (define-key map "\C-cx" 'm2-export)
    (define-key map "\C-cy" 'm2-import)
    (define-key map "\C-c{" 'm2-begin-comment)
    (define-key map "\C-c}" 'm2-end-comment)
    (define-key map "\C-j"  'm2-newline)
    (define-key map "\C-c\C-z" 'suspend-emacs)
    (define-key map "\C-c\C-v" 'm2-visit)
    (define-key map "\C-c\C-t" 'm2-toggle)
    (define-key map "\C-c\C-l" 'm2-link)
    (define-key map "\C-c\C-c" 'm2-compile)
    (setq m2-mode-map map)))

(defvar m2-indent 5 "*This variable gives the indentation in Modula-2-Mode")
  
;;;###autoload
(defun modula-2-mode ()
  "This is a mode intended to support program development in Modula-2.
All control constructs of Modula-2 can be reached by typing C-c
followed by the first character of the construct.
\\<m2-mode-map>
  \\[m2-begin] begin         \\[m2-case] case
  \\[m2-definition] definition    \\[m2-else] else
  \\[m2-for] for           \\[m2-header] header
  \\[m2-if] if            \\[m2-module] module
  \\[m2-loop] loop          \\[m2-or] or
  \\[m2-procedure] procedure     Control-c Control-w with
  \\[m2-record] record        \\[m2-stdio] stdio
  \\[m2-type] type          \\[m2-until] until
  \\[m2-var] var           \\[m2-while] while
  \\[m2-export] export        \\[m2-import] import
  \\[m2-begin-comment] begin-comment \\[m2-end-comment] end-comment
  \\[suspend-emacs] suspend Emacs     \\[m2-toggle] toggle
  \\[m2-compile] compile           \\[m2-next-error] next-error
  \\[m2-link] link

   `m2-indent' controls the number of spaces for each indentation.
   `m2-compile-command' holds the command to compile a Modula-2 program.
   `m2-link-command' holds the command to link a Modula-2 program."
  (interactive)
  (kill-all-local-variables)
  (use-local-map m2-mode-map)
  (setq major-mode 'modula-2-mode)
  (setq mode-name "Modula-2")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 75)
  (set-syntax-table m2-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
;  (make-local-variable 'indent-line-function)
;  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'm2-mode-hook))

(defun m2-newline ()
  "Insert a newline and indent following line like previous line."
  (interactive)
  (let ((hpos (current-indentation)))
    (newline)
    (indent-to hpos)))

(defun m2-tab ()
  "Indent to next tab stop."
  (interactive)
  (indent-to (* (1+ (/ (current-indentation) m2-indent)) m2-indent)))

(defun m2-begin ()
  "Insert a BEGIN keyword and indent for the next line."
  (interactive)
  (insert "BEGIN")
  (m2-newline)
  (m2-tab))

(defun m2-case ()
  "Build skeleton CASE statment, prompting for the <expression>."
  (interactive)
  (let ((name (read-string "Case-Expression: ")))
    (insert "CASE " name " OF")
    (m2-newline)
    (m2-newline)
    (insert "END (* case " name " *);"))
  (end-of-line 0)
  (m2-tab))

(defun m2-definition ()
  "Build skeleton DEFINITION MODULE, prompting for the <module name>."
  (interactive)
  (insert "DEFINITION MODULE ")
  (let ((name (read-string "Name: ")))
    (insert name ";\n\n\n\nEND " name ".\n"))
  (previous-line 3))

(defun m2-else ()
  "Insert ELSE keyword and indent for next line."
  (interactive)
  (m2-newline)
  (backward-delete-char-untabify m2-indent ())
  (insert "ELSE")
  (m2-newline)
  (m2-tab))

(defun m2-for ()
  "Build skeleton FOR loop statment, prompting for the loop parameters."
  (interactive)
  (insert "FOR ")
  (let ((name (read-string "Loop Initialiser: ")) limit by)
    (insert name " TO ")
    (setq limit (read-string "Limit: "))
    (insert limit)
    (setq by (read-string "Step: "))
    (if (not (string-equal by ""))
	(insert " BY " by))
    (insert " DO")
    (m2-newline)
    (m2-newline)
    (insert "END (* for " name " to " limit " *);"))
  (end-of-line 0)
  (m2-tab))

(defun m2-header ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (insert "(*\n    Title: \t")
  (insert (read-string "Title: "))
  (insert "\n    Created:\t")
  (insert (current-time-string))
  (insert "\n    Author: \t")
  (insert (user-full-name))
  (insert (concat "\n\t\t<" (user-login-name) "@" (system-name) ">\n"))
  (insert "*)\n\n"))

(defun m2-if ()
  "Insert skeleton IF statment, prompting for <boolean-expression>."
  (interactive)
  (insert "IF ")
  (let ((thecondition (read-string "<boolean-expression>: ")))
    (insert thecondition " THEN")
    (m2-newline)
    (m2-newline)
    (insert "END (* if " thecondition " *);"))
  (end-of-line 0)
  (m2-tab))

(defun m2-loop ()
  "Build skeleton LOOP (with END)."
  (interactive)
  (insert "LOOP")
  (m2-newline)
  (m2-newline)
  (insert "END (* loop *);")
  (end-of-line 0)
  (m2-tab))

(defun m2-module ()
  "Build skeleton IMPLEMENTATION MODULE, prompting for <module-name>."
  (interactive)
  (insert "IMPLEMENTATION MODULE ")
  (let ((name (read-string "Name: ")))
    (insert name ";\n\n\n\nEND " name ".\n")
    (previous-line 3)
    (m2-header)
    (m2-type)
    (newline)
    (m2-var)
    (newline)
    (m2-begin)
    (m2-begin-comment)
    (insert " Module " name " Initialisation Code "))
  (m2-end-comment)
  (newline)
  (m2-tab))

(defun m2-or ()
  (interactive)
  (m2-newline)
  (backward-delete-char-untabify m2-indent)
  (insert "|")
  (m2-newline)
  (m2-tab))

(defun m2-procedure ()
  (interactive)
  (insert "PROCEDURE ")
  (let ((name (read-string "Name: " ))
	args)
    (insert name " (")
    (insert (read-string "Arguments: ") ")")
    (setq args (read-string "Result Type: "))
    (if (not (string-equal args ""))
	(insert " : " args))
    (insert ";")
    (m2-newline)
    (insert "BEGIN")
    (m2-newline)
    (m2-newline)
    (insert "END ")
    (insert name)
    (insert ";")
    (end-of-line 0)
    (m2-tab)))

(defun m2-with ()
  (interactive)
  (insert "WITH ")
  (let ((name (read-string "Record-Type: ")))
    (insert name)
    (insert " DO")
    (m2-newline)
    (m2-newline)
    (insert "END (* with " name " *);"))
  (end-of-line 0)
  (m2-tab))

(defun m2-record ()
  (interactive)
  (insert "RECORD")
  (m2-newline)
  (m2-newline)
  (insert "END (* record *);")
  (end-of-line 0)
  (m2-tab))

(defun m2-stdio ()
  (interactive)
  (insert "
FROM TextIO IMPORT 
   WriteCHAR, ReadCHAR, WriteINTEGER, ReadINTEGER,
   WriteCARDINAL, ReadCARDINAL, WriteBOOLEAN, ReadBOOLEAN,
   WriteREAL, ReadREAL, WriteBITSET, ReadBITSET,
   WriteBasedCARDINAL, ReadBasedCARDINAL, WriteChars, ReadChars,
   WriteString, ReadString, WhiteSpace, EndOfLine;

FROM SysStreams IMPORT sysIn, sysOut, sysErr;

"))

(defun m2-type ()
  (interactive)
  (insert "TYPE")
  (m2-newline)
  (m2-tab))

(defun m2-until ()
  (interactive)
  (insert "REPEAT")
  (m2-newline)
  (m2-newline)
  (insert "UNTIL ")
  (insert (read-string "<boolean-expression>: ") ";")
  (end-of-line 0)
  (m2-tab))

(defun m2-var ()
  (interactive)
  (m2-newline)
  (insert "VAR")
  (m2-newline)
  (m2-tab))

(defun m2-while ()
  (interactive)
  (insert "WHILE ")
  (let ((name (read-string "<boolean-expression>: ")))
    (insert name " DO" )
    (m2-newline)
    (m2-newline)
    (insert "END (* while " name " *);"))
  (end-of-line 0)
  (m2-tab))

(defun m2-export ()
  (interactive)
  (insert "EXPORT QUALIFIED "))

(defun m2-import ()
  (interactive)
  (insert "FROM ")
  (insert (read-string "Module: "))
  (insert " IMPORT "))

(defun m2-begin-comment ()
  (interactive)
  (if (not (bolp))
      (indent-to comment-column 0))
  (insert "(*  "))

(defun m2-end-comment ()
  (interactive)
  (if (not (bolp))
      (indent-to end-comment-column))
  (insert "*)"))

(defun m2-compile ()
  (interactive)
  (setq modulename (buffer-name))
  (compile (concat m2-compile-command " " modulename)))

(defun m2-link ()
  (interactive)
  (setq modulename (buffer-name))
  (if m2-link-name
      (compile (concat m2-link-command " " m2-link-name))
    (compile (concat m2-link-command " "
		     (setq m2-link-name (read-string "Name of executable: "
						     modulename))))))

(defun execute-monitor-command (command)
  (let* ((shell shell-file-name)
	 (csh (equal (file-name-nondirectory shell) "csh")))
    (call-process shell nil t t "-cf" (concat "exec " command))))

(defun m2-visit ()
  (interactive)
  (let ((deffile nil)
	(modfile nil)
	modulename)
    (save-excursion
      (setq modulename
	    (read-string "Module name: "))
      (switch-to-buffer "*Command Execution*")
      (execute-monitor-command (concat "m2whereis " modulename))
      (goto-char (point-min))
      (condition-case ()
	  (progn (re-search-forward "\\(.*\\.def\\) *$")
		 (setq deffile (buffer-substring (match-beginning 1)
						 (match-end 1))))
	(search-failed ()))
      (condition-case ()
	  (progn (re-search-forward "\\(.*\\.mod\\) *$")
		 (setq modfile (buffer-substring (match-beginning 1)
						 (match-end 1))))
	(search-failed ()))
      (if (not (or deffile modfile))
	  (error "I can find neither definition nor implementation of %s"
		 modulename)))
    (cond (deffile
	    (find-file deffile)
	    (if modfile
		(save-excursion
		  (find-file modfile))))
	  (modfile
	   (find-file modfile)))))

(defun m2-toggle ()
  "Toggle between .mod and .def files for the module."
  (interactive)
  (cond ((string-equal (substring (buffer-name) -4) ".def")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -4) ".mod")))
	((string-equal (substring (buffer-name) -4) ".mod")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -4)  ".def")))
	((string-equal (substring (buffer-name) -3) ".mi")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -3)  ".md")))
	((string-equal (substring (buffer-name) -3) ".md")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -3)  ".mi")))))

;;; modula2.el ends here
