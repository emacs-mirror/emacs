;;; ada-stmt.el - An extension to Ada mode for inserting statement templates.

;; Copyright (C) 1987, 1993, 1994, 1996, 1997 Free Software Foundation, Inc.

;; Ada Core Technologies's version:   $Revision: 1.1 $ 

;; Authors: Daniel Pfeiffer, Markus Heritsch, Rolf Ebert <ebert@waporo.muc.de>
;; Maintainer: Rolf Ebert <ebert@waporo.muc.de>
;; Keywords: languages, ada
;; Rolf Ebert's version: 2.26

;;; Commentary:

;;
;; put the following statement in your .emacs:
;; (require 'ada-stmt)
;;

;;; History:

;; Created May 1987.
;; Original version from V. Bowman as in ada.el of Emacs-18
;; (borrowed heavily from Mick Jordan's Modula-2 package for GNU,
;; as modified by Peter Robinson, Michael Schmidt, and Tom Perrine.)
;;
;; Sep 1993. Daniel Pfeiffer <pfeiffer@cict.fr> (DP)
;; Introduced statement.el for smaller code and user configurability.
;;
;; Nov 1993. Rolf Ebert <ebert@enpc.fr> (RE) Moved the
;; skeleton generation into this separate file. The code still is
;; essentially written by DP
;; 
;; Adapted Jun 1994. Markus Heritsch
;; <Markus.Heritsch@studbox.uni-stuttgart.de> (MH)
;; added menu bar support for templates
;;
;; 1994/12/02  Christian Egli <cegli@hcsd.hac.com>
;; General cleanup and bug fixes.
;;
;; 1995/12/20  John Hutchison <hutchiso@epi.syr.ge.com>
;; made it work with skeleton.el from emacs-19.30. Several
;; enhancements and bug fixes.

;; BUGS:
;;;> I have the following suggestions for the function template: 1) I
;;;> don't want it automatically assigning it a name for the return variable. I
;;;> never want it to be called "Result" because that is nondescriptive. If you
;;;> must define a variable, give me the ability to specify its name.
;;;>
;;;> 2) You do not provide a type for variable 'Result'. Its type is the same
;;;> as the function's return type, which the template knows, so why force me
;;;> to type it in?
;;;>

;;;It would be nice if one could configure such layout details separately
;;;without patching the LISP code. Maybe the metalanguage used in ada-stmt.el
;;;could be taken even further, providing the user with some nice syntax
;;;for describing layout. Then my own hacks would survive the next
;;;update of the package :-)


;;; Code:

(eval-when-compile
  (condition-case nil  (require 'skeleton)
    (error nil)))
(require 'easymenu)

(defun ada-stmt-add-to-ada-menu ()
  "Add a new submenu to the Ada menu"
  (interactive)
  (let ((menu  '(["Header" ada-header t]
		 ["-" nil nil]
		 ["Package Body" ada-package-body t]
		 ["Package Spec" ada-package-spec t]
		 ["Function Spec" ada-function-spec t]
		 ["Procedure Spec" ada-procedure-spec t]
		 ["Proc/func Body" ada-subprogram-body t]
		 ["Task Body" ada-task-body t]
		 ["Task Spec" ada-task-spec t]
		 ["Declare Block" ada-declare-block t]
		 ["Exception Block" ada-exception-block t]
		 ["--" nil nil]
		 ["Entry" ada-entry t]
		 ["Entry family" ada-entry-family t]
		 ["Select" ada-select t]
		 ["Accept" ada-accept t]
		 ["Or accept" ada-or-accep t]
		 ["Or delay" ada-or-delay t]
		 ["Or terminate" ada-or-terminate t]
		 ["---" nil nil]
		 ["Type" ada-type t]
		 ["Private" ada-private t]
		 ["Subtype" ada-subtype t]
		 ["Record" ada-record t]
		 ["Array" ada-array t]
		 ["----" nil nil]
		 ["If" ada-if t]
		 ["Else" ada-else t]
		 ["Elsif" ada-elsif t]
		 ["Case" ada-case t]
		 ["-----" nil nil]
		 ["While Loop" ada-while-loop t]
		 ["For Loop" ada-for-loop t]
		 ["Loop" ada-loop t]
		 ["------" nil nil]
		 ["Exception" ada-exception t]
		 ["Exit" ada-exit t]
		 ["When" ada-when t])))
    (if ada-xemacs
	(progn
	  (add-to-list 'menu "Statements")
	  (add-submenu '("Ada") menu))

      (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Statements]
	(cons "Statements" (easy-menu-create-menu "Statements" menu)) t))
    ))




(defun ada-func-or-proc-name ()
  ;; Get the name of the current function or procedure."
  (save-excursion
    (let ((case-fold-search t))
      (if (re-search-backward ada-procedure-start-regexp nil t)
	  (buffer-substring (match-beginning 2) (match-end 2))
	"NAME?"))))

(defvar ada-template-map nil
  "Keymap used in Ada mode for smart template operations.")

(define-key ada-mode-map "\C-cth" 'ada-header)
(define-key ada-mode-map "\C-ct\C-a" 'ada-array)
(define-key ada-mode-map "\C-ctb" 'ada-exception-block)
(define-key ada-mode-map "\C-ctd" 'ada-declare-block)
(define-key ada-mode-map "\C-ctc" 'ada-case)
(define-key ada-mode-map "\C-ct\C-e"  'ada-elsif)
(define-key ada-mode-map "\C-cte"      'ada-else)
(define-key ada-mode-map "\C-ct\C-k"   'ada-package-spec)
(define-key ada-mode-map "\C-ctk"      'ada-package-body)
(define-key ada-mode-map "\C-ct\C-p"   'ada-procedure-spec)
(define-key ada-mode-map "\C-ctp"      'ada-subprogram-body)
(define-key ada-mode-map "\C-ct\C-f"   'ada-function-spec)
(define-key ada-mode-map "\C-ctf"      'ada-for-loop)
(define-key ada-mode-map "\C-cti"      'ada-if)
(define-key ada-mode-map "\C-ctl"      'ada-loop)
(define-key ada-mode-map "\C-ct\C-r"   'ada-record)
(define-key ada-mode-map "\C-ct\C-s"   'ada-subtype)
(define-key ada-mode-map "\C-ctS"      'ada-tabsize)
(define-key ada-mode-map "\C-ct\C-t"   'ada-task-spec)
(define-key ada-mode-map "\C-ctt"      'ada-task-body)
(define-key ada-mode-map "\C-ct\C-y"   'ada-type)
(define-key ada-mode-map "\C-ct\C-v"   'ada-private)
(define-key ada-mode-map "\C-ctu"      'ada-use)
(define-key ada-mode-map "\C-ct\C-u"   'ada-with)
(define-key ada-mode-map "\C-ct\C-w"   'ada-when)
(define-key ada-mode-map "\C-ctw"      'ada-while-loop)
(define-key ada-mode-map "\C-ct\C-x"   'ada-exception)
(define-key ada-mode-map "\C-ctx"      'ada-exit)

;;; ---- statement skeletons ------------------------------------------

(define-skeleton ada-array
  "Insert array type definition.
Uses the minibuffer to prompt for component type and index subtypes."
  ()
  "array (" ("index definition: " str ", " ) -2 ") of " _ ?\;)


(define-skeleton ada-case
  "Build skeleton case statement.
Prompt for the selector expression. Also builds the first when clause."
  "[selector expression]: "
  "case " str " is" \n
  > "when " ("discrete choice: " str " | ") -3 " =>" \n
  > _ \n
  < < "end case;")


(define-skeleton ada-when
  "Start a case statement alternative with a when clause."
  ()
  < "when " ("discrete choice: " str " | ") -3 " =>" \n
  >)


(define-skeleton ada-declare-block
  "Insert a block with a declare part.
Indent for the first declaration."
  "[block name]: "
  < str & ?: & \n
  > "declare" \n
  > _ \n
  < "begin" \n
  > \n
  < "end " str | -1 ?\;)


(define-skeleton ada-exception-block
  "Insert a block with an exception part.
Indent for the first line of code."
  "[block name]: "
  < str & ?: & \n
  > "begin" \n
  > _ \n
  < "exception" \n
  > \n
  < "end " str | -1 ?\;)


(define-skeleton ada-exception
  "Insert an indented exception part into a block."
  ()
  < "exception" \n
  >)


(define-skeleton ada-exit-1
  "Insert then exit condition of the exit statement, prompting for condition."
  "[exit condition]: "
  "when " str | -5)


(define-skeleton ada-exit
  "Insert an exit statement, prompting for loop name and condition."
  "[name of loop to exit]: "
  "exit " str & ?\ 
  (ada-exit-1)
  | -1 ?\;)

;;;###autoload
(defun ada-header ()
  "Insert a descriptive header at the top of the file."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (if (fboundp 'make-header)
	(make-header)
      (ada-header-tmpl))))


(define-skeleton ada-header-tmpl
  "Insert a comment block containing the module title, author, etc."
  "[Description]: "
  "--                              -*- Mode: Ada -*-"
  "\n-- Filename        : " (buffer-name)
  "\n-- Description     : " str
  "\n-- Author          : " (user-full-name) 
  "\n-- Created On      : " (current-time-string)
  "\n-- Last Modified By: ."
  "\n-- Last Modified On: ."
  "\n-- Update Count    : 0"
  "\n-- Status          : Unknown, Use with caution!"
  "\n")


(define-skeleton ada-display-comment
  "Inserts three comment lines, making a display comment."
  ()
  "--\n-- " _ "\n--")


(define-skeleton ada-if
  "Insert skeleton if statment, prompting for a boolean-expression."
  "[condition]: "
  "if " str " then" \n
  > _ \n
  < "end if;")


(define-skeleton ada-elsif
  "Add an elsif clause to an if statement, 
prompting for the boolean-expression."
  "[condition]: "
  < "elsif " str " then" \n
  >)


(define-skeleton ada-else
  "Add an else clause inside an if-then-end-if clause."
  ()
  < "else" \n
  >)


(define-skeleton ada-loop
  "Insert a skeleton loop statement.  The exit statement is added by hand."
  "[loop name]: "
  < str & ?: & \n
  > "loop" \n
  > _ \n
  < "end loop " str | -1 ?\;)


(define-skeleton ada-for-loop-prompt-variable
  "Prompt for the loop variable."
  "[loop variable]: "
  str)


(define-skeleton ada-for-loop-prompt-range
  "Prompt for the loop range."
  "[loop range]: "
  str)


(define-skeleton ada-for-loop
  "Build a skeleton for-loop statement, prompting for the loop parameters."
  "[loop name]: "
  < str & ?: & \n
  > "for "
  (ada-for-loop-prompt-variable)
  " in "
  (ada-for-loop-prompt-range)
  " loop" \n
  > _ \n
  < "end loop " str | -1 ?\;)


(define-skeleton ada-while-loop-prompt-entry-condition
  "Prompt for the loop entry condition."
  "[entry condition]: "
  str)


(define-skeleton ada-while-loop
  "Insert a skeleton while loop statement."
  "[loop name]: "
  < str & ?: & \n
  > "while "
  (ada-while-loop-prompt-entry-condition)
  " loop" \n
  > _ \n
  < "end loop " str | -1 ?\;)


(define-skeleton ada-package-spec
  "Insert a skeleton package specification."
  "[package name]: "
  "package " str  " is" \n
  > _ \n
  < "end " str ?\;)


(define-skeleton ada-package-body
  "Insert a skeleton package body --  includes a begin statement."
  "[package name]: "
  "package body " str " is" \n
  > _ \n
;  < "begin" \n
  < "end " str ?\;)


(define-skeleton ada-private
  "Undent and start a private section of a package spec. Reindent."
  ()
  < "private" \n
  >)


(define-skeleton ada-function-spec-prompt-return
  "Prompts for function result type."
  "[result type]: "
  str)


(define-skeleton ada-function-spec
  "Insert a function specification.  Prompts for name and arguments."
  "[function name]: "
  "function " str 
  " (" ("[parameter_specification]: " str "; " ) -2 ")"
  " return "
  (ada-function-spec-prompt-return)
  ";" \n )


(define-skeleton ada-procedure-spec
  "Insert a procedure specification, prompting for its name and arguments."
  "[procedure name]: "
  "procedure " str 
  " (" ("[parameter_specification]: " str "; " ) -2 ")"
  ";" \n )


(define-skeleton ada-subprogram-body
  "Insert frame for subprogram body.
Invoke right after `ada-function-spec' or `ada-procedure-spec'."
  ()
  ;; Remove `;' from subprogram decl
  (save-excursion
    (let ((pos (1+ (point))))
      (ada-search-ignore-string-comment ada-subprog-start-re t nil)
      (if (ada-search-ignore-string-comment "(" nil pos t 'search-forward)
	  (progn
	    (backward-char 1)
	    (forward-sexp 1)))
      )
    (if (looking-at ";")
        (delete-char 1)))
  " is" \n
   _ \n
   < "begin" \n
   \n
   < "exception" \n
   "when others => null;" \n
   < < "end "
  (ada-func-or-proc-name)
  ";" \n)


(define-skeleton ada-separate
  "Finish a body stub with `separate'."
  ()
  > "separate;" \n
  <)


;(define-skeleton ada-with
;  "Inserts a with clause, prompting for the list of units depended upon."
;  "[list of units depended upon]: "
;  "with " str ?\;)

;(define-skeleton ada-use
;  "Inserts a use clause, prompting for the list of packages used."
;  "[list of packages used]: "
;  "use " str ?\;)
 

(define-skeleton ada-record
  "Insert a skeleton record type declaration."
  ()
  "record" \n
  > _ \n
  < "end record;")


(define-skeleton ada-subtype
  "Start insertion of a subtype declaration, prompting for the subtype name."
  "[subtype name]: "
  "subtype " str " is " _ ?\;
  (not (message "insert subtype indication.")))


(define-skeleton ada-type
  "Start insertion of a type declaration, prompting for the type name."
  "[type name]: "
  "type " str ?\(
  ("[discriminant specs]: " str " ")
  | (backward-delete-char 1) | ?\)
  " is "
  (not (message "insert type definition.")))


(define-skeleton ada-task-body
  "Insert a task body, prompting for the task name."
  "[task name]: "
  "task body " str " is\n"
  "begin\n"
  > _ \n
  < "end " str ";" )


(define-skeleton ada-task-spec
  "Insert a task specification, prompting for the task name."
  "[task name]: "
  "task " str 
  " (" ("[discriminant]: " str "; ") ") is\n"
  > "entry " _ \n
  <"end " str ";" )
  

(define-skeleton ada-get-param1
  "Prompt for arguments and if any enclose them in brackets."
  ()
  ("[parameter_specification]: " str "; " ) & -2 & ")"
  )


(define-skeleton ada-get-param
  "Prompt for arguments and if any enclose them in brackets."
  ()
  " (" 
  (ada-get-param1) | -2
  )


(define-skeleton ada-entry
  "Insert a task entry, prompting for the entry name."
  "[entry name]: "
  "entry " str   
  (ada-get-param)
  ";" \n
;  (ada-indent-current)
)


(define-skeleton ada-entry-family-prompt-discriminant
  "Insert a entry specification, prompting for the entry name."
  "[discriminant name]: "
  str)


(define-skeleton ada-entry-family
  "Insert a entry specification, prompting for the entry name."
  "[entry name]: "
  "entry " str
  " (" (ada-entry-family-prompt-discriminant) ")"
  (ada-get-param)
  ";" \n
  ;(ada-indent-current)
)


(define-skeleton ada-select
  "Insert a select block."
  ()
  "select\n"
  > _ \n
  < "end select;")


(define-skeleton ada-accept-1
  "Insert a condition statement, prompting for the condition name."
  "[condition]: " 
  "when " str | -5 )


(define-skeleton ada-accept-2
  "Insert an accept statement, prompting for the name and arguments."
  "[accept name]: " 
  > "accept " str 
  (ada-get-param)
;  " (" ("[parameter_specification]: " str "; ") -2 ")"
  " do" \n
  > _ \n
  < "end " str ";" )


(define-skeleton ada-accept
  "Insert an accept statement (prompt for condition, name and arguments)."
  ()
  > (ada-accept-1) & " =>\n"
  (ada-accept-2)
)


(define-skeleton ada-or-accept
  "Insert a or statement, prompting for the condition name."
  ()
  < "or\n"
  (ada-accept)
)


(define-skeleton ada-or-delay
  "Insert a delay statement, prompting for the delay value."
  "[delay value]: " 
  < "or\n"
  > "delay " str ";")
  

(define-skeleton ada-or-terminate
  "Insert a terminate statement."
  ()
  < "or\n"
  > "terminate;")


;; ---- 
(defun ada-adjust-case-skeleton ()
  "Adjusts the case of the text inserted by a skeleton."
  (save-excursion 
    (let ((aa-end (point)))
      (ada-adjust-case-region 
       (progn (goto-char beg) (forward-word -1) (point)) 
       (goto-char aa-end))
      )))
(add-hook 'ada-mode-hook '(lambda ()
                            (setq skeleton-further-elements 
                                  '((< '(backward-delete-char-untabify
                                         (min ada-indent (current-column))))))
                            (add-hook 'skeleton-end-hook
                                      'ada-adjust-case-skeleton)))

(add-hook 'ada-mode-hook 'ada-stmt-add-to-ada-menu)

(provide 'ada-stmt)

;;; ada-stmt.el ends here
