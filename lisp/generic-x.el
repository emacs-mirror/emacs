;;; generic-x.el --- Extra Modes for generic-mode

;; Copyright (C) 1997, 1998 Free Software Foundation, Inc.

;; Author:  Peter Breton <pbreton@cs.umb.edu>
;; Created: Tue Oct 08 1996
;; Keywords: generic, comment, font-lock

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file contains some pre-defined generic-modes.
;;
;; INSTALLATION:
;;
;; Add this line to your .emacs file:
;;
;;   (require 'generic-x)
;;
;; You can decide which modes to load by setting the variable
;; `generic-extras-enable-list'. Some platform-specific modes are
;; affected by the variables `generic-define-mswindows-modes' and
;; `generic-define-unix-modes' (which see).
;;
;; You can also send in new modes; if the file types a reasonably common,
;; we would like to install them.
;;
;; PROBLEMS WHEN USED WITH FOLDING MODE:
;;
;; [The following relates to the obsolete selective-display technique.
;; Folding mode should use invisible text properties instead. -- Dave
;; Love]
;;
;; From Anders Lindgren <andersl@csd.uu.se>
;;
;; Problem summary: Wayne Adams has found a problem when using folding
;; mode in conjuction with font-lock for a mode defined in
;; `generic-x.el'.
;;
;; The problem, as Wayne described it, was that error messages of the
;; following form appeared when both font-lock and folding are used:
;;
;; >      - various msgs including "Fontifying region...(error Stack
;; > overflow in regexp matcher)" appear
;;
;; I have just tracked down the cause of the problem.  The regexp:s in
;; `generic-x.el' does not take into account the way that folding
;; hides sections of the buffer.  The technique is known as
;; `selective-display' and has been available for a very long time (I
;; started using it back in the good old' Emacs 18 days).  Basically, a
;; section is hidden by creating one very long line were the newline
;; character (C-j) is replaced by a linefeed (C-m) character.
;;
;; Many other hiding packages, besides folding, use the same technique,
;; the problem should occur when using them as well.
;;
;; The erroronous lines in `generic-extras' look like the following (this
;; example is from the `ini' section):
;;
;;     '(("^\\(\\[.*\\]\\)"   1 'font-lock-constant-face)
;;       ("^\\(.*\\)="        1 'font-lock-variable-name-face)
;;
;; The intention of these lines is to highlight lines of the following
;; form:
;;
;; [foo]
;; bar = xxx
;;
;; However, since the `.' regexp symbol match the linefeed character the
;; entire folded section is searched, resulting in a regexp stack
;; overflow.
;;
;; Solution suggestion 2: Instead of using ".", use the sequence
;; "[^\n\r]".  This will make the rules behave just as before, but they
;; will work together with selective-display.

;;; Code:

(require 'generic)
(require 'font-lock)

(defgroup generic-x nil
  "Extra modes for generic mode."
  :prefix "generic-"
  :group 'generic
  :version "20.3")

(defcustom generic-extras-enable-list nil
  "*List of generic modes to enable by default.
Each entry in the list should be a symbol.
The variables `generic-define-mswindows-modes' and `generic-define-unix-modes'
also affect which generic modes are defined.
Please note that if you set this variable after generic-x is loaded,
you must reload generic-x to enable the specified modes."
  :group 'generic-x
  :type  '(repeat sexp)
  )

(defcustom generic-define-mswindows-modes
  (memq system-type (list 'windows-nt 'ms-dos))
  "*If non-nil, some MS-Windows specific generic modes will be defined."
  :group 'generic-x
  :type  'boolean
  )

(defcustom generic-define-unix-modes
  (not (memq system-type (list 'windows-nt 'ms-dos)))
  "*If non-nil, some Unix specific generic modes will be defined."
  :group 'generic-x
  :type  'boolean
  )

(and generic-define-mswindows-modes
    (setq generic-extras-enable-list
	  (append (list 'bat-generic-mode 'ini-generic-mode
			'inf-generic-mode 'rc-generic-mode
			'reg-generic-mode 'rul-generic-mode
			'hosts-generic-mode
			'apache-conf-generic-mode
			'apache-log-generic-mode
			)
		  generic-extras-enable-list)))

(and generic-define-unix-modes
     (setq generic-extras-enable-list
	   (append (list 'apache-conf-generic-mode
			 'apache-log-generic-mode
			 'samba-generic-mode
			 'hosts-generic-mode  'fvwm-generic-mode
			 'x-resource-generic-mode
			 'alias-generic-mode
			 'inetd-conf-generic-mode
			 'etc-services-generic-mode
			 'etc-passwd-generic-mode
			 'etc-fstab-generic-mode
			 )
		   generic-extras-enable-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic-modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Apache
(and
 (memq 'apache-conf-generic-mode generic-extras-enable-list)

(define-generic-mode 'apache-conf-generic-mode
   (list ?#)
   nil
   '(("^\\s-*\\(<.*>\\)"       1 'font-lock-constant-face)
     ("^\\(\\sw+\\)\\s-"  1 'font-lock-variable-name-face))
   (list "srm\\.conf\\'" "httpd\\.conf\\'" "access\\.conf\\'")
   (list
    (function
     (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([-A-Za-z0-9_]+\\)" 1)
	      ("*Directories*" "^\\s-*<Directory\\s-*\\([^>]+\\)>" 1)
	      ("*Locations*"   "^\\s-*<Location\\s-*\\([^>]+\\)>" 1)
	      ))
       )))
   "Generic mode for Apache or HTTPD configuration files."))

(and
 (memq 'apache-log-generic-mode generic-extras-enable-list)

(define-generic-mode 'apache-log-generic-mode
  nil
  nil
  ;; Hostname ? user date request return-code number-of-bytes
  '(("^\\([-a-zA-z0-9.]+\\) - [-A-Za-z]+ \\(\\[.*\\]\\)"
     (1 font-lock-constant-face)
     (2 font-lock-variable-name-face))
    )
  (list "access_log\\'")
  nil
  "Mode for Apache log files"))

;;; Samba
(and
 (memq 'samba-generic-mode generic-extras-enable-list)

(define-generic-mode 'samba-generic-mode
   (list ?\; ?#)
   nil
   '(
     ("^\\(\\[.*\\]\\)"   1 'font-lock-constant-face)
     ("^\\s-*\\(.+\\)=\\([^\r\n]*\\)"
      (1 'font-lock-variable-name-face)
      (2 'font-lock-type-face))
     )
   (list "smb\\.conf\\'")
   (list 'generic-bracket-support)
   "Generic mode for Samba configuration files."))

;;; Fvwm
;; This is pretty basic. Also, modes for other window managers could
;; be defined as well.
(and
(memq 'fvwm-generic-mode generic-extras-enable-list)

(define-generic-mode 'fvwm-generic-mode
   (list ?#)
   (list
    "AddToMenu"
    "AddToFunc"
    "ButtonStyle"
    "EndFunction"
    "EndPopup"
    "Function"
    "IconPath"
    "Key"
    "ModulePath"
    "Mouse"
    "PixmapPath"
    "Popup"
    "Style"
    )
   nil
   (list "\\.fvwmrc\\'" "\\.fvwm2rc\\'")
   nil
   "Generic mode for FVWM configuration files."))

;;; X Resource
;; I'm pretty sure I've seen an actual mode to do this, but I don't
;; think it's standard with Emacs
(and
 (memq 'x-resource-generic-mode generic-extras-enable-list)

(define-generic-mode 'x-resource-generic-mode
   (list ?!)
   nil
   '(("^\\([^:\n]+:\\)" 1 'font-lock-variable-name-face))
   (list "\\.Xdefaults\\'" "\\.Xresources\\'" "\\.Xenvironment\\'" "\\.ad\\'")
   nil
   "Generic mode for X Resource configuration files."))

;;; Hosts
(and
 (memq 'hosts-generic-mode generic-extras-enable-list)

(define-generic-mode 'hosts-generic-mode
   (list ?#)
   (list "localhost")
   '(("\\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)" 1 'font-lock-constant-face))
   (list "[hH][oO][sS][tT][sS]\\'")
   nil
   "Generic mode for HOSTS files."))

;;; Windows INF files
(and
 (memq 'inf-generic-mode generic-extras-enable-list)

(define-generic-mode 'inf-generic-mode
   (list ?\;)
   nil
   '(("^\\(\\[.*\\]\\)"   1 'font-lock-constant-face))
   (list "\\.[iI][nN][fF]\\'")
   (list 'generic-bracket-support)
   "Generic mode for MS-Windows INF files."))

;;; Windows INI files
;; Should define escape character as well!
(and
 (memq 'ini-generic-mode generic-extras-enable-list)

(define-generic-mode 'ini-generic-mode
   (list ?\;)
   nil
   '(("^\\(\\[.*\\]\\)"   1 'font-lock-constant-face)
     ("^\\([^=\n\r]*\\)=\\([^\n\r]*\\)$"
      (1 font-lock-function-name-face)
      (2 font-lock-variable-name-face)))
   (list "\\.[iI][nN][iI]\\'")
   (list
    (function
     (lambda ()
       (setq imenu-generic-expression
	     '((nil "^\\[\\(.*\\)\\]" 1)
	       ("*Variables*" "^\\s-*\\([^=]+\\)\\s-*=" 1)))
       )))
    "Generic mode for MS-Windows INI files."))

;;; Windows REG files
;;; Unfortunately, Windows 95 and Windows NT have different REG file syntax!
(and
 (memq 'reg-generic-mode generic-extras-enable-list)

(define-generic-mode 'reg-generic-mode
   '(?\;)
   '("key" "classes_root" "REGEDIT" "REGEDIT4")
   '(("\\(\\[.*]\\)"     1 'font-lock-constant-face)
     ("^\\([^\n\r]*\\)\\s-*="  1 'font-lock-variable-name-face))
   '("\\.[rR][eE][gG]\\'")
    (list
     (function
      (lambda ()
	(setq imenu-generic-expression
	'((nil "^\\s-*\\(.*\\)\\s-*=" 1))))))
    "Generic mode for MS-Windows Registry files."))

;;; DOS/Windows BAT files
(if (not (memq 'bat-generic-mode generic-extras-enable-list))
    nil
(define-generic-mode 'bat-generic-mode
    nil
    nil
    (list
     ;; Make this one first in the list, otherwise comments will
     ;; be over-written by other variables
     (list "^[@ \t]*\\([rR][eE][mM][^\n\r]*\\)" 1 'font-lock-comment-face t)
     (list "^[ \t]*\\(::.*\\)"		        1 'font-lock-comment-face t)
     (list
      "^[@ \t]*\\([bB][rR][eE][aA][kK]\\|[vV][eE][rR][iI][fF][yY]\\)[ \t]+\\([oO]\\([nN]\\|[fF][fF]\\)\\)"
      '(1 font-lock-builtin-face)
      '(2 font-lock-constant-face t t))
     ;; Any text (except ON/OFF) following ECHO is a string.
     (list
      "^[@ \t]*\\([eE][cC][hH][oO]\\)[ \t]+\\(\\([oO]\\([nN]\\|[fF][fF]\\)\\)\\|\\([^>|\r\n]+\\)\\)"
      '(1 font-lock-builtin-face)
      '(3 font-lock-constant-face t t)
      '(5 font-lock-string-face t t))
     ;; These keywords appear as the first word on a line.  (Actually, they
     ;; can also appear after "if ..." or "for ..." clause, but since they
     ;; are frequently used in simple text, we punt.)
     ;; In `generic-bat-mode-setup-function' we make the keywords
     ;; case-insensitive
     (generic-make-keywords-list
      (list
       "for"
       "if"
       )
      'font-lock-keyword-face "^[@ \t]*")
     ;; These keywords can be anywhere on a line
     ;; In `generic-bat-mode-setup-function' we make the keywords
     ;; case-insensitive
     (generic-make-keywords-list
      (list
       "do"
       "exist"
       "errorlevel"
       "goto"
       "not"
       ) 'font-lock-keyword-face)
     ;; These are built-in commands.  Only frequently-used ones are listed.
     (generic-make-keywords-list
      (list
       "CALL"	    "call"	 "Call"
       "CD"	    "cd"	 "Cd"
       "CLS"	    "cls"	 "Cls"
       "COPY"	    "copy"	 "Copy"
       "DEL"	    "del"	 "Del"
       "ECHO"	    "echo"	 "Echo"
       "MD"	    "md"	 "Md"
       "PATH"	    "path"	 "Path"
       "PAUSE"	    "pause"	 "Pause"
       "PROMPT"	    "prompt"	 "Prompt"
       "RD"	    "rd"	 "Rd"
       "REN"	    "ren"	 "Ren"
       "SET"	    "set"	 "Set"
       "START"	    "start"	 "Start"
       "SHIFT"	    "shift"	 "Shift"
       ) 'font-lock-builtin-face "[ \t|\n]")
     (list "^[ \t]*\\(:\\sw+\\)"         1 'font-lock-function-name-face t)
     (list "\\(%\\sw+%\\)"		 1 'font-lock-variable-name-face t)
     (list "\\(%[0-9]\\)"		 1 'font-lock-variable-name-face t)
     (list "\\(/[^/ \"\t\n]+\\)"	 1 'font-lock-type-face)
     (list "[\t ]+\\([+-][^\t\n\" ]+\\)" 1 'font-lock-type-face)
     (list "[ \t\n|]\\<\\([gG][oO][tT][oO]\\)\\>[ \t]*\\(\\sw+\\)?"
	   '(1 font-lock-keyword-face)
	   '(2 font-lock-function-name-face nil t))
     (list "[ \t\n|]\\<\\([sS][eE][tT]\\)\\>[ \t]*\\(\\sw+\\)?[ \t]*=?"
	   '(1 font-lock-builtin-face)
	   '(2 font-lock-variable-name-face t t))

     )
    (list
     "\\.[bB][aA][tT]\\'"
     "\\`[cC][oO][nN][fF][iI][gG]\\."
     "\\`[aA][uU][tT][oO][eE][xX][eE][cC]\\." )
    (list 'generic-bat-mode-setup-function)
    "Generic mode for MS-Windows BAT files.")

  (defvar bat-generic-mode-syntax-table nil
    "Syntax table in use in bat-generic-mode buffers.")

  (defvar bat-generic-mode-keymap (make-sparse-keymap)
    "Keymap for bet-generic-mode.")

  (defun bat-generic-mode-compile ()
    "Run the current BAT file in a compilation buffer."
    (interactive)
    (let ((compilation-buffer-name-function
	   (function
	    (lambda(ign)
	      (concat "*" (buffer-file-name) "*")))
	      )
	    )
      (compile
       (concat (w32-shell-name) " -c " (buffer-file-name)))))

  (defun bat-generic-mode-run-as-comint ()
    "Run the current BAT file in a comint buffer."
    (interactive)
    (require 'comint)
    (let* ((file (buffer-file-name))
	   (buf-name (concat "*" file "*")))
      (save-excursion
	(set-buffer
	 (get-buffer-create buf-name))
	(erase-buffer)
	(comint-mode)
	(comint-exec
	 buf-name
	 file
	 (w32-shell-name)
	 nil
	 (list
	  "-c"
	  file
	  )
	 )
	(display-buffer buf-name))))

  (define-key bat-generic-mode-keymap "\C-c\C-c" 'bat-generic-mode-compile)

  ;; Make underscores count as words
  (if bat-generic-mode-syntax-table
      nil
    (setq bat-generic-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?_  "w"  bat-generic-mode-syntax-table))

  ;; bat-generic-mode doesn't use the comment functionality of generic-mode
  ;; because it has a three-letter comment-string, so we do it
  ;; here manually instead
  (defun generic-bat-mode-setup-function ()
    (make-local-variable	     'parse-sexp-ignore-comments)
    (make-local-variable	     'comment-start)
    (make-local-variable	     'comment-start-skip)
    (make-local-variable	     'comment-end)
    (setq imenu-generic-expression  '((nil "^:\\(\\sw+\\)" 1))
	  parse-sexp-ignore-comments t
	  comment-end                ""
	  comment-start		     "REM "
	  comment-start-skip	     "[Rr][Ee][Mm] *"
	  )
    (set-syntax-table	      bat-generic-mode-syntax-table)
    ;; Make keywords case-insensitive
    (setq font-lock-defaults (list 'generic-font-lock-defaults nil t))
    (use-local-map bat-generic-mode-keymap)
    )
  )

;;; Mailagent
;; Mailagent is a Unix mail filtering program. Anyone wanna do a generic mode
;; for procmail?
(and
 (memq 'mailagent-rules-generic-mode generic-extras-enable-list)

(define-generic-mode 'mailagent-rules-generic-mode
   (list ?#)
   (list "SAVE" "DELETE" "PIPE" "ANNOTATE" "REJECT")
   '(("^\\(\\sw+\\)\\s-*="         1 'font-lock-variable-name-face)
     ("\\s-/\\([^/]+\\)/[i, \t\n]" 1 'font-lock-constant-face))
   (list "\\.rules\\'")
   (list 'mailagent-rules-setup-function)
   "Mode for Mailagent rules files.")

(defun mailagent-rules-setup-function ()
   (make-local-variable 'imenu-generic-expression)
   (setq imenu-generic-expression
	 '((nil "\\s-/\\([^/]+\\)/[i, \t\n]" 1))))
 )

;; Solaris/Sys V prototype files
(and
 (memq 'prototype-generic-mode generic-extras-enable-list)

(define-generic-mode 'prototype-generic-mode
   (list ?#)
   nil
   '(
     ("^\\([0-9]\\)?\\s-*\\([a-z]\\)\\s-+\\([A-Za-z_]+\\)\\s-+\\([^\n\r]*\\)$"
      (2 font-lock-constant-face)
      (3 font-lock-keyword-face))
     ("^\\([a-z]\\) \\([A-Za-z_]+\\)=\\([^\n\r]*\\)$"
      (1 font-lock-constant-face)
	  (2 font-lock-keyword-face)
	  (3 font-lock-variable-name-face))
     ("^\\(!\\s-*\\(search\\|include\\|default\\)\\)\\s-*\\([^\n\r]*\\)$"
      (1 font-lock-keyword-face)
      (3 font-lock-variable-name-face))
     ("^\\(!\\s-*\\sw+\\)=\\([^\n\r]*\\)$"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face))
     )
   (list "prototype\\'")
   nil
   "Mode for Sys V prototype files."))

;; Solaris/Sys V pkginfo files
(and
 (memq 'pkginfo-generic-mode generic-extras-enable-list)

(define-generic-mode 'pkginfo-generic-mode
   (list ?#)
   nil
   '(
     ("^\\([A-Za-z_]+\\)=\\([^\n\r]*\\)$"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face))
     )
   (list "pkginfo\\'")
   nil
   "Mode for Sys V pkginfo files."))

;; Javascript mode
;; Includes extra keywords from Armando Singer [asinger@MAIL.COLGATE.EDU]
(define-generic-mode 'javascript-generic-mode
  (list "//")
  (list
   "break"
   "case"
   "continue"
   "default"
   "delete"
   "do"
   "else"
   "export"
   "for"
   "function"
   "if"
   "import"
   "in"
   "new"
   "return"
   "switch"
   "this"
   "typeof"
   "var"
   "void"
   "while"
   "with"
   ;; words reserved for ECMA extensions below
   "catch"
   "class"
   "const"
   "debugger"
   "enum"
   "extends"
   "finally"
   "super"
   "throw"
   "try"
   ;; Java Keywords reserved by JavaScript
   "abstract"
   "boolean"
   "byte"
   "char"
   "double"
   "false"
   "final"
   "float"
   "goto"
   "implements"
   "instanceof"
   "int"
   "interface"
   "long"
   "native"
   "null"
   "package"
   "private"
   "protected"
   "public"
   "short"
   "static"
   "synchronized"
   "throws"
   "transient"
   "true"
   )
  (list
   (list "^\\s-*function\\s-+\\([A-Za-z0-9_]+\\)"
	  '(1 font-lock-function-name-face))
     (list "^\\s-*var\\s-+\\([A-Za-z0-9_]+\\)"
	      '(1 font-lock-variable-name-face))
     )
  (list "\\.js\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^function\\s-+\\([A-Za-z0-9_]+\\)" 1)
	      ("*Variables*" "^var\\s-+\\([A-Za-z0-9_]+\\)" 1)
	      ))
      )))
  "Mode for JavaScript files.")

;; VRML files
(define-generic-mode 'vrml-generic-mode
  (list ?#)
  (list
   "DEF"
   "NULL"
   "USE"
   "Viewpoint"
   "ambientIntensity"
   "appearance"
   "children"
   "color"
   "coord"
   "coordIndex"
   "creaseAngle"
   "diffuseColor"
   "emissiveColor"
   "fieldOfView"
   "geometry"
   "info"
   "material"
   "normal"
   "orientation"
   "position"
   "shininess"
   "specularColor"
   "texCoord"
   "texture"
   "textureTransform"
   "title"
   "transparency"
   "type"
   )
  (list
   (list "USE\\s-+\\([-A-Za-z0-9_]+\\)"
	 '(1 font-lock-constant-face))
   (list "DEF\\s-+\\([-A-Za-z0-9_]+\\)\\s-+\\([A-Za-z0-9]+\\)\\s-*{"
	 '(1 font-lock-type-face)
	 '(2 font-lock-constant-face))
   (list "^\\s-*\\([-A-Za-z0-9_]+\\)\\s-*{"
	 '(1 font-lock-function-name-face))
   (list
    "^\\s-*\\(geometry\\|appearance\\|material\\)\\s-+\\([-A-Za-z0-9_]+\\)"
    '(2 font-lock-variable-name-face))
   )
  (list "\\.wrl\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([A-Za-z0-9_]+\\)\\s-*{" 1)
	      ("*Definitions*"
	       "DEF\\s-+\\([-A-Za-z0-9_]+\\)\\s-+\\([A-Za-z0-9]+\\)\\s-*{"
	       1)))
      )))
  "Generic Mode for VRML files.")

;; Java Manifests
(define-generic-mode 'java-manifest-generic-mode
  (list ?#)
  (list
   "Name"
   "Digest-Algorithms"
   "Manifest-Version"
   "Required-Version"
   "Signature-Version"
   "Magic"
   "Java-Bean"
   "Depends-On"
   )
  '(("^Name:\\s-+\\([^\n\r]*\\)$"
     (1 font-lock-variable-name-face))
    ("^\\(Manifest\\|Required\\|Signature\\)-Version:\\s-+\\([^\n\r]*\\)$"
     (2 font-lock-constant-face))
    )
  (list "[mM][aA][nN][iI][fF][eE][sS][tT]\\.[mM][fF]\\'")
  nil
  "Mode for Java Manifest files")

;; Java properties files
(define-generic-mode 'java-properties-generic-mode
  (list ?! ?#)
  nil
  (let ((java-properties-key
	 "\\(\\([-A-Za-z0-9_\\./]\\|\\(\\\\[ =:]\\)\\)+\\)")
	(java-properties-value
	 "\\([^\r\n]*\\)")
	)
    ;; Property and value can be separated in a number of different ways:
    ;;   * whitespace
    ;;   * an equal sign
    ;;   * a colon
    (mapcar
     (function
      (lambda (elt)
	(list
	 (concat "^" java-properties-key elt java-properties-value "$")
	 '(1 font-lock-constant-face)
	 '(4 font-lock-variable-name-face)
	 )))
     ;; These are the separators
     (list ":\\s-*" "\\s-+" "\\s-*=\\s-*")
     )
    )
  nil
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([^#! \t\n\r=:]+\\)" 1)))
      )))
  "Mode for Java properties files.")

;; C shell alias definitions
(and
 (memq 'alias-generic-mode generic-extras-enable-list)

(define-generic-mode 'alias-generic-mode
  (list ?#)
  (list "alias" "unalias")
  '(("^alias\\s-+\\([-A-Za-z0-9_]+\\)\\s-+"
     (1 font-lock-variable-name-face))
    ("^unalias\\s-+\\([-A-Za-z0-9_]+\\)\\s-*$"
     (1 font-lock-variable-name-face))
    )
  (list "alias\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\(alias\\|unalias\\)\\s-+\\([-a-zA-Z0-9_]+\\)" 2)))
      )))
  "Mode for C Shell alias files.")
)

;;; Windows RC files
;; Contributed by ACorreir@pervasive-sw.com (Alfred Correira)
(and
 (memq 'rc-generic-mode generic-extras-enable-list)

(define-generic-mode 'rc-generic-mode
;;   (list ?\/)
   (list "//")
   '("ACCELERATORS"
     "AUTO3STATE"
     "AUTOCHECKBOX"
     "AUTORADIOBUTTON"
     "BITMAP"
     "BOTTOMMARGIN"
     "BUTTON"
     "CAPTION"
     "CHARACTERISTICS"
     "CHECKBOX"
     "CLASS"
     "COMBOBOX"
     "CONTROL"
     "CTEXT"
     "CURSOR"
     "DEFPUSHBUTTON"
     "DESIGNINFO"
     "DIALOG"
     "DISCARDABLE"
     "EDITTEXT"
     "EXSTYLE"
     "FONT"
     "GROUPBOX"
     "GUIDELINES"
     "ICON"
     "LANGUAGE"
     "LEFTMARGIN"
     "LISTBOX"
     "LTEXT"
     "MENUITEM SEPARATOR"
     "MENUITEM"
     "MENU"
     "MOVEABLE"
     "POPUP"
     "PRELOAD"
     "PURE"
     "PUSHBOX"
     "PUSHBUTTON"
     "RADIOBUTTON"
     "RCDATA"
     "RIGHTMARGIN"
     "RTEXT"
     "SCROLLBAR"
     "SEPARATOR"
     "STATE3"
     "STRINGTABLE"
     "STYLE"
     "TEXTINCLUDE"
     "TOOLBAR"
     "TOPMARGIN"
     "VERSIONINFO"
     "VERSION"
     )
   ;; the choice of what tokens go where is somewhat arbitrary,
   ;; as is the choice of which value tokens are included, as
   ;; the choice of face for each token group
   (list
    (eval-when-compile
      (generic-make-keywords-list
       (list
	"FILEFLAGSMASK"
	"FILEFLAGS"
	"FILEOS"
	"FILESUBTYPE"
	"FILETYPE"
	"FILEVERSION"
	"PRODUCTVERSION"
	) 'font-lock-type-face))
    (eval-when-compile
      (generic-make-keywords-list
       (list
	"BEGIN"
	"BLOCK"
	"END"
	"VALUE"
	) 'font-lock-function-name-face))
    '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
    '("^#[ \t]*define[ \t]+\\(\\sw+\\)("       1 font-lock-function-name-face)
    '("^#[ \t]*\\(elif\\|if\\)\\>"
      ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
       (1 font-lock-constant-face) (2 font-lock-variable-name-face nil t)))
    '("^#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 font-lock-constant-face) (2 font-lock-variable-name-face nil t)))
   (list "\\.[rR][cC]$")
   nil
   "Generic mode for MS-Windows Resource files."))

;; InstallShield RUL files
;; Contributed by  Alfred.Correira@Pervasive.Com
(and
(memq 'rul-generic-mode generic-extras-enable-list)
;;; build the regexp strings using regexp-opt
(defvar installshield-statement-keyword-list
  (list
   "abort"
   "begin"
   "call"
   "case"
   "declare"
   "default"
   "downto"
   "elseif"
   "else"
   "endfor"
   "endif"
   "endswitch"
   "endwhile"
   "end"
   "exit"
   "external"
   "for"
   "function"
   ;; "goto" -- handled elsewhere
   "if"
   "program"
   "prototype"
   "repeat"
   "return"
   "step"
   "switch"
   "then"
   "to"
   "typedef"
   "until"
   "void"
   "while"
   )
  "Statement keywords used in InstallShield 3 and 5.")

(defvar installshield-system-functions-list
  (list
   "AddFolderIcon"
   "AddProfString"
   "AddressString"
   "AppCommand"
   "AskDestPath"
   "AskOptions"
   "AskPath"
   "AskText"
   "AskYesNo"
   "BatchDeleteEx"
   "BatchFileLoad"
   "BatchFileSave"
   "BatchFind"
   "BatchGetFileName"
   "BatchMoveEx"
   "BatchSetFileName"
   "ChangeDirectory"
   "CloseFile"
   "CmdGetHwndDlg"
   "ComponentAddItem"                ; differs between IS3 and IS5
   "ComponentCompareSizeRequired"    ; IS5 only
   "ComponentDialog"
   "ComponentError"                  ; IS5 only
   "ComponentFileEnum"               ; IS5 only
   "ComponentFileInfo"               ; IS5 only
   "ComponentFilterLanguage"         ; IS5 only
   "ComponentFilterOS"               ; IS5 only
   "ComponentGetData"                ; IS5 only
   "ComponentGetItemInfo"            ; IS3 only
   "ComponentGetItemSize"            ; differs between IS3 and IS5
   "ComponentIsItemSelected"         ; differs between IS3 and IS5
   "ComponentListItems"
   "ComponentMoveData"               ; IS5 only
   "ComponentSelectItem"             ; differs between IS3 and IS5
   "ComponentSetData"                ; IS5 only
   "ComponentSetItemInfo"            ; IS3 only
   "ComponentSetTarget"              ; IS5 only
   "ComponentSetupTypeEnum"          ; IS5 only
   "ComponentSetupTypeGetData"       ; IS5 only
   "ComponentSetupTypeSet"           ; IS5 only
   "ComponentTotalSize"
   "ComponentValidate"               ; IS5 only
   "CompressAdd"                     ; IS3 only
   "CompressDel"                     ; IS3 only
   "CompressEnum"                    ; IS3 only
   "CompressGet"                     ; IS3 only
   "CompressInfo"                    ; IS3 only
   "CopyFile"
   "CreateDir"
   "CreateFile"
   "CreateProgramFolder"
   "DeinstallSetReference"           ; IS5 only
   "DeinstallStart"
   "Delay"
   "DeleteDir"
   "DeleteFile"
   "DialogSetInfo"
   "Disable"
   "DoInstall"
   "Do"
   "Enable"
   "EnterDisk"
   "ExistsDir"
   "ExistsDisk"
   "ExitProgMan"
   "EzBatchAddPath"
   "EzBatchAddString"
   "EzBatchReplace"
   "EzConfigAddDriver"
   "EzConfigAddString"
   "EzConfigGetValue"
   "EzConfigSetValue"
   "EzDefineDialog"
   "FileCompare"
   "FileDeleteLine"
   "FileGrep"
   "FileInsertLine"
   "FileSetBeginDefine"              ; IS3 only
   "FileSetEndDefine"                ; IS3 only
   "FileSetPerformEz"                ; IS3 only
   "FileSetPerform"                  ; IS3 only
   "FileSetReset"                    ; IS3 only
   "FileSetRoot"                     ; IS3 only
   "FindAllDirs"
   "FindAllFiles"
   "FindFile"
   "FindWindow"
   "GetDiskSpace"
   "GetDisk"
   "GetEnvVar"
   "GetExtents"
   "GetFileInfo"
   "GetLine"
   "GetProfInt"
   "GetProfString"
   "GetSystemInfo"
   "GetValidDrivesList"
   "GetVersion"
   "GetWindowHandle"
   "InstallationInfo"
   "Is"
   "LaunchApp"
   "LaunchAppAndWait"
   "ListAddItem"
   "ListAddString"
   "ListCount"
   "ListCreate"
   "ListDestroy"
   "ListFindItem"
   "ListFindString"
   "ListGetFirstItem"
   "ListGetFirstString"
   "ListGetNextItem"
   "ListGetNextString"
   "ListReadFromFile"
   "ListSetCurrentItem"
   "ListSetNextItem"
   "ListSetNextString"
   "ListSetIndex"
   "ListWriteToFile"
   "LongPathToQuote"
   "LongPathToShortPath"
   "MessageBox"
   "NumToStr"
   "OpenFileMode"
   "OpenFile"
   "ParsePath"
   "PathAdd"
   "PathDelete"
   "PathFind"
   "PathGet"
   "PathMove"
   "PathSet"
   "Path"
   "PlaceBitmap"
   "PlaceWindow"
   "PlayMMedia"                      ; IS5 only
   "ProgDefGroupType"
   "RegDBCreateKeyEx"
   "RegDBDeleteValue"
   "RegDBGetItem"
   "RegDBKeyExist"
   "RegDBSetItem"
   "RegDBGetKeyValueEx"
   "RegDBSetKeyValueEx"
   "RegDBSetDefaultRoot"
   "RenameFile"
   "ReplaceFolderIcon"
   "ReplaceProfString"
   "SdAskDestPath"
   "SdAskOptions"
   "SdAskOptionsList"
   "SdBitmap"
   "SdCloseDlg"
   "SdComponentAdvCheckSpace"
   "SdComponentAdvInit"
   "SdComponentAdvUpdateSpace"
   "SdComponentDialog"
   "SdComponentDialog2"
   "SdComponentDialogAdv"
   "SdComponentDialogEx"
   "SdComponentDlgCheckSpace"
   "SdComponentMult"
   "SdConfirmNewDir"
   "SdConfirmRegistration"
   "SdDiskSpace"
   "SdDisplayTopics"
   "SdDoStdButton"
   "SdEnablement"
   "SdError"
   "SdFinish"
   "SdFinishInit32"
   "SdFinishReboot"
   "SdGeneralInit"
   "SdGetItemName"
   "SdGetTextExtent"
   "SdGetUserCompanyInfo"
   "SdInit"
   "SdIsShellExplorer"
   "SdIsStdButton"
   "SdLicense"
   "SdMakeName"
   "SdOptionInit"
   "SdOptionSetState"
   "SdOptionsButtons"
   "SdOptionsButtonsInit"
   "SdPlugInProductName"
   "SdProductName"
   "SdRegEnableButton"
   "SdRegExEnableButton"
   "SdRegisterUser"
   "SdRegisterUserEx"
   "SdRemoveEndSpace"
   "SdSelectFolder"
   "SdSetSequentialItems"
   "SdSetStatic"
   "SdSetupTypeEx"                   ; IS5 only
   "SdSetupType"
   "SdShowAnyDialog"
   "SdShowDlgEdit1"
   "SdShowDlgEdit2"
   "SdShowDlgEdit3"
   "SdShowFileMods"
   "SdShowInfoList"
   "SdShowMsg"
   "SdStartCopy"
   "SdUnInit"
   "SdUpdateComponentSelection"
   "SdWelcome"
   "SendMessage"
   "SetColor"
   "SetFont"
   "SetDialogTitle"
   "SetDisplayEffect"                ; IS5 only
   "SetFileInfo"
   "SetForegroundWindow"
   "SetStatusWindow"
   "SetTitle"
   "SetupType"
   "ShowProgramFolder"
   "Split"                           ; IS3 only
   "SprintfBox"
   "Sprintf"
   "StatusUpdate"
   "StrCompare"
   "StrFind"
   "StrGetTokens"
   "StrLength"
   "StrRemoveLastSlash"
   "StrToLower"
   "StrToNum"
   "StrToUpper"
   "StrSub"
   "VarRestore"
   "VarSave"
   "VerCompare"
   "VerGetFileVersion"
   "WaitOnDialog"
   "Welcome"
   "WriteLine"
   "WriteProfString"
   "XCopyFile"
   )
  "System functions defined in InstallShield 3 and 5.")

(defvar installshield-system-variables-list
  (list
   "BATCH_INSTALL"
   "CMDLINE"
   "COMMONFILES"
   "CORECOMPONENTHANDLING"
   "DIALOGCACHE"
   "ERRORFILENAME"
   "FOLDER_DESKTOP"
   "FOLDER_PROGRAMS"
   "FOLDER_STARTMENU"
   "FOLDER_STARTUP"
   "INFOFILENAME"
   "ISRES"
   "ISUSER"
   "ISVERSION"
   "MEDIA"
   "MODE"
   "PROGRAMFILES"
   "SELECTED_LANGUAGE"
   "SRCDIR"
   "SRCDISK"
   "SUPPORTDIR"
   "TARGETDIR"
   "TARGETDISK"
   "UNINST"
   "WINDIR"
   "WINDISK"
   "WINMAJOR"
   "WINSYSDIR"
   "WINSYSDISK"
   )
  "System variables used in InstallShield 3 and 5.")

(defvar installshield-types-list
  (list
   "BOOL"
   "BYREF"
   "CHAR"
   "HIWORD"
   "HWND"
   "INT"
   "LIST"
   "LONG"
   "LOWORD"
   "LPSTR"
   "NUMBER"
   "NUMBERLIST"
   "POINTER"
   "QUAD"
   "RGB"
   "SHORT"
   "STRINGLIST"
   "STRING"
   )
  "Type keywords used in InstallShield 3 and 5.")

;;; some might want to skip highlighting these to improve performance
(defvar installshield-funarg-constants-list
  (list
   "AFTER"
   "APPEND"
   "ALLCONTENTS"
   "BACKBUTTON"
   "BACKGROUNDCAPTION"
   "BACKGROUND"
   "BACK"
   "BASEMEMORY"
   "BEFORE"
   "BIOS"
   "BITMAPICON"
   "BK_BLUE"
   "BK_GREEN"
   "BK_RED"
   "BLUE"
   "BOOTUPDRIVE"
   "CANCEL"
   "CDROM_DRIVE"
   "CDROM"
   "CHECKBOX95"
   "CHECKBOX"
   "CHECKLINE"
   "CHECKMARK"
   "COLORS"
   "COMMANDEX"
   "COMMAND"
   "COMP_NORMAL"
   "COMP_UPDATE_DATE"
   "COMP_UPDATE_SAME"
   "COMP_UPDATE_VERSION"
   "COMPACT"
   "CONTINUE"
   "CPU"
   "CUSTOM"
   "DATE"
   "DEFWINDOWMODE"
   "DIR_WRITEABLE"
   "DIRECTORY"
   "DISABLE"
   "DISK_TOTALSPACE"
   "DISK"
   "DLG_OPTIONS"
   "DLG_PATH"
   "DLG_TEXT"
   "DLG_ASK_YESNO"
   "DLG_ENTER_DISK"
   "DLG_ERR"
   "DLG_INFO_ALTIMAGE"
   "DLG_INFO_CHECKSELECTION"
   "DLG_INFO_KUNITS"
   "DLG_INFO_USEDECIMAL"
   "DLG_MSG_INFORMATION"
   "DLG_MSG_SEVERE"
   "DLG_MSG_WARNING"
   "DLG_STATUS"
   "DLG_WARNING"
   "DLG_USER_CAPTION"
   "DRIVE"
   "ENABLE"
   "END_OF_FILE"
   "END_OF_LIST"
   "ENVSPACE"
   "EQUALS"
   "EXCLUDE_SUBDIR"
   "EXCLUSIVE"
   "EXISTS"
   "EXIT"
   "EXTENDED_MEMORY"
   "EXTENSION_ONLY"
   "FAILIFEXISTS"
   "FALSE"
   "FEEDBACK_FULL"
   "FILE_ATTR_ARCHIVED"
   "FILE_ATTR_DIRECTORY"
   "FILE_ATTR_HIDDEN"
   "FILE_ATTR_NORMAL"
   "FILE_ATTR_READONLY"
   "FILE_ATTR_SYSTEM"
   "FILE_ATTRIBUTE"
   "FILE_DATE"
   "FILE_LINE_LENGTH"
   "FILE_MODE_APPEND"
   "FILE_MODE_BINARYREADONLY"
   "FILE_MODE_BINARY"
   "FILE_MODE_NORMAL"
   "FILE_NO_VERSION"
   "FILE_NOT_FOUND"
   "FILE_SIZE"
   "FILE_TIME"
   "FILENAME_ONLY"
   "FILENAME"
   "FIXED_DRIVE"
   "FOLDER_DESKTOP"
   "FOLDER_PROGRAMS"
   "FOLDER_STARTMENU"
   "FOLDER_STARTUP"
   "FREEENVSPACE"
   "FULLWINDOWMODE"
   "FULL"
   "FONT_TITLE"
   "GREATER_THAN"
   "GREEN"
   "HKEY_CLASSES_ROOT"
   "HKEY_CURRENT_USER"
   "HKEY_LOCAL_MACHINE"
   "HKEY_USERS"
   "HOURGLASS"
   "INCLUDE_SUBDIR"
   "INDVFILESTATUS"
   "INFORMATION"
   "IS_WINDOWSNT"
   "IS_WINDOWS95"
   "IS_WINDOWS"
   "IS_WIN32S"
   "ISTYPE"
   "LANGUAGE_DRV"
   "LANGUAGE"
   "LESS_THAN"
   "LIST_NULL"
   "LISTFIRST"
   "LISTNEXT"
   "LOCKEDFILE"
   "LOGGING"
   "LOWER_LEFT"
   "LOWER_RIGHT"
   "MAGENTA"
   "MOUSE_DRV"
   "MOUSE"
   "NETWORK_DRV"
   "NETWORK"
   "NEXT"
   "NONEXCLUSIVE"
   "NORMALMODE"
   "NOSET"
   "NOTEXISTS"
   "NOWAIT"
   "NO"
   "OFF"
   "ONLYDIR"
   "ON"
   "OSMAJOR"
   "OSMINOR"
   "OS"
   "OTHER_FAILURE"
   "PARALLEL"
   "PARTIAL"
   "PATH_EXISTS"
   "PATH"
   "RED"
   "REGDB_APPPATH_DEFAULT"
   "REGDB_APPPATH"
   "REGDB_BINARY"
   "REGDB_ERR_CONNECTIONEXISTS"
   "REGDB_ERR_CORRUPTEDREGSITRY"
   "REGDB_ERR_INITIALIZATION"
   "REGDB_ERR_INVALIDHANDLE"
   "REGDB_ERR_INVALIDNAME"
   "REGDB_NUMBER"
   "REGDB_STRING_EXPAND"
   "REGDB_STRING_MULTI"
   "REGDB_STRING"
   "REGDB_UNINSTALL_NAME"
   "REMOTE_DRIVE"
   "REMOVALE_DRIVE"
   "REPLACE_ITEM"
   "REPLACE"
   "RESET"
   "RESTART"
   "ROOT"
   "SELFREGISTER"
   "SERIAL"
   "SET"
   "SEVERE"
   "SHAREDFILE"
   "SHARE"
   "SILENTMODE"
   "SRCTARGETDIR"
   "STATUSBAR"
   "STATUSDLG"
   "STATUSOLD"
   "STATUS"
   "STYLE_NORMAL"
   "SW_MAXIMIZE"
   "SW_MINIMIZE"
   "SW_RESTORE"
   "SW_SHOW"
   "SYS_BOOTMACHINE"
   "TIME"
   "TRUE"
   "TYPICAL"
   "UPPER_LEFT"
   "UPPER_RIGHT"
   "VALID_PATH"
   "VERSION"
   "VIDEO"
   "VOLUMELABEL"
   "YELLOW"
   "YES"
   "WAIT"
   "WARNING"
   "WINMAJOR"
   "WINMINOR"
   "WIN32SINSTALLED"
   "WIN32SMAJOR"
   "WIN32SMINOR"
   )
  "Function argument constants used in InstallShield 3 and 5.")

(define-generic-mode 'rul-generic-mode
  ;; Using "/*" and "*/" doesn't seem to be working right
  (list "//")
  installshield-statement-keyword-list
  (list
   ;; preprocessor constructs
   '("#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)"
     1 font-lock-string-face)
   '("#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-constant-face)
     (2 font-lock-variable-name-face nil t))
   ;; indirect string constants
   '("\\(@[A-Za-z][A-Za-z0-9_]+\\)" 1 font-lock-builtin-face)
   ;; gotos
   '("[ \t]*\\(\\sw+:\\)" 1 font-lock-constant-face)
   '("\\<\\(goto\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))
   ;; system variables
   (generic-make-keywords-list
    installshield-system-variables-list
    'font-lock-variable-name-face "[^_]" "[^_]")
   ;; system functions
   (generic-make-keywords-list
    installshield-system-functions-list
    'font-lock-function-name-face "[^_]" "[^_]")
   ;; type keywords
   (generic-make-keywords-list
    installshield-types-list
    'font-lock-type-face "[^_]" "[^_]")
   ;; function argument constants
   (generic-make-keywords-list
    installshield-funarg-constants-list
    'font-lock-variable-name-face "[^_]" "[^_]") ; is this face the best choice?
   )
  (list "\\.[rR][uU][lL]$")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
    '((nil "^function\\s-+\\([A-Za-z0-9_]+\\)" 1)))
      )))
  "Generic mode for InstallShield RUL files.")

(define-skeleton rul-if
   "Insert an if statement."
   "condition: "
   "if(" str ") then" \n
   > _ \n
   ( "other condition, %s: "
     > "elseif(" str ") then" \n
     > \n)
   > "else" \n
   > \n
   resume:
   > "endif;"
   )

(define-skeleton rul-function
  "Insert a function statement."
  "function: "
  "function " str " ()" \n
  ( "local variables, %s: "
  > "  " str ";" \n)
  > "begin" \n
  > _ \n
  resume:
  > "end;")

)

;; Additions by ACorreir@pervasive-sw.com (Alfred Correira)
(define-generic-mode 'mailrc-generic-mode
  (list ?#)
  (list
   "alias"
   "else"
   "endif"
   "group"
   "if"
   "ignore"
   "set"
   "unset"
   )
  '(("^\\s-*\\(alias\\|group\\)\\s-+\\([-A-Za-z0-9_]+\\)\\s-+\\([^\n\r#]*\\)\\(#.*\\)?$"
     (2 font-lock-constant-face) (3 font-lock-variable-name-face))
    ("^\\s-*\\(unset\\|set\\|ignore\\)\\s-+\\([-A-Za-z0-9_]+\\)=?\\([^\n\r#]*\\)\\(#.*\\)?$"
     (2 font-lock-constant-face) (3 font-lock-variable-name-face)))
  (list "\\.mailrc\\'")
  nil
  "Mode for mailrc files.")

;; Inetd.conf
(and
 (memq 'inetd-conf-generic-mode generic-extras-enable-list)

(define-generic-mode 'inetd-conf-generic-mode
  (list ?#)
  (list
   "stream"
   "dgram"
   "tcp"
   "udp"
   "wait"
   "nowait"
   "internal"
   )
  '(
    ("^\\([-A-Za-z0-9_]+\\)"
     1 'font-lock-type-face)
    )
  '("/etc/inetd.conf\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([-A-Za-z0-9_]+\\)" 1)))
      )))
  )
)

;; Services
(and
 (memq 'etc-services-generic-mode generic-extras-enable-list)

(define-generic-mode 'etc-services-generic-mode
  (list ?#)
  (list
   "tcp"
   "udp"
   "ddp"
   )
  '(
    ("^\\([-A-Za-z0-9_]+\\)\\s-+\\([0-9]+\\)/"
     (1 'font-lock-type-face)
     (2 'font-lock-variable-name-face)
     )
    )
  '("/etc/services\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([-A-Za-z0-9_]+\\)" 1)))
      )))
  )
)

;; Password and Group files
(and
 (memq 'etc-passwd-generic-mode generic-extras-enable-list)

(define-generic-mode 'etc-passwd-generic-mode
  nil              ;; No comment characters
  (list "root")    ;; Only one keyword
  (list
    (list
     (concat
      "^"
      ;; User name -- Never blank!
      "\\([^:]+\\)"
      ":"
      ;; Password, UID and GID
      (mapconcat
       'identity
       (make-list 3 "\\([^:]+\\)")
       ":"
       )
      ":"
      ;; GECOS/Name -- might be blank
      "\\([^:]*\\)"
      ":"
      ;; Home directory and shell
      "\\([^:]+\\)"
      ":?"
      "\\([^:]*\\)"
      "$"
     )
     '(1 'font-lock-type-face)
     '(5 'font-lock-variable-name-face)
     '(6 'font-lock-constant-face)
     '(7 'font-lock-warning-face)
     )
    '("^\\([^:]+\\):\\([^:]*\\):\\([0-9]+\\):\\(.*\\)$"
      (1 'font-lock-type-face)
      (4 'font-lock-variable-name-face)
     )
    )
  '("/etc/passwd\\'" "/etc/group\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([-A-Za-z0-9_]+\\):" 1)))
      )))
  )
)

;; Fstab
(and
 (memq 'etc-fstab-generic-mode generic-extras-enable-list)

(define-generic-mode 'etc-fstab-generic-mode
  (list ?#)
  (list
   "ext2"
   "fd"
   "iso9660"
   "nfs"
   "proc"
   "swap"
   "ufs"
   )
  '(
    ("^\\([/-A-Za-z0-9_]+\\)\\s-+\\([/-A-Za-z0-9_]+\\)"
     (1 'font-lock-type-face)
     (2 'font-lock-variable-name-face)
     )
    )
  '("/etc/[v]*fstab\\'")
  (list
   (function
    (lambda ()
      (setq imenu-generic-expression
	    '((nil "^\\([/-A-Za-z0-9_]+\\)\\s-+" 1)))
      )))
  )
)

;; From Jacques Duthen <jacques.duthen@sncf.fr>
(defvar show-tabs-generic-mode-font-lock-defaults-1
   '(  ;; trailing spaces must come before...
       ("[ \t]+$" . 'show-tabs-space-face)
       ;; ...embedded tabs
       ("[^\n\t]\\(\t+\\)" (1 'show-tabs-tab-face))))

(defvar show-tabs-generic-mode-font-lock-defaults-2
   '(  ;; trailing spaces must come before...
       ("[ \t]+$" . 'show-tabs-space-face)
       ;; ...tabs
       ("\t+" . 'show-tabs-tab-face)))

(defface show-tabs-tab-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "red"))
    (((class color)     (background dark))  (:foreground "red"))
    (t (:bold t)))
  "Font Lock mode face used to highlight TABs."
  :group 'show-tabs)

(defface show-tabs-space-face
  '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark))  (:foreground "DimGray"   :bold t))
    (((class color)     (background light)) (:foreground "yellow"))
    (((class color)     (background dark))  (:foreground "yellow"))
    (t (:bold t)))
  "Font Lock mode face used to highlight spaces."
  :group 'show-tabs)

(define-generic-mode 'show-tabs-generic-mode
  () ;; no comment char
  () ;; no keywords
   show-tabs-generic-mode-font-lock-defaults-1
  () ;; no auto-mode-alist
  ;; (list 'show-tabs-generic-mode-hook-fun)
  nil
  "Generic mode to show tabs and trailing spaces")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DNS modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic-mode 'named-boot-generic-mode
  ;;List of comment characters
   (list ?\;)
  ;;List of keywords
  (list "cache" "primary" "secondary" "forwarders" "limit" "options"
	"directory" "check-names")
  ;;List of additional font-lock-expressions
  (list
   (list "\\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)" 1 'font-lock-constant-face)
   (list "^directory\\s-+\\(.*\\)" 1 'font-lock-variable-name-face)
   (list "^\\(primary\\|cache\\)\\s-+\\([.A-Za-z]+\\)\\s-+\\(.*\\)"
	 (list 2 'font-lock-variable-name-face)
	 (list 3 'font-lock-constant-face))
   )
  ;;List of additional automode-alist expressions
  (list "/etc/named.boot\\'")
  ;;List of set up functions to call
  nil
  )

(define-generic-mode 'named-database-generic-mode
  ;;List of comment characters
  (list ?\;)
  ;;List of keywords
  (list "IN" "NS" "CNAME" "SOA" "PTR" "MX" "A")
  ;;List of additional font-lock-expressions
  (list
   (list "\\([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\\)" 1 'font-lock-constant-face)
   (list "^\\([.A-Za-z0-9]+\\)" 1 'font-lock-variable-name-face)
   )
  ;;List of additional automode-alist expressions
  nil
  ;;List of set up functions to call
  nil
  )

(defvar named-database-time-string "%Y%m%d%H"
  "Timestring for named serial numbers.")

(defun named-database-print-serial ()
  "Print a serial number based on the current date."
  (interactive)
  (insert (format-time-string named-database-time-string (current-time)))
  )

(define-generic-mode 'resolve-conf-generic-mode
  ;;List of comment characters
  (list ?#)
  ;;List of keywords
  (list "nameserver" "domain" "search" "sortlist" "options")
  ;;List of additional font-lock-expressions
  nil
  ;;List of additional automode-alist expressions
  (list "/etc/resolv[e]?.conf\\'")
  ;;List of set up functions to call
  nil
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes for spice and common electrical engineering circuit netlist formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic-mode 'spice-generic-mode
  nil
  (list
   "and"
   "cccs"
   "ccvs"
   "delay"
   "nand"
   "nor"
   "npwl"
   "or"
   "par"
   "ppwl"
   "pwl"
   "vccap"
   "vccs"
   "vcr"
   "vcvs"
   )
  '(
    ("^\\s-*\\([*].*\\)"                1 'font-lock-comment-face)
    (" \\(\\$ .*\\)$"                   1 'font-lock-comment-face)
    ("^\\(\\$ .*\\)$"                   1 'font-lock-comment-face)
    ("\\([*].*\\)"                      1 'font-lock-comment-face)
    ("^\\([+]\\)"                       1 'font-lock-string-face)
    ("^\\s-*\\([.]\\w+\\>\\)"           1 'font-lock-keyword-face)
    ("\\(\\([.]\\|_\\|\\w\\)+\\)\\s-*=" 1 'font-lock-variable-name-face)
    ("\\('[^']+'\\)"                    1 'font-lock-string-face)
    ("\\(\"[^\"]+\"\\)"                 1 'font-lock-string-face)
    )
  (list "\\.[sS][pP]\\'"
	"\\.[sS][pP][iI]\\'"
	"\\.[sS][pP][iI][cC][eE]\\'"
	"\\.[iI][nN][cC]\\'")
  (list
   'generic-bracket-support
   ;; Make keywords case-insensitive
   (function
    (lambda()
      (setq font-lock-defaults (list 'generic-font-lock-defaults nil t))))
   )
  "Generic mode for SPICE circuit netlist files."
  )

(define-generic-mode 'ibis-generic-mode
  (list ?|)
  nil
  '(
    ("[[]\\([^]]*\\)[]]"          1 'font-lock-keyword-face)
    ("\\(\\(_\\|\\w\\)+\\)\\s-*=" 1 'font-lock-variable-name-face)
    )
  (list "\\.[iI][bB][sS]\\'")
  (list 'generic-bracket-support)
  "Generic mode for IBIS circuit netlist files."
  )

(define-generic-mode 'astap-generic-mode
  nil
  (list
   "analyze"
   "description"
   "elements"
   "execution"
   "features"
   "functions"
   "ground"
   "model"
   "outputs"
   "print"
   "run"
   "controls"
   "table"
   )
  '(
    ("^\\s-*\\([*].*\\)"      1 'font-lock-comment-face)
    (";\\s-*\\([*].*\\)"      1 'font-lock-comment-face)
    ("^\\s-*\\([.]\\w+\\>\\)" 1 'font-lock-keyword-face)
    ("\\('[^']+'\\)"          1 'font-lock-string-face)
    ("\\(\"[^\"]+\"\\)"       1 'font-lock-string-face)
    ("[(,]\\s-*\\(\\([.]\\|_\\|\\w\\)+\\)\\s-*=" 1 'font-lock-variable-name-face)
    )
  (list "\\.[aA][pP]\\'"
	"\\.[aA][sS][xX]\\'"
	"\\.[aA][sS][tT][aA][pP]\\'"
	"\\.[pP][sS][pP]\\'"
	"\\.[dD][eE][cC][kK]\\'"
	"\\.[gG][oO][dD][aA][tT][aA]")
  (list
   'generic-bracket-support
   ;; Make keywords case-insensitive
   (function
    (lambda()
      (setq font-lock-defaults (list 'generic-font-lock-defaults nil t))))
   )
  "Generic mode for ASTAP circuit netlist files."
  )


(provide 'generic-x)

;;; generic-x.el ends here
