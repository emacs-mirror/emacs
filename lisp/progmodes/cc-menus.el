;;; cc-menus.el --- imenu support for CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:    1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: cc-mode-help@python.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    5.12
;; Keywords:   c languages oop

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


;; imenu integration
(defvar cc-imenu-c++-generic-expression
  (` 
   ((nil
     (, 
      (concat
       "^"				      ; beginning of line is required
       "\\(template[ \t]*<[^>]+>[ \t]*\\)?"   ; there may be a "template <...>"
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	      ; type specs; there can be no
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	      ; more than 3 tokens, right?
        
       "\\("				      ; last type spec including */&
       "[a-zA-Z0-9_:]+"
       "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)"     ; either ptr/ref sign or ws
       "\\)?"				      ; if there is a last type spec
       "\\("				      ; name, take into the imenu entry
       "[a-zA-Z0-9_:~]+"		      ; member func, ctor or dtor...
 					      ; (may not contain * because then
 					      ; "a::operator char*" would
					      ; become "char*"!)
       "\\|"
       "\\([a-zA-Z0-9_:~]*::\\)?operator"
       "[^a-zA-Z1-9_][^(]*"		      ; ...or operator
       " \\)"
       "[ \t]*([^)]*)[ \t\n]*[^		;]"   ; require something other than
					      ; a `;' after the (...) to
					      ; avoid prototypes.  Can't
					      ; catch cases with () inside
					      ; the parentheses surrounding
					      ; the parameters.  e.g.:
					      ; "int foo(int a=bar()) {...}"
        
       )) 6)    
    ("Class" 
     (, (concat 
 	 "^"				      ; beginning of line is required
 	 "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
 	 "class[ \t]+"
 	 "\\([a-zA-Z0-9_]+\\)"		      ; the string we want to get
 	 "[ \t]*[:{]"
 	 )) 2)))
  "Imenu generic expression for C++ mode.  See `imenu-generic-expression'.")
 
(defvar cc-imenu-c-generic-expression
  cc-imenu-c++-generic-expression
  "Imenu generic expression for C mode.  See `imenu-generic-expression'.")

;(defvar cc-imenu-objc-generic-expression
;  ())
; Please contribute one!

(defvar cc-imenu-java-generic-expression
  (`
   ((nil
     (,
      (concat
       "^\\([ \t]\\)*"
       "\\([A-Za-z0-9_-]+[ \t]+\\)?"	      ; type specs; there can be
        "\\([A-Za-z0-9_-]+[ \t]+\\)?"	      ; more than 3 tokens, right?
       "\\([A-Za-z0-9_-]+[ \t]*[[]?[]]?\\)"
       "\\([ \t]\\)"
       "\\([A-Za-z0-9_-]+\\)"		      ; the string we want to get
       "\\([ \t]*\\)+("
       "\\([a-zA-Z,_1-9\n \t]*[[]?[]]?\\)*"   ; arguments
       ")[ \t]*"
       "[^;(]"
       "[,a-zA-Z_1-9\n \t]*{"               
       )) 6)))
  "Imenu generic expression for Java mode.  See `imenu-generic-expression'.")


(provide 'cc-menus)
;;; cc-menus.el ends here
