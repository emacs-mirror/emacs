;;; em-pred.el --- argument predicates and modifiers (ala zsh)  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; Argument predication is used to affect which members of a list are
;; selected for use as argument.  This is most useful with globbing,
;; but can be used on any list argument, to select certain members.
;;
;; Argument modifiers are used to manipulate argument values.  For
;; example, sorting lists, upcasing words, substituting characters,
;; etc.
;;
;; Here are some examples of how to use argument predication.  Most of
;; the predicates and modifiers are modeled after those provided by
;; zsh.
;;
;;   ls -ld *(/)           ; list all directories
;;   ls -l *(@u'johnw')    ; list all symlinks owned by 'johnw'
;;   bzip2 -9v **/*(a+30)  ; compress everything which hasn't been
;;                           accessed in 30 days
;;   echo *.c(:o:R)     ; a reversed, sorted list of C files
;;   *(^@:U^u0)         ; all non-symlinks not owned by 'root', upcased
;;   chmod u-x *(U*)    : remove exec bit on all executables owned by user
;;
;; See the zsh docs for more on the syntax ([(zsh.info)Filename
;; Generation]).

;;; Code:

(require 'esh-mode)

;;;###esh-module-autoload
(progn
(defgroup eshell-pred nil
  "This module allows for predicates to be applied to globbing
patterns (similar to zsh), in addition to string modifiers which can
be applied either to globbing results, variable references, or just
ordinary strings."
  :tag "Value modifiers and predicates"
  :group 'eshell-module))

;;; User Variables:

(defcustom eshell-pred-load-hook nil
  "A list of functions to run when `eshell-pred' is loaded."
  :version "24.1"			; removed eshell-pred-initialize
  :type 'hook)

(defcustom eshell-predicate-alist
  '((?/ . (eshell-pred-file-type ?d))   ; directories
    (?. . (eshell-pred-file-type ?-))   ; regular files
    (?= . (eshell-pred-file-type ?s))   ; sockets
    (?p . (eshell-pred-file-type ?p))   ; named pipes
    (?@ . (eshell-pred-file-type ?l))   ; symbolic links
    (?% . (eshell-pred-file-type ?%))   ; allow user to specify (c def.)
    (?r . (eshell-pred-file-mode #o0400)) ; owner-readable
    (?w . (eshell-pred-file-mode #o0200)) ; owner-writable
    (?x . (eshell-pred-file-mode #o0100)) ; owner-executable
    (?A . (eshell-pred-file-mode #o0040)) ; group-readable
    (?I . (eshell-pred-file-mode #o0020)) ; group-writable
    (?E . (eshell-pred-file-mode #o0010)) ; group-executable
    (?R . (eshell-pred-file-mode #o0004)) ; world-readable
    (?W . (eshell-pred-file-mode #o0002)) ; world-writable
    (?X . (eshell-pred-file-mode #o0001)) ; world-executable
    (?s . (eshell-pred-file-mode #o4000)) ; setuid
    (?S . (eshell-pred-file-mode #o2000)) ; setgid
    (?t . (eshell-pred-file-mode #o1000)) ; sticky bit
    (?U . (lambda (file)                   ; owned by effective uid
            (if (file-exists-p file)
                (= (file-attribute-user-id (file-attributes file))
                   (file-user-uid)))))
    (?G . (lambda (file)               ; owned by effective gid
            (if (file-exists-p file)
                (= (file-attribute-group-id (file-attributes file))
                   (file-group-gid)))))
    (?* . (lambda (file)
            (and (file-regular-p file)
                 (not (file-symlink-p file))
                 (file-executable-p file))))
    (?l . (eshell-pred-file-links))
    (?u . (eshell-pred-user-or-group ?u "user" 2 #'eshell-user-id))
    (?g . (eshell-pred-user-or-group ?g "group" 3 #'eshell-group-id))
    (?a . (eshell-pred-file-time ?a "access" 4))
    (?m . (eshell-pred-file-time ?m "modification" 5))
    (?c . (eshell-pred-file-time ?c "change" 6))
    (?L . (eshell-pred-file-size)))
  "A list of predicates than can be applied to a globbing pattern.
The format of each entry is

  (CHAR . PREDICATE-FUNC-SEXP)"
  :type '(repeat (cons character sexp))
  :risky t)

(defcustom eshell-modifier-alist
  '((?E . (lambda (lst) (mapcar #'eshell-eval-argument lst)))
    (?L . (lambda (lst) (mapcar #'downcase lst)))
    (?U . (lambda (lst) (mapcar #'upcase lst)))
    (?C . (lambda (lst) (mapcar #'capitalize lst)))
    (?h . (lambda (lst) (mapcar #'file-name-directory lst)))
    (?i . (eshell-include-members ?i))
    (?x . (eshell-include-members ?x t))
    (?r . (lambda (lst) (mapcar #'file-name-sans-extension lst)))
    (?e . (lambda (lst) (mapcar #'file-name-extension lst)))
    (?t . (lambda (lst) (mapcar #'file-name-nondirectory lst)))
    (?q . #'identity)                   ; Obsolete as of Emacs 31.1.
    (?u . #'seq-uniq)
    (?o . (lambda (lst) (sort lst #'string-lessp)))
    (?O . (lambda (lst) (sort lst #'string-greaterp)))
    (?j . (eshell-join-members))
    (?S . (eshell-split-members))
    (?R . #'reverse)
    (?g . (progn
	    (forward-char)
	    (if (eq (char-before) ?s)
		(eshell-pred-substitute t)
	      (error "`g' modifier cannot be used alone"))))
    (?s . (eshell-pred-substitute)))
  "A list of modifiers than can be applied to an argument expansion.
The format of each entry is

  (CHAR . MODIFIER-FUNC-SEXP)"
  :type '(repeat (cons character sexp))
  :risky t)

(defvar eshell-predicate-help-string
  "Eshell predicate quick reference:

  -  follow symbolic references for predicates after the `-'
  ^  invert sense of predicates after the `^'

FILE TYPE:
  /  directories              s  sockets
  .  regular files            p  named pipes
  *  executable (files only)  @  symbolic links

  %x  file type == `x' (as by ls -l; so `c' = char device, etc.)

PERMISSION BITS (for owner/group/world):
  r/A/R  readable    s  setuid
  w/I/W  writable    S  setgid
  x/E/X  executable  t  sticky bit

OWNERSHIP:
  U               owned by effective uid
  G               owned by effective gid
  u(UID|\\='user\\=')   owned by UID/user
  g(GID|\\='group\\=')  owned by GID/group

FILE ATTRIBUTES:
  l[+-]N                 +/-/= N links
  a[Mwhms][+-](N|\\='FILE\\=') access time +/-/= N months/weeks/hours/mins/secs
			 (days if unspecified) if FILE specified,
			 use as comparison basis; so a+\\='file.c\\='
			 shows files accessed before file.c was
			 last accessed
  m[Mwhms][+-](N|\\='FILE\\=') modification time...
  c[Mwhms][+-](N|\\='FILE\\=') change time...
  L[kmp][+-]N            file size +/-/= N Kb/Mb/blocks

EXAMPLES:
  *(^@)         all non-dot files which are not symlinks
  .#*(^@)       all files which are not symbolic links
  **/.#*(*)     all executable files, searched recursively
  ***/*~f*(-/)  recursively (though not traversing symlinks),
		find all directories (or symlinks referring to
		directories) whose names do not begin with f.
  e*(*Lk+50)    executables 50k or larger beginning with `e'")

(defvar eshell-modifier-help-string
  "Eshell modifier quick reference:

FOR SINGLE ARGUMENTS, or each argument of a list of strings:
  E  evaluate again
  L  lowercase
  U  uppercase
  C  capitalize
  h  dirname
  t  basename
  e  file extension
  r  strip file extension

  S       split string at any whitespace character
  S/PAT/  split string at each occurrence of PAT

FOR LISTS OF ARGUMENTS:
  o  sort alphabetically
  O  reverse sort alphabetically
  u  uniq list (typically used after :o or :O)
  R  reverse list

  j       join list members, separated by a space
  j/PAT/  join list members, separated by PAT
  i/PAT/  exclude all members not matching PAT
  x/PAT/  exclude all members matching PAT

  s/pat/match/   substitute PAT with MATCH
  gs/pat/match/  substitute PAT with MATCH for all occurrences

EXAMPLES:
  *.c(:o)  sorted list of .c files")

(defvar eshell-pred-delimiter-pairs
  '((?\( . ?\))
    (?\[ . ?\])
    (?\< . ?\>)
    (?\{ . ?\})
    (?\' . ?\')
    (?\" . ?\")
    (?/  . ?/)
    (?|  . ?|))
  "A list of delimiter pairs that can be used in argument predicates/modifiers.
Each element is of the form (OPEN . CLOSE), where OPEN and CLOSE
are characters representing the opening and closing delimiter,
respectively.")

(defvar eshell-error-if-no-glob)        ; Defined in em-glob.el.

(defvar-keymap eshell-pred-mode-map
  "C-c M-q" #'eshell-display-predicate-help
  "C-c M-m" #'eshell-display-modifier-help)

;;; Functions:

(defun eshell-display-predicate-help ()
  (interactive)
  (with-electric-help
   (lambda ()
     (insert eshell-predicate-help-string))))

(defun eshell-display-modifier-help ()
  (interactive)
  (with-electric-help
   (lambda ()
     (insert eshell-modifier-help-string))))

(define-minor-mode eshell-pred-mode
  "Minor mode for the eshell-pred module.

\\{eshell-pred-mode-map}"
  :keymap eshell-pred-mode-map)

(defun eshell-pred-initialize ()    ;Called from `eshell-mode' via intern-soft!
  "Initialize the predicate/modifier code."
  ;; Make sure this function runs before `eshell-parse-glob-chars'.
  (add-hook 'eshell-parse-argument-hook #'eshell-parse-arg-modifier 50 t)
  (eshell-pred-mode))

(defun eshell-apply-modifiers (lst predicates modifiers string-desc)
  "Apply to list LST a series of PREDICATES and MODIFIERS.
STRING-DESC is the original string defining these predicates and
modifiers."
  (let (stringified)
    (if (stringp lst)
	(setq lst (list lst)
	      stringified t))
    (when (listp lst)
      (when lst
        (setq lst (or (eshell-winnow-list lst nil predicates)
                      (when eshell-error-if-no-glob
                        (error "No matches found: (%s)" string-desc)))))
      (while modifiers
	(setq lst (funcall (car modifiers) lst)
	      modifiers (cdr modifiers)))
      (if (and stringified
	       (= (length lst) 1))
	  (car lst)
	lst))))

(defun eshell-parse-arg-modifier ()
  "Parse a modifier that has been specified after an argument.
This function is specially for adding onto `eshell-parse-argument-hook'."
  (when (eq (char-after) ?\()
    (forward-char)
    (let ((end (eshell-find-delimiter ?\( ?\))))
      (if (not end)
          (throw 'eshell-incomplete "(")
	(when (eshell-arg-delimiter (1+ end))
	  (save-restriction
	    (narrow-to-region (point) end)
	    (let* ((modifier-string (buffer-string))
                   (modifiers (eshell-parse-modifiers))
		   (preds (car modifiers))
		   (mods (cdr modifiers)))
              (when (or preds mods)
                ;; Has to go near the end (but before
                ;; `eshell-splice-args'), which is only natural since
                ;; syntactically it can only occur at the end.
                (add-hook 'eshell-current-modifiers
                          (lambda (lst)
                            (eshell-apply-modifiers
                             lst preds mods modifier-string))
                          90))))
	  (goto-char (1+ end))
	  (eshell-finish-arg))))))

(defun eshell-parse-modifiers ()
  "Parse value modifiers and predicates at point.
Return a cons cell of the form

  (PRED-FUNC-LIST . MOD-FUNC-LIST)

PRED-FUNC-LIST is a list of predicate functions.  MOD-FUNC-LIST
is a list of result modifier functions.  PRED-FUNCS take a
filename and return t if the test succeeds; MOD-FUNCS take any
list of strings and perform a modification, returning the
resultant list of strings."
  (let (negate follow preds mods)
    (condition-case nil
	(while (not (eobp))
	  (let ((char (char-after)))
	    (cond
	     ((eq char ?')
	      (forward-char)
	      (if (looking-at "[^|':]")
		  (let ((func (read (current-buffer))))
		    (if (and func (functionp func))
			(setq preds (eshell-add-pred-func (eval func t) preds
							  negate follow))
		      (error "Invalid function predicate `%s'"
			     (eshell-stringify func))))
		(error "Invalid function predicate")))
	     ((eq char ?^)
	      (forward-char)
	      (setq negate (not negate)))
	     ((eq char ?-)
	      (forward-char)
	      (setq follow (not follow)))
	     ((eq char ?|)
	      (forward-char)
	      (if (looking-at "[^|':]")
		  (let ((func (read (current-buffer))))
		    (if (and func (functionp func))
			(setq mods
			      (cons (lambda (lst) (mapcar func lst))
				    mods))
		      (error "Invalid function modifier `%s'"
			     (eshell-stringify func))))
		(error "Invalid function modifier")))
	     ((eq char ?:)
	      (forward-char)
	      (let ((mod (assq (char-after) eshell-modifier-alist)))
		(if (not mod)
		    (error "Unknown modifier character `%c'" (char-after))
		  (forward-char)
		  (setq mods (cons (eval (cdr mod) t) mods)))))
	     (t
	      (let ((pred (assq char eshell-predicate-alist)))
		(if (not pred)
		    (error "Unknown predicate character `%c'" char)
		  (forward-char)
		  (setq preds
			(eshell-add-pred-func (eval (cdr pred) t) preds
					      negate follow))))))))
      (end-of-buffer
       (error "Predicate or modifier ended prematurely")))
    (cons (nreverse preds) (nreverse mods))))

(defun eshell-add-pred-func (pred funcs negate follow)
  "Add the predicate function PRED to FUNCS."
  (when negate
    (setq pred (let ((pred pred))
                 (lambda (file) (not (funcall pred file))))))
  (when follow
    (setq pred (let ((pred pred))
                 (lambda (file) (funcall pred (file-truename file))))))
  (cons pred funcs))

(defun eshell-get-comparison-modifier-argument (&optional functions)
  "Starting at point, get the comparison modifier argument, if any.
These are the -/+ characters, corresponding to `<' and `>',
respectively.  If no comparison modifier is at point, return `='.

FUNCTIONS, if non-nil, is a list of comparison functions,
specified as (LESS-THAN GREATER-THAN EQUAL-TO)."
  (let ((functions (or functions (list #'< #'> #'=))))
    (if (memq (char-after) '(?- ?+))
        (prog1
            (if (eq (char-after) ?-) (nth 0 functions) (nth 1 functions))
          (forward-char))
      (nth 2 functions))))

(defun eshell-get-numeric-modifier-argument ()
  "Starting at point, get the numeric modifier argument, if any.
If a number is found, update point to just after the number."
  (when (looking-at "[0-9]+")
    (prog1
	(string-to-number (match-string 0))
      (goto-char (match-end 0)))))

(defun eshell-get-delimited-modifier-argument (&optional chained-p)
  "Starting at point, get the delimited modifier argument, if any.
If the character after point is a predicate/modifier
delimiter (see `eshell-pred-delimiter-pairs', read the value of
the argument and update point to be just after the closing
delimiter.

If CHAINED-P is true, then another delimited modifier argument
will immediately follow this one.  In this case, when the opening
and closing delimiters are the same, update point to be just
before the closing delimiter.  This allows modifiers like
`:s/match/repl' to work as expected."
  (when-let* ((open (char-after))
              (close (cdr (assoc open eshell-pred-delimiter-pairs)))
              (end (eshell-find-delimiter open close nil nil t)))
    (prog1
        (replace-regexp-in-string
         (rx-to-string `(seq "\\" (group (or "\\" ,open ,close)))) "\\1"
         (buffer-substring-no-properties (1+ (point)) end))
      (goto-char (if (and chained-p (eq open close))
                     end
                   (1+ end))))))

(defun eshell-pred-user-or-group (mod-char mod-type attr-index get-id-func)
  "Return a predicate to test whether a file match a given user/group id."
  (let ((ugid (eshell-get-numeric-modifier-argument)))
    (unless ugid
      (let ((ugname (or (eshell-get-delimited-modifier-argument)
                        (error "Malformed %s name string for modifier `%c'"
                               mod-type mod-char))))
        (setq ugid (funcall get-id-func ugname))))
    (unless ugid
      (error "Unknown %s name specified for modifier `%c'"
	     mod-type mod-char))
    (lambda (file)
      (when-let* ((attrs (file-attributes file)))
	(= (nth attr-index attrs) ugid)))))

(defun eshell-pred-file-time (mod-char mod-type attr-index)
  "Return a predicate to test whether a file matches a certain time."
  (let* ((quantum 86400)
	 qual when)
    (when (memq (char-after) '(?M ?w ?h ?m ?s))
      (setq quantum (char-after))
      (cond
       ((eq quantum ?M)
	(setq quantum (* 60 60 24 30)))
       ((eq quantum ?w)
	(setq quantum (* 60 60 24 7)))
       ((eq quantum ?h)
	(setq quantum (* 60 60)))
       ((eq quantum ?m)
	(setq quantum 60))
       ((eq quantum ?s)
	(setq quantum 1)))
      (forward-char))
    (setq qual (eshell-get-comparison-modifier-argument
                (list #'time-less-p
                      (lambda (a b) (time-less-p b a))
                      #'time-equal-p)))
    (if-let* ((number (eshell-get-numeric-modifier-argument)))
        (setq when (time-since (* number quantum)))
      (let* ((file (or (eshell-get-delimited-modifier-argument)
                       (error "Malformed %s time modifier `%c'"
                              mod-type mod-char)))
             (attrs (or (file-attributes file)
                        (error "Cannot stat file `%s'" file))))
        (setq when (nth attr-index attrs))))
    (lambda (file)
      (when-let* ((attrs (file-attributes file)))
        (funcall qual when (nth attr-index attrs))))))

(defun eshell-pred-file-type (type)
  "Return a test which tests that the file is of a certain TYPE.
TYPE must be a character, and should be one of the possible options
that `ls -l' will show in the first column of its display."
  (when (eq type ?%)
    (setq type (char-after))
    (if (memq type '(?b ?c))
	(forward-char)
      (setq type ?%)))
  (let ((set (if (eq type ?%)
		 '(?b ?c)
	       (list type))))
    (lambda (file)
      (when-let* ((attrs (eshell-file-attributes (directory-file-name file))))
	(memq (aref (file-attribute-modes attrs) 0) set)))))

(defsubst eshell-pred-file-mode (mode)
  "Return a test which tests that MODE pertains to the file."
  (lambda (file)
    (when-let* ((modes (file-modes file 'nofollow)))
      (not (zerop (logand mode modes))))))

(defun eshell-pred-file-links ()
  "Return a predicate to test whether a file has a given number of links."
  (let ((qual (eshell-get-comparison-modifier-argument))
        (amount (or (eshell-get-numeric-modifier-argument)
                    (error "Invalid file link count modifier `l'"))))
    (lambda (file)
      (when-let* ((attrs (eshell-file-attributes file)))
	  (funcall qual (file-attribute-link-number attrs) amount)))))

(defun eshell-pred-file-size ()
  "Return a predicate to test whether a file is of a given size."
  (let ((quantum 1) qual amount)
    (when (memq (downcase (char-after)) '(?k ?m ?p))
      (setq qual (downcase (char-after)))
      (cond
       ((eq qual ?k)
	(setq quantum 1024))
       ((eq qual ?m)
	(setq quantum (* 1024 1024)))
       ((eq qual ?p)
	(setq quantum 512)))
      (forward-char))
    (setq qual (eshell-get-comparison-modifier-argument))
    (setq amount (* (or (eshell-get-numeric-modifier-argument)
                        (error "Invalid file size modifier `L'"))
                    quantum))
    (lambda (file)
      (when-let* ((attrs (eshell-file-attributes file)))
	(funcall qual (file-attribute-size attrs) amount)))))

(defun eshell-pred-substitute (&optional repeat)
  "Return a modifier function that will substitute matches."
  (let* ((match (or (eshell-get-delimited-modifier-argument t)
                    (error "Malformed pattern string for modifier `s'")))
         (replace (or (eshell-get-delimited-modifier-argument)
                      (error "Malformed replace string for modifier `s'")))
         (function (if repeat
                       (lambda (str)
                         (replace-regexp-in-string match replace str t))
                     (lambda (str)
                       (if (string-match match str)
                           (replace-match replace t nil str)
                         (error (concat str ": substitution failed")))))))
    (lambda (lst) (mapcar function lst))))

(defun eshell-include-members (mod-char &optional invert-p)
  "Include only Lisp members matching a regexp.
If INVERT-P is non-nil, include only members not matching a regexp."
  (let* ((regexp (or (eshell-get-delimited-modifier-argument)
                     (error "Malformed pattern string for modifier `%c'"
                            mod-char)))
         (predicates
	  (list (if invert-p
		    (lambda (elem) (not (string-match regexp elem)))
		  (lambda (elem) (string-match regexp elem))))))
    (lambda (lst)
      (eshell-winnow-list lst nil predicates))))

(defun eshell-join-members ()
  "Return a modifier function that join matches."
  (let ((str (or (eshell-get-delimited-modifier-argument)
                 " ")))
    (lambda (lst)
      (mapconcat #'identity lst str))))

(defun eshell-split-members ()
  "Return a modifier function that splits members."
  (let ((sep (eshell-get-delimited-modifier-argument)))
    (lambda (lst)
      (mapcar
       (lambda (str)
         (split-string str sep))
       lst))))

(provide 'em-pred)
;;; em-pred.el ends here
