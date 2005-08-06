;;; conf-mode.el --- Simple major mode for editing conf/ini/properties files

;; Copyright (C) 2004, 2005 by Daniel Pfeiffer <occitan@esperanto.org>
;; Keywords: conf ini windows java

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This mode is designed to edit many similar varieties of Conf/Ini files and
;; Java properties.  It started out from Aurélien Tisné's ini-mode.
;; `conf-space-keywords' were inspired by Robert Fitzgerald's any-ini-mode.


;;; Code:

(require 'newcomment)

;; Variables:

(defgroup conf nil
  "Configuration files."
  :group 'data
  :version "22.1")

(defcustom conf-assignment-column 24
  "Align assignments to this column by default with \\[conf-align-assignments].
If this number is negative, the `=' comes before the whitespace.  Use 0 to
not align (only setting space according to `conf-assignment-space')."
  :type 'integer
  :group 'conf)

(defcustom conf-javaprop-assignment-column 32
  "Value for `conf-assignment-column' in Java properties buffers."
  :type 'integer
  :group 'conf)

(defcustom conf-colon-assignment-column (- (abs conf-assignment-column))
  "Value for `conf-assignment-column' in Java properties buffers."
  :type 'integer
  :group 'conf)

(defcustom conf-assignment-space t
  "Put at least one space around assignments when aligning."
  :type 'boolean
  :group 'conf)

(defcustom conf-colon-assignment-space nil
  "Value for `conf-assignment-space' in colon style Conf mode buffers."
  :type 'boolean
  :group 'conf)


(defvar conf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-u" 'conf-unix-mode)
    (define-key map "\C-c\C-w" 'conf-windows-mode)
    (define-key map "\C-c\C-j" 'conf-javaprop-mode)
    (define-key map "\C-c\C-s" 'conf-space-mode)
    (define-key map "\C-c " 'conf-space-mode)
    (define-key map "\C-c\C-c" 'conf-colon-mode)
    (define-key map "\C-c:" 'conf-colon-mode)
    (define-key map "\C-c\C-x" 'conf-xdefaults-mode)
    (define-key map "\C-c\C-p" 'conf-ppd-mode)
    (define-key map "\C-c\C-q" 'conf-quote-normal)
    (define-key map "\C-c\"" 'conf-quote-normal)
    (define-key map "\C-c'" 'conf-quote-normal)
    (define-key map "\C-c\C-a" 'conf-align-assignments)
    map)
  "Local keymap for conf-mode buffers.")

(defvar conf-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ?-  "_" table)
    (modify-syntax-entry ?.  "_" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    table)
  "Syntax table in use in Windows style conf-mode buffers.")

(defvar conf-unix-mode-syntax-table
  (let ((table (make-syntax-table conf-mode-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    ;; override
    (modify-syntax-entry ?\; "." table)
    table)
  "Syntax table in use in Unix style conf-mode buffers.")

(defvar conf-javaprop-mode-syntax-table
  (let ((table (make-syntax-table conf-unix-mode-syntax-table)))
    (modify-syntax-entry ?/  ". 124" table)
    (modify-syntax-entry ?*  ". 23b" table)
    table)
  "Syntax table in use in Java prperties buffers.")

(defvar conf-ppd-mode-syntax-table
  (let ((table (make-syntax-table conf-mode-syntax-table)))
    (modify-syntax-entry ?*  ". 1" table)
    (modify-syntax-entry ?%  ". 2" table)
    ;; override
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\; "." table)
    table)
  "Syntax table in use in PPD conf-mode buffers.")

(defvar conf-xdefaults-mode-syntax-table
  (let ((table (make-syntax-table conf-mode-syntax-table)))
    (modify-syntax-entry ?!  "<" table)
    ;; override
    (modify-syntax-entry ?\; "." table)
    table)
  "Syntax table in use in Xdefaults style conf-mode buffers.")


(defvar conf-font-lock-keywords
  `(;; [section] (do this first because it may look like a parameter)
    ("^[ \t]*\\[\\(.+\\)\\]" 1 'font-lock-type-face)
    ;; var=val or var[index]=val
    ("^[ \t]*\\(.+?\\)\\(?:\\[\\(.*?\\)\\]\\)?[ \t]*="
     (1 'font-lock-variable-name-face)
     (2 'font-lock-constant-face nil t))
    ;; section { ... } (do this last because some assign ...{...)
    ("^[ \t]*\\([^=:\n]+?\\)[ \t\n]*{[^{}]*?$" 1 'font-lock-type-face prepend))
  "Keywords to hilight in Conf mode")

(defvar conf-javaprop-font-lock-keywords
  '(;; var=val
    ("^[ \t]*\\(.+?\\)\\(?:\\.\\([0-9]+\\)\\(?:\\.\\(.+?\\)\\(?:\\.\\([0-9]+\\)\\(?:\\.\\(.+?\\)\\(?:\\.\\([0-9]+\\)\\(\\..+?\\)?\\)?\\)?\\)?\\)?\\)?\\([:= \t]\\|$\\)"
     (1 'font-lock-variable-name-face)
     (2 'font-lock-constant-face nil t)
     (3 'font-lock-variable-name-face nil t)
     (4 'font-lock-constant-face nil t)
     (5 'font-lock-variable-name-face nil t)
     (6 'font-lock-constant-face nil t)
     (7 'font-lock-variable-name-face nil t)))
  "Keywords to hilight in Conf Java Properties mode")

(defvar conf-space-keywords-alist
  '(("\\`/etc/gpm/" . "key\\|name\\|foreground\\|background\\|border\\|head")
    ("\\`/etc/magic\\'" . "[^ \t]+[ \t]+\\(?:[bl]?e?\\(?:short\\|long\\)\\|byte\\|string\\)[^ \t]*")
    ("/mod\\(?:ules\\|probe\\)\\.conf" . "alias\\|in\\(?:clude\\|stall\\)\\|options\\|remove")
    ("/manpath\\.config" . "MAN\\(?:DATORY_MANPATH\\|PATH_MAP\\|DB_MAP\\)")
    ("/sensors\\.conf" . "chip\\|bus\\|label\\|compute\\|set\\|ignore")
    ("/sane\\(\\.d\\)?/" . "option\\|device\\|port\\|usb\\|sc\\(?:si\\|anner\\)")
    ("/resmgr\\.conf" . "class\\|add\\|allow\\|deny")
    ("/dictionary\\.lst\\'" . "DICT\\|HYPH\\|THES")
    ("/tuxracer/options" . "set"))
  "File name based settings for `conf-space-keywords'.")

(defvar conf-space-keywords nil
  "Regexps for functions that may come before a space assignment.
This allows constructs such as
keyword var value
This variable is best set in the file local variables, or through
`conf-space-keywords-alist'.")

(defvar conf-space-font-lock-keywords
  `(;; [section] (do this first because it may look like a parameter)
    ("^[ \t]*\\[\\(.+\\)\\]" 1 'font-lock-type-face)
    ;; section { ... } (do this first because it looks like a parameter)
    ("^[ \t]*\\(.+?\\)[ \t\n]*{[^{}]*?$" 1 'font-lock-type-face)
    ;; var val
    (eval if conf-space-keywords
	  (list (concat "^[ \t]*\\(" conf-space-keywords "\\)[ \t]+\\([^\000- ]+\\)")
		'(1 'font-lock-keyword-face)
		'(2 'font-lock-variable-name-face))
	  '("^[ \t]*\\([^\000- ]+\\)" 1 'font-lock-variable-name-face)))
  "Keywords to hilight in Conf Space mode")

(defvar conf-colon-font-lock-keywords
  `(;; [section] (do this first because it may look like a parameter)
    ("^[ \t]*\\[\\(.+\\)\\]" 1 'font-lock-type-face)
    ;; var: val
    ("^[ \t]*\\(.+?\\)[ \t]*:"
     (1 'font-lock-variable-name-face))
    ;; section { ... } (do this last because some assign ...{...)
    ("^[ \t]*\\([^:\n]+\\)[ \t\n]*{[^{}]*?$" 1 'font-lock-type-face prepend))
  "Keywords to hilight in Conf Colon mode")

(defvar conf-assignment-sign ?=
  "What sign is used for assignments.")

(defvar conf-assignment-regexp ".+?\\([ \t]*=[ \t]*\\)"
  "Regexp to recognize assignments.
It is anchored after the first sexp on a line.  There must a
grouping for the assignment sign, including leading and trailing
whitespace.")


;; If anybody can figure out how to get the same effect by configuring
;; `align', I'd be glad to hear.
(defun conf-align-assignments (&optional arg)
  (interactive "P")
  (setq arg (if arg
		(prefix-numeric-value arg)
	      conf-assignment-column))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((cs (comment-beginning)))	; go before comment if within
	(if cs (goto-char cs)))
      (while (forward-comment 9))	; max-int?
      (when (and (not (eobp))
		 (looking-at conf-assignment-regexp))
	(goto-char (match-beginning 1))
	(delete-region (point) (match-end 1))
	(if conf-assignment-sign
	    (if (>= arg 0)
		(progn
		  (indent-to-column arg)
		  (or (not conf-assignment-space) (memq (char-before (point)) '(?  ?\t)) (insert ? ))
		  (insert conf-assignment-sign (if (and conf-assignment-space (not (eolp))) ?\  "")))
	      (insert (if conf-assignment-space ?\  "") conf-assignment-sign)
	      (unless (eolp)
		(indent-to-column (- arg))
		(or (not conf-assignment-space) (memq (char-before (point)) '(?  ?\t)) (insert ? ))))
	  (unless (eolp)
	    (if (>= (current-column) (abs arg))
		(insert ? )
	      (indent-to-column (abs arg))))))
      (forward-line))))


(defun conf-quote-normal (arg)
  "Set the syntax of ' and \" to punctuation.
With prefix arg, only do it for ' if 1, or only for \" if 2.
This only affects the current buffer.  Some conf files use quotes
to delimit strings, while others allow quotes as simple parts of
the assigned value.  In those files font locking will be wrong,
and you can correct it with this command.  (Some files even do
both, i.e. quotes delimit strings, except when they are
unbalanced, but hey...)"
  (interactive "P")
  (let ((table (copy-syntax-table (syntax-table))))
    (if (or (not arg) (= (prefix-numeric-value arg) 1)) (modify-syntax-entry ?\' "." table))
    (if (or (not arg) (= (prefix-numeric-value arg) 2)) (modify-syntax-entry ?\" "." table))
    (set-syntax-table table)
    (and (boundp 'font-lock-mode)
	 font-lock-mode
	 (font-lock-fontify-buffer))))


(defun conf-outline-level ()
  (let ((depth 0)
	(pt (match-end 0)))
    (condition-case nil
	(while (setq pt (scan-lists pt -1 1)
		     depth (1+ depth)))
      (scan-error depth))))



;;;###autoload
(defun conf-mode (&optional comment syntax-table name)
  "Mode for Unix and Windows Conf files and Java properties.
Most conf files know only three kinds of constructs: parameter
assignments optionally grouped into sections and comments.  Yet
there is a great range of variation in the exact syntax of conf
files.  See below for various wrapper commands that set up the
details for some of the most widespread variants.

This mode sets up font locking, outline, imenu and it provides
alignment support through `conf-align-assignments'.  If strings
come out wrong, try `conf-quote-normal'.

Some files allow continuation lines, either with a backslash at
the end of line, or by indenting the next line (further).  These
constructs cannot currently be recognized.

Because of this great variety of nuances, which are often not
even clearly specified, please don't expect it to get every file
quite right.  Patches that clearly identify some special case,
without breaking the general ones, are welcome.

If instead you start this mode with the generic `conf-mode'
command, it will parse the buffer.  It will generally well
identify the first four cases listed below.  If the buffer
doesn't have enough contents to decide, this is identical to
`conf-windows-mode' on Windows, elsewhere to `conf-unix-mode'.
See also `conf-space-mode', `conf-colon-mode', `conf-javaprop-mode',
`conf-ppd-mode' and `conf-xdefaults-mode'.

\\{conf-mode-map}"

  (interactive)
  (if (not comment)
      (let ((unix 0) (win 0) (equal 0) (colon 0) (space 0) (jp 0))
	(save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
	    (skip-chars-forward " \t\f")
	    (cond ((eq (char-after) ?\#) (setq unix (1+ unix)))
		  ((eq (char-after) ?\;) (setq win (1+ win)))
		  ((eq (char-after) ?\[))	; nop
		  ((eolp))			; nop
		  ((eq (char-after) ?}))	; nop
		  ;; recognize at most double spaces within names
		  ((looking-at "[^ \t\n=:]+\\(?:  ?[^ \t\n=:]+\\)*[ \t]*[=:]")
		   (if (eq (char-before (match-end 0)) ?=)
		       (setq equal (1+ equal))
		     (setq colon (1+ colon))))
		  ((looking-at "/[/*]") (setq jp (1+ jp)))
		  ((looking-at ".*{"))		; nop
		  ((setq space (1+ space))))
	    (forward-line)))
	(if (> jp (max unix win 3))
	    (conf-javaprop-mode)
	  (if (> colon (max equal space))
	      (conf-colon-mode)
	    (if (> space (max equal colon))
		(conf-space-mode)
	      (if (or (> win unix)
		      (and (= win unix) (eq system-type 'windows-nt)))
		  (conf-windows-mode)
		(conf-unix-mode))))))
    (kill-all-local-variables)
    (use-local-map conf-mode-map)

    (setq major-mode 'conf-mode
	  mode-name name)
    (set (make-local-variable 'comment-start) comment)
    (set (make-local-variable 'comment-start-skip)
	 (concat (regexp-quote comment-start) "+\\s *"))
    (set (make-local-variable 'comment-use-syntax) t)
    (set (make-local-variable 'parse-sexp-ignore-comments) t)
    (set (make-local-variable 'outline-regexp)
	 "[ \t]*\\(?:\\[\\|.+[ \t\n]*{\\)")
    (set (make-local-variable 'outline-heading-end-regexp)
	 "[\n}]")
    (set (make-local-variable 'outline-level)
	 'conf-outline-level)
    (set-syntax-table syntax-table)
    (setq imenu-generic-expression
	  '(("Parameters" "^[ \t]*\\(.+?\\)[ \t]*=" 1)
	    ;; [section]
	    (nil "^[ \t]*\\[[ \t]*\\(.+\\)[ \t]*\\]" 1)
	    ;; section { ... }
	    (nil "^[ \t]*\\([^=:{} \t\n][^=:{}\n]+\\)[ \t\n]*{" 1)))

    (run-mode-hooks 'conf-mode-hook)))

;;;###autoload
(defun conf-unix-mode ()
  "Conf Mode starter for Unix style Conf files.
Comments start with `#'.
For details see `conf-mode'.  Example:

# Conf mode font-locks this right on Unix and with C-c C-u

\[Desktop Entry]
	 Encoding=UTF-8
	 Name=The GIMP
	 Name[ca]=El GIMP
	 Name[cs]=GIMP"
  (interactive)
  (conf-mode "#" conf-unix-mode-syntax-table "Conf[Unix]"))

;;;###autoload
(defun conf-windows-mode ()
  "Conf Mode starter for Windows style Conf files.
Comments start with `;'.
For details see `conf-mode'.  Example:

; Conf mode font-locks this right on Windows and with C-c C-w

\[ExtShellFolderViews]
Default={5984FFE0-28D4-11CF-AE66-08002B2E1262}
{5984FFE0-28D4-11CF-AE66-08002B2E1262}={5984FFE0-28D4-11CF-AE66-08002B2E1262}

\[{5984FFE0-28D4-11CF-AE66-08002B2E1262}]
PersistMoniker=file://Folder.htt"
  (interactive)
  (conf-mode ";" conf-mode-syntax-table "Conf[WinIni]"))

;; Here are a few more or less widespread styles.  There are others, so
;; obscure, they are not covered.  E.g. RFC 2614 allows both Unix and Windows
;; comments.  Or the donkey has (* Pascal comments *) -- roll your own starter
;; if you need it.

;;;###autoload
(defun conf-javaprop-mode ()
  "Conf Mode starter for Java properties files.
Comments start with `#' but are also recognized with `//' or
between `/*' and `*/'.
For details see `conf-mode'.  Example:

# Conf mode font-locks this right with C-c C-j (Java properties)
// another kind of comment
/* yet another */

name:value
name=value
name value
x.1 =
x.2.y.1.z.1 =
x.2.y.1.z.2.zz ="
  (interactive)
  (conf-mode "#" conf-javaprop-mode-syntax-table "Conf[JavaProp]")
  (set (make-local-variable 'conf-assignment-column)
       conf-javaprop-assignment-column)
  (set (make-local-variable 'conf-assignment-regexp)
       ".+?\\([ \t]*[=: \t][ \t]*\\|$\\)")
  (set (make-local-variable 'conf-font-lock-keywords)
       conf-javaprop-font-lock-keywords)
  (setq comment-start-skip "\\(?:#+\\|/[/*]+\\)\\s *")
  (setq imenu-generic-expression
	'(("Parameters" "^[ \t]*\\(.+?\\)[=: \t]" 1))))

;;;###autoload
(defun conf-space-mode (&optional keywords)
  "Conf Mode starter for space separated conf files.
\"Assignments\" are with ` '.  Keywords before the parameters are
recognized according to `conf-space-keywords'.  Interactively
with a prefix ARG of `0' no keywords will be recognized.  With
any other prefix arg you will be prompted for a regexp to match
the keywords.  Programmatically you can pass such a regexp as
KEYWORDS, or any non-nil non-string for no keywords.

For details see `conf-mode'.  Example:

# Conf mode font-locks this right with C-c C-s (space separated)

image/jpeg			jpeg jpg jpe
image/png			png
image/tiff			tiff tif

# Or with keywords (from a recognized file name):
class desktop
# Standard multimedia devices
add /dev/audio		desktop
add /dev/mixer		desktop"
  (interactive
   (list (if current-prefix-arg
	     (if (> (prefix-numeric-value current-prefix-arg) 0)
		 (read-string "Regexp to match keywords: ")
	       t))))
  (conf-unix-mode)
  (setq mode-name "Conf[Space]")
  (set (make-local-variable 'conf-assignment-sign)
       nil)
  (set (make-local-variable 'conf-font-lock-keywords)
       conf-space-font-lock-keywords)
  ;; This doesn't seem right, but the next two depend on conf-space-keywords
  ;; being set, while after-change-major-mode-hook might set up imenu, needing
  ;; the following result:
  (hack-local-variables-prop-line)
  (hack-local-variables)
  (if keywords
      (set (make-local-variable 'conf-space-keywords)
	   (if (stringp keywords) keywords))
    (or conf-space-keywords
	(not buffer-file-name)
	(set (make-local-variable 'conf-space-keywords)
	     (assoc-default buffer-file-name conf-space-keywords-alist
			    'string-match))))
  (set (make-local-variable 'conf-assignment-regexp)
       (if conf-space-keywords
	   (concat "\\(?:" conf-space-keywords "\\)[ \t]+.+?\\([ \t]+\\|$\\)")
	 ".+?\\([ \t]+\\|$\\)"))
  (setq imenu-generic-expression
	`(,@(cdr imenu-generic-expression)
	  ("Parameters"
	   ,(if conf-space-keywords
		(concat "^[ \t]*\\(?:" conf-space-keywords
			"\\)[ \t]+\\([^ \t\n]+\\)\\(?:[ \t]\\|$\\)")
	      "^[ \t]*\\([^ \t\n[]+\\)\\(?:[ \t]\\|$\\)")
	   1))))

;;;###autoload
(defun conf-colon-mode (&optional comment syntax-table name)
  "Conf Mode starter for Colon files.
\"Assignments\" are with `:'.
For details see `conf-mode'.  Example:

# Conf mode font-locks this right with C-c C-c (colon)

<Multi_key> <exclam> <exclam>		: \"\\241\"	exclamdown
<Multi_key> <c> <slash>			: \"\\242\"	cent"
  (interactive)
  (if comment
      (conf-mode comment syntax-table name)
    (conf-unix-mode)
    (setq mode-name "Conf[Colon]"))
  (set (make-local-variable 'conf-assignment-space)
       conf-colon-assignment-space)
  (set (make-local-variable 'conf-assignment-column)
       conf-colon-assignment-column)
  (set (make-local-variable 'conf-assignment-sign)
       ?:)
  (set (make-local-variable 'conf-assignment-regexp)
       ".+?\\([ \t]*:[ \t]*\\)")
  (set (make-local-variable 'conf-font-lock-keywords)
       conf-colon-font-lock-keywords)
  (setq imenu-generic-expression
	`(("Parameters" "^[ \t]*\\(.+?\\)[ \t]*:" 1)
	  ,@(cdr imenu-generic-expression))))

;;;###autoload
(defun conf-ppd-mode ()
  "Conf Mode starter for Adobe/CUPS PPD files.
Comments start with `*%' and \"assignments\" are with `:'.
For details see `conf-mode'.  Example:

*% Conf mode font-locks this right with C-c C-p (PPD)

*DefaultTransfer: Null
*Transfer Null.Inverse: \"{ 1 exch sub }\""
  (interactive)
  (conf-colon-mode "*%" conf-ppd-mode-syntax-table "Conf[PPD]")
  ;; no sections, they match within PostScript code
  (setq imenu-generic-expression (list (car imenu-generic-expression))))

;;;###autoload
(defun conf-xdefaults-mode ()
  "Conf Mode starter for Xdefaults files.
Comments start with `!' and \"assignments\" are with `:'.
For details see `conf-mode'.  Example:

! Conf mode font-locks this right with C-c C-x (.Xdefaults)

*background:			gray99
*foreground:			black"
  (interactive)
  (conf-colon-mode "!" conf-xdefaults-mode-syntax-table "Conf[Xdefaults]"))


;; font lock support
(if (boundp 'font-lock-defaults-alist)
    (add-to-list
     'font-lock-defaults-alist
     (cons 'conf-mode
	   (list 'conf-font-lock-keywords nil t nil nil))))


(provide 'conf-mode)

;; arch-tag: 0a3805b2-0371-4d3a-8498-8897116b2356
;;; conf-mode.el ends here
