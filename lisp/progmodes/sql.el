;;; sql.el --- specialized comint.el for SQL interpreters

;; Copyright (C) 1998  Free Software Foundation, Inc.

;; Author: Alex Schroeder <a.schroeder@bsiag.ch>
;; Maintainer: Alex Schroeder <a.schroeder@bsiag.ch>
;; Version: 1.1.5
;; Keywords: processes SQL

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

;; Please send me bug reports and bug fixes so that I can merge them
;; into the master source.

;; You can get the latest version of this file from my homepage
;; <URL:http://www.geocities.com/TimesSquare/6120/emacs.html>.

;; This file provides a sql-mode and a sql-interactive-mode.  My goals
;; were two simple modes providing syntactic hilighting.  The
;; interactive mode had to provide a command-line history; the other
;; mode had to provide "send region/buffer to SQL interpreter"
;; functions.  "simple" in this context means easy to use, easy to
;; maintain and little or no bells and whistles.  

;; If anybody feels like extending this sql mode, take a look at the
;; above mentioned modes and write a sqlx-mode on top of this one.  If
;; this proves to be difficult, please suggest changes that will
;; facilitate your plans.

;; sql-interactive-mode is used to interact with a SQL interpreter
;; process in the *SQL* buffer.  The *SQL* buffer is created by
;; calling a SQL interpreter-specific entry function.  Do *not* call
;; sql-interactive-mode by itself.

;; The list of currently supported interpreters and the corresponding
;; entry function used to create the *SQL* buffers is shown with
;; `sql-help' (M-x sql-help).

;; Since sql-interactive-mode is built on top of the general
;; command-interpreter-in-a-buffer mode (comint mode), it shares a
;; common base functionality, and a common set of bindings, with all
;; modes derived from comint mode.  This makes these modes easier to
;; use.

;; sql-mode can be used to enable syntactic hilighting for SQL
;; statements in another buffer.  SQL statements can then be sent to
;; the SQL process in the *SQL* buffer.  sql-mode has already been
;; used as a template to a simple PL/SQL mode.

;; For documentation on the functionality provided by comint mode, and
;; the hooks available for customising it, see the file `comint.el'.

;; Hint for newbies: take a look at `dabbrev-expand' and `abbrev-mode'.

;;; Requirements for Emacs 19.34:

;; If you are using Emacs 19.34, you will have to get and install
;; the file regexp-opt.el
;; <URL:ftp://ftp.ifi.uio.no/pub/emacs/emacs-20.3/lisp/emacs-lisp/regexp-opt.el>
;; and the custom package
;; <URL:http://www.dina.kvl.dk/~abraham/custom/>.

;;; Bugs:

;; Using sql-ms (isql by Microsoft): When commands with syntax errors
;; or execution errors are executed, there is no server feedback.
;; This happens in stored procedures for example.  The server messages
;; only appear after the process is exited.  This makes things
;; somewhat unreliable.

;;; To Do:

;; Add better hilight support for other brands; there is a bias towards
;; Oracle because that's what I use at work.  Anybody else just send in
;; your lists of reserved words, keywords and builtin functions!

;; Add different hilighting levels.

;;; Thanks to all the people who helped me out:

;; Kai Blauberg <kai.blauberg@metla.fi>
;; <ibalaban@dalet.com>
;; Yair Friedman <yfriedma@JohnBryce.Co.Il>
;; Gregor Zych <zych@pool.informatik.rwth-aachen.de>


;;; Code:

(require 'comint)
;; Need the following to allow GNU Emacs 19 to compile the file.
(require 'regexp-opt)
(require 'custom)

;;; Allow customization

(defgroup SQL nil
  "Running a SQL interpreter from within Emacs buffers"
  :group 'processes)

;; These three variables will be used as defaults, if set.

(defcustom sql-user ""
  "*Default username."
  :type 'string
  :group 'SQL)

(defcustom sql-password ""
  "*Default password.

Storing your password in a textfile such as ~/.emacs could be dangerous.
Customizing your password will store it in your ~/.emacs file."
  :type 'string
  :group 'SQL)

(defcustom sql-database ""
  "*Default database."
  :type 'string
  :group 'SQL)

(defcustom sql-server ""
  "*Default server.

Currently, this is only used by MS isql."
  :type 'string
  :group 'SQL)

(defcustom sql-pop-to-buffer-after-send-region nil
  "*If t, pop to the buffer SQL statements are sent to.

After a call to `sql-send-region' or `sql-send-buffer',
the window is split and the SQLi buffer is shown.  If this
variable is not nil, that buffer's window will be selected."
  :type 'string
  :group 'SQL)

;; The usual hooks

(defcustom sql-interactive-mode-hook '()
  "*Hook for customising `sql-interactive-mode'."
  :type 'hook
  :group 'SQL)

(defcustom sql-mode-hook '()
  "*Hook for customising `sql-mode'."
  :type 'hook
  :group 'SQL)

;; Customisation for Oracle

(defcustom sql-oracle-program "sqlplus"
  "*Command to start sqlplus by Oracle.

Starts `sql-interactive-mode' after doing some setup.

Under NT, \"sqlplus\" usually starts the sqlplus \"GUI\".  In order to
start the sqlplus console, use \"plus33\" or something similar.  You
will find the file in your Orant\\bin directory.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

;; Customisation for SyBase

(defcustom sql-sybase-program "isql"
  "*Command to start isql by SyBase.  

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

;; Customisation for Informix

(defcustom sql-informix-program "dbaccess"
  "*Command to start dbaccess by Informix.  

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

;; Customisation for Ingres

(defcustom sql-ingres-program "sql"
  "*Command to start sql by Ingres.  

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

;; Customisation for Microsoft

(defcustom sql-ms-program "isql"
  "*Command to start isql by Microsoft.  

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

;; Customisation for Postgres

(defcustom sql-postgres-program "psql"
  "Command to start psql by Postgres.  

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)



;;; Variables which do not need customization

(defvar sql-user-history nil
  "History of usernames used.")

(defvar sql-database-history nil
  "History of databases used.")

(defvar sql-server-history nil
  "History of servers used.")

;; Passwords are not kept in a history.

(defvar sql-buffer nil
  "Current *SQL* buffer.")

(defvar sql-prompt-regexp nil
  "Prompt used to initialize `comint-prompt-regexp'.

You can change `comint-prompt-regexp' on `sql-interactive-mode-hook'.")

;; Keymap for sql-interactive-mode, based on comint-mode-map.

(if (not (string-match "XEmacs\\|Lucid" emacs-version))
    (defvar sql-interactive-mode-map 
      (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
	(define-key map "\C-j" 'sql-accumulate-and-indent)
	(define-key map "\C-c\C-w" 'sql-copy-column)
	map)
      "Mode map used for `sql-interactive-mode'.")
  ;; XEmacs
  (defvar sql-interactive-mode-map nil)
  (if (not sql-interactive-mode-map)
      (let ((map (make-keymap)))
	(set-keymap-parents map (list comint-mode-map))
	(set-keymap-name map 'sql-interactive-mode-map)
	(define-key map "\C-j" 'sql-accumulate-and-indent)
	(define-key map "\C-c\C-w" 'sql-copy-column)
	(setq sql-interactive-mode-map map))))

;; Keymap for sql-mode.

(defvar sql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'sql-send-region)
    (define-key map "\C-c\C-b" 'sql-send-buffer)
    (define-key map "\t" 'indent-relative)
    map)
  "Mode map used for `sql-mode'.")

;; easy menu for sql-mode.

(easy-menu-define 
 sql-mode-menu sql-mode-map 
 "Menu for `sql-mode'."
 '("SQL"
   ["Send Region" sql-send-region mark-active]
   ["Send Buffer" sql-send-buffer t]
   ["Pop to SQLi buffer after send" 
    sql-toggle-pop-to-buffer-after-send-region
    :style toggle
    :selected sql-pop-to-buffer-after-send-region]))

;; Abbreviations -- if you want more of them, define them in your
;; ~/.emacs file.  Abbrevs have to be enabled in your ~/.emacs, too.

(defvar sql-mode-abbrev-table nil
  "Abbrev table used in `sql-mode' and `sql-interactive-mode'.")
(if sql-mode-abbrev-table
    ()
  (let ((wrapper))
    (define-abbrev-table 'sql-mode-abbrev-table ())
    (define-abbrev sql-mode-abbrev-table  "ins" "insert" nil)
    (define-abbrev sql-mode-abbrev-table  "upd" "update" nil)
    (define-abbrev sql-mode-abbrev-table  "del" "delete" nil)
    (define-abbrev sql-mode-abbrev-table  "sel" "select" nil)))

;; Syntax Table

(defvar sql-mode-syntax-table 
  (let ((table (make-syntax-table)))
    ;; C-style comments /**/ (see elisp manual "Syntax Flags"))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    ;; double-dash starts comment
    (modify-syntax-entry ?- ". 12b" table)
    ;; newline and formfeed end coments
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\f "> b" table)
    table)
  "Syntax table used in `sql-mode' and `sql-interactive-mode'.")

;; Font lock support

(defvar sql-mode-ansi-font-lock-keywords nil
  "ANSI SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-ansi-font-lock-keywords.  You may want to
add functions and PL/SQL keywords.")
(if sql-mode-ansi-font-lock-keywords
    ()
  (let ((ansi-keywords (eval-when-compile
			 (concat "\\b"
				 (regexp-opt '(
"authorization" "avg" "begin" "close" "cobol" "commit"
"continue" "count" "declare" "double" "end" "escape"
"exec" "fetch" "foreign" "fortran" "found" "go" "goto" "indicator" 
"key" "language" "max" "min" "module" "numeric" "open" "pascal" "pli"
"precision" "primary" "procedure" "references" "rollback"
"schema" "section" "some" "sqlcode" "sqlerror" "sum" "work") t) "\\b")))
	(ansi-reserved-words (eval-when-compile
			       (concat "\\b"
				       (regexp-opt '(
"all" "and" "any" "as" "asc" "between" "by" "check" "create"
"current" "default" "delete" "desc" "distinct" "exists" "float" "for"
"from" "grant" "group" "having" "in" "insert" "into" "is"
"like" "not" "null" "of" "on" "option" "or" "order" "privileges"
"public" "select" "set" "table" "to" "union" "unique"
"update" "user" "values" "view" "where" "with") t) "\\b")))
	(ansi-types (eval-when-compile
		      (concat "\\b"
			      (regexp-opt '(
;; ANSI Keywords that look like types					  
"character" "cursor" "dec" "int" "real" 
;; ANSI Reserved Word that look like types
"char" "integer" "smallint" ) t) "\\b"))))
    (setq sql-mode-ansi-font-lock-keywords
	  (list (cons ansi-keywords font-lock-function-name-face)
		(cons ansi-reserved-words font-lock-keyword-face)
		(cons ansi-types font-lock-type-face)))))

(defvar sql-mode-oracle-font-lock-keywords nil
  "Oracle SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-oracle-font-lock-keywords.  You may want
to add functions and PL/SQL keywords.")
(if sql-mode-oracle-font-lock-keywords
    ()
  (let ((oracle-keywords (eval-when-compile
			   (concat "\\b"
				   (regexp-opt '(
"admin" "after" "allocate" "analyze" "archive" "archivelog" "backup"
"become" "before" "block" "body" "cache" "cancel" "cascade" "change"
"checkpoint" "compile" "constraint" "constraints" "contents"
"controlfile" "cycle" "database" "datafile" "dba" "disable" "dismount"
"dump" "each" "enable" "events" "except" "exceptions" "execute"
"explain" "extent" "externally" "flush" "force" "freelist" "freelists"
"function" "groups" "including" "initrans" "instance" "layer" "link"
"lists" "logfile" "manage" "manual" "maxdatafiles" "maxinistances"
"maxlogfiles" "maxloghistory" "maxlogmembers" "maxtrans" "maxvalue"
"minextents" "minvalue" "mount" "new" "next" "noarchivelog" "nocache"
"nocycle" "nomaxvalue" "nominvalue" "none" "noorder" "noresetlogs"
"normal" "nosort" "off" "old" "only" "optimal" "own" "package"
"parallel" "pctincrease" "pctused" "plan" "private" "profile" "quota"
"read" "recover" "referencing" "resetlogs" "restricted" "reuse" "role"
"roles" "savepoint" "scn" "segment" "sequence" "shared" "snapshot"
"sort" "statement_id" "statistics" "stop" "storage" "switch" "system"
"tables" "tablespace" "temporary" "thread" "time" "tracing"
"transaction" "triggers" "truncate" "under" "unlimited" "until" "use"
"using" "when" "write") t) "\\b")))
	(oracle-reserved-words (eval-when-compile
				 (concat "\\b"
					 (regexp-opt '(
"access" "add" "alter" "audit" "cluster" "column" "comment" "compress"
"connect" "drop" "else" "exclusive" "file" "grant"
"identified" "immediate" "increment" "index" "initial" "intersect"
"level" "lock" "long" "maxextents" "minus" "mode" "modify" "noaudit"
"nocompress" "nowait" "number" "offline" "online" "pctfree" "prior"
"raw" "rename" "resource" "revoke" "row" "rowlabel" "rownum"
"rows" "session" "share" "size" "start" "successful" "synonym" "sysdate"
"then" "trigger" "uid" "validate" "whenever") t) "\\b")))
	(oracle-types (eval-when-compile
			(concat "\\b"
				(regexp-opt '(
;; Oracle Keywords that look like types
;; Oracle Reserved Words that look like types
"date" "decimal" "rowid" "varchar" "varchar2") t) "\\b")))
	(oracle-builtin-functions (eval-when-compile
			(concat "\\b"
				(regexp-opt '(
;; Misc Oracle builtin functions
"abs" "add_months" "ascii" "avg" "ceil" "chartorowid" "chr" "concat"
"convert" "cos" "cosh" "count" "currval" "decode" "dump" "exp" "floor"
"glb" "greatest" "greatest_lb" "hextoraw" "initcap" "instr" "instrb"
"last_day" "least" "least_ub" "length" "lengthb" "ln" "log" "lower"
"lpad" "ltrim" "lub" "max" "min" "mod" "months_between" "new_time"
"next_day" "nextval" "nls_initcap" "nls_lower" "nls_upper" "nlssort"
"nvl" "power" "rawtohex" "replace" "round" "rowidtochar" "rpad"
"rtrim" "sign" "sin" "sinh" "soundex" "sqlcode" "sqlerrm" "sqrt"
"stddev" "sum" "substr" "substrb" "tan" "tanh" "to_char"
"to_date" "to_label" "to_multi_byte" "to_number" "to_single_byte"
"translate" "trunc" "uid" "upper" "userenv" "variance" "vsize") t) "\\b"))))
    (setq sql-mode-oracle-font-lock-keywords
	  (append sql-mode-ansi-font-lock-keywords
		  (list (cons oracle-keywords font-lock-function-name-face)
			(cons oracle-reserved-words font-lock-keyword-face)
			;; XEmacs doesn't have font-lock-builtin-face
			(if (string-match "XEmacs\\|Lucid" emacs-version)
			    (cons oracle-builtin-functions font-lock-preprocessor-face)
			  ;; GNU Emacs 19 doesn't have it either
			  (if (string-match "GNU Emacs 19" emacs-version)
			      (cons oracle-builtin-functions font-lock-function-name-face)
			    ;; Emacs
			    (cons oracle-builtin-functions font-lock-builtin-face)))
			(cons oracle-types font-lock-type-face))))))

(defvar sql-mode-postgres-font-lock-keywords nil
  "Postgres SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-postgres-font-lock-keywords.")

(if sql-mode-postgres-font-lock-keywords
    ()
  (let ((postgres-reserved-words (eval-when-compile
				 (concat "\\b"
					 (regexp-opt '(
"language"
) t) "\\b")))
	(postgres-types (eval-when-compile 
			  (concat "\\b" 
				  (regexp-opt '( 
"bool" "box" "circle" "char" "char2" "char4" "char8" "char16" "date" 
"float4" "float8" "int2" "int4" "int8" "line" "lseg" "money" "path" 
"point" "polygon" "serial" "text" "time" "timespan" "timestamp" "varchar"
) t)"\\b")))
	(postgres-builtin-functions (eval-when-compile
			(concat "\\b"
				(regexp-opt '(
;; Misc Postgres builtin functions
"abstime" "age" "area" "box" "center" "date_part" "date_trunc"
"datetime" "dexp" "diameter" "dpow" "float" "float4" "height"
"initcap" "integer" "isclosed" "isfinite" "isoldpath" "isopen"
"length" "lower" "lpad" "ltrim" "pclose" "point" "points" "popen"
"position" "radius" "reltime" "revertpoly" "rpad" "rtrim" "substr"
"substring" "text" "timespan" "translate" "trim" "upgradepath"
"upgradepoly" "upper" "varchar" "width"
) t) "\\b"))))
    (setq sql-mode-postgres-font-lock-keywords
	  (append sql-mode-ansi-font-lock-keywords
		  (list (cons postgres-reserved-words font-lock-keyword-face)
			;; XEmacs doesn't have font-lock-builtin-face
			(if (string-match "XEmacs\\|Lucid" emacs-version)
			    (cons postgres-builtin-functions font-lock-preprocessor-face)
			  ;; Emacs
			  (cons postgres-builtin-functions font-lock-builtin-face))
			(cons postgres-types font-lock-type-face))))))


(defvar sql-mode-font-lock-keywords sql-mode-ansi-font-lock-keywords
  "SQL keywords used by font-lock.

This variable defaults to `sql-mode-ansi-font-lock-keywords'.  This is
used for the default `font-lock-defaults' value in `sql-mode'.  This
can be changed by some entry functions to provide more hilighting.")



;;; Small functions

(defun sql-accumulate-and-indent ()
  "Continue SQL statement on the next line."
  (interactive)
  ;; comint-accumulate is a Emacs 20.X thingie
  (if (not (string-match "XEmacs\\|Lucid\\|GNU Emacs 19" emacs-version))
      (comint-accumulate))
  (indent-according-to-mode))

;;;###autoload
(defun sql-help ()
  "Shows short help for the SQL modes.

Use an entry function to open an interactive SQL buffer.  This buffer is
usually named *SQL*.  The name of the major mode is SQLi.

Use the following commands to start a specific SQL interpreter:

psql by PostGres: \\[sql-postgres]
SQL*Plus: \\[sql-oracle]
dbaccess: \\[sql-informix]
isql (Sybase): \\[sql-sybase]
sql (Ingres): \\[sql-ingres]
isql (Microsoft): \\[sql-ms]

Once you have the SQLi buffer, you can enter SQL statements in the
buffer.  The output generated is appended to the buffer and a new prompt
is generated.  See the In/Out menu in the SQLi buffer for some functions
that help you navigate through the buffer, the input history, etc.

Put a line with a call to autoload into your `~/.emacs' file for each
entry function you want to use regularly:

\(autoload 'sql-postgres \"sql\" \"Interactive SQL mode.\" t)

If you have a really complex SQL statement or if you are writing a
procedure, you can do this in a separate buffer.  Put the new buffer in
`sql-mode' by calling \\[sql-mode].  The name of this buffer can be
anything.  The name of the major mode is SQL.

In this SQL buffer (SQL mode), you can send the region or the entire
buffer to the interactive SQL buffer (SQLi mode).  The results are
appended to the SQLi buffer without disturbing your SQL buffer."
  (interactive)
  (describe-function 'sql-help))

(defun sql-read-passwd (prompt &optional default)
  "Read a password using PROMPT.
Optional DEFAULT is password to start with.  This function calls
`read-passwd' if it is available.  If not, function
`ange-ftp-read-passwd' is called.  This should always be available,
even in old versions of Emacs."
  (if (fboundp 'read-passwd)
      (read-passwd prompt nil default)
    (unless (fboundp 'ange-ftp-read-passwd)
      (autoload 'ange-ftp-read-passwd "ange-ftp"))
    (ange-ftp-read-passwd prompt default)))

(defun sql-get-login (&rest what)
  "Get username, password and database from the user.

The variables `sql-user', `sql-password', `sql-server' and
`sql-database' can be customised.  They are used as the default
values.  Usernames, servers and databases are stored in
`sql-user-history', `sql-server-history' and `database-history'.
Passwords are not stored in a history.

Parameter WHAT is a list of the arguments passed to this function.
The function asks for the username if WHAT contains symbol `user', for
the password if it contains symbol `password', for the server if it
contains symbol `server', and for the database if it contains symbol
`database'.

In order to ask the user for username, password and database, call the
function like this: (sql-get-login 'user 'password 'database)."
  (interactive)
  (if (memq 'user what)
      (setq sql-user 
	    (read-from-minibuffer "User: " sql-user nil nil
				  sql-user-history)))
  (if (memq 'password what)
      (setq sql-password 
	    (sql-read-passwd "Password: " sql-password)))
  (if (memq 'server what)
      (setq sql-server 
	    (read-from-minibuffer "Server: " sql-server nil nil
				  sql-server-history)))
  (if (memq 'database what)
      (setq sql-database 
	    (read-from-minibuffer "Database: " sql-database nil nil
				  sql-database-history))))

(defun sql-copy-column ()
  "Copy current column to the end of buffer.
Inserts SELECT or commas if appropriate."
  (interactive)
  (let ((column))
    (save-excursion
      (setq column (buffer-substring
		  (progn (forward-char 1) (backward-sexp 1) (point))
		  (progn (forward-sexp 1) (point))))
      (goto-char (point-max))
      (cond
       ;; if empty command line, insert SELECT
       ((save-excursion (beginning-of-line) 
			(looking-at (concat comint-prompt-regexp "$")))
	(insert "SELECT "))
       ;; else if appending to SELECT or ORDER BY, insert a comma
       ((save-excursion 
	  (re-search-backward "\\b\\(select\\|order by\\) .+"
			      (save-excursion (beginning-of-line) (point)) t))
	(insert ", "))
       ;; else insert a space
       (t
	(if (eq (preceding-char) ? )
	    nil
	  (insert " "))))
      ;; in any case, insert the column
      (insert column)
      (message "%s" column))))



;;; Sending the region to the SQLi buffer.

(defun sql-send-region (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (if (buffer-live-p sql-buffer)
      (save-excursion
	(comint-send-region sql-buffer start end)
	(if (string-match "\n$" (buffer-substring start end))
	    ()
	  (comint-send-string sql-buffer "\n"))
	(if sql-pop-to-buffer-after-send-region 
	    (pop-to-buffer sql-buffer)
	  (display-buffer sql-buffer)))
    (message "No SQL process started.")))

(defun sql-send-buffer ()
  "Send the buffer contents to the SQL process."
  (interactive)
  (sql-send-region (point-min) (point-max)))

(defun sql-toggle-pop-to-buffer-after-send-region (&optional value)
  "Toggle `sql-pop-to-buffer-after-send-region'.

If given the optional parameter VALUE, sets
sql-toggle-pop-to-buffer-after-send-region to VALUE."
  (interactive "P")
  (if value
      (setq sql-pop-to-buffer-after-send-region value)
    (setq sql-pop-to-buffer-after-send-region 
	  (null sql-pop-to-buffer-after-send-region ))))



;;; SQL mode -- uses SQL interactive mode

;;;###autoload
(defun sql-mode ()
  "Major mode to edit SQL.

You can send SQL statements to the *SQL* buffer using
\\[sql-send-region].  Such a buffer must exist before you can do this.
See `sql-help'.

\\{sql-mode-map} 
Customization: Entry to this mode runs the `sql-mode-hook'.

Here is an example for your .emacs file.  It opens every file ending in
.sql with sql-mode.

\(setq auto-mode-alist (append auto-mode-alist
                              \(list '(\"\\\\.sql$\" . sql-mode))))"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sql-mode)
  (setq mode-name "SQL")
  (use-local-map sql-mode-map)
  (set-syntax-table sql-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sql-mode-font-lock-keywords 
			     nil t ((95 . "w") (46 . "w"))))
  (setq local-abbrev-table sql-mode-abbrev-table)
  (setq abbrev-all-caps 1)
  (run-hooks 'sql-mode-hook))



;;; SQL interactive mode

(put 'sql-interactive-mode 'mode-class 'special)

(defun sql-interactive-mode ()
  "Major mode to use a SQL interpreter interactively.

Do not call this function by yourself.  The environment must be
initialized by an entry function specific for the SQL interpreter.  See
`sql-help' for a list of available entry functions.

\\[comint-send-input] after the end of the process' output sends the
text from the end of process to the end of the current line.
\\[comint-send-input] before end of process output copies the current
line minus the prompt to the end of the buffer and sends it.
\\[comint-copy-old-input] just copies the current line.
Use \\[sql-accumulate-and-indent] to enter multi-line statements.

If you want to make multiple SQL buffers, rename the `*SQL*' buffer
using \\[rename-buffer] or \\[rename-uniquely] and start a new process.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\\{sql-interactive-mode-map}
Customization: Entry to this mode runs the hooks on `comint-mode-hook'
and `sql-interactive-mode-hook' (in that order).  Before each input, the
hooks on `comint-input-filter-functions' are run.  After each SQL
interpreter output, the hooks on `comint-output-filter-functions' are
run.

Variable `comint-input-ring-file-name' controls the initialisation of
the input ring history.

Variables `comint-output-filter-functions', a hook, and
`comint-scroll-to-bottom-on-input' and
`comint-scroll-to-bottom-on-output' control whether input and output
cause the window to scroll to the end of the buffer.

If you want to make SQL buffers limited in length, add the function
`comint-truncate-buffer' to `comint-output-filter-functions'.

Here is an example for your .emacs file.  It keeps the *SQL* Buffer a
certain length and stores all inputs in an input-ring file.

\(add-hook 'sql-interactive-mode-hook
    \(function (lambda ()
        \(setq comint-input-ring-file-name \"~/.sql_history\")
        \(setq comint-output-filter-functions 'comint-truncate-buffer))))

Here is another example.  It will always put point back to the statement
you entered, right above the output it created.

\(setq comint-output-filter-functions 
       \(function (lambda (STR) (comint-show-output))))"
  (comint-mode)
  (setq comint-prompt-regexp sql-prompt-regexp)
  (setq major-mode 'sql-interactive-mode)
  (setq mode-name "SQLi")
  (use-local-map sql-interactive-mode-map)
  (set-syntax-table sql-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sql-mode-font-lock-keywords t t ((95 . "w") (46 . "w"))))
  (setq left-margin 5)
  (setq local-abbrev-table sql-mode-abbrev-table)
  (setq abbrev-all-caps 1)
  (set-process-sentinel (get-buffer-process sql-buffer) 'sql-stop)
  (run-hooks 'sql-interactive-mode-hook)
  ;; calling the hook before calling comint-read-input-ring allows users
  ;; to set comint-input-ring-file-name in sql-interactive-mode-hook.
  (comint-read-input-ring t))

(defun sql-stop (process event)
  "Called when the SQL process is stopped.

Writes the input history to a history file using `comint-write-input-ring'
and inserts a short message in the SQL buffer.

This function is a sentinel watching the SQL interpreter process.
Sentinels will always get the two parameters PROCESS and EVENT."
  (comint-write-input-ring)
  (if (buffer-live-p sql-buffer)
      (insert (format "\nProcess %s %s\n" process event))))



;;; Entry functions for different SQL interpreters.

(defun sql-oracle ()
  "Run sqlplus by Oracle as an inferior process.

If buffer *SQL* exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-oracle-program'.  Login
uses the variables `sql-user', `sql-password' and `sql-database' as
defaults, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-oracle].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (if (comint-check-proc "*SQL*")
      (pop-to-buffer "*SQL*")
    (sql-get-login 'user 'password 'database)
    (message "Login...")
    ;; Produce user/password@database construct.  Password without user
    ;; is meaningless; database without user/password is meaningless,
    ;; because "@param" will ask sqlplus to interpret the script
    ;; "param".
    (let ((parameter nil))
      (if (not (string= "" sql-user))
	  (if (not (string= "" sql-password))
	      (setq parameter (concat sql-user "/" sql-password))
	    (setq parameter sql-user)))
      (if (and parameter (not (string= "" sql-database)))
	  (setq parameter (concat parameter "@" sql-database)))
      (if parameter
	  (set-buffer (make-comint "SQL" sql-oracle-program nil parameter))
	(set-buffer (make-comint "SQL" sql-oracle-program nil))))
    (setq sql-prompt-regexp "^SQL> ")
    (setq sql-buffer (current-buffer))
    ;; set sql-mode-font-lock-keywords to something different before
    ;; calling sql-interactive-mode.
    (setq sql-mode-font-lock-keywords sql-mode-oracle-font-lock-keywords)
    (sql-interactive-mode)
    (message "Login...done")
    (pop-to-buffer sql-buffer)))



(defun sql-sybase ()
  "Run isql by SyBase as an inferior process.

If buffer *SQL* exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-sybase-program'.  Login uses
the variables `sql-user', `sql-password' and `sql-database' as defaults,
if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-sybase].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (if (comint-check-proc "*SQL*")
      (pop-to-buffer "*SQL*")
    (sql-get-login 'user 'password 'database)
    (message "Login...")
    ;; Put all parameters to the program (if defined) in a list and call
    ;; make-comint.
    (let ((params '("-w" "2048" "-n")))
      ;; I had a zillion versions of this using nconc and mapcar,
      ;; mixtures of eval, list and quotes -- you have been warned.
      (if (not (string= "" sql-database))
	  (setq params (append (list "-S" sql-database) params)))
      (if (not (string= "" sql-password))
	  (setq params (append (list "-P" sql-password) params)))
      (if (not (string= "" sql-user))
	  (setq params (append (list "-U" sql-user) params)))
      (set-buffer (apply 'make-comint "SQL" sql-sybase-program 
			 nil params)))
    (setq sql-prompt-regexp "^SQL> ")
    (setq sql-buffer (current-buffer))
    (sql-interactive-mode)
    (message "Login...done")
    (pop-to-buffer sql-buffer)))



(defun sql-informix ()
  "Run dbaccess by Informix as an inferior process.

If buffer *SQL* exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-informix-program'.  Login uses
the variable `sql-database' as default, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-informix].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (if (comint-check-proc "*SQL*")
      (pop-to-buffer "*SQL*")
    (sql-get-login 'database)
    (message "Login...")
    ;; username and password are ignored.
    (if (string= "" sql-database)
	(set-buffer (make-comint "SQL" sql-informix-program nil))
      (set-buffer (make-comint "SQL" sql-informix-program nil sql-database)))
    (setq sql-prompt-regexp "^SQL> ")
    (setq sql-buffer (current-buffer))
    (sql-interactive-mode)
    (message "Login...done")
    (pop-to-buffer sql-buffer)))



(defun sql-ingres ()
  "Run sql by Ingres as an inferior process.

If buffer *SQL* exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-ingres-program'.  Login uses
the variable `sql-database' as default, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-ingres].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (if (comint-check-proc "*SQL*")
      (pop-to-buffer "*SQL*")
    (sql-get-login 'database)
    (message "Login...")
    ;; username and password are ignored.
    (if (string= "" sql-database)
	(set-buffer (make-comint "SQL" sql-ingres-program nil))
      (set-buffer (make-comint "SQL" sql-ingres-program nil sql-database)))
    (setq sql-prompt-regexp "^\* ")
    (setq sql-buffer (current-buffer))
    (sql-interactive-mode)
    (message "Login...done")
    (pop-to-buffer sql-buffer)))



(defun sql-ms ()
  "Run isql by Microsoft as an inferior process.

If buffer *SQL* exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-ms-program'.  Login uses the
variables `sql-user', `sql-password', `sql-server' and `sql-database'
as defaults, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-sybase].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (if (comint-check-proc "*SQL*")
      (pop-to-buffer "*SQL*")
    (sql-get-login 'user 'password 'database 'server)
    (message "Login...")
    ;; Put all parameters to the program (if defined) in a list and call
    ;; make-comint.
    (let ((params '("-w 300")))
      (if (not (string= "" sql-server))
        (setq params (append (list "-S" sql-server) params)))
      (if (not (string= "" sql-database))
        (setq params (append (list "-d" sql-database) params)))
      (if (not (string= "" sql-user))
	  (setq params (append (list "-U" sql-user) params)))
      (if (not (string= "" sql-password))
	  (setq params (append (list "-P" sql-password) params))
	;; If -P is passed to ISQL as the last argument without a password,
	;; it's considered null.
	(setq params (append params (list "-P"))))
      (set-buffer (apply 'make-comint "SQL" sql-ms-program
			 nil params)))
    (setq sql-prompt-regexp "^[0-9]*>")
    (setq sql-buffer (current-buffer))
    (sql-interactive-mode)
    (message "Login...done")
    (pop-to-buffer sql-buffer)))




;;;###autoload
(defun sql-postgres ()
  "Run psql by Postgres as an inferior process.

If buffer *SQL* exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-postgres-program'.  Login uses
the variable `sql-database' as default, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-postgres].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.  If your output lines end with ^M, 
your might try undecided-dos as a coding system.  If this doesn't help,
Try to set `comint-output-filter-functions' like this:

\(setq comint-output-filter-functions (append comint-output-filter-functions
					     '(comint-strip-ctrl-m)))

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (if (comint-check-proc "*SQL*")
      (pop-to-buffer "*SQL*")
    (sql-get-login 'database)
    (message "Login...")
    ;; username and password are ignored.
    (if (string= "" sql-database)
	(set-buffer (make-comint "SQL" sql-postgres-program nil))
      (set-buffer (make-comint "SQL" sql-postgres-program nil sql-database)))
    (setq sql-prompt-regexp "^.*> *")
    ;; This is a lousy hack to prevent psql from truncating it's output
    ;; and giving stupid warnings. If s.o. knows a way to prevent psql 
    ;; from acting this way, then I would be very thankful to
    ;; incorporate this (Gregor Zych <zych@pool.informatik.rwth-aachen.de>)
    (comint-send-string "*SQL*" "\\o \| cat\n")
    (setq sql-mode-font-lock-keywords sql-mode-postgres-font-lock-keywords)
    (setq sql-buffer (current-buffer))
    (sql-interactive-mode)
    (message "Login...done")
    (pop-to-buffer sql-buffer)))

(provide 'sql)

;;; sql.el ends here
