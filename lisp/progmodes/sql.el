;;; sql.el --- specialized comint.el for SQL interpreters

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
;; Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Michael Mauger <mmaug@yahoo.com>
;; Version: 2.0.1
;; Keywords: comm languages processes
;; URL: http://savannah.gnu.org/cgi-bin/viewcvs/emacs/emacs/lisp/progmodes/sql.el
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?SqlMode

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

;; Please send bug reports and bug fixes to the mailing list at
;; sql.el@gnu.org.  If you want to subscribe to the mailing list, send
;; mail to sql.el-request@gnu.org with `subscribe sql.el FIRSTNAME
;; LASTNAME' in the mail body.

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
;; process in a SQLi buffer (usually called `*SQL*').  The SQLi buffer
;; is created by calling a SQL interpreter-specific entry function.  Do
;; *not* call sql-interactive-mode by itself.

;; The list of currently supported interpreters and the corresponding
;; entry function used to create the SQLi buffers is shown with
;; `sql-help' (M-x sql-help).

;; Since sql-interactive-mode is built on top of the general
;; command-interpreter-in-a-buffer mode (comint mode), it shares a
;; common base functionality, and a common set of bindings, with all
;; modes derived from comint mode.  This makes these modes easier to
;; use.

;; sql-mode can be used to keep editing SQL statements.  The SQL
;; statements can be sent to the SQL process in the SQLi buffer.

;; For documentation on the functionality provided by comint mode, and
;; the hooks available for customizing it, see the file `comint.el'.

;; Hint for newbies: take a look at `dabbrev-expand', `abbrev-mode', and
;; `imenu-add-menubar-index'.

;;; Requirements for Emacs 19.34:

;; If you are using Emacs 19.34, you will have to get and install
;; the file regexp-opt.el
;; <URL:ftp://ftp.ifi.uio.no/pub/emacs/emacs-20.3/lisp/emacs-lisp/regexp-opt.el>
;; and the custom package
;; <URL:http://www.dina.kvl.dk/~abraham/custom/>.

;;; Bugs:

;; sql-ms now uses osql instead of isql.  Osql flushes its error
;; stream more frequently than isql so that error messages are
;; available.  There is no prompt and some output still is buffered.
;; This improves the interaction under Emacs but it still is somewhat
;; awkward.

;; Quoted identifiers are not supported for hilighting.  Most
;; databases support the use of double quoted strings in place of
;; identifiers; ms (Microsoft SQLServer) also supports identifiers
;; enclosed within brackets [].

;; ChangeLog available on request.

;;; Product Support:

;; To add support for additional SQL products the following steps
;; must be followed ("xyz" is the name of the product in the examples
;; below):

;; 1) Add the product to `sql-product' choice list.

;;     (const :tag "XyzDB" xyz)

;; 2) Add an entry to the `sql-product-alist' list.

;;     (xyz
;;      :font-lock sql-mode-xyz-font-lock-keywords
;;      :sqli-login (user password server database)
;;      :sqli-connect sql-connect-xyz
;;      :sqli-prompt-regexp "^xyzdb> "
;;      :sqli-prompt-length 7
;;      :sqli-input-sender nil
;;      :syntax-alist ((?# . "w")))

;; 3) Add customizable values for the product interpreter and options.

;;     ;; Customization for XyzDB
;;
;;     (defcustom sql-xyz-program "ixyz"
;;       "*Command to start ixyz by XyzDB."
;;       :type 'file
;;       :group 'SQL)
;;
;;     (defcustom sql-xyz-options '("-X" "-Y" "-Z")
;;       "*List of additional options for `sql-xyz-program'."
;;       :type '(repeat string)
;;       :group 'SQL)

;; 4) Add an entry to SQL->Product submenu.

;;     ["XyzDB" sql-highlight-xyz-keywords
;;      :style radio
;;      :selected (eq sql-product 'xyz)]

;; 5) Add the font-lock specifications.  At a minimum, default to
;;    using ANSI keywords.  See sql-mode-oracle-font-lock-keywords for
;;    a more complex example.

;;     (defvar sql-mode-xyz-font-lock-keywords nil
;;       "XyzDB SQL keywords used by font-lock.")

;; 6) Add a product highlighting function.

;;     (defun sql-highlight-xyz-keywords ()
;;       "Highlight XyzDB keywords."
;;       (interactive)
;;       (sql-set-product 'xyz))

;; 7) Add an autoloaded SQLi function.

;;     ;;;###autoload
;;     (defun sql-xyz ()
;;       "Run ixyz by XyzDB as an inferior process."
;;       (interactive)
;;       (sql-product-interactive 'xyz))

;; 8) Add a connect function which formats the command line arguments
;;    and starts the product interpreter in a comint buffer.  See the
;;    existing connect functions for examples of the types of
;;    processing available.

;;     (defun sql-connect-xyz ()
;;       "Create comint buffer and connect to XyzDB using the login
;;     parameters and command options."
;;
;;         ;; Do something with `sql-user', `sql-password',
;;         ;; `sql-database', and `sql-server'.
;;         (let ((params sql-xyz-options))
;;           (if (not (string= "" sql-server))
;;              (setq params (append (list "-S" sql-server) params)))
;;           (if (not (string= "" sql-database))
;;               (setq params (append (list "-D" sql-database) params)))
;;           (if (not (string= "" sql-password))
;;               (setq params (append (list "-P" sql-password) params)))
;;           (if (not (string= "" sql-user))
;;               (setq params (append (list "-U" sql-user) params)))
;;           (set-buffer (apply 'make-comint "SQL" sql-xyz-program
;;                              nil params))))

;; 9) Save and compile sql.el.

;;; To Do:

;; Add better hilight support for other brands; there is a bias towards
;; Oracle because that's what I use at work.  Anybody else just send in
;; your lists of reserved words, keywords and builtin functions!  As
;; long as I don't receive any feedback, everything is hilighted with
;; ANSI keywords only.  I received the list of ANSI keywords from a
;; user; if you know of any changes, let me know.

;; Add different hilighting levels.

;;; Thanks to all the people who helped me out:

;; Alex Schroeder <alex@gnu.org>
;; Kai Blauberg <kai.blauberg@metla.fi>
;; <ibalaban@dalet.com>
;; Yair Friedman <yfriedma@JohnBryce.Co.Il>
;; Gregor Zych <zych@pool.informatik.rwth-aachen.de>
;; nino <nino@inform.dk>
;; Berend de Boer <berend@pobox.com>
;; Adam Jenkins <adam@thejenkins.org>
;; Michael Mauger <mmaug@yahoo.com> -- improved product support
;; Drew Adams <drew.adams@oracle.com> -- Emacs 20 support
;; Harald Maier <maierh@myself.com> -- sql-send-string
;; Stefan Monnier <monnier@iro.umontreal.ca> -- font-lock corrections



;;; Code:

(require 'comint)
;; Need the following to allow GNU Emacs 19 to compile the file.
(eval-when-compile
  (require 'regexp-opt))
(require 'custom)
(eval-when-compile ;; needed in Emacs 19, 20
  (setq max-specpdl-size 2000))

;;; Allow customization

(defgroup SQL nil
  "Running a SQL interpreter from within Emacs buffers."
  :version "20.4"
  :group 'processes)

;; These four variables will be used as defaults, if set.

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
  "*Default server or host."
  :type 'string
  :group 'SQL)

;; SQL Product support
(defcustom sql-product 'ansi
  "*Select the SQL database product used so that buffers can be
highlighted properly when you open them."
  :type '(choice (const :tag "ANSI" ansi)
		 (const :tag "DB2" db2)
		 (const :tag "Informix" informix)
		 (const :tag "Ingres" ingres)
		 (const :tag "Interbase" interbase)
		 (const :tag "Linter" linter)
		 (const :tag "Microsoft" ms)
		 (const :tag "MySQL" mysql)
		 (const :tag "Oracle" oracle)
		 (const :tag "PostGres" postgres)
		 (const :tag "Solid" solid)
		 (const :tag "SQLite" sqlite)
		 (const :tag "Sybase" sybase))
  :group 'SQL)

(defvar sql-interactive-product nil
  "Product under `sql-interactive-mode'.")

(defvar sql-product-alist
  '((ansi
     :font-lock sql-mode-ansi-font-lock-keywords)
    (db2
     :font-lock sql-mode-db2-font-lock-keywords
     :sqli-login nil
     :sqli-connect sql-connect-db2
     :sqli-prompt-regexp "^db2 => "
     :sqli-prompt-length 7)
    (informix
     :font-lock sql-mode-informix-font-lock-keywords
     :sqli-login (database)
     :sqli-connect sql-connect-informix
     :sqli-prompt-regexp "^SQL> "
     :sqli-prompt-length 5)
    (ingres
     :font-lock sql-mode-ingres-font-lock-keywords
     :sqli-login (database)
     :sqli-connect sql-connect-ingres
     :sqli-prompt-regexp "^\* "
     :sqli-prompt-length 2)
    (interbase
     :font-lock sql-mode-interbase-font-lock-keywords
     :sqli-login (user password database)
     :sqli-connect sql-connect-interbase
     :sqli-prompt-regexp "^SQL> "
     :sqli-prompt-length 5)
    (linter
     :font-lock sql-mode-linter-font-lock-keywords
     :sqli-login (user password database server)
     :sqli-connect sql-connect-linter
     :sqli-prompt-regexp "^SQL>"
     :sqli-prompt-length 4)
    (ms
     :font-lock sql-mode-ms-font-lock-keywords
     :sqli-login (user password server database)
     :sqli-connect sql-connect-ms
     :sqli-prompt-regexp "^[0-9]*>"
     :sqli-prompt-length 5
     :syntax-alist ((?@ . "w")))
    (mysql
     :font-lock sql-mode-mysql-font-lock-keywords
     :sqli-login (user password database server)
     :sqli-connect sql-connect-mysql
     :sqli-prompt-regexp "^mysql> "
     :sqli-prompt-length 6)
    (oracle
     :font-lock sql-mode-oracle-font-lock-keywords
     :sqli-login (user password database)
     :sqli-connect sql-connect-oracle
     :sqli-prompt-regexp "^SQL> "
     :sqli-prompt-length 5
     :syntax-alist ((?$ . "w") (?# . "w")))
    (postgres
     :font-lock sql-mode-postgres-font-lock-keywords
     :sqli-login (user database server)
     :sqli-connect sql-connect-postgres
     :sqli-prompt-regexp "^.*[#>] *"
     :sqli-prompt-length 5)
    (solid
     :font-lock sql-mode-solid-font-lock-keywords
     :sqli-login (user password server)
     :sqli-connect sql-connect-solid
     :sqli-prompt-regexp "^"
     :sqli-prompt-length 0)
    (sqlite
     :font-lock sql-mode-sqlite-font-lock-keywords
     :sqli-login (user password server database)
     :sqli-connect sql-connect-sqlite
     :sqli-prompt-regexp "^sqlite> "
     :sqli-prompt-length 8)
    (sybase
     :font-lock sql-mode-sybase-font-lock-keywords
     :sqli-login (server user password database)
     :sqli-connect sql-connect-sybase
     :sqli-prompt-regexp "^SQL> "
     :sqli-prompt-length 5
     :syntax-alist ((?@ . "w")))
    )
  "This variable contains a list of product features for each of the
SQL products handled by `sql-mode'.  Without an entry in this list a
product will not be properly highlighted and will not support
`sql-interactive-mode'.

Each element in the list is in the following format:

 \(PRODUCT FEATURE VALUE ...)

where PRODUCT is the appropriate value of `sql-product'.  The product
name is then followed by FEATURE-VALUE pairs.  If a FEATURE is not
specified, its VALUE is treated as nil.  FEATURE must be one of the
following:

 :font-lock             name of the variable containing the product
                        specific font lock highlighting patterns.

 :sqli-login            a list of login parameters (i.e., user,
                        password, database and server) needed to
                        connect to the database.

 :sqli-connect          the name of a function which accepts no
                        parameters that will use the values of
                        `sql-user', `sql-password',
                        `sql-database' and `sql-server' to open a
                        comint buffer and connect to the
                        database.  Do product specific
                        configuration of comint in this function.

 :sqli-prompt-regexp    a regular expression string that matches
                        the prompt issued by the product
                        interpreter.  (Not needed in 21.3+)

 :sqli-prompt-length    the length of the prompt on the line.(Not
                        needed in 21.3+)

 :syntax-alist          an alist of syntax table entries to enable
                        special character treatment by font-lock and
                        imenu. ")

;; misc customization of sql.el behaviour

(defcustom sql-electric-stuff nil
  "Treat some input as electric.
If set to the symbol `semicolon', then hitting `;' will send current
input in the SQLi buffer to the process.
If set to the symbol `go', then hitting `go' on a line by itself will
send current input in the SQLi buffer to the process.
If set to nil, then you must use \\[comint-send-input] in order to send
current input in the SQLi buffer to the process."
  :type '(choice (const :tag "Nothing" nil)
		 (const :tag "The semikolon `;'" semicolon)
		 (const :tag "The string `go' by itself" go))
  :version "20.8"
  :group 'SQL)

(defcustom sql-pop-to-buffer-after-send-region nil
  "*If t, pop to the buffer SQL statements are sent to.

After a call to `sql-send-region' or `sql-send-buffer',
the window is split and the SQLi buffer is shown.  If this
variable is not nil, that buffer's window will be selected
by calling `pop-to-buffer'.  If this variable is nil, that
buffer is shown using `display-buffer'."
  :type 'boolean
  :group 'SQL)

;; imenu support for sql-mode.

(defvar sql-imenu-generic-expression
  ;; Items are in reverse order because they are rendered in reverse.
  '(("Rules/Defaults" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*\\(rule\\|default\\)\\s-+\\(\\w+\\)" 3)
    ("Sequences" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*sequence\\s-+\\(\\w+\\)" 2)
    ("Triggers" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*trigger\\s-+\\(\\w+\\)" 2)
    ("Functions" "^\\s-*\\(create\\s-+\\(\\w+\\s-+\\)*\\)?function\\s-+\\(\\w+\\)" 3)
    ("Procedures" "^\\s-*\\(create\\s-+\\(\\w+\\s-+\\)*\\)?proc\\(edure\\)?\\s-+\\(\\w+\\)" 4)
    ("Packages" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*package\\s-+\\(body\\s-+\\)?\\(\\w+\\)" 3)
    ("Indexes" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*index\\s-+\\(\\w+\\)" 2)
    ("Tables/Views" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*\\(table\\|view\\)\\s-+\\(\\w+\\)" 3))
  "Define interesting points in the SQL buffer for `imenu'.

This is used to set `imenu-generic-expression' when SQL mode is
entered.  Subsequent changes to sql-imenu-generic-expression will not
affect existing SQL buffers because imenu-generic-expression is a
local variable.")

;; history file

(defcustom sql-input-ring-file-name nil
  "*If non-nil, name of the file to read/write input history.

You have to set this variable if you want the history of your commands
saved from one Emacs session to the next.  If this variable is set,
exiting the SQL interpreter in an SQLi buffer will write the input
history to the specified file.  Starting a new process in a SQLi buffer
will read the input history from the specified file.

This is used to initialize `comint-input-ring-file-name'.

Note that the size of the input history is determined by the variable
`comint-input-ring-size'."
  :type '(choice (const :tag "none" nil)
		 (file))
  :group 'SQL)

(defcustom sql-input-ring-separator "\n--\n"
  "*Separator between commands in the history file.

If set to \"\\n\", each line in the history file will be interpreted as
one command.  Multi-line commands are split into several commands when
the input ring is initialized from a history file.

This variable used to initialize `comint-input-ring-separator'.
`comint-input-ring-separator' is part of Emacs 21; if your Emacs
does not have it, setting `sql-input-ring-separator' will have no
effect.  In that case multiline commands will be split into several
commands when the input history is read, as if you had set
`sql-input-ring-separator' to \"\\n\"."
  :type 'string
  :group 'SQL)

;; The usual hooks

(defcustom sql-interactive-mode-hook '()
  "*Hook for customizing `sql-interactive-mode'."
  :type 'hook
  :group 'SQL)

(defcustom sql-mode-hook '()
  "*Hook for customizing `sql-mode'."
  :type 'hook
  :group 'SQL)

(defcustom sql-set-sqli-hook '()
  "*Hook for reacting to changes of `sql-buffer'.

This is called by `sql-set-sqli-buffer' when the value of `sql-buffer'
is changed."
  :type 'hook
  :group 'SQL)

;; Customization for Oracle

(defcustom sql-oracle-program "sqlplus"
  "*Command to start sqlplus by Oracle.

Starts `sql-interactive-mode' after doing some setup.

Under NT, \"sqlplus\" usually starts the sqlplus \"GUI\".  In order to
start the sqlplus console, use \"plus33\" or something similar.  You
will find the file in your Orant\\bin directory.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

(defcustom sql-oracle-options nil
  "*List of additional options for `sql-oracle-program'."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

;; Customization for SQLite

(defcustom sql-sqlite-program "sqlite"
  "*Command to start SQLite.

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

(defcustom sql-sqlite-options nil
  "*List of additional options for `sql-sqlite-program'.
The following list of options is reported to make things work
on Windows: \"-C\" \"-t\" \"-f\" \"-n\"."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

;; Customization for MySql

(defcustom sql-mysql-program "mysql"
  "*Command to start mysql by TcX.

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

(defcustom sql-mysql-options nil
  "*List of additional options for `sql-mysql-program'.
The following list of options is reported to make things work
on Windows: \"-C\" \"-t\" \"-f\" \"-n\"."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

;; Customization for Solid

(defcustom sql-solid-program "solsql"
  "*Command to start SOLID SQL Editor.

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

;; Customization for SyBase

(defcustom sql-sybase-program "isql"
  "*Command to start isql by SyBase.

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

(defcustom sql-sybase-options nil
  "*List of additional options for `sql-sybase-program'.
Some versions of isql might require the -n option in order to work."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

;; Customization for Informix

(defcustom sql-informix-program "dbaccess"
  "*Command to start dbaccess by Informix.

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

;; Customization for Ingres

(defcustom sql-ingres-program "sql"
  "*Command to start sql by Ingres.

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

;; Customization for Microsoft

(defcustom sql-ms-program "osql"
  "*Command to start osql by Microsoft.

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

(defcustom sql-ms-options '("-w" "300" "-n")
  ;; -w is the linesize
  "*List of additional options for `sql-ms-program'."
  :type '(repeat string)
  :version "22.1"
  :group 'SQL)

;; Customization for Postgres

(defcustom sql-postgres-program "psql"
  "Command to start psql by Postgres.

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

(defcustom sql-postgres-options '("-P" "pager=off")
  "*List of additional options for `sql-postgres-program'.
The default setting includes the -P option which breaks older versions
of the psql client (such as version 6.5.3).  The -P option is equivalent
to the --pset option.  If you want the psql to prompt you for a user
name, add the string \"-u\" to the list of options.  If you want to
provide a user name on the command line (newer versions such as 7.1),
add your name with a \"-U\" prefix (such as \"-Umark\") to the list."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

;; Customization for Interbase

(defcustom sql-interbase-program "isql"
  "*Command to start isql by Interbase.

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

(defcustom sql-interbase-options nil
  "*List of additional options for `sql-interbase-program'."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

;; Customization for DB2

(defcustom sql-db2-program "db2"
  "*Command to start db2 by IBM.

Starts `sql-interactive-mode' after doing some setup.

The program can also specify a TCP connection.  See `make-comint'."
  :type 'file
  :group 'SQL)

(defcustom sql-db2-options nil
  "*List of additional options for `sql-db2-program'."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

;; Customization for Linter

(defcustom sql-linter-program "inl"
  "*Command to start inl by RELEX.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-linter-options nil
  "*List of additional options for `sql-linter-program'."
  :type '(repeat string)
  :version "21.3"
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
  "Current SQLi buffer.

The global value of sql-buffer is the name of the latest SQLi buffer
created.  Any SQL buffer created will make a local copy of this value.
See `sql-interactive-mode' for more on multiple sessions.  If you want
to change the SQLi buffer a SQL mode sends its SQL strings to, change
the local value of `sql-buffer' using \\[sql-set-sqli-buffer].")

(defvar sql-prompt-regexp nil
  "Prompt used to initialize `comint-prompt-regexp'.

You can change `sql-prompt-regexp' on `sql-interactive-mode-hook'.")

(defvar sql-prompt-length 0
  "Prompt used to set `left-margin' in `sql-interactive-mode'.

You can change `sql-prompt-length' on `sql-interactive-mode-hook'.")

(defvar sql-alternate-buffer-name nil
  "Buffer-local string used to possibly rename the SQLi buffer.

Used by `sql-rename-buffer'.")

;; Keymap for sql-interactive-mode.

(defvar sql-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (if (functionp 'set-keymap-parent)
	(set-keymap-parent map comint-mode-map); Emacs
      (set-keymap-parents map (list comint-mode-map))); XEmacs
    (if (functionp 'set-keymap-name)
	(set-keymap-name map 'sql-interactive-mode-map)); XEmacs
    (define-key map (kbd "C-j") 'sql-accumulate-and-indent)
    (define-key map (kbd "C-c C-w") 'sql-copy-column)
    (define-key map (kbd "O") 'sql-magic-go)
    (define-key map (kbd "o") 'sql-magic-go)
    (define-key map (kbd ";") 'sql-magic-semicolon)
    map)
  "Mode map used for `sql-interactive-mode'.
Based on `comint-mode-map'.")

;; Keymap for sql-mode.

(defvar sql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'sql-send-paragraph)
    (define-key map (kbd "C-c C-r") 'sql-send-region)
    (define-key map (kbd "C-c C-s") 'sql-send-string)
    (define-key map (kbd "C-c C-b") 'sql-send-buffer)
    map)
  "Mode map used for `sql-mode'.")

;; easy menu for sql-mode.

(easy-menu-define
 sql-mode-menu sql-mode-map
 "Menu for `sql-mode'."
 '("SQL"
   ["Send Paragraph" sql-send-paragraph (and (buffer-live-p sql-buffer)
					     (get-buffer-process sql-buffer))]
   ["Send Region" sql-send-region (and (or (and (boundp 'mark-active); Emacs
						mark-active)
					   (mark t)); XEmacs
				       (buffer-live-p sql-buffer)
				       (get-buffer-process sql-buffer))]
   ["Send Buffer" sql-send-buffer (and (buffer-live-p sql-buffer)
				       (get-buffer-process sql-buffer))]
   ["Send String" sql-send-string t]
   ["--" nil nil]
   ["Start SQLi session" sql-product-interactive (sql-product-feature :sqli-connect)]
   ["Show SQLi buffer" sql-show-sqli-buffer t]
   ["Set SQLi buffer" sql-set-sqli-buffer t]
   ["Pop to SQLi buffer after send"
    sql-toggle-pop-to-buffer-after-send-region
    :style toggle
    :selected sql-pop-to-buffer-after-send-region]
   ["--" nil nil]
   ("Product"
    ["ANSI" sql-highlight-ansi-keywords
     :style radio
     :selected (eq sql-product 'ansi)]
    ["DB2" sql-highlight-db2-keywords
     :style radio
     :selected (eq sql-product 'db2)]
    ["Informix" sql-highlight-informix-keywords
     :style radio
     :selected (eq sql-product 'informix)]
    ["Ingres" sql-highlight-ingres-keywords
     :style radio
     :selected (eq sql-product 'ingres)]
    ["Interbase" sql-highlight-interbase-keywords
     :style radio
     :selected (eq sql-product 'interbase)]
    ["Linter" sql-highlight-linter-keywords
     :style radio
     :selected (eq sql-product 'linter)]
    ["MS SQLServer" sql-highlight-ms-keywords
     :style radio
     :selected (eq sql-product 'ms)]
    ["MySQL" sql-highlight-mysql-keywords
     :style radio
     :selected (eq sql-product 'mysql)]
    ["Oracle" sql-highlight-oracle-keywords
     :style radio
     :selected (eq sql-product 'oracle)]
    ["Postgres" sql-highlight-postgres-keywords
     :style radio
     :selected (eq sql-product 'postgres)]
    ["Solid" sql-highlight-solid-keywords
     :style radio
     :selected (eq sql-product 'solid)]
    ["SQLite" sql-highlight-sqlite-keywords
     :style radio
     :selected (eq sql-product 'sqlite)]
    ["Sybase" sql-highlight-sybase-keywords
     :style radio
     :selected (eq sql-product 'sybase)]
    )))

;; easy menu for sql-interactive-mode.

(easy-menu-define
 sql-interactive-mode-menu sql-interactive-mode-map
 "Menu for `sql-interactive-mode'."
 '("SQL"
   ["Rename Buffer" sql-rename-buffer t]))

;; Abbreviations -- if you want more of them, define them in your
;; ~/.emacs file.  Abbrevs have to be enabled in your ~/.emacs, too.

(defvar sql-mode-abbrev-table nil
  "Abbrev table used in `sql-mode' and `sql-interactive-mode'.")
(unless sql-mode-abbrev-table
  (define-abbrev-table 'sql-mode-abbrev-table nil)
  (mapcar
    ;; In Emacs 21.3+, provide SYSTEM-FLAG to define-abbrev.
   '(lambda (abbrev)
      (let ((name (car abbrev))
	    (expansion (cdr abbrev)))
	(condition-case nil
	    (define-abbrev sql-mode-abbrev-table name expansion nil 0 t)
	  (error
	   (define-abbrev sql-mode-abbrev-table name expansion)))))
   '(("ins" "insert")
    ("upd" "update")
    ("del" "delete")
    ("sel" "select")
    ("proc" "procedure")
    ("func" "function")
    ("cr" "create"))))

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
    ;; single quotes (') quotes delimit strings
    (modify-syntax-entry ?' "\"" table)
    ;; backslash is no escape character
    (modify-syntax-entry ?\\ "." table)
    table)
  "Syntax table used in `sql-mode' and `sql-interactive-mode'.")

;; Font lock support

(defvar sql-mode-font-lock-object-name
  (list (concat "^\\s-*\\(create\\|drop\\|alter\\)\\s-+" ;; lead off with CREATE, DROP or ALTER
		"\\(\\w+\\s-+\\)*"  ;; optional intervening keywords
		"\\(table\\|view\\|package\\(\\s-+body\\)?\\|proc\\(edure\\)?"
		"\\|function\\|trigger\\|sequence\\|rule\\|default\\)\\s-+"
		"\\(\\w+\\)")
	6 'font-lock-function-name-face)

  "Pattern to match the names of top-level objects.

The pattern matches the name in a CREATE, DROP or ALTER
statement.  The format of variable should be a valid
`font-lock-keywords' entry.")

(defmacro sql-keywords-re (&rest keywords)
  "Compile-time generation of regexp matching any one of KEYWORDS."
  `(eval-when-compile
     (concat "\\b"
	     (regexp-opt ',keywords t)
	     "\\b")))

(defvar sql-mode-ansi-font-lock-keywords
  (let ((ansi-funcs (sql-keywords-re
"abs" "avg" "bit_length" "cardinality" "cast" "char_length"
"character_length" "coalesce" "convert" "count" "current_date"
"current_path" "current_role" "current_time" "current_timestamp"
"current_user" "extract" "localtime" "localtimestamp" "lower" "max"
"min" "mod" "nullif" "octet_length" "overlay" "placing" "session_user"
"substring" "sum" "system_user" "translate" "treat" "trim" "upper"
"user"
))

	(ansi-non-reserved (sql-keywords-re
"ada" "asensitive" "assignment" "asymmetric" "atomic" "between"
"bitvar" "called" "catalog_name" "chain" "character_set_catalog"
"character_set_name" "character_set_schema" "checked" "class_origin"
"cobol" "collation_catalog" "collation_name" "collation_schema"
"column_name" "command_function" "command_function_code" "committed"
"condition_number" "connection_name" "constraint_catalog"
"constraint_name" "constraint_schema" "contains" "cursor_name"
"datetime_interval_code" "datetime_interval_precision" "defined"
"definer" "dispatch" "dynamic_function" "dynamic_function_code"
"existing" "exists" "final" "fortran" "generated" "granted"
"hierarchy" "hold" "implementation" "infix" "insensitive" "instance"
"instantiable" "invoker" "key_member" "key_type" "length" "m"
"message_length" "message_octet_length" "message_text" "method" "more"
"mumps" "name" "nullable" "number" "options" "overlaps" "overriding"
"parameter_mode" "parameter_name" "parameter_ordinal_position"
"parameter_specific_catalog" "parameter_specific_name"
"parameter_specific_schema" "pascal" "pli" "position" "repeatable"
"returned_length" "returned_octet_length" "returned_sqlstate"
"routine_catalog" "routine_name" "routine_schema" "row_count" "scale"
"schema_name" "security" "self" "sensitive" "serializable"
"server_name" "similar" "simple" "source" "specific_name" "style"
"subclass_origin" "sublist" "symmetric" "system" "table_name"
"transaction_active" "transactions_committed"
"transactions_rolled_back" "transform" "transforms" "trigger_catalog"
"trigger_name" "trigger_schema" "type" "uncommitted" "unnamed"
"user_defined_type_catalog" "user_defined_type_name"
"user_defined_type_schema"
))

	(ansi-reserved (sql-keywords-re
"absolute" "action" "add" "admin" "after" "aggregate" "alias" "all"
"allocate" "alter" "and" "any" "are" "as" "asc" "assertion" "at"
"authorization" "before" "begin" "both" "breadth" "by" "call"
"cascade" "cascaded" "case" "catalog" "check" "class" "close"
"collate" "collation" "column" "commit" "completion" "connect"
"connection" "constraint" "constraints" "constructor" "continue"
"corresponding" "create" "cross" "cube" "current" "cursor" "cycle"
"data" "day" "deallocate" "declare" "default" "deferrable" "deferred"
"delete" "depth" "deref" "desc" "describe" "descriptor" "destroy"
"destructor" "deterministic" "diagnostics" "dictionary" "disconnect"
"distinct" "domain" "drop" "dynamic" "each" "else" "end" "equals"
"escape" "every" "except" "exception" "exec" "execute" "external"
"false" "fetch" "first" "for" "foreign" "found" "free" "from" "full"
"function" "general" "get" "global" "go" "goto" "grant" "group"
"grouping" "having" "host" "hour" "identity" "ignore" "immediate" "in"
"indicator" "initialize" "initially" "inner" "inout" "input" "insert"
"intersect" "into" "is" "isolation" "iterate" "join" "key" "language"
"last" "lateral" "leading" "left" "less" "level" "like" "limit"
"local" "locator" "map" "match" "minute" "modifies" "modify" "module"
"month" "names" "natural" "new" "next" "no" "none" "not" "null" "of"
"off" "old" "on" "only" "open" "operation" "option" "or" "order"
"ordinality" "out" "outer" "output" "pad" "parameter" "parameters"
"partial" "path" "postfix" "prefix" "preorder" "prepare" "preserve"
"primary" "prior" "privileges" "procedure" "public" "read" "reads"
"recursive" "references" "referencing" "relative" "restrict" "result"
"return" "returns" "revoke" "right" "role" "rollback" "rollup"
"routine" "rows" "savepoint" "schema" "scroll" "search" "second"
"section" "select" "sequence" "session" "set" "sets" "size" "some"
"space" "specific" "specifictype" "sql" "sqlexception" "sqlstate"
"sqlwarning" "start" "state" "statement" "static" "structure" "table"
"temporary" "terminate" "than" "then" "timezone_hour"
"timezone_minute" "to" "trailing" "transaction" "translation"
"trigger" "true" "under" "union" "unique" "unknown" "unnest" "update"
"usage" "using" "value" "values" "variable" "view" "when" "whenever"
"where" "with" "without" "work" "write" "year"
))

	(ansi-types (sql-keywords-re
"array" "binary" "bit" "blob" "boolean" "char" "character" "clob"
"date" "dec" "decimal" "double" "float" "int" "integer" "interval"
"large" "national" "nchar" "nclob" "numeric" "object" "precision"
"real" "ref" "row" "scope" "smallint" "time" "timestamp" "varchar"
"varying" "zone"
)))

    `((,ansi-non-reserved . font-lock-keyword-face)
      (,ansi-reserved     . font-lock-keyword-face)
      (,ansi-funcs        . font-lock-builtin-face)
      (,ansi-types        . font-lock-type-face)))

  "ANSI SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-ansi-font-lock-keywords.  You may want to
add functions and PL/SQL keywords.")

(defvar sql-mode-oracle-font-lock-keywords
  (let ((oracle-functions (sql-keywords-re
"abs" "acos" "add_months" "ascii" "asciistr" "asin" "atan" "atan2"
"avg" "bfilename" "bin_to_num" "bitand" "cast" "ceil" "chartorowid"
"chr" "coalesce" "compose" "concat" "convert" "corr" "cos" "cosh"
"count" "covar_pop" "covar_samp" "cume_dist" "current_date"
"current_timestamp" "current_user" "dbtimezone" "decode" "decompose"
"dense_rank" "depth" "deref" "dump" "empty_clob" "existsnode" "exp"
"extract" "extractvalue" "first" "first_value" "floor" "following"
"from_tz" "greatest" "group_id" "grouping_id" "hextoraw" "initcap"
"instr" "lag" "last" "last_day" "last_value" "lead" "least" "length"
"ln" "localtimestamp" "lower" "lpad" "ltrim" "make_ref" "max" "min"
"mod" "months_between" "new_time" "next_day" "nls_charset_decl_len"
"nls_charset_id" "nls_charset_name" "nls_initcap" "nls_lower"
"nls_upper" "nlssort" "ntile" "nullif" "numtodsinterval"
"numtoyminterval" "nvl" "nvl2" "over" "path" "percent_rank"
"percentile_cont" "percentile_disc" "power" "preceding" "rank"
"ratio_to_report" "rawtohex" "rawtonhex" "reftohex" "regr_"
"regr_avgx" "regr_avgy" "regr_count" "regr_intercept" "regr_r2"
"regr_slope" "regr_sxx" "regr_sxy" "regr_syy" "replace" "round"
"row_number" "rowidtochar" "rowidtonchar" "rpad" "rtrim"
"sessiontimezone" "sign" "sin" "sinh" "soundex" "sqrt" "stddev"
"stddev_pop" "stddev_samp" "substr" "sum" "sys_connect_by_path"
"sys_context" "sys_dburigen" "sys_extract_utc" "sys_guid" "sys_typeid"
"sys_xmlagg" "sys_xmlgen" "sysdate" "systimestamp" "tan" "tanh"
"to_char" "to_clob" "to_date" "to_dsinterval" "to_lob" "to_multi_byte"
"to_nchar" "to_nclob" "to_number" "to_single_byte" "to_timestamp"
"to_timestamp_tz" "to_yminterval" "translate" "treat" "trim" "trunc"
"tz_offset" "uid" "unbounded" "unistr" "updatexml" "upper" "user"
"userenv" "var_pop" "var_samp" "variance" "vsize" "width_bucket" "xml"
"xmlagg" "xmlattribute" "xmlcolattval" "xmlconcat" "xmlelement"
"xmlforest" "xmlsequence" "xmltransform"
))

	(oracle-keywords (sql-keywords-re
"abort" "access" "accessed" "account" "activate" "add" "admin"
"advise" "after" "agent" "aggregate" "all" "allocate" "allow" "alter"
"always" "analyze" "ancillary" "and" "any" "apply" "archive"
"archivelog" "array" "as" "asc" "associate" "at" "attribute"
"attributes" "audit" "authenticated" "authid" "authorization" "auto"
"autoallocate" "automatic" "availability" "backup" "before" "begin"
"behalf" "between" "binding" "bitmap" "block" "blocksize" "body"
"both" "buffer_pool" "build" "by"  "cache" "call" "cancel"
"cascade" "case" "category" "certificate" "chained" "change" "check"
"checkpoint" "child" "chunk" "class" "clear" "clone" "close" "cluster"
"column" "column_value" "columns" "comment" "commit" "committed"
"compatibility" "compile" "complete" "composite_limit" "compress"
"compute" "connect" "connect_time" "consider" "consistent"
"constraint" "constraints" "constructor" "contents" "context"
"continue" "controlfile" "corruption" "cost" "cpu_per_call"
"cpu_per_session" "create" "cross" "cube" "current" "currval" "cycle"
"dangling" "data" "database" "datafile" "datafiles" "day" "ddl"
"deallocate" "debug" "default" "deferrable" "deferred" "definer"
"delay" "delete" "demand" "desc" "determines" "deterministic"
"dictionary" "dimension" "directory" "disable" "disassociate"
"disconnect" "distinct" "distinguished" "distributed" "dml" "drop"
"each" "element" "else" "enable" "end" "equals_path" "escape"
"estimate" "except" "exceptions" "exchange" "excluding" "exists"
"expire" "explain" "extent" "external" "externally"
"failed_login_attempts" "fast" "file" "final" "finish" "flush" "for"
"force" "foreign" "freelist" "freelists" "freepools" "fresh" "from"
"full" "function" "functions" "generated" "global" "global_name"
"globally" "grant" "group" "grouping" "groups" "guard" "hash"
"hashkeys" "having" "heap" "hierarchy" "id" "identified" "identifier"
"idle_time" "immediate" "in" "including" "increment" "index" "indexed"
"indexes" "indextype" "indextypes" "indicator" "initial" "initialized"
"initially" "initrans" "inner" "insert" "instance" "instantiable"
"instead" "intersect" "into" "invalidate" "is" "isolation" "java"
"join"  "keep" "key" "kill" "language" "left" "less" "level"
"levels" "library" "like" "like2" "like4" "likec" "limit" "link"
"list" "lob" "local" "location" "locator" "lock" "log" "logfile"
"logging" "logical" "logical_reads_per_call"
"logical_reads_per_session"  "managed" "management" "manual" "map"
"mapping" "master" "matched" "materialized" "maxdatafiles"
"maxextents" "maximize" "maxinstances" "maxlogfiles" "maxloghistory"
"maxlogmembers" "maxsize" "maxtrans" "maxvalue" "member" "memory"
"merge" "migrate" "minextents" "minimize" "minimum" "minus" "minvalue"
"mode" "modify" "monitoring" "month" "mount" "move" "movement" "name"
"named" "natural" "nested" "never" "new" "next" "nextval" "no"
"noarchivelog" "noaudit" "nocache" "nocompress" "nocopy" "nocycle"
"nodelay" "noforce" "nologging" "nomapping" "nomaxvalue" "nominimize"
"nominvalue" "nomonitoring" "none" "noorder" "noparallel" "norely"
"noresetlogs" "noreverse" "normal" "norowdependencies" "nosort"
"noswitch" "not" "nothing" "notimeout" "novalidate" "nowait" "null"
"nulls" "object" "of" "off" "offline" "oidindex" "old" "on" "online"
"only" "open" "operator" "optimal" "option" "or" "order"
"organization" "out" "outer" "outline" "overflow" "overriding"
"package" "packages" "parallel" "parallel_enable" "parameters"
"parent" "partition" "partitions" "password" "password_grace_time"
"password_life_time" "password_lock_time" "password_reuse_max"
"password_reuse_time" "password_verify_function" "pctfree"
"pctincrease" "pctthreshold" "pctused" "pctversion" "percent"
"performance" "permanent" "pfile" "physical" "pipelined" "plan"
"post_transaction" "pragma" "prebuilt" "preserve" "primary" "private"
"private_sga" "privileges" "procedure" "profile" "protection" "public"
"purge" "query" "quiesce" "quota" "range" "read" "reads" "rebuild"
"records_per_block" "recover" "recovery" "recycle" "reduced" "ref"
"references" "referencing" "refresh" "register" "reject" "relational"
"rely" "rename" "reset" "resetlogs" "resize" "resolve" "resolver"
"resource" "restrict" "restrict_references" "restricted" "result"
"resumable" "resume" "retention" "return" "returning" "reuse"
"reverse" "revoke" "rewrite" "right" "rnds" "rnps" "role" "roles"
"rollback" "rollup" "row" "rowdependencies" "rownum" "rows" "sample"
"savepoint" "scan" "schema" "scn" "scope" "segment" "select"
"selectivity" "self" "sequence" "serializable" "session"
"sessions_per_user" "set" "sets" "settings" "shared" "shared_pool"
"shrink" "shutdown" "siblings" "sid" "single" "size" "skip" "some"
"sort" "source" "space" "specification" "spfile" "split" "standby"
"start" "statement_id" "static" "statistics" "stop" "storage" "store"
"structure" "subpartition" "subpartitions" "substitutable"
"successful" "supplemental" "suspend" "switch" "switchover" "synonym"
"sys" "system" "table" "tables" "tablespace" "tempfile" "template"
"temporary" "test" "than" "then" "thread" "through" "time_zone"
"timeout" "to" "trace" "transaction" "trigger" "triggers" "truncate"
"trust" "type" "types" "unarchived" "under" "under_path" "undo"
"uniform" "union" "unique" "unlimited" "unlock" "unquiesce"
"unrecoverable" "until" "unusable" "unused" "update" "upgrade" "usage"
"use" "using" "validate" "validation" "value" "values" "variable"
"varray" "version" "view" "wait" "when" "whenever" "where" "with"
"without" "wnds" "wnps" "work" "write" "xmldata" "xmlschema" "xmltype"
))

	(oracle-types (sql-keywords-re
"bfile" "blob" "byte" "char" "character" "clob" "date" "dec" "decimal"
"double" "float" "int" "integer" "interval" "long" "national" "nchar"
"nclob" "number" "numeric" "nvarchar2" "precision" "raw" "real"
"rowid" "second" "smallint" "time" "timestamp" "urowid" "varchar"
"varchar2" "varying" "year" "zone"
))

	(plsql-functions (sql-keywords-re
"%bulk_rowcount" "%found" "%isopen" "%notfound" "%rowcount" "%rowtype"
"%type" "extend" "prior"
))

	(plsql-keywords (sql-keywords-re
"autonomous_transaction" "bulk" "char_base" "collect" "constant"
"cursor" "declare" "do" "elsif" "exception_init" "execute" "exit"
"extends" "false" "fetch" "forall" "goto" "hour" "if" "interface"
"loop" "minute" "number_base" "ocirowid" "opaque" "others" "rowtype"
"separate" "serially_reusable" "sql" "sqlcode" "sqlerrm" "subtype"
"the" "timezone_abbr" "timezone_hour" "timezone_minute"
"timezone_region" "true" "varrying" "while"
))

	(plsql-type (sql-keywords-re
"binary_integer" "boolean" "naturaln" "pls_integer" "positive"
"positiven" "record" "signtype" "string"
))

	(plsql-warning (sql-keywords-re
"access_into_null" "case_not_found" "collection_is_null"
"cursor_already_open" "dup_val_on_index" "invalid_cursor"
"invalid_number" "login_denied" "no_data_found" "not_logged_on"
"program_error" "rowtype_mismatch" "self_is_null" "storage_error"
"subscript_beyond_count" "subscript_outside_limit" "sys_invalid_rowid"
"timeout_on_resource" "too_many_rows" "value_error" "zero_divide"
"exception" "notfound"
))

	(sqlplus-commands
	 (eval-when-compile (concat "^\\(\\("
				    (regexp-opt '(
"@" "@@" "accept" "append" "archive" "attribute" "break"
"btitle" "change" "clear" "column" "connect" "copy" "define"
"del" "describe" "disconnect" "edit" "execute" "exit" "get" "help"
"host" "input" "list" "password" "pause" "print" "prompt" "recover"
"remark" "repfooter" "repheader" "run" "save" "show" "shutdown"
"spool" "start" "startup" "store" "timing" "ttitle" "undefine"
"variable" "whenever"

) t)

   "\\)\\|"
   "\\(compute\\s-+\\(avg\\|cou\\|min\\|max\\|num\\|sum\\|std\\|var\\)\\)\\|"
   "\\(set\\s-+\\(appi\\(nfo\\)?\\|array\\(size\\)?\\|"
   "auto\\(commit\\)?\\|autop\\(rint\\)?\\|autorecovery\\|"
   "autot\\(race\\)?\\|blo\\(ckterminator\\)?\\|cmds\\(ep\\)?\\|"
   "colsep\\|com\\(patibility\\)?\\|con\\(cat\\)?\\|"
   "copyc\\(ommit\\)?\\|copytypecheck\\|def\\(ine\\)?\\|"
   "describe\\|echo\\|editf\\(ile\\)?\\|emb\\(edded\\)?\\|"
   "esc\\(ape\\)?\\|feed\\(back\\)?\\|flagger\\|"
   "flu\\(sh\\)?\\|hea\\(ding\\)?\\|heads\\(ep\\)?\\|"
   "instance\\|lin\\(esize\\)?\\|lobof\\(fset\\)?\\|"
   "logsource\\|long\\|longc\\(hunksize\\)?\\|mark\\(up\\)?\\|"
   "newp\\(age\\)?\\|null\\|numf\\(ormat\\)?\\|"
   "num\\(width\\)?\\|pages\\(ize\\)?\\|pau\\(se\\)?\\|"
   "recsep\\|recsepchar\\|serverout\\(put\\)?\\|"
   "shift\\(inout\\)?\\|show\\(mode\\)?\\|"
   "sqlbl\\(anklines\\)?\\|sqlc\\(ase\\)?\\|"
   "sqlco\\(ntinue\\)?\\|sqln\\(umber\\)?\\|"
   "sqlpluscompat\\(ibility\\)?\\|sqlpre\\(fix\\)?\\|"
   "sqlp\\(rompt\\)?\\|sqlt\\(erminator\\)?\\|"
   "suf\\(fix\\)?\\|tab\\|term\\(out\\)?\\|ti\\(me\\)?\\|"
   "timi\\(ng\\)?\\|trim\\(out\\)?\\|trims\\(pool\\)?\\|"
   "und\\(erline\\)?\\|ver\\(ify\\)?\\|wra\\(p\\)?\\)\\)\\)"
   "\\b.*$"
   ))))

    `((,sqlplus-commands . font-lock-doc-face)
      (,oracle-functions . font-lock-builtin-face)
      (,oracle-keywords  . font-lock-keyword-face)
      (,oracle-types     . font-lock-type-face)
      (,plsql-functions  . font-lock-builtin-face)
      (,plsql-keywords   . font-lock-keyword-face)
      (,plsql-type       . font-lock-type-face)
      (,plsql-warning    . font-lock-warning-face)))

  "Oracle SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-oracle-font-lock-keywords.  You may want
to add functions and PL/SQL keywords.")

(defvar sql-mode-postgres-font-lock-keywords
  (let ((pg-funcs (sql-keywords-re
"abbrev" "abs" "acos" "age" "area" "ascii" "asin" "atab2" "atan"
"atan2" "avg" "bit_length" "both" "broadcast" "btrim" "cbrt" "ceil"
"center" "char_length" "chr" "coalesce" "col_description" "convert"
"cos" "cot" "count" "current_database" "current_date" "current_schema"
"current_schemas" "current_setting" "current_time" "current_timestamp"
"current_user" "currval" "date_part" "date_trunc" "decode" "degrees"
"diameter" "encode" "exp" "extract" "floor" "get_bit" "get_byte"
"has_database_privilege" "has_function_privilege"
"has_language_privilege" "has_schema_privilege" "has_table_privilege"
"height" "host" "initcap" "isclosed" "isfinite" "isopen" "leading"
"length" "ln" "localtime" "localtimestamp" "log" "lower" "lpad"
"ltrim" "masklen" "max" "min" "mod" "netmask" "network" "nextval"
"now" "npoints" "nullif" "obj_description" "octet_length" "overlay"
"pclose" "pg_client_encoding" "pg_function_is_visible"
"pg_get_constraintdef" "pg_get_indexdef" "pg_get_ruledef"
"pg_get_userbyid" "pg_get_viewdef" "pg_opclass_is_visible"
"pg_operator_is_visible" "pg_table_is_visible" "pg_type_is_visible"
"pi" "popen" "position" "pow" "quote_ident" "quote_literal" "radians"
"radius" "random" "repeat" "replace" "round" "rpad" "rtrim"
"session_user" "set_bit" "set_byte" "set_config" "set_masklen"
"setval" "sign" "sin" "split_part" "sqrt" "stddev" "strpos" "substr"
"substring" "sum" "tan" "timeofday" "to_ascii" "to_char" "to_date"
"to_hex" "to_number" "to_timestamp" "trailing" "translate" "trim"
"trunc" "upper" "variance" "version" "width"
))

	(pg-reserved (sql-keywords-re
"abort" "access" "add" "after" "aggregate" "alignment" "all" "alter"
"analyze" "and" "any" "as" "asc" "assignment" "authorization"
"backward" "basetype" "before" "begin" "between" "binary" "by" "cache"
"called" "cascade" "case" "cast" "characteristics" "check"
"checkpoint" "class" "close" "cluster" "column" "comment" "commit"
"committed" "commutator" "constraint" "constraints" "conversion"
"copy" "create" "createdb" "createuser" "cursor" "cycle" "database"
"deallocate" "declare" "default" "deferrable" "deferred" "definer"
"delete" "delimiter" "desc" "distinct" "do" "domain" "drop" "each"
"element" "else" "encoding" "encrypted" "end" "escape" "except"
"exclusive" "execute" "exists" "explain" "extended" "external" "false"
"fetch" "finalfunc" "for" "force" "foreign" "forward" "freeze" "from"
"full" "function" "grant" "group" "gtcmp" "handler" "hashes" "having"
"immediate" "immutable" "implicit" "in" "increment" "index" "inherits"
"initcond" "initially" "input" "insensitive" "insert" "instead"
"internallength" "intersect" "into" "invoker" "is" "isnull"
"isolation" "join" "key" "language" "leftarg" "level" "like" "limit"
"listen" "load" "local" "location" "lock" "ltcmp" "main" "match"
"maxvalue" "merges" "minvalue" "mode" "move" "natural" "negator"
"next" "nocreatedb" "nocreateuser" "none" "not" "nothing" "notify"
"notnull" "null" "of" "offset" "oids" "on" "only" "operator" "or"
"order" "output" "owner" "partial" "passedbyvalue" "password" "plain"
"prepare" "primary" "prior" "privileges" "procedural" "procedure"
"public" "read" "recheck" "references" "reindex" "relative" "rename"
"reset" "restrict" "returns" "revoke" "rightarg" "rollback" "row"
"rule" "schema" "scroll" "security" "select" "sequence" "serializable"
"session" "set" "sfunc" "share" "show" "similar" "some" "sort1"
"sort2" "stable" "start" "statement" "statistics" "storage" "strict"
"stype" "sysid" "table" "temp" "template" "temporary" "then" "to"
"transaction" "trigger" "true" "truncate" "trusted" "type"
"unencrypted" "union" "unique" "unknown" "unlisten" "until" "update"
"usage" "user" "using" "vacuum" "valid" "validator" "values"
"variable" "verbose" "view" "volatile" "when" "where" "with" "without"
"work"
))

	(pg-types (sql-keywords-re
"anyarray" "bigint" "bigserial" "bit" "boolean" "box" "bytea" "char"
"character" "cidr" "circle" "cstring" "date" "decimal" "double"
"float4" "float8" "inet" "int2" "int4" "int8" "integer" "internal"
"interval" "language_handler" "line" "lseg" "macaddr" "money"
"numeric" "oid" "opaque" "path" "point" "polygon" "precision" "real"
"record" "regclass" "regoper" "regoperator" "regproc" "regprocedure"
"regtype" "serial" "serial4" "serial8" "smallint" "text" "time"
"timestamp" "varchar" "varying" "void" "zone"
)))

  `((,pg-funcs    . font-lock-builtin-face)
    (,pg-reserved . font-lock-keyword-face)
    (,pg-types    . font-lock-type-face)))

  "Postgres SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-postgres-font-lock-keywords.")

(defvar sql-mode-linter-font-lock-keywords
  (let ((linter-keywords (sql-keywords-re
"autocommit" "autoinc" "autorowid" "cancel" "cascade" "channel"
"committed" "count" "countblob" "cross" "current" "data" "database"
"datafile" "datafiles" "datesplit" "dba" "dbname" "default" "deferred"
"denied" "description" "device" "difference" "directory" "error"
"escape" "euc" "exclusive" "external" "extfile" "false" "file"
"filename" "filesize" "filetime" "filter" "findblob" "first" "foreign"
"full" "fuzzy" "global" "granted" "ignore" "immediate" "increment"
"indexes" "indexfile" "indexfiles" "indextime" "initial" "integrity"
"internal" "key" "last_autoinc" "last_rowid" "limit" "linter"
"linter_file_device" "linter_file_size" "linter_name_length" "ln"
"local" "login" "maxisn" "maxrow" "maxrowid" "maxvalue" "message"
"minvalue" "module" "names" "national" "natural" "new" "new_table"
"no" "node" "noneuc" "nulliferror" "numbers" "off" "old" "old_table"
"only" "operation" "optimistic" "option" "page" "partially" "password"
"phrase" "plan" "precision" "primary" "priority" "privileges"
"proc_info_size" "proc_par_name_len" "protocol" "quant" "range" "raw"
"read" "record" "records" "references" "remote" "rename" "replication"
"restart" "rewrite" "root" "row" "rule" "savepoint" "security"
"sensitive" "sequence" "serializable" "server" "since" "size" "some"
"startup" "statement" "station" "success" "sys_guid" "tables" "test"
"timeout" "trace" "transaction" "translation" "trigger"
"trigger_info_size" "true" "trunc" "uncommitted" "unicode" "unknown"
"unlimited" "unlisted" "user" "utf8" "value" "varying" "volumes"
"wait" "windows_code" "workspace" "write" "xml"
))

	(linter-reserved (sql-keywords-re
"access" "action" "add" "address" "after" "all" "alter" "always" "and"
"any" "append" "as" "asc" "ascic" "async" "at_begin" "at_end" "audit"
"aud_obj_name_len" "backup" "base" "before" "between" "blobfile"
"blobfiles" "blobpct" "brief" "browse" "by" "case" "cast" "check"
"clear" "close" "column" "comment" "commit" "connect" "contains"
"correct" "create" "delete" "desc" "disable" "disconnect" "distinct"
"drop" "each" "ef" "else" "enable" "end" "event" "except" "exclude"
"execute" "exists" "extract" "fetch" "finish" "for" "from" "get"
"grant" "group" "having" "identified" "in" "index" "inner" "insert"
"instead" "intersect" "into" "is" "isolation" "join" "left" "level"
"like" "lock" "mode" "modify" "not" "nowait" "null" "of" "on" "open"
"or" "order" "outer" "owner" "press" "prior" "procedure" "public"
"purge" "rebuild" "resource" "restrict" "revoke" "right" "role"
"rollback" "rownum" "select" "session" "set" "share" "shutdown"
"start" "stop" "sync" "synchronize" "synonym" "sysdate" "table" "then"
"to" "union" "unique" "unlock" "until" "update" "using" "values"
"view" "when" "where" "with" "without"
))

	(linter-types (sql-keywords-re
"bigint" "bitmap" "blob" "boolean" "char" "character" "date"
"datetime" "dec" "decimal" "double" "float" "int" "integer" "nchar"
"number" "numeric" "real" "smallint" "varbyte" "varchar" "byte"
"cursor" "long"
))

	(linter-functions (sql-keywords-re
"abs" "acos" "asin" "atan" "atan2" "avg" "ceil" "cos" "cosh" "divtime"
"exp" "floor" "getbits" "getblob" "getbyte" "getlong" "getraw"
"getstr" "gettext" "getword" "hextoraw" "lenblob" "length" "log"
"lower" "lpad" "ltrim" "max" "min" "mod" "monthname" "nvl"
"octet_length" "power" "rand" "rawtohex" "repeat_string"
"right_substr" "round" "rpad" "rtrim" "sign" "sin" "sinh" "soundex"
"sqrt" "sum" "tan" "tanh" "timeint_to_days" "to_char" "to_date"
"to_gmtime" "to_localtime" "to_number" "trim" "upper" "decode"
"substr" "substring" "chr" "dayname" "days" "greatest" "hex" "initcap"
"instr" "least" "multime" "replace" "width"
)))

    `((,linter-keywords  . font-lock-keyword-face)
      (,linter-reserved  . font-lock-keyword-face)
      (,linter-functions . font-lock-builtin-face)
      (,linter-types     . font-lock-type-face)))

  "Linter SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.")

(defvar sql-mode-ms-font-lock-keywords
  (let ((ms-reserved (sql-keywords-re
"absolute" "add" "all" "alter" "and" "any" "as" "asc" "authorization"
"avg" "backup" "begin" "between" "break" "browse" "bulk" "by"
"cascade" "case" "check" "checkpoint" "close" "clustered" "coalesce"
"column" "commit" "committed" "compute" "confirm" "constraint"
"contains" "containstable" "continue" "controlrow" "convert" "count"
"create" "cross" "current" "current_date" "current_time"
"current_timestamp" "current_user" "database" "deallocate" "declare"
"default" "delete" "deny" "desc" "disk" "distinct" "distributed"
"double" "drop" "dummy" "dump" "else" "end" "errlvl" "errorexit"
"escape" "except" "exec" "execute" "exists" "exit" "fetch" "file"
"fillfactor" "first" "floppy" "for" "foreign" "freetext"
"freetexttable" "from" "full" "goto" "grant" "group" "having"
"holdlock" "identity" "identity_insert" "identitycol" "if" "in"
"index" "inner" "insert" "intersect" "into" "is" "isolation" "join"
"key" "kill" "last" "left" "level" "like" "lineno" "load" "max" "min"
"mirrorexit" "national" "next" "nocheck" "nolock" "nonclustered" "not"
"null" "nullif" "of" "off" "offsets" "on" "once" "only" "open"
"opendatasource" "openquery" "openrowset" "option" "or" "order"
"outer" "output" "over" "paglock" "percent" "perm" "permanent" "pipe"
"plan" "precision" "prepare" "primary" "print" "prior" "privileges"
"proc" "procedure" "processexit" "public" "raiserror" "read"
"readcommitted" "readpast" "readtext" "readuncommitted" "reconfigure"
"references" "relative" "repeatable" "repeatableread" "replication"
"restore" "restrict" "return" "revoke" "right" "rollback" "rowcount"
"rowguidcol" "rowlock" "rule" "save" "schema" "select" "serializable"
"session_user" "set" "shutdown" "some" "statistics" "sum"
"system_user" "table" "tablock" "tablockx" "tape" "temp" "temporary"
"textsize" "then" "to" "top" "tran" "transaction" "trigger" "truncate"
"tsequal" "uncommitted" "union" "unique" "update" "updatetext"
"updlock" "use" "user" "values" "view" "waitfor" "when" "where"
"while" "with" "work" "writetext" "collate" "function" "openxml"
"returns"
))

	(ms-types (sql-keywords-re
"binary" "bit" "char" "character" "cursor" "datetime" "dec" "decimal"
"double" "float" "image" "int" "integer" "money" "national" "nchar"
"ntext" "numeric" "numeric" "nvarchar" "precision" "real"
"smalldatetime" "smallint" "smallmoney" "text" "timestamp" "tinyint"
"uniqueidentifier" "varbinary" "varchar" "varying"
))

	(ms-vars "\\b@[a-zA-Z0-9_]*\\b")

	(ms-functions (sql-keywords-re
"@@connections" "@@cpu_busy" "@@cursor_rows" "@@datefirst" "@@dbts"
"@@error" "@@fetch_status" "@@identity" "@@idle" "@@io_busy"
"@@langid" "@@language" "@@lock_timeout" "@@max_connections"
"@@max_precision" "@@nestlevel" "@@options" "@@pack_received"
"@@pack_sent" "@@packet_errors" "@@procid" "@@remserver" "@@rowcount"
"@@servername" "@@servicename" "@@spid" "@@textsize" "@@timeticks"
"@@total_errors" "@@total_read" "@@total_write" "@@trancount"
"@@version" "abs" "acos" "and" "app_name" "ascii" "asin" "atan" "atn2"
"avg" "case" "cast" "ceiling" "char" "charindex" "coalesce"
"col_length" "col_name" "columnproperty" "containstable" "convert"
"cos" "cot" "count" "current_timestamp" "current_user" "cursor_status"
"databaseproperty" "datalength" "dateadd" "datediff" "datename"
"datepart" "day" "db_id" "db_name" "degrees" "difference" "exp"
"file_id" "file_name" "filegroup_id" "filegroup_name"
"filegroupproperty" "fileproperty" "floor" "formatmessage"
"freetexttable" "fulltextcatalogproperty" "fulltextserviceproperty"
"getansinull" "getdate" "grouping" "host_id" "host_name" "ident_incr"
"ident_seed" "identity" "index_col" "indexproperty" "is_member"
"is_srvrolemember" "isdate" "isnull" "isnumeric" "left" "len" "log"
"log10" "lower" "ltrim" "max" "min" "month" "nchar" "newid" "nullif"
"object_id" "object_name" "objectproperty" "openquery" "openrowset"
"parsename" "patindex" "patindex" "permissions" "pi" "power"
"quotename" "radians" "rand" "replace" "replicate" "reverse" "right"
"round" "rtrim" "session_user" "sign" "sin" "soundex" "space" "sqrt"
"square" "stats_date" "stdev" "stdevp" "str" "stuff" "substring" "sum"
"suser_id" "suser_name" "suser_sid" "suser_sname" "system_user" "tan"
"textptr" "textvalid" "typeproperty" "unicode" "upper" "user"
"user_id" "user_name" "var" "varp" "year"
))

	(ms-commands
	 (eval-when-compile
	   (concat "^\\(\\(set\\s-+\\("
		   (regexp-opt '(
"datefirst" "dateformat" "deadlock_priority" "lock_timeout"
"concat_null_yields_null" "cursor_close_on_commit"
"disable_def_cnst_chk" "fips_flagger" "identity_insert" "language"
"offsets" "quoted_identifier" "arithabort" "arithignore" "fmtonly"
"nocount" "noexec" "numeric_roundabort" "parseonly"
"query_governor_cost_limit" "rowcount" "textsize" "ansi_defaults"
"ansi_null_dflt_off" "ansi_null_dflt_on" "ansi_nulls" "ansi_padding"
"ansi_warnings" "forceplan" "showplan_all" "showplan_text"
"statistics" "implicit_transactions" "remote_proc_transactions"
"transaction" "xact_abort"
) t)
		   "\\)\\)\\|go\\s-*\\|use\\s-+\\|setuser\\s-+\\|dbcc\\s-+\\).*$"))))

    `((,ms-commands  . font-lock-doc-face)
      (,ms-reserved  . font-lock-keyword-face)
      (,ms-functions . font-lock-builtin-face)
      (,ms-vars      . font-lock-variable-name-face)
      (,ms-types     . font-lock-type-face)))

  "Microsoft SQLServer SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-ms-font-lock-keywords.")

(defvar sql-mode-sybase-font-lock-keywords nil
  "Sybase SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-sybase-font-lock-keywords.")

(defvar sql-mode-informix-font-lock-keywords nil
  "Informix SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-informix-font-lock-keywords.")

(defvar sql-mode-interbase-font-lock-keywords nil
  "Interbase SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-interbase-font-lock-keywords.")

(defvar sql-mode-ingres-font-lock-keywords nil
  "Ingres SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-interbase-font-lock-keywords.")

(defvar sql-mode-solid-font-lock-keywords nil
  "Solid SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-solid-font-lock-keywords.")

(defvar sql-mode-mysql-font-lock-keywords
  (let ((mysql-funcs (sql-keywords-re
"ascii" "avg" "bdmpolyfromtext" "bdmpolyfromwkb" "bdpolyfromtext"
"bdpolyfromwkb" "benchmark" "bin" "bit_and" "bit_length" "bit_or"
"bit_xor" "both" "cast" "char_length" "character_length" "coalesce"
"concat" "concat_ws" "connection_id" "conv" "convert" "count"
"curdate" "current_date" "current_time" "current_timestamp" "curtime"
"elt" "encrypt" "export_set" "field" "find_in_set" "found_rows" "from"
"geomcollfromtext" "geomcollfromwkb" "geometrycollectionfromtext"
"geometrycollectionfromwkb" "geometryfromtext" "geometryfromwkb"
"geomfromtext" "geomfromwkb" "get_lock" "group_concat" "hex" "ifnull"
"instr" "interval" "isnull" "last_insert_id" "lcase" "leading"
"length" "linefromtext" "linefromwkb" "linestringfromtext"
"linestringfromwkb" "load_file" "locate" "lower" "lpad" "ltrim"
"make_set" "master_pos_wait" "max" "mid" "min" "mlinefromtext"
"mlinefromwkb" "mpointfromtext" "mpointfromwkb" "mpolyfromtext"
"mpolyfromwkb" "multilinestringfromtext" "multilinestringfromwkb"
"multipointfromtext" "multipointfromwkb" "multipolygonfromtext"
"multipolygonfromwkb" "now" "nullif" "oct" "octet_length" "ord"
"pointfromtext" "pointfromwkb" "polyfromtext" "polyfromwkb"
"polygonfromtext" "polygonfromwkb" "position" "quote" "rand"
"release_lock" "repeat" "replace" "reverse" "rpad" "rtrim" "soundex"
"space" "std" "stddev" "substring" "substring_index" "sum" "sysdate"
"trailing" "trim" "ucase" "unix_timestamp" "upper" "user" "variance"
))

	(mysql-keywords (sql-keywords-re
"action" "add" "after" "against" "all" "alter" "and" "as" "asc"
"auto_increment" "avg_row_length" "bdb" "between" "by" "cascade"
"case" "change" "character" "check" "checksum" "close" "collate"
"collation" "column" "columns" "comment" "committed" "concurrent"
"constraint" "create" "cross" "data" "database" "default"
"delay_key_write" "delayed" "delete" "desc" "directory" "disable"
"distinct" "distinctrow" "do" "drop" "dumpfile" "duplicate" "else"
"enable" "enclosed" "end" "escaped" "exists" "fields" "first" "for"
"force" "foreign" "from" "full" "fulltext" "global" "group" "handler"
"having" "heap" "high_priority" "if" "ignore" "in" "index" "infile"
"inner" "insert" "insert_method" "into" "is" "isam" "isolation" "join"
"key" "keys" "last" "left" "level" "like" "limit" "lines" "load"
"local" "lock" "low_priority" "match" "max_rows" "merge" "min_rows"
"mode" "modify" "mrg_myisam" "myisam" "natural" "next" "no" "not"
"null" "offset" "oj" "on" "open" "optionally" "or" "order" "outer"
"outfile" "pack_keys" "partial" "password" "prev" "primary"
"procedure" "quick" "raid0" "raid_type" "read" "references" "rename"
"repeatable" "restrict" "right" "rollback" "rollup" "row_format"
"savepoint" "select" "separator" "serializable" "session" "set"
"share" "show" "sql_big_result" "sql_buffer_result" "sql_cache"
"sql_calc_found_rows" "sql_no_cache" "sql_small_result" "starting"
"straight_join" "striped" "table" "tables" "temporary" "terminated"
"then" "to" "transaction" "truncate" "type" "uncommitted" "union"
"unique" "unlock" "update" "use" "using" "values" "when" "where"
"with" "write" "xor"
))

	(mysql-types (sql-keywords-re
"bigint" "binary" "bit" "blob" "bool" "boolean" "char" "curve" "date"
"datetime" "dec" "decimal" "double" "enum" "fixed" "float" "geometry"
"geometrycollection" "int" "integer" "line" "linearring" "linestring"
"longblob" "longtext" "mediumblob" "mediumint" "mediumtext"
"multicurve" "multilinestring" "multipoint" "multipolygon"
"multisurface" "national" "numeric" "point" "polygon" "precision"
"real" "smallint" "surface" "text" "time" "timestamp" "tinyblob"
"tinyint" "tinytext" "unsigned" "varchar" "year" "year2" "year4"
"zerofill"
)))

    `((,mysql-funcs    . font-lock-builtin-face)
      (,mysql-keywords . font-lock-keyword-face)
      (,mysql-types    . font-lock-type-face)))

  "MySQL SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-mysql-font-lock-keywords.")

(defvar sql-mode-sqlite-font-lock-keywords nil
  "SQLite SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-sqlite-font-lock-keywords.")

(defvar sql-mode-db2-font-lock-keywords nil
  "DB2 SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own sql-mode-db2-font-lock-keywords.")

(defvar sql-mode-font-lock-keywords nil
  "SQL keywords used by font-lock.

Setting this variable directly no longer has any affect.  Use
`sql-product' and `sql-add-product-keywords' to control the
highlighting rules in sql-mode.")



;;; SQL Product support functions

(defun sql-product-feature (feature &optional product)
  "Lookup `feature' needed to support the current SQL product.

See \[sql-product-alist] for a list of products and supported features."
  (plist-get
   (cdr (assoc (or product sql-product)
	       sql-product-alist))
   feature))

(defun sql-product-font-lock (keywords-only imenu)
  "Sets `font-lock-defaults' and `font-lock-keywords' based on
the product-specific keywords and syntax-alists defined in
`sql-product-alist'."
  (let
      ;; Get the product-specific syntax-alist.
      ((syntax-alist
	(append
	 (sql-product-feature :syntax-alist)
	 '((?_ . "w") (?. . "w")))))

    ;; Get the product-specific keywords.
    (setq sql-mode-font-lock-keywords
	  (append
	   (unless (eq sql-product 'ansi)
	     (eval (sql-product-feature :font-lock)))
	   ;; Always highlight ANSI keywords
	   (eval (sql-product-feature :font-lock 'ansi))
	   ;; Fontify object names in CREATE, DROP and ALTER DDL
	   ;; statements
	   (list sql-mode-font-lock-object-name)))

    ;; Setup font-lock.  Force re-parsing of `font-lock-defaults'.
    (set (make-local-variable 'font-lock-set-defaults) nil)
    (setq font-lock-defaults (list 'sql-mode-font-lock-keywords
				   keywords-only t syntax-alist))

    ;; Force font lock to reinitialize if it is already on
    ;; Otherwise, we can wait until it can be started.
    (when (and (fboundp 'font-lock-mode)
	       font-lock-mode)
      (font-lock-mode-internal nil)
      (font-lock-mode-internal t))

    (add-hook 'font-lock-mode-hook
	      (lambda ()
		;; Provide defaults for new font-lock faces.
		(defvar font-lock-builtin-face
		  (if (boundp 'font-lock-preprocessor-face)
		      font-lock-preprocessor-face
		    font-lock-keyword-face))
		(defvar font-lock-doc-face font-lock-string-face))
	      nil t)

    ;; Setup imenu; it needs the same syntax-alist.
    (when imenu
	(setq imenu-syntax-alist syntax-alist))))

;;;###autoload
(defun sql-add-product-keywords (product keywords &optional append)
  "Add highlighting KEYWORDS for SQL PRODUCT.

PRODUCT should be a symbol, the name of a sql product, such as
`oracle'.  KEYWORDS should be a list; see the variable
`font-lock-keywords'.  By default they are added at the beginning
of the current highlighting list.  If optional argument APPEND is
`set', they are used to replace the current highlighting list.
If APPEND is any other non-nil value, they are added at the end
of the current highlighting list.

For example:

 (sql-add-product-keywords 'ms
  '((\"\\\\b\\\\w+_t\\\\b\" . font-lock-type-face)))

adds a fontification pattern to fontify identifiers ending in
`_t' as data types."

  (let ((font-lock (sql-product-feature :font-lock product))
	old)
    (setq old (eval font-lock))
    (set font-lock
	 (if (eq append 'set)
	     keywords
	   (if append
	       (append old keywords)
	     (append keywords old))))))



;;; Functions to switch highlighting

(defun sql-highlight-product ()
  "Turns on the appropriate font highlighting for the SQL product
selected."

  (when (eq major-mode 'sql-mode)
    ;; Setup font-lock
    (sql-product-font-lock nil t)

    ;; Set the mode name to include the product.
    (setq mode-name (concat "SQL[" (prin1-to-string sql-product) "]"))))

(defun sql-set-product (product)
  "Set `sql-product' to product and enable appropriate
highlighting."
  (interactive "SEnter SQL product: ")
  (when (not (assoc product sql-product-alist))
    (error "SQL product %s is not supported; treated as ANSI" product)
    (setq product 'ansi))

  ;; Save product setting and fontify.
  (setq sql-product product)
  (sql-highlight-product))

(defun sql-highlight-oracle-keywords ()
  "Highlight Oracle keywords."
  (interactive)
  (sql-set-product 'oracle))

(defun sql-highlight-postgres-keywords ()
  "Highlight Postgres keywords."
  (interactive)
  (sql-set-product 'postgres))

(defun sql-highlight-linter-keywords ()
  "Highlight LINTER keywords."
  (interactive)
  (sql-set-product 'linter))

(defun sql-highlight-ms-keywords ()
  "Highlight Microsoft SQLServer keywords."
  (interactive)
  (sql-set-product 'ms))

(defun sql-highlight-ansi-keywords ()
  "Highlight ANSI SQL keywords."
  (interactive)
  (sql-set-product 'ansi))

(defun sql-highlight-sybase-keywords ()
  "Highlight Sybase SQL keywords."
  (interactive)
  (sql-set-product 'sybase))

(defun sql-highlight-informix-keywords ()
  "Highlight Informix SQL keywords."
  (interactive)
  (sql-set-product 'informix))

(defun sql-highlight-interbase-keywords ()
  "Highlight Interbase SQL keywords."
  (interactive)
  (sql-set-product 'interbase))

(defun sql-highlight-ingres-keywords ()
  "Highlight Ingres SQL keywords."
  (interactive)
  (sql-set-product 'ingres))

(defun sql-highlight-solid-keywords ()
  "Highlight Solid SQL keywords."
  (interactive)
  (sql-set-product 'solid))

(defun sql-highlight-mysql-keywords ()
  "Highlight MySQL SQL keywords."
  (interactive)
  (sql-set-product 'mysql))

(defun sql-highlight-sqlite-keywords ()
  "Highlight SQLite SQL keywords."
  (interactive)
  (sql-set-product 'sqlite))

(defun sql-highlight-db2-keywords ()
  "Highlight DB2 SQL keywords."
  (interactive)
  (sql-set-product 'db2))



;;; Compatibility functions

(if (not (fboundp 'comint-line-beginning-position))
    ;; comint-line-beginning-position is defined in Emacs 21
    (defun comint-line-beginning-position ()
      "Returns the buffer position of the beginning of the line, after any prompt.
The prompt is assumed to be any text at the beginning of the line matching
the regular expression `comint-prompt-regexp', a buffer local variable."
      (save-excursion (comint-bol nil) (point))))



;;; Small functions

(defun sql-magic-go (arg)
  "Insert \"o\" and call `comint-send-input'.
`sql-electric-stuff' must be the symbol `go'."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (and (equal sql-electric-stuff 'go)
	   (save-excursion
	     (comint-bol nil)
	     (looking-at "go\\b")))
      (comint-send-input)))

(defun sql-magic-semicolon (arg)
  "Insert semicolon and call `comint-send-input'.
`sql-electric-stuff' must be the symbol `semicolon'."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (equal sql-electric-stuff 'semicolon)
       (comint-send-input)))

(defun sql-accumulate-and-indent ()
  "Continue SQL statement on the next line."
  (interactive)
  (if (fboundp 'comint-accumulate)
      (comint-accumulate)
    (newline))
  (indent-according-to-mode))

;;;###autoload
(defun sql-help ()
  "Show short help for the SQL modes.

Use an entry function to open an interactive SQL buffer.  This buffer is
usually named `*SQL*'.  The name of the major mode is SQLi.

Use the following commands to start a specific SQL interpreter:

    PostGres: \\[sql-postgres]
    MySQL: \\[sql-mysql]
    SQLite: \\[sql-sqlite]

Other non-free SQL implementations are also supported:

    Solid: \\[sql-solid]
    Oracle: \\[sql-oracle]
    Informix: \\[sql-informix]
    Sybase: \\[sql-sybase]
    Ingres: \\[sql-ingres]
    Microsoft: \\[sql-ms]
    DB2: \\[sql-db2]
    Interbase: \\[sql-interbase]
    Linter: \\[sql-linter]

But we urge you to choose a free implementation instead of these.

Once you have the SQLi buffer, you can enter SQL statements in the
buffer.  The output generated is appended to the buffer and a new prompt
is generated.  See the In/Out menu in the SQLi buffer for some functions
that help you navigate through the buffer, the input history, etc.

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

The variables `sql-user', `sql-password', `sql-server', and
`sql-database' can be customized.  They are used as the default values.
Usernames, servers and databases are stored in `sql-user-history',
`sql-server-history' and `database-history'.  Passwords are not stored
in a history.

Parameter WHAT is a list of the arguments passed to this function.
The function asks for the username if WHAT contains symbol `user', for
the password if it contains symbol `password', for the server if it
contains symbol `server', and for the database if it contains symbol
`database'.  The members of WHAT are processed in the order in which
they are provided.

In order to ask the user for username, password and database, call the
function like this: (sql-get-login 'user 'password 'database)."
  (interactive)
  (while what
    (cond
     ((eq (car what) 'user)		; user
      (setq sql-user
	    (read-from-minibuffer "User: " sql-user nil nil
				  sql-user-history)))
     ((eq (car what) 'password)		; password
      (setq sql-password
	    (sql-read-passwd "Password: " sql-password)))
     ((eq (car what) 'server)		; server
      (setq sql-server
	    (read-from-minibuffer "Server: " sql-server nil nil
				  sql-server-history)))
     ((eq (car what) 'database)		; database
      (setq sql-database
	    (read-from-minibuffer "Database: " sql-database nil nil
				  sql-database-history))))
    (setq what (cdr what))))

(defun sql-find-sqli-buffer ()
  "Return the current default SQLi buffer or nil.
In order to qualify, the SQLi buffer must be alive,
be in `sql-interactive-mode' and have a process."
  (let ((default-buffer (default-value 'sql-buffer)))
    (if (and (buffer-live-p default-buffer)
	     (get-buffer-process default-buffer))
	default-buffer
      (save-excursion
	(let ((buflist (buffer-list))
	      (found))
	  (while (not (or (null buflist)
			  found))
	    (let ((candidate (car buflist)))
	      (set-buffer candidate)
	      (if (and (equal major-mode 'sql-interactive-mode)
		       (get-buffer-process candidate))
		  (setq found candidate))
	      (setq buflist (cdr buflist))))
	  found)))))

(defun sql-set-sqli-buffer-generally ()
  "Set SQLi buffer for all SQL buffers that have none.
This function checks all SQL buffers for their SQLi buffer.  If their
SQLi buffer is nonexistent or has no process, it is set to the current
default SQLi buffer.  The current default SQLi buffer is determined
using `sql-find-sqli-buffer'.  If `sql-buffer' is set,
`sql-set-sqli-hook' is run."
  (interactive)
  (save-excursion
    (let ((buflist (buffer-list))
	  (default-sqli-buffer (sql-find-sqli-buffer)))
      (setq-default sql-buffer default-sqli-buffer)
      (while (not (null buflist))
	(let ((candidate (car buflist)))
	  (set-buffer candidate)
	  (if (and (equal major-mode 'sql-mode)
		   (not (buffer-live-p sql-buffer)))
	      (progn
		(setq sql-buffer default-sqli-buffer)
		(run-hooks 'sql-set-sqli-hook))))
	(setq buflist (cdr buflist))))))

(defun sql-set-sqli-buffer ()
  "Set the SQLi buffer SQL strings are sent to.

Call this function in a SQL buffer in order to set the SQLi buffer SQL
strings are sent to.  Calling this function sets `sql-buffer' and runs
`sql-set-sqli-hook'.

If you call it from a SQL buffer, this sets the local copy of
`sql-buffer'.

If you call it from anywhere else, it sets the global copy of
`sql-buffer'."
  (interactive)
  (let ((default-buffer (sql-find-sqli-buffer)))
    (if (null default-buffer)
	(error "There is no suitable SQLi buffer"))
    (let ((new-buffer
	   (get-buffer
	    (read-buffer "New SQLi buffer: " default-buffer t))))
      (if (null (get-buffer-process new-buffer))
	  (error "Buffer %s has no process" (buffer-name new-buffer)))
      (if (null (save-excursion
		  (set-buffer new-buffer)
		  (equal major-mode 'sql-interactive-mode)))
	  (error "Buffer %s is no SQLi buffer" (buffer-name new-buffer)))
      (if new-buffer
	  (progn
	    (setq sql-buffer new-buffer)
	    (run-hooks 'sql-set-sqli-hook))))))

(defun sql-show-sqli-buffer ()
  "Show the name of current SQLi buffer.

This is the buffer SQL strings are sent to.  It is stored in the
variable `sql-buffer'.  See `sql-help' on how to create such a buffer."
  (interactive)
  (if (null (buffer-live-p sql-buffer))
      (message "%s has no SQLi buffer set." (buffer-name (current-buffer)))
    (if (null (get-buffer-process sql-buffer))
	(message "Buffer %s has no process." (buffer-name sql-buffer))
      (message "Current SQLi buffer is %s." (buffer-name sql-buffer)))))

(defun sql-make-alternate-buffer-name ()
  "Return a string that can be used to rename a SQLi buffer.

This is used to set `sql-alternate-buffer-name' within
`sql-interactive-mode'."
  (concat (if (string= "" sql-user)
	      (if (string= "" (user-login-name))
		  ()
		(concat (user-login-name) "/"))
	    (concat sql-user "/"))
	  (if (string= "" sql-database)
	      (if (string= "" sql-server)
		  (system-name)
		sql-server)
	    sql-database)))

(defun sql-rename-buffer ()
  "Renames a SQLi buffer."
  (interactive)
  (rename-buffer (format "*SQL: %s*" sql-alternate-buffer-name) t))

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
      (let ((bol (comint-line-beginning-position)))
	(cond
	 ;; if empty command line, insert SELECT
	 ((= bol (point))
	  (insert "SELECT "))
	 ;; else if appending to INTO .* (, SELECT or ORDER BY, insert a comma
	 ((save-excursion
	    (re-search-backward "\\b\\(\\(into\\s-+\\S-+\\s-+(\\)\\|select\\|order by\\) .+"
				bol t))
	  (insert ", "))
	 ;; else insert a space
	 (t
	  (if (eq (preceding-char) ?\s)
	      nil
	    (insert " ")))))
      ;; in any case, insert the column
      (insert column)
      (message "%s" column))))

;; On NT, SQL*Plus for Oracle turns on full buffering for stdout if it
;; is not attached to a character device; therefore placeholder
;; replacement by SQL*Plus is fully buffered.  The workaround lets
;; Emacs query for the placeholders.

(defvar sql-placeholder-history nil
  "History of placeholder values used.")

(defun sql-query-placeholders-and-send (proc string)
  "Send to PROC input STRING, maybe replacing placeholders.
Placeholders are words starting with and ampersand like &this.
This function is used for `comint-input-sender' if using `sql-oracle' on NT."
  (while (string-match "&\\(\\sw+\\)" string)
    (setq string (replace-match
		  (read-from-minibuffer
		   (format "Enter value for %s: " (match-string 1 string))
		   nil nil nil sql-placeholder-history)
		  t t string)))
  (comint-send-string proc string)
  (if comint-input-sender-no-newline
      (if (not (string-equal string ""))
	  (process-send-eof))
    (comint-send-string proc "\n")))

;; Using DB2 interactively, newlines must be escaped with " \".
;; The space before the backslash is relevant.
(defun sql-escape-newlines-and-send (proc string)
  "Send to PROC input STRING, escaping newlines if necessary.
Every newline in STRING will be preceded with a space and a backslash."
  (let ((result "") (start 0) mb me)
    (while (string-match "\n" string start)
      (setq mb (match-beginning 0)
	    me (match-end 0))
      (if (and (> mb 1)
	       (string-equal " \\" (substring string (- mb 2) mb)))
	  (setq result (concat result (substring string start me)))
	(setq result (concat result (substring string start mb) " \\\n")))
      (setq start me))
    (setq result (concat result (substring string start)))
    (comint-send-string proc result)
    (if comint-input-sender-no-newline
	(if (not (string-equal string ""))
	    (process-send-eof))
      (comint-send-string proc "\n"))))



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
	(message "Sent string to buffer %s." (buffer-name sql-buffer))
	(if sql-pop-to-buffer-after-send-region
	    (pop-to-buffer sql-buffer)
	  (display-buffer sql-buffer)))
    (message "No SQL process started.")))

(defun sql-send-paragraph ()
  "Send the current paragraph to the SQL process."
  (interactive)
  (let ((start (save-excursion
		 (backward-paragraph)
		 (point)))
	(end (save-excursion
	       (forward-paragraph)
	       (point))))
    (sql-send-region start end)))

(defun sql-send-buffer ()
  "Send the buffer contents to the SQL process."
  (interactive)
  (sql-send-region (point-min) (point-max)))

(defun sql-send-string (str)
  "Send a string to the SQL process."
  (interactive "sSQL Text: ")
  (if (buffer-live-p sql-buffer)
      (save-excursion
        (comint-send-string sql-buffer str)
        (comint-send-string sql-buffer "\n")
        (message "Sent string to buffer %s." (buffer-name sql-buffer))
        (if sql-pop-to-buffer-after-send-region
            (pop-to-buffer sql-buffer)
          (display-buffer sql-buffer)))
    (message "No SQL process started.")))

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

You can send SQL statements to the SQLi buffer using
\\[sql-send-region].  Such a buffer must exist before you can do this.
See `sql-help' on how to create SQLi buffers.

\\{sql-mode-map}
Customization: Entry to this mode runs the `sql-mode-hook'.

When you put a buffer in SQL mode, the buffer stores the last SQLi
buffer created as its destination in the variable `sql-buffer'.  This
will be the buffer \\[sql-send-region] sends the region to.  If this
SQLi buffer is killed, \\[sql-send-region] is no longer able to
determine where the strings should be sent to.  You can set the
value of `sql-buffer' using \\[sql-set-sqli-buffer].

For information on how to create multiple SQLi buffers, see
`sql-interactive-mode'.

Note that SQL doesn't have an escape character unless you specify
one.  If you specify backslash as escape character in SQL,
you must tell Emacs.  Here's how to do that in your `~/.emacs' file:

\(add-hook 'sql-mode-hook
          (lambda ()
	    (modify-syntax-entry ?\\\\ \".\" sql-mode-syntax-table)))"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sql-mode)
  (setq mode-name "SQL")
  (use-local-map sql-mode-map)
  (if sql-mode-menu
      (easy-menu-add sql-mode-menu)); XEmacs
  (set-syntax-table sql-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'sql-mode-font-lock-keywords)
  (make-local-variable 'comment-start)
  (setq comment-start "--")
  ;; Make each buffer in sql-mode remember the "current" SQLi buffer.
  (make-local-variable 'sql-buffer)
  ;; Add imenu support for sql-mode.  Note that imenu-generic-expression
  ;; is buffer-local, so we don't need a local-variable for it.  SQL is
  ;; case-insensitive, that's why we have to set imenu-case-fold-search.
  (setq imenu-generic-expression sql-imenu-generic-expression
	imenu-case-fold-search t)
  ;; Make `sql-send-paragraph' work on paragraphs that contain indented
  ;; lines.
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-separate "[\f]*$"
	paragraph-start "[\n\f]")
  ;; Abbrevs
  (setq local-abbrev-table sql-mode-abbrev-table)
  (setq abbrev-all-caps 1)
  ;; Run hook
  (run-mode-hooks 'sql-mode-hook)
  ;; Catch changes to sql-product and highlight accordingly
  (sql-highlight-product)
  (add-hook 'hack-local-variables-hook 'sql-highlight-product t t))



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
See `sql-help' for a list of available entry functions.  The last buffer
created by such an entry function is the current SQLi buffer.  SQL
buffers will send strings to the SQLi buffer current at the time of
their creation.  See `sql-mode' for details.

Sample session using two connections:

1. Create first SQLi buffer by calling an entry function.
2. Rename buffer \"*SQL*\" to \"*Connection 1*\".
3. Create a SQL buffer \"test1.sql\".
4. Create second SQLi buffer by calling an entry function.
5. Rename buffer \"*SQL*\" to \"*Connection 2*\".
6. Create a SQL buffer \"test2.sql\".

Now \\[sql-send-region] in buffer \"test1.sql\" will send the region to
buffer \"*Connection 1*\", \\[sql-send-region] in buffer \"test2.sql\"
will send the region to buffer \"*Connection 2*\".

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.  On some operating systems, this will not work because
the signals are not supported.

\\{sql-interactive-mode-map}
Customization: Entry to this mode runs the hooks on `comint-mode-hook'
and `sql-interactive-mode-hook' (in that order).  Before each input, the
hooks on `comint-input-filter-functions' are run.  After each SQL
interpreter output, the hooks on `comint-output-filter-functions' are
run.

Variable `sql-input-ring-file-name' controls the initialisation of the
input ring history.

Variables `comint-output-filter-functions', a hook, and
`comint-scroll-to-bottom-on-input' and
`comint-scroll-to-bottom-on-output' control whether input and output
cause the window to scroll to the end of the buffer.

If you want to make SQL buffers limited in length, add the function
`comint-truncate-buffer' to `comint-output-filter-functions'.

Here is an example for your .emacs file.  It keeps the SQLi buffer a
certain length.

\(add-hook 'sql-interactive-mode-hook
    \(function (lambda ()
        \(setq comint-output-filter-functions 'comint-truncate-buffer))))

Here is another example.  It will always put point back to the statement
you entered, right above the output it created.

\(setq comint-output-filter-functions
       \(function (lambda (STR) (comint-show-output))))"
  (delay-mode-hooks (comint-mode))
  ;; Get the `sql-product' for this interactive session.
  (set (make-local-variable 'sql-product)
       (or sql-interactive-product
	   sql-product))
  ;; Setup the mode.
  (setq major-mode 'sql-interactive-mode)
  (setq mode-name (concat "SQLi[" (prin1-to-string sql-product) "]"))
  (use-local-map sql-interactive-mode-map)
  (if sql-interactive-mode-menu
      (easy-menu-add sql-interactive-mode-menu)) ; XEmacs
  (set-syntax-table sql-mode-syntax-table)
  (make-local-variable 'sql-mode-font-lock-keywords)
  (make-local-variable 'font-lock-defaults)
  ;; Note that making KEYWORDS-ONLY nil will cause havoc if you try
  ;; SELECT 'x' FROM DUAL with SQL*Plus, because the title of the column
  ;; will have just one quote.  Therefore syntactic hilighting is
  ;; disabled for interactive buffers.  No imenu support.
  (sql-product-font-lock t nil)
  ;; Enable commenting and uncommenting of the region.
  (make-local-variable 'comment-start)
  (setq comment-start "--")
  ;; Abbreviation table init and case-insensitive.  It is not activated
  ;; by default.
  (setq local-abbrev-table sql-mode-abbrev-table)
  (setq abbrev-all-caps 1)
  ;; Exiting the process will call sql-stop.
  (set-process-sentinel (get-buffer-process sql-buffer) 'sql-stop)
  ;; Create a usefull name for renaming this buffer later.
  (make-local-variable 'sql-alternate-buffer-name)
  (setq sql-alternate-buffer-name (sql-make-alternate-buffer-name))
  ;; User stuff.  Initialize before the hook.
  (set (make-local-variable 'sql-prompt-regexp)
       (sql-product-feature :sqli-prompt-regexp))
  (set (make-local-variable 'sql-prompt-length)
       (sql-product-feature :sqli-prompt-length))
  (make-local-variable 'sql-input-ring-separator)
  (make-local-variable 'sql-input-ring-file-name)
  ;; Run hook.
  (run-mode-hooks 'sql-interactive-mode-hook)
  ;; Set comint based on user overrides.
  (setq comint-prompt-regexp sql-prompt-regexp)
  (setq left-margin sql-prompt-length)
  ;; People wanting a different history file for each
  ;; buffer/process/client/whatever can change separator and file-name
  ;; on the sql-interactive-mode-hook.
  (setq comint-input-ring-separator sql-input-ring-separator
	comint-input-ring-file-name sql-input-ring-file-name)
  ;; Calling the hook before calling comint-read-input-ring allows users
  ;; to set comint-input-ring-file-name in sql-interactive-mode-hook.
  (comint-read-input-ring t))

(defun sql-stop (process event)
  "Called when the SQL process is stopped.

Writes the input history to a history file using
`comint-write-input-ring' and inserts a short message in the SQL buffer.

This function is a sentinel watching the SQL interpreter process.
Sentinels will always get the two parameters PROCESS and EVENT."
  (comint-write-input-ring)
  (if (and (eq (current-buffer) sql-buffer)
	   (not buffer-read-only))
      (insert (format "\nProcess %s %s\n" process event))
    (message "Process %s %s" process event)))



;;; Entry functions for different SQL interpreters.

;;;###autoload
(defun sql-product-interactive (&optional product)
  "Run product interpreter as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (setq product (or product sql-product))
  (when (sql-product-feature :sqli-connect product)
    (if (comint-check-proc "*SQL*")
	(pop-to-buffer "*SQL*")
      ;; Get credentials.
      (apply 'sql-get-login (sql-product-feature :sqli-login product))
      ;; Connect to database.
      (message "Login...")
      (funcall (sql-product-feature :sqli-connect product))
      ;; Set SQLi mode.
      (setq sql-interactive-product product)
      (setq sql-buffer (current-buffer))
      (sql-interactive-mode)
      ;; All done.
      (message "Login...done")
      (pop-to-buffer sql-buffer))))

;;;###autoload
(defun sql-oracle ()
  "Run sqlplus by Oracle as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-oracle-program'.  Login uses
the variables `sql-user', `sql-password', and `sql-database' as
defaults, if set.  Additional command line parameters can be stored in
the list `sql-oracle-options'.

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
  (sql-product-interactive 'oracle))

(defun sql-connect-oracle ()
  "Create comint buffer and connect to Oracle using the login
parameters and command options."
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
	(setq parameter (nconc (list parameter) sql-oracle-options))
      (setq parameter sql-oracle-options))
    (if parameter
	(set-buffer (apply 'make-comint "SQL" sql-oracle-program nil
			   parameter))
      (set-buffer (make-comint "SQL" sql-oracle-program nil)))
    ;; SQL*Plus is buffered on WindowsNT; this handles &placeholders.
    (if (eq window-system 'w32)
	(setq comint-input-sender 'sql-query-placeholders-and-send))))



;;;###autoload
(defun sql-sybase ()
  "Run isql by SyBase as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-sybase-program'.  Login uses
the variables `sql-server', `sql-user', `sql-password', and
`sql-database' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-sybase-options'.

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
  (sql-product-interactive 'sybase))

(defun sql-connect-sybase ()
  "Create comint buffer and connect to Sybase using the login
parameters and command options."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params sql-sybase-options))
    (if (not (string= "" sql-server))
	(setq params (append (list "-S" sql-server) params)))
    (if (not (string= "" sql-database))
	(setq params (append (list "-D" sql-database) params)))
    (if (not (string= "" sql-password))
	(setq params (append (list "-P" sql-password) params)))
    (if (not (string= "" sql-user))
	(setq params (append (list "-U" sql-user) params)))
    (set-buffer (apply 'make-comint "SQL" sql-sybase-program
		       nil params))))



;;;###autoload
(defun sql-informix ()
  "Run dbaccess by Informix as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
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
  (sql-product-interactive 'informix))

(defun sql-connect-informix ()
  "Create comint buffer and connect to Informix using the login
parameters and command options."
  ;; username and password are ignored.
  (if (string= "" sql-database)
      (set-buffer (make-comint "SQL" sql-informix-program nil))
    (set-buffer (make-comint "SQL" sql-informix-program nil sql-database "-"))))



;;;###autoload
(defun sql-sqlite ()
  "Run sqlite as an inferior process.

SQLite is free software.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-sqlite-program'.  Login uses
the variables `sql-user', `sql-password', `sql-database', and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-sqlite-options'.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-sqlite].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (sql-product-interactive 'sqlite))

(defun sql-connect-sqlite ()
  "Create comint buffer and connect to SQLite using the login
parameters and command options."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params))
    (if (not (string= "" sql-database))
	(setq params (append (list sql-database) params)))
    (if (not (string= "" sql-server))
	(setq params (append (list (concat "--host=" sql-server)) params)))
    (if (not (string= "" sql-password))
	(setq params (append (list (concat "--password=" sql-password)) params)))
    (if (not (string= "" sql-user))
	(setq params (append (list (concat "--user=" sql-user)) params)))
    (if (not (null sql-sqlite-options))
	(setq params (append sql-sqlite-options params)))
    (set-buffer (apply 'make-comint "SQL" sql-sqlite-program
		       nil params))))



;;;###autoload
(defun sql-mysql ()
  "Run mysql by TcX as an inferior process.

Mysql versions 3.23 and up are free software.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-mysql-program'.  Login uses
the variables `sql-user', `sql-password', `sql-database', and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-mysql-options'.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-mysql].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (sql-product-interactive 'mysql))

(defun sql-connect-mysql ()
  "Create comint buffer and connect to MySQL using the login
parameters and command options."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params))
    (if (not (string= "" sql-database))
	(setq params (append (list sql-database) params)))
    (if (not (string= "" sql-server))
	(setq params (append (list (concat "--host=" sql-server)) params)))
    (if (not (string= "" sql-password))
	(setq params (append (list (concat "--password=" sql-password)) params)))
    (if (not (string= "" sql-user))
	(setq params (append (list (concat "--user=" sql-user)) params)))
    (if (not (null sql-mysql-options))
	(setq params (append sql-mysql-options params)))
    (set-buffer (apply 'make-comint "SQL" sql-mysql-program
		       nil params))))



;;;###autoload
(defun sql-solid ()
  "Run solsql by Solid as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-solid-program'.  Login uses
the variables `sql-user', `sql-password', and `sql-server' as
defaults, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-solid].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (sql-product-interactive 'solid))

(defun sql-connect-solid ()
  "Create comint buffer and connect to Solid using the login
parameters and command options."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params))
    ;; It only makes sense if both username and password are there.
    (if (not (or (string= "" sql-user)
		 (string= "" sql-password)))
	(setq params (append (list sql-user sql-password) params)))
    (if (not (string= "" sql-server))
	(setq params (append (list sql-server) params)))
    (set-buffer (apply 'make-comint "SQL" sql-solid-program
		       nil params))))



;;;###autoload
(defun sql-ingres ()
  "Run sql by Ingres as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
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
  (sql-product-interactive 'ingres))

(defun sql-connect-ingres ()
  "Create comint buffer and connect to Ingres using the login
parameters and command options."
  ;; username and password are ignored.
  (if (string= "" sql-database)
      (set-buffer (make-comint "SQL" sql-ingres-program nil))
    (set-buffer (make-comint "SQL" sql-ingres-program nil sql-database))))



;;;###autoload
(defun sql-ms ()
  "Run osql by Microsoft as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-ms-program'.  Login uses the
variables `sql-user', `sql-password', `sql-database', and `sql-server'
as defaults, if set.  Additional command line parameters can be stored
in the list `sql-ms-options'.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-ms].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (sql-product-interactive 'ms))

(defun sql-connect-ms ()
  "Create comint buffer and connect to Microsoft using the login
parameters and command options."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params sql-ms-options))
    (if (not (string= "" sql-server))
        (setq params (append (list "-S" sql-server) params)))
    (if (not (string= "" sql-database))
        (setq params (append (list "-d" sql-database) params)))
    (if (not (string= "" sql-user))
	(setq params (append (list "-U" sql-user) params)))
    (if (not (string= "" sql-password))
	(setq params (append (list "-P" sql-password) params))
      (if (string= "" sql-user)
	  ;; if neither user nor password is provided, use system
	  ;; credentials.
	  (setq params (append (list "-E") params))
	;; If -P is passed to ISQL as the last argument without a
	;; password, it's considered null.
	(setq params (append params (list "-P")))))
    (set-buffer (apply 'make-comint "SQL" sql-ms-program
		       nil params))))



;;;###autoload
(defun sql-postgres ()
  "Run psql by Postgres as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-postgres-program'.  Login uses
the variables `sql-database' and `sql-server' as default, if set.
Additional command line parameters can be stored in the list
`sql-postgres-options'.

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
  (sql-product-interactive 'postgres))

(defun sql-connect-postgres ()
  "Create comint buffer and connect to Postgres using the login
parameters and command options."
  ;; username and password are ignored.  Mark Stosberg suggest to add
  ;; the database at the end.  Jason Beegan suggest using --pset and
  ;; pager=off instead of \\o|cat.  The later was the solution by
  ;; Gregor Zych.  Jason's suggestion is the default value for
  ;; sql-postgres-options.
  (let ((params sql-postgres-options))
    (if (not (string= "" sql-database))
	(setq params (append params (list sql-database))))
    (if (not (string= "" sql-server))
	(setq params (append (list "-h" sql-server) params)))
    (if (not (string= "" sql-user))
	(setq params (append (list "-U" sql-user) params)))
    (set-buffer (apply 'make-comint "SQL" sql-postgres-program
		       nil params))))



;;;###autoload
(defun sql-interbase ()
  "Run isql by Interbase as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-interbase-program'.  Login
uses the variables `sql-user', `sql-password', and `sql-database' as
defaults, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-interbase].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (sql-product-interactive 'interbase))

(defun sql-connect-interbase ()
  "Create comint buffer and connect to Interbase using the login
parameters and command options."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params sql-interbase-options))
    (if (not (string= "" sql-user))
	(setq params (append (list "-u" sql-user) params)))
    (if (not (string= "" sql-password))
	(setq params (append (list "-p" sql-password) params)))
    (if (not (string= "" sql-database))
        (setq params (cons sql-database params))) ; add to the front!
    (set-buffer (apply 'make-comint "SQL" sql-interbase-program
		       nil params))))



;;;###autoload
(defun sql-db2 ()
  "Run db2 by IBM as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-db2-program'.  There is not
automatic login.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

If you use \\[sql-accumulate-and-indent] to send multiline commands to
db2, newlines will be escaped if necessary.  If you don't want that, set
`comint-input-sender' back to `comint-simple-send' by writing an after
advice.  See the elisp manual for more information.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-db2].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (sql-product-interactive 'db2))

(defun sql-connect-db2 ()
  "Create comint buffer and connect to DB2 using the login
parameters and command options."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (set-buffer (apply 'make-comint "SQL" sql-db2-program
		     nil sql-db2-options))
  ;; Properly escape newlines when DB2 is interactive.
  (setq comint-input-sender 'sql-escape-newlines-and-send))

;;;###autoload
(defun sql-linter ()
  "Run inl by RELEX as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-linter-program' - usually `inl'.
Login uses the variables `sql-user', `sql-password', `sql-database' and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-linter-options'. Run inl -h to get help on
parameters.

`sql-database' is used to set the LINTER_MBX environment variable for
local connections, `sql-server' refers to the server name from the
`nodetab' file for the network connection (dbc_tcp or friends must run
for this to work).  If `sql-password' is an empty string, inl will use
an empty password.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive)
  (sql-product-interactive 'linter))

(defun sql-connect-linter ()
  "Create comint buffer and connect to Linter using the login
parameters and command options."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params sql-linter-options) (login nil) (old-mbx (getenv "LINTER_MBX")))
    (if (not (string= "" sql-user))
	(setq login (concat sql-user "/" sql-password)))
    (setq params (append (list "-u" login) params))
    (if (not (string= "" sql-server))
	(setq params (append (list "-n" sql-server) params)))
    (if (string= "" sql-database)
	(setenv "LINTER_MBX" nil)
      (setenv "LINTER_MBX" sql-database))
    (set-buffer (apply 'make-comint "SQL" sql-linter-program nil
		       params))
    (setenv "LINTER_MBX" old-mbx)))



(provide 'sql)

;;; arch-tag: 7e1fa1c4-9ca2-402e-87d2-83a5eccb7ac3
;;; sql.el ends here
