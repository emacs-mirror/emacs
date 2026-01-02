;;; php-ts-mode.el --- Major mode for editing PHP files using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author: Vincenzo Pupillo <v.pupillo@gmail.com>
;; Maintainer: Vincenzo Pupillo <v.pupillo@gmail.com>
;; Created: Jun 2024
;; Keywords: PHP language tree-sitter

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

;;; Tree-sitter language versions
;;
;; php-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-phpdoc: v0.1.6
;; - tree-sitter-css: v0.23.1-1-g6a442a3
;; - tree-sitter-jsdoc: v0.23.2
;; - tree-sitter-javascript: v0.23.1-2-g108b2d4
;; - tree-sitter-html: v0.23.2-1-gd9219ad
;; - tree-sitter-php: v0.24.2
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

;;; Commentary:
;;
;; This package provides `php-ts-mode' which is a major mode
;; for editing PHP files with embedded HTML, JavaScript, CSS and phpdoc.
;; Tree Sitter is used to parse each of these languages.
;;
;; Please note that this package requires `mhtml-ts-mode', which
;; registers itself as the major mode for editing HTML.
;;
;; This package is compatible and has been tested with the following
;; tree-sitter grammars:
;; * https://github.com/tree-sitter/tree-sitter-php
;; * https://github.com/tree-sitter/tree-sitter-html
;; * https://github.com/tree-sitter/tree-sitter-javascript
;; * https://github.com/tree-sitter/tree-sitter-jsdoc
;; * https://github.com/tree-sitter/tree-sitter-css
;; * https://github.com/claytonrcarter/tree-sitter-phpdoc
;;
;; Features
;;
;; * Indent
;; * IMenu
;; * Navigation
;; * Which-function
;; * Flymake
;; * Tree-sitter parser installation helper
;; * PHP built-in server support
;; * Shell interaction: execute PHP code in a inferior PHP process

;;; Code:

(require 'treesit)
(require 'c-ts-common) ;; For comment indent and filling.
(require 'mhtml-ts-mode) ;; For embed html, css and js.
(require 'comint)
(treesit-declare-unavailable-functions)

(eval-when-compile
  (require 'cl-lib)
  (require 'rx)
  (require 'subr-x))

;;; Install treesitter language parsers
(defvar php-ts-mode--language-source-alist
  `((php "https://github.com/tree-sitter/tree-sitter-php"
	 :commit ,(if (and (treesit-available-p)
                           (< (treesit-library-abi-version) 15))
                      "f7cf7348737d8cff1b13407a0bfedce02ee7b046"
                    "5b5627faaa290d89eb3d01b9bf47c3bb9e797dea")
	 :source-dir "php/src")
    (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc"
	    :commit "03bb10330704b0b371b044e937d5cc7cd40b4999"))
  "Treesitter language parsers required by `php-ts-mode'.
You can customize `treesit-language-source-alist' if you want
to stick to a specific commit and/or use different parsers.")

(dolist (item php-ts-mode--language-source-alist)
  (add-to-list 'treesit-language-source-alist item t))

(defun php-ts-mode-install-parsers ()
  "Install all the required treesitter parsers.
`treesit-language-source-alist' defines which parsers to install.
It's pre-filled by loading \"mhtml-ts-mode\"."
  (interactive)
  (mhtml-ts-mode-install-parsers)
  (dolist (lang '(php phpdoc))
    (treesit-install-language-grammar lang)))

;;; Custom variables

(defgroup php-ts-mode nil
  "Major mode for editing PHP files."
  :prefix "php-ts-mode-"
  :group 'languages)

(defcustom php-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `php-ts-mode'."
  :tag "PHP indent offset"
  :version "30.1"
  :type 'integer
  :safe 'integerp)

(defcustom php-ts-mode-js-css-indent-offset 2
  "JavaScript and CSS indent spaces related to the <script> and <style> HTML tags.
It is advisable to have this have the same value as
`mhtml-ts-mode-js-css-indent-offset'."
  :tag "PHP javascript or css indent offset"
  :version "30.1"
  :type 'integer
  :safe 'integerp)

(defcustom php-ts-mode-html-indent-offset 2
  "The number of spaces to indent PHP code relative to HTML tags.
It is advisable to have this have the same value as
`mhtml-ts-mode-js-css-indent-offset'."
  :tag "PHP indent offset relative to HTML"
  :version "31.1"
  :type 'integer
  :safe 'integerp)

(defcustom php-ts-mode-html-relative-indent t
  "How PHP code is indented relative to the HTML tags.

When t, the default, indentation looks like:

  <html>
    ...
    <div>
       <?php
       ?>
    </div>

When nil, indentation of the tag body starts just below the
tag, like:

  <html>
    ...
    <div>
    <?php
    ?>
    </div>

When `ignore', the tag body starts in the first column, like:

  <html>
    ...
    <div>
  <?php
  ?>
    </div>"
  :type '(choice
          (const :tag "Indent starts just below the HTML tag" nil)
          (const :tag "Indent relative to the HTML tag" t)
          (const :tag "Always in the first column" ignore))
  :safe 'symbolp
  :tag "PHP indent relative to HTML"
  :version "31.1")

(defcustom php-ts-mode-php-default-executable (or (executable-find "php") "php")
  "The default file name of the PHP executable."
  :tag "PHP executable"
  :version "30.1"
  :type 'file)

(defvar-local php-ts-mode-alternative-php-program-name nil
  "An alternative to the usual `php' program name.
If non-nil, `php-ts-mode--executable' looks for this instead of \"php\".")

(defcustom php-ts-mode-php-config nil
  "The location of php.ini file.
If nil the default one is used to run the embedded webserver or
inferior PHP process."
  :tag "PHP init file"
  :version "30.1"
  :type '(choice (const :tag "Default init file" nil) file))

(defcustom php-ts-mode-ws-hostname "localhost"
  "The hostname that will be served by the PHP built-in webserver.
If nil then `php-ts-mode-run-php-webserver' will ask you for the hostname.
See `https://www.php.net/manual/en/features.commandline.webserver.php'."
  :tag "PHP built-in web server hostname"
  :version "30.1"
  :type 'string
  :safe 'stringp)

(defcustom php-ts-mode-ws-port nil
  "The port on which the PHP built-in webserver will listen.
If nil `php-ts-mode-run-php-webserver' will ask you for the port number."
  :tag "PHP built-in web server port"
  :version "30.1"
  :type '(choice (const :tag "Ask which port to use" nil) integer)
  :safe 'integer-or-null-p)

(defcustom php-ts-mode-ws-document-root nil
  "The root of the documents that the PHP built-in webserver will serve.
If nil `php-ts-mode-run-php-webserver' will ask you for the document root."
  :tag "PHP built-in web server document root"
  :version "30.1"
  :type '(choice (const :tag "Ask for document root" nil) directory))

(defcustom php-ts-mode-ws-workers nil
  "The number of workers the PHP built-in webserver will fork.
Useful for testing code against multiple simultaneous requests."
  :tag "PHP built-in number of workers"
  :version "30.1"
  :type '(choice (const :tag "No workers" nil) integer)
  :safe 'integer-or-null-p)

(defcustom php-ts-mode-inferior-php-buffer "*PHP*"
  "Name of the inferior PHP buffer."
  :tag "PHP inferior process buffer name"
  :version "30.1"
  :type 'string
  :safe 'stringp)

(defcustom php-ts-mode-inferior-history nil
  "File used to save command history of the inferior PHP process."
  :tag "PHP inferior process history file."
  :version "30.1"
  :type '(choice (const :tag "None" nil) file)
  :safe 'string-or-null-p)

(defcustom php-ts-mode-find-sibling-rules
  (list (list (rx "src/" (group (+ not-newline) "/") (group (+ (not "/"))) ".php") "tests/\\1\\2Test.php")
	(list (rx "tests/" (group (+ not-newline) "/") (group (+ (not "/"))) "Test.php") "src/\\1\\2.php"))
  "Rules for finding sibling files.
See `find-sibling-rules' for the form of the value.
As a default, the rules try to find the corresponding test of the
current source file and vice versa.  Source files are assumed to be in
src/, and tests of in tests/.  Many frameworks have a folder
arrangement similar to this."
  :type '(alist :key-type (regexp :tag "Match")
		:value-type (repeat (string :tag "Expansion")))
  :tag "PHP find sibling rules"
  :version "31.1")

(defcustom php-ts-mode-phpdoc-highlight-errors nil
  "Highlight tags unknown to the phpdoc parser.
If nil try to highlight even incorrect nodes as if they were correct,
otherwise show phpdoc error using a `font-lock-warning-face'."
  :tag "PHPDOC face for unknown tags or errors."
  :version "31.1"
  :type 'boolean
  :safe 'booleanp)

(defvar php-ts-mode--inferior-prompt "php >"
  "Prompt used by PHP inferior process.")

(defun php-ts-mode--indent-style-setter (sym val)
  "Custom setter for `php-ts-mode-set-style'.

Apart from setting the default value of SYM to VAL, also change
the value of SYM in `php-ts-mode' buffers to VAL.
SYM should be `php-ts-mode-indent-style', and VAL should be a style
symbol."
  (set-default sym val)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'php-ts-mode)
	(php-ts-mode-set-style val)))))

;; taken from c-ts-mode
(defun php-ts-indent-style-safep (style)
  "Non-nil if STYLE's value is safe for file-local variables."
  (and (symbolp style) (not (functionp style))))

(defcustom php-ts-mode-indent-style 'psr2
  "Style used for indentation.
The selected style could be one of:
`PSR-2/PSR-12' - use PSR standards (PSR-2, PSR-12), this is the default.
`PEAR' - use coding styles preferred for PEAR code and modules.
`Drupal' - use coding styles preferred for working with Drupal projects.
`WordPress' - use coding styles preferred for working with WordPress projects.
`Symfony' - use coding styles preferred for working with Symfony projects.
`Zend' - use coding styles preferred for working with Zend projects.

If one of the supplied styles doesn't suffice, a function could be
set instead.  This function is expected return a list that
follows the form of `treesit-simple-indent-rules'."
  :tag "PHP indent style"
  :version "30.1"
  :type '(choice (const :tag "PSR-2/PSR-12" psr2)
		 (const :tag "PEAR" pear)
		 (const :tag "Drupal" drupal)
		 (const :tag "WordPress" wordpress)
		 (const :tag "Symfony" symfony)
		 (const :tag "Zend" zend)
		 (function :tag "A function for user customized style" ignore))
  :set #'php-ts-mode--indent-style-setter
  :safe #'php-ts-indent-style-safep)


;;; Flymake integration

;; based on lua-ts-mode
(defvar-local php-ts-mode--flymake-process nil
  "Store the Flymake process.")

;; TODO: add phpmd and phpcs
(defun php-ts-mode-flymake-php (report-fn &rest _args)
  "PHP backend for Flymake.
Calls REPORT-FN directly."
  (when (process-live-p php-ts-mode--flymake-process)
    (kill-process php-ts-mode--flymake-process))
  (let ((source (current-buffer))
	(diagnostics-pattern (eval-when-compile
			       (rx bol (? "PHP ") ;; every diagnostic line start with PHP
				   (group (or "Fatal" "Parse")) ;; 1: type
				   " error:" (+ (syntax whitespace))
				   (group (+? nonl)) ;; 2: msg
				   " in " (group (+? nonl)) ;; 3: file
				   " on line " (group (+ num)) ;; 4: line
				   eol))))
    (save-restriction
      (widen)
      (setq php-ts-mode--flymake-process
	    (make-process
	     :name "php-ts-mode-flymake"
	     :noquery t
	     :connection-type 'pipe
	     :buffer (generate-new-buffer " *php-ts-mode-flymake*")
	     :command `(,(php-ts-mode--executable)
			"-l" "-d" "display_errors=0")
	     :sentinel
	     (lambda (proc _event)
	       (when (eq 'exit (process-status proc))
		 (unwind-protect
		     (if (with-current-buffer source
			   (eq proc php-ts-mode--flymake-process))
			 (with-current-buffer (process-buffer proc)
			   (goto-char (point-min))
			   (let (diags)
			     (while (search-forward-regexp
				     diagnostics-pattern
				     nil t)
			       (let* ((beg
				       (car (flymake-diag-region
					     source
					     (string-to-number (match-string 4)))))
				      (end
				       (cdr (flymake-diag-region
					     source
					     (string-to-number (match-string 4)))))
				      (msg (match-string 2))
				      (type :error))
				 (push (flymake-make-diagnostic
					source beg end type msg)
				       diags)))
			     (funcall report-fn diags)))
		       (flymake-log :warning "Canceling obsolete check %s" proc))
		   (kill-buffer (process-buffer proc)))))))
      (process-send-region php-ts-mode--flymake-process (point-min) (point-max))
      (process-send-eof php-ts-mode--flymake-process))))


;;; Utils

(defun php-ts-mode--executable ()
  "Return the absolute filename of the php executable.
If the `default-directory' is remote, search on a remote host, otherwise
it searches locally.  If `php-ts-mode-alternative-php-program-name' is
non-nil, it searches for this program instead of the usual `php'.
If the search fails, it returns `php-ts-mode-php-default-executable'."
  (or (executable-find
       (or php-ts-mode-alternative-php-program-name "php") t)
      php-ts-mode-php-default-executable))

(defun php-ts-mode-show-ini ()
  "Pop a buffer showing the PHP configuration file names."
  (interactive)
  (let ((buffer (get-buffer-create "*PHP ini*"))
	(button-action (lambda (_) (find-file-at-point)))
	(remote (file-remote-p default-directory)))
    (with-current-buffer buffer
      (view-mode -1)
      (erase-buffer)
      (shell-command (concat (php-ts-mode--executable) " --ini") buffer)
      ;; Remove useless ',' at end of some rows.
      (while (re-search-forward "," nil t)
	(replace-match "" nil nil))
      (goto-char (point-min))
      (while (re-search-forward
	      "[a-zA-Z]?[:]?[/\\].+"
	      nil t)
	(let ((start (match-beginning 0))
	      (end (match-end 0)))
	  ;; If PHP is on a remote machine, the path must be preceded
	  ;; by the remote identifier.
	  (when remote
	    (goto-char start)
	    (insert remote)
	    (setq end (+ end (length remote)))
	    (goto-char end))
	  (make-button start end 'action button-action)))
      (goto-char (point-min))
      (view-mode +1))
    (when buffer
      (pop-to-buffer buffer))))

(defun php-ts-mode--get-indent-style ()
  "Helper function to set indentation style.
MODE can be `psr2', `pear', `drupal', `wordpress', `symfony', `zend'."
  (let ((style
	 (if (functionp php-ts-mode-indent-style)
	     (funcall php-ts-mode-indent-style)
	   (cl-case php-ts-mode-indent-style
	     (psr2 (alist-get 'psr2 (php-ts-mode--indent-styles)))
	     (pear (alist-get 'pear (php-ts-mode--indent-styles)))
	     (drupal (alist-get 'drupal (php-ts-mode--indent-styles)))
	     (wordpress (alist-get 'wordpress (php-ts-mode--indent-styles)))
	     (symfony (alist-get 'symfony (php-ts-mode--indent-styles)))
	     (zend (alist-get 'zend (php-ts-mode--indent-styles)))
	     (t (alist-get 'psr2 (php-ts-mode--indent-styles)))))))
    `((php ,@style))))

(defun php-ts-mode--prompt-for-style ()
  "Prompt for an indent style and return the symbol for it."
  (intern
   (completing-read
    "Style: "
    (mapcar #'car (php-ts-mode--indent-styles))
    nil t nil nil "default")))

(defun php-ts-mode-set-global-style (style)
  "Set the indent style of PHP modes globally to STYLE.

This changes the current indent style of every PHP buffer and
the default PHP indent style for `php-ts-mode'
in this Emacs session."
  (interactive (list (php-ts-mode--prompt-for-style)))
  (php-ts-mode--indent-style-setter 'php-ts-mode-indent-style style))

(defun php-ts-mode--set-indent-property (style)
  "Set the offset, tab, etc. according to STYLE."
  (cl-case style
    (psr2 (setq php-ts-mode-indent-offset 4
		tab-width 4
		indent-tabs-mode nil))
    (pear (setq php-ts-mode-indent-offset 4
		tab-width 4
		indent-tabs-mode nil))
    (drupal (setq php-ts-mode-indent-offset 2
		  tab-width 2
		  indent-tabs-mode nil))
    (wordpress (setq php-ts-mode-indent-offset 4
		     tab-width 4
		     indent-tabs-mode t))
    (symfony (setq php-ts-mode-indent-offset 4
		   tab-width 4
		   indent-tabs-mode nil))
    (zend (setq php-ts-mode-indent-offset 4
		tab-width 4
		indent-tabs-mode nil))))

(defun php-ts-mode-set-style (style)
  "Set the PHP indent style of the current buffer to STYLE.
To set the default indent style globally, use
`php-ts-mode-set-global-style'."
  (interactive (list (php-ts-mode--prompt-for-style)))
  (cond
   ((not (derived-mode-p 'php-ts-mode))
    (user-error "The current buffer is not in `php-ts-mode'"))
   ((equal php-ts-mode-indent-style style)
    (message "The style is already %s" style));; nothing to do
   (t (progn
	(setq-local php-ts-mode-indent-style style)
	(php-ts-mode--set-indent-property style)
	(let ((rules (assq-delete-all 'php treesit-simple-indent-rules))
	      (new-style (car (treesit--indent-rules-optimize
			       (php-ts-mode--get-indent-style)))))
	  (setq treesit-simple-indent-rules (cons new-style rules))
	  (message "Switch to %s style" style))))))

(defun php-ts-mode--get-parser-ranges ()
  "Return the ranges covered by the parsers.

`php-ts-mode' use five parsers, this function returns, for the
current buffer, the ranges covered by each parser.
Useful for debugging."
  (let ((ranges)
	(parsers (treesit-parser-list nil nil t)))
    (if (not parsers)
	(message "At least one parser must be initialized"))
    (cl-loop
     for parser in parsers
     do (push (list parser (treesit-parser-included-ranges parser)) ranges)
     finally return ranges)))


;;; Syntax table

(defvar php-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\"     table)
    (modify-syntax-entry ?+  "."      table)
    (modify-syntax-entry ?-  "."      table)
    (modify-syntax-entry ?=  "."      table)
    (modify-syntax-entry ?%  "."      table)
    (modify-syntax-entry ?<  "."      table)
    (modify-syntax-entry ?>  "."      table)
    (modify-syntax-entry ?&  "."      table)
    (modify-syntax-entry ?|  "."      table)
    (modify-syntax-entry ?\' "\""     table)
    (modify-syntax-entry ?\240 "."    table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)
    ;; php specific syntax
    (modify-syntax-entry ?_  "w"      table)
    (modify-syntax-entry ?`  "\""     table)
    (modify-syntax-entry ?\" "\""     table)
    (modify-syntax-entry ?\r "> b"    table)
    (modify-syntax-entry ?#  "< b"    table)
    (modify-syntax-entry ?$  "_"      table)
    table)
  "Syntax table for `php-ts-mode'.")


;;; Indent

;; taken from c-ts-mode
(defun php-ts-mode--else-heuristic (node parent bol &rest _)
  "Heuristic matcher for when \"else\" is followed by a closing bracket.

PARENT is NODE's parent, BOL is the beginning of non-whitespace
characters of the current line."
  (and (null node)
       (save-excursion
	 (forward-line -1)
	 (looking-at (rx (* whitespace) "else" (* whitespace) eol)))
       (let ((next-node (treesit-node-first-child-for-pos parent bol)))
	 (equal (treesit-node-type next-node) "}"))))

;; taken from c-ts-mode
(defun php-ts-mode--first-sibling (node parent &rest _)
  "Matches when NODE is the \"first sibling\".

\"First sibling\" is defined as: the first child node of PARENT
such that it's on its own line.  NODE is the node to match and
PARENT is its parent."
  (let ((prev-sibling (treesit-node-prev-sibling node t)))
    (or (null prev-sibling)
	(save-excursion
	  (goto-char (treesit-node-start prev-sibling))
	  (<= (line-beginning-position)
	      (treesit-node-start parent)
	      (line-end-position))))))

(defun php-ts-mode--parent-eol (_node parent &rest _)
  "Find the last non-space characters of the PARENT of the current NODE.

NODE is the node to match and PARENT is its parent."
  (save-excursion
    (goto-char (treesit-node-start parent))
    (line-end-position)))

(defun php-ts-mode--parent-html-heuristic (node parent bol &rest _)
  "Return position based on html indentation.

Return 0 if the NODE is after the </html>.  If
`php-ts-mode-html-relative-indent' is not nil return the indentation
point of the last word before the NODE, plus the indentation offset,
otherwise return only the indentation point.  If there is no HTML tag,
it returns the beginning of the parent.  When NODE is nil it behaves
like \"prev-siblings\" of `treesit-simple-indent-presets'.  It can be
used when you want to indent PHP code relative to the HTML.  PARENT is
NODE's parent, BOL is the beginning of non-whitespace characters of
the current line."
  (save-excursion
    (cond
     ((eq php-ts-mode-html-relative-indent 'ignore) (line-beginning-position))
     ((search-backward "</html>" (treesit-node-start parent) t 1) (line-beginning-position))
     ((null node) (apply (alist-get 'prev-sibling treesit-simple-indent-presets) node parent bol nil))
     (t (when-let* ((html-node (treesit-search-forward
				node
				(lambda (node)
				  (equal (treesit-node-type node) "text"))
				t))
		    (end-html (treesit-node-end html-node)))
	  (goto-char end-html)
	  ;; go to the start of the last tag
	  ;; of the "text" node
	  (backward-word)
	  (back-to-indentation)
	  (if php-ts-mode-html-relative-indent
	      (+ (point) php-ts-mode-html-indent-offset)
	    (point)))))))

(defun php-ts-mode--array-element-heuristic (_node parent _bol &rest _)
  "Return of the position of the first element of the array.

PARENT is NODE's parent, BOL is the beginning of non-whitespace
characters of the current line."
  (let ((parent-start
	 (treesit-node-start parent))
	(parent-first-child-start
	 (treesit-node-start (treesit-node-child parent 2))))
    (if (equal
	 (line-number-at-pos parent-start)
	 (line-number-at-pos parent-first-child-start))
	;; if array_creation_expression and the first
	;; array_element_initializer are on the same same line
	parent-first-child-start
      ;; else return parent-bol plus the offset
      (save-excursion
	(goto-char (treesit-node-start parent))
	(back-to-indentation)
	(+ (point) php-ts-mode-indent-offset)))))

(defun php-ts-mode--anchor-first-sibling (_node parent _bol &rest _)
  "Return the start of the first child of a sibling of PARENT.

If the first sibling of PARENT and the first child of the sibling are
on the same line return the start position of the first child of the
sibling.  Otherwise return the start of the first sibling.
PARENT is NODE's parent, BOL is the beginning of non-whitespace
characters of the current line."
  (let ((first-sibling-start
	 (treesit-node-start (treesit-node-child parent 0)))
	(first-sibling-child-start
	 (treesit-node-start (treesit-node-child parent 1))))
    (if (equal
	 (line-number-at-pos first-sibling-start)
	 (line-number-at-pos first-sibling-child-start))
	;; if are on the same line return the child start
	first-sibling-child-start
      first-sibling-start)))

;; adapted from c-ts-mode--anchor-prev-sibling
(defun php-ts-mode--anchor-prev-sibling (node parent bol &rest _)
  "Return the start of the previous named sibling of NODE.

Return nil if a) there is no prev-sibling, or b) prev-sibling
doesn't have a child.

PARENT is NODE's parent, BOL is the beginning of non-whitespace
characters of the current line."
  (when-let* ((prev-sibling
	       (or (treesit-node-prev-sibling node t)
		   (treesit-node-prev-sibling
		    (treesit-node-first-child-for-pos parent bol) t)
		   (treesit-node-child parent -1 t)))
	      (continue t))
    (save-excursion
      (while (and prev-sibling continue)
	(goto-char (treesit-node-start prev-sibling))
	(if (looking-back (rx bol (* whitespace))
			  (line-beginning-position))
	    (setq continue nil)
	  (setq prev-sibling
		(treesit-node-prev-sibling prev-sibling)))))
    (treesit-node-start prev-sibling)))

(defun php-ts-mode--pipe-heuristic (node parent _bol &rest _)
  "Return the start of the previous pipe to the current pipe NODE.
Otherwise return the beginning of line of the previous non pipe NODE.

PARENT is NODE's parent, BOL is the beginning of non-whitespace
characters of the current line."
  (save-excursion
    (let* ((parent-start (treesit-node-start parent))
	   (node-start (treesit-node-start node))
	   (bound (progn
		    (goto-char parent-start)
		    (beginning-of-line 1)
		    (point))))
      (goto-char node-start)
      (let ((previous-pipe (search-backward "|>" bound t nil)))
	(or previous-pipe (+ bound php-ts-mode-indent-offset))))))

(defun php-ts-mode--indent-styles ()
  "Indent rules supported by `php-ts-mode'."
  (let ((common
	 `(;; Handle indentation relatives to HTML.
	   ((or (parent-is "program")
		(parent-is "text_interpolation"))
	    php-ts-mode--parent-html-heuristic 0)

	   (php-ts-mode--else-heuristic prev-line php-ts-mode-indent-offset)

	   ((query "(ERROR (ERROR)) @indent") parent-bol 0)

	   ((node-is ")") parent-bol 0)
	   ((node-is "]") parent-bol 0)
	   ((node-is "else_clause") parent-bol 0)
	   ((node-is "case_statement") parent-bol php-ts-mode-indent-offset)
	   ((node-is "default_statement") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "default_statement") parent-bol php-ts-mode-indent-offset)
	   ((and
	     (parent-is "expression_statement")
	     (node-is ";"))
	    parent-bol 0)
	   ((parent-is "expression_statement") parent-bol php-ts-mode-indent-offset)
	   ;; `c-ts-common-looking-at-star' has to come before
	   ;; `c-ts-common-comment-2nd-line-matcher'.
	   ((and (parent-is "comment") c-ts-common-looking-at-star)
	    c-ts-common-comment-start-after-first-star -1)
	   (c-ts-common-comment-2nd-line-matcher
	    c-ts-common-comment-2nd-line-anchor
	    1)
	   ((parent-is "comment") prev-adaptive-prefix 0)

	   ((parent-is "method_declaration") parent-bol 0)
	   ((node-is "class_interface_clause") parent-bol php-ts-mode-indent-offset)
	   ((query "(class_interface_clause (name) @indent)") php-ts-mode--parent-eol 1)
	   ((query "(class_interface_clause (qualified_name) @indent)")
	    parent-bol php-ts-mode-indent-offset)
	   ((parent-is "class_declaration") parent-bol 0)
	   ((parent-is "namespace_use_declaration") parent-bol php-ts-mode-indent-offset)
	   ((node-is "namespace_use_clause") parent-bol php-ts-mode-indent-offset)
	   ((or (parent-is "use_declaration")
		(parent-is "use_list"))
	    parent-bol php-ts-mode-indent-offset)
	   ((parent-is "function_definition") parent-bol 0)
	   ((parent-is "member_call_expression") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "conditional_expression") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "assignment_expression") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "array_creation_expression") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "attribute_group") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "parenthesized_expression") first-sibling 1)

	   ((node-is "|>") php-ts-mode--pipe-heuristic 0)
	   ((parent-is "binary_expression") parent 0)
	   ((or (parent-is "arguments")
		(parent-is "formal_parameters"))
	    parent-bol php-ts-mode-indent-offset)

	   ((query "(for_statement (assignment_expression left: (_)) @indent)")
	    parent-bol php-ts-mode-indent-offset)
	   ((query "(for_statement (binary_expression left: (_)) @indent)")
	    parent-bol php-ts-mode-indent-offset)
	   ((query "(for_statement (update_expression (_)) @indent)")
	    parent-bol php-ts-mode-indent-offset)
	   ((query "(function_call_expression arguments: (_) @indent)")
	    parent php-ts-mode-indent-offset)
	   ((query "(member_call_expression arguments: (_) @indent)")
	    parent php-ts-mode-indent-offset)
	   ((query "(scoped_call_expression name: (_) @indent)")
	    parent php-ts-mode-indent-offset)
	   ((parent-is "scoped_property_access_expression")
	    parent php-ts-mode-indent-offset)

	   ;; Closing bracket. Must stay here, the rule order matter.
	   ((node-is "}") standalone-parent 0)
	   ;; handle multiple single line comment that start at the and of a line
	   ((match "comment" "declaration_list") php-ts-mode--anchor-prev-sibling 0)
	   ((parent-is "declaration_list") column-0 php-ts-mode-indent-offset)

	   ((parent-is "initializer_list") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "property_hook_list") parent-bol php-ts-mode-indent-offset)

	   ;; Statement in {} blocks.
	   ((or (and (or (parent-is "compound_statement")
			 (parent-is "colon_block"))
		     ;; If the previous sibling(s) are not on their
		     ;; own line, indent as if this node is the first
		     ;; sibling
		     php-ts-mode--first-sibling)
		(or (match null "compound_statement")
		    (match null "colon_block")))
	    standalone-parent php-ts-mode-indent-offset)
	   ((or (parent-is "compound_statement")
		(parent-is "colon_block"))
	    parent-bol php-ts-mode-indent-offset)
	   ;; Opening bracket.
	   ((or (node-is "compound_statement")
		(node-is "colon_block"))
	    standalone-parent php-ts-mode-indent-offset)

	   ((parent-is "match_block") parent-bol php-ts-mode-indent-offset)
	   ((parent-is "switch_block") parent-bol 0)

	   ;; These rules are for cases where the body is bracketless.
	   ((match "while" "do_statement") parent-bol 0)
	   ;; rule for PHP alternative syntax
	   ((or (node-is "else_if_clause")
		(node-is "endif")
		(node-is "endfor")
		(node-is "endforeach")
		(node-is "endwhile"))
	    parent-bol 0)
	   ((or (parent-is "if_statement")
		(parent-is "else_clause")
		(parent-is "for_statement")
		(parent-is "foreach_statement")
		(parent-is "while_statement")
		(parent-is "do_statement")
		(parent-is "switch_statement")
		(parent-is "case_statement")
		(parent-is "empty_statement"))
	    parent-bol php-ts-mode-indent-offset)

	   ;; Do not touch the indentation inside a multiline the strings.
	   ((parent-is "string_content") no-indent 0)

	   ;; Workaround: handle "for" open statement group. Currently
	   ;; the grammar handles it differently than other control structures.
	   (no-node prev-sibling 0))))
    `((psr2
       ((parent-is "function_call_expression") parent-bol php-ts-mode-indent-offset)
       ,@common)
      (pear
       ((or (node-is "case_statement")
	    (node-is "default_statement"))
	parent-bol 0)
       ((parent-is "binary_expression") parent-bol php-ts-mode-indent-offset)
       ,@common)
      (drupal
       ((parent-is "if_statement") parent-bol 0)
       ((parent-is "binary_expression") parent-bol php-ts-mode-indent-offset)
       ((parent-is "function_call_expression") parent-bol php-ts-mode-indent-offset)
       ,@common)
      (symfony
       ((parent-is "function_call_expression") parent-bol php-ts-mode-indent-offset)
       ,@common)
      (wordpress
       ,@common)
      (zend
       ((parent-is "class_interface_clause") php-ts-mode--anchor-first-sibling 0)
       ((parent-is "function_call_expression") first-sibling 0)
       ((parent-is "array_creation_expression") php-ts-mode--array-element-heuristic 0)
       ,@common))))

(defvar php-ts-mode--phpdoc-indent-rules
  '((phpdoc
     ((and (parent-is "document") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     (c-ts-common-comment-2nd-line-matcher
      c-ts-common-comment-2nd-line-anchor
      1)))
  "Tree-sitter indentation rules for `phpdoc'.")


;;; Font-lock

(defun php-ts-mode--test-namespace-name-as-prefix-p ()
  "Return t if namespace_name_as_prefix is a named node, nil otherwise."
  (treesit-query-valid-p 'php "(namespace_name_as_prefix)"))

(defun php-ts-mode--test-namespace-aliasing-clause-p ()
  "Return t if namespace_aliasing_clause is a named node, nil otherwise."
  (treesit-query-valid-p 'php "(namespace_aliasing_clause)"))

(defun php-ts-mode--test-namespace-use-group-clause-p ()
  "Return t if namespace_use_group_clause is a named node, nil otherwise."
  (treesit-query-valid-p 'php "(namespace_use_group_clause)"))

(defun php-ts-mode--test-visibility-modifier-operation-p ()
  "Return t if (visibility_modifier (operation)) is defined, nil otherwise."
  (treesit-query-valid-p 'php "(visibility_modifier (operation))"))

(defun php-ts-mode--test-property-hook-p ()
  "Return t if property_hook is a named node, nil otherwise."
  (treesit-query-valid-p 'php "(property_hook)"))

(defun php-ts-mode--test-relative-name-p ()
  "Return t if relative_name is a named node, nil otherwise."
  (treesit-query-valid-p 'php "(relative_name)"))

(defun php-ts-mode--test-php-end-tag-p ()
  "Return t if php_end_tag is a named node, nil otherwise."
  (treesit-query-valid-p 'php "(php_end_tag)"))

(defun php-ts-mode--test-yield-from-p ()
  "Return t if the keyword `yield from' is defined, nil otherwise."
  (treesit-query-valid-p 'php '("yield from")))

(defun php-ts-mode--test-pipe-p ()
  "Return t if the operator '|>' is defined, nil otherwise."
  (treesit-query-valid-p 'php '("|>")))

(defun php-ts-mode--keywords ()
  "PHP keywords for tree-sitter font-locking."
  (append
   '("abstract" "and" "array" "as" "break" "case" "catch"
     "class" "clone" "const" "continue" "declare" "default" "do" "echo"
     "else" "elseif" "enddeclare" "endfor" "endforeach" "endif"
     "endswitch" "endwhile" "enum" "exit" "extends" "final" "finally" "fn"
     "for" "foreach" "function" "global" "goto" "if" "implements"
     "include" "include_once" "instanceof" "insteadof" "interface"
     "list" "match" "namespace" "new" "null" "or" "print" "private"
     "protected" "public" "readonly" "require" "require_once" "return"
     "static" "switch" "throw" "trait" "try" "unset" "use" "while" "xor"
     "yield")
   (if (php-ts-mode--test-yield-from-p) '("yield from") '("from"))))

(defun php-ts-mode--operators ()
  "PHP operators for tree-sitter font-locking."
  (append
   '("--" "**=" "*=" "/=" "%=" "+=" "-=" ".=" "<<=" ">>=" "&=" "^="
     "|=" "??"  "??=" "||" "&&" "|" "^" "&" "==" "!=" "<>" "===" "!=="
     "<" ">" "<=" ">=" "<=>" "<<" ">>" "+" "-" "." "*" "**" "/" "%"
     "->" "?->" "...")
   (when (php-ts-mode--test-pipe-p) '("|>"))))

(defconst php-ts-mode--predefined-constant
  '(;; predefined constant
    "PHP_VERSION" "PHP_MAJOR_VERSION" "PHP_MINOR_VERSION"
    "PHP_RELEASE_VERSION" "PHP_VERSION_ID" "PHP_EXTRA_VERSION"
    "ZEND_THREAD_SAFE" "ZEND_DEBUG_BUILD" "PHP_ZTS" "PHP_DEBUG"
    "PHP_MAXPATHLEN" "PHP_OS" "PHP_OS_FAMILY" "PHP_SAPI" "PHP_EOL"
    "PHP_INT_MAX" "PHP_INT_MIN" "PHP_INT_SIZE" "PHP_FLOAT_DIG"
    "PHP_FLOAT_EPSILON" "PHP_FLOAT_MIN" "PHP_FLOAT_MAX"
    "PHP_WINDOWS_EVENT_CTRL_C" "PHP_WINDOWS_EVENT_CTRL_BREAK"
    "DEFAULT_INCLUDE_PATH" "PEAR_INSTALL_DIR" "PEAR_EXTENSION_DIR"
    "PHP_EXTENSION_DIR" "PHP_PREFIX" "PHP_BINDIR" "PHP_BINARY"
    "PHP_MANDIR" "PHP_LIBDIR" "PHP_DATADIR" "PHP_SYSCONFDIR"
    "PHP_LOCALSTATEDIR" "PHP_CONFIG_FILE_PATH" "PHP_CONFIG_FILE_SCAN_DIR"
    "PHP_SHLIB_SUFFIX" "PHP_FD_SETSIZE" "E_ERROR" "E_WARNING" "E_PARSE"
    "E_NOTICE" "E_CORE_ERROR" "E_CORE_WARNING" "E_COMPILE_ERROR"
    "E_COMPILE_WARNING" "E_USER_ERROR" "E_USER_WARNING"
    "E_USER_NOTICE" "E_DEPRECATED" "E_USER_DEPRECATED"
    "E_ALL" "E_STRICT"
    ;; math constant
    "M_PI" "M_E" "M_LOG2E" "M_LOG10E" "M_LN2" "M_LN10" "M_PI_2"
    "M_PI_4" "M_1_PI" "M_2_PI" "M_SQRTPI" "M_2_SQRTPI" "M_SQRT2"
    "M_SQRT3" "M_SQRT1_2" "M_LNPI" "M_EULER" "PHP_ROUND_HALF_UP"
    "PHP_ROUND_HALF_DOWN" "PHP_ROUND_HALF_EVEN" "PHP_ROUND_HALF_ODD"
    "NAN" "INF"
    ;; magic constant
    "__COMPILER_HALT_OFFSET__" "__CLASS__" "__DIR__" "__FILE__"
    "__FUNCTION__" "__LINE__" "__METHOD__" "__NAMESPACE__" "__TRAIT__")
  "PHP predefined constant.")

(defconst php-ts-mode--class-magic-methods
  '("__construct" "__destruct" "__call" "__callStatic" "__get" "__set"
    "__isset" "__unset" "__sleep" "__wakeup" "__serialize" "__unserialize"
    "__toString" "__invoke" "__set_state" "__clone" "__debugInfo")
  "PHP predefined magic methods.")

(defconst php-ts-mode--prettify-symbols-alist
  '(("<=" . ?≤)
    (">=" . ?≥)
    ("->"  . ?→)
    ("=>"  . ?⇒)
    ("::" . ?∷))
  "Value for `prettify-symbols-alist' in `php-ts-mode'.")

(defun php-ts-mode--font-lock-settings ()
  "Return tree-sitter font-lock settings for `php-ts-mode'."
  (treesit-font-lock-rules

   :language 'php
   :feature 'keyword
   :override t
   `([,@(php-ts-mode--keywords)] @font-lock-keyword-face
     ,@(when (php-ts-mode--test-visibility-modifier-operation-p)
	 '((visibility_modifier (operation) @font-lock-builtin-face)))
     (var_modifier) @font-lock-builtin-face)

   :language 'php
   :feature 'comment
   :override t
   '((comment) @font-lock-comment-face)

   :language 'php
   :feature 'constant
   `((boolean) @font-lock-constant-face
     (null) @font-lock-constant-face
     ;; predefined constant or built in constant (part of PHP core)
     ((name) @font-lock-builtin-face
      (:match ,(rx-to-string
		`(: bos (or ,@php-ts-mode--predefined-constant) eos))
	      @font-lock-builtin-face))
     ;; user defined constant
     ((name) @font-lock-constant-face
      (:match "\\`_*[A-Z][0-9A-Z_]+\\'" @font-lock-constant-face))
     (const_declaration
      (const_element (name) @font-lock-constant-face))
     ;; declare directive
     (declare_directive ["strict_types" "encoding" "ticks"] @font-lock-constant-face))

   :language 'php
   :feature 'name
   '((goto_statement (name) @font-lock-constant-face)
     (named_label_statement (name) @font-lock-constant-face))

   :language 'php
   :feature 'delimiter
   `((["," ":" ";" "\\"]) @font-lock-delimiter-face)

   :language 'php
   :feature 'operator
   `((error_suppression_expression "@" @font-lock-keyword-face)
     [,@(php-ts-mode--operators)] @font-lock-operator-face)

   :language 'php
   :feature 'variable-name
   :override t
   '(((name) @font-lock-keyword-face (:equal "this" @font-lock-keyword-face))
     (variable_name (name) @font-lock-variable-name-face)
     (relative_scope ["parent" "self" "static"] @font-lock-builtin-face)
     (relative_scope) @font-lock-constant-face
     (dynamic_variable_name (name) @font-lock-variable-name-face)
     (member_access_expression
      name: (_) @font-lock-variable-name-face)
     (scoped_property_access_expression
      scope: (name) @font-lock-constant-face)
     (nullsafe_member_access_expression (name) @font-lock-variable-name-face)
     (error_suppression_expression (name) @font-lock-property-name-face))

   :language 'php
   :feature 'string
   `(("\"") @font-lock-string-face
     (encapsed_string) @font-lock-string-face
     (string_content) @font-lock-string-face
     (string) @font-lock-string-face)

   :language 'php
   :feature 'literal
   '((integer) @font-lock-number-face
     (float) @font-lock-number-face
     (heredoc identifier: (heredoc_start) @font-lock-constant-face)
     (heredoc_body (string_content) @font-lock-string-face)
     (heredoc end_tag: (heredoc_end) @font-lock-constant-face)
     (nowdoc identifier: (heredoc_start) @font-lock-constant-face)
     (nowdoc_body (nowdoc_string) @font-lock-string-face)
     (nowdoc end_tag: (heredoc_end) @font-lock-constant-face)
     (shell_command_expression) @font-lock-string-face)

   :language 'php
   :feature 'type
   :override t
   '((union_type "|" @font-lock-operator-face)
     (union_type) @font-lock-type-face
     (bottom_type) @font-lock-type-face
     (primitive_type) @font-lock-type-face
     ((primitive_type) @font-lock-keyword-face
      (:equal "callable" @font-lock-keyword-face))
     (cast_type) @font-lock-type-face
     (named_type) @font-lock-type-face
     (optional_type) @font-lock-type-face)

   :language 'php
   :feature 'definition
   :override t
   `((php_tag) @font-lock-preprocessor-face
     ,@(if (php-ts-mode--test-php-end-tag-p)
	   '((php_end_tag) @font-lock-preprocessor-face)
	 '(("?>") @font-lock-preprocessor-face))
     ;; Highlights identifiers in declarations.
     (class_declaration
      name: (_) @font-lock-type-face)
     (class_interface_clause (name) @font-lock-type-face)
     (interface_declaration
      name: (_) @font-lock-type-face)
     (trait_declaration
      name: (_) @font-lock-type-face)
     (enum_declaration
      name: (_) @font-lock-type-face)
     (function_definition
      name: (_) @font-lock-function-name-face)
     ,@(when (php-ts-mode--test-property-hook-p)
	 '((property_hook (name) @font-lock-function-name-face)))
     (method_declaration
      name: (_) @font-lock-function-name-face)
     (method_declaration
      name: (name) @font-lock-builtin-face
      (:match ,(rx-to-string
		`(: bos (or ,@php-ts-mode--class-magic-methods) eos))
	      @font-lock-builtin-face))
     ("=>") @font-lock-keyword-face
     (object_creation_expression
      (name) @font-lock-type-face)
     ,@(when (php-ts-mode--test-namespace-name-as-prefix-p)
	 '((namespace_name_as_prefix "\\" @font-lock-delimiter-face)
	   (namespace_name_as_prefix
	    (namespace_name (name)) @font-lock-type-face)))
     ,@(if (php-ts-mode--test-namespace-aliasing-clause-p)
	   '((namespace_aliasing_clause (name) @font-lock-type-face))
	 '((namespace_use_clause alias: (name) @font-lock-type-face)))
     ,@(when (not (php-ts-mode--test-namespace-use-group-clause-p))
	 '((namespace_use_group
	    (namespace_use_clause (name) @font-lock-type-face))))
     (namespace_use_clause (name) @font-lock-type-face)
     (namespace_name "\\" @font-lock-delimiter-face)
     (namespace_name (name) @font-lock-type-face)
     (use_declaration (name) @font-lock-property-use-face)
     (use_instead_of_clause (name) @font-lock-type-face)
     (binary_expression
      operator: "instanceof"
      right: (name) @font-lock-type-face))

   :language 'php
   :feature 'function-scope
   :override t
   '((scoped_call_expression
      scope: (name) @font-lock-constant-face)
     (class_constant_access_expression (name) @font-lock-constant-face))

   :language 'php
   :feature  'function-call
   :override t
   '((function_call_expression
      function: (name) @font-lock-function-call-face)
     (scoped_call_expression
      name: (name) @font-lock-function-call-face)
     (member_call_expression
      name: (name) @font-lock-function-call-face)
     (nullsafe_member_call_expression
      name: (_) @font-lock-function-call-face))

   :language 'php
   :feature 'argument
   '((argument
      name: (_) @font-lock-constant-face))

   :language 'php
   :feature 'escape-sequence
   :override t
   '((string (escape_sequence) @font-lock-escape-face)
     (encapsed_string (escape_sequence) @font-lock-escape-face)
     (heredoc_body (escape_sequence) @font-lock-escape-face))

   :language 'php
   :feature 'base-clause
   :override t
   `((base_clause (name) @font-lock-type-face)
     (use_as_clause (name) @font-lock-property-use-face)
     ,@(when (not (php-ts-mode--test-namespace-name-as-prefix-p))
	 '((qualified_name prefix: "\\" @font-lock-delimiter-face)))
     (qualified_name (name) @font-lock-constant-face)
     ,@(when (php-ts-mode--test-relative-name-p)
	 '((relative_name (name) @font-lock-constant-face))))

   :language 'php
   :feature 'property
   '((enum_case
      name: (_) @font-lock-type-face))

   :language 'php
   :feature 'attribute
   '((((attribute (_) @attribute_name) @font-lock-preprocessor-face)
      (:equal "Deprecated" @attribute_name))
     (attribute_group (attribute (name) @font-lock-constant-face)))

   :language 'php
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'php
   :feature 'error
   :override t
   '((ERROR) @php-ts-mode--fontify-error)))


;;; Font-lock helpers

(defun php-ts-mode--custom-html-font-lock-settings ()
  "Tree-sitter Font-lock settings for HTML when embedded in PHP.
Like `mhtml-ts-mode--font-lock-settings' but adapted for `php-ts-mode'."
  (treesit-replace-font-lock-feature-settings
   (treesit-font-lock-rules
    :language 'html
    :override t
    :feature 'comment
    '((comment) @font-lock-comment-face
      ;; handle shebang path and others type of comment
      (document (text) @font-lock-comment-face)))
   (mhtml-ts-mode--treesit-font-lock-settings)))

(defvar php-ts-mode--phpdoc-font-lock-settings
  (treesit-font-lock-rules
   :language 'phpdoc
   :feature 'document
   :override t
   '((document) @font-lock-doc-face)

   :language 'phpdoc
   :feature 'type
   :override t
   '((union_type
      [(primitive_type) (named_type) (optional_type)] @font-lock-type-face)
     (array_type ["array" "list"] @font-lock-keyword-face)
     ([(primitive_type) (named_type) (optional_type)] @font-lock-type-face)
     (fqsen (name) @font-lock-function-name-face))

   :language 'phpdoc
   :feature 'attribute
   :override t
   `((tag_name) @font-lock-doc-markup-face
     (uri) @font-lock-doc-markup-face
     (tag
      [(version) (email_address)] @font-lock-doc-markup-face)
     (tag (author_name) @font-lock-property-name-face))

   :language 'phpdoc
   :feature 'variable
   :override t
   '((variable_name (name) @font-lock-variable-name-face))

   :language 'phpdoc
   :feature 'phpdoc-error
   :override t
   '((ERROR) @php-ts-mode--phpdoc-fontify-error))
  "Tree-sitter font-lock settings for phpdoc.")

(defun php-ts-mode--fontify-error (node override start end &rest _)
  "Fontify the error nodes.
For NODE, OVERRIDE, START, and END, see `treesit-font-lock-rules'."
  (treesit-fontify-with-override
   (treesit-node-start node) (treesit-node-end node)
   'font-lock-warning-face
   override start end))

(defun php-ts-mode--phpdoc-fontify-error (node override start end &rest _)
  "Fontify the phpdoc error nodes.
Most of the error are only tag non handled by the phpdoc parser.
For NODE, OVERRIDE, START, and END, see `treesit-font-lock-rules'."
  (when php-ts-mode-phpdoc-highlight-errors
    (php-ts-mode--fontify-error node override start end)))


;;; Imenu

(defun php-ts-mode--parent-object (node)
  "Return the name of the object that own NODE."
  (treesit-parent-until
   node
   (lambda (n)
     (member (treesit-node-type n)
	     '("class_declaration"
	       "enum_declaration"
	       "function_definition"
	       "interface_declaration"
	       "method_declaration"
	       "namespace_definition"
	       "trait_declaration")))))

(defun php-ts-mode--defun-name-separator (node)
  "Return a separator to connect object name, based on NODE type."
  (let ((node-type (treesit-node-type node)))
    (cond ((member node-type '("function_definition" "method_declaration"))
	   "()::")
	  ((member node-type '("class_declaration" "enum_declaration" "trait_declaration"))
	   "::")
	  (t "\\"))))

(defun php-ts-mode--defun-object-name (node node-text)
  "Compose the full name of a NODE that is a PHP variable, method, class etc.
If the NODE has a parent, it recursively concat the parent names with NODE-TEXT,
otherwise it returns NODE-TEXT."
  (let* ((parent-node (php-ts-mode--parent-object node))
	 (parent-node-text
	  (treesit-node-text
	   (treesit-node-child-by-field-name parent-node "name") t))
	 (parent-node-separator (php-ts-mode--defun-name-separator parent-node)))
    (if parent-node
	(progn
	  (setq parent-node-text
		(php-ts-mode--defun-object-name
		 parent-node
		 parent-node-text))
	  (concat parent-node-text parent-node-separator node-text))
      node-text)))

(defun php-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if the NODE has no field “name” or if NODE is not a defun node."
  (let ((lang (treesit-node-language node))
	(child (treesit-node-child-by-field-name node "name")))
    (if (eq lang 'php)
	(cl-case (intern (treesit-node-type node))
	  (class_declaration (treesit-node-text child t))
	  (trait_declaration (treesit-node-text child t))
	  (interface_declaration (treesit-node-text child t))
	  (namespace_definition (treesit-node-text child t))
	  (enum_declaration (treesit-node-text child t))
	  (function_definition (treesit-node-text child t))
	  (method_declaration
	   (php-ts-mode--defun-object-name node (treesit-node-text child t)))
	  (variable_name
	   (php-ts-mode--defun-object-name node (treesit-node-text node t)))
	  (const_element
	   (php-ts-mode--defun-object-name
	    node
	    (treesit-node-text (treesit-node-child node 0) t))))
      (mhtml-ts-mode--defun-name node))))


;;; Defun navigation

(defvar php-ts-mode--treesit-defun-type-regexp
  (rx bos (or "class_declaration"
	      "enum_declaration"
	      "function_definition"
	      "interface_declaration"
	      "method_declaration"
	      "namespace_definition"
	      "trait_declaration")
      eos)
  "Settings for `treesit-defun-type-regexp'.")

(defun php-ts-mode--indent-defun ()
  "Indent the current top-level declaration syntactically.
`treesit-defun-type-regexp' defines what constructs to indent."
  (interactive "*")
  (when-let* ((orig-point (point-marker))
	      (node (treesit-defun-at-point)))
    (indent-region (treesit-node-start node)
		   (treesit-node-end node))
    (goto-char orig-point)))

(defun php-ts-mode--defun-valid-p (node)
  "Return non-nil if NODE is a valid defun node.
Ie, NODE is not nested."
  (not (and (member (treesit-node-type node)
		    '("variable_name"
		      "const_element"
		      "enum_declaration"
		      "union_declaration"
		      "declaration"))
	    ;; If NODE's type is one of the above, make sure it is
	    ;; top-level.
	    (treesit-node-top-level
	     node (rx (or "variable_name"
			  "const_element"
			  "function_definition"
			  "enum_declaration"
			  "union_declaration"
			  "declaration"))))))


;;; Filling

(defun php-ts-mode--comment-indent-new-line (&optional soft)
  "Break line at point and indent, continuing comment if within one.
Like `c-ts-common-comment-indent-new-line', but handle the
less common PHP-style # comment.  SOFT works the same as in
`comment-indent-new-line'."
  (if (save-excursion
	;; Line start with # or ## or ###...
	(beginning-of-line)
	(re-search-forward
	 (rx "#" (group (* (any "#")) (* " ")))
	 (line-end-position)
	 t nil))
      (let ((offset (- (match-beginning 0) (line-beginning-position)))
	    (comment-prefix (match-string 0))
	    (insert-line-break
	     (lambda ()
	       (delete-horizontal-space)
	       (if soft
		   (insert-and-inherit ?\n)
		 (newline  1)))))
	(funcall insert-line-break)
	(delete-region (line-beginning-position) (point))
	(insert
	 (make-string offset ?\s)
	 comment-prefix))
    ;; other style of comments
    (c-ts-common-comment-indent-new-line soft)))

(defvar-local php-ts-mode--comment-current-plist
  '(:php nil :comment-start "// " :comment-end "")
  "Store the current state of PHP comment configuration.
`mhtml-ts-mode' changes the comment configuration when switching
between HTML, Css and Javascript, so we have to do the same when
switching back to PHP.
The prop :php must be t if we are in a PHP or PHPDOC range.")

(defun php-ts-mode--comment-setup (&optional force)
  "Set up local variables for comments.
If FORCE is t setup comment for PHP.  Depends on
`mhtml-ts-mode--comment-setup' and
`mhtml-ts-mode--comment-current-lang'."
  (let ((lang (treesit-language-at (point))))
    (if (and (or (eq lang 'php) (eq lang 'phpdoc) force)
	     (eq (plist-get php-ts-mode--comment-current-plist :php) nil))
	;; do setup only if the previous language isn't PHP
	(progn
	  (plist-put php-ts-mode--comment-current-plist :php t)
	  (c-ts-common-comment-setup)
	  (setq-local c-ts-common--comment-regexp "comment"
		      comment-line-break-function #'php-ts-mode--comment-indent-new-line
		      comment-style 'extra-line
		      comment-start (plist-get php-ts-mode--comment-current-plist :comment-start)
		      comment-end (plist-get php-ts-mode--comment-current-plist :comment-end)
		      comment-start-skip (rx (or (seq "#" (not (any "[")))
						 (seq "/" (+ "/"))
						 (seq "/" (+ "*")))
					     (* (syntax whitespace)))
		      ;; reset the state of mhtml-ts-mode--comment-setup
		      mhtml-ts-mode--comment-current-lang nil))
      ;; otherwise set comment style for other languages.
      (plist-put php-ts-mode--comment-current-plist :php nil)
      (setq-local comment-style 'indent) ; this is the default for mhtml-ts-mode.
      (mhtml-ts-mode--comment-setup))))


;;; Modes

(defun php-ts-mode-set-comment-style ()
  "Set a different comment style."
  (interactive)
  (let ((lang (treesit-language-at (point))))
    (when (or (eq lang 'php) (eq lang 'phpdoc))
      (setq-local comment-start
		  (completing-read
		   "Choose comment style: "
		   '("/**" "// " "/*" "# ") nil t nil nil "// "))
      (cond
       ((equal comment-start "/*") (setq-local comment-end "*/"))
       ((equal comment-start "// ") (setq-local comment-end ""))
       ((equal comment-start "# ") (setq-local comment-end ""))
       ((equal comment-start "/**") (setq-local comment-end "*/")))
      ;; store the value in order to recover when switch to php from other languages.
      (plist-put php-ts-mode--comment-current-plist :comment-start comment-start)
      (plist-put php-ts-mode--comment-current-plist :comment-end comment-end))))

(defvar-keymap php-ts-mode-map
  :doc "Keymap for `php-ts-mode' buffers."
  :parent prog-mode-map
  "C-c C-q" #'php-ts-mode--indent-defun
  "C-c ."   #'php-ts-mode-set-style
  "C-c C-k" #'php-ts-mode-set-comment-style
  "C-c C-n" #'run-php
  "C-c C-c" #'php-ts-mode-send-buffer
  "C-c C-l" #'php-ts-mode-send-file
  "C-c C-r" #'php-ts-mode-send-region)

(easy-menu-define php-ts-mode-menu php-ts-mode-map
  "Menu bar entry for `php-ts-mode'."
  `("PHP"
    ["Comment Out Region" comment-region
     :enable mark-active
     :help "Comment out the region between the mark and point"]
    ["Uncomment Region" (comment-region (region-beginning)
					(region-end) '(4))
     :enable mark-active
     :help "Uncomment the region between the mark and point"]
    ["Indent Top-level Expression" php-ts-mode--indent-defun
     :help "Indent/reindent top-level function, class, etc."]
    ["Indent Line or Region" indent-for-tab-command
     :help "Indent current line or region, or insert a tab"]
    ["Forward Expression" forward-sexp
     :help "Move forward across one balanced expression"]
    ["Backward Expression" backward-sexp
     :help "Move back across one balanced expression"]
    ("Style..."
     ["Set Indentation Style..." php-ts-mode-set-style
      :help "Set PHP indentation style for current buffer"]
     ["Show Current Style Name"(message "Indentation Style: %s"
					php-ts-mode-indent-style)
      :help "Show the name of the PHP indentation style for current buffer"]
     ["Set Comment Style" php-ts-mode-set-comment-style
      :help "Choose PHP comment style between block and line comments"])
    "--"
    ["Start interpreter" run-php
     :help "Run inferior PHP process in a separate buffer"]
    ["Show interpreter buffer" php-ts-mode-show-process-buffer]
    ["Hide interpreter buffer" php-ts-mode-hide-process-buffer]
    ["Kill interpreter process" php-ts-mode-kill-process]
    ["Evaluate buffer" php-ts-mode-send-buffer]
    ["Evaluate file" php-ts-mode-send-file]
    ["Evaluate region" php-ts-mode-send-region]
    "--"
    ["Start built-in webserver" php-ts-mode-run-php-webserver
     :help "Run the built-in PHP webserver"]
    "--"
    ["Customize" (lambda () (interactive) (customize-group "php-ts-mode"))]))

(defvar php-ts-mode--feature-list
  '((;; common
     comment definition spell
     ;; CSS specific
     query selector
     ;; HTML specific
     text
     ;; PHPDOC specific
     document
     phpdoc-error)
    (keyword string property type name)
    (;; common
     attribute assignment constant escape-sequence function-scope
     base-clause literal variable-name variable
     ;; Javascript specific
     jsx number pattern string-interpolation)
    (;; common
     argument bracket delimiter error function-call operator
     ;; Javascript specific
     function)))

;;;###autoload
(define-derived-mode php-ts-mode prog-mode
  '("PHP"
    (:eval (let ((lang (treesit-language-at (point))))
	     (cond ((eq lang 'html) "+HTML")
		   ((eq lang 'javascript) "+HTML+JS")
		   ((eq lang 'css) "+HTML+CSS")))))
  "Major mode for editing PHP, powered by tree-sitter."
  :syntax-table php-ts-mode--syntax-table

  ;; TODO: wouldn't better to delegate the check and the
  ;; init of the mhtml parser to mhtml-ts-mode?
  (if (not (and
	    (treesit-ensure-installed 'php)
	    (treesit-ensure-installed 'phpdoc)
	    (treesit-ensure-installed 'html)
	    (treesit-ensure-installed 'javascript)
	    (treesit-ensure-installed 'jsdoc)
	    (treesit-ensure-installed 'css)))
      (error "Tree-sitter for PHP isn't
    available.  You can install the parsers with M-x
    `php-ts-mode-install-parsers'")

    ;; phpdoc is a local parser, don't create a parser for it
    ;; create the parser needed by mhtml-ts-mode
    (treesit-parser-create 'html)
    (treesit-parser-create 'css)
    (treesit-parser-create 'javascript)

    ;; define the injected parser ranges
    (setq-local treesit-range-settings
		(append
		 (treesit-range-rules
		  :embed 'phpdoc
		  :host 'php
		  :local t
		  '(((comment) @cap
		     (:match "/\\*\\*" @cap)))

		  :embed 'html
		  :host 'php
		  '((program (text) @cap)
		    (text_interpolation (text) @cap)))
		 ;; inject the range rules from mhtml-ts-mode
		 mhtml-ts-mode--range-settings))

    ;; Navigation.
    (setq-local treesit-defun-name-function #'php-ts-mode--defun-name)

    (setq-local treesit-thing-settings
		(append
		 `((php
		    (defun ,php-ts-mode--treesit-defun-type-regexp)
		    (sexp (not (or (and named
					,(rx bos (or "program"
						     "comment")
					     eos))
				   (and anonymous
					,(rx bos (or "{" "}" "[" "]"
						     "(" ")" ",")
					     eos)))))
		    (list
		     ,(rx bos (or "namespace_use_group"
				  "enum_declaration_list"
				  "declaration_list"
				  "property_hook_list"
				  "use_list"
				  "anonymous_function_use_clause"
				  "formal_parameters"
				  "match_block"
				  "switch_block"
				  "compound_statement"
				  "parenthesized_expression"
				  "_array_destructing"
				  "arguments"
				  "_complex_string_part")
			  eos))
		    (sentence  ,(regexp-opt
				 '("break_statement"
				   "case_statement"
				   "continue_statement"
				   "declaration"
				   "default_statement"
				   "do_statement"
				   "expression_statement"
				   "for_statement"
				   "if_statement"
				   "return_statement"
				   "switch_statement"
				   "while_statement"
				   "statement")))
		    (text ,(regexp-opt '("comment" "text")))))
		 mhtml-ts-mode--treesit-thing-settings))

    ;; Indent.
    (when (eq php-ts-mode-indent-style 'wordpress)
      (setq-local indent-tabs-mode t))

    (setq-local c-ts-common-indent-offset 'php-ts-mode-indent-offset)
    (setq-local treesit-simple-indent-rules (php-ts-mode--get-indent-style))
    (setq-local treesit-simple-indent-rules
		(append treesit-simple-indent-rules
			php-ts-mode--phpdoc-indent-rules
			`((html ,@(alist-get 'html
					     (mhtml-ts-mode--treesit-indent-rules))))
			;; Replace rules for js and css, to
			;; indent appropriately when injected
			;; into html.
			(treesit-simple-indent-modify-rules
			 'javascript
			 `((javascript ((parent-is "program")
					mhtml-ts-mode--js-css-tag-bol
					php-ts-mode-js-css-indent-offset)))
			 `((javascript
			    ,@(alist-get 'javascript
					 (mhtml-ts-mode--treesit-indent-rules)))))

			(treesit-simple-indent-modify-rules
			 'css
			 `((css ((parent-is "stylesheet")
				 mhtml-ts-mode--js-css-tag-bol
				 php-ts-mode-js-css-indent-offset)))
			 `((css
			    ,@(alist-get 'css
					 (mhtml-ts-mode--treesit-indent-rules)))))
			))

    ;; Comment
    (php-ts-mode--comment-setup t) ; Init the comment setup for PHP
    ;; Allow the comment machinery to automatically switch between the
    ;; various comment styles supported by `php-ts-mode'.
    (setq-local comment-setup-function #'php-ts-mode--comment-setup)

    ;; PHP vars are case-sensitive
    (setq-local case-fold-search t)

    ;; Electric
    (setq-local electric-indent-chars
		(append "{}():;," electric-indent-chars))

    (setq-local electric-layout-rules
	        '((?\; . after) (?\{ . after) (?\} . before)))

    ;; Imenu/Which-function
    (setq-local treesit-aggregated-simple-imenu-settings
		(append
		 '((php ("Class" "\\`class_declaration\\'" nil nil)
			("Enum" "\\`enum_declaration\\'" nil nil)
			("Function" "\\`function_definition\\'" nil nil)
			("Interface" "\\`interface_declaration\\'" nil nil)
			("Method" "\\`method_declaration\\'" nil nil)
			("Namespace" "\\`namespace_definition\\'" nil nil)
			("Trait" "\\`trait_declaration\\'" nil nil)
			("Variable" "\\`variable_name\\'" nil nil)
			("Constant" "\\`const_element\\'" nil nil)))
		 mhtml-ts-mode--treesit-aggregated-simple-imenu-settings))

    ;; Outline
    (setq-local treesit-aggregated-outline-predicate
		(append
		 `((php . ,(rx bos (or "class_declaration"
				       "function_definition"
				       "interface_declaration"
				       "method_declaration"
				       "namespace_definition"
				       "trait_declaration")
			       eos))
		   (phpdoc . ,(rx bos "document" eos)))
		 mhtml-ts-mode--treesit-aggregated-outline-predicate))

    ;; Font-lock.
    (setq-local treesit-font-lock-settings
		(append
		 (php-ts-mode--font-lock-settings)
		 (php-ts-mode--custom-html-font-lock-settings)
		 php-ts-mode--phpdoc-font-lock-settings))

    (setq-local treesit-font-lock-feature-list php-ts-mode--feature-list)
    (setq-local prettify-symbols-alist php-ts-mode--prettify-symbols-alist)

    ;; Align.
    (setq-local align-indent-before-aligning t)

    ;; Find files
    (setq-local find-sibling-rules php-ts-mode-find-sibling-rules)

    ;; should be the last one
    (setq-local treesit-primary-parser (treesit-parser-create 'php))
    (treesit-major-mode-setup)
    (add-hook 'flymake-diagnostic-functions #'php-ts-mode-flymake-php nil 'local)))


;;;###autoload
(defun php-ts-mode-run-php-webserver (&optional port
						hostname
						document-root
						router-script
						num-of-workers
						config)
  "Run PHP built-in web server.

PORT: Port number of built-in web server, default `php-ts-mode-ws-port'.
Prompt for the port if the default value is nil.
HOSTNAME: Hostname or IP address of Built-in web server,
default `php-ts-mode-ws-hostname'.  Prompt for the hostname if the
default value is nil.
DOCUMENT-ROOT: Path to Document root, default `php-ts-mode-ws-document-root'.
Prompt for the document-root if the default value is nil.
ROUTER-SCRIPT: Path of the router PHP script,
see `https://www.php.net/manual/en/features.commandline.webserver.php'
NUM-OF-WORKERS: Before run the web server set the
PHP_CLI_SERVER_WORKERS env variable useful for testing code against
multiple simultaneous requests
CONFIG: Alternative php.ini config, default `php-ts-mode-php-config'.

Interactively, when invoked with prefix argument, always prompt for
PORT, HOSTNAME, DOCUMENT-ROOT, ROUTER-SCRIPT, NUM-OF-WORKERS and
CONFIG."
  (interactive (when current-prefix-arg
		 (php-ts-mode--webserver-read-args)))
  (let* ((port (or
		port
		php-ts-mode-ws-port
		(php-ts-mode--webserver-read-args 'port)))
	 (hostname (or
		    hostname
		    php-ts-mode-ws-hostname
		    (php-ts-mode--webserver-read-args 'hostname)))
	 (document-root (or
			 document-root
			 php-ts-mode-ws-document-root
			 (php-ts-mode--webserver-read-args 'document-root)))
	 (config (or config
		     (when php-ts-mode-php-config
		       (expand-file-name php-ts-mode-php-config))))
	 (host (format "%s:%d" hostname port))
	 (name (format "PHP web server on: %s" host))
	 (buf-name (format "*%s*" name))
	 (args (delq
		nil
		(list "-S" host
		      "-t" document-root
		      (when config
			(format "-c %s" config))
		      router-script)))
	 (process-environment
	  (nconc (cond
		  (num-of-workers
		   (list
		    (format "PHP_CLI_SERVER_WORKERS=%d" num-of-workers)))
		  (php-ts-mode-ws-workers
		   (list
		    (format "PHP_CLI_SERVER_WORKERS=%d" php-ts-mode-ws-workers))))
		 process-environment)))
    (if (get-buffer buf-name)
	(message "Switch to already running web server into buffer %s" buf-name)
      (message "Run PHP built-in web server with args %s into buffer %s"
	       (string-join args " ")
	       buf-name)
      (apply #'make-comint name (php-ts-mode--executable) nil args))
    (funcall
     (if (called-interactively-p 'interactive) #'display-buffer #'get-buffer)
     buf-name)))

(derived-mode-add-parents 'php-ts-mode '(php-mode))

(defun php-ts-mode--webserver-read-args (&optional type)
  "Helper for `php-ts-mode-run-php-webserver'.
The optional TYPE can be the symbol \"port\", \"hostname\", \"document-root\",
\"router-script\", \"num-workers\" or \"config\", otherwise it requires all of them."
  (let ((ask-port (lambda ()
		    (read-number "Port: " (or
					   php-ts-mode-ws-port
					   3000))))
	(ask-hostname (lambda ()
			(read-string "Hostname: "
				     (or
				      php-ts-mode-ws-hostname
				      "localhost"))))
	(ask-document-root (lambda ()
			     (expand-file-name
			      (read-directory-name "Document root: "
						   (file-name-directory
						    (or (buffer-file-name)
							default-directory))))))
	(ask-router-script (lambda ()
			     (expand-file-name
			      (read-file-name "Router script: "
					      (file-name-directory
					       (or (buffer-file-name)
						   default-directory))))))
	(ask-num-workers (lambda ()
			   (let ((num-workers
				  (read-number
				   "Number of workers (less then 2 means no workers): "
				   (or php-ts-mode-ws-workers 0))))
			     ;; num-workers must be >= 2 or nil
			     ;; otherwise PHP's built-in web server will not start.
			     (if (> num-workers 1)
				 num-workers
			       nil))))
	(ask-config (lambda()
		      (let ((file-name (expand-file-name
					(read-file-name "Alternative php.ini: "
							(file-name-directory
							 (or (buffer-file-name)
							     default-directory))))))
			(if (string= "" (file-name-directory file-name))
			    nil
			  file-name)))))
    (cl-case type
      (port (funcall ask-port))
      (hostname (funcall ask-hostname))
      (document-root (funcall ask-document-root))
      (router-script (funcall ask-router-script))
      (num-of-workers (funcall ask-num-workers))
      (config (funcall ask-config))
      (t (list
	  (funcall ask-port)
	  (funcall ask-hostname)
	  (funcall ask-document-root)
	  (funcall ask-router-script)
	  (funcall ask-num-workers)
	  (funcall ask-config))))))

(define-derived-mode inferior-php-ts-mode comint-mode "Inferior PHP"
  "Major mode for PHP inferior process."
  (setq-local scroll-conservatively 1
	      comint-input-ring-file-name php-ts-mode-inferior-history
	      comint-input-ignoredups t
	      comint-prompt-read-only t
	      comint-use-prompt-regexp t
	      comint-prompt-regexp (concat "^" php-ts-mode--inferior-prompt " "))
  (comint-read-input-ring t))


;;; Inferior PHP process.

;;;###autoload
(defun run-php (&optional cmd config)
  "Run an PHP interpreter as a inferior process.

Arguments CMD and CONFIG, default to `php-ts-mode-php-executable'
and `php-ts-mode-php-config' respectively, control which PHP interpreter is run.
Prompt for CMD if `php-ts-mode-php-executable' is nil.
Optional CONFIG, if supplied, is the php.ini file to use."
  (interactive (when current-prefix-arg
		 (list
		  (read-string "Run PHP: " (php-ts-mode--executable))
		  (expand-file-name
		   (read-file-name "With config: " php-ts-mode-php-config)))))
  (let* ((php-prog (php-ts-mode--executable))
	 (buffer (get-buffer-create php-ts-mode-inferior-php-buffer))
	 (cmd (or
	       cmd
	       php-prog
	       (read-string "Run PHP: " php-prog)))
	 (config (or
		  config
		  (and php-ts-mode-php-config
		       (expand-file-name php-ts-mode-php-config)))))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
	(inferior-php-ts-mode-startup cmd config)
	(inferior-php-ts-mode)))
    (when buffer
      (pop-to-buffer buffer))))

(defun inferior-php-ts-mode-startup (cmd &optional config)
  "Start an inferior PHP process with command CMD and init file CONFIG.
CMD is the command to run.  Optional CONFIG, if supplied, is the php.ini
file to use."
  (apply #'make-comint-in-buffer
		     (string-replace "*" "" php-ts-mode-inferior-php-buffer)
		     php-ts-mode-inferior-php-buffer
		     cmd
		     nil
		     (delq
		      nil
		      (list
		       (when config
			 (format "-c %s" config))
		       "-a")))
  (add-hook 'comint-preoutput-filter-functions
	    (lambda (string)
	      (let ((prompt (concat php-ts-mode--inferior-prompt " ")))
		(if (member
		     string
		     (list prompt "php { " "php ( " "/* > " "Interactive shell\n\n"))
		    string
		  (let (;; Filter out prompts characters that accumulate when sending
			;; regions to the inferior process.
			(clean-string
			 (replace-regexp-in-string
			  (rx-to-string `(or
					  (+ "php >" (opt space))
					  (+ "php {" (opt space))
					  (+ "php (" (opt space))
					  (+ "/*" (1+ space) (1+ ">") (opt space))))
			  "" string)))
		    ;; Re-add the prompt for the next line, if isn't empty.
		    (if (string= clean-string "")
			""
		      (concat (string-chop-newline clean-string) "\n" prompt))))))
	    nil t)
  (when php-ts-mode-inferior-history
    (set-process-sentinel
     (get-buffer-process php-ts-mode-inferior-php-buffer)
     'php-ts-mode-inferior--write-history)))

;; taken and adapted from lua-ts-mode
(defun php-ts-mode-inferior--write-history (process _)
  "Write history file for inferior PHP PROCESS."
  ;; Depending on how the process is killed the buffer may not be
  ;; around anymore; e.g. `kill-buffer'.
  (when-let* ((buffer (process-buffer process))
	      ((buffer-live-p (process-buffer process))))
    (with-current-buffer buffer (comint-write-input-ring))))

(defun php-ts-mode-send-region (beg end)
  "Send region between BEG and END to the inferior PHP process."
  (interactive "r")
  (if-let* ((php-process
	     (get-buffer-process php-ts-mode-inferior-php-buffer)))
      (progn
	(php-ts-mode-show-process-buffer)
	(comint-send-string php-process "\n")
	(comint-send-string
	 php-process
	 (buffer-substring-no-properties beg end))
	(comint-send-string php-process "\n"))
    (message "Invoke run-php first!")))

(defun php-ts-mode-send-buffer ()
  "Send current buffer to the inferior PHP process."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "<?php" nil t)
    (php-ts-mode-send-region (point) (point-max))))

(defun php-ts-mode-send-file (file)
  "Send contents of FILE to the inferior PHP process."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents-literally file)
    (search-forward "<?php" nil t)
    (php-ts-mode-send-region (point) (point-max))))

(defun php-ts-mode-show-process-buffer ()
  "Show the inferior PHP process buffer."
  (interactive)
  (display-buffer php-ts-mode-inferior-php-buffer))

(defun php-ts-mode-hide-process-buffer ()
  "Hide the inferior PHP process buffer."
  (interactive)
  (delete-windows-on php-ts-mode-inferior-php-buffer))

(defun php-ts-mode-kill-process ()
  "Kill the inferior PHP process."
  (interactive)
  (with-current-buffer php-ts-mode-inferior-php-buffer
    (kill-buffer-and-window)))

;;;###autoload
(defun php-ts-mode-maybe ()
  "Enable `php-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'php)
          (eq treesit-enabled-modes t)
          (memq 'php-ts-mode treesit-enabled-modes))
      (php-ts-mode)
    (fundamental-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list
   'auto-mode-alist '("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-ts-mode-maybe))
  (add-to-list
   'auto-mode-alist '("\\.\\(?:php\\|inc\\|stub\\)\\'" . php-ts-mode-maybe))
  (add-to-list
   'auto-mode-alist '("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-ts-mode-maybe))
  (add-to-list
   'interpreter-mode-alist
   (cons "php\\(?:-?[34578]\\(?:\\.[0-9]+\\)*\\)?" 'php-ts-mode-maybe))
  ;; To be able to toggle between an external package and core ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(php-mode . php-ts-mode)))

(provide 'php-ts-mode)
;;; php-ts-mode.el ends here
