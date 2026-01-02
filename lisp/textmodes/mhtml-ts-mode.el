;;; mhtml-ts-mode.el --- Major mode for HTML using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author: Vincenzo Pupillo <v.pupillo@gmail.com>
;; Maintainer: Vincenzo Pupillo <v.pupillo@gmail.com>
;; Created: Nov 2024
;; Keywords: HTML languages hypermedia tree-sitter

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
;;
;; This package provides `mhtml-ts-mode' which is a major mode
;; for editing HTML files with embedded JavaScript and CSS.
;; Tree Sitter is used to parse each of these languages.
;;
;; Please note that this package requires `html-ts-mode', which
;; registers itself as the major mode for editing HTML.
;;
;; This package is compatible and has been tested with the following
;; tree-sitter grammars:
;; * https://github.com/tree-sitter/tree-sitter-html
;; * https://github.com/tree-sitter/tree-sitter-javascript
;; * https://github.com/tree-sitter/tree-sitter-jsdoc
;; * https://github.com/tree-sitter/tree-sitter-css
;;
;; Features
;;
;; * Indent
;; * Flymake
;; * IMenu
;; * Navigation
;; * Which-function
;; * Tree-sitter parser installation helper

;;; Code:

(require 'treesit)
(require 'html-ts-mode)
(require 'css-mode) ;; for embed css into html
(require 'js) ;; for embed javascript into html

(eval-when-compile
  (require 'rx))

;; This tells the byte-compiler where the functions are defined.
;; Is only needed when a file needs to be able to byte-compile
;; in a Emacs not built with tree-sitter library.
(treesit-declare-unavailable-functions)

(defun mhtml-ts-mode-install-parsers ()
  "Install all the required treesitter parsers.
`treesit-language-source-alist' defines which parsers to install.
It's pre-filled by loading \"html-ts-mode\", \"css-mode\", \"js\"."
  (interactive)
  (dolist (lang '(html css javascript jsdoc))
    (treesit-install-language-grammar lang)))

;;; Custom variables

(defgroup mhtml-ts-mode nil
  "Major mode for editing HTML files, based on `html-ts-mode'.
Works with JS and CSS and for that use `js-ts-mode' and `css-ts-mode'."
  :prefix "mhtml-ts-mode-"
  ;; :group 'languages
  :group 'html)

(defcustom mhtml-ts-mode-js-css-indent-offset 2
  "JavaScript and CSS indent spaces related to the <script> and <style> HTML tags.
By default should have same value as `html-ts-mode-indent-offset'."
  :tag "HTML javascript or css indent offset"
  :version "31.1"
  :type 'integer
  :safe 'integerp)

(defcustom mhtml-ts-mode-pretty-print-command
  ;; prefer tidy because it's used by sgml-mode
  (let ((executable nil))
    (cond ((setq executable (executable-find "tidy"))
           (format
            "%s --gnu-emacs yes --wrap 0 --indent-spaces %s -q -i -"
            executable html-ts-mode-indent-offset))
          ((setq executable (executable-find "xmllint"))
           (format "%s --html --quiet --format -" executable))
          (t "Install tidy, or some other HTML pretty print tool, and set `mhtml-ts-mode-pretty-print-command'.")))
  "The command to pretty print the current HTML buffer."
  :type 'string
  :version "31.1")

(defvar mhtml-ts-mode--js-css-indent-offset
  mhtml-ts-mode-js-css-indent-offset
  "Internal copy of `mhtml-ts-mode-js-css-indent-offset'.
The value changes, by `mhtml-ts-mode--tag-relative-indent-offset' according to
the value of `mhtml-ts-mode-tag-relative-indent'.")

(defun mhtml-ts-mode--tag-relative-indent-offset (sym val)
  "Custom setter for `mhtml-ts-mode-tag-relative-indent'.
Apart from setting the default value of SYM to VAL, also change the
value of SYM in `mhtml-ts-mode' buffers to VAL.  SYM should be
`mhtml-ts-mode-tag-relative-indent', and VAL should be t, nil or
`ignore'.  When sym is `mhtml-ts-mode-tag-relative-indent' set the
value of `mhtml-ts-mode--js-css-indent-offset' to 0 if VAL is t,
otherwise to `mhtml-ts-mode-js-css-indent-offset'."
  (set-default sym val)
  (when (eq sym 'mhtml-ts-mode-tag-relative-indent)
    (setq
     mhtml-ts-mode--js-css-indent-offset
     (if (eq val t)
         mhtml-ts-mode-js-css-indent-offset
       0))))

(defcustom mhtml-ts-mode-tag-relative-indent t
  "How <script> and <style> bodies are indented relative to the tag.

When t, indentation looks like:

  <script>
    code();
  </script>

When nil, indentation of the tag body starts just below the
tag, like:

  <script>
  code();
  </script>

When `ignore', the tag body starts in the first column, like:

  <script>
code();
  </script>"
  :type '(choice (const nil) (const t) (const ignore))
  :safe 'symbolp
  :set #'mhtml-ts-mode--tag-relative-indent-offset
  :version "31.1")

(defcustom mhtml-ts-mode-css-fontify-colors t
  "Whether CSS colors should be fontified using the color as the background.
If non-nil, text representing a CSS color will be fontified
such that its background is the color itself.
Works like `css--fontify-region'."
  :tag "HTML colors the CSS properties values."
  :version "31.1"
  :type 'boolean
  :safe 'booleanp)

(defvar mhtml-ts-mode-saved-pretty-print-command nil
  "The command last used to pretty print in this buffer.")

(defun mhtml-ts-mode-pretty-print (command)
  "Prettify the current buffer.
Argument COMMAND The command to use."
  (interactive
   (list (read-string
          "Prettify command: "
          (or mhtml-ts-mode-saved-pretty-print-command
              (concat mhtml-ts-mode-pretty-print-command " ")))))
  (setq mhtml-ts-mode-saved-pretty-print-command command)
  (save-excursion
    (shell-command-on-region
     (point-min) (point-max)
     command (buffer-name) t
     "*mhtml-ts-mode-pretty-pretty-print-errors*" t)))

(defun mhtml-ts-mode--switch-fill-defun (&rest arguments)
  "Switch between `fill-paragraph' and `prog-fill-reindent-defun'.
In an HTML region it calls `fill-paragraph' as does `html-ts-mode',
otherwise it calls `prog-fill-reindent-defun'.
Optional ARGUMENTS to to be passed to it."
  (interactive)
  (if (eq (treesit-language-at (point)) 'html)
      (funcall-interactively #'fill-paragraph arguments)
    (funcall-interactively #'prog-fill-reindent-defun arguments)))

(defvar-keymap mhtml-ts-mode-map
  :doc "Keymap for `mhtml-ts-mode' buffers."
  :parent html-mode-map
  ;; `mhtml-ts-mode' derive from `html-ts-mode' so the keymap is the
  ;; same, we need to add some mapping from others languages.
  "C-c C-f" #'css-cycle-color-format
  "M-q" #'mhtml-ts-mode--switch-fill-defun)

;; Place the CSS menu in the menu bar as well.
(easy-menu-define mhtml-ts-mode-menu mhtml-ts-mode-map
  "Menu bar for `mhtml-ts-mode'."
  css-mode--menu)

;; Custom font-lock function that's used to apply color to css color
;; The signature of the function should be conforming to signature
;; QUERY-SPEC required by `treesit-font-lock-rules'.
(defun mhtml-ts-mode--colorize-css-value (node override start end &rest _)
  "Colorize CSS property value like `css--fontify-region'.
For NODE, OVERRIDE, START, and END, see `treesit-font-lock-rules'."
  (let ((node-start (treesit-node-start node))
	(node-end (treesit-node-end node)))
    (treesit-fontify-with-override
     node-start node-end
     'font-lock-variable-name-face
     override start end)
    ;; apply color if required
    (when-let* ((ok (and mhtml-ts-mode-css-fontify-colors
			 (member (treesit-node-type node) '("plain_value" "color_value"))))
		(color (css--compute-color start (treesit-node-text node t))))
	(with-silent-modifications
	  (add-text-properties
	   node-start node-end
	   (list 'face (list :background color
                             :foreground (readable-foreground-color
					  color)
                             :box '(:line-width -1))))))))

;; Embedded languages should be indented according to the language
;; that embeds them.
;; This function signature complies with `treesit-simple-indent-rules'
;; ANCHOR.
(defun mhtml-ts-mode--js-css-tag-bol (_node _parent &rest _)
  "Find the first non-space characters of html tags <script> or <style>.
Return `line-beginning-position' when `treesit-node-at' is html, or
`mhtml-ts-mode-tag-relative-indent' is equal to ignore.
NODE and PARENT are ignored."
  (if (or (eq (treesit-language-at (point)) 'html)
          (eq mhtml-ts-mode-tag-relative-indent 'ignore))
      (line-beginning-position)
    ;; Ok, we are in js or css block.
    (save-excursion
      (re-search-backward "<script.*>\\|<style.*>" nil t))))

;; Treesit supports 4 level of decoration, `treesit-font-lock-level'
;; define which level to use.  Major modes categorize their fontification
;; features, these categories are defined by `treesit-font-lock-rules' of
;; each major-mode using :feature keyword.
;; In a multiple language Major mode it's a good idea to provide, for each
;; level, the union of the :feature of the same level.
;; TODO: Since the feature-list is not defined per "parser" (like, for
;; example, the thing-settings), the same feature can appear in
;; different levels, so the appearance of a multiple main mode can be
;; different from the main mode used.  For e.g the feature "function" is
;; at level 4 for Javascript while it is at level 3 for CSS.
(defvar mhtml-ts-mode--treesit-font-lock-feature-list
  (treesit-merge-font-lock-feature-list
   html-ts-mode--treesit-font-lock-feature-list
   (treesit-merge-font-lock-feature-list
    js--treesit-font-lock-feature-list
    css--treesit-font-lock-feature-list))
  "Settings for `treesit-font-lock-feature-list'.")

(defun mhtml-ts-mode--treesit-font-lock-settings ()
  "Return tree-sitter font-lock settings for `mhtml-ts-mode'."
  (append html-ts-mode--font-lock-settings
          (js--treesit-font-lock-settings)
          ;; Let's replace a css rule with a new one that adds
          ;; color to the css value.
          (treesit-replace-font-lock-feature-settings
           (treesit-font-lock-rules
            :language 'css
            :override t
            :feature 'variable
            '((plain_value) @mhtml-ts-mode--colorize-css-value
              (color_value) @mhtml-ts-mode--colorize-css-value))
           css--treesit-settings)))

(defvar mhtml-ts-mode--treesit-thing-settings
  ;; In addition to putting together the various definitions, we need to
  ;; add 'defun' which is used to support `imenu' and 'which-function'.
  (list
   ;; HTML thing settings
   (append
    (car html-ts-mode--treesit-things-settings)
    `((defun ,(regexp-opt (list html-ts-mode--treesit-defun-type-regexp)))))
   ;; Javascript thing settings
   (append
    (car js--treesit-thing-settings)
    `((defun ,js--treesit-defun-type-regexp)))
   ;; CSS thing settings
   (append
    (car css--treesit-thing-settings)
    `((defun ,css--treesit-defun-type-regexp))))
  "Settings for `treesit-thing-settings'.")

(defun mhtml-ts-mode--treesit-indent-rules ()
  "Return tree-sitter indent rules for `mhtml-ts-mode'."
  (treesit--indent-rules-optimize
   (append html-ts-mode--indent-rules
           ;; Extended rules for js and css, to indent
           ;; appropriately when injected into html
           (treesit-simple-indent-modify-rules
            'javascript
            `((javascript ((parent-is "program")
                           mhtml-ts-mode--js-css-tag-bol
                           mhtml-ts-mode--js-css-indent-offset)))
            (js--treesit-indent-rules)
            :replace)
           (treesit-simple-indent-modify-rules
            'css
            `((css ((parent-is "stylesheet")
                    mhtml-ts-mode--js-css-tag-bol
                    mhtml-ts-mode--js-css-indent-offset)))
            css--treesit-indent-rules
	    :prepend))))

(defvar mhtml-ts-mode--treesit-aggregated-simple-imenu-settings
  `((html ,@html-ts-mode--treesit-simple-imenu-settings)
    (javascript ,@js--treesit-simple-imenu-settings)
    (css ,@css--treesit-simple-imenu-settings))
  "Settings for `treesit-simple-imenu'.")

;; TODO: treesit-defun-type-regexp should have an aggregated version,
;; like treesit-aggregated-simple-imenu-settings. Otherwise we can't
;; reuse the regex defined in the major mode we use.
(defvar mhtml-ts-mode--treesit-defun-type-regexp
  (regexp-opt '("class_declaration"
                "method_definition"
                "function_declaration"
                "lexical_declaration"
                "element"
                "rule_set"
		"keyframe_block"))
  "Settings for `treesit-defun-type-regexp'.")

;; In order to support `prettify-symbols-mode', just `append' the prettify
;; alist of all the languages. In our case only javascript defined this alist.
(defvar mhtml-ts-mode--prettify-symbols-alist js--prettify-symbols-alist
  "Alist of symbol prettifications for various supported languages.")

;; In order to support `which-fuction-mode' we should define
;; a function that return the defun name.
;; In a multilingual treesit mode, this can be implemented simply by
;; calling language-specific functions.
(defun mhtml-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (let ((lang (treesit-node-language node)))
    (cond
     ((eq lang 'html) (html-ts-mode--defun-name node))
     ((eq lang 'javascript) (js--treesit-defun-name node))
     ((eq lang 'css) (css--treesit-defun-name node)))))

(defvar-local mhtml-ts-mode--comment-current-lang nil)

(defun mhtml-ts-mode--comment-setup ()
  (let ((lang (treesit-language-at (point))))
    (unless (eq mhtml-ts-mode--comment-current-lang lang)
      (setq mhtml-ts-mode--comment-current-lang lang)
      (pcase lang
        ('html
         (setq-local comment-start "<!-- ")
         (setq-local comment-start-skip nil)
         (setq-local comment-end " -->")
         (setq-local comment-end-skip nil))
        ('css
         (setq-local comment-start "/*")
         (setq-local comment-start-skip "/\\*+[ \t]*")
         (setq-local comment-end "*/")
         (setq-local comment-end-skip "[ \t]*\\*+/"))
        ('javascript
         (c-ts-common-comment-setup))))))

;;; Flymake integration

(defvar-local mhtml-ts-mode--flymake-process nil
  "Store the Flymake process.")

(defun mhtml-ts-mode-flymake-mhtml (report-fn &rest _args)
  "MHTML backend for Flymake.
Calls REPORT-FN directly.  Requires tidy."
  (when (process-live-p mhtml-ts-mode--flymake-process)
    (kill-process mhtml-ts-mode--flymake-process))
  (let ((tidy (executable-find "tidy"))
        (source (current-buffer))
        (diagnostics-pattern (eval-when-compile
                               (rx bol
                                   "line " (group (+ num))    ;; :1 line
                                   " column " (group (+ num)) ;; :2 column
                                   " - " (group (+? nonl))    ;; :3 type
                                   ": " (group (+? nonl))     ;; :4 msg
                                   eol))))
    (if (not tidy)
        (error "Unable to find tidy command")
      (save-restriction
        (widen)
        (setq mhtml-ts-mode--flymake-process
              (make-process
               :name "mhtml-ts-mode-flymake"
               :noquery t
               :connection-type 'pipe
               :buffer (generate-new-buffer "*mhtml-ts-mode-flymake*")
               :command `(,tidy "--gnu-emacs" "yes" "-e" "-q")
               :sentinel
               (lambda (proc _event)
                 (when (eq 'exit (process-status proc))
                   (unwind-protect
                       (if (with-current-buffer source
                             (eq proc mhtml-ts-mode--flymake-process))
                           (with-current-buffer (process-buffer proc)
                             (goto-char (point-min))
                             (let (diags)
                               (while (search-forward-regexp diagnostics-pattern nil t)
                                 (let* ((pos
                                         (flymake-diag-region
                                          source
                                          (string-to-number (match-string 1))
                                          (string-to-number (match-string 2)))) ;; line and column
                                        (type (cond ((equal (match-string 3) "Warning") :warning)
                                                    ((equal (match-string 3) "Error") :error))) ;; type of message
                                        (msg (match-string 4))) ;; message
                                   (push (flymake-make-diagnostic source (car pos) (cdr pos) type msg)
                                         diags)))
                               (funcall report-fn diags)))
                         (flymake-log :warning "Canceling obsolete check %s" proc))
                     (kill-buffer (process-buffer proc)))))))
        (process-send-region mhtml-ts-mode--flymake-process (point-min) (point-max))
        (process-send-eof mhtml-ts-mode--flymake-process)))))

(defvar mhtml-ts-mode--range-settings
  (append
   (treesit-range-rules
    :embed 'javascript
    :host 'html
    '((script_element
       (start_tag (tag_name))
       (raw_text) @cap))

    ;; Another rule could be added that when it matches an
    ;; attribute_value that has as its parent an
    ;; attribute_name "style" it captures it and then
    ;; passes it to the css parser.
    :embed 'css
    :host 'html
    '((style_element
       (start_tag (tag_name))
       (raw_text) @cap)))

   ;; jsdoc is not mandatory for js-ts-mode, so we respect this by
   ;; adding jsdoc range rules only when jsdoc is available.
   (when (and (fboundp 'treesit-language-available-p)
              (treesit-language-available-p 'jsdoc t))
     (treesit-range-rules
      :embed 'jsdoc
      :host 'javascript
      :local t
      `(((comment) @cap
         (:match ,js--treesit-jsdoc-beginning-regexp @cap)))))))

(defvar mhtml-ts-mode--treesit-aggregated-outline-predicate
  `((html . ,#'html-ts-mode--outline-predicate)
    (javascript . ,js-ts-mode--outline-predicate)
    (css . ,css-ts-mode--outline-predicate))
  "Settings for `treesit-aggregated-outline-predicate'.")

;;;###autoload
(define-derived-mode mhtml-ts-mode html-ts-mode
  '("HTML+" (:eval (let ((lang (treesit-language-at (point))))
                     (cond ((eq lang 'html) "")
                           ((eq lang 'javascript) "JS")
                           ((eq lang 'css) "CSS")))))
  "Major mode for editing HTML with embedded JavaScript and CSS.
Powered by tree-sitter."
  (if (not (and
            (treesit-ensure-installed 'html)
            (treesit-ensure-installed 'javascript)
            (treesit-ensure-installed 'css)))
      (error "Tree-sitter parsers for HTML isn't available.  You can
    install the parsers with M-x `mhtml-ts-mode-install-parsers'")

    ;; When an language is embedded, you should initialize some variable
    ;; just like it's done in the original mode.

    ;; Comment.
    (setq-local comment-multi-line t)
    (setq-local comment-setup-function #'mhtml-ts-mode--comment-setup)

    ;; Font-lock.

    ;; There are two ways to handle embedded code:
    ;; 1. Use a single parser for all the embedded code in the buffer. In
    ;; this case, the embedded code blocks are concatenated together and are
    ;; seen as a single continuous document to the parser.
    ;; 2. Each embedded code block gets its own parser. Each parser only sees
    ;; that particular code block.

    ;; If you go with 2 for a language, the local parsers are created and
    ;; destroyed automatically by Emacs. So don't create a global parser for
    ;; that embedded language here.

    ;; Create the parsers, only the global ones.
    ;; jsdoc is a local parser, don't create a parser for it.
    (treesit-parser-create 'css)
    (treesit-parser-create 'javascript)

    ;; Multi-language modes must set the  primary parser.
    (setq-local treesit-primary-parser (treesit-parser-create 'html))

    (setq-local treesit-range-settings mhtml-ts-mode--range-settings)

    ;; jsdoc is not mandatory for js-ts-mode, so we respect this by
    ;; adding jsdoc range rules only when jsdoc is available.
    (when (treesit-ensure-installed 'jsdoc)
      (setq-local c-ts-common--comment-regexp
                  js--treesit-jsdoc-comment-regexp))

    (setq-local prettify-symbols-alist mhtml-ts-mode--prettify-symbols-alist)

    ;; Indent.

    ;; Since `mhtml-ts-mode' inherits indentation rules from `html-ts-mode', `js-ts-mode'
    ;; and `css-ts-mode', if you want to change the offset you have to act on the
    ;; *-offset variables defined for those languages.

    ;; JavaScript and CSS must be indented relative to their code block.
    ;; This is done by inserting a special rule before the normal
    ;; indentation rules of these languages.
    ;; The value of `mhtml-ts-mode--js-css-indent-offset' changes based on
    ;; `mhtml-ts-mode-tag-relative-indent' and can be used to indent
    ;; JavaScript and CSS code relative to the HTML that contains them,
    ;; just like in mhtml-mode.
    (setq-local treesit-simple-indent-rules
                (mhtml-ts-mode--treesit-indent-rules))

    ;; Navigation.

    ;; This is for which-function-mode.
    ;; Since mhtml-ts-mode is derived from html-ts-mode, which sets
    ;; the value of `treesit-defun-type-regexp', you have to reset it to nil
    ;; otherwise `imenu' and `which-function-mode' will not work.
    (setq-local treesit-defun-type-regexp nil)

    ;; This is for finding defun name, it's used by IMenu as default
    ;; function no specific functions are defined.
    (setq-local treesit-defun-name-function #'mhtml-ts-mode--defun-name)

    ;; Define what are 'thing' for treesit.
    ;; 'Thing' is a symbol representing the thing, like `defun', `sexp', or
    ;; `sentence'.
    ;; As an alternative, if you want just defun, you can define a `treesit-defun-type-regexp'.
    (setq-local treesit-thing-settings mhtml-ts-mode--treesit-thing-settings)

    ;; Font-lock.

    ;; In a multi-language scenario, font lock settings are usually a
    ;; concatenation of language rules. As you can see, it is possible
    ;; to extend/modify the default rule or use a different set of
    ;; rules. See `php-ts-mode--custom-html-font-lock-settings' for more
    ;; advanced usage.
    (setq-local treesit-font-lock-settings (mhtml-ts-mode--treesit-font-lock-settings))

    ;; Tells treesit the list of features to fontify.
    (setq-local treesit-font-lock-feature-list mhtml-ts-mode--treesit-font-lock-feature-list)

    ;; Imenu

    ;; Setup Imenu: if no function is specified, try to find an object
    ;; using `treesit-defun-name-function'.
    (setq-local treesit-aggregated-simple-imenu-settings
                mhtml-ts-mode--treesit-aggregated-simple-imenu-settings)

    (setq-local treesit-aggregated-outline-predicate
		mhtml-ts-mode--treesit-aggregated-outline-predicate)

    (treesit-major-mode-setup)

    ;; This is sort of a prog-mode as well as a text mode.
    (run-mode-hooks 'prog-mode-hook)

    ;; Flymake
    (add-hook 'flymake-diagnostic-functions #'mhtml-ts-mode-flymake-mhtml nil 'local)))

;; Add some extra parents.
(derived-mode-add-parents 'mhtml-ts-mode '(css-mode js-mode))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'treesit-major-mode-remap-alist
               '(mhtml-mode . mhtml-ts-mode)))

(provide 'mhtml-ts-mode)
;;; mhtml-ts-mode.el ends here
