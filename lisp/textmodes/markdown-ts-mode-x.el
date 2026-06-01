;;; markdown-ts-mode-x.el --- markdown-ts-mode extras -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author           : Rahul Martim Juliato <rahul.juliato@gmail.com>
;;                  : Stéphane Marks <shipmints@gmail.com>
;; Maintainer       : Rahul Martim Juliato <rahul.juliato@gmail.com>
;; Created          : April 2026
;; Version          : 1.0
;; Package-Requires : ((emacs "31.1"))
;; Keywords         : markdown md text edit languages tree-sitter

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

;; This is an experimental mode that has a number of unresolved issues.

;;; Code:

(require 'markdown-ts-mode)

;;; Convenience:

(defconst markdown-ts-commonmark-spec-url
  "https://spec.commonmark.org/current/")

(defconst markdown-ts-gfm-spec-url
  "https://github.github.com/gfm/")

(defun markdown-ts-browse-commonmark-spec ()
  "Browse the CommonMark Markdown spec in a web browser."
  (interactive)
  (browse-url markdown-ts-commonmark-spec-url))

(defun markdown-ts-browse-gfm-spec ()
  "Browse the GitHub Flavored Markdown spec in a web browser."
  (interactive)
  (browse-url markdown-ts-gfm-spec-url))

;;; Export and preview:

(defgroup markdown-ts-convert nil
  "Convert `markdown-ts-mode' buffers or files."
  :prefix "markdown-ts-convert-"
  :group 'text
  :group 'editing
  :version "31.1")

(defcustom markdown-ts-default-converter nil
  "Default Markdown format and converter.
If nil, you will be prompted for a format and a converter.
See `markdown-ts-converters'."
  :type '(choice (const :tag "Prompt for format and converter" nil)
                 (const :tag "PDF via Pandoc" (pdf . pandoc))
                 (const :tag "HTML via Pandoc" (html . pandoc))
                 (const :tag "HTML via cmark" (html . cmark))
                 (const :tag "HTML via cmark-gfm (GitHub bells and whistles)" (html . cmark-gfm))
                 (const :tag "HTML via markdown" (html . markdown))
                 (const :tag "HTML via markdown.pl" (html . markdown.pl))
                 (cons  :tag "Format and Converter"
                        (symbol :tag "Format")
                        (symbol :tag "Converter")))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-convert-display-function #'eww-open-file
  "Function to display converted Markdown the output file.
The function is called with one argument, the file name."
  :type '(choice (const :tag "Display using `eww'" eww-open-file)
                 (const :tag "Display in a browser" browse-url-of-file)
                 (const :tag "Display output file in a buffer" find-file-other-window)
                 (function :tag "Other function"))
  :version "31.1"
  :package-version "1.0")

(defconst markdown-ts--convert-error-buffer
  "*markdown-ts-convert errors*")

(defconst markdown-ts--convert-output-buffer
  "*markdown-ts-convert output*")

(treesit-declare-unavailable-functions)

(defvar markdown-ts-converters
  (list
   (cons 'html
         (list
          (cons 'pandoc
                (list :command "pandoc"
                      :input '(file stdin)
                      :output '(file)
                      :arguments-function
                      (lambda (input-file output-file)
                        (append (list "-f" "gfm"
                                      "-t" "html5"
                                      "--standalone"
                                      "-o" output-file)
                                (when input-file
                                  (list input-file))))))
          (cons 'cmark
                (list :command "cmark"
                      :input '(file stdin)
                      :output '(stdout)
                      :arguments-function
                      (lambda (input-file _output-file)
                        (append (list "-t" "html")
                                (when input-file
                                  (list input-file))))))
          (cons 'cmark-gfm
                (list :command "cmark-gfm"
                      :input '(file stdin)
                      :output '(stdout)
                      :arguments-function
                      (lambda (input-file _output-file)
                        (append (list "-t" "html"
                                      ;; Use GitHub-style <pre lang> for code blocks
                                      "--github-pre-lang"
                                      ;; Include remainder of code block info
                                      ;; string in a separate attribute.
                                      "--full-info-string"
                                      ;; Use style attributes to align table
                                      ;; cells instead of align attributes.
                                      "--table-prefer-style-attributes"
                                      "--extension" "footnotes"
                                      "--extension" "table"
                                      "--extension" "strikethrough"
                                      "--extension" "autolink"
                                      "--extension" "tagfilter"
                                      "--extension" "tasklist")
                                (when input-file
                                  (list input-file))))))
          ;; 'markdown.pl is the same as 'markdown with another common file
          ;; name.
          (cons 'markdown.pl
                (list :command "markdown.pl"
                      :input '(file stdin)
                      :output '(stdout)
                      :arguments-function
                      (lambda (input-file _output-file)
                        (append (list "--html4tags")
                                (when input-file
                                  (list "--" input-file))))))
          ;; 'markdown is the same as 'markdown.pl with another common file
          ;; name.
          (cons 'markdown
                (list :command "markdown"
                      :input '(file stdin)
                      :output '(stdout)
                      :arguments-function
                      (lambda (input-file _output-file)
                        (append (list "--html4tags")
                                (when input-file
                                  (list "--" input-file))))))))
   (cons 'pdf
         (list
          (cons 'pandoc
                (list :command "pandoc"
                      :input '(file stdin)
                      :output '(file)
                      :arguments-function
                      (lambda (input-file output-file)
                        (append (list "-f" "gfm"
                                      "-t" "pdf"
                                      "-o" output-file)
                                (when input-file
                                  (list input-file)))))))))
  "An alist of format/converter configurations.
It is a two level alist with the first level being format, for example
`html' or `pdf', and the second being the converter name, for example
`pandoc'.  A converter configuration entry is a plist of the form:

  \\='(:command \"command name\"
    :input (file stdin) ; accepts file name or stdin
    :output (stdout)    ; produces stdout only
    :arguments-function ; function; e.g.,
     (lambda (input-file _output-file)
       (append (list \"-t\" \"html\")
               (when input-file
                 (list input-file)))))

`arguments-function' is passed two arguments, INPUT-FILE and
OUTPUT-FILE.  If INPUT-FILE is nil, assume stdin, or it is a
fully-qualified file name.  If OUTPUT-FILE is nil, assume stdout, or it
is a fully-qualified file name.  It should return a list of
arguments suitable for `call-process'.")

(defun markdown-ts-convert-file (input-file
                                 &optional
                                 format
                                 output-file
                                 display overwrite quiet)
  "Convert Markdown INPUT-FILE to FORMAT.
If optional DISPLAY is non-nil, show the output file in a buffer, if
possible, using the function `markdown-ts-convert-display-function'.

With a prefix argument, DISPLAY will be non-nil.

Optional FORMAT is a format/converter pair specified as a cons
\\='(format . converter) for example \\='(html . pandoc).  See
`markdown-ts-converters'.  If nil, prompt for the format and converter.

If optional OVERWRITE is non-nil, silently overwrite OUTPUT-FILE if it
exists.

If QUIET is non-nil, inhibit showing conversion warnings or errors.

The external executable specified by a converter must be installed and
found; see the variable `exec-path'."
  (interactive "fConvert Markdown file: ")
  (markdown-ts-convert input-file output-file
                       format
                       (or display current-prefix-arg)
                       overwrite
                       quiet))

(defun markdown-ts-convert (&optional
                            input-file output-file
                            format display overwrite quiet)
  "Convert a `markdown-ts-mode' buffer or file to FORMAT.
Convert INPUT-FILE to OUTPUT-FILE.

If INPUT-FILE is nil, use the current buffer.

If OUTPUT-FILE is nil, prompt for an output file.

Optional FORMAT is a format/converter pair specified as a cons
\\='(format . converter) for example \\='(html . pandoc).  See
`markdown-ts-converters'.  If nil, prompt for the format and converter.

If optional DISPLAY is non-nil, show the output file in a buffer, if
possible, using the function `markdown-ts-convert-display-function'.

With a prefix argument, DISPLAY will be non-nil.

If optional OVERWRITE is non-nil, silently overwrite OUTPUT-FILE if it
exists.

If QUIET is non-nil, inhibit showing conversion warnings or errors.

The external executable specified by a converter must be installed and
found; see the variable `exec-path'."
  (interactive)
  (when (and input-file
             (equal buffer-file-name (expand-file-name input-file))
             (buffer-modified-p))
    (when (yes-or-no-p "Save modified buffer before converting? ")
      (save-buffer)))
  (unless input-file
    (markdown-ts--barf-if-not-mode 'markdown-ts-convert))

  (setq display (or display current-prefix-arg))

  (let* ((converter-format
          (or (car format)
              (car markdown-ts-default-converter)
              (intern
               (completing-read
                "Select an output format: "
                (mapcar (lambda (elt) (symbol-name (car elt)))
                        markdown-ts-converters)
                nil
                t
                nil
                'markdown-ts-converter-format))))
         (converter
          (or (cdr format)
              (cdr markdown-ts-default-converter)
              (intern
               (completing-read
                "Select a converter: "
                (mapcar (lambda (elt) (symbol-name (car elt)))
                        (alist-get converter-format markdown-ts-converters))
                nil
                t
                nil
                'markdown-ts-converter))))
         (config (alist-get converter
                            (alist-get converter-format
                                       markdown-ts-converters)))
         (command (plist-get config :command))
         (command-input (plist-get config :input))
         (command-output (plist-get config :output))
         (stdin-only (equal command-input '(stdin)))
         (stdout-only (equal command-output '(stdout))))
    (unless output-file
      (setq output-file
            (read-file-name "Output file name: "
                            nil
                            nil
                            nil
                            (concat (file-name-sans-extension
                                     (file-relative-name
                                      (or input-file buffer-file-name)))
                                    "."
                                    (symbol-name converter-format))))
      (unless output-file
        (user-error "Output file name is required")))
    (when output-file
      (when (and (not overwrite)
                 (file-exists-p output-file)
                 (not (yes-or-no-p (format "%s exists; overwrite?" output-file))))
        (user-error "Not overwriting existing file: %s" output-file))
      (setq output-file (expand-file-name output-file)))


    (let ((command-args (funcall (plist-get config :arguments-function)
                                 (when input-file (expand-file-name input-file))
                                 output-file)))
      (unless (executable-find command)
        (user-error "`%s' executable not found" command))
      (let ((error-buffer (get-buffer-create
                           markdown-ts--convert-error-buffer))
            (error-temp-file (make-temp-file "markdown-ts-convert-stderr"
                                             nil ".log"))
            (stdout-buffer (when stdout-only
                             (get-buffer-create
                              markdown-ts--convert-output-buffer))))
        (unwind-protect
            (let (exit-status)
              (with-current-buffer error-buffer
                (special-mode)
                (setq buffer-read-only nil)
                (widen)
                (erase-buffer))
              (when stdout-buffer
                (with-current-buffer stdout-buffer
                  (special-mode)
                  (setq buffer-read-only nil)
                  (widen)
                  (erase-buffer)))
              (cond
               ((and input-file (not stdin-only))
                (setq exit-status
                      (apply #'call-process
                             command
                             nil
                             (list stdout-buffer error-temp-file)
                             nil
                             command-args)))
               ((and input-file stdin-only)
                (with-temp-buffer
                  (insert-file-contents input-file)
                  (setq exit-status
                        (apply #'call-process-region
                               nil
                               nil
                               command
                               nil
                               (list stdout-buffer error-temp-file)
                               nil
                               command-args))))
               (t
                (without-restriction
                  (setq exit-status
                        (apply #'call-process-region
                               nil
                               nil
                               command
                               nil
                               (list stdout-buffer error-temp-file)
                               nil
                               command-args)))))
              (let ((error-size (file-attribute-size (file-attributes error-temp-file))))
                (when (and error-size (> error-size 0))
                  (with-current-buffer error-buffer
                    (insert-file-contents error-temp-file)
                    (goto-char (point-min)))))
              (cond ((eq 0 exit-status) ; Integer or string.
                     (when stdout-buffer
                       (with-current-buffer stdout-buffer
                         (write-region (point-min) (point-max) output-file)))
                     (when display
                       (funcall markdown-ts-convert-display-function
                                output-file))
                     (when (and (not quiet)
                                (not (zerop (buffer-size error-buffer)))
                                (yes-or-no-p
                                 (format-message
                                  "Conversion file `%s' successfully created with warnings; display them?"
                                  output-file)))
                       (display-buffer error-buffer))
                     (message "Conversion file successfully created `%s'%s"
                              output-file
                              (if (zerop (buffer-size error-buffer))
                                  ""
                                (format " with warnings; see buffer `%s'" error-buffer))))
                    (t
                     (if quiet
                         (error
                          "Converter aborted with exit code %S; see buffer `%s' for errors"
                          exit-status
                          markdown-ts--convert-error-buffer)
                       (when (yes-or-no-p (format
                                           "Converter aborted with exit code %S; display errors?"
                                           exit-status))
                         (display-buffer error-buffer))))))
          (delete-file error-temp-file))))))

;;; Table of contents:

(defgroup markdown-ts-toc nil
  "Table of contents for `markdown-ts-mode' buffers."
  :prefix "markdown-ts-toc-"
  :group 'text
  :group 'editing
  :version "31.1")

(defcustom markdown-ts-toc-generate-warn-if-none 'insert-template
  "Warn if no tables were processed when calling `markdown-ts-toc-generate'.
If nil, do nothing.  If the symbol `insert-template' and
`markdown-ts-toc-generate' was called interactively, offer to insert a
template if there are none.  If `no-tables', warn when there are no
tables in the buffer.  `no-active-tables', is the same as `no-tables'
with the addition of a warning if there are tables, but none are active."
  :type '(choice (const :tag "No warnings" nil)
                 (const :tag "Insert a table template (if interactive)" insert-template)
                 (const :tag "Warn if no tables were processed" no-tables)
                 (const :tag "Warn if no active tables were processed" no-active-tables))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-toc-update-before-save-mode-lighter " [toc on save]"
  "Minor-mode \"lighter\" for `markdown-ts-toc-update-before-save-mode'.
Note the single space prefix."
  :type '(choice (const :tag "Default" " [toc on save]")
                 (const :tag "None" nil)
                 (string :tag "Other string"))
  :version "31.1"
  :package-version "1.0")

(defcustom markdown-ts-toc-slug-function #'markdown-ts--slug-github
  "Function to generate Markdown heading \"slugs\" in tables of contents."
  :type '(choice (const :tag "GitHub slug" markdown-ts--slug-github)
                 (const :tag "Pandoc slug" markdown-ts--slug-pandoc)
                 (function :tag "Other function"))
  :version "31.1"
  :package-version "1.0")

(defvar markdown-ts--toc-handles
  '((heading . (h1 h2 h3 h4 h5 h6))
    (setext . (sh1 sh2))
    (item . (minus plus star))
    (numbered-item . (dot paren))
    (code . (code)))
  "Map high level node handles to their base constituents.
The special symbol `all' means all handles.")

(defvar markdown-ts--toc-handle-classes
  (let ((x))
    (dolist (elt markdown-ts--toc-handles)
      (dolist (val (cdr elt))
        (push `(,val . ,(car elt)) x)))
    x)
"Inverse of `markdown-ts--toc-handles'.")

(defun markdown-ts--toc-expand-candidate-handles (handles)
  "Expand candidate HANDLES and keep only base handles.
See `markdown-ts--toc-handles'."
  (let ((expanded (make-hash-table))
        (res))
    (unless (listp handles)
      (setq handles (list handles)))
    (when (equal handles '(all))
      (setq handles (mapcar #'car markdown-ts--toc-handles)))
    (dolist (elt handles)
      (setq elt (alist-get elt markdown-ts--toc-handles elt))
      (cond ((consp elt)
             (dolist (k elt) (puthash k t expanded)))
            (t (puthash elt t expanded))))
    (maphash (lambda (k _v) (push k res)) expanded)
    res))

(defun markdown-ts--tocs (&optional beg end)
  "Scan the current buffer for toc entries from BEG to END.
If BEG is nil, use `point-min'.  If END is nil, use `point-max'.

Return an alist of the form:
  \\='((keyword (node beg end vars)))

Vars are expanded via `hack-local-variables-prop-line'.

Return the list in the order of appearance in the buffer."
  ;; NOTE: This scans all matching nodes below BEG with no maximum count
  ;; even when we want only a single next matching item.  Perhaps
  ;; `treesit-query-capture' could be improved to accept a max-count
  ;; argument.
  ;;
  ;; Match "markdown-ts-toc:", "markdown-ts-toc-abc123:" but not
  ;; "markdown-ts-tocxxx:".
  (let ((res))
    (dolist (node
             (treesit-query-capture
              'markdown
              '((((html_block) @markdown-ts-toc)
                 (:match "<!--[[:blank:]]*markdown-ts-toc\\(-[[:alnum:]]+\\)*:" @markdown-ts-toc)))
              (or beg (point-min))
              (or end (point-max))
              'node-only))
      (let* ((s (treesit-node-text node 'no-property))
             (match (string-match "\\(markdown-ts-toc\\(-[[:alnum:]]+\\)*\\):.*" s))
             (keyword (when match (match-string 1 s))))
        (let ((vars))
          (with-work-buffer
            (insert s)
            (setq vars (hack-local-variables-prop-line)))
          (push
           `(,keyword
             (node . ,node)
             (beg . ,(treesit-node-start node))
             (end . ,(treesit-node-end node))
             (vars . ,vars))
           res))))
    (nreverse res)))

(defun markdown-ts--tocs-sanity-check (tocs)
  "Sanity check TOCS for paired markdown-ts-toc: markdown-ts-toc-end: nodes.
Signal an error if TOCS are insane.
See `markdown-ts--tocs'."
  (seq-map-indexed
   (lambda (elt idx)
     (when (equal "markdown-ts-toc" (car elt))
       (unless (equal "markdown-ts-toc-end" (car (nth (1+ idx) tocs)))
         (user-error
          "\"markdown-ts-toc\" at position %d must be followed by a matching \"markdown-ts-toc-end\""
          (alist-get 'beg elt)))))
   tocs))

(defun markdown-ts--toc-collect-candidates (toc-start-node
                                            toc-end-node handles from)
  "Return an alist of toc entry candidates from the current buffer.
HANDLES is a list of base handles; see `markdown-ts--toc-handles'.

TOC-START-NODE is the \"markdown-ts-toc-start\" node.
TOC-END-NODE is the \"markdown-ts-toc-end\" node.

If FROM is the symbol `above', source candidates from the beginning of
the buffer until the toc start node.  If `below', source candidates from
toc start node until the end of the buffer.  If `all', use the whole
buffer.

Remove ignored candidates containing \"<!-- markdown-ts-ignore: -->\".

Sort the list by the starting character positions of nodes.

The alist is of the form:
  \\='((handle . node))"
  (let (candidates beg end)
    (pcase from
      ('above
       (setq beg (point-min))
       (setq end (treesit-node-start toc-start-node)))
      ('below
       (setq beg (treesit-node-end toc-end-node))
       (setq end (point-max)))
      ('all
       (setq beg (point-min))
       (setq end (point-max))))

    ;; It looks like we cannot get them in one fell swoop.
    (dolist (elt handles)
      (setq
       candidates
       (append
        candidates
        (pcase elt
          ('h1 (treesit-query-capture
                'markdown '((atx_heading (atx_h1_marker) (inline) @h1)) beg end))
          ('h2 (treesit-query-capture
                'markdown '((atx_heading (atx_h2_marker) (inline) @h2)) beg end))
          ('h3 (treesit-query-capture
                'markdown '((atx_heading (atx_h3_marker) (inline) @h3)) beg end))
          ('h4 (treesit-query-capture
                'markdown '((atx_heading (atx_h4_marker) (inline) @h4)) beg end))
          ('h5 (treesit-query-capture
                'markdown '((atx_heading (atx_h5_marker) (inline) @h5)) beg end))
          ('h6 (treesit-query-capture
                'markdown '((atx_heading (atx_h6_marker) (inline) @h6)) beg end))
          ('sh1 (treesit-query-capture
                 'markdown '((setext_heading heading_content: (_) @sh1 (setext_h1_underline))) beg end))
          ('sh2 (treesit-query-capture
                 'markdown '((setext_heading heading_content: (_) @sh2 (setext_h2_underline))) beg end))
          ('minus (treesit-query-capture
                   'markdown '((list_item (list_marker_minus) (paragraph (inline) @minus))) beg end))
          ('plus (treesit-query-capture
                  'markdown '((list_item (list_marker_plus) (paragraph (inline) @plus))) beg end))
          ('star (treesit-query-capture
                  'markdown '((list_item (list_marker_star) (paragraph (inline) @star))) beg end))
          ('dot (treesit-query-capture
                 'markdown '((list_item (list_marker_dot) (paragraph (inline) @dot))) beg end))
          ('paren (treesit-query-capture
                   'markdown '((list_item (list_marker_parenthesis) (paragraph (inline) @paren))) beg end))
          ('code (treesit-query-capture
                  'markdown '((fenced_code_block (info_string (language)) @code)) beg end))))))
    (sort
     (seq-remove
      (lambda (elt)
        (string-match-p "<!--[[:blank:]]*markdown-ts-ignore:[[:blank:]]*-->"
                        (treesit-node-text (cdr elt) t)))
      candidates)
     :key (lambda (elt)
            (treesit-node-start (cdr elt)))
     :lessp #'<
     :in-place t)))

(defun markdown-ts--toc-list-item-depth (node)
  "Return NODE's item depth relative to its predecessors.
If NODE is not part of a list, return nil."
    (let ((depth nil) (n (treesit-node-parent node)))
      (while n
        (when (equal (treesit-node-type n) "list")
          (setq depth (1+ (or depth 0))))
        (setq n (treesit-node-parent n)))
      depth))

(defun markdown-ts--toc-atx_header-normalize (text)
  "Remove trailing # from TEXT unless they are backslash escaped.
First remove HTML comments."
  (if (string-match-p "[[:blank:]]*\\\\+#+[[:blank:]]*\n*\\'" text)
      text
    (replace-regexp-in-string "[[:blank:]]*#+[[:blank:]]*\n*\\'" "" text)))

(defvar markdown-ts--toc-text-normalizers
  `((heading . markdown-ts--toc-atx_header-normalize)
    (h1 . markdown-ts--toc-atx_header-normalize)
    (h2 . markdown-ts--toc-atx_header-normalize)
    (h3 . markdown-ts--toc-atx_header-normalize)
    (h4 . markdown-ts--toc-atx_header-normalize)
    (h5 . markdown-ts--toc-atx_header-normalize)
    (h6 . markdown-ts--toc-atx_header-normalize))
  "Map node type handles to functions that normalize its text.
See `markdown-ts--toc-text-normalize'.
Each function accepts one argument, the string TEXT, and returns a string.")

(defun markdown-ts--toc-text-normalize (handle node &optional text)
  "Normalize NODE's TEXT according to its HANDLE.
TEXT should have no text properties.  If TEXT is nil, use the NODE's
text.  This is not always correct so it's best to pass TEXT if you know
it in the calling context.

If no normalizer is found, return TEXT.

One example normalization is atx_heading text that ends in trailing hash
signs which are intended to be elided so \"Heading Text ##\" is
transformed into \"Heading Text\"."
  (setq text (or text (treesit-node-text node 'no-property)))
  (if-let* ((fixer (alist-get handle markdown-ts--toc-text-normalizers)))
      (funcall fixer text)
    text))

(define-minor-mode markdown-ts-toc-update-before-save-mode
  "If enabled, update `markdown-ts-mode' tables of contents before saving."
  :init-value nil
  :global nil
  :lighter markdown-ts-toc-update-before-save-mode-lighter
  (if (derived-mode-p 'markdown-ts-mode)
      (if markdown-ts-toc-update-before-save-mode
          (add-hook 'before-save-hook #'markdown-ts-toc-generate nil 'local)
        (remove-hook 'before-save-hook #'markdown-ts-toc-generate 'local))
    (setq markdown-ts-toc-update-before-save-mode nil)
    (lwarn 'markdown-ts-toc-update-before-save-mode
           :warning
           "Minor mode valid only in `markdown-ts-mode' buffers.")))

(defun markdown-ts-toc-clear-and-remove (&optional beg end)
  "Remove `markdown-ts-mode' table of contents bodies and templates.
Operate on the active region BEG to END, otherwise operate on the
buffer, which may be narrowed."
  (interactive "R")
  (markdown-ts--barf-if-not-mode 'markdown-ts-toc-clear-and-remove)
  (markdown-ts-toc-clear beg end 'remove))

(defun markdown-ts-toc-clear (&optional beg end remove)
  "Clear `markdown-ts-mode' table of contents bodies.
Operate on the active region BEG to END, otherwise operate on the
buffer, which may be narrowed.
If optional REMOVE is non-nil, remove tables including their templates."
  (interactive "R")
  (markdown-ts--barf-if-not-mode 'markdown-ts-toc-clear)
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))
  (unless (eq beg end)
    ;; Convert markers to absolute positions.
    (setq beg (if (markerp beg) (marker-position beg) beg))
    (setq end (if (markerp end) (marker-position end) end))
    ;; NOTE: Operate on the widened buffer to avoid treesiter crash issues.
    (without-restriction
      (let ((tocs)
            (beg-pos beg)
            (end-pos end))
        ;; We scan each pass because their positions change.
        (while (and (< beg-pos end-pos)
                    (setq tocs (markdown-ts--tocs beg-pos end-pos)))
          (markdown-ts--tocs-sanity-check tocs)
          ;; After sanity checking, we know the second element is a
          ;; toc-end.
          (let* ((toc (nth 0 tocs))
                 (toc-end (nth 1 tocs))
                 (reg-beg (if remove
                              (alist-get 'beg toc)
                            (alist-get 'end toc)))
                 (reg-end (if remove
                              (alist-get 'end toc-end)
                            (alist-get 'beg toc-end)))
                 (reg-size (- reg-end reg-beg)))
            (delete-region reg-beg reg-end)
            ;; Start the next pass after this toc-end.  Adjust for the
            ;; deleted region.
            (setq beg-pos (- (alist-get 'end toc-end)
                             reg-size))
            (setq end-pos (min (point-max) (- end-pos reg-size)))))))))

(defun markdown-ts-toc-insert-template (&optional char)
  "Insert a `markdown-ts-mode` table of contents template at point.

CHAR is the template type \"b\" for basic, \"c\" for complete.  If CHAR
is nil and the command is run interactively, prompt for a template.

The basic template uses all defaults and is likely the best choice for
most uses.  The complete template illustrates all parameters set to
their defaults and is useful as a starting point to customize a table."
  (interactive
   (list (car (read-multiple-choice
               "Table of contents template"
               '((?b "basic")
                 (?c "complete"))))))
  (pcase char
    (?b
     (insert "<!-- markdown-ts-toc: -->\n"
             "<!-- markdown-ts-toc-end: -->\n"))
    (?c
     (insert "<!-- markdown-ts-toc: -*- "
             "min-depth: 1; "
             "max-depth: nil; "
             "candidates: (heading); "
             "from: below; "
             "style: item; "
             "indent: 1; "
             "no-link: nil; "
             "relative-depth: nil; "
             "ignore: nil; "
             "-*- -->\n"
             "<!-- markdown-ts-toc-end: -->\n"))
    (_ (user-error "No such template type: %c" char))))

(defun markdown-ts-toc-generate (&optional interactive beg end)
  "Generate tables of contents in the current buffer.
`markdown-ts-mode' uses Markdown HTML comment elements to identify table
of contents (aka toc) insertion boundaries and the parameters for each
table.  Derive table content from the Markdown elements in the current
buffer which are usually headings but can also be list items, setext
headers, named code blocks.

Operate on the active region BEG to END, otherwise operate on the
buffer, which may be narrowed.

Each time you run this command, existing tables of contents are cleared
and refreshed with new content.  So don't place toc elements around
important text.

The buffer can have one or more tocs.  By default, populate each toc
with elements that appear in the buffer below the toc.  Therefore, the
easiest way to insert a complete table of contents, is to put the toc
template near the top of your buffer.

A basic empty toc template looks like this:

Header text you want here.

<!-- markdown-ts-toc: -->
Contents inserted between these elements.
<!-- markdown-ts-toc-end: -->

Footer text you want here.

Use the command `markdown-ts-toc-insert-template' to insert a table of
contents template.

Use the command `markdown-ts-toc-clear' to clear table content, and the
command `markdown-ts-toc-clear-and-remove' to both clear and remove
table content and templates.

By default, a toc includes headers at all levels below the toc which are
presented as list items with links.

To ignore a Markdown element that would otherwise be included in the
toc, add a toc ignore element:

## Ignore Me <!-- markdown-ts-toc-ignore: -->

The starting toc element accepts parameters in the syntax of Emacs file
variables.

For example, if you want to generate a table of contents limited to
headings two levels deep:

<!-- markdown-ts-toc: -*- max-depth: 2; -*- -->
<!-- markdown-ts-toc-end: -->

Or use only headings two and three levels deep.

<!-- markdown-ts-toc: -*- min-depth: 2; max-depth: 3; -*- -->
<!-- markdown-ts-toc-end: -->

These are the supported parameters which are optional.  They each list
their defaults.

  min-depth: 1          ; an integer
  max-depth: nil        ; an integer
  candidates: (heading) ; a list
  from: below           ; a symbol
  style: item           ; a symbol
  indent: 1             ; a number
  no-link: nil          ; a boolean
  relative-depth: nil   ; a boolean
  ignore: nil           ; a boolean

`min-depth' and `max-depth' both default to using all candidates at
every level.  Use `min-depth' to ignore level 1 headings like \"#
Heading\", by using 2.  Use `max-depth' to control how deep into the
heading hierarchy you need your toc to go.  Use 3 to stop at level three
headings.

Headings dictate the level of their children even if headings are
excluded from candidates.  Other element types inherit the level of
their preceding heading.

`candidates' is a list and can contain any of the following Markdown
element shortcut names.  You can mix higher-level grouping symbols and
lower level symbols.

  all (includes everything)
  heading (includes the below)
    h1 h2 h3 h4 h5 h6
  setext (includes the below)
    sh1 sh2
  item (includes the below)
    minus plus star
  numbered-item (includes the below)
    dot paren
  code (named code blocks)

`from' controls the direction and scope from which candidates are
selected.  Typically, a toc is placed at the beginning of a buffer and
`from' is `below' to capture entry candidates after the toc.  If you set
`from' to `above', candidates are selected from above the toc.  Use
`all' to capture every entry candidate.  This is useful to create a
complete toc at the end of your buffer.

The `style' parameter can be nil, `item', `number' or `number.'.  Under
nil, entries have no decorations.  If `item', entries are prefixed with
\"-\".  If `bullet', prefix with \"*\".  If `number', numeric prefixes
are generated, for example 1, 1.1, 2, 2.1, 2.1.1, 2.1.2.  Use `number.'
to add a period to each entry's number.

`indent' is an integer between 0 and 10.  0 means nothing should be
indented.  Otherwise, each entry is indented by the number of spaces of
its Markdown level multiplied by this value.  Candidates that have no
native level such as list items inherit the base indentation level of
its preceding header.  List item hierarchies and code blocks indent
under that base.

Set `no-link' to t if you do not want your entries to have header links.
By default, each entry derived from a heading is presented as a link to
its source heading.  NOTE: The elements represented by `setext', `item',
`numbered-item', `code' do not support links.

Use `relative-depth' to create a toc underneath a heading, limiting the
toc to heading depths underneath the preceding header and specify the
levels relative to that header.  More concretely, if the preceding
header is a level 1 \"#\" header, `min-depth' 1 and `max-depth' 2 will
be interpreted to be 2 and 3.  If you demote the header to level 2
\"##\" they will be interpreted as 3 and 4.  This is useful to avoid
fussing with toc configurations under headers when you change their
levels.

For example:

<!-- markdown-ts-toc: -*- relative-depth: t; min-depth: 1; max-depth: 2; -*- -->
<!-- markdown-ts-toc-end: -->

Finally, if `ignore' is t, you can keep a toc element in place and skip
it.

INTERACTIVE will be non-nil if this command was invoked interactively.

See `markdown-ts-toc-generate-warn-if-none' to configure warnings about
tables not being processed when this function is called."
  (interactive "p\nR")
  ;; NOTE: If parameters or their defaults change, be sure to mirror
  ;; them in `markdown-ts-toc-insert-template'.
  (markdown-ts--barf-if-not-mode 'markdown-ts-toc-generate)
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))
  (unless (eq beg end)
    ;; Convert markers to absolute positions.
    (setq beg (if (markerp beg) (marker-position beg) beg))
    (setq end (if (markerp end) (marker-position end) end))
    ;; NOTE: Operate on the widened buffer to avoid treesiter crash issues.
    (without-restriction
      (let ((tocs (markdown-ts--tocs beg end)))
        (unless tocs
          (if (and interactive
                   (eq 'insert-template markdown-ts-toc-generate-warn-if-none))
              (progn
                (markdown-ts-toc-insert-template)
                ;; If the user didn't quit the prompt, a template may have
                ;; been added, so rescan the buffer.
                (setq tocs (markdown-ts--tocs)))
            (when (eq 'no-tables markdown-ts-toc-generate-warn-if-none)
              (lwarn 'markdown-ts-toc-generate
                     :warning
                     "No tables of contents found in %s."
                     (buffer-name (current-buffer))))))
        (markdown-ts--tocs-sanity-check tocs)

        ;; Strategy is process one toc at a time from the top to the
        ;; bottom of the range BEG END.  Subsequent toc and entry
        ;; candidate node positions change as we update each toc text so
        ;; we have to retrieve them fresh from the end of the most recent
        ;; toc.
        (let ((tocs)
              (num-tocs-processed 0)
              (toc)
              (toc-end)
              (toc-size)
              ;; These are adjusted by the size of inserted text each pass.
              (beg-pos beg)
              (end-pos end)
              (source-buffer (current-buffer)))
          ;; Find each "markdown-ts-toc" in succession starting at the end
          ;; of each previous one we find within the requested region.
          (while (and (< beg-pos end-pos)
                      (setq tocs (markdown-ts--tocs beg-pos end-pos)))
            (setq toc (car tocs))
            (setq toc-size 0)

            (when (equal "markdown-ts-toc" (car toc))
              ;; After sanity checking, we know the second element is a
              ;; toc-end.
              (setq toc-end (nth 1 tocs))
              (let* ((toc-node (alist-get 'node toc))
                     (toc-vars (alist-get 'vars toc))
                     (toc-ignore (alist-get 'ignore toc-vars)))
                (unless toc-ignore
                  ;; Build the toc one candidate at a time, tracking
                  ;; indentation and creating links if needed.
                  (let* ((toc-end-node (alist-get 'node toc-end))
                         (reg-beg (treesit-node-end toc-node))
                         (reg-end (treesit-node-start toc-end-node))
                         (toc-handles (markdown-ts--toc-expand-candidate-handles
                                       (or (cdr-safe (assq 'candidates toc-vars))
                                           'heading)))
                         ;; Now add headings to always retrieve and derive
                         ;; levels even if they are excluded from the toc.
                         (seed-handles (copy-tree toc-handles))
                         (_ (dolist (elt (alist-get 'heading markdown-ts--toc-handles))
                              (cl-pushnew elt seed-handles)))
                         (toc-from (or (cdr-safe (assq 'from toc-vars)) 'below))
                         (toc-style (or (cdr-safe (assq 'style toc-vars)) 'item))
                         (toc-no-link (alist-get 'no-link toc-vars))
                         (toc-indent (min 10
                                          (max 0 (or (cdr-safe (assq 'indent toc-vars)) 1))))
                         (toc-min-depth (max 1
                                             (or (cdr-safe (assq 'min-depth toc-vars)) 1)))
                         (toc-max-depth (max toc-min-depth
                                             (or (cdr-safe (assq 'max-depth toc-vars))
                                                 most-positive-fixnum)))
                         (toc-relative-depth (cdr-safe (assq 'relative-depth toc-vars)))
                         (toc-candidates (markdown-ts--toc-collect-candidates
                                          toc-node toc-end-node seed-handles toc-from))
                         (toc-relative-heading-depth
                          (1+ (or (seq-position
                                   '("atx_h1_marker" "atx_h2_marker" "atx_h3_marker"
                                     "atx_h4_marker" "atx_h5_marker" "atx_h6_marker")
                                   (treesit-node-type
                                    (treesit-node-child
                                     (treesit-search-forward
                                      toc-node
                                      "atx_heading" 'backward) 0 'named))
                                   #'equal)
                                  -1)))
                         (slug-to-pos (markdown-ts--heading-ids))
                         (pos-to-slug
                          (let ((ht (make-hash-table :size (hash-table-count slug-to-pos)
                                                     :test #'equal)))
                            (maphash (lambda (k v) (puthash v k ht)) slug-to-pos)
                            ht)))

                    (with-work-buffer
                      (insert "<!-- NOTE: markdown-ts-toc generated text section may be overwritten. -->\n")
                      (let ((handle) (handle-class)
                            (node)
                            (indent 0) (level 1) (level-prev 0)
                            ;; Heading level takes precedence over items under
                            ;; it.  If there are no headings, it will remain 1.
                            (level-heading 1)
                            ;; Adjust for non headings under headings.
                            (number-stack))

                        ;; Candidates should be in buffer order.
                        (dolist (candidate toc-candidates)
                          (let ((slug-pos) (text) (link-target)
                                (level-adj 0))
                            (setq handle (car candidate)
                                  handle-class (alist-get
                                                handle markdown-ts--toc-handle-classes))
                            (setq node (cdr candidate))
                            (setq slug-pos nil)

                            ;; Strip trailing newlines.  Add them consistently below.
                            (setq text (string-trim-right
                                        (markdown-ts--toc-text-normalize
                                         handle node
                                         (treesit-node-text node 'no-property))))

                            ;; Candidate node type logic.
                            (pcase handle-class
                              ('heading
                               (let ((x (seq-position '(h1 h2 h3 h4 h5 h6) handle)))
                                 (setq slug-pos (treesit-node-start
                                                 (treesit-search-forward node "atx_heading" 'backward)))
                                 (setq level (setq level-heading (1+ x)))
                                 (setq level-adj 0)
                                 (setq indent (1- level))
                                 (setq link-target text)))
                              ('setext
                               (let ((x (seq-position '(sh1 sh2) handle)))
                                 (setq level (1+ x))
                                 (setq level-adj 0)
                                 (setq indent (1- level))
                                 (setq link-target nil)))
                              ((or 'item 'numbered-item)
                               (setq level level-heading)
                               (setq level-adj (markdown-ts--toc-list-item-depth node))
                               (setq indent (1-  (+ level level-adj)))
                               (setq link-target nil))
                              ('code
                               ;; Inherit the ambient level.
                               (setq level-adj 1)
                               (setq indent (1-  (+ level level-adj)))
                               (setq link-target nil)))

                            ;; Filter desired handles and levels.
                            (when (and (memq handle toc-handles)
                                       (>= level (+ toc-min-depth
                                                    (if toc-relative-depth
                                                        toc-relative-heading-depth 0)))
                                       (<= level (+ toc-max-depth
                                                    (if toc-relative-depth
                                                        toc-relative-heading-depth 0))))

                              ;; Create the entry with indent, item/number prefix, link.

                              ;; Corner case: initial level higher than 1.
                              (when (and (> level 1)
                                         (eq level-prev 0))
                                (setq level 1))
                              (when (and (not toc-no-link) link-target)
                                ;; Sanitize the id to conform to CSS ident syntax.
                                (setq text
                                      (format "[%s](#%s)"
                                              text
                                              (if-let* ((slug (gethash slug-pos pos-to-slug)))
                                                  slug
                                                (funcall markdown-ts-toc-slug-function
                                                         link-target)))))
                              (insert (make-string (* (1- (max indent 1)) toc-indent) ?\s)
                                      (pcase toc-style
                                        ('item "- ")
                                        ('bullet "* ")
                                        ((or 'number 'number.)
                                         (let ((level (+ level level-adj)))
                                           (cond ((or (not number-stack)
                                                      (> level (caar number-stack)))
                                                  (push `(,level . 0) number-stack))
                                                 ((and number-stack
                                                       (< level (caar number-stack)))
                                                  (while (and number-stack
                                                              (< level (caar number-stack)))
                                                    (pop number-stack))
                                                  (unless number-stack
                                                    (push `(,level . 0) number-stack)))))
                                         (setcdr (car number-stack) (1+ (cdar number-stack)))
                                         (let ((s) (n (length number-stack)))
                                           (seq-map-indexed
                                            (lambda (elt idx)
                                              (setq s
                                                    (concat
                                                     s
                                                     (format "%d" (cdr elt))
                                                     (if (eq (1+ idx) n) "" "."))))
                                            (reverse number-stack))
                                           (concat s
                                                   (when (eq toc-style 'number.) ".")
                                                   ") ")))
                                        (_ ""))
                                      text
                                      ;; Add a hard line break.  Backslash
                                      ;; seems less well supported by
                                      ;; renderers.
                                      (make-string 2 ?\s)
                                      "\n"))
                            (setq level-prev level))))
                      (setq toc-size (buffer-size))
                      (let ((temp-buffer (current-buffer)))
                        (with-current-buffer source-buffer
                          (replace-region-contents reg-beg reg-end temp-buffer)))))
                  (setq num-tocs-processed (1+ num-tocs-processed)))))
            ;; Start the next pass after this toc-end.  Adjust for the
            ;; inserted text size.
            (setq beg-pos (+ (alist-get 'end toc-end) toc-size))
            (setq end-pos (min (point-max) (+ end-pos toc-size))))
          (when (and (eq num-tocs-processed 0)
                     (eq 'no-active-tables markdown-ts-toc-generate-warn-if-none))
            (lwarn 'markdown-ts-toc-generate
                   :warning
                   "No tables of contents generated in %s."
                   (buffer-name (current-buffer)))))))))

(provide 'markdown-ts-mode-x)
;;; markdown-ts-mode-x.el ends here
