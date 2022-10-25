;;; treesit.el --- tree-sitter utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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
;; This file is the lisp counterpart of treesit.c, together they
;; provide tree-sitter integration for Emacs.  This file contains
;; convenient functions that are more idiomatic and flexible than the
;; exposed C API of tree-sitter.  It also contains frameworks for
;; integrating tree-sitter with font-lock, indent, activate/deactivate
;; tree-sitter, debugging tree-sitter, etc.

;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x)) ; For `string-join'.
(require 'cl-seq)
(require 'font-lock)

;;; Custom options

;; Tree-sitter always appear as treesit in symbols.
(defgroup treesit nil
  "Incremental parser.
It is used to enhance major mode features like font-lock,
indent, imenu, etc."
  :group 'tools
  :version "29.1")

(defcustom treesit-max-buffer-size (* 4 1024 1024)
  "Maximum buffer size for enabling tree-sitter parsing (in bytes)."
  :type 'integer
  :version "29.1")

(declare-function treesit-available-p "treesit.c")

(defcustom treesit-settings '((t nil t))
  "Tree-sitter toggle settings for major modes.

A list of (MODE ACTIVATE INHERIT).  MODE is a major mode, ACTIVATE
can be one of the following:

  demand => Demand the use of tree-sitter, warn if it can't activate
  t => Enable if available
  nil => Don't enable

If INHERIT is t, the setting for MODE is inherited by all its
derived modes.  For a derived mode, closer ancestor mode's
setting takes higher precedence.

A special MODE, t, is considered the ancestor of every mode, and
its INHERIT flag is ignored."
  :type '(repeat
          (list :tag "Setting"
                (symbol :tag "Mode")
                (choice :tag "Activate"
                        (const :tag "No" nil)
                        (const :tag "Yes" t)
                        (const :tag "Demand" demand))
                (choice :tag "Inherit"
                        (const :tag "Yes" t)
                        (const :tag "No" nil))))
  :version "29.1")

;;; Parser API supplement

(defun treesit-parse-string (string language)
  "Parse STRING using a parser for LANGUAGE.
Return the root node of the syntax tree."
  (with-temp-buffer
    (insert string)
    (treesit-parser-root-node
     (treesit-parser-create language))))

(defun treesit-language-at (pos)
  "Return the language used at position POS."
  (cl-loop for parser in (treesit-parser-list)
           if (treesit-node-on pos pos parser)
           return (treesit-parser-language parser)))

(defun treesit-set-ranges (parser-or-lang ranges)
  "Set the ranges of PARSER-OR-LANG to RANGES."
  (treesit-parser-set-included-ranges
   (cond ((symbolp parser-or-lang)
          (or (treesit-parser-create parser-or-lang)
              (error "Cannot find a parser for %s" parser-or-lang)))
         ((treesit-parser-p parser-or-lang)
          parser-or-lang)
         (t (error "Expecting a parser or language, but got %s"
                   parser-or-lang)))
   ranges))

(defun treesit-get-ranges (parser-or-lang)
  "Get the ranges of PARSER-OR-LANG."
  (treesit-parser-included-ranges
   (cond ((symbolp parser-or-lang)
          (or (treesit-parser-create parser-or-lang)
              (error "Cannot find a parser for %s" parser-or-lang)))
         ((treesit-parser-p parser-or-lang)
          parser-or-lang)
         (t (error "Expecting a parser or language, but got %s"
                   parser-or-lang)))))

;;; Node API supplement

(defun treesit-node-buffer (node)
  "Return the buffer in where NODE belongs."
  (treesit-parser-buffer
   (treesit-node-parser node)))

(defun treesit-node-language (node)
  "Return the language symbol that NODE's parser uses."
  (treesit-parser-language
   (treesit-node-parser node)))

(defun treesit-node-at (pos &optional parser-or-lang named strict)
  "Return the smallest node that starts at or after buffer position POS.

\"Starts at or after POS\" means the start of the node is greater or
equal than POS.

Return nil if none find.  If NAMED is non-nil, only look for named node.

If PARSER-OR-LANG is nil, use the first parser in
\(`treesit-parser-list'); if PARSER-OR-LANG is a parser, use
that parser; if PARSER-OR-LANG is a language, find a parser using
that language in the current buffer, and use that.

If POS is after all the text in the buffer, i.e., there is no
node after POS, return the last leaf node in the parse tree, even
though that node is before POS.  If STRICT is non-nil, return nil
in this case."
  (let* ((root (if (treesit-parser-p parser-or-lang)
                   (treesit-parser-root-node parser-or-lang)
                 (treesit-buffer-root-node parser-or-lang)))
         (node root)
         next)
    (when node
      ;; This is very fast so no need for C implementation.
      (while (setq next (treesit-node-first-child-for-pos
                         node pos named))
        (setq node next))
      ;; If we are at the end of buffer or after all the text, we will
      ;; end up with NODE = root node.  For convenience, return the last
      ;; leaf node in the tree.
      (if (treesit-node-eq node root)
          (if strict
              nil
            (while (setq next (treesit-node-child node -1 named))
              (setq node next))
            node)
        node))))

(defun treesit-node-on (beg end &optional parser-or-lang named)
  "Return the smallest node covering BEG to END.

BEWARE!  Calling this function on an empty line that is not
inside any top-level construct (function definition, etc) most
probably will give you the root node, because the root node is
the smallest node that covers that empty line.  You probably want
to use `treesit-node-at' instead.

Return nil if none find.  If NAMED non-nil, only look for named
node.

If PARSER-OR-LANG is nil, use the first parser in
(`treesit-parser-list'); if PARSER-OR-LANG is a parser, use
that parser; if PARSER-OR-LANG is a language, find a parser using
that language in the current buffer, and use that."
  (let ((root (if (treesit-parser-p parser-or-lang)
                  (treesit-parser-root-node parser-or-lang)
                (treesit-buffer-root-node parser-or-lang))))
    (treesit-node-descendant-for-range root beg (or end beg) named)))

(defun treesit-buffer-root-node (&optional language)
  "Return the root node of the current buffer.
Use the first parser in (`treesit-parser-list'), if LANGUAGE is
non-nil, use the first parser for LANGUAGE."
  (if-let ((parser
            (or (if language
                    (or (treesit-parser-create language)
                        (error "Cannot find a parser for %s" language))
                  (or (car (treesit-parser-list))
                      (error "Buffer has no parser"))))))
      (treesit-parser-root-node parser)))

(defun treesit-filter-child (node pred &optional named)
  "Return children of NODE that satisfies PRED.
PRED is a function that takes one argument, the child node.  If
NAMED non-nil, only search for named node."
  (let ((child (treesit-node-child node 0 named))
        result)
    (while child
      (when (funcall pred child)
        (push child result))
      (setq child (treesit-node-next-sibling child named)))
    (reverse result)))

(defun treesit-node-text (node &optional no-property)
  "Return the buffer (or string) content corresponding to NODE.
If NO-PROPERTY is non-nil, remove text properties."
  (when node
    (with-current-buffer (treesit-node-buffer node)
      (if no-property
          (buffer-substring-no-properties
           (treesit-node-start node)
           (treesit-node-end node))
        (buffer-substring
         (treesit-node-start node)
         (treesit-node-end node))))))

(defun treesit-parent-until (node pred)
  "Return the closest parent of NODE that satisfies PRED.
Return nil if none found.  PRED should be a function that takes
one argument, the parent node."
  (let ((node (treesit-node-parent node)))
    (while (and node (not (funcall pred node)))
      (setq node (treesit-node-parent node)))
    node))

(defun treesit-parent-while (node pred)
  "Return the furthest parent of NODE that satisfies PRED.
Return nil if none found.  PRED should be a function that takes
one argument, the parent node."
  (let ((last nil))
    (while (and node (funcall pred node))
      (setq last node
            node (treesit-node-parent node)))
    last))

(defalias 'treesit-traverse-parent #'treesit-parent-until)

(defun treesit-node-children (node &optional named)
  "Return a list of NODE's children.
If NAMED is non-nil, collect named child only."
  (mapcar (lambda (idx)
            (treesit-node-child node idx named))
          (number-sequence
           0 (1- (treesit-node-child-count node named)))))

(defun treesit-node-index (node &optional named)
  "Return the index of NODE in its parent.
If NAMED is non-nil, count named child only."
  (let ((count 0))
    (while (setq node (treesit-node-prev-sibling node named))
      (cl-incf count))
    count))

(defun treesit-node-field-name (node)
  "Return the field name of NODE as a child of its parent."
  (when-let ((parent (treesit-node-parent node))
             (idx (treesit-node-index node)))
    (treesit-node-field-name-for-child parent idx)))

;;; Query API supplement

(defun treesit-query-string (string query language)
  "Query STRING with QUERY in LANGUAGE.
See `treesit-query-capture' for QUERY."
  (with-temp-buffer
    (insert string)
    (let ((parser (treesit-parser-create language)))
      (treesit-query-capture
       (treesit-parser-root-node parser)
       query))))

(defun treesit-query-range (source query &optional beg end)
  "Query the current buffer and return ranges of captured nodes.

QUERY, SOURCE, BEG, END are the same as in
`treesit-query-in'.  This function returns a list
of (START . END), where START and END specifics the range of each
captured node.  Capture names don't matter."
  (cl-loop for capture
           in (treesit-query-capture source query beg end)
           for node = (cdr capture)
           collect (cons (treesit-node-start node)
                         (treesit-node-end node))))

;;; Range API supplement

(defvar-local treesit-range-functions nil
  "A list of range functions.
Font-locking and indenting code uses functions in this list to
set correct ranges for a language parser before using it.

The signature of each function should be

    (start end &rest _)

where START and END marks the region that is about to be used.  A
range function only need to (but not limited to) update ranges in
that region.

Each function in the list is called in-order.")

(defun treesit-update-ranges (&optional start end)
  "Update the ranges for each language in the current buffer.
Calls each range functions in `treesit-range-functions'
in-order.  START and END are passed to each range function."
  (dolist (range-fn treesit-range-functions)
    (funcall range-fn (or start (point-min)) (or end (point-max)))))

(defun treesit-parser-range-on (parser beg &optional end)
  "Check if PARSER's range covers the portion between BEG and END.

If it does, return the range covering that portion in the form
of (RANGE-BEG . RANGE-END), if not, return nil.  If nil or
omitted, default END to BEG."
  (let ((ranges (treesit-parser-included-ranges parser))
        (end (or end beg)))
    (if (null ranges)
        (cons (point-min) (point-max))
      (cl-loop for rng in ranges
               if (<= (car rng) beg end (cdr rng))
               return rng
               finally return nil))))

;;; Font-lock

(define-error 'treesit-font-lock-error
              "Generic tree-sitter font-lock error"
              'treesit-error)

(defvar-local treesit-font-lock-feature-list nil
  "A list of lists of feature symbols.

Each sublist represents a decoration level.
`font-lock-maximum-decoration' controls which levels are
activated.

Inside each sublist are feature symbols, which corresponds to the
:feature value of a query defined in `treesit-font-lock-rules'.
Removing a feature symbol from this list disables the
corresponding query during font-lock.

Common feature names (for general programming language) include
function-name, type, variable-name (LHS of assignments), builtin,
constant, keyword, string-interpolation, comment, doc, string,
operator, preprocessor, escape-sequence, key (in key-value
pairs).  Major modes are free to subdivide or extend on these
common features.

For changes to this variable to take effect, run
`treesit-font-lock-recompute-features'.")

(defvar-local treesit-font-lock-settings nil
  "A list of SETTINGs for treesit-based fontification.

The exact format of this variable is considered internal.  One
should always use `treesit-font-lock-rules' to set this variable.

Each SETTING is of form

    (QUERY ENABLE FEATURE OVERRIDE)

QUERY must be a compiled query.  See Info node `(elisp)Pattern
Matching' for how to write a query and compile it.

For SETTING to be activated for font-lock, ENABLE must be t.  To
disable this SETTING, set ENABLE to nil.

FEATURE is the \"feature name\" of the query, users can control
which features are enabled with `font-lock-maximum-decoration'
and `treesit-font-lock-feature-list'.

OVERRIDE is the override flag for this query.  Its value can be
t, nil, append, prepend, keep.  See more in
`treesit-font-lock-rules'.")

(defun treesit-font-lock-rules (&rest args)
  "Return a value suitable for `treesit-font-lock-settings'.

Take a series of QUERIES in either string, s-expression or
compiled form.  Same as in `treesit-font-lock-settings', for each
query, captured nodes are highlighted with the capture name as
its face.

Before each QUERY there could be :KEYWORD VALUE pairs that
configure the query (and only that query).  For example,

    (treesit-font-lock-rules
     :language \\='javascript
     :override t
     :feature\\='constant
     \\='((true) @font-lock-constant-face
       (false) @font-lock-constant-face)
     :language \\='html
     :feature \\='script
     \"(script_element) @font-lock-builtin-face\")

For each QUERY, a :language keyword and a :feature keyword is
required. Each query's :feature is a symbol summarizing what does
the query fontify.  It is used to allow users to enable/disable
certain features.  See `treesit-font-lock-kind-list' for more.
Other keywords include:

  KEYWORD    VALUE    DESCRIPTION
  :override  nil      If the region already has a face,
                      discard the new face
             t        Always apply the new face
             append   Append the new face to existing ones
             prepend  Prepend the new face to existing ones
             keep     Fill-in regions without an existing face

Capture names in QUERY should be face names like
`font-lock-keyword-face'.  The captured node will be fontified
with that face.  Capture names can also be function names, in
which case the function is called with (START END NODE), where
START and END are the start and end position of the node in
buffer, and NODE is the tree-sitter node object.  If a capture
name is both a face and a function, the face takes priority.  If
a capture name is not a face name nor a function name, it is
ignored.

\(fn :KEYWORD VALUE QUERY...)"
  (let (;; Tracks the current :language/:override/:toggle/:level value
        ;; that following queries will apply to.
        current-language current-override
        current-feature
        ;; The list this function returns.
        (result nil))
    (while args
      (let ((token (pop args)))
        (pcase token
          ;; (1) Process keywords.
          (:language
           (let ((lang (pop args)))
             (when (or (not (symbolp lang)) (null lang))
               (signal 'treesit-font-lock-error
                       `("Value of :language should be a symbol"
                         ,lang)))
             (setq current-language lang)))
          (:override
           (let ((flag (pop args)))
             (when (not (memq flag '(t nil append prepend keep)))
               (signal 'treesit-font-lock-error
                       `("Value of :override should be one of t, nil, append, prepend, keep"
                         ,flag))
               (signal 'wrong-type-argument
                       `((or t nil append prepend keep)
                         ,flag)))
             (setq current-override flag)))
          (:feature
           (let ((var (pop args)))
             (when (or (not (symbolp var))
                       (memq var '(t nil)))
               (signal 'treesit-font-lock-error
                       `("Value of :feature should be a symbol"
                         ,var)))
             (setq current-feature var)))
          ;; (2) Process query.
          ((pred treesit-query-p)
           (when (null current-language)
             (signal 'treesit-font-lock-error
                     `("Language unspecified, use :language keyword to specify a language for this query" ,token)))
           (when (null current-feature)
             (signal 'treesit-font-lock-error
                     `("Feature unspecified, use :feature keyword to specify the feature name for this query" ,token)))
           (if (treesit-compiled-query-p token)
               (push `(,current-language token) result)
             (push `(,(treesit-query-compile current-language token)
                     t
                     ,current-feature
                     ,current-override)
                   result))
           ;; Clears any configurations set for this query.
           (setq current-language nil
                 current-override nil
                 current-feature nil))
          (_ (signal 'treesit-font-lock-error
                     `("Unexpected value" ,token))))))
    (nreverse result)))

(defun treesit-font-lock-recompute-features ()
  "Enable/disable font-lock settings according to decoration level.
Sets the ENABLE flag for each setting in
`treesit-font-lock-settings', according to
`treesit-font-lock-feature-list' and
`font-lock-maximum-decoration'."
  (let* ((level (font-lock-value-in-major-mode
                 font-lock-maximum-decoration))
         (features (cl-loop
                    for idx = 0 then (1+ idx)
                    for features in treesit-font-lock-feature-list
                    if (or (eq level t)
                           (>= level (1+ idx)))
                    append features)))
    (cl-loop for idx = 0 then (1+ idx)
             for setting in treesit-font-lock-settings
             for feature = (nth 2 setting)
             ;; Set the ENABLE flag for the setting.
             do (setf (nth 1 (nth idx treesit-font-lock-settings))
                      (if (memq feature features) t nil)))))

(defun treesit-font-lock-fontify-region
    (start end &optional loudly)
  "Fontify the region between START and END.
If LOUDLY is non-nil, message some debugging information."
  (treesit-update-ranges start end)
  (font-lock-unfontify-region start end)
  (dolist (setting treesit-font-lock-settings)
    (let* ((query (nth 0 setting))
           (enable (nth 1 setting))
           (override (nth 3 setting))
           (language (treesit-query-language query)))
      (when-let ((node (treesit-node-on start end language))
                 ;; Only activate if ENABLE flag is t.
                 (activate (eq t enable)))
        (ignore activate)
        (let ((captures (treesit-query-capture
                         node query
                         ;; Specifying the range is important. More
                         ;; often than not, NODE will be the root
                         ;; node, and if we don't specify the range,
                         ;; we are basically querying the whole file.
                         start end))
              (inhibit-point-motion-hooks t))
          (with-silent-modifications
            (dolist (capture captures)
              (let* ((face (car capture))
                     (node (cdr capture))
                     (start (treesit-node-start node))
                     (end (treesit-node-end node)))
                (cond
                 ((facep face)
                  (pcase override
                    ('nil (unless (text-property-not-all
                                   start end 'face nil)
                            (put-text-property start end 'face face)))
                    ('t (put-text-property start end 'face face))
                    ('append (font-lock-append-text-property
                              start end 'face face))
                    ('prepend (font-lock-prepend-text-property
                               start end 'face face))
                    ('keep (font-lock-fillin-text-property
                            start end 'face face))
                    (_ (signal 'treesit-font-lock-error
                               (list
                                "Unrecognized value of :override option"
                                override)))))
                 ((functionp face)
                  (funcall face start end node)))
                ;; Don't raise an error if FACE is neither a face nor
                ;; a function.  This is to allow intermediate capture
                ;; names used for #match and #eq.
                (when loudly
                  (message "Fontifying text from %d to %d, Face: %s Language: %s"
                           start end face language)))))))))
  ;; Call regexp font-lock after tree-sitter, as it is usually used
  ;; for custom fontification.
  (let ((font-lock-unfontify-region-function #'ignore))
    (funcall #'font-lock-default-fontify-region start end loudly)))

(defun treesit-font-lock-enable ()
  "Enable tree-sitter font-locking for the current buffer."
  (treesit-font-lock-recompute-features)
  (setq-local font-lock-fontify-region-function
              #'treesit-font-lock-fontify-region)
  ;; If we don't set `font-lock-defaults' to some non-nil value,
  ;; font-lock doesn't enable properly (`font-lock-mode-internal'
  ;; doesn't run).  See `font-lock-specified-p'.
  (when (null font-lock-defaults)
    (setq font-lock-defaults '(nil)))
  (font-lock-mode 1))

;;; Indent

(defvar treesit--indent-verbose nil
  "If non-nil, log progress when indenting.")

;; This is not bound locally like we normally do with major-mode
;; stuff, because for tree-sitter, a buffer could contain more than
;; one language.
(defvar treesit-simple-indent-rules nil
  "A list of indent rule settings.
Each indent rule setting should be (LANGUAGE . RULES),
where LANGUAGE is a language symbol, and RULES is a list of

    (MATCHER ANCHOR OFFSET).

MATCHER determines whether this rule applies, ANCHOR and OFFSET
together determines which column to indent to.

A MATCHER is a function that takes three arguments (NODE PARENT
BOL).  BOL is the point where we are indenting: the beginning of
line content, the position of the first non-whitespace character.
NODE is the largest (highest-in-tree) node starting at that
point.  PARENT is the parent of NODE.

If MATCHER returns non-nil, meaning the rule matches, Emacs then
uses ANCHOR to find an anchor, it should be a function that takes
the same argument (NODE PARENT BOL) and returns a point.

Finally Emacs computes the column of that point returned by ANCHOR
and adds OFFSET to it, and indents to that column.

For MATCHER and ANCHOR, Emacs provides some convenient presets.
See `treesit-simple-indent-presets'.")

(defvar treesit-simple-indent-presets
  '((match . (lambda
               (&optional node-type parent-type node-field
                          node-index-min node-index-max)
               `(lambda (node parent bol &rest _)
                  (and (or (null ,node-type)
                           (equal (treesit-node-type node)
                                  ,node-type))
                       (or (null ,parent-type)
                           (equal (treesit-node-type parent)
                                  ,parent-type))
                       (or (null ,node-field)
                           (equal (treesit-node-field-name node)
                                  ,node-field))
                       (or (null ,node-index-min)
                           (>= (treesit-node-index node t)
                               ,node-index-min))
                       (or (null ,node-index-max)
                           (<= (treesit-node-index node t)
                               ,node-index-max))))))
    (no-node . (lambda (node parent bol &rest _) (null node)))
    (parent-is . (lambda (type)
                   `(lambda (node parent bol &rest _)
                      (equal ,type (treesit-node-type parent)))))

    (node-is . (lambda (type)
                 `(lambda (node parent bol &rest _)
                    (equal ,type (treesit-node-type node)))))

    (query . (lambda (pattern)
               `(lambda (node parent bol &rest _)
                  (cl-loop for capture
                           in (treesit-query-capture
                               parent ,pattern)
                           if (treesit-node-eq node (cdr capture))
                           return t
                           finally return nil))))
    (first-sibling . (lambda (node parent bol &rest _)
                       (treesit-node-start
                        (treesit-node-child parent 0 t))))

    (parent . (lambda (node parent bol &rest _)
                (treesit-node-start parent)))
    (parent-bol . (lambda (node parent bol &rest _)
                    (save-excursion
                      (goto-char (treesit-node-start parent))
                      (back-to-indentation)
                      (point))))
    (prev-sibling . (lambda (node parent bol &rest _)
                      (treesit-node-start
                       (treesit-node-prev-sibling node))))
    (no-indent . (lambda (node parent bol &rest _) bol))
    (prev-line . (lambda (node parent bol &rest _)
                   (save-excursion
                     (goto-char bol)
                     (forward-line -1)
                     (skip-chars-forward " \t")))))
  "A list of presets.
These presets that can be used as MATHER and ANCHOR in
`treesit-simple-indent-rules'.

MATCHER:

\(match NODE-TYPE PARENT-TYPE NODE-FIELD NODE-INDEX-MIN NODE-INDEX-MAX)

    NODE-TYPE checks for node's type, PARENT-TYPE checks for
    parent's type, NODE-FIELD checks for the filed name of node
    in the parent, NODE-INDEX-MIN and NODE-INDEX-MAX checks for
    the node's index in the parent.  Therefore, to match the
    first child where parent is \"argument_list\", use

        (match nil \"argument_list\" nil nil 0 0).

no-node

    Matches the case where node is nil, i.e., there is no node
    that starts at point.  This is the case when indenting an
    empty line.

\(parent-is TYPE)

    Check that the parent has type TYPE.

\(node-is TYPE)

    Checks that the node has type TYPE.

\(query QUERY)

    Queries the parent node with QUERY, and checks if the node
    is captured (by any capture name).

ANCHOR:

first-sibling

    Find the first child of the parent.

parent

    Find the parent.

parent-bol

    Find the beginning of non-space characters on the line where
    the parent is on.

prev-sibling

    Find node's previous sibling.

no-indent

    Do nothing.

prev-line

    The first non-whitespace charater on the previous line.")

(defun treesit--simple-apply (fn args)
  "Apply ARGS to FN.

If FN is a key in `treesit-simple-indent-presets', use the
corresponding value as the function."
  ;; We don't want to match uncompiled lambdas, so make sure this cons
  ;; is not a function.  We could move the condition functionp
  ;; forward, but better be explicit.
  (cond ((and (consp fn) (not (functionp fn)))
         (apply (treesit--simple-apply (car fn) (cdr fn))
                ;; We don't evaluate ARGS with `simple-apply', i.e.,
                ;; no composing, better keep it simple.
                args))
        ((and (symbolp fn)
              (alist-get fn treesit-simple-indent-presets))
         (apply (alist-get fn treesit-simple-indent-presets)
                args))
        ((functionp fn) (apply fn args))
        (t (error "Couldn't find the function corresponding to %s" fn))))

;; This variable might seem unnecessary: why split
;; `treesit-indent' and `treesit-simple-indent' into two
;; functions?  We add this variable in between because later we might
;; add more powerful indentation engines, and that new engine can
;; probably share `treesit-indent'.  It is also useful, suggested
;; by Stefan M, to have a function that figures out how much to indent
;; but doesn't actually performs the indentation, because we might
;; want to know where will a node indent to if we put it at some other
;; location, and use that information to calculate the actual
;; indentation.  And `treesit-simple-indent' is that function.  I
;; forgot the example Stefan gave, but it makes a lot of sense.
(defvar treesit-indent-function #'treesit-simple-indent
  "Function used by `treesit-indent' to do some of the work.

This function is called with

    (NODE PARENT BOL &rest _)

and returns

    (ANCHOR . OFFSET).

BOL is the position of the beginning of the line; NODE is the
\"largest\" node that starts at BOL; PARENT is its parent; ANCHOR
is a point (not a node), and OFFSET is a number.  Emacs finds the
column of ANCHOR and adds OFFSET to it as the final indentation
of the current line.")

(defun treesit-indent ()
  "Indent according to the result of `treesit-indent-function'."
  (treesit-update-ranges)
  (let* ((orig-pos (point))
         (bol (save-excursion
                (forward-line 0)
                (skip-chars-forward " \t")
                (point)))
         (smallest-node
          (cl-loop for parser in (treesit-parser-list)
                   for node = (treesit-node-at bol parser)
                   if node return node))
         (node (treesit-parent-while
                smallest-node
                (lambda (node)
                  (eq bol (treesit-node-start node))))))
    (pcase-let*
        ((parser (if smallest-node
                     (treesit-node-parser smallest-node)
                   nil))
         ;; NODE would be nil if BOL is on a whitespace.  In that case
         ;; we set PARENT to the "node at point", which would
         ;; encompass the whitespace.
         (parent (cond ((and node parser)
                        (treesit-node-parent node))
                       (parser
                        (treesit-node-at bol parser))
                       (t nil)))
         (`(,anchor . ,offset)
          (funcall treesit-indent-function node parent bol)))
      (if (null anchor)
          (when treesit--indent-verbose
            (message "Failed to find the anchor"))
        (let ((col (+ (save-excursion
                        (goto-char anchor)
                        (current-column))
                      offset)))
          (if (< bol orig-pos)
              (save-excursion
                (indent-line-to col))
            (indent-line-to col)))))))

(defun treesit-simple-indent (node parent bol)
  "Calculate indentation according to `treesit-simple-indent-rules'.

BOL is the position of the first non-whitespace character on the
current line.  NODE is the largest node that starts at BOL,
PARENT is NODE's parent.

Return (ANCHOR . OFFSET) where ANCHOR is a node, OFFSET is the
indentation offset, meaning indent to align with ANCHOR and add
OFFSET."
  (if (null parent)
      (when treesit--indent-verbose
        (message "PARENT is nil, not indenting"))
    (let* ((language (treesit-node-language parent))
           (rules (alist-get language
                             treesit-simple-indent-rules)))
      (cl-loop for rule in rules
               for pred = (nth 0 rule)
               for anchor = (nth 1 rule)
               for offset = (nth 2 rule)
               if (treesit--simple-apply
                   pred (list node parent bol))
               do (when treesit--indent-verbose
                    (message "Matched rule: %S" rule))
               and
               return (cons (treesit--simple-apply
                             anchor (list node parent bol))
                            offset)))))

(defun treesit-check-indent (mode)
  "Check current buffer's indentation against a major mode MODE.

Pop up a diff buffer showing the difference.  Correct
indentation (target) is in green, current indentation is in red."
  (interactive "CTarget major mode: ")
  (let ((source-buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring source-buf)
      (funcall mode)
      (indent-region (point-min) (point-max))
      (diff-buffers source-buf (current-buffer)))))

;;; Search

(defun treesit-search-forward-goto
    (node predicate &optional start backward all)
  "Search forward for a node and move to its end position.

Stops at the first node after NODE that matches PREDICATE.
PREDICATE can be either a regexp that matches against each node's
type case-insensitively, or a function that takes a node and
returns nil/non-nil for match/no match.

If a node matches, move to that node and return the node,
otherwise return nil.  If START is non-nil, stop at the
beginning rather than the end of a node.

This function guarantees that the matched node it returns makes
progress in terms of buffer position: the start/end position of
the returned node is always greater than that of NODE.

BACKWARD and ALL are the same as in `treesit-search-forward'."
  (when-let ((start-pos (if start
                            (treesit-node-start node)
                          (treesit-node-end node))))
    ;; When searching forward and stopping at beginnings, or search
    ;; backward stopping at ends, it is possible to "roll back" in
    ;; position.  Take three nodes N1, N2, N3 as an example, if we
    ;; start at N3, search for forward for beginning, and N1 matches,
    ;; we would stop at beg of N1, which is backwards!  So we skip N1
    ;; and keep going.
    ;;
    ;;   |<--------N1------->|
    ;;   |<--N2-->| |<--N3-->|
    (while (and node (if backward
                         (>= (point) start-pos)
                       (<= (point) start-pos)))
      (setq node (treesit-search-forward
                  node predicate backward all))
      (if-let ((pos (if start
                        (treesit-node-start node)
                      (treesit-node-end node))))
          (goto-char pos)))
    ;; If we made reverse progress, go back to where we started.
    (when (if backward
              (>= (point) start-pos)
            (<= (point) start-pos))
      (goto-char start-pos))
    node))

;;; Navigation

(defvar-local treesit-defun-type-regexp nil
  "A regexp that matches the node type of defun nodes.
For example, \"(function|class)_definition\".

This is used by `treesit-beginning-of-defun' and friends.")

(defun treesit--find-top-level-match (node type)
  "Return the top-level parent of NODE matching TYPE.
TYPE is a regexp, this function matches TYPE with each parent's
type."
  (cl-loop for cursor = (treesit-node-parent node)
           then (treesit-node-parent cursor)
           while cursor
           if (string-match-p type (treesit-node-type cursor))
           do (setq node cursor)
           finally return node))

(defun treesit-beginning-of-defun (&optional arg)
  "Tree-sitter `beginning-of-defun' function.
ARG is the same as in `beginning-of-defun."
  (let ((arg (or arg 1))
        (node (treesit-node-at (point))))
    (if (> arg 0)
        ;; Go backward.
        (while (and (> arg 0)
                    (setq node (treesit-search-forward-goto
                                node treesit-defun-type-regexp t t)))
          (setq node (treesit--find-top-level-match
                      node treesit-defun-type-regexp))
          (setq arg (1- arg)))
      ;; Go forward.
      (while (and (< arg 0)
                  (setq node (treesit-search-forward-goto
                              node treesit-defun-type-regexp t t)))
        (setq node (treesit--find-top-level-match
                    node treesit-defun-type-regexp))
        (setq arg (1+ arg))))
    (goto-char (treesit-node-start node))))

(defun treesit-end-of-defun ()
  "Tree-sitter `end-of-defun' function."
  ;; Why not simply get the largest node at point: when point is at
  ;; (point-min), that gives us the root node.
  (let ((node (treesit--find-top-level-match
               (treesit-node-at (point))
               treesit-defun-type-regexp)))
    (goto-char (treesit-node-end node))))

;;; Imenu

(defvar-local treesit-imenu-function nil
  "Tree-sitter version of `imenu-create-index-function'.

Set this variable to a function and `treesit-mode' will bind it
to `imenu-create-index-function'.")

;;; Activating tree-sitter

(defun treesit--setting-for-mode (mode settings)
  "Get the setting for MODE in SETTINGS.
MODE is a major mode symbol.  SETTINGS should be `treesit-settings'."
  ;;    A setting for exactly this MODE.  The shape is (FLAG INHERIT).
  (let ((self (alist-get mode settings))
        ;; Fallback setting, shape is (FLAG INHERIT).
        (fallback (alist-get t settings))
        ;; Settings for ancestor modes of MODE.  Its shape is
        ;; ((MODE . FLAG)...)
        (applicable (cl-loop for setting in settings
                             for m = (nth 0 setting)
                             for flag = (nth 1 setting)
                             for inherit = (nth 2 setting)
                             if (and (not (eq m t))
                                     (not (eq m mode))
                                     inherit
                                     (provided-mode-derived-p mode m))
                             collect (cons m flag))))
    (cond
     (self (car self))
     ((null applicable) (car fallback))
     (t
      ;; After sort, the most specific setting is at the top.
      (setq applicable
            (cl-sort applicable
                     (lambda (a b)
                       ;; Major mode inheritance has a total ordering
                       ;; right?
                       (provided-mode-derived-p (car a) (car b)))))
      (cdar applicable)))))

(defun treesit-ready-p (mode language &optional quiet)
  "Check that tree-sitter is ready to be used for MODE.

Checks the user setting in `treesit-settings', if user sets
`demand' for MODE, and tree-sitter is not ready, emit a warning
and return nil.  If user chose to activate tree-sitter for MODE
and tree-sitter is ready, return non-nil.  If QUIET is t, no
warning is emitted in any case, if quiet is `message', message
instead of emitting warning.

If MODE is nil, don't check for user setting and assume the
setting is t.

LANGUAGE is languages symbol we want check for availability.  It
can also be a list of language symbols."
  (let ((language-list (if (consp language)
                           language
                         (list language)))
        (activate (if mode
                      (treesit--setting-for-mode mode treesit-settings)
                    t))
        msg)
    ;; Check for each condition and set MSG.
    (if (null activate)
        nil
      (catch 'term
        (when (not (treesit-available-p))
          (setq msg "tree-sitter library is not compiled with Emacs")
          (throw 'term nil))
        (when (> (buffer-size) treesit-max-buffer-size)
          (setq msg "buffer larger than `treesit-max-buffer-size'")
          (throw 'term nil))
        (dolist (lang language-list)
          (pcase-let ((`(,available . ,err)
                       (treesit-language-available-p lang t)))
            (when (not available)
              (setq msg (format "language definition for %s is unavailable (%s): %s"
                                lang (nth 0 err)
                                (string-join
                                 (mapcar (lambda (x) (format "%s" x))
                                         (cdr err))
                                 " ")))
              (throw 'term nil)))))
      ;; Decide if all conditions met and whether emit a warning.
      (if (not msg)
          t
        (when (eq activate 'demand)
          (setq msg (concat "Cannot activate tree-sitter, because " msg))
          (pcase quiet
            ('nil (display-warning 'treesit msg))
            ('message (message "%s" msg))))
        nil))))

(defun treesit-major-mode-setup ()
  "Activate tree-sitter to power major-mode features.

If `treesit-font-lock-settings' is non-nil, setup fontification and
enable `font-lock-mode'.

If `treesit-simple-indent-rules' is non-nil, setup indentation.

If `treesit-defun-type-regexp' is non-nil, setup
`beginning/end-of-defun' functions."
  ;; Font-lock.
  (when treesit-font-lock-settings
    ;; `font-lock-mode' wouldn't setup properly if
    ;; `font-lock-defaults' is nil, see `font-lock-specified-p'.
    ;; And we disable syntax-table-based font-lock by setting the
    ;; KEYWORD-ONLY flag to t, so syntax-table-based font-lock
    ;; doesn't override tree-sitter's fontification.
    (setq-local font-lock-defaults '(nil t))
    (setq-local font-lock-fontify-region-function
                #'treesit-font-lock-fontify-region)
    (font-lock-mode 1)
    (treesit-font-lock-recompute-features))
  ;; Indent.
  (when treesit-simple-indent-rules
    (setq-local indent-line-function #'treesit-indent))
  ;; Navigation.
  (when treesit-defun-type-regexp
    (setq-local beginning-of-defun-function #'treesit-beginning-of-defun)
    (setq-local end-of-defun-function #'treesit-end-of-defun)))

;;; Debugging

(defvar-local treesit--inspect-name nil
  "treesit-inspect-mode uses this to show node name in mode-line.")

(defun treesit-inspect-node-at-point (&optional arg)
  "Show information of the node at point.
If called interactively, show in echo area, otherwise set
`treesit--inspect-name' (which will appear in the mode-line
if `treesit-inspect-mode' is enabled).  Uses the first parser
in (`treesit-parser-list')."
  (interactive "p")
  ;; NODE-LIST contains all the node that starts at point.
  (let* ((node-list
          (cl-loop for node = (treesit-node-at (point))
                   then (treesit-node-parent node)
                   while node
                   if (eq (treesit-node-start node)
                          (point))
                   collect node))
         (largest-node (car (last node-list)))
         (parent (treesit-node-parent largest-node))
         ;; node-list-acending contains all the node bottom-up, then
         ;; the parent.
         (node-list-acending
          (if (null largest-node)
              ;; If there are no nodes that start at point, just show
              ;; the node at point and its parent.
              (list (treesit-node-at (point))
                    (treesit-node-parent
                     (treesit-node-at (point))))
            (append node-list (list parent))))
         (name ""))
    ;; We draw nodes like (parent field-name: (node)) recursively,
    ;; so it could be (node1 field-name: (node2 field-name: (node3))).
    (dolist (node node-list-acending)
      (setq
       name
       (concat
        (if (treesit-node-field-name node)
            (format " %s: " (treesit-node-field-name node))
          " ")
        (if (treesit-node-check node 'named) "(" "\"")
        (or (treesit-node-type node)
            "N/A")
        name
        (if (treesit-node-check node 'named) ")" "\""))))
    (setq treesit--inspect-name name)
    (force-mode-line-update)
    (when arg
      (if node-list
          (message "%s" treesit--inspect-name)
        (message "No node at point")))))

(define-minor-mode treesit-inspect-mode
  "Shows the node that _starts_ at point in the mode-line.

The mode-line displays

    PARENT FIELD-NAME: (CHILD FIELD_NAME: (GRAND-CHILD (...)))

CHILD, GRAND-CHILD, and GRAND-GRAND-CHILD, etc, are nodes that
have their beginning at point.  And PARENT is the parent of
CHILD.

If no node starts at point, i.e., point is in the middle of a
node, then we just display the smallest node that spans point and
its immediate parent.

This minor mode doesn't create parsers on its own.  It simply
uses the first parser in (`treesit-parser-list')."
  :lighter nil
  (if treesit-inspect-mode
      (progn
        (add-hook 'post-command-hook
                  #'treesit-inspect-node-at-point 0 t)
        (add-to-list 'mode-line-misc-info
                     '(:eval treesit--inspect-name)))
    (remove-hook 'post-command-hook
                 #'treesit-inspect-node-at-point t)
    (setq mode-line-misc-info
          (remove '(:eval treesit--inspect-name)
                  mode-line-misc-info))))

(defun treesit-query-validate (language query)
  "Check if QUERY is valid for LANGUAGE.
If QUERY is invalid, display the query in a popup buffer, jumps
to the offending pattern and highlight the pattern."
  (cl-assert (or (consp query) (stringp query)))
  (let ((buf (get-buffer-create "*tree-sitter check query*")))
    (with-temp-buffer
      (treesit-parser-create language)
      (condition-case err
          (progn (treesit-query-capture language query)
                 (message "QUERY is valid"))
        (treesit-query-error
         (with-current-buffer buf
           (let* ((data (cdr err))
                  (message (nth 0 data))
                  (start (nth 1 data)))
             (erase-buffer)
             (insert (treesit-query-expand query))
             (goto-char start)
             (search-forward " " nil t)
             (put-text-property start (point) 'face 'error)
             (message "%s" (buffer-substring start (point)))
             (goto-char (point-min))
             (insert (format "%s: %d\n" message start))
             (forward-char start)))
         (pop-to-buffer buf))))))

;;; Etc

(declare-function find-library-name "find-func.el")
(defun treesit--check-manual-coverage ()
  "Print tree-sitter functions missing from the manual in message buffer."
  (interactive)
  (require 'find-func)
  (let ((functions-in-source
         (with-temp-buffer
           (insert-file-contents (find-library-name "treesit"))
           (cl-remove-if
            (lambda (name) (string-match "treesit--" name))
            (cl-sort
             (save-excursion
               (goto-char (point-min))
               (cl-loop while (re-search-forward
                               "^(defun \\([^ ]+\\)" nil t)
                        collect (match-string-no-properties 1)))
             #'string<))))
        (functions-in-manual
         (with-temp-buffer
           (insert-file-contents (expand-file-name
                                  "doc/lispref/parsing.texi"
                                  source-directory))
           (insert-file-contents (expand-file-name
                                  "doc/lispref/modes.texi"
                                  source-directory))
           (cl-sort
            (save-excursion
              (goto-char (point-min))
              (cl-loop while (re-search-forward
                              "^@defun \\([^ ]+\\)" nil t)
                       collect (match-string-no-properties 1)))
            #'string<))))
    (message "Missing: %s"
             (string-join
              (cl-remove-if
               (lambda (name) (member name functions-in-manual))
               functions-in-source)
              "\n"))))

(provide 'treesit)

;;; treesit.el ends here
