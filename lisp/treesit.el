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
;; Note to self: we don't create parsers automatically in any provided
;; functions if we don't know what language to use.

;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x)) ; For `string-join'.
(require 'cl-seq)
(require 'font-lock)

;;; Activating tree-sitter

(defgroup treesit
  nil
  "Tree-sitter is an incremental parser."
  :group 'tools)

(defcustom treesit-max-buffer-size (* 4 1024 1024)
  "Maximum buffer size for enabling tree-sitter parsing."
  :type 'integer)

(defun treesit-available-p ()
  "Return non-nil if tree-sitter features are available."
  (fboundp 'treesit-parser-create))

(defun treesit-can-enable-p ()
  "Return non-nil if current buffer can activate tree-sitter.
Currently this function checks whether tree-sitter is available
and the buffer size."
  (and (treesit-available-p)
       (< (buffer-size) treesit-max-buffer-size)))

;;; Parser API supplement

(defun treesit-parse-string (string language)
  "Parse STRING using a parser for LANGUAGE.
Return the root node of the syntax tree."
  (with-temp-buffer
    (insert string)
    (treesit-parser-root-node
     (treesit-parser-create language))))

(defun treesit-language-at (point)
  "Return the language used at POINT."
  (cl-loop for parser in (treesit-parser-list)
           if (treesit-node-on point point parser)
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

(defun treesit-node-at (point &optional parser-or-lang named)
  "Return the smallest node that starts at or after POINT.

\"Starts at or after POINT\" means the start of the node is
greater or larger than POINT.  Return nil if none find.  If NAMED
non-nil, only look for named node.

If PARSER-OR-LANG is nil, use the first parser in
(`treesit-parser-list'); if PARSER-OR-LANG is a parser, use
that parser; if PARSER-OR-LANG is a language, find a parser using
that language in the current buffer, and use that."
  (let ((node (if (treesit-parser-p parser-or-lang)
                  (treesit-parser-root-node parser-or-lang)
                (treesit-buffer-root-node parser-or-lang))))
    ;; TODO: We might want a `treesit-node-descendant-for-pos' in C.
    (while (cond ((and node (< (treesit-node-end node) point))
                  (setq node (treesit-node-next-sibling node))
                  t)
                 ((treesit-node-child node 0 named)
                  (setq node (treesit-node-child node 0 named))
                  t)))
    node))

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
Font-locking and indenting code uses functions in this alist to
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

;;; Font-lock

(defvar-local treesit-font-lock-settings nil
  "A list of SETTINGs for treesit-based fontification.

The exact format of this variable is considered internal.  One
should always use `treesit-font-lock-rules' to set this variable.

Each SETTING is of form

    (LANGUAGE QUERY)

Each SETTING controls one parser (often of different language).
LANGUAGE is the language symbol.  See Info node `(elisp)Language
Definitions'.

QUERY is either a string query, a sexp query, or a compiled
query.  See Info node `(elisp)Pattern Matching' for how to write
a query in either string or s-expression form.  When using
repeatedly, a compiled query is much faster than a string or sexp
one, so it is recommend to compile your queries if it will be
used over and over.")

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
     \\='((true) @font-lock-constant-face
       (false) @font-lock-constant-face)
     :language \\='html
     \"(script_element) @font-lock-builtin-face\")

For each QUERY, a :language keyword is required.  Currently the
only recognized keyword is :language.

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
  (let (;; Tracks the current language that following queries will
        ;; apply to.
        (current-language nil)
        ;; The list this function returns.
        (result nil))
    (while args
      (let ((token (pop args)))
        (pcase token
          (:language
           (let ((lang (pop args)))
             (when (or (not (symbolp lang)) (null lang))
               (signal 'wrong-type-argument `(symbolp ,lang)))
             (setq current-language lang)))
          ((pred treesit-query-p)
           (when (null current-language)
             (signal 'treesit-error
                     `("Language unspecified, use :language keyword to specify a language for this query" ,token)))
           (if (treesit-compiled-query-p token)
               (push `(,current-language token) result)
             (push `(,current-language
                     ,(treesit-query-compile current-language token))
                   result))
           ;; Clears any configurations set for this query.
           (setq current-language nil))
          (_ (signal 'treesit-error
                     `("Unexpected value" token))))))
    (nreverse result)))

(defun treesit-font-lock-fontify-region (start end &optional loudly)
  "Fontify the region between START and END.
If LOUDLY is non-nil, message some debugging information."
  (treesit-update-ranges start end)
  (font-lock-unfontify-region start end)
  (dolist (setting treesit-font-lock-settings)
    (when-let* ((language (nth 0 setting))
                (match-pattern (nth 1 setting))
                (parser (treesit-parser-create language)))
      (when-let ((node (treesit-node-on start end parser)))
        (let ((captures (treesit-query-capture
                         node match-pattern
                         ;; Specifying the range is important. More
                         ;; often than not, NODE will be the root
                         ;; node, and if we don't specify the range,
                         ;; we are basically querying the whole file.
                         start end)))
          (with-silent-modifications
            (dolist (capture captures)
              (let* ((face (car capture))
                     (node (cdr capture))
                     (start (treesit-node-start node))
                     (end (treesit-node-end node)))
                (cond ((facep face)
                       (put-text-property start end 'face face))
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
  (setq-local font-lock-fontify-region-function
              #'treesit-font-lock-fontify-region)
  ;; If we don't set `font-lock-defaults' to some non-nil value,
  ;; font-lock doesn't enable properly (the font-lock-mode-internal
  ;; doesn't run).  See `font-lock-add-keywords'.
  (when (and font-lock-mode
             (null font-lock-keywords)
             (null font-lock-defaults))
    (font-lock-mode -1)
    (setq-local font-lock-defaults '(nil t))
    (font-lock-mode 1)))

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
    (predicate side &optional all backward up)
  "Search forward for a node and move to it.

Stops at the first node after point that matches PREDICATE.
PREDICATE can be either a regexp that matches against each node's
type case-insensitively, or a function that takes a node and
returns nil/non-nil for match/no match.

If a node matches, move to that node and return the node,
otherwise return nil.  SIDE controls whether we move to the start
or end of the matches node, it can be either \\='start or
\\='end.

ALL, BACKWARD, and UP are the same as in `treesit-search-forward'."
  (let ((node (treesit-node-at (point)))
        (start (point)))
    ;; When searching forward, it is possible for (point) < start,
    ;; because `treesit-search-forward' goes to parents.
    (while (and node (if backward
                         (>= (point) start)
                       (<= (point) start)))
      (setq node (treesit-search-forward
                  node predicate all backward up))
      (if-let ((pos (pcase side
                      ('start (treesit-node-start node))
                      ('end (treesit-node-end node)))))
          (goto-char pos)))
    ;; If we made reverse progress, go back to where we started.
    (when (if backward
              (>= (point) start)
            (<= (point) start))
      (goto-char start))
    node))

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
(defun treesit--check-manual-covarage ()
  "Print tree-sitter functions missing from the manual in message buffer."
  (interactive)
  (require 'find-func)
  (let ((functions-in-source
         (with-temp-buffer
           (insert-file-contents (find-library-name "tree-sitter"))
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
