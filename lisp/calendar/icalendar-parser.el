;;; icalendar-parser.el --- Parse iCalendar grammar  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Richard Lawrence <rwl@recursewithless.net>
;; Created: October 2024
;; Keywords: calendar
;; Human-Keywords: calendar, iCalendar

;; This file is part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines regular expressions, constants and functions that
;; implement the iCalendar grammar according to RFC5545.
;;
;; iCalendar data is grouped into *components*, such as events or
;; to-do items.  Each component contains one or more *content lines*,
;; which each contain a *property* name and its *value*, and possibly
;; also property *parameters* with additional data that affects the
;; interpretation of the property.
;;
;; The macros `ical:define-type', `ical:define-param',
;; `ical:define-property' and `ical:define-component', defined in
;; icalendar-macs.el, each create rx-style regular expressions for one
;; of these categories in the grammar and are used here to define the
;; particular value types, parameters, properties and components in the
;; standard as type symbols.  These type symbols store all the metadata
;; about the relevant types, and are used for type-based dispatch in the
;; parser and printer functions.  In the abstract syntax tree, each node
;; contains a type symbol naming its type.  A number of other regular
;; expressions which encode basic categories of the grammar are also
;; defined in this file.
;;
;; The following functions provide the high-level interface to the parser:
;;
;;   `icalendar-parse-and-index'
;;   `icalendar-parse'
;;   `icalendar-parse-calendar'
;;   `icalendar-parse-component'
;;   `icalendar-parse-property'
;;   `icalendar-parse-params'
;;
;; The format of the abstract syntax tree which these functions create
;; is documented in icalendar-ast.el.  Nodes in this tree can be
;; serialized to iCalendar format with the corresponding printer
;; functions:
;;
;;   `icalendar-print-calendar-node'
;;   `icalendar-print-component-node'
;;   `icalendar-print-property-node'
;;   `icalendar-print-params'

;;; Code:

(require 'icalendar)
(eval-when-compile (require 'icalendar-macs))
(require 'icalendar-ast)
(eval-when-compile (require 'cl-lib))
(require 'subr-x)
(require 'seq)
(require 'rx)
(require 'calendar)
(require 'time-date)
(require 'simple)
(require 'help-mode)

;;; Customization
(defgroup icalendar-parser nil
  "iCalendar parsing options."
  :version "31.1"
  :group 'icalendar
  :prefix 'icalendar)

(defcustom ical:parse-strictly nil
  "When non-nil, iCalendar data will be parsed strictly.

By default, the iCalendar parser accepts certain harmless deviations
from RFC5545 that are common in real-world data (e.g., unescaped commas
in text values).  Setting this to t will cause the parser to produce
errors instead of silently accepting such data."
  :version "31.1"
  :type '(choice (const :tag "Ignore minor errors" nil)
                 (const :tag "Parse strictly" t)))

;;; Functions for folding and unfolding
;;
;; According to RFC5545, iCalendar content lines longer than 75 octets
;; should be *folded* by inserting extra line breaks and leading
;; whitespace to continue the line.  Such lines must be *unfolded*
;; before they can be parsed.  Unfolding can only reliably happen
;; before Emacs decodes a region of text, because decoding potentially
;; replaces the CR-LF line endings which terminate content lines.
;; Programs that can control when decoding happens should use the
;; stricter `ical:unfold-undecoded-region' to unfold text; programs
;; that must work with decoded data should use the looser
;; `ical:unfold-region'. `ical:fold-region' will fold content lines
;; using line breaks appropriate to the buffer's coding system.
;;
;; All the parsing-related code belows assumes that lines have
;; already been unfolded if necessary.
(defcustom ical:pre-unfolding-hook nil
  "Hook run before unfolding iCalendar data.

The functions in this hook will be run before the iCalendar data is
\"unfolded\", i.e., before whitespace introduced for breaking long lines
is removed (see `icalendar-unfold-region' and
`icalendar-unfold-undecoded-region').  If you routinely receive
iCalendar data that is not correctly folded, you can add functions to
this hook which clean up that data before unfolding is attempted.

Each function should accept zero arguments and should perform its
operation on the entire current buffer."
  :version "31.1"
  :type '(hook)
  :options '(ical:fix-line-endings))

(defun ical:fix-line-endings ()
  "Convert all line endings to LF.
This function is intended to be used from `icalendar-pre-unfolding-hook'
(which see) to make files with inconsistent line endings parseable."
  (when ical:parse-strictly
    (ical:warn
     (concat "Converting line endings to LF causes parsing "
             "errors when `icalendar-parse-strictly' is non-nil.")))
  (goto-char (point-min))
  (while (re-search-forward "\r\n?" nil t)
    (replace-match "\n")))

(defun ical:unfold-undecoded-region (start end &optional buffer)
  "Unfold an undecoded region in BUFFER between START and END.
If omitted, BUFFER defaults to the current buffer.

\"Unfolding\" means removing the whitespace characters inserted to
continue lines longer than 75 octets (see `icalendar-fold-region'
for the folding operation).  RFC5545 specifies these whitespace
characters to be a CR-LF sequence followed by a single space or
tab character.  Unfolding can only be done reliably before a
region is decoded, since decoding potentially replaces CR-LF line
endings.

When `icalendar-parse-strictly' is non-nil, this function searches
strictly for CR-LF sequences and will fail if they have already been
replaced, so it should only be called with a region that has not yet
been decoded.  Otherwise, it also searches for folds containing
Unix-style LF line endings, since these are common in real data."
  (with-current-buffer (or buffer (current-buffer))
    (let ((modp (buffer-modified-p)))
      (with-restriction start end
        (run-hooks 'ical:pre-unfolding-hook)
        (goto-char (point-min))
        ;; Testing reveals that a *significant* amount of real-world data
        ;; does not use CR-LF line endings, even if it is otherwise
        ;; OK.  So unless we're explicitly parsing strictly, we allow the
        ;; CR to be missing, as we do in `icalendar-unfold-region':
        (let ((fold (if ical:parse-strictly (rx (seq "\r\n" (or " " "\t")))
                      (rx (seq (zero-or-one "\r") "\n" (or " " "\t"))))))
          (while (re-search-forward fold nil t)
            (replace-match "" nil nil)))
        ;; merely unfolding should not mark the buffer as modified;
        ;; this prevents querying the user before killing it:
        (set-buffer-modified-p modp)))))

(defun ical:unfold-region (start end &optional buffer)
  "Unfold region between START and END in BUFFER (default: current buffer).

\"Unfolding\" means removing the whitespace characters inserted to
continue lines longer than 75 octets (see `icalendar-fold-region'
for the folding operation).

Returns the new end position after unfolding finishes.  Thus this
function is a suitable FROM-FN (decoding function) for `format-alist'.

WARNING: Unfolding can only be done reliably before text is
decoded, since decoding potentially replaces CR-LF line endings.
Unfolding an already-decoded region could lead to unexpected
results, such as displaying multibyte characters incorrectly,
depending on the contents and the coding system used.

This function attempts to do the right thing even if the region
is already decoded.  If it is still undecoded, it is better to
call `icalendar-unfold-undecoded-region' directly instead, and
decode it afterward."
  ;; TODO: also make this a command so it can be run manually?
  (with-current-buffer (or buffer (current-buffer))
    (let ((was-multibyte enable-multibyte-characters)
          (start-char (position-bytes start))
          (end-char (position-bytes end))
          (end-marker (make-marker)))
      ;; set a marker at the original end position so we can return
      ;; the updated position later:
      (set-marker end-marker end)
      ;; we put the buffer in unibyte mode and later restore its
      ;; previous state, so that if the buffer was already multibyte,
      ;; any multibyte characters where line folds broke up their
      ;; bytes can be reinterpreted:
      (set-buffer-multibyte nil)
      (with-restriction start-char end-char
        (run-hooks 'ical:pre-unfolding-hook)
        (goto-char (point-min))
        ;; since we can't be sure that line folds have a leading CR
        ;; in already-decoded regions, do the best we can:
        (while (re-search-forward (rx (seq (zero-or-one "\r") "\n"
                                           (or " " "\t")))
                                  nil t)
          (replace-match "" nil nil)))
      ;; restore previous state, possibly reinterpreting characters:
      (set-buffer-multibyte was-multibyte)
      ;; return the new end of the region, for format.el conversion:
      (marker-position end-marker))))

(defun ical:unfolded-buffer-from-region (start end &optional buffer)
  "Create a new, unfolded buffer with the same contents as the region.

Copies the buffer contents between START and END (in BUFFER, if
provided) to a new buffer and performs line unfolding in the new buffer
with `icalendar-unfold-region'.  That function can in some cases have
undesirable effects; see its docstring.  If BUFFER is visiting a file, it
may be better to reload its contents from that file and perform line
unfolding before decoding; see `icalendar-unfolded-buffer-from-file'.
Returns the new buffer."
  (let* ((old-buffer (or buffer (current-buffer)))
         (contents (with-current-buffer old-buffer
                     (buffer-substring start end)))
         (uf-buffer (generate-new-buffer ;; TODO: again, move to modeline?
                     (concat " *UNFOLDED:" (buffer-name old-buffer)))))
    (with-current-buffer uf-buffer
      (insert contents)
      (ical:unfold-region (point-min) (point-max))
      ;; ensure we'll use CR-LF line endings on write, even if they weren't
      ;; in the source data.  The standard also says UTF-8 is the default
      ;; encoding, so use 'prefer-utf-8-dos when last-coding-system-used
      ;; is nil.
      (setq buffer-file-coding-system
            (if last-coding-system-used
                (coding-system-change-eol-conversion last-coding-system-used
                                                     'dos)
              'prefer-utf-8-dos))
      ;; inhibit auto-save-mode, which will otherwise create save
      ;; files containing the unfolded data; these are probably
      ;; not useful to the user and a nuisance when running tests:
      (auto-save-mode -1))
    uf-buffer))

(defun ical:unfolded-buffer-from-buffer (buffer)
  "Create a new, unfolded buffer with the same contents as BUFFER.

Copies the contents of BUFFER to a new buffer and performs line
unfolding there with `icalendar-unfold-region'.  That function can in
some cases have undesirable effects; see its docstring.  If BUFFER is
visiting a file, it may be better to reload its contents from that file
and perform line unfolding before decoding; see
`icalendar-unfolded-buffer-from-file'.  Returns the new buffer."
  (with-current-buffer buffer
    (ical:unfolded-buffer-from-region (point-min) (point-max) buffer)))

(defun ical:find-unfolded-buffer-visiting (filename)
  "Find an existing unfolded buffer visiting FILENAME."
  ;; FIXME: I was previously using
  ;;   (find-buffer-visiting filename #'ical:unfolded-p)
  ;; for this, but found that it would sometimes return nil even when an
  ;; unfolded buffer already existed for FILENAME, leading to buffers
  ;; getting unfolded and parsed multiple times.  Hence this kludge.
  (catch 'unfolded
    (let ((exp-name (expand-file-name filename)))
      (dolist (buf (match-buffers "UNFOLDED"))
        (when (and (equal exp-name (buffer-file-name buf))
                   (ical:unfolded-p buf))
          (throw 'unfolded buf))))))

(defun ical:unfolded-buffer-from-file (filename &optional visit beg end)
    "Return a buffer visiting FILENAME with unfolded lines.

If an unfolded buffer is already visiting FILENAME, return
it.  Otherwise, create a new buffer with the contents of FILENAME and
perform line unfolding with `icalendar-unfold-undecoded-region', then
decode the buffer, setting an appropriate value for
`buffer-file-coding-system', and return the new buffer.  Optional
arguments VISIT, BEG, END are as in `insert-file-contents'."
    (unless (and (file-exists-p filename)
                 (file-readable-p filename))
      (error "File cannot be read: %s" filename))
    (or (ical:find-unfolded-buffer-visiting filename)
        (let ((uf-buffer
               (generate-new-buffer
                (concat " *UNFOLDED:" (file-name-nondirectory filename)))))
          (with-current-buffer uf-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally filename visit beg end t)
            (ical:unfold-undecoded-region (point-min) (point-max))
            ;; now proceed with decoding:
            (set-buffer-multibyte t)
            (decode-coding-inserted-region (point-min) (point-max) filename)
            ;; ensure we'll use CR-LF line endings on write, even if they weren't
            ;; in the source data.  The standard also says UTF-8 is the default
            ;; encoding, so use 'prefer-utf-8-dos when last-coding-system-used
            ;; is nil.  FIXME: for some reason, this doesn't seem to run at all!
            (setq buffer-file-coding-system
                  (if last-coding-system-used
                      (coding-system-change-eol-conversion last-coding-system-used
                                                           'dos)
                    'prefer-utf-8-dos))
            ;; restore buffer name after renaming by set-visited-file-name:
            (let ((bname (buffer-name)))
              (set-visited-file-name filename t)
              (rename-buffer bname))
            ;; merely unfolding should not mark the buffer as modified;
            ;; this prevents querying the user before killing it:
            (set-buffer-modified-p nil)
            ;; inhibit auto-save-mode, which will otherwise create save
            ;; files containing the unfolded data; these are probably
            ;; not useful to the user and a nuisance when running tests:
            (auto-save-mode -1))
        uf-buffer)))

(defun ical:fold-region (begin end &optional annotate-only use-tabs)
  "Fold content lines between BEGIN and END when longer than 75 octets.

\"Folding\" means inserting a line break and a single space
character at the beginning of the new line.  If USE-TABS is
non-nil, insert a tab character instead of a single space.

RFC5545 specifies that lines longer than 75 *octets* (excluding
the line-ending CR-LF sequence) must be folded, and allows that
some implementations might fold lines in the middle of a
multibyte character.  This function takes care not to do that in a
buffer where `enable-multibyte-characters' is non-nil, and only
folds between character boundaries.  If the buffer is in unibyte
mode, however, and contains undecoded multibyte data, it may fold
lines in the middle of a multibyte character.

By default, this function modifies the region by inserting line folds.
If the optional argument ANNOTATE-ONLY is non-nil, it will instead leave
the buffer unmodified, and return a list of \"annotations\"
\(POSITION . LINE-FOLD), indicating where line folds in the region should
be inserted.  This output is suitable for a function in
`write-region-annotation-functions'; `icalendar-folding-annotations'
is a wrapper for this function which can be added to that list."
  ;; TODO: also make this a command so it can be run manually?
  (let (annotations)
    (save-excursion
      (goto-char begin)
      (when (not (bolp))
        (let ((inhibit-field-text-motion t))
          (beginning-of-line)))
      (let ((bol (point))
            (eol (make-marker))
            (reg-end (make-marker))
            (line-fold (if use-tabs "\n\t" "\n ")))
        (set-marker reg-end end)
        (while (< bol reg-end)
          (let ((inhibit-field-text-motion t))
            (end-of-line))
          (set-marker eol (point))
          (when (< 75 (- (position-bytes (marker-position eol))
                         (position-bytes bol)))
            (goto-char
             ;; the max of 75 excludes the two CR-LF
             ;; characters we're about to add:
             (byte-to-position (+ 75 (position-bytes bol))))
            (if annotate-only
                (push (cons (point) line-fold) annotations)
              (insert line-fold))
            (set-marker eol (point)))
          (setq bol (goto-char (1+ eol))))))
    ;; Return annotations, or nil if we modified the buffer directly:
    (nreverse annotations)))

(defun ical:folding-annotations (start end &optional buffer)
  "Return a list of annotations for folding lines in the region.

This function is a wrapper for `icalendar-fold-region' that provides the
interface to be used from `write-region-annotation-functions', which
see."
  ;; start may be nil or a string; see `write-region'
  (if (stringp start)
      (let ((buf (generate-new-buffer " *icalendar-folded*")))
        (set-buffer buf)
        (insert start)
        (ical:fold-region (point-min) (point-max) t))

    (when (bufferp buffer) (set-buffer buffer))
    (ical:fold-region (or start (point-min))
                      (if start end (point-max))
                      t)))

(defun ical:contains-folded-lines-p (&optional buffer)
  "Return non-nil if BUFFER contains folded content lines.

BUFFER defaults to the current buffer.  Folded content lines need to be
unfolded before parsing the buffer or performing syntax
highlighting.  Returns the position at the end of the first fold, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (rx (seq line-start (or " " "\t")))
                         nil t))))

(defun ical:unfolded-p (&optional buffer)
  "Return non-nil if BUFFER does not contain any folded content lines.
BUFFER defaults to the current buffer."
  (not (ical:contains-folded-lines-p buffer)))

(defun ical:contains-unfolded-lines-p (&optional buffer)
  "Return non-nil if BUFFER contains long content lines that should be folded.

Lines longer than 75 bytes need to folded before saving or transmitting
the data in BUFFER (default: current buffer).  If BUFFER contains such
lines, return the position at the beginning of the first line that
requires folding."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((bol (point))
            (eol (make-marker)))
        (catch 'unfolded-line
          (while (< bol (point-max))
            (let ((inhibit-field-text-motion t))
              (end-of-line))
            (set-marker eol (point))
            ;; the max of 75 excludes the two CR-LF characters
            ;; after position eol:
            (when (< 75 (- (position-bytes (marker-position eol))
                           (position-bytes bol)))
              (throw 'unfolded-line bol))
            (setq bol (goto-char (1+ eol))))
          nil)))))

(defun ical:folded-p (&optional buffer)
  "Return non-nil if BUFFER contains no content lines that require folding.
BUFFER defaults to the current buffer."
  (not (ical:contains-unfolded-lines-p buffer)))


;; Parsing-related code starts here.  All the parsing code assumes that
;; content lines have already been unfolded.

;;;; Error handling:

;; Errors at the parsing stage:
;; e.g. value does not match expected regex
(define-error 'ical:parse-error "Could not parse iCalendar data" 'ical:error)

(cl-defun ical:signal-parse-error (msg &key (buffer (current-buffer))
                                       (position (point))
                                       (severity 2)
                                       (line (line-number-at-pos position))
                                       column restart-at)
  (signal 'ical:parse-error
          (list :message msg
                :line line
                :column column
                :severity severity
                :position position
                :buffer buffer
                :restart-at restart-at)))

(defun ical:handle-parse-error (err-data &optional skip-msg err-buffer)
  (let* ((err-sym (car err-data))
         (err-plist (cdr err-data))
         (buf (plist-get err-plist :buffer))
         (restart-pos (plist-get err-plist :restart-at))
         (new-msg
          (concat (plist-get err-plist :message)
                  "..."
                  (cond (skip-msg skip-msg)
                        (restart-pos (format "skipping to %d" restart-pos))
                        (t "skipping")))))
    (setq err-plist (plist-put err-plist :message new-msg))
    (setq err-plist (plist-put err-plist :severity 1))
    (ical:handle-generic-error (cons err-sym err-plist) err-buffer)
    (when restart-pos
      (with-current-buffer buf
        (goto-char restart-pos)))))

;; Errors at the printing stage:
;; e.g. default print function doesn't know how to print value
(define-error 'ical:print-error "Unable to print iCalendar data" 'ical:error)

(cl-defun ical:signal-print-error (msg &key (severity 2) node)
  (signal 'ical:print-error
          (list :message msg
                :node node
                :buffer (ical:ast-node-meta-get :buffer node)
                :severity severity
                :position (ical:ast-node-meta-get :begin node))))

(defun ical:handle-print-error (err-data &optional skip-msg err-buffer)
  (let* ((err-sym (car err-data))
         (err-plist (cdr err-data))
         (new-msg (concat (plist-get err-plist :message)
                          "..."
                          (or skip-msg "skipping"))))
    (setq err-plist (plist-put err-plist :message new-msg))
    (setq err-plist (plist-put err-plist :severity 1))
    (ical:handle-generic-error (cons err-sym err-plist) err-buffer))
  (ical:handle-generic-error err-data err-buffer))

;;;; Some utilities:
(defun ical:parse-from-string (type s)
  "Parse string S to an iCalendar syntax node of type TYPE.
S should not contain folded content lines."
  ;; TODO: support unfolding?
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (cond ((ical:component-type-symbol-p type)
           (ical:parse-component (point-max)))
          ((ical:property-type-symbol-p type)
           (ical:parse-property (point-max)))
          ((ical:param-type-symbol-p type)
           (unless (looking-at-p ";")
             (insert ";")
             (backward-char))
           (ical:parse-params (point-max)))
          ((ical:value-type-symbol-p type)
           (ical:parse-value-node type (point-max)))
          (t
           (error "Don't know how to parse type %s" type)))))

(defun ical:parse-one-of (types limit)
  "Parse a value, from point up to LIMIT, of one of the TYPES.

TYPES should be a list of type symbols.  For each type in TYPES, the
parser function associated with that type will be called at point.  The
return value of the first successful parser function is returned.  If
none of the parser functions are able to parse a value, an
`icalendar-parse-error' is signaled."
  (let* ((value nil)
         (start (point))
         (type (car types))
         (parser (get type 'ical:value-parser))
         (rest (cdr types)))
    (while (and parser (not value))
      (condition-case nil
          (setq value (funcall parser limit))
        (ical:parse-error
         ;; value of this type not found, so try again:
         (goto-char start)
         (setq type (car rest)
               rest (cdr rest)
               parser (get type 'ical:value-parser)))))
    (unless value
      (ical:signal-parse-error
       (format "Unable to parse any of %s between %d and %d" types start limit)
       :position start))
    value))

(defun ical:read-list-with (reader string
                            &optional value-regex separators omit-nulls trim)
  "Read a list of values from STRING with READER.

READER should be a reader function that accepts a single string argument.
SEPARATORS, OMIT-NULLS, and TRIM are as in `split-string'.
SEPARATORS defaults to \"[^\\][,;]\".  TRIM defaults to matching a
double quote character.

VALUE-REGEX should be a regular expression if READER assumes that
individual substrings in STRING have previously been matched
against this regex.  In this case, each value in S is placed in a
temporary buffer and the match against VALUE-REGEX is performed
before READER is called."
  (let* ((wrapped-reader
           (if (not value-regex)
               ;; no need for temp buffer:
               reader
             ;; match the regex in a temp buffer before calling reader:
             (lambda (s)
               (with-temp-buffer
                 (insert s)
                 (goto-char (point-min))
                 (unless (looking-at value-regex)
                   (ical:signal-parse-error
                    (format "Expected list of values matching '%s'" value-regex)))
                 (funcall reader (match-string 0))))))
         (seps (or separators "[^\\][,;]"))
         (trm (or trim "\""))
         (raw-values (split-string string seps omit-nulls trm)))

    (unless (functionp reader)
      (signal 'ical:parser-error
              (list (format "`%s' is not a reader function" reader))))

    (mapcar wrapped-reader raw-values)))

(defun ical:read-list-of (type string
                          &optional separators omit-nulls trim)
  "Read a list of values of type TYPE from STRING.

TYPE should be a value type symbol.  The reader function
associated with that type will be called to read the successive
values in STRING, and the values will be returned as a list of
syntax nodes.

SEPARATORS, OMIT-NULLS, and TRIM are as in `split-string' and
will be passed on, if provided, to `icalendar-read-list-with'."
  (let* ((reader (lambda (s) (ical:read-value-node type s)))
         (val-regex (rx-to-string (get type 'ical:value-rx))))
    (ical:read-list-with reader string val-regex
                         separators omit-nulls trim)))

(defun ical:list-of-p (list type)
  "Return non-nil if each value in LIST satisfies TYPE.
TYPE should be a type specifier for `cl-typep'."
  (seq-every-p (lambda (val) (cl-typep val type)) list))

(defun ical:default-value-printer (val)
  "Default printer for a *single* property or parameter value.

If VAL is a string, just return it unchanged.

Otherwise, VAL should be a syntax node representing a value.  In
that case, return the original string value if another was
substituted at parse time, or look up the printer function for
the node's type and call it on the value inside the node.

For properties and parameters that only allow a single value,
this function should be a sufficient value printer.  It is not
sufficient for those that allow lists of values, or which have
other special requirements like quoting or escaping."
  (cond ((stringp val) val)
        ((and (ical:ast-node-p val)
              (get (ical:ast-node-type val) 'ical:value-printer))
         (or (ical:ast-node-meta-get :original-value val)
             (let* ((stored-value (ical:ast-node-value val))
                    (type (ical:ast-node-type val))
                    (printer (get type 'ical:value-printer)))
               (funcall printer stored-value))))
        ;; TODO: other cases to make things easy?
        ;; e.g. symbols print as their names?
        (t (ical:signal-print-error
            (format "Don't know how to print value: %s" val)))))


;;; Section 3.1: Content lines

;; Regexp constants for parsing:

;; In the following regexps and define-* declarations, because
;; Emacs does not have named groups, we observe the following
;; convention so that the regexps can be combined in sensible ways:
;;
;; - Groups 1 through 5 are reserved for the highest-level regexes
;;   created by define-param, define-property and define-component and
;;   used in the match-* functions.  Group 1 always represents a 'key'
;;   (e.g. param or property name), group 2 always represents a
;;   correctly parsed value for that key, and group 3 (if matched) an
;;   invalid or unknown value.
;;
;;   Groups 4 and 5 are reserved for other information in these
;;   highest-level regexes, such as the parameter string between a
;;   property name and its value, or unrecognized values allowed by
;;   the standard and required to be treated like a default value.
;;
;; - Groups 6 through 10 are currently unused
;; - Groups 11 through 20 are reserved for significant sub-expressions
;;   of individual value expressions, e.g. the number of weeks in a
;;   duration value.  The various read-* functions rely on these groups
;;   when converting iCalendar data to Elisp data structures.

(rx-define ical:iana-token
  (one-or-more (any "A-Za-z0-9" "-")))

(rx-define ical:x-name
  (seq "X-"
       (zero-or-one (>= 3 (any "A-Za-z0-9")) "-") ; Vendor ID
       (one-or-more (any "A-Za-z0-9" "-")))) ; Name

(rx-define ical:name
  (or ical:iana-token ical:x-name))

(rx-define ical:crlf
  (seq #x12 #xa))

(rx-define ical:control
  ;; All the controls except HTAB
  (any (#x00 . #x08) (#x0A . #x1F) #x7F))

;; TODO: double check that "nonascii" class actually corresponds to
;; the range in the standard
(rx-define ical:safe-char
  ;; Any character except ical:control, ?\", ?\;, ?:, ?,
  (any #x09 #x20 #x21  (#x23 . #x2B) (#x2D . #x39) (#x3C . #x7E) nonascii))

(rx-define ical:qsafe-char
  ;; Any character except ical:control and ?\"
  (any #x09 #x20 #x21 (#x23 . #x7E) nonascii))

(rx-define ical:quoted-string
  (seq ?\" (zero-or-more ical:qsafe-char) ?\"))

(rx-define ical:paramtext
  ;; RFC5545 allows *zero* characters here, but that would mean we could
  ;; have parameters like ;FOO=;BAR="somethingelse", and what would then
  ;; be the value of FOO?  I see no reason to allow this and it breaks
  ;; parameter parsing so I have required at least one char here
  (one-or-more ical:safe-char))

(rx-define ical:param-name
  (or ical:iana-token ical:x-name))

(rx-define ical:param-value
  (or ical:paramtext ical:quoted-string))

(rx-define ical:value-char
  (any #x09 #x20 (#x21 . #x7E) nonascii))

(rx-define ical:value
  (zero-or-more ical:value-char))

;; some helpers for brevity, not defined in the standard:
(rx-define ical:comma-list (item-rx)
  (seq item-rx
       (zero-or-more (seq ?, item-rx))))

(rx-define ical:semicolon-list (item-rx)
  (seq item-rx
       (zero-or-more (seq ?\; item-rx))))


;;; Section 3.3: Property Value Data Types

;; Note: These definitions are here (out of order with respect to the
;; standard) because a few of them are already required for property
;; parameter definitions (section 3.2) below.

(defvar ical:value-types nil ;; populated by define-type
  "Alist mapping value type strings to type symbols.
Value type strings are those which can appear in `icalendar-valuetypeparam'
parameters and specify the type of a property's value.")

(defun ical:read-value-node (type s)
  "Read an iCalendar value of type TYPE from string S to a syntax node.
Returns a syntax node containing the value."
  (let ((reader (get type 'ical:value-reader)))
    (ical:make-ast-node type (list :value (funcall reader s)))))

(defun ical:parse-value-node (type limit)
  "Parse an iCalendar value of type TYPE from point up to LIMIT.
Returns a syntax node containing the value."
  (let ((value-regex (rx-to-string (get type 'ical:value-rx))))

    (unless (re-search-forward value-regex limit t)
      (ical:signal-parse-error
       (format "No %s value between %d and %d" type (point) limit)))

    (let ((begin (match-beginning 0))
          (end (match-end 0))
          (node (ical:read-value-node type (match-string 0))))
      (ical:ast-node-meta-set node :buffer (current-buffer))
      (ical:ast-node-meta-set node :begin begin)
      (ical:ast-node-meta-set node :end end)

      node)))

(defun ical:print-value-node (node)
  "Serialize an iCalendar syntax NODE containing a value to a string."
  (let* ((type (ical:ast-node-type node))
         (value-printer (get type 'ical:value-printer)))
    (funcall value-printer (ical:ast-node-value node))))

(defun ical:printable-value-type-symbol-p (symbol)
  "Return non-nil if SYMBOL represents a printable iCalendar value type.

This means that SYMBOL names a type for a property or parameter value
defined by `icalendar-define-type' which has a print name (mainly for
use in `icalendar-valuetypeparam' parameters).  That is, SYMBOL must *both*
satisfy `icalendar-value-type-symbol-p' and be associated with a print
name in `icalendar-value-types'."
  (and (ical:value-type-symbol-p symbol)
       (rassq symbol ical:value-types)))

(defun ical:value-node-p (node)
  "Return non-nil if NODE is a syntax node whose type is a value type."
  (and (ical:ast-node-p node)
       (ical:value-type-symbol-p (ical:ast-node-type node))))

;;;; 3.3.1 Binary
;; from https://www.rfc-editor.org/rfc/rfc4648#section-4:
(rx-define ical:base64char
  (any (?A . ?Z) (?a . ?z) (?0 . ?9) ?+ ?/))

(ical:define-type ical:binary "BINARY"
   "Type for Binary values.

The parsed and printed representations are the same: a string of characters
representing base64-encoded data."
   '(and string (satisfies ical:match-binary-value))
   (seq (zero-or-more (= 4 ical:base64char))
        (zero-or-one (or (seq (= 2 ical:base64char) "==")
                         (seq (= 3 ical:base64char) "="))))
   :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.1")

;;;; 3.3.2 Boolean
(defun ical:read-boolean (s)
  "Read an `icalendar-boolean' value from a string S.
S should be a match against rx `icalendar-boolean'."
  (let ((upcased (upcase s)))
    (cond ((equal upcased "TRUE") t)
          ((equal upcased "FALSE") nil)
          (t (ical:signal-parse-error
              (format "Expected 'TRUE' or 'FALSE'; got %s" s))))))

(defun ical:print-boolean (b)
  "Serialize an `icalendar-boolean' value B to a string."
    (if b "TRUE" "FALSE"))

(ical:define-type ical:boolean "BOOLEAN"
   "Type for Boolean values.

When printed, either the string 'TRUE' or 'FALSE'.
When read, either t or nil."
   'boolean
   (or "TRUE" "FALSE")
   :reader ical:read-boolean
   :printer ical:print-boolean
   :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.2")

;;;; 3.3.3 Calendar User Address
;; Defined with URI, below

;; Dates and Times:

;;;; 3.3.4 Date
(cl-deftype ical:numeric-year () '(integer 0 9999))
(cl-deftype ical:numeric-month () '(integer 1 12))
(cl-deftype ical:numeric-monthday () '(integer 1 31))

(rx-define ical:year
  (= 4 digit))

(rx-define ical:month
  (= 2 digit))

(rx-define ical:mday
  (= 2 digit))

(defun ical:read-date (s)
  "Read an `icalendar-date' from a string S.
S should be a match against rx `icalendar-date'."
  (let ((year (string-to-number (substring s 0 4)))
        (month (string-to-number (substring s 4 6)))
        (day (string-to-number (substring s 6 8))))
    (list month day year)))

(defun ical:print-date (d)
  "Serialize an `icalendar-date' to a string."
  (format "%04d%02d%02d"
          (calendar-extract-year d)
          (calendar-extract-month d)
          (calendar-extract-day d)))

(ical:define-type ical:date "DATE"
   "Type for Date values.

When printed, a date is a string of digits in YYYYMMDD format.

When read, a date is a list (MONTH DAY YEAR), with the three
values being integers in the appropriate ranges; see calendar.el
for functions that work with this representation."
   '(and (satisfies calendar-date-is-valid-p))
   (seq ical:year ical:month ical:mday)
   :reader ical:read-date
   :printer ical:print-date
   :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.4")

;;;; 3.3.12 Time
;; (Defined here so that ical:time RX can be used in ical:date-time)
(cl-deftype ical:numeric-hour () '(integer 0 23))
(cl-deftype ical:numeric-minute () '(integer 0 59))
(cl-deftype ical:numeric-second () '(integer 0 60)) ; 60 represents a leap second

(defun ical:read-time (s)
  "Read an `icalendar-time' from a string S.
S should be a match against rx `icalendar-time'."
  (require 'icalendar-utils) ; for ical:make-date-time; avoids circular require
  (declare-function ical:make-date-time "icalendar-utils")
  (let ((hour (string-to-number (substring s 0 2)))
        (minute (string-to-number (substring s 2 4)))
        (second (string-to-number (substring s 4 6)))
        (utcoffset (if (and (length= s 7)
                            (equal "Z" (substring s 6 7)))
                       0
                     ;; unknown/'floating' time zone:
                     nil)))
    (ical:make-date-time :second second
                         :minute minute
                         :hour hour
                         :zone utcoffset)))

(defun ical:print-time (time)
  "Serialize an `icalendar-time' to a string."
  (format "%02d%02d%02d%s"
          (decoded-time-hour time)
          (decoded-time-minute time)
          (decoded-time-second time)
          (if (eql 0 (decoded-time-zone time))
              "Z" "")))

(defun ical:-decoded-time-p (val)
  "Return non-nil if VAL is a valid decoded *time*.
This predicate does not check date-related values in VAL;
for that, see `icalendar--decoded-date-time-p'."
  (and (listp val)
       (length= val 9)
       (cl-typep (decoded-time-second val) 'ical:numeric-second)
       (cl-typep (decoded-time-minute val) 'ical:numeric-minute)
       (cl-typep (decoded-time-hour val) 'ical:numeric-hour)
       (cl-typep (decoded-time-dst val) '(member t nil -1))
       (cl-typep (decoded-time-zone val) '(or integer null))))

(ical:define-type ical:time "TIME"
  "Type for Time values.

When printed, a time is a string of six digits HHMMSS, followed
by the letter 'Z' if it is in UTC.

When read, a time is a decoded time, i.e. a list in the format
(SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF).  See
`decode-time' for the specifics of the individual values.  When
read, the DAY, MONTH, YEAR, and DOW fields are nil, and these
fields and DST are ignored when printed."
  '(satisfies ical:-decoded-time-p)
  (seq (= 6 digit) (zero-or-one ?Z))
  :reader ical:read-time
  :printer ical:print-time
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.12")

;;;; 3.3.5 Date-Time
(defun ical:-decoded-date-time-p (val)
  (and (listp val)
       (length= val 9)
       (cl-typep (decoded-time-second val) 'ical:numeric-second)
       (cl-typep (decoded-time-minute val) 'ical:numeric-minute)
       (cl-typep (decoded-time-hour val) 'ical:numeric-hour)
       (cl-typep (decoded-time-day val) 'ical:numeric-monthday)
       (cl-typep (decoded-time-month val) 'ical:numeric-month)
       (cl-typep (decoded-time-year val) 'ical:numeric-year)
       (calendar-date-is-valid-p (list (decoded-time-month val)
                                       (decoded-time-day val)
                                       (decoded-time-year val)))
       ;; FIXME: the weekday slot value should be automatically
       ;; calculated from month, day, and year, like:
       ;;   (calendar-day-of-week (list month day year))
       ;; Although `ical:read-date-time' does this correctly,
       ;; `make-decoded-time' does not.  Thus we can't use
       ;; `make-decoded-time' to construct valid `ical:date-time'
       ;; values unless this check is turned off,
       ;; which means it's annoying to write tests and anything
       ;; that uses cl-typecase to dispatch on values created by
       ;; `make-decoded-time':
       ;; (cl-typep (decoded-time-weekday val) '(integer 0 6))
       (cl-typep (decoded-time-dst val) '(member t nil -1))
       (cl-typep (decoded-time-zone val) '(or integer null))))

(defun ical:read-date-time (s)
  "Read an `icalendar-date-time' from a string S.
S should be a match against rx `icalendar-date-time'."
  (require 'icalendar-utils) ; for ical:make-date-time; avoids circular requires
  (let ((year (string-to-number (substring s 0 4)))
        (month (string-to-number (substring s 4 6)))
        (day (string-to-number (substring s 6 8)))
        ;; "T" is index 8
        (hour (string-to-number (substring s 9 11)))
        (minute (string-to-number (substring s 11 13)))
        (second (string-to-number (substring s 13 15)))
        (utcoffset (if (and (length= s 16)
                            (equal "Z" (substring s 15 16)))
                       0
                     ;; unknown/'floating' time zone:
                     nil)))
    (ical:make-date-time :second second
                         :minute minute
                         :hour hour
                         :day day
                         :month month
                         :year year
                         :zone utcoffset)))

(defun ical:print-date-time (datetime)
  "Serialize an `icalendar-date-time' to a string."
  (format "%04d%02d%02dT%02d%02d%02d%s"
          (decoded-time-year datetime)
          (decoded-time-month datetime)
          (decoded-time-day datetime)
          (decoded-time-hour datetime)
          (decoded-time-minute datetime)
          (decoded-time-second datetime)
          (if (ical:date-time-is-utc-p datetime)
              "Z" "")))

(defun ical:date-time-is-utc-p (datetime)
  "Return non-nil if DATETIME is in UTC time."
  (let ((offset (decoded-time-zone datetime)))
    (and offset (= 0 offset))))

(ical:define-type ical:date-time "DATE-TIME"
   "Type for Date-Time values.

When printed, a date-time is a string of digits like:
  YYYYMMDDTHHMMSS
where the 'T' is literal, and separates the date string from the
time string.

When read, a date-time is a decoded time, i.e. a list in the format
(SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF).  See
`decode-time' for the specifics of the individual values."
   '(satisfies ical:-decoded-date-time-p)
  (seq ical:date ?T ical:time)
  :reader ical:read-date-time
  :printer ical:print-date-time
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.5")

;;;; 3.3.6 Duration
(rx-define ical:dur-second
  (seq (group-n 19 (one-or-more digit)) ?S))

(rx-define ical:dur-minute
  (seq (group-n 18 (one-or-more digit)) ?M (zero-or-one ical:dur-second)))

(rx-define ical:dur-hour
  (seq (group-n 17 (one-or-more digit)) ?H (zero-or-one ical:dur-minute)))

(rx-define ical:dur-day
  (seq (group-n 16 (one-or-more digit)) ?D))

(rx-define ical:dur-week
  (seq (group-n 15 (one-or-more digit)) ?W))

(rx-define ical:dur-time
  (seq ?T (or ical:dur-hour ical:dur-minute ical:dur-second)))

(rx-define ical:dur-date
  (seq ical:dur-day (zero-or-one ical:dur-time)))

(defun ical:read-dur-value (s)
  "Read an `icalendar-dur-value' from a string S.
S should be a match against rx `icalendar-dur-value'."
  ;; TODO: this smells like a design flaw.  Silence the byte compiler for now.
  (ignore s)
  (let ((sign (if (equal (match-string 20) "-") -1 1)))
    (if (match-string 15)
        ;; dur-value specified in weeks, so just return an integer:
        (* sign (string-to-number (match-string 15)))
      ;; otherwise, make a time delta from the other units:
      (let* ((days (match-string 16))
             (ndays (* sign (if days (string-to-number days) 0)))
             (hours (match-string 17))
             (nhours (* sign (if hours (string-to-number hours) 0)))
             (minutes (match-string 18))
             (nminutes (* sign (if minutes (string-to-number minutes) 0)))
             (seconds (match-string 19))
             (nseconds (* sign (if seconds (string-to-number seconds) 0))))
        (make-decoded-time :second nseconds :minute nminutes :hour nhours
                           :day ndays)))))

(defun ical:print-dur-value (dur)
  "Serialize an `icalendar-dur-value' to a string."
  (if (integerp dur)
      ;; dur-value specified in weeks can only contain weeks:
      (format "%sP%dW" (if (< dur 0) "-" "") (abs dur))
    ;; otherwise, show all the time units present:
    (let* ((days+- (or (decoded-time-day dur) 0))
           (hours+- (or (decoded-time-hour dur) 0))
           (minutes+- (or (decoded-time-minute dur) 0))
           (seconds+- (or (decoded-time-second dur) 0))
           ;; deal with the possibility of mixed positive and negative values
           ;; in a time delta list:
           (sum (+ seconds+-
                   (* 60 minutes+-)
                   (* 60 60 hours+-)
                   (* 60 60 24 days+-)))
           (abssum (abs sum))
           (days (/ abssum (* 60 60 24)))
           (sumnodays (mod abssum (* 60 60 24)))
           (hours (/ sumnodays (* 60 60)))
           (sumnohours (mod sumnodays (* 60 60)))
           (minutes (/ sumnohours 60))
           (seconds (mod sumnohours 60))
           (sign (when (< sum 0) "-"))
           (time-sep (unless (and (zerop hours) (zerop minutes) (zerop seconds))
                       "T")))
      (concat sign
              "P"
              (unless (zerop days) (format "%dD" days))
              time-sep
              (unless (zerop hours) (format "%dH" hours))
              (unless (zerop minutes) (format "%dM" minutes))
              (unless (zerop seconds) (format "%dS" seconds))))))

(defun ical:-time-delta-p (val)
  (and (listp val)
       (length= val 9)
       (let ((seconds (decoded-time-second val))
             (minutes (decoded-time-minute val))
             (hours (decoded-time-hour val))
             (days (decoded-time-day val))) ; other values in list are ignored
         (or (and (integerp seconds) (not (zerop seconds)))
             (and (integerp minutes) (not (zerop minutes)))
             (and (integerp hours) (not (zerop hours)))
             (and (integerp days) (not (zerop days)))))))

(ical:define-type ical:dur-value "DURATION"
  "Type for Duration values.

When printed, a duration is a string containing:
  - possibly a +/- sign
  - the letter 'P'
  - one or more sequences of digits followed by a letter representing a unit
    of time: 'W' for weeks, 'D' for days, etc.  Units smaller than a day are
    separated from days by the letter 'T'.  If a duration is specified in weeks,
    other units of time are not allowed.

For example, a duration of 15 days, 5 hours, and 20 seconds would be printed:
   P15DT5H0M20S
and a duration of 7 weeks would be printed:
   P7W

When read, a duration is either an integer, in which case it
represents a number of weeks, or a decoded time, in which case it
must represent a time delta in the sense of `decoded-time-add'.
Note that, in the time delta representation, units of time longer
than a day are not supported and will be ignored if present.

This type is named `icalendar-dur-value' rather than
`icalendar-duration' for consistency with the text of RFC5545 and
so that its name does not collide with the symbol for the
`DURATION' property."
  '(or integer (satisfies ical:-time-delta-p))
  ;; Group 15: weeks
  ;; Group 16: days
  ;; Group 17: hours
  ;; Group 18: minutes
  ;; Group 19: seconds
  ;; Group 20: sign
  (seq
   (group-n 20 (zero-or-one (or ?+ ?-)))
   ?P
   (or ical:dur-date ical:dur-time ical:dur-week))
  :reader ical:read-dur-value
  :printer ical:print-dur-value
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.6")


;;;; 3.3.7 Float
(ical:define-type ical:float "FLOAT"
   "Type for Float values.

When printed, possibly a sign + or -, followed by a sequence of digits,
and possibly a decimal.  When read, an Elisp float value."
   '(float * *)
   (seq
    (zero-or-one (or ?+ ?-))
    (one-or-more digit)
    (zero-or-one (seq ?. (one-or-more digit))))
   :reader string-to-number
   :printer number-to-string
   :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.7")

;;;; 3.3.8 Integer
(ical:define-type ical:integer "INTEGER"
   "Type for Integer values.

When printed, possibly a sign + or -, followed by a sequence of digits.
When read, an Elisp integer value between -2147483648 and 2147483647."
   '(integer -2147483648 2147483647)
   (seq
    (zero-or-one (or ?+ ?-))
    (one-or-more digit))
   :reader string-to-number
   :printer number-to-string
   :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.8")

;;;; 3.3.9 Period
(defsubst ical:period-start (period)
  "Return the `icalendar-date-time' which marks the start of PERIOD."
  (car period))

(defsubst ical:period--defined-end (period)
  "Return the `icalendar-date-time' which marks the end of PERIOD, or nil."
  (cadr period))

(defsubst ical:period-dur-value (period)
  "Return the `icalendar-dur-value' which gives the length of PERIOD, or nil."
  (caddr period))

(defun ical:period-end (period &optional vtimezone)
  "Return the `icalendar-date-time' which marks the end of PERIOD.
If the end is not explicitly specified, it will be computed from the
period's start and duration.  VTIMEZONE, if given, should be the
`icalendar-vtimezone' in which to compute the end time."
  (require 'icalendar-utils) ; for date/time-add-duration; avoids circular import
  (declare-function ical:date/time-add-duration "icalendar-utils")
  (or (ical:period--defined-end period)
      ;; compute end from duration and cache it:
      (setf (cadr period)
            (ical:date/time-add-duration
             (ical:period-start period)
             (ical:period-dur-value period)
             vtimezone))))

(defun ical:period-p (val)
  (and (listp val)
       (length= val 3)
       (cl-typep (ical:period-start val) 'ical:date-time)
       (cl-typep (ical:period-end val) '(or null ical:date-time))
       (cl-typep (ical:period-dur-value val) '(or null ical:dur-value))))

(cl-defun ical:make-period (start &key end duration)
  "Make an `icalendar-period' value.

START and END (if given) should be `icalendar-date-time' values.
DURATION, if given, should be an `icalendar-dur-value'.  It is an error
to pass both END and DURATION, or neither."
  (when (and end duration)
    (signal 'wrong-type-argument (list end duration)))
  (unless (or end duration)
    (signal 'wrong-type-argument (list end duration)))
  (list start end duration))

(defun ical:read-period (s)
  "Read an `icalendar-period' from a string S.
S should have been matched against rx `icalendar-period'."
  ;; TODO: this smells like a design flaw.  Silence the byte compiler for now.
  (ignore s)
  (let ((start (ical:read-date-time (match-string 11)))
        (end (when (match-string 12) (ical:read-date-time (match-string 12))))
        (dur (when (match-string 13) (ical:read-dur-value (match-string 13)))))
    (ical:make-period start :end end :duration dur)))

(defun ical:print-period (per)
  "Serialize an `icalendar-period' to a string."
  (let ((start (ical:period-start per))
        (end (ical:period-end per))
        (dur (ical:period-dur-value per)))
    (concat (ical:print-date-time start)
            "/"
            (if dur
                (ical:print-dur-value dur)
              (ical:print-date-time end)))))

(ical:define-type ical:period "PERIOD"
   "Type for Period values.

A period of time is specified as a starting date-time together
with either an explicit date-time as its end, or a duration which
gives its length and implicitly marks its end.

When printed, the starting date-time is separated from the end or
duration by a / character.

When read, a period is represented as a list (START END DUR), where
START is an `icalendar-date-time', END is either an
`icalendar-date-time' or nil, and DUR is either an `icalendar-dur-value'
or nil.  See the functions `icalendar-make-period',
`icalendar-period-start', `icalendar-period-end', and
`icalendar-period-dur-value' to work with period values."
  '(satisfies ical:period-p)
  (seq (group-n 11 ical:date-time)
       "/"
       (or (group-n 12 ical:date-time)
           (group-n 13 ical:dur-value)))
  :reader ical:read-period
  :printer ical:print-period
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.9")

;;;; 3.3.10 Recurrence rules:
(rx-define ical:freq
   (or "SECONDLY" "MINUTELY" "HOURLY" "DAILY" "WEEKLY" "MONTHLY" "YEARLY"))

(rx-define ical:weekday
   (or "SU" "MO" "TU" "WE" "TH" "FR" "SA"))

(rx-define ical:ordwk
  (** 1 2 digit)) ; 1 to 53

(rx-define ical:weekdaynum
  ;; Group 19: Week num, if present
  ;; Group 20: week day abbreviation
   (seq (zero-or-one
         (group-n 19 (seq (zero-or-one (or ?+ ?-))
                          ical:ordwk)))
        (group-n 20 ical:weekday)))

(rx-define ical:weeknum
  (seq (zero-or-one (or ?+ ?-))
       ical:ordwk))

(rx-define ical:monthdaynum
  (seq (zero-or-one (or ?+ ?-))
       (** 1 2 digit))) ; 1 to 31

(rx-define ical:monthnum
  (seq (zero-or-one (or ?+ ?-))
       (** 1 2 digit))) ; 1 to 12

(rx-define ical:yeardaynum
  (seq (zero-or-one (or ?+ ?-))
       (** 1 3 digit))) ; 1 to 366

(defconst ical:weekday-numbers
  '(("SU" . 0)
    ("MO" . 1)
    ("TU" . 2)
    ("WE" . 3)
    ("TH" . 4)
    ("FR" . 5)
    ("SA" . 6))
  "Alist mapping two-letter weekday abbreviations to numbers 0 to 6.
Weekday abbreviations in recurrence rule parts are translated to
and from numbers for compatibility with calendar-* and
decoded-time-* functions.")

(defun ical:read-weekdaynum (s)
  "Read a weekday abbreviation to a number.
If the abbreviation is preceded by an offset, read a dotted
pair (WEEKDAY . OFFSET).  Thus \"SU\" becomes 0, \"-1SU\"
becomes (0 . -1), etc.  S should have been matched against
`icalendar-weekdaynum'."
  ;; TODO: this smells like a design flaw.  Silence the byte compiler for now.
  (ignore s)
  (let ((dayno (cdr (assoc (match-string 20) ical:weekday-numbers)))
        (weekno (match-string 19)))
    (if weekno
        (cons dayno (string-to-number weekno))
      dayno)))

(defun ical:print-weekdaynum (val)
  "Serialize a number or dotted pair VAL to a string.
The result is in the format required for a BYDAY recurrence rule clause.
See `icalendar-read-weekdaynum' for the format of VAL."
  (if (consp val)
      (let* ((dayno (car val))
             (day (car (rassq dayno ical:weekday-numbers)))
             (offset (cdr val)))
        (concat (number-to-string offset) day))
    ;; number alone just stands for a day:
    (car (rassq val ical:weekday-numbers))))

(defun ical:read-recur-rule-part (s)
  "Read an `icalendar-recur-rule-part' from string S.
S should have been matched against `icalendar-recur-rule-part'.
The return value is a list (KEYWORD VALUE), where VALUE may
itself be a list, depending on the values allowed by KEYWORD."
  ;; TODO: this smells like a design flaw.  Silence the byte compiler for now.
  (ignore s)
  (let ((keyword (intern (upcase (match-string 11))))
        (values (match-string 12)))
    (list keyword
      (cl-case keyword
        (FREQ (intern (upcase values)))
        (UNTIL (if (length> values 8)
                   (ical:read-date-time values)
                 (ical:read-date values)))
        ((COUNT INTERVAL)
         (string-to-number values))
        ((BYSECOND BYMINUTE BYHOUR BYMONTHDAY BYYEARDAY BYWEEKNO BYMONTH BYSETPOS)
         (ical:read-list-with #'string-to-number values nil ","))
        (BYDAY
         (ical:read-list-with #'ical:read-weekdaynum values
                              (rx ical:weekdaynum) ","))
        (WKST (cdr (assoc values ical:weekday-numbers)))))))

(defun ical:print-recur-rule-part (part)
  "Serialize recur rule part PART to a string."
  (let ((keyword (car part))
        (values (cadr part))
        values-str)
    (cl-case keyword
      (FREQ (setq values-str (symbol-name values)))
      (UNTIL (setq values-str (cl-typecase values
                                (ical:date-time (ical:print-date-time values))
                                (ical:date (ical:print-date values)))))
      ((COUNT INTERVAL)
       (setq values-str (number-to-string values)))
      ((BYSECOND BYMINUTE BYHOUR BYMONTHDAY BYYEARDAY BYWEEKNO BYMONTH BYSETPOS)
       (setq values-str (string-join (mapcar #'number-to-string values)
                                     ",")))
      (BYDAY
       (setq values-str (string-join (mapcar #'ical:print-weekdaynum values)
                                     ",")))
      (WKST (setq values-str (car (rassq values ical:weekday-numbers)))))

    (concat (symbol-name keyword) "=" values-str)))

(rx-define ical:recur-rule-part
  ;; Group 11: keyword
  ;; Group 12: value(s)
  (or (seq (group-n 11 "FREQ") "=" (group-n 12 ical:freq))
      (seq (group-n 11 "UNTIL") "=" (group-n 12 (or ical:date-time ical:date)))
      (seq (group-n 11 "COUNT") "=" (group-n 12 (one-or-more digit)))
      (seq (group-n 11 "INTERVAL") "=" (group-n 12 (one-or-more digit)))
      (seq (group-n 11 "BYSECOND") "=" (group-n 12 ; 0 to 60
                                         (ical:comma-list (** 1 2 digit))))
      (seq (group-n 11 "BYMINUTE") "=" (group-n 12 ; 0 to 59
                                         (ical:comma-list (** 1 2 digit))))
      (seq (group-n 11 "BYHOUR") "=" (group-n 12 ; 0 to 23
                                       (ical:comma-list (** 1 2 digit)))) ; 0 to 23
      (seq (group-n 11 "BYDAY") "=" (group-n 12 ; weeknum? daynum, e.g. SU or 34SU
                                      (ical:comma-list ical:weekdaynum)))
      (seq (group-n 11 "BYMONTHDAY") "=" (group-n 12
                                           (ical:comma-list ical:monthdaynum)))
      (seq (group-n 11 "BYYEARDAY") "=" (group-n 12
                                          (ical:comma-list ical:yeardaynum)))
      (seq (group-n 11 "BYWEEKNO") "=" (group-n 12 (ical:comma-list ical:weeknum)))
      (seq (group-n 11 "BYMONTH") "=" (group-n 12 (ical:comma-list ical:monthnum)))
      (seq (group-n 11 "BYSETPOS") "=" (group-n 12
                                         (ical:comma-list ical:yeardaynum)))
      (seq (group-n 11 "WKST") "=" (group-n 12 ical:weekday))))

(defun ical:read-recur (s)
  "Read a recurrence rule value from string S.
S should be a match against rx `icalendar-recur'."
  ;; TODO: let's switch to keywords and a plist, so we can more easily
  ;; write these clauses also in diary sexp entries without so many parens
  (ical:read-list-with #'ical:read-recur-rule-part s (rx ical:recur-rule-part) ";"))

(defun ical:print-recur (val)
  "Serialize a recurrence rule value VAL to a string."
  ;; RFC5545 sec. 3.3.10: "to ensure backward compatibility with
  ;; applications that pre-date this revision of iCalendar the
  ;; FREQ rule part MUST be the first rule part specified in a
  ;; RECUR value."
  (string-join
   (cons
    (ical:print-recur-rule-part (assq 'FREQ val))
    (mapcar #'ical:print-recur-rule-part
            (seq-filter (lambda (part) (not (eq 'FREQ (car part))))
                        val)))
   ";"))

(defconst ical:-recur-value-types
  ;; `list-of' is not a cl-type specifier, just a symbol here; it is
  ;; handled specially when checking types in `ical:recur-value-p':
  '(FREQ (member YEARLY MONTHLY WEEKLY DAILY HOURLY MINUTELY SECONDLY)
    UNTIL (or ical:date-time ical:date)
    COUNT (integer 1 *)
    INTERVAL (integer 1 *)
    BYSECOND (list-of (integer 0 60))
    BYMINUTE (list-of (integer 0 59))
    BYHOUR (list-of (integer 0 23))
    BYDAY (list-of (or (integer 0 6) (satisfies ical:dayno-offset-p)))
    BYMONTHDAY (list-of (or (integer -31 -1) (integer 1 31)))
    BYYEARDAY (list-of (or (integer -366 -1) (integer 1 366)))
    BYWEEKNO (list-of (or (integer -53 -1) (integer 1 53)))
    BYMONTH (list-of (integer 1 12)) ; unlike the others, months cannot be negative
    BYSETPOS (list-of (or (integer -366 -1) (integer 1 366)))
    WKST (integer 0 6))
  "Plist mapping `icalendar-recur' keywords to type specifiers.")

(defun ical:dayno-offset-p (val)
  "Return non-nil if VAL is a pair (DAYNO . OFFSET).
DAYNO must be in [0..6] and OFFSET in [-53..53], excluding 0."
  (and (consp val)
       (cl-typep (car val) '(integer 0 6))
       (cl-typep (cdr val) '(or (integer -53 -1) (integer 1 53)))))

(defun ical:recur-value-p (vals)
  "Return non-nil if VALS is an iCalendar recurrence rule value."
  (and (listp vals)
       ;; FREQ is always required:
       (assq 'FREQ vals)
       ;; COUNT and UNTIL are mutually exclusive if present:
       (not (and (assq 'COUNT vals) (assq 'UNTIL vals)))
       ;; If BYSETPOS is present, another BYXXX clause must be too:
       (or (not (assq 'BYSETPOS vals))
           (assq 'BYMONTH vals)
           (assq 'BYWEEKNO vals)
           (assq 'BYYEARDAY vals)
           (assq 'BYMONTHDAY vals)
           (assq 'BYDAY vals)
           (assq 'BYHOUR vals)
           (assq 'BYMINUTE vals)
           (assq 'BYSECOND vals))
       (let ((freq (ical:recur-freq vals))
             (byday (ical:recur-by* 'BYDAY vals))
             (byweekno (ical:recur-by* 'BYWEEKNO vals))
             (bymonthday (ical:recur-by* 'BYMONTHDAY vals))
             (byyearday (ical:recur-by* 'BYYEARDAY vals)))
         (and
          ;; "The BYDAY rule part MUST NOT be specified with a numeric
          ;; value when the FREQ rule part is not set to MONTHLY or
          ;; YEARLY."
          (or (not (consp (car byday)))
              (memq freq '(MONTHLY YEARLY)))
          ;; "The BYDAY rule part MUST NOT be specified with a numeric
          ;; value with the FREQ rule part set to YEARLY when the
          ;; BYWEEKNO rule part is specified." This also covers:
          ;; "[The BYWEEKNO] rule part MUST NOT be used when the FREQ
          ;; rule part is set to anything other than YEARLY."
          (or (not byweekno)
              (and (eq freq 'YEARLY)
                   (not (consp (car byday)))))
          ;; "The BYMONTHDAY rule part MUST NOT be specified when the
          ;; FREQ rule part is set to WEEKLY."
          (not (and bymonthday (eq freq 'WEEKLY)))
          ;; "The BYYEARDAY rule part MUST NOT be specified when the
          ;; FREQ rule part is set to DAILY, WEEKLY, or MONTHLY."
          (not (and byyearday (memq freq '(DAILY WEEKLY MONTHLY))))))
       ;; check types of all rule parts:
       (seq-every-p
        (lambda (kv)
          (when (consp kv)
            (let* ((keyword (car kv))
                   (val (cadr kv))
                   (type (plist-get ical:-recur-value-types keyword)))
              (and keyword val type
                   (if (and (consp type)
                            (eq (car type) 'list-of))
                       (ical:list-of-p val (cadr type))
                     (cl-typep val type))))))
         vals)))

(ical:define-type ical:recur "RECUR"
  "Type for Recurrence Rule values.

When printed, a recurrence rule value looks like
  KEY1=VAL1;KEY2=VAL2;...
where the VALs may themselves be lists or have other syntactic
structure; see RFC5545 sec. 3.3.10 for all the details.

The KEYs and their associated value types when read are as follows.
The first is required:
  '(FREQ (member YEARLY MONTHLY WEEKLY DAILY HOURLY MINUTELY SECONDLY)
These two are mutually exclusive; at most one may appear:
    UNTIL (or icalendar-date-time icalendar-date)
    COUNT (integer 1 *)
All others are optional:
    INTERVAL (integer 1 *)
    BYSECOND (list-of (integer 0 60))
    BYMINUTE (list-of (integer 0 59))
    BYHOUR (list-of (integer 0 23))
    BYDAY (list-of (or (integer 0 6) ; day of week
                       (pair (integer 0 6)  ; (day of week . offset)
                             (integer -53 53))) ; except 0
    BYMONTHDAY (list-of (integer -31 31))  ; except 0
    BYYEARDAY (list-of (integer -366 366)) ; except 0
    BYWEEKNO (list-of (integer -53 53))    ; except 0
    BYMONTH (list-of (integer 1 12))       ; months cannot be negative
    BYSETPOS (list-of (integer -366 366))  ; except 0
    WKST (integer 0 6))

When read, these KEYs and their associated VALs are gathered into
an alist.

In general, the VALs consist of integers or lists of integers.
Abbreviations for weekday names are translated into integers
0 (=Sunday) through 6 (=Saturday), for compatibility with
calendar.el and decoded-time-* functions.

Some examples:

1) Printed: FREQ=DAILY;COUNT=10;INTERVAL=2
   Meaning: 10 occurrences that occur every other day
   Read: ((FREQ DAILY)
          (COUNT 10)
          (INTERVAL 2))

2) Printed: FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA
   Meaning: Every day in January of every year until 2000/01/31 at 14:00 UTC
   Read: ((FREQ YEARLY)
          (UNTIL (0 0 14 31 1 2000 1 -1 0))
          (BYMONTH (1))
          (BYDAY (0 1 2 3 4 5 6)))

3) Printed: FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2
   Meaning: Every month on the second-to-last weekday of the month
   Read: ((FREQ MONTHLY)
          (BYDAY (1 2 3 4 5))
          (BYSETPOS (-2)))

Notice that singleton values are still wrapped in a list when the
KEY accepts a list of values, but not when the KEY always has a
single (e.g. integer) value."
  '(satisfies ical:recur-value-p)
  (ical:semicolon-list ical:recur-rule-part)
  :reader ical:read-recur
  :printer ical:print-recur
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.10")

(defun ical:recur-freq (recur-value)
  "Return the frequency in RECUR-VALUE."
  (car (alist-get 'FREQ recur-value)))

(defun ical:recur-interval-size (recur-value)
  "Return the interval size in RECUR-VALUE, or the default of 1."
  (or (car (alist-get 'INTERVAL recur-value)) 1))

(defun ical:recur-until (recur-value)
  "Return the UNTIL date(-time) in RECUR-VALUE."
  (car (alist-get 'UNTIL recur-value)))

(defun ical:recur-count (recur-value)
  "Return the COUNT in RECUR-VALUE."
  (car (alist-get 'COUNT recur-value)))

(defun ical:recur-weekstart (recur-value)
  "Return the weekday which starts the work week in RECUR-VALUE.
If no starting weekday is specified in RECUR-VALUE, returns the default,
1 (= Monday)."
  (or (car (alist-get 'WKST recur-value)) 1))

(defun ical:recur-by* (byunit recur-value)
  "Return the values in the BYUNIT clause in RECUR-VALUE.
BYUNIT should be a symbol: \\='BYMONTH, \\='BYDAY, etc.
See `icalendar-recur' for all the possible BYUNIT values."
  (car (alist-get byunit recur-value)))

;;;; 3.3.11 Text
(rx-define ical:escaped-char
   (seq ?\\ (or ?\\ ?\; ?, ?N ?n)))

(rx-define ical:text-safe-char
  ;; "Any character except CONTROLs not needed by the current character
  ;; set, DQUOTE, ";", ":", "\", "," "
  (any #x09 #x20 #x21 ; htab, space, and "!"
       (#x23 . #x2B) (#x2D . #x39) ; "#".."9" skipping #x2C=","
       (#x3C . #x5B) (#x5D . #x7E) ; "<".."~" skipping #x5C="\"
       nonascii))

(defun ical:text-region-p (val)
  "Return t if VAL represents a region of text."
  (and (listp val)
       (markerp (car val))
       (not (null (marker-buffer (car val))))
       (markerp (cdr val))))

(defun ical:make-text-region (&optional buffer begin end)
  "Return an object that represents a region of text.
The region is taken from BUFFER between BEGIN and END.  BUFFER defaults
to the current buffer, and BEGIN and END default to point and mark in
BUFFER."
  (let ((buf (or buffer (current-buffer)))
        (b (make-marker))
        (e (make-marker)))
    (with-current-buffer buf
      (set-marker b (or begin (region-beginning)) buf)
      (set-marker e (or end (region-end)))
      (cons b e))))

(defsubst ical:text-region-begin (r)
  "Return the marker at the beginning of the text region R."
  (car r))

(defsubst ical:text-region-end (r)
  "Return the marker at the end of the text region R."
  (cdr r))

(defun ical:unescape-text-in-region (begin end)
 "Unescape the text between BEGIN and END.
Unescaping replaces literal '\\n' and '\\N' with newline, and removes
backslashes that escape commas, semicolons, and backslashes."
 (with-restriction begin end
   (save-excursion
    (replace-string-in-region "\\N" "\n" (point-min) (point-max))
    (replace-string-in-region "\\n" "\n" (point-min) (point-max))
    (replace-string-in-region "\\," "," (point-min) (point-max))
    (replace-string-in-region "\\;" ";" (point-min) (point-max)))
    (replace-string-in-region (concat "\\" "\\") "\\" (point-min) (point-max))))

(defun ical:unescape-text-string (s)
 "Unescape the text in string S.
Unescaping replaces literal '\\n' and '\\N' with newline, and removes
backslashes that escape commas, semicolons, and backslashes."
  (with-temp-buffer
    (insert s)
    (ical:unescape-text-in-region (point-min) (point-max))
    (buffer-string)))

(defun ical:escape-text-in-region (begin end)
  "Escape the text between BEGIN and END in the current buffer.
Escaping replaces newlines with literal '\\n', and escapes commas,
semicolons and backslashes with a backslash."
 (with-restriction begin end
  (save-excursion
    ;; replace backslashes first, so the ones introduced when
    ;; escaping other characters don't end up double-escaped:
    (replace-string-in-region "\\" (concat "\\" "\\") (point-min) (point-max))
    (replace-string-in-region "\n" "\\n" (point-min) (point-max))
    (replace-string-in-region "," "\\," (point-min) (point-max))
    (replace-string-in-region ";" "\\;" (point-min) (point-max)))))

(defun ical:escape-text-string (s)
  "Escape the text in string S.
Escaping replaces newlines with literal '\\n', and escapes commas,
semicolons and backslashes with a backslash."
  (with-temp-buffer
    (insert s)
    (ical:escape-text-in-region (point-min) (point-max))
    (buffer-string)))

(defun ical:read-text (s)
  "Read an `icalendar-text' value from a string S.
S should be a match against rx `icalendar-text'."
  (ical:unescape-text-string s))

(defun ical:print-text (val)
  "Serialize an iCalendar text value.
VAL may be a string or text region (see `icalendar-make-text-region').
The text will be escaped before printing.  If VAL is a region, the text
it contains will not be modified; it is copied before escaping."
  (if (stringp val)
      (ical:escape-text-string val)
    ;; val is a region, so copy and escape its contents:
    (let* ((beg (ical:text-region-begin val))
           (buf (marker-buffer beg))
           (end (ical:text-region-end val)))
      (with-temp-buffer
        (insert-buffer-substring buf (marker-position beg) (marker-position end))
        (ical:escape-text-in-region (point-min) (point-max))
        (buffer-string)))))

(defun ical:text-to-string (node)
  "Return the value of an `icalendar-text' NODE as a string.
The returned string is *not* escaped.  For that, see `icalendar-print-text'."
  (ical:with-node-value node nil
    (if (stringp value) value
      ;; Otherwise the value is a text region:
      (let* ((beg (ical:text-region-begin value))
             (buf (marker-buffer beg))
             (end (ical:text-region-end value)))
        (with-current-buffer buf
          (buffer-substring (marker-position beg) (marker-position end)))))))

;; TODO: would it be useful to add a third representation, namely a
;; function or thunk?  So that e.g. Org can pre-process its own syntax
;; and return a plain text string to use in the description?
(ical:define-type ical:text "TEXT"
   "Type for Text values.

Text values can be represented in Elisp in two ways: as strings,
or as buffer regions.  For values which aren't expected to change,
such as property values in a text/calendar email attachment, use
strings.  For values which are user-editable and might change
between parsing and serializing to iCalendar format, use a
region.  In that case, a text value contains two markers BEGIN and
END which mark the bounds of the region.  See
`icalendar-make-text-region' to create such values, and
`icalendar-text-region-begin' and `icalendar-text-region-end' to
access the markers.

Certain characters in text values are required to be escaped by
the iCalendar standard.  These characters should NOT be
pre-escaped when inserting them into the parse tree.  Instead,
`icalendar-print-text' takes care of escaping text values, and
`icalendar-read-text' takes care of unescaping them, when parsing and
printing iCalendar data."
  '(or string (satisfies ical:text-region-p))
  (zero-or-more (or ical:text-safe-char ?: ?\" ical:escaped-char))
  :reader ical:read-text
  :printer ical:print-text
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.11")

;; 3.3.12 Time - Defined above

;;;; 3.3.13 URI
;; see https://www.rfc-editor.org/rfc/rfc3986#section-3
(rx-define ical:uri-with-scheme
  ;; Group 11: URI scheme; see icalendar-uri-schemes.el
  ;; Group 12: rest of URI after ":"
  ;; This regex mostly just scans for all characters allowed by RFC3986,
  ;; except we make an effort to parse the scheme, because otherwise the
  ;; regex is either too permissive (ical:binary, in particular, matches
  ;; a subset of the characters allowed in a URI) or too complicated to
  ;; be useful.
  ;; TODO: use url-parse.el to parse to struct?
  (seq (group-n 11 (any "a-zA-Z") (zero-or-more (any ?- ?+ ?. "A-Za-z0-9")))
       ":"
       (group-n 12
         (one-or-more
          (any "A-Za-z0-9" ?- ?. ?_ ?~             ; unreserved chars
               ?: ?/ ?? ?# ?\[ ?\] ?@              ; gen-delims
               ?! ?$ ?& ?' ?\( ?\) ?* ?+ ?, ?\; ?= ; sub-delims
               ?%)))))                             ; for %-encoding

(ical:define-type ical:uri "URI"
   "Type for URI values.

The parsed and printed representations are the same: a URI string."
   '(satisfies ical:match-uri-value)
   ical:uri-with-scheme
   :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.13")

;;;; 3.3.3 Calendar User Address
(ical:define-type ical:cal-address "CAL-ADDRESS"
   "Type for Calendar User Address values.

The parsed and printed representations are the same: a URI string.
Typically, this should be a \"mailto:\" URI.

RFC5545 says: \"*When used to address an Internet email transport
  address* for a calendar user, the value MUST be a mailto URI,
  as defined by [RFC2368]\"

Since it is unclear whether there are Calendar User Address values
which are not used to address email, this type does not enforce the use
of the mailto: scheme, but be prepared for problems if you create
values of this type with any other scheme."
   '(and string (satisfies ical:match-cal-address-value))
   ical:uri-with-scheme
   :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.3")

;;;; 3.3.14 UTC Offset
(defun ical:read-utc-offset (s)
  "Read a UTC offset from a string.
S should be a match against rx `icalendar-utc-offset'"
  (let ((sign (if (equal (substring s 0 1) "-") -1 1))
        (nhours (string-to-number (substring s 1 3)))
        (nminutes (string-to-number (substring s 3 5)))
        (nseconds (if (length= s 7)
                      (string-to-number (substring s 5 7))
                    0)))
    (* sign (+ nseconds (* 60 nminutes) (* 60 60 nhours)))))

(defun ical:print-utc-offset (utcoff)
  "Serialize a UTC offset to a string."
  (let* ((sign (if (< utcoff 0) "-" "+"))
         (absoff (abs utcoff))
         (nhours (/ absoff (* 60 60)))
         (no-hours (mod absoff (* 60 60)))
         (nminutes (/ no-hours 60))
         (nseconds (mod no-hours 60)))
    (if (zerop nseconds)
        (format "%s%02d%02d" sign nhours nminutes)
      (format "%s%02d%02d%02d" sign nhours nminutes nseconds))))

(ical:define-type ical:utc-offset "UTC-OFFSET"
  "Type for UTC Offset values.

When printed, a sign followed by a string of digits, like +HHMM
or -HHMMSS.  When read, an integer representing the number of
seconds offset from UTC.  This representation is for compatibility
with `decode-time' and related functions."
  '(integer -999999 999999)
  (seq (or ?+ ?-) ; + is not optional for positive values!
       (= 4 digit) ; HHMM
       (zero-or-one (= 2 digit))) ; SS
  :reader ical:read-utc-offset
  :printer ical:print-utc-offset
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.3.14")


;;; Section 3.2: Property Parameters

(defvar ical:param-types nil ;; populated by ical:define-param
  "Alist mapping printed parameter names to type symbols.")

(defun ical:maybe-quote-param-value (s &optional always)
  "Add quotes around param value string S if required.
If ALWAYS is non-nil, add quotes to S regardless of its contents."
  (if (or always
          (not (string-match (rx ical:paramtext) s))
          (< (match-end 0) (length s)))
      (concat "\"" s "\"")
    s))

(defun ical:read-param-value (type s)
  "Read a value for a parameter of type TYPE from a string S.
S should have already been matched against the regex for TYPE and
the match data should be available to this function.  Returns a
syntax node of type TYPE containing the read value.

If TYPE accepts a list of values, S will be split on the list
separator for TYPE and read individually."
  (let* ((value-type (get type 'ical:value-type)) ; if nil, value is just a string
         (value-regex (when (get type 'ical:value-rx)
                         (rx-to-string (get type 'ical:value-rx))))
         (list-sep (get type 'ical:list-sep))
         (substitute-val (get type 'ical:substitute-value))
         (unrecognized-val (match-string 5)) ; see :unrecognized in define-param
         (raw-val (if unrecognized-val substitute-val s))
         (one-val-reader (if (ical:value-type-symbol-p value-type)
                             (lambda (s) (ical:read-value-node value-type s))
                           #'identity)) ; value is just a string
         ;; values may be quoted even if :quoted does not require it,
         ;; so they need to be stripped of quotes. read-list-with does
         ;; this by default; in the single value case, use string-trim
         (read-val (if list-sep
                       (ical:read-list-with one-val-reader raw-val
                                            value-regex list-sep)
                     (funcall one-val-reader
                              (string-trim raw-val "\"" "\"")))))
    (ical:make-ast-node type
                        (list :value read-val
                              :original-value unrecognized-val))))

(defun ical:parse-param-value (type limit)
  "Parse the value for a parameter of type TYPE from point up to LIMIT.
TYPE should be a type symbol for an iCalendar parameter type.
This function expects point to be at the start of the value
string, after the parameter name and the equals sign.  Returns a
syntax node representing the parameter."
  (let ((full-value-regex (rx-to-string (get type 'ical:full-value-rx))))
    ;; By far the most common invalid data seem to be text values that
    ;; contain unescaped characters (e.g. commas in addresses).  These
    ;; are harmless as long as the parameter accepts arbitrary text and
    ;; does not expect a list of values.  The only such parameter
    ;; defined in RFC5545 is `ical:cnparam', so we treat this as a
    ;; special case and loosen the official regexp to accept anything up
    ;; to the start of the next param or property value:
    (when (and (eq type 'ical:cnparam)
               (not ical:parse-strictly))
      (setq full-value-regex
            (rx (group-n 2 (or ical:quoted-string
                               (zero-or-more (not (any ?: ?\;))))))))

    (unless (re-search-forward full-value-regex limit t)
      (ical:signal-parse-error
       (format "Unable to parse `%s' value between %d and %d"
               type (point) limit)))
    (when (match-string 3)
      (ical:signal-parse-error
       (format "Invalid value for `%s' parameter: %s" type (match-string 3))))

    (let ((value-begin (match-beginning 2))
          (value-end (match-end 2))
          (node (ical:read-param-value type (match-string 2))))
      (ical:ast-node-meta-set node :buffer (current-buffer))
      ;; :begin must be set by parse-params
      (ical:ast-node-meta-set node :value-begin value-begin)
      (ical:ast-node-meta-set node :value-end value-end)
      (ical:ast-node-meta-set node :end value-end)

      node)))

(defun ical:parse-params (limit)
  "Parse the parameter string of the current property, up to LIMIT.
Point should be at the \";\" at the start of the first parameter.
Returns a list of parameters, which may be nil if none are present.
After parsing, point is at the end of the parameter string and the
start of the property value string."
  (let (params param-node)
    (rx-let ((ical:param-start (seq ";" (group-n 1 ical:param-name) "=")))
      (while (re-search-forward (rx ical:param-start) limit t)
        (when-let* ((begin (match-beginning 1))
                    (param-name (match-string 1))
                    (param-type (alist-get (upcase param-name)
                                           ical:param-types
                                           'ical:otherparam
                                            nil #'equal)))
          (condition-case err
              (setq param-node (ical:parse-param-value param-type limit))
            (ical:parse-error
             (ical:handle-parse-error err (format "Skipping bad %s parameter"
                                                  param-name))
             (setq param-node nil)))
          (when param-node
            (ical:ast-node-meta-set param-node :begin begin)
            ;; store the original param name if we didn't recognize it:
            (when (eq param-type 'ical:otherparam)
              (ical:ast-node-meta-set param-node :original-name param-name))
            (push param-node params))))
    (nreverse params))))

(defun ical:print-param-node (node)
  "Serialize a parameter syntax node NODE to a string.
NODE should be a syntax node whose type is an iCalendar
parameter type."
  (let* ((param-type (ical:ast-node-type node))
         (param-name (car (rassq param-type ical:param-types)))
         (name-str (or param-name
                       ;; set by parse-params for unrecognized params:
                       (ical:ast-node-meta-get :original-name node))))

    (unless (and name-str (stringp name-str) (not (equal name-str "")))
      (ical:signal-print-error "No printable parameter name" :node node))

    (let* ((list-sep (get param-type 'ical:list-sep))
           (val/s (ical:ast-node-value node))
           (vals (if (and list-sep (listp val/s))
                     val/s
                   (list val/s)))
           ;; any ical:print-error here propagates:
           (printed (mapcar #'ical:default-value-printer vals))
           ;; add quotes to each value as needed, even if :quoted
           ;; does not require it:
           (must-quote (get param-type 'ical:is-quoted))
           (quoted (mapcar
                    (lambda (v) (ical:maybe-quote-param-value v must-quote))
                    printed))
           (val-str (or (ical:ast-node-meta-get :original-value node)
                        (string-join quoted list-sep)
                        quoted)))

      (unless (and (stringp val-str) (not (equal val-str "")))
        (ical:signal-print-error "Unable to print parameter value" :node node))

      (format ";%s=%s" name-str val-str))))

(defun ical:print-params (param-nodes)
  "Print the property parameter nodes in PARAM-NODES.
Returns the printed parameter list as a string."
  (let (param-strs)
    (dolist (node param-nodes)
      (condition-case err
          (push (ical:print-param-node node) param-strs)
        (ical:print-error
         (ical:handle-print-error err))))
    (apply #'concat (nreverse param-strs))))

;; Parameter definitions in RFC5545:

(ical:define-param ical:altrepparam "ALTREP"
  "Alternate text representation (URI)"
  ical:uri
  :quoted t
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.1")

(ical:define-param ical:cnparam "CN"
  "Common Name"
  ical:param-value
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.2")

(ical:define-param ical:cutypeparam "CUTYPE"
  "Calendar User Type"
  (or "INDIVIDUAL"
      "GROUP"
      "RESOURCE"
      "ROOM"
      "UNKNOWN"
      (group-n 5
        (or ical:x-name ical:iana-token)))
  :default "INDIVIDUAL"
  ;; "Applications MUST treat x-name and iana-token values they
  ;; don't recognize the same way as they would the UNKNOWN
  ;; value":
  :unrecognized "UNKNOWN"
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.3")

(ical:define-param ical:delfromparam "DELEGATED-FROM"
  "Delegators.

This is a comma-separated list of quoted `icalendar-cal-address' URIs,
typically specified on the `icalendar-attendee' property.  The users in
this list have delegated their participation to the user which is
the value of the property."
  ical:cal-address
  :quoted t
  :list-sep ","
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.4")

(ical:define-param ical:deltoparam "DELEGATED-TO"
  "Delegatees.

This is a comma-separated list of quoted `icalendar-cal-address' URIs,
typically specified on the `icalendar-attendee' property.  The users in
this list have been delegated to participate by the user which is
the value of the property."
  ical:cal-address
  :quoted t
  :list-sep ","
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.5")

(ical:define-param ical:dirparam "DIR"
  "Directory Entry Reference.

This parameter may be specified on properties with a
`icalendar-cal-address' value type.  It is a quoted URI which specifies
a reference to a directory entry associated with the calendar
user which is the value of the property."
   ical:uri
   :quoted t
   :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.6")

(ical:define-param ical:encodingparam "ENCODING"
  "Inline Encoding, either \"8BIT\" (text, default) or \"BASE64\" (binary).

If \"BASE64\", the property value is base64-encoded binary data.
This parameter must be specified if the `icalendar-valuetypeparam'
is \"BINARY\"."
  (or "8BIT" "BASE64")
  :default "8BIT"
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.7")

(rx-define ical:mimetype
  (seq ical:mimetype-regname "/" ical:mimetype-regname))

;; from https://www.rfc-editor.org/rfc/rfc4288#section-4.2:
(rx-define ical:mimetype-regname
  (** 1 127 (any "A-Za-z0-9" ?! ?# ?$ ?& ?. ?+ ?- ?^ ?_)))

(ical:define-param ical:fmttypeparam "FMTTYPE"
  "Format Type (Mimetype per RFC4288)

Specifies the media type of the object referenced in the property value,
for example \"text/plain\" or \"text/html\".
Valid media types are defined in RFC4288; see
URL `https://www.rfc-editor.org/rfc/rfc4288#section-4.2'"
  ical:mimetype
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.8")

(ical:define-param ical:fbtypeparam "FBTYPE"
  "Free/Busy Time Type.  Default is \"BUSY\".

RFC5545 gives the following meanings to the values:

FREE: the time interval is free for scheduling.
BUSY: the time interval is busy because one or more events have
  been scheduled for that interval.
BUSY-UNAVAILABLE: the time interval is busy and the interval
  can not be scheduled.
BUSY-TENTATIVE: the time interval is busy because one or more
  events have been tentatively scheduled for that interval.
Other values are treated like BUSY."
  (or "FREE"
      "BUSY-UNAVAILABLE"
      "BUSY-TENTATIVE"
      "BUSY"
      ical:x-name
      ical:iana-token)
  :default "BUSY"
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.9")

;; TODO: see https://www.rfc-editor.org/rfc/rfc5646#section-2.1
(rx-define ical:rfc5646-lang
  (one-or-more (any "A-Za-z0-9" ?-)))

(ical:define-param ical:languageparam "LANGUAGE"
  "Language tag (per RFC5646)

This parameter specifies the language of the property value as a
language tag, for example \"en-US\" for US English or \"no\" for
Norwegian.  Valid language tags are defined in RFC5646; see
URL `https://www.rfc-editor.org/rfc/rfc5646'"
  ical:rfc5646-lang
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.10")

(ical:define-param ical:memberparam "MEMBER"
  "Group or List Membership.

This is a comma-separated list of quoted `icalendar-cal-address'
values.  These are addresses of groups or lists of which the user
in the property value is a member."
  ical:cal-address
  :quoted t
  :list-sep ","
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.11")

(ical:define-param ical:partstatparam "PARTSTAT"
  "Participation status.

The value specifies the participation status of the calendar user
in the property value.  They have different interpretations
depending on whether they occur in a VEVENT, VTODO or VJOURNAL
component.  RFC5545 gives the values the following meanings:

NEEDS-ACTION (all): needs action by the user
ACCEPTED (all): accepted by the user
DECLINED (all): declined by the user
TENTATIVE (VEVENT, VTODO): tentatively accepted by the user
DELEGATED (VEVENT, VTODO): delegated by the user
COMPLETED (VTODO): completed at the `icalendar-date-time' in the
  VTODO's `icalendar-completed' property
IN-PROCESS (VTODO): in the process of being completed"
  (or "NEEDS-ACTION"
      "ACCEPTED"
      "DECLINED"
      "TENTATIVE"
      "DELEGATED"
      "COMPLETED"
      "IN-PROCESS"
      (group-n 5 (or ical:x-name
                     ical:iana-token)))
  ;; "Applications MUST treat x-name and iana-token values
  ;; they don't recognize the same way as they would the
  ;; NEEDS-ACTION value."
  :default "NEEDS-ACTION"
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.12")

(ical:define-param ical:rangeparam "RANGE"
  "Recurrence Identifier Range.

Specifies the effective range of recurrence instances of the property's value.
The value \"THISANDFUTURE\" is the only value compliant with RFC5545;
legacy applications might also produce \"THISANDPRIOR\"."
  "THISANDFUTURE"
  :default "THISANDFUTURE"
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.13")

(ical:define-param ical:trigrelparam "RELATED"
  "Alarm Trigger Relationship.

This parameter may be specified on properties whose values give
an alarm trigger as an `icalendar-duration'.  If the parameter
value is \"START\" (the default), the alarm triggers relative to
the start of the component; similarly for \"END\"."
  (or "START" "END")
  :default "START"
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.14")

(ical:define-param ical:reltypeparam "RELTYPE"
  "Relationship type.

This parameter specifies a hierarchical relationship between the
calendar component referenced in a `icalendar-related-to'
property and the calendar component in which it occurs.
\"PARENT\" means the referenced component is superior to this
one, \"CHILD\" that the referenced component is subordinate to
this one, and \"SIBLING\" means they are peers."
  (or "PARENT"
      "CHILD"
      "SIBLING"
      (group-n 5 (or ical:x-name
                     ical:iana-token)))
  ;; "Applications MUST treat x-name and iana-token values they don't
  ;; recognize the same way as they would the PARENT value."
  :default "PARENT"
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.15")

(ical:define-param ical:roleparam "ROLE"
  "Participation role.

This parameter specifies the participation role of the calendar
user in the property value.  RFC5545 gives the parameter values
the following meanings:
CHAIR: chair of the calendar entity
REQ-PARTICIPANT (default): user's participation is required
OPT-PARTICIPANT: user's participation is optional
NON-PARTICIPANT: user is copied for information purposes only"
  (or "CHAIR"
      "REQ-PARTICIPANT"
      "OPT-PARTICIPANT"
      "NON-PARTICIPANT"
      (group-n 5 (or ical:x-name
                     ical:iana-token)))
  ;; "Applications MUST treat x-name and iana-token values
  ;; they don't recognize the same way as they would the
  ;; REQ-PARTICIPANT value."
  :default "REQ-PARTICIPANT"
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.16")

(ical:define-param ical:rsvpparam "RSVP"
  "RSVP expectation.

This parameter is an `icalendar-boolean' which specifies whether
the calendar user in the property value is expected to reply to
the Organizer of a VEVENT or VTODO."
  ical:boolean
  :default "FALSE"
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.17")

(ical:define-param ical:sentbyparam "SENT-BY"
  "Sent by.

This parameter specifies a calendar user that is acting on behalf
of the user in the property value."
  ;; "The parameter value MUST be a mailto URI as defined in [RFC2368]"
  ;; Weirdly, this is the only place in the standard I've seen "mailto:"
  ;; be *required* for a cal-address.  We ignore this requirement because
  ;; coding around the exception is not worth it: it requires working
  ;; around the fact that two different types, the looser and the more
  ;; stringent cal-address, would need to have the same print name.
  ical:cal-address
  :quoted t
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.18")

(ical:define-param ical:tzidparam "TZID"
  "Time Zone identifier.

This parameter identifies the VTIMEZONE component in the calendar
which should be used to interpret the time value given in the
property.  The value of this parameter must be equal to the value
of the TZID property in that VTIMEZONE component; there must be
exactly one such component for every unique value of this
parameter in the calendar."
  (seq (zero-or-one "/") ical:paramtext)
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.19")

(defun ical:read-value-type (s)
  "Read a value type from string S.
S should contain the printed representation of a value type in a \"VALUE=...\"
property parameter.  If S represents a known type in `icalendar-value-types',
it is read as the associated type symbol.  Otherwise S is returned unchanged."
  (let ((type-assoc (assoc s ical:value-types)))
    (if type-assoc
        (cdr type-assoc)
      s)))

(defun ical:print-value-type (type)
  "Print a value type TYPE.
TYPE should be an iCalendar type symbol naming a known value type
defined with `icalendar-define-type', or a string naming an
unknown type.  If it is a symbol, return the associated printed
representation for the type from `icalendar-value-types'.
Otherwise return TYPE."
  (if (symbolp type)
      (car (rassq type ical:value-types))
    type))

(ical:define-type ical:printed-value-type nil
  "Type to represent values of the `icalendar-valuetypeparam' parameter.

When read, if the type named by the parameter is a known value
type in `icalendar-value-types', it is represented as a type
symbol for that value type.  If it is an unknown value type, it is
represented as a string.  When printed, a string is returned
unchanged; a type symbol is printed as the associated name in
`icalendar-value-types'.

This is not a type defined by RFC5545; it is defined here to
facilitate parsing of the `icalendar-valuetypeparam' parameter."
  '(or string (satisfies ical:printable-value-type-symbol-p))
  (or "BINARY"
      "BOOLEAN"
      "CAL-ADDRESS"
      "DATE-TIME"
      "DATE"
      "DURATION"
      "FLOAT"
      "INTEGER"
      "PERIOD"
      "RECUR"
      "TEXT"
      "TIME"
      "URI"
      "UTC-OFFSET"
      ;; Note: "Applications MUST preserve the value data for x-name
      ;; and iana-token values that they don't recognize without
      ;; attempting to interpret or parse the value data." So in this
      ;; case we don't specify :default or :unrecognized in the
      ;; parameter definition, and we don't put the value in group 5;
      ;; the reader will just preserve whatever string matches here.
      ical:x-name
      ical:iana-token)
  :reader ical:read-value-type
  :printer ical:print-value-type)

(ical:define-param ical:valuetypeparam "VALUE"
  "Property value data type.

This parameter is used to specify the value type of the
containing property's value, if it is not of the default value
type."
  ical:printed-value-type
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.2.20")

(ical:define-param ical:otherparam nil ; don't add to ical:param-types
  "Parameter with an unknown name.

This is not a parameter type defined by RFC5545; it represents
parameters with an unknown name (matching rx `icalendar-param-name')
whose values must be parsed and preserved but not further
interpreted."
  ical:param-value)

(rx-define ical:other-param-safe
  ;; we use this rx to skip params when matching properties and
  ;; their values.  Thus we *don't* capture the param names and param values
  ;; in numbered groups here, which would clobber the groups of the enclosing
  ;; expression.
  (seq ";"
       (or ical:iana-token ical:x-name)
       "="
       (ical:comma-list ical:param-value)))


;;; Properties:

(defvar ical:property-types nil ;; populated by ical:define-property
  "Alist mapping printed property names to type symbols.")

(defun ical:read-property-value (type s &optional params)
  "Read a value for the property type TYPE from a string S.

TYPE should be a type symbol for an iCalendar property type
defined with `icalendar-define-property'.  The property value is
assumed to be of TYPE's default value type, unless an
`icalendar-valuetypeparam' parameter appears in PARAMS, in which
case a value of that type will be read.  S should have already
been matched against TYPE's value regex and the match data should
be available to this function.  Returns a property syntax node of
type TYPE containing the read value and the list of PARAMS.

If TYPE accepts lists of values, they will be split from S on the
list separator and read separately."
  (let* ((value-type (or (ical:value-type-from-params params)
                         (get type 'ical:default-type)))
         (list-sep (get type 'ical:list-sep))
         (unrecognized-val (match-string 5))
         (raw-val (if unrecognized-val
                      (get type 'ical:substitute-value)
                    s))
         (value (if list-sep
                    (ical:read-list-of value-type raw-val list-sep)
                  (ical:read-value-node value-type raw-val))))
    (ical:make-ast-node type
                        (list :value value
                              :original-value unrecognized-val)
                        params)))

(defun ical:parse-property-value (type limit &optional params)
  "Parse a value for the property type TYPE from point up to LIMIT.
This function expects point to be at the start of the value
expression, after \"PROPERTY-NAME[PARAM...]:\".  Returns a syntax
node of type TYPE containing the parsed value and the list of
PARAMS."
  (let ((start (point))
        (full-value-regex (rx-to-string (get type 'ical:full-value-rx))))

    ;; By far the most common invalid data seem to be text values that
    ;; contain unescaped characters (e.g. commas in addresses in
    ;; LOCATION).  These are harmless as long as the property accepts
    ;; any text value, accepts no other types of values, and does not
    ;; expect a list of values.  So we treat this as a special case and
    ;; loosen the regexp to accept any non-control character until eol:
    (when (and (eq 'ical:text (get type 'ical:default-type))
               (equal (rx-to-string 'ical:text t)
                      (rx-to-string (get type 'ical:value-rx) t))
               (null (get type 'ical:other-types))
               (not (ical:expects-list-of-values-p type))
               (not ical:parse-strictly))
        (setq full-value-regex
              (rx (group-n 2 (zero-or-more (not (any control))))
                  line-end)))

    (unless (re-search-forward full-value-regex limit t)
      (ical:signal-parse-error
       (format "Unable to parse `%s' property value between %d and %d"
               type start limit)
       :restart-at (1+ limit)))

    (when (match-string 3)
      (ical:signal-parse-error
       (format "Invalid value for `%s' property: %s" type (match-string 3))
       :restart-at (1+ limit)))

    (let* ((value-begin (match-beginning 2))
           (value-end (match-end 2))
           (end value-end)
           (node (ical:read-property-value type (match-string 2) params)))
      (ical:ast-node-meta-set node :buffer (current-buffer))
      ;; 'begin must be set by parse-property
      (ical:ast-node-meta-set node :value-begin value-begin)
      (ical:ast-node-meta-set node :value-end value-end)
      (ical:ast-node-meta-set node :end end)

      node)))

(defun ical:print-property-node (node)
  "Serialize a property syntax node NODE to a string."
  (setq node (ical:maybe-add-value-param node))
  (let* ((type (ical:ast-node-type node))
         (list-sep (get type 'ical:list-sep))
         (property-name (car (rassq type ical:property-types)))
         (name-str (or property-name
                       (ical:ast-node-meta-get :original-name node)))
         (params (ical:ast-node-children node))
         (value (ical:ast-node-value node))
         (value-str
          (or (ical:ast-node-meta-get :original-value node)
              ;; any ical:print-error here propagates:
              (if list-sep
                  (string-join (mapcar #'ical:default-value-printer value)
                               list-sep)
                (ical:default-value-printer value)))))

    (unless (and (stringp name-str) (length> name-str 0))
      (ical:signal-print-error
       (format "Unknown property name for type `%s'" type)
       :node node))

    (concat name-str
            (ical:print-params params)
            ":"
            value-str
            "\n")))

(defun ical:maybe-add-value-param (property-node)
  "Add a VALUE parameter to PROPERTY-NODE if necessary.

If the type of PROPERTY-NODE's value is not the same as its
default-type, check that its parameter list contains an
`icalendar-valuetypeparam' specifying that type as the type for
the value.  If not, add such a parameter to PROPERTY-NODE's list
of parameters.  Returns the possibly-modified PROPERTY-NODE.

If the parameter list already contains a value type parameter for
a type other than the property value's type, an
`icalendar-validation-error' is signaled.

If PROPERTY's value is a list, the type of the first element will
be assumed to be the type for all the values in the list.  If the
list is empty, no change will be made to PROPERTY's parameters."
  (catch 'no-value-type
    (let* ((property-type (ical:ast-node-type property-node))
           (value/s (ical:ast-node-value property-node))
           (value (if (and (ical:expects-list-of-values-p property-type)
                           (listp value/s))
                      (car value/s)
                    value/s))
           (value-type (cond ((stringp value) 'ical:text)
                             ((ical:ast-node-p value)
                              (ical:ast-node-type value))
                             ;; if we can't determine a type from the value, bail:
                             (t (throw 'no-value-type property-node))))
           (params (ical:ast-node-children property-node))
           (expected-type (ical:value-type-from-params params)))

      (when (not (eq value-type (get property-type 'ical:default-type)))
        (if expected-type
            (when (not (eq value-type expected-type))
              (ical:signal-validation-error
                (format (concat "Mismatching VALUE parameter.  VALUE specifies %s "
                                "but property value has type %s")
                        expected-type value-type)
                :node property-node))
          ;; the value isn't of the default type, but we didn't find a
          ;; VALUE parameter, so add one now:
          (let* ((valuetype-param
                  (ical:make-ast-node 'ical:valuetypeparam
                                      (list :value (ical:make-ast-node
                                                    'ical:printed-value-type
                                                    (list :value value-type)))))
                 (new-params (cons valuetype-param
                                   (ical:ast-node-children property-node))))
            (apply #'ical:ast-node-set-children property-node new-params))))

      ;; Return the modified property node:
      property-node)))

(defun ical:value-type-from-params (params)
  "Return the type symbol associated with any VALUE parameter in PARAMS.
PARAMS should be a list of parameter nodes.  The type symbol specified by
the first `icalendar-valuetypeparam' in PARAMS, or nil, will be returned."
  (catch 'found
    (dolist (param params)
      (when (ical:value-param-p param)
        (let ((type (ical:ast-node-value
                     (ical:ast-node-value param))))
          (throw 'found type))))))

(defun ical:parse-property (limit)
  "Parse the current property, up to LIMIT.

Point should be at the beginning of a property line; LIMIT should be the
position at the end of the line.

Returns a syntax node for the property.  After parsing, point is at the
beginning of the next content line."
  (rx-let ((ical:property-start (seq line-start (group-n 1 ical:name))))
    (let (line-begin line-end property-name property-type params node)
      ;; Property name
      (unless (re-search-forward (rx ical:property-start) limit t)
        (ical:signal-parse-error
         "Malformed property: could not match property name"
         :restart-at (1+ limit)))

      (setq property-name (match-string 1))
      (setq line-begin (line-beginning-position))
      (setq line-end (line-end-position))

      ;; Parameters
      (when (looking-at-p ";")
        (setq params (ical:parse-params line-end)))

      (unless (looking-at-p ":")
        (ical:signal-parse-error
         "Malformed property: parameters did not end at colon"
         :restart-at (1+ limit)))
      (forward-char)

      ;; Value
      (setq property-type (alist-get (upcase property-name)
                                     ical:property-types
                                     'ical:other-property
                                     nil #'equal))
      (setq node (ical:parse-property-value property-type limit params))

      ;; sanity check, since e.g. invalid base64 data might not
      ;; match all the way to the end of the line, as test
      ;; rfc5545-sec3.1.3/2 initially revealed
      (unless (eql (point) (line-end-position))
        (ical:signal-parse-error
         (format "%s property value did not consume line: %s"
                 property-name
                 (ical:default-value-printer (ical:ast-node-value node)))
         :restart-at (1+ limit)))

      ;; value, children are set in ical:read-property-value,
      ;; value-begin, value-end, end in ical:parse-property-value.
      ;; begin and original-name are only available here:
      (ical:ast-node-meta-set node :begin line-begin)
      (when (eq property-type 'ical:other-property)
        (ical:ast-node-meta-set node :original-name property-name))

      ;; Set point up for the next property parser.
      (while (not (bolp))
        (forward-char))

      ;; Return the syntax node
      node)))


;;;; Section 3.7: Calendar Properties
(ical:define-property ical:calscale "CALSCALE"
  "Calendar scale.

This property specifies the time scale of an
`icalendar-vcalendar' object.  The only scale defined by RFC5545
is \"GREGORIAN\", which is the default."
  ;; only allowed value:
  "GREGORIAN"
  :default "GREGORIAN"
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.7.1")

(ical:define-property ical:method "METHOD"
  "Method for a scheduling request.

When an `icalendar-vcalendar' is sent in a MIME message, this property
specifies the semantics of the request in the message: e.g. it is
a request to publish the calendar object, or a reply to an
invitation.  This property and the MIME message's \"method\"
parameter value must be the same.

RFC5545 does not define any methods, but RFC5546 does; see
URL `https://www.rfc-editor.org/rfc/rfc5546.html#section-3.2'"
  ;; TODO: implement methods in RFC5546?
  ical:iana-token
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.7.2")

(ical:define-property ical:prodid "PRODID"
  "Product Identifier.

This property identifies the program that created an
`icalendar-vcalendar' object.  It must be specified exactly once in a
calendar object.  Its value should be a globally unique identifier for
the program.  RFC5545 suggests using an ISO \"Formal Public Identifier\";
see URL `https://en.wikipedia.org/wiki/Formal_Public_Identifier'."
  ical:text
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.7.3")

(ical:define-property ical:version "VERSION"
  "Version (2.0 corresponds to RFC5545).

This property specifies the version number of the iCalendar
specification to which an `icalendar-vcalendar' object conforms,
and must be specified exactly once in a calendar object.  It is
either the string \"2.0\" or a string like MIN;MAX specifying
minimum and maximum versions of future revisions of the
specification."
  (or "2.0"
      ;; minver ";" maxver
      (seq ical:iana-token ?\; ical:iana-token))
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.7.4")


;;;; Section 3.8:
;;;;; Section 3.8.1: Descriptive Component Properties

(ical:define-property ical:attach "ATTACH"
  "Attachment.

This property specifies a file attached to an iCalendar
component, either via a URI, or as encoded binary data.  In
`icalendar-valarm' components, it is used to specify the
notification sent by the alarm."
  ;; Groups 11, 12 are used in ical:uri
  (or (group-n 13 ical:uri)
      (group-n 14 ical:binary))
  :default-type ical:uri
  :other-types (ical:binary)
  :child-spec (:zero-or-one (ical:fmttypeparam
                             ical:valuetypeparam
                             ical:encodingparam)
               :zero-or-more (ical:otherparam))
  :other-validator ical:attach-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.1")

(defun ical:attach-validator (node)
  "Additional validator for an `icalendar-attach' NODE.
Checks that NODE has a correct `icalendar-encodingparam' and
`icalendar-valuetypeparam' if its value is an `icalendar-binary'.

This function is called by `icalendar-ast-node-valid-p' for
ATTACH nodes; it is not normally necessary to call it directly."
  (let* ((value-node (ical:ast-node-value node))
         (value-type (ical:ast-node-type value-node))
         (valtypeparam (ical:ast-node-first-child-of 'ical:valuetypeparam node))
         (encodingparam (ical:ast-node-first-child-of 'ical:encodingparam node)))

    (when (eq value-type 'ical:binary)
      (unless (and (ical:ast-node-p valtypeparam)
                   (eq 'ical:binary
                       (ical:ast-node-value ; unwrap inner printed-value-type
                        (ical:ast-node-value valtypeparam))))
        (ical:signal-validation-error
         "`icalendar-binary' attachment requires 'VALUE=BINARY' parameter"
        :node node))
      (unless (and (ical:ast-node-p encodingparam)
                   (equal "BASE64" (ical:ast-node-value encodingparam)))
        (ical:signal-validation-error
         "`icalendar-binary' attachment requires 'ENCODING=BASE64' parameter"
         :node node)))
    ;; success:
    node))

(ical:define-property ical:categories "CATEGORIES"
  "Categories.

This property lists categories or subtypes of an iCalendar
component for e.g. searching or filtering.  The categories can be
any `icalendar-text' value."
  ical:text
  :list-sep ","
  :child-spec (:zero-or-one (ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.2")

(ical:define-property ical:class "CLASS"
  "(Access) Classification.

This property specifies the scope of access that the calendar
owner intends for a given component, e.g. public or private."
  (or "PUBLIC"
      "PRIVATE"
      "CONFIDENTIAL"
      (group-n 5
        (or ical:iana-token
            ical:x-name)))
  ;; "If not specified in a component that allows this property, the
  ;; default value is PUBLIC.  Applications MUST treat x-name and
  ;; iana-token values they don't recognize the same way as they would
  ;; the PRIVATE value."
  :default "PUBLIC"
  :unrecognized "PRIVATE"
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.3")

(ical:define-property ical:comment "COMMENT"
  "Comment to calendar user.

This property can be specified multiple times in calendar components,
and can contain any `icalendar-text' value."
  ical:text
  :child-spec (:zero-or-one (ical:altrepparam ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.4")

(ical:define-property ical:description "DESCRIPTION"
  "Description.

This property should be a longer, more complete description of
the calendar component than is contained in the
`icalendar-summary' property.  In a `icalendar-vjournal'
component, it is used to capture a journal entry, and may be
specified multiple times.  Otherwise it may only be specified
once.  In an `icalendar-valarm' component, it contains the
notification text for a DISPLAY or EMAIL alarm."
  ical:text
  :child-spec (:zero-or-one (ical:altrepparam ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.5")

(defun ical:read-geo-coordinates (s)
  "Read an `icalendar-geo-coordinates' value from string S."
  (let ((vals (mapcar #'string-to-number (string-split s ";"))))
    (cons (car vals) (cadr vals))))

(defun ical:print-geo-coordinates (val)
  "Serialize an `icalendar-geo-coordinates' value to a string."
  (concat (number-to-string (car val)) ";" (number-to-string (cdr val))))

(defun ical:geo-coordinates-p (val)
  "Return non-nil if VAL is an `icalendar-geo-coordinates' value."
  (and (floatp (car val)) (floatp (cdr val))))

(ical:define-type ical:geo-coordinates nil ; don't add to ical:value-types
  "Type for global positions.

This is not a type defined by RFC5545; it is defined here to
facilitate parsing the `icalendar-geo' property.  When printed, it
is represented as a pair of `icalendar-float' values separated by
a semicolon, like LATITUDE;LONGITUDE.  When read, it is a dotted
pair of Elisp floats (LATITUDE . LONGITUDE)."
  '(satisfies ical:geo-coordinates-p)
  (seq ical:float ";" ical:float)
  :reader ical:read-geo-coordinates
  :printer ical:print-geo-coordinates)

(ical:define-property ical:geo "GEO"
  "Global position of a component as a pair LATITUDE;LONGITUDE.

Both values are floats representing a number of degrees.  The
latitude value is north of the equator if positive, and south of
the equator if negative.  The longitude value is east of the prime
meridian if positive, and west of it if negative."
  ical:geo-coordinates
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.6")

(ical:define-property ical:location "LOCATION"
  "Location.

This property describes the intended location or venue of a
component, e.g. a particular room or building, with an
`icalendar-text' value.  RFC5545 suggests using the
`icalendar-altrep' parameter on this property to provide more
structured location information."
  ical:text
  :child-spec (:zero-or-one (ical:altrepparam ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.7")

;; TODO: type for percentages?
(ical:define-property ical:percent-complete "PERCENT-COMPLETE"
  "Percent Complete.

This property describes progress toward the completion of an
`icalendar-vtodo' component.  It can appear at most once in such a
component.  If this TODO is assigned to multiple people, the value
represents the completion state for each person individually.  The
value should be between 0 and 100 (though this is not currently
enforced here)."
  ical:integer
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.8")

;; TODO: type for priority values?
(ical:define-property ical:priority "PRIORITY"
  "Priority.

This property describes the priority of a component. 0 means an
undefined priority.  Other values range from 1 (highest priority)
to 9 (lowest priority).  See RFC5545 for suggestions on how to
represent other priority schemes with this property."
  ical:integer
  :default "0"
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.9")

(ical:define-property ical:resources "RESOURCES"
  "Resources for an activity.

This property is a list of `icalendar-text' values that describe
any resources required or foreseen for the activity represented
by a component, e.g. a projector and screen for a meeting."
  ical:text
  :list-sep ","
  :child-spec (:zero-or-one (ical:altrepparam ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.10")

(ical:define-type ical:status-keyword nil
  "Keyword value of a STATUS property.

This is not a real type defined by RFC5545; it is defined here to
facilitate parsing that property."
  '(and string (satisfies ical:match-status-keyword-value))
  ;; Note that this type does NOT allow arbitrary text:
  (or "TENTATIVE"
      "CONFIRMED"
      "CANCELLED"
      "NEEDS-ACTION"
      "COMPLETED"
      "IN-PROCESS"
      "DRAFT"
      "FINAL"))

(ical:define-property ical:status "STATUS"
  "Overall status or confirmation.

This property is a keyword used by an Organizer to inform
Attendees about the status of a component, e.g. whether an
`icalendar-vevent' has been cancelled, whether an
`icalendar-vtodo' has been completed, or whether an
`icalendar-vjournal' is still in draft form.  It can be specified
at most once on these components."
  ical:status-keyword
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.11")

(ical:define-property ical:summary "SUMMARY"
  "Short summary.

This property provides a short, one-line description of a
component for display purposes.  In an EMAIL `icalendar-valarm',
it is used as the subject of the email.  A longer description of
the component can be provided in the `icalendar-description'
property."
  ical:text
  :child-spec (:zero-or-one (ical:altrepparam ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.1.12")

;;;;; Section 3.8.2: Date and Time Component Properties

(defun ical:property-w/tzid-validator (node)
  "Additional validator for property NODE with `icalendar-tzid' parameters.
Checks that this parameter does not occur in combination with an
`icalendar-date' value or an `icalendar-date-time' in UTC time."
  (ical:with-property node
     ((ical:tzidparam :first tzidnode))
    (when (and tzidnode (eq value-type 'ical:date))
      (icalendar-signal-validation-error
       "Property cannot contain `icalendar-tzidparam' with `icalendar-date' value"
       :node node))
    (when (and tzidnode (eq value-type 'ical:date-time)
               (ical:date-time-is-utc-p value))
      (icalendar-signal-validation-error
       "Property cannot contain `icalendar-tzidparam' in combination with UTC time"
       :node node))))

(ical:define-property ical:completed "COMPLETED"
  "Time completed.

This property is a timestamp that records the date and time when
an `icalendar-vtodo' was actually completed.  The value must be an
`icalendar-date-time' with a UTC time."
  ical:date-time
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.2.1")

(ical:define-property ical:dtend "DTEND"
  "End time of an event or free/busy block.

This property's value specifies when an `icalendar-vevent' or
`icalendar-freebusy' ends.  Its value must be of the same type as
the value of the component's corresponding `icalendar-dtstart'
property.  The value is a non-inclusive bound, i.e., the value of
this property must be the first time or date *after* the end of
the event or free/busy block."
  (or ical:date-time
      ical:date)
  :default-type ical:date-time
  :other-types (ical:date)
  :child-spec (:zero-or-one (ical:valuetypeparam ical:tzidparam)
               :zero-or-more (ical:otherparam))
  :other-validator ical:property-w/tzid-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.2.2")

(ical:define-property ical:due "DUE"
  "Due date.

This property specifies the date (and possibly time) by which an
`icalendar-todo' item is expected to be completed, i.e., its
deadline.  If the component also has an `icalendar-dtstart'
property, the two properties must have the same value type, and
the value of the DTSTART property must be earlier than the value
of this property."
  (or ical:date-time
      ical:date)
  :default-type ical:date-time
  :other-types (ical:date)
  :child-spec (:zero-or-one (ical:valuetypeparam ical:tzidparam)
               :zero-or-more (ical:otherparam))
  :other-validator ical:property-w/tzid-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.2.3")

(ical:define-property ical:dtstart "DTSTART"
  "Start time of a component.

This property's value specifies when a component starts.  In an
`icalendar-vevent', it specifies the start of the event.  In an
`icalendar-vfreebusy', it specifies the start of the free/busy
block.  In `icalendar-standard' and `icalendar-daylight'
sub-components, it defines the start time of a time zone
specification.

It is required in any component with an `icalendar-rrule'
property, and in any `icalendar-vevent' component contained in a
calendar that does not have a `icalendar-method' property.

Its value must be of the same type as the value of the
component's corresponding `icalendar-dtend' property.  In an
`icalendar-vtodo' component, it must also be of the same type as
the value of an `icalendar-due' property (if present)."
  (or ical:date-time
      ical:date)
  :default-type ical:date-time
  :other-types (ical:date)
  :child-spec (:zero-or-one (ical:valuetypeparam ical:tzidparam)
               :zero-or-more (ical:otherparam))
  :other-validator ical:property-w/tzid-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.2.4")

(ical:define-property ical:duration "DURATION"
  "Duration.

This property specifies a duration of time for a component.
In an `icalendar-vevent', it can be used to implicitly specify
the end of the event, instead of an explicit `icalendar-dtend'.
In an `icalendar-vtodo', it can likewise be used to implicitly specify
the due date, instead of an explicit `icalendar-due'.
In an `icalendar-valarm', it used to specify the delay period
before the alarm repeats.

If a related `icalendar-dtstart' property has an `icalendar-date'
value, then the duration must be given as a number of weeks or days."
  ical:dur-value
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.2.5")

(ical:define-property ical:freebusy "FREEBUSY"
  "Free/Busy Times.

This property specifies a list of periods of free or busy time in
an `icalendar-vfreebusy' component.  Whether it specifies free or
busy times is determined by its `icalendar-fbtype' parameter.  The
times in each period must be in UTC format."
  ical:period
  :list-sep ","
  :child-spec (:zero-or-one (ical:fbtypeparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.2.6")

(ical:define-property ical:transp "TRANSP"
  "Time Transparency for free/busy searches.

Note that this property only allows two values: \"TRANSPARENT\"
or \"OPAQUE\".  An OPAQUE value means that the component consumes
time on a calendar.  TRANSPARENT means it does not, and thus is
invisible to free/busy time searches."
  ;; Note that this does NOT allow arbitrary text:
  (or "TRANSPARENT"
      "OPAQUE")
  :default "OPAQUE"
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.2.7")

;;;;; Section 3.8.3: Time Zone Component Properties

(ical:define-property ical:tzid "TZID"
  "Time Zone Identifier.

This property specifies the unique identifier for a time zone in
an `icalendar-vtimezone' component, and is a required property of
that component.  This is an identifier that `icalendar-tzidparam'
parameters in other components may then refer to."
  (seq (zero-or-one "/") ical:text)
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.3.1")

(ical:define-property ical:tzname "TZNAME"
  "Time Zone Name.

This property specifies a customary name for a time zone in
`icalendar-daylight' and `icalendar-standard' sub-components."
  ical:text
  :child-spec (:zero-or-one (ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.3.2")

(ical:define-property ical:tzoffsetfrom "TZOFFSETFROM"
  "Time Zone Offset (prior to observance).

This property specifies the time zone offset that is in use
*prior to* this time zone observance.  It is used to calculate the
absolute time at which the observance takes place.  It is a
required property of an `icalendar-vtimezone' component.  Positive
numbers indicate time east of the prime meridian (ahead of UTC).
Negative numbers indicate time west of the prime meridian (behind
UTC)."
  ical:utc-offset
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.3.3")

(ical:define-property ical:tzoffsetto "TZOFFSETTO"
  "Time Zone Offset (in this observance).

This property specifies the time zone offset that is in use *in*
this time zone observance.  It is used to calculate the absolute
time at which a new observance takes place.  It is a required
property of `icalendar-standard' and `icalendar-daylight'
components.  Positive numbers indicate time east of the prime
meridian (ahead of UTC).  Negative numbers indicate time west of
the prime meridian (behind UTC)."
  ical:utc-offset
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.3.4")

(ical:define-property ical:tzurl "TZURL"
  "Time Zone URL.

This property specifies a URL where updated versions of an
`icalendar-vtimezone' component are published."
  ical:uri
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.3.5")

;;;;; Section 3.8.4: Relationship Component Properties

(ical:define-property ical:attendee "ATTENDEE"
  "Attendee.

This property specfies a participant in a `icalendar-vevent',
`icalendar-vtodo', or `icalendar-valarm'.  It is required when the
containing component represents event, task, or notification for
a *group* of people, but not for components that simply represent
these items in a single user's calendar (in that case, it should
not be specified).  The property can be specified multiple times,
once for each participant in the event or task.  In an
EMAIL-category VALARM component, this property specifies the
address of the user(s) who should receive the notification email.

The parameters `icalendar-roleparam', `icalendar-partstatparam',
`icalendar-rsvpparam', `icalendar-delfromparam', and
`icalendar-deltoparam' are especially relevant for further
specifying the roles of each participant in the containing
component."
  ical:cal-address
  :child-spec (:zero-or-one (ical:cutypeparam
                             ical:memberparam
                             ical:roleparam
                             ical:partstatparam
                             ical:rsvpparam
                             ical:deltoparam
                             ical:delfromparam
                             ical:sentbyparam
                             ical:cnparam
                             ical:dirparam
                             ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.4.1")

(ical:define-property ical:contact "CONTACT"
  "Contact.

This property provides textual contact information relevant to an
`icalendar-vevent', `icalendar-vtodo', `icalendar-vjournal', or
`icalendar-vfreebusy'."
  ical:text
  :child-spec (:zero-or-one (ical:altrepparam ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.4.2")

(ical:define-property ical:organizer "ORGANIZER"
  "Organizer.

This property specifies the organizer of a group-scheduled
`icalendar-vevent', `icalendar-vtodo', or `icalendar-vjournal'.
It is required in those components if they represent a calendar
entity with multiple participants.  In an `icalendar-vfreebusy'
component, it used to specify the user requesting free or busy
time, or the user who published the calendar that the free/busy
information comes from."
  ical:cal-address
  :child-spec (:zero-or-one (ical:cnparam
                             ical:dirparam
                             ical:sentbyparam
                             ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.4.3")

(ical:define-property ical:recurrence-id "RECURRENCE-ID"
  "Recurrence ID.

This property is used together with the `icalendar-uid' and
`icalendar-sequence' properties to identify a specific instance
of a recurring `icalendar-vevent', `icalendar-vtodo', or
`icalendar-vjournal' component.  The property value is the
original value of the `icalendar-dtstart' property of the
recurrence instance.  Its value must have the same type as that
property's value, and both must specify times in the same way
(either local or UTC)."
  (or ical:date-time
      ical:date)
  :default-type ical:date-time
  :other-types (ical:date)
  :child-spec (:zero-or-one (ical:valuetypeparam
                             ical:tzidparam
                             ical:rangeparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.4.4")

(ical:define-property ical:related-to "RELATED-TO"
  "Related To (component UID).

This property specifies the `icalendar-uid' value of a different,
related calendar component.  It can be specified on an
`icalendar-vevent', `icalendar-vtodo', or `icalendar-vjournal'
component.  An `icalendar-reltypeparam' can be used to specify the
relationship type."
  ical:text
  :child-spec (:zero-or-one (ical:reltypeparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.4.5")

(ical:define-property ical:url "URL"
  "Uniform Resource Locator.

This property specifies the URL associated with an
`icalendar-vevent', `icalendar-vtodo', `icalendar-vjournal', or
`icalendar-vfreebusy' component."
  ical:uri
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.4.6")

;; TODO: UID should probably be its own type
(ical:define-property ical:uid "UID"
  "Unique Identifier.

This property specifies a globally unique identifier for the
containing component, and is required in an `icalendar-vevent',
`icalendar-vtodo', `icalendar-vjournal', or `icalendar-vfreebusy'
component.

RFC5545 requires that the program generating the UID guarantee
that it be unique, and recommends generating it in a format which
includes a timestamp on the left hand side of an '@' character,
and the domain name or IP address of the host on the right-hand
side."
  ical:text
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.4.7")

;;;;; Section 3.8.5: Recurrence Component Properties

(ical:define-property ical:exdate "EXDATE"
  "Exception Date-Times.

This property defines a list of exceptions to a recurrence rule
in an `icalendar-vevent', `icalendar-todo', `icalendar-vjournal',
`icalendar-standard', or `icalendar-daylight' component.  Together
with the `icalendar-dtstart', `icalendar-rrule', and
`icalendar-rdate' properties, it defines the recurrence set of
the component."
  (or ical:date-time
      ical:date)
  :default-type ical:date-time
  :other-types (ical:date)
  :list-sep ","
  :child-spec (:zero-or-one (ical:valuetypeparam ical:tzidparam)
               :zero-or-more (ical:otherparam))
  :other-validator ical:property-w/tzid-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.5.1")

(ical:define-property ical:rdate "RDATE"
  "Recurrence Date-Times.

This property defines a list of date-times or dates on which an
`icalendar-vevent', `icalendar-todo', `icalendar-vjournal',
`icalendar-standard', or `icalendar-daylight' component recurs.
Together with the `icalendar-dtstart', `icalendar-rrule', and
`icalendar-exdate' properties, it defines the recurrence set of
the component."
  (or ical:period
      ical:date-time
      ical:date)
  :default-type ical:date-time
  :other-types (ical:date ical:period)
  :list-sep ","
  :child-spec (:zero-or-one (ical:valuetypeparam ical:tzidparam)
               :zero-or-more (ical:otherparam))
  :other-validator ical:property-w/tzid-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.5.2")

(ical:define-property ical:rrule "RRULE"
  "Recurrence Rule.

This property defines a rule or repeating pattern for the dates
and times on which an `icalendar-vevent', `icalendar-todo',
`icalendar-vjournal', `icalendar-standard', or
`icalendar-daylight' component recurs.  Together with the
`icalendar-dtstart', `icalendar-rdate', and `icalendar-exdate'
properties, it defines the recurrence set of the component."
  ical:recur
  ;; TODO: faces for subexpressions?
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.5.3")

;;;;; Section 3.8.6: Alarm Component Properties

(ical:define-property ical:action "ACTION"
  "Action (when alarm triggered).

This property defines the action to be taken when the containing
`icalendar-valarm' component is triggered.  It is a required
property in an alarm component."
  (or "AUDIO"
      "DISPLAY"
      "EMAIL"
      (group-n 5
        (or ical:iana-token
            ical:x-name)))
  ;; "Applications MUST ignore alarms with x-name and iana-token values
  ;; they don't recognize." This substitute is not defined in the
  ;; standard but is the simplest way to parse such alarms:
  :unrecognized "IGNORE"
  :default-type ical:text
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.6.1")

(ical:define-property ical:repeat "REPEAT"
  "Repeat Count (after initial trigger).

This property specifies the number of times an `icalendar-valarm'
should repeat after it is initially triggered.  This property,
along with the `icalendar-duration' property, is required if the
alarm triggers more than once."
  ical:integer
  :default "0"
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.6.2")

(ical:define-property ical:trigger "TRIGGER"
  "Trigger.

This property specifies when an `icalendar-valarm' should
trigger.  If the value is an `icalendar-dur-value', it represents
a time of that duration relative to the start or end of a related
`icalendar-vevent' or `icalendar-vtodo'.  Whether the trigger
applies to the start time or end time of the related component
can be specified with the `icalendar-trigrelparam' parameter.  A
positive duration value triggers after the start or end of the
related component; a negative duration value triggers before.

If the value is an `icalendar-date-time', it must be in UTC
format, and it triggers at the specified time."
  (or ical:dur-value
      ical:date-time)
  :default-type ical:dur-value
  :other-types (ical:date-time)
  :child-spec (:zero-or-one (ical:valuetypeparam ical:trigrelparam)
               :zero-or-more (ical:otherparam))
  :other-validator ical:trigger-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.6.3")

(defun ical:trigger-validator (node)
  "Additional validator for an `icalendar-trigger' NODE.
Checks that NODE has valid parameters depending on the type of its value.

This function is called by `icalendar-ast-node-valid-p' for
TRIGGER nodes; it is not normally necessary to call it directly."
  (let* ((params (ical:ast-node-children node))
         (value-node (ical:ast-node-value node))
         (value-type (and value-node (ical:ast-node-type value-node))))
    (when (eq value-type 'ical:date-time)
      (let ((expl-type (ical:value-type-from-params params))
            (dt-value (ical:ast-node-value value-node)))
        (unless (eq expl-type 'ical:date-time)
          (ical:signal-validation-error
           (concat "Explicit `icalendar-valuetypeparam' required in "
                   "`icalendar-trigger' with non-duration value")
           :node node))
        (when (ical:ast-node-first-child-of 'ical:trigrelparam node)
          (ical:signal-validation-error
           (concat "`icalendar-trigrelparam' not allowed in "
                   "`icalendar-trigger' with non-duration value")
           :node node))
        (unless (ical:date-time-is-utc-p dt-value)
          (ical:signal-validation-error
           (concat "`icalendar-date-time' value of `icalendar-trigger' "
                   "must be in UTC time")
           :node node))))
    ;; success:
    node))

;;;;; Section 3.8.7: Change Management Component Properties

(ical:define-property ical:created "CREATED"
  "Date-Time Created.

This property specifies the date and time when the calendar user
initially created an `icalendar-vevent', `icalendar-vtodo', or
`icalendar-vjournal' in the calendar database.  The value must be
in UTC time."
  ical:date-time
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.7.1")

(ical:define-property ical:dtstamp "DTSTAMP"
  "Timestamp (of last revision or instance creation).

In an `icalendar-vevent', `icalendar-vtodo',
`icalendar-vjournal', or `icalendar-vfreebusy', this property
specifies the date and time when the calendar user last revised
the component's data in the calendar database.  (In this case, it
is equivalent to the `icalendar-last-modified' property.)

If this property is specified on an `icalendar-vcalendar' object
which contains an `icalendar-method' property, it specifies the
date and time when that instance of the calendar object was
created.  In this case, it differs from the `icalendar-creation'
and `icalendar-last-modified' properties: whereas those specify
the time the underlying data was created and last modified in the
calendar database, this property specifies when the calendar
object *representing* that data was created.

The value must be in UTC time."
  ical:date-time
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.7.2")

(ical:define-property ical:last-modified "LAST-MODIFIED"
  "Last Modified timestamp.

This property specifies when the data in an `icalendar-vevent',
`icalendar-vtodo', `icalendar-vjournal', or `icalendar-vtimezone'
was last modified in the calendar database."
  ical:date-time
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.7.3")

(ical:define-property ical:sequence "SEQUENCE"
  "Revision Sequence Number.

This property specifies the number of the current revision in a
sequence of revisions in an `icalendar-vevent',
`icalendar-vtodo', or `icalendar-vjournal' component.  It starts
at 0 and should be incremented monotonically every time the
Organizer makes a significant revision to the calendar data that
component represents."
  ical:integer
  :default "0"
  :child-spec (:zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.7.4")

;;;;; Section 3.8.8: Miscellaneous Component Properties
;; IANA and X- properties should be parsed and printed but can be ignored:
(ical:define-property ical:other-property nil ; don't add to ical:property-types
  "IANA or X-name property.

This property type corresponds to the IANA Properties and
Non-Standard Properties defined in RFC5545; it represents
properties with an unknown name (matching rx
`icalendar-iana-token' or `icalendar-x-name') whose values must
be parsed and preserved but not further interpreted.  Its value
may be set to any type with the `icalendar-valuetypeparam'
parameter."
  ical:value
  :default-type ical:text
  ;; "The default value type is TEXT.  The value type can be set to any
  ;; value type." TODO: should we specify :other-types?  Without it, a
  ;; VALUE param will be required to parse anything other than text,
  ;; but that seems reasonable.
  :child-spec (:allow-others t)
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.8")

(defun ical:read-req-status-info (s)
  "Read a request status value from S.
S should have been previously matched against `icalendar-request-status-info'."
  ;; TODO: this smells like a design flaw.  Silence the byte compiler for now.
  (ignore s)
  (let ((code (match-string 11))
        (desc (match-string 12))
        (exdata (match-string 13)))
    (list code (ical:read-text desc) (when exdata (ical:read-text exdata)))))

(defun ical:print-req-status-info (rsi)
  "Serialize request status info value RSI to a string."
  (let ((code (car rsi))
        (desc (cadr rsi))
        (exdata (caddr rsi)))
    (if exdata
        (format "%s;%s;%s" code (ical:print-text desc) (ical:print-text exdata))
      (format "%s;%s" code (ical:print-text desc)))))

(defun ical:req-status-info-p (val)
  "Return non-nil if VAL is an `icalendar-request-status-info' value."
  (and (listp val)
       (length= val 3)
       (stringp (car val))
       (stringp (cadr val))
       (cl-typep (caddr val) '(or string null))))

(ical:define-type ical:req-status-info nil
  "Type for REQUEST-STATUS property values.

When read, a list (CODE DESCRIPTION EXCEPTION).  CODE is a hierarchical
numerical code, represented as a string, with the following meanings:
  1.xx Preliminary success
  2.xx Successful
  3.xx Client Error
  4.xx Scheduling Error
DESCRIPTION is a longer description of the request status, also a string.
EXCEPTION (which may be nil) is textual data describing an error.

When printed, the three elements are separated by semicolons, like
  CODE;DESCRIPTION;EXCEPTION
or
  CODE;DESCRIPTION
if EXCEPTION is nil.

This is not a type defined by RFC5545; it is defined here to
facilitate parsing the `icalendar-request-status' property."
  '(satisfies ical:req-status-info-p)
  (seq
   ;; statcode: hierarchical status code
   (group-n 11
     (seq (one-or-more digit)
          (** 1 2 (seq ?. (one-or-more digit)))))
   ?\;
   ;; statdesc: status description
   (group-n 12 ical:text)
   ;; exdata: exception data
   (zero-or-one (seq ?\; (group-n 13 ical:text))))
  :reader ical:read-req-status-info
  :printer ical:print-req-status-info
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.8.3")

(ical:define-property ical:request-status "REQUEST-STATUS"
  "Request status"
  ical:req-status-info
  :child-spec (:zero-or-one (ical:languageparam)
               :zero-or-more (ical:otherparam))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.8.8.3")


;;; Section 3.6: Calendar Components

(defvar ical:component-types nil ;; populated by ical:define-component
  "Alist mapping printed component names to type symbols.")

(defun ical:parse-component (limit)
  "Parse an iCalendar component from point up to LIMIT.
Point should be at the start of the component, i.e., at the start
of a line that looks like \"BEGIN:[COMPONENT-NAME]\".  After parsing,
point is at the beginning of the next line following the component
\(or end of the buffer).  Returns a syntax node representing the component."
  (let ((begin-pos nil)
        (body-begin-pos nil)
        (end-pos nil)
        (body-end-pos nil)
        (begin-regex (rx line-start "BEGIN:" (group-n 2 ical:name) line-end)))

    (unless (re-search-forward begin-regex limit t)
      (ical:signal-parse-error "Not at start of a component"))

    (setq begin-pos (match-beginning 0)
          body-begin-pos (1+ (match-end 0))) ; start of next line

    (let* ((component-name (match-string 2))
           (known-type (alist-get (upcase component-name)
                                  ical:component-types
                                  nil nil #'equal))
           (component-type (or known-type 'ical:other-component))
           child children)

      ;; Find end of component:
      (save-excursion
        (if (re-search-forward (concat "^END:" component-name "$") limit t)
            (setq end-pos (match-end 0)
                  body-end-pos (1- (match-beginning 0))) ; end of prev. line
          (ical:signal-parse-error
           (format  "Matching 'END:%s' not found between %d and %d"
                    component-name begin-pos limit)
           :restart-at (1+ limit))))

      (while (not (bolp))
        (forward-char))

      ;; Parse the properties and subcomponents of this component:
      (while (<= (point) body-end-pos)
        (condition-case err
            (setq child (ical:parse-property-or-component end-pos))
          (ical:parse-error
           (ical:handle-parse-error err)
           (setq child nil)))
        (when child (push child children)))

      ;; Set point up for the next parser:
      (goto-char end-pos)
      (while (and (< (point) (point-max)) (not (bolp)))
        (forward-char))

      ;; Return the syntax node for the component:
      (when children
        (ical:make-ast-node component-type
                            (list
                             :original-name
                             (when (eq component-type 'ical:other-component)
                               component-name)
                             :buffer (current-buffer)
                             :begin begin-pos
                             :end end-pos
                             :value-begin body-begin-pos
                             :value-end body-end-pos)
                            (nreverse children))))))

(defun ical:parse-property-or-component (limit)
  "Parse a component or a property at point, up to LIMIT.
Point should be at the beginning of a line which begins a
component or contains a property."
  (cond ((looking-at-p (rx line-start "BEGIN:" ical:name line-end))
         (ical:parse-component limit))
        ((looking-at-p (rx line-start ical:name))
         (ical:parse-property (line-end-position)))
        (t (ical:signal-parse-error
            "Not at start of property or component"
            :restart-at ; find start of next content line:
            (save-excursion
              (if (re-search-forward (rx line-start ical:name) nil t)
                  (match-beginning 0)
                (point-max)))))))

(defun ical:print-component-node (node)
  "Serialize a component syntax node NODE to a string."
  (let* ((type (ical:ast-node-type node))
         (name (or (ical:ast-node-meta-get :original-name node)
                   (car (rassq type ical:component-types))))
         (children (ical:ast-node-children node))
         body)

    (unless name
      (ical:signal-print-error
       (format "Unknown component name for type `%s'" type)
       :node node))

    (dolist (child children)
      (condition-case err
          (setq body
                (concat body (ical:print-property-or-component child)))
        (ical:print-error
         (if (ical:ast-node-required-child-p child node)
             (ical:signal-print-error
              (format
               "Unable to print required `%s' %s in `%s' component.  Error was:\n%s"
               (ical:ast-node-type child)
               (if (ical:component-node-p child) "subcomponent" "property")
               (ical:ast-node-type node)
               (plist-get (cdr err) :message))
              :node node)
           (ical:handle-print-error err)))))
    (concat
     (format "BEGIN:%s\n" name)
     body
     (format "END:%s\n" name))))

(defun ical:print-property-or-component (node)
  "Serialize a property or component node NODE to a string."
  (cond ((ical:property-node-p node)
         (ical:print-property-node node))
        ((ical:component-node-p node)
         (ical:print-component-node node))
        (t (ical:signal-print-error "Not a component or property node"
                                    :node node))))

(ical:define-component ical:vevent "VEVENT"
  "Represents an event.

This component contains properties which describe an event, such
as its start and end time (`icalendar-dtstart' and
`icalendar-dtend') and a summary (`icalendar-summary') and
description (`icalendar-description').  It may also contain
`icalendar-valarm' components as subcomponents which describe
reminder notifications related to the event.  Event components can
only be direct children of an `icalendar-vcalendar'; they cannot
be subcomponents of any other component."
  :child-spec (:one (ical:dtstamp ical:uid)
               :zero-or-one (ical:dtstart
                             ;; TODO: dtstart required if METHOD not present
                             ;; in parent calendar
                             ical:class
                             ical:created
                             ical:description
                             ical:dtend
                             ical:duration
                             ical:geo
                             ical:last-modified
                             ical:location
                             ical:organizer
                             ical:priority
                             ical:sequence
                             ical:status
                             ical:summary
                             ical:transp
                             ical:url
                             ical:recurrence-id
                             ical:rrule)
               :zero-or-more (ical:attach
                              ical:attendee
                              ical:categories
                              ical:comment
                              ical:contact
                              ical:exdate
                              ical:request-status
                              ical:related-to
                              ical:resources
                              ical:rdate
                              ical:other-property
                              ical:valarm))
  :other-validator ical:vevent-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.6.1")

(defun ical:rrule-validator (node)
  "Validate that NODE has the properties required by a recurrence rule.

NODE should represent an iCalendar component.  When NODE has an
`icalendar-rrule' property, this function validates that its
`icalendar-dtstart', `icalendar-rdate', and `icalendar-exdate'
properties satisfy the requirements imposed by this rule.

This function is called by the additional validator functions for
component nodes (e.g. `icalendar-vevent-validator'); it is not normally
necessary to call it directly."
  (let* ((rrule (ical:ast-node-first-child-of 'ical:rrule node))
         (recval (when rrule (ical:ast-node-value rrule)))
         (dtstart (ical:ast-node-first-child-of 'ical:dtstart node))
         (start (when dtstart (ical:ast-node-value dtstart)))
         (rdates (ical:ast-node-children-of 'ical:rdate node))
         (included (when rdates
                     (mapcar #'ical:ast-node-value
                             (apply #'append
                                    (mapcar #'ical:ast-node-value rdates))))))
    (when rrule
      (unless dtstart
        (ical:signal-validation-error
         "An `icalendar-rrule' requires an `icalendar-dtstart' property"
         :node node))
      (when included
        ;; ""RDATE" in this usage [i.e., in STANDARD and DAYLIGHT
        ;; subcomponents] MUST be specified as a date with local time
        ;; value, relative to the UTC offset specified in the
        ;; "TZOFFSETFROM" property."
        (when (and (memq (ical:ast-node-type node) '(ical:standard ical:daylight)))
          (unless (ical:list-of-p included 'ical:date-time)
            (ical:signal-validation-error
             (format
              (concat "`icalendar-rdate' values must be `icalendar-date-time' "
                      "values in %s components")
              (ical:ast-node-type node))
             :node node))
          (when (seq-some #'decoded-time-zone included)
            (ical:signal-validation-error
             (format
              (concat "`icalendar-rdate' values must be in local (\"floating\")"
                      "time in %s components")
              (ical:ast-node-type node))
             :node node))))

      (let* ((freq (car (alist-get 'FREQ recval)))
             (until (car (alist-get 'UNTIL recval))))
        (when (eq 'ical:date (ical:ast-node-type start))
          (when (or (memq freq '(HOURLY MINUTELY SECONDLY))
                    (assq 'BYSECOND recval)
                    (assq 'BYMINUTE recval)
                    (assq 'BYHOUR recval))
            (ical:signal-validation-error
             (concat "`icalendar-rrule' must not contain time-based "
                     "rules when `icalendar-dtstart' is a plain date")
             :node node)))
        (when until
          (unless (eq (ical:ast-node-type start)
                      (ical:ast-node-type until))
            (ical:signal-validation-error
             (concat "`icalendar-rrule' UNTIL clause must agree with "
                     "type of `icalendar-dtstart' property")
             :node node))
          (when (eq 'ical:date-time (ical:ast-node-type until))
            (let ((until-zone
                   (decoded-time-zone (ical:ast-node-value until)))
                  (start-zone
                   (decoded-time-zone (ical:ast-node-value start))))
              ;; "If the "DTSTART" property is specified as a date
              ;; with local time, then the UNTIL rule part MUST also
              ;; be specified as a date with local time":
              (when (and (null start-zone) (not (null until-zone)))
                (ical:signal-validation-error
                  (concat "`icalendar-rrule' UNTIL clause must be in "
                          "local time if `icalendar-dtstart' is")
                  :node node))
              ;; "If the "DTSTART" property is specified as a date
              ;; with UTC time or a date with local time and time zone
              ;; reference, then the UNTIL rule part MUST be specified
              ;; as a date with UTC time":
              (when (and (integerp start-zone)
                         (not (ical:date-time-is-utc-p until)))
                (ical:signal-validation-error
                  (concat "`icalendar-rrule' UNTIL clause must be in UTC time "
                          "if `icalendar-dtstart' has a defined time zone")
                  :node node))))
          (when (memq (ical:ast-node-type node) '(ical:standard ical:daylight))
            ;; "In the case of the "STANDARD" and "DAYLIGHT"
            ;; sub-components the UNTIL rule part MUST always be
            ;; specified as a date with UTC time":
            (unless (ical:date-time-is-utc-p until)
              (ical:signal-validation-error
               (concat "`icalendar-rrule' UNTIL clause must be in UTC time in "
                       "`icalendar-standard' and `icalendar-daylight' components")
               :node node))))

        ;; "DTSTART in this usage [i.e., in STANDARD and DAYLIGHT
        ;; subcomponents] MUST be specified as a date with a local
        ;; time value."
        (when (memq (ical:ast-node-type node) '(ical:standard ical:daylight))
          (unless (eq 'ical:date-time (ical:ast-node-type start))
            (ical:signal-validation-error
              (concat "`icalendar-dtstart' must be an `icalendar-date-time' in "
                      "`icalendar-standard' and `icalendar-daylight' components")
              :node node))

          (when (decoded-time-zone (ical:ast-node-value start))
            (ical:signal-validation-error
             (concat "`icalendar-dtstart' must be in local (\"floating\") time in "
                     "`icalendar-standard' and `icalendar-daylight' components")
             :node node)))))

    ;; Success:
    node))

(defun ical:vevent-validator (node)
  "Additional validator for an `icalendar-vevent' NODE.
Checks that NODE has does not have both `icalendar-duration' and
`icalendar-dtend' properties, and calls `icalendar-rrule-validator'.

This function is called by `icalendar-ast-node-valid-p' for
VEVENT nodes; it is not normally necessary to call it directly."
  (let* ((duration (ical:ast-node-first-child-of 'ical:duration node))
         (dur-value (when duration (ical:ast-node-value
                                     (ical:ast-node-value duration))))
         (dtend (ical:ast-node-first-child-of 'ical:dtend node))
         (dtstart (ical:ast-node-first-child-of 'ical:dtstart node)))
    (when (and dtend duration)
      (ical:signal-validation-error
       (concat "`icalendar-dtend' and `icalendar-duration' properties must "
               "not appear in the same `icalendar-vevent'")
       :node node))
    ;; don't allow time-based durations with dates
    ;; TODO: check that the standard disallows this...?
    (when (and dtstart duration
               (eq 'ical:date (ical:ast-node-type dtstart))
               (or (not (integerp dur-value))
                   (decoded-time-hour dur-value)
                   (decoded-time-minute dur-value)
                   (decoded-time-second dur-value)))
      (ical:signal-validation-error
       (concat "Event with `icalendar-date' value in `icalendar-dtstart' "
               "cannot have time units in `icalendar-duration'")
       :node node))

  (ical:rrule-validator node)
  ;; success:
  node))

(ical:define-component ical:vtodo "VTODO"
  "Represents a To-Do item or task.

This component contains properties which describe a to-do item or
task, such as its due date (`icalendar-due') and a summary
(`icalendar-summary') and description (`icalendar-description').
It may also contain `icalendar-valarm' components as
subcomponents which describe reminder notifications related to
the task.  To-do components can only be direct children of an
`icalendar-vcalendar'; they cannot be subcomponents of any other
component."
  :child-spec (:one (ical:dtstamp ical:uid)
               :zero-or-one (ical:class
                             ical:completed
                             ical:created
                             ical:description
                             ical:dtstart
                             ical:due
                             ical:duration
                             ical:geo
                             ical:last-modified
                             ical:location
                             ical:organizer
                             ical:percent-complete
                             ical:priority
                             ical:recurrence-id
                             ical:sequence
                             ical:status
                             ical:summary
                             ical:url
                             ical:rrule)
               :zero-or-more (ical:attach
                              ical:attendee
                              ical:categories
                              ical:comment
                              ical:contact
                              ical:exdate
                              ical:request-status
                              ical:related-to
                              ical:resources
                              ical:rdate
                              ical:other-property
                              ical:valarm))
  :other-validator ical:vtodo-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.6.2")

(defun ical:vtodo-validator (node)
  "Additional validator for an `icalendar-vtodo' NODE.
Checks that NODE has conformant `icalendar-due',
`icalendar-duration', and `icalendar-dtstart' properties, and calls
`icalendar-rrule-validator'.

This function is called by `icalendar-ast-node-valid-p' for
VTODO nodes; it is not normally necessary to call it directly."
  (let* ((due (ical:ast-node-first-child-of 'ical:due node))
         (duration (ical:ast-node-first-child-of 'ical:duration node))
         (dtstart (ical:ast-node-first-child-of 'ical:dtstart node)))
    (when (and due duration)
      (ical:signal-validation-error
       (concat "`icalendar-due' and `icalendar-duration' properties "
               "must not appear in the same `icalendar-vtodo'")
       :node node))
    (when (and duration (not dtstart))
      (ical:signal-validation-error
       (concat "`icalendar-duration' requires `icalendar-dtstart' "
               "property in the same `icalendar-vtodo'")
       :node node)))
  (ical:rrule-validator node)
  ;; success:
  node)

(ical:define-component ical:vjournal "VJOURNAL"
  "Represents a journal entry.

This component contains properties which describe a journal
entry, which might be any longer-form data (e.g., meeting notes,
a diary entry, or information needed to complete a task).  It can
be associated with an `icalendar-vevent' or `icalendar-vtodo' via
the `icalendar-related-to' property.  A journal entry does not
take up time in a calendar, and plays no role in searches for
free or busy time.  Journal components can only be direct children
of `icalendar-vcalendar'; they cannot be subcomponents of any
other component."
  :child-spec (:one (ical:dtstamp ical:uid)
               :zero-or-one (ical:class
                             ical:created
                             ical:dtstart
                             ical:last-modified
                             ical:organizer
                             ical:recurrence-id
                             ical:sequence
                             ical:status
                             ical:summary
                             ical:url
                             ical:rrule)
               :zero-or-more (ical:attach
                              ical:attendee
                              ical:categories
                              ical:comment
                              ical:contact
                              ical:description
                              ical:exdate
                              ical:related-to
                              ical:rdate
                              ical:request-status
                              ical:other-property)
               :other-validator ical:rrule-validator)
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.6.3")

(ical:define-component ical:vfreebusy "VFREEBUSY"
  "Represents a published set of free/busy time blocks, or a request
or response for such blocks.

The free/busy information is represented by the
`icalendar-freebusy' property (which may be given more than once)
and the related `icalendar-fbtype' parameter.  Note that
recurrence properties (`icalendar-rrule', `icalendar-rdate', and
`icalendar-exdate') are NOT permitted in this component.

When used to publish blocks of free/busy time in a user's
schedule, the `icalendar-organizer' property specifies the user.

When used to request free/busy time in a user's schedule, or to
respond to such a request, the `icalendar-attendee' property
specifies the user whose time is being requested, and the
`icalendar-organizer' property specifies the user making the
request.

Free/busy components can only be direct children
of `icalendar-vcalendar'; they cannot be subcomponents of any
other component, and cannot contain subcomponents."
  :child-spec (:one (ical:dtstamp ical:uid)
               :zero-or-one (ical:contact
                             ical:dtstart
                             ical:dtend
                             ical:organizer
                             ical:url)
               :zero-or-more (ical:attendee
                              ical:comment
                              ical:freebusy
                              ical:request-status
                              ical:other-property))
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.6.4")

;; TODO: RFC7808 defines additional properties that are relevant here:
;; https://www.rfc-editor.org/rfc/rfc7808.html#section-7
(ical:define-component ical:vtimezone "VTIMEZONE"
  "Represents a time zone.

A time zone is identified by an `icalendar-tzid' property, which
is required in this component.  Times in other calendar components
can be specified in local time in this time zone with the
`icalendar-tzidparam' parameter.  An `icalendar-vcalendar' object
must contain exactly one `icalendar-vtimezone' component for each
unique time zone identifier used in the calendar.

Besides the time zone identifier, a time zone component must
contain at least one `icalendar-standard' or `icalendar-daylight'
subcomponent, which describe the observance of standard or
daylight time in the time zone, including the dates of the
observance and the relevant offsets from UTC time."
  :child-spec (:one (ical:tzid)
               :zero-or-one (ical:last-modified
                             ical:tzurl)
               :zero-or-more (ical:standard
                              ical:daylight
                              ical:other-property))
  :other-validator ical:vtimezone-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.6.5")

(defun ical:vtimezone-validator (node)
  "Additional validator for an `icalendar-vtimezone' NODE.
Checks that NODE has at least one `icalendar-standard' or
`icalendar-daylight' child.

This function is called by `icalendar-ast-node-valid-p' for
VTIMEZONE nodes; it is not normally necessary to call it directly."
  (let ((child-counts (ical:count-children-by-type node)))
    (when (and (= 0 (alist-get 'ical:standard child-counts 0))
               (= 0 (alist-get 'ical:daylight child-counts 0)))
      (ical:signal-validation-error
       (concat "`icalendar-vtimezone' must have at least one "
               "`icalendar-standard' or `icalendar-daylight' child")
       :node node)))

  ;; success:
  node)

(ical:define-component ical:standard "STANDARD"
  "Represents a Standard Time observance in a time zone.

The observance has a start time, specified by an
`icalendar-dtstart' property, which is required in this component
and must be in *local* time format.  The observance may have a
recurring onset (e.g. each year on a particular day or date)
described by the `icalendar-rrule' and `icalendar-rdate'
properties.  An end date for the observance, if there is one, must
be specified in the UNTIL clause of the `icalendar-rrule' in UTC
time.

The offset from UTC time when the observance begins is specified
in the `icalendar-tzoffsetfrom' property, which is required.  The
offset from UTC time while the observance is in effect is
specified by the `icalendar-tzoffsetto' property, which is also
required.  A common identifier for the time zone observance can be
specified in the `icalendar-tzname' property.  Other explanatory
comments can be provided in `icalendar-comment'.

This component must be a direct child of an `icalendar-vtimezone'
component and cannot contain other subcomponents."
  :child-spec (:one (ical:dtstart
                     ical:tzoffsetto
                     ical:tzoffsetfrom)
               :zero-or-one (ical:rrule)
               :zero-or-more (ical:comment
                              ical:rdate
                              ical:tzname
                              ical:other-property)
               :other-validator ical:rrule-validator)
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.6.5")

(ical:define-component ical:daylight "DAYLIGHT"
  "Represents a Daylight Savings Time observance in a time zone.

The observance has a start time, specified by an
`icalendar-dtstart' property, which is required in this component
and must be in *local* time format.  The observance may have a
recurring onset (e.g. each year on a particular day or date)
described by the `icalendar-rrule' and `icalendar-rdate'
properties.  An end date for the observance, if there is one, must
be specified in the UNTIL clause of the `icalendar-rrule' in UTC
time.

The offset from UTC time when the observance begins is specified
in the `icalendar-tzoffsetfrom' property, which is required.  The
offset from UTC time while the observance is in effect is
specified by the `icalendar-tzoffsetto' property, which is also
required.  A common identifier for the time zone observance can be
specified in the `icalendar-tzname' property.  Other
explanatory comments can be provided in `icalendar-comment'.

This component must be a direct child of an `icalendar-vtimezone'
component and cannot contain other subcomponents."
  :child-spec (:one (ical:dtstart
                     ical:tzoffsetto
                     ical:tzoffsetfrom)
               :zero-or-one (ical:rrule)
               :zero-or-more (ical:comment
                              ical:rdate
                              ical:tzname
                              ical:other-property)
               :other-validator ical:rrule-validator)
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.6.5")

(ical:define-component ical:valarm "VALARM"
  "Represents an alarm.

An alarm is a notification or reminder for an event or task.  The
type of notification is determined by this component's
`icalendar-action' property: it may be an AUDIO, DISPLAY, or
EMAIL notification.
If it is an audio alarm, it can include an
`icalendar-attach' property specifying the audio to be rendered.
If it is a DISPLAY alarm, it must include an `icalendar-description'
property containing the text to be displayed.
If it is an EMAIL alarm, it must include both an
`icalendar-summary' and an `icalendar-description', which specify
the subject and body of the email, and one or more
`icalendar-attendee' properties, which specify the recipients.

The required `icalendar-trigger' property specifies when the
alarm triggers.  If the alarm repeats, then `icalendar-duration'
and `icalendar-repeat' properties are also both required.

This component must occur as a direct child of an
`icalendar-vevent' or `icalendar-vtodo' component, and cannot
contain any subcomponents."
  :child-spec (:one (ical:action ical:trigger)
               :zero-or-one (ical:duration ical:repeat)
               :zero-or-more (ical:summary
                              ical:description
                              ical:attendee
                              ical:attach
                              ical:other-property))
  :other-validator ical:valarm-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.6.6")

(defun ical:valarm-validator (node)
  "Additional validator function for `icalendar-valarm' components.
Checks that NODE has the right properties corresponding to its
`icalendar-action' type, e.g., that an EMAIL alarm has a
subject (`icalendar-summary') and recipients (`icalendar-attendee').

This function is called by `icalendar-ast-node-valid-p' for
VALARM nodes; it is not normally necessary to call it directly."
  (let* ((action (ical:ast-node-first-child-of 'ical:action node))
         (duration (ical:ast-node-first-child-of 'ical:duration node))
         (repeat (ical:ast-node-first-child-of 'ical:repeat node))
         (child-counts (ical:count-children-by-type node)))

    (when (and duration (not repeat))
      (ical:signal-validation-error
       (concat "`icalendar-valarm' node with `icalendar-duration' "
               "must also have `icalendar-repeat' property")
       :node node))

    (when (and repeat (not duration))
      (ical:signal-validation-error
       (concat "`icalendar-valarm' node with `icalendar-repeat' "
               "must also have `icalendar-duration' property")
       :node node))

    (let ((action-str (upcase (ical:text-to-string
                               (ical:ast-node-value action)))))
      (cond ((equal "AUDIO" action-str)
             (unless (<= (alist-get 'ical:attach child-counts 0) 1)
               (ical:signal-validation-error
                (concat "AUDIO `icalendar-valarm' may not have "
                        "more than one `icalendar-attach'")
                :node node))
             node)

            ((equal "DISPLAY" action-str)
             (unless (= 1 (alist-get 'ical:description child-counts 0))
               (ical:signal-validation-error
                (concat "DISPLAY `icalendar-valarm' must have "
                        "exactly one `icalendar-description'")
                :node node))
             node)

            ((equal "EMAIL" action-str)
             (unless (= 1 (alist-get 'ical:summary child-counts 0))
               (ical:signal-validation-error
                (concat "EMAIL `icalendar-valarm' must have "
                        "exactly one `icalendar-summary'")
                :node node))
             (unless (= 1 (alist-get 'ical:description child-counts 0))
               (ical:signal-validation-error
                (concat "EMAIL `icalendar-valarm' must have "
                        "exactly one `icalendar-description'")
                :node node))
             (unless (<= 1 (alist-get 'ical:attendee child-counts 0))
               (ical:signal-validation-error
                (concat "EMAIL `icalendar-valarm' must have "
                        "at least one `icalendar-attendee'")
                :node node))
             node)

            (t
             ;; "Applications MUST ignore alarms with x-name and iana-token
             ;; values they don't recognize." So this is not a validation-error:
             (ical:warn
              (format "Unknown ACTION value in VALARM: %s" action-str)
              :buffer (ical:ast-node-meta-get node :buffer)
              :position (ical:ast-node-meta-get node :value-begin))
             node)))))

(ical:define-component ical:other-component nil
  "Component type for unrecognized component names.

This component type corresponds to the IANA and X-name components
allowed by RFC5545 sec. 3.6; it represents components with an
unknown name (matching rx `icalendar-iana-token' or
`icalendar-x-name') which must be parsed and preserved but not
further interpreted."
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.6")

;; Technically VCALENDAR is not a "component", but for the
;; purposes of parsing and syntax highlighting, it looks just like
;; one, so we define it as such here.
;; (If this becomes a problem, modify `ical:component-node-p'
;; to return nil for VCALENDAR components.)
(ical:define-component ical:vcalendar "VCALENDAR"
  "Calendar Object.

This is the top-level data structure defined by RFC5545.  A
VCALENDAR must contain the calendar properties `icalendar-prodid'
and `icalendar-version', and may contain the calendar properties
`icalendar-method' and `icalendar-calscale'.

It must also contain at least one VEVENT, VTODO, VJOURNAL,
VFREEBUSY, or other component, and for every unique
`icalendar-tzidparam' value appearing in a property within these
components, the calendar object must contain an
`icalendar-vtimezone' defining a time zone with that TZID."
  :child-spec (:one (ical:prodid ical:version)
               :zero-or-one (ical:calscale ical:method)
               :zero-or-more (ical:other-property
                              ical:vevent
                              ical:vtodo
                              ical:vjournal
                              ical:vfreebusy
                              ical:vtimezone
                              ical:other-component))
  :other-validator ical:vcalendar-validator
  :link "https://www.rfc-editor.org/rfc/rfc5545#section-3.4")

(defun ical:all-tzidparams-in (node)
  "Recursively find all `icalendar-tzidparam' values in NODE and its children."
  (cond ((ical:tzid-param-p node)
         (list (ical:ast-node-value node)))
        ((ical:param-node-p node)
         nil)
        (t ;; TODO: could prune search here when properties don't allow tzidparam
         (seq-uniq (mapcan #'ical:all-tzidparams-in
                           (ical:ast-node-children node))))))

(defun ical:vcalendar-validator (node)
  "Additional validator for `icalendar-vcalendar' NODE.

Checks that NODE has at least one component child and that all of the
`ical-tzidparam' values appearing in subcomponents have a corresponding
`icalendar-vtimezone' definition.

This function is called by `icalendar-ast-node-valid-p' for
VCALENDAR nodes; it is not normally necessary to call it directly."
  (let* ((children (ical:ast-node-children node))
         (comp-children (seq-filter #'ical:component-node-p children))
         (tz-children (seq-filter #'ical:vtimezone-component-p children))
         (defined-tzs
          (mapcar
           (lambda (tz)
             ;; ensure vtimezone component has a TZID property and
             ;; extract its string value:
             (when (ical:ast-node-valid-p tz)
               (ical:with-component tz ((ical:tzid :value-node tzid-text))
                 (ical:text-to-string tzid-text))))
           tz-children))
         (appearing-tzids (ical:all-tzidparams-in node)))
    (unless comp-children
      (ical:signal-validation-error
       "`icalendar-vcalendar' must contain at least one component"
       :node node))

    (let ((seen nil))
      (dolist (tzid appearing-tzids)
        (unless (member tzid seen)
          (unless (member tzid defined-tzs)
            (ical:signal-validation-error
             (format "No `icalendar-vtimezone' with TZID '%s' in calendar" tzid)
             :node node)))
        (push tzid seen)))

    ;; success:
    node))

(defun ical:contains-vcalendar-p (&optional buffer)
  "Determine whether BUFFER contains \"BEGIN:VCALENDAR\".

If so, then BUFFER is a candidate for parsing with, e.g.,
`icalendar-parse-calendar'.  BUFFER defaults to the current
buffer.  Returns the position where parsing should start, or nil."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^BEGIN:VCALENDAR" nil t)
        (beginning-of-line)
        (point)))))

;; `icalendar-parse-component' is sufficient to parse all the syntax in
;; a calendar, but a calendar-level parsing function is needed to add
;; support for time zones.  This function ensures that every
;; `icalendar-tzidparam' in the calendar has a corresponding
;; `icalendar-vtimezone' component, and modifies the zone information of
;; the parsed date-time according to the offset in that time zone.
(defun ical:parse-calendar (limit)
  "Parse an `icalendar-vcalendar' object from point up to LIMIT.
Point should be at the start of the calendar object, i.e., at the start
of a line that looks like \"BEGIN:VCALENDAR\".  After parsing, point is
at the beginning of the next line following the calendar (or end of the
buffer).  Returns a syntax node representing the calendar."
  (require 'icalendar-recur) ; for icr:tz-set-zones-in; avoids circular require
  (declare-function icr:tz-set-zones-in "icalendar-recur")
  (unless (looking-at-p "^BEGIN:VCALENDAR")
    (ical:signal-parse-error "Not at start of VCALENDAR"))
  (let ((cal-node (ical:parse-component limit)))
      ;(when (ical:ast-node-valid-p cal-node t)
      (ical:with-component cal-node
          ((ical:vtimezone :all tzs))
        ;; After parsing the whole calendar, set the zone and dst slots
        ;; in all date-times which are relative to a time zone defined
        ;; in the calendar:
        ;; (TODO: if this proves too slow in general, we could instead
        ;; do it lazily when individual components are queried somehow.
        ;; But I'm not convinced that will actually save any time, because
        ;; if we're parsing, we're probably already in the middle of a
        ;; function that will immediately query all these times, e.g.
        ;; `diary-icalendar-import-buffer'.)
        (dolist (comp (ical:ast-node-children cal-node))
          (unless (ical:vtimezone-component-p comp)
            (icr:tz-set-zones-in tzs comp))));)
      cal-node))

;; TODO: should we do anything to *create* VTIMEZONE nodes in VCALENDAR
;; when they're required but don't exist?
(defun ical:print-calendar-node (vcalendar)
  "Serialize an `icalendar-vcalendar' VCALENDAR to a string.

If VCALENDAR is not a valid `icalendar-vcalendar', an
`icalendar-validation-error' will be signaled.  Any errors that arise
during printing will be logged in the buffer returned by
`icalendar-error-buffer'."
  (when (ical:ast-node-valid-p vcalendar t)
    (condition-case err
        (ical:print-component-node vcalendar)
      (ical:print-error
       (ical:handle-print-error err)))))


;;; High-level parsing and printing functions.
(defun ical:parse (&optional buffer)
  "Parse an `icalendar-vcalendar' object in BUFFER (default: current buffer).

An unfolded copy of BUFFER (see `icalendar-unfolded-buffer-from-buffer')
will first be obtained if necessary.  Parsing will begin at the first
occurrence of \"BEGIN:VCALENDAR\" in the unfolded buffer.

The buffer may be tidied up by user functions before parsing begins; see
`icalendar-pre-unfolding-hook' and `icalendar-pre-parsing-hook'.

If parsing is successful, the VCALENDAR object is returned.  Otherwise,
nil is returned, a warning is issued, and errors are logged in the
buffer returned by `icalendar-error-buffer'."
  (let* ((buf (or buffer (current-buffer)))
         (unfolded (cond ((ical:unfolded-p buf) buf)
                         ((buffer-file-name buf)
                          (ical:unfolded-buffer-from-file (buffer-file-name buf)))
                         (t (ical:unfolded-buffer-from-buffer buf)))))
    (ical:init-error-buffer)
    (with-current-buffer unfolded
      (run-hooks 'ical:pre-parsing-hook)
      (let ((cal-start (ical:contains-vcalendar-p))
            vcalendar)
        (unless cal-start
          (ical:signal-parse-error "Buffer does not contain \"BEGIN:VCALENDAR\""))
        (save-excursion
          (goto-char cal-start)
          (ical:condition-case err
              (setq vcalendar (ical:parse-calendar (point-max)))
            (ical:parse-error
             (ical:handle-parse-error err)
             (warn "Errors while parsing %s; see buffer %s"
                   buffer (buffer-name (ical:error-buffer))))))
        vcalendar))))

;; TODO: The function `ical:print' below is not really useful yet.
;; Feels like it's needed for completeness but interface needs more thought.
;; Should this instead be a generic function that prints any
;; kind of node at point? at a given marker?
;; What about the coding system?  If we want to use this function to print
;; iCalendar data to stdout, need to set up coding system correctly and
;; perform line folding.
;; Etc.
;;
;; (defun ical:print (vcalendar &optional buffer pos)
;;   "Insert VCALENDAR as a string at position POS in BUFFER.
;;
;; VCALENDAR should be an `icalendar-vcalendar'.  BUFFER defaults to the
;; current buffer and POS defaults to point.
;;
;; If printing is successful, VCALENDAR is returned.  Otherwise, nil is
;; returned, a warning is issued, and errors are logged in the buffer
;; returned by `icalendar-error-buffer'."
;;   (with-current-buffer (or buffer (current-buffer))
;;     (when pos (goto-char pos))
;;     (condition-case err
;;         (insert (ical:print-calendar-node vcalendar))
;;       (ical:print-error
;;        (ical:handle-print-error err)
;;        (setq vcalendar nil) ; return
;;        (warn "Errors while printing; see buffer %s"
;;              (buffer-name (ical:error-buffer)))))
;;     vcalendar))


;;; Pre-parsing cleanup
;;
;; The following functions are based on observed syntax errors in
;; real-world data and can help clean up such data before parsing.
;; More functions can be added here based on user feedback.
(defcustom ical:pre-parsing-hook nil
  "Hook run by `icalendar-parse' before parsing iCalendar data.

If you routinely receive iCalendar data in an incorrect format, you can
add functions to this hook which clean up that data before parsing is
attempted.  The functions in this hook will be run after the iCalendar
data has been \"unfolded\" but before parsing begins.  (If you need to
clean up data before unfolding happens, see
`icalendar-pre-unfolding-hook'.)

Each function should accept zero arguments and should perform its
operation on the entire current buffer."
  :version "31.1"
  :type '(hook)
  :options '(ical:fix-blank-lines
             ical:fix-hyphenated-dates
             ical:fix-missing-mailtos))

(defun ical:fix-blank-lines ()
  "Remove blank lines.
This function is intended to be used from `icalendar-pre-parsing-hook',
which see."
  (goto-char (point-min))
  (while (re-search-forward (rx "\n" (zero-or-more space) line-end)
                            nil t)
    (replace-match "" nil nil)))

(defun ical:fix-hyphenated-dates ()
  "Correct dates in \"YYYY-MM-DD...\" format to \"YYYYMMDD...\" format.
This function is intended to be used from `icalendar-pre-parsing-hook',
which see."
  (goto-char (point-min))
  (while (re-search-forward
          (rx line-start
              (or "COMPLETED" "DTEND" "DUE" "DTSTART" "RECURRENCE-ID"
                  "EXDATE" "RDATE" "CREATED" "DTSTAMP" "LAST-MODIFIED")
              (zero-or-more ical:other-param-safe)
              ":")
          nil t)
    (unless (looking-at-p (rx (or ical:date ical:date-time)))
      (while (re-search-forward ; exdate, rdate allow lists
              (rx (group-n 1 (= 4 digit))
                  "-"
                  (group-n 2 (= 2 digit))
                  "-"
                  (group-n 3 (= 2 digit)))
              (line-end-position) t)
      (replace-match "\\1\\2\\3" nil nil)))))

(defun ical:fix-missing-mailtos ()
  "Insert \"mailto:\" when it is missing before email addresses.
This function is intended to be used from `icalendar-pre-parsing-hook',
which see."
  ;; fix property values in properties that require an address:
  (goto-char (point-min))
  (while (re-search-forward
          (rx line-start (or "ORGANIZER" "ATTENDEE")
              (zero-or-more ical:other-param-safe) ":")
          nil t)
   (unless (looking-at-p (rx ical:cal-address))
     (when (looking-at
            (rx
             ;; match local part of mail address: all the characters
             ;; allowed after a URI scheme, *except*
             ;; ?@ (so we can match that after) and
             ;; ?: (in case we're looking at a non-"mailto:" scheme)
             (group-n 1
               (one-or-more
                (any "A-Za-z0-9" ?- ?. ?_ ?~ ?/ ?? ?# ?\[ ?\] ?! ?$ ?& ?'
                     ?\( ?\) ?* ?+ ?, ?\; ?= ?%)))
             "@"))
       (when (or (< (length (match-string 0)) 7)
                 (not (equal "mailto:"
                             (substring (downcase (match-string 0)) 0 7))))
         (replace-match "mailto:\\1" nil nil nil 1)))))

  ;; fix parameter values in parameters that require an address:
  (goto-char (point-min))
  (while (re-search-forward
          (rx line-start ical:name
              (zero-or-more icalendar-other-param-safe)
              ";"
              (or "DELEGATED-FROM" "DELEGATED-TO" "MEMBER" "SENT-BY")
              "=")
          nil t)
    (unless (looking-at-p (rx ical:cal-address))
      (while ; DELEGATED* params accept lists
          (looking-at
           (rx
            ?\" ; values of these params must always be quoted
            (group-n 1 ; matches local part of mail address as above
              (one-or-more
               (any "A-Za-z0-9" ?- ?. ?_ ?~ ?/ ?? ?# ?\[ ?\] ?! ?$ ?& ?'
                    ?\( ?\) ?* ?+ ?, ?= ?%)))
            "@"
            (zero-or-more (not ?\"))
            ?\"
            (zero-or-one ",")))
        (when (or (< (length (match-string 1)) 7)
                  (not (equal "mailto:"
                              (substring (downcase (match-string 1)) 0 7))))
          (replace-match "mailto:\\1" nil nil nil 1))
        (goto-char (match-end 0))))))


;;; Caching and indexing parse trees
;;
;; The following functions provide a simple in-memory cache and index
;; for faster access to parsed iCalendar data by date, UID, and other
;; fields of interest.  The index and parse tree are stored in a
;; buffer-local variable of the parsed buffer and not recomputed if the
;; buffer hasn't changed.  Most users of the library should just call
;; `icalendar-parse-and-index' to get both the parse tree and a
;; reference to the index, and get objects of interest from them
;; with `icalendar-index-get'.
(defun ical:make-index ()
  "Create an empty index of iCalendar components."
  (list :bydate (make-hash-table :test #'equal) ;; date => list of components
        :byuid (make-hash-table :test #'equal)  ;; UID => component
        :bytzid (make-hash-table :test #'equal) ;; tzid => vtimezone
        :recurring (list))) ;; list of components

(defun ical:index-insert-tz (index vtimezone)
  "Insert VTIMEZONE into INDEX."
  (ical:with-component vtimezone
      ((ical:tzid :value tzid))
    (let ((tzid-index (plist-get index :bytzid)))
      (puthash tzid vtimezone tzid-index)
      ;; Update and return the index:
      (plist-put index :bytzid tzid-index))))


(defun ical:index-insert (index component)
  "Insert COMPONENT into INDEX."
  (require 'icalendar-recur) ; avoid circular imports
  (require 'icalendar-utils) ;
  (declare-function icr:recurrences-to-count "icalendar-recur")
  (declare-function ical:date/time-to-local "icalendar-utils")
  (declare-function ical:date/time-to-date "icalendar-utils")
  (declare-function ical:dates-until "icalendar-utils")

  (ical:with-component component
    ((ical:dtstart :first dtstart-node :value dtstart)
     (ical:dtend :first dtend-node :value dtend)
     (ical:due :value due)
     (ical:duration :value duration)
     (ical:rrule :value recur-value)
     (ical:rdate :all rdate-nodes)
     (ical:exdate :all exdate-nodes)
     (ical:uid :value uid))
    (let ((date-index (plist-get index :bydate))
          (uid-index (plist-get index :byuid))
          (tzid-index (plist-get index :bytzid))
          (recurring (plist-get index :recurring))
          (rdates
           (mapcar #'ical:ast-node-value
                   (apply #'append (mapcar #'ical:ast-node-value rdate-nodes))))
          (exdates
           (mapcar #'ical:ast-node-value
                   (apply #'append (mapcar #'ical:ast-node-value exdate-nodes))))
          dates)
      ;; Everything with a UID goes into the uid-index:
      (when uid
        (puthash uid component uid-index))
      ;; For all top-level components, we gather a list of dates on which
      ;; they recur for date-index, or put them in the recurring list:
      (when dtstart
        (cond
         ;; If the component has an RRULE that specifies a fixed number
         ;; of recurrences, compute them now and index them for each date
         ;; in each recurrence:
         ((and recur-value (ical:recur-count recur-value))
          (let* ((tz (gethash (ical:with-param-of dtstart-node 'ical:tzidparam)
                              tzid-index))
                 (recs (cons dtstart (icr:recurrences-to-count component tz))))
            (dolist (rec recs)
              (let ((end-time
                     (when duration (ical:date/time-add-duration rec duration))))
                (setq dates
                      (append dates
                              (if end-time (ical:dates-until rec end-time t)
                                (list (ical:date/time-to-date
                                       (ical:date/time-to-local rec))))))))))
         ;; Same with RDATEs when there's no RRULE:
         ((and rdates (not recur-value))
          (dolist (rec (cons dtstart rdates))
            (unless (or (cl-typep rec 'ical:period) (member rec exdates))
              (let ((end-time
                     (when duration
                       (ical:date/time-add-duration rec duration))))
                (setq dates
                      (append dates
                              (if end-time (ical:dates-until rec end-time t)
                                (list (ical:date/time-to-date
                                       (ical:date/time-to-local rec))))))))
            (when (cl-typep rec 'ical:period)
              (let* ((start (ical:period-start rec))
                     (end (or (ical:period-end rec)
                              (ical:date/time-add-duration
                               start (ical:period-dur-value rec)))))
                (setq dates (append dates (ical:dates-until start end t)))))))
         ;; A non-recurring event also gets an index entry for each date
         ;; until its end time:
         ((not recur-value)
          (let ((end-time
                 (or dtend due
                     (when duration
                       (ical:date/time-add-duration dtstart duration)))))
            (setq dates (if end-time (ical:dates-until dtstart end-time t)
                          (list
                           (ical:date/time-to-date
                            (ical:date/time-to-local dtstart)))))))
         ;; Otherwise, we put off the computation of recurrences until queried:
         (t (push component recurring)))

        (dolist (date (seq-uniq dates))
          (let ((others (gethash date date-index)))
            ;; TODO: wonder if we should normalize, and instead store UIDs
            ;; in the date index, then look them up by UID when queried.
            (puthash date (cons component others) date-index))))

      ;; Return the updated index:
      (setq index (plist-put index :byuid uid-index))
      (setq index (plist-put index :bytzid tzid-index))
      (setq index (plist-put index :bydate date-index))
      (setq index (plist-put index :recurring recurring))
      index)))

(defun ical:index-populate-from-calendar (index vcalendar)
  "Insert all components in VCALENDAR into INDEX."
  (let* ((tzs (ical:ast-node-children-of 'ical:vtimezone vcalendar))
         (vevents (ical:ast-node-children-of 'ical:vevent vcalendar))
         (vjournals (ical:ast-node-children-of 'ical:vjournal vcalendar))
         (vtodos (ical:ast-node-children-of 'ical:vtodo vcalendar))
         ;; TODO: customizable selection? what about valarms?
         (to-index (append vevents vjournals vtodos)))

    ;; First insert the tzs, so that they're available when inserting
    ;; the others by date:
    (dolist (tz tzs)
      (setq index (ical:index-insert-tz index tz)))

    (dolist (component to-index)
      (setq index (ical:index-insert index component)))
    index))

(cl-defun ical:index-get (index &rest args &key date uid tzid)
  "Get an iCalendar component from INDEX by date, UID, or TZID.

INDEX should be a reference to a parse tree index as returned by
`icalendar-parse-and-index', which see.  The index can be queried by:

:uid UID (string, see `icalendar-uid') - returns the component with that
  UID.

:tzid TZID (string, see `icalendar-tzid' and `icalendar-tzidparam') -
  returns the `icalendar-vtimezone' component with that TZID.

:date DT (an `icalendar-date', i.e. a list (M D Y)) - returns a list of
  the components occurring (or recurring) on that date.

Only one keyword argument can be queried at a time."
  (require 'icalendar-recur) ; avoid circular imports
  (require 'icalendar-utils) ;

  (declare-function icr:find-interval "icalendar-recur")
  (declare-function icr:recurrences-in-interval "icalendar-recur")
  (declare-function ical:date/time-in-period-p "icalendar-utils")
  (declare-function ical:date/time<= "icalendar-utils")
  (declare-function ical:date/time< "icalendar-utils")
  (declare-function ical:date/time-add-duration "icalendar-utils")

  (when (length> args 2)
    (error "Only one keyword argument can be queried"))
  (cond (uid (gethash uid (plist-get index :byuid)))
        (tzid (gethash tzid (plist-get index :bytzid)))
        (date
         (let ((computed (gethash date (plist-get index :bydate)))
               (recurring (plist-get index :recurring)))
           (dolist (component recurring)
             (ical:with-component component
                 ((ical:dtstart :first dtstart-node :value dtstart)
                  (ical:rrule :value recur-value)
                  (ical:rdate :all rdate-nodes)
                  (ical:duration :value duration))
               (unless (ical:date/time<= date dtstart)
                 (let* ((tz (ical:with-param-of dtstart-node 'ical:tzidparam nil
                              (gethash value (plist-get index :bytzid))))
                        (int (icr:find-interval date dtstart recur-value tz))
                        (recs (icr:recurrences-in-interval int component tz)))
                   (catch 'found
                     (dolist (rec recs)
                       (let* ((local-rec (ical:date/time-to-local rec))
                              (end
                               (when duration
                                 (ical:date/time-add-duration local-rec duration)))
                              (rec-dates
                               (if end (ical:dates-until local-rec end t)
                                 (list (ical:date/time-to-date local-rec)))))
                         (when (member date rec-dates)
                           (push component computed)
                           (throw 'found nil))))
                     (dolist (node rdate-nodes)
                       ;; normal RDATE recurrences have already been
                       ;; checked above, but we check whether `date'
                       ;; occurs in any RDATE period values here:
                       (when (eq 'ical:period
                                 (ical:value-type-from-params
                                  (ical:ast-node-children node)))
                         (let* ((tz
                                 (ical:with-param-of node 'ical:tzidparam nil
                                   (gethash value (plist-get index :bytzid)))))
                           (ical:with-property node nil
                             (dolist (period values)
                               (when (ical:date/time-in-period-p date period tz)
                                 (push component computed)
                                 (throw 'found nil))))))))))))
           computed))
        (t (error "At least one of :uid, :tzid, or :date is required"))))

;; Buffer local variable to cache the index and parse tree.
;; Format: (TICKS VCALENDAR INDEX)
;; TICKS is the value of (buffer-modified-tick) at last parse
(defvar-local ical:-parsed-calendar-and-index '(0 nil nil))

(defun ical:parse-and-index (&optional buffer-or-file)
  "Parse and index the first iCalendar VCALENDAR object in BUFFER-OR-FILE.

Returns a list (VCALENDAR INDEX), where VCALENDAR is the parsed
`icalendar-vcalendar' syntax tree.  The index can then be queried to
retrieve components from this calendar by UID, TZID, or date; see
`icalendar-index-get'.

BUFFER-OR-FILE may be a buffer or a string containing a filename; it
defaults to the current buffer.  If it is a filename, an unfolded buffer
containing its data will be found, or created if necessary (see
`icalendar-unfolded-buffer-from-file').  The resulting buffer must
contain an iCalendar VCALENDAR object, which will be parsed and indexed.

The results of parsing and indexing are cached in buffer-local
variables, and subsequent calls with the same BUFFER-OR-FILE will return
the cached results as long as the buffer has not been modified in the
meantime."
  (let* ((buffer (cond ((null buffer-or-file) (current-buffer))
                       ((bufferp buffer-or-file) buffer-or-file)
                       ((and (stringp buffer-or-file)
                             (file-exists-p buffer-or-file))
                        (find-buffer-visiting buffer-or-file))))
         (file-name (cond (buffer (buffer-file-name buffer))
                          ((and (stringp buffer-or-file)
                                (file-exists-p buffer-or-file))
                           (expand-file-name buffer-or-file))))
         (unfolded (cond ((and buffer (ical:unfolded-p buffer))
                          buffer)
                         (file-name
                          (or (ical:find-unfolded-buffer-visiting file-name)
                              (ical:unfolded-buffer-from-file file-name)))
                         (buffer
                          (ical:unfolded-buffer-from-buffer buffer))
                         (t
                          (error "Unable to get unfolded buffer for '%s'"
                                 buffer-or-file)))))
    (with-current-buffer unfolded
      (when (ical:contains-vcalendar-p)
        (if (eql (car ical:-parsed-calendar-and-index) (buffer-modified-tick))
            (cdr ical:-parsed-calendar-and-index)
          (message "Parsing and indexing iCalendar data in %s..." (buffer-name))
          (let ((vcalendar (ical:parse)))
            (when vcalendar
              (setq ical:-parsed-calendar-and-index
                    (list
                     (buffer-modified-tick)
                     vcalendar
                     (ical:index-populate-from-calendar (ical:make-index)
                                                         vcalendar)))
              (message "Parsing and indexing iCalendar data in %s...Done."
                       (buffer-name))
              (cdr ical:-parsed-calendar-and-index))))))))



;;; Documentation for all of the above via `describe-symbol':
(defun ical:documented-symbol-p (sym)
  "Return non-nil if SYM is a symbol with iCalendar documentation."
  (or (get sym 'icalendar-type-documentation)
      ;; grammatical categories defined with rx-define, but with no
      ;; other special icalendar docs:
      (and (get sym 'rx-definition)
           (length> (symbol-name sym) 10)
           (equal "icalendar-" (substring (symbol-name sym) 0 10)))))

(defun ical:documentation (sym buf frame)
  "iCalendar documentation backend for `describe-symbol-backends'."
  (ignore buf frame) ; Silence the byte compiler
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (let* ((type-doc (get sym 'icalendar-type-documentation))
             (link (get sym 'icalendar-link))
             (rx-def (get sym 'rx-definition))
             (rx-doc (when rx-def
                       (with-output-to-string
                         (pp rx-def))))
             (value-rx-def (get sym 'ical:value-rx))
             (value-rx-doc (when value-rx-def
                             (with-output-to-string
                               (pp value-rx-def))))
             (values-rx-def (get sym 'ical:values-rx))
             (values-rx-doc (when values-rx-def
                             (with-output-to-string
                               (pp values-rx-def))))

             (full-doc
              (concat
               (when type-doc
                 (format "`%s' is an iCalendar type:\n\n%s\n\n"
                         sym type-doc))
               (when link
                 (format "For further information see\nURL `%s'\n\n" link))
               ;; FIXME: this is probably better done in rx.el!
               ;; TODO: could also generalize this to recursively
               ;; search rx-def for any symbol that starts with "icalendar-"...
               (when rx-def
                 (format "`%s' is an iCalendar grammar category.
Its `rx' definition is:\n\n%s%s%s"
                         sym
                         rx-doc
                         (if value-rx-def
                             (format "\nIndividual values must match:\n%s"
                                      value-rx-doc)
                           "")
                         (if values-rx-def
                             (format "\nLists of values must match:\n%s"
                                      values-rx-doc)
                           "")))
               "\n")))

        (insert full-doc)
        full-doc))))


(defconst ical:describe-symbol-backend
  '(nil icalendar-documented-symbol-p icalendar-documentation)
  "Entry for icalendar documentation in `describe-symbol-backends'.")

(push ical:describe-symbol-backend describe-symbol-backends)

;; Unloading:
(defun ical:parser-unload-function ()
  "Unload function for `icalendar-parser'."
  (mapatoms
   (lambda (sym)
     (when (string-match "^icalendar-" (symbol-name sym))
       (makunbound sym)
       (fmakunbound sym))))

  (setq describe-symbol-backends
        (remq ical:describe-symbol-backend describe-symbol-backends))
  ;; Proceed with normal unloading:
  nil)

(provide 'icalendar-parser)

;; Local Variables:
;; read-symbol-shorthands: (("ical:" . "icalendar-") ("icr:" . "icalendar-recur-"))
;; End:
;;; icalendar-parser.el ends here
