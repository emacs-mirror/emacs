;;; json.el --- JavaScript Object Notation parser / generator -*- lexical-binding: t -*-

;; Copyright (C) 2006-2020 Free Software Foundation, Inc.

;; Author: Theresa O'Connor <ted@oconnor.cx>
;; Version: 1.5
;; Keywords: convenience

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

;; This is a library for parsing and generating JSON (JavaScript Object
;; Notation).

;; Learn all about JSON here: <URL:http://json.org/>.

;; The user-serviceable entry points for the parser are the functions
;; `json-read' and `json-read-from-string'.  The encoder has a single
;; entry point, `json-encode'.

;; Since there are several natural representations of key-value pair
;; mappings in Elisp (alist, plist, hash-table), `json-read' allows you
;; to specify which you'd prefer (see `json-object-type' and
;; `json-array-type').

;; Similarly, since `false' and `null' are distinct in JSON, you can
;; distinguish them by binding `json-false' and `json-null' as desired.

;;; History:

;; 2006-03-11 - Initial version.
;; 2006-03-13 - Added JSON generation in addition to parsing. Various
;;              other cleanups, bugfixes, and improvements.
;; 2006-12-29 - XEmacs support, from Aidan Kehoe <kehoea@parhasard.net>.
;; 2008-02-21 - Installed in GNU Emacs.
;; 2011-10-17 - Patch `json-alist-p' and `json-plist-p' to avoid recursion -tzz
;; 2012-10-25 - Added pretty-printed reformatting -Ryan Crum (ryan@ryancrum.org)
;; 2019-02-02 - Pretty-printing now uses replace-region-contents and support for
;;              minimization -tsdh

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)

;; Parameters

(defvar json-object-type 'alist
  "Type to convert JSON objects to.
Must be one of `alist', `plist', or `hash-table'.  Consider let-binding
this around your call to `json-read' instead of `setq'ing it.  Ordering
is maintained for `alist' and `plist', but not for `hash-table'.")

(defvar json-array-type 'vector
  "Type to convert JSON arrays to.
Must be one of `vector' or `list'.  Consider let-binding this around
your call to `json-read' instead of `setq'ing it.")

(defvar json-key-type nil
  "Type to convert JSON keys to.
Must be one of `string', `symbol', `keyword', or nil.

If nil, `json-read' will guess the type based on the value of
`json-object-type':

    If `json-object-type' is:   nil will be interpreted as:
      `hash-table'                `string'
      `alist'                     `symbol'
      `plist'                     `keyword'

Note that values other than `string' might behave strangely for
Sufficiently Weird keys.  Consider let-binding this around your call to
`json-read' instead of `setq'ing it.")

(defvar json-false :json-false
  "Value to use when reading JSON `false'.
If this has the same value as `json-null', you might not be able to tell
the difference between `false' and `null'.  Consider let-binding this
around your call to `json-read' instead of `setq'ing it.")

(defvar json-null nil
  "Value to use when reading JSON `null'.
If this has the same value as `json-false', you might not be able to
tell the difference between `false' and `null'.  Consider let-binding
this around your call to `json-read' instead of `setq'ing it.")

(defvar json-encoding-separator ","
  "Value to use as an element separator when encoding.")

(defvar json-encoding-default-indentation "  "
  "The default indentation level for encoding.
Used only when `json-encoding-pretty-print' is non-nil.")

(defvar json--encoding-current-indentation "\n"
  "Internally used to keep track of the current indentation level of encoding.
Used only when `json-encoding-pretty-print' is non-nil.")

(defvar json-encoding-pretty-print nil
  "If non-nil, then the output of `json-encode' will be pretty-printed.")

(defvar json-encoding-lisp-style-closings nil
  "If non-nil, delimiters ] and } will be formatted Lisp-style.
This means they will be placed on the same line as the last
element of the respective array or object, without indentation.
Used only when `json-encoding-pretty-print' is non-nil.")

(defvar json-encoding-object-sort-predicate nil
  "Sorting predicate for JSON object keys during encoding.
If nil, no sorting is performed.  Else, JSON object keys are
ordered by the specified sort predicate during encoding.  For
instance, setting this to `string<' will have JSON object keys
ordered alphabetically.")

(defvar json-pre-element-read-function nil
  "If non-nil, a function to call before reading a JSON array or object.
It is called by `json-read-array' and `json-read-object',
respectively, with one argument, which is the current JSON key.")

(defvar json-post-element-read-function nil
  "If non-nil, a function to call after reading a JSON array or object.
It is called by `json-read-array' and `json-read-object',
respectively, with no arguments.")



;;; Utilities

(define-obsolete-function-alias 'json-join #'string-join "28.1")

(defun json-alist-p (list)
  "Non-nil if and only if LIST is an alist with simple keys."
  (declare (pure t) (side-effect-free error-free))
  (while (and (consp (car-safe list))
              (atom (caar list))
              (setq list (cdr list))))
  (null list))

(defun json-plist-p (list)
  "Non-nil if and only if LIST is a plist with keyword keys."
  (declare (pure t) (side-effect-free error-free))
  (while (and (keywordp (car-safe list))
              (consp (cdr list))
              (setq list (cddr list))))
  (null list))

(defun json--plist-nreverse (plist)
  "Return PLIST in reverse order.
Unlike `nreverse', this keeps the ordering of each property
relative to its value intact.  Like `nreverse', this function may
destructively modify PLIST to produce the result."
  (let (prev (next (cddr plist)))
    (while next
      (setcdr (cdr plist) prev)
      (setq prev plist plist next next (cddr next))
      (setcdr (cdr plist) prev)))
  plist)

(defmacro json--with-indentation (&rest body)
  "Evaluate BODY with the correct indentation for JSON encoding.
This macro binds `json--encoding-current-indentation' according
to `json-encoding-pretty-print' around BODY."
  (declare (debug t) (indent 0))
  `(let ((json--encoding-current-indentation
          (if json-encoding-pretty-print
              (concat json--encoding-current-indentation
                      json-encoding-default-indentation)
            "")))
     ,@body))

;; Reader utilities

(define-inline json-advance (&optional n)
  "Advance N characters forward, or 1 character if N is nil.
On reaching the end of the accessible region of the buffer, stop
and signal an error."
  (inline-quote (forward-char ,n)))

(define-inline json-peek ()
  "Return the character at point.
At the end of the accessible region of the buffer, return 0."
  (inline-quote (following-char)))

(define-inline json-pop ()
  "Advance past the character at point, returning it.
Signal `json-end-of-file' if called at the end of the buffer."
  (inline-quote
   (prog1 (or (char-after)
              (signal 'json-end-of-file ()))
     (json-advance))))

(define-inline json-skip-whitespace ()
  "Skip past the whitespace at point."
  ;; See
  ;; https://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
  ;; or https://tools.ietf.org/html/rfc7159#section-2 for the
  ;; definition of whitespace in JSON.
  (inline-quote (skip-chars-forward "\t\n\r ")))



;; Error conditions

(define-error 'json-error "Unknown JSON error")
(define-error 'json-readtable-error "JSON readtable error" 'json-error)
(define-error 'json-unknown-keyword "Unrecognized keyword" 'json-error)
(define-error 'json-number-format "Invalid number format" 'json-error)
(define-error 'json-string-escape "Bad Unicode escape" 'json-error)
(define-error 'json-string-format "Bad string format" 'json-error)
(define-error 'json-key-format "Bad JSON object key" 'json-error)
(define-error 'json-object-format "Bad JSON object" 'json-error)
(define-error 'json-array-format "Bad JSON array" 'json-error)
(define-error 'json-end-of-file "End of file while parsing JSON"
  '(end-of-file json-error))



;;; Paths

(defvar json--path '()
  "Keeps track of the path during recursive calls to `json-read'.
Used internally by `json-path-to-position'.")

(defun json--record-path (key)
  "Record the KEY to the current JSON path.
Used internally by `json-path-to-position'."
  (push (cons (point) key) json--path))

(defun json--check-position (position)
  "Check if the last parsed JSON structure passed POSITION.
Used internally by `json-path-to-position'."
  (let ((start (caar json--path)))
    (when (< start position (1+ (point)))
      (throw :json-path (list :path (nreverse (mapcar #'cdr json--path))
                              :match-start start
                              :match-end (point)))))
  (pop json--path))

(defun json-path-to-position (position &optional string)
  "Return the path to the JSON element at POSITION.

When STRING is provided, return the path to the position in the
string, else to the position in the current buffer.

The return value is a property list with the following
properties:

:path        -- A list of strings and numbers forming the path to
                the JSON element at the given position.  Strings
                denote object names, while numbers denote array
                indices.

:match-start -- Position where the matched JSON element begins.

:match-end   -- Position where the matched JSON element ends.

This can, for instance, be useful to determine the path to a JSON
element in a deeply nested structure."
  (save-excursion
    (unless string
      (goto-char (point-min)))
    (let* ((json--path '())
           (json-pre-element-read-function #'json--record-path)
           (json-post-element-read-function
            (lambda () (json--check-position position)))
           (path (catch :json-path
                   (if string
                       (json-read-from-string string)
                     (json-read)))))
      (when (plist-get path :path)
        path))))

;;; Keywords

(defconst json-keywords '("true" "false" "null")
  "List of JSON keywords.")
(make-obsolete-variable 'json-keywords "it is no longer used." "28.1")

;; Keyword parsing

;; Characters that can follow a JSON value.
(rx-define json--post-value (| (in "\t\n\r ,]}") eos))

(defun json-read-keyword (keyword)
  "Read the expected JSON KEYWORD at point."
  (prog1 (cond ((equal keyword "true")  t)
               ((equal keyword "false") json-false)
               ((equal keyword "null")  json-null)
               (t (signal 'json-unknown-keyword (list keyword))))
    (or (looking-at-p keyword)
        (signal 'json-unknown-keyword (list (thing-at-point 'word))))
    (json-advance (length keyword))
    (or (looking-at-p (rx json--post-value))
        (signal 'json-unknown-keyword (list (thing-at-point 'word))))
    (json-skip-whitespace)))

;; Keyword encoding

(defun json-encode-keyword (keyword)
  "Encode KEYWORD as a JSON value."
  (declare (side-effect-free t))
  (cond ((eq keyword t)          "true")
        ((eq keyword json-false) "false")
        ((eq keyword json-null)  "null")))

;;; Numbers

;; Number parsing

(rx-define json--number
  (: (? ?-)                                   ; Sign.
     (| (: (in "1-9") (* digit)) ?0)          ; Integer.
     (? ?. (+ digit))                         ; Fraction.
     (? (in "Ee") (? (in ?+ ?-)) (+ digit)))) ; Exponent.

(defun json-read-number (&optional _sign)
  "Read the JSON number following point."
  (declare (advertised-calling-convention () "28.1"))
  (or (looking-at (rx json--number))
      (signal 'json-number-format (list (point))))
  (goto-char (match-end 0))
  (prog1 (string-to-number (match-string 0))
    (or (looking-at-p (rx json--post-value))
        (signal 'json-number-format (list (point))))
    (json-skip-whitespace)))

;; Number encoding

(defalias 'json-encode-number #'number-to-string
  "Return a JSON representation of NUMBER.")

;;; Strings

(defconst json-special-chars
  '((?\" . ?\")
    (?\\ . ?\\)
    (?b . ?\b)
    (?f . ?\f)
    (?n . ?\n)
    (?r . ?\r)
    (?t . ?\t))
  "Characters which are escaped in JSON, with their Elisp counterparts.")

;; String parsing

(defun json--decode-utf-16-surrogates (high low)
  "Return the code point represented by the UTF-16 surrogates HIGH and LOW."
  (+ (ash (- high #xD800) 10) (- low #xDC00) #x10000))

(defun json-read-escaped-char ()
  "Read the JSON string escaped character at point."
  ;; Skip over the '\'.
  (json-advance)
  (let ((char (json-pop)))
    (cond
     ((cdr (assq char json-special-chars)))
     ((/= char ?u) char)
     ;; Special-case UTF-16 surrogate pairs,
     ;; cf. <https://tools.ietf.org/html/rfc7159#section-7>.  Note that
     ;; this clause overlaps with the next one and therefore has to
     ;; come first.
     ((looking-at
       (rx (group (any "Dd") (any "89ABab") (= 2 xdigit))
           "\\u" (group (any "Dd") (any "C-Fc-f") (= 2 xdigit))))
      (json-advance 10)
      (json--decode-utf-16-surrogates
       (string-to-number (match-string 1) 16)
       (string-to-number (match-string 2) 16)))
     ((looking-at (rx (= 4 xdigit)))
      (json-advance 4)
      (string-to-number (match-string 0) 16))
     (t
      (signal 'json-string-escape (list (point)))))))

(defun json-read-string ()
  "Read the JSON string at point."
  ;; Skip over the '"'.
  (json-advance)
  (let ((characters '())
        (char (json-peek)))
    (while (/= char ?\")
      (when (< char 32)
        (if (zerop char)
            (signal 'json-end-of-file ())
          (signal 'json-string-format (list char))))
      (push (if (= char ?\\)
                (json-read-escaped-char)
              (json-advance)
              char)
            characters)
      (setq char (json-peek)))
    ;; Skip over the '"'.
    (json-advance)
    (if characters
        (concat (nreverse characters))
      "")))

;; String encoding

;; Escape only quotation mark, backslash, and the control
;; characters U+0000 to U+001F (RFC 4627, ECMA-404).
(rx-define json--escape (in ?\" ?\\ cntrl))

(defvar json--long-string-threshold 200
  "Length above which strings are considered long for JSON encoding.
It is generally faster to manipulate such strings in a buffer
rather than directly.")

(defvar json--string-buffer nil
  "Buffer used for encoding Lisp strings as JSON.
Initialized lazily by `json-encode-string'.")

(defun json-encode-string (string)
  "Return a JSON representation of STRING."
  ;; Try to avoid buffer overhead in trivial cases, while also
  ;; avoiding searching pathological strings for escape characters.
  ;; Since `string-match-p' doesn't take a LIMIT argument, we use
  ;; string length as our heuristic.  See also bug#20154.
  (if (and (< (length string) json--long-string-threshold)
           (not (string-match-p (rx json--escape) string)))
      (concat "\"" string "\"")
    (with-current-buffer
        (or json--string-buffer
            (with-current-buffer (generate-new-buffer " *json-string*")
              ;; This seems to afford decent performance gains.
              (setq-local inhibit-modification-hooks t)
              (setq json--string-buffer (current-buffer))))
      (insert ?\" string)
      (goto-char (1+ (point-min)))
      (while (re-search-forward (rx json--escape) nil 'move)
        (let ((char (preceding-char)))
          (delete-char -1)
          (insert ?\\ (or
                       ;; Special JSON character (\n, \r, etc.).
                       (car (rassq char json-special-chars))
                       ;; Fallback: UCS code point in \uNNNN form.
                       (format "u%04x" char)))))
      (insert ?\")
      ;; Empty buffer for next invocation.
      (delete-and-extract-region (point-min) (point-max)))))

(defun json-encode-key (object)
  "Return a JSON representation of OBJECT.
If the resulting JSON object isn't a valid JSON object key,
this signals `json-key-format'."
  (let ((encoded (json-encode object)))
    (unless (stringp (json-read-from-string encoded))
      (signal 'json-key-format (list object)))
    encoded))

;;; Objects

(defun json-new-object ()
  "Create a new Elisp object corresponding to an empty JSON object.
Please see the documentation of `json-object-type'."
  (and (eq json-object-type 'hash-table)
       (make-hash-table :test #'equal)))

(defun json-add-to-object (object key value)
  "Add a new KEY -> VALUE association to OBJECT.
Returns the updated object, which you should save, e.g.:
    (setq obj (json-add-to-object obj \"foo\" \"bar\"))
Please see the documentation of `json-object-type' and `json-key-type'."
  (let ((json-key-type
         (cond (json-key-type)
               ((eq json-object-type 'hash-table) 'string)
               ((eq json-object-type 'alist)      'symbol)
               ((eq json-object-type 'plist)      'keyword))))
    (setq key
          (cond ((eq json-key-type 'string)
                 key)
                ((eq json-key-type 'symbol)
                 (intern key))
                ((eq json-key-type 'keyword)
                 (intern (concat ":" key)))))
    (cond ((eq json-object-type 'hash-table)
           (puthash key value object)
           object)
          ((eq json-object-type 'alist)
           (cons (cons key value) object))
          ((eq json-object-type 'plist)
           (cons key (cons value object))))))

;; JSON object parsing

(defun json-read-object ()
  "Read the JSON object at point."
  ;; Skip over the '{'.
  (json-advance)
  (json-skip-whitespace)
  ;; Read key/value pairs until '}'.
  (let ((elements (json-new-object))
        key value)
    (while (/= (json-peek) ?\})
      (json-skip-whitespace)
      (setq key (json-read-string))
      (json-skip-whitespace)
      (if (= (json-peek) ?:)
          (json-advance)
        (signal 'json-object-format (list ":" (json-peek))))
      (json-skip-whitespace)
      (when json-pre-element-read-function
        (funcall json-pre-element-read-function key))
      (setq value (json-read))
      (when json-post-element-read-function
        (funcall json-post-element-read-function))
      (setq elements (json-add-to-object elements key value))
      (json-skip-whitespace)
      (when (/= (json-peek) ?\})
        (if (= (json-peek) ?,)
            (json-advance)
          (signal 'json-object-format (list "," (json-peek))))))
    ;; Skip over the '}'.
    (json-advance)
    (pcase json-object-type
      ('alist (nreverse elements))
      ('plist (json--plist-nreverse elements))
      (_ elements))))

;; Hash table encoding

(defun json-encode-hash-table (hash-table)
  "Return a JSON representation of HASH-TABLE."
  (cond ((hash-table-empty-p hash-table) "{}")
        (json-encoding-object-sort-predicate
         (json--encode-alist (map-pairs hash-table) t))
        (t
         (let ((kv-sep (if json-encoding-pretty-print ": " ":"))
               result)
           (json--with-indentation
             (maphash
              (lambda (k v)
                (push (concat json--encoding-current-indentation
                              (json-encode-key k)
                              kv-sep
                              (json-encode v))
                      result))
              hash-table))
           (concat "{"
                   (string-join (nreverse result) json-encoding-separator)
                   (and json-encoding-pretty-print
                        (not json-encoding-lisp-style-closings)
                        json--encoding-current-indentation)
                   "}")))))

;; List encoding (including alists and plists)

(defun json--encode-alist (alist &optional destructive)
  "Return a JSON representation of ALIST.
DESTRUCTIVE non-nil means it is safe to modify ALIST by
side-effects."
  (when json-encoding-object-sort-predicate
    (setq alist (sort (if destructive alist (copy-sequence alist))
                      (lambda (a b)
                        (funcall json-encoding-object-sort-predicate
                                 (car a) (car b))))))
  (concat "{"
          (let ((kv-sep (if json-encoding-pretty-print ": " ":")))
            (json--with-indentation
              (mapconcat (lambda (cons)
                           (concat json--encoding-current-indentation
                                   (json-encode-key (car cons))
                                   kv-sep
                                   (json-encode (cdr cons))))
                         alist
                         json-encoding-separator)))
          (and json-encoding-pretty-print
               (not json-encoding-lisp-style-closings)
               json--encoding-current-indentation)
          "}"))

(defun json-encode-alist (alist)
  "Return a JSON representation of ALIST."
  (if alist (json--encode-alist alist) "{}"))

(defun json-encode-plist (plist)
  "Return a JSON representation of PLIST."
  (cond ((null plist) "{}")
        (json-encoding-object-sort-predicate
         (json--encode-alist (map-pairs plist) t))
        (t
         (let ((kv-sep (if json-encoding-pretty-print ": " ":"))
               result)
           (json--with-indentation
             (while plist
               (push (concat json--encoding-current-indentation
                             (json-encode-key (pop plist))
                             kv-sep
                             (json-encode (pop plist)))
                     result)))
           (concat "{"
                   (string-join (nreverse result) json-encoding-separator)
                   (and json-encoding-pretty-print
                        (not json-encoding-lisp-style-closings)
                        json--encoding-current-indentation)
                   "}")))))

(defun json-encode-list (list)
  "Return a JSON representation of LIST.
Tries to DWIM: simple lists become JSON arrays, while alists and plists
become JSON objects."
  (cond ((json-alist-p list) (json-encode-alist list))
        ((json-plist-p list) (json-encode-plist list))
        ((listp list)        (json-encode-array list))
        (t
         (signal 'json-error (list list)))))

;;; Arrays

;; Array parsing

(defun json-read-array ()
  "Read the JSON array at point."
  ;; Skip over the '['.
  (json-advance)
  (json-skip-whitespace)
  ;; Read values until ']'.
  (let (elements
        (len 0))
    (while (/= (json-peek) ?\])
      (json-skip-whitespace)
      (when json-pre-element-read-function
        (funcall json-pre-element-read-function len)
        (setq len (1+ len)))
      (push (json-read) elements)
      (when json-post-element-read-function
        (funcall json-post-element-read-function))
      (json-skip-whitespace)
      (when (/= (json-peek) ?\])
        (if (= (json-peek) ?,)
            (json-advance)
          (signal 'json-array-format (list "," (json-peek))))))
    ;; Skip over the ']'.
    (json-advance)
    (pcase json-array-type
      ('vector (nreverse (vconcat elements)))
      ('list (nreverse elements)))))

;; Array encoding

(defun json-encode-array (array)
  "Return a JSON representation of ARRAY."
  (if (and json-encoding-pretty-print
           (not (seq-empty-p array)))
      (concat
       "["
       (json--with-indentation
         (concat json--encoding-current-indentation
                 (mapconcat #'json-encode array
                            (concat json-encoding-separator
                                    json--encoding-current-indentation))))
       (unless json-encoding-lisp-style-closings
         json--encoding-current-indentation)
       "]")
    (concat "["
            (mapconcat #'json-encode array json-encoding-separator)
            "]")))



;;; Reader

(defmacro json-readtable-dispatch (char)
  "Dispatch reader function for CHAR at point.
If CHAR is nil, signal `json-end-of-file'."
  (declare (debug t))
  (macroexp-let2 nil char char
    `(cond ,@(map-apply
              (lambda (key expr)
                `((eq ,char ,key) ,expr))
              `((?\" ,#'json-read-string)
                (?\[ ,#'json-read-array)
                (?\{ ,#'json-read-object)
                (?n  ,#'json-read-keyword "null")
                (?f  ,#'json-read-keyword "false")
                (?t  ,#'json-read-keyword "true")
                ,@(mapcar (lambda (c) (list c #'json-read-number))
                          '(?- ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))
           (,char (signal 'json-readtable-error (list ,char)))
           (t     (signal 'json-end-of-file ())))))

(defun json-read ()
  "Parse and return the JSON object following point.
Advances point just past JSON object.

If called with the following JSON after point

  {\"a\": [1, 2, {\"c\": false}],
   \"b\": \"foo\"}

you will get the following structure returned:

  ((a .
      [1 2
         ((c . :json-false))])
   (b . \"foo\"))"
  (json-skip-whitespace)
  (json-readtable-dispatch (char-after)))

;; Syntactic sugar for the reader

(defun json-read-from-string (string)
  "Read the JSON object contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (json-read)))

(defun json-read-file (file)
  "Read the first JSON object contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (json-read)))



;;; Encoder

(defun json-encode (object)
  "Return a JSON representation of OBJECT as a string.

OBJECT should have a structure like one returned by `json-read'.
If an error is detected during encoding, an error based on
`json-error' is signaled."
  (cond ((eq object t)          (json-encode-keyword object))
        ((eq object json-null)  (json-encode-keyword object))
        ((eq object json-false) (json-encode-keyword object))
        ((stringp object)       (json-encode-string object))
        ((keywordp object)      (json-encode-string
                                 (substring (symbol-name object) 1)))
        ((listp object)         (json-encode-list object))
        ((symbolp object)       (json-encode-string
                                 (symbol-name object)))
        ((numberp object)       (json-encode-number object))
        ((arrayp object)        (json-encode-array object))
        ((hash-table-p object)  (json-encode-hash-table object))
        (t                      (signal 'json-error (list object)))))

;;; Pretty printing & minimizing

(defun json-pretty-print-buffer (&optional minimize)
  "Pretty-print current buffer.
With prefix argument MINIMIZE, minimize it instead."
  (interactive "P")
  (json-pretty-print (point-min) (point-max) minimize))

(defvar json-pretty-print-max-secs 2.0
  "Maximum time for `json-pretty-print's comparison.
The function `json-pretty-print' uses `replace-region-contents'
(which see) passing the value of this variable as argument
MAX-SECS.")

(defun json-pretty-print (begin end &optional minimize)
  "Pretty-print selected region.
With prefix argument MINIMIZE, minimize it instead."
  (interactive "r\nP")
  (let ((json-encoding-pretty-print (null minimize))
        ;; Distinguish an empty object from 'null'.
        (json-null :json-null)
        ;; Ensure that ordering is maintained.
        (json-object-type 'alist)
        (orig-buf (current-buffer))
        error)
    ;; Strategy: Repeatedly `json-read' from the original buffer and
    ;; write the pretty-printed snippet to a temporary buffer.  As
    ;; soon as we get an error from `json-read', simply append the
    ;; remainder which we couldn't pretty-print to the temporary
    ;; buffer as well (probably the region ends _inside_ a JSON
    ;; object).
    ;;
    ;; Finally, use `replace-region-contents' to swap the original
    ;; region with the contents of the temporary buffer so that point,
    ;; marks, etc. are kept.
    (with-temp-buffer
      (let ((tmp-buf (current-buffer)))
        (set-buffer orig-buf)
        (replace-region-contents
         begin end
         (lambda ()
           (let ((pos (point))
                 (keep-going t))
             (while keep-going
               (condition-case err
                   ;; We want to format only the JSON snippets in the
                   ;; region without modifying the whitespace between
                   ;; them.
                   (let ((space (buffer-substring
                                 (point)
                                 (+ (point) (skip-chars-forward " \t\n"))))
                         (json (json-read)))
                     (setq pos (point)) ; End of last good json-read.
                     (set-buffer tmp-buf)
                     (insert space (json-encode json))
                     (set-buffer orig-buf))
                 (t
                  (setq keep-going nil)
                  (set-buffer orig-buf)
                  ;; Rescue the remainder we couldn't pretty-print.
                  (append-to-buffer tmp-buf pos (point-max))
                  ;; EOF is expected because we json-read until we hit
                  ;; the end of the narrow region.
                  (unless (eq (car err) 'json-end-of-file)
                    (setq error err)))))
             tmp-buf))
         json-pretty-print-max-secs
         ;; FIXME: What's a good value here?  Can we use something better,
         ;; e.g., by deriving a value from the size of the region?
         64)))
    ;; If we got an error during JSON processing (possibly the region
    ;; starts or ends inside a JSON object), signal it to the user.
    ;; We did our best.
    (when error
      (signal (car error) (cdr error)))))

(defun json-pretty-print-buffer-ordered (&optional minimize)
  "Pretty-print current buffer with object keys ordered.
With prefix argument MINIMIZE, minimize it instead."
  (interactive "P")
  (let ((json-encoding-object-sort-predicate #'string<))
    (json-pretty-print-buffer minimize)))

(defun json-pretty-print-ordered (begin end &optional minimize)
  "Pretty-print the region with object keys ordered.
With prefix argument MINIMIZE, minimize it instead."
  (interactive "r\nP")
  (let ((json-encoding-object-sort-predicate #'string<))
    (json-pretty-print begin end minimize)))

(provide 'json)

;;; json.el ends here
