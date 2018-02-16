;;; json.el --- JavaScript Object Notation parser / generator -*- lexical-binding: t -*-

;; Copyright (C) 2006-2018 Free Software Foundation, Inc.

;; Author: Theresa O'Connor <ted@oconnor.cx>
;; Version: 1.4
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
;; `json-read' and `json-read-from-string'. The encoder has a single
;; entry point, `json-encode'.

;; Since there are several natural representations of key-value pair
;; mappings in elisp (alist, plist, hash-table), `json-read' allows you
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

;;; Code:

(require 'map)

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
  "If non-nil, ] and } closings will be formatted lisp-style,
without indentation.")

(defvar json-encoding-object-sort-predicate nil
  "Sorting predicate for JSON object keys during encoding.
If nil, no sorting is performed.  Else, JSON object keys are
ordered by the specified sort predicate during encoding.  For
instance, setting this to `string<' will have JSON object keys
ordered alphabetically.")

(defvar json-pre-element-read-function nil
  "Function called (if non-nil) by `json-read-array' and
`json-read-object' right before reading a JSON array or object,
respectively.  The function is called with one argument, which is
the current JSON key.")

(defvar json-post-element-read-function nil
  "Function called (if non-nil) by `json-read-array' and
`json-read-object' right after reading a JSON array or object,
respectively.")



;;; Utilities

(defun json-join (strings separator)
  "Join STRINGS with SEPARATOR."
  (mapconcat 'identity strings separator))

(defun json-alist-p (list)
  "Non-null if and only if LIST is an alist with simple keys."
  (while (consp list)
    (setq list (if (and (consp (car list))
                        (atom (caar list)))
                   (cdr list)
                 'not-alist)))
  (null list))

(defun json-plist-p (list)
  "Non-null if and only if LIST is a plist with keyword keys."
  (while (consp list)
    (setq list (if (and (keywordp (car list))
                        (consp (cdr list)))
                   (cddr list)
                 'not-plist)))
  (null list))

(defun json--plist-reverse (plist)
  "Return a copy of PLIST in reverse order.
Unlike `reverse', this keeps the property-value pairs intact."
  (let (res)
    (while plist
      (let ((prop (pop plist))
            (val (pop plist)))
        (push val res)
        (push prop res)))
    res))

(defun json--plist-to-alist (plist)
  "Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (pop plist))
            (val (pop plist)))
        (push (cons prop val) res)))
    (nreverse res)))

(defmacro json--with-indentation (body)
  `(let ((json--encoding-current-indentation
          (if json-encoding-pretty-print
              (concat json--encoding-current-indentation
                      json-encoding-default-indentation)
            "")))
     ,body))

;; Reader utilities

(define-inline json-advance (&optional n)
  "Advance N characters forward."
  (inline-quote (forward-char ,n)))

(define-inline json-peek ()
  "Return the character at point."
  (inline-quote (following-char)))

(define-inline json-pop ()
  "Advance past the character at point, returning it."
  (inline-quote
   (let ((char (json-peek)))
     (if (zerop char)
         (signal 'json-end-of-file nil)
       (json-advance)
       char))))

(define-inline json-skip-whitespace ()
  "Skip past the whitespace at point."
  ;; See
  ;; https://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
  ;; or https://tools.ietf.org/html/rfc7159#section-2 for the
  ;; definition of whitespace in JSON.
  (inline-quote (skip-chars-forward "\t\r\n ")))



;; Error conditions

(define-error 'json-error "Unknown JSON error")
(define-error 'json-readtable-error "JSON readtable error" 'json-error)
(define-error 'json-unknown-keyword "Unrecognized keyword" 'json-error)
(define-error 'json-number-format "Invalid number format" 'json-error)
(define-error 'json-string-escape "Bad Unicode escape" 'json-error)
(define-error 'json-string-format "Bad string format" 'json-error)
(define-error 'json-key-format "Bad JSON object key" 'json-error)
(define-error 'json-object-format "Bad JSON object" 'json-error)
(define-error 'json-end-of-file "End of file while parsing JSON"
  '(end-of-file json-error))



;;; Paths

(defvar json--path '()
  "Used internally by `json-path-to-position' to keep track of
the path during recursive calls to `json-read'.")

(defun json--record-path (key)
  "Record the KEY to the current JSON path.
Used internally by `json-path-to-position'."
  (push (cons (point) key) json--path))

(defun json--check-position (position)
  "Check if the last parsed JSON structure passed POSITION.
Used internally by `json-path-to-position'."
  (let ((start (caar json--path)))
    (when (< start position (+ (point) 1))
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
                indexes.

:match-start -- Position where the matched JSON element begins.

:match-end   -- Position where the matched JSON element ends.

This can for instance be useful to determine the path to a JSON
element in a deeply nested structure."
  (save-excursion
    (unless string
      (goto-char (point-min)))
    (let* ((json--path '())
           (json-pre-element-read-function #'json--record-path)
           (json-post-element-read-function
            (apply-partially #'json--check-position position))
           (path (catch :json-path
                   (if string
                       (json-read-from-string string)
                     (json-read)))))
      (when (plist-get path :path)
        path))))

;;; Keywords

(defvar json-keywords '("true" "false" "null")
  "List of JSON keywords.")

;; Keyword parsing

(defun json-read-keyword (keyword)
  "Read a JSON keyword at point.
KEYWORD is the keyword expected."
  (unless (member keyword json-keywords)
    (signal 'json-unknown-keyword (list keyword)))
  (mapc (lambda (char)
          (when (/= char (json-peek))
            (signal 'json-unknown-keyword
                    (list (save-excursion
                            (backward-word-strictly 1)
                            (thing-at-point 'word)))))
          (json-advance))
        keyword)
  (json-skip-whitespace)
  (unless (looking-at "\\([],}]\\|$\\)")
    (signal 'json-unknown-keyword
            (list (save-excursion
                    (backward-word-strictly 1)
                    (thing-at-point 'word)))))
  (cond ((string-equal keyword "true") t)
        ((string-equal keyword "false") json-false)
        ((string-equal keyword "null") json-null)))

;; Keyword encoding

(defun json-encode-keyword (keyword)
  "Encode KEYWORD as a JSON value."
  (cond ((eq keyword t)          "true")
        ((eq keyword json-false) "false")
        ((eq keyword json-null)  "null")))

;;; Numbers

;; Number parsing

(defun json-read-number (&optional sign)
 "Read the JSON number following point.
The optional SIGN argument is for internal use.

N.B.: Only numbers which can fit in Emacs Lisp's native number
representation will be parsed correctly."
 ;; If SIGN is non-nil, the number is explicitly signed.
 (let ((number-regexp
        "\\([0-9]+\\)?\\(\\.[0-9]+\\)?\\([Ee][+-]?[0-9]+\\)?"))
   (cond ((and (null sign) (= (json-peek) ?-))
          (json-advance)
          (- (json-read-number t)))
         ((and (null sign) (= (json-peek) ?+))
          (json-advance)
          (json-read-number t))
         ((and (looking-at number-regexp)
               (or (match-beginning 1)
                   (match-beginning 2)))
          (goto-char (match-end 0))
          (string-to-number (match-string 0)))
         (t (signal 'json-number-format (list (point)))))))

;; Number encoding

(defun json-encode-number (number)
  "Return a JSON representation of NUMBER."
  (format "%s" number))

;;; Strings

(defvar json-special-chars
  '((?\" . ?\")
    (?\\ . ?\\)
    (?b . ?\b)
    (?f . ?\f)
    (?n . ?\n)
    (?r . ?\r)
    (?t . ?\t))
  "Characters which are escaped in JSON, with their elisp counterparts.")

;; String parsing

(defun json--decode-utf-16-surrogates (high low)
  "Return the code point represented by the UTF-16 surrogates HIGH and LOW."
  (+ (lsh (- high #xD800) 10) (- low #xDC00) #x10000))

(defun json-read-escaped-char ()
  "Read the JSON string escaped character at point."
  ;; Skip over the '\'
  (json-advance)
  (let* ((char (json-pop))
         (special (assq char json-special-chars)))
    (cond
     (special (cdr special))
     ((not (eq char ?u)) char)
     ;; Special-case UTF-16 surrogate pairs,
     ;; cf. <https://tools.ietf.org/html/rfc7159#section-7>.  Note that
     ;; this clause overlaps with the next one and therefore has to
     ;; come first.
     ((looking-at
       (rx (group (any "Dd") (any "89ABab") (= 2 (any xdigit)))
           "\\u" (group (any "Dd") (any "C-Fc-f") (= 2 (any xdigit)))))
      (json-advance 10)
      (json--decode-utf-16-surrogates
       (string-to-number (match-string 1) 16)
       (string-to-number (match-string 2) 16)))
     ((looking-at (rx (= 4 xdigit)))
      (let ((hex (match-string 0)))
        (json-advance 4)
        (string-to-number hex 16)))
     (t
      (signal 'json-string-escape (list (point)))))))

(defun json-read-string ()
  "Read the JSON string at point."
  (unless (= (json-peek) ?\")
    (signal 'json-string-format (list "doesn't start with `\"'!")))
  ;; Skip over the '"'
  (json-advance)
  (let ((characters '())
        (char (json-peek)))
    (while (not (= char ?\"))
      (when (< char 32)
        (signal 'json-string-format (list (prin1-char char))))
      (push (if (= char ?\\)
                (json-read-escaped-char)
              (json-pop))
            characters)
      (setq char (json-peek)))
    ;; Skip over the '"'
    (json-advance)
    (if characters
        (concat (nreverse characters))
      "")))

;; String encoding

(defun json-encode-string (string)
  "Return a JSON representation of STRING."
  ;; Reimplement the meat of `replace-regexp-in-string', for
  ;; performance (bug#20154).
  (let ((l (length string))
        (start 0)
        res mb)
    ;; Only escape quotation mark, backslash and the control
    ;; characters U+0000 to U+001F (RFC 4627, ECMA-404).
    (while (setq mb (string-match "[\"\\[:cntrl:]]" string start))
      (let* ((c (aref string mb))
             (special (rassq c json-special-chars)))
        (push (substring string start mb) res)
        (push (if special
                  ;; Special JSON character (\n, \r, etc.).
                  (string ?\\ (car special))
                ;; Fallback: UCS code point in \uNNNN form.
                (format "\\u%04x" c))
              res)
        (setq start (1+ mb))))
    (push (substring string start l) res)
    (push "\"" res)
    (apply #'concat "\"" (nreverse res))))

(defun json-encode-key (object)
  "Return a JSON representation of OBJECT.
If the resulting JSON object isn't a valid JSON object key,
this signals `json-key-format'."
  (let ((encoded (json-encode object)))
    (unless (stringp (json-read-from-string encoded))
      (signal 'json-key-format (list object)))
    encoded))

;;; JSON Objects

(defun json-new-object ()
  "Create a new Elisp object corresponding to a JSON object.
Please see the documentation of `json-object-type'."
  (cond ((eq json-object-type 'hash-table)
         (make-hash-table :test 'equal))
        (t
         ())))

(defun json-add-to-object (object key value)
  "Add a new KEY -> VALUE association to OBJECT.
Returns the updated object, which you should save, e.g.:
    (setq obj (json-add-to-object obj \"foo\" \"bar\"))
Please see the documentation of `json-object-type' and `json-key-type'."
  (let ((json-key-type
         (or json-key-type
             (cdr (assq json-object-type '((hash-table . string)
                                           (alist . symbol)
                                           (plist . keyword)))))))
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
  ;; Skip over the "{"
  (json-advance)
  (json-skip-whitespace)
  ;; read key/value pairs until "}"
  (let ((elements (json-new-object))
        key value)
    (while (not (= (json-peek) ?}))
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
      (when (/= (json-peek) ?})
        (if (= (json-peek) ?,)
            (json-advance)
          (signal 'json-object-format (list "," (json-peek))))))
    ;; Skip over the "}"
    (json-advance)
    (pcase json-object-type
      (`alist (nreverse elements))
      (`plist (json--plist-reverse elements))
      (_ elements))))

;; Hash table encoding

(defun json-encode-hash-table (hash-table)
  "Return a JSON representation of HASH-TABLE."
  (if json-encoding-object-sort-predicate
      (json-encode-alist (map-into hash-table 'list))
    (format "{%s%s}"
            (json-join
             (let (r)
               (json--with-indentation
                (maphash
                 (lambda (k v)
                   (push (format
                          (if json-encoding-pretty-print
                              "%s%s: %s"
                            "%s%s:%s")
                          json--encoding-current-indentation
                          (json-encode-key k)
                          (json-encode v))
                         r))
                 hash-table))
               r)
             json-encoding-separator)
            (if (or (not json-encoding-pretty-print)
                    json-encoding-lisp-style-closings)
                ""
              json--encoding-current-indentation))))

;; List encoding (including alists and plists)

(defun json-encode-alist (alist)
  "Return a JSON representation of ALIST."
  (when json-encoding-object-sort-predicate
    (setq alist
          (sort alist (lambda (a b)
                        (funcall json-encoding-object-sort-predicate
                                 (car a) (car b))))))
  (format "{%s%s}"
          (json-join
           (json--with-indentation
            (mapcar (lambda (cons)
                      (format (if json-encoding-pretty-print
                                  "%s%s: %s"
                                "%s%s:%s")
                              json--encoding-current-indentation
                              (json-encode-key (car cons))
                              (json-encode (cdr cons))))
                    alist))
           json-encoding-separator)
          (if (or (not json-encoding-pretty-print)
                  json-encoding-lisp-style-closings)
              ""
            json--encoding-current-indentation)))

(defun json-encode-plist (plist)
  "Return a JSON representation of PLIST."
  (if json-encoding-object-sort-predicate
      (json-encode-alist (json--plist-to-alist plist))
    (let (result)
      (json--with-indentation
       (while plist
         (push (concat
                json--encoding-current-indentation
                (json-encode-key (car plist))
                (if json-encoding-pretty-print
                    ": "
                  ":")
                (json-encode (cadr plist)))
               result)
         (setq plist (cddr plist))))
      (concat "{"
              (json-join (nreverse result) json-encoding-separator)
              (if (and json-encoding-pretty-print
                       (not json-encoding-lisp-style-closings))
                  json--encoding-current-indentation
                "")
              "}"))))

(defun json-encode-list (list)
  "Return a JSON representation of LIST.
Tries to DWIM: simple lists become JSON arrays, while alists and plists
become JSON objects."
  (cond ((null list)         "null")
        ((json-alist-p list) (json-encode-alist list))
        ((json-plist-p list) (json-encode-plist list))
        ((listp list)        (json-encode-array list))
        (t
         (signal 'json-error (list list)))))

;;; Arrays

;; Array parsing

(defun json-read-array ()
  "Read the JSON array at point."
  ;; Skip over the "["
  (json-advance)
  (json-skip-whitespace)
  ;; read values until "]"
  (let (elements)
    (while (not (= (json-peek) ?\]))
      (json-skip-whitespace)
      (when json-pre-element-read-function
        (funcall json-pre-element-read-function (length elements)))
      (push (json-read) elements)
      (when json-post-element-read-function
        (funcall json-post-element-read-function))
      (json-skip-whitespace)
      (when (/= (json-peek) ?\])
        (if (= (json-peek) ?,)
            (json-advance)
          (signal 'json-error (list 'bleah)))))
    ;; Skip over the "]"
    (json-advance)
    (pcase json-array-type
      (`vector (nreverse (vconcat elements)))
      (`list (nreverse elements)))))

;; Array encoding

(defun json-encode-array (array)
  "Return a JSON representation of ARRAY."
  (if (and json-encoding-pretty-print
           (> (length array) 0))
      (concat
       (json--with-indentation
         (concat (format "[%s" json--encoding-current-indentation)
                 (json-join (mapcar 'json-encode array)
                            (format "%s%s"
                                    json-encoding-separator
                                    json--encoding-current-indentation))))
       (format "%s]"
               (if json-encoding-lisp-style-closings
                   ""
                 json--encoding-current-indentation)))
    (concat "["
            (mapconcat 'json-encode array json-encoding-separator)
            "]")))



;;; JSON reader.

(defmacro json-readtable-dispatch (char)
  "Dispatch reader function for CHAR."
  (declare (debug (symbolp)))
  (let ((table
         '((?t json-read-keyword "true")
           (?f json-read-keyword "false")
           (?n json-read-keyword "null")
           (?{ json-read-object)
           (?\[ json-read-array)
           (?\" json-read-string)))
        res)
    (dolist (c '(?- ?+ ?. ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
      (push (list c 'json-read-number) table))
    (pcase-dolist (`(,c . ,rest) table)
      (push `((eq ,char ,c) (,@rest)) res))
    `(cond ,@res (t (signal 'json-readtable-error (list ,char))))))

(defun json-read ()
  "Parse and return the JSON object following point.
Advances point just past JSON object."
  (json-skip-whitespace)
  (let ((char (json-peek)))
    (if (zerop char)
        (signal 'json-end-of-file nil)
      (json-readtable-dispatch char))))

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
    (goto-char (point-min))
    (json-read)))



;;; JSON encoder

(defun json-encode (object)
  "Return a JSON representation of OBJECT as a string."
  (cond ((memq object (list t json-null json-false))
         (json-encode-keyword object))
        ((stringp object)      (json-encode-string object))
        ((keywordp object)     (json-encode-string
                                (substring (symbol-name object) 1)))
        ((symbolp object)      (json-encode-string
                                (symbol-name object)))
        ((numberp object)      (json-encode-number object))
        ((arrayp object)       (json-encode-array object))
        ((hash-table-p object) (json-encode-hash-table object))
        ((listp object)        (json-encode-list object))
        (t                     (signal 'json-error (list object)))))

;; Pretty printing

(defun json-pretty-print-buffer ()
  "Pretty-print current buffer."
  (interactive)
  (json-pretty-print (point-min) (point-max)))

(defun json-pretty-print (begin end)
  "Pretty-print selected region."
  (interactive "r")
  (atomic-change-group
    (let ((json-encoding-pretty-print t)
          ;; Ensure that ordering is maintained
          (json-object-type 'alist)
          (txt (delete-and-extract-region begin end)))
      (insert (json-encode (json-read-from-string txt))))))

(defun json-pretty-print-buffer-ordered ()
  "Pretty-print current buffer with object keys ordered."
  (interactive)
  (let ((json-encoding-object-sort-predicate 'string<))
    (json-pretty-print-buffer)))

(defun json-pretty-print-ordered (begin end)
  "Pretty-print the region with object keys ordered."
  (interactive "r")
  (let ((json-encoding-object-sort-predicate 'string<))
    (json-pretty-print begin end)))

(provide 'json)

;;; json.el ends here
