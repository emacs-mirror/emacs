;;; esh-util.el --- general utilities  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;;; Code:

(require 'seq)
(eval-when-compile (require 'cl-lib))

(defgroup eshell-util nil
  "This is general utility code, meant for use by Eshell itself."
  :tag "General utilities"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-stringify-t t
  "If non-nil, the string representation of t is \"t\".
If nil, t will be represented only in the exit code of the function,
and not printed as a string.  This causes Lisp functions to behave
similarly to external commands, as far as successful result output."
  :type 'boolean)

(defcustom eshell-group-file "/etc/group"
  "If non-nil, the name of the group file on your system."
  :type '(choice (const :tag "No group file" nil) file))

(defcustom eshell-passwd-file "/etc/passwd"
  "If non-nil, the name of the passwd file on your system."
  :type '(choice (const :tag "No passwd file" nil) file))

(defcustom eshell-hosts-file "/etc/hosts"
  "The name of the /etc/hosts file.
Use `pcomplete-hosts-file' instead; this variable is obsolete and
has no effect."
  :type '(choice (const :tag "No hosts file" nil) file))
;; Don't make it into an alias, because it doesn't really work with
;; custom and risks creating duplicate entries.  Just point users to
;; the other variable, which is less frustrating.
(make-obsolete-variable 'eshell-hosts-file nil "28.1")

(defcustom eshell-handle-errors t
  "If non-nil, Eshell will handle errors itself.
Setting this to nil is offered as an aid to debugging only."
  :type 'boolean)

(defcustom eshell-private-file-modes #o600 ; umask 177
  "The file-modes value to use for creating \"private\" files.
This is decimal, not octal.  The default is 384 (0600 in octal)."
  :type 'integer)

(defcustom eshell-private-directory-modes #o700 ; umask 077
  "The file-modes value to use for creating \"private\" directories.
This is decimal, not octal.  The default is 448 (0700 in octal)."
  :type 'integer)

(defcustom eshell-tar-regexp
  "\\.t\\(ar\\(\\.\\(gz\\|bz2\\|xz\\|Z\\)\\)?\\|gz\\|a[zZ]\\|z2\\)\\'"
  "Regular expression used to match tar file names."
  :version "24.1"			; added xz
  :type 'regexp)

(defcustom eshell-convert-numeric-arguments t
  "If non-nil, converting arguments of numeric form to Lisp numbers.
Numeric form is tested using the regular expression
`eshell-number-regexp'.

NOTE: If you find that numeric conversions are interfering with the
specification of filenames (for example, in calling `find-file', or
some other Lisp function that deals with files, not numbers), add the
following in your init file:

  (put \\='find-file \\='eshell-no-numeric-conversions t)

Any function with the property `eshell-no-numeric-conversions' set to
a non-nil value, will be passed strings, not numbers, even when an
argument matches `eshell-number-regexp'."
  :type 'boolean)

(defcustom eshell-ange-ls-uids nil
  "List of user/host/id strings, used to determine remote ownership."
  :type '(repeat (cons :tag "Host for User/UID map"
		       (string :tag "Hostname")
		       (repeat (cons :tag "User/UID List"
				     (string :tag "Username")
				     (repeat :tag "UIDs" string))))))

(defcustom eshell-debug-command nil
  "A list of debug features to enable when running Eshell commands.
Possible entries are `form', to log the manipulation of Eshell
command forms, and `process', to log external process operations.

If nil, don't debug commands at all."
  :version "30.1"
  :type '(set (const :tag "Form manipulation" form)
              (const :tag "Process operations" process)))

;;; Internal Variables:

(defvar eshell-number-regexp
  (rx (? "-")
      (or (seq (+ digit) (? "." (* digit)))
          (seq (* digit) "." (+ digit)))
      ;; Optional exponent
      (? (or "e" "E")
         (or "+INF" "+NaN"
             (seq (? (or "+" "-")) (+ digit)))))
  "Regular expression used to match numeric arguments.
If `eshell-convert-numeric-arguments' is non-nil, and an argument
matches this regexp, it will be converted to a Lisp number, using the
function `string-to-number'.")

(defvar eshell-integer-regexp (rx (? "-") (+ digit))
  "Regular expression used to match integer arguments.")

(defvar eshell-group-names nil
  "A cache to hold the names of groups.")

(defvar eshell-group-timestamp nil
  "A timestamp of when the group file was read.")

(defvar eshell-user-names nil
  "A cache to hold the names of users.")

(defvar eshell-user-timestamp nil
  "A timestamp of when the user file was read.")

(defvar eshell-command-output-properties
  `( field command-output
     front-sticky (field)
     rear-nonsticky (field)
     ;; Text inserted by a user in the middle of process output
     ;; should be marked as output.  This is needed for commands
     ;; such as `yank' or `just-one-space' which don't use
     ;; `insert-and-inherit' and thus bypass default text property
     ;; inheritance.
     insert-in-front-hooks (,#'eshell--mark-as-output
                            ,#'eshell--mark-yanked-as-output))
  "A list of text properties to apply to command output.")

(defvar eshell-debug-command-buffer "*eshell last cmd*"
  "The name of the buffer to log debug messages about command invocation.")

;;; Obsolete variables:

(define-obsolete-variable-alias 'eshell-host-names
  'pcomplete--host-name-cache "28.1")
(define-obsolete-variable-alias 'eshell-host-timestamp
  'pcomplete--host-name-cache-timestamp "28.1")
(defvar pcomplete--host-name-cache)
(defvar pcomplete--host-name-cache-timestamp)

;;; Functions:

(defsubst eshell-under-windows-p ()
  "Return non-nil if we are running under MS-DOS/Windows."
  (memq system-type '(ms-dos windows-nt)))

(defmacro eshell-condition-case (tag form &rest handlers)
  "If `eshell-handle-errors' is non-nil, this is `condition-case'.
Otherwise, evaluates FORM with no error handling."
  (declare (indent 2) (debug (sexp form &rest form)))
  `(if eshell-handle-errors
       (condition-case-unless-debug ,tag
           ,form
         ,@handlers)
     ,form))

(defun eshell-debug-command-start (command)
  "Start debugging output for the command string COMMAND.
If debugging is enabled (see `eshell-debug-command'), this will
start logging to `*eshell last cmd*'."
  (when eshell-debug-command
    (with-current-buffer (get-buffer-create eshell-debug-command-buffer)
      (erase-buffer)
      (insert "command: \"" command "\"\n"))))

(defun eshell-always-debug-command (kind string &rest objects)
  "Output a debugging message to `*eshell last cmd*'.
KIND is the kind of message to log.  STRING and OBJECTS are as
`format-message' (which see)."
  (declare (indent 1))
  (with-current-buffer (get-buffer-create eshell-debug-command-buffer)
    (insert "\n\C-l\n[" (symbol-name kind) "] "
            (apply #'format-message string objects))))

(defmacro eshell-debug-command (kind string &rest objects)
  "Output a debugging message to `*eshell last cmd*' if debugging is enabled.
KIND is the kind of message to log (either `form' or `process').  If
present in `eshell-debug-command', output this message; otherwise, ignore it.

STRING and OBJECTS are as `format-message' (which see)."
  (declare (indent 1))
  (let ((kind-sym (make-symbol "kind")))
    `(let ((,kind-sym ,kind))
       (when (memq ,kind-sym eshell-debug-command)
         (eshell-always-debug-command ,kind-sym ,string ,@objects)))))

(defun eshell--mark-as-output (start end &optional object)
  "Mark the text from START to END as Eshell output.
OBJECT can be a buffer or string.  If nil, mark the text in the
current buffer."
  (with-silent-modifications
    (add-text-properties start end eshell-command-output-properties
                         object)))

(defun eshell--mark-yanked-as-output (start end)
  "Mark yanked text from START to END as Eshell output."
  ;; `yank' removes the field text property from the text it inserts
  ;; due to `yank-excluded-properties', so arrange for this text
  ;; property to be reapplied in the `after-change-functions'.
  (letrec ((hook
            (lambda (start1 end1 _len1)
              (remove-hook 'after-change-functions hook t)
              (when (and (= start start1)
                         (= end end1))
                (eshell--mark-as-output start1 end1)))))
    (add-hook 'after-change-functions hook nil t)))

(defun eshell--unmark-string-as-output (string)
  "Unmark STRING as Eshell output."
  (remove-list-of-text-properties
   0 (length string)
   '(rear-nonsticky front-sticky field insert-in-front-hooks)
   string)
  string)

(defsubst eshell--region-p (object)
  "Return non-nil if OBJECT is a pair of numbers or markers."
  (and (consp object)
       (number-or-marker-p (car object))
       (number-or-marker-p (cdr object))))

(defmacro eshell-with-temp-command (command &rest body)
  "Temporarily insert COMMAND into the buffer and execute the forms in BODY.

COMMAND can be a string to insert, a cons cell (START . END)
specifying a region in the current buffer, or (:file . FILENAME)
to temporarily insert the contents of FILENAME.

Before executing BODY, narrow the buffer to the text for COMMAND
and set point to the beginning of the narrowed region.

The value returned is the last form in BODY."
  (declare (indent 1))
  (let ((command-sym (make-symbol "command"))
        (begin-sym (make-symbol "begin"))
        (end-sym (make-symbol "end")))
    `(let ((,command-sym ,command))
       (if (eshell--region-p ,command-sym)
           (save-restriction
             (narrow-to-region (car ,command-sym) (cdr ,command-sym))
             (goto-char (car ,command-sym))
             ,@body)
         ;; Since parsing relies partly on buffer-local state
         ;; (e.g. that of `eshell-parse-argument-hook'), we need to
         ;; perform the parsing in the Eshell buffer.
         (let ((,begin-sym (point)) ,end-sym)
           (with-silent-modifications
             (if (stringp ,command-sym)
                 (insert ,command-sym)
               (forward-char (cadr (insert-file-contents (cdr ,command-sym)))))
             (setq ,end-sym (point))
             (unwind-protect
                 (save-restriction
                   (narrow-to-region ,begin-sym ,end-sym)
                   (goto-char ,begin-sym)
                   ,@body)
               (delete-region ,begin-sym ,end-sym))))))))

(defun eshell-find-delimiter
  (open close &optional bound reverse-p backslash-p)
  "From point, find the CLOSE delimiter corresponding to OPEN.
The matching is bounded by BOUND.  If REVERSE-P is non-nil,
process the region backwards.

If BACKSLASH-P is non-nil, or OPEN and CLOSE are different
characters, then a backslash can be used to escape a delimiter
(or another backslash).  Otherwise, the delimiter is escaped by
doubling it up."
  (save-excursion
    (let ((depth 1)
	  (bound (or bound (point-max))))
      (when (if reverse-p
                (eq (char-before) close)
              (eq (char-after) open))
        (forward-char (if reverse-p -1 1)))
      (while (and (> depth 0)
                  (funcall (if reverse-p #'> #'<) (point) bound))
        (let ((c (if reverse-p (char-before) (char-after))))
	  (cond ((and (not reverse-p)
		      (or (not (eq open close))
			  backslash-p)
		      (eq c ?\\)
                      (memq (char-after (1+ (point)))
                            (list open close ?\\)))
		 (forward-char 1))
		((and reverse-p
		      (or (not (eq open close))
			  backslash-p)
                      (eq (char-before (1- (point))) ?\\)
                      (memq c (list open close ?\\)))
		 (forward-char -1))
		((eq open close)
                 (when (eq c open)
                   (if (and (not backslash-p)
                            (eq (if reverse-p
                                    (char-before (1- (point)))
                                  (char-after (1+ (point))))
                                open))
                       (forward-char (if reverse-p -1 1))
                     (setq depth (1- depth)))))
		((= c open)
		 (setq depth (+ depth (if reverse-p -1 1))))
		((= c close)
		 (setq depth (+ depth (if reverse-p 1 -1))))))
	(forward-char (if reverse-p -1 1)))
      (when (= depth 0)
        (if reverse-p (point) (1- (point)))))))

(defun eshell-convertible-to-number-p (string)
  "Return non-nil if STRING can be converted to a number.
If `eshell-convert-numeric-arguments', always return nil."
  (and eshell-convert-numeric-arguments
       (string-match
        (concat "\\`\\s-*" eshell-number-regexp "\\s-*\\'")
        string)))

(defsubst eshell--do-mark-numeric-string (string)
  (put-text-property 0 (length string) 'number t string))

(defun eshell-mark-numeric-string (string)
  "If STRING is convertible to a number, add a text property indicating so.
See `eshell-convertible-to-number-p'."
  (when (eshell-convertible-to-number-p string)
    (eshell--do-mark-numeric-string string))
  string)

(defsubst eshell--numeric-string-p (string)
  "Return non-nil if STRING has been marked as numeric."
  (and (stringp string)
       (length> string 0)
       (not (text-property-not-all 0 (length string) 'number t string))))

(defun eshell-convert-to-number (string)
  "Try to convert STRING to a number.
If STRING doesn't look like a number (or
`eshell-convert-numeric-arguments' is nil), just return STRING
unchanged."
  (declare (obsolete 'eshell-mark-numeric-string "31.1"))
  (if (eshell-convertible-to-number-p string)
      (string-to-number string)
    string))

(cl-defstruct (eshell-range
               (:constructor nil)
               (:constructor eshell-range-create (begin end)))
  "A half-open range from BEGIN to END."
  begin end)

(defsubst eshell--range-string-p (string)
  "Return non-nil if STRING has been marked as a range."
  (and (stringp string)
       (text-property-any 0 (length string) 'eshell-range t string)))

(defun eshell--string-to-range (string)
  "Convert STRING to an `eshell-range' object."
  (let* ((startpos (text-property-any 0 (length string) 'eshell-range t string))
         (endpos (next-single-property-change startpos 'eshell-range
                                              string (length string)))
         range-begin range-end)
    (unless (= startpos 0)
      (setq range-begin (substring string 0 startpos))
      (unless (eshell--numeric-string-p range-begin)
        (user-error "range begin `%s' is not a number" range-begin))
      (setq range-begin (string-to-number range-begin)))
    (unless (= endpos (length string))
      (setq range-end (substring string endpos))
      (unless (eshell--numeric-string-p range-end)
        (user-error "range end `%s' is not a number" range-end))
      (setq range-end (string-to-number range-end)))
    (eshell-range-create range-begin range-end)))

(defun eshell-convert (string &optional to-string)
  "Convert STRING into a more-native Lisp object.
If TO-STRING is non-nil, always return a single string with
trailing newlines removed.  Otherwise, this behaves as follows:

* Return non-strings as-is.

* Split multiline strings by line.

* If `eshell-convert-numeric-arguments' is non-nil and every line
  of output looks like a number, convert them to numbers."
  (cond
   ((not (stringp string))
    (if to-string
        (eshell-stringify string t)
      string))
   (to-string (string-trim-right string "\n+"))
   (t (let ((len (length string)))
        (if (= len 0)
	    string
	  (when (eq (aref string (1- len)) ?\n)
	    (setq string (substring string 0 (1- len))))
          (if (string-search "\n" string)
              (let ((lines (split-string string "\n")))
                (when (seq-every-p #'eshell-convertible-to-number-p lines)
                  (mapc #'eshell--do-mark-numeric-string lines))
                lines)
            (eshell-mark-numeric-string string)))))))

(defvar-local eshell-path-env (getenv "PATH")
  "Content of $PATH.
It might be different from \(getenv \"PATH\"), when
`default-directory' points to a remote host.")

(make-obsolete-variable 'eshell-path-env 'eshell-get-path "29.1")

(defvar-local eshell-path-env-list nil)

(connection-local-set-profile-variables
 'eshell-connection-default-profile
 '((eshell-path-env-list . nil)))

(connection-local-set-profiles
 '(:application eshell)
 'eshell-connection-default-profile)

(defun eshell-get-path (&optional literal-p)
  "Return $PATH as a list.
If LITERAL-P is nil, return each directory of the path as a full,
possibly-remote file name; on MS-Windows, add the current
directory as the first directory in the path as well.

If LITERAL-P is non-nil, return the local part of each directory,
as the $PATH was actually specified."
  (with-connection-local-application-variables 'eshell
    (let ((remote (file-remote-p default-directory))
          (path
           (or eshell-path-env-list
               ;; If not already cached, get the path from
               ;; `exec-path', removing the last element, which is
               ;; `exec-directory'.
               (setq-connection-local eshell-path-env-list
                                      (butlast (exec-path))))))
      (when (and (not literal-p)
                 (not remote)
                 (eshell-under-windows-p)
                 (not (member "." path)))
        (push "." path))
      (if (and remote (not literal-p))
          (mapcar (lambda (x) (concat remote x)) path)
        path))))

(defun eshell-set-path (path)
  "Set the Eshell $PATH to PATH.
PATH can be either a list of directories or a string of
directories separated by `path-separator'."
  (with-connection-local-application-variables 'eshell
    (setq-connection-local
     eshell-path-env-list
     (if (listp path)
	 path
       ;; Don't use `parse-colon-path' here, since we don't want
       ;; the additional translations it does on each element.
       (split-string path (path-separator))))))

(defun eshell-parse-colon-path (path-env)
  "Split string with `parse-colon-path'.
Prepend remote identification of `default-directory', if any."
  (declare (obsolete nil "29.1"))
  (let ((remote (file-remote-p default-directory)))
    (if remote
	(mapcar
	 (lambda (x) (concat remote x))
	 (parse-colon-path path-env))
      (parse-colon-path path-env))))

(defun eshell-split-filename (filename)
  "Split a FILENAME into a list of file/directory components."
  (let* ((remote (file-remote-p filename))
         (filename (or (file-remote-p filename 'localname 'never) filename))
         (len (length filename))
         (index 0) (curr-start 0)
         parts)
    (when (and (eshell-under-windows-p)
               (string-prefix-p "//" filename))
      (setq index 2))
    (while (< index len)
      (when (eq (aref filename index) ?/)
        (push (if (= curr-start index) "/"
                (substring filename curr-start (1+ index)))
              parts)
        (setq curr-start (1+ index)))
      (setq index (1+ index)))
    (when (< curr-start len)
      (push (substring filename curr-start) parts))
    (setq parts (nreverse parts))
    (when (and (eshell-under-windows-p)
               (string-match "\\`[A-Za-z]:\\'" (car parts)))
      (setcar parts (concat (car parts) "/")))
    (if remote (cons remote parts) parts)))

(define-obsolete-function-alias 'eshell-split-path
  'eshell-split-filename "30.1")

(defun eshell-to-flat-string (value)
  "Make value a string.  If separated by newlines change them to spaces."
  (declare (obsolete nil "29.1"))
  (let ((text (eshell-stringify value)))
    (if (string-match "\n+\\'" text)
	(setq text (replace-match "" t t text)))
    (while (string-match "\n+" text)
      (setq text (replace-match " " t t text)))
    text))

(define-obsolete-function-alias 'eshell-flatten-list #'flatten-tree "27.1")

(defun eshell-stringify (object &optional quoted)
  "Convert OBJECT into a string value."
  (cond
   ((stringp object) object)
   ((numberp object)
    (if quoted
        (number-to-string object)
      (propertize (number-to-string object) 'number t)))
   ((and (eq object t)
	 (not eshell-stringify-t))
    nil)
   (t
    (string-trim-right (pp-to-string object)))))

(defsubst eshell-stringify-list (args &optional quoted)
  "Convert each element of ARGS into a string value."
  (mapcar (lambda (i) (eshell-stringify i quoted)) args))

(defsubst eshell-list-to-string (list)
  "Convert LIST into a single string separated by spaces."
  (mapconcat (lambda (i) (eshell-stringify i t)) list " "))

(defsubst eshell-flatten-and-stringify (&rest args)
  "Flatten and stringify all of the ARGS into a single string."
  (eshell-list-to-string (flatten-tree args)))

(defsubst eshell-directory-files (regexp &optional directory)
  "Return a list of files in the given DIRECTORY matching REGEXP."
  (directory-files (or directory default-directory)
		   directory regexp))

(defun eshell-regexp-arg (prompt)
  "Return list of regexp and prefix arg using PROMPT."
  (let* (;; Don't clobber this.
	 (last-command last-command)
	 (regexp (read-from-minibuffer prompt nil nil nil
				       'minibuffer-history-search-history)))
    (list (if (string-equal regexp "")
	      (setcar minibuffer-history-search-history
		      (nth 1 minibuffer-history-search-history))
	    regexp)
	  (prefix-numeric-value current-prefix-arg))))

(defun eshell-printable-size (filesize &optional human-readable
				       block-size use-colors)
  "Return a printable FILESIZE."
  (when (and human-readable
             (not (= human-readable 1000))
             (not (= human-readable 1024)))
    (error "human-readable must be 1000 or 1024"))
  (let ((size (float (or filesize 0))))
    (if human-readable
        (let* ((flavor (and (= human-readable 1000) 'si))
               (str (file-size-human-readable size flavor)))
          (if (not use-colors)
              str
            (cond ((> size (expt human-readable 3))
                   (propertize str 'face 'bold-italic))
                  ((> size (expt human-readable 2))
                   (propertize str 'face 'bold))
                  (t str))))
      (if block-size
	  (setq size (/ size block-size)))
      (format "%.0f" size))))

(defun eshell-winnow-list (entries exclude &optional predicates)
  "Pare down the ENTRIES list using the EXCLUDE regexp, and PREDICATES.
The original list is not affected.  If the result is only one element
long, it will be returned itself, rather than returning a one-element
list."
  (let ((flist (list t))
	valid p listified)
    (unless (listp entries)
      (setq entries (list entries)
	    listified t))
    (dolist (entry entries)
      (unless (and exclude (string-match exclude entry))
	(setq p predicates valid (null p))
	(while p
	  (if (funcall (car p) entry)
	      (setq valid t)
	    (setq p nil valid nil))
	  (setq p (cdr p)))
	(when valid
	  (nconc flist (list entry)))))
    (if listified
	(cadr flist)
      (cdr flist))))

(defun eshell-user-login-name ()
  "Return the connection-aware value of the user's login name.
See also `user-login-name'."
  (or (file-remote-p default-directory 'user) (user-login-name)))

(defun eshell-read-passwd-file (file)
  "Return an alist correlating gids to group names in FILE."
  (let (names)
    (when (file-readable-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((fields
		  (split-string (buffer-substring
				 (point) (progn (end-of-line)
						(point))) ":")))
	    (if (and (and fields (nth 0 fields) (nth 2 fields))
		     (not (assq (string-to-number (nth 2 fields)) names)))
		(setq names (cons (cons (string-to-number (nth 2 fields))
					(nth 0 fields))
				  names))))
	  (forward-line))))
    names))

(defun eshell-read-passwd (file result-var timestamp-var)
  "Read the contents of /etc/passwd for user names."
  (if (or (not (symbol-value result-var))
	  (not (symbol-value timestamp-var))
	  (time-less-p
	   (symbol-value timestamp-var)
	   (file-attribute-modification-time (file-attributes file))))
      (progn
	(set result-var (eshell-read-passwd-file file))
	(set timestamp-var (current-time))))
  (symbol-value result-var))

(defun eshell-read-group-names ()
  "Read the contents of /etc/group for group names."
  (if eshell-group-file
      (eshell-read-passwd eshell-group-file 'eshell-group-names
			  'eshell-group-timestamp)))

(defsubst eshell-group-id (name)
  "Return the user id for user NAME."
  (car (rassoc name (eshell-read-group-names))))

(defsubst eshell-group-name (gid)
  "Return the group name for the given GID."
  (cdr (assoc gid (eshell-read-group-names))))

(defun eshell-read-user-names ()
  "Read the contents of /etc/passwd for user names."
  (if eshell-passwd-file
      (eshell-read-passwd eshell-passwd-file 'eshell-user-names
			  'eshell-user-timestamp)))

(defsubst eshell-user-id (name)
  "Return the user id for user NAME."
  (car (rassoc name (eshell-read-user-names))))

(autoload 'pcomplete-read-hosts-file "pcomplete")
(autoload 'pcomplete-read-hosts "pcomplete")
(autoload 'pcomplete-read-host-names "pcomplete")
(define-obsolete-function-alias 'eshell-read-hosts-file
  #'pcomplete-read-hosts-file "28.1")
(define-obsolete-function-alias 'eshell-read-hosts
  #'pcomplete-read-hosts "28.1")
(define-obsolete-function-alias 'eshell-read-host-names
  #'pcomplete-read-host-names "28.1")

(defsubst eshell-copy-environment ()
  "Return an unrelated copy of `process-environment'."
  (mapcar #'concat process-environment))

(defun eshell-subgroups (groupsym)
  "Return all of the subgroups of GROUPSYM."
  (let ((subgroups (get groupsym 'custom-group))
	(subg (list t)))
    (while subgroups
      (if (eq (cadr (car subgroups)) 'custom-group)
	  (nconc subg (list (caar subgroups))))
      (setq subgroups (cdr subgroups)))
    (cdr subg)))

(defmacro eshell-with-file-modes (modes &rest forms)
  "Evaluate, with file-modes set to MODES, the given FORMS."
  (declare (obsolete with-file-modes "25.1"))
  `(with-file-modes ,modes ,@forms))

(defmacro eshell-with-private-file-modes (&rest forms)
  "Evaluate FORMS with private file modes set."
  `(with-file-modes ,eshell-private-file-modes ,@forms))

(defsubst eshell-make-private-directory (dir &optional parents)
  "Make DIR with file-modes set to `eshell-private-directory-modes'."
  (with-file-modes eshell-private-directory-modes
    (make-directory dir parents)))

(defsubst eshell-substring (string sublen)
  "Return the beginning of STRING, up to SUBLEN bytes."
  (if string
      (if (> (length string) sublen)
	  (substring string 0 sublen)
	string)))

(defun eshell-directory-files-and-attributes (dir &optional full match nosort id-format)
  "Make sure to use the handler for `directory-files-and-attributes'."
  (let* ((dir (expand-file-name dir)))
    (if (string-equal (file-remote-p dir 'method) "ftp")
	(let ((files (directory-files dir full match nosort)))
	  (mapcar
	   (lambda (file)
	     (cons file (eshell-file-attributes (expand-file-name file dir))))
	   files))
      (directory-files-and-attributes dir full match nosort id-format))))

(defun eshell-current-ange-uids ()
  (if (string-match "/\\([^@]+\\)@\\([^:]+\\):" default-directory)
      (let* ((host (match-string 2 default-directory))
	     (user (match-string 1 default-directory))
	     (host-users (assoc host eshell-ange-ls-uids)))
	(when host-users
	  (setq host-users (cdr host-users))
	  (cdr (assoc user host-users))))))

(eval-when-compile
  (require 'ange-ftp))		; ange-ftp-parse-filename

(defvar tramp-file-name-structure)
(declare-function ange-ftp-ls "ange-ftp"
		  (file lsargs parse &optional no-error wildcard))
(declare-function ange-ftp-file-modtime "ange-ftp" (file))

(defun eshell-parse-ange-ls (dir)
  (require 'ange-ftp)
  (require 'tramp)
  (let ((ange-ftp-name-format
	 (list (nth 0 tramp-file-name-structure)
	       (nth 3 tramp-file-name-structure)
	       (nth 2 tramp-file-name-structure)
	       (nth 4 tramp-file-name-structure)))
	;; ange-ftp uses `ange-ftp-ftp-name-arg' and `ange-ftp-ftp-name-res'
	;; for optimization in `ange-ftp-ftp-name'. If Tramp wasn't active,
	;; there could be incorrect values from previous calls in case the
	;; "ftp" method is used in the Tramp file name. So we unset
	;; those values.
	(ange-ftp-ftp-name-arg "")
	(ange-ftp-ftp-name-res nil)
	entry)
    (with-temp-buffer
      (insert (ange-ftp-ls dir "-la" nil))
      (goto-char (point-min))
      (if (looking-at "^total [0-9]+$")
	  (forward-line 1))
      ;; Some systems put in a blank line here.
      (if (eolp) (forward-line 1))
      (while (looking-at
	      `,(concat "\\([dlscb-][rwxst-]+\\)"
			"\\s-*" "\\([0-9]+\\)" "\\s-+"
			"\\(\\S-+\\)" "\\s-+"
			"\\(\\S-+\\)" "\\s-+"
			"\\([0-9]+\\)" "\\s-+" "\\(.*\\)"))
	(let* ((perms (match-string 1))
	       (links (string-to-number (match-string 2)))
	       (user (match-string 3))
	       (group (match-string 4))
	       (size (string-to-number (match-string 5)))
	       (name (ange-ftp-parse-filename))
	       (mtime
		(let ((moment (parse-time-string (match-string 6))))
		  (if (decoded-time-second moment)
		      (setf (decoded-time-year moment)
			    (decoded-time-year (decode-time)))
		    (setf (decoded-time-second moment) 0)
		    (setf (decoded-time-minute moment) 0)
                    (setf (decoded-time-hour moment) 0))
		  (encode-time moment)))
	       symlink)
	  (if (string-match "\\(.+\\) -> \\(.+\\)" name)
	      (setq symlink (match-string 2 name)
		    name (match-string 1 name)))
	  (setq entry
		(cons
		 (cons name
		       (list (if (eq (aref perms 0) ?d)
				 t
			       symlink)
			     links user group
			     nil mtime nil
			     size perms nil nil)) entry)))
	(forward-line)))
    entry))

(defun eshell-file-attributes (file &optional id-format)
  "Return the attributes of FILE, playing tricks if it's over ange-ftp.
The optional argument ID-FORMAT specifies the preferred uid and
gid format.  Valid values are `string' and `integer', defaulting to
`integer'.  See `file-attributes'."
  (let* ((expanded-file (expand-file-name file))
	 entry)
    (if (string-equal (file-remote-p expanded-file 'method) "ftp")
	(let ((base (file-name-nondirectory expanded-file))
	      (dir (file-name-directory expanded-file)))
	  (if (string-equal "" base) (setq base "."))
	  (unless entry
	    (setq entry (eshell-parse-ange-ls dir))
	    (if entry
		(let ((fentry (assoc base (cdr entry))))
		  (if fentry
		      (setq entry (cdr fentry))
		    (setq entry nil)))))
	  entry)
      (file-attributes file id-format))))

(defsubst eshell-processp (proc)
  "If the `processp' function does not exist, PROC is not a process."
  (and (fboundp 'processp) (processp proc)))

(defun eshell-process-list-p (procs)
  "Return non-nil if PROCS is a list of process objects."
  (and (listp procs)
       (seq-every-p #'eshell-processp procs)))

(defun eshell-make-process-list (procs)
  "Make a list of process objects from PROCS if possible.
PROCS can be a single process or a list thereof.  If PROCS is
anything else, return nil instead."
  (pcase procs
    ((pred eshell-processp) (list procs))
    ((pred eshell-process-list-p) procs)))

;; (defun eshell-copy-file
;;   (file newname &optional ok-if-already-exists keep-date)
;;   "Copy FILE to NEWNAME.  See docs for `copy-file'."
;;   (let (copied)
;;     (if (string-match "\\`\\([^:]+\\):\\(.*\\)" file)
;;	(let ((front (match-string 1 file))
;;	      (back (match-string 2 file))
;;	      buffer)
;;	  (if (and front (string-match eshell-tar-regexp front)
;;		     (setq buffer (find-file-noselect front)))
;;	    (with-current-buffer buffer
;;	      (goto-char (point-min))
;;	      (if (re-search-forward (concat " " (regexp-quote back)
;;					     "$") nil t)
;;		  (progn
;;		    (tar-copy (if (file-directory-p newname)
;;				  (expand-file-name
;;				   (file-name-nondirectory back) newname)
;;				newname))
;;		    (setq copied t))
;;		(error "%s not found in tar file %s" back front))))))
;;     (unless copied
;;       (copy-file file newname ok-if-already-exists keep-date))))

;; (defun eshell-file-attributes (filename)
;;   "Return a list of attributes of file FILENAME.
;; See the documentation for `file-attributes'."
;;   (let (result)
;;     (when (string-match "\\`\\([^:]+\\):\\(.*\\)\\'" filename)
;;       (let ((front (match-string 1 filename))
;;	    (back (match-string 2 filename))
;;	    buffer)
;;	(when (and front (string-match eshell-tar-regexp front)
;;		   (setq buffer (find-file-noselect front)))
;;	  (with-current-buffer buffer
;;	    (goto-char (point-min))
;;	    (when (re-search-forward (concat " " (regexp-quote back)
;;					     "\\s-*$") nil t)
;;	      (let* ((descrip (tar-current-descriptor))
;;		     (tokens (tar-desc-tokens descrip)))
;;		(setq result
;;		      (list
;;		       (cond
;;			((eq (tar-header-link-type tokens) 5)
;;			 t)
;;			((eq (tar-header-link-type tokens) t)
;;			 (tar-header-link-name tokens)))
;;		       1
;;		       (tar-header-uid tokens)
;;		       (tar-header-gid tokens)
;;		       (tar-header-date tokens)
;;		       (tar-header-date tokens)
;;		       (tar-header-date tokens)
;;		       (tar-header-size tokens)
;;		       (file-modes-number-to-symbolic
;;                       (logior (tar-header-mode tokens)
;;			        (cond
;;			         ((eq (tar-header-link-type tokens) 5) 16384)
;;			         ((eq (tar-header-link-type tokens) t) 32768))))
;;		       nil nil nil))))))))
;;     (or result
;;	(file-attributes filename))))

;; Obsolete.

(define-obsolete-function-alias 'eshell-uniquify-list #'seq-uniq "28.1")
(define-obsolete-function-alias 'eshell-uniqify-list #'seq-uniq "28.1")
(define-obsolete-function-alias 'eshell-copy-tree #'copy-tree "28.1")
(define-obsolete-function-alias 'eshell-user-name #'user-login-name "28.1")

(defun eshell-sublist (l &optional n m)
  "Return from LIST the N to M elements.
If N or M is nil, it means the end of the list."
  (declare (obsolete seq-subseq "28.1"))
  (seq-subseq l n (1+ m)))

(define-obsolete-function-alias 'eshell-redisplay #'redisplay "30.1")

(provide 'esh-util)

;;; esh-util.el ends here
