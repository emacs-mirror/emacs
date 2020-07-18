;;; bookmark.el --- set bookmarks, maybe annotate them, jump to them later -*- lexical-binding: t -*-

;; Copyright (C) 1993-1997, 2001-2020 Free Software Foundation, Inc.

;; Author: Karl Fogel <kfogel@red-bean.com>
;; Created: July, 1993
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

;; This package is for setting "bookmarks" in files.  A bookmark
;; associates a string with a location in a certain file.  Thus, you
;; can navigate your way to that location by providing the string.
;; See the "User Variables" section for customizations.


;;; Code:

(require 'pp)
(require 'text-property-search)
(eval-when-compile (require 'cl-lib))

;;; Misc comments:
;;
;; The bookmark list is sorted lexically by default, but you can turn
;; this off by setting bookmark-sort-flag to nil.  If it is nil, then
;; the list will be presented in the order it is recorded
;; (chronologically), which is actually fairly useful as well.

;;; User Variables

(defgroup bookmark nil
  "Setting, annotation and jumping to bookmarks."
  :group 'matching)


(defcustom bookmark-use-annotations nil
  "If non-nil, setting a bookmark queries for an annotation in a buffer."
  :type 'boolean)


(defcustom bookmark-save-flag t
  "Controls when Emacs saves bookmarks to a file.
--> nil means never save bookmarks, except when `bookmark-save' is
    explicitly called (\\[bookmark-save]).
--> t means save bookmarks when Emacs is killed.
--> Otherwise, it should be a number that is the frequency with which
    the bookmark list is saved (i.e.: the number of times which
    Emacs's bookmark list may be modified before it is automatically
    saved.).  If it is a number, Emacs will also automatically save
    bookmarks when it is killed.

Therefore, the way to get it to save every time you make or delete a
bookmark is to set this variable to 1 (or 0, which produces the same
behavior.)

To specify the file in which to save them, modify the variable
`bookmark-default-file'."
  :type '(choice (const nil) integer (other t)))


(define-obsolete-variable-alias 'bookmark-old-default-file
  'bookmark-default-file "27.1")
(define-obsolete-variable-alias 'bookmark-file 'bookmark-default-file "27.1")
(defcustom bookmark-default-file
  (locate-user-emacs-file "bookmarks" ".emacs.bmk")
  "File in which to save bookmarks by default."
  ;; The current default file is defined via the internal variable
  ;; `bookmark-bookmarks-timestamp'.  This does not affect the value
  ;; of `bookmark-default-file'.
  :type 'file)

(defcustom bookmark-watch-bookmark-file t
  "If non-nil watch the default bookmark file.
If this file has changed on disk since it was last loaded, query the user
whether to load it again.  If the value is `silent' reload without querying.
This file defaults to `bookmark-default-file'.  But during an Emacs session,
`bookmark-load' and `bookmark-save' can redefine the current default file."
  :version "27.1"
  :type 'boolean
  :group 'bookmark)

(defcustom bookmark-version-control 'nospecial
  "Whether or not to make numbered backups of the bookmark file.
It can have four values: t, nil, `never', or `nospecial'.
The first three have the same meaning that they do for the
variable `version-control'; the value `nospecial' (the default) means
just use the value of `version-control'."
  :type '(choice (const :tag "If existing" nil)
                 (const :tag "Never" never)
                 (const :tag "Use value of option `version-control'" nospecial)
                 (other :tag "Always" t)))


(defcustom bookmark-completion-ignore-case t
  "Non-nil means bookmark functions ignore case in completion."
  :type 'boolean)


(defcustom bookmark-sort-flag t
  "Non-nil means that bookmarks will be displayed sorted by bookmark name.
Otherwise they will be displayed in LIFO order (that is, most
recently set ones come first, oldest ones come last)."
  :type 'boolean)


(defcustom bookmark-automatically-show-annotations t
  "Non-nil means show annotations when jumping to a bookmark."
  :type 'boolean)

(defconst bookmark-bmenu-buffer "*Bookmark List*"
  "Name of buffer used for Bookmark List.")

(defcustom bookmark-bmenu-use-header-line t
  "Non-nil means to use an immovable header line.
This is as opposed to inline text at the top of the buffer."
  :version "24.4"
  :type 'boolean)

(defconst bookmark-bmenu-inline-header-height 2
  "Number of lines used for the *Bookmark List* header.
\(This is only significant when `bookmark-bmenu-use-header-line'
is nil.)")

(defconst bookmark-bmenu-marks-width 2
  "Number of columns (chars) used for the *Bookmark List* marks column.
This includes the annotations column.")

(defcustom bookmark-bmenu-file-column 30
  "Column at which to display filenames in a buffer listing bookmarks.
You can toggle whether files are shown with \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-toggle-filenames]."
  :type 'integer)


(defcustom bookmark-bmenu-toggle-filenames t
  "Non-nil means show filenames when listing bookmarks.
A non-nil value may result in truncated bookmark names."
  :type 'boolean)

(defface bookmark-menu-bookmark
  '((t (:weight bold)))
  "Face used to highlight bookmark names in bookmark menu buffers.")

(defcustom bookmark-menu-length 70
  "Maximum length of a bookmark name displayed on a popup menu."
  :type 'integer)

;; FIXME: Is it really worth a customization option?
(defcustom bookmark-search-delay 0.2
  "Time before `bookmark-bmenu-search' updates the display."
  :type  'number)

(defface bookmark-menu-heading
  '((t (:inherit font-lock-type-face)))
  "Face used to highlight the heading in bookmark menu buffers."
  :version "22.1")


;;; No user-serviceable parts beyond this point.


;;; Keymap stuff:

;; Set up these bindings dumping time *only*;
;; if the user alters them, don't override the user when loading bookmark.el.

;;;###autoload (define-key ctl-x-r-map "b" 'bookmark-jump)
;;;###autoload (define-key ctl-x-r-map "m" 'bookmark-set)
;;;###autoload (define-key ctl-x-r-map "M" 'bookmark-set-no-overwrite)
;;;###autoload (define-key ctl-x-r-map "l" 'bookmark-bmenu-list)

;;;###autoload
(defvar bookmark-map
  (let ((map (make-sparse-keymap)))
    ;; Read the help on all of these functions for details...
    (define-key map "x" 'bookmark-set)
    (define-key map "m" 'bookmark-set) ;"m"ark
    (define-key map "M" 'bookmark-set-no-overwrite) ;"M"aybe mark
    (define-key map "j" 'bookmark-jump)
    (define-key map "g" 'bookmark-jump) ;"g"o
    (define-key map "o" 'bookmark-jump-other-window)
    (define-key map "5" 'bookmark-jump-other-frame)
    (define-key map "i" 'bookmark-insert)
    (define-key map "e" 'edit-bookmarks)
    (define-key map "f" 'bookmark-insert-location) ;"f"ind
    (define-key map "r" 'bookmark-rename)
    (define-key map "d" 'bookmark-delete)
    (define-key map "l" 'bookmark-load)
    (define-key map "w" 'bookmark-write)
    (define-key map "s" 'bookmark-save)
    map)
  "Keymap containing bindings to bookmark functions.
It is not bound to any key by default: to bind it
so that you have a bookmark prefix, just use `global-set-key' and bind a
key of your choice to variable `bookmark-map'.  All interactive bookmark
functions have a binding in this keymap.")

;;;###autoload (fset 'bookmark-map bookmark-map)


;;; Core variables and data structures:
(defvar bookmark-alist ()
  "Association list of bookmark names and their parameters.
Bookmark functions update the value automatically.
You probably do NOT want to change the value yourself.

The value is an alist with bookmarks of the form

 (BOOKMARK-NAME . PARAM-ALIST)

or the deprecated form (BOOKMARK-NAME PARAM-ALIST).

BOOKMARK-NAME is the name you gave to the bookmark when creating it.

PARAM-ALIST is an alist of bookmark information.  The order of the
entries in PARAM-ALIST is not important.  The default entries are
described below.  An entry with a key but null value means the entry
is not used.

 (filename . FILENAME)
 (buf . BUFFER-OR-NAME)
 (position . POS)
 (front-context-string . STR-AFTER-POS)
 (rear-context-string  . STR-BEFORE-POS)
 (handler . HANDLER)
 (annotation . ANNOTATION)

FILENAME names the bookmarked file.
BUFFER-OR-NAME is a buffer or the name of a buffer that is used
  if FILENAME is not defined or it refers to a non-existent file.
POS is the bookmarked buffer position.
STR-AFTER-POS is buffer text that immediately follows POS.
STR-BEFORE-POS is buffer text that immediately precedes POS.
ANNOTATION is a string that describes the bookmark.
  See options `bookmark-use-annotations' and
  `bookmark-automatically-show-annotations'.
HANDLER is a function that provides the bookmark-jump behavior for a
specific kind of bookmark instead of the default `bookmark-default-handler'.
This is the case for Info bookmarks, for instance.  HANDLER must accept
a bookmark as its single argument.

A function `bookmark-make-record-function' may define additional entries
in PARAM-LIST that can be used by HANDLER.")

(define-obsolete-variable-alias 'bookmarks-already-loaded
  'bookmark-bookmarks-timestamp "27.1")
(defvar bookmark-bookmarks-timestamp nil
  "Timestamp of current default bookmark file.
The value is actually (FILE . MODTIME), where FILE is a bookmark file that
defaults to `bookmark-default-file' and MODTIME is its modification time.")

(defvar bookmark-file-coding-system nil
  "The coding-system of the last loaded or saved bookmark file.")

(defvar bookmark-current-bookmark nil
  "Name of bookmark most recently used in the current file.
It is buffer local, used to make moving a bookmark forward
through a file easier.")

(make-variable-buffer-local 'bookmark-current-bookmark)


(defvar bookmark-alist-modification-count 0
  "Number of modifications to bookmark list since it was last saved.")


(defvar bookmark-search-size 16
  "Length of the context strings recorded on either side of a bookmark.")


(defvar bookmark-current-buffer nil
  "The buffer in which a bookmark is currently being set or renamed.
Functions that insert strings into the minibuffer use this to know
the source buffer for that information; see `bookmark-yank-word'
for example.")


(defvar bookmark-yank-point 0
  "The next point from which to pull source text for `bookmark-yank-word'.
This point is in `bookmark-current-buffer'.")


(defvar bookmark-quit-flag nil
  "Non-nil means `bookmark-bmenu-search' quits immediately.")
(make-obsolete-variable 'bookmark-quit-flag "no longer used" "27.1")


;; Helper functions and macros.

(defmacro with-buffer-modified-unmodified (&rest body)
  "Run BODY while preserving the buffer's `buffer-modified-p' state."
  (let ((was-modified (make-symbol "was-modified")))
    `(let ((,was-modified (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (set-buffer-modified-p ,was-modified)))))

;; Only functions below, in this page and the next one (file formats),
;; need to know anything about the format of bookmark-alist entries.
;; Everyone else should go through them.

(defun bookmark-name-from-full-record (bookmark-record)
  "Return the name of BOOKMARK-RECORD.
BOOKMARK-RECORD is, e.g., one element from `bookmark-alist'."
  (car bookmark-record))


(defun bookmark-all-names ()
  "Return a list of all current bookmark names."
  (bookmark-maybe-load-default-file)
  (mapcar 'bookmark-name-from-full-record bookmark-alist))


(defun bookmark-get-bookmark (bookmark-name-or-record &optional noerror)
  "Return the bookmark record corresponding to BOOKMARK-NAME-OR-RECORD.
If BOOKMARK-NAME-OR-RECORD is a string, look for the corresponding
bookmark record in `bookmark-alist'; return it if found, otherwise
error.  If optional argument NOERROR is non-nil, return nil
instead of signaling an error.  Else if BOOKMARK-NAME-OR-RECORD
is already a bookmark record, just return it."
  (cond
   ((consp bookmark-name-or-record) bookmark-name-or-record)
   ((stringp bookmark-name-or-record)
    (or (assoc-string bookmark-name-or-record bookmark-alist
                      bookmark-completion-ignore-case)
        (unless noerror (error "Invalid bookmark %s"
                               bookmark-name-or-record))))))


(defun bookmark-get-bookmark-record (bookmark-name-or-record)
  "Return the record portion of BOOKMARK-NAME-OR-RECORD in `bookmark-alist'.
In other words, return all information but the name."
  (let ((alist (cdr (bookmark-get-bookmark bookmark-name-or-record))))
    ;; The bookmark objects can either look like (NAME ALIST) or
    ;; (NAME . ALIST), so we have to distinguish the two here.
    (if (and (null (cdr alist)) (consp (caar alist)))
        (car alist) alist)))


(defun bookmark-set-name (bookmark-name-or-record newname)
  "Set BOOKMARK-NAME-OR-RECORD's name to NEWNAME."
  (setcar (bookmark-get-bookmark bookmark-name-or-record) newname))

(defun bookmark-prop-get (bookmark-name-or-record prop)
  "Return the property PROP of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (cdr (assq prop (bookmark-get-bookmark-record bookmark-name-or-record))))

(defun bookmark-prop-set (bookmark-name-or-record prop val)
  "Set the property PROP of BOOKMARK-NAME-OR-RECORD to VAL."
  (let ((cell (assq
               prop (bookmark-get-bookmark-record bookmark-name-or-record))))
    (if cell
        (setcdr cell val)
      (nconc (bookmark-get-bookmark-record bookmark-name-or-record)
             (list (cons prop val))))))

(defun bookmark-get-annotation (bookmark-name-or-record)
  "Return the annotation of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'annotation))

(defun bookmark-set-annotation (bookmark-name-or-record ann)
  "Set the annotation of BOOKMARK-NAME-OR-RECORD to ANN."
  (bookmark-prop-set bookmark-name-or-record 'annotation ann))


(defun bookmark-get-filename (bookmark-name-or-record)
  "Return the full filename of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'filename))


(defun bookmark-set-filename (bookmark-name-or-record filename)
  "Set the full filename of BOOKMARK-NAME-OR-RECORD to FILENAME."
  (bookmark-prop-set bookmark-name-or-record 'filename filename))


(defun bookmark-get-position (bookmark-name-or-record)
  "Return the position (i.e.: point) of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'position))


(defun bookmark-set-position (bookmark-name-or-record position)
  "Set the position (i.e.: point) of BOOKMARK-NAME-OR-RECORD to POSITION."
  (bookmark-prop-set bookmark-name-or-record 'position position))


(defun bookmark-get-front-context-string (bookmark-name-or-record)
  "Return the front-context-string of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'front-context-string))


(defun bookmark-set-front-context-string (bookmark-name-or-record string)
  "Set the front-context-string of BOOKMARK-NAME-OR-RECORD to STRING."
  (bookmark-prop-set bookmark-name-or-record 'front-context-string string))


(defun bookmark-get-rear-context-string (bookmark-name-or-record)
  "Return the rear-context-string of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'rear-context-string))


(defun bookmark-set-rear-context-string (bookmark-name-or-record string)
  "Set the rear-context-string of BOOKMARK-NAME-OR-RECORD to STRING."
  (bookmark-prop-set bookmark-name-or-record 'rear-context-string string))


(defun bookmark-get-handler (bookmark-name-or-record)
  "Return the handler function for BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'handler))

(defvar bookmark-history nil
  "The history list for bookmark functions.")


(defun bookmark-completing-read (prompt &optional default)
  "Prompting with PROMPT, read a bookmark name in completion.
PROMPT will get a \": \" stuck on the end no matter what, so you
probably don't want to include one yourself.
Optional arg DEFAULT is a string to return if the user input is empty.
If DEFAULT is nil then return empty string for empty input."
  (bookmark-maybe-load-default-file) ; paranoia
  (if (listp last-nonmenu-event)
      (bookmark-menu-popup-paned-menu t prompt
				      (if bookmark-sort-flag
					  (sort (bookmark-all-names)
						'string-lessp)
					(bookmark-all-names)))
    (let* ((completion-ignore-case bookmark-completion-ignore-case)
           (default (unless (equal "" default) default))
	   (prompt (concat prompt (if default
                                      (format " (%s): " default)
                                    ": "))))
      (completing-read prompt
                       (lambda (string pred action)
                         (if (eq action 'metadata)
                             '(metadata (category . bookmark))
                             (complete-with-action
                              action bookmark-alist string pred)))
                       nil 0 nil 'bookmark-history default))))


(defmacro bookmark-maybe-historicize-string (string)
  "Put STRING into the bookmark prompt history, if caller non-interactive.
We need this because sometimes bookmark functions are invoked from
menus, so `completing-read' never gets a chance to set `bookmark-history'."
  `(or
    (called-interactively-p 'interactive)
    (setq bookmark-history (cons ,string bookmark-history))))

(defvar bookmark-make-record-function 'bookmark-make-record-default
  "A function that should be called to create a bookmark record.
Modes may set this variable buffer-locally to enable bookmarking of
locations that should be treated specially, such as Info nodes,
news posts, images, pdf documents, etc.

The function will be called with no arguments.
It should signal a user error if it is unable to construct a record for
the current location.

The returned record should be a cons cell of the form (NAME . ALIST)
where ALIST is as described in `bookmark-alist' and may typically contain
a special cons (handler . HANDLER-FUNC) which specifies the handler function
that should be used instead of `bookmark-default-handler' to open this
bookmark.  See the documentation for `bookmark-alist' for more.

NAME is a suggested name for the constructed bookmark.  It can be nil
in which case a default heuristic will be used.  The function can also
equivalently just return ALIST without NAME.")

(defun bookmark-make-record ()
  "Return a new bookmark record (NAME . ALIST) for the current location."
  (let ((record (funcall bookmark-make-record-function)))
    ;; Set up default name if the function does not provide one.
    (unless (stringp (car record))
      (if (car record) (push nil record))
      (setcar record (or bookmark-current-bookmark (bookmark-buffer-name))))
    ;; Set up defaults.
    (bookmark-prop-set
     record 'defaults
     (delq nil (delete-dups (append (bookmark-prop-get record 'defaults)
				    (list bookmark-current-bookmark
					  (car record)
                                          (bookmark-buffer-name))))))
    record))

(defun bookmark-store (name alist no-overwrite)
  "Store the bookmark NAME with data ALIST.
If NO-OVERWRITE is non-nil and another bookmark of the same name already
exists in `bookmark-alist', record the new bookmark without throwing away the
old one."
  (bookmark-maybe-load-default-file)
  (let ((stripped-name (copy-sequence name)))
    (set-text-properties 0 (length stripped-name) nil stripped-name)
    (if (and (not no-overwrite)
             (bookmark-get-bookmark stripped-name 'noerror))
        ;; already existing bookmark under that name and
        ;; no prefix arg means just overwrite old bookmark
        ;; Use the new (NAME . ALIST) format.
        (setcdr (bookmark-get-bookmark stripped-name) alist)

      ;; otherwise just cons it onto the front (either the bookmark
      ;; doesn't exist already, or there is no prefix arg.  In either
      ;; case, we want the new bookmark consed onto the alist...)

      (push (cons stripped-name alist) bookmark-alist))

    ;; Added by db
    (setq bookmark-current-bookmark stripped-name)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (bookmark-save))

    (setq bookmark-current-bookmark stripped-name)
    (bookmark-bmenu-surreptitiously-rebuild-list)))

(defun bookmark-make-record-default (&optional no-file no-context posn)
  "Return the record describing the location of a new bookmark.
Point should be at the buffer in which the bookmark is being set,
and normally should be at the position where the bookmark is desired,
but see the optional arguments for other possibilities.

If NO-FILE is non-nil, then only return the subset of the
record that pertains to the location within the buffer, leaving off
the part that records the filename.

If NO-CONTEXT is non-nil, do not include the front- and rear-context
strings in the record -- the position is enough.

If POSN is non-nil, record POSN as the point instead of `(point)'."
  `(,@(unless no-file `((filename . ,(bookmark-buffer-file-name))))
    ,@(unless no-context `((front-context-string
                           . ,(if (>= (- (point-max) (point))
                                      bookmark-search-size)
                                  (buffer-substring-no-properties
                                   (point)
                                   (+ (point) bookmark-search-size))
                                  nil))))
    ,@(unless no-context `((rear-context-string
                           . ,(if (>= (- (point) (point-min))
                                      bookmark-search-size)
                                  (buffer-substring-no-properties
                                   (point)
                                   (- (point) bookmark-search-size))
                                  nil))))
    (position . ,(or posn (point)))))


;;; File format stuff

;; *IMPORTANT NOTICE* If you are thinking about modifying (redefining)
;; the bookmark file format -- please don't.  The current format
;; should be extensible enough.  If you feel the need to change it,
;; please discuss it with other Emacs developers first.
;;
;; The format of `bookmark-alist' has changed twice in its lifetime.
;; This comment describes the three formats, FIRST, SECOND, and
;; CURRENT.
;;
;; The FIRST format was used prior to Emacs 20:
;;
;;       ((BOOKMARK-NAME (FILENAME
;;                          STRING-IN-FRONT
;;                          STRING-BEHIND
;;                          POINT))
;;        ...)
;;
;; The SECOND format was introduced in Emacs 20:
;;
;;       ((BOOKMARK-NAME ((filename   . FILENAME)
;;                        (position   . POS)
;;                        (front-context-string . STR-AFTER-POS)
;;                        (rear-context-string  . STR-BEFORE-POS)
;;                        (annotation . ANNOTATION)
;;                        (whatever   . VALUE)
;;                        ...
;;                       ))
;;        ...)
;;
;; The CURRENT format was introduced in Emacs 22:
;;
;;       ((BOOKMARK-NAME (filename   . FILENAME)
;;                       (position   . POS)
;;                       (front-context-string . STR-AFTER-POS)
;;                       (rear-context-string  . STR-BEFORE-POS)
;;                       (annotation . ANNOTATION)
;;                       (whatever   . VALUE)
;;                       ...
;;                       )
;;        ...)
;;
;; Both FIRST and SECOND have the same level of nesting: the cadr of a
;; bookmark record is a list of entry information.  FIRST and SECOND
;; differ in the form of the record information: FIRST uses a list of
;; atoms, and SECOND uses an alist.  In the FIRST format, the order of
;; the list elements matters.  In the SECOND format, the order of the
;; alist elements is unimportant.  The SECOND format facilitates the
;; addition of new kinds of elements, to support new kinds of
;; bookmarks or code evolution.
;;
;; The CURRENT format removes a level of nesting wrt FIRST and SECOND,
;; saving one cons cell per bookmark: the cadr of a bookmark record is
;; no longer a cons.  Why that change was made remains a mystery --
;; just be aware of it.  (Be aware too that this explanatory comment
;; was incorrect in Emacs 22 and Emacs 23.1.)
;;
;; To deal with the change from FIRST format to SECOND, conversion
;; code was added, which is no longer used and has been declared
;; obsolete.  See `bookmark-maybe-upgrade-file-format'.
;;
;; No conversion from SECOND to CURRENT is done.  Instead, the code
;; handles both formats OK.  It must continue to do so.
;;
;; See the doc string of `bookmark-alist' for information about the
;; elements that define a bookmark (e.g. `filename').


(defconst bookmark-file-format-version 1
  "The current version of the format used by bookmark files.
You should never need to change this.")


(defconst bookmark-end-of-version-stamp-marker
  "-*- End Of Bookmark File Format Version Stamp -*-\n"
  "This string marks the end of the version stamp in a bookmark file.")


(defun bookmark-alist-from-buffer ()
  "Return a `bookmark-alist' from the current buffer.
The buffer must of course contain bookmark format information.
Does not care from where in the buffer it is called, and does not
affect point."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward bookmark-end-of-version-stamp-marker nil t)
        (read (current-buffer))
      (if buffer-file-name
          (error "File not in bookmark format: %s" buffer-file-name)
        (error "Buffer not in bookmark format: %s" (buffer-name))))))

(defun bookmark-upgrade-version-0-alist (old-list)
  "Upgrade a version 0 alist OLD-LIST to the current version."
  (declare (obsolete nil "27.1"))
  (mapcar
   (lambda (bookmark)
     (let* ((name      (car bookmark))
            (record    (car (cdr bookmark)))
            (filename  (nth 0 record))
            (front-str (nth 1 record))
            (rear-str  (nth 2 record))
            (position  (nth 3 record))
            (ann       (nth 4 record)))
       (list
        name
        `((filename             .    ,filename)
          (front-context-string .    ,(or front-str ""))
          (rear-context-string  .    ,(or rear-str  ""))
          (position             .    ,position)
          (annotation           .    ,ann)))))
   old-list))


(defun bookmark-upgrade-file-format-from-0 ()
  "Upgrade a bookmark file of format 0 (the original format) to format 1.
This expects to be called from `point-min' in a bookmark file."
  (declare (obsolete nil "27.1"))
  (let* ((reporter (make-progress-reporter
                    (format "Upgrading bookmark format from 0 to %d..."
                            bookmark-file-format-version)))
         (old-list (bookmark-alist-from-buffer))
         (new-list (with-suppressed-warnings
                       ((obsolete bookmark-upgrade-version-0-alist))
                     (bookmark-upgrade-version-0-alist old-list))))
    (delete-region (point-min) (point-max))
    (bookmark-insert-file-format-version-stamp buffer-file-coding-system)
    (pp new-list (current-buffer))
    (save-buffer)
    (goto-char (point-min))
    (progress-reporter-done reporter)))


(defun bookmark-grok-file-format-version ()
  "Return an integer which is the file-format version of this bookmark file.
This expects to be called from `point-min' in a bookmark file."
  (declare (obsolete nil "27.1"))
  (if (looking-at "^;;;;")
      (save-excursion
        (save-match-data
          (re-search-forward "[0-9]")
          (forward-char -1)
          (read (current-buffer))))
    ;; Else this is format version 0, the original one, which didn't
    ;; even have version stamps.
    0))


(defun bookmark-maybe-upgrade-file-format ()
  "Check the file-format version of this bookmark file.
If the version is not up-to-date, upgrade it automatically.
This expects to be called from `point-min' in a bookmark file."
  (declare (obsolete nil "27.1"))
  (let ((version
         (with-suppressed-warnings
             ((obsolete bookmark-grok-file-format-version))
           (bookmark-grok-file-format-version))))
    (cond
     ((= version bookmark-file-format-version)
      ) ; home free -- version is current
     ((= version 0)
      (with-suppressed-warnings
          ((obsolete bookmark-upgrade-file-format-from-0))
        (bookmark-upgrade-file-format-from-0)))
     (t
      (error "Bookmark file format version strangeness")))))


(defun bookmark-insert-file-format-version-stamp (coding)
  "Insert text indicating current version of bookmark file format.
CODING is the symbol of the coding-system in which the file is encoded."
  (if (memq (coding-system-base coding) '(undecided prefer-utf-8))
      (setq coding 'utf-8-emacs))
  (insert
   (format ";;;; Emacs Bookmark Format Version %d ;;;; -*- coding: %S -*-\n"
           bookmark-file-format-version (coding-system-base coding)))
  (insert ";;; This format is meant to be slightly human-readable;\n"
          ";;; nevertheless, you probably don't want to edit it.\n"
          ";;; "
          bookmark-end-of-version-stamp-marker))


;;; end file-format stuff


;;; Core code:

(define-obsolete-function-alias 'bookmark-maybe-message 'message "27.1")

(defvar bookmark-minibuffer-read-name-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-w" 'bookmark-yank-word)
    map))

(defun bookmark-set-internal (prompt name overwrite-or-push)
  "Set a bookmark using specified NAME or prompting with PROMPT.
The bookmark is set at the current location.

If NAME is non-nil, use it as the name of the new bookmark.  In
this case, the value of PROMPT is ignored.

Otherwise, prompt the user for the bookmark name.  Begin the
interactive prompt with PROMPT, followed by a space, a generated
default name in parentheses, a colon and a space.

OVERWRITE-OR-PUSH controls what happens if there is already a
bookmark with the same name: nil means signal an error;
`overwrite' means replace any existing bookmark; `push' means
push the new bookmark onto the bookmark alist.  The `push'
behavior means that among bookmarks with the same name, this most
recently set one becomes the one in effect, but the others are
still there, in order, if the topmost one is ever deleted."
  (unwind-protect
       (let* ((record (bookmark-make-record))
              ;; `defaults' is a transient element of the
              ;; extensible format described above in the section
              ;; `File format stuff'.  Bookmark record functions
              ;; can use it to specify a list of default values
              ;; accessible via M-n while reading a bookmark name.
              (defaults (bookmark-prop-get record 'defaults))
              (default (if (consp defaults) (car defaults) defaults)))

         (if defaults
             ;; Don't store default values in the record.
             (setq record (assq-delete-all 'defaults record))
           ;; When no defaults in the record, use its first element.
           (setq defaults (car record) default defaults))

         (bookmark-maybe-load-default-file)
         ;; Don't set `bookmark-yank-point' and `bookmark-current-buffer'
         ;; if they have been already set in another buffer. (e.g gnus-art).
         (unless (and bookmark-yank-point
                      bookmark-current-buffer)
           (setq bookmark-yank-point (point))
           (setq bookmark-current-buffer (current-buffer)))

         (let ((str
                (or name
                    (read-from-minibuffer
                     (format "%s (default %s): " prompt default)
                     nil
                     bookmark-minibuffer-read-name-map
                     nil nil defaults))))
           (and (string-equal str "") (setq str default))

           (cond
            ((eq overwrite-or-push nil)
             (if (bookmark-get-bookmark str t)
                 (error "A bookmark named \"%s\" already exists" str)
               (bookmark-store str (cdr record) nil)))
            ((eq overwrite-or-push 'overwrite)
             (bookmark-store str (cdr record) nil))
            ((eq overwrite-or-push 'push)
             (bookmark-store str (cdr record) t))
            (t
             (error "Unrecognized value for `overwrite-or-push': %S"
                    overwrite-or-push)))

           ;; Ask for an annotation buffer for this bookmark
           (when bookmark-use-annotations
             (bookmark-edit-annotation str))))
    (setq bookmark-yank-point nil)
    (setq bookmark-current-buffer nil)))


;;;###autoload
(defun bookmark-set (&optional name no-overwrite)
  "Set a bookmark named NAME at the current location.
If NAME is nil, then prompt the user.

With a prefix arg (non-nil NO-OVERWRITE), do not overwrite any
existing bookmark that has the same name as NAME, but instead push the
new bookmark onto the bookmark alist.  The most recently set bookmark
with name NAME is thus the one in effect at any given time, but the
others are still there, should the user decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts (at the bookmark name prompt) the name of the last
bookmark used in the document where the new bookmark is being set;
this helps you use a single bookmark name to track progress through a
large document.  If there is no prior bookmark for this document, then
C-u inserts an appropriate name based on the buffer or file.

Use \\[bookmark-delete] to remove bookmarks (you give it a name and
it removes only the first instance of a bookmark with that name from
the list of bookmarks.)"
  (interactive (list nil current-prefix-arg))
  (let ((prompt
         (if no-overwrite "Set bookmark" "Set bookmark unconditionally")))
    (bookmark-set-internal prompt name (if no-overwrite 'push 'overwrite))))

;;;###autoload
(defun bookmark-set-no-overwrite (&optional name push-bookmark)
  "Set a bookmark named NAME at the current location.
If NAME is nil, then prompt the user.

If a bookmark named NAME already exists and prefix argument
PUSH-BOOKMARK is non-nil, then push the new bookmark onto the
bookmark alist.  Pushing it means that among bookmarks named
NAME, this one becomes the one in effect, but the others are
still there, in order, and become effective again if the user
ever deletes the most recent one.

Otherwise, if a bookmark named NAME already exists but PUSH-BOOKMARK
is nil, raise an error.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts (at the bookmark name prompt) the name of the last
bookmark used in the document where the new bookmark is being set;
this helps you use a single bookmark name to track progress through a
large document.  If there is no prior bookmark for this document, then
C-u inserts an appropriate name based on the buffer or file.

Use \\[bookmark-delete] to remove bookmarks (you give it a name and
it removes only the first instance of a bookmark with that name from
the list of bookmarks.)"
  (interactive (list nil current-prefix-arg))
  (bookmark-set-internal "Set bookmark" name (if push-bookmark 'push nil)))


(defun bookmark-kill-line (&optional newline-too)
  "Kill from point to end of line.
If optional arg NEWLINE-TOO is non-nil, delete the newline too.
Does not affect the kill ring."
  (let ((eol (line-end-position)))
    (delete-region (point) eol)
    (when (and newline-too (= (following-char) ?\n))
      (delete-char 1))))

(defvar bookmark-annotation-name nil
  "Name of bookmark under edit in `bookmark-edit-annotation-mode'.")
(make-variable-buffer-local 'bookmark-annotation-name)

(defvar bookmark--annotation-from-bookmark-list nil
  "If non-nil, `bookmark-edit-annotation-mode' should return to bookmark list.")
(make-variable-buffer-local 'bookmark--annotation-from-bookmark-list)

(defun bookmark-default-annotation-text (bookmark-name)
  "Return default annotation text for BOOKMARK-NAME.
The default annotation text is simply some text explaining how to use
annotations."
  (concat (format-message
           "#  Type the annotation for bookmark `%s' here.\n"
           bookmark-name)
	  (format-message
           "#  All lines which start with a `#' will be deleted.\n")
	  "#  Type C-c C-c when done.\n#\n"
	  "#  Author: " (user-full-name) " <" (user-login-name) "@"
	  (system-name) ">\n"
	  "#  Date:    " (current-time-string) "\n"))


(define-obsolete-variable-alias 'bookmark-read-annotation-text-func
  'bookmark-edit-annotation-text-func "23.1")
(defvar bookmark-edit-annotation-text-func 'bookmark-default-annotation-text
  "Function to return default text to use for a bookmark annotation.
It takes one argument, the name of the bookmark, as a string.")

(defvar bookmark-edit-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'bookmark-send-edited-annotation)
    map)
  "Keymap for editing an annotation of a bookmark.")

(defun bookmark-insert-annotation (bookmark-name-or-record)
  "Insert annotation for BOOKMARK-NAME-OR-RECORD at point."
  (when (not (bookmark-get-bookmark bookmark-name-or-record t))
    (error "Invalid bookmark: %s" bookmark-name-or-record))
  (insert (funcall bookmark-edit-annotation-text-func bookmark-name-or-record))
  (let ((annotation (bookmark-get-annotation bookmark-name-or-record)))
    (if (and annotation (not (string-equal annotation "")))
	(insert annotation))))

(define-derived-mode bookmark-edit-annotation-mode
  text-mode "Edit Bookmark Annotation"
  "Mode for editing the annotation of bookmarks.
When you have finished composing, type \\[bookmark-send-edited-annotation].

\\{bookmark-edit-annotation-mode-map}")


(defun bookmark-send-edited-annotation ()
  "Use buffer contents as annotation for a bookmark.
Lines beginning with `#' are ignored."
  (interactive)
  (if (not (derived-mode-p 'bookmark-edit-annotation-mode))
      (error "Not in bookmark-edit-annotation-mode"))
  (goto-char (point-min))
  (while (< (point) (point-max))
    (if (= (following-char) ?#)
        (bookmark-kill-line t)
      (forward-line 1)))
  ;; Take no chances with text properties.
  (let ((annotation (buffer-substring-no-properties (point-min) (point-max)))
        (bookmark-name bookmark-annotation-name)
        (from-bookmark-list bookmark--annotation-from-bookmark-list)
        (old-buffer (current-buffer)))
    (bookmark-set-annotation bookmark-name annotation)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (message "Annotation updated for \"%s\"" bookmark-name)
    (quit-window)
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (when from-bookmark-list
      (pop-to-buffer (get-buffer bookmark-bmenu-buffer))
      (goto-char (point-min))
      (text-property-search-forward 'bookmark-name-prop bookmark-name))
    (kill-buffer old-buffer)))


(defun bookmark-edit-annotation (bookmark-name-or-record &optional from-bookmark-list)
  "Pop up a buffer for editing bookmark BOOKMARK-NAME-OR-RECORD's annotation.
If optional argument FROM-BOOKMARK-LIST is non-nil, return to the
bookmark list when editing is done."
  (pop-to-buffer (generate-new-buffer-name "*Bookmark Annotation Compose*"))
  (bookmark-insert-annotation bookmark-name-or-record)
  (bookmark-edit-annotation-mode)
  (setq bookmark--annotation-from-bookmark-list from-bookmark-list)
  (setq bookmark-annotation-name bookmark-name-or-record))


(defun bookmark-buffer-name ()
  "Return the name of the current buffer in a form usable as a bookmark name.
If the buffer is associated with a file or directory, use that name."
  (cond
   ;; Or are we a file?
   (buffer-file-name (file-name-nondirectory buffer-file-name))
   ;; Or are we a directory?
   ((and (boundp 'dired-directory) dired-directory)
    (let* ((dirname (if (stringp dired-directory)
                        dired-directory
                      (car dired-directory)))
           (idx (1- (length dirname))))
      ;; Strip the trailing slash.
      (if (= ?/ (aref dirname idx))
          (file-name-nondirectory (substring dirname 0 idx))
        ;; Else return the current-buffer
        (buffer-name (current-buffer)))))
   ;; If all else fails, use the buffer's name.
   (t
    (buffer-name (current-buffer)))))


(defun bookmark-yank-word ()
  "Get the next word from buffer `bookmark-current-buffer' and append
it to the name of the bookmark currently being set, advancing
`bookmark-yank-point' by one word."
  (interactive)
  (let ((string (with-current-buffer bookmark-current-buffer
                  (goto-char bookmark-yank-point)
                  (buffer-substring-no-properties
                   (point)
                   (progn
                     (forward-word 1)
                     (setq bookmark-yank-point (point)))))))
    (insert string)))

(defun bookmark-buffer-file-name ()
  "Return the current buffer's file in a way useful for bookmarks."
  ;; Abbreviate the path, both so it's shorter and so it's more
  ;; portable.  E.g., the user's home dir might be a different
  ;; path on different machines, but "~/" will still reach it.
  (abbreviate-file-name
   (cond
    (buffer-file-name buffer-file-name)
    ((and (boundp 'dired-directory) dired-directory)
     (if (stringp dired-directory)
         dired-directory
       (car dired-directory)))
    (t (error "Buffer not visiting a file or directory")))))


(defun bookmark-maybe-load-default-file ()
  "If bookmarks have not been loaded from the default place, load them."
  (cond ((and (not bookmark-bookmarks-timestamp)
              (null bookmark-alist)
              (file-readable-p bookmark-default-file)
              (bookmark-load bookmark-default-file t t)))
        ((and bookmark-watch-bookmark-file
              (not (equal (nth 5 (file-attributes
                                  (car bookmark-bookmarks-timestamp)))
                          (cdr bookmark-bookmarks-timestamp)))
              (or (eq 'silent bookmark-watch-bookmark-file)
                  (yes-or-no-p
                   (format "Bookmarks %s changed on disk.  Reload? "
                           (car bookmark-bookmarks-timestamp)))))
         (bookmark-load (car bookmark-bookmarks-timestamp) t t))))

(defun bookmark-maybe-sort-alist ()
  "Return `bookmark-alist' for display.
If `bookmark-sort-flag' is non-nil, then return a sorted copy of the alist."
  (if bookmark-sort-flag
      (sort (copy-alist bookmark-alist)
            (function
             (lambda (x y) (string-lessp (car x) (car y)))))
    bookmark-alist))


(defvar bookmark-after-jump-hook nil
  "Hook run after `bookmark-jump' jumps to a bookmark.
Useful for example to unhide text in `outline-mode'.")

(defun bookmark--jump-via (bookmark-name-or-record display-function)
  "Handle BOOKMARK-NAME-OR-RECORD, then call DISPLAY-FUNCTION.
DISPLAY-FUNCTION is called with the current buffer as argument.

After calling DISPLAY-FUNCTION, set window point to the point specified
by BOOKMARK-NAME-OR-RECORD, if necessary, run `bookmark-after-jump-hook',
and then show any annotations for this bookmark."
  (bookmark-handle-bookmark bookmark-name-or-record)
  (save-current-buffer
    (funcall display-function (current-buffer)))
  (let ((win (get-buffer-window (current-buffer) 0)))
    (if win (set-window-point win (point))))
  ;; FIXME: we used to only run bookmark-after-jump-hook in
  ;; `bookmark-jump' itself, but in none of the other commands.
  (run-hooks 'bookmark-after-jump-hook)
  (if bookmark-automatically-show-annotations
      ;; if there is an annotation for this bookmark,
      ;; show it in a buffer.
      (bookmark-show-annotation bookmark-name-or-record)))


;;;###autoload
(defun bookmark-jump (bookmark &optional display-func)
  "Jump to bookmark BOOKMARK (a point in some file).
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

If the file pointed to by BOOKMARK no longer exists, you will be asked
if you wish to give the bookmark a new location, and `bookmark-jump'
will then jump to the new location, as well as recording it in place
of the old one in the permanent bookmark record.

BOOKMARK is usually a bookmark name (a string).  It can also be a
bookmark record, but this is usually only done by programmatic callers.

If DISPLAY-FUNC is non-nil, it is a function to invoke to display the
bookmark.  It defaults to `pop-to-buffer-same-window'.  A typical value for
DISPLAY-FUNC would be `switch-to-buffer-other-window'."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark"
				   bookmark-current-bookmark)))
  (unless bookmark
    (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark)
  ;; Don't use `switch-to-buffer' because it would let the
  ;; window-point override the bookmark's point when
  ;; `switch-to-buffer-preserve-window-point' is non-nil.
  (bookmark--jump-via bookmark (or display-func 'pop-to-buffer-same-window)))


;;;###autoload
(defun bookmark-jump-other-window (bookmark)
  "Jump to BOOKMARK in another window.  See `bookmark-jump' for more."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark (in another window)"
                                   bookmark-current-bookmark)))
  (bookmark-jump bookmark 'switch-to-buffer-other-window))

;;;###autoload
(defun bookmark-jump-other-frame (bookmark)
  "Jump to BOOKMARK in another frame.  See `bookmark-jump' for more."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark (in another frame)"
                                   bookmark-current-bookmark)))
  (let ((pop-up-frames t))
    (bookmark-jump-other-window bookmark)))

(defun bookmark-jump-noselect (bookmark)
  "Return the location pointed to by BOOKMARK (see `bookmark-jump').
The return value has the form (BUFFER . POINT).

Note: this function is deprecated and is present for Emacs 22
compatibility only."
  (declare (obsolete bookmark-handle-bookmark "23.1"))
  (save-excursion
    (bookmark-handle-bookmark bookmark)
    (cons (current-buffer) (point))))

(defun bookmark-handle-bookmark (bookmark-name-or-record)
  "Call BOOKMARK-NAME-OR-RECORD's handler or `bookmark-default-handler'
if it has none.  This changes current buffer and point and returns nil,
or signals a `file-error'.

If BOOKMARK-NAME-OR-RECORD has no file, this is a no-op.  If
BOOKMARK-NAME-OR-RECORD has a file, but that file no longer exists,
then offer interactively to relocate BOOKMARK-NAME-OR-RECORD."
  (condition-case err
      (funcall (or (bookmark-get-handler bookmark-name-or-record)
                   'bookmark-default-handler)
               (bookmark-get-bookmark bookmark-name-or-record))
    (bookmark-error-no-filename         ;file-error
     ;; We were unable to find the marked file, so ask if user wants to
     ;; relocate the bookmark, else remind them to consider deletion.
     (when (stringp bookmark-name-or-record)
       ;; `bookmark-name-or-record' can be either a bookmark name
       ;; (from `bookmark-alist')  or a bookmark object.  If it's an
       ;; object, we assume it's a bookmark used internally by some
       ;; other package.
       (let ((file (bookmark-get-filename bookmark-name-or-record)))
         (when file        ;Don't know how to relocate if there's no `file'.
           ;; If file is not a dir, directory-file-name just returns file.
           (let ((display-name (directory-file-name file)))
             (ding)
             ;; Dialog boxes can accept a file target, but usually don't
             ;; know how to accept a directory target (at least, this
             ;; is true in Gnome on GNU/Linux, and Bug#4230 says it's
             ;; true on Windows as well).  So we suppress file dialogs
             ;; when relocating.
             (let ((use-dialog-box nil)
                   (use-file-dialog nil))
               (if (y-or-n-p (concat display-name " nonexistent.  Relocate \""
                                     bookmark-name-or-record "\"? "))
                   (progn
                     (bookmark-relocate bookmark-name-or-record)
                     ;; Try again.
                     (funcall (or (bookmark-get-handler bookmark-name-or-record)
                                  'bookmark-default-handler)
                              (bookmark-get-bookmark bookmark-name-or-record)))
                 (message
                  "Bookmark not relocated; consider removing it (%s)."
                  bookmark-name-or-record)
                 (signal (car err) (cdr err))))))))))
  ;; Added by db.
  (when (stringp bookmark-name-or-record)
    (setq bookmark-current-bookmark bookmark-name-or-record))
  nil)

(define-error 'bookmark-errors
  "Bookmark error")
(define-error 'bookmark-error-no-filename
  "Bookmark has no associated file (or directory)" 'bookmark-errors)

(defun bookmark-default-handler (bmk-record)
  "Default handler to jump to a particular bookmark location.
BMK-RECORD is a bookmark record, not a bookmark name (i.e., not a string).
Changes current buffer and point and returns nil, or signals a `file-error'."
  (let ((file          (bookmark-get-filename bmk-record))
	(buf           (bookmark-prop-get bmk-record 'buffer))
        (forward-str   (bookmark-get-front-context-string bmk-record))
        (behind-str    (bookmark-get-rear-context-string bmk-record))
        (place         (bookmark-get-position bmk-record)))
    (set-buffer
     (cond
      ((and file (file-readable-p file) (not (buffer-live-p buf)))
       (find-file-noselect file))
      ;; No file found.  See if buffer BUF has been created.
      ((and buf (get-buffer buf)))
      (t ;; If not, raise error.
       (signal 'bookmark-error-no-filename (list 'stringp file)))))
    (if place (goto-char place))
    ;; Go searching forward first.  Then, if forward-str exists and
    ;; was found in the file, we can search backward for behind-str.
    ;; Rationale is that if text was inserted between the two in the
    ;; file, it's better to be put before it so you can read it,
    ;; rather than after and remain perhaps unaware of the changes.
    (when (and forward-str (search-forward forward-str (point-max) t))
      (goto-char (match-beginning 0)))
    (when (and behind-str (search-backward behind-str (point-min) t))
      (goto-char (match-end 0)))
    nil))

;;;###autoload
(defun bookmark-relocate (bookmark-name)
  "Relocate BOOKMARK-NAME to another file, reading file name with minibuffer.

This makes an already existing bookmark point to that file, instead of
the one it used to point at.  Useful when a file has been renamed
after a bookmark was set in it."
  (interactive (list (bookmark-completing-read "Bookmark to relocate")))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let* ((bmrk-filename (bookmark-get-filename bookmark-name))
         (newloc (abbreviate-file-name
                  (expand-file-name
                   (read-file-name
                    (format "Relocate %s to: " bookmark-name)
                    (file-name-directory bmrk-filename))))))
    (bookmark-set-filename bookmark-name newloc)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (bookmark-save))
    (bookmark-bmenu-surreptitiously-rebuild-list)))


;;;###autoload
(defun bookmark-insert-location (bookmark-name &optional no-history)
  "Insert the name of the file associated with BOOKMARK-NAME.

Optional second arg NO-HISTORY means don't record this in the
minibuffer history list `bookmark-history'."
  (interactive (list (bookmark-completing-read "Insert bookmark location")))
  (or no-history (bookmark-maybe-historicize-string bookmark-name))
  (insert (bookmark-location bookmark-name)))

;;;###autoload
(defalias 'bookmark-locate 'bookmark-insert-location)

(defun bookmark-location (bookmark-name-or-record)
  "Return a description of the location of BOOKMARK-NAME-OR-RECORD."
  (bookmark-maybe-load-default-file)
  ;; We could call the `handler' and ask for it to construct a description
  ;; dynamically: it would open up several new possibilities, but it
  ;; would have the major disadvantage of forcing to load each and
  ;; every handler when the user calls bookmark-menu.
  (or (bookmark-prop-get bookmark-name-or-record 'location)
      (bookmark-get-filename bookmark-name-or-record)
      "-- Unknown location --"))


;;;###autoload
(defun bookmark-rename (old-name &optional new-name)
  "Change the name of OLD-NAME bookmark to NEW-NAME name.
If called from keyboard, prompt for OLD-NAME and NEW-NAME.
If called from menubar, select OLD-NAME from a menu and prompt for NEW-NAME.

If called from Lisp, prompt for NEW-NAME if only OLD-NAME was passed
as an argument.  If called with two strings, then no prompting is done.
You must pass at least OLD-NAME when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consecutive words from the text of the buffer into the new bookmark
name."
  (interactive (list (bookmark-completing-read "Old bookmark name")))
  (bookmark-maybe-historicize-string old-name)
  (bookmark-maybe-load-default-file)

  (setq bookmark-yank-point (point))
  (setq bookmark-current-buffer (current-buffer))
  (let ((final-new-name
         (or new-name   ; use second arg, if non-nil
             (read-from-minibuffer
              "New name: "
              nil
              (let ((now-map (copy-keymap minibuffer-local-map)))
                (define-key now-map "\C-w" 'bookmark-yank-word)
                now-map)
              nil
              'bookmark-history))))
    (bookmark-set-name old-name final-new-name)
    (setq bookmark-current-bookmark final-new-name)
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (bookmark-save))))


;;;###autoload
(defun bookmark-insert (bookmark-name)
  "Insert the text of the file pointed to by bookmark BOOKMARK-NAME.
BOOKMARK-NAME is a bookmark name (a string), not a bookmark record.

You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this."
  (interactive (list (bookmark-completing-read "Insert bookmark contents")))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let ((orig-point (point))
	(str-to-insert
	 (save-current-buffer
           (bookmark-handle-bookmark bookmark-name)
	   (buffer-string))))
    (insert str-to-insert)
    (push-mark)
    (goto-char orig-point)))


;;;###autoload
(defun bookmark-delete (bookmark-name &optional batch)
  "Delete BOOKMARK-NAME from the bookmark list.

Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark (that is, the
one most recently used in this file, if any).
Optional second arg BATCH means don't update the bookmark list buffer,
probably because we were called from there."
  (interactive
   (list (bookmark-completing-read "Delete bookmark"
				   bookmark-current-bookmark)))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let ((will-go (bookmark-get-bookmark bookmark-name 'noerror)))
    (setq bookmark-alist (delq will-go bookmark-alist))
    ;; Added by db, nil bookmark-current-bookmark if the last
    ;; occurrence has been deleted
    (or (bookmark-get-bookmark bookmark-current-bookmark 'noerror)
        (setq bookmark-current-bookmark nil)))
  (unless batch
    (bookmark-bmenu-surreptitiously-rebuild-list))
  (setq bookmark-alist-modification-count
        (1+ bookmark-alist-modification-count))
  (when (bookmark-time-to-save-p)
    (bookmark-save)))


(defun bookmark-time-to-save-p (&optional final-time)
  "Return t if it is time to save bookmarks to disk, nil otherwise.
Optional argument FINAL-TIME means this is being called when Emacs
is being killed, so save even if `bookmark-save-flag' is a number and
is greater than `bookmark-alist-modification-count'."
  ;; By Gregory M. Saunders <saunders{_AT_}cis.ohio-state.edu>
  (cond (final-time
	 (and (> bookmark-alist-modification-count 0)
	      bookmark-save-flag))
	((numberp bookmark-save-flag)
	 (>= bookmark-alist-modification-count bookmark-save-flag))
	(t
	 nil)))


;;;###autoload
(defun bookmark-write ()
  "Write bookmarks to a file (reading the file name with the minibuffer)."
  (declare (interactive-only bookmark-save))
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-save t))


;;;###autoload
(defun bookmark-save (&optional parg file make-default)
  "Save currently defined bookmarks in FILE.
FILE defaults to `bookmark-default-file'.
With prefix PARG, query user for a file to save in.
If MAKE-DEFAULT is non-nil (interactively with prefix C-u C-u)
the file we save in becomes the new default in the current Emacs
session (without affecting the value of `bookmark-default-file'.).

When you want to load in the bookmarks from a file, use
`bookmark-load', \\[bookmark-load].  That function will prompt you
for a file, defaulting to the file defined by variable
`bookmark-default-file'."
  (interactive
   (list current-prefix-arg nil (equal '(16) current-prefix-arg)))
  (bookmark-maybe-load-default-file)
  (unless file
    (setq file
          (let ((default (or (car bookmark-bookmarks-timestamp)
                             bookmark-default-file)))
            (if parg
                ;; This should be part of the `interactive' spec.
                (read-file-name (format "File to save bookmarks in: (%s) "
                                        default)
                                (file-name-directory default) default)
              default))))
  (bookmark-write-file file)
  ;; Signal that we have synced the bookmark file by setting this to 0.
  ;; If there was an error at any point before, it will not get set,
  ;; which is what we want.
  (setq bookmark-alist-modification-count 0)
  (if make-default
      (let ((default (expand-file-name file)))
        (setq bookmark-bookmarks-timestamp
              (cons default (nth 5 (file-attributes default)))))
    (let ((default (car bookmark-bookmarks-timestamp)))
      (if (string= default (expand-file-name file))
          (setq bookmark-bookmarks-timestamp
                (cons default (nth 5 (file-attributes default))))))))


(defun bookmark-write-file (file)
  "Write `bookmark-alist' to FILE."
  (let ((reporter (make-progress-reporter
		   (format "Saving bookmarks to file %s..." file))))
    (with-current-buffer (get-buffer-create " *Bookmarks*")
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (let ((coding-system-for-write
	     (or coding-system-for-write
		 bookmark-file-coding-system 'utf-8-emacs))
	    (print-length nil)
	    (print-level nil)
	    ;; See bug #12503 for why we bind `print-circle'.  Users
	    ;; can define their own bookmark types, which can result in
	    ;; arbitrary Lisp objects being stored in bookmark records,
	    ;; and some users create objects containing circularities.
	    (print-circle t))
	(insert "(")
	;; Rather than a single call to `pp' we make one per bookmark.
	;; Apparently `pp' has a poor algorithmic complexity, so this
	;; scales a lot better.  bug#4485.
	(dolist (i bookmark-alist) (pp i (current-buffer)))
	(insert ")\n")
	;; Make sure the specified encoding can safely encode the
	;; bookmarks.  If it cannot, suggest utf-8-emacs as default.
	(with-coding-priority '(utf-8-emacs)
	  (setq coding-system-for-write
		(select-safe-coding-system (point-min) (point-max)
					   (list t coding-system-for-write))))
	(goto-char (point-min))
	(bookmark-insert-file-format-version-stamp coding-system-for-write)
	(let ((version-control
	       (cond
		((null bookmark-version-control) nil)
		((eq 'never bookmark-version-control) 'never)
		((eq 'nospecial bookmark-version-control) version-control)
		(t t))))
	  (condition-case nil
	      (write-region (point-min) (point-max) file)
	    (file-error (message "Can't write %s" file)))
	  (setq bookmark-file-coding-system coding-system-for-write)
	  (kill-buffer (current-buffer))
	  (progress-reporter-done reporter))))))


(defun bookmark-import-new-list (new-list)
  "Add NEW-LIST of bookmarks to `bookmark-alist'.
Rename new bookmarks as needed using suffix \"<N>\" (N=1,2,3...), when
they conflict with existing bookmark names."
  (let ((names (bookmark-all-names)))
    (dolist (full-record new-list)
      (bookmark-maybe-rename full-record names)
      (setq bookmark-alist (nconc bookmark-alist (list full-record)))
      (push (bookmark-name-from-full-record full-record) names))))


(defun bookmark-maybe-rename (full-record names)
  "Rename bookmark FULL-RECORD if its current name is already used.
This is a helper for `bookmark-import-new-list'."
  (let ((found-name (bookmark-name-from-full-record full-record)))
    (if (member found-name names)
        ;; We've got a conflict, so generate a new name
        (let ((count 2)
              (new-name found-name))
          (while (member new-name names)
            (setq new-name (concat found-name (format "<%d>" count)))
            (setq count (1+ count)))
          (bookmark-set-name full-record new-name)))))


;;;###autoload
(defun bookmark-load (file &optional overwrite no-msg default)
  "Load bookmarks from FILE (which must be in bookmark format).
Appends loaded bookmarks to the front of the list of bookmarks.
If argument OVERWRITE is non-nil, existing bookmarks are destroyed.
Optional third arg NO-MSG means don't display any messages while loading.
If DEFAULT is non-nil make FILE the new bookmark file to watch.
Interactively, a prefix arg makes OVERWRITE and DEFAULT non-nil.

If you load a file that doesn't contain a proper bookmark alist, you
will corrupt Emacs's bookmark list.  Generally, you should only load
in files that were created with the bookmark functions in the first
place.  Your own personal bookmark file, specified by the variable
`bookmark-default-file', is maintained automatically by Emacs; you
shouldn't need to load it explicitly.

If you load a file containing bookmarks with the same names as
bookmarks already present in your Emacs, the new bookmarks will get
unique numeric suffixes \"<2>\", \"<3>\", etc."
  (interactive
   (let ((default (abbreviate-file-name
		   (or (car bookmark-bookmarks-timestamp)
		       (expand-file-name bookmark-default-file))))
	 (prefix current-prefix-arg))
     (list (read-file-name (format "Load bookmarks from: (%s) " default)
			   (file-name-directory default) default 'confirm)
	   prefix nil prefix)))
  (let* ((file (expand-file-name file))
	 (afile (abbreviate-file-name file)))
    (unless (file-readable-p file)
      (user-error "Cannot read bookmark file %s" afile))
    (let ((reporter
	   (unless no-msg
	     (make-progress-reporter
	      (format "Loading bookmarks from %s..." file)))))
      (with-current-buffer (let (enable-local-variables)
			     (find-file-noselect file))
	(goto-char (point-min))
	(let ((blist (bookmark-alist-from-buffer)))
	  (unless (listp blist)
	    (error "Invalid bookmark list in %s" file))
	  ;; RW: Upon loading the bookmarks, we could add to each bookmark
	  ;; in `bookmark-alist' an extra key `bookmark-file', so that
	  ;; upon reloading the bookmarks with OVERWRITE non-nil,
	  ;; we overwrite only those bookmarks for which the key `bookmark-file'
	  ;; matches FILE.  `bookmark-save' can ignore this key.
	  ;; Would this be a useful option?
	  (if overwrite
	      (setq bookmark-alist blist
		    bookmark-alist-modification-count 0)
	    (bookmark-import-new-list blist)
	    (setq bookmark-alist-modification-count
		  (1+ bookmark-alist-modification-count)))
	  (if (or default
		  (string= file (or (car bookmark-bookmarks-timestamp)
				    (expand-file-name bookmark-default-file))))
	      (setq bookmark-bookmarks-timestamp
		    (cons file (nth 5 (file-attributes file)))))
	  (bookmark-bmenu-surreptitiously-rebuild-list)
	  (setq bookmark-file-coding-system buffer-file-coding-system))
	(kill-buffer (current-buffer)))
      (unless no-msg
	(progress-reporter-done reporter)))))


;;; Code supporting the dired-like bookmark list.
;; Prefix is "bookmark-bmenu" for "buffer-menu":

(defvar bookmark-bmenu-hidden-bookmarks ())


(defvar bookmark-bmenu-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "v" 'bookmark-bmenu-select)
    (define-key map "w" 'bookmark-bmenu-locate)
    (define-key map "5" 'bookmark-bmenu-other-frame)
    (define-key map "2" 'bookmark-bmenu-2-window)
    (define-key map "1" 'bookmark-bmenu-1-window)
    (define-key map "j" 'bookmark-bmenu-this-window)
    (define-key map "\C-c\C-c" 'bookmark-bmenu-this-window)
    (define-key map "f" 'bookmark-bmenu-this-window)
    (define-key map "\C-m" 'bookmark-bmenu-this-window)
    (define-key map "o" 'bookmark-bmenu-other-window)
    (define-key map "\C-o" 'bookmark-bmenu-switch-other-window)
    (define-key map "s" 'bookmark-bmenu-save)
    (define-key map "\C-x\C-s" 'bookmark-bmenu-save)
    (define-key map "k" 'bookmark-bmenu-delete)
    (define-key map "\C-d" 'bookmark-bmenu-delete-backwards)
    (define-key map "x" 'bookmark-bmenu-execute-deletions)
    (define-key map "d" 'bookmark-bmenu-delete)
    (define-key map " " 'next-line)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "\177" 'bookmark-bmenu-backup-unmark)
    (define-key map "u" 'bookmark-bmenu-unmark)
    (define-key map "m" 'bookmark-bmenu-mark)
    (define-key map "l" 'bookmark-bmenu-load)
    (define-key map "r" 'bookmark-bmenu-rename)
    (define-key map "R" 'bookmark-bmenu-relocate)
    (define-key map "t" 'bookmark-bmenu-toggle-filenames)
    (define-key map "a" 'bookmark-bmenu-show-annotation)
    (define-key map "A" 'bookmark-bmenu-show-all-annotations)
    (define-key map "e" 'bookmark-bmenu-edit-annotation)
    (define-key map "/" 'bookmark-bmenu-search)
    (define-key map [mouse-2] 'bookmark-bmenu-other-window-with-mouse)
    map))

(easy-menu-define
  bookmark-menu bookmark-bmenu-mode-map "Bookmark Menu"
  '("Bookmark"
    ["Select Bookmark in This Window" bookmark-bmenu-this-window  t]
    ["Select Bookmark in Full-Frame Window" bookmark-bmenu-1-window  t]
    ["Select Bookmark in Other Window" bookmark-bmenu-other-window  t]
    ["Select Bookmark in Other Frame" bookmark-bmenu-other-frame  t]
    ["Select Marked Bookmarks" bookmark-bmenu-select t]
    "---"
    ["Mark Bookmark" bookmark-bmenu-mark t]
    ["Unmark Bookmark" bookmark-bmenu-unmark  t]
    ["Unmark Backwards" bookmark-bmenu-backup-unmark  t]
    ["Toggle Display of Filenames" bookmark-bmenu-toggle-filenames  t]
    ["Display Location of Bookmark" bookmark-bmenu-locate  t]
    "---"
    ("Edit Bookmarks"
     ["Rename Bookmark" bookmark-bmenu-rename  t]
     ["Relocate Bookmark's File" bookmark-bmenu-relocate  t]
     ["Mark Bookmark for Deletion" bookmark-bmenu-delete  t]
     ["Delete Marked Bookmarks" bookmark-bmenu-execute-deletions  t])
    ("Annotations"
     ["Show Annotation for Current Bookmark" bookmark-bmenu-show-annotation  t]
     ["Show Annotations for All Bookmarks" bookmark-bmenu-show-all-annotations  t]
     ["Edit Annotation for Current Bookmark."  bookmark-bmenu-edit-annotation  t])
    "---"
    ["Save Bookmarks" bookmark-bmenu-save  t]
    ["Load Bookmarks" bookmark-bmenu-load  t]))

;; Bookmark Buffer Menu mode is suitable only for specially formatted
;; data.
(put 'bookmark-bmenu-mode 'mode-class 'special)


;; todo: need to display whether or not bookmark exists as a buffer in
;; flag column.

;; Format:
;; FLAGS  BOOKMARK [ LOCATION ]


(defun bookmark-bmenu-surreptitiously-rebuild-list ()
  "Rebuild the Bookmark List if it exists.
Don't affect the buffer ring order."
  (if (get-buffer bookmark-bmenu-buffer)
      (save-excursion
        (save-window-excursion
          (bookmark-bmenu-list)))))


;;;###autoload
(defun bookmark-bmenu-list ()
  "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
  (interactive)
  (bookmark-maybe-load-default-file)
  (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
    (if (called-interactively-p 'interactive)
        (switch-to-buffer buf)
      (set-buffer buf)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (not bookmark-bmenu-use-header-line)
      (insert "% Bookmark\n- --------\n"))
    (add-text-properties (point-min) (point)
			 '(font-lock-face bookmark-menu-heading))
    (dolist (full-record (bookmark-maybe-sort-alist))
      (let ((name        (bookmark-name-from-full-record full-record))
            (annotation  (bookmark-get-annotation full-record))
            (start       (point))
            end)
        ;; if a bookmark has an annotation, prepend a "*"
        ;; in the list of bookmarks.
        (insert (if (and annotation (not (string-equal annotation "")))
                    " *" "  ")
                name)
        (setq end (point))
        (put-text-property
         (+ bookmark-bmenu-marks-width start) end 'bookmark-name-prop name)
        (when (display-mouse-p)
          (add-text-properties
           (+ bookmark-bmenu-marks-width start) end
           '(font-lock-face bookmark-menu-bookmark
	     mouse-face highlight
             follow-link t
             help-echo "mouse-2: go to this bookmark in other window")))
        (insert "\n")))
    (set-buffer-modified-p (not (= bookmark-alist-modification-count 0)))
    (goto-char (point-min))
    (bookmark-bmenu-mode)
    (if bookmark-bmenu-use-header-line
	(bookmark-bmenu-set-header)
      (forward-line bookmark-bmenu-inline-header-height))
    (when (and bookmark-alist bookmark-bmenu-toggle-filenames)
      (bookmark-bmenu-toggle-filenames t))))

;;;###autoload
(defalias 'list-bookmarks 'bookmark-bmenu-list)
;;;###autoload
(defalias 'edit-bookmarks 'bookmark-bmenu-list)

;; FIXME: This could also display the current default bookmark file
;; according to `bookmark-bookmarks-timestamp'.
(defun bookmark-bmenu-set-header ()
  "Set the immutable header line."
  (let ((header (concat "%% " "Bookmark")))
    (when bookmark-bmenu-toggle-filenames
      (setq header (concat header
			   (make-string (- bookmark-bmenu-file-column
					   (- (length header) 3))  ?\s)
			   "File")))
    (let ((pos 0))
      (while (string-match "[ \t\n]+" header pos)
	(setq pos (match-end 0))
	(put-text-property (match-beginning 0) pos 'display
			   (list 'space :align-to (- pos 1))
			   header)))
    (put-text-property 0 2 'face 'fixed-pitch header)
    (setq header (concat (propertize " " 'display '(space :align-to 0))
			 header))
    ;; Code derived from `buff-menu.el'.
    (setq header-line-format header)))

(define-derived-mode bookmark-bmenu-mode special-mode "Bookmark Menu"
  "Major mode for editing a list of bookmarks.
Each line describes one of the bookmarks in Emacs.
Letters do not insert themselves; instead, they are commands.
Bookmark names preceded by a \"*\" have annotations.
\\<bookmark-bmenu-mode-map>
\\[bookmark-bmenu-mark] -- mark bookmark to be displayed.
\\[bookmark-bmenu-select] -- select bookmark of line point is on.
  Also show bookmarks marked using m in other windows.
\\[bookmark-bmenu-toggle-filenames] -- toggle displaying of filenames (they may obscure long bookmark names).
\\[bookmark-bmenu-locate] -- display (in minibuffer) location of this bookmark.
\\[bookmark-bmenu-1-window] -- select this bookmark in full-frame window.
\\[bookmark-bmenu-2-window] -- select this bookmark in one window,
  together with bookmark selected before this one in another window.
\\[bookmark-bmenu-this-window] -- select this bookmark in place of the bookmark menu buffer.
\\[bookmark-bmenu-other-window] -- select this bookmark in another window,
  so the bookmark menu bookmark remains visible in its window.
\\[bookmark-bmenu-other-frame] -- select this bookmark in another frame.
\\[bookmark-bmenu-switch-other-window] -- switch the other window to this bookmark.
\\[bookmark-bmenu-rename] -- rename this bookmark (prompts for new name).
\\[bookmark-bmenu-relocate] -- relocate this bookmark's file (prompts for new file).
\\[bookmark-bmenu-delete] -- mark this bookmark to be deleted, and move down.
\\[bookmark-bmenu-delete-backwards] -- mark this bookmark to be deleted, and move up.
\\[bookmark-bmenu-execute-deletions] -- delete bookmarks marked with `\\[bookmark-bmenu-delete]'.
\\[bookmark-bmenu-save] -- save the current bookmark list in the default file.
  With a prefix arg, prompts for a file to save in.
\\[bookmark-bmenu-load] -- load in a file of bookmarks (prompts for file.)
\\[bookmark-bmenu-unmark] -- remove all kinds of marks from current line.
  With prefix argument, also move up one line.
\\[bookmark-bmenu-backup-unmark] -- back up a line and remove marks.
\\[bookmark-bmenu-show-annotation] -- show the annotation, if it exists, for the current bookmark
  in another buffer.
\\[bookmark-bmenu-show-all-annotations] -- show the annotations of all bookmarks in another buffer.
\\[bookmark-bmenu-edit-annotation] -- edit the annotation for the current bookmark.
\\[bookmark-bmenu-search] -- incrementally search for bookmarks."
  (setq truncate-lines t)
  (setq buffer-read-only t))


(defun bookmark-bmenu-toggle-filenames (&optional show)
  "Toggle whether filenames are shown in the bookmark list.
Optional argument SHOW means show them unconditionally."
  (interactive)
  (cond
   (show
    (setq bookmark-bmenu-toggle-filenames nil)
    (bookmark-bmenu-show-filenames)
    (setq bookmark-bmenu-toggle-filenames t))
   (bookmark-bmenu-toggle-filenames
    (bookmark-bmenu-hide-filenames)
    (setq bookmark-bmenu-toggle-filenames nil))
   (t
    (bookmark-bmenu-show-filenames)
    (setq bookmark-bmenu-toggle-filenames t)))
  (when bookmark-bmenu-use-header-line
    (bookmark-bmenu-set-header)))


(defun bookmark-bmenu-show-filenames (&optional force)
  "In an interactive bookmark list, show filenames along with bookmarks.
Non-nil FORCE forces a redisplay showing the filenames.  FORCE is used
mainly for debugging, and should not be necessary in normal use."
  (if (and (not force) bookmark-bmenu-toggle-filenames)
      nil ;already shown, so do nothing
    (with-buffer-modified-unmodified
     (save-excursion
       (save-window-excursion
         (goto-char (point-min))
	 (if (not bookmark-bmenu-use-header-line)
	     (forward-line bookmark-bmenu-inline-header-height))
         (setq bookmark-bmenu-hidden-bookmarks ())
         (let ((inhibit-read-only t))
           (while (< (point) (point-max))
             (let ((bmrk (bookmark-bmenu-bookmark)))
               (push bmrk bookmark-bmenu-hidden-bookmarks)
               (let ((start (line-end-position)))
                 (move-to-column bookmark-bmenu-file-column t)
                 ;; Strip off `mouse-face' from the white spaces region.
                 (if (display-mouse-p)
                     (remove-text-properties start (point)
                                             '(mouse-face nil help-echo nil))))
               (delete-region (point) (progn (end-of-line) (point)))
               (insert "  ")
               ;; Pass the NO-HISTORY arg:
               (bookmark-insert-location bmrk t)
               (forward-line 1)))))))))


(defun bookmark-bmenu-hide-filenames (&optional force)
  "In an interactive bookmark list, hide the filenames of the bookmarks.
Non-nil FORCE forces a redisplay showing the filenames.  FORCE is used
mainly for debugging, and should not be necessary in normal use."
  (when (and (not force) bookmark-bmenu-toggle-filenames)
    ;; nothing to hide if above is nil
    (with-buffer-modified-unmodified
     (save-excursion
       (goto-char (point-min))
       (if (not bookmark-bmenu-use-header-line)
	   (forward-line bookmark-bmenu-inline-header-height))
       (setq bookmark-bmenu-hidden-bookmarks
             (nreverse bookmark-bmenu-hidden-bookmarks))
       (let ((inhibit-read-only t))
         (while bookmark-bmenu-hidden-bookmarks
           (move-to-column bookmark-bmenu-marks-width t)
           (bookmark-kill-line)
           (let ((name  (pop bookmark-bmenu-hidden-bookmarks))
                 (start (point)))
             (insert name)
             (put-text-property start (point) 'bookmark-name-prop name)
             (if (display-mouse-p)
                 (add-text-properties
                  start (point)
                  '(font-lock-face bookmark-menu-bookmark
		    mouse-face highlight
		    follow-link t help-echo
                    "mouse-2: go to this bookmark in other window"))))
           (forward-line 1)))))))


(defun bookmark-bmenu-ensure-position ()
  "If point is not on a bookmark line, move it to one.
If before the first bookmark line, move to the first; if after the
last full line, move to the last full line.  The return value is undefined."
  (cond ((and (not bookmark-bmenu-use-header-line)
	      (< (count-lines (point-min) (point))
		 bookmark-bmenu-inline-header-height))
         (goto-char (point-min))
         (forward-line bookmark-bmenu-inline-header-height))
        ((and (bolp) (eobp))
         (beginning-of-line 0))))


(defun bookmark-bmenu-bookmark ()
  "Return the bookmark for this line in an interactive bookmark list buffer."
  (bookmark-bmenu-ensure-position)
  (save-excursion
    (beginning-of-line)
    (forward-char bookmark-bmenu-marks-width)
    (get-text-property (point) 'bookmark-name-prop)))


(defun bookmark-show-annotation (bookmark-name-or-record)
  "Display the annotation for BOOKMARK-NAME-OR-RECORD in a buffer.
If the annotation does not exist, do nothing."
  (let ((annotation (bookmark-get-annotation bookmark-name-or-record)))
    (when (and annotation (not (string-equal annotation "")))
      (save-excursion
        (let ((old-buf (current-buffer)))
          (pop-to-buffer (get-buffer-create "*Bookmark Annotation*") t)
          (let (buffer-read-only)
            (erase-buffer)
            (insert annotation)
            (goto-char (point-min))
            (set-buffer-modified-p nil))
          (setq buffer-read-only t)
          (switch-to-buffer-other-window old-buf))))))


(defun bookmark-show-all-annotations ()
  "Display the annotations for all bookmarks in a buffer."
  (save-selected-window
    (pop-to-buffer (get-buffer-create "*Bookmark Annotation*") t)
    (let (buffer-read-only)
      (erase-buffer)
      (dolist (full-record (bookmark-maybe-sort-alist))
        (let* ((name (bookmark-name-from-full-record full-record))
               (ann  (bookmark-get-annotation full-record)))
          (insert (concat name ":\n"))
          (if (and ann (not (string-equal ann "")))
              ;; insert the annotation, indented by 4 spaces.
              (progn
                (save-excursion (insert ann) (unless (bolp)
                                               (insert "\n")))
                (while (< (point) (point-max))
                  (beginning-of-line)     ; paranoia
                  (insert "    ")
                  (forward-line)
                  (end-of-line))))))
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (setq buffer-read-only t)))


(defun bookmark-bmenu-mark ()
  "Mark bookmark on this line to be displayed by \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-select]."
  (interactive)
  (beginning-of-line)
  (bookmark-bmenu-ensure-position)
  (with-buffer-modified-unmodified
   (let ((inhibit-read-only t))
     (delete-char 1)
     (insert ?>)
     (forward-line 1)
     (bookmark-bmenu-ensure-position))))


(defun bookmark-bmenu-select ()
  "Select this line's bookmark; also display bookmarks marked with `>'.
You can mark bookmarks with the \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-mark] command."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark))
        (menu (current-buffer))
        (others ())
        tem)
    (goto-char (point-min))
    (while (re-search-forward "^>" nil t)
      (setq tem (bookmark-bmenu-bookmark))
      (let ((inhibit-read-only t))
        (delete-char -1)
        (insert ?\s))
      (or (string-equal tem bmrk)
          (member tem others)
          (setq others (cons tem others))))
    (setq others (nreverse others)
          tem (/ (1- (frame-height)) (1+ (length others))))
    (delete-other-windows)
    (bookmark-jump bmrk)
    (bury-buffer menu)
    (if others
        (while others
          (split-window nil tem)
          (other-window 1)
          (bookmark-jump (car others))
          (setq others (cdr others)))
      (other-window 1))))


(defun bookmark-bmenu-any-marks ()
  "Return non-nil if any bookmarks are marked in the marks column."
  (save-excursion
    (goto-char (point-min))
    (bookmark-bmenu-ensure-position)
    (catch 'found-mark
      (while (not (eobp))
        (beginning-of-line)
        (if (looking-at "^\\S-")
            (throw 'found-mark t)
          (forward-line 1)))
      nil)))


(defun bookmark-bmenu-save ()
  "Save the current list into a bookmark file.
With a prefix arg, prompts for a file to save them in."
  (interactive)
  (save-excursion
    (save-window-excursion
      (call-interactively 'bookmark-save)
      (set-buffer-modified-p nil))))


(defun bookmark-bmenu-load ()
  "Load the bookmark file and rebuild the bookmark menu-buffer."
  (interactive)
  (bookmark-bmenu-ensure-position)
  (save-excursion
    (save-window-excursion
      ;; This will call `bookmark-bmenu-list'
      (call-interactively 'bookmark-load))))


(defun bookmark-bmenu-1-window ()
  "Select this line's bookmark, alone, in full frame."
  (interactive)
  (bookmark-jump (bookmark-bmenu-bookmark))
  (bury-buffer (other-buffer))
  (delete-other-windows))


(defun bookmark-bmenu-2-window ()
  "Select this line's bookmark, with previous buffer in second window."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark))
        (menu (current-buffer))
        (pop-up-windows t))
    (delete-other-windows)
    (switch-to-buffer (other-buffer) nil t)
    (bookmark--jump-via bmrk 'pop-to-buffer)
    (bury-buffer menu)))


(defun bookmark-bmenu-this-window ()
  "Select this line's bookmark in this window."
  (interactive)
  (bookmark-jump (bookmark-bmenu-bookmark)))


(defun bookmark-bmenu-other-window ()
  "Select this line's bookmark in other window, leaving bookmark menu visible."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
    (bookmark--jump-via bookmark 'switch-to-buffer-other-window)))


(defun bookmark-bmenu-other-frame ()
  "Select this line's bookmark in other frame."
  (interactive)
  (let  ((bookmark (bookmark-bmenu-bookmark))
         (pop-up-frames t))
    (bookmark-jump-other-window bookmark)))

(defun bookmark-bmenu-switch-other-window ()
  "Make the other window select this line's bookmark.
The current window remains selected."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark))
	(fun (lambda (b) (display-buffer b t))))
    (bookmark--jump-via bookmark fun)))

(defun bookmark-bmenu-other-window-with-mouse (event)
  "Jump to bookmark at mouse EVENT position in other window.
Move point in menu buffer to the position of EVENT and leave
bookmark menu visible."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (bookmark-bmenu-other-window))))


(defun bookmark-bmenu-show-annotation ()
  "Show the annotation for the current bookmark in another window."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
    (bookmark-show-annotation bookmark)))


(defun bookmark-bmenu-show-all-annotations ()
  "Show the annotation for all bookmarks in another window."
  (interactive)
  (bookmark-show-all-annotations))


(defun bookmark-bmenu-edit-annotation ()
  "Edit the annotation for the current bookmark in another window."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
    (bookmark-edit-annotation bookmark t)))


(defun bookmark-bmenu-unmark (&optional backup)
  "Cancel all requested operations on bookmark on this line and move down.
Optional BACKUP means move up."
  (interactive "P")
  (beginning-of-line)
  (bookmark-bmenu-ensure-position)
  (with-buffer-modified-unmodified
   (let ((inhibit-read-only t))
     (delete-char 1)
     ;; any flags to reset according to circumstances?  How about a
     ;; flag indicating whether this bookmark is being visited?
     ;; well, we don't have this now, so maybe later.
     (insert " "))
   (forward-line (if backup -1 1))
   (bookmark-bmenu-ensure-position)))


(defun bookmark-bmenu-backup-unmark ()
  "Move up and cancel all requested operations on bookmark on line above."
  (interactive)
  (forward-line -1)
  (bookmark-bmenu-ensure-position)
  (bookmark-bmenu-unmark)
  (forward-line -1)
  (bookmark-bmenu-ensure-position))


(defun bookmark-bmenu-delete ()
  "Mark bookmark on this line to be deleted.
To carry out the deletions that you've marked, use \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-execute-deletions]."
  (interactive)
  (beginning-of-line)
  (bookmark-bmenu-ensure-position)
  (with-buffer-modified-unmodified
   (let ((inhibit-read-only t))
     (delete-char 1)
     (insert ?D)
     (forward-line 1)
     (bookmark-bmenu-ensure-position))))


(defun bookmark-bmenu-delete-backwards ()
  "Mark bookmark on this line to be deleted, then move up one line.
To carry out the deletions that you've marked, use \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-execute-deletions]."
  (interactive)
  (bookmark-bmenu-delete)
  (forward-line -2)
  (bookmark-bmenu-ensure-position)
  (forward-line 1)
  (bookmark-bmenu-ensure-position))


(defun bookmark-bmenu-execute-deletions ()
  "Delete bookmarks flagged `D'."
  (interactive)
  (let ((reporter (make-progress-reporter "Deleting bookmarks..."))
        (o-point  (point))
        (o-str    (save-excursion
                    (beginning-of-line)
                    (unless (= (following-char) ?D)
                      (buffer-substring
                       (point)
                       (progn (end-of-line) (point))))))
        (o-col     (current-column)))
    (goto-char (point-min))
    (unless bookmark-bmenu-use-header-line
      (forward-line 1))
    (while (re-search-forward "^D" (point-max) t)
      (bookmark-delete (bookmark-bmenu-bookmark) t)) ; pass BATCH arg
    (bookmark-bmenu-list)
    (if o-str
        (progn
          (goto-char (point-min))
          (search-forward o-str)
          (beginning-of-line)
          (forward-char o-col))
      (goto-char o-point))
    (beginning-of-line)
    (progress-reporter-done reporter)))


(defun bookmark-bmenu-rename ()
  "Rename bookmark on current line.  Prompts for a new name."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark))
        (thispoint (point)))
    (bookmark-rename bmrk)
    (goto-char thispoint)))


(defun bookmark-bmenu-locate ()
  "Display location of this bookmark.  Displays in the minibuffer."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark)))
    (message "%s" (bookmark-location bmrk))))

(defun bookmark-bmenu-relocate ()
  "Change the absolute file name of the bookmark on the current line.
Prompt with completion for the new path."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark))
        (thispoint (point)))
    (bookmark-relocate bmrk)
    (goto-char thispoint)))

;;; Bookmark-bmenu search

(defun bookmark-bmenu-filter-alist-by-regexp (regexp)
  "Filter `bookmark-alist' with bookmarks matching REGEXP and rebuild list."
  (let ((bookmark-alist
         (cl-loop for i in bookmark-alist
                  when (string-match regexp (car i)) collect i into new
                  finally return new)))
    (bookmark-bmenu-list)))


;;;###autoload
(defun bookmark-bmenu-search ()
  "Incremental search of bookmarks, hiding the non-matches as we go."
  (interactive)
  (let ((bmk (bookmark-bmenu-bookmark))
        (timer nil))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq timer (run-with-idle-timer
                           bookmark-search-delay 'repeat
                           #'(lambda (buf)
                               (with-current-buffer buf
                                 (bookmark-bmenu-filter-alist-by-regexp
                                  (minibuffer-contents))))
                           (current-buffer))))
          (read-string "Pattern: ")
          (when timer (cancel-timer timer) (setq timer nil)))
      (when timer ;; Signaled an error or a `quit'.
        (cancel-timer timer)
        (bookmark-bmenu-list)
        (bookmark-bmenu-goto-bookmark bmk)))))

(defun bookmark-bmenu-goto-bookmark (name)
  "Move point to bookmark with name NAME."
  (goto-char (point-min))
  (while (not (or (equal name (bookmark-bmenu-bookmark))
                  (eobp)))
    (forward-line 1))
  (forward-line 0))



;;; Menu bar stuff.  Prefix is "bookmark-menu".

(defun bookmark-menu-popup-paned-menu (event name entries)
  "Pop up multi-paned menu at EVENT, return string chosen from ENTRIES.
That is, ENTRIES is a list of strings which appear as the choices
in the menu.
The number of panes depends on the number of entries.
The visible entries are truncated to `bookmark-menu-length', but the
strings returned are not."
  (let ((f-height (/ (frame-height) 2))
	(pane-list nil)
	(iter 0))
    (while entries
      (let (lst
	    (count 0))
	(while (and (< count f-height) entries)
	  (let ((str (car entries)))
	    (push (cons
		   (if (> (length str) bookmark-menu-length)
		       (substring str 0 bookmark-menu-length)
		     str)
		   str)
		  lst)
	    (setq entries (cdr entries))
	    (setq count (1+ count))))
	(setq iter (1+ iter))
	(push (cons
	       (format "-*- %s (%d) -*-" name iter)
	       (nreverse lst))
	      pane-list)))

    ;; Popup the menu and return the string.
    (x-popup-menu event (cons (concat "-*- " name " -*-")
			      (nreverse pane-list)))))


;; Thanks to Roland McGrath for fixing menubar.el so that the
;; following works, and for explaining what to do to make it work.

;; We MUST autoload EACH form used to set up this variable's value, so
;; that the whole job is done in loaddefs.el.

;; Emacs menubar stuff.

;;;###autoload
(defvar menu-bar-bookmark-map
  (let ((map (make-sparse-keymap "Bookmark functions")))
    (bindings--define-key map [load]
      '(menu-item "Load a Bookmark File..." bookmark-load
		  :help "Load bookmarks from a bookmark file)"))
    (bindings--define-key map [write]
      '(menu-item "Save Bookmarks As..." bookmark-write
		  :help "Write bookmarks to a file (reading the file name with the minibuffer)"))
    (bindings--define-key map [save]
      '(menu-item "Save Bookmarks" bookmark-save
		  :help "Save currently defined bookmarks"))
    (bindings--define-key map [edit]
      '(menu-item "Edit Bookmark List" bookmark-bmenu-list
		  :help "Display a list of existing bookmarks"))
    (bindings--define-key map [delete]
      '(menu-item "Delete Bookmark..." bookmark-delete
		  :help "Delete a bookmark from the bookmark list"))
    (bindings--define-key map [rename]
      '(menu-item "Rename Bookmark..." bookmark-rename
		  :help "Change the name of a bookmark"))
    (bindings--define-key map [locate]
      '(menu-item "Insert Location..." bookmark-locate
		  :help "Insert the name of the file associated with a bookmark"))
    (bindings--define-key map [insert]
      '(menu-item "Insert Contents..." bookmark-insert
		  :help "Insert the text of the file pointed to by a bookmark"))
    (bindings--define-key map [set]
      '(menu-item "Set Bookmark..." bookmark-set
		  :help "Set a bookmark named inside a file."))
    (bindings--define-key map [jump]
      '(menu-item "Jump to Bookmark..." bookmark-jump
		  :help "Jump to a bookmark (a point in some file)"))
    map))

;;;###autoload
(defalias 'menu-bar-bookmark-map menu-bar-bookmark-map)

;; make bookmarks appear toward the right side of the menu.
(if (boundp 'menu-bar-final-items)
    (if menu-bar-final-items
        (push 'bookmark menu-bar-final-items))
  (setq menu-bar-final-items '(bookmark)))

;;;; end bookmark menu stuff ;;;;


;; Load Hook
(defvar bookmark-load-hook nil
  "Hook run at the end of loading library `bookmark.el'.")

;; Exit Hook, called from kill-emacs-hook
(defvar bookmark-exit-hook nil
  "Hook run when Emacs exits.")

(defun bookmark-exit-hook-internal ()
  "Save bookmark state, if necessary, at Emacs exit time.
This also runs `bookmark-exit-hook'."
  (run-hooks 'bookmark-exit-hook)
  (and (bookmark-time-to-save-p t)
       (bookmark-save)))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'bookmark-exit-hook-internal))

(defun bookmark-unload-function ()
  "Unload the Bookmark library."
  (when bookmark-save-flag (bookmark-save))
  ;; continue standard unloading
  nil)


(run-hooks 'bookmark-load-hook)

(provide 'bookmark)

;;; bookmark.el ends here
