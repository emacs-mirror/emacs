;;; bookmark.el --- set bookmarks, maybe annotate them, jump to them later

;; Copyright (C) 1993, 1994, 1995, 1996, 1997, 2001, 2002, 2003,
;;   2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Karl Fogel <kfogel@red-bean.com>
;; Maintainer: Karl Fogel <kfogel@red-bean.com>
;; Created: July, 1993
;; Keywords: bookmarks, placeholders, annotations

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package is for setting "bookmarks" in files.  A bookmark
;; associates a string with a location in a certain file.  Thus, you
;; can navigate your way to that location by providing the string.
;; See the "User Variables" section for customizations.

;; Thanks to David Bremner <bremner@cs.sfu.ca> for thinking of and
;; then implementing the bookmark-current-bookmark idea.  He even
;; sent *patches*, bless his soul...

;; Thanks to Gregory M. Saunders <saunders@cis.ohio-state.edu> for
;; fixing and improving bookmark-time-to-save-p.

;; Thanks go to Andrew V. Klein <avk@cig.mot.com> for the code that
;; sorts the alist before presenting it to the user (in bookmark-bmenu-list
;; and the menu-bar).

;; And much thanks to David Hughes <djh@harston.cv.com> for many small
;; suggestions and the code to implement them (like
;; bookmark-bmenu-check-position, and some of the Lucid compatibility
;; stuff).

;; Kudos (whatever they are) go to Jim Blandy <jimb@red-bean.com>
;; for his eminently sensible suggestion to separate bookmark-jump
;; into bookmark-jump and bookmark-jump-noselect, which made many
;; other things cleaner as well.

;; Thanks to Roland McGrath for encouragement and help with defining
;; autoloads on the menu-bar.

;; Jonathan Stigelman <stig@hackvan.com> gave patches for default
;; values in bookmark-jump and bookmark-set.  Everybody please keep
;; all the keystrokes they save thereby and send them to him at the
;; end of each year :-)  (No, seriously, thanks Jonathan!)

;; Buckets of gratitude to John Grabowski <johng@media.mit.edu> for
;; thinking up the annotations feature and implementing it so well.

;; Based on info-bookmark.el, by Karl Fogel and Ken Olstad
;; <olstad@msc.edu>.

;; Thanks to Mikio Nakajima <PBC01764@niftyserve.or.jp> for many bugs
;; reported and fixed.

;; Thank you, Michael Kifer, for contributing the XEmacs support.

;; Enough with the credits already, get on to the good stuff:

;; FAVORITE CHINESE RESTAURANT:
;; Boy, that's a tough one.  Probably Hong Min, or maybe Emperor's
;; Choice (both in Chicago's Chinatown).  Well, both.  How about you?

;;; Code:

(require 'pp)

;;; Misc comments:
;;
;; If variable bookmark-use-annotations is non-nil, an annotation is
;; queried for when setting a bookmark.
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
  "*If non-nil, saving a bookmark queries for an annotation in a buffer."
  :type 'boolean
  :group 'bookmark)


(defcustom bookmark-save-flag t
  "*Controls when Emacs saves bookmarks to a file.
--> nil means never save bookmarks, except when `bookmark-save' is
    explicitly called \(\\[bookmark-save]\).
--> t means save bookmarks when Emacs is killed.
--> Otherwise, it should be a number that is the frequency with which
    the bookmark list is saved \(i.e.: the number of times which
    Emacs' bookmark list may be modified before it is automatically
    saved.\).  If it is a number, Emacs will also automatically save
    bookmarks when it is killed.

Therefore, the way to get it to save every time you make or delete a
bookmark is to set this variable to 1 \(or 0, which produces the same
behavior.\)

To specify the file in which to save them, modify the variable
`bookmark-default-file', which is `~/.emacs.bmk' by default."
  :type '(choice (const nil) integer (other t))
  :group 'bookmark)


(defconst bookmark-old-default-file "~/.emacs-bkmrks"
  "*The `.emacs.bmk' file used to be called this name.")


;; defvarred to avoid a compilation warning:
(defvar bookmark-file nil
  "Old name for `bookmark-default-file'.")

(defcustom bookmark-default-file
  (if bookmark-file
      ;; In case user set `bookmark-file' in her .emacs:
      bookmark-file
    (convert-standard-filename "~/.emacs.bmk"))
  "*File in which to save bookmarks by default."
  :type 'file
  :group 'bookmark)


(defcustom bookmark-version-control 'nospecial
  "*Whether or not to make numbered backups of the bookmark file.
It can have four values: t, nil, `never', and `nospecial'.
The first three have the same meaning that they do for the
variable `version-control', and the final value `nospecial' means just
use the value of `version-control'."
  :type '(choice (const nil) (const never) (const nospecial)
		 (other t))
  :group 'bookmark)


(defcustom bookmark-completion-ignore-case t
  "*Non-nil means bookmark functions ignore case in completion."
  :type 'boolean
  :group 'bookmark)


(defcustom bookmark-sort-flag t
  "*Non-nil means that bookmarks will be displayed sorted by bookmark name.
Otherwise they will be displayed in LIFO order (that is, most
recently set ones come first, oldest ones come last)."
  :type 'boolean
  :group 'bookmark)


(defcustom bookmark-automatically-show-annotations t
  "*Non-nil means show annotations when jumping to a bookmark."
  :type 'boolean
  :group 'bookmark)


(defcustom bookmark-bmenu-file-column 30
  "*Column at which to display filenames in a buffer listing bookmarks.
You can toggle whether files are shown with \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-toggle-filenames]."
  :type 'integer
  :group 'bookmark)


(defcustom bookmark-bmenu-toggle-filenames t
  "*Non-nil means show filenames when listing bookmarks.
This may result in truncated bookmark names.  To disable this, put the
following in your `.emacs' file:

\(setq bookmark-bmenu-toggle-filenames nil\)"
  :type 'boolean
  :group 'bookmark)


(defcustom bookmark-menu-length 70
  "*Maximum length of a bookmark name displayed on a popup menu."
  :type 'integer
  :group 'bookmark)


(defface bookmark-menu-heading
  '((t (:inherit font-lock-type-face)))
  "Face used to highlight the heading in bookmark menu buffers."
  :group 'bookmark
  :version "22.1")


;;; No user-serviceable parts beyond this point.

;; Added  for lucid emacs  compatibility, db
(or (fboundp 'defalias)  (fset 'defalias 'fset))

;; suggested for lucid compatibility by david hughes:
(or (fboundp 'frame-height)  (defalias 'frame-height 'screen-height))


;;; Keymap stuff:

;; Set up these bindings dumping time *only*;
;; if the user alters them, don't override the user when loading bookmark.el.

;;;###autoload (define-key ctl-x-map "rb" 'bookmark-jump)
;;;###autoload (define-key ctl-x-map "rm" 'bookmark-set)
;;;###autoload (define-key ctl-x-map "rl" 'bookmark-bmenu-list)

;;;###autoload
(defvar bookmark-map
  (let ((map (make-sparse-keymap)))
    ;; Read the help on all of these functions for details...
    (define-key map "x" 'bookmark-set)
    (define-key map "m" 'bookmark-set) ;"m"ark
    (define-key map "j" 'bookmark-jump)
    (define-key map "g" 'bookmark-jump) ;"g"o
    (define-key map "o" 'bookmark-jump-other-window)
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
key of your choice to `bookmark-map'.  All interactive bookmark
functions have a binding in this keymap.")

;;;###autoload (fset 'bookmark-map bookmark-map)

;;; The annotation maps.
(defvar bookmark-read-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'bookmark-send-annotation)
    map)
  "Keymap for composing an annotation for a bookmark.")


;;; Core variables and data structures:
(defvar bookmark-alist ()
  "Association list of bookmarks and their records.
You probably don't want to change the value of this alist yourself;
instead, let the various bookmark functions do it for you.

The format of the alist is

       \(BOOKMARK1 BOOKMARK2 ...\)

where each BOOKMARK is typically of the form

\(NAME
  \(filename . FILE\)
  \(front-context-string . FRONT-STR\)
  \(rear-context-string  . REAR-STR\)
  \(position . POS\)
  \(annotation . ANNOTATION\)\)

So the cdr of each bookmark is an alist too.")


(defvar bookmarks-already-loaded nil)


;; more stuff added by db.

(defvar bookmark-current-bookmark nil
  "Name of bookmark most recently used in the current file.
It is buffer local, used to make moving a bookmark forward
through a file easier.")

(make-variable-buffer-local 'bookmark-current-bookmark)


(defvar bookmark-alist-modification-count 0
  "Number of modifications to bookmark list since it was last saved.")


(defvar bookmark-search-size 16
  "Length of the context strings recorded on either side of a bookmark.")


(defvar bookmark-current-point 0)
(defvar bookmark-yank-point 0)
(defvar bookmark-current-buffer nil)

(defvar Info-current-node)
(defvar Info-suffix-list)

;; Helper functions.

;; Only functions on this page and the next one (file formats) need to
;; know anything about the format of bookmark-alist entries.
;; Everyone else should go through them.


(defun bookmark-name-from-full-record (full-record)
  "Return name of FULL-RECORD \(an alist element instead of a string\)."
  (car full-record))


(defun bookmark-all-names ()
  "Return a list of all current bookmark names."
  (bookmark-maybe-load-default-file)
  (mapcar
   (lambda (full-record)
     (bookmark-name-from-full-record full-record))
   bookmark-alist))


(defun bookmark-get-bookmark (bookmark)
  "Return the full entry for BOOKMARK in `bookmark-alist'.
If BOOKMARK is not a string, return nil."
  (when (stringp bookmark)
    (assoc-string bookmark bookmark-alist bookmark-completion-ignore-case)))


(defun bookmark-get-bookmark-record (bookmark)
  "Return the guts of the entry for BOOKMARK in `bookmark-alist'.
That is, all information but the name."
  (car (cdr (bookmark-get-bookmark bookmark))))


(defun bookmark-set-name (bookmark newname)
  "Set BOOKMARK's name to NEWNAME."
  (setcar
   (if (stringp bookmark) (bookmark-get-bookmark bookmark) bookmark)
   newname))

(defun bookmark-prop-get (bookmark prop)
  "Return the property PROP of BOOKMARK, or nil if none."
  (cdr (assq prop (bookmark-get-bookmark-record bookmark))))

(defun bookmark-prop-set (bookmark prop val)
  "Set the property PROP of BOOKMARK to VAL."
  (let ((cell (assq prop (bookmark-get-bookmark-record bookmark))))
    (if cell
        (setcdr cell val)
      (nconc (bookmark-get-bookmark-record bookmark)
             (list (cons prop val))))))

(defun bookmark-get-annotation (bookmark)
  "Return the annotation of BOOKMARK, or nil if none."
  (bookmark-prop-get bookmark 'annotation))

(defun bookmark-set-annotation (bookmark ann)
  "Set the annotation of BOOKMARK to ANN."
  (bookmark-prop-set bookmark 'annotation ann))


(defun bookmark-get-filename (bookmark)
  "Return the full filename of BOOKMARK."
  (bookmark-prop-get bookmark 'filename))


(defun bookmark-set-filename (bookmark filename)
  "Set the full filename of BOOKMARK to FILENAME."
  (bookmark-prop-set bookmark 'filename filename)
  (setq bookmark-alist-modification-count
        (1+ bookmark-alist-modification-count))
  (if (bookmark-time-to-save-p)
      (bookmark-save)))


(defun bookmark-get-position (bookmark)
  "Return the position \(i.e.: point\) of BOOKMARK."
  (bookmark-prop-get bookmark 'position))


(defun bookmark-set-position (bookmark position)
  "Set the position \(i.e.: point\) of BOOKMARK to POSITION."
  (bookmark-prop-set bookmark 'position position))


(defun bookmark-get-front-context-string (bookmark)
  "Return the front-context-string of BOOKMARK."
  (bookmark-prop-get bookmark 'front-context-string))


(defun bookmark-set-front-context-string (bookmark string)
  "Set the front-context-string of BOOKMARK to STRING."
  (bookmark-prop-set bookmark 'front-context-string string))


(defun bookmark-get-rear-context-string (bookmark)
  "Return the rear-context-string of BOOKMARK."
  (bookmark-prop-get bookmark 'rear-context-string))


(defun bookmark-set-rear-context-string (bookmark string)
  "Set the rear-context-string of BOOKMARK to STRING."
  (bookmark-prop-set bookmark 'rear-context-string string))


(defun bookmark-get-handler (bookmark)
  (bookmark-prop-get bookmark 'handler))

(defvar bookmark-history nil
  "The history list for bookmark functions.")


(defun bookmark-completing-read (prompt &optional default)
  "Prompting with PROMPT, read a bookmark name in completion.
PROMPT will get a \": \" stuck on the end no matter what, so you
probably don't want to include one yourself.
Optional second arg DEFAULT is a string to return if the user enters
the empty string."
  (bookmark-maybe-load-default-file) ; paranoia
  (if (listp last-nonmenu-event)
      (bookmark-menu-popup-paned-menu t prompt (bookmark-all-names))
    (let* ((completion-ignore-case bookmark-completion-ignore-case)
	   (default default)
	   (prompt (if default
		       (concat prompt (format " (%s): " default))
		     (concat prompt ": ")))
	   (str
	    (completing-read prompt
			     bookmark-alist
			     nil
			     0
			     nil
			     'bookmark-history)))
      (if (string-equal "" str) default str))))


(defmacro bookmark-maybe-historicize-string (string)
  "Put STRING into the bookmark prompt history, if caller non-interactive.
We need this because sometimes bookmark functions are invoked from
menus, so `completing-read' never gets a chance to set `bookmark-history'."
  `(or
    (interactive-p)
    (setq bookmark-history (cons ,string bookmark-history))))

(defvar bookmark-make-name-function nil
  "A function that should be called to return the name of the bookmark.
When called with an argument, the function should return a file
name -- or whatever is required to jump to the location.  Modes
may set this variable buffer-locally to enable a default name to
be proposed when calling `bookmark-set'.")

(defvar bookmark-make-record-function 'bookmark-make-record-for-text-file
  "A function that should be called to create a bookmark record.
Modes may set this variable buffer-locally to enable bookmarking of
locations that should be treated specially, such as Info nodes,
news posts, images, pdf documents, etc.

The function will be called with no arguments.
The returned record may contain a special cons (handler . SOME-FUNCTION)
which sets the handler function that should be used to open this
bookmark instead of `bookmark-default-handler'.  The handler should
return an alist like the one that function returns, and (of course)
should likewise not select the buffer.")

(defun bookmark-make (name &optional annotation overwrite)
  "Make a bookmark named NAME.
Optional second arg ANNOTATION gives it an annotation.
Optional third arg OVERWRITE means replace any existing bookmarks with
this name."
  (bookmark-maybe-load-default-file)
  (let ((stripped-name (copy-sequence name)))
    (or (featurep 'xemacs)
        ;; XEmacs's `set-text-properties' doesn't work on
        ;; free-standing strings, apparently.
        (set-text-properties 0 (length stripped-name) nil stripped-name))
    (if (and (bookmark-get-bookmark stripped-name) (not overwrite))
        ;; already existing bookmark under that name and
        ;; no prefix arg means just overwrite old bookmark
        (setcdr (bookmark-get-bookmark stripped-name)
                (list (funcall bookmark-make-record-function)))

      ;; otherwise just cons it onto the front (either the bookmark
      ;; doesn't exist already, or there is no prefix arg.  In either
      ;; case, we want the new bookmark consed onto the alist...)

      (push (list stripped-name
                  (funcall bookmark-make-record-function))
            bookmark-alist))

    (when annotation
      ;; Take no chances with text properties.
      (set-text-properties 0 (length annotation) nil annotation)
      (bookmark-prop-set stripped-name 'annotation annotation))

    ;; Added by db
    (setq bookmark-current-bookmark stripped-name)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (bookmark-save))))


(defun bookmark-make-record-for-text-file ()
  "Return the record describing the location of a new bookmark.
Must be at the correct position in the buffer in which the bookmark is
being set (this might change someday)."
  `((filename . ,(bookmark-buffer-file-name))
    (front-context-string
     . ,(if (>= (- (point-max) (point)) bookmark-search-size)
            (buffer-substring-no-properties
             (point)
             (+ (point) bookmark-search-size))
          nil))
    (rear-context-string
     . ,(if (>= (- (point) (point-min)) bookmark-search-size)
            (buffer-substring-no-properties
             (point)
             (- (point) bookmark-search-size))
          nil))
    (position . ,(point))))


;;; File format stuff

;; The OLD format of the bookmark-alist was:
;;
;;       ((bookmark-name (filename
;;                        string-in-front
;;                        string-behind
;;                        point))
;;        ...)
;;
;; The NEW format of the bookmark-alist is:
;;
;;       ((bookmark-name ((filename . FILENAME)
;;                        (front-context-string . string-in-front)
;;                        (rear-context-string  . string-behind)
;;                        (position . POINT)
;;                        (annotation . annotation)
;;                        (whatever   . VALUE)
;;                        ...
;;                        ))
;;        ...)
;;
;;
;; I switched to using an internal as well as external alist because I
;; felt that would be a more flexible framework in which to add
;; features.  It means that the order in which values appear doesn't
;; matter, and it means that arbitrary values can be added without
;; risk of interfering with existing ones.
;;
;; BOOKMARK-NAME is the string the user gives the bookmark and
;; accesses it by from then on.
;;
;; FILENAME is the location of the file in which the bookmark is set.
;;
;; STRING-IN-FRONT is a string of `bookmark-search-size' chars of
;; context in front of the point at which the bookmark is set.
;;
;; STRING-BEHIND is the same thing, but after the point.
;;
;; The context strings exist so that modifications to a file don't
;; necessarily cause a bookmark's position to be invalidated.
;; bookmark-jump will search for STRING-BEHIND and STRING-IN-FRONT in
;; case the file has changed since the bookmark was set.  It will
;; attempt to place the user before the changes, if there were any.
;; ANNOTATION is the annotation for the bookmark; it may not exist
;; (for backward compatibility), be nil (no annotation), or be a
;; string.


(defconst bookmark-file-format-version 1
  "The current version of the format used by bookmark files.
You should never need to change this.")


(defconst bookmark-end-of-version-stamp-marker
  "-*- End Of Bookmark File Format Version Stamp -*-\n"
  "This string marks the end of the version stamp in a bookmark file.")


(defun bookmark-alist-from-buffer ()
  "Return a `bookmark-alist' (in any format) from the current buffer.
The buffer must of course contain bookmark format information.
Does not care from where in the buffer it is called, and does not
affect point."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward bookmark-end-of-version-stamp-marker nil t)
        (read (current-buffer))
      ;; Else we're dealing with format version 0
      (if (search-forward "(" nil t)
          (progn
            (forward-char -1)
            (read (current-buffer)))
        ;; Else no hope of getting information here.
        (error "Not bookmark format")))))


(defun bookmark-upgrade-version-0-alist (old-list)
  "Upgrade a version 0 alist OLD-LIST to the current version."
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
  (message "Upgrading bookmark format from 0 to %d..."
           bookmark-file-format-version)
  (let* ((old-list (bookmark-alist-from-buffer))
         (new-list (bookmark-upgrade-version-0-alist old-list)))
    (delete-region (point-min) (point-max))
    (bookmark-insert-file-format-version-stamp)
    (pp new-list (current-buffer))
    (save-buffer))
  (goto-char (point-min))
  (message "Upgrading bookmark format from 0 to %d...done"
           bookmark-file-format-version)
  )


(defun bookmark-grok-file-format-version ()
  "Return an integer which is the file-format version of this bookmark file.
This expects to be called from `point-min' in a bookmark file."
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
  (let ((version (bookmark-grok-file-format-version)))
    (cond
     ((= version bookmark-file-format-version)
      ) ; home free -- version is current
     ((= version 0)
      (bookmark-upgrade-file-format-from-0))
     (t
      (error "Bookmark file format version strangeness")))))


(defun bookmark-insert-file-format-version-stamp ()
  "Insert text indicating current version of bookmark file format."
  (insert
   (format ";;;; Emacs Bookmark Format Version %d ;;;;\n"
           bookmark-file-format-version))
  (insert ";;; This format is meant to be slightly human-readable;\n"
          ";;; nevertheless, you probably don't want to edit it.\n"
          ";;; "
          bookmark-end-of-version-stamp-marker))


;;; end file-format stuff


;;; Generic helpers.

(defun bookmark-maybe-message (fmt &rest args)
  "Apply `message' to FMT and ARGS, but only if the display is fast enough."
  (if (>= baud-rate 9600)
      (apply 'message fmt args)))


;;; Core code:

;;;###autoload
(defun bookmark-set (&optional name parg)
  "Set a bookmark named NAME inside a file.
If name is nil, then the user will be prompted.
With prefix arg, will not overwrite a bookmark that has the same name
as NAME if such a bookmark already exists, but instead will \"push\"
the new bookmark onto the bookmark alist.  Thus the most recently set
bookmark with name NAME would be the one in effect at any given time,
but the others are still there, should you decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts the name of the last bookmark used in the buffer
\(as an aid in using a single bookmark name to track your progress
through a large file\).  If no bookmark was used, then C-u inserts the
name of the file being visited.

Use \\[bookmark-delete] to remove bookmarks \(you give it a name,
and it removes only the first instance of a bookmark with that name from
the list of bookmarks.\)"
  (interactive (list nil current-prefix-arg))
  (or
   (bookmark-buffer-file-name)
   (error "Buffer not visiting a file or directory"))

  (bookmark-maybe-load-default-file)

  (setq bookmark-current-point (point))
  (setq bookmark-yank-point (point))
  (setq bookmark-current-buffer (current-buffer))

  (let* ((default (or bookmark-current-bookmark
                      (bookmark-buffer-name)))
	 (str
	  (or name
              (read-from-minibuffer
               (format "Set bookmark (%s): " default)
               nil
               (let ((now-map (copy-keymap minibuffer-local-map)))
                 (define-key now-map "\C-w" 'bookmark-yank-word)
                 (define-key now-map "\C-u" 'bookmark-insert-current-bookmark)
                 now-map))))
	 (annotation nil))
    (and (string-equal str "") (setq str default))
    ;; Ask for an annotation buffer for this bookmark
    (if bookmark-use-annotations
	(bookmark-read-annotation parg str)
      (bookmark-make str annotation parg)
      (setq bookmark-current-bookmark str)
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (goto-char bookmark-current-point))))


(defun bookmark-kill-line (&optional newline-too)
  "Kill from point to end of line.
If optional arg NEWLINE-TOO is non-nil, delete the newline too.
Does not affect the kill ring."
  (let ((eol (save-excursion (end-of-line) (point))))
    (delete-region (point) eol)
    (if (and newline-too (looking-at "\n"))
        (delete-char 1))))


;; Defvars to avoid compilation warnings:
(defvar bookmark-annotation-paragraph nil)
(defvar bookmark-annotation-name nil)
(defvar bookmark-annotation-buffer nil)
(defvar bookmark-annotation-file nil)
(defvar bookmark-annotation-point nil)


(defun bookmark-send-annotation ()
  "Use buffer contents as the annotation for a bookmark.
Exclude lines that begin with `#'.
Store the annotation text in the bookmark list with
the bookmark (and file, and point) specified in buffer local variables."
  (interactive)
  (if (not (eq major-mode 'bookmark-read-annotation-mode))
      (error "Not in bookmark-read-annotation-mode"))
  (goto-char (point-min))
  (while (< (point) (point-max))
    (if (looking-at "^#")
        (bookmark-kill-line t)
      (forward-line 1)))
  (let ((annotation (buffer-string))
	(parg bookmark-annotation-paragraph)
	(bookmark bookmark-annotation-name)
	(pt bookmark-annotation-point)
	(buf bookmark-annotation-buffer))
    ;; for bookmark-make-record-function to work, we need to be
    ;; in the relevant buffer, at the relevant point.
    ;; Actually, the bookmark-make-record-function spec should
    ;; probably be changed to avoid this need.  Should I handle the
    ;; error if a buffer is killed between "C-x r m" and a "C-c C-c"
    ;; in the annotation buffer?
    (save-excursion
      (pop-to-buffer buf)
      (goto-char pt)
      (bookmark-make bookmark annotation parg)
      (setq bookmark-current-bookmark bookmark))
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (goto-char bookmark-current-point))
  (kill-buffer (current-buffer)))


(defun bookmark-default-annotation-text (bookmark)
  (concat "#  Type the annotation for bookmark '" bookmark "' here.\n"
	  "#  All lines which start with a '#' will be deleted.\n"
	  "#  Type C-c C-c when done.\n#\n"
	  "#  Author: " (user-full-name) " <" (user-login-name) "@"
	  (system-name) ">\n"
	  "#  Date:    " (current-time-string) "\n"))


(defvar bookmark-read-annotation-text-func 'bookmark-default-annotation-text
  "Function to return default text to use for a bookmark annotation.
It takes one argument, the name of the bookmark, as a string.")

(defun bookmark-read-annotation-mode (buf point parg bookmark)
  "Mode for composing annotations for a bookmark.
Wants BUF, POINT, PARG, and BOOKMARK.
When you have finished composing, type \\[bookmark-send-annotation] to send
the annotation.

\\{bookmark-read-annotation-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'bookmark-annotation-paragraph)
  (make-local-variable 'bookmark-annotation-name)
  (make-local-variable 'bookmark-annotation-buffer)
  (make-local-variable 'bookmark-annotation-file)
  (make-local-variable 'bookmark-annotation-point)
  (setq bookmark-annotation-paragraph parg)
  (setq bookmark-annotation-name bookmark)
  (setq bookmark-annotation-buffer buf)
  (setq bookmark-annotation-file (buffer-file-name buf))
  (setq bookmark-annotation-point point)
  (use-local-map bookmark-read-annotation-mode-map)
  (setq major-mode 'bookmark-read-annotation-mode)
  (insert (funcall bookmark-read-annotation-text-func bookmark))
  (run-mode-hooks 'text-mode-hook))


(defun bookmark-read-annotation (parg bookmark)
  "Pop up a buffer for entering a bookmark annotation.
Text surrounding the bookmark is PARG; the bookmark name is BOOKMARK."
  (let ((buf (current-buffer))
	(point (point)))
    (pop-to-buffer (generate-new-buffer-name "*Bookmark Annotation Compose*"))
    (bookmark-read-annotation-mode buf point parg bookmark)))


(defvar bookmark-edit-annotation-mode-map (copy-keymap text-mode-map)
  "Keymap for editing an annotation of a bookmark.")


(define-key bookmark-edit-annotation-mode-map "\C-c\C-c"
  'bookmark-send-edited-annotation)


(defun bookmark-edit-annotation-mode (bookmark)
  "Mode for editing the annotation of bookmark BOOKMARK.
When you have finished composing, type \\[bookmark-send-annotation].

\\{bookmark-edit-annotation-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'bookmark-annotation-name)
  (setq bookmark-annotation-name bookmark)
  (use-local-map bookmark-edit-annotation-mode-map)
  (setq major-mode 'bookmark-edit-annotation-mode
        mode-name "Edit Bookmark Annotation")
  (insert (funcall bookmark-read-annotation-text-func bookmark))
  (let ((annotation (bookmark-get-annotation bookmark)))
    (if (and annotation (not (string-equal annotation "")))
	(insert annotation)))
  (run-mode-hooks 'text-mode-hook))


(defun bookmark-send-edited-annotation ()
  "Use buffer contents as annotation for a bookmark.
Lines beginning with `#' are ignored."
  (interactive)
  (if (not (eq major-mode 'bookmark-edit-annotation-mode))
      (error "Not in bookmark-edit-annotation-mode"))
  (goto-char (point-min))
  (while (< (point) (point-max))
    (if (looking-at "^#")
        (bookmark-kill-line t)
      (forward-line 1)))
  (let ((annotation (buffer-string))
	(bookmark bookmark-annotation-name))
    (bookmark-set-annotation bookmark annotation)
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (goto-char bookmark-current-point))
  (kill-buffer (current-buffer)))


(defun bookmark-edit-annotation (bookmark)
  "Pop up a buffer for editing bookmark BOOKMARK's annotation."
  (pop-to-buffer (generate-new-buffer-name "*Bookmark Annotation Compose*"))
  (bookmark-edit-annotation-mode bookmark))


(defun bookmark-insert-current-bookmark ()
  "Insert this buffer's value of `bookmark-current-bookmark'.
Default to file name if it's nil."
  (interactive)
  (let ((str
	 (save-excursion
	   (set-buffer bookmark-current-buffer)
	   bookmark-current-bookmark)))
    (if str (insert str) (bookmark-insert-buffer-name))))


(defun bookmark-insert-buffer-name ()
  "Insert the current file name into the bookmark name being set.
The directory part of the file name is not used."
  (interactive)
  (let ((str
         (save-excursion
           (set-buffer bookmark-current-buffer)
           (bookmark-buffer-name))))
    (insert str)))


(defun bookmark-buffer-name ()
  "Return the name of the current buffer's file, non-directory.
In Info, return the current node."
  (cond
   ;; Is the mode defining the bookmark buffer name?
   (bookmark-make-name-function
    (funcall bookmark-make-name-function))
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
  (interactive)
  ;; get the next word from the buffer and append it to the name of
  ;; the bookmark currently being set.
  (let ((string (save-excursion
                    (set-buffer bookmark-current-buffer)
                    (goto-char bookmark-yank-point)
                    (buffer-substring-no-properties
                     (point)
                     (progn
                       (forward-word 1)
                       (setq bookmark-yank-point (point)))))))
    (insert string)))


(defvar Info-current-file)

(defun bookmark-buffer-file-name ()
  "Return the current buffer's file in a way useful for bookmarks.
For example, if this is a Info buffer, return the Info file's name."
  (cond
   ;; Return the location the handler should jump to
   ;; E.g. the Info file name for the Info handler
   (bookmark-make-name-function
    (funcall bookmark-make-name-function t))
   (buffer-file-name
    ;; Abbreviate the path, both so it's shorter and so it's more
    ;; portable.  E.g., the user's home dir might be a different
    ;; path on different machines, but "~/" will still reach it.
    (abbreviate-file-name buffer-file-name))
   ((and (boundp 'dired-directory) dired-directory)
    (if (stringp dired-directory)
        dired-directory
      (car dired-directory)))))


(defun bookmark-maybe-load-default-file ()
  (and (not bookmarks-already-loaded)
       (null bookmark-alist)
       (prog2
           (and
            ;; Possibly the old bookmark file, "~/.emacs-bkmrks", needs
            ;; to be renamed.
            (file-exists-p (expand-file-name bookmark-old-default-file))
            (not (file-exists-p (expand-file-name bookmark-default-file)))
            (rename-file (expand-file-name bookmark-old-default-file)
                         (expand-file-name bookmark-default-file)))
           ;; return t so the `and' will continue...
           t)

       (file-readable-p (expand-file-name bookmark-default-file))
       (bookmark-load bookmark-default-file t t)
       (setq bookmarks-already-loaded t)))


(defun bookmark-maybe-sort-alist ()
  ;;Return the bookmark-alist for display.  If the bookmark-sort-flag
  ;;is non-nil, then return a sorted copy of the alist.
  (if bookmark-sort-flag
      (sort (copy-alist bookmark-alist)
            (function
             (lambda (x y) (string-lessp (car x) (car y)))))
    bookmark-alist))


(defvar bookmark-after-jump-hook nil
  "Hook run after `bookmark-jump' jumps to a bookmark.
Useful for example to unhide text in `outline-mode'.")

;;;###autoload
(defun bookmark-jump (bookmark)
  "Jump to bookmark BOOKMARK (a point in some file).
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

If the file pointed to by BOOKMARK no longer exists, you will be asked
if you wish to give the bookmark a new location, and `bookmark-jump'
will then jump to the new location, as well as recording it in place
of the old one in the permanent bookmark record."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark"
				   bookmark-current-bookmark)))
  (unless bookmark
    (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark)
  (let ((alist (bookmark-jump-noselect bookmark)))
    (and alist
         (switch-to-buffer (cadr (assq 'buffer alist)))
         (goto-char (cadr (assq 'position alist)))
	 (progn (run-hooks 'bookmark-after-jump-hook) t)
	 (if bookmark-automatically-show-annotations
             ;; if there is an annotation for this bookmark,
             ;; show it in a buffer.
             (bookmark-show-annotation bookmark)))))


;;;###autoload
(defun bookmark-jump-other-window (bookmark)
  "Jump to BOOKMARK (a point in some file) in another window.
See `bookmark-jump'."
  (interactive
   (let ((bkm (bookmark-completing-read "Jump to bookmark (in another window)"
                                        bookmark-current-bookmark)))
     (if (> emacs-major-version 21)
         (list bkm) bkm)))
  (when bookmark
    (bookmark-maybe-historicize-string bookmark)
    (let ((alist (bookmark-jump-noselect bookmark)))
      (and alist
           (switch-to-buffer-other-window (cadr (assq 'buffer alist)))
           (goto-char (cadr (assq 'position alist)))
           (if bookmark-automatically-show-annotations
               ;; if there is an annotation for this bookmark,
               ;; show it in a buffer.
               (bookmark-show-annotation bookmark))))))


(defun bookmark-file-or-variation-thereof (file)
  "Return FILE (a string) if it exists, or return a reasonable
variation of FILE if that exists.  Reasonable variations are checked
by appending suffixes defined in `Info-suffix-list'.  If cannot find FILE
nor a reasonable variation thereof, then still return FILE if it can
be retrieved from a VC backend, else return nil."
  (if (file-exists-p file)
      file
    (or
     (progn (require 'info)  ; ensure Info-suffix-list is bound
            (catch 'found
              (mapc (lambda (elt)
                      (let ((suffixed-file (concat file (car elt))))
                        (if (file-exists-p suffixed-file)
                            (throw 'found suffixed-file))))
                    Info-suffix-list)
              nil))
     ;; Last possibility: try VC
     (if (vc-backend file) file))))

(defun bookmark-jump-noselect (bookmark)
  "Call BOOKMARK's handler or `bookmark-default-handler' if it has none."
  (let ((found (funcall (or (bookmark-get-handler bookmark)
                          'bookmark-default-handler)
                      bookmark)))
    (unless found
      ;; Else unable to find the marked file, so ask if user wants to
      ;; relocate the bookmark, else remind them to consider deletion.
      (let ((file (bookmark-get-filename bookmark)))
        (when file         ;Don't know how to relocate if there's no `file'.
          (setq file (expand-file-name file))
          (ding)
          (if (y-or-n-p (concat (file-name-nondirectory file)
                                " nonexistent.  Relocate \""
                                bookmark
                                "\"? "))
              (progn
                (bookmark-relocate bookmark)
                ;; Try again.
                (setq found (funcall (or (bookmark-get-handler bookmark)
                                         'bookmark-default-handler)
                                     bookmark)))
            (message
             "Bookmark not relocated; consider removing it \(%s\)." bookmark)))))
    (when found
      ;; Added by db.
      (setq bookmark-current-bookmark bookmark)
      found)))

(defun bookmark-default-handler (str)
  ;; Helper for bookmark-jump.  STR is a bookmark name, of the sort
  ;; accepted by `bookmark-get-bookmark'.
  ;;
  ;; Return an alist '((buffer BUFFER) (position POSITION) ...)
  ;; indicating the bookmarked point within the specied buffer.  Any
  ;; elements not documented here should be ignored.
  (bookmark-maybe-load-default-file)
  (let* ((file (expand-file-name (bookmark-get-filename str)))
         (forward-str            (bookmark-get-front-context-string str))
         (behind-str             (bookmark-get-rear-context-string str))
         (place                  (bookmark-get-position str)))
    ;; FIXME: bookmark-file-or-variation-thereof was needed for Info files,
    ;; but now that Info bookmarks are handled elsewhere it seems that we
    ;; should be able to get rid of it.  --Stef
    (if (setq file (bookmark-file-or-variation-thereof file))
        (with-current-buffer (find-file-noselect file)
          (goto-char place)

          ;; Go searching forward first.  Then, if forward-str exists and
          ;; was found in the file, we can search backward for behind-str.
          ;; Rationale is that if text was inserted between the two in the
          ;; file, it's better to be put before it so you can read it,
          ;; rather than after and remain perhaps unaware of the changes.
          (if forward-str
              (if (search-forward forward-str (point-max) t)
                  (goto-char (match-beginning 0))))
          (if behind-str
              (if (search-backward behind-str (point-min) t)
                  (goto-char (match-end 0))))
          `((buffer ,(current-buffer)) (position ,(point)))))))


;;;###autoload
(defun bookmark-relocate (bookmark)
  "Relocate BOOKMARK to another file (reading file name with minibuffer).
This makes an already existing bookmark point to that file, instead of
the one it used to point at.  Useful when a file has been renamed
after a bookmark was set in it."
  (interactive (list (bookmark-completing-read "Bookmark to relocate")))
  (bookmark-maybe-historicize-string bookmark)
  (bookmark-maybe-load-default-file)
  (let* ((bmrk-filename (bookmark-get-filename bookmark))
         (newloc (expand-file-name
                  (read-file-name
                   (format "Relocate %s to: " bookmark)
                   (file-name-directory bmrk-filename)))))
    (bookmark-set-filename bookmark newloc)
    (bookmark-bmenu-surreptitiously-rebuild-list)))


;;;###autoload
(defun bookmark-insert-location (bookmark &optional no-history)
  "Insert the name of the file associated with BOOKMARK.
Optional second arg NO-HISTORY means don't record this in the
minibuffer history list `bookmark-history'."
  (interactive (list (bookmark-completing-read "Insert bookmark location")))
  (or no-history (bookmark-maybe-historicize-string bookmark))
  (let ((start (point)))
    (prog1
	(insert (bookmark-location bookmark)) ; *Return this line*
      (if (and (display-color-p) (display-mouse-p))
	  (add-text-properties
	   start
	   (save-excursion (re-search-backward
			    "[^ \t]")
					       (1+ (point)))
	   '(mouse-face highlight
	     follow-link t
	     help-echo "mouse-2: go to this bookmark in other window"))))))

;;;###autoload
(defalias 'bookmark-locate 'bookmark-insert-location)

(defun bookmark-location (bookmark)
  "Return the name of the file associated with BOOKMARK."
  (bookmark-maybe-load-default-file)
  (bookmark-get-filename bookmark))


;;;###autoload
(defun bookmark-rename (old &optional new)
  "Change the name of OLD bookmark to NEW name.
If called from keyboard, prompt for OLD and NEW.  If called from
menubar, select OLD from a menu and prompt for NEW.

If called from Lisp, prompt for NEW if only OLD was passed as an
argument.  If called with two strings, then no prompting is done.  You
must pass at least OLD when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consecutive words from the text of the buffer into the new bookmark
name."
  (interactive (list (bookmark-completing-read "Old bookmark name")))
  (bookmark-maybe-historicize-string old)
  (bookmark-maybe-load-default-file)

  (setq bookmark-current-point (point))
  (setq bookmark-yank-point (point))
  (setq bookmark-current-buffer (current-buffer))
  (let ((newname
         (or new   ; use second arg, if non-nil
             (read-from-minibuffer
              "New name: "
              nil
              (let ((now-map (copy-keymap minibuffer-local-map)))
                (define-key now-map  "\C-w" 'bookmark-yank-word)
                now-map)
              nil
              'bookmark-history))))
    (bookmark-set-name old newname)
    (setq bookmark-current-bookmark newname)
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (bookmark-save))))


;;;###autoload
(defun bookmark-insert (bookmark)
  "Insert the text of the file pointed to by bookmark BOOKMARK.
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this."
  (interactive (list (bookmark-completing-read "Insert bookmark contents")))
  (bookmark-maybe-historicize-string bookmark)
  (bookmark-maybe-load-default-file)
  (let ((orig-point (point))
	(str-to-insert
	 (save-excursion
	   (set-buffer (cadr (assq 'buffer (bookmark-jump-noselect bookmark))))
	   (buffer-string))))
    (insert str-to-insert)
    (push-mark)
    (goto-char orig-point)))


;;;###autoload
(defun bookmark-delete (bookmark &optional batch)
  "Delete BOOKMARK from the bookmark list.
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark \(that is, the
one most recently used in this file, if any\).
Optional second arg BATCH means don't update the bookmark list buffer,
probably because we were called from there."
  (interactive
   (list (bookmark-completing-read "Delete bookmark"
				   bookmark-current-bookmark)))
  (bookmark-maybe-historicize-string bookmark)
  (bookmark-maybe-load-default-file)
  (let ((will-go (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq will-go bookmark-alist))
    ;; Added by db, nil bookmark-current-bookmark if the last
    ;; occurrence has been deleted
    (or (bookmark-get-bookmark bookmark-current-bookmark)
        (setq bookmark-current-bookmark nil)))
  ;; Don't rebuild the list
  (if batch
      nil
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (bookmark-save))))


(defun bookmark-time-to-save-p (&optional last-time)
  ;; By Gregory M. Saunders <saunders@cis.ohio-state.edu>
  ;; finds out whether it's time to save bookmarks to a file, by
  ;; examining the value of variable bookmark-save-flag, and maybe
  ;; bookmark-alist-modification-count.  Returns t if they should be
  ;; saved, nil otherwise.  if last-time is non-nil, then this is
  ;; being called when emacs is killed.
  (cond (last-time
	 (and (> bookmark-alist-modification-count 0)
	      bookmark-save-flag))
	((numberp bookmark-save-flag)
	 (>= bookmark-alist-modification-count bookmark-save-flag))
	(t
	 nil)))


;;;###autoload
(defun bookmark-write ()
  "Write bookmarks to a file (reading the file name with the minibuffer).
Don't use this in Lisp programs; use `bookmark-save' instead."
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-save t))


;;;###autoload
(defun bookmark-save (&optional parg file)
  "Save currently defined bookmarks.
Saves by default in the file defined by the variable
`bookmark-default-file'.  With a prefix arg, save it in file FILE
\(second argument\).

If you are calling this from Lisp, the two arguments are PARG and
FILE, and if you just want it to write to the default file, then
pass no arguments.  Or pass in nil and FILE, and it will save in FILE
instead.  If you pass in one argument, and it is non-nil, then the
user will be interactively queried for a file to save in.

When you want to load in the bookmarks from a file, use
\`bookmark-load\', \\[bookmark-load].  That function will prompt you
for a file, defaulting to the file defined by variable
`bookmark-default-file'."
  (interactive "P")
  (bookmark-maybe-load-default-file)
  (cond
   ((and (null parg) (null file))
    ;;whether interactive or not, write to default file
    (bookmark-write-file bookmark-default-file))
   ((and (null parg) file)
    ;;whether interactive or not, write to given file
    (bookmark-write-file file))
   ((and parg (not file))
    ;;have been called interactively w/ prefix arg
    (let ((file (read-file-name "File to save bookmarks in: ")))
      (bookmark-write-file file)))
   (t ; someone called us with prefix-arg *and* a file, so just write to file
    (bookmark-write-file file)))
  ;; signal that we have synced the bookmark file by setting this to
  ;; 0.  If there was an error at any point before, it will not get
  ;; set, which is what we want.
  (setq bookmark-alist-modification-count 0))



(defun bookmark-write-file (file)
  (save-excursion
    (save-window-excursion
      (bookmark-maybe-message "Saving bookmarks to file %s..." file)
      (set-buffer (get-buffer-create " *Bookmarks*"))
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (let ((print-length nil)
	    (print-level nil))
	(bookmark-insert-file-format-version-stamp)
	(pp bookmark-alist (current-buffer))
	(let ((version-control
	       (cond
		((null bookmark-version-control) nil)
		((eq 'never bookmark-version-control) 'never)
		((eq 'nospecial bookmark-version-control) version-control)
		(t
		 t))))
          (condition-case nil
              (write-region (point-min) (point-max) file)
            (file-error (message "Can't write %s" file)))
	  (kill-buffer (current-buffer))
          (bookmark-maybe-message
           "Saving bookmarks to file %s...done" file))))))


(defun bookmark-import-new-list (new-list)
  ;; Walk over the new list, adding each individual bookmark
  ;; carefully.  "Carefully" means checking against the existing
  ;; bookmark-alist and renaming the new bookmarks with <N> extensions
  ;; as necessary.
  (let ((lst new-list)
        (names (bookmark-all-names)))
    (while lst
      (let* ((full-record (car lst)))
        (bookmark-maybe-rename full-record names)
        (setq bookmark-alist (nconc bookmark-alist (list full-record)))
        (setq names (cons (bookmark-name-from-full-record full-record) names))
        (setq lst (cdr lst))))))


(defun bookmark-maybe-rename (full-record names)
  ;; just a helper for bookmark-import-new-list; it is only for
  ;; readability that this is not inlined.
  ;;
  ;; Once this has found a free name, it sets full-record to that
  ;; name.
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
(defun bookmark-load (file &optional overwrite no-msg)
  "Load bookmarks from FILE (which must be in bookmark format).
Appends loaded bookmarks to the front of the list of bookmarks.  If
optional second argument OVERWRITE is non-nil, existing bookmarks are
destroyed.  Optional third arg NO-MSG means don't display any messages
while loading.

If you load a file that doesn't contain a proper bookmark alist, you
will corrupt Emacs's bookmark list.  Generally, you should only load
in files that were created with the bookmark functions in the first
place.  Your own personal bookmark file, `~/.emacs.bmk', is
maintained automatically by Emacs; you shouldn't need to load it
explicitly.

If you load a file containing bookmarks with the same names as
bookmarks already present in your Emacs, the new bookmarks will get
unique numeric suffixes \"<2>\", \"<3>\", ... following the same
method buffers use to resolve name collisions."
  (interactive
   (list (read-file-name
          (format "Load bookmarks from: (%s) "
                  bookmark-default-file)
          ;;Default might not be used often,
          ;;but there's no better default, and
          ;;I guess it's better than none at all.
          "~/" bookmark-default-file 'confirm)))
  (setq file (expand-file-name file))
  (if (file-readable-p file)
      (save-excursion
        (save-window-excursion
          (if (null no-msg)
              (bookmark-maybe-message "Loading bookmarks from %s..." file))
          (set-buffer (let ((enable-local-variables nil))
                        (find-file-noselect file)))
          (goto-char (point-min))
          (bookmark-maybe-upgrade-file-format)
          (let ((blist (bookmark-alist-from-buffer)))
            (if (listp blist)
                (progn
                  (if overwrite
                      (progn
                        (setq bookmark-alist blist)
                        (setq bookmark-alist-modification-count 0))
                    ;; else
                    (bookmark-import-new-list blist)
                    (setq bookmark-alist-modification-count
                          (1+ bookmark-alist-modification-count)))
                  (if (string-equal
                       (expand-file-name bookmark-default-file)
                       file)
                      (setq bookmarks-already-loaded t))
                  (bookmark-bmenu-surreptitiously-rebuild-list))
              (error "Invalid bookmark list in %s" file)))
          (kill-buffer (current-buffer)))
	(if (null no-msg)
            (bookmark-maybe-message "Loading bookmarks from %s...done" file)))
    (error "Cannot read bookmark file %s" file)))



;;; Code supporting the dired-like bookmark menu.  Prefix is
;;; "bookmark-bmenu" for "buffer-menu":


(defvar bookmark-bmenu-bookmark-column nil)


(defvar bookmark-bmenu-hidden-bookmarks ())


(defvar bookmark-bmenu-mode-map nil)


(if bookmark-bmenu-mode-map
    nil
  (setq bookmark-bmenu-mode-map (make-keymap))
  (suppress-keymap bookmark-bmenu-mode-map t)
  (define-key bookmark-bmenu-mode-map "q" 'quit-window)
  (define-key bookmark-bmenu-mode-map "v" 'bookmark-bmenu-select)
  (define-key bookmark-bmenu-mode-map "w" 'bookmark-bmenu-locate)
  (define-key bookmark-bmenu-mode-map "2" 'bookmark-bmenu-2-window)
  (define-key bookmark-bmenu-mode-map "1" 'bookmark-bmenu-1-window)
  (define-key bookmark-bmenu-mode-map "j" 'bookmark-bmenu-this-window)
  (define-key bookmark-bmenu-mode-map "\C-c\C-c" 'bookmark-bmenu-this-window)
  (define-key bookmark-bmenu-mode-map "f" 'bookmark-bmenu-this-window)
  (define-key bookmark-bmenu-mode-map "\C-m" 'bookmark-bmenu-this-window)
  (define-key bookmark-bmenu-mode-map "o" 'bookmark-bmenu-other-window)
  (define-key bookmark-bmenu-mode-map "\C-o"
    'bookmark-bmenu-switch-other-window)
  (define-key bookmark-bmenu-mode-map "s" 'bookmark-bmenu-save)
  (define-key bookmark-bmenu-mode-map "k" 'bookmark-bmenu-delete)
  (define-key bookmark-bmenu-mode-map "\C-d" 'bookmark-bmenu-delete-backwards)
  (define-key bookmark-bmenu-mode-map "x" 'bookmark-bmenu-execute-deletions)
  (define-key bookmark-bmenu-mode-map "d" 'bookmark-bmenu-delete)
  (define-key bookmark-bmenu-mode-map " " 'next-line)
  (define-key bookmark-bmenu-mode-map "n" 'next-line)
  (define-key bookmark-bmenu-mode-map "p" 'previous-line)
  (define-key bookmark-bmenu-mode-map "\177" 'bookmark-bmenu-backup-unmark)
  (define-key bookmark-bmenu-mode-map "?" 'describe-mode)
  (define-key bookmark-bmenu-mode-map "u" 'bookmark-bmenu-unmark)
  (define-key bookmark-bmenu-mode-map "m" 'bookmark-bmenu-mark)
  (define-key bookmark-bmenu-mode-map "l" 'bookmark-bmenu-load)
  (define-key bookmark-bmenu-mode-map "r" 'bookmark-bmenu-rename)
  (define-key bookmark-bmenu-mode-map "R" 'bookmark-bmenu-relocate)
  (define-key bookmark-bmenu-mode-map "t" 'bookmark-bmenu-toggle-filenames)
  (define-key bookmark-bmenu-mode-map "a" 'bookmark-bmenu-show-annotation)
  (define-key bookmark-bmenu-mode-map "A" 'bookmark-bmenu-show-all-annotations)
  (define-key bookmark-bmenu-mode-map "e" 'bookmark-bmenu-edit-annotation)
  (define-key bookmark-bmenu-mode-map [mouse-2]
    'bookmark-bmenu-other-window-with-mouse))



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
  (if (get-buffer "*Bookmark List*")
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
  (if (interactive-p)
      (switch-to-buffer (get-buffer-create "*Bookmark List*"))
    (set-buffer (get-buffer-create "*Bookmark List*")))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "% Bookmark\n- --------\n")
    (add-text-properties (point-min) (point)
			 '(font-lock-face bookmark-menu-heading))
    (mapc
     (lambda (full-record)
       ;; if a bookmark has an annotation, prepend a "*"
       ;; in the list of bookmarks.
       (let ((annotation (bookmark-get-annotation
                          (bookmark-name-from-full-record full-record))))
         (if (and annotation (not (string-equal annotation "")))
             (insert " *")
           (insert "  "))
	 (let ((start (point)))
	   (insert (bookmark-name-from-full-record full-record))
	   (if (and (display-color-p) (display-mouse-p))
	       (add-text-properties
		start
		(save-excursion (re-search-backward
				 "[^ \t]")
				(1+ (point)))
		'(mouse-face highlight
		  follow-link t
		  help-echo "mouse-2: go to this bookmark in other window")))
	   (insert "\n")
	   )))
     (bookmark-maybe-sort-alist)))
  (goto-char (point-min))
  (forward-line 2)
  (bookmark-bmenu-mode)
  (if bookmark-bmenu-toggle-filenames
      (bookmark-bmenu-toggle-filenames t)))

;;;###autoload
(defalias 'list-bookmarks 'bookmark-bmenu-list)
;;;###autoload
(defalias 'edit-bookmarks 'bookmark-bmenu-list)



(defun bookmark-bmenu-mode ()
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
\\[bookmark-bmenu-switch-other-window] -- switch the other window to this bookmark.
\\[bookmark-bmenu-rename] -- rename this bookmark \(prompts for new name\).
\\[bookmark-bmenu-relocate] -- relocate this bookmark's file \(prompts for new file\).
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
\\[bookmark-bmenu-edit-annotation] -- edit the annotation for the current bookmark."
  (kill-all-local-variables)
  (use-local-map bookmark-bmenu-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'bookmark-bmenu-mode)
  (setq mode-name "Bookmark Menu")
  (run-mode-hooks 'bookmark-bmenu-mode-hook))


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
    (setq bookmark-bmenu-toggle-filenames t))))


(defun bookmark-bmenu-show-filenames (&optional force)
  (if (and (not force) bookmark-bmenu-toggle-filenames)
      nil ;already shown, so do nothing
    (save-excursion
      (save-window-excursion
        (goto-char (point-min))
        (forward-line 2)
        (setq bookmark-bmenu-hidden-bookmarks ())
        (let ((inhibit-read-only t))
          (while (< (point) (point-max))
            (let ((bmrk (bookmark-bmenu-bookmark)))
              (setq bookmark-bmenu-hidden-bookmarks
                    (cons bmrk bookmark-bmenu-hidden-bookmarks))
	      (let ((start (save-excursion (end-of-line) (point))))
		(move-to-column bookmark-bmenu-file-column t)
		;; Strip off `mouse-face' from the white spaces region.
		(if (and (display-color-p) (display-mouse-p))
		    (remove-text-properties start (point)
					    '(mouse-face nil help-echo nil))))
	      (delete-region (point) (progn (end-of-line) (point)))
              (insert "  ")
              ;; Pass the NO-HISTORY arg:
              (bookmark-insert-location bmrk t)
              (forward-line 1))))))))


(defun bookmark-bmenu-hide-filenames (&optional force)
  (if (and (not force) bookmark-bmenu-toggle-filenames)
      ;; nothing to hide if above is nil
      (save-excursion
        (save-window-excursion
          (goto-char (point-min))
          (forward-line 2)
          (setq bookmark-bmenu-hidden-bookmarks
                (nreverse bookmark-bmenu-hidden-bookmarks))
          (save-excursion
            (goto-char (point-min))
            (search-forward "Bookmark")
            (backward-word 1)
            (setq bookmark-bmenu-bookmark-column (current-column)))
          (save-excursion
            (let ((inhibit-read-only t))
              (while bookmark-bmenu-hidden-bookmarks
                (move-to-column bookmark-bmenu-bookmark-column t)
                (bookmark-kill-line)
		(let ((start (point)))
		  (insert (car bookmark-bmenu-hidden-bookmarks))
		  (if (and (display-color-p) (display-mouse-p))
		      (add-text-properties
		       start
		       (save-excursion (re-search-backward
					"[^ \t]")
				       (1+ (point)))
		       '(mouse-face highlight
			 follow-link t
			 help-echo
			 "mouse-2: go to this bookmark in other window"))))
                (setq bookmark-bmenu-hidden-bookmarks
                      (cdr bookmark-bmenu-hidden-bookmarks))
                (forward-line 1))))))))


(defun bookmark-bmenu-check-position ()
  ;; Returns non-nil if on a line with a bookmark.
  ;; (The actual value returned is bookmark-alist).
  ;; Else reposition and try again, else return nil.
  (cond ((< (count-lines (point-min) (point)) 2)
         (goto-char (point-min))
         (forward-line 2)
         bookmark-alist)
        ((and (bolp) (eobp))
         (beginning-of-line 0)
         bookmark-alist)
        (t
         bookmark-alist)))


(defun bookmark-bmenu-bookmark ()
  ;; return a string which is bookmark of this line.
  (if (bookmark-bmenu-check-position)
      (save-excursion
        (save-window-excursion
          (goto-char (point-min))
          (search-forward "Bookmark")
          (backward-word 1)
          (setq bookmark-bmenu-bookmark-column (current-column)))))
  (if bookmark-bmenu-toggle-filenames
      (bookmark-bmenu-hide-filenames))
  (save-excursion
    (save-window-excursion
      (beginning-of-line)
      (forward-char bookmark-bmenu-bookmark-column)
      (prog1
          (buffer-substring-no-properties (point)
                            (progn
                              (end-of-line)
                              (point)))
        ;; well, this is certainly crystal-clear:
        (if bookmark-bmenu-toggle-filenames
            (bookmark-bmenu-toggle-filenames t))))))


(defun bookmark-show-annotation (bookmark)
  "Display the annotation for bookmark named BOOKMARK in a buffer,
if an annotation exists."
  (let ((annotation (bookmark-get-annotation bookmark)))
    (if (and annotation (not (string-equal annotation "")))
        (save-excursion
          (let ((old-buf (current-buffer)))
            (pop-to-buffer (get-buffer-create "*Bookmark Annotation*") t)
            (delete-region (point-min) (point-max))
            ;; (insert (concat "Annotation for bookmark '" bookmark "':\n\n"))
            (insert annotation)
            (goto-char (point-min))
            (pop-to-buffer old-buf))))))


(defun bookmark-show-all-annotations ()
  "Display the annotations for all bookmarks in a buffer."
  (let ((old-buf (current-buffer)))
    (pop-to-buffer (get-buffer-create "*Bookmark Annotation*") t)
    (delete-region (point-min) (point-max))
    (mapc
     (lambda (full-record)
       (let* ((name (bookmark-name-from-full-record full-record))
              (ann  (bookmark-get-annotation name)))
         (insert (concat name ":\n"))
         (if (and ann (not (string-equal ann "")))
             ;; insert the annotation, indented by 4 spaces.
             (progn
               (save-excursion (insert ann) (unless (bolp)
                                              (insert "\n")))
               (while (< (point) (point-max))
                 (beginning-of-line) ; paranoia
                 (insert "    ")
                 (forward-line)
                 (end-of-line))))))
     bookmark-alist)
    (goto-char (point-min))
    (pop-to-buffer old-buf)))


(defun bookmark-bmenu-mark ()
  "Mark bookmark on this line to be displayed by \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-select]."
  (interactive)
  (beginning-of-line)
  (if (bookmark-bmenu-check-position)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ?>)
        (forward-line 1)
        (bookmark-bmenu-check-position))))


(defun bookmark-bmenu-select ()
  "Select this line's bookmark; also display bookmarks marked with `>'.
You can mark bookmarks with the \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-mark] command."
  (interactive)
  (if (bookmark-bmenu-check-position)
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
          (other-window 1)))))


(defun bookmark-bmenu-save (parg)
  "Save the current list into a bookmark file.
With a prefix arg, prompts for a file to save them in."
  (interactive "P")
  (save-excursion
    (save-window-excursion
      (bookmark-save parg))))


(defun bookmark-bmenu-load ()
  "Load the bookmark file and rebuild the bookmark menu-buffer."
  (interactive)
  (if (bookmark-bmenu-check-position)
      (save-excursion
        (save-window-excursion
          ;; This will call `bookmark-bmenu-list'
          (call-interactively 'bookmark-load)))))


(defun bookmark-bmenu-1-window ()
  "Select this line's bookmark, alone, in full frame."
  (interactive)
  (if (bookmark-bmenu-check-position)
      (progn
        (bookmark-jump (bookmark-bmenu-bookmark))
        (bury-buffer (other-buffer))
        (delete-other-windows))))


(defun bookmark-bmenu-2-window ()
  "Select this line's bookmark, with previous buffer in second window."
  (interactive)
  (if (bookmark-bmenu-check-position)
      (let ((bmrk (bookmark-bmenu-bookmark))
            (menu (current-buffer))
            (pop-up-windows t))
        (delete-other-windows)
        (switch-to-buffer (other-buffer))
	(let* ((alist (bookmark-jump-noselect bmrk))
               (buff (cadr (assq 'buffer alist)))
               (pos  (cadr (assq 'position alist))))
          (pop-to-buffer buff)
          (goto-char pos))
        (bury-buffer menu))))


(defun bookmark-bmenu-this-window ()
  "Select this line's bookmark in this window."
  (interactive)
  (if (bookmark-bmenu-check-position)
      (bookmark-jump (bookmark-bmenu-bookmark))))


(defun bookmark-bmenu-other-window ()
  "Select this line's bookmark in other window, leaving bookmark menu visible."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
    (if (bookmark-bmenu-check-position)
	(let* ((alist (bookmark-jump-noselect bookmark))
               (buff (cadr (assq 'buffer alist)))
               (pos  (cadr (assq 'position alist))))
	  (switch-to-buffer-other-window buff)
          (goto-char pos)
          (set-window-point (get-buffer-window buff) pos)
	  (bookmark-show-annotation bookmark)))))


(defun bookmark-bmenu-switch-other-window ()
  "Make the other window select this line's bookmark.
The current window remains selected."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark))
        (pop-up-windows t)
        same-window-buffer-names
        same-window-regexps)
    (if (bookmark-bmenu-check-position)
	(let* ((alist (bookmark-jump-noselect bookmark))
               (buff (cadr (assq 'buffer alist)))
               (pos  (cadr (assq 'position alist))))
	  (display-buffer buff)
          (let ((o-buffer (current-buffer)))
            ;; save-excursion won't do
            (set-buffer buff)
            (goto-char pos)
            (set-window-point (get-buffer-window buff) pos)
            (set-buffer o-buffer))
	  (bookmark-show-annotation bookmark)))))

(defun bookmark-bmenu-other-window-with-mouse (event)
  "Select bookmark at the mouse pointer in other window, leaving bookmark menu visible."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-end event))))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (bookmark-bmenu-other-window))))


(defun bookmark-bmenu-show-annotation ()
  "Show the annotation for the current bookmark in another window."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
    (if (bookmark-bmenu-check-position)
	(bookmark-show-annotation bookmark))))


(defun bookmark-bmenu-show-all-annotations ()
  "Show the annotation for all bookmarks in another window."
  (interactive)
  (bookmark-show-all-annotations))


(defun bookmark-bmenu-edit-annotation ()
  "Edit the annotation for the current bookmark in another window."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
    (if (bookmark-bmenu-check-position)
	(bookmark-edit-annotation bookmark))))


(defun bookmark-bmenu-unmark (&optional backup)
  "Cancel all requested operations on bookmark on this line and move down.
Optional BACKUP means move up."
  (interactive "P")
  (beginning-of-line)
  (if (bookmark-bmenu-check-position)
      (progn
        (let ((inhibit-read-only t))
          (delete-char 1)
          ;; any flags to reset according to circumstances?  How about a
          ;; flag indicating whether this bookmark is being visited?
          ;; well, we don't have this now, so maybe later.
          (insert " "))
        (forward-line (if backup -1 1))
        (bookmark-bmenu-check-position))))


(defun bookmark-bmenu-backup-unmark ()
  "Move up and cancel all requested operations on bookmark on line above."
  (interactive)
  (forward-line -1)
  (if (bookmark-bmenu-check-position)
      (progn
        (bookmark-bmenu-unmark)
        (forward-line -1)
        (bookmark-bmenu-check-position))))


(defun bookmark-bmenu-delete ()
  "Mark bookmark on this line to be deleted.
To carry out the deletions that you've marked, use \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-execute-deletions]."
  (interactive)
  (beginning-of-line)
  (if (bookmark-bmenu-check-position)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ?D)
        (forward-line 1)
        (bookmark-bmenu-check-position))))


(defun bookmark-bmenu-delete-backwards ()
  "Mark bookmark on this line to be deleted, then move up one line.
To carry out the deletions that you've marked, use \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-execute-deletions]."
  (interactive)
  (bookmark-bmenu-delete)
  (forward-line -2)
  (if (bookmark-bmenu-check-position)
      (forward-line 1))
  (bookmark-bmenu-check-position))


(defun bookmark-bmenu-execute-deletions ()
  "Delete bookmarks marked with \\<Buffer-menu-mode-map>\\[Buffer-menu-delete] commands."
  (interactive)
  (message "Deleting bookmarks...")
  (let ((hide-em bookmark-bmenu-toggle-filenames)
        (o-point  (point))
        (o-str    (save-excursion
                    (beginning-of-line)
                    (if (looking-at "^D")
                        nil
                      (buffer-substring
                       (point)
                       (progn (end-of-line) (point))))))
        (o-col     (current-column)))
    (if hide-em (bookmark-bmenu-hide-filenames))
    (setq bookmark-bmenu-toggle-filenames nil)
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^D" (point-max) t)
      (bookmark-delete (bookmark-bmenu-bookmark) t)) ; pass BATCH arg
    (bookmark-bmenu-list)
    (setq bookmark-bmenu-toggle-filenames hide-em)
    (if bookmark-bmenu-toggle-filenames
        (bookmark-bmenu-toggle-filenames t))
    (if o-str
        (progn
          (goto-char (point-min))
          (search-forward o-str)
          (beginning-of-line)
          (forward-char o-col))
      (goto-char o-point))
    (beginning-of-line)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (bookmark-save))
    (message "Deleting bookmarks...done")
    ))


(defun bookmark-bmenu-rename ()
  "Rename bookmark on current line.  Prompts for a new name."
  (interactive)
  (if (bookmark-bmenu-check-position)
      (let ((bmrk (bookmark-bmenu-bookmark))
            (thispoint (point)))
        (bookmark-rename bmrk)
        (bookmark-bmenu-list)
        (goto-char thispoint))))


(defun bookmark-bmenu-locate ()
  "Display location of this bookmark.  Displays in the minibuffer."
  (interactive)
  (if (bookmark-bmenu-check-position)
      (let ((bmrk (bookmark-bmenu-bookmark)))
        (message "%s" (bookmark-location bmrk)))))

(defun bookmark-bmenu-relocate ()
  "Change the file path of the bookmark on the current line,
  prompting with completion for the new path."
  (interactive)
  (if (bookmark-bmenu-check-position)
      (let ((bmrk (bookmark-bmenu-bookmark))
            (thispoint (point)))
        (bookmark-relocate bmrk)
        (goto-char thispoint))))


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
    (define-key map [load]	'("Load a Bookmark File..." . bookmark-load))
    (define-key map [write]	'("Save Bookmarks As..." . bookmark-write))
    (define-key map [save]	'("Save Bookmarks" . bookmark-save))
    (define-key map [edit]	'("Edit Bookmark List" . bookmark-bmenu-list))
    (define-key map [delete]	'("Delete Bookmark..." . bookmark-delete))
    (define-key map [rename]	'("Rename Bookmark..." . bookmark-rename))
    (define-key map [locate]	'("Insert Location..." . bookmark-locate))
    (define-key map [insert]	'("Insert Contents..." . bookmark-insert))
    (define-key map [set]	'("Set Bookmark..." . bookmark-set))
    (define-key map [jump]	'("Jump to Bookmark..." . bookmark-jump))
    map))

;;;###autoload
(defalias 'menu-bar-bookmark-map menu-bar-bookmark-map)

;; make bookmarks appear toward the right side of the menu.
(if (boundp 'menu-bar-final-items)
    (if menu-bar-final-items
        (setq menu-bar-final-items
              (cons 'bookmark menu-bar-final-items)))
  (setq menu-bar-final-items '(bookmark)))

;;;; end bookmark menu stuff ;;;;


;;; Load Hook
(defvar bookmark-load-hook nil
  "Hook run at the end of loading bookmark.")

;;; Exit Hook, called from kill-emacs-hook
(defvar bookmark-exit-hook nil
  "Hook run when Emacs exits.")

(define-obsolete-variable-alias 'bookmark-exit-hooks 'bookmark-exit-hook "22.1")

(defun bookmark-exit-hook-internal ()
  "Save bookmark state, if necessary, at Emacs exit time.
This also runs `bookmark-exit-hook'."
  (run-hooks 'bookmark-exit-hook)
  (and bookmark-alist
       (bookmark-time-to-save-p t)
       (bookmark-save)))

(add-hook 'kill-emacs-hook 'bookmark-exit-hook-internal)


(run-hooks 'bookmark-load-hook)

(provide 'bookmark)

;; arch-tag: 139f519a-dd0c-4b8d-8b5d-f9fcf53ca8f6
;;; bookmark.el ends here
