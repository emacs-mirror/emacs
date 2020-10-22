;;; org-attach.el --- Manage file attachments to Org outlines -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2020 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@newartisans.com>
;; Keywords: org data attachment

;; This file is part of GNU Emacs.
;;
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

;; See the Org manual for information on how to use it.
;;
;; Attachments are managed either by using a custom property DIR or by
;; using property ID from org-id.  When DIR is defined, a location in
;; the filesystem is directly attached to the outline node.  When
;; org-id is used, attachments are stored in a folder named after the
;; ID, in a location defined by `org-attach-id-dir'.  DIR has
;; precedence over ID when both parameters are defined for the current
;; outline node (also when inherited parameters are taken into
;; account).

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ol)
(require 'org-id)

(declare-function dired-dwim-target-directory "dired-aux")

(defgroup org-attach nil
  "Options concerning attachments in Org mode."
  :tag "Org Attach"
  :group 'org)

(defcustom org-attach-id-dir "data/"
  "The directory where attachments are stored.
If this is a relative path, it will be interpreted relative to the directory
where the Org file lives."
  :group 'org-attach
  :type 'directory
  :safe #'stringp)

(defcustom org-attach-dir-relative nil
  "Non-nil means directories in DIR property are added as relative links.
Defaults to absolute location."
  :group 'org-attach
  :type 'boolean
  :package-version '(Org . "9.3")
  :safe #'booleanp)

(defcustom org-attach-auto-tag "ATTACH"
  "Tag that will be triggered automatically when an entry has an attachment."
  :group 'org-attach
  :type '(choice
	  (const :tag "None" nil)
	  (string :tag "Tag")))

(defcustom org-attach-preferred-new-method 'id
  "Preferred way to attach to nodes without existing ID and DIR property.
This choice is used when adding attachments to nodes without ID
and DIR properties.

Allowed values are:

id         Create and use an ID parameter
dir        Create and use a DIR parameter
ask        Ask the user for input of which method to choose
nil        Prefer to not create a new parameter

           nil means that ID or DIR has to be created explicitly
           before attaching files."
  :group 'org-attach
  :package-version '(org . "9.3")
  :type '(choice
	  (const :tag "ID parameter" id)
	  (const :tag "DIR parameter" dir)
	  (const :tag "Ask user" ask)
	  (const :tag "Don't create" nil)))

(defcustom org-attach-method 'cp
  "The preferred method to attach a file.
Allowed values are:

mv    rename the file to move it into the attachment directory
cp    copy the file
ln    create a hard link.  Note that this is not supported
      on all systems, and then the result is not defined.
lns   create a symbol link.  Note that this is not supported
      on all systems, and then the result is not defined."
  :group 'org-attach
  :type '(choice
	  (const :tag "Copy" cp)
	  (const :tag "Move/Rename" mv)
	  (const :tag "Hard Link" ln)
	  (const :tag "Symbol Link" lns)))

(defcustom org-attach-expert nil
  "Non-nil means do not show the splash buffer with the attach dispatcher."
  :group 'org-attach
  :type 'boolean)

(defcustom org-attach-use-inheritance 'selective
  "Attachment inheritance for the outline.

Enabling inheritance for org-attach implies two things.  First,
that attachment links will look through all parent headings until
it finds the linked attachment.  Second, that running org-attach
inside a node without attachments will make org-attach operate on
the first parent heading it finds with an attachment.

Selective means to respect the inheritance setting in
`org-use-property-inheritance'."
  :group 'org-attach
  :type '(choice
	  (const :tag "Don't use inheritance" nil)
	  (const :tag "Inherit parent node attachments" t)
	  (const :tag "Respect org-use-property-inheritance" selective))
  :type 'boolean)

(defcustom org-attach-store-link-p nil
  "Non-nil means store a link to a file when attaching it."
  :group 'org-attach
  :version "24.1"
  :type '(choice
	  (const :tag "Don't store link" nil)
	  (const :tag "Link to origin location" t)
	  (const :tag "Link to the attach-dir location" attached)))

(defcustom org-attach-archive-delete nil
  "Non-nil means attachments are deleted upon archiving a subtree.
When set to `query', ask the user instead."
  :group 'org-attach
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "Never delete attachments" nil)
	  (const :tag "Always delete attachments" t)
	  (const :tag "Query the user" query)))

(defun org-attach-id-uuid-folder-format (id)
  "Translate an UUID ID into a folder-path.
Default format for how Org translates ID properties to a path for
attachments.  Useful if ID is generated with UUID."
  (format "%s/%s"
	  (substring id 0 2)
	  (substring id 2)))

(defun org-attach-id-ts-folder-format (id)
  "Translate an ID based on a timestamp to a folder-path.
Useful way of translation if ID is generated based on ISO8601
timestamp.  Splits the attachment folder hierarchy into
year-month, the rest."
  (format "%s/%s"
	  (substring id 0 6)
	  (substring id 6)))

(defcustom org-attach-id-to-path-function-list '(org-attach-id-uuid-folder-format
						 org-attach-id-ts-folder-format)
  "List of functions parsing an ID string into a folder-path.
The first function in this list defines the preferred function
which will be used when creating new attachment folders.  All
functions of this list will be tried when looking for existing
attachment folders based on ID."
  :group 'org-attach
  :package-version '(Org . "9.3")
  :type '(repeat (function :tag "Function with ID as input")))

(defvar org-attach-after-change-hook nil
  "Hook to be called when files have been added or removed to the attachment folder.")

(defvar org-attach-open-hook nil
  "Hook that is invoked by `org-attach-open'.

Created mostly to be compatible with org-attach-git after removing
git-functionality from this file.")

(defcustom org-attach-commands
  '(((?a ?\C-a) org-attach-attach
     "Select a file and attach it to the task, using `org-attach-method'.")
    ((?c ?\C-c) org-attach-attach-cp
     "Attach a file using copy method.")
    ((?m ?\C-m) org-attach-attach-mv
     "Attach a file using move method.")
    ((?l ?\C-l) org-attach-attach-ln
     "Attach a file using link method.")
    ((?y ?\C-y) org-attach-attach-lns
     "Attach a file using symbolic-link method.")
    ((?u ?\C-u) org-attach-url
     "Attach a file from URL (downloading it).")
    ((?b) org-attach-buffer
     "Select a buffer and attach its contents to the task.")
    ((?n ?\C-n) org-attach-new
     "Create a new attachment, as an Emacs buffer.")
    ((?z ?\C-z) org-attach-sync
     "Synchronize the current node with its attachment\n directory, in case \
you added attachments yourself.\n")
    ((?o ?\C-o) org-attach-open
     "Open current node's attachments.")
    ((?O) org-attach-open-in-emacs
     "Like \"o\", but force opening in Emacs.")
    ((?f ?\C-f) org-attach-reveal
     "Open current node's attachment directory.  Create if missing.")
    ((?F) org-attach-reveal-in-emacs
     "Like \"f\", but force using Dired in Emacs.\n")
    ((?d ?\C-d) org-attach-delete-one
     "Delete one attachment, you will be prompted for a file name.")
    ((?D) org-attach-delete-all
     "Delete all of a node's attachments.  A safer way is\n to open the \
directory in dired and delete from there.\n")
    ((?s ?\C-s) org-attach-set-directory
     "Set a specific attachment directory for this entry. Sets DIR property.")
    ((?S ?\C-S) org-attach-unset-directory
     "Unset the attachment directory for this entry.  Removes DIR property.")
    ((?q) (lambda () (interactive) (message "Abort")) "Abort."))
  "The list of commands for the attachment dispatcher.
Each entry in this list is a list of three elements:
- A list of keys (characters) to select the command (the fist
  character in the list is shown in the attachment dispatcher's
  splash buffer and minibuffer prompt).
- A command that is called interactively when one of these keys
  is pressed.
- A docstring for this command in the attachment dispatcher's
  splash buffer."
  :group 'org-attach
  :package-version '(Org . "9.3")
  :type '(repeat (list (repeat :tag "Keys" character)
		       (function :tag "Command")
		       (string :tag "Docstring"))))

;;;###autoload
(defun org-attach ()
  "The dispatcher for attachment commands.
Shows a list of commands and prompts for another key to execute a command."
  (interactive)
  (let ((dir (org-attach-dir nil 'no-fs-check))
	c marker)
    (when (eq major-mode 'org-agenda-mode)
      (setq marker (or (get-text-property (point) 'org-hd-marker)
		       (get-text-property (point) 'org-marker)))
      (unless marker
	(error "No item in current line")))
    (save-excursion
      (when marker
	(set-buffer (marker-buffer marker))
	(goto-char marker))
      (org-back-to-heading t)
      (save-excursion
	(save-window-excursion
	  (unless org-attach-expert
	    (with-output-to-temp-buffer "*Org Attach*"
              (princ
               (concat "Attachment folder:\n"
		       (or dir
			   "Can't find an existing attachment-folder")
		       (unless (and dir (file-directory-p dir))
			 "\n(Not yet created)")
		       "\n\n"
	               (format "Select an Attachment Command:\n\n%s"
		               (mapconcat
		                (lambda (entry)
		                  (pcase entry
		                    (`((,key . ,_) ,_ ,docstring)
		                     (format "%c       %s"
			                     key
			                     (replace-regexp-in-string "\n\\([\t ]*\\)"
							               "        "
							               docstring
							               nil nil 1)))
		                    (_
		                     (user-error
			              "Invalid `org-attach-commands' item: %S"
			              entry))))
		                org-attach-commands
		                "\n"))))))
	  (org-fit-window-to-buffer (get-buffer-window "*Org Attach*"))
	  (message "Select command: [%s]"
		   (concat (mapcar #'caar org-attach-commands)))
	  (setq c (read-char-exclusive))
	  (and (get-buffer "*Org Attach*") (kill-buffer "*Org Attach*"))))
      (let ((command (cl-some (lambda (entry)
				(and (memq c (nth 0 entry)) (nth 1 entry)))
			      org-attach-commands)))
	(if (commandp command t)
	    (call-interactively command)
	  (error "No such attachment command: %c" c))))))

(defun org-attach-dir (&optional create-if-not-exists-p no-fs-check)
  "Return the directory associated with the current outline node.
First check for DIR property, then ID property.
`org-attach-use-inheritance' determines whether inherited
properties also will be considered.

If an ID property is found the default mechanism using that ID
will be invoked to access the directory for the current entry.
Note that this method returns the directory as declared by ID or
DIR even if the directory doesn't exist in the filesystem.

If CREATE-IF-NOT-EXIST-P is non-nil, `org-attach-dir-get-create'
is run.  If NO-FS-CHECK is non-nil, the function returns the path
to the attachment even if it has not yet been initialized in the
filesystem.

If no attachment directory can be derived, return nil."
  (let (attach-dir id)
    (cond
     (create-if-not-exists-p
      (setq attach-dir (org-attach-dir-get-create)))
     ((setq attach-dir (org-entry-get nil "DIR" org-attach-use-inheritance))
      (org-attach-check-absolute-path attach-dir))
     ;; Deprecated and removed from documentation, but still
     ;; works. FIXME: Remove after major nr change.
     ((setq attach-dir (org-entry-get nil "ATTACH_DIR" org-attach-use-inheritance))
      (org-attach-check-absolute-path attach-dir))
     ((setq id (org-entry-get nil "ID" org-attach-use-inheritance))
      (org-attach-check-absolute-path nil)
      (setq attach-dir (org-attach-dir-from-id id 'try-all))))
    (if no-fs-check
	attach-dir
      (when (and attach-dir (file-directory-p attach-dir))
	attach-dir))))

(defun org-attach-dir-get-create ()
  "Return existing or new directory associated with the current outline node.
`org-attach-preferred-new-method' decides how to attach new
directory if neither ID nor DIR property exist.

If the attachment by some reason cannot be created an error will be raised."
  (interactive)
  (let ((attach-dir (org-attach-dir nil 'no-fs-check)))
    (unless attach-dir
      (let (answer)
	(when (eq org-attach-preferred-new-method 'ask)
	  (message "Create new ID [1] property or DIR [2] property for attachments?")
	  (setq answer (read-char-exclusive)))
	(cond
	 ((or (eq org-attach-preferred-new-method 'id) (eq answer ?1))
	  (setq attach-dir (org-attach-dir-from-id (org-id-get nil t))))
	 ((or (eq org-attach-preferred-new-method 'dir) (eq answer ?2))
	  (setq attach-dir (org-attach-set-directory)))
	 ((eq org-attach-preferred-new-method 'nil)
	  (error "No existing directory. DIR or ID property has to be explicitly created")))))
    (unless attach-dir
      (error "No attachment directory is associated with the current node"))
    (unless (file-directory-p attach-dir)
      (make-directory attach-dir t))
    attach-dir))

(defun org-attach-dir-from-id (id  &optional try-all)
  "Returns a folder path based on `org-attach-id-dir' and ID.
If TRY-ALL is non-nil, try all id-to-path functions in
`org-attach-id-to-path-function-list' and return the first path
that exist in the filesystem, or the first one if none exist.
Otherwise only use the first function in that list."
  (let ((attach-dir-preferred (expand-file-name
			       (funcall (car org-attach-id-to-path-function-list) id)
			       (expand-file-name org-attach-id-dir))))
    (if try-all
	(let ((attach-dir attach-dir-preferred)
	      (fun-list (cdr org-attach-id-to-path-function-list)))
	  (while (and fun-list (not (file-directory-p attach-dir)))
	    (setq attach-dir (expand-file-name
			      (funcall (car fun-list) id)
			      (expand-file-name org-attach-id-dir)))
	    (setq fun-list (cdr fun-list)))
	  (if (file-directory-p attach-dir)
	      attach-dir
	    attach-dir-preferred))
      attach-dir-preferred)))

(defun org-attach-check-absolute-path (dir)
  "Check if we have enough information to root the attachment directory.
When DIR is given, check also if it is already absolute.  Otherwise,
assume that it will be relative, and check if `org-attach-id-dir' is
absolute, or if at least the current buffer has a file name.
Throw an error if we cannot root the directory."
  (or (and dir (file-name-absolute-p dir))
      (file-name-absolute-p org-attach-id-dir)
      (buffer-file-name (buffer-base-buffer))
      (error "Need absolute `org-attach-id-dir' to attach in buffers without filename")))

(defun org-attach-set-directory ()
  "Set the DIR node property and ask to move files there.
The property defines the directory that is used for attachments
of the entry.  Creates relative links if `org-attach-dir-relative'
is non-nil.

Return the directory."
  (interactive)
  (let ((old (org-attach-dir))
	(new
	 (let* ((attach-dir (read-directory-name
			     "Attachment directory: "
			     (org-entry-get nil "DIR")))
		(current-dir (file-name-directory (or default-directory
						      buffer-file-name)))
		(attach-dir-relative (file-relative-name attach-dir current-dir)))
	   (org-entry-put nil "DIR" (if org-attach-dir-relative
					attach-dir-relative
				      attach-dir))
           attach-dir)))
    (unless (or (string= old new)
                (not old))
      (when (yes-or-no-p "Copy over attachments from old directory? ")
        (copy-directory old new t t t))
      (when (yes-or-no-p (concat "Delete " old))
        (delete-directory old t)))
    new))

(defun org-attach-unset-directory ()
  "Removes DIR node property.
If attachment folder is changed due to removal of DIR-property
ask to move attachments to new location and ask to delete old
attachment-folder.

Change of attachment-folder due to unset might be if an ID
property is set on the node, or if a separate inherited
DIR-property exists (that is different from the unset one)."
  (interactive)
  (let ((old (org-attach-dir))
	(new
         (progn
	   (org-entry-delete nil "DIR")
	   ;; ATTACH-DIR is deprecated and removed from documentation,
	   ;; but still works. Remove code for it after major nr change.
	   (org-entry-delete nil "ATTACH_DIR")
	   (org-attach-dir))))
    (unless (or (string= old new)
                (not old))
      (when (and new (yes-or-no-p "Copy over attachments from old directory? "))
        (copy-directory old new t nil t))
      (when (yes-or-no-p (concat "Delete " old))
        (delete-directory old t)))))

(defun org-attach-tag (&optional off)
  "Turn the autotag on or (if OFF is set) off."
  (when org-attach-auto-tag
    (save-excursion
      (org-back-to-heading t)
      (org-toggle-tag org-attach-auto-tag (if off 'off 'on)))))

(defun org-attach-untag ()
  "Turn the autotag off."
  (org-attach-tag 'off))

(defun org-attach-store-link (file)
  "Add a link to `org-stored-link' when attaching a file.
Only do this when `org-attach-store-link-p' is non-nil."
  (setq org-stored-links
	(cons (list (org-attach-expand-link file)
		    (file-name-nondirectory file))
	      org-stored-links)))

(defun org-attach-url (url)
  (interactive "MURL of the file to attach: \n")
  (let ((org-attach-method 'url))
    (org-attach-attach url)))

(defun org-attach-buffer (buffer-name)
  "Attach BUFFER-NAME's contents to current outline node.
BUFFER-NAME is a string.  Signals a `file-already-exists' error
if it would overwrite an existing filename."
  (interactive "bBuffer whose contents should be attached: ")
  (let* ((attach-dir (org-attach-dir 'get-create))
	 (output (expand-file-name buffer-name attach-dir)))
    (when (file-exists-p output)
      (signal 'file-already-exists (list "File exists" output)))
    (run-hook-with-args 'org-attach-after-change-hook attach-dir)
    (org-attach-tag)
    (with-temp-file output
      (insert-buffer-substring buffer-name))))

(defun org-attach-attach (file &optional visit-dir method)
  "Move/copy/link FILE into the attachment directory of the current outline node.
If VISIT-DIR is non-nil, visit the directory with dired.
METHOD may be `cp', `mv', `ln', `lns' or `url' default taken from
`org-attach-method'."
  (interactive
   (list
    (read-file-name "File to keep as an attachment:"
                    (or (progn
                          (require 'dired-aux)
                          (dired-dwim-target-directory))
                        default-directory))
    current-prefix-arg
    nil))
  (setq method (or method org-attach-method))
  (let ((basename (file-name-nondirectory file)))
    (let* ((attach-dir (org-attach-dir 'get-create))
           (fname (expand-file-name basename attach-dir)))
      (cond
       ((eq method 'mv) (rename-file file fname))
       ((eq method 'cp) (copy-file file fname))
       ((eq method 'ln) (add-name-to-file file fname))
       ((eq method 'lns) (make-symbolic-link file fname))
       ((eq method 'url) (url-copy-file file fname)))
      (run-hook-with-args 'org-attach-after-change-hook attach-dir)
      (org-attach-tag)
      (cond ((eq org-attach-store-link-p 'attached)
             (org-attach-store-link fname))
            ((eq org-attach-store-link-p t)
             (org-attach-store-link file)))
      (if visit-dir
          (dired attach-dir)
        (message "File %S is now an attachment." basename)))))

(defun org-attach-attach-cp ()
  "Attach a file by copying it."
  (interactive)
  (let ((org-attach-method 'cp)) (call-interactively 'org-attach-attach)))
(defun org-attach-attach-mv ()
  "Attach a file by moving (renaming) it."
  (interactive)
  (let ((org-attach-method 'mv)) (call-interactively 'org-attach-attach)))
(defun org-attach-attach-ln ()
  "Attach a file by creating a hard link to it.
Beware that this does not work on systems that do not support hard links.
On some systems, this apparently does copy the file instead."
  (interactive)
  (let ((org-attach-method 'ln)) (call-interactively 'org-attach-attach)))
(defun org-attach-attach-lns ()
  "Attach a file by creating a symbolic link to it.

Beware that this does not work on systems that do not support symbolic links.
On some systems, this apparently does copy the file instead."
  (interactive)
  (let ((org-attach-method 'lns)) (call-interactively 'org-attach-attach)))

(defun org-attach-new (file)
  "Create a new attachment FILE for the current outline node.
The attachment is created as an Emacs buffer."
  (interactive "sCreate attachment named: ")
  (let ((attach-dir (org-attach-dir 'get-create)))
    (org-attach-tag)
    (find-file (expand-file-name file attach-dir))
    (message "New attachment %s" file)))

(defun org-attach-delete-one (&optional file)
  "Delete a single attachment."
  (interactive)
  (let* ((attach-dir (org-attach-dir))
	 (files (org-attach-file-list attach-dir))
	 (file (or file
		   (completing-read
		    "Delete attachment: "
		    (mapcar (lambda (f)
			      (list (file-name-nondirectory f)))
			    files)))))
    (setq file (expand-file-name file attach-dir))
    (unless (file-exists-p file)
      (error "No such attachment: %s" file))
    (delete-file file)
    (run-hook-with-args 'org-attach-after-change-hook attach-dir)))

(defun org-attach-delete-all (&optional force)
  "Delete all attachments from the current outline node.
This actually deletes the entire attachment directory.
A safer way is to open the directory in dired and delete from there."
  (interactive "P")
  (let ((attach-dir (org-attach-dir)))
    (when (and attach-dir
	       (or force
		   (yes-or-no-p "Really remove all attachments of this entry? ")))
      (delete-directory attach-dir (yes-or-no-p "Recursive?") t)
      (message "Attachment directory removed")
      (run-hook-with-args 'org-attach-after-change-hook attach-dir)
      (org-attach-untag))))

(defun org-attach-sync ()
  "Synchronize the current outline node with its attachments.
This can be used after files have been added externally."
  (interactive)
  (let ((attach-dir (org-attach-dir)))
    (when attach-dir
      (run-hook-with-args 'org-attach-after-change-hook attach-dir)
      (let ((files (org-attach-file-list attach-dir)))
	(org-attach-tag (not files))))
    (unless attach-dir (org-attach-tag t))))

(defun org-attach-file-list (dir)
  "Return a list of files in the attachment directory.
This ignores files ending in \"~\"."
  (delq nil
	(mapcar (lambda (x) (if (string-match "^\\.\\.?\\'" x) nil x))
		(directory-files dir nil "[^~]\\'"))))

(defun org-attach-reveal ()
  "Show the attachment directory of the current outline node.
This will attempt to use an external program to show the
directory.  Will create an attachment and folder if it doesn't
exist yet.  Respects `org-attach-preferred-new-method'."
  (interactive)
  (org-open-file (org-attach-dir-get-create)))

(defun org-attach-reveal-in-emacs ()
  "Show the attachment directory of the current outline node in dired.
Will create an attachment and folder if it doesn't exist yet.
Respects `org-attach-preferred-new-method'."
  (interactive)
  (dired (org-attach-dir-get-create)))

(defun org-attach-open (&optional in-emacs)
  "Open an attachment of the current outline node.
If there are more than one attachment, you will be prompted for the file name.
This command will open the file using the settings in `org-file-apps'
and in the system-specific variants of this variable.
If IN-EMACS is non-nil, force opening in Emacs."
  (interactive "P")
  (let ((attach-dir (org-attach-dir)))
    (if attach-dir
	(let* ((file (pcase (org-attach-file-list attach-dir)
		       (`(,file) file)
		       (files (completing-read "Open attachment: "
					       (mapcar #'list files) nil t))))
	       (path (expand-file-name file attach-dir)))
	  (run-hook-with-args 'org-attach-open-hook path)
	  (org-open-file path in-emacs))
      (error "No attachment directory exist"))))

(defun org-attach-open-in-emacs ()
  "Open attachment, force opening in Emacs.
See `org-attach-open'."
  (interactive)
  (org-attach-open 'in-emacs))

(defun org-attach-expand (file)
  "Return the full path to the current entry's attachment file FILE.
Basically, this adds the path to the attachment directory."
  (expand-file-name file (org-attach-dir)))

(defun org-attach-expand-link (file)
  "Return a file link pointing to the current entry's attachment file FILE.
Basically, this adds the path to the attachment directory, and a \"file:\"
prefix."
  (concat "file:" (org-attach-expand file)))

(org-link-set-parameters "attachment"
                         :follow #'org-attach-open-link
                         :export #'org-attach-export-link
                         :complete #'org-attach-complete-link)

(defun org-attach-open-link (link &optional in-emacs)
  "Attachment link type LINK is expanded with the attached directory and opened.

With optional prefix argument IN-EMACS, Emacs will visit the file.
With a double \\[universal-argument] \\[universal-argument] \
prefix arg, Org tries to avoid opening in Emacs
and to use an external application to visit the file."
  (interactive "P")
  (let (line search)
    (cond
     ((string-match "::\\([0-9]+\\)\\'" link)
      (setq line (string-to-number (match-string 1 link))
	    link (substring link 0 (match-beginning 0))))
     ((string-match "::\\(.+\\)\\'" link)
      (setq search (match-string 1 link)
            link (substring link 0 (match-beginning 0)))))
    (if (string-match "[*?{]" (file-name-nondirectory link))
        (dired (org-attach-expand link))
      (org-open-file (org-attach-expand link) in-emacs line search))))

(defun org-attach-complete-link ()
  "Advise the user with the available files in the attachment directory."
  (let ((attach-dir (org-attach-dir)))
    (if attach-dir
	(let* ((attached-dir (expand-file-name attach-dir))
	       (file (read-file-name "File: " attached-dir))
	       (pwd (file-name-as-directory attached-dir))
               (pwd-relative (file-name-as-directory
			      (abbreviate-file-name attached-dir))))
	  (cond
	   ((string-match (concat "^" (regexp-quote pwd-relative) "\\(.+\\)") file)
	    (concat "attachment:" (match-string 1 file)))
	   ((string-match (concat "^" (regexp-quote pwd) "\\(.+\\)")
			  (expand-file-name file))
	    (concat "attachment:" (match-string 1 (expand-file-name file))))
	   (t (concat "attachment:" file))))
      (error "No attachment directory exist"))))

(defun org-attach-export-link (link description format)
  "Translate attachment LINK from Org mode format to exported FORMAT.
Also includes the DESCRIPTION of the link in the export."
  (save-excursion
    (let (path desc)
      (cond
       ((string-match "::\\([0-9]+\\)\\'" link)
        (setq link (substring link 0 (match-beginning 0))))
       ((string-match "::\\(.+\\)\\'" link)
        (setq link (substring link 0 (match-beginning 0)))))
      (setq path (file-relative-name (org-attach-expand link))
            desc (or description link))
      (pcase format
        (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
        (`latex (format "\\href{%s}{%s}" path desc))
        (`texinfo (format "@uref{%s,%s}" path desc))
        (`ascii (format "%s (%s)" desc path))
        (`md (format "[%s](%s)" desc path))
        (_ path)))))

(defun org-attach-archive-delete-maybe ()
  "Maybe delete subtree attachments when archiving.
This function is called by `org-archive-hook'.  The option
`org-attach-archive-delete' controls its behavior."
  (when org-attach-archive-delete
    (org-attach-delete-all (not (eq org-attach-archive-delete 'query)))))


;; Attach from dired.

;; Add the following lines to the config file to get a binding for
;; dired-mode.

;; (add-hook
;;  'dired-mode-hook
;;  (lambda ()
;;    (define-key dired-mode-map (kbd "C-c C-x a") #'org-attach-dired-to-subtree))))

;;;###autoload
(defun org-attach-dired-to-subtree (files)
  "Attach FILES marked or current file in dired to subtree in other window.
Takes the method given in `org-attach-method' for the attach action.
Precondition: Point must be in a dired buffer.
Idea taken from `gnus-dired-attach'."
  (interactive
   (list (dired-get-marked-files)))
  (unless (eq major-mode 'dired-mode)
    (user-error "This command must be triggered in a dired buffer"))
  (let ((start-win (selected-window))
        (other-win
         (get-window-with-predicate
          (lambda (window)
            (with-current-buffer (window-buffer window)
              (eq major-mode 'org-mode))))))
    (unless other-win
      (user-error
       "Can't attach to subtree.  No window displaying an Org buffer"))
    (select-window other-win)
    (dolist (file files)
      (org-attach-attach file))
    (select-window start-win)
    (when (eq 'mv org-attach-method)
      (revert-buffer))))



(add-hook 'org-archive-hook 'org-attach-archive-delete-maybe)

(provide 'org-attach)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-attach.el ends here
