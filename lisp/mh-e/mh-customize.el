;;; mh-customize.el --- MH-E customization

;; Copyright (C) 2002, 2003 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; All of the defgroups, defcustoms, and deffaces in MH-E are found
;; here. This makes it possible to customize modules that aren't loaded
;; yet. It also makes it easier to organize the customization groups.

;; This file contains the following sections:
;;
;; 1. MH-E Customization Groups
;;
;;    These are the customization group definitions. These are organized in a
;;    logical order. High-level, windows and toolbar, folder, message,
;;    composing and hooks.
;;
;; 2. MH-E Customization
;;
;;    Here are the actual customization variables. There is a sub-section for
;;    each group in the MH-E Customization Groups section. Within each
;;    section, variables are sorted alphabetically. The manual section
;;    dictates which group a variable should be placed. New variables should
;;    be placed in the section where they would most likely be defined.
;;
;;    All hooks should be placed in the 'mh-hook group; in addition, add the
;;    group in which the hook is defined in the manual (or, if it is new,
;;    where it would be defined). These two actions insures that the hooks
;;    appear last in each group.
;;
;; 3. Faces

;;; Change Log:

;;; Code:
(provide 'mh-customize)
(require 'mh-e)

;;;###mh-autoload
(defun mh-customize (&optional delete-other-windows-flag)
  "Customize MH-E variables.
With optional argument DELETE-OTHER-WINDOWS-FLAG, other windows in the frame
are removed."
  (interactive "P")
  (customize-group 'mh)
  (when delete-other-windows-flag
    (delete-other-windows)))

;;; MH-E Customization Groups

(defgroup mh nil
  "GNU Emacs interface to the MH mail system."
  :link '(custom-manual "(mh-e)Top")
  :group 'mail)

(defgroup mh-toolbar nil
  "Toolbar configuration."
  :prefix "mh-"
  :group 'mh)

(defgroup mh-speed nil
  "Speedbar and folder configuration."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Customizing Moving Mail")
  :group 'mh)

(defgroup mh-folder nil
  "Options for controlling scan listing."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Customizing Moving Mail")
  :group 'mh)

(defgroup mh-index nil
  "Indexed searching."
  :link '(custom-manual "(mh-e)Customizing mh-e")
  :prefix "mh-"
  :group 'mh)

(defgroup mh-junk nil
  "Spam handling."
  :link '(custom-manual "(mh-e)Customizing mh-e")
  :prefix "mh-junk-"
  :group 'mh)

(defgroup mh-show nil
  "Message display."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Customizing Reading")
  :group 'mh)

(defgroup mh-faces nil
  "Faces used in MH-E."
  :link '(custom-manual "(mh-e)Customizing mh-e")
  :prefix "mh-"
  :group 'faces
  :group 'mh)

(defgroup mh-letter nil
  "Composing messages."
  :prefix "mh-"
  :link '(custom-manual "(mh-e)Customizing Sending")
  :group 'mh)

(defgroup mh-alias nil
  "Alias handling."
  :link '(custom-manual "(mh-e)Customizing mh-e")
  :prefix "mh-alias-"
  :group 'mh)

(defgroup mh-identity nil
  "Multiple personalities."
  :link '(custom-manual "(mh-e)Customizing mh-e")
  :prefix "mh-"
  :group 'mh)

(defgroup mh-hooks nil
  "MH-E hooks."
  :link '(custom-manual "(mh-e)Customizing mh-e")
  :prefix "mh-"
  :group 'mh)

;;; Faces

(defgroup mh-speed-faces nil
  "Faces used in speedbar."
  :link '(custom-manual "(mh-e)Customizing mh-e")
  :prefix "mh-"
  :group 'mh-faces
  :group 'mh-speed)

(defgroup mh-folder-faces nil
  "Faces used in scan listing."
  :link '(custom-manual "(mh-e)Customizing mh-e")
  :prefix "mh-"
  :group 'mh-faces
  :group 'mh-folder)

(defgroup mh-show-faces nil
  "Faces used in message display."
  :link '(custom-manual "(mh-e)Customizing mh-e")
  :prefix "mh-"
  :group 'mh-faces
  :group 'mh-show)

(defgroup mh-index-faces nil
  "Faces used in indexed searches."
  :link '(custom-manual "(mh-e)Customizing mh-e")
  :prefix "mh-"
  :group 'mh-faces
  :group 'mh-index)



;;; MH-E Customization (:group mh)

;;; Toolbar configuration (:group 'mh-toolbar)

(defcustom mh-tool-bar-search-function 'mh-search-folder
  "*Function called by the tool-bar search button.
See `mh-search-folder' and `mh-index-search' for details."
  :type '(choice (const mh-search-folder)
                 (const mh-index-search)
                 (function :tag "Other function"))
  :group 'mh-toolbar)

;; Functions called from the tool bar
(defun mh-tool-bar-search (&optional arg)
  "Interactively call `mh-tool-bar-search-function'.
Optional argument ARG is not used."
  (interactive "P")
  (call-interactively mh-tool-bar-search-function))

(defun mh-tool-bar-customize ()
  "Call `mh-customize' from the toolbar."
  (interactive)
  (mh-customize t))

(defun mh-tool-bar-folder-help ()
  "Visit \"(mh-e)Top\"."
  (interactive)
  (Info-goto-node "(mh-e)Top")
  (delete-other-windows))

(defun mh-tool-bar-letter-help ()
  "Visit \"(mh-e)Draft Editing\"."
  (interactive)
  (Info-goto-node "(mh-e)Draft Editing")
  (delete-other-windows))

(defmacro mh-tool-bar-reply-generator (function recipient folder-buffer-flag)
  "Generate FUNCTION that replies to RECIPIENT.
If FOLDER-BUFFER-FLAG is nil then the function generated
When INCLUDE-FLAG is non-nil, include message body being replied to."
  `(defun ,function (&optional arg)
     ,(format "Reply to \"%s\".\nWhen ARG is non-nil include message in reply."
              recipient)
     (interactive "P")
     ,(if folder-buffer-flag nil '(set-buffer mh-show-folder-buffer))
     (mh-reply (mh-get-msg-num nil) ,recipient arg)))

(mh-tool-bar-reply-generator mh-tool-bar-reply-from "from" t)
(mh-tool-bar-reply-generator mh-show-tool-bar-reply-from "from" nil)
(mh-tool-bar-reply-generator mh-tool-bar-reply-to "to" t)
(mh-tool-bar-reply-generator mh-show-tool-bar-reply-to "to" nil)
(mh-tool-bar-reply-generator mh-tool-bar-reply-all "all" t)
(mh-tool-bar-reply-generator mh-show-tool-bar-reply-all "all" nil)

;; XEmacs has a couple of extra customizations...
(mh-do-in-xemacs
  (require 'mh-xemacs-icons)
  (defcustom mh-xemacs-use-toolbar-flag (if (and (featurep 'toolbar)
                                                 (featurep 'xpm)
                                                 (device-on-window-system-p))
                                            t
                                          nil)
    "*If non-nil, use toolbar.

This will default to t if you are in an environment that supports
toolbars and xpm."
    :type 'boolean
    :group 'mh-toolbar)

  (defcustom mh-xemacs-toolbar-position (if mh-xemacs-use-toolbar-flag
                                            'default
                                          nil)
    "*Where to put the toolbar.

Valid non-nil values are \"default\", \"top\", \"bottom\", \"left\",
\"right\".  These match the four edges of the frame, with \"default\"
meaning \"use the same position as the default-toolbar\".

A nil value means do not use a toolbar.

If this variable is set to anything other than \"default\" and the
default-toolbar has a different positional setting from the value of
this variable, then two toolbars will be displayed.  The MH-E toolbar
and the default-toolbar."
    :type '(radio (const :tag "Same position as the \"default-toolbar\""
                         :value default)
                  (const :tag "Along the top edge of the frame"
                         :value top)
                  (const :tag "Along the bottom edge of the frame"
                         :value bottom)
                  (const :tag "Along the left edge of the frame"
                         :value left)
                  (const :tag "Along the right edge of the frame"
                         :value right)
                  (const :tag "Don't use a toolbar" nil))
    :group 'mh-toolbar))

(defmacro mh-tool-bar-define (defaults &rest buttons)
  "Define a tool bar for MH-E.
DEFAULTS is the list of buttons that are present by default. It is a list of
lists where the sublists are of the following form:

  (:KEYWORD FUNC1 FUNC2 FUNC3 ...)

Here :KEYWORD is one of :folder or :letter. If it is :folder then the default
buttons in the folder and show mode buffers are being specified. If it is
:letter then the default buttons in the letter mode are listed. FUNC1, FUNC2,
FUNC3, ... are the names of the functions that the buttons would execute.

Each element of BUTTONS is a list of four things:

  (FUNCTION MODES ICON DOC)

where,

  FUNCTION is the name of the function that will be executed when the button
  is clicked.

  MODES is a list of symbols. List elements must be from `folder', `letter' and
  `sequence'. If `folder' is present then the button is available in the
  folder and show buffer. If the name of FUNCTION is of the form \"mh-foo\",
  where foo is some arbitrary string, then we check if the function
  `mh-show-foo' exists. If it exists then that function is used in the show
  buffer. Otherwise the original function `mh-foo' is used in the show buffer
  as well. Presence of `sequence' is handled similar to the above. The only
  difference is that the button is shown only when the folder is narrowed to a
  sequence. If `letter' is present in MODES, then the button is available
  during draft editing and runs FUNCTION when clicked.

  ICON is the icon that is drawn in the button.

  DOC is the documentation for the button. It is used in tool-tips and in
  providing other help to the user. GNU Emacs uses only the first line of the
  string. So the DOC should be formatted such that the first line is useful and
  complete without the rest of the string."
  ;; The following variable names have been carefully chosen to make code
  ;; generation easier. Modifying the names should be done carefully.
  (let (folder-buttons folder-docs folder-button-setter sequence-button-setter
        show-buttons show-button-setter show-seq-button-setter
        letter-buttons letter-docs letter-button-setter
        folder-defaults letter-defaults
        folder-vectors show-vectors letter-vectors)
    (dolist (x defaults)
      (cond ((eq (car x) :folder) (setq folder-defaults (cdr x)))
            ((eq (car x) :letter) (setq letter-defaults (cdr x)))))
    (dolist (button buttons)
      (unless (and (listp button) (equal (length button) 4))
        (error "Incorrect MH-E tool-bar button specification: %s" button))
      (let* ((name (nth 0 button))
             (name-str (symbol-name name))
             (icon (nth 2 button))
             (xemacs-icon (mh-do-in-xemacs
                           (cdr (assoc (intern icon) mh-xemacs-icon-map))))
             (full-doc (nth 3 button))
             (doc (if (string-match "\\(.*\\)\n" full-doc)
                      (match-string 1 full-doc)
                    full-doc))
             (modes (nth 1 button))
             functions show-sym)
        (when (memq 'letter modes) (setq functions `(:letter ,name)))
        (when (or (memq 'folder modes) (memq 'sequence modes))
          (setq functions
                (append `(,(if (memq 'folder modes) :folder :sequence) ,name)
                        functions))
          (setq show-sym
                (if (string-match "^mh-\\(.*\\)$" name-str)
                    (intern (concat "mh-show-" (match-string 1 name-str)))
                  name))
          (setq functions
                (append `(,(if (memq 'folder modes) :show :show-seq)
                          ,(if (fboundp show-sym) show-sym name))
                        functions)))
        (do ((functions functions (cddr functions)))
            ((null functions))
          (let* ((type (car functions))
                 (function (cadr functions))
                 (type1 (substring (symbol-name type) 1))
                 (vector-list (cond ((eq type :show) 'show-vectors)
                                    ((eq type :show-seq) 'show-vectors)
                                    ((eq type :letter) 'letter-vectors)
                                    (t 'folder-vectors)))
                 (list (cond ((eq type :letter) 'mh-tool-bar-letter-buttons)
                             (t 'mh-tool-bar-folder-buttons)))
                 (key (intern (concat "mh-" type1 "toolbar-" name-str)))
                 (setter (intern (concat type1 "-button-setter")))
                 (mbuttons (cond ((eq type :letter) 'letter-buttons)
                                 ((eq type :show) 'show-buttons)
                                 ((eq type :show-seq) 'show-buttons)
                                 (t 'folder-buttons)))
                 (docs (cond ((eq mbuttons 'letter-buttons) 'letter-docs)
                             ((eq mbuttons 'folder-buttons) 'folder-docs))))
            (add-to-list vector-list `[,xemacs-icon ,function t ,full-doc])
            (add-to-list
             setter `(when (member ',name ,list)
                       (mh-funcall-if-exists
                        tool-bar-add-item ,icon ',function ',key :help ,doc)))
            (add-to-list mbuttons name)
            (if docs (add-to-list docs doc))))))
    (setq folder-buttons (nreverse folder-buttons)
          letter-buttons (nreverse letter-buttons)
          show-buttons (nreverse show-buttons)
          letter-docs (nreverse letter-docs)
          folder-docs (nreverse folder-docs)
          folder-vectors (nreverse folder-vectors)
          show-vectors (nreverse show-vectors)
          letter-vectors (nreverse letter-vectors))
    (dolist (x folder-defaults)
      (unless (memq x folder-buttons)
        (error "Folder defaults contains unknown button '%s'" x)))
    (dolist (x letter-defaults)
      (unless (memq x letter-buttons)
        (error "Letter defaults contains unknown button '%s'" x)))
    `(eval-when (compile load eval)
       (defvar mh-folder-tool-bar-map nil)
       (defvar mh-folder-seq-tool-bar-map nil)
       (defvar mh-show-tool-bar-map nil)
       (defvar mh-show-seq-tool-bar-map nil)
       (defvar mh-letter-tool-bar-map nil)
       ;; GNU Emacs tool bar specific code
       (mh-do-in-gnu-emacs
         ;; Custom setter functions
         (defun mh-tool-bar-folder-buttons-set (symbol value)
           "Construct toolbar for `mh-folder-mode' and `mh-show-mode'."
           (set-default symbol value)
           (setq mh-folder-tool-bar-map
                 (let ((tool-bar-map (make-sparse-keymap)))
                   ,@(nreverse folder-button-setter)
                   tool-bar-map))
           (setq mh-show-tool-bar-map
                 (let ((tool-bar-map (make-sparse-keymap)))
                   ,@(nreverse show-button-setter)
                   tool-bar-map))
           (setq mh-show-seq-tool-bar-map
                 (let ((tool-bar-map (copy-keymap mh-show-tool-bar-map)))
                   ,@(nreverse show-seq-button-setter)
                   tool-bar-map))
           (setq mh-folder-seq-tool-bar-map
                 (let ((tool-bar-map (copy-keymap mh-folder-tool-bar-map)))
                   ,@(nreverse sequence-button-setter)
                   tool-bar-map)))
         (defun mh-tool-bar-letter-buttons-set (symbol value)
           "Construct toolbar for `mh-letter-mode'."
           (set-default symbol value)
           (setq mh-letter-tool-bar-map
                 (let ((tool-bar-map (make-sparse-keymap)))
                   ,@(nreverse letter-button-setter)
                   tool-bar-map))))
       ;; XEmacs specific code
       (mh-do-in-xemacs
         (defvar mh-toolbar-folder-vector-map
           ',(loop for button in folder-buttons
                   for vector in folder-vectors
                   collect (cons button vector)))
         (defvar mh-toolbar-show-vector-map
           ',(loop for button in show-buttons
                   for vector in show-vectors
                   collect (cons button vector)))
         (defvar mh-toolbar-letter-vector-map
           ',(loop for button in letter-buttons
                   for vector in letter-vectors
                   collect (cons button vector)))
         (defvar mh-toolbar-folder-buttons nil)
         (defvar mh-toolbar-show-buttons nil)
         (defvar mh-toolbar-letter-buttons nil)
         ;; Custom setter functions
         (defun mh-tool-bar-letter-buttons-set (symbol value)
           (set-default symbol value)
           (setq mh-toolbar-letter-buttons
                 (loop for b in value
                       collect (cdr (assoc b mh-toolbar-letter-vector-map)))))
         (defun mh-tool-bar-folder-buttons-set (symbol value)
           (set-default symbol value)
           (setq mh-toolbar-folder-buttons
                 (loop for b in value
                       collect (cdr (assoc b mh-toolbar-folder-vector-map))))
           (setq mh-toolbar-show-buttons
                 (loop for b in value
                       collect (cdr (assoc b mh-toolbar-show-vector-map)))))
         ;; Initialize toolbar
         (defun mh-toolbar-init (mode)
           "Install toolbar in MODE."
           (let ((toolbar (cond ((eq mode :folder) mh-toolbar-folder-buttons)
                                ((eq mode :letter) mh-toolbar-letter-buttons)
                                ((eq mode :show) mh-toolbar-show-buttons)))
                 (height 37)
                 (width 40)
                 (buffer (current-buffer)))
             (when (and mh-xemacs-toolbar-position mh-xemacs-use-toolbar-flag)
               (cond
                ((eq mh-xemacs-toolbar-position 'top)
                 (set-specifier top-toolbar (cons buffer toolbar))
                 (set-specifier top-toolbar-visible-p t)
                 (set-specifier top-toolbar-height height))
                ((eq mh-xemacs-toolbar-position 'bottom)
                 (set-specifier bottom-toolbar (cons buffer toolbar))
                 (set-specifier bottom-toolbar-visible-p t)
                 (set-specifier bottom-toolbar-height height))
                ((eq mh-xemacs-toolbar-position 'left)
                 (set-specifier left-toolbar (cons buffer toolbar))
                 (set-specifier left-toolbar-visible-p t)
                 (set-specifier left-toolbar-width width))
                ((eq mh-xemacs-toolbar-position 'right)
                 (set-specifier right-toolbar (cons buffer toolbar))
                 (set-specifier right-toolbar-visible-p t)
                 (set-specifier right-toolbar-width width))
                (t (set-specifier default-toolbar (cons buffer toolbar))))))))
       ;; Declare customizable toolbars
       (custom-declare-variable
        'mh-tool-bar-folder-buttons
        '(list ,@(mapcar (lambda (x) `(quote ,x)) folder-defaults))
        "Choose buttons to include in MH-E folder/show toolbar."
        :group 'mh-toolbar :set 'mh-tool-bar-folder-buttons-set
        :type '(set ,@(loop for x in folder-buttons
                            for y in folder-docs
                            collect `(const :tag ,y ,x))))
       (custom-declare-variable
        'mh-tool-bar-letter-buttons
        '(list ,@(mapcar (lambda (x) `(quote ,x)) letter-defaults))
        "Choose buttons to include in MH-E letter toolbar."
        :group 'mh-toolbar :set 'mh-tool-bar-letter-buttons-set
        :type '(set ,@(loop for x in letter-buttons
                            for y in letter-docs
                            collect `(const :tag ,y ,x)))))))

(mh-tool-bar-define
    ((:folder mh-inc-folder mh-mime-save-parts mh-previous-undeleted-msg
              mh-page-msg  mh-next-undeleted-msg mh-delete-msg mh-refile-msg
              mh-undo mh-execute-commands mh-toggle-tick mh-reply
              mh-alias-grab-from-field mh-send mh-rescan-folder
              mh-tool-bar-search mh-visit-folder
              mh-tool-bar-customize mh-tool-bar-folder-help mh-widen)
     (:letter mh-send-letter mh-compose-insertion ispell-message save-buffer
              undo kill-region menu-bar-kill-ring-save yank mh-fully-kill-draft
              mh-tool-bar-customize mh-tool-bar-letter-help))
  ;; Folder/Show buffer buttons
  (mh-inc-folder (folder) "mail"
    "Incorporate new mail in Inbox
This button runs `mh-inc-folder' which drags any
new mail into your Inbox folder.")
  (mh-mime-save-parts (folder) "attach"
    "Save MIME parts from this message
This button runs `mh-mime-save-parts' which saves a message's
different parts into separate files.")
  (mh-previous-undeleted-msg (folder) "left_arrow"
    "Go to the previous undeleted message
This button runs `mh-previous-undeleted-msg'")
  (mh-page-msg (folder) "page-down"
    "Page the current message forwards\nThis button runs `mh-page-msg'")
  (mh-next-undeleted-msg (folder) "right_arrow"
    "Go to the next undeleted message\nThe button runs `mh-next-undeleted-msg'")
  (mh-delete-msg (folder) "close"
    "Mark this message for deletion\nThis button runs `mh-delete-msg'")
  (mh-refile-msg (folder) "refile"
    "Refile this message\nThis button runs `mh-refile-msg'")
  (mh-undo (folder) "undo" "Undo last operation\nThis button runs `undo'")
  (mh-execute-commands (folder) "execute"
    "Perform moves and deletes\nThis button runs `mh-execute-commands'")
  (mh-toggle-tick (folder) "highlight"
    "Toggle tick mark\nThis button runs `mh-toggle-tick'")
  (mh-toggle-showing (folder) "show"
    "Toggle showing message\nThis button runs `mh-toggle-showing'")
  (mh-tool-bar-reply-from (folder) "reply-from" "Reply to \"from\"")
  (mh-tool-bar-reply-to (folder) "reply-to" "Reply to \"to\"")
  (mh-tool-bar-reply-all (folder) "reply-all" "Reply to \"all\"")
  (mh-reply (folder) "mail/reply2"
    "Reply to this message\nThis button runs `mh-reply'")
  (mh-alias-grab-from-field (folder) "alias"
    "Grab From alias\nThis button runs `mh-alias-grab-from-field'")
  (mh-send (folder) "mail_compose"
    "Compose new message\nThis button runs `mh-send'")
  (mh-rescan-folder (folder) "rescan"
    "Rescan this folder\nThis button runs `mh-rescan-folder'")
  (mh-pack-folder (folder) "repack"
    "Repack this folder\nThis button runs `mh-pack-folder'")
  (mh-tool-bar-search (folder) "search"
    "Search\nThis button runs `mh-tool-bar-search-function'")
  (mh-visit-folder (folder) "fld_open"
    "Visit other folder\nThis button runs `mh-visit-folder'")
  ;; Letter buffer buttons
  (mh-send-letter (letter) "mail_send" "Send this letter")
  (mh-compose-insertion (letter) "attach" "Insert attachment")
  (ispell-message (letter) "spell" "Check spelling")
  (save-buffer (letter) "save" "Save current buffer to its file")
  (undo (letter) "undo" "Undo last operation")
  (kill-region (letter) "cut"
    "Cut (kill) text in region between mark and current position")
  (menu-bar-kill-ring-save (letter) "copy"
    "Copy text in region between mark and current position")
  (yank (letter) "paste" "Paste (yank) text cut or copied earlier")
  (mh-fully-kill-draft (letter) "close" "Kill this draft")
  ;; Common buttons
  (mh-tool-bar-customize (folder letter) "preferences" "MH-E Preferences")
  (mh-tool-bar-folder-help (folder) "help"
    "Help! (general help)\nThis button runs `Info-goto-node'")
  (mh-tool-bar-letter-help (letter) "help"
    "Help! (general help)\nThis button runs `Info-goto-node'")
  ;; Folder narrowed to sequence buttons
  (mh-widen (sequence) "widen"
    "Widen from the sequence\nThis button runs `mh-widen'"))



;;; Speedbar and folder configuration (:group 'mh-speed)

(defcustom mh-large-folder 200
  "The number of messages that indicates a large folder.
If a folder is deemed to be large, that is the number of messages in it exceed
this value, then confirmation is needed when it is visited. Even when
`mh-show-threads-flag' is non-nil, the folder is not automatically threaded, if
it is large. If set to nil all folders are treated as if they are small."
  :type '(choice (const :tag "No limit") integer)
  :group 'mh-speed)

(defcustom mh-speed-flists-interval 60
  "Time between calls to flists in seconds.
If 0, flists is not called repeatedly."
  :type 'integer
  :group 'mh-speed)

(defcustom mh-speed-run-flists-flag t
  "Non-nil means flists is used.
If non-nil, flists is executed every `mh-speed-flists-interval' seconds to
update the display of the number of unseen and total messages in each folder.
If resources are limited, this can be set to nil and the speedbar display can
be updated manually with the \\[mh-speed-flists] command."
  :type 'boolean
  :group 'mh-speed)



;;; Options for controlling scan listing (:group 'mh-folder)

(defcustom mh-adaptive-cmd-note-flag t
  "*Non-nil means that the message number width is determined dynamically.
This is done once when a folder is first opened by running scan on the last
message of the folder. The message number for the last message is extracted
and its width calculated. This width is used when calling `mh-set-cmd-note'.

If you prefer fixed-width message numbers, set this variable to nil and call
`mh-set-cmd-note' with the width specified by the scan format in
`mh-scan-format-file'. For example, the default width is 4, so you would use
\"(mh-set-cmd-note 4)\" if `mh-scan-format-file' were nil."
  :type 'boolean
  :group 'mh-folder)

(defcustom mh-default-folder-list nil
  "*Alist of addresses and folders.
When refiling messages, these folders are the default that is provided if the
sender (or recipient if the Check Recipient checkbox has been selected) has
the associated address, a regexp. The first entry to match will be used, so
order them according to the wanted priority. You do not need to list your
aliases here as that lookup is already performed.

See `mh-prompt-for-refile-folder' and `mh-folder-from-address' for more
information."
  :type '(repeat (list (regexp :tag "Address")
                       (string :tag "Folder")
                       (boolean :tag "Check Recipient")))
  :group 'mh-folder)

(defcustom mh-default-folder-must-exist-flag t
  "*Non-nil means guessed folder name must exist to be used.
If this variable is t, then the guessed name is only used if the folder
already exists\; if the folder doesn't exist, then the last folder name used
is suggested. This is useful if you get mail from various people for whom you
have an alias, but file them all in the same project folder.
See `mh-prompt-for-refile-folder' and `mh-folder-from-address' for more
information."
  :type 'boolean
  :group 'mh-folder)

(defcustom mh-default-folder-prefix ""
  "*Prefix used for guessed folder names.
This can be used to put folders associated with your aliases in a sub-folder
so as to not clutter your mail directory.
See `mh-prompt-for-refile-folder' and `mh-folder-from-address' for more
information."
  :type 'string
  :group 'mh-folder)

(defcustom mh-inc-prog "inc"
  "*Program to run to incorporate new mail into a folder.
Normally \"inc\".  This file is searched for relative to
the `mh-progs' directory unless it is an absolute pathname."
  :type 'string
  :group 'mh-folder)


(defcustom mh-inc-spool-list nil
  "*Alist of alternate spool files, corresponding folders and keybindings.
Here's an example. Suppose you have subscribed to the MH-E devel mailing
list. You could filter its mail into a separate spool file named
~/mail/mh-e using Procmail and a .procmailrc entry like:

MAILDIR=$HOME/mail      #you'd better make sure it exists
:0:
* ^From mh-e-devel-admin@lists.sourceforge.net
mh-e

If you wanted to incorporate that spool file into an MH folder called
mh-e by pressing \"I m\" in folder-mode or by `M-x mh-inc-spool-mh-e',
you would setup  `mh-inc-spool-list' with an entry:

 Spool file:  ~/mail/mh-e
 Folder:      mh-e
 Key binding: m

Then, you could also install `xbuffy' and configure an extra mailbox like so:

box ~/mail/mh-e
 title mh-e
 origMode
 polltime 10
 headertime 0
 command gnudoit -q '(mh-inc-spool-mh-e)'

Note that the entry above uses the gnuserv package to communicate the
command `mh-inc-spool-mh-e' to Emacs. It will incorporate the spool file
when clicking the xbuffy box with the middle mouse button."
  :type '(repeat (list (file :tag "Spool file")
                       (string :tag "Folder")
                       (character :tag "Key binding")))
  :set 'mh-inc-spool-list-set
  :group 'mh-folder)

(defcustom mh-lpr-command-format "lpr -J '%s'"
  "*Format for Unix command that prints a message.
The string should be a Unix command line, with the string '%s' where
the job's name (folder and message number) should appear.  The formatted
message text is piped to this command when you type \\<mh-folder-mode-map>`\\[mh-print-msg]'."
  :type 'string
  :group 'mh-folder)

(defcustom mh-mime-save-parts-default-directory t
  "Default directory to use for `mh-mime-save-parts'.
If nil, prompt and set for next time the command is used during same session.
If t, prompt always"
  :type '(choice (const :tag "Prompt the first time" nil)
                 (const :tag "Prompt always" t)
                 directory)
  :group 'mh-folder)

(defcustom mh-print-background-flag nil
  "*Non-nil means messages should be printed in the background.
WARNING: do not delete the messages until printing is finished;
otherwise, your output may be truncated."
  :type 'boolean
  :group 'mh-folder)

(defcustom mh-recenter-summary-flag nil
  "*Non-nil means to recenter the summary window.
Recenter the summary window when the show window is toggled off if non-nil."
  :type 'boolean
  :group 'mh-folder)

(defcustom mh-recursive-folders-flag nil
  "*Non-nil means that commands which operate on folders do so recursively."
  :type 'boolean
  :group 'mh-folder)

(defcustom mh-scan-format-file t
  "Specifies the format file to pass to the scan program.
If t, the format string will be taken from the either `mh-scan-format-mh'
or `mh-scan-format-nmh' depending on whether MH or nmh is in use.
If nil, the default scan output will be used.

If you customize the scan format, you may need to modify a few variables
containing regexps that MH-E uses to identify specific portions of the output.
Use `M-x apropos RET mh-scan.*regexp' to obtain a list of these variables. You
may also have to call `mh-set-cmd-note' with the width of your message
numbers. See also `mh-adaptive-cmd-note-flag'."
  :type '(choice (const :tag "Use MH-E scan format" t)
                 (const :tag "Use default scan format" nil)
                 (file  :tag "Specify a scan format file"))
  :group 'mh-folder)

(defcustom mh-scan-prog "scan"
  "*Program to run to generate one-line-per-message listing of a folder.
Normally \"scan\" or a file name linked to scan.  This file is searched
for relative to the `mh-progs' directory unless it is an absolute pathname."
  :type 'string
  :group 'mh-folder)
(make-variable-buffer-local 'mh-scan-prog)

(defcustom mh-show-threads-flag nil
  "Non-nil means new folders start in threaded mode.
Threading large number of messages can be time consuming. So if the flag is
non-nil then threading will be done only if the number of messages being
threaded is less than `mh-large-folder'."
  :type 'boolean
  :group 'mh-folder)

(defcustom mh-store-default-directory nil
  "*Last directory used by \\[mh-store-msg]; default for next store.
A directory name string, or nil to use current directory."
  :type '(choice (const :tag "Current" nil)
                 directory)
  :group 'mh-folder)

(defcustom mh-tick-seq 'tick
  "The name of the MH tick sequence."
  :type '(choice (const :tag "Disable ticking" nil)
                 symbol)
  :group 'mh-folder)

(defcustom mh-update-sequences-after-mh-show-flag t
  "*Non-nil means `mh-update-sequence' is called from `mh-show-mode'.
If set, `mh-update-sequence' is run every time a message is shown, telling
MH or nmh that this is your current message.  It's useful, for example, to
display MIME content using \"M-! mhshow RET\""
  :type 'boolean
  :group 'mh-folder)



;;; Indexed searching (:group 'mh-index)

(defcustom mh-index-new-messages-folders t
  "Folders searched for `mh-unseen-seq'.
If t, then `mh-inbox' is searched. If nil, all the top level folders are
searched. Otherwise the list of folders specified as strings are searched.
See also `mh-recursive-folders-flag'."
  :group 'mh-index
  :type '(choice (const :tag "Inbox" t)
                 (const :tag "All" nil)
                 (repeat :tag "Choose folders" (string :tag "Folder"))))

(defcustom mh-index-program nil
  "Indexing program that MH-E shall use.
The possible choices are swish++, swish-e, mairix, namazu, glimpse, pick and
grep. By default this variable is nil which means that the programs are tried
in order and the first one found is used.

More information about setting up an indexing program to use with MH-E can be
found in the documentation of `mh-index-search'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const :tag "swish++" swish++)
                 (const :tag "swish-e" swish)
                 (const :tag "mairix" mairix)
                 (const :tag "namazu" namazu)
                 (const :tag "glimpse" glimpse)
                 (const :tag "pick" pick)
                 (const :tag "grep" grep))
  :group 'mh-index)



;;; Spam Handling (:group 'mh-junk)

;; Spam fighting program chosen
(defvar mh-junk-choice nil)

;; Available spam filter interfaces
(defvar mh-junk-function-alist
  '((bogofilter mh-bogofilter-blacklist mh-bogofilter-whitelist)
    (spamprobe mh-spamprobe-blacklist mh-spamprobe-whitelist)
    (spamassassin mh-spamassassin-blacklist mh-spamassassin-whitelist))
  "Available choices of spam programs to use.
This is an alist. For each element there are functions that blacklist a message
as spam and whitelist a message incorrectly classified as spam.")

(defun mh-junk-choose (symbol value)
  "Choose spam program to use.
The function is always called with SYMBOL bound to `mh-junk-program' and VALUE
bound to the new value of `mh-junk-program'. The function sets the variable
`mh-junk-choice' in addition to `mh-junk-program'."
  (set symbol value)
  (setq mh-junk-choice
        (or value
            (loop for element in mh-junk-function-alist
                  until (executable-find (symbol-name (car element)))
                  finally return (car element)))))

;; User customizable variables
(defcustom mh-junk-mail-folder nil
  "Folder to put spam mail in.
If nil then the spam is deleted."
  :type '(choice (const :tag "Delete spam" nil)
                 (string :tag "Spam folder"))
  :group 'mh-junk)

(defcustom mh-junk-program nil
  "Spam program that MH-E shall use.
The possible choices are bogofilter, spamprobe, and spamassassin. By default
this variable is nil which means that the programs are tried in order and the
first one found is used."
  :type '(choice (const :tag "auto-detect" nil)
                 (const :tag "bogofilter" bogofilter)
                 (const :tag "spamprobe" spamprobe)
                 (const :tag "spamassassin" spamassassin))
  :set 'mh-junk-choose
  :group 'mh-junk)



;;; Message display (:group 'mh-show)

(defcustom mh-bury-show-buffer-flag t
  "*Non-nil means that the displayed show buffer for a folder is buried."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-clean-message-header-flag t
  "*Non-nil means clean headers of messages that are displayed or inserted.
The variables `mh-invisible-headers' and `mh-visible-headers' control
what is removed."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-decode-mime-flag (not (not (locate-library "mm-decode")))
  "*Non-nil means that Gnus is used to show MIME attachments with Gnus."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-display-buttons-for-inline-parts-flag nil
  "*Non-nil means display buttons for all inline MIME parts.
If non-nil, buttons are displayed for all MIME parts. Inline parts start off
in displayed state but they can be hidden by clicking the button. If nil no
buttons are shown for inline parts."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-do-not-confirm-flag nil
  "*Non-nil means do not prompt for confirmation.
Commands such as `mh-pack-folder' prompt to confirm whether to process
outstanding moves and deletes or not before continuing. A non-nil setting will
perform the action--which is usually desired but cannot be retracted--without
question."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-fetch-x-image-url nil
  "Control fetching of X-Image-URL header field image.
This setting only has effect if `mh-show-use-xface-flag' is non-nil.

If set to t, the image is fetched.

If set to 'ask, the user is prompted before the image is fetched. MH-E will
remember your reply and will either use the already fetched image the next time
the same URL is encountered or silently skip it if you didn't fetch it the
first time.

If set to nil, the default, images are not fetched and only displayed if they
are already present in the cache."
  :type '(choice (const :tag "Always fetch" t)
                 (const :tag "Ask before fetching" ask)
                 (const :tag "Never fetch" nil))
  :group 'mh-show)

(defcustom mh-graphical-smileys-flag t
  "*Non-nil means graphical smileys are displayed.
Non-nil means that small graphics will be used in the show buffer instead of
patterns like :-), ;-) etc. The setting only has effect if
`mh-decode-mime-flag' is non-nil."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-graphical-emphasis-flag t
  "*Non-nil means graphical emphasis is displayed.
Non-nil means that _underline_ will be underlined, *bold* will appear in bold,
/italic/ will appear in italic etc. See `gnus-emphasis-alist' for the whole
list. The setting only has effect if `mh-decode-mime-flag' is non-nil."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-highlight-citation-p 'gnus
  "How to highlight citations in show buffers.
The gnus method uses a different color for each indentation."
  :type '(choice (const :tag "Use Gnus" gnus)
                 (const :tag "Use font-lock" font-lock)
                 (const :tag "Don't fontify" nil))
  :group 'mh-show)

(defvar mh-invisible-headers nil
  "*Regexp matching lines in a message header that are not to be shown.
Use the function `mh-invisible-headers' to generate this variable.
If `mh-visible-headers' is non-nil, it is used instead to specify what
to keep.")

(defun mh-invisible-headers ()
  "Make or remake the variable `mh-invisible-headers'.
Done using `mh-invisible-header-fields' as input."
  (setq mh-invisible-headers
        (concat
         "^"
         (let ((max-specpdl-size 1000) ;workaround for insufficient default
               (fields mh-invisible-header-fields))
           (regexp-opt fields t)))))

(defun mh-invisible-header-fields-set (symbol value)
  "Update `mh-invisible-header-fields'.
The function is called with SYMBOL bound to `mh-invisible-header-fields' and
VALUE is the the list of headers that are invisible. As a side effect, the
variable `mh-invisible-fields' is set."
  (set-default symbol value)
  (mh-invisible-headers))

;; Keep fields alphabetized. Mention source, if known.
(defcustom mh-invisible-header-fields
  '("Approved:"
    "Autoforwarded:"
    "Bestservhost:"
    "Cancel-Lock:"                      ; NNTP posts
    "Content-"                          ; RFC 2045
    "Delivered-To:"              ; Egroups/yahoogroups mailing list manager
    "Delivery-Date:"                    ; MH
    "Delivery:"
    "Encoding:"
    "Errors-To:"
    "Face:"                             ; Gnus Face header
    "Forwarded:"                        ; MH
    "From "                             ; sendmail
    "Importance:"                       ; MS Outlook
    "In-Reply-To:"                      ; MH
    "Lines:"
    "List-"                             ; Mailman mailing list manager
    "List-"                             ; Unknown mailing list managers
    "List-Subscribe:"                   ; Unknown mailing list managers
    "List-Unsubscribe:"                 ; Unknown mailing list managers
    "Mail-from:"                        ; MH
    "Mailing-List:"              ; Egroups/yahoogroups mailing list manager
    "Message-Id:"                       ; RFC 822
    "Mime-Version"                      ; RFC 2045
    "NNTP-"                             ; News
    "Old-Return-Path:"
    "Original-Encoded-Information-Types:"  ; X400
    "Original-Lines:"                   ; mail to news
    "Original-Newsgroups:"              ; mail to news
    "Original-NNTP-"                    ; mail to news
    "Original-Path:"                    ; mail to news
    "Original-Received:"                ; mail to news
    "Original-To:"                      ; mail to news
    "Original-X-"                       ; mail to news
    "P1-Content-Type:"                  ; X400
    "P1-Message-Id:"                    ; X400
    "P1-Recipient:"                     ; X400
    "Path:"
    "Precedence:"
    "Prev-Resent"                       ; MH
    "Priority:"
    "Received:"                         ; RFC 822
    "References:"
    "Remailed-"                         ; MH
    "Replied:"                          ; MH
    "Resent"                            ; MH
    "Return-Path:"                      ; RFC 822
    "Sensitivity:"                      ; MS Outlook
    "Status:"                           ; sendmail
    "Ua-Content-Id:"                    ; X400
    "User-Agent:"
    "Via:"                              ; MH
    "X-Abuse-Info:"
    "X-Accept-Language:"
    "X-Accept-Language:"                ; Netscape/Mozilla
    "X-Ack:"
    "X-Apparently-From:"                ; MS Outlook
    "X-Apparently-To:"           ; Egroups/yahoogroups mailing list manager
    "X-Authentication-Warning:"         ; sendmail
    "X-Beenthere:"                      ; Mailman mailing list manager
    "X-Bogosity:"                       ; bogofilter
    "X-Complaints-To:"
    "X-Cron-Env:"
    "X-Delivered"
    "X-Envelope-Sender:"
    "X-Envelope-To:"
    "X-Face:"
    "X-Folder:"                         ; Spam
    "X-From-Line"
    "X-Gnus-Mail-Source:"               ; gnus
    "X-Habeas-SWE-1:"                   ; Spam
    "X-Habeas-SWE-2:"                   ; Spam
    "X-Habeas-SWE-3:"                   ; Spam
    "X-Habeas-SWE-4:"                   ; Spam
    "X-Habeas-SWE-5:"                   ; Spam
    "X-Habeas-SWE-6:"                   ; Spam
    "X-Habeas-SWE-7:"                   ; Spam
    "X-Habeas-SWE-8:"                   ; Spam
    "X-Habeas-SWE-9:"                   ; Spam
    "X-Info:"                           ; NTMail
    "X-Juno-"                           ; Juno
    "X-List-Host:"                      ; Unknown mailing list managers
    "X-List-Subscribe:"                 ; Unknown mailing list managers
    "X-List-Unsubscribe:"               ; Unknown mailing list managers
    "X-Listserver:"                     ; Unknown mailing list managers
    "X-Loop:"                           ; Unknown mailing list managers
    "X-MIME-Autoconverted:"             ; sendmail
    "X-MIMETrack:"
    "X-MS-TNEF-Correlator:"             ; MS Outlook
    "X-Mailing-List:"                   ; Unknown mailing list managers
    "X-Mailman-Version:"                ; Mailman mailing list manager
    "X-Majordomo:"                      ; Majordomo mailing list manager
    "X-Message-Id"
    "X-MHE-Checksum"                    ; Checksum added during index search
    "X-MimeOLE:"                        ; MS Outlook
    "X-Mozilla-Status:"                 ; Netscape/Mozilla
    "X-Msmail-"                         ; MS Outlook
    "X-News:"                           ; News
    "X-No-Archive:"
    "X-Notes-Item:"                     ; Lotus Notes Domino structured header
    "X-Orcl-Content-Type:"
    "X-Original-Complaints-To:"
    "X-Original-Date:"                  ; SourceForge mailing list manager
    "X-Original-Trace:"
    "X-OriginalArrivalTime:"            ; Hotmail
    "X-Originating-IP:"                 ; Hotmail
    "X-Priority:"                       ; MS Outlook
    "X-Qotd-"                           ; User added
    "X-Received-Date:"
    "X-Received:"
    "X-Request-"
    "X-SBClass:"                        ; Spam
    "X-SBNote:"                         ; Spam
    "X-SBPass:"                         ; Spam
    "X-SBRule:"                         ; Spam
    "X-Scanned-By"
    "X-Sender:"
    "X-Server-Date:"
    "X-Server-Uuid:"
    "X-Sieve:"                          ; Sieve filtering
    "X-Spam-Checker-Version:"           ; Spamassassin
    "X-Spam-Level:"                     ; Spamassassin
    "X-Spam-Score:"                     ; Spamassassin
    "X-Spam-Status:"                    ; Spamassassin
    "X-SpamBouncer:"                    ; Spam
    "X-Trace:"
    "X-UIDL:"
    "X-UserInfo1:"
    "X-VSMLoop:"                        ; NTMail
    "X-Vms-To:"
    "X-Wss-Id:"                         ; Worldtalk gateways
    "X-eGroups-"                 ; Egroups/yahoogroups mailing list manager
    "X-pgp:"
    "X-submission-address:"
    "X400-"                             ; X400
    "Xref:")
"*List of header fields that are not to be shown.
Regexps are not allowed. Unique fields should have a \":\" suffix; otherwise,
the element can be used to render invisible an entire class of fields that
start with the same prefix.
This variable is ignored if `mh-visible-headers' is set."
  :type '(repeat (string :tag "Header field"))
  :set 'mh-invisible-header-fields-set
  :group 'mh-show)

(defcustom mh-max-inline-image-height nil
  "*Maximum inline image height if Content-Disposition is not present.
If nil, image will be displayed if its height is smaller than the height of
the window."
  :type '(choice (const nil) integer)
  :group 'mh-show)

(defcustom mh-max-inline-image-width nil
  "*Maximum inline image width if Content-Disposition is not present.
If nil, image will be displayed if its width is smaller than the width of the
window."
  :type '(choice (const nil) integer)
  :group 'mh-show)

(defcustom mh-show-maximum-size 0
  "*Maximum size of message (in bytes) to display automatically.
Provides an opportunity to skip over large messages which may be slow to load.
Use a value of 0 to display all messages automatically regardless of size."
  :type 'integer
  :group 'mh-show)

;; Use goto-addr if it was already loaded (which probably sets this
;; variable to t), or if this variable is otherwise set to t.
(defcustom mh-show-use-goto-addr-flag (and (boundp 'goto-address-highlight-p)
                                           goto-address-highlight-p)
  "*Non-nil means highlight URLs and email addresses.
The `goto-addr' module is used."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-show-use-xface-flag (>= emacs-major-version 21)
  "*Non-nil means display face images in `mh-show-mode'.
This flag controls the display of three kinds of faces.

The first is the traditional X-Face header field. For GNU Emacs 21
and above, the `uncompface' binary is required to be in the execute
PATH for the display of X-Face images. It can be obtained from
ftp://ftp.cs.indiana.edu/pub/faces/compface/compface.tar.Z.

If the XEmacs you are using has internal support for X-Face images, then MH-E
will display X-Face images in XEmacs \"out of the box\". Even if you don't have
X-Face support compiled into your XEmacs, you can still see the X-Face images
in MH-E with the aid of an external x-face package and `uncompface'. It is
available from ftp://ftp.jpl.org/pub/elisp/. Download it, put its files in the
`load-path' and MH-E will invoke it automatically.

Second, MH-E supports the display of the Gnus-specific Face
header field in GNU Emacs >= 21 and XEmacs. No external packages
are required. More information about the Face header can be found
at: http://quimby.gnus.org/circus/face/.

Finally, MH-E can also display images from the X-Image-URL header field. The
display of the images requires the `wget' program, available from
http://www.gnu.org/software/wget/wget.html, to fetch the image and the
`convert' program from the ImageMagick suite, available from
http://www.imagemagick.org/. Of the three header fields this is the most
efficient in terms of network usage since the image doesn't need to be
transmitted with every single mail. However its display needs the recipient to
fetch a URL and this can be misused. So it is disabled by default. It can be
enabled by customizing `mh-fetch-x-image-url'. Setting that to ask for
confirmation before fetching seems like a good choice.

Versions of GNU Emacs prior to 21.1 don't support the display of
inline images. So face images are not displayed in these versions."
  :type 'boolean
  :group 'mh-show)

(defcustom mh-summary-height (or (and (fboundp 'frame-height)
                                      (> (frame-height) 24)
                                      (min 10 (/ (frame-height) 6)))
                                 4)
  "*Number of lines in MH-Folder window (including the mode line)."
  :type 'integer
  :group 'mh-show)

(defcustom mh-visible-headers nil
  "*Contains a regexp specifying the headers to keep when cleaning.
Only used if `mh-clean-message-header-flag' is non-nil. Setting it overrides
the variable `mh-invisible-headers'."
  :type '(choice (const nil) regexp)
  :group 'mh-show)

(defcustom mhl-formfile nil
  "*Name of format file to be used by mhl to show and print messages.
A value of t means use the default format file.
nil means don't use mhl to format messages when showing; mhl is still used,
with the default format file, to format messages when printing them.
The format used should specify a non-zero value for overflowoffset so
the message continues to conform to RFC 822 and MH-E can parse the headers."
  :type '(choice (const nil) (const t) string)
  :group 'mh-show)
(put 'mhl-formfile 'info-file "mh-e")



;;; Composing messages (:group 'mh-letter)

(defcustom mh-compose-insertion (if (locate-library "mml") 'gnus 'mhn)
  "Use either 'gnus or 'mhn to insert MIME message directives in messages."
  :type '(choice (const :tag "Use Gnus" gnus)
                 (const :tag "Use mhn"  mhn))
  :group 'mh-letter)

(defcustom mh-compose-letter-function nil
  "Invoked when setting up a letter draft.
It is passed three arguments: TO recipients, SUBJECT, and CC recipients."
  :type '(choice (const nil) function)
  :group 'mh-letter)

(defcustom mh-delete-yanked-msg-window-flag nil
  "*Non-nil means delete any window displaying the message.
Controls window display when a message is yanked by \\<mh-letter-mode-map>\\[mh-yank-cur-msg].
If non-nil, yanking the current message into a draft letter deletes any
windows displaying the message."
  :type 'boolean
  :group 'mh-letter)

(defcustom mh-extract-from-attribution-verb "wrote:"
  "*Verb to use for attribution when a message is yanked by \\<mh-letter-mode-map>\\[mh-yank-cur-msg]."
  :type '(choice (const "wrote:")
                 (const "a écrit :")
                 (string :tag "Custom string"))
  :group 'mh-letter)

(defcustom mh-forward-subject-format "%s: %s"
  "*Format to generate the Subject: line contents for a forwarded message.
The two string arguments to the format are the sender of the original
message and the original subject line."
  :type 'string
  :group 'mh-letter)

(defcustom mh-ins-buf-prefix "> "
  "*String to put before each non-blank line of a yanked or inserted message.
\\<mh-letter-mode-map>Used when the message is inserted into an outgoing letter
by \\[mh-insert-letter] or \\[mh-yank-cur-msg]."
  :type 'string
  :group 'mh-letter)

(defcustom mh-insert-x-mailer-flag t
  "*Non-nil means append an X-Mailer field to the header."
  :type 'boolean
  :group 'mh-letter)

(defcustom mh-letter-complete-function 'ispell-complete-word
  "*Function to call when completing outside of fields specific to aliases."
  :type '(choice function (const nil))
  :group 'mh-letter)

(defcustom mh-letter-fill-column 72
  "*Fill column to use in `mh-letter-mode'.
This is usually less than in other text modes because email messages get
quoted by some prefix (sometimes many times) when they are replied to,
and it's best to avoid quoted lines that span more than 80 columns."
  :type 'integer
  :group 'mh-letter)

(defcustom mh-reply-default-reply-to nil
  "*Sets the person or persons to whom a reply will be sent.
If nil, prompt for recipient.  If non-nil, then \\<mh-folder-mode-map>`\\[mh-reply]' will use this
value and it should be one of \"from\", \"to\", \"cc\", or \"all\".
The values \"cc\" and \"all\" do the same thing."
  :type '(choice (const :tag "Prompt" nil)
                 (const "from") (const "to")
                 (const "cc") (const "all"))
  :group 'mh-letter)

(defcustom mh-reply-show-message-flag t
  "*Non-nil means the show buffer is displayed using \\<mh-letter-mode-map>\\[mh-reply].

The setting of this variable determines whether the MH `show-buffer' is
displayed with the current message when using `mh-reply' without a prefix
argument.  Set it to nil if you already include the message automatically
in your draft using
 repl: -filter repl.filter
in your ~/.mh_profile file."
  :type 'boolean
  :group 'mh-letter)

(defcustom mh-signature-file-name "~/.signature"
  "*Name of file containing the user's signature.
Inserted into message by \\<mh-letter-mode-map>\\[mh-insert-signature]."
  :type 'file
  :group 'mh-letter)

(defcustom mh-x-face-file "~/.face"
  "*File containing X-Face or Face header field to insert in outgoing mail.

If the file starts with either of the strings \"X-Face: \", \"Face: \" or
\"X-Image-URL: \" then it is assumed to contain the whole field and is added to
the message header verbatim. Otherwise it is assumed that the file contains the
value of the X-Face header field.

X-Face header fields can be generated using `compface', which can be obtained
from ftp://ftp.cs.indiana.edu/pub/faces/compface/compface.tar.Z. The \"Online
X-Face Convertor\" at http://www.dairiki.org/xface/ is a useful resource for
quick conversion of images into X-Face header fields.

There is a `make-face' script that converts a jpeg image to a Face header
field at http://quimby.gnus.org/circus/face/make-face.

The URL of any image can be used for the X-Image-URL field and no processing
of the image is required.

If nil, or the file does not exist, nothing is added to the message header."
  :type 'file
  :group 'mh-letter)

(defcustom mh-yank-from-start-of-msg 'attribution
  "*Controls which part of a message is yanked by \\<mh-letter-mode-map>\\[mh-yank-cur-msg].
If t, include the entire message, with full headers.  This is historically
here for use with supercite, but is now deprecated in favor of the setting
`supercite' below.

If the symbol `body', then yank the message minus the header.

If the symbol `supercite', include the entire message, with full headers.
This also causes the invocation of `sc-cite-original' without the setting
of `mail-citation-hook', now deprecated practice.

If the symbol `autosupercite', do as for `supercite' automatically when
show buffer matches the message being replied-to.  When this option is used,
the -noformat switch is passed to the repl program to override a -filter or
-format switch.

If the symbol `attribution', then yank the message minus the header and add
a simple attribution line at the top.

If the symbol `autoattrib', do as for `attribution' automatically when show
buffer matches the message being replied-to.  You can make sure this is
always the case by setting `mh-reply-show-message-flag' to t (which is the
default) and optionally `mh-delete-yanked-msg-window-flag' to t as well such
that the show window is never displayed.  When the `autoattrib' option is
used, the -noformat switch is passed to the repl program to override a
-filter or -format switch.

If nil, yank only the portion of the message following the point.

If the show buffer has a region, this variable is ignored unless its value is
one of `attribution' or `autoattrib' in which case the attribution is added
to the yanked region."
  :type '(choice (const :tag "Below point" nil)
                 (const :tag "Without header" body)
                 (const :tag "Invoke supercite" supercite)
                 (const :tag "Invoke supercite, automatically" autosupercite)
                 (const :tag "Without header, with attribution" attribution)
                 (const :tag "Without header, with attribution, automatically"
                        autoattrib)
                 (const :tag "Entire message with headers" t))
  :group 'mh-letter)



;;; Alias handling (:group 'mh-alias)

(defcustom mh-alias-completion-ignore-case-flag t
  "*Non-nil means don't consider case significant in MH alias completion.
This is the default in plain MH, so it is the default here as well. It
can be useful to set this to t if, for example, you use lowercase
aliases for people and uppercase for mailing lists."
  :type 'boolean
  :group 'mh-alias)

(defcustom mh-alias-expand-aliases-flag nil
  "*Non-nil means to expand aliases entered in the minibuffer.
In other words, aliases entered in the minibuffer will be expanded to the full
address in the message draft. By default, this expansion is not performed."
  :type 'boolean
  :group 'mh-alias)

(defcustom mh-alias-flash-on-comma t
  "*Specify whether to flash or warn on translation.
When a [comma] is pressed while entering aliases or addresses, setting this
variable to the following values has the listed effects:
t   Flash alias translation but don't warn if there is no translation.
1   Flash alias translation and warn if there is no translation.
nil Do not flash alias translation nor warn if there is no translation."
  :type '(choice (const :tag "Flash but don't warn if no translation" t)
		 (const :tag "Flash and warn if no translation" 1)
		 (const :tag "Don't flash nor warn if no translation" nil))
  :group 'mh-alias)

(defcustom mh-alias-insert-file nil
  "*Filename to use to store new MH-E aliases.
This variable can also be a list of filenames, in which case MH-E will prompt
for one of them. If nil, the default, then MH-E will use the first file found
in the \"AliasFile\" component of the MH profile."
  :type '(choice (const :tag "Use AliasFile MH profile component" nil)
                 (file :tag "Alias file")
                 (repeat :tag "List of alias files" file))
  :group 'mh-alias)

(defcustom mh-alias-insertion-location 'sorted
  "Specifies where new aliases are entered in alias files.
Options are sorted alphabetically, at the top of the file or at the bottom."
  :type '(choice (const :tag "Sorted alphabetically" sorted)
                 (const :tag "At the top of file" top)
                 (const :tag "At the bottom of file" bottom))
  :group 'mh-alias)

(defcustom mh-alias-local-users t
  "*If t, local users are completed in MH-E To: and Cc: prompts.

Users with a userid greater than some magic number (usually 200) are available
for completion.

If you set this variable to a string, it will be executed to generate a
password file. A value of \"ypcat passwd\" is helpful if NIS is in use."
  :type '(choice (boolean) (string))
  :group 'mh-alias)

(defcustom mh-alias-system-aliases
  '("/etc/nmh/MailAliases" "/usr/lib/mh/MailAliases" "/etc/passwd")
  "*A list of system files from which to cull aliases.
If these files are modified, they are automatically reread. This list need
include only system aliases and the passwd file, since personal alias files
listed in your \"AliasFile\" MH profile component are automatically included.
You can update the alias list manually using \\[mh-alias-reload]."
  :type '(choice (file) (repeat file))
  :group 'mh-alias)



;;; Multiple personalities (:group 'mh-identity)

(defvar mh-identity-list ())

(defcustom mh-auto-fields-list nil
  "Alist of addresses for which header lines are automatically inserted.
Each element has the form (REGEXP ((KEYWORD VALUE) (KEYWORD VALUE)).
When the REGEXP appears in the To or cc fields of a message, the corresponding
KEYWORD header field is insert with its VALUE in the message header.

There is one special case for KEYWORD, that of \"identity\", which means to
insert that identity using `mh-insert-identity'.

The common KEYWORD cases of \"Mail-Followup-To\" and \"fcc\" are also
prompted for in the customization interface."
  :type `(repeat
          (list :tag ""
                (string :tag "Regular expression to match")
                (repeat :tag "At least one pair from below"
                        (choice
                         (cons :tag "Identity entry"
                          (const "identity")
                          ,(append
                            '(radio)
                            (mapcar (function (lambda (arg) `(const ,arg)))
                                    (mapcar 'car mh-identity-list))))
                         (cons :tag "fcc field"
                               (const "fcc")
                               (string :tag "Value"))
                         (cons :tag "Mail-Followup-To field"
                               (const "Mail-Followup-To")
                               (string :tag "Value"))
                         (cons :tag "Other field and value pair"
                                 (string :tag "Field")
                                 (string :tag "Value"))))))
  :group 'mh-identity)

(defcustom mh-identity-default nil
  "Default identity to use when `mh-letter-mode' is called."
  ;; Dynamically render :type corresponding to `mh-identity-list' entries,
  ;; e.g.:
  ;;  :type '(radio (const :tag "none" nil)
  ;;                (const "home")
  ;;                (const "work"))
  :type (append
         '(radio)
         (cons '(const :tag "None" nil)
               (mapcar (function (lambda (arg) `(const ,arg)))
                       (mapcar 'car mh-identity-list))))
  :group 'mh-identity)

(defcustom mh-identity-list nil
  "*List holding MH-E identity.
Omit the colon and trailing space from the field names.
The keyword name \"none\" is reversed for internal use.
Use the keyname name \"signature\" to specify either a signature file or a
function to call to insert a signature at point.

Providing an empty Value (\"\") will cause the field to be deleted.

Example entries using the customize interface:
   Keyword name: work
            From
            Value: John Doe <john@work.com>
            Organization
            Value: Acme Inc.
   Keyword name: home
            From
            Value: John Doe <johndoe@home.net>
            Organization
            Value:

This would produce the equivalent of:
 (setq mh-identity-list
      '((\"work\"
         ((\"From\" . \"John Doe <john@work.com>\")
          (\"Organization\" . \"Acme Inc.\")))
        (\"home\"
         ((\"From\" . \"John Doe <johndoe@home.net>\")
          (\"Organization\" . \"\")))))"
  :type '(repeat (list :tag ""
                       (string :tag "Keyword name")
                       (repeat :tag "At least one pair from below"
                               (choice (cons :tag "From field"
                                             (const "From")
                                             (string :tag "Value"))
                                       (cons :tag "Organization field"
                                             (const "Organization")
                                             (string :tag "Value"))
                                       (cons :tag "Signature"
                                             (const "signature")
                                             (choice (file) (function)))
                                       (cons :tag "Other field & value pair"
                                             (string :tag "Field")
                                             (string :tag "Value"))))))
  :set 'mh-identity-list-set
  :group 'mh-identity)



;;; Hooks (:group 'mh-hooks + group where hook defined)

;;; These are alphabetized. All hooks should be placed in the 'mh-hook group;
;;; in addition, add the group in which the hook is defined in the manual (or,
;;; if it is new, where it would be defined).

(defcustom mh-before-quit-hook nil
  "Invoked by \\<mh-folder-mode-map>`\\[mh-quit]' before quitting MH-E.
See also `mh-quit-hook'."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-folder)

(defcustom mh-before-send-letter-hook nil
  "Invoked at the beginning of the \\<mh-letter-mode-map>\\[mh-send-letter] command."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-letter)

(defcustom mh-delete-msg-hook nil
  "Invoked after marking each message for deletion."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-folder)

(defcustom mh-edit-mhn-hook nil
  "Invoked on the formatted letter by \\<mh-letter-mode-map>\\[mh-edit-mhn]."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-letter)

(defcustom mh-find-path-hook nil
  "Invoked by `mh-find-path' after reading the user's MH profile."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-folder)

(defcustom mh-folder-mode-hook nil
  "Invoked in `mh-folder-mode' on a new folder."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-folder)

(defcustom mh-folder-updated-hook nil
  "Invoked when the folder actions (such as moves and deletes) are performed.
Variables that are useful in this hook include `mh-delete-list' and
`mh-refile-list' which can be used to see which changes are being made to
current folder, `mh-current-folder'."
  :type 'hook
  :group 'mh-hooks)

(defcustom mh-inc-folder-hook nil
  "Invoked by \\<mh-folder-mode-map>`\\[mh-inc-folder]' after incorporating mail into a folder."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-folder)

(defcustom mh-letter-insert-signature-hook nil
  "Invoked at the beginning of the \\<mh-letter-mode-map>\\[mh-insert-signature] command.
Can be used to determine which signature file to use based on message content.
On return, if `mh-signature-file-name' is non-nil that file will be inserted at
the current point in the buffer."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-letter)

(defcustom mh-letter-mode-hook nil
  "Invoked in `mh-letter-mode' on a new letter."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-letter)

(defcustom mh-pick-mode-hook nil
  "Invoked upon entry to `mh-pick-mode'."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-folder)

(defcustom mh-quit-hook nil
  "Invoked after \\<mh-folder-mode-map>`\\[mh-quit]' quits MH-E.
See also `mh-before-quit-hook'."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-folder)

(defcustom mh-refile-msg-hook nil
  "Invoked after marking each message for refiling."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-folder)

(defcustom mh-show-hook nil
  "Invoked after \\<mh-folder-mode-map>`\\[mh-show]' shows a message."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-show-mode-hook nil
  "Invoked upon entry to `mh-show-mode'."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-show)

(defcustom mh-unseen-updated-hook nil
  "Invoked after the unseen sequence has been updated.
The variable `mh-seen-list' can be used to obtain the list of messages which
will be removed from the unseen sequence."
  :type 'hook
  :group 'mh-hooks
  :group 'mh-folder)



;;; Faces

;;; Faces used in speedbar (:group mh-speed-faces)

(defface mh-speedbar-folder-face
  '((((class color) (background light))
     (:foreground "blue4"))
    (((class color) (background dark))
     (:foreground "light blue")))
  "Face used for folders in the speedbar buffer."
  :group 'mh-speed-faces)

(defface mh-speedbar-selected-folder-face
  '((((class color) (background light))
     (:foreground "red" :underline t))
    (((class color) (background dark))
     (:foreground "red" :underline t))
    (t (:underline t)))
  "Face used for the current folder."
  :group 'mh-speed-faces)

(defface mh-speedbar-folder-with-unseen-messages-face
  '((t (:inherit mh-speedbar-folder-face :bold t)))
  "Face used for folders in the speedbar buffer which have unread messages."
  :group 'mh-speed-faces)

(defface mh-speedbar-selected-folder-with-unseen-messages-face
  '((t (:inherit mh-speedbar-selected-folder-face :bold t)))
  "Face used for the current folder when it has unread messages."
  :group 'mh-speed-faces)



;;; Faces used in scan listing (:group mh-folder-faces)

(defvar mh-folder-body-face 'mh-folder-body-face
  "Face for highlighting body text in MH-Folder buffers.")
(defface mh-folder-body-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face for highlighting body text in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-cur-msg-face 'mh-folder-cur-msg-face
  "Face for the current message line in MH-Folder buffers.")
(defface mh-folder-cur-msg-face
  '((((type tty pc) (class color))
     (:background "LightGreen"))
    (((class color) (background light))
     (:background "LightGreen")         ;Use this for solid background colour
     ;;  (:underline t)                 ;Use this for underlining
     )
    (((class color) (background dark))
     (:background "DarkOliveGreen4"))
    (t (:underline t)))
  "Face for the current message line in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-cur-msg-number-face 'mh-folder-cur-msg-number-face
  "Face for highlighting the current message in MH-Folder buffers.")
(defface mh-folder-cur-msg-number-face
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:bold t)))
  "Face for highlighting the current message in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-date-face 'mh-folder-date-face
  "Face for highlighting the date in MH-Folder buffers.")
(defface mh-folder-date-face
  '((((class color) (background light))
     (:foreground "snow4"))
    (((class color) (background dark))
     (:foreground "snow3"))
    (t
     (:bold t)))
  "Face for highlighting the date in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-followup-face 'mh-folder-followup-face
  "Face for highlighting Re: (followup) subject text in MH-Folder buffers.")
(defface mh-folder-followup-face
  '((((class color) (background light))
     (:foreground "blue3"))
    (((class color) (background dark))
     (:foreground "LightGoldenRod"))
    (t
     (:bold t)))
  "Face for highlighting Re: (followup) subject text in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-msg-number-face 'mh-folder-msg-number-face
  "Face for highlighting the message number in MH-Folder buffers.")
(defface mh-folder-msg-number-face
  '((((class color) (background light))
     (:foreground "snow4"))
    (((class color) (background dark))
     (:foreground "snow3"))
    (t
     (:bold t)))
  "Face for highlighting the message number in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-deleted-face 'mh-folder-deleted-face
  "Face for highlighting deleted messages in MH-Folder buffers.")
(copy-face 'mh-folder-msg-number-face 'mh-folder-deleted-face)

(defvar mh-folder-refiled-face 'mh-folder-refiled-face
  "Face for highlighting refiled messages in MH-Folder buffers.")
(defface mh-folder-refiled-face
  '((((type tty) (class color)) (:foreground "yellow" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:bold t :italic t)))
  "Face for highlighting refiled messages in MH-Folder buffers."
  :group 'mh-folder-faces)

(defvar mh-folder-subject-face 'mh-folder-subject-face
  "Face for highlighting subject text in MH-Folder buffers.")
(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces "^mh-folder"))
(defface mh-folder-subject-face
  '((((class color) (background light))
     (:foreground "blue4"))
    (((class color) (background dark))
     (:foreground "yellow"))
    (t
     (:bold t)))
  "Face for highlighting subject text in MH-Folder buffers."
  :group 'mh-folder-faces)

(defface mh-folder-tick-face
  '((((class color) (background dark)) (:background "#dddf7e"))
    (((class color) (background light)) (:background "#dddf7e"))
    (t (:underline t)))
  "Face used to show ticked messages."
  :group 'mh-folder-faces)

(defvar mh-folder-address-face 'mh-folder-address-face
  "Face for highlighting the address in MH-Folder buffers.")
(copy-face 'mh-folder-subject-face 'mh-folder-address-face)

(defvar mh-folder-scan-format-face 'mh-folder-scan-format-face
  "Face for highlighting `mh-scan-format-regexp' matches in MH-Folder buffers.")
(copy-face 'mh-folder-followup-face 'mh-folder-scan-format-face)

(defvar mh-folder-to-face 'mh-folder-to-face
  "Face for highlighting the To: string in MH-Folder buffers.")
(defface mh-folder-to-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face for highlighting the To: string in MH-Folder buffers."
  :group 'mh-folder-faces)



;;; Faces used in message display (:group mh-show-faces)

(defvar mh-show-cc-face 'mh-show-cc-face
  "Face for highlighting cc header fields.")
(defface mh-show-cc-face
  '((((type tty) (class color)) (:foreground "yellow" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:bold t :italic t)))
  "Face for highlighting cc header fields."
  :group 'mh-show-faces)

(defvar mh-show-date-face 'mh-show-date-face
  "Face for highlighting the Date header field.")
(defface mh-show-date-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "Gray90" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    (t (:bold t :underline t)))
  "Face for highlighting the Date header field."
  :group 'mh-show-faces)

(defvar mh-show-header-face 'mh-show-header-face
  "Face used to deemphasize unspecified header fields.")
(defface mh-show-header-face
  '((((type tty) (class color)) (:foreground "green"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face used to deemphasize unspecified header fields."
  :group 'mh-show-faces)

(defvar mh-show-to-face 'mh-show-to-face
  "Face for highlighting the To: header field.")
(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces "^mh-show"))
(defface mh-show-to-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :underline t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :underline t))
    (((class color) (background light)) (:foreground "SaddleBrown"))
    (((class color) (background dark))  (:foreground "burlywood"))
    (t (:underline t)))
  "Face for highlighting the To: header field."
  :group 'mh-show-faces)

(defvar mh-show-from-face 'mh-show-from-face
  "Face for highlighting the From: header field.")
(defface mh-show-from-face
  '((((class color) (background light))
     (:foreground "red3"))
    (((class color) (background dark))
     (:foreground "cyan"))
    (t
     (:bold t)))
  "Face for highlighting the From: header field."
  :group 'mh-show-faces)

(defface mh-show-xface-face
  '((t (:foreground "black" :background "white")))
  "Face for displaying the X-Face image.
The background and foreground is used in the image."
  :group 'mh-show-faces)

(defvar mh-show-subject-face 'mh-show-subject-face
  "Face for highlighting the Subject header field.")
(copy-face 'mh-folder-subject-face 'mh-show-subject-face)



;;; Faces used in indexed searches (:group mh-index-faces)

(defvar mh-index-folder-face 'mh-index-folder-face
  "Face for highlighting folders in MH-Index buffers.")
(defface mh-index-folder-face
  '((((class color) (background light))
     (:foreground "dark green" :bold t))
    (((class color) (background dark))
     (:foreground "indian red" :bold t))
    (t
     (:bold t)))
  "Face for highlighting folders in MH-Index buffers."
  :group 'mh-index-faces)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 778d2a20-82e2-4276-be9d-309386776a68
;;; mh-customize.el ends here
