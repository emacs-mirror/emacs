;;; gnus.el --- a newsreader for GNU Emacs
;; Copyright (C) 1987,88,89,90,93,94,95,96,97,98 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval '(run-hooks 'gnus-load-hook))

(eval-when-compile (require 'cl))

(eval-when-compile (require 'cl))

(require 'custom)
(eval-and-compile
  (if (< emacs-major-version 20)
      (require 'gnus-load)))
(require 'message)

(defgroup gnus nil
  "The coffee-brewing, all singing, all dancing, kitchen sink newsreader."
  :group 'news
  :group 'mail)

(defgroup gnus-cache nil
  "Cache interface."
  :group 'gnus)

(defgroup gnus-start nil
  "Starting your favorite newsreader."
  :group 'gnus)

(defgroup gnus-start-server nil
  "Server options at startup."
  :group 'gnus-start)

;; These belong to gnus-group.el.
(defgroup gnus-group nil
  "Group buffers."
  :link '(custom-manual "(gnus)The Group Buffer")
  :group 'gnus)

(defgroup gnus-group-foreign nil
  "Foreign groups."
  :link '(custom-manual "(gnus)Foreign Groups")
  :group 'gnus-group)

(defgroup gnus-group-new nil
  "Automatic subscription of new groups."
  :group 'gnus-group)

(defgroup gnus-group-levels nil
  "Group levels."
  :link '(custom-manual "(gnus)Group Levels")
  :group 'gnus-group)

(defgroup gnus-group-select nil
  "Selecting a Group."
  :link '(custom-manual "(gnus)Selecting a Group")
  :group 'gnus-group)

(defgroup gnus-group-listing nil
  "Showing slices of the group list."
  :link '(custom-manual "(gnus)Listing Groups")
  :group 'gnus-group)

(defgroup gnus-group-visual nil
  "Sorting the group buffer."
  :link '(custom-manual "(gnus)Group Buffer Format")
  :group 'gnus-group
  :group 'gnus-visual)

(defgroup gnus-group-various nil
  "Various group options."
  :link '(custom-manual "(gnus)Scanning New Messages")
  :group 'gnus-group)

;; These belong to gnus-sum.el.
(defgroup gnus-summary nil
  "Summary buffers."
  :link '(custom-manual "(gnus)The Summary Buffer")
  :group 'gnus)

(defgroup gnus-summary-exit nil
  "Leaving summary buffers."
  :link '(custom-manual "(gnus)Exiting the Summary Buffer")
  :group 'gnus-summary)

(defgroup gnus-summary-marks nil
  "Marks used in summary buffers."
  :link '(custom-manual "(gnus)Marking Articles")
  :group 'gnus-summary)

(defgroup gnus-thread nil
  "Ordering articles according to replies."
  :link '(custom-manual "(gnus)Threading")
  :group 'gnus-summary)

(defgroup gnus-summary-format nil
  "Formatting of the summary buffer."
  :link '(custom-manual "(gnus)Summary Buffer Format")
  :group 'gnus-summary)

(defgroup gnus-summary-choose nil
  "Choosing Articles."
  :link '(custom-manual "(gnus)Choosing Articles")
  :group 'gnus-summary)

(defgroup gnus-summary-maneuvering nil
  "Summary movement commands."
  :link '(custom-manual "(gnus)Summary Maneuvering")
  :group 'gnus-summary)

(defgroup gnus-summary-mail nil
  "Mail group commands."
  :link '(custom-manual "(gnus)Mail Group Commands")
  :group 'gnus-summary)

(defgroup gnus-summary-sort nil
  "Sorting the summary buffer."
  :link '(custom-manual "(gnus)Sorting")
  :group 'gnus-summary)

(defgroup gnus-summary-visual nil
  "Highlighting and menus in the summary buffer."
  :link '(custom-manual "(gnus)Summary Highlighting")
  :group 'gnus-visual
  :group 'gnus-summary)

(defgroup gnus-summary-various nil
  "Various summary buffer options."
  :link '(custom-manual "(gnus)Various Summary Stuff")
  :group 'gnus-summary)

(defgroup gnus-summary-pick nil
  "Pick mode in the summary buffer."
  :link '(custom-manual "(gnus)Pick and Read")
  :prefix "gnus-pick-"
  :group 'gnus-summary)

(defgroup gnus-summary-tree nil
  "Tree display of threads in the summary buffer."
  :link '(custom-manual "(gnus)Tree Display")
  :prefix "gnus-tree-"
  :group 'gnus-summary)

;; Belongs to gnus-uu.el
(defgroup gnus-extract-view nil
  "Viewing extracted files."
  :link '(custom-manual "(gnus)Viewing Files")
  :group 'gnus-extract)

;; Belongs to gnus-score.el
(defgroup gnus-score nil
  "Score and kill file handling."
  :group 'gnus)

(defgroup gnus-score-kill nil
  "Kill files."
  :group 'gnus-score)

(defgroup gnus-score-adapt nil
  "Adaptive score files."
  :group 'gnus-score)

(defgroup gnus-score-default nil
  "Default values for score files."
  :group 'gnus-score)

(defgroup gnus-score-expire nil
  "Expiring score rules."
  :group 'gnus-score)

(defgroup gnus-score-decay nil
  "Decaying score rules."
  :group 'gnus-score)

(defgroup gnus-score-files nil
  "Score and kill file names."
  :group 'gnus-score
  :group 'gnus-files)

(defgroup gnus-score-various nil
  "Various scoring and killing options."
  :group 'gnus-score)

;; Other
(defgroup gnus-visual nil
  "Options controling the visual fluff."
  :group 'gnus
  :group 'faces)

(defgroup gnus-agent nil
  "Offline support for Gnus."
  :group 'gnus)

(defgroup gnus-files nil
  "Files used by Gnus."
  :group 'gnus)

(defgroup gnus-dribble-file nil
  "Auto save file."
  :link '(custom-manual "(gnus)Auto Save")
  :group 'gnus-files)

(defgroup gnus-newsrc nil
  "Storing Gnus state."
  :group 'gnus-files)

(defgroup gnus-server nil
  "Options related to newsservers and other servers used by Gnus."
  :group 'gnus)

(defgroup gnus-message '((message custom-group))
  "Composing replies and followups in Gnus."
  :group 'gnus)

(defgroup gnus-meta nil
  "Meta variables controling major portions of Gnus.
In general, modifying these variables does not take affect until Gnus
is restarted, and sometimes reloaded."
  :group 'gnus)

(defgroup gnus-various nil
  "Other Gnus options."
  :link '(custom-manual "(gnus)Various Various")
  :group 'gnus)

(defgroup gnus-exit nil
  "Exiting gnus."
  :link '(custom-manual "(gnus)Exiting Gnus")
  :group 'gnus)

(defconst gnus-version-number "5.7"
  "Version number for this version of Gnus.")

(defconst gnus-version (format "Gnus v%s" gnus-version-number)
  "Version string for this version of Gnus.")

(defcustom gnus-inhibit-startup-message nil
  "If non-nil, the startup message will not be displayed.
This variable is used before `.gnus.el' is loaded, so it should
be set in `.emacs' instead."
  :group 'gnus-start
  :type 'boolean)

(defcustom gnus-play-startup-jingle nil
  "If non-nil, play the Gnus jingle at startup."
  :group 'gnus-start
  :type 'boolean)

;;; Kludges to help the transition from the old `custom.el'.

(unless (featurep 'gnus-xmas)
  (defalias 'gnus-make-overlay 'make-overlay)
  (defalias 'gnus-delete-overlay 'delete-overlay)
  (defalias 'gnus-overlay-put 'overlay-put)
  (defalias 'gnus-move-overlay 'move-overlay)
  (defalias 'gnus-overlay-end 'overlay-end)
  (defalias 'gnus-extent-detached-p 'ignore)
  (defalias 'gnus-extent-start-open 'ignore)
  (defalias 'gnus-set-text-properties 'set-text-properties)
  (defalias 'gnus-group-remove-excess-properties 'ignore)
  (defalias 'gnus-appt-select-lowest-window 'appt-select-lowest-window)
  (defalias 'gnus-mail-strip-quoted-names 'mail-strip-quoted-names)
  (defalias 'gnus-character-to-event 'identity)
  (defalias 'gnus-add-text-properties 'add-text-properties)
  (defalias 'gnus-put-text-property 'put-text-property)
  (defalias 'gnus-mode-line-buffer-identification 'identity)
  (defalias 'gnus-characterp 'numberp)
  (defalias 'gnus-deactivate-mark 'deactivate-mark)
  (defalias 'gnus-window-edges 'window-edges)
  (defalias 'gnus-key-press-event-p 'numberp))

;; We define these group faces here to avoid the display
;; update forced when creating new faces.

(defface gnus-group-news-1-face
  '((((class color)
      (background dark))
     (:foreground "PaleTurquoise" :bold t))
    (((class color)
      (background light))
     (:foreground "ForestGreen" :bold t))
    (t
     ()))
  "Level 1 newsgroup face.")

(defface gnus-group-news-1-empty-face
  '((((class color)
      (background dark))
     (:foreground "PaleTurquoise"))
    (((class color)
      (background light))
     (:foreground "ForestGreen"))
    (t
     ()))
  "Level 1 empty newsgroup face.")

(defface gnus-group-news-2-face
  '((((class color)
      (background dark))
     (:foreground "turquoise" :bold t))
    (((class color)
      (background light))
     (:foreground "CadetBlue4" :bold t))
    (t
     ()))
  "Level 2 newsgroup face.")

(defface gnus-group-news-2-empty-face
  '((((class color)
      (background dark))
     (:foreground "turquoise"))
    (((class color)
      (background light))
     (:foreground "CadetBlue4"))
    (t
     ()))
  "Level 2 empty newsgroup face.")

(defface gnus-group-news-3-face
  '((((class color)
      (background dark))
     (:bold t))
    (((class color)
      (background light))
     (:bold t))
    (t
     ()))
  "Level 3 newsgroup face.")

(defface gnus-group-news-3-empty-face
  '((((class color)
      (background dark))
     ())
    (((class color)
      (background light))
     ())
    (t
     ()))
  "Level 3 empty newsgroup face.")

(defface gnus-group-news-low-face
  '((((class color)
      (background dark))
     (:foreground "DarkTurquoise" :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen" :bold t))
    (t
     ()))
  "Low level newsgroup face.")

(defface gnus-group-news-low-empty-face
  '((((class color)
      (background dark))
     (:foreground "DarkTurquoise"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     ()))
  "Low level empty newsgroup face.")

(defface gnus-group-mail-1-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine1" :bold t))
    (((class color)
      (background light))
     (:foreground "DeepPink3" :bold t))
    (t
     (:bold t)))
  "Level 1 mailgroup face.")

(defface gnus-group-mail-1-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine1"))
    (((class color)
      (background light))
     (:foreground "DeepPink3"))
    (t
     (:italic t :bold t)))
  "Level 1 empty mailgroup face.")

(defface gnus-group-mail-2-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine2" :bold t))
    (((class color)
      (background light))
     (:foreground "HotPink3" :bold t))
    (t
     (:bold t)))
  "Level 2 mailgroup face.")

(defface gnus-group-mail-2-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine2"))
    (((class color)
      (background light))
     (:foreground "HotPink3"))
    (t
     (:bold t)))
  "Level 2 empty mailgroup face.")

(defface gnus-group-mail-3-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine3" :bold t))
    (((class color)
      (background light))
     (:foreground "magenta4" :bold t))
    (t
     (:bold t)))
  "Level 3 mailgroup face.")

(defface gnus-group-mail-3-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine3"))
    (((class color)
      (background light))
     (:foreground "magenta4"))
    (t
     ()))
  "Level 3 empty mailgroup face.")

(defface gnus-group-mail-low-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine4" :bold t))
    (((class color)
      (background light))
     (:foreground "DeepPink4" :bold t))
    (t
     (:bold t)))
  "Low level mailgroup face.")

(defface gnus-group-mail-low-empty-face
  '((((class color)
      (background dark))
     (:foreground "aquamarine4"))
    (((class color)
      (background light))
     (:foreground "DeepPink4"))
    (t
     (:bold t)))
  "Low level empty mailgroup face.")

;; Summary mode faces.

(defface gnus-summary-selected-face '((t
				       (:underline t)))
  "Face used for selected articles.")

(defface gnus-summary-cancelled-face
  '((((class color))
     (:foreground "yellow" :background "black")))
  "Face used for cancelled articles.")

(defface gnus-summary-high-ticked-face
  '((((class color)
      (background dark))
     (:foreground "pink" :bold t))
    (((class color)
      (background light))
     (:foreground "firebrick" :bold t))
    (t
     (:bold t)))
  "Face used for high interest ticked articles.")

(defface gnus-summary-low-ticked-face
  '((((class color)
      (background dark))
     (:foreground "pink" :italic t))
    (((class color)
      (background light))
     (:foreground "firebrick" :italic t))
    (t
     (:italic t)))
  "Face used for low interest ticked articles.")

(defface gnus-summary-normal-ticked-face
  '((((class color)
      (background dark))
     (:foreground "pink"))
    (((class color)
      (background light))
     (:foreground "firebrick"))
    (t
     ()))
  "Face used for normal interest ticked articles.")

(defface gnus-summary-high-ancient-face
  '((((class color)
      (background dark))
     (:foreground "SkyBlue" :bold t))
    (((class color)
      (background light))
     (:foreground "RoyalBlue" :bold t))
    (t
     (:bold t)))
  "Face used for high interest ancient articles.")

(defface gnus-summary-low-ancient-face
  '((((class color)
      (background dark))
     (:foreground "SkyBlue" :italic t))
    (((class color)
      (background light))
     (:foreground "RoyalBlue" :italic t))
    (t
     (:italic t)))
  "Face used for low interest ancient articles.")

(defface gnus-summary-normal-ancient-face
  '((((class color)
      (background dark))
     (:foreground "SkyBlue"))
    (((class color)
      (background light))
     (:foreground "RoyalBlue"))
    (t
     ()))
  "Face used for normal interest ancient articles.")

(defface gnus-summary-high-unread-face
  '((t
     (:bold t)))
  "Face used for high interest unread articles.")

(defface gnus-summary-low-unread-face
  '((t
     (:italic t)))
  "Face used for low interest unread articles.")

(defface gnus-summary-normal-unread-face
  '((t
     ()))
  "Face used for normal interest unread articles.")

(defface gnus-summary-high-read-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"
		  :bold t))
    (((class color)
      (background light))
     (:foreground "DarkGreen"
		  :bold t))
    (t
     (:bold t)))
  "Face used for high interest read articles.")

(defface gnus-summary-low-read-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"
		  :italic t))
    (((class color)
      (background light))
     (:foreground "DarkGreen"
		  :italic t))
    (t
     (:italic t)))
  "Face used for low interest read articles.")

(defface gnus-summary-normal-read-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     ()))
  "Face used for normal interest read articles.")


;;;
;;; Gnus buffers
;;;

(defvar gnus-buffers nil)

(defun gnus-get-buffer-create (name)
  "Do the same as `get-buffer-create', but store the created buffer."
  (or (get-buffer name)
      (car (push (get-buffer-create name) gnus-buffers))))

(defun gnus-add-buffer ()
  "Add the current buffer to the list of Gnus buffers."
  (push (current-buffer) gnus-buffers))

(defun gnus-buffers ()
  "Return a list of live Gnus buffers."
  (while (and gnus-buffers
	      (not (buffer-name (car gnus-buffers))))
    (pop gnus-buffers))
  (let ((buffers gnus-buffers))
    (while (cdr buffers)
      (if (buffer-name (cadr buffers))
	  (pop buffers)
	(setcdr buffers (cddr buffers)))))
  gnus-buffers)

;;; Splash screen.

(defvar gnus-group-buffer "*Group*")

(eval-and-compile
  (autoload 'gnus-play-jingle "gnus-audio"))

(defface gnus-splash-face
  '((((class color)
      (background dark))
     (:foreground "ForestGreen"))
    (((class color)
      (background light))
     (:foreground "ForestGreen"))
    (t
     ()))
  "Level 1 newsgroup face.")

(defun gnus-splash ()
  (save-excursion
    (switch-to-buffer (gnus-get-buffer-create gnus-group-buffer))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (unless gnus-inhibit-startup-message
	(gnus-group-startup-message)
	(sit-for 0)
	(when gnus-play-startup-jingle
	  (gnus-play-jingle))))))

(defun gnus-indent-rigidly (start end arg)
  "Indent rigidly using only spaces and no tabs."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((tab-width 8))
	(indent-rigidly start end arg)
	;; We translate tabs into spaces -- not everybody uses
	;; an 8-character tab.
	(goto-char (point-min))
	(while (search-forward "\t" nil t)
	  (replace-match "        " t t))))))

(defvar gnus-simple-splash nil)

(defun gnus-group-startup-message (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (erase-buffer)
  (insert
   (format "              %s
          _    ___ _             _
          _ ___ __ ___  __    _ ___
          __   _     ___    __  ___
              _           ___     _
             _  _ __             _
             ___   __            _
                   __           _
                    _      _   _
                   _      _    _
                      _  _    _
                  __  ___
                 _   _ _     _
                _   _
              _    _
             _    _
            _
          __

"
           ""))
  ;; And then hack it.
  (gnus-indent-rigidly (point-min) (point-max)
		       (/ (max (- (window-width) (or x 46)) 0) 2))
  (goto-char (point-min))
  (forward-line 1)
  (let* ((pheight (count-lines (point-min) (point-max)))
	 (wheight (window-height))
	 (rest (- wheight pheight)))
    (insert (make-string (max 0 (* 2 (/ rest 3))) ?\n)))
  ;; Fontify some.
  (put-text-property (point-min) (point-max) 'face 'gnus-splash-face)
  (goto-char (point-min))
  (setq mode-line-buffer-identification (concat " " gnus-version))
  (setq gnus-simple-splash t)
  (set-buffer-modified-p t))

(eval-when (load)
  (let ((command (format "%s" this-command)))
    (if (and (string-match "gnus" command)
	     (not (string-match "gnus-other-frame" command)))
	(gnus-splash)
      (gnus-get-buffer-create gnus-group-buffer))))

;;; Do the rest.

(require 'custom)
(require 'gnus-util)
(require 'nnheader)

(defcustom gnus-home-directory "~/"
  "Directory variable that specifies the \"home\" directory.
All other Gnus path variables are initialized from this variable."
  :group 'gnus-files
  :type 'directory)

(defcustom gnus-directory (or (getenv "SAVEDIR")
			      (nnheader-concat gnus-home-directory "News/"))
  "*Directory variable from which all other Gnus file variables are derived.

Note that Gnus is mostly loaded when the `.gnus.el' file is read.
This means that other directory variables that are initialized from
this variable won't be set properly if you set this variable in `.gnus.el'.
Set this variable in `.emacs' instead."
  :group 'gnus-files
  :type 'directory)

(defcustom gnus-default-directory nil
  "*Default directory for all Gnus buffers."
  :group 'gnus-files
  :type '(choice (const :tag "current" nil)
		 directory))

;; Site dependent variables.  These variables should be defined in
;; paths.el.

(defvar gnus-default-nntp-server nil
  "Specify a default NNTP server.
This variable should be defined in paths.el, and should never be set
by the user.
If you want to change servers, you should use `gnus-select-method'.
See the documentation to that variable.")

;; Don't touch this variable.
(defvar gnus-nntp-service "nntp"
  "NNTP service name (\"nntp\" or 119).
This is an obsolete variable, which is scarcely used.  If you use an
nntp server for your newsgroup and want to change the port number
used to 899, you would say something along these lines:

 (setq gnus-select-method '(nntp \"my.nntp.server\" (nntp-port-number 899)))")

(defcustom gnus-nntpserver-file "/etc/nntpserver"
  "A file with only the name of the nntp server in it."
  :group 'gnus-files
  :group 'gnus-server
  :type 'file)

;; This function is used to check both the environment variable
;; NNTPSERVER and the /etc/nntpserver file to see whether one can find
;; an nntp server name default.
(defun gnus-getenv-nntpserver ()
  (or (getenv "NNTPSERVER")
      (and (file-readable-p gnus-nntpserver-file)
	   (save-excursion
	     (set-buffer (gnus-get-buffer-create " *gnus nntp*"))
	     (buffer-disable-undo (current-buffer))
	     (insert-file-contents gnus-nntpserver-file)
	     (let ((name (buffer-string)))
	       (prog1
		   (if (string-match "^[ \t\n]*$" name)
		       nil
		     name)
		 (kill-buffer (current-buffer))))))))

(defcustom gnus-select-method
  (condition-case nil
    (nconc
     (list 'nntp (or (condition-case nil
			 (gnus-getenv-nntpserver)
		       (error nil))
		     (when (and gnus-default-nntp-server
				(not (string= gnus-default-nntp-server "")))
		       gnus-default-nntp-server)
		     "news"))
     (if (or (null gnus-nntp-service)
	     (equal gnus-nntp-service "nntp"))
	 nil
       (list gnus-nntp-service)))
    (error nil))
  "*Default method for selecting a newsgroup.
This variable should be a list, where the first element is how the
news is to be fetched, the second is the address.

For instance, if you want to get your news via NNTP from
\"flab.flab.edu\", you could say:

\(setq gnus-select-method '(nntp \"flab.flab.edu\"))

If you want to use your local spool, say:

\(setq gnus-select-method (list 'nnspool (system-name)))

If you use this variable, you must set `gnus-nntp-server' to nil.

There is a lot more to know about select methods and virtual servers -
see the manual for details."
  :group 'gnus-server
  :type 'gnus-select-method)

(defcustom gnus-message-archive-method
  `(nnfolder
    "archive"
    (nnfolder-directory ,(nnheader-concat message-directory "archive"))
    (nnfolder-active-file
     ,(nnheader-concat message-directory "archive/active"))
    (nnfolder-get-new-mail nil)
    (nnfolder-inhibit-expiry t))
  "*Method used for archiving messages you've sent.
This should be a mail method.

It's probably not a very effective to change this variable once you've
run Gnus once.  After doing that, you must edit this server from the
server buffer."
  :group 'gnus-server
  :group 'gnus-message
  :type 'gnus-select-method)

(defcustom gnus-message-archive-group nil
  "*Name of the group in which to save the messages you've written.
This can either be a string; a list of strings; or an alist
of regexps/functions/forms to be evaluated to return a string (or a list
of strings).  The functions are called with the name of the current
group (or nil) as a parameter.

If you want to save your mail in one group and the news articles you
write in another group, you could say something like:

 \(setq gnus-message-archive-group
        '((if (message-news-p)
              \"misc-news\"
            \"misc-mail\")))

Normally the group names returned by this variable should be
unprefixed -- which implicitly means \"store on the archive server\".
However, you may wish to store the message on some other server.  In
that case, just return a fully prefixed name of the group --
\"nnml+private:mail.misc\", for instance."
  :group 'gnus-message
  :type '(choice (const :tag "none" nil)
		 sexp
		 string))

(defcustom gnus-secondary-servers nil
  "List of NNTP servers that the user can choose between interactively.
To make Gnus query you for a server, you have to give `gnus' a
non-numeric prefix - `C-u M-x gnus', in short."
  :group 'gnus-server
  :type '(repeat string))

(defcustom gnus-nntp-server nil
  "*The name of the host running the NNTP server.
This variable is semi-obsolete.	 Use the `gnus-select-method'
variable instead."
  :group 'gnus-server
  :type '(choice (const :tag "disable" nil)
		 string))

(defcustom gnus-secondary-select-methods nil
  "A list of secondary methods that will be used for reading news.
This is a list where each element is a complete select method (see
`gnus-select-method').

If, for instance, you want to read your mail with the nnml backend,
you could set this variable:

\(setq gnus-secondary-select-methods '((nnml \"\")))"
:group 'gnus-server
:type '(repeat gnus-select-method))

(defvar gnus-backup-default-subscribed-newsgroups
  '("news.announce.newusers" "news.groups.questions" "gnu.emacs.gnus")
  "Default default new newsgroups the first time Gnus is run.
Should be set in paths.el, and shouldn't be touched by the user.")

(defcustom gnus-local-domain nil
  "Local domain name without a host name.
The DOMAINNAME environment variable is used instead if it is defined.
If the `system-name' function returns the full Internet name, there is
no need to set this variable."
  :group 'gnus-message
  :type '(choice (const :tag "default" nil)
		 string))

(defvar gnus-local-organization nil
  "String with a description of what organization (if any) the user belongs to.
Obsolete variable; use `message-user-organization' instead.")

;; Customization variables

(defcustom gnus-refer-article-method nil
  "Preferred method for fetching an article by Message-ID.
If you are reading news from the local spool (with nnspool), fetching
articles by Message-ID is painfully slow.  By setting this method to an
nntp method, you might get acceptable results.

The value of this variable must be a valid select method as discussed
in the documentation of `gnus-select-method'."
  :group 'gnus-server
  :type '(choice (const :tag "default" nil)
		 gnus-select-method))

(defcustom gnus-group-faq-directory
  '("/ftp@mirrors.aol.com:/pub/rtfm/usenet/"
    "/ftp@sunsite.auc.dk:/pub/usenet/"
    "/ftp@sunsite.doc.ic.ac.uk:/pub/usenet/news-faqs/"
    "/ftp@src.doc.ic.ac.uk:/usenet/news-FAQS/"
    "/ftp@ftp.seas.gwu.edu:/pub/rtfm/"
    "/ftp@rtfm.mit.edu:/pub/usenet/"
    "/ftp@ftp.uni-paderborn.de:/pub/FAQ/"
    "/ftp@ftp.sunet.se:/pub/usenet/"
    "/ftp@nctuccca.edu.tw:/USENET/FAQ/"
    "/ftp@hwarang.postech.ac.kr:/pub/usenet/"
    "/ftp@ftp.hk.super.net:/mirror/faqs/")
  "*Directory where the group FAQs are stored.
This will most commonly be on a remote machine, and the file will be
fetched by ange-ftp.

This variable can also be a list of directories.  In that case, the
first element in the list will be used by default.  The others can
be used when being prompted for a site.

Note that Gnus uses an aol machine as the default directory.  If this
feels fundamentally unclean, just think of it as a way to finally get
something of value back from them.

If the default site is too slow, try one of these:

   North America: mirrors.aol.com		 /pub/rtfm/usenet
		  ftp.seas.gwu.edu		 /pub/rtfm
		  rtfm.mit.edu			 /pub/usenet
   Europe:	  ftp.uni-paderborn.de		 /pub/FAQ
                  src.doc.ic.ac.uk               /usenet/news-FAQS
		  ftp.sunet.se			 /pub/usenet
	          sunsite.auc.dk                 /pub/usenet
   Asia:	  nctuccca.edu.tw		 /USENET/FAQ
		  hwarang.postech.ac.kr		 /pub/usenet
		  ftp.hk.super.net		 /mirror/faqs"
  :group 'gnus-group-various
  :type '(choice directory
		 (repeat directory)))

(defcustom gnus-use-cross-reference t
  "*Non-nil means that cross referenced articles will be marked as read.
If nil, ignore cross references.  If t, mark articles as read in
subscribed newsgroups.	If neither t nor nil, mark as read in all
newsgroups."
  :group 'gnus-server
  :type '(choice (const :tag "off" nil)
		 (const :tag "subscribed" t)
		 (sexp :format "all"
		       :value always)))

(defcustom gnus-process-mark ?#
  "*Process mark."
  :group 'gnus-group-visual
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-asynchronous nil
  "*If non-nil, Gnus will supply backends with data needed for async article fetching."
  :group 'gnus-asynchronous
  :type 'boolean)

(defcustom gnus-large-newsgroup 200
  "*The number of articles which indicates a large newsgroup.
If the number of articles in a newsgroup is greater than this value,
confirmation is required for selecting the newsgroup."
  :group 'gnus-group-select
  :type 'integer)

(defcustom gnus-use-long-file-name (not (memq system-type '(usg-unix-v xenix)))
  "*Non-nil means that the default name of a file to save articles in is the group name.
If it's nil, the directory form of the group name is used instead.

If this variable is a list, and the list contains the element
`not-score', long file names will not be used for score files; if it
contains the element `not-save', long file names will not be used for
saving; and if it contains the element `not-kill', long file names
will not be used for kill files.

Note that the default for this variable varies according to what system
type you're using.  On `usg-unix-v' and `xenix' this variable defaults
to nil while on all other systems it defaults to t."
  :group 'gnus-start
  :type 'boolean)

(defcustom gnus-kill-files-directory gnus-directory
  "*Name of the directory where kill files will be stored (default \"~/News\")."
  :group 'gnus-score-files
  :group 'gnus-score-kill
  :type 'directory)

(defcustom gnus-save-score nil
  "*If non-nil, save group scoring info."
  :group 'gnus-score-various
  :group 'gnus-start
  :type 'boolean)

(defcustom gnus-use-undo t
  "*If non-nil, allow undoing in Gnus group mode buffers."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-use-adaptive-scoring nil
  "*If non-nil, use some adaptive scoring scheme.
If a list, then the values `word' and `line' are meaningful.  The
former will perform adaption on individual words in the subject
header while `line' will perform adaption on several headers."
  :group 'gnus-meta
  :group 'gnus-score-adapt
  :type '(set (const word) (const line)))

(defcustom gnus-use-cache 'passive
  "*If nil, Gnus will ignore the article cache.
If `passive', it will allow entering (and reading) articles
explicitly entered into the cache.  If anything else, use the
cache to the full extent of the law."
  :group 'gnus-meta
  :group 'gnus-cache
  :type '(choice (const :tag "off" nil)
		 (const :tag "passive" passive)
		 (const :tag "active" t)))

(defcustom gnus-use-trees nil
  "*If non-nil, display a thread tree buffer."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-use-grouplens nil
  "*If non-nil, use GroupLens ratings."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-keep-backlog nil
  "*If non-nil, Gnus will keep read articles for later re-retrieval.
If it is a number N, then Gnus will only keep the last N articles
read.  If it is neither nil nor a number, Gnus will keep all read
articles.  This is not a good idea."
  :group 'gnus-meta
  :type '(choice (const :tag "off" nil)
		 integer
		 (sexp :format "all"
		       :value t)))

(defcustom gnus-use-nocem nil
  "*If non-nil, Gnus will read NoCeM cancel messages."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-suppress-duplicates nil
  "*If non-nil, Gnus will mark duplicate copies of the same article as read."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-use-demon nil
  "If non-nil, Gnus might use some demons."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-use-scoring t
  "*If non-nil, enable scoring."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-use-picons nil
  "*If non-nil, display picons."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-summary-prepare-exit-hook
  '(gnus-summary-expire-articles)
  "*A hook called when preparing to exit from the summary buffer.
It calls `gnus-summary-expire-articles' by default."
  :group 'gnus-summary-exit
  :type 'hook)

(defcustom gnus-novice-user t
  "*Non-nil means that you are a usenet novice.
If non-nil, verbose messages may be displayed and confirmations may be
required."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-expert-user nil
  "*Non-nil means that you will never be asked for confirmation about anything.
That doesn't mean *anything* anything; particularly destructive
commands will still require prompting."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-interactive-catchup t
  "*If non-nil, require your confirmation when catching up a group."
  :group 'gnus-group-select
  :type 'boolean)

(defcustom gnus-interactive-exit t
  "*If non-nil, require your confirmation when exiting Gnus."
  :group 'gnus-exit
  :type 'boolean)

(defcustom gnus-extract-address-components 'gnus-extract-address-components
  "*Function for extracting address components from a From header.
Two pre-defined function exist: `gnus-extract-address-components',
which is the default, quite fast, and too simplistic solution, and
`mail-extract-address-components', which works much better, but is
slower."
  :group 'gnus-summary-format
  :type '(radio (function-item gnus-extract-address-components)
		(function-item mail-extract-address-components)
		(function :tag "Other")))

(defcustom gnus-carpal nil
  "*If non-nil, display clickable icons."
  :group 'gnus-meta
  :type 'boolean)

(defcustom gnus-shell-command-separator ";"
  "String used to separate to shell commands."
  :group 'gnus-files
  :type 'string)

(defcustom gnus-valid-select-methods
  '(("nntp" post address prompt-address physical-address)
    ("nnspool" post address)
    ("nnvirtual" post-mail virtual prompt-address)
    ("nnmbox" mail respool address)
    ("nnml" mail respool address)
    ("nnmh" mail respool address)
    ("nndir" post-mail prompt-address physical-address)
    ("nneething" none address prompt-address physical-address)
    ("nndoc" none address prompt-address)
    ("nnbabyl" mail address respool)
    ("nnkiboze" post virtual)
    ("nnsoup" post-mail address)
    ("nndraft" post-mail)
    ("nnfolder" mail respool address)
    ("nngateway" post-mail address prompt-address physical-address)
    ("nnweb" none)
    ("nnlistserv" none)
    ("nnagent" post-mail))
  "*An alist of valid select methods.
The first element of each list lists should be a string with the name
of the select method.  The other elements may be the category of
this method (i. e., `post', `mail', `none' or whatever) or other
properties that this method has (like being respoolable).
If you implement a new select method, all you should have to change is
this variable.	I think."
  :group 'gnus-server
  :type '(repeat (group (string :tag "Name")
			(radio-button-choice (const :format "%v " post)
					     (const :format "%v " mail)
					     (const :format "%v " none)
					     (const post-mail))
			(checklist :inline t
				   (const :format "%v " address)
				   (const :format "%v " prompt-address)
				   (const :format "%v " physical-address)
				   (const :format "%v " virtual)
				   (const respool)))))

(define-widget 'gnus-select-method 'list
  "Widget for entering a select method."
  :args `((choice :tag "Method"
		  ,@(mapcar (lambda (entry)
			      (list 'const :format "%v\n"
				    (intern (car entry))))
			    gnus-valid-select-methods))
	  (string :tag "Address")
	  (editable-list  :inline t
			  (list :format "%v"
				variable
				(sexp :tag "Value")))))

(defcustom gnus-updated-mode-lines '(group article summary tree)
  "List of buffers that should update their mode lines.
The list may contain the symbols `group', `article', `tree' and
`summary'.  If the corresponding symbol is present, Gnus will keep
that mode line updated with information that may be pertinent.
If this variable is nil, screen refresh may be quicker."
  :group 'gnus-various
  :type '(set (const group)
	      (const article)
	      (const summary)
	      (const tree)))

;; Added by Keinonen Kari <kk85613@cs.tut.fi>.
(defcustom gnus-mode-non-string-length nil
  "*Max length of mode-line non-string contents.
If this is nil, Gnus will take space as is needed, leaving the rest
of the modeline intact.  Note that the default of nil is unlikely
to be desirable; see the manual for further details."
  :group 'gnus-various
  :type '(choice (const nil)
		 integer))

(defcustom gnus-auto-expirable-newsgroups nil
  "*Groups in which to automatically mark read articles as expirable.
If non-nil, this should be a regexp that should match all groups in
which to perform auto-expiry.  This only makes sense for mail groups."
  :group 'nnmail-expire
  :type '(choice (const nil)
		 regexp))

(defcustom gnus-total-expirable-newsgroups nil
  "*Groups in which to perform expiry of all read articles.
Use with extreme caution.  All groups that match this regexp will be
expiring - which means that all read articles will be deleted after
\(say) one week.	 (This only goes for mail groups and the like, of
course.)"
  :group 'nnmail-expire
  :type '(choice (const nil)
		 regexp))

(defcustom gnus-group-uncollapsed-levels 1
  "Number of group name elements to leave alone when making a short group name."
  :group 'gnus-group-visual
  :type 'integer)

(defcustom gnus-group-use-permanent-levels nil
  "*If non-nil, once you set a level, Gnus will use this level."
  :group 'gnus-group-levels
  :type 'boolean)

;; Hooks.

(defcustom gnus-load-hook nil
  "A hook run while Gnus is loaded."
  :group 'gnus-start
  :type 'hook)

(defcustom gnus-apply-kill-hook '(gnus-apply-kill-file)
  "A hook called to apply kill files to a group.
This hook is intended to apply a kill file to the selected newsgroup.
The function `gnus-apply-kill-file' is called by default.

Since a general kill file is too heavy to use only for a few
newsgroups, I recommend you to use a lighter hook function.  For
example, if you'd like to apply a kill file to articles which contains
a string `rmgroup' in subject in newsgroup `control', you can use the
following hook:

 (setq gnus-apply-kill-hook
      (list
	(lambda ()
	  (cond ((string-match \"control\" gnus-newsgroup-name)
		 (gnus-kill \"Subject\" \"rmgroup\")
		 (gnus-expunge \"X\"))))))"
  :group 'gnus-score-kill
  :options '(gnus-apply-kill-file)
  :type 'hook)

(defcustom gnus-group-change-level-function nil
  "Function run when a group level is changed.
It is called with three parameters -- GROUP, LEVEL and OLDLEVEL."
  :group 'gnus-group-level
  :type 'function)

;;; Face thingies.

(defcustom gnus-visual
  '(summary-highlight group-highlight article-highlight
		      mouse-face
		      summary-menu group-menu article-menu
		      tree-highlight menu highlight
		      browse-menu server-menu
		      page-marker tree-menu binary-menu pick-menu
		      grouplens-menu)
  "*Enable visual features.
If `visual' is disabled, there will be no menus and few faces.  Most of
the visual customization options below will be ignored.  Gnus will use
less space and be faster as a result.

This variable can also be a list of visual elements to switch on.  For
instance, to switch off all visual things except menus, you can say:

   (setq gnus-visual '(menu))

Valid elements include `summary-highlight', `group-highlight',
`article-highlight', `mouse-face', `summary-menu', `group-menu',
`article-menu', `tree-highlight', `menu', `highlight', `browse-menu',
`server-menu', `page-marker', `tree-menu', `binary-menu', `pick-menu',
and `grouplens-menu'."
  :group 'gnus-meta
  :group 'gnus-visual
  :type '(set (const summary-highlight)
	      (const group-highlight)
	      (const article-highlight)
	      (const mouse-face)
	      (const summary-menu)
	      (const group-menu)
	      (const article-menu)
	      (const tree-highlight)
	      (const menu)
	      (const highlight)
	      (const browse-menu)
	      (const server-menu)
	      (const page-marker)
	      (const tree-menu)
	      (const binary-menu)
	      (const pick-menu)
	      (const grouplens-menu)))

(defcustom gnus-mouse-face
  (condition-case ()
      (if (gnus-visual-p 'mouse-face 'highlight)
	  (if (boundp 'gnus-mouse-face)
	      (or gnus-mouse-face 'highlight)
	    'highlight)
	'default)
    (error 'highlight))
  "*Face used for group or summary buffer mouse highlighting.
The line beneath the mouse pointer will be highlighted with this
face."
  :group 'gnus-visual
  :type 'face)

(defcustom gnus-article-display-hook
  (if (and (string-match "XEmacs" emacs-version)
	   (featurep 'xface))
      '(gnus-article-hide-headers-if-wanted
	gnus-article-hide-boring-headers
	gnus-article-treat-overstrike
	gnus-article-maybe-highlight
	gnus-article-display-x-face)
    '(gnus-article-hide-headers-if-wanted
      gnus-article-hide-boring-headers
      gnus-article-treat-overstrike
      gnus-article-maybe-highlight))
  "*Controls how the article buffer will look.

If you leave the list empty, the article will appear exactly as it is
stored on the disk.  The list entries will hide or highlight various
parts of the article, making it easier to find the information you
want."
  :group 'gnus-article-highlight
  :group 'gnus-visual
  :type 'hook
  :options '(gnus-article-add-buttons
	     gnus-article-add-buttons-to-head
	     gnus-article-emphasize
	     gnus-article-fill-cited-article
	     gnus-article-remove-cr
	     gnus-article-de-quoted-unreadable
	     gnus-summary-stop-page-breaking
	     ;; gnus-summary-caesar-message
	     ;; gnus-summary-verbose-headers
	     gnus-summary-toggle-mime
	     gnus-article-hide
	     gnus-article-hide-headers
	     gnus-article-hide-boring-headers
	     gnus-article-hide-signature
	     gnus-article-hide-citation
	     gnus-article-hide-pgp
	     gnus-article-hide-pem
	     gnus-article-highlight
	     gnus-article-highlight-headers
	     gnus-article-highlight-citation
	     gnus-article-highlight-signature
	     gnus-article-date-ut
	     gnus-article-date-local
	     gnus-article-date-lapsed
	     gnus-article-date-original
	     gnus-article-remove-trailing-blank-lines
	     gnus-article-strip-leading-blank-lines
	     gnus-article-strip-multiple-blank-lines
	     gnus-article-strip-blank-lines
	     gnus-article-treat-overstrike
	     gnus-article-display-x-face
	     gnus-smiley-display))

(defcustom gnus-article-save-directory gnus-directory
  "*Name of the directory articles will be saved in (default \"~/News\")."
  :group 'gnus-article-saving
  :type 'directory)

(defvar gnus-plugged t
  "Whether Gnus is plugged or not.")


;;; Internal variables

(defvar gnus-group-get-parameter-function 'gnus-group-get-parameter)
(defvar gnus-original-article-buffer " *Original Article*")
(defvar gnus-newsgroup-name nil)
(defvar gnus-ephemeral-servers nil)

(defvar gnus-agent nil
  "Whether we want to use the Gnus agent or not.")

(defvar gnus-command-method nil
  "Dynamically bound variable that says what the current backend is.")

(defvar gnus-current-select-method nil
  "The current method for selecting a newsgroup.")

(defvar gnus-tree-buffer "*Tree*"
  "Buffer where Gnus thread trees are displayed.")

;; Dummy variable.
(defvar gnus-use-generic-from nil)

;; Variable holding the user answers to all method prompts.
(defvar gnus-method-history nil)

;; Variable holding the user answers to all mail method prompts.
(defvar gnus-mail-method-history nil)

;; Variable holding the user answers to all group prompts.
(defvar gnus-group-history nil)

(defvar gnus-server-alist nil
  "List of available servers.")

(defcustom gnus-cache-directory
  (nnheader-concat gnus-directory "cache/")
  "*The directory where cached articles will be stored."
  :group 'gnus-cache
  :type 'directory)

(defvar gnus-predefined-server-alist
  `(("cache"
     nnspool "cache"
     (nnspool-spool-directory ,gnus-cache-directory)
     (nnspool-nov-directory ,gnus-cache-directory)
     (nnspool-active-file
      ,(nnheader-concat gnus-cache-directory "active"))))
  "List of predefined (convenience) servers.")

(defvar gnus-topic-indentation "") ;; Obsolete variable.

(defconst gnus-article-mark-lists
  '((marked . tick) (replied . reply)
    (expirable . expire) (killed . killed)
    (bookmarks . bookmark) (dormant . dormant)
    (scored . score) (saved . save)
    (cached . cache) (downloadable . download)
    (unsendable . unsend)))

(defvar gnus-headers-retrieved-by nil)
(defvar gnus-article-reply nil)
(defvar gnus-override-method nil)
(defvar gnus-article-check-size nil)
(defvar gnus-opened-servers nil)

(defvar gnus-current-kill-article nil)

(defvar gnus-have-read-active-file nil)

(defconst gnus-maintainer
  "bugs@gnus.org (The Gnus Bugfixing Girls + Boys)"
  "The mail address of the Gnus maintainers.")

(defvar gnus-info-nodes
  '((gnus-group-mode "(gnus)The Group Buffer")
    (gnus-summary-mode "(gnus)The Summary Buffer")
    (gnus-article-mode "(gnus)The Article Buffer")
    (mime/viewer-mode "(gnus)The Article Buffer")
    (gnus-server-mode "(gnus)The Server Buffer")
    (gnus-browse-mode "(gnus)Browse Foreign Server")
    (gnus-tree-mode "(gnus)Tree Display"))
  "Alist of major modes and related Info nodes.")

(defvar gnus-group-buffer "*Group*")
(defvar gnus-summary-buffer "*Summary*")
(defvar gnus-article-buffer "*Article*")
(defvar gnus-server-buffer "*Server*")

(defvar gnus-slave nil
  "Whether this Gnus is a slave or not.")

(defvar gnus-batch-mode nil
  "Whether this Gnus is running in batch mode or not.")

(defvar gnus-variable-list
  '(gnus-newsrc-options gnus-newsrc-options-n
    gnus-newsrc-last-checked-date
    gnus-newsrc-alist gnus-server-alist
    gnus-killed-list gnus-zombie-list
    gnus-topic-topology gnus-topic-alist
    gnus-format-specs)
  "Gnus variables saved in the quick startup file.")

(defvar gnus-newsrc-alist nil
  "Assoc list of read articles.
gnus-newsrc-hashtb should be kept so that both hold the same information.")

(defvar gnus-newsrc-hashtb nil
  "Hashtable of gnus-newsrc-alist.")

(defvar gnus-killed-list nil
  "List of killed newsgroups.")

(defvar gnus-killed-hashtb nil
  "Hash table equivalent of gnus-killed-list.")

(defvar gnus-zombie-list nil
  "List of almost dead newsgroups.")

(defvar gnus-description-hashtb nil
  "Descriptions of newsgroups.")

(defvar gnus-list-of-killed-groups nil
  "List of newsgroups that have recently been killed by the user.")

(defvar gnus-active-hashtb nil
  "Hashtable of active articles.")

(defvar gnus-moderated-hashtb nil
  "Hashtable of moderated newsgroups.")

;; Save window configuration.
(defvar gnus-prev-winconf nil)

(defvar gnus-reffed-article-number nil)

;;; Let the byte-compiler know that we know about this variable.
(defvar rmail-default-rmail-file)

(defvar gnus-dead-summary nil)

;;; End of variables.

;; Define some autoload functions Gnus might use.
(eval-and-compile

  ;; This little mapcar goes through the list below and marks the
  ;; symbols in question as autoloaded functions.
  (mapcar
   (lambda (package)
     (let ((interactive (nth 1 (memq ':interactive package))))
       (mapcar
	(lambda (function)
	  (let (keymap)
	    (when (consp function)
	      (setq keymap (car (memq 'keymap function)))
	      (setq function (car function)))
	    (autoload function (car package) nil interactive keymap)))
	(if (eq (nth 1 package) ':interactive)
	    (cdddr package)
	  (cdr package)))))
   '(("metamail" metamail-buffer)
     ("info" Info-goto-node)
     ("hexl" hexl-hex-string-to-integer)
     ("pp" pp pp-to-string pp-eval-expression)
     ("ps-print" ps-print-preprint)
     ("mail-extr" mail-extract-address-components)
     ("browse-url" browse-url)
     ("message" :interactive t
      message-send-and-exit message-yank-original)
     ("nnmail" nnmail-split-fancy nnmail-article-group nnmail-date-to-time)
     ("nnvirtual" nnvirtual-catchup-group nnvirtual-convert-headers)
     ("timezone" timezone-make-date-arpa-standard timezone-fix-time
      timezone-make-sortable-date timezone-make-time-string)
     ("rmailout" rmail-output)
     ("rmail" rmail-insert-rmail-file-header rmail-count-new-messages
      rmail-show-message rmail-summary-exists
      rmail-select-summary rmail-update-summary)
     ("gnus-audio" :interactive t gnus-audio-play)
     ("gnus-xmas" gnus-xmas-splash)
     ("gnus-soup" :interactive t
      gnus-group-brew-soup gnus-brew-soup gnus-soup-add-article
      gnus-soup-send-replies gnus-soup-save-areas gnus-soup-pack-packet)
     ("nnsoup" nnsoup-pack-replies)
     ("score-mode" :interactive t gnus-score-mode)
     ("gnus-mh" gnus-summary-save-article-folder
      gnus-Folder-save-name gnus-folder-save-name)
     ("gnus-mh" :interactive t gnus-summary-save-in-folder)
     ("gnus-demon" gnus-demon-add-nocem gnus-demon-add-scanmail
      gnus-demon-add-rescan gnus-demon-add-scan-timestamps
      gnus-demon-add-disconnection gnus-demon-add-handler
      gnus-demon-remove-handler)
     ("gnus-demon" :interactive t
      gnus-demon-init gnus-demon-cancel)
     ("gnus-salt" gnus-highlight-selected-tree gnus-possibly-generate-tree
      gnus-tree-open gnus-tree-close gnus-carpal-setup-buffer)
     ("gnus-nocem" gnus-nocem-scan-groups gnus-nocem-close
      gnus-nocem-unwanted-article-p)
     ("gnus-srvr" gnus-enter-server-buffer gnus-server-set-info
      gnus-server-server-name)
     ("gnus-srvr" gnus-browse-foreign-server)
     ("gnus-cite" :interactive t
      gnus-article-highlight-citation gnus-article-hide-citation-maybe
      gnus-article-hide-citation gnus-article-fill-cited-article
      gnus-article-hide-citation-in-followups)
     ("gnus-kill" gnus-kill gnus-apply-kill-file-internal
      gnus-kill-file-edit-file gnus-kill-file-raise-followups-to-author
      gnus-execute gnus-expunge)
     ("gnus-cache" gnus-cache-possibly-enter-article gnus-cache-save-buffers
      gnus-cache-possibly-remove-articles gnus-cache-request-article
      gnus-cache-retrieve-headers gnus-cache-possibly-alter-active
      gnus-cache-enter-remove-article gnus-cached-article-p
      gnus-cache-open gnus-cache-close gnus-cache-update-article)
      ("gnus-cache" :interactive t gnus-jog-cache gnus-cache-enter-article
       gnus-cache-remove-article gnus-summary-insert-cached-articles)
      ("gnus-score" :interactive t
       gnus-summary-increase-score gnus-summary-set-score
       gnus-summary-raise-thread gnus-summary-raise-same-subject
       gnus-summary-raise-score gnus-summary-raise-same-subject-and-select
       gnus-summary-lower-thread gnus-summary-lower-same-subject
       gnus-summary-lower-score gnus-summary-lower-same-subject-and-select
       gnus-summary-current-score gnus-score-default
       gnus-score-flush-cache gnus-score-close
       gnus-possibly-score-headers gnus-score-followup-article
       gnus-score-followup-thread)
      ("gnus-score"
       (gnus-summary-score-map keymap) gnus-score-save gnus-score-headers
      gnus-current-score-file-nondirectory gnus-score-adaptive
      gnus-score-find-trace gnus-score-file-name)
     ("gnus-cus" :interactive t gnus-group-customize gnus-score-customize)
     ("gnus-topic" :interactive t gnus-topic-mode)
     ("gnus-topic" gnus-topic-remove-group gnus-topic-set-parameters)
     ("gnus-salt" :interactive t gnus-pick-mode gnus-binary-mode)
     ("gnus-uu" (gnus-uu-extract-map keymap) (gnus-uu-mark-map keymap))
     ("gnus-uu" :interactive t
      gnus-uu-post-news
      gnus-uu-digest-mail-forward gnus-uu-digest-post-forward
      gnus-uu-mark-series gnus-uu-mark-region gnus-uu-mark-buffer
      gnus-uu-mark-by-regexp gnus-uu-mark-all
      gnus-uu-mark-sparse gnus-uu-mark-thread gnus-uu-decode-uu
      gnus-uu-decode-uu-and-save gnus-uu-decode-unshar
      gnus-uu-decode-unshar-and-save gnus-uu-decode-save
      gnus-uu-decode-binhex gnus-uu-decode-uu-view
      gnus-uu-decode-uu-and-save-view gnus-uu-decode-unshar-view
      gnus-uu-decode-unshar-and-save-view gnus-uu-decode-save-view
      gnus-uu-decode-binhex-view gnus-uu-unmark-thread
      gnus-uu-mark-over gnus-uu-post-news gnus-uu-post-news)
     ("gnus-uu" gnus-uu-delete-work-dir gnus-quote-arg-for-sh-or-csh
      gnus-uu-unmark-thread)
     ("gnus-msg" (gnus-summary-send-map keymap)
      gnus-article-mail gnus-copy-article-buffer gnus-extended-version)
     ("gnus-msg" :interactive t
      gnus-summary-wide-reply
      gnus-summary-wide-reply-with-original
      gnus-summary-followup-to-mail
      gnus-summary-followup-to-mail-with-original
      gnus-summary-post-forward
      gnus-group-post-news gnus-group-mail gnus-summary-post-news
      gnus-summary-followup gnus-summary-followup-with-original
      gnus-summary-cancel-article gnus-summary-supersede-article
      gnus-post-news gnus-summary-reply gnus-summary-reply-with-original
      gnus-summary-mail-forward gnus-summary-mail-other-window
      gnus-summary-resend-message gnus-summary-resend-bounced-mail
      gnus-summary-wide-reply gnus-summary-followup-to-mail
      gnus-summary-followup-to-mail-with-original gnus-bug
      gnus-summary-wide-reply-with-original
      gnus-summary-post-forward gnus-summary-wide-reply-with-original
      gnus-summary-post-forward)
     ("gnus-picon" :interactive t gnus-article-display-picons
      gnus-group-display-picons gnus-picons-article-display-x-face
      gnus-picons-display-x-face)
     ("gnus-gl" bbb-login bbb-logout bbb-grouplens-group-p
      gnus-grouplens-mode)
     ("smiley" :interactive t gnus-smiley-display)
     ("gnus-win" gnus-configure-windows gnus-add-configuration)
     ("gnus-sum" gnus-summary-insert-line gnus-summary-read-group
      gnus-list-of-unread-articles gnus-list-of-read-articles
      gnus-offer-save-summaries gnus-make-thread-indent-array
      gnus-summary-exit gnus-update-read-articles gnus-summary-last-subject
      gnus-summary-skip-intangible gnus-summary-article-number
      gnus-data-header gnus-data-find)
     ("gnus-group" gnus-group-insert-group-line gnus-group-quit
      gnus-group-list-groups gnus-group-first-unread-group
      gnus-group-set-mode-line gnus-group-set-info gnus-group-save-newsrc
      gnus-group-setup-buffer gnus-group-get-new-news
      gnus-group-make-help-group gnus-group-update-group
      gnus-clear-inboxes-moved gnus-group-iterate
      gnus-group-group-name)
     ("gnus-bcklg" gnus-backlog-request-article gnus-backlog-enter-article
      gnus-backlog-remove-article)
     ("gnus-art" gnus-article-read-summary-keys gnus-article-save
      gnus-article-prepare gnus-article-set-window-start
      gnus-article-next-page gnus-article-prev-page
      gnus-request-article-this-buffer gnus-article-mode
      gnus-article-setup-buffer gnus-narrow-to-page
      gnus-article-delete-invisible-text gnus-hack-decode-rfc1522)
     ("gnus-art" :interactive t
      gnus-article-hide-headers gnus-article-hide-boring-headers
      gnus-article-treat-overstrike gnus-article-word-wrap
      gnus-article-remove-cr gnus-article-remove-trailing-blank-lines
      gnus-article-display-x-face gnus-article-de-quoted-unreadable
      gnus-article-mime-decode-quoted-printable gnus-article-hide-pgp
      gnus-article-hide-pem gnus-article-hide-signature
      gnus-article-strip-leading-blank-lines gnus-article-date-local
      gnus-article-date-original gnus-article-date-lapsed
      gnus-article-show-all-headers
      gnus-article-edit-mode gnus-article-edit-article
      gnus-article-edit-done gnus-decode-rfc1522 article-decode-rfc1522
      gnus-start-date-timer gnus-stop-date-timer)
     ("gnus-int" gnus-request-type)
     ("gnus-start" gnus-newsrc-parse-options gnus-1 gnus-no-server-1
      gnus-dribble-enter gnus-read-init-file gnus-dribble-touch)
     ("gnus-dup" gnus-dup-suppress-articles gnus-dup-unsuppress-article
      gnus-dup-enter-articles)
     ("gnus-range" gnus-copy-sequence)
     ("gnus-eform" gnus-edit-form)
     ("gnus-move" :interactive t
      gnus-group-move-group-to-server gnus-change-server)
     ("gnus-logic" gnus-score-advanced)
     ("gnus-undo" gnus-undo-mode gnus-undo-register)
     ("gnus-async" gnus-async-request-fetched-article gnus-async-prefetch-next
      gnus-async-prefetch-article gnus-async-prefetch-remove-group
      gnus-async-halt-prefetch)
     ("gnus-agent" gnus-open-agent gnus-agent-get-function
      gnus-agent-save-groups gnus-agent-save-active gnus-agent-method-p
      gnus-agent-get-undownloaded-list gnus-agent-fetch-session
      gnus-summary-set-agent-mark gnus-agent-save-group-info)
     ("gnus-agent" :interactive t
      gnus-unplugged gnus-agentize gnus-agent-batch)
     ("gnus-vm" :interactive t gnus-summary-save-in-vm
      gnus-summary-save-article-vm)
     ("gnus-draft" :interactive t gnus-draft-mode gnus-group-send-drafts))))

;;; gnus-sum.el thingies


(defcustom gnus-summary-line-format "%U%R%z%I%(%[%4L: %-20,20n%]%) %s\n"
  "*The format specification of the lines in the summary buffer.

It works along the same lines as a normal formatting string,
with some simple extensions.

%N   Article number, left padded with spaces (string)
%S   Subject (string)
%s   Subject if it is at the root of a thread, and \"\" otherwise (string)
%n   Name of the poster (string)
%a   Extracted name of the poster (string)
%A   Extracted address of the poster (string)
%F   Contents of the From: header (string)
%x   Contents of the Xref: header (string)
%D   Date of the article (string)
%d   Date of the article (string) in DD-MMM format
%M   Message-id of the article (string)
%r   References of the article (string)
%c   Number of characters in the article (integer)
%L   Number of lines in the article (integer)
%I   Indentation based on thread level (a string of spaces)
%T   A string with two possible values: 80 spaces if the article
     is on thread level two or larger and 0 spaces on level one
%R   \"A\" if this article has been replied to, \" \" otherwise (character)
%U   Status of this article (character, \"R\", \"K\", \"-\" or \" \")
%[   Opening bracket (character, \"[\" or \"<\")
%]   Closing bracket (character, \"]\" or \">\")
%>   Spaces of length thread-level (string)
%<   Spaces of length (- 20 thread-level) (string)
%i   Article score (number)
%z   Article zcore (character)
%t   Number of articles under the current thread (number).
%e   Whether the thread is empty or not (character).
%l   GroupLens score (string).
%V   Total thread score (number).
%P   The line number (number).
%O   Download mark (character).
%u   User defined specifier.  The next character in the format string should
     be a letter.  Gnus will call the function gnus-user-format-function-X,
     where X is the letter following %u.  The function will be passed the
     current header as argument.  The function should return a string, which
     will be inserted into the summary just like information from any other
     summary specifier.

Text between %( and %) will be highlighted with `gnus-mouse-face'
when the mouse point is placed inside the area.	 There can only be one
such area.

The %U (status), %R (replied) and %z (zcore) specs have to be handled
with care.  For reasons of efficiency, Gnus will compute what column
these characters will end up in, and \"hard-code\" that.  This means that
it is illegal to have these specs after a variable-length spec.	 Well,
you might not be arrested, but your summary buffer will look strange,
which is bad enough.

The smart choice is to have these specs as for to the left as
possible.

This restriction may disappear in later versions of Gnus."
  :type 'string
  :group 'gnus-summary-format)

;;;
;;; Skeleton keymaps
;;;

(defun gnus-suppress-keymap (keymap)
  (suppress-keymap keymap)
  (let ((keys `([backspace] [delete] "\177" "\M-u"))) ;gnus-mouse-2
    (while keys
      (define-key keymap (pop keys) 'undefined))))

(defvar gnus-article-mode-map
  (let ((keymap (make-keymap)))
    (gnus-suppress-keymap keymap)
    keymap))
(defvar gnus-summary-mode-map
  (let ((keymap (make-keymap)))
    (gnus-suppress-keymap keymap)
    keymap))
(defvar gnus-group-mode-map
  (let ((keymap (make-keymap)))
    (gnus-suppress-keymap keymap)
    keymap))



;; Fix by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
;; If you want the cursor to go somewhere else, set these two
;; functions in some startup hook to whatever you want.
(defalias 'gnus-summary-position-point 'gnus-goto-colon)
(defalias 'gnus-group-position-point 'gnus-goto-colon)

;;; Various macros and substs.

(defun gnus-header-from (header)
  (mail-header-from header))

(defmacro gnus-gethash (string hashtable)
  "Get hash value of STRING in HASHTABLE."
  `(symbol-value (intern-soft ,string ,hashtable)))

(defmacro gnus-sethash (string value hashtable)
  "Set hash value.  Arguments are STRING, VALUE, and HASHTABLE."
  `(set (intern ,string ,hashtable) ,value))
(put 'gnus-sethash 'edebug-form-spec '(form form form))

(defmacro gnus-group-unread (group)
  "Get the currently computed number of unread articles in GROUP."
  `(car (gnus-gethash ,group gnus-newsrc-hashtb)))

(defmacro gnus-group-entry (group)
  "Get the newsrc entry for GROUP."
  `(gnus-gethash ,group gnus-newsrc-hashtb))

(defmacro gnus-active (group)
  "Get active info on GROUP."
  `(gnus-gethash ,group gnus-active-hashtb))

(defmacro gnus-set-active (group active)
  "Set GROUP's active info."
  `(gnus-sethash ,group ,active gnus-active-hashtb))

;; Info access macros.

(defmacro gnus-info-group (info)
  `(nth 0 ,info))
(defmacro gnus-info-rank (info)
  `(nth 1 ,info))
(defmacro gnus-info-read (info)
  `(nth 2 ,info))
(defmacro gnus-info-marks (info)
  `(nth 3 ,info))
(defmacro gnus-info-method (info)
  `(nth 4 ,info))
(defmacro gnus-info-params (info)
  `(nth 5 ,info))

(defmacro gnus-info-level (info)
  `(let ((rank (gnus-info-rank ,info)))
     (if (consp rank)
	 (car rank)
       rank)))
(defmacro gnus-info-score (info)
  `(let ((rank (gnus-info-rank ,info)))
     (or (and (consp rank) (cdr rank)) 0)))

(defmacro gnus-info-set-group (info group)
  `(setcar ,info ,group))
(defmacro gnus-info-set-rank (info rank)
  `(setcar (nthcdr 1 ,info) ,rank))
(defmacro gnus-info-set-read (info read)
  `(setcar (nthcdr 2 ,info) ,read))
(defmacro gnus-info-set-marks (info marks &optional extend)
  (if extend
      `(gnus-info-set-entry ,info ,marks 3)
    `(setcar (nthcdr 3 ,info) ,marks)))
(defmacro gnus-info-set-method (info method &optional extend)
  (if extend
      `(gnus-info-set-entry ,info ,method 4)
    `(setcar (nthcdr 4 ,info) ,method)))
(defmacro gnus-info-set-params (info params &optional extend)
  (if extend
      `(gnus-info-set-entry ,info ,params 5)
    `(setcar (nthcdr 5 ,info) ,params)))

(defun gnus-info-set-entry (info entry number)
  ;; Extend the info until we have enough elements.
  (while (<= (length info) number)
    (nconc info (list nil)))
  ;; Set the entry.
  (setcar (nthcdr number info) entry))

(defmacro gnus-info-set-level (info level)
  `(let ((rank (cdr ,info)))
     (if (consp (car rank))
	 (setcar (car rank) ,level)
       (setcar rank ,level))))
(defmacro gnus-info-set-score (info score)
  `(let ((rank (cdr ,info)))
     (if (consp (car rank))
	 (setcdr (car rank) ,score)
       (setcar rank (cons (car rank) ,score)))))

(defmacro gnus-get-info (group)
  `(nth 2 (gnus-gethash ,group gnus-newsrc-hashtb)))

;; Byte-compiler warning.
(defvar gnus-visual)
;; Find out whether the gnus-visual TYPE is wanted.
(defun gnus-visual-p (&optional type class)
  (and gnus-visual			; Has to be non-nil, at least.
       (if (not type)			; We don't care about type.
	   gnus-visual
	 (if (listp gnus-visual)	; It's a list, so we check it.
	     (or (memq type gnus-visual)
		 (memq class gnus-visual))
	   t))))

;;; Load the compatability functions.

(require 'gnus-ems)


;;;
;;; Shutdown
;;;

(defvar gnus-shutdown-alist nil)

(defun gnus-add-shutdown (function &rest symbols)
  "Run FUNCTION whenever one of SYMBOLS is shut down."
  (push (cons function symbols) gnus-shutdown-alist))

(defun gnus-shutdown (symbol)
  "Shut down everything that waits for SYMBOL."
  (let ((alist gnus-shutdown-alist)
	entry)
    (while (setq entry (pop alist))
      (when (memq symbol (cdr entry))
	(funcall (car entry))))))


;;;
;;; Gnus Utility Functions
;;;


(defmacro gnus-string-or (&rest strings)
  "Return the first element of STRINGS that is a non-blank string.
STRINGS will be evaluated in normal `or' order."
  `(gnus-string-or-1 ',strings))

(defun gnus-string-or-1 (strings)
  (let (string)
    (while strings
      (setq string (eval (pop strings)))
      (if (string-match "^[ \t]*$" string)
	  (setq string nil)
	(setq strings nil)))
    string))

(defun gnus-version (&optional arg)
  "Version number of this version of Gnus.
If ARG, insert string at point."
  (interactive "P")
  (if arg
      (insert (message gnus-version))
    (message gnus-version)))

(defun gnus-continuum-version (version)
  "Return VERSION as a floating point number."
  (when (or (string-match "^\\([^ ]+\\)? ?Gnus v?\\([0-9.]+\\)$" version)
	    (string-match "^\\(.?\\)gnus-\\([0-9.]+\\)$" version))
    (let ((alpha (and (match-beginning 1) (match-string 1 version)))
	  (number (match-string 2 version))
	  major minor least)
      (unless (string-match
	       "\\([0-9]\\)\\.\\([0-9]+\\)\\.?\\([0-9]+\\)?" number)
	(error "Invalid version string: %s" version))
      (setq major (string-to-number (match-string 1 number))
	    minor (string-to-number (match-string 2 number))
	    least (if (match-beginning 3)
		      (string-to-number (match-string 3 number))
		    0))
      (string-to-number
       (if (zerop major)
	   (format "%s00%02d%02d"
		   (cond
		    ((member alpha '("(ding)" "d")) "4.99")
		    ((member alpha '("September" "s")) "5.01")
		    ((member alpha '("Red" "r")) "5.03")
		    ((member alpha '("Quassia" "q")) "5.05")
		    ((member alpha '("p")) "5.07")
		    ((member alpha '("o")) "5.09")
		    ((member alpha '("n")) "5.11"))
		   minor least)
	 (format "%d.%02d%02d" major minor least))))))

(defun gnus-info-find-node ()
  "Find Info documentation of Gnus."
  (interactive)
  ;; Enlarge info window if needed.
  (let (gnus-info-buffer)
    (Info-goto-node (cadr (assq major-mode gnus-info-nodes)))
    (setq gnus-info-buffer (current-buffer))
    (gnus-configure-windows 'info)))

;;;
;;; gnus-interactive
;;;

(defvar gnus-current-prefix-symbol nil
  "Current prefix symbol.")

(defvar gnus-current-prefix-symbols nil
  "List of current prefix symbols.")

(defun gnus-interactive (string &optional params)
  "Return a list that can be fed to `interactive'.
See `interactive' for full documentation.

Adds the following specs:

y -- The current symbolic prefix.
Y -- A list of the current symbolic prefix(es).
A -- Article number.
H -- Article header.
g -- Group name."
  (let ((i 0)
	out c prompt)
    (while (< i (length string))
      (string-match ".\\([^\n]*\\)\n?" string i)
      (setq c (aref string i))
      (when (match-end 1)
	(setq prompt (match-string 1 string)))
      (setq i (match-end 0))
      ;; We basically emulate just about everything that
      ;; `interactive' does, but add the specs listed above.
      (push
       (cond
	((= c ?a)
	 (completing-read prompt obarray 'fboundp t))
	((= c ?b)
	 (read-buffer prompt (current-buffer) t))
	((= c ?B)
	 (read-buffer prompt (other-buffer (current-buffer))))
	((= c ?c)
	 (read-char))
	((= c ?C)
	 (completing-read prompt obarray 'commandp t))
	((= c ?d)
	 (point))
	((= c ?D)
	 (read-file-name prompt nil default-directory 'lambda))
	((= c ?f)
	 (read-file-name prompt nil nil 'lambda))
	((= c ?F)
	 (read-file-name prompt))
	((= c ?k)
	 (read-key-sequence prompt))
	((= c ?K)
	 (error "Not implemented spec"))
	((= c ?e)
	 (error "Not implemented spec"))
	((= c ?m)
	 (mark))
	((= c ?N)
	 (error "Not implemented spec"))
	((= c ?n)
	 (string-to-number (read-from-minibuffer prompt)))
	((= c ?p)
	 (prefix-numeric-value current-prefix-arg))
	((= c ?P)
	 current-prefix-arg)
	((= c ?r)
	 'gnus-prefix-nil)
	((= c ?s)
	 (read-string prompt))
	((= c ?S)
	 (intern (read-string prompt)))
	((= c ?v)
	 (read-variable prompt))
	((= c ?x)
	 (read-minibuffer prompt))
	((= c ?x)
	 (eval-minibuffer prompt))
	;; And here the new specs come.
	((= c ?y)
	 gnus-current-prefix-symbol)
	((= c ?Y)
	 gnus-current-prefix-symbols)
	((= c ?g)
	 (gnus-group-group-name))
	((= c ?A)
	 (gnus-summary-skip-intangible)
	 (or (get-text-property (point) 'gnus-number)
	     (gnus-summary-last-subject)))
	((= c ?H)
	 (gnus-data-header (gnus-data-find (gnus-summary-article-number))))
	(t
	 (error "Non-implemented spec")))
       out)
      (cond
       ((= c ?r)
	(push (if (< (point) (mark) (point) (mark))) out)
	(push (if (> (point) (mark) (point) (mark))) out))))
    (setq out (delq 'gnus-prefix-nil out))
    (nreverse out)))

(defun gnus-symbolic-argument (&optional arg)
  "Read a symbolic argument and a command, and then execute command."
  (interactive "P")
  (let* ((in-command (this-command-keys))
	 (command in-command)
	 gnus-current-prefix-symbols
	 gnus-current-prefix-symbol
	 syms)
    (while (equal in-command command)
      (message "%s-" (key-description (this-command-keys)))
      (push (intern (char-to-string (read-char))) syms)
      (setq command (read-key-sequence nil t)))
    (setq gnus-current-prefix-symbols (nreverse syms)
	  gnus-current-prefix-symbol (car gnus-current-prefix-symbols))
    (call-interactively (key-binding command t))))

;;; More various functions.

(defsubst gnus-check-backend-function (func group)
  "Check whether GROUP supports function FUNC.
GROUP can either be a string (a group name) or a select method."
  (ignore-errors
    (let ((method (if (stringp group)
		      (car (gnus-find-method-for-group group))
		    group)))
      (unless (featurep method)
	(require method))
      (fboundp (intern (format "%s-%s" method func))))))

(defun gnus-group-read-only-p (&optional group)
  "Check whether GROUP supports editing or not.
If GROUP is nil, `gnus-newsgroup-name' will be checked instead.	 Note
that that variable is buffer-local to the summary buffers."
  (let ((group (or group gnus-newsgroup-name)))
    (not (gnus-check-backend-function 'request-replace-article group))))

(defun gnus-group-total-expirable-p (group)
  "Check whether GROUP is total-expirable or not."
  (let ((params (gnus-group-find-parameter group))
	val)
    (cond
     ((memq 'total-expire params)
      t)
     ((setq val (assq 'total-expire params)) ; (auto-expire . t)
      (cdr val))
     (gnus-total-expirable-newsgroups	; Check var.
      (string-match gnus-total-expirable-newsgroups group)))))

(defun gnus-group-auto-expirable-p (group)
  "Check whether GROUP is auto-expirable or not."
  (let ((params (gnus-group-find-parameter group))
	val)
    (cond
     ((memq 'auto-expire params)
      t)
     ((setq val (assq 'auto-expire params)) ; (auto-expire . t)
      (cdr val))
     (gnus-auto-expirable-newsgroups	; Check var.
      (string-match gnus-auto-expirable-newsgroups group)))))

(defun gnus-virtual-group-p (group)
  "Say whether GROUP is virtual or not."
  (memq 'virtual (assoc (symbol-name (car (gnus-find-method-for-group group)))
			gnus-valid-select-methods)))

(defun gnus-news-group-p (group &optional article)
  "Return non-nil if GROUP (and ARTICLE) come from a news server."
  (or (gnus-member-of-valid 'post group) ; Ordinary news group.
      (and (gnus-member-of-valid 'post-mail group) ; Combined group.
	   (if (or (null article)
		   (not (< article 0)))
	       (eq (gnus-request-type group article) 'news)
	     (if (not (vectorp article))
		 nil
	       ;; It's a real article.
	       (eq (gnus-request-type group (mail-header-id article))
		   'news))))))

;; Returns a list of writable groups.
(defun gnus-writable-groups ()
  (let ((alist gnus-newsrc-alist)
	groups group)
    (while (setq group (car (pop alist)))
      (unless (gnus-group-read-only-p group)
	(push group groups)))
    (nreverse groups)))

;; Check whether to use long file names.
(defun gnus-use-long-file-name (symbol)
  ;; The variable has to be set...
  (and gnus-use-long-file-name
       ;; If it isn't a list, then we return t.
       (or (not (listp gnus-use-long-file-name))
	   ;; If it is a list, and the list contains `symbol', we
	   ;; return nil.
	   (not (memq symbol gnus-use-long-file-name)))))

;; Generate a unique new group name.
(defun gnus-generate-new-group-name (leaf)
  (let ((name leaf)
	(num 0))
    (while (gnus-gethash name gnus-newsrc-hashtb)
      (setq name (concat leaf "<" (int-to-string (setq num (1+ num))) ">")))
    name))

(defun gnus-ephemeral-group-p (group)
  "Say whether GROUP is ephemeral or not."
  (gnus-group-get-parameter group 'quit-config t))

(defun gnus-group-quit-config (group)
  "Return the quit-config of GROUP."
  (gnus-group-get-parameter group 'quit-config t))

(defun gnus-kill-ephemeral-group (group)
  "Remove ephemeral GROUP from relevant structures."
  (gnus-sethash group nil gnus-newsrc-hashtb))

(defun gnus-simplify-mode-line ()
  "Make mode lines a bit simpler."
  (setq mode-line-modified (cdr gnus-mode-line-modified))
  (when (listp mode-line-format)
    (make-local-variable 'mode-line-format)
    (setq mode-line-format (copy-sequence mode-line-format))
    (when (equal (nth 3 mode-line-format) "   ")
      (setcar (nthcdr 3 mode-line-format) " "))))

;;; Servers and groups.

(defsubst gnus-server-add-address (method)
  (let ((method-name (symbol-name (car method))))
    (if (and (memq 'address (assoc method-name gnus-valid-select-methods))
	     (not (assq (intern (concat method-name "-address")) method))
	     (memq 'physical-address (assq (car method)
					   gnus-valid-select-methods)))
	(append method (list (list (intern (concat method-name "-address"))
				   (nth 1 method))))
      method)))

(defsubst gnus-server-get-method (group method)
  ;; Input either a server name, and extended server name, or a
  ;; select method, and return a select method.
  (cond ((stringp method)
	 (gnus-server-to-method method))
	((equal method gnus-select-method)
	 gnus-select-method)
	((and (stringp (car method))
	      group)
	 (gnus-server-extend-method group method))
	((and method
	      (not group)
	      (equal (cadr method) ""))
	 method)
	(t
	 (gnus-server-add-address method))))

(defun gnus-server-to-method (server)
  "Map virtual server names to select methods."
  (or
   ;; Is this a method, perhaps?
   (and server (listp server) server)
   ;; Perhaps this is the native server?
   (and (equal server "native") gnus-select-method)
   ;; It should be in the server alist.
   (cdr (assoc server gnus-server-alist))
   ;; It could be in the predefined server alist.
   (cdr (assoc server gnus-predefined-server-alist))
   ;; If not, we look through all the opened server
   ;; to see whether we can find it there.
   (let ((opened gnus-opened-servers))
     (while (and opened
		 (not (equal server (format "%s:%s" (caaar opened)
					    (cadaar opened)))))
       (pop opened))
     (caar opened))))

(defmacro gnus-method-equal (ss1 ss2)
  "Say whether two servers are equal."
  `(let ((s1 ,ss1)
	 (s2 ,ss2))
     (or (equal s1 s2)
	 (and (= (length s1) (length s2))
	      (progn
		(while (and s1 (member (car s1) s2))
		  (setq s1 (cdr s1)))
		(null s1))))))

(defun gnus-server-equal (m1 m2)
  "Say whether two methods are equal."
  (let ((m1 (cond ((null m1) gnus-select-method)
		  ((stringp m1) (gnus-server-to-method m1))
		  (t m1)))
	(m2 (cond ((null m2) gnus-select-method)
		  ((stringp m2) (gnus-server-to-method m2))
		  (t m2))))
    (gnus-method-equal m1 m2)))

(defun gnus-servers-using-backend (backend)
  "Return a list of known servers using BACKEND."
  (let ((opened gnus-opened-servers)
	out)
    (while opened
      (when (eq backend (caaar opened))
	(push (caar opened) out))
      (pop opened))
    out))

(defun gnus-archive-server-wanted-p ()
  "Say whether the user wants to use the archive server."
  (cond
   ((or (not gnus-message-archive-method)
	(not gnus-message-archive-group))
    nil)
   ((and gnus-message-archive-method gnus-message-archive-group)
    t)
   (t
    (let ((active (cadr (assq 'nnfolder-active-file
			      gnus-message-archive-method))))
      (and active
	   (file-exists-p active))))))

(defun gnus-group-prefixed-name (group method)
  "Return the whole name from GROUP and METHOD."
  (and (stringp method) (setq method (gnus-server-to-method method)))
  (if (or (not method)
	  (gnus-server-equal method "native"))
      group
    (concat (format "%s" (car method))
	    (when (and
		   (or (assoc (format "%s" (car method))
			      (gnus-methods-using 'address))
		       (gnus-server-equal method gnus-message-archive-method))
		   (nth 1 method)
		   (not (string= (nth 1 method) "")))
	      (concat "+" (nth 1 method)))
	    ":" group)))

(defun gnus-group-real-prefix (group)
  "Return the prefix of the current group name."
  (if (string-match "^[^:]+:" group)
      (substring group 0 (match-end 0))
    ""))

(defun gnus-group-method (group)
  "Return the server or method used for selecting GROUP.
You should probably use `gnus-find-method-for-group' instead."
  (let ((prefix (gnus-group-real-prefix group)))
    (if (equal prefix "")
	gnus-select-method
      (let ((servers gnus-opened-servers)
	    (server "")
	    backend possible found)
	(if (string-match "^[^\\+]+\\+" prefix)
	    (setq backend (intern (substring prefix 0 (1- (match-end 0))))
		  server (substring prefix (match-end 0) (1- (length prefix))))
	  (setq backend (intern (substring prefix 0 (1- (length prefix))))))
	(while servers
	  (when (eq (caaar servers) backend)
	    (setq possible (caar servers))
	    (when (equal (cadaar servers) server)
	      (setq found (caar servers))))
	  (pop servers))
	(or (car (rassoc found gnus-server-alist))
	    found
	    (car (rassoc possible gnus-server-alist))
	    possible
	    (list backend server))))))

(defsubst gnus-secondary-method-p (method)
  "Return whether METHOD is a secondary select method."
  (let ((methods gnus-secondary-select-methods)
	(gmethod (gnus-server-get-method nil method)))
    (while (and methods
		(not (equal (gnus-server-get-method nil (car methods))
			    gmethod)))
      (setq methods (cdr methods)))
    methods))

(defun gnus-groups-from-server (server)
  "Return a list of all groups that are fetched from SERVER."
  (let ((alist (cdr gnus-newsrc-alist))
	info groups)
    (while (setq info (pop alist))
      (when (gnus-server-equal (gnus-info-method info) server)
	(push (gnus-info-group info) groups)))
    (sort groups 'string<)))

(defun gnus-group-foreign-p (group)
  "Say whether a group is foreign or not."
  (and (not (gnus-group-native-p group))
       (not (gnus-group-secondary-p group))))

(defun gnus-group-native-p (group)
  "Say whether the group is native or not."
  (not (string-match ":" group)))

(defun gnus-group-secondary-p (group)
  "Say whether the group is secondary or not."
  (gnus-secondary-method-p (gnus-find-method-for-group group)))

(defun gnus-group-find-parameter (group &optional symbol allow-list)
  "Return the group parameters for GROUP.
If SYMBOL, return the value of that symbol in the group parameters."
  (save-excursion
    (set-buffer gnus-group-buffer)
    (let ((parameters (funcall gnus-group-get-parameter-function group)))
      (if symbol
	  (gnus-group-parameter-value parameters symbol allow-list)
	parameters))))

(defun gnus-group-get-parameter (group &optional symbol allow-list)
  "Return the group parameters for GROUP.
If SYMBOL, return the value of that symbol in the group parameters.
Most functions should use `gnus-group-find-parameter', which
also examines the topic parameters."
  (let ((params (gnus-info-params (gnus-get-info group))))
    (if symbol
	(gnus-group-parameter-value params symbol allow-list)
      params)))

(defun gnus-group-parameter-value (params symbol &optional allow-list)
  "Return the value of SYMBOL in group PARAMS."
  ;; We only wish to return group parameters (dotted lists) and
  ;; not local variables, which may have the same names.
  ;; But first we handle single elements...
  (or (car (memq symbol params))
      ;; Handle alist.
      (let (elem)
	(catch 'found
	  (while (setq elem (pop params))
	    (when (and (consp elem)
		       (eq (car elem) symbol)
		       (or allow-list
			   (atom (cdr elem))))
	      (throw 'found (cdr elem))))))))

(defun gnus-group-add-parameter (group param)
  "Add parameter PARAM to GROUP."
  (let ((info (gnus-get-info group)))
    (when info
      (gnus-group-remove-parameter group (if (consp param) (car param) param))
      ;; Cons the new param to the old one and update.
      (gnus-group-set-info (cons param (gnus-info-params info))
			   group 'params))))

(defun gnus-group-set-parameter (group name value)
  "Set parameter NAME to VALUE in GROUP."
  (let ((info (gnus-get-info group)))
    (when info
      (gnus-group-remove-parameter group name)
      (let ((old-params (gnus-info-params info))
	    (new-params (list (cons name value))))
	(while old-params
	  (when (or (not (listp (car old-params)))
		    (not (eq (caar old-params) name)))
	    (setq new-params (append new-params (list (car old-params)))))
	  (setq old-params (cdr old-params)))
	(gnus-group-set-info new-params group 'params)))))

(defun gnus-group-remove-parameter (group name)
  "Remove parameter NAME from GROUP."
  (let ((info (gnus-get-info group)))
    (when info
      (let ((params (gnus-info-params info)))
	(when params
	  (setq params (delq name params))
	  (while (assq name params)
	    (gnus-pull name params))
	  (gnus-info-set-params info params))))))

(defun gnus-group-add-score (group &optional score)
  "Add SCORE to the GROUP score.
If SCORE is nil, add 1 to the score of GROUP."
  (let ((info (gnus-get-info group)))
    (when info
      (gnus-info-set-score info (+ (gnus-info-score info) (or score 1))))))

;; Function written by Stainless Steel Rat <ratinox@peorth.gweep.net>
(defun gnus-short-group-name (group &optional levels)
  "Collapse GROUP name LEVELS.
Select methods are stripped and any remote host name is stripped down to
just the host name."
  (let* ((name "")
	 (foreign "")
	 (depth 0)
	 (skip 1)
	 (levels (or levels
		     (progn
		       (while (string-match "\\." group skip)
			 (setq skip (match-end 0)
			       depth (+ depth 1)))
		       depth))))
    ;; separate foreign select method from group name and collapse.
    ;; if method contains a server, collapse to non-domain server name,
    ;; otherwise collapse to select method
    (when (string-match ":" group)
      (cond ((string-match "+" group)
	     (let* ((plus (string-match "+" group))
		    (colon (string-match ":" group (or plus 0)))
		    (dot (string-match "\\." group)))
	       (setq foreign (concat
			      (substring group (+ 1 plus)
					 (cond ((null dot) colon)
					       ((< colon dot) colon)
					       ((< dot colon) dot)))
			      ":")
		     group (substring group (+ 1 colon)))))
	    (t
	     (let* ((colon (string-match ":" group)))
	       (setq foreign (concat (substring group 0 (+ 1 colon)))
		     group (substring group (+ 1 colon)))))))
    ;; collapse group name leaving LEVELS uncollapsed elements
    (while group
      (if (and (string-match "\\." group) (> levels 0))
	  (setq name (concat name (substring group 0 1))
		group (substring group (match-end 0))
		levels (- levels 1)
		name (concat name "."))
	(setq name (concat foreign name group)
	      group nil)))
    name))

(defun gnus-narrow-to-body ()
  "Narrow to the body of an article."
  (narrow-to-region
   (progn
     (goto-char (point-min))
     (or (search-forward "\n\n" nil t)
	 (point-max)))
   (point-max)))


;;;
;;; Kill file handling.
;;;

(defun gnus-apply-kill-file ()
  "Apply a kill file to the current newsgroup.
Returns the number of articles marked as read."
  (if (or (file-exists-p (gnus-newsgroup-kill-file nil))
	  (file-exists-p (gnus-newsgroup-kill-file gnus-newsgroup-name)))
      (gnus-apply-kill-file-internal)
    0))

(defun gnus-kill-save-kill-buffer ()
  (let ((file (gnus-newsgroup-kill-file gnus-newsgroup-name)))
    (when (get-file-buffer file)
      (save-excursion
	(set-buffer (get-file-buffer file))
	(when (buffer-modified-p)
	  (save-buffer))
	(kill-buffer (current-buffer))))))

(defcustom gnus-kill-file-name "KILL"
  "Suffix of the kill files."
  :group 'gnus-score-kill
  :group 'gnus-score-files
  :type 'string)

(defun gnus-newsgroup-kill-file (newsgroup)
  "Return the name of a kill file name for NEWSGROUP.
If NEWSGROUP is nil, return the global kill file name instead."
  (cond
   ;; The global KILL file is placed at top of the directory.
   ((or (null newsgroup)
	(string-equal newsgroup ""))
    (expand-file-name gnus-kill-file-name
		      gnus-kill-files-directory))
   ;; Append ".KILL" to newsgroup name.
   ((gnus-use-long-file-name 'not-kill)
    (expand-file-name (concat (gnus-newsgroup-savable-name newsgroup)
			      "." gnus-kill-file-name)
		      gnus-kill-files-directory))
   ;; Place "KILL" under the hierarchical directory.
   (t
    (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
			      "/" gnus-kill-file-name)
		      gnus-kill-files-directory))))

;;; Server things.

(defun gnus-member-of-valid (symbol group)
  "Find out if GROUP has SYMBOL as part of its \"valid\" spec."
  (memq symbol (assoc
		(symbol-name (car (gnus-find-method-for-group group)))
		gnus-valid-select-methods)))

(defun gnus-method-option-p (method option)
  "Return non-nil if select METHOD has OPTION as a parameter."
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (memq option (assoc (format "%s" (car method))
		      gnus-valid-select-methods)))

(defun gnus-similar-server-opened (method)
  (let ((opened gnus-opened-servers))
    (while (and method opened)
      (when (and (equal (cadr method) (cadaar opened))
		 (not (equal method (caar opened))))
	(setq method nil))
      (pop opened))
    (not method)))

(defun gnus-server-extend-method (group method)
  ;; This function "extends" a virtual server.	If the server is
  ;; "hello", and the select method is ("hello" (my-var "something"))
  ;; in the group "alt.alt", this will result in a new virtual server
  ;; called "hello+alt.alt".
  (if (or (not (inline (gnus-similar-server-opened method)))
	  (not (cddr method)))
      method
    `(,(car method) ,(concat (cadr method) "+" group)
      (,(intern (format "%s-address" (car method))) ,(cadr method))
      ,@(cddr method))))

(defun gnus-server-status (method)
  "Return the status of METHOD."
  (nth 1 (assoc method gnus-opened-servers)))

(defun gnus-group-name-to-method (group)
  "Guess a select method based on GROUP."
  (if (string-match ":" group)
      (let ((server (substring group 0 (match-beginning 0))))
	(if (string-match "\\+" server)
	    (list (intern (substring server 0 (match-beginning 0)))
		  (substring server (match-end 0)))
	  (list (intern server) "")))
    gnus-select-method))

(defun gnus-find-method-for-group (group &optional info)
  "Find the select method that GROUP uses."
  (or gnus-override-method
      (and (not group)
	   gnus-select-method)
      (let ((info (or info (gnus-get-info group)))
	    method)
	(if (or (not info)
		(not (setq method (gnus-info-method info)))
		(equal method "native"))
	    gnus-select-method
	  (setq method
		(cond ((stringp method)
		       (inline (gnus-server-to-method method)))
		      ((stringp (cadr method))
		       (inline (gnus-server-extend-method group method)))
		      (t
		       method)))
	  (cond ((equal (cadr method) "")
		 method)
		((null (cadr method))
		 (list (car method) ""))
		(t
		 (gnus-server-add-address method)))))))

(defun gnus-methods-using (feature)
  "Find all methods that have FEATURE."
  (let ((valids gnus-valid-select-methods)
	outs)
    (while valids
      (when (memq feature (car valids))
	(push (car valids) outs))
      (setq valids (cdr valids)))
    outs))

(defun gnus-read-group (prompt &optional default)
  "Prompt the user for a group name.
Disallow illegal group names."
  (let ((prefix "")
	group)
    (while (not group)
      (when (string-match
	     "[: `'\"/]\\|^$"
	     (setq group (read-string (concat prefix prompt)
				      (cons (or default "") 0)
				      'gnus-group-history)))
	(setq prefix (format "Illegal group name: \"%s\".  " group)
	      group nil)))
    group))

(defun gnus-read-method (prompt)
  "Prompt the user for a method.
Allow completion over sensible values."
  (let* ((servers
	  (append gnus-valid-select-methods
		  gnus-predefined-server-alist
		  gnus-server-alist))
	 (method
	  (completing-read
	   prompt servers
	   nil t nil 'gnus-method-history)))
    (cond
     ((equal method "")
      (setq method gnus-select-method))
     ((assoc method gnus-valid-select-methods)
      (list (intern method)
	    (if (memq 'prompt-address
		      (assoc method gnus-valid-select-methods))
		(read-string "Address: ")
	      "")))
     ((assoc method servers)
      method)
     (t
      (list (intern method) "")))))

;;; User-level commands.

;;;###autoload
(defun gnus-slave-no-server (&optional arg)
  "Read network news as a slave, without connecting to local server."
  (interactive "P")
  (gnus-no-server arg t))

;;;###autoload
(defun gnus-no-server (&optional arg slave)
  "Read network news.
If ARG is a positive number, Gnus will use that as the
startup level.	If ARG is nil, Gnus will be started at level 2.
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server."
  (interactive "P")
  (gnus-no-server-1 arg slave))

;;;###autoload
(defun gnus-slave (&optional arg)
  "Read news as a slave."
  (interactive "P")
  (gnus arg nil 'slave))

;;;###autoload
(defun gnus-other-frame (&optional arg)
  "Pop up a frame to read news."
  (interactive "P")
  (let ((window (get-buffer-window gnus-group-buffer)))
    (cond (window
	   (select-frame (window-frame window)))
	  ((= (length (frame-list)) 1)
	   (select-frame (make-frame)))
	  (t
	   (other-frame 1))))
  (gnus arg))

;;;###autoload
(defun gnus (&optional arg dont-connect slave)
  "Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.	If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use."
  (interactive "P")
  (gnus-1 arg dont-connect slave))

;; Allow redefinition of Gnus functions.

(gnus-ems-redefine)

(provide 'gnus)

;;; gnus.el ends here
