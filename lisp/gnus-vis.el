;;; gnus-vis.el --- display-oriented parts of Gnus

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; 	Per Abrahamsen <abraham@iesd.auc.dk>
;; Keywords: news

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

;;; Code:

(require 'gnus)
(require 'gnus-ems)
(require 'easymenu)
(require 'custom)

(defvar gnus-group-menu-hook nil
  "*Hook run after the creation of the group mode menu.")

(defvar gnus-summary-menu-hook nil
  "*Hook run after the creation of the summary mode menu.")

(defvar gnus-article-menu-hook nil
  "*Hook run after the creation of the article mode menu.")

(defvar gnus-server-menu-hook nil
  "*Hook run after the creation of the server mode menu.")

(defvar gnus-browse-menu-hook nil
  "*Hook run after the creation of the browse mode menu.")
  
;;; Summary highlights.

;(defvar gnus-summary-highlight-properties
;  '((unread "ForestGreen" "green")
;    (ticked "Firebrick" "pink")
;    (read "black" "white")
;    (low italic italic)
;    (high bold bold)
;    (canceled "yellow/black" "black/yellow")))

;(defvar gnus-summary-highlight-translation
;  '(((unread (= mark gnus-unread-mark))
;     (ticked (or (= mark gnus-ticked-mark) (= mark gnus-dormant-mark)))
;     (read (not (or (= mark gnus-unread-mark) (= mark gnus-dormant-mark)
;		    (= mark gnus-ticked-mark) (= mark gnus-canceled-mark))))
;     (canceled (= mark gnus-canceled-mark)))
;    ((low (< score gnus-summary-default-score))
;     (high (> score gnus-summary-default-score)))))

;(defun gnus-visual-map-face-translation ()
;  (let ((props gnus-summary-highlight-properties)
;	(trans gnus-summary-highlight-translation)
;	map)
;    (while props)))
      
;see gnus-cus.el
;(defvar gnus-summary-selected-face 'underline
;  "*Face used for highlighting the current article in the summary buffer.")
 
;see gnus-cus.el
;(defvar gnus-summary-highlight
;  (cond ((not (eq gnus-display-type 'color))
;	 '(((> score default) . bold)
;	   ((< score default) . italic)))
;	((eq gnus-background-mode 'dark)
;	 (list (cons '(= mark gnus-canceled-mark)
;		     (custom-face-lookup "yellow" "black" nil nil nil nil))
;	       (cons '(and (> score default) 
;			   (or (= mark gnus-dormant-mark)
;			       (= mark gnus-ticked-mark)))
;		     (custom-face-lookup "pink" nil nil t nil nil))
;	       (cons '(and (< score default) 
;			   (or (= mark gnus-dormant-mark)
;			       (= mark gnus-ticked-mark)))
;		     (custom-face-lookup "pink" nil nil nil t nil))
;	       (cons '(or (= mark gnus-dormant-mark)
;			  (= mark gnus-ticked-mark))
;		     (custom-face-lookup "pink" nil nil nil nil nil))

;	       (cons '(and (> score default) (= mark gnus-ancient-mark))
;		     (custom-face-lookup "SkyBlue" nil nil t nil nil))
;	       (cons '(and (< score default) (= mark gnus-ancient-mark))
;		     (custom-face-lookup "SkyBlue" nil nil nil t nil))
;	       (cons '(= mark gnus-ancient-mark)
;		     (custom-face-lookup "SkyBlue" nil nil nil nil nil))

;	       (cons '(and (> score default) (= mark gnus-unread-mark))
;		     (custom-face-lookup "white" nil nil t nil nil))
;	       (cons '(and (< score default) (= mark gnus-unread-mark))
;		     (custom-face-lookup "white" nil nil nil t nil))
;	       (cons '(= mark gnus-unread-mark)
;		     (custom-face-lookup "white" nil nil nil nil nil))

;	       (cons '(> score default) 'bold)
;	       (cons '(< score default) 'italic)))
;	(t
;	 (list (cons '(= mark gnus-canceled-mark)
;		     (custom-face-lookup "yellow" "black" nil nil nil nil))
;	       (cons '(and (> score default) 
;			   (or (= mark gnus-dormant-mark)
;			       (= mark gnus-ticked-mark)))
;		     (custom-face-lookup "firebrick" nil nil t nil nil))
;	       (cons '(and (< score default) 
;			   (or (= mark gnus-dormant-mark)
;			       (= mark gnus-ticked-mark)))
;		     (custom-face-lookup "firebrick" nil nil nil t nil))
;	       (cons '(or (= mark gnus-dormant-mark)
;			  (= mark gnus-ticked-mark))
;		     (custom-face-lookup "firebrick" nil nil nil nil nil))

;	       (cons '(and (> score default) (= mark gnus-ancient-mark))
;		     (custom-face-lookup "RoyalBlue" nil nil t nil nil))
;	       (cons '(and (< score default) (= mark gnus-ancient-mark))
;		     (custom-face-lookup "RoyalBlue" nil nil nil t nil))
;	       (cons '(= mark gnus-ancient-mark)
;		     (custom-face-lookup "RoyalBlue" nil nil nil nil nil))

;	       (cons '(and (> score default) (/= mark gnus-unread-mark))
;		     (custom-face-lookup "DarkGreen" nil nil t nil nil))
;	       (cons '(and (< score default) (/= mark gnus-unread-mark))
;		     (custom-face-lookup "DarkGreen" nil nil nil t nil))
;	       (cons '(/= mark gnus-unread-mark)
;		     (custom-face-lookup "DarkGreen" nil nil nil nil nil))

;	       (cons '(> score default) 'bold)
;	       (cons '(< score default) 'italic))))
;  "*Alist of `(FORM . FACE)'.
;Summary lines are highlighted with the FACE for the first FORM which
;evaluate to a non-nil value.  

;Point will be at the beginning of the line when FORM is evaluated.
;The following can be used for convenience:

;score:   (gnus-summary-article-score)
;default: gnus-summary-default-score
;below:   gnus-summary-mark-below
;mark:    (gnus-summary-article-mark)

;The latter can be used like this:
;   ((= mark gnus-replied-mark) . underline)")

;;; article highlights

;see gnus-cus.el
;(defvar gnus-header-face-alist 
;  (cond ((not (eq gnus-display-type 'color))
;	 '(("" bold italic)))
;	((eq gnus-background-mode 'dark)
;	 (list (list "From" nil 
;		     (custom-face-lookup "SkyBlue" nil nil t t nil))
;	       (list "Subject" nil 
;		     (custom-face-lookup "pink" nil nil t t nil))
;	       (list "Newsgroups:.*," nil
;		     (custom-face-lookup "yellow" nil nil t t nil))
;	       (list "" 
;		     (custom-face-lookup "cyan" nil nil t nil nil)
;		     (custom-face-lookup "green" nil nil nil t nil))))
;	(t
;	 (list (list "From" nil 
;		     (custom-face-lookup "RoyalBlue" nil nil t t nil))
;	       (list "Subject" nil 
;		     (custom-face-lookup "firebrick" nil nil t t nil))
;	       (list "Newsgroups:.*," nil
;		     (custom-face-lookup "red" nil nil t t nil))
;	       (list ""
;		     (custom-face-lookup "DarkGreen" nil nil t nil nil)
;		     (custom-face-lookup "DarkGreen" nil nil nil t nil)))))
;  "Alist of headers and faces used for highlighting them.
;The entries in the list has the form `(REGEXP NAME CONTENT)', where
;REGEXP is a regular expression matching the beginning of the header,
;NAME is the face used for highlighting the header name and CONTENT is
;the face used for highlighting the header content. 

;The first non-nil NAME or CONTENT with a matching REGEXP in the list
;will be used.")


;see gnus-cus.el
;(defvar gnus-make-foreground t
;  "Non nil means foreground color to highlight citations.")

;see gnus-cus.el
;(defvar gnus-article-button-face 'bold
;  "Face used for text buttons.")

;see gnus-cus.el
;(defvar gnus-article-mouse-face (if (boundp 'gnus-mouse-face)
;				    gnus-mouse-face
;				  'highlight)
;  "Face used when the mouse is over the button.")

;see gnus-cus.el
;(defvar gnus-signature-face 'italic
;  "Face used for signature.")

(defvar gnus-button-alist 
  '(("in\\( +article\\)? +\\(<\\([^\n @<>]+@[^\n @<>]+\\)>\\)" 2 
     (assq (count-lines (point-min) (match-end 0)) 
	   gnus-cite-attribution-alist)
     gnus-button-message-id 3)
    ;; This is how URLs _should_ be embedded in text...
    ("<URL:\\([^\n\r>]*\\)>" 0 t gnus-button-url 1)
    ;; Next regexp stolen from highlight-headers.el.
    ;; Modified by Vladimir Alexiev.
    ("\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]" 0 t gnus-button-url 0))
  "Alist of regexps matching buttons in an article.

Each entry has the form (REGEXP BUTTON FORM CALLBACK PAR...), where
REGEXP: is the string matching text around the button,
BUTTON: is the number of the regexp grouping actually matching the button,
FORM: is a lisp expression which must eval to true for the button to
be added, 
CALLBACK: is the function to call when the user push this button, and each
PAR: is a number of a regexp grouping whose text will be passed to CALLBACK.

CALLBACK can also be a variable, in that case the value of that
variable it the real callback function.")

;see gnus-cus.el
;(eval-when-compile
;  (defvar browse-url-browser-function))

;see gnus-cus.el
;(defvar gnus-button-url
;  (cond ((boundp 'browse-url-browser-function) browse-url-browser-function)
;	((fboundp 'w3-fetch) 'w3-fetch)
;	((eq window-system 'x) 'gnus-netscape-open-url))
;  "*Function to fetch URL.
;The function will be called with one argument, the URL to fetch.
;Useful values of this function are:

;w3-fetch: 
;   defined in the w3 emacs package by William M. Perry.
;gnus-netscape-open-url:
;   open url in existing netscape, start netscape if none found.
;gnus-netscape-start-url:
;   start new netscape with url.")



(eval-and-compile
  (autoload 'nnkiboze-generate-groups "nnkiboze")
  (autoload 'gnus-cite-parse-maybe "gnus-cite" nil t))

;;;
;;; gnus-menu
;;;

(defun gnus-visual-turn-off-edit-menu (type)
  (define-key (symbol-value (intern (format "gnus-%s-mode-map" type)))
    [menu-bar edit] 'undefined))

;; Newsgroup buffer

(defun gnus-group-make-menu-bar ()
  (gnus-visual-turn-off-edit-menu 'group)
  (or 
   (boundp 'gnus-group-reading-menu)
   (progn
     (easy-menu-define
      gnus-group-reading-menu
      gnus-group-mode-map
      ""
      '("Group"
	["Read" gnus-group-read-group t]
	["Select" gnus-group-select-group t]
	["See old articles" gnus-group-select-group-all t]
	["Catch up" gnus-group-catchup-current t]
	["Catch up all articles" gnus-group-catchup-current-all t]
	["Check for new articles" gnus-group-get-new-news-this-group t]
	["Toggle subscription" gnus-group-unsubscribe-current-group t]
	["Kill" gnus-group-kill-group t]
	["Yank" gnus-group-yank-group t]
	["Describe" gnus-group-describe-group t]
	["Fetch FAQ" gnus-group-fetch-faq t]
	["Edit kill file" gnus-group-edit-local-kill t]
	["Expire articles" gnus-group-expire-articles t]
	["Set group level" gnus-group-set-current-level t]
	))
  
     (easy-menu-define
      gnus-group-group-menu
      gnus-group-mode-map
      ""
      '("Groups"
	("Listing"
	 ["List subscribed groups" gnus-group-list-groups t]
	 ["List all groups" gnus-group-list-all-groups t]
	 ["List groups matching..." gnus-group-list-matching t]
	 ["List killed groups" gnus-group-list-killed t]
	 ["List zombie groups" gnus-group-list-zombies t]
	 ["Describe all groups" gnus-group-describe-all-groups t]
	 ["Group apropos" gnus-group-apropos t]
	 ["Group and description apropos" gnus-group-description-apropos t]
	 ["List groups matching..." gnus-group-list-matching t])
	("Mark"
	 ["Mark group" gnus-group-mark-group t]
	 ["Unmark group" gnus-group-unmark-group t]
	 ["Mark region" gnus-group-mark-region t])
	("Subscribe"
	 ["Subscribe to random group" gnus-group-unsubscribe-group t]
	 ["Kill all newsgroups in region" gnus-group-kill-region t]
	 ["Kill all zombie groups" gnus-group-kill-all-zombies t])
	("Foreign groups"
	 ["Make a foreign group" gnus-group-make-group t]
	 ["Add a directory group" gnus-group-make-directory-group t]
	 ["Add the help group" gnus-group-make-help-group t]
	 ["Add the archive group" gnus-group-make-archive-group t]
	 ["Make a doc group" gnus-group-make-doc-group t]
	 ["Make a kiboze group" gnus-group-make-kiboze-group t]
	 ["Make a virtual group" gnus-group-make-empty-virtual t]
	 ["Add a group to a virtual" gnus-group-add-to-virtual t])
	("Editing groups"
	 ["Parameters" gnus-group-edit-group-parameters t]
	 ["Select method" gnus-group-edit-group-method t]
	 ["Info" gnus-group-edit-group t])
	["Read a directory as a group" gnus-group-enter-directory t]
	["Jump to group" gnus-group-jump-to-group t]
	["Best unread group" gnus-group-best-unread-group t]
	))

     (easy-menu-define
      gnus-group-misc-menu
      gnus-group-mode-map
      ""
      '("Misc"
	["Send a bug report" gnus-bug t]
	["Send a mail" gnus-group-mail t]
	["Post an article" gnus-group-post-news t]
	["Customize score file" gnus-score-customize 
	 (not (string-match "XEmacs" emacs-version)) ]
	["Check for new news" gnus-group-get-new-news t]     
	["Delete bogus groups" gnus-group-check-bogus-groups t]
	["Find new newsgroups" gnus-find-new-newsgroups t]
	["Restart Gnus" gnus-group-restart t]
	["Read init file" gnus-group-read-init-file t]
	["Browse foreign server" gnus-group-browse-foreign-server t]
	["Enter server buffer" gnus-group-enter-server-mode t]
	["Expire expirable articles" gnus-group-expire-all-groups t]
	["Generate any kiboze groups" nnkiboze-generate-groups t]
	["Gnus version" gnus-version t]
	["Save .newsrc files" gnus-group-save-newsrc t]
	["Suspend Gnus" gnus-group-suspend t]
	["Clear dribble buffer" gnus-group-clear-dribble t]
	["Exit from Gnus" gnus-group-exit t]
	["Exit without saving" gnus-group-quit t]
	["Edit global kill file" gnus-group-edit-global-kill t]
	["Sort group buffer" gnus-group-sort-groups t]
	))
     (run-hooks 'gnus-group-menu-hook)
     )))

;; Server mode
(defun gnus-server-make-menu-bar ()
  (gnus-visual-turn-off-edit-menu 'server)
  (or
   (boundp 'gnus-server-menu)
   (progn
     (easy-menu-define
      gnus-server-menu
      gnus-server-mode-map
      ""
      '("Server"
	["Add" gnus-server-add-server t]
	["Browse" gnus-server-read-server t]
	["List" gnus-server-list-servers t]
	["Kill" gnus-server-kill-server t]
	["Yank" gnus-server-yank-server t]
	["Copy" gnus-server-copy-server t]
	["Edit" gnus-server-edit-server t]
	["Exit" gnus-server-exit t]
	))
     (run-hooks 'gnus-server-menu-hook)
     )))

;; Browse mode
(defun gnus-browse-make-menu-bar ()
  (gnus-visual-turn-off-edit-menu 'browse)
  (or
   (boundp 'gnus-browse-menu)
   (progn
     (easy-menu-define
      gnus-browse-menu
      gnus-browse-mode-map
      ""
      '("Browse"
	["Subscribe" gnus-browse-unsubscribe-current-group t]
	["Read" gnus-group-read-group t]
	["Exit" gnus-browse-exit t]
	))
      (run-hooks 'gnus-browse-menu-hook)
      )))


;; Summary buffer
(defun gnus-summary-make-menu-bar ()
  (gnus-visual-turn-off-edit-menu 'summary)

  (or
   (boundp 'gnus-summary-misc-menu)
   (progn

     (easy-menu-define
      gnus-summary-misc-menu
      gnus-summary-mode-map
      ""
      '("Misc"
	("Mark"
	 ("Read"
	  ["Mark as read" gnus-summary-mark-as-read-forward t]
	  ["Mark same subject and select" gnus-summary-kill-same-subject-and-select t]
	  ["Mark same subject" gnus-summary-kill-same-subject t]
	  ["Catchup" gnus-summary-catchup t]
	  ["Catchup all" gnus-summary-catchup-all t]
	  ["Catchup to here" gnus-summary-catchup-to-here t]
	  ["Catchup region" gnus-summary-mark-region-as-read t])
	 ("Various"
	  ["Tick" gnus-summary-tick-article-forward t]
	  ["Mark as dormant" gnus-summary-mark-as-dormant t]
	  ["Remove marks" gnus-summary-clear-mark-forward t]
	  ["Set expirable mark" gnus-summary-mark-as-expirable t]
	  ["Set bookmark" gnus-summary-set-bookmark t]
	  ["Remove bookmark" gnus-summary-remove-bookmark t])
	 ("Display"
	  ["Remove lines marked as read" gnus-summary-remove-lines-marked-as-read t]
	  ["Remove lines marked with..." gnus-summary-remove-lines-marked-with t]
	  ["Show dormant articles" gnus-summary-show-all-dormant t]
	  ["Hide dormant articles" gnus-summary-hide-all-dormant t]
	  ["Show expunged articles" gnus-summary-show-all-expunged t])
	 ("Process mark"
	  ["Set mark" gnus-summary-mark-as-processable t]
	  ["Remove mark" gnus-summary-unmark-as-processable t]
	  ["Remove all marks" gnus-summary-unmark-all-processable t]
	  ["Mark series" gnus-uu-mark-series t]
	  ["Mark region" gnus-uu-mark-region t]
	  ["Mark by regexp" gnus-uu-mark-by-regexp t]
	  ["Mark all" gnus-uu-mark-all t]
	  ["Mark sparse" gnus-uu-mark-sparse t]
	  ["Mark thread" gnus-uu-mark-thread t]))
	("Move"
	 ["Scroll article forwards" gnus-summary-next-page t]
	 ["Next unread article" gnus-summary-next-unread-article t]
	 ["Previous unread article" gnus-summary-prev-unread-article t]
	 ["Next article" gnus-summary-next-article t]
	 ["Previous article" gnus-summary-prev-article t]
	 ["Next article same subject" gnus-summary-next-same-subject t]
	 ["Previous article same subject" gnus-summary-prev-same-subject t]
	 ["First unread article" gnus-summary-first-unread-article t]
	 ["Go to subject number..." gnus-summary-goto-subject t]
	 ["Go to the last article" gnus-summary-goto-last-article t]
	 ["Pop article off history" gnus-summary-pop-article t])	
	("Sort"
	 ["Sort by number" gnus-summary-sort-by-number t]
	 ["Sort by author" gnus-summary-sort-by-author t]
	 ["Sort by subject" gnus-summary-sort-by-subject t]
	 ["Sort by date" gnus-summary-sort-by-date t]
	 ["Sort by score" gnus-summary-sort-by-score t])
	("Exit"
	 ["Catchup and exit" gnus-summary-catchup-and-exit t]
	 ["Catchup and goto next" gnus-summary-catchup-and-goto-next-group t]
	 ["Exit group" gnus-summary-exit t]
	 ["Exit group without updating" gnus-summary-exit-no-update t]
	 ["Reselect group" gnus-summary-reselect-current-group t]
	 ["Rescan group" gnus-summary-rescan-group t])
	["Fetch group FAQ" gnus-summary-fetch-faq t]
	["Filter articles" gnus-summary-execute-command t]
	["Toggle line truncation" gnus-summary-toggle-truncation t]
	["Expire expirable articles" gnus-summary-expire-articles t]
	["Describe group" gnus-summary-describe-group t]
	["Edit local kill file" gnus-summary-edit-local-kill t]
	))

     (easy-menu-define
      gnus-summary-kill-menu
      gnus-summary-mode-map
      ""
      (cons
       "Score"
       (nconc
	(list
	 ["Enter score" gnus-summary-score-entry t])
	(gnus-visual-score-map 'increase)
	(gnus-visual-score-map 'lower)
	'(["Current score" gnus-summary-current-score t]
	  ["Set score" gnus-summary-set-score t]
	  ["Customize score file" gnus-score-customize t]
	  ["Switch current score file" gnus-score-change-score-file t]
	  ["Set mark below" gnus-score-set-mark-below t]
	  ["Set expunge below" gnus-score-set-expunge-below t]
	  ["Edit current score file" gnus-score-edit-alist t]
	  ["Edit score file" gnus-score-edit-file t]
	  ["Trace score" gnus-score-find-trace t]
	  ["Increase score" gnus-summary-increase-score t]
	  ["Lower score" gnus-summary-lower-score t]))))

     (and nil
	  '(("Default header"
	     ["Ask" (gnus-score-set-default 'gnus-score-default-header nil)
	      :style radio 
	      :selected (null gnus-score-default-header)]
	     ["From" (gnus-score-set-default 'gnus-score-default-header 'a)
	      :style radio 
	      :selected (eq gnus-score-default-header 'a )]
	     ["Subject" (gnus-score-set-default 'gnus-score-default-header 's)
	      :style radio 
	      :selected (eq gnus-score-default-header 's )]
	     ["Article body"
	      (gnus-score-set-default 'gnus-score-default-header 'b)
	      :style radio 
	      :selected (eq gnus-score-default-header 'b )]
	     ["All headers"
	      (gnus-score-set-default 'gnus-score-default-header 'h)
	      :style radio 
	      :selected (eq gnus-score-default-header 'h )]
	     ["Message-Id" (gnus-score-set-default 'gnus-score-default-header 'i)
	      :style radio 
	      :selected (eq gnus-score-default-header 'i )]
	     ["Thread" (gnus-score-set-default 'gnus-score-default-header 't)
	      :style radio 
	      :selected (eq gnus-score-default-header 't )]
	     ["Crossposting"
	      (gnus-score-set-default 'gnus-score-default-header 'x)
	      :style radio 
	      :selected (eq gnus-score-default-header 'x )]
	     ["Lines" (gnus-score-set-default 'gnus-score-default-header 'l)
	      :style radio 
	      :selected (eq gnus-score-default-header 'l )]
	     ["Date" (gnus-score-set-default 'gnus-score-default-header 'd)
	      :style radio 
	      :selected (eq gnus-score-default-header 'd )]
	     ["Followups to author"
	      (gnus-score-set-default 'gnus-score-default-header 'f)
	      :style radio 
	      :selected (eq gnus-score-default-header 'f )])
	    ("Default type"
	     ["Ask" (gnus-score-set-default 'gnus-score-default-type nil)
	      :style radio 
	      :selected (null gnus-score-default-type)]
	     ;; The `:active' key is commented out in the following,
	     ;; because the GNU Emacs hack to support radio buttons use
	     ;; active to indicate which button is selected.  
	     ["Substring" (gnus-score-set-default 'gnus-score-default-type 's)
	      :style radio 
	      ;; :active (not (memq gnus-score-default-header '(l d)))
	      :selected (eq gnus-score-default-type 's)]
	     ["Regexp" (gnus-score-set-default 'gnus-score-default-type 'r)
	      :style radio
	      ;; :active (not (memq gnus-score-default-header '(l d)))
	      :selected (eq gnus-score-default-type 'r)]
	     ["Exact" (gnus-score-set-default 'gnus-score-default-type 'e)
	      :style radio
	      ;; :active (not (memq gnus-score-default-header '(l d)))
	      :selected (eq gnus-score-default-type 'e)]
	     ["Fuzzy" (gnus-score-set-default 'gnus-score-default-type 'f)
	      :style radio 
	      ;; :active (not (memq gnus-score-default-header '(l d)))
	      :selected (eq gnus-score-default-type 'f)]
	     ["Before date" (gnus-score-set-default 'gnus-score-default-type 'b)
	      :style radio 
	      ;; :active (eq (gnus-score-default-header 'd))
	      :selected (eq gnus-score-default-type 'b)]
	     ["At date" (gnus-score-set-default 'gnus-score-default-type 'n)
	      :style radio 
	      ;; :active (eq (gnus-score-default-header 'd))
	      :selected (eq gnus-score-default-type 'n)]
	     ["After date" (gnus-score-set-default 'gnus-score-default-type 'a)
	      :style radio 
	      ;; :active (eq (gnus-score-default-header 'd))
	      :selected (eq gnus-score-default-type 'a)]
	     ["Less than number"
	      (gnus-score-set-default 'gnus-score-default-type '<)
	      :style radio 
	      ;; :active (eq (gnus-score-default-header 'l))
	      :selected (eq gnus-score-default-type '<)]
	     ["Equal to number"
	      (gnus-score-set-default 'gnus-score-default-type '=)
	      :style radio 
	      ;; :active (eq (gnus-score-default-header 'l))
	      :selected (eq gnus-score-default-type '=)]
	     ["Greater than number" 
	      (gnus-score-set-default 'gnus-score-default-type '>)
	      :style radio 
	      ;; :active (eq (gnus-score-default-header 'l))
	      :selected (eq gnus-score-default-type '>)])
	    ["Default fold" gnus-score-default-fold-toggle
	     :style toggle
	     :selected gnus-score-default-fold]
	    ("Default duration"
	     ["Ask" (gnus-score-set-default 'gnus-score-default-duration nil)
	      :style radio
	      :selected (null gnus-score-default-duration)]
	     ["Permanent"
	      (gnus-score-set-default 'gnus-score-default-duration 'p)
	      :style radio
	      :selected (eq gnus-score-default-duration 'p)]
	     ["Temporary"
	      (gnus-score-set-default 'gnus-score-default-duration 't)
	      :style radio
	      :selected (eq gnus-score-default-duration 't)]
	     ["Immediate" 
	      (gnus-score-set-default 'gnus-score-default-duration 'i)
	      :style radio
	      :selected (eq gnus-score-default-duration 'i)])
	    ))

     (easy-menu-define
      gnus-summary-article-menu
      gnus-summary-mode-map
      ""
      '("Article"
	("Hide"
	 ["All" gnus-article-hide t]
	 ["Headers" gnus-article-hide-headers t]
	 ["Signature" gnus-article-hide-signature t]
	 ["Citation" gnus-article-hide-citation t])
	("Highlight"
	 ["All" gnus-article-highlight t]
	 ["Headers" gnus-article-highlight-headers t]
	 ["Signature" gnus-article-highlight-signature t]
	 ["Citation" gnus-article-highlight-citation t])
	("Date"
	 ["Local" gnus-article-date-local t]
	 ["UT" gnus-article-date-ut t]
	 ["Lapsed" gnus-article-date-lapsed t])
	("Filter"
	 ["Overstrike" gnus-article-treat-overstrike t]
	 ["Word wrap" gnus-article-word-wrap t]
	 ["CR" gnus-article-remove-cr t]
	 ["Show X-Face" gnus-article-display-x-face t]
	 ["Quoted-Printable" gnus-article-de-quoted-unreadable t]
	 ["Rot 13" gnus-summary-caesar-message t]
	 ["Add buttons" gnus-article-add-buttons t]
	 ["Stop page breaking" gnus-summary-stop-page-breaking t]
	 ["Toggle MIME" gnus-summary-toggle-mime t]
	 ["Toggle header" gnus-summary-toggle-header t])
	("Output"
	 ["Save in default format" gnus-summary-save-article t]
	 ["Save in file" gnus-summary-save-article-file t]
	 ["Save in Unix mail format" gnus-summary-save-article-mail t]
	 ["Save in MH folder" gnus-summary-save-article-folder t]
	 ["Save in VM folder" gnus-summary-save-article-vm t]
	 ["Save in RMAIL mbox" gnus-summary-save-article-rmail t]
	 ["Pipe through a filter" gnus-summary-pipe-output t])
	("Backend"
	 ["Respool article" gnus-summary-respool-article t]
	 ["Move article" gnus-summary-move-article t]
	 ["Copy article" gnus-summary-copy-article t]
	 ["Import file" gnus-summary-import-article t]
	 ["Edit article" gnus-summary-edit-article t]
	 ["Delete article" gnus-summary-delete-article t])
	("Extract"
	 ["Uudecode" gnus-uu-decode-uu t]
	 ["Uudecode and save" gnus-uu-decode-uu-and-save t]
	 ["Unshar" gnus-uu-decode-unshar t]
	 ["Unshar and save" gnus-uu-decode-unshar-and-save t]
	 ["Save" gnus-uu-decode-save t]
	 ["Binhex" gnus-uu-decode-binhex t])
	["Enter digest buffer" gnus-summary-enter-digest-group t]
	["Isearch article" gnus-summary-isearch-article t]
	["Search all articles" gnus-summary-search-article-forward t]
	["Beginning of the article" gnus-summary-beginning-of-article t]
	["End of the article" gnus-summary-end-of-article t]
	["Fetch parent of article" gnus-summary-refer-parent-article t]
	["Fetch article with id..." gnus-summary-refer-article t]
	["Redisplay" gnus-summary-show-article t]))


	 
     (easy-menu-define
      gnus-summary-thread-menu
      gnus-summary-mode-map
      ""
      '("Threads"
	["Toggle threading" gnus-summary-toggle-threads t]
	["Display hidden thread" gnus-summary-show-thread t]
	["Hide thread" gnus-summary-hide-thread t]
	["Go to next thread" gnus-summary-next-thread t]
	["Go to previous thread" gnus-summary-prev-thread t]
	["Go down thread" gnus-summary-down-thread t]
	["Go up thread" gnus-summary-up-thread t]
	["Mark thread as read" gnus-summary-kill-thread t]
	["Lower thread score" gnus-summary-lower-thread t]
	["Raise thread score" gnus-summary-raise-thread t]
	))
     (easy-menu-define
      gnus-summary-post-menu
      gnus-summary-mode-map
      ""
      '("Post"
	["Post an article" gnus-summary-post-news t]
	["Followup" gnus-summary-followup t]
	["Followup and yank" gnus-summary-followup-with-original t]
	["Supersede article" gnus-summary-supersede-article t]
	["Cancel article" gnus-summary-cancel-article t]
	["Reply" gnus-summary-reply t]
	["Reply and yank" gnus-summary-reply-with-original t]
	["Mail forward" gnus-summary-mail-forward t]
	["Post forward" gnus-summary-post-forward t]
	["Digest and mail" gnus-uu-digest-mail-forward t]
	["Digest and post" gnus-uu-digest-post-forward t]
	["Send a mail" gnus-summary-mail-other-window t]
	["Reply & followup" gnus-summary-followup-and-reply t]
	["Reply & followup and yank" gnus-summary-followup-and-reply-with-original t]
	["Uuencode and post" gnus-uu-post-news t]
	))
     (run-hooks 'gnus-summary-menu-hook)
     )))

(defun gnus-score-set-default (var value)
  ;; A version of set that updates the GNU Emacs menu-bar.
  (set var value)
  ;; It is the message that forces the active status to be updated.
  (message ""))

(defvar gnus-score-default-header nil
  "Default header when entering new scores.

Should be one of the following symbols.

 a: from
 s: subject
 b: body
 h: head
 i: message-id
 t: references
 x: xref
 l: lines
 d: date
 f: followup

If nil, the user will be asked for a header.")

(defvar gnus-score-default-type nil
  "Default match type when entering new scores.

Should be one of the following symbols.

 s: substring
 e: exact string
 f: fuzzy string
 r: regexp string
 b: before date
 a: at date
 n: this date
 <: less than number
 >: greater than number
 =: equal to number

If nil, the user will be asked for a match type.")

(defvar gnus-score-default-fold nil
  "Use case folding for new score file entries iff not nil.")


(defun gnus-score-default-fold-toggle ()
  "Toggle folding for new score file entries."
  (interactive)
  (setq gnus-score-default-fold (not gnus-score-default-fold))
  (if gnus-score-default-fold
      (message "New score file entries will be case insensitive.")
    (message "New score file entries will be case sensitive.")))

(defvar gnus-score-default-duration nil
  "Default duration of effect when entering new scores.

Should be one of the following symbols.

 t: temporary
 p: permanent
 i: immediate

If nil, the user will be asked for a duration.")

(defun gnus-visual-score-map (type)
  (if t
      nil
    (let ((headers '(("author" "from" string)
		     ("subject" "subject" string)
		     ("article body" "body" string)
		     ("article head" "head" string)
		     ("xref" "xref" string)
		     ("lines" "lines" number)
		     ("followups to author" "followup" string)))
	  (types '((number ("less than" <)
			   ("greater than" >)
			   ("equal" =))
		   (string ("substring" s)
			   ("exact string" e)
			   ("fuzzy string" f)
			   ("regexp" r))))
	  (perms '(("temporary" (current-time-string))
		   ("permanent" nil)
		   ("immediate" now)))
	  header)
      (list 
       (apply 
	'nconc
	(list
	 (if (eq type 'lower)
	     "Lower score"
	   "Increase score"))
	(let (outh)
	  (while headers
	    (setq header (car headers))
	    (setq outh 
		  (cons 
		   (apply 
		    'nconc
		    (list (car header))
		    (let ((ts (cdr (assoc (nth 2 header) types)))
			  outt)
		      (while ts
			(setq outt
			      (cons 
			       (apply 
				'nconc
				(list (car (car ts)))
				(let ((ps perms)
				      outp)
				  (while ps
				    (setq outp
					  (cons
					   (vector
					    (car (car ps)) 
					    (list
					     'gnus-summary-score-entry
					     (nth 1 header)
					     (if (or (string= (nth 1 header) 
							      "head")
						     (string= (nth 1 header)
							      "body"))
						 ""
					       (list 'gnus-summary-header 
						     (nth 1 header)))
					     (list 'quote (nth 1 (car ts)))
					     (list 'gnus-score-default nil)
					     (nth 1 (car ps))
					     t)
					    t)
					   outp))
				    (setq ps (cdr ps)))
				  (list (nreverse outp))))
			       outt))
			(setq ts (cdr ts)))
		      (list (nreverse outt))))
		   outh))
	    (setq headers (cdr headers)))
	  (list (nreverse outh))))))))
 
;; Article buffer
(defun gnus-article-make-menu-bar ()
  (gnus-visual-turn-off-edit-menu 'summary)
  (or
   (boundp 'gnus-article-article-menu)
   (progn
     (easy-menu-define
      gnus-article-article-menu
      gnus-article-mode-map
      ""
      '("Article"
	["Scroll forwards" gnus-article-next-page t]
	["Scroll backwards" gnus-article-prev-page t]
	["Show summary" gnus-article-show-summary t]
	["Fetch Message-ID at point" gnus-article-refer-article t]
	["Mail to address at point" gnus-article-mail t]
	))

     (easy-menu-define
      gnus-article-treatment-menu
      gnus-article-mode-map
      ""
      '("Treatment"
	["Hide headers" gnus-article-hide-headers t]
	["Hide signature" gnus-article-hide-signature t]
	["Hide citation" gnus-article-hide-citation t]
	["Treat overstrike" gnus-article-treat-overstrike t]
	["Remove carriage return" gnus-article-remove-cr t]
	["Remove quoted-unreadable" gnus-article-de-quoted-unreadable t]
	))
     (run-hooks 'gnus-article-menu-hook)
     )))

;;;
;;; summary highlights
;;;

(defun gnus-highlight-selected-summary ()
  ;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
  ;; Highlight selected article in summary buffer
  (if gnus-summary-selected-face
      (save-excursion
	(let* ((beg (progn (beginning-of-line) (point)))
	       (end (progn (end-of-line) (point)))
	       ;; Fix by Mike Dugan <dugan@bucrf16.bu.edu>.
	       (from (if (get-text-property beg 'mouse-face) 
			 beg
		       (1+ (or (next-single-property-change 
				beg 'mouse-face nil end) 
			       beg))))
	       (to (1- (or (next-single-property-change
			    from 'mouse-face nil end)
			   end))))
	  ;; If no mouse-face prop on line (e.g. xemacs) we 
	  ;; will have to = from = end, so we highlight the
	  ;; entire line instead.
	  (if (= (+ to 2) from)
	      (progn
		(setq from beg)
		(setq to end)))
	  (if gnus-newsgroup-selected-overlay
	      (gnus-move-overlay gnus-newsgroup-selected-overlay 
				 from to (current-buffer))
	    (setq gnus-newsgroup-selected-overlay (gnus-make-overlay from to))
	    (gnus-overlay-put gnus-newsgroup-selected-overlay 'face 
			      gnus-summary-selected-face))))))

;; New implementation by Christian Limpach <Christian.Limpach@nice.ch>.
(defun gnus-summary-highlight-line ()
  "Highlight current line according to `gnus-summary-highlight'."
  (let* ((list gnus-summary-highlight)
	 (p (point))
	 (end (progn (end-of-line) (point)))
	 ;; now find out where the line starts and leave point there.
	 (beg (progn (beginning-of-line) (point)))
	 (score (or (cdr (assq (or (get-text-property beg 'gnus-number)
				   gnus-current-article)
			       gnus-newsgroup-scored))
		    gnus-summary-default-score 0))
	 (default gnus-summary-default-score)
	 (mark (get-text-property beg 'gnus-mark))
	 (inhibit-read-only t))
    (while (and list (not (eval (car (car list)))))
      (setq list (cdr list)))
    (let ((face (and list (cdr (car list)))))
      (or (eobp)
	  (eq face (get-text-property beg 'face))
	  (put-text-property beg end 'face 
			     (if (boundp face) (symbol-value face) face))))
    (goto-char p)))

;;;
;;; gnus-carpal
;;;

(defvar gnus-carpal-group-buffer-buttons
  '(("next" . gnus-group-next-unread-group)
    ("prev" . gnus-group-prev-unread-group)
    ("read" . gnus-group-read-group)
    ("select" . gnus-group-select-group)
    ("catch-up" . gnus-group-catchup-current)
    ("new-news" . gnus-group-get-new-news-this-group)
    ("toggle-sub" . gnus-group-unsubscribe-current-group)
    ("subscribe" . gnus-group-unsubscribe-group)
    ("kill" . gnus-group-kill-group)
    ("yank" . gnus-group-yank-group)
    ("describe" . gnus-group-describe-group)
    "list"
    ("subscribed" . gnus-group-list-groups)
    ("all" . gnus-group-list-all-groups)
    ("killed" . gnus-group-list-killed)
    ("zombies" . gnus-group-list-zombies)
    ("matching" . gnus-group-list-matching)
    ("post" . gnus-group-post-news)
    ("mail" . gnus-group-mail)
    ("rescan" . gnus-group-get-new-news)
    ("browse-foreign" . gnus-group-browse-foreign)
    ("exit" . gnus-group-exit)))

(defvar gnus-carpal-summary-buffer-buttons
  '("mark" 
    ("read" . gnus-summary-mark-as-read-forward)
    ("tick" . gnus-summary-tick-article-forward)
    ("clear" . gnus-summary-clear-mark-forward)
    ("expirable" . gnus-summary-mark-as-expirable)
    "move"
    ("scroll" . gnus-summary-next-page)
    ("next-unread" . gnus-summary-next-unread-article)
    ("prev-unread" . gnus-summary-prev-unread-article)
    ("first" . gnus-summary-first-unread-article)
    ("best" . gnus-summary-best-unread-article)
    "article"
    ("headers" . gnus-summary-toggle-header)
    ("uudecode" . gnus-uu-decode-uu)
    ("enter-digest" . gnus-summary-enter-digest-group)
    ("fetch-parent" . gnus-summary-refer-parent-article)
    "mail"
    ("move" . gnus-summary-move-article)
    ("copy" . gnus-summary-copy-article)
    ("respool" . gnus-summary-respool-article)
    "threads"
    ("lower" . gnus-summary-lower-thread)
    ("kill" . gnus-summary-kill-thread)
    "post"
    ("post" . gnus-summary-post-news)
    ("mail" . gnus-summary-mail)
    ("followup" . gnus-summary-followup-with-original)
    ("reply" . gnus-summary-reply-with-original)
    ("cancel" . gnus-summary-cancel-article)
    "misc"
    ("exit" . gnus-summary-exit)
    ("fed-up" . gnus-summary-catchup-and-goto-next-group)))

(defvar gnus-carpal-server-buffer-buttons 
  '(("add" . gnus-server-add-server)
    ("browse" . gnus-server-browse-server)
    ("list" . gnus-server-list-servers)
    ("kill" . gnus-server-kill-server)
    ("yank" . gnus-server-yank-server)
    ("copy" . gnus-server-copy-server)
    ("exit" . gnus-server-exit)))

(defvar gnus-carpal-browse-buffer-buttons
  '(("subscribe" . gnus-browse-unsubscribe-current-group)
    ("exit" . gnus-browse-exit)))

(defvar gnus-carpal-group-buffer "*Carpal Group*")
(defvar gnus-carpal-summary-buffer "*Carpal Summary*")
(defvar gnus-carpal-server-buffer "*Carpal Server*")
(defvar gnus-carpal-browse-buffer "*Carpal Browse*")

(defvar gnus-carpal-attached-buffer nil)

(defvar gnus-carpal-mode-hook nil
  "*Hook run in carpal mode buffers.")

(defvar gnus-carpal-button-face 'bold
  "*Face used on carpal buttons.")

(defvar gnus-carpal-header-face 'bold-italic
  "*Face used on carpal buffer headers.")

(defvar gnus-carpal-mode-map nil)
(put 'gnus-carpal-mode 'mode-class 'special)

(if gnus-carpal-mode-map
    nil
  (setq gnus-carpal-mode-map (make-keymap))
  (suppress-keymap gnus-carpal-mode-map)
  (define-key gnus-carpal-mode-map " " 'gnus-carpal-select)
  (define-key gnus-carpal-mode-map "\r" 'gnus-carpal-select)
  (define-key gnus-carpal-mode-map gnus-mouse-2 'gnus-carpal-mouse-select))

(defun gnus-carpal-mode ()
  "Major mode for clicking buttons.

All normal editing commands are switched off.
\\<gnus-carpal-mode-map>
The following commands are available:

\\{gnus-carpal-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-line-modified "-- ")
  (setq major-mode 'gnus-carpal-mode)
  (setq mode-name "Gnus Carpal")
  (setq mode-line-process nil)
  (use-local-map gnus-carpal-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)
  (make-local-variable 'gnus-carpal-attached-buffer)
  (run-hooks 'gnus-carpal-mode-hook))

(defun gnus-carpal-setup-buffer (type)
  (let ((buffer (symbol-value (intern (format "gnus-carpal-%s-buffer" type)))))
    (if (get-buffer buffer)
	()
      (save-excursion
	(set-buffer (get-buffer-create buffer))
	(gnus-carpal-mode)
	(setq gnus-carpal-attached-buffer 
	      (intern (format "gnus-%s-buffer" type)))
	(gnus-add-current-to-buffer-list)
	(let ((buttons (symbol-value 
			(intern (format "gnus-carpal-%s-buffer-buttons"
					type))))
	      (buffer-read-only nil)
	      button)
	  (while buttons
	    (setq button (car buttons)
		  buttons (cdr buttons))
	    (if (stringp button)
		(set-text-properties
		 (point)
		 (prog2 (insert button) (point) (insert " "))
		 (list 'face gnus-carpal-header-face))
	      (set-text-properties
	       (point)
	       (prog2 (insert (car button)) (point) (insert " "))
	       (list 'gnus-callback (cdr button)
		     'face gnus-carpal-button-face
		     'mouse-face 'highlight))))
	  (let ((fill-column (- (window-width) 2)))
	    (fill-region (point-min) (point-max)))
	  (set-window-point (get-buffer-window (current-buffer)) 
			    (point-min)))))))

(defun gnus-carpal-select ()
  "Select the button under point."
  (interactive)
  (let ((func (get-text-property (point) 'gnus-callback)))
    (if (null func)
	()
      (pop-to-buffer (symbol-value gnus-carpal-attached-buffer))
      (call-interactively func))))

(defun gnus-carpal-mouse-select (event)
  "Select the button under the mouse pointer."
  (interactive "e")
  (mouse-set-point event)
  (gnus-carpal-select))

;;; 
;;; article highlights
;;;

;; Written by Per Abrahamsen <abraham@iesd.auc.dk>.

;;; Internal Variables:

(defvar gnus-button-regexp nil)
;; Regexp matching any of the regexps from `gnus-button-alist'.

(defvar gnus-button-last nil)
;; The value of `gnus-button-alist' when `gnus-button-regexp' was build.

;;; Commands:

(defun gnus-article-push-button (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start event))))
  (let* ((pos (posn-point (event-start event)))
         (data (get-text-property pos 'gnus-data))
	 (fun (get-text-property pos 'gnus-callback)))
    (if fun (funcall fun data))))

(defun gnus-article-press-button ()
  "Check text at point for a callback function.
If the text at point has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive)
  (let* ((data (get-text-property (point) 'gnus-data))
	 (fun (get-text-property (point) 'gnus-callback)))
    (if fun (funcall fun data))))

;; Suggested by Arne Elofsson <arne@hodgkin.mbi.ucla.edu>
(defun gnus-article-next-button ()
  "Move point to next button."
  (interactive)
  (if (get-text-property (point) 'gnus-callback)
      (goto-char (next-single-property-change (point) 'gnus-callback
					      nil (point-max))))
  (let ((pos (next-single-property-change (point) 'gnus-callback)))
    (if pos
	(goto-char pos)
      (setq pos (next-single-property-change (point-min) 'gnus-callback))
      (if pos
	  (goto-char pos)
	(error "No buttons found")))))

(defun gnus-article-highlight (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-citation', 
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-citation force)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons force))

(defun gnus-article-highlight-some (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons))

(defun gnus-article-hide (&optional force)
  "Hide current article.
This function calls `gnus-article-hide-headers',
`gnus-article-hide-citation-maybe', and `gnus-article-hide-signature'
to do the hiding.  See the documentation for those functions." 
  (interactive (list 'force))
  (gnus-article-hide-headers)
  (gnus-article-hide-citation-maybe force)
  (gnus-article-hide-signature))

(defun gnus-article-highlight-headers ()
  "Highlight article headers as specified by `gnus-header-face-alist'."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char (point-min))
    (if (not (search-forward "\n\n" nil t))
	()
      (beginning-of-line 0)
      (while (not (bobp))
	(let ((alist gnus-header-face-alist)
	      (buffer-read-only nil)
	      (case-fold-search t)
	      (end (point))
	      (inhibit-point-motion-hooks t)
	      begin entry regexp header-face field-face 
	      header-found field-found)
	  (re-search-backward "^[^ \t]" nil t)
	  (setq begin (point))
	  (while alist
	    (setq entry (car alist)
		  regexp (nth 0 entry)
		  header-face (nth 1 entry)
		  field-face (nth 2 entry)
		  alist (cdr alist))
	    (if (looking-at regexp)
		(let ((from (point)))
		  (skip-chars-forward "^:\n")
		  (and (not header-found)
		       header-face
		       (progn
			 (put-text-property  from (point) 'face header-face)
			 (setq header-found t)))
		  (and (not field-found)
		       field-face
		       (progn 
			 (skip-chars-forward ": \t")
			 (let ((from (point)))
			   (goto-char end)
			   (skip-chars-backward " \t")
			   (put-text-property from (point) 'face field-face)
			   (setq field-found t))))))
	    (goto-char begin)))))))

(defun gnus-article-highlight-signature ()
  "Highlight the signature in an article.
It does this by highlighting everything after
`gnus-signature-separator' using `gnus-signature-face'." 
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t))
      (goto-char (point-max))
      (and (re-search-backward gnus-signature-separator nil t)
	   gnus-signature-face
	   (let ((start (match-beginning 0))
		 (end (match-end 0)))
	     (gnus-article-add-button start end 'gnus-signature-toggle end)
	     (gnus-overlay-put (gnus-make-overlay end (point-max))
			       'face gnus-signature-face))))))

(defun gnus-article-hide-signature ()
  "Hide the signature in an article.
It does this by making everything after `gnus-signature-separator' invisible."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (and (re-search-backward gnus-signature-separator nil t)
	   gnus-signature-face
	   (add-text-properties (match-end 0) (point-max)
				gnus-hidden-properties)))))

(defun gnus-article-add-buttons (&optional force)
  "Find external references in article and make them to buttons.

External references are things like message-ids and URLs, as specified by 
`gnus-button-alist'."
  (interactive (list 'force))
  (if (eq gnus-button-last gnus-button-alist)
      ()
    (setq gnus-button-regexp (mapconcat 'car gnus-button-alist  "\\|")
	  gnus-button-last gnus-button-alist))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (gnus-cite-parse-maybe force)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t))
      (goto-char (point-min))
      (or (search-forward "\n\n" nil t)
	  (goto-char (point-max)))
      (while (re-search-forward gnus-button-regexp nil t)
	(goto-char (match-beginning 0))
	(let* ((from (point))
	       (entry (gnus-button-entry))
	       (start (and entry (match-beginning (nth 1 entry))))
	       (end (and entry (match-end (nth 1 entry))))
	       (form (nth 2 entry)))
	  (if (not entry)
	      ()
	    (goto-char (match-end 0))
	    (if (eval form)
		(gnus-article-add-button start end 'gnus-button-push
					 (set-marker (make-marker)
						     from)))))))))
(defun gnus-netscape-open-url (url)
  "Open URL in netscape, or start new scape with URL."
  (let ((process (start-process (concat "netscape " url)
				nil
				"netscape"
				"-remote" 
				(concat "openUrl(" url ")'"))))
    (set-process-sentinel process 
			  (` (lambda (process change)
			       (or (eq (process-exit-status process) 0)
				   (gnus-netscape-start-url (, url))))))))

(defun gnus-netscape-start-url (url)
  "Start netscape with URL."
  (start-process (concat "netscape" url) nil "netscape" url))

;;; External functions:

(defun gnus-article-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (and gnus-article-button-face
       (gnus-overlay-put (gnus-make-overlay from to)
			 'face gnus-article-button-face))
  (add-text-properties from to
		       (append (and gnus-article-mouse-face
				    (list 'mouse-face gnus-article-mouse-face))
			       (list 'gnus-callback fun)
			       (and data (list 'gnus-data data)))))

;;; Internal functions:

(defun gnus-signature-toggle (end)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil))
      (if (get-text-property end 'invisible)
	  (remove-text-properties end (point-max) gnus-hidden-properties)
	(add-text-properties end (point-max) gnus-hidden-properties)))))

;see gnus-cus.el
;(defun gnus-make-face (color)
;  ;; Create entry for face with COLOR.
;  (if gnus-make-foreground
;      (custom-face-lookup color nil nil nil nil nil)
;    (custom-face-lookup nil color nil nil nil nil)))

(defun gnus-button-entry ()
  ;; Return the first entry in `gnus-button-alist' matching this place.
  (let ((alist gnus-button-alist)
	(entry nil))
    (while alist
      (setq entry (car alist)
	    alist (cdr alist))
      (if (looking-at (car entry))
	  (setq alist nil)
	(setq entry nil)))
    entry))

(defun gnus-button-push (marker)
  ;; Push button starting at MARKER.
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char marker)
    (let* ((entry (gnus-button-entry))
	   (inhibit-point-motion-hooks t)
	   (fun (nth 3 entry))
	   (args (mapcar (lambda (group) 
			   (let ((string (buffer-substring
					  (match-beginning group)
					  (match-end group))))
			     (set-text-properties 0 (length string) nil string)
			     string))
			 (nthcdr 4 entry))))
      (cond ((fboundp fun)
	     (apply fun args))
	    ((and (boundp fun)
		  (fboundp (symbol-value fun)))
	     (apply (symbol-value fun) args))
	    (t
	     (message "You must define `%S' to use this button"
		      (cons fun args)))))))

(defun gnus-button-message-id (message-id)
  ;; Push on MESSAGE-ID.
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (gnus-summary-refer-article message-id)))

;;; Compatibility Functions:

(or (fboundp 'rassoc)
    ;; Introduced in Emacs 19.29.
    (defun rassoc (elt list)
      "Return non-nil if ELT is `equal' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr is ELT."
      (let (result)
	(while list
	  (setq result (car list))
	  (if (equal (cdr result) elt)
	      (setq list nil)
	    (setq result nil
		  list (cdr list))))
	result)))

; (require 'gnus-cus)
(gnus-ems-redefine)
(provide 'gnus-vis)

;;; gnus-vis.el ends here
