;;; gnus-vis.el --- display-oriented parts of Gnus
;; Copyright (C) 1995,96 Free Software Foundation, Inc.

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
(require 'browse-url)
(require 'gnus-score)
(eval-when-compile (require 'cl))

(defvar gnus-group-menu-hook nil
  "*Hook run after the creation of the group mode menu.")

(defvar gnus-summary-menu-hook nil
  "*Hook run after the creation of the summary mode menu.")

(defvar gnus-article-menu-hook nil
  "*Hook run after the creation of the article mode menu.")

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

(defvar gnus-button-url-regexp "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-\\wa-zA-Z0-9_=!?#$@~`%&*+|\\/.,]*[-\\wa-zA-Z0-9_=#$@~`%&*+|\\/]"
  "*Regular expression that matches URLs.")

(defvar gnus-button-alist 
  `(("\\bin\\( +article\\)? +\\(<\\([^\n @<>]+@[^\n @<>]+\\)>\\)" 2 
     t gnus-button-message-id 3)
    ("\\(<?\\(url: ?\\)?news:\\([^>\n\t ]*\\)>?\\)" 1 t
     gnus-button-message-id 3)
    ("\\(<URL: *\\)?mailto: *\\([^> \n\t]+\\)>?" 0 t gnus-button-reply 2)
    ;; Next regexp stolen from highlight-headers.el.
    ;; Modified by Vladimir Alexiev.
    (,gnus-button-url-regexp 0 t gnus-button-url 0)
    ;; This is how URLs _should_ be embedded in text...  It should go
    ;; last to avoid matching only a subset of the URL, depending on
    ;; how it was broken across lines.
    ("<URL:\\([^>]+\\)>" 0 t gnus-button-url 1))
  "Alist of regexps matching buttons in article bodies.

Each entry has the form (REGEXP BUTTON FORM CALLBACK PAR...), where
REGEXP: is the string matching text around the button,
BUTTON: is the number of the regexp grouping actually matching the button,
FORM: is a lisp expression which must eval to true for the button to
be added, 
CALLBACK: is the function to call when the user push this button, and each
PAR: is a number of a regexp grouping whose text will be passed to CALLBACK.

CALLBACK can also be a variable, in that case the value of that
variable it the real callback function.")

(defvar gnus-header-button-alist 
  `(("^\\(References\\|Message-I[Dd]\\):" "<[^>]+>"
     0 t gnus-button-message-id 0)
    ("^\\(From\\|Reply-To\\): " ": *\\(.+\\)$" 1 t gnus-button-reply 0)
    ("^\\(Cc\\|To\\):" "[^ \t\n<>,()\"]+@[^ \t\n<>,()\"]+" 
     0 t gnus-button-mailto 0)
    ("^X-[Uu][Rr][Ll]:" ,gnus-button-url-regexp 0 t gnus-button-url 0)
    ("^[^:]+:" ,gnus-button-url-regexp 0 t gnus-button-url 0)
    ("^[^:]+:" "\\(<\\(url: \\)?news:\\([^>\n ]*\\)>\\)" 1 t
     gnus-button-message-id 3))
  "Alist of headers and regexps to match buttons in article heads.

This alist is very similar to `gnus-button-alist', except that each
alist has an additional HEADER element first in each entry:

\(HEADER REGEXP BUTTON FORM CALLBACK PAR)

HEADER is a regexp to match a header.  For a fuller explanation, see
`gnus-button-alist'.")

;see gnus-cus.el
;(eval-when-compile
;  (defvar browse-url-browser-function))

;;; Group mode highlighting.

;see gnus-cus.el
;(defvar gnus-group-highlight nil
;  "Group lines are highlighted with the FACE for the first FORM which
;evaluate to a non-nil value.  
;
;Point will be at the beginning of the line when FORM is evaluated.
;Variables bound when these forms are evaluated include:
;
;group: The group name.
;unread: The number of unread articles.
;method: The select method.
;mailp: Whether the select method is a mail method.
;level: The level of the group.
;score: The score of the group.
;ticked: The number of ticked articles in the group.
;")


;;; Internal variables.

(defvar gnus-button-marker-list nil)



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
      gnus-group-reading-menu gnus-group-mode-map ""
      '("Group"
	["Read" gnus-group-read-group (gnus-group-group-name)]
	["Select" gnus-group-select-group (gnus-group-group-name)]
	["See old articles" (gnus-group-select-group 'all)
	 :keys "C-u SPC" :active (gnus-group-group-name)]
	["Catch up" gnus-group-catchup-current (gnus-group-group-name)]
	["Catch up all articles" gnus-group-catchup-current-all
	 (gnus-group-group-name)]
	["Check for new articles" gnus-group-get-new-news-this-group
	 (gnus-group-group-name)]
	["Toggle subscription" gnus-group-unsubscribe-current-group
	 (gnus-group-group-name)]
	["Kill" gnus-group-kill-group (gnus-group-group-name)]
	["Yank" gnus-group-yank-group gnus-list-of-killed-groups]
	["Describe" gnus-group-describe-group (gnus-group-group-name)]
	["Fetch FAQ" gnus-group-fetch-faq (gnus-group-group-name)]
	["Edit kill file" gnus-group-edit-local-kill
	 (gnus-group-group-name)]
	;; Actually one should check, if any of the marked groups gives t for
	;; (gnus-check-backend-function 'request-expire-articles ...)
	["Expire articles" gnus-group-expire-articles
	 (or (and (gnus-group-group-name)
		  (gnus-check-backend-function
		   'request-expire-articles
		   (gnus-group-group-name))) gnus-group-marked)]
	["Set group level" gnus-group-set-current-level
	 (gnus-group-group-name)]
	["Select quick" gnus-group-quick-select-group (gnus-group-group-name)]
	))
  
     (easy-menu-define
      gnus-group-group-menu gnus-group-mode-map ""
      '("Groups"
	("Listing"
	 ["List unread subscribed groups" gnus-group-list-groups t]
	 ["List (un)subscribed groups" gnus-group-list-all-groups t]
	 ["List killed groups" gnus-group-list-killed gnus-killed-list]
	 ["List zombie groups" gnus-group-list-zombies gnus-zombie-list]
	 ["List level..." gnus-group-list-level t]
	 ["Describe all groups" gnus-group-describe-all-groups t]
	 ["Group apropos..." gnus-group-apropos t]
	 ["Group and description apropos..." gnus-group-description-apropos t]
	 ["List groups matching..." gnus-group-list-matching t]
	 ["List all groups matching..." gnus-group-list-all-matching t]
	 ["List active file" gnus-group-list-active t])
	("Sort"
	 ["Default sort" gnus-group-sort-groups
	  (or (not (boundp 'gnus-topic-mode)) (not gnus-topic-mode))]
	 ["Sort by method" gnus-group-sort-groups-by-method
	  (or (not (boundp 'gnus-topic-mode)) (not gnus-topic-mode))]
	 ["Sort by rank" gnus-group-sort-groups-by-rank
	  (or (not (boundp 'gnus-topic-mode)) (not gnus-topic-mode))]
	 ["Sort by score" gnus-group-sort-groups-by-score
	  (or (not (boundp 'gnus-topic-mode)) (not gnus-topic-mode))]
	 ["Sort by level" gnus-group-sort-groups-by-level
	  (or (not (boundp 'gnus-topic-mode)) (not gnus-topic-mode))]
	 ["Sort by unread" gnus-group-sort-groups-by-unread
	  (or (not (boundp 'gnus-topic-mode)) (not gnus-topic-mode))]
	 ["Sort by name" gnus-group-sort-groups-by-alphabet
	  (or (not (boundp 'gnus-topic-mode)) (not gnus-topic-mode))])
	("Mark"
	 ["Mark group" gnus-group-mark-group
	  (and (gnus-group-group-name)
	       (not (memq (gnus-group-group-name) gnus-group-marked)))]
	 ["Unmark group" gnus-group-unmark-group
	  (and (gnus-group-group-name)
	       (memq (gnus-group-group-name) gnus-group-marked))]
	 ["Unmark all" gnus-group-unmark-all-groups gnus-group-marked]
	 ["Mark regexp..." gnus-group-mark-regexp t]
	 ["Mark region" gnus-group-mark-region t]
	 ["Mark buffer" gnus-group-mark-buffer t]
	 ["Execute command" gnus-group-universal-argument
	  (or gnus-group-marked (gnus-group-group-name))])
	("Subscribe"
	 ["Subscribe to random group" gnus-group-unsubscribe-group t]
	 ["Kill all newsgroups in region" gnus-group-kill-region t]
	 ["Kill all zombie groups" gnus-group-kill-all-zombies
	  gnus-zombie-list]
	 ["Kill all groups on level..." gnus-group-kill-level t])
	("Foreign groups"
	 ["Make a foreign group" gnus-group-make-group t]
	 ["Add a directory group" gnus-group-make-directory-group t]
	 ["Add the help group" gnus-group-make-help-group t]
	 ["Add the archive group" gnus-group-make-archive-group t]
	 ["Make a doc group" gnus-group-make-doc-group t]
	 ["Make a kiboze group" gnus-group-make-kiboze-group t]
	 ["Make a virtual group" gnus-group-make-empty-virtual t]
	 ["Add a group to a virtual" gnus-group-add-to-virtual t]
	 ["Rename group" gnus-group-rename-group
	  (gnus-check-backend-function
	   'request-rename-group (gnus-group-group-name))]
	 ["Delete group" gnus-group-delete-group
	  (gnus-check-backend-function
	   'request-delete-group (gnus-group-group-name))])
	("Editing groups"
	 ["Parameters" gnus-group-edit-group-parameters
	  (gnus-group-group-name)]
	 ["Select method" gnus-group-edit-group-method
	  (gnus-group-group-name)]
	 ["Info" gnus-group-edit-group (gnus-group-group-name)])
	("Score file"
	 ["Flush cache" gnus-score-flush-cache
	  (or gnus-score-cache gnus-short-name-score-file-cache)])
	("Move"
	 ["Next" gnus-group-next-group t]
	 ["Previous" gnus-group-prev-group t]
	 ["Next unread" gnus-group-next-unread-group t]
	 ["Previous unread" gnus-group-prev-unread-group t]
	 ["Next unread same level" gnus-group-next-unread-group-same-level t]
	 ["Previous unread same level"
	  gnus-group-previous-unread-group-same-level t]
	 ["Jump to group" gnus-group-jump-to-group t]
	 ["First unread group" gnus-group-first-unread-group t]
	 ["Best unread group" gnus-group-best-unread-group t])
	["Transpose" gnus-group-transpose-groups
	 (gnus-group-group-name)]
	["Read a directory as a group..." gnus-group-enter-directory t]
	))

     (easy-menu-define
      gnus-group-misc-menu gnus-group-mode-map ""
      '("Misc"
	["Send a bug report" gnus-bug t]
	["Send a mail" gnus-group-mail t]
	["Post an article..." gnus-group-post-news t]
	["Customize score file" gnus-score-customize t]
	["Check for new news" gnus-group-get-new-news t]     
	["Activate all groups" gnus-activate-all-groups t]
	["Delete bogus groups" gnus-group-check-bogus-groups t]
	["Find new newsgroups" gnus-find-new-newsgroups t]
	["Restart Gnus" gnus-group-restart t]
	["Read init file" gnus-group-read-init-file t]
	["Browse foreign server" gnus-group-browse-foreign-server t]
	["Enter server buffer" gnus-group-enter-server-mode t]
	["Expire all expirable articles" gnus-group-expire-all-groups t]
	["Generate any kiboze groups" nnkiboze-generate-groups t]
	["Gnus version" gnus-version t]
	["Save .newsrc files" gnus-group-save-newsrc t]
	["Suspend Gnus" gnus-group-suspend t]
	["Clear dribble buffer" gnus-group-clear-dribble t]
	["Exit from Gnus" gnus-group-exit t]
	["Exit without saving" gnus-group-quit t]
	["Edit global kill file" gnus-group-edit-global-kill t]
	["Read manual" gnus-info-find-node t]
	["Toggle topics" gnus-topic-mode t]
	("SOUP"
	 ["Pack replies" nnsoup-pack-replies (fboundp 'nnsoup-request-group)]
	 ["Send replies" gnus-soup-send-replies
	  (fboundp 'gnus-soup-pack-packet)]
	 ["Pack packet" gnus-soup-pack-packet (fboundp 'gnus-soup-pack-packet)]
	 ["Save areas" gnus-soup-save-areas (fboundp 'gnus-soup-pack-packet)]
	 ["Brew SOUP" gnus-soup-brew-soup (fboundp 'gnus-soup-pack-packet)])
	))
     (run-hooks 'gnus-group-menu-hook)
     )))

;; Summary buffer
(defun gnus-summary-make-menu-bar ()
  (gnus-visual-turn-off-edit-menu 'summary)

  (unless (boundp 'gnus-summary-misc-menu)

    (easy-menu-define
     gnus-summary-misc-menu gnus-summary-mode-map ""
     '("Misc"
       ("Mark"
	("Read"
	 ["Mark as read" gnus-summary-mark-as-read-forward t]
	 ["Mark same subject and select"
	  gnus-summary-kill-same-subject-and-select t]
	 ["Mark same subject" gnus-summary-kill-same-subject t]
	 ["Catchup" gnus-summary-catchup t]
	 ["Catchup all" gnus-summary-catchup-all t]
	 ["Catchup to here" gnus-summary-catchup-to-here t]
	 ["Catchup region" gnus-summary-mark-region-as-read t]
	 ["Mark excluded" gnus-summary-limit-mark-excluded-as-read t])
	("Various"
	 ["Tick" gnus-summary-tick-article-forward t]
	 ["Mark as dormant" gnus-summary-mark-as-dormant t]
	 ["Remove marks" gnus-summary-clear-mark-forward t]
	 ["Set expirable mark" gnus-summary-mark-as-expirable t]
	 ["Set bookmark" gnus-summary-set-bookmark t]
	 ["Remove bookmark" gnus-summary-remove-bookmark t])
	("Limit"
	 ["Marks..." gnus-summary-limit-to-marks t]
	 ["Subject..." gnus-summary-limit-to-subject t]
	 ["Author..." gnus-summary-limit-to-author t]
	 ["Score" gnus-summary-limit-to-score t]
	 ["Unread" gnus-summary-limit-to-unread t]
	 ["Non-dormant" gnus-summary-limit-exclude-dormant t]
	 ["Articles" gnus-summary-limit-to-articles t]
	 ["Pop limit" gnus-summary-pop-limit t]
	 ["Show dormant" gnus-summary-limit-include-dormant t]
	 ["Hide childless dormant" 
	  gnus-summary-limit-exclude-childless-dormant t]
	 ;;["Hide thread" gnus-summary-limit-exclude-thread t]
	 ["Show expunged" gnus-summary-show-all-expunged t])
	("Process mark"
	 ["Set mark" gnus-summary-mark-as-processable t]
	 ["Remove mark" gnus-summary-unmark-as-processable t]
	 ["Remove all marks" gnus-summary-unmark-all-processable t]
	 ["Mark above" gnus-uu-mark-over t]
	 ["Mark series" gnus-uu-mark-series t]
	 ["Mark region" gnus-uu-mark-region t]
	 ["Mark by regexp..." gnus-uu-mark-by-regexp t]
	 ["Mark all" gnus-uu-mark-all t]
	 ["Mark buffer" gnus-uu-mark-buffer t]
	 ["Mark sparse" gnus-uu-mark-sparse t]
	 ["Mark thread" gnus-uu-mark-thread t]
	 ["Unmark thread" gnus-uu-unmark-thread t]))
       ("Scroll article"
	["Page forward" gnus-summary-next-page t]
	["Page backward" gnus-summary-prev-page t]
	["Line forward" gnus-summary-scroll-up t])
       ("Move"
	["Next unread article" gnus-summary-next-unread-article t]
	["Previous unread article" gnus-summary-prev-unread-article t]
	["Next article" gnus-summary-next-article t]
	["Previous article" gnus-summary-prev-article t]
	["Next unread subject" gnus-summary-next-unread-subject t]
	["Previous unread subject" gnus-summary-prev-unread-subject t]
	["Next article same subject" gnus-summary-next-same-subject t]
	["Previous article same subject" gnus-summary-prev-same-subject t]
	["First unread article" gnus-summary-first-unread-article t]
	["Best unread article" gnus-summary-best-unread-article t]
	["Go to subject number..." gnus-summary-goto-subject t]
	["Go to article number..." gnus-summary-goto-article t]
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
	["Catchup all and exit" gnus-summary-catchup-and-exit t]
	["Catchup and goto next" gnus-summary-catchup-and-goto-next-group t]
	["Exit group" gnus-summary-exit t]
	["Exit group without updating" gnus-summary-exit-no-update t]
	["Exit and goto next group" gnus-summary-next-group t]
	["Exit and goto prev group" gnus-summary-prev-group t]
	["Reselect group" gnus-summary-reselect-current-group t]
	["Rescan group" gnus-summary-rescan-group t])
       ("Help"
	["Fetch group FAQ" gnus-summary-fetch-faq t]
	["Describe group" gnus-summary-describe-group t]
	["Read manual" gnus-info-find-node t])
       ("Cache"
	["Enter article" gnus-cache-enter-article t]
	["Remove article" gnus-cache-remove-article t])
       ("Modes"
	["Pick and read" gnus-pick-mode t]
	["Binary" gnus-binary-mode t])
       ["Filter articles..." gnus-summary-execute-command t]
       ["Run command on subjects..." gnus-summary-universal-argument t]
       ["Toggle line truncation" gnus-summary-toggle-truncation t]
       ["Expand window" gnus-summary-expand-window t]
       ["Expire expirable articles" gnus-summary-expire-articles
	(gnus-check-backend-function
	 'request-expire-articles gnus-newsgroup-name)]
       ["Edit local kill file" gnus-summary-edit-local-kill t]
       ["Edit main kill file" gnus-summary-edit-global-kill t]
       ))

    (easy-menu-define
     gnus-summary-kill-menu gnus-summary-mode-map ""
     (cons
      "Score"
      (nconc
       (list
	["Enter score..." gnus-summary-score-entry t])
       (gnus-visual-score-map 'increase)
       (gnus-visual-score-map 'lower)
       '(("Mark"
	  ["Kill below" gnus-summary-kill-below t]
	  ["Mark above" gnus-summary-mark-above t]
	  ["Tick above" gnus-summary-tick-above t]
	  ["Clear above" gnus-summary-clear-above t])
	 ["Current score" gnus-summary-current-score t]
	 ["Set score" gnus-summary-set-score t]
	 ["Customize score file" gnus-score-customize t]
	 ["Switch current score file..." gnus-score-change-score-file t]
	 ["Set mark below..." gnus-score-set-mark-below t]
	 ["Set expunge below..." gnus-score-set-expunge-below t]
	 ["Edit current score file" gnus-score-edit-current-scores t]
	 ["Edit score file" gnus-score-edit-file t]
	 ["Trace score" gnus-score-find-trace t]
	 ["Rescore buffer" gnus-summary-rescore t]
	 ["Increase score..." gnus-summary-increase-score t]
	 ["Lower score..." gnus-summary-lower-score t]))))

    '(("Default header"
       ["Ask" (gnus-score-set-default 'gnus-score-default-header nil)
	:style radio 
	:selected (null gnus-score-default-header)]
       ["From" (gnus-score-set-default 'gnus-score-default-header 'a)
	:style radio 
	:selected (eq gnus-score-default-header 'a)]
       ["Subject" (gnus-score-set-default 'gnus-score-default-header 's)
	:style radio 
	:selected (eq gnus-score-default-header 's)]
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
	:selected (eq gnus-score-default-duration 'i)]))

    (easy-menu-define
     gnus-summary-article-menu gnus-summary-mode-map ""
     '("Article"
       ("Hide"
	["All" gnus-article-hide t]
	["Headers" gnus-article-hide-headers t]
	["Signature" gnus-article-hide-signature t]
	["Citation" gnus-article-hide-citation t]
	["PGP" gnus-article-hide-pgp t]
	["Boring headers" gnus-article-hide-boring-headers t])
       ("Highlight"
	["All" gnus-article-highlight t]
	["Headers" gnus-article-highlight-headers t]
	["Signature" gnus-article-highlight-signature t]
	["Citation" gnus-article-highlight-citation t])
       ("Date"
	["Local" gnus-article-date-local t]
	["UT" gnus-article-date-ut t]
	["Original" gnus-article-date-original t]
	["Lapsed" gnus-article-date-lapsed t])
       ("Filter"
	["Overstrike" gnus-article-treat-overstrike t]
	["Word wrap" gnus-article-fill-cited-article t]
	["CR" gnus-article-remove-cr t]
	["Trailing blank lines" gnus-article-remove-trailing-blank-lines t]
	["Show X-Face" gnus-article-display-x-face t]
	["Quoted-Printable" gnus-article-de-quoted-unreadable t]
	["Rot 13" gnus-summary-caesar-message t]
	["Add buttons" gnus-article-add-buttons t]
	["Add buttons to head" gnus-article-add-buttons-to-head t]
	["Stop page breaking" gnus-summary-stop-page-breaking t]
	["Toggle MIME" gnus-summary-toggle-mime t]
	["Verbose header" gnus-summary-verbose-headers t]
	["Toggle header" gnus-summary-toggle-header t])
       ("Output"
	["Save in default format" gnus-summary-save-article t]
	["Save in file" gnus-summary-save-article-file t]
	["Save in Unix mail format" gnus-summary-save-article-mail t]
	["Save in MH folder" gnus-summary-save-article-folder t]
	["Save in VM folder" gnus-summary-save-article-vm t]
	["Save in RMAIL mbox" gnus-summary-save-article-rmail t]
	["Save body in file" gnus-summary-save-article-body-file t]
	["Pipe through a filter" gnus-summary-pipe-output t]
	["Add to SOUP packet" gnus-soup-add-article t])
       ("Backend"
	["Respool article..." gnus-summary-respool-article t]
	["Move article..." gnus-summary-move-article
	 (gnus-check-backend-function
	  'request-move-article gnus-newsgroup-name)]
	["Copy article..." gnus-summary-copy-article t]
	["Crosspost article..." gnus-summary-crosspost-article
	 (gnus-check-backend-function
	  'request-replace-article gnus-newsgroup-name)]
	["Import file..." gnus-summary-import-article t]
	["Edit article" gnus-summary-edit-article
	 (not (gnus-group-read-only-p))]
	["Delete article" gnus-summary-delete-article
	 (gnus-check-backend-function
	  'request-expire-articles gnus-newsgroup-name)]
	["Query respool" gnus-summary-respool-query t]
	["Delete expirable articles" gnus-summary-expire-articles-now
	 (gnus-check-backend-function
	  'request-expire-articles gnus-newsgroup-name)])
       ("Extract"
	["Uudecode" gnus-uu-decode-uu t]
	["Uudecode and save" gnus-uu-decode-uu-and-save t]
	["Unshar" gnus-uu-decode-unshar t]
	["Unshar and save" gnus-uu-decode-unshar-and-save t]
	["Save" gnus-uu-decode-save t]
	["Binhex" gnus-uu-decode-binhex t]
	["Postscript" gnus-uu-decode-postscript t])
       ["Enter digest buffer" gnus-summary-enter-digest-group t]
       ["Isearch article..." gnus-summary-isearch-article t]
       ["Search articles forward..." gnus-summary-search-article-forward t]
       ["Search articles backward..." gnus-summary-search-article-backward t]
       ["Beginning of the article" gnus-summary-beginning-of-article t]
       ["End of the article" gnus-summary-end-of-article t]
       ["Fetch parent of article" gnus-summary-refer-parent-article t]
       ["Fetch referenced articles" gnus-summary-refer-references t]
       ["Fetch article with id..." gnus-summary-refer-article t]
       ["Redisplay" gnus-summary-show-article t]))

    (easy-menu-define
     gnus-summary-thread-menu gnus-summary-mode-map ""
     '("Threads"
       ["Toggle threading" gnus-summary-toggle-threads t]
       ["Hide threads" gnus-summary-hide-all-threads t]
       ["Show threads" gnus-summary-show-all-threads t]
       ["Hide thread" gnus-summary-hide-thread t]
       ["Show thread" gnus-summary-show-thread t]
       ["Go to next thread" gnus-summary-next-thread t]
       ["Go to previous thread" gnus-summary-prev-thread t]
       ["Go down thread" gnus-summary-down-thread t]
       ["Go up thread" gnus-summary-up-thread t]
       ["Top of thread" gnus-summary-top-thread t]
       ["Mark thread as read" gnus-summary-kill-thread t]
       ["Lower thread score" gnus-summary-lower-thread t]
       ["Raise thread score" gnus-summary-raise-thread t]
       ["Rethread current" gnus-summary-rethread-current t]
       ))

    (easy-menu-define
     gnus-summary-post-menu gnus-summary-mode-map ""
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
       ["Resend message" gnus-summary-resend-message t]
       ["Send bounced mail" gnus-summary-resend-bounced-mail t]
       ["Send a mail" gnus-summary-mail-other-window t]
       ["Uuencode and post" gnus-uu-post-news t]
       ;;("Draft"
       ;;["Send" gnus-summary-send-draft t]
       ;;["Send bounced" gnus-resend-bounced-mail t])
       ))
    (run-hooks 'gnus-summary-menu-hook)
    ))

(defun gnus-score-set-default (var value)
  "A version of set that updates the GNU Emacs menu-bar."
  (set var value)
  ;; It is the message that forces the active status to be updated.
  (message ""))

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
				(list (caar ts))
				(let ((ps perms)
				      outp)
				  (while ps
				    (setq outp
					  (cons
					   (vector
					    (caar ps) 
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
      gnus-article-article-menu gnus-article-mode-map ""
      '("Article"
	["Scroll forwards" gnus-article-goto-next-page t]
	["Scroll backwards" gnus-article-goto-prev-page t]
	["Show summary" gnus-article-show-summary t]
	["Fetch Message-ID at point" gnus-article-refer-article t]
	["Mail to address at point" gnus-article-mail t]
	))

     (easy-menu-define
      gnus-article-treatment-menu gnus-article-mode-map ""
      '("Treatment"
	["Hide headers" gnus-article-hide-headers t]
	["Hide signature" gnus-article-hide-signature t]
	["Hide citation" gnus-article-hide-citation t]
	["Treat overstrike" gnus-article-treat-overstrike t]
	["Remove carriage return" gnus-article-remove-cr t]
	["Remove quoted-unreadable" gnus-article-de-quoted-unreadable t]
	))
     (run-hooks 'gnus-article-menu-hook))))

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
	       (from (if (get-text-property beg gnus-mouse-face-prop) 
			 beg
		       (1+ (or (next-single-property-change 
				beg gnus-mouse-face-prop nil end) 
			       beg))))
	       (to (1- (or (next-single-property-change
			    from gnus-mouse-face-prop nil end)
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
	 (article (gnus-summary-article-number))
	 (score (or (cdr (assq (or article gnus-current-article)
			       gnus-newsgroup-scored))
		    gnus-summary-default-score 0))
	 (mark (or (gnus-summary-article-mark) gnus-unread-mark))
	 (inhibit-read-only t))
    ;; Eval the cars of the lists until we find a match.
    (let ((default gnus-summary-default-score))
      (while (and list
		  (not (eval (caar list))))
	(setq list (cdr list))))
    (let ((face (cdar list)))
      (unless (eq face (get-text-property beg 'face))
	(gnus-put-text-property 
	 beg end 'face 
	 (setq face (if (boundp face) (symbol-value face) face)))
	(when gnus-summary-highlight-line-function
	  (funcall gnus-summary-highlight-line-function article face))))
    (goto-char p)))

(defun gnus-group-highlight-line ()
  "Highlight the current line according to `gnus-group-highlight'."
  (let* ((list gnus-group-highlight)
	 (p (point))
	 (end (progn (end-of-line) (point)))
	 ;; now find out where the line starts and leave point there.
	 (beg (progn (beginning-of-line) (point)))
	 (group (gnus-group-group-name))
	 (entry (gnus-group-entry group))
	 (unread (if (numberp (car entry)) (car entry) 0))
	 (info (nth 2 entry))
	 (method (gnus-server-get-method group (gnus-info-method info)))
	 (marked (gnus-info-marks info))
	 (mailp (memq 'mail (assoc (symbol-name
				    (car (or method gnus-select-method)))
				   gnus-valid-select-methods)))
	 (level (or (gnus-info-level info) 9))
	 (score (or (gnus-info-score info) 0))
	 (ticked (gnus-range-length (cdr (assq 'tick marked))))
	 (inhibit-read-only t))
    ;; Eval the cars of the lists until we find a match.
    (while (and list
		(not (eval (caar list))))
      (setq list (cdr list)))
    (let ((face (cdar list)))
      (unless (eq face (get-text-property beg 'face))
	(gnus-put-text-property 
	 beg end 'face 
	 (setq face (if (boundp face) (symbol-value face) face)))
	(gnus-extent-start-open beg)))
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
		(gnus-set-text-properties
		 (point)
		 (prog2 (insert button) (point) (insert " "))
		 (list 'face gnus-carpal-header-face))
	      (gnus-set-text-properties
	       (point)
	       (prog2 (insert (car button)) (point) (insert " "))
	       (list 'gnus-callback (cdr button)
		     'face gnus-carpal-button-face
		     gnus-mouse-face-prop 'highlight))))
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

(defun gnus-article-prev-button (n)
  "Move point to N buttons backward.
If N is negative, move forward instead."
  (interactive "p")
  (gnus-article-next-button (- n)))

(defun gnus-article-next-button (n)
  "Move point to N buttons forward.
If N is negative, move backward instead."
  (interactive "p")
  (let ((function (if (< n 0) 'previous-single-property-change
		    'next-single-property-change))
	(inhibit-point-motion-hooks t)
	(backward (< n 0))
	(limit (if (< n 0) (point-min) (point-max))))
    (setq n (abs n))
    (while (and (not (= limit (point)))
		(> n 0))
      ;; Skip past the current button.
      (when (get-text-property (point) 'gnus-callback)
	(goto-char (funcall function (point) 'gnus-callback nil limit)))
      ;; Go to the next (or previous) button.
      (gnus-goto-char (funcall function (point) 'gnus-callback nil limit))
      ;; Put point at the start of the button.
      (when (and backward (not (get-text-property (point) 'gnus-callback)))
	(goto-char (funcall function (point) 'gnus-callback nil limit)))
      ;; Skip past intangible buttons.
      (when (get-text-property (point) 'intangible)
	(incf n))
      (decf n))
    (unless (zerop n)
      (gnus-message 5 "No more buttons"))
    n))

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
  (gnus-article-add-buttons force)
  (gnus-article-add-buttons-to-head))

(defun gnus-article-highlight-some (&optional force)
  "Highlight current article.
This function calls `gnus-article-highlight-headers',
`gnus-article-highlight-signature', and `gnus-article-add-buttons' to
do the highlighting.  See the documentation for those functions."
  (interactive (list 'force))
  (gnus-article-highlight-headers)
  (gnus-article-highlight-signature)
  (gnus-article-add-buttons))

(defun gnus-article-highlight-headers ()
  "Highlight article headers as specified by `gnus-header-face-alist'."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (let ((alist gnus-header-face-alist)
	    (buffer-read-only nil)
	    (case-fold-search t)
	    (inhibit-point-motion-hooks t)
	    entry regexp header-face field-face from hpoints fpoints)
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (narrow-to-region (1- (point)) (point-min))
	  (while (setq entry (pop alist))
	    (goto-char (point-min))
	    (setq regexp (concat "^\\("
				 (if (string-equal "" (nth 0 entry))
				     "[^\t ]"
				   (nth 0 entry))
				 "\\)")
		  header-face (nth 1 entry)
		  field-face (nth 2 entry))
	    (while (and (re-search-forward regexp nil t)
			(not (eobp)))
	      (beginning-of-line)
	      (setq from (point))
	      (or (search-forward ":" nil t)
		  (forward-char 1))
	      (when (and header-face
			 (not (memq (point) hpoints)))
		(push (point) hpoints)
		(gnus-put-text-property from (point) 'face header-face))
	      (when (and field-face
			 (not (memq (setq from (point)) fpoints)))
		(push from fpoints)
		(if (re-search-forward "^[^ \t]" nil t)
		    (forward-char -2)
		  (goto-char (point-max)))
		(gnus-put-text-property from (point) 'face field-face)))))))))

(defun gnus-article-highlight-signature ()
  "Highlight the signature in an article.
It does this by highlighting everything after
`gnus-signature-separator' using `gnus-signature-face'." 
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t))
      (save-restriction
	(when (and gnus-signature-face
		   (gnus-narrow-to-signature))
	  (gnus-overlay-put (gnus-make-overlay (point-min) (point-max))
			    'face gnus-signature-face)
	  (widen)
	  (re-search-backward gnus-signature-separator nil t)
	  (let ((start (match-beginning 0))
		(end (set-marker (make-marker) (1+ (match-end 0)))))
	    (gnus-article-add-button start (1- end) 'gnus-signature-toggle
				     end)))))))

(defun gnus-article-add-buttons (&optional force)
  "Find external references in the article and make buttons of them.
\"External references\" are things like Message-IDs and URLs, as
specified by `gnus-button-alist'."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    ;; Remove all old markers.
    (while gnus-button-marker-list
      (set-marker (pop gnus-button-marker-list) nil))
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist gnus-button-alist)
	  beg entry regexp)
      (goto-char (point-min))
      ;; We skip the headers.
      (unless (search-forward "\n\n" nil t)
	(goto-char (point-max)))
      (setq beg (point))
      (while (setq entry (pop alist))
	(setq regexp (car entry))
	(goto-char beg)
	(while (re-search-forward regexp nil t)
	  (let* ((start (and entry (match-beginning (nth 1 entry))))
		 (end (and entry (match-end (nth 1 entry))))
		 (from (match-beginning 0)))
	    (when (or (eq t (nth 1 entry))
		      (eval (nth 1 entry)))
	      ;; That optional form returned non-nil, so we add the
	      ;; button. 
	      (gnus-article-add-button 
	       start end 'gnus-button-push 
	       (car (push (set-marker (make-marker) from)
			  gnus-button-marker-list))))))))))

;; Add buttons to the head of an article.
(defun gnus-article-add-buttons-to-head ()
  "Add buttons to the head of the article."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist gnus-header-button-alist)
	  entry beg end)
      (nnheader-narrow-to-headers)
      (while alist
	;; Each alist entry.
	(setq entry (car alist)
	      alist (cdr alist))
	(goto-char (point-min))
	(while (re-search-forward (car entry) nil t)
	  ;; Each header matching the entry.
	  (setq beg (match-beginning 0))
	  (setq end (or (and (re-search-forward "^[^ \t]" nil t)
			     (match-beginning 0))
			(point-max)))
	  (goto-char beg)
	  (while (re-search-forward (nth 1 entry) end t)
	    ;; Each match within a header.
	    (let* ((from (match-beginning 0))
		   (entry (cdr entry))
		   (start (match-beginning (nth 1 entry)))
		   (end (match-end (nth 1 entry)))
		   (form (nth 2 entry)))
	      (goto-char (match-end 0))
	      (and (eval form)
		   (gnus-article-add-button 
		    start end (nth 3 entry)
		    (buffer-substring (match-beginning (nth 4 entry))
				      (match-end (nth 4 entry)))))))
	  (goto-char end))))
    (widen)))

;;; External functions:

(defun gnus-article-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (and gnus-article-button-face
       (gnus-overlay-put (gnus-make-overlay from to)
			 'face gnus-article-button-face))
  (gnus-add-text-properties 
   from to
   (nconc (and gnus-article-mouse-face
	       (list gnus-mouse-face-prop gnus-article-mouse-face))
	  (list 'gnus-callback fun)
	  (and data (list 'gnus-data data)))))

;;; Internal functions:

(defun gnus-signature-toggle (end)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t))
      (if (get-text-property end 'invisible)
	  (gnus-unhide-text end (point-max))
	(gnus-hide-text end (point-max) gnus-hidden-properties)))))

(defun gnus-button-entry ()
  ;; Return the first entry in `gnus-button-alist' matching this place.
  (let ((alist gnus-button-alist)
	(entry nil))
    (while alist
      (setq entry (pop alist))
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
			     (gnus-set-text-properties
			      0 (length string) nil string)
			     string))
			 (nthcdr 4 entry))))
      (cond
       ((fboundp fun)
	(apply fun args))
       ((and (boundp fun)
	     (fboundp (symbol-value fun)))
	(apply (symbol-value fun) args))
       (t
	(gnus-message 1 "You must define `%S' to use this button"
		      (cons fun args)))))))

(defun gnus-button-message-id (message-id)
  "Fetch MESSAGE-ID."
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (gnus-summary-refer-article message-id)))

(defun gnus-button-mailto (address)
  ;; Mail to ADDRESS.
  (set-buffer (gnus-copy-article-buffer))
  (message-reply address))

(defun gnus-button-reply (address)
  ;; Reply to ADDRESS.
  (message-reply address))

(defun gnus-button-url (address)
  "Browse ADDRESS."
  (funcall browse-url-browser-function
	   ;; Zap whitespace in case <URL:...> contained it.
	   ;; (Whitespace illegal in raw URL.)
	   (let ((stripped-address address))
	     (while (string-match "\\s +\\|\n+" stripped-address)
	       (setq stripped-address (replace-match "" t t stripped-address)))
	     stripped-address)))

;;; Next/prev buttons in the article buffer.

(defvar gnus-next-page-line-format "%{%(Next page...%)%}\n")
(defvar gnus-prev-page-line-format "%{%(Previous page...%)%}\n")

(defvar gnus-prev-page-map nil)
(unless gnus-prev-page-map
  (setq gnus-prev-page-map (make-sparse-keymap))
  (define-key gnus-prev-page-map gnus-mouse-2 'gnus-button-prev-page)
  (define-key gnus-prev-page-map "\r" 'gnus-button-prev-page))

(defun gnus-insert-prev-page-button ()
  (let ((buffer-read-only nil))
    (gnus-eval-format 
     gnus-prev-page-line-format nil
     `(gnus-prev t local-map ,gnus-prev-page-map
		 gnus-callback gnus-article-button-prev-page))))

(defvar gnus-next-page-map nil)
(unless gnus-next-page-map
  (setq gnus-next-page-map (make-keymap))
  (suppress-keymap gnus-prev-page-map)
  (define-key gnus-next-page-map gnus-mouse-2 'gnus-button-next-page)
  (define-key gnus-next-page-map "\r" 'gnus-button-next-page))

(defun gnus-button-next-page ()
  "Go to the next page."
  (interactive)
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-next-page)
    (select-window win)))

(defun gnus-button-prev-page ()
  "Go to the prev page."
  (interactive)
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-prev-page)
    (select-window win)))

(defun gnus-insert-next-page-button ()
  (let ((buffer-read-only nil))
    (gnus-eval-format gnus-next-page-line-format nil
		      `(gnus-next t local-map ,gnus-next-page-map
				  gnus-callback 
				  gnus-article-button-next-page))))

(defun gnus-article-button-next-page (arg)
  "Go to the next page."
  (interactive "P")
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-next-page)
    (select-window win)))

(defun gnus-article-button-prev-page (arg)
  "Go to the prev page."
  (interactive "P")
  (let ((win (selected-window)))
    (select-window (get-buffer-window gnus-article-buffer t))
    (gnus-article-prev-page)
    (select-window win)))

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
