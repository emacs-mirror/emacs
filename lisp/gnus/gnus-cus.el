;;; gnus-cus.el --- customization commands for Gnus
;;
;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: news

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

(require 'wid-edit)
(require 'gnus-score)

;;; Widgets:

;; There should be special validation for this.
(define-widget 'gnus-email-address 'string
  "An email address")

(defun gnus-custom-mode ()
  "Major mode for editing Gnus customization buffers.

The following commands are available:

\\[widget-forward]		Move to next button or editable field.
\\[widget-backward]		Move to previous button or editable field.
\\[widget-button-click]		Activate button under the mouse pointer.
\\[widget-button-press]		Activate button under point.

Entry to this mode calls the value of `gnus-custom-mode-hook'
if that value is non-nil."
  (kill-all-local-variables)
  (setq major-mode 'gnus-custom-mode
	mode-name "Gnus Customize")
  (use-local-map widget-keymap)
  (run-hooks 'gnus-custom-mode-hook))

;;; Group Customization:

(defconst gnus-group-parameters
  '((to-address (gnus-email-address :tag "To Address") "\
This will be used when doing followups and posts.

This is primarily useful in mail groups that represent closed
mailing lists--mailing lists where it's expected that everybody that
writes to the mailing list is subscribed to it.  Since using this
parameter ensures that the mail only goes to the mailing list itself,
it means that members won't receive two copies of your followups.

Using `to-address' will actually work whether the group is foreign or
not.  Let's say there's a group on the server that is called
`fa.4ad-l'.  This is a real newsgroup, but the server has gotten the
articles from a mail-to-news gateway.  Posting directly to this group
is therefore impossible--you have to send mail to the mailing list
address instead.")

    (to-list (gnus-email-address :tag "To List") "\
This address will be used when doing a `a' in the group.

It is totally ignored when doing a followup--except that if it is
present in a news group, you'll get mail group semantics when doing
`f'.")

    (broken-reply-to (const :tag "Broken Reply To" t) "\
Ignore `Reply-To' headers in this group.

That can be useful if you're reading a mailing list group where the
listserv has inserted `Reply-To' headers that point back to the
listserv itself.  This is broken behavior.  So there!")

    (to-group (string :tag "To Group") "\
All posts will be send to the specified group.")

    (gcc-self (choice :tag  "GCC"
		      :value t
		      (const t)
		      (const none)
		      (string :format "%v" :hide-front-space t)) "\
Specify default value for GCC header.

If this symbol is present in the group parameter list and set to `t',
new composed messages will be `Gcc''d to the current group. If it is
present and set to `none', no `Gcc:' header will be generated, if it
is present and a string, this string will be inserted literally as a
`gcc' header (this symbol takes precedence over any default `Gcc'
rules as described later).")

    (auto-expire (const :tag "Automatic Expire" t) "\
All articles that are read will be marked as expirable.")

    (total-expire (const :tag "Total Expire" t) "\
All read articles will be put through the expiry process

This happens even if they are not marked as expirable.
Use with caution.")

    (expiry-wait (choice :tag  "Expire Wait"
			 :value never
			 (const never)
			 (const immediate)
			 (number :hide-front-space t
				 :format "%v")) "\
When to expire.

Overrides any `nnmail-expiry-wait' and `nnmail-expiry-wait-function'
when expiring expirable messages. The value can either be a number of
days (not necessarily an integer) or the symbols `never' or
`immediate'.")

    (score-file (file :tag "Score File") "\
Make the specified file into the current score file.
This means that all score commands you issue will end up in this file.")

    (adapt-file (file :tag "Adapt File") "\
Make the specified file into the current adaptive file.
All adaptive score entries will be put into this file.")

    (admin-address (gnus-email-address :tag "Admin Address") "\
Administration address for a mailing list.

When unsubscribing to a mailing list you should never send the
unsubscription notice to the mailing list itself.  Instead, you'd
send messages to the administrative address.  This parameter allows
you to put the admin address somewhere convenient.")

    (display (choice :tag "Display"
		     :value default
		     (const all)
		     (const default)) "\
Which articles to display on entering the group.

`all'
     Display all articles, both read and unread.

`default'
     Display the default visible articles, which normally includes
     unread and ticked articles.")

    (comment (string :tag  "Comment") "\
An arbitrary comment on the group."))
  "Alist of valid group parameters.

Each entry has the form (NAME TYPE DOC), where NAME is the parameter
itself (a symbol), TYPE is the parameters type (a sexp widget), and
DOC is a documentation string for the parameter.")

(defvar gnus-custom-params)
(defvar gnus-custom-method)
(defvar gnus-custom-group)

(defun gnus-group-customize (group &optional part)
  "Edit the group on the current line."
  (interactive (list (gnus-group-group-name)))
  (let ((part (or part 'info))
	info
	(types (mapcar (lambda (entry)
			 `(cons :format "%v%h\n"
				:doc ,(nth 2 entry)
				(const :format "" ,(nth 0 entry))
				,(nth 1 entry)))
		       gnus-group-parameters)))
    (unless group
      (error "No group on current line"))
    (unless (setq info (gnus-get-info group))
      (error "Killed group; can't be edited"))
    ;; Ready.
    (kill-buffer (get-buffer-create "*Gnus Customize*"))
    (switch-to-buffer (get-buffer-create "*Gnus Customize*"))
    (gnus-custom-mode)
    (make-local-variable 'gnus-custom-group)
    (setq gnus-custom-group group)
    (widget-insert "Customize the ")
    (widget-create 'info-link
		   :help-echo "Push me to learn more."
		   :tag "group parameters"
		   "(gnus)Group Parameters")
    (widget-insert " for <")
    (widget-insert group)
    (widget-insert "> and press ")
    (widget-create 'push-button
		   :tag "done"
		   :help-echo "Push me when done customizing."
		   :action 'gnus-group-customize-done)
    (widget-insert ".\n\n")
    (make-local-variable 'gnus-custom-params)
    (setq gnus-custom-params
	  (widget-create 'group
			 :value (gnus-info-params info)
			 `(set :inline t
			       :greedy t
			       :tag "Parameters"
			       :format "%t:\n%h%v"
			       :doc "\
These special paramerters are recognized by Gnus.
Check the [ ] for the parameters you want to apply to this group, then
edit the value to suit your taste."
			       ,@types)
			 '(repeat :inline t
				  :tag "Variables"
				  :format "%t:\n%h%v%i\n\n"
				  :doc "\
Set variables local to the group you are entering.

If you want to turn threading off in `news.answers', you could put
`(gnus-show-threads nil)' in the group parameters of that group.
`gnus-show-threads' will be made into a local variable in the summary
buffer you enter, and the form `nil' will be `eval'ed there.

This can also be used as a group-specific hook function, if you'd
like.  If you want to hear a beep when you enter a group, you could
put something like `(dummy-variable (ding))' in the parameters of that
group.  `dummy-variable' will be set to the result of the `(ding)'
form, but who cares?"
				  (group :value (nil nil)
					 (symbol :tag "Variable")
					 (sexp :tag
					       "Value")))

			 '(repeat :inline t
				  :tag "Unknown entries"
				  sexp)))
    (widget-insert "\n\nYou can also edit the ")
    (widget-create 'info-link
		   :tag "select method"
		   :help-echo "Push me to learn more about select methods."
		   "(gnus)Select Methods")
    (widget-insert " for the group.\n")
    (setq gnus-custom-method
	  (widget-create 'sexp
			 :tag "Method"
			 :value (gnus-info-method info)))
    (use-local-map widget-keymap)
    (widget-setup)))

(defun gnus-group-customize-done (&rest ignore)
  "Apply changes and bury the buffer."
  (interactive)
  (gnus-group-edit-group-done 'params gnus-custom-group
			      (widget-value gnus-custom-params))
  (gnus-group-edit-group-done 'method gnus-custom-group
			      (widget-value gnus-custom-method))
  (bury-buffer))

;;; Score Customization:

(defconst gnus-score-parameters
  '((mark (number :tag "Mark") "\
The value of this entry should be a number.
Any articles with a score lower than this number will be marked as read.")

    (expunge (number :tag "Expunge") "\
The value of this entry should be a number.
Any articles with a score lower than this number will be removed from
the summary buffer.")

    (mark-and-expunge (number :tag "Mark-and-expunge") "\
The value of this entry should be a number.
Any articles with a score lower than this number will be marked as
read and removed from the summary buffer.")

    (thread-mark-and-expunge (number :tag "Thread-mark-and-expunge") "\
The value of this entry should be a number.
All articles that belong to a thread that has a total score below this
number will be marked as read and removed from the summary buffer.
`gnus-thread-score-function' says how to compute the total score
for a thread.")

    (files (repeat :tag "Files" file) "\
The value of this entry should be any number of file names.
These files are assumed to be score files as well, and will be loaded
the same way this one was.")

    (exclude-files (repeat :tag "Exclude-files" file) "\
The clue of this entry should be any number of files.
These files will not be loaded, even though they would normally be so,
for some reason or other.")

    (eval (sexp :tag "Eval" :value nil) "\
The value of this entry will be `eval'el.
This element will be ignored when handling global score files.")

    (read-only (boolean :tag "Read-only" :value t) "\
Read-only score files will not be updated or saved.
Global score files should feature this atom.")

    (orphan (number :tag "Orphan") "\
The value of this entry should be a number.
Articles that do not have parents will get this number added to their
scores.  Imagine you follow some high-volume newsgroup, like
`comp.lang.c'.  Most likely you will only follow a few of the threads,
also want to see any new threads.

You can do this with the following two score file entries:

     (orphan -500)
     (mark-and-expunge -100)

When you enter the group the first time, you will only see the new
threads.  You then raise the score of the threads that you find
interesting (with `I T' or `I S'), and ignore (`C y') the rest.
Next time you enter the group, you will see new articles in the
interesting threads, plus any new threads.

I.e.---the orphan score atom is for high-volume groups where there
exist a few interesting threads which can't be found automatically
by ordinary scoring rules.")

    (adapt (choice :tag "Adapt"
		   (const t)
		   (const ignore)
		   (sexp :format "%v"
			 :hide-front-space t)) "\
This entry controls the adaptive scoring.
If it is `t', the default adaptive scoring rules will be used.  If it
is `ignore', no adaptive scoring will be performed on this group.  If
it is a list, this list will be used as the adaptive scoring rules.
If it isn't present, or is something other than `t' or `ignore', the
default adaptive scoring rules will be used.  If you want to use
adaptive scoring on most groups, you'd set `gnus-use-adaptive-scoring'
to `t', and insert an `(adapt ignore)' in the groups where you do not
want adaptive scoring.  If you only want adaptive scoring in a few
groups, you'd set `gnus-use-adaptive-scoring' to `nil', and insert
`(adapt t)' in the score files of the groups where you want it.")

    (adapt-file (file :tag "Adapt-file") "\
All adaptive score entries will go to the file named by this entry.
It will also be applied when entering the group.  This atom might
be handy if you want to adapt on several groups at once, using the
same adaptive file for a number of groups.")

    (local (repeat :tag "Local"
		   (group :value (nil nil)
			  (symbol :tag "Variable")
			  (sexp :tag "Value"))) "\
The value of this entry should be a list of `(VAR VALUE)' pairs.
Each VAR will be made buffer-local to the current summary buffer,
and set to the value specified.  This is a convenient, if somewhat
strange, way of setting variables in some groups if you don't like
hooks much.")
    (touched (sexp :format "Touched\n") "Internal variable."))
  "Alist of valid symbolic score parameters.

Each entry has the form (NAME TYPE DOC), where NAME is the parameter
itself (a symbol), TYPE is the parameters type (a sexp widget), and DOC is a
documentation string for the parameter.")

(define-widget 'gnus-score-string 'group
  "Edit score entries for string-valued headers."
  :convert-widget 'gnus-score-string-convert)

(defun gnus-score-string-convert (widget)
  ;; Set args appropriately.
  (let* ((tag (widget-get widget :tag))
	 (item `(const :format "" :value ,(downcase tag)))
	 (match '(string :tag "Match"))
	 (score '(choice :tag "Score"
			(const :tag "default" nil)
			(integer :format "%v"
				 :hide-front-space t)))
	 (expire '(choice :tag "Expire"
			  (const :tag "off" nil)
			  (integer :format "%v"
				   :hide-front-space t)))
	 (type '(choice :tag "Type"
			:value s
			;; I should really create a forgiving :match
			;; function for each type below, that only
			;; looked at the first letter.
			(const :tag "Regexp" r)
			(const :tag "Regexp (fixed case)" R)
			(const :tag "Substring" s)
			(const :tag "Substring (fixed case)" S)
			(const :tag "Exact" e)
			(const :tag "Exact (fixed case)" E)
			(const :tag "Word" w)
			(const :tag "Word (fixed case)" W)
			(const :tag "default" nil)))
	 (group `(group ,match ,score ,expire ,type))
	 (doc (concat (or (widget-get widget :doc)
			  (concat "Change score based on the " tag
				  " header.\n"))
		      "
You can have an arbitrary number of score entries for this header,
each score entry has four elements:

1. The \"match element\".  This should be the string to look for in the
   header.

2. The \"score element\".  This number should be an integer in the
   neginf to posinf interval.  This number is added to the score
   of the article if the match is successful.  If this element is
   not present, the `gnus-score-interactive-default-score' number
   will be used instead.  This is 1000 by default.

3. The \"date element\".  This date says when the last time this score
   entry matched, which provides a mechanism for expiring the
   score entries.  It this element is not present, the score
   entry is permanent.  The date is represented by the number of
   days since December 31, 1 ce.

4. The \"type element\".  This element specifies what function should
   be used to see whether this score entry matches the article.

   There are the regexp, as well as substring types, and exact match,
   and word match types.  If this element is not present, Gnus will
   assume that substring matching should be used.  There is case
   sensitive variants of all match types.")))
    (widget-put widget :args `(,item
			       (repeat :inline t
				       :indent 0
				       :tag ,tag
				       :doc ,doc
				       :format "%t:\n%h%v%i\n\n"
				       (choice :format "%v"
					       :value ("" nil nil s)
					       ,group
					       sexp)))))
  widget)

(define-widget 'gnus-score-integer 'group
  "Edit score entries for integer-valued headers."
  :convert-widget 'gnus-score-integer-convert)

(defun gnus-score-integer-convert (widget)
  ;; Set args appropriately.
  (let* ((tag (widget-get widget :tag))
	 (item `(const :format "" :value ,(downcase tag)))
	 (match '(integer :tag "Match"))
	 (score '(choice :tag "Score"
			(const :tag "default" nil)
			(integer :format "%v"
				 :hide-front-space t)))
	 (expire '(choice :tag "Expire"
			  (const :tag "off" nil)
			  (integer :format "%v"
				   :hide-front-space t)))
	 (type '(choice :tag "Type"
			:value <
			(const <)
			(const >)
			(const =)
			(const >=)
			(const <=)))
	 (group `(group ,match ,score ,expire ,type))
	 (doc (concat (or (widget-get widget :doc)
			  (concat "Change score based on the " tag
				  " header.")))))
    (widget-put widget :args `(,item
			       (repeat :inline t
				       :indent 0
				       :tag ,tag
				       :doc ,doc
				       :format "%t:\n%h%v%i\n\n"
				       ,group))))
  widget)

(define-widget 'gnus-score-date 'group
  "Edit score entries for date-valued headers."
  :convert-widget 'gnus-score-date-convert)

(defun gnus-score-date-convert (widget)
  ;; Set args appropriately.
  (let* ((tag (widget-get widget :tag))
	 (item `(const :format "" :value ,(downcase tag)))
	 (match '(string :tag "Match"))
	 (score '(choice :tag "Score"
			(const :tag "default" nil)
			(integer :format "%v"
				 :hide-front-space t)))
	 (expire '(choice :tag "Expire"
			  (const :tag "off" nil)
			  (integer :format "%v"
				   :hide-front-space t)))
	 (type '(choice :tag "Type"
			:value regexp
			(const regexp)
			(const before)
			(const at)
			(const after)))
	 (group `(group ,match ,score ,expire ,type))
	 (doc (concat (or (widget-get widget :doc)
			  (concat "Change score based on the " tag
				  " header."))
		      "
For the Date header we have three kinda silly match types: `before',
`at' and `after'.  I can't really imagine this ever being useful, but,
like, it would feel kinda silly not to provide this function.  Just in
case.  You never know.  Better safe than sorry.  Once burnt, twice
shy.  Don't judge a book by its cover.  Never not have sex on a first
date.  (I have been told that at least one person, and I quote,
\"found this function indispensable\", however.)

A more useful match type is `regexp'.  With it, you can match the date
string using a regular expression.  The date is normalized to ISO8601
compact format first---`YYYYMMDDTHHMMSS'.  If you want to match all
articles that have been posted on April 1st in every year, you could
use `....0401.........' as a match string, for instance.  (Note that
the date is kept in its original time zone, so this will match
articles that were posted when it was April 1st where the article was
posted from.  Time zones are such wholesome fun for the whole family,
eh?")))
    (widget-put widget :args `(,item
			       (repeat :inline t
				       :indent 0
				       :tag ,tag
				       :doc ,doc
				       :format "%t:\n%h%v%i\n\n"
				       ,group))))
  widget)

(defvar gnus-custom-scores)
(defvar gnus-custom-score-alist)

(defun gnus-score-customize (file)
  "Customize score file FILE."
  (interactive (list gnus-current-score-file))
  (let ((scores (gnus-score-load file))
	(types (mapcar (lambda (entry)
		 `(group :format "%v%h\n"
			 :doc ,(nth 2 entry)
			 (const :format "" ,(nth 0 entry))
			 ,(nth 1 entry)))
	       gnus-score-parameters)))
    ;; Ready.
    (kill-buffer (get-buffer-create "*Gnus Customize*"))
    (switch-to-buffer (get-buffer-create "*Gnus Customize*"))
    (gnus-custom-mode)
    (make-local-variable 'gnus-custom-score-alist)
    (setq gnus-custom-score-alist scores)
    (widget-insert "Customize the ")
    (widget-create 'info-link
		   :help-echo "Push me to learn more."
		   :tag "score entries"
		   "(gnus)Score File Format")
    (widget-insert " for\n\t")
    (widget-insert file)
    (widget-insert "\nand press ")
    (widget-create 'push-button
		   :tag "done"
		   :help-echo "Push me when done customizing."
		   :action 'gnus-score-customize-done)
    (widget-insert ".\n
Check the [ ] for the entries you want to apply to this score file, then
edit the value to suit your taste.  Don't forget to mark the checkbox,
if you do all your changes will be lost.  ")
    (widget-create 'push-button
		   :action (lambda (&rest ignore)
			     (require 'gnus-audio)
			     (gnus-audio-play "Evil_Laugh.au"))
		   "Bhahahah!")
    (widget-insert "\n\n")
    (make-local-variable 'gnus-custom-scores)
    (setq gnus-custom-scores
	  (widget-create 'group
			 :value scores
			 `(checklist :inline t
				     :greedy t
				     (gnus-score-string :tag "From")
				     (gnus-score-string :tag "Subject")
				     (gnus-score-string :tag "References")
				     (gnus-score-string :tag "Xref")
				     (gnus-score-string :tag "Message-ID")
				     (gnus-score-integer :tag "Lines")
				     (gnus-score-integer :tag "Chars")
				     (gnus-score-date :tag "Date")
				     (gnus-score-string :tag "Head"
							:doc "\
Match all headers in the article.

Using one of `Head', `Body', `All' will slow down scoring considerable.
")
				     (gnus-score-string :tag "Body"
							:doc "\
Match the body sans header of the article.

Using one of `Head', `Body', `All' will slow down scoring considerable.
")
				     (gnus-score-string :tag "All"
							:doc "\
Match the entire article, including both headers and body.

Using one of `Head', `Body', `All' will slow down scoring
considerable.
")
				     (gnus-score-string :tag
							"Followup"
							:doc "\
Score all followups to the specified authors.

This entry is somewhat special, in that it will match the `From:'
header, and affect the score of not only the matching articles, but
also all followups to the matching articles.  This allows you
e.g. increase the score of followups to your own articles, or decrease
the score of followups to the articles of some known trouble-maker.
")
				     (gnus-score-string :tag "Thread"
							:doc "\
Add a score entry on all articles that are part of a thread.

This match key works along the same lines as the `Followup' match key.
If you say that you want to score on a (sub-)thread that is started by
an article with a `Message-ID' X, then you add a `thread' match.  This
will add a new `thread' match for each article that has X in its
`References' header.  (These new `thread' matches will use the
`Message-ID's of these matching articles.)  This will ensure that you
can raise/lower the score of an entire thread, even though some
articles in the thread may not have complete `References' headers.
Note that using this may lead to undeterministic scores of the
articles in the thread.
")
				     ,@types)
			 '(repeat :inline t
				  :tag "Unknown entries"
				  sexp)))
    (use-local-map widget-keymap)
    (widget-setup)))

(defun gnus-score-customize-done (&rest ignore)
  "Reset the score alist with the present value."
  (let ((alist gnus-custom-score-alist)
	(value (widget-value gnus-custom-scores)))
    (setcar alist (car value))
    (setcdr alist (cdr value))
    (gnus-score-set 'touched '(t) alist))
  (bury-buffer))

;;; The End:

(provide 'gnus-cus)

;;; gnus-cus.el ends here

