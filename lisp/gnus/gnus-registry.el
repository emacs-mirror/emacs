;;; gnus-registry.el --- article registry for Gnus

;; Copyright (C) 2002-2018 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: news registry

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

;; This is the gnus-registry.el package, which works with all
;; Gnus backends, not just nnmail.  The major issue is that it
;; doesn't go across backends, so for instance if an article is in
;; nnml:sys and you see a reference to it in nnimap splitting, the
;; article will end up in nnimap:sys

;; gnus-registry.el intercepts article respooling, moving, deleting,
;; and copying for all backends.  If it doesn't work correctly for
;; you, submit a bug report and I'll be glad to fix it.  It needs
;; better documentation in the manual (also on my to-do list).

;; If you want to track recipients (and you should to make the
;; gnus-registry splitting work better), you need the To and Cc
;; headers collected by Gnus.  Note that in more recent Gnus versions
;; this is already the case: look at `gnus-extra-headers' to be sure.

;; ;;; you may also want Gcc Newsgroups Keywords X-Face
;; (add-to-list 'gnus-extra-headers 'To)
;; (add-to-list 'gnus-extra-headers 'Cc)
;; (setq nnmail-extra-headers gnus-extra-headers)

;; Put this in your startup file (~/.gnus.el for instance) or use Customize:

;; (setq gnus-registry-max-entries 2500
;;       gnus-registry-track-extra '(sender subject recipient))

;; (gnus-registry-initialize)

;; Then use this in your fancy-split:

;; (: gnus-registry-split-fancy-with-parent)

;; You should also consider using the nnregistry backend to look up
;; articles.  See the Gnus manual for more information.

;; Finally, you can put %uM in your summary line format to show the
;; registry marks if you do this:

;; show the marks as single characters (see the :char property in
;; `gnus-registry-marks'):
;; (defalias 'gnus-user-format-function-M 'gnus-registry-article-marks-to-chars)

;; show the marks by name (see `gnus-registry-marks'):
;; (defalias 'gnus-user-format-function-M 'gnus-registry-article-marks-to-names)

;; TODO:

;; - get the correct group on spool actions

;; - articles that are spooled to a different backend should be moved
;;   after splitting

;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))

(require 'gnus)
(require 'gnus-int)
(require 'gnus-sum)
(require 'gnus-art)
(require 'gnus-util)
(require 'nnmail)
(require 'easymenu)
(require 'registry)

(defvar gnus-adaptive-word-syntax-table)

(defvar gnus-registry-dirty t
 "Boolean set to t when the registry is modified.")

(defgroup gnus-registry nil
  "The Gnus registry."
  :version "22.1"
  :group 'gnus)

(defvar gnus-registry-marks
  '((Important
     :char ?i
     :image "summary_important")
    (Work
     :char ?w
     :image "summary_work")
    (Personal
     :char ?p
     :image "summary_personal")
    (To-Do
     :char ?t
     :image "summary_todo")
    (Later
     :char ?l
     :image "summary_later"))

  "List of registry marks and their options.

`gnus-registry-mark-article' will offer symbols from this list
for completion.

Each entry must have a character to be useful for summary mode
line display and for keyboard shortcuts.

Each entry must have an image string to be useful for visual
display.")

(defcustom gnus-registry-default-mark 'To-Do
  "The default mark.  Should be a valid key for `gnus-registry-marks'."
  :group 'gnus-registry
  :type 'symbol)

(defcustom gnus-registry-unfollowed-addresses
  (list (regexp-quote user-mail-address))
  "List of addresses that gnus-registry-split-fancy-with-parent won't trace.
The addresses are matched, they don't have to be fully qualified.
In the messages, these addresses can be the sender or the
recipients."
  :version "24.1"
  :group 'gnus-registry
  :type '(repeat regexp))

(defcustom gnus-registry-unfollowed-groups
  '("delayed$" "drafts$" "queue$" "INBOX$" "^nnmairix:" "archive")
  "List of groups that gnus-registry-split-fancy-with-parent won't return.
The group names are matched, they don't have to be fully
qualified.  This parameter tells the Gnus registry 'never split a
message into a group that matches one of these, regardless of
references.'

nnmairix groups are specifically excluded because they are ephemeral."
  :group 'gnus-registry
  :type '(repeat regexp))

(defcustom gnus-registry-install 'ask
  "Whether the registry should be installed."
  :group 'gnus-registry
  :type '(choice (const :tag "Never Install" nil)
                 (const :tag "Always Install" t)
                 (const :tag "Ask Me" ask)))

(defvar gnus-registry-enabled nil)

(defvar gnus-summary-misc-menu) ;; Avoid byte compiler warning.

(defvar gnus-registry-misc-menus nil)   ; ugly way to keep the menus

;; It was simply deleted.
(make-obsolete-variable 'gnus-registry-max-pruned-entries nil "25.1")

(defcustom gnus-registry-track-extra '(subject sender recipient)
  "Whether the registry should track extra data about a message.
The subject, recipients (To: and Cc:), and Sender (From:) headers
are tracked this way by default."
  :group 'gnus-registry
  :type
  '(set :tag "Tracking choices"
    (const :tag "Track by subject (Subject: header)" subject)
    (const :tag "Track by recipient (To: and Cc: headers)" recipient)
    (const :tag "Track by sender (From: header)"  sender)))

(defcustom gnus-registry-split-strategy nil
  "The splitting strategy applied to the keys in `gnus-registry-track-extra'.

Given a set of unique found groups G and counts for each element
of G, and a key K (typically `sender' or `subject'):

When nil, if G has only one element, use it.  Otherwise give up.
This is the fastest but also least useful strategy.

When `majority', use the majority by count.  So if there is a
group with the most articles counted by K, use that.  Ties are
resolved in no particular order, simply the first one found wins.
This is the slowest strategy but also the most accurate one.

When `first', the first element of G wins.  This is fast and
should be OK if your senders and subjects don't \"bleed\" across
groups."
  :group 'gnus-registry
  :type
  '(choice :tag "Splitting strategy"
           (const :tag "Only use single choices, discard multiple matches" nil)
           (const :tag "Majority of matches wins" majority)
           (const :tag "First found wins"  first)))

(defcustom gnus-registry-minimum-subject-length 5
  "The minimum length of a subject before it's considered trackable."
  :group 'gnus-registry
  :type 'integer)

(defcustom gnus-registry-extra-entries-precious '(mark)
  "What extra keys are precious, meaning entries with them won't get pruned.
By default, 'mark is included, so articles with marks are
considered precious.

Before you save the Gnus registry, it's pruned.  Any entries with
keys in this list will not be pruned.  All other entries go to
the Bit Bucket."
  :group 'gnus-registry
  :type '(repeat symbol))

(defcustom gnus-registry-cache-file
  (nnheader-concat
   (or gnus-dribble-directory gnus-home-directory "~/")
   ".gnus.registry.eieio")
  "File where the Gnus registry will be stored."
  :group 'gnus-registry
  :type 'file)

(defcustom gnus-registry-max-entries nil
  "Maximum number of entries in the registry, nil for unlimited."
  :group 'gnus-registry
  :type '(radio (const :format "Unlimited " nil)
                (integer :format "Maximum number: %v")))

(defcustom gnus-registry-prune-factor 0.1
  "When pruning, try to prune back to this factor less than the maximum size.

In order to prevent constant pruning, we prune back to a number
somewhat less than the maximum size.  This option controls
exactly how much less.  For example, given a maximum size of
50000 and a prune factor of 0.1, the pruning process will try to
cut the registry back to \(- 50000 \(* 50000 0.1)) -> 45000
entries.  The pruning process is constrained by the presence of
\"precious\" entries."
  :version "25.1"
  :group 'gnus-registry
  :type 'float)

(defcustom gnus-registry-default-sort-function
  #'gnus-registry-sort-by-creation-time
  "Sort function to use when pruning the registry.
Entries that sort to the front of the list are pruned first.
This can slow pruning down.  Set to nil to perform no sorting."
  :version "25.1"
  :group 'gnus-registry
  :type '(choice (const :tag "No sorting" nil) function))

(defun gnus-registry-sort-by-creation-time (l r)
  "Sort older entries to front of list."
  ;; Pruning starts from the front of the list.
  (time-less-p
   (cadr (assq 'creation-time r))
   (cadr (assq 'creation-time l))))

(defun gnus-registry-fixup-registry (db)
  (when db
    (let ((old (oref db tracked)))
      (setf (oref db precious)
            (append gnus-registry-extra-entries-precious
                    '()))
      (setf (oref db max-size)
            (or gnus-registry-max-entries
                most-positive-fixnum))
      (setf (oref db prune-factor)
            (or gnus-registry-prune-factor
		0.1))
      (setf (oref db tracked)
            (append gnus-registry-track-extra
                    '(mark group keyword)))
      (when (not (equal old (oref db tracked)))
        (gnus-message 9 "Reindexing the Gnus registry (tracked change)")
        (registry-reindex db))))
  db)

(defun gnus-registry-make-db (&optional file)
  (interactive "fGnus registry persistence file: \n")
  (gnus-registry-fixup-registry
   (make-instance 'registry-db
                  :file (or file gnus-registry-cache-file)
                  ;; these parameters are set in `gnus-registry-fixup-registry'
                  :max-size most-positive-fixnum
                  :version registry-db-version
                  :precious nil
                  :tracked nil)))

(defvar gnus-registry-db (gnus-registry-make-db)
  "The article registry by Message ID.  See `registry-db'.")

;; top-level registry data management
(defun gnus-registry-remake-db (&optional forsure)
  "Remake the registry database after customization.
This is not required after changing `gnus-registry-cache-file'."
  (interactive (list (y-or-n-p "Remake and CLEAR the Gnus registry? ")))
  (when forsure
    (gnus-message 4 "Remaking the Gnus registry")
    (setq gnus-registry-db (gnus-registry-make-db))))

(defun gnus-registry-load ()
  "Load the registry from the cache file."
  (interactive)
  (let ((file gnus-registry-cache-file))
    (condition-case nil
        (gnus-registry-read file)
      (file-error
       ;; Fix previous mis-naming of the registry file.
       (let ((old-file-name
	      (concat (file-name-sans-extension
		      gnus-registry-cache-file)
		     ".eioio")))
	 (if (and (file-exists-p old-file-name)
		  (yes-or-no-p
		   (format "Rename registry file from %s to %s? "
			   old-file-name file)))
	     (progn
	       (gnus-registry-read old-file-name)
	       (setf (oref gnus-registry-db file) file)
	       (gnus-message 1 "Registry filename changed to %s" file))
	   (gnus-registry-remake-db t))))
      (error
       (gnus-message
        1
        "The Gnus registry could not be loaded from %s, creating a new one"
        file)
       (gnus-registry-remake-db t)))))

(defun gnus-registry-read (file)
  "Do the actual reading of the registry persistence file."
  (gnus-message 5 "Reading Gnus registry from %s..." file)
  (setq gnus-registry-db
	(gnus-registry-fixup-registry
	 (condition-case nil
	     (with-no-warnings
	       (eieio-persistent-read file 'registry-db))
	   ;; Older EIEIO versions do not check the class name.
	   ('wrong-number-of-arguments
	    (eieio-persistent-read file)))))
  (gnus-message 5 "Reading Gnus registry from %s...done" file))

(defun gnus-registry-save (&optional file db)
  "Save the registry cache file."
  (interactive)
  (let ((file (or file gnus-registry-cache-file))
        (db (or db gnus-registry-db)))
    (gnus-message 5 "Saving Gnus registry (%d entries) to %s..."
                  (registry-size db) file)
    (registry-prune
     db gnus-registry-default-sort-function)
    ;; TODO: call (gnus-string-remove-all-properties v) on all elements?
    (eieio-persistent-save db file)
    (gnus-message 5 "Saving Gnus registry (size %d) to %s...done"
                  (registry-size db) file)))

(defun gnus-registry-remove-ignored ()
  (interactive)
  (let* ((db gnus-registry-db)
         (grouphashtb (registry-lookup-secondary db 'group))
         (old-size (registry-size db)))
    (registry-reindex db)
    (cl-loop for k being the hash-keys of grouphashtb
          using (hash-values v)
          when (gnus-registry-ignore-group-p k)
          do (registry-delete db v nil))
    (registry-reindex db)
    (gnus-message 4 "Removed %d ignored entries from the Gnus registry"
                  (- old-size (registry-size db)))))

;; article move/copy/spool/delete actions
(defun gnus-registry-action (action data-header from &optional to method)
  (let* ((id (mail-header-id data-header))
         (subject (mail-header-subject data-header))
         (extra (mail-header-extra data-header))
         (recipients (gnus-registry-sort-addresses
                      (or (cdr-safe (assq 'Cc extra)) "")
                      (or (cdr-safe (assq 'To extra)) "")))
         (sender (nth 0 (gnus-registry-extract-addresses
                         (mail-header-from data-header))))
         (from (gnus-group-guess-full-name-from-command-method from))
         (to (if to (gnus-group-guess-full-name-from-command-method to) nil)))
    (gnus-message 7 "Gnus registry: article %s %s from %s to %s"
                  id (if method "respooling" "going") from to)

    (gnus-registry-handle-action
     id
     ;; unless copying, remove the old "from" group
     (if (not (equal 'copy action)) from nil)
     to subject sender recipients)))

(defun gnus-registry-spool-action (id group &optional subject sender recipients)
  (let ((to (gnus-group-guess-full-name-from-command-method group))
        (recipients (or recipients
                        (gnus-registry-sort-addresses
                         (or (message-fetch-field "cc") "")
                         (or (message-fetch-field "to") ""))))
        (subject (or subject (message-fetch-field "subject")))
        (sender (or sender (message-fetch-field "from"))))
    (when (and (stringp id) (string-match "\r$" id))
      (setq id (substring id 0 -1)))
    (gnus-message 7 "Gnus registry: article %s spooled to %s"
                  id
                  to)
    (gnus-registry-handle-action id nil to subject sender recipients)))

(defun gnus-registry-handle-action (id from to subject sender
                                       &optional recipients)
  (gnus-message
   10
   "gnus-registry-handle-action %S" (list id from to subject sender recipients))
  (let ((db gnus-registry-db)
        ;; if the group is ignored, set the destination to nil (same as delete)
        (to (if (gnus-registry-ignore-group-p to) nil to))
        ;; safe if not found
        (entry (gnus-registry-get-or-make-entry id))
        (subject (gnus-string-remove-all-properties
                  (gnus-registry-simplify-subject subject)))
        (sender (gnus-string-remove-all-properties sender)))

    ;; this could be done by calling `gnus-registry-set-id-key'
    ;; several times but it's better to bunch the transactions
    ;; together

    (registry-delete db (list id) nil)
    (when from
      (setq entry (cons (delete from (assoc 'group entry))
                        (assq-delete-all 'group entry))))

    (dolist (kv `((group ,to)
                  (sender ,sender)
                  (recipient ,@recipients)
                  (subject ,subject)))
      (when (cadr kv)
        (let ((new (or (assq (car kv) entry)
                       (list (car kv)))))
          (dolist (toadd (cdr kv))
            (unless (member toadd new)
              (setq new (append new (list toadd)))))
          (setq entry (cons new
                            (assq-delete-all (car kv) entry))))))
    (gnus-message 10 "Gnus registry: new entry for %s is %S"
                  id
                  entry)
    (gnus-registry-insert db id entry)))

;; Function for nn{mail|imap}-split-fancy: look up all references in
;; the cache and if a match is found, return that group.
(defun gnus-registry-split-fancy-with-parent ()
  "Split this message into the same group as its parent.
The parent is obtained from the registry.  This function can be used as an
entry in `nnmail-split-fancy' or `nnimap-split-fancy', for example like
this: (: gnus-registry-split-fancy-with-parent)

This function tracks ALL backends, unlike
`nnmail-split-fancy-with-parent' which tracks only nnmail
messages.

For a message to be split, it looks for the parent message in the
References or In-Reply-To header and then looks in the registry
to see which group that message was put in.  This group is
returned, unless `gnus-registry-follow-group-p' return nil for
that group.

See the Info node `(gnus)Fancy Mail Splitting' for more details."
  (let* ((refstr (or (message-fetch-field "references") "")) ; guaranteed
         (reply-to (message-fetch-field "in-reply-to"))      ; may be nil
         ;; now, if reply-to is valid, append it to the References
         (refstr (if reply-to
                     (concat refstr " " reply-to)
                   refstr))
         (references (and refstr (gnus-extract-references refstr)))
         ;; these may not be used, but the code is cleaner having them up here
         (sender (gnus-string-remove-all-properties
                  (message-fetch-field "from")))
         (recipients (gnus-registry-sort-addresses
                      (or (message-fetch-field "cc") "")
                      (or (message-fetch-field "to") "")))
         (subject (gnus-string-remove-all-properties
                   (gnus-registry-simplify-subject
                    (message-fetch-field "subject"))))

         (nnmail-split-fancy-with-parent-ignore-groups
          (if (listp nnmail-split-fancy-with-parent-ignore-groups)
              nnmail-split-fancy-with-parent-ignore-groups
            (list nnmail-split-fancy-with-parent-ignore-groups))))
    (gnus-registry--split-fancy-with-parent-internal
     :references references
     :refstr refstr
     :sender sender
     :recipients recipients
     :subject subject
     :log-agent "Gnus registry fancy splitting with parent")))

(cl-defun gnus-registry--split-fancy-with-parent-internal
    (&rest spec
           &key references refstr sender subject recipients log-agent
           &allow-other-keys)
  (gnus-message
   10
   "gnus-registry--split-fancy-with-parent-internal %S" spec)
  (let ((db gnus-registry-db)
        found)
    ;; this is a big chain of statements.  it uses
    ;; gnus-registry-post-process-groups to filter the results after
    ;; every step.
    ;; the references string must be valid and parse to valid references
    (when references
      (gnus-message
       9
       "%s is tracing references %s"
       log-agent refstr)
      (dolist (reference (nreverse references))
        (gnus-message 9 "%s is looking up %s" log-agent reference)
        (cl-loop for group in (gnus-registry-get-id-key reference 'group)
              when (gnus-registry-follow-group-p group)
              do
              (progn
                (gnus-message 7 "%s traced %s to %s" log-agent reference group)
                (push group found))))
      ;; filter the found groups and return them
      ;; the found groups are the full groups
      (setq found (gnus-registry-post-process-groups
                   "references" refstr found)))

     ;; else: there were no matches, now try the extra tracking by subject
     (when (and (null found)
                (memq 'subject gnus-registry-track-extra)
                subject
                (< gnus-registry-minimum-subject-length (length subject)))
       (let ((groups (apply
                      'append
                      (mapcar
                       (lambda (reference)
                         (gnus-registry-get-id-key reference 'group))
                       (registry-lookup-secondary-value db 'subject subject)))))
         (setq found
               (cl-loop for group in groups
                     when (gnus-registry-follow-group-p group)
                     do (gnus-message
                         ;; warn more if gnus-registry-track-extra
                         (if gnus-registry-track-extra 7 9)
                         "%s (extra tracking) traced subject `%s' to %s"
                         log-agent subject group)
                    and collect group))
         ;; filter the found groups and return them
         ;; the found groups are NOT the full groups
         (setq found (gnus-registry-post-process-groups
                      "subject" subject found))))

     ;; else: there were no matches, try the extra tracking by sender
     (when (and (null found)
                (memq 'sender gnus-registry-track-extra)
                sender
                (not (gnus-grep-in-list
                      sender
                      gnus-registry-unfollowed-addresses)))
       (let ((groups (apply
                      'append
                      (mapcar
                       (lambda (reference)
                         (gnus-registry-get-id-key reference 'group))
                       (registry-lookup-secondary-value db 'sender sender)))))
         (setq found
               (cl-loop for group in groups
                     when (gnus-registry-follow-group-p group)
                     do (gnus-message
                         ;; warn more if gnus-registry-track-extra
                         (if gnus-registry-track-extra 7 9)
                         "%s (extra tracking) traced sender `%s' to %s"
                         log-agent sender group)
                     and collect group)))

       ;; filter the found groups and return them
       ;; the found groups are NOT the full groups
       (setq found (gnus-registry-post-process-groups
                    "sender" sender found)))

     ;; else: there were no matches, try the extra tracking by recipient
     (when (and (null found)
                (memq 'recipient gnus-registry-track-extra)
                recipients)
       (dolist (recp recipients)
         (when (and (null found)
                    (not (gnus-grep-in-list
                          recp
                          gnus-registry-unfollowed-addresses)))
           (let ((groups (apply 'append
                                (mapcar
                                 (lambda (reference)
                                   (gnus-registry-get-id-key reference 'group))
                                 (registry-lookup-secondary-value
                                  db 'recipient recp)))))
             (setq found
                   (cl-loop for group in groups
                         when (gnus-registry-follow-group-p group)
                         do (gnus-message
                             ;; warn more if gnus-registry-track-extra
                             (if gnus-registry-track-extra 7 9)
                             "%s (extra tracking) traced recipient `%s' to %s"
                             log-agent recp group)
                        and collect group)))))

       ;; filter the found groups and return them
       ;; the found groups are NOT the full groups
       (setq found (gnus-registry-post-process-groups
                    "recipients" (mapconcat 'identity recipients ", ") found)))

     ;; after the (cond) we extract the actual value safely
     (car-safe found)))

(defun gnus-registry-post-process-groups (mode key groups)
  "Inspects GROUPS found by MODE for KEY to determine which ones to follow.

MODE can be `subject' or `sender' for example.  The KEY is the
value by which MODE was searched.

Transforms each group name to the equivalent short name.

Checks if the current Gnus method (from `gnus-command-method' or
from `gnus-newsgroup-name') is the same as the group's method.
Foreign methods are not supported so they are rejected.

Reduces the list to a single group, or complains if that's not
possible.  Uses `gnus-registry-split-strategy'."
  (let ((log-agent "gnus-registry-post-process-group")
        (desc (format "%d groups" (length groups)))
        out chosen)
    ;; the strategy can be nil, in which case chosen is nil
    (setq chosen
          (cl-case gnus-registry-split-strategy
            ;; default, take only one-element lists into chosen
            ((nil)
             (and (= (length groups) 1)
                  (car-safe groups)))

            ((first)
             (car-safe groups))

            ((majority)
             (let ((freq (make-hash-table
                          :size 256
                          :test 'equal)))
               (mapc (lambda (x) (let ((x (gnus-group-short-name x)))
                              (puthash x (1+ (gethash x freq 0)) freq)))
                     groups)
               (setq desc (format "%d groups, %d unique"
                                  (length groups)
                                  (hash-table-count freq)))
               (car-safe
                (sort groups
                      (lambda (a b)
                        (> (gethash (gnus-group-short-name a) freq 0)
                           (gethash (gnus-group-short-name b) freq 0)))))))))

    (if chosen
        (gnus-message
         9
         "%s: strategy %s on %s produced %s"
         log-agent gnus-registry-split-strategy desc chosen)
      (gnus-message
       9
       "%s: strategy %s on %s did not produce an answer"
       log-agent
       (or gnus-registry-split-strategy "default")
       desc))

    (setq groups (and chosen (list chosen)))

    (dolist (group groups)
      (let ((m1 (gnus-find-method-for-group group))
            (m2 (or gnus-command-method
                    (gnus-find-method-for-group gnus-newsgroup-name)))
            (short-name (gnus-group-short-name group)))
        (if (gnus-methods-equal-p m1 m2)
            (progn
              ;; this is REALLY just for debugging
              (when (not (equal group short-name))
                (gnus-message
                 10
                 "%s: stripped group %s to %s"
                 log-agent group short-name))
              (cl-pushnew short-name out :test #'equal))
          ;; else...
          (gnus-message
           7
           "%s: ignored foreign group %s"
           log-agent group))))

    (setq out (delq nil out))

    (cond
     ((= (length out) 1) out)
     ((null out)
      (gnus-message
       5
       "%s: no matches for %s `%s'."
       log-agent mode key)
      nil)
     (t (gnus-message
         5
         "%s: too many extra matches (%s) for %s `%s'.  Returning none."
         log-agent out mode key)
        nil))))

(defun gnus-registry-follow-group-p (group)
  "Determines if a group name should be followed.
Consults `gnus-registry-unfollowed-groups' and
`nnmail-split-fancy-with-parent-ignore-groups'."
  (and group
       (not (or (gnus-grep-in-list
                 group
                 gnus-registry-unfollowed-groups)
                (gnus-grep-in-list
                 group
                 nnmail-split-fancy-with-parent-ignore-groups)))))

;; note that gnus-registry-ignored-groups is defined in gnus.el as a
;; group/topic parameter and an associated variable!

;; we do special logic for ignoring to accept regular expressions and
;; nnmail-split-fancy-with-parent-ignore-groups as well
(defun gnus-registry-ignore-group-p (group)
  "Determines if a group name should be ignored.
Consults `gnus-registry-ignored-groups' and
`nnmail-split-fancy-with-parent-ignore-groups'."
  (and group
       (or (gnus-grep-in-list
            group
            (delq nil (mapcar (lambda (g)
                                (cond
                                 ((stringp g) g)
                                 ((and (listp g) (nth 1 g))
                                  (nth 0 g))
                                 (t nil))) gnus-registry-ignored-groups)))
           ;; only use `gnus-parameter-registry-ignore' if
           ;; `gnus-registry-ignored-groups' is a list of lists
           ;; (it can be a list of regexes)
           (and (listp (nth 0 gnus-registry-ignored-groups))
                (get-buffer "*Group*")  ; in automatic tests this is false
                (gnus-parameter-registry-ignore group))
           (gnus-grep-in-list
            group
            nnmail-split-fancy-with-parent-ignore-groups))))

(defun gnus-registry-wash-for-keywords (&optional force)
  "Get the keywords of the current article.
Overrides existing keywords with FORCE set non-nil."
  (interactive)
  (let ((id (gnus-registry-fetch-message-id-fast gnus-current-article))
        word words)
    (if (or (not (gnus-registry-get-id-key id 'keyword))
            force)
        (with-current-buffer gnus-article-buffer
          (article-goto-body)
          (save-window-excursion
            (save-restriction
              (narrow-to-region (point) (point-max))
              (with-syntax-table gnus-adaptive-word-syntax-table
                (while (re-search-forward "\\b\\w+\\b" nil t)
                  (setq word (gnus-string-remove-all-properties
                              (downcase (buffer-substring
                                         (match-beginning 0) (match-end 0)))))
                  (if (> (length word) 2)
                      (push word words))))))
          (gnus-registry-set-id-key id 'keyword words)))))

(defun gnus-registry-keywords ()
  (let ((table (registry-lookup-secondary gnus-registry-db 'keyword))
        (ks ()))
    (when table (maphash (lambda (k _v) (push k ks)) table) ks)))

(defun gnus-registry-find-keywords (keyword)
  (interactive (list
                (completing-read "Keyword: " (gnus-registry-keywords) nil t)))
  (registry-lookup-secondary-value gnus-registry-db 'keyword keyword))

(defun gnus-registry-register-message-ids ()
  "Register the Message-ID of every article in the group."
  (unless (gnus-parameter-registry-ignore gnus-newsgroup-name)
    (dolist (article gnus-newsgroup-articles)
      (let* ((id (gnus-registry-fetch-message-id-fast article))
             (groups (gnus-registry-get-id-key id 'group)))
        (unless (member gnus-newsgroup-name groups)
          (gnus-message 9 "Registry: Registering article %d with group %s"
                        article gnus-newsgroup-name)
          (gnus-registry-handle-action id nil gnus-newsgroup-name
           (gnus-registry-fetch-simplified-message-subject-fast article)
           (gnus-registry-fetch-sender-fast article)
           (gnus-registry-fetch-recipients-fast article)))))))

;; message field fetchers
(defun gnus-registry-fetch-message-id-fast (article)
  "Fetch the Message-ID quickly, using the internal gnus-data-list function."
  (if (and (numberp article)
           (assoc article (gnus-data-list nil)))
      (mail-header-id (gnus-data-header (assoc article (gnus-data-list nil))))
    nil))

(defun gnus-registry-extract-addresses (text)
  "Extract all the addresses in a normalized way from TEXT.
Returns an unsorted list of strings in the name <address> format.
Addresses without a name will say \"noname\"."
  (mapcar (lambda (add)
            (gnus-string-remove-all-properties
             (let* ((name (or (nth 0 add) "noname"))
                    (addr (nth 1 add))
                    (addr (if (bufferp addr)
                              (with-current-buffer addr
                                (buffer-string))
                            addr)))
               (format "%s <%s>" name addr))))
          (mail-extract-address-components text t)))

(defun gnus-registry-sort-addresses (&rest addresses)
  "Return a normalized and sorted list of ADDRESSES."
  (sort (mapcan 'gnus-registry-extract-addresses addresses) 'string-lessp))

(defun gnus-registry-simplify-subject (subject)
  (if (stringp subject)
      (gnus-simplify-subject subject)
    nil))

(defun gnus-registry-fetch-simplified-message-subject-fast (article)
  "Fetch the Subject quickly, using the internal gnus-data-list function."
  (if (and (numberp article)
           (assoc article (gnus-data-list nil)))
      (gnus-string-remove-all-properties
       (gnus-registry-simplify-subject
        (mail-header-subject (gnus-data-header
                              (assoc article (gnus-data-list nil))))))
    nil))

(defun gnus-registry-fetch-sender-fast (article)
  (when-let* ((data (and (numberp article)
			 (assoc article (gnus-data-list nil)))))
    (mail-header-from (gnus-data-header data))))

(defun gnus-registry-fetch-recipients-fast (article)
  (when-let* ((data (and (numberp article)
			 (assoc article (gnus-data-list nil))))
	      (extra (mail-header-extra (gnus-data-header data))))
    (gnus-registry-sort-addresses
     (or (cdr (assq 'Cc extra)) "")
     (or (cdr (assq 'To extra)) ""))))

;; registry marks glue
(defun gnus-registry-do-marks (type function)
  "For each known mark, call FUNCTION for each cell of type TYPE.

FUNCTION should take two parameters, a mark symbol and the cell value."
  (dolist (mark-info gnus-registry-marks)
    (let* ((mark (car-safe mark-info))
           (data (cdr-safe mark-info))
           (cell-data (plist-get data type)))
      (when cell-data
        (funcall function mark cell-data)))))

;; FIXME: Why not merge gnus-registry--set/remove-mark and
;; gnus-registry-set-article-mark-internal?
(defun gnus-registry--set/remove-mark (mark remove articles)
  "Set/remove the MARK over process-marked ARTICLES."
  ;; If this is called and the user doesn't want the
  ;; registry enabled, we'll ask anyhow.
  (unless gnus-registry-install
    (let ((gnus-registry-install 'ask))
      (gnus-registry-install-p)))

  ;; Now the user is asked if gnus-registry-install is `ask'.
  (when (gnus-registry-install-p)
    (gnus-registry-set-article-mark-internal
     ;; All this just to get the mark, I must be doing it wrong.
     mark articles remove t)
    ;; FIXME: Why do we do the above only here and not directly inside
    ;; gnus-registry-set-article-mark-internal?  I.e. we wouldn't we want to do
    ;; the things below when gnus-registry-set-article-mark-internal is called
    ;; from gnus-registry-set-article-mark or
    ;; gnus-registry-remove-article-mark?
    (gnus-message 9 "Applying mark %s to %d articles"
                  mark (length articles))
    (dolist (article articles)
      (gnus-summary-update-article
       article
       (assoc article (gnus-data-list nil))))))

;; This is ugly code, but I don't know how to do it better.
(defun gnus-registry-install-shortcuts ()
  "Install the keyboard shortcuts and menus for the registry.
Uses `gnus-registry-marks' to find what shortcuts to install."
  (let (keys-plist)
    (setq gnus-registry-misc-menus nil)
    (gnus-registry-do-marks
     :char
     (lambda (mark data)
       (let ((function-format
              (format "gnus-registry-%%s-article-%s-mark" mark)))

;;;  The following generates these functions:
;;;  (defun gnus-registry-set-article-Important-mark (&rest articles)
;;;    "Apply the Important mark to process-marked ARTICLES."
;;;    (interactive (gnus-summary-work-articles current-prefix-arg))
;;;    (gnus-registry-set-article-mark-internal 'Important articles nil t))
;;;  (defun gnus-registry-remove-article-Important-mark (&rest articles)
;;;    "Apply the Important mark to process-marked ARTICLES."
;;;    (interactive (gnus-summary-work-articles current-prefix-arg))
;;;    (gnus-registry-set-article-mark-internal 'Important articles t t))

         (dolist (remove '(t nil))
           (let* ((variant-name (if remove "remove" "set"))
                  (function-name
                   (intern (format function-format variant-name)))
                  (shortcut (format "%c" (if remove (upcase data) data))))
             (defalias function-name
               ;; If it weren't for the function's docstring, we could
               ;; use a closure, with lexical-let :-(
               `(lambda (&rest articles)
                  ,(format
                    "%s the %s mark over process-marked ARTICLES."
                    (upcase-initials variant-name)
                    mark)
                  (interactive
                   (gnus-summary-work-articles current-prefix-arg))
                  (gnus-registry--set/remove-mark ',mark ',remove articles)))
             (push function-name keys-plist)
             (push shortcut keys-plist)
             (push (vector (format "%s %s"
                                   (upcase-initials variant-name)
                                   (symbol-name mark))
                           function-name t)
                   gnus-registry-misc-menus)
             (gnus-message 9 "Defined mark handling function %s"
                           function-name))))))
    (gnus-define-keys-1
     '(gnus-registry-mark-map "M" gnus-summary-mark-map)
     keys-plist)
    (add-hook 'gnus-summary-menu-hook
              (lambda ()
                (easy-menu-add-item
                 gnus-summary-misc-menu
                 nil
                 (cons "Registry Marks" gnus-registry-misc-menus))))))

(make-obsolete 'gnus-registry-user-format-function-M
               'gnus-registry-article-marks-to-chars "24.1") ?

(defalias 'gnus-registry-user-format-function-M
  'gnus-registry-article-marks-to-chars)

;; use like this:
;; (defalias 'gnus-user-format-function-M 'gnus-registry-article-marks-to-chars)
(defun gnus-registry-article-marks-to-chars (headers)
  "Show the marks for an article by the :char property."
  (let* ((id (mail-header-message-id headers))
         (marks (when id (gnus-registry-get-id-key id 'mark))))
    (concat (delq nil
		  (mapcar
		   (lambda (m)
		     (plist-get
		      (cdr-safe (assoc m gnus-registry-marks))
		      :char))
		   marks)))))

;; use like this:
;; (defalias 'gnus-user-format-function-M 'gnus-registry-article-marks-to-names)
(defun gnus-registry-article-marks-to-names (headers)
  "Show the marks for an article by name."
  (let* ((id (mail-header-message-id headers))
         (marks (when id (gnus-registry-get-id-key id 'mark))))
    (mapconcat (lambda (mark) (symbol-name mark)) marks ",")))

(defun gnus-registry-read-mark ()
  "Read a mark name from the user with completion."
  (let ((mark (gnus-completing-read
               "Label"
               (mapcar 'symbol-name (mapcar 'car gnus-registry-marks))
               nil nil nil
               (symbol-name gnus-registry-default-mark))))
    (when (stringp mark)
      (intern mark))))

(defun gnus-registry-set-article-mark (&rest articles)
  "Apply a mark to process-marked ARTICLES."
  (interactive (gnus-summary-work-articles current-prefix-arg))
  (gnus-registry-set-article-mark-internal (gnus-registry-read-mark)
                                           articles nil t))

(defun gnus-registry-remove-article-mark (&rest articles)
  "Remove a mark from process-marked ARTICLES."
  (interactive (gnus-summary-work-articles current-prefix-arg))
  (gnus-registry-set-article-mark-internal (gnus-registry-read-mark)
                                           articles t t))

(defun gnus-registry-set-article-mark-internal (mark
                                                articles
                                                &optional remove
                                                show-message)
  "Apply or remove MARK across a list of ARTICLES."
  (let ((article-id-list
         (mapcar 'gnus-registry-fetch-message-id-fast articles)))
    (dolist (id article-id-list)
      (let* ((marks (delq mark (gnus-registry-get-id-key id 'mark)))
             (marks (if remove marks (cons mark marks))))
        (when show-message
          (gnus-message 1 "%s mark %s with message ID %s, resulting in %S"
                        (if remove "Removing" "Adding")
                        mark id marks))
        (gnus-registry-set-id-key id 'mark marks)))))

(defun gnus-registry-get-article-marks (&rest articles)
  "Get the Gnus registry marks for ARTICLES and show them if interactive.
Uses process/prefix conventions.  For multiple articles,
only the last one's marks are returned."
  (interactive (gnus-summary-work-articles 1))
  (let* ((article (last articles))
         (id (gnus-registry-fetch-message-id-fast article))
         (marks (when id (gnus-registry-get-id-key id 'mark))))
    (when (called-interactively-p 'any)
      (gnus-message 1 "Marks are %S" marks))
    marks))

(defun gnus-registry-group-count (id)
  "Get the number of groups of a message, based on the message ID."
  (length (gnus-registry-get-id-key id 'group)))

(defun gnus-registry-get-or-make-entry (id)
  (let* ((db gnus-registry-db)
         ;; safe if not found
         (entries (registry-lookup db (list id))))

    (when (null entries)
      (gnus-registry-insert db id (list (list 'creation-time (current-time))
                                        '(group) '(sender) '(subject)))
      (setq entries (registry-lookup db (list id))))

    (nth 1 (assoc id entries))))

(defun gnus-registry-delete-entries (idlist)
  (registry-delete gnus-registry-db idlist nil))

(defun gnus-registry-get-id-key (id key)
  (cdr-safe (assq key (gnus-registry-get-or-make-entry id))))

(defun gnus-registry-set-id-key (id key vals)
  (let* ((db gnus-registry-db)
         (entry (gnus-registry-get-or-make-entry id)))
    (registry-delete db (list id) nil)
    (setq entry (cons (cons key vals) (assq-delete-all key entry)))
    (gnus-registry-insert db id entry)
    entry))

(defun gnus-registry-insert (db id entry)
  "Just like `registry-insert' but tries to prune on error."
  (when (registry-full db)
    (message "Trying to prune the registry because it's full")
    (registry-prune
     db gnus-registry-default-sort-function))
  (registry-insert db id entry)
  entry)

(defun gnus-registry-import-eld (file)
  (interactive "fOld registry file to import? ")
  ;; example content:
  ;;   (setq gnus-registry-alist '(
  ;; ("<messageID>" ((marks nil)
  ;;                 (mtime 19365 1776 440496)
  ;;                 (sender . "root (Cron Daemon)")
  ;;                 (subject . "Cron"))
  ;;  "cron" "nnml+private:cron")
  (load file t)
  (when (boundp 'gnus-registry-alist)
    (let* ((old (symbol-value 'gnus-registry-alist))
           (count 0)
           (expected (length old))
           entry)
      (while (car-safe old)
        (cl-incf count)
        ;; don't use progress reporters for backwards compatibility
        (when (and (< 0 expected)
                   (= 0 (mod count 100)))
          (message "importing: %d of %d (%.2f%%)"
                   count expected (/ (* 100.0 count) expected)))
        (setq entry (car-safe old)
              old (cdr-safe old))
        (let* ((id (car-safe entry))
               (rest (cdr-safe entry))
               (groups (cl-loop for p in rest
                             when (stringp p)
                             collect p))
               extra-cell key val)
          ;; remove all the strings from the entry
          (dolist (elem rest)
            (if (stringp elem) (setq rest (delq elem rest))))
          (gnus-registry-set-id-key id 'group groups)
          ;; just use the first extra element
          (setq rest (car-safe rest))
          (while (car-safe rest)
            (setq extra-cell (car-safe rest)
                  key (car-safe extra-cell)
                  val (cdr-safe extra-cell)
                  rest (cdr-safe rest))
            (when (and val (atom val))
              (setq val (list val)))
            (gnus-registry-set-id-key id key val))))
      (message "Import done, collected %d entries" count))))

;;;###autoload
(defun gnus-registry-initialize ()
  "Initialize the Gnus registry."
  (interactive)
  (gnus-message 5 "Initializing the registry")
  (gnus-registry-install-hooks)
  (gnus-registry-install-shortcuts)
  (gnus-registry-load))

;; FIXME: Why autoload this function?
;;;###autoload
(defun gnus-registry-install-hooks ()
  "Install the registry hooks."
  (interactive)
  (setq gnus-registry-enabled t)
  (add-hook 'gnus-summary-article-move-hook 'gnus-registry-action)
  (add-hook 'gnus-summary-article-delete-hook 'gnus-registry-action)
  (add-hook 'gnus-summary-article-expire-hook 'gnus-registry-action)
  (add-hook 'nnmail-spool-hook 'gnus-registry-spool-action)

  (add-hook 'gnus-save-newsrc-hook 'gnus-registry-save)
  (add-hook 'gnus-read-newsrc-el-hook 'gnus-registry-load)

  (add-hook 'gnus-summary-prepare-hook 'gnus-registry-register-message-ids))

(defun gnus-registry-unload-hook ()
  "Uninstall the registry hooks."
  (interactive)
  (remove-hook 'gnus-summary-article-move-hook 'gnus-registry-action)
  (remove-hook 'gnus-summary-article-delete-hook 'gnus-registry-action)
  (remove-hook 'gnus-summary-article-expire-hook 'gnus-registry-action)
  (remove-hook 'nnmail-spool-hook 'gnus-registry-spool-action)

  (remove-hook 'gnus-save-newsrc-hook 'gnus-registry-save)
  (remove-hook 'gnus-read-newsrc-el-hook 'gnus-registry-load)

  (remove-hook 'gnus-summary-prepare-hook 'gnus-registry-register-message-ids)
  (setq gnus-registry-enabled nil))

(add-hook 'gnus-registry-unload-hook 'gnus-registry-unload-hook)

(defun gnus-registry-install-p ()
  "Return non-nil if the registry is enabled (and maybe enable it first).
If the registry is not already enabled, then if `gnus-registry-install'
is `ask', ask the user; or if `gnus-registry-install' is non-nil, enable it."
  (interactive)
  (unless gnus-registry-enabled
    (when (if (eq gnus-registry-install 'ask)
              (gnus-y-or-n-p
               (concat "Enable the Gnus registry?  "
                       "See the variable `gnus-registry-install' "
                       "to get rid of this query permanently. "))
            gnus-registry-install)
      (gnus-registry-initialize)))
  gnus-registry-enabled)

;; largely based on nnir-warp-to-article
(defun gnus-try-warping-via-registry ()
  "Try to warp via the registry.
This will be done via the current article's source group based on
data stored in the registry."
  (interactive)
  (when (gnus-summary-article-header)
    (let* ((message-id (mail-header-id (gnus-summary-article-header)))
           ;; Retrieve the message's group(s) from the registry
           (groups (gnus-registry-get-id-key message-id 'group))
           ;; If starting from an ephemeral group, this describes
           ;; how to restore the window configuration
           (quit-config
            (gnus-ephemeral-group-p gnus-newsgroup-name))
           (seen-groups (list (gnus-group-group-name))))

      (catch 'found
        (dolist (group (mapcar 'gnus-simplify-group-name groups))

          ;; skip over any groups we really don't want to warp to.
          (unless (or (member group seen-groups)
                      (gnus-ephemeral-group-p group) ;; any ephemeral group
                      (memq (car (gnus-find-method-for-group group))
			    ;; Specific methods; this list may need to expand.
                            '(nnir)))

            ;; remember that we've seen this group already
            (push group seen-groups)

            ;; first exit from any ephemeral summary buffer.
            (when quit-config
              (gnus-summary-exit)
              ;; and if the ephemeral summary buffer in turn came from
              ;; another summary buffer we have to clean that summary
              ;; up too.
              (when (eq (cdr quit-config) 'summary)
                (gnus-summary-exit))
              ;; remember that we've already done this part
              (setq quit-config nil))

            ;; Try to activate the group.  If that fails, just move
            ;; along.  We may have more groups to work with
            (when
                (ignore-errors
                  (gnus-select-group-with-message-id group message-id) t)
              (throw 'found t))))))))

(defun gnus-registry-remove-extra-data (extra)
  "Remove tracked EXTRA data from the gnus registry.
EXTRA is a list of symbols.  Valid symbols are those contained in
the docs of `gnus-registry-track-extra'.  This command is useful
when you stop tracking some extra data and now want to purge it
from your existing entries."
  (interactive (list (mapcar 'intern
			     (completing-read-multiple
			      "Extra data: "
			      '("subject" "sender" "recipient")))))
  (when extra
    (let ((db gnus-registry-db))
      (registry-reindex db)
      (cl-loop for k being the hash-keys of (oref db data)
	    using (hash-value v)
	    do (let ((newv (delq nil (mapcar #'(lambda (entry)
						 (unless (member (car entry) extra)
						   entry))
					     v))))
		 (registry-delete db (list k) nil)
		 (gnus-registry-insert db k newv)))
      (registry-reindex db))))

;; TODO: a few things

(provide 'gnus-registry)

;;; gnus-registry.el ends here
