;;; mh-seq.el --- mh-e sequences support

;; Copyright (C) 1993, 1995, 2001, 2002 Free Software Foundation, Inc.

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

;; Internal support for mh-e package.

;;; Change Log:

;; $Id: mh-seq.el,v 1.14 2002/04/07 19:20:56 wohler Exp $

;;; Code:

(provide 'mh-seq)
(require 'mh-e)

;;; Internal variables:

(defvar mh-last-seq-used nil)		;Name of seq to which a msg was last added.

(defvar mh-non-seq-mode-line-annotation nil) ;Saved value of mh-mode-line-annotation when narrowed to a seq.


(defun mh-delete-seq (sequence)
  "Delete the SEQUENCE."
  (interactive (list (mh-read-seq-default "Delete" t)))
  (mh-map-to-seq-msgs 'mh-notate-if-in-one-seq sequence ?  (1+ mh-cmd-note)
		      sequence)
  (mh-undefine-sequence sequence '("all"))
  (mh-delete-seq-locally sequence))


(defun mh-list-sequences (folder)
  "List the sequences defined in FOLDER."
  (interactive (list (mh-prompt-for-folder "List sequences in"
					   mh-current-folder t)))
  (let ((temp-buffer mh-temp-sequences-buffer)
	(seq-list mh-seq-list))
    (with-output-to-temp-buffer temp-buffer
      (save-excursion
	(set-buffer temp-buffer)
	(erase-buffer)
	(message "Listing sequences ...")
	(insert "Sequences in folder " folder ":\n")
	(while seq-list
	  (let ((name (mh-seq-name (car seq-list)))
		(sorted-seq-msgs
		 (sort (copy-sequence (mh-seq-msgs (car seq-list))) '<))
		(last-col (- (window-width) 4))
		name-spec)
	    (insert (setq name-spec (format "%20s:" name)))
	    (while sorted-seq-msgs
	      (if (> (current-column) last-col)
		  (progn
		    (insert "\n")
		    (move-to-column (length name-spec))))
	      (insert (format " %s" (car sorted-seq-msgs)))
	      (setq sorted-seq-msgs (cdr sorted-seq-msgs)))
	    (insert "\n"))
	  (setq seq-list (cdr seq-list)))
	(goto-char (point-min))
	(view-mode 1)
	(setq view-exit-action 'kill-buffer)
	(message "Listing sequences...done")))))


(defun mh-msg-is-in-seq (message)
  "Display the sequences that contain MESSAGE (default: current message)."
  (interactive (list (mh-get-msg-num t)))
  (message "Message %d is in sequences: %s"
	   message
	   (mapconcat 'concat
		      (mh-list-to-string (mh-seq-containing-msg message t))
		      " ")))


(defun mh-narrow-to-seq (sequence)
  "Restrict display of this folder to just messages in SEQUENCE.
Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive (list (mh-read-seq "Narrow to" t)))
  (with-mh-folder-updating (t)
    (cond ((mh-seq-to-msgs sequence)
	   (mh-widen)
	   (let ((eob (point-max)))
	     (mh-copy-seq-to-point sequence eob)
	     (narrow-to-region eob (point-max))
	     (make-variable-buffer-local 'mh-non-seq-mode-line-annotation)
	     (setq mh-non-seq-mode-line-annotation mh-mode-line-annotation)
	     (setq mh-mode-line-annotation (symbol-name sequence))
	     (mh-make-folder-mode-line)
	     (mh-recenter nil)
             (if (and (boundp 'tool-bar-mode) tool-bar-mode)
                 (set (make-local-variable 'tool-bar-map)
                      mh-folder-seq-tool-bar-map))
	     (setq mh-narrowed-to-seq sequence)))
	  (t
	   (error "No messages in sequence `%s'" (symbol-name sequence))))))


(defun mh-put-msg-in-seq (msg-or-seq sequence)
  "Add MSG-OR-SEQ (default: displayed message) to SEQUENCE.
If optional prefix argument provided, then prompt for the message sequence."
  (interactive (list (if current-prefix-arg
			 (mh-read-seq-default "Add messages from" t)
		         (mh-get-msg-num t))
		     (mh-read-seq-default "Add to" nil)))
  (if (not (mh-internal-seq sequence))
      (setq mh-last-seq-used sequence))
  (mh-add-msgs-to-seq (if (numberp msg-or-seq)
			  msg-or-seq
			  (mh-seq-to-msgs msg-or-seq))
		      sequence))


(defun mh-widen ()
  "Remove restrictions from current folder, thereby showing all messages."
  (interactive)
  (let ((msg (mh-get-msg-num nil)))
    (when mh-narrowed-to-seq
      (with-mh-folder-updating (t)
        (delete-region (point-min) (point-max))
        (widen)
        (setq mh-mode-line-annotation mh-non-seq-mode-line-annotation)
        (mh-make-folder-mode-line))
      (if msg
          (mh-goto-msg msg t nil))))
  (mh-notate-deleted-and-refiled)
  (if (and (boundp 'tool-bar-mode) tool-bar-mode)
      (set (make-local-variable 'tool-bar-map) mh-folder-tool-bar-map))
  (setq mh-narrowed-to-seq nil))


;; FIXME?  We may want to clear all notations and add one for current-message
;;         and process user sequences.
(defun mh-notate-deleted-and-refiled ()
  ;; notate the sequence 'deleted as well as all the sequences in
  ;; mh-refile-list.
  ;;
  ;; First, the 'deleted sequence is straightforward
  (mh-notate-seq 'deleted mh-note-deleted mh-cmd-note)
  ;; Second, refiles are stored in multiple sequences, one for each folder
  ;; name to refile to.  This list of buffer names is stored in
  ;; mh-refile-list
  (mh-mapc
   (function
    (lambda (dest)
      ;; foreach folder name, get the keyed sequence from mh-seq-list
      (let ((msg-list (cdr (assoc dest mh-seq-list))))
        (mapcar (lambda (msg)
                  ;; foreach msg in a sequence, do the mh-notate
                  (mh-notate msg mh-note-refiled mh-cmd-note))
                msg-list))))
   mh-refile-list))


;;; Commands to manipulate sequences.  Sequences are stored in an alist
;;; of the form:
;;;	((seq-name msgs ...) (seq-name msgs ...) ...)


(defun mh-read-seq-default (prompt not-empty)
  ;; Read and return sequence name with default narrowed or previous sequence.
  (mh-read-seq prompt not-empty
	       (or mh-narrowed-to-seq
		   mh-last-seq-used
		   (car (mh-seq-containing-msg (mh-get-msg-num nil) nil)))))


(defun mh-read-seq (prompt not-empty &optional default)
  ;; Read and return a sequence name.  Prompt with PROMPT, raise an error
  ;; if the sequence is empty and the NOT-EMPTY flag is non-nil, and supply
  ;; an optional DEFAULT sequence.
  ;; A reply of '%' defaults to the first sequence containing the current
  ;; message.
  (let* ((input (completing-read (format "%s %s %s" prompt "sequence:"
					 (if default
					     (format "[%s] " default)
					     ""))
				 (mh-seq-names mh-seq-list)))
	 (seq (cond ((equal input "%")
		     (car (mh-seq-containing-msg (mh-get-msg-num t) nil)))
		    ((equal input "") default)
		    (t (intern input))))
	 (msgs (mh-seq-to-msgs seq)))
    (if (and (null msgs) not-empty)
	(error "No messages in sequence `%s'" seq))
    seq))


(defun mh-seq-names (seq-list)
  ;; Return an alist containing the names of the SEQUENCES.
  (mapcar (function (lambda (entry) (list (symbol-name (mh-seq-name entry)))))
	  seq-list))


(defun mh-rename-seq (sequence new-name)
  "Rename SEQUENCE to have NEW-NAME."
  (interactive (list (mh-read-seq "Old" t)
		     (intern (read-string "New sequence name: "))))
  (let ((old-seq (mh-find-seq sequence)))
    (or old-seq
	(error "Sequence %s does not exist" sequence))
    ;; create new sequence first, since it might raise an error.
    (mh-define-sequence new-name (mh-seq-msgs old-seq))
    (mh-undefine-sequence sequence (mh-seq-msgs old-seq))
    (rplaca old-seq new-name)))


(defun mh-map-to-seq-msgs (func seq &rest args)
  ;; Invoke the FUNCTION at each message in the SEQUENCE, passing the
  ;; remaining ARGS as arguments.
  (save-excursion
    (let ((msgs (mh-seq-to-msgs seq)))
      (while msgs
	(if (mh-goto-msg (car msgs) t t)
	    (apply func (car msgs) args))
	(setq msgs (cdr msgs))))))


(defun mh-notate-seq (seq notation offset)
  ;; Mark the scan listing of all messages in the SEQUENCE with the CHARACTER
  ;; at the given OFFSET from the beginning of the listing line.
  (mh-map-to-seq-msgs 'mh-notate seq notation offset))


(defun mh-add-to-sequence (seq msgs)
  ;; Add to a SEQUENCE each message the list of MSGS.
  (if (not (mh-folder-name-p seq))
      (if msgs
	  (apply 'mh-exec-cmd "mark" mh-current-folder "-add"
		 "-sequence" (symbol-name seq)
		 (mh-coalesce-msg-list msgs)))))


(defun mh-copy-seq-to-point (seq location)
  ;; Copy the scan listing of the messages in SEQUENCE to after the point
  ;; LOCATION in the current buffer.
  (mh-map-to-seq-msgs 'mh-copy-line-to-point seq location))


(defun mh-copy-line-to-point (msg location)
  ;; Copy the current line to the LOCATION in the current buffer.
  (beginning-of-line)
  (save-excursion
    (let ((beginning-of-line (point))
	  end)
      (forward-line 1)
      (setq end (point))
      (goto-char location)
      (insert-buffer-substring (current-buffer) beginning-of-line end))))

(defun mh-region-to-sequence (begin end)
  "Define sequence 'region as the messages between point and mark.
When called programmatically, use arguments BEGIN and END to define region."
  (interactive "r")
  (mh-delete-seq-locally 'region)
  (save-excursion
    (goto-char begin)
    (while (<= (point) end)
      (mh-add-msgs-to-seq (mh-get-msg-num t) 'region t)
      (forward-line 1))))


;;; Commands to handle new 'subject sequence.
;;; Or "Poor man's threading" by psg.
(defun mh-subject-thread-to-sequence (all)
  "Put all following messages with same subject in sequence 'subject.
If arg ALL is t, move to beginning of folder buffer to collect all messages.
If arg ALL is nil, collect only messages fron current one on forward.
Return number of messages put in the sequence:
 nil -> there was no subject line.
 0   -> there were no later messages with the same subject (sequence not made)
 >1  -> the total number of messages including current one."
  (if (not (eq major-mode 'mh-folder-mode))
      (error "Not in a folder buffer"))
  (save-excursion
    (beginning-of-line)
    (if (or (not (looking-at mh-scan-subject-regexp))
            (not (match-string 2))
            (string-equal "" (match-string 2)))
        (progn (message "No subject line.")
               nil)
      (let ((subject (match-string-no-properties 2))
            (end (point-max))
            (list))
        (if (> (length subject) 41)
            (setq subject (substring subject 0 41)))
        (save-excursion
          (if all
              (goto-char (point-min)))
          (while (re-search-forward mh-scan-subject-regexp nil t)
            (let ((this-subject (match-string-no-properties 2)))
              (if (> (length this-subject) 41)
                  (setq this-subject (substring this-subject 0 41)))
              (if (string-equal this-subject subject)
                  (setq list (cons (mh-get-msg-num t) list))))))
        (cond
         (list
          ;; If we created a new sequence, add the initial message to it too.
          (if (not (member (mh-get-msg-num t) list))
              (setq list (cons (mh-get-msg-num t) list)))
          (mh-delete-seq-locally 'subject)
          ;; sort the result into a sequence
          (let ((sorted-list (sort (copy-sequence list) 'mh-lessp))
                (msg))
            (while sorted-list
              (mh-add-msgs-to-seq (car sorted-list) 'subject t)
              (setq sorted-list (cdr sorted-list)))
            (safe-length list)))
         (t
          0))))))

(defun mh-narrow-to-subject-thread ()
  "Narrow to a sequence containing all following messages with same subject."
  (interactive)
  (let ((num (mh-get-msg-num nil))
        (count (mh-subject-thread-to-sequence t)))
    (cond
     ((not count)                       ; No subject line, delete msg anyway
      nil)
     ((= 0 count)                       ; No other msgs, delete msg anyway.
      (message "No other messages with same Subject following this one.")
      nil)
     (t                                 ; We have a subject sequence.
      (message "Found %d messages for subject sequence." count)
      (mh-narrow-to-seq 'subject)
      (if (numberp num)
          (mh-goto-msg num t t))))))

(defun mh-toggle-subject-thread ()
  "Narrow to or widen from a sequence containing current subject sequence."
  (interactive)
  (if (and (stringp mh-mode-line-annotation)
           (string-equal mh-mode-line-annotation "subject"))
      (progn
        (goto-char (point-min))
        (mh-widen))
    (mh-narrow-to-subject-thread)))

(defun mh-delete-subject-thread ()
  "Mark all following messages with same subject to be deleted."
  (interactive)
  (let ((count (mh-subject-thread-to-sequence nil)))
    (cond
     ((not count)                       ; No subject line, delete msg anyway
      (mh-delete-msg (mh-get-msg-num t)))
     ((= 0 count)                       ; No other msgs, delete msg anyway.
      (message "No other messages with same Subject following this one.")
      (mh-delete-msg (mh-get-msg-num t)))
     (t                                 ; We have a subject sequence.
      (message "Marked %d messages for deletion" count)
      (mh-delete-msg 'subject)))))

(defun mh-next-unseen-subject-thread ()
  "Get the next unseen subject thread."
  (interactive)
  (if (and mh-mode-line-annotation
           (string-equal mh-mode-line-annotation "subject"))
      (goto-char (point-min)))
  (if (or (not mh-mode-line-annotation)
          (not (string-equal mh-mode-line-annotation "unseen")))
      (mh-narrow-to-seq 'unseen))
  (mh-next-undeleted-msg)
  (mh-narrow-to-subject-thread))

;;; mh-seq.el ends here
