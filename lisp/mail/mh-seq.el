;;; mh-seq --- mh-e sequences support
;; Time-stamp: <93/12/02 09:36:09 gildea>

;; Copyright 1993 Free Software Foundation, Inc.

;; This file is part of mh-e.

;; mh-e is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; mh-e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mh-e; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Internal support for mh-e package.

;;; Code:

(provide 'mh-seq)
(require 'mh-e)

(defvar mh-last-seq-used nil
  "Name of the sequence to which a message was last added.")


(defun mh-delete-seq (seq)
  "Delete the SEQUENCE."
  (interactive (list (mh-read-seq-default "Delete" t)))
  (mh-map-to-seq-msgs 'mh-notate-if-in-one-seq seq ?  (1+ mh-cmd-note) seq)
  (mh-undefine-sequence seq "all")
  (mh-delete-seq-locally seq))


(defun mh-list-sequences (folder)
  "List the sequences defined in FOLDER."
  (interactive (list (mh-prompt-for-folder "List sequences in"
					   mh-current-folder t)))
  (let ((temp-buffer " *mh-temp*")
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
	(message "Listing sequences...done")))))


(defun mh-msg-is-in-seq (msg)
  "Display the sequences that contain MESSAGE (default: displayed message)."
  (interactive (list (mh-get-msg-num t)))
  (message "Message %d is in sequences: %s"
	   msg
	   (mapconcat 'concat
		      (mh-list-to-string (mh-seq-containing-msg msg))
		      " ")))


(defun mh-narrow-to-seq (seq)
  "Restrict display of this folder to just messages in a sequence.
Reads which sequence.\\<mh-folder-mode-map>  Use \\[mh-widen] to undo this command."
  (interactive (list (mh-read-seq "Narrow to" t)))
  (let ((eob (point-max)))
    (with-mh-folder-updating (t)
      (cond ((mh-seq-to-msgs seq)
	     (mh-copy-seq-to-point seq eob)
	     (narrow-to-region eob (point-max))
	     (mh-make-folder-mode-line (symbol-name seq))
	     (mh-recenter nil)
	     (setq mh-narrowed-to-seq seq))
	    (t
	     (error "No messages in sequence `%s'" (symbol-name seq)))))))


(defun mh-put-msg-in-seq (msg-or-seq to)
  "Add MESSAGE(s) (default: displayed message) to SEQUENCE.
If optional prefix argument provided, then prompt for the message sequence."
  (interactive (list (if current-prefix-arg
			 (mh-read-seq-default "Add messages from" t)
		         (mh-get-msg-num t))
		     (mh-read-seq-default "Add to" nil)))
  (setq mh-last-seq-used to)
  (mh-add-msgs-to-seq (if (numberp msg-or-seq)
			  msg-or-seq
			  (mh-seq-to-msgs msg-or-seq))
		      to))


(defun mh-widen ()
  "Remove restrictions from current folder, thereby showing all messages."
  (interactive)
  (if mh-narrowed-to-seq
      (with-mh-folder-updating (t)
	(delete-region (point-min) (point-max))
	(widen)
	(mh-make-folder-mode-line)))
  (setq mh-narrowed-to-seq nil))



;;; Commands to manipulate sequences.  Sequences are stored in an alist
;;; of the form:
;;;	((seq-name msgs ...) (seq-name msgs ...) ...)


(defun mh-read-seq-default (prompt not-empty)
  ;; Read and return sequence name with default narrowed or previous sequence.
  (mh-read-seq prompt not-empty (or mh-narrowed-to-seq mh-last-seq-used)))


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
	 (seq (cond ((equal input "%") (mh-msg-to-seq (mh-get-msg-num t)))
		    ((equal input "") default)
		    (t (intern input))))
	 (msgs (mh-seq-to-msgs seq)))
    (if (and (null msgs) not-empty)
	(error (format "No messages in sequence `%s'" seq)))
    seq))


(defun mh-msg-to-seq (msg)
  ;; Given a MESSAGE number, return the first sequence in which it occurs.
  (car (mh-seq-containing-msg msg)))


(defun mh-seq-names (seq-list)
  ;; Return an alist containing the names of the SEQUENCES.
  (mapcar (function (lambda (entry) (list (symbol-name (mh-seq-name entry)))))
	  seq-list))


(defun mh-rename-seq (seq new-name)
  "Rename a SEQUENCE to have a new NAME."
  (interactive (list (mh-read-seq "Old" t)
		     (intern (read-string "New sequence name: "))))
  (let ((old-seq (mh-find-seq seq)))
    (or old-seq
	(error "Sequence %s does not exist" seq))
    ;; create new seq first, since it might raise an error.
    (mh-define-sequence new-name (mh-seq-msgs old-seq))
    (mh-undefine-sequence seq (mh-seq-msgs old-seq))
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
		 msgs))))


(defun mh-copy-seq-to-point (seq location)
  ;; Copy the scan listing of the messages in SEQUENCE to after the point
  ;; LOCATION in the current buffer.
  (mh-map-to-seq-msgs 'mh-copy-line-to-point seq location))


(defun mh-copy-line-to-point (msg location)
  ;; Copy the current line to the LOCATION in the current buffer.
  (beginning-of-line)
  (let ((beginning-of-line (point)))
    (forward-line 1)
    (copy-region-as-kill beginning-of-line (point))
    (goto-char location)
    (yank)
    (goto-char beginning-of-line)))

