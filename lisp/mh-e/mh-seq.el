;;; mh-seq.el --- MH-E sequences support

;; Copyright (C) 1993, 1995, 2001, 02, 2003 Free Software Foundation, Inc.

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
;;
;;   This tries to implement the algorithm described at:
;;     http://www.jwz.org/doc/threading.html
;;   It is also a start to implementing the IMAP Threading extension RFC. The
;;   implementation lacks the reference and subject canonicalization of the
;;   RFC.
;;
;;   In the presentation buffer, children messages are shown indented with
;;   either [ ] or < > around them. Square brackets ([ ]) denote that the
;;   algorithm can point out some headers which when taken together implies
;;   that the unindented message is an ancestor of the indented message. If
;;   no such proof exists then angles (< >) are used.
;;
;;   Some issues and problems are as follows:
;;
;;    (1) Scan truncates the fields at length 512. So longer references:
;;        headers get mutilated. The same kind of MH format string works when
;;        composing messages. Is there a way to avoid this? My scan command
;;        is as follows:
;;          scan +folder -width 10000 \
;;               -format "%(msg)\n%{message-id}\n%{references}\n%{subject}\n"
;;        I would really appreciate it if someone would help me with this.
;;
;;    (2) Implement heuristics to recognize message identifiers in
;;        In-Reply-To: header. Right now it just assumes that the last text
;;        between angles (< and >) is the message identifier. There is the
;;        chance that this will incorrectly use an email address like a
;;        message identifier.
;;
;;    (3) Error checking of found message identifiers should be done.
;;
;;    (4) Since this breaks the assumption that message indices increase as
;;        one goes down the buffer, the binary search based mh-goto-msg
;;        doesn't work. I have a simpler replacement which may be less
;;        efficient.
;;
;;    (5) Better canonicalizing for message identifier and subject strings.
;;

;; Internal support for MH-E package.

;;; Change Log:

;;; Code:

(require 'cl)
(require 'mh-e)

;; Shush the byte-compiler
(defvar tool-bar-mode)

;;; Data structures (used in message threading)...
(defstruct (mh-thread-message (:conc-name mh-message-)
                              (:constructor mh-thread-make-message))
  (id nil)
  (references ())
  (subject "")
  (subject-re-p nil))

(defstruct (mh-thread-container (:conc-name mh-container-)
                                (:constructor mh-thread-make-container))
  message parent children
  (real-child-p t))


;;; Internal variables:
(defvar mh-last-seq-used nil
  "Name of seq to which a msg was last added.")

(defvar mh-non-seq-mode-line-annotation nil
  "Saved value of `mh-mode-line-annotation' when narrowed to a seq.")

;;; Maps and hashes...
(defvar mh-thread-id-hash nil
  "Hashtable used to canonicalize message identifiers.")
(defvar mh-thread-subject-hash nil
  "Hashtable used to canonicalize subject strings.")
(defvar mh-thread-id-table nil
  "Thread ID table maps from message identifiers to message containers.")
(defvar mh-thread-id-index-map nil
  "Table to look up message index number from message identifier.")
(defvar mh-thread-index-id-map nil
  "Table to look up message identifier from message index.")
(defvar mh-thread-scan-line-map nil
  "Map of message index to various parts of the scan line.")
(defvar mh-thread-old-scan-line-map nil
  "Old map of message index to various parts of the scan line.
This is the original map that is stored when the folder is narrowed.")
(defvar mh-thread-subject-container-hash nil
  "Hashtable used to group messages by subject.")
(defvar mh-thread-duplicates nil
  "Hashtable used to associate messages with the same message identifier.")
(defvar mh-thread-history ()
  "Variable to remember the transformations to the thread tree.
When new messages are added, these transformations are rewound, then the
links are added from the newly seen messages. Finally the transformations are
redone to get the new thread tree. This makes incremental threading easier.")
(defvar mh-thread-body-width nil
  "Width of scan substring that contains subject and body of message.")

(make-variable-buffer-local 'mh-thread-id-hash)
(make-variable-buffer-local 'mh-thread-subject-hash)
(make-variable-buffer-local 'mh-thread-id-table)
(make-variable-buffer-local 'mh-thread-id-index-map)
(make-variable-buffer-local 'mh-thread-index-id-map)
(make-variable-buffer-local 'mh-thread-scan-line-map)
(make-variable-buffer-local 'mh-thread-old-scan-line-map)
(make-variable-buffer-local 'mh-thread-subject-container-hash)
(make-variable-buffer-local 'mh-thread-duplicates)
(make-variable-buffer-local 'mh-thread-history)

;;;###mh-autoload
(defun mh-delete-seq (sequence)
  "Delete the SEQUENCE."
  (interactive (list (mh-read-seq-default "Delete" t)))
  (let ((msg-list (mh-seq-to-msgs sequence)))
    (mh-undefine-sequence sequence '("all"))
    (mh-delete-seq-locally sequence)
    (mh-iterate-on-messages-in-region msg (point-min) (point-max)
      (cond ((and mh-tick-seq (eq sequence mh-tick-seq))
             (mh-notate-tick msg ()))
            ((and (member msg msg-list) (not (mh-seq-containing-msg msg nil)))
             (mh-notate nil ?  (1+ mh-cmd-note)))))))

;; Avoid compiler warnings
(defvar view-exit-action)

;;;###mh-autoload
(defun mh-list-sequences ()
  "List the sequences defined in the folder being visited."
  (interactive)
  (let ((folder mh-current-folder)
        (temp-buffer mh-sequences-buffer)
        (seq-list mh-seq-list)
        (max-len 0))
    (with-output-to-temp-buffer temp-buffer
      (save-excursion
        (set-buffer temp-buffer)
        (erase-buffer)
        (message "Listing sequences ...")
        (insert "Sequences in folder " folder ":\n")
        (let ((seq-list seq-list))
          (while seq-list
            (setq max-len
                  (max (length (symbol-name (mh-seq-name (pop seq-list))))
                       max-len)))
          (setq max-len (+ 2 max-len)))
        (while seq-list
          (let ((name (mh-seq-name (car seq-list)))
                (sorted-seq-msgs
                 (mh-coalesce-msg-list
                  (sort (copy-sequence (mh-seq-msgs (car seq-list))) '<)))
                name-spec)
            (insert (setq name-spec (format (format "%%%ss:" max-len) name)))
            (while sorted-seq-msgs
              (let ((next-element (format " %s" (pop sorted-seq-msgs))))
                (when (>= (+ (current-column) (length next-element))
                          (window-width))
                  (insert "\n")
                  (insert (format (format "%%%ss" (length name-spec)) "")))
                (insert next-element)))
            (insert "\n"))
          (setq seq-list (cdr seq-list)))
        (goto-char (point-min))
        (view-mode 1)
        (setq view-exit-action 'kill-buffer)
        (message "Listing sequences...done")))))

;;;###mh-autoload
(defun mh-msg-is-in-seq (message)
  "Display the sequences that contain MESSAGE.
Default is the displayed message."
  (interactive (list (mh-get-msg-num t)))
  (let* ((dest-folder (loop for seq in mh-refile-list
                            until (member message (cdr seq))
                            finally return (car seq)))
         (deleted-flag (unless dest-folder (member message mh-delete-list))))
    (message "Message %d%s is in sequences: %s"
             message
             (cond (dest-folder (format " (to be refiled to %s)" dest-folder))
                   (deleted-flag (format " (to be deleted)"))
                   (t ""))
             (mapconcat 'concat
                        (mh-list-to-string (mh-seq-containing-msg message t))
                        " "))))

;; Avoid compiler warning
(defvar tool-bar-map)

;;;###mh-autoload
(defun mh-narrow-to-seq (sequence)
  "Restrict display of this folder to just messages in SEQUENCE.
Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive (list (mh-read-seq "Narrow to" t)))
  (with-mh-folder-updating (t)
    (cond ((mh-seq-to-msgs sequence)
           (mh-widen)
           (mh-remove-all-notation)
           (let ((eob (point-max))
                 (msg-at-cursor (mh-get-msg-num nil)))
             (setq mh-thread-old-scan-line-map mh-thread-scan-line-map)
             (setq mh-thread-scan-line-map (make-hash-table :test #'eql))
             (mh-copy-seq-to-eob sequence)
             (narrow-to-region eob (point-max))
             (setq mh-narrowed-to-seq sequence)
             (mh-notate-user-sequences)
             (mh-notate-deleted-and-refiled)
             (mh-notate-cur)
             (when msg-at-cursor (mh-goto-msg msg-at-cursor t t))
             (make-variable-buffer-local 'mh-non-seq-mode-line-annotation)
             (setq mh-non-seq-mode-line-annotation mh-mode-line-annotation)
             (setq mh-mode-line-annotation (symbol-name sequence))
             (mh-make-folder-mode-line)
             (mh-recenter nil)
             (when (and (boundp 'tool-bar-mode) tool-bar-mode)
               (set (make-local-variable 'tool-bar-map)
                    mh-folder-seq-tool-bar-map)
               (when (buffer-live-p (get-buffer mh-show-buffer))
                 (save-excursion
                   (set-buffer (get-buffer mh-show-buffer))
                   (set (make-local-variable 'tool-bar-map)
                        mh-show-seq-tool-bar-map))))
             (push 'widen mh-view-ops)))
          (t
           (error "No messages in sequence `%s'" (symbol-name sequence))))))

;;;###mh-autoload
(defun mh-put-msg-in-seq (msg-or-seq sequence)
  "Add MSG-OR-SEQ to SEQUENCE.
Default is the displayed message.
If optional prefix argument is provided, then prompt for the message sequence.
If variable `transient-mark-mode' is non-nil and the mark is active, then the
selected region is added to the sequence.
In a program, MSG-OR-SEQ can be a message number, a list of message numbers, a
region in a cons cell, or a sequence."
  (interactive (list (mh-interactive-msg-or-seq "Add messages from")
                     (mh-read-seq-default "Add to" nil)))
  (when (and (interactive-p) mh-tick-seq (eq sequence mh-tick-seq))
    (error "Use `mh-toggle-tick' to add messages to %s" mh-tick-seq))
  (let* ((internal-seq-flag (mh-internal-seq sequence))
         (note-seq (if internal-seq-flag nil mh-note-seq))
         (msg-list ()))
    (mh-iterate-on-msg-or-seq m msg-or-seq
      (push m msg-list)
      (mh-notate nil note-seq (1+ mh-cmd-note)))
    (mh-add-msgs-to-seq msg-list sequence nil t)
    (if (not internal-seq-flag)
        (setq mh-last-seq-used sequence))
    (when (and (eq sequence mh-unseen-seq) (mh-speed-flists-active-p))
      (mh-speed-flists t mh-current-folder))))

(defun mh-valid-view-change-operation-p (op)
  "Check if the view change operation can be performed.
OP is one of 'widen and 'unthread."
  (cond ((eq (car mh-view-ops) op)
         (pop mh-view-ops))
        (t nil)))

;;;###mh-autoload
(defun mh-widen ()
  "Remove restrictions from current folder, thereby showing all messages."
  (interactive)
  (let ((msg (mh-get-msg-num nil)))
    (when mh-narrowed-to-seq
      (cond ((mh-valid-view-change-operation-p 'widen) nil)
            ((memq 'widen mh-view-ops)
             (while (not (eq (car mh-view-ops) 'widen))
               (setq mh-view-ops (cdr mh-view-ops)))
             (pop mh-view-ops))
            (t (error "Widening is not applicable")))
      (when (memq 'unthread mh-view-ops)
        (setq mh-thread-scan-line-map mh-thread-old-scan-line-map))
      (with-mh-folder-updating (t)
        (delete-region (point-min) (point-max))
        (widen)
        (setq mh-mode-line-annotation mh-non-seq-mode-line-annotation)
        (mh-make-folder-mode-line))
      (if msg
          (mh-goto-msg msg t t))
      (setq mh-narrowed-to-seq nil)
      (setq mh-tick-seq-changed-when-narrowed-flag nil)
      (mh-notate-deleted-and-refiled)
      (mh-notate-user-sequences)
      (mh-notate-cur)
      (mh-recenter nil)))
  (when (and (boundp 'tool-bar-mode) tool-bar-mode)
    (set (make-local-variable 'tool-bar-map) mh-folder-tool-bar-map)
    (when (buffer-live-p (get-buffer mh-show-buffer))
      (save-excursion
        (set-buffer (get-buffer mh-show-buffer))
        (set (make-local-variable 'tool-bar-map) mh-show-tool-bar-map)))))

;; FIXME?  We may want to clear all notations and add one for current-message
;;         and process user sequences.
(defun mh-notate-deleted-and-refiled ()
  "Notate messages marked for deletion or refiling.
Messages to be deleted are given by `mh-delete-list' while messages to be
refiled are present in `mh-refile-list'."
  (let ((refiled-hash (make-hash-table))
        (deleted-hash (make-hash-table)))
    (dolist (msg mh-delete-list)
      (setf (gethash msg deleted-hash) t))
    (dolist (dest-msg-list mh-refile-list)
      (dolist (msg (cdr dest-msg-list))
        (setf (gethash msg refiled-hash) t)))
    (mh-iterate-on-messages-in-region msg (point-min) (point-max)
      (cond ((gethash msg refiled-hash)
             (mh-notate nil mh-note-refiled mh-cmd-note))
            ((gethash msg deleted-hash)
             (mh-notate nil mh-note-deleted mh-cmd-note))))))



;;; Commands to manipulate sequences.  Sequences are stored in an alist
;;; of the form:
;;;     ((seq-name msgs ...) (seq-name msgs ...) ...)

(defun mh-read-seq-default (prompt not-empty)
  "Read and return sequence name with default narrowed or previous sequence.
PROMPT is the prompt to use when reading. If NOT-EMPTY is non-nil then a
non-empty sequence is read."
  (mh-read-seq prompt not-empty
               (or mh-narrowed-to-seq
                   mh-last-seq-used
                   (car (mh-seq-containing-msg (mh-get-msg-num nil) nil)))))

(defun mh-read-seq (prompt not-empty &optional default)
  "Read and return a sequence name.
Prompt with PROMPT, raise an error if the sequence is empty and the NOT-EMPTY
flag is non-nil, and supply an optional DEFAULT sequence. A reply of '%'
defaults to the first sequence containing the current message."
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
  "Return an alist containing the names of the SEQ-LIST."
  (mapcar (lambda (entry) (list (symbol-name (mh-seq-name entry))))
          seq-list))

;;;###mh-autoload
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

;;;###mh-autoload
(defun mh-map-to-seq-msgs (func seq &rest args)
  "Invoke the FUNC at each message in the SEQ.
SEQ can either be a list of messages or a MH sequence. The remaining ARGS are
passed as arguments to FUNC."
  (save-excursion
    (let ((msgs (if (listp seq) seq (mh-seq-to-msgs seq))))
      (while msgs
        (if (mh-goto-msg (car msgs) t t)
            (apply func (car msgs) args))
        (setq msgs (cdr msgs))))))

;;;###mh-autoload
(defun mh-notate-seq (seq notation offset)
  "Mark the scan listing.
All messages in SEQ are marked with NOTATION at OFFSET from the beginning of
the line."
  (let ((msg-list (mh-seq-to-msgs seq)))
    (mh-iterate-on-messages-in-region msg (point-min) (point-max)
      (when (member msg msg-list)
        (mh-notate nil notation offset)))))

;;;###mh-autoload
(defun mh-notate-cur ()
  "Mark the MH sequence cur.
In addition to notating the current message with `mh-note-cur' the function
uses `overlay-arrow-position' to put a marker in the fringe."
  (let ((cur (car (mh-seq-to-msgs 'cur))))
    (when (and cur (mh-goto-msg cur t t))
      (beginning-of-line)
      (when (looking-at mh-scan-good-msg-regexp)
        (mh-notate nil mh-note-cur mh-cmd-note))
      (setq mh-arrow-marker (set-marker mh-arrow-marker (point)))
      (setq overlay-arrow-position mh-arrow-marker))))

;;;###mh-autoload
(defun mh-add-to-sequence (seq msgs)
  "The sequence SEQ is augmented with the messages in MSGS."
  ;; Add to a SEQUENCE each message the list of MSGS.
  (if (not (mh-folder-name-p seq))
      (if msgs
          (apply 'mh-exec-cmd "mark" mh-current-folder "-add"
                 "-sequence" (symbol-name seq)
                 (mh-coalesce-msg-list msgs)))))

;; This has a tricky bug. mh-map-to-seq-msgs uses mh-goto-msg, which assumes
;; that the folder buffer is sorted. However in this case that assumption
;; doesn't hold. So we will do this the dumb way.
;(defun mh-copy-seq-to-point (seq location)
;  ;; Copy the scan listing of the messages in SEQUENCE to after the point
;  ;; LOCATION in the current buffer.
;  (mh-map-to-seq-msgs 'mh-copy-line-to-point seq location))

(defvar mh-thread-last-ancestor)

(defun mh-copy-seq-to-eob (seq)
  "Copy SEQ to the end of the buffer."
  ;; It is quite involved to write something which will work at any place in
  ;; the buffer, so we will write something which works only at the end of
  ;; the buffer. If we ever need to insert sequences in the middle of the
  ;; buffer, this will need to be fixed.
  (save-excursion
    (let* ((msgs (mh-seq-to-msgs seq))
           (coalesced-msgs (mh-coalesce-msg-list msgs)))
      (goto-char (point-max))
      (save-restriction
        (narrow-to-region (point) (point))
        (mh-regenerate-headers coalesced-msgs t)
        (cond ((memq 'unthread mh-view-ops)
               ;; Populate restricted scan-line map
               (goto-char (point-min))
               (while (not (eobp))
                 (let ((msg (mh-get-msg-num nil)))
                   (when (numberp msg)
                     (setf (gethash msg mh-thread-scan-line-map)
                           (mh-thread-parse-scan-line))))
                 (forward-line))
               ;; Remove scan lines and read results from pre-computed tree
               (delete-region (point-min) (point-max))
               (mh-thread-print-scan-lines
                (mh-thread-generate mh-current-folder ())))
              (mh-index-data
               (mh-index-insert-folder-headers)))))))

(defun mh-copy-line-to-point (msg location)
  "Copy current message line to a specific location.
The argument MSG is not used. The message in the current line is copied to
LOCATION."
  ;; msg is not used?
  ;; Copy the current line to the LOCATION in the current buffer.
  (beginning-of-line)
  (save-excursion
    (let ((beginning-of-line (point))
          end)
      (forward-line 1)
      (setq end (point))
      (goto-char location)
      (insert-buffer-substring (current-buffer) beginning-of-line end))))

;;;###mh-autoload
(defmacro mh-iterate-on-messages-in-region (var begin end &rest body)
  "Iterate over region.
VAR is bound to the message on the current line as we loop starting from BEGIN
till END. In each step BODY is executed.

If VAR is nil then the loop is executed without any binding."
  (unless (symbolp var)
    (error "Can not bind the non-symbol %s" var))
  (let ((binding-needed-flag var))
    `(save-excursion
       (goto-char ,begin)
       (beginning-of-line)
       (while (and (<= (point) ,end) (not (eobp)))
         (when (looking-at mh-scan-valid-regexp)
           (let ,(if binding-needed-flag `((,var (mh-get-msg-num t))) ())
             ,@body))
         (forward-line 1)))))

(put 'mh-iterate-on-messages-in-region 'lisp-indent-hook 'defun)

;;;###mh-autoload
(defmacro mh-iterate-on-msg-or-seq (var msg-or-seq &rest body)
  "Iterate an operation over a region or sequence.

VAR is bound to each message in turn in a loop over MSG-OR-SEQ, which can be a
message number, a list of message numbers, a sequence, or a region in a cons
cell. In each iteration, BODY is executed.

The parameter MSG-OR-SEQ is usually created with `mh-interactive-msg-or-seq'
in order to provide a uniform interface to MH-E functions."
  (unless (symbolp var)
    (error "Can not bind the non-symbol %s" var))
  (let ((binding-needed-flag var)
        (msgs (make-symbol "msgs"))
        (seq-hash-table (make-symbol "seq-hash-table")))
    `(cond ((numberp ,msg-or-seq)
            (when (mh-goto-msg ,msg-or-seq t t)
              (let ,(if binding-needed-flag `((,var ,msg-or-seq)) ())
                ,@body)))
           ((and (consp ,msg-or-seq)
                 (numberp (car ,msg-or-seq)) (numberp (cdr ,msg-or-seq)))
            (mh-iterate-on-messages-in-region ,var
              (car ,msg-or-seq) (cdr ,msg-or-seq)
              ,@body))
           (t (let ((,msgs (if (and ,msg-or-seq (symbolp ,msg-or-seq))
                               (mh-seq-to-msgs ,msg-or-seq)
                             ,msg-or-seq))
                    (,seq-hash-table (make-hash-table)))
                (dolist (msg ,msgs)
                  (setf (gethash msg ,seq-hash-table) t))
                (mh-iterate-on-messages-in-region v (point-min) (point-max)
                  (when (gethash v ,seq-hash-table)
                    (let ,(if binding-needed-flag `((,var v)) ())
                      ,@body))))))))

(put 'mh-iterate-on-msg-or-seq 'lisp-indent-hook 'defun)

;;;###mh-autoload
(defun mh-msg-or-seq-to-msg-list (msg-or-seq)
  "Return a list of messages for MSG-OR-SEQ.
MSG-OR-SEQ can be a message number, a list of message numbers, a sequence, or
a region in a cons cell."
  (let (msg-list)
    (mh-iterate-on-msg-or-seq msg msg-or-seq
      (push msg msg-list))
    (nreverse msg-list)))

;;;###mh-autoload
(defun mh-interactive-msg-or-seq (sequence-prompt)
  "Return interactive specification for message, sequence, or region.
By convention, the name of this argument is msg-or-seq.

If variable `transient-mark-mode' is non-nil and the mark is active, then this
function returns a cons-cell of the region.
If optional prefix argument provided, then prompt for message sequence with
SEQUENCE-PROMPT and return sequence.
Otherwise, the message number at point is returned.

This function is usually used with `mh-iterate-on-msg-or-seq' in order to
provide a uniform interface to MH-E functions."
  (cond
   ((mh-mark-active-p t)
    (cons (region-beginning) (region-end)))
   (current-prefix-arg
    (mh-read-seq-default sequence-prompt t))
   (t
    (mh-get-msg-num t))))

;;;###mh-autoload
(defun mh-region-to-msg-list (begin end)
  "Return a list of messages within the region between BEGIN and END."
  ;; If end is end of buffer back up one position
  (setq end (if (equal end (point-max)) (1- end) end))
  (let ((result))
    (mh-iterate-on-messages-in-region index begin end
      (when (numberp index) (push index result)))
    result))



;;; Commands to handle new 'subject sequence.
;;; Or "Poor man's threading" by psg.

(defun mh-subject-to-sequence (all)
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
            (not (match-string 3))
            (string-equal "" (match-string 3)))
        (progn (message "No subject line.")
               nil)
      (let ((subject (match-string-no-properties 3))
            (list))
        (if (> (length subject) 41)
            (setq subject (substring subject 0 41)))
        (save-excursion
          (if all
              (goto-char (point-min)))
          (while (re-search-forward mh-scan-subject-regexp nil t)
            (let ((this-subject (match-string-no-properties 3)))
              (if (> (length this-subject) 41)
                  (setq this-subject (substring this-subject 0 41)))
              (if (string-equal this-subject subject)
                  (setq list (cons (mh-get-msg-num t) list))))))
        (cond
         (list
          ;; If we created a new sequence, add the initial message to it too.
          (if (not (member (mh-get-msg-num t) list))
              (setq list (cons (mh-get-msg-num t) list)))
          (if (member '("subject") (mh-seq-names mh-seq-list))
              (mh-delete-seq 'subject))
          ;; sort the result into a sequence
          (let ((sorted-list (sort (copy-sequence list) 'mh-lessp)))
            (while sorted-list
              (mh-add-msgs-to-seq (car sorted-list) 'subject nil)
              (setq sorted-list (cdr sorted-list)))
            (safe-length list)))
         (t
          0))))))

;;;###mh-autoload
(defun mh-narrow-to-subject ()
  "Narrow to a sequence containing all following messages with same subject."
  (interactive)
  (let ((num (mh-get-msg-num nil))
        (count (mh-subject-to-sequence t)))
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

;;;###mh-autoload
(defun mh-delete-subject ()
  "Mark all following messages with same subject to be deleted.
This puts the messages in a sequence named subject.  You can undo the last
deletion marks using `mh-undo' with a prefix argument and then specifying the
subject sequence."
  (interactive)
  (let ((count (mh-subject-to-sequence nil)))
    (cond
     ((not count)                       ; No subject line, delete msg anyway
      (mh-delete-msg (mh-get-msg-num t)))
     ((= 0 count)                       ; No other msgs, delete msg anyway.
      (message "No other messages with same Subject following this one.")
      (mh-delete-msg (mh-get-msg-num t)))
     (t                                 ; We have a subject sequence.
      (message "Marked %d messages for deletion" count)
      (mh-delete-msg 'subject)))))

;;;###mh-autoload
(defun mh-delete-subject-or-thread ()
  "Mark messages for deletion intelligently.
If the folder is threaded then `mh-thread-delete' is used to mark the current
message and all its descendants for deletion. Otherwise `mh-delete-subject' is
used to mark the current message and all messages following it with the same
subject for deletion."
  (interactive)
  (if (memq 'unthread mh-view-ops)
      (mh-thread-delete)
    (mh-delete-subject)))

;;; Message threading:

(defun mh-thread-initialize ()
  "Make hash tables, otherwise clear them."
  (cond
   (mh-thread-id-hash
    (clrhash mh-thread-id-hash)
    (clrhash mh-thread-subject-hash)
    (clrhash mh-thread-id-table)
    (clrhash mh-thread-id-index-map)
    (clrhash mh-thread-index-id-map)
    (clrhash mh-thread-scan-line-map)
    (clrhash mh-thread-subject-container-hash)
    (clrhash mh-thread-duplicates)
    (setq mh-thread-history ()))
   (t (setq mh-thread-id-hash (make-hash-table :test #'equal))
      (setq mh-thread-subject-hash (make-hash-table :test #'equal))
      (setq mh-thread-id-table (make-hash-table :test #'eq))
      (setq mh-thread-id-index-map (make-hash-table :test #'eq))
      (setq mh-thread-index-id-map (make-hash-table :test #'eql))
      (setq mh-thread-scan-line-map (make-hash-table :test #'eql))
      (setq mh-thread-subject-container-hash (make-hash-table :test #'eq))
      (setq mh-thread-duplicates (make-hash-table :test #'eq))
      (setq mh-thread-history ()))))

(defsubst mh-thread-id-container (id)
  "Given ID, return the corresponding container in `mh-thread-id-table'.
If no container exists then a suitable container is created and the id-table
is updated."
  (when (not id)
    (error "1"))
  (or (gethash id mh-thread-id-table)
      (setf (gethash id mh-thread-id-table)
            (let ((message (mh-thread-make-message :id id)))
              (mh-thread-make-container :message message)))))

(defsubst mh-thread-remove-parent-link (child)
  "Remove parent link of CHILD if it exists."
  (let* ((child-container (if (mh-thread-container-p child)
                              child (mh-thread-id-container child)))
         (parent-container (mh-container-parent child-container)))
    (when parent-container
      (setf (mh-container-children parent-container)
            (loop for elem in (mh-container-children parent-container)
                  unless (eq child-container elem) collect elem))
      (setf (mh-container-parent child-container) nil))))

(defsubst mh-thread-add-link (parent child &optional at-end-p)
  "Add links so that PARENT becomes a parent of CHILD.
Doesn't make any changes if CHILD is already an ancestor of PARENT. If
optional argument AT-END-P is non-nil, the CHILD is added to the end of the
children list of PARENT."
  (let ((parent-container (cond ((null parent) nil)
                                ((mh-thread-container-p parent) parent)
                                (t (mh-thread-id-container parent))))
        (child-container (if (mh-thread-container-p child)
                             child (mh-thread-id-container child))))
    (when (and parent-container
               (not (mh-thread-ancestor-p child-container parent-container))
               (not (mh-thread-ancestor-p parent-container child-container)))
      (mh-thread-remove-parent-link child-container)
      (cond ((not at-end-p)
             (push child-container (mh-container-children parent-container)))
            ((null (mh-container-children parent-container))
             (push child-container (mh-container-children parent-container)))
            (t (let ((last-child (mh-container-children parent-container)))
                 (while (cdr last-child)
                   (setq last-child (cdr last-child)))
                 (setcdr last-child (cons child-container nil)))))
      (setf (mh-container-parent child-container) parent-container))
    (unless parent-container
      (mh-thread-remove-parent-link child-container))))

(defun mh-thread-ancestor-p (ancestor successor)
  "Return t if ANCESTOR is really an ancestor of SUCCESSOR and nil otherwise.
In the limit, the function returns t if ANCESTOR and SUCCESSOR are the same
containers."
  (block nil
    (while successor
      (when (eq ancestor successor) (return t))
      (setq successor (mh-container-parent successor)))
    nil))

(defsubst mh-thread-get-message-container (message)
  "Return container which has MESSAGE in it.
If there is no container present then a new container is allocated."
  (let* ((id (mh-message-id message))
         (container (gethash id mh-thread-id-table)))
    (cond (container (setf (mh-container-message container) message)
                     container)
          (t (setf (gethash id mh-thread-id-table)
                   (mh-thread-make-container :message message))))))

(defsubst mh-thread-get-message (id subject-re-p subject refs)
  "Return appropriate message.
Otherwise update message already present to have the proper ID, SUBJECT-RE-P,
SUBJECT and REFS fields."
  (let* ((container (gethash id mh-thread-id-table))
         (message (if container (mh-container-message container) nil)))
    (cond (message
           (setf (mh-message-subject-re-p message) subject-re-p)
           (setf (mh-message-subject message) subject)
           (setf (mh-message-id message) id)
           (setf (mh-message-references message) refs)
           message)
          (container
           (setf (mh-container-message container)
                 (mh-thread-make-message :subject subject
                                         :subject-re-p subject-re-p
                                         :id id :references refs)))
          (t (let ((message (mh-thread-make-message
                             :subject subject
                             :subject-re-p subject-re-p
                             :id id :references refs)))
               (prog1 message
                 (mh-thread-get-message-container message)))))))

(defsubst mh-thread-canonicalize-id (id)
  "Produce canonical string representation for ID.
This allows cheap string comparison with EQ."
  (or (and (equal id "") (copy-sequence ""))
      (gethash id mh-thread-id-hash)
      (setf (gethash id mh-thread-id-hash) id)))

(defsubst mh-thread-prune-subject (subject)
  "Prune leading Re:'s, Fwd:'s etc. and trailing (fwd)'s from SUBJECT.
If the result after pruning is not the empty string then it is canonicalized
so that subjects can be tested for equality with eq. This is done so that all
the messages without a subject are not put into a single thread."
  (let ((case-fold-search t)
        (subject-pruned-flag nil))
    ;; Prune subject leader
    (while (or (string-match "^[ \t]*\\(re\\|fwd?\\)\\(\\[[0-9]*\\]\\)?:[ \t]*"
                             subject)
               (string-match "^[ \t]*\\[[^\\]][ \t]*" subject))
      (setq subject-pruned-flag t)
      (setq subject (substring subject (match-end 0))))
    ;; Prune subject trailer
    (while (or (string-match "(fwd)$" subject)
               (string-match "[ \t]+$" subject))
      (setq subject-pruned-flag t)
      (setq subject (substring subject 0 (match-beginning 0))))
    ;; Canonicalize subject only if it is non-empty
    (cond ((equal subject "") (values subject subject-pruned-flag))
          (t (values
              (or (gethash subject mh-thread-subject-hash)
                  (setf (gethash subject mh-thread-subject-hash) subject))
              subject-pruned-flag)))))

(defun mh-thread-container-subject (container)
  "Return the subject of CONTAINER.
If CONTAINER is empty return the subject info of one of its children."
  (cond ((and (mh-container-message container)
              (mh-message-id (mh-container-message container)))
         (mh-message-subject (mh-container-message container)))
        (t (block nil
             (dolist (kid (mh-container-children container))
               (when (and (mh-container-message kid)
                          (mh-message-id (mh-container-message kid)))
                 (let ((kid-message (mh-container-message kid)))
                   (return (mh-message-subject kid-message)))))
             (error "This can't happen!")))))

(defun mh-thread-rewind-pruning ()
  "Restore the thread tree to its state before pruning."
  (while mh-thread-history
    (let ((action (pop mh-thread-history)))
      (cond ((eq (car action) 'DROP)
             (mh-thread-remove-parent-link (cadr action))
             (mh-thread-add-link (caddr action) (cadr action)))
            ((eq (car action) 'PROMOTE)
             (let ((node (cadr action))
                   (parent (caddr action))
                   (children (cdddr action)))
               (dolist (child children)
                 (mh-thread-remove-parent-link child)
                 (mh-thread-add-link node child))
               (mh-thread-add-link parent node)))
            ((eq (car action) 'SUBJECT)
             (let ((node (cadr action)))
               (mh-thread-remove-parent-link node)
               (setf (mh-container-real-child-p node) t)))))))

(defun mh-thread-prune-containers (roots)
  "Prune empty containers in the containers ROOTS."
  (let ((dfs-ordered-nodes ())
        (work-list roots))
    (while work-list
      (let ((node (pop work-list)))
        (dolist (child (mh-container-children node))
          (push child work-list))
        (push node dfs-ordered-nodes)))
    (while dfs-ordered-nodes
      (let ((node (pop dfs-ordered-nodes)))
        (cond ((gethash (mh-message-id (mh-container-message node))
                        mh-thread-id-index-map)
               ;; Keep it
               (setf (mh-container-children node)
                     (mh-thread-sort-containers (mh-container-children node))))
              ((and (mh-container-children node)
                    (or (null (cdr (mh-container-children node)))
                        (mh-container-parent node)))
               ;; Promote kids
               (let ((children ()))
                 (dolist (kid (mh-container-children node))
                   (mh-thread-remove-parent-link kid)
                   (mh-thread-add-link (mh-container-parent node) kid)
                   (push kid children))
                 (push `(PROMOTE ,node ,(mh-container-parent node) ,@children)
                       mh-thread-history)
                 (mh-thread-remove-parent-link node)))
              ((mh-container-children node)
               ;; Promote the first orphan to parent and add the other kids as
               ;; his children
               (setf (mh-container-children node)
                     (mh-thread-sort-containers (mh-container-children node)))
               (let ((new-parent (car (mh-container-children node)))
                     (other-kids (cdr (mh-container-children node))))
                 (mh-thread-remove-parent-link new-parent)
                 (dolist (kid other-kids)
                   (mh-thread-remove-parent-link kid)
                   (setf (mh-container-real-child-p kid) nil)
                   (mh-thread-add-link new-parent kid t))
                 (push `(PROMOTE ,node ,(mh-container-parent node)
                                 ,new-parent ,@other-kids)
                       mh-thread-history)
                 (mh-thread-remove-parent-link node)))
              (t
               ;; Drop it
               (push `(DROP ,node ,(mh-container-parent node))
                     mh-thread-history)
               (mh-thread-remove-parent-link node)))))
    (let ((results ()))
      (maphash #'(lambda (k v)
                   (declare (ignore k))
                   (when (and (null (mh-container-parent v))
                              (gethash (mh-message-id (mh-container-message v))
                                       mh-thread-id-index-map))
                     (push v results)))
               mh-thread-id-table)
      (mh-thread-sort-containers results))))

(defun mh-thread-sort-containers (containers)
  "Sort a list of message CONTAINERS to be in ascending order wrt index."
  (sort containers
        #'(lambda (x y)
            (when (and (mh-container-message x) (mh-container-message y))
              (let* ((id-x (mh-message-id (mh-container-message x)))
                     (id-y (mh-message-id (mh-container-message y)))
                     (index-x (gethash id-x mh-thread-id-index-map))
                     (index-y (gethash id-y mh-thread-id-index-map)))
                (and (integerp index-x) (integerp index-y)
                     (< index-x index-y)))))))

(defsubst mh-thread-group-by-subject (roots)
  "Group the set of message containers, ROOTS based on subject.
Bug: Check for and make sure that something without Re: is made the parent in
preference to something that has it."
  (clrhash mh-thread-subject-container-hash)
  (let ((results ()))
    (dolist (root roots)
      (let* ((subject (mh-thread-container-subject root))
             (parent (gethash subject mh-thread-subject-container-hash)))
        (cond (parent (mh-thread-remove-parent-link root)
                      (mh-thread-add-link parent root t)
                      (setf (mh-container-real-child-p root) nil)
                      (push `(SUBJECT ,root) mh-thread-history))
              (t
               (setf (gethash subject mh-thread-subject-container-hash) root)
               (push root results)))))
    (nreverse results)))

(defsubst mh-thread-process-in-reply-to (reply-to-header)
  "Extract message id's from REPLY-TO-HEADER.
Ideally this should have some regexp which will try to guess if a string
between < and > is a message id and not an email address. For now it will
take the last string inside angles."
  (let ((end (mh-search-from-end ?> reply-to-header)))
    (when (numberp end)
      (let ((begin (mh-search-from-end ?< (substring reply-to-header 0 end))))
        (when (numberp begin)
          (list (substring reply-to-header begin (1+ end))))))))

(defun mh-thread-set-tables (folder)
  "Use the tables of FOLDER in current buffer."
  (flet ((mh-get-table (symbol)
                       (save-excursion
                         (set-buffer folder)
                         (symbol-value symbol))))
    (setq mh-thread-id-hash (mh-get-table 'mh-thread-id-hash))
    (setq mh-thread-subject-hash (mh-get-table 'mh-thread-subject-hash))
    (setq mh-thread-id-table (mh-get-table 'mh-thread-id-table))
    (setq mh-thread-id-index-map (mh-get-table 'mh-thread-id-index-map))
    (setq mh-thread-index-id-map (mh-get-table 'mh-thread-index-id-map))
    (setq mh-thread-scan-line-map (mh-get-table 'mh-thread-scan-line-map))
    (setq mh-thread-subject-container-hash
          (mh-get-table 'mh-thread-subject-container-hash))
    (setq mh-thread-duplicates (mh-get-table 'mh-thread-duplicates))
    (setq mh-thread-history (mh-get-table 'mh-thread-history))))

(defsubst mh-thread-update-id-index-maps (id index)
  "Message with id, ID is the message in INDEX.
The function also checks for duplicate messages (that is multiple messages
with the same ID). These messages are put in the `mh-thread-duplicates' hash
table."
  (let ((old-index (gethash id mh-thread-id-index-map)))
    (when old-index (push old-index (gethash id mh-thread-duplicates)))
    (setf (gethash id mh-thread-id-index-map) index)
    (setf (gethash index mh-thread-index-id-map) id)))



;;; Generate Threads...

(defvar mh-message-id-regexp "^<.*@.*>$"
  "Regexp to recognize whether a string is a message identifier.")

(defun mh-thread-generate (folder msg-list)
  "Scan FOLDER to get info for threading.
Only information about messages in MSG-LIST are added to the tree."
  (with-temp-buffer
    (mh-thread-set-tables folder)
    (when msg-list
      (apply
       #'call-process (expand-file-name mh-scan-prog mh-progs) nil '(t nil) nil
       "-width" "10000" "-format"
       "%(msg)\n%{message-id}\n%{references}\n%{in-reply-to}\n%{subject}\n"
       folder (mapcar #'(lambda (x) (format "%s" x)) msg-list)))
    (goto-char (point-min))
    (let ((roots ())
          (case-fold-search t))
      (block nil
        (while (not (eobp))
          (block process-message
            (let* ((index-line
                    (prog1 (buffer-substring (point) (line-end-position))
                      (forward-line)))
                   (index (car (read-from-string index-line)))
                   (id (prog1 (buffer-substring (point) (line-end-position))
                         (forward-line)))
                   (refs (prog1 (buffer-substring (point) (line-end-position))
                           (forward-line)))
                   (in-reply-to (prog1 (buffer-substring (point)
                                                         (line-end-position))
                                  (forward-line)))
                   (subject (prog1
                                (buffer-substring (point) (line-end-position))
                              (forward-line)))
                   (subject-re-p nil))
              (unless (gethash index mh-thread-scan-line-map)
                (return-from process-message))
              (unless (integerp index) (return)) ;Error message here
              (multiple-value-setq (subject subject-re-p)
                (mh-thread-prune-subject subject))
              (setq in-reply-to (mh-thread-process-in-reply-to in-reply-to))
              (setq refs (loop for x in (append (split-string refs) in-reply-to)
                               when (string-match mh-message-id-regexp x)
                               collect x))
              (setq id (mh-thread-canonicalize-id id))
              (mh-thread-update-id-index-maps id index)
              (setq refs (mapcar #'mh-thread-canonicalize-id refs))
              (mh-thread-get-message id subject-re-p subject refs)
              (do ((ancestors refs (cdr ancestors)))
                  ((null (cdr ancestors))
                   (when (car ancestors)
                     (mh-thread-remove-parent-link id)
                     (mh-thread-add-link (car ancestors) id)))
                (mh-thread-add-link (car ancestors) (cadr ancestors)))))))
      (maphash #'(lambda (k v)
                   (declare (ignore k))
                   (when (null (mh-container-parent v))
                     (push v roots)))
               mh-thread-id-table)
      (setq roots (mh-thread-prune-containers roots))
      (prog1 (setq roots (mh-thread-group-by-subject roots))
        (let ((history mh-thread-history))
          (set-buffer folder)
          (setq mh-thread-history history))))))

;;;###mh-autoload
(defun mh-thread-inc (folder start-point)
  "Update thread tree for FOLDER.
All messages after START-POINT are added to the thread tree."
  (mh-thread-rewind-pruning)
  (goto-char start-point)
  (let ((msg-list ()))
    (while (not (eobp))
      (let ((index (mh-get-msg-num nil)))
        (when (numberp index)
          (push index msg-list)
          (setf (gethash index mh-thread-scan-line-map)
                (mh-thread-parse-scan-line)))
        (forward-line)))
    (let ((thread-tree (mh-thread-generate folder msg-list))
          (buffer-read-only nil)
          (old-buffer-modified-flag (buffer-modified-p)))
      (delete-region (point-min) (point-max))
      (mh-thread-print-scan-lines thread-tree)
      (mh-notate-user-sequences)
      (mh-notate-deleted-and-refiled)
      (mh-notate-cur)
      (set-buffer-modified-p old-buffer-modified-flag))))

(defun mh-thread-generate-scan-lines (tree level)
  "Generate scan lines.
TREE is the hierarchical tree of messages, SCAN-LINE-MAP maps message indices
to the corresponding scan lines and LEVEL used to determine indentation of
the message."
  (cond ((null tree) nil)
        ((mh-thread-container-p tree)
         (let* ((message (mh-container-message tree))
                (id (mh-message-id message))
                (index (gethash id mh-thread-id-index-map))
                (duplicates (gethash id mh-thread-duplicates))
                (new-level (+ level 2))
                (dupl-flag t)
                (force-angle-flag nil)
                (increment-level-flag nil))
           (dolist (scan-line (mapcar (lambda (x)
                                        (gethash x mh-thread-scan-line-map))
                                      (reverse (cons index duplicates))))
             (when scan-line
               (when (and dupl-flag (equal level 0)
                          (mh-thread-ancestor-p mh-thread-last-ancestor tree))
                 (setq level (+ level 2)
                       new-level (+ new-level 2)
                       force-angle-flag t))
               (when (equal level 0)
                 (setq mh-thread-last-ancestor tree)
                 (while (mh-container-parent mh-thread-last-ancestor)
                   (setq mh-thread-last-ancestor
                         (mh-container-parent mh-thread-last-ancestor))))
               (let* ((lev (if dupl-flag level new-level))
                      (square-flag (or (and (mh-container-real-child-p tree)
                                            (not force-angle-flag)
                                            dupl-flag)
                                       (equal lev 0))))
                 (insert (car scan-line)
                         (format (format "%%%ss" lev) "")
                         (if square-flag "[" "<")
                         (cadr scan-line)
                         (if square-flag "]" ">")
                         (truncate-string-to-width
                          (caddr scan-line) (- mh-thread-body-width lev))
                         "\n"))
               (setq increment-level-flag t)
               (setq dupl-flag nil)))
           (unless increment-level-flag (setq new-level level))
           (dolist (child (mh-container-children tree))
             (mh-thread-generate-scan-lines child new-level))))
        (t (let ((nlevel (+ level 2)))
             (dolist (ch tree)
               (mh-thread-generate-scan-lines ch nlevel))))))

;; Another and may be better approach would be to generate all the info from
;; the scan which generates the threading info. For now this will have to do.
(defun mh-thread-parse-scan-line (&optional string)
  "Parse a scan line.
If optional argument STRING is given then that is assumed to be the scan line.
Otherwise uses the line at point as the scan line to parse."
  (let* ((string (or string
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position))))
         (first-string (substring string 0 (+ mh-cmd-note 8))))
    (setf (elt first-string mh-cmd-note) ? )
    (when (equal (elt first-string (1+ mh-cmd-note)) (elt mh-note-seq 0))
      (setf (elt first-string (1+ mh-cmd-note)) ? ))
    (list first-string
          (substring string
                     (+ mh-cmd-note mh-scan-field-from-start-offset)
                     (+ mh-cmd-note mh-scan-field-from-end-offset -2))
          (substring string (+ mh-cmd-note mh-scan-field-from-end-offset))
          string)))

;;;###mh-autoload
(defun mh-thread-add-spaces (count)
  "Add COUNT spaces to each scan line in `mh-thread-scan-line-map'."
  (let ((spaces (format (format "%%%ss" count) "")))
    (while (not (eobp))
      (let* ((msg-num (mh-get-msg-num nil))
             (old-line (nth 3 (gethash msg-num mh-thread-scan-line-map))))
        (when (numberp msg-num)
          (setf (gethash msg-num mh-thread-scan-line-map)
                (mh-thread-parse-scan-line (format "%s%s" spaces old-line)))))
      (forward-line 1))))

(defun mh-thread-print-scan-lines (thread-tree)
  "Print scan lines in THREAD-TREE in threaded mode."
  (let ((mh-thread-body-width (- (window-width) mh-cmd-note
                                 (1- mh-scan-field-subject-start-offset)))
        (mh-thread-last-ancestor nil))
    (if (null mh-index-data)
        (mh-thread-generate-scan-lines thread-tree -2)
      (loop for x in (mh-index-group-by-folder)
            do (let* ((old-map mh-thread-scan-line-map)
                      (mh-thread-scan-line-map (make-hash-table)))
                 (setq mh-thread-last-ancestor nil)
                 (loop for msg in (cdr x)
                       do (let ((v (gethash msg old-map)))
                            (when v
                              (setf (gethash msg mh-thread-scan-line-map) v))))
                 (when (> (hash-table-count mh-thread-scan-line-map) 0)
                   (insert (if (bobp) "" "\n") (car x) "\n")
                   (mh-thread-generate-scan-lines thread-tree -2)))))))

(defun mh-thread-folder ()
  "Generate thread view of folder."
  (message "Threading %s..." (buffer-name))
  (mh-thread-initialize)
  (goto-char (point-min))
  (let ((msg-list ()))
    (while (not (eobp))
      (let ((index (mh-get-msg-num nil)))
        (when (numberp index)
          (push index msg-list)
          (setf (gethash index mh-thread-scan-line-map)
                (mh-thread-parse-scan-line))))
      (forward-line))
    (let* ((range (mh-coalesce-msg-list msg-list))
           (thread-tree (mh-thread-generate (buffer-name) range)))
      (delete-region (point-min) (point-max))
      (mh-thread-print-scan-lines thread-tree)
      (mh-notate-user-sequences)
      (mh-notate-deleted-and-refiled)
      (mh-notate-cur)
      (message "Threading %s...done" (buffer-name)))))

;;;###mh-autoload
(defun mh-toggle-threads ()
  "Toggle threaded view of folder."
  (interactive)
  (let ((msg-at-point (mh-get-msg-num nil))
        (old-buffer-modified-flag (buffer-modified-p))
        (buffer-read-only nil))
    (cond ((memq 'unthread mh-view-ops)
           (unless (mh-valid-view-change-operation-p 'unthread)
             (error "Can't unthread folder"))
           (let ((msg-list ()))
             (goto-char (point-min))
             (while (not (eobp))
               (let ((index (mh-get-msg-num nil)))
                 (when index
                   (push index msg-list)))
               (forward-line))
             (mh-scan-folder mh-current-folder
                             (mapcar #'(lambda (x) (format "%s" x))
                                     (mh-coalesce-msg-list msg-list))
                             t))
           (when mh-index-data
             (mh-index-insert-folder-headers)
             (mh-notate-cur)))
          (t (mh-thread-folder)
             (push 'unthread mh-view-ops)))
    (when msg-at-point (mh-goto-msg msg-at-point t t))
    (set-buffer-modified-p old-buffer-modified-flag)
    (mh-recenter nil)))

;;;###mh-autoload
(defun mh-thread-forget-message (index)
  "Forget the message INDEX from the threading tables."
  (let* ((id (gethash index mh-thread-index-id-map))
         (id-index (gethash id mh-thread-id-index-map))
         (duplicates (gethash id mh-thread-duplicates)))
    (remhash index mh-thread-index-id-map)
    (remhash index mh-thread-scan-line-map)
    (cond ((and (eql index id-index) (null duplicates))
           (remhash id mh-thread-id-index-map))
          ((eql index id-index)
           (setf (gethash id mh-thread-id-index-map) (car duplicates))
           (setf (gethash (car duplicates) mh-thread-index-id-map) id)
           (setf (gethash id mh-thread-duplicates) (cdr duplicates)))
          (t
           (setf (gethash id mh-thread-duplicates)
                 (remove index duplicates))))))



;;; Operations on threads

(defun mh-thread-current-indentation-level ()
  "Find the number of spaces by which current message is indented."
  (save-excursion
    (let ((address-start-offset (+ mh-cmd-note mh-scan-date-flag-width
                                   mh-scan-date-width 1))
          (level 0))
      (beginning-of-line)
      (forward-char address-start-offset)
      (while (char-equal (char-after) ? )
        (incf level)
        (forward-char))
      level)))

;;;###mh-autoload
(defun mh-thread-next-sibling (&optional previous-flag)
  "Jump to next sibling.
With non-nil optional argument PREVIOUS-FLAG jump to the previous sibling."
  (interactive)
  (cond ((not (memq 'unthread mh-view-ops))
         (error "Folder isn't threaded"))
        ((eobp)
         (error "No message at point")))
  (beginning-of-line)
  (let ((point (point))
        (done nil)
        (my-level (mh-thread-current-indentation-level)))
    (while (and (not done)
                (equal (forward-line (if previous-flag -1 1)) 0)
                (not (eobp)))
      (let ((level (mh-thread-current-indentation-level)))
        (cond ((equal level my-level)
               (setq done 'success))
              ((< level my-level)
               (message "No %s sibling" (if previous-flag "previous" "next"))
               (setq done 'failure)))))
    (cond ((eq done 'success) (mh-maybe-show))
          ((eq done 'failure) (goto-char point))
          (t (message "No %s sibling" (if previous-flag "previous" "next"))
             (goto-char point)))))

;;;###mh-autoload
(defun mh-thread-previous-sibling ()
  "Jump to previous sibling."
  (interactive)
  (mh-thread-next-sibling t))

(defun mh-thread-immediate-ancestor ()
  "Jump to immediate ancestor in thread tree."
  (beginning-of-line)
  (let ((point (point))
        (ancestor-level (- (mh-thread-current-indentation-level) 2))
        (done nil))
    (if (< ancestor-level 0)
        nil
      (while (and (not done) (equal (forward-line -1) 0))
        (when (equal ancestor-level (mh-thread-current-indentation-level))
          (setq done t)))
      (unless done
        (goto-char point))
      done)))

;;;###mh-autoload
(defun mh-thread-ancestor (&optional thread-root-flag)
  "Jump to the ancestor of current message.
If optional argument THREAD-ROOT-FLAG is non-nil then jump to the root of the
thread tree the message belongs to."
  (interactive "P")
  (beginning-of-line)
  (cond ((not (memq 'unthread mh-view-ops))
         (error "Folder isn't threaded"))
        ((eobp)
         (error "No message at point")))
  (let ((current-level (mh-thread-current-indentation-level)))
    (cond (thread-root-flag
           (while (mh-thread-immediate-ancestor))
           (mh-maybe-show))
          ((equal current-level 1)
           (message "Message has no ancestor"))
          (t (mh-thread-immediate-ancestor)
             (mh-maybe-show)))))

(defun mh-thread-find-children ()
  "Return a region containing the current message and its children.
The result is returned as a list of two elements. The first is the point at the
start of the region and the second is the point at the end."
  (beginning-of-line)
  (if (eobp)
      nil
    (let ((address-start-offset (+ mh-cmd-note mh-scan-date-flag-width
                                   mh-scan-date-width 1))
          (level (mh-thread-current-indentation-level))
          spaces begin)
      (setq begin (point))
      (setq spaces (format (format "%%%ss" (1+ level)) ""))
      (forward-line)
      (block nil
        (while (not (eobp))
          (forward-char address-start-offset)
          (unless (equal (string-match spaces (buffer-substring-no-properties
                                               (point) (line-end-position)))
                         0)
            (beginning-of-line)
            (backward-char)
            (return))
          (forward-line)))
      (list begin (point)))))

;;;###mh-autoload
(defun mh-thread-delete ()
  "Mark current message and all its children for subsequent deletion."
  (interactive)
  (cond ((not (memq 'unthread mh-view-ops))
         (error "Folder isn't threaded"))
        ((eobp)
         (error "No message at point"))
        (t (let ((region (mh-thread-find-children)))
             (mh-iterate-on-messages-in-region () (car region) (cadr region)
               (mh-delete-a-msg nil))
             (mh-next-msg)))))

;;;###mh-autoload
(defun mh-thread-refile (folder)
  "Mark current message and all its children for refiling to FOLDER."
  (interactive (list (intern (mh-prompt-for-refile-folder))))
  (cond ((not (memq 'unthread mh-view-ops))
         (error "Folder isn't threaded"))
        ((eobp)
         (error "No message at point"))
        (t (let ((region (mh-thread-find-children)))
             (mh-iterate-on-messages-in-region () (car region) (cadr region)
               (mh-refile-a-msg nil folder))
             (mh-next-msg)))))



;; Tick mark handling

;; Functions to highlight and unhighlight ticked messages.
(defun mh-tick-add-overlay ()
  "Add tick overlay to current line."
  (with-mh-folder-updating (t)
    (let ((overlay
           (or (mh-funcall-if-exists make-overlay (point) (line-end-position))
               (mh-funcall-if-exists make-extent (point) (line-end-position)))))
      (or (mh-funcall-if-exists overlay-put overlay 'face 'mh-folder-tick-face)
          (mh-funcall-if-exists set-extent-face overlay 'mh-folder-tick-face))
      (mh-funcall-if-exists set-extent-priority overlay 10)
      (add-text-properties (point) (line-end-position) `(mh-tick ,overlay)))))

(defun mh-tick-remove-overlay ()
  "Remove tick overlay from current line."
  (let ((overlay (get-text-property (point) 'mh-tick)))
    (when overlay
      (with-mh-folder-updating (t)
        (or (mh-funcall-if-exists delete-overlay overlay)
            (mh-funcall-if-exists delete-extent overlay))
        (remove-text-properties (point) (line-end-position) `(mh-tick nil))))))

;;;###mh-autoload
(defun mh-notate-tick (msg ticked-msgs &optional ignore-narrowing)
  "Highlight current line if MSG is in TICKED-MSGS.
If optional argument IGNORE-NARROWING is non-nil then highlighting is carried
out even if folder is narrowed to `mh-tick-seq'."
  (when mh-tick-seq
    (let ((narrowed-to-tick (and (not ignore-narrowing)
                                 (eq mh-narrowed-to-seq mh-tick-seq)))
          (overlay (get-text-property (point) 'mh-tick))
          (in-tick (member msg ticked-msgs)))
      (cond (narrowed-to-tick (mh-tick-remove-overlay))
            ((and (not overlay) in-tick) (mh-tick-add-overlay))
            ((and overlay (not in-tick)) (mh-tick-remove-overlay))))))

;; Interactive function to toggle tick.
;;;###mh-autoload
(defun mh-toggle-tick (begin end)
  "Toggle tick mark of all messages in region BEGIN to END."
  (interactive (cond ((mh-mark-active-p t)
                      (list (region-beginning) (region-end)))
                     (t (list (line-beginning-position) (line-end-position)))))
  (unless mh-tick-seq
    (error "Enable ticking by customizing `mh-tick-seq'"))
  (let* ((tick-seq (mh-find-seq mh-tick-seq))
         (tick-seq-msgs (mh-seq-msgs tick-seq)))
    (mh-iterate-on-messages-in-region msg begin end
      (cond ((member msg tick-seq-msgs)
             (mh-undefine-sequence mh-tick-seq (list msg))
             (setcdr tick-seq (delq msg (cdr tick-seq)))
             (when (null (cdr tick-seq)) (setq mh-last-seq-used nil))
             (mh-tick-remove-overlay))
            (t
             (mh-add-msgs-to-seq (list msg) mh-tick-seq nil t)
             (setq mh-last-seq-used mh-tick-seq)
             (mh-tick-add-overlay))))
    (when (and (eq mh-tick-seq mh-narrowed-to-seq)
               (not mh-tick-seq-changed-when-narrowed-flag))
      (setq mh-tick-seq-changed-when-narrowed-flag t)
      (let ((ticked-msgs (mh-seq-msgs (mh-find-seq mh-tick-seq))))
        (mh-iterate-on-messages-in-region msg (point-min) (point-max)
          (mh-notate-tick msg ticked-msgs t))))))

;;;###mh-autoload
(defun mh-narrow-to-tick ()
  "Restrict display of this folder to just messages in `mh-tick-seq'.
Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive)
  (cond ((not mh-tick-seq)
         (error "Enable ticking by customizing `mh-tick-seq'"))
        ((null (mh-seq-msgs (mh-find-seq mh-tick-seq)))
         (message "No messages in tick sequence"))
        (t (mh-narrow-to-seq mh-tick-seq))))


(provide 'mh-seq)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 8e952711-01a2-485b-bf21-c9e3ad4de942
;;; mh-seq.el ends here
