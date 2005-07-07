;;; mh-seq.el --- MH-E sequences support

;; Copyright (C) 1993, 1995,
;; 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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

(eval-when-compile (require 'mh-acros))
(mh-require-cl)
(require 'mh-e)

;; Shush the byte-compiler
(defvar tool-bar-mode)

;;; Data structures (used in message threading)...
(mh-defstruct (mh-thread-message (:conc-name mh-message-)
                                 (:constructor mh-thread-make-message))
  (id nil)
  (references ())
  (subject "")
  (subject-re-p nil))

(mh-defstruct (mh-thread-container (:conc-name mh-container-)
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
(defvar mh-thread-scan-line-map-stack nil
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
(make-variable-buffer-local 'mh-thread-scan-line-map-stack)
(make-variable-buffer-local 'mh-thread-subject-container-hash)
(make-variable-buffer-local 'mh-thread-duplicates)
(make-variable-buffer-local 'mh-thread-history)

;;;###mh-autoload
(defun mh-delete-seq (sequence)
  "Delete the SEQUENCE."
  (interactive (list (mh-read-seq-default "Delete" t)))
  (let ((msg-list (mh-seq-to-msgs sequence))
        (internal-flag (mh-internal-seq sequence))
        (folders-changed (list mh-current-folder)))
    (mh-iterate-on-range msg sequence
      (mh-remove-sequence-notation msg internal-flag))
    (mh-undefine-sequence sequence '("all"))
    (mh-delete-seq-locally sequence)
    (when mh-index-data
      (setq folders-changed
            (append folders-changed
                    (mh-index-delete-from-sequence sequence msg-list))))
    (when (and (eq sequence mh-unseen-seq) (mh-speed-flists-active-p))
      (apply #'mh-speed-flists t folders-changed))))

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
        (view-mode-enter)
        (setq view-exit-action 'kill-buffer)
        (message "Listing sequences...done")))))

;;;###mh-autoload
(defun mh-msg-is-in-seq (message)
  "Display the sequences in which the current message appears.
Use a prefix argument to display the sequences in which another MESSAGE
appears."
  (interactive "P")
  (if (not message)
      (setq message (mh-get-msg-num t)))
  (let* ((dest-folder (loop for seq in mh-refile-list
                            when (member message (cdr seq)) return (car seq)
                            finally return nil))
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
           (mh-remove-all-notation)
           (let ((eob (point-max))
                 (msg-at-cursor (mh-get-msg-num nil)))
             (push mh-thread-scan-line-map mh-thread-scan-line-map-stack)
             (setq mh-thread-scan-line-map (make-hash-table :test #'eql))
             (mh-copy-seq-to-eob sequence)
             (push (buffer-substring-no-properties (point-min) eob)
                   mh-folder-view-stack)
             (delete-region (point-min) eob)
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
(defun mh-put-msg-in-seq (range sequence)
  "Add RANGE to SEQUENCE.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use."
  (interactive (list (mh-interactive-range "Add messages from")
                     (mh-read-seq-default "Add to" nil)))
  (unless (mh-valid-seq-p sequence)
    (error "Can't put message in invalid sequence `%s'" sequence))
  (let* ((internal-seq-flag (mh-internal-seq sequence))
         (original-msgs (mh-seq-msgs (mh-find-seq sequence)))
         (folders (list mh-current-folder))
         (msg-list (mh-range-to-msg-list range)))
    (mh-add-msgs-to-seq msg-list sequence nil t)
    (mh-iterate-on-range m range
      (unless (memq m original-msgs)
        (mh-add-sequence-notation m internal-seq-flag)))
    (if (not internal-seq-flag)
        (setq mh-last-seq-used sequence))
    (when mh-index-data
      (setq folders
            (append folders (mh-index-add-to-sequence sequence msg-list))))
    (when (and (eq sequence mh-unseen-seq) (mh-speed-flists-active-p))
      (apply #'mh-speed-flists t folders))))

(defun mh-valid-view-change-operation-p (op)
  "Check if the view change operation can be performed.
OP is one of 'widen and 'unthread."
  (cond ((eq (car mh-view-ops) op)
         (pop mh-view-ops))
        (t nil)))

;;;###mh-autoload
(defun mh-widen (&optional all-flag)
  "Restore the previous limit.
If optional prefix argument ALL-FLAG is non-nil, remove all limits."
  (interactive "P")
  (let ((msg (mh-get-msg-num nil)))
    (when mh-folder-view-stack
      (cond (all-flag
             (while (cdr mh-view-ops)
               (setq mh-view-ops (cdr mh-view-ops)))
             (when (eq (car mh-view-ops) 'widen)
               (setq mh-view-ops (cdr mh-view-ops))))
            ((mh-valid-view-change-operation-p 'widen) nil)
            ((memq 'widen mh-view-ops)
             (while (not (eq (car mh-view-ops) 'widen))
               (setq mh-view-ops (cdr mh-view-ops)))
             (setq mh-view-ops (cdr mh-view-ops)))
            (t (error "Widening is not applicable")))
      ;; If ALL-FLAG is non-nil then rewind stacks
      (when all-flag
        (while (cdr mh-thread-scan-line-map-stack)
          (setq mh-thread-scan-line-map-stack
                (cdr mh-thread-scan-line-map-stack)))
        (while (cdr mh-folder-view-stack)
          (setq mh-folder-view-stack (cdr mh-folder-view-stack))))
      (setq mh-thread-scan-line-map (pop mh-thread-scan-line-map-stack))
      (with-mh-folder-updating (t)
        (delete-region (point-min) (point-max))
        (insert (pop mh-folder-view-stack))
        (mh-remove-all-notation)
        (setq mh-mode-line-annotation mh-non-seq-mode-line-annotation)
        (mh-make-folder-mode-line))
      (if msg
          (mh-goto-msg msg t t))
      (mh-notate-deleted-and-refiled)
      (mh-notate-user-sequences)
      (mh-notate-cur)
      (mh-recenter nil)))
  (when (and (null mh-folder-view-stack) (boundp 'tool-bar-mode) tool-bar-mode)
    (set (make-local-variable 'tool-bar-map) mh-folder-tool-bar-map)
    (when (buffer-live-p (get-buffer mh-show-buffer))
      (save-excursion
        (set-buffer (get-buffer mh-show-buffer))
        (set (make-local-variable 'tool-bar-map) mh-show-tool-bar-map)))))

;; FIXME?  We may want to clear all notations and add one for current-message
;;         and process user sequences.
;;;###mh-autoload
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

(defvar mh-sequence-history ())

;;;###mh-autoload
(defun mh-read-seq-default (prompt not-empty)
  "Read and return sequence name with default narrowed or previous sequence.
PROMPT is the prompt to use when reading. If NOT-EMPTY is non-nil then a
non-empty sequence is read."
  (mh-read-seq prompt not-empty
               (or mh-last-seq-used
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
                                 (mh-seq-names mh-seq-list)
                                 nil nil nil 'mh-sequence-history))
         (seq (cond ((equal input "%")
                     (car (mh-seq-containing-msg (mh-get-msg-num t) nil)))
                    ((equal input "") default)
                    (t (intern input))))
         (msgs (mh-seq-to-msgs seq)))
    (if (and (null msgs) not-empty)
        (error "No messages in sequence `%s'" seq))
    seq))

;;; Functions to read ranges with completion...
(defvar mh-range-seq-names)
(defvar mh-range-history ())
(defvar mh-range-completion-map (copy-keymap minibuffer-local-completion-map))
(define-key mh-range-completion-map " " 'self-insert-command)

(defun mh-range-completion-function (string predicate flag)
  "Programmable completion of message ranges.
STRING is the user input that is to be completed. PREDICATE if non-nil is a
function used to filter the possible choices and FLAG determines whether the
completion is over."
  (let* ((candidates mh-range-seq-names)
         (last-char (and (not (equal string ""))
                         (aref string (1- (length string)))))
         (last-word (cond ((null last-char) "")
                          ((memq last-char '(?  ?- ?:)) "")
                          (t (car (last (split-string string "[ -:]+"))))))
         (prefix (substring string 0 (- (length string) (length last-word)))))
    (cond ((eq flag nil)
           (let ((res (try-completion last-word candidates predicate)))
             (cond ((null res) nil)
                   ((eq res t) t)
                   (t (concat prefix res)))))
          ((eq flag t)
           (all-completions last-word candidates predicate))
          ((eq flag 'lambda)
           (loop for x in candidates
                 when (equal x last-word) return t
                 finally return nil)))))

;;;###mh-autoload
(defun mh-read-range (prompt &optional folder default
                             expand-flag ask-flag number-as-range-flag)
  "Read a message range with PROMPT.

If FOLDER is non-nil then a range is read from that folder, otherwise use
`mh-current-folder'.

If DEFAULT is a string then use that as default range to return. If DEFAULT is
nil then ask user with default answer a range based on the sequences that seem
relevant. Finally if DEFAULT is t, try to avoid prompting the user. Unseen
messages, if present, are returned. If the folder has fewer than
`mh-large-folder' messages then \"all\" messages are returned. Finally as a
last resort prompt the user.

If EXPAND-FLAG is non-nil then a list of message numbers corresponding to the
input is returned. If this list is empty then an error is raised. If
EXPAND-FLAG is nil just return the input string. In this case we don't check
if the range is empty.

If ASK-FLAG is non-nil, then the user is always queried for a range of
messages. If ASK-FLAG is nil, then the function checks if the unseen sequence
is non-empty. If that is the case, `mh-unseen-seq', or the list of messages in
it depending on the value of EXPAND, is returned. Otherwise if the folder has
fewer than `mh-large-folder' messages then the list of messages corresponding
to \"all\" is returned. If neither of the above holds then as a last resort
the user is queried for a range of messages.

If NUMBER-AS-RANGE-FLAG is non-nil, then if a number, N is read as input, it
is interpreted as the range \"last:N\".

This function replaces the existing function `mh-read-msg-range'. Calls to:
  (mh-read-msg-range folder flag)
should be replaced with:
  (mh-read-range \"Suitable prompt\" folder t nil flag
                 mh-interpret-number-as-range-flag)"
  (setq default (or default mh-last-seq-used
                    (car (mh-seq-containing-msg (mh-get-msg-num nil) t)))
        prompt (format "%s range" prompt))
  (let* ((folder (or folder mh-current-folder))
         (default (cond ((or (eq default t) (stringp default)) default)
                        ((symbolp default) (symbol-name default))))
         (guess (eq default t))
         (counts (and guess (mh-folder-size folder)))
         (unseen (and counts (> (cadr counts) 0)))
         (large (and counts mh-large-folder (> (car counts) mh-large-folder)))
         (str (cond ((and guess large
                          (setq default (format "last:%s" mh-large-folder)
                                prompt (format "%s (folder has %s messages)"
                                               prompt (car counts)))
                          nil))
                    ((and guess (not large) (setq default "all") nil))
                    ((eq default nil) "")
                    (t (format "[%s] " default))))
         (minibuffer-local-completion-map mh-range-completion-map)
         (seq-list (if (eq folder mh-current-folder)
                       mh-seq-list
                     (mh-read-folder-sequences folder nil)))
         (mh-range-seq-names
          (append '(("first") ("last") ("all") ("prev") ("next"))
                  (mh-seq-names seq-list)))
         (input (cond ((and (not ask-flag) unseen) (symbol-name mh-unseen-seq))
                      ((and (not ask-flag) (not large)) "all")
                      (t (completing-read (format "%s: %s" prompt str)
                                          'mh-range-completion-function nil nil
                                          nil 'mh-range-history default))))
         msg-list)
    (when (and number-as-range-flag
               (string-match "^[ \t]*\\([0-9]+\\)[ \t]*$" input))
      (setq input (concat "last:" (match-string 1 input))))
    (cond ((not expand-flag) input)
          ((assoc (intern input) seq-list)
           (cdr (assoc (intern input) seq-list)))
          ((setq msg-list (mh-translate-range folder input)) msg-list)
          (t (error "No messages in range `%s'" input)))))

;;;###mh-autoload
(defun mh-translate-range (folder expr)
  "In FOLDER, translate the string EXPR to a list of messages numbers."
  (save-excursion
    (let ((strings (delete "" (split-string expr "[ \t\n]")))
          (result ()))
      (ignore-errors
        (apply #'mh-exec-cmd-quiet nil "mhpath" folder strings)
        (set-buffer mh-temp-buffer)
        (goto-char (point-min))
        (while (re-search-forward "/\\([0-9]*\\)$" nil t)
          (push (car (read-from-string (match-string 1))) result))
        (nreverse result)))))

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
  (if (and (mh-valid-seq-p seq) (not (mh-folder-name-p seq)))
      (if msgs
          (apply 'mh-exec-cmd "mark" mh-current-folder "-add"
                 "-sequence" (symbol-name seq)
                 (mh-coalesce-msg-list msgs)))))

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
               (mh-remove-all-notation)
               (mh-iterate-on-range msg (cons (point-min) (point-max))
                 (setf (gethash msg mh-thread-scan-line-map)
                       (mh-thread-parse-scan-line)))
               ;; Remove scan lines and read results from pre-computed tree
               (delete-region (point-min) (point-max))
               (mh-thread-print-scan-lines
                (mh-thread-generate mh-current-folder ()))
               (mh-notate-user-sequences))
              (mh-index-data
               (mh-index-insert-folder-headers)))))))

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
(defmacro mh-iterate-on-range (var range &rest body)
  "Iterate an operation over a region or sequence.

VAR is bound to each message in turn in a loop over RANGE, which can be a
message number, a list of message numbers, a sequence, a region in a cons
cell, or a MH range (something like last:20) in a string. In each iteration,
BODY is executed.

The parameter RANGE is usually created with `mh-interactive-range'
in order to provide a uniform interface to MH-E functions."
  (unless (symbolp var)
    (error "Can not bind the non-symbol %s" var))
  (let ((binding-needed-flag var)
        (msgs (make-symbol "msgs"))
        (seq-hash-table (make-symbol "seq-hash-table")))
    `(cond ((numberp ,range)
            (when (mh-goto-msg ,range t t)
              (let ,(if binding-needed-flag `((,var ,range)) ())
                ,@body)))
           ((and (consp ,range)
                 (numberp (car ,range)) (numberp (cdr ,range)))
            (mh-iterate-on-messages-in-region ,var
              (car ,range) (cdr ,range)
              ,@body))
           (t (let ((,msgs (cond ((and ,range (symbolp ,range))
                                  (mh-seq-to-msgs ,range))
                                 ((stringp ,range)
                                  (mh-translate-range mh-current-folder
                                                      ,range))
                                 (t ,range)))
                    (,seq-hash-table (make-hash-table)))
                (dolist (msg ,msgs)
                  (setf (gethash msg ,seq-hash-table) t))
                (mh-iterate-on-messages-in-region v (point-min) (point-max)
                  (when (gethash v ,seq-hash-table)
                    (let ,(if binding-needed-flag `((,var v)) ())
                      ,@body))))))))

(put 'mh-iterate-on-range 'lisp-indent-hook 'defun)

;;;###mh-autoload
(defun mh-range-to-msg-list (range)
  "Return a list of messages for RANGE.
RANGE can be a message number, a list of message numbers, a sequence, or
a region in a cons cell."
  (let (msg-list)
    (mh-iterate-on-range msg range
      (push msg msg-list))
    (nreverse msg-list)))

;;;###mh-autoload
(defun mh-interactive-range (range-prompt &optional default)
  "Return interactive specification for message, sequence, range or region.
By convention, the name of this argument is RANGE.

If variable `transient-mark-mode' is non-nil and the mark is active, then this
function returns a cons-cell of the region.

If optional prefix argument is provided, then prompt for message range with
RANGE-PROMPT. A list of messages in that range is returned.

If a MH range is given, say something like last:20, then a list containing
the messages in that range is returned.

If DEFAULT non-nil then it is returned.

Otherwise, the message number at point is returned.

This function is usually used with `mh-iterate-on-range' in order to provide
a uniform interface to MH-E functions."
  (cond ((mh-mark-active-p t) (cons (region-beginning) (region-end)))
        (current-prefix-arg (mh-read-range range-prompt nil nil t t))
        (default default)
        (t (mh-get-msg-num t))))



;;; Commands to handle new 'subject sequence.
;;; Or "Poor man's threading" by psg.

;;; XXX: The function mh-subject-to-sequence-unthreaded uses the magic number
;;;  41 for the max size of the subject part. Avoiding this would be desirable.
(defun mh-subject-to-sequence (all)
  "Put all following messages with same subject in sequence 'subject.
If arg ALL is t, move to beginning of folder buffer to collect all messages.
If arg ALL is nil, collect only messages fron current one on forward.

Return number of messages put in the sequence:

 nil -> there was no subject line.
 0   -> there were no later messages with the same subject (sequence not made)
 >1  -> the total number of messages including current one."
  (if (memq 'unthread mh-view-ops)
      (mh-subject-to-sequence-threaded all)
    (mh-subject-to-sequence-unthreaded all)))

(defun mh-subject-to-sequence-unthreaded (all)
  "Put all following messages with same subject in sequence 'subject.
This function only works with an unthreaded folder. If arg ALL is t, move to
beginning of folder buffer to collect all messages. If arg ALL is nil, collect
only messages fron current one on forward.

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
        (progn (message "No subject line")
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
          (if (assoc 'subject mh-seq-list) (mh-delete-seq 'subject))
          ;; sort the result into a sequence
          (let ((sorted-list (sort (copy-sequence list) 'mh-lessp)))
            (while sorted-list
              (mh-add-msgs-to-seq (car sorted-list) 'subject nil)
              (setq sorted-list (cdr sorted-list)))
            (safe-length list)))
         (t
          0))))))

(defun mh-subject-to-sequence-threaded (all)
  "Put all messages with the same subject in the 'subject sequence.
This function works when the folder is threaded. In this situation the subject
could get truncated and so the normal matching doesn't work.

The parameter ALL is non-nil then all the messages in the buffer are
considered, otherwise only the messages after the current one are taken into
account."
  (let* ((cur (mh-get-msg-num nil))
         (subject (mh-thread-find-msg-subject cur))
         region msgs)
    (if (null subject)
        (and (message "No subject line") nil)
      (setq region (cons (if all (point-min) (point)) (point-max)))
      (mh-iterate-on-range msg region
        (when (eq (mh-thread-find-msg-subject msg) subject)
          (push msg msgs)))
      (setq msgs (sort msgs #'mh-lessp))
      (if (null msgs)
          0
        (when (assoc 'subject mh-seq-list)
          (mh-delete-seq 'subject))
        (mh-add-msgs-to-seq msgs 'subject)
        (length msgs)))))

(defun mh-thread-find-msg-subject (msg)
  "Find canonicalized subject of MSG.
This function can only be used the folder is threaded."
  (ignore-errors
    (mh-message-subject
     (mh-container-message (gethash (gethash msg mh-thread-index-id-map)
                                    mh-thread-id-table)))))

(defun mh-edit-pick-expr (default)
  "With prefix arg edit a pick expression.
If no prefix arg is given, then return DEFAULT."
  (let ((default-string (loop for x in default concat (format " %s" x))))
    (if (or current-prefix-arg (equal default-string ""))
        (mh-pick-args-list (read-string "Pick expression: "
                                        default-string))
      default)))

(defun mh-pick-args-list (s)
  "Form list by grouping elements in string S suitable for pick arguments.
For example, the string \"-subject a b c -from Joe User <user@domain.com>\"
is converted to (\"-subject\" \"a b c\" \"-from\"
\"Joe User <user@domain.com>\""
  (let ((full-list (split-string s))
        current-arg collection arg-list)
    (while full-list
      (setq current-arg (car full-list))
      (if (null (string-match "^-" current-arg))
          (setq collection
                (if (null collection)
                    current-arg
                  (format "%s %s" collection current-arg)))
        (when collection
          (setq arg-list (append arg-list (list collection)))
          (setq collection nil))
        (setq arg-list (append arg-list (list current-arg))))
      (setq full-list (cdr full-list)))
    (when collection
      (setq arg-list (append arg-list (list collection))))
    arg-list))

;;;###mh-autoload
(defun mh-narrow-to-subject (&optional pick-expr)
  "Limit to messages with same subject.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive
   (list (mh-edit-pick-expr (mh-current-message-header-field 'subject))))
  (mh-narrow-to-header-field 'subject pick-expr))

;;;###mh-autoload
(defun mh-narrow-to-from (&optional pick-expr)
  "Limit to messages with the same `From:' field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive
   (list (mh-edit-pick-expr (mh-current-message-header-field 'from))))
  (mh-narrow-to-header-field 'from pick-expr))

;;;###mh-autoload
(defun mh-narrow-to-cc (&optional pick-expr)
  "Limit to messages with the same `Cc:' field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive
   (list (mh-edit-pick-expr (mh-current-message-header-field 'cc))))
  (mh-narrow-to-header-field 'cc pick-expr))

;;;###mh-autoload
(defun mh-narrow-to-to (&optional pick-expr)
  "Limit to messages with the same `To:' field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive
   (list (mh-edit-pick-expr (mh-current-message-header-field 'to))))
  (mh-narrow-to-header-field 'to pick-expr))

(defun mh-narrow-to-header-field (header-field pick-expr)
  "Limit to messages whose HEADER-FIELD match PICK-EXPR.
The MH command pick is used to do the match."
  (let ((folder mh-current-folder)
        (original (mh-coalesce-msg-list
                   (mh-range-to-msg-list (cons (point-min) (point-max)))))
        (msg-list ()))
    (with-temp-buffer
      (apply #'mh-exec-cmd-output "pick" nil folder
             (append original (list "-list") pick-expr))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((num (read-from-string
                    (buffer-substring (point) (line-end-position)))))
          (when (numberp (car num)) (push (car num) msg-list))
          (forward-line))))
    (if (null msg-list)
        (message "No matches")
      (when (assoc 'header mh-seq-list) (mh-delete-seq 'header))
      (mh-add-msgs-to-seq msg-list 'header)
      (mh-narrow-to-seq 'header))))

(defun mh-current-message-header-field (header-field)
  "Return a pick regexp to match HEADER-FIELD of the message at point."
  (let ((num (mh-get-msg-num nil)))
    (when num
      (let ((folder mh-current-folder))
        (with-temp-buffer
          (insert-file-contents-literally (mh-msg-filename num folder))
          (goto-char (point-min))
          (when (search-forward "\n\n" nil t)
            (narrow-to-region (point-min) (point)))
          (let* ((field (or (message-fetch-field (format "%s" header-field))
                            ""))
                 (field-option (format "-%s" header-field))
                 (patterns (loop for x in (split-string  field "[ ]*,[ ]*")
                                 unless (equal x "")
                                 collect (if (string-match "<\\(.*@.*\\)>" x)
                                             (match-string 1 x)
                                           x))))
            (when patterns
              (loop with accum = `(,field-option ,(car patterns))
                    for e in (cdr patterns)
                    do (setq accum `(,field-option ,e "-or" ,@accum))
                    finally return accum))))))))

;;;###mh-autoload
(defun mh-narrow-to-range (range)
  "Limit to messages in RANGE.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive (list (mh-interactive-range "Narrow to")))
  (when (assoc 'range mh-seq-list) (mh-delete-seq 'range))
  (mh-add-msgs-to-seq (mh-range-to-msg-list range) 'range)
  (mh-narrow-to-seq 'range))


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
      (message "No other messages with same Subject following this one")
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

(defmacro mh-thread-initialize-hash (var test)
  "Initialize the hash table in VAR.
TEST is the test to use when creating a new hash table."
  (unless (symbolp var) (error "Expected a symbol: %s" var))
  `(if ,var (clrhash ,var) (setq ,var (make-hash-table :test ,test))))

(defun mh-thread-initialize ()
  "Make new hash tables, or clear them if already present."
  (mh-thread-initialize-hash mh-thread-id-hash #'equal)
  (mh-thread-initialize-hash mh-thread-subject-hash #'equal)
  (mh-thread-initialize-hash mh-thread-id-table #'eq)
  (mh-thread-initialize-hash mh-thread-id-index-map #'eq)
  (mh-thread-initialize-hash mh-thread-index-id-map #'eql)
  (mh-thread-initialize-hash mh-thread-scan-line-map #'eql)
  (mh-thread-initialize-hash mh-thread-subject-container-hash #'eq)
  (mh-thread-initialize-hash mh-thread-duplicates #'eq)
  (setq mh-thread-history ()))

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
                 (mh-thread-make-message :id id :references refs
                                         :subject subject
                                         :subject-re-p subject-re-p)))
          (t (let ((message (mh-thread-make-message :id id :references refs
                                                    :subject-re-p subject-re-p
                                                    :subject subject)))
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

(defun mh-thread-process-in-reply-to (reply-to-header)
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
  (mh-remove-all-notation)
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
         (address-start (+ mh-cmd-note mh-scan-field-from-start-offset))
         (body-start (+ mh-cmd-note mh-scan-field-from-end-offset))
         (first-string (substring string 0 address-start)))
    (list first-string
          (substring string address-start (- body-start 2))
          (substring string body-start)
          string)))

;;;###mh-autoload
(defun mh-thread-update-scan-line-map (msg notation offset)
  "In threaded view update `mh-thread-scan-line-map'.
MSG is the message being notated with NOTATION at OFFSET."
  (let* ((msg (or msg (mh-get-msg-num nil)))
         (cur-scan-line (and mh-thread-scan-line-map
                             (gethash msg mh-thread-scan-line-map)))
         (old-scan-lines (loop for map in mh-thread-scan-line-map-stack
                               collect (and map (gethash msg map)))))
    (when cur-scan-line
      (setf (aref (car cur-scan-line) offset) notation))
    (dolist (line old-scan-lines)
      (when line (setf (aref (car line) offset) notation)))))

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
                   (mh-thread-generate-scan-lines thread-tree -2))))
      (mh-index-create-imenu-index))))

(defun mh-thread-folder ()
  "Generate thread view of folder."
  (message "Threading %s..." (buffer-name))
  (mh-thread-initialize)
  (goto-char (point-min))
  (mh-remove-all-notation)
  (let ((msg-list ()))
    (mh-iterate-on-range msg (cons (point-min) (point-max))
      (setf (gethash msg mh-thread-scan-line-map) (mh-thread-parse-scan-line))
      (push msg msg-list))
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

;;;###mh-autoload
(defun mh-toggle-tick (range)
  "Toggle tick mark of all messages in RANGE."
  (interactive (list (mh-interactive-range "Tick")))
  (unless mh-tick-seq
    (error "Enable ticking by customizing `mh-tick-seq'"))
  (let* ((tick-seq (mh-find-seq mh-tick-seq))
         (tick-seq-msgs (mh-seq-msgs tick-seq))
         (ticked ())
         (unticked ()))
    (mh-iterate-on-range msg range
      (cond ((member msg tick-seq-msgs)
             (push msg unticked)
             (setcdr tick-seq (delq msg (cdr tick-seq)))
             (when (null (cdr tick-seq)) (setq mh-last-seq-used nil))
             (mh-remove-sequence-notation msg (mh-colors-in-use-p)))
            (t
             (push msg ticked)
             (setq mh-last-seq-used mh-tick-seq)
             (let ((mh-seq-list (cons `(,mh-tick-seq ,msg) mh-seq-list)))
               (mh-add-sequence-notation msg (mh-colors-in-use-p))))))
    (mh-add-msgs-to-seq ticked mh-tick-seq nil t)
    (mh-undefine-sequence mh-tick-seq unticked)
    (when mh-index-data
      (mh-index-add-to-sequence mh-tick-seq ticked)
      (mh-index-delete-from-sequence mh-tick-seq unticked))))

;;;###mh-autoload
(defun mh-narrow-to-tick ()
  "Limit to messages in `mh-tick-seq'.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive)
  (cond ((not mh-tick-seq)
         (error "Enable ticking by customizing `mh-tick-seq'"))
        ((null (mh-seq-msgs (mh-find-seq mh-tick-seq)))
         (message "No messages in %s sequence" mh-tick-seq))
        (t (mh-narrow-to-seq mh-tick-seq))))

(provide 'mh-seq)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 8e952711-01a2-485b-bf21-c9e3ad4de942
;;; mh-seq.el ends here
