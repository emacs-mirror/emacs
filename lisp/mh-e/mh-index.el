;;; mh-index  --  MH-E interface to indexing programs

;; Copyright (C) 2002, 2003 Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;;  (1) The following search engines are supported:
;;;        swish++
;;;        swish-e
;;;        mairix
;;;        namazu
;;;        glimpse
;;;        grep
;;;
;;;  (2) To use this package, you first have to build an index. Please read
;;;      the documentation for `mh-index-search' to get started. That
;;;      documentation will direct you to the specific instructions for your
;;;      particular indexer.

;;; Change Log:

;;; Code:

(require 'cl)
(require 'mh-e)
(require 'mh-mime)
(require 'mh-pick)

(autoload 'gnus-local-map-property "gnus-util")
(autoload 'gnus-eval-format "gnus-spec")
(autoload 'widget-convert-button "wid-edit")
(autoload 'executable-find "executable")

;; Support different indexing programs
(defvar mh-indexer-choices
  '((swish++
     mh-swish++-binary mh-swish++-execute-search mh-swish++-next-result
     mh-swish++-regexp-builder)
    (swish
     mh-swish-binary mh-swish-execute-search mh-swish-next-result nil)
    (mairix
     mh-mairix-binary mh-mairix-execute-search mh-mairix-next-result
     mh-mairix-regexp-builder)
    (namazu
     mh-namazu-binary mh-namazu-execute-search mh-namazu-next-result nil)
    (glimpse
     mh-glimpse-binary mh-glimpse-execute-search mh-glimpse-next-result nil)
    (pick
     mh-pick-binary mh-pick-execute-search mh-pick-next-result
     mh-pick-regexp-builder)
    (grep
     mh-grep-binary mh-grep-execute-search mh-grep-next-result nil))
  "List of possible indexer choices.")
(defvar mh-indexer nil
  "Chosen index program.")
(defvar mh-index-execute-search-function nil
  "Function which executes the search program.")
(defvar mh-index-next-result-function nil
  "Function to parse the next line of output.")
(defvar mh-index-regexp-builder nil
  "Function used to construct search regexp.")

;; FIXME: This should be a defcustom...
(defvar mh-index-folder "+mhe-index"
  "Folder that contains the folders resulting from the index searches.")

;; Temporary buffers for search results
(defvar mh-index-temp-buffer " *mh-index-temp*")
(defvar mh-checksum-buffer " *mh-checksum-buffer*")



;;; A few different checksum programs are supported. The supported programs
;;; are:
;;;   1. md5sum
;;;   2. md5
;;;   3. openssl
;;;
;;; To add support for your favorite checksum program add a clause to the cond
;;; statement in mh-checksum-choose. This should set the variable
;;; mh-checksum-cmd to the command line needed to run the checsum program and
;;; should set mh-checksum-parser to a function which returns a cons cell
;;; containing the message number and checksum string.

(defvar mh-checksum-cmd)
(defvar mh-checksum-parser)

(defun mh-checksum-choose ()
  "Check if a program to create a checksum is present."
  (unless (boundp 'mh-checksum-cmd)
    (let ((exec-path (append '("/sbin" "/usr/sbin") exec-path)))
      (cond ((executable-find "md5sum")
             (setq mh-checksum-cmd (list (executable-find "md5sum")))
             (setq mh-checksum-parser #'mh-md5sum-parser))
            ((executable-find "openssl")
             (setq mh-checksum-cmd (list (executable-find "openssl") "md5"))
             (setq mh-checksum-parser #'mh-openssl-parser))
            ((executable-find "md5")
             (setq mh-checksum-cmd (list (executable-find "md5")))
             (setq mh-checksum-parser #'mh-md5-parser))
            (t (error "No suitable checksum program"))))))

(defun mh-md5sum-parser ()
  "Parse md5sum output."
  (let ((begin (line-beginning-position))
        (end (line-end-position))
        first-space last-slash)
    (setq first-space (search-forward " " end t))
    (goto-char end)
    (setq last-slash (search-backward "/" begin t))
    (cond ((and first-space last-slash)
           (cons (car (read-from-string (buffer-substring-no-properties
                                         (1+ last-slash) end)))
                 (buffer-substring-no-properties begin (1- first-space))))
          (t (cons nil nil)))))

(defun mh-openssl-parser ()
  "Parse openssl output."
  (let ((begin (line-beginning-position))
        (end (line-end-position))
        last-space last-slash)
    (goto-char end)
    (setq last-space (search-backward " " begin t))
    (setq last-slash (search-backward "/" begin t))
    (cond ((and last-slash last-space)
           (cons (car (read-from-string (buffer-substring-no-properties
                                         (1+ last-slash) (1- last-space))))
                 (buffer-substring-no-properties (1+ last-space) end))))))

(defalias 'mh-md5-parser 'mh-openssl-parser)



;;; Make sure that we don't produce too long a command line.

(defvar mh-index-max-cmdline-args 500
  "Maximum number of command line args.")

(defun mh-index-execute (cmd &rest args)
  "Partial imitation of xargs.
The current buffer contains a list of strings, one on each line. The function
will execute CMD with ARGS and pass the first `mh-index-max-cmdline-args'
strings to it. This is repeated till all the strings have been used."
  (goto-char (point-min))
  (let ((current-buffer (current-buffer)))
    (with-temp-buffer
      (let ((out (current-buffer)))
        (set-buffer current-buffer)
        (while (not (eobp))
          (let ((arg-list (reverse args))
                (count 0))
            (while (and (not (eobp)) (< count mh-index-max-cmdline-args))
              (push (buffer-substring-no-properties (point) (line-end-position))
                    arg-list)
              (incf count)
              (forward-line))
            (apply #'call-process cmd nil (list out nil) nil
                   (nreverse arg-list))))
        (erase-buffer)
        (insert-buffer-substring out)))))



(defun mh-index-update-single-msg (msg checksum origin-map)
  "Update various maps for one message.
MSG is a index folder message, CHECKSUM its MD5 hash and ORIGIN-MAP, if
non-nil, a hashtable containing which maps each message in the index folder to
the folder and message that it was copied from. The function updates the hash
tables `mh-index-msg-checksum-map' and `mh-index-checksum-origin-map'.

This function should only be called in the appropriate index folder buffer."
  (cond ((and origin-map (gethash checksum mh-index-checksum-origin-map))
         (let* ((intermediate (gethash msg origin-map))
                (ofolder (car intermediate))
                (omsg (cdr intermediate)))
           ;; This is most probably a duplicate. So eliminate it.
           (call-process "rm" nil nil nil
                         (format "%s%s/%s" mh-user-path
                                 (substring mh-current-folder 1) msg))
           (remhash omsg (gethash ofolder mh-index-data))))
        (t
         (setf (gethash msg mh-index-msg-checksum-map) checksum)
         (when origin-map
           (setf (gethash checksum mh-index-checksum-origin-map)
                 (gethash msg origin-map))))))

;;;###mh-autoload
(defun mh-index-update-maps (folder &optional origin-map)
  "Annotate all as yet unannotated messages in FOLDER with their MD5 hash.
As a side effect msg -> checksum map is updated. Optional argument ORIGIN-MAP
is a hashtable which maps each message in the index folder to the original
folder and message from whence it was copied. If present the
checksum -> (origin-folder, origin-index) map is updated too."
  (clrhash mh-index-msg-checksum-map)
  (save-excursion
    ;; Clear temp buffer
    (set-buffer (get-buffer-create mh-checksum-buffer))
    (erase-buffer)
    ;; Run scan to check if any messages needs MD5 annotations at all
    (with-temp-buffer
      (mh-exec-cmd-output mh-scan-prog nil "-width" "80"
                          "-format" "%(msg)\n%{x-mhe-checksum}\n"
                          folder "all")
      (goto-char (point-min))
      (let (msg checksum)
        (while (not (eobp))
          (setq msg (buffer-substring-no-properties
                     (point) (line-end-position)))
          (forward-line)
          (save-excursion
            (cond ((not (string-match "^[0-9]*$" msg)))
                  ((eolp)
                   ;; need to compute checksum
                   (set-buffer mh-checksum-buffer)
                   (insert mh-user-path (substring folder 1) "/" msg "\n"))
                  (t
                   ;; update maps
                   (setq checksum (buffer-substring-no-properties
                                   (point) (line-end-position)))
                   (let ((msg (car (read-from-string msg))))
                     (set-buffer folder)
                     (mh-index-update-single-msg msg checksum origin-map)))))
          (forward-line))))
    ;; Run checksum program if needed
    (unless (and (eobp) (bobp))
      (apply #'mh-index-execute mh-checksum-cmd)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((intermediate (funcall mh-checksum-parser))
               (msg (car intermediate))
               (checksum (cdr intermediate)))
          (when msg
            ;; annotate
            (mh-exec-cmd "anno" folder msg "-component" "X-MHE-Checksum"
                         "-nodate" "-text" checksum "-inplace")
            ;; update maps
            (save-excursion
              (set-buffer folder)
              (mh-index-update-single-msg msg checksum origin-map)))
          (forward-line))))))

(defvar mh-flists-results-folder "new"
  "Subfolder for `mh-index-folder' where flists output is placed.")

(defun mh-index-generate-pretty-name (string)
  "Given STRING generate a name which is suitable for use as a folder name.
White space from the beginning and end are removed. All spaces in the name are
replaced with underscores and all / are replaced with $. If STRING is longer
than 20 it is truncated too. STRING could be a list of strings in which case
they are concatenated to construct the base name."
  (with-temp-buffer
    (if (stringp string)
        (insert string)
      (when (car string) (insert (car string)))
      (dolist (s (cdr string))
        (insert "_" s)))
    (setq string (mh-replace-string "-lbrace" " "))
    (setq string (mh-replace-string "-rbrace" " "))
    (subst-char-in-region (point-min) (point-max) ?( ?  t)
    (subst-char-in-region (point-min) (point-max) ?) ?  t)
    (subst-char-in-region (point-min) (point-max) ?- ?  t)
    (goto-char (point-min))
    (while (and (not (eobp)) (memq (char-after) '(?  ?\t ?\n ?\r ?_)))
      (delete-char 1))
    (goto-char (point-max))
    (while (and (not (bobp)) (memq (char-before) '(?  ?\t ?\n ?\r ?_)))
      (delete-backward-char 1))
    (subst-char-in-region (point-min) (point-max) ?  ?_ t)
    (subst-char-in-region (point-min) (point-max) ?\t ?_ t)
    (subst-char-in-region (point-min) (point-max) ?\n ?_ t)
    (subst-char-in-region (point-min) (point-max) ?\r ?_ t)
    (subst-char-in-region (point-min) (point-max) ?/ ?$ t)
    (let ((out (truncate-string-to-width (buffer-string) 20)))
      (cond ((eq mh-indexer 'flists) mh-flists-results-folder)
            ((equal out mh-flists-results-folder) (concat out "1"))
            (t out)))))

;;;###mh-autoload
(defun* mh-index-search (redo-search-flag folder search-regexp
                        &optional window-config unseen-flag)
  "Perform an indexed search in an MH mail folder.
Use a prefix argument to repeat the search, as in REDO-SEARCH-FLAG below.

If REDO-SEARCH-FLAG is non-nil and the current folder buffer was generated by a
index search, then the search is repeated. Otherwise, FOLDER is searched with
SEARCH-REGEXP and the results are presented in an MH-E folder. If FOLDER is
\"+\" then mail in all folders are searched. Optional argument WINDOW-CONFIG
stores the window configuration that will be restored after the user quits the
folder containing the index search results. If optional argument UNSEEN-FLAG
is non-nil, then all the messages are marked as unseen.

Four indexing programs are supported; if none of these are present, then grep
is used. This function picks the first program that is available on your
system. If you would prefer to use a different program, set the customization
variable `mh-index-program' accordingly.

The documentation for the following functions describes how to generate the
index for each program:

    - `mh-swish++-execute-search'
    - `mh-swish-execute-search'
    - `mh-mairix-execute-search'
    - `mh-namazu-execute-search'
    - `mh-glimpse-execute-search'

If none of these programs are present then we use pick. If desired grep can be
used instead. Details about these methods can be found in:

    - `mh-pick-execute-search'
    - `mh-grep-execute-search'

This and related functions use an X-MHE-Checksum header to cache the MD5
checksum of a message. This means that already present X-MHE-Checksum headers
in the incoming email could result in messages not being found. The following
procmail recipe should avoid this:

  :0 wf
  | formail -R \"X-MHE-Checksum\" \"Old-X-MHE-Checksum\"

This has the effect of renaming already present X-MHE-Checksum headers."
  (interactive
   (list current-prefix-arg
         (progn
           (unless mh-find-path-run (mh-find-path))
           (or (and current-prefix-arg (car mh-index-previous-search))
               (mh-prompt-for-folder "Search" "+" nil "all" t)))
         (progn
           ;; Yes, we do want to call mh-index-choose every time in case the
           ;; user has switched the indexer manually.
           (unless (mh-index-choose) (error "No indexing program found"))
           (or (and current-prefix-arg (cadr mh-index-previous-search))
               mh-index-regexp-builder
               (read-string (format "%s regexp: "
                                    (upcase-initials
                                     (symbol-name mh-indexer))))))
         (if (and (not
                   (and current-prefix-arg (cadr mh-index-previous-search)))
                  mh-index-regexp-builder)
             (current-window-configuration)
           nil)))
  (when (symbolp search-regexp)
    (mh-search-folder folder window-config)
    (setq mh-searching-function 'mh-index-do-search)
    (return-from mh-index-search))
  (mh-checksum-choose)
  (let ((result-count 0)
        (old-window-config (or window-config mh-previous-window-config))
        (previous-search mh-index-previous-search)
        (index-folder (format "%s/%s" mh-index-folder
                              (mh-index-generate-pretty-name search-regexp))))
    ;; Create a new folder for the search results or recreate the old one...
    (if (and redo-search-flag mh-index-previous-search)
        (let ((buffer-name (buffer-name (current-buffer))))
          (mh-process-or-undo-commands buffer-name)
          (save-excursion (mh-exec-cmd-quiet nil "rmf" buffer-name))
          (mh-exec-cmd-quiet nil "folder" "-create" "-fast" buffer-name)
          (setq index-folder buffer-name))
      (setq index-folder (mh-index-new-folder index-folder)))

    (let ((folder-path (format "%s%s" mh-user-path (substring folder 1)))
          (folder-results-map (make-hash-table :test #'equal))
          (origin-map (make-hash-table :test #'equal)))
      ;; Run search program...
      (message "Executing %s... " mh-indexer)
      (funcall mh-index-execute-search-function folder-path search-regexp)

      ;; Parse indexer output
      (message "Processing %s output... " mh-indexer)
      (goto-char (point-min))
      (loop for next-result = (funcall mh-index-next-result-function)
            while next-result
            do (unless (eq next-result 'error)
                 (unless (gethash (car next-result) folder-results-map)
                   (setf (gethash (car next-result) folder-results-map)
                         (make-hash-table :test #'equal)))
                 (setf (gethash (cadr next-result)
                                (gethash (car next-result) folder-results-map))
                       t)))

      ;; Copy the search results over
      (maphash #'(lambda (folder msgs)
                   (let ((msgs (sort (loop for msg being the hash-keys of msgs
                                           collect msg)
                                     #'<)))
                     (mh-exec-cmd "refile" msgs "-src" folder
                                  "-link" index-folder)
                     (loop for msg in msgs
                           do (incf result-count)
                           (setf (gethash result-count origin-map)
                                 (cons folder msg)))))
               folder-results-map)

      ;; Mark messages as unseen (if needed)
      (when (and unseen-flag (> result-count 0))
        (mh-exec-cmd "mark" index-folder "all"
                     "-sequence" (symbol-name mh-unseen-seq) "-add"))

      ;; Generate scan lines for the hits.
      (mh-visit-folder index-folder () (list folder-results-map origin-map))

      (goto-char (point-min))
      (forward-line)
      (mh-update-sequences)
      (mh-recenter nil)

      ;; Maintain history
      (when (or (and redo-search-flag previous-search) window-config)
        (setq mh-previous-window-config old-window-config))
      (setq mh-index-previous-search (list folder search-regexp))

      (message "%s found %s matches in %s folders"
               (upcase-initials (symbol-name mh-indexer))
               (loop for msg-hash being hash-values of mh-index-data
                     sum (hash-table-count msg-hash))
               (loop for msg-hash being hash-values of mh-index-data
                     count (> (hash-table-count msg-hash) 0))))))

;;;###mh-autoload
(defun mh-index-do-search ()
  "Construct appropriate regexp and call `mh-index-search'."
  (interactive)
  (unless (mh-index-choose) (error "No indexing program found"))
  (let* ((regexp-list (mh-pick-parse-search-buffer))
         (pattern (funcall mh-index-regexp-builder regexp-list)))
    (if pattern
        (mh-index-search nil mh-current-folder pattern
                         mh-previous-window-config)
      (error "No search terms"))))

(defun mh-replace-string (old new)
  "Replace all occurrences of OLD with NEW in the current buffer."
  (goto-char (point-min))
  (while (search-forward old nil t)
    (replace-match new)))

;;;###mh-autoload
(defun mh-index-parse-search-regexp (input-string)
  "Construct parse tree for INPUT-STRING.
All occurrences of &, |, ! and ~ in INPUT-STRING are replaced by AND, OR and
NOT as appropriate. Then the resulting string is parsed."
  (let (input)
    (with-temp-buffer
      (insert input-string)
      (downcase-region (point-min) (point-max))
      ;; replace tabs
      (mh-replace-string "\t" " ")
      ;; synonyms of AND
      (mh-replace-string "&" " and ")
      (mh-replace-string " -and " " and ")
      ;; synonyms of OR
      (mh-replace-string "|" " or ")
      (mh-replace-string " -or " " or ")
      ;; synonyms of NOT
      (mh-replace-string "!" " not ")
      (mh-replace-string "~" " not ")
      (mh-replace-string " -not " " not ")
      ;; synonyms of left brace
      (mh-replace-string "(" " ( ")
      (mh-replace-string " -lbrace " " ( ")
      ;; synonyms of right brace
      (mh-replace-string ")" " ) ")
      (mh-replace-string " -rbrace " " ) ")
      ;; get the normalized input
      (setq input (format "( %s )" (buffer-substring (point-min) (point-max)))))

    (let ((tokens (mh-index-add-implicit-ops (split-string input)))
          (op-stack ())
          (operand-stack ())
          oper1)
      (dolist (token tokens)
        (cond ((equal token "(") (push 'paren op-stack))
              ((equal token "not") (push 'not op-stack))
              ((equal token "or") (push 'or op-stack))
              ((equal token "and") (push 'and op-stack))
              ((equal token ")")
               (multiple-value-setq (op-stack operand-stack)
                 (mh-index-evaluate op-stack operand-stack))
               (when (eq (car op-stack) 'not)
                 (pop op-stack)
                 (push `(not ,(pop operand-stack)) operand-stack))
               (when (eq (car op-stack) 'and)
                 (pop op-stack)
                 (setq oper1 (pop operand-stack))
                 (push `(and ,(pop operand-stack) ,oper1) operand-stack)))
              ((eq (car op-stack) 'not)
               (pop op-stack)
               (push `(not ,token) operand-stack)
               (when (eq (car op-stack) 'and)
                 (pop op-stack)
                 (setq oper1 (pop operand-stack))
                 (push `(and ,(pop operand-stack) ,oper1) operand-stack)))
              ((eq (car op-stack) 'and)
               (pop op-stack)
               (push `(and ,(pop operand-stack) ,token) operand-stack))
              (t (push token operand-stack))))
      (prog1 (pop operand-stack)
        (when (or op-stack operand-stack)
          (error "Invalid regexp: %s" input))))))

(defun mh-index-add-implicit-ops (tokens)
  "Add implicit operators in the list TOKENS."
  (let ((result ())
        (literal-seen nil)
        current)
    (while tokens
      (setq current (pop tokens))
      (cond ((or (equal current ")") (equal current "and") (equal current "or"))
             (setq literal-seen nil)
             (push current result))
            ((and literal-seen
                  (push "and" result)
                  (setq literal-seen nil)
                  nil))
            (t
             (push current result)
             (unless (or (equal current "(") (equal current "not"))
               (setq literal-seen t)))))
    (nreverse result)))

(defun mh-index-evaluate (op-stack operand-stack)
  "Read expression till starting paren based on OP-STACK and OPERAND-STACK."
  (block mh-index-evaluate
    (let (op oper1)
      (while op-stack
        (setq op (pop op-stack))
        (cond ((eq op 'paren)
               (return-from mh-index-evaluate (values op-stack operand-stack)))
              ((eq op 'not)
               (push `(not ,(pop operand-stack)) operand-stack))
              ((or (eq op 'and) (eq op 'or))
               (setq oper1 (pop operand-stack))
               (push `(,op ,(pop operand-stack) ,oper1) operand-stack))))
      (error "Ran out of tokens"))))

;;;###mh-autoload
(defun mh-index-next-folder (&optional backward-flag)
  "Jump to the next folder marker.
The function is only applicable to folders displaying index search results.
With non-nil optional argument BACKWARD-FLAG, jump to the previous group of
results."
  (interactive "P")
  (if (null mh-index-data)
      (message "Only applicable in an MH-E index search buffer")
    (let ((point (point)))
      (forward-line (if backward-flag -1 1))
      (cond ((if backward-flag
                 (re-search-backward "^+" (point-min) t)
               (re-search-forward "^+" (point-max) t))
             (beginning-of-line))
            ((and (if backward-flag
                      (goto-char (point-max))
                    (goto-char (point-min)))
                  nil))
            ((if backward-flag
                 (re-search-backward "^+" (point-min) t)
               (re-search-forward "^+" (point-max) t))
             (beginning-of-line))
            (t (goto-char point))))))

;;;###mh-autoload
(defun mh-index-previous-folder ()
  "Jump to the previous folder marker."
  (interactive)
  (mh-index-next-folder t))

(defun mh-folder-exists-p (folder)
  "Check if FOLDER exists."
  (and (mh-folder-name-p folder)
       (save-excursion
         (with-temp-buffer
           (mh-exec-cmd-output "folder" nil "-fast" "-nocreate" folder)
           (goto-char (point-min))
           (not (eobp))))))

(defun mh-msg-exists-p (msg folder)
  "Check if MSG exists in FOLDER."
  (file-exists-p (format "%s%s/%s" mh-user-path (substring folder 1) msg)))

(defun mh-index-new-folder (name)
  "Create and return an MH folder name based on NAME.
If the folder NAME already exists then check if NAME<2> exists. If it doesn't
then it is created and returned. Otherwise try NAME<3>. This is repeated till
we find a new folder name."
  (unless (mh-folder-name-p name)
    (error "The argument should be a valid MH folder name"))
  (let ((chosen-name name))
    (block unique-name
      (unless (mh-folder-exists-p name)
        (return-from unique-name))
      (loop for index from 2
            do (let ((new-name (format "%s<%s>" name index)))
                 (unless (mh-folder-exists-p new-name)
                   (setq chosen-name new-name)
                   (return-from unique-name)))))
    (mh-exec-cmd-quiet nil "folder" "-create" "-fast" chosen-name)
    (mh-remove-from-sub-folders-cache chosen-name)
    (when (boundp 'mh-speed-folder-map)
      (mh-speed-add-folder chosen-name))
    chosen-name))

;;;###mh-autoload
(defun mh-index-insert-folder-headers ()
  "Annotate the search results with original folder names."
  (let ((cur-msg (mh-get-msg-num nil))
        (old-buffer-modified-flag (buffer-modified-p))
        (buffer-read-only nil)
        current-folder last-folder)
    (goto-char (point-min))
    (while (not (eobp))
      (setq current-folder (car (gethash (gethash (mh-get-msg-num nil)
                                                  mh-index-msg-checksum-map)
                                         mh-index-checksum-origin-map)))
      (when (and current-folder (not (eq current-folder last-folder)))
        (insert (if last-folder "\n" "") current-folder "\n")
        (setq last-folder current-folder))
      (forward-line))
    (when cur-msg (mh-goto-msg cur-msg t))
    (set-buffer-modified-p old-buffer-modified-flag)))

;;;###mh-autoload
(defun mh-index-group-by-folder ()
  "Partition the messages based on source folder.
Returns an alist with the the folder names in the car and the cdr being the
list of messages originally from that folder."
  (save-excursion
    (goto-char (point-min))
    (let ((result-table (make-hash-table)))
      (loop for msg being hash-keys of mh-index-msg-checksum-map
            do (push msg (gethash (car (gethash
                                        (gethash msg mh-index-msg-checksum-map)
                                        mh-index-checksum-origin-map))
                                  result-table)))
      (loop for x being the hash-keys of result-table
            collect (cons x (nreverse (gethash x result-table)))))))

;;;###mh-autoload
(defun mh-index-delete-folder-headers ()
  "Delete the folder headers."
  (let ((cur-msg (mh-get-msg-num nil))
        (old-buffer-modified-flag (buffer-modified-p))
        (buffer-read-only nil))
    (while (and (not cur-msg) (not (eobp)))
      (forward-line)
      (setq cur-msg (mh-get-msg-num nil)))
    (goto-char (point-min))
    (while (not (eobp))
      (if (or (char-equal (char-after) ?+) (char-equal (char-after) 10))
          (delete-region (point) (progn (forward-line) (point)))
        (forward-line)))
    (when cur-msg (mh-goto-msg cur-msg t t))
    (set-buffer-modified-p old-buffer-modified-flag)))

;;;###mh-autoload
(defun mh-index-visit-folder ()
  "Visit original folder from where the message at point was found."
  (interactive)
  (unless mh-index-data
    (error "Not in an index folder"))
  (let (folder msg)
    (save-excursion
      (cond ((and (bolp) (eolp))
             (ignore-errors (forward-line -1))
             (setq msg (mh-get-msg-num t)))
            ((equal (char-after (line-beginning-position)) ?+)
             (setq folder (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
            (t (setq msg (mh-get-msg-num t)))))
    (when (not folder)
      (setq folder (car (gethash (gethash msg mh-index-msg-checksum-map)
                                 mh-index-checksum-origin-map))))
    (when (or (not (get-buffer folder))
              (y-or-n-p (format "Reuse buffer displaying %s? " folder)))
      (mh-visit-folder
       folder (loop for x being the hash-keys of (gethash folder mh-index-data)
                    when (mh-msg-exists-p x folder) collect x)))))

;;;###mh-autoload
(defun mh-index-update-unseen (msg)
  "Remove counterpart of MSG in source folder from `mh-unseen-seq'.
Also `mh-update-unseen' is called in the original folder, if we have it open."
  (let* ((checksum (gethash msg mh-index-msg-checksum-map))
         (folder-msg-pair (gethash checksum mh-index-checksum-origin-map))
         (orig-folder (car folder-msg-pair))
         (orig-msg (cdr folder-msg-pair)))
    (when (mh-index-match-checksum orig-msg orig-folder checksum)
      (when (get-buffer orig-folder)
        (save-excursion
          (set-buffer orig-folder)
          (unless (member orig-msg mh-seen-list) (push orig-msg mh-seen-list))
          (mh-update-unseen)))
      (mh-exec-cmd-daemon "mark" #'ignore orig-folder (format "%s" orig-msg)
                          "-sequence" (symbol-name mh-unseen-seq) "-del"))))

(defun mh-index-match-checksum (msg folder checksum)
  "Check if MSG in FOLDER has X-MHE-Checksum header value of CHECKSUM."
  (with-temp-buffer
    (mh-exec-cmd-output mh-scan-prog nil "-width" "80"
                        "-format" "%{x-mhe-checksum}\n" folder msg)
    (goto-char (point-min))
    (string-equal (buffer-substring-no-properties (point) (line-end-position))
                  checksum)))

;;;###mh-autoload
(defun mh-index-execute-commands ()
  "Delete/refile the actual messages.
The copies in the searched folder are then deleted/refiled to get the desired
result. Before deleting the messages we make sure that the message being
deleted is identical to the one that the user has marked in the index buffer."
  (let ((message-table (make-hash-table :test #'equal)))
    (dolist (msg-list (cons mh-delete-list (mapcar #'cdr mh-refile-list)))
      (dolist (msg msg-list)
        (let* ((checksum (gethash msg mh-index-msg-checksum-map))
               (pair (gethash checksum mh-index-checksum-origin-map)))
          (when (and checksum (car pair) (cdr pair)
                     (mh-index-match-checksum (cdr pair) (car pair) checksum))
            (push (cdr pair) (gethash (car pair) message-table))
            (remhash (cdr pair) (gethash (car pair) mh-index-data))))))
    (maphash (lambda (folder msgs)
               (apply #'mh-exec-cmd "rmm" folder (mh-coalesce-msg-list msgs)))
             message-table)))



;; Glimpse interface

(defvar mh-glimpse-binary (executable-find "glimpse"))
(defvar mh-glimpse-directory ".glimpse")

;;;###mh-autoload
(defun mh-glimpse-execute-search (folder-path search-regexp)
  "Execute glimpse and read the results.

In the examples below, replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.glimpse. Then create the file
/home/user/Mail/.glimpse/.glimpse_exclude with the following contents:

    */.*
    */#*
    */,*
    */*~
    ^/home/user/Mail/.glimpse
    ^/home/user/Mail/mhe-index

If there are any directories you would like to ignore, append lines like the
following to .glimpse_exclude:

    ^/home/user/Mail/scripts

You do not want to index the folders that hold the results of your searches
since they tend to be ephemeral and the original messages are indexed anyway.
The configuration file above assumes that the results are found in sub-folders
of `mh-index-folder' which is +mhe-index by default.

Use the following command line to generate the glimpse index. Run this
daily from cron:

    glimpseindex -H /home/user/Mail/.glimpse /home/user/Mail

FOLDER-PATH is the directory in which SEARCH-REGEXP is used to search."
  (set-buffer (get-buffer-create mh-index-temp-buffer))
  (erase-buffer)
  (call-process mh-glimpse-binary nil '(t nil) nil
                                        ;(format "-%s" fuzz)
                "-i" "-y"
                "-H" (format "%s%s" mh-user-path mh-glimpse-directory)
                "-F" (format "^%s" folder-path)
                search-regexp)
  (goto-char (point-min)))

(defun mh-glimpse-next-result ()
  "Read the next result.
Parse it and return the message folder, message index and the match. If no
other matches left then return nil. If the current record is invalid return
'error."
  (prog1
      (block nil
        (when (eobp)
          (return nil))
        (let ((eol-pos (line-end-position))
              (bol-pos (line-beginning-position))
              folder-start msg-end)
          (goto-char bol-pos)
          (unless (search-forward mh-user-path eol-pos t)
            (return 'error))
          (setq folder-start (point))
          (unless (search-forward ": " eol-pos t)
            (return 'error))
          (let ((match (buffer-substring-no-properties (point) eol-pos)))
            (forward-char -2)
            (setq msg-end (point))
            (unless (search-backward "/" folder-start t)
              (return 'error))
            (list (format "+%s" (buffer-substring-no-properties
                                 folder-start (point)))
                  (let ((val (ignore-errors (read-from-string
                                             (buffer-substring-no-properties
                                              (1+ (point)) msg-end)))))
                    (if (and (consp val) (integerp (car val)))
                        (car val)
                      (return 'error)))
                  match))))
    (forward-line)))



;; Pick interface

(defvar mh-index-pick-folder)
(defvar mh-pick-binary "pick")

(defun mh-pick-execute-search (folder-path search-regexp)
  "Execute pick.

Unlike the other index search programs \"pick\" only searches messages present
in the folder itself and does not descend into any sub-folders that may be
present.

FOLDER-PATH is the directory containing the mails to be searched and
SEARCH-REGEXP is the pattern that pick gets."
  (set-buffer (get-buffer-create mh-index-temp-buffer))
  (erase-buffer)
  (setq mh-index-pick-folder
        (concat "+" (substring folder-path (length mh-user-path))))
  (apply #'call-process (expand-file-name "pick" mh-progs) nil '(t nil) nil
         mh-index-pick-folder "-list" search-regexp)
  (goto-char (point-min)))

(defun mh-pick-next-result ()
  "Return the next pick search result."
  (prog1 (block nil
           (when (eobp) (return nil))
           (unless (re-search-forward "^[1-9][0-9]*$" (line-end-position) t)
             (return 'error))
           (list mh-index-pick-folder
                 (car (read-from-string (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (line-end-position))))
                 nil))
    (forward-line)))



;; Grep interface

(defvar mh-grep-binary (executable-find "grep"))

(defun mh-grep-execute-search (folder-path search-regexp)
  "Execute grep and read the results.
FOLDER-PATH is the directory in which SEARCH-REGEXP is used to search."
  (set-buffer (get-buffer-create mh-index-temp-buffer))
  (erase-buffer)
  (call-process mh-grep-binary nil '(t nil) nil
                "-i" "-r" search-regexp folder-path)
  (goto-char (point-min)))

(defun mh-grep-next-result ()
  "Read the next result.
Parse it and return the message folder, message index and the match. If no
other matches left then return nil. If the current record is invalid return
'error."
  (prog1
      (block nil
        (when (eobp)
          (return nil))
        (let ((eol-pos (line-end-position))
              (bol-pos (line-beginning-position))
              folder-start msg-end)
          (goto-char bol-pos)
          (unless (search-forward mh-user-path eol-pos t)
            (return 'error))
          (setq folder-start (point))
          (unless (search-forward ":" eol-pos t)
            (return 'error))
          (let ((match (buffer-substring-no-properties (point) eol-pos)))
            (forward-char -1)
            (setq msg-end (point))
            (unless (search-backward "/" folder-start t)
              (return 'error))
            (list (format "+%s" (buffer-substring-no-properties
                                 folder-start (point)))
                  (let ((val (ignore-errors (read-from-string
                                             (buffer-substring-no-properties
                                              (1+ (point)) msg-end)))))
                    (if (and (consp val) (integerp (car val)))
                        (car val)
                      (return 'error)))
                  match))))
    (forward-line)))



;; Mairix interface

(defvar mh-mairix-binary (executable-find "mairix"))
(defvar mh-mairix-directory ".mairix")
(defvar mh-mairix-folder nil)

(defun mh-mairix-execute-search (folder-path search-regexp-list)
  "Execute mairix and read the results.

In the examples below replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.mairix. Then create the file
/home/user/Mail/.mairix/config with the following contents:

   # This should contain the same thing as your `mh-user-path'
   base=/home/user/Mail

   # List of folders that should be indexed. 3 dots at the end means there are
   # subfolders within the folder
   mh_folders=archive...:inbox:drafts:news:sent:trash

   vfolder_format=raw
   database=/home/user/Mail/mairix/database

Use the following command line to generate the mairix index. Run this daily
from cron:

    mairix -f /home/user/Mail/.mairix/config

FOLDER-PATH is the directory in which SEARCH-REGEXP-LIST is used to search."
  (set-buffer (get-buffer-create mh-index-temp-buffer))
  (erase-buffer)
  (unless mh-mairix-binary
    (error "Set mh-mairix-binary appropriately"))
  (apply #'call-process mh-mairix-binary nil '(t nil) nil
         "-f" (format "%s%s/config" mh-user-path mh-mairix-directory)
         search-regexp-list)
  (goto-char (point-min))
  (setq mh-mairix-folder
        (let ((last-char (substring folder-path (1- (length folder-path)))))
          (if (equal last-char "/")
              folder-path
            (format "%s/" folder-path)))))

(defun mh-mairix-next-result ()
  "Return next result from mairix output."
  (prog1
      (block nil
        (when (or (eobp) (and (bolp) (eolp)))
          (return nil))
        (unless (eq (char-after) ?/)
          (return 'error))
        (let ((start (point))
              end msg-start)
          (setq end (line-end-position))
          (unless (search-forward mh-mairix-folder end t)
            (return 'error))
          (goto-char (match-beginning 0))
          (unless (equal (point) start)
            (return 'error))
          (goto-char end)
          (unless (search-backward "/" start t)
            (return 'error))
          (setq msg-start (1+ (point)))
          (goto-char start)
          (unless (search-forward mh-user-path end t)
            (return 'error))
          (list (format "+%s" (buffer-substring-no-properties
                               (point) (1- msg-start)))
                (car (read-from-string
                      (buffer-substring-no-properties msg-start end)))
                ())))
    (forward-line)))

(defun mh-mairix-regexp-builder (regexp-list)
  "Generate query for mairix.
REGEXP-LIST is an alist of fields and values."
  (let ((result ()))
    (dolist (pair regexp-list)
      (when (cdr pair)
        (push
         (concat
          (cond ((eq (car pair) 'to) "t:")
                ((eq (car pair) 'from) "f:")
                ((eq (car pair) 'cc) "c:")
                ((eq (car pair) 'subject) "s:")
                ((eq (car pair) 'date) "d:")
                (t ""))
          (let ((sop (cdr (mh-mairix-convert-to-sop* (cdr pair))))
                (final ""))
            (dolist (conjunct sop)
              (let ((expr-list (cdr conjunct))
                    (expr-string ""))
                (dolist (e expr-list)
                  (setq expr-string (concat expr-string "+"
                                            (if (atom e) "" "~")
                                            (if (atom e) e (cadr e)))))
                (setq final (concat final "," (substring expr-string 1)))))
            (substring final 1)))
         result)))
    result))

(defun mh-mairix-convert-to-sop* (expr)
  "Convert EXPR to sum of product form."
  (cond ((atom expr) `(or (and ,expr)))
        ((eq (car expr) 'or)
         (cons 'or
               (loop for e in (mapcar #'mh-mairix-convert-to-sop* (cdr expr))
                     append (cdr e))))
        ((eq (car expr) 'and)
         (let ((conjuncts (mapcar #'mh-mairix-convert-to-sop* (cdr expr)))
               result next-factor)
           (setq result (pop conjuncts))
           (while conjuncts
             (setq next-factor (pop conjuncts))
             (setq result (let ((res ()))
                            (dolist (t1 (cdr result))
                              (dolist (t2 (cdr next-factor))
                                (push `(and ,@(cdr t1) ,@(cdr t2)) res)))
                            (cons 'or res))))
           result))
        ((atom (cadr expr)) `(or (and ,expr)))
        ((eq (caadr expr) 'not) (mh-mairix-convert-to-sop* (cadadr expr)))
        ((eq (caadr expr) 'and) (mh-mairix-convert-to-sop*
                                 `(or ,@(mapcar #'(lambda (x) `(not ,x))
                                                (cdadr expr)))))
        ((eq (caadr expr) 'or) (mh-mairix-convert-to-sop*
                                `(and ,@(mapcar #'(lambda (x) `(not ,x))
                                                (cdadr expr)))))
        (t (error "Unreachable: %s" expr))))



;; Interface to unseen messages script

(defvar mh-flists-search-folders)

(defun mh-flists-execute (&rest args)
  "Search for unseen messages in `mh-flists-search-folders'.
If `mh-recursive-folders-flag' is t, then the folders are searched
recursively. All parameters ARGS are ignored."
  (set-buffer (get-buffer-create mh-index-temp-buffer))
  (erase-buffer)
  (unless (executable-find "sh")
    (error "Didn't find sh"))
  (with-temp-buffer
    (let ((unseen (symbol-name mh-unseen-seq)))
      (insert "for folder in `flists "
              (cond ((eq mh-flists-search-folders t) mh-inbox)
                    ((eq mh-flists-search-folders nil) "")
                    ((listp mh-flists-search-folders)
                     (loop for folder in mh-flists-search-folders
                           concat (concat " " folder))))
              (if mh-recursive-folders-flag " -recurse" "")
              " -sequence " unseen " -noshowzero -fast` ; do\n"
              "mhpath \"+$folder\" " unseen "\n" "done\n"))
    (call-process-region
     (point-min) (point-max) "sh" nil (get-buffer mh-index-temp-buffer))))

;;;###mh-autoload
(defun mh-index-new-messages (folders)
  "Display new messages.
All messages in the `mh-unseen-seq' sequence from FOLDERS are displayed.
By default the folders specified by `mh-index-new-messages-folders' are
searched. With a prefix argument, enter a space-separated list of folders, or
nothing to search all folders."
  (interactive
   (list (if current-prefix-arg
             (split-string (read-string "Folders to search: "))
           mh-index-new-messages-folders)))
  (let* ((mh-flists-search-folders folders)
         (mh-indexer 'flists)
         (mh-index-execute-search-function 'mh-flists-execute)
         (mh-index-next-result-function 'mh-mairix-next-result)
         (mh-mairix-folder mh-user-path)
         (mh-index-regexp-builder nil)
         (new-folder (format "%s/%s" mh-index-folder mh-flists-results-folder))
         (window-config (if (equal new-folder mh-current-folder)
                            mh-previous-window-config
                          (current-window-configuration)))
         (redo-flag nil))
    (cond ((buffer-live-p (get-buffer new-folder))
           ;; The destination folder is being visited. Trick `mh-index-search'
           ;; into thinking that the folder was the result of a previous search.
           (set-buffer new-folder)
           (setq mh-index-previous-search (list "+" mh-flists-results-folder))
           (setq redo-flag t))
          ((mh-folder-exists-p new-folder)
           ;; Folder exists but we don't have it open. That means they are
           ;; stale results from a old flists search. Clear it out.
           (mh-exec-cmd-quiet nil "rmf" new-folder)))
    (mh-index-search redo-flag "+" mh-flists-results-folder window-config t)))



;; Swish interface

(defvar mh-swish-binary (executable-find "swish-e"))
(defvar mh-swish-directory ".swish")
(defvar mh-swish-folder nil)

;;;###mh-autoload
(defun mh-swish-execute-search (folder-path search-regexp)
  "Execute swish-e and read the results.

In the examples below, replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.swish. Then create the file
/home/user/Mail/.swish/config with the following contents:

    IndexDir /home/user/Mail
    IndexFile /home/user/Mail/.swish/index
    IndexName \"Mail Index\"
    IndexDescription \"Mail Index\"
    IndexPointer \"http://nowhere\"
    IndexAdmin \"nobody\"
    #MetaNames automatic
    IndexReport 3
    FollowSymLinks no
    UseStemming no
    IgnoreTotalWordCountWhenRanking yes
    WordCharacters abcdefghijklmnopqrstuvwxyz0123456789-
    BeginCharacters abcdefghijklmnopqrstuvwxyz
    EndCharacters abcdefghijklmnopqrstuvwxyz0123456789
    IgnoreLimit 50 1000
    IndexComments 0
    FileRules pathname contains /home/user/Mail/.swish
    FileRules pathname contains /home/user/Mail/mhe-index
    FileRules filename is index
    FileRules filename is \\..*
    FileRules filename is #.*
    FileRules filename is ,.*
    FileRules filename is .*~

If there are any directories you would like to ignore, append lines like the
following to config:

    FileRules pathname contains /home/user/Mail/scripts

You do not want to index the folders that hold the results of your searches
since they tend to be ephemeral and the original messages are indexed anyway.
The configuration file above assumes that the results are found in sub-folders
of `mh-index-folder' which is +mhe-index by default.

Use the following command line to generate the swish index. Run this
daily from cron:

    swish-e -c /home/user/Mail/.swish/config

FOLDER-PATH is the directory in which SEARCH-REGEXP is used to search."
  (set-buffer (get-buffer-create mh-index-temp-buffer))
  (erase-buffer)
  (unless mh-swish-binary
    (error "Set mh-swish-binary appropriately"))
  (call-process mh-swish-binary nil '(t nil) nil
                "-w" search-regexp
                "-f" (format "%s%s/index" mh-user-path mh-swish-directory))
  (goto-char (point-min))
  (setq mh-swish-folder
        (let ((last-char (substring folder-path (1- (length folder-path)))))
          (if (equal last-char "/")
              folder-path
            (format "%s/" folder-path)))))

(defun mh-swish-next-result ()
  "Get the next result from swish output."
  (prog1
      (block nil
        (when (or (eobp) (equal (char-after (point)) ?.))
          (return nil))
        (when (equal (char-after (point)) ?#)
          (return 'error))
        (let* ((start (search-forward " " (line-end-position) t))
               (end (search-forward " " (line-end-position) t)))
          (unless (and start end)
            (return 'error))
          (setq end (1- end))
          (unless (file-exists-p (buffer-substring-no-properties start end))
            (return 'error))
          (unless (search-backward "/" start t)
            (return 'error))
          (list (let* ((s (buffer-substring-no-properties start (1+ (point)))))
                  (unless (string-match mh-swish-folder s)
                    (return 'error))
                  (if (string-match mh-user-path s)
                      (format "+%s"
                              (substring s (match-end 0) (1- (length s))))
                    (return 'error)))
                (let* ((s (buffer-substring-no-properties (1+ (point)) end))
                       (val (ignore-errors (read-from-string s))))
                  (if (and (consp val) (numberp (car val)))
                      (car val)
                    (return 'error)))
                nil)))
    (forward-line)))



;; Swish++ interface

(defvar mh-swish++-binary (or (executable-find "search++")
                              (executable-find "search")))
(defvar mh-swish++-directory ".swish++")

;;;###mh-autoload
(defun mh-swish++-execute-search (folder-path search-regexp)
  "Execute swish++ and read the results.

In the examples below, replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.swish++. Then create the file
/home/user/Mail/.swish++/swish++.conf with the following contents:

    IncludeMeta         Bcc Cc Comments Content-Description From Keywords
    IncludeMeta         Newsgroups Resent-To Subject To
    IncludeMeta         Message-Id References In-Reply-To
    IncludeFile         Mail    *
    IndexFile           /home/user/Mail/.swish++/swish++.index

Use the following command line to generate the swish index. Run this
daily from cron:

 find /home/user/Mail -path /home/user/Mail/mhe-index -prune \\
                   -o -path /home/user/Mail/.swish++ -prune \\
                   -o -name \"[0-9]*\" -print \\
    | index -c /home/user/Mail/.swish++/swish++.conf /home/user/Mail

You do not want to index the folders that hold the results of your searches
since they tend to be ephemeral and the original messages are indexed anyway.
The command above assumes that the results are found in sub-folders of
`mh-index-folder' which is +mhe-index by default.

On some systems (Debian GNU/Linux, for example), use index++ instead of index.

FOLDER-PATH is the directory in which SEARCH-REGEXP is used to search."
  (set-buffer (get-buffer-create mh-index-temp-buffer))
  (erase-buffer)
  (unless mh-swish++-binary
    (error "Set mh-swish++-binary appropriately"))
  (call-process mh-swish++-binary nil '(t nil) nil
                "-m" "10000"
                (format "-i%s%s/swish++.index"
                        mh-user-path mh-swish++-directory)
                search-regexp)
  (goto-char (point-min))
  (setq mh-swish-folder
        (let ((last-char (substring folder-path (1- (length folder-path)))))
          (if (equal last-char "/")
              folder-path
            (format "%s/" folder-path)))))

(defalias 'mh-swish++-next-result 'mh-swish-next-result)

(defun mh-swish++-regexp-builder (regexp-list)
  "Generate query for swish++.
REGEXP-LIST is an alist of fields and values."
  (let ((regexp ""))
    (dolist (elem regexp-list)
      (when (cdr elem)
        (setq regexp (concat regexp " and "
                             (if (car elem) "(" "")
                             (if (car elem) (symbol-name (car elem)) "")
                             (if (car elem) " = " "")
                             (mh-swish++-print-regexp (cdr elem))
                             (if (car elem) ")" "")))))
    (substring regexp 4)))

(defun mh-swish++-print-regexp (expr)
  "Return infix expression corresponding to EXPR."
  (cond ((atom expr) (format "%s" expr))
        ((eq (car expr) 'not)
         (format "(not %s)" (mh-swish++-print-regexp (cadr expr))))
        (t (format "(%s %s %s)" (mh-swish++-print-regexp (cadr expr))
                   (symbol-name (car expr))
                   (mh-swish++-print-regexp (caddr expr))))))



;; Namazu interface

(defvar mh-namazu-binary (executable-find "namazu"))
(defvar mh-namazu-directory ".namazu")
(defvar mh-namazu-folder nil)

;;;###mh-autoload
(defun mh-namazu-execute-search (folder-path search-regexp)
  "Execute namazu and read the results.

In the examples below, replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.namazu. Then create the file
/home/user/Mail/.namazu/mknmzrc with the following contents:

    package conf;  # Don't remove this line!
    $ADDRESS = 'user@localhost';
    $ALLOW_FILE = \"[0-9]*\";
    $EXCLUDE_PATH = \"^/home/user/Mail/(mhe-index|spam)\";

In the above example configuration, none of the mail files contained in the
directories /home/user/Mail/mhe-index and /home/user/Mail/spam are indexed.

You do not want to index the folders that hold the results of your searches
since they tend to be ephemeral and the original messages are indexed anyway.
The configuration file above assumes that the results are found in sub-folders
of `mh-index-folder' which is +mhe-index by default.

Use the following command line to generate the namazu index. Run this
daily from cron:

   mknmz -f /home/user/Mail/.namazu/mknmzrc -O /home/user/Mail/.namazu \\
         /home/user/Mail

FOLDER-PATH is the directory in which SEARCH-REGEXP is used to search."
  (let ((namazu-index-directory
         (format "%s%s" mh-user-path mh-namazu-directory)))
    (unless (file-exists-p namazu-index-directory)
      (error "Namazu directory %s not present" namazu-index-directory))
    (unless (executable-find mh-namazu-binary)
      (error "Set mh-namazu-binary appropriately"))
    (set-buffer (get-buffer-create mh-index-temp-buffer))
    (erase-buffer)
    (call-process mh-namazu-binary nil '(t nil) nil
                  "-alR" search-regexp namazu-index-directory)
    (goto-char (point-min))
    (setq mh-namazu-folder
          (let ((last (substring folder-path (1- (length folder-path)))))
            (if (equal last "/")
                folder-path
              (format "%s/" folder-path))))))

(defun mh-namazu-next-result ()
  "Get the next result from namazu output."
  (prog1
      (block nil
        (when (eobp) (return nil))
        (let ((file-name (buffer-substring-no-properties
                          (point) (line-end-position))))
          (unless (equal (string-match mh-namazu-folder file-name) 0)
            (return 'error))
          (unless (file-exists-p file-name)
            (return 'error))
          (string-match mh-user-path file-name)
          (let* ((folder/msg (substring file-name (match-end 0)))
                 (mark (mh-search-from-end ?/ folder/msg)))
            (unless mark (return 'error))
            (list (format "+%s" (substring folder/msg 0 mark))
                  (let ((n (ignore-errors (read-from-string
                                           (substring folder/msg (1+ mark))))))
                    (if (and (consp n) (numberp (car n)))
                        (car n)
                      (return 'error)))
                  nil))))
    (forward-line)))



;;;###mh-autoload
(defun mh-index-choose ()
  "Choose an indexing function.
The side-effects of this function are that the variables `mh-indexer',
`mh-index-execute-search-function', and `mh-index-next-result-function' are
set according to the first indexer in `mh-indexer-choices' present on the
system."
  (block nil
    ;; The following favors the user's preference; otherwise, the last
    ;; automatically chosen indexer is used for efficiency rather than going
    ;; through the list.
    (let ((program-alist (cond (mh-index-program
                                (list
                                 (assoc mh-index-program mh-indexer-choices)))
                               (mh-indexer
                                (list (assoc mh-indexer mh-indexer-choices)))
                               (t mh-indexer-choices))))
      (while program-alist
        (let* ((current (pop program-alist))
               (executable (symbol-value (cadr current))))
          (when executable
            (setq mh-indexer (car current))
            (setq mh-index-execute-search-function (nth 2 current))
            (setq mh-index-next-result-function (nth 3 current))
            (setq mh-index-regexp-builder (nth 4 current))
            (return mh-indexer))))
      nil)))



(provide 'mh-index)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 607762ad-0dff-4fe1-a27e-6c0dde0dcc47
;;; mh-index ends here
