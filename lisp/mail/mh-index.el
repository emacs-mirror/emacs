;;; mh-index  --  MH-E interface to indexing programs

;; Copyright (C) 2002 Free Software Foundation, Inc.

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
;;;        namazu
;;;        glimpse
;;;        grep
;;;      
;;;  (2) To use this package, you first have to build an index. Please read
;;;      the documentation for `mh-index-search' to get started. That
;;;      documentation will direct you to the specific instructions for your
;;;      particular indexer.
;;;
;;;  (3) Right now only viewing messages and moving between messages works in
;;;      the index buffer. With a little bit of work more stuff like
;;;      replying or forwarding messages can be done.

;;; Change Log:

;; $Id: mh-index.el,v 1.51 2002/11/13 18:43:57 satyaki Exp $

;;; Code:

(require 'cl)
(require 'mh-e)
(require 'mh-mime)

;; Shush the byte-compiler
(defvar font-lock-defaults)

(autoload 'gnus-local-map-property "gnus-util")
(autoload 'gnus-eval-format "gnus-spec")
(autoload 'widget-convert-button "wid-edit")
(autoload 'executable-find "executable")

;;; User customizable
(defcustom mh-index-program nil
  "Indexing program that MH-E shall use.
The possible choices are swish++, swish-e, namazu, glimpse and grep. By
default this variable is nil which means that the programs are tried in order
and the first one found is used."
  :group 'mh
  :type '(choice (const :tag "auto-detect" nil)
                 (const :tag "swish++" swish++)
                 (const :tag "swish-e" swish)
                 (const :tag "namazu" namazu)
                 (const :tag "glimpse" glimpse)
                 (const :tag "grep" grep)))

;;; Hooks
(defcustom mh-index-show-hook nil
  "Invoked after the message has been displayed."
  :type 'hook
  :group 'mh-hook)

;; Support different indexing programs
(defvar mh-indexer-choices
  '((swish++
     mh-swish++-binary mh-swish++-execute-search mh-swish++-next-result)
    (swish
     mh-swish-binary mh-swish-execute-search mh-swish-next-result)
    (namazu
     mh-namazu-binary mh-namazu-execute-search mh-namazu-next-result)
    (glimpse
     mh-glimpse-binary mh-glimpse-execute-search mh-glimpse-next-result)
    (grep
     mh-grep-binary mh-grep-execute-search mh-grep-next-result))
  "List of possible indexer choices.")
(defvar mh-indexer nil
  "Chosen index program.")
(defvar mh-index-execute-search-function nil
  "Function which executes the search program.")
(defvar mh-index-next-result-function nil
  "Function to parse the next line of output.")

;; Names for the default mh-index-buffers...
(defvar mh-index-buffer "*mh-index*")
(defvar mh-index-show-buffer "*mh-index-show*")

;; For use with adaptive size setting...
(defvar mh-index-max-msg-index 0)

;; Buffer locals to allow multiple concurrent search folders.
(defvar mh-index-other-buffer nil
  "Keeps track of other buffer associated with current buffer.
The value is the show buffer or the folder-buffer depending on whether we are
in a folder buffer or show buffer respectively.")
(defvar mh-index-matches nil
  "Map of folder to messages which match.")
(defvar mh-index-previous-window-configuration nil
  "Keep track of previous window configuration that is restored on exit.")
(defvar mh-index-current-msg nil
  "Message index of message being shown.")

;; Make variables buffer local ...
(make-variable-buffer-local 'mh-index-other-buffer)
(make-variable-buffer-local 'mh-index-matches)
(make-variable-buffer-local 'mh-index-previous-window-configuration)
(make-variable-buffer-local 'mh-current-folder)
(make-variable-buffer-local 'mh-index-current-msg)

;; ... and arrange for them to not get slaughtered by a call to text-mode
;; (text-mode is called by mh-show-mode and mh-folder-mode).
(put 'mh-index-other-buffer 'permanent-local t)
(put 'mh-index-matches 'permanent-local t)
(put 'mh-index-previous-window-configuration 'permanent-local t)
(put 'mh-index-current-msg 'permanent-local t)
(put 'mh-current-folder 'permanent-local t)
(put 'mh-cmd-note 'permanent-local t)

;; Temporary buffer where search results are output.
(defvar mh-index-temp-buffer " *mh-index-temp*")

;; Keymaps

;; N.B. If this map were named mh-index-folder-mode-map, it would inherit the
;; keymap from mh-folder-mode. Since we want our own keymap, we tweak the name
;; to avoid this unwanted inheritance.
(defvar mh-index-folder-mode-keymap (make-sparse-keymap)
  "Keymap for MH index folder.")
(suppress-keymap mh-index-folder-mode-keymap)
(gnus-define-keys mh-index-folder-mode-keymap
  " "		mh-index-page-msg
  ","		mh-index-header-display
  "."		mh-index-show
  [mouse-2]     mh-index-show
  "?"		mh-help
  "\177"	mh-index-previous-page
  "\M-\t"	mh-index-prev-button
  [backtab]	mh-index-prev-button
  "\r"		mh-index-show
  "\t"		mh-index-next-button
  "i"		mh-inc-folder
  "m"		mh-send			;alias
  "n"		mh-index-next
  "p"		mh-index-prev
  "q"		mh-index-quit
  "s"		mh-send)

(gnus-define-keys (mh-index-folder-map "F" mh-index-folder-mode-keymap)
  "?"           mh-prefix-help
  "f"		mh-visit-folder		;alias
  "i"		mh-index-search-again
  "o"		mh-visit-folder		;alias
  "v"		mh-visit-folder)

(defvar mh-index-button-map (make-sparse-keymap))
(gnus-define-keys mh-index-button-map
  "\r"		mh-index-press-button)



;;; Help Messages

;;; If you add a new prefix, add appropriate text to the nil key.
;;;
;;; In general, messages are grouped logically. Taking the main commands for
;;; example, the first line is "ways to view messages," the second line is
;;; "things you can do with messages", and the third is "composing" messages.
;;;
;;; When adding a new prefix, ensure that the help message contains "what" the
;;; prefix is for. For example, if the word "folder" were not present in the
;;; `F' entry, it would not be clear what these commands operated upon.
(defvar mh-index-folder-mode-help-messages
  '((nil "[i]nc, [.]show, [,]show all, [n]ext, [p]revious,\n"
	 "[s]end, [q]uit")
    (?F "[v]isit folder; [i]ndexed search"))
  "Key binding cheat sheet.

This is an associative array which is used to show the most common commands.
The key is a prefix char. The value is one or more strings which are
concatenated together and displayed in the minibuffer if ? is pressed after
the prefix character. The special key nil is used to display the
non-prefixed commands.

The substitutions described in `substitute-command-keys' are performed as
well.")



(defun mh-index-search (folder search-regexp &optional new-buffer-flag)
  "Perform an indexed search in an MH mail folder.

FOLDER is searched with SEARCH-REGEXP and the results are presented in an MH-E
folder. If FOLDER is \"+\" then mail in all folders are searched. Optional
prefix argument NEW-BUFFER-FLAG decides whether the results are presented in a
new buffer. This allows multiple search results to coexist.

Four indexing programs are supported; if none of these are present, then grep
is used. This function picks the first program that is available on your
system. If you would prefer to use a different program, set the customization
variable `mh-index-program' accordingly.

The documentation for the following functions describes how to generate the
index for each program:

    - `mh-swish++-execute-search'
    - `mh-swish-execute-search'
    - `mh-namazu-execute-search'
    - `mh-glimpse-execute-search'"
  (interactive
   (list (progn
           (unless mh-find-path-run (mh-find-path))
           (mh-prompt-for-folder "Search" "+" nil "all"))
         (progn
           ;; Yes, we do want to call mh-index-choose every time in case the
           ;; user has switched the indexer manually.
           (unless (mh-index-choose) (error "No indexing program found"))
           (read-string (format "%s regexp: "
                                (upcase-initials (symbol-name mh-indexer)))))
         current-prefix-arg))
  (setq mh-index-max-msg-index 0)
  (let ((config (current-window-configuration))
        (mh-index-buffer
          (cond (new-buffer-flag
                 (buffer-name (generate-new-buffer mh-index-buffer)))
                ((and (eq major-mode 'mh-index-folder-mode))
                 (buffer-name (current-buffer)))
                (t mh-index-buffer)))
        (mh-index-show-buffer
          (cond (new-buffer-flag
                 (buffer-name (generate-new-buffer mh-index-show-buffer)))
                ((eq major-mode 'mh-index-folder-mode)
                 mh-index-other-buffer)
                (t mh-index-show-buffer))))
    (when (buffer-live-p (get-buffer mh-index-show-buffer))
      (kill-buffer (get-buffer mh-index-show-buffer)))
    (get-buffer-create mh-index-buffer)
    (get-buffer-create mh-index-show-buffer)
    (save-excursion
      (set-buffer mh-index-buffer)
      (setq mh-index-other-buffer mh-index-show-buffer))
    (save-excursion
      (set-buffer mh-index-show-buffer)
      (setq mh-index-other-buffer mh-index-buffer))
    (set-buffer mh-index-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let* ((folder-path (format "%s%s" mh-user-path (substring folder 1)))
           (count 0)
           (folder-count 0)
           cur-folder last-folder cur-index last-index
           parse-results button-start button-end)
      (setq mh-index-matches (make-hash-table :test #'equal))

      ;; Run search program...
      (message "%s searching... " (upcase-initials (symbol-name mh-indexer)))
      (funcall mh-index-execute-search-function folder-path search-regexp)

      ;; Parse output and generate folder view
      (message "Processing %s output... " mh-indexer)
      (goto-char (point-min))
      (while (setq parse-results (funcall mh-index-next-result-function))
        (unless (eq parse-results 'error)
          (setq cur-folder (car parse-results)
                cur-index (cadr parse-results))
          (setq mh-index-max-msg-index (max mh-index-max-msg-index cur-index))
          (cond ((and (equal cur-folder last-folder)
                      (= cur-index last-index))
                 nil)
                ((equal cur-folder last-folder)
                 (save-excursion
                   (set-buffer mh-index-buffer)
                   (push cur-index (gethash cur-folder mh-index-matches))))
                (t
                 (save-excursion
                   (set-buffer mh-index-buffer)
                   (unless (gethash cur-folder mh-index-matches)
                     (setq button-start (point))
                     (gnus-eval-format "%T\n" '((?T cur-folder ?s))
                                       `(,@(gnus-local-map-property
                                            mh-index-button-map)
                                         mh-callback mh-index-callback
                                         mh-data ,cur-folder))
                     (setq button-end (point))
                     (widget-convert-button
                      'link button-start button-end
                      :button-keymap mh-index-button-map
                      :action 'mh-index-callback)
                     (insert "\n"))
                   (push cur-index (gethash cur-folder mh-index-matches)))))
          (setq last-folder cur-folder)
          (setq last-index cur-index)))

      ;; Get rid of extra line at end of the buffer if there were any hits.
      (set-buffer mh-index-buffer)
      (goto-char (point-max))
      (when (and (= (forward-line -1) 0) (bolp) (eolp))
        (delete-char 1))

      ;; Set mh-cmd-note to a large enough value...
      (when mh-adaptive-cmd-note-flag
        (mh-set-cmd-note (mh-index-find-max-width mh-index-max-msg-index)))

      ;; Generate scan lines for the hits.
      (message "Generating scan lines... ")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((folder (get-text-property (point) 'mh-data)))
          (when folder
            (incf folder-count)
            (forward-line)
            (incf count (mh-index-insert-scan folder))))
        (forward-line))

      ;; Go to the first hit (if any).
      (goto-char (point-min))
      (forward-line)

      ;; Remember old window configuration
      (setq mh-index-previous-window-configuration config)

      ;; Setup folder buffer mode
      (when mh-decode-mime-flag
        (add-hook 'kill-buffer-hook 'mh-mime-cleanup))
      (mh-index-folder-mode)
      (setq mh-show-buffer mh-index-show-buffer)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (mh-index-configure-one-window)
      (setq mh-current-folder nil mh-index-current-msg nil)
      (message "%s found %s matches in %s folders"
               (upcase-initials (symbol-name mh-indexer))
               count folder-count))))

(defun mh-index-find-max-width (max-index)
  "Given MAX-INDEX find the number of digits necessary to print it."
  (let ((result 1)
        (max-int 9))
    (while (< max-int max-index)
      (incf result)
      (setq max-int (+ (* 10 max-int) 9)))
    result))

(defun mh-index-search-again ()
  "Call `mh-index-search' from index search buffer."
  (interactive)
  (cond ((eq major-mode 'mh-index-show-mode)
         (set-buffer mh-index-other-buffer))
        ((not (eq major-mode 'mh-index-folder-mode))
         (error "Should be called from one of the index buffers")))
  (let ((old-buffer (current-buffer))
        (window-config mh-index-previous-window-configuration))
    (unwind-protect (call-interactively 'mh-index-search)
      (when (eq old-buffer (current-buffer))
        (setq mh-index-previous-window-configuration window-config)))))

(defun mh-index-insert-scan (folder)
  "Insert scan lines for hits in FOLDER that the indexing program found.
The only twist is to replace the subject/body field with the match (if
possible)."
  (save-excursion
    (apply #'mh-exec-cmd-output
           mh-scan-prog nil (mh-scan-format)
           "-noclear" "-noheader" "-width" (window-width)
           folder (mh-coalesce-msg-list (gethash folder mh-index-matches))))
  (save-excursion
    (let ((window-width (window-width))
          (count 0))
      (while (not (or (get-text-property (point) 'mh-data) (eobp)))
        (beginning-of-line)
        (unless (and (eolp) (bolp))
          (incf count)
          (forward-char mh-cmd-note)
          (delete-char 1)
          (insert " "))
        (forward-line 1))
      count)))

(defun mh-index-callback ()
  "Callback function for buttons in the index buffer."
  (let* ((folder (save-excursion
                   (buffer-substring-no-properties
                    (progn (beginning-of-line) (point))
                    (progn (end-of-line) (point)))))
         (data (get-text-property (point) 'mh-data))
         (msg-list (gethash data mh-index-matches)))
    (when msg-list
      (mh-visit-folder folder msg-list))))

(defmacro mh-defun-index (func args &rest body)
  "Macro to generate a function callable both from index and show buffer.
FUNC is the function name, ARGS the argument list and BODY the function
body."
  (let ((cur (gensym))
        interactive-spec doc-string)
    (when (stringp (car body))
      (setq doc-string (car body))
      (setq body (cdr body)))
    (when (and (listp (car body)) (eq (caar body) 'interactive))
      (setq interactive-spec (car body))
      (setq body (cdr body)))
    `(defun ,func ,args
       ,@(if doc-string (list doc-string) ())
       ,interactive-spec
       (let* ((mh-index-buffer (if (eq major-mode 'mh-index-folder-mode)
                                   (buffer-name (current-buffer))
                                 mh-index-other-buffer))
              (mh-index-show-buffer (if (eq major-mode 'mh-index-show-mode)
                                        (buffer-name (current-buffer))
                                      mh-index-other-buffer))
              (,cur (cond ((eq (get-buffer mh-index-buffer)
                               (current-buffer))
                           mh-index-buffer)
                          ((eq (get-buffer mh-index-show-buffer)
                               (current-buffer))
                           mh-index-show-buffer)
                          (t (error "Not called from mh-index buffer")))))
         (flet ((mh-msg-folder (folder) mh-index-buffer)
                (mh-msg-filename (msg-num folder)
                  (format "%s%s/%s" mh-user-path (subseq folder 1) msg-num)))
           (cond ((eq ,cur mh-index-buffer)
                  (mh-index-goto-nearest-msg)
                  (when (and mh-current-folder mh-index-current-msg)
                    (mh-index-notate mh-current-folder
                                     mh-index-current-msg " " mh-cmd-note))
                  (setq mh-current-folder (mh-index-parse-folder))
                  (setq mh-index-current-msg (mh-index-parse-msg-number)))
                 ((eq ,cur mh-index-show-buffer)
                  (set-buffer mh-index-buffer)
                  (mh-index-goto-msg mh-current-folder
                                     mh-index-current-msg)
                  (mh-index-notate nil nil " " mh-cmd-note))
                 (t (error "This can't happen!")))
           (unwind-protect
               (progn ,@body)
             (save-excursion
               (set-buffer mh-index-buffer)
               (mh-index-goto-msg mh-current-folder mh-index-current-msg)
               (mh-recenter nil))
             (mh-index-configure-windows)
             (pop-to-buffer ,cur)))))))

(defun mh-index-advance (steps)
  "Advance STEPS messages in the folder buffer.
If there are less than STEPS messages left then an error message is printed."
  (let* ((backward-flag (< steps 0))
         (steps (if backward-flag (- steps) steps))
         point)
    (block body
      (save-excursion
        (while (> steps 0)
          (unless (= (forward-line (if backward-flag -1 1)) 0)
            (return-from body))
          (cond ((and (eolp) (bolp) (not backward-flag))
                 (unless (= (forward-line 2) 0) (return-from body)))
                ((and (get-text-property (point) 'mh-data) backward-flag)
                 (unless (= (forward-line -2) 0) (return-from body)))
                ((or (and (eolp) (bolp))
                     (get-text-property (point) 'mh-data))
                 (error "Mh-index-buffer is inconsistent")))
          (decf steps))
        (setq point (point))))
    (cond (point (goto-char point) t)
          (t nil))))

;; Details about message at point. These functions assume that we are on a
;; line which contains a message scan line and not on a blank line or a line
;; with a folder name.
(defun mh-index-parse-msg-number ()
  "Parse message number of message at point."
  (save-excursion
    (beginning-of-line)
    (let* ((b (point))
           (e (progn (forward-char mh-cmd-note) (point)))
           (data (ignore-errors
                   (read-from-string (buffer-substring-no-properties b e)))))
      (unless (and (consp data) (integerp (car data)))
        (error "Didn't find message number"))
      (car data))))

(defun mh-index-parse-folder ()
  "Parse folder of message at point."
  (save-excursion
    (while (not (get-text-property (point) 'mh-data))
      (unless (eql (forward-line -1) 0)
        (error "Reached beginning of buffer without seeing a folder")))
    (buffer-substring-no-properties (progn (beginning-of-line) (point))
                                    (progn (end-of-line) (point)))))

(defun mh-index-goto-nearest-msg ()
  "If point is not at a message go to the closest line with a message on it."
  (beginning-of-line)
  (cond ((and (eolp) (bolp)) (forward-line -1))
        ((get-text-property (point) 'mh-data) (forward-line 1))))

;; Window configuration for mh-index... There should be similar functions
;; in MH-E but I couldn't find them. I got the idea of using next-window,
;; previous-window and minibuffer-window from MH-E code.
(defun mh-index-configure-windows ()
  "Configure windows."
  (cond ((and (buffer-live-p (get-buffer mh-index-show-buffer))
              (buffer-live-p (get-buffer mh-index-buffer))
              (eq (save-excursion (set-buffer mh-index-show-buffer) major-mode)
                  'mh-index-show-mode))
         (mh-index-configure-two-windows))
        ((buffer-live-p (get-buffer mh-index-buffer))
         (mh-index-configure-one-window))))

(defun mh-count-windows ()
  "Count the number of windows in the current frame.
The minibuffer window is excluded from the count."
  (let* ((start-window (next-window nil t))
         (current-window (next-window start-window t))
         (count 0))
    (while (not (eq current-window start-window))
      (incf count)
      (setq current-window (next-window current-window t)))
    count))

(defun mh-index-configure-two-windows ()
  "Force a split view like that of MH-E."
  (save-excursion
    (unless (and (get-buffer mh-index-show-buffer)
                 (get-buffer mh-index-buffer))
      (error "We don't have both index buffers"))
    (let ((window-count (mh-count-windows)))
      (unless (and (= window-count 2)
                   (eq (window-buffer (next-window (minibuffer-window)))
                       (get-buffer mh-index-buffer))
                   (eq (window-buffer (previous-window (minibuffer-window)))
                       (get-buffer mh-index-show-buffer)))
        (unless (= window-count 2)
          (delete-other-windows)
          (split-window-vertically))
        (set-window-buffer (next-window (minibuffer-window))
                           mh-index-buffer)
        (set-window-buffer (previous-window (minibuffer-window))
                           mh-index-show-buffer))
      (unless (and (get-buffer-window mh-index-buffer)
                   (= (window-height (get-buffer-window mh-index-buffer))
                      mh-summary-height))
        (pop-to-buffer mh-index-buffer)
        (shrink-window (- (window-height) mh-summary-height))))
    (set-window-point (previous-window (minibuffer-window))
                      (progn (set-buffer mh-index-show-buffer) (point)))
    (set-window-point (next-window (minibuffer-window))
                      (progn (set-buffer mh-index-buffer) (point)))))

(defun mh-index-configure-one-window ()
  "Single window view."
  (save-excursion
    (unless (buffer-live-p (get-buffer mh-index-buffer))
      (error "Should have mh-index-buffer"))
    (switch-to-buffer mh-index-buffer)
    (delete-other-windows)
    (set-window-point (next-window (minibuffer-window))
                      (progn (set-buffer mh-index-buffer) (point)))))

;; This is slightly more involved than normal MH-E since we may have multiple
;; folders in the same buffer.
(defun mh-index-goto-msg (folder msg)
  "Move the cursor to the message specified by FOLDER and MSG."
  (block body
    (unless (buffer-live-p (get-buffer mh-index-buffer))
      (error "No index buffer to go to"))
    (set-buffer mh-index-buffer)
    (goto-char (point-min))
    (while (re-search-forward (format "^%s$" folder) nil t)
      (forward-line)
      (while (not (eolp))
        (when (= (mh-index-parse-msg-number) msg)
          (return-from body))
        (forward-line)))
    (error "Folder: %s, msg: %s doesn't exist" folder msg)))

;; Can't use mh-notate directly since we could have more than one folder in
;; the same buffer
(defun mh-index-notate (folder msg notation offset)
  "Add notation to scan line.
FOLDER is the message folder and MSG the message index. These arguments
specify the message to be notated. NOTATION is the character to be used to
notate and OFFSET is the number of chars from start of the line where
notation is to be placed."
  (save-excursion
    (set-buffer mh-index-buffer)
    (let ((buffer-read-only nil)
          (modified-p (buffer-modified-p))
          (found t))
      (setq found nil)
      (when (and (stringp folder) (numberp msg))
        (block nil
          (goto-char (point-min))
          (re-search-forward (format "^%s$" folder))
          (forward-line)
          (while (not (eolp))
            (when (= (mh-index-parse-msg-number) msg)
              (setq found t)
              (return))
            (forward-line))))
      (when found
        (beginning-of-line)
        (forward-char offset)
        (delete-char 1)
        (insert notation)
        (unless modified-p (set-buffer-modified-p nil))))))



;;; User functions

(mh-defun-index mh-index-show (display-headers-flag)
  "Display message at point.
If there are no messages at point then display the closest message.
The value of `mh-index-show-hook' is a list of functions to be called,
with no arguments, after the message has been displayed.
If DISPLAY-HEADERS-FLAG is non-nil then the raw message is shown."
  (interactive (list nil))
  (when (or (and (bolp) (eolp)) (get-text-property (point) 'mh-data))
    (error "No message at point"))
  (setq mh-current-folder (mh-index-parse-folder))
  (setq mh-index-current-msg (mh-index-parse-msg-number))
  ;; Do new notation
  (when (and mh-current-folder mh-index-current-msg)
    (mh-index-notate mh-current-folder mh-index-current-msg
                     mh-note-cur mh-cmd-note))
  (let ((mh-decode-mime-flag (and (not display-headers-flag) mh-decode-mime-flag))
        (mh-clean-message-header-flag
	 (and (not display-headers-flag) mh-clean-message-header-flag))
        (mhl-formfile (if display-headers-flag nil mhl-formfile))
        (msg mh-index-current-msg)
        (folder mh-current-folder))
    (when (not (eq display-headers-flag mh-showing-with-headers))
      (mh-invalidate-show-buffer))
    (mh-in-show-buffer (mh-index-show-buffer)
      (mh-display-msg msg folder))
    ;; Search for match in shown message
    (select-window (get-buffer-window mh-index-show-buffer))
    (set-buffer mh-index-show-buffer)
    (mh-index-show-mode))
  (run-hooks 'mh-index-show-hook))

(defun mh-index-header-display ()
  "Show the message with full headers."
  (interactive)
  (mh-index-show t)
  (setq mh-showing-with-headers t))

(mh-defun-index mh-index-next (steps)
  "Display next message.
Prefix argument STEPS specifies the number of messages to skip ahead."
  (interactive "p")
  (mh-index-goto-nearest-msg)
  (if (mh-index-advance steps)
      (mh-index-show nil)
    (mh-index-show nil)
    (message "Not enough messages")))

(mh-defun-index mh-index-prev (steps)
  "Display previous message.
Prefix argument STEPS specifies the number of messages to skip backward."
  (interactive "p")
  (mh-index-goto-nearest-msg)
  (if (mh-index-advance (- steps))
      (mh-index-show nil)
    (mh-index-show nil)
    (message "Not enough messages")))

(defun mh-index-page-msg (arg)
  "Scroll the displayed message upward ARG lines."
  (interactive "P")
  (save-excursion
    (let* ((show-buffer (cond ((eq major-mode 'mh-index-folder-mode)
                               mh-index-other-buffer)
                              ((eq major-mode 'mh-index-show-mode)
                               (buffer-name (current-buffer)))
                              (t (error "Don't use mh-index-page-msg"))))
           (window (get-buffer-window show-buffer))
           (current-window (selected-window)))
      (when (window-live-p window)
        (select-window window)
        (unwind-protect (scroll-up arg)
          (select-window current-window))))))

(defun mh-index-previous-page (arg)
  "Scroll the displayed message downward ARG lines."
  (interactive "P")
  (save-excursion
    (let* ((show-buffer (cond ((eq major-mode 'mh-index-folder-mode)
                               mh-index-other-buffer)
                              ((eq major-mode 'mh-index-show-mode)
                               (buffer-name (current-buffer)))
                              (t (error "Don't use mh-index-previous-page"))))
           (window (get-buffer-window show-buffer))
           (current-window (selected-window)))
      (when (window-live-p window)
        (select-window window)
        (unwind-protect (scroll-down arg)
          (select-window current-window))))))

(defun mh-index-press-button ()
  "Press index button."
  (interactive)
  (let ((function (get-text-property (point) 'mh-callback)))
    (when function
      (funcall function))))

(defun mh-index-quit ()
  "Quit the index folder.
Restore the previous window configuration, if one exists.
The value of `mh-before-quit-hook' is a list of functions to be called, with
no arguments, immediately upon entry to this function.
The value of `mh-quit-hook' is a list of functions to be called, with no
arguments, upon exit of this function."
  (interactive)
  (cond ((eq major-mode 'mh-index-show-mode)
         (set-buffer mh-index-other-buffer))
        ((not (eq major-mode 'mh-index-folder-mode))
         (error "The function mh-index-quit shouldn't be called")))
  (run-hooks 'mh-before-quit-hook)
  (let ((mh-index-buffer (buffer-name (current-buffer)))
        (mh-index-show-buffer mh-index-other-buffer)
        (window-config mh-index-previous-window-configuration))
    (when (buffer-live-p (get-buffer mh-index-buffer))
      (bury-buffer (get-buffer mh-index-buffer)))
    (when (buffer-live-p (get-buffer mh-index-show-buffer))
      (bury-buffer (get-buffer mh-index-show-buffer)))
    (when window-config
      (set-window-configuration window-config)))
  (run-hooks 'mh-quit-hook))

;; Can't quite use mh-next-button... This buffer has no concept of
;; folder-buffer or show-buffer. Maybe refactor mh-next-button?
(defun mh-index-next-button (&optional backward-flag)
  "Go to the next button.
Advance point to the next button in the show buffer. If the end of buffer is
reached then the search wraps over to the start of the buffer. With optional
argument BACKWARD-FLAG the point will move to the previous button."
  (interactive current-prefix-arg)
  (mh-goto-next-button backward-flag))

(defun mh-index-prev-button ()
  "Go to the next button.
Move point to the previous button in the show buffer. If the beginning of
the buffer is reached then the search wraps over to the end."
  (interactive)
  (mh-index-next-button t))



;; Glimpse interface

(defvar mh-glimpse-binary (executable-find "glimpse"))
(defvar mh-glimpse-directory ".glimpse")

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

If there are any directories you would like to ignore, append lines like the
following to .glimpse_exclude:

    ^/home/user/Mail/scripts

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



;; Swish interface

(defvar mh-swish-binary (executable-find "swish-e"))
(defvar mh-swish-directory ".swish")
(defvar mh-swish-folder nil)

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
    FileRules filename is index
    FileRules filename is \..*
    FileRules filename is #.*
    FileRules filename is ,.*
    FileRules filename is .*~

If there are any directories you would like to ignore, append lines like the
following to config:

    FileRules pathname contains /home/user/Mail/scripts

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

(defun mh-swish++-execute-search (folder-path search-regexp)
  "Execute swish++ and read the results.

In the examples below, replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.swish++. Then create the file
/home/user/Mail/.swish++/swish++.conf with the following contents:

    IncludeMeta		Bcc Cc Comments Content-Description From Keywords
    IncludeMeta		Newsgroups Resent-To Subject To
    IncludeFile		Mail	[0-9]*
    IndexFile		/home/user/Mail/.swish++/swish++.index

Use the following command line to generate the swish index. Run this
daily from cron:

    index -c /home/user/Mail/.swish++/swish++.conf /home/user/Mail

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



;; Namazu interface

(defvar mh-namazu-binary (executable-find "namazu"))
(defvar mh-namazu-directory ".namazu")
(defvar mh-namazu-folder nil)

(defun mh-namazu-execute-search (folder-path search-regexp)
  "Execute namazu and read the results.

In the examples below, replace /home/user/Mail with the path to your MH
directory.

First create the directory /home/user/Mail/.namazu. Then create the file
/home/user/Mail/.namazu/mknmzrc with the following contents:

    package conf;  # Don't remove this line!
    $ADDRESS = 'user@localhost';
    $ALLOW_FILE = \"[0-9]*\";

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
                 (mark (search "/" folder/msg :from-end t)))
            (unless mark (return 'error))
            (list (format "+%s" (substring folder/msg 0 mark))
                  (let ((n (ignore-errors (read-from-string
                                           (substring folder/msg (1+ mark))))))
                    (if (and (consp n) (numberp (car n)))
                        (car n)
                      (return 'error)))
                  nil))))
    (forward-line)))



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
            (setq mh-index-execute-search-function (caddr current))
            (setq mh-index-next-result-function (cadddr current))
            (return mh-indexer))))
      nil)))



;;; Menu extracted from mh-menubar.el V1.1 (31 July 2001)
;;; Menus for folder mode: folder, message (in that order)
;;; folder-mode "Message" menu
(easy-menu-define
  mh-index-folder-message-menu mh-index-folder-mode-keymap
  "Menu for MH-E folder-message."
  '("Message"
    ["Show Message"             mh-index-show (mh-get-msg-num nil)]
    ["Show Message with Header" mh-index-header-display (mh-get-msg-num nil)]
    ["Next Message"             mh-index-next t]
    ["Previous Message"         mh-index-prev t]
    "--"
    ["Compose a New Message"    mh-send t]))

;;; folder-mode "Folder" menu
(easy-menu-define
  mh-index-folder-folder-menu mh-index-folder-mode-keymap
  "Menu for MH-E folder."
  '("Folder"
    ["Incorporate New Mail"     mh-inc-folder t]
    "--"
    ["Visit a Folder..."        mh-visit-folder t]
    ["Indexed Search..."        mh-index-search-again t]
    "--"
    ["Quit Indexed Search"      mh-index-quit t]))



;;; Support for emacs21 toolbar using gnus/message.el icons (and code).
(eval-when-compile (defvar tool-bar-map))
(defvar mh-index-folder-tool-bar-map nil)
(when (fboundp 'tool-bar-add-item)
  (setq mh-index-folder-tool-bar-map
    (let ((tool-bar-map (make-sparse-keymap)))
      (tool-bar-add-item "mail" 'mh-inc-folder
                         'mh-indexfoldertoolbar-inc-folder
                         :help "Incorporate new mail in Inbox")
      (tool-bar-add-item "left_arrow" 'mh-index-prev
                         'mh-indexfoldertoolbar-prev :help "Previous message")
      (tool-bar-add-item "page-down" 'mh-index-page-msg
                         'mh-indexfoldertoolbar-page
                         :help "Page this message")
      (tool-bar-add-item "right_arrow" 'mh-index-next
                         'mh-indexfoldertoolbar-next :help "Next message")

      (tool-bar-add-item "mail_compose" 'mh-send 'mh-indexfoldertoolbar-compose
                         :help "Compose new message")

      (tool-bar-add-item "search"
                         (lambda (&optional arg)
                           (interactive "P")
                           (call-interactively mh-tool-bar-search-function))
                         'mh-indexfoldertoolbar-search :help "Search")
      (tool-bar-add-item "fld_open" 'mh-visit-folder
                         'mh-indexfoldertoolbar-visit
                         :help "Visit other folder")

      (tool-bar-add-item "preferences" (lambda ()
                                         (interactive)
                                         (customize-group "mh"))
                         'mh-indexfoldertoolbar-customize
                         :help "MH-E preferences")
      (tool-bar-add-item "help" (lambda ()
                                  (interactive)
                                  (Info-goto-node "(mh-e)Top"))
                         'mh-indexfoldertoolbar-help :help "Help")
      tool-bar-map)))

;; Modes for mh-index
(define-derived-mode mh-index-folder-mode mh-folder-mode "MH-Index-Folder"
  "Major MH-E mode for displaying the results of searching.\\<mh-index-folder-mode-keymap>

You can display the message the cursor is pointing to and step through the
messages.

You can also jump to the folders narrowed to the search results by pressing
RET on the folder name. Many operations, such as replying to a message,
require that you do this first.

\\{mh-index-folder-mode-keymap}"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mh-index-font-lock-keywords t))
  (use-local-map mh-index-folder-mode-keymap)
  (make-local-variable 'mh-help-messages)
  (easy-menu-add mh-index-folder-message-menu)
  (easy-menu-add mh-index-folder-folder-menu)
  (if (and (boundp 'tool-bar-mode) tool-bar-mode)
      (set (make-local-variable 'tool-bar-map) mh-index-folder-tool-bar-map))
  (setq mh-help-messages mh-index-folder-mode-help-messages))

(define-derived-mode mh-index-show-mode mh-show-mode "MH-Index-Show"
  "Major mode for showing messages in MH-E index.\\<mh-index-folder-mode-keymap>
\\{mh-index-folder-mode-keymap}"
  (use-local-map mh-index-folder-mode-keymap)
  (setq mh-help-messages mh-index-folder-mode-help-messages))

;; Font lock support for mh-index-folder. This is the same as mh-folder
;; except that the folder line needs to be recognized and highlighted.
(defvar mh-index-folder-face 'mh-index-folder-face
  "Face for highlighting folders in MH-Index buffers.")
(defface mh-index-folder-face
  '((((class color) (background light))
     (:foreground "dark green"))
    (((class color) (background dark))
     (:foreground "indian red"))
    (t
     (:bold t)))
  "Face for highlighting folders in MH-Index buffers."
  :group 'mh)

(eval-after-load "font-lock"
  '(progn
     (defvar mh-index-folder-face 'mh-index-folder-face
       "Face for highlighting folders in MH-Index buffers.")
     
     (defvar mh-index-font-lock-keywords
       (list
        ;; Folder name
        (list "^\\+.*" '(0 mh-index-folder-face))
        ;; Marked for deletion
        (list (concat mh-scan-deleted-msg-regexp ".*")
	      '(0 mh-folder-deleted-face))
        ;; Marked for refile
        (list (concat mh-scan-refiled-msg-regexp ".*")
	      '(0 mh-folder-refiled-face))
        ;;after subj
        (list mh-scan-body-regexp '(1 mh-folder-body-face nil t))
        '(mh-folder-font-lock-subject
          (1 mh-folder-followup-face append t)
          (2 mh-folder-subject-face append t))
        ;;current msg
        (list mh-scan-cur-msg-number-regexp
	      '(1 mh-folder-cur-msg-number-face))
        (list mh-scan-good-msg-regexp
	      '(1 mh-folder-msg-number-face))  ;; Msg number
        (list mh-scan-date-regexp '(1 mh-folder-date-face))       ;; Date
        (list mh-scan-rcpt-regexp
              '(1 mh-folder-to-face)                              ;; To:
              '(2 mh-folder-address-face))                        ;; address
        ;; scan font-lock name
        (list mh-scan-format-regexp
              '(1 mh-folder-date-face)
              '(3 mh-folder-scan-format-face))
        ;; Current message line
        (list mh-scan-cur-msg-regexp
	      '(1 mh-folder-cur-msg-face prepend t)))
       "Regexp keywords used to fontify the MH-Index-Folder buffer.")))

(provide 'mh-index)

;;; Local Variables:
;;; sentence-end-double-space: nil
;;; End:

;;; mh-index ends here
