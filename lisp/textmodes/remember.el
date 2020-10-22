;;; remember --- a mode for quickly jotting down things to remember

;; Copyright (C) 1999-2001, 2003-2020 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: emacs-devel@gnu.org
;; Created: 29 Mar 1999
;; Old-Version: 2.0
;; Keywords: data memory todo pim
;; URL: http://gna.org/projects/remember-el/

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

;; * The idea
;;
;; Todo lists, schedules, phone databases... everything we use
;; databases for is really just a way to extend the power of our
;; memory.  To be able to remember what our conscious mind may not
;; currently have access to.
;;
;; There are many different databases out there -- and good ones --
;; which this mode is not trying to replace.  Rather, it's how that
;; data gets there that's the question.  Most of the time, we just
;; want to say "Remember so-and-so's phone number, or that I have to
;; buy dinner for the cats tonight."  That's the FACT.  How it's
;; stored is really the computer's problem.  But at this point in
;; time, it's most definitely also the user's problem, and sometimes
;; so laboriously so that people just let data slip, rather than
;; expend the effort to record it.
;;
;; "Remember" is a mode for remembering data.  It uses whatever
;; back-end is appropriate to record and correlate the data, but it's
;; main intention is to allow you to express as _little_ structure as
;; possible up front.  If you later want to express more powerful
;; relationships between your data, or state assumptions that were at
;; first too implicit to be recognized, you can "study" the data later
;; and rearrange it.  But the initial "just remember this" impulse
;; should be as close to simply throwing the data at Emacs as
;; possible.
;;
;; * Implementation
;;
;; Hyperbole, as a data presentation tool, always struck me as being
;; very powerful, but it seemed to require a lot of "front-end" work
;; before that data was really available.  The problem with BBDB, or
;; keeping up a Bibl-mode file, is that you have to use different
;; functions to record the data, and it always takes time to stop what
;; you're doing, format the data in the manner expected by that
;; particular data interface, and then resume your work.
;;
;; With "remember", you just hit `M-x remember' (you'd probably want
;; to bind this to an easily accessible keystroke, like C-x M-r), slam
;; in your text however you like, and then hit C-c C-c.  It will file
;; the data away for later retrieval, and possibly indexing.
;;
;; Indexing is to data what "studying" is in the real world.  What you
;; do when you study (or lucubrate, for some of us) is to realize
;; certain relationships implicit in the data, so that you can make
;; use of those relationships.  Expressing that a certain quote you
;; remembered was a religious quote, and that you want the ability to
;; pull up all quotes of a religious nature, is what studying does.
;; This is a more labor intensive task than the original remembering
;; of the data, and it's typical in real life to set aside a special
;; period of time for doing this work.
;;
;; "Remember" works in the same way.  When you enter data, either by
;; typing it into a buffer, or using the contents of the selected
;; region, it will store that data -- unindexed, uninterpreted -- in a
;; data pool.  It will also try to remember as much context
;; information as possible (any text properties that were set, where
;; you copied it from, when, how, etc).  Later, you can walk through
;; your accumulated set of data (both organized, and unorganized) and
;; easily begin moving things around, and making annotations that will
;; express the full meaning of that data, as far as you know it.
;;
;; Obviously this latter stage is more user-interface intensive, and
;; it would be nice if "remember" could do it as elegantly as
;; possible, rather than requiring a billion keystrokes to reorganize
;; your hierarchy.  Well, as the future arrives, hopefully experience
;; and user feedback will help to make this as intuitive a tool as
;; possible.
;;
;; * Future Goals
;;
;; This tool hopes to track (and by doing it with as little new code
;; as possible):
;;
;;  - The raw data that gets entered
;;
;;  - The relationships between that data (either determined
;;    implicitly by parsing the input, or explicitly by the user's
;;    studying the data).
;;
;;  - Revisioning of the data
;;
;;  - Where it came from, and any context information that can be
;;    programmatically determined.
;;
;;  - Allowing particular views of the initially amorphous data pool
;;    (ala the Xanadu concept).
;;
;;  - Storage of the data in a manner most appropriate to that data,
;;    such as keeping address-book type information in BBDB, etc.
;;
;; * Using "remember"
;;
;; As a rough beginning, what I do is to keep my `remember-data-file' in
;; outline-mode format, with a final entry called "* Raw data".  Then,
;; at intervals, I can move the data that gets appended there into
;; other places.  But certainly this should evolve into an intuitive
;; mechanism for shuffling data off to its appropriate corner of the
;; universe.
;;
;; To map the primary remember function to the keystroke F8, do the
;; following.
;;
;;   (autoload 'remember "remember" nil t)
;;
;;   (define-key global-map [f8] 'remember)
;;
;; * Feedback
;;
;; If Emacs could become a more intelligent data store, where
;; brainstorming would focus on the IDEAS involved -- rather than the
;; structuring and format of those ideas, or having to stop your
;; current flow of work in order to record them -- it would map much
;; more closely to how the mind (well, at least mine) works, and hence
;; would eliminate that very manual-ness which computers from the very
;; beginning have been championed as being able to reduce.
;;
;; Have you ever noticed that having a laptop to write on doesn't
;; _actually_ increase the amount of quality material that you turn
;; out, in the long run?  Perhaps its because the time we save
;; electronically in one way, we're losing electronically in another;
;; the tool should never dominate one's focus.  As the mystic
;; Farīd ud-Dīn ʿAṭṭār wrote: "Be occupied as little as possible with
;; things of the outer world but much with things of the inner world;
;; then right action will overcome inaction."
;;
;; * Diary integration
;;
;; To use, add the following to your .emacs:
;;
;;   ;; This should be before other entries that may return t
;;   (add-to-list 'remember-handler-functions 'remember-diary-extract-entries)
;;
;; This module recognizes entries of the form
;;
;;   DIARY: ....
;;
;; and puts them in your ~/.diary (or remember-diary-file) together
;; with an annotation.  Dates in the form YYYY.MM.DD are converted to
;; YYYY-MM-DD so that diary can understand them.
;;
;; For example:
;;
;;   DIARY: 2003.08.12 Sacha's birthday
;;
;; is stored as
;;
;;   2003.08.12 Sacha's birthday

;;; History:

;;; Code:

(defconst remember-version "2.0"
  "This version of remember.")
(make-obsolete-variable 'remember-version nil "28.1")

(defgroup remember nil
  "A mode to remember information."
  :group 'data)

;;; User Variables:

(defcustom remember-mode-hook nil
  "Functions run upon entering `remember-mode'."
  :type 'hook
  :options '(flyspell-mode turn-on-auto-fill org-remember-apply-template)
  :group 'remember)

(defcustom remember-in-new-frame nil
  "Non-nil means use a separate frame for capturing remember data."
  :type 'boolean
  :group 'remember)

(defcustom remember-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'remember)

(defcustom remember-filter-functions nil
  "Functions run to filter remember data.
All functions are run in the remember buffer."
  :type 'hook
  :group 'remember)

(defcustom remember-handler-functions '(remember-append-to-file)
  "Functions run to process remember data.
Each function is called with the current buffer narrowed to what the
user wants remembered.
If any function returns non-nil, the data is assumed to have been
recorded somewhere by that function."
  :type 'hook
  :options '(remember-store-in-mailbox
             remember-append-to-file
             remember-store-in-files
             remember-diary-extract-entries
             org-remember-handler)
  :group 'remember)

(defcustom remember-all-handler-functions nil
  "If non-nil every function in `remember-handler-functions' is called."
  :type 'boolean
  :group 'remember)

;; See below for more user variables.

;;; Internal Variables:

(defvar remember-buffer "*Remember*"
  "The name of the remember data entry buffer.")

(defcustom remember-save-after-remembering t
  "Non-nil means automatically save after remembering."
  :type 'boolean
  :group 'remember)

;;; User Functions:

(defcustom remember-annotation-functions '(buffer-file-name)
  "Hook that returns an annotation to be inserted into the remember buffer."
  :type 'hook
  :options '(org-remember-annotation buffer-file-name)
  :group 'remember)

(defvar remember-annotation nil
  "Current annotation.")
(defvar remember-initial-contents nil
  "Initial contents to place into *Remember* buffer.")

(defcustom remember-before-remember-hook nil
  "Functions run before switching to the *Remember* buffer."
  :type 'hook
  :group 'remember)

(defcustom remember-run-all-annotation-functions-flag nil
  "Non-nil means use all annotations returned by `remember-annotation-functions'."
  :type 'boolean
  :group 'remember)

;;;###autoload
(defun remember (&optional initial)
  "Remember an arbitrary piece of data.
INITIAL is the text to initially place in the *Remember* buffer,
or nil to bring up a blank *Remember* buffer.

With a prefix or a visible region, use the region as INITIAL."
  (interactive
   (list (when (or current-prefix-arg
                   (and mark-active
                        transient-mark-mode))
           (buffer-substring (region-beginning) (region-end)))))
  (funcall (if remember-in-new-frame
               #'frameset-to-register
             #'window-configuration-to-register) remember-register)
  (let* ((annotation
          (if remember-run-all-annotation-functions-flag
              (mapconcat 'identity
                         (delq nil
                               (mapcar 'funcall remember-annotation-functions))
                         "\n")
            (run-hook-with-args-until-success
             'remember-annotation-functions)))
         (buf (get-buffer-create remember-buffer)))
    (run-hooks 'remember-before-remember-hook)
    (funcall (if remember-in-new-frame
                 #'switch-to-buffer-other-frame
               #'switch-to-buffer-other-window) buf)
    (if remember-in-new-frame
        (set-window-dedicated-p
         (get-buffer-window (current-buffer) (selected-frame)) t))
    (setq buffer-offer-save t)
    (remember-mode)
    (when (= (point-max) (point-min))
      (when initial (insert initial))
      (setq remember-annotation annotation)
      (when remember-initial-contents (insert remember-initial-contents))
      (when (and (stringp annotation)
                 (not (equal annotation "")))
        (insert "\n\n" annotation))
      (setq remember-initial-contents nil)
      (goto-char (point-min)))
    (message "Use C-c C-c to remember the data.")))

;;;###autoload
(defun remember-other-frame (&optional initial)
  "Call `remember' in another frame."
  (interactive
   (list (when current-prefix-arg
           (buffer-substring (point) (mark)))))
  (let ((remember-in-new-frame t))
    (remember initial)))

(defsubst remember-mail-date (&optional rfc822-p)
  "Return a simple date.  Nothing fancy."
  (format-time-string (if rfc822-p "%a, %e %b %Y %T %z" "%a %b %e %T %Y")))

(defun remember-buffer-desc ()
  "Using the first line of the current buffer, create a short description."
  (buffer-substring (point-min)
                    (save-excursion
                      (goto-char (point-min))
                      (end-of-line)
                      (if (> (- (point) (point-min)) 60)
                          (goto-char (+ (point-min) 60)))
                      (point))))

;; Remembering to UNIX mailboxes

(defcustom remember-mailbox "~/Mail/remember"
  "The file in which to store remember data as mail."
  :type 'file
  :group 'remember)

(defcustom remember-default-priority "medium"
  "The default priority for remembered mail messages."
  :type 'string
  :group 'remember)

(defun remember-store-in-mailbox ()
  "Store remember data as if it were incoming mail.
In which case `remember-mailbox' should be the name of the mailbox.
Each piece of pseudo-mail created will have an `X-Todo-Priority'
field, for the purpose of appropriate splitting."
  (let ((who (read-string "Who is this item related to? "))
        (moment (format-time-string "%s"))
        (desc (remember-buffer-desc))
        (text (buffer-string)))
    (with-temp-buffer
      (insert (format "From %s  %s
Date: %s
From: %s
Message-Id: <remember-%s@%s>
X-Todo-Priority: %s
To: %s <%s>
Subject: %s\n\n"
                      (user-login-name)
                      (remember-mail-date)
                      (remember-mail-date t)
                      who
                      moment (system-name)
                      remember-default-priority
                      (user-full-name) user-mail-address
                      desc))
      (let ((here (point)))
        (insert text)
        (unless (bolp)
          (insert "\n"))
        (insert "\n")
        (goto-char here)
        (while (re-search-forward "^\\(From[: ]\\)" nil t)
          (replace-match ">\\1")))
      (append-to-file (point-min) (point-max) remember-mailbox)
      t)))

;; Remembering to plain files

(defcustom remember-data-file (locate-user-emacs-file "notes" ".notes")
  "The file in which to store unprocessed data.
When set via customize, visited file of the notes buffer (if it
exists) might be changed."
  :version "24.4"                       ; added locate-user-emacs-file
  :type 'file
  :set (lambda (symbol value)
         (let ((buf (find-buffer-visiting (default-value symbol))))
           (set-default symbol value)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (set-visited-file-name
                (expand-file-name remember-data-file))))))
  :initialize 'custom-initialize-default
  :group 'remember)

(defcustom remember-leader-text "** "
  "The text used to begin each remember item."
  :type 'string
  :group 'remember)

(defcustom remember-time-format "%a %b %d %H:%M:%S %Y"
  "The format for time stamp, passed to `format-time-string'.
The default emulates `current-time-string' for backward compatibility."
  :type 'string
  :group 'remember
  :version "27.1")

(defun remember-append-to-file ()
  "Remember, with description DESC, the given TEXT."
  (let* ((text (buffer-string))
         (desc (remember-buffer-desc))
         (remember-text (concat "\n" remember-leader-text
                                (format-time-string remember-time-format)
                                " (" desc ")\n\n" text
                                (save-excursion (goto-char (point-max))
                                                (if (bolp) nil "\n"))))
         (buf (find-buffer-visiting remember-data-file)))
    (if buf
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-max))
            (insert remember-text))
          (if remember-save-after-remembering (save-buffer)))
      (append-to-file remember-text nil remember-data-file))))

(defun remember-region (&optional beg end)
  "Remember the data from BEG to END.
It is called from within the *Remember* buffer to save the text
that was entered.

If BEG and END are nil, the entire buffer will be remembered.

If you want to remember a region, supply a universal prefix to
`remember' instead.  For example: \\[universal-argument] \\[remember] RET."
  ;; Sacha: I have no idea where remember.el gets this context information, but
  ;; you can just use remember-annotation-functions.
  (interactive)
  (let ((b (or beg (min (point) (or (mark) (point-min)))))
        (e (or end (max (point) (or (mark) (point-max))))))
    (save-restriction
      (narrow-to-region b e)
      (if remember-all-handler-functions
          (run-hooks 'remember-handler-functions)
        (run-hook-with-args-until-success 'remember-handler-functions))
      (remember-destroy))))

(defcustom remember-data-directory "~/remember"
  "The directory in which to store remember data as files.
Used by `remember-store-in-files'."
  :type 'directory
  :version "24.4"
  :group 'remember)

(defcustom remember-directory-file-name-format "%Y-%m-%d_%T-%z"
  "Format string for the file name in which to store unprocessed data.
This is passed to `format-time-string'.
Used by `remember-store-in-files'."
  :type 'string
  :version "24.4"
  :group 'remember)

(defun remember-store-in-files ()
  "Store remember data in a file in `remember-data-directory'.
The file is named by calling `format-time-string' using
`remember-directory-file-name-format' as the format string."
  (let ((name (format-time-string remember-directory-file-name-format))
        (text (buffer-string)))
    (with-temp-buffer
      (insert text)
      (write-file (convert-standard-filename
                   (format "%s/%s" remember-data-directory name))))))

;;;###autoload
(defun remember-clipboard ()
  "Remember the contents of the current clipboard.
Most useful for remembering things from other applications."
  (interactive)
  (remember (current-kill 0)))

(defun remember-finalize ()
  "Remember the contents of the current buffer."
  (interactive)
  (remember-region (point-min) (point-max)))

(defun remember-destroy ()
  "Destroy the current *Remember* buffer."
  (interactive)
  (when (equal remember-buffer (buffer-name))
    (kill-buffer (current-buffer))
    (jump-to-register remember-register)))

;;; Diary integration

(defcustom remember-diary-file nil
  "File for extracted diary entries.
If this is nil, then `diary-file' will be used instead."
  :type '(choice (const :tag "diary-file" nil) file)
  :group 'remember)

(defvar calendar-date-style)            ; calendar.el

(defun remember-diary-convert-entry (entry)
  "Translate MSG to an entry readable by diary."
  (save-match-data
    (when remember-annotation
        (setq entry (concat entry " " remember-annotation)))
    (if (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" entry)
        (progn
          ;; For calendar-date-style.  This costs us nothing because
          ;; the call to diary-make-entry below loads diary-lib
          ;; which requires calendar.
          (require 'calendar)
          (replace-match
           (cond ((eq calendar-date-style 'european)
                  (concat (match-string 3 entry) "/"
                          (match-string 2 entry) "/"
                          (match-string 1 entry)))
                 ((eq calendar-date-style 'iso)
                  (concat (match-string 1 entry) "-"
                            (match-string 2 entry) "-"
                            (match-string 3 entry)))
                 (t (concat (match-string 2 entry) "/"
                            (match-string 3 entry) "/"
                            (match-string 1 entry))))
           t t entry))
      entry)))

(autoload 'diary-make-entry "diary-lib")

;;;###autoload
(defun remember-diary-extract-entries ()
  "Extract diary entries from the region."
  (save-excursion
    (goto-char (point-min))
    (let (list)
      (while (re-search-forward "^DIARY:\\s-*\\(.+\\)" nil t)
        (push (remember-diary-convert-entry (match-string 1)) list))
      (when list
        (diary-make-entry (mapconcat 'identity list "\n")
                          nil remember-diary-file))
      nil))) ;; Continue processing

;;; Internal Functions:

(defvar remember-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'remember-finalize)
    (define-key map "\C-c\C-c" 'remember-finalize)
    (define-key map "\C-c\C-k" 'remember-destroy)
    map)
  "Keymap used in `remember-mode'.")

(define-derived-mode remember-mode indented-text-mode "Remember"
  "Major mode for output from \\[remember].
This buffer is used to collect data that you want to remember.
\\<remember-mode-map>
Just hit \\[remember-finalize] when you're done entering, and it will file
the data away for latter retrieval, and possible indexing.

\\{remember-mode-map}"
  (set-keymap-parent remember-mode-map nil))

;; Notes buffer showing the notes:

(defcustom remember-notes-buffer-name "*notes*"
  "Name of the notes buffer.
Setting it to *scratch* will hijack the *scratch* buffer for the
purpose of storing notes."
  :type 'string
  :version "24.4")

(defcustom remember-notes-initial-major-mode nil
  "Major mode to use in the notes buffer when it's created.
If this is nil, use `initial-major-mode'."
  :type '(choice (const    :tag "Use `initial-major-mode'" nil)
		 (function :tag "Major mode" text-mode))
  :version "24.4")

(defcustom remember-notes-bury-on-kill t
  "Non-nil means `kill-buffer' will bury the notes buffer instead of killing."
  :type 'boolean
  :version "24.4")

(defun remember-notes-save-and-bury-buffer ()
  "Save (if it is modified) and bury the current buffer."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (bury-buffer))



(defvar remember-notes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'remember-notes-save-and-bury-buffer)
    map)
  "Keymap used in `remember-notes-mode'.")

(define-minor-mode remember-notes-mode
  "Minor mode for the `remember-notes' buffer.
This sets `buffer-save-without-query' so that `save-some-buffers' will
save the notes buffer without asking.

\\{remember-notes-mode-map}"
  nil nil nil
  (cond
   (remember-notes-mode
    (add-hook 'kill-buffer-query-functions
              #'remember-notes--kill-buffer-query nil t)
    (setq buffer-save-without-query t))))

;;;###autoload
(defun remember-notes (&optional switch-to)
  "Return the notes buffer, creating it if needed, and maybe switch to it.
This buffer is for notes that you want to preserve across Emacs sessions.
The notes are saved in `remember-data-file'.

If a buffer is already visiting that file, just return it.

Otherwise, create the buffer, and rename it to `remember-notes-buffer-name',
unless a buffer of that name already exists.  Set the major mode according
to `remember-notes-initial-major-mode', and enable `remember-notes-mode'
minor mode.

Use \\<remember-notes-mode-map>\\[remember-notes-save-and-bury-buffer] to save and bury the notes buffer.

Interactively, or if SWITCH-TO is non-nil, switch to the buffer.
Return the buffer.

Set `initial-buffer-choice' to `remember-notes' to visit your notes buffer
when Emacs starts.  Set `remember-notes-buffer-name' to \"*scratch*\"
to turn the *scratch* buffer into your notes buffer."
  (interactive "p")
  (let ((buf (or (find-buffer-visiting remember-data-file)
                 (with-current-buffer (find-file-noselect remember-data-file)
                   (and remember-notes-buffer-name
                        (not (get-buffer remember-notes-buffer-name))
                        (rename-buffer remember-notes-buffer-name))
                   (funcall (or remember-notes-initial-major-mode
                                initial-major-mode))
                   (remember-notes-mode 1)
                   (current-buffer)))))
    (when switch-to
      (switch-to-buffer buf))
    buf))

(defun remember-notes--kill-buffer-query ()
  "Function that `remember-notes-mode' adds to `kill-buffer-query-functions'.
Save the current buffer if modified.  If `remember-notes-bury-on-kill'
is non-nil, bury it and return nil; otherwise return t."
  (when (buffer-modified-p)
    (save-buffer))
  (if remember-notes-bury-on-kill
      (progn
        ;; bury-buffer always returns nil, but let's be explicit.
        (bury-buffer)
        nil)
    t))

(provide 'remember)

;;; remember.el ends here
