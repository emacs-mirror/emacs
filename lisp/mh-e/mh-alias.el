;;; mh-alias.el --- MH-E mail alias completion and expansion
;;
;; Copyright (C) 1994, 95, 96, 1997,
;;  2001, 02, 2003 Free Software Foundation, Inc.

;; Author: Peter S. Galbraith <psg@debian.org>
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

;;  [To be deleted when documented in MH-E manual.]
;;
;;  This module provides mail alias completion when entering addresses.
;;
;;  Use the TAB key to complete aliases (and optionally local usernames) when
;;  initially composing a message in the To: and Cc: minibuffer prompts. You
;;  may enter multiple addressees separated with a comma (but do *not* add any
;;  space after the comma).
;;
;;  In the header of a message draft, use "M-TAB (mh-letter-complete)" to
;;  complete aliases. This is useful when you want to add an addressee as an
;;  afterthought when creating a message, or when adding an additional
;;  addressee to a reply.
;;
;;  By default, completion is case-insensitive. This can be changed by
;;  customizing the variable `mh-alias-completion-ignore-case-flag'. This is
;;  useful, for example, to differentiate between people aliases in lowercase
;;  such as:
;;
;;    p.galbraith: Peter Galbraith <GalbraithP@dfo-mpo.gc.ca>
;;
;;  and lists in uppercase such as:
;;
;;    MH-E: MH-E mailing list <mh-e-devel@lists.sourceforge.net>
;;
;;  Note that this variable affects minibuffer completion only. If you have an
;;  alias for P.Galbraith and type in p.galbraith at the prompt, it will still
;;  be expanded in the letter buffer because MH is case-insensitive.
;;
;;  When you press ", (mh-alias-minibuffer-confirm-address)" after an alias in
;;  the minibuffer, the expansion for the previous mail alias appears briefly.
;;  To inhibit this, customize the variable `mh-alias-flash-on-comma'.
;;
;;  The addresses and aliases entered in the minibuffer are added to the
;;  message draft. To expand the aliases before they are added to the draft,
;;  customize the variable `mh-alias-expand-aliases-flag'.
;;
;;  Completion is also performed on usernames extracted from the /etc/passwd
;;  file. This can be a handy tool on a machine where you and co-workers
;;  exchange messages, but should probably be disabled on a system with
;;  thousands of users you don't know. This is done by customizing the
;;  variable `mh-alias-local-users'. This variable also takes a string which
;;  is executed to generate the password file. For example, you'd use "ypcat
;;  passwd" for NIS.
;;
;;  Aliases are loaded the first time you send mail and get the "To:" prompt
;;  and whenever a source of aliases changes. Sources of system aliases are
;;  defined in the customization variable `mh-alias-system-aliases' and
;;  include:
;;
;;    /etc/nmh/MailAliases
;;    /usr/lib/mh/MailAliases
;;    /etc/passwd
;;
;;  Sources of personal aliases are read from the files listed in your MH
;;  profile component Aliasfile. Multiple files are separated by white space
;;  and are relative to your mail directory.
;;
;;  Alias Insertions
;;  ~~~~~~~~~~~~~~~~
;;  There are commands to insert new aliases into your alias file(s) (defined
;;  by the `Aliasfile' component in the .mh_profile file or by the variable
;;  `mh-alias-insert-file').  In particular, there is a tool-bar icon to grab
;;  an alias from the From line of the current message.

;;; Change Log:

;;; Code:

(require 'mh-e)
(load "cmr" t t)                        ; Non-fatal dependency for
					; completing-read-multiple.
(eval-when-compile (defvar mail-abbrev-syntax-table))

;;; Autoloads
(eval-when (compile load eval)
  (ignore-errors
    (require 'mailabbrev)
    (require 'multi-prompt)))

(defvar mh-alias-alist 'not-read
  "Alist of MH aliases.")
(defvar mh-alias-blind-alist nil
  "Alist of MH aliases that are blind lists.")
(defvar mh-alias-passwd-alist nil
  "Alist of aliases extracted from passwd file and their expansions.")
(defvar mh-alias-tstamp nil
  "Time aliases were last loaded.")
(defvar mh-alias-read-address-map nil)
(if mh-alias-read-address-map
    ()
  (setq mh-alias-read-address-map
	(copy-keymap minibuffer-local-completion-map))
  (if mh-alias-flash-on-comma
      (define-key mh-alias-read-address-map
	"," 'mh-alias-minibuffer-confirm-address))
  (define-key mh-alias-read-address-map " " 'self-insert-command))


;;; Alias Loading

(defun mh-alias-tstamp (arg)
  "Check whether alias files have been modified.
Return t if any file listed in the MH profile component Aliasfile has been
modified since the timestamp.
If ARG is non-nil, set timestamp with the current time."
  (if arg
      (let ((time (current-time)))
        (setq mh-alias-tstamp (list (nth 0 time) (nth 1 time))))
    (let ((stamp))
      (car (memq t (mapcar
                    (function
                     (lambda (file)
                       (when (and file (file-exists-p file))
                         (setq stamp (nth 5 (file-attributes file)))
                         (or (> (car stamp) (car mh-alias-tstamp))
                             (and (= (car stamp) (car mh-alias-tstamp))
                                  (> (cadr stamp) (cadr mh-alias-tstamp)))))))
                    (mh-alias-filenames t)))))))

(defun mh-alias-filenames (arg)
  "Return list of filenames that contain aliases.
The filenames come from the MH profile component Aliasfile and are expanded.
If ARG is non-nil, filenames listed in `mh-alias-system-aliases' are appended."
  (or mh-progs (mh-find-path))
  (save-excursion
    (let* ((filename (mh-profile-component "Aliasfile"))
           (filelist (and filename (split-string filename "[ \t]+")))
           (userlist
            (mapcar
             (function
              (lambda (file)
                (if (and mh-user-path file
                         (file-exists-p (expand-file-name file mh-user-path)))
                    (expand-file-name file mh-user-path))))
             filelist)))
      (if arg
          (if (stringp mh-alias-system-aliases)
              (append userlist (list mh-alias-system-aliases))
            (append userlist mh-alias-system-aliases))
        userlist))))

(defun mh-alias-local-users ()
  "Return an alist of local users from /etc/passwd."
  (let (passwd-alist)
    (save-excursion
      (set-buffer (get-buffer-create mh-temp-buffer))
      (erase-buffer)
      (cond
       ((eq mh-alias-local-users t)
        (if (file-readable-p "/etc/passwd")
            (insert-file-contents "/etc/passwd")))
       ((stringp mh-alias-local-users)
        (insert mh-alias-local-users "\n")
        (shell-command-on-region (point-min) (point-max) mh-alias-local-users t)
        (goto-char (point-min))))
      (while  (< (point) (point-max))
        (cond
         ((looking-at "\\([^:]*\\):[^:]*:\\([^:]*\\):[^:]*:\\([^:,]*\\)[:,]")
          (when (> (string-to-int (match-string 2)) 200)
            (let* ((username (match-string 1))
                   (gecos-name (match-string 3))
                   (realname
                    (if (string-match "&" gecos-name)
                        (concat
                         (substring gecos-name 0 (match-beginning 0))
                         (capitalize username)
                         (substring gecos-name (match-end 0)))
                      gecos-name)))
              (setq passwd-alist
                    (cons (list username
                                (if (string-equal "" realname)
                                    (concat "<" username ">")
                                  (concat realname " <" username ">")))
                          passwd-alist))))))
        (forward-line 1)))
    passwd-alist))

;;;###mh-autoload
(defun mh-alias-reload ()
  "Load MH aliases into `mh-alias-alist'."
  (interactive)
  (save-excursion
    (message "Loading MH aliases...")
    (mh-alias-tstamp t)
    (mh-exec-cmd-quiet t "ali" "-nolist" "-nouser")
    (setq mh-alias-alist nil)
    (setq mh-alias-blind-alist nil)
    (while  (< (point) (point-max))
      (cond
       ((looking-at "^[ \t]"))          ;Continuation line
       ((looking-at "\\(.+\\): .+: .*$") ; A new -blind- MH alias
        (when (not (assoc-ignore-case (match-string 1) mh-alias-blind-alist))
          (setq mh-alias-blind-alist
                (cons (list (match-string 1)) mh-alias-blind-alist))
          (setq mh-alias-alist (cons (list (match-string 1)) mh-alias-alist))))
       ((looking-at "\\(.+\\): .*$")    ; A new MH alias
        (when (not (assoc-ignore-case (match-string 1) mh-alias-alist))
          (setq mh-alias-alist
                (cons (list (match-string 1)) mh-alias-alist)))))
      (forward-line 1)))
  (when mh-alias-local-users
    (setq mh-alias-passwd-alist (mh-alias-local-users))
    ;; Update aliases with local users, but leave existing aliases alone.
    (let ((local-users mh-alias-passwd-alist)
          user)
      (while local-users
        (setq user (car local-users))
        (if (not (assoc-ignore-case (car user) mh-alias-alist))
            (setq mh-alias-alist (append mh-alias-alist (list user))))
        (setq local-users (cdr local-users)))))
  (message "Loading MH aliases...done"))

(defun mh-alias-reload-maybe ()
  "Load new MH aliases."
  (if (or (eq mh-alias-alist 'not-read) ; Doesn't exist, so create it.
          (mh-alias-tstamp nil))        ; Out of date, so recreate it.
      (mh-alias-reload)))


;;; Alias Expansion

(defun mh-alias-ali (alias &optional user)
  "Return ali expansion for ALIAS.
ALIAS must be a string for a single alias.
If USER is t, then assume ALIAS is an address and call ali -user.
ali returns the string unchanged if not defined.  The same is done here."
  (condition-case err
      (save-excursion
        (let ((user-arg (if user "-user" "-nouser")))
          (mh-exec-cmd-quiet t "ali" user-arg "-nolist" alias))
        (goto-char (point-max))
        (if (looking-at "^$") (delete-backward-char 1))
        (buffer-substring (point-min)(point-max)))
    (error (progn
             (message (error-message-string err))
             alias))))

(defun mh-alias-expand (alias)
  "Return expansion for ALIAS.
Blind aliases or users from /etc/passwd are not expanded."
  (cond
   ((assoc-ignore-case alias mh-alias-blind-alist)
    alias)                              ; Don't expand a blind alias
   ((assoc-ignore-case alias mh-alias-passwd-alist)
    (cadr (assoc-ignore-case alias mh-alias-passwd-alist)))
   (t
    (mh-alias-ali alias))))

;;;###mh-autoload
(defun mh-read-address (prompt)
  "Read an address from the minibuffer with PROMPT."
  (mh-alias-reload-maybe)
  (if (not mh-alias-alist)		; If still no aliases, just prompt
      (read-string prompt)
    (let* ((minibuffer-local-completion-map mh-alias-read-address-map)
           (completion-ignore-case mh-alias-completion-ignore-case-flag)
           (the-answer
            (cond ((fboundp 'completing-read-multiple)
                   (mh-funcall-if-exists
                    completing-read-multiple prompt mh-alias-alist nil nil))
                  ((featurep 'multi-prompt)
                   (mh-funcall-if-exists
                    multi-prompt "," nil prompt mh-alias-alist nil nil))
                  (t (split-string
                      (completing-read prompt mh-alias-alist nil nil) ",")))))
      (if (not mh-alias-expand-aliases-flag)
          (mapconcat 'identity the-answer ", ")
        ;; Loop over all elements, checking if in passwd aliast or blind first
        (mapconcat 'mh-alias-expand the-answer ",\n ")))))

;;;###mh-autoload
(defun mh-alias-minibuffer-confirm-address ()
  "Display the alias expansion if `mh-alias-flash-on-comma' is non-nil."
  (interactive)
  (if (not mh-alias-flash-on-comma)
      ()
    (save-excursion
      (let* ((case-fold-search t)
             (the-name (buffer-substring
                        (progn (skip-chars-backward " \t")(point))
                        ;; This moves over to previous comma, if any
                        (progn (or (and (not (= 0 (skip-chars-backward "^,")))
                                        ;; the skips over leading whitespace
                                        (skip-chars-forward " "))
                                   ;; no comma, then to beginning of word
                                   (skip-chars-backward "^ \t"))
                               ;; In Emacs21, the beginning of the prompt
                               ;; line is accessible, which wasn't the case
                               ;; in emacs20.  Skip over it.
                               (if (looking-at "^[^ \t]+:")
                                   (skip-chars-forward "^ \t"))
                               (skip-chars-forward " ")
                               (point)))))
        (if (assoc-ignore-case the-name mh-alias-alist)
            (message "%s -> %s" the-name (mh-alias-expand the-name))
          ;; Check if if was a single word likely to be an alias
          (if (and (equal mh-alias-flash-on-comma 1)
                   (not (string-match " " the-name)))
              (message "No alias for %s" the-name))))))
  (self-insert-command 1))

(mh-do-in-xemacs (defvar mail-abbrevs))

;;;###mh-autoload
(defun mh-alias-letter-expand-alias ()
  "Expand mail alias before point."
  (mh-alias-reload-maybe)
  (let ((mail-abbrevs mh-alias-alist))
    (mh-funcall-if-exists mail-abbrev-complete-alias))
  (when mh-alias-expand-aliases-flag
    (let* ((end (point))
           (syntax-table (syntax-table))
           (beg (unwind-protect
                    (save-excursion
                      (set-syntax-table mail-abbrev-syntax-table)
                      (backward-word 1)
                      (point))
                  (set-syntax-table syntax-table)))
           (alias (buffer-substring beg end))
           (expansion (mh-alias-expand alias)))
      (delete-region beg end)
      (insert expansion))))

;;; Adding addresses to alias file.

(defun mh-alias-suggest-alias (string)
  "Suggest an alias for STRING."
  (cond
   ((string-match "^<\\(.*\\)>$" string)
    ;; <somename@foo.bar>  -> recurse, stripping brackets.
    (mh-alias-suggest-alias (match-string 1 string)))
   ((string-match "^\\sw+$" string)
    ;; One word -> downcase it.
    (downcase string))
   ((string-match "^\\(\\sw+\\)\\s-+\\(\\sw+\\)$" string)
    ;; Two words -> first.last
    (downcase
     (format "%s.%s" (match-string 1 string) (match-string 2 string))))
   ((string-match "^\\([-a-zA-Z0-9._]+\\)@[-a-zA-z0-9_]+\\.+[a-zA-Z0-9]+$"
                  string)
    ;; email only -> downcase username
    (downcase (match-string 1 string)))
   ((string-match "^\"\\(.*\\)\".*" string)
    ;; "Some name" <somename@foo.bar>  -> recurse -> "Some name"
    (mh-alias-suggest-alias (match-string 1 string)))
   ((string-match "^\\(.*\\) +<.*>$" string)
    ;; Some name <somename@foo.bar>  -> recurse -> Some name
    (mh-alias-suggest-alias (match-string 1 string)))
   ((string-match (concat mh-address-mail-regexp " +(\\(.*\\))$") string)
    ;; somename@foo.bar (Some name)  -> recurse -> Some name
    (mh-alias-suggest-alias (match-string 1 string)))
   ((string-match "^\\(Dr\\|Prof\\)\\.? +\\(.*\\)" string)
    ;; Strip out title
    (mh-alias-suggest-alias (match-string 2 string)))
   ((string-match "^\\(.*\\), +\\(Jr\\.?\\|II+\\)$" string)
    ;; Strip out tails with comma
    (mh-alias-suggest-alias (match-string 1 string)))
   ((string-match "^\\(.*\\) +\\(Jr\\.?\\|II+\\)$" string)
    ;; Strip out tails
    (mh-alias-suggest-alias (match-string 1 string)))
   ((string-match "^\\(\\sw+\\) +[A-Z]\\.? +\\(.*\\)$" string)
    ;; Strip out initials
    (mh-alias-suggest-alias
     (format "%s %s" (match-string 1 string) (match-string 2 string))))
   ((string-match "^\\([^,]+\\), +\\(.*\\)$" string)
    ;; Reverse order of comma-separated fields
    (mh-alias-suggest-alias
     (format "%s %s" (match-string 2 string) (match-string 1 string))))
   (t
    ;; Output string, with spaces replaced by dots.
    (mh-alias-canonicalize-suggestion string))))

(defun mh-alias-canonicalize-suggestion (string)
  "Process STRING to replace spacess by periods.
First all spaces are replaced by periods. Then every run of consecutive periods
are replaced with a single period. Finally the string is converted to lower
case."
  (with-temp-buffer
    (insert string)
    ;; Replace spaces with periods
    (goto-char (point-min))
    (replace-regexp " +" ".")
    ;; Replace consecutive periods with a single period
    (goto-char (point-min))
    (replace-regexp "\\.\\.+" ".")
    ;; Convert to lower case
    (downcase-region (point-min) (point-max))
    ;; Whew! all done...
    (buffer-string)))

(defun mh-alias-which-file-has-alias (alias file-list)
  "Return the name of writable file which defines ALIAS from list FILE-LIST."
  (save-excursion
    (set-buffer (get-buffer-create mh-temp-buffer))
    (let ((the-list file-list)
          (found))
      (while the-list
        (erase-buffer)
        (when (file-writable-p (car file-list))
          (insert-file-contents (car file-list))
          (if (re-search-forward (concat "^" (regexp-quote alias) ":") nil t)
              (setq found (car file-list)
                    the-list nil)
            (setq the-list (cdr the-list)))))
      found)))

(defun mh-alias-insert-file (&optional alias)
  "Return the alias file to write a new entry for ALIAS in.
Use variable `mh-alias-insert-file' if non-nil, else use AliasFile component
value.
If ALIAS is specified and it already exists, try to return the file that
contains it."
  (cond
   ((and mh-alias-insert-file (listp mh-alias-insert-file))
    (if (not (elt mh-alias-insert-file 1))        ; Only one entry, use it
        (car mh-alias-insert-file)
      (if (or (not alias)
              (string-equal alias (mh-alias-ali alias))) ;alias doesn't exist
          (completing-read "Alias file [press Tab]: "
                           (mapcar 'list mh-alias-insert-file) nil t)
        (or (mh-alias-which-file-has-alias alias mh-alias-insert-file)
            (completing-read "Alias file [press Tab]: "
                             (mapcar 'list mh-alias-insert-file) nil t)))))
   ((and mh-alias-insert-file (stringp mh-alias-insert-file))
    mh-alias-insert-file)
   (t
    ;; writable ones returned from (mh-alias-filenames):
    (let ((autolist (delq nil (mapcar (lambda (file)
                                        (if (and (file-writable-p file)
                                                 (not (string-equal
                                                       file "/etc/passwd")))
                                            file))
                                     (mh-alias-filenames t)))))
      (cond
       ((not autolist)
        (error "No writable alias file.
Set `mh-alias-insert-file' or set AliasFile in your .mh_profile file"))
       ((not (elt autolist 1))        ; Only one entry, use it
        (car autolist))
       ((or (not alias)
            (string-equal alias (mh-alias-ali alias))) ;alias doesn't exist
        (completing-read "Alias file [press Tab]: "
                         (mapcar 'list autolist) nil t))
       (t
        (or (mh-alias-which-file-has-alias alias autolist)
            (completing-read "Alias file [press Tab]: "
                             (mapcar 'list autolist) nil t))))))))

;;;###mh-autoload
(defun mh-alias-address-to-alias (address)
  "Return the ADDRESS alias if defined, or nil."
  (let* ((aliases (mh-alias-ali address t)))
    (if (string-equal aliases address)
        nil                             ; ali returned same string -> no.
      ;; Double-check that we have an individual alias. This means that the
      ;; alias doesn't expand into a list (of which this address is part).
      (car (delq nil (mapcar
                      (function
                       (lambda (alias)
                         (let ((recurse (mh-alias-ali alias nil)))
                           (if (string-match ".*,.*" recurse)
                               nil
                             alias))))
                      (split-string aliases ", +")))))))

;;;###mh-autoload
(defun mh-alias-from-has-no-alias-p ()
  "Return t is From has no current alias set.
In the exceptional situation where there isn't a From header in the message the
function returns nil."
  (mh-alias-reload-maybe)
  (save-excursion
    (if (not (mh-folder-line-matches-show-buffer-p))
        nil                             ;No corresponding show buffer
      (if (eq major-mode 'mh-folder-mode)
          (set-buffer mh-show-buffer))
      (let ((from-header (mh-extract-from-header-value)))
        (and from-header
             (not (mh-alias-address-to-alias from-header)))))))

(defun mh-alias-add-alias-to-file (alias address &optional file)
  "Add ALIAS for ADDRESS in alias FILE without alias check or prompts.
Prompt for alias file if not provided and there is more than one candidate.
If ALIAS matches exactly, prompt to [i]nsert before old value or [a]ppend
after it."
  (if (not file)
      (setq file (mh-alias-insert-file alias)))
  (save-excursion
    (set-buffer (find-file-noselect file))
    (goto-char (point-min))
    (let ((alias-search (concat alias ":"))
          (letter)
          (case-fold-search t))
      (cond
       ;; Search for exact match (if we had the same alias before)
       ((re-search-forward
         (concat "^" (regexp-quote alias-search) " *\\(.*\\)") nil t)
        (let ((answer (read-string
                       (format "Exists for %s; [i]nsert, [a]ppend: "
                               (match-string 1))))
              (case-fold-search t))
          (cond ((string-match "^i" answer))
                ((string-match "^a" answer)
                 (forward-line 1))
                (t
                 (error "Quitting")))))
       ;; No, so sort-in at the right place
       ;; search for "^alias", then "^alia", etc.
       ((eq mh-alias-insertion-location 'sorted)
        (setq letter       (substring alias-search -1)
              alias-search (substring alias-search 0 -1))
        (while (and (not (equal alias-search ""))
                    (not (re-search-forward
                          (concat "^" (regexp-quote alias-search)) nil t)))
          (setq letter       (substring alias-search -1)
                alias-search (substring alias-search 0 -1)))
        ;; Next, move forward to sort alphabetically for following letters
        (beginning-of-line)
        (while (re-search-forward
                (concat "^" (regexp-quote alias-search) "[a-" letter "]")
                nil t)
          (forward-line 1)))
       ((eq mh-alias-insertion-location 'bottom)
        (goto-char (point-max)))
       ((eq mh-alias-insertion-location 'top)
        (goto-char (point-min)))))
    (beginning-of-line)
    (insert (format "%s: %s\n" alias address))
    (save-buffer)))

;;;###mh-autoload
(defun mh-alias-add-alias (alias address)
  "*Add ALIAS for ADDRESS in personal alias file.
Prompts for confirmation if the address already has an alias.
If the alias is already is use, `mh-alias-add-alias-to-file' will prompt."
  (interactive "P\nP")
  (mh-alias-reload-maybe)
  (setq alias (completing-read "Alias: " mh-alias-alist nil nil alias))
  (if (and address (string-match "^<\\(.*\\)>$" address))
      (setq address (match-string 1 address)))
  (setq address (read-string "Address: " address))
  (if (string-match "^<\\(.*\\)>$" address)
      (setq address (match-string 1 address)))
  (let ((address-alias (mh-alias-address-to-alias address))
        (alias-address (mh-alias-expand alias)))
    (if (string-equal alias-address alias)
        (setq alias-address nil))
    (cond
     ((and (equal alias address-alias)
           (equal address alias-address))
      (message "Already defined as: %s" alias-address))
     (address-alias
      (if (y-or-n-p (format "Address has alias %s; set new one? "
                            address-alias))
          (mh-alias-add-alias-to-file alias address)))
     (t
      (mh-alias-add-alias-to-file alias address)))))

;;;###mh-autoload
(defun mh-alias-grab-from-field ()
  "*Add ALIAS for ADDRESS in personal alias file.
Prompts for confirmation if the alias is already in use or if the address
already has an alias."
  (interactive)
  (mh-alias-reload-maybe)
  (save-excursion
    (cond
     ((mh-folder-line-matches-show-buffer-p)
      (set-buffer mh-show-buffer))
     ((and (eq major-mode 'mh-folder-mode)
           (mh-get-msg-num nil))
      (set-buffer (get-buffer-create mh-temp-buffer))
      (insert-file-contents (mh-msg-filename (mh-get-msg-num t))))
     ((eq major-mode 'mh-folder-mode)
      (error "Cursor not pointing to a message")))
    (let* ((address (or (mh-extract-from-header-value)
                        (error "Message has no From: header")))
           (alias (mh-alias-suggest-alias address)))
      (mh-alias-add-alias alias address))))

;;;###mh-autoload
(defun mh-alias-add-address-under-point ()
  "Insert an alias for email address under point."
  (interactive)
  (let ((address (mh-goto-address-find-address-at-point)))
    (if address
        (mh-alias-add-alias nil address)
      (message "No email address found under point."))))

(provide 'mh-alias)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 49879e46-5aa3-4569-bece-e5a58731d690
;;; mh-alias.el ends here
