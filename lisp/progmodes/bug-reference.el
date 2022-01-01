;;; bug-reference.el --- buttonize bug references  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2022 Free Software Foundation, Inc.

;; Author: Tom Tromey <tromey@redhat.com>
;; Created: 21 Mar 2007
;; Keywords: tools

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

;; This file provides minor modes for putting clickable overlays on
;; references to bugs.  A bug reference is text like "PR foo/29292";
;; this is mapped to a URL using a user-supplied format; see
;; `bug-reference-url-format' and `bug-reference-bug-regexp'.  More
;; extensive documentation is in (info "(emacs) Bug Reference").

;; Two minor modes are provided.  One works on any text in the buffer;
;; the other operates only on comments and strings.  By default, the
;; URL link is followed by invoking C-c RET or mouse-2.

;;; Code:

(defgroup bug-reference nil
  "Hyperlinking references to bug reports."
  ;; Somewhat arbitrary, by analogy with eg goto-address.
  :group 'comm)

(defvar bug-reference-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'bug-reference-push-button)
    (define-key map (kbd "C-c RET") 'bug-reference-push-button)
    map)
  "Keymap used by bug reference buttons.")

;; E.g., "https://gcc.gnu.org/PR%s"
(defvar bug-reference-url-format nil
  "Format used to turn a bug number into a URL.
The bug number is supplied as a string, so this should have a single %s.
This can also be a function designator; it is called without arguments
 and should return a string.
It can use `match-string' to get parts matched against
`bug-reference-bug-regexp', specifically:
 1. issue kind (bug, patch, rfe &c)
 2. issue number.

There is no default setting for this, it must be set per file.
If you set it to a symbol in the file Local Variables section,
you need to add a `bug-reference-url-format' property to it:
\(put \\='my-bug-reference-url-format \\='bug-reference-url-format t)
so that it is considered safe, see `enable-local-variables'.")

;;;###autoload
(put 'bug-reference-url-format 'safe-local-variable
     (lambda (s)
       (or (stringp s)
           (and (symbolp s)
                (get s 'bug-reference-url-format)))))

(defcustom bug-reference-bug-regexp
  "\\(\\b\\(?:[Bb]ug ?#?\\|[Pp]atch ?#\\|RFE ?#\\|PR [a-z+-]+/\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)\\)"
  "Regular expression matching bug references.
The first subexpression defines the region of the bug-reference
overlay, i.e., the region being fontified and made clickable in
order to browse the referenced bug in the corresponding project's
issue tracker.

If `bug-reference-url-format' is set to a format string with
single %s placeholder, the second subexpression must match
the (part of the) bug reference which needs to be injected in
place of the %s in order to form the bug's ticket URL.

If `bug-reference-url-format' is a function, the interpretation
of the subexpressions larger than 1 is up to the function.
However, it is checked that the bounds of all matching
subexpressions from 2 to 10 are within the bounds of the
subexpression 1 defining the overlay region.  Larger
subexpressions may also be used by the function but may lay
outside the bounds of subexpressions 1 and then don't contribute
to the highlighted and clickable region."
  :type 'regexp
  ; 24.3: defconst -> defcustom
  ; 28.1: contract about subexpression 1 defines the overlay region.
  :version "28.1")

;;;###autoload
(put 'bug-reference-bug-regexp 'safe-local-variable 'stringp)

(defun bug-reference-set-overlay-properties ()
  "Set properties of bug reference overlays."
  (put 'bug-reference 'evaporate t)
  (put 'bug-reference 'face 'link)
  (put 'bug-reference 'mouse-face 'highlight)
  (put 'bug-reference 'help-echo "mouse-1, C-c RET: visit this bug")
  (put 'bug-reference 'keymap bug-reference-map)
  (put 'bug-reference 'follow-link t))

(bug-reference-set-overlay-properties)

(defun bug-reference--overlays-in (start end)
  "Return bug reference overlays in the region between START and END."
  (let (overlays)
    (dolist (o (overlays-in start end))
      (when (eq (overlay-get o 'category) 'bug-reference)
        (push o overlays)))
    (nreverse overlays)))

(defun bug-reference-unfontify (start end)
  "Remove bug reference overlays from the region between START and END."
  (mapc #'delete-overlay (bug-reference--overlays-in start end)))

(defvar bug-reference-prog-mode)

(defvar bug-reference--nonconforming-regexps nil)

(defun bug-reference--overlay-bounds ()
  (let ((m-b1 (match-beginning 1))
        (m-e1 (match-end 1)))
    (if (and m-b1 m-e1
             (catch 'within-bounds
               (let ((i 2))
                 (while (<= i 10)
                   (when (and (match-beginning i)
                              (or (< (match-beginning i) m-b1)
                                  (> (match-end i) m-e1)))
                     (throw 'within-bounds nil))
                   (cl-incf i))
                 t)))
        ;; All groups 2..10 are within bounds.
        (cons m-b1 m-e1)
      ;; The regexp doesn't fulfil the contract of
      ;; bug-reference-bug-regexp, so fall back to the old behavior.
      (unless (member bug-reference-bug-regexp
                      bug-reference--nonconforming-regexps)
        (setq bug-reference--nonconforming-regexps
              (cons bug-reference-bug-regexp
                    bug-reference--nonconforming-regexps))
        (display-warning
         'bug-reference
         (format-message
          "The value of `bug-reference-bug-regexp'

  %S

in buffer %S doesn't conform to the contract specified by its
docstring.  The subexpression 1 should define the region of the
bug-reference overlay and cover all other subexpressions up to
subexpression 10."
          bug-reference-bug-regexp
          (buffer-name))))
      (cons (match-beginning 0) (match-end 0)))))

(defun bug-reference-fontify (start end)
  "Apply bug reference overlays to the region between START and END."
  (save-excursion
    (let* ((beg-line (progn (goto-char start) (line-beginning-position)))
           (end-line (progn (goto-char end) (line-end-position)))
           ;; Reuse existing overlays overlays.
           (overlays (bug-reference--overlays-in beg-line end-line)))
      (goto-char beg-line)
      (while (and (< (point) end-line)
                  (re-search-forward bug-reference-bug-regexp end-line 'move))
        (when (or (not bug-reference-prog-mode)
                  ;; This tests for both comment and string syntax.
                  (nth 8 (syntax-ppss)))
          (let* ((bounds (bug-reference--overlay-bounds))
                 (overlay (or
                           (let ((ov (pop overlays)))
                             (when ov
                               (move-overlay ov (car bounds) (cdr bounds))
                               ov))
                           (let ((ov (make-overlay (car bounds) (cdr bounds)
                                                   nil t nil)))
                             (overlay-put ov 'category 'bug-reference)
                             ov))))
            ;; Don't put a link if format is undefined.
            (when bug-reference-url-format
              (overlay-put overlay 'bug-reference-url
                           (if (stringp bug-reference-url-format)
                               (format bug-reference-url-format
                                       (match-string-no-properties 2))
                             (funcall bug-reference-url-format)))))))
      ;; Delete remaining but unused overlays.
      (dolist (ov overlays)
        (delete-overlay ov)))))

;; Taken from button.el.
(defun bug-reference-push-button (&optional pos _use-mouse-action)
  "Open URL corresponding to the bug reference at POS."
  (interactive
   (list (if (integerp last-command-event) (point) last-command-event)))
  (when (null bug-reference-url-format)
    (user-error
     "You must customize some bug-reference variables; see Emacs info node Bug Reference"))
  (if (and (not (integerp pos)) (eventp pos))
      ;; POS is a mouse event; switch to the proper window/buffer
      (let ((posn (event-start pos)))
        (with-current-buffer (window-buffer (posn-window posn))
          (bug-reference-push-button (posn-point posn) t)))
    ;; POS is just normal position.
    (dolist (o (overlays-at pos))
      ;; It should only be possible to have one URL overlay.
      (let ((url (overlay-get o 'bug-reference-url)))
        (when url
          (browse-url url))))))

(defun bug-reference-maybe-setup-from-vc (url url-rx bug-rx bug-url-fmt)
  (when (string-match url-rx url)
    (setq-local bug-reference-bug-regexp bug-rx)
    (setq-local bug-reference-url-format
                (let (groups)
                  (dotimes (i (/ (length (match-data)) 2))
                    (push (match-string i url) groups))
                  (funcall bug-url-fmt (nreverse groups))))))

(defvar bug-reference--setup-from-vc-alist nil
  "An alist for setting up `bug-reference-mode' based on VC URL.
This is like `bug-reference-setup-from-vc-alist' but generated
from a few default entries, and the value of
`bug-reference-forge-alist'.")

(defvar bug-reference-forge-alist
  '(("github.com"       github    "https")
    ("gitea.com"        gitea     "https")
    ("codeberg.org"     gitea     "https")
    ("gitlab.com"       gitlab    "https")
    ("framagit.org"     gitlab    "https")
    ("salsa.debian.org" gitlab    "https")
    ("sr.ht"            sourcehut "https"))
  "An alist of forge instances.
Each entry has the form (HOST-DOMAIN FORGE-TYPE PROTOCOL).
HOST-DOMAIN is the host- and domain name, e.g., gitlab.com,
salsa.debian.org, or sr.ht.
FORGE-TYPE is the type of the forge, e.g., gitlab, gitea,
sourcehut, or github.
PROTOCOL is the protocol for accessing the forge's issue tracker,
usually \"https\" but for self-hosted forge instances not
accessible via the internet it might also be \"http\".")

(cl-defgeneric bug-reference--build-forge-setup-entry
    (host-domain forge-type protocol)
  "Build an entry for `bug-reference--setup-from-vc-alist'.
HOST-DOMAIN is the host- and domain name, e.g., gitlab.com, or
sr.ht.

FORGE-TYPE is the type of the forge, e.g., gitlab, gitea,
sourcehut, or github.

PROTOCOL is the protocol for accessing the forge's issue tracker,
usually https but for self-hosted forge instances not accessible
via the internet it might also be http.")

;; GitHub: Here #17 may refer to either an issue or a pull request but
;; visiting the issue/17 web page will automatically redirect to the
;; pull/17 page if 17 is a PR.  Explicit user/project#17 links to
;; possibly different projects are also supported.
(cl-defmethod bug-reference--build-forge-setup-entry
  (host-domain (_forge-type (eql github)) protocol)
  `(,(concat "[/@]" (regexp-quote host-domain)
             "[/:]\\([.A-Za-z0-9_/-]+\\)\\.git")
    "\\(\\([.A-Za-z0-9_/-]+\\)?\\(?:#\\)\\([0-9]+\\)\\)\\>"
    ,(lambda (groups)
       (let ((ns-project (nth 1 groups)))
         (lambda ()
           (format "%s://%s/%s/issues/%s"
                   protocol host-domain
                   (or (match-string-no-properties 2) ns-project)
                   (match-string-no-properties 3)))))))

;; GitLab: Here #18 is an issue and !17 is a merge request.  Explicit
;; namespace/project#18 or namespace/project!17 references to possibly
;; different projects are also supported.
(cl-defmethod bug-reference--build-forge-setup-entry
  (host-domain (_forge-type (eql gitlab)) protocol)
  `(,(concat "[/@]" (regexp-quote host-domain)
             "[/:]\\([.A-Za-z0-9_/-]+\\)\\.git")
    "\\(\\([.A-Za-z0-9_/-]+\\)?\\([#!]\\)\\([0-9]+\\)\\)\\>"
    ,(lambda (groups)
       (let ((ns-project (nth 1 groups)))
         (lambda ()
           (format "%s://%s/%s/-/%s/%s"
                   protocol host-domain
                   (or (match-string-no-properties 2) ns-project)
                   (if (string= (match-string-no-properties 3) "#")
                       "issues/"
                     "merge_requests/")
                   (match-string-no-properties 4)))))))

;; Gitea: The systematics is exactly as for Github projects.
(cl-defmethod bug-reference--build-forge-setup-entry
  (host-domain (_forge-type (eql gitea)) protocol)
  `(,(concat "[/@]" (regexp-quote host-domain)
             "[/:]\\([.A-Za-z0-9_/-]+\\)\\.git")
    "\\(\\([.A-Za-z0-9_/-]+\\)?\\(?:#\\)\\([0-9]+\\)\\)\\>"
    ,(lambda (groups)
       (let ((ns-project (nth 1 groups)))
         (lambda ()
           (format "%s://%s/%s/issues/%s"
                   protocol host-domain
                   (or (match-string-no-properties 2) ns-project)
                   (match-string-no-properties 3)))))))

;; Sourcehut: #19 is an issue.  Other project's issues can be
;; referenced as ~user/project#19.
;;
;; Caveat: The code assumes that a project on git.sr.ht or hg.sr.ht
;; has a tracker of the same name on todo.sh.ht.  That's a very common
;; setup but all sr.ht services are loosely coupled, so you can have a
;; repo without tracker, or a repo with a tracker using a different
;; name, etc.  So we can only try to make a good guess.
(cl-defmethod bug-reference--build-forge-setup-entry
  (host-domain (_forge-type (eql sourcehut)) protocol)
  `(,(concat "[/@]\\(?:git\\|hg\\)." (regexp-quote host-domain)
             "[/:]\\(~[.A-Za-z0-9_/-]+\\)")
    "\\(\\(~[.A-Za-z0-9_/-]+\\)?\\(?:#\\)\\([0-9]+\\)\\)\\>"
    ,(lambda (groups)
       (let ((ns-project (nth 1 groups)))
         (lambda ()
           (format "%s://todo.%s/%s/%s"
                   protocol host-domain
                   (or (match-string-no-properties 2) ns-project)
                   (match-string-no-properties 3)))))))

(defun bug-reference--setup-from-vc-alist (&optional rebuild)
  "Compute the `bug-reference--setup-from-vc-alist' value.
If REBUILD is non-nil, compute it again even if has been computed
already.  The value contains a few default entries, and entries
generated from `bug-reference-forge-alist'."
  (if (and bug-reference--setup-from-vc-alist
           (null rebuild))
      bug-reference--setup-from-vc-alist
    (setq bug-reference--setup-from-vc-alist
          `(;; GNU projects on savannah.
            ;;
            ;; Not all of them use debbugs but that doesn't really
            ;; matter because the auto-setup is only performed if
            ;; `bug-reference-url-format' and
            ;; `bug-reference-bug-regexp' aren't set already.
            ("git\\.\\(?:sv\\|savannah\\)\\.gnu\\.org:"
             "\\(\\b\\(?:[Bb]ug ?#?\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)\\)\\>"
             ,(lambda (_) "https://debbugs.gnu.org/%s"))

            ;; Entries for the software forges of
            ;; `bug-reference-forge-alist'.
            ,@(mapcar (lambda (entry)
                        (apply #'bug-reference--build-forge-setup-entry entry))
                      bug-reference-forge-alist)))))

(defvar bug-reference-setup-from-vc-alist nil
  "An alist for setting up `bug-reference-mode' based on VC URL.

Each element has the form (URL-REGEXP BUG-REGEXP URL-FORMAT-FN).

URL-REGEXP is matched against the version control URL of the
current buffer's file.  If it matches, BUG-REGEXP is set as
`bug-reference-bug-regexp'.  URL-FORMAT-FN is a function of one
argument that receives a list of the groups 0 to N of matching
URL-REGEXP against the VCS URL and returns the value to be set as
`bug-reference-url-format'.")

(defun bug-reference-try-setup-from-vc ()
  "Try setting up `bug-reference-mode' based on VC information.
Test each configuration in `bug-reference-setup-from-vc-alist'
and `bug-reference--setup-from-vc-alist' and apply it if
applicable."
  (when-let ((file-or-dir (or buffer-file-name
                              ;; Catches modes such as vc-dir and Magit.
                              default-directory))
             (backend (vc-responsible-backend file-or-dir t))
             (url (seq-some (lambda (remote)
                              (ignore-errors
                                (vc-call-backend backend 'repository-url
                                                 file-or-dir remote)))
                            '("upstream" nil))))
    (seq-some (lambda (config)
                (apply #'bug-reference-maybe-setup-from-vc url config))
              (append bug-reference-setup-from-vc-alist
                      (bug-reference--setup-from-vc-alist)))))

(defvar bug-reference-setup-from-mail-alist
  `((,(regexp-opt '("emacs" "auctex" "gnus" "tramp" "orgmode") 'words)
     ,(regexp-opt '("@debbugs.gnu.org" "-devel@gnu.org"
                    ;; List-Id of Gnus devel mailing list.
                    "ding.gnus.org"))
     "\\(\\b[Bb]ug ?#?\\([0-9]+\\(?:#[0-9]+\\)?\\)\\)"
     "https://debbugs.gnu.org/%s"))
  "An alist for setting up `bug-reference-mode' in mail modes.

This takes action if `bug-reference-mode' is enabled in group and
message buffers of Emacs mail clients.  Currently, Gnus and Rmail
are supported.

Each element has the form

  (GROUP-REGEXP HEADER-REGEXP BUG-REGEXP URL-FORMAT)

GROUP-REGEXP is a regexp matched against the current mail folder
or newsgroup name.  HEADER-REGEXP is a regexp matched against the
From, To, Cc, Newsgroup, and List-ID header values of the current
mail or newsgroup message.  If any of those matches, BUG-REGEXP
is set as `bug-reference-bug-regexp' and URL-FORMAT is set as
`bug-reference-url-format'.

Note: In Gnus, if a summary buffer has been set up based on
GROUP-REGEXP, all article buffers opened from there will get the
same `bug-reference-url-format' and `bug-reference-url-format'.")

(defvar gnus-newsgroup-name)

(defun bug-reference-maybe-setup-from-mail (group header-values)
  "Set up according to mail GROUP or HEADER-VALUES.
Group is a mail group/folder name and HEADER-VALUES is a list of
mail header values, e.g., the values of From, To, Cc, List-ID,
and Newsgroup.

If any GROUP-REGEXP or HEADER-REGEXP of
`bug-reference-setup-from-mail-alist' matches GROUP or any
element in HEADER-VALUES, the corresponding BUG-REGEXP and
URL-FORMAT are set."
  (catch 'setup-done
    (dolist (config bug-reference-setup-from-mail-alist)
      (when (or
             (and group
                  (car config)
                  (string-match-p (car config) group))
             (and header-values
                  (nth 1 config)
                  (catch 'matching-header
                    (dolist (h header-values)
                      (when (and h (string-match-p (nth 1 config) h))
                        (throw 'matching-header t))))))
        (setq-local bug-reference-bug-regexp (nth 2 config))
        (setq-local bug-reference-url-format (nth 3 config))
        (throw 'setup-done t)))))

(defun bug-reference-try-setup-from-gnus ()
  "Try setting up `bug-reference-mode' based on Gnus group or article.
Test each configuration in `bug-reference-setup-from-mail-alist'
and set it if applicable."
  (when (and (derived-mode-p 'gnus-summary-mode)
             (bound-and-true-p gnus-newsgroup-name))
    ;; Gnus reuses its article buffer so we have to check whenever the
    ;; article changes.
    (add-hook 'gnus-article-prepare-hook
              #'bug-reference--try-setup-gnus-article)
    (bug-reference-maybe-setup-from-mail gnus-newsgroup-name nil)))

(defvar gnus-article-buffer)
(defvar gnus-original-article-buffer)
(defvar gnus-summary-buffer)
(defvar bug-reference-mode)

(defun bug-reference--try-setup-gnus-article ()
  (when (and bug-reference-mode ;; Only if enabled in article buffers.
             (derived-mode-p
              'gnus-article-mode
              ;; Apparently, gnus-article-prepare-hook is run in the
              ;; summary buffer...
              'gnus-summary-mode)
             gnus-article-buffer
             gnus-original-article-buffer
             (buffer-live-p (get-buffer gnus-article-buffer))
             (buffer-live-p (get-buffer gnus-original-article-buffer)))
    (with-current-buffer gnus-article-buffer
      (catch 'setup-done
        ;; Copy over the values from the summary buffer.
        (when (and gnus-summary-buffer
                   (buffer-live-p gnus-summary-buffer))
          (setq-local bug-reference-bug-regexp
                      (with-current-buffer gnus-summary-buffer
                        bug-reference-bug-regexp))
          (setq-local bug-reference-url-format
                      (with-current-buffer gnus-summary-buffer
                        bug-reference-url-format))
          (when (and bug-reference-bug-regexp
                     bug-reference-url-format)
            (throw 'setup-done t)))
        ;; If the summary had no values, try setting according to
        ;; the values of the From, To, and Cc headers.
        (let (header-values)
          (with-current-buffer
              (get-buffer gnus-original-article-buffer)
            (save-excursion
              (goto-char (point-min))
              ;; The Newsgroup is omitted because we already matched
              ;; based on group name in the summary buffer.
              (dolist (field '("list-id" "to" "from" "cc"))
                (let ((val (mail-fetch-field field)))
                  (when val
                    (push val header-values))))))
          (bug-reference-maybe-setup-from-mail nil header-values))))))

(defun bug-reference-try-setup-from-rmail ()
  "Try setting up `bug-reference-mode' from the current rmail mail.
Guesses suitable `bug-reference-bug-regexp' and
`bug-reference-url-format' values by matching the current Rmail
file's name against GROUP-REGEXP and the values of List-Id, To,
From, and Cc against HEADER-REGEXP in
`bug-reference-setup-from-mail-alist'."
  (when (and bug-reference-mode
             (derived-mode-p 'rmail-mode))
    (let (header-values)
      (save-excursion
        (goto-char (point-min))
        (dolist (field '("list-id" "to" "from" "cc"))
          (let ((val (mail-fetch-field field)))
            (when val
              (push val header-values)))))
      (bug-reference-maybe-setup-from-mail
       (buffer-file-name) header-values))))

(defvar bug-reference-setup-from-irc-alist
  `((,(concat "#" (regexp-opt '("emacs" "gnus" "org-mode" "rcirc"
                                "erc") 'words))
     "Libera.Chat"
     "\\(\\b[Bb]ug ?#?\\([0-9]+\\(?:#[0-9]+\\)?\\)\\)"
     "https://debbugs.gnu.org/%s"))
  "An alist for setting up `bug-reference-mode' in IRC modes.

This takes action if `bug-reference-mode' is enabled in IRC
channels using one of Emacs' IRC clients.  Currently, rcirc and
ERC are supported.

Each element has the form

  (CHANNEL-REGEXP NETWORK-REGEXP BUG-REGEXP URL-FORMAT)

CHANNEL-REGEXP is a regexp matched against the current IRC
channel name (e.g. #emacs).  NETWORK-REGEXP is matched against
the IRC network name (e.g. Libera.Chat).  Both entries are
optional.  If all given entries match, BUG-REGEXP is set as
`bug-reference-bug-regexp' and URL-FORMAT is set as
`bug-reference-url-format'.")

(defun bug-reference-maybe-setup-from-irc (channel network)
  "Set up according to IRC CHANNEL or NETWORK.
CHANNEL is an IRC channel name (or generally a target, i.e., it
could also be a user name) and NETWORK is that channel's network
name.

If any `bug-reference-setup-from-irc-alist' entry's
CHANNEL-REGEXP and NETWORK-REGEXP match CHANNEL and NETWORK, the
corresponding BUG-REGEXP and URL-FORMAT are set."
  (catch 'setup-done
    (dolist (config bug-reference-setup-from-irc-alist)
      (let ((channel-rx (car config))
            (network-rx (nth 1 config)))
        (when (and
               ;; One of both has to be given.
               (or channel-rx network-rx)
               ;; The args have to be set.
               channel network)
          (when (and
                 (or (null channel-rx)
                     (string-match-p channel-rx channel))
                 (or (null network-rx)
                     (string-match-p network-rx network)))
            (setq-local bug-reference-bug-regexp (nth 2 config))
            (setq-local bug-reference-url-format (nth 3 config))
            (throw 'setup-done t)))))))

(defvar rcirc-target)
(defvar rcirc-server-buffer)
(defvar rcirc-server)

(defun bug-reference-try-setup-from-rcirc ()
  "Try setting up `bug-reference-mode' based on rcirc channel and server.
Test each configuration in `bug-reference-setup-from-irc-alist'
and set it if applicable."
  (when (derived-mode-p 'rcirc-mode)
    (bug-reference-maybe-setup-from-irc
     rcirc-target
     (and rcirc-server-buffer
          (buffer-live-p rcirc-server-buffer)
          (with-current-buffer rcirc-server-buffer
            rcirc-server)))))

(declare-function erc-format-target "erc")
(declare-function erc-network-name "erc-networks")

(defun bug-reference-try-setup-from-erc ()
  "Try setting up `bug-reference-mode' based on ERC channel and server.
Test each configuration in `bug-reference-setup-from-irc-alist'
and set it if applicable."
  (when (derived-mode-p 'erc-mode)
    (bug-reference-maybe-setup-from-irc
     (erc-format-target)
     (erc-network-name))))

(defvar bug-reference-auto-setup-functions
  (list #'bug-reference-try-setup-from-vc
        #'bug-reference-try-setup-from-gnus
        #'bug-reference-try-setup-from-rmail
        #'bug-reference-try-setup-from-rcirc
        #'bug-reference-try-setup-from-erc)
  "Functions trying to auto-setup `bug-reference-mode'.
These functions are run after `bug-reference-mode' has been
activated in a buffer and try to guess suitable values for
`bug-reference-bug-regexp' and `bug-reference-url-format'.  Their
guesswork is based on these variables:

- `bug-reference-setup-from-vc-alist' for guessing based on
  version control, e.g., URL of repository.
- `bug-reference-setup-from-mail-alist' for guessing based on
  mail group names or mail header values.
- `bug-reference-setup-from-irc-alist' for guessing based on IRC
  channel or network names.")

(defun bug-reference--run-auto-setup ()
  (when (or bug-reference-mode
            bug-reference-prog-mode)
    ;; Automatic setup only if the variables aren't already set, e.g.,
    ;; by a local variables section in the file.
    (unless (and bug-reference-bug-regexp
                 bug-reference-url-format)
      (with-demoted-errors
          "Error during bug-reference auto-setup: %S"
        (run-hook-with-args-until-success
         'bug-reference-auto-setup-functions)))))

;;;###autoload
(define-minor-mode bug-reference-mode
  "Toggle hyperlinking bug references in the buffer (Bug Reference mode)."
  :after-hook (bug-reference--run-auto-setup)
  (if bug-reference-mode
      (jit-lock-register #'bug-reference-fontify)
    (jit-lock-unregister #'bug-reference-fontify)
    (save-restriction
      (widen)
      (bug-reference-unfontify (point-min) (point-max)))))

(defun bug-reference-mode-force-auto-setup ()
  "Enable `bug-reference-mode' and force auto-setup.
Enabling `bug-reference-mode' runs its auto-setup only if
`bug-reference-bug-regexp' and `bug-reference-url-format' are not
set already.  This function sets the latter to nil
buffer-locally, so that the auto-setup will always run.

This is mostly intended for MUA modes like `rmail-mode' where the
same buffer is re-used for different contexts."
  (setq-local bug-reference-url-format nil)
  (bug-reference-mode))

;;;###autoload
(define-minor-mode bug-reference-prog-mode
  "Like `bug-reference-mode', but only buttonize in comments and strings."
  :after-hook (bug-reference--run-auto-setup)
  (if bug-reference-prog-mode
      (jit-lock-register #'bug-reference-fontify)
    (jit-lock-unregister #'bug-reference-fontify)
    (save-restriction
      (widen)
      (bug-reference-unfontify (point-min) (point-max)))))

(provide 'bug-reference)
;;; bug-reference.el ends here
