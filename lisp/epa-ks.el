;;; epa-ks.el --- EasyPG Key Server Client -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: Philip K. <philipk@posteo.net>
;; Keywords: PGP, GnuPG

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

;; Keyserver client in Emacs.

;;; Code:

(require 'cl-lib)
(require 'epa)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)
(require 'url-http)

(defgroup epa-ks nil
  "The EasyPG Assistant Keyserver client."
  :version "28.1"
  :group 'epa)

(defcustom epa-keyserver "pgp.mit.edu"
  "Domain of keyserver.

This is used by `epa-ks-lookup-key', for looking up public keys."
  :type '(choice :tag "Keyserver"
                 (repeat :tag "Random pool"
                         (string :tag "Keyserver address"))
                 (const "keyring.debian.org")
                 (const "keys.gnupg.net")
                 (const "keyserver.ubuntu.com")
                 (const "pgp.mit.edu")
                 (const "pool.sks-keyservers.net")
                 (const "zimmermann.mayfirst.org")
                 (string :tag "Custom keyserver"))
  :version "28.1")

(cl-defstruct epa-ks-key
  "Structure to hold key data."
  id algo len created expires names flags)

(cl-defstruct epa-ks-name
  "Structure to hold user associated with keys data."
  uid created expires flags)

(defvar epa-ks-last-query nil
  "List of arguments to pass to `epa-search-keys'.
This is used when reverting a buffer to restart search.")

(defvar epa-ks-search-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "f") #'epa-ks-mark-key-to-fetch)
    (define-key map (kbd "i") #'epa-ks-inspect-key-to-fetch)
    (define-key map (kbd "u") #'epa-ks-unmark-key-to-fetch)
    (define-key map (kbd "x") #'epa-ks-do-key-to-fetch)
    map))

(define-derived-mode epa-ks-search-mode tabulated-list-mode "Keyserver"
  "Major mode for listing public key search results."
  (buffer-disable-undo)
  (setq tabulated-list-format [("ID" 8 t)
                               ("Algo." 5 nil)
                               ("Created" 10 t)
                               ("Expires" 10 t)
                               ("User" 0 t)]
        tabulated-list-sort-key '("User" . nil)
        tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook
            #'epa-ks--restart-search
            nil t)
  (tabulated-list-init-header))

(defun epa-ks-inspect-key-to-fetch ()
  "Display full ID of key under point in the minibuffer."
  (interactive)
  (message "Full ID: %s" (epa-ks-key-id (car (tabulated-list-get-id)))))

(defun epa-ks-unmark-key-to-fetch ()
  "Remove fetch mark for key under point.

If a region is active, unmark all keys in active region."
  (interactive)
  (epa-ks-mark-key-to-fetch ""))

(defun epa-ks-mark-key-to-fetch (tag)
  "Add fetch-mark to key under point.

If a region is active, mark all keys in active region.

When all keys have been selected, use \\[epa-ks-do-key-to-fetch] to
actually import the keys.

When called interactively, `epa-ks-mark-key-to-fetch' will always
add a \"F\" tag.  Non-interactivly the tag must be specified by
setting the TAG parameter."
  (interactive (list "F"))
  (if (region-active-p)
      (save-mark-and-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (1- (region-end)))
          (goto-char (point-min))
          (while (not (eobp))
            (tabulated-list-put-tag tag t))))
    (tabulated-list-put-tag tag t)))

(defun epa-ks-do-key-to-fetch ()
  "Fetch all marked keys from keyserver and import them.

Keys are marked using `epa-ks-mark-key-to-fetch'."
  (interactive)
  (save-excursion
    (let (keys)
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at-p (rx bol "F"))
          (push (epa-ks-key-id (car (tabulated-list-get-id)))
                keys))
        (forward-line))
      (when (yes-or-no-p (format "Proceed with fetching all %d key(s)? "
                                 (length keys))))
      (dolist (id keys)
        (epa-ks--fetch-key id))))
  (tabulated-list-clear-all-tags))

(defun epa-ks--query-url (query exact)
  "Return URL for QUERY.
If EXACT is non-nil, don't accept approximate matches."
  (format "https://%s/pks/lookup?%s"
          (cond ((null epa-keyserver)
                 (user-error "Empty keyserver pool"))
                ((listp epa-keyserver)
                 (seq-random-elt epa-keyserver))
                ((stringp epa-keyserver)
                 epa-keyserver)
                ((error "Invalid type for `epa-keyserver'")))
          (url-build-query-string
           (append `(("search" ,query)
                     ("options" "mr")
                     ("op" "index"))
                   (and exact '(("exact" "on")))))))

(defun epa-ks--fetch-key (id)
  "Send request to import key with specified ID."
  (url-retrieve
   (epa-ks--query-url (concat "0x" (url-hexify-string id)) t)
   (lambda (status)
     (when (plist-get status :error)
       (error "Request failed: %s"
              (caddr (assq (caddr (plist-get status :error))
                           url-http-codes))))
     (forward-paragraph)
     (save-excursion
       (goto-char (point-max))
       (while (memq (char-before) '(?\s ?\t ?\n))
         (forward-char -1))
       (delete-region (point) (point-max)))
     (let ((epa-popup-info-window nil))
       (epa-import-armor-in-region (point) (point-max)))
     (kill-buffer))))

(defun epa-ks--display-keys (buf keys)
  "Prepare KEYS for `tabulated-list-mode', for buffer BUF.

KEYS is a list of `epa-ks-key' structures, as parsed by
`epa-ks-parse-result'."
  (when (buffer-live-p buf)
    (let (entries)
      (dolist (key keys)
        (dolist (name (epa-ks-key-names key))
          (push (list (cons key name)
                      (vector
                       (substring (epa-ks-key-id key) -8)
                       (cdr (epa-ks-key-algo key))
                       (if (epa-ks-key-created key)
                           (format-time-string "%F" (epa-ks-key-created key))
                         "N/A")
                       (if (epa-ks-key-expires key)
                           (let* ((date (epa-ks-key-expires key))
                                  (str (format-time-string "%F" date)))
                             (when (< 0 (time-to-seconds (time-since date)))
                               (setq str (propertize str 'face
                                                     'font-lock-warning-face)))
                             str)
                         (propertize "N/A" 'face 'shadow))
                       (decode-coding-string
                        (epa-ks-name-uid name)
                        (select-safe-coding-system (epa-ks-name-uid name)
                                                   nil 'utf-8))))
                entries)))
      (with-current-buffer buf
        (setq tabulated-list-entries entries)
        (tabulated-list-print t t))
      (message "Press `f' to mark a key, `x' to fetch all marked keys."))))

(defun epa-ks--restart-search ()
  (when epa-ks-last-query
    (apply #'epa-search-keys epa-ks-last-query)))

;;;###autoload
(defun epa-search-keys (query exact)
  "Ask a keyserver for all keys matching QUERY.

The keyserver to be used is specified by `epa-keyserver'.

If EXACT is non-nil (interactively, prefix argument), require
exact matches.

Note that the request may fail if the query is not specific
enough, since keyservers have strict timeout settings."
  (interactive (list (read-string "Search for: ")
                     current-prefix-arg))
  (when (string-empty-p query)
    (user-error "No query"))
  (let ((buf (get-buffer-create "*Key search*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (epa-ks-search-mode))
    (url-retrieve
     (epa-ks--query-url query exact)
     (lambda (status)
       (when (plist-get status :error)
         (when buf
           (kill-buffer buf))
         (error "Request failed: %s"
                (caddr (assq (caddr (plist-get status :error))
                             url-http-codes))))
       (goto-char (point-min))
       (while (search-forward "\r\n" nil t)
         (replace-match "\n" t t))
       (goto-char (point-min))
       (re-search-forward "\n\n")
       (let (keys)
         (save-match-data
           (setq keys (epa-ks--parse-buffer))
           (kill-buffer (current-buffer)))
         (when buf
           (epa-ks--display-keys buf keys) keys))))
    (pop-to-buffer buf)
    (setq epa-ks-last-query (list query exact)))
  (message "Searching keys..."))

(defun epa-ks--parse-buffer ()
  ;; parse machine readable response according to
  ;; https://tools.ietf.org/html/draft-shaw-openpgp-hkp-00#section-5.2
  (when (looking-at (rx bol "info:" (group (+ digit))
                        ":" (* digit) eol))
    (unless (string= (match-string 1) "1")
      (error "Unsupported keyserver version")))
  (forward-line 1)
  (let (key keys)
    (while (and (not (eobp))
                (not (looking-at "[ \t]*\n")))
      (cond
       ((looking-at (rx bol "pub:" (group (+ alnum))
                        ":" (group (* digit))
                        ":" (group (* digit))
                        ":" (group (* digit))
                        ":" (group (* digit))
                        ":" (group (* (any ?r ?d ?e)))
                        eol))
        (setq key
              (make-epa-ks-key
               :id (match-string 1)
               :algo
               (and (match-string 2)
                    (not (string-empty-p (match-string 2)))
                    (assoc (string-to-number (match-string 2))
                           epg-pubkey-algorithm-alist))
               :len
               (and (match-string 3)
                    (not (string-empty-p (match-string 3)))
                    (string-to-number (match-string 3)))
               :created
               (and  (match-string 4)
                     (not (string-empty-p (match-string 4)))
                     (seconds-to-time
                      (string-to-number (match-string 4))))
               :expires
               (and (match-string 5)
                    (not (string-empty-p (match-string 5)))
                    (seconds-to-time
                     (string-to-number (match-string 5))))
               :flags
               (mapcar (lambda (flag)
                         (cdr (assq flag '((?r revoked)
                                           (?d disabled)
                                           (?e expired)))))
                       (match-string 6))))
        (push key keys))
       ((looking-at (rx bol "uid:" (group (+ (not ":")))
                        ":" (group (* digit))
                        ":" (group (* digit))
                        ":" (group (* (any ?r ?d ?e)))
                        eol))
        (push (make-epa-ks-name
               :uid (url-unhex-string (match-string 1) t)
               :created
               (and (match-string 2)
                    (not (string-empty-p (match-string 2)))
                    (decode-time (seconds-to-time
                                  (string-to-number
                                   (match-string 2)))))
               :expires
               (and (match-string 3)
                    (not (string-empty-p (match-string 3)))
                    (decode-time (seconds-to-time
                                  (string-to-number
                                   (match-string 3)))))
               :flags
               (mapcar (lambda (flag)
                         (cdr (assq flag '((?r revoked)
                                           (?d disabled)
                                           (?e expired)))))
                       (match-string 4)))
              (epa-ks-key-names key)))
       ((looking-at-p (rx bol "uat:"))
        ;; user attribute fields are ignored
        nil)
       (t (error "Invalid server response")))
      (forward-line))
    keys))

;;; epa-ks.el ends here
