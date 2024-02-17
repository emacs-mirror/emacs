;;; ol-man.el --- Links to man pages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2024 Free Software Foundation, Inc.
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: outlines, hypermedia, calendar, wp
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

(require 'org-macs)
(org-assert-version)

(require 'ol)

(org-link-set-parameters "man"
			 :follow #'org-man-open
			 :export #'org-man-export
			 :store #'org-man-store-link)

(defcustom org-man-command 'man
  "The Emacs command to be used to display a man page."
  :group 'org-link
  :type '(choice (const man) (const woman)))

(declare-function Man-translate-references "man" (ref))
(defun org-man-open (path _)
  "Visit the manpage on PATH.
PATH should be a topic that can be thrown at the man command.
If PATH contains extra ::STRING which will use `occur' to search
matched strings in man buffer."
  (require 'man) ; For `Man-translate-references'
  (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?$" path)
  (let* ((command (match-string 1 path))
         ;; FIXME: Remove after we drop Emacs 29 support.
         ;; Working around security bug #66390.
         (command (if (not (equal (Man-translate-references ";id") ";id"))
                      ;; We are on Emacs that escapes man command args
                      ;; (see Emacs commit 820f0793f0b).
                      command
                    ;; Older Emacs without the fix - escape the
                    ;; arguments ourselves.
                    (mapconcat 'identity
                               (mapcar #'shell-quote-argument
                                       (split-string command "\\s-+"))
                               " ")))
         (search (match-string 2 path))
         (buffer (funcall org-man-command command)))
    (when search
      (with-current-buffer buffer
        (goto-char (point-min))
        (unless (search-forward search nil t)
          (let ((process (get-buffer-process buffer)))
            (while (process-live-p process)
              (accept-process-output process)))
          (goto-char (point-min))
          (search-forward search))
        (forward-line -1)
        (let ((point (point)))
          (let ((window (get-buffer-window buffer)))
            (set-window-point window point)
            (set-window-start window point)))))))

(defun org-man-store-link ()
  "Store a link to a README file."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, we do make this link
    (let* ((page (org-man-get-page-name))
           (link (concat "man:" page))
           (description (format "Manpage for %s" page)))
      (org-link-store-props
       :type "man"
       :link link
       :description description))))

(defun org-man-get-page-name ()
  "Extract the page name from the buffer name."
  ;; This works for both `Man-mode' and `woman-mode'.
  (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name))
    (error "Cannot create link to this man page")))

(defun org-man-export (link description format)
  "Export a man page link from Org files."
  (let ((path (format "http://man.he.net/?topic=%s&section=all" link))
	(desc (or description link)))
    (cond
     ((eq format 'html) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "%s (%s)" desc path))
     ((eq format 'md) (format "[%s](%s)" desc path))
     (t path))))

(provide 'ol-man)

;;; ol-man.el ends here
