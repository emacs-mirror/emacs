;;; send-to.el --- send files to applications or services  -*- lexical-binding: t -*-

;; Copyright (C) 1993-2026 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: send, applications
;; Package: emacs

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

;; This package provides commands to facilitate sending files to
;; external applications or services.
;;
;; Enable `context-menu-mode' to get a "Send to..." context menu item.
;;
;; `send-to' uses `send-to-handlers' to pick which file(s) to send
;; and what handler to use.

;;; Code:

(require 'map)

(declare-function ns-send-items "nsfns.m")
(declare-function dired-between-files "dired")
(declare-function dired-get-filename "dired")
(declare-function dired-get-marked-files "dired")
(declare-function dired-move-to-filename "dired")
(declare-function shell-command-do-open "dired-aux")

(defgroup send-to nil
  "Send files or text to external applications or services."
  :group 'external
  :version "31.1")

(defvar-local send-to-handlers '(((:supported . send-to--ns-supported-p)
                                  (:collect . send-to--collect-items)
                                  (:send . send-to--ns-send-items))
                                 ((:supported . send-to--open-externally-supported-p)
                                  (:collect . send-to--collect-items)
                                  (:send . send-to--open-externally)))
  "A list of handlers that may be able to send files to applications or services.

Sending is handled by the first supported handler from `send-to-handlers' whose
`:supported' function returns non-nil.

Handlers are of the form:

\((:supported . `is-supported-p')
 (:collect . `collect-items')
 (:send . `send-items'))

\(defun is-supported-p ()
  \"Return non-nil for platform supporting send capability.\"
  ...)

\(defun collect-items ()
  \"Return a list of items to be sent.

Items are strings and will be sent as text unless they are local file
paths known to exist. In these instances, files will be sent instead.\"
  ...)

\(defun send-to--send-items (items)
  \"Send ITEMS.\"
  ...)")

;;;###autoload
(defun send-to-supported-p ()
  "Return non-nil for platforms where `send-to' is supported."
  (funcall (map-elt (send-to--resolve-handler) :supported)))

;;;###autoload
(defun send-to (&optional items)
  "Send file(s) or region text to (non-Emacs) applications or services.

Sending is handled by the first supported handler from `send-to-handlers'.

ITEMS list is also populated by the resolved handler, but can be
explicitly overridden."
  (interactive)
  (let ((handler (send-to--resolve-handler)))
    (unless handler
      (error "No handler found"))
    (dolist (item items)
      (unless (stringp item)
        (error "Item must be a string: %s" item)))
    (funcall (or (map-elt handler :send)
                 (error "Handler without :send capability"))
             (or items
                 (when (map-elt handler :collect)
                   (funcall (map-elt handler :collect)))
                 (user-error "Nothing to send")))))

(defun send-to--format-items (items)
  "Format ITEMS into a user-presentable message string."
  (truncate-string-to-width
   (string-join
    (seq-map (lambda (item)
               (if (and (stringp item)
                        ;; Ignore url-handler-mode
                        (and (file-local-name item)
                             (file-exists-p item)))
                   (format "\"%s\"" (file-name-nondirectory item))
                 (format "\"%s\"" (truncate-string-to-width
                                   item 35 nil nil "…"))))
             items) " ") 70 nil nil "…"))

(defun send-to--dired-filenames-in-region ()
  "If in a `dired' buffer, return region files.  nil otherwise."
  (when (and (derived-mode-p 'dired-mode)
             (use-region-p))
    (let* ((start (region-beginning))
           (end (region-end))
           (marked-files (dired-get-marked-files nil nil nil t))
           (active-marks-p (if (= (length marked-files) 1)
                               nil ;; File found at point (not marked)
                             (not (seq-empty-p marked-files))))
           (filenames))
      (when active-marks-p
        (user-error "Either mark `dired' files or select a region, but not both"))
      (save-excursion
        (save-restriction
          (goto-char start)
          (while (< (point) end)
            ;; Skip non-file lines.
            (while (and (< (point) end) (dired-between-files))
              (forward-line 1))
            (when (and (dired-get-filename nil t)
                       ;; Filename must be in region.
                       (< (save-excursion
                            (forward-line 0)
                            (dired-move-to-filename))
                          end))
              (setq filenames (append filenames (list (dired-get-filename nil t)))))
            (forward-line 1))))
      filenames)))

(defun send-to--resolve-handler ()
  "Return first supported handler from `send-to-handlers'."
  (seq-find (lambda (handler)
              (funcall (map-elt handler :supported)))
            send-to-handlers))

(defun send-to--ns-supported-p ()
  "Return non-nil for macOS platform supporting send capability."
  (and (featurep 'ns)
       (display-graphic-p)
       (fboundp 'ns-send-items)))

(defun send-to--ns-send-items (items)
  "Send ITEMS on macOS."
  (ns-send-items
   (seq-map #'send-to--convert-item-to-filename items)))

(defun send-to--open-externally-supported-p ()
  "Return non-nil for platforms supporting open externally capability."
  (unless (fboundp 'shell-command-do-open)
    (require 'dired-aux))
  (fboundp 'shell-command-do-open))

(defun send-to--open-externally (items)
  "Open ITEMS externally (using a non-Emacs application)."
  (unless (fboundp 'shell-command-do-open)
    (require 'dired-aux))
  (when (y-or-n-p (format "Open externally: %s ?"
                          (send-to--format-items items)))
    (shell-command-do-open items)))

(defun send-to--convert-item-to-filename (item)
  "Convert ITEM to a filename.

Unless ITEM is a verifiable filename, save its content to a file and
return its new timestamped filename."
  (if (and (file-local-name item) ;; Ignore url-handler-mode
           (file-exists-p item))
      item
    (let ((filename (concat temporary-file-directory
                            (format-time-string "%F_%H.%M.%S") ".txt")))
      (with-temp-file filename
        (insert item))
      filename)))

(defun send-to--collect-items ()
  "Build a list of items to send based on default context.

From a `dired' buffer, chosen items are based on either of these being
active:

  - Marked files
  - Files in region.
  - File at point.

From any other buffer, either of these two, in order of preference:

  - Active region text.
  - Thing at point (via `existing-filename').
  - Buffer file."
  (cond ((derived-mode-p 'dired-mode)
         (or
          (send-to--dired-filenames-in-region)
          (dired-get-marked-files)))
        ((use-region-p)
         (list (buffer-substring-no-properties
                (region-beginning)
                (region-end))))
        ((thing-at-point 'existing-filename)
         (thing-at-point 'existing-filename))
        ((buffer-file-name)
         (list (buffer-file-name)))))

(provide 'send-to)

;;; send-to.el ends here
