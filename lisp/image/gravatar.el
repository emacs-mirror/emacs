;;; gravatar.el --- Get Gravatars -*- lexical-binding: t -*-

;; Copyright (C) 2010-2020 Free Software Foundation, Inc.

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm, multimedia

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

;;; Code:

(require 'url)
(require 'url-cache)
(eval-when-compile
  (require 'subr-x))

(defgroup gravatar nil
  "Gravatars."
  :version "24.1"
  :group 'comm)

(defcustom gravatar-automatic-caching t
  "Whether to cache retrieved gravatars."
  :type 'boolean
  :group 'gravatar)

(defcustom gravatar-cache-ttl 2592000
  "Time to live in seconds for gravatar cache entries.
If a requested gravatar has been cached for longer than this, it
is retrieved anew.  The default value is 30 days."
  :type 'integer
  ;; Restricted :type to number of seconds.
  :version "27.1"
  :group 'gravatar)

(defcustom gravatar-rating "g"
  "Most explicit Gravatar rating level to allow.
Some gravatars are rated according to how suitable they are for
different audiences.  The supported rating levels are, in order
of increasing explicitness, the following:

\"g\"  - Suitable for any audience.
\"pg\" - May contain rude gestures, provocatively dressed
       individuals, mild profanity, or mild violence.
\"r\"  - May contain harsh profanity, intense violence, nudity,
       or hard drug use.
\"x\"  - May contain hardcore sexual imagery or extremely
       disturbing violence.

Each level covers itself as well as all less explicit levels.
For example, setting this variable to \"pg\" will allow gravatars
rated either \"g\" or \"pg\"."
  :type '(choice (const :tag "General Audience" "g")
                 (const :tag "Parental Guidance" "pg")
                 (const :tag "Restricted" "r")
                 (const :tag "Explicit" "x"))
  ;; Restricted :type to ratings recognized by Gravatar.
  :version "27.1"
  :group 'gravatar)

(defcustom gravatar-size 32
  "Gravatar size in pixels to request.
Valid sizes range from 1 to 2048 inclusive.  If nil, use the
Gravatar default (usually 80)."
  :type '(choice (const :tag "Gravatar default" nil)
                 (integer :tag "Pixels"))
  :version "27.1"
  :group 'gravatar)

(defcustom gravatar-default-image "404"
  "Default gravatar to use when none match the request.
This happens when no gravatar satisfying `gravatar-rating' exists
for a given email address.  The following options are supported:

nil         - Default placeholder.
\"404\"       - No placeholder.
\"mp\"        - Mystery Person: generic avatar outline.
\"identicon\" - Geometric pattern based on email address.
\"monsterid\" - Generated \"monster\" with different colors, faces, etc.
\"wavatar\"   - Generated faces with different features and backgrounds.
\"retro\"     - Generated 8-bit arcade-style pixelated faces.
\"robohash\"  - Generated robot with different colors, faces, etc.
\"blank\"     - Transparent PNG image.
URL         - Custom image URL."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "None" "404")
                 (const :tag "Mystery person" "mp")
                 (const :tag "Geometric patterns" "identicon")
                 (const :tag "Monsters" "monsterid")
                 (const :tag "Faces" "wavatar")
                 (const :tag "Retro" "retro")
                 (const :tag "Robots" "robohash")
                 (const :tag "Blank" "blank")
                 (string :tag "Custom URL"))
  :version "27.1"
  :group 'gravatar)

(defcustom gravatar-force-default nil
  "Whether to force use of `gravatar-default-image'.
Non-nil means use `gravatar-default-image' even when there exists
a gravatar for a given email address."
  :type 'boolean
  :version "27.1"
  :group 'gravatar)

(defconst gravatar-base-url
  "https://www.gravatar.com/avatar"
  "Base URL for getting gravatars.")

(defun gravatar-hash (mail-address)
  "Return the Gravatar hash for MAIL-ADDRESS."
  ;; https://gravatar.com/site/implement/hash/
  (md5 (downcase (string-trim mail-address))))

(defun gravatar--query-string ()
  "Return URI-encoded query string for Gravatar."
  (url-build-query-string
   `((r ,gravatar-rating)
     ,@(and gravatar-default-image
            `((d ,gravatar-default-image)))
     ,@(and gravatar-force-default
            '((f y)))
     ,@(and gravatar-size
            `((s ,gravatar-size))))))

(defun gravatar-build-url (mail-address)
  "Return the URL of a gravatar for MAIL-ADDRESS."
  ;; https://gravatar.com/site/implement/images/
  (format "%s/%s?%s"
          gravatar-base-url
          (gravatar-hash mail-address)
          (gravatar--query-string)))

(defun gravatar-get-data ()
  "Return body of current URL buffer, or nil on failure."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward "^HTTP/.+ 200 OK$" nil (line-end-position))
         (search-forward "\n\n" nil t)
         (buffer-substring (point) (point-max)))))

;;;###autoload
(defun gravatar-retrieve (mail-address callback &optional cbargs)
  "Asynchronously retrieve a gravatar for MAIL-ADDRESS.
When finished, call CALLBACK as (apply CALLBACK GRAVATAR CBARGS),
where GRAVATAR is either an image descriptor, or the symbol
`error' if the retrieval failed."
  (let ((url (gravatar-build-url mail-address)))
    (if (url-cache-expired url gravatar-cache-ttl)
        (url-retrieve url #'gravatar-retrieved (list callback cbargs) t)
      (with-current-buffer (url-fetch-from-cache url)
        (gravatar-retrieved () callback cbargs)))))

;;;###autoload
(defun gravatar-retrieve-synchronously (mail-address)
  "Synchronously retrieve a gravatar for MAIL-ADDRESS.
Value is either an image descriptor, or the symbol `error' if the
retrieval failed."
  (let ((url (gravatar-build-url mail-address)))
    (with-current-buffer (if (url-cache-expired url gravatar-cache-ttl)
                             (url-retrieve-synchronously url t)
                           (url-fetch-from-cache url))
      (gravatar-retrieved () #'identity))))

(defun gravatar-retrieved (status cb &optional cbargs)
  "Handle Gravatar response data in current buffer.
Return the result of (apply CB DATA CBARGS), where DATA is either
an image descriptor, or the symbol `error' on failure.
This function is intended as a callback for `url-retrieve'."
  (let ((data (unless (plist-get status :error)
                (gravatar-get-data))))
    (and data                      ; Only cache on success.
         url-current-object        ; Only cache if not already cached.
         gravatar-automatic-caching
         (url-store-in-cache))
    (prog1 (apply cb (if data (create-image data nil t) 'error) cbargs)
      (kill-buffer))))

(provide 'gravatar)

;;; gravatar.el ends here
