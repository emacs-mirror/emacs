;;; gravatar.el --- Get Gravatars -*- lexical-binding: t -*-

;; Copyright (C) 2010-2022 Free Software Foundation, Inc.

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
(require 'dns)
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
(make-obsolete-variable 'gravatar-automatic-caching nil "28.1")

(defcustom gravatar-cache-ttl 2592000
  "Time to live in seconds for gravatar cache entries.
If a requested gravatar has been cached for longer than this, it
is retrieved anew.  The default value is 30 days."
  :type 'integer
  ;; Restricted :type to number of seconds.
  :version "27.1"
  :group 'gravatar)
(make-obsolete-variable 'gravatar-cache-ttl nil "28.1")

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

(defconst gravatar-service-alist
  `((gravatar . ,(lambda (_addr callback)
                   (funcall callback "https://www.gravatar.com/avatar")))
    (unicornify . ,(lambda (_addr callback)
                     (funcall callback "https://unicornify.pictures/avatar/")))
    (libravatar . ,#'gravatar--service-libravatar))
  "Alist of supported gravatar services.")

(defcustom gravatar-service 'gravatar
  "Symbol denoting gravatar-like service to use.
Note that certain services might ignore other options, such as
`gravatar-default-image' or certain values as with
`gravatar-rating'.

Note that `'libravatar' has security implications: It can be used
to track whether you're reading a specific mail."
  :type `(choice ,@(mapcar (lambda (s) `(const ,(car s)))
                           gravatar-service-alist))
  :version "28.1"
  :link '(url-link "https://www.libravatar.org/")
  :link '(url-link "https://unicornify.pictures/")
  :link '(url-link "https://gravatar.com/")
  :group 'gravatar)

(defun gravatar--service-libravatar (addr callback)
  "Find domain that hosts avatars for email address ADDR."
  ;; implements https://wiki.libravatar.org/api/
  (save-match-data
    (if (not (string-match ".+@\\(.+\\)" addr))
        (funcall callback "https://seccdn.libravatar.org/avatar")
      (let ((domain (match-string 1 addr))
            (records '(("_avatars-sec" . "https")
                       ("_avatars" . "http")))
            func)
        (setq func
              (lambda (result)
                (cond
                 ((and
                   result               ;there is a result
                   (let* ((answers (dns-get 'answers result))
                          (data (mapcar (lambda (record)
                                          (dns-get 'data (cdr record)))
                                        ;; We may get junk data back (or CNAME;
                                        ;; ignore).
                                        (and (eq (dns-get 'type answers) 'SRV)
                                             answers)))
                          (priorities (mapcar (lambda (r)
                                                (dns-get 'priority r))
                                              data))
                          (max-priority (apply #'max 0 priorities))
                          (sum 0)
                          top)
                     ;; Attempt to find all records with the same maximal
                     ;; priority, and calculate the sum of their weights.
                     (dolist (ent data)
                       (when (= max-priority (dns-get 'priority ent))
                         (setq sum (+ sum (dns-get 'weight ent)))
                         (push ent top)))
                     ;; In case there is more than one maximal priority
                     ;; record, choose one at random, while taking the
                     ;; individual record weights into consideration.
                     (catch 'done
                       (dolist (ent top)
                         (when (and (or (= 0 sum)
                                        (<= 0 (random sum)
                                            (dns-get 'weight ent)))
                                    ;; Ensure that port and domain data are
                                    ;; valid. In case non of the results
                                    ;; were valid, `catch' will evaluate to
                                    ;; nil, and the next cond clause will be
                                    ;; tested.
                                    (<= 1 (dns-get 'port ent) 65535)
                                    (string-match-p "\\`[-.0-9A-Za-z]+\\'"
                                                    (dns-get 'target ent)))
                           (funcall callback
                                    (url-normalize-url
                                     (format "%s://%s:%s/avatar"
                                             (cdar records)
                                             (dns-get 'target ent)
                                             (dns-get 'port ent))))
                           (throw 'done t))
                         (setq sum (- sum (dns-get 'weight ent))))))))
                 ((setq records (cdr records))
                  ;; In case there are at least two methods.
                  (dns-query-asynchronous
                   (concat (caar records) "._tcp." domain)
                   func 'SRV))
                 (t                     ;fallback
                  (funcall callback "https://seccdn.libravatar.org/avatar")))))
        (dns-query-asynchronous
         (concat (caar records) "._tcp." domain)
         func 'SRV t)))))

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

(defun gravatar-build-url (mail-address callback)
  "Find the URL of a gravatar for MAIL-ADDRESS and call CALLBACK with it."
  ;; https://gravatar.com/site/implement/images/
  (let ((query-string (gravatar--query-string)))
    (funcall (alist-get gravatar-service gravatar-service-alist)
             mail-address
             (lambda (url)
               (funcall callback
                        (format "%s/%s?%s"
                                url
                                (gravatar-hash mail-address)
                                query-string))))))

(defun gravatar-get-data ()
  "Return body of current URL buffer, or nil on failure."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward "^HTTP/.+ 200 OK$" nil (line-end-position))
         (search-forward "\n\n" nil t)
         (buffer-substring (point) (point-max)))))

(defvar gravatar--cache (make-hash-table :test 'equal)
  "Cache for gravatars.")

;;;###autoload
(defun gravatar-retrieve (mail-address callback &optional cbargs)
  "Asynchronously retrieve a gravatar for MAIL-ADDRESS.
When finished, call CALLBACK as (apply CALLBACK GRAVATAR CBARGS),
where GRAVATAR is either an image descriptor, or the symbol
`error' if the retrieval failed."
  (let ((cached (gethash mail-address gravatar--cache)))
    (gravatar--prune-cache)
    (if cached
        (apply callback (cdr cached) cbargs)
      ;; Nothing in the cache, fetch it.
      (gravatar-build-url
       mail-address
       (lambda (url)
         (url-retrieve
          url
          (lambda (status)
            (let* ((data (and (not (plist-get status :error))
                              (gravatar-get-data)))
                   (image (and data (create-image data nil t))))
              ;; Store the image in the cache.
              (when image
                (setf (gethash mail-address gravatar--cache)
                      (cons (time-convert (current-time) 'integer)
                            image)))
              (prog1
                  (apply callback (if data image 'error) cbargs)
                (kill-buffer))))
          nil t))))))

(defun gravatar--prune-cache ()
  (let ((expired nil)
        (time (- (time-convert (current-time) 'integer)
                 ;; Twelve hours.
                 (* 12 60 60))))
    (maphash (lambda (key val)
               (when (< (car val) time)
                 (push key expired)))
             gravatar--cache)
    (dolist (key expired)
      (remhash key gravatar--cache))))

;;;###autoload
(defun gravatar-retrieve-synchronously (mail-address)
  "Synchronously retrieve a gravatar for MAIL-ADDRESS.
Value is either an image descriptor, or the symbol `error' if the
retrieval failed."
  (let ((url nil))
    (gravatar-build-url mail-address (lambda (u) (setq url u)))
    (while (not url)
      (sleep-for 0.01))
    (with-current-buffer (url-retrieve-synchronously url t)
      (gravatar-retrieved nil #'identity))))

(defun gravatar-retrieved (status cb &optional cbargs)
  "Handle Gravatar response data in current buffer.
Return the result of (apply CB DATA CBARGS), where DATA is either
an image descriptor, or the symbol `error' on failure.
This function is intended as a callback for `url-retrieve'."
  (let ((data (unless (plist-get status :error)
                (gravatar-get-data))))
    (prog1 (apply cb (if data (create-image data nil t) 'error) cbargs)
      (kill-buffer))))

(provide 'gravatar)

;;; gravatar.el ends here
