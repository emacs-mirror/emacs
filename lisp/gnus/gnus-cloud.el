;;; gnus-cloud.el --- storing and retrieving data via IMAP

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: mail

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

;; The name gnus-cloud parodizes but otherwise has little to do with
;; "cloud computing", a misleading term normally best avoided.  See:
;; https://www.gnu.org/philosophy/words-to-avoid.html#CloudComputing

;;; Code:

(eval-when-compile (require 'cl))
(require 'parse-time)
(require 'nnimap)

(eval-when-compile (require 'epg)) ;; setf-method for `epg-context-armor'
(autoload 'epg-make-context "epg")
(autoload 'epg-context-set-passphrase-callback "epg")
(autoload 'epg-decrypt-string "epg")
(autoload 'epg-encrypt-string "epg")

(defgroup gnus-cloud nil
  "Syncing Gnus data via IMAP."
  :version "25.1"
  :group 'gnus)

(defcustom gnus-cloud-synced-files
  '(;;"~/.authinfo"
    "~/.authinfo.gpg"
    "~/.gnus.el"
    (:directory "~/News" :match ".*.SCORE\\'"))
  "List of file regexps that should be kept up-to-date via the cloud."
  :group 'gnus-cloud
  ;; FIXME this type does not match the default.  Nor does the documentation.
  :type '(repeat regexp))

(defcustom gnus-cloud-storage-method (if (featurep 'epg) 'epg 'base64-gzip)
  "Storage method for cloud data, defaults to EPG if that's available."
  :version "26.1"
  :group 'gnus-cloud
  :type '(radio (const :tag "No encoding" nil)
                (const :tag "Base64" base64)
                (const :tag "Base64+gzip" base64-gzip)
                (const :tag "EPG" epg)))

(defcustom gnus-cloud-interactive t
  "Whether Gnus Cloud changes should be confirmed."
  :version "26.1"
  :group 'gnus-cloud
  :type 'boolean)

(defvar gnus-cloud-group-name "Emacs-Cloud")
(defvar gnus-cloud-covered-servers nil)

(defvar gnus-cloud-version 1)
(defvar gnus-cloud-sequence 1)

(defcustom gnus-cloud-method nil
  "The IMAP select method used to store the cloud data.
See also `gnus-server-set-cloud-method-server' for an
easy interactive way to set this from the Server buffer."
  :group 'gnus-cloud
  :type '(radio (const :tag "Not set" nil)
                (string :tag "A Gnus server name as a string")))

(defun gnus-cloud-make-chunk (elems)
  (with-temp-buffer
    (insert (format "Gnus-Cloud-Version %s\n" gnus-cloud-version))
    (insert (gnus-cloud-insert-data elems))
    (buffer-string)))

(defun gnus-cloud-insert-data (elems)
  (mm-with-unibyte-buffer
    (dolist (elem elems)
      (cond
       ((eq (plist-get elem :type) :file)
        (let (length data)
          (mm-with-unibyte-buffer
            (insert-file-contents-literally (plist-get elem :file-name))
            (setq length (buffer-size)
                  data (buffer-string)))
          (insert (format "(:type :file :file-name %S :timestamp %S :length %d)\n"
                          (plist-get elem :file-name)
                          (plist-get elem :timestamp)
                          length))
          (insert data)
          (insert "\n")))
       ((eq (plist-get elem :type) :newsrc-data)
        (let ((print-level nil)
              (print-length nil))
          (print elem (current-buffer)))
        (insert "\n"))
       ((eq (plist-get elem :type) :delete)
        (insert (format "(:type :delete :file-name %S)\n"
                        (plist-get elem :file-name))))))
    (gnus-cloud-encode-data)
    (buffer-string)))

(defun gnus-cloud-encode-data ()
  (cond
   ((eq gnus-cloud-storage-method 'base64-gzip)
    (progn
      (call-process-region (point-min) (point-max) "gzip"
                           t (current-buffer) nil
                           "-c")
      (base64-encode-region (point-min) (point-max))))

   ((eq gnus-cloud-storage-method 'base64)
    (base64-encode-region (point-min) (point-max)))

   ((eq gnus-cloud-storage-method 'epg)
    (let ((context (epg-make-context 'OpenPGP))
          cipher)
      (setf (epg-context-armor context) t)
      (setf (epg-context-textmode context) t)
      (let ((data (epg-encrypt-string context
                                      (buffer-substring-no-properties
                                       (point-min)
                                       (point-max))
                                      nil)))
        (delete-region (point-min) (point-max))
        (insert data))))

   ((null gnus-cloud-storage-method)
    (gnus-message 5 "Leaving cloud data plaintext"))
   (t (gnus-error 1 "Invalid cloud storage method %S"
                  gnus-cloud-storage-method))))

(defun gnus-cloud-decode-data ()
  (cond
   ((memq gnus-cloud-storage-method '(base64 base64-gzip))
    (base64-decode-region (point-min) (point-max)))

   ((eq gnus-cloud-storage-method 'base64-gzip)
    (call-process-region (point-min) (point-max) "gunzip"
                         t (current-buffer) nil
                         "-c"))

   ((eq gnus-cloud-storage-method 'epg)
    (let* ((context (epg-make-context 'OpenPGP))
           (data (epg-decrypt-string context (buffer-substring-no-properties
                                              (point-min)
                                              (point-max)))))
      (delete-region (point-min) (point-max))
      (insert data)))

   ((null gnus-cloud-storage-method)
    (gnus-message 5 "Reading cloud data as plaintext"))

   (t (gnus-error 1 "Invalid cloud storage method %S"
                  gnus-cloud-storage-method))))

(defun gnus-cloud-parse-chunk ()
  (save-excursion
    (unless (looking-at "Gnus-Cloud-Version \\([0-9]+\\)")
      (error "Not a valid Cloud chunk in the current buffer"))
    (forward-line 1)
    (let ((version (string-to-number (match-string 1)))
          (data (buffer-substring (point) (point-max))))
      (mm-with-unibyte-buffer
        (insert data)
        (cond
         ((= version 1)
          (gnus-cloud-decode-data)
          (goto-char (point-min))
          (gnus-cloud-parse-version-1))
         (t
          (error "Unsupported Cloud chunk version %s" version)))))))

(defun gnus-cloud-parse-version-1 ()
  (let ((elems nil))
    (while (not (eobp))
      (while (and (not (eobp))
                  (not (looking-at "(:type")))
        (forward-line 1))
      (unless (eobp)
        (let ((spec (ignore-errors (read (current-buffer))))
              length)
          (when (consp spec)
            (cond
             ((memq (plist-get spec :type) '(:file :delete))
              (setq length (plist-get spec :length))
              (push (append spec
                            (list
                             :contents (buffer-substring (1+ (point))
                                                         (+ (point) 1 length))))
                    elems)
              (goto-char (+ (point) 1 length)))
             ((memq (plist-get spec :type) '(:newsrc-data))
              (push spec elems)))))))
    (nreverse elems)))

(defun gnus-cloud-update-all (elems)
  (dolist (elem elems)
    (let ((type (plist-get elem :type)))
      (cond
       ((eq type :newsrc-data)
        (gnus-cloud-update-newsrc-data (plist-get elem :name) elem))
       ((memq type '(:delete :file))
        (gnus-cloud-update-file elem type))
       (t
        (gnus-message 1 "Unknown type %s; ignoring" type))))))

(defun gnus-cloud-update-newsrc-data (group elem &optional force-older)
  "Update the newsrc data for GROUP from ELEM.
Use old data if FORCE-OLDER is not nil."
  (let* ((contents (plist-get elem :contents))
         (date (or (plist-get elem :timestamp) "0"))
         (now (gnus-cloud-timestamp nil))
         (newer (string-lessp date now))
         (group-info (gnus-get-info group)))
    (if (and contents
             (stringp (nth 0 contents))
             (integerp (nth 1 contents)))
        (if group-info
            (if (equal (format "%S" group-info)
                       (format "%S" contents))
                (gnus-message 3 "Skipping cloud update of group %s, the info is the same" group)
              (if (and newer (not force-older))
                (gnus-message 3 "Skipping outdated cloud info for group %s, the info is from %s (now is %s)" group date now)
                (when (or (not gnus-cloud-interactive)
                          (gnus-y-or-n-p
                           (format "%s has older different info in the cloud as of %s, update it here? "
                                   group date))))
                (gnus-message 2 "Installing cloud update of group %s" group)
                (gnus-set-info group contents)
                (gnus-group-update-group group)))
          (gnus-error 1 "Sorry, group %s is not subscribed" group))
      (gnus-error 1 "Sorry, could not update newsrc for group %s (invalid data %S)"
                  group elem))))

(defun gnus-cloud-update-file (elem op)
  "Apply Gnus Cloud data ELEM and operation OP to a file."
  (let* ((file-name (plist-get elem :file-name))
         (date (plist-get elem :timestamp))
         (contents (plist-get elem :contents))
         (exists (file-exists-p file-name)))
    (if (gnus-cloud-file-covered-p file-name)
        (cond
         ((eq op :delete)
          (if (and exists
                   ;; prompt only if the file exists already
                   (or (not gnus-cloud-interactive)
                       (gnus-y-or-n-p (format "%s has been deleted as of %s, delete it locally? "
                                              file-name date))))
              (rename-file file-name (car (find-backup-file-name file-name)))
            (gnus-message 3 "%s was already deleted before the cloud got it" file-name)))
         ((eq op :file)
          (when (or (not exists)
                    (and exists
                         (mm-with-unibyte-buffer
                           (insert-file-contents-literally file-name)
                           (not (equal (buffer-string) contents)))
                         ;; prompt only if the file exists already
                         (or (not gnus-cloud-interactive)
                             (gnus-y-or-n-p (format "%s has updated contents as of %s, update it? "
                                                    file-name date)))))
            (gnus-cloud-replace-file file-name date contents))))
      (gnus-message 2 "%s isn't covered by the cloud; ignoring" file-name))))

(defun gnus-cloud-replace-file (file-name date new-contents)
  (mm-with-unibyte-buffer
    (insert new-contents)
    (when (file-exists-p file-name)
      (rename-file file-name (car (find-backup-file-name file-name))))
    (write-region (point-min) (point-max) file-name)
    (set-file-times file-name (parse-iso8601-time-string date))))

(defun gnus-cloud-file-covered-p (file-name)
  (let ((matched nil))
    (dolist (elem gnus-cloud-synced-files)
      (cond
       ((stringp elem)
        (when (equal elem file-name)
          (setq matched t)))
       ((consp elem)
        (when (and (equal (directory-file-name (plist-get elem :directory))
                          (directory-file-name (file-name-directory file-name)))
                   (string-match (plist-get elem :match)
                                 (file-name-nondirectory file-name)))
          (setq matched t)))))
    matched))

(defun gnus-cloud-all-files ()
  (let ((files nil))
    (dolist (elem gnus-cloud-synced-files)
      (cond
       ((stringp elem)
        (push elem files))
       ((consp elem)
        (dolist (file (directory-files (plist-get elem :directory)
                                       nil
                                       (plist-get elem :match)))
          (push (format "%s/%s"
                        (directory-file-name (plist-get elem :directory))
                        file)
                files)))))
    (nreverse files)))

(defvar gnus-cloud-file-timestamps nil)

(defun gnus-cloud-files-to-upload (&optional full)
  (let ((files nil)
        timestamp)
    (dolist (file (gnus-cloud-all-files))
      (if (file-exists-p file)
          (when (setq timestamp (gnus-cloud-file-new-p file full))
            (push `(:type :file :file-name ,file :timestamp ,timestamp) files))
        (when (assoc file gnus-cloud-file-timestamps)
          (push `(:type :delete :file-name ,file) files))))
    (nreverse files)))

(defun gnus-cloud-timestamp (time)
  "Return a general timestamp string for TIME."
  (format-time-string "%FT%T%z" time))

(defun gnus-cloud-file-new-p (file full)
  (let ((timestamp (gnus-cloud-timestamp (nth 5 (file-attributes file))))
        (old (cadr (assoc file gnus-cloud-file-timestamps))))
    (when (or full
              (null old)
              (string< old timestamp))
      timestamp)))

(declare-function gnus-activate-group "gnus-start"
                  (group &optional scan dont-check method dont-sub-check))
(declare-function gnus-subscribe-group "gnus-start"
                  (group &optional previous method))

(defun gnus-cloud-ensure-cloud-group ()
  (let ((method (if (stringp gnus-cloud-method)
                    (gnus-server-to-method gnus-cloud-method)
                  gnus-cloud-method)))
    (unless (or (gnus-active gnus-cloud-group-name)
                (gnus-activate-group gnus-cloud-group-name nil nil
                                     gnus-cloud-method))
      (and (gnus-request-create-group gnus-cloud-group-name gnus-cloud-method)
           (gnus-activate-group gnus-cloud-group-name nil nil gnus-cloud-method)
           (gnus-subscribe-group gnus-cloud-group-name)))))

(defun gnus-cloud-upload-all-data ()
  "Upload all data (newsrc and files) to the Gnus Cloud."
  (interactive)
  (gnus-cloud-upload-data t))

(autoload 'gnus-group-refresh-group "gnus-group")

(defun gnus-cloud-upload-data (&optional full)
  "Upload data (newsrc and files) to the Gnus Cloud.
When FULL is t, upload everything, not just a difference from the last full."
  (interactive)
  (gnus-cloud-ensure-cloud-group)
  (with-temp-buffer
    (let ((elems (append
                  (gnus-cloud-files-to-upload full)
                  (gnus-cloud-collect-full-newsrc)))
          (group (gnus-group-full-name gnus-cloud-group-name gnus-cloud-method)))
      (insert (format "Subject: (sequence: %s type: %s storage-method: %s)\n"
                      (or gnus-cloud-sequence "UNKNOWN")
                      (if full :full :partial)
                      gnus-cloud-storage-method))
      (insert "From: nobody@gnus.cloud.invalid\n")
      (insert "\n")
      (insert (gnus-cloud-make-chunk elems))
      (if (gnus-request-accept-article gnus-cloud-group-name gnus-cloud-method
                                       t t)
          (progn
            (setq gnus-cloud-sequence (1+ (or gnus-cloud-sequence 0)))
            (gnus-cloud-add-timestamps elems)
            (gnus-message 3 "Uploaded Gnus Cloud data successfully to %s" group)
            (gnus-group-refresh-group group))
        (gnus-error 2 "Failed to upload Gnus Cloud data to %s" group)))))

(defun gnus-cloud-add-timestamps (elems)
  (dolist (elem elems)
    (let* ((file-name (plist-get elem :file-name))
           (old (assoc file-name gnus-cloud-file-timestamps)))
      (when old
        (setq gnus-cloud-file-timestamps
              (delq old gnus-cloud-file-timestamps)))
      (push (list file-name (plist-get elem :timestamp))
            gnus-cloud-file-timestamps))))

(defun gnus-cloud-available-chunks ()
  (gnus-activate-group gnus-cloud-group-name nil nil gnus-cloud-method)
  (let* ((group (gnus-group-full-name gnus-cloud-group-name gnus-cloud-method))
         (active (gnus-active group))
         headers head)
    (when (gnus-retrieve-headers (gnus-uncompress-range active) group)
      (with-current-buffer nntp-server-buffer
        (goto-char (point-min))
        (while (and (not (eobp))
                    (setq head (nnheader-parse-head)))
          (push head headers))))
    (sort (nreverse headers)
          (lambda (h1 h2)
            (> (gnus-cloud-chunk-sequence (mail-header-subject h1))
               (gnus-cloud-chunk-sequence (mail-header-subject h2)))))))

(defun gnus-cloud-chunk-sequence (string)
  (if (string-match "sequence: \\([0-9]+\\)" string)
      (string-to-number (match-string 1 string))
    0))

;; TODO: use this
(defun gnus-cloud-prune-old-chunks (headers)
  (let ((headers (reverse headers))
        (found nil))
  (while (and headers
              (not found))
    (when (string-match "type: :full" (mail-header-subject (car headers)))
      (setq found t))
    (pop headers))
  ;; All the chunks that are older than the newest :full chunk can be
  ;; deleted.
  (when headers
    (gnus-request-expire-articles
     (mapcar (lambda (h)
               (mail-header-number h))
             (nreverse headers))
     (gnus-group-full-name gnus-cloud-group-name gnus-cloud-method)))))

(defun gnus-cloud-download-all-data ()
  "Download the Gnus Cloud data and install it.
Starts at `gnus-cloud-sequence' in the sequence."
  (interactive)
  (gnus-cloud-download-data t))

(defun gnus-cloud-download-data (&optional update sequence-override)
  "Download the Gnus Cloud data and install it if UPDATE is t.
When SEQUENCE-OVERRIDE is given, start at that sequence number
instead of `gnus-cloud-sequence'.

When UPDATE is t, returns the result of calling `gnus-cloud-update-all'.
Otherwise, returns the Gnus Cloud data chunks."
  (let ((articles nil)
        chunks)
    (dolist (header (gnus-cloud-available-chunks))
      (when (> (gnus-cloud-chunk-sequence (mail-header-subject header))
               (or sequence-override gnus-cloud-sequence -1))

        (if (string-match (format "storage-method: %s" gnus-cloud-storage-method)
                          (mail-header-subject header))
            (push (mail-header-number header) articles)
          (gnus-message 1 "Skipping article %s because it didn't match the Gnus Cloud method %s: %s"
                        (mail-header-number header)
                        gnus-cloud-storage-method
                        (mail-header-subject header)))))
    (when articles
      (nnimap-request-articles (nreverse articles) gnus-cloud-group-name)
      (with-current-buffer nntp-server-buffer
        (goto-char (point-min))
        (while (re-search-forward "^Gnus-Cloud-Version " nil t)
          (beginning-of-line)
          (push (gnus-cloud-parse-chunk) chunks)
          (forward-line 1))))
    (if update
        (mapcar #'gnus-cloud-update-all chunks)
      chunks)))

(defun gnus-cloud-server-p (server)
  (member server gnus-cloud-covered-servers))

(defun gnus-cloud-host-server-p (server)
  (equal gnus-cloud-method server))

(defun gnus-cloud-host-acceptable-method-p (server)
  (eq (car-safe (gnus-server-to-method server)) 'nnimap))

(defun gnus-cloud-collect-full-newsrc ()
  "Collect all the Gnus newsrc data in a portable format."
  (let ((infos nil))
    (dolist (info (cdr gnus-newsrc-alist))
      (when (gnus-cloud-server-p
             (gnus-method-to-server
              (gnus-find-method-for-group (gnus-info-group info))))

        (push `(:type :newsrc-data :name ,(gnus-info-group info) :contents ,info :timestamp ,(gnus-cloud-timestamp nil))
              infos)))
    infos))

(provide 'gnus-cloud)

;;; gnus-cloud.el ends here
