;;; gnus-soup.el --- SOUP packet writing support for Gnus

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news, mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-art)
(require 'message)
(require 'gnus-start)
(require 'gnus-range)

(defgroup gnus-soup nil
  "SOUP packet writing support for Gnus."
  :group 'gnus)

;;; User Variables:

(defcustom gnus-soup-directory (nnheader-concat gnus-home-directory "SoupBrew/")
  "Directory containing an unpacked SOUP packet."
  :version "22.1" ;; Gnus 5.10.9
  :type 'directory
  :group 'gnus-soup)

(defcustom gnus-soup-replies-directory
  (nnheader-concat gnus-soup-directory "SoupReplies/")
  "Directory where Gnus will do processing of replies."
  :version "22.1" ;; Gnus 5.10.9
  :type 'directory
  :group 'gnus-soup)

(defcustom gnus-soup-prefix-file "gnus-prefix"
  "Name of the file where Gnus stores the last used prefix."
  :version "22.1" ;; Gnus 5.10.9
  :type 'file
  :group 'gnus-soup)

(defcustom gnus-soup-packer "tar cf - %s | gzip > $HOME/Soupout%d.tgz"
  "Format string command for packing a SOUP packet.
The SOUP files will be inserted where the %s is in the string.
This string MUST contain both %s and %d.  The file number will be
inserted where %d appears."
  :version "22.1" ;; Gnus 5.10.9
  :type 'string
  :group 'gnus-soup)

(defcustom gnus-soup-unpacker "gunzip -c %s | tar xvf -"
  "Format string command for unpacking a SOUP packet.
The SOUP packet file name will be inserted at the %s."
  :version "22.1" ;; Gnus 5.10.9
  :type 'string
  :group 'gnus-soup)

(defcustom gnus-soup-packet-directory gnus-home-directory
  "Where gnus-soup will look for REPLIES packets."
  :version "22.1" ;; Gnus 5.10.9
  :type 'directory
  :group 'gnus-soup)

(defcustom gnus-soup-packet-regexp "Soupin"
  "Regular expression matching SOUP REPLIES packets in `gnus-soup-packet-directory'."
  :version "22.1" ;; Gnus 5.10.9
  :type 'regexp
  :group 'gnus-soup)

(defcustom gnus-soup-ignored-headers "^Xref:"
  "Regexp to match headers to be removed when brewing SOUP packets."
  :version "22.1" ;; Gnus 5.10.9
  :type 'regexp
  :group 'gnus-soup)

;;; Internal Variables:

(defvar gnus-soup-encoding-type ?u
  "*Soup encoding type.
`u' is USENET news format, `m' is Unix mbox format, and `M' is MMDF mailbox
format.")

(defvar gnus-soup-index-type ?c
  "*Soup index type.
`n' means no index file and `c' means standard Cnews overview
format.")

(defvar gnus-soup-areas nil)
(defvar gnus-soup-last-prefix nil)
(defvar gnus-soup-prev-prefix nil)
(defvar gnus-soup-buffers nil)

;;; Access macros:

(defmacro gnus-soup-area-prefix (area)
  `(aref ,area 0))
(defmacro gnus-soup-set-area-prefix (area prefix)
  `(aset ,area 0 ,prefix))
(defmacro gnus-soup-area-name (area)
  `(aref ,area 1))
(defmacro gnus-soup-area-encoding (area)
  `(aref ,area 2))
(defmacro gnus-soup-area-description (area)
  `(aref ,area 3))
(defmacro gnus-soup-area-number (area)
  `(aref ,area 4))
(defmacro gnus-soup-area-set-number (area value)
  `(aset ,area 4 ,value))

(defmacro gnus-soup-encoding-format (encoding)
  `(aref ,encoding 0))
(defmacro gnus-soup-encoding-index (encoding)
  `(aref ,encoding 1))
(defmacro gnus-soup-encoding-kind (encoding)
  `(aref ,encoding 2))

(defmacro gnus-soup-reply-prefix (reply)
  `(aref ,reply 0))
(defmacro gnus-soup-reply-kind (reply)
  `(aref ,reply 1))
(defmacro gnus-soup-reply-encoding (reply)
  `(aref ,reply 2))

;;; Commands:

(defun gnus-soup-send-replies ()
  "Unpack and send all replies in the reply packet."
  (interactive)
  (let ((packets (directory-files
		  gnus-soup-packet-directory t gnus-soup-packet-regexp)))
    (while packets
      (when (gnus-soup-send-packet (car packets))
	(delete-file (car packets)))
      (setq packets (cdr packets)))))

(defun gnus-soup-add-article (n)
  "Add the current article to SOUP packet.
If N is a positive number, add the N next articles.
If N is a negative number, add the N previous articles.
If N is nil and any articles have been marked with the process mark,
move those articles instead."
  (interactive "P")
  (let* ((articles (gnus-summary-work-articles n))
	 (tmp-buf (gnus-get-buffer-create "*soup work*"))
	 (area (gnus-soup-area gnus-newsgroup-name))
	 (prefix (gnus-soup-area-prefix area))
	 headers)
    (buffer-disable-undo tmp-buf)
    (save-excursion
      (while articles
	;; Put the article in a buffer.
	(set-buffer tmp-buf)
	(when (gnus-request-article-this-buffer
	       (car articles) gnus-newsgroup-name)
	  (setq headers (nnheader-parse-head t))
	  (save-restriction
	    (message-narrow-to-head)
	    (message-remove-header gnus-soup-ignored-headers t))
	  (gnus-soup-store gnus-soup-directory prefix headers
			   gnus-soup-encoding-type
			   gnus-soup-index-type)
	  (gnus-soup-area-set-number
	   area (1+ (or (gnus-soup-area-number area) 0)))
	  ;; Mark article as read.
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-mark-as-read (car articles) gnus-souped-mark))
	(gnus-summary-remove-process-mark (car articles))
	(setq articles (cdr articles)))
      (kill-buffer tmp-buf))
    (gnus-soup-save-areas)
    (gnus-set-mode-line 'summary)))

(defun gnus-soup-pack-packet ()
  "Make a SOUP packet from the SOUP areas."
  (interactive)
  (gnus-soup-read-areas)
  (if (file-exists-p gnus-soup-directory)
      (if (directory-files gnus-soup-directory nil "\\.MSG$")
	  (gnus-soup-pack gnus-soup-directory gnus-soup-packer)
	(message "No files to pack."))
    (message "No such directory: %s" gnus-soup-directory)))

(defun gnus-group-brew-soup (n)
  "Make a soup packet from the current group.
Uses the process/prefix convention."
  (interactive "P")
  (let ((groups (gnus-group-process-prefix n)))
    (while groups
      (gnus-group-remove-mark (car groups))
      (gnus-soup-group-brew (car groups) t)
      (setq groups (cdr groups)))
    (gnus-soup-save-areas)))

(defun gnus-brew-soup (&optional level)
  "Go through all groups on LEVEL or less and make a soup packet."
  (interactive "P")
  (let ((level (or level gnus-level-subscribed))
	(newsrc (cdr gnus-newsrc-alist)))
    (while newsrc
      (when (<= (nth 1 (car newsrc)) level)
	(gnus-soup-group-brew (caar newsrc) t))
      (setq newsrc (cdr newsrc)))
    (gnus-soup-save-areas)))

;;;###autoload
(defun gnus-batch-brew-soup ()
  "Brew a SOUP packet from groups mention on the command line.
Will use the remaining command line arguments as regular expressions
for matching on group names.

For instance, if you want to brew on all the nnml groups, as well as
groups with \"emacs\" in the name, you could say something like:

$ emacs -batch -f gnus-batch-brew-soup ^nnml \".*emacs.*\"

Note -- this function hasn't been implemented yet."
  (interactive)
  nil)

;;; Internal Functions:

;; Store the current buffer.
(defun gnus-soup-store (directory prefix headers format index)
  ;; Create the directory, if needed.
  (gnus-make-directory directory)
  (let* ((msg-buf (nnheader-find-file-noselect
		   (concat directory prefix ".MSG")))
	 (idx-buf (if (= index ?n)
		      nil
		    (nnheader-find-file-noselect
		     (concat directory prefix ".IDX"))))
	 (article-buf (current-buffer))
	 from head-line beg type)
    (setq gnus-soup-buffers (cons msg-buf (delq msg-buf gnus-soup-buffers)))
    (buffer-disable-undo msg-buf)
    (when idx-buf
      (push idx-buf gnus-soup-buffers)
      (buffer-disable-undo idx-buf))
    (save-excursion
      ;; Make sure the last char in the buffer is a newline.
      (goto-char (point-max))
      (unless (= (current-column) 0)
	(insert "\n"))
      ;; Find the "from".
      (goto-char (point-min))
      (setq from
	    (gnus-mail-strip-quoted-names
	     (or (mail-fetch-field "from")
		 (mail-fetch-field "really-from")
		 (mail-fetch-field "sender"))))
      (goto-char (point-min))
      ;; Depending on what encoding is supposed to be used, we make
      ;; a soup header.
      (setq head-line
	    (cond
	     ((or (= gnus-soup-encoding-type ?u)
		  (= gnus-soup-encoding-type ?n)) ;;Gnus back compatibility.
	      (format "#! rnews %d\n" (buffer-size)))
	     ((= gnus-soup-encoding-type ?m)
	      (while (search-forward "\nFrom " nil t)
		(replace-match "\n>From " t t))
	      (concat "From " (or from "unknown")
		      " " (current-time-string) "\n"))
	     ((= gnus-soup-encoding-type ?M)
	      "\^a\^a\^a\^a\n")
	     (t (error "Unsupported type: %c" gnus-soup-encoding-type))))
      ;; Insert the soup header and the article in the MSG buf.
      (set-buffer msg-buf)
      (goto-char (point-max))
      (insert head-line)
      (setq beg (point))
      (insert-buffer-substring article-buf)
      ;; Insert the index in the IDX buf.
      (cond ((= index ?c)
	     (set-buffer idx-buf)
	     (gnus-soup-insert-idx beg headers))
	    ((/= index ?n)
	     (error "Unknown index type: %c" type)))
      ;; Return the MSG buf.
      msg-buf)))

(defun gnus-soup-group-brew (group &optional not-all)
  "Enter GROUP and add all articles to a SOUP package.
If NOT-ALL, don't pack ticked articles."
  (let ((gnus-expert-user t)
	(gnus-large-newsgroup nil)
	(entry (gnus-group-entry group)))
    (when (or (null entry)
	      (eq (car entry) t)
	      (and (car entry)
		   (> (car entry) 0))
	      (and (not not-all)
		   (gnus-range-length (cdr (assq 'tick (gnus-info-marks
							(nth 2 entry)))))))
      (when (gnus-summary-read-group group nil t)
	(setq gnus-newsgroup-processable
	      (reverse
	       (if (not not-all)
		   (append gnus-newsgroup-marked gnus-newsgroup-unreads)
		 gnus-newsgroup-unreads)))
	(gnus-soup-add-article nil)
	(gnus-summary-exit)))))

(defun gnus-soup-insert-idx (offset header)
  ;; [number subject from date id references chars lines xref]
  (goto-char (point-max))
  (insert
   (format "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%s\t\t\n"
	   offset
	   (or (mail-header-subject header) "(none)")
	   (or (mail-header-from header) "(nobody)")
	   (or (mail-header-date header) "")
	   (or (mail-header-id header)
	       (concat "soup-dummy-id-"
		       (mapconcat
			(lambda (time) (int-to-string time))
			(current-time) "-")))
	   (or (mail-header-references header) "")
	   (or (mail-header-chars header) 0)
	   (or (mail-header-lines header) "0"))))

(defun gnus-soup-save-areas ()
  "Write all SOUP buffers."
  (interactive)
  (gnus-soup-write-areas)
  (save-excursion
    (let (buf)
      (while gnus-soup-buffers
	(setq buf (car gnus-soup-buffers)
	      gnus-soup-buffers (cdr gnus-soup-buffers))
	(if (not (buffer-name buf))
	    ()
	  (set-buffer buf)
	  (when (buffer-modified-p)
	    (save-buffer))
	  (kill-buffer (current-buffer)))))
    (gnus-soup-write-prefixes)))

(defun gnus-soup-write-prefixes ()
  (let ((prefixes gnus-soup-last-prefix)
	prefix)
    (save-excursion
      (gnus-set-work-buffer)
      (while (setq prefix (pop prefixes))
	(erase-buffer)
	(insert (format "(setq gnus-soup-prev-prefix %d)\n" (cdr prefix)))
	(let ((coding-system-for-write mm-text-coding-system))
	  (gnus-write-buffer (concat (car prefix) gnus-soup-prefix-file)))))))

(defun gnus-soup-pack (dir packer)
  (let* ((files (mapconcat 'identity
			   '("AREAS" "*.MSG" "*.IDX" "INFO"
			     "LIST" "REPLIES" "COMMANDS" "ERRORS")
			   " "))
	 (packer (if (< (string-match "%s" packer)
			(string-match "%d" packer))
		     (format packer files
			     (string-to-number (gnus-soup-unique-prefix dir)))
		   (format packer
			   (string-to-number (gnus-soup-unique-prefix dir))
			   files)))
	 (dir (expand-file-name dir)))
    (gnus-make-directory dir)
    (setq gnus-soup-areas nil)
    (gnus-message 4 "Packing %s..." packer)
    (if (eq 0 (call-process shell-file-name
			    nil nil nil shell-command-switch
			    (concat "cd " dir " ; " packer)))
	(progn
	  (call-process shell-file-name nil nil nil shell-command-switch
			(concat "cd " dir " ; rm " files))
	  (gnus-message 4 "Packing...done" packer))
      (error "Couldn't pack packet"))))

(defun gnus-soup-parse-areas (file)
  "Parse soup area file FILE.
The result is a of vectors, each containing one entry from the AREA file.
The vector contain five strings,
  [prefix name encoding description number]
though the two last may be nil if they are missing."
  (let (areas)
    (when (file-exists-p file)
      (save-excursion
	(set-buffer (nnheader-find-file-noselect file 'force))
	(buffer-disable-undo)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (vector (gnus-soup-field)
			(gnus-soup-field)
			(gnus-soup-field)
			(and (eq (preceding-char) ?\t)
			     (gnus-soup-field))
			(and (eq (preceding-char) ?\t)
			     (string-to-number (gnus-soup-field))))
		areas)
	  (when (eq (preceding-char) ?\t)
	    (beginning-of-line 2)))
	(kill-buffer (current-buffer))))
    areas))

(defun gnus-soup-parse-replies (file)
  "Parse soup REPLIES file FILE.
The result is a of vectors, each containing one entry from the REPLIES
file.  The vector contain three strings, [prefix name encoding]."
  (let (replies)
    (save-excursion
      (set-buffer (nnheader-find-file-noselect file))
      (buffer-disable-undo)
      (goto-char (point-min))
      (while (not (eobp))
	(push (vector (gnus-soup-field) (gnus-soup-field)
		      (gnus-soup-field))
	      replies)
	(when (eq (preceding-char) ?\t)
	  (beginning-of-line 2)))
      (kill-buffer (current-buffer)))
    replies))

(defun gnus-soup-field ()
  (prog1
      (buffer-substring (point) (progn (skip-chars-forward "^\t\n") (point)))
    (forward-char 1)))

(defun gnus-soup-read-areas ()
  (or gnus-soup-areas
      (setq gnus-soup-areas
	    (gnus-soup-parse-areas (concat gnus-soup-directory "AREAS")))))

(defun gnus-soup-write-areas ()
  "Write the AREAS file."
  (interactive)
  (when gnus-soup-areas
    (with-temp-file (concat gnus-soup-directory "AREAS")
      (let ((areas gnus-soup-areas)
	    area)
	(while (setq area (pop areas))
	  (insert
	   (format
	    "%s\t%s\t%s%s\n"
	    (gnus-soup-area-prefix area)
	    (gnus-soup-area-name area)
	    (gnus-soup-area-encoding area)
	    (if (or (gnus-soup-area-description area)
		    (gnus-soup-area-number area))
		(concat "\t" (or (gnus-soup-area-description
				  area) "")
			(if (gnus-soup-area-number area)
			    (concat "\t" (int-to-string
					  (gnus-soup-area-number area)))
			  "")) ""))))))))

(defun gnus-soup-write-replies (dir areas)
  "Write a REPLIES file in DIR containing AREAS."
  (with-temp-file (concat dir "REPLIES")
    (let (area)
      (while (setq area (pop areas))
	(insert (format "%s\t%s\t%s\n"
			(gnus-soup-reply-prefix area)
			(gnus-soup-reply-kind area)
			(gnus-soup-reply-encoding area)))))))

(defun gnus-soup-area (group)
  (gnus-soup-read-areas)
  (let ((areas gnus-soup-areas)
	(real-group (gnus-group-real-name group))
	area result)
    (while areas
      (setq area (car areas)
	    areas (cdr areas))
      (when (equal (gnus-soup-area-name area) real-group)
	(setq result area)))
    (unless result
      (setq result
	    (vector (gnus-soup-unique-prefix)
		    real-group
		    (format "%c%c%c"
			    gnus-soup-encoding-type
			    gnus-soup-index-type
			    (if (gnus-member-of-valid 'mail group) ?m ?n))
		    nil nil)
	    gnus-soup-areas (cons result gnus-soup-areas)))
    result))

(defun gnus-soup-unique-prefix (&optional dir)
  (let* ((dir (file-name-as-directory (or dir gnus-soup-directory)))
	 (entry (assoc dir gnus-soup-last-prefix))
	 gnus-soup-prev-prefix)
    (if entry
	()
      (when (file-exists-p (concat dir gnus-soup-prefix-file))
	(ignore-errors
	  (load (concat dir gnus-soup-prefix-file) nil t t)))
      (push (setq entry (cons dir (or gnus-soup-prev-prefix 0)))
	    gnus-soup-last-prefix))
    (setcdr entry (1+ (cdr entry)))
    (gnus-soup-write-prefixes)
    (int-to-string (cdr entry))))

(defun gnus-soup-unpack-packet (dir unpacker packet)
  "Unpack PACKET into DIR using UNPACKER.
Return whether the unpacking was successful."
  (gnus-make-directory dir)
  (gnus-message 4 "Unpacking: %s" (format unpacker packet))
  (prog1
      (eq 0 (call-process
	     shell-file-name nil nil nil shell-command-switch
	     (format "cd %s ; %s" (expand-file-name dir)
		     (format unpacker packet))))
    (gnus-message 4 "Unpacking...done")))

(defun gnus-soup-send-packet (packet)
  (gnus-soup-unpack-packet
   gnus-soup-replies-directory gnus-soup-unpacker packet)
  (let ((replies (gnus-soup-parse-replies
		  (concat gnus-soup-replies-directory "REPLIES"))))
    (save-excursion
      (while replies
	(let* ((msg-file (concat gnus-soup-replies-directory
				 (gnus-soup-reply-prefix (car replies))
				 ".MSG"))
	       (msg-buf (and (file-exists-p msg-file)
			     (nnheader-find-file-noselect msg-file)))
	       (tmp-buf (gnus-get-buffer-create " *soup send*"))
	       beg end)
	  (cond
	   ((and (/= (gnus-soup-encoding-format
		      (gnus-soup-reply-encoding (car replies)))
		     ?u)
		 (/= (gnus-soup-encoding-format
		      (gnus-soup-reply-encoding (car replies)))
		     ?n)) ;; Gnus back compatibility.
	    (error "Unsupported encoding"))
	   ((null msg-buf)
	    t)
	   (t
	    (buffer-disable-undo msg-buf)
	    (set-buffer msg-buf)
	    (goto-char (point-min))
	    (while (not (eobp))
	      (unless (looking-at "#! *rnews +\\([0-9]+\\)")
		(error "Bad header"))
	      (forward-line 1)
	      (setq beg (point)
		    end (+ (point) (string-to-number
				    (buffer-substring
				     (match-beginning 1) (match-end 1)))))
	      (switch-to-buffer tmp-buf)
	      (erase-buffer)
	      (mm-disable-multibyte)
	      (insert-buffer-substring msg-buf beg end)
	      (cond
	       ((string= (gnus-soup-reply-kind (car replies)) "news")
		(gnus-message 5 "Sending news message to %s..."
			      (mail-fetch-field "newsgroups"))
		(sit-for 1)
		(let ((message-syntax-checks
		       'dont-check-for-anything-just-trust-me)
		      (method (if (functionp message-post-method)
				  (funcall message-post-method)
				message-post-method))
		      result)
		  (run-hooks 'message-send-news-hook)
		  (gnus-open-server method)
		  (message "Sending news via %s..."
			   (gnus-server-string method))
		  (unless (let ((mail-header-separator ""))
			    (gnus-request-post method))
		    (message "Couldn't send message via news: %s"
			     (nnheader-get-report (car method))))))
	       ((string= (gnus-soup-reply-kind (car replies)) "mail")
		(gnus-message 5 "Sending mail to %s..."
			      (mail-fetch-field "to"))
		(sit-for 1)
		(let ((mail-header-separator ""))
                  (funcall (or message-send-mail-real-function
                               message-send-mail-function))))
	       (t
		(error "Unknown reply kind")))
	      (set-buffer msg-buf)
	      (goto-char end))
	    (delete-file (buffer-file-name))
	    (kill-buffer msg-buf)
	    (kill-buffer tmp-buf)
	    (gnus-message 4 "Sent packet"))))
	(setq replies (cdr replies)))
      t)))

(provide 'gnus-soup)

;; arch-tag: eddfa69d-13e8-4aea-84ef-62a526ef185c
;;; gnus-soup.el ends here
