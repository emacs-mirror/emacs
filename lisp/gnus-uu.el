;;; gnus-uu.el --- extract (uu)encoded files in Gnus
;; Copyright (C) 1985,86,87,93,94,95,96 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Created: 2 Oct 1993
;; Keyword: news

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

;;; Code: 

(require 'gnus)
(require 'gnus-msg)
(eval-when-compile (require 'cl))

;; Default viewing action rules

(defvar gnus-uu-default-view-rules 
  '(("\\.te?xt$\\|\\.doc$\\|read.*me\\|\\.c?$\\|\\.h$\\|\\.bat$\\|\\.asm$\\|makefile" "cat %s | sed s/\r//g")
    ("\\.pas$" "cat %s | sed s/\r//g")
    ("\\.[1-9]$" "groff -mandoc -Tascii %s | sed s/\b.//g")
    ("\\.\\(jpe?g\\|gif\\|tiff?\\|p[pgb]m\\|xwd\\|xbm\\|pcx\\)$" "xv")
    ("\\.tga$" "tgatoppm %s | xv -")
    ("\\.\\(wav\\|aiff\\|hcom\\|u[blw]\\|s[bfw]\\|voc\\|smp\\)$" 
     "sox -v .5 %s -t .au -u - > /dev/audio")
    ("\\.au$" "cat %s > /dev/audio")
    ("\\.midi?$" "playmidi -f")
    ("\\.mod$" "str32")
    ("\\.ps$" "ghostview")
    ("\\.dvi$" "xdvi")
    ("\\.html$" "xmosaic")
    ("\\.mpe?g$" "mpeg_play")
    ("\\.\\(flc\\|fli\\|rle\\|iff\\|pfx\\|avi\\|sme\\|rpza\\|dl\\|qt\\|rsrc\\|mov\\)$" "xanim")
    ("\\.\\(tar\\|arj\\|zip\\|zoo\\|arc\\|gz\\|Z\\|lzh\\|ar\\|lha\\)$" 
     "gnus-uu-archive"))
  "*Default actions to be taken when the user asks to view a file.  
To change the behaviour, you can either edit this variable or set
`gnus-uu-user-view-rules' to something useful.

For example:

To make gnus-uu use 'xli' to display JPEG and GIF files, put the
following in your .emacs file:

  (setq gnus-uu-user-view-rules '((\"jpg$\\\\|gif$\" \"xli\")))

Both these variables are lists of lists with two string elements. The
first string is a regular expression. If the file name matches this
regular expression, the command in the second string is executed with
the file as an argument.

If the command string contains \"%s\", the file name will be inserted
at that point in the command string. If there's no \"%s\" in the
command string, the file name will be appended to the command string
before executing.

There are several user variables to tailor the behaviour of gnus-uu to
your needs. First we have `gnus-uu-user-view-rules', which is the
variable gnus-uu first consults when trying to decide how to view a
file. If this variable contains no matches, gnus-uu examines the
default rule variable provided in this package. If gnus-uu finds no
match here, it uses `gnus-uu-user-view-rules-end' to try to make a
match.")

(defvar gnus-uu-user-view-rules nil 
  "*Variable detailing what actions are to be taken to view a file.
See the documentation on the `gnus-uu-default-view-rules' variable for 
details.")

(defvar gnus-uu-user-view-rules-end 
  '(("" "file"))
  "*Variable saying what actions are to be taken if no rule matched the file name.
See the documentation on the `gnus-uu-default-view-rules' variable for 
details.")

;; Default unpacking commands

(defvar gnus-uu-default-archive-rules 
  '(("\\.tar$" "tar xf")
    ("\\.zip$" "unzip -o")
    ("\\.ar$" "ar x")
    ("\\.arj$" "unarj x")
    ("\\.zoo$" "zoo -e")
    ("\\.\\(lzh\\|lha\\)$" "lha x")
    ("\\.Z$" "uncompress")
    ("\\.gz$" "gunzip")
    ("\\.arc$" "arc -x")))

(defvar gnus-uu-destructive-archivers 
  (list "uncompress" "gunzip"))

(defvar gnus-uu-user-archive-rules nil
  "*A list that can be set to override the default archive unpacking commands.
To use, for instance, 'untar' to unpack tar files and 'zip -x' to
unpack zip files, say the following:
  (setq gnus-uu-user-archive-rules 
    '((\"\\\\.tar$\" \"untar\")
      (\"\\\\.zip$\" \"zip -x\")))")

(defvar gnus-uu-ignore-files-by-name nil
  "*A regular expression saying what files should not be viewed based on name.
If, for instance, you want gnus-uu to ignore all .au and .wav files, 
you could say something like

  (setq gnus-uu-ignore-files-by-name \"\\\\.au$\\\\|\\\\.wav$\")

Note that this variable can be used in conjunction with the
`gnus-uu-ignore-files-by-type' variable.")

(defvar gnus-uu-ignore-files-by-type nil
  "*A regular expression saying what files that shouldn't be viewed, based on MIME file type.
If, for instance, you want gnus-uu to ignore all audio files and all mpegs, 
you could say something like

  (setq gnus-uu-ignore-files-by-type \"audio/\\\\|video/mpeg\")

Note that this variable can be used in conjunction with the
`gnus-uu-ignore-files-by-name' variable.")

;; Pseudo-MIME support

(defconst gnus-uu-ext-to-mime-list
  '(("\\.gif$" "image/gif")
    ("\\.jpe?g$" "image/jpeg")
    ("\\.tiff?$" "image/tiff")
    ("\\.xwd$" "image/xwd")
    ("\\.pbm$" "image/pbm")
    ("\\.pgm$" "image/pgm")
    ("\\.ppm$" "image/ppm")
    ("\\.xbm$" "image/xbm")
    ("\\.pcx$" "image/pcx")
    ("\\.tga$" "image/tga")
    ("\\.ps$" "image/postscript")
    ("\\.fli$" "video/fli")
    ("\\.wav$" "audio/wav")
    ("\\.aiff$" "audio/aiff")
    ("\\.hcom$" "audio/hcom")
    ("\\.voc$" "audio/voc")
    ("\\.smp$" "audio/smp")
    ("\\.mod$" "audio/mod")
    ("\\.dvi$" "image/dvi")
    ("\\.mpe?g$" "video/mpeg")
    ("\\.au$" "audio/basic")
    ("\\.\\(te?xt\\|doc\\|c\\|h\\)$" "text/plain")
    ("\\.\\(c\\|h\\)$" "text/source")
    ("read.*me" "text/plain")
    ("\\.html$" "text/html")
    ("\\.bat$" "text/bat")
    ("\\.[1-6]$" "text/man")
    ("\\.flc$" "video/flc")
    ("\\.rle$" "video/rle")
    ("\\.pfx$" "video/pfx")
    ("\\.avi$" "video/avi")
    ("\\.sme$" "video/sme")
    ("\\.rpza$" "video/prza")
    ("\\.dl$" "video/dl")
    ("\\.qt$" "video/qt")
    ("\\.rsrc$" "video/rsrc")
    ("\\..*$" "unknown/unknown")))

;; Various variables users may set 

(defvar gnus-uu-tmp-dir "/tmp/" 
  "*Variable saying where gnus-uu is to do its work.
Default is \"/tmp/\".")

(defvar gnus-uu-do-not-unpack-archives nil 
  "*Non-nil means that gnus-uu won't peek inside archives looking for files to display. 
Default is nil.")

(defvar gnus-uu-ignore-default-view-rules nil
  "*Non-nil means that gnus-uu will ignore the default viewing rules.
Only the user viewing rules will be consulted. Default is nil.")

(defvar gnus-uu-grabbed-file-functions nil
  "*Functions run on each file after successful decoding.
They will be called with the name of the file as the argument.
Likely functions you can use in this list are `gnus-uu-grab-view' 
and `gnus-uu-grab-move'.")

(defvar gnus-uu-ignore-default-archive-rules nil 
  "*Non-nil means that gnus-uu will ignore the default archive unpacking commands.  
Only the user unpacking commands will be consulted. Default is nil.")

(defvar gnus-uu-kill-carriage-return t
  "*Non-nil means that gnus-uu will strip all carriage returns from articles.
Default is t.")

(defvar gnus-uu-view-with-metamail nil
  "*Non-nil means that files will be viewed with metamail.
The gnus-uu viewing functions will be ignored and gnus-uu will try
to guess at a content-type based on file name suffixes. Default
it nil.")

(defvar gnus-uu-unmark-articles-not-decoded nil
  "*Non-nil means that gnus-uu will mark articles that were unsuccessfully decoded as unread. 
Default is nil.")

(defvar gnus-uu-correct-stripped-uucode nil
  "*Non-nil means that gnus-uu will *try* to fix uuencoded files that have had trailing spaces deleted. 
Default is nil.")

(defvar gnus-uu-save-in-digest nil
  "*Non-nil means that gnus-uu, when asked to save without decoding, will save in digests.
If this variable is nil, gnus-uu will just save everything in a 
file without any embellishments. The digesting almost conforms to RFC1153 -
no easy way to specify any meaningful volume and issue numbers were found, 
so I simply dropped them.")

(defvar gnus-uu-digest-headers 
  '("^Date:" "^From:" "^To:" "^Cc:" "^Subject:" "^Message-ID:" "^Keywords:"
    "^Summary:" "^References:")
  "*List of regexps to match headers included in digested messages.
The headers will be included in the sequence they are matched.")

(defvar gnus-uu-save-separate-articles nil
  "*Non-nil means that gnus-uu will save articles in separate files.")

;; Internal variables

(defvar gnus-uu-saved-article-name nil)

(defconst gnus-uu-begin-string "^begin[ \t]+[0-7][0-7][0-7][ \t]+\\(.*\\)$")
(defconst gnus-uu-end-string "^end[ \t]*$")

(defconst gnus-uu-body-line "^M")
(let ((i 61))
  (while (> (setq i (1- i)) 0)
    (setq gnus-uu-body-line (concat gnus-uu-body-line "[^a-z]")))
  (setq gnus-uu-body-line (concat gnus-uu-body-line ".?$")))

;"^M.............................................................?$"

(defconst gnus-uu-shar-begin-string "^#! */bin/sh")

(defvar gnus-uu-shar-file-name nil)
(defconst gnus-uu-shar-name-marker "begin [0-7][0-7][0-7][ \t]+\\(\\(\\w\\|\\.\\)*\\b\\)")

(defconst gnus-uu-postscript-begin-string "^%!PS-")
(defconst gnus-uu-postscript-end-string "^%%EOF$")

(defvar gnus-uu-file-name nil)
(defconst gnus-uu-uudecode-process nil)
(defvar gnus-uu-binhex-article-name nil)

(defvar gnus-uu-work-dir nil)

(defconst gnus-uu-output-buffer-name " *Gnus UU Output*")

(defvar gnus-uu-default-dir gnus-article-save-directory)
(defvar gnus-uu-digest-from-subject nil)

;; Keymaps

(gnus-define-keys 
 (gnus-uu-mark-map "P" gnus-summary-mark-map)
 "p" gnus-summary-mark-as-processable
 "u" gnus-summary-unmark-as-processable
 "U" gnus-summary-unmark-all-processable
 "v" gnus-uu-mark-over
 "s" gnus-uu-mark-series
 "r" gnus-uu-mark-region
 "R" gnus-uu-mark-by-regexp
 "t" gnus-uu-mark-thread
 "T" gnus-uu-unmark-thread
 "a" gnus-uu-mark-all
 "b" gnus-uu-mark-buffer
 "S" gnus-uu-mark-sparse)

(gnus-define-keys 
 (gnus-uu-extract-map "X" gnus-summary-mode-map)
 ;;"x" gnus-uu-extract-any
 ;;"m" gnus-uu-extract-mime
 "u" gnus-uu-decode-uu
 "U" gnus-uu-decode-uu-and-save
 "s" gnus-uu-decode-unshar
 "S" gnus-uu-decode-unshar-and-save
 "o" gnus-uu-decode-save
 "O" gnus-uu-decode-save
 "b" gnus-uu-decode-binhex
 "B" gnus-uu-decode-binhex
 "p" gnus-uu-decode-postscript
 "P" gnus-uu-decode-postscript-and-save)

(gnus-define-keys 
 (gnus-uu-extract-view-map "v" gnus-uu-extract-map)
 "u" gnus-uu-decode-uu-view
 "U" gnus-uu-decode-uu-and-save-view
 "s" gnus-uu-decode-unshar-view
 "S" gnus-uu-decode-unshar-and-save-view
 "o" gnus-uu-decode-save-view
 "O" gnus-uu-decode-save-view
 "b" gnus-uu-decode-binhex-view
 "B" gnus-uu-decode-binhex-view
 "p" gnus-uu-decode-postscript-view
 "P" gnus-uu-decode-postscript-and-save-view)


;; Commands.

(defun gnus-uu-decode-uu (&optional n)
  "Uudecodes the current article."
  (interactive "P") 
  (gnus-uu-decode-with-method 'gnus-uu-uustrip-article n))

(defun gnus-uu-decode-uu-and-save (n dir)
  "Decodes and saves the resulting file."
  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (read-file-name "Uudecode and save in dir: "
			  gnus-uu-default-dir
			  gnus-uu-default-dir t))))
  (gnus-uu-decode-with-method 'gnus-uu-uustrip-article n dir nil nil t))

(defun gnus-uu-decode-unshar (&optional n)
  "Unshars the current article."
  (interactive "P")
  (gnus-uu-decode-with-method 'gnus-uu-unshar-article n nil nil 'scan t))

(defun gnus-uu-decode-unshar-and-save (n dir)
  "Unshars and saves the current article."
  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (read-file-name "Unshar and save in dir: "
			  gnus-uu-default-dir
			  gnus-uu-default-dir t))))
  (gnus-uu-decode-with-method 'gnus-uu-unshar-article n dir nil 'scan t))

(defun gnus-uu-decode-save (n file)
  "Saves the current article."
  (interactive
   (list current-prefix-arg
	 (read-file-name 
	  (if gnus-uu-save-separate-articles
	      "Save articles is dir: "
	    "Save articles in file: ")
	  gnus-uu-default-dir
	  gnus-uu-default-dir)))
  (setq gnus-uu-saved-article-name file)
  (gnus-uu-decode-with-method 'gnus-uu-save-article n nil t))

(defun gnus-uu-decode-binhex (n dir)
  "Unbinhexes the current article."
  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (read-file-name "Unbinhex and save in dir: "
			  gnus-uu-default-dir
			  gnus-uu-default-dir))))
  (setq gnus-uu-binhex-article-name 
	(make-temp-name (concat gnus-uu-work-dir "binhex")))
  (gnus-uu-decode-with-method 'gnus-uu-binhex-article n dir))

(defun gnus-uu-decode-uu-view (&optional n)
  "Uudecodes and views the current article."    
  (interactive "P")
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-uu n)))

(defun gnus-uu-decode-uu-and-save-view (n dir)
  "Decodes, views and saves the resulting file."
  (interactive
   (list current-prefix-arg
	 (read-file-name "Uudecode, view and save in dir: "
			 gnus-uu-default-dir
			 gnus-uu-default-dir t)))
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-uu-and-save n dir)))

(defun gnus-uu-decode-unshar-view (&optional n)
  "Unshars and views the current article."
  (interactive "P")
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-unshar n)))

(defun gnus-uu-decode-unshar-and-save-view (n dir)
  "Unshars and saves the current article."
  (interactive
   (list current-prefix-arg
	 (read-file-name "Unshar, view and save in dir: "
			 gnus-uu-default-dir
			 gnus-uu-default-dir t)))
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-unshar-and-save n dir)))

(defun gnus-uu-decode-save-view (n file)
  "Saves and views the current article."
  (interactive
   (list current-prefix-arg
	 (read-file-name  (if gnus-uu-save-separate-articles
			      "Save articles is dir: "
			    "Save articles in file: ")
			  gnus-uu-default-dir gnus-uu-default-dir)))
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-save n file)))

(defun gnus-uu-decode-binhex-view (n file)
  "Unbinhexes and views the current article."
  (interactive
   (list current-prefix-arg
	 (read-file-name "Unbinhex, view and save in dir: "
			 gnus-uu-default-dir gnus-uu-default-dir)))
  (setq gnus-uu-binhex-article-name 
	(make-temp-name (concat gnus-uu-work-dir "binhex")))
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-binhex n file)))


;; Digest and forward articles

(defun gnus-uu-digest-mail-forward (&optional n post)
  "Digests and forwards all articles in this series."
  (interactive "P")
  (let ((gnus-uu-save-in-digest t)
	(file (make-temp-name (concat gnus-uu-tmp-dir "forward")))
	buf subject from)
    (setq gnus-uu-digest-from-subject nil)
    (gnus-uu-decode-save n file)
    (setq buf (switch-to-buffer (get-buffer-create " *gnus-uu-forward*")))
    (gnus-add-current-to-buffer-list)
    (erase-buffer)
    (delete-other-windows)
    (insert-file file)
    (let ((fs gnus-uu-digest-from-subject))
      (if (not fs)
	  ()
	(setq from (caar fs)
	      subject (gnus-simplify-subject-fuzzy (cdar fs))
	      fs (cdr fs))
	(while (and fs (or from subject))
	  (and from
	       (or (string= from (caar fs))
		   (setq from nil)))
	  (and subject
	       (or (string= (gnus-simplify-subject-fuzzy (cdar fs))
			    subject)
		   (setq subject nil)))
	  (setq fs (cdr fs))))
      (or subject (setq subject "Digested Articles"))
      (or from (setq from "Various")))
    (goto-char (point-min))
    (and (re-search-forward "^Subject: ")
	 (progn
	   (delete-region (point) (gnus-point-at-eol))
	   (insert subject)))
    (goto-char (point-min))
    (and (re-search-forward "^From: ")
	 (progn
	   (delete-region (point) (gnus-point-at-eol))
	   (insert from)))
    (message-forward post)
    (delete-file file)
    (kill-buffer buf)
    (setq gnus-uu-digest-from-subject nil)))

(defun gnus-uu-digest-post-forward (&optional n)
  "Digest and forward to a newsgroup."
  (interactive "P")
  (gnus-uu-digest-mail-forward n t))

;; Process marking.

(defun gnus-uu-mark-by-regexp (regexp &optional unmark)
  "Ask for a regular expression and set the process mark on all articles that match."
  (interactive (list (read-from-minibuffer "Mark (regexp): ")))
  (gnus-set-global-variables)
  (let ((articles (gnus-uu-find-articles-matching regexp)))
    (while articles
      (if unmark
	  (gnus-summary-remove-process-mark (pop articles))
	(gnus-summary-set-process-mark (pop articles))))
    (message ""))
  (gnus-summary-position-point))

(defun gnus-uu-unmark-by-regexp (regexp &optional unmark)
  "Ask for a regular expression and remove the process mark on all articles that match."
  (interactive (list (read-from-minibuffer "Mark (regexp): ")))
  (gnus-uu-mark-by-regexp regexp t))

(defun gnus-uu-mark-series ()
  "Mark the current series with the process mark."
  (interactive)
  (gnus-set-global-variables)
  (let ((articles (gnus-uu-find-articles-matching)))
    (while articles
      (gnus-summary-set-process-mark (car articles))
      (setq articles (cdr articles)))
    (message ""))
  (gnus-summary-position-point))

(defun gnus-uu-mark-region (beg end &optional unmark)
  "Set the process mark on all articles between point and mark."
  (interactive "r")
  (gnus-set-global-variables)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (if unmark
	  (gnus-summary-remove-process-mark (gnus-summary-article-number))
	(gnus-summary-set-process-mark (gnus-summary-article-number)))
      (forward-line 1)))
  (gnus-summary-position-point))

(defun gnus-uu-unmark-region (beg end)
  "Remove the process mark from all articles between point and mark."
  (interactive "r")
  (gnus-uu-mark-region beg end t))

(defun gnus-uu-mark-buffer ()
  "Set the process mark on all articles in the buffer."
  (interactive)
  (gnus-uu-mark-region (point-min) (point-max)))
      
(defun gnus-uu-unmark-buffer ()
  "Remove the process mark on all articles in the buffer."
  (interactive)
  (gnus-uu-mark-region (point-min) (point-max) t))
      
(defun gnus-uu-mark-thread ()
  "Marks all articles downwards in this thread."
  (interactive)
  (gnus-set-global-variables)
  (let ((level (gnus-summary-thread-level)))
    (while (and (gnus-summary-set-process-mark (gnus-summary-article-number))
		(zerop (gnus-summary-next-subject 1))
		(> (gnus-summary-thread-level) level))))
  (gnus-summary-position-point))

(defun gnus-uu-unmark-thread ()
  "Unmarks all articles downwards in this thread."
  (interactive)
  (gnus-set-global-variables)
  (let ((level (gnus-summary-thread-level)))
    (while (and (gnus-summary-remove-process-mark
		 (gnus-summary-article-number))
		(zerop (gnus-summary-next-subject 1))
		(> (gnus-summary-thread-level) level))))
  (gnus-summary-position-point))

(defun gnus-uu-mark-over (&optional score)
  "Mark all articles with a score over SCORE (the prefix.)"
  (interactive "P")
  (let ((score (gnus-score-default score))
	(data gnus-newsgroup-data))
    (save-excursion
      (while data
	(when (> (or (cdr (assq (gnus-data-number (caar data))
				gnus-newsgroup-scored))
		     gnus-summary-default-score 0)
		 score)
	  (gnus-summary-set-process-mark (caar data)))
	(setq data (cdr data))))
    (gnus-summary-position-point)))

(defun gnus-uu-mark-sparse ()
  "Mark all series that have some articles marked."
  (interactive)
  (gnus-set-global-variables)
  (let ((marked (nreverse gnus-newsgroup-processable))
	subject articles total headers)
    (or marked (error "No articles marked with the process mark"))
    (setq gnus-newsgroup-processable nil)
    (save-excursion
      (while marked
	(and (vectorp (setq headers 
			    (gnus-summary-article-header (car marked))))
	     (setq subject (mail-header-subject headers)
		   articles (gnus-uu-find-articles-matching 
			     (gnus-uu-reginize-string subject))
		   total (nconc total articles)))
	(while articles
	  (gnus-summary-set-process-mark (car articles))
	  (setcdr marked (delq (car articles) (cdr marked)))
	  (setq articles (cdr articles)))
	(setq marked (cdr marked)))
      (setq gnus-newsgroup-processable (nreverse total)))
    (gnus-summary-position-point)))

(defun gnus-uu-mark-all ()
  "Mark all articles in \"series\" order."
  (interactive)
  (gnus-set-global-variables)
  (setq gnus-newsgroup-processable nil)
  (save-excursion
    (let ((data gnus-newsgroup-data)
	  number)
      (while data
	(when (and (not (memq (setq number (gnus-data-number (car data)))
			      gnus-newsgroup-processable))
		   (vectorp (gnus-data-header (car data))))
	  (gnus-summary-goto-subject number)
	  (gnus-uu-mark-series))
	(setq data (cdr data)))))
  (gnus-summary-position-point))

;; All PostScript functions written by Erik Selberg <speed@cs.washington.edu>. 

(defun gnus-uu-decode-postscript (&optional n)
  "Gets postscript of the current article."
  (interactive "P")
  (gnus-uu-decode-with-method 'gnus-uu-decode-postscript-article n))

(defun gnus-uu-decode-postscript-view (&optional n)
  "Gets and views the current article."
  (interactive "P")
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-postscript n)))

(defun gnus-uu-decode-postscript-and-save (n dir)
  "Extracts postscript and saves the current article."
  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (read-file-name "Save in dir: "
			  gnus-uu-default-dir
			  gnus-uu-default-dir t))))
  (gnus-uu-decode-with-method 'gnus-uu-decode-postscript-article 
			      n dir nil nil t))

(defun gnus-uu-decode-postscript-and-save-view (n dir)
  "Decodes, views and saves the resulting file."
  (interactive
   (list current-prefix-arg
	 (read-file-name "Where do you want to save the file(s)? "
			 gnus-uu-default-dir
			 gnus-uu-default-dir t)))
  (let ((gnus-view-pseudos (or gnus-view-pseudos 'automatic)))
    (gnus-uu-decode-postscript-and-save n dir)))


;; Internal functions.

(defun gnus-uu-decode-with-method (method n &optional save not-insert 
					  scan cdir)
  (gnus-uu-initialize scan)
  (if save (setq gnus-uu-default-dir save))
  ;; Create the directory we save to.
  (when (and scan cdir save
	     (not (file-exists-p save)))
    (make-directory save t))
  (let ((articles (gnus-uu-get-list-of-articles n))
	files)
    (setq files (gnus-uu-grab-articles articles method t))
    (let ((gnus-current-article (car articles)))
      (and scan (setq files (gnus-uu-scan-directory gnus-uu-work-dir))))
    (and save (gnus-uu-save-files files save))
    (if (eq gnus-uu-do-not-unpack-archives nil)
      (setq files (gnus-uu-unpack-files files)))
    (setq files (nreverse (gnus-uu-get-actions files)))
    (or not-insert (not gnus-insert-pseudo-articles)
	(gnus-summary-insert-pseudos files save))))

(defun gnus-uu-scan-directory (dir &optional rec)
  "Return a list of all files under DIR."
  (let ((files (directory-files dir t))
	out file)
    (while (setq file (pop files))
      (unless (member (file-name-nondirectory file) '("." ".."))
	(push (list (cons 'name file)
		    (cons 'article gnus-current-article))
	      out)
	(when (file-directory-p file)
	  (setq out (nconc (gnus-uu-scan-directory file t) out)))))
    (if rec 
	out
      (nreverse out))))

(defun gnus-uu-save-files (files dir)
  "Save FILES in DIR."
  (let ((len (length files))
	(reg (concat "^" (regexp-quote gnus-uu-work-dir)))
	to-file file fromdir)
    (while (setq file (cdr (assq 'name (pop files))))
      (when (file-exists-p file)
	(string-match reg file)
	(setq fromdir (substring file (match-end 0)))
	(if (file-directory-p file)
	    (unless (file-exists-p (concat dir fromdir))
	      (make-directory (concat dir fromdir) t))
	  (setq to-file (concat dir fromdir))
	  (when (or (not (file-exists-p to-file))
		    (gnus-y-or-n-p (format "%s exists; overwrite? " to-file)))
	    (copy-file file to-file t t)))))
    (gnus-message 5 "Saved %d file%s" len (if (= len 1) "" "s"))))

;; Functions for saving and possibly digesting articles without
;; any decoding.

;; Function called by gnus-uu-grab-articles to treat each article.
(defun gnus-uu-save-article (buffer in-state)
  (cond 
   (gnus-uu-save-separate-articles
    (save-excursion
      (set-buffer buffer)
      (write-region 1 (point-max) (concat gnus-uu-saved-article-name 
					  gnus-current-article))
      (cond ((eq in-state 'first) (list gnus-uu-saved-article-name 'begin))
	    ((eq in-state 'first-and-last) (list gnus-uu-saved-article-name 
						 'begin 'end))
	    ((eq in-state 'last) (list 'end))
	    (t (list 'middle)))))
   ((not gnus-uu-save-in-digest)
    (save-excursion
      (set-buffer buffer)
      (write-region 1 (point-max) gnus-uu-saved-article-name t)
      (cond ((eq in-state 'first) (list gnus-uu-saved-article-name 'begin))
	    ((eq in-state 'first-and-last) (list gnus-uu-saved-article-name 
						 'begin 'end))
	    ((eq in-state 'last) (list 'end))
	    (t (list 'middle)))))
   (t
    (let ((header (gnus-summary-article-header)))
      (setq gnus-uu-digest-from-subject
	    (cons (cons (mail-header-from header)
			(mail-header-subject header))
		  gnus-uu-digest-from-subject)))
    (let ((name (file-name-nondirectory gnus-uu-saved-article-name))
	  (delim (concat "^" (make-string 30 ?-) "$"))
	  beg subj headers headline sorthead body end-string state)
      (if (or (eq in-state 'first) 
	      (eq in-state 'first-and-last))
	  (progn 
	    (setq state (list 'begin))
	    (save-excursion (set-buffer (get-buffer-create "*gnus-uu-body*"))
			    (erase-buffer))
	    (save-excursion 
	      (set-buffer (get-buffer-create "*gnus-uu-pre*"))
	      (erase-buffer)
	      (insert (format 
		       "Date: %s\nFrom: %s\nSubject: %s Digest\n\nTopics:\n"
		       (current-time-string) name name))))
	(if (not (eq in-state 'end))
	    (setq state (list 'middle))))
      (save-excursion
	(set-buffer (get-buffer "*gnus-uu-body*"))
	(goto-char (setq beg (point-max)))
	(save-excursion
	  (save-restriction
	    (set-buffer buffer)
	    (let (buffer-read-only)
	      (gnus-set-text-properties (point-min) (point-max) nil)
	      ;; These two are necessary for XEmacs 19.12 fascism.
	      (put-text-property (point-min) (point-max) 'invisible nil)
	      (put-text-property (point-min) (point-max) 'intangible nil))
	    (goto-char (point-min))
	    (re-search-forward "\n\n")
	    ;; Quote all 30-dash lines.
	    (save-excursion
	      (while (re-search-forward delim nil t)
		(beginning-of-line)
		(delete-char 1)
		(insert " ")))
	    (setq body (buffer-substring (1- (point)) (point-max)))
	    (narrow-to-region (point-min) (point))
	    (if (not (setq headers gnus-uu-digest-headers))
		(setq sorthead (buffer-substring (point-min) (point-max)))
	      (while headers
		(setq headline (car headers))
		(setq headers (cdr headers))
		(goto-char (point-min))
		(while (re-search-forward headline nil t)
		  (setq sorthead 
			(concat sorthead
				(buffer-substring 
				 (match-beginning 0)
				 (or (and (re-search-forward "^[^ \t]" nil t)
					  (1- (point)))
				     (progn (forward-line 1) (point)))))))))
	    (widen)))
	(insert sorthead) (goto-char (point-max))
	(insert body) (goto-char (point-max))
	(insert (concat "\n" (make-string 30 ?-) "\n\n"))
	(goto-char beg)
	(if (re-search-forward "^Subject: \\(.*\\)$" nil t)
	    (progn
	      (setq subj (buffer-substring (match-beginning 1) (match-end 1)))
	      (save-excursion 
		(set-buffer (get-buffer "*gnus-uu-pre*"))
		(insert (format "   %s\n" subj))))))
      (if (or (eq in-state 'last)
	      (eq in-state 'first-and-last))
	  (progn
	    (save-excursion
	      (set-buffer (get-buffer "*gnus-uu-pre*"))
	      (insert (format "\n\n%s\n\n" (make-string 70 ?-)))
	      (write-region 1 (point-max) gnus-uu-saved-article-name))
	    (save-excursion
	      (set-buffer (get-buffer "*gnus-uu-body*"))
	      (goto-char (point-max))
	      (insert 
	       (concat (setq end-string (format "End of %s Digest" name)) 
		       "\n"))
	      (insert (concat (make-string (length end-string) ?*) "\n"))
	      (write-region 1 (point-max) gnus-uu-saved-article-name t))
	    (kill-buffer (get-buffer "*gnus-uu-pre*"))
	    (kill-buffer (get-buffer "*gnus-uu-body*"))
	    (setq state (cons 'end state))))
      (if (memq 'begin state)
	  (cons gnus-uu-saved-article-name state)
	state)))))

;; Binhex treatment - not very advanced. 

(defconst gnus-uu-binhex-body-line 
  "^[^:]...............................................................$")
(defconst gnus-uu-binhex-begin-line 
  "^:...............................................................$")
(defconst gnus-uu-binhex-end-line
  ":$")

(defun gnus-uu-binhex-article (buffer in-state)
  (let (state start-char)
    (save-excursion
      (set-buffer buffer)
      (widen)
      (goto-char (point-min))
      (if (not (re-search-forward gnus-uu-binhex-begin-line nil t))
	  (if (not (re-search-forward gnus-uu-binhex-body-line nil t))
	      (setq state (list 'wrong-type))))

      (if (memq 'wrong-type state)
	  ()
	(beginning-of-line)
	(setq start-char (point))
	(if (looking-at gnus-uu-binhex-begin-line)
	    (progn
	      (setq state (list 'begin))
	      (write-region 1 1 gnus-uu-binhex-article-name))
	  (setq state (list 'middle)))
	(goto-char (point-max))
	(re-search-backward (concat gnus-uu-binhex-body-line "\\|" 
				    gnus-uu-binhex-end-line) nil t)
	(if (looking-at gnus-uu-binhex-end-line)
	    (setq state (if (memq 'begin state)
			    (cons 'end state)
			  (list 'end))))
	(beginning-of-line)
	(forward-line 1)
	(if (file-exists-p gnus-uu-binhex-article-name)
	    (append-to-file start-char (point) gnus-uu-binhex-article-name))))
    (if (memq 'begin state)
	(cons gnus-uu-binhex-article-name state)
      state)))

;; PostScript

(defun gnus-uu-decode-postscript-article (process-buffer in-state)
  (let ((state (list 'ok))
	start-char end-char file-name)
    (save-excursion
      (set-buffer process-buffer)
      (goto-char (point-min))
      (if (not (re-search-forward gnus-uu-postscript-begin-string nil t))
	  (setq state (list 'wrong-type))
	(beginning-of-line)
	(setq start-char (point))
	(if (not (re-search-forward gnus-uu-postscript-end-string nil t))
	    (setq state (list 'wrong-type))
	  (setq end-char (point))
	  (set-buffer (get-buffer-create gnus-uu-output-buffer-name))
	  (insert-buffer-substring process-buffer start-char end-char)
	  (setq file-name (concat gnus-uu-work-dir
				  (cdr gnus-article-current) ".ps"))
	  (write-region (point-min) (point-max) file-name)
	  (setq state (list file-name 'begin 'end)))))
    state))
      

;; Find actions.

(defun gnus-uu-get-actions (files)
  (let ((ofiles files)
	action name)
    (while files
      (setq name (cdr (assq 'name (car files))))
      (and 
       (setq action (gnus-uu-get-action name))
       (setcar files (nconc (list (if (string= action "gnus-uu-archive")
				      (cons 'action "file")
				    (cons 'action action))
				  (cons 'execute (gnus-uu-command
						  action name)))
			    (car files))))
      (setq files (cdr files)))
    ofiles))

(defun gnus-uu-get-action (file-name)
  (let (action)
    (setq action 
	  (gnus-uu-choose-action 
	   file-name
	   (append 
	    gnus-uu-user-view-rules
	    (if gnus-uu-ignore-default-view-rules 
		nil 
	      gnus-uu-default-view-rules)
	    gnus-uu-user-view-rules-end)))
    (if (and (not (string= (or action "") "gnus-uu-archive")) 
	     gnus-uu-view-with-metamail)
	(if (setq action 
		  (gnus-uu-choose-action file-name gnus-uu-ext-to-mime-list))
	    (setq action (format "metamail -d -b -c \"%s\"" action))))
    action))


;; Functions for treating subjects and collecting series.

(defun gnus-uu-reginize-string (string)
  ;; Takes a string and puts a \ in front of every special character;
  ;; ignores any leading "version numbers" thingies that they use in
  ;; the comp.binaries groups, and either replaces anything that looks
  ;; like "2/3" with "[0-9]+/[0-9]+" or, if it can't find something
  ;; like that, replaces the last two numbers with "[0-9]+". This, in
  ;; my experience, should get most postings of a series.
  (let ((count 2)
	(vernum "v[0-9]+[a-z][0-9]+:")
	beg)
    (save-excursion
      (set-buffer (get-buffer-create gnus-uu-output-buffer-name))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (insert (regexp-quote string))
      (setq beg 1)

      (setq case-fold-search nil)
      (goto-char (point-min))
      (if (looking-at vernum)
	  (progn
	    (replace-match vernum t t)
	    (setq beg (length vernum))))

      (goto-char beg)
      (if (re-search-forward "[ \t]*[0-9]+/[0-9]+" nil t)
	  (replace-match " [0-9]+/[0-9]+")

	(goto-char beg)
	(if (re-search-forward "[0-9]+[ \t]*of[ \t]*[0-9]+" nil t)
	    (replace-match "[0-9]+ of [0-9]+")

	  (end-of-line)
	  (while (and (re-search-backward "[0-9]" nil t) (> count 0))
            (while (and 
		    (looking-at "[0-9]") 
		    (< 1 (goto-char (1- (point))))))
            (re-search-forward "[0-9]+" nil t)
	    (replace-match "[0-9]+")
	    (backward-char 5)
	    (setq count (1- count)))))

      (goto-char beg)
      (while (re-search-forward "[ \t]+" nil t)
	(replace-match "[ \t]*" t t))

      (buffer-substring 1 (point-max)))))

(defun gnus-uu-get-list-of-articles (n)
  ;; If N is non-nil, the article numbers of the N next articles
  ;; will be returned.
  ;; If any articles have been marked as processable, they will be
  ;; returned. 
  ;; Failing that, articles that have subjects that are part of the
  ;; same "series" as the current will be returned.
  (let (articles)
    (cond 
     (n
      (let ((backward (< n 0))
	    (n (abs n)))
	(save-excursion
	  (while (and (> n 0)
		      (setq articles (cons (gnus-summary-article-number) 
					   articles))
		      (gnus-summary-search-forward nil nil backward))
	    (setq n (1- n))))
	(nreverse articles)))
     (gnus-newsgroup-processable
      (reverse gnus-newsgroup-processable))
     (t
      (gnus-uu-find-articles-matching)))))

(defun gnus-uu-string< (l1 l2)
  (string< (car l1) (car l2)))

(defun gnus-uu-find-articles-matching 
  (&optional subject only-unread do-not-translate)
  ;; Finds all articles that matches the regexp SUBJECT.  If it is
  ;; nil, the current article name will be used. If ONLY-UNREAD is
  ;; non-nil, only unread articles are chosen. If DO-NOT-TRANSLATE is
  ;; non-nil, article names are not equalized before sorting.
  (let ((subject (or subject 
		     (gnus-uu-reginize-string (gnus-summary-article-subject))))
	list-of-subjects)
    (save-excursion
      (if (not subject)
	  ()
	;; Collect all subjects matching subject.
	(let ((case-fold-search t)
	      (data gnus-newsgroup-data)
	      subj mark d)
	  (while data
	    (setq d (pop data))
	    (and (not (gnus-data-pseudo-p d))
		 (or (not only-unread)
		     (= (setq mark (gnus-data-mark d))
			gnus-unread-mark)
		     (= mark gnus-ticked-mark)
		     (= mark gnus-dormant-mark))
		 (setq subj (mail-header-subject (gnus-data-header d)))
		 (string-match subject subj)
		 (setq list-of-subjects 
		       (cons (cons subj (gnus-data-number d))
			     list-of-subjects)))))

	;; Expand numbers, sort, and return the list of article
	;; numbers.
	(mapcar (lambda (sub) (cdr sub)) 
		(sort (gnus-uu-expand-numbers 
		       list-of-subjects
		       (not do-not-translate)) 
		      'gnus-uu-string<))))))

(defun gnus-uu-expand-numbers (string-list &optional translate)
  ;; Takes a list of strings and "expands" all numbers in all the
  ;; strings.  That is, this function makes all numbers equal length by
  ;; prepending lots of zeroes before each number. This is to ease later
  ;; sorting to find out what sequence the articles are supposed to be
  ;; decoded in. Returns the list of expanded strings.
  (let ((out-list string-list)
	string)
    (save-excursion
      (set-buffer (get-buffer-create gnus-uu-output-buffer-name))
      (buffer-disable-undo (current-buffer))
      (while string-list
	(erase-buffer)
	(insert (caar string-list))
	;; Translate multiple spaces to one space.
	(goto-char (point-min))
	(while (re-search-forward "[ \t]+" nil t)
	  (replace-match " "))
	;; Translate all characters to "a".
	(goto-char (point-min))
	(if translate 
	    (while (re-search-forward "[A-Za-z]" nil t)
	      (replace-match "a" t t)))
	;; Expand numbers.
	(goto-char (point-min))
	(while (re-search-forward "[0-9]+" nil t)
	  (replace-match  
	   (format "%06d" 
		   (string-to-int (buffer-substring 
				   (match-beginning 0) (match-end 0))))))
	(setq string (buffer-substring 1 (point-max)))
	(setcar (car string-list) string)
	(setq string-list (cdr string-list))))
    out-list))


;; `gnus-uu-grab-articles' is the general multi-article treatment
;; function.  It takes a list of articles to be grabbed and a function
;; to apply to each article.
;;
;; The function to be called should take two parameters.  The first
;; parameter is the article buffer. The function should leave the
;; result, if any, in this buffer. Most treatment functions will just
;; generate files...
;;
;; The second parameter is the state of the list of articles, and can
;; have four values: `first', `middle', `last' and `first-and-last'.
;;
;; The function should return a list. The list may contain the
;; following symbols:
;; `error' if an error occurred
;; `begin' if the beginning of an encoded file has been received
;;   If the list returned contains a `begin', the first element of
;;   the list *must* be a string with the file name of the decoded
;;   file.
;; `end' if the the end of an encoded file has been received
;; `middle' if the article was a body part of an encoded file
;; `wrong-type' if the article was not a part of an encoded file
;; `ok', which can be used everything is ok

(defvar gnus-uu-has-been-grabbed nil)

(defun gnus-uu-unmark-list-of-grabbed (&optional dont-unmark-last-article)
  (let (art)
    (if (not (and gnus-uu-has-been-grabbed
		  gnus-uu-unmark-articles-not-decoded))
	()
      (if dont-unmark-last-article
	  (progn
	    (setq art (car gnus-uu-has-been-grabbed))
	    (setq gnus-uu-has-been-grabbed (cdr gnus-uu-has-been-grabbed))))
      (while gnus-uu-has-been-grabbed
	(gnus-summary-tick-article (car gnus-uu-has-been-grabbed) t)
	(setq gnus-uu-has-been-grabbed (cdr gnus-uu-has-been-grabbed)))
      (if dont-unmark-last-article
	  (setq gnus-uu-has-been-grabbed (list art))))))

;; This function takes a list of articles and a function to apply to
;; each article grabbed. 
;; 
;; This function returns a list of files decoded if the grabbing and
;; the process-function has been successful and nil otherwise.
(defun gnus-uu-grab-articles (articles process-function 
				       &optional sloppy limit no-errors)
  (let ((state 'first) 
	has-been-begin article result-file result-files process-state
	gnus-summary-display-article-function
	gnus-article-display-hook gnus-article-prepare-hook
	article-series files)
 
    (while (and articles 
		(not (memq 'error process-state))
		(or sloppy
		    (not (memq 'end process-state))))

      (setq article (pop articles))
      (push article article-series)

      (unless articles 
	(if (eq state 'first)
	    (setq state 'first-and-last)
	  (setq state 'last)))

      (let ((part (gnus-uu-part-number article)))
	(gnus-message 6 "Getting article %d%s..." 
		      article (if (string= part "") "" (concat ", " part))))
      (gnus-summary-display-article article)
      
      ;; Push the article to the processing function.
      (save-excursion
	(set-buffer gnus-original-article-buffer)
	(let ((buffer-read-only nil))
	  (save-excursion
	    (set-buffer gnus-summary-buffer)
	    (setq process-state 
		  (funcall process-function
			   gnus-original-article-buffer state)))))

      (gnus-summary-remove-process-mark article)

      ;; If this is the beginning of a decoded file, we push it 
      ;; on to a list.
      (when (or (memq 'begin process-state)
		(and (or (eq state 'first) 
			 (eq state 'first-and-last))
		     (memq 'ok process-state)))
	(if has-been-begin
	    ;; If there is a `result-file' here, that means that the
	    ;; file was unsuccessfully decoded, so we delete it.
	    (when (and result-file 
		       (file-exists-p result-file)) 
	      (delete-file result-file)))
	(when (memq 'begin process-state)
	  (setq result-file (car process-state)))
	(setq has-been-begin t))

      ;; Check whether we have decoded one complete file.
      (when (memq 'end process-state)
	(setq article-series nil)
	(setq has-been-begin nil)
	(if (stringp result-file)
	    (setq files (list result-file))
	  (setq files result-file))
	(setq result-file (car files))
	(while files
	  (push (list (cons 'name (pop files))
		      (cons 'article article))
		result-files))
	;; Allow user-defined functions to be run on this file.
	(when gnus-uu-grabbed-file-functions
	  (let ((funcs gnus-uu-grabbed-file-functions))
	    (unless (listp funcs)
	      (setq funcs (list funcs)))
	    (while funcs
	      (funcall (pop funcs) result-file))))
	;; Check whether we have decoded enough articles.
	(and limit (= (length result-files) limit)
	     (setq articles nil)))

      ;; If this is the last article to be decoded, and
      ;; we still haven't reached the end, then we delete
      ;; the partially decoded file.
      (and (or (eq state 'last) (eq state 'first-and-last))
	   (not (memq 'end process-state))
	   result-file 
	   (file-exists-p result-file)
	   (delete-file result-file))

      ;; If this was a file of the wrong sort, then 
      (when (and (or (memq 'wrong-type process-state)
		     (memq 'error process-state))
		 gnus-uu-unmark-articles-not-decoded)
	(gnus-summary-tick-article article t))

      ;; Set the new series state.
      (if (and (not has-been-begin)
	       (not sloppy)
	       (or (memq 'end process-state)
		   (memq 'middle process-state)))
	  (progn
	    (setq process-state (list 'error))
	    (gnus-message 2 "No begin part at the beginning")
	    (sleep-for 2))
	(setq state 'middle)))

    ;; When there are no result-files, then something must be wrong.
    (if result-files
	(message "")
      (cond
       ((not has-been-begin)
	(gnus-message 2 "Wrong type file"))
       ((memq 'error process-state)
	(gnus-message 2 "An error occurred during decoding"))
       ((not (or (memq 'ok process-state) 
		 (memq 'end process-state)))
	(gnus-message 2 "End of articles reached before end of file")))
      ;; Make unsuccessfully decoded articles unread.
      (when gnus-uu-unmark-articles-not-decoded
	(while article-series
	  (gnus-summary-tick-article (pop article-series) t))))

    result-files))

(defun gnus-uu-grab-view (file)
  "View FILE using the gnus-uu methods."
  (let ((action (gnus-uu-get-action file)))
    (gnus-execute-command
     (if (string-match "%" action)
	 (format action file)
       (concat action " " file))
     (eq gnus-view-pseudos 'not-confirm))))

(defun gnus-uu-grab-move (file)
  "Move FILE to somewhere."
  (when gnus-uu-default-dir
    (let ((to-file (concat (file-name-as-directory gnus-uu-default-dir)
			   (file-name-nondirectory file))))
      (rename-file file to-file)
      (unless (file-exists-p file)
	(make-symbolic-link to-file file)))))

(defun gnus-uu-part-number (article)
  (let* ((header (gnus-summary-article-header article))
	 (subject (and header (mail-header-subject header))))
    (if (and subject 
	     (string-match "[0-9]+ */[0-9]+\\|[0-9]+ * of *[0-9]+" subject))
	(match-string 0 subject)
      "")))

(defun gnus-uu-uudecode-sentinel (process event)
  (delete-process (get-process process)))

(defun gnus-uu-uustrip-article (process-buffer in-state)
  ;; Uudecodes a file asynchronously.
  (save-excursion
    (set-buffer process-buffer)
    (let ((state (list 'wrong-type))
	  process-connection-type case-fold-search buffer-read-only 
	  files start-char)
      (goto-char (point-min))

      ;; Deal with ^M at the end of the lines.
      (when gnus-uu-kill-carriage-return
	(save-excursion
	  (while (search-forward "\r" nil t)
	    (delete-backward-char 1))))

      (while (or (re-search-forward gnus-uu-begin-string nil t)
		 (re-search-forward gnus-uu-body-line nil t))
	(setq state (list 'ok))
	;; Ok, we are at the first uucoded line.
	(beginning-of-line)
	(setq start-char (point))

	(if (not (looking-at gnus-uu-begin-string))
	    (setq state (list 'middle))
	  ;; This is the beginning of an uuencoded article.
	  ;; We replace certain characters that could make things messy.
	  (setq gnus-uu-file-name 
		(let ((nnheader-file-name-translation-alist
		       '((?/ . ?,) (? . ?_) (?* . ?_) (?$ . ?_))))
		  (nnheader-translate-file-chars (match-string 1))))

	  ;; Remove any non gnus-uu-body-line right after start.
	  (forward-line 1)
	  (while (and (not (eobp))
		      (not (looking-at gnus-uu-body-line)))
	    (gnus-delete-line))

	  ;; If a process is running, we kill it.
	  (when (and gnus-uu-uudecode-process
		     (memq (process-status gnus-uu-uudecode-process) 
			   '(run stop)))
	    (delete-process gnus-uu-uudecode-process)
	    (gnus-uu-unmark-list-of-grabbed t))

	  ;; Start a new uudecoding process.
	  (setq gnus-uu-uudecode-process
		(start-process 
		 "*uudecode*" 
		 (get-buffer-create gnus-uu-output-buffer-name)
		 shell-file-name shell-command-switch
		 (format "cd %s ; uudecode" gnus-uu-work-dir)))
	  (set-process-sentinel 
	   gnus-uu-uudecode-process 'gnus-uu-uudecode-sentinel)
	  (setq state (list 'begin))
	  (push (concat gnus-uu-work-dir gnus-uu-file-name) files))
	
	;; We look for the end of the thing to be decoded.
	(if (re-search-forward gnus-uu-end-string nil t)
	    (setq state (cons 'end state))
	  (goto-char (point-max))
	  (re-search-backward gnus-uu-body-line nil t))
	 
	(forward-line 1)

	(when gnus-uu-uudecode-process
	  (when (memq (process-status gnus-uu-uudecode-process) '(run stop))
	    ;; Try to correct mishandled uucode.
	    (when gnus-uu-correct-stripped-uucode
	      (gnus-uu-check-correct-stripped-uucode start-char (point)))

	    ;; Send the text to the process.
	    (condition-case nil
		(process-send-region
		 gnus-uu-uudecode-process start-char (point))
	      (error 
	       (progn 
		 (delete-process gnus-uu-uudecode-process)
		 (gnus-message 2 "gnus-uu: Couldn't uudecode")
		 (setq state (list 'wrong-type)))))

	    (if (memq 'end state)
		(progn
		  ;; Send an EOF, just in case.
		  (condition-case ()
		      (process-send-eof gnus-uu-uudecode-process)
		    (error nil))
		  (while (memq (process-status gnus-uu-uudecode-process)
			       '(open run))
		    (accept-process-output gnus-uu-uudecode-process 1)))
	      (when (or (not gnus-uu-uudecode-process)
			(not (memq (process-status gnus-uu-uudecode-process)
				   '(run stop))))
		(setq state (list 'wrong-type)))))))

      (if (memq 'begin state)
	  (cons (if (= (length files) 1) (car files) files) state)
	state))))

;; This function is used by `gnus-uu-grab-articles' to treat
;; a shared article.
(defun gnus-uu-unshar-article (process-buffer in-state)
  (let ((state (list 'ok))
	start-char)
    (save-excursion
      (set-buffer process-buffer)
      (goto-char (point-min))
      (if (not (re-search-forward gnus-uu-shar-begin-string nil t))
	  (setq state (list 'wrong-type))
	(beginning-of-line)
	(setq start-char (point))
	(call-process-region 
	 start-char (point-max) shell-file-name nil 
	 (get-buffer-create gnus-uu-output-buffer-name) nil 
	 shell-command-switch (concat "cd " gnus-uu-work-dir " ; sh"))))
    state))

;; Returns the name of what the shar file is going to unpack.
(defun gnus-uu-find-name-in-shar ()
  (let ((oldpoint (point))
	res)
    (goto-char (point-min))
    (if (re-search-forward gnus-uu-shar-name-marker nil t)
	(setq res (buffer-substring (match-beginning 1) (match-end 1))))
    (goto-char oldpoint)
    res))

;; `gnus-uu-choose-action' chooses what action to perform given the name
;; and `gnus-uu-file-action-list'.  Returns either nil if no action is
;; found, or the name of the command to run if such a rule is found.
(defun gnus-uu-choose-action (file-name file-action-list &optional no-ignore)
  (let ((action-list (copy-sequence file-action-list))
	(case-fold-search t)
	rule action)
    (and 
     (or no-ignore 
	 (and (not 
	       (and gnus-uu-ignore-files-by-name
		    (string-match gnus-uu-ignore-files-by-name file-name)))
	      (not 
	       (and gnus-uu-ignore-files-by-type
		    (string-match gnus-uu-ignore-files-by-type 
				  (or (gnus-uu-choose-action 
				       file-name gnus-uu-ext-to-mime-list t) 
				      ""))))))
     (while (not (or (eq action-list ()) action))
       (setq rule (car action-list))
       (setq action-list (cdr action-list))
       (if (string-match (car rule) file-name)
	   (setq action (cadr rule)))))
    action))

(defun gnus-uu-treat-archive (file-path)
  ;; Unpacks an archive. Returns t if unpacking is successful.
  (let ((did-unpack t)
	action command dir)
    (setq action (gnus-uu-choose-action 
		  file-path (append gnus-uu-user-archive-rules
				    (if gnus-uu-ignore-default-archive-rules
					nil
				      gnus-uu-default-archive-rules))))

    (if (not action) (error "No unpackers for the file %s" file-path))

    (string-match "/[^/]*$" file-path)
    (setq dir (substring file-path 0 (match-beginning 0)))

    (if (member action gnus-uu-destructive-archivers)
	(copy-file file-path (concat file-path "~") t))

    (setq command (format "cd %s ; %s" dir (gnus-uu-command action file-path)))

    (save-excursion
      (set-buffer (get-buffer-create gnus-uu-output-buffer-name))
      (erase-buffer))

    (gnus-message 5 "Unpacking: %s..." (gnus-uu-command action file-path))

    (if (= 0 (call-process shell-file-name nil 
			   (get-buffer-create gnus-uu-output-buffer-name)
			   nil shell-command-switch command))
	(message "")
      (gnus-message 2 "Error during unpacking of archive")
      (setq did-unpack nil))

    (if (member action gnus-uu-destructive-archivers)
	(rename-file (concat file-path "~") file-path t))

    did-unpack))

(defun gnus-uu-dir-files (dir)
  (let ((dirs (directory-files dir t "[^/][^\\.][^\\.]?$"))
	files file)
    (while dirs
      (if (file-directory-p (setq file (car dirs)))
	  (setq files (append files (gnus-uu-dir-files file)))
	(setq files (cons file files)))
      (setq dirs (cdr dirs)))
    files))

(defun gnus-uu-unpack-files (files &optional ignore)
  ;; Go through FILES and look for files to unpack. 
  (let* ((totfiles (gnus-uu-ls-r gnus-uu-work-dir))
	 (ofiles files)
	 file did-unpack)
    (while files
      (setq file (cdr (assq 'name (car files))))
      (if (and (not (member file ignore))
	       (equal (gnus-uu-get-action (file-name-nondirectory file))
		      "gnus-uu-archive"))
	  (progn
	    (setq did-unpack (cons file did-unpack))
	    (or (gnus-uu-treat-archive file)
		(gnus-message 2 "Error during unpacking of %s" file))
	    (let* ((newfiles (gnus-uu-ls-r gnus-uu-work-dir))
		   (nfiles newfiles))
	      (while nfiles
		(or (member (car nfiles) totfiles)
		    (setq ofiles (cons (list (cons 'name (car nfiles))
					     (cons 'original file))
				       ofiles)))
		(setq nfiles (cdr nfiles)))
	      (setq totfiles newfiles))))
      (setq files (cdr files)))
    (if did-unpack 
	(gnus-uu-unpack-files ofiles (append did-unpack ignore))
      ofiles)))

(defun gnus-uu-ls-r (dir)
  (let* ((files (gnus-uu-directory-files dir t))
	 (ofiles files))
    (while files
      (if (file-directory-p (car files))
	  (progn
	    (setq ofiles (delete (car files) ofiles))
	    (setq ofiles (append ofiles (gnus-uu-ls-r (car files))))))
      (setq files (cdr files)))
    ofiles))

;; Various stuff

(defun gnus-uu-directory-files (dir &optional full)
  (let (files out file)
    (setq files (directory-files dir full))
    (while files
      (setq file (car files))
      (setq files (cdr files))
      (or (member (file-name-nondirectory file) '("." ".."))
	  (setq out (cons file out))))
    (setq out (nreverse out))
    out))

(defun gnus-uu-check-correct-stripped-uucode (start end)
  (save-excursion
    (let (found beg length)
      (if (not gnus-uu-correct-stripped-uucode)
	  ()
	(goto-char start)

	(if (re-search-forward " \\|`" end t)
	    (progn
	      (goto-char start)
	      (while (not (eobp))
		(progn
		  (if (looking-at "\n") (replace-match ""))
		  (forward-line 1))))
	    
	  (while (not (eobp))
	    (if (looking-at (concat gnus-uu-begin-string "\\|" 
				    gnus-uu-end-string))
		()
	      (if (not found)
		  (progn
		    (beginning-of-line)
		    (setq beg (point))
		    (end-of-line)
		    (setq length (- (point) beg))))
	      (setq found t)
	      (beginning-of-line)
	      (setq beg (point))
	      (end-of-line)
	      (if (not (= length (- (point) beg)))
		  (insert (make-string (- length (- (point) beg)) ? ))))
	    (forward-line 1)))))))

(defvar gnus-uu-tmp-alist nil)

(defun gnus-uu-initialize (&optional scan)
  (let (entry)
    (if (and (not scan)
	     (if (setq entry (assoc gnus-newsgroup-name gnus-uu-tmp-alist))
		 (if (file-exists-p (cdr entry))
		     (setq gnus-uu-work-dir (cdr entry))
		   (setq gnus-uu-tmp-alist (delq entry gnus-uu-tmp-alist))
		   nil)))
	t
      (setq gnus-uu-tmp-dir (file-name-as-directory 
			     (expand-file-name gnus-uu-tmp-dir)))
      (if (not (file-directory-p gnus-uu-tmp-dir))
	  (error "Temp directory %s doesn't exist" gnus-uu-tmp-dir)
	(if (not (file-writable-p gnus-uu-tmp-dir))
	    (error "Temp directory %s can't be written to" 
		   gnus-uu-tmp-dir)))

      (setq gnus-uu-work-dir 
	    (make-temp-name (concat gnus-uu-tmp-dir "gnus")))
      (if (not (file-directory-p gnus-uu-work-dir)) 
	  (gnus-make-directory gnus-uu-work-dir))
      (set-file-modes gnus-uu-work-dir 448)
      (setq gnus-uu-work-dir (file-name-as-directory gnus-uu-work-dir))
      (setq gnus-uu-tmp-alist (cons (cons gnus-newsgroup-name gnus-uu-work-dir)
				    gnus-uu-tmp-alist)))))


;; Kills the temporary uu buffers, kills any processes, etc.
(defun gnus-uu-clean-up ()
  (let (buf pst)
    (and gnus-uu-uudecode-process
	 (memq (process-status (or gnus-uu-uudecode-process "nevair"))
	       '(stop run))
	 (delete-process gnus-uu-uudecode-process))
    (and (setq buf (get-buffer gnus-uu-output-buffer-name))
	 (kill-buffer buf))))

;; Inputs an action and a file and returns a full command, putting
;; quotes round the file name and escaping any quotes in the file name.
(defun gnus-uu-command (action file)
  (let ((ofile ""))
    (while (string-match "!\\|`\\|\"\\|\\$\\|\\\\\\|&" file)
      (progn
	(setq ofile
	      (concat ofile (substring file 0 (match-beginning 0)) "\\"
		      (substring file (match-beginning 0) (match-end 0))))
	(setq file (substring file (1+ (match-beginning 0))))))
    (setq ofile (concat "\"" ofile file "\""))
    (if (string-match "%s" action)
	(format action ofile)
      (concat action " " ofile))))

(defun gnus-uu-delete-work-dir (&optional dir)
  "Delete recursively all files and directories under `gnus-uu-work-dir'."
  (if dir
      (gnus-message 7 "Deleting directory %s..." dir)
    (setq dir gnus-uu-work-dir))
  (when (and dir
	     (file-exists-p dir))
    (let ((files (directory-files dir t nil t))
	  file)
      (while (setq file (pop files))
	(unless (member (file-name-nondirectory file) '("." ".."))
	  (if (file-directory-p file)
	      (gnus-uu-delete-work-dir file)
	    (gnus-message 9 "Deleting file %s..." file)
	    (delete-file file))))
      (delete-directory dir)))
  (gnus-message 7 ""))

;; Initializing

(add-hook 'gnus-exit-group-hook 'gnus-uu-clean-up)
(add-hook 'gnus-exit-group-hook	'gnus-uu-delete-work-dir)



;;;
;;; uuencoded posting
;;;

;; Any function that is to be used as and encoding method will take two
;; parameters: PATH-NAME and FILE-NAME. (E.g. "/home/gaga/spiral.jpg"
;; and "spiral.jpg", respectively.) The function should return nil if
;; the encoding wasn't successful.
(defvar gnus-uu-post-encode-method 'gnus-uu-post-encode-uuencode
  "Function used for encoding binary files.
There are three functions supplied with gnus-uu for encoding files:
`gnus-uu-post-encode-uuencode', which does straight uuencoding;
`gnus-uu-post-encode-mime', which encodes with base64 and adds MIME 
headers; and `gnus-uu-post-encode-mime-uuencode', which encodes with 
uuencode and adds MIME headers.")

(defvar gnus-uu-post-include-before-composing nil
  "Non-nil means that gnus-uu will ask for a file to encode before you compose the article.
If this variable is t, you can either include an encoded file with
\\[gnus-uu-post-insert-binary-in-article] or have one included for you when you post the article.")

(defvar gnus-uu-post-length 990
  "Maximum length of an article.
The encoded file will be split into how many articles it takes to
post the entire file.")

(defvar gnus-uu-post-threaded nil
  "Non-nil means that gnus-uu will post the encoded file in a thread.
This may not be smart, as no other decoder I have seen are able to
follow threads when collecting uuencoded articles. (Well, I have seen
one package that does that - gnus-uu, but somehow, I don't think that 
counts...) Default is nil.")

(defvar gnus-uu-post-separate-description t
  "Non-nil means that the description will be posted in a separate article.
The first article will typically be numbered (0/x). If this variable
is nil, the description the user enters will be included at the 
beginning of the first article, which will be numbered (1/x). Default 
is t.")

(defvar gnus-uu-post-binary-separator "--binary follows this line--")
(defvar gnus-uu-post-message-id nil)
(defvar gnus-uu-post-inserted-file-name nil)
(defvar gnus-uu-winconf-post-news nil)

(defun gnus-uu-post-news ()
  "Compose an article and post an encoded file."
  (interactive)
  (setq gnus-uu-post-inserted-file-name nil)
  (setq gnus-uu-winconf-post-news (current-window-configuration))

  (gnus-summary-post-news)

  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key "\C-c\C-c" 'gnus-summary-edit-article-done)
  (local-set-key "\C-c\C-c" 'gnus-uu-post-news-inews)
  (local-set-key "\C-c\C-s" 'gnus-uu-post-news-inews)
  (local-set-key "\C-c\C-i" 'gnus-uu-post-insert-binary-in-article)
      
  (if gnus-uu-post-include-before-composing
      (save-excursion (setq gnus-uu-post-inserted-file-name 
			    (gnus-uu-post-insert-binary)))))

(defun gnus-uu-post-insert-binary-in-article ()
  "Inserts an encoded file in the buffer.
The user will be asked for a file name."
  (interactive)
  (save-excursion 
    (setq gnus-uu-post-inserted-file-name (gnus-uu-post-insert-binary))))

;; Encodes with uuencode and substitutes all spaces with backticks.
(defun gnus-uu-post-encode-uuencode (path file-name)
  (if (gnus-uu-post-encode-file "uuencode" path file-name)
      (progn
	(goto-char (point-min))
	(forward-line 1)
	(while (re-search-forward " " nil t)
	  (replace-match "`"))
	t)))

;; Encodes with uuencode and adds MIME headers.
(defun gnus-uu-post-encode-mime-uuencode (path file-name)
  (if (gnus-uu-post-encode-uuencode path file-name)
      (progn
	(gnus-uu-post-make-mime file-name "x-uue")
	t)))

;; Encodes with base64 and adds MIME headers
(defun gnus-uu-post-encode-mime (path file-name)
  (if (gnus-uu-post-encode-file "mmencode" path file-name)
      (progn
	(gnus-uu-post-make-mime file-name "base64")
	t)))

;; Adds MIME headers.
(defun gnus-uu-post-make-mime (file-name encoding)
  (goto-char (point-min))
  (insert (format "Content-Type: %s; name=\"%s\"\n" 
		  (gnus-uu-choose-action file-name gnus-uu-ext-to-mime-list) 
		  file-name))
  (insert (format "Content-Transfer-Encoding: %s\n\n" encoding))
  (save-restriction
    (set-buffer gnus-message-buffer)
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (forward-line -1)
    (narrow-to-region 1 (point))
    (or (mail-fetch-field "mime-version")
	(progn
	  (widen)
	  (insert "MIME-Version: 1.0\n")))
    (widen)))

;; Encodes a file PATH with COMMAND, leaving the result in the
;; current buffer.
(defun gnus-uu-post-encode-file (command path file-name)
  (= 0 (call-process shell-file-name nil t nil shell-command-switch 
		     (format "%s %s %s" command path file-name))))

(defun gnus-uu-post-news-inews ()
  "Posts the composed news article and encoded file.
If no file has been included, the user will be asked for a file."
  (interactive)

  (let (file-name)

    (if gnus-uu-post-inserted-file-name
	(setq file-name gnus-uu-post-inserted-file-name)
      (setq file-name (gnus-uu-post-insert-binary)))
  
    (if gnus-uu-post-threaded
	(let ((message-required-news-headers 
	       (if (memq 'Message-ID message-required-news-headers)
		   message-required-news-headers
		 (cons 'Message-ID message-required-news-headers)))
	      gnus-inews-article-hook)

	  (setq gnus-inews-article-hook (if (listp gnus-inews-article-hook)
					    gnus-inews-article-hook
					  (list gnus-inews-article-hook)))
	  (setq gnus-inews-article-hook 
		(cons
		 '(lambda ()
		    (save-excursion
		      (goto-char (point-min))
		      (if (re-search-forward "^Message-ID: \\(.*\\)$" nil t)
			  (setq gnus-uu-post-message-id 
				(buffer-substring 
				 (match-beginning 1) (match-end 1)))
			(setq gnus-uu-post-message-id nil))))
		 gnus-inews-article-hook))
	  (gnus-uu-post-encoded file-name t))
      (gnus-uu-post-encoded file-name nil)))
  (setq gnus-uu-post-inserted-file-name nil)
  (and gnus-uu-winconf-post-news
       (set-window-configuration gnus-uu-winconf-post-news)))
      
;; Asks for a file to encode, encodes it and inserts the result in
;; the current buffer. Returns the file name the user gave.
(defun gnus-uu-post-insert-binary ()
  (let ((uuencode-buffer-name "*uuencode buffer*")
	file-path uubuf file-name)

    (setq file-path (read-file-name 
		     "What file do you want to encode? "))
    (if (not (file-exists-p file-path))
	(error "%s: No such file" file-path))

    (goto-char (point-max))
    (insert (format "\n%s\n" gnus-uu-post-binary-separator))
    
    (if (string-match "^~/" file-path)
	(setq file-path (concat "$HOME" (substring file-path 1))))
    (if (string-match "/[^/]*$" file-path)
	(setq file-name (substring file-path (1+ (match-beginning 0))))
      (setq file-name file-path))

    (unwind-protect
	(if (save-excursion
	      (set-buffer (setq uubuf 
				(get-buffer-create uuencode-buffer-name)))
	      (erase-buffer)
	      (funcall gnus-uu-post-encode-method file-path file-name))
	    (insert-buffer-substring uubuf)
	  (error "Encoding unsuccessful"))
      (kill-buffer uubuf))
    file-name))

;; Posts the article and all of the encoded file.
(defun gnus-uu-post-encoded (file-name &optional threaded)
  (let ((send-buffer-name "*uuencode send buffer*")
	(encoded-buffer-name "*encoded buffer*")
	(top-string "[ cut here %s (%s %d/%d) %s gnus-uu ]")
	(separator (concat mail-header-separator "\n\n"))
	uubuf length parts header i end beg
	beg-line minlen buf post-buf whole-len beg-binary end-binary)

    (setq post-buf (current-buffer))

    (goto-char (point-min))
    (if (not (re-search-forward 
	      (if gnus-uu-post-separate-description 
		  (concat "^" (regexp-quote gnus-uu-post-binary-separator)
			  "$")
		(concat "^" (regexp-quote mail-header-separator) "$")) nil t))
	(error "Internal error: No binary/header separator"))
    (beginning-of-line)
    (forward-line 1)
    (setq beg-binary (point))
    (setq end-binary (point-max))

    (save-excursion 
      (set-buffer (setq uubuf (get-buffer-create encoded-buffer-name)))
      (erase-buffer)
      (insert-buffer-substring post-buf beg-binary end-binary)
      (goto-char (point-min))
      (setq length (count-lines 1 (point-max)))
      (setq parts (/ length gnus-uu-post-length))
      (if (not (< (% length gnus-uu-post-length) 4))
	  (setq parts (1+ parts))))

    (if gnus-uu-post-separate-description
	(forward-line -1))
    (kill-region (point) (point-max))

    (goto-char (point-min))
    (re-search-forward 
     (concat "^" (regexp-quote mail-header-separator) "$") nil t)
    (beginning-of-line)
    (setq header (buffer-substring 1 (point)))

    (goto-char (point-min))
    (if (not gnus-uu-post-separate-description)
	()
      (if (and (not threaded) (re-search-forward "^Subject: " nil t))
	  (progn
	    (end-of-line)
	    (insert (format " (0/%d)" parts))))
      (message-send))

    (save-excursion
      (setq i 1)
      (setq beg 1)
      (while (not (> i parts))
	(set-buffer (get-buffer-create send-buffer-name))
	(erase-buffer)
	(insert header)
	(if (and threaded gnus-uu-post-message-id)
	    (insert (format "References: %s\n" gnus-uu-post-message-id)))
	(insert separator)
	(setq whole-len
	      (- 62 (length (format top-string "" file-name i parts ""))))
	(if (> 1 (setq minlen (/ whole-len 2)))
	    (setq minlen 1))
	(setq 
	 beg-line 
	 (format top-string
		 (make-string minlen ?-) 
		 file-name i parts
		 (make-string 
		  (if (= 0 (% whole-len 2)) (1- minlen) minlen) ?-)))

	(goto-char (point-min))
	(if (not (re-search-forward "^Subject: " nil t))
	    ()
	  (if (not threaded)
	      (progn
		(end-of-line)
		(insert (format " (%d/%d)" i parts)))
	    (if (or (and (= i 2) gnus-uu-post-separate-description)
		    (and (= i 1) (not gnus-uu-post-separate-description)))
		(replace-match "Subject: Re: "))))
		  
	(goto-char (point-max))
	(save-excursion
	  (set-buffer uubuf)
	  (goto-char beg)
	  (if (= i parts)
	      (goto-char (point-max))
	    (forward-line gnus-uu-post-length))
	  (if (and (= (1+ i) parts) (< (count-lines (point) (point-max)) 4))
	      (forward-line -4))
	  (setq end (point)))
	(insert-buffer-substring uubuf beg end)
	(insert beg-line)
	(insert "\n")
	(setq beg end)
	(setq i (1+ i))
	(goto-char (point-min))
	(re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "$") nil t)
	(beginning-of-line)
	(forward-line 2)
	(if (re-search-forward 
	     (concat "^" (regexp-quote gnus-uu-post-binary-separator) "$")
	     nil t)
	    (progn 
	      (replace-match "")
	      (forward-line 1)))
	(insert beg-line)
	(insert "\n")
	(let (message-sent-message-via)
	  (message-send))))

    (and (setq buf (get-buffer send-buffer-name))
	 (kill-buffer buf))
    (and (setq buf (get-buffer encoded-buffer-name))
	 (kill-buffer buf))

    (if (not gnus-uu-post-separate-description)
	(progn
	  (set-buffer-modified-p nil)
	  (and (fboundp 'bury-buffer) (bury-buffer))))))

(provide 'gnus-uu)

;; gnus-uu.el ends here
