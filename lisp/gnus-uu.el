;;; gnus-uu.el --- extract (uu)encoded files in Gnus

;; Copyright (C) 1985,86,87,93,94,95 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Created: 2 Oct 1993
;; Version: v3.0
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

(defvar gnus-uu-view-and-save nil 
  "*Non-nil means that the user will always be asked to save a file after viewing it.
If the variable is nil, the user will only be asked to save if the
viewing is unsuccessful. Default is nil.")

(defvar gnus-uu-ignore-default-view-rules nil
  "*Non-nil means that gnus-uu will ignore the default viewing rules.
Only the user viewing rules will be consulted. Default is nil.")

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

(defvar gnus-uu-generated-file-list nil)
(defvar gnus-uu-work-dir nil)

(defconst gnus-uu-output-buffer-name " *Gnus UU Output*")

(defvar gnus-uu-default-dir default-directory)

;; Keymaps

(defvar gnus-uu-extract-map nil)
(defvar gnus-uu-extract-view-map nil)
(defvar gnus-uu-mark-map nil)

(define-prefix-command 'gnus-uu-mark-map)
(define-key gnus-summary-mark-map "P" 'gnus-uu-mark-map)
(define-key gnus-uu-mark-map "p" 'gnus-summary-mark-as-processable)
(define-key gnus-uu-mark-map "u" 'gnus-summary-unmark-as-processable)
(define-key gnus-uu-mark-map "U" 'gnus-summary-unmark-all-processable)
(define-key gnus-uu-mark-map "s" 'gnus-uu-mark-series)
(define-key gnus-uu-mark-map "r" 'gnus-uu-mark-region)
(define-key gnus-uu-mark-map "R" 'gnus-uu-mark-by-regexp)
(define-key gnus-uu-mark-map "t" 'gnus-uu-mark-thread)
(define-key gnus-uu-mark-map "a" 'gnus-uu-mark-all)
(define-key gnus-uu-mark-map "S" 'gnus-uu-mark-sparse)

(define-prefix-command 'gnus-uu-extract-map)
(define-key gnus-summary-mode-map "X" 'gnus-uu-extract-map)
;;(define-key gnus-uu-extract-map "x" 'gnus-uu-extract-any)
;;(define-key gnus-uu-extract-map "m" 'gnus-uu-extract-mime)
(define-key gnus-uu-extract-map "u" 'gnus-uu-decode-uu)
(define-key gnus-uu-extract-map "U" 'gnus-uu-decode-uu-and-save)
(define-key gnus-uu-extract-map "s" 'gnus-uu-decode-unshar)
(define-key gnus-uu-extract-map "S" 'gnus-uu-decode-unshar-and-save)
(define-key gnus-uu-extract-map "o" 'gnus-uu-decode-save)
(define-key gnus-uu-extract-map "O" 'gnus-uu-decode-save)
(define-key gnus-uu-extract-map "b" 'gnus-uu-decode-binhex)
(define-key gnus-uu-extract-map "B" 'gnus-uu-decode-binhex)
(define-key gnus-uu-extract-map "p" 'gnus-uu-decode-postscript)
(define-key gnus-uu-extract-map "P" 'gnus-uu-decode-postscript-and-save)

(define-prefix-command 'gnus-uu-extract-view-map)
(define-key gnus-uu-extract-map "v" 'gnus-uu-extract-view-map)
(define-key gnus-uu-extract-view-map "u" 'gnus-uu-decode-uu-view)
(define-key gnus-uu-extract-view-map "U" 'gnus-uu-decode-uu-and-save-view)
(define-key gnus-uu-extract-view-map "s" 'gnus-uu-decode-unshar-view)
(define-key gnus-uu-extract-view-map "S" 'gnus-uu-decode-unshar-and-save-view)
(define-key gnus-uu-extract-view-map "o" 'gnus-uu-decode-save-view)
(define-key gnus-uu-extract-view-map "O" 'gnus-uu-decode-save-view)
(define-key gnus-uu-extract-view-map "b" 'gnus-uu-decode-binhex-view)
(define-key gnus-uu-extract-view-map "B" 'gnus-uu-decode-binhex-view)
(define-key gnus-uu-extract-view-map "p" 'gnus-uu-decode-postscript-view)
(define-key gnus-uu-extract-view-map "P" 'gnus-uu-decode-postscript-and-save-view)



;; Commands.

(defun gnus-uu-decode-uu (n)
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
  (gnus-uu-decode-with-method 'gnus-uu-uustrip-article n dir))

(defun gnus-uu-decode-unshar (n)
  "Unshars the current article."
  (interactive "P")
  (gnus-uu-decode-with-method 'gnus-uu-unshar-article n nil nil 'scan))

(defun gnus-uu-decode-unshar-and-save (n dir)
  "Unshars and saves the current article."
  (interactive
   (list current-prefix-arg
	 (file-name-as-directory
	  (read-file-name "Unshar and save in dir: "
			  gnus-uu-default-dir
			  gnus-uu-default-dir t))))
  (gnus-uu-decode-with-method 'gnus-uu-unshar-article n dir nil 'scan))

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
  (gnus-uu-decode-with-method 'gnus-uu-save-article n nil t)
  (setq gnus-uu-generated-file-list 
	(delete file gnus-uu-generated-file-list)))

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

(defun gnus-uu-decode-uu-view (n)
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

(defun gnus-uu-decode-unshar-view (n)
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

(defun gnus-uu-digest-mail-forward (n &optional post)
  "Digests and forwards all articles in this series."
  (interactive "P")
  (let ((gnus-uu-save-in-digest t)
	(file (make-temp-name (concat gnus-uu-tmp-dir "forward")))
	buf)
    (gnus-uu-decode-save n file)
    (gnus-uu-add-file file)
    (setq buf (switch-to-buffer (get-buffer-create " *gnus-uu-forward*")))
    (gnus-add-current-to-buffer-list)
    (erase-buffer)
    (delete-other-windows)
    (insert-file file)
    (goto-char (point-min))
    (and (re-search-forward "^Subject: ")
	 (progn
	   (delete-region (point) (gnus-point-at-eol))
	   (insert "Digested Articles")))
    (goto-char (point-min))
    (and (re-search-forward "^From: ")
	 (progn
	   (delete-region (point) (gnus-point-at-eol))
	   (insert "Various")))
    (if post
	(gnus-forward-using-post)
      (funcall gnus-mail-forward-method))
    (delete-file file)
    (kill-buffer buf)))

(defun gnus-uu-digest-post-forward (n)
  "Digest and forward to a newsgroup."
  (interactive "P")
  (gnus-uu-digest-mail-forward n t))

;; Process marking.

(defun gnus-uu-mark-by-regexp (regexp)
  "Ask for a regular expression and set the process mark on all articles that match."
  (interactive (list (read-from-minibuffer "Mark (regexp): ")))
  (gnus-set-global-variables)
  (let ((articles (gnus-uu-find-articles-matching regexp)))
    (while articles
      (gnus-summary-set-process-mark (car articles))
      (setq articles (cdr articles)))
    (message ""))
  (gnus-summary-position-cursor))

(defun gnus-uu-mark-series ()
  "Mark the current series with the process mark."
  (interactive)
  (gnus-set-global-variables)
  (let ((articles (gnus-uu-find-articles-matching)))
    (while articles
      (gnus-summary-set-process-mark (car articles))
      (setq articles (cdr articles)))
    (message ""))
  (gnus-summary-position-cursor))

(defun gnus-uu-mark-region (beg end)
  "Marks all articles between point and mark."
  (interactive "r")
  (gnus-set-global-variables)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (gnus-summary-set-process-mark (gnus-summary-article-number))
      (forward-line 1)))
  (gnus-summary-position-cursor))
      
(defun gnus-uu-mark-thread ()
  "Marks all articles downwards in this thread."
  (interactive)
  (gnus-set-global-variables)
  (let ((level (gnus-summary-thread-level)))
    (while (and (gnus-summary-set-process-mark (gnus-summary-article-number))
		(zerop (gnus-summary-next-subject 1))
		(> (gnus-summary-thread-level) level))))
  (gnus-summary-position-cursor))

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
	(and (setq headers (gnus-get-header-by-number (car marked)))
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
    (gnus-summary-position-cursor)))

(defun gnus-uu-mark-all ()
  "Mark all articles in \"series\" order."
  (interactive)
  (gnus-set-global-variables)
  (setq gnus-newsgroup-processable nil)
  (save-excursion
    (goto-char (point-min))
    (let (number)
      (while (and (not (eobp)) 
		  (setq number (gnus-summary-article-number)))
	(if (not (memq number gnus-newsgroup-processable))
	    (save-excursion (gnus-uu-mark-series)))
	(forward-line 1))))
  (gnus-summary-position-cursor))

;; All PostScript functions written by Erik Selberg <speed@cs.washington.edu>. 

(defun gnus-uu-decode-postscript (n)
  "Gets postscript of the current article."
  (interactive "P")
  (gnus-uu-decode-with-method 'gnus-uu-decode-postscript-article n))

(defun gnus-uu-decode-postscript-view (n)
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
  (gnus-uu-decode-with-method 'gnus-uu-decode-postscript-article n dir))


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

(defun gnus-uu-decode-with-method (method n &optional save not-insert scan)
  (gnus-uu-initialize scan)
  (if save (setq gnus-uu-default-dir save))
  (let ((articles (gnus-uu-get-list-of-articles n))
	files)
    (setq files (gnus-uu-grab-articles articles method t))
    (let ((gnus-current-article (car articles)))
      (and scan (setq files (gnus-uu-scan-directory gnus-uu-work-dir))))
    (and save (gnus-uu-save-files files save))
    (setq files (gnus-uu-unpack-files files))
    (gnus-uu-add-file (mapcar (lambda (file) (cdr (assq 'name file))) files))
    (setq files (nreverse (gnus-uu-get-actions files)))
    (or not-insert (gnus-summary-insert-pseudos files save))))

;; Return a list of files in dir.
(defun gnus-uu-scan-directory (dir)
  (let ((files (directory-files dir t))
	dirs out)
    (while files
      (cond ((string-match "/\\.\\.?$" (car files)))
	    ((file-directory-p (car files))
	     (setq dirs (cons (car files) dirs)))
	    (t (setq out (cons (list (cons 'name (car files))
				     (cons 'article gnus-current-article))
			       out))))
      (setq files (cdr files)))
    (apply 'nconc out (mapcar (lambda (d) (gnus-uu-scan-directory d))
			      dirs))))

(defun gnus-uu-save-files (files dir)
  (let ((len (length files))
	to-file file)
    (while files
      (and 
       (setq file (cdr (assq 'name (car files))))
       (file-exists-p file)
       (progn
	 (setq to-file (if (file-directory-p dir)
			   (concat dir (file-name-nondirectory file))
			 dir))
	 (and (or (not (file-exists-p to-file))
		  (gnus-y-or-n-p (format "%s exists; overwrite? "
					 to-file)))
	      (copy-file file to-file t t))))
      (setq files (cdr files)))
    (message "Saved %d file%s" len (if (> len 1) "s" ""))))

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
    (let ((name (file-name-nondirectory gnus-uu-saved-article-name))
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
	      (set-text-properties (point-min) (point-max) nil)
	      ;; These two are necessary for XEmacs 19.12 fascism.
	      (put-text-property (point-min) (point-max) 'invisible nil)
	      (put-text-property (point-min) (point-max) 'intangible nil))
	    (goto-char (point-min))
	    (re-search-forward "\n\n")
	    (setq body (buffer-substring (1- (point)) (point-max)))
	    (narrow-to-region 1 (point))
	    (if (not (setq headers gnus-uu-digest-headers))
		(setq sorthead (buffer-substring (point-min) (point-max)))
	      (while headers
		(setq headline (car headers))
		(setq headers (cdr headers))
		(goto-char (point-min))
		(if (re-search-forward headline nil t)
		    (setq sorthead 
			  (concat sorthead
				  (buffer-substring 
				   (match-beginning 0)
				   (or (and (re-search-forward "^[^ \t]" nil t)
					    (1- (point)))
				       (progn (forward-line 1) (point)))))))))
	    (widen)))
	(insert sorthead)(goto-char (point-max))
	(insert body)(goto-char (point-max))
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
	  (setq file-name (concat gnus-uu-work-dir (cdr gnus-article-current) ".ps"))
	  (write-region (point-min) (point-max) file-name)
	  (setq state (list file-name'begin 'end))

	  ))
      )
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
				  (cons 'execute (if (string-match "%" action)
						     (format action name)
						   (concat action " " name))))
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
		     (gnus-uu-reginize-string (gnus-summary-subject-string))))
	list-of-subjects)
    (save-excursion
      (if (not subject)
	  ()
	;; Collect all subjects matching subject.
	(let ((case-fold-search t)
	      subj mark)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (and (setq subj (gnus-summary-subject-string))
		 (string-match subject subj)
		 (or (not only-unread)
		     (= (setq mark (gnus-summary-article-mark)) 
			gnus-unread-mark)
		     (= mark gnus-ticked-mark)
		     (= mark gnus-dormant-mark))
		 (setq list-of-subjects 
		       (cons (cons subj (gnus-summary-article-number))
			     list-of-subjects)))
	    (forward-line 1)))

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
	(insert (car (car string-list)))
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
(defun gnus-uu-grab-articles 
  (articles process-function &optional sloppy limit no-errors)
  (let ((state 'first) 
	has-been-begin article result-file result-files process-state 
	article-buffer)
 
    (if (not (gnus-server-opened gnus-current-select-method))
	(progn
	  (gnus-start-news-server)
	  (gnus-request-group gnus-newsgroup-name)))

    (setq gnus-uu-has-been-grabbed nil)

    (while (and articles 
		(not (memq 'error process-state))
		(or sloppy
		    (not (memq 'end process-state))))

      (setq article (car articles))
      (setq articles (cdr articles))
      (setq gnus-uu-has-been-grabbed (cons article gnus-uu-has-been-grabbed))

      (if (eq articles ()) 
	  (if (eq state 'first)
	      (setq state 'first-and-last)
	    (setq state 'last)))

      (message "Getting article %d, %s" article (gnus-uu-part-number article))

      (if (not (= (or gnus-current-article 0) article))
	  (let ((nntp-async-number nil))
	    (gnus-request-article article gnus-newsgroup-name
				  nntp-server-buffer)
	    (setq gnus-last-article gnus-current-article)
	    (setq gnus-current-article article)
	    (setq gnus-article-current (cons gnus-newsgroup-name article))
	    (if (stringp nntp-server-buffer)
		(setq article-buffer nntp-server-buffer)
	      (setq article-buffer (buffer-name nntp-server-buffer))))
	(gnus-summary-stop-page-breaking)
	(setq article-buffer gnus-article-buffer))

      (buffer-disable-undo article-buffer)
      ;; Mark article as read.
      (and (memq article gnus-newsgroup-processable)
	   (gnus-summary-remove-process-mark article))
      (run-hooks 'gnus-mark-article-hook)

      (setq process-state (funcall process-function article-buffer state))

      (if (or (memq 'begin process-state)
	      (and (or (eq state 'first) (eq state 'first-and-last))
		   (memq 'ok process-state)))
	  (progn
	    (if has-been-begin
		(if (and result-file (file-exists-p result-file)) 
		    (delete-file result-file)))
	    (if (memq 'begin process-state)
		(setq result-file (car process-state)))
	    (setq has-been-begin t)))

      (if (memq 'end process-state)
	  (progn
	    (setq gnus-uu-has-been-grabbed nil)
	    (setq result-files (cons (list (cons 'name result-file)
					   (cons 'article article))
				     result-files))
	    (setq has-been-begin nil)
	    (and limit (= (length result-files) limit)
		 (setq articles nil))))

      (if (and (or (eq state 'last) (eq state 'first-and-last))
	       (not (memq 'end process-state)))
	  (if (and result-file (file-exists-p result-file))
	      (delete-file result-file)))

      (if (not (memq 'wrong-type process-state))
	  ()
	(if gnus-uu-unmark-articles-not-decoded
	    (gnus-summary-tick-article article t)))

      (if (and (not has-been-begin)
	       (not sloppy)
	       (or (memq 'end process-state)
		   (memq 'middle process-state)))
	  (progn
	    (setq process-state (list 'error))
	    (message "No begin part at the beginning")
	    (sleep-for 2))
	(setq state 'middle)))

    ;; Make sure the last article is put in the article buffer & fix
    ;; windows etc.

    (if (not (string= article-buffer gnus-article-buffer))
	(save-excursion
	  (set-buffer (get-buffer-create gnus-article-buffer))
	  (let ((buffer-read-only nil))
	    (widen)
	    (erase-buffer)
	    (insert-buffer-substring article-buffer)
	    (gnus-set-mode-line 'article)
	    (goto-char (point-min)))))

    (gnus-set-mode-line 'summary)

    (if result-files
	()
      (if (not has-been-begin)
	  (if (not no-errors) (message "Wrong type file"))
	(if (memq 'error process-state)
	    (setq result-files nil)
	  (if (not (or (memq 'ok process-state) 
		       (memq 'end process-state)))
	      (progn
		(if (not no-errors)
		    (message "End of articles reached before end of file"))
		(setq result-files nil))
	    (gnus-uu-unmark-list-of-grabbed)))))
    result-files))

(defun gnus-uu-part-number (article)
  (let ((subject (mail-header-subject (gnus-get-header-by-number article))))
    (if (string-match "[0-9]+ */[0-9]+\\|[0-9]+ * of *[0-9]+"
		      subject)
	(substring subject (match-beginning 0) (match-end 0))
      "")))

(defun gnus-uu-uudecode-sentinel (process event)
  (delete-process (get-process process)))

(defun gnus-uu-uustrip-article (process-buffer in-state)
  ;; Uudecodes a file asynchronously.
  (let ((state (list 'ok))
	(process-connection-type nil)
	start-char pst name-beg name-end)
    (save-excursion
      (set-buffer process-buffer)
      (let ((case-fold-search nil)
	    (buffer-read-only nil))

	(goto-char (point-min))

	(if gnus-uu-kill-carriage-return
	    (progn
	      (while (search-forward "\r" nil t)
		(delete-backward-char 1))
	      (goto-char (point-min))))

	(if (not (re-search-forward gnus-uu-begin-string nil t))
	    (if (not (re-search-forward gnus-uu-body-line nil t))
		(setq state (list 'wrong-type))))
     
	(if (memq 'wrong-type state)
	    ()
	  (beginning-of-line)
	  (setq start-char (point))

	  (if (looking-at gnus-uu-begin-string)
	      (progn 
		(setq name-end (match-end 1)
		      name-beg (match-beginning 1))
		;; Remove any non gnus-uu-body-line right after start.
		(forward-line 1)
		(or (looking-at gnus-uu-body-line)
		    (gnus-delete-line))
 
					; Replace any slashes and spaces in file names before decoding
		(goto-char name-beg)
		(while (re-search-forward "/" name-end t)
		  (replace-match ","))
		(goto-char name-beg)
		(while (re-search-forward " " name-end t)
		  (replace-match "_"))
		(goto-char name-beg)
		(if (re-search-forward "_*$" name-end t)
		    (replace-match ""))

		(setq gnus-uu-file-name (buffer-substring name-beg name-end))
		(and gnus-uu-uudecode-process
		     (setq pst (process-status 
				(or gnus-uu-uudecode-process "nevair")))
		     (if (or (eq pst 'stop) (eq pst 'run))
			 (progn
			   (delete-process gnus-uu-uudecode-process)
			   (gnus-uu-unmark-list-of-grabbed t))))
		(if (get-process "*uudecode*")
		    (delete-process "*uudecode*"))
		(setq gnus-uu-uudecode-process
		      (start-process 
		       "*uudecode*" 
		       (get-buffer-create gnus-uu-output-buffer-name)
		       "sh" "-c" 
		       (format "cd %s ; uudecode" gnus-uu-work-dir)))
		(set-process-sentinel 
		 gnus-uu-uudecode-process 'gnus-uu-uudecode-sentinel)
		(setq state (list 'begin))
		(gnus-uu-add-file (concat gnus-uu-work-dir gnus-uu-file-name)))
	    (setq state (list 'middle)))
	
	  (goto-char (point-max))

	  (re-search-backward 
	   (concat gnus-uu-body-line "\\|" gnus-uu-end-string) nil t)
	  (beginning-of-line)

	  (if (looking-at gnus-uu-end-string)
	      (setq state (cons 'end state)))
	  (forward-line 1)

	  (and gnus-uu-uudecode-process
	       (setq pst (process-status 
			  (or gnus-uu-uudecode-process "nevair")))
	       (if (or (eq pst 'run) (eq pst 'stop))
		   (progn
		     (if gnus-uu-correct-stripped-uucode
			 (progn
			   (gnus-uu-check-correct-stripped-uucode 
			    start-char (point))
			   (goto-char (point-max))
			   (re-search-backward 
			    (concat gnus-uu-body-line "\\|" 
				    gnus-uu-end-string) 
			    nil t)
			   (forward-line 1)))

		     (condition-case nil
			 (process-send-region gnus-uu-uudecode-process 
					      start-char (point))
		       (error 
			(progn 
			  (delete-process gnus-uu-uudecode-process)
			  (message "gnus-uu: Couldn't uudecode")
					;			  (sleep-for 2)
			  (setq state (list 'wrong-type)))))

		     (if (memq 'end state)
			 (accept-process-output gnus-uu-uudecode-process)))
		 (setq state (list 'wrong-type))))
	  (if (not gnus-uu-uudecode-process)
	      (setq state (list 'wrong-type)))))

      (if (memq 'begin state)
	  (cons (concat gnus-uu-work-dir gnus-uu-file-name) state)
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
	 start-char (point-max) "sh" nil 
	 (get-buffer-create gnus-uu-output-buffer-name) nil 
	 "-c" (concat "cd " gnus-uu-work-dir " ; sh"))))
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
	   (setq action (car (cdr rule))))))
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

    (message "Unpacking: %s..." (gnus-uu-command action file-path))

    (if (= 0 (call-process "sh" nil 
			   (get-buffer-create gnus-uu-output-buffer-name)
			   nil "-c" command))
	(message "")
      (message "Error during unpacking of archive")
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
	 file did-unpack file-entry)
    (gnus-uu-add-file totfiles) 
    (while files
      (setq file (cdr (setq file-entry (assq 'name (car files)))))
      (if (and (not (member file ignore))
	       (equal (gnus-uu-get-action (file-name-nondirectory file))
		      "gnus-uu-archive"))
	  (progn
	    (setq did-unpack (cons file did-unpack))
	    (or (gnus-uu-treat-archive file)
		(message "Error during unpacking of %s" file))
	    (let* ((newfiles (gnus-uu-ls-r gnus-uu-work-dir))
		   (nfiles newfiles))
	      (gnus-uu-add-file newfiles)
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
      (or (string-match "/\\.\\.?$" file)
	  (setq out (cons file out))))
    (setq out (nreverse out))
    out))

(defun gnus-uu-check-correct-stripped-uucode (start end)
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
	  (forward-line 1))))))

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
      (gnus-uu-add-file gnus-uu-work-dir)
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
	 (setq pst (process-status (or gnus-uu-uudecode-process "nevair")))
	 (if (or (eq pst 'stop) (eq pst 'run))
	     (delete-process gnus-uu-uudecode-process)))
    (and (setq buf (get-buffer gnus-uu-output-buffer-name))
	 (kill-buffer buf))))

;; `gnus-uu-check-for-generated-files' deletes any generated files that
;; hasn't been deleted, if, for instance, the user terminated decoding
;; with `C-g'.
(defun gnus-uu-check-for-generated-files ()
  (let (file dirs)
    (while gnus-uu-generated-file-list
      (setq file (car gnus-uu-generated-file-list))
      (setq gnus-uu-generated-file-list (cdr gnus-uu-generated-file-list))
      (if (not (string-match "/\\.[\\.]?$" file))
	  (progn
	    (if (file-directory-p file)
		(setq dirs (cons file dirs))
	      (if (file-exists-p file)
		  (delete-file file))))))
    (setq dirs (nreverse dirs))
    (while dirs
      (setq file (car dirs))
      (setq dirs (cdr dirs))
      (if (file-directory-p file)
	  (if (string-match "/$" file)
	      (delete-directory (substring file 0 (match-beginning 0)))
	    (delete-directory file))))))

;; Add a file (or a list of files) to be checked (and deleted if it/they
;; still exists upon exiting the newsgroup).
(defun gnus-uu-add-file (file)
  (if (stringp file)
      (setq gnus-uu-generated-file-list 
	    (cons file gnus-uu-generated-file-list))
    (setq gnus-uu-generated-file-list 
	  (append file gnus-uu-generated-file-list))))

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


;; Initializing

(add-hook 'gnus-exit-group-hook 'gnus-uu-clean-up)
(add-hook 'gnus-exit-group-hook	'gnus-uu-check-for-generated-files)



;;;
;;; uuencoded posting
;;;

(require 'sendmail)
(require 'rnews)

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
  (if (not (eq (current-buffer) (get-buffer gnus-post-news-buffer)))
      (error "Not in post-news buffer"))
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
    (set-buffer gnus-post-news-buffer)
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
  (= 0 (call-process "sh" nil t nil "-c" 
		     (format "%s %s %s" command path file-name))))

(defun gnus-uu-post-news-inews ()
  "Posts the composed news article and encoded file.
If no file has been included, the user will be asked for a file."
  (interactive)
  (if (not (eq (current-buffer) (get-buffer gnus-post-news-buffer)))
      (error "Not in post news buffer"))

  (let (file-name)

    (if gnus-uu-post-inserted-file-name
	(setq file-name gnus-uu-post-inserted-file-name)
      (setq file-name (gnus-uu-post-insert-binary)))
  
    (if gnus-uu-post-threaded
	(let ((gnus-required-headers 
	       (if (memq 'Message-ID gnus-required-headers)
		   gnus-required-headers
		 (cons 'Message-ID gnus-required-headers)))
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
	    (insert-buffer uubuf)
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
      (gnus-inews-news))

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
	(gnus-inews-news)))

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
