;;; gnus-cus.el --- User friendly customization of Gnus

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Keywords: help, news
;; Version: 0.1

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

(require 'custom)
(require 'gnus-ems)

;; The following is just helper functions and data, not ment to be set
;; by the user.
(defun gnus-make-face (color)
  ;; Create entry for face with COLOR.
  (custom-face-lookup color nil nil 'as-is 'as-is 'as-is))

(defvar gnus-face-light-name-list
  '("light blue" "light cyan" "light yellow" "light pink"
    "pale green" "beige" "orange" "magenta" "violet" "medium purple"
    "turquoise"))

(defvar gnus-face-dark-name-list
  '("RoyalBlue" "firebrick"
    "dark green" "OrangeRed" "dark khaki" "dark violet"
    "SteelBlue4"))
; CornflowerBlue SeaGreen OrangeRed SteelBlue4 DeepPink3
; DarkOlviveGreen4 

(custom-declare '()
  '((tag . "GNUS")
    (doc . "\
The coffee-brewing, all singing, all dancing, kitchen sink newsreader.")
    (type . group)
    (data ((tag . "Visual")
	   (doc . "\
GNUS can be made colorful and fun or grey and dull as you wish.")
	   (type . group)
	   (data ((tag . "Visual")
		  (doc . "Enable visual features.
If `visual' is disabled, there will be no menus and few faces.  Most of
the visual customization options below will be ignored.  GNUS will use
less space and be faster as a result.")
		  (default . t)
		  (name . gnus-visual)
		  (type . toggle))
		 ((tag . "WWW Browser")
		  (doc . "\
WWW Browser to call when clicking on an URL button in the article buffer.

You can choose between one of the predefined browsers, or `Other'.")
		  (name . gnus-button-url)
		  (calculate . (cond ((boundp 'browse-url-browser-function)
				      browse-url-browser-function)
				     ((fboundp 'w3-fetch) 
				      'w3-fetch)
				     ((eq window-system 'x) 
				      'gnus-netscape-open-url)))
		  (type . choice)
		  (data ((tag . "W3")
			 (type . const)
			 (default . w3-fetch))
			((tag . "Netscape")
			 (type . const)
			 (default . gnus-netscape-open-url))
			((prompt . "Other")
			 (doc . "\
You must specify the name of a Lisp function here.  The lisp function
should open a WWW browser when called with an URL (a string).
")
			 (default . __uninitialized__)
			 (type . symbol))))
		 ((tag . "Mouse Face")
		  (doc . "\
Face used for group or summary buffer mouse highlighting.
The line beneath the mouse pointer will be highlighted with this
face.")
		  (name . gnus-mouse-face)
		  (calculate . (if (boundp 'gnus-mouse-face)
				    gnus-mouse-face
				  'highlight))
		  (type . face))
		 ((tag . "Article Display")
		  (doc . "Controls how the article buffer will look.

The list below contains various filters you can use to change the look
of the article.  If you leave the list empty, the article will appear
exactly as it is stored on the disk.  The list entries will hide or
highlight various parts of the article, making it easier to find the
information you want.")
		  (name . gnus-article-display-hook)
		  (type . list)
		  (default . (gnus-article-hide-headers-if-wanted
			      gnus-article-treat-overstrike
			      gnus-article-maybe-highlight))
		  (data ((type . repeat)
			 (header . nil)
			 (data (tag . "Filter")
			       (type . choice)
			       (data ((tag . "Treat Overstrike")
				      (doc . "\
Convert use of overstrike into bold and underline.

Two identical letters separated by a backspace are displayed as a
single bold letter, while a letter followed by a backspace and an
underscore will be displayed as a single underlined letter.  This
technique was developed for old line printers (think about it), and is
still in use on some newsgroups, in particular the ClariNet
hierarchy.
")
				      (type . const)
				      (default . 
					gnus-article-treat-overstrike))
				     ((tag . "Word Wrap")
				      (doc . "\
Format too long lines.
")
				      (type . const)
				      (default . gnus-article-word-wrap))
				     ((tag . "Remove CR")
				      (doc . "\
Remove carriage returns from an article.
")
				      (type . const)
				      (default . gnus-article-remove-cr))
				     ((tag . "Display X-Face")
				      (doc . "\
Look for an X-Face header and display it if present.

See also `X Face Command' for a definition of the external command
used for decoding and displaying the face.
")
				      (type . const)
				      (default . gnus-article-display-x-face))
				     ((tag . "Unquote Printable")
				      (doc . "\
Transform MIME quoted printable into 8-bit characters.

Quoted printable is often seen by strings like `=EF' where you would
expect a non-English letter.
")
				      (type . const)
				      (default .
					gnus-article-de-quoted-unreadable))
				     ((tag . "Universal Time")
				      (doc . "\
Convert date header to universal time.
")
				      (type . const)
				      (default . gnus-article-date-ut))
				     ((tag . "Local Time")
				      (doc . "\
Convert date header to local timezone.
")
				      (type . const)
				      (default . gnus-article-date-local))
				     ((tag . "Lapsed Time")
				      (doc . "\
Replace date header with a header showing the articles age.
")
				      (type . const)
				      (default . gnus-article-date-lapsed))
				     ((tag . "Highlight")
				      (doc . "\
Highlight headers, citations, signature, and buttons.
")
				      (type . const)
				      (default . gnus-article-highlight))
				     ((tag . "Maybe Highlight")
				      (doc . "\
Highlight headers, signature, and buttons if `Visual' is turned on.
")
				      (type . const)
				      (default . 
					gnus-article-maybe-highlight))
				     ((tag . "Highlight Some")
				      (doc . "\
Highlight headers, signature, and buttons.
")
				      (type . const)
				      (default . gnus-article-highlight-some))
				     ((tag . "Highlight Headers")
				      (doc . "\
Highlight headers as specified by `Article Header Highlighting'.
")
				      (type . const)
				      (default .
					gnus-article-highlight-headers))
				     ((tag . "Highlight Signature")
				      (doc . "\
Highlight the signature as specified by `Article Signature Face'.
")
				      (type . const)
				      (default .
					gnus-article-highlight-signature))
				     ((tag . "Citation")
				      (doc . "\
Highlight the citations as specified by `Citation Faces'.
")
				      (type . const)
				      (default . 
					gnus-article-highlight-citation))
				     ((tag . "Hide")
				      (doc . "\
Hide unwanted headers, excess citation, and the signature.
")
				      (type . const)
				      (default . gnus-article-hide))
				     ((tag . "Hide Headers If Wanted")
				      (doc . "\
Hide headers, but allow user to display them with `t' or `v'.
")
				      (type . const)
				      (default . 
					gnus-article-hide-headers-if-wanted))
				     ((tag . "Hide Headers")
				      (doc . "\
Hide unwanted headers and possibly sort them as well.
Most likely you want to use `Hide Headers If Wanted' instead.
")
				      (type . const)
				      (default . gnus-article-hide-headers))
				     ((tag . "Hide Signature")
				      (doc . "\
Hide the signature.
")
				      (type . const)
				      (default . gnus-article-hide-signature))
				     ((tag . "Hide Excess Citations")
				      (doc . "\
Hide excess citation.

Excess is defined by `Citation Hide Percentage' and `Citation Hide Absolute'.
")
				      (type . const)
				      (default . 
					gnus-article-hide-citation-maybe))
				     ((tag . "Hide Citations")
				      (doc . "\
Hide all cited text.
")
				      (type . const)
				      (default . gnus-article-hide-citation))
				     ((tag . "Add Buttons")
				      (doc . "\
Make URL's into clickable buttons.
")
				      (type . const)
				      (default . gnus-article-add-buttons))
				     ((prompt . "Other")
				      (doc . "\
Name of Lisp function to call.

Push the `Filter' button to select one of the predefined filters.
")
				      (type . symbol)))))))
		 ((tag . "Article Button Face")
		  (doc . "\
Face used for highlighting buttons in the article buffer.

An article button is a piece of text that you can activate by pressing
`RET' or `mouse-2' above it.")
		  (name . gnus-article-button-face)
		  (default . bold)
		  (type . face))
		 ((tag . "Article Mouse Face")
		  (doc . "\
Face used for mouse highlighting in the article buffer.

Article buttons will be displayed in this face when the cursor is
above them.")
		  (name . gnus-article-mouse-face)
		  (default . highlight)
		  (type . face))
		 ((tag . "Article Signature Face")
		  (doc . "\
Face used for highlighting a signature in the article buffer.")
		  (name . gnus-signature-face)
		  (default . italic)
		  (type . face))
		 ((tag . "Article Header Highlighting")
		  (doc . "\
Controls highlighting of article header.

Below is a list of article header names, and the faces used for
displaying the name and content of the header.  The `Header' field
should contain the name of the header.  The field actually contains a
regular expression that should match the beginning of the header line,
but if you don't know what a regular expression is, just write the
name of the header.  The second field is the `Name' field, which
determines how the the header name (i.e. the part of the header left
of the `:') is displayed.  The third field is the `Content' field,
which determines how the content (i.e. the part of the header right of
the `:') is displayed.  

If you leave the last `Header' field in the list empty, the `Name' and
`Content' fields will determine how headers not listed above are
displayed.  

If you only want to change the display of the name part for a specific
header, specify `None' in the `Content' field.  Similarly, specify
`None' in the `Name' field if you only want to leave the name part
alone.")
		  (name . gnus-header-face-alist)
		  (type . list)
		  (calculate . (cond ((not (eq gnus-display-type 'color))
				      '(("" bold italic)))
				     ((eq gnus-background-mode 'dark)
				      (list (list "From" nil 
						  (custom-face-lookup 
						   "dark blue" nil nil t t 
						   'as-is))
					    (list "Subject" nil 
						  (custom-face-lookup
						   "pink" nil nil t t 'as-is))
					    (list "Newsgroups:.*," nil
						  (custom-face-lookup
						   "yellow" nil nil t t 'as-is))
					    (list "" 
						  (custom-face-lookup
						   "cyan" nil nil t 'as-is 'as-is)
						  (custom-face-lookup
						   "forestgreen"
						   nil nil 'as-is t 'as-is))))
				     (t
				      (list (list "From" nil 
						  (custom-face-lookup
						   "RoyalBlue"
						   nil nil t t 'as-is))
					    (list "Subject" nil 
						  (custom-face-lookup
						   "firebrick"
						   nil nil t t 'as-is))
					    (list "Newsgroups:.*," nil
						  (custom-face-lookup
						   "indianred" nil nil t t 'as-is))
					    (list ""
						  (custom-face-lookup
						   "DarkGreen"
						   nil nil t 'as-is 'as-is)
						  (custom-face-lookup
						   "DarkGreen"
						   nil nil nil t 'as-is))))))
		  (data ((type . repeat)
			 (header . nil)
			 (data (type . list)
			       (compact . t)
			       (data ((type . string)
				      (prompt . "Header")
				      (tag . "Header "))
				     "\n            "
				     ((type . face)
				      (prompt . "Name")
				      (tag . "Name   "))
				     "\n            "
				     ((type . face)
				      (tag . "Content"))
				     "\n")))))
		 ((tag . "Attribution Face")
		  (doc . "\
Face used for attribution lines.
It is merged with the face for the cited text belonging to the attribution.")
		  (name . gnus-cite-attribution-face)
		  (default . underline)
		  (type . face))
		 ((tag . "Citation Faces")
		  (doc . "\
List of faces used for highlighting citations. 

When there are citations from multiple articles in the same message,
Gnus will try to give each citation from each article its own face.
This should make it easier to see who wrote what.")
		  (name . gnus-cite-face-list)
		  (import . gnus-custom-import-cite-face-list)
		  (type . list)
		  (calculate . (cond ((not (eq gnus-display-type 'color))
				      '(italic))
				     ((eq gnus-background-mode 'dark)
				      (mapcar 'gnus-make-face 
					      gnus-face-light-name-list))
				     (t 
				      (mapcar 'gnus-make-face 
					      gnus-face-dark-name-list))))
		  (data ((type . repeat)
			 (header . nil)
			 (data (type . face)
			       (tag . "Face")))))
		 ((tag . "Citation Hide Percentage")
		  (doc . "\
Only hide excess citation if above this percentage of the body.")
		  (name . gnus-cite-hide-percentage)
		  (default . 50)
		  (type . integer))
		 ((tag . "Citation Hide Absolute")
		  (doc . "\
Only hide excess citation if above this number of lines in the body.")
		  (name . gnus-cite-hide-absolute)
		  (default . 10)
		  (type . integer))
		 ((tag . "Summary Selected Face")
		  (doc . "\
Face used for highlighting the current article in the summary buffer.")
		  (name . gnus-summary-selected-face)
		  (default . underline)
		  (type . face))
		 ((tag . "Summary Line Highlighting")
		  (doc . "\
Controls the highlighting of summary buffer lines. 

Below is a list of `Form'/`Face' pairs.  When deciding how a a
particular summary line should be displayed, each form is
evaluated. The content of the face field after the first true form is
used.  You can change how those summary lines are displayed, by
editing the face field.  

It is also possible to change and add form fields, but currently that
requires an understanding of Lisp expressions.  Hopefully this will
change in a future release.  For now, you can use the following
variables in the Lisp expression:

score:   The article's score
default: The default article score.
below:   The score below which articles are automatically marked as read. 
mark:    The article's mark.")
		  (name . gnus-summary-highlight)
		  (type . list)
		  (calculate . (cond ((not (eq gnus-display-type 'color))
				      '(((> score default) . bold)
					((< score default) . italic)))
				     ((eq gnus-background-mode 'dark)
				      (list (cons '(= mark gnus-canceled-mark)
						  (custom-face-lookup "yellow" "black" nil 'as-is 'as-is 'as-is))
					    (cons '(and (> score default) 
							(or (= mark gnus-dormant-mark)
							    (= mark gnus-ticked-mark)))
						  (custom-face-lookup "pink" nil nil t 'as-is 'as-is))
					    (cons '(and (< score default) 
							(or (= mark gnus-dormant-mark)
							    (= mark gnus-ticked-mark)))
						  (custom-face-lookup "pink" nil nil 'as-is t 'as-is))
					    (cons '(or (= mark gnus-dormant-mark)
						       (= mark gnus-ticked-mark))
						  (custom-face-lookup "pink" nil nil 'as-is 'as-is 'as-is))

					    (cons '(and (> score default) (= mark gnus-ancient-mark))
						  (custom-face-lookup "dark blue" nil nil t 'as-is 'as-is))
					    (cons '(and (< score default) (= mark gnus-ancient-mark))
						  (custom-face-lookup "SkyBlue" nil nil 'as-is t 'as-is))
					    (cons '(= mark gnus-ancient-mark)
						  (custom-face-lookup "SkyBlue" nil nil 'as-is 'as-is 'as-is))

					    (cons '(and (> score default) (= mark gnus-unread-mark))
						  (custom-face-lookup "white" nil nil t 'as-is 'as-is))
					    (cons '(and (< score default) (= mark gnus-unread-mark))
						  (custom-face-lookup "white" nil nil 'as-is t 'as-is))
					    (cons '(= mark gnus-unread-mark)
						  (custom-face-lookup "white" nil nil 'as-is 'as-is 'as-is))

					    (cons '(> score default) 'bold)
					    (cons '(< score default) 'italic)))
				     (t
				      (list (cons '(= mark gnus-canceled-mark)
						  (custom-face-lookup "yellow" "black" nil 'as-is 'as-is 'as-is))
					    (cons '(and (> score default) 
							(or (= mark gnus-dormant-mark)
							    (= mark gnus-ticked-mark)))
						  (custom-face-lookup "firebrick" nil nil t 'as-is 'as-is))
					    (cons '(and (< score default) 
							(or (= mark gnus-dormant-mark)
							    (= mark gnus-ticked-mark)))
						  (custom-face-lookup "firebrick" nil nil 'as-is t 'as-is))
					    (cons '(or (= mark gnus-dormant-mark)
						       (= mark gnus-ticked-mark))
						  (custom-face-lookup "firebrick" nil nil 'as-is 'as-is 'as-is))

					    (cons '(and (> score default) (= mark gnus-ancient-mark))
						  (custom-face-lookup "RoyalBlue" nil nil t 'as-is 'as-is))
					    (cons '(and (< score default) (= mark gnus-ancient-mark))
						  (custom-face-lookup "RoyalBlue" nil nil 'as-is t 'as-is))
					    (cons '(= mark gnus-ancient-mark)
						  (custom-face-lookup "RoyalBlue" nil nil 'as-is 'as-is 'as-is))

					    (cons '(and (> score default) (/= mark gnus-unread-mark))
						  (custom-face-lookup "DarkGreen" nil nil t 'as-is 'as-is))
					    (cons '(and (< score default) (/= mark gnus-unread-mark))
						  (custom-face-lookup "DarkGreen" nil nil 'as-is t 'as-is))
					    (cons '(/= mark gnus-unread-mark)
						  (custom-face-lookup "DarkGreen" nil nil 'as-is 'as-is 'as-is))

					    (cons '(> score default) 'bold)
					    (cons '(< score default) 'italic)))))
		  (data ((type . repeat)
			 (header . nil)
			 (data (type . pair)
			       (compact . t)
			       (data ((type . sexp)
				      (width . 60)
				      (tag . "Form"))
				     "\n            "
				     ((type . face)
				      (tag . "Face"))
				     "\n")))))
		 ;; Do not define `gnus-button-alist' before we have
		 ;; some `complexity' attribute so we can hide it from
		 ;; beginners. 
		 )))))

(defun gnus-custom-import-cite-face-list (custom alist)
  ;; Backward compatible grokking of light and dark.
  (cond ((eq alist 'light)
	 (setq alist (mapcar 'gnus-make-face gnus-face-light-name-list)))
	((eq alist 'dark)
	 (setq alist (mapcar 'gnus-make-face gnus-face-dark-name-list))))
  (funcall (custom-super custom 'import) custom alist))

;(defun gnus-custom-import-swap-alist (custom alist)
;  ;; Swap key and value in CUSTOM ALIST.
;  (let ((swap (mapcar (lambda (e) (cons (cdr e) (car e))) alist)))
;    (funcall (custom-super custom 'import) custom swap)))

;(defun gnus-custom-export-swap-alist (custom alist)
;  ;; Swap key and value in CUSTOM ALIST.
;  (let ((swap (mapcar (lambda (e) (cons (cdr e) (car e))) alist)))
;    (funcall (custom-super custom 'export) custom swap)))

(provide 'gnus-cus)

;;; gnus-cus.el ends here
