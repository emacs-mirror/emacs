;;; newst-reader.el --- Generic RSS reader functions.

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
;;   Free Software Foundation, Inc.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Filename:    newst-reader.el
;; URL:         http://www.nongnu.org/newsticker
;; Time-stamp:  "6. Dezember 2009, 19:16:38 (ulf)"

;; ======================================================================

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; ======================================================================
;;; Commentary:

;; See newsticker.el

;; ======================================================================
;;; Code:

(require 'newst-backend)

;; ======================================================================
;;; Customization
;; ======================================================================
(defun newsticker--set-customvar-formatting (symbol value)
  "Set newsticker-variable SYMBOL value to VALUE.
Calls all actions which are necessary in order to make the new
value effective."
  (if (or (not (boundp symbol))
          (equal (symbol-value symbol) value))
      (set symbol value)
    ;; something must have changed
    (set symbol value)
    (when (fboundp 'newsticker--forget-preformatted)
      (newsticker--forget-preformatted))))

;; ======================================================================
;; reader
(defgroup newsticker-reader nil
  "Settings for the feed reader."
  :group 'newsticker)

(defcustom newsticker-frontend
  'newsticker-treeview
  "Newsticker frontend for reading news.
This must be one of the functions `newsticker-plainview' or
`newsticker-treeview'."
  :type '(choice :tag "Frontend"
                 (const :tag "Single buffer (plainview)" newsticker-plainview)
                 (const :tag "Tree view (treeview)" newsticker-treeview))
  :group 'newsticker-reader)

;; image related things
(defcustom newsticker-enable-logo-manipulations
  t
  "If non-nil newsticker manipulates logo images.
This enables the following image properties: heuristic mask for all
logos, and laplace-conversion for images without new items."
  :type 'boolean
  :group 'newsticker-reader)

(defcustom newsticker-justification
  'left
  "How to fill item descriptions.
If non-nil newsticker calls `fill-region' to wrap long lines in
item descriptions.  However, if an item description contains HTML
text and `newsticker-html-renderer' is non-nil, filling is not
done."
  :type '(choice :tag "Justification"
                 (const :tag "No filling" nil)
                 (const :tag "Left"       left)
                 (const :tag "Right"      right)
                 (const :tag "Center"     center)
                 (const :tag "Full"       full))
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-reader)

(defcustom newsticker-use-full-width
  t
  "Decides whether to use the full window width when filling.
If non-nil newsticker sets `fill-column' so that the whole
window is used when filling.  See also `newsticker-justification'."
  :type 'boolean
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-reader)

(defcustom newsticker-html-renderer
  nil
  "Function for rendering HTML contents.
If non-nil, newsticker.el will call this function whenever it finds
HTML-like tags in item descriptions.  Possible functions are, for
example, `w3m-region', `w3-region', and (if you have htmlr.el installed)
`newsticker-htmlr-render'.

In order to make sure that the HTML renderer is loaded when you
run newsticker, you should add one of the following statements to
your .emacs.  If you use w3m,

  (autoload 'w3m-region \"w3m\"
    \"Render region in current buffer and replace with result.\" t)

  (autoload 'w3m-toggle-inline-image \"w3m\"
    \"Toggle the visibility of an image under point.\" t)

or, if you use w3,

  (require 'w3-auto)

or, if you use htmlr

  (require 'htmlr)"
  :type '(choice :tag "Function"
                 (const :tag "None" nil)
                 (const :tag "w3" w3-region)
                 (const :tag "w3m" w3m-region)
                 (const :tag "htmlr" newsticker-htmlr-render))
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-reader)

(defcustom newsticker-date-format
  "(%A, %H:%M)"
  "Format for the date part in item and feed lines.
See `format-time-string' for a list of valid specifiers."
  :type 'string
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-reader)

(defgroup newsticker-faces nil
  "Settings for the faces of the feed reader."
  :group 'newsticker-reader)

(defface newsticker-feed-face
  '((((class color) (background dark))
     (:family "helvetica" :bold t :height 1.2 :foreground "misty rose"))
    (((class color) (background light))
     (:family "helvetica" :bold t :height 1.2 :foreground "black")))
  "Face for news feeds."
  :group 'newsticker-faces)

(defface newsticker-extra-face
  '((((class color) (background dark))
     (:italic t :foreground "gray50" :height 0.8))
    (((class color) (background light))
     (:italic t :foreground "gray50" :height 0.8)))
  "Face for newsticker dates."
  :group 'newsticker-faces)

(defface newsticker-enclosure-face
  '((((class color) (background dark))
     (:bold t :background "orange"))
    (((class color) (background light))
     (:bold t :background "orange")))
  "Face for enclosed elements."
  :group 'newsticker-faces)

;; ======================================================================
;;; Utility functions
;; ======================================================================
(defun newsticker--insert-enclosure (item keymap)
  "Insert enclosure element of a news ITEM into the current buffer.
KEYMAP will be applied."
  (let ((enclosure (newsticker--enclosure item))
        (beg (point)))
    (when enclosure
      (let ((url (cdr (assoc 'url enclosure)))
            (length (string-to-number (or (cdr (assoc 'length enclosure))
                                          "-1")))
            (type (cdr (assoc 'type enclosure))))
        (cond ((> length 1048576)
               (insert (format "Enclosed file (%s, %1.2f MBytes)" type
                               (/ length 1048576))))
              ((> length 1024)
               (insert (format "Enclosed file (%s, %1.2f KBytes)" type
                               (/ length 1024))))
              ((> length 0)
               (insert (format "Enclosed file (%s, %1.2f Bytes)" type
                               length)))
              (t
               (insert (format "Enclosed file (%s, unknown size)" type))))
        (add-text-properties beg (point)
                             (list 'mouse-face 'highlight
                                   'nt-link url
                                   'help-echo (format
                                               "mouse-2: visit (%s)" url)
                                   'keymap keymap
                                   'nt-face 'enclosure
                                   'nt-type 'desc))
        (insert "\n")))))

(defun newsticker--print-extra-elements (item keymap)
  "Insert extra-elements of ITEM in a pretty form into the current buffer.
KEYMAP is applied."
  (let ((ignored-elements '(items link title description content
                                  content:encoded dc:subject
                                  dc:date entry item guid pubDate
                                  published updated
                                  enclosure))
        (left-column-width 1))
    (mapc (lambda (extra-element)
            (when (listp extra-element) ;; take care of broken xml
                                        ;; data, 2007-05-25
              (unless (memq (car extra-element) ignored-elements)
                (setq left-column-width (max left-column-width
                                             (length (symbol-name
                                                      (car extra-element))))))))
          (newsticker--extra item))
    (mapc (lambda (extra-element)
            (when (listp extra-element) ;; take care of broken xml
                                        ;; data, 2007-05-25
              (unless (memq (car extra-element) ignored-elements)
                (newsticker--do-print-extra-element extra-element
                                                    left-column-width
                                                    keymap))))
          (newsticker--extra item))))

(defun newsticker--do-print-extra-element (extra-element width keymap)
  "Actually print an EXTRA-ELEMENT using the given WIDTH.
KEYMAP is applied."
  (let ((name (symbol-name (car extra-element))))
    (insert (format "%s: " name))
    (insert (make-string (- width (length name)) ? )))
  (let (;;(attributes (cadr extra-element)) ;FIXME!!!!
        (contents (cddr extra-element)))
    (cond ((listp contents)
           (mapc (lambda (i)
                   (if (and (stringp i)
                            (string-match "^http://.*" i))
                       (let ((pos (point)))
                         (insert i " ") ; avoid self-reference from the
                                        ; nt-link thing
                         (add-text-properties
                          pos (point)
                          (list 'mouse-face 'highlight
                                'nt-link i
                                'help-echo
                                (format "mouse-2: visit (%s)" i)
                                'keymap keymap)))
                         (insert (format "%s" i))))
                 contents))
          (t
           (insert (format "%s" contents))))
    (insert "\n")))

(defun newsticker--image-read (feed-name-symbol disabled)
  "Read the cached image for FEED-NAME-SYMBOL from disk.
If DISABLED is non-nil the image will be converted to a disabled look
\(unless `newsticker-enable-logo-manipulations' is not t\).
Return the image."
  (let ((image-name (concat (newsticker--images-dir)
                            (symbol-name feed-name-symbol)))
        (img nil))
    (when (file-exists-p image-name)
      (condition-case error-data
          (setq img (create-image
                     image-name nil nil
                     :conversion (and newsticker-enable-logo-manipulations
                                      disabled
                                      'disabled)
                     :mask (and newsticker-enable-logo-manipulations
                                'heuristic)
                     :ascent 70))
        (error
         (message "Error: cannot create image for %s: %s"
                  feed-name-symbol error-data))))
    img))

;; the functions we need for retrieval and display
;;;###autoload
(defun newsticker-show-news ()
  "Start reading news.  You may want to bind this to a key."
  (interactive)
  (newsticker-start t) ;; will start only if not running
  (funcall newsticker-frontend))

;; ======================================================================
;;; Toolbar
;; ======================================================================
(defconst newsticker--next-item-image
  (and (fboundp 'image-type-available-p)
       (image-type-available-p 'xpm)
       (create-image "/* XPM */
static char * next_xpm[] = {
\"24 24 42 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #7EB6DE\",
\"@	c #82BBE2\",
\"#	c #85BEE4\",
\"$	c #88C1E7\",
\"%	c #8AC3E8\",
\"&	c #87C1E6\",
\"*	c #8AC4E9\",
\"=	c #8CC6EA\",
\"-	c #8CC6EB\",
\";	c #88C2E7\",
\">	c #8BC5E9\",
\",	c #8DC7EB\",
\"'	c #87C0E6\",
\")	c #8AC4E8\",
\"!	c #8BC5EA\",
\"~	c #8BC4E9\",
\"{	c #88C1E6\",
\"]	c #89C3E8\",
\"^	c #86BFE5\",
\"/	c #83BBE2\",
\"(	c #82BBE1\",
\"_	c #86C0E5\",
\":	c #87C0E5\",
\"<	c #83BCE2\",
\"[	c #81B9E0\",
\"}	c #81BAE1\",
\"|	c #78B0D9\",
\"1	c #7BB3DB\",
\"2	c #7DB5DD\",
\"3	c #7DB6DD\",
\"4	c #72A9D4\",
\"5	c #75ACD6\",
\"6	c #76AED7\",
\"7	c #77AFD8\",
\"8	c #6BA1CD\",
\"9	c #6EA4CF\",
\"0	c #6FA6D1\",
\"a	c #6298C6\",
\"b	c #659BC8\",
\"c	c #5C91C0\",
\"                        \",
\"                        \",
\"       .                \",
\"       ..               \",
\"       .+.              \",
\"       .@#.             \",
\"       .#$%.            \",
\"       .&*=-.           \",
\"       .;>,,,.          \",
\"       .;>,,,=.         \",
\"       .')!==~;.        \",
\"       .#{]*%;^/.       \",
\"       .(#_':#<.        \",
\"       .+[@</}.         \",
\"       .|1232.          \",
\"       .4567.           \",
\"       .890.            \",
\"       .ab.             \",
\"       .c.              \",
\"       ..               \",
\"       .                \",
\"                        \",
\"                        \",
\"                        \"};
"
                     'xpm t))
  "Image for the next item button.")

(defconst newsticker--previous-item-image
  (and (fboundp 'image-type-available-p)
       (image-type-available-p 'xpm)
       (create-image "/* XPM */
static char * previous_xpm[] = {
\"24 24 39 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #7BB3DB\",
\"@	c #83BCE2\",
\"#	c #7FB8DF\",
\"$	c #89C2E7\",
\"%	c #86BFE5\",
\"&	c #83BBE2\",
\"*	c #8CC6EA\",
\"=	c #8BC4E9\",
\"-	c #88C2E7\",
\";	c #85BEE4\",
\">	c #8DC7EB\",
\",	c #89C3E8\",
\"'	c #8AC4E8\",
\")	c #8BC5EA\",
\"!	c #88C1E6\",
\"~	c #8AC4E9\",
\"{	c #8AC3E8\",
\"]	c #86C0E5\",
\"^	c #87C0E6\",
\"/	c #87C0E5\",
\"(	c #82BBE2\",
\"_	c #81BAE1\",
\":	c #7FB7DF\",
\"<	c #7DB6DD\",
\"[	c #7DB5DD\",
\"}	c #7CB4DC\",
\"|	c #79B1DA\",
\"1	c #76ADD7\",
\"2	c #77AFD8\",
\"3	c #73AAD4\",
\"4	c #70A7D1\",
\"5	c #6EA5D0\",
\"6	c #6CA2CE\",
\"7	c #689ECB\",
\"8	c #6399C7\",
\"9	c #6095C4\",
\"0	c #5C90C0\",
\"                        \",
\"                        \",
\"                .       \",
\"               ..       \",
\"              .+.       \",
\"             .@#.       \",
\"            .$%&.       \",
\"           .*=-;.       \",
\"          .>>*,%.       \",
\"         .>>>*,%.       \",
\"        .')**=-;.       \",
\"       .;!,~{-%&.       \",
\"        .;]^/;@#.       \",
\"         .(@&_:+.       \",
\"          .<[}|1.       \",
\"           .2134.       \",
\"            .567.       \",
\"             .89.       \",
\"              .0.       \",
\"               ..       \",
\"                .       \",
\"                        \",
\"                        \",
\"                        \"};
"
                     'xpm t))
  "Image for the previous item button.")

(defconst newsticker--previous-feed-image
  (and (fboundp 'image-type-available-p)
       (image-type-available-p 'xpm)
       (create-image "/* XPM */
static char * prev_feed_xpm[] = {
\"24 24 52 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #70A7D2\",
\"@	c #75ADD6\",
\"#	c #71A8D3\",
\"$	c #79B1DA\",
\"%	c #7BB3DB\",
\"&	c #7DB5DD\",
\"*	c #83BBE2\",
\"=	c #7EB6DE\",
\"-	c #78B0D9\",
\";	c #7FB7DE\",
\">	c #88C2E7\",
\",	c #85BEE4\",
\"'	c #80B9E0\",
\")	c #80B8DF\",
\"!	c #8CC6EA\",
\"~	c #89C3E8\",
\"{	c #86BFE5\",
\"]	c #81BAE1\",
\"^	c #7CB4DC\",
\"/	c #7FB8DF\",
\"(	c #8DC7EB\",
\"_	c #7BB3DC\",
\":	c #7EB7DE\",
\"<	c #8BC4E9\",
\"[	c #8AC4E9\",
\"}	c #8AC3E8\",
\"|	c #87C0E6\",
\"1	c #87C0E5\",
\"2	c #83BCE2\",
\"3	c #75ACD6\",
\"4	c #7FB7DF\",
\"5	c #77AED8\",
\"6	c #71A8D2\",
\"7	c #70A7D1\",
\"8	c #76ADD7\",
\"9	c #6CA2CE\",
\"0	c #699FCC\",
\"a	c #73AAD4\",
\"b	c #6BA1CD\",
\"c	c #669CC9\",
\"d	c #6298C5\",
\"e	c #689ECB\",
\"f	c #6499C7\",
\"g	c #6095C3\",
\"h	c #5C91C0\",
\"i	c #5E93C2\",
\"j	c #5B90C0\",
\"k	c #588CBC\",
\"l	c #578CBC\",
\"m	c #5589BA\",
\"                        \",
\"                        \",
\"     ...          .     \",
\"     .+.         ..     \",
\"     .@.        .#.     \",
\"     .$.       .%@.     \",
\"     .&.      .*=-.     \",
\"     .;.     .>,'%.     \",
\"     .).    .!~{]^.     \",
\"     ./.   .(!~{]_.     \",
\"     .:.  .!!<>,'%.     \",
\"     .&. .~[}>{*=-.     \",
\"     .$.  .|1,2/%@.     \",
\"     .3.   .*]4%56.     \",
\"     .7.    .^$8#9.     \",
\"     .0.     .a7bc.     \",
\"     .d.      .efg.     \",
\"     .h.       .ij.     \",
\"     .k.        .l.     \",
\"     .m.         ..     \",
\"     ...          .     \",
\"                        \",
\"                        \",
\"                        \"};
"
                     'xpm t))
  "Image for the previous feed button.")

(defconst newsticker--next-feed-image
  (and (fboundp 'image-type-available-p)
       (image-type-available-p 'xpm)
       (create-image "/* XPM */
static char * next_feed_xpm[] = {
\"24 24 57 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #6CA2CE\",
\"@	c #75ADD6\",
\"#	c #71A8D3\",
\"$	c #79B1DA\",
\"%	c #7EB7DE\",
\"&	c #7DB5DD\",
\"*	c #81BAE1\",
\"=	c #85BEE4\",
\"-	c #78B0D9\",
\";	c #7FB7DE\",
\">	c #83BCE3\",
\",	c #87C1E6\",
\"'	c #8AC4E9\",
\")	c #7BB3DB\",
\"!	c #80B8DF\",
\"~	c #88C2E7\",
\"{	c #8BC5E9\",
\"]	c #8DC7EB\",
\"^	c #7CB4DC\",
\"/	c #7FB8DF\",
\"(	c #84BDE3\",
\"_	c #7BB3DC\",
\":	c #83BCE2\",
\"<	c #87C0E6\",
\"[	c #8AC4E8\",
\"}	c #8BC5EA\",
\"|	c #8CC6EA\",
\"1	c #88C1E6\",
\"2	c #89C3E8\",
\"3	c #8AC3E8\",
\"4	c #7EB6DE\",
\"5	c #82BBE1\",
\"6	c #86C0E5\",
\"7	c #87C0E5\",
\"8	c #75ACD6\",
\"9	c #7AB2DA\",
\"0	c #81B9E0\",
\"a	c #82BBE2\",
\"b	c #71A8D2\",
\"c	c #70A7D1\",
\"d	c #74ACD6\",
\"e	c #699FCC\",
\"f	c #6EA5D0\",
\"g	c #72A9D4\",
\"h	c #669CC9\",
\"i	c #6298C5\",
\"j	c #679DCA\",
\"k	c #6BA1CD\",
\"l	c #6095C3\",
\"m	c #5C91C0\",
\"n	c #5F94C2\",
\"o	c #5B90C0\",
\"p	c #588CBC\",
\"q	c #578CBC\",
\"r	c #5589BA\",
\"                        \",
\"                        \",
\"     .          ...     \",
\"     ..         .+.     \",
\"     .@.        .#.     \",
\"     .$%.       .@.     \",
\"     .&*=.      .-.     \",
\"     .;>,'.     .).     \",
\"     .!=~{].    .^.     \",
\"     ./(~{]].   ._.     \",
\"     .%:<[}||.  .).     \",
\"     .&*=12'3~. .-.     \",
\"     .$45=6<7.  .@.     \",
\"     .8940a:.   .b.     \",
\"     .cd-)&.    .+.     \",
\"     .efg8.     .h.     \",
\"     .ijk.      .l.     \",
\"     .mn.       .o.     \",
\"     .p.        .q.     \",
\"     ..         .r.     \",
\"     .          ...     \",
\"                        \",
\"                        \",
\"                        \"};
"
                     'xpm t))
  "Image for the next feed button.")

(defconst newsticker--mark-read-image
  (and (fboundp 'image-type-available-p)
       (image-type-available-p 'xpm)
       (create-image "/* XPM */
static char * mark_read_xpm[] = {
\"24 24 44 1\",
\" 	c None\",
\".	c #C20000\",
\"+	c #BE0000\",
\"@	c #C70000\",
\"#	c #CE0000\",
\"$	c #C90000\",
\"%	c #BD0000\",
\"&	c #CB0000\",
\"*	c #D10000\",
\"=	c #D70000\",
\"-	c #D30000\",
\";	c #CD0000\",
\">	c #C60000\",
\",	c #D40000\",
\"'	c #DA0000\",
\")	c #DE0000\",
\"!	c #DB0000\",
\"~	c #D60000\",
\"{	c #D00000\",
\"]	c #DC0000\",
\"^	c #E00000\",
\"/	c #E40000\",
\"(	c #E10000\",
\"_	c #DD0000\",
\":	c #D80000\",
\"<	c #E50000\",
\"[	c #E70000\",
\"}	c #E60000\",
\"|	c #E20000\",
\"1	c #E90000\",
\"2	c #E80000\",
\"3	c #E30000\",
\"4	c #DF0000\",
\"5	c #D90000\",
\"6	c #CC0000\",
\"7	c #C10000\",
\"8	c #C30000\",
\"9	c #BF0000\",
\"0	c #B90000\",
\"a	c #BC0000\",
\"b	c #BB0000\",
\"c	c #B80000\",
\"d	c #B50000\",
\"e	c #B70000\",
\"                        \",
\"                        \",
\"                        \",
\"    .              +    \",
\"   +@#            $.%   \",
\"    &*=          -;>    \",
\"     ,')        !~{     \",
\"      ]^/      (_:      \",
\"       (<[    }|)       \",
\"        <[1  2<|        \",
\"         }222[<         \",
\"          }}}<          \",
\"          333|          \",
\"         _4^4)]         \",
\"        ~:'  5=-        \",
\"       6{-    *#$       \",
\"      7>$      @89      \",
\"     0a+        %bc     \",
\"    ddc          edd    \",
\"   ddd            ddd   \",
\"    d              d    \",
\"                        \",
\"                        \",
\"                        \"};
"
                    'xpm t))
   "Image for the mark read button.")

(defconst newsticker--mark-immortal-image
  (and (fboundp 'image-type-available-p)
       (image-type-available-p 'xpm)
       (create-image "/* XPM */
static char * mark_immortal_xpm[] = {
\"24 24 93 2\",
\"  	c None\",
\". 	c #171717\",
\"+ 	c #030303\",
\"@ 	c #000000\",
\"# 	c #181818\",
\"$ 	c #090909\",
\"% 	c #FFC960\",
\"& 	c #FFCB61\",
\"* 	c #FFCB62\",
\"= 	c #FFC961\",
\"- 	c #FFC75F\",
\"; 	c #FFC65E\",
\"> 	c #FFCA61\",
\", 	c #FFCD63\",
\"' 	c #FFCF65\",
\") 	c #FFD065\",
\"! 	c #FFCE64\",
\"~ 	c #FFC35C\",
\"{ 	c #FFC45D\",
\"] 	c #FFD166\",
\"^ 	c #FFD267\",
\"/ 	c #FFD368\",
\"( 	c #FFD167\",
\"_ 	c #FFC05A\",
\": 	c #010101\",
\"< 	c #040404\",
\"[ 	c #FFCC62\",
\"} 	c #FFD569\",
\"| 	c #FFD56A\",
\"1 	c #FFC860\",
\"2 	c #FFC25B\",
\"3 	c #FFBB56\",
\"4 	c #020202\",
\"5 	c #060606\",
\"6 	c #FFC15B\",
\"7 	c #FFC85F\",
\"8 	c #FFD469\",
\"9 	c #FFD66A\",
\"0 	c #FFBC57\",
\"a 	c #1B1B1B\",
\"b 	c #070707\",
\"c 	c #FFBA55\",
\"d 	c #FFB451\",
\"e 	c #FFB954\",
\"f 	c #FFB350\",
\"g 	c #FFB652\",
\"h 	c #FFBE58\",
\"i 	c #FFCD64\",
\"j 	c #FFD066\",
\"k 	c #FFC059\",
\"l 	c #FFB14E\",
\"m 	c #0B0B0B\",
\"n 	c #FFBB55\",
\"o 	c #FFC15A\",
\"p 	c #FFB552\",
\"q 	c #FFAD4B\",
\"r 	c #080808\",
\"s 	c #FFAF4C\",
\"t 	c #FFB853\",
\"u 	c #FFA948\",
\"v 	c #050505\",
\"w 	c #FFB04E\",
\"x 	c #FFB753\",
\"y 	c #FFBC56\",
\"z 	c #FFC55D\",
\"A 	c #FFC55E\",
\"B 	c #FFC45C\",
\"C 	c #FFBD57\",
\"D 	c #FFB854\",
\"E 	c #FFB34F\",
\"F 	c #FFAB4A\",
\"G 	c #FFA545\",
\"H 	c #FFAA49\",
\"I 	c #FFB04D\",
\"J 	c #FFB551\",
\"K 	c #FFBF58\",
\"L 	c #FFB24F\",
\"M 	c #FFAC4A\",
\"N 	c #FFA646\",
\"O 	c #FFA344\",
\"P 	c #FFA848\",
\"Q 	c #FFB14F\",
\"R 	c #FFAF4D\",
\"S 	c #FFA546\",
\"T 	c #FFA243\",
\"U 	c #FFA445\",
\"V 	c #FFAE4C\",
\"W 	c #FFA444\",
\"X 	c #FFA142\",
\"Y 	c #FF9F41\",
\"Z 	c #0A0A0A\",
\"` 	c #FF9E40\",
\" .	c #FF9F40\",
\"                                                \",
\"                                                \",
\"                                                \",
\"                  . + @ @ + #                   \",
\"              $ @ % & * * = - + +               \",
\"            @ ; > , ' ) ' ! * - ~ @             \",
\"          @ { > ! ] ^ / / ( ' * ; _ :           \",
\"        < _ ; [ ) / } | } / ] , 1 2 3 4         \",
\"        5 6 7 , ] 8 9 9 9 } ^ ! = ~ 0 a         \",
\"      b c 6 - , ] 8 9 9 9 } ^ ! % ~ 0 d 5       \",
\"      : e _ ; * ) / 8 } } / ] , 1 2 3 f 5       \",
\"      : g h { = i j ^ / ^ ] ! * ; k e l m       \",
\"      : f n o ; > , ' ) ' ! * - 2 0 p q r       \",
\"      : s g 0 6 ; % > * * = - ~ h t l u r       \",
\"      v u w x y k ~ z A z B o C D E F G b       \",
\"        5 H I J e 0 h K h C c x L M N .         \",
\"        4 O P q Q d g x g J L R H S T <         \",
\"          @ T U P F q V q M H N W X +           \",
\"            @ Y T O W G G W O X Y @             \",
\"              4 Z ` Y Y Y  .` 4 4               \",
\"                  5 : : @ @ Z                   \",
\"                                                \",
\"                                                \",
\"                                                \"};
"
                     'xpm t))
  "Image for the mark immortal button.")

(defconst newsticker--narrow-image
  (and (fboundp 'image-type-available-p)
       (image-type-available-p 'xpm)
       (create-image "/* XPM */
static char * narrow_xpm[] = {
\"24 24 48 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #969696\",
\"@	c #9E9E9E\",
\"#	c #A4A4A4\",
\"$	c #AAAAAA\",
\"%	c #AEAEAE\",
\"&	c #B1B1B1\",
\"*	c #B3B3B3\",
\"=	c #B4B4B4\",
\"-	c #B2B2B2\",
\";	c #AFAFAF\",
\">	c #ABABAB\",
\",	c #A6A6A6\",
\"'	c #A0A0A0\",
\")	c #989898\",
\"!	c #909090\",
\"~	c #73AAD4\",
\"{	c #7AB2DA\",
\"]	c #7FB8DF\",
\"^	c #84BDE3\",
\"/	c #88C2E7\",
\"(	c #8BC5E9\",
\"_	c #8DC7EB\",
\":	c #8CC6EA\",
\"<	c #89C3E8\",
\"[	c #86BFE5\",
\"}	c #81BAE1\",
\"|	c #7BB3DC\",
\"1	c #75ACD6\",
\"2	c #6DA4CF\",
\"3	c #979797\",
\"4	c #A3A3A3\",
\"5	c #A8A8A8\",
\"6	c #ADADAD\",
\"7	c #ACACAC\",
\"8	c #A9A9A9\",
\"9	c #A5A5A5\",
\"0	c #9A9A9A\",
\"a	c #929292\",
\"b	c #8C8C8C\",
\"c	c #808080\",
\"d	c #818181\",
\"e	c #838383\",
\"f	c #848484\",
\"g	c #858585\",
\"h	c #868686\",
\"i	c #828282\",
\"                        \",
\"                        \",
\"                        \",
\"   ..................   \",
\"   .+@#$%&*=*-;>,')!.   \",
\"   ..................   \",
\"                        \",
\"                        \",
\"   ..................   \",
\"   .~{]^/(___:<[}|12.   \",
\"   ..................   \",
\"                        \",
\"                        \",
\"   ..................   \",
\"   .!3@45>666789'0ab.   \",
\"   ..................   \",
\"                        \",
\"                        \",
\"   ..................   \",
\"   .cccdefghhgficccc.   \",
\"   ..................   \",
\"                        \",
\"                        \",
\"                        \"};
"
                     'xpm t))
  "Image for the narrow image button.")

(defconst newsticker--get-all-image
  (and (fboundp 'image-type-available-p)
       (image-type-available-p 'xpm)
       (create-image "/* XPM */
static char * get_all_xpm[] = {
\"24 24 70 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #F3DA00\",
\"@	c #F5DF00\",
\"#	c #F7E300\",
\"$	c #F9E700\",
\"%	c #FAEA00\",
\"&	c #FBEC00\",
\"*	c #FBED00\",
\"=	c #FCEE00\",
\"-	c #FAEB00\",
\";	c #F9E800\",
\">	c #F8E500\",
\",	c #F6E000\",
\"'	c #F4DB00\",
\")	c #F1D500\",
\"!	c #EFD000\",
\"~	c #B7CA00\",
\"{	c #BFD100\",
\"]	c #C5D700\",
\"^	c #CBDB00\",
\"/	c #CFDF00\",
\"(	c #D2E200\",
\"_	c #D4E400\",
\":	c #D3E300\",
\"<	c #D0E000\",
\"[	c #CCDD00\",
\"}	c #C7D800\",
\"|	c #C1D300\",
\"1	c #BACC00\",
\"2	c #B1C500\",
\"3	c #A8BC00\",
\"4	c #20A900\",
\"5	c #22AF00\",
\"6	c #24B500\",
\"7	c #26B900\",
\"8	c #27BC00\",
\"9	c #27BE00\",
\"0	c #28BF00\",
\"a	c #27BD00\",
\"b	c #26BA00\",
\"c	c #25B600\",
\"d	c #23B100\",
\"e	c #21AB00\",
\"f	c #1FA400\",
\"g	c #1C9B00\",
\"h	c #21AA00\",
\"i	c #24B300\",
\"j	c #25B800\",
\"k	c #25B700\",
\"l	c #24B400\",
\"m	c #23B000\",
\"n	c #1FA500\",
\"o	c #1D9E00\",
\"p	c #20A800\",
\"q	c #21AC00\",
\"r	c #23B200\",
\"s	c #22AD00\",
\"t	c #1D9F00\",
\"u	c #20A700\",
\"v	c #1EA100\",
\"w	c #1C9C00\",
\"x	c #1DA000\",
\"y	c #1B9800\",
\"z	c #1A9600\",
\"A	c #1A9700\",
\"B	c #1A9500\",
\"C	c #199200\",
\"D	c #189100\",
\"E	c #178C00\",
\"                        \",
\"                        \",
\"                        \",
\"                        \",
\"   ...................  \",
\"   .+@#$%&*=*&-;>,')!.  \",
\"   ...................  \",
\"                        \",
\"   ...................  \",
\"   .~{]^/(___:<[}|123.  \",
\"   ...................  \",
\"                        \",
\"   ...................  \",
\"    .45678909abcdefg.   \",
\"     .h5icj7jklmeno.    \",
\"      .pq5drrmshft.     \",
\"       .fu4h4pnvw.      \",
\"        .oxvxtwy.       \",
\"         .zAAzB.        \",
\"          .CCD.         \",
\"           .E.          \",
\"            .           \",
\"                        \",
\"                        \"};
"
                     'xpm t))
  "Image for the get all image button.")

(defconst newsticker--update-image
  (and (fboundp 'image-type-available-p)
       (image-type-available-p 'xpm)
       (create-image "/* XPM */
static char * update_xpm[] = {
\"24 24 37 1\",
\" 	c None\",
\".	c #076D00\",
\"+	c #0A8600\",
\"@	c #0A8800\",
\"#	c #098400\",
\"$	c #087200\",
\"%	c #087900\",
\"&	c #098500\",
\"*	c #098100\",
\"=	c #087600\",
\"-	c #097E00\",
\";	c #097F00\",
\">	c #0A8700\",
\",	c #0A8C00\",
\"'	c #097C00\",
\")	c #098300\",
\"!	c #0A8900\",
\"~	c #0A8E00\",
\"{	c #0B9200\",
\"]	c #087700\",
\"^	c #076E00\",
\"/	c #076C00\",
\"(	c #076B00\",
\"_	c #076A00\",
\":	c #076900\",
\"<	c #076800\",
\"[	c #066700\",
\"}	c #066500\",
\"|	c #066400\",
\"1	c #066300\",
\"2	c #066600\",
\"3	c #066200\",
\"4	c #076700\",
\"5	c #065E00\",
\"6	c #066100\",
\"7	c #065F00\",
\"8	c #066000\",
\"                        \",
\"                        \",
\"                        \",
\"    .    +@@@+#         \",
\"    $% &@      +*       \",
\"    =-#          ;      \",
\"    %*>,          '     \",
\"    ')!~{          =    \",
\"                   ]$   \",
\"   ^                ^   \",
\"   .                .   \",
\"   /                (   \",
\"   _                :   \",
\"   <                [   \",
\"   }                |   \",
\"   [[                   \",
\"    1          $.:23    \",
\"     3          4}35    \",
\"      6          655    \",
\"       76      85 55    \",
\"        5555555    5    \",
\"                        \",
\"                        \",
\"                        \"};
"
                     'xpm t))
   "Image for the update button.")

(defconst newsticker--browse-image
  (and (fboundp 'image-type-available-p)
       (image-type-available-p 'xpm)
       (create-image "/* XPM */
static char * visit_xpm[] = {
\"24 24 39 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #FFFFFF\",
\"@	c #00E63D\",
\"#	c #00E83E\",
\"$	c #00E73D\",
\"%	c #00E93E\",
\"&	c #00E63C\",
\"*	c #00E53C\",
\"=	c #00E23B\",
\"-	c #00E33B\",
\";	c #00E83D\",
\">	c #00E13A\",
\",	c #00DD38\",
\"'	c #00DE38\",
\")	c #00E23A\",
\"!	c #00E43C\",
\"~	c #00DF39\",
\"{	c #00DB37\",
\"]	c #00D634\",
\"^	c #00D734\",
\"/	c #00E039\",
\"(	c #00DC37\",
\"_	c #00D835\",
\":	c #00D332\",
\"<	c #00CD2F\",
\"[	c #00DB36\",
\"}	c #00D433\",
\"|	c #00CF30\",
\"1	c #00DA36\",
\"2	c #00D936\",
\"3	c #00D533\",
\"4	c #00D131\",
\"5	c #00CE2F\",
\"6	c #00CC2F\",
\"7	c #00CA2D\",
\"8	c #00C62B\",
\"9	c #00C52A\",
\"0	c #00BE27\",
\"                        \",
\"                        \",
\"            .           \",
\"           .+.          \",
\"          .+++.         \",
\"         .++.++.        \",
\"        .++.@.++.       \",
\"       .++.##$.++.      \",
\"      .++.%%%#&.++.     \",
\"     .++.$%%%#*=.++.    \",
\"    .++.-@;##$*>,.++.   \",
\"   .++.')!&@@*=~{].++.  \",
\"  .++.^{~>---)/(_:<.++. \",
\"   .++.^[,~/~'(_}|.++.  \",
\"    .++.]_1[12^:|.++.   \",
\"     .++.:}33:45.++.    \",
\"      .++.<5567.++.     \",
\"       .++.889.++.      \",
\"        .++.0.++.       \",
\"         .++.++.        \",
\"          .+++.         \",
\"           .+.          \",
\"            .           \",
\"                        \"};
"
                     'xpm t))
  "Image for the browse button.")

(defun newsticker-browse-url-item (feed item)
  "Convert FEED ITEM to html and call `browse-url' on result."
  (interactive)
  (let ((t-file (make-temp-file "newsticker")))
    (with-temp-file t-file
      (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
               <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
               <html xmlns=\"http://www.w3.org/1999/xhtml\">
               <body>")
      (insert "<h1>" feed ": " (newsticker--title item) "</h1>")
      (insert (format-time-string newsticker-date-format
                                  (newsticker--time item)))
      (insert "<br/>")
      (insert (or (newsticker--desc item) "[No Description]"))
      (when (newsticker--enclosure item)
        (insert "<br/><hr/><i>")
        (newsticker--insert-enclosure item nil)
        (insert "</i>"))
      (when (newsticker--extra item)
        (insert "<br/><hr/><tt>")
        (newsticker--print-extra-elements item nil)
        (insert "</tt>"))
      (insert "</body></html>"))
    (browse-url t-file)))

(provide 'newst-reader)

;; arch-tag: c604b701-bdf1-4fc1-8d05-5fabd1939533
;;; newst-reader.el ends here
