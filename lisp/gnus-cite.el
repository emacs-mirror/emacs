;;; gnus-cite.el --- parse citations in articles for Gnus

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Keywords: news, mail

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
(require 'gnus-ems)

(eval-and-compile
  (autoload 'gnus-article-add-button "gnus-vis")
  )

;;; Customization:

(defvar gnus-cite-parse-max-size 25000
  "Maximum article size (in bytes) where parsing citations is allowed.
Set it to nil to parse all articles.")

(defvar gnus-cite-prefix-regexp 
    "^[]>|:}+ ]*[]>|:}+]\\(.*>\\)?\\|^.*>"
  "Regexp matching the longest possible citation prefix on a line.")

(defvar gnus-cite-max-prefix 20
  "Maximal possible length for a citation prefix.")

(defvar gnus-supercite-regexp 
  (concat "^\\(" gnus-cite-prefix-regexp "\\)? *"
	  ">>>>> +\"\\([^\"\n]+\\)\" +==")
  "Regexp matching normal SuperCite attribution lines.
The first regexp group should match a prefix added by another package.")

(defvar gnus-supercite-secondary-regexp "^.*\"\\([^\"\n]+\\)\" +=="
  "Regexp matching mangled SuperCite attribution lines.
The first regexp group should match the SuperCite attribution.")

(defvar gnus-cite-minimum-match-count 2
  "Minimal number of identical prefix'es before we believe it is a citation.")

;see gnus-cus.el
;(defvar gnus-cite-face-list 
;  (if (eq gnus-display-type 'color)
;      (if (eq gnus-background-mode 'dark) 'light 'dark)
;    '(italic))
;  "Faces used for displaying different citations.
;It is either a list of face names, or one of the following special
;values:

;dark: Create faces from `gnus-face-dark-name-list'.
;light: Create faces from `gnus-face-light-name-list'.

;The variable `gnus-make-foreground' determines whether the created
;faces change the foreground or the background colors.")

(defvar gnus-cite-attribution-prefix "in article\\|in <"
  "Regexp matching the beginning of an attribution line.")

(defvar gnus-cite-attribution-postfix
  "\\(wrote\\|writes\\|said\\|says\\):[ \t]*$"
  "Regexp matching the end of an attribution line.
The text matching the first grouping will be used as a button.")

;see gnus-cus.el
;(defvar gnus-cite-attribution-face 'underline
;  "Face used for attribution lines.
;It is merged with the face for the cited text belonging to the attribution.")

;see gnus-cus.el
;(defvar gnus-cite-hide-percentage 50
;  "Only hide cited text if it is larger than this percent of the body.")

;see gnus-cus.el
;(defvar gnus-cite-hide-absolute 10
;  "Only hide cited text if there is at least this number of cited lines.")

;see gnus-cus.el
;(defvar gnus-face-light-name-list
;  '("light blue" "light cyan" "light yellow" "light pink"
;    "pale green" "beige" "orange" "magenta" "violet" "medium purple"
;    "turquoise")
;  "Names of light colors.")

;see gnus-cus.el
;(defvar gnus-face-dark-name-list
;  '("dark salmon" "firebrick"
;    "dark green" "dark orange" "dark khaki" "dark violet"
;    "dark turquoise")
;  "Names of dark colors.")

;;; Internal Variables:

(defvar gnus-article-length nil)
;; Length of article last time we parsed it.
;; BUG! KLUDGE! UGLY! FIX ME!

(defvar gnus-cite-prefix-alist nil)
;; Alist of citation prefixes.  
;; The cdr is a list of lines with that prefix.

(defvar gnus-cite-attribution-alist nil)
;; Alist of attribution lines.
;; The car is a line number.
;; The cdr is the prefix for the citation started by that line.

(defvar gnus-cite-loose-prefix-alist nil)
;; Alist of citation prefixes that have no matching attribution.
;; The cdr is a list of lines with that prefix.

(defvar gnus-cite-loose-attribution-alist nil)
;; Alist of attribution lines that have no matching citation.
;; Each member has the form (WROTE IN PREFIX TAG), where
;; WROTE: is the attribution line number
;; IN: is the line number of the previous line if part of the same attribution,
;; PREFIX: Is the citation prefix of the attribution line(s), and
;; TAG: Is a SuperCite tag, if any.

;;; Commands:

(defun gnus-article-highlight-citation (&optional force)
  "Highlight cited text.
Each citation in the article will be highlighted with a different face.
The faces are taken from `gnus-cite-face-list'.
Attribution lines are highlighted with the same face as the
corresponding citation merged with `gnus-cite-attribution-face'.

Text is considered cited if at least `gnus-cite-minimum-match-count'
lines matches `gnus-cite-prefix-regexp' with the same prefix.  

Lines matching `gnus-cite-attribution-postfix' and perhaps
`gnus-cite-attribution-prefix' are considered attribution lines."
  (interactive (list 'force))
  ;; Create dark or light faces if necessary.
  (cond ((eq gnus-cite-face-list 'light)
	 (setq gnus-cite-face-list
	       (mapcar 'gnus-make-face gnus-face-light-name-list)))
	((eq gnus-cite-face-list 'dark)
	 (setq gnus-cite-face-list
	       (mapcar 'gnus-make-face gnus-face-dark-name-list))))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (gnus-cite-parse-maybe force)
    (let ((buffer-read-only nil)
	  (alist gnus-cite-prefix-alist)
	  (faces gnus-cite-face-list)
	  (inhibit-point-motion-hooks t)
	  face entry prefix skip numbers number face-alist)
      ;; Loop through citation prefixes.
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      prefix (car entry)
	      numbers (cdr entry)
	      face (car faces)
	      faces (or (cdr faces) gnus-cite-face-list)
	      face-alist (cons (cons prefix face) face-alist))
	(while numbers
	  (setq number (car numbers)
		numbers (cdr numbers))
	  (and (not (assq number gnus-cite-attribution-alist))
	       (not (assq number gnus-cite-loose-attribution-alist))
	       (gnus-cite-add-face number prefix face))))
      ;; Loop through attribution lines.
      (setq alist gnus-cite-attribution-alist)
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      number (car entry)
	      prefix (cdr entry)
	      skip (gnus-cite-find-prefix number)
	      face (cdr (assoc prefix face-alist)))
	;; Add attribution button.
	(goto-line number)
	(if (re-search-forward gnus-cite-attribution-postfix 
			       (save-excursion (end-of-line 1) (point))
			       t)
	    (gnus-article-add-button (match-beginning 1) (match-end 1)
				     'gnus-cite-toggle prefix))
	;; Highlight attribution line.
	(gnus-cite-add-face number skip face)
	(gnus-cite-add-face number skip gnus-cite-attribution-face))
      ;; Loop through attribution lines.
      (setq alist gnus-cite-loose-attribution-alist)
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      number (car entry)
	      skip (gnus-cite-find-prefix number))
	(gnus-cite-add-face number skip gnus-cite-attribution-face)))))

(defun gnus-article-hide-citation (&optional force)
  "Hide all cited text except attribution lines.
See the documentation for `gnus-article-highlight-citation'."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (gnus-cite-parse-maybe force)
    (let ((buffer-read-only nil)
	  (alist gnus-cite-prefix-alist)
	  (inhibit-point-motion-hooks t)
	  numbers number)
      (while alist
	(setq numbers (cdr (car alist))
	      alist (cdr alist))
	(while numbers
	  (setq number (car numbers)
		numbers (cdr numbers))
	  (goto-line number)
	  (or (assq number gnus-cite-attribution-alist)
	      (add-text-properties (point) (progn (forward-line 1) (point))
				   gnus-hidden-properties)))))))

(defun gnus-article-hide-citation-maybe (&optional force)
  "Hide cited text that has an attribution line.
This will do nothing unless at least `gnus-cite-hide-percentage'
percent and at least `gnus-cite-hide-absolute' lines of the body is
cited text with attributions.  When called interactively, these two
variables are ignored.
See also the documentation for `gnus-article-highlight-citation'."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (gnus-cite-parse-maybe force)
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (let ((start (point))
	  (atts gnus-cite-attribution-alist)
	  (buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (hiden 0)
	  total)
      (goto-char (point-max))
      (re-search-backward gnus-signature-separator nil t)
      (setq total (count-lines start (point)))
      (while atts
	(setq hiden (+ hiden (length (cdr (assoc (cdr (car atts))
						 gnus-cite-prefix-alist))))
	      atts (cdr atts)))
      (if (or force
	      (and (> (* 100 hiden) (* gnus-cite-hide-percentage total))
		   (> hiden gnus-cite-hide-absolute)))
	  (progn
	    (setq atts gnus-cite-attribution-alist)
	    (while atts
	      (setq total (cdr (assoc (cdr (car atts)) gnus-cite-prefix-alist))
		    atts (cdr atts))
	      (while total
		(setq hiden (car total)
		      total (cdr total))
		(goto-line hiden)
		(or (assq hiden gnus-cite-attribution-alist)
		    (add-text-properties (point) 
					 (progn (forward-line 1) (point))
					 gnus-hidden-properties)))))))))

;;; Internal functions:

(defun gnus-cite-parse-maybe (&optional force)
  ;; Parse if the buffer has changes since last time.
  (if (eq gnus-article-length (- (point-max) (point-min)))
      ()
    ;;Reset parser information.
    (setq gnus-cite-prefix-alist nil
	  gnus-cite-attribution-alist nil
	  gnus-cite-loose-prefix-alist nil
	  gnus-cite-loose-attribution-alist nil)
    ;; Parse if not too large.
    (if (and (not force) 
	     gnus-cite-parse-max-size
	     (> (buffer-size) gnus-cite-parse-max-size))
	()
      (setq gnus-article-length (- (point-max) (point-min)))
      (gnus-cite-parse))))

(defun gnus-cite-parse ()
  ;; Parse and connect citation prefixes and attribution lines.
  
  ;; Parse current buffer searching for citation prefixes.
  (goto-char (point-min))
  (or (search-forward "\n\n" nil t)
      (goto-char (point-max)))
  (let ((line (1+ (count-lines (point-min) (point))))
	(case-fold-search t)
	(max (save-excursion
	       (goto-char (point-max))
	       (re-search-backward gnus-signature-separator nil t)
	       (point)))
	alist entry start begin end numbers prefix)
    ;; Get all potential prefixes in `alist'.
    (while (< (point) max)
      ;; Each line.
      (setq begin (point)
	    end (progn (beginning-of-line 2) (point))
	    start end)
      (goto-char begin)
      ;; Ignore standard SuperCite attribution prefix.
      (if (looking-at gnus-supercite-regexp)
	  (if (match-end 1)
	      (setq end (1+ (match-end 1)))
	    (setq end (1+ begin))))
      ;; Ignore very long prefixes.
      (if (> end (+ (point) gnus-cite-max-prefix))
	  (setq end (+ (point) gnus-cite-max-prefix)))
      (while (re-search-forward gnus-cite-prefix-regexp (1- end) t)
	;; Each prefix.
	(setq end (match-end 0)
	      prefix (buffer-substring begin end))
	(set-text-properties 0 (length prefix) nil prefix)
	(setq entry (assoc prefix alist))
	(if entry 
	    (setcdr entry (cons line (cdr entry)))
	  (setq alist (cons (list prefix line) alist)))
	(goto-char begin))
      (goto-char start)
      (setq line (1+ line)))
    ;; We got all the potential prefixes.  Now create
    ;; `gnus-cite-prefix-alist' containing the oldest prefix for each
    ;; line that appears at least gnus-cite-minimum-match-count
    ;; times. First sort them by length.  Longer is older.
    (setq alist (sort alist (lambda (a b)
			      (> (length (car a)) (length (car b))))))
    (while alist
      (setq entry (car alist)
	    prefix (car entry)
	    numbers (cdr entry)
	    alist (cdr alist))
      (cond ((null numbers)
	     ;; No lines with this prefix that wasn't also part of
	     ;; a longer prefix.
	     )
	    ((< (length numbers) gnus-cite-minimum-match-count)
	     ;; Too few lines with this prefix.  We keep it a bit
	     ;; longer in case it is an exact match for an attribution
	     ;; line, but we don't remove the line from other
	     ;; prefixes. 
	     (setq gnus-cite-prefix-alist
		   (cons entry gnus-cite-prefix-alist)))
	    (t
	     (setq gnus-cite-prefix-alist (cons entry
						gnus-cite-prefix-alist))
	     ;; Remove articles from other prefixes.
	     (let ((loop alist)
		   current)
	       (while loop
		 (setq current (car loop)
		       loop (cdr loop))
		 (setcdr current 
			 (gnus-set-difference (cdr current) numbers))))))))
  ;; No citations have been connected to attribution lines yet.
  (setq gnus-cite-loose-prefix-alist (append gnus-cite-prefix-alist nil))

  ;; Parse current buffer searching for attribution lines.
  (goto-char (point-min))
  (search-forward "\n\n" nil t)
  (while (re-search-forward gnus-cite-attribution-postfix (point-max) t)
    (let* ((start (match-beginning 0))
	   (end (match-end 0))
	   (wrote (count-lines (point-min) end))
	   (prefix (gnus-cite-find-prefix wrote))
	   ;; Check previous line for an attribution leader.
	   (tag (progn
		  (beginning-of-line 1)
		  (and (looking-at gnus-supercite-secondary-regexp)
		       (buffer-substring (match-beginning 1)
					 (match-end 1)))))
	   (in (progn
		 (goto-char start)
		 (and (re-search-backward gnus-cite-attribution-prefix
					  (save-excursion
					    (beginning-of-line 0)
					    (point))
					  t)
		      (not (re-search-forward gnus-cite-attribution-postfix
					      start t))
		      (count-lines (point-min) (1+ (point)))))))
      (if (eq wrote in)
	  (setq in nil))
      (goto-char end)
      (setq gnus-cite-loose-attribution-alist
	    (cons (list wrote in prefix tag)
		  gnus-cite-loose-attribution-alist))))
  ;; Find exact supercite citations.
  (gnus-cite-match-attributions 'small nil
				(lambda (prefix tag)
				  (if tag
				      (concat "\\`" 
					      (regexp-quote prefix) "[ \t]*" 
					      (regexp-quote tag) ">"))))
  ;; Find loose supercite citations after attributions.
  (gnus-cite-match-attributions 'small t
				(lambda (prefix tag)
				  (if tag (concat "\\<"
						  (regexp-quote tag)
						  "\\>"))))
  ;; Find loose supercite citations anywhere.
  (gnus-cite-match-attributions 'small nil
				(lambda (prefix tag)
				  (if tag (concat "\\<"
						  (regexp-quote tag)
						  "\\>"))))
  ;; Find nested citations after attributions.
  (gnus-cite-match-attributions 'small-if-unique t
				(lambda (prefix tag)
				  (concat "\\`" (regexp-quote prefix) ".+")))
  ;; Find nested citations anywhere.
  (gnus-cite-match-attributions 'small nil
				(lambda (prefix tag)
				  (concat "\\`" (regexp-quote prefix) ".+")))
  ;; Remove loose prefixes with too few lines.
  (let ((alist gnus-cite-loose-prefix-alist)
	entry)
    (while alist
      (setq entry (car alist)
	    alist (cdr alist))
      (if (< (length (cdr entry)) gnus-cite-minimum-match-count)
	  (setq gnus-cite-prefix-alist
		(delq entry gnus-cite-prefix-alist)
		gnus-cite-loose-prefix-alist
		(delq entry gnus-cite-loose-prefix-alist)))))
  ;; Find flat attributions.
  (gnus-cite-match-attributions 'first t nil)
  ;; Find any attributions (are we getting desperate yet?).
  (gnus-cite-match-attributions 'first nil nil))

(defun gnus-cite-match-attributions (sort after fun)
  ;; Match all loose attributions and citations (SORT AFTER FUN) .
  ;;
  ;; If SORT is `small', the citation with the shortest prefix will be
  ;; used, if it is `first' the first prefix will be used, if it is
  ;; `small-if-unique' the shortest prefix will be used if the
  ;; attribution line does not share its own prefix with other
  ;; loose attribution lines, otherwise the first prefix will be used.
  ;;
  ;; If AFTER is non-nil, only citations after the attribution line
  ;; will be considered.
  ;;
  ;; If FUN is non-nil, it will be called with the arguments (WROTE
  ;; PREFIX TAG) and expected to return a regular expression.  Only
  ;; citations whose prefix matches the regular expression will be
  ;; considered. 
  ;; 
  ;; WROTE is the attribution line number.
  ;; PREFIX is the attribution line prefix.
  ;; TAG is the SuperCite tag on the attribution line.
  (let ((atts gnus-cite-loose-attribution-alist)
	(case-fold-search t)
	att wrote in prefix tag regexp limit smallest best size)
    (while atts
      (setq att (car atts)
	    atts (cdr atts)
	    wrote (nth 0 att)
	    in (nth 1 att)
	    prefix (nth 2 att)
	    tag (nth 3 att)
	    regexp (if fun (funcall fun prefix tag) "")
	    size (cond ((eq sort 'small) t)
		       ((eq sort 'first) nil)
		       (t (< (length (gnus-cite-find-loose prefix)) 2)))
	    limit (if after wrote -1)
	    smallest 1000000		       
	    best nil)
      (let ((cites gnus-cite-loose-prefix-alist)
	    cite candidate numbers first compare)
	(while cites
	  (setq cite (car cites)
		cites (cdr cites)
		candidate (car cite)
		numbers (cdr cite)
		first (apply 'min numbers)
		compare (if size (length candidate) first))
	  (and (> first limit)
	       regexp
	       (string-match regexp candidate)
	       (< compare smallest)
	       (setq best cite
		     smallest compare))))
      (if (null best)
	  ()
	(setq gnus-cite-loose-attribution-alist
	      (delq att gnus-cite-loose-attribution-alist))
	(setq gnus-cite-attribution-alist 
	      (cons (cons wrote (car best)) gnus-cite-attribution-alist))
	(if in
	    (setq gnus-cite-attribution-alist 
		  (cons (cons in (car best)) gnus-cite-attribution-alist)))
	(if (memq best gnus-cite-loose-prefix-alist)
	    (let ((loop gnus-cite-prefix-alist)
		  (numbers (cdr best))
		  current)
	      (setq gnus-cite-loose-prefix-alist
		    (delq best gnus-cite-loose-prefix-alist))
	      (while loop
		(setq current (car loop)
		      loop (cdr loop))
		(if (eq current best)
		    ()
		  (setcdr current (gnus-set-difference (cdr current) numbers))
		  (if (null (cdr current))
		      (setq gnus-cite-loose-prefix-alist
			    (delq current gnus-cite-loose-prefix-alist)
			    atts (delq current atts)))))))))))

(defun gnus-cite-find-loose (prefix)
  ;; Return a list of loose attribution lines prefixed by PREFIX.
  (let* ((atts gnus-cite-loose-attribution-alist)
	 att line lines)
    (while atts
      (setq att (car atts)
	    line (car att)
	    atts (cdr atts))
      (if (string-equal (gnus-cite-find-prefix line) prefix)
	  (setq lines (cons line lines))))
    lines))

(defun gnus-cite-add-face (number prefix face)
  ;; At line NUMBER, ignore PREFIX and add FACE to the rest of the line.
  (if face
      (let ((inhibit-point-motion-hooks t)
	    from to)
	(goto-line number)
	(forward-char (length prefix))
	(skip-chars-forward " \t")
	(setq from (point))
	(end-of-line 1)
	(skip-chars-backward " \t")
	(setq to (point))
	(if (< from to)
	    (gnus-overlay-put (gnus-make-overlay from to) 'face face)))))

(defun gnus-cite-toggle (prefix)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (numbers (cdr (assoc prefix gnus-cite-prefix-alist)))
	  (inhibit-point-motion-hooks t)
	  number)
      (while numbers
	(setq number (car numbers)
	      numbers (cdr numbers))
	(goto-line number)
	(cond ((get-text-property (point) 'invisible)
	       (remove-text-properties (point) (progn (forward-line 1) (point))
				       gnus-hidden-properties))
	      ((assq number gnus-cite-attribution-alist))
	      (t
	       (add-text-properties (point) (progn (forward-line 1) (point))
				    gnus-hidden-properties)))))))

(defun gnus-cite-find-prefix (line)
  ;; Return citation prefix for LINE.
  (let ((alist gnus-cite-prefix-alist)
	(prefix "")
	entry)
    (while alist
      (setq entry (car alist)
	    alist (cdr alist))
      (if (memq line (cdr entry))
	  (setq prefix (car entry))))
    prefix))

(gnus-ems-redefine)

(provide 'gnus-cite)

;;; gnus-cite.el ends here
