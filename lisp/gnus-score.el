;;; gnus-score.el --- scoring code for Gnus
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <amanda@iesd.auc.dk>
;;	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'gnus)

(defvar gnus-score-expiry-days 7
  "*Number of days before unused score file entries are expired.")

(defvar gnus-orphan-score nil
  "*All orphans get this score added. Set in the score file.")

(defvar gnus-default-adaptive-score-alist  
  '((gnus-kill-file-mark)
    (gnus-unread-mark)
    (gnus-read-mark (from  3) (subject  30))
    (gnus-catchup-mark (subject -10))
    (gnus-killed-mark (from -1) (subject -20))
    (gnus-del-mark (from -2) (subject -15)))
"*Alist of marks and scores.")

(defvar gnus-score-mimic-keymap nil
  "*Have the score entry functions pretend that they are a keymap.")

(defvar gnus-score-exact-adapt-limit 10
  "*Number that says how long a match has to be before using substring matching.
When doing adaptive scoring, one normally uses fuzzy or substring
matching. However, if the header one matches is short, the possibility
for false positives is great, so if the length of the match is less
than this variable, exact matching will be used.

If this variable is nil, exact matching will always be used.")



;; Internal variables.

(defvar gnus-score-help-winconf nil)
(defvar gnus-adaptive-score-alist gnus-default-adaptive-score-alist)
(defvar gnus-score-trace nil)
(defvar gnus-score-edit-buffer nil)

(defvar gnus-score-alist nil
  "Alist containing score information.
The keys can be symbols or strings.  The following symbols are defined. 

touched: If this alist has been modified.
mark:    Automatically mark articles below this.
expunge: Automatically expunge articles below this.
files:   List of other score files to load when loading this one.
eval:    Sexp to be evaluated when the score file is loaded.

String entries have the form (HEADER (MATCH TYPE SCORE DATE) ...) 
where HEADER is the header being scored, MATCH is the string we are
looking for, TYPE is a flag indicating whether it should use regexp or
substring matching, SCORE is the score to add and DATE is the date
of the last successful match.")

(defvar gnus-score-cache nil)
(defvar gnus-scores-articles nil)
(defvar gnus-header-index nil)
(defvar gnus-score-index nil)

(eval-and-compile
  (autoload 'gnus-uu-ctl-map "gnus-uu" nil nil 'keymap)
  (autoload 'appt-select-lowest-window "appt.el"))

;;; Summary mode score maps.

(defvar gnus-summary-score-map nil)

(define-prefix-command 'gnus-summary-score-map)
(define-key gnus-summary-mode-map "V" 'gnus-summary-score-map)
(define-key gnus-summary-score-map "s" 'gnus-summary-set-score)
(define-key gnus-summary-score-map "a" 'gnus-summary-score-entry)
(define-key gnus-summary-score-map "S" 'gnus-summary-current-score)
(define-key gnus-summary-score-map "c" 'gnus-score-change-score-file)
(define-key gnus-summary-score-map "m" 'gnus-score-set-mark-below)
(define-key gnus-summary-score-map "x" 'gnus-score-set-expunge-below)
(define-key gnus-summary-score-map "e" 'gnus-score-edit-alist)
(define-key gnus-summary-score-map "f" 'gnus-score-edit-file)
(define-key gnus-summary-score-map "t" 'gnus-score-find-trace)
(define-key gnus-summary-score-map "C" 'gnus-score-customize)



;; Summary score file commands

;; Much modification of the kill (ahem, score) code and lots of the
;; functions are written by Per Abrahamsen <amanda@iesd.auc.dk>.

(defun gnus-summary-lower-score (&optional score)
  "Make a score entry based on the current article.
The user will be prompted for header to score on, match type,
permanence, and the string to be used.  The numerical prefix will be
used as score."
  (interactive "P")
  (gnus-summary-increase-score (- (gnus-score-default score))))

(defun gnus-summary-increase-score (&optional score)
  "Make a score entry based on the current article.
The user will be prompted for header to score on, match type,
permanence, and the string to be used.  The numerical prefix will be
used as score."
  (interactive "P")
  (gnus-set-global-variables)
  (let* ((nscore (gnus-score-default score))
	 (prefix (if (< nscore 0) ?L ?I))
	 (increase (> nscore 0))
	 (char-to-header 
	  '((?a "from" nil nil string)
	    (?s "subject" nil nil string)
	    (?b "body" "" nil body-string)
	    (?h "head" "" nil body-string)
	    (?i "message-id" nil t string)
	    (?t "references" "message-id" t string)
	    (?x "xref" nil nil string)
	    (?l "lines" nil nil number)
	    (?d "date" nil nil date)
	    (?f "followup" nil nil string)))
	 (char-to-type
	  '((?s s "substring" string)
	    (?e e "exact string" string)
	    (?f f "fuzzy string" string)
	    (?r r "regexp string" string)
	    (?s s "substring" body-string)
	    (?r s "regexp string" body-string)
	    (?b before "before date" date)
	    (?a at "at date" date) 
	    (?n now "this date" date)
	    (?< < "less than number" number)
	    (?> > "greater than number" number) 
	    (?= = "equal to number" number)))
	 (char-to-perm
	  (list (list ?t (current-time-string) "temporary") 
		'(?p perm "permanent") '(?i now "immediate")))
	 (mimic gnus-score-mimic-keymap)
	 hchar entry temporary tchar pchar end type)
    ;; First we read the header to score.
    (while (not hchar)
      (if mimic
	  (progn 
	    (sit-for 1)
	    (message "%c-" prefix))
	(message "%s header (%s?): " (if increase "Increase" "Lower")
		 (mapconcat (lambda (s) (char-to-string (car s)))
			    char-to-header "")))
      (setq hchar (read-char))
      (if (not (or (= hchar ??) (= hchar ?\C-h)))
	  ()
	(setq hchar nil)
	(gnus-score-insert-help "Match on header" char-to-header 1)))

    (and (get-buffer "*Score Help*")
	 (progn
	   (kill-buffer "*Score Help*")
	   (and gnus-score-help-winconf
		(set-window-configuration gnus-score-help-winconf))))

    (or (setq entry (assq (downcase hchar) char-to-header))
	(progn
	  (ding)
	  (setq end t)
	  (if mimic (message "%c %c" prefix hchar) (message ""))))
    (if (or end (/= (downcase hchar) hchar))
	(progn
	  ;; This was a majuscle, so we end reading and set the defaults.
	  (if mimic (message "%c %c" prefix hchar) (message ""))
	  (setq type nil
		temporary (current-time-string)))

      ;; We continue reading - the type.
      (while (not tchar)
	(if mimic
	    (progn
	      (sit-for 1)
	      (message "%c %c-" prefix hchar))
	  (message "%s header '%s' with match type (%s?): "
		   (if increase "Increase" "Lower")
		   (nth 1 entry)
		   (mapconcat (lambda (s) 
				(if (eq (nth 4 entry) 
					(nth 3 s))
				    (char-to-string (car s))
				  ""))
			      char-to-type "")))
	(setq tchar (read-char))
	(if (not (or (= tchar ??) (= tchar ?\C-h)))
	    ()
	  (setq tchar nil)
	  (gnus-score-insert-help "Match type" char-to-type 2)))

      (and (get-buffer "*Score Help*")
	   (progn
	     (and gnus-score-help-winconf
		  (set-window-configuration gnus-score-help-winconf))
	     (kill-buffer "*Score Help*")))
      
      (or (setq type (nth 1 (assq (downcase tchar) char-to-type)))
	  (progn
	    (ding)
	    (if mimic (message "%c %c" prefix hchar) (message ""))
	    (setq end t)))
      (if (or end (/= (downcase tchar) tchar))
	  (progn
	    ;; It was a majuscle, so we end reading and the the default.
	    (if mimic (message "%c %c %c" prefix hchar tchar)
	      (message ""))
	    (setq temporary (current-time-string)))

	;; We continue reading.
	(while (not pchar)
	  (if mimic
	      (progn
		(sit-for 1)
		(message "%c %c %c-" prefix hchar tchar))
	    (message "%s permanence (%s?): " (if increase "Increase" "Lower")
		     (mapconcat (lambda (s) (char-to-string (car s)))
				char-to-perm "")))
	  (setq pchar (read-char))
	  (if (not (or (= pchar ??) (= pchar ?\C-h)))
	      ()
	    (setq pchar nil)
	    (gnus-score-insert-help "Match permanence" char-to-perm 2)))

	(and (get-buffer "*Score Help*")
	     (progn
	       (and gnus-score-help-winconf
		    (set-window-configuration gnus-score-help-winconf))
	       (kill-buffer "*Score Help*")))

	(if mimic (message "%c %c %c" prefix hchar tchar pchar)
	  (message ""))
	(if (setq temporary (nth 1 (assq pchar char-to-perm)))
	    ()
	  (ding)
	  (setq end t)
	  (if mimic 
	      (message "%c %c %c %c" prefix hchar tchar pchar)
	    (message "")))))

    ;; We have all the data, so we enter this score.
    (if end
	()
      (gnus-summary-score-entry
       (nth 1 entry)			; Header
       (if (string= (nth 2 entry) "") ""
	 (gnus-summary-header (or (nth 2 entry) (nth 1 entry)))) ; Match
       type				; Type
       (if (eq 's score) nil score)     ; Score
       (if (eq 'perm temporary)         ; Temp
           nil
         temporary)
       (not (nth 3 entry)))		; Prompt
      )))
  
(defun gnus-score-insert-help (string alist idx)
  (setq gnus-score-help-winconf (current-window-configuration))
  (save-excursion
    (set-buffer (get-buffer-create "*Score Help*"))
    (buffer-disable-undo (current-buffer))
    (delete-windows-on (current-buffer))
    (erase-buffer)
    (insert string ":\n\n")
    (let ((max -1)
	  (list alist)
	  (i 0)
	  n width pad format)
      ;; find the longest string to display
      (while list
	(setq n (length (nth idx (car list))))
	(or (> max n)
	    (setq max n))
	(setq list (cdr list)))
      (setq max (+ max 4))		; %c, `:', SPACE, a SPACE at end
      (setq n (/ (window-width) max))	; items per line
      (setq width (/ (window-width) n)) ; width of each item
      ;; insert `n' items, each in a field of width `width' 
      (while alist
	(if (< i n)
	    ()
	  (setq i 0)
	  (delete-char -1)		; the `\n' takes a char
	  (insert "\n"))
	(setq pad (- width 3))
	(setq format (concat "%c: %-" (int-to-string pad) "s"))
	(insert (format format (car (car alist)) (nth idx (car alist))))
	(setq alist (cdr alist))
	(setq i (1+ i))))
    ;; display ourselves in a small window at the bottom
    (appt-select-lowest-window)
    (split-window)
    (pop-to-buffer "*Score Help*")
    (shrink-window-if-larger-than-buffer)
    (select-window (get-buffer-window gnus-summary-buffer))))
  
(defun gnus-summary-header (header &optional no-err)
  ;; Return HEADER for current articles, or error.
  (let ((article (gnus-summary-article-number))
	headers)
    (if article
	(if (and (setq headers (gnus-get-header-by-number article))
		 (vectorp headers))
	    (aref headers (nth 1 (assoc header gnus-header-index)))
	  (if no-err
	      nil
	    (error "Pseudo-articles can't be scored")))
      (if no-err
	  (error "No article on current line")
	nil))))

(defun gnus-summary-score-entry 
  (header match type score date &optional prompt silent)
  "Enter score file entry.
HEADER is the header being scored.
MATCH is the string we are looking for.
TYPE is the match type: substring, regexp, exact, fuzzy.
SCORE is the score to add.
DATE is the expire date, or nil for no expire, or 'now for immediate expire.
If optional argument `PROMPT' is non-nil, allow user to edit match.
If optional argument `SILENT' is nil, show effect of score entry."
  (interactive
   (list (completing-read "Header: "
			  gnus-header-index
			  (lambda (x) (fboundp (nth 2 x)))
			  t)
	 (read-string "Match: ")
	 (if (y-or-n-p "Use regexp match? ") 'r 's)
	 (and current-prefix-arg
	      (prefix-numeric-value current-prefix-arg))
	 (cond ((not (y-or-n-p "Add to score file? "))
		'now)
	       ((y-or-n-p "Expire kill? ")
		(current-time-string))
	       (t nil))))
  ;; Regexp is the default type.
  (if (eq type t) (setq type 'r))
  ;; Simplify matches...
  (cond ((or (eq type 'r) (eq type 's) (eq type nil))
	 (setq match (if match (gnus-simplify-subject-re match) "")))
	((eq type 'f)
	 (setq match (gnus-simplify-subject-fuzzy match))))
  (let ((score (gnus-score-default score))
	(header (downcase header)))
    (and prompt (setq match (read-string 
			     (format "Match %s on %s, %s: " 
				     (cond ((eq date 'now)
					    "now")
					   ((stringp date)
					    "temp")
					   (t "permanent"))
				     header
				     (if (< score 0) "lower" "raise"))
			     (if (numberp match)
				 (int-to-string match)
			       match))))
    (and (>= (nth 1 (assoc header gnus-header-index)) 0)
	 (eq (nth 2 (assoc header gnus-header-index)) 'gnus-score-string)
	 (not silent)
	 (gnus-summary-score-effect header match type score))

    ;; If this is an integer comparison, we transform from string to int. 
    (and (eq (nth 2 (assoc header gnus-header-index)) 'gnus-score-integer)
	 (setq match (string-to-int match)))

    (if (eq date 'now)
	()
      (and (= score gnus-score-interactive-default-score)
	   (setq score nil))
      (let ((new (cond 
		  (type
		   (list match score (and date (gnus-day-number date)) type))
		  (date
		   (list match score (gnus-day-number date)))
		  (score
		   (list match score))
		  (t
		   (list match))))
	    (old (gnus-score-get header))
	    elem)
	;; We see whether we can collapse some score entries.
	;; This isn't quite correct, because there may be more elements
	;; later on with the same key that have matching elems... Hm.
	(if (and old
		 (setq elem (assoc match old))
		 (eq (nth 3 elem) (nth 3 new))
		 (or (and (numberp (nth 2 elem)) (numberp (nth 2 new)))
		     (and (not (nth 2 elem)) (not (nth 2 new)))))
	    ;; Yup, we just add this new score to the old elem.
	    (setcar (cdr elem) (+ (or (nth 1 elem) 
				      gnus-score-interactive-default-score)
				  (or (nth 1 new)
				      gnus-score-interactive-default-score)))
	  ;; Nope, we have to add a new elem.
	  (gnus-score-set header (if old (cons new old) (list new)))))
      (gnus-score-set 'touched '(t)))))

(defun gnus-summary-score-effect (header match type score)
  "Simulate the effect of a score file entry.
HEADER is the header being scored.
MATCH is the string we are looking for.
TYPE is a flag indicating if it is a regexp or substring.
SCORE is the score to add."
  (interactive (list (completing-read "Header: "
				      gnus-header-index
				      (lambda (x) (fboundp (nth 2 x)))
				      t)
		     (read-string "Match: ")
		     (y-or-n-p "Use regexp match? ")
		     (prefix-numeric-value current-prefix-arg)))
  (save-excursion
    (or (and (stringp match) (> (length match) 0))
	(error "No match"))
    (goto-char (point-min))
    (let ((regexp (cond ((eq type 'f)
			 (gnus-simplify-subject-fuzzy match))
			(type match)
			(t (concat "\\`.*" (regexp-quote match) ".*\\'")))))
      (while (not (eobp))
	(let ((content (gnus-summary-header header 'noerr))
	      (case-fold-search t))
	  (and content
	       (if (if (eq type 'f)
		       (string-equal (gnus-simplify-subject-fuzzy content)
				     regexp)
		     (string-match regexp content))
		   (gnus-summary-raise-score score))))
	(beginning-of-line 2)))))

(defun gnus-summary-score-crossposting (score date)
  ;; Enter score file entry for current crossposting.
  ;; SCORE is the score to add.
  ;; DATE is the expire date.
  (let ((xref (gnus-summary-header "xref"))
	(start 0)
	group)
    (or xref (error "This article is not crossposted"))
    (while (string-match " \\([^ \t]+\\):" xref start)
      (setq start (match-end 0))
      (if (not (string= 
		(setq group 
		      (substring xref (match-beginning 1) (match-end 1)))
		gnus-newsgroup-name))
	  (gnus-summary-score-entry
	   "xref" (concat " " group ":") nil score date t)))))


;;;
;;; Gnus Score Files
;;;

;; All score code written by Per Abrahamsen <abraham@iesd.auc.dk>.

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-score-set-mark-below (score)
  "Automatically mark articles with score below SCORE as read."
  (interactive 
   (list (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
	     (string-to-int (read-string "Mark below: ")))))
  (setq score (or score gnus-summary-default-score 0))
  (gnus-score-set 'mark (list score))
  (gnus-score-set 'touched '(t))
  (setq gnus-summary-mark-below score)
  (gnus-summary-update-lines))

(defun gnus-score-set-expunge-below (score)
  "Automatically expunge articles with score below SCORE."
  (interactive 
   (list (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
	     (string-to-int (read-string "Expunge below: ")))))
  (setq score (or score gnus-summary-default-score 0))
  (gnus-score-set 'expunge (list score))
  (gnus-score-set 'touched '(t)))

(defun gnus-score-set (symbol value &optional alist)
  ;; Set SYMBOL to VALUE in ALIST.
  (let* ((alist 
	  (or alist 
	      gnus-score-alist
	      (progn
		(gnus-score-load (gnus-score-file-name gnus-newsgroup-name))
		gnus-score-alist)))
	 (entry (assoc symbol alist)))
    (cond ((gnus-score-get 'read-only alist)
	   ;; This is a read-only score file, so we do nothing.
	   )
	  (entry
	   (setcdr entry value))
	  ((null alist)
	   (error "Empty alist"))
	  (t
	   (setcdr alist
		   (cons (cons symbol value) (cdr alist)))))))

(defun gnus-score-get (symbol &optional alist)
  ;; Get SYMBOL's definition in ALIST.
  (cdr (assoc symbol 
	      (or alist 
		  gnus-score-alist
		  (progn
		    (gnus-score-load 
		     (gnus-score-file-name gnus-newsgroup-name))
		    gnus-score-alist)))))

(defun gnus-score-change-score-file (file)
  "Change current score alist."
  (interactive 
   (list (read-file-name "Edit score file: " gnus-kill-files-directory)))
  (gnus-score-load-file file)
  (gnus-set-mode-line 'summary))

(defun gnus-score-edit-alist (file)
  "Edit the current score alist."
  (interactive (list gnus-current-score-file))
  (let ((winconf (current-window-configuration)))
    (and (buffer-name gnus-summary-buffer) (gnus-score-save))
    (setq gnus-score-edit-buffer (find-file-noselect file))
    (gnus-configure-windows 'edit-score)
    (gnus-score-mode)
    (make-local-variable 'gnus-prev-winconf)
    (setq gnus-prev-winconf winconf))
  (gnus-message 
   4 (substitute-command-keys 
      "\\<gnus-score-mode-map>\\[gnus-score-edit-done] to save edits")))
  
(defun gnus-score-edit-file (file)
  "Edit a score file."
  (interactive 
   (list (read-file-name "Edit score file: " gnus-kill-files-directory)))
  (and (buffer-name gnus-summary-buffer) (gnus-score-save))
  (let ((winconf (current-window-configuration)))
    (setq gnus-score-edit-buffer (find-file-noselect file))
    (gnus-configure-windows 'edit-score)
    (gnus-score-mode)
    (make-local-variable 'gnus-prev-winconf)
    (setq gnus-prev-winconf winconf))
  (gnus-message 
   4 (substitute-command-keys 
      "\\<gnus-score-mode-map>\\[gnus-score-edit-done] to save edits")))
  
(defun gnus-score-load-file (file)
  ;; Load score file FILE.  Returns a list a retrieved score-alists.
  (setq gnus-kill-files-directory (or gnus-kill-files-directory "~/News/"))
  (let* ((file (expand-file-name 
		(or (and (string-match
			  (concat "^" (expand-file-name
				       gnus-kill-files-directory)) 
			  (expand-file-name file))
			 file)
		    (concat gnus-kill-files-directory file))))
	 (cached (assoc file gnus-score-cache))
	 (global (member file gnus-internal-global-score-files))
	 lists alist)
    (if cached
	;; The score file was already loaded.
	(setq alist (cdr cached))
      ;; We load the score file.
      (setq gnus-score-alist nil)
      (setq alist (gnus-score-load-score-alist file))
      ;; We add '(touched) to the alist to signify that it hasn't been
      ;; touched (yet). 
      (or (assq 'touched alist) (setq alist (cons (list 'touched nil) alist)))
      ;; If it is a global score file, we make it read-only.
      (and global
	   (not (assq 'read-only alist))
	   (setq alist (cons (list 'read-only t) alist)))
      ;; Update cache.
      (setq gnus-score-cache
	    (cons (cons file alist) gnus-score-cache)))
    ;; If there are actual scores in the alist, we add it to the
    ;; return value of this function.
    (if (memq t (mapcar (lambda (e) (stringp (car e))) alist))
	(setq lists (list alist)))
    ;; Treat the other possible atoms in the score alist.
    (let ((mark (car (gnus-score-get 'mark alist)))
	  (expunge (car (gnus-score-get 'expunge alist)))
	  (mark-and-expunge (car (gnus-score-get 'mark-and-expunge alist)))
	  (files (gnus-score-get 'files alist))
	  (exclude-files (gnus-score-get 'exclude-files alist))
          (orphan (car (gnus-score-get 'orphan alist)))
	  (adapt (gnus-score-get 'adapt alist))
	  (local (gnus-score-get 'local alist))
	  (eval (car (gnus-score-get 'eval alist))))
      ;; We do not respect eval and files atoms from global score
      ;; files. 
      (and files (not global)
	   (setq lists (apply 'append lists
			      (mapcar (lambda (file)
					(gnus-score-load-file file)) 
				      files))))
      (and eval (not global) (eval eval))
      ;; We then expand any exclude-file directives.
      (setq gnus-scores-exclude-files 
	    (nconc 
	     (mapcar 
	      (lambda (sfile) 
		(expand-file-name sfile (file-name-directory file)))
	      exclude-files) gnus-scores-exclude-files))
      (if (not local)
	  ()
	(save-excursion
	  (set-buffer gnus-summary-buffer)
	  (while local
	    (and (consp (car local))
		 (symbolp (car (car local)))
		 (progn
		   (make-local-variable (car (car local)))
		   (set (car (car local)) (nth 1 (car local)))))
	    (setq local (cdr local)))))
      (if orphan (setq gnus-orphan-score orphan))
      (setq gnus-adaptive-score-alist
	    (cond ((equal adapt '(t))
		   (setq gnus-newsgroup-adaptive t)
		   gnus-default-adaptive-score-alist)
		  ((equal adapt '(ignore))
		   (setq gnus-newsgroup-adaptive nil))
		  ((consp adapt)
		   (setq gnus-newsgroup-adaptive t)
		   adapt)
		  (t
		   ;;(setq gnus-newsgroup-adaptive gnus-use-adaptive-scoring)
		   gnus-default-adaptive-score-alist)))
      (setq gnus-summary-mark-below 
	    (or mark mark-and-expunge gnus-summary-mark-below))
      (setq gnus-summary-expunge-below 
	    (or expunge mark-and-expunge gnus-summary-expunge-below)))
    (setq gnus-current-score-file file)
    (setq gnus-score-alist alist)
    lists))

(defun gnus-score-load (file)
  ;; Load score FILE.
  (let ((cache (assoc file gnus-score-cache)))
    (if cache
	(setq gnus-score-alist (cdr cache))
      (setq gnus-score-alist nil)
      (gnus-score-load-score-alist file)
      (or gnus-score-alist
	  (setq gnus-score-alist (copy-alist '((touched nil)))))
      (setq gnus-score-cache
	    (cons (cons file gnus-score-alist) gnus-score-cache)))))

(defun gnus-score-remove-from-cache (file)
  (setq gnus-score-cache 
	(delq (assoc file gnus-score-cache) gnus-score-cache)))

(defun gnus-score-load-score-alist (file)
  (let (alist)
    (if (file-readable-p file)
	(progn
	  (save-excursion
	    (gnus-set-work-buffer)
	    (insert-file-contents file)
	    (goto-char (point-min))
	    ;; Only do the loading if the score file isn't empty.
	    (if (save-excursion (re-search-forward "[()0-9a-zA-Z]" nil t))
		(setq alist
		      (condition-case ()
			  (read (current-buffer))
			(error 
			 (progn
			   (gnus-message 3 "Problem with score file %s" file)
			   (ding) 
			   (sit-for 2)
			   nil))))))
	  (if (eq (car alist) 'setq)
	      (setq gnus-score-alist (gnus-score-transform-old-to-new alist))
	    (setq gnus-score-alist alist))
	  (setq gnus-score-alist
		(gnus-score-check-syntax gnus-score-alist file)))
      (setq gnus-score-alist nil))))

(defun gnus-score-check-syntax (alist file)
  (cond 
   ((null alist)
    nil)
   ((not (consp alist))
    (gnus-message 1 "Score file is not a list: %s" file)
    (ding)
    nil)
   (t
    (let ((a alist)
	  err)
      (while (and a (not err))
	(cond ((not (listp (car a)))
	       (gnus-message 3 "Illegal score element %s in %s" (car a) file)
	       (setq err t))
	      ((and (stringp (car (car a)))
		    (not (listp (nth 1 (car a)))))
	       (gnus-message 3 "Illegal header match %s in %s" (nth 1 (car a)) file)
	       (setq err t))
	      (t
	       (setq a (cdr a)))))
      (if err
	  (progn
	    (ding)
	    nil)
	alist)))))    

(defun gnus-score-transform-old-to-new (alist)
  (let* ((alist (nth 2 alist))
	 out entry)
    (if (eq (car alist) 'quote)
	(setq alist (nth 1 alist)))
    (while alist
      (setq entry (car alist))
      (if (stringp (car entry))
	  (let ((scor (cdr entry)))
	    (setq out (cons entry out))
	    (while scor
	      (setcar scor
		      (list (car (car scor)) (nth 2 (car scor))
			    (and (nth 3 (car scor))
				 (gnus-day-number (nth 3 (car scor))))
			    (if (nth 1 (car scor)) 'r 's)))
	      (setq scor (cdr scor))))
	(setq out (cons (if (not (listp (cdr entry))) 
			    (list (car entry) (cdr entry))
			  entry)
			out)))
      (setq alist (cdr alist)))
    (cons (list 'touched t) (nreverse out))))
  
(defun gnus-score-save ()
  ;; Save all score information.
  (let ((cache gnus-score-cache))
    (save-excursion
      (setq gnus-score-alist nil)
      (set-buffer (get-buffer-create "*Score*"))
      (buffer-disable-undo (current-buffer))
      (let (entry score file)
	(while cache
	  (setq entry (car cache)
		cache (cdr cache)
		file (car entry)
		score (cdr entry))
	  (if (or (not (equal (gnus-score-get 'touched score) '(t)))
		  (gnus-score-get 'read-only score)
		  (and (file-exists-p file)
		       (not (file-writable-p file))))
	      ()
	    (setq score (setcdr entry (delq (assq 'touched score) score)))
	    (erase-buffer)
	    (let (emacs-lisp-mode-hook)
	      (if (string-match (concat gnus-adaptive-file-suffix "$") file)
		  ;; This is an adaptive score file, so we do not run
		  ;; it through `pp'.  These files can get huge, and
		  ;; are not meant to be edited by human hands.
		  (insert (format "%S" score))
		;; This is a normal score file, so we print it very
		;; prettily. 
		(pp score (current-buffer))))
	    (if (not (gnus-make-directory (file-name-directory file)))
		()
	      ;; If the score file is empty, we delete it.
	      (if (zerop (buffer-size))
		  (delete-file file)
		;; There are scores, so we write the file. 
		(and (file-writable-p file)
		     (write-region (point-min) (point-max) 
				   file nil 'silent)))))))
      (kill-buffer (current-buffer)))))
  
(defun gnus-score-headers (score-files &optional trace)
  ;; Score `gnus-newsgroup-headers'.
  (let (scores)
    ;; PLM: probably this is not the best place to clear orphan-score
    (setq gnus-orphan-score nil)
    (setq gnus-scores-articles nil)
    (setq gnus-scores-exclude-files nil)
    ;; Load the score files.
    (while score-files
      (if (stringp (car score-files))
	  ;; It is a string, which means that it's a score file name,
	  ;; so we load the score file and add the score alist to
	  ;; the list of alists.
	  (setq scores (nconc (gnus-score-load-file (car score-files)) scores))
	;; It is an alist, so we just add it to the list directly.
	(setq scores (nconc (car score-files) scores)))
      (setq score-files (cdr score-files)))
    ;; Prune the score files that are to be excluded, if any.
    (if (not gnus-scores-exclude-files)
	()
      (let ((s scores)
	    c)
	(while s
	  (and (setq c (rassq (car s) gnus-score-cache))
	       (member (car c) gnus-scores-exclude-files)
	       (setq scores (delq (car s) scores)))
	  (setq s (cdr s)))))
    (if (not (and gnus-summary-default-score
		  scores
		  (> (length gnus-newsgroup-headers)
		     (length gnus-newsgroup-scored))))
	()
      (let* ((entries gnus-header-index)
	     (now (gnus-day-number (current-time-string)))
	     (expire (- now gnus-score-expiry-days))
	     (headers gnus-newsgroup-headers)
	     (current-score-file gnus-current-score-file)
	     entry header)
	(gnus-message 5 "Scoring...")
	;; Create articles, an alist of the form `(HEADER . SCORE)'.
	(while headers
	  (setq header (car headers)
		headers (cdr headers))
	  ;; WARNING: The assq makes the function O(N*S) while it could
	  ;; be written as O(N+S), where N is (length gnus-newsgroup-headers)
	  ;; and S is (length gnus-newsgroup-scored).
	  (or (assq (mail-header-number header) gnus-newsgroup-scored)
	      (setq gnus-scores-articles ;Total of 2 * N cons-cells used.
		    (cons (cons header (or gnus-summary-default-score 0))
			  gnus-scores-articles))))

	(save-excursion
	  (set-buffer (get-buffer-create "*Headers*"))
	  (buffer-disable-undo (current-buffer))

	  ;; Set the global variant of this variable.
	  (setq gnus-current-score-file current-score-file)
          ;; score orphans
          (if gnus-orphan-score 
              (progn
                (setq gnus-score-index 
                      (nth 1 (assoc "references" gnus-header-index)))
                (gnus-score-orphans gnus-orphan-score)))
	  ;; Run each header through the score process.
	  (while entries
	    (setq entry (car entries)
		  header (nth 0 entry)
		  entries (cdr entries))
	    (setq gnus-score-index (nth 1 (assoc header gnus-header-index)))
	    (if (< 0 (apply 'max (mapcar
				  (lambda (score)
				    (length (gnus-score-get header score)))
				  scores)))
		(funcall (nth 2 entry) scores header now expire trace)))
	  ;; Remove the buffer.
	  (kill-buffer (current-buffer)))

	;; Add articles to `gnus-newsgroup-scored'.
	(while gnus-scores-articles
	  (or (= gnus-summary-default-score (cdr (car gnus-scores-articles)))
	      (setq gnus-newsgroup-scored
		    (cons (cons (mail-header-number 
				 (car (car gnus-scores-articles)))
				(cdr (car gnus-scores-articles)))
			  gnus-newsgroup-scored)))
	  (setq gnus-scores-articles (cdr gnus-scores-articles)))

	(gnus-message 5 "Scoring...done")))))


(defun gnus-get-new-thread-ids (articles)
  (let ((index (nth 1 (assoc "message-id" gnus-header-index)))
        (refind gnus-score-index)
        id-list art this tref)
    (while articles
      (setq art (car articles)
            this (aref (car art) index)
            tref (aref (car art) refind)
            articles (cdr articles))
      (if (string-equal tref "")        ;no references line
          (setq id-list (cons this id-list))))
    id-list))

;; Orphan functions written by plm@atcmp.nl (Peter Mutsaers).
(defun gnus-score-orphans (score)
  (let ((new-thread-ids (gnus-get-new-thread-ids gnus-scores-articles))
        alike articles art arts this last this-id)
    
    (setq gnus-scores-articles (sort gnus-scores-articles 'gnus-score-string<)
	  articles gnus-scores-articles)

    ;;more or less the same as in gnus-score-string
    (erase-buffer)
    (while articles
      (setq art (car articles)
            this (aref (car art) gnus-score-index)
            articles (cdr articles))
      ;;completely skip if this is empty (not a child, so not an orphan)
      (if (not (string= this ""))
          (if (equal last this)
              ;; O(N*H) cons-cells used here, where H is the number of
              ;; headers.
              (setq alike (cons art alike))
            (if last
                (progn
                  ;; Insert the line, with a text property on the
                  ;; terminating newline referring to the articles with
                  ;; this line.
                  (insert last ?\n)
                  (put-text-property (1- (point)) (point) 'articles alike)))
            (setq alike (list art)
                  last this))))
    (and last                           ; Bwadr, duplicate code.
         (progn
           (insert last ?\n)                    
           (put-text-property (1- (point)) (point) 'articles alike)))

    ;; PLM: now delete those lines that contain an entry from new-thread-ids
    (while new-thread-ids
      (setq this-id (car new-thread-ids)
            new-thread-ids (cdr new-thread-ids))
      (goto-char (point-min))
      (while (search-forward this-id nil t)
        ;; found a match. remove this line
	(beginning-of-line)
	(kill-line 1)))

    ;; now for each line: update its articles with score by moving to
    ;; every end-of-line in the buffer and read the articles property
    (goto-char (point-min))
    (while (eq 0 (progn
                   (end-of-line)
                   (setq arts (get-text-property (point) 'articles))
                   (while arts
                     (setq art (car arts)
                           arts (cdr arts))
                     (setcdr art (+ score (cdr art))))
                   (forward-line))))))
             

(defun gnus-score-integer (scores header now expire &optional trace)
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	entries alist)

    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))		
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (or (nth 3 kill) '>))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (match-func (if (or (eq type '>) (eq type '<) (eq type '<=)
				   (eq type '>=) (eq type '=))
			       type
			     (error "Illegal match type: %s" type)))
	       (articles gnus-scores-articles))
	  ;; Instead of doing all the clever stuff that
	  ;; `gnus-score-string' does to minimize searches and stuff,
	  ;; I will assume that people generally will put so few
	  ;; matches on numbers that any cleverness will take more
	  ;; time than one would gain.
	  (while articles
	    (and (funcall match-func 
			  (or (aref (car (car articles)) gnus-score-index) 0)
			  match)
		 (progn
		   (and trace (setq gnus-score-trace 
				    (cons (cons (car (car articles)) kill)
					  gnus-score-trace)))
		   (setq found t)
		   (setcdr (car articles) (+ score (cdr (car articles))))))
	    (setq articles (cdr articles)))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		(found			;Match, update date.
		 (gnus-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((< date expire)	;Old entry, remove.
		 (gnus-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest))))))

(defun gnus-score-date (scores header now expire &optional trace)
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	entries alist)

    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))		
	       (kill (car rest))
	       (match (timezone-make-date-sortable (nth 0 kill)))
	       (type (or (nth 3 kill) 'before))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (match-func 
		(cond ((eq type 'after) 'string<)
		      ((eq type 'before) 'gnus-string>)
		      ((eq type 'at) 'string=)
		      (t (error "Illegal match type: %s" type))))
	       (articles gnus-scores-articles)
	       l)
	  ;; Instead of doing all the clever stuff that
	  ;; `gnus-score-string' does to minimize searches and stuff,
	  ;; I will assume that people generally will put so few
	  ;; matches on numbers that any cleverness will take more
	  ;; time than one would gain.
	  (while articles
	    (and
	     (setq l (aref (car (car articles)) gnus-score-index))
	     (funcall match-func match (timezone-make-date-sortable l))
	     (progn
	       (and trace (setq gnus-score-trace 
				(cons (cons (car (car articles)) kill)
				      gnus-score-trace)))
	       (setq found t)
	       (setcdr (car articles) (+ score (cdr (car articles))))))
	    (setq articles (cdr articles)))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		(found			;Match, update date.
		 (gnus-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((< date expire)	;Old entry, remove.
		 (gnus-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest))))))

(defun gnus-score-body (scores header now expire &optional trace)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (save-restriction
      (let* ((buffer-read-only nil)
	     (articles gnus-scores-articles)
	     (last (mail-header-number (car (car gnus-scores-articles))))
	     (all-scores scores)
	     (request-func (cond ((string= "head" (downcase header))
				  'gnus-request-head)
				 ((string= "body" (downcase header))
				  'gnus-request-body)
				 (t 'gnus-request-article)))
	     entries alist ofunc article)
	;; Not all backends support partial fetching.  In that case,
	;; we just fetch the entire article.
	(or (gnus-check-backend-function 
	     (and (string-match "^gnus-" (symbol-name request-func))
		  (intern (substring (symbol-name request-func)
				     (match-end 0))))
	     gnus-newsgroup-name)
	    (progn
	      (setq ofunc request-func)
	      (setq request-func 'gnus-request-article)))
	(while articles
	  (setq article (mail-header-number (car (car articles))))
	  (gnus-message 7 "Scoring on article %s of %s..." article last)
	  (if (not (funcall request-func article gnus-newsgroup-name))
	      ()
	    (widen)
	    (goto-char (point-min))
	    ;; If just parts of the article is to be searched, but the
	    ;; backend didn't support partial fetching, we just narrow
	    ;; to the relevant parts.
	    (if ofunc
		(if (eq ofunc 'gnus-request-head)
		    (narrow-to-region
		     (point)
		     (or (search-forward "\n\n" nil t) (point-max)))
		  (narrow-to-region
		   (or (search-forward "\n\n" nil t) (point))
		   (point-max))))
	    (setq scores all-scores)
	    ;; Find matches.
	    (while scores
	      (setq alist (car scores)
		    scores (cdr scores)
		    entries (assoc header alist))
	      (while (cdr entries)	;First entry is the header index.
		(let* ((rest (cdr entries))		
		       (kill (car rest))
		       (match (nth 0 kill))
		       (type (or (nth 3 kill) 's))
		       (score (or (nth 1 kill) 
				  gnus-score-interactive-default-score))
		       (date (nth 2 kill))
		       (found nil)
		       (case-fold-search 
			(not (or (eq type 'R) (eq type 'S)
				 (eq type 'Regexp) (eq type 'String))))
		       (search-func 
			(cond ((or (eq type 'r) (eq type 'R)
				   (eq type 'regexp) (eq type 'Regexp))
			       're-search-forward)
			      ((or (eq type 's) (eq type 'S)
				   (eq type 'string) (eq type 'String))
			       'search-forward)
			      (t
			       (error "Illegal match type: %s" type)))))
		  (goto-char (point-min))
		  (if (funcall search-func match nil t)
		      ;; Found a match, update scores.
		      (progn
			(setcdr (car articles) (+ score (cdr (car articles))))
			(setq found t)
			(and trace (setq gnus-score-trace 
					 (cons (cons (car (car articles)) kill)
					       gnus-score-trace)))))
		  ;; Update expire date
		  (cond ((null date))	;Permanent entry.
			(found		;Match, update date.
			 (gnus-score-set 'touched '(t) alist)
			 (setcar (nthcdr 2 kill) now))
			((< date expire) ;Old entry, remove.
			 (gnus-score-set 'touched '(t) alist)
			 (setcdr entries (cdr rest))
			 (setq rest entries)))
		  (setq entries rest)))))
	  (setq articles (cdr articles)))))))



(defun gnus-score-followup (scores header now expire &optional trace)
  ;; Insert the unique article headers in the buffer.
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	(current-score-file gnus-current-score-file)
	(all-scores scores)
	;; gnus-score-index is used as a free variable.
	alike last this art entries alist articles)

    ;; Change score file to the adaptive score file.  All entries that
    ;; this function makes will be put into this file.
    (gnus-score-load-file (gnus-score-file-name 
			   gnus-newsgroup-name gnus-adaptive-file-suffix))

    (setq gnus-scores-articles (sort gnus-scores-articles 'gnus-score-string<)
	  articles gnus-scores-articles)

    (erase-buffer)
    (while articles
      (setq art (car articles)
	    this (aref (car art) gnus-score-index)
	    articles (cdr articles))
      (if (equal last this)
	  (setq alike (cons art alike))
	(if last
	    (progn
	      (insert last ?\n)
	      (put-text-property (1- (point)) (point) 'articles alike)))
	(setq alike (list art)
	      last this)))
    (and last				; Bwadr, duplicate code.
	 (progn
	   (insert last ?\n)			
	   (put-text-property (1- (point)) (point) 'articles alike)))
  
    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))		
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (or (nth 3 kill) 's))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (mt (aref (symbol-name type) 0))
	       (case-fold-search 
		(not (or (= mt ?R) (= mt ?S) (= mt ?E) (= mt ?F))))
	       (dmt (downcase mt))
	       (search-func 
		(cond ((= dmt ?r) 're-search-forward)
		      ((or (= dmt ?e) (= dmt ?s) (= dmt ?f)) 'search-forward)
		      (t (error "Illegal match type: %s" type))))
	       arts art)
	  (goto-char (point-min))
	  (if (= dmt ?e)
	      (while (funcall search-func match nil t)
		(and (= (progn (beginning-of-line) (point))
			(match-beginning 0))
		     (= (progn (end-of-line) (point))
			(match-end 0))
		     (progn
		       (setq found (setq arts (get-text-property 
					       (point) 'articles)))
		       ;; Found a match, update scores.
		       (while arts
			 (setq art (car arts)
			       arts (cdr arts))
			 (gnus-score-add-followups 
			  (car art) score all-scores)))))
	    (while (funcall search-func match nil t)
	      (end-of-line)
	      (setq found (setq arts (get-text-property (point) 'articles)))
	      ;; Found a match, update scores.
	      (while arts
		(setq art (car arts)
		      arts (cdr arts))
		(gnus-score-add-followups (car art) score all-scores))))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		(found			;Match, update date.
		 (gnus-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((< date expire)	;Old entry, remove.
		 (gnus-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest))))
    ;; We change the score file back to the previous one.
    (gnus-score-load-file current-score-file)))

(defun gnus-score-add-followups (header score scores)
  (save-excursion
    (set-buffer gnus-summary-buffer)
    (let* ((id (mail-header-id header))
	   (scores (car scores))
	   entry dont)
      ;; Don't enter a score if there already is one.
      (while scores
	(setq entry (car scores))
	(and (equal "references" (car entry))
	     (or (null (nth 3 (car (cdr entry))))
		 (eq 's (nth 3 (car (cdr entry)))))
	     (progn
	       (if (assoc id entry)
		   (setq dont t))))
	(setq scores (cdr scores)))
      (or dont
	  (gnus-summary-score-entry 
	   "references" id 's score (current-time-string) nil t)))))


(defun gnus-score-string (score-list header now expire &optional trace)
  ;; Score ARTICLES according to HEADER in SCORE-LIST.
  ;; Update matching entries to NOW and remove unmatched entries older
  ;; than EXPIRE.
  
  ;; Insert the unique article headers in the buffer.
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	;; gnus-score-index is used as a free variable.
	alike last this art entries alist articles scores fuzzy)

    ;; Sorting the articles costs os O(N*log N) but will allow us to
    ;; only match with each unique header.  Thus the actual matching
    ;; will be O(M*U) where M is the number of strings to match with,
    ;; and U is the number of unique headers.  It is assumed (but
    ;; untested) this will be a net win because of the large constant
    ;; factor involved with string matching.
    (setq gnus-scores-articles (sort gnus-scores-articles 'gnus-score-string<)
	  articles gnus-scores-articles)

    (erase-buffer)
    (while articles
      (setq art (car articles)
	    this (aref (car art) gnus-score-index)
	    articles (cdr articles))
      (if (equal last this)
	  ;; O(N*H) cons-cells used here, where H is the number of
	  ;; headers.
	  (setq alike (cons art alike))
	(if last
	    (progn
	      ;; Insert the line, with a text property on the
	      ;; terminating newline referring to the articles with
	      ;; this line.
	      (insert last ?\n)
	      (put-text-property (1- (point)) (point) 'articles alike)))
	(setq alike (list art)
	      last this)))
    (and last				; Bwadr, duplicate code.
	 (progn
	   (insert last ?\n)			
	   (put-text-property (1- (point)) (point) 'articles alike)))
  
    ;; Find ordinary matches.
    (setq scores score-list) 
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))		
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (or (nth 3 kill) 's))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (mt (aref (symbol-name type) 0))
	       (case-fold-search 
		(not (or (= mt ?R) (= mt ?S) (= mt ?E) (= mt ?F))))
	       (dmt (downcase mt))
	       (search-func 
		(cond ((= dmt ?r) 're-search-forward)
		      ((or (= dmt ?e) (= dmt ?s) (= dmt ?f)) 'search-forward)
		      (t (error "Illegal match type: %s" type))))
	       arts art)
	  (if (= dmt ?f)
	      (setq fuzzy t)
	    (goto-char (point-min))
	    (if (= dmt ?e)
		(while (and (not (eobp)) 
			    (funcall search-func match nil t))
		  (and (= (progn (beginning-of-line) (point))
			  (match-beginning 0))
		       (= (progn (end-of-line) (point))
			  (match-end 0))
		       (progn
			 (setq found (setq arts (get-text-property 
						 (point) 'articles)))
			 ;; Found a match, update scores.
			 (if trace
			     (while arts
			       (setq art (car arts)
				     arts (cdr arts))
			       (setcdr art (+ score (cdr art)))
			       (setq gnus-score-trace 
				     (cons (cons (mail-header-number
						  (car art)) kill)
					   gnus-score-trace)))
			   (while arts
			     (setq art (car arts)
				   arts (cdr arts))
			     (setcdr art (+ score (cdr art)))))))
		  (forward-line 1))
	      (and (string= match "") (setq match "\n"))
	      (while (and (not (eobp))
			  (funcall search-func match nil t))
		(goto-char (match-beginning 0))
		(end-of-line)
		(setq found (setq arts (get-text-property (point) 'articles)))
		;; Found a match, update scores.
		(if trace
		    (while arts
		      (setq art (car arts)
			    arts (cdr arts))
		      (setcdr art (+ score (cdr art)))
		      (setq gnus-score-trace 
			    (cons (cons (mail-header-number (car art)) kill)
				  gnus-score-trace)))
		  (while arts
		    (setq art (car arts)
			  arts (cdr arts))
		    (setcdr art (+ score (cdr art)))))
		(forward-line 1)))
	    ;; Update expire date
	    (cond ((null date))		;Permanent entry.
		  (found		;Match, update date.
		   (gnus-score-set 'touched '(t) alist)
		   (setcar (nthcdr 2 kill) now))
		  ((< date expire)	;Old entry, remove.
		   (gnus-score-set 'touched '(t) alist)
		   (setcdr entries (cdr rest))
		   (setq rest entries))))
	  (setq entries rest))))
  
    ;; Find fuzzy matches.
    (setq scores (and fuzzy score-list))
    (if fuzzy (gnus-simplify-buffer-fuzzy))
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))		
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (or (nth 3 kill) 's))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (mt (aref (symbol-name type) 0))
	       (case-fold-search 
		(not (or (= mt ?R) (= mt ?S) (= mt ?E) (= mt ?F))))
	       (dmt (downcase mt))
	       (search-func 
		(cond ((= dmt ?r) 're-search-forward)
		      ((or (= dmt ?e) (= dmt ?s) (= dmt ?f)) 'search-forward)
		      (t (error "Illegal match type: %s" type))))
	       arts art)
	  (if (/= dmt ?f)
	      ()
	    (goto-char (point-min))
	    (while (and (not (eobp)) 
			(funcall search-func match nil t))
	      (and (= (progn (beginning-of-line) (point))
		      (match-beginning 0))
		   (= (progn (end-of-line) (point))
		      (match-end 0))
		   (progn
		     (setq found (setq arts (get-text-property 
					     (point) 'articles)))
		     ;; Found a match, update scores.
		     (if trace
			 (while arts
			   (setq art (car arts)
				 arts (cdr arts))
			   (setcdr art (+ score (cdr art)))
			   (setq gnus-score-trace 
				 (cons (cons (mail-header-number
					      (car art)) kill)
				       gnus-score-trace)))
		       (while arts
			 (setq art (car arts)
			       arts (cdr arts))
			 (setcdr art (+ score (cdr art)))))))
	      (forward-line 1))
	    ;; Update expire date
	    (cond ((null date))		;Permanent entry.
		  (found		;Match, update date.
		   (gnus-score-set 'touched '(t) alist)
		   (setcar (nthcdr 2 kill) now))
		  ((< date expire)	;Old entry, remove.
		   (gnus-score-set 'touched '(t) alist)
		   (setcdr entries (cdr rest))
		   (setq rest entries))))
	  (setq entries rest))))))

(defun gnus-score-string< (a1 a2)
  ;; Compare headers in articles A2 and A2.
  ;; The header index used is the free variable `gnus-score-index'.
  (string-lessp (aref (car a1) gnus-score-index)
		(aref (car a2) gnus-score-index)))

(defun gnus-score-build-cons (article)
  ;; Build a `gnus-newsgroup-scored' type cons from ARTICLE.
  (cons (mail-header-number (car article)) (cdr article)))

(defconst gnus-header-index
  ;; Name to index alist.
  '(("number" 0 gnus-score-integer)
    ("subject" 1 gnus-score-string)
    ("from" 2 gnus-score-string)
    ("date" 3 gnus-score-date)
    ("message-id" 4 gnus-score-string) 
    ("references" 5 gnus-score-string) 
    ("chars" 6 gnus-score-integer) 
    ("lines" 7 gnus-score-integer) 
    ("xref" 8 gnus-score-string)
    ("head" -1 gnus-score-body)
    ("body" -1 gnus-score-body)
    ("all" -1 gnus-score-body)
    ("followup" 2 gnus-score-followup)))

(defun gnus-current-score-file-nondirectory (&optional score-file)
  (let ((score-file (or score-file gnus-current-score-file)))
    (if score-file 
	(gnus-short-group-name (file-name-nondirectory score-file))
      "none")))

(defun gnus-score-adaptive ()
  (save-excursion
    (let* ((malist (gnus-copy-sequence gnus-adaptive-score-alist))
	   (alist malist)
	   (date (current-time-string)) 
	   elem headers match)
      ;; First we transform the adaptive rule alist into something
      ;; that's faster to process.
      (while malist
	(setq elem (car malist))
	(if (symbolp (car elem))
	    (setcar elem (symbol-value (car elem))))
	(setq elem (cdr elem))
	(while elem
	  (setcdr (car elem) 
		  (cons (symbol-name (car (car elem))) (cdr (car elem))))
	  (setcar (car elem) 
		  (intern 
		   (concat "gnus-header-" 
			   (downcase (symbol-name (car (car elem)))))))
	  (setq elem (cdr elem)))
	(setq malist (cdr malist)))
      ;; We change the score file to the adaptive score file.
      (gnus-score-load-file (gnus-score-file-name 
			     gnus-newsgroup-name gnus-adaptive-file-suffix))
      ;; The we score away.
      (goto-char (point-min))
      (while (not (eobp))
	(setq elem (cdr (assq (gnus-summary-article-mark) alist)))
	(if (or (not elem)
		(get-text-property (point) 'gnus-pseudo))
	    ()
	  (setq headers (gnus-get-header-by-number 
			 (gnus-summary-article-number)))
	  (while (and elem headers)
	    (setq match (funcall (car (car elem)) headers))
	    (gnus-summary-score-entry 
	     (nth 1 (car elem)) match
	     (cond
	      ((numberp match)
	       '=)
	      ((equal (nth 1 (car elem)) "date")
	       'a)
	      (t
	       ;; Whether we use substring or exact matches are controlled
	       ;; here.  
	       (if (or (not gnus-score-exact-adapt-limit)
		       (< (length match) gnus-score-exact-adapt-limit))
		   'e 
		 (if (equal (nth 1 (car elem)) "subject")
		     'f 's))))
	     (nth 2 (car elem)) date nil t)
	    (setq elem (cdr elem))))
	(forward-line 1)))))

(defun gnus-score-remove-lines-adaptive (marks)
  (save-excursion
    (let* ((malist (gnus-copy-sequence gnus-adaptive-score-alist))
	   (alist malist)
	   (date (current-time-string)) 
	   (cur-score gnus-current-score-file)
	   elem headers match)
      ;; First we transform the adaptive rule alist into something
      ;; that's faster to process.
      (while malist
	(setq elem (car malist))
	(if (symbolp (car elem))
	    (setcar elem (symbol-value (car elem))))
	(setq elem (cdr elem))
	(while elem
	  (setcdr (car elem) 
		  (cons (symbol-name (car (car elem))) (cdr (car elem))))
	  (setcar (car elem) 
		  (intern 
		   (concat "gnus-header-" 
			   (downcase (symbol-name (car (car elem)))))))
	  (setq elem (cdr elem)))
	(setq malist (cdr malist)))
      ;; The we score away.
      (goto-char (point-min))
      ;; We change the score file to the adaptive score file.
      (gnus-score-load-file (gnus-score-file-name 
			     gnus-newsgroup-name gnus-adaptive-file-suffix))
      (while (re-search-forward marks nil t)
	(beginning-of-line)
	(setq elem (cdr (assq (gnus-summary-article-mark) alist)))
	(if (or (not elem)
		(get-text-property (gnus-point-at-bol) 'gnus-pseudo))
	    ()
	  (setq headers (gnus-get-header-by-number 
			 (gnus-summary-article-number)))
	  (while elem
	    (setq match (funcall (car (car elem)) headers))
	    (gnus-summary-score-entry 
	     (nth 1 (car elem)) match
	     (if (or (not gnus-score-exact-adapt-limit)
		     (< (length match) gnus-score-exact-adapt-limit))
		 'e 's) 
	     (nth 2 (car elem)) date nil t)
	    (setq elem (cdr elem))))
	(delete-region (point) (progn (forward-line 1) (point))))
      ;; Switch back to the old score file.
      (gnus-score-load-file cur-score))))

;;;
;;; Score mode.
;;;

(defvar gnus-score-mode-map nil)
(defvar gnus-score-mode-hook nil)

(if gnus-score-mode-map
    ()
  (setq gnus-score-mode-map (copy-keymap emacs-lisp-mode-map))
  (define-key gnus-score-mode-map "\C-c\C-c" 'gnus-score-edit-done)
  (define-key gnus-score-mode-map "\C-c\C-d" 'gnus-score-edit-insert-date))

(defun gnus-score-mode ()
  "Mode for editing score files.
This mode is an extended emacs-lisp mode.

\\{gnus-score-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnus-score-mode-map)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq major-mode 'gnus-score-mode)
  (setq mode-name "Score")
  (lisp-mode-variables nil)
  (run-hooks 'emacs-lisp-mode-hook 'gnus-score-mode-hook))

(defun gnus-score-edit-insert-date ()
  "Insert date in numerical format."
  (interactive)
  (insert (int-to-string (gnus-day-number (current-time-string)))))

(defun gnus-score-edit-done ()
  "Save the score file and return to the summary buffer."
  (interactive)
  (let ((bufnam (buffer-file-name (current-buffer)))
	(winconf gnus-prev-winconf))
    (gnus-make-directory (file-name-directory (buffer-file-name)))
    (save-buffer)
    (kill-buffer (current-buffer))
    (gnus-score-remove-from-cache bufnam)
    (gnus-score-load-file bufnam)
    (and winconf (set-window-configuration winconf))))

(defun gnus-score-find-trace ()
  "Find all score rules applied to this article."
  (interactive)
  (let ((gnus-newsgroup-headers
	 (list (gnus-get-header-by-number (gnus-summary-article-number))))
	(gnus-newsgroup-scored nil)
	(buf (current-buffer))
	trace)
    (setq gnus-score-trace nil)
    (gnus-possibly-score-headers 'trace)
    (or (setq trace gnus-score-trace)
	(error "No score rules apply to the current article."))
    (pop-to-buffer "*Gnus Scores*")
    (gnus-add-current-to-buffer-list)
    (erase-buffer)
    (while trace
      (insert (format "%S\n" (cdr (car trace))))
      (setq trace (cdr trace)))
    (goto-char (point-min))
    (pop-to-buffer buf)))
  

(provide 'gnus-score)

;;; gnus-score.el ends here
