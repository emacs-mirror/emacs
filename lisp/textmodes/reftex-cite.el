;;; reftex-cite.el --- creating citations with RefTeX
;; Copyright (c) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@strw.LeidenUniv.nl>
;; Version: 4.16

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

(eval-when-compile (require 'cl))
(provide 'reftex-cite)
(require 'reftex)
;;;

;; Variables and constants

;; The history list of regular expressions used for citations
(defvar reftex-cite-regexp-hist nil)

;; Prompt and help string for citation selection
(defconst reftex-citation-prompt
  "Select: [n]ext [p]revious [r]estrict [ ]full_entry [q]uit RET [?]Help+more")

(defconst reftex-citation-help
  " n / p      Go to next/previous entry (Cursor motion works as well).
 g / r      Start over with new regexp / Refine with additional regexp.
 SPC        Show full database entry in other window.
 f          Toggle follow mode: Other window will follow with full db entry.
 .          Show insertion point.
 q          Quit without inserting \\cite macro into buffer.
 TAB        Enter citation key with completion.
 RET        Accept current entry (also on mouse-2) and create \\cite macro.
 m / u      Mark/Unmark the entry.
 a / A      Put all (marked) entries into one/many \\cite commands.")

;; Find bibtex files

(defun reftex-default-bibliography ()
  ;; Return the expanded value of `reftex-default-bibliography'.
  ;; The expanded value is cached.
  (unless (eq (get 'reftex-default-bibliography :reftex-raw)
	      reftex-default-bibliography)
    (put 'reftex-default-bibliography :reftex-expanded
	 (reftex-locate-bibliography-files 
	  default-directory reftex-default-bibliography))
    (put 'reftex-default-bibliography :reftex-raw
	 reftex-default-bibliography))
  (get 'reftex-default-bibliography :reftex-expanded))

(defun reftex-get-bibfile-list ()
  ;; Return list of bibfiles for current document.
  ;; When using the chapterbib or bibunits package you should either
  ;; use the same database files everywhere, or separate parts using
  ;; different databases into different files (included into the mater file).
  ;; Then this function will return the applicable database files.

  ;; Ensure access to scanning info
  (reftex-access-scan-info)
  (or
   ;; Try inside this file (and its includes)
   (cdr (reftex-last-assoc-before-elt
         'bib (list 'eof (buffer-file-name))
         (member (list 'bof (buffer-file-name))
                 (symbol-value reftex-docstruct-symbol))))
   ;; Try after the beginning of this file
   (cdr (assq 'bib (member (list 'bof (buffer-file-name))
                           (symbol-value reftex-docstruct-symbol))))
   ;; Anywhere in the entire document
   (cdr (assq 'bib (symbol-value reftex-docstruct-symbol)))
   (error "\\bibliography statement missing or .bib files not found")))

;; Find a certain reference in any of the BibTeX files.

(defun reftex-pop-to-bibtex-entry (key file-list &optional mark-to-kill
				       highlight item return)
  ;; Find BibTeX KEY in any file in FILE-LIST in another window.
  ;; If MARK-TO-KILL is non-nil, mark new buffer to kill.
  ;; If HIGHLIGHT is non-nil, highlight the match.
  ;; If ITEM in non-nil, search for bibitem instead of database entry.
  ;; If RETURN is non-nil, just return the entry.

  (let* ((re
	  (if item 
	      (concat "\\\\bibitem\\(\\[[^]]*\\]\\)?{" (regexp-quote key) "}")
	    (concat "@[a-zA-Z]+[ \t\n\r]*[{(][ \t\n\r]*" (regexp-quote key)
		    "[, \t\r\n}]")))
	 (buffer-conf (current-buffer))
         file buf pos)

    (catch 'exit
      (while file-list
        (setq file (car file-list)
              file-list (cdr file-list))
        (unless (setq buf (reftex-get-file-buffer-force file mark-to-kill))
          (error "No such file %s" file))
        (set-buffer buf)
        (widen)
        (goto-char (point-min))
        (when (re-search-forward re nil t)
          (goto-char (match-beginning 0))
	  (setq pos (point))
	  (when return
	    ;; Just return the relevant entry
	    (if item (goto-char (match-end 0)))
	    (setq return (buffer-substring 
			  (point) (reftex-end-of-bib-entry item)))
	    (set-buffer buffer-conf)
	    (throw 'exit return))
	  (switch-to-buffer-other-window buf)
	  (goto-char pos)
          (recenter 0)
          (if highlight
              (reftex-highlight 0 (match-beginning 0) (match-end 0)))
          (throw 'exit (selected-window))))
      (set-buffer buffer-conf)
      (if item
	  (error "No \\bibitem with citation key %s" key)
	(error "No BibTeX entry with citation key %s" key)))))

(defun reftex-end-of-bib-entry (item)
  (save-excursion 
    (condition-case nil
	(if item 
	    (progn (end-of-line)
		   (re-search-forward
		    "\\\\bibitem\\|\\end{thebibliography}")
		   (1- (match-beginning 0)))
	  (progn (forward-list 1) (point)))
      (error (min (point-max) (+ 300 (point)))))))

;; Parse bibtex buffers

(defun reftex-extract-bib-entries (buffers)
  ;; Extract bib entries which match regexps from BUFFERS.
  ;; BUFFERS is a list of buffers or file names.
  ;; Return list with entries."
  (let* (re-list first-re rest-re
                 (buffer-list (if (listp buffers) buffers (list buffers)))
                 found-list entry buffer1 buffer alist
                 key-point start-point end-point)

    ;; Read a regexp, completing on known citation keys.
    (setq re-list 
	  (split-string 
	   (completing-read 
	    "RegExp [ && RegExp...]: "
	    (if reftex-mode
		(if (fboundp 'LaTeX-bibitem-list)
		    (LaTeX-bibitem-list)
		  (cdr (assoc 'bibview-cache 
			      (symbol-value reftex-docstruct-symbol))))
	      nil)
	    nil nil nil 'reftex-cite-regexp-hist)
	   "[ \t]*&&[ \t]*"))

    (setq first-re (car re-list)    ; We'll use the first re to find things,
          rest-re  (cdr re-list))   ; the others to narrow down.
    (if (string-match "\\`[ \t]*\\'" (or first-re ""))
        (error "Empty regular expression"))

    (save-excursion
      (save-window-excursion

        ;; Walk through all bibtex files
        (while buffer-list
          (setq buffer (car buffer-list)
                buffer-list (cdr buffer-list))
          (if (and (bufferp buffer)
                   (buffer-live-p buffer))
              (setq buffer1 buffer)
            (setq buffer1 (reftex-get-file-buffer-force
                           buffer (not reftex-keep-temporary-buffers))))
          (if (not buffer1)
              (message "No such BibTeX file %s (ignored)" buffer)
            (message "Scanning bibliography database %s" buffer1))

          (set-buffer buffer1)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward first-re nil t)
              (catch 'search-again
                (setq key-point (point))
                (unless (re-search-backward
                         "\\(\\`\\|[\n\r]\\)[ \t]*@\\([a-zA-Z]+\\)[ \t\n\r]*[{(]" nil t)
                  (throw 'search-again nil))
                (setq start-point (point))
                (goto-char (match-end 0))
                (condition-case nil
                    (up-list 1)
                  (error (goto-char key-point)
                          (throw 'search-again nil)))
                (setq end-point (point))

                ;; Ignore @string, @comment and @c entries or things
                ;; outside entries
                (when (or (string= (downcase (match-string 2)) "string")
                          (string= (downcase (match-string 2)) "comment")
                          (string= (downcase (match-string 2)) "c")
                          (< (point) key-point)) ; this means match not in {}
                  (goto-char key-point)
                  (throw 'search-again nil))

                ;; Well, we have got a match
                (setq entry (concat
                             (buffer-substring start-point (point)) "\n"))

                ;; Check if other regexp match as well
                (setq re-list rest-re)
                (while re-list
                  (unless (string-match (car re-list) entry)
                    ;; nope - move on
                    (throw 'search-again nil))
                  (pop re-list))

                (setq alist (reftex-parse-bibtex-entry
                             nil start-point end-point))
                (push (cons "&entry" entry) alist)

                ;; check for crossref entries
                (if (assoc "crossref" alist)
                    (setq alist
                          (append
                           alist (reftex-get-crossref-alist alist))))

                ;; format the entry
                (push (cons "&formatted" (reftex-format-bib-entry alist))
                      alist)

		;; make key the first element
		(push (reftex-get-bib-field "&key" alist) alist)

                ;; add it to the list
                (push alist found-list))))
          (reftex-kill-temporary-buffers))))
    (setq found-list (nreverse found-list))

    ;; Sorting
    (cond
     ((eq 'author reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-author))
     ((eq 'year   reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-year))
     ((eq 'reverse-year reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-year-reverse))
     (t found-list))))

(defun reftex-bib-sort-author (e1 e2)
  (let ((al1 (reftex-get-bib-names "author" e1))
        (al2 (reftex-get-bib-names "author" e2)))
    (while (and al1 al2 (string= (car al1) (car al2)))
      (pop al1)
      (pop al2))
    (if (and (stringp (car al1))
             (stringp (car al2)))
        (string< (car al1) (car al2))
      (not (stringp (car al1))))))

(defun reftex-bib-sort-year (e1 e2)
  (< (string-to-int (cdr (assoc "year" e1)))
     (string-to-int (cdr (assoc "year" e2)))))

(defun reftex-bib-sort-year-reverse (e1 e2)
  (> (string-to-int (or (cdr (assoc "year" e1)) "0"))
     (string-to-int (or (cdr (assoc "year" e2)) "0"))))

(defun reftex-get-crossref-alist (entry)
  ;; return the alist from a crossref entry
  (let ((crkey (cdr (assoc "crossref" entry)))
        start)
    (save-excursion
      (save-restriction
        (widen)
        (if (re-search-forward
             (concat "@\\w+[{(][ \t\n\r]*" (regexp-quote crkey)
                     "[ \t\n\r]*,") nil t)
            (progn
              (setq start (match-beginning 0))
              (condition-case nil
                  (up-list 1)
                (error nil))
              (reftex-parse-bibtex-entry nil start (point)))
          nil)))))

;; Parse the bibliography environment
(defun reftex-extract-bib-entries-from-thebibliography (files)
  ;; Extract bib-entries from the \begin{thebibliography} environment.
  ;; Parsing is not as good as for the BibTeX database stuff.
  ;; The environment should be located in file FILE.

  (let* (start end buf entries re re-list file)
    (unless files
      (error "Need file name to find thebibliography environment"))
    (while (setq file (pop files))
      (setq buf (reftex-get-file-buffer-force 
		 file (not reftex-keep-temporary-buffers)))
      (unless buf
	(error "No such file %s" file))
      (message "Scanning thebibliography environment in %s" file)

      (save-excursion
	(set-buffer buf)
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (re-search-forward 
		  "\\(\\`\\|[\n\r]\\)[ \t]*\\\\begin{thebibliography}" nil t)
	    (beginning-of-line 2)
	    (setq start (point))
	    (if (re-search-forward 
		 "\\(\\`\\|[\n\r]\\)[ \t]*\\\\end{thebibliography}" nil t)
		(progn
		  (beginning-of-line 1)
		  (setq end (point))))
	    (when (and start end)
	      (setq entries 
		    (append entries
                      (mapcar 'reftex-parse-bibitem
			(delete ""
				(split-string 
				 (buffer-substring-no-properties start end)
				 "[ \t\n\r]*\\\\bibitem\\(\\[[^]]*]\\)*"))))))
	    (goto-char end)))))
    (unless entries
      (error "No bibitems found"))

    (setq re-list (split-string 
		   (read-string "RegExp [ && RegExp...]: "
				nil 'reftex-cite-regexp-hist)
		   "[ \t]*&&[ \t]*"))
    (if (string-match "\\`[ \t]*\\'" (car re-list))
        (error "Empty regular expression"))

    (while (and (setq re (pop re-list)) entries)
      (setq entries 
	    (delq nil (mapcar
		       (lambda (x)
			 (if (string-match re (cdr (assoc "&entry" x)))
			     x nil))
		       entries))))
    (setq entries 
	  (mapcar 
	    (lambda (x)
	      (push (cons "&formatted" (reftex-format-bibitem x)) x)
	      (push (reftex-get-bib-field "&key" x) x)
	      x)
	   entries))

    entries))

;; Parse and format individual entries

(defun reftex-get-bib-names (field entry)
  ;; Return a list with the author or editor names in ENTRY
  (let ((names (reftex-get-bib-field field entry)))
    (if (equal "" names)
        (setq names (reftex-get-bib-field "editor" entry)))
    (while (string-match "\\band\\b[ \t]*" names)
      (setq names (replace-match "\n" nil t names)))
    (while (string-match "[\\.a-zA-Z\\-]+\\.[ \t]*\\|,.*\\|[{}]+" names)
      (setq names (replace-match "" nil t names)))
    (while (string-match "^[ \t]+\\|[ \t]+$" names)
      (setq names (replace-match "" nil t names)))
    (while (string-match "[ \t][ \t]+" names)
      (setq names (replace-match " " nil t names)))
    (split-string names "\n")))

(defun reftex-parse-bibtex-entry (entry &optional from to)
  (let (alist key start field)
    (save-excursion
      (save-restriction
        (if entry
            (progn
              (set-buffer (get-buffer-create " *RefTeX-scratch*"))
              (fundamental-mode)
              (erase-buffer)
              (insert entry))
          (widen)
          (narrow-to-region from to))
        (goto-char (point-min))

        (if (re-search-forward
             "@\\(\\w+\\)[ \t\n\r]*[{(][ \t\n\r]*\\([^ \t\n\r,]+\\)" nil t)
            (setq alist
                  (list
                   (cons "&type" (downcase (reftex-match-string 1)))
                   (cons "&key"  (reftex-match-string 2)))))
        (while (re-search-forward "\\(\\w+\\)[ \t\n\r]*=[ \t\n\r]*" nil t)
          (setq key (downcase (reftex-match-string 1)))
          (cond
           ((= (following-char) ?{)
            (forward-char 1)
            (setq start (point))
            (condition-case nil
                (up-list 1)
              (error nil)))
           ((= (following-char) ?\")
            (forward-char 1)
            (setq start (point))
            (while (and (search-forward "\"" nil t)
                        (= ?\\ (char-after (- (point) 2))))))
           (t
            (setq start (point))
            (re-search-forward "[ \t]*[\n\r,}]" nil 1)))
          (setq field (buffer-substring-no-properties start (1- (point))))
          ;; remove extra whitespace
          (while (string-match "[\n\t\r]\\|[ \t][ \t]+" field)
            (setq field (replace-match " " nil t field)))
          ;; remove leading garbage
          (if (string-match "^[ \t{]+" field)
              (setq field (replace-match "" nil t field)))
          ;; remove trailing garbage
          (if (string-match "[ \t}]+$" field)
              (setq field (replace-match "" nil t field)))
          (push (cons key field) alist))))
    alist))

(defun reftex-get-bib-field (fieldname entry &optional format)
  ;; Extract the field FIELDNAME from an ENTRY
  (let ((cell (assoc fieldname entry)))
    (if cell
	(if format
	    (format format (cdr cell))
	  (cdr cell))
      "")))

(defun reftex-format-bib-entry (entry)
  ;; Format a BibTeX ENTRY so that it is nice to look at
  (let*
      ((auth-list (reftex-get-bib-names "author" entry))
       (authors (mapconcat 'identity auth-list ", "))
       (year      (reftex-get-bib-field "year" entry))
       (title     (reftex-get-bib-field "title" entry))
       (type      (reftex-get-bib-field "&type" entry))
       (key       (reftex-get-bib-field "&key"  entry))
       (extra
        (cond
         ((equal type "article")
          (concat (reftex-get-bib-field "journal" entry) " "
                  (reftex-get-bib-field "volume" entry) ", "
                  (reftex-get-bib-field "pages" entry)))
         ((equal type "book")
          (concat "book (" (reftex-get-bib-field "publisher" entry) ")"))
         ((equal type "phdthesis")
          (concat "PhD: " (reftex-get-bib-field "school" entry)))
         ((equal type "mastersthesis")
          (concat "Master: " (reftex-get-bib-field "school" entry)))
         ((equal type "inbook")
          (concat "Chap: " (reftex-get-bib-field "chapter" entry)
                  ", pp. " (reftex-get-bib-field "pages"   entry)))
         ((or (equal type "conference")
              (equal type "incollection")
              (equal type "inproceedings"))
          (reftex-get-bib-field "booktitle" entry "in: %s"))
         (t ""))))
    (setq authors (reftex-truncate authors 30 t t))
    (when (reftex-use-fonts)
      (put-text-property 0 (length key)     'face
			 (reftex-verified-face reftex-label-face
					       'font-lock-constant-face
					       'font-lock-reference-face)
                         key)
      (put-text-property 0 (length authors) 'face reftex-bib-author-face
                         authors)
      (put-text-property 0 (length year)    'face reftex-bib-year-face
                         year)
      (put-text-property 0 (length title)   'face reftex-bib-title-face
                         title)
      (put-text-property 0 (length extra)   'face reftex-bib-extra-face
                         extra))
    (concat key "\n     " authors " " year " " extra "\n     " title "\n\n")))

(defun reftex-parse-bibitem (item)
  ;; Parse a \bibitem entry
  (let ((key "") (text ""))
    (when (string-match "\\`{\\([^}]+\\)}\\([^\000]*\\)" item)
      (setq key (match-string 1 item)
	    text (match-string 2 item)))
    ;; Clean up the text a little bit
    (while (string-match "[\n\r\t]\\|[ \t][ \t]+" text)
      (setq text (replace-match " " nil t text)))
    (if (string-match "\\`[ \t]+" text)
	(setq text (replace-match "" nil t text)))
    (list
     (cons "&key" key)
     (cons "&text" text)
     (cons "&entry" (concat key " " text)))))

(defun reftex-format-bibitem (item)
  ;; Format a \bibitem entry so that it is (relatively) nice to look at.
  (let ((text (reftex-get-bib-field "&text" item))
	(key  (reftex-get-bib-field "&key" item))
	(lines nil))

    ;; Wrap the text into several lines.
    (while (and (> (length text) 70)
		(string-match " " (substring text 60)))
	(push (substring text 0 (+ 60 (match-beginning 0))) lines)
	(setq text (substring text (+ 61 (match-beginning 0)))))
    (push text lines)
    (setq text (mapconcat 'identity (nreverse lines) "\n     "))

    (when (reftex-use-fonts)
      (put-text-property 0 (length text) 'face reftex-bib-author-face text))
    (concat key "\n     " text "\n\n")))

;; Make a citation

;;;###autoload
(defun reftex-citation (&optional no-insert format-key)
  "Make a citation using BibTeX database files.
After prompting for a regular expression, scans the buffers with
bibtex entries (taken from the \\bibliography command) and offers the
matching entries for selection.  The selected entry is formated according
to `reftex-cite-format' and inserted into the buffer.

If NO-INSERT is non-nil, nothing is inserted, only the selected key returned.

FORAT-KEY can be used to pre-select a citation format.

When called with one or two `C-u' prefixes, first rescans the document.
When called with a numeric prefix, make that many citations.  When
called with point inside the braces of a `\\cite' command, it will
add another key, ignoring the value of `reftex-cite-format'.

The regular expression uses an expanded syntax: && is interpreted as `and'.
Thus, `aaaa&&bbb' matches entries which contain both `aaaa' and `bbb'.
While entering the regexp, completion on knows citation keys is possible.
`=' is a good regular expression to match all entries in all files."

  (interactive)

  ;; check for recursive edit
  (reftex-check-recursive-edit)

  ;; This function may also be called outside reftex-mode.
  ;; Thus look for the scanning info only if in reftex-mode.

  (when reftex-mode
    (reftex-access-scan-info current-prefix-arg))

  ;; Call reftex-do-citation, but protected
  (unwind-protect
      (reftex-do-citation current-prefix-arg no-insert format-key)
    (reftex-kill-temporary-buffers)))

(defun reftex-do-citation (&optional arg no-insert format-key)
  ;; This really does the work of reftex-citation.

  (let* ((format (reftex-figure-out-cite-format arg no-insert format-key))
	 (docstruct-symbol reftex-docstruct-symbol)
	 (selected-entries (reftex-offer-bib-menu))
	 (insert-entries selected-entries)
	 entry string cite-view)

    (unless selected-entries (error "Quit"))

    (if (stringp selected-entries)
	;; Nonexistent entry
	(setq selected-entries nil
	      insert-entries (list (list selected-entries
					 (cons "&key" selected-entries))))
      ;; It makes sense to compute the cite-view strings.
      (setq cite-view t))

    (when (eq (car selected-entries) 'concat)
      ;; All keys go into a single command - we need to trick a little
      (pop selected-entries)
      (let ((concat-keys (mapconcat 'car selected-entries ",")))
	(setq insert-entries 
	      (list (list concat-keys (cons "&key" concat-keys))))))
    
    (unless no-insert

      ;; We shall insert this into the buffer...
      (message "Formatting...")

      (while (setq entry (pop insert-entries))
	;; Format the citation and insert it
	(setq string (if reftex-format-cite-function
			 (funcall reftex-format-cite-function
				  (reftex-get-bib-field "&key" entry)
				  format)
		       (reftex-format-citation entry format)))
	(insert string))

      ;; Reposition cursor?
      (when (string-match "\\?" string)
	(search-backward "?")
	(delete-char 1))

      ;; Tell AUCTeX
      (when (and reftex-mode 
		 (fboundp 'LaTeX-add-bibitems)
		 reftex-plug-into-AUCTeX)
	(apply 'LaTeX-add-bibitems (mapcar 'car selected-entries)))
      
      ;; Produce the cite-view strings
      (when (and reftex-mode reftex-cache-cite-echo cite-view)
	(mapcar (lambda (entry) 
		  (reftex-make-cite-echo-string entry docstruct-symbol))
		selected-entries))

      (message ""))

    (set-marker reftex-select-return-marker nil)
    (reftex-kill-buffer "*RefTeX Select*")
    
    ;; Check if the prefix arg was numeric, and call recursively
    (when (integerp arg)
      (if (> arg 1)
	  (progn      
	    (skip-chars-backward "}")
	    (decf arg)
	    (reftex-do-citation arg))
	(forward-char 1)))
    
    ;; Return the citation key
    (car (car selected-entries))))

(defun reftex-figure-out-cite-format (arg &optional no-insert format-key)
  ;; Check if there is already a cite command at point and change cite format
  ;; in order to only add another reference in the same cite command.
  (let ((macro (car (reftex-what-macro 1)))
	(cite-format-value (reftex-get-cite-format))
	key format)
    (cond
     (no-insert
      ;; Format does not really matter because nothing will be inserted.
      (setq format "%l"))
     
     ((and (stringp macro)
	   (string-match "\\`\\\\cite\\|cite\\'" macro))
      ;; We are already inside a cite macro
      (if (or (not arg) (not (listp arg)))
	  (setq format
		(concat
		 (if (member (preceding-char) '(?\{ ?,)) "" ",")
		 "%l"
		 (if (member (following-char) '(?\} ?,)) "" ",")))
	(setq format "%l")))
     (t
      ;; Figure out the correct format
      (setq format
            (if (and (symbolp cite-format-value)
		     (assq cite-format-value reftex-cite-format-builtin))
		(nth 2 (assq cite-format-value reftex-cite-format-builtin))
	      cite-format-value))
      (when (listp format)
	(setq key
	      (or format-key
		  (reftex-select-with-char 
		   "" (concat "SELECT A CITATION FORMAT\n\n"
			      (mapconcat
			       (lambda (x)
				 (format "[%c] %s  %s" (car x)
					 (if (> (car x) 31) " " "")
					 (cdr x)))
			       format "\n")))))
	(if (assq key format)
	    (setq format (cdr (assq key format)))
	  (error "No citation format associated with key `%c'" key)))))
    format))

(defun reftex-citep ()
  "Call `reftex-citation' with a format selector `?p'."
  (interactive)
  (reftex-citation nil ?p))

(defun reftex-citet ()
  "Call `reftex-citation' with a format selector `?t'."
  (interactive)
  (reftex-citation nil ?t))

(defvar reftex-select-bib-map)
(defun reftex-offer-bib-menu ()
  ;; Offer bib menu and return list of selected items

  (let (found-list rtn key data selected-entries)
    (while 
	(not 
	 (catch 'done
	   ;; Scan bibtex files
	   (setq found-list
	      (cond
	       ((assq 'bib (symbol-value reftex-docstruct-symbol))
		;; using BibTeX database files.
		(reftex-extract-bib-entries (reftex-get-bibfile-list)))
	       ((assq 'thebib (symbol-value reftex-docstruct-symbol))
		;; using thebibliography environment.
		(reftex-extract-bib-entries-from-thebibliography
		 (reftex-uniquify
		  (mapcar 'cdr
			  (reftex-all-assq 
			   'thebib (symbol-value reftex-docstruct-symbol))))))
	       (reftex-default-bibliography
		(message "Using default bibliography")
		(reftex-extract-bib-entries (reftex-default-bibliography)))
	       (t (error "No valid bibliography in this document, and no default available"))))
	   
	   (unless found-list
	     (error "Sorry, no matches found"))
    
	  ;; Remember where we came from
	  (setq reftex-call-back-to-this-buffer (current-buffer))
	  (set-marker reftex-select-return-marker (point))
    
	  ;; Offer selection
	  (save-window-excursion
	    (delete-other-windows)
	    (let ((default-major-mode 'reftex-select-bib-mode))
	      (reftex-kill-buffer "*RefTeX Select*")
	      (switch-to-buffer-other-window "*RefTeX Select*")
	      (unless (eq major-mode 'reftex-select-bib-mode)
		(reftex-select-bib-mode))
	      (let ((buffer-read-only nil))
		(erase-buffer)
		(reftex-insert-bib-matches found-list)))
	    (setq buffer-read-only t)
	    (if (= 0 (buffer-size))
		(error "No matches found"))
	    (setq truncate-lines t)
	    (goto-char 1)
	    (while t
	      (setq rtn
		    (reftex-select-item
		     reftex-citation-prompt
		     reftex-citation-help
		     reftex-select-bib-map
		     nil
		     'reftex-bibtex-selection-callback nil))
	      (setq key (car rtn)
		    data (nth 1 rtn))
	      (unless key (throw 'done t))
	      (cond
	       ((eq key ?g)
		;; Start over
		(throw 'done nil))
	       ((eq key ?r)
		;; Restrict with new regular expression
		(setq found-list (reftex-restrict-bib-matches found-list))
		(let ((buffer-read-only nil))
		  (erase-buffer)
		  (reftex-insert-bib-matches found-list))
		(goto-char 1))
	       ((eq key ?A)
		;; Take all (marked)
		(setq selected-entries 
		      (if reftex-select-marked
			  (mapcar 'car (nreverse reftex-select-marked))
			found-list))
		(throw 'done t))
	       ((eq key ?a)
		;; Take all (marked), and push the symbol 'concat
		(setq selected-entries 
		      (cons 'concat 
			    (if reftex-select-marked
				(mapcar 'car (nreverse reftex-select-marked))
			      found-list)))
		(throw 'done t))
	       ((or (eq key ?\C-m)
		    (eq key 'return))
		;; Take selected
		(setq selected-entries 
		      (if reftex-select-marked
			  (cons 'concat 
				(mapcar 'car (nreverse reftex-select-marked)))
			(if data (list data) nil)))
		(throw 'done t))
	       ((stringp key)
		;; Got this one with completion
		(setq selected-entries key)
		(throw 'done t))
	       (t
		(ding))))))))
    selected-entries))

(defun reftex-restrict-bib-matches (found-list)
  ;; Limit FOUND-LIST with more regular expressions
  (let ((re-list (split-string (read-string
				"RegExp [ && RegExp...]: "
				nil 'reftex-cite-regexp-hist)
			       "[ \t]*&&[ \t]*"))
	(found-list-r found-list)
	re)
    (while (setq re (pop re-list))
      (setq found-list-r
	    (delq nil
		  (mapcar
		   (lambda (x)
		     (if (string-match
			  re (cdr (assoc "&entry" x)))
			 x
		       nil))
		   found-list-r))))
    (if found-list-r
	found-list-r
      (ding)
      found-list)))

(defun reftex-insert-bib-matches (list)
  ;; Insert the bib matches and number them correctly
  (let ((mouse-face
	 (if (memq reftex-highlight-selection '(mouse both))
	     reftex-mouse-selected-face
	   nil))
	tmp len)
    (mapcar 
     (lambda (x)
       (setq tmp (cdr (assoc "&formatted" x))
	     len (length tmp))
       (put-text-property 0 len :data x tmp)
       (put-text-property 0 (1- len) 'mouse-face mouse-face tmp)
       (insert tmp))
     list))
  (run-hooks 'reftex-display-copied-context-hook))

(defun reftex-format-names (namelist n)
  (let (last (len (length namelist)))
    (cond
     ((< len 1) "")
     ((= 1 len) (car namelist))
     ((> len n) (concat (car namelist) (nth 2 reftex-cite-punctuation)))
     (t
      (setq n (min len n)
            last (nth (1- n) namelist))
      (setcdr (nthcdr (- n 2) namelist) nil)
      (concat
       (mapconcat 'identity namelist (nth 0 reftex-cite-punctuation))
       (nth 1 reftex-cite-punctuation)
       last)))))

(defun reftex-format-citation (entry format)
  ;; Format a citation from the info in the BibTeX ENTRY

  (unless (stringp format) (setq format "\\cite{%l}"))

  (if (and reftex-comment-citations
           (string-match "%l" reftex-cite-comment-format))
      (error "reftex-cite-comment-format contains illegal %%l"))

  (while (string-match
          "\\(\\`\\|[^%]\\)\\(\\(%\\([0-9]*\\)\\([a-zA-Z]\\)\\)[.,;: ]*\\)"
          format)
    (let ((n (string-to-int (match-string 4 format)))
          (l (string-to-char (match-string 5 format)))
          rpl b e)
      (save-match-data
        (setq rpl
              (cond
               ((= l ?l) (concat
                          (reftex-get-bib-field "&key" entry)
                          (if reftex-comment-citations
                              reftex-cite-comment-format
                            "")))
               ((= l ?a) (reftex-format-names
                          (reftex-get-bib-names "author" entry)
                          (or n 2)))
               ((= l ?A) (car (reftex-get-bib-names "author" entry)))
               ((= l ?b) (reftex-get-bib-field "booktitle" entry "in: %s"))
               ((= l ?B) (reftex-abbreviate-title
			  (reftex-get-bib-field "booktitle" entry "in: %s")))
               ((= l ?c) (reftex-get-bib-field "chapter" entry))
               ((= l ?d) (reftex-get-bib-field "edition" entry))
               ((= l ?e) (reftex-format-names
                          (reftex-get-bib-names "editor" entry)
                          (or n 2)))
               ((= l ?E) (car (reftex-get-bib-names "editor" entry)))
               ((= l ?h) (reftex-get-bib-field "howpublished" entry))
               ((= l ?i) (reftex-get-bib-field "institution" entry))
               ((= l ?j) (reftex-get-bib-field "journal" entry))
               ((= l ?k) (reftex-get-bib-field "key" entry))
               ((= l ?m) (reftex-get-bib-field "month" entry))
               ((= l ?n) (reftex-get-bib-field "number" entry))
               ((= l ?o) (reftex-get-bib-field "organization" entry))
               ((= l ?p) (reftex-get-bib-field "pages" entry))
               ((= l ?P) (car (split-string
                               (reftex-get-bib-field "pages" entry)
                               "[- .]+")))
               ((= l ?s) (reftex-get-bib-field "school" entry))
               ((= l ?u) (reftex-get-bib-field "publisher" entry))
               ((= l ?r) (reftex-get-bib-field "address" entry))
               ((= l ?t) (reftex-get-bib-field "title" entry))
               ((= l ?T) (reftex-abbreviate-title
			  (reftex-get-bib-field "title" entry)))
               ((= l ?v) (reftex-get-bib-field "volume" entry))
               ((= l ?y) (reftex-get-bib-field "year" entry)))))

      (if (string= rpl "")
          (setq b (match-beginning 2) e (match-end 2))
        (setq b (match-beginning 3) e (match-end 3)))
      (setq format (concat (substring format 0 b) rpl (substring format e)))))
  (while (string-match "%%" format)
    (setq format (replace-match "%" t t format)))
  (while (string-match "[ ,.;:]*%<" format)
    (setq format (replace-match "" t t format)))
  format)

(defun reftex-make-cite-echo-string (entry docstruct-symbol)
  ;; Format a bibtex entry for the echo area and cache the result.
  (let* ((key (reftex-get-bib-field "&key" entry))
	 (string 
	  (let* ((reftex-cite-punctuation '(" " " & " " etal.")))
	    (reftex-format-citation entry reftex-cite-view-format)))
	 (cache (assq 'bibview-cache (symbol-value docstruct-symbol)))
	 (cache-entry (assoc key (cdr cache))))
    (unless cache
      ;; This docstruct has no cache - make one.
      (set docstruct-symbol (cons (cons 'bibview-cache nil)
				  (symbol-value docstruct-symbol))))
    (when reftex-cache-cite-echo
      (setq key (copy-sequence key))
      (set-text-properties 0 (length key) nil key)
      (set-text-properties 0 (length string) nil string)
      (if cache-entry
	  (unless (string= (cdr cache-entry) string)
	    (setcdr cache-entry string)
	    (put reftex-docstruct-symbol 'modified t))
	(push (cons key string) (cdr cache))
	(put reftex-docstruct-symbol 'modified t)))
    string))

(defun reftex-bibtex-selection-callback (data ignore no-revisit)
  ;; Callback function to be called from the BibTeX selection, in
  ;; order to display context.  This function is relatively slow and not
  ;; recommended for follow mode.  It works OK for individual lookups.
  (let ((win (selected-window))
        (key (reftex-get-bib-field "&key" data))
        bibfile-list item)

    (catch 'exit
      (save-excursion
	(set-buffer reftex-call-back-to-this-buffer)
	(cond
	 ((assq 'bib (symbol-value reftex-docstruct-symbol))
	  (setq bibfile-list (reftex-get-bibfile-list)))
	 ((assq 'thebib (symbol-value reftex-docstruct-symbol))
	  (setq bibfile-list
		(reftex-uniquify
		 (mapcar 'cdr
			 (reftex-all-assq 
			  'thebib (symbol-value reftex-docstruct-symbol))))
		item t))
	 (reftex-default-bibliography
	  (setq bibfile-list (reftex-default-bibliography)))
	 (t (ding) (throw 'exit nil))))

      (when no-revisit
	(setq bibfile-list (reftex-visited-files bibfile-list)))

      (condition-case nil
	  (reftex-pop-to-bibtex-entry 
	   key bibfile-list (not reftex-keep-temporary-buffers) t item)
	(error (ding))))
      
    (select-window win)))

;;; reftex-cite.el ends here
