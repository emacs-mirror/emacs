;;; reftex-index.el - Index support with RefTeX
;;; Version: 4.5
;;;
;;; See main file reftex.el for licensing information

(provide 'reftex-index)
(require 'reftex)
;;;

(defvar mark-active)
(defvar zmacs-regions)
(defvar transient-mark-mode)
(defun reftex-index-selection-or-word (&optional arg)
  "Put selection or the word near point into the default index macro.
This uses the information in `reftex-index-default-macro' to make an index
entry.  The phrase indexed is the current selection or the word near point.
When called with one `C-u' prefix, let the user have a chance to edit the
index entry.  When called with 2 `C-u' as prefix, also ask for the index
macro and other stuff.
When called inside TeX math mode as determined by the `texmathp.el' library
which is part of AUCTeX, the string is first processed with the
`reftex-index-math-format', which see."
  (interactive "P")
  (let* ((use-default (not (equal arg '(16))))  ; check for double prefix
	 ;; check if we have an active selection
	 (active (if (boundp 'zmacs-regions)
		     (and zmacs-regions (region-exists-p))  ; XEmacs
		   (and transient-mark-mode mark-active)))  ; Emacs
	 (beg (if active 
		  (region-beginning)
		(save-excursion 
		  (skip-syntax-backward "w\\") (point))))
	 (end (if active
		  (region-end)
		(save-excursion 
		  (skip-syntax-forward "w\\") (point))))
	 (sel (buffer-substring beg end))
	 (mathp (condition-case nil (texmathp) (error nil)))
	 (current-prefix-arg nil) ; we want to call reftex-index without prefix.
	 key def-char def-tag full-entry repeat-word)

    (if (equal sel "")
	;; Nothing selecte, no word, so use full reftex-index command
	(reftex-index)
      ;; OK, we have something to index here.
      ;; Add the dollars when necessary
      (setq key (if mathp
		    (format reftex-index-math-format sel)
		  sel))
      ;; Get info from `reftex-index-default-macro'
      (setq def-char (if use-default (car reftex-index-default-macro)))
      (setq def-tag  (if use-default (nth 1 reftex-index-default-macro)))
      ;; Does the user want to edit the entry?
      (setq full-entry (if arg
			   (reftex-index-complete-key
			    def-tag nil (cons key 0))
			 key))
      ;; Do we neet to repeat the word outside the macro?
      (setq repeat-word (if use-default
			    (nth 2 reftex-index-default-macro)
			  (y-or-n-p "Repeat phrase outside macro? ")))
      ;; Delete what is in the buffer and make the index entry
      (delete-region beg end)
      (reftex-index def-char full-entry def-tag (if repeat-word sel nil)))))

(defun reftex-index (&optional char key tag postfix no-insert)
  "Query for an index macro and insert it along with its argments.
The index macros available are those defined in `reftex-index-macro' or
by a call to `reftex-add-index-macros', typically from an AUCTeX style file.
RefteX provides completion for the index tag and the index key, and
will prompt for other arguments."

  (interactive)

  ;; Ensure access to scanning info
  (reftex-ensure-index-support t)
  (reftex-access-scan-info current-prefix-arg)

  ;; Find out which macro we are going to use
  (let* ((char (or char
		   (reftex-select-with-char reftex-query-index-macro-prompt
					    reftex-query-index-macro-help)))
	 (macro (nth 1 (assoc char reftex-key-to-index-macro-alist)))
	 (entry (or (assoc macro reftex-index-macro-alist)
		    (error "No index macro associated with %c" char)))
	 (ntag (nth 1 entry))
	 (tag (or tag (nth 1 entry)))
	 (nargs (nth 4 entry))
	 (nindex (nth 5 entry))
	 (opt-args (nth 6 entry))
	 opt tag1 value)

    ;; Get the supported arguments
    (if (stringp tag)
	(setq tag1 tag)
      (setq tag1 (or (reftex-index-complete-tag tag opt-args) "")))
    (setq key (or key
		  (reftex-index-complete-key
		   (if (string= tag1 "") "idx" tag1)
		   (member nindex opt-args))))

    ;; Insert the macro and ask for any additional args
    (insert macro)
    (loop for i from 1 to nargs do
      (setq opt (member i opt-args)
	    value (cond ((= nindex i) key)
			((equal ntag i) tag1)
			(t (read-string (concat "Macro arg nr. "
						(int-to-string i)
						(if opt " (optional)" "")
						": ")))))
      (unless (and opt (string= value ""))
	(insert (if opt "[" "{") value (if opt "]" "}"))))
    (and (stringp postfix) (insert postfix))
    (and key reftex-plug-into-AUCTeX (fboundp 'LaTeX-add-index-entries)
	 (LaTeX-add-index-entries key))
    (reftex-index-update-taglist tag1)
    (reftex-notice-new)))

(defun reftex-default-index ()
  (cond ((null reftex-index-default-tag) nil)
	((stringp reftex-index-default-tag) reftex-index-default-tag)
	(t (or (get reftex-docstruct-symbol 'default-index-tag)
	       "idx"))))

(defun reftex-update-default-index (tag &optional tag-list)
  (if (and (not (equal tag ""))
	   (stringp tag)
	   (eq reftex-index-default-tag 'last)
	   (or (null tag-list)
	       (member tag tag-list)))
      (put reftex-docstruct-symbol 'default-index-tag tag)))

(defun reftex-index-complete-tag (&optional itag opt-args)
  ;; Ask the user for a tag, completing on known tags.
  ;; ITAG is the argument number which contains the tag.
  ;; OPT-ARGS is a list of optional argument indices, as given by
  ;; `reftex-parse-args'.
  (let* ((opt (and (integerp itag) (member itag opt-args)))
	 (index-tags (cdr (assq 'index-tags 
				(symbol-value reftex-docstruct-symbol))))
	 (default (reftex-default-index))
	 (prompt (concat "Index tag"
			 (if default (format " (default: %s)" default) "")
			 (if opt " (optional)" "") ": "))
	 (tag (completing-read prompt (mapcar 'list index-tags))))
    (if (and default (equal tag "")) (setq tag default))
    (reftex-update-default-index tag)
    tag))

(defun reftex-index-select-tag ()
  ;; Have the user select an index tag.
  ;; FIXME: should we cache tag-alist, prompt and help?
  (let* ((index-tags (cdr (assoc 'index-tags 
				 (symbol-value reftex-docstruct-symbol))))
	 (default (reftex-default-index)))
    (cond 
     ((null index-tags)
      (error "No index tags available"))

     ((= (length index-tags) 1)
      ;; Just one index, use it
      (car index-tags))
	  
     ((> (length index-tags) 1)
      ;; Several indices, ask.
      (let* ((tags (copy-sequence index-tags))
	     (cnt 0)
	     tag-alist i val len tag prompt help rpl)
	;; Move idx and glo up in the list to ensure ?i and ?g shortcuts
	(if (member "glo" tags)
	    (setq tags (cons "glo" (delete "glo" tags))))
	(if (member "idx" tags)
	    (setq tags (cons "idx" (delete "idx" tags))))
	;; Find unique shortcuts for each index.
	(while (setq tag (pop tags))
	  (setq len (length tag)
		i -1
		val nil)
	  (catch 'exit
	    (while (and (< (incf i) len) (null val))
	      (unless (assq (aref tag i) tag-alist)
		(push (list (aref tag i)
			    tag
			    (concat (substring tag 0 i) 
				    "[" (substring tag i (incf i)) "]"
				    (substring tag i)))
		      tag-alist)
		(throw 'exit t)))
	    (push (list (+ ?0 (incf cnt)) tag 
			(concat "[" (int-to-string cnt) "]:" tag))
		  tag-alist)))
	(setq tag-alist (nreverse tag-alist))
	;; Compute Prompt and Help strings
	(setq prompt
	      (concat
	       (format "Select Index%s: "
		       (if default (format " (Default <%s>)" default) ""))
	       (mapconcat (lambda(x) (nth 2 x)) tag-alist "  ")))
	(setq help
	      (concat "Select an Index\n===============\n"
		      (if default
			  (format "[^M]  %s (the default)\n" default)
			"")
		      (mapconcat (lambda(x) 
				   (apply 'format "[%c]   %s" x))
				 tag-alist "\n")))
	;; Query the user for an index-tag
	(setq rpl (reftex-select-with-char prompt help 3 t))
	(message "")
	(if (and default (equal rpl ?\C-m))
	    default
	  (if (assq rpl tag-alist)
	      (progn
		(reftex-update-default-index (nth 1 (assq rpl tag-alist)))
		(nth 1 (assq rpl tag-alist)))
	    (error "No index tag associated with %c" rpl)))))
     (t (error "This should not happen (reftex-index-select-tag)")))))

(defun reftex-index-complete-key (&optional tag optional initial)
  ;; Read an index key, with completion.
  ;; Restrict completion table on index tag TAG.
  ;; OPTIONAL indicates if the arg is optional.
  (let* ((table (reftex-sublist-nth
		 (symbol-value reftex-docstruct-symbol) 6
		 (lambda(x) (and (eq (car x) 'index)
				 (string= (nth 1 x) (or tag ""))))
		 t))
	 (prompt (concat "Index key" (if optional " (optional)" "") ": "))
	 (key (completing-read prompt table nil nil initial)))
    key))

(defun reftex-index-update-taglist (newtag)
  ;; add NEWTAG to the list of available index tags. 
  (let ((cell (assoc 'index-tags (symbol-value reftex-docstruct-symbol))))
    (and newtag (cdr cell) (not (member newtag (cdr cell)))
	 (push newtag (cdr cell)))))

(defvar reftex-last-index-file)
(defun reftex-index-globally (&optional data call-file)
  "Index a word with a global search and replace.
This works very much like `reftex-query-replace-document', but the
defaults for the search and replace strings are derived from
local context.
When there is an index entry, we try to index similar words.  The word
to search for is either a word in direct contact with the index macro
(like `\\index{WORD}WORD' or `WORD\\index{WORD}') or the index key.
The replacement text is the index macro with all its arguments and the
attached word.
When there is no index entry at point, we search for the word near point
and propose to index it like this: `\\index{word}word'.
You get a chance to edit the search and replacement strings.
DATA can be a docstruct entry describing an index entry, and then the 
defaults will be derived from it.
CALL-FILE may be the file from where to call the global search command."
  (interactive)
  (let* ((call-file (cond (call-file call-file)
			  (reftex-mode (buffer-file-name))
			  ((eq major-mode 'reftex-index-mode)
			   reftex-last-index-file)
			  (t (error "Need a call file here"))))
	 (pos (point))
	 (data (cond 
		(data data)
		((and reftex-mode
		      (save-excursion
			(forward-char 20)
			(re-search-backward reftex-everything-regexp nil t)
			(< (count-lines (min pos (point)) (max pos (point)))
			   2)))
		 (reftex-index-info (buffer-file-name)))
		(t nil)))
	 (ksep (car reftex-index-special-chars))
	 (words-include-escapes t)
	 (case-replace nil)
	 (case-fold-search t)
	 word rpl start analyze-list pre key attr actual post)

    ;; Find the word and construct the replacement string
    (if (and data (eq (car data) 'index))
        ;; OK, we have an index entry
	(progn
	  (setq analyze-list (reftex-index-analyze-entry data)
		pre (car analyze-list)
		key (nth 1 analyze-list)
		attr (nth 2 analyze-list)
		actual (nth 3 analyze-list)
		post (nth 4 analyze-list)) 
	  (when (string-match (concat "\\<\\(\\sw+\\)" reftex-index-re) pre)
	    (setq word (match-string 1 pre)
		  pre (concat "<<<1>>>" (substring pre (match-end 1)))
		  rpl (concat pre key attr actual post)))	    
	  (when (string-match "}\\(\\sw+\\)\\>[^}]*\\'" post)
	    (setq word (match-string 1 post)
		  post (concat (substring post 0 (match-beginning 1))
			       "<<<1>>>")
		  rpl (concat pre key attr actual post)))
	  (when (and (not word) key)
	    (if (string-match (concat ".*" (regexp-quote ksep)) key)
		(setq word (substring key (match-end 0)))
	      (setq word key))
	    (setq rpl (concat pre key attr actual post))))
      ;; No index entry, just use local word.
      (setq word (save-excursion
		   (buffer-substring-no-properties
		    (progn (skip-syntax-backward "w") (point))
		    (progn (skip-syntax-forward  "w") (point))))
	    rpl (concat "\\index{" word "}<<<1>>>")))
    ;; Quote what is necessary
    (setq word (regexp-quote (downcase word)))
    (setq start 0)
    (while (setq start (string-match "\\\\" rpl start))
      (setq rpl (replace-match "\\\\" t t rpl)
	    start (+ 2 start)))
    ;; We used <<<1>>> instead of \1 to avoid the quoting.  Fix this now.
    (if (string-match "<<<1>>>" rpl)
	(setq rpl (replace-match "\\1" t t rpl)))

    ;; Give the user a chance to edit the strings
    (setq word (read-string "Search: " 
			    (if word (format "\\<\\(%s\\)\\>" word)))
	  rpl (read-string "Replace with: " rpl))

    ;; Execute the command
    (save-excursion
      (switch-to-buffer (get-file-buffer call-file))
      (condition-case nil
	  (reftex-query-replace-document word rpl)
	(error nil)))))

(defvar reftex-index-map (make-sparse-keymap)
  "Keymap used for *Index* buffers.")

(defvar reftex-index-menu)

(defvar reftex-last-index-file nil
  "Stores the file name from which `reftex-display-index' was called.")
(defvar reftex-index-tag nil
  "Stores the tag of the index in an index buffer.")

(defvar reftex-index-return-marker (make-marker)
  "Marker which makes it possible to return from index to old position.")

(defvar reftex-index-restriction-indicator nil)
(defvar reftex-index-restriction-data nil)

(defun reftex-index-mode ()
  "Major mode for managing Index buffers for LaTeX files.
This buffer was created with RefTeX.
Press `?' for a summary of important key bindings, or check the menu.

Here are all local bindings.

\\{reftex-index-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'reftex-index-mode
	mode-name "RefTeX Index")
  (use-local-map reftex-index-map)
  (set (make-local-variable 'revert-buffer-function) 'reftex-index-revert)
  (set (make-local-variable 'reftex-index-restriction-data) nil)
  (set (make-local-variable 'reftex-index-restriction-indicator) nil)
  (setq mode-line-format
	(list "----  " 'mode-line-buffer-identification
	      "   " 'global-mode-string
	      "  R<" 'reftex-index-restriction-indicator ">"
	      " -%-"))
  (setq truncate-lines t)
  (make-local-hook 'post-command-hook)
  (make-local-hook 'pre-command-hook)
  (make-local-variable 'reftex-last-follow-point)
  (easy-menu-add reftex-index-menu reftex-index-map)
  (add-hook 'post-command-hook 'reftex-index-post-command-hook nil t)
  (add-hook 'pre-command-hook  'reftex-index-pre-command-hook nil t)
  (run-hooks 'reftex-index-mode-hook))

(defconst reftex-index-help
"                      AVAILABLE KEYS IN INDEX BUFFER
                      ==============================
! A..Z     Goto the section of entries starting with this letter.
n / p      next-entry / previous-entry
SPC / TAB  Show/Goto the corresponding entry in the LaTeX document.
RET        Goto the entry and hide the *Index* window (also on mouse-2).
q / k      Hide/Kill *Index* buffer.
C-c =      Switch to the TOC buffer.
f / c      Toggle follow mode             / Toggle display of [c]ontext.
g          Refresh *Index* buffer.
r / C-u r  Reparse the LaTeX document     / Reparse entire LaTeX document.
s          Switch to a different index (for documents with multiple indices).
e / C-k    Edit/Kill the entry.
* | @      Edit specific part of entry: [*]key [|]attribute [@]visual
           With prefix: kill that part.
( )        Toggle entry's beginning/end of page range property.
_ ^        Add/Remove parent key (to make this item a subitem).
&          Index the same word everywhere in the document.
} / {      Restrict Index to a single document section / Widen.
< / >      When restricted, move restriction to previous/next section.")

(defun reftex-index-show-entry (data &optional no-revisit)
  ;; Find an index entry associated with DATA and display it highlighted
  ;; in another window.  NO-REVISIT means we are not allowed to visit
  ;; files for this.
  ;; Note:  This function just looks for the nearest match of the
  ;; context string and may fail if the entry moved and an identical
  ;; entry is close to the old position.  Frequent rescans make this
  ;; safer. 
  (let* ((file (nth 3 data))
	 (literal (nth 2 data))
	 (pos (nth 4 data))
	 (re (regexp-quote literal))
	 (match
	  (cond
	   ((or (not no-revisit)
		(reftex-get-buffer-visiting file))
	    (switch-to-buffer-other-window
	     (reftex-get-file-buffer-force file nil))
	    (goto-char (or pos (point-min)))
	    (or (looking-at re)
		(reftex-nearest-match re (length literal))))
	   (t (message reftex-no-follow-message) nil))))
    (when match
      (goto-char (match-beginning 0))
      (recenter '(4))
      (reftex-highlight 0 (match-beginning 0) (match-end 0) (current-buffer)))
    match))

(defun reftex-display-index (&optional tag overriding-restriction
				       &rest locations)
  "Display a buffer with an index compiled from the current document.
When the document has multiple indices, first prompts for the correct one.
When index support is turned off, offer to turn it on.
With one or two `C-u' prefixes, rescan document first.
With prefix 2, restrict index to current document section.
With prefix 3, restrict index to region."

  (interactive)

  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4).
  (let ((current-prefix-arg current-prefix-arg))
    (reftex-ensure-index-support t)
    (reftex-access-scan-info current-prefix-arg))

  (set-marker reftex-index-return-marker (point))
  (setq reftex-last-follow-point 1)

  ;; Determine the correct index to process
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
	 (docstruct-symbol reftex-docstruct-symbol)
	 (index-tag (or tag (reftex-index-select-tag)))
	 (master (reftex-TeX-master-file))
	 (calling-file (buffer-file-name))
	 (restriction
	  (or overriding-restriction
	      (and (interactive-p) 
		   (reftex-get-restriction current-prefix-arg docstruct))))
	 (locations
	  ;; See if we are on an index macro as initial position
	  (or locations
	      (let* ((what-macro (reftex-what-macro-safe 1))
		     (macro (car what-macro))
		     (here-I-am (when (member macro reftex-macros-with-index)
				  (save-excursion
				    (goto-char (+ (cdr what-macro) 
						  (length macro)))
				    (reftex-move-over-touching-args)
				    (reftex-where-am-I)))))
		(if (eq (car (car here-I-am)) 'index)
		    (list (car here-I-am))))))
	 buffer-name)

    (setq buffer-name (reftex-make-index-buffer-name index-tag))

    ;; Goto the buffer and put it into the correct mode
		      
    (when (or restriction current-prefix-arg)
	 (reftex-kill-buffer buffer-name))

    (if (get-buffer-window buffer-name)
	(select-window (get-buffer-window buffer-name))
      (let ((default-major-mode 'reftex-index-mode))
	(switch-to-buffer buffer-name)))

    (or (eq major-mode 'reftex-index-mode) (reftex-index-mode))

    ;; If the buffer is currently restricted, empty it to force update.
    (when reftex-index-restriction-data
      (reftex-erase-buffer))
    (set (make-local-variable 'reftex-last-index-file) calling-file)
    (set (make-local-variable 'reftex-index-tag) index-tag)
    (set (make-local-variable 'reftex-docstruct-symbol) docstruct-symbol)
    (if restriction
	(setq reftex-index-restriction-indicator (car restriction)
	      reftex-index-restriction-data (cdr restriction))
      (if (interactive-p)
	  (setq reftex-index-restriction-indicator nil
		reftex-index-restriction-data nil)))
    (when (= (buffer-size) 0)
      ;; buffer is empty - fill it
      (message "Building %s buffer..." buffer-name)

      (setq buffer-read-only nil)
      (insert (format
"INDEX <%s> on %s
Restriction: <%s>
SPC=view TAB=goto RET=goto+hide [e]dit [q]uit [r]escan [f]ollow [?]Help
------------------------------------------------------------------------------
" index-tag (abbreviate-file-name master)
(if (eq (car (car reftex-index-restriction-data)) 'toc)
    (nth 2 (car reftex-index-restriction-data))
  reftex-index-restriction-indicator)))

      (if (reftex-use-fonts)
          (put-text-property 1 (point) 'face reftex-index-header-face))
      (put-text-property 1 (point) 'intangible t)

      (reftex-insert-index docstruct index-tag)
      (goto-char (point-min))
      (run-hooks 'reftex-display-copied-context-hook)
      (message "Building %s buffer...done." buffer-name)
      (setq buffer-read-only t))
    (and locations (apply 'reftex-find-start-point (point) locations))
    (if reftex-index-restriction-indicator
        (message "Index restricted: <%s>" reftex-index-restriction-indicator))))

(defun reftex-insert-index (docstruct tag &optional update-one remark)
  ;; Insert an index into the current buffer.  Entries are from the
  ;; DOCSTRUCT.
  ;; TAG is the subindex to process.
  ;; UPDATE-ONE: When non-nil, delete the entry at point and replace
  ;; it with whatever the DOCSTRUCT contains.
  ;; REMARK can be a note to add to the entry.
  (let* ((all docstruct)
	 (indent "   ")
	 (context reftex-index-include-context)
	 (context-indent (concat indent "  "))
	 (section-chars (mapcar 'identity reftex-index-section-letters))
	 (this-section-char 0)
	 (font (reftex-use-fonts))
	 (bor (car reftex-index-restriction-data))
	 (eor (nth 1 reftex-index-restriction-data))
	 (mouse-face
	  (if (memq reftex-highlight-selection '(mouse both))
	      reftex-mouse-selected-face
	    nil))
	 (index-face (reftex-verified-face reftex-label-face
					   'font-lock-constant-face
					   'font-lock-reference-face))
	 sublist cell from to first-char)

    ;; Make the sublist and sort it
    (when bor
      (setq all (or (memq bor all) all)))

    (while (setq cell (pop all))
      (if (eq cell eor)
	  (setq all nil)
	(and (eq (car cell) 'index)
	     (equal (nth 1 cell) tag)
	     (push cell sublist))))
    (setq sublist (sort (nreverse sublist)
			(lambda (a b) (string< (nth 8 a) (nth 8 b)))))

    (when update-one
      ;; Delete the entry at place
      (and (bolp) (forward-char 1))
      (delete-region (previous-single-property-change (1+ (point)) :data)
		     (or (next-single-property-change (point) :data) 
			 (point-max))))

    ;; Walk through the list and insert all entries
    (while (setq cell (pop sublist))
      (unless update-one
	(setq first-char (upcase (string-to-char (nth 6 cell))))
	(when (and (not (equal first-char this-section-char))
		   (member first-char section-chars))
	  ;; There is a new initial letter, so start a new section
	  (reftex-index-insert-new-letter first-char font)
	  (setq section-chars (delete first-char section-chars)
		this-section-char first-char))
	(when (= this-section-char 0)
	  (setq this-section-char ?!)
	  (reftex-index-insert-new-letter this-section-char font)))

      (setq from (point))
      (insert indent (nth 7 cell))
      (when font
	(setq to (point))
	(put-text-property 
	 (- (point) (length (nth 7 cell))) to
	 'face index-face)
	(goto-char to))

      (when (or remark (nth 9 cell))
	(and (< (current-column) 40)
	     ;; FIXME: maybe this is too slow?
	     (insert (make-string (max (- 40 (current-column)) 0) ?\ )))
	(and (nth 9 cell) (insert "   " (substring (nth 5 cell) (nth 9 cell))))
	(and remark (insert "     " remark)))

      (insert "\n")
      (setq to (point))

      (when context
	(insert context-indent (nth 2 cell) "\n")
	(setq to (point)))
      (put-text-property from to :data cell)
      (when mouse-face
	(put-text-property from (1- to)
			   'mouse-face mouse-face))
      (goto-char to))))


(defun reftex-index-insert-new-letter (letter &optional font)
  ;; Start a new section in the index
  (let ((from (point)))
    (insert "\n" letter letter letter 
	    "-----------------------------------------------------------------")
    (when font
      (put-text-property from (point) 'face reftex-index-section-face))
    (insert "\n")))

(defun reftex-get-restriction (arg docstruct)
  ;; Interprete the prefix ARG and derive index restriction specs.
  (let* ((beg (min (point) (or (condition-case nil (mark) (error nil))
			       (point-max))))
	 (end (max (point) (or (condition-case nil (mark) (error nil))
			       (point-min))))
	 bor eor label here-I-am)
    (cond
     ((eq arg 2)
      (setq here-I-am (car (reftex-where-am-I))
	    bor (if (eq (car here-I-am) 'toc)
		    here-I-am
		  (reftex-last-assoc-before-elt
		   'toc here-I-am docstruct))
	    eor (car (memq (assq 'toc (cdr (memq bor docstruct))) docstruct))
	    label (nth 6 bor)))
     ((eq arg 3)
      (save-excursion
	(setq label "region")
	(goto-char beg)
	(setq bor (car (reftex-where-am-I)))
	(setq bor (nth 1 (memq bor docstruct)))
	(goto-char end)
	(setq eor (nth 1 (memq (car (reftex-where-am-I)) docstruct)))))
     (t nil))
    (if (and label (or bor eor))
	(list label bor eor)
      nil)))

(defun reftex-index-pre-command-hook ()
  ;; Used as pre command hook in *Index* buffer
  (reftex-unhighlight 0)
  (reftex-unhighlight 1))

(defun reftex-index-post-command-hook ()
  ;; Used in the post-command-hook for the *Index* buffer
  (when (get-text-property (point) :data)
    (and (> (point) 1)
	 (not (get-text-property (point) 'intangible))
	 (memq reftex-highlight-selection '(cursor both))
	 (reftex-highlight 1
	   (or (previous-single-property-change (1+ (point)) :data)
	       (point-min))
	   (or (next-single-property-change (point) :data)
	       (point-max)))))
  (if (integerp reftex-index-follow-mode)
      ;; Remove delayed action
      (setq reftex-index-follow-mode t)
    (and reftex-index-follow-mode
	 (not (equal reftex-last-follow-point (point)))
	 ;; Show context in other window
	 (setq reftex-last-follow-point (point))
	 (condition-case nil
	     (reftex-index-visit-location nil (not reftex-revisit-to-follow))
	   (error t)))))

(defun reftex-index-show-help ()
  "Show a summary of special key bindings."
  (interactive)
  (with-output-to-temp-buffer "*RefTeX Help*"
    (princ reftex-index-help))
  (reftex-enlarge-to-fit "*RefTeX Help*" t)
  ;; If follow mode is active, arrange to delay it one command
  (if reftex-index-follow-mode
      (setq reftex-index-follow-mode 1)))

(defun reftex-index-next (&optional arg)
  "Move to next selectable item."
  (interactive "p")
  (setq reftex-callback-fwd t)
  (or (eobp) (forward-char 1))
  (goto-char (or (next-single-property-change (point) :data) 
		 (point)))
  (unless (get-text-property (point) :data)
    (goto-char (or (next-single-property-change (point) :data) 
		   (point)))))
(defun reftex-index-previous (&optional arg)
  "Move to previous selectable item."
  (interactive "p")
  (setq reftex-callback-fwd nil)
  (goto-char (or (previous-single-property-change (point) :data)
		 (point)))
  (unless (get-text-property (point) :data)
    (goto-char (or (previous-single-property-change (point) :data)
		   (point)))))
(defun reftex-index-toggle-follow ()
  "Toggle follow (other window follows with context)."
  (interactive)
  (setq reftex-last-follow-point -1)
  (setq reftex-index-follow-mode (not reftex-index-follow-mode)))
(defun reftex-index-toggle-context ()
  "Toggle inclusion of label context in *Index* buffer.
Label context is only displayed when the labels are there as well."
  (interactive)
  (setq reftex-index-include-context (not reftex-index-include-context))
  (reftex-index-revert))
(defun reftex-index-view-entry ()
  "View document location in other window."
  (interactive)
  (reftex-index-visit-location))
(defun reftex-index-goto-entry-and-hide ()
  "Go to document location in other window.  Hide the *Index* window."
  (interactive)
  (reftex-index-visit-location 'hide))
(defun reftex-index-goto-entry ()
  "Go to document location in other window. *Index* window stays."
  (interactive)
  (reftex-index-visit-location t))
(defun reftex-index-mouse-goto-line-and-hide (ev)
  "Go to document location in other window.  Hide the *Index* window."
  (interactive "e")
  (mouse-set-point ev)
  (reftex-index-visit-location 'hide))
(defun reftex-index-quit ()
  "Hide the *Index* window and do not move point."
  (interactive)
  (or (one-window-p) (delete-window))
  (switch-to-buffer (marker-buffer reftex-index-return-marker))
  (goto-char (or (marker-position reftex-index-return-marker) (point))))
(defun reftex-index-quit-and-kill ()
  "Kill the *Index* buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (or (one-window-p) (delete-window))
  (switch-to-buffer (marker-buffer reftex-index-return-marker))
  (goto-char (or (marker-position reftex-index-return-marker) (point))))
(defun reftex-index-goto-toc (&rest ignore)
  "Switch to the table of contents of the current document.
The function will go to the section where the entry at point was defined."
  (interactive)
  (if (get-text-property (point) :data)
      (reftex-index-goto-entry)
    (switch-to-buffer (marker-buffer reftex-index-return-marker)))
  (delete-other-windows)
  (reftex-toc))
(defun reftex-index-rescan (&rest ignore)
  "Regenerate the *Index* buffer after reparsing file of section at point."
  (interactive)
  (let ((index-tag reftex-index-tag))
    (if (and reftex-enable-partial-scans
	     (null current-prefix-arg))
	(let* ((data (get-text-property (point) :data))
	       (file (nth 3 data))
	       (line (+ (count-lines (point-min) (point)) (if (bolp) 1 0))))
	  (if (not file)
	      (error "Don't know which file to rescan.  Try `C-u r'")
	    (switch-to-buffer (reftex-get-file-buffer-force file))
	    (setq current-prefix-arg '(4))
	    (reftex-display-index index-tag nil line)))
      (reftex-index-Rescan))
    (reftex-kill-temporary-buffers)))
(defun reftex-index-Rescan (&rest ignore)
  "Regenerate the *Index* buffer after reparsing the entire document."
  (interactive)
  (let ((index-tag reftex-index-tag)
	(line (+ (count-lines (point-min) (point)) (if (bolp) 1 0))))
    (switch-to-buffer
     (reftex-get-file-buffer-force reftex-last-index-file))
    (setq current-prefix-arg '(16))
    (reftex-display-index index-tag nil line)))
(defun reftex-index-revert (&rest ignore)
  "Regenerate the *Index* from the internal lists.  No reparsing os done."
  (interactive)
  (let ((buf (current-buffer))
	(index-tag reftex-index-tag)
	(data (get-text-property (point) :data))
	(line (+ (count-lines (point-min) (point)) (if (bolp) 1 0))))
    (switch-to-buffer
     (reftex-get-file-buffer-force reftex-last-index-file))
    (reftex-erase-buffer buf)
    (setq current-prefix-arg nil
	  reftex-last-follow-point 1)
    (reftex-display-index index-tag nil data line)))
(defun reftex-index-switch-index-tag (&rest ignore)
  "Switch to a different index of the same document."
  (interactive)
  (switch-to-buffer
   (reftex-get-file-buffer-force reftex-last-index-file))
  (setq current-prefix-arg nil)
  (reftex-display-index))

(defun reftex-index-restrict-to-section (&optional force)
  "Restrict index to entries defined in same document sect. as entry at point."
  ;; Optional FORCE means, even if point is not on an index entry.
  (interactive)
  (let* ((data (get-text-property (point) :data))
	 (docstruct (symbol-value reftex-docstruct-symbol))
	 bor eor)
    (if (and (not data) force)
	(setq data (assq 'toc docstruct)))
    (when data
      (setq bor (reftex-last-assoc-before-elt 'toc data docstruct)
	    eor (car (memq (assq 'toc (cdr (memq bor docstruct)))
			   docstruct))
	    reftex-index-restriction-data (list bor eor)
	    reftex-index-restriction-indicator (nth 6 bor) )))
  (reftex-index-revert))

(defun reftex-index-widen (&rest ignore)
  "Show the unrestricted index (all entries)."
  (interactive)
  (setq reftex-index-restriction-indicator nil
	reftex-index-restriction-data nil)
  (reftex-index-revert)
  (message "Index widened"))
(defun reftex-index-restriction-forward (&rest ignore)
  "Restrict to previous section.
When index is currently unrestricted, restrict it to a section.
When index is restricted, select the next section as restriction criterion."
  (interactive)
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
	 (bor (nth 1 reftex-index-restriction-data)))
    (if (or (not bor)
	    (not (eq (car bor) 'toc)))
	(reftex-index-restrict-to-section t)
      (setq reftex-index-restriction-indicator (nth 6 bor)
	    reftex-index-restriction-data
	    (list bor 
		  (car (memq (assq 'toc (cdr (memq bor docstruct)))
			     docstruct))))
      (reftex-index-revert))))
(defun reftex-index-restriction-backward (&rest ignore)
  "Restrict to next section.
When index is currently unrestricted, restrict it to a section.
When index is restricted, select the previous section as restriction criterion."
  (interactive)
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
	 (eor (car reftex-index-restriction-data))
	 (bor (reftex-last-assoc-before-elt 'toc eor docstruct t)))
    (if (or (not bor)
	    (not (eq (car bor) 'toc)))
	(reftex-index-restrict-to-section t)
      (setq reftex-index-restriction-indicator (nth 6 bor)
	    reftex-index-restriction-data
	    (list bor eor))
      (reftex-index-revert))))

(defun reftex-index-visit-location (&optional final no-revisit)
  ;; Visit the tex file corresponding to the index entry on the current line.
  ;; If FINAL is t, stay there
  ;; If FINAL is 'hide, hide the *Index* window.
  ;; Otherwise, move cursor back into *Index* window.
  ;; NO-REVISIT means don't visit files, just use live biffers.

  (let* ((data (get-text-property (point) :data))
         (index-window (selected-window))
         show-window show-buffer match)

    (unless data (error "Don't know which index entry to visit"))
    
    (if (eq (car data) 'index)
	(setq match (reftex-index-show-entry data no-revisit)))

    (setq show-window (selected-window)
          show-buffer (current-buffer))

    (unless match
      (select-window index-window)
      (error "Cannot find location"))

    (select-window index-window)

    ;; Use the `final' parameter to decide what to do next
    (cond
     ((eq final t)
      (reftex-unhighlight 0)
      (select-window show-window))
     ((eq final 'hide)
      (reftex-unhighlight 0)
      (or (one-window-p) (delete-window))
      (switch-to-buffer show-buffer))
     (t nil))))

(defun reftex-index-analyze-entry (data)
  ;; This splits the index context so that key, attribute and visual
  ;; values are accessible individually.
  (interactive)
  (let* ((arg (nth 5 data))
	 (context (nth 2 data))
	 (sc reftex-index-special-chars)
	 (boa (if (string-match (regexp-quote (concat "{" arg "}")) context)
		  (1+ (match-beginning 0))
		(error "Something is wrong here")))
	 (eoa (1- (match-end 0)))
	 (boactual (if (string-match (concat "[^" (nth 3 sc) "]" (nth 2 sc))
				     context boa)
		       (1+ (match-beginning 0))
		     eoa))
	 (boattr (if (string-match (concat "[^" (nth 3 sc) "]" (nth 1 sc))
				   context boa)
		     (1+ (match-beginning 0))
		   boactual))
	 (pre (substring context 0 boa))
	 (key (substring context boa boattr))
	 (attr (substring context boattr boactual))
	 (actual (substring context boactual eoa))
	 (post (substring context eoa)))
    (list pre key attr actual post)))

(defun reftex-index-globalize (&optional arg)
  "Globalize the current index entry.
This starts a global search and replace to index the same word
at other places in the document. After this function completes, you 
need to rescan the document with `r' or `C-u r' in order to get the
entries into the index buffer.
Defaults for the search and replace strings are derived from
the current entry.  See the command `reftex-index-globally'."
  (interactive)
  (let* ((data (get-text-property (point) :data))
	 (buf (current-buffer)))
    (unless data
      (error "No index entry at point"))
    (reftex-index-globally data)
    (switch-to-buffer buf)))

(defun reftex-index-edit ()
  "Edit the index entry at point."
  (interactive)
  (let* ((data (get-text-property (point) :data))
	 old new)
    (unless data (error "Don't know which index entry to edit"))
    (reftex-index-view-entry)
    (setq old (nth 2 data) new (read-string "Edit: " old))
    (reftex-index-change-entry new)))

(defun reftex-index-toggle-range-beginning ()
  "Toggle the page range start attribute `|('."
  (interactive)
  (let* ((data (get-text-property (point) :data))
	 (bor (concat (nth 1 reftex-index-special-chars) "("))
	 new analyze attr)
    (unless data (error "Don't know which index entry to edit"))
    (setq analyze (reftex-index-analyze-entry data)
	  attr (nth 2 analyze))
    (setf (nth 2 analyze) (if (string= attr bor) "" bor))
    (setq new (apply 'concat analyze))
    (reftex-index-change-entry 
     new (if (string= (nth 2 analyze) bor)
	     "Entry is now START-OF-PAGE-RANGE"
	   "START-OF-PAGE-RANGE canceled"))))

(defun reftex-index-toggle-range-end ()
  "Toggle the page-range-end attribute `|)'."
  (interactive)
  (let* ((data (get-text-property (point) :data))
	 (eor (concat (nth 1 reftex-index-special-chars) "("))
	 new analyze attr)
    (unless data (error "Don't know which index entry to edit"))
    (setq analyze (reftex-index-analyze-entry data)
	  attr (nth 2 analyze))
    (setf (nth 2 analyze) (if (string= attr eor) "" eor))
    (setq new (apply 'concat analyze))
    (reftex-index-change-entry
     new (if (string= (nth 2 analyze) eor)
	     "Entry is now END-OF-PAGE-RANGE"
	   "END-OF-PAGE-RANGE canceled"))))

(defun reftex-index-edit-key ()
  "Edit the KEY part of the index entry."
  (interactive)
  (reftex-index-edit-part nil 1 "" "Key: " t))

(defun reftex-index-edit-attribute (&optional arg)
  "EDIT the ATTRIBUTE part of the entry.  With arg: remove entire ATTRIBUTE."
  (interactive "P")
  (reftex-index-edit-part arg 2 (nth 1 reftex-index-special-chars)
			  "Attribute: "))

(defun reftex-index-edit-visual (&optional arg)
  "EDIT the VISUAL part of the entry.  With arg: remove entire VISUAL string."
  (interactive "P")
  (reftex-index-edit-part arg 3 (nth 2 reftex-index-special-chars) "Visual: "))

(defun reftex-index-edit-part (arg n initial prompt &optional dont-allow-empty)
  ;; This function does the work for all partial editing commands
  (let* ((data (get-text-property (point) :data))
	 new analyze opart npart)
    (unless data (error "Don't know which index entry to edit"))
    ;; Analyze the whole context string
    (setq analyze (reftex-index-analyze-entry data)
	  opart (nth n analyze))
    (and (> (length opart) 0) (setq opart (substring opart 1)))
    ;; Have the user editing the part
    (setq npart (if arg "" (read-string (concat prompt initial) opart)))
    ;; Tests:
    (cond ((string= npart opart)
	   (error "Not changed"))
	  ((string= npart "")
	   (if dont-allow-empty
	       (error "Illegal value")
	     (setf (nth n analyze) npart)))
	  (t (setf (nth n analyze) (concat initial npart))))
    (setq new (apply 'concat analyze))
    ;; Change the entry and insert the changed version into the index.
    (reftex-index-change-entry 
     new (if (string= npart "")
	     (format "Deleted: %s" opart)
	   (format "New value is: %s" npart)))))

(defun reftex-index-level-down ()
  "Make index entry a subitem of another entry."
  (interactive)
  (let* ((data (get-text-property (point) :data))
	 (docstruct (symbol-value reftex-docstruct-symbol))
	 old new prefix key)
    (unless data (error "Don't know which index entry to change"))
    (setq old (nth 2 data)
	  key (nth 6 data)
	  prefix (completing-read 
		  "Prefix: " 
		  (reftex-sublist-nth 
		   docstruct 6
		   (lambda (x)
		     (and (eq (car x) 'index)
			  (string= (nth 1 x) reftex-index-tag))) t)))
    (unless (string-match 
	     (concat (regexp-quote (car reftex-index-special-chars)) "\\'")
	     prefix)
      (setq prefix (concat prefix (car reftex-index-special-chars))))
    (if (string-match (regexp-quote key) old)
	(setq new (replace-match (concat prefix key) t t old))
      (error "Cannot construct new index key"))
    (reftex-index-change-entry new (format "Added prefix: %s" prefix))))

(defun reftex-index-level-up ()
  "Remove the highest level of a hierarchical index entry."
  (interactive)
  (let* ((data (get-text-property (point) :data))
	 old new prefix)
    (unless data (error "Don't know which entry to change"))
    (setq old (nth 2 data))
    (if (string-match (concat "{\\([^" (nth 0 reftex-index-special-chars) "]*"
			      "[^" (nth 3 reftex-index-special-chars) "]"
			      (regexp-quote (nth 0 reftex-index-special-chars))
			      "\\)")
		      old)
	(setq prefix (substring old (match-beginning 1) (match-end 1))
	      new (concat (substring old 0 (match-beginning 1))
			  (substring old (match-end 1))))
      (error "Entry is not a subitem"))
    (reftex-index-change-entry new (format "Removed prefix: %s" prefix))))

(defun reftex-index-kill ()
  "FIXME: Not yet implemented"
  (interactive)
  (error "This function is currently not implemented"))

(defun reftex-index-undo ()
  "FIXME: Not yet implemented"
  (interactive)
  (error "This function is currently not implemented"))

(defun reftex-index-change-entry (new &optional message)
  ;; Change the full context string of the index entry at point to
  ;; NEW.  This actually edits the buffer where the entry is defined.
  
  (let* ((data (get-text-property (point) :data))
	 old beg end info)
    (unless data (error "Cannot change entry"))
    (reftex-index-view-entry)
    (setq beg (match-beginning 0) end (match-end 0))
    (setq old (nth 2 data))
    (and (equal old new) (error "Entry unchanged"))
    (save-excursion
      (set-buffer (get-file-buffer (nth 3 data)))
      (goto-char beg)
      (unless (looking-at (regexp-quote old))
	(error "This should not happen (reftex-index-change-entry)"))
      (delete-region beg end)
      (insert new)
      (goto-char (1- beg))
      (when (and (re-search-forward (reftex-everything-regexp) nil t)
		 (match-end 10)
		 (< (abs (- (match-beginning 10) beg)) (length new))
		 (setq info (reftex-index-info-safe buffer-file-name)))
	(setcdr data (cdr info))))
    (let ((buffer-read-only nil))
      (save-excursion
	(reftex-insert-index (list data) reftex-index-tag t
			     "EDITED")))
    (setq reftex-last-follow-point 1)
    (and message (message message))))

;; Index map
(define-key reftex-index-map (if (featurep 'xemacs) [(button2)] [(mouse-2)])
  'reftex-index-mouse-goto-line-and-hide)

(substitute-key-definition
 'next-line 'reftex-index-next reftex-index-map global-map)
(substitute-key-definition
 'previous-line 'reftex-index-previous reftex-index-map global-map)

(loop for x in
      '(("n"    . reftex-index-next)
	("p"    . reftex-index-previous)
	("?"    . reftex-index-show-help)
	(" "    . reftex-index-view-entry)
	("\C-m" . reftex-index-goto-entry-and-hide)
	("\C-i" . reftex-index-goto-entry)
	("\C-k" . reftex-index-kill)
	("r"    . reftex-index-rescan)
	("R"    . reftex-index-Rescan)
	("g"    . revert-buffer)
	("q"    . reftex-index-quit)
	("k"    . reftex-index-quit-and-kill)
	("f"    . reftex-index-toggle-follow)
	("s"    . reftex-index-switch-index-tag)
	("e"    . reftex-index-edit)
	("^"    . reftex-index-level-up)
	("_"    . reftex-index-level-down)
	("}"    . reftex-index-restrict-to-section)
	("{"    . reftex-index-widen)
	(">"    . reftex-index-restriction-forward)
	("<"    . reftex-index-restriction-backward)
	("("    . reftex-index-toggle-range-beginning)
	(")"    . reftex-index-toggle-range-end)
	("|"    . reftex-index-edit-attribute)
	("@"    . reftex-index-edit-visual)
	("*"    . reftex-index-edit-key)
	("&"    . reftex-index-globalize)
	("\C-c=". reftex-index-goto-toc)
	("c"    . reftex-index-toggle-context))
      do (define-key reftex-index-map (car x) (cdr x)))

(loop for key across "0123456789" do
      (define-key reftex-index-map (vector (list key)) 'digit-argument))
(define-key reftex-index-map "-" 'negative-argument)

;; The capital letters and the exclamation mark
(loop for key across (concat "!" reftex-index-section-letters) do
      (define-key reftex-index-map (vector (list key))
	(list 'lambda '() '(interactive)
	      (list 'reftex-index-goto-letter key))))

(defun reftex-index-goto-letter (char)
  "Go to the CHAR section in the index."
  (let ((pos (point))
	(case-fold-search nil))
    (goto-line 3)
    (if (re-search-forward (concat "^" (char-to-string char)) nil t)
	(progn
	  (beginning-of-line)
	  (recenter 0)
	  (reftex-index-next))
      (goto-char pos)
      (error "This <%s> index does not contain entries starting with `%c'" 
	     reftex-index-tag char))))

(easy-menu-define 
 reftex-index-menu reftex-index-map
 "Menu for Index buffer"
 `("Index"
   ["Goto section A-Z" 
    (message "To go to a section, just press any of: !%s"
	     reftex-index-section-letters) t]
   ["Show Entry" reftex-index-view-entry t]
   ["Go To Entry" reftex-index-goto-entry t]
   ["Exit & Go To Entry" reftex-index-goto-entry-and-hide t]
   ["Table of Contents" reftex-index-goto-toc t]
   ["Quit" reftex-index-quit t]
   "--"
   ("Update"
    ["Rebuilt *Index* Buffer" revert-buffer t]
    "--"
    ["Rescan One File" reftex-index-rescan reftex-enable-partial-scans]
    ["Rescan Entire Document" reftex-index-Rescan t])
   ("Restrict"
    ["Restrict to section" reftex-index-restrict-to-section t]
    ["Widen" reftex-index-widen reftex-index-restriction-indicator]
    ["Next Section" reftex-index-restriction-forward
     reftex-index-restriction-indicator]
    ["Previous Section" reftex-index-restriction-backward
     reftex-index-restriction-indicator])
   ("Edit"
    ["Edit Entry" reftex-index-edit t]
    ["Edit Key" reftex-index-edit-key t]
    ["Edit Attribute" reftex-index-edit-attribute t]
    ["Edit Visual" reftex-index-edit-visual t]
    "--"
    ["Add Parentkey" reftex-index-level-down t]
    ["Remove Parentkey " reftex-index-level-up t]
    "--"
    ["Make Start-of-Range" reftex-index-toggle-range-beginning t]
    ["Make End-of-Range" reftex-index-toggle-range-end t]
    "--"
    ["Globalize"  reftex-index-globalize t]
    ["Kill Entry" reftex-index-kill nil]
    "--"
    ["Undo" reftex-index-undo nil])
   ("Options"
    ["Context" reftex-index-toggle-context :style toggle
     :selected reftex-index-include-context]
    "--"
    ["Follow Mode" reftex-index-toggle-follow :style toggle 
     :selected reftex-index-follow-mode])
   "--"
   ["Help" reftex-index-show-help t]))

;;; reftex-index.el ends here
