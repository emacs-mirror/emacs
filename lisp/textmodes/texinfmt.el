;;; texinfmt.el --- convert Texinfo files to Info files.

;; Copyright (C) 1985, 1986, 1988, 1990 Free Software Foundation, Inc.

;; Author: Robert J. Chassell <bob@gnu.ai.mit.edu>
;; Version: 2.00
;; Keywords: tex, help

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

;; Updated May 1990 to correspond, more or less, to version 2.8 of
;; texinfo.tex.  NOTE: texinfmt.el is being phased out; it is being
;; replaced by makeinfo.c, which is faster and provides better error
;; checking.  
;; Robert J. Chassell, bob@ai.mit.edu

;;; Code:

(defvar texinfo-format-syntax-table nil)

(defvar texinfo-vindex)
(defvar texinfo-findex)
(defvar texinfo-cindex)
(defvar texinfo-pindex)
(defvar texinfo-tindex)
(defvar texinfo-kindex)
(defvar texinfo-last-node)
(defvar texinfo-node-names)

(if texinfo-format-syntax-table
    nil
  (setq texinfo-format-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" " " texinfo-format-syntax-table)
  (modify-syntax-entry ?\\ " " texinfo-format-syntax-table)
  (modify-syntax-entry ?@ "\\" texinfo-format-syntax-table)
  (modify-syntax-entry ?\^q "\\" texinfo-format-syntax-table)
  (modify-syntax-entry ?\[ "." texinfo-format-syntax-table)
  (modify-syntax-entry ?\] "." texinfo-format-syntax-table)
  (modify-syntax-entry ?\( "." texinfo-format-syntax-table)
  (modify-syntax-entry ?\) "." texinfo-format-syntax-table)
  (modify-syntax-entry ?{ "(}" texinfo-format-syntax-table)
  (modify-syntax-entry ?} "){" texinfo-format-syntax-table)
  (modify-syntax-entry ?\' "." texinfo-format-syntax-table))

;;;###autoload
(defun texinfo-format-buffer (&optional notagify)
  "Process the current buffer as texinfo code, into an Info file.
The Info file output is generated in a buffer visiting the Info file
names specified in the @setfilename command.

Non-nil argument (prefix, if interactive) means don't make tag table
and don't split the file if large.  You can use Info-tagify and
Info-split to do these manually."
  (interactive "P")
  (let ((lastmessage "Formatting Info file..."))
    (message lastmessage)
    (texinfo-format-buffer-1)
    (if notagify
        nil
      (if (> (buffer-size) 30000)
          (progn
            (message (setq lastmessage "Making tags table for Info file..."))
            (Info-tagify)))
      (if (> (buffer-size) 100000)
          (progn
            (message (setq lastmessage "Splitting Info file..."))
            (Info-split))))
    (message (concat lastmessage
                     (if (interactive-p) "done.  Now save it." "done.")))))


(defun texinfo-format-buffer-1 ()
  (let (texinfo-format-filename
        texinfo-example-start
        texinfo-command-start
        texinfo-command-end
        texinfo-command-name
        texinfo-last-node
        texinfo-vindex
        texinfo-findex
        texinfo-cindex
        texinfo-pindex
        texinfo-tindex
        texinfo-kindex
        texinfo-stack
        texinfo-node-names
	(texinfo-footnote-number 0)
        last-input-buffer
        outfile
        (fill-column fill-column)
        (input-buffer (current-buffer))
        (input-directory default-directory))
    (save-excursion
      (goto-char (point-min))
      (search-forward "@setfilename")
      (setq texinfo-command-end (point))
      (setq outfile (texinfo-parse-line-arg)))
    (find-file outfile)
    (texinfo-mode)
    (set-syntax-table texinfo-format-syntax-table)
    (erase-buffer)
    (insert-buffer-substring input-buffer)
    (goto-char (point-min))
    (search-forward "@setfilename")
    (beginning-of-line)
    (delete-region (point-min) (point))
    ;; Remove @bye at end of file, if it is there.
    (goto-char (point-max))
    (if (search-backward "@bye" nil t)
        (delete-region (point) (point-max)))
    ;; Make sure buffer ends in a newline.
    (or (= (preceding-char) ?\n)
        (insert "\n"))
    ;; Scan the whole buffer, converting to Info format.
    (texinfo-format-scan)
    ;; Return data for indices.
    (goto-char (point-min))
    (list outfile
          texinfo-vindex texinfo-findex texinfo-cindex
          texinfo-pindex texinfo-tindex texinfo-kindex)))

(defvar texinfo-region-buffer-name "*Info Region*"
  "*Name of the temporary buffer used by \\[texinfo-format-region].")

;;;###autoload
(defun texinfo-format-region (region-beginning region-ending)
  "Convert the current region of the Texinfo file to Info format.
This lets you see what that part of the file will look like in Info.
The command is bound to \\[texinfo-format-region].  The text that is
converted to Info is stored in a temporary buffer."
  (interactive "r")
  (message "Converting region to Info format...")
  (let (texinfo-command-start
        texinfo-command-end
        texinfo-command-name
        texinfo-vindex
        texinfo-findex
        texinfo-cindex
        texinfo-pindex
        texinfo-tindex
        texinfo-kindex
        texinfo-stack
        texinfo-format-filename
        texinfo-example-start
        texinfo-last-node
        texinfo-node-names
	(texinfo-footnote-number 0)
        last-input-buffer
        (fill-column fill-column)
        (input-buffer (current-buffer))
        (input-directory default-directory)
        filename-beginning
        filename-ending)

;;; Find a buffer to use.

    (switch-to-buffer (get-buffer-create texinfo-region-buffer-name))

    ;; Insert the region into the buffer.
    (erase-buffer)

    (save-excursion
      (set-buffer input-buffer)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;; Initialize the buffer with the filename
          ;; or else explain that a filename is needed.
          (or (search-forward "@setfilename"
                              (save-excursion (forward-line 100) (point)) t)
              (error "The texinfo file needs a line saying: @setfilename <name>"))
          (beginning-of-line)
          (setq filename-beginning (point))
          (forward-line 1)
          (setq filename-ending (point)))))

    ;; Insert the @setfilename line into the buffer.
    (insert-buffer-substring input-buffer
                             (min filename-beginning region-beginning)  
                             filename-ending)
    
    ;; Insert the region into the buffer.
    (insert-buffer-substring input-buffer
                             (max region-beginning filename-ending)
                             region-ending)

    (texinfo-mode)

    ;; Install a syntax table useful for scanning command operands.
    (set-syntax-table texinfo-format-syntax-table)
    
    ;; If the region includes the effective end of the data,
    ;; discard everything after that.
    (goto-char (point-max))
    (if (re-search-backward "^@bye" nil t)
        (delete-region (point) (point-max)))
    ;; Make sure buffer ends in a newline.
    (or (= (preceding-char) ?\n)
        (insert "\n"))

    ;; Now convert for real.
    (goto-char (point-min))
    (texinfo-format-scan)
    (goto-char (point-min)))

  (message "Done."))


;; Perform those texinfo-to-info conversions that apply to the whole input
;; uniformly.
(defun texinfo-format-scan ()
    ;; Convert left and right quotes to typewriter font quotes.
    (goto-char (point-min))
    (while (search-forward "``" nil t)
      (replace-match "\""))
    (goto-char (point-min))
    (while (search-forward "''" nil t)
      (replace-match "\""))
    ;; Scan for @-commands.
    (goto-char (point-min))
    (while (search-forward "@" nil t)
      (if (looking-at "[@{}'` *]")
          ;; Handle a few special @-followed-by-one-char commands.
          (if (= (following-char) ?*)
              (progn
                ;; remove command
                (delete-region (1- (point)) (1+ (point)))
                ;; insert return if not at end of line;
                ;; else line is already broken.
                (if (not (= (following-char) ?\n))
                    (insert ?\n)))      
            ;; The other characters are simply quoted.  Delete the @.
            (delete-char -1)
            (forward-char 1))
        ;; @ is followed by a command-word; find the end of the word.
        (setq texinfo-command-start (1- (point)))
        (if (= (char-syntax (following-char)) ?w)
            (forward-word 1)
          (forward-char 1))
        (setq texinfo-command-end (point))
        ;; Call the handler for this command.
        (setq texinfo-command-name
              (intern (buffer-substring (1+ texinfo-command-start)
                                        texinfo-command-end)))
        (let ((cmd (get texinfo-command-name 'texinfo-format)))
          (if cmd (funcall cmd)
            (texinfo-unsupported)))))
    (cond (texinfo-stack
           (goto-char (nth 2 (car texinfo-stack)))
           (error "Unterminated @%s" (car (car texinfo-stack))))))

(put 'begin 'texinfo-format 'texinfo-format-begin)
(defun texinfo-format-begin ()
  (texinfo-format-begin-end 'texinfo-format))

(put 'end 'texinfo-format 'texinfo-format-end)
(defun texinfo-format-end ()
  (texinfo-format-begin-end 'texinfo-end))

(defun texinfo-format-begin-end (prop)
  (setq texinfo-command-name (intern (texinfo-parse-line-arg)))
  (setq cmd (get texinfo-command-name prop))
  (if cmd (funcall cmd)
    (texinfo-unsupported)))

(defun texinfo-parse-line-arg ()
  (goto-char texinfo-command-end)
  (let ((start (point)))
    (cond ((looking-at " ")
	   (skip-chars-forward " ")
	   (setq start (point))
	   (end-of-line)
           (skip-chars-backward " ")
	   (setq texinfo-command-end (1+ (point))))
	  ((looking-at "{")
	   (setq start (1+ (point)))
	   (forward-list 1)
	   (setq texinfo-command-end (point))
	   (forward-char -1))
	  (t
	   (error "Invalid texinfo command arg format")))
    (prog1 (buffer-substring start (point))
	   (if (eolp) (forward-char 1)))))

(defun texinfo-parse-expanded-arg ()
  (goto-char texinfo-command-end)
  (let ((start (point))
	marker)
    (cond ((looking-at " ")
	   (skip-chars-forward " ")
	   (setq start (point))
	   (end-of-line)
	   (setq texinfo-command-end (1+ (point))))
	  ((looking-at "{")
	   (setq start (1+ (point)))
	   (forward-list 1)
	   (setq texinfo-command-end (point))
	   (forward-char -1))
	  (t
	   (error "Invalid texinfo command arg format")))
    (setq marker (move-marker (make-marker) texinfo-command-end))
    (texinfo-format-expand-region start (point))
    (setq texinfo-command-end (marker-position marker))
    (move-marker marker nil)
    (prog1 (buffer-substring start (point))
	   (if (eolp) (forward-char 1)))))

(defun texinfo-format-expand-region (start end)
  (save-restriction
    (narrow-to-region start end)
    (let (texinfo-command-start
	  texinfo-command-end
	  texinfo-command-name
	  texinfo-stack)
      (texinfo-format-scan))
    (goto-char (point-max))))

(defun texinfo-parse-arg-discard ()
  (prog1 (texinfo-parse-line-arg)
	 (texinfo-discard-command)))

(defun texinfo-discard-command ()
  (delete-region texinfo-command-start texinfo-command-end))

(defun texinfo-optional-braces-discard ()
  "Discard braces following command, if any."
  (goto-char texinfo-command-end)
  (let ((start (point)))
    (cond ((looking-at "[ \t]*\n"))     ; do nothing
          ((looking-at "{")             ; remove braces, if any
	   (forward-list 1)
	   (setq texinfo-command-end (point)))
	  (t
           (error
            "Invalid `texinfo-optional-braces-discard' format \(need braces?\)")))
    (delete-region texinfo-command-start texinfo-command-end)))

(defun texinfo-format-parse-line-args ()
  (let ((start (1- (point)))
	next beg end
	args)
    (skip-chars-forward " ")
    (while (not (eolp))
      (setq beg (point))
      (re-search-forward "[\n,]")
      (setq next (point))
      (if (bolp) (setq next (1- next)))
      (forward-char -1)
      (skip-chars-backward " ")
      (setq end (point))
      (setq args (cons (if (> end beg) (buffer-substring beg end))
		       args))
      (goto-char next)
      (skip-chars-forward " "))
    (if (eolp) (forward-char 1))
    (setq texinfo-command-end (point))
    (nreverse args)))

(defun texinfo-format-parse-args ()
  (let ((start (1- (point)))
	next beg end
	args)
    (search-forward "{")
    (save-excursion
      (texinfo-format-expand-region 
       (point)
       (save-excursion (up-list 1) (1- (point)))))
    (while (/= (preceding-char) ?\})
      (skip-chars-forward " \t\n")
      (setq beg (point))
      (re-search-forward "[},]")
      (setq next (point))
      (forward-char -1)
      (skip-chars-backward " \t\n")
      (setq end (point))
      (cond ((< beg end)
	     (goto-char beg)
	     (while (search-forward "\n" end t)
	       (replace-match " "))))
      (setq args (cons (if (> end beg) (buffer-substring beg end))
		       args))
      (goto-char next))
    (if (eolp) (forward-char 1))
    (setq texinfo-command-end (point))
    (nreverse args)))

(defun texinfo-format-parse-defun-args ()
  (goto-char texinfo-command-end)
  (let ((start (point)))
    (end-of-line)
    (setq texinfo-command-end (1+ (point)))
    (let ((marker (move-marker (make-marker) texinfo-command-end)))
      (texinfo-format-expand-region start (point))
      (setq texinfo-command-end (marker-position marker))
      (move-marker marker nil))
    (goto-char start)
    (let ((args '())
	  beg end)
      (skip-chars-forward " ")
      (while (not (eolp))
	(cond ((looking-at "{")
	       (setq beg (1+ (point)))
	       (forward-list 1)
	       (setq end (1- (point))))
	      (t
	       (setq beg (point))
	       (re-search-forward "[\n ]")
	       (forward-char -1)
	       (setq end (point))))
	(setq args (cons (buffer-substring beg end) args))
	(skip-chars-forward " "))
      (forward-char 1)
      (nreverse args))))


; 19 October 1990
; @setfilename modifed to work with include files; see @include
; (defun texinfo-format-setfilename ()
;   (let ((arg (texinfo-parse-arg-discard)))
;     (setq texinfo-format-filename
; 	  (file-name-nondirectory (expand-file-name arg)))
;     (insert "Info file: "
; 	    texinfo-format-filename ",    -*-Text-*-\n"
; 	    "produced by texinfo-format-buffer\nfrom "
; 	    (if (buffer-file-name input-buffer)
; 		(concat "file: "
; 			(file-name-sans-versions
; 			 (file-name-nondirectory
; 			  (buffer-file-name input-buffer))))
; 	      (concat "buffer " (buffer-name input-buffer)))
; 	    "\n\n")))

(put 'setfilename 'texinfo-format 'texinfo-format-setfilename)
(defun texinfo-format-setfilename ()
  (let ((arg (texinfo-parse-arg-discard)))
    (if (eq input-buffer last-input-buffer)
	nil				; only use first setfilename in buffer
      (message "Formatting Info file: %s" arg)
      (setq texinfo-format-filename
	    (file-name-nondirectory (expand-file-name arg)))
      (insert "Info file: "
	      texinfo-format-filename ",    -*-Text-*-\n"
	      "produced by texinfo-format-buffer\nfrom "
	      (if (buffer-file-name input-buffer)
		  (concat "file: "
			  (file-name-sans-versions
			   (file-name-nondirectory
			    (buffer-file-name input-buffer))))
		(concat "buffer " (buffer-name input-buffer)))
	      "\n\n"))))

(put 'node 'texinfo-format 'texinfo-format-node)
(defun texinfo-format-node ()
  (let* ((args (texinfo-format-parse-line-args))
	 (name (nth 0 args))
	 (next (nth 1 args))
	 (prev (nth 2 args))
	 (up (nth 3 args)))
    (texinfo-discard-command)
    (setq texinfo-last-node name)
    (let ((tem (downcase name)))
      (if (assoc tem texinfo-node-names)
	  (error "Duplicate node name: %s" name)
	(setq texinfo-node-names (cons (list tem) texinfo-node-names))))
    (setq texinfo-footnote-number 0)
    (or (bolp)
	(insert ?\n))
    (insert "\^_\nFile: " texinfo-format-filename
	    "  Node: " name)
    (if prev
	(insert ", Prev: " prev))
    (if up
	(insert ", Up: " up))
    (if next
	(insert ", Next: " next))
    (insert ?\n)))

(put 'menu 'texinfo-format 'texinfo-format-menu)
(defun texinfo-format-menu ()
  (texinfo-discard-line)
  (insert "* Menu:\n\n"))

(put 'menu 'texinfo-end 'texinfo-discard-command)
(defun texinfo-discard-line ()
  (goto-char texinfo-command-end)
  (skip-chars-forward " \t")
  (or (eolp)
      (error "Extraneous text at end of command line."))
  (goto-char texinfo-command-start)
  (or (bolp)
      (error "Extraneous text at beginning of command line."))
  (delete-region (point) (progn (forward-line 1) (point))))

; @xref {NODE, FNAME, NAME, FILE, DOCUMENT}
; -> *Note FNAME: (FILE)NODE
;   If FILE is missing,
;    *Note FNAME: NODE
;   If FNAME is empty and NAME is present
;    *Note NAME: Node
;   If both NAME and FNAME are missing
;    *Note NODE::
;   texinfo ignores the DOCUMENT argument.
; -> See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;   If FILE is specified, (FILE)NODE is used for xrefs.
;   If fifth argument DOCUMENT is specified, produces
;    See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;    of DOCUMENT

; @ref             a reference that does not put `See' or `see' in
;                  the hardcopy and is the same as @xref in Info
(put 'ref 'texinfo-format 'texinfo-format-xref)

(put 'xref 'texinfo-format 'texinfo-format-xref)
(defun texinfo-format-xref ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (insert "*Note ")
    (let ((fname (or (nth 1 args) (nth 2 args))))
      (if (null (or fname (nth 3 args)))
	  (insert (car args) "::")
	(insert (or fname (car args)) ": ")
	(if (nth 3 args)
	    (insert "(" (nth 3 args) ")"))
	(insert (car args))))))

(put 'pxref 'texinfo-format 'texinfo-format-pxref)
(defun texinfo-format-pxref ()
  (texinfo-format-xref)
  (or (save-excursion
	(forward-char -2)
	(looking-at "::"))
      (insert ".")))

;@inforef{NODE, FNAME, FILE}
;Like @xref{NODE, FNAME,,FILE} in texinfo.
;In Tex, generates "See Info file FILE, node NODE"
(put 'inforef 'texinfo-format 'texinfo-format-inforef)
(defun texinfo-format-inforef ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (if (nth 1 args)
        (insert "*Note " (nth 1 args) ": (" (nth 2 args) ")" (car args))
      (insert "*Note " "(" (nth 2 args) ")" (car args) "::"))))

(put 'chapheading 'texinfo-format 'texinfo-format-chapter)
(put 'ichapter 'texinfo-format 'texinfo-format-chapter)
(put 'chapter 'texinfo-format 'texinfo-format-chapter)
(put 'iappendix 'texinfo-format 'texinfo-format-chapter)
(put 'appendix 'texinfo-format 'texinfo-format-chapter)
(put 'iunnumbered 'texinfo-format 'texinfo-format-chapter)
(put 'unnumbered 'texinfo-format 'texinfo-format-chapter)
(defun texinfo-format-chapter ()
  (texinfo-format-chapter-1 ?*))

(put 'heading 'texinfo-format 'texinfo-format-section)
(put 'isection 'texinfo-format 'texinfo-format-section)
(put 'section 'texinfo-format 'texinfo-format-section)
(put 'iappendixsection 'texinfo-format 'texinfo-format-section)
(put 'appendixsection 'texinfo-format 'texinfo-format-section)
(put 'iappendixsec 'texinfo-format 'texinfo-format-section)
(put 'appendixsec 'texinfo-format 'texinfo-format-section)
(put 'iunnumberedsec 'texinfo-format 'texinfo-format-section)
(put 'unnumberedsec 'texinfo-format 'texinfo-format-section)
(defun texinfo-format-section ()
  (texinfo-format-chapter-1 ?=))

(put 'subheading 'texinfo-format 'texinfo-format-subsection)
(put 'isubsection 'texinfo-format 'texinfo-format-subsection)
(put 'subsection 'texinfo-format 'texinfo-format-subsection)
(put 'iappendixsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'appendixsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'iunnumberedsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'unnumberedsubsec 'texinfo-format 'texinfo-format-subsection)
(defun texinfo-format-subsection ()
  (texinfo-format-chapter-1 ?-))

(put 'subsubheading 'texinfo-format 'texinfo-format-subsubsection)
(put 'isubsubsection 'texinfo-format 'texinfo-format-subsubsection)
(put 'subsubsection 'texinfo-format 'texinfo-format-subsubsection)
(put 'iappendixsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'appendixsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'iunnumberedsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'unnumberedsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(defun texinfo-format-subsubsection ()
  (texinfo-format-chapter-1 ?.))

(defun texinfo-format-chapter-1 (belowchar)
  (let ((arg (texinfo-parse-arg-discard)))
    (message "Formatting: %s ... " arg)   ; So we can see where we are.
    (insert ?\n arg ?\n "@SectionPAD " belowchar ?\n)
    (forward-line -2)))

(put 'SectionPAD 'texinfo-format 'texinfo-format-sectionpad)
(defun texinfo-format-sectionpad ()
  (let ((str (texinfo-parse-arg-discard)))
    (forward-char -1)
    (let ((column (current-column)))
      (forward-char 1)
      (while (> column 0)
	(insert str)
	(setq column (1- column))))
    (insert ?\n)))

(put '\. 'texinfo-format 'texinfo-format-\.)
(defun texinfo-format-\. ()
  (texinfo-discard-command)
  (insert "."))

(put '\: 'texinfo-format 'texinfo-format-\:)
(defun texinfo-format-\: ()
  (texinfo-discard-command))

(put 'center 'texinfo-format 'texinfo-format-center)
(defun texinfo-format-center ()
  (texinfo-discard-command)
  (let ((indent-tabs-mode nil))
    (center-line)))

(put 'sp 'texinfo-format 'texinfo-format-sp)
(defun texinfo-format-sp ()
  (let* ((arg (texinfo-parse-arg-discard))
	 (num (read arg)))
    (insert-char ?\n num)))

(put 'br 'texinfo-format 'texinfo-format-paragraph-break)
(defun texinfo-format-paragraph-break ()
  "Force a paragraph break.
If used within a line, follow `@br' with braces."
  (texinfo-optional-braces-discard)
  ;; insert one return if at end of line;
  ;; else insert two returns, to generate a blank line.
  (if (= (following-char) ?\n)
      (insert ?\n)
    (insert-char ?\n 2)))


;;; @footnote

; In Texinfo, footnotes are created with the `@footnote' command.
; This command is followed immediately by a left brace, then by the text of
; the footnote, and then by a terminating right brace.  The
; template for a footnote is:
; 
;      @footnote{TEXT}
;
; Info has two footnote styles:
; 
; `End Node'
;      In the "End Node" style, all the footnotes for a single node
;      are placed at the end of that node.  The footnotes are
;      separated from the rest of the node by a line of dashes with
;      the word `Footnotes' within it.
; 
; `Make Node'
;      In the "Make Node" style, all the footnotes for a single node are
;      placed in an automatically constructed node of their own.  

(put 'footnote 'texinfo-format 'texinfo-format-footnote)

(defvar texinfo-footnote-style 'MN "\
*Footnote style, either EN for end node or MN for make node.")

(defvar texinfo-footnote-number)

(defun texinfo-format-footnote ()
  "Format a footnote in either `end node' or `make node' style.
The `texinfo-footnote-style' variable controls which style is used."
  (setq texinfo-footnote-number (1+ texinfo-footnote-number))
  (cond ((eq texinfo-footnote-style 'EN) (texinfo-format-end-node))
        ((eq texinfo-footnote-style 'MN) (texinfo-format-make-node))))

(defun texinfo-format-make-node ()
  "Format footnote in `MN', Make Node, style with notes in own node.
The node is constructed automatically."
  (let* (start
         (arg (texinfo-parse-expanded-arg))
         (node-name-beginning
          (save-excursion
            (re-search-backward
             "^File: \\w+\\(\\w\\|\\s_\\|\\.\\)*[ \t]+Node:")
            (match-end 0)))
         (node-name
          (save-excursion
            (buffer-substring
             (progn (goto-char node-name-beginning) ; skip over node command
                    (skip-chars-forward " \t")  ; and over spaces
                    (point))
             (if (search-forward
                  ","
                  (save-excursion (end-of-line) (point)) t) ; bound search
                 (1- (point))
               (end-of-line) (point))))))
    (texinfo-discard-command)
    (insert (format "(%d) (*note %s-Footnotes::)"
		    texinfo-footnote-number node-name))
    (fill-paragraph nil)
    (save-excursion
    (if (re-search-forward "^@node" nil 'move)
        (forward-line -1))

    ;; two cases: for the first footnote, we must insert a node header;
    ;; for the second and subsequent footnotes, we need only insert 
    ;; the text of the  footnote.

    (if (save-excursion
         (re-search-backward
          (concat node-name "-Footnotes, Up: ")
          node-name-beginning
          t))
        (progn   ; already at least one footnote
          (setq start (point))
          (insert (format "\n(%d)  %s\n" texinfo-footnote-number arg))
          (fill-region start (point)))
      ;; else not yet a footnote
      (insert "\n\^_\nFile: "  texinfo-format-filename
              "  Node: " node-name "-Footnotes, Up: " node-name "\n")
      (setq start (point))
      (insert (format "\n(%d)  %s\n" texinfo-footnote-number arg))
      (fill-region start (point))))))

(defun texinfo-format-end-node ()
  "Format footnote in `EN', End Node, style with notes at end of node."
  (let (start
        (arg (texinfo-parse-expanded-arg)))
    (texinfo-discard-command)
    (insert (format "(%d) " texinfo-footnote-number))
    (fill-paragraph nil)
    (save-excursion
      (if (search-forward "\n--------- Footnotes ---------\n" nil t)
          (progn ; already have footnote, put new one before end of node
            (if (re-search-forward "^@node" nil 'move)
                (forward-line -1))
            (setq start (point))
            (insert (format "\n(%d)  %s\n" texinfo-footnote-number arg))
            (fill-region start (point)))
        ;; else no prior footnote
        (if (re-search-forward "^@node" nil 'move)
            (forward-line -1))
        (insert "\n--------- Footnotes ---------\n")
        (setq start (point))
        (insert (format "\n(%d)  %s\n" texinfo-footnote-number arg))
        (fill-region start (point))))))


;; @itemize pushes (itemize "COMMANDS" STARTPOS) on texinfo-stack.
;; @enumerate pushes (enumerate 0 STARTPOS).
;; @item dispatches to the texinfo-item prop of the first elt of the list.
;; For itemize, this puts in and rescans the COMMANDS.
;; For enumerate, this increments the number and puts it in.
;; In either case, it puts a Backspace at the front of the line
;; which marks it not to be indented later.
;; All other lines get indented by 5 when the @end is reached.

(defun texinfo-push-stack (check arg)
  (setq texinfo-stack
	(cons (list check arg texinfo-command-start)
	      texinfo-stack)))

(defun texinfo-pop-stack (check)
  (if (null texinfo-stack)
      (error "Unmatched @end %s" check))
  (if (not (eq (car (car texinfo-stack)) check))
      (error "@end %s matches @%s"
	     check (car (car texinfo-stack))))
  (prog1 (cdr (car texinfo-stack))
	 (setq texinfo-stack (cdr texinfo-stack))))

(put 'itemize 'texinfo-format 'texinfo-itemize)
(defun texinfo-itemize ()
  (texinfo-push-stack 'itemize (texinfo-parse-arg-discard))
  (setq fill-column (- fill-column 5)))

(put 'itemize 'texinfo-end 'texinfo-end-itemize)
(defun texinfo-end-itemize ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
	 (texinfo-pop-stack 'itemize)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'enumerate 'texinfo-format 'texinfo-enumerate)
(defun texinfo-enumerate ()
  (texinfo-push-stack 'enumerate 0)
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'enumerate 'texinfo-end 'texinfo-end-enumerate)
(defun texinfo-end-enumerate ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
	 (texinfo-pop-stack 'enumerate)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'table 'texinfo-format 'texinfo-table)
(defun texinfo-table ()
  (texinfo-push-stack 'table (texinfo-parse-arg-discard))
  (setq fill-column (- fill-column 5)))

(put 'ftable 'texinfo-format 'texinfo-ftable)
(defun texinfo-ftable ()
  (texinfo-push-stack 'table "@code")
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'description 'texinfo-format 'texinfo-description)
(defun texinfo-description ()
  (texinfo-push-stack 'table "@asis")
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'table 'texinfo-end 'texinfo-end-table)
(put 'ftable 'texinfo-end 'texinfo-end-table)
(put 'description 'texinfo-end 'texinfo-end-table)
(defun texinfo-end-table ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
	 (texinfo-pop-stack 'table)))
    (texinfo-do-itemize (nth 1 stacktop))))

;; At the @end, indent all the lines within the construct
;; except those marked with backspace.  FROM says where
;; construct started.
(defun texinfo-do-itemize (from)
  (save-excursion
   (while (progn (forward-line -1)
		 (>= (point) from))
     (if (= (following-char) ?\b)
	 (save-excursion
	   (delete-char 1)
	   (end-of-line)
	   (delete-char 6))
       (if (not (looking-at "[ \t]*$"))
	   (save-excursion (insert "     ")))))))

(put 'item 'texinfo-format 'texinfo-item)
(put 'itemx 'texinfo-format 'texinfo-item)
(defun texinfo-item ()
  (funcall (get (car (car texinfo-stack)) 'texinfo-item)))

(put 'itemize 'texinfo-item 'texinfo-itemize-item)
(defun texinfo-itemize-item ()
  (texinfo-discard-line)
  (insert "\b   " (nth 1 (car texinfo-stack)) " \n")
  (forward-line -1))

(put 'enumerate 'texinfo-item 'texinfo-enumerate-item)
(defun texinfo-enumerate-item ()
  (texinfo-discard-line)
  (let ((next (1+ (car (cdr (car texinfo-stack))))))
    (setcar (cdr (car texinfo-stack)) next)
    (insert ?\b (format "%3d. " next) ?\n))
  (forward-line -1))

(put 'table 'texinfo-item 'texinfo-table-item)
(defun texinfo-table-item ()
  (let ((arg (texinfo-parse-arg-discard))
	(itemfont (car (cdr (car texinfo-stack)))))
    (insert ?\b itemfont ?\{ arg "}\n     \n"))
  (forward-line -2))


; @ftable

; The `@ftable' command is like the `@table' command but it also
; inserts each item in the first column into the function index.

(put 'ftable 'texinfo-format 'texinfo-ftable)

; The following function presumes that the first column of the table
; should be in `@code' font; but the texinfo.tex source does not
; presume this.  
; (defun texinfo-ftable ()
;   (texinfo-push-stack 'ftable "@code")
;   (setq fill-column (- fill-column 5))
;   (texinfo-discard-line))

(defun texinfo-ftable ()
  (texinfo-push-stack 'ftable (texinfo-parse-arg-discard))
  (setq fill-column (- fill-column 5)))

(put 'ftable 'texinfo-item 'texinfo-ftable-item)
(defun texinfo-ftable-item ()
  (let ((item (texinfo-parse-arg-discard))
        (itemfont (car (cdr (car texinfo-stack))))
        (indexvar 'texinfo-findex))
    (insert ?\b itemfont ?\{ item "}\n     \n")
    (set indexvar
         (cons
          (list item texinfo-last-node)
          (symbol-value indexvar)))
    (forward-line -2)))

(put 'ftable 'texinfo-end 'texinfo-end-ftable)
(defun texinfo-end-ftable ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
         (texinfo-pop-stack 'ftable)))
    (texinfo-do-itemize (nth 1 stacktop))))


(put 'ifinfo 'texinfo-format 'texinfo-discard-line)
(put 'ifinfo 'texinfo-end 'texinfo-discard-command)

(put 'iftex 'texinfo-format 'texinfo-format-iftex)
(defun texinfo-format-iftex ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end iftex\n")
			(point))))

(put 'tex 'texinfo-format 'texinfo-format-tex)
(defun texinfo-format-tex ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "@end tex\n")
			(point))))

(put 'titlepage 'texinfo-format 'texinfo-format-titlepage)
(defun texinfo-format-titlepage ()
  (delete-region texinfo-command-start
		 (progn (search-forward "@end titlepage\n")
			(point))))

(put 'endtitlepage 'texinfo-format 'texinfo-discard-line)

; @titlespec         an alternative titling command; ignored by Info

(put 'titlespec 'texinfo-format 'texinfo-format-titlespec)
(defun texinfo-format-titlespec ()
  (delete-region texinfo-command-start
                 (progn (search-forward "@end titlespec\n")
                        (point))))

(put 'endtitlespec 'texinfo-format 'texinfo-discard-line)

; @today{}

(put 'today 'texinfo-format 'texinfo-format-today)

; Produces Day Month Year style of output.  eg `1 Jan 1900'
; The `@today{}' command requires a pair of braces, like `@dots{}'.
(defun texinfo-format-today ()
  (texinfo-parse-arg-discard)
  (insert (format "%s %s %s"
          (substring (current-time-string) 8 10)
          (substring (current-time-string) 4 7)
          (substring (current-time-string) -4))))


(put 'ignore 'texinfo-format 'texinfo-format-ignore)
(defun texinfo-format-ignore ()
  (delete-region texinfo-command-start
		 (progn (search-forward "@end ignore\n")
			(point))))

(put 'endignore 'texinfo-format 'texinfo-discard-line)

(put 'var 'texinfo-format 'texinfo-format-var)
;  @sc  a small caps font for TeX; formatted as `var' in Info
(put 'sc 'texinfo-format 'texinfo-format-var)
(defun texinfo-format-var ()
  (insert (upcase (texinfo-parse-arg-discard)))
  (goto-char texinfo-command-start))

; various noops

(put 'asis 'texinfo-format 'texinfo-format-noop)
(put 'b 'texinfo-format 'texinfo-format-noop)
(put 't 'texinfo-format 'texinfo-format-noop)
(put 'i 'texinfo-format 'texinfo-format-noop)
(put 'r 'texinfo-format 'texinfo-format-noop)
(put 'titlefont 'texinfo-format 'texinfo-format-noop)
(put 'key 'texinfo-format 'texinfo-format-noop)
(put 'w 'texinfo-format 'texinfo-format-noop)
(defun texinfo-format-noop ()
  (insert (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

(put 'code 'texinfo-format 'texinfo-format-code)
(put 'samp 'texinfo-format 'texinfo-format-code)
(put 'file 'texinfo-format 'texinfo-format-code)
(put 'kbd 'texinfo-format 'texinfo-format-code)
(put 'cite 'texinfo-format 'texinfo-format-code)
(defun texinfo-format-code ()
  (insert "`" (texinfo-parse-arg-discard) "'")
  (goto-char texinfo-command-start))

(put 'emph 'texinfo-format 'texinfo-format-emph)
(put 'strong 'texinfo-format 'texinfo-format-emph)
(defun texinfo-format-emph ()
  (insert "*" (texinfo-parse-arg-discard) "*")
  (goto-char texinfo-command-start))

(put 'defn 'texinfo-format 'texinfo-format-defn)
(put 'dfn 'texinfo-format 'texinfo-format-defn)
(defun texinfo-format-defn ()
  (insert "\"" (texinfo-parse-arg-discard) "\"")
  (goto-char texinfo-command-start))

(put 'bullet 'texinfo-format 'texinfo-format-bullet)
(defun texinfo-format-bullet ()
  "Insert an asterisk.
If used within a line, follow `@bullet' with braces."
  (texinfo-optional-braces-discard)
  (insert "*"))

(put 'smallexample 'texinfo-format 'texinfo-format-example)
(put 'smalllisp 'texinfo-format 'texinfo-format-example)
(put 'example 'texinfo-format 'texinfo-format-example)
(put 'quotation 'texinfo-format 'texinfo-format-example)
(put 'lisp 'texinfo-format 'texinfo-format-example)
(put 'display 'texinfo-format 'texinfo-format-example)
(put 'format 'texinfo-format 'texinfo-format-example)
(put 'flushleft 'texinfo-format 'texinfo-format-example)
(defun texinfo-format-example ()
  (texinfo-push-stack 'example nil)
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'smallexample 'texinfo-end 'texinfo-end-example)
(put 'example 'texinfo-end 'texinfo-end-example)
(put 'quotation 'texinfo-end 'texinfo-end-example)
(put 'lisp 'texinfo-end 'texinfo-end-example)
(put 'display 'texinfo-end 'texinfo-end-example)
(put 'format 'texinfo-end 'texinfo-end-example)
(put 'flushleft 'texinfo-end 'texinfo-end-example)
(defun texinfo-end-example ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
	 (texinfo-pop-stack 'example)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'exdent 'texinfo-format 'texinfo-format-exdent)
(defun texinfo-format-exdent ()
  (texinfo-discard-command)
  (delete-region (point)
		 (progn
		  (skip-chars-forward " ")
		  (point)))
  (insert ?\b)
  ;; Cancel out the deletion that texinfo-do-itemize
  ;; is going to do at the end of this line.
  (save-excursion
    (end-of-line)
    (insert "\n     ")))


;; @flushright  ...   @end flushright

; The @flushright command right justifies every line but leaves the
; left end ragged.

(put 'flushright 'texinfo-format 'texinfo-format-flushright)
(defun texinfo-format-flushright ()
  (texinfo-push-stack 'flushright nil)
  (texinfo-discard-line))

(put 'flushright 'texinfo-end 'texinfo-end-flushright)
(defun texinfo-end-flushright ()
  (texinfo-discard-command)

  (let ((stacktop
         (texinfo-pop-stack 'flushright)))

    (texinfo-do-flushright (nth 1 stacktop))))

(defun texinfo-do-flushright (from)
  (save-excursion
   (while (progn (forward-line -1)
                 (>= (point) from))

     (beginning-of-line)
     (insert
      (make-string
       (- fill-column
          (save-excursion
            (end-of-line)
            (current-column)))  
       ? )))))


(put 'ctrl 'texinfo-format 'texinfo-format-ctrl)
(defun texinfo-format-ctrl ()
  (let ((str (texinfo-parse-arg-discard)))
    (insert (logand 31 (aref str 0)))))

(put 'TeX 'texinfo-format 'texinfo-format-TeX)
(defun texinfo-format-TeX ()
  (texinfo-parse-arg-discard)
  (insert "TeX"))

(put 'copyright 'texinfo-format 'texinfo-format-copyright)
(defun texinfo-format-copyright ()
  (texinfo-parse-arg-discard)
  (insert "(C)"))

(put 'minus 'texinfo-format 'texinfo-format-minus)
(defun texinfo-format-minus ()
  "Insert a minus sign.
If used within a line, follow `@minus' with braces."
  (texinfo-optional-braces-discard)
  (insert "-"))

(put 'dots 'texinfo-format 'texinfo-format-dots)
(defun texinfo-format-dots ()
  (texinfo-parse-arg-discard)
  (insert "..."))

(put 'refill 'texinfo-format 'texinfo-format-refill)
(defun texinfo-format-refill ()
  (texinfo-discard-command)
  (fill-paragraph nil))


;;; Index generation

(put 'vindex 'texinfo-format 'texinfo-format-vindex)
(defun texinfo-format-vindex ()
  (texinfo-index 'texinfo-vindex))

(put 'cindex 'texinfo-format 'texinfo-format-cindex)
(defun texinfo-format-cindex ()
  (texinfo-index 'texinfo-cindex))

(put 'findex 'texinfo-format 'texinfo-format-findex)
(defun texinfo-format-findex ()
  (texinfo-index 'texinfo-findex))

(put 'pindex 'texinfo-format 'texinfo-format-pindex)
(defun texinfo-format-pindex ()
  (texinfo-index 'texinfo-pindex))

(put 'tindex 'texinfo-format 'texinfo-format-tindex)
(defun texinfo-format-tindex ()
  (texinfo-index 'texinfo-tindex))

(put 'kindex 'texinfo-format 'texinfo-format-kindex)
(defun texinfo-format-kindex ()
  (texinfo-index 'texinfo-kindex))

(defun texinfo-index (indexvar)
  (let ((arg (texinfo-parse-expanded-arg)))
    (texinfo-discard-command)
    (set indexvar
	 (cons (list arg texinfo-last-node)
	       (symbol-value indexvar)))))

(defconst texinfo-indexvar-alist
  '(("cp" . texinfo-cindex)
    ("fn" . texinfo-findex)
    ("vr" . texinfo-vindex)
    ("tp" . texinfo-tindex)
    ("pg" . texinfo-pindex)
    ("ky" . texinfo-kindex)))


;;; @defindex   @defcodeindex
(put 'defindex 'texinfo-format 'texinfo-format-defindex)
(put 'defcodeindex 'texinfo-format 'texinfo-format-defindex)

(defun texinfo-format-defindex ()
  (let* ((index-name (texinfo-parse-arg-discard)) ; eg: `aa'
         (indexing-command (intern (concat index-name "index")))
         (index-formatting-command      ; eg: `texinfo-format-aaindex'
          (intern (concat "texinfo-format-" index-name "index")))
         (index-alist-name              ; eg: `texinfo-aaindex'
          (intern (concat "texinfo-" index-name "index"))))

    (set index-alist-name nil)

    (put indexing-command               ; eg, aaindex
         'texinfo-format
         index-formatting-command)      ; eg, texinfo-format-aaindex

    ;; eg: "aa" . texinfo-aaindex
    (or (assoc index-name texinfo-indexvar-alist)
        (setq texinfo-indexvar-alist
              (cons
               (cons index-name
                     index-alist-name)
               texinfo-indexvar-alist)))

    (fset index-formatting-command
          (list 'lambda 'nil
                (list 'texinfo-index 
                      (list 'quote index-alist-name))))))


;;; @synindex   @syncodeindex

(put 'synindex 'texinfo-format 'texinfo-format-synindex)
(put 'syncodeindex 'texinfo-format 'texinfo-format-synindex)

(defun texinfo-format-synindex ()
  (let* ((args (texinfo-parse-arg-discard))
         (second (cdr (read-from-string args)))
         (joiner (symbol-name (car (read-from-string args))))
         (joined (symbol-name (car (read-from-string args second)))))

    (if (assoc joiner texinfo-short-index-cmds-alist)
        (put
          (cdr (assoc joiner texinfo-short-index-cmds-alist))
         'texinfo-format
         (or (cdr (assoc joined texinfo-short-index-format-cmds-alist))
             (intern (concat "texinfo-format-" joined "index"))))
      (put
       (intern (concat joiner "index"))
       'texinfo-format
       (or (cdr(assoc joined texinfo-short-index-format-cmds-alist))
           (intern (concat "texinfo-format-" joined "index")))))))

(defconst texinfo-short-index-cmds-alist
  '(("cp" . cindex)
    ("fn" . findex)
    ("vr" . vindex)
    ("tp" . tindex)
    ("pg" . pindex)
    ("ky" . kindex)))

(defconst texinfo-short-index-format-cmds-alist
  '(("cp" . texinfo-format-cindex)
    ("fn" . texinfo-format-findex)
    ("vr" . texinfo-format-vindex)
    ("tp" . texinfo-format-tindex)
    ("pg" . texinfo-format-pindex)
    ("ky" . texinfo-format-kindex)))


;;; @printindex

(put 'printindex 'texinfo-format 'texinfo-format-printindex)

(defun texinfo-format-printindex ()
  (let ((indexelts (symbol-value
		    (cdr (assoc (texinfo-parse-arg-discard)
				texinfo-indexvar-alist))))
	opoint)
    (insert "\n* Menu:\n\n")
    (setq opoint (point))
    (texinfo-print-index nil indexelts)

    (if (eq system-type 'vax-vms)
        (texinfo-sort-region opoint (point))
      (shell-command-on-region opoint (point) "sort -fd" 1))))

(defun texinfo-print-index (file indexelts)
  (while indexelts
    (if (stringp (car (car indexelts)))
	(insert "* " (car (car indexelts))
		": " (if file (concat "(" file ")") "")
		(nth 1 (car indexelts)) ".\n")
      ;; index entries from @include'd file
      (texinfo-print-index (nth 1 (car indexelts))
			   (nth 2 (car indexelts))))
    (setq indexelts (cdr indexelts))))


;;; NOTATIONS: @equiv, @error, etc

;; @equiv           to show that two expressions are equivalent
;; @error           to show an error message
;; @expansion       to show what a macro expands to
;; @point           to show the location of point in an example
;; @print           to show what an evaluated expression prints
;; @result          to indicate the value returned by an expression

(put 'equiv 'texinfo-format 'texinfo-format-equiv)
(defun texinfo-format-equiv ()
  (texinfo-parse-arg-discard)
  (insert "=="))

(put 'error 'texinfo-format 'texinfo-format-error)
(defun texinfo-format-error ()
  (texinfo-parse-arg-discard)
  (insert "error-->"))

(put 'expansion 'texinfo-format 'texinfo-format-expansion)
(defun texinfo-format-expansion ()
  (texinfo-parse-arg-discard)
  (insert "==>"))

(put 'point 'texinfo-format 'texinfo-format-point)
(defun texinfo-format-point ()
  (texinfo-parse-arg-discard)
  (insert "-!-"))

(put 'print 'texinfo-format 'texinfo-format-print)
(defun texinfo-format-print ()
  (texinfo-parse-arg-discard)
  (insert "-|"))

(put 'result 'texinfo-format 'texinfo-format-result)
(defun texinfo-format-result ()
  (texinfo-parse-arg-discard)
  (insert "=>"))


;;;; Description formatting: @deffn, @defun, etc

(defun texinfo-format-defun ()
  (texinfo-push-stack 'defun nil)
  (setq fill-column (- fill-column 5))
  (texinfo-format-defun-1 t))

(defun texinfo-format-defunx ()
  (texinfo-format-defun-1 nil))

(defun texinfo-format-defun-1 (first-p)
  (let ((args (texinfo-format-parse-defun-args))
	(command-type (get texinfo-command-name 'texinfo-defun-type))
        (class "")
        (name "")
        (classification "") 
        (data-type ""))
    (texinfo-discard-command)

    (cond 
     ;; Generalized object oriented entity: `category class name [args...]'
     ;; In Info, `Category on class: name ARG'
     ((eq (eval (car command-type)) 'defop-type)
      (setq category (car args))
      (setq class (car (cdr args)))
      (setq name (car args))
      (setq args (cdr (cdr args))))

     ;; Specialized object oriented entity:  @defmethod, @defivar
     ;; "Instance Variable"    `class name [args...]'
     ;; In Info, `Instance variable of class: name'
     ((eq  (eval (car command-type)) 'defmethod-type)
      (setq category (car (cdr command-type)))
      (setq class (car args))
      (setq name (car args))
      (setq args (cdr args)))
     
     ;; Generalized function-like or variable-like entity:
     ;; `category name [args...]'
     ;; In Info, `Category: name ARGS'
     ((or (eq (eval (car command-type)) 'deffn-type)
          (eq (eval (car command-type)) 'deftp-type))
      (setq category (car args))
      (setq args (cdr args))
      (setq name (car args)))
     
     ;; Specialized function-like or variable-like entity:
     ;; "Macro"    `name [args...]'
     ;; In Info, `Macro: Name ARGS'
     ((eq (eval (car command-type)) 'defun-type)
      (setq category (car (cdr command-type)))
      (setq name (car args)))

     ;; Generalized typed-function-like or typed-variable-like entity:
     ;; `Classification data-type name [args...]'
     ;; In Info, `Classification:  data-type name ARGS'
     ((or (eq (eval (car command-type)) 'deftypefn-type)
          (eq (eval (car command-type)) 'deftypevr-type))
      (setq classification (car args))
      (setq data-type (car (cdr args)))
      (setq name (car (cdr (cdr args))))
      (setq args (cdr (cdr (cdr args)))))

     ;; Specialized typed-function-like or typed-variable-like entity:
     ;; `data-type name [args...]'
     ;; In Info, `Function:  data-type name ARGS'
     ;; or,      `Variable:  data-type name'
     ((or (eq (eval (car command-type)) 'deftypefun-type)
          (eq (eval (car command-type)) 'deftypevar-type))
      (setq classification (car (cdr command-type)))
      (setq data-type (car args))
      (setq name (car (cdr args)))
      (setq args (cdr (cdr args)))))

    ;; Delete extra newline inserted after previous header line.
    (if (not first-p)
	(delete-char -1))

    (let ((formatter (get texinfo-command-name 'texinfo-defun-format-type)))
      (cond
       ;; if typed function or variable
       ((eq formatter 'texinfo-format-deftypefn-type)
        (insert "* " classification ": " data-type " " name)
        (let ((args args))
          (while args
            (insert " " (car args))
            (setq args (cdr args)))))
       (t
        ;; and if object oriented, set category
        (if (or (eq formatter 'texinfo-format-defop-type)
                 (eq formatter 'texinfo-format-defcv-type))
             (setq category (funcall formatter category class)))
        (insert "* " category ": " name)
        (let ((args (cdr args)))
          (while args
            (insert " "
                    (if (or (= ?& (aref (car args) 0))
                            (eq (eval (car command-type)) 'deftp-type))
                        (car args)
                      (upcase (car args))))
            (setq args (cdr args)))))))

    ;; Insert extra newline so that paragraph filling does not mess
    ;; with header line.
    (insert "\n\n")
    (rplaca (cdr (cdr (car texinfo-stack))) (point))

    (let ((indexvar (get texinfo-command-name 'texinfo-defun-index))
	  (index-formatter
           (get texinfo-command-name 'texinfo-defun-format-index)))
      (set indexvar
	   (cons (list
                  (cond
                   ;; if object oriented
                   ((or (eq index-formatter 'texinfo-format-defop-index)
                        (eq index-formatter 'texinfo-format-defcv-index))
                    (funcall index-formatter name class))
                   ((eq index-formatter 'texinfo-format-deftypefn-index)
                    (funcall index-formatter name data-type))
                   (t (car args)))
                  texinfo-last-node)
		 (symbol-value indexvar))))))

(defun texinfo-end-defun ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((start (nth 1 (texinfo-pop-stack 'defun))))
    (texinfo-do-itemize start)
    ;; Delete extra newline inserted after header.
    (save-excursion
      (goto-char start)
      (delete-char -1))))

(defun texinfo-format-defop-type (category class)
  (format "%s on %s" category class))

(defun texinfo-format-defop-index (name class)
  (format "%s on %s" name class))

(defun texinfo-format-defcv-type (category class)
  (format "%s of %s" category class))

(defun texinfo-format-defcv-index (name class)
  (format "%s of %s" name class))

(put 'deffn 'texinfo-format 'texinfo-format-defun)
(put 'deffnx 'texinfo-format 'texinfo-format-defunx)
(put 'deffn 'texinfo-end 'texinfo-end-defun)
(put 'deffn 'texinfo-defun-type '('deffn-type nil))
(put 'deffnx 'texinfo-defun-type '('deffn-type nil))
(put 'deffn 'texinfo-defun-index 'texinfo-findex)
(put 'deffnx 'texinfo-defun-index 'texinfo-findex)

(put 'defun 'texinfo-format 'texinfo-format-defun)
(put 'defunx 'texinfo-format 'texinfo-format-defunx)
(put 'defun 'texinfo-end 'texinfo-end-defun)
(put 'defun 'texinfo-defun-type '('defun-type "Function"))
(put 'defunx 'texinfo-defun-type '('defun-type "Function"))
(put 'defun 'texinfo-defun-index 'texinfo-findex)
(put 'defunx 'texinfo-defun-index 'texinfo-findex)

(put 'defmac 'texinfo-format 'texinfo-format-defun)
(put 'defmacx 'texinfo-format 'texinfo-format-defunx)
(put 'defmac 'texinfo-end 'texinfo-end-defun)
(put 'defmac 'texinfo-defun-type '('defun-type "Macro"))
(put 'defmacx 'texinfo-defun-type '('defun-type "Macro"))
(put 'defmac 'texinfo-defun-index 'texinfo-findex)
(put 'defmacx 'texinfo-defun-index 'texinfo-findex)

(put 'defspec 'texinfo-format 'texinfo-format-defun)
(put 'defspecx 'texinfo-format 'texinfo-format-defunx)
(put 'defspec 'texinfo-end 'texinfo-end-defun)
(put 'defspec 'texinfo-defun-type '('defun-type "Special form"))
(put 'defspecx 'texinfo-defun-type '('defun-type "Special form"))
(put 'defspec 'texinfo-defun-index 'texinfo-findex)
(put 'defspecx 'texinfo-defun-index 'texinfo-findex)

(put 'defvr 'texinfo-format 'texinfo-format-defun)
(put 'defvrx 'texinfo-format 'texinfo-format-defunx)
(put 'defvr 'texinfo-end 'texinfo-end-defun)
(put 'defvr 'texinfo-defun-type '('deffn-type nil))
(put 'defvrx 'texinfo-defun-type '('deffn-type nil))
(put 'defvr 'texinfo-defun-index 'texinfo-vindex)
(put 'defvrx 'texinfo-defun-index 'texinfo-vindex)

(put 'defvar 'texinfo-format 'texinfo-format-defun)
(put 'defvarx 'texinfo-format 'texinfo-format-defunx)
(put 'defvar 'texinfo-end 'texinfo-end-defun)
(put 'defvar 'texinfo-defun-type '('defun-type "Variable"))
(put 'defvarx 'texinfo-defun-type '('defun-type "Variable"))
(put 'defvar 'texinfo-defun-index 'texinfo-vindex)
(put 'defvarx 'texinfo-defun-index 'texinfo-vindex)

(put 'defconst 'texinfo-format 'texinfo-format-defun)
(put 'defconstx 'texinfo-format 'texinfo-format-defunx)
(put 'defconst 'texinfo-end 'texinfo-end-defun)
(put 'defconst 'texinfo-defun-type '('defun-type "Constant"))
(put 'defconstx 'texinfo-defun-type '('defun-type "Constant"))
(put 'defconst 'texinfo-defun-index 'texinfo-vindex)
(put 'defconstx 'texinfo-defun-index 'texinfo-vindex)

(put 'defcmd 'texinfo-format 'texinfo-format-defun)
(put 'defcmdx 'texinfo-format 'texinfo-format-defunx)
(put 'defcmd 'texinfo-end 'texinfo-end-defun)
(put 'defcmd 'texinfo-defun-type '('defun-type "Command"))
(put 'defcmdx 'texinfo-defun-type '('defun-type "Command"))
(put 'defcmd 'texinfo-defun-index 'texinfo-findex)
(put 'defcmdx 'texinfo-defun-index 'texinfo-findex)

(put 'defopt 'texinfo-format 'texinfo-format-defun)
(put 'defoptx 'texinfo-format 'texinfo-format-defunx)
(put 'defopt 'texinfo-end 'texinfo-end-defun)
(put 'defopt 'texinfo-defun-type '('defun-type "User Option"))
(put 'defoptx 'texinfo-defun-type '('defun-type "User Option"))
(put 'defopt 'texinfo-defun-index 'texinfo-vindex)
(put 'defoptx 'texinfo-defun-index 'texinfo-vindex)

(put 'deftp 'texinfo-format 'texinfo-format-defun)
(put 'deftpx 'texinfo-format 'texinfo-format-defunx)
(put 'deftp 'texinfo-end 'texinfo-end-defun)
(put 'deftp 'texinfo-defun-type '('deftp-type nil))
(put 'deftpx 'texinfo-defun-type '('deftp-type nil))
(put 'deftp 'texinfo-defun-index 'texinfo-tindex)
(put 'deftpx 'texinfo-defun-index 'texinfo-tindex)

;;; Object-oriented stuff is a little hairier.

(put 'defop 'texinfo-format 'texinfo-format-defun)
(put 'defopx 'texinfo-format 'texinfo-format-defunx)
(put 'defop 'texinfo-end 'texinfo-end-defun)
(put 'defop 'texinfo-defun-type '('defop-type nil))
(put 'defopx 'texinfo-defun-type '('defop-type nil))
(put 'defop 'texinfo-defun-format-type 'texinfo-format-defop-type)
(put 'defopx 'texinfo-defun-format-type 'texinfo-format-defop-type)
(put 'defop 'texinfo-defun-index 'texinfo-findex)
(put 'defopx 'texinfo-defun-index 'texinfo-findex)
(put 'defop 'texinfo-defun-format-index 'texinfo-format-defop-index)
(put 'defopx 'texinfo-defun-format-index 'texinfo-format-defop-index)

(put 'defmethod 'texinfo-format 'texinfo-format-defun)
(put 'defmethodx 'texinfo-format 'texinfo-format-defunx)
(put 'defmethod 'texinfo-end 'texinfo-end-defun)
(put 'defmethod 'texinfo-defun-type '('defmethod-type "Operation"))
(put 'defmethodx 'texinfo-defun-type '('defmethod-type "Operation"))
(put 'defmethod 'texinfo-defun-format-type 'texinfo-format-defop-type)
(put 'defmethodx 'texinfo-defun-format-type 'texinfo-format-defop-type)
(put 'defmethod 'texinfo-defun-index 'texinfo-findex)
(put 'defmethodx 'texinfo-defun-index 'texinfo-findex)
(put 'defmethod 'texinfo-defun-format-index 'texinfo-format-defop-index)
(put 'defmethodx 'texinfo-defun-format-index 'texinfo-format-defop-index)

(put 'defcv 'texinfo-format 'texinfo-format-defun)
(put 'defcvx 'texinfo-format 'texinfo-format-defunx)
(put 'defcv 'texinfo-end 'texinfo-end-defun)
(put 'defcv 'texinfo-defun-type '('defop-type nil))
(put 'defcvx 'texinfo-defun-type '('defop-type nil))
(put 'defcv 'texinfo-defun-format-type 'texinfo-format-defcv-type)
(put 'defcvx 'texinfo-defun-format-type 'texinfo-format-defcv-type)
(put 'defcv 'texinfo-defun-index 'texinfo-vindex)
(put 'defcvx 'texinfo-defun-index 'texinfo-vindex)
(put 'defcv 'texinfo-defun-format-index 'texinfo-format-defcv-index)
(put 'defcvx 'texinfo-defun-format-index 'texinfo-format-defcv-index)

(put 'defivar 'texinfo-format 'texinfo-format-defun)
(put 'defivarx 'texinfo-format 'texinfo-format-defunx)
(put 'defivar 'texinfo-end 'texinfo-end-defun)
(put 'defivar 'texinfo-defun-type '('defmethod-type "Instance variable"))
(put 'defivarx 'texinfo-defun-type '('defmethod-type "Instance variable"))
(put 'defivar 'texinfo-defun-format-type 'texinfo-format-defcv-type)
(put 'defivarx 'texinfo-defun-format-type 'texinfo-format-defcv-type)
(put 'defivar 'texinfo-defun-index 'texinfo-vindex)
(put 'defivarx 'texinfo-defun-index 'texinfo-vindex)
(put 'defivar 'texinfo-defun-format-index 'texinfo-format-defcv-index)
(put 'defivarx 'texinfo-defun-format-index 'texinfo-format-defcv-index)

;;; Typed functions and variables

(defun texinfo-format-deftypefn-type (classification data-type)
  (format "%s" classification data-type))

(defun texinfo-format-deftypefn-index (name data-type)
  (format "%s of type %s" name data-type))


(put 'deftypefn 'texinfo-format 'texinfo-format-defun)
(put 'deftypefnx 'texinfo-format 'texinfo-format-defunx)
(put 'deftypefn 'texinfo-end 'texinfo-end-defun)
(put 'deftypefn 'texinfo-defun-type '('deftypefn-type nil))
(put 'deftypefnx 'texinfo-defun-type '('deftypefn-type nil))
(put 'deftypefn 'texinfo-defun-format-type 'texinfo-format-deftypefn-type)
(put 'deftypefnx 'texinfo-defun-format-type 'texinfo-format-deftypefn-type)
(put 'deftypefn 'texinfo-defun-index 'texinfo-findex)
(put 'deftypefnx 'texinfo-defun-index 'texinfo-findex)
(put 'deftypefn 'texinfo-defun-format-index 'texinfo-format-deftypefn-index)
(put 'deftypefnx 'texinfo-defun-format-index 'texinfo-format-deftypefn-index)

(put 'deftypefun 'texinfo-format 'texinfo-format-defun)
(put 'deftypefunx 'texinfo-format 'texinfo-format-defunx)
(put 'deftypefun 'texinfo-end 'texinfo-end-defun)
(put 'deftypefun 'texinfo-defun-type '('deftypefun-type "Function"))
(put 'deftypefunx 'texinfo-defun-type '('deftypefun-type "Function"))
(put 'deftypefun 'texinfo-defun-format-type 'texinfo-format-deftypefn-type)
(put 'deftypefunx 'texinfo-defun-format-type 'texinfo-format-deftypefn-type)
(put 'deftypefun 'texinfo-defun-index 'texinfo-findex)
(put 'deftypefunx 'texinfo-defun-index 'texinfo-findex)
(put 'deftypefun 'texinfo-defun-format-index 'texinfo-format-deftypefn-index)
(put 'deftypefunx 'texinfo-defun-format-index 'texinfo-format-deftypefn-index)

(put 'deftypevr 'texinfo-format 'texinfo-format-defun)
(put 'deftypevrx 'texinfo-format 'texinfo-format-defunx)
(put 'deftypevr 'texinfo-end 'texinfo-end-defun)
(put 'deftypevr 'texinfo-defun-type '('deftypefn-type nil))
(put 'deftypevrx 'texinfo-defun-type '('deftypefn-type nil))
(put 'deftypevr 'texinfo-defun-format-type 'texinfo-format-deftypefn-type)
(put 'deftypevrx 'texinfo-defun-format-type 'texinfo-format-deftypefn-type)
(put 'deftypevr 'texinfo-defun-index 'texinfo-vindex)
(put 'deftypevrx 'texinfo-defun-index 'texinfo-vindex)
(put 'deftypevr 'texinfo-defun-format-index 'texinfo-format-deftypefn-index)
(put 'deftypevrx 'texinfo-defun-format-index 'texinfo-format-deftypefn-index)

(put 'deftypevar 'texinfo-format 'texinfo-format-defun)
(put 'deftypevarx 'texinfo-format 'texinfo-format-defunx)
(put 'deftypevar 'texinfo-end 'texinfo-end-defun)
(put 'deftypevar 'texinfo-defun-type '('deftypevar-type "Variable"))
(put 'deftypevarx 'texinfo-defun-type '('deftypevar-type "Variable"))
(put 'deftypevar 'texinfo-defun-format-type 'texinfo-format-deftypefn-type)
(put 'deftypevarx 'texinfo-defun-format-type 'texinfo-format-deftypefn-type)
(put 'deftypevar 'texinfo-defun-index 'texinfo-vindex)
(put 'deftypevarx 'texinfo-defun-index 'texinfo-vindex)
(put 'deftypevar 'texinfo-defun-format-index 'texinfo-format-deftypefn-index)
(put 'deftypevarx 'texinfo-defun-format-index 'texinfo-format-deftypefn-index)


;; process included files:  `@include' command

;; Updated 19 October 1990
;; In the original version, include files were ignored by Info but
;; incorporated in to the printed manual.  To make references to the
;; included file, the Texinfo source file has to refer to the included
;; files using the `(filename)nodename' format for refering to other
;; Info files.  Also, the included files had to be formatted on their
;; own.  It was just like they were another file.

;; Currently, include files are inserted into the buffer that is
;; formatted for Info.  If large, the resulting info file is split and
;; tagified.  For current include files to work, the master menu must
;; refer to all the nodes, and the highest level nodes in the include
;; files must have the correct next, prev, and up pointers.

;; The included file may have an @setfilename and even an @settitle,
;; but not an /input texinfo

; Original definition:
; (defun texinfo-format-include ()
;   (let ((filename (texinfo-parse-arg-discard))
; 	(default-directory input-directory)
; 	subindex)
;     (setq subindex
; 	  (save-excursion
; 	    (progn (find-file
; 		    (cond ((file-readable-p (concat filename ".texinfo"))
; 			   (concat filename ".texinfo"))
; 			  ((file-readable-p (concat filename ".texi"))
; 			   (concat filename ".texi"))
; 			  ((file-readable-p (concat filename ".tex"))
; 			   (concat filename ".tex"))
; 			  ((file-readable-p filename)
; 			   filename)
; 			  (t (error "@include'd file %s not found"
; 				    filename))))
; 		   (texinfo-format-buffer-1))))
;     (texinfo-subindex 'texinfo-vindex (car subindex) (nth 1 subindex))
;     (texinfo-subindex 'texinfo-findex (car subindex) (nth 2 subindex))
;     (texinfo-subindex 'texinfo-cindex (car subindex) (nth 3 subindex))
;     (texinfo-subindex 'texinfo-pindex (car subindex) (nth 4 subindex))
;     (texinfo-subindex 'texinfo-tindex (car subindex) (nth 5 subindex))
;     (texinfo-subindex 'texinfo-kindex (car subindex) (nth 6 subindex))))

(defun texinfo-subindex (indexvar file content)
  (set indexvar (cons (list 'recurse file content)
		      (symbol-value indexvar))))

(put 'include 'texinfo-format 'texinfo-format-include)
(defun texinfo-format-include ()
  (let ((filename (concat input-directory
			  (texinfo-parse-arg-discard)))
	(default-directory input-directory))
    (message "Reading: %s" filename)
    (save-excursion
      (insert-file-contents filename)))
  (setq last-input-buffer input-buffer)  ; to bypass setfilename
  )



;; Lots of bolio constructs do nothing in texinfo.

(put 'page 'texinfo-format 'texinfo-discard-line-with-args)
(put 'c 'texinfo-format 'texinfo-discard-line-with-args)
(put 'comment 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setchapternewpage 'texinfo-format 'texinfo-discard-line-with-args)
(put 'contents 'texinfo-format 'texinfo-discard-line-with-args)
(put 'summarycontents 'texinfo-format 'texinfo-discard-line-with-args)
(put 'shortcontents 'texinfo-format 'texinfo-discard-line-with-args)
(put 'nopara 'texinfo-format 'texinfo-discard-line-with-args)
(put 'noindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setx 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setq 'texinfo-format 'texinfo-discard-line-with-args)
(put 'settitle 'texinfo-format 'texinfo-discard-line-with-args)
(put 'hsize 'texinfo-format 'texinfo-discard-line-with-args)
(put 'parindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'lispnarrowing 'texinfo-format 'texinfo-discard-line-with-args)
(put 'itemindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'headings 'texinfo-format 'texinfo-discard-line-with-args)
(put 'group 'texinfo-format 'texinfo-discard-line-with-args)
(put 'group 'texinfo-end 'texinfo-discard-line-with-args)
(put 'need 'texinfo-format 'texinfo-discard-line-with-args)
(put 'bye 'texinfo-format 'texinfo-discard-line)
(put 'smallbook 'texinfo-format 'texinfo-discard-line)

(defun texinfo-discard-line-with-args ()
  (goto-char texinfo-command-start)
  (delete-region (point) (progn (forward-line 1) (point))))

;; Sort an index which is in the current buffer between START and END.
;; Used on VMS, where the `sort' utility is not available.
(defun texinfo-sort-region (start end)
  (require 'sort)
  (save-restriction
    (narrow-to-region start end)
    (sort-subr nil 'forward-line 'end-of-line 'texinfo-sort-startkeyfun)))

;; Subroutine for sorting an index.
;; At start of a line, return a string to sort the line under.
(defun texinfo-sort-startkeyfun ()
  (let ((line
	 (buffer-substring (point) (save-excursion (end-of-line) (point)))))
    ;; Canonicalize whitespace and eliminate funny chars.
    (while (string-match "[ \t][ \t]+\\|[^a-z0-9 ]+" line)
      (setq line (concat (substring line 0 (match-beginning 0))
			 " "
			 (substring line (match-end 0) (length line)))))
    line))

;; Some cannot be handled

(defun texinfo-unsupported ()
  (error "%s is not handled by texinfo"
	 (buffer-substring texinfo-command-start texinfo-command-end)))

;;;###autoload
(defun batch-texinfo-format ()
  "Runs `texinfo-format-buffer' on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke
  \"emacs -batch -funcall batch-texinfo-format $docs/ ~/*.texinfo\"."
  (if (not noninteractive)
      (error "batch-texinfo-format may only be used -batch."))
  (let ((version-control t)
	(auto-save-default nil)
	(find-file-run-dired nil)
	(kept-old-versions 259259)
	(kept-new-versions 259259))
    (let ((error 0)
	  file
	  (files ()))
      (while command-line-args-left
	(setq file (expand-file-name (car command-line-args-left)))
	(cond ((not (file-exists-p file))
	       (message ">> %s does not exist!" file)
	       (setq error 1
		     command-line-args-left (cdr command-line-args-left)))
	      ((file-directory-p file)
	       (setq command-line-args-left
		     (nconc (directory-files file)
			    (cdr command-line-args-left))))
	      (t
	       (setq files (cons file files)
		     command-line-args-left (cdr command-line-args-left)))))
      (while files
	(setq file (car files)
	      files (cdr files))
	(condition-case err
	    (progn
	      (if buffer-file-name (kill-buffer (current-buffer)))
	      (find-file file)
	      (buffer-disable-undo (current-buffer))
	      (set-buffer-modified-p nil)
	      (texinfo-mode)
	      (message "texinfo formatting %s..." file)
	      (texinfo-format-buffer nil)
	      (if (buffer-modified-p)
		  (progn (message "Saving modified %s" (buffer-file-name))
			 (save-buffer))))
	  (error
	   (message ">> Error: %s" (prin1-to-string err))
	   (message ">>  point at")
	   (let ((s (buffer-substring (point)
				      (min (+ (point) 100)
					   (point-max))))
		 (tem 0))
	     (while (setq tem (string-match "\n+" s tem))
	       (setq s (concat (substring s 0 (match-beginning 0))
			       "\n>>  "
			       (substring s (match-end 0)))
		     tem (1+ tem)))
	     (message ">>  %s" s))
	   (setq error 1))))
      (kill-emacs error))))

;;; texinfmt.el ends here
