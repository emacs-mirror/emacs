;;; org-exp-blocks.el --- pre-process blocks when exporting org files

;; Copyright (C) 2009
;;   Free Software Foundation, Inc.

;; Author: Eric Schulte

;; This file is part of GNU Emacs.
;;
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

;;; Commentary:
;;
;; This is a utility for pre-processing blocks in org files before
;; export using the `org-export-preprocess-hook'.  It can be used for
;; exporting new types of blocks from org-mode files and also for
;; changing the default export behavior of existing org-mode blocks.
;; The `org-export-blocks' and `org-export-interblocks' variables can
;; be used to control how blocks and the spaces between blocks
;; respectively are processed upon export.
;;
;; The type of a block is defined as the string following =#+begin_=,
;; so for example the following block would be of type ditaa.  Note
;; that both upper or lower case are allowed in =#+BEGIN_= and
;; =#+END_=.
;;
;; #+begin_ditaa blue.png -r -S
;; +---------+
;; | cBLU    |
;; |         |
;; |    +----+
;; |    |cPNK|
;; |    |    |
;; +----+----+
;; #+end_ditaa
;;
;;; Currently Implemented Block Types
;;
;; ditaa :: Convert ascii pictures to actual images using ditaa
;;          http://ditaa.sourceforge.net/.  To use this set
;;          `org-ditaa-jar-path' to the path to ditaa.jar on your
;;          system (should be set automatically in most cases) .
;;
;; dot :: Convert graphs defined using the dot graphing language to
;;        images using the dot utility.  For information on dot see
;;        http://www.graphviz.org/
;;
;; comment :: Wrap comments with titles and author information, in
;;            their own divs with author-specific ids allowing for css
;;            coloring of comments based on the author.
;;
;; R :: Implements Sweave type exporting, evaluates blocks of R code,
;;      and also replaces \R{} chunks in the file with their result
;;      when passed to R.  This require the `R' command which is
;;      provided by ESS (Emacs Speaks Statistics).
;;
;;; Adding new blocks
;;
;; When adding a new block type first define a formatting function
;; along the same lines as `org-export-blocks-format-dot' and then use
;; `org-export-blocks-add-block' to add your block type to
;; `org-export-blocks'.

(eval-when-compile
  (require 'cl))
(require 'org)

(defvar comint-last-input-end)
(defvar comint-prompt-regexp)
(defvar comint-last-input-end)
(defvar htmlp)
(defvar latexp)
(defvar docbookp)
(defvar asciip)

(declare-function comint-send-input "comint" (&optional no-newline artificial))
(declare-function R "ess" nil)

(defun org-export-blocks-set (var value)
  "Set the value of `org-export-blocks' and install fontification."
  (set var value)
  (mapc (lambda (spec)
	  (if (nth 2 spec)
	      (setq org-protecting-blocks
		    (delete (symbol-name (car spec))
			    org-protecting-blocks))
	    (add-to-list 'org-protecting-blocks
			 (symbol-name (car spec)))))
	value))

(defcustom org-export-blocks
  '((comment org-export-blocks-format-comment t)
    (ditaa org-export-blocks-format-ditaa nil)
    (dot org-export-blocks-format-dot nil)
    (r org-export-blocks-format-R nil)
    (R org-export-blocks-format-R nil))
  "Use this a-list to associate block types with block exporting
functions.  The type of a block is determined by the text
immediately following the '#+BEGIN_' portion of the block header.
Each block export function should accept three argumets..."
  :group 'org-export-general
  :type '(repeat
	  (list
	   (symbol :tag "Block name")
	   (function :tag "Block formatter")
	   (boolean :tag "Fontify content as Org syntax")))
  :set 'org-export-blocks-set)

(defun org-export-blocks-add-block (block-spec)
  "Add a new block type to `org-export-blocks'.  BLOCK-SPEC
should be a three element list the first element of which should
indicate the name of the block, the second element should be the
formatting function called by `org-export-blocks-preprocess' and
the third element a flag indicating whether these types of blocks
should be fontified in org-mode buffers (see
`org-protecting-blocks').  For example the BLOCK-SPEC for ditaa
blocks is as follows...

  (ditaa org-export-blocks-format-ditaa nil)"
  (unless (member block-spec org-export-blocks)
    (setq org-export-blocks (cons block-spec org-export-blocks))
    (org-export-blocks-set 'org-export-blocks org-export-blocks)))

(defcustom org-export-interblocks
  '((r org-export-interblocks-format-R)
    (R org-export-interblocks-format-R))
  "Use this a-list to associate block types with block exporting
functions.  The type of a block is determined by the text
immediately following the '#+BEGIN_' portion of the block header.
Each block export function should accept three argumets..."
  :group 'org-export-general
  :type 'alist)

(defcustom org-export-blocks-witheld
  '(hidden)
  "List of block types (see `org-export-blocks') which should not
be exported."
  :group 'org-export-general
  :type 'list)

(defvar org-export-blocks-postblock-hooks nil "")

(defun org-export-blocks-html-quote (body &optional open close)
  "Protext BODY from org html export.  The optional OPEN and
CLOSE tags will be inserted around BODY."
  (concat
   "\n#+BEGIN_HTML\n"
   (or open "")
   body (if (string-match "\n$" body) "" "\n")
   (or close "")
   "#+END_HTML\n"))

(defun org-export-blocks-latex-quote (body &optional open close)
  "Protext BODY from org latex export.  The optional OPEN and
CLOSE tags will be inserted around BODY."
  (concat
   "\n#+BEGIN_LaTeX\n"
   (or open "")
   body (if (string-match "\n$" body) "" "\n")
   (or close "")
   "#+END_LaTeX\n"))

(defun org-export-blocks-preprocess ()
  "Export all blocks acording to the `org-export-blocks' block
exportation alist.  Does not export block types specified in
specified in BLOCKS which default to the value of
`org-export-blocks-witheld'."
  (interactive)
  (save-window-excursion
    (let ((count 0)
	  (blocks org-export-blocks-witheld)
	  (case-fold-search t)
	  (types '())
	  indentation type func start end)
      (flet ((interblock (start end type)
			 (save-match-data
			   (when (setf func (cadr (assoc type org-export-interblocks)))
			     (funcall func start end)))))
	(goto-char (point-min))
	(setf start (point))
	(while (re-search-forward
		"^\\([ \t]*\\)#\\+begin_\\(\\S-+\\)[ \t]*\\(.*\\)?[\r\n]\\([^\000]*?\\)[\r\n][ \t]*#\\+end_\\S-+.*" nil t)
          (save-match-data (setq indentation (length (match-string 1))))
	  (save-match-data (setf type (intern (match-string 2))))
	  (unless (memq type types) (setf types (cons type types)))
	  (setf end (save-match-data (match-beginning 0)))
	  (interblock start end type)
	  (if (setf func (cadr (assoc type org-export-blocks)))
	      (progn
                (replace-match (save-match-data
                                 (if (memq type blocks)
                                     ""
                                   (apply func (save-match-data (org-remove-indentation (match-string 4)))
                                          (split-string (match-string 3) " ")))) t t)
                ;; indent the replaced match
                (indent-region (match-beginning 0) (match-end 0) indentation)
                ))
	  (setf start (save-match-data (match-end 0))))
	(mapcar (lambda (type)
		  (interblock start (point-max) type))
		types)))))

(add-hook 'org-export-preprocess-hook 'org-export-blocks-preprocess)

;;================================================================================
;; type specific functions

;;--------------------------------------------------------------------------------
;; ditaa: create images from ASCII art using the ditaa utility
(defvar org-ditaa-jar-path (expand-file-name
			    "ditaa.jar"
			    (file-name-as-directory
			     (expand-file-name
			      "scripts"
			      (file-name-as-directory
			       (expand-file-name
				"../contrib"
				(file-name-directory (or load-file-name buffer-file-name)))))))
  "Path to the ditaa jar executable")

(defun org-export-blocks-format-ditaa (body &rest headers)
  "Pass block BODY to the ditaa utility creating an image.
Specify the path at which the image should be saved as the first
element of headers, any additional elements of headers will be
passed to the ditaa utility as command line arguments."
  (message "ditaa-formatting...")
  (let ((out-file (if headers (car headers)))
	(args (if (cdr headers) (mapconcat 'identity (cdr headers) " ")))
	(data-file (make-temp-file "org-ditaa")))
    (unless (file-exists-p org-ditaa-jar-path)
      (error (format "Could not find ditaa.jar at %s" org-ditaa-jar-path)))
    (setq body (if (string-match "^\\([^:\\|:[^ ]\\)" body)
		   body
		 (mapconcat (lambda (x) (substring x (if (> (length x) 1) 2 1)))
			    (org-split-string body "\n")
			    "\n")))
    (cond
     ((or htmlp latexp docbookp)
      (with-temp-file data-file (insert body))
      (message (concat "java -jar " org-ditaa-jar-path " " args " " data-file " " out-file))
      (shell-command (concat "java -jar " org-ditaa-jar-path " " args " " data-file " " out-file))
      (format "\n[[file:%s]]\n" out-file))
     (t (concat
	 "\n#+BEGIN_EXAMPLE\n"
	 body (if (string-match "\n$" body) "" "\n")
	 "#+END_EXAMPLE\n")))))

;;--------------------------------------------------------------------------------
;; dot: create graphs using the dot graphing language
;;      (require the dot executable to be in your path)
(defun org-export-blocks-format-dot (body &rest headers)
  "Pass block BODY to the dot graphing utility creating an image.
Specify the path at which the image should be saved as the first
element of headers, any additional elements of headers will be
passed to the dot utility as command line arguments.  Don't
forget to specify the output type for the dot command, so if you
are exporting to a file with a name like 'image.png' you should
include a '-Tpng' argument, and your block should look like the
following.

#+begin_dot models.png -Tpng
digraph data_relationships {
  \"data_requirement\" [shape=Mrecord, label=\"{DataRequirement|description\lformat\l}\"]
  \"data_product\" [shape=Mrecord, label=\"{DataProduct|name\lversion\lpoc\lformat\l}\"]
  \"data_requirement\" -> \"data_product\"
}
#+end_dot"
  (message "dot-formatting...")
  (let ((out-file (if headers (car headers)))
	(args (if (cdr headers) (mapconcat 'identity (cdr headers) " ")))
	(data-file (make-temp-file "org-ditaa")))
    (cond
     ((or htmlp latexp docbookp)
      (with-temp-file data-file (insert body))
      (message (concat "dot " data-file " " args " -o " out-file))
      (shell-command (concat "dot " data-file " " args " -o " out-file))
      (format "\n[[file:%s]]\n" out-file))
     (t (concat
	 "\n#+BEGIN_EXAMPLE\n"
	 body (if (string-match "\n$" body) "" "\n")
	 "#+END_EXAMPLE\n")))))

;;--------------------------------------------------------------------------------
;; comment: export comments in author-specific css-stylable divs
(defun org-export-blocks-format-comment (body &rest headers)
  "Format comment BODY by OWNER and return it formatted for export.
Currently, this only does something for HTML export, for all
other backends, it converts the comment into an EXAMPLE segment."
  (let ((owner (if headers (car headers)))
	(title (if (cdr headers) (mapconcat 'identity (cdr headers) " "))))
    (cond
     (htmlp ;; We are exporting to HTML
      (concat "#+BEGIN_HTML\n"
	      "<div class=\"org-comment\""
	      (if owner (format " id=\"org-comment-%s\" " owner))
	      ">\n"
	      (if owner (concat "<b>" owner "</b> ") "")
	      (if (and title (> (length title) 0)) (concat " -- " title "</br>\n") "</br>\n")
	      "<p>\n"
	      "#+END_HTML\n"
	      body
	      "#+BEGIN_HTML\n"
	      "</p>\n"
	      "</div>\n"
	      "#+END_HTML\n"))
     (t ;; This is not HTML, so just make it an example.
      (concat "#+BEGIN_EXAMPLE\n"
	      (if title (concat "Title:" title "\n") "")
	      (if owner (concat "By:" owner "\n") "")
	      body
	      (if (string-match "\n\\'" body) "" "\n")
	      "#+END_EXAMPLE\n")))))

;;--------------------------------------------------------------------------------
;; R: Sweave-type functionality
(defvar interblock-R-buffer nil
  "Holds the buffer for the current R process")

(defvar count) ; dynamicaly scoped from `org-export-blocks-preprocess'?
(defun org-export-blocks-format-R (body &rest headers)
  "Process R blocks and replace \R{} forms outside the blocks
with their values as determined by R."
  (interactive)
  (message "R processing...")
  (let ((image-path (or (and (car headers)
			     (string-match "\\(.?\\)\.\\(EPS\\|eps\\)" (car headers))
			     (match-string 1 (car headers)))
			(and (> (length (car headers)) 0)
			     (car headers))
			;; create the default filename
			(format "Rplot-%03d" count)))
	(plot (string-match "plot" body))
	R-proc)
    (setf count (+ count 1))
    (interblock-initiate-R-buffer)
    (setf R-proc (get-buffer-process interblock-R-buffer))
    ;; send strings to the ESS process using `comint-send-string'
    (setf body (mapconcat (lambda (line)
			    (interblock-R-input-command line) (concat "> " line))
			  (butlast (split-string body "[\r\n]"))
			  "\n"))
    ;; if there is a plot command, then create the images
    (when plot
      (interblock-R-input-command (format "dev.copy2eps(file=\"%s.eps\")" image-path)))
    (concat (cond
	     (htmlp (org-export-blocks-html-quote body
						  (format "<div id=\"R-%d\">\n<pre>\n" count)
						  "</pre>\n</div>\n"))
	     (latexp (org-export-blocks-latex-quote body
						    "\\begin{Schunk}\n\\begin{Sinput}\n"
						    "\\end{Sinput}\n\\end{Schunk}\n"))
	     (t (insert ;; default export
		 "#+begin_R " (mapconcat 'identity headers " ") "\n"
		 body (if (string-match "\n$" body) "" "\n")
		 "#+end_R\n")))
	    (if plot
		(format "[[file:%s.eps]]\n" image-path)
	      ""))))

(defun org-export-interblocks-format-R (start end)
  "This is run over parts of the org-file which are between R
blocks.  It's main use is to expand the \R{stuff} chunks for
export."
  (save-excursion
    (goto-char start)
    (interblock-initiate-R-buffer)
    (let (code replacement)
      (while (and (< (point) end) (re-search-forward "\\\\R{\\(.*\\)}" end t))
	(save-match-data (setf code (match-string 1)))
	(setf replacement (interblock-R-command-to-string code))
	(setf replacement (cond
			   (htmlp replacement)
			   (latexp replacement)
			   (t replacement)))
	(setf end (+ end (- (length replacement) (length code))))
	(replace-match replacement t t)))))

(defun interblock-initiate-R-buffer ()
  "If there is not a current R process then create one."
  (unless (and (buffer-live-p interblock-R-buffer) (get-buffer interblock-R-buffer))
    (save-excursion
      (R)
      (setf interblock-R-buffer (current-buffer))
      (interblock-R-wait-for-output)
      (interblock-R-input-command ""))))

(defun interblock-R-command-to-string (command)
  "Send a command to R, and return the results as a string."
  (interblock-R-input-command command)
  (interblock-R-last-output))

(defun interblock-R-input-command (command)
  "Pass COMMAND to the R process running in `interblock-R-buffer'."
  (save-excursion
    (save-match-data
      (set-buffer interblock-R-buffer)
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (insert command)
      (comint-send-input)
      (interblock-R-wait-for-output))))

(defun interblock-R-wait-for-output ()
  "Wait until output arrives"
  (save-excursion
    (save-match-data
      (set-buffer interblock-R-buffer)
      (while (progn
	       (goto-char comint-last-input-end)
	       (not (re-search-forward comint-prompt-regexp nil t)))
	(accept-process-output (get-buffer-process (current-buffer)))))))

(defun interblock-R-last-output ()
  "Return the last R output as a string"
  (save-excursion
    (save-match-data
      (set-buffer interblock-R-buffer)
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (let ((raw (buffer-substring comint-last-input-end (- (point) 1))))
	(if (string-match "\n" raw)
	    raw
	  (and (string-match "\\[[[:digit:]+]\\] *\\(.*\\)$" raw)
	       (message raw)
	       (message (match-string 1 raw))
	       (match-string 1 raw)))))))

(provide 'org-exp-blocks)

;;; org-exp-blocks.el ends here
