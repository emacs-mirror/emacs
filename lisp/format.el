;;; format.el -- read and save files in multiple formats
;; Copyright (c) 1994, 1995 Free Software Foundation

;; Author: Boris Goldowsky <boris@gnu.ai.mit.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; This file defines a unified mechanism for saving & loading files stored in
;; different formats.  `format-alist' contains information that directs
;; Emacs to call an encoding or decoding function when reading or writing
;; files that match certain conditions.  
;;
;; When a file is visited, its format is determined by matching the beginning
;; of the file against regular expressions stored in `format-alist'.  If this
;; fails, you can manually translate the buffer using `format-decode-buffer'.
;; In either case, the formats used are listed in the variable
;; `buffer-file-format', and become the default format for saving the buffer.
;; To save a buffer in a different format, change this variable, or use
;; `format-write-file'.
;;
;; Auto-save files are normally created in the same format as the visited
;; file, but the variable `auto-save-file-format' can be set to a particularly
;; fast or otherwise preferred format to be used for auto-saving (or nil to do
;; no encoding on auto-save files, but then you risk losing any
;; text-properties in the buffer).
;;
;; You can manually translate a buffer into or out of a particular format with
;; the functions `format-encode-buffer' and `format-decode-buffer'.
;; To translate just the region use the functions `format-encode-region' and
;; `format-decode-region'.  
;;
;; You can define a new format by writing the encoding and decoding functions,
;; and adding an entry to `format-alist'.  See enriched.el for an example of
;; how to implement a file format.  There are various functions defined
;; in this file that may be useful for writing the encoding and decoding
;; functions:
;;  * `format-annotate-region' and `format-deannotate-region' allow a single
;;     alist of information to be used for encoding and decoding.  The alist
;;     defines a correspondence between strings in the file ("annotations")
;;     and text-properties in the buffer.
;;  * `format-replace-strings' is similarly useful for doing simple
;;     string->string translations in a reversible manner.

(put 'buffer-file-format 'permanent-local t)

(defconst format-alist 
  '((text/enriched "Extended MIME text/enriched format."
		   "Content-[Tt]ype:[ \t]*text/enriched"
		   enriched-decode enriched-encode t enriched-mode)
    (plain "Standard ASCII format, no text properties."
	   ;; Plain only exists so that there is an obvious neutral choice in
	   ;; the completion list.
	   nil nil nil nil nil))
  "List of information about understood file formats.
Elements are of the form \(NAME DOC-STR REGEXP FROM-FN TO-FN MODIFY MODE-FN).
NAME    is a symbol, which is stored in `buffer-file-format'.
DOC-STR should be a single line providing more information about the
        format.  It is currently unused, but in the future will be shown to
        the user if they ask for more information.
REGEXP  is a regular expression to match against the beginning of the file;
        it should match only files in that format.
FROM-FN is called to decode files in that format; it gets two args, BEGIN 
        and END, and can make any modifications it likes, returning the new
        end.  It must make sure that the beginning of the file no longer
        matches REGEXP, or else it will get called again.
TO-FN   is called to encode a region into that format; it is also passed BEGIN
        and END, and either returns a list of annotations like
        `write-region-annotate-functions', or modifies the region and returns
        the new end.
MODIFY, if non-nil, means the TO-FN modifies the region.  If nil, TO-FN may
        not make any changes and should return a list of annotations.
MODE-FN, if specified, is called when visiting a file with that format.")

;;; Basic Functions (called from Lisp)

(defun format-annotate-function (format from to)
  "Returns annotations for writing region as FORMAT.
FORMAT is a symbol naming one of the formats defined in `format-alist',
it must be a single symbol, not a list like `buffer-file-format'.
This function works like a function on `write-region-annotate-functions':
it either returns a list of annotations, or returns with a different buffer
current, which contains the modified text to write.

For most purposes, consider using `format-encode-region' instead."
  ;; This function is called by write-region (actually build-annotations) 
  ;; for each element of buffer-file-format.
  (let* ((info (assq format format-alist))
	 (to-fn  (nth 4 info))
	 (modify (nth 5 info)))
    (if to-fn
	(if modify
	    ;; To-function wants to modify region.  Copy to safe place.
	    (let ((copy-buf (get-buffer-create " *Format Temp*")))
	      (copy-to-buffer copy-buf from to)
	      (set-buffer copy-buf)
	      (format-insert-annotations write-region-annotations-so-far from)
	      (funcall to-fn (point-min) (point-max))
	      nil)
	  ;; Otherwise just call function, it will return annotations.
	  (funcall to-fn from to)))))

(defun format-decode (format length &optional visit-flag)
  ;; This function is called by insert-file-contents whenever a file is read.
  "Decode text from any known FORMAT.
FORMAT is a symbol appearing in `format-alist' or a list of such symbols, 
or nil, in which case this function tries to guess the format of the data by
matching against the regular expressions in `format-alist'.  After a match is
found and the region decoded, the alist is searched again from the beginning
for another match.

Second arg LENGTH is the number of characters following point to operate on.
If optional third arg VISIT-FLAG is true, set `buffer-file-format'
to the list of formats used, and call any mode functions defined for those
formats.

Returns the new length of the decoded region.

For most purposes, consider using `format-decode-region' instead."
  (let ((mod (buffer-modified-p))
	(begin (point))
	(end (+ (point) length)))
    (if (null format)
	;; Figure out which format it is in, remember list in `format'.
	(let ((try format-alist))
	  (while try
	    (let* ((f (car try))
		   (regexp (nth 2 f))
		   (p (point)))
	      (if (and regexp (looking-at regexp)
		       (< (match-end 0) (+ begin length)))
		  (progn
		    (setq format (cons (car f) format))
		    ;; Decode it
		    (if (nth 3 f) (setq end (funcall (nth 3 f) begin end)))
		    ;; Call visit function if required
		    (if (and visit-flag (nth 6 f)) (funcall (nth 6 f) 1))
		    ;; Safeguard against either of the functions changing pt.
		    (goto-char p)
		    ;; Rewind list to look for another format
		    (setq try format-alist))
		(setq try (cdr try))))))
      ;; Deal with given format(s)
      (or (listp format) (setq format (list format)))
      (let ((do format) f)
	(while do
	  (or (setq f (assq (car do) format-alist))
	      (error "Unknown format" (car do)))
	  ;; Decode:
	  (if (nth 3 f) (setq end (funcall (nth 3 f) begin end)))
	  ;; Call visit function if required
	  (if (and visit-flag (nth 6 f)) (funcall (nth 6 f) 1))
	  (setq do (cdr do)))))
    (if visit-flag
	(setq buffer-file-format format))
    (set-buffer-modified-p mod)
    ;; Return new length of region
    (- end begin)))

;;;
;;; Interactive functions & entry points
;;;

(defun format-decode-buffer (&optional format)
  "Translate the buffer from some FORMAT.
If the format is not specified, this function attempts to guess.
`buffer-file-format' is set to the format used, and any mode-functions 
for the format are called."
  (interactive
   (list (format-read "Translate buffer from format (default: guess): ")))
  (save-excursion
    (goto-char (point-min))
    (format-decode format (buffer-size) t)))

(defun format-decode-region (from to &optional format)
  "Decode the region from some format.
Arg FORMAT is optional; if omitted the format will be determined by looking
for identifying regular expressions at the beginning of the region."
  (interactive
   (list (region-beginning) (region-end) 
	 (format-read "Translate region from format (default: guess): ")))
  (save-excursion
    (goto-char from)
    (format-decode format (- to from) nil)))

(defun format-encode-buffer (&optional format)
  "Translate the buffer into FORMAT.
FORMAT defaults to `buffer-file-format'.  It is a symbol naming one of the
formats defined in `format-alist', or a list of such symbols."
  (interactive
   (list (format-read (format "Translate buffer to format (default %s): "
			      buffer-file-format))))
  (format-encode-region (point-min) (point-max) format))

(defun format-encode-region (beg end &optional format)
 "Translate the region into some FORMAT.
FORMAT defaults to `buffer-file-format', it is a symbol naming
one of the formats defined in `format-alist', or a list of such symbols."
 (interactive
  (list (region-beginning) (region-end)
	(format-read (format "Translate region to format (default %s): "
			     buffer-file-format))))
 (if (null format)    (setq format buffer-file-format))
 (if (symbolp format) (setq format (list format)))
 (save-excursion
   (goto-char end)
   (let ((cur-buf (current-buffer))
	 (end (point-marker)))
     (while format 
       (let* ((info (assq (car format) format-alist))
	      (to-fn  (nth 4 info))
	      (modify (nth 5 info))
	      result)
	 (if to-fn
	     (if modify
		 (setq end (funcall to-fn beg end))
	       (format-insert-annotations (funcall to-fn beg end))))
	 (setq format (cdr format)))))))

(defun format-write-file (filename format)
  "Write current buffer into a FILE using some FORMAT.
Makes buffer visit that file and sets the format as the default for future
saves.  If the buffer is already visiting a file, you can specify a directory
name as FILE, to write a file of the same old name in that directory."
  (interactive
   ;; Same interactive spec as write-file, plus format question.
   (let* ((file (if buffer-file-name
		    (read-file-name "Write file: "
				    nil nil nil nil)
		  (read-file-name "Write file: "
				  (cdr (assq 'default-directory
					     (buffer-local-variables)))
				  nil nil (buffer-name))))
	  (fmt (format-read (format "Write file `%s' in format: " 
				    (file-name-nondirectory file)))))
     (list file fmt)))
  (setq buffer-file-format format)
  (write-file filename))

(defun format-read (&optional prompt)
  "Read and return the name of a format.
Return value is a list, like `buffer-file-format'; it may be nil.
Formats are defined in `format-alist'.  Optional arg is the PROMPT to use."
  (let* ((table (mapcar (lambda (x) (list (symbol-name (car x))))
			format-alist))
	 (ans (completing-read (or prompt "Format: ") table nil t)))
    (if (not (equal "" ans)) (list (intern ans)))))


;;;
;;; Below are some functions that may be useful in writing encoding and
;;; decoding functions for use in format-alist.
;;;

(defun format-replace-strings (alist &optional reverse beg end)
  "Do multiple replacements on the buffer.
ALIST is a list of (from . to) pairs, which should be proper arguments to
`search-forward' and `replace-match' respectively.
Optional 2nd arg REVERSE, if non-nil, means the pairs are (to . from), so that
you can use the same list in both directions if it contains only literal
strings. 
Optional args BEGIN and END specify a region of the buffer to operate on."
  (save-excursion
    (save-restriction
      (or beg (setq beg (point-min)))
      (if end (narrow-to-region (point-min) end))
      (while alist
	(let ((from (if reverse (cdr (car alist)) (car (car alist))))
	      (to   (if reverse (car (cdr alist)) (cdr (car alist)))))
	  (goto-char beg)
	  (while (search-forward from nil t)
	    (goto-char (match-beginning 0))
	    (insert to)
	    (set-text-properties (- (point) (length to)) (point)
				 (text-properties-at (point)))
	    (delete-region (point) (+ (point) (- (match-end 0)
						 (match-beginning 0)))))
	  (setq alist (cdr alist)))))))

;;; Some list-manipulation functions that we need.

(defun format-delq-cons (cons list)
  "Remove the given CONS from LIST by side effect,
and return the new LIST.  Since CONS could be the first element 
of LIST, write `\(setq foo \(format-delq-cons element foo))' to be sure of 
changing the value of `foo'."
  (if (eq cons list)
      (cdr list)
    (let ((p list))
      (while (not (eq (cdr p) cons))
	(if (null p) (error "format-delq-cons: not an element."))
	(setq p (cdr p)))
      ;; Now (cdr p) is the cons to delete
      (setcdr p (cdr cons))
      list)))
    
(defun format-make-relatively-unique (a b)
  "Delete common elements of lists A and B, return as pair.
Compares using `equal'."
  (let* ((acopy (copy-sequence a))
	 (bcopy (copy-sequence b))
	 (tail acopy))
    (while tail
      (let ((dup (member (car tail) bcopy))
	    (next (cdr tail)))
	(if dup (setq acopy (format-delq-cons tail acopy)
		      bcopy (format-delq-cons dup  bcopy)))
	(setq tail next)))
    (cons acopy bcopy)))

(defun format-common-tail (a b)
  "Given two lists that have a common tail, return it.
Compares with `equal', and returns the part of A that is equal to the
equivalent part of B.  If even the last items of the two are not equal,
returns nil."
  (let ((la (length a))
	(lb (length b)))
    ;; Make sure they are the same length
    (if (> la lb) 
	(setq a (nthcdr (- la lb) a))
      (setq b (nthcdr (- lb la) b))))
  (while (not (equal a b))
    (setq a (cdr a)
	  b (cdr b)))
  a)

(defun format-reorder (items order)
  "Arrange ITEMS to following partial ORDER.
Elements of ITEMS equal to elements of ORDER will be rearranged to follow the
ORDER.  Unmatched items will go last."
  (if order
      (let ((item (member (car order) items)))
	(if item
	    (cons (car item) 
		  (format-reorder (format-delq-cons item items)
			   (cdr order)))
	  (format-reorder items (cdr order))))
    items))

(put 'face 'format-list-valued t)	; These text-properties take values
(put 'unknown 'format-list-valued t)	; that are lists, the elements of which
					; should be considered separately.
					; See format-deannotate-region and
					; format-annotate-region.

;;;
;;; Decoding
;;;

(defun format-deannotate-region (from to translations next-fn)
  "Translate annotations in the region into text properties.
This sets text properties between FROM to TO as directed by the 
TRANSLATIONS and NEXT-FN arguments.

NEXT-FN is a function that searches forward from point for an annotation.
It should return a list of 4 elements: \(BEGIN END NAME POSITIVE).  BEGIN and
END are buffer positions bounding the annotation, NAME is the name searched
for in TRANSLATIONS, and POSITIVE should be non-nil if this annotation marks
the beginning of a region with some property, or nil if it ends the region.
NEXT-FN should return nil if there are no annotations after point.

The basic format of the TRANSLATIONS argument is described in the
documentation for the `format-annotate-region' function.  There are some
additional things to keep in mind for decoding, though:

When an annotation is found, the TRANSLATIONS list is searched for a
text-property name and value that corresponds to that annotation.  If the
text-property has several annotations associated with it, it will be used only
if the other annotations are also in effect at that point.  The first match
found whose annotations are all present is used.

The text property thus determined is set to the value over the region between
the opening and closing annotations.  However, if the text-property name has a
non-nil `format-list-valued' property, then the value will be consed onto the
surrounding value of the property, rather than replacing that value.

There are some special symbols that can be used in the \"property\" slot of
the TRANSLATIONS list: PARAMETER and FUNCTION \(spelled in uppercase).
Annotations listed under the pseudo-property PARAMETER are considered to be
arguments of the immediately surrounding annotation; the text between the
opening and closing parameter annotations is deleted from the buffer but saved
as a string.  The surrounding annotation should be listed under the
pseudo-property FUNCTION.  Instead of inserting a text-property for this
annotation, the function listed in the VALUE slot is called to make whatever
changes are appropriate.  The function's first two arguments are the START and
END locations, and the rest of the arguments are any PARAMETERs found in that
region.

Any annotations that are found by NEXT-FN but not defined by TRANSLATIONS
are saved as values of the `unknown' text-property \(which is list-valued).
The TRANSLATIONS list should usually contain an entry of the form
    \(unknown \(nil format-annotate-value))
to write these unknown annotations back into the file."
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) to)
      (goto-char from)
      (let (next open-ans todo loc unknown-ans)
	(while (setq next (funcall next-fn))
	  (let* ((loc      (nth 0 next))
		 (end      (nth 1 next))
		 (name     (nth 2 next))
		 (positive (nth 3 next))
		 (found    nil))

	    ;; Delete the annotation
	    (delete-region loc end)
	    (if positive
		;; Positive annotations are stacked, remembering location
		(setq open-ans (cons (list name loc) open-ans))
	      ;; It is a negative annotation:
	      ;; Close the top annotation & add its text property.
	      ;; If the file's nesting is messed up, the close might not match
	      ;; the top thing on the open-annotations stack.
	      ;; If no matching annotation is open, just ignore the close.
	      (if (not (assoc name open-ans))
		  (message "Extra closing annotation (%s) in file" name)
	      ;; If one is open, but not on the top of the stack, close
	      ;; the things in between as well.  Set `found' when the real
	      ;; oneis closed.
		(while (not found)
		  (let* ((top (car open-ans)) ; first on stack: should match.
			 (top-name (car top))
			 (start (car (cdr top))) ; location of start
			 (params (cdr (cdr top))) ; parameters
			 (aalist translations)
			 (matched nil))
		    (if (equal name top-name)
			(setq found t)
		      (message "Improper nesting in file."))
		    ;; Look through property names in TRANSLATIONS
		    (while aalist
		      (let ((prop (car (car aalist)))
			    (alist (cdr (car aalist))))
			;; And look through values for each property
			(while alist
			  (let ((value (car (car alist)))
				(ans (cdr (car alist))))
			    (if (member top-name ans)
				;; This annotation is listed, but still have to
				;; check if multiple annotations are satisfied
				(if (member 'nil (mapcar 
						  (lambda (r)
						    (assoc r open-ans))
						  ans))
				    nil	; multiple ans not satisfied
				  ;; Yes, use the current property name &
				  ;; value.  Set loop variables to nil so loop
				  ;; will exit.
				  (setq alist nil aalist nil matched t
					;; pop annotation off stack.
					open-ans (cdr open-ans))
				  (cond 
				   ;; Check for pseudo-properties
				   ((eq prop 'PARAMETER)
				    ;; This is a parameter of the top open ann:
				    ;; delete text and use as arg.
				    (if open-ans
					;; (If nothing open, discard).
					(setq open-ans
					      (cons (append (car open-ans)
							    (list
							     (buffer-substring
							      start loc)))
						    (cdr open-ans))))
				    (delete-region start loc))
				   ((eq prop 'FUNCTION)
				    ;; Not a property, but a function to call.
				    (let ((rtn (apply value start loc params)))
				      (if rtn (setq todo (cons rtn todo)))))
				   (t 
				    ;; Normal property/value pair
				    (setq todo 
					  (cons (list start loc prop value)
						todo)))))))
			  (setq alist (cdr alist))))
		      (setq aalist (cdr aalist)))
		    (if matched
			nil
		      ;; Didn't find any match for the annotation:
		      ;; Store as value of text-property `unknown'.
		      (setq open-ans (cdr open-ans))
		      (setq todo (cons (list start loc 'unknown top-name)
				       todo))
		      (setq unknown-ans (cons name unknown-ans)))))))))

	;; Once entire file has been scanned, add the properties.
	(while todo
	  (let* ((item (car todo))
		 (from (nth 0 item))
		 (to   (nth 1 item))
		 (prop (nth 2 item))
		 (val  (nth 3 item)))
	
	    (put-text-property 
	       from to prop
	       (cond ((numberp val) ; add to ambient value if numeric
		      (+ val (or (get-text-property from prop) 0)))
		     ((get prop 'format-list-valued) ; value gets consed onto
						     ; list-valued properties
		      (let ((prev (get-text-property from prop)))
			(cons val (if (listp prev) prev (list prev)))))
		     (t val)))) ; normally, just set to val.
	  (setq todo (cdr todo)))
    
	(if unknown-ans
	    (message "Unknown annotations: %s" unknown-ans))))))

;;;
;;; Encoding
;;;

(defun format-insert-annotations (list &optional offset)
  "Apply list of annotations to buffer as `write-region' would.
Inserts each element of the given LIST of buffer annotations at its
appropriate place.  Use second arg OFFSET if the annotations' locations are
not relative to the beginning of the buffer: annotations will be inserted
at their location-OFFSET+1 \(ie, the offset is treated as the character number
of the first character in the buffer)."
  (if (not offset) 
      (setq offset 0)
    (setq offset (1- offset)))
  (let ((l (reverse list)))
    (while l
      (goto-char (- (car (car l)) offset))
      (insert (cdr (car l)))
      (setq l (cdr l)))))

(defun format-annotate-value (old new)
  "Return OLD and NEW as a \(close . open) annotation pair.
Useful as a default function for TRANSLATIONS alist when the value of the text
property is the name of the annotation that you want to use, as it is for the
`unknown' text property."
  (cons (if old (list old))
	(if new (list new))))

(defun format-annotate-region (from to trans format-fn ignore)
  "Generate annotations for text properties in the region.
Searches for changes between FROM and TO, and describes them with a list of
annotations as defined by alist TRANSLATIONS and FORMAT-FN.  IGNORE lists text
properties not to consider; any text properties that are neither ignored nor
listed in TRANSLATIONS are warned about.
If you actually want to modify the region, give the return value of this
function to `format-insert-annotations'.

Format of the TRANSLATIONS argument:

Each element is a list whose car is a PROPERTY, and the following
elements are VALUES of that property followed by the names of zero or more
ANNOTATIONS.  Whenever the property takes on that value, the annotations
\(as formatted by FORMAT-FN) are inserted into the file.
When the property stops having that value, the matching negated annotation
will be inserted \(it may actually be closed earlier and reopened, if
necessary, to keep proper nesting). 

If the property's value is a list, then each element of the list is dealt with
separately.

If a VALUE is numeric, then it is assumed that there is a single annotation
and each occurrence of it increments the value of the property by that number.
Thus, given the entry \(left-margin \(4 \"indent\")), if the left margin
changes from 4 to 12, two <indent> annotations will be generated.

If the VALUE is nil, then instead of annotations, a function should be
specified.  This function is used as a default: it is called for all
transitions not explicitly listed in the table.  The function is called with
two arguments, the OLD and NEW values of the property.  It should return
lists of annotations like `format-annotate-location' does.

    The same structure can be used in reverse for reading files."
  (let ((all-ans nil)    ; All annotations - becomes return value
	(open-ans nil)   ; Annotations not yet closed
	(loc nil)	 ; Current location
	(not-found nil)) ; Properties that couldn't be saved
    (while (or (null loc)
	       (and (setq loc (next-property-change loc nil to))
		    (< loc to)))
      (or loc (setq loc from))
      (let* ((ans (format-annotate-location loc (= loc from) ignore trans))
	     (neg-ans (format-reorder (aref ans 0) open-ans))
	     (pos-ans (aref ans 1))
	     (ignored (aref ans 2)))
	(setq not-found (append ignored not-found)
	      ignore    (append ignored ignore))
	;; First do the negative (closing) annotations
	(while neg-ans
	  ;; Check if it's missing.  This can happen (eg, a numeric property
	  ;; going negative can generate closing annotations before there are
	  ;; any open).  Warn user & ignore.
	  (if (not (member (car neg-ans) open-ans))
	      (message "Can't close %s: not open." (car neg-ans))
	    (while (not (equal (car neg-ans) (car open-ans)))
	      ;; To close anno. N, need to first close ans 1 to N-1,
	      ;; remembering to re-open them later.
	      (setq pos-ans (cons (car open-ans) pos-ans))
	      (setq all-ans 
		    (cons (cons loc (funcall format-fn (car open-ans) nil))
			  all-ans))
	      (setq open-ans (cdr open-ans)))
	    ;; Now remove the one we're really interested in from open list.
	    (setq open-ans (cdr open-ans))
	    ;; And put the closing annotation here.
	    (setq all-ans 
		  (cons (cons loc (funcall format-fn (car neg-ans) nil))
			all-ans)))
	  (setq neg-ans (cdr neg-ans)))
	;; Now deal with positive (opening) annotations
	(let ((p pos-ans))
	  (while pos-ans
	    (setq open-ans (cons (car pos-ans) open-ans))
	    (setq all-ans 
		  (cons (cons loc (funcall format-fn (car pos-ans) t))
			all-ans))
	    (setq pos-ans (cdr pos-ans))))))

    ;; Close any annotations still open
    (while open-ans
      (setq all-ans 
	    (cons (cons to (funcall format-fn (car open-ans) nil))
		  all-ans))
      (setq open-ans (cdr open-ans)))
    (if not-found
	(message "These text properties could not be saved:\n    %s"
		 not-found))
    (nreverse all-ans)))

;;; Internal functions for format-annotate-region.

(defun format-annotate-location (loc all ignore trans)
  "Return annotation(s) needed at LOCATION.
This includes any properties that change between LOC-1 and LOC.
If ALL is true, don't look at previous location, but generate annotations for
all non-nil properties.
Third argument IGNORE is a list of text-properties not to consider.

Return value is a vector of 3 elements:
1. List of names of the annotations to close
2. List of the names of annotations to open.
3. List of properties that were ignored or couldn't be annotated."
  (let* ((prev-loc (1- loc))
	 (before-plist (if all nil (text-properties-at prev-loc)))
	 (after-plist (text-properties-at loc))
	 p negatives positives prop props not-found)
    ;; make list of all property names involved
    (setq p before-plist)
    (while p
      (if (not (memq (car p) props))
	  (setq props (cons (car p) props)))
      (setq p (cdr (cdr p))))
    (setq p after-plist)
    (while p
      (if (not (memq (car p) props))
	  (setq props (cons (car p) props)))
      (setq p (cdr (cdr p))))

    (while props
      (setq prop (car props)
	    props (cdr props))
      (if (memq prop ignore)
	  nil  ; If it's been ignored before, ignore it now.
	(let ((before (if all nil (car (cdr (memq prop before-plist)))))
	      (after (car (cdr (memq prop after-plist)))))
	  (if (equal before after)
	      nil ; no change; ignore
	    (let ((result (format-annotate-single-property-change
			   prop before after trans)))
	      (if (not result)
		  (setq not-found (cons prop not-found))
		(setq negatives (nconc negatives (car result))
		      positives (nconc positives (cdr result)))))))))
    (vector negatives positives not-found)))

(defun format-annotate-single-property-change (prop old new trans)
  "Return annotations for PROPERTY changing from OLD to NEW.
These are searched for in the TRANSLATIONS alist.
If NEW does not appear in the list, but there is a default function, then that
function is called.
Annotations to open and to close are returned as a dotted pair."
  (let ((prop-alist (cdr (assoc prop trans)))
	default)
    (if (not prop-alist)
	nil
      ;; If property is numeric, nil means 0
      (cond ((and (numberp old) (null new))
	     (setq new 0))
	    ((and (numberp new) (null old))
	     (setq old 0)))
      ;; If either old or new is a list, have to treat both that way.
      (if (or (consp old) (consp new))
	  (let* ((old (if (listp old) old (list old)))
		 (new (if (listp new) new (list new)))
		 (tail (format-common-tail old new))
		 close open)
	    (while old
	      (setq close 
		    (append (car (format-annotate-atomic-property-change
				  prop-alist (car old) nil))
			    close)
		    old (cdr old)))
	    (while new
	      (setq open 
		    (append (cdr (format-annotate-atomic-property-change
				  prop-alist nil (car new)))
			    open)
		    new (cdr new)))
	    (format-make-relatively-unique close open))
	(format-annotate-atomic-property-change prop-alist old new)))))

(defun format-annotate-atomic-property-change (prop-alist old new)
  "Internal function annotate a single property change.
PROP-ALIST is the relevant segement of a TRANSLATIONS list.
OLD and NEW are the values."
  (cond
   ;; Numerical annotation - use difference
   ((and (numberp old) (numberp new))
    (let* ((entry (progn
		    (while (and (car (car prop-alist))
				(not (numberp (car (car prop-alist)))))
		      (setq prop-alist (cdr prop-alist)))
		    (car prop-alist)))
	   (increment (car (car prop-alist)))
	   (n (ceiling (/ (float (- new old)) (float increment))))
	   (anno (car (cdr (car prop-alist)))))
      (if (> n 0)
	  (cons nil (make-list n anno))
	(cons (make-list (- n) anno) nil))))

   ;; Standard annotation
   (t (let ((close (and old (cdr (assoc old prop-alist))))
	    (open  (and new (cdr (assoc new prop-alist)))))
	(if (or close open)
	    (format-make-relatively-unique close open)
	  ;; Call "Default" function, if any
	  (let ((default (assq nil prop-alist)))
	    (if default
		(funcall (car (cdr default)) old new))))))))

;; format.el ends here
