;;; xml-lite.el --- an indentation-engine for XML

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author:     Mike Williams <mdub@bigfoot.com>
;; Created:    February 2001
;; Keywords:   xml

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This package provides a simple indentation engine for XML.  It is
;; intended for use in situations where the full power of the popular PSGML
;; package (DTD parsing, syntax checking) is not required.
;;
;; xml-lite is designed to be used in conjunction with the default GNU
;; Emacs sgml-mode, to provide a lightweight XML-editing environment.

;;; Thanks:
;;
;;    Jens Schmidt <Jens.Schmidt@oracle.com>
;;        for his feedback and suggestions

;;; Code:

(eval-when-compile (require 'cl))
(require 'sgml-mode)


;; Syntax analysis

(defsubst xml-lite-at-indentation-p ()
  "Return true if point is at the first non-whitespace character on the line."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))


;; Parsing

(defstruct (xml-lite-tag
            (:constructor xml-lite-make-tag (type start end name)))
  type start end name)

(defsubst xml-lite-parse-tag-name ()
  "Skip past a tag-name, and return the name."
  (buffer-substring-no-properties
   (point) (progn (skip-syntax-forward "w_") (point))))

(defsubst xml-lite-looking-back-at (s)
  (let ((limit (max (- (point) (length s)) (point-min))))
    (equal s (buffer-substring-no-properties limit (point)))))

(defsubst xml-lite-looking-at (s)
  (let ((limit (min (+ (point) (length s)))))
    (equal s (buffer-substring-no-properties (point) limit))))

(defun xml-lite-parse-tag-backward ()
  "Parse an SGML tag backward, and return information about the tag.
Assume that parsing starts from within a textual context.
Leave point at the beginning of the tag."
  (let (tag-type tag-start tag-end name)
    (search-backward ">")
    (setq tag-end (1+ (point)))
    (cond
     ((xml-lite-looking-back-at "--")   ; comment
      (setq tag-type 'comment
            tag-start (search-backward "<!--" nil t)))
     ((xml-lite-looking-back-at "]]")   ; cdata
      (setq tag-type 'cdata
            tag-start (search-backward "<![CDATA[" nil t)))
     (t
      (setq tag-start
            (with-syntax-table sgml-tag-syntax-table
              (goto-char tag-end)
              (backward-sexp)
              (point)))
      (goto-char (1+ tag-start))
      (case (char-after)
        (?!                             ; declaration
         (setq tag-type 'decl))
        (??                             ; processing-instruction
         (setq tag-type 'pi))
        (?/                             ; close-tag
         (forward-char 1)
         (setq tag-type 'close
               name (xml-lite-parse-tag-name)))
        ((?% ?#)                        ; JSP tags etc
         (setq tag-type 'unknown))
        (t                              ; open or empty tag
         (setq tag-type 'open
               name (xml-lite-parse-tag-name))
         (if (eq ?/ (char-before (- tag-end 1)))
             (setq tag-type 'empty))))))
    (goto-char tag-start)
    (xml-lite-make-tag tag-type tag-start tag-end name)))

(defsubst xml-lite-inside-tag-p (tag-info &optional point)
  "Return true if TAG-INFO contains the POINT."
  (let ((end (xml-lite-tag-end tag-info))
        (point (or point (point))))
    (or (null end)
        (> end point))))

(defun xml-lite-get-context (&optional full)
  "Determine the context of the current position.
If FULL is `empty', return even if the context is empty (i.e.
we just skipped over some element and got to a beginning of line).
If FULL is non-nil, parse back to the beginning of the buffer, otherwise
parse until we find a start-tag as the first thing on a line.

The context is a list of tag-info structures.  The last one is the tag
immediately enclosing the current position."
  (let ((here (point))
	(ignore nil)
	(context nil)
	tag-info)
    ;; CONTEXT keeps track of the tag-stack
    ;; IGNORE keeps track of the nesting level of point relative to the
    ;;   first (outermost) tag on the context.  This is the list of
    ;;   enclosing start-tags we'll have to ignore.
    (skip-chars-backward " \t\n")      ; Make sure we're not at indentation.
    (while
	(and (or ignore 
                 (not (if full (eq full 'empty) context))
		 (not (xml-lite-at-indentation-p))
		 (and context
		      (/= (point) (xml-lite-tag-start (car context)))
                      (sgml-unclosed-tag-p (xml-lite-tag-name (car context)))))
	     (setq tag-info (ignore-errors (xml-lite-parse-tag-backward))))
      
      ;; This tag may enclose things we thought were tags.  If so,
      ;; discard them.
      (while (and context
                  (> (xml-lite-tag-end tag-info)
                     (xml-lite-tag-end (car context))))
        (setq context (cdr context)))
           
      (cond

       ;; inside a tag ...
       ((xml-lite-inside-tag-p tag-info here)
	(push tag-info context))

       ;; start-tag
       ((eq (xml-lite-tag-type tag-info) 'open)
	(cond
	 ((null ignore)
	  (if (and context
                   (sgml-unclosed-tag-p (xml-lite-tag-name tag-info))
		   (eq t (compare-strings
			  (xml-lite-tag-name tag-info) nil nil
			  (xml-lite-tag-name (car context)) nil nil t)))
	      ;; There was an implicit end-tag.
	      nil
	    (push tag-info context)))
	 ((eq t (compare-strings (xml-lite-tag-name tag-info) nil nil
				 (car ignore) nil nil t))
	  (setq ignore (cdr ignore)))
	 (t
	  ;; The open and close tags don't match.
	  (if (not sgml-xml-mode)
	      ;; Assume the open tag is simply not closed.
	      (unless (sgml-unclosed-tag-p (xml-lite-tag-name tag-info))
		(message "Unclosed tag <%s>" (xml-lite-tag-name tag-info)))
	    (message "Unmatched tags <%s> and </%s>"
		     (xml-lite-tag-name tag-info) (pop ignore))))))

       ;; end-tag
       ((eq (xml-lite-tag-type tag-info) 'close)
	(if (sgml-empty-tag-p (xml-lite-tag-name tag-info))
	    (message "Spurious </%s>: empty tag" (xml-lite-tag-name tag-info))
	  (push (xml-lite-tag-name tag-info) ignore)))
       ))

    ;; return context
    context))

(defun xml-lite-show-context (&optional full)
  "Display the current context.
If FULL is non-nil, parse back to the beginning of the buffer."
  (interactive "P")
  (with-output-to-temp-buffer "*XML Context*"
    (pp (save-excursion (xml-lite-get-context full)))))


;; Editing shortcuts

(defun xml-lite-insert-end-tag ()
  "Insert an end-tag for the current element."
  (interactive)
  (let* ((context (save-excursion (xml-lite-get-context)))
         (tag-info (car (last context)))
         (type (and tag-info (xml-lite-tag-type tag-info))))

    (cond

     ((null context)
      (error "Nothing to close"))

     ;; inside a tag
     ((xml-lite-inside-tag-p tag-info)
      (insert (cond
	       ((eq type 'open) 	" />")
	       ((eq type 'comment)	" -->")
	       ((eq type 'cdata)	"]]>")
	       ((eq type 'jsp) 		"%>")
	       ((eq type 'pi) 		"?>")
	       (t 			">"))))

     ;; inside an element
     ((eq type 'open)
      (insert "</" (xml-lite-tag-name tag-info) ">")
      (indent-according-to-mode))

     (t
      (error "Nothing to close")))))

(defun xml-lite-slash (arg)
  "Insert ARG slash characters.
Behaves electrically if `xml-lite-electric-slash' is non-nil."
  (interactive "p")
  (cond
   ((not (and (eq (char-before) ?<) (= arg 1)))
    (insert-char ?/ arg))
   ((eq xml-lite-electric-slash 'indent)
    (insert-char ?/ 1)
    (indent-according-to-mode))
   ((eq xml-lite-electric-slash 'close)
    (delete-backward-char 1)
    (xml-lite-insert-end-tag))
   (t
    (insert-char ?/ arg))))

(provide 'xml-lite)

;;; xml-lite.el ends here
