;; Jim's Pretty-Good PostScript Generator for Emacs 19 (ps-print).
;; Copyright (C) 1993, 1994 Free Software Foundation, Inc.

;; Author: James C. Thompson <thompson@wg2.waii.com>
;; Keywords: faces, postscript, printing

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

;; Acknowledgements
;; ----------------
;; Thanks to Avishai Yacobi, avishaiy@mcil.comm.mot.com, for writing
;; the Emacs 19 port.
;;
;; Thanks to Remi Houdaille and Michel Train, michel@metasoft.fdn.org,
;; for adding underline support and title code.  (Titling will appear
;; in the next release.)
;;
;; Thanks to Heiko Muenkel, muenkel@tnt.uni-hannover.de, for showing
;; me how to handle ISO-8859/1 characters.
;;
;; Code to handle ISO-8859/1 characters borrowed from the mp prologue
;; file mp.pro.ps, used with permission of Rich Burridge of Sun
;; Microsystems (Rich.Burridge@eng.sun.com).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; About ps-print:
;; --------------
;; This package provides printing of Emacs buffers on PostScript
;; printers; the buffer's bold and italic text attributes are
;; preserved in the printer output.  Ps-print is intended for use with
;; Emacs 19 (Lucid or FSF) and a fontifying package such as font-lock
;; or hilit.
;; 
;; Installing ps-print:
;; -------------------
;; Place ps-print somewhere in your load-path and byte-compile it.
;; Load ps-print with (require 'ps-print).
;;
;; Using ps-print:
;; --------------
;; The variables ps-bold-faces and ps-italic-faces *must* contain
;; lists of the faces that you wish to print in bold or italic font.
;; These variables already contain some default values, but most users
;; will probably have to add some of their own.  To add a face to one
;; of these lists, put code something like the following into your
;; .emacs startup file:
;;
;;   (setq ps-bold-faces (cons 'my-bold-face ps-bold-faces))
;;
;; Ps-print's printer interface is governed by the variables ps-lpr-
;; command and ps-lpr-switches; these are analogous to the variables
;; lpr-command and lpr-switches in the Emacs lpr package.
;;
;; To use ps-print, invoke the command ps-print-buffer-with-faces.
;; This will generate a PostScript image of the current buffer and
;; send it to the printer.  Precede this command with a numeric prefix
;; (C-u), and the PostScript output will be saved in a file; you will
;; be prompted for a filename.  Also see the functions ps-print-
;; buffer, ps-print-region, and ps-print-region-with-faces.
;;
;; I recommend binding ps-print-buffer-with-faces to a key sequence;
;; on a Sun 4 keyboard, for example, you can bind to the PrSc key (aka
;; r22):
;;
;;   (global-set-key 'f22 'ps-print-buffer-with-faces)
;;   (global-set-key '(shift f22) 'ps-print-region-with-faces)
;;
;; Or, as I now prefer, you can also bind the ps-spool- functions to
;; keys; here's my bindings:
;;
;;   (global-set-key 'f22 'ps-spool-buffer-with-faces)
;;   (global-set-key '(shift f22) 'ps-spool-region-with-faces)
;;   (global-set-key '(control f22) 'ps-despool)
;;
;; Using ps-print with other Emacses:
;; ---------------------------------
;; Although it was intended for use with Emacs 19, ps-print will also work
;; with Emacs version 18; you won't get fancy fontified output, but it
;; should work.
;; 
;; A few words about support:
;; -------------------------
;; Despite its appearance, with comment blocks, usage instructions, and
;; documentation strings, ps-print is not a supported package.  That's all
;; a masquerade.  Ps-print is something I threw together in my spare time--
;; an evening here, a Saturday there--to make my printouts look like my
;; Emacs buffers.  It works, but is not complete.
;;
;; Unfortunately, supporting elisp code is not my job and, now that I have
;; what I need out of ps-print, additional support is going to be up to
;; you, the user.  But that's the spirit of Emacs, isn't it?  I call on
;; all who use this package to help in developing it further. If you
;; notice a bug, fix it and send me the patches.  If you add a feature,
;; again, send me the patches.  I will collect all such contributions and
;; periodically post the updates to the appropriate places.
;;
;; A few more words about support:
;; ------------------------------
;; The response to my call for public support of ps-print has been
;; terrific.  With the exception of the spooling mechanism, all the new
;; features in this version of ps-print were contributed by users.  I have
;; some contributed code for printing headers that I'll add to the next
;; release of ps-print, but there are still other features that users can
;; write.  See the "Features to Add" list a little further on, and keep
;; that elisp rolling in.
;;
;; Please send all bug fixes and enhancements to me, thompson@wg2.waii.com.
;;
;; New in version 1.5
;; ------------------
;; Support for Emacs 19.  Works with both overlays and text
;; properties.
;;
;; Underlining.
;;
;; Local spooling; see function ps-spool-buffer.
;;
;; Support for ISO8859-1 character set.
;;
;; Page breaks are now handled correctly.
;;
;; Percentages reported while formatting are now correct.
;;
;; Known bugs and limitations of ps-print:
;; --------------------------------------
;; Slow.  (Byte-compiling helps.)
;;
;; The PostScript needs review/cleanup/enhancing by a PS expert.
;; 
;; ASCII Control characters other than tab, linefeed and pagefeed are
;; not handled.
;;
;; The mechanism for determining whether a stretch of characters
;; should be printed bold, italic, or plain is crude and extremely
;; limited.
;;
;; Faces are always treated as opaque.
;;
;; Font names are hardcoded.
;;
;; Epoch not fully supported.
;;
;; Tested with only one PostScript printer.
;;
;; Features to add:
;; ---------------
;; Line numbers.
;;
;; Simple headers with date, filename, and page numbers.
;;
;; Gaudy headers a`la enscript and mp.
;;
;; 2-up and 4-up capability.
;;
;; Wide-print capability.
;;

;;; Code:

(defconst ps-print-version  (substring "$Revision: 1.5 $" 11 -2)
  "$Id: ps-print.el,v 1.5 1994/04/22 13:25:18 jct Exp $

Please send all bug fixes and enhancements to Jim Thompson,
thompson@wg2.waii.com.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ps-lpr-command (if (memq system-type
				 '(usg-unix-v hpux silicon-graphics-unix))
			   "lp" "lpr")
  "The shell command for printing a PostScript file.")

(defvar ps-lpr-switches nil
  "A list of extra switches to pass to ps-lpr-command.")

(defvar ps-bold-faces
  '(bold
    bold-italic
    font-lock-function-name-face
    message-headers
    )
  "A list of the faces that should be printed italic.")

(defvar ps-italic-faces
  '(italic
    bold-italic
    font-lock-function-name-face
    font-lock-string-face
    font-lock-comment-face
    message-header-contents
    message-highlighted-header-contents
    message-cited-text
    )
  "A list of the faces that should be printed bold.")

(defvar ps-underline-faces
  '(underline
    font-lock-string-face)
  "A list of the faces that should be printed underline.")

(defvar ps-razzle-dazzle t
  "Non-nil means report progress while formatting buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-print-buffer (&optional filename)

"Generate and print a PostScript image of the buffer.

When called with a numeric prefix argument (C-u), prompt the user for
the name of a file to save the PostScript image in, instead of sending
it to the printer.

More specifically, the FILENAME argument is treated as follows: if it
is nil, send the image to the printer.  If FILENAME is a string, save
the PostScript image in a file with that name.  If FILENAME is a
number, prompt the user for the name of the file to save in.

The image is rendered using the PostScript font Courier.

See also: ps-print-buffer-with-faces
          ps-spool-buffer
          ps-spool-buffer-with-faces"

  (interactive "P")
  (setq filename (ps-preprint filename))
  (ps-generate (current-buffer) (point-min) (point-max)
	       'ps-generate-postscript)
  (ps-do-despool filename))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-print-buffer-with-faces (&optional filename)

"Generate and print a PostScript image of the buffer.

This function works like ps-print-buffer, with the additional benefit
that any bold/italic formatting information present in the buffer
(contained in extents and faces) will be retained in the PostScript
image. In other words, WYSIAWYG -- What You See Is (Almost) What You
Get.

Ps-print uses three lists to determine which faces should be printed
bold, italic, and/or underlined; the lists are named ps-bold-faces, ps-
italic-faces, and ps-underline-faces.  A given face should appear on as
many lists as are appropriate; for example, face bold-italic is in both
the lists ps-bold-faces and ps-italic-faces.  The lists are pre-built
with the standard bold, italic, and bold-italic faces, with font-lock's
faces, and with the faces used by gnus and rmail.

The image is rendered using the PostScript fonts Courier, Courier-Bold,
Courier-Oblique, and Courier-BoldOblique.

See also: ps-print-buffer
          ps-spool-buffer
          ps-spool-buffer-with-faces."

  (interactive "P")
  (setq filename (ps-preprint filename))
  (ps-generate (current-buffer) (point-min) (point-max)
	       'ps-generate-postscript-with-faces)
  (ps-do-despool filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-print-region (from to &optional filename)

"Generate and print a PostScript image of the region.

When called with a numeric prefix argument (C-u), prompt the user for
the name of a file to save the PostScript image in, instead of sending
it to the printer.

This function is essentially the same as ps-print-buffer except that it
prints just a region, and not the entire buffer.  For more information,
see the function ps-print-buffer.

See also: ps-print-region-with-faces
          ps-spool-region
          ps-spool-region-with-faces"
  
  (interactive "r\nP")
  (setq filename (ps-preprint filename))
  (ps-generate (current-buffer) from to
	       'ps-generate-postscript)
  (ps-do-despool filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-print-region-with-faces (from to &optional filename)

"Generate and print a PostScript image of the region.

This function is essentially the same as ps-print-buffer except that it
prints just a region, and not the entire buffer.  See the functions
ps-print-region and ps-print-buffer-with-faces for
more information.

See also: ps-print-region
          ps-spool-region
          ps-spool-region-with-faces"
  
  (interactive "r\nP")
  (setq filename (ps-preprint filename))
  (ps-generate (current-buffer) from to
	       'ps-generate-postscript-with-faces)
  (ps-do-despool filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-spool-buffer ()

"Generate and spool a PostScript image of the buffer.

This function is essentially the same as function ps-print-buffer
except that the PostScript image is saved in a local buffer to be sent
to the printer later.

Each time you call one of the ps-spool- functions, the generated
PostScript is appended to a buffer named *PostScript*; to send the
spooled PostScript to the printer, or save it to a file, use the command
ps-despool.

If the variable ps-spool-duplex is non-nil, then the spooled PostScript
is padded with blank pages, when needed, so that each printed buffer
will start on a front page when printed on a duplex printer (a printer
that prints on both sides on the paper).  Users of non-duplex printers
will want to leave ps-spool-duplex nil.

The spooling mechanism was designed for printing lots of small files
(mail messages or netnews articles) to save paper that would otherwise
be wasted on banner pages, and to make it easier to find your output at
the printer (it's easier to pick up one 50-page printout than to find 50
single-page printouts).

Ps-print has a hook in the kill-emacs-hook list so that you won't
accidently quit from Emacs while you have unprinted PostScript waiting
in the spool buffer.  If you do attempt to exit with spooled PostScript,
you'll be asked if you want to print it, and if you decline, you'll be
asked to confirm the exit; this is modeled on the confirmation that
Emacs uses for modified buffers.

See also: ps-despool
          ps-print-buffer
          ps-print-buffer-with-faces
          ps-spool-buffer-with-faces"

  (interactive)
  (ps-generate (current-buffer) (point-min) (point-max)
	       'ps-generate-postscript))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-spool-buffer-with-faces ()

"Generate and spool PostScript image of the buffer.

This function is essentially the same as function ps-print-buffer-with-
faces except that the PostScript image is saved in a local buffer to be
sent to the printer later.

Use the function ps-despool to send the spooled images to the printer.
See the function ps-spool-buffer for a description of the spooling
mechanism.

See also: ps-despool
          ps-spool-buffer
          ps-print-buffer
          ps-print-buffer-with-faces"

  (interactive)
  (ps-generate (current-buffer) (point-min) (point-max)
	       'ps-generate-postscript-with-faces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-spool-region (from to)

"Generate PostScript image of the region and spool locally.

This function is essentially the same as function ps-print-region except
that the PostScript image is saved in a local buffer to be sent to the
printer later.

Use the function ps-despool to send the spooled images to the printer.
See the function ps-spool-buffer for a description of the spooling
mechanism.

See also: ps-despool
          ps-spool-buffer
          ps-print-buffer
          ps-print-buffer-with-faces"

  (interactive "r")
  (ps-generate (current-buffer) from to
	       'ps-generate-postscript))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-spool-region-with-faces (from to)

"Generate PostScript image of the region and spool locally.

This function is essentially the same as function ps-print-region-with-
faces except that the PostScript image is saved in a local buffer to be
sent to the printer later.

Use the function ps-despool to send the spooled images to the printer.
See the function ps-spool-buffer for a description of the spooling
mechanism.

See also: ps-despool
          ps-spool-buffer
          ps-print-buffer
          ps-print-buffer-with-faces"

  (interactive "r")
  (ps-generate (current-buffer) from to
	       'ps-generate-postscript-with-faces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ps-spool-duplex nil		; Not many people have duplex
					; printers, so default to nil.
  "*Non-nil indicates spooling is for a two-sided printer.
For a duplex printer, the ps-spool functions will insert blank pages
as needed between print jobs so that the next buffer printed will
start on the right page.")

(defun ps-despool (&optional filename)
  "Send the spooled PostScript to the printer.

When called with a numeric prefix argument (C-u), prompt the user for
the name of a file to save the spooled PostScript in, instead of sending
it to the printer.

More specifically, the FILENAME argument is treated as follows: if it
is nil, send the image to the printer.  If FILENAME is a string, save
the PostScript image in a file with that name.  If FILENAME is a
number, prompt the user for the name of the file to save in."

  (interactive "P")

;; If argument FILENAME is nil, send the image to the printer; if
;; FILENAME is a string, save the PostScript image in that filename;
;; if FILENAME is a number, prompt the user for the name of the file
;; to save in.

  (setq filename (ps-preprint filename))
  (ps-do-despool filename))

;; Here end the definitions that users need to know about; proceed
;; further at your own risk!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ps-kill-emacs-check ()
  (if (and (setq ps-buffer (get-buffer ps-spool-buffer-name))
	   (buffer-modified-p ps-buffer))
      (if (y-or-n-p "Unprinted PostScript waiting... print now? ")
	  (ps-despool)))

  (if (and (setq ps-buffer (get-buffer ps-spool-buffer-name))
	   (buffer-modified-p ps-buffer))
      (if (yes-or-no-p "Unprinted PostScript waiting; exit anyway? ")
	  nil
	(error "Unprinted PostScript"))))

(if (fboundp 'add-hook)
    (add-hook 'kill-emacs-hook 'ps-kill-emacs-check)
  (if kill-emacs-hook
      (message "Won't override existing kill-emacs-hook.")
    (setq kill-emacs-hook 'ps-kill-emacs-check)))

(defun ps-preprint (&optional filename)
  (if (and filename
	   (or (numberp filename)
	       (listp filename)))
      (setq filename
	    (let* ((name (concat (buffer-name) ".ps"))
		   (prompt (format "Save PostScript to file: (default %s) "
				   name)))
	      (read-file-name prompt default-directory
			      name nil)))))

(defvar ps-spool-buffer-name "*PostScript*")

(defvar ps-col 0)
(defvar ps-row 0)
(defvar ps-xpos 0)
(defvar ps-ypos 0)

(defvar ps-chars-per-line 80)
(defvar ps-lines-per-page 66)

(defvar ps-page-start-ypos 745)
(defvar ps-line-start-xpos 40)

(defvar ps-char-xpos-inc 6)
(defvar ps-line-ypos-inc 11)

(defvar ps-current-font 0)

(defvar ps-multiple nil)
(defvar ps-virtual-page-number 0)

(defun ps-begin-file ()
  (save-excursion
    (set-buffer ps-output-buffer)
    (goto-char (point-min))
    (setq ps-real-page-number 1)
    (insert
"%!PS-Adobe-1.0

/S /show load def
/M /moveto load def
/L { gsave newpath 3 1 roll 1 sub M 0 rlineto closepath stroke grestore } def

/F{$fd exch get setfont}def

/StartPage{/svpg save def}def
/EndPage{svpg restore showpage}def

/SetUpFonts
 {dup/$fd exch array def{findfont exch scalefont $fd 3 1 roll put}repeat}def

% Define /ISOLatin1Encoding only if it's not already there.
/ISOLatin1Encoding where { pop save true }{ false } ifelse
/ISOLatin1Encoding [ StandardEncoding 0 45 getinterval aload pop /minus
  StandardEncoding 46 98 getinterval aload pop /dotlessi /grave /acute
  /circumflex /tilde /macron /breve /dotaccent /dieresis /.notdef /ring
  /cedilla /.notdef /hungarumlaut /ogonek /caron /space /exclamdown /cent
  /sterling /currency /yen /brokenbar /section /dieresis /copyright
  /ordfeminine /guillemotleft /logicalnot /hyphen /registered /macron
  /degree /plusminus /twosuperior /threesuperior /acute /mu /paragraph
  /periodcentered /cedilla /onesuperior /ordmasculine /guillemotright
  /onequarter /onehalf /threequarters /questiondown /Agrave /Aacute
  /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla /Egrave /Eacute
  /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex /Idieresis /Eth
  /Ntilde /Ograve /Oacute /Ocircumflex /Otilde /Odieresis /multiply
  /Oslash /Ugrave /Uacute /Ucircumflex /Udieresis /Yacute /Thorn
  /germandbls /agrave /aacute /acircumflex /atilde /adieresis /aring /ae
  /ccedilla /egrave /eacute /ecircumflex /edieresis /igrave /iacute
  /icircumflex /idieresis /eth /ntilde /ograve /oacute /ocircumflex
  /otilde /odieresis /divide /oslash /ugrave /uacute /ucircumflex
  /udieresis /yacute /thorn /ydieresis ] def
{ restore } if

/reencodeISO { %def
  findfont dup length dict begin
    { 1 index /FID ne { def }{ pop pop } ifelse } forall
    /Encoding ISOLatin1Encoding def
    currentdict end definefont pop
} bind def

/CourierISO /Courier          	      	     reencodeISO
/Courier-ObliqueISO /Courier-Oblique  	     reencodeISO
/Courier-BoldISO /Courier-Bold        	     reencodeISO
/Courier-BoldObliqueISO /Courier-BoldOblique reencodeISO

3 10 /Courier-BoldObliqueISO
2 10 /Courier-ObliqueISO
1 10 /Courier-BoldISO
0 10 /CourierISO
4 SetUpFonts

.4 setlinewidth
")))

(defun ps-end-file ()
  )

(defun ps-next-page ()
  (ps-end-page)
  (ps-begin-page)
  (ps-set-font ps-current-font)
  (ps-init-page))

(defun ps-top-of-page () (ps-next-page))

(defun ps-init-page ()
  (setq ps-row 0)
  (setq ps-col 0)
  (setq ps-ypos ps-page-start-ypos)
  (setq ps-xpos ps-line-start-xpos)
  (ps-set-font))

(defun ps-begin-page ()
  (save-excursion
    (set-buffer ps-output-buffer)
    (goto-char (point-max))
    (insert (format "%%%%Page: ? %d\n" ps-real-page-number))
    (setq ps-real-page-number (+ 1 ps-real-page-number))
    (insert "StartPage\n0.4 setlinewidth\n")))

(defun ps-end-page ()
  (save-excursion
    (set-buffer ps-output-buffer)
    (goto-char (point-max))
    (insert "EndPage\n")))

(defun ps-next-line ()
  (setq ps-row (+ ps-row 1))
  (if (>= ps-row ps-lines-per-page)
      (ps-next-page)
    (setq ps-col 0)
    (setq ps-xpos ps-line-start-xpos)
    (setq ps-ypos (- ps-ypos ps-line-ypos-inc))))

(defun ps-continue-line ()
  (ps-next-line))

(defvar ps-source-buffer nil)
(defvar ps-output-buffer nil)

(defun ps-basic-plot-string (from to &optional underline-p)
  (setq text (buffer-substring from to))
  (save-excursion
    (set-buffer ps-output-buffer)
    (goto-char (point-max))
    (setq count (- to from))

    (if underline-p
	(insert (format "%d %d %d L\n" ps-xpos ps-ypos
			(* count ps-char-xpos-inc))))
  
    (insert (format "%d %d M (" ps-xpos ps-ypos))
    (save-excursion
      (insert text))

    (while (re-search-forward "[()\\]" nil t)
      (save-excursion
        (forward-char -1)
        (insert "\\")))
    
    (end-of-line)
    (insert ") S\n")

    (setq ps-xpos (+ ps-xpos (* count ps-char-xpos-inc)))))

(defun ps-basic-plot-whitespace (from to underline-p)
  (setq count (- to from))
  (setq ps-xpos (+ ps-xpos (* count ps-char-xpos-inc))))

(defun ps-plot (plotfunc from to &optional underline-p)

  (while (< from to)
    (setq count (- to from))
    ;; Test to see whether this region will fit on the current line
    (if (<= (+ ps-col count) ps-chars-per-line)
        (progn
          ;; It fits; plot it.
          (funcall plotfunc from to underline-p)
          (setq from to))
    
      ;; It needs to be wrapped;  plot part of it, then loop
      (setq chars-that-will-fit (- ps-chars-per-line ps-col))
      (funcall plotfunc from (+ from chars-that-will-fit))
      
      (ps-continue-line)
      
      (setq from (+ from chars-that-will-fit))))

  (if ps-razzle-dazzle
      (let* ((q-todo (- (point-max) (point-min)))
	     (q-done (- to (point-min)))
	     (chunkfrac (/ q-todo 8))
	     (chunksize (if (> chunkfrac 10000) 10000 chunkfrac)))
	(if (> (- q-done ps-razchunk) chunksize)
	    (progn
	      (setq ps-razchunk q-done)
	      (setq foo
		    (if (< q-todo 100)
			(* (/ q-done q-todo) 100)
		      (setq basis (/ q-todo 100))
		      (/ q-done basis)))

	      (message "Formatting... %d%%" foo))))))

(defun ps-set-font (&optional font)
  (save-excursion
    (set-buffer ps-output-buffer)
    (goto-char (point-max))
    (insert (format "%d F\n" (if font font ps-current-font))))
  (if font
      (setq ps-current-font font)))

(defun ps-plot-region (from to font &optional underline-p)

  (ps-set-font font)

  (save-excursion
    (goto-char from)
    (while (< from to)
      (if (re-search-forward "[\t\n\014]" to t)
          (let ((match (char-after (match-beginning 0))))
            (cond
             ((= match ?\n)
              (ps-plot 'ps-basic-plot-string from (- (point) 1) underline-p)
              (ps-next-line))

             ((= match ?\t)
              (ps-plot 'ps-basic-plot-string from (- (point) 1) underline-p)
	      (setq linestart (save-excursion (beginning-of-line) (point)))
              (forward-char -1)
              (setq from (+ linestart (current-column)))
              (if (re-search-forward "[ \t]+" to t)
                  (ps-plot 'ps-basic-plot-whitespace from
			   (+ linestart (current-column)))))

             ((= match ?\014)
              (ps-plot 'ps-basic-plot-string from (- (point) 1) underline-p)
              (ps-top-of-page)))
            (setq from (point)))

        (ps-plot 'ps-basic-plot-string from to underline-p)
        (setq from to)))))

(defun ps-format-buffer ()
  (interactive)

  (setq ps-source-buffer (current-buffer))
  (setq ps-output-buffer (get-buffer-create "%PostScript%"))

  (save-excursion
    (set-buffer ps-output-buffer)
    (delete-region (point-max) (point-min)))

  (ps-begin-file)
  (ps-begin-page)
  (ps-init-page)

  (ps-plot-region (point-min) (point-max) 0)

  (ps-end-page)
  (ps-end-file)
  )

(defun ps-mapper (extent list)
  (nconc list (list (list (extent-start-position extent) 'push extent)
                    (list (extent-end-position extent) 'pull extent)))
  nil)

(defun ps-sorter (a b)
  (< (car a) (car b)))

(defun ps-extent-sorter (a b)
  (< (extent-priority a) (extent-priority b)))

(defun overlay-priority (p)
  (if (setq priority (overlay-get p 'priority)) priority 0))

(defun ps-overlay-sorter (a b)
  (> (overlay-priority a) (overlay-priority b)))

(defun ps-plot-with-face (from to face)

  (setq bold-p (memq face ps-bold-faces))
  (setq italic-p (memq face ps-italic-faces))
  (setq underline-p (memq face ps-underline-faces))

  (cond
   ((and bold-p italic-p)
    (ps-plot-region from to 3 underline-p))
   (italic-p
    (ps-plot-region from to 2 underline-p))
   (bold-p
    (ps-plot-region from to 1 underline-p))
   (t
    (ps-plot-region from to 0 underline-p))))


(defun ps-generate-postscript-with-faces (from to)

  (save-restriction
    (narrow-to-region from to)
    (setq face 'default)

    (cond ((string-match "Lucid" emacs-version)
	   ;; Build the list of extents...
	   (let ((a (cons 'dummy nil)))
	     (map-extents 'ps-mapper nil from to a)
	     (setq a (cdr a))
	     (setq a (sort a 'ps-sorter))
	   
	     (setq extent-list nil)
	   
	     ;; Loop through the extents...
	     (while a
	       (setq record (car a))
	     
	       (setq position (car record))
	       (setq record (cdr record))
	     
	       (setq type (car record))
	       (setq record (cdr record))
	     
	       (setq extent (car record))
	     
	       ;; Plot up to this record.
	       (ps-plot-with-face from position face)
	     
	       (cond
		((eq type 'push)
		 (setq extent-list (sort (cons extent extent-list)
					 'ps-extent-sorter)))
	      
		((eq type 'pull)
		 (setq extent-list (sort (delq extent extent-list)
					 'ps-extent-sorter))))
	     
	       (setq face
		     (if extent-list
			 (extent-face (car extent-list))
		       'default))
	     
	       (setq from position)
	       (setq a (cdr a)))))

	  ((string-match "^19" emacs-version)
	 
	   (while (< from to)
	   
	     (setq prop-position
		   (if (setq p (next-property-change from))
		       (if (> p to) to p)
		     to))

	     (setq over-position
		   (if (setq p (next-overlay-change from))
		       (if (> p to) to p)
		     to))

	     (setq position
		   (if (< prop-position over-position)
		       prop-position
		     over-position))

	     (setq face
		   (if (setq f (get-text-property from 'face)) f 'default))

	     (if (setq overlays (overlays-at from))
		 (progn
		   (setq overlays (sort overlays 'ps-overlay-sorter))
		   (while overlays
		     (if (setq face (overlay-get (car overlays) 'face))
			 (setq overlays nil)
		       (setq overlays (cdr overlays))))))
	   
	     ;; Plot up to this record.
	     (ps-plot-with-face from position face)
	   
	     (setq from position))))

    (ps-plot-with-face from to face)))

(defun ps-generate-postscript (from to)
  (ps-plot-region from to 0))

(defun ps-generate (buffer from to genfunc)

  (save-restriction
    (narrow-to-region from to)
    (if ps-razzle-dazzle
	(message "Formatting... %d%%" (setq ps-razchunk 0)))

    (set-buffer buffer)
    (setq ps-source-buffer buffer)
    (setq ps-output-buffer (get-buffer-create ps-spool-buffer-name))

    (unwind-protect
	(progn
	
	  (set-buffer ps-output-buffer)
	  (goto-char (point-min))
	  (if (looking-at (regexp-quote "%!PS-Adobe-1.0"))
	      (ps-set-font ps-current-font)
	    (ps-begin-file))
	  (ps-begin-page)
	  (ps-init-page)
	
	  (goto-char (point-max))
	  (if (and ps-spool-duplex
		   (re-search-backward "^%%Page")
		   (looking-at "^%%Page.*[24680]$"))
	      (ps-next-page))
	    
	  (set-buffer ps-source-buffer)
	  (funcall genfunc from to)

	  (ps-end-page)))

    (if ps-razzle-dazzle
	(message "Formatting... Done."))))

(defun ps-do-despool (filename)

  (if (or (not (boundp 'ps-output-buffer))
	  (not ps-output-buffer))
      (message "No spooled PostScript to print.")
    
    (ps-end-file)
  
    (if filename
	(save-excursion
	  (if ps-razzle-dazzle
	      (message "Saving..."))
	  
	  (set-buffer ps-output-buffer)
	  (setq filename (expand-file-name filename))
	  (write-region (point-min) (point-max) filename)
	
	  (if ps-razzle-dazzle
	      (message "Wrote %s" filename)))

      ;; Else, spool to the printer
      (if ps-razzle-dazzle
	  (message "Printing..."))
    
      (save-excursion
	(set-buffer ps-output-buffer)
	(apply 'call-process-region
	       (point-min) (point-max) ps-lpr-command nil 0 nil
	       ps-lpr-switches))

      (if ps-razzle-dazzle
	  (message "Printing... Done.")))

    (kill-buffer ps-output-buffer)))

(defun ps-testpattern ()
  (setq foo 1)
  (while (< foo 60)
    (insert "|" (make-string foo ?\ ) (format "%d\n" foo))
    (setq foo (+ 1 foo))))

(defun pts (stuff)
  (save-excursion
    (set-buffer "*scratch*")
    (goto-char (point-max))
    (insert "---------------------------------\n"
            (symbol-name stuff) ":\n"
            (prin1-to-string (symbol-value stuff))
            "\n")))

(provide 'ps-print)

;; ps-print.el ends here
