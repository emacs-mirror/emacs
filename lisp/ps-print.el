;;; ps-print.el --- Print text from the buffer as PostScript

;; Copyright (C) 1993, 94, 95, 96, 97, 98, 1999 Free Software Foundation, Inc.

;; Author:	Jim Thompson (was <thompson@wg2.waii.com>)
;; Author:	Jacques Duthen (was <duthen@cegelec-red.fr>)
;; Author:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Author:	Kenichi Handa <handa@etl.go.jp> (multi-byte characters)
;; Maintainer:	Kenichi Handa <handa@etl.go.jp> (multi-byte characters)
;; Maintainer:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Keywords:	wp, print, PostScript
;; Time-stamp:	<99/07/03 20:16:48 vinicius>
;; Version:	5.0

(defconst ps-print-version "5.0"
  "ps-print.el, v 5.0 <99/07/03 vinicius>

Vinicius's last change version -- this file may have been edited as part of
Emacs without changes to the version number.  When reporting bugs,
please also report the version of Emacs, if any, that ps-print was
distributed with.

Please send all bug fixes and enhancements to
	Vinicius Jose Latorre <vinicius@cpqd.com.br>.
")

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; About ps-print
;; --------------
;;
;; This package provides printing of Emacs buffers on PostScript
;; printers; the buffer's bold and italic text attributes are
;; preserved in the printer output.  ps-print is intended for use with
;; Emacs 19 or Lucid Emacs, together with a fontifying package such as
;; font-lock or hilit.
;;
;; ps-print uses the same face attributes defined through font-lock or hilit
;; to print a PostScript file, but some faces are better seeing on the screen
;; than on paper, specially when you have a black/white PostScript printer.
;;
;; ps-print allows a remap of face to another one that it is better to print,
;; for example, the face font-lock-comment-face (if you are using font-lock)
;; could have bold or italic attribute when printing, besides foreground color.
;; This remap improves printing look (see How Ps-Print Maps Faces).
;;
;;
;; Using ps-print
;; --------------
;;
;; ps-print provides eight commands for generating PostScript images
;; of Emacs buffers:
;;
;;        ps-print-buffer
;;        ps-print-buffer-with-faces
;;        ps-print-region
;;        ps-print-region-with-faces
;;        ps-spool-buffer
;;        ps-spool-buffer-with-faces
;;        ps-spool-region
;;        ps-spool-region-with-faces
;;
;; These commands all perform essentially the same function: they
;; generate PostScript images suitable for printing on a PostScript
;; printer or displaying with GhostScript.  These commands are
;; collectively referred to as "ps-print- commands".
;;
;; The word "print" or "spool" in the command name determines when the
;; PostScript image is sent to the printer:
;;
;;        print      - The PostScript image is immediately sent to the
;;                     printer;
;;
;;        spool      - The PostScript image is saved temporarily in an
;;                     Emacs buffer.  Many images may be spooled locally
;;                     before printing them.  To send the spooled images
;;                     to the printer, use the command `ps-despool'.
;;
;; The spooling mechanism was designed for printing lots of small
;; files (mail messages or netnews articles) to save paper that would
;; otherwise be wasted on banner pages, and to make it easier to find
;; your output at the printer (it's easier to pick up one 50-page
;; printout than to find 50 single-page printouts).
;;
;; ps-print has a hook in the `kill-emacs-hook' so that you won't
;; accidentally quit from Emacs while you have unprinted PostScript
;; waiting in the spool buffer.  If you do attempt to exit with
;; spooled PostScript, you'll be asked if you want to print it, and if
;; you decline, you'll be asked to confirm the exit; this is modeled
;; on the confirmation that Emacs uses for modified buffers.
;;
;; The word "buffer" or "region" in the command name determines how
;; much of the buffer is printed:
;;
;;        buffer     - Print the entire buffer.
;;
;;        region     - Print just the current region.
;;
;; The -with-faces suffix on the command name means that the command
;; will include font, color, and underline information in the
;; PostScript image, so the printed image can look as pretty as the
;; buffer.  The ps-print- commands without the -with-faces suffix
;; don't include font, color, or underline information; images printed
;; with these commands aren't as pretty, but are faster to generate.
;;
;; Two ps-print- command examples:
;;
;;        ps-print-buffer             - print the entire buffer,
;;                                      without font, color, or
;;                                      underline information, and
;;                                      send it immediately to the
;;                                      printer.
;;
;;        ps-spool-region-with-faces  - print just the current region;
;;                                      include font, color, and
;;                                      underline information, and
;;                                      spool the image in Emacs to
;;                                      send to the printer later.
;;
;;
;; Invoking Ps-Print
;; -----------------
;;
;; To print your buffer, type
;;
;;        M-x ps-print-buffer
;;
;; or substitute one of the other seven ps-print- commands.  The
;; command will generate the PostScript image and print or spool it as
;; specified.  By giving the command a prefix argument
;;
;;        C-u M-x ps-print-buffer
;;
;; it will save the PostScript image to a file instead of sending it
;; to the printer; you will be prompted for the name of the file to
;; save the image to.  The prefix argument is ignored by the commands
;; that spool their images, but you may save the spooled images to a
;; file by giving a prefix argument to `ps-despool':
;;
;;        C-u M-x ps-despool
;;
;; When invoked this way, `ps-despool' will prompt you for the name of
;; the file to save to.
;;
;; Any of the `ps-print-' commands can be bound to keys; I recommend
;; binding `ps-spool-buffer-with-faces', `ps-spool-region-with-faces',
;; and `ps-despool'.  Here are the bindings I use on my Sun 4 keyboard:
;;
;;   (global-set-key 'f22 'ps-spool-buffer-with-faces) ;f22 is prsc
;;   (global-set-key '(shift f22) 'ps-spool-region-with-faces)
;;   (global-set-key '(control f22) 'ps-despool)
;;
;;
;; The Printer Interface
;; ---------------------
;;
;; The variables `ps-lpr-command' and `ps-lpr-switches' determine what
;; command is used to send the PostScript images to the printer, and
;; what arguments to give the command.  These are analogous to
;; `lpr-command' and `lpr-switches'.
;;
;; Make sure that they contain appropriate values for your system;
;; see the usage notes below and the documentation of these variables.
;;
;; The variable `ps-printer-name' determines the name of a local printer for
;; printing PostScript files.
;;
;; NOTE: `ps-lpr-command' and `ps-lpr-switches' take their initial values
;;       from the variables `lpr-command' and `lpr-switches'.  If you have
;;       `lpr-command' set to invoke a pretty-printer such as `enscript',
;;       then ps-print won't work properly.  `ps-lpr-command' must name
;;       a program that does not format the files it prints.
;;       `ps-printer-name' takes its initial value from the variable
;;       `printer-name'.
;;
;; The variable `ps-print-region-function' specifies a function to print the
;; region on a PostScript printer.
;; See definition of `call-process-region' for calling conventions.  The fourth
;; and the sixth arguments are both nil.
;;
;;
;; The Page Layout
;; ---------------
;;
;; All dimensions are floats in PostScript points.
;; 1 inch  ==       2.54  cm    ==     72       points
;; 1 cm    ==  (/ 1 2.54) inch  ==  (/ 72 2.54) points
;;
;; The variable `ps-paper-type' determines the size of paper ps-print
;; formats for; it should contain one of the symbols:
;; `a4' `a3' `letter' `legal' `letter-small' `tabloid'
;; `ledger' `statement' `executive' `a4small' `b4' `b5'
;;
;; The variable `ps-landscape-mode' determines the orientation
;; of the printing on the page:
;; nil means `portrait' mode, non-nil means `landscape' mode.
;; There is no oblique mode yet, though this is easy to do in ps.
;;
;; In landscape mode, the text is NOT scaled: you may print 70 lines
;; in portrait mode and only 50 lignes in landscape mode.
;; The margins represent margins in the printed paper:
;; the top margin is the margin between the top of the page
;; and the printed header, whatever the orientation is.
;;
;; The variable `ps-number-of-columns' determines the number of columns
;; both in landscape and portrait mode.
;; You can use:
;; - (the standard) one column portrait mode
;; - (my favorite) two columns landscape mode (which spares trees)
;; but also
;; - one column landscape mode for files with very long lines.
;; - multi-column portrait or landscape mode
;;
;;
;; Horizontal layout
;; -----------------
;;
;; The horizontal layout is determined by the variables
;; `ps-left-margin' `ps-inter-column' `ps-right-margin'
;; as follows:
;;
;;  ------------------------------------------
;;  |    |      |    |      |    |      |    |
;;  | lm | text | ic | text | ic | text | rm |
;;  |    |      |    |      |    |      |    |
;;  ------------------------------------------
;;
;; If `ps-number-of-columns' is 1, `ps-inter-column' is not relevant.
;; Usually, lm = rm > 0 and ic = lm
;; If (ic < 0), the text of adjacent columns can overlap.
;;
;;
;; Vertical layout
;; ---------------
;;
;; The vertical layout is determined by the variables
;; `ps-bottom-margin' `ps-top-margin' `ps-header-offset'
;; as follows:
;;
;; |--------|        |--------|
;; | tm     |        | tm     |
;; |--------|        |--------|
;; | header |        |        |
;; |--------|        |        |
;; | ho     |        |        |
;; |--------|   or   | text   |
;; |        |        |        |
;; | text   |        |        |
;; |        |        |        |
;; |--------|        |--------|
;; | bm     |        | bm     |
;; |--------|        |--------|
;;
;; If `ps-print-header' is nil, `ps-header-offset' is not relevant.
;; The margins represent margins in the printed paper:
;; the top margin is the margin between the top of the page
;; and the printed header, whatever the orientation is.
;;
;;
;; Headers
;; -------
;;
;; ps-print can print headers at the top of each column or at the top
;; of each page; the default headers contain the following four items:
;; on the left, the name of the buffer and, if the buffer is visiting
;; a file, the file's directory; on the right, the page number and
;; date of printing.  The default headers look something like this:
;;
;;     ps-print.el                                         1/21
;;     /home/jct/emacs-lisp/ps/new                     94/12/31
;;
;; When printing on duplex printers, left and right are reversed so
;; that the page numbers are toward the outside (cf. `ps-spool-duplex').
;;
;; Headers are configurable:
;; To turn them off completely, set `ps-print-header' to nil.
;; To turn off the header's gaudy framing box,
;; set `ps-print-header-frame' to nil.
;;
;; To print only one header at the top of each page,
;; set `ps-print-only-one-header' to t.
;;
;; The font family and size of text in the header are determined
;; by the variables `ps-header-font-family', `ps-header-font-size' and
;; `ps-header-title-font-size' (see below).
;;
;; The variable `ps-header-line-pad' determines the portion of a header
;; title line height to insert between the header frame and the text
;; it contains, both in the vertical and horizontal directions:
;; .5 means half a line.

;; Page numbers are printed in `n/m' format, indicating page n of m pages;
;; to omit the total page count and just print the page number,
;; set `ps-show-n-of-n' to nil.
;;
;; The amount of information in the header can be changed by changing
;; the number of lines.  To show less, set `ps-header-lines' to 1, and
;; the header will show only the buffer name and page number.  To show
;; more, set `ps-header-lines' to 3, and the header will show the time of
;; printing below the date.
;;
;; To change the content of the headers, change the variables
;; `ps-left-header' and `ps-right-header'.
;; These variables are lists, specifying top-to-bottom the text
;; to display on the left or right side of the header.
;; Each element of the list should be a string or a symbol.
;; Strings are inserted directly into the PostScript arrays,
;; and should contain the PostScript string delimiters '(' and ')'.
;;
;; Symbols in the header format lists can either represent functions
;; or variables.  Functions are called, and should return a string to
;; show in the header.  Variables should contain strings to display in
;; the header.  In either case, function or variable, the PostScript
;; string delimiters are added by ps-print, and should not be part of
;; the returned value.
;;
;; Here's an example: say we want the left header to display the text
;;
;;     Moe
;;     Larry
;;     Curly
;;
;; where we have a function to return "Moe"
;;
;;     (defun moe-func ()
;;       "Moe")
;;
;; a variable specifying "Larry"
;;
;;     (setq larry-var "Larry")
;;
;; and a literal for "Curly".  Here's how `ps-left-header' should be
;; set:
;;
;;     (setq ps-left-header (list 'moe-func 'larry-var "(Curly)"))
;;
;; Note that Curly has the PostScript string delimiters inside his
;; quotes -- those aren't misplaced lisp delimiters!
;;
;; Without them, PostScript would attempt to call the undefined
;; function Curly, which would result in a PostScript error.
;;
;; Since most printers don't report PostScript errors except by
;; aborting the print job, this kind of error can be hard to track down.
;;
;; Consider yourself warned!
;;
;;
;; PostScript Prologue Header
;; --------------------------
;;
;; It is possible to add PostScript prologue header comments besides that
;; ps-print generates by setting the variable `ps-print-prologue-header'.
;;
;; `ps-print-prologue-header' may be a string or a symbol function which returns
;; a string.  Note that this string is inserted on PostScript prologue header
;; section which is used to define some document characteristic through
;; PostScript special comments, like "%%Requirements: jog\n".
;;
;; By default `ps-print-prologue-header' is nil.
;;
;; ps-print always inserts the %%Requirements: comment, so if you need to insert
;; more requirements put them first in `ps-print-prologue-header' using the
;; "%%+" comment.  For example, if you need to set numcopies to 3 and jog on
;; requirements and set %%LanguageLevel: to 2, do:
;;
;; (setq ps-print-prologue-header
;;       "%%+ numcopies(3) jog\n%%LanguageLevel: 2\n")
;;
;; The duplex requirement is inserted by ps-print (see section Duplex Printers).
;;
;; Do not forget to terminate the string with "\n".
;;
;; For more information about PostScript document comments, see:
;;    PostScript Language Reference Manual (2nd edition)
;;    Adobe Systems Incorporated
;;    Appendix G: Document Structuring Conventions -- Version 3.0
;;
;;
;; Duplex Printers
;; ---------------
;;
;; If you have a duplex-capable printer (one that prints both sides of the
;; paper), set `ps-spool-duplex' to t.
;; ps-print will insert blank pages to make sure each buffer starts on the
;; correct side of the paper.
;;
;; The variable `ps-spool-config' specifies who is the responsable for setting
;; duplex and page size switches.  Valid values are:
;;
;; lpr-switches    duplex and page size are configured by `ps-lpr-switches'.
;;                 Don't forget to set `ps-lpr-switches' to select duplex
;;                 printing for your printer.
;;
;; setpagedevice   duplex and page size are configured by ps-print using the
;;                 setpagedevice PostScript operator.
;;
;; nil             duplex and page size are configured by ps-print *not* using
;;                 the setpagedevice PostScript operator.
;;
;; Any other value is treated as nil.
;;
;; The default value is `lpr-switches'.
;;
;; WARNING: The setpagedevice PostScript operator affects ghostview utility when
;;          viewing file generated using landscape.  Also on some printers,
;;          setpagedevice affects zebra stripes; on other printers,
;;          setpagedevice affects the left margin.
;;          Besides all that, if your printer does not have the paper size
;;          specified by setpagedevice, your printing will be aborted.
;;          So, if you need to use setpagedevice, set `ps-spool-config' to
;;          `setpagedevice', generate a test file and send it to your printer;
;;          if the printed file isn't ok, set `ps-spool-config' to nil.
;;
;; The variable `ps-spool-tumble' specifies how the page images on opposite
;; sides of a sheet are oriented with respect to each other.  If
;; `ps-spool-tumble' is nil, produces output suitable for binding on the left or
;; right.  If `ps-spool-tumble' is non-nil, produces output suitable for binding
;; at the top or bottom.  It has effect only when `ps-spool-duplex' is non-nil.
;; The default value is nil.
;;
;; Some printer system prints a header page and forces the first page be printed
;; on header page back, when using duplex.  If your printer system has this
;; behavior, set variable `ps-banner-page-when-duplexing' to t.
;;
;; When `ps-banner-page-when-duplexing' is non-nil means the very first page is
;; skipped.  It's like the very first character of buffer (or region) is ^L
;; (\014).
;;
;; The default for `ps-banner-page-when-duplexing' is nil (*don't* skip the very
;; first page).
;;
;;
;; N-up Printing
;; -------------
;;
;; The variable `ps-n-up-printing' specifies the number of pages per sheet of
;; paper.  The value specified must be between 1 and 100.  The default is 1.
;;
;; NOTE: some PostScript printer may crash printing if `ps-n-up-printing' is set
;;       to a high value (for example, 23).  If this happens, set a lower value.
;;
;; The variable `ps-n-up-margin' specifies the margin in points between the
;; sheet border and the n-up printing.  The default is 1 cm (or 0.3937 inches,
;; or 28.35 points).
;;
;; If variable `ps-n-up-border-p' is non-nil a border is drawn around each page.
;; The default is t.
;;
;; The variable `ps-n-up-filling' specifies how page matrix is filled on each
;; sheet of paper.  Following are the valid values for `ps-n-up-filling' with a
;; filling example using a 3x4 page matrix:
;;
;;  left-top   1  2  3  4         left-bottom    9  10 11 12
;;             5  6  7  8                        5  6  7  8
;;             9  10 11 12                       1  2  3  4
;;
;;  right-top  4  3  2  1         right-bottom   12 11 10 9
;;             8  7  6  5                        8  7  6  5
;;             12 11 10 9                        4  3  2  1
;;
;;  top-left   1  4  7  10        bottom-left    3  6  9  12
;;             2  5  8  11                       2  5  8  11
;;             3  6  9  12                       1  4  7  10
;;
;;  top-right  10 7  4  1         bottom-right   12 9  6  3
;;             11 8  5  2                        11 8  5  2
;;             12 9  6  3                        10 7  4  1
;;
;; Any other value is treated as left-top.
;;
;; The default value is left-top.
;;
;;
;; Control And 8-bit Characters
;; ----------------------------
;;
;; The variable `ps-print-control-characters' specifies whether you want to see
;; a printable form for control and 8-bit characters, that is, instead of
;; sending, for example, a ^D (\004) to printer, it is sent the string "^D".
;;
;; Valid values for `ps-print-control-characters' are:
;;
;;  8-bit           This is the value to use when you want an ASCII encoding of
;;                  any control or non-ASCII character. Control characters are
;;                  encoded as "^D", and non-ASCII characters have an
;;                  octal encoding.
;;
;;  control-8-bit   This is the value to use when you want an ASCII encoding of
;;                  any control character, whether it is 7 or 8-bit.
;;                  European 8-bits accented characters are printed according
;;                  the current font.
;;
;;  control         Only ASCII control characters have an ASCII encoding.
;;                  European 8-bits accented characters are printed according
;;                  the current font.
;;
;;  nil             No ASCII encoding. Any character is printed according the
;;                  current font.
;;
;; Any other value is treated as nil.
;;
;; The default is `control-8-bit'.
;;
;; Characters TAB, NEWLINE and FORMFEED are always treated by ps-print engine.
;;
;;
;; Printing Multi-byte Buffer
;; --------------------------
;;
;; See ps-mule.el for documentation.
;;
;; See ps-print-def.el for definition.
;;
;;
;; Line Number
;; -----------
;;
;; The variable `ps-line-number' specifies whether to number each line;
;; non-nil means do so.  The default is nil (don't number each line).
;;
;;
;; Zebra Stripes
;; -------------
;;
;; Zebra stripes are a kind of background that appear "underneath" the text
;; and can make the text easier to read.  They look like this:
;;
;; XXXXXXXXXXXXXXXXXXXXXXXX
;; XXXXXXXXXXXXXXXXXXXXXXXX
;; XXXXXXXXXXXXXXXXXXXXXXXX
;;
;;
;;
;; XXXXXXXXXXXXXXXXXXXXXXXX
;; XXXXXXXXXXXXXXXXXXXXXXXX
;; XXXXXXXXXXXXXXXXXXXXXXXX
;;
;; The blocks of X's represent rectangles filled with a light gray color.
;; Each rectangle extends all the way across the page.
;;
;; The height, in lines, of each rectangle is controlled by
;; the variable `ps-zebra-stripe-height', which is 3 by default.
;; The distance between stripes equals the height of a stripe.
;;
;; The variable `ps-zebra-stripes' controls whether to print zebra stripes.
;; Non-nil means yes, nil means no.  The default is nil.
;;
;; The variable `ps-zebra-gray' controls the zebra stripes gray scale.
;; It should be a float number between 0.0 (black color) and 1.0 (white color).
;; The default is 0.95.
;;
;; See also section How Ps-Print Has A Text And/Or Image On Background.
;;
;;
;; Hooks
;; -----
;;
;; ps-print has the following hook variables:
;;
;; `ps-print-hook'
;;    It is evaluated once before any printing process.  This is the right
;;    place to initialize ps-print global data.
;;    For an example, see section Adding a New Font Family.
;;
;; `ps-print-begin-sheet-hook'
;;    It is evaluated on each beginning of sheet of paper.
;;    If `ps-n-up-printing' is equal to 1, `ps-print-begin-page-hook' is never
;;    evaluated.
;;
;; `ps-print-begin-page-hook'
;;    It is evaluated on each beginning of page, except in the beginning
;;    of page that `ps-print-begin-sheet-hook' is evaluated.
;;
;; `ps-print-begin-column-hook'
;;    It is evaluated on each beginning of column, except in the beginning
;;    of column that `ps-print-begin-page-hook' is evaluated or that
;;    `ps-print-begin-sheet-hook' is evaluated.
;;
;;
;; Font Managing
;; -------------
;;
;; ps-print now knows rather precisely some fonts: the variable
;; `ps-font-info-database' contains information for a list of font families
;; (currently mainly `Courier' `Helvetica' `Times' `Palatino' `Helvetica-Narrow'
;; `NewCenturySchlbk').  Each font family contains the font names for standard,
;; bold, italic and bold-italic characters, a reference size (usually 10) and
;; the corresponding line height, width of a space and average character width.
;;
;; The variable `ps-font-family' determines which font family is to be used for
;; ordinary text.  If its value does not correspond to a known font family, an
;; error message is printed into the `*Messages*' buffer, which lists the
;; currently available font families.
;;
;; The variable `ps-font-size' determines the size (in points) of the font for
;; ordinary text, when generating PostScript.  Its value is a float or a cons of
;; floats which has the following form:
;;
;;    (LANDSCAPE-SIZE . PORTRAIT-SIZE)
;;
;; Similarly, the variable `ps-header-font-family' determines which font family
;; is to be used for text in the header.
;;
;; The variable `ps-header-font-size' determines the font size, in points, for
;; text in the header (similar to `ps-font-size').
;;
;; The variable `ps-header-title-font-size' determines the font size, in points,
;; for the top line of text in the header (similar to `ps-font-size').
;;
;;
;; Adding a New Font Family
;; ------------------------
;;
;; To use a new font family, you MUST first teach ps-print
;; this font, i.e., add its information to `ps-font-info-database',
;; otherwise ps-print cannot correctly place line and page breaks.
;;
;; For example, assuming `Helvetica' is unknown,
;; you first need to do the following ONLY ONCE:
;;
;; - create a new buffer
;; - generate the PostScript image to a file (C-u M-x ps-print-buffer)
;; - open this file and find the line:
;;	`% 3 cm 20 cm moveto  10 /Courier ReportFontInfo  showpage'
;; - delete the leading `%' (which is the PostScript comment character)
;; - replace in this line `Courier' by the new font (say `Helvetica')
;;   to get the line:
;;	`3 cm 20 cm moveto  10 /Helvetica ReportFontInfo  showpage'
;; - send this file to the printer (or to ghostscript).
;;   You should read the following on the output page:
;;
;;     For Helvetica 10 point, the line height is 11.56, the space width is 2.78
;;     and a crude estimate of average character width is 5.09243
;;
;; - Add these values to the `ps-font-info-database':
;;   (setq ps-font-info-database
;;	   (append
;;	    '((Helvetica			; the family key
;;	       (fonts (normal      . "Helvetica")
;;		      (bold        . "Helvetica-Bold")
;;		      (italic      . "Helvetica-Oblique")
;;		      (bold-italic . "Helvetica-BoldOblique"))
;;	       (size . 10.0)
;;	       (line-height . 11.56)
;;	       (space-width . 2.78)
;;	       (avg-char-width . 5.09243)))
;;	    ps-font-info-database))
;; - Now you can use this font family with any size:
;;	(setq ps-font-family 'Helvetica)
;; - if you want to use this family in another emacs session, you must
;;   put into your `~/.emacs':
;;	(require 'ps-print)
;;	(setq ps-font-info-database (append ...)))
;;   if you don't want to load ps-print, you have to copy the whole value:
;;	(setq ps-font-info-database '(<your stuff> <the standard stuff>))
;;   or, use `ps-print-hook' (see section Hooks):
;;	(add-hook 'ps-print-hook
;;		  '(lambda ()
;;		     (or (assq 'Helvetica ps-font-info-database)
;;			 (setq ps-font-info-database (append ...)))))
;;
;; You can create new `mixed' font families like:
;;      (my-mixed-family
;;       (fonts (normal               . "Courier-Bold")
;;              (bold                 . "Helvetica")
;;              (italic               . "Zapf-Chancery-MediumItalic")
;;              (bold-italic          . "NewCenturySchlbk-BoldItalic")
;;              (w3-table-hack-x-face . "LineDrawNormal"))
;;       (size . 10.0)
;;       (line-height . 10.55)
;;       (space-width . 6.0)
;;       (avg-char-width . 6.0))
;;
;; Now you can use your new font family with any size:
;;	(setq ps-font-family 'my-mixed-family)
;;
;; Note that on above example the `w3-table-hack-x-face' entry refers to
;; a face symbol, so when printing this face it'll be used the font
;; `LineDrawNormal'.  If the face  `w3-table-hack-x-face'  is remapped to
;; use bold and/or italic attribute, the corresponding entry (bold, italic
;; or bold-italic) will be used instead of `w3-table-hack-x-face' entry.
;;
;; Note also that the font family entry order is irrelevant, so the above
;; example could also be written:
;;      (my-mixed-family
;;       (size . 10.0)
;;       (fonts (w3-table-hack-x-face . "LineDrawNormal")
;;              (bold                 . "Helvetica")
;;              (bold-italic          . "NewCenturySchlbk-BoldItalic")
;;              (italic               . "Zapf-Chancery-MediumItalic")
;;              (normal               . "Courier-Bold"))
;;       (avg-char-width . 6.0)
;;       (space-width . 6.0)
;;       (line-height . 10.55))
;;
;; Despite the note above, it is recommended that some convention about
;; entry order be used.
;;
;; You can get information on all the fonts resident in YOUR printer
;; by uncommenting the line:
;;	% 3 cm 20 cm moveto  ReportAllFontInfo           showpage
;;
;; The PostScript file should be sent to YOUR PostScript printer.
;; If you send it to ghostscript or to another PostScript printer,
;; you may get slightly different results.
;; Anyway, as ghostscript fonts are autoload, you won't get
;; much font info.
;;
;;
;; How Ps-Print Deals With Faces
;; -----------------------------
;;
;; The ps-print-*-with-faces commands attempt to determine which faces
;; should be printed in bold or italic, but their guesses aren't
;; always right.  For example, you might want to map colors into faces
;; so that blue faces print in bold, and red faces in italic.
;;
;; It is possible to force ps-print to consider specific faces bold,
;; italic or underline, no matter what font they are displayed in, by setting
;; the variables `ps-bold-faces', `ps-italic-faces' and `ps-underlined-faces'.
;; These variables contain lists of faces that ps-print should consider bold,
;; italic or underline; to set them, put code like the following into your
;; .emacs file:
;;
;;      (setq ps-bold-faces '(my-blue-face))
;;      (setq ps-italic-faces '(my-red-face))
;;      (setq ps-underlined-faces '(my-green-face))
;;
;; Faces like bold-italic that are both bold and italic should go in
;; *both* lists.
;;
;; ps-print keeps internal lists of which fonts are bold and which are
;; italic; these lists are built the first time you invoke ps-print.
;; For the sake of efficiency, the lists are built only once; the same
;; lists are referred in later invocations of ps-print.
;;
;; Because these lists are built only once, it's possible for them to
;; get out of sync, if a face changes, or if new faces are added.  To
;; get the lists back in sync, you can set the variable
;; `ps-build-face-reference' to t, and the lists will be rebuilt the
;; next time ps-print is invoked.  If you need that the lists always be
;; rebuilt when ps-print is invoked, set the variable
;; `ps-always-build-face-reference' to t.
;;
;;
;; How Ps-Print Deals With Color
;; -----------------------------
;;
;; ps-print detects faces with foreground and background colors
;; defined and embeds color information in the PostScript image.
;; The default foreground and background colors are defined by the
;; variables `ps-default-fg' and `ps-default-bg'.
;; On black-and-white printers, colors are displayed in grayscale.
;; To turn off color output, set `ps-print-color-p' to nil.
;;
;;
;; How Ps-Print Maps Faces
;; -----------------------
;;
;; As ps-print uses PostScript to print buffers, it is possible to have
;; other attributes associated with faces. So the new attributes used
;; by ps-print are:
;;
;;   strikeout - like underline, but the line is in middle of text.
;;   overline  - like underline, but the line is over the text.
;;   shadow    - text will have a shadow.
;;   box       - text will be surrounded by a box.
;;   outline   - print characters as hollow outlines.
;;
;; See the documentation for `ps-extend-face'.
;;
;; Let's, for example, remap `font-lock-keyword-face' to another foreground
;; color and bold attribute:
;;
;;    (ps-extend-face '(font-lock-keyword-face "RoyalBlue" nil bold) 'MERGE)
;;
;; If you want to use a new face, define it first with `defface',
;; and then call `ps-extend-face' to specify how to print it.
;;
;;
;; How Ps-Print Has A Text And/Or Image On Background
;; --------------------------------------------------
;;
;; ps-print can print texts and/or EPS PostScript images on background; it is
;; possible to define the following text attributes: font name, font size,
;; initial position, angle, gray scale and pages to print.
;;
;; It has the following EPS PostScript images attributes: file name containing
;; the image, initial position, X and Y scales, angle and pages to print.
;;
;; See documentation for `ps-print-background-text' and
;; `ps-print-background-image'.
;;
;; For example, if we wish to print text "preliminary" on all pages and text
;; "special" on page 5 and from page 11 to page 17, we could specify:
;;
;; (setq ps-print-background-text
;;       '(("preliminary")
;;         ("special"
;;          "LeftMargin" "BottomMargin PrintHeight add" ; X and Y position
;;                                      ; (upper left corner)
;;          nil nil nil
;;          "PrintHeight neg PrintPageWidth atan" ; angle
;;          5 (11 . 17))                ; page list
;;         ))
;;
;; Similarly, we could print image "~/images/EPS-image1.ps" on all pages and
;; image "~/images/EPS-image2.ps" on page 5 and from page 11 to page 17, we
;; specify:
;;
;; (setq ps-print-background-image
;;       '(("~/images/EPS-image1.ps"
;;          "LeftMargin" "BottomMargin") ; X and Y position (lower left corner)
;;         ("~/images/EPS-image2.ps"
;;          "LeftMargin" "BottomMargin PrintHeight 2 div add" ; X and Y position
;;                                      ; (upper left corner)
;;          nil nil nil
;;          5 (11 . 17))                ; page list
;;         ))
;;
;; If it is not possible to read (or does not exist) an image file, that file
;; is ignored.
;;
;; The printing order is:
;;
;;    1. Print zebra stripes
;;    2. Print background texts that it should be on all pages
;;    3. Print background images that it should be on all pages
;;    4. Print background texts only for current page (if any)
;;    5. Print background images only for current page (if any)
;;    6. Print header
;;    7. Print buffer text (with faces, if specified) and line number
;;
;;
;; Utilities
;; ---------
;;
;; Some tools are provided to help you customize your font setup.
;;
;; `ps-setup' returns (some part of) the current setup.
;;
;; To avoid wrapping too many lines, you may want to adjust the
;; left and right margins and the font size.  On UN*X systems, do:
;; pr -t file | awk '{printf "%3d %s\n", length($0), $0}' | sort -r | head
;; to determine the longest lines of your file.
;; Then, the command `ps-line-lengths' will give you the correspondence
;; between a line length (number of characters) and the maximum font
;; size which doesn't wrap such a line with the current ps-print setup.
;;
;; The commands `ps-nb-pages-buffer' and `ps-nb-pages-region' display
;; the correspondence between a number of pages and the maximum font
;; size which allow the number of lines of the current buffer or of
;; its current region to fit in this number of pages.
;;
;; NOTE: line folding is not taken into account in this process and could
;;       change the results.
;;
;;
;; New since version 1.5
;; ---------------------
;;
;; Color output capability.
;; Automatic detection of font attributes (bold, italic).
;; Configurable headers with page numbers.
;; Slightly faster.
;; Support for different paper sizes.
;; Better conformance to PostScript Document Structure Conventions.
;;
;;
;; New since version 2.8
;; ---------------------
;;
;; [vinicius] 990703 Vinicius Jose Latorre <vinicius@cpqd.com.br>
;;
;; Better customization.
;; `ps-banner-page-when-duplexing' and `ps-zebra-gray'.
;;
;; [vinicius] 990513 Vinicius Jose Latorre <vinicius@cpqd.com.br>
;;
;; N-up printing.
;; Hook: `ps-print-begin-sheet-hook'.
;;
;; [keinichi] 990509 Kein'ichi Handa <handa@etl.go.jp>
;;
;; `ps-print-region-function'
;;
;; [vinicius] 990301 Vinicius Jose Latorre <vinicius@cpqd.com.br>
;;
;; PostScript tumble and setpagedevice.
;;
;; [vinicius] 980922 Vinicius Jose Latorre <vinicius@cpqd.com.br>
;;
;; PostScript prologue header comment insertion.
;; Skip invisible text better.
;;
;; [keinichi] 980819 Kein'ichi Handa <handa@etl.go.jp>
;;
;; Multi-byte buffer handling.
;;
;; [vinicius] 980306 Vinicius Jose Latorre <vinicius@cpqd.com.br>
;;
;; Skip invisible text.
;;
;; [vinicius] 971130 Vinicius Jose Latorre <vinicius@cpqd.com.br>
;;
;; Hooks: `ps-print-hook', `ps-print-begin-page-hook' and
;; `ps-print-begin-column-hook'.
;; Put one header per page over the columns.
;; Better database font management.
;; Better control characters handling.
;;
;; [vinicius] 971121 Vinicius Jose Latorre <vinicius@cpqd.com.br>
;;
;; Dynamic evaluation at print time of `ps-lpr-switches'.
;; Handle control characters.
;; Face remapping.
;; New face attributes.
;; Line number.
;; Zebra stripes.
;; Text and/or image on background.
;;
;; [jack] 960517 Jacques Duthen <duthen@cegelec-red.fr>
;;
;; Font family and float size for text and header.
;; Landscape mode.
;; Multiple columns.
;; Tools for page setup.
;;
;;
;; Known bugs and limitations of ps-print
;; --------------------------------------
;;
;; Although color printing will work in XEmacs 19.12, it doesn't work
;; well; in particular, bold or italic fonts don't print in the right
;; background color.
;;
;; Invisible properties aren't correctly ignored in XEmacs 19.12.
;;
;; Automatic font-attribute detection doesn't work well, especially
;; with hilit19 and older versions of get-create-face.  Users having
;; problems with auto-font detection should use the lists
;; `ps-italic-faces', `ps-bold-faces' and `ps-underlined-faces' and/or
;; turn off automatic detection by setting `ps-auto-font-detect' to nil.
;;
;; Automatic font-attribute detection doesn't work with XEmacs 19.12
;; in tty mode; use the lists `ps-italic-faces', `ps-bold-faces' and
;; `ps-underlined-faces' instead.
;;
;; Still too slow; could use some hand-optimization.
;;
;; Default background color isn't working.
;;
;; Faces are always treated as opaque.
;;
;; Epoch and Emacs 18 not supported.  At all.
;;
;; Fixed-pitch fonts work better for line folding, but are not required.
;;
;; `ps-nb-pages-buffer' and `ps-nb-pages-region' don't take care
;; of folding lines.
;;
;;
;; Things to change
;; ----------------
;;
;; Avoid page break inside a paragraph.
;; Add `ps-non-bold-faces' and `ps-non-italic-faces' (should be easy).
;; Improve the memory management for big files (hard?).
;; `ps-nb-pages-buffer' and `ps-nb-pages-region' should take care
;; of folding lines.
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Kein'ichi Handa <handa@etl.go.jp> for multi-byte buffer handling.
;;
;; Thanks to Matthew O Persico <Matthew.Persico@lazard.com> for line number on
;; empty columns.
;;
;; Thanks to Theodore Jump <tjump@cais.com> for adjust PostScript code order on
;; last page.
;;
;; Thanks to Roland Ducournau <ducour@lirmm.fr> for
;; `ps-print-control-characters' variable documentation.
;;
;; Thanks to Marcus G Daniels <marcus@cathcart.sysc.pdx.edu> for a better
;; database font management.
;;
;; Thanks to Martin Boyer <gamin@videotron.ca> for some ideas on putting one
;; header per page over the columns and correct line numbers when printing a
;; region.
;;
;; Thanks to Steven L Baur <steve@miranova.com> for dynamic evaluation at
;; print time of `ps-lpr-switches'.
;;
;; Thanks to Kevin Rodgers <kevinr@ihs.com> for handling control characters
;; (his code was severely modified, but the main idea was kept).
;;
;; Thanks to some suggestions on:
;;  * Face color map: Marco Melgazzi <marco@techie.com>
;;  * XEmacs compatibility: William J. Henney <will@astrosmo.unam.mx>
;;  * Check `ps-paper-type': Sudhakar Frederick <sfrederi@asc.corp.mot.com>
;;
;; Thanks to Jacques Duthen <duthen@cegelec-red.fr> (Jack) for the 3.4 version
;; I started from. [vinicius]
;;
;; Thanks to Jim Thompson <?@?> for the 2.8 version I started from.
;; [jack]
;;
;; Thanks to Kevin Rodgers <kevinr@ihs.com> for adding support for
;; color and the invisible property.
;;
;; Thanks to Avishai Yacobi, avishaiy@mcil.comm.mot.com, for writing
;; the initial port to Emacs 19.  His code is no longer part of
;; ps-print, but his work is still appreciated.
;;
;; Thanks to Remi Houdaille and Michel Train, michel@metasoft.fdn.org,
;; for adding underline support.  Their code also is no longer part of
;; ps-print, but their efforts are not forgotten.
;;
;; Thanks also to all of you who mailed code to add features to
;; ps-print; although I didn't use your code, I still appreciate your
;; sharing it with me.
;;
;; Thanks to all who mailed comments, encouragement, and criticism.
;; Thanks also to all who responded to my survey; I had too many
;; responses to reply to them all, but I greatly appreciate your
;; interest.
;;
;; Jim
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(unless (featurep 'lisp-float-type)
  (error "`ps-print' requires floating point support"))

;; For Emacs 20.2 and the earlier version.
(eval-and-compile
  (and (boundp 'mule-version)		; only if mule package is loaded
       (string< mule-version "4.0")
       (progn
	 (defun set-buffer-multibyte (arg)
	   (setq enable-multibyte-characters arg))
	 (defun string-as-unibyte (arg) arg)
	 (defun string-as-multibyte (arg) arg)
	 (defun charset-after (&optional arg)
	   (char-charset (char-after arg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Variables:

;;; Interface to the command system

(defgroup postscript nil
  "PostScript Group"
  :tag "PostScript"
  :group 'emacs)

(defgroup ps-print nil
  "PostScript generator for Emacs 19"
  :prefix "ps-"
  :group 'wp
  :group 'postscript)

(defgroup ps-print-horizontal nil
  "Horizontal page layout"
  :prefix "ps-"
  :tag "Horizontal"
  :group 'ps-print)

(defgroup ps-print-vertical nil
  "Vertical page layout"
  :prefix "ps-"
  :tag "Vertical"
  :group 'ps-print)

(defgroup ps-print-header nil
  "Headers layout"
  :prefix "ps-"
  :tag "Header"
  :group 'ps-print)

(defgroup ps-print-font nil
  "Fonts customization"
  :prefix "ps-"
  :tag "Font"
  :group 'ps-print)

(defgroup ps-print-color nil
  "Color customization"
  :prefix "ps-"
  :tag "Color"
  :group 'ps-print)

(defgroup ps-print-face nil
  "Faces customization"
  :prefix "ps-"
  :tag "PS Faces"
  :group 'ps-print
  :group 'faces)

(defgroup ps-print-n-up nil
  "N-up customization"
  :prefix "ps-"
  :tag "N-Up"
  :group 'ps-print)

(defgroup ps-print-zebra nil
  "Zebra customization"
  :prefix "ps-"
  :tag "Zebra"
  :group 'ps-print)

(defgroup ps-print-background nil
  "Background customization"
  :prefix "ps-"
  :tag "Background"
  :group 'ps-print)

(defgroup ps-print-printer nil
  "Printer customization"
  :prefix "ps-"
  :tag "Printer"
  :group 'ps-print)

(defgroup ps-print-page nil
  "Page customization"
  :prefix "ps-"
  :tag "Page"
  :group 'ps-print)


(require 'ps-print-def)			; Common definitions


(defcustom ps-print-prologue-header nil
  "*PostScript prologue header comments besides that ps-print generates.

`ps-print-prologue-header' may be a string or a symbol function which
returns a string.  Note that this string is inserted on PostScript prologue
header section which is used to define some document characteristic through
PostScript special comments, like \"%%Requirements: jog\\n\".

ps-print always inserts the %%Requirements: comment, so if you need to insert
more requirements put them first in `ps-print-prologue-header' using the
\"%%+\" comment.  For example, if you need to set numcopies to 3 and jog on
requirements and set %%LanguageLevel: to 2, do:

(setq ps-print-prologue-header
      \"%%+ numcopies(3) jog\\n%%LanguageLevel: 2\\n\")

The duplex requirement is inserted by ps-print (see `ps-spool-duplex').

Do not forget to terminate the string with \"\\n\".

For more information about PostScript document comments, see:
   PostScript Language Reference Manual (2nd edition)
   Adobe Systems Incorporated
   Appendix G: Document Structuring Conventions -- Version 3.0"
  :type '(choice :tag "Prologue Header"
		 string symbol (other :tag "nil" nil))
  :group 'ps-print)

(defcustom ps-printer-name (and (boundp 'printer-name)
				printer-name)
  "*The name of a local printer for printing PostScript files.

On Unix-like systems, a string value should be a name understood by
lpr's -P option; a value of nil means use the value of `printer-name'
instead.  Any other value will be ignored.

On MS-DOS and MS-Windows systems, a string value is taken as the name of
the printer device or port to which PostScript files are written,
provided `ps-lpr-command' is \"\".  By default it is the same as
`printer-name'; typical non-default settings would be \"LPT1\" to
\"LPT3\" for parallel printers, or \"COM1\" to \"COM4\" or \"AUX\" for
serial printers, or \"//hostname/printer\" for a shared network printer.
You can also set it to a name of a file, in which case the output gets
appended to that file.  \(Note that `ps-print' package already has
facilities for printing to a file, so you might as well use them instead
of changing the setting of this variable.\)  If you want to silently
discard the printed output, set this to \"NUL\"."
  :type '(choice :tag "Printer Name"
		 file (other :tag "Pipe to ps-lpr-command" pipe))
  :group 'ps-print-printer)

(defcustom ps-lpr-command lpr-command
  "*Name of program for printing a PostScript file.

On MS-DOS and MS-Windows systems, if the value is an empty string then
Emacs will write directly to the printer port named by `ps-printer-name'.
The programs `print' and `nprint' (the standard print programs on Windows
NT and Novell Netware respectively) are handled specially, using
`ps-printer-name' as the destination for output; any other program is
treated like `lpr' except that an explicit filename is given as the last
argument."
  :type 'string
  :group 'ps-print-printer)

(defcustom ps-lpr-switches lpr-switches
  "*A list of extra switches to pass to `ps-lpr-command'."
  :type '(repeat string)
  :group 'ps-print-printer)

(defcustom ps-print-region-function nil
  "*Specify a function to print the region on a PostScript printer.
See definition of `call-process-region' for calling conventions.  The fourth and
the sixth arguments are both nil."
  :type 'function
  :group 'ps-print-printer)

;;; Page layout

;; All page dimensions are in PostScript points.
;; 1 inch  ==       2.54  cm    ==     72       points
;; 1 cm    ==  (/ 1 2.54) inch  ==  (/ 72 2.54) points

;; Letter      8.5   inch x 11.0   inch
;; Legal       8.5   inch x 14.0   inch
;; A4          8.26  inch x 11.69  inch = 21.0 cm x 29.7 cm

;; LetterSmall 7.68  inch x 10.16  inch
;; Tabloid    11.0   inch x 17.0   inch
;; Ledger     17.0   inch x 11.0   inch
;; Statement   5.5   inch x  8.5   inch
;; Executive   7.5   inch x 10.0   inch
;; A3         11.69  inch x 16.5   inch = 29.7 cm x 42.0 cm
;; A4Small     7.47  inch x 10.85  inch
;; B4         10.125 inch x 14.33  inch
;; B5          7.16  inch x 10.125 inch

(defcustom ps-page-dimensions-database
  (list (list 'a4    (/ (* 72 21.0) 2.54) (/ (* 72 29.7) 2.54) "A4")
	(list 'a3    (/ (* 72 29.7) 2.54) (/ (* 72 42.0) 2.54) "A3")
	(list 'letter       (* 72  8.5)   (* 72 11.0)          "Letter")
	(list 'legal        (* 72  8.5)   (* 72 14.0)          "Legal")
	(list 'letter-small (* 72  7.68)  (* 72 10.16)         "LetterSmall")
	(list 'tabloid      (* 72 11.0)   (* 72 17.0)          "Tabloid")
	(list 'ledger       (* 72 17.0)   (* 72 11.0)          "Ledger")
	(list 'statement    (* 72  5.5)   (* 72  8.5)          "Statement")
	(list 'executive    (* 72  7.5)   (* 72 10.0)          "Executive")
	(list 'a4small      (* 72  7.47)  (* 72 10.85)         "A4Small")
	(list 'b4           (* 72 10.125) (* 72 14.33)         "B4")
	(list 'b5           (* 72  7.16)  (* 72 10.125)        "B5"))
  "*List associating a symbolic paper type to its width, height and doc media.
See `ps-paper-type'."
  :type '(repeat (list :tag "Paper Type"
		       (symbol :tag "Name")
		       (number :tag "Width")
		       (number :tag "Height")
		       (string :tag "Media")))
  :group 'ps-print-page)

;;;###autoload
(defcustom ps-paper-type 'letter
  "*Specify the size of paper to format for.
Should be one of the paper types defined in `ps-page-dimensions-database', for
example `letter', `legal' or `a4'."
  :type '(symbol :validate (lambda (wid)
			     (if (assq (widget-value wid)
				       ps-page-dimensions-database)
				 nil
			       (widget-put wid :error "Unknown paper size")
			       wid)))
  :group 'ps-print-page)

(defcustom ps-landscape-mode nil
  "*Non-nil means print in landscape mode."
  :type 'boolean
  :group 'ps-print-page)

(defcustom ps-print-control-characters 'control-8-bit
  "*Specify the printable form for control and 8-bit characters.
That is, instead of sending, for example, a ^D (\\004) to printer,
it is sent the string \"^D\".

Valid values are:

  `8-bit'         This is the value to use when you want an ASCII encoding of
		  any control or non-ASCII character.  Control characters are
		  encoded as \"^D\", and non-ASCII characters have an
		  octal encoding.

  `control-8-bit' This is the value to use when you want an ASCII encoding of
		  any control character, whether it is 7 or 8-bit.
		  European 8-bits accented characters are printed according
		  the current font.

  `control'       Only ASCII control characters have an ASCII encoding.
		  European 8-bits accented characters are printed according
		  the current font.

  nil             No ASCII encoding.  Any character is printed according the
		  current font.

Any other value is treated as nil."
  :type '(choice :tag "Control Char"
		 (const 8-bit)   (const control-8-bit)
		 (const control) (other :tag "nil" nil))
  :group 'ps-print)

(defcustom ps-n-up-printing 1
  "*Specify the number of pages per sheet paper."
  :type '(integer
	  :tag "N Up Printing"
	  :validate
	  (lambda (wid)
	    (if (and (< 0 (widget-value wid))
		     (<= (widget-value wid) 100))
		nil
	      (widget-put
	       wid :error
	       "Number of pages per sheet paper must be between 1 and 100.")
	      wid)))
  :group 'ps-print-n-up)

(defcustom ps-n-up-margin (/ (* 72  1.0) 2.54) ; 1 cm
  "*Specify the margin in points between the sheet border and n-up printing."
  :type 'number
  :group 'ps-print-n-up)

(defcustom ps-n-up-border-p t
  "*Non-nil means a border is drawn around each page."
  :type 'boolean
  :group 'ps-print-n-up)

(defcustom ps-n-up-filling 'left-top
  "*Specify how page matrix is filled on each sheet of paper.

Following are the valid values for `ps-n-up-filling' with a filling example
using a 3x4 page matrix:

   `left-top'   1  2  3  4         `left-bottom'    9  10 11 12
		5  6  7  8                          5  6  7  8
		9  10 11 12                         1  2  3  4

   `right-top'  4  3  2  1         `right-bottom'   12 11 10 9
		8  7  6  5                          8  7  6  5
		12 11 10 9                          4  3  2  1

   `top-left'   1  4  7  10        `bottom-left'    3  6  9  12
		2  5  8  11                         2  5  8  11
		3  6  9  12                         1  4  7  10

   `top-right'  10 7  4  1         `bottom-right'   12 9  6  3
		11 8  5  2                          11 8  5  2
		12 9  6  3                          10 7  4  1

Any other value is treated as `left-top'."
  :type '(choice :tag "N-Up Filling"
		 (const left-top)  (const left-bottom)
		 (const right-top) (const right-bottom)
		 (const top-left)  (const bottom-left)
		 (const top-right) (const bottom-right))
  :group 'ps-print-n-up)

(defcustom ps-number-of-columns (if ps-landscape-mode 2 1)
  "*Specify the number of columns"
  :type 'number
  :group 'ps-print)

(defcustom ps-zebra-stripes nil
  "*Non-nil means print zebra stripes.
See also documentation for `ps-zebra-stripe-height' and `ps-zebra-gray'."
  :type 'boolean
  :group 'ps-print-zebra)

(defcustom ps-zebra-stripe-height 3
  "*Number of zebra stripe lines.
See also documentation for `ps-zebra-stripes' and `ps-zebra-gray'."
  :type 'number
  :group 'ps-print-zebra)

(defcustom ps-zebra-gray 0.95
  "*Zebra stripe gray scale.
See also documentation for `ps-zebra-stripes' and `ps-zebra-stripe-height'."
  :type 'number
  :group 'ps-print-zebra)

(defcustom ps-line-number nil
  "*Non-nil means print line number."
  :type 'boolean
  :group 'ps-print)

(defcustom ps-print-background-image nil
  "*EPS image list to be printed on background.

The elements are:

   (FILENAME X Y XSCALE YSCALE ROTATION PAGES...)

FILENAME is a file name which contains an EPS image or some PostScript
programming like EPS.
FILENAME is ignored, if it doesn't exist or is read protected.

X and Y are relative positions on paper to put the image.
If X and Y are nil, the image is centralized on paper.

XSCALE and YSCALE are scale factor to be applied to image before printing.
If XSCALE and YSCALE are nil, the original size is used.

ROTATION is the image rotation angle; if nil, the default is 0.

PAGES designates the page to print background image.
PAGES may be a number or a cons cell (FROM . TO) designating FROM page
to TO page.
If PAGES is nil, print background image on all pages.

X, Y, XSCALE, YSCALE and ROTATION may be a floating point number,
an integer number or a string. If it is a string, the string should contain
PostScript programming that returns a float or integer value.

For example, if you wish to print an EPS image on all pages do:

   '((\"~/images/EPS-image.ps\"))"
  :type '(repeat (list (file   :tag "EPS File")
		       (choice :tag "X" number string (const nil))
		       (choice :tag "Y" number string (const nil))
		       (choice :tag "X Scale" number string (const nil))
		       (choice :tag "Y Scale" number string (const nil))
		       (choice :tag "Rotation" number string (const nil))
		       (repeat :tag "Pages" :inline t
			       (radio (integer :tag "Page")
				      (cons :tag "Range"
					    (integer :tag "From")
					    (integer :tag "To"))))))
  :group 'ps-print-background)

(defcustom ps-print-background-text nil
  "*Text list to be printed on background.

The elements are:

   (STRING X Y FONT FONTSIZE GRAY ROTATION PAGES...)

STRING is the text to be printed on background.

X and Y are positions on paper to put the text.
If X and Y are nil, the text is positioned at lower left corner.

FONT is a font name to be used on printing the text.
If nil, \"Times-Roman\" is used.

FONTSIZE is font size to be used, if nil, 200 is used.

GRAY is the text gray factor (should be very light like 0.8).
If nil, the default is 0.85.

ROTATION is the text rotation angle; if nil, the angle is given by
the diagonal from lower left corner to upper right corner.

PAGES designates the page to print background text.
PAGES may be a number or a cons cell (FROM . TO) designating FROM page
to TO page.
If PAGES is nil, print background text on all pages.

X, Y, FONTSIZE, GRAY and ROTATION may be a floating point number,
an integer number or a string. If it is a string, the string should contain
PostScript programming that returns a float or integer value.

For example, if you wish to print text \"Preliminary\" on all pages do:

   '((\"Preliminary\"))"
  :type '(repeat (list (string :tag "Text")
		       (choice :tag "X" number string (const nil))
		       (choice :tag "Y" number string (const nil))
		       (choice :tag "Font" string (const nil))
		       (choice :tag "Fontsize" number string (const nil))
		       (choice :tag "Gray" number string (const nil))
		       (choice :tag "Rotation" number string (const nil))
		       (repeat :tag "Pages" :inline t
			       (radio (integer :tag "Page")
				      (cons :tag "Range"
					    (integer :tag "From")
					    (integer :tag "To"))))))
  :group 'ps-print-background)

;;; Horizontal layout

;;  ------------------------------------------
;;  |    |      |    |      |    |      |    |
;;  | lm | text | ic | text | ic | text | rm |
;;  |    |      |    |      |    |      |    |
;;  ------------------------------------------

(defcustom ps-left-margin   (/ (* 72  2.0) 2.54) ;   2 cm
  "*Left margin in points (1/72 inch)."
  :type 'number
  :group 'ps-print-horizontal)

(defcustom ps-right-margin  (/ (* 72  2.0) 2.54) ;   2 cm
  "*Right margin in points (1/72 inch)."
  :type 'number
  :group 'ps-print-horizontal)

(defcustom ps-inter-column  (/ (* 72  2.0) 2.54) ;   2 cm
  "*Horizontal space between columns in points (1/72 inch)."
  :type 'number
  :group 'ps-print-horizontal)

;;; Vertical layout

;; |--------|
;; | tm     |
;; |--------|
;; | header |
;; |--------|
;; | ho     |
;; |--------|
;; | text   |
;; |--------|
;; | bm     |
;; |--------|

(defcustom ps-bottom-margin (/ (* 72  1.5) 2.54) ; 1.5 cm
  "*Bottom margin in points (1/72 inch)."
  :type 'number
  :group 'ps-print-vertical)

(defcustom ps-top-margin    (/ (* 72  1.5) 2.54) ; 1.5 cm
  "*Top margin in points (1/72 inch)."
  :type 'number
  :group 'ps-print-vertical)

(defcustom ps-header-offset (/ (* 72  1.0) 2.54) ; 1.0 cm
  "*Vertical space in points (1/72 inch) between the main text and the header."
  :type 'number
  :group 'ps-print-vertical)

(defcustom ps-header-line-pad 0.15
  "*Portion of a header title line height to insert between the header frame
and the text it contains, both in the vertical and horizontal directions."
  :type 'number
  :group 'ps-print-vertical)

;;; Header setup

(defcustom ps-print-header t
  "*Non-nil means print a header at the top of each page.
By default, the header displays the buffer name, page number, and, if
the buffer is visiting a file, the file's directory.  Headers are
customizable by changing variables `ps-left-header' and
`ps-right-header'."
  :type 'boolean
  :group 'ps-print-header)

(defcustom ps-print-only-one-header nil
  "*Non-nil means print only one header at the top of each page.
This is useful when printing more than one column, so it is possible
to have only one header over all columns or one header per column.
See also `ps-print-header'."
  :type 'boolean
  :group 'ps-print-header)

(defcustom ps-print-header-frame t
  "*Non-nil means draw a gaudy frame around the header."
  :type 'boolean
  :group 'ps-print-header)

(defcustom ps-header-lines 2
  "*Number of lines to display in page header, when generating PostScript."
  :type 'integer
  :group 'ps-print-header)
(make-variable-buffer-local 'ps-header-lines)

(defcustom ps-show-n-of-n t
  "*Non-nil means show page numbers as N/M, meaning page N of M.
NOTE: page numbers are displayed as part of headers,
      see variable `ps-print-headers'."
  :type 'boolean
  :group 'ps-print-header)

(defcustom ps-spool-config 'lpr-switches
  "*Specify who is responsable for setting duplex and page size switches.

Valid values are:

  `lpr-switches'    duplex and page size are configured by `ps-lpr-switches'.
		    Don't forget to set `ps-lpr-switches' to select duplex
		    printing for your printer.

  `setpagedevice'   duplex and page size are configured by ps-print using the
		    setpagedevice PostScript operator.

  nil               duplex and page size are configured by ps-print *not* using
		    the setpagedevice PostScript operator.

Any other value is treated as nil.

WARNING: The setpagedevice PostScript operator affects ghostview utility when
	 viewing file generated using landscape.  Also on some printers,
	 setpagedevice affects zebra stripes; on other printers, setpagedevice
	 affects the left margin.
	 Besides all that, if your printer does not have the paper size
	 specified by setpagedevice, your printing will be aborted.
	 So, if you need to use setpagedevice, set `ps-spool-config' to
	 `setpagedevice', generate a test file and send it to your printer; if
	 the printed file isn't ok, set `ps-spool-config' to nil."
  :type '(choice :tag "Spool Config"
		 (const lpr-switches) (const setpagedevice)
		 (other :tag "nil" nil))
  :group 'ps-print-header)

(defcustom ps-spool-duplex nil		; Not many people have duplex printers,
					; so default to nil.
  "*Non-nil generates PostScript for a two-sided printer.
For a duplex printer, the `ps-spool-*' and `ps-print-*' commands will insert
blank pages as needed between print jobs so that the next buffer printed will
start on the right page.  Also, if headers are turned on, the headers will be
reversed on duplex printers so that the page numbers fall to the left on
even-numbered pages."
  :type 'boolean
  :group 'ps-print-header)

(defcustom ps-spool-tumble nil
  "*Specify how the page images on opposite sides of a sheet are oriented.
If `ps-spool-tumble' is nil, produces output suitable for binding on the left or
right.  If `ps-spool-tumble' is non-nil, produces output suitable for binding at
the top or bottom.

It has effect only when `ps-spool-duplex' is non-nil."
  :type 'boolean
  :group 'ps-print-header)

;;; Fonts

(defcustom ps-font-info-database
  '((Courier				; the family key
     (fonts (normal      . "Courier")
	    (bold        . "Courier-Bold")
	    (italic      . "Courier-Oblique")
	    (bold-italic . "Courier-BoldOblique"))
     (size . 10.0)
     (line-height . 10.55)
     (space-width . 6.0)
     (avg-char-width . 6.0))
    (Helvetica				; the family key
     (fonts (normal      . "Helvetica")
	    (bold        . "Helvetica-Bold")
	    (italic      . "Helvetica-Oblique")
	    (bold-italic . "Helvetica-BoldOblique"))
     (size . 10.0)
     (line-height . 11.56)
     (space-width . 2.78)
     (avg-char-width . 5.09243))
    (Times
     (fonts (normal      . "Times-Roman")
	    (bold        . "Times-Bold")
	    (italic      . "Times-Italic")
	    (bold-italic . "Times-BoldItalic"))
     (size . 10.0)
     (line-height . 11.0)
     (space-width . 2.5)
     (avg-char-width . 4.71432))
    (Palatino
     (fonts (normal      . "Palatino-Roman")
	    (bold        . "Palatino-Bold")
	    (italic      . "Palatino-Italic")
	    (bold-italic . "Palatino-BoldItalic"))
     (size . 10.0)
     (line-height . 12.1)
     (space-width . 2.5)
     (avg-char-width . 5.08676))
    (Helvetica-Narrow
     (fonts (normal      . "Helvetica-Narrow")
	    (bold        . "Helvetica-Narrow-Bold")
	    (italic      . "Helvetica-Narrow-Oblique")
	    (bold-italic . "Helvetica-Narrow-BoldOblique"))
     (size . 10.0)
     (line-height . 11.56)
     (space-width . 2.2796)
     (avg-char-width . 4.17579))
    (NewCenturySchlbk
     (fonts (normal      . "NewCenturySchlbk-Roman")
	    (bold        . "NewCenturySchlbk-Bold")
	    (italic      . "NewCenturySchlbk-Italic")
	    (bold-italic . "NewCenturySchlbk-BoldItalic"))
     (size . 10.0)
     (line-height . 12.15)
     (space-width . 2.78)
     (avg-char-width . 5.31162))
    ;; got no bold for the next ones
    (AvantGarde-Book
     (fonts (normal . "AvantGarde-Book")
	    (italic . "AvantGarde-BookOblique"))
     (size . 10.0)
     (line-height . 11.77)
     (space-width . 2.77)
     (avg-char-width . 5.45189))
    (AvantGarde-Demi
     (fonts (normal . "AvantGarde-Demi")
	    (italic . "AvantGarde-DemiOblique"))
     (size . 10.0)
     (line-height . 12.72)
     (space-width . 2.8)
     (avg-char-width . 5.51351))
    (Bookman-Demi
     (fonts (normal . "Bookman-Demi")
	    (italic . "Bookman-DemiItalic"))
     (size . 10.0)
     (line-height . 11.77)
     (space-width . 3.4)
     (avg-char-width . 6.05946))
    (Bookman-Light
     (fonts (normal . "Bookman-Light")
	    (italic . "Bookman-LightItalic"))
     (size . 10.0)
     (line-height . 11.79)
     (space-width . 3.2)
     (avg-char-width . 5.67027))
    ;; got no bold and no italic for the next ones
    (Symbol
     (fonts (normal . "Symbol"))
     (size . 10.0)
     (line-height . 13.03)
     (space-width . 2.5)
     (avg-char-width . 3.24324))
    (Zapf-Dingbats
     (fonts (normal . "Zapf-Dingbats"))
     (size . 10.0)
     (line-height . 9.63)
     (space-width . 2.78)
     (avg-char-width . 2.78))
    (Zapf-Chancery-MediumItalic
     (fonts (normal . "Zapf-Chancery-MediumItalic"))
     (size . 10.0)
     (line-height . 11.45)
     (space-width . 2.2)
     (avg-char-width . 4.10811))
    )
  "*Font info database: font family (the key), name, bold, italic, bold-italic,
reference size, line height, space width, average character width.
To get the info for another specific font (say Helvetica), do the following:
- create a new buffer
- generate the PostScript image to a file (C-u M-x ps-print-buffer)
- open this file and delete the leading `%' (which is the PostScript
  comment character) from the line
	   `% 3 cm 20 cm moveto  10 /Courier ReportFontInfo  showpage'
  to get the line
	   `3 cm 20 cm moveto  10 /Helvetica ReportFontInfo  showpage'
- add the values to `ps-font-info-database'.
You can get all the fonts of YOUR printer using `ReportAllFontInfo'."
  :type '(repeat (list :tag "Font Definition"
		       (symbol :tag "Font Family")
		       (cons :format "%v"
			     (const :format "" fonts)
			     (repeat :tag "Faces"
				     (cons (choice (const normal)
						   (const bold)
						   (const italic)
						   (const bold-italic)
						   (symbol :tag "Face"))
					   (string :tag "Font Name"))))
		       (cons :format "%v"
			     (const :format "" size)
			     (number :tag "Reference Size"))
		       (cons :format "%v"
			     (const :format "" line-height)
			     (number :tag "Line Height"))
		       (cons :format "%v"
			     (const :format "" space-width)
			     (number :tag "Space Width"))
		       (cons :format "%v"
			     (const :format "" avg-char-width)
			     (number :tag "Average Character Width"))))
  :group 'ps-print-font)

(defcustom ps-font-family 'Courier
  "*Font family name for ordinary text, when generating PostScript."
  :type 'symbol
  :group 'ps-print-font)

(defcustom ps-font-size   '(7 . 8.5)
  "*Font size, in points, for ordinary text, when generating PostScript."
  :type '(choice (number :tag "Text Size")
		 (cons :tag "Landscape/Portrait"
		       (number :tag "Landscape Text Size")
		       (number :tag "Portrait Text Size")))
  :group 'ps-print-font)

(defcustom ps-header-font-family      'Helvetica
  "*Font family name for text in the header, when generating PostScript."
  :type 'symbol
  :group 'ps-print-font)

(defcustom ps-header-font-size       '(10 . 12)
  "*Font size, in points, for text in the header, when generating PostScript."
  :type '(choice (number :tag "Header Size")
		 (cons :tag "Landscape/Portrait"
		       (number :tag "Landscape Header Size")
		       (number :tag "Portrait Header Size")))
  :group 'ps-print-font)

(defcustom ps-header-title-font-size '(12 . 14)
  "*Font size, in points, for the top line of text in header, in PostScript."
  :type '(choice (number :tag "Header Title Size")
		 (cons :tag "Landscape/Portrait"
		       (number :tag "Landscape Header Title Size")
		       (number :tag "Portrait Header Title Size")))
  :group 'ps-print-font)

;;; Colors

;; Printing color requires x-color-values.
(defcustom ps-print-color-p (or (fboundp 'x-color-values) ; Emacs
				(fboundp 'color-instance-rgb-components))
					; XEmacs
  "*Non-nil means print the buffer's text in color."
  :type 'boolean
  :group 'ps-print-color)

(defcustom ps-default-fg '(0.0 0.0 0.0)
  "*RGB values of the default foreground color.  Defaults to black."
  :type '(list (number :tag "Red") (number :tag "Green") (number :tag "Blue"))
  :group 'ps-print-color)

(defcustom ps-default-bg '(1.0 1.0 1.0)
  "*RGB values of the default background color.  Defaults to white."
  :type '(list (number :tag "Red") (number :tag "Green") (number :tag "Blue"))
  :group 'ps-print-color)

(defcustom ps-auto-font-detect t
  "*Non-nil means automatically detect bold/italic face attributes.
If nil, we rely solely on the lists `ps-bold-faces', `ps-italic-faces',
and `ps-underlined-faces'."
  :type 'boolean
  :group 'ps-print-font)

(defcustom ps-bold-faces
  (unless ps-print-color-p
    '(font-lock-function-name-face
      font-lock-builtin-face
      font-lock-variable-name-face
      font-lock-keyword-face
      font-lock-warning-face))
  "*A list of the \(non-bold\) faces that should be printed in bold font.
This applies to generating PostScript."
  :type '(repeat face)
  :group 'ps-print-face)

(defcustom ps-italic-faces
  (unless ps-print-color-p
    '(font-lock-variable-name-face
      font-lock-type-face
      font-lock-string-face
      font-lock-comment-face
      font-lock-warning-face))
  "*A list of the \(non-italic\) faces that should be printed in italic font.
This applies to generating PostScript."
  :type '(repeat face)
  :group 'ps-print-face)

(defcustom ps-underlined-faces
  (unless ps-print-color-p
    '(font-lock-function-name-face
      font-lock-constant-face
      font-lock-warning-face))
  "*A list of the \(non-underlined\) faces that should be printed underlined.
This applies to generating PostScript."
  :type '(repeat face)
  :group 'ps-print-face)

(defcustom ps-left-header
  (list 'ps-get-buffer-name 'ps-header-dirpart)
  "*The items to display (each on a line) on the left part of the page header.
This applies to generating PostScript.

The value should be a list of strings and symbols, each representing an
entry in the PostScript array HeaderLinesLeft.

Strings are inserted unchanged into the array; those representing
PostScript string literals should be delimited with PostScript string
delimiters '(' and ')'.

For symbols with bound functions, the function is called and should
return a string to be inserted into the array.  For symbols with bound
values, the value should be a string to be inserted into the array.
In either case, function or variable, the string value has PostScript
string delimiters added to it."
  :type '(repeat (choice string symbol))
  :group 'ps-print-header)
(make-variable-buffer-local 'ps-left-header)

(defcustom ps-right-header
  (list "/pagenumberstring load" 'time-stamp-mon-dd-yyyy 'time-stamp-hh:mm:ss)
  "*The items to display (each on a line) on the right part of the page header.
This applies to generating PostScript.

See the variable `ps-left-header' for a description of the format of
this variable."
  :type '(repeat (choice string symbol))
  :group 'ps-print-header)
(make-variable-buffer-local 'ps-right-header)

(defcustom ps-razzle-dazzle t
  "*Non-nil means report progress while formatting buffer."
  :type 'boolean
  :group 'ps-print)

(defcustom ps-adobe-tag "%!PS-Adobe-3.0\n"
  "*Contains the header line identifying the output as PostScript.
By default, `ps-adobe-tag' contains the standard identifier.  Some
printers require slightly different versions of this line."
  :type 'string
  :group 'ps-print)

(defcustom ps-build-face-reference t
  "*Non-nil means build the reference face lists.

ps-print sets this value to nil after it builds its internal reference
lists of bold and italic faces.  By settings its value back to t, you
can force ps-print to rebuild the lists the next time you invoke one
of the ...-with-faces commands.

You should set this value back to t after you change the attributes of
any face, or create new faces.  Most users shouldn't have to worry
about its setting, though."
  :type 'boolean
  :group 'ps-print-face)

(defcustom ps-always-build-face-reference nil
  "*Non-nil means always rebuild the reference face lists.

If this variable is non-nil, ps-print will rebuild its internal
reference lists of bold and italic faces *every* time one of the
...-with-faces commands is called.  Most users shouldn't need to set this
variable."
  :type 'boolean
  :group 'ps-print-face)

(defcustom ps-banner-page-when-duplexing nil
  "*Non-nil means the very first page is skipped.
It's like the very first character of buffer (or region) is ^L (\\014)."
  :type 'boolean
  :group 'ps-print-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User commands

;;;###autoload
(defun ps-print-buffer (&optional filename)
  "Generate and print a PostScript image of the buffer.

Interactively, when you use a prefix argument (C-u), the command
prompts the user for a file name, and saves the PostScript image
in that file instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it
is nil, send the image to the printer.  If FILENAME is a string, save
the PostScript image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (ps-print-without-faces (point-min) (point-max) filename))


;;;###autoload
(defun ps-print-buffer-with-faces (&optional filename)
  "Generate and print a PostScript image of the buffer.
Like `ps-print-buffer', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (ps-print-with-faces (point-min) (point-max) filename))


;;;###autoload
(defun ps-print-region (from to &optional filename)
  "Generate and print a PostScript image of the region.
Like `ps-print-buffer', but prints just the current region."
  (interactive (list (point) (mark) (ps-print-preprint current-prefix-arg)))
  (ps-print-without-faces from to filename t))


;;;###autoload
(defun ps-print-region-with-faces (from to &optional filename)
  "Generate and print a PostScript image of the region.
Like `ps-print-region', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values."
  (interactive (list (point) (mark) (ps-print-preprint current-prefix-arg)))
  (ps-print-with-faces from to filename t))


;;;###autoload
(defun ps-spool-buffer ()
  "Generate and spool a PostScript image of the buffer.
Like `ps-print-buffer' except that the PostScript image is saved in a
local buffer to be sent to the printer later.

Use the command `ps-despool' to send the spooled images to the printer."
  (interactive)
  (ps-spool-without-faces (point-min) (point-max)))


;;;###autoload
(defun ps-spool-buffer-with-faces ()
  "Generate and spool a PostScript image of the buffer.
Like `ps-spool-buffer', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values.

Use the command `ps-despool' to send the spooled images to the printer."
  (interactive)
  (ps-spool-with-faces (point-min) (point-max)))


;;;###autoload
(defun ps-spool-region (from to)
  "Generate a PostScript image of the region and spool locally.
Like `ps-spool-buffer', but spools just the current region.

Use the command `ps-despool' to send the spooled images to the printer."
  (interactive "r")
  (ps-spool-without-faces from to t))


;;;###autoload
(defun ps-spool-region-with-faces (from to)
  "Generate a PostScript image of the region and spool locally.
Like `ps-spool-region', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values.

Use the command `ps-despool' to send the spooled images to the printer."
  (interactive "r")
  (ps-spool-with-faces from to t))

;;;###autoload
(defun ps-despool (&optional filename)
  "Send the spooled PostScript to the printer.

Interactively, when you use a prefix argument (C-u), the command
prompts the user for a file name, and saves the spooled PostScript
image in that file instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it
is nil, send the image to the printer.  If FILENAME is a string, save
the PostScript image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (ps-do-despool filename))

;;;###autoload
(defun ps-line-lengths ()
  "Display the correspondence between a line length and a font size,
using the current ps-print setup.
Try: pr -t file | awk '{printf \"%3d %s\n\", length($0), $0}' | sort -r | head"
  (interactive)
  (ps-line-lengths-internal))

;;;###autoload
(defun ps-nb-pages-buffer (nb-lines)
  "Display number of pages to print this buffer, for various font heights.
The table depends on the current ps-print setup."
  (interactive (list (count-lines (point-min) (point-max))))
  (ps-nb-pages nb-lines))

;;;###autoload
(defun ps-nb-pages-region (nb-lines)
  "Display number of pages to print the region, for various font heights.
The table depends on the current ps-print setup."
  (interactive (list (count-lines (mark) (point))))
  (ps-nb-pages nb-lines))

;;;###autoload
(defun ps-setup ()
  "Return the current PostScript-generation setup."
  (format
   "
\(setq ps-print-color-p         %s
      ps-lpr-command           %S
      ps-lpr-switches          %s
      ps-printer-name          %S
      ps-print-region-function %s

      ps-paper-type          %s
      ps-landscape-mode      %s
      ps-number-of-columns   %s

      ps-zebra-stripes       %s
      ps-zebra-stripe-height %s
      ps-zebra-gray          %s
      ps-line-number         %s

      ps-print-control-characters %s

      ps-print-background-image %s

      ps-print-background-text %s

      ps-print-prologue-header %s

      ps-left-margin                %s
      ps-right-margin               %s
      ps-inter-column               %s
      ps-bottom-margin              %s
      ps-top-margin                 %s
      ps-header-offset              %s
      ps-header-line-pad            %s
      ps-print-header               %s
      ps-print-only-one-header      %s
      ps-print-header-frame         %s
      ps-header-lines               %s
      ps-show-n-of-n                %s
      ps-spool-config               %s
      ps-spool-duplex               %s
      ps-spool-tumble               %s
      ps-banner-page-when-duplexing %s

      ps-n-up-printing         %s
      ps-n-up-margin           %s
      ps-n-up-border-p         %s
      ps-n-up-filling          %s

      ps-multibyte-buffer       %s
      ps-font-family            %s
      ps-font-size              %s
      ps-header-font-family     %s
      ps-header-font-size       %s
      ps-header-title-font-size %s)
"
   ps-print-color-p
   ps-lpr-command
   (ps-print-quote ps-lpr-switches)
   ps-printer-name
   (ps-print-quote ps-print-region-function)
   (ps-print-quote ps-paper-type)
   ps-landscape-mode
   ps-number-of-columns
   ps-zebra-stripes
   ps-zebra-stripe-height
   ps-zebra-gray
   ps-line-number
   (ps-print-quote ps-print-control-characters)
   (ps-print-quote ps-print-background-image)
   (ps-print-quote ps-print-background-text)
   (ps-print-quote ps-print-prologue-header)
   ps-left-margin
   ps-right-margin
   ps-inter-column
   ps-bottom-margin
   ps-top-margin
   ps-header-offset
   ps-header-line-pad
   ps-print-header
   ps-print-only-one-header
   ps-print-header-frame
   ps-header-lines
   ps-show-n-of-n
   (ps-print-quote ps-spool-config)
   ps-spool-duplex
   ps-spool-tumble
   ps-banner-page-when-duplexing
   ps-n-up-printing
   ps-n-up-margin
   ps-n-up-border-p
   (ps-print-quote ps-n-up-filling)
   (ps-print-quote ps-multibyte-buffer)	; see `ps-mule.el' and `ps-print-def.el'
   (ps-print-quote ps-font-family)
   (ps-print-quote ps-font-size)
   (ps-print-quote ps-header-font-family)
   (ps-print-quote ps-header-font-size)
   (ps-print-quote ps-header-title-font-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions and variables:

(defun ps-print-quote (sym)
  (cond ((null sym)
	 nil)
	((or (symbolp sym) (listp sym))
	 (format "'%S" sym))
	((stringp sym)
	 (format "%S" sym))
	(t
	 sym)))

(defvar ps-print-emacs-type
  (cond ((string-match "XEmacs" emacs-version) 'xemacs)
	((string-match "Lucid" emacs-version) 'lucid)
	((string-match "Epoch" emacs-version) 'epoch)
	(t 'emacs)))

(if (or (eq ps-print-emacs-type 'lucid)
	(eq ps-print-emacs-type 'xemacs))
    (if (< emacs-minor-version 12)
	(setq ps-print-color-p nil))
  (require 'faces))			; face-font, face-underline-p,
					; x-font-regexp

;; Return t if the device (which can be changed during an emacs session)
;; can handle colors.
;; This is function is not yet implemented for GNU emacs.
(cond ((and (eq ps-print-emacs-type 'xemacs)
	    (>= emacs-minor-version 12)) ; xemacs
       (defun ps-color-device ()
	 (eq (device-class) 'color))
       )

      (t				; emacs
       (defun ps-color-device ()
	 t)
       ))


(require 'time-stamp)

(defconst ps-print-prologue-1
  "
% ISOLatin1Encoding stolen from ps_init.ps in GhostScript 2.6.1.4:
/ISOLatin1Encoding where { pop } {
% -- The ISO Latin-1 encoding vector isn't known, so define it.
% -- The first half is the same as the standard encoding,
% -- except for minus instead of hyphen at code 055.
/ISOLatin1Encoding
StandardEncoding 0 45 getinterval aload pop
    /minus
StandardEncoding 46 82 getinterval aload pop
%*** NOTE: the following are missing in the Adobe documentation,
%*** but appear in the displayed table:
%*** macron at 0225, dieresis at 0230, cedilla at 0233, space at 0240.
% 0200 (128)
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /dotlessi /grave /acute /circumflex /tilde /macron /breve /dotaccent
    /dieresis /.notdef /ring /cedilla /.notdef /hungarumlaut /ogonek /caron
% 0240 (160)
    /space /exclamdown /cent /sterling
	/currency /yen /brokenbar /section
    /dieresis /copyright /ordfeminine /guillemotleft
	/logicalnot /hyphen /registered /macron
    /degree /plusminus /twosuperior /threesuperior
	/acute /mu /paragraph /periodcentered
    /cedilla /onesuperior /ordmasculine /guillemotright
	/onequarter /onehalf /threequarters /questiondown
% 0300 (192)
    /Agrave /Aacute /Acircumflex /Atilde
	/Adieresis /Aring /AE /Ccedilla
    /Egrave /Eacute /Ecircumflex /Edieresis
	/Igrave /Iacute /Icircumflex /Idieresis
    /Eth /Ntilde /Ograve /Oacute
	/Ocircumflex /Otilde /Odieresis /multiply
    /Oslash /Ugrave /Uacute /Ucircumflex
	/Udieresis /Yacute /Thorn /germandbls
% 0340 (224)
    /agrave /aacute /acircumflex /atilde
	/adieresis /aring /ae /ccedilla
    /egrave /eacute /ecircumflex /edieresis
	/igrave /iacute /icircumflex /idieresis
    /eth /ntilde /ograve /oacute
	/ocircumflex /otilde /odieresis /divide
    /oslash /ugrave /uacute /ucircumflex
	/udieresis /yacute /thorn /ydieresis
256 packedarray def
} ifelse

/reencodeFontISO { %def
  dup
  length 12 add dict	% Make a new font (a new dict the same size
			% as the old one) with room for our new symbols.

  begin			% Make the new font the current dictionary.


    { 1 index /FID ne
      { def } { pop pop } ifelse
    } forall		% Copy each of the symbols from the old dictionary
			% to the new one except for the font ID.

    currentdict /FontType get 0 ne {
      /Encoding ISOLatin1Encoding def	% Override the encoding with
					% the ISOLatin1 encoding.
    } if

    % Use the font's bounding box to determine the ascent, descent,
    % and overall height; don't forget that these values have to be
    % transformed using the font's matrix.

%          ^    (x2 y2)
%          |       |
%          |       v
%          |  +----+ - -
%          |  |    |   ^
%          |  |    |   | Ascent (usually > 0)
%          |  |    |   |
% (0 0) -> +--+----+-------->
%             |    |   |
%             |    |   v Descent (usually < 0)
% (x1 y1) --> +----+ - -

    currentdict /FontType get 0 ne {
      /FontBBox load aload pop			% -- x1 y1 x2 y2
      FontMatrix transform /Ascent  exch def pop
      FontMatrix transform /Descent exch def pop
    } {
      /PrimaryFont FDepVector 0 get def
      PrimaryFont /FontBBox get aload pop
      PrimaryFont /FontMatrix get transform /Ascent exch def pop
      PrimaryFont /FontMatrix get transform /Descent exch def pop
    } ifelse

    /FontHeight Ascent Descent sub def	% use `sub' because descent < 0

    % Define these in case they're not in the FontInfo
    % (also, here they're easier to get to).
    /UnderlinePosition  Descent 0.70 mul def
    /OverlinePosition   Descent UnderlinePosition sub Ascent add def
    /StrikeoutPosition  Ascent 0.30 mul def
    /LineThickness      FontHeight 0.05 mul def
    /Xshadow            FontHeight  0.08 mul def
    /Yshadow            FontHeight -0.09 mul def
    /SpaceBackground    Descent neg UnderlinePosition add def
    /XBox               Descent neg def
    /YBox               LineThickness 0.7 mul def

    currentdict		% Leave the new font on the stack
    end			% Stop using the font as the current dictionary.
    definefont		% Put the font into the font dictionary
    pop			% Discard the returned font.
} bind def

/DefFont {				% Font definition
  findfont exch scalefont reencodeFontISO
} def

/F {					% Font selection
  findfont
  dup /Ascent            get /Ascent            exch def
  dup /Descent           get /Descent           exch def
  dup /FontHeight        get /FontHeight        exch def
  dup /UnderlinePosition get /UnderlinePosition exch def
  dup /OverlinePosition  get /OverlinePosition  exch def
  dup /StrikeoutPosition get /StrikeoutPosition exch def
  dup /LineThickness     get /LineThickness     exch def
  dup /Xshadow           get /Xshadow           exch def
  dup /Yshadow           get /Yshadow           exch def
  dup /SpaceBackground   get /SpaceBackground   exch def
  dup /XBox              get /XBox              exch def
  dup /YBox              get /YBox              exch def
  setfont
} def

/FG /setrgbcolor load def

/bg false def
/BG {
  dup /bg exch def
  {mark 4 1 roll ]}
  {[ 1.0 1.0 1.0 ]}
  ifelse
  /bgcolor exch def
} def

%  B    width    C
%   +-----------+
%               | Ascent  (usually > 0)
% A +           +
%               | Descent (usually < 0)
%   +-----------+
%  E    width    D

/dobackground {				% width --
  currentpoint				% -- width x y
  gsave
    newpath
    moveto				% A (x y)
    0 Ascent rmoveto			% B
    dup 0 rlineto			% C
    0 Descent Ascent sub rlineto	% D
    neg 0 rlineto			% E
    closepath
    bgcolor aload pop setrgbcolor
    fill
  grestore
} def

/eolbg {				% dobackground until right margin
  PrintWidth				% -- x-eol
  currentpoint pop			% -- cur-x
  sub					% -- width until eol
  dobackground
} def

/PLN {PrintLineNumber {doLineNumber}if} def

/SL {					% Soft Linefeed
  bg { eolbg } if
  0  currentpoint exch pop LineHeight sub  moveto
} def

/HL {SL PLN} def			% Hard Linefeed

% Some debug
/dcp { currentpoint exch 40 string cvs print (, ) print = } def
/dp { print 2 copy  exch 40 string cvs print (, ) print = } def

/W {
  ( ) stringwidth	% Get the width of a space in the current font.
  pop			% Discard the Y component.
  mul			% Multiply the width of a space
			% by the number of spaces to plot
  bg { dup dobackground } if
  0 rmoveto
} def

/Effect 0 def
/EF {/Effect exch def} def

% stack:  string  |-  --
% effect: 1  - underline  2   - strikeout  4  - overline
%         8  - shadow     16  - box        32 - outline
/S {
  /xx currentpoint dup Descent add /yy exch def
  Ascent add /YY exch def def
  dup stringwidth pop xx add /XX exch def
  Effect 8 and 0 ne {
    /yy yy Yshadow add def
    /XX XX Xshadow add def
  } if
  bg {
    true
    Effect 16 and 0 ne
      {SpaceBackground doBox}
      {xx yy XX YY doRect}
    ifelse
  } if							% background
  Effect 16 and 0 ne {false 0 doBox}if			% box
  Effect 8  and 0 ne {dup doShadow}if			% shadow
  Effect 32 and 0 ne
    {true doOutline}					% outline
    {show}						% normal text
  ifelse
  Effect 1  and 0 ne {UnderlinePosition Hline}if	% underline
  Effect 2  and 0 ne {StrikeoutPosition Hline}if	% strikeout
  Effect 4  and 0 ne {OverlinePosition  Hline}if	% overline
} bind def

% stack:  position  |-  --
/Hline {
  currentpoint exch pop add dup
  gsave
  newpath
  xx exch moveto
  XX exch lineto
  closepath
  LineThickness setlinewidth stroke
  grestore
} bind def

% stack:  fill-or-not delta  |-  --
/doBox {
  /dd exch def
  xx XBox sub dd sub yy YBox sub dd sub
  XX XBox add dd add YY YBox add dd add
  doRect
} bind def

% stack:  fill-or-not lower-x lower-y upper-x upper-y  |-  --
/doRect {
  /rYY exch def
  /rXX exch def
  /ryy exch def
  /rxx exch def
  gsave
  newpath
  rXX rYY moveto
  rxx rYY lineto
  rxx ryy lineto
  rXX ryy lineto
  closepath
  % top of stack: fill-or-not
    {FillBgColor}
    {LineThickness setlinewidth stroke}
  ifelse
  grestore
} bind def

% stack:  string  |-  --
/doShadow {
  gsave
  Xshadow Yshadow rmoveto
  false doOutline
  grestore
} bind def

/st 1 string def

% stack:  string fill-or-not  |-  --
/doOutline {
  /-fillp- exch def
  /-ox- currentpoint /-oy- exch def def
  gsave
  LineThickness setlinewidth
  {
    st 0 3 -1 roll put
    st dup true charpath
    -fillp- {gsave FillBgColor grestore}if
    stroke stringwidth
    -oy- add /-oy- exch def
    -ox- add /-ox- exch def
    -ox- -oy- moveto
  } forall
  grestore
  -ox- -oy- moveto
} bind def

% stack:  --
/FillBgColor {bgcolor aload pop setrgbcolor fill} bind def

/L0 6 /Times-Italic DefFont

% stack:  --
/doLineNumber {
  /LineNumber where
  {
    pop
    currentfont
    gsave
    0.0 0.0 0.0 setrgbcolor
    /L0 findfont setfont
    LineNumber Lines ge
      {(end      )}
      {LineNumber 6 string cvs (      ) strcat}
    ifelse
    dup stringwidth pop neg 0 rmoveto
    show
    grestore
    setfont
    /LineNumber LineNumber 1 add def
  } if
} def

% stack: --
/printZebra {
  gsave
  ZebraGray setgray
  /double-zebra ZebraHeight ZebraHeight add def
  /yiter double-zebra LineHeight mul neg def
  /xiter PrintWidth InterColumn add def
  NumberOfColumns {LinesPerColumn doColumnZebra xiter 0 rmoveto}repeat
  grestore
} def

% stack:  lines-per-column |- --
/doColumnZebra {
  gsave
  dup double-zebra idiv {ZebraHeight doZebra 0 yiter rmoveto}repeat
  double-zebra mod
  dup 0 le {pop}{dup ZebraHeight gt {pop ZebraHeight}if doZebra}ifelse
  grestore
} def

% stack:  zebra-height (in lines) |- --
/doZebra {
  /zh exch 0.05 sub LineHeight mul def
  gsave
  0 LineHeight 0.65 mul rmoveto
  PrintWidth 0 rlineto
  0 zh neg rlineto
  PrintWidth neg 0 rlineto
  0 zh rlineto
  fill
  grestore
} def

% tx ty rotation xscale yscale xpos ypos BeginBackImage
/BeginBackImage {
  /-save-image- save def
  /showpage {}def
  translate
  scale
  rotate
  translate
} def

/EndBackImage {
  -save-image- restore
} def

% string fontsize fontname rotation gray xpos ypos ShowBackText
/ShowBackText {
  gsave
  translate
  setgray
  rotate
  findfont exch dup /-offset- exch -0.25 mul def scalefont setfont
  0 -offset- moveto
  /-saveLineThickness- LineThickness def
  /LineThickness 1 def
  false doOutline
  /LineThickness -saveLineThickness- def
  grestore
} def

/BeginDoc {
  % ---- Remember space width of the normal text font `f0'.
  /SpaceWidth /f0 findfont setfont ( ) stringwidth pop def
  % ---- save the state of the document (useful for ghostscript!)
  /docState save def
  % ---- [andrewi] set PageSize based on chosen dimensions
  UseSetpagedevice {
    0
    {<< /PageSize [PageWidth LandscapePageHeight] >> setpagedevice}
    CheckConfig
  }{
    LandscapeMode {
      % ---- translate to bottom-right corner of Portrait page
      LandscapePageHeight 0 translate
      90 rotate
    }if
  }ifelse
  % ---- [jack] Kludge: my ghostscript window is 21x27.7 instead of 21x29.7
  /JackGhostscript where {pop 1 27.7 29.7 div scale}if
  % ---- N-Up printing
  N-Up 1 gt {
    % ---- landscape
    N-Up-Landscape {
      PageWidth 0 translate
      90 rotate
    }if
    N-Up-Margin dup translate
    % ---- scale
    LandscapeMode{
      /HH PageWidth def
      /WW LandscapePageHeight def
    }{
      /HH LandscapePageHeight def
      /WW PageWidth def
    }ifelse
    WW N-Up-Margin sub N-Up-Margin sub
    N-Up-Landscape
     {N-Up-Lines div HH}{N-Up-Columns N-Up-Missing add div WW}ifelse
    div dup scale
    0 N-Up-Repeat 1 sub LandscapePageHeight mul translate
    % ---- go to start position in page matrix
    N-Up-XStart N-Up-Missing 0.5 mul
    LandscapeMode{
      LandscapePageHeight mul N-Up-YStart add
    }{
      PageWidth mul add N-Up-YStart
    }ifelse
    translate
  }if
  /ColumnWidth PrintWidth InterColumn add def
  % ---- translate to lower left corner of TEXT
  LeftMargin BottomMargin translate
  % ---- define where  printing will start
  /f0 F					% this installs Ascent
  /PrintStartY PrintHeight Ascent sub def
  /ColumnIndex 1 def
  /N-Up-Counter N-Up-End 1 sub def
  SkipFirstPage{save showpage restore}if
}def

/EndDoc {
  % ---- restore the state of the document (useful for ghostscript!)
  docState restore
}def

/BeginDSCPage {
  % ---- when 1st column, save the state of the page
  ColumnIndex 1 eq {
    /pageState save def
  }if
  % ---- save the state of the column
  /columnState save def
}def

/PrintHeaderWidth PrintOnlyOneHeader{PrintPageWidth}{PrintWidth}ifelse def

/BeginPage {
  % ---- when 1st column, print all background effects
  ColumnIndex 1 eq {
    0 PrintStartY moveto		% move to where printing will start
    Zebra {printZebra}if
    printGlobalBackground
    printLocalBackground
  }if
  PrintHeader {
    PrintOnlyOneHeader{ColumnIndex 1 eq}{true}ifelse {
      PrintHeaderFrame {HeaderFrame}if
      HeaderText
    }if
  }if
  0 PrintStartY moveto			% move to where printing will start
  PLN
}def

/EndPage {
  bg {eolbg}if
}def

/EndDSCPage {
  ColumnIndex NumberOfColumns eq {
    % ---- restore the state of the page
    pageState restore
    /ColumnIndex 1 def
    % ---- N-up printing
    N-Up 1 gt {
      N-Up-Counter 0 gt {
	% ---- Next page on same row
	/N-Up-Counter N-Up-Counter 1 sub def
	N-Up-XColumn N-Up-YColumn
      }{
	% ---- Next page on next line
	/N-Up-Counter N-Up-End 1 sub def
	N-Up-XLine N-Up-YLine
      }ifelse
      translate
    }if
  }{ % else
    % ---- restore the state of the current column
    columnState restore
    % ---- and translate to the next column
    ColumnWidth 0 translate
    /ColumnIndex ColumnIndex 1 add def
  }ifelse
}def

% stack: number-of-pages-per-sheet |- --
/BeginSheet {
  /sheetState save def
  /pages-per-sheet exch def
  % ---- N-up printing
  N-Up 1 gt N-Up-Border and pages-per-sheet 0 gt and {
    % ---- page border
    gsave
    0 setgray
    LeftMargin neg BottomMargin neg moveto
    N-Up-Repeat
    {N-Up-End
     {gsave
      PageWidth 0 rlineto
      0 LandscapePageHeight rlineto
      PageWidth neg 0 rlineto
      closepath stroke
      grestore
      /pages-per-sheet pages-per-sheet 1 sub def
      pages-per-sheet 0 le{exit}if
      N-Up-XColumn N-Up-YColumn rmoveto
     }repeat
     pages-per-sheet 0 le{exit}if
     N-Up-XLine N-Up-XColumn sub N-Up-YLine rmoveto
    }repeat
    grestore
  }if
}def

/EndSheet {
  showpage
  sheetState restore
}def

/SetHeaderLines {			% nb-lines --
  /HeaderLines exch def
  % ---- bottom up
  HeaderPad
  HeaderLines 1 sub HeaderLineHeight mul add
  HeaderTitleLineHeight add
  HeaderPad add
  /HeaderHeight exch def
} def

% |---------|
% |  tm     |
% |---------|
% |  header |
% |-+-------| <-- (x y)
% |  ho     |
% |---------|
% |  text   |
% |-+-------| <-- (0 0)
% |  bm     |
% |---------|

/HeaderFrameStart {			% -- x y
  0  PrintHeight HeaderOffset add
} def

/HeaderFramePath {
  PrintHeaderWidth	0			rlineto
  0			HeaderHeight		rlineto
  PrintHeaderWidth neg	0			rlineto
  0			HeaderHeight neg	rlineto
} def

/HeaderFrame {
  gsave
    0.4 setlinewidth
    % ---- fill a black rectangle (the shadow of the next one)
    HeaderFrameStart moveto
    1 -1 rmoveto
    HeaderFramePath
    0 setgray fill
    % ---- do the next rectangle ...
    HeaderFrameStart moveto
    HeaderFramePath
    gsave 0.9 setgray fill grestore	% filled with grey
    gsave 0 setgray stroke grestore	% drawn  with black
  grestore
} def

/HeaderStart {
  HeaderFrameStart
  exch HeaderPad add exch	% horizontal pad
  % ---- bottom up
  HeaderPad add			% vertical   pad
  HeaderDescent sub
  HeaderLineHeight HeaderLines 1 sub mul add
} def

/strcat {
  dup length 3 -1 roll dup length dup 4 -1 roll add string dup
  0 5 -1 roll putinterval
  dup 4 2 roll exch putinterval
} def

/pagenumberstring {
  PageNumber 32 string cvs
  ShowNofN {
    (/) strcat
    PageCount 32 string cvs strcat
  } if
} def

/HeaderText {
  HeaderStart moveto

  HeaderLinesRight HeaderLinesLeft	% -- rightLines leftLines

  % ---- hack: `PN 1 and'  ==  `PN 2 modulo'

  % ---- if even page number and duplex, then exchange left and right
  PageNumber 1 and 0 eq DuplexValue and { exch } if

  { % ---- process the left lines
    aload pop
    exch F
    gsave
      dup xcheck { exec } if
      show
    grestore
    0 HeaderLineHeight neg rmoveto
  } forall

  HeaderStart moveto

  { % ---- process the right lines
    aload pop
    exch F
    gsave
      dup xcheck { exec } if
      dup stringwidth pop
      PrintHeaderWidth exch sub HeaderPad 2 mul sub 0 rmoveto
      show
    grestore
    0 HeaderLineHeight neg rmoveto
  } forall
} def

/ReportFontInfo {
  2 copy
  /t0 3 1 roll DefFont
  /t0 F
  /lh FontHeight def
  /sw ( ) stringwidth pop def
  /aw (01234567890abcdefghijklmnopqrstuvwxyz) dup length exch
  stringwidth pop exch div def
  /t1 12 /Helvetica-Oblique DefFont
  /t1 F
  gsave
    (languagelevel = ) show
    gs_languagelevel 32 string cvs show
  grestore
  0 FontHeight neg rmoveto
  gsave
    (For ) show
    128 string cvs show
    ( ) show
    32 string cvs show
    ( point, the line height is ) show
    lh 32 string cvs show
    (, the space width is ) show
    sw 32 string cvs show
    (,) show
  grestore
  0 FontHeight neg rmoveto
  gsave
    (and a crude estimate of average character width is ) show
    aw 32 string cvs show
    (.) show
  grestore
  0 FontHeight neg rmoveto
} def

/cm { % cm to point
  72 mul 2.54 div
} def

/ReportAllFontInfo {
  FontDirectory
  { % key = font name  value = font dictionary
    pop 10 exch ReportFontInfo
  } forall
} def

% 3 cm 20 cm moveto  10 /Courier ReportFontInfo  showpage
% 3 cm 20 cm moveto  ReportAllFontInfo           showpage

/ErrorMessages
 [(This PostScript printer is not configured with this document page size.)
  (Duplex printing is not supported on this PostScript printer.)]def

% stack: error-index proc |- --
/CheckConfig {
  stopped {
   1 cm LandscapePageHeight 0.5 mul moveto
   /Courier findfont 10 scalefont setfont
   gsave
    (ps-print error:) show
   grestore
   0 -10 rmoveto
   ErrorMessages exch get show
   showpage
   $error /newerror false put
   stop
  }if
} bind def

")

(defconst ps-print-prologue-2
  "
% ---- These lines must be kept together because...

/h0 F
/HeaderTitleLineHeight FontHeight def

/h1 F
/HeaderLineHeight FontHeight def
/HeaderDescent    Descent def

% ---- ...because `F' has a side-effect on `FontHeight' and `Descent'

")

(defconst ps-print-duplex-feature
  "
% --- duplex feature verification
1
UseSetpagedevice {
  {<< /Duplex DuplexValue /Tumble TumbleValue >> setpagedevice}
}{
  {statusdict begin
    DuplexValue setduplexmode TumbleValue settumble
   end}
}ifelse
CheckConfig
")

;; Start Editing Here:

(defvar ps-source-buffer nil)
(defvar ps-spool-buffer-name "*PostScript*")
(defvar ps-spool-buffer nil)

(defvar ps-output-head nil)
(defvar ps-output-tail nil)

(defvar ps-page-postscript 0)
(defvar ps-page-order 0)
(defvar ps-page-count 0)
(defvar ps-showline-count 1)

(defvar ps-control-or-escape-regexp nil)

(defvar ps-background-pages nil)
(defvar ps-background-all-pages nil)
(defvar ps-background-text-count 0)
(defvar ps-background-image-count 0)

(defvar ps-current-font 0)
(defvar ps-default-color (and ps-print-color-p ps-default-fg)) ; black
(defvar ps-current-color ps-default-color)
(defvar ps-current-bg nil)

(defvar ps-razchunk 0)

(defvar ps-color-p nil)
(defvar ps-color-format
  (if (eq ps-print-emacs-type 'emacs)

      ;; Emacs understands the %f format; we'll use it to limit color RGB
      ;; values to three decimals to cut down some on the size of the
      ;; PostScript output.
      "%0.3f %0.3f %0.3f"

    ;; Lucid emacsen will have to make do with %s (princ) for floats.
    "%s %s %s"))

;; These values determine how much print-height to deduct when headers
;; are turned on.  This is a pretty clumsy way of handling it, but
;; it'll do for now.

(defvar ps-header-pad 0
  "Vertical and horizontal space between the header frame and the text.
This is in units of points (1/72 inch).")

;; Define accessors to the dimensions list.

(defmacro ps-page-dimensions-get-width  (dims) `(nth 0 ,dims))
(defmacro ps-page-dimensions-get-height (dims) `(nth 1 ,dims))
(defmacro ps-page-dimensions-get-media  (dims) `(nth 2 ,dims))

(defvar ps-landscape-page-height nil)

(defvar ps-print-width nil)
(defvar ps-print-height nil)

(defvar ps-height-remaining nil)
(defvar ps-width-remaining nil)

(defvar ps-print-color-scale nil)

(defvar ps-font-size-internal nil)
(defvar ps-header-font-size-internal nil)
(defvar ps-header-title-font-size-internal nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables


(defvar ps-print-face-extension-alist nil
  "Alist of symbolic faces *WITH* extension features (box, outline, etc).
An element of this list has the following form:

   (FACE . [BITS FG BG])

   FACE is a symbol denoting a face name
   BITS is a bit vector, where each bit correspond
      to a feature (bold, underline, etc)
      (see documentation for `ps-print-face-map-alist')
   FG foreground color (string or nil)
   BG background color (string or nil)

Don't change this list directly; instead,
use `ps-extend-face' and `ps-extend-face-list'.
See documentation for `ps-extend-face' for valid extension symbol.")


(defvar ps-print-face-alist nil
  "Alist of symbolic faces *WITHOUT* extension features (box, outline, etc).

An element of this list has the same form as an element of
`ps-print-face-extension-alist'.

Don't change this list directly; this list is used by `ps-face-attributes',
`ps-map-face' and `ps-build-reference-face-lists'.")


(defconst ps-print-face-map-alist
  '((bold        . 1)
    (italic      . 2)
    (underline   . 4)
    (strikeout   . 8)
    (overline    . 16)
    (shadow      . 32)
    (box         . 64)
    (outline     . 128))
  "Alist of all features and the corresponding bit mask.
Each symbol correspond to one bit in a bit vector.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remapping Faces


;;;###autoload
(defun ps-extend-face-list (face-extension-list &optional merge-p)
  "Extend face in `ps-print-face-extension-alist'.

If optional MERGE-P is non-nil, extensions in FACE-EXTENSION-LIST are merged
with face extension in `ps-print-face-extension-alist'; otherwise, overrides.

The elements in FACE-EXTENSION-LIST is like those for `ps-extend-face'.

See `ps-extend-face' for documentation."
  (while face-extension-list
    (ps-extend-face (car face-extension-list) merge-p)
    (setq face-extension-list (cdr face-extension-list))))


;;;###autoload
(defun ps-extend-face (face-extension &optional merge-p)
  "Extend face in `ps-print-face-extension-alist'.

If optional MERGE-P is non-nil, extensions in FACE-EXTENSION list are merged
with face extensions in `ps-print-face-extension-alist'; otherwise, overrides.

The elements of FACE-EXTENSION list have the form:

   (FACE-NAME FOREGROUND BACKGROUND EXTENSION...)

FACE-NAME is a face name symbol.

FOREGROUND and BACKGROUND may be nil or a string that denotes the
foreground and background colors respectively.

EXTENSION is one of the following symbols:
   bold      - use bold font.
   italic    - use italic font.
   underline - put a line under text.
   strikeout - like underline, but the line is in middle of text.
   overline  - like underline, but the line is over the text.
   shadow    - text will have a shadow.
   box       - text will be surrounded by a box.
   outline   - print characters as hollow outlines.

If EXTENSION is any other symbol, it is ignored."
  (let* ((face-name  (nth 0 face-extension))
	 (foreground (nth 1 face-extension))
	 (background (nth 2 face-extension))
	 (ps-face (cdr (assq face-name ps-print-face-extension-alist)))
	 (face-vector (or ps-face (vector 0 nil nil)))
	 (face-bit (ps-extension-bit face-extension)))
    ;; extend face
    (aset face-vector 0 (if merge-p
			    (logior (aref face-vector 0) face-bit)
			  face-bit))
    (and foreground (stringp foreground) (aset face-vector 1 foreground))
    (and background (stringp background) (aset face-vector 2 background))
    ;; if face does not exist, insert it
    (or ps-face
	(setq ps-print-face-extension-alist
	      (cons (cons face-name face-vector)
		    ps-print-face-extension-alist)))))


(defun ps-extension-bit (face-extension)
  (let ((face-bit 0))
    ;; map valid symbol extension to bit vector
    (setq face-extension (cdr (cdr face-extension)))
    (while (setq face-extension (cdr face-extension))
      (setq face-bit (logior face-bit
			     (or (cdr (assq (car face-extension)
					    ps-print-face-map-alist))
				 0))))
    face-bit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapted from font-lock: (obsolete stuff)
;; Originally face attributes were specified via `font-lock-face-attributes'.
;; Users then changed the default face attributes by setting that variable.
;; However, we try and be back-compatible and respect its value if set except
;; for faces where M-x customize has been used to save changes for the face.

(defun ps-font-lock-face-attributes ()
  (and (boundp 'font-lock-mode) (symbol-value 'font-lock-mode)
       (boundp 'font-lock-face-attributes)
       (let ((face-attributes font-lock-face-attributes))
	 (while face-attributes
	   (let* ((face-attribute
		   (car (prog1 face-attributes
			  (setq face-attributes (cdr face-attributes)))))
		  (face (car face-attribute)))
	     ;; Rustle up a `defface' SPEC from a
	     ;; `font-lock-face-attributes' entry.
	     (unless (get face 'saved-face)
	       (let ((foreground (nth 1 face-attribute))
		     (background (nth 2 face-attribute))
		     (bold-p (nth 3 face-attribute))
		     (italic-p (nth 4 face-attribute))
		     (underline-p (nth 5 face-attribute))
		     face-spec)
		 (when foreground
		   (setq face-spec (cons ':foreground
					 (cons foreground face-spec))))
		 (when background
		   (setq face-spec (cons ':background
					 (cons background face-spec))))
		 (when bold-p
		   (setq face-spec (append '(:bold t) face-spec)))
		 (when italic-p
		   (setq face-spec (append '(:italic t) face-spec)))
		 (when underline-p
		   (setq face-spec (append '(:underline t) face-spec)))
		 (custom-declare-face face (list (list t face-spec)) nil)
		 )))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions and variables


(make-local-hook 'ps-print-hook)
(make-local-hook 'ps-print-begin-sheet-hook)
(make-local-hook 'ps-print-begin-page-hook)
(make-local-hook 'ps-print-begin-column-hook)


(defun ps-print-without-faces (from to &optional filename region-p)
  (ps-spool-without-faces from to region-p)
  (ps-do-despool filename))


(defun ps-spool-without-faces (from to &optional region-p)
  (run-hooks 'ps-print-hook)
  (ps-printing-region region-p)
  (ps-generate (current-buffer) from to 'ps-generate-postscript))


(defun ps-print-with-faces (from to &optional filename region-p)
  (ps-spool-with-faces from to region-p)
  (ps-do-despool filename))


(defun ps-spool-with-faces (from to &optional region-p)
  (run-hooks 'ps-print-hook)
  (ps-printing-region region-p)
  (ps-generate (current-buffer) from to 'ps-generate-postscript-with-faces))


(defun ps-count-lines (from to)
  (+ (count-lines from to)
     (save-excursion
       (goto-char to)
       (if (= (current-column) 0) 1 0))))


(defvar ps-printing-region nil
  "Variable used to indicate if the region that ps-print is printing.
It is a cons, the car of which is the line number where the region begins, and
its cdr is the total number of lines in the buffer.  Formatting functions can
use this information to print the original line number (and not the number of
lines printed), and to indicate in the header that the printout is of a partial
file.")


(defvar ps-printing-region-p nil
  "Non-nil means ps-print is printing a region.")


(defun ps-printing-region (region-p)
  (setq ps-printing-region-p region-p
	ps-printing-region
	(cons (if region-p
		  (ps-count-lines (point-min) (region-beginning))
		1)
	      (ps-count-lines (point-min) (point-max)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions

(defsubst ps-font-alist (font-sym)
  (get font-sym 'fonts))

(defun ps-font (font-sym font-type)
  "Font family name for text of `font-type', when generating PostScript."
  (let* ((font-list (ps-font-alist font-sym))
	 (normal-font (cdr (assq 'normal font-list))))
    (while (and font-list (not (eq font-type (car (car font-list)))))
      (setq font-list (cdr font-list)))
    (or (cdr (car font-list)) normal-font)))

(defun ps-fonts (font-sym)
  (mapcar 'cdr (ps-font-alist font-sym)))

(defun ps-font-number (font-sym font-type)
  (or (ps-alist-position font-type (ps-font-alist font-sym))
      0))

(defsubst ps-line-height (font-sym)
  "The height of a line, for generating PostScript.
This is the value that ps-print uses to determine the height,
y-dimension, of the lines of text it has printed, and thus affects the
point at which page-breaks are placed.
The line-height is *not* the same as the point size of the font."
  (get font-sym 'line-height))

(defsubst ps-title-line-height (font-sym)
  "The height of a `title' line, for generating PostScript.
This is the value that ps-print uses to determine the height,
y-dimension, of the lines of text it has printed, and thus affects the
point at which page-breaks are placed.
The title-line-height is *not* the same as the point size of the font."
  (get font-sym 'title-line-height))

(defsubst ps-space-width (font-sym)
  "The width of a space character, for generating PostScript.
This value is used in expanding tab characters."
  (get font-sym 'space-width))

(defsubst ps-avg-char-width (font-sym)
  "The average width, in points, of a character, for generating PostScript.
This is the value that ps-print uses to determine the length,
x-dimension, of the text it has printed, and thus affects the point at
which long lines wrap around."
  (get font-sym 'avg-char-width))

(defun ps-line-lengths-internal ()
  "Display the correspondence between a line length and a font size,
using the current ps-print setup.
Try: pr -t file | awk '{printf \"%3d %s\n\", length($0), $0}' | sort -r | head"
  (let ((buf (get-buffer-create "*Line-lengths*"))
	(ifs ps-font-size-internal)	; initial font size
	(icw (ps-avg-char-width 'ps-font-for-text)) ; initial character width
	(print-width (progn (ps-get-page-dimensions)
			    ps-print-width))
	(ps-setup (ps-setup))		; setup for the current buffer
	(fs-min 5)			; minimum font size
	cw-min				; minimum character width
	nb-cpl-max			; maximum nb of characters per line
	(fs-max 14)			; maximum font size
	cw-max				; maximum character width
	nb-cpl-min			; minimum nb of characters per line
	fs				; current font size
	cw				; current character width
	nb-cpl				; current nb of characters per line
	)
    (setq cw-min     (/ (* icw fs-min) ifs)
	  nb-cpl-max (floor (/ print-width cw-min))
	  cw-max     (/ (* icw fs-max) ifs)
	  nb-cpl-min (floor (/ print-width cw-max))
	  nb-cpl     nb-cpl-min)
    (set-buffer buf)
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    (insert ps-setup
	    "nb char per line / font size\n")
    (while (<= nb-cpl nb-cpl-max)
      (setq cw (/ print-width (float nb-cpl))
	    fs (/ (* ifs cw) icw))
      (insert (format "%3s %s\n" nb-cpl fs))
      (setq nb-cpl (1+ nb-cpl)))
    (insert "\n")
    (display-buffer buf 'not-this-window)))

(defun ps-nb-pages (nb-lines)
  "Display correspondence between font size and the number of pages.
The correspondence is based on having NB-LINES lines of text,
and on the current ps-print setup."
  (let ((buf (get-buffer-create "*Nb-Pages*"))
	(ifs ps-font-size-internal)	; initial font size
	(ilh (ps-line-height 'ps-font-for-text)) ; initial line height
	(page-height (progn (ps-get-page-dimensions)
			    ps-print-height))
	(ps-setup (ps-setup))		; setup for the current buffer
	(fs-min 4)			; minimum font size
	lh-min				; minimum line height
	nb-lpp-max			; maximum nb of lines per page
	nb-page-min			; minimum nb of pages
	(fs-max 14)			; maximum font size
	lh-max				; maximum line height
	nb-lpp-min			; minimum nb of lines per page
	nb-page-max			; maximum nb of pages
	fs				; current font size
	lh				; current line height
	nb-lpp				; current nb of lines per page
	nb-page				; current nb of pages
	)
    (setq lh-min      (/ (* ilh fs-min) ifs)
	  nb-lpp-max  (floor (/ page-height lh-min))
	  nb-page-min (ceiling (/ (float nb-lines) nb-lpp-max))
	  lh-max      (/ (* ilh fs-max) ifs)
	  nb-lpp-min  (floor (/ page-height lh-max))
	  nb-page-max (ceiling (/ (float nb-lines) nb-lpp-min))
	  nb-page     nb-page-min)
    (set-buffer buf)
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    (insert ps-setup
	    (format "%d lines\n" nb-lines)
	    "nb page / font size\n")
    (while (<= nb-page nb-page-max)
      (setq nb-lpp (ceiling (/ nb-lines (float nb-page)))
	    lh     (/ page-height nb-lpp)
	    fs     (/ (* ifs lh) ilh))
      (insert (format "%s %s\n" nb-page fs))
      (setq nb-page (1+ nb-page)))
    (insert "\n")
    (display-buffer buf 'not-this-window)))

;; macros used in `ps-select-font'
(defmacro ps-lookup (key) `(cdr (assq ,key font-entry)))
(defmacro ps-size-scale (key) `(/ (* (ps-lookup ,key) font-size) size))

(defun ps-select-font (font-family sym font-size title-font-size)
  (let ((font-entry (cdr (assq font-family ps-font-info-database))))
    (or font-entry
	(error "Don't have data to scale font %s. Known fonts families are %s"
	       font-family
	       (mapcar 'car ps-font-info-database)))
    (let ((size (ps-lookup 'size)))
      (put sym 'fonts (ps-lookup 'fonts))
      (put sym 'space-width (ps-size-scale 'space-width))
      (put sym 'avg-char-width (ps-size-scale 'avg-char-width))
      (put sym 'line-height (ps-size-scale 'line-height))
      (put sym 'title-line-height
	   (/ (* (ps-lookup 'line-height) title-font-size) size)))))

(defun ps-get-page-dimensions ()
  (let ((page-dimensions (cdr (assq ps-paper-type ps-page-dimensions-database)))
	page-width page-height)
    (cond
     ((null page-dimensions)
      (error "`ps-paper-type' must be one of:\n%s"
	     (mapcar 'car ps-page-dimensions-database)))
     ((< ps-number-of-columns 1)
      (error "The number of columns %d should be positive"
	     ps-number-of-columns)))

    (ps-select-font ps-font-family 'ps-font-for-text
		    ps-font-size-internal ps-font-size-internal)
    (ps-select-font ps-header-font-family 'ps-font-for-header
		    ps-header-font-size-internal
		    ps-header-title-font-size-internal)

    (setq page-width  (ps-page-dimensions-get-width  page-dimensions)
	  page-height (ps-page-dimensions-get-height page-dimensions))

    ;; Landscape mode
    (if ps-landscape-mode
	;; exchange width and height
	(setq page-width (prog1 page-height (setq page-height page-width))))

    ;; It is used to get the lower right corner (only in landscape mode)
    (setq ps-landscape-page-height page-height)

    ;; | lm | text | ic | text | ic | text | rm |
    ;; page-width == lm  +  n * pw  +  (n - 1) * ic  +  rm
    ;; => pw == (page-width - lm -rm - (n - 1) * ic) / n
    (setq ps-print-width (/ (- page-width
			       ps-left-margin ps-right-margin
			       (* (1- ps-number-of-columns) ps-inter-column))
			    ps-number-of-columns))
    (if (<= ps-print-width 0)
	(error "Bad horizontal layout:
page-width           == %s
ps-left-margin       == %s
ps-right-margin      == %s
ps-inter-column      == %s
ps-number-of-columns == %s
| lm | text | ic | text | ic | text | rm |
page-width == lm  +  n * print-width  +  (n - 1) * ic  +  rm
=> print-width == %d !"
	       page-width
	       ps-left-margin
	       ps-right-margin
	       ps-inter-column
	       ps-number-of-columns
	       ps-print-width))

    (setq ps-print-height
	  (- page-height ps-bottom-margin ps-top-margin))
    (if (<= ps-print-height 0)
	(error "Bad vertical layout:
ps-top-margin    == %s
ps-bottom-margin == %s
page-height == bm + print-height + tm
=> print-height == %d !"
	       ps-top-margin
	       ps-bottom-margin
	       ps-print-height))
    ;; If headers are turned on, deduct the height of the header from
    ;; the print height.
    (if ps-print-header
	(setq ps-header-pad   (* ps-header-line-pad
				 (ps-title-line-height 'ps-font-for-header))
	      ps-print-height (- ps-print-height
				 ps-header-offset
				 ps-header-pad
				 (ps-title-line-height 'ps-font-for-header)
				 (* (ps-line-height 'ps-font-for-header)
				    (1- ps-header-lines))
				 ps-header-pad)))
    (if (<= ps-print-height 0)
	(error "Bad vertical layout:
ps-top-margin    == %s
ps-bottom-margin == %s
ps-header-offset == %s
ps-header-pad    == %s
header-height    == %s
page-height == bm + print-height + tm - ho - hh
=> print-height == %d !"
	       ps-top-margin
	       ps-bottom-margin
	       ps-header-offset
	       ps-header-pad
	       (+ ps-header-pad
		  (ps-title-line-height 'ps-font-for-header)
		  (* (ps-line-height 'ps-font-for-header)
		     (1- ps-header-lines))
		  ps-header-pad)
	       ps-print-height))))

(defun ps-print-preprint (&optional filename)
  (and filename
       (or (numberp filename)
	   (listp filename))
       (let* ((name   (concat (file-name-nondirectory (or (buffer-file-name)
							  (buffer-name)))
			      ".ps"))
	      (prompt (format "Save PostScript to file: (default %s) " name))
	      (res    (read-file-name prompt default-directory name nil)))
	 (while (cond ((not (file-writable-p res))
		       (ding)
		       (setq prompt "is unwritable"))
		      ((file-exists-p res)
		       (setq prompt "exists")
		       (not (y-or-n-p (format "File `%s' exists; overwrite? "
					      res))))
		      (t nil))
	   (setq res (read-file-name
		      (format "File %s; save PostScript to file: " prompt)
		      (file-name-directory res) nil nil
		      (file-name-nondirectory res))))
	 (if (file-directory-p res)
	     (expand-file-name name (file-name-as-directory res))
	   res))))

;; The following functions implement a simple list-buffering scheme so
;; that ps-print doesn't have to repeatedly switch between buffers
;; while spooling.  The functions `ps-output' and `ps-output-string' build
;; up the lists; the function `ps-flush-output' takes the lists and
;; insert its contents into the spool buffer (*PostScript*).

(defvar ps-string-escape-codes
  (let ((table (make-vector 256 nil))
	(char ?\000))
    ;; control characters
    (while (<= char ?\037)
      (aset table char (format "\\%03o" char))
      (setq char (1+ char)))
    ;; printable characters
    (while (< char ?\177)
      (aset table char (format "%c" char))
      (setq char (1+ char)))
    ;; DEL and 8-bit characters
    (while (<= char ?\377)
      (aset table char (format "\\%o" char))
      (setq char (1+ char)))
    ;; Override ASCII formatting characters with named escape code:
    (aset table ?\n "\\n")		; [NL] linefeed
    (aset table ?\r "\\r")		; [CR] carriage return
    (aset table ?\t "\\t")		; [HT] horizontal tab
    (aset table ?\b "\\b")		; [BS] backspace
    (aset table ?\f "\\f")		; [NP] form feed
    ;; Escape PostScript escape and string delimiter characters:
    (aset table ?\\ "\\\\")
    (aset table ?\( "\\(")
    (aset table ?\) "\\)")
    table)
  "Vector used to map characters to PostScript string escape codes.")

(defun ps-output-string-prim (string)
  (insert "(")				;insert start-string delimiter
  (save-excursion			;insert string
    (insert (string-as-unibyte string)))
  ;; Find and quote special characters as necessary for PS
  ;; This skips everything except control chars, non-ASCII chars, (, ) and \.
  (while (progn (skip-chars-forward " -'*-[]-~") (not (eobp)))
    (let ((special (following-char)))
      (delete-char 1)
      (insert (aref ps-string-escape-codes special))))
  (goto-char (point-max))
  (insert ")"))				;insert end-string delimiter

(defun ps-init-output-queue ()
  (setq ps-output-head '("")
	ps-output-tail ps-output-head))

(defun ps-output (&rest args)
  (setcdr ps-output-tail args)
  (while (cdr ps-output-tail)
    (setq ps-output-tail (cdr ps-output-tail))))

(defun ps-output-string (string)
  (ps-output t string))

(defun ps-output-list (the-list)
  (mapcar 'ps-output the-list))

;; Output strings in the list ARGS in the PostScript prologue part.
(defun ps-output-prologue (args)
  (ps-output 'prologue (if (stringp args) (list args) args)))

(defun ps-flush-output ()
  (save-excursion
    (set-buffer ps-spool-buffer)
    (goto-char (point-max))
    (while ps-output-head
      (let ((it (car ps-output-head)))
	(cond
	 ((eq t it)
	  (setq ps-output-head (cdr ps-output-head))
	  (ps-output-string-prim (car ps-output-head)))
	 ((eq 'prologue it)
	  (setq ps-output-head (cdr ps-output-head))
	  (save-excursion
	    (search-backward "\nBeginDoc")
	    (forward-char 1)
	    (apply 'insert (car ps-output-head))))
	 (t
	  (insert it))))
      (setq ps-output-head (cdr ps-output-head))))
  (ps-init-output-queue))

(defun ps-insert-file (fname)
  (ps-flush-output)
  ;; Check to see that the file exists and is readable; if not, throw
  ;; an error.
  (or (file-readable-p fname)
      (error "Could not read file `%s'" fname))
  (save-excursion
    (set-buffer ps-spool-buffer)
    (goto-char (point-max))
    (insert-file fname)))

;; These functions insert the arrays that define the contents of the
;; headers.

(defun ps-generate-header-line (fonttag &optional content)
  (ps-output "  [ " fonttag " ")
  (cond
   ;; Literal strings should be output as is -- the string must
   ;; contain its own PS string delimiters, '(' and ')', if necessary.
   ((stringp content)
    (ps-output content))

   ;; Functions are called -- they should return strings; they will be
   ;; inserted as strings and the PS string delimiters added.
   ((and (symbolp content) (fboundp content))
    (ps-output-string (funcall content)))

   ;; Variables will have their contents inserted.  They should
   ;; contain strings, and will be inserted as strings.
   ((and (symbolp content) (boundp content))
    (ps-output-string (symbol-value content)))

   ;; Anything else will get turned into an empty string.
   (t
    (ps-output-string "")))
  (ps-output " ]\n"))

(defun ps-generate-header (name contents)
  (ps-output "/" name " [\n")
  (if (> ps-header-lines 0)
      (let ((count 1))
	(ps-generate-header-line "/h0" (car contents))
	(while (and (< count ps-header-lines)
		    (setq contents (cdr contents)))
	  (ps-generate-header-line "/h1" (car contents))
	  (setq count (1+ count)))
	(ps-output "] def\n"))))


(defun ps-output-boolean (name bool &optional no-def)
  (ps-output (format "/%s %s%s"
		     name (if bool "true" "false") (if no-def "\n" " def\n"))))


(defun ps-background-pages (page-list func)
  (if page-list
      (mapcar
       #'(lambda (pages)
	   (let ((start (if (consp pages) (car pages) pages))
		 (end   (if (consp pages) (cdr pages) pages)))
	     (and (integerp start) (integerp end) (<= start end)
		  (add-to-list 'ps-background-pages (vector start end func)))))
       page-list)
    (setq ps-background-all-pages (cons func ps-background-all-pages))))


(defconst ps-boundingbox-re
  "^%%BoundingBox:\
\\s-+\\([0-9.]+\\)\\s-+\\([0-9.]+\\)\\s-+\\([0-9.]+\\)\\s-+\\([0-9.]+\\)")


(defun ps-get-boundingbox ()
  (save-excursion
    (set-buffer ps-spool-buffer)
    (save-excursion
      (if (re-search-forward ps-boundingbox-re nil t)
	  (vector (string-to-number	; lower x
		   (buffer-substring (match-beginning 1) (match-end 1)))
		  (string-to-number	; lower y
		   (buffer-substring (match-beginning 2) (match-end 2)))
		  (string-to-number	; upper x
		   (buffer-substring (match-beginning 3) (match-end 3)))
		  (string-to-number	; upper y
		   (buffer-substring (match-beginning 4) (match-end 4))))
	(vector 0 0 0 0)))))


;; Emacs understands the %f format; we'll use it to limit color RGB values
;; to three decimals to cut down some on the size of the PostScript output.
;; Lucid emacsen will have to make do with %s (princ) for floats.

(defvar ps-float-format (if (eq ps-print-emacs-type 'emacs)
			    "%0.3f "	; emacs
			  "%s "))	; Lucid emacsen


(defun ps-float-format (value &optional default)
  (let ((literal (or value default)))
    (if literal
	(format (if (numberp literal)
		    ps-float-format
		  "%s ")
		literal)
      " ")))


(defun ps-background-text ()
  (mapcar
   #'(lambda (text)
       (setq ps-background-text-count (1+ ps-background-text-count))
       (ps-output (format "/ShowBackText-%d {\n" ps-background-text-count))
       (ps-output-string (nth 0 text))	; text
       (ps-output
	"\n"
	(ps-float-format (nth 4 text) 200.0) ; font size
	(format "/%s " (or (nth 3 text) "Times-Roman")) ; font name
	(ps-float-format (nth 6 text)
			 "PrintHeight PrintPageWidth atan") ; rotation
	(ps-float-format (nth 5 text) 0.85) ; gray
	(ps-float-format (nth 1 text) "0") ; x position
	(ps-float-format (nth 2 text) "BottomMargin") ; y position
	"\nShowBackText} def\n")
       (ps-background-pages (nthcdr 7 text) ; page list
			    (format "ShowBackText-%d\n"
				    ps-background-text-count)))
   ps-print-background-text))


(defun ps-background-image ()
  (mapcar
   #'(lambda (image)
       (let ((image-file (expand-file-name (nth 0 image))))
	 (if (file-readable-p image-file)
	     (progn
	       (setq ps-background-image-count (1+ ps-background-image-count))
	       (ps-output
		(format "/ShowBackImage-%d {\n--back-- "
			ps-background-image-count)
		(ps-float-format (nth 5 image) 0.0) ; rotation
		(ps-float-format (nth 3 image) 1.0) ; x scale
		(ps-float-format (nth 4 image) 1.0) ; y scale
		(ps-float-format (nth 1 image) ; x position
				 "PrintPageWidth 2 div")
		(ps-float-format (nth 2 image) ; y position
				 "PrintHeight 2 div BottomMargin add")
		"\nBeginBackImage\n")
	       (ps-insert-file image-file)
	       ;; coordinate adjustment to centralize image
	       ;; around x and y position
	       (let ((box (ps-get-boundingbox)))
		 (save-excursion
		   (set-buffer ps-spool-buffer)
		   (save-excursion
		     (if (re-search-backward "^--back--" nil t)
			 (replace-match
			  (format "%s %s"
				  (ps-float-format
				   (- (+ (/ (- (aref box 2) (aref box 0)) 2.0)
					 (aref box 0))))
				  (ps-float-format
				   (- (+ (/ (- (aref box 3) (aref box 1)) 2.0)
					 (aref box 1)))))
			  t)))))
	       (ps-output "\nEndBackImage} def\n")
	       (ps-background-pages (nthcdr 6 image) ; page list
				    (format "ShowBackImage-%d\n"
					    ps-background-image-count))))))
   ps-print-background-image))


(defun ps-background (page-number)
  (let (has-local-background)
    (mapcar #'(lambda (range)
		(and (<= (aref range 0) page-number)
		     (<= page-number (aref range 1))
		     (if has-local-background
			 (ps-output (aref range 2))
		       (setq has-local-background t)
		       (ps-output "/printLocalBackground {\n"
				  (aref range 2)))))
	    ps-background-pages)
    (and has-local-background (ps-output "} def\n"))))


;; Return a list of the distinct elements of LIST.
;; Elements are compared with `equal'.
(defun ps-remove-duplicates (list)
  (let (new (tail list))
    (while tail
      (or (member (car tail) new)
	  (setq new (cons (car tail) new)))
      (setq tail (cdr tail)))
    (nreverse new)))


;; Find the first occurrence of ITEM in LIST.
;; Return the index of the matching item, or nil if not found.
;; Elements are compared with `eq'.
(defun ps-alist-position (item list)
  (let ((tail list) (index 0) found)
    (while tail
      (if (setq found (eq (car (car tail)) item))
	  (setq tail nil)
	(setq index (1+ index)
	      tail (cdr tail))))
    (and found index)))


(defconst ps-n-up-database
  '((a4
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  t   3  4  2)
     (16  nil 4  4  0)
     (18  t   3  6  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (32  t   4  8  0)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (50  t   5  10 0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (a3
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (18  t   3  6  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (32  t   4  8  0)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (50  t   5  10 0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (letter
     (1   nil 1  1  0)
     (4   nil 2  2  0)
     (6   t   2  3  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (40  t   5  8  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (legal
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   nil 3  2  1)
     (9   nil 3  3  0)
     (10  t   2  5  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (70  t   5  14 0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (letter-small
     (1   nil 1  1  0)
     (4   nil 2  2  0)
     (6   t   2  3  0)
     (9   nil 3  3  0)
     (12  t   3  4  1)
     (15  t   3  5  0)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (28  t   4  7  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (40  t   5  8  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (60  t   6  10 0)
     (64  nil 8  8  0)
     (72  ni  9  8  1)
     (81  nil 9  9  0)
     (84  t   7  12 0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (tabloid
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (84  t   6  14 0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    ;; Ledger paper size is a special case, it is the only paper size where the
    ;; normal size is landscaped, that is, the height is smaller than width.
    ;; So, we use the special value `pag' in the `landscape' field.
    (ledger
     (1   nil 1  1  0)
     (2   pag 1  2  0)
     (4   nil 2  2  0)
     (6   pag 2  3  1)
     (8   pag 2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (84  pag 6  14 0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (statement
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   nil 3  2  1)
     (9   nil 3  3  0)
     (10  t   2  5  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (21  t   3  7  0)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (40  t   4  10 0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (60  t   5  12 0)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (executive
     (1   nil 1  1  0)
     (4   nil 2  2  0)
     (6   t   2  3  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (28  t   4  7  0)
     (30  nil 6  5  1)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (45  t   5  9  0)
     (49  nil 7  7  0)
     (56  nil 8  7  1)
     (60  t   6  10 0)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (84  t   7  12 0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (a4small
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (18  t   3  6  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (32  t   4  8  0)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (50  t   5  10 0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (78  t   6  13 0)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (b4
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (18  t   3  6  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (32  t   4  8  0)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (50  t   5  10 0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  1)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (100 nil 10 10 0))
    (b5
     (1   nil 1  1  0)
     (2   t   1  2  0)
     (4   nil 2  2  0)
     (6   t   2  3  1)
     (8   t   2  4  0)
     (9   nil 3  3  0)
     (12  nil 4  3  1)
     (16  nil 4  4  0)
     (18  t   3  6  0)
     (20  nil 5  4  1)
     (25  nil 5  5  0)
     (30  nil 6  5  1)
     (32  t   4  8  0)
     (36  nil 6  6  0)
     (42  nil 7  6  1)
     (49  nil 7  7  0)
     (50  t   5  10 0)
     (56  nil 8  7  1)
     (64  nil 8  8  0)
     (72  nil 9  8  0)
     (81  nil 9  9  0)
     (90  nil 10 9  1)
     (98  t   7  14 0)
     (100 nil 10 10 0)))
  "Alist which is the page matrix database used for N-up printing.

Each element has the following form:

   (PAGE
    (MAX LANDSCAPE LINES COLUMNS COL-MISSING)
    ...)

Where:
PAGE is the page size used (see `ps-paper-type').
MAX is the maximum elements of this page matrix.
LANDSCAPE specifies if page matrix is landscaped, has the following valid
	  values:
	    nil   the sheet is in portrait mode.
	    t     the sheet is in landscape mode.
	    pag   the sheet is in portrait mode and page is in landscape mode.
LINES is the number of lines of page matrix.
COLUMNS is the number of columns of page matrix.
COL-MISSING is the number of columns missing to fill the sheet.")


(defmacro ps-n-up-landscape (mat) `(nth 1 ,mat))
(defmacro ps-n-up-lines     (mat) `(nth 2 ,mat))
(defmacro ps-n-up-columns   (mat) `(nth 3 ,mat))
(defmacro ps-n-up-missing   (mat) `(nth 4 ,mat))


(defun ps-n-up-printing ()
  ;; force `ps-n-up-printing' be in range 1 to 100.
  (setq ps-n-up-printing (max (min ps-n-up-printing 100) 1))
  ;; find suitable page matrix for a given `ps-paper-type'.
  (let ((the-list (cdr (assq ps-paper-type ps-n-up-database))))
    (and the-list
	 (while (> ps-n-up-printing (caar the-list))
	   (setq the-list (cdr the-list))))
    (car the-list)))


(defconst ps-n-up-filling-database
  '((left-top
     "PageWidth"			; N-Up-XColumn
     "0"				; N-Up-YColumn
     "N-Up-End 1 sub PageWidth mul neg" ; N-Up-XLine
     "LandscapePageHeight neg"		; N-Up-YLine
     "N-Up-Lines"			; N-Up-Repeat
     "N-Up-Columns"			; N-Up-End
     "0"				; N-Up-XStart
     "0")				; N-Up-YStart
    (left-bottom
     "PageWidth"			; N-Up-XColumn
     "0"				; N-Up-YColumn
     "N-Up-End 1 sub PageWidth mul neg" ; N-Up-XLine
     "LandscapePageHeight"		; N-Up-YLine
     "N-Up-Lines"			; N-Up-Repeat
     "N-Up-Columns"			; N-Up-End
     "0"				; N-Up-XStart
     "N-Up-Repeat 1 sub LandscapePageHeight mul neg") ; N-Up-YStart
    (right-top
     "PageWidth neg"			; N-Up-XColumn
     "0"				; N-Up-YColumn
     "N-Up-End 1 sub PageWidth mul"	; N-Up-XLine
     "LandscapePageHeight neg"		; N-Up-YLine
     "N-Up-Lines"			; N-Up-Repeat
     "N-Up-Columns"			; N-Up-End
     "N-Up-End 1 sub PageWidth mul"	; N-Up-XStart
     "0")				; N-Up-YStart
    (right-bottom
     "PageWidth neg"			; N-Up-XColumn
     "0"				; N-Up-YColumn
     "N-Up-End 1 sub PageWidth mul"	; N-Up-XLine
     "LandscapePageHeight"		; N-Up-YLine
     "N-Up-Lines"			; N-Up-Repeat
     "N-Up-Columns"			; N-Up-End
     "N-Up-End 1 sub PageWidth mul"	; N-Up-XStart
     "N-Up-Repeat 1 sub LandscapePageHeight mul neg") ; N-Up-YStart
    (top-left
     "0"				; N-Up-XColumn
     "LandscapePageHeight neg"		; N-Up-YColumn
     "PageWidth"			; N-Up-XLine
     "N-Up-End 1 sub LandscapePageHeight mul" ; N-Up-YLine
     "N-Up-Columns"			; N-Up-Repeat
     "N-Up-Lines"			; N-Up-End
     "0"				; N-Up-XStart
     "0")				; N-Up-YStart
    (bottom-left
     "0"				; N-Up-XColumn
     "LandscapePageHeight"		; N-Up-YColumn
     "PageWidth"			; N-Up-XLine
     "N-Up-End 1 sub LandscapePageHeight mul neg" ; N-Up-YLine
     "N-Up-Columns"			; N-Up-Repeat
     "N-Up-Lines"			; N-Up-End
     "0"				; N-Up-XStart
     "N-Up-End 1 sub LandscapePageHeight mul neg") ; N-Up-YStart
    (top-right
     "0"				; N-Up-XColumn
     "LandscapePageHeight neg"		; N-Up-YColumn
     "PageWidth neg"			; N-Up-XLine
     "N-Up-End 1 sub LandscapePageHeight mul" ; N-Up-YLine
     "N-Up-Columns"			; N-Up-Repeat
     "N-Up-Lines"			; N-Up-End
     "N-Up-Repeat 1 sub PageWidth mul"	; N-Up-XStart
     "0")				; N-Up-YStart
    (bottom-right
     "0"				; N-Up-XColumn
     "LandscapePageHeight"		; N-Up-YColumn
     "PageWidth neg"			; N-Up-XLine
     "N-Up-End 1 sub LandscapePageHeight mul neg" ; N-Up-YLine
     "N-Up-Columns"			; N-Up-Repeat
     "N-Up-Lines"			; N-Up-End
     "N-Up-Repeat 1 sub PageWidth mul"	; N-Up-XStart
     "N-Up-End 1 sub LandscapePageHeight mul neg")) ; N-Up-YStart
  "Alist for n-up printing initializations.

Each element has the following form:

   (KIND XCOL YCOL XLIN YLIN REPEAT END XSTART YSTART)

Where:
KIND is a valid value of `ps-n-up-filling'.
XCOL YCOL are the relative position for the next column.
XLIN YLIN are the relative position for the beginning of next line.
REPEAT is the number of repetions for external loop.
END is the number of repetions for internal loop and also the number of pages in
    a row.
XSTART YSTART are the relative position for the first page in a sheet.")


(defun ps-n-up-filling ()
  (cdr (or (assq ps-n-up-filling ps-n-up-filling-database)
	   (assq 'left-top ps-n-up-filling-database))))


(defmacro ps-n-up-xcolumn (init) `(nth 0 ,init))
(defmacro ps-n-up-ycolumn (init) `(nth 1 ,init))
(defmacro ps-n-up-xline	  (init) `(nth 2 ,init))
(defmacro ps-n-up-yline	  (init) `(nth 3 ,init))
(defmacro ps-n-up-repeat  (init) `(nth 4 ,init))
(defmacro ps-n-up-end	  (init) `(nth 5 ,init))
(defmacro ps-n-up-xstart  (init) `(nth 6 ,init))
(defmacro ps-n-up-ystart  (init) `(nth 7 ,init))


(defun ps-begin-file ()
  (ps-get-page-dimensions)
  (setq ps-page-postscript 0
	ps-page-order 0
	ps-background-text-count 0
	ps-background-image-count 0
	ps-background-pages nil
	ps-background-all-pages nil)

  (let ((dimensions (cdr (assq ps-paper-type ps-page-dimensions-database)))
	(tumble (if ps-landscape-mode (not ps-spool-tumble) ps-spool-tumble))
	(n-up (ps-n-up-printing))
	(n-up-filling (ps-n-up-filling)))
    (and (> ps-n-up-printing 1) (setq tumble (not tumble)))
    (ps-output
     ps-adobe-tag
     "%%Title: " (buffer-name)		; Take job name from name of
					; first buffer printed
     "\n%%Creator: " (user-full-name)
     " (using ps-print v" ps-print-version
     ")\n%%CreationDate: "
     (time-stamp-hh:mm:ss) " " (time-stamp-mon-dd-yyyy)
     "\n%%Orientation: "
     (if ps-landscape-mode "Landscape" "Portrait")
     "\n%%DocumentNeededResources: font Times-Roman Times-Italic\n%%+ font "
     (mapconcat 'identity
		(ps-remove-duplicates
		 (append (ps-fonts 'ps-font-for-text)
			 (list (ps-font 'ps-font-for-header 'normal)
			       (ps-font 'ps-font-for-header 'bold))))
		"\n%%+ font ")
     "\n%%DocumentMedia: " (ps-page-dimensions-get-media dimensions)
     (format " %d" (round (ps-page-dimensions-get-width dimensions)))
     (format " %d" (round (ps-page-dimensions-get-height dimensions)))
     " 0 () ()\n%%PageOrder: Ascend\n%%Pages: (atend)\n%%Requirements:"
     (if ps-spool-duplex
	 (format " duplex%s" (if tumble "(tumble)\n" "\n"))
       "\n"))

    (let ((comments (if (functionp ps-print-prologue-header)
			(funcall ps-print-prologue-header)
		      ps-print-prologue-header)))
      (and (stringp comments)
	   (ps-output comments)))

    (ps-output "%%EndComments\n\n%%BeginPrologue\n\n"
	       "/gs_languagelevel /languagelevel where {pop languagelevel}{1}ifelse def\n\n")

    (ps-output-boolean "SkipFirstPage      " ps-banner-page-when-duplexing)
    (ps-output-boolean "LandscapeMode      "
		       (or ps-landscape-mode
			   (eq (ps-n-up-landscape n-up) 'pag)))
    (ps-output (format "/NumberOfColumns     %d def\n" ps-number-of-columns)

	       (format "/LandscapePageHeight %s def\n" ps-landscape-page-height)
	       (format "/PrintPageWidth      %s def\n"
		       (- (* (+ ps-print-width ps-inter-column)
			     ps-number-of-columns)
			  ps-inter-column))
	       (format "/PrintWidth   %s def\n" ps-print-width)
	       (format "/PrintHeight  %s def\n" ps-print-height)

	       (format "/LeftMargin   %s def\n" ps-left-margin)
	       (format "/RightMargin  %s def\n" ps-right-margin)
	       (format "/InterColumn  %s def\n" ps-inter-column)

	       (format "/BottomMargin %s def\n" ps-bottom-margin)
	       (format "/TopMargin    %s def\n" ps-top-margin) ; not used
	       (format "/HeaderOffset %s def\n" ps-header-offset)
	       (format "/HeaderPad    %s def\n" ps-header-pad))

    (ps-output-boolean "PrintHeader       " ps-print-header)
    (ps-output-boolean "PrintOnlyOneHeader" ps-print-only-one-header)
    (ps-output-boolean "PrintHeaderFrame  " ps-print-header-frame)
    (ps-output-boolean "ShowNofN          " ps-show-n-of-n)
    (ps-output-boolean "DuplexValue       " ps-spool-duplex)
    (ps-output-boolean "TumbleValue       " tumble)

    (let ((line-height (ps-line-height 'ps-font-for-text)))
      (ps-output (format "/LineHeight     %s def\n" line-height)
		 (format "/LinesPerColumn %d def\n"
			 (round (/ (+ ps-print-height
				      (* line-height 0.45))
				   line-height)))))

    (ps-output-boolean "Zebra           " ps-zebra-stripes)
    (ps-output-boolean "PrintLineNumber " ps-line-number)
    (ps-output (format "/ZebraHeight      %d def\n" ps-zebra-stripe-height)
	       (format "/ZebraGray        %s def\n" ps-zebra-gray)
	       "/UseSetpagedevice "
	       (if (eq ps-spool-config 'setpagedevice)
		   "/setpagedevice where {pop true}{false}ifelse def\n"
		 "false def\n")
	       "\n/PageWidth "
	       "PrintPageWidth LeftMargin add RightMargin add def\n\n"
	       (format "/N-Up           %d def\n" ps-n-up-printing))
    (ps-output-boolean "N-Up-Landscape" (eq (ps-n-up-landscape n-up) t))
    (ps-output-boolean "N-Up-Border   " ps-n-up-border-p)
    (ps-output (format "/N-Up-Lines     %d def\n" (ps-n-up-lines n-up))
	       (format "/N-Up-Columns   %d def\n" (ps-n-up-columns n-up))
	       (format "/N-Up-Missing   %d def\n" (ps-n-up-missing n-up))
	       (format "/N-Up-Margin    %s" ps-n-up-margin)
	       " def\n/N-Up-Repeat    "
	       (if ps-landscape-mode
		   (ps-n-up-end     n-up-filling)
		 (ps-n-up-repeat  n-up-filling))
	       " def\n/N-Up-End       "
	       (if ps-landscape-mode
		   (ps-n-up-repeat  n-up-filling)
		 (ps-n-up-end     n-up-filling))
	       " def\n/N-Up-XColumn   " (ps-n-up-xcolumn n-up-filling)
	       " def\n/N-Up-YColumn   " (ps-n-up-ycolumn n-up-filling)
	       " def\n/N-Up-XLine     " (ps-n-up-xline   n-up-filling)
	       " def\n/N-Up-YLine     " (ps-n-up-yline   n-up-filling)
	       " def\n/N-Up-XStart    " (ps-n-up-xstart  n-up-filling)
	       " def\n/N-Up-YStart    " (ps-n-up-ystart  n-up-filling) " def\n")

    (ps-background-text)
    (ps-background-image)
    (setq ps-background-all-pages (nreverse ps-background-all-pages)
	  ps-background-pages (nreverse ps-background-pages))

    (ps-output ps-print-prologue-1)

    (ps-output "/printGlobalBackground {\n")
    (ps-output-list ps-background-all-pages)
    (ps-output "} def\n/printLocalBackground {\n} def\n")

    ;; Header fonts
    (ps-output (format "/h0 %s (%s) cvn DefFont\n" ; /h0 14 /Helvetica-Bold DefFont
		       ps-header-title-font-size-internal
		       (ps-font 'ps-font-for-header 'bold))
	       (format "/h1 %s (%s) cvn DefFont\n" ; /h1 12 /Helvetica DefFont
		       ps-header-font-size-internal
		       (ps-font 'ps-font-for-header 'normal)))

    (ps-output ps-print-prologue-2)

    ;; Text fonts
    (let ((font (ps-font-alist 'ps-font-for-text))
	  (i 0))
      (while font
	(ps-output (format "/f%d %s (%s) cvn DefFont\n"
			   i
			   ps-font-size-internal
			   (ps-font 'ps-font-for-text (car (car font)))))
	(setq font (cdr font)
	      i (1+ i))))

    (let ((font-entry (cdr (assq ps-font-family ps-font-info-database))))
      (ps-output (format "/SpaceWidthRatio %f def\n"
			 (/ (ps-lookup 'space-width) (ps-lookup 'size)))))

    (ps-output "\n%%EndPrologue\n\n%%BeginSetup\n")
    (unless (eq ps-spool-config 'lpr-switches)
      (ps-output "\n%%BeginFeature: *Duplex "
		 (ps-boolean-capitalized ps-spool-duplex)
		 " *Tumble "
		 (ps-boolean-capitalized tumble)
		 ps-print-duplex-feature
		 "%%EndFeature\n")))
  (ps-output "\n/Lines 0 def\n/PageCount 0 def\n\nBeginDoc\n%%EndSetup\n"))


(defun ps-boolean-capitalized (bool)
  (if bool "True" "False"))


(defun ps-header-dirpart ()
  (let ((fname (buffer-file-name)))
    (if fname
	(if (string-equal (buffer-name) (file-name-nondirectory fname))
	    (file-name-directory fname)
	  fname)
      "")))


(defun ps-get-buffer-name ()
  (cond
   ;; Indulge Jim this little easter egg:
   ((string= (buffer-name) "ps-print.el")
    "Hey, Cool!  It's ps-print.el!!!")
   ;; Indulge Jack this other little easter egg:
   ((string= (buffer-name) "sokoban.el")
    "Super! C'est sokoban.el!")
   (t (concat
       (and ps-printing-region-p "Subset of: ")
       (buffer-name)
       (and (buffer-modified-p) " (unsaved)")))))


(defun ps-get-font-size (font-sym)
  (let ((font-size (symbol-value font-sym)))
    (cond ((numberp font-size)
	   font-size)
	  ((and (consp font-size)
		(numberp (car font-size))
		(numberp (cdr font-size)))
	   (if ps-landscape-mode
	       (car font-size)
	     (cdr font-size)))
	  (t
	   (error "Invalid font size `%S' for `%S'" font-size font-sym)))))


(defun ps-begin-job ()
  (save-excursion
    (set-buffer ps-spool-buffer)
    (goto-char (point-max))
    (and (re-search-backward "^%%Trailer$" nil t)
	 (delete-region (match-beginning 0) (point-max))))
  (setq ps-showline-count (car ps-printing-region)
	ps-page-count 0
	ps-font-size-internal        (ps-get-font-size 'ps-font-size)
	ps-header-font-size-internal (ps-get-font-size 'ps-header-font-size)
	ps-header-title-font-size-internal
	(ps-get-font-size 'ps-header-title-font-size)
	ps-control-or-escape-regexp
	(cond ((eq ps-print-control-characters '8-bit)
	       (string-as-unibyte "[\000-\037\177-\377]"))
	      ((eq ps-print-control-characters 'control-8-bit)
	       (string-as-unibyte "[\000-\037\177-\237]"))
	      ((eq ps-print-control-characters 'control)
	       "[\000-\037\177]")
	      (t "[\t\n\f]"))))

(defmacro ps-page-number ()
  `(1+ (/ (1- ps-page-count) ps-number-of-columns)))

(defun ps-end-file ()
  ;; Back to the PS output buffer to set the last page n-up printing
  (save-excursion
    (let ((pages-per-sheet (mod ps-page-postscript ps-n-up-printing))
	  case-fold-search)
      (set-buffer ps-spool-buffer)
      (goto-char (point-max))
      (and (> pages-per-sheet 0)
	   (re-search-backward "^[0-9]+ BeginSheet$" nil t)
	   (replace-match (format "%d BeginSheet" pages-per-sheet) t))))
  ;; Set dummy page
  (and ps-spool-duplex (= (mod ps-page-order 2) 1)
       (ps-dummy-page))
  ;; Set end of PostScript file
  (ps-output "EndSheet\n\n%%Trailer\n%%Pages: "
	     (format "%d" ps-page-order)
	     "\n\nEndDoc\n\n%%EOF\n"))


(defun ps-next-page ()
  (ps-end-page)
  (ps-flush-output)
  (ps-begin-page))


(defun ps-header-sheet ()
  ;; Print only when a new sheet begins.
  (setq ps-page-postscript (1+ ps-page-postscript)
	ps-page-order (1+ ps-page-order))
  (and (> ps-page-order 1)
       (ps-output "EndSheet\n"))
  (ps-output (format "\n%%%%Page: %d %d\n"
		     ps-page-postscript ps-page-order))
  (ps-output (format "%d BeginSheet\nBeginDSCPage\n" ps-n-up-printing)))


(defsubst ps-header-page ()
  ;; set total line and page number when printing has finished
  ;; (see `ps-generate')
  (run-hooks
   (if (prog1
	   (zerop (mod ps-page-count ps-number-of-columns))
	 (setq ps-page-count (1+ ps-page-count)))
       (prog1
	   (if (zerop (mod ps-page-postscript ps-n-up-printing))
	       ;; Print only when a new sheet begins.
	       (progn
		 (ps-header-sheet)
		 'ps-print-begin-sheet-hook)
	     ;; Print only when a new page begins.
	     (setq ps-page-postscript (1+ ps-page-postscript))
	     (ps-output "BeginDSCPage\n")
	     'ps-print-begin-page-hook)
	 (ps-background ps-page-postscript))
     ;; Print only when a new column begins.
     (ps-output "BeginDSCPage\n")
     'ps-print-begin-column-hook)))

(defun ps-begin-page ()
  (ps-get-page-dimensions)
  (setq ps-width-remaining  ps-print-width
	ps-height-remaining ps-print-height)

  (ps-header-page)

  (ps-output (format "/LineNumber %d def\n" ps-showline-count)
	     (format "/PageNumber %d def\n" (if ps-print-only-one-header
						(ps-page-number)
					      ps-page-count)))

  (when ps-print-header
    (ps-generate-header "HeaderLinesLeft"    ps-left-header)
    (ps-generate-header "HeaderLinesRight"   ps-right-header)
    (ps-output (format "%d SetHeaderLines\n" ps-header-lines)))

  (ps-output "BeginPage\n")
  (ps-set-font  ps-current-font)
  (ps-set-bg    ps-current-bg)
  (ps-set-color ps-current-color)
  (ps-mule-begin-page))

(defun ps-end-page ()
  (ps-output "EndPage\nEndDSCPage\n"))

(defun ps-dummy-page ()
  (let ((ps-n-up-printing 0))
    (ps-header-sheet))
  (ps-output "/PrintHeader false def
/ColumnIndex 0 def
/PrintLineNumber false def
BeginPage
EndPage
EndDSCPage\n")
  (setq ps-page-postscript ps-n-up-printing))

(defun ps-next-line ()
  (setq ps-showline-count (1+ ps-showline-count))
  (let ((lh (ps-line-height 'ps-font-for-text)))
    (if (< ps-height-remaining lh)
	(ps-next-page)
      (setq ps-width-remaining  ps-print-width
	    ps-height-remaining (- ps-height-remaining lh))
      (ps-output "HL\n"))))

(defun ps-continue-line ()
  (let ((lh (ps-line-height 'ps-font-for-text)))
    (if (< ps-height-remaining lh)
	(ps-next-page)
      (setq ps-width-remaining  ps-print-width
	    ps-height-remaining (- ps-height-remaining lh))
      (ps-output "SL\n"))))

(defun ps-find-wrappoint (from to char-width)
  (let ((avail (truncate (/ ps-width-remaining char-width)))
	(todo (- to from)))
    (if (< todo avail)
	(cons to (* todo char-width))
      (cons (+ from avail) ps-width-remaining))))

(defun ps-basic-plot-string (from to &optional bg-color)
  (let* ((wrappoint (ps-find-wrappoint from to
				       (ps-avg-char-width 'ps-font-for-text)))
	 (to (car wrappoint))
	 (string (buffer-substring-no-properties from to)))
    (ps-mule-prepare-ascii-font string)
    (ps-output-string string)
    (ps-output " S\n")
    wrappoint))

(defun ps-basic-plot-whitespace (from to &optional bg-color)
  (let* ((wrappoint (ps-find-wrappoint from to
				       (ps-space-width 'ps-font-for-text)))
	 (to (car wrappoint)))
    (ps-output (format "%d W\n" (- to from)))
    wrappoint))

(defun ps-plot (plotfunc from to &optional bg-color)
  (while (< from to)
    (let* ((wrappoint (funcall plotfunc from to bg-color))
	   (plotted-to (car wrappoint))
	   (plotted-width (cdr wrappoint)))
      (setq from plotted-to
	    ps-width-remaining (- ps-width-remaining plotted-width))
      (if (< from to)
	  (ps-continue-line))))
  (if ps-razzle-dazzle
      (let* ((q-todo (- (point-max) (point-min)))
	     (q-done (- (point) (point-min)))
	     (chunkfrac (/ q-todo 8))
	     (chunksize (min chunkfrac 1000)))
	(if (> (- q-done ps-razchunk) chunksize)
	    (progn
	      (setq ps-razchunk q-done)
	      (message "Formatting...%3d%%"
		       (if (< q-todo 100)
			   (/ (* 100 q-done) q-todo)
			 (/ q-done (/ q-todo 100)))
		       ))))))

(defvar ps-last-font nil)

(defun ps-set-font (font)
  (setq ps-last-font (format "f%d" (setq ps-current-font font)))
  (ps-output (format "/%s F\n" ps-last-font)))

(defun ps-set-bg (color)
  (if (setq ps-current-bg color)
      (ps-output (format ps-color-format
			 (nth 0 color) (nth 1 color) (nth 2 color))
		 " true BG\n")
    (ps-output "false BG\n")))

(defun ps-set-color (color)
  (setq ps-current-color (or color ps-default-fg))
  (ps-output (format ps-color-format
		     (nth 0 ps-current-color)
		     (nth 1 ps-current-color) (nth 2 ps-current-color))
	     " FG\n"))


(defvar ps-current-effect 0)


(defun ps-plot-region (from to font &optional fg-color bg-color effects)
  (if (not (equal font ps-current-font))
      (ps-set-font font))

  ;; Specify a foreground color only if one's specified and it's
  ;; different than the current.
  (if (not (equal fg-color ps-current-color))
      (ps-set-color fg-color))

  (if (not (equal bg-color ps-current-bg))
      (ps-set-bg bg-color))

  ;; Specify effects (underline, overline, box, etc)
  (cond
   ((not (integerp effects))
    (ps-output "0 EF\n")
    (setq ps-current-effect 0))
   ((/= effects ps-current-effect)
    (ps-output (number-to-string effects) " EF\n")
    (setq ps-current-effect effects)))

  ;; Starting at the beginning of the specified region...
  (save-excursion
    (goto-char from)

    ;; ...break the region up into chunks separated by tabs, linefeeds,
    ;; pagefeeds, control characters, and plot each chunk.
    (while (< from to)
      (if (re-search-forward ps-control-or-escape-regexp to t)
	  ;; region with some control characters or some multi-byte characters
	  (let* ((match-point (match-beginning 0))
		 (match (char-after match-point)))
	    (when (< from match-point)
	      (ps-mule-set-ascii-font)
	      (ps-plot 'ps-basic-plot-string from match-point bg-color))
	    (cond
	     ((= match ?\t)		; tab
	      (let ((linestart (line-beginning-position)))
		(forward-char -1)
		(setq from (+ linestart (current-column)))
		(when (re-search-forward "[ \t]+" to t)
		  (ps-mule-set-ascii-font)
		  (ps-plot 'ps-basic-plot-whitespace
			   from (+ linestart (current-column))
			   bg-color))))

	     ((= match ?\n)		; newline
	      (ps-next-line))

	     ((= match ?\f)		; form feed
	      ;; do not skip page if previous character is NEWLINE and
	      ;; it is a beginning of page.
	      (or (and (= (char-after (1- match-point)) ?\n)
		       (= ps-height-remaining ps-print-height))
		  (ps-next-page)))

	     ((> match 255)		; a multi-byte character
	      (let ((charset (char-charset match)))
		(or (eq charset 'composition)
		    (while (eq (charset-after) charset)
		      (forward-char 1)))
		(ps-plot 'ps-mule-plot-string match-point (point) bg-color)))
					; characters from ^@ to ^_ and
	     (t				; characters from 127 to 255
	      (ps-control-character match)))
	    (setq from (point)))
	;; region without control characters nor multi-byte characters
	(ps-mule-set-ascii-font)
	(ps-plot 'ps-basic-plot-string from to bg-color)
	(setq from to)))))

(defvar ps-string-control-codes
  (let ((table (make-vector 256 nil))
	(char ?\000))
    ;; control character
    (while (<= char ?\037)
      (aset table char (format "^%c" (+ char ?@)))
      (setq char (1+ char)))
    ;; printable character
    (while (< char ?\177)
      (aset table char (format "%c" char))
      (setq char (1+ char)))
    ;; DEL
    (aset table char "^?")
    ;; 8-bit character
    (while (<= (setq char (1+ char)) ?\377)
      (aset table char (format "\\%o" char)))
    table)
  "Vector used to map characters to a printable string.")

(defun ps-control-character (char)
  (let* ((str (aref ps-string-control-codes char))
	 (from (1- (point)))
	 (len (length str))
	 (to (+ from len))
	 (char-width (ps-avg-char-width 'ps-font-for-text))
	 (wrappoint (ps-find-wrappoint from to char-width)))
    (if (< (car wrappoint) to)
	(ps-continue-line))
    (setq ps-width-remaining (- ps-width-remaining (* len char-width)))
    (ps-mule-prepare-ascii-font str)
    (ps-output-string str)
    (ps-output " S\n")))

(defun ps-color-value (x-color-value)
  ;; Scale 16-bit X-COLOR-VALUE to PostScript color value in [0, 1] interval.
  (/ x-color-value ps-print-color-scale))


(cond ((eq ps-print-emacs-type 'emacs)  ; emacs

       (defun ps-color-values (x-color)
	 (if (fboundp 'x-color-values)
	     (x-color-values x-color)
	   (error "No available function to determine X color values.")))
       )
					; xemacs
					; lucid
      (t				; epoch
       (defun ps-color-values (x-color)
	 (cond ((fboundp 'x-color-values)
		(x-color-values x-color))
	       ((and (fboundp 'color-instance-rgb-components)
		     (ps-color-device))
		(color-instance-rgb-components
		 (if (color-instance-p x-color)
		     x-color
		   (make-color-instance
		    (if (color-specifier-p x-color)
			(color-name x-color)
		      x-color)))))
	       (t
		(error "No available function to determine X color values."))))
       ))


(defun ps-face-attributes (face)
  "Return face attribute vector.

If FACE is not in `ps-print-face-extension-alist' or in
`ps-print-face-alist', insert it on `ps-print-face-alist' and
return the attribute vector.

If FACE is not a valid face name, it is used default face."
  (cdr (or (assq face ps-print-face-extension-alist)
	   (assq face ps-print-face-alist)
	   (let* ((the-face (if (facep face) face 'default))
		  (new-face (ps-screen-to-bit-face the-face)))
	     (or (and (eq the-face 'default)
		      (assq the-face ps-print-face-alist))
		 (setq ps-print-face-alist (cons new-face ps-print-face-alist)))
	     new-face))))


(defun ps-face-attribute-list (face-or-list)
  (if (listp face-or-list)
      ;; list of faces
      (let ((effects 0)
	    foreground background face-attr)
	(while face-or-list
	  (setq face-attr (ps-face-attributes (car face-or-list))
		effects (logior effects (aref face-attr 0)))
	  (or foreground (setq foreground (aref face-attr 1)))
	  (or background (setq background (aref face-attr 2)))
	  (setq face-or-list (cdr face-or-list)))
	(vector effects foreground background))
    ;; simple face
    (ps-face-attributes face-or-list)))


(defconst ps-font-type (vector nil 'bold 'italic 'bold-italic))


(defun ps-plot-with-face (from to face)
  (cond
   ((null face)				; print text with null face
    (ps-plot-region from to 0))
   ((eq face 'emacs--invisible--face))	; skip invisible text!!!
   (t					; otherwise, text has a valid face
    (let* ((face-bit   (ps-face-attribute-list face))
	   (effect     (aref face-bit 0))
	   (foreground (aref face-bit 1))
	   (background (aref face-bit 2))
	   (fg-color (if (and ps-color-p foreground)
			 (mapcar 'ps-color-value
				 (ps-color-values foreground))
		       ps-default-color))
	   (bg-color (and ps-color-p background
			  (mapcar 'ps-color-value
				  (ps-color-values background)))))
      (ps-plot-region
       from to
       (ps-font-number 'ps-font-for-text
		       (or (aref ps-font-type (logand effect 3))
			   face))
       fg-color bg-color (lsh effect -2)))))
  (goto-char to))


(defun ps-xemacs-face-kind-p (face kind kind-regex kind-list)
  (let* ((frame-font (or (face-font-instance face)
			 (face-font-instance 'default)))
	 (kind-cons (and frame-font
			 (assq kind
			       (font-instance-properties frame-font))))
	 (kind-spec (cdr-safe kind-cons))
	 (case-fold-search t))
    (or (and kind-spec (string-match kind-regex kind-spec))
	;; Kludge-compatible:
	(memq face kind-list))))


(cond ((eq ps-print-emacs-type 'emacs)  ; emacs

       (defun ps-face-bold-p (face)
	 (or (face-bold-p face)
	     (memq face ps-bold-faces)))

       (defun ps-face-italic-p (face)
	 (or (face-italic-p face)
	     (memq face ps-italic-faces)))
       )
					; xemacs
					; lucid
      (t				; epoch
       (defun ps-face-bold-p (face)
	 (ps-xemacs-face-kind-p face 'WEIGHT_NAME "bold\\|demibold"
				ps-bold-faces))

       (defun ps-face-italic-p (face)
	 (or (ps-xemacs-face-kind-p face 'ANGLE_NAME "i\\|o" ps-italic-faces)
	     (ps-xemacs-face-kind-p face 'SLANT "i\\|o" ps-italic-faces)))
       ))


(defun ps-face-underlined-p (face)
  (or (face-underline-p face)
      (memq face ps-underlined-faces)))


;; Ensure that face-list is fbound.
(or (fboundp 'face-list) (defalias 'face-list 'list-faces))


(defun ps-build-reference-face-lists ()
  ;; Ensure that face database is updated with faces on
  ;; `font-lock-face-attributes' (obsolete stuff)
  (ps-font-lock-face-attributes)
  ;; Now, rebuild reference face lists
  (setq ps-print-face-alist nil)
  (if ps-auto-font-detect
      (mapcar 'ps-map-face (face-list))
    (mapcar 'ps-set-face-bold ps-bold-faces)
    (mapcar 'ps-set-face-italic ps-italic-faces)
    (mapcar 'ps-set-face-underline ps-underlined-faces))
  (setq ps-build-face-reference nil))


(defun ps-set-face-bold (face)
  (ps-set-face-attribute face 1))

(defun ps-set-face-italic (face)
  (ps-set-face-attribute face 2))

(defun ps-set-face-underline (face)
  (ps-set-face-attribute face 4))


(defun ps-set-face-attribute (face effect)
  (let ((face-bit (cdr (ps-map-face face))))
    (aset face-bit 0 (logior (aref face-bit 0) effect))))


(defun ps-map-face (face)
  (let* ((face-map (ps-screen-to-bit-face face))
	 (ps-face-bit (cdr (assq (car face-map) ps-print-face-alist))))
    (if ps-face-bit
	;; if face exists, merge both
	(let ((face-bit (cdr face-map)))
	  (aset ps-face-bit 0 (logior (aref ps-face-bit 0) (aref face-bit 0)))
	  (or (aref ps-face-bit 1) (aset ps-face-bit 1 (aref face-bit 1)))
	  (or (aref ps-face-bit 2) (aset ps-face-bit 2 (aref face-bit 2))))
      ;; if face does not exist, insert it
      (setq ps-print-face-alist (cons face-map ps-print-face-alist)))
    face-map))


(defun ps-screen-to-bit-face (face)
  (cons face
	(vector (logior (if (ps-face-bold-p face) 1 0) ; bold
			(if (ps-face-italic-p face) 2 0) ; italic
			(if (ps-face-underlined-p face) 4 0)) ; underline
		(face-foreground face)
		(face-background face))))


(cond ((not (eq ps-print-emacs-type 'emacs))
					; xemacs
					; lucid
					; epoch
       (defun ps-mapper (extent list)
	 (nconc list (list (list (extent-start-position extent) 'push extent)
			   (list (extent-end-position extent) 'pull extent)))
	 nil)

       (defun ps-extent-sorter (a b)
	 (< (extent-priority a) (extent-priority b)))
       ))


(defun ps-print-ensure-fontified (start end)
  (and (boundp 'lazy-lock-mode) (symbol-value 'lazy-lock-mode)
       (lazy-lock-fontify-region start end)))

(defun ps-generate-postscript-with-faces (from to)
  ;; Some initialization...
  (setq ps-current-effect 0)

  ;; Build the reference lists of faces if necessary.
  (if (or ps-always-build-face-reference
	  ps-build-face-reference)
      (progn
	(message "Collecting face information...")
	(ps-build-reference-face-lists)))
  ;; Set the color scale.  We do it here instead of in the defvar so
  ;; that ps-print can be dumped into emacs.  This expression can't be
  ;; evaluated at dump-time because X isn't initialized.
  (setq ps-color-p           (and ps-print-color-p (ps-color-device))
	ps-print-color-scale (if ps-color-p
				 (float (car (ps-color-values "white")))
			       1.0))
  ;; Generate some PostScript.
  (save-restriction
    (narrow-to-region from to)
    (ps-print-ensure-fontified from to)
    (let ((face 'default)
	  (position to))
      (cond
       ((or (eq ps-print-emacs-type 'lucid)
	    (eq ps-print-emacs-type 'xemacs))
	;; Build the list of extents...
	(let ((a (cons 'dummy nil))
	      record type extent extent-list)
	  (map-extents 'ps-mapper nil from to a)
	  (setq a (sort (cdr a) 'car-less-than-car)
		extent-list nil)

	  ;; Loop through the extents...
	  (while a
	    (setq record (car a)

		  position (car record)
		  record (cdr record)

		  type (car record)
		  record (cdr record)

		  extent (car record))

	    ;; Plot up to this record.
	    ;; XEmacs 19.12: for some reason, we're getting into a
	    ;; situation in which some of the records have
	    ;; positions less than 'from'.  Since we've narrowed
	    ;; the buffer, this'll generate errors.  This is a
	    ;; hack, but don't call ps-plot-with-face unless from >
	    ;; point-min.
	    (and (>= from (point-min)) (<= position (point-max))
		 (ps-plot-with-face from position face))

	    (cond
	     ((eq type 'push)
	      (and (extent-face extent)
		   (setq extent-list (sort (cons extent extent-list)
					   'ps-extent-sorter))))

	     ((eq type 'pull)
	      (setq extent-list (sort (delq extent extent-list)
				      'ps-extent-sorter))))

	    (setq face (if extent-list
			   (extent-face (car extent-list))
			 'default)
		  from position
		  a (cdr a)))))

       ((eq ps-print-emacs-type 'emacs)
	(let ((property-change from)
	      (overlay-change from)
	      (save-buffer-invisibility-spec buffer-invisibility-spec)
	      (buffer-invisibility-spec nil))
	  (while (< from to)
	    (and (< property-change to)	; Don't search for property change
					; unless previous search succeeded.
		 (setq property-change (next-property-change from nil to)))
	    (and (< overlay-change to)	; Don't search for overlay change
					; unless previous search succeeded.
		 (setq overlay-change (min (next-overlay-change from) to)))
	    (setq position (min property-change overlay-change))
	    ;; The code below is not quite correct,
	    ;; because a non-nil overlay invisible property
	    ;; which is inactive according to the current value
	    ;; of buffer-invisibility-spec nonetheless overrides
	    ;; a face text property.
	    (setq face
		  (cond ((let ((prop (get-text-property from 'invisible)))
			   ;; Decide whether this invisible property
			   ;; really makes the text invisible.
			   (if (eq save-buffer-invisibility-spec t)
			       (not (null prop))
			     (or (memq prop save-buffer-invisibility-spec)
				 (assq prop save-buffer-invisibility-spec))))
			 'emacs--invisible--face)
			((get-text-property from 'face))
			(t 'default)))
	    (let ((overlays (overlays-at from))
		  (face-priority -1))	; text-property
	      (while (and overlays
			  (not (eq face 'emacs--invisible--face)))
		(let* ((overlay (car overlays))
		       (overlay-invisible (overlay-get overlay 'invisible))
		       (overlay-priority (or (overlay-get overlay 'priority)
					     0)))
		  (and (> overlay-priority face-priority)
		       (setq face
			     (cond ((if (eq save-buffer-invisibility-spec t)
					(not (null overlay-invisible))
				      (or (memq overlay-invisible
						save-buffer-invisibility-spec)
					  (assq overlay-invisible
						save-buffer-invisibility-spec)))
				    'emacs--invisible--face)
				   ((overlay-get overlay 'face))
				   (t face))
			     face-priority overlay-priority)))
		(setq overlays (cdr overlays))))
	    ;; Plot up to this record.
	    (ps-plot-with-face from position face)
	    (setq from position)))))
      (ps-plot-with-face from to face))))

(defun ps-generate-postscript (from to)
  (ps-plot-region from to 0 nil))

(defun ps-generate (buffer from to genfunc)
  (save-excursion
    (let ((from (min to from))
	  (to (max to from))
	  ;; This avoids trouble if chars with read-only properties
	  ;; are copied into ps-spool-buffer.
	  (inhibit-read-only t))
      (save-restriction
	(narrow-to-region from to)
	(and ps-razzle-dazzle
	     (message "Formatting...%3d%%" (setq ps-razchunk 0)))
	(setq ps-source-buffer buffer
	      ps-spool-buffer (get-buffer-create ps-spool-buffer-name))
	(ps-init-output-queue)
	(let (safe-marker completed-safely needs-begin-file)
	  (unwind-protect
	      (progn
		(set-buffer ps-spool-buffer)
		(set-buffer-multibyte nil)

		;; Get a marker and make it point to the current end of the
		;; buffer,  If an error occurs, we'll delete everything from
		;; the end of this marker onwards.
		(setq safe-marker (make-marker))
		(set-marker safe-marker (point-max))

		(goto-char (point-min))
		(or (looking-at (regexp-quote ps-adobe-tag))
		    (setq needs-begin-file t))
		(save-excursion
		  (set-buffer ps-source-buffer)
		  (ps-begin-job)
		  (when needs-begin-file
		    (ps-begin-file)
		    (ps-mule-initialize))
		  (ps-mule-begin-job from to)
		  (ps-begin-page))
		(set-buffer ps-source-buffer)
		(funcall genfunc from to)
		(ps-end-page)

		(ps-end-file)
		(ps-flush-output)
		(ps-end-job)

		;; Setting this variable tells the unwind form that the
		;; the PostScript was generated without error.
		(setq completed-safely t))

	    ;; Unwind form: If some bad mojo occurred while generating
	    ;; PostScript, delete all the PostScript that was generated.
	    ;; This protects the previously spooled files from getting
	    ;; corrupted.
	    (and (markerp safe-marker) (not completed-safely)
		 (progn
		   (set-buffer ps-spool-buffer)
		   (delete-region (marker-position safe-marker) (point-max))))))

	(and ps-razzle-dazzle (message "Formatting...done"))))))


(defun ps-end-job ()
  (let ((total-lines (cdr ps-printing-region))
	(total-pages (if ps-print-only-one-header
			 (ps-page-number)
		       ps-page-count))
	case-fold-search)
    (set-buffer ps-spool-buffer)
    ;; Back to the PS output buffer to set the page count
    (goto-char (point-min))
    (and (re-search-forward "^/Lines 0 def\n/PageCount 0 def$" nil t)
	 (replace-match (format "/Lines %d def\n/PageCount %d def"
				total-lines total-pages) t))))


;; Permit dynamic evaluation at print time of `ps-lpr-switches'.
(defun ps-do-despool (filename)
  (if (or (not (boundp 'ps-spool-buffer))
	  (not (symbol-value 'ps-spool-buffer)))
      (message "No spooled PostScript to print")
    (if filename
	(save-excursion
	  (and ps-razzle-dazzle (message "Saving..."))
	  (set-buffer ps-spool-buffer)
	  (setq filename (expand-file-name filename))
	  (let ((coding-system-for-write 'raw-text-unix))
	    (write-region (point-min) (point-max) filename))
	  (and ps-razzle-dazzle (message "Wrote %s" filename)))
      ;; Else, spool to the printer
      (and ps-razzle-dazzle (message "Printing..."))
      (save-excursion
	(set-buffer ps-spool-buffer)
	(let* ((coding-system-for-write 'raw-text-unix)
	       (ps-printer-name (or ps-printer-name
				    (and (boundp 'printer-name)
					 printer-name)))
	       (ps-lpr-switches
		(append (and (stringp ps-printer-name)
			     (list (concat "-P" ps-printer-name)))
			ps-lpr-switches)))
	  (apply (or ps-print-region-function 'call-process-region)
		 (point-min) (point-max) ps-lpr-command nil
		 (and (fboundp 'start-process) 0)
		 nil
		 (ps-flatten-list	; dynamic evaluation
		  (mapcar 'ps-eval-switch ps-lpr-switches)))))
      (and ps-razzle-dazzle (message "Printing...done")))
    (kill-buffer ps-spool-buffer)))

;; Dynamic evaluation
(defun ps-eval-switch (arg)
  (cond ((stringp arg) arg)
	((functionp arg) (apply arg nil))
	((symbolp arg) (symbol-value arg))
	((consp arg) (apply (car arg) (cdr arg)))
	(t nil)))

;; `ps-flatten-list' is defined here (copied from "message.el" and
;; enhanced to handle dotted pairs as well) until we can get some
;; sensible autoloads, or `flatten-list' gets put somewhere decent.

;; (ps-flatten-list '((a . b) c (d . e) (f g h) i . j))
;; => (a b c d e f g h i j)

(defun ps-flatten-list (&rest list)
  (ps-flatten-list-1 list))

(defun ps-flatten-list-1 (list)
  (cond ((null list) nil)
	((consp list) (append (ps-flatten-list-1 (car list))
			      (ps-flatten-list-1 (cdr list))))
	(t (list list))))

(defun ps-kill-emacs-check ()
  (let (ps-buffer)
    (and (setq ps-buffer (get-buffer ps-spool-buffer-name))
	 (buffer-modified-p ps-buffer)
	 (y-or-n-p "Unprinted PostScript waiting; print now? ")
	 (ps-despool))
    (and (setq ps-buffer (get-buffer ps-spool-buffer-name))
	 (buffer-modified-p ps-buffer)
	 (not (yes-or-no-p "Unprinted PostScript waiting; exit anyway? "))
	 (error "Unprinted PostScript"))))

(cond ((fboundp 'add-hook)
       (funcall 'add-hook 'kill-emacs-hook 'ps-kill-emacs-check))
      (kill-emacs-hook
       (message "Won't override existing `kill-emacs-hook'"))
      (t
       (setq kill-emacs-hook 'ps-kill-emacs-check)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sample Setup Code:

;; This stuff is for anybody that's brave enough to look this far,
;; and able to figure out how to use it.  It isn't really part of
;; ps-print, but I'll leave it here in hopes it might be useful:

;; WARNING!!! The following code is *sample* code only.
;; Don't use it unless you understand what it does!

(defmacro ps-prsc ()
  `(if (eq ps-print-emacs-type 'emacs) [f22] 'f22))
(defmacro ps-c-prsc ()
  `(if (eq ps-print-emacs-type 'emacs) [C-f22] '(control f22)))
(defmacro ps-s-prsc ()
  `(if (eq ps-print-emacs-type 'emacs) [S-f22] '(shift f22)))

;; A hook to bind to `rmail-mode-hook' to locally bind prsc and set the
;; `ps-left-headers' specially for mail messages.
(defun ps-rmail-mode-hook ()
  (local-set-key (ps-prsc) 'ps-rmail-print-message-from-summary)
  (setq ps-header-lines 3
	ps-left-header
	;; The left headers will display the message's subject, its
	;; author, and the name of the folder it was in.
	'(ps-article-subject ps-article-author buffer-name)))

;; See `ps-gnus-print-article-from-summary'.  This function does the
;; same thing for rmail.
(defun ps-rmail-print-message-from-summary ()
  (interactive)
  (ps-print-message-from-summary 'rmail-summary-buffer "RMAIL"))

;; Used in `ps-rmail-print-article-from-summary',
;; `ps-gnus-print-article-from-summary' and `ps-vm-print-message-from-summary'.
(defun ps-print-message-from-summary (summary-buffer summary-default)
  (let ((ps-buf (or (and (boundp summary-buffer)
			 (symbol-value summary-buffer))
		    summary-default)))
    (and (get-buffer ps-buf)
	 (save-excursion
	   (set-buffer ps-buf)
	   (ps-spool-buffer-with-faces)))))

;; Look in an article or mail message for the Subject: line.  To be
;; placed in `ps-left-headers'.
(defun ps-article-subject ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Subject:[ \t]+\\(.*\\)$" nil t)
	(buffer-substring-no-properties (match-beginning 1) (match-end 1))
      "Subject ???")))

;; Look in an article or mail message for the From: line.  Sorta-kinda
;; understands RFC-822 addresses and can pull the real name out where
;; it's provided.  To be placed in `ps-left-headers'.
(defun ps-article-author ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^From:[ \t]+\\(.*\\)$" nil t)
	(let ((fromstring (buffer-substring-no-properties (match-beginning 1)
							  (match-end 1))))
	  (cond

	   ;; Try first to match addresses that look like
	   ;; thompson@wg2.waii.com (Jim Thompson)
	   ((string-match ".*[ \t]+(\\(.*\\))" fromstring)
	    (substring fromstring (match-beginning 1) (match-end 1)))

	   ;; Next try to match addresses that look like
	   ;; Jim Thompson <thompson@wg2.waii.com>
	   ((string-match "\\(.*\\)[ \t]+<.*>" fromstring)
	    (substring fromstring (match-beginning 1) (match-end 1)))

	   ;; Couldn't find a real name -- show the address instead.
	   (t fromstring)))
      "From ???")))

;; A hook to bind to `gnus-article-prepare-hook'.  This will set the
;; `ps-left-headers' specially for gnus articles.  Unfortunately,
;; `gnus-article-mode-hook' is called only once, the first time the *Article*
;; buffer enters that mode, so it would only work for the first time
;; we ran gnus.  The second time, this hook wouldn't get set up.  The
;; only alternative is `gnus-article-prepare-hook'.
(defun ps-gnus-article-prepare-hook ()
  (setq ps-header-lines 3
	ps-left-header
	;; The left headers will display the article's subject, its
	;; author, and the newsgroup it was in.
	'(ps-article-subject ps-article-author gnus-newsgroup-name)))

;; A hook to bind to `vm-mode-hook' to locally bind prsc and set the
;; `ps-left-headers' specially for mail messages.
(defun ps-vm-mode-hook ()
  (local-set-key (ps-prsc) 'ps-vm-print-message-from-summary)
  (setq ps-header-lines 3
	ps-left-header
	;; The left headers will display the message's subject, its
	;; author, and the name of the folder it was in.
	'(ps-article-subject ps-article-author buffer-name)))

;; Every now and then I forget to switch from the *Summary* buffer to
;; the *Article* before hitting prsc, and a nicely formatted list of
;; article subjects shows up at the printer.  This function, bound to
;; prsc for the gnus *Summary* buffer means I don't have to switch
;; buffers first.
;; sb:  Updated for Gnus 5.
(defun ps-gnus-print-article-from-summary ()
  (interactive)
  (ps-print-message-from-summary 'gnus-article-buffer "*Article*"))

;; See `ps-gnus-print-article-from-summary'.  This function does the
;; same thing for vm.
(defun ps-vm-print-message-from-summary ()
  (interactive)
  (ps-print-message-from-summary 'vm-mail-buffer ""))

;; A hook to bind to bind to `gnus-summary-setup-buffer' to locally bind
;; prsc.
(defun ps-gnus-summary-setup ()
  (local-set-key (ps-prsc) 'ps-gnus-print-article-from-summary))

;; Look in an article or mail message for the Subject: line.  To be
;; placed in `ps-left-headers'.
(defun ps-info-file ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "File:[ \t]+\\([^, \t\n]*\\)" nil t)
	(buffer-substring-no-properties (match-beginning 1) (match-end 1))
      "File ???")))

;; Look in an article or mail message for the Subject: line.  To be
;; placed in `ps-left-headers'.
(defun ps-info-node ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "Node:[ \t]+\\([^,\t\n]*\\)" nil t)
	(buffer-substring-no-properties (match-beginning 1) (match-end 1))
      "Node ???")))

(defun ps-info-mode-hook ()
  (setq ps-left-header
	;; The left headers will display the node name and file name.
	'(ps-info-node ps-info-file)))

;; WARNING! The following function is a *sample* only, and is *not*
;; meant to be used as a whole unless you understand what the effects
;; will be!  (In fact, this is a copy of Jim's setup for ps-print --
;; I'd be very surprised if it was useful to *anybody*, without
;; modification.)

(defun ps-jts-ps-setup ()
  (global-set-key (ps-prsc) 'ps-spool-buffer-with-faces) ;f22 is prsc
  (global-set-key (ps-s-prsc) 'ps-spool-region-with-faces)
  (global-set-key (ps-c-prsc) 'ps-despool)
  (add-hook 'gnus-article-prepare-hook 'ps-gnus-article-prepare-hook)
  (add-hook 'gnus-summary-mode-hook 'ps-gnus-summary-setup)
  (add-hook 'vm-mode-hook 'ps-vm-mode-hook)
  (add-hook 'vm-mode-hooks 'ps-vm-mode-hook)
  (add-hook 'Info-mode-hook 'ps-info-mode-hook)
  (setq ps-spool-duplex t
	ps-print-color-p nil
	ps-lpr-command "lpr"
	ps-lpr-switches '("-Jjct,duplex_long"))
  'ps-jts-ps-setup)

;; WARNING! The following function is a *sample* only, and is *not*
;; meant to be used as a whole unless it corresponds to your needs.
;; (In fact, this is a copy of Jack's setup for ps-print --
;; I would not be that surprised if it was useful to *anybody*,
;; without modification.)

(defun ps-jack-setup ()
  (setq ps-print-color-p  nil
	ps-lpr-command    "lpr"
	ps-lpr-switches   nil

	ps-paper-type        'a4
	ps-landscape-mode    t
	ps-number-of-columns 2

	ps-left-margin   (/ (* 72  1.0) 2.54) ;  1.0 cm
	ps-right-margin  (/ (* 72  1.0) 2.54) ;  1.0 cm
	ps-inter-column  (/ (* 72  1.0) 2.54) ;  1.0 cm
	ps-bottom-margin (/ (* 72  1.5) 2.54) ;  1.5 cm
	ps-top-margin    (/ (* 72  1.5) 2.54) ;  1.5 cm
	ps-header-offset (/ (* 72  1.0) 2.54) ;  1.0 cm
	ps-header-line-pad    .15
	ps-print-header       t
	ps-print-header-frame t
	ps-header-lines       2
	ps-show-n-of-n        t
	ps-spool-duplex       nil

	ps-font-family             'Courier
	ps-font-size               5.5
	ps-header-font-family      'Helvetica
	ps-header-font-size        6
	ps-header-title-font-size  8)
  'ps-jack-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To make this file smaller, some commands go in a separate file.
;; But autoload them here to make the separation invisible.

(autoload 'ps-mule-prepare-ascii-font "ps-mule"
  "Setup special ASCII font for STRING.
STRING should contain only ASCII characters.")

(autoload 'ps-mule-set-ascii-font     "ps-mule"
  "Adjust current font if current charset is not ASCII.")

(autoload 'ps-mule-plot-string        "ps-mule"
  "Generate PostScript code for ploting characters in the region FROM and TO.

It is assumed that all characters in this region belong to the same charset.

Optional argument BG-COLOR specifies background color.

Returns the value:

	(ENDPOS . RUN-WIDTH)

Where ENDPOS is the end position of the sequence and RUN-WIDTH is the width of
the sequence.")

(autoload 'ps-mule-initialize         "ps-mule"
  "Initialize global data for printing multi-byte characters.")

(autoload 'ps-mule-begin-job          "ps-mule"
  "Start printing job for multi-byte chars between FROM and TO.
This checks if all multi-byte characters in the region are printable or not.")

(autoload 'ps-mule-begin-page         "ps-mule"
  "Initialize multi-byte charset for printing current page.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ps-print)

;;; ps-print.el ends here
