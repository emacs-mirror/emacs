;;; ada-mode.el - An Emacs major-mode for editing Ada source.
;;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

;;; Authors: Markus Heritsch <Markus.Heritsch@studbox.uni-stuttgart.de>
;;;          Rolf Ebert      <ebert@inf.enst.fr>

;;; This file is part of GNU Emacs.

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

;;; This mode is a complete rewrite of a major mode for editing Ada 83
;;; and Ada 95 source code under Emacs-19.  It contains completely new
;;; indenting code and support for code browsing (see ada-xref).


;;; USAGE
;;; =====
;;; Emacs should enter ada-mode when you load an ada source (*.ad[abs]).
;;;
;;; When you have entered ada-mode, you may get more info by pressing
;;; C-h m. You may also get online help describing various functions by:
;;; C-h d <Name of function you want described>


;;; HISTORY
;;; =======
;;; The first Ada mode for GNU Emacs was written by V. Broman in
;;; 1985. He based his work on the already existing Modula-2 mode.
;;; This was distributed as ada.el in versions of Emacs prior to 19.29.
;;;
;;; Lynn Slater wrote an extensive Ada mode in 1989. It consisted of
;;; several files with support for dired commands and other nice
;;; things. It is currently available from the PAL
;;; (wuarchive.wustl.edu:/languages/ada) as ada-mode-1.06a.tar.Z.
;;;
;;; The probably very first Ada mode (called electric-ada.el) was
;;; written by Steven D. Litvintchouk and Steven M. Rosen for the
;;; Gosling Emacs. L. Slater based his development on ada.el and
;;; electric-ada.el.
;;;
;;; The current Ada mode is a complete rewrite by M. Heritsch and
;;; R. Ebert.  Some ideas from the ada-mode mailing list have been
;;; added.  Some of the functionality of L. Slater's mode has not
;;; (yet) been recoded in this new mode.  Perhaps you prefer sticking
;;; to his version.


;;; KNOWN BUGS
;;; ==========
;;;
;;; In the presence of comments and/or incorrect syntax
;;; ada-format-paramlist produces weird results.
;;;
;;; Indenting of some tasking constructs is still buggy.
;;; -------------------
;;;   For tagged types the problem comes from the keyword abstract:

;;;   type T2 is abstract tagged record
;;;   X : Integer;
;;;   Y : Float;
;;;   end record;
;;; -------------------	
;;; In Emacs FSF 19.28, ada-mode will correctly indent comments at the
;;; very beginning of the buffer (_before_ any code) when I go M-; but
;;; when I press TAB I'd expect the comments to be placed at the beginning
;;; of the line, just as the first line of _code_ would be indented.

;;; This does not happen but the comment stays put :-( I end up going 
;;; M-; C-a M-\
;;; -------------------
;;; package Test is
;;;    -- If I hit return on the "type" line it will indent the next line
;;;    -- in another 3 space instead of heading out to the "(". If I hit
;;;    -- tab or return it reindents the line correctly but does not initially.
;;;    type Wait_Return is (Read_Success, Read_Timeout, Wait_Timeout,
;;;       Nothing_To_Wait_For_In_Wait_List);
;;;
;;;    -- The following line will be wrongly reindented after typing it in after
;;;    -- the initial indent for the line was correct after type return after
;;;    -- this line. Subsequent lines will show the same problem.
;;; Unused:    constant Queue_ID := 0;
;;; -------------------
;;; -- If I do the following I get 
;;; -- "no matching procedure/function/task/declare/package"
;;; -- when I do return (I reverse the mappings of ^j and ^m) after "private".
;;; package Package1 is
;;;    package Package1_1 is
;;;       type The_Type is private;
;;;       private
;;; -------------------
;;; -- But what about this:
;;; package G is
;;;    type T1 is new Integer;
;;;    type T2 is new Integer;  --< incorrect, correct if subtype
;;;    package H is
;;;       type T3 is new Integer;
;;;    type                     --< Indentation is incorrect
;;; -------------------



;;; CREDITS
;;; =======
;;;
;;; Many thanks to
;;;    Philippe Warroquiers (PW) <philippe@cfmu.eurocontrol.be> in particular,
;;;    woodruff@stc.llnl.gov (John Woodruff)
;;;    jj@ddci.dk (Jesper Joergensen)
;;;    gse@ocsystems.com (Scott Evans)
;;;    comar@LANG8.CS.NYU.EDU (Cyrille Comar)
;;;    and others for their valuable hints.

;;;--------------------
;;;    USER OPTIONS
;;;--------------------

;; ---- configure indentation

(defvar ada-indent 3
  "*Defines the size of Ada indentation.")

(defvar ada-broken-indent 2
  "*# of columns to indent the continuation of a broken line.")

(defvar ada-label-indent -4
  "*# of columns to indent a label.")

(defvar ada-stmt-end-indent 0
  "*# of columns to indent a statement end keyword in a separate line.
Examples are 'is', 'loop', 'record', ...")

(defvar ada-when-indent 3
  "*Defines the indentation for 'when' relative to 'exception' or 'case'.")

(defvar ada-indent-record-rel-type 3
  "*Defines the indentation for 'record' relative to 'type' or 'use'.")

(defvar ada-indent-comment-as-code t
  "*If non-nil, comment-lines get indented as ada-code.")

(defvar ada-indent-is-separate t
  "*If non-nil, 'is separate' or 'is abstract' on a separate line are
indented.")

(defvar ada-indent-to-open-paren t
  "*If non-nil, following lines get indented according to the innermost
open parenthesis.")

(defvar ada-search-paren-char-count-limit 3000
  "*Search that many characters for an open parenthesis.")


;; ---- other user options

(defvar ada-tab-policy 'indent-auto
  "*Control behaviour of the TAB key.
Must be one of 'indent-rigidly, 'indent-auto, 'gei, 'indent-af or 'always-tab.

'indent-rigidly : always adds ada-indent blanks at the beginning of the line.
'indent-auto    : use indentation functions in this file.
'gei            : use David Kågedal's Generic Indentation Engine.
'indent-af      : use Gary E. Barnes' ada-format.el
'always-tab     : do indent-relative.")

(defvar ada-move-to-declaration nil
  "*If non-nil, ada-move-to-start moves point to the subprog-declaration,
not to 'begin'.")

(defvar ada-spec-suffix ".ads"
  "*Suffix of Ada specification files.")

(defvar ada-body-suffix ".adb"
  "*Suffix of Ada body files.")

(defvar ada-language-version 'ada95
  "*Do we program in 'ada83 or 'ada95?")

(defvar ada-case-keyword 'downcase-word
  "*downcase-word, upcase-word, ada-loose-case-word or capitalize-word
to adjust ada keywords case.")

(defvar ada-case-identifier 'ada-loose-case-word
  "*downcase-word, upcase-word, ada-loose-case-word or capitalize-word
to adjust ada identifier case.")

(defvar ada-case-attribute 'capitalize-word
  "*downcase-word, upcase-word, ada-loose-case-word or capitalize-word
to adjust ada identifier case.")

(defvar ada-auto-case t
  "*Non-nil automatically changes casing of preceeding word while typing.
Casing is done according to ada-case-keyword and ada-case-identifier.")

(defvar ada-clean-buffer-before-saving  nil
  "*If non-nil, remove-trailing-spaces and untabify buffer before saving.")

(defvar ada-mode-hook nil
  "*List of functions to call when Ada Mode is invoked.
This is a good place to add Ada environment specific bindings.")

(defvar ada-external-pretty-print-program "aimap"
  "*External pretty printer to call from within Ada Mode.")

(defvar ada-tmp-directory "/tmp/"
  "*Directory to store the temporary file for the Ada pretty printer.")

(defvar ada-fill-comment-prefix "-- "
  "*This is inserted in the first columns when filling a comment paragraph.")

(defvar ada-fill-comment-postfix " --"
  "*This is inserted at the end of each line when filling a comment paragraph
with ada-fill-comment-paragraph postfix.")

(defvar ada-krunch-args "0"
  "*Argument of gnatk8, a string containing the max number of characters.
Set to 0, if you dont use crunched filenames.")

;;; ---- end of user configurable variables


(defvar ada-mode-abbrev-table nil
  "Abbrev table used in Ada mode.")
(define-abbrev-table 'ada-mode-abbrev-table ())

(defvar ada-mode-map ()
  "Local keymap used for ada-mode.")

(defvar ada-mode-syntax-table nil
  "Syntax table to be used for editing Ada source code.")

(defvar ada-mode-symbol-syntax-table nil
  "Syntax table for Ada, where `_' is a word constituent.")

(defconst ada-83-keywords
  "\\<\\(abort\\|abs\\|accept\\|access\\|all\\|and\\|array\\|\
at\\|begin\\|body\\|case\\|constant\\|declare\\|delay\\|delta\\|\
digits\\|do\\|else\\|elsif\\|end\\|entry\\|exception\\|exit\\|for\\|\
function\\|generic\\|goto\\|if\\|in\\|is\\|limited\\|loop\\|mod\\|\
new\\|not\\|null\\|of\\|or\\|others\\|out\\|package\\|pragma\\|\
private\\|procedure\\|raise\\|range\\|record\\|rem\\|renames\\|\
return\\|reverse\\|select\\|separate\\|subtype\\|task\\|terminate\\|\
then\\|type\\|use\\|when\\|while\\|with\\|xor\\)\\>"
  "regular expression for looking at Ada83 keywords.")

(defconst ada-95-keywords
  "\\<\\(abort\\|abs\\|abstract\\|accept\\|access\\|aliased\\|\
all\\|and\\|array\\|at\\|begin\\|body\\|case\\|constant\\|declare\\|\
delay\\|delta\\|digits\\|do\\|else\\|elsif\\|end\\|entry\\|\
exception\\|exit\\|for\\|function\\|generic\\|goto\\|if\\|in\\|\
is\\|limited\\|loop\\|mod\\|new\\|not\\|null\\|of\\|or\\|others\\|\
out\\|package\\|pragma\\|private\\|procedure\\|protected\\|raise\\|\
range\\|record\\|rem\\|renames\\|requeue\\|return\\|reverse\\|\
select\\|separate\\|subtype\\|tagged\\|task\\|terminate\\|then\\|\
type\\|until\\|use\\|when\\|while\\|with\\|xor\\)\\>"
  "regular expression for looking at Ada95 keywords.")

(defvar ada-keywords ada-95-keywords
  "regular expression for looking at Ada keywords.")

(defvar ada-ret-binding nil
  "Variable to save key binding of RET when casing is activated.")

(defvar ada-lfd-binding nil
  "Variable to save key binding of LFD when casing is activated.")

;;; ---- Regexps to find procedures/functions/packages

(defconst ada-ident-re 
  "[a-zA-Z0-9_\\.]+"
  "Regexp matching Ada identifiers.")

(defvar ada-procedure-start-regexp
  "^[ \t]*\\(procedure\\|function\\|task\\)[ \t\n]+\\([a-zA-Z0-9_\\.]+\\)"
  "Regexp used to find Ada procedures/functions.")

(defvar ada-package-start-regexp
  "^[ \t]*\\(package\\)"
  "Regexp used to find Ada packages")


;;; ---- regexps for indentation functions

(defvar ada-block-start-re
  "\\<\\(begin\\|select\\|declare\\|private\\|or\\|generic\\|\
exception\\|loop\\|else\\|\
\\(\\(limited\\|abstract\\|tagged\\)[ \t]+\\)*record\\)\\>"
  "Regexp for keywords starting ada-blocks.")

(defvar ada-end-stmt-re
  "\\(;\\|=>\\|^[ \t]*separate[ \t]+([a-zA-Z0-9_\\.]+)\\|\
\\<\\(begin\\|else\\|record\\|loop\\|select\\|do\\|\
^[ \t]*package[ \ta-zA-Z0-9_\\.]+is\\|\
^[ \t]*exception\\|declare\\|generic\\|private\\)\\>\\)"
  "Regexp of possible ends for a non-broken statement.
'end' means that there has to start a new statement after these.")

(defvar ada-loop-start-re
  "\\<\\(for\\|while\\|loop\\)\\>"
  "Regexp for the start of a loop.")

(defvar ada-subprog-start-re
  "\\<\\(procedure\\|protected\\|package[ \t]+body\\|function\\|\
task\\|accept\\|entry\\)\\>"
  "Regexp for the start of a subprogram.")


;;;-------------
;;;  functions
;;;-------------

(defun ada-xemacs ()
  (or (string-match "Lucid"  emacs-version)
      (string-match "XEmacs" emacs-version)))

(defun ada-create-syntax-table ()
  "Create the syntax table for ada-mode."
  ;; There are two different syntax-tables.  The standard one declares
  ;; `_' a symbol constituent, in the second one, it is a word
  ;; constituent.  For some search and replacing routines we
  ;; temporarily switch between the two.
  (setq ada-mode-syntax-table (make-syntax-table))
  (set-syntax-table  ada-mode-syntax-table)

  ;; define string brackets (% is alternative string bracket)
  (modify-syntax-entry ?%  "\"" ada-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" ada-mode-syntax-table)

  (modify-syntax-entry ?\#  "$" ada-mode-syntax-table)

  (modify-syntax-entry ?:  "." ada-mode-syntax-table)
  (modify-syntax-entry ?\; "." ada-mode-syntax-table)
  (modify-syntax-entry ?&  "." ada-mode-syntax-table)
  (modify-syntax-entry ?\|  "." ada-mode-syntax-table)
  (modify-syntax-entry ?+  "." ada-mode-syntax-table)
  (modify-syntax-entry ?*  "." ada-mode-syntax-table)
  (modify-syntax-entry ?/  "." ada-mode-syntax-table)
  (modify-syntax-entry ?=  "." ada-mode-syntax-table)
  (modify-syntax-entry ?<  "." ada-mode-syntax-table)
  (modify-syntax-entry ?>  "." ada-mode-syntax-table)
  (modify-syntax-entry ?$ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\[ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\] "." ada-mode-syntax-table)
  (modify-syntax-entry ?\{ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\} "." ada-mode-syntax-table)
  (modify-syntax-entry ?. "." ada-mode-syntax-table)
  (modify-syntax-entry ?\\ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\' "." ada-mode-syntax-table)

  ;; a single hyphen is punctuation, but a double hyphen starts a comment
  (modify-syntax-entry ?-  ". 12" ada-mode-syntax-table)

  ;; and \f and \n end a comment
  (modify-syntax-entry ?\f  ">   " ada-mode-syntax-table)
  (modify-syntax-entry ?\n  ">   " ada-mode-syntax-table)

  ;; define what belongs in ada symbols
  (modify-syntax-entry ?_ "_" ada-mode-syntax-table)

  ;; define parentheses to match
  (modify-syntax-entry ?\( "()" ada-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" ada-mode-syntax-table)

  (setq ada-mode-symbol-syntax-table (copy-syntax-table ada-mode-syntax-table))
  (modify-syntax-entry ?_ "w" ada-mode-symbol-syntax-table)
  )


;;;###autoload
(defun ada-mode ()
  "Ada Mode is the major mode for editing Ada code.

Bindings are as follows: (Note: 'LFD' is control-j.)

 Indent line                                          '\\[ada-tab]'
 Indent line, insert newline and indent the new line. '\\[newline-and-indent]'

 Re-format the parameter-list point is in             '\\[ada-format-paramlist]'
 Indent all lines in region                           '\\[ada-indent-region]'
 Call external pretty printer program                 '\\[ada-call-pretty-printer]'

 Adjust case of identifiers and keywords in region    '\\[ada-adjust-case-region]'
 Adjust case of identifiers and keywords in buffer    '\\[ada-adjust-case-buffer]'

 Call EXTERNAL pretty printer (if you have one)       '\\[ada-call-pretty-printer]'

 Fill comment paragraph                               '\\[ada-fill-comment-paragraph]'
 Fill comment paragraph and justify each line         '\\[ada-fill-comment-paragraph-justify]'
 Fill comment paragraph, justify and append postfix   '\\[ada-fill-comment-paragraph-postfix]'

 Next func/proc/task '\\[ada-next-procedure]'    Previous func/proc/task '\\[ada-previous-procedure]'
 Next package        '\\[ada-next-package]'  Previous package        '\\[ada-previous-package]'

 Goto matching start of current 'end ...;'            '\\[ada-move-to-start]'
 Goto end of current block                            '\\[ada-move-to-end]'

Comments are handled using standard GNU Emacs conventions, including:
 Start a comment                                      '\\[indent-for-comment]'
 Comment region                                       '\\[comment-region]'
 Uncomment region                                     '\\[ada-uncomment-region]'
 Continue comment on next line                        '\\[indent-new-comment-line]'

If you use imenu.el:
 Display index-menu of functions & procedures         '\\[imenu]'

If you use find-file.el:
 Switch to other file (Body <-> Spec)                 '\\[ff-find-other-file]'
                                                   or '\\[ff-mouse-find-other-file]
 Switch to other file in other window                 '\\[ada-ff-other-window]'
                                                   or '\\[ff-mouse-find-other-file-other-window]
 If you use this function in a spec and no body is available, it gets created
 with body stubs.

If you use ada-xref.el:
 Goto declaration:          '\\[ada-point-and-xref]' on the identifier
                         or '\\[ada-goto-declaration]' with point on the identifier
 Complete identifier:       '\\[ada-complete-identifier]'
 Execute Gnatf:             '\\[ada-gnatf-current]'"

  (interactive)
  (kill-all-local-variables)

  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)

  (make-local-variable 'comment-start)
  (setq comment-start "-- ")

  ;; comment end must be set because it may hold a wrong value if
  ;; this buffer had been in another mode before. RE
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'comment-start-skip) ;; used by autofill
  (setq comment-start-skip "--+[ \t]*")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ada-indent-current-function)

  (make-local-variable 'fill-column)
  (setq fill-column 75)

  (make-local-variable 'comment-column)
  (setq comment-column 40)

  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)

  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'ada-fill-comment-paragraph)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ada-font-lock-keywords nil t ((?\_ . "w"))))

  (setq major-mode 'ada-mode)
  (setq mode-name "Ada")

  (setq blink-matching-paren t)

  (use-local-map ada-mode-map)

  (if ada-mode-syntax-table
      (set-syntax-table ada-mode-syntax-table)
    (ada-create-syntax-table))

  (if ada-clean-buffer-before-saving
      (progn
	;; remove all spaces at the end of lines in the whole buffer.
	(add-hook 'local-write-file-hooks 'ada-remove-trailing-spaces)
	;; convert all tabs to the correct number of spaces.
	(add-hook 'local-write-file-hooks 'ada-untabify-buffer)))


  ;; add menu 'Ada' to the menu bar
  (ada-add-ada-menu)

  (run-hooks 'ada-mode-hook)

  ;; the following has to be done after running the ada-mode-hook
  ;; because users might want to set the values of these variable
  ;; inside the hook (MH)

  (cond ((eq ada-language-version 'ada83)
         (setq ada-keywords ada-83-keywords))
        ((eq ada-language-version 'ada95)
         (setq ada-keywords ada-95-keywords)))

  (if ada-auto-case
      (ada-activate-keys-for-case)))


;;;--------------------------
;;;  Fill Comment Paragraph
;;;--------------------------

(defun ada-fill-comment-paragraph-justify ()
  "Fills current comment paragraph and justifies each line as well."
  (interactive)
  (ada-fill-comment-paragraph t))


(defun ada-fill-comment-paragraph-postfix ()
  "Fills current comment paragraph and justifies each line as well.
Prompts for a postfix to be appended to each line."
  (interactive)
  (ada-fill-comment-paragraph t t))


(defun ada-fill-comment-paragraph (&optional justify postfix)
  "Fills the current comment paragraph.
If JUSTIFY is non-nil, each line is justified as well.
If POSTFIX and JUSTIFY are  non-nil, ada-fill-comment-postfix is appended
to each filled and justified line.
If ada-indent-comment-as code is non-nil, the paragraph is idented."
  (interactive "P")
  (let ((opos (point-marker))
        (begin nil)
        (end nil)
        (end-2 nil)
        (indent nil)
        (ada-fill-comment-old-postfix "")
        (fill-prefix nil))

    ;; check if inside comment
    (if (not (ada-in-comment-p))
        (error "not inside comment"))

    ;; prompt for postfix if wanted
    (if (and justify
             postfix)
        (setq ada-fill-comment-postfix
              (read-from-minibuffer "enter new postfix string: "
                                    ada-fill-comment-postfix)))

    ;; prompt for old postfix to remove if necessary
    (if (and justify
             postfix)
        (setq ada-fill-comment-old-postfix
              (read-from-minibuffer "enter already existing postfix string: "
                                    ada-fill-comment-postfix)))

    ;;
    ;; find limits of paragraph
    ;;
    (message "filling comment paragraph ...")
    (save-excursion
      (back-to-indentation)
      ;; find end of paragraph
      (while (and (looking-at "--.*$")
                  (not (looking-at "--[ \t]*$")))
        (forward-line 1)
        (back-to-indentation))
      (beginning-of-line)
      (setq end (point-marker))
      (goto-char opos)
      ;; find begin of paragraph
      (back-to-indentation)
      (while (and (looking-at "--.*$")
                  (not (looking-at "--[ \t]*$")))
        (forward-line -1)
        (back-to-indentation))
      (forward-line 1)
      ;; get indentation to calculate width for filling
      (ada-indent-current)
      (back-to-indentation)
      (setq indent (current-column))
      (setq begin (point-marker)))

    ;; delete old postfix if necessary
    (if (and justify
             postfix)
        (save-excursion
          (goto-char begin)
          (while (re-search-forward (concat ada-fill-comment-old-postfix
                                            "\n")
                                    end t)
            (replace-match "\n"))))

    ;; delete leading whitespace and uncomment
    (save-excursion
      (goto-char begin)
      (beginning-of-line)
      (while (re-search-forward "^[ \t]*--[ \t]*" end t)
        (replace-match "")))

    ;; calculate fill width
    (setq fill-column (- fill-column indent
                         (length ada-fill-comment-prefix)
                         (if postfix
                             (length ada-fill-comment-postfix)
                           0)))
    ;; fill paragraph
    (fill-region begin (1- end) justify)
    (setq fill-column (+ fill-column indent
                         (length ada-fill-comment-prefix)
                         (if postfix
                             (length ada-fill-comment-postfix)
                           0)))
   ;; find end of second last line
    (save-excursion
      (goto-char end)
      (forward-line -2)
      (end-of-line)
      (setq end-2 (point-marker)))

    ;; re-comment and re-indent region
    (save-excursion
      (goto-char begin)
      (indent-to indent)
      (insert ada-fill-comment-prefix)
      (while (re-search-forward "\n" (1- end-2) t)
        (replace-match (concat "\n" ada-fill-comment-prefix))
        (beginning-of-line)
        (indent-to indent)))

    ;; append postfix if wanted
    (if (and justify
             postfix
             ada-fill-comment-postfix)
        (progn
          ;; append postfix up to there
          (save-excursion
            (goto-char begin)
            (while (re-search-forward "\n" (1- end-2) t)
              (replace-match (concat ada-fill-comment-postfix "\n")))

            ;; fill last line and append postfix
            (end-of-line)
            (insert-char ?
                         (- fill-column
                            (current-column)
                            (length ada-fill-comment-postfix)))
            (insert ada-fill-comment-postfix))))

    ;; delete the extra line that gets inserted somehow(??)
    (save-excursion
      (goto-char (1- end))
      (end-of-line)
      (delete-char 1))

     (message "filling comment paragraph ... done")
    (goto-char opos))
  t)


;;;--------------------------------;;;
;;;  Call External Pretty Printer  ;;;
;;;--------------------------------;;;

(defun ada-call-pretty-printer ()
  "Calls the external Pretty Printer.
The name is specified in ada-external-pretty-print-program.  Saves the
current buffer in a directory specified by ada-tmp-directory,
starts the Pretty Printer as external process on that file and then
reloads the beautyfied program in the buffer and cleans up
ada-tmp-directory."
  (interactive)
  (let ((filename-with-path buffer-file-name)
        (curbuf (current-buffer))
        (orgpos (point))
        (mesgbuf nil) ;; for byte-compiling
        (file-path (file-name-directory buffer-file-name))
        (filename-without-path (file-name-nondirectory buffer-file-name))
        (tmp-file-with-directory
         (concat ada-tmp-directory
                 (file-name-nondirectory buffer-file-name))))
    ;;
    ;; save buffer in temporary file
    ;;
    (message "saving current buffer to temporary file ...")
    (write-file tmp-file-with-directory)
    (auto-save-mode nil)
    (message "saving current buffer to temporary file ... done")
    ;;
    ;; call external pretty printer program
    ;;

    (message "running external pretty printer ...")
    ;; create a temporary buffer for messages of pretty printer
    (setq mesgbuf (get-buffer-create "Pretty Printer Messages"))
    ;; execute pretty printer on temporary file
    (call-process ada-external-pretty-print-program
                  nil mesgbuf t
                  tmp-file-with-directory)
    ;; display messages if there are some
    (if (buffer-modified-p mesgbuf)
        ;; show the message buffer
        (display-buffer mesgbuf t)
      ;; kill the message buffer
      (kill-buffer mesgbuf))
    (message "running external pretty printer ... done")
    ;;
    ;; kill current buffer and load pretty printer output
    ;; or restore old buffer
    ;;
    (if (y-or-n-p
         "Really replace current buffer with pretty printer output ? ")
        (progn
          (set-buffer-modified-p nil)
          (kill-buffer curbuf)
          (find-file tmp-file-with-directory))
      (message "old buffer contents restored"))
    ;;
    ;; delete temporary file and restore information of current buffer
    ;;
    (delete-file tmp-file-with-directory)
    (set-visited-file-name filename-with-path)
    (auto-save-mode t)
    (goto-char orgpos)))


;;;---------------
;;;  auto-casing
;;;---------------

;; from Philippe Waroquiers <philippe@cfmu.eurocontrol.be>
;; modifiedby RE and MH

(defun ada-after-keyword-p ()
  ;; returns t if cursor is after a keyword.
  (save-excursion
    (forward-word -1)
    (and (save-excursion
           (or
            (= (point) (point-min))
            (backward-char 1))
           (not (looking-at "_")))     ; (MH)
         (looking-at (concat ada-keywords "[^_]")))))

(defun ada-after-char-p ()
  ;; returns t if after ada character "'". This is interpreted as being
  ;; in a character constant.
  (save-excursion
    (if (> (point) 2)
        (progn
          (forward-char -2)
          (looking-at "'"))
      nil)))


(defun ada-adjust-case (&optional force-identifier)
  "Adjust the case of the word before the just-typed character,
according to ada-case-keyword and ada-case-identifier
If FORCE-IDENTIFIER is non-nil then also adjust keyword as
identifier." ; (MH)
  (forward-char -1)
  (if (and (> (point) 1) (not (or (ada-in-string-p)
                                  (ada-in-comment-p)
                                  (ada-after-char-p))))
      (if (eq (char-syntax (char-after (1- (point)))) ?w)
	  (if (save-excursion
		(forward-word -1)
		(or (= (point) (point-min))
		    (backward-char 1))
		(looking-at "'"))
	      (funcall ada-case-attribute -1)
	    (if (and
		 (not force-identifier) ; (MH)
		 (ada-after-keyword-p))
		(funcall ada-case-keyword -1)
	      (funcall ada-case-identifier -1)))))
  (forward-char 1))


(defun ada-adjust-case-interactive (arg)
  (interactive "P")
  (let ((lastk last-command-char))
    (cond ((or (eq lastk ?\n)
               (eq lastk ?\r))
           ;; horrible kludge
           (insert " ")
           (ada-adjust-case)
           ;; horrible dekludge
           (delete-backward-char 1)
           ;; some special keys and their bindings
           (cond
            ((eq lastk ?\n)
             (funcall ada-lfd-binding))
            ((eq lastk ?\r)
             (funcall ada-ret-binding))))
          ((eq lastk ?\C-i) (ada-tab))
          ((self-insert-command (prefix-numeric-value arg))))
    ;; if there is a keyword in front of the underscore
    ;; then it should be part of an identifier (MH)
    (if (eq lastk ?_)
        (ada-adjust-case t)
      (ada-adjust-case))))


(defun ada-activate-keys-for-case ()
  ;; save original keybindings to allow swapping ret/lfd
  ;; when casing is activated
  ;; the 'or ...' is there to be sure that the value will not
  ;; be changed again when ada-mode is called more than once (MH)
  (or ada-ret-binding
      (setq ada-ret-binding (key-binding "\C-M")))
  (or ada-lfd-binding
      (setq ada-lfd-binding (key-binding "\C-j")))
  ;; call case modifying function after certain keys.
  (mapcar (function (lambda(key) (define-key
                                   ada-mode-map
                                   (char-to-string key)
                                   'ada-adjust-case-interactive)))
          '( ?` ?~ ?! ?@ ?# ?$ ?% ?^ ?& ?* ?( ?)  ?- ?= ?+ ?[ ?{ ?] ?}
                ?_ ?\\ ?| ?\; ?: ?' ?\" ?< ?, ?. ?> ?? ?/ ?\n 32 ?\r )))
;; deleted ?\t from above list

;;
;; added by MH
;;
(defun ada-loose-case-word (&optional arg)
  "Capitalizes the first and the letters following _
ARG is ignored, it's there to fit the standard casing functions' style."
  (let ((pos (point))
        (first t))
    (skip-chars-backward "a-zA-Z0-9_")
    (while (or first
               (search-forward "_" pos t))
      (and first
           (setq first nil))
      (insert-char (upcase (following-char)) 1)
      (delete-char 1))
    (goto-char pos)))


;;
;; added by MH
;;
(defun ada-adjust-case-region (from to)
  "Adjusts the case of all identifiers and keywords in the region.
ATTENTION: This function might take very long for big regions !"
  (interactive "*r")
  (let ((begin nil)
        (end nil)
        (keywordp nil)
        (reldiff nil))
    (unwind-protect
	(save-excursion
	  (set-syntax-table ada-mode-symbol-syntax-table)
	  (goto-char to)
	  ;;
	  ;; loop: look for all identifiers and keywords
	  ;;
	  (while (re-search-backward
		  "[^a-zA-Z0-9_]\\([a-zA-Z0-9_]+\\)[^a-zA-Z0-9_]"
		  from
		  t)
	    ;;
	    ;; print status message
	    ;;
	    (setq reldiff (- (point) from))
	    (message (format "adjusting case ... %5d characters left"
			     (- (point) from)))
	    (forward-char 1)
	    (or
	     ;; do nothing if it is a string or comment
	     (ada-in-string-or-comment-p)
	     (progn
	       ;;
	       ;; get the identifier or keyword
	       ;;
	       (setq begin (point))
	       (setq keywordp (looking-at (concat ada-keywords "[^_]")))
	       (skip-chars-forward "a-zA-Z0-9_")
	       ;;
	       ;; casing according to user-option
	       ;;
	       (if keywordp
		   (funcall ada-case-keyword -1)
		 (funcall ada-case-identifier -1))
	       (goto-char begin))))
	  (message "adjusting case ... done"))
      (set-syntax-table ada-mode-syntax-table))))


;;
;; added by MH
;;
(defun ada-adjust-case-buffer ()
  "Adjusts the case of all identifiers and keywords in the whole buffer.
ATTENTION: This function might take very long for big buffers !"
  (interactive "*")
  (ada-adjust-case-region (point-min) (point-max)))


;;;------------------------;;;
;;; Format Parameter Lists ;;;
;;;------------------------;;;

(defun ada-format-paramlist ()
  "Re-formats a parameter-list.
ATTENTION:  1) Comments inside the list are killed !
            2) If the syntax is not correct (especially, if there are
               semicolons missing), it can get totally confused !
In such a case, use 'undo', correct the syntax and try again."

  (interactive)
  (let ((begin nil)
        (end nil)
        (delend nil)
        (paramlist nil))
    (unwind-protect
	(progn 
	  (set-syntax-table ada-mode-symbol-syntax-table)

	  ;; check if really inside parameter list
	  (or (ada-in-paramlist-p)
	      (error "not in parameter list"))
	  ;;
	  ;; find start of current parameter-list
	  ;;
	  (ada-search-ignore-string-comment
	   (concat "\\<\\("
		   "procedure\\|function\\|body\\|package\\|task\\|entry\\|accept"
		   "\\)\\>") t nil)
	  (ada-search-ignore-string-comment "(" nil nil t)
	  (backward-char 1)
	  (setq begin (point))

	  ;;
	  ;; find end of parameter-list
	  ;;
	  (forward-sexp 1)
	  (setq delend (point))
	  (delete-char -1)

	  ;;
	  ;; find end of last parameter-declaration
	  ;;
	  (ada-search-ignore-string-comment "[^ \t\n]" t nil t)
	  (forward-char 1)
	  (setq end (point))

	  ;;
	  ;; build a list of all elements of the parameter-list
	  ;;
	  (setq paramlist (ada-scan-paramlist (1+ begin) end))

	  ;;
	  ;; delete the original parameter-list
	  ;;
	  (delete-region begin (1- delend))

	  ;;
	  ;; insert the new parameter-list
	  ;;
	  (goto-char begin)
	  (ada-insert-paramlist paramlist))

      ;;
      ;; restore syntax-table
      ;;
      (set-syntax-table ada-mode-syntax-table)
      )))


(defun ada-scan-paramlist (begin end)
  ;; Scans a parameter-list  between BEGIN and END and returns a list
  ;; of its contents.
  ;; The list has the following format:
  ;;
  ;;   Name of Param  in? out? accept?  Name of Type   Default-Exp or nil
  ;;
  ;; ( ('Name_Param_1' t   nil    t      Type_Param_1   ':= expression')
  ;;   ('Name_Param_2' nil nil    t      Type_Param_2    nil) )

  (let ((paramlist (list))
        (param (list))
        (notend t)
        (apos nil)
        (epos nil)
        (semipos nil)
        (match-cons nil))

    (goto-char begin)
    ;;
    ;; loop until end of last parameter
    ;;
    (while notend

      ;;
      ;; find first character of parameter-declaration
      ;;
      (ada-goto-next-non-ws)
      (setq apos (point))

      ;;
      ;; find last character of parameter-declaration
      ;;
      (if (setq match-cons
                (ada-search-ignore-string-comment "[ \t\n]*;" nil end t))
          (progn
            (setq epos (car match-cons))
            (setq semipos (cdr match-cons)))
        (setq epos end))

      ;;
      ;; read name(s) of parameter(s)
      ;;
      (goto-char apos)
      (looking-at "\\([a-zA-Z0-9_, \t\n]*[a-zA-Z0-9_]\\)[ \t\n]*:[^=]")

      (setq param (list (buffer-substring (match-beginning 1)
                                          (match-end 1))))
      (ada-search-ignore-string-comment ":" nil epos t)

      ;;
      ;; look for 'in'
      ;;
      (setq apos (point))
      (setq param
            (append param
                    (list
                     (consp
                      (ada-search-ignore-string-comment "\\<in\\>"
                                                        nil
                                                        epos
                                                        t)))))

      ;;
      ;; look for 'out'
      ;;
      (goto-char apos)
      (setq param
            (append param
                    (list
                     (consp
                      (ada-search-ignore-string-comment "\\<out\\>"
                                                        nil
                                                        epos
                                                        t)))))

      ;;
      ;; look for 'accept'
      ;;
      (goto-char apos)
      (setq param
            (append param
                    (list
                     (consp
                      (ada-search-ignore-string-comment "\\<accept\\>"
                                                        nil
                                                        epos
                                                        t)))))

      ;;
      ;; skip 'in'/'out'/'accept'
      ;;
      (goto-char apos)
      (ada-goto-next-non-ws)
      (while (looking-at "\\<\\(in\\|out\\|accept\\)\\>")
        (forward-word 1)
        (ada-goto-next-non-ws))

      ;;
      ;; read type of parameter
      ;;
      (looking-at "\\<[a-zA-Z0-9_\\.]+\\>")
      (setq param
            (append param
                    (list
                     (buffer-substring (match-beginning 0)
                                       (match-end 0)))))

      ;;
      ;; read default-expression, if there is one
      ;;
      (goto-char (setq apos (match-end 0)))
      (setq param
            (append param
                    (list
                     (if (setq match-cons
                               (ada-search-ignore-string-comment ":="
                                                                 nil
                                                                 epos
                                                                 t))
                         (buffer-substring (car match-cons)
                                           epos)
                       nil))))
      ;;
      ;; add this parameter-declaration to the list
      ;;
      (setq paramlist (append paramlist (list param)))

      ;;
      ;; check if it was the last parameter
      ;;
      (if (eq epos end)
          (setq notend nil)
        (goto-char semipos))

      ) ; end of loop

    (reverse paramlist)))


(defun ada-insert-paramlist (paramlist)
  ;; Inserts a formatted PARAMLIST in the buffer.
  ;; See doc of ada-scan-paramlist for the format.
  (let ((i (length paramlist))
        (parlen 0)
        (typlen 0)
        (temp 0)
        (inp nil)
        (outp nil)
        (acceptp nil)
        (column nil)
        (orgpoint 0)
        (firstcol nil))

    ;;
    ;; loop until last parameter
    ;;
    (while (not (zerop i))
      (setq i (1- i))

      ;;
      ;; get max length of parameter-name
      ;;
      (setq parlen
            (if (<= parlen (setq temp
                              (length (nth 0 (nth i paramlist)))))
                temp
              parlen))

      ;;
      ;; get max length of type-name
      ;;
      (setq typlen
            (if (<= typlen (setq temp
                              (length (nth 4 (nth i paramlist)))))
                temp
              typlen))

      ;;
      ;; is there any 'in' ?
      ;;
      (setq inp
            (or inp
                (nth 1 (nth i paramlist))))

      ;;
      ;; is there any 'out' ?
      ;;
      (setq outp
            (or outp
                (nth 2 (nth i paramlist))))

      ;;
      ;; is there any 'accept' ?
      ;;
      (setq acceptp
            (or acceptp
                (nth 3 (nth i paramlist))))) ; end of loop

    ;;
    ;; does paramlist already start on a separate line ?
    ;;
    (if (save-excursion
          (re-search-backward "^.\\|[^ \t]" nil t)
          (looking-at "^."))
        ;; yes => re-indent it
        (ada-indent-current)
      ;;
      ;; no => insert newline and indent it
      ;;
      (progn
        (ada-indent-current)
        (newline)
        (delete-horizontal-space)
        (setq orgpoint (point))
        (setq column (save-excursion
                       (funcall (ada-indent-function) orgpoint)))
        (indent-to column)
        ))

    (insert "(")

    (setq firstcol (current-column))
    (setq i (length paramlist))

    ;;
    ;; loop until last parameter
    ;;
    (while (not (zerop i))
      (setq i (1- i))
      (setq column firstcol)

      ;;
      ;; insert parameter-name, space and colon
      ;;
      (insert (nth 0 (nth i paramlist)))
      (indent-to (+ column parlen 1))
      (insert ": ")
      (setq column (current-column))

      ;;
      ;; insert 'in' or space
      ;;
      (if (nth 1 (nth i paramlist))
          (insert "in ")
        (if (and
             (or inp
                 acceptp)
             (not (nth 3 (nth i paramlist))))
            (insert "   ")))

      ;;
      ;; insert 'out' or space
      ;;
      (if (nth 2 (nth i paramlist))
          (insert "out ")
        (if (and
             (or outp
                 acceptp)
             (not (nth 3 (nth i paramlist))))
            (insert "    ")))

      ;;
      ;; insert 'accept'
      ;;
      (if (nth 3 (nth i paramlist))
          (insert "accept "))

      (setq column (current-column))

      ;;
      ;; insert type-name and, if necessary, space and default-expression
      ;;
      (insert (nth 4 (nth i paramlist)))
      (if (nth 5 (nth i paramlist))
          (progn
            (indent-to (+ column typlen 1))
            (insert (nth 5 (nth i paramlist)))))

      ;;
      ;; check if it was the last parameter
      ;;
      (if (not (zerop i))
          ;; no => insert ';' and newline and indent
          (progn
            (insert ";")
            (newline)
            (indent-to firstcol))
        ;; yes
        (insert ")"))

      ) ; end of loop

    ;;
    ;; if anything follows, except semicolon:
    ;; put it in a new line and indent it
    ;;
    (if (not (looking-at "[ \t]*[;\n]"))
        (ada-indent-newline-indent))

    ))


;;;----------------------------;;;
;;; Move To Matching Start/End ;;;
;;;----------------------------;;;

(defun ada-move-to-start ()
  "Moves point to the matching start of the current end ... around point."
  (interactive)
  (let ((pos (point)))
    (unwind-protect
	(progn
	  (set-syntax-table ada-mode-symbol-syntax-table)

	  (message "searching for block start ...")
	  (save-excursion
	    ;;
	    ;; do nothing if in string or comment or not on 'end ...;'
	    ;;            or if an error occurs during processing
	    ;;
	    (or
	     (ada-in-string-or-comment-p)
	     (and (progn
		    (or (looking-at "[ \t]*\\<end\\>")
			(backward-word 1))
		    (or (looking-at "[ \t]*\\<end\\>")
			(backward-word 1))
		    (or (looking-at "[ \t]*\\<end\\>")
			(error "not on end ...;")))
		  (ada-goto-matching-start 1)
		  (setq pos (point))

		  ;;
		  ;; on 'begin' => go on, according to user option
		  ;;
		  ada-move-to-declaration
		  (looking-at "\\<begin\\>")
		  (ada-goto-matching-decl-start)
		  (setq pos (point))))

	    ) ; end of save-excursion

	  ;; now really move to the found position
	  (goto-char pos)
	  (message "searching for block start ... done"))

      ;;
      ;; restore syntax-table
      ;;
      (set-syntax-table ada-mode-syntax-table))))


(defun ada-move-to-end ()
  "Moves point to the matching end of the current block around point.
Moves to 'begin' if in a declarative part."
  (interactive)
  (let ((pos (point))
        (decstart nil)
        (packdecl nil))
    (unwind-protect
	(progn
	  (set-syntax-table ada-mode-symbol-syntax-table)

	  (message "searching for block end ...")
	  (save-excursion

	    (forward-char 1)
	    (cond
	     ;; directly on 'begin'
	     ((save-excursion
		(ada-goto-previous-word)
		(looking-at "\\<begin\\>"))
	      (ada-goto-matching-end 1))
	     ;; on first line of defun declaration
	     ((save-excursion
		(and (ada-goto-stmt-start)
		     (looking-at "\\<function\\>\\|\\<procedure\\>" )))
	      (ada-search-ignore-string-comment "\\<begin\\>"))
	     ;; on first line of task declaration
	     ((save-excursion
		(and (ada-goto-stmt-start)
		     (looking-at "\\<task\\>" )
		     (forward-word 1)
		     (ada-search-ignore-string-comment "[^ \n\t]")
		     (not (backward-char 1))
		     (looking-at "\\<body\\>")))
	      (ada-search-ignore-string-comment "\\<begin\\>"))
	     ;; accept block start
	     ((save-excursion
		(and (ada-goto-stmt-start)
		     (looking-at "\\<accept\\>" )))
	      (ada-goto-matching-end 0))
	     ;; package start
	     ((save-excursion
		(and (ada-goto-matching-decl-start t)
		     (looking-at "\\<package\\>")))
	      (ada-goto-matching-end 1))
	     ;; inside a 'begin' ... 'end' block
	     ((save-excursion
		(ada-goto-matching-decl-start t))
	      (ada-search-ignore-string-comment "\\<begin\\>"))
	     ;; (hopefully ;-) everything else
	     (t
	      (ada-goto-matching-end 1)))
	    (setq pos (point))

	    ) ; end of save-excursion

	  ;; now really move to the found position
	  (goto-char pos)
	  (message "searching for block end ... done"))
      
      ;;
      ;; restore syntax-table
      ;;
      (set-syntax-table ada-mode-syntax-table))))


;;;-----------------------------;;;
;;;  Functions For Indentation  ;;;
;;;-----------------------------;;;

;; ---- main functions for indentation

(defun ada-indent-region (beg end)
  "Indents the region using ada-indent-current on each line."
  (interactive "*r")
  (goto-char beg)
  (let ((block-done 0)
	(lines-remaining (count-lines beg end))
	(msg (format "indenting %4d lines %%4d lines remaining ..."
		     (count-lines beg end)))
        (endmark (copy-marker end)))
    ;; catch errors while indenting
    (condition-case err
        (while (< (point) endmark)
          (if (> block-done 9)
              (progn (message (format msg lines-remaining))
                     (setq block-done 0)))
	  (if (looking-at "^$") nil
	    (ada-indent-current))
          (forward-line 1)
	  (setq block-done (1+ block-done))
	  (setq lines-remaining (1- lines-remaining)))
      ;; show line number where the error occured
      (error
       (error (format "line %d: %s"
                      (1+ (count-lines (point-min) (point)))
                      err) nil)))
    (message "indenting ... done")))


(defun ada-indent-newline-indent ()
  "Indents the current line, inserts a newline and then indents the new line."
  (interactive "*")
  (let ((column)
        (orgpoint))

    (ada-indent-current)
    (newline)
    (delete-horizontal-space)
    (setq orgpoint (point))

    (unwind-protect
	(progn
	  (set-syntax-table ada-mode-symbol-syntax-table)

	  (setq column (save-excursion
			 (funcall (ada-indent-function) orgpoint))))

      ;;
      ;; restore syntax-table
      ;;
      (set-syntax-table ada-mode-syntax-table))

    (indent-to column)

    ;; The following is needed to ensure that indentation will still be
    ;; correct if something follows behind point when typing LFD
    ;; For example: Imagine point to be there (*) when LFD is typed:
    ;;              while cond loop
    ;;                 null; *end loop;
    ;; Result without the following statement would be:
    ;;              while cond loop
    ;;                 null;
    ;;                *end loop;
    ;; You would then have to type TAB to correct it.
    ;; If that doesn't bother you, you can comment out the following
    ;; statement to speed up indentation a LITTLE bit.

    (if (not (looking-at "[ \t]*$"))
        (ada-indent-current))
    ))


(defun ada-indent-current ()
  "Indents current line as Ada code.
This works by two steps:
 1) It moves point to the end of the previous code-line.
    Then it calls the function to calculate the indentation for the
    following line as if a newline would be inserted there.
    The calculated column # is saved and the old position of point
    is restored.
 2) Then another function is called to calculate the indentation for
    the current line, based on the previously calculated column #."

  (interactive)

  (unwind-protect
      (progn
	(set-syntax-table ada-mode-symbol-syntax-table)

	(let ((line-end)
	      (orgpoint (point-marker))
	      (cur-indent)
	      (prev-indent)
	      (prevline t))

	  ;;
	  ;; first step
	  ;;
	  (save-excursion
	    (if (ada-goto-prev-nonblank-line t)
		;;
		;; we are not in the first accessible line in the buffer
		;;
		(progn
		  ;;(end-of-line)
		  ;;(forward-char 1)
		  ;; we are already at the BOL
		  (forward-line 1)
		  (setq line-end (point))
		  (setq prev-indent
			(save-excursion
			  (funcall (ada-indent-function) line-end))))
	      (setq prevline nil)))

	  (if prevline
	      ;;
	      ;; we are not in the first accessible line in the buffer
	      ;;
	      (progn
		;;
		;; second step
		;;
		(back-to-indentation)
		(setq cur-indent (ada-get-current-indent prev-indent))
		(delete-horizontal-space)
		(indent-to cur-indent)

		;;
		;; restore position of point
		;;
		(goto-char orgpoint)
		(if (< (current-column) (current-indentation))
              (back-to-indentation))))))

    ;;
    ;; restore syntax-table
    ;;
    (set-syntax-table ada-mode-syntax-table)))


(defun ada-get-current-indent (prev-indent)
  ;; Returns the column # to indent the current line to.
  ;; PREV-INDENT is the indentation resulting from the previous lines.
  (let ((column nil)
        (pos nil)
        (match-cons nil))

    (cond
     ;;
     ;; in open parenthesis, but not in parameter-list
     ;;
     ((and
       ada-indent-to-open-paren
       (not (ada-in-paramlist-p))
       (setq column (ada-in-open-paren-p)))
      ;; check if we have something like this  (Table_Component_Type =>
      ;;                                          Source_File_Record,)
      (save-excursion
        (if (and (ada-search-ignore-string-comment "[^ \t]" t nil)
                 (looking-at "\n")
                 (ada-search-ignore-string-comment "[^ \t\n]" t nil)
                 (looking-at ">"))
            (setq column (+ ada-broken-indent column))))
      column)

     ;;
     ;; end
     ;;
     ((looking-at "\\<end\\>")
      (save-excursion
        (ada-goto-matching-start 1)

        ;;
        ;; found 'loop' => skip back to 'while' or 'for'
        ;;                 if 'loop' is not on a separate line
        ;;
        (if (and
             (looking-at "\\<loop\\>")
             (save-excursion
               (back-to-indentation)
               (not (looking-at "\\<loop\\>"))))
            (if (save-excursion
                  (and
                   (setq match-cons
                         (ada-search-ignore-string-comment
                          ada-loop-start-re t nil))
                   (not (looking-at "\\<loop\\>"))))
                (goto-char (car match-cons))))

        (current-indentation)))
     ;;
     ;; exception
     ;;
     ((looking-at "\\<exception\\>")
      (save-excursion
        (ada-goto-matching-start 1)
        (current-indentation)))
     ;;
     ;; when
     ;;
     ((looking-at "\\<when\\>")
      (save-excursion
        (ada-goto-matching-start 1)
        (+ (current-indentation) ada-when-indent)))
     ;;
     ;; else
     ;;
     ((looking-at "\\<else\\>")
      (if (save-excursion
            (ada-goto-previous-word)
            (looking-at "\\<or\\>"))
          prev-indent
        (save-excursion
          (ada-goto-matching-start 1 nil t)
          (current-indentation))))
     ;;
     ;; elsif
     ;;
     ((looking-at "\\<elsif\\>")
      (save-excursion
        (ada-goto-matching-start 1 nil t)
        (current-indentation)))
     ;;
     ;; then
     ;;
     ((looking-at "\\<then\\>")
      (if (save-excursion
            (ada-goto-previous-word)
            (looking-at "\\<and\\>"))
          prev-indent
        (save-excursion
          (ada-search-ignore-string-comment "\\<elsif\\>\\|\\<if\\>" t nil)
          (+ (current-indentation) ada-stmt-end-indent))))
     ;;
     ;; loop
     ;;
     ((looking-at "\\<loop\\>")
      (setq pos (point))
      (save-excursion
        (goto-char (match-end 0))
        (ada-goto-stmt-start)
        (if (looking-at "\\<loop\\>\\|\\<if\\>")
            prev-indent
          (progn
            (if (not (looking-at ada-loop-start-re))
                (ada-search-ignore-string-comment ada-loop-start-re
                                                  nil pos))
            (if (looking-at "\\<loop\\>")
                prev-indent
              (+ (current-indentation) ada-stmt-end-indent))))))
     ;;
     ;; begin
     ;;
     ((looking-at "\\<begin\\>")
      (save-excursion
        (if (ada-goto-matching-decl-start t)
            (current-indentation)
          (progn
            (message "no matching declaration start")
            prev-indent))))
     ;;
     ;; is
     ;;
     ((looking-at "\\<is\\>")
      (if (and
           ada-indent-is-separate
           (save-excursion
             (goto-char (match-end 0))
             (ada-goto-next-non-ws (save-excursion
                                     (end-of-line)
                                     (point)))
             (looking-at "\\<abstract\\>\\|\\<separate\\>")))
          (save-excursion
            (ada-goto-stmt-start)
            (+ (current-indentation) ada-indent))
        (save-excursion
          (ada-goto-stmt-start)
          (+ (current-indentation) ada-stmt-end-indent))))
     ;;
     ;; record
     ;;
     ((looking-at "\\<record\\>")
      (save-excursion
        (ada-search-ignore-string-comment
         "\\<\\(type\\|use\\)\\>" t nil)
        (if (looking-at "\\<use\\>")
            (ada-search-ignore-string-comment "\\<for\\>" t nil))
        (+ (current-indentation) ada-indent-record-rel-type)))
     ;;
     ;; or as statement-start
     ;;
     ((ada-looking-at-semi-or)
      (save-excursion
        (ada-goto-matching-start 1)
        (current-indentation)))
     ;;
     ;; private as statement-start
     ;;
     ((ada-looking-at-semi-private)
      (save-excursion
        (ada-goto-matching-decl-start)
        (current-indentation)))
     ;;
     ;; new/abstract/separate
     ;;
     ((looking-at "\\<\\(new\\|abstract\\|separate\\)\\>")
      (- prev-indent ada-indent (- ada-broken-indent)))
     ;;
     ;; return
     ;;
     ((looking-at "\\<return\\>")
      (save-excursion
        (forward-sexp -1)
        (if (and (looking-at "(")
                 (save-excursion
                   (backward-sexp 2)
                   (looking-at "\\<function\\>")))
            (1+ (current-column))
          prev-indent)))
     ;;
     ;; do
     ;;
     ((looking-at "\\<do\\>")
      (save-excursion
        (ada-goto-stmt-start)
        (+ (current-indentation) ada-stmt-end-indent)))
     ;;
     ;; package/function/procedure
     ;;
     ((and (looking-at "\\<\\(package\\|function\\|procedure\\)\\>")
           (save-excursion
             (forward-char 1)
             (ada-goto-stmt-start)
             (looking-at "\\<\\(package\\|function\\|procedure\\)\\>")))
      (save-excursion
        ;; look for 'generic'
        (if (and (ada-goto-matching-decl-start t)
                 (looking-at "generic"))
            (current-column)
          prev-indent)))
     ;;
     ;; label
     ;;
     ((looking-at "\\<[a-zA-Z0-9_]+[ \t\n]*:[^=]")
      (if (ada-in-decl-p)
          prev-indent
        (+ prev-indent ada-label-indent)))
     ;;
     ;; identifier and other noindent-statements
     ;;
     ((looking-at "\\<[a-zA-Z0-9_]+[ \t\n]*")
      prev-indent)
     ;;
     ;; beginning of a parameter list
     ;;
     ((looking-at "(")
      prev-indent)
     ;;
     ;; end of a parameter list
     ;;
     ((looking-at ")")
      (save-excursion
        (forward-char 1)
        (backward-sexp 1)
        (current-column)))
     ;;
     ;; comment
     ;;
     ((looking-at "--")
      (if ada-indent-comment-as-code
          prev-indent
        (current-indentation)))
     ;;
     ;; unknown syntax - maybe this should signal an error ?
     ;;
     (t
      prev-indent))))


(defun ada-indent-function (&optional nomove)
  ;; Returns the function to calculate the indentation for the current
  ;; line according to the previous statement, ignoring the contents
  ;; of the current line after point.  Moves point to the beginning of
  ;; the current statement, if NOMOVE is nil.

  (let ((orgpoint (point))
        (func nil)
        (stmt-start nil))
    ;;
    ;; inside a parameter-list
    ;;
    (if (ada-in-paramlist-p)
        (setq func 'ada-get-indent-paramlist)
      (progn
        ;;
        ;; move to beginning of current statement
        ;;
        (if (not nomove)
            (setq stmt-start (ada-goto-stmt-start)))
        ;;
        ;; no beginning found => don't change indentation
        ;;
        (if (and
             (eq orgpoint (point))
             (not nomove))
              (setq func 'ada-get-indent-nochange)

          (cond
           ;;
           ((and
             ada-indent-to-open-paren
             (ada-in-open-paren-p))
            (setq func 'ada-get-indent-open-paren))
           ;;
           ((looking-at "\\<end\\>")
            (setq func 'ada-get-indent-end))
           ;;
           ((looking-at ada-loop-start-re)
            (setq func 'ada-get-indent-loop))
           ;;
           ((looking-at ada-subprog-start-re)
            (setq func 'ada-get-indent-subprog))
           ;;
           ((looking-at "\\<package\\>")
            (setq func 'ada-get-indent-subprog)) ; maybe it needs a
                                                 ; special function
                                                 ; sometimes ?
           ;;
           ((looking-at ada-block-start-re)
            (setq func 'ada-get-indent-block-start))
           ;;
           ((looking-at "\\<type\\>")
            (setq func 'ada-get-indent-type))
           ;;
           ((looking-at "\\<\\(els\\)?if\\>")
            (setq func 'ada-get-indent-if))
           ;;
           ((looking-at "\\<case\\>")
            (setq func 'ada-get-indent-case))
           ;;
           ((looking-at "\\<when\\>")
            (setq func 'ada-get-indent-when))
           ;;
           ((looking-at "--")
            (setq func 'ada-get-indent-comment))
           ;;
           ((looking-at "[a-zA-Z0-9_]+[ \t\n]*:[^=]")
            (setq func 'ada-get-indent-label))
           ;;
	   ((looking-at "\\<separate\\>")
	    (setq func 'ada-get-indent-nochange))
           (t
            (setq func 'ada-get-indent-noindent))))))

    func))


;; ---- functions to return indentation for special cases

(defun ada-get-indent-open-paren (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be behind an open paranthesis not yet closed.
  (ada-in-open-paren-p))


(defun ada-get-indent-nochange (orgpoint)
  ;; Returns the indentation (column #) of the current line.
  (save-excursion
    (forward-line -1)
    (current-indentation)))


(defun ada-get-indent-paramlist (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be inside a parameter-list.
  (save-excursion
    (ada-search-ignore-string-comment "[^ \t\n]" t nil t)
    (cond
     ;;
     ;; in front of the first parameter
     ;;
     ((looking-at "(")
      (goto-char (match-end 0))
      (current-column))
     ;;
     ;; in front of another parameter
     ;;
     ((looking-at ";")
      (goto-char (cdr (ada-search-ignore-string-comment "(\\|;" t nil t)))
      (ada-goto-next-non-ws)
      (current-column))
     ;;
     ;; inside a parameter declaration
     ;;
     (t
      (goto-char (cdr (ada-search-ignore-string-comment "(\\|;" t nil t)))
      (ada-goto-next-non-ws)
      (+ (current-column) ada-broken-indent)))))


(defun ada-get-indent-end (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of an end-statement.
  ;; Therefore it has to find the corresponding start. This can be a little
  ;; slow, if it has to search through big files with many nested blocks.
  ;; Signals an error if the corresponding block-start doesn't match.
  (let ((defun-name nil)
        (indent nil))
    ;;
    ;; is the line already terminated by ';' ?
    ;;
    (if (save-excursion
          (ada-search-ignore-string-comment ";" nil orgpoint))
        ;;
        ;; yes, look what's following 'end'
        ;;
        (progn
          (forward-word 1)
          (ada-goto-next-non-ws)
          (cond
           ;;
           ;; loop/select/if/case/record/select
           ;;
           ((looking-at "\\<\\(loop\\|select\\|if\\|case\\|record\\)\\>")
            (save-excursion
              (ada-check-matching-start
               (buffer-substring (match-beginning 0)
                                 (match-end 0)))
              (if (looking-at "\\<\\(loop\\|record\\)\\>")
                  (progn
                    (forward-word 1)
                    (ada-goto-stmt-start)))
              ;; a label ? => skip it
              (if (looking-at "[a-zA-Z0-9_]+[ \n\t]+:")
                  (progn
                    (goto-char (match-end 0))
                    (ada-goto-next-non-ws)))
              ;; really looking-at the right thing ?
              (or (looking-at (concat "\\<\\("
                                      "loop\\|select\\|if\\|case\\|"
                                      "record\\|while\\|type\\)\\>"))
                  (progn
                    (ada-search-ignore-string-comment
                     (concat "\\<\\("
                             "loop\\|select\\|if\\|case\\|"
                             "record\\|while\\|type\\)\\>")))
                  (backward-word 1))
              (current-indentation)))
           ;;
           ;; a named block end
           ;;
           ((looking-at ada-ident-re)
            (setq defun-name (buffer-substring (match-beginning 0)
                                               (match-end 0)))
            (save-excursion
              (ada-goto-matching-start 0)
              (ada-check-defun-name defun-name)
              (current-indentation)))
           ;;
           ;; a block-end without name
           ;;
           ((looking-at ";")
            (save-excursion
              (ada-goto-matching-start 0)
              (if (looking-at "\\<begin\\>")
                  (progn
                    (setq indent (current-column))
                    (if (ada-goto-matching-decl-start t)
                        (current-indentation)
                      indent)))))
           ;;
           ;; anything else - should maybe signal an error ?
           ;;
           (t
            (+ (current-indentation) ada-broken-indent))))

      (+ (current-indentation) ada-broken-indent))))


(defun ada-get-indent-case (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of an case-statement.
  (let ((cur-indent (current-indentation))
        (match-cons nil)
        (opos (point)))
    (cond
     ;;
     ;; case..is..when..=>
     ;;
     ((save-excursion
       (setq match-cons (ada-search-ignore-string-comment
                         "[ \t\n]+=>" nil orgpoint)))
      (save-excursion
        (goto-char (car match-cons))
        (if (not (ada-search-ignore-string-comment "\\<when\\>" t opos))
            (error "missing 'when' between 'case' and '=>'"))
        (+ (current-indentation) ada-indent)))
     ;;
     ;; case..is..when
     ;;
     ((save-excursion
       (setq match-cons (ada-search-ignore-string-comment
                         "\\<when\\>" nil orgpoint)))
      (goto-char (cdr match-cons))
      (+ (current-indentation) ada-broken-indent))
     ;;
     ;; case..is
     ;;
     ((save-excursion
       (setq match-cons (ada-search-ignore-string-comment
                         "\\<is\\>" nil orgpoint)))
      (+ (current-indentation) ada-when-indent))
     ;;
     ;; incomplete case
     ;;
     (t
      (+ (current-indentation) ada-broken-indent)))))


(defun ada-get-indent-when (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of an when-statement.
  (let ((cur-indent (current-indentation)))
    (if (ada-search-ignore-string-comment
         "[ \t\n]+=>" nil orgpoint)
        (+ cur-indent  ada-indent)
      (+ cur-indent ada-broken-indent))))


(defun ada-get-indent-if (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of an if-statement.
  (let ((cur-indent (current-indentation))
        (match-cons nil))
    ;;
    ;; if..then ?
    ;;
    (if (ada-search-but-not
         "\\<then\\>" "\\<and\\>[ \t\n]+\\<then\\>" nil orgpoint)

        (progn
          ;;
          ;; 'then' first in separate line ?
          ;; => indent according to 'then'
          ;;
          (if (save-excursion
                (back-to-indentation)
                (looking-at "\\<then\\>"))
              (setq cur-indent (current-indentation)))
          (forward-word 1)
          ;;
          ;; something follows 'then' ?
          ;;
          (if (setq match-cons
                    (ada-search-ignore-string-comment
                     "[^ \t\n]" nil orgpoint))
              (progn
                (goto-char (car match-cons))
                (+ ada-indent
                   (- cur-indent (current-indentation))
                   (funcall (ada-indent-function t) orgpoint)))

            (+ cur-indent ada-indent)))

      (+ cur-indent ada-broken-indent))))


(defun ada-get-indent-block-start (orgpoint)
  ;; Returns the indentation (column #) for the new line after
  ;; ORGPOINT.  Assumes point to be at the beginning of a block start
  ;; keyword.
  (let ((cur-indent (current-indentation))
        (pos nil))
    (cond
     ((save-excursion
        (forward-word 1)
        (setq pos (car (ada-search-ignore-string-comment
                        "[^ \t\n]" nil orgpoint))))
      (goto-char pos)
      (save-excursion
        (funcall (ada-indent-function t) orgpoint)))
     ;;
     ;; nothing follows the block-start
     ;;
     (t
      (+ (current-indentation) ada-indent)))))


(defun ada-get-indent-subprog (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of a subprog-/package-declaration.
  (let ((match-cons nil)
        (cur-indent (current-indentation))
        (foundis nil)
        (addind 0)
        (fstart (point)))
    ;;
    ;; is there an 'is' in front of point ?
    ;;
    (if (save-excursion
          (setq match-cons
                (ada-search-ignore-string-comment
                 "\\<is\\>\\|\\<do\\>" nil orgpoint)))
        ;;
        ;; yes, then skip to its end
        ;;
        (progn
          (setq foundis t)
          (goto-char (cdr match-cons)))
      ;;
      ;; no, then goto next non-ws, if there is one in front of point
      ;;
      (progn
        (if (ada-search-ignore-string-comment "[^ \t\n]" nil orgpoint)
            (ada-goto-next-non-ws)
          (goto-char orgpoint))))

    (cond
     ;;
     ;; nothing follows 'is'
     ;;
     ((and
       foundis
       (save-excursion
         (not (ada-search-ignore-string-comment
               "[^ \t\n]" nil orgpoint t))))
      (+ cur-indent ada-indent))
     ;;
     ;; is abstract/separate/new ...
     ;;
     ((and
       foundis
       (save-excursion
         (setq match-cons
               (ada-search-ignore-string-comment
                "\\<\\(separate\\|new\\|abstract\\)\\>"
                nil orgpoint))))
      (goto-char (car match-cons))
      (ada-search-ignore-string-comment (concat ada-subprog-start-re
                                                "\\|\\<package\\>") t)
      (ada-get-indent-noindent orgpoint))
     ;;
     ;; something follows 'is'
     ;;
     ((and
       foundis
       (save-excursion
         (ada-search-ignore-string-comment "[^ \t\n]" nil orgpoint))
       (ada-goto-next-non-ws)
      (funcall (ada-indent-function t) orgpoint)))
     ;;
     ;; no 'is' but ';'
     ;;
     ((save-excursion
        (ada-search-ignore-string-comment ";" nil orgpoint))
      cur-indent)
     ;;
     ;; no 'is' or ';'
     ;;
     (t
      (+ cur-indent ada-broken-indent)))))


(defun ada-get-indent-noindent (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of a 'noindent statement'.
  (if (save-excursion
        (ada-search-ignore-string-comment ";" nil orgpoint))
      (current-indentation)
    (+ (current-indentation) ada-broken-indent)))


(defun ada-get-indent-label (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of a label or variable declaration.
  ;; Checks the context to decide if it's a label or a variable declaration.
  ;; This check might be a bit slow.
  (let ((match-cons nil)
        (cur-indent (current-indentation)))
    (goto-char (cdr (ada-search-ignore-string-comment ":")))
    (cond
     ;;
     ;; loop label
     ;;
     ((save-excursion
        (setq match-cons (ada-search-ignore-string-comment
                          ada-loop-start-re nil orgpoint)))
      (goto-char (car match-cons))
      (ada-get-indent-loop orgpoint))
     ;;
     ;; declare label
     ;;
     ((save-excursion
        (setq match-cons (ada-search-ignore-string-comment
                          "\\<declare\\>" nil orgpoint)))
      (save-excursion
        (goto-char (car match-cons))
        (+ (current-indentation) ada-indent)))
     ;;
     ;; complete statement following colon
     ;;
     ((save-excursion
        (ada-search-ignore-string-comment ";" nil orgpoint))
      (if (ada-in-decl-p)
          cur-indent                      ; variable-declaration
        (- cur-indent ada-label-indent))) ; label
     ;;
     ;; broken statement
     ;;
     ((save-excursion
        (ada-search-ignore-string-comment "[^ \t\n]" nil orgpoint))
      (if (ada-in-decl-p)
          (+ cur-indent ada-broken-indent)
        (+ cur-indent ada-broken-indent (- ada-label-indent))))
     ;;
     ;; nothing follows colon
     ;;
     (t
      (if (ada-in-decl-p)
          (+ cur-indent ada-broken-indent)   ; variable-declaration
        (- cur-indent ada-label-indent)))))) ; label


(defun ada-get-indent-loop (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of a loop statement
  ;; or (unfortunately) also a for ... use statement.
  (let ((match-cons nil)
        (pos (point)))
    (cond

     ;;
     ;; statement complete
     ;;
     ((save-excursion
        (ada-search-ignore-string-comment ";" nil orgpoint))
      (current-indentation))
     ;;
     ;; simple loop
     ;;
     ((looking-at "loop\\>")
      (ada-get-indent-block-start orgpoint))

     ;;
     ;; 'for'- loop (or also a for ... use statement)
     ;;
     ((looking-at "for\\>")
      (cond
       ;;
       ;; for ... use
       ;;
       ((save-excursion
          (and
           (goto-char (match-end 0))
           (ada-search-ignore-string-comment "[^ /n/t]" nil orgpoint)
           (not (backward-char 1))
           (not (zerop (skip-chars-forward "_a-zA-Z0-9'")))
           (ada-search-ignore-string-comment "[^ /n/t]" nil orgpoint)
           (not (backward-char 1))
           (looking-at "\\<use\\>")
           ;;
           ;; check if there is a 'record' before point
           ;;
           (progn
             (setq match-cons (ada-search-ignore-string-comment
                               "\\<record\\>" nil orgpoint))
             t)))
        (if match-cons
            (goto-char (car match-cons)))
        (+ (current-indentation) ada-indent))
       ;;
       ;; for..loop
       ;;
       ((save-excursion
          (setq match-cons (ada-search-ignore-string-comment
                            "\\<loop\\>" nil orgpoint)))
        (goto-char (car match-cons))
        ;;
        ;; indent according to 'loop', if it's first in the line;
        ;; otherwise to 'for'
        ;;
        (if (not (save-excursion
                   (back-to-indentation)
                   (looking-at "\\<loop\\>")))
            (goto-char pos))
        (+ (current-indentation) ada-indent))
       ;;
       ;; for-statement is broken
       ;;
       (t
        (+ (current-indentation) ada-broken-indent))))

     ;;
     ;; 'while'-loop
     ;;
     ((looking-at "while\\>")
      ;;
      ;; while..loop ?
      ;;
      (if (save-excursion
            (setq match-cons (ada-search-ignore-string-comment
                              "\\<loop\\>" nil orgpoint)))

          (progn
            (goto-char (car match-cons))
            ;;
            ;; indent according to 'loop', if it's first in the line;
            ;; otherwise to 'while'.
            ;;
            (if (not (save-excursion
                       (back-to-indentation)
                       (looking-at "\\<loop\\>")))
                (goto-char pos))
            (+ (current-indentation) ada-indent))

        (+ (current-indentation) ada-broken-indent))))))


(defun ada-get-indent-type (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of a type statement.
  (let ((match-dat nil))
    (cond
     ;;
     ;; complete record declaration
     ;;
     ((save-excursion
        (and
         (setq match-dat (ada-search-ignore-string-comment "\\<end\\>"
                                                           nil
                                                           orgpoint))
         (ada-goto-next-non-ws)
         (looking-at "\\<record\\>")
         (forward-word 1)
         (ada-goto-next-non-ws)
         (looking-at ";")))
      (goto-char (car match-dat))
      (current-indentation))
     ;;
     ;; record type
     ;;
     ((save-excursion
        (setq match-dat (ada-search-ignore-string-comment "\\<record\\>"
                                                          nil
                                                          orgpoint)))
      (goto-char (car match-dat))
      (+ (current-indentation) ada-indent))
     ;;
     ;; complete type declaration
     ;;
     ((save-excursion
        (ada-search-ignore-string-comment ";" nil orgpoint))
      (current-indentation))
     ;;
     ;; "type ... is", but not "type ... is ...", which is broken
     ;;
     ((save-excursion
	(and
	 (ada-search-ignore-string-comment "\\<is\\>" nil orgpoint)
	 (not (ada-search-ignore-string-comment "[^ \t\n]" nil orgpoint))))
      (+ (current-indentation) ada-indent))
     ;;
     ;; broken statement
     ;;
     (t
      (+ (current-indentation) ada-broken-indent)))))


;;; ---- support-functions for indentation

;;; ---- searching and matching

(defun ada-goto-stmt-start (&optional limit)
  ;; Moves point to the beginning of the statement that point is in or
  ;; after.  Returns the new position of point.  Beginnings are found
  ;; by searching for 'ada-end-stmt-re' and then moving to the
  ;; following non-ws that is not a comment.  LIMIT is actually not
  ;; used by the indentation functions.
  (let ((match-dat nil)
        (orgpoint (point)))

    (setq match-dat (ada-search-prev-end-stmt limit))
    (if match-dat
        ;;
        ;; found a previous end-statement => check if anything follows
        ;;
        (progn
          (if (not
               (save-excursion
                 (goto-char (cdr match-dat))
                 (ada-search-ignore-string-comment
                  "[^ \t\n]" nil orgpoint)))
              ;;
              ;; nothing follows => it's the end-statement directly in
              ;;                    front of point => search again
              ;;
              (setq match-dat (ada-search-prev-end-stmt limit)))
          ;;
          ;; if found the correct end-stetement => goto next non-ws
          ;;
          (if match-dat
              (goto-char (cdr match-dat)))
          (ada-goto-next-non-ws))

      ;;
      ;; no previous end-statement => we are at the beginning of the
      ;;                              accessible part of the buffer
      ;;
      (progn
        (goto-char (point-min))
        ;;
        ;; skip to the very first statement, if there is one
        ;;
        (if (setq match-dat
                  (ada-search-ignore-string-comment
                   "[^ \t\n]" nil orgpoint))
            (goto-char (car match-dat))
          (goto-char orgpoint))))


    (point)))


(defun ada-search-prev-end-stmt (&optional limit)
  ;; Moves point to previous end-statement.  Returns a cons cell whose
  ;; car is the beginning and whose cdr the end of the match.
  ;; End-statements are defined by 'ada-end-stmt-re'.  Checks for
  ;; certain keywords if they follow 'end', which means they are no
  ;; end-statement there.
  (let ((match-dat nil)
        (pos nil)
        (found nil))
    ;;
    ;; search until found or beginning-of-buffer
    ;;
    (while
        (and
         (not found)
         (setq match-dat (ada-search-ignore-string-comment ada-end-stmt-re
                                                           t
                                                           limit)))

      (goto-char (car match-dat))

      (if (not (ada-in-open-paren-p))
          ;;
          ;; check if there is an 'end' in front of the match
          ;;
          (if (not (and
                    (looking-at "\\<\\(record\\|loop\\|select\\)\\>")
                    (save-excursion
                      (ada-goto-previous-word)
                      (looking-at "\\<end\\>"))))
              (setq found t)

            (backward-word 1)))) ; end of loop

    (if found
        match-dat
      nil)))


(defun ada-goto-next-non-ws (&optional limit)
  ;; Skips whitespaces, newlines and comments to next non-ws
  ;; character.  Signals an error if there is no more such character
  ;; and limit is nil.
  (let ((match-cons nil))
    (setq match-cons (ada-search-ignore-string-comment
                      "[^ \t\n]" nil limit t))
    (if match-cons
        (goto-char (car match-cons))
      (if (not limit)
          (error "no more non-ws")
        nil))))


(defun ada-goto-stmt-end (&optional limit)
  ;; Moves point to the end of the statement that point is in or
  ;; before.  Returns the new position of point or nil if not found.
  (if (ada-search-ignore-string-comment ada-end-stmt-re nil limit)
      (point)
    nil))


(defun ada-goto-previous-word ()
  ;; Moves point to the beginning of the previous word of ada-code.
  ;; Returns the new position of point or nil if not found.
  (let ((match-cons nil)
        (orgpoint (point)))
    (if (setq match-cons
              (ada-search-ignore-string-comment "[^ \t\n]" t nil t))
        ;;
        ;; move to the beginning of the word found
        ;;
        (progn
          (goto-char (cdr match-cons))
          (skip-chars-backward "_a-zA-Z0-9")
          (point))
      ;;
      ;; if not found, restore old position of point
      ;;
      (progn
        (goto-char orgpoint)
        'nil))))


(defun ada-check-matching-start (keyword)
  ;; Signals an error if matching block start is not KEYWORD.
  ;; Moves point to the matching block start.
  (ada-goto-matching-start 0)
  (if (not (looking-at (concat "\\<" keyword "\\>")))
      (error (concat
              "matching start is not '"
              keyword "'"))))


(defun ada-check-defun-name (defun-name)
  ;; Checks if the name of the matching defun really is DEFUN-NAME.
  ;; Assumes point to be already positioned by 'ada-goto-matching-start'.
  ;; Moves point to the beginning of the declaration.

  ;;
  ;; 'accept' or 'package' ?
  ;;
  (if (not (looking-at "\\<\\(accept\\|package\\|task\\|protected\\)\\>"))
      (ada-goto-matching-decl-start))
  ;;
  ;; 'begin' of 'procedure'/'function'/'task' or 'declare'
  ;;
  (save-excursion
    ;;
    ;; a named 'declare'-block ?
    ;;
    (if (looking-at "\\<declare\\>")
        (ada-goto-stmt-start)
      ;;
      ;; no, => 'procedure'/'function'/'task'/'protected'
      ;;
      (progn
        (forward-word 2)
        (backward-word 1)
        ;;
        ;; skip 'body' 'protected' 'type'
        ;;
        (if (looking-at "\\<\\(body\\|type\\)\\>")
            (forward-word 1))
        (forward-sexp 1)
        (backward-sexp 1)))
    ;;
    ;; should be looking-at the correct name
    ;;
    (if (not (looking-at (concat "\\<" defun-name "\\>")))
        (error
         (concat
          "matching defun has different name: "
          (buffer-substring
           (point)
           (progn
             (forward-sexp 1)
             (point))))))))


(defun ada-goto-matching-decl-start (&optional noerror nogeneric)
  ;; Moves point to the matching declaration start of the current 'begin'.
  ;; If NOERROR is non-nil, it only returns nil if no match was found.
  (let ((nest-count 1)
        (pos nil)
        (first t)
        (flag nil))
    ;;
    ;; search backward for interesting keywords
    ;;
    (while (and
            (not (zerop nest-count))
            (ada-search-ignore-string-comment
             (concat "\\<\\("
                     "is\\|separate\\|end\\|declare\\|new\\|begin\\|generic"
                     "\\)\\>") t))
      ;;
      ;; calculate nest-depth
      ;;
      (cond
       ;;
       ((looking-at "end")
        (ada-goto-matching-start 1 noerror)
        (if (looking-at "begin")
            (setq nest-count (1+ nest-count))))
       ;;
       ((looking-at "declare\\|generic")
        (setq nest-count (1- nest-count))
        (setq first nil))
       ;;
       ((looking-at "is")
        ;; check if it is only a type definition
        (if (save-excursion
              (ada-goto-previous-word)
              (skip-chars-backward "a-zA-Z0-9_.'")
              (if (save-excursion
                    (backward-char 1)
                    (looking-at ")"))
                  (progn
                    (forward-char 1)
                    (backward-sexp 1)
                    (skip-chars-backward "a-zA-Z0-9_.'")
                    ))
              (ada-goto-previous-word)
              (looking-at "\\<type\\>")) ; end of save-excursion
            (goto-char (match-beginning 0))
          (progn
            (setq nest-count (1- nest-count))
            (setq first nil))))

       ;;
       ((looking-at "new")
        (if (save-excursion
              (ada-goto-previous-word)
              (looking-at "is"))
            (goto-char (match-beginning 0))))
       ;;
       ((and first
             (looking-at "begin"))
        (setq nest-count 0)
        (setq flag t))
       ;;
       (t
        (setq nest-count (1+ nest-count))
        (setq first nil)))

      )  ;; end of loop

    ;; check if declaration-start is really found
    (if (not
         (and
          (zerop nest-count)
          (not flag)
          (progn
            (if (looking-at "is")
                  (ada-search-ignore-string-comment
                   ada-subprog-start-re t)
              (looking-at "declare\\|generic")))))
        (if noerror nil
          (error "no matching procedure/function/task/declare/package"))
      t)))


(defun ada-goto-matching-start (&optional nest-level noerror gotothen)
  ;; Moves point to the beginning of a block-start.  Which block
  ;; depends on the value of NEST-LEVEL, which defaults to zero.  If
  ;; NOERROR is non-nil, it only returns nil if no matching start was
  ;; found.  If GOTOTHEN is non-nil, point moves to the 'then'
  ;; following 'if'.
  (let ((nest-count (if nest-level nest-level 0))
        (found nil)
        (pos nil))

    ;;
    ;; search backward for interesting keywords
    ;;
    (while (and
            (not found)
            (ada-search-ignore-string-comment
             (concat "\\<\\("
                     "end\\|loop\\|select\\|begin\\|case\\|do\\|"
                     "if\\|task\\|package\\|record\\|protected\\)\\>")
             t))

      ;;
      ;; calculate nest-depth
      ;;
      (cond
       ;; found block end => increase nest depth
       ((looking-at "end")
        (setq nest-count (1+ nest-count)))
       ;; found loop/select/record/case/if => check if it starts or
       ;; ends a block
       ((looking-at "loop\\|select\\|record\\|case\\|if")
        (setq pos (point))
        (save-excursion
          ;;
          ;; check if keyword follows 'end'
          ;;
          (ada-goto-previous-word)
          (if (looking-at "\\<end\\>")
              ;; it ends a block => increase nest depth
              (progn
                (setq nest-count (1+ nest-count))
                (setq pos (point)))
            ;; it starts a block => decrease nest depth
            (setq nest-count (1- nest-count))))
        (goto-char pos))
       ;; found package start => check if it really is a block
       ((looking-at "package")
        (save-excursion
          (ada-search-ignore-string-comment "\\<is\\>")
          (ada-goto-next-non-ws)
          ;; ignore it if it is only a declaration with 'new'
          (if (not (looking-at "\\<new\\>"))
              (setq nest-count (1- nest-count)))))
       ;; found task start => check if it has a body
       ((looking-at "task")
        (save-excursion
          (forward-word 1)
          (ada-goto-next-non-ws)
          ;; ignore it if it has no body
          (if (not (looking-at "\\<body\\>"))
              (setq nest-count (1- nest-count)))))
       ;; all the other block starts
       (t
        (setq nest-count (1- nest-count)))) ; end of 'cond'

      ;; match is found, if nest-depth is zero
      ;;
      (setq found (zerop nest-count))) ; end of loop

    (if found
        ;;
        ;; match found => is there anything else to do ?
        ;;
        (progn
          (cond
           ;;
           ;; found 'if' => skip to 'then', if it's on a separate line
           ;;                               and GOTOTHEN is non-nil
           ;;
           ((and
             gotothen
             (looking-at "if")
             (save-excursion
               (ada-search-ignore-string-comment "\\<then\\>" nil nil)
               (back-to-indentation)
               (looking-at "\\<then\\>")))
            (goto-char (match-beginning 0)))
           ;;
           ;; found 'do' => skip back to 'accept'
           ;;
           ((looking-at "do")
            (if (not (ada-search-ignore-string-comment "\\<accept\\>" t nil))
                (error "missing 'accept' in front of 'do'"))))
          (point))

      (if noerror
          nil
        (error "no matching start")))))


(defun ada-goto-matching-end (&optional nest-level noerror)
  ;; Moves point to the end of a block.  Which block depends on the
  ;; value of NEST-LEVEL, which defaults to zero.  If NOERROR is
  ;; non-nil, it only returns nil if found no matching start.
  (let ((nest-count (if nest-level nest-level 0))
        (found nil))

    ;;
    ;; search forward for interesting keywords
    ;;
    (while (and
            (not found)
            (ada-search-ignore-string-comment
             (concat "\\<\\(end\\|loop\\|select\\|begin\\|case\\|"
                     "if\\|task\\|package\\|record\\|do\\)\\>")))

      ;;
      ;; calculate nest-depth
      ;;
      (backward-word 1)
      (cond
       ;; found block end => decrease nest depth
       ((looking-at "\\<end\\>")
        (setq nest-count (1- nest-count))
        ;; skip the following keyword
        (if (progn
              (skip-chars-forward "end")
              (ada-goto-next-non-ws)
              (looking-at "\\<\\(loop\\|select\\|record\\|case\\|if\\)\\>"))
            (forward-word 1)))
       ;; found package start => check if it really starts a block
       ((looking-at "\\<package\\>")
        (ada-search-ignore-string-comment "\\<is\\>")
        (ada-goto-next-non-ws)
        ;; ignore and skip it if it is only a 'new' package
        (if (not (looking-at "\\<new\\>"))
            (setq nest-count (1+ nest-count))
          (skip-chars-forward "new")))
       ;; all the other block starts
       (t
        (setq nest-count (1+ nest-count))
        (forward-word 1))) ; end of 'cond'

      ;; match is found, if nest-depth is zero
      ;;
      (setq found (zerop nest-count))) ; end of loop

    (if (not found)
        (if noerror
            nil
          (error "no matching end"))
      t)))


(defun ada-forward-sexp-ignore-comment ()
  ;; Skips one sexp forward, ignoring comments.
  (while (looking-at "[ \t\n]*--")
    (skip-chars-forward "[ \t\n]")
    (end-of-line))
  (forward-sexp 1))


(defun ada-search-ignore-string-comment
  (search-re &optional backward limit paramlists)
  ;; Regexp-Search for SEARCH-RE, ignoring comments, strings and
  ;; parameter lists, if PARAMLISTS is nil. Returns a cons cell of
  ;; begin and end of match data or nil, if not found.
  (let ((found nil)
        (begin nil)
        (end nil)
        (pos nil)
        (search-func
         (if backward 're-search-backward
           're-search-forward)))

    ;;
    ;; search until found or end-of-buffer
    ;;
    (while (and (not found)
                (funcall search-func search-re limit 1))
      (setq begin (match-beginning 0))
      (setq end (match-end 0))

      (cond
       ;;
       ;; found in comment => skip it
       ;;
       ((ada-in-comment-p)
        (if backward
            (progn
              (re-search-backward "--" nil 1)
              (goto-char (match-beginning 0)))
          (progn
            (forward-line 1)
            (beginning-of-line))))
       ;;
       ;; found in string => skip it
       ;;
       ((ada-in-string-p)
        (if backward
            (progn
              (re-search-backward "\"" nil 1) ; "\"\\|#" don't treat #
              (goto-char (match-beginning 0))))
        (re-search-forward "\"" nil 1))
       ;;
       ;; found character constant => ignore it
       ;;
       ((save-excursion
          (setq pos (- (point) (if backward 1 2)))
          (and (char-after pos)
               (= (char-after pos) ?')
               (= (char-after (+ pos 2)) ?')))
        ())
       ;;
       ;; found a parameter-list but should ignore it => skip it
       ;;
       ((and (not paramlists)
             (ada-in-paramlist-p))
        (if backward
            (ada-search-ignore-string-comment "(" t nil t)))
       ;;
       ;; directly in front of a comment => skip it, if searching forward
       ;;
       ((save-excursion
          (goto-char begin)
          (looking-at "--"))
        (if (not backward)
            (progn
              (forward-line 1)
              (beginning-of-line))))
       ;;
       ;; found what we were looking for
       ;;
       (t
        (setq found t)))) ; end of loop

    (if found
        (cons begin end)
      nil)))


(defun ada-search-but-not (search-re not-search-re &optional backward limit)
  ;; Searches SEARCH-RE, ignoring parts of NOT-SEARCH-RE, strings,
  ;; comments and parameter-lists.
  (let ((begin nil)
        (end nil)
        (begin-not nil)
        (begin-end nil)
        (end-not nil)
        (ret-cons nil)
        (found nil))

    ;;
    ;; search until found or end-of-buffer
    ;;
    (while (and
            (not found)
            (save-excursion
              (setq ret-cons
                    (ada-search-ignore-string-comment search-re
                                                      backward limit))
              (if (consp ret-cons)
                  (progn
                    (setq begin (car ret-cons))
                    (setq end (cdr ret-cons))
                    t)
                nil)))

      (if (or
           ;;
           ;; if no NO-SEARCH-RE was found
           ;;
           (not
            (save-excursion
              (setq ret-cons
                    (ada-search-ignore-string-comment not-search-re
                                                      backward nil))
              (if (consp ret-cons)
                  (progn
                    (setq begin-not (car ret-cons))
                    (setq end-not (cdr ret-cons))
                    t)
                nil)))
           ;;
           ;;  or this NO-SEARCH-RE is not a part of the SEARCH-RE
           ;;  found before.
           ;;
           (or
            (<= end-not begin)
            (>= begin-not end)))

          (setq found t)

        ;;
        ;; not found the correct match => skip this match
        ;;
        (goto-char (if backward
                       begin
                     end)))) ; end of loop

    (if found
        (progn
          (goto-char begin)
          (cons begin end))
      nil)))


(defun ada-goto-prev-nonblank-line ( &optional ignore-comment)
  ;; Moves point to the beginning of previous non-blank line,
  ;; ignoring comments if IGNORE-COMMENT is non-nil.
  ;; It returns t if a matching line was found.
  (let ((notfound t)
        (newpoint nil))

    (save-excursion
      ;;
      ;; backward one line, if there is one
      ;;
      (if (zerop (forward-line -1))
          ;;
          ;; there is some kind of previous line
          ;;
          (progn
            (beginning-of-line)
            (setq newpoint (point))

            ;;
            ;; search until found or beginning-of-buffer
            ;;
            (while (and (setq notfound
                              (or (looking-at "[ \t]*$")
                                  (and (looking-at "[ \t]*--")
                                       ignore-comment)))
                        (not (ada-in-limit-line-p)))
              (forward-line -1)
              ;;(beginning-of-line)
              (setq newpoint (point))) ; end of loop

            )) ; end of if

      ) ; end of save-excursion

    (if notfound nil
      (progn
        (goto-char newpoint)
        t))))


(defun ada-goto-next-nonblank-line ( &optional ignore-comment)
  ;; Moves point to next non-blank line,
  ;; ignoring comments if IGNORE-COMMENT is non-nil.
  ;; It returns t if a matching line was found.
  (let ((notfound t)
        (newpoint nil))

    (save-excursion
    ;;
    ;; forward one line
    ;;
      (if (zerop (forward-line 1))
          ;;
          ;; there is some kind of previous line
          ;;
          (progn
            (beginning-of-line)
            (setq newpoint (point))

            ;;
            ;; search until found or end-of-buffer
            ;;
            (while (and (setq notfound
                              (or (looking-at "[ \t]*$")
                                  (and (looking-at "[ \t]*--")
                                       ignore-comment)))
                        (not (ada-in-limit-line-p)))
              (forward-line 1)
              (beginning-of-line)
              (setq newpoint (point))) ; end of loop

            )) ; end of if

      ) ; end of save-excursion

    (if notfound nil
      (progn
        (goto-char newpoint)
        t))))


;; ---- boolean functions for indentation

(defun ada-in-decl-p ()
  ;; Returns t if point is inside a declarative part.
  ;; Assumes point to be at the end of a statement.
  (or
   (ada-in-paramlist-p)
   (save-excursion
     (ada-goto-matching-decl-start t))))


(defun ada-looking-at-semi-or ()
  ;; Returns t if looking-at an 'or' following a semicolon.
  (save-excursion
    (and (looking-at "\\<or\\>")
         (progn
           (forward-word 1)
           (ada-goto-stmt-start)
           (looking-at "\\<or\\>")))))


(defun ada-looking-at-semi-private ()
  ;; Returns t if looking-at an 'private' following a semicolon.
  (save-excursion
    (and (looking-at "\\<private\\>")
         (progn
           (forward-word 1)
           (ada-goto-stmt-start)
           (looking-at "\\<private\\>")))))


;;; make a faster??? ada-in-limit-line-p not using count-lines
(defun ada-in-limit-line-p ()
  ;; return t if point is in first or last accessible line.
  (or (save-excursion (beginning-of-line) (= (point-min) (point)))
      (save-excursion (end-of-line) (= (point-max) (point)))))


(defun ada-in-comment-p ()
  ;; Returns t if inside a comment.
  (save-excursion (and (re-search-backward "\\(--\\|\n\\)" nil 1)
                       (looking-at "-"))))


(defun ada-in-string-p ()
  ;; Returns t if point is inside a string
  ;; (Taken from pascal-mode.el, modified by MH).
  (save-excursion
    (and
     (nth 3 (parse-partial-sexp
             (save-excursion
               (beginning-of-line)
               (point)) (point)))
     ;; check if 'string quote' is only a character constant
     (progn
       (re-search-backward "\"" nil t) ; # not a string delimiter anymore
       (not (= (char-after (1- (point))) ?'))))))


(defun ada-in-string-or-comment-p ()
  ;; Returns t if point is inside a string or a comment.
  (or (ada-in-comment-p)
      (ada-in-string-p)))


(defun ada-in-paramlist-p ()
  ;; Returns t if point is inside a parameter-list
  ;; following 'function'/'procedure'/'package'.
  (save-excursion
    (and
     (re-search-backward "(\\|)" nil t)
     ;; inside parentheses ?
     (looking-at "(")
     (backward-word 2)
     ;; right keyword before paranthesis ?
     (looking-at (concat "\\<\\("
                         "procedure\\|function\\|body\\|package\\|"
                         "task\\|entry\\|accept\\)\\>"))
     (re-search-forward ")\\|:" nil t)
     ;; at least one ':' inside the parentheses ?
     (not (backward-char 1))
     (looking-at ":"))))


;; not really a boolean function ...
(defun ada-in-open-paren-p ()
  ;; If point is somewhere behind an open parenthesis not yet closed,
  ;; it returns the column # of the first non-ws behind this open
  ;; parenthesis, otherwise nil."

  (let ((start (if (< (point) ada-search-paren-char-count-limit)
                   1
                 (- (point) ada-search-paren-char-count-limit)))
        parse-result
        (col nil))
    (setq parse-result (parse-partial-sexp start (point)))
    (if (nth 1 parse-result)
        (save-excursion
          (goto-char (1+ (nth 1 parse-result)))
          (if (save-excursion
                (re-search-forward "[^ \t]" nil 1)
                (backward-char 1)
                (and
                 (not (looking-at "\n"))
                 (setq col (current-column))))
              col
            (current-column)))
      nil)))



;;;----------------------;;;
;;; Behaviour Of TAB Key ;;;
;;;----------------------;;;

(defun ada-tab ()
  "Do indenting or tabbing according to `ada-tab-policy'."
  (interactive)
  (cond ((eq ada-tab-policy 'indent-and-tab) (error "not implemented"))
        ;; ada-indent-and-tab
        ((eq ada-tab-policy 'indent-rigidly) (ada-tab-hard))
        ((eq ada-tab-policy 'indent-auto) (ada-indent-current))
        ((eq ada-tab-policy 'gei) (ada-tab-gei))
        ((eq ada-tab-policy 'indent-af) (af-indent-line)) ; GEB
        ((eq ada-tab-policy 'always-tab) (error "not implemented"))
        ))


(defun ada-untab (arg)
  "Delete leading indenting according to `ada-tab-policy'."
  (interactive "P")
  (cond ((eq ada-tab-policy 'indent-rigidly) (ada-untab-hard))
        ((eq ada-tab-policy 'indent-af) (backward-delete-char-untabify ; GEB
                                         (prefix-numeric-value arg) ; GEB
                                         arg)) ; GEB
        ((eq ada-tab-policy 'indent-auto) (error "not implemented"))
        ((eq ada-tab-policy 'always-tab) (error "not implemented"))
        ))


(defun ada-indent-current-function ()
  "Ada Mode version of the indent-line-function."
  (interactive "*")
  (let ((starting-point (point-marker)))
    (ada-beginning-of-line)
    (ada-tab)
    (if (< (point) starting-point)
        (goto-char starting-point))
    (set-marker starting-point nil)
    ))


(defun ada-tab-hard ()
  "Indent current line to next tab stop."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert-char ?  ada-indent))
  (if (save-excursion (= (point) (progn (beginning-of-line) (point))))
      (forward-char ada-indent)))


(defun ada-untab-hard ()
  "indent current line to previous tab stop."
  (interactive)
  (let  ((bol (save-excursion (progn (beginning-of-line) (point))))
        (eol (save-excursion (progn (end-of-line) (point)))))
    (indent-rigidly bol eol  (- 0 ada-indent))))



;;;---------------;;;
;;; Miscellaneous ;;;
;;;---------------;;;

(defun ada-remove-trailing-spaces  ()
;; remove all trailing spaces at the end of lines.
 "remove trailing spaces in the whole buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))))


(defun ada-untabify-buffer ()
;; change all tabs to spaces
  (save-excursion
    (untabify (point-min) (point-max))))


(defun ada-uncomment-region (beg end)
  "delete comment-start at the beginning of a line in the region."
  (interactive "r")
  (comment-region beg end -1))


;; define a function to support find-file.el if loaded
(defun ada-ff-other-window ()
  "Find other file in other window using ff-find-other-file."
  (interactive)
  (and (fboundp 'ff-find-other-file)
       (ff-find-other-file t)))


;;;-------------------------------;;;
;;; Moving To Procedures/Packages ;;;
;;;-------------------------------;;;

(defun ada-next-procedure ()
  "Moves point to next procedure."
  (interactive)
  (end-of-line)
  (if (re-search-forward ada-procedure-start-regexp nil t)
      (goto-char (match-beginning 1))
    (error "No more functions/procedures/tasks")))

(defun ada-previous-procedure ()
  "Moves point to previous procedure."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward ada-procedure-start-regexp nil t)
      (goto-char (match-beginning 1))
    (error "No more functions/procedures/tasks")))

(defun ada-next-package ()
  "Moves point to next package."
  (interactive)
  (end-of-line)
  (if (re-search-forward ada-package-start-regexp nil t)
      (goto-char (match-beginning 1))
    (error "No more packages")))

(defun ada-previous-package ()
  "Moves point to previous package."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward ada-package-start-regexp nil t)
      (goto-char (match-beginning 1))
    (error "No more packages")))


;;;-----------------------
;;; define keymap for Ada
;;;-----------------------

(if (not ada-mode-map)
    (progn
      (setq ada-mode-map (make-sparse-keymap))

      ;; Indentation and Formatting
      (define-key ada-mode-map "\C-j"     'ada-indent-newline-indent)
      (define-key ada-mode-map "\t"       'ada-tab)
      (define-key ada-mode-map "\C-c\C-l" 'ada-indent-region)
      (if (ada-xemacs)
	  (define-key ada-mode-map '(shift tab)    'ada-untab)
	(define-key ada-mode-map [S-tab]    'ada-untab))
      (define-key ada-mode-map "\C-c\C-f" 'ada-format-paramlist)
      (define-key ada-mode-map "\C-c\C-p" 'ada-call-pretty-printer)
;;; We don't want to make meta-characters case-specific.
;;;   (define-key ada-mode-map "\M-Q"     'ada-fill-comment-paragraph-justify)
      (define-key ada-mode-map "\M-\C-q"  'ada-fill-comment-paragraph-postfix)

      ;; Movement
;;; It isn't good to redefine these.  What should be done instead?  -- rms.
;;;   (define-key ada-mode-map "\M-e"     'ada-next-package)
;;;   (define-key ada-mode-map "\M-a"     'ada-previous-package)
      (define-key ada-mode-map "\M-\C-e"  'ada-next-procedure)
      (define-key ada-mode-map "\M-\C-a"  'ada-previous-procedure)
      (define-key ada-mode-map "\C-c\C-a" 'ada-move-to-start)
      (define-key ada-mode-map "\C-c\C-e" 'ada-move-to-end)

      ;; Compilation
      (define-key ada-mode-map "\C-c\C-c" 'compile)

      ;; Casing
      (define-key ada-mode-map "\C-c\C-r" 'ada-adjust-case-region)
      (define-key ada-mode-map "\C-c\C-b" 'ada-adjust-case-buffer)

      (define-key ada-mode-map "\177"     'backward-delete-char-untabify)

      ;; Use predefined function of emacs19 for comments (RE)
      (define-key ada-mode-map "\C-c;"    'comment-region)
      (define-key ada-mode-map "\C-c:"    'ada-uncomment-region)

      ;; Change basic functionality

      ;; substitute-key-definition is not defined equally in GNU Emacs
      ;; and XEmacs, you cannot put in an optional 4th parameter in
      ;; XEmacs.  I don't think it's necessary, so I leave it out for
      ;; GNU Emacs as well.  If you encounter any problems with the
      ;; following three functions, please tell me. RE
      (mapcar (function (lambda (pair)
			  (substitute-key-definition (car pair) (cdr pair)
						     ada-mode-map)))
	      '((beginning-of-line      . ada-beginning-of-line)
		(end-of-line            . ada-end-of-line)
		(forward-to-indentation . ada-forward-to-indentation)
		))
      ;; else GNU Emacs
      ;;(mapcar (lambda (pair)
      ;;             (substitute-key-definition (car pair) (cdr pair)
      ;;				   ada-mode-map global-map))

      ))


;;;-------------------
;;; define menu 'Ada'
;;;-------------------

(require 'easymenu)

(defun ada-add-ada-menu ()
  "Adds the menu 'Ada' to the menu-bar in Ada Mode."
  (easy-menu-define ada-mode-menu ada-mode-map "Menu keymap for Ada mode."
                    '("Ada"
                      ["Next Package" ada-next-package t]
                      ["Previous Package" ada-previous-package t]
                      ["Next Procedure" ada-next-procedure t]
                      ["Previous Procedure" ada-previous-procedure t]
                      ["Goto Start" ada-move-to-start t]
                      ["Goto End" ada-move-to-end t]
                      ["------------------" nil nil]
                      ["Indent Current Line (TAB)"
                       ada-indent-current-function t]
                      ["Indent Lines in Region" ada-indent-region t]
                      ["Format Parameter List" ada-format-paramlist t]
                      ["Pretty Print Buffer" ada-call-pretty-printer t]
                      ["------------" nil nil]
                      ["Fill Comment Paragraph"
                       ada-fill-comment-paragraph t]
                      ["Justify Comment Paragraph"
                       ada-fill-comment-paragraph-justify t]
                      ["Postfix Comment Paragraph"
                       ada-fill-comment-paragraph-postfix t]
                      ["------------" nil nil]
                      ["Adjust Case Region" ada-adjust-case-region t]
                      ["Adjust Case Buffer" ada-adjust-case-buffer t]
                      ["----------" nil nil]
                      ["Comment   Region" comment-region t]
                      ["Uncomment Region" ada-uncomment-region t]
                      ["----------------" nil nil]
                      ["Compile" compile (fboundp 'compile)]
                      ["Next Error" next-error (fboundp 'next-error)]
                      ["---------------" nil nil]
                      ["Index" imenu (fboundp 'imenu)]
                      ["--------------" nil nil]
                      ["Other File Other Window" ada-ff-other-window
                       (fboundp 'ff-find-other-file)]
                      ["Other File" ff-find-other-file
                       (fboundp 'ff-find-other-file)]))
  (if (ada-xemacs) (progn
                     (easy-menu-add ada-mode-menu)
                     (setq mode-popup-menu (cons "Ada Mode" ada-mode-menu)))))



;;;-------------------------------
;;; Define Some Support Functions
;;;-------------------------------

(defun ada-beginning-of-line (&optional arg)
  (interactive "P")
  (cond
   ((eq ada-tab-policy 'indent-af) (af-beginning-of-line arg))
   (t (beginning-of-line arg))
   ))

(defun ada-end-of-line (&optional arg)
  (interactive "P")
  (cond
   ((eq ada-tab-policy 'indent-af) (af-end-of-line arg))
   (t (end-of-line arg))
   ))

(defun ada-current-column ()
  (cond
   ((eq ada-tab-policy 'indent-af) (af-current-column))
   (t (current-column))
   ))

(defun ada-forward-to-indentation (&optional arg)
  (interactive "P")
  (cond
   ((eq ada-tab-policy 'indent-af) (af-forward-to-indentation arg))
   (t (forward-to-indentation arg))
   ))

;;;---------------------------------------------------
;;; support for find-file
;;;---------------------------------------------------


;;;###autoload
(defun ada-make-filename-from-adaname (adaname)
  "determine the filename of a package/procedure from its own Ada name."
  ;; this is done simply by calling gkrunch, when we work with GNAT. It
  ;; must be a more complex function in other compiler environments.
  (interactive "s")

  ;; things that should really be done by the external process
  ;; since gnat-2.0, gnatk8 can do these things. If you still use a
  ;; previous version, just uncomment the following lines.
  (let (krunch-buf)
    (setq krunch-buf (generate-new-buffer "*gkrunch*"))
    (save-excursion
      (set-buffer krunch-buf)
;      (insert (downcase adaname))
;      (goto-char (point-min))
;      (while (search-forward "." nil t)
;        (replace-match "-" nil t))
;      (setq adaname (buffer-substring (point-min)
;                                      (progn
;                                        (goto-char (point-min))
;                                        (end-of-line)
;                                        (point))))
;      ;; clean the buffer
;      (delete-region (point-min) (point-max))
      ;; send adaname to external process "gnatk8"
      (call-process "gnatk8" nil krunch-buf nil
                    adaname ada-krunch-args)
      ;; fetch output of that process
      (setq adaname (buffer-substring
                     (point-min)
                     (progn
                       (goto-char (point-min))
                       (end-of-line)
                       (point))))
      (kill-buffer krunch-buf)))
  (setq adaname adaname) ;; can I avoid this statement?
  )


;;; functions for placing the cursor on the corresponding subprogram
(defun ada-which-function-are-we-in ()
  "Determine whether we are on a function definition/declaration and remember
the name of that function."

  (setq ff-function-name nil)

  (save-excursion
    (if (re-search-backward ada-procedure-start-regexp nil t)
	(setq ff-function-name (buffer-substring (match-beginning 0)
						 (match-end 0)))
      ; we didn't find a procedure start, perhaps there is a package
      (if (re-search-backward ada-package-start-regexp nil t)
	  (setq ff-function-name (buffer-substring (match-beginning 0)
						   (match-end 0)))
	))))


;;;---------------------------------------------------
;;; support for imenu
;;;---------------------------------------------------

(defun imenu-create-ada-index (&optional regexp)
  "create index alist for Ada files."
  (let ((index-alist '())
        prev-pos char)
    (goto-char (point-min))
    ;(imenu-progress-message prev-pos 0)
    ;; Search for functions/procedures
    (save-match-data
     (while (re-search-forward
             (or regexp ada-procedure-start-regexp)
             nil t)
       ;(imenu-progress-message prev-pos)
       ;; do not store forward definitions
       ;; right now we store them. We want to avoid them only in
       ;; package bodies, not in the specs!! ???RE???
       (save-match-data
;        (if (not (looking-at (concat
;                              "[ \t\n]*" ; WS
;                              "\([^)]+\)" ; parameterlist
;                              "\\([ \n\t]+return[ \n\t]+"; potential return
;                              "[a-zA-Z0-9_\\.]+\\)?"
;                              "[ \t]*" ; WS
;                              ";"  ;; THIS is what we really look for
;                              )))
;            ; (push (imenu-example--name-and-position) index-alist)
            (setq index-alist (cons (imenu-example--name-and-position)
                        index-alist))
;          )
	)
       ;(imenu-progress-message 100)
       ))
    (nreverse index-alist)))

;;;---------------------------------------------------
;;; support for font-lock
;;;---------------------------------------------------

;; Strings are a real pain in Ada because both ' and " can appear in a
;; non-string quote context (the former as an operator, the latter as
;; a character string).  We follow the least losing solution, in which
;; only " is a string quote.  Therefore a character string of the form
;; '"' will throw fontification off on the wrong track.

(defconst ada-font-lock-keywords-1
  (list
   ;;
   ;; accept, entry, function, package (body), protected (body|type),
   ;; pragma, procedure, task (body) plus name.
   (list (concat
	  "\\<\\("
	  "accept\\|"
	  "entry\\|"
	  "function\\|"
	  "package\\|"
	  "package[ \t]+body\\|"
	  "procedure\\|"
	  "protected\\|"
	  "protected[ \t]+body\\|"
	  "protected[ \t]+type\\|"
;;	  "p\\(\\(ackage\\|rotected\\)\\(\\|[ \t]+\\(body\\|type\\)\\)\
;;\\|r\\(agma\\|ocedure\\)\\)\\|"
	  "task\\|"
	  "task[ \t]+body\\|"
	  "task[ \t]+type"
;;	  "task\\(\\|[ \t]+body\\)"
	  "\\)\\>[ \t]*"
	  "\\(\\sw+\\(\\.\\sw*\\)*\\)?")
    '(1 font-lock-keyword-face) '(2 font-lock-function-name-face nil t)))
  "For consideration as a value of `ada-font-lock-keywords'.
This does fairly subdued highlighting.")

(defconst ada-font-lock-keywords-2
  (append ada-font-lock-keywords-1
   (list
    ;;
    ;; Main keywords, except those treated specially below.
    (concat "\\<\\("
;    ("abort" "abs" "abstract" "accept" "access" "aliased" "all"
;     "and" "array" "at" "begin" "case" "declare" "delay" "delta"
;     "digits" "do" "else" "elsif" "entry" "exception" "exit" "for"
;     "generic" "if" "in" "is" "limited" "loop" "mod" "not"
;     "null" "or" "others" "private" "protected"
;     "range" "record" "rem" "renames" "requeue" "return" "reverse"
;     "select" "separate" "tagged" "task" "terminate" "then" "until"
;     "while" "xor")
            "a\\(b\\(ort\\|s\\(\\|tract\\)\\)\\|cce\\(pt\\|ss\\)\\|"
            "l\\(iased\\|l\\)\\|nd\\|rray\\|t\\)\\|begin\\|case\\|"
            "d\\(e\\(clare\\|l\\(ay\\|ta\\)\\)\\|igits\\|o\\)\\|"
            "e\\(ls\\(e\\|if\\)\\|ntry\\|x\\(ception\\|it\\)\\)\\|for\\|"
            "generic\\|i[fns]\\|l\\(imited\\|oop\\)\\|mod\\|n\\(ot\\|ull\\)\\|"
            "o\\(r\\|thers\\|ut\\)\\|pr\\(ivate\\|otected\\)\\|"
            "r\\(ange\\|e\\(cord\\|m\\|names\\|queue\\|turn\\|verse\\)\\)\\|"
            "se\\(lect\\|parate\\)\\|"
            "t\\(agged\\|erminate\\|hen\\)\\|until\\|" ; task removed
	    "wh\\(ile\\|en\\)\\|xor" ; "when" added
            "\\)\\>")
    ;;
    ;; Anything following end and not already fontified is a body name.
    '("\\<\\(end\\)\\>[ \t]+\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
    ;;
    ;; Variable name plus optional keywords followed by a type name.  Slow.
;    (list (concat "\\<\\(\\sw+\\)\\>[ \t]*:?[ \t]*"
;                 "\\(access\\|constant\\|in\\|in[ \t]+out\\|out\\)?[ \t]*"
;                 "\\(\\sw+\\)?")
;         '(1 font-lock-variable-name-face)
;         '(2 font-lock-keyword-face nil t) '(3 font-lock-type-face nil t))
    ;;
    ;; Optional keywords followed by a type name.
    (list (concat ; ":[ \t]*"
                  "\\<\\(access\\|constant\\|in\\|in[ \t]+out\\|out\\)\\>"
                  "[ \t]*"
                  "\\(\\sw+\\)?")
          '(1 font-lock-keyword-face nil t) '(2 font-lock-type-face nil t))
    ;;
    ;; Keywords followed by a type or function name.
    (list (concat "\\<\\("
                  "new\\|of\\|subtype\\|type"
                  "\\)\\>[ \t]*\\(\\sw+\\)?[ \t]*\\((\\)?")
          '(1 font-lock-keyword-face)
          '(2 (if (match-beginning 4)
                  font-lock-function-name-face
                font-lock-type-face) nil t))
    ;;
    ;; Keywords followed by a (comma separated list of) reference.
    (list (concat "\\<\\(goto\\|raise\\|use\\|with\\)\\>" ; "when" removed
                  ; "[ \t]*\\(\\sw+\\(\\.\\sw*\\)*\\)?") ; RE
                  "[ \t]*\\([a-zA-Z0-9_\\.\\|, ]+\\)\\W")
          '(1 font-lock-keyword-face) '(2 font-lock-reference-face nil t))
    ;;
    ;; Goto tags.
    '("<<\\(\\sw+\\)>>" 1 font-lock-reference-face)
    ))
  "For consideration as a value of `ada-font-lock-keywords'.
This does a lot more highlighting.")

(defvar ada-font-lock-keywords (if font-lock-maximum-decoration
				   ada-font-lock-keywords-2
				 ada-font-lock-keywords-1)
  "*Expressions to highlight in Ada mode.")

;;;
;;; ????
;;;
(defun ada-gen-comment-until-proc ()
  ;; comment until spec of a procedure or a function.
  (forward-line 1)
  (set-mark-command (point))
  (if (re-search-forward ada-procedure-start-regexp nil t)
      (progn (goto-char (match-beginning 1))
             (comment-region (mark) (point)))
    (error "No more functions/procedures")))


(defun ada-gen-treat-proc (match)
  ;; make dummy body of a procedure/function specification.
  ;; MATCH is a cons cell containing the start and end location of the
  ;; last search for ada-procedure-start-regexp. 
  (goto-char (car match))
  (let (proc-found func-found)
    (cond
     ((or (setq proc-found (looking-at "^[ \t]*procedure"))
	  (setq func-found (looking-at "^[ \t]*function")))
      ;; treat it as a proc/func
      (forward-word 2) 
      (forward-word -1)
      (setq procname (buffer-substring (point) (cdr match))) ; store  proc name

    ;; goto end of procname
    (goto-char (cdr match))

    ;; skip over parameterlist
    (forward-sexp)
    ;; if function, skip over 'return' and result type.
    (if func-found
	(progn
	  (forward-word 1)
	  (skip-chars-forward " \t\n")
	  (setq functype (buffer-substring (point)
					   (progn 
					     (skip-chars-forward
					      "a-zA-Z0-9_\.")
					     (point))))))
    ;; look for next non WS
    (cond
     ((looking-at "[ \t]*;")
      (delete-region (match-beginning 0) (match-end 0)) ;; delete the ';'
      (ada-indent-newline-indent)
      (insert " is")
      (ada-indent-newline-indent)
      (if func-found
	  (progn
	    (insert "Result : ")
	    (insert functype)
	    (insert ";")
	    (ada-indent-newline-indent)))
      (insert "begin -- ")
      (insert procname)
      (ada-indent-newline-indent)
      (insert "null;")
      (ada-indent-newline-indent)
      (if func-found
	  (progn
	    (insert "return Result;")
	    (ada-indent-newline-indent)))
      (insert "end ")
      (insert procname)
      (insert ";")
      (ada-indent-newline-indent)	
      )
      ;; else
     ((looking-at "[ \t\n]*is")
      ;; do nothing
      )
     ((looking-at "[ \t\n]*rename")
      ;; do nothing
      )
     (t
      (message "unknown syntax")))
    ))))


(defun ada-make-body ()
  "Create an Ada package body in the current buffer.
The potential old buffer contents is deleted first, then we copy the
spec buffer in here and modify it to make it a body.

This function typically is to be hooked into `ff-file-created-hooks'."
  (interactive)
  (delete-region (point-min) (point-max))
  (insert-buffer (car (cdr (buffer-list))))
  (ada-mode)

  (let (found)
    (if (setq found 
	      (ada-search-ignore-string-comment ada-package-start-regexp))
	(progn (goto-char (cdr found))
	       (insert " body")
	       ;; (forward-line -1)
	       ;;(comment-region (point-min) (point))
	       )
      (error "No package"))
    
    ;; (comment-until-proc)
    ;;   does not work correctly
    ;;   must be done by hand
    
    (while (setq found
		 (ada-search-ignore-string-comment ada-procedure-start-regexp))
      (ada-gen-treat-proc found))))


;;; provide ourself

(provide 'ada-mode)

;;; ada-mode.el ends here
