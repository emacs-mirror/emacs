;;; viper-ex.el -- functions implementing the Ex commands for Viper

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


(require 'viper-util)

;;; Variables

(defconst vip-ex-work-buf-name " *ex-working-space*")
(defconst vip-ex-work-buf (get-buffer-create vip-ex-work-buf-name))


;;; Completion in :set command
  
;; The list of Ex commands. Used for completing command names.
(defconst ex-token-alist
  '(("!") ("=") (">") ("&") ("~")
    ("yank") ("xit") ("WWrite") ("Write") ("write") ("wq") ("visual") 
    ("version") ("vglobal") ("unmap") ("undo") ("tag") ("transfer") ("suspend")
    ("substitute") ("submitReport") ("stop")  ("sr") ("source") ("shell")
    ("set") ("rewind") ("recover") ("read") ("quit") ("pwd")
    ("put") ("preserve") ("PreviousRelatedFile") ("RelatedFile")
    ("next") ("Next") ("move") ("mark") ("map") ("kmark") ("join")
    ("help") ("goto") ("global") ("file") ("edit") ("delete") ("copy")
    ("chdir") ("cd") ("Buffer") ("buffer") ("args"))  )

;; A-list of Ex variables that can be set using the :set command.
(defconst ex-variable-alist 
  '(("wrapscan") ("ws") ("wrapmargin") ("wm")
    ("tab-stop-local") ("tsl") ("tabstop") ("ts")
    ("showmatch") ("sm") ("shiftwidth") ("sw") ("shell") ("sh")
    ("readonly") ("ro") 
    ("nowrapscan") ("nows") ("noshowmatch") ("nosm")
    ("noreadonly") ("noro") ("nomagic") ("noma")
    ("noignorecase") ("noic") ("noautoindent") ("noai")
    ("magic") ("ma") ("ignorecase") ("ic") ("autoindent") ("ai")
    ))

  

;; Token recognized during parsing of Ex commands (e.g., "read", "comma")
(defvar ex-token nil)

;; Type of token. 
;; If non-nil, gives type of address; if nil, it is a command.
(defvar ex-token-type nil)

;; List of addresses passed to Ex command
(defvar ex-addresses nil)

;; It seems that this flag is used only for `#', `print', and `list', which
;; aren't implemented. Check later.
(defvar ex-flag nil)

;; "buffer" where Ex commands keep deleted data.
;; In Emacs terms, this is a register.
(defvar ex-buffer nil)

;; Value of ex count.
(defvar ex-count nil)

;; Flag for global command.
(defvar ex-g-flag nil)

;; If t, global command is executed on lines not matching ex-g-pat.
(defvar ex-g-variant nil)

;; Save reg-exp used in substitute.
(defvar ex-reg-exp nil)


;; Replace pattern for substitute.
(defvar ex-repl nil)

;; Pattern for global command.
(defvar ex-g-pat nil)

;; `sh' doesn't seem to expand wildcards, like `*'
(defconst ex-find-file-shell "csh"
  "Shell in which to interpret wildcards.")
(defvar ex-find-file-shell-options "-f"
  "*Options to pass to `ex-find-file-shell'.")

;; Remembers the previous Ex tag.
(defvar ex-tag nil)

;; file used by Ex commands like :r, :w, :n
(defvar ex-file nil)

;; If t, tells Ex that this is a variant-command, i.e., w>>, r!, etc.
(defvar ex-variant nil)

;; Specified the offset of an Ex command, such as :read.
(defvar ex-offset nil)

;; Tells Ex that this is a w>> command.
(defvar ex-append nil)

;; File containing the shell command to be executed at Ex prompt,
;; e.g., :r !date
(defvar ex-cmdfile nil)
  
;; flag used in vip-ex-read-file-name to indicate that we may be reading
;; multiple file names. Used for :edit and :next
(defvar vip-keep-reading-filename nil)

(defconst ex-cycle-other-window t
  "*If t, :n and :b cycles through files and buffers in other window.
Then :N and :B cycles in the current window. If nil, this behavior is
reversed.")

(defconst ex-cycle-through-non-files nil
  "*Cycle through *scratch* and other buffers that don't visit any file.")

;; Last shell command executed with :! command.
(defvar vip-ex-last-shell-com nil)
  
;; Indicates if Minibuffer was exited temporarily in Ex-command.
(defvar vip-incomplete-ex-cmd nil)
  
;; Remembers the last ex-command prompt.
(defvar vip-last-ex-prompt "")


;;; Code
  
(defun vip-check-sub (str)
  "Check if ex-token is an initial segment of STR."
  (let ((length (length ex-token)))
    (if (and (<= length (length str))
  	     (string= ex-token (substring str 0 length)))
	(setq ex-token str)
      (setq ex-token-type 'non-command))))

(defun vip-get-ex-com-subr ()
  "Get a complete ex command."
  (let (case-fold-search)
    (set-mark (point))
    (re-search-forward "[a-zA-Z][a-zA-Z]*")
    (setq ex-token-type 'command)
    (setq ex-token (buffer-substring (point) (mark t)))
    (exchange-point-and-mark)
    (cond ((looking-at "a")
	   (cond ((looking-at "ab") (vip-check-sub "abbreviate"))
		 ((looking-at "ar") (vip-check-sub "args"))
		 (t (vip-check-sub "append"))))
	  ((looking-at "h") (vip-check-sub "help"))
	  ((looking-at "c")
	   (cond ((looking-at "cd") (vip-check-sub "cd"))
		 ((looking-at "ch") (vip-check-sub "chdir"))
		 ((looking-at "co") (vip-check-sub "copy"))
		 (t (vip-check-sub "change"))))
	  ((looking-at "d") (vip-check-sub "delete"))
	  ((looking-at "b") (vip-check-sub "buffer"))
	  ((looking-at "B") (vip-check-sub "Buffer"))
	  ((looking-at "e")
	   (if (looking-at "ex") (vip-check-sub "ex")
	     (vip-check-sub "edit")))
	  ((looking-at "f") (vip-check-sub "file"))
	  ((looking-at "g") (vip-check-sub "global"))
	  ((looking-at "i") (vip-check-sub "insert"))
	  ((looking-at "j") (vip-check-sub "join"))
	  ((looking-at "l") (vip-check-sub "list"))
	  ((looking-at "m")
	   (cond ((looking-at "map") (vip-check-sub "map"))
		 ((looking-at "mar") (vip-check-sub "mark"))
		 (t (vip-check-sub "move"))))
	  ((looking-at "k[a-z][^a-z]")
	   (setq ex-token "kmark")
	   (forward-char 1)
	   (exchange-point-and-mark)) ;; this is canceled out by another
				      ;; exchange-point-and-mark at the end
	  ((looking-at "k") (vip-check-sub "kmark"))
	  ((looking-at "n") (if (looking-at "nu")
				(vip-check-sub "number")
			      (vip-check-sub "next")))
	  ((looking-at "N") (vip-check-sub "Next"))
	  ((looking-at "o") (vip-check-sub "open"))
	  ((looking-at "p")
	   (cond ((looking-at "pre") (vip-check-sub "preserve"))
		 ((looking-at "pu") (vip-check-sub "put"))
		 ((looking-at "pw") (vip-check-sub "pwd"))
		 (t (vip-check-sub "print"))))
	  ((looking-at "P") (vip-check-sub "PreviousRelatedFile"))
	  ((looking-at "R") (vip-check-sub "RelatedFile"))
	  ((looking-at "q") (vip-check-sub "quit"))
	  ((looking-at "r")
	   (cond ((looking-at "rec") (vip-check-sub "recover"))
		 ((looking-at "rew") (vip-check-sub "rewind"))
		 (t (vip-check-sub "read"))))
	  ((looking-at "s")
	   (cond ((looking-at "se") (vip-check-sub "set"))
		 ((looking-at "sh") (vip-check-sub "shell"))
		 ((looking-at "so") (vip-check-sub "source"))
		 ((looking-at "sr") (vip-check-sub "sr"))
		 ((looking-at "st") (vip-check-sub "stop"))
		 ((looking-at "sus") (vip-check-sub "suspend"))
		 ((looking-at "subm") (vip-check-sub "submitReport"))
		 (t (vip-check-sub "substitute"))))
	  ((looking-at "t")
	   (if (looking-at "ta") (vip-check-sub "tag")
	     (vip-check-sub "transfer")))
	  ((looking-at "u")
	   (cond ((looking-at "una") (vip-check-sub "unabbreviate"))
		 ((looking-at "unm") (vip-check-sub "unmap"))
		 (t (vip-check-sub "undo"))))
	  ((looking-at "v")
	   (cond ((looking-at "ve") (vip-check-sub "version"))
		 ((looking-at "vi") (vip-check-sub "visual"))
		 (t (vip-check-sub "vglobal"))))
	  ((looking-at "w")
	   (if (looking-at "wq") (vip-check-sub "wq")
	     (vip-check-sub "write")))
	  ((looking-at "W")
	   (if (looking-at "WW") 
	       (vip-check-sub "WWrite")
	     (vip-check-sub "Write")))
	  ((looking-at "x") (vip-check-sub "xit"))
	  ((looking-at "y") (vip-check-sub "yank"))
	  ((looking-at "z") (vip-check-sub "z")))
    (exchange-point-and-mark)
    ))

(defun vip-get-ex-token ()
  "Get an ex-token which is either an address or a command.
A token has a type, \(command, address, end-mark\), and a value."
  (save-window-excursion
    (set-buffer vip-ex-work-buf)
    (skip-chars-forward " \t|")
    (cond ((looking-at "#")
	   (setq ex-token-type 'command)
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  ((looking-at "[a-z]") (vip-get-ex-com-subr))
	  ((looking-at "\\.")
	   (forward-char 1)
	   (setq ex-token-type 'dot))
	  ((looking-at "[0-9]")
	   (set-mark (point))
	   (re-search-forward "[0-9]*")
	   (setq ex-token-type
		 (cond ((eq ex-token-type 'plus) 'add-number)
		       ((eq ex-token-type 'minus) 'sub-number)
		       (t 'abs-number)))
	   (setq ex-token (string-to-int (buffer-substring (point) (mark t)))))
	  ((looking-at "\\$")
	   (forward-char 1)
	   (setq ex-token-type 'end))
	  ((looking-at "%")
	   (forward-char 1)
	   (setq ex-token-type 'whole))
	  ((looking-at "+")
	   (cond ((or (looking-at "+[-+]") (looking-at "+[\n|]"))
		  (forward-char 1)
		  (insert "1")
		  (backward-char 1)
		  (setq ex-token-type 'plus))
		 ((looking-at "+[0-9]")
		  (forward-char 1)
		  (setq ex-token-type 'plus))
		 (t
		  (error vip-BadAddress))))
	  ((looking-at "-")
	   (cond ((or (looking-at "-[-+]") (looking-at "-[\n|]"))
		  (forward-char 1)
		  (insert "1")
		  (backward-char 1)
		  (setq ex-token-type 'minus))
		 ((looking-at "-[0-9]")
		  (forward-char 1)
		  (setq ex-token-type 'minus))
		 (t
		  (error vip-BadAddress))))
	  ((looking-at "/")
	   (forward-char 1)
	   (set-mark (point))
	   (let ((cont t))
	     (while (and (not (eolp)) cont)
	       ;;(re-search-forward "[^/]*/")
	       (re-search-forward "[^/]*\\(/\\|\n\\)")
	       (if (not (vip-looking-back "[^\\\\]\\(\\\\\\\\\\)*\\\\/"))
		   (setq cont nil))))
	   (backward-char 1)
	   (setq ex-token (buffer-substring (point) (mark t)))
	   (if (looking-at "/") (forward-char 1))
	   (setq ex-token-type 'search-forward))
	  ((looking-at "\\?")
	   (forward-char 1)
	   (set-mark (point))
	   (let ((cont t))
	     (while (and (not (eolp)) cont)
	       ;;(re-search-forward "[^\\?]*\\?")
	       (re-search-forward "[^\\?]*\\(\\?\\|\n\\)")
	       (if (not (vip-looking-back "[^\\\\]\\(\\\\\\\\\\)*\\\\\\?"))
		   (setq cont nil))
	       (backward-char 1)
	       (if (not (looking-at "\n")) (forward-char 1))))
	   (setq ex-token-type 'search-backward)
	   (setq ex-token (buffer-substring (1- (point)) (mark t))))
	  ((looking-at ",")
	   (forward-char 1)
	   (setq ex-token-type 'comma))
	  ((looking-at ";")
	   (forward-char 1)
	   (setq ex-token-type 'semi-colon))
	  ((looking-at "[!=><&~]")
	   (setq ex-token-type 'command)
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  ((looking-at "'")
	   (setq ex-token-type 'goto-mark)
	   (forward-char 1)
	   (cond ((looking-at "'") (setq ex-token nil))
		 ((looking-at "[a-z]") (setq ex-token (following-char)))
		 (t (error "Marks are ' and a-z")))
	   (forward-char 1))
	  ((looking-at "\n")
	   (setq ex-token-type 'end-mark)
	   (setq ex-token "goto"))
	  (t
	   (error vip-BadExCommand)))))

;; Reads Ex command. Tries to determine if it has to exit because command
;; is complete or invalid. If not, keeps reading command.
(defun ex-cmd-read-exit ()
  (interactive)
  (setq vip-incomplete-ex-cmd t)
  (let ((quit-regex1 (concat
		      "\\("
		      "set[ \t]*" "\\|" "edit[ \t]*" "\\|" "[nN]ext[ \t]*"
		      "\\|" "unm[ \t]*" "\\|" "^[ \t]*rep"
		      "\\)"))
	(quit-regex2 (concat
		      "[a-zA-Z][ \t]*"
		      "\\(" "!" "\\|" ">>"
		      "\\|" "\\+[0-9]+"
		      "\\)"
		      "*[ \t]*$"))
	(stay-regex (concat
		     "\\("
		     "^[ \t]*$" "\\|" "[ktgjmsz][ \t]*$" "\\|" "^[ \t]*ab.*"
		     "\\|" "tr[ansfer \t]*" "\\|" "sr[ \t]*"
		     "\\|" "mo.*" "\\|" "^[ \t]*k?ma[^p]*"
		     "\\|" "^[ \t]*fi.*" "\\|" "v?gl.*" "\\|" "[vg][ \t]*$"
		     "\\|" "jo.*" "\\|" "^[ \t]*ta.*" "\\|" "^[ \t]*una.*"
		     "\\|" "^[ \t]*su.*" "\\|['`][a-z][ \t]*"
		     "\\|" "![ \t]*[a-zA-Z].*"
		     "\\)"
		     "!*")))
	
    (save-window-excursion ;; put cursor at the end of the Ex working buffer
      (set-buffer vip-ex-work-buf)
      (goto-char (point-max)))
    (cond ((vip-looking-back quit-regex1) (exit-minibuffer))
	  ((vip-looking-back stay-regex)  (insert " "))
	  ((vip-looking-back quit-regex2) (exit-minibuffer))
	  (t (insert " ")))))
  
;; complete Ex command
(defun ex-cmd-complete ()
  (interactive)
  (let (save-pos dist compl-list string-to-complete completion-result)
    
    (save-excursion
      (setq dist (skip-chars-backward "[a-zA-Z!=>&~]")
	    save-pos (point)))
	
    (if (or (= dist 0)
	    (vip-looking-back "\\([ \t]*['`][ \t]*[a-z]*\\)")
	    (vip-looking-back
	     "^[ \t]*[a-zA-Z!=>&~][ \t]*[/?]*+[ \t]+[a-zA-Z!=>&~]+"))
	;; Preceding characters are not the ones allowed in an Ex command
	;; or we have typed past command name.
	;; Note: we didn't do parsing, so there may be surprises.
	(if (or (vip-looking-back "[a-zA-Z!=>&~][ \t]*[/?]*[ \t]*")
		(vip-looking-back "\\([ \t]*['`][ \t]*[a-z]*\\)")
		(looking-at "[^ \t\n\C-m]"))
	    nil
	  (with-output-to-temp-buffer "*Completions*" 
	    (display-completion-list
	     (vip-alist-to-list ex-token-alist))))
      ;; Preceding chars may be part of a command name
      (setq string-to-complete (buffer-substring save-pos (point)))
      (setq completion-result
	    (try-completion string-to-complete ex-token-alist))
      
      (cond ((eq completion-result t)  ;; exact match--do nothing
	     (vip-tmp-insert-at-eob " (Sole completion)"))
	    ((eq completion-result nil)
	     (vip-tmp-insert-at-eob " (No match)"))
	    (t  ;; partial completion
	     (goto-char save-pos)
	     (delete-region (point) (point-max))
	     (insert completion-result)
	     (let (case-fold-search)
	       (setq compl-list
		     (vip-filter-alist (concat "^" completion-result)
				       ex-token-alist)))
	     (if (> (length compl-list) 1)
		 (with-output-to-temp-buffer "*Completions*" 
		   (display-completion-list
		    (vip-alist-to-list (reverse compl-list)))))))
      )))
    
(defun vip-ex (&optional string)
  "Ex commands within Viper."
  (interactive)
  (or string
      (setq ex-g-flag nil
	    ex-g-variant nil))
  (let* ((map (copy-keymap minibuffer-local-map))
	 (address nil)
	 (cont t)
	 (dot (point))
	 com-str)
	 
    (vip-add-keymap vip-ex-cmd-map map)
    
    (setq com-str (or string (vip-read-string-with-history
			      ":" 
			      nil
			      'vip-ex-history
			      (car vip-ex-history)
			      map)))
    (save-window-excursion
      ;; just a precaution
      (or (vip-buffer-live-p vip-ex-work-buf)
	  (setq vip-ex-work-buf (get-buffer-create vip-ex-work-buf-name)))
      (set-buffer vip-ex-work-buf)
      (delete-region (point-min) (point-max))
      (insert com-str "\n")
      (goto-char (point-min)))
    (setq ex-token-type nil
	  ex-addresses nil)
    (while cont
      (vip-get-ex-token)
      (cond ((memq ex-token-type '(command end-mark))
	     (if address (setq ex-addresses (cons address ex-addresses)))
	     (cond ((string= ex-token "global")
		    (ex-global nil)
		    (setq cont nil))
		   ((string= ex-token "vglobal")
		    (ex-global t)
		    (setq cont nil))
		   (t
		    (vip-execute-ex-command)
		    (save-window-excursion
		      (set-buffer vip-ex-work-buf)
		      (skip-chars-forward " \t")
		      (cond ((looking-at "|")
			     (forward-char 1))
			    ((looking-at "\n")
			     (setq cont nil))
			    (t (error "`%s': %s" ex-token vip-SpuriousText)))
		      ))
		   ))
	    ((eq ex-token-type 'non-command)
	     (error (format "`%s': %s" ex-token vip-BadExCommand)))
	    ((eq ex-token-type 'whole)
	     (setq ex-addresses
		   (cons (point-max) (cons (point-min) ex-addresses))))
	    ((eq ex-token-type 'comma)
	     (setq ex-addresses
		   (cons (if (null address) (point) address) ex-addresses)))
	    ((eq ex-token-type 'semi-colon)
	     (if address (setq dot address))
	     (setq ex-addresses
		   (cons (if (null address) (point) address) ex-addresses)))
	    (t (let ((ans (vip-get-ex-address-subr address dot)))
		 (if ans (setq address ans))))))))

(defun vip-get-ex-pat ()
  "Get a regular expression and set `ex-variant', if found."
  (save-window-excursion
    (set-buffer vip-ex-work-buf)
    (skip-chars-forward " \t")
    (if (looking-at "!")
	(progn
	  (setq ex-g-variant (not ex-g-variant)
		ex-g-flag (not ex-g-flag))
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (let ((c (following-char)))
      (if (string-match "[0-9A-Za-z]" (format "%c" c))
	  (error
	   "Global regexp must be inside matching non-alphanumeric chars"))
      (if (looking-at "[^\\\\\n]")
	  (progn
	    (forward-char 1)
	    (set-mark (point))
	    (let ((cont t))
	      (while (and (not (eolp)) cont)
		(if (not (re-search-forward (format "[^%c]*%c" c c) nil t))
		    (if (member ex-token '("global" "vglobal"))
			(error
			 "Missing closing delimiter for global regexp")
		      (goto-char (point-max))))
		(if (not (vip-looking-back
			  (format "[^\\\\]\\(\\\\\\\\\\)*\\\\%c" c)))
		    (setq cont nil))))
	    (setq ex-token
		  (if (= (mark t) (point)) ""
		    (buffer-substring (1- (point)) (mark t))))
	    (backward-char 1))
	(setq ex-token nil))
      c)))

(defun vip-get-ex-command ()
  "get an ex command"
  (save-window-excursion
    (set-buffer vip-ex-work-buf)
    (if (looking-at "/") (forward-char 1))
    (skip-chars-forward " \t")
    (cond ((looking-at "[a-z]")
	   (vip-get-ex-com-subr)
	   (if (eq ex-token-type 'non-command)
	       (error "`%s': %s" ex-token vip-BadExCommand)))
	  ((looking-at "[!=><&~]")
	   (setq ex-token (char-to-string (following-char)))
	   (forward-char 1))
	  (t (error vip-BadExCommand)))))

(defun vip-get-ex-opt-gc (c)
  "Get an Ex option g or c."
  (save-window-excursion
    (set-buffer vip-ex-work-buf)
    (if (looking-at (format "%c" c)) (forward-char 1))
    (skip-chars-forward " \t")
    (cond ((looking-at "g")
	   (setq ex-token "g")
	   (forward-char 1)
	   t)
	  ((looking-at "c")
	   (setq ex-token "c")
	   (forward-char 1)
	   t)
	  (t nil))))

(defun vip-default-ex-addresses (&optional whole-flag)
  "Compute default addresses.  WHOLE-FLAG means use the whole buffer."
  (cond ((null ex-addresses)
	 (setq ex-addresses
	       (if whole-flag
		   (cons (point-max) (cons (point-min) nil))
		 (cons (point) (cons (point) nil)))))
	((null (cdr ex-addresses))
	 (setq ex-addresses
	       (cons (car ex-addresses) ex-addresses)))))

(defun vip-get-ex-address ()
  "Get an ex-address as a marker and set ex-flag if a flag is found."
  (let ((address (point-marker)) (cont t))
    (setq ex-token "")
    (setq ex-flag nil)
    (while cont
      (vip-get-ex-token)
      (cond ((eq ex-token-type 'command)
	     (if (member ex-token '("print" "list" "#"))
		 (progn
		   (setq ex-flag t
			 cont nil))
	       (error "Address expected in this Ex command")))
	    ((eq ex-token-type 'end-mark)
	     (setq cont nil))
	    ((eq ex-token-type 'whole)
	     (error "Trailing address expected"))
	    ((eq ex-token-type 'comma)
	     (error "`%s': %s" ex-token vip-SpuriousText))
	    (t (let ((ans (vip-get-ex-address-subr address (point-marker))))
		 (if ans (setq address ans))))))
    address))

(defun vip-get-ex-address-subr (old-address dot)
  "Returns an address as a point."
  (let ((address nil))
    (if (null old-address) (setq old-address dot))
    (cond ((eq ex-token-type 'dot)
	   (setq address dot))
	  ((eq ex-token-type 'add-number)
	   (save-excursion
	     (goto-char old-address)
	     (forward-line (if (= old-address 0) (1- ex-token) ex-token))
	     (setq address (point-marker))))
	  ((eq ex-token-type 'sub-number)
	   (save-excursion
	     (goto-char old-address)
	     (forward-line (- ex-token))
	     (setq address (point-marker))))
	  ((eq ex-token-type 'abs-number)
	   (save-excursion
	     (goto-char (point-min))
	     (if (= ex-token 0) (setq address 0)
	       (forward-line (1- ex-token))
	       (setq address (point-marker)))))
	  ((eq ex-token-type 'end)
	   (setq address (point-max-marker)))
	  ((eq ex-token-type 'plus) t);; do nothing
	  ((eq ex-token-type 'minus) t);; do nothing
	  ((eq ex-token-type 'search-forward)
	   (save-excursion
	     (ex-search-address t)
	     (setq address (point-marker))))
	  ((eq ex-token-type 'search-backward)
	   (save-excursion
	     (ex-search-address nil)
	     (setq address (point-marker))))
	  ((eq ex-token-type 'goto-mark)
	   (save-excursion
	     (if (null ex-token)
		 (exchange-point-and-mark)
	       (goto-char (vip-register-to-point
			   (1+ (- ex-token ?a)) 'enforce-buffer)))
	     (setq address (point-marker)))))
    address))


(defun ex-search-address (forward)
  "Search pattern and set address."
  (if (string= ex-token "")
      (if (null vip-s-string)
	  (error vip-NoPrevSearch)
	(setq ex-token vip-s-string))
    (setq vip-s-string ex-token))
  (if forward
      (progn
	(forward-line 1)
	(re-search-forward ex-token))
    (forward-line -1)
    (re-search-backward ex-token)))

(defun vip-get-ex-buffer ()
  "Get a buffer name and set `ex-count' and `ex-flag' if found."
  (setq ex-buffer nil)
  (setq ex-count nil)
  (setq ex-flag nil)
  (save-window-excursion
    (set-buffer vip-ex-work-buf)
    (skip-chars-forward " \t")
    (if (looking-at "[a-zA-Z]")
	(progn
	  (setq ex-buffer (following-char))
	  (forward-char 1)
	  (skip-chars-forward " \t")))
    (if (looking-at "[0-9]")
	(progn
	  (set-mark (point))
	  (re-search-forward "[0-9][0-9]*")
	  (setq ex-count (string-to-int (buffer-substring (point) (mark t))))
	  (skip-chars-forward " \t")))
    (if (looking-at "[pl#]")
	(progn
	  (setq ex-flag t)
	  (forward-char 1)))
    (if (not (looking-at "[\n|]"))
	(error "`%s': %s" ex-token vip-SpuriousText))))

(defun vip-get-ex-count ()
  (setq ex-variant nil
	ex-count nil
	ex-flag nil)
  (save-window-excursion
    (set-buffer vip-ex-work-buf)
    (skip-chars-forward " \t")
    (if (looking-at "!")
	(progn
	  (setq ex-variant t)
	  (forward-char 1)))
    (skip-chars-forward " \t")
    (if (looking-at "[0-9]")
	(progn
	  (set-mark (point))
	  (re-search-forward "[0-9][0-9]*")
	  (setq ex-count (string-to-int (buffer-substring (point) (mark t))))
	  (skip-chars-forward " \t")))
    (if (looking-at "[pl#]")
	(progn
	  (setq ex-flag t)
	  (forward-char 1)))
    (if (not (looking-at "[\n|]"))
	(error "`%s': %s"
	       (buffer-substring (point-min) (1- (point-max))) vip-BadExCommand))))

(defun ex-expand-filsyms (cmd buf)
  "Expand \% and \# in ex command."
  (let (cf pf ret)
    (save-excursion 
      (set-buffer buf)
      (setq cf buffer-file-name)
      (setq pf (ex-next nil t))) ;; this finds alternative file name
    (if (and (null cf) (string-match "[^\\]%\\|\\`%" cmd))
	(error "No current file to substitute for `\%'"))
    (if (and (null pf) (string-match "[^\\]#\\|\\`#" cmd))
	(error "No alternate file to substitute for `#'"))
    (save-excursion
      (set-buffer (get-buffer-create " ex-tmp"))
      (insert cmd)
      (goto-char (point-min))
      (while (re-search-forward "%\\|#" nil t)
	(let ((data (match-data)) 
	      (char (buffer-substring (match-beginning 0) (match-end 0))))
	  (if (vip-looking-back (concat "\\\\" char))
	      (replace-match char)
	    (store-match-data data)
	    (if (string= char "%")
		(replace-match cf)
	      (replace-match pf)))))
      (end-of-line)
      (setq ret (buffer-substring (point-min) (point)))
      (kill-buffer (current-buffer))
      (message "%s" ret))
    ret))

(defun vip-get-ex-file ()
  "Get a file name and set ex-variant, `ex-append' and `ex-offset' if found."
  (let (prompt)
    (setq ex-file nil
	  ex-variant nil
	  ex-append nil
	  ex-offset nil
	  ex-cmdfile nil)
    (save-excursion
      (save-window-excursion
	(set-buffer vip-ex-work-buf)
	(skip-chars-forward " \t")
	(if (looking-at "!")
	    (if (not (vip-looking-back "[ \t]"))
		(progn
		  (setq ex-variant t)
		  (forward-char 1)
		  (skip-chars-forward " \t"))
	      (setq ex-cmdfile t)
	      (forward-char 1)
	      (skip-chars-forward " \t")))
	(if (looking-at ">>")
	    (progn
	      (setq ex-append t
		    ex-variant t)
	      (forward-char 2)
	      (skip-chars-forward " \t")))
	(if (looking-at "+")
	    (progn
	      (forward-char 1)
	      (set-mark (point))
	      (re-search-forward "[ \t\n]")
	      (backward-char 1)
	      (setq ex-offset (buffer-substring (point) (mark t)))
	      (forward-char 1)
	      (skip-chars-forward " \t")))
	;; this takes care of :r, :w, etc., when they get file names
	;; from the history list
	(if (member ex-token '("read" "write" "edit" "visual"))
	    (progn
	      (setq ex-file (buffer-substring (point)  (1- (point-max))))
	      (setq ex-file
		    ;; For :e, match multiple non-white strings separated
		    ;; by white. For others, find the first non-white string
		    (if (string-match
			 (if (string= ex-token "edit")
			     "[^ \t\n]+\\([ \t]+[^ \t\n]+\\)*"
			   "[^ \t\n]+")
			 ex-file)
			(progn
			  ;; if file name comes from history, don't leave
			  ;; minibuffer when the user types space
			  (setq vip-incomplete-ex-cmd nil)
			  ;; this must be the last clause in this progn
			  (substring ex-file (match-beginning 0) (match-end 0))
			  )
		      ""))
	      ;; this leaves only the command name in the work area
	      ;; file names are gone
	      (delete-region (point) (1- (point-max)))
	      ))
	(goto-char (point-max))
	(skip-chars-backward " \t\n")
	(setq prompt (buffer-substring (point-min) (point)))
	))
    
    (setq vip-last-ex-prompt prompt)
    
    ;; If we just finished reading command, redisplay prompt
    (if vip-incomplete-ex-cmd
	(setq ex-file (vip-ex-read-file-name (format ":%s " prompt)))
      ;; file was typed in-line
      (setq ex-file (or ex-file "")))
    ))


;; Completes file name or exits minibuffer. If Ex command accepts multiple
;; file names, arranges to re-enter the minibuffer.
(defun vip-complete-filename-or-exit ()
  (interactive)
  (setq vip-keep-reading-filename t) 
  ;; don't exit if directory---ex-commands don't 
  (cond ((ex-cmd-accepts-multiple-files-p ex-token) (exit-minibuffer))
	(t (minibuffer-complete-word))))
     
	       
(defun ex-cmd-accepts-multiple-files-p (token)
  (member token '("edit" "next" "Next")))

;; If user doesn't enter anything, then "" is returned, i.e., the
;; prompt-directory is not returned.
(defun vip-ex-read-file-name (prompt)
  (let* ((str "")
	 (minibuffer-local-completion-map
	  (copy-keymap minibuffer-local-completion-map))
	 beg end cont val)
    
    (vip-add-keymap ex-read-filename-map minibuffer-local-completion-map) 
		    
    (setq cont (setq vip-keep-reading-filename t))
    (while cont
      (setq vip-keep-reading-filename nil
	    val (read-file-name (concat prompt str) nil default-directory)
	    str  (concat str (if (equal val "") "" " ")
			 val (if (equal val "") "" " ")))
			 
      ;; Only edit, next, and Next commands accept multiple files.
      ;; vip-keep-reading-filename is set in the anonymous function that is
      ;; bound to " " in ex-read-filename-map.
      (setq cont (and vip-keep-reading-filename
		      (ex-cmd-accepts-multiple-files-p ex-token)))
      )
    
    (setq beg (string-match "[^ \t]" str)   ;; delete leading blanks
	  end (string-match "[ \t]*$" str)) ;; delete trailing blanks
    (if (member ex-token '("read" "write"))
	  (if (string-match "[\t ]*!" str)
	      ;; this is actually a shell command
	      (progn
		(setq ex-cmdfile t)
		(setq beg (1+ beg))
		(setq vip-last-ex-prompt (concat vip-last-ex-prompt " !")))))
    (substring str (or beg 0) end)))

(defun vip-execute-ex-command ()
  "Execute ex command using the value of addresses."
  (vip-deactivate-mark)
  (cond ((string= ex-token "args") (ex-args))
	((string= ex-token "copy") (ex-copy nil))
	((string= ex-token "cd") (ex-cd))
	((string= ex-token "chdir") (ex-cd))
	((string= ex-token "delete") (ex-delete))
	((string= ex-token "edit") (ex-edit))
	((string= ex-token "file") (vip-info-on-file))
	((string= ex-token "goto") (ex-goto))
	((string= ex-token "help") (ex-help))
	((string= ex-token "join") (ex-line "join"))
	((string= ex-token "kmark") (ex-mark))
	((string= ex-token "mark") (ex-mark))
	((string= ex-token "map") (ex-map))
	((string= ex-token "move") (ex-copy t))
	((string= ex-token "next") (ex-next ex-cycle-other-window))
	((string= ex-token "Next") (ex-next (not ex-cycle-other-window)))
	((string= ex-token "RelatedFile") (ex-next-related-buffer 1))
	((string= ex-token "put") (ex-put))
	((string= ex-token "pwd") (ex-pwd))
	((string= ex-token "preserve") (ex-preserve))
	((string= ex-token "PreviousRelatedFile") (ex-next-related-buffer -1))
	((string= ex-token "quit") (ex-quit))
	((string= ex-token "read") (ex-read))
	((string= ex-token "recover") (ex-recover))
	((string= ex-token "rewind") (ex-rewind))
	((string= ex-token "submitReport") (vip-submit-report))
	((string= ex-token "set") (ex-set))
	((string= ex-token "shell") (ex-shell))
	((string= ex-token "source") (ex-source))
	((string= ex-token "sr") (ex-substitute t t))
	((string= ex-token "substitute") (ex-substitute))
	((string= ex-token "suspend") (suspend-emacs))
	((string= ex-token "stop") (suspend-emacs))
	((string= ex-token "transfer") (ex-copy nil))
	((string= ex-token "buffer") (if ex-cycle-other-window
					 (vip-switch-to-buffer-other-window)
					 (vip-switch-to-buffer)))
	((string= ex-token "Buffer") (if ex-cycle-other-window
					 (vip-switch-to-buffer)
					 (vip-switch-to-buffer-other-window)))
	((string= ex-token "tag") (ex-tag))
	((string= ex-token "undo") (vip-undo))
	((string= ex-token "unmap") (ex-unmap))
	((string= ex-token "version") (vip-version))
	((string= ex-token "visual") (ex-edit))
	((string= ex-token "write") (ex-write nil))
	((string= ex-token "Write") (save-some-buffers))
	((string= ex-token "wq") (ex-write t))
	((string= ex-token "WWrite") (save-some-buffers t)) ; don't ask
	((string= ex-token "xit") (ex-write t))
	((string= ex-token "yank") (ex-yank))
	((string= ex-token "!") (ex-command))
	((string= ex-token "=") (ex-line-no))
	((string= ex-token ">") (ex-line "right"))
	((string= ex-token "<") (ex-line "left"))
	((string= ex-token "&") (ex-substitute t))
	((string= ex-token "~") (ex-substitute t t))
	((or (string= ex-token "append")
	     (string= ex-token "change")
	     (string= ex-token "insert")
	     (string= ex-token "open"))
	 (error
	  (format "`%s': Obsolete command, not supported by Viper"
		  ex-token)))
	((or (string= ex-token "abbreviate")
	     (string= ex-token "unabbreviate"))
	 (error
	  (format
	   "`%s': Vi's abbrevs are obsolete. Use more powerful Emacs' abbrevs"
	   ex-token)))
	((or (string= ex-token "list")
	     (string= ex-token "print")
	     (string= ex-token "z")
	     (string= ex-token "#"))
	 (error
	  (format "`%s': Command not implemented in Viper" ex-token)))
	(t (error (format "`%s': %s" ex-token vip-BadExCommand)))))

(defun vip-undisplayed-files ()
  (mapcar
   (function 
    (lambda (b) 
      (if (null (get-buffer-window b))
	  (let ((f (buffer-file-name b)))
	    (if f f
	      (if ex-cycle-through-non-files 
		  (let ((s (buffer-name b)))
		    (if (string= " " (substring s 0 1))
			nil
		      s))
		nil)))
	nil)))
   (buffer-list)))


(defun ex-args ()
  (let ((l (vip-undisplayed-files))
	(args "")
	(file-count 1))
    (while (not (null l))
      (if (car l) 
	  (setq args (format "%s %d) %s\n" args file-count (car l))
		file-count (1+ file-count)))
      (setq l (cdr l)))
    (if (string= args "")
	(message "All files are already displayed")
      (save-excursion
	(save-window-excursion
	  (with-output-to-temp-buffer " *vip-info*"
	    (princ "\n\nThese files are not displayed in any window.\n")
	    (princ "\n=============\n")
	    (princ args)
	    (princ "\n=============\n")
	    (princ "\nThe numbers can be given as counts to :next. ")
	    (princ "\n\nPress any key to continue...\n\n"))
	  (vip-read-char-exclusive))))))

(defun ex-cd ()
  "Ex cd command. Default directory of this buffer changes."
  (vip-get-ex-file)
  (if (string= ex-file "")
      (setq ex-file "~"))
  (setq default-directory (file-name-as-directory (expand-file-name ex-file))))

(defun ex-copy (del-flag)
  "Ex copy and move command.  DEL-FLAG means delete."
  (vip-default-ex-addresses)
  (let ((address (vip-get-ex-address))
	(end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (goto-char end)
    (save-excursion
      (push-mark beg t)
      (vip-enlarge-region (mark t) (point))
      (if del-flag
	  (kill-region (point) (mark t))
	(copy-region-as-kill (point) (mark t)))
      (if ex-flag
	  (progn
	    (with-output-to-temp-buffer "*copy text*"
	      (princ
	       (if (or del-flag ex-g-flag ex-g-variant)
		   (current-kill 0)
		 (buffer-substring (point) (mark t)))))
	    (condition-case nil
		(progn
		  (read-string "[Hit return to continue] ")
		  (save-excursion (kill-buffer "*copy text*")))
	      (quit (save-excursion (kill-buffer "*copy text*"))
		    (signal 'quit nil))))))
    (if (= address 0)
	(goto-char (point-min))
      (goto-char address)
      (forward-line 1))
      (insert (current-kill 0))))

(defun ex-delete ()
  "Ex delete command."
  (vip-default-ex-addresses)
  (vip-get-ex-buffer)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error vip-FirstAddrExceedsSecond))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (if ex-count
	  (progn
	    (set-mark (point))
	    (forward-line (1- ex-count)))
	(set-mark end))
      (vip-enlarge-region (point) (mark t))
      (if ex-flag
	  ;; show text to be deleted and ask for confirmation
	  (progn
	    (with-output-to-temp-buffer " *delete text*"
	      (princ (buffer-substring (point) (mark t))))
	    (condition-case nil
		(read-string "[Hit return to continue] ")
	      (quit
	       (save-excursion (kill-buffer " *delete text*"))
	       (error "")))
	    (save-excursion (kill-buffer " *delete text*")))
	(if ex-buffer
	    (cond ((vip-valid-register ex-buffer '(Letter))
		   (vip-append-to-register
		    (downcase ex-buffer) (point) (mark t)))
		  ((vip-valid-register ex-buffer)
		   (copy-to-register ex-buffer (point) (mark t) nil))
		  (t (error vip-InvalidRegister ex-buffer))))
	(kill-region (point) (mark t))))))



(defun ex-edit (&optional file)
  "Ex edit command.
In Viper, `e' and `e!' behave identically. In both cases, the user is
asked if current buffer should really be discarded.
This command can take multiple file names. It replaces the current buffer
with the first file in its argument list."
  (if (not file)
      (vip-get-ex-file))
  (cond ((and (string= ex-file "") buffer-file-name)
	 (setq ex-file  (abbreviate-file-name (buffer-file-name))))
	((string= ex-file "")
	 (error vip-NoFileSpecified)))
      
  (let (msg do-edit)
    (if buffer-file-name
	(cond ((buffer-modified-p)
	       (setq msg
		     (format "Buffer %s is modified. Edit buffer? "
			     (buffer-name))
		     do-edit t))
	      ((not (verify-visited-file-modtime (current-buffer)))
	       (setq msg
		     (format "File %s changed on disk.  Reread from disk? "
			     buffer-file-name)
		     do-edit t))
	      (t (setq do-edit nil))))
      
    (if do-edit
	(if (yes-or-no-p msg)
	    (progn
	      (set-buffer-modified-p nil)
	      (kill-buffer (current-buffer)))
	  (message "Buffer %s was left intact" (buffer-name))))
    ) ; let
  
  (if (null (setq file (get-file-buffer ex-file)))
      (progn 
	(ex-find-file ex-file)
	(vip-change-state-to-vi)
	(goto-char (point-min)))
    (switch-to-buffer file))
  (if ex-offset
      (progn
	(save-window-excursion
	  (set-buffer vip-ex-work-buf)
	  (delete-region (point-min) (point-max))
	  (insert ex-offset "\n")
	  (goto-char (point-min)))
	(goto-char (vip-get-ex-address))
	(beginning-of-line)))
  (ex-fixup-history vip-last-ex-prompt ex-file))

;; splits the string FILESPEC into substrings separated by newlines `\012'
;; each line assumed to be a file name. find-file's each file thus obtained.
(defun ex-find-file (filespec)
  (let (s f filebuf)
    (if (string-match "[^a-zA-Z0-9_.-/]" filespec)
	(progn
	  (save-excursion 
	    (set-buffer (get-buffer-create " ex-tmp"))
	    (call-process ex-find-file-shell nil t nil
			  ex-find-file-shell-options 
			  "-c"
			  (format "echo %s | tr ' ' '\\012'" filespec))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (setq s (point))
	      (end-of-line)
	      (setq f (buffer-substring s (point)))
	      (setq filebuf (find-file-noselect f))
	      (forward-to-indentation 1))
	    (kill-buffer (current-buffer))))
      (setq filebuf (find-file-noselect (setq f filespec))))
    (switch-to-buffer filebuf)
    ))

(defun ex-global (variant)
  "Ex global command."
  (let ((gcommand ex-token))
    (if (or ex-g-flag ex-g-variant)
	(error "`%s' within `global' is not allowed" gcommand)
      (if variant
	  (setq ex-g-flag nil
		ex-g-variant t)
	(setq ex-g-flag t
	      ex-g-variant nil)))
    (vip-get-ex-pat)
    (if (null ex-token)
	(error "`%s': Missing regular expression" gcommand)))
  
  (if (string= ex-token "")
      (if (null vip-s-string)
	  (error vip-NoPrevSearch)
	(setq ex-g-pat vip-s-string))
    (setq ex-g-pat ex-token
	  vip-s-string ex-token))
  (if (null ex-addresses)
      (setq ex-addresses (list (point-max) (point-min)))
    (vip-default-ex-addresses))
  (let ((marks nil) (mark-count 0)
	com-str (end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error vip-FirstAddrExceedsSecond))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (let ((cont t) (limit (point-marker)))
	(exchange-point-and-mark)
	;; skip the last line if empty
	(beginning-of-line)
	(if (eobp) (vip-backward-char-carefully))
	(while (and cont (not (bobp)) (>= (point) limit))
	  (beginning-of-line)
	  (set-mark (point))
	  (end-of-line)
	  (let ((found (re-search-backward ex-g-pat (mark t) t)))
	    (if (or (and ex-g-flag found)
		    (and ex-g-variant (not found)))
		(progn
		  (end-of-line)
		  (setq mark-count (1+ mark-count))
		  (setq marks (cons (point-marker) marks)))))
	  (beginning-of-line)
	  (if (bobp) (setq cont nil)
	    (forward-line -1)
	    (end-of-line)))))
    (save-window-excursion
      (set-buffer vip-ex-work-buf)
      (setq com-str (buffer-substring (1+ (point)) (1- (point-max)))))
    (while marks
      (goto-char (car marks))
      (vip-ex com-str)
      (setq mark-count (1- mark-count))
      (setq marks (cdr marks)))))

(defun ex-goto ()
  "Ex goto command."
  (if (null ex-addresses)
      (setq ex-addresses (cons (point) nil)))
  (push-mark (point) t)
  (goto-char (car ex-addresses))
  (beginning-of-line))

(defun ex-line (com)
  "Ex line commands.  COM is join, shift-right or shift-left."
  (vip-default-ex-addresses)
  (vip-get-ex-count)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))) point)
    (if (> beg end) (error vip-FirstAddrExceedsSecond))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (if ex-count
	  (progn
	    (set-mark (point))
	    (forward-line ex-count)))
      (if ex-flag
	  ;; show text to be joined and ask for confirmation
	  (progn
	    (with-output-to-temp-buffer " *text*"
	      (princ (buffer-substring (point) (mark t))))
	    (condition-case nil
		(progn
		  (read-string "[Hit return to continue] ")
		  (ex-line-subr com (point) (mark t)))
	      (quit (ding)))
	    (save-excursion (kill-buffer " *text*")))
	(ex-line-subr com (point) (mark t)))
      (setq point (point)))
    (goto-char (1- point))
    (beginning-of-line)))

(defun ex-line-subr (com beg end)
  (cond ((string= com "join")
	 (goto-char (min beg end))
	 (while (and (not (eobp)) (< (point) (max beg end)))
	   (end-of-line)
	   (if (and (<= (point) (max beg end)) (not (eobp)))
	       (progn
		 (forward-line 1)
		 (delete-region (point) (1- (point)))
		 (if (not ex-variant) (fixup-whitespace))))))
	((or (string= com "right") (string= com "left"))
	 (indent-rigidly
	  (min beg end) (max beg end)
	  (if (string= com "right") vip-shift-width (- vip-shift-width)))
	 (goto-char (max beg end))
	 (end-of-line)
	 (vip-forward-char-carefully))))


(defun ex-mark ()
  "Ex mark command."
  (let (char)
    (if (null ex-addresses)
	(setq ex-addresses
	      (cons (point) nil)))
    (save-window-excursion
      (set-buffer vip-ex-work-buf)
      (skip-chars-forward " \t")
      (if (looking-at "[a-z]")
	  (progn
	    (setq char (following-char))
	    (forward-char 1)
	    (skip-chars-forward " \t")
	    (if (not (looking-at "[\n|]"))
		(error "`%s': %s" ex-token vip-SpuriousText)))
	(error "`%s' requires a following letter" ex-token)))
    (save-excursion
      (goto-char (car ex-addresses))
      (point-to-register (1+ (- char ?a))))))

    
      
;; Alternate file is the file next to the first one in the buffer ring
(defun ex-next (cycle-other-window &optional find-alt-file)
  (catch 'ex-edit
    (let (count l)
      (if (not find-alt-file) 
	  (progn
	    (vip-get-ex-file)
	    (if (or (char-or-string-p ex-offset)
		    (not (string= "" ex-file)))
		    ;(and (not (string= "" ex-file)) 
		    ;     (not (string-match "[0-9]+" ex-file))))
		(progn
		  (ex-edit t)
		  (throw 'ex-edit nil))
	      (setq count (string-to-int ex-file))
	      (if (= count 0) (setq count 1))
	      (if (< count 0) (error "Usage: `next <count>' (count >= 0)"))))
	(setq count 1))
      (setq l (vip-undisplayed-files))
      (while (> count 0)
	(while (and (not (null l)) (null (car l)))
	  (setq l (cdr l)))
	(setq count (1- count))
	(if (> count 0)
	    (setq l (cdr l))))
      (if find-alt-file (car l)
	(progn
	  (if (car l)
	      (let* ((w (if cycle-other-window
			    (get-lru-window) (selected-window)))
		     (b (window-buffer w)))
		(set-window-buffer w (get-file-buffer (car l)))
		(bury-buffer b))
	    (error "Not that many undisplayed files")))))))


(defun ex-next-related-buffer (direction &optional no-recursion)
  
  (vip-ring-rotate1 vip-related-files-and-buffers-ring direction)
  
  (let ((file-or-buffer-name 
	 (vip-current-ring-item vip-related-files-and-buffers-ring))
	(old-ring vip-related-files-and-buffers-ring)
	(old-win (selected-window))
	skip-rest buf wind)
    
    (or (and (ring-p vip-related-files-and-buffers-ring)
	     (> (ring-length vip-related-files-and-buffers-ring) 0))
	(error "This buffer has no related files or buffers"))
	
    (or (stringp file-or-buffer-name)
	(error
	 "File and buffer names must be strings, %S" file-or-buffer-name))
    
    (setq buf (cond ((get-buffer file-or-buffer-name))
		    ((file-exists-p file-or-buffer-name)
		     (find-file-noselect file-or-buffer-name))
		    ))
    
    (if (not (vip-buffer-live-p buf))
	(error "Didn't find buffer %S or file %S"
	       file-or-buffer-name
	       (abbreviate-file-name (expand-file-name file-or-buffer-name))))
	  
    (if (equal buf (current-buffer))
	(or no-recursion
	    ;; try again
	    (setq skip-rest t)
	    (ex-next-related-buffer direction 'norecursion)))
	
    (if skip-rest
	()
      ;; setup buffer
      (if (setq wind (vip-get-visible-buffer-window buf))
	  ()
	(setq wind (get-lru-window (if vip-xemacs-p nil 'visible)))
	(set-window-buffer wind buf))
	    
      (if window-system
	  (progn
	    (vip-raise-frame (vip-window-frame wind))
	    (if (equal (vip-window-frame wind) (vip-window-frame old-win))
		(save-window-excursion (select-window wind) (sit-for 1))
	      (select-window wind)))
	(save-window-excursion (select-window wind) (sit-for 1)))
	
      (save-excursion
	(set-buffer buf)
	(setq vip-related-files-and-buffers-ring old-ring))
      
      (setq vip-local-search-start-marker (point-marker))
      )))
  
    
(defun ex-preserve ()
  "Force auto save."
  (message "Autosaving all buffers that need to be saved...")
  (do-auto-save t))

(defun ex-put ()
  "Ex put."
  (let ((point (if (null ex-addresses) (point) (car ex-addresses))))
    (vip-get-ex-buffer)
    (setq vip-use-register ex-buffer)
    (goto-char point)
    (if (bobp) (vip-Put-back 1) (vip-put-back 1))))

(defun ex-pwd ()
  "Ex print working directory."
  (message default-directory))

(defun ex-quit ()
  "Ex quit command."
  (if (< vip-expert-level 3)
      (save-buffers-kill-emacs)
    (kill-buffer (current-buffer))))


(defun ex-read ()
  "Ex read command."
  (vip-get-ex-file)
  (let ((point (if (null ex-addresses) (point) (car ex-addresses))))
    (goto-char point)
    (vip-add-newline-at-eob-if-necessary)
    (if (not (or (bobp) (eobp))) (forward-line 1))
    (if (and (not ex-variant) (string= ex-file ""))
	(progn
	  (if (null buffer-file-name)
	      (error vip-NoFileSpecified))
	  (setq ex-file buffer-file-name)))
    (if ex-cmdfile
	(shell-command ex-file t)
      (insert-file-contents ex-file)))
  (ex-fixup-history vip-last-ex-prompt ex-file))
  
;; this function fixes ex-history for some commands like ex-read, ex-edit
(defun ex-fixup-history (&rest args)  
  (setq vip-ex-history
	(cons (mapconcat 'identity args " ") (cdr vip-ex-history))))
  

(defun ex-recover ()
  "Ex recover from emacs \#file\#."
  (vip-get-ex-file)
  (if (or ex-append ex-offset)
      (error "`recover': %s" vip-SpuriousText))
  (if (string= ex-file "")
      (progn
	(if (null buffer-file-name)
	    (error "This buffer isn't visiting any file"))
	(setq ex-file buffer-file-name))
    (setq ex-file (expand-file-name ex-file)))
  (if (and (not (string= ex-file (buffer-file-name)))
	   (buffer-modified-p)
	   (not ex-variant))
      (error "No write since last change \(:rec! overrides\)"))
  (recover-file ex-file))

(defun ex-rewind ()
  "Tell that `rewind' is obsolete and that one should use `:next count'"
  (message
   "Use `:n <count>' instead. Counts are obtained from the `:args' command"))


;; read variable name for ex-set
(defun ex-set-read-variable ()
  (let ((minibuffer-local-completion-map
	 (copy-keymap minibuffer-local-completion-map))
	(cursor-in-echo-area t)
	str batch)
    (define-key
      minibuffer-local-completion-map " " 'minibuffer-complete-and-exit)
    (define-key minibuffer-local-completion-map "=" 'exit-minibuffer)
    (if (vip-set-unread-command-events
	 (ex-get-inline-cmd-args "[ \t]*[a-zA-Z]*[ \t]*" nil "\C-m"))
	(progn
	  (setq batch t)
	  (vip-set-unread-command-events ?\C-m)))
    (message ":set  <Variable> [= <Value>]")
    (or batch (sit-for 2))
    
    (while (string-match "^[ \\t\\n]*$"
			 (setq str
			       (completing-read ":set " ex-variable-alist)))
      (message ":set <Variable> ")
      ;; if there are unread events, don't wait
      (or (vip-set-unread-command-events "") (sit-for 2))
      ) ; while
    str))


(defun ex-set ()
  (let ((var (ex-set-read-variable))
	(val 0)
	(set-cmd "setq")
	(ask-if-save t)
	(auto-cmd-label "; don't touch or else...")
	(delete-turn-on-auto-fill-pattern
	 "([ \t]*add-hook[ \t]+'vip-insert-state-hooks[ \t]+'turn-on-auto-fill.*)")
	actual-lisp-cmd lisp-cmd-del-pattern
	val2 orig-var)
    (setq orig-var var)
    (cond ((member var '("ai" "autoindent"))
	   (setq var "vip-auto-indent"
		 val "t"))
	  ((member var '("noai" "noautoindent"))
	   (setq var "vip-auto-indent"
		 val "nil"))
	  ((member var '("ic" "ignorecase"))
	   (setq var "vip-case-fold-search"
		 val "t"))
	  ((member var '("noic" "noignorecase"))
	   (setq var "vip-case-fold-search"
		 val "nil"))
	  ((member var '("ma" "magic"))
	   (setq var "vip-re-search"
		 val "t"))
	  ((member var '("noma" "nomagic"))
	   (setq var "vip-re-search"
		 val "nil"))
	  ((member var '("ro" "readonly"))
	   (setq var "buffer-read-only"
		 val "t"))
	  ((member var '("noro" "noreadonly"))
	   (setq var "buffer-read-only"
		 val "nil"))
	  ((member var '("sm" "showmatch"))
	   (setq var "blink-matching-paren"
		 val "t"))
	  ((member var '("nosm" "noshowmatch"))
	   (setq var "blink-matching-paren"
		 val "nil"))
	  ((member var '("ws" "wrapscan"))
	   (setq var "vip-search-wrap-around-t"
		 val "t"))
	  ((member var '("nows" "nowrapscan"))
	   (setq var "vip-search-wrap-around-t"
		 val "nil")))
    (if (eq val 0) ;; value must be set by the user
	(let ((cursor-in-echo-area t))
	  (message (format ":set %s = <Value>" var))
	  ;; if there are unread events, don't wait
	  (or (vip-set-unread-command-events "") (sit-for 2))
	  (setq val (read-string (format ":set %s = " var)))
	  (ex-fixup-history "set" orig-var val)
	  
	  ;; check numerical values
	  (if (member var
		      '("sw" "shiftwidth" "ts" "tabstop" "wm" "wrapmargin"))
	      (condition-case nil
		  (or (numberp (setq val2 (car (read-from-string val))))
		      (error "%s: Invalid value, numberp, %S" var val))
		(error
		 (error "%s: Invalid value, numberp, %S" var val))))
		  
	  (cond
	   ((member var '("sw" "shiftwidth"))
	    (setq var "vip-shift-width"))
	   ((member var '("ts" "tabstop"))
	    ;; make it take effect in curr buff and new bufs
	    (kill-local-variable 'tab-width)
	    (setq var "tab-width"
		  set-cmd "setq-default"))
	   ((member var '("tsl" "tab-stop-local"))
	    (setq var "tab-width"
		  set-cmd "setq"
		  ask-if-save nil))
	   ((member var '("wm" "wrapmargin"))
	    ;; make it take effect in curr buff and new bufs
	    (kill-local-variable 'fill-column) 
	    (setq var "fill-column" 
		  val (format "(- (window-width) %s)" val)
		  set-cmd "setq-default"))
	   ((member var '("sh" "shell"))
	    (setq var "explicit-shell-file-name"
		  val (format "\"%s\"" val)))))
      (ex-fixup-history "set" orig-var))
    
    (setq actual-lisp-cmd (format "\n(%s %s %s) %s"
				  set-cmd var val auto-cmd-label))
    (setq lisp-cmd-del-pattern
	  (format "^\n?[ \t]*([ \t]*%s[ \t]+%s[ \t].*)[ \t]*%s"
		  set-cmd var auto-cmd-label))
    
    (if (and ask-if-save
	     (y-or-n-p (format "Do you want to save this setting in %s "
			       vip-custom-file-name)))
	(progn
	  (vip-save-string-in-file 
	   actual-lisp-cmd vip-custom-file-name
	   ;; del pattern
	   lisp-cmd-del-pattern)
	  (if (string= var "fill-column")
	      (if (> val2 0)
		  (vip-save-string-in-file
		   (concat
		    "(add-hook 'vip-insert-state-hooks 'turn-on-auto-fill) "
		    auto-cmd-label)
		   vip-custom-file-name
		   delete-turn-on-auto-fill-pattern)
		(vip-save-string-in-file
		 nil vip-custom-file-name delete-turn-on-auto-fill-pattern)
		(vip-save-string-in-file
		 nil vip-custom-file-name
		 ;; del pattern
		 lisp-cmd-del-pattern)
		))
	  ))
    
    (message (format "%s %s %s" set-cmd var (if (string-match "^[ \t]*$" val)
						(format "%S" val)
					      val)))
    (eval (car (read-from-string actual-lisp-cmd)))
	(if (string= var "fill-column")
		(if (> val2 0)
			(auto-fill-mode 1)
		  (auto-fill-mode -1)))
		
    ))

;; In inline args, skip regex-forw and (optionally) chars-back.
;; Optional 3d arg is a string that should replace ' ' to prevent its
;; special meaning
(defun ex-get-inline-cmd-args (regex-forw &optional chars-back replace-str)
  (save-excursion
    (set-buffer vip-ex-work-buf)
    (goto-char (point-min))
    (re-search-forward regex-forw nil t)
    (let ((beg (point))
	  end)
      (goto-char (point-max))
      (if chars-back
	  (skip-chars-backward chars-back)
	(skip-chars-backward " \t\n\C-m"))
      (setq end (point))
      ;; replace SPC with `=' to suppress the special meaning SPC has
      ;; in Ex commands
      (goto-char beg)
      (if replace-str
	  (while (re-search-forward " +" nil t)
	    (replace-match replace-str nil t)
	    (vip-forward-char-carefully)))
      (goto-char end)
      (buffer-substring beg end))))


(defun ex-shell ()
  "Ex shell command."
  (shell))
  
(defun ex-help ()
  "Viper help. Invokes Info."
  (condition-case nil
      (progn
	(pop-to-buffer (get-buffer-create "*info*"))
	(info vip-info-file-name)
	(message "Type `i' to search for a specific topic"))
    (error (beep 1)
	   (with-output-to-temp-buffer " *vip-info*"
	     (princ "The Info file for Viper does not seem to be installed.

This file is part of the distribution of Viper. If you do not
have the full distribution, please obtain it from the `anonymous'
FTP account at `archive.cis.ohio-state.edu':

	    /pub/gnu/emacs/elisp-archive/modes/viper.shar
	    
The Info files for Viper should be installed as <name>, <name>-1, etc.,
where <name> is the value of `vip-info-file-name'.")))))

(defun ex-source ()
  "Ex source command. Loads the file specified as argument or `~/.vip'."
  (vip-get-ex-file)
  (if (string= ex-file "")
      (load vip-custom-file-name)
    (load ex-file)))

(defun ex-substitute (&optional repeat r-flag) 
  "Ex substitute command. 
If REPEAT use previous regexp which is ex-reg-exp or vip-s-string"
  (let ((opt-g nil)
	(opt-c nil)
	(matched-pos nil)
	(case-fold-search vip-case-fold-search)
	delim pat repl)
    (if repeat (setq ex-token nil) (setq delim (vip-get-ex-pat)))
    (if (null ex-token)
	(setq pat (if r-flag vip-s-string ex-reg-exp)
	      repl ex-repl
	      delim (string-to-char pat))
      (setq pat (if (string= ex-token "") vip-s-string ex-token))
      (setq vip-s-string pat
	    ex-reg-exp pat)
      (setq delim (vip-get-ex-pat))
      (if (null ex-token)
	  (setq ex-token ""
		ex-repl "")
	(setq repl ex-token
	      ex-repl ex-token)))
    (while (vip-get-ex-opt-gc delim)
      (if (string= ex-token "g") (setq opt-g t) (setq opt-c t)))
    (vip-get-ex-count)
    (if ex-count
	(save-excursion
	  (if ex-addresses (goto-char (car ex-addresses)))
	  (set-mark (point))
	  (forward-line (1- ex-count))
	  (setq ex-addresses (cons (point) (cons (mark t) nil))))
      (if (null ex-addresses)
	  (setq ex-addresses (cons (point) (cons (point) nil)))
	(if (null (cdr ex-addresses))
	    (setq ex-addresses (cons (car ex-addresses) ex-addresses)))))
					;(setq G opt-g)
    (let ((beg (car ex-addresses))
	  (end (car (cdr ex-addresses)))
	  eol-mark)
      (save-excursion
	(vip-enlarge-region beg end)
	(let ((limit (save-excursion
		       (goto-char (max (point) (mark t)))
		       (point-marker))))
	  (goto-char (min (point) (mark t)))
	  (while (< (point) limit)
	    (end-of-line)
	    (setq eol-mark (point-marker))
	    (beginning-of-line)
	    (if opt-g
		(progn
		  (while (and (not (eolp))
			      (re-search-forward pat eol-mark t))
		    (if (or (not opt-c) (y-or-n-p "Replace? "))
			(progn
			  (setq matched-pos (point))
			  (if (not (stringp repl))
			      (error "Can't perform Ex substitution: No previous replacement pattern"))
			  (replace-match repl t t))))
		  (end-of-line)
		  (vip-forward-char-carefully))
	      (if (null pat)
		  (error
		   "Can't repeat Ex substitution: No previous regular expression"))
	      (if (and (re-search-forward pat eol-mark t)
		       (or (not opt-c) (y-or-n-p "Replace? ")))
		  (progn
		    (setq matched-pos (point))
		    (if (not (stringp repl))
			(error "Can't perform Ex substitution: No previous replacement pattern"))
		    (replace-match repl t t)))
	      (end-of-line)
	      (vip-forward-char-carefully))))))
    (if matched-pos (goto-char matched-pos))
    (beginning-of-line)
    (if opt-c (message "done"))))

(defun ex-tag ()
  "Ex tag command."
  (let (tag)
    (save-window-excursion
      (set-buffer vip-ex-work-buf)
      (skip-chars-forward " \t")
      (set-mark (point))
      (skip-chars-forward "^ |\t\n")
      (setq tag (buffer-substring (mark t) (point))))
    (if (not (string= tag "")) (setq ex-tag tag))
    (vip-change-state-to-emacs)
    (condition-case conds
	(progn
	  (if (string= tag "")
	      (find-tag ex-tag t)
	    (find-tag-other-window ex-tag))
	  (vip-change-state-to-vi))
      (error
       (vip-change-state-to-vi)
       (vip-message-conditions conds)))))

(defun ex-write (q-flag)
  "Ex write command."
  (vip-default-ex-addresses t)
  (vip-get-ex-file)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))) 
	temp-buf writing-same-file region
	file-exists writing-whole-file)
    (if (> beg end) (error vip-FirstAddrExceedsSecond))
    (if ex-cmdfile
	(progn
	  (vip-enlarge-region beg end)
	  (shell-command-on-region (point) (mark t) ex-file))
      (if (and (string= ex-file "") (not (buffer-file-name)))
	  (setq ex-file
		(read-file-name
		 (format "Buffer %s isn't visiting any file. File to save in: "
			 (buffer-name)))))
      
      (setq writing-whole-file (and (= (point-min) beg) (= (point-max) end))
	    ex-file (if (string= ex-file "")
			(buffer-file-name)
		      (expand-file-name ex-file))
	    file-exists (file-exists-p ex-file)
	    writing-same-file (string= ex-file (buffer-file-name)))
      (if (and writing-whole-file writing-same-file)
	  (if (not (buffer-modified-p))
	      (message "(No changes need to be saved)")
	    (save-buffer)
	    (ex-write-info file-exists ex-file beg end))
	;; writing some other file or portion of the currents
	;; file---create temp buffer for it
	;; disable undo in that buffer, for efficiency
	(buffer-disable-undo (setq temp-buf (create-file-buffer ex-file)))
	(unwind-protect 
	    (save-excursion
	      (if (and file-exists
		       (not writing-same-file)
		       (not (yes-or-no-p
			     (format "File %s exists. Overwrite? " ex-file))))
		  (error "Quit")
		(vip-enlarge-region beg end)
		(setq region (buffer-substring (point) (mark t)))
		(set-buffer temp-buf)
		(set-visited-file-name ex-file)
		(erase-buffer)
		(if (and file-exists ex-append)
		    (insert-file-contents ex-file))
		(goto-char (point-max))
		(insert region)
		(save-buffer)
		(ex-write-info file-exists ex-file (point-min) (point-max))
		)
	      (set-buffer temp-buf)
	      (set-buffer-modified-p nil)
	      (kill-buffer temp-buf)
	      )
	  ))
      ;; this prevents the loss of data if writing part of the buffer
      (if (and (buffer-file-name) writing-same-file)
	  (set-visited-file-modtime))
      (or writing-whole-file 
	  (not writing-same-file)
	  (set-buffer-modified-p t))
      (if q-flag
	  (if (< vip-expert-level 2)
	      (save-buffers-kill-emacs)
	    (kill-buffer (current-buffer))))
      )))
	  

(defun ex-write-info (exists file-name beg end)
  (message "`%s'%s %d lines, %d characters"
	   (abbreviate-file-name file-name)
	   (if exists "" " [New file]")
	   (count-lines beg (min (1+ end) (point-max)))
	   (- end beg)))

(defun ex-yank ()
  "Ex yank command."
  (vip-default-ex-addresses)
  (vip-get-ex-buffer)
  (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
    (if (> beg end) (error vip-FirstAddrExceedsSecond))
    (save-excursion
      (vip-enlarge-region beg end)
      (exchange-point-and-mark)
      (if (or ex-g-flag ex-g-variant)
	  (error "Can't execute `yank' within `global'"))
      (if ex-count
	  (progn
	    (set-mark (point))
	    (forward-line (1- ex-count)))
	(set-mark end))
      (vip-enlarge-region (point) (mark t))
      (if ex-flag (error "`yank': %s" vip-SpuriousText))
      (if ex-buffer
	  (cond ((vip-valid-register ex-buffer '(Letter))
		 (vip-append-to-register
		  (downcase ex-buffer) (point) (mark t)))
		((vip-valid-register ex-buffer)
		 (copy-to-register ex-buffer (point) (mark t) nil))
		(t (error vip-InvalidRegister ex-buffer))))
      (copy-region-as-kill (point) (mark t)))))

(defun ex-command ()
  "Execute shell command."
  (let (command)
    (save-window-excursion
      (set-buffer vip-ex-work-buf)
      (skip-chars-forward " \t")
      (setq command (buffer-substring (point) (point-max)))
      (end-of-line))
    (setq command (ex-expand-filsyms command (current-buffer)))
    (if (and (> (length command) 0) (string= "!" (substring command 0 1)))
	(if vip-ex-last-shell-com
	    (setq command (concat vip-ex-last-shell-com (substring command 1)))
	  (error "No previous shell command")))
    (setq vip-ex-last-shell-com command)
    (if (null ex-addresses)
	(shell-command command)
      (let ((end (car ex-addresses)) (beg (car (cdr ex-addresses))))
	(if (null beg) (setq beg end))
	(save-excursion
	  (goto-char beg)
	  (set-mark end)
	  (vip-enlarge-region (point) (mark t))
	  (shell-command-on-region (point) (mark t) command t))
	(goto-char beg)))))

(defun ex-line-no ()
  "Print line number."
  (message "%d"
	   (1+ (count-lines
		(point-min)
		(if (null ex-addresses) (point-max) (car ex-addresses))))))

(defun vip-info-on-file ()
  "Give information on the file visited by the current buffer."
  (interactive)
  (message "%s: pos=%d(%d) line=%d(%d) col=%d %s"
           (if (buffer-file-name)
	       (abbreviate-file-name (buffer-file-name))
	     "[No visited file]")
	   (point) (1- (point-max))
           (count-lines (point-min) (vip-line-pos 'end))
           (count-lines (point-min) (point-max))
	   (1+ (current-column))
	   (if (buffer-modified-p) "[Modified]" "[Unchanged]")
  ))


(provide 'viper-ex)

;;;  viper-ex.el ends here
