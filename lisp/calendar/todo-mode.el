; todomode.el -- major mode for editing TODO list files

;; ---------------------------------------------------------------------------

;; Note: You may copy this file freely for non-commercial use; otherwise,
;;       please contact   (address) O Seidel, Lessingstr 8, Eschborn, FRG
;;                        (e-mail ) Oliver.Seidel@cl.cam.ac.uk (2 Aug 1997)

;; $Id: todomode.el,v 1.5 1997/08/05 14:43:39 os10000 Exp os10000 $
;;
;; $Log: todomode.el,v $
;; Revision 1.5  1997/08/05  14:43:39  os10000
;; Added improvements from Ron Gut <rgut@aware.com>.
;; Added category management.
;;
;; Revision 1.4  1997/08/04  16:18:45  os10000
;; Added Raise/Lower item.
;;
;; Revision 1.3  1997/08/03  12:47:26  os10000
;; Cleaned up variables, prefix and cursor position.
;;
;; Revision 1.2  1997/08/03 12:15:28  os10000
;; It appears to work.
;;
;; Revision 1.1  1997/08/03 12:15:13  os10000
;; Initial revision
;;

;; ---------------------------------------------------------------------------

;; Description:
;;
;; To get this to work, make emacs execute the line "(require 'todomode)"
;; and maybe initialise the variables below on startup.
;;
;; Just for the case that you are wondering about the ugly name of this
;; package: I am one of those unfortunate people who have DOS, LINUX and
;; OS/2 on one of their computers, so part of my home-filespace is shared
;; and stored on a DOS partition, which is accessible to all systems.  If
;; you wish, you can of course rename the name of the file (and the last
;; command) to something more aisthetically (please don't argue about
;; this spelling ...) pleasing, like i.e. todo-mode.
;;
;; You will have the following facilities available:
;;
;; M-x todo-mode              will enter the todo list screen, here type
;; +                          to go to next category
;; -                          to go to previous category
;; e                          to edit the current entry
;; f                          to file the current entry, including a
;;                                                 comment and timestamp
;; i                          to insert a new entry
;; k                          to kill the current entry
;; l                          lower the current entry's priority
;; n                          for the next entry
;; p                          for the previous entry
;; q                          to save the list and exit the buffer
;; r                          raise current entryk's priority
;; s                          to save the list
;;
;; When you add a new entry, you are asked for the text and then for the
;; category.  I for example have categories for things that I want to do
;; in the office (like mail my mum), that I want to do in town (like buy
;; cornflakes) and things I want to do at home (move my suitcases).  The
;; categories can be selected with the cursor keys and if you type in the
;; name of a category which didn't exist before, an empty category of the
;; desired name will be added.
;;
;; I would recommend to add the following bindings to your global keymap:
;;
;; (global-set-key "\C-ct" 'todo-show)
;; (global-set-key "\C-ci" 'todo-cmd-inst)
;;
;; This will enable you to quickly find the todo-list, or to simply add an
;; entry, without changing to it and getting sidetracked from your current
;; project.
;;
;; I would also recommend that use the prefix "*/*" (by leaving the
;; variable 'todo-prefix' untouched) so that the diary displays
;; each entry every day.
;;
;; For this, please read the documentation that goes with the calendar
;; since that will tell you how you can set up the fancy diary display
;; and use the #include command to include your todo list file as part
;; of your diary.
;;
;; Enjoy this package and express your gratitude by sending valuables
;; to my parents' address as listed above!!!
;;
;; Oliver Seidel

;; ---------------------------------------------------------------------------

;; User-configurable variables:

(defvar todo-prefix "*/*" "TODO mode prefix when creating entries")
(defvar todo-file-do "~/.todo-do" "TODO mode filename of list file")
(defvar todo-file-done "~/.todo-done" "TODO mode filename of archive file")
(defvar todo-mode-hook nil "Hooks invoked when the *TODO* buffer is created.")

;; ---------------------------------------------------------------------------

(require 'time-stamp)

(defvar todo-mode-map (make-sparse-keymap) "TODO mode keymap.  See `todo-mode'")
(define-key todo-mode-map "+" 'todo-cmd-forw)
(define-key todo-mode-map "-" 'todo-cmd-back)
(define-key todo-mode-map "e" 'todo-cmd-edit)
(define-key todo-mode-map "f" 'todo-cmd-file)
(define-key todo-mode-map "i" 'todo-cmd-inst)
(define-key todo-mode-map "k" 'todo-cmd-kill)
(define-key todo-mode-map "l" 'todo-cmd-lowr)
(define-key todo-mode-map "n" 'todo-cmd-next)
(define-key todo-mode-map "p" 'todo-cmd-prev)
(define-key todo-mode-map "q" 'todo-cmd-done)
(define-key todo-mode-map "r" 'todo-cmd-rais)
(define-key todo-mode-map "s" 'todo-cmd-save)

(defun todo-cat-slct ()
  (let ((todo-category-name (nth todo-category-number todo-cats)))
    (setq mode-line-buffer-identification (concat "Category: " todo-category-name))
    (widen)
    (goto-char (point-min))
    (search-forward (concat "--- " todo-category-name))
    (setq begin (+ (point-at-eol) 1))
    (search-forward "--- End")
    (narrow-to-region begin (point-at-bol))
    (goto-char (point-min))
    )
)

(defun todo-cmd-forw () "Go forward to TODO list of next category."
  (interactive)
  (let ((todo-cat-cnt (- (length todo-cats) 1)))
    (setq todo-category-number (if (< todo-category-number todo-cat-cnt)
				   (+ todo-category-number 1) 0))
    (todo-cat-slct)
    )
  )

(defun todo-cmd-back () "Go back to TODO list of previous category."
  (interactive)
  (let ((todo-cat-cnt (- (length todo-cats) 1)))
    (setq todo-category-number (if (> todo-category-number 0)
				   (- todo-category-number 1) todo-cat-cnt))
    (todo-cat-slct)
    )
  )

(defun todo-cmd-prev () "Select previous entry of TODO list."
  (interactive)
  (forward-line -1)
  (beginning-of-line nil)
  (message "")
  )

(defun todo-cmd-next () "Select next entry of TODO list."
  (interactive)
  (forward-line 1)
  (beginning-of-line nil)
  (message "")
  )

(defun todo-cmd-save () "Save the TODO list."
  (interactive)
  (save-buffer)
  )

(defun todo-cmd-done () "Done with TODO list for now."
  (interactive)
  (widen)
  (save-buffer)
  (beginning-of-line nil)
  (message "")
  (bury-buffer)
  )

(defun todo-line () "Find current line in buffer."  (buffer-substring (point-at-bol) (point-at-eol)))

(defun todo-cmd-edit () "Edit current TODO list entry."
  (interactive)
  (let ((todo-entry (todo-line)))
    (delete-region (point-at-bol) (point-at-eol))
    (insert (read-from-minibuffer "Edit: " todo-entry))
    (beginning-of-line nil)
    (message "")
    )
  )

(defvar todo-prv-lne 0 "previous line that I asked about.")
(defvar todo-prv-ans 0 "previous answer that I got.")

(defun todo-ask (lne) "Ask whether entry is more important than at LNE."
  (if (not (equal todo-prv-lne lne))
      (progn
	(setq todo-prv-lne lne)
	(goto-line todo-prv-lne)
	(setq todo-prv-ans (y-or-n-p (concat "More important than '" (todo-line) "'? ")))
	)
    )
  todo-prv-ans
  )

(defun todo-add-category (cat) "Add a new category to the TODO list."
  (interactive)
  (save-window-excursion
    (setq todo-cats (cons cat todo-cats))
    (find-file todo-file-do)
    (widen)
    (goto-char (point-min))
    (let ((posn (search-forward "-*- mode: todo; " 17 t)))
      (if (not (null posn)) (goto-char posn))
      (if (equal posn nil) (progn (insert "-*- mode: todo; \n") (forward-char -1)) (kill-line))
      )
    (insert (format "todo-cats: %S; -*-" todo-cats))
    (forward-char 1)
    (insert (format "%s --- %s\n--- End\n%s %s\n" todo-prefix cat todo-prefix (make-string 75 ?-)))
    )
  0
  )

(defun todo-cmd-inst () "Insert new TODO list entry."
  (interactive)
  (beginning-of-line nil)
  (let* ((todo-entry (concat todo-prefix " " (read-from-minibuffer "New TODO entry: ")))
	 (temp-catgs todo-cats)
	 (todo-hstry (cons 'temp-catgs (+ todo-category-number 1))))
    (save-window-excursion
      (setq todo-category
	    (read-from-minibuffer "Category: " (nth todo-category-number todo-cats) nil nil todo-hstry))
      (let* ((ltrgt todo-category)
	     (lnmbr 0)
	     (ltext (car todo-cats))
	     (lrest (cdr todo-cats)))
	(setq ltext (car todo-cats))
	(while (not (or (null lrest) (string-equal ltext ltrgt)))
	  (setq ltext (car lrest))
	  (setq lrest (cdr lrest))
	  (setq lnmbr (+ 1 lnmbr))
	  )
	(setq todo-category-number
	      (if (string-equal ltext todo-category) lnmbr (todo-add-category todo-category)))
	)
      (todo-show)
      (setq todo-prv-lne 0)
      (let* ((todo-fst 1)
	     (todo-lst (+ 1 (count-lines (point-min) (point-max)))))
	(while (< todo-fst todo-lst)
	  (let* ((todo-cur (/ (+ todo-fst todo-lst) 2))
		 (todo-ans (if (< todo-cur todo-lst) (todo-ask todo-cur) nil)))
	    (if todo-ans
		(setq todo-lst todo-cur)
	      (setq todo-fst (+ todo-cur 1)))
	    )
	  )
	(goto-line todo-fst)
	)
      (insert (concat todo-entry "\n"))
      (forward-line -1)
      )
    (beginning-of-line nil)
    (message "")
    )
  )

(defun todo-cmd-kill () "Delete current TODO list entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (progn
	(let* ((todo-entry (todo-line))
	       (todo-answer (y-or-n-p (concat "Permanently remove '" todo-entry "'? "))))
	  (if todo-answer
	      (progn
		(delete-region (point-at-bol) (+ 1 (point-at-eol))) 
		(forward-line -1)
		)
	    )
	  )
	(message "")
	)
    (message "No TODO list entry to delete.")
    )
  (beginning-of-line nil)
  )

(defun todo-cmd-rais () "Raise priority of current entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (progn
	(setq todo-entry (todo-line))
	(delete-region (point-at-bol) (+ 1 (point-at-eol))) 
	(forward-line -1)
	(insert (concat todo-entry "\n"))
	(forward-line -1)
	(message "")
	)
    (message "No TODO list entry to raise.")
    )
  (beginning-of-line nil)
  )

(defun todo-cmd-lowr () "Lower priority of current entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (progn
	(setq todo-entry (todo-line))
	(delete-region (point-at-bol) (+ 1 (point-at-eol))) 
	(forward-line 1)
	(insert (concat todo-entry "\n"))
	(forward-line -1)
	(message "")
	)
    (message "No TODO list entry to raise.")
    )
  (beginning-of-line nil)
  )

(defun todo-cmd-file () "File away the current TODO list entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (progn
	(let ((time-stamp-format "%3b %2d, %y, %02I:%02M%p"))
	  (beginning-of-line nil)
	  (delete-region (point-at-bol) (search-forward todo-prefix))
	  (insert (time-stamp-string))
	  (end-of-line nil)
	  (insert (concat " (" (read-from-minibuffer "Comment: ") ")"))
	  (append-to-file (point-at-bol) (+ 1 (point-at-eol)) todo-file-done)
	  (delete-region (point-at-bol) (+ 1 (point-at-eol)))
	  (forward-line -1)
	  )
	(message "")
	)
    (message "No TODO list entry to delete.")
    )
  (beginning-of-line nil)
  )

;; ---------------------------------------------------------------------------

;; utility functions:  These are available in XEmacs, but not in Emacs 19.34

(if (not (fboundp 'point-at-bol))
    (defun point-at-bol ()
      (save-excursion
	(beginning-of-line)
	(point))))

(if (not (fboundp 'point-at-eol))
    (defun point-at-eol ()
      (save-excursion
	(end-of-line)
	(point))))

;; ---------------------------------------------------------------------------

(defvar todo-mode-popup-menu
  (purecopy '("Todo Mode Menu"
              ["Forward item"         todo-cmd-forw t]
              ["Backward item"        todo-cmd-back t]
              "---"
              ["Edit item"            todo-cmd-edit t]
              ["File item"            todo-cmd-file t]
              ["Insert new item"      todo-cmd-inst t]
              ["Kill item"            todo-cmd-kill t]
              "---"
              ["Lower item priority"  todo-cmd-lowr t]
              ["Raise item priority"  todo-cmd-rais t]
              "---"
              ["Next item"            todo-cmd-next t]
              ["Previous item"        todo-cmd-prev t]
              "---"
              ["Save"                 todo-cmd-save t]
              "---"
              ["Quit"                 todo-cmd-done t]
              )
	    )
  )

(defvar todo-cats nil "TODO categories.")
(defvar todo-category-number 0 "TODO category number.")

(defun todo-mode () "Major mode for editing TODO lists.\n\n\\{todo-mode-map}"
  (interactive)
  (setq major-mode 'todo-mode)
  (setq mode-name "TODO")
  (use-local-map todo-mode-map)
  (setq mode-popup-menu todo-mode-popup-menu)
  (run-hooks 'todo-mode-hook)
  )

(defun todo-show () "Show TODO list."
  (interactive)
  (find-file todo-file-do)
  (if (null todo-cats)
      (progn
	(todo-add-category "Todo")
	(goto-char (point-min))
	(goto-char (search-forward "--- End"))
	(let ((bol (point-at-bol)))
	  (forward-line 1)
	  (let* ((eol (+ (point-at-eol) 1))
		 (mrkr (buffer-substring bol eol)))
	    (delete-region bol eol)
	    (goto-char (point-max))
	    (insert mrkr)
	    )
	  )
	(save-buffer)
	(kill-buffer (current-buffer))
	(find-file todo-file-do)
	)
    )
  (beginning-of-line nil)
  (todo-cat-slct)
  )

(provide 'todomode)

;; ---------------------------------------------------------------------------

;; todomode.el ends here

;; ---------------------------------------------------------------------------
