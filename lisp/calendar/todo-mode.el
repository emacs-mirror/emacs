;; todomode.el -- major mode for editing Todo-List files

;; ---------------------------------------------------------------------------

;; Note: You may copy this file freely for non-commercial use; otherwise,
;;       please contact   (address) O Seidel, Lessingstr 8, Eschborn, FRG
;;                        (e-mail ) Oliver.Seidel@cl.cam.ac.uk (2 Aug 1997)

;; $Id: todomode.el,v 1.3 1997/08/03 12:47:26 os10000 Exp os10000 $
;;
;; $Log: todomode.el,v $
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
;; Then you have the following facilities available:
;;
;; M-x todo-mode              will enter the todo list screen, here type
;; p                          for the previous entry
;; n                          for the next entry
;; q                          to save the list and exit the buffer
;; e                          to edit the current entry
;; i                          to insert a new entry
;; k                          to kill the current entry
;; r                          raise current entryk's priority
;; l                          lower the current entry's priority
;; f                          to file the current entry, including a
;;                            comment and timestamp
;;
;; I would recommend to add the following bindings to your global keymap:
;;
;; (global-set-key "\C-ct" 'todo-mode)
;; (global-set-key "\C-ci" 'todo-cmd-inst)
;;
;; This will enable you to quickly find the todo-list, or to simply add an
;; entry, without changing to it and getting sidetracked from your current
;; project.
;;
;; I would also recommend that you execute the command
;;
;; (setq todo-prefix "*/* ")
;;
;; to have all your entries prefixed in such a way that the diary displays
;; them every day.

;; ---------------------------------------------------------------------------

;; User-configurable variables:

(defvar todo-prefix "" "TODO mode prefix when creating entries")
(defvar todo-file-do "~/.todo-do" "TODO mode filename of list file")
(defvar todo-file-done "~/.todo-done" "TODO mode filename of archive file")
(defvar todo-mode-hook nil "Hooks invoked when the *TODO* buffer is created.")

;; ---------------------------------------------------------------------------

(require 'time-stamp)

(defvar todo-begin (point-min) "TODO mode beginning of line")
(defvar todo-end   (point-min) "TODO mode end of line")

(setq todo-mode-map (make-keymap))
(suppress-keymap todo-mode-map)
(define-key todo-mode-map "p" 'todo-cmd-prev)
(define-key todo-mode-map "n" 'todo-cmd-next)
(define-key todo-mode-map "q" 'todo-cmd-done)
(define-key todo-mode-map "e" 'todo-cmd-edit)
(define-key todo-mode-map "i" 'todo-cmd-inst)
(define-key todo-mode-map "k" 'todo-cmd-kill)
(define-key todo-mode-map "r" 'todo-cmd-rais)
(define-key todo-mode-map "l" 'todo-cmd-lowr)
(define-key todo-mode-map "f" 'todo-cmd-file)

(defun todo-cmd-prev () "Select previous entry."
  (interactive)
  (forward-line -1)
  (beginning-of-line nil)
  (message "")
  )

(defun todo-cmd-next () "Select next entry."
  (interactive)
  (forward-line 1)
  (beginning-of-line nil)
  (message "")
  )

(defun todo-cmd-done () "Done with todo list for now."
  (interactive)
  (beginning-of-line nil)
  (message "")
  (save-buffer)
  (bury-buffer)
  )

(defun todo-line () "Find current line in buffer."
  (end-of-line nil)       (setq todo-end (point))
  (beginning-of-line nil) (setq todo-begin (point))
  (buffer-substring todo-begin todo-end)
  )

(defun todo-cmd-edit () "Edit current todo list entry."
  (interactive)
  (setq todo-entry (todo-line))
  (delete-region todo-begin todo-end)
  (insert (read-from-minibuffer "Edit: " todo-entry))
  (beginning-of-line nil)
  (message "")
  )

(defvar todo-prv-lne 0 "previous line that I asked about.")
(defvar todo-prv-ans 0 "previous answer that I got.")

(defun todo-ask (lne) "Ask whether entry is more important than at LNE."
  (if (not (equal todo-prv-lne lne) )
      (progn
	(setq todo-prv-lne lne)
	(goto-line todo-prv-lne)
	(setq todo-prv-ans (y-or-n-p (concat "More important than '" (todo-line) "'? ")))
	)
    )
  todo-prv-ans
  )

(defun todo-cmd-inst () "Insert new todo list entry."
  (interactive)
  (beginning-of-line nil)
  (setq todo-entry (concat "*/* " (read-from-minibuffer "New TODO entry: ")))
  (save-window-excursion
    (find-file todo-file-do)
    (setq todo-prv-lne 0)
    (let* ((todo-fst 1)
	   (todo-lst (+ 1 (count-lines (point-min) (point-max)))))
      (while (< todo-fst todo-lst)
	(setq todo-cur (/ (+ todo-fst todo-lst) 2))
	(setq todo-ans (if (< todo-cur todo-lst) (todo-ask todo-cur) nil))
	(if todo-ans
	    (setq todo-lst todo-cur)
	  (setq todo-fst (+ todo-cur 1)))
	)
      (goto-line todo-fst)
      )
    (insert (concat todo-entry "\n"))
    (forward-char -1)
    )
  (beginning-of-line nil)
  (message "")
  )

(defun todo-cmd-kill () "Delete current todo list entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (progn
	(setq todo-entry (todo-line))
	(setq todo-answer (y-or-n-p (concat "Permanently remove '" todo-entry "'? ")))
	(if todo-answer (progn (delete-region todo-begin (+ 1 todo-end)) (forward-char -1)))
	)
    (message "No entry to delete.")
    )
  (beginning-of-line nil)
  (message "")
  )

(defun todo-cmd-rais () "Raise priority of current entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (progn
	(setq todo-entry (todo-line))
	(delete-region todo-begin (+ 1 todo-end))
	(forward-line -1)
	(insert (concat todo-entry "\n"))
	(forward-char -1)
	)
    (message "No entry to raise.")
    )
  (beginning-of-line nil)
  (message "")
  )

(defun todo-cmd-lowr () "Lower priority of current entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (progn
	(setq todo-entry (todo-line))
	(delete-region todo-begin (+ 1 todo-end))
	(forward-line 1)
	(insert (concat todo-entry "\n"))
	(forward-char -1)
	)
    (message "No entry to raise.")
    )
  (beginning-of-line nil)
  (message "")
  )

(defun todo-cmd-file () "File away the current todo list entry."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (progn
	(setq time-stamp-format " %2d, %y, %02I:%02M%p %b")
	(setq tmp (time-stamp-string))
	(beginning-of-line nil)
	(insert (concat (substring tmp 19 22) (substring tmp 0 19)))
	(end-of-line nil)
	(insert (concat " (" (read-from-minibuffer "Comment: ") ")"))
	(todo-line)
	(append-to-file todo-begin (+ 1 todo-end) todo-file-done)
	(delete-region todo-begin (+ 1 todo-end))
	(forward-char -1)
	)
    (message "No entry to delete.")
    )
  (beginning-of-line nil)
  (message "")
  )

;; ---------------------------------------------------------------------------

(defun todo-mode ()
  "Major mode for editing TODO lists.\n\n\\{todo-mode-map}"
  (interactive)
  (find-file todo-file-do)
  (setq major-mode 'todo-mode)
  (setq mode-name "TODO")
  (use-local-map todo-mode-map)
  (beginning-of-line nil)
  (run-hooks 'todo-mode-hook) )

(provide 'todomode)

;; ---------------------------------------------------------------------------

;;; todomode.el ends here

;; ---------------------------------------------------------------------------
