;;; tq.el --- utility to maintain a transaction queue

;; Copyright (C) 1985, 1986, 1987, 1992 Free Software Foundation, Inc.

;; Author: Scott Draves <spot@cs.cmu.edu>
;; Adapted-By: ESR
;; Keywords: extensions

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
 
;;; manages receiving a stream asynchronously, 
;;; parsing it into transactions, and then calling
;;; handler functions

;;; Our basic structure is the queue/process/buffer triple.  Each entry
;;; of the queue is a regexp/closure/function triple.  We buffer
;;; bytes from the process until we see the regexp at the head of the
;;; queue.  Then we call the function with the closure and the
;;; collected bytes.

;;; Code:

;;;###autoload
(defun tq-create (process)
  "Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine."
  (let ((tq (cons nil (cons process
			    (generate-new-buffer
			     (concat " tq-temp-"
				     (process-name process)))))))
    (set-process-filter process
			(`(lambda (proc string)
			   (tq-filter  '(, tq) string))))
    tq))

;;; accessors
(defun tq-queue   (tq) (car tq))
(defun tq-process (tq) (car (cdr tq)))
(defun tq-buffer  (tq) (cdr (cdr tq)))

(defun tq-queue-add (tq re closure fn)
  (setcar tq (nconc (tq-queue tq)
		    (cons (cons re (cons closure fn)) nil)))
  'ok)

(defun tq-queue-head-regexp  (tq) (car (car (tq-queue tq))))
(defun tq-queue-head-fn      (tq) (cdr (cdr (car (tq-queue tq)))))
(defun tq-queue-head-closure (tq) (car (cdr (car (tq-queue tq)))))
(defun tq-queue-empty        (tq) (not (tq-queue tq)))
(defun tq-queue-pop          (tq) (setcar tq (cdr (car tq))) (null (car tq)))
 

;;; must add to queue before sending!
(defun tq-enqueue (tq question regexp closure fn)
  "Add a transaction to transaction queue TQ.
This sends the string QUESTION to the process that TQ communicates with.
When the corresponding answer comes back, we call FN
with two arguments: CLOSURE, and the answer to the question.
REGEXP is a regular expression to match the entire answer;
that's how we tell where the answer ends."
  (tq-queue-add tq regexp closure fn)
  (process-send-string (tq-process tq) question))

(defun tq-close (tq)
  "Shut down transaction queue TQ, terminating the process."
  (delete-process (tq-process tq))
  (kill-buffer (tq-buffer tq)))

(defun tq-filter (tq string)
  "Append STRING to the TQ's buffer; then process the new data."
  (set-buffer (tq-buffer tq))
  (goto-char (point-max))
  (insert string)
  (tq-process-buffer tq))

(defun tq-process-buffer (tq)
  "Check TQ's buffer for the regexp at the head of the queue."
  (set-buffer (tq-buffer tq))
  (if (= 0 (buffer-size)) ()
    (if (tq-queue-empty tq)
	(let ((buf (generate-new-buffer "*spurious*")))
	  (copy-to-buffer buf (point-min) (point-max))
	  (delete-region (point-min) (point))
	  (pop-to-buffer buf nil)
	  (error (concat "Spurious communication from process "
			 (process-name (tq-process tq))
			 ", see buffer " (buffer-name buf) ".")))
      (goto-char (point-min))
      (if (re-search-forward (tq-queue-head-regexp tq) nil t)
	  (let ((answer (buffer-substring (point-min) (point))))
	    (delete-region (point-min) (point))
	    (funcall (tq-queue-head-fn tq)
		     (tq-queue-head-closure tq)
		     answer)
	    (tq-queue-pop tq)
	    (tq-process-buffer tq))))))

(provide 'tq)

;;; tq.el ends here
