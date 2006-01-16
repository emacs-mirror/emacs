;;; gnus-gl.el --- an interface to GroupLens for Gnus

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005 Free Software Foundation, Inc.

;; Author: Brad Miller <bmiller@cs.umn.edu>
;; Keywords: news, score

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GroupLens software and documentation is copyright (c) 1995 by Paul
;; Resnick (Massachusetts Institute of Technology); Brad Miller, John
;; Riedl, Jon Herlocker, and Joseph Konstan (University of Minnesota),
;; and David Maltz (Carnegie-Mellon University).
;;
;; Permission to use, copy, modify, and distribute this documentation
;; for non-commercial and commercial purposes without fee is hereby
;; granted provided that this copyright notice and permission notice
;; appears in all copies and that the names of the individuals and
;; institutions holding this copyright are not used in advertising or
;; publicity pertaining to this software without specific, written
;; prior permission.  The copyright holders make no representations
;; about the suitability of this software and documentation for any
;; purpose.  It is provided ``as is'' without express or implied
;; warranty.
;;
;; The copyright holders request that they be notified of
;; modifications of this code.  Please send electronic mail to
;; grouplens@cs.umn.edu for more information or to announce derived
;; works.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Brad Miller
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User Documentation:
;; To use GroupLens you must load this file.
;; You must also register a pseudonym with the Better Bit Bureau.
;; http://www.cs.umn.edu/Research/GroupLens
;;
;;    ---------------- For your .emacs or .gnus file ----------------
;;
;; As of version 2.5, grouplens now works as a minor mode of
;; gnus-summary-mode.  To get make that work you just need a couple of
;; hooks.
;; (setq gnus-use-grouplens t)
;; (setq grouplens-pseudonym "")
;; (setq grouplens-bbb-host "grouplens.cs.umn.edu")
;;
;; (setq gnus-summary-default-score 0)
;;
;;                              USING GROUPLENS
;; How do I Rate an article??
;;   Before you type n to go to the next article, hit a number from 1-5
;;   Type r in the summary buffer and you will be prompted.
;;   Note that when you're in grouplens-minor-mode 'r' masks the
;;   usual reply binding for 'r'
;;
;; What if, Gasp, I find a bug???
;; Please type M-x gnus-gl-submit-bug-report.  This will set up a
;; mail buffer with the  state of variables and buffers that will help
;; me debug the problem.  A short description up front would help too!
;;
;; How do I display the prediction for an article:
;;  If you set the gnus-summary-line-format as shown above, the score
;;  (prediction) will be shown automatically.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programmer  Notes
;; 10/9/95
;; gnus-scores-articles contains the articles
;; When scoring is done, the call tree looks something like:
;; gnus-possibly-score-headers
;;  ==> gnus-score-headers
;;      ==> gnus-score-load-file
;;          ==> get-all-mids  (from the eval form)
;;
;; it would be nice to have one that gets called after all the other
;; headers have been scored.
;; we may want a variable gnus-grouplens-scale-factor
;; and gnus-grouplens-offset  this would probably be either -3 or 0
;; to make the scores centered around zero or not.
;; Notes 10/12/95
;; According to Lars, Norse god of gnus, the simple way to insert a
;; call to an external function is to have a function added to the
;; variable gnus-score-find-files-function  This new function
;; gnus-grouplens-score-alist will return a core alist that
;; has (("message-id" ("<message-id-xxxx>" score) ("<message-id-xxxy>" score))
;; This seems like it would be pretty inefficient, though workable.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 3. Add some more ways to rate messages
;; 4. Better error handling for token timeouts.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bugs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus-score)
(require 'gnus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gnus-summary-grouplens-line-format
  "%U\%R\%z%l%I\%(%[%4L: %-23,23n%]%) %s\n"
  "*The line format spec in summary GroupLens mode buffers.")

(defvar grouplens-pseudonym ""
  "User's pseudonym.
This pseudonym is obtained during the registration process")

(defvar grouplens-bbb-host "grouplens.cs.umn.edu"
  "Host where the bbbd is running.")

(defvar grouplens-bbb-port 9000
  "Port where the bbbd is listening.")

(defvar grouplens-newsgroups
  '("comp.groupware" "comp.human-factors" "comp.lang.c++"
    "comp.lang.java" "comp.os.linux.admin" "comp.os.linux.advocacy"
    "comp.os.linux.announce" "comp.os.linux.answers"
    "comp.os.linux.development" "comp.os.linux.development.apps"
    "comp.os.linux.development.system" "comp.os.linux.hardware"
    "comp.os.linux.help" "comp.os.linux.m68k" "comp.os.linux.misc"
    "comp.os.linux.networking" "comp.os.linux.setup" "comp.os.linux.x"
    "mn.general" "rec.arts.movies" "rec.arts.movies.current-films"
    "rec.food.recipes" "rec.humor")
  "*Groups that are part of the GroupLens experiment.")

(defvar grouplens-prediction-display 'prediction-spot
  "valid values are:
      prediction-spot -- an * corresponding to the prediction between 1 and 5,
      confidence-interval -- a numeric confidence interval
      prediction-bar --  |#####     | the longer the bar, the better the article,
      confidence-bar --  |  -----   } the prediction is in the middle of the bar,
      confidence-spot -- )  *       | the spot gets bigger with more confidence,
      prediction-num  --   plain-old numeric value,
      confidence-plus-minus  -- prediction +/i confidence")

(defvar grouplens-score-offset 0
  "Offset the prediction by this value.
Setting this variable to -2 would have the following effect on
GroupLens scores:

   1   -->   -2
   2   -->   -1
   3   -->    0
   4   -->    1
   5   -->    2

The reason is that a user might want to do this is to combine
GroupLens predictions with scores calculated by other score methods.")

(defvar grouplens-score-scale-factor 1
  "This variable allows the user to magnify the effect of GroupLens scores.
The scale factor is applied after the offset.")

(defvar gnus-grouplens-override-scoring 'override
  "Tell GroupLens to override the normal Gnus scoring mechanism.
GroupLens scores can be combined with gnus scores in one of three ways.
'override -- just use grouplens predictions for grouplens groups
'combine  -- combine grouplens scores with gnus scores
'separate -- treat grouplens scores completely separate from gnus")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Program global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar grouplens-bbb-token nil
  "Current session token number.")

(defvar grouplens-bbb-process nil
  "Process Id of current bbbd network stream process.")

(defvar grouplens-bbb-buffer nil
  "Buffer associated with the BBBD process.")

(defvar grouplens-rating-alist nil
  "Current set of  message-id rating pairs.")

(defvar grouplens-current-hashtable nil
  "A hashtable to hold predictions from the BBB.")

(defvar grouplens-current-group nil)

;;(defvar bbb-alist nil)

(defvar bbb-timeout-secs 10
  "Number of seconds to wait for some response from the BBB.
If this times out we give up and assume that something has died..." )

(defvar grouplens-previous-article nil
  "Message-ID of the last article read.")

(defvar bbb-read-point)
(defvar bbb-response-point)

(defun bbb-renew-hash-table ()
  (setq grouplens-current-hashtable (make-vector 100 0)))

(bbb-renew-hash-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bbb-connect-to-bbbd (host port)
  (unless grouplens-bbb-buffer
    (setq grouplens-bbb-buffer
	  (gnus-get-buffer-create (format " *BBBD trace: %s*" host)))
    (save-excursion
      (set-buffer grouplens-bbb-buffer)
      (make-local-variable 'bbb-read-point)
      (make-local-variable 'bbb-response-point)
      (setq bbb-read-point (point-min))))

  ;; if an old process is still running for some reason, kill it
  (when grouplens-bbb-process
    (ignore-errors
      (when (eq 'open (process-status grouplens-bbb-process))
	(set-process-buffer grouplens-bbb-process nil)
	(delete-process grouplens-bbb-process))))

  ;; clear the trace buffer of old output
  (save-excursion
    (set-buffer grouplens-bbb-buffer)
    (erase-buffer))

  ;; open the connection to the server
  (catch 'done
    (condition-case error
	(setq grouplens-bbb-process
	      (open-network-stream "BBBD" grouplens-bbb-buffer host port))
      (error (gnus-message 3 "Error: Failed to connect to BBB")
	     nil))
    (and (null grouplens-bbb-process)
	 (throw 'done nil))
    (save-excursion
      (set-buffer grouplens-bbb-buffer)
      (setq bbb-read-point (point-min))
      (or (bbb-read-response grouplens-bbb-process)
	  (throw 'done nil))))

  ;; return the process
  grouplens-bbb-process)

(defun bbb-send-command (process command)
  (goto-char (point-max))
  (insert command)
  (insert "\r\n")
  (setq bbb-read-point (point))
  (setq bbb-response-point (point))
  (set-marker (process-mark process) (point)) ; process output also comes here
  (process-send-string process command)
  (process-send-string process "\r\n")
  (process-send-eof process))

(defun bbb-read-response (process)
  "This function eats the initial response of OK or ERROR from the BBB."
  (let ((case-fold-search nil)
	match-end)
    (goto-char bbb-read-point)
    (while (and (not (search-forward "\r\n" nil t))
		(accept-process-output process bbb-timeout-secs))
      (goto-char bbb-read-point))
    (setq match-end (point))
    (goto-char bbb-read-point)
    (setq bbb-read-point match-end)
    (looking-at "OK")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Login Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bbb-login ()
  "return the token number if login is successful, otherwise return nil."
  (interactive)
  (setq grouplens-bbb-token nil)
  (if (not (equal grouplens-pseudonym ""))
      (let ((bbb-process
	     (bbb-connect-to-bbbd grouplens-bbb-host grouplens-bbb-port)))
	(if bbb-process
	    (save-excursion
	      (set-buffer (process-buffer bbb-process))
	      (bbb-send-command bbb-process
				(concat "login " grouplens-pseudonym))
	      (if (bbb-read-response bbb-process)
		  (setq grouplens-bbb-token (bbb-extract-token-number))
		(gnus-message 3 "Error: GroupLens login failed")))))
    (gnus-message 3 "Error: you must set a pseudonym"))
  grouplens-bbb-token)

(defun bbb-extract-token-number ()
  (let ((token-pos (search-forward "token=" nil t)))
    (when (looking-at "[0-9]+")
      (buffer-substring token-pos (match-end 0)))))

(gnus-add-shutdown 'bbb-logout 'gnus)

(defun bbb-logout ()
  "logout of bbb session."
  (when grouplens-bbb-token
    (let ((bbb-process
	   (bbb-connect-to-bbbd grouplens-bbb-host grouplens-bbb-port)))
      (when bbb-process
	(save-excursion
	  (set-buffer (process-buffer bbb-process))
	  (bbb-send-command bbb-process (concat "logout " grouplens-bbb-token))
	  (bbb-read-response bbb-process))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Get Predictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bbb-build-mid-scores-alist (groupname)
  "this function can be called as part of the function to return the list of score files to use.
See the gnus variable `gnus-score-find-score-files-function'.

*Note:*  If you want to use grouplens scores along with calculated scores,
you should see the offset and scale variables.  At this point, I don't
recommend using both scores and grouplens predictions together."
  (setq grouplens-current-group groupname)
  (when (member groupname grouplens-newsgroups)
    (setq grouplens-previous-article nil)
    ;; scores-alist should be a list of lists:
    ;;  ((("message-id" ("<mid1>" score1 nil s) ("<mid2> score2 nil s))))
    ;;`((("message-id" . ,predict-list))) ; Yes, this is the return value
    (list
     (list
      (list (append (list "message-id")
		    (bbb-get-predictions (bbb-get-all-mids) groupname)))))))

(defun bbb-get-predictions (midlist groupname)
  "Ask the bbb for predictions, and build up the score alist."
  (gnus-message 5 "Fetching Predictions...")
  (if grouplens-bbb-token
      (let ((bbb-process (bbb-connect-to-bbbd grouplens-bbb-host
					      grouplens-bbb-port)))
	(when bbb-process
	  (save-excursion
	    (set-buffer (process-buffer bbb-process))
	    (bbb-send-command bbb-process
			      (bbb-build-predict-command midlist groupname
							 grouplens-bbb-token))
	    (if (bbb-read-response bbb-process)
		(bbb-get-prediction-response bbb-process)
	      (gnus-message 1 "Invalid Token, login and try again")
	      (ding)))))
    (gnus-message 3 "Error: You are not logged in to a BBB")
    (ding)))

(defun bbb-get-all-mids ()
  (mapcar (function (lambda (x) (mail-header-id x))) gnus-newsgroup-headers))

(defun bbb-build-predict-command (mlist grpname token)
  (concat "getpredictions " token " " grpname "\r\n"
	  (mapconcat 'identity mlist "\r\n") "\r\n.\r\n"))

(defun bbb-get-prediction-response (process)
  (let ((case-fold-search nil))
    (goto-char bbb-read-point)
    (while (and (not (search-forward ".\r\n" nil t))
		(accept-process-output process bbb-timeout-secs))
      (goto-char bbb-read-point))
    (goto-char (+ bbb-response-point 4));; we ought to be right before OK
    (bbb-build-response-alist)))

;; build-response-alist assumes that the cursor has been positioned at
;; the first line of the list of mid/rating pairs.
(defun bbb-build-response-alist ()
  (let (resp mid pred)
    (while
	(cond
	 ((looking-at "\\(<.*>\\) :nopred=")
	  ;;(push `(,(bbb-get-mid) ,gnus-summary-default-score nil s) resp)
	  (forward-line 1)
	  t)
	 ((looking-at "\\(<.*>\\) :pred=\\([0-9]\.[0-9][0-9]\\) :conflow=\\([0-9]\.[0-9][0-9]\\) :confhigh=\\([0-9]\.[0-9][0-9]\\)")
	  (setq mid (bbb-get-mid)
		pred (bbb-get-pred))
	  (push `(,mid ,pred nil s) resp)
	  (gnus-sethash mid (list pred (bbb-get-confl) (bbb-get-confh))
			grouplens-current-hashtable)
	  (forward-line 1)
	  t)
	 ((looking-at "\\(<.*>\\) :pred=\\([0-9]\.[0-9][0-9]\\)")
	  (setq mid (bbb-get-mid)
		pred (bbb-get-pred))
	  (push `(,mid ,pred nil s) resp)
	  (gnus-sethash mid (list pred 0 0) grouplens-current-hashtable)
	  (forward-line 1)
	  t)
	 (t nil)))
    resp))

;; these "get" functions assume that there is an active match lying
;; around.  Where the first parenthesized expression is the
;; message-id, and the second is the prediction, the third and fourth
;; are the confidence interval
;;
;; Since gnus assumes that scores are integer values?? we round the
;; prediction.
(defun bbb-get-mid ()
  (buffer-substring (match-beginning 1) (match-end 1)))

(defun bbb-get-pred ()
  (let ((tpred (string-to-number (buffer-substring (match-beginning 2)
						   (match-end 2)))))
    (if (> tpred 0)
	(round (* grouplens-score-scale-factor
		  (+ grouplens-score-offset tpred)))
      1)))

(defun bbb-get-confl ()
  (string-to-number (buffer-substring (match-beginning 4) (match-end 4))))

(defun bbb-get-confh ()
  (string-to-number (buffer-substring (match-beginning 4) (match-end 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Prediction Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst grplens-rating-range 4.0)
(defconst grplens-maxrating 5)
(defconst grplens-minrating 1)
(defconst grplens-predstringsize 12)

(defvar gnus-tmp-score)
(defun bbb-grouplens-score (header)
  (if (eq gnus-grouplens-override-scoring 'separate)
      (bbb-grouplens-other-score header)
    (let* ((rate-string (make-string 12 ?\ ))
	   (mid (mail-header-id header))
	   (hashent (gnus-gethash mid grouplens-current-hashtable))
	   (iscore gnus-tmp-score)
	   (low (car (cdr hashent)))
	   (high (car (cdr (cdr hashent)))))
      (aset rate-string 0 ?|)
      (aset rate-string 11 ?|)
      (unless (member grouplens-current-group grouplens-newsgroups)
	(unless (equal grouplens-prediction-display 'prediction-num)
	  (cond ((< iscore 0)
		 (setq iscore 1))
		((> iscore 5)
		 (setq iscore 5))))
	(setq low 0)
	(setq high 0))
      (if (and (bbb-valid-score iscore)
	       (not (null mid)))
	  (cond
	   ;; prediction-spot
	   ((equal grouplens-prediction-display 'prediction-spot)
	    (setq rate-string (bbb-fmt-prediction-spot rate-string iscore)))
	   ;; confidence-interval
	   ((equal grouplens-prediction-display 'confidence-interval)
	    (setq rate-string (bbb-fmt-confidence-interval iscore low high)))
	   ;; prediction-bar
	   ((equal grouplens-prediction-display 'prediction-bar)
	    (setq rate-string (bbb-fmt-prediction-bar rate-string iscore)))
	   ;; confidence-bar
	   ((equal grouplens-prediction-display 'confidence-bar)
	    (setq rate-string (format "|   %4.2f   |" iscore)))
	   ;; confidence-spot
	   ((equal grouplens-prediction-display 'confidence-spot)
	    (setq rate-string (format "|   %4.2f   |" iscore)))
	   ;; prediction-num
	   ((equal grouplens-prediction-display 'prediction-num)
	    (setq rate-string (bbb-fmt-prediction-num iscore)))
	   ;; confidence-plus-minus
	   ((equal grouplens-prediction-display 'confidence-plus-minus)
	    (setq rate-string (bbb-fmt-confidence-plus-minus iscore low high))
	    )
	   (t (gnus-message 3 "Invalid prediction display type")))
	(aset rate-string 5 ?N) (aset rate-string 6 ?A))
      rate-string)))

;; Gnus user format function that doesn't depend on
;; bbb-build-mid-scores-alist being used as the score function, but is
;; instead called from gnus-select-group-hook. -- LAB
(defun bbb-grouplens-other-score (header)
  (if (not (member grouplens-current-group grouplens-newsgroups))
      ;; Return an empty string
      ""
    (let* ((rate-string (make-string 12 ?\ ))
	   (mid (mail-header-id header))
	   (hashent (gnus-gethash mid grouplens-current-hashtable))
	   (pred (or (nth 0 hashent) 0))
	   (low (nth 1 hashent))
	   (high (nth 2 hashent)))
      ;; Init rate-string
      (aset rate-string 0 ?|)
      (aset rate-string 11 ?|)
      (unless (equal grouplens-prediction-display 'prediction-num)
	(cond ((< pred 0)
	       (setq pred 1))
	      ((> pred 5)
	       (setq pred 5))))
      ;; If no entry in BBB hash mark rate string as NA and return
      (cond
       ((null hashent)
	(aset rate-string 5 ?N)
	(aset rate-string 6 ?A)
	rate-string)

       ((equal grouplens-prediction-display 'prediction-spot)
	(bbb-fmt-prediction-spot rate-string pred))

       ((equal grouplens-prediction-display 'confidence-interval)
	(bbb-fmt-confidence-interval pred low high))

       ((equal grouplens-prediction-display 'prediction-bar)
	(bbb-fmt-prediction-bar rate-string pred))

       ((equal grouplens-prediction-display 'confidence-bar)
	(format "|   %4.2f   |" pred))

       ((equal grouplens-prediction-display 'confidence-spot)
	(format "|   %4.2f   |" pred))

       ((equal grouplens-prediction-display 'prediction-num)
	(bbb-fmt-prediction-num pred))

       ((equal grouplens-prediction-display 'confidence-plus-minus)
	(bbb-fmt-confidence-plus-minus pred low high))

       (t
	(gnus-message 3 "Invalid prediction display type")
	(aset rate-string 0 ?|)
	(aset rate-string 11 ?|)
	rate-string)))))

(defun bbb-valid-score (score)
  (or (equal grouplens-prediction-display 'prediction-num)
      (and (>= score grplens-minrating)
	   (<= score grplens-maxrating))))

(defun bbb-requires-confidence (format-type)
  (or (equal format-type 'confidence-plus-minus)
      (equal format-type 'confidence-spot)
      (equal format-type 'confidence-interval)))

(defun bbb-have-confidence (clow chigh)
  (not (or (null clow)
	   (null chigh))))

(defun bbb-fmt-prediction-spot (rate-string score)
  (aset rate-string
	(round (* (/ (- score grplens-minrating) grplens-rating-range)
		  (+ (- grplens-predstringsize 4) 1.49)))
	?*)
  rate-string)

(defun bbb-fmt-confidence-interval (score low high)
  (if (bbb-have-confidence low high)
      (format "|%4.2f-%4.2f |" low high)
    (bbb-fmt-prediction-num score)))

(defun bbb-fmt-confidence-plus-minus (score low high)
  (if (bbb-have-confidence low high)
      (format "|%3.1f+/-%4.2f|" score (/ (- high low) 2.0))
    (bbb-fmt-prediction-num score)))

(defun bbb-fmt-prediction-bar (rate-string score)
  (let* ((i 1)
	 (step (/ grplens-rating-range (- grplens-predstringsize 4)))
	 (half-step (/ step 2))
	 (loc (- grplens-minrating half-step)))
    (while (< i (- grplens-predstringsize 2))
      (if (> score loc)
	  (aset rate-string i ?#)
	(aset rate-string i ?\ ))
      (setq i (+ i 1))
      (setq loc (+ loc step)))
    )
  rate-string)

(defun bbb-fmt-prediction-num (score)
  (format "|   %4.2f   |" score))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Put Ratings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bbb-put-ratings ()
  (if (and grouplens-bbb-token
	   grouplens-rating-alist
	   (member gnus-newsgroup-name grouplens-newsgroups))
      (let ((bbb-process (bbb-connect-to-bbbd grouplens-bbb-host
					      grouplens-bbb-port))
	    (rate-command (bbb-build-rate-command grouplens-rating-alist)))
	(if bbb-process
	    (save-excursion
	      (set-buffer (process-buffer bbb-process))
	      (gnus-message 5 "Sending Ratings...")
	      (bbb-send-command bbb-process rate-command)
	      (if (bbb-read-response bbb-process)
		  (setq grouplens-rating-alist nil)
		(gnus-message 1
			      "Token timed out: call bbb-login and quit again")
		(ding))
	      (gnus-message 5 "Sending Ratings...Done"))
	  (gnus-message 3 "No BBB connection")))
    (setq grouplens-rating-alist nil)))

(defun bbb-build-rate-command (rate-alist)
  (concat "putratings " grouplens-bbb-token " " grouplens-current-group " \r\n"
	  (mapconcat (lambda (this)	; form (mid . (score . time))
		       (concat (car this)
			       " :rating=" (cadr this) ".00"
			       " :time=" (cddr this)))
		     rate-alist "\r\n")
	  "\r\n.\r\n"))

;; Interactive rating functions.
(defun bbb-summary-rate-article (rating &optional midin)
  (interactive "nRating: ")
  (when (member gnus-newsgroup-name grouplens-newsgroups)
    (let ((mid (or midin (bbb-get-current-id))))
      (if (and rating
	       (>= rating grplens-minrating)
	       (<= rating grplens-maxrating)
	       mid)
	  (let ((oldrating (assoc mid grouplens-rating-alist)))
	    (if oldrating
		(setcdr oldrating (cons rating 0))
	      (push `(,mid . (,rating . 0)) grouplens-rating-alist))
	    (gnus-summary-mark-article nil (int-to-string rating)))
	(gnus-message 3 "Invalid rating")))))

(defun grouplens-next-unread-article (rating)
  "Select unread article after current one."
  (interactive "P")
  (when rating
    (bbb-summary-rate-article rating))
  (gnus-summary-next-unread-article))

(defun grouplens-best-unread-article (rating)
  "Select unread article after current one."
  (interactive "P")
  (when rating
    (bbb-summary-rate-article rating))
  (gnus-summary-best-unread-article))

(defun grouplens-summary-catchup-and-exit (rating)
  "Mark all articles not marked as unread in this newsgroup as read, then exit.
If prefix argument ALL is non-nil, all articles are marked as read."
  (interactive "P")
  (when rating
    (bbb-summary-rate-article rating))
  (if (numberp rating)
      (gnus-summary-catchup-and-exit)
    (gnus-summary-catchup-and-exit rating)))

(defun grouplens-score-thread (score)
  "Raise the score of the articles in the current thread with SCORE."
  (interactive "nRating: ")
  (let (e)
    (save-excursion
      (let ((articles (gnus-summary-articles-in-thread))
	    article)
	(while (setq article (pop articles))
	  (gnus-summary-goto-subject article)
	  (bbb-summary-rate-article score
				    (mail-header-id
				     (gnus-summary-article-header article)))))
      (setq e (point)))
    (let ((gnus-summary-check-current t))
      (or (zerop (gnus-summary-next-subject 1 t))
	  (goto-char e))))
  (gnus-summary-recenter)
  (gnus-summary-position-point)
  (gnus-set-mode-line 'summary))

(defun bbb-exit-group ()
  (bbb-put-ratings)
  (bbb-renew-hash-table))

(defun bbb-get-current-id ()
  (if gnus-current-headers
      (mail-header-id gnus-current-headers)
    (gnus-message 3 "You must select an article before you rate it")))

(defun bbb-grouplens-group-p (group)
  "Say whether GROUP is a GroupLens group."
  (if (member group grouplens-newsgroups) " (GroupLens Enhanced)" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          TIME SPENT READING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar grouplens-current-starting-time nil)

(defun grouplens-start-timer ()
  (setq grouplens-current-starting-time (current-time)))

(defun grouplens-elapsed-time ()
  (let ((et (bbb-time-float (current-time))))
    (- et (bbb-time-float grouplens-current-starting-time))))

(defun bbb-time-float (timeval)
  (+ (* (car timeval) 65536)
     (cadr timeval)))

(defun grouplens-do-time ()
  (when (member gnus-newsgroup-name grouplens-newsgroups)
    (when grouplens-previous-article
      (let ((elapsed-time (grouplens-elapsed-time))
	    (oldrating (assoc grouplens-previous-article
			      grouplens-rating-alist)))
	(if (not oldrating)
	    (push `(,grouplens-previous-article . (0 . ,elapsed-time))
		  grouplens-rating-alist)
	  (setcdr oldrating (cons (cadr oldrating) elapsed-time)))))
    (grouplens-start-timer)
    (setq grouplens-previous-article (bbb-get-current-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          BUG REPORTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst gnus-gl-version "gnus-gl.el 2.50")
(defconst gnus-gl-maintainer-address "grouplens-bug@cs.umn.edu")
(defun gnus-gl-submit-bug-report ()
  "Submit via mail a bug report on gnus-gl."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report gnus-gl-maintainer-address
			      (concat "gnus-gl.el " gnus-gl-version)
			      (list 'grouplens-pseudonym
				    'grouplens-bbb-host
				    'grouplens-bbb-port
				    'grouplens-newsgroups
				    'grouplens-bbb-token
				    'grouplens-bbb-process
				    'grouplens-current-group
				    'grouplens-previous-article)
			      nil
			      'gnus-gl-get-trace))

(defun gnus-gl-get-trace ()
  "Insert the contents of the BBBD trace buffer."
  (when grouplens-bbb-buffer
    (insert-buffer-substring grouplens-bbb-buffer)))

;;
;; GroupLens minor mode
;;

(defvar gnus-grouplens-mode nil
  "Minor mode for providing a GroupLens interface in Gnus summary buffers.")

(defvar gnus-grouplens-mode-map nil)

(unless gnus-grouplens-mode-map
  (setq gnus-grouplens-mode-map (make-keymap))
  (gnus-define-keys
      gnus-grouplens-mode-map
    "n" grouplens-next-unread-article
    "r" bbb-summary-rate-article
    "k" grouplens-score-thread
    "c" grouplens-summary-catchup-and-exit
    "," grouplens-best-unread-article))

(defun gnus-grouplens-make-menu-bar ()
  (unless (boundp 'gnus-grouplens-menu)
    (easy-menu-define
     gnus-grouplens-menu gnus-grouplens-mode-map ""
     '("GroupLens"
       ["Login" bbb-login t]
       ["Rate" bbb-summary-rate-article t]
       ["Next article" grouplens-next-unread-article t]
       ["Best article" grouplens-best-unread-article t]
       ["Raise thread" grouplens-score-thread t]
       ["Report bugs" gnus-gl-submit-bug-report t]))))

(defun gnus-grouplens-mode (&optional arg)
  "Minor mode for providing a GroupLens interface in Gnus summary buffers."
  (interactive "P")
  (when (and (eq major-mode 'gnus-summary-mode)
	     (member gnus-newsgroup-name grouplens-newsgroups))
    (make-local-variable 'gnus-grouplens-mode)
    (setq gnus-grouplens-mode
	  (if (null arg) (not gnus-grouplens-mode)
	    (> (prefix-numeric-value arg) 0)))
    (when gnus-grouplens-mode
      (gnus-make-local-hook 'gnus-select-article-hook)
      (add-hook 'gnus-select-article-hook 'grouplens-do-time nil 'local)
      (gnus-make-local-hook 'gnus-exit-group-hook)
      (add-hook 'gnus-exit-group-hook 'bbb-exit-group nil 'local)
      (make-local-variable 'gnus-score-find-score-files-function)

      (cond
       ((eq gnus-grouplens-override-scoring 'combine)
	;; either add bbb-buld-mid-scores-alist to a list
	;; or make a list
	(if (listp gnus-score-find-score-files-function)
	    (setq gnus-score-find-score-files-function
		  (append 'bbb-build-mid-scores-alist
			  gnus-score-find-score-files-function))
	  (setq gnus-score-find-score-files-function
		(list gnus-score-find-score-files-function
		      'bbb-build-mid-scores-alist))))
       ;; leave the gnus-score-find-score-files variable alone
       ((eq gnus-grouplens-override-scoring 'separate)
	(add-hook 'gnus-select-group-hook
		  (lambda ()
		    (bbb-get-predictions (bbb-get-all-mids)
					 gnus-newsgroup-name))))
       ;; default is to override
       (t
	(setq gnus-score-find-score-files-function
	      'bbb-build-mid-scores-alist)))

      ;; Change how summary lines look
      (make-local-variable 'gnus-summary-line-format)
      (make-local-variable 'gnus-summary-line-format-spec)
      (setq gnus-summary-line-format gnus-summary-grouplens-line-format)
      (setq gnus-summary-line-format-spec nil)
      (gnus-update-format-specifications nil 'summary)
      (gnus-update-summary-mark-positions)

      ;; Set up the menu.
      (when (and menu-bar-mode
		 (gnus-visual-p 'grouplens-menu 'menu))
	(gnus-grouplens-make-menu-bar))
      (gnus-add-minor-mode
       'gnus-grouplens-mode " GroupLens" gnus-grouplens-mode-map)
      (gnus-run-hooks 'gnus-grouplens-mode-hook))))

(provide 'gnus-gl)

;;; arch-tag: 6f1bab2c-c2a3-4764-9ef6-0714cd5902a4
;;; gnus-gl.el ends here
