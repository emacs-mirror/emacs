;;; landmark.el --- neural-network robot that learns landmarks

;; Copyright (C) 1996, 1997, 2000, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Terrence Brannon (was: <brannon@rana.usc.edu>)
;; Created: December 16, 1996 - first release to usenet
;; Keywords: gomoku, neural network, adaptive search, chemotaxis

;;;_* Usage
;;; Just type
;;;   M-x eval-current-buffer
;;;   M-x lm-test-run


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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;;; Lm is a relatively non-participatory game in which a robot
;;; attempts to maneuver towards a tree at the center of the window
;;; based on unique olfactory cues from each of the 4 directions. If
;;; the smell of the tree increases, then the weights in the robot's
;;; brain are adjusted to encourage this odor-driven behavior in the
;;; future. If the smell of the tree decreases, the robots weights are
;;; adjusted to discourage a correct move.

;;; In laymen's terms, the search space is initially flat. The point
;;; of training is to "turn up the edges of the search space" so that
;;; the robot rolls toward the center.

;;; Further, do not become alarmed if the robot appears to oscillate
;;; back and forth between two or a few positions. This simply means
;;; it is currently caught in a local minimum and is doing its best to
;;; work its way out.

;;; The version of this program as described has a small problem. a
;;; move in a net direction can produce gross credit assignment. for
;;; example, if moving south will produce positive payoff, then, if in
;;; a single move, one moves east,west and south, then both east and
;;; west will be improved when they shouldn't

;;; Many thanks to Yuri Pryadkin (yuri@rana.usc.edu) for this
;;; concise problem description.

;;;_* Require
(eval-when-compile (require 'cl))

;;;_* From Gomoku

;;; Code:

(defgroup lm nil
  "Neural-network robot that learns landmarks."
  :prefix "lm-"
  :group 'games)

;;;_ +  THE BOARD.

;; The board is a rectangular grid. We code empty squares with 0, X's with 1
;; and O's with 6. The rectangle is recorded in a one dimensional vector
;; containing padding squares (coded with -1). These squares allow us to
;; detect when we are trying to move out of the board.	We denote a square by
;; its (X,Y) coords, or by the INDEX corresponding to them in the vector.  The
;; leftmost topmost square has coords (1,1) and index lm-board-width + 2.
;; Similarly, vectors between squares may be given by two DX, DY coords or by
;; one DEPL (the difference between indexes).

(defvar lm-board-width nil
  "Number of columns on the Lm board.")
(defvar lm-board-height nil
  "Number of lines on the Lm board.")

(defvar lm-board nil
  "Vector recording the actual state of the Lm board.")

(defvar lm-vector-length nil
  "Length of lm-board vector.")

(defvar lm-draw-limit nil
  ;; This is usually set to 70% of the number of squares.
  "After how many moves will Emacs offer a draw?")

(defvar lm-cx 0
  "This is the x coordinate of the center of the board.")

(defvar lm-cy 0
  "This is the y coordinate of the center of the board.")

(defvar lm-m 0
  "This is the x dimension of the playing board.")

(defvar lm-n 0
  "This is the y dimension of the playing board.")


(defun lm-xy-to-index (x y)
  "Translate X, Y cartesian coords into the corresponding board index."
  (+ (* y lm-board-width) x y))

(defun lm-index-to-x (index)
  "Return corresponding x-coord of board INDEX."
  (% index (1+ lm-board-width)))

(defun lm-index-to-y (index)
  "Return corresponding y-coord of board INDEX."
  (/ index (1+ lm-board-width)))

(defun lm-init-board ()
  "Create the lm-board vector and fill it with initial values."
  (setq lm-board (make-vector lm-vector-length 0))
  ;; Every square is 0 (i.e. empty) except padding squares:
  (let ((i 0) (ii (1- lm-vector-length)))
    (while (<= i lm-board-width)	; The squares in [0..width] and in
      (aset lm-board i  -1)		;    [length - width - 1..length - 1]
      (aset lm-board ii -1)		;    are padding squares.
      (setq i  (1+ i)
	    ii (1- ii))))
  (let ((i 0))
    (while (< i lm-vector-length)
      (aset lm-board i -1)		; and also all k*(width+1)
      (setq i (+ i lm-board-width 1)))))

;;;_ +  DISPLAYING THE BOARD.

;; You may change these values if you have a small screen or if the squares
;; look rectangular, but spacings SHOULD be at least 2 (MUST BE at least 1).

(defconst lm-square-width 2
  "*Horizontal spacing between squares on the Lm board.")

(defconst lm-square-height 1
  "*Vertical spacing between squares on the Lm board.")

(defconst lm-x-offset 3
  "*Number of columns between the Lm board and the side of the window.")

(defconst lm-y-offset 1
  "*Number of lines between the Lm board and the top of the window.")


;;;_ +  LM MODE AND KEYMAP.

(defcustom lm-mode-hook nil
  "If non-nil, its value is called on entry to Lm mode."
  :type 'hook
  :group 'lm)

(defvar lm-mode-map nil
  "Local keymap to use in Lm mode.")

(if lm-mode-map nil
  (setq lm-mode-map (make-sparse-keymap))

  ;; Key bindings for cursor motion.
  (define-key lm-mode-map "y" 'lm-move-nw)		; y
  (define-key lm-mode-map "u" 'lm-move-ne)		; u
  (define-key lm-mode-map "b" 'lm-move-sw)		; b
  (define-key lm-mode-map "n" 'lm-move-se)		; n
  (define-key lm-mode-map "h" 'backward-char)		; h
  (define-key lm-mode-map "l" 'forward-char)		; l
  (define-key lm-mode-map "j" 'lm-move-down)		; j
  (define-key lm-mode-map "k" 'lm-move-up)		; k

  (define-key lm-mode-map [kp-7] 'lm-move-nw)
  (define-key lm-mode-map [kp-9] 'lm-move-ne)
  (define-key lm-mode-map [kp-1] 'lm-move-sw)
  (define-key lm-mode-map [kp-3] 'lm-move-se)
  (define-key lm-mode-map [kp-4] 'backward-char)
  (define-key lm-mode-map [kp-6] 'forward-char)
  (define-key lm-mode-map [kp-2] 'lm-move-down)
  (define-key lm-mode-map [kp-8] 'lm-move-up)

  (define-key lm-mode-map "\C-n" 'lm-move-down)		; C-n
  (define-key lm-mode-map "\C-p" 'lm-move-up)		; C-p

  ;; Key bindings for entering Human moves.
  (define-key lm-mode-map "X" 'lm-human-plays)		; X
  (define-key lm-mode-map "x" 'lm-human-plays)		; x

  (define-key lm-mode-map " " 'lm-start-robot)		; SPC
  (define-key lm-mode-map [down-mouse-1] 'lm-start-robot)
  (define-key lm-mode-map [drag-mouse-1] 'lm-click)
  (define-key lm-mode-map [mouse-1] 'lm-click)
  (define-key lm-mode-map [down-mouse-2] 'lm-click)
  (define-key lm-mode-map [mouse-2] 'lm-mouse-play)
  (define-key lm-mode-map [drag-mouse-2] 'lm-mouse-play)

  (define-key lm-mode-map [remap previous-line] 'lm-move-up)
  (define-key lm-mode-map [remap next-line] 'lm-move-down)
  (define-key lm-mode-map [remap beginning-of-line] 'lm-beginning-of-line)
  (define-key lm-mode-map [remap end-of-line] 'lm-end-of-line)
  (define-key lm-mode-map [remap undo] 'lm-human-takes-back)
  (define-key lm-mode-map [remap advertised-undo] 'lm-human-takes-back))

(defvar lm-emacs-won ()
  "*For making font-lock use the winner's face for the line.")

(defvar lm-font-lock-face-O
  (if (display-color-p)
      (list (facemenu-get-face 'fg:red) 'bold))
  "*Face to use for Emacs' O.")

(defvar lm-font-lock-face-X
  (if (display-color-p)
      (list (facemenu-get-face 'fg:green) 'bold))
  "*Face to use for your X.")

(defvar lm-font-lock-keywords
  '(("O" . lm-font-lock-face-O)
    ("X" . lm-font-lock-face-X)
    ("[-|/\\]" 0 (if lm-emacs-won
		     lm-font-lock-face-O
		   lm-font-lock-face-X)))
  "*Font lock rules for Lm.")

(put 'lm-mode 'front-sticky
     (put 'lm-mode 'rear-nonsticky '(intangible)))
(put 'lm-mode 'intangible 1)
;; This one is for when they set view-read-only to t: Landmark cannot
;; allow View Mode to be activated in its buffer.
(put 'lm-mode 'mode-class 'special)

(defun lm-mode ()
  "Major mode for playing Lm against Emacs.
You and Emacs play in turn by marking a free square.  You mark it with X
and Emacs marks it with O.  The winner is the first to get five contiguous
marks horizontally, vertically or in diagonal.

You play by moving the cursor over the square you choose and hitting \\[lm-human-plays].

Other useful commands:
\\{lm-mode-map}
Entry to this mode calls the value of `lm-mode-hook' if that value
is non-nil.  One interesting value is `turn-on-font-lock'."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lm-mode
	mode-name "Lm")
  (lm-display-statistics)
  (use-local-map lm-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(lm-font-lock-keywords t))
  (toggle-read-only t)
  (run-mode-hooks 'lm-mode-hook))


;;;_ +  THE SCORE TABLE.


;; Every (free) square has a score associated to it, recorded in the
;; LM-SCORE-TABLE vector. The program always plays in the square having
;; the highest score.

(defvar lm-score-table nil
  "Vector recording the actual score of the free squares.")


;; The key point point about the algorithm is that, rather than considering
;; the board as just a set of squares, we prefer to see it as a "space" of
;; internested 5-tuples of contiguous squares (called qtuples).
;;
;; The aim of the program is to fill one qtuple with its O's while preventing
;; you from filling another one with your X's. To that effect, it computes a
;; score for every qtuple, with better qtuples having better scores. Of
;; course, the score of a qtuple (taken in isolation) is just determined by
;; its contents as a set, i.e. not considering the order of its elements. The
;; highest score is given to the "OOOO" qtuples because playing in such a
;; qtuple is winning the game. Just after this comes the "XXXX" qtuple because
;; not playing in it is just loosing the game, and so on. Note that a
;; "polluted" qtuple, i.e. one containing at least one X and at least one O,
;; has score zero because there is no more any point in playing in it, from
;; both an attacking and a defending point of view.
;;
;; Given the score of every qtuple, the score of a given free square on the
;; board is just the sum of the scores of all the qtuples to which it belongs,
;; because playing in that square is playing in all its containing qtuples at
;; once. And it is that function which takes into account the internesting of
;; the qtuples.
;;
;; This algorithm is rather simple but anyway it gives a not so dumb level of
;; play. It easily extends to "n-dimensional Lm", where a win should not
;; be obtained with as few as 5 contiguous marks: 6 or 7 (depending on n !)
;; should be preferred.


;; Here are the scores of the nine "non-polluted" configurations.  Tuning
;; these values will change (hopefully improve) the strength of the program
;; and may change its style (rather aggressive here).

(defconst nil-score	  7  "Score of an empty qtuple.")
(defconst Xscore	 15  "Score of a qtuple containing one X.")
(defconst XXscore	400  "Score of a qtuple containing two X's.")
(defconst XXXscore     1800  "Score of a qtuple containing three X's.")
(defconst XXXXscore  100000  "Score of a qtuple containing four X's.")
(defconst Oscore	 35  "Score of a qtuple containing one O.")
(defconst OOscore	800  "Score of a qtuple containing two O's.")
(defconst OOOscore    15000  "Score of a qtuple containing three O's.")
(defconst OOOOscore  800000  "Score of a qtuple containing four O's.")

;; These values are not just random: if, given the following situation:
;;
;;			  . . . . . . . O .
;;			  . X X a . . . X .
;;			  . . . X . . . X .
;;			  . . . X . . . X .
;;			  . . . . . . . b .
;;
;; you want Emacs to play in "a" and not in "b", then the parameters must
;; satisfy the inequality:
;;
;;		   6 * XXscore > XXXscore + XXscore
;;
;; because "a" mainly belongs to six "XX" qtuples (the others are less
;; important) while "b" belongs to one "XXX" and one "XX" qtuples.  Other
;; conditions are required to obtain sensible moves, but the previous example
;; should illustrate the point. If you manage to improve on these values,
;; please send me a note. Thanks.


;; As we chose values 0, 1 and 6 to denote empty, X and O squares, the
;; contents of a qtuple are uniquely determined by the sum of its elements and
;; we just have to set up a translation table.

(defconst lm-score-trans-table
  (vector nil-score Xscore XXscore XXXscore XXXXscore 0
	  Oscore    0	   0	   0	    0	      0
	  OOscore   0	   0	   0	    0	      0
	  OOOscore  0	   0	   0	    0	      0
	  OOOOscore 0	   0	   0	    0	      0
	  0)
  "Vector associating qtuple contents to their score.")


;; If you do not modify drastically the previous constants, the only way for a
;; square to have a score higher than OOOOscore is to belong to a "OOOO"
;; qtuple, thus to be a winning move. Similarly, the only way for a square to
;; have a score between XXXXscore and OOOOscore is to belong to a "XXXX"
;; qtuple. We may use these considerations to detect when a given move is
;; winning or loosing.

(defconst lm-winning-threshold OOOOscore
  "Threshold score beyond which an Emacs move is winning.")

(defconst lm-loosing-threshold XXXXscore
  "Threshold score beyond which a human move is winning.")


(defun lm-strongest-square ()
  "Compute index of free square with highest score, or nil if none."
  ;; We just have to loop other all squares. However there are two problems:
  ;; 1/ The SCORE-TABLE only gives correct scores to free squares. To speed
  ;;	up future searches, we set the score of padding or occupied squares
  ;;	to -1 whenever we meet them.
  ;; 2/ We want to choose randomly between equally good moves.
  (let ((score-max 0)
	(count	   0)			; Number of equally good moves
	(square	   (lm-xy-to-index 1 1)) ; First square
	(end	   (lm-xy-to-index lm-board-width lm-board-height))
	best-square score)
    (while (<= square end)
      (cond
       ;; If score is lower (i.e. most of the time), skip to next:
       ((< (aref lm-score-table square) score-max))
       ;; If score is better, beware of non free squares:
       ((> (setq score (aref lm-score-table square)) score-max)
	(if (zerop (aref lm-board square)) ; is it free ?
	    (setq count 1		       ; yes: take it !
		  best-square square
		  score-max   score)
	    (aset lm-score-table square -1))) ; no: kill it !
       ;; If score is equally good, choose randomly. But first check freeness:
       ((not (zerop (aref lm-board square)))
	(aset lm-score-table square -1))
       ((zerop (random (setq count (1+ count))))
	(setq best-square square
	      score-max	  score)))
      (setq square (1+ square)))	; try next square
    best-square))

;;;_  -  INITIALIZING THE SCORE TABLE.

;; At initialization the board is empty so that every qtuple amounts for
;; nil-score. Therefore, the score of any square is nil-score times the number
;; of qtuples that pass through it. This number is 3 in a corner and 20 if you
;; are sufficiently far from the sides. As computing the number is time
;; consuming, we initialize every square with 20*nil-score and then only
;; consider squares at less than 5 squares from one side. We speed this up by
;; taking symmetry into account.
;; Also, as it is likely that successive games will be played on a board with
;; same size, it is a good idea to save the initial SCORE-TABLE configuration.

(defvar lm-saved-score-table nil
  "Recorded initial value of previous score table.")

(defvar lm-saved-board-width nil
  "Recorded value of previous board width.")

(defvar lm-saved-board-height nil
  "Recorded value of previous board height.")


(defun lm-init-score-table ()
  "Create the score table vector and fill it with initial values."
  (if (and lm-saved-score-table	; Has it been stored last time ?
	   (= lm-board-width  lm-saved-board-width)
	   (= lm-board-height lm-saved-board-height))
      (setq lm-score-table (copy-sequence lm-saved-score-table))
      ;; No, compute it:
      (setq lm-score-table
	    (make-vector lm-vector-length (* 20 nil-score)))
      (let (i j maxi maxj maxi2 maxj2)
	(setq maxi  (/ (1+ lm-board-width) 2)
	      maxj  (/ (1+ lm-board-height) 2)
	      maxi2 (min 4 maxi)
	      maxj2 (min 4 maxj))
	;; We took symmetry into account and could use it more if the board
	;; would have been square and not rectangular !
	;; In our case we deal with all (i,j) in the set [1..maxi2]*[1..maxj] U
	;; [maxi2+1..maxi]*[1..maxj2]. Maxi2 and maxj2 are used because the
	;; board may well be less than 8 by 8 !
	(setq i 1)
	(while (<= i maxi2)
	  (setq j 1)
	  (while (<= j maxj)
	    (lm-init-square-score i j)
	    (setq j (1+ j)))
	  (setq i (1+ i)))
	(while (<= i maxi)
	  (setq j 1)
	  (while (<= j maxj2)
	    (lm-init-square-score i j)
	    (setq j (1+ j)))
	  (setq i (1+ i))))
      (setq lm-saved-score-table  (copy-sequence lm-score-table)
	    lm-saved-board-width  lm-board-width
	    lm-saved-board-height lm-board-height)))

(defun lm-nb-qtuples (i j)
  "Return the number of qtuples containing square I,J."
  ;; This function is complicated because we have to deal
  ;; with ugly cases like 3 by 6 boards, but it works.
  ;; If you have a simpler (and correct) solution, send it to me. Thanks !
  (let ((left  (min 4 (1- i)))
	(right (min 4 (- lm-board-width i)))
	(up    (min 4 (1- j)))
	(down  (min 4 (- lm-board-height j))))
    (+ -12
       (min (max (+ left right) 3) 8)
       (min (max (+ up down) 3) 8)
       (min (max (+ (min left up) (min right down)) 3) 8)
       (min (max (+ (min right up) (min left down)) 3) 8))))

(defun lm-init-square-score (i j)
  "Give initial score to square I,J and to its mirror images."
  (let ((ii (1+ (- lm-board-width i)))
	(jj (1+ (- lm-board-height j)))
	(sc (* (lm-nb-qtuples i j) (aref lm-score-trans-table 0))))
    (aset lm-score-table (lm-xy-to-index i  j)	sc)
    (aset lm-score-table (lm-xy-to-index ii j)	sc)
    (aset lm-score-table (lm-xy-to-index i  jj) sc)
    (aset lm-score-table (lm-xy-to-index ii jj) sc)))
;;;_  - MAINTAINING THE SCORE TABLE.


;; We do not provide functions for computing the SCORE-TABLE given the
;; contents of the BOARD. This would involve heavy nested loops, with time
;; proportional to the size of the board. It is better to update the
;; SCORE-TABLE after each move. Updating needs not modify more than 36
;; squares: it is done in constant time.

(defun lm-update-score-table (square dval)
  "Update score table after SQUARE received a DVAL increment."
  ;; The board has already been updated when this function is called.
  ;; Updating scores is done by looking for qtuples boundaries in all four
  ;; directions and then calling update-score-in-direction.
  ;; Finally all squares received the right increment, and then are up to
  ;; date, except possibly for SQUARE itself if we are taking a move back for
  ;; its score had been set to -1 at the time.
  (let* ((x    (lm-index-to-x square))
	 (y    (lm-index-to-y square))
	 (imin (max -4 (- 1 x)))
	 (jmin (max -4 (- 1 y)))
	 (imax (min 0 (- lm-board-width x 4)))
	 (jmax (min 0 (- lm-board-height y 4))))
    (lm-update-score-in-direction imin imax
				      square 1 0 dval)
    (lm-update-score-in-direction jmin jmax
				      square 0 1 dval)
    (lm-update-score-in-direction (max imin jmin) (min imax jmax)
				      square 1 1 dval)
    (lm-update-score-in-direction (max (- 1 y) -4
					   (- x lm-board-width))
				      (min 0 (- x 5)
					   (- lm-board-height y 4))
				      square -1 1 dval)))

(defun lm-update-score-in-direction (left right square dx dy dval)
  "Update scores for all squares in the qtuples in range.
That is, those between the LEFTth square and the RIGHTth after SQUARE,
along the DX, DY direction, considering that DVAL has been added on SQUARE."
  ;; We always have LEFT <= 0, RIGHT <= 0 and DEPL > 0 but we may very well
  ;; have LEFT > RIGHT, indicating that no qtuple contains SQUARE along that
  ;; DX,DY direction.
  (cond
   ((> left right))			; Quit
   (t					; Else ..
    (let (depl square0 square1 square2 count delta)
      (setq depl    (lm-xy-to-index dx dy)
	    square0 (+ square (* left depl))
	    square1 (+ square (* right depl))
	    square2 (+ square0 (* 4 depl)))
      ;; Compute the contents of the first qtuple:
      (setq square square0
	    count  0)
      (while (<= square square2)
	(setq count  (+ count (aref lm-board square))
	      square (+ square depl)))
      (while (<= square0 square1)
	;; Update the squares of the qtuple beginning in SQUARE0 and ending
	;; in SQUARE2.
	(setq delta (- (aref lm-score-trans-table count)
		       (aref lm-score-trans-table (- count dval))))
	(cond ((not (zerop delta))	; or else nothing to update
	       (setq square square0)
	       (while (<= square square2)
		 (if (zerop (aref lm-board square)) ; only for free squares
		     (aset lm-score-table square
			   (+ (aref lm-score-table square) delta)))
		 (setq square (+ square depl)))))
	;; Then shift the qtuple one square along DEPL, this only requires
	;; modifying SQUARE0 and SQUARE2.
	(setq square2 (+ square2 depl)
	      count   (+ count (- (aref lm-board square0))
			 (aref lm-board square2))
	      square0 (+ square0 depl)))))))

;;;
;;; GAME CONTROL.
;;;

;; Several variables are used to monitor a game, including a GAME-HISTORY (the
;; list of all (SQUARE . PREVSCORE) played) that allows to take moves back
;; (anti-updating the score table) and to compute the table from scratch in
;; case of an interruption.

(defvar lm-game-in-progress nil
  "Non-nil if a game is in progress.")

(defvar lm-game-history nil
  "A record of all moves that have been played during current game.")

(defvar lm-number-of-moves nil
  "Number of moves already played in current game.")

(defvar lm-number-of-human-moves nil
  "Number of moves already played by human in current game.")

(defvar lm-emacs-played-first nil
  "Non-nil if Emacs played first.")

(defvar lm-human-took-back nil
  "Non-nil if Human took back a move during the game.")

(defvar lm-human-refused-draw nil
  "Non-nil if Human refused Emacs offer of a draw.")

(defvar lm-emacs-is-computing nil
  ;; This is used to detect interruptions. Hopefully, it should not be needed.
  "Non-nil if Emacs is in the middle of a computation.")


(defun lm-start-game (n m)
  "Initialize a new game on an N by M board."
  (setq lm-emacs-is-computing t)	; Raise flag
  (setq lm-game-in-progress t)
  (setq lm-board-width   n
	lm-board-height  m
	lm-vector-length (1+ (* (+ m 2) (1+ n)))
	lm-draw-limit    (/ (* 7 n m) 10))
  (setq lm-emacs-won	         nil
	lm-game-history	         nil
	lm-number-of-moves	 0
	lm-number-of-human-moves 0
	lm-emacs-played-first    nil
	lm-human-took-back	 nil
	lm-human-refused-draw    nil)
  (lm-init-display n m)		; Display first: the rest takes time
  (lm-init-score-table)		; INIT-BOARD requires that the score
  (lm-init-board)			;   table be already created.
  (setq lm-emacs-is-computing nil))

(defun lm-play-move (square val &optional dont-update-score)
  "Go to SQUARE, play VAL and update everything."
  (setq lm-emacs-is-computing t)	; Raise flag
  (cond ((= 1 val)			; a Human move
	 (setq lm-number-of-human-moves (1+ lm-number-of-human-moves)))
	((zerop lm-number-of-moves)	; an Emacs move. Is it first ?
	 (setq lm-emacs-played-first t)))
  (setq lm-game-history
	(cons (cons square (aref lm-score-table square))
	      lm-game-history)
	lm-number-of-moves (1+ lm-number-of-moves))
  (lm-plot-square square val)
  (aset lm-board square val)	; *BEFORE* UPDATE-SCORE !
  (if dont-update-score nil
      (lm-update-score-table square val) ; previous val was 0: dval = val
      (aset lm-score-table square -1))
  (setq lm-emacs-is-computing nil))

(defun lm-take-back ()
  "Take back last move and update everything."
  (setq lm-emacs-is-computing t)
  (let* ((last-move (car lm-game-history))
	 (square (car last-move))
	 (oldval (aref lm-board square)))
    (if (= 1 oldval)
	(setq lm-number-of-human-moves (1- lm-number-of-human-moves)))
    (setq lm-game-history	 (cdr lm-game-history)
	  lm-number-of-moves (1- lm-number-of-moves))
    (lm-plot-square square 0)
    (aset lm-board square 0)	; *BEFORE* UPDATE-SCORE !
    (lm-update-score-table square (- oldval))
    (aset lm-score-table square (cdr last-move)))
  (setq lm-emacs-is-computing nil))


;;;_ +  SESSION CONTROL.

(defvar lm-number-of-trials 0
  "The number of times that landmark has been run.")

(defvar lm-sum-of-moves 0
  "The total number of moves made in all games.")

(defvar lm-number-of-emacs-wins 0
  "Number of games Emacs won in this session.")

(defvar lm-number-of-human-wins 0
  "Number of games you won in this session.")

(defvar lm-number-of-draws 0
  "Number of games already drawn in this session.")


(defun lm-terminate-game (result)
  "Terminate the current game with RESULT."
  (setq lm-number-of-trials (1+ lm-number-of-trials))
  (setq lm-sum-of-moves (+ lm-sum-of-moves lm-number-of-moves))
  (if (eq result 'crash-game)
      (message
       "Sorry, I have been interrupted and cannot resume that game..."))
  (lm-display-statistics)
  ;;(ding)
  (setq lm-game-in-progress nil))

(defun lm-crash-game ()
  "What to do when Emacs detects it has been interrupted."
  (setq lm-emacs-is-computing nil)
  (lm-terminate-game 'crash-game)
  (sit-for 4)				; Let's see the message
  (lm-prompt-for-other-game))


;;;_ +  INTERACTIVE COMMANDS.

(defun lm-emacs-plays ()
  "Compute Emacs next move and play it."
  (interactive)
  (lm-switch-to-window)
  (cond
   (lm-emacs-is-computing
    (lm-crash-game))
   ((not lm-game-in-progress)
    (lm-prompt-for-other-game))
   (t
    (message "Let me think...")
    (let (square score)
      (setq square (lm-strongest-square))
      (cond ((null square)
	     (lm-terminate-game 'nobody-won))
	    (t
	     (setq score (aref lm-score-table square))
	     (lm-play-move square 6)
	     (cond ((>= score lm-winning-threshold)
		    (setq lm-emacs-won t) ; for font-lock
		    (lm-find-filled-qtuple square 6)
		    (lm-terminate-game 'emacs-won))
		   ((zerop score)
		    (lm-terminate-game 'nobody-won))
		   ((and (> lm-number-of-moves lm-draw-limit)
			 (not lm-human-refused-draw)
			 (lm-offer-a-draw))
		    (lm-terminate-game 'draw-agreed))
		   (t
		    (lm-prompt-for-move)))))))))

;; For small square dimensions this is approximate, since though measured in
;; pixels, event's (X . Y) is a character's top-left corner.
(defun lm-click (click)
  "Position at the square where you click."
  (interactive "e")
  (and (windowp (posn-window (setq click (event-end click))))
       (numberp (posn-point click))
       (select-window (posn-window click))
       (setq click (posn-col-row click))
       (lm-goto-xy
	(min (max (/ (+ (- (car click)
			   lm-x-offset
			   1)
			(window-hscroll)
			lm-square-width
			(% lm-square-width 2)
			(/ lm-square-width 2))
		     lm-square-width)
		  1)
	     lm-board-width)
	(min (max (/ (+ (- (cdr click)
			   lm-y-offset
			   1)
			(let ((inhibit-point-motion-hooks t))
			  (count-lines 1 (window-start)))
			lm-square-height
			(% lm-square-height 2)
			(/ lm-square-height 2))
		     lm-square-height)
		  1)
	     lm-board-height))))

(defun lm-mouse-play (click)
  "Play at the square where you click."
  (interactive "e")
  (if (lm-click click)
      (lm-human-plays)))

(defun lm-human-plays ()
  "Signal to the Lm program that you have played.
You must have put the cursor on the square where you want to play.
If the game is finished, this command requests for another game."
  (interactive)
  (lm-switch-to-window)
  (cond
   (lm-emacs-is-computing
    (lm-crash-game))
   ((not lm-game-in-progress)
    (lm-prompt-for-other-game))
   (t
    (let (square score)
      (setq square (lm-point-square))
      (cond ((null square)
	     (error "Your point is not on a square. Retry !"))
	    ((not (zerop (aref lm-board square)))
	     (error "Your point is not on a free square. Retry !"))
	    (t
	     (setq score (aref lm-score-table square))
	     (lm-play-move square 1)
	     (cond ((and (>= score lm-loosing-threshold)
			 ;; Just testing SCORE > THRESHOLD is not enough for
			 ;; detecting wins, it just gives an indication that
			 ;; we confirm with LM-FIND-FILLED-QTUPLE.
			 (lm-find-filled-qtuple square 1))
		    (lm-terminate-game 'human-won))
		   (t
		    (lm-emacs-plays)))))))))

(defun lm-human-takes-back ()
  "Signal to the Lm program that you wish to take back your last move."
  (interactive)
  (lm-switch-to-window)
  (cond
   (lm-emacs-is-computing
    (lm-crash-game))
   ((not lm-game-in-progress)
    (message "Too late for taking back...")
    (sit-for 4)
    (lm-prompt-for-other-game))
   ((zerop lm-number-of-human-moves)
    (message "You have not played yet... Your move ?"))
   (t
    (message "One moment, please...")
    ;; It is possible for the user to let Emacs play several consecutive
    ;; moves, so that the best way to know when to stop taking back moves is
    ;; to count the number of human moves:
    (setq lm-human-took-back t)
    (let ((number lm-number-of-human-moves))
      (while (= number lm-number-of-human-moves)
	(lm-take-back)))
    (lm-prompt-for-move))))

(defun lm-human-resigns ()
  "Signal to the Lm program that you may want to resign."
  (interactive)
  (lm-switch-to-window)
  (cond
   (lm-emacs-is-computing
    (lm-crash-game))
   ((not lm-game-in-progress)
    (message "There is no game in progress"))
   ((y-or-n-p "You mean, you resign ")
    (lm-terminate-game 'human-resigned))
   ((y-or-n-p "You mean, we continue ")
    (lm-prompt-for-move))
   (t
    (lm-terminate-game 'human-resigned)))) ; OK. Accept it

;;;_ + PROMPTING THE HUMAN PLAYER.

(defun lm-prompt-for-move ()
  "Display a message asking for Human's move."
  (message (if (zerop lm-number-of-human-moves)
	       "Your move ? (move to a free square and hit X, RET ...)"
	       "Your move ?"))
  ;; This may seem silly, but if one omits the following line (or a similar
  ;; one), the cursor may very well go to some place where POINT is not.
  (save-excursion (set-buffer (other-buffer))))

(defun lm-prompt-for-other-game ()
  "Ask for another game, and start it."
  (if (y-or-n-p "Another game ")
      (if (y-or-n-p "Retain learned weights ")
	  (lm 2)
	(lm 1))
    (message "Chicken !")))

(defun lm-offer-a-draw ()
  "Offer a draw and return t if Human accepted it."
  (or (y-or-n-p "I offer you a draw. Do you accept it ")
      (not (setq lm-human-refused-draw t))))


(defun lm-max-width ()
  "Largest possible board width for the current window."
  (1+ (/ (- (window-width (selected-window))
	    lm-x-offset lm-x-offset 1)
	 lm-square-width)))

(defun lm-max-height ()
  "Largest possible board height for the current window."
  (1+ (/ (- (window-height (selected-window))
	    lm-y-offset lm-y-offset 2)
	 ;; 2 instead of 1 because WINDOW-HEIGHT includes the mode line !
	 lm-square-height)))

(defun lm-point-y ()
  "Return the board row where point is."
  (let ((inhibit-point-motion-hooks t))
    (1+ (/ (- (count-lines 1 (point)) lm-y-offset (if (bolp) 0 1))
	   lm-square-height))))

(defun lm-point-square ()
  "Return the index of the square point is on."
  (let ((inhibit-point-motion-hooks t))
    (lm-xy-to-index (1+ (/ (- (current-column) lm-x-offset)
			       lm-square-width))
			(lm-point-y))))

(defun lm-goto-square (index)
  "Move point to square number INDEX."
  (lm-goto-xy (lm-index-to-x index) (lm-index-to-y index)))

(defun lm-goto-xy (x y)
  "Move point to square at X, Y coords."
  (let ((inhibit-point-motion-hooks t))
    (goto-line (+ 1 lm-y-offset (* lm-square-height (1- y)))))
  (move-to-column (+ lm-x-offset (* lm-square-width (1- x)))))

(defun lm-plot-square (square value)
  "Draw 'X', 'O' or '.' on SQUARE depending on VALUE, leave point there."
  (or (= value 1)
      (lm-goto-square square))
  (let ((inhibit-read-only t)
	(inhibit-point-motion-hooks t))
    (insert-and-inherit (cond ((= value 1) ?.)
			      ((= value 2) ?N)
			      ((= value 3) ?S)
			      ((= value 4) ?E)
			      ((= value 5) ?W)
			      ((= value 6) ?^)))

    (and (zerop value)
	 (add-text-properties (1- (point)) (point)
			      '(mouse-face highlight
				help-echo "\
mouse-1: get robot moving, mouse-2: play on this square")))
    (delete-char 1)
    (backward-char 1))
  (sit-for 0))	; Display NOW

(defun lm-init-display (n m)
  "Display an N by M Lm board."
  (buffer-disable-undo (current-buffer))
  (let ((inhibit-read-only t)
	(point 1) opoint
	(intangible t)
	(i m) j x)
    ;; Try to minimize number of chars (because of text properties)
    (setq tab-width
	  (if (zerop (% lm-x-offset lm-square-width))
	      lm-square-width
	    (max (/ (+ (% lm-x-offset lm-square-width)
		       lm-square-width 1) 2) 2)))
    (erase-buffer)
    (newline lm-y-offset)
    (while (progn
	     (setq j n
		   x (- lm-x-offset lm-square-width))
	     (while (>= (setq j (1- j)) 0)
	       (insert-char ?\t (/ (- (setq x (+ x lm-square-width))
				      (current-column))
				   tab-width))
	       (insert-char ?  (- x (current-column)))
	       (if (setq intangible (not intangible))
		   (put-text-property point (point) 'intangible 2))
	       (and (zerop j)
		    (= i (- m 2))
		    (progn
		      (while (>= i 3)
			(append-to-buffer (current-buffer) opoint (point))
			(setq i (- i 2)))
		      (goto-char (point-max))))
	       (setq point (point))
	       (insert ?=)
	       (add-text-properties point (point)
				    '(mouse-face highlight help-echo "\
mouse-1: get robot moving, mouse-2: play on this square")))
	     (> (setq i (1- i)) 0))
      (if (= i (1- m))
	  (setq opoint point))
      (insert-char ?\n lm-square-height))
    (or (eq (char-after 1) ?.)
	(put-text-property 1 2 'point-entered
			   (lambda (x y) (if (bobp) (forward-char)))))
    (or intangible
	(put-text-property point (point) 'intangible 2))
    (put-text-property point (point) 'point-entered
		       (lambda (x y) (if (eobp) (backward-char))))
    (put-text-property (point-min) (point) 'category 'lm-mode))
  (lm-goto-xy (/ (1+ n) 2) (/ (1+ m) 2)) ; center of the board
  (sit-for 0))				; Display NOW

(defun lm-display-statistics ()
  "Obnoxiously display some statistics about previous games in mode line."
  ;; We store this string in the mode-line-process local variable.
  ;; This is certainly not the cleanest way out ...
  (setq mode-line-process
	(format ": Trials: %d, Avg#Moves: %d"
		lm-number-of-trials
		(if (zerop lm-number-of-trials)
		    0
		  (/ lm-sum-of-moves lm-number-of-trials))))
  (force-mode-line-update))

(defun lm-switch-to-window ()
  "Find or create the Lm buffer, and display it."
  (interactive)
  (let ((buff (get-buffer "*Lm*")))
    (if buff				; Buffer exists:
	(switch-to-buffer buff)		;   no problem.
      (if lm-game-in-progress
	  (lm-crash-game))		;   buffer has been killed or something
      (switch-to-buffer "*Lm*")	; Anyway, start anew.
      (lm-mode))))


;;;_ + CROSSING WINNING QTUPLES.

;; When someone succeeds in filling a qtuple, we draw a line over the five
;; corresponding squares. One problem is that the program does not know which
;; squares ! It only knows the square where the last move has been played and
;; who won. The solution is to scan the board along all four directions.

(defun lm-find-filled-qtuple (square value)
  "Return t if SQUARE belongs to a qtuple filled with VALUEs."
  (or (lm-check-filled-qtuple square value 1 0)
      (lm-check-filled-qtuple square value 0 1)
      (lm-check-filled-qtuple square value 1 1)
      (lm-check-filled-qtuple square value -1 1)))

(defun lm-check-filled-qtuple (square value dx dy)
  "Return t if SQUARE belongs to a qtuple filled with VALUEs along DX, DY."
  (let ((a 0) (b 0)
	(left square) (right square)
	(depl (lm-xy-to-index dx dy)))
    (while (and (> a -4)		; stretch tuple left
		(= value (aref lm-board (setq left (- left depl)))))
      (setq a (1- a)))
    (while (and (< b (+ a 4))		; stretch tuple right
		(= value (aref lm-board (setq right (+ right depl)))))
      (setq b (1+ b)))
    (cond ((= b (+ a 4))		; tuple length = 5 ?
	   (lm-cross-qtuple (+ square (* a depl)) (+ square (* b depl))
				dx dy)
	   t))))

(defun lm-cross-qtuple (square1 square2 dx dy)
  "Cross every square between SQUARE1 and SQUARE2 in the DX, DY direction."
  (save-excursion			; Not moving point from last square
    (let ((depl (lm-xy-to-index dx dy))
	  (inhibit-read-only t)
	  (inhibit-point-motion-hooks t))
      ;; WARNING: this function assumes DEPL > 0 and SQUARE2 > SQUARE1
      (while (/= square1 square2)
	(lm-goto-square square1)
	(setq square1 (+ square1 depl))
	(cond
	  ((= dy 0)			; Horizontal
	   (forward-char 1)
	   (insert-char ?- (1- lm-square-width) t)
	   (delete-region (point) (progn
				    (skip-chars-forward " \t")
				    (point))))
	  ((= dx 0)			; Vertical
	   (let ((lm-n 1)
		 (column (current-column)))
	     (while (< lm-n lm-square-height)
	       (setq lm-n (1+ lm-n))
	       (forward-line 1)
	       (indent-to column)
	       (insert-and-inherit ?|))))
	  ((= dx -1)			; 1st Diagonal
	   (indent-to (prog1 (- (current-column) (/ lm-square-width 2))
			(forward-line (/ lm-square-height 2))))
	   (insert-and-inherit ?/))
	  (t				; 2nd Diagonal
	   (indent-to (prog1 (+ (current-column) (/ lm-square-width 2))
			(forward-line (/ lm-square-height 2))))
	   (insert-and-inherit ?\\))))))
  (sit-for 0))				; Display NOW


;;;_ + CURSOR MOTION.

;; previous-line and next-line don't work right with intangible newlines
(defun lm-move-down ()
  "Move point down one row on the Lm board."
  (interactive)
  (if (< (lm-point-y) lm-board-height)
      (next-line 1)));;; lm-square-height)))

(defun lm-move-up ()
  "Move point up one row on the Lm board."
  (interactive)
  (if (> (lm-point-y) 1)
      (previous-line lm-square-height)))

(defun lm-move-ne ()
  "Move point North East on the Lm board."
  (interactive)
  (lm-move-up)
  (forward-char))

(defun lm-move-se ()
  "Move point South East on the Lm board."
  (interactive)
  (lm-move-down)
  (forward-char))

(defun lm-move-nw ()
  "Move point North West on the Lm board."
  (interactive)
  (lm-move-up)
  (backward-char))

(defun lm-move-sw ()
  "Move point South West on the Lm board."
  (interactive)
  (lm-move-down)
  (backward-char))

(defun lm-beginning-of-line ()
  "Move point to first square on the Lm board row."
  (interactive)
  (move-to-column lm-x-offset))

(defun lm-end-of-line ()
  "Move point to last square on the Lm board row."
  (interactive)
  (move-to-column (+ lm-x-offset
		     (* lm-square-width (1- lm-board-width)))))


;;;_ + Simulation variables

;;;_  - lm-nvar
(defvar lm-nvar 0.0075
  "Not used.
Affects a noise generator which was used in an earlier incarnation of
this program to add a random element to the way moves were made.")
;;;_  - lists of cardinal directions
;;;_   :
(defvar lm-ns '(lm-n lm-s)
  "Used when doing something relative to the north and south axes.")
(defvar lm-ew '(lm-e lm-w)
  "Used when doing something relative to the east and west axes.")
(defvar lm-directions '(lm-n lm-s lm-e lm-w)
  "The cardinal directions.")
(defvar lm-8-directions
  '((lm-n) (lm-n lm-w) (lm-w) (lm-s lm-w)
    (lm-s) (lm-s lm-e) (lm-e) (lm-n lm-e))
  "The full 8 possible directions.")

(defvar lm-number-of-moves
  "The number of moves made by the robot so far.")


;;;_* Terry's mods to create lm.el

;;;(setq lm-debug nil)
(defvar lm-debug nil
  "If non-nil, debugging is printed.")
(defcustom lm-one-moment-please nil
  "If non-nil, print \"One moment please\" when a new board is generated.
The drawback of this is you don't see how many moves the last run took
because it is overwritten by \"One moment please\"."
  :type 'boolean
  :group 'lm)
(defcustom lm-output-moves t
  "If non-nil, output number of moves so far on a move-by-move basis."
  :type 'boolean
  :group 'lm)


(defun lm-weights-debug ()
  (if lm-debug
      (progn (lm-print-wts) (lm-blackbox) (lm-print-y,s,noise)
	     (lm-print-smell))))

;;;_  - Printing various things
(defun lm-print-distance-int (direction)
  (interactive)
  (insert (format "%S %S " direction  (get direction 'distance))))


(defun lm-print-distance ()
  (insert (format "tree: %S \n" (calc-distance-of-robot-from 'lm-tree)))
  (mapc 'lm-print-distance-int lm-directions))


;;(setq direction 'lm-n)
;;(get 'lm-n 'lm-s)
(defun lm-nslify-wts-int (direction)
  (mapcar (lambda (target-direction)
	     (get direction target-direction))
	  lm-directions))


(defun lm-nslify-wts ()
  (interactive)
  (let ((l (apply 'append (mapcar 'lm-nslify-wts-int lm-directions))))
    (insert (format "set data_value WTS \n %s \n" l))
    (insert (format "/* max: %S min: %S */"
		  (eval (cons 'max l)) (eval (cons 'min l))))))

(defun lm-print-wts-int (direction)
  (mapc (lambda (target-direction)
	     (insert (format "%S %S %S "
			      direction
			      target-direction
			     (get direction target-direction))))
	  lm-directions)
  (insert "\n"))

(defun lm-print-wts ()
  (interactive)
  (save-excursion
    (set-buffer "*lm-wts*")
    (insert "==============================\n")
    (mapc 'lm-print-wts-int lm-directions)))

(defun lm-print-moves (moves)
  (interactive)
  (save-excursion
    (set-buffer "*lm-moves*")
    (insert (format "%S\n" moves))))


(defun lm-print-y,s,noise-int (direction)
  (insert (format "%S:lm-y %S, s %S, noise %S \n"
		    (symbol-name direction)
		    (get direction 'y_t)
		    (get direction 's)
		    (get direction 'noise)
		    )))

(defun lm-print-y,s,noise ()
  (interactive)
  (save-excursion
    (set-buffer "*lm-y,s,noise*")
    (insert "==============================\n")
    (mapc 'lm-print-y,s,noise-int lm-directions)))

(defun lm-print-smell-int (direction)
  (insert (format "%S: smell: %S \n"
		    (symbol-name direction)
		    (get direction 'smell))))

(defun lm-print-smell ()
  (interactive)
  (save-excursion
    (set-buffer "*lm-smell*")
    (insert "==============================\n")
    (insert (format "tree: %S \n" (get 'z 't)))
    (mapc 'lm-print-smell-int lm-directions)))

(defun lm-print-w0-int (direction)
  (insert (format "%S: w0: %S \n"
		    (symbol-name direction)
		    (get direction 'w0))))

(defun lm-print-w0 ()
  (interactive)
  (save-excursion
    (set-buffer "*lm-w0*")
    (insert "==============================\n")
    (mapc 'lm-print-w0-int lm-directions)))

(defun lm-blackbox ()
  (save-excursion
    (set-buffer "*lm-blackbox*")
    (insert "==============================\n")
    (insert "I smell: ")
    (mapc (lambda (direction)
	       (if (> (get direction 'smell) 0)
		   (insert (format "%S " direction))))
	    lm-directions)
    (insert "\n")

    (insert "I move: ")
    (mapc (lambda (direction)
	       (if (> (get direction 'y_t) 0)
		   (insert (format "%S " direction))))
	    lm-directions)
    (insert "\n")
    (lm-print-wts-blackbox)
    (insert (format "z_t-z_t-1: %S" (- (get 'z 't) (get 'z 't-1))))
    (lm-print-distance)
    (insert "\n")))

(defun lm-print-wts-blackbox ()
  (interactive)
  (mapc 'lm-print-wts-int lm-directions))

;;;_  - learning parameters
(defcustom lm-bound 0.005
  "The maximum that w0j may be."
  :type 'number
  :group 'lm)
(defcustom lm-c 1.0
  "A factor applied to modulate the increase in wij.
Used in the function lm-update-normal-weights."
  :type 'number
  :group 'lm)
(defcustom lm-c-naught 0.5
  "A factor applied to modulate the increase in w0j.
Used in the function lm-update-naught-weights."
  :type 'number
  :group 'lm)
(defvar lm-initial-w0 0.0)
(defvar lm-initial-wij 0.0)
(defcustom lm-no-payoff 0
  "The amount of simulation cycles that have occurred with no movement.
Used to move the robot when he is stuck in a rut for some reason."
  :type 'integer
  :group 'lm)
(defcustom lm-max-stall-time 2
  "The maximum number of cycles that the robot can remain stuck in a place.
After this limit is reached, lm-random-move is called to push him out of it."
  :type 'integer
  :group 'lm)


;;;_ + Randomizing functions
;;;_  - lm-flip-a-coin ()
(defun lm-flip-a-coin ()
  (if (> (random 5000) 2500)
      -1
    1))
;;;_   : lm-very-small-random-number ()
;(defun lm-very-small-random-number ()
;  (/
;   (* (/ (random 900000) 900000.0) .0001)))
;;;_   : lm-randomize-weights-for (direction)
(defun lm-randomize-weights-for (direction)
  (mapc (lambda (target-direction)
	     (put direction
		  target-direction
		  (* (lm-flip-a-coin) (/  (random 10000) 10000.0))))
	  lm-directions))
;;;_   : lm-noise ()
(defun lm-noise ()
  (* (- (/ (random 30001) 15000.0) 1) lm-nvar))

;;;_   : lm-fix-weights-for (direction)
(defun lm-fix-weights-for (direction)
  (mapc (lambda (target-direction)
	     (put direction
		  target-direction
		  lm-initial-wij))
	  lm-directions))


;;;_ + Plotting functions
;;;_  - lm-plot-internal (sym)
(defun lm-plot-internal (sym)
  (lm-plot-square (lm-xy-to-index
		   (get sym 'x)
		   (get sym 'y))
		   (get sym 'sym)))
;;;_  - lm-plot-landmarks ()
(defun lm-plot-landmarks ()
  (setq lm-cx (/ lm-board-width  2))
  (setq lm-cy (/ lm-board-height 2))

  (put 'lm-n    'x lm-cx)
  (put 'lm-n    'y 1)
  (put 'lm-n    'sym 2)

  (put 'lm-tree 'x lm-cx)
  (put 'lm-tree 'y lm-cy)
  (put 'lm-tree 'sym 6)

  (put 'lm-s    'x lm-cx)
  (put 'lm-s    'y lm-board-height)
  (put 'lm-s    'sym 3)

  (put 'lm-w    'x 1)
  (put 'lm-w    'y (/ lm-board-height 2))
  (put 'lm-w    'sym 5)

  (put 'lm-e    'x lm-board-width)
  (put 'lm-e    'y (/ lm-board-height 2))
  (put 'lm-e    'sym 4)

  (mapc 'lm-plot-internal '(lm-n lm-s lm-e lm-w lm-tree)))



;;;_ + Distance-calculation functions
;;;_  - square (a)
(defun square (a)
  (* a a))

;;;_  - distance (x x0 y y0)
(defun distance (x x0 y y0)
  (sqrt (+ (square (- x x0)) (square (- y y0)))))

;;;_  - calc-distance-of-robot-from (direction)
(defun calc-distance-of-robot-from (direction)
  (put direction 'distance
       (distance (get direction 'x)
		 (lm-index-to-x (lm-point-square))
		 (get direction 'y)
		 (lm-index-to-y (lm-point-square)))))

;;;_  - calc-smell-internal (sym)
(defun calc-smell-internal (sym)
  (let ((r (get sym 'r))
	(d (calc-distance-of-robot-from sym)))
    (if (> (* 0.5 (- 1 (/ d r))) 0)
	(* 0.5 (- 1 (/ d r)))
      0)))


;;;_ + Learning (neural) functions
(defun lm-f (x)
  (cond
   ((> x lm-bound) lm-bound)
   ((< x 0.0) 0.0)
   (t x)))

(defun lm-y (direction)
  (let ((noise (put direction 'noise (lm-noise))))
    (put direction 'y_t
	 (if (> (get direction 's) 0.0)
	     1.0
	   0.0))))

(defun lm-update-normal-weights (direction)
  (mapc (lambda (target-direction)
	     (put direction target-direction
		  (+
		   (get direction target-direction)
		   (* lm-c
		      (- (get 'z 't) (get 'z 't-1))
		      (get target-direction 'y_t)
		      (get direction 'smell)))))
	  lm-directions))

(defun lm-update-naught-weights (direction)
  (mapc (lambda (target-direction)
	     (put direction 'w0
		  (lm-f
		   (+
		    (get direction 'w0)
		    (* lm-c-naught
		       (- (get 'z 't) (get 'z 't-1))
		       (get direction 'y_t))))))
	  lm-directions))


;;;_ + Statistics gathering and creating functions

(defun lm-calc-current-smells ()
  (mapc (lambda (direction)
	     (put direction 'smell (calc-smell-internal direction)))
	  lm-directions))

(defun lm-calc-payoff ()
  (put 'z 't-1 (get 'z 't))
  (put 'z 't (calc-smell-internal 'lm-tree))
  (if (= (- (get 'z 't) (get 'z 't-1)) 0.0)
      (incf lm-no-payoff)
    (setf lm-no-payoff 0)))

(defun lm-store-old-y_t ()
  (mapc (lambda (direction)
	     (put direction 'y_t-1 (get direction 'y_t)))
	  lm-directions))


;;;_ + Functions to move robot

(defun lm-confidence-for (target-direction)
  (apply '+
	 (get target-direction 'w0)
	 (mapcar (lambda (direction)
		   (*
		    (get direction target-direction)
		    (get direction 'smell)))
		 lm-directions)))


(defun lm-calc-confidences ()
  (mapc (lambda (direction)
	     (put direction 's (lm-confidence-for direction)))
	     lm-directions))

(defun lm-move ()
  (if (and (= (get 'lm-n 'y_t) 1.0) (= (get 'lm-s 'y_t) 1.0))
      (progn
	(mapc (lambda (dir) (put dir 'y_t 0)) lm-ns)
	(if lm-debug
	    (message "n-s normalization."))))
  (if (and (= (get 'lm-w 'y_t) 1.0) (= (get 'lm-e 'y_t) 1.0))
      (progn
	(mapc (lambda (dir) (put dir 'y_t 0)) lm-ew)
	(if lm-debug
	    (message "e-w normalization"))))

  (mapc (lambda (pair)
	     (if (> (get (car pair) 'y_t) 0)
		 (funcall (car (cdr pair)))))
	  '(
	    (lm-n lm-move-up)
	    (lm-s lm-move-down)
	    (lm-e forward-char)
	    (lm-w backward-char)))
  (lm-plot-square (lm-point-square) 1)
  (incf lm-number-of-moves)
  (if lm-output-moves
      (message (format "Moves made: %d" lm-number-of-moves))))


(defun lm-random-move ()
  (mapc
   (lambda (direction) (put direction 'y_t 0))
   lm-directions)
  (dolist (direction (nth (random 8) lm-8-directions))
    (put direction 'y_t 1.0))
  (lm-move))

(defun lm-amble-robot ()
  (interactive)
  (while (> (calc-distance-of-robot-from 'lm-tree) 0)

    (lm-store-old-y_t)
    (lm-calc-current-smells)

    (if (> lm-no-payoff lm-max-stall-time)
	(lm-random-move)
      (progn
	(lm-calc-confidences)
	(mapc 'lm-y lm-directions)
	(lm-move)))

    (lm-calc-payoff)

    (mapc 'lm-update-normal-weights lm-directions)
    (mapc 'lm-update-naught-weights lm-directions)
    (if lm-debug
	(lm-weights-debug)))
  (lm-terminate-game nil))


;;;_  - lm-start-robot ()
(defun lm-start-robot ()
  "Signal to the Lm program that you have played.
You must have put the cursor on the square where you want to play.
If the game is finished, this command requests for another game."
  (interactive)
  (lm-switch-to-window)
  (cond
   (lm-emacs-is-computing
    (lm-crash-game))
   ((not lm-game-in-progress)
    (lm-prompt-for-other-game))
   (t
    (let (square score)
      (setq square (lm-point-square))
      (cond ((null square)
	     (error "Your point is not on a square. Retry !"))
	    ((not (zerop (aref lm-board square)))
	     (error "Your point is not on a free square. Retry !"))
	    (t
	     (progn
	       (lm-plot-square square 1)

	       (lm-store-old-y_t)
	       (lm-calc-current-smells)
	       (put 'z 't (calc-smell-internal 'lm-tree))

	       (lm-random-move)

	       (lm-calc-payoff)

	       (mapc 'lm-update-normal-weights lm-directions)
	       (mapc 'lm-update-naught-weights lm-directions)
	       (lm-amble-robot)
	       )))))))


;;;_ + Misc functions
;;;_  - lm-init (auto-start save-weights)
(defvar lm-tree-r "")

(defun lm-init (auto-start save-weights)

  (setq lm-number-of-moves 0)

  (lm-plot-landmarks)

  (if lm-debug
      (progn
	(save-excursion
	  (set-buffer (get-buffer-create "*lm-w0*"))
    (erase-buffer)
    (set-buffer (get-buffer-create "*lm-moves*"))
    (set-buffer (get-buffer-create "*lm-wts*"))
    (erase-buffer)
    (set-buffer (get-buffer-create "*lm-y,s,noise*"))
    (erase-buffer)
    (set-buffer (get-buffer-create "*lm-smell*"))
    (erase-buffer)
    (set-buffer (get-buffer-create "*lm-blackbox*"))
    (erase-buffer)
    (set-buffer (get-buffer-create "*lm-distance*"))
    (erase-buffer))))


  (lm-set-landmark-signal-strengths)

  (mapc (lambda (direction)
	     (put direction 'y_t 0.0))
	  lm-directions)

  (if (not save-weights)
      (progn
	(mapc 'lm-fix-weights-for lm-directions)
	(mapc (lambda (direction)
		   (put direction 'w0 lm-initial-w0))
	lm-directions))
    (message "Weights preserved for this run."))

  (if auto-start
      (progn
	(lm-goto-xy (1+ (random lm-board-width)) (1+ (random lm-board-height)))
	(lm-start-robot))))


;;;_  - something which doesn't work
; no-a-worka!!
;(defum lm-sum-list (list)
;  (if (> (length list) 0)
;      (+ (car list) (lm-sum-list (cdr list)))
;    0))
; this a worka!
; (eval  (cons '+ list))
;;;_  - lm-set-landmark-signal-strengths ()
;;; on a screen higher than wide, I noticed that the robot would amble
;;; left and right and not move forward. examining *lm-blackbox*
;;; revealed that there was no scent from the north and south
;;; landmarks, hence, they need less factoring down of the effect of
;;; distance on scent.

(defun lm-set-landmark-signal-strengths ()

  (setq lm-tree-r       (* (sqrt (+ (square lm-cx) (square lm-cy))) 1.5))

  (mapc (lambda (direction)
	     (put direction 'r (* lm-cx 1.1)))
	lm-ew)
  (mapc (lambda (direction)
	     (put direction 'r (* lm-cy 1.1)))
	lm-ns)
  (put 'lm-tree 'r lm-tree-r))


;;;_ + lm-test-run ()

;;;###autoload
(defalias 'landmark-repeat 'lm-test-run)
;;;###autoload
(defun lm-test-run ()
  "Run 100 Lm games, each time saving the weights from the previous game."
  (interactive)

  (lm 1)

  (dotimes (scratch-var 100)

    (lm 2)))


;;;_ + lm: The function you invoke to play

;;;###autoload
(defalias 'landmark 'lm)
;;;###autoload
(defun lm (parg)
  "Start or resume an Lm game.
If a game is in progress, this command allows you to resume it.
Here is the relation between prefix args and game options:

prefix arg | robot is auto-started | weights are saved from last game
---------------------------------------------------------------------
none / 1   | yes                   | no
       2   | yes                   | yes
       3   | no                    | yes
       4   | no                    | no

You start by moving to a square and typing \\[lm-start-robot],
if you did not use a prefix arg to ask for automatic start.
Use \\[describe-mode] for more info."
  (interactive "p")

  (setf lm-n nil lm-m nil)
  (lm-switch-to-window)
  (cond
   (lm-emacs-is-computing
    (lm-crash-game))
   ((or (not lm-game-in-progress)
	(<= lm-number-of-moves 2))
    (let ((max-width (lm-max-width))
	  (max-height (lm-max-height)))
      (or lm-n (setq lm-n max-width))
      (or lm-m (setq lm-m max-height))
      (cond ((< lm-n 1)
	     (error "I need at least 1 column"))
	    ((< lm-m 1)
	     (error "I need at least 1 row"))
	    ((> lm-n max-width)
	     (error "I cannot display %d columns in that window" lm-n)))
      (if (and (> lm-m max-height)
	       (not (eq lm-m lm-saved-board-height))
	       ;; Use EQ because SAVED-BOARD-HEIGHT may be nil
	       (not (y-or-n-p (format "Do you really want %d rows " lm-m))))
	  (setq lm-m max-height)))
    (if lm-one-moment-please
	(message "One moment, please..."))
    (lm-start-game lm-n lm-m)
    (eval (cons 'lm-init
		(cond
		 ((= parg 1)  '(t nil))
		 ((= parg 2)  '(t t))
		 ((= parg 3)  '(nil t))
		 ((= parg 4)  '(nil nil))
		 (t '(nil t))))))))


;;;_ + Local variables

;;; The following `outline-layout' local variable setting:
;;;  - closes all topics from the first topic to just before the third-to-last,
;;;  - shows the children of the third to last (config vars)
;;;  - and the second to last (code section),
;;;  - and closes the last topic (this local-variables section).
;;;Local variables:
;;;outline-layout: (0 : -1 -1 0)
;;;End:

(provide 'landmark)

;;; arch-tag: ae5031be-96e6-459e-a3df-1df53117d3f2
;;; landmark.el ends here
