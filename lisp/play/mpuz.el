;;; mpuz.el --- multiplication puzzle for GNU Emacs

;;; Copyright (C) 1990 Free Software Foundation, Inc.

;; Author: Philippe Schnoebelen <phs@lifia.imag.fr>
;; Keywords: games

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:

;; When this package is loaded, `M-x mpuz' generates a random multiplication
;; puzzle.  This is a multiplication example in which each digit has been
;; consistently replaced with some letter.  Your job is to reconstruct
;; the original digits.  Type `?' while the mode is active for detailed help.

;;; Code:

(random t)				; randomize

(defvar mpuz-silent nil
  "*Set this to T if you don't want dings on inputs.")

(defun mpuz-ding ()
  "Dings, unless global variable `mpuz-silent' forbids it."
  (or mpuz-silent (ding t)))


;; Mpuz mode and keymaps
;;----------------------
(defvar mpuz-mode-hook nil)

(defvar mpuz-mode-map nil
  "Local keymap to use in Mult Puzzle.")

(if mpuz-mode-map nil
    (setq mpuz-mode-map (make-sparse-keymap))
    (define-key mpuz-mode-map "a" 'mpuz-try-letter)
    (define-key mpuz-mode-map "b" 'mpuz-try-letter)
    (define-key mpuz-mode-map "c" 'mpuz-try-letter)
    (define-key mpuz-mode-map "d" 'mpuz-try-letter)
    (define-key mpuz-mode-map "e" 'mpuz-try-letter)
    (define-key mpuz-mode-map "f" 'mpuz-try-letter)
    (define-key mpuz-mode-map "g" 'mpuz-try-letter)
    (define-key mpuz-mode-map "h" 'mpuz-try-letter)
    (define-key mpuz-mode-map "i" 'mpuz-try-letter)
    (define-key mpuz-mode-map "j" 'mpuz-try-letter)
    (define-key mpuz-mode-map "A" 'mpuz-try-letter)
    (define-key mpuz-mode-map "B" 'mpuz-try-letter)
    (define-key mpuz-mode-map "C" 'mpuz-try-letter)
    (define-key mpuz-mode-map "D" 'mpuz-try-letter)
    (define-key mpuz-mode-map "E" 'mpuz-try-letter)
    (define-key mpuz-mode-map "F" 'mpuz-try-letter)
    (define-key mpuz-mode-map "G" 'mpuz-try-letter)
    (define-key mpuz-mode-map "H" 'mpuz-try-letter)
    (define-key mpuz-mode-map "I" 'mpuz-try-letter)
    (define-key mpuz-mode-map "J" 'mpuz-try-letter)
    (define-key mpuz-mode-map "\C-g" 'mpuz-offer-abort)
    (define-key mpuz-mode-map "?" 'describe-mode))

(defun mpuz-mode ()
  "Multiplication puzzle mode.

You have to guess which letters stand for which digits in the
multiplication displayed inside the `*Mult Puzzle*' buffer.

You may enter a guess for a letter's value by typing first the letter,
then the digit.  Thus, to guess that A=3, type A 3.

To leave the game to do other editing work, just switch buffers.
Then you may resume the game with M-x mpuz.
You may abort a game by typing \\<mpuz-mode-map>\\[mpuz-offer-abort]."
  (interactive)
  (setq major-mode 'mpuz-mode
	mode-name  "Mult Puzzle")
  (use-local-map mpuz-mode-map)
  (run-hooks 'mpuz-mode-hook))


;; Some variables for statistics
;;------------------------------
(defvar mpuz-nb-errors 0
  "Number of errors made in current game.")

(defvar mpuz-nb-completed-games 0
  "Number of games completed.")

(defvar mpuz-nb-cumulated-errors 0
  "Number of errors made in previous games.")


;; Some variables for game tracking
;;---------------------------------
(defvar mpuz-in-progress nil
  "True if a game is currently in progress.")

(defvar mpuz-found-digits (make-vector 10 nil)
  "A vector recording which digits have been decrypted.")

(defmacro mpuz-digit-solved-p (digit)
  (list 'aref 'mpuz-found-digits digit))


;; A puzzle uses a permutation of [0..9] into itself.
;; We use both the permutation and its inverse.
;;---------------------------------------------------
(defvar mpuz-digit-to-letter (make-vector 10 0)
  "A permutation from [0..9] to [0..9].")

(defvar mpuz-letter-to-digit (make-vector 10 0)
  "The inverse of mpuz-digit-to-letter.")

(defmacro mpuz-to-digit (letter)
  (list 'aref 'mpuz-letter-to-digit letter))

(defmacro mpuz-to-letter (digit)
  (list 'aref 'mpuz-digit-to-letter digit))

(defun mpuz-build-random-perm ()
  "Initialize puzzle coding with a random permutation."
  (let ((letters (list 0 1 2 3 4 5 6 7 8 9)) ; new cons cells, because of delq
	(index 10)
	elem)
    (while letters
      (setq elem    (nth (random index) letters)
	    letters (delq elem letters)
	    index   (1- index))
      (aset mpuz-digit-to-letter index elem)
      (aset mpuz-letter-to-digit elem index))))


;; A puzzle also uses a board displaying a multiplication.
;; Every digit appears in the board, crypted or not.
;;------------------------------------------------------
(defvar mpuz-board (make-vector 10 nil)
  "The board associates to any digit the list of squares where it appears.")

(defun mpuz-put-digit-on-board (number square)
  "Put (last digit of) NUMBER on SQUARE of the puzzle board."
  ;; i.e. push SQUARE on NUMBER square-list
  (setq number (% number 10))
  (aset mpuz-board number (cons square (aref mpuz-board number))))

(defun mpuz-check-all-solved ()
  "Check whether all digits have been solved. Return t if yes."
  (catch 'found
    (let ((digit -1))
      (while (> 10 (setq digit (1+ digit)))
	(if (and (not (mpuz-digit-solved-p digit)) ; unsolved
		 (aref mpuz-board digit)) ; and appearing in the puzzle !
	    (throw 'found nil))))
    t))


;; To build a puzzle, we take two random numbers and multiply them.
;; We also take a random permutation for encryption.
;; The random numbers are only use to see which digit appears in which square
;; of the board. Everything is stored in individual squares.
;;---------------------------------------------------------------------------
(defun mpuz-random-puzzle ()
  "Draw random values to be multiplied in a puzzle."
  (mpuz-build-random-perm)
  (fillarray mpuz-board nil)		; erase the board
  (let (A B C D E)
    ;; A,B,C,D & E, are the five rows of our multiplication.
    ;; Choose random values, discarding uninteresting cases.
    (while (progn
	     (setq A (random 1000)
		   B (random 100)
		   C (* A (% B 10))
		   D (* A (/ B 10))
		   E (* A B))
	     (or (< C 1000) (< D 1000)))) ; forbid leading zeros in C or D
    ;; Individual digits are now put on their respectives squares.
    ;; [NB: A square is a pair <row,column> of the screen.]
    (mpuz-put-digit-on-board A		 '(2 . 9))
    (mpuz-put-digit-on-board (/ A 10)	 '(2 . 7))
    (mpuz-put-digit-on-board (/ A 100)	 '(2 . 5))
    (mpuz-put-digit-on-board B		 '(4 . 9))
    (mpuz-put-digit-on-board (/ B 10)	 '(4 . 7))
    (mpuz-put-digit-on-board C		 '(6 . 9))
    (mpuz-put-digit-on-board (/ C 10)	 '(6 . 7))
    (mpuz-put-digit-on-board (/ C 100)	 '(6 . 5))
    (mpuz-put-digit-on-board (/ C 1000)	 '(6 . 3))
    (mpuz-put-digit-on-board D		 '(8 . 7))
    (mpuz-put-digit-on-board (/ D 10)	 '(8 . 5))
    (mpuz-put-digit-on-board (/ D 100)	 '(8 . 3))
    (mpuz-put-digit-on-board (/ D 1000)	 '(8 . 1))
    (mpuz-put-digit-on-board E		 '(10 . 9))
    (mpuz-put-digit-on-board (/ E 10)	 '(10 . 7))
    (mpuz-put-digit-on-board (/ E 100)	 '(10 . 5))
    (mpuz-put-digit-on-board (/ E 1000)	 '(10 . 3))
    (mpuz-put-digit-on-board (/ E 10000) '(10 . 1))))

;; Display
;;--------
(defconst mpuz-framework
  "
     . . .
                   Number of errors (this game): 0
    x  . .
   -------
   . . . .
                        Number of completed games: 0
 . . . .
 ---------              Average number of errors: 0.00
 . . . . ."
  "The general picture of the puzzle screen, as a string.")

(defun mpuz-create-buffer ()
  "Create (or recreate) the puzzle buffer. Return it."
  (let ((buff (get-buffer-create "*Mult Puzzle*")))
    (save-excursion
      (set-buffer buff)
      (let ((buffer-read-only nil))
	(erase-buffer)
	(insert mpuz-framework)
	(mpuz-paint-board)
	(mpuz-paint-errors)
	(mpuz-paint-statistics)))
    buff))

(defun mpuz-paint-errors ()
  "Paint error count on the puzzle screen."
  (mpuz-switch-to-window)
  (let ((buffer-read-only nil))
    (goto-line 3)
    (move-to-column 49)
    (mpuz-delete-line)
    (insert (prin1-to-string mpuz-nb-errors))))

(defun mpuz-paint-statistics ()
  "Paint statistics about previous games on the puzzle screen."
  (let* ((mean (if (zerop mpuz-nb-completed-games) 0
		   (/ (+ mpuz-nb-completed-games (* 200 mpuz-nb-cumulated-errors))
		      (* 2 mpuz-nb-completed-games))))
	 (frac-part (% mean 100)))
    (let ((buffer-read-only nil))
      (goto-line 7)
      (move-to-column 51)
      (mpuz-delete-line)
      (insert (prin1-to-string mpuz-nb-completed-games))
      (goto-line 9)
      (move-to-column 50)
      (mpuz-delete-line)
      (insert (format "%d.%d%d" (/ mean 100) (/ frac-part 10) (% frac-part 10))))))

(defun mpuz-paint-board ()
  "Paint board situation on the puzzle screen."
  (mpuz-switch-to-window)
  (let ((letter -1))
    (while (> 10 (setq letter (1+ letter)))
      (mpuz-paint-digit (mpuz-to-digit letter))))
  (goto-char (point-min)))

(defun mpuz-paint-digit (digit)
  "Paint all occurrences of DIGIT on the puzzle board."
  ;; (mpuz-switch-to-window)
  (let ((char (if (mpuz-digit-solved-p digit)
		  (+ digit ?0)
		  (+ (mpuz-to-letter digit) ?A)))
	(square-l (aref mpuz-board digit)))
    (let ((buffer-read-only nil))
      (while square-l
	(goto-line (car (car square-l)))	; line before column !
	(move-to-column (cdr (car square-l)))
	(insert char)
	(delete-char 1)
	(backward-char 1)
	(setq square-l (cdr square-l))))))

(defun mpuz-delete-line ()
  "Clear from point to next newline."	; & put nothing in the kill ring
  (while (not (= ?\n (char-after (point))))
    (delete-char 1)))

(defun mpuz-get-buffer ()
  "Get the puzzle buffer if it exists."
  (get-buffer "*Mult Puzzle*"))

(defun mpuz-switch-to-window ()
  "Find or create the Mult-Puzzle buffer, and display it."
  (let ((buff (mpuz-get-buffer)))
    (or buff (setq buff (mpuz-create-buffer)))
    (switch-to-buffer buff)
    (or buffer-read-only (toggle-read-only))
    (mpuz-mode)))


;; Game control
;;-------------
(defun mpuz-abort-game ()
  "Abort any puzzle in progress."
  (message "Mult Puzzle aborted.")
  (setq mpuz-in-progress nil
	mpuz-nb-errors 0)
  (fillarray mpuz-board nil)
  (let ((buff (mpuz-get-buffer)))
    (if buff (kill-buffer buff))))

(defun mpuz-start-new-game ()
  "Start a new puzzle."
  (message "Here we go...")
  (setq mpuz-nb-errors 0
	mpuz-in-progress t)
  (fillarray mpuz-found-digits nil)	; initialize mpuz-found-digits
  (mpuz-random-puzzle)
  (mpuz-switch-to-window)
  (mpuz-paint-board)
  (mpuz-paint-errors)
  (mpuz-ask-for-try))

(defun mpuz-offer-new-game ()
  "Ask if user wants to start a new puzzle."
  (if (y-or-n-p "Start a new game ")
      (mpuz-start-new-game)
      (message "OK. I won't.")))

;;;###autoload
(defun mpuz ()
  "Multiplication puzzle with GNU Emacs."
  ;; Main entry point
  (interactive)
  (mpuz-switch-to-window)
  (if mpuz-in-progress
      (mpuz-offer-abort)
      (mpuz-start-new-game)))

(defun mpuz-offer-abort ()
  "Ask if user wants to abort current puzzle."
  (interactive)
  (if (y-or-n-p "Abort game ")
      (mpuz-abort-game)
      (mpuz-ask-for-try)))

(defun mpuz-ask-for-try ()
  "Ask for user proposal in puzzle."
  (message "Your try ?"))

(defun mpuz-try-letter ()
  "Propose a digit for a letter in puzzle."
  (interactive)
  (if mpuz-in-progress
      (let (letter-char digit digit-char message)
	(setq letter-char (upcase last-command-char)
	      digit (mpuz-to-digit (- letter-char ?A)))
	(cond ((mpuz-digit-solved-p digit)
	       (message "%c already solved." letter-char))
	      ((null (aref mpuz-board digit))
	       (message "%c does not appear." letter-char))
	      ((progn (message "%c = " letter-char)
		      ;; <char> has been entered.
		      ;; Print "<char> =" and
		      ;; read <num> or = <num>
		      (setq digit-char (read-char))
		      (if (eq digit-char ?=)
			  (setq digit-char (read-char)))
		      (message "%c = %c" letter-char digit-char)
		      (or (> digit-char ?9) (< digit-char ?0))) ; bad input
	       (ding t))
	      (t
	       (mpuz-try-proposal letter-char digit-char))))
      (mpuz-offer-new-game)))

(defun mpuz-try-proposal (letter-char digit-char)
  "Propose LETTER-CHAR as code for DIGIT-CHAR."
  (let* ((letter (- letter-char ?A))
	 (digit (- digit-char ?0))
	 (correct-digit (mpuz-to-digit letter)))
    (cond ((mpuz-digit-solved-p correct-digit)
	   (message "%c has already been found."))
	  ((= digit correct-digit)
	   (message "%c = %c correct !" letter-char digit-char)
	   (mpuz-ding)
	   (mpuz-correct-guess digit))
	  (t ;;; incorrect guess
	   (message "%c = %c incorrect !" letter-char digit-char)
	   (mpuz-ding)
	   (setq mpuz-nb-errors (1+ mpuz-nb-errors))
	   (mpuz-paint-errors)))))

(defun mpuz-correct-guess (digit)
  "Handle correct guessing of DIGIT."
  (aset mpuz-found-digits digit t)	; Mark digit as solved
  (mpuz-paint-digit digit)		; Repaint it (now as a digit)
  (if (mpuz-check-all-solved)
      (mpuz-close-game)))

(defun mpuz-close-game ()
  "Housecleaning when puzzle has been solved."
  (setq mpuz-in-progress nil
	mpuz-nb-cumulated-errors (+ mpuz-nb-cumulated-errors mpuz-nb-errors)
	mpuz-nb-completed-games (1+ mpuz-nb-completed-games))
  (mpuz-paint-statistics)
  (let ((message (mpuz-congratulate)))
    (message message)
    (sit-for 4)
    (if (y-or-n-p (concat message "  Start a new game "))
	(mpuz-start-new-game)
	(message "Good Bye !"))))

(defun mpuz-congratulate ()
  "Build a congratulation message when puzzle is solved."
  (format "Puzzle solved with %d errors. %s"
	   mpuz-nb-errors
	   (cond ((= mpuz-nb-errors 0)	      "That's perfect !")
		 ((= mpuz-nb-errors 1)	      "That's very good !")
		 ((= mpuz-nb-errors 2)	      "That's good.")
		 ((= mpuz-nb-errors 3)	      "That's not bad.")
		 ((= mpuz-nb-errors 4)	      "That's not too bad...")
		 ((and (>= mpuz-nb-errors 5)
		       (< mpuz-nb-errors 10)) "That's bad !")
		 ((and (>= mpuz-nb-errors 10)
		       (< mpuz-nb-errors 15)) "That's awful.")
		 ((>= mpuz-nb-errors 15)      "That's not serious."))))

(defun mpuz-show-solution ()
  "Display solution for debugging purposes."
  (interactive)
  (mpuz-switch-to-window)
  (let (digit list)
    (setq digit -1)
    (while (> 10 (setq digit (1+ digit)))
      (or (mpuz-digit-solved-p digit)
	  (setq list (cons digit list))))
    (mapcar 'mpuz-correct-guess list)))

;;; mpuz.el ends here
