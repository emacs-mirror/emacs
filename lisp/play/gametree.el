;;; gametree.el --- manage game analysis trees in Emacs

;;  Copyright (C) 1997 Free Software Foundation, Inc

;; Author: Ian T Zimmerman <itz@rahul.net>
;; Created: Wed Dec 10 07:41:46 PST 1997
;; Keywords: games

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

;;  This little hack has enabled me to keep track of my email chess
;;  games in Emacs.  For a long time I dreamt about writing a real,
;;  graphical tree editor; but, then the idea struck me, why do it
;;  graphically, when it can be done in Emacs? :-)  And in fact Emacs
;;  almost had what I needed out of the box, namely the powerful
;;  Outline mode.  This code is built entirely on Outline mode, it
;;  only adds two commands that I found indispensable when dealing
;;  with the special kind of trees that analysis trees comprise.

;;  The built-in documentation should be enough to explain the use,
;;  along with the following example (yes, this is a real game).

;;  *** 23. f4 ef 24. Nf3 Rf3 -/+
;;  25. Rf3 Qh2 26. Kh1 Qh1 27. Ke2 Qg2 -+
;;  ******* 25. gf3 Nce3 26. Qe2 Nf1 -/+
;;  27. Qe5 Ne5 28. Kf1 Nf3 -/+
;;  27. Kf1 Nh2 28. Kf2 Ng4 29. fg4 Rf8 30. Ke1 Qg3 31. Kd1 Rf2 -+

;;  Place the preceding in a scratch buffer, load this code, and do
;;  M-x gametree-mode.  Now place the cursor just after the `Nf3' and
;;  before the `Rf3' on the first line, and do C-c C-j.  The result is

;;  *** 23. f4 ef 24. Nf3
;;  ****** 24: Rf3 -/+
;;  25. Rf3 Qh2 26. Kh1 Qh1 27. Ke2 Qg2 -+
;;  ******* 25. gf3 Nce3 26. Qe2 Nf1 -/+
;;  27. Qe5 Ne5 28. Kf1 Nf3 -/+
;;  27. Kf1 Nh2 28. Kf2 Ng4 29. fg4 Rf8 30. Ke1 Qg3 31. Kd1 Rf2 -+

;;  Now you can add another subvariation on Black's 24th move: with
;;  the cursor still on the first line, do C-c C-v, and voila

;;  *** 23. f4 ef 24. Nf3
;;  24:
;;  ****** 24: Rf3 -/+
;;  25. Rf3 Qh2 26. Kh1 Qh1 27. Ke2 Qg2 -+
;;  ******* 25. gf3 Nce3 26. Qe2 Nf1 -/+
;;  27. Qe5 Ne5 28. Kf1 Nf3 -/+
;;  27. Kf1 Nh2 28. Kf2 Ng4 29. fg4 Rf8 30. Ke1 Qg3 31. Kd1 Rf2 -+

;;  and the cursor is positioned on the new line just after the move
;;  number, so you can start typing the new analysis.  That's it,
;;  quite simple.  If you want more, read on.

;;; ToDo:

;;  BIG: automatic score reducer.  It should be possible to label the
;;  leaf variations with numeric scores (instead of the Informant-like
;;  symbols used in the example) and have the program apply the
;;  min-max algorithm to score the internal nodes.  That's about as
;;  far as one can go in a postal game while honestly claiming not to
;;  use computer analysis.

;;  I'd definitely like to hear from you if you use this, and even
;;  more if you have suggestions for improvement, ranging from bug
;;  reports to feature requests.  (But be warned that I am a fan of
;;  simplicity and orthogonality).

;;; Code:

(require 'derived)
(require 'outline)

;;;; Configuration variables

(defgroup gametree nil
  "Manage game analysis trees in Emacs."
  :prefix "gametree-"
  :group 'games
  :version "20.3")

(defcustom gametree-half-ply-regexp (regexp-quote ":")
  "*Matches ends of numbers of moves by the \"second\" player.
For instance, it is an almost universal convention in chess to postfix
numbers of moves by Black (if considered in isolation) by the ellipsis
\"...\".  This is NOT a good choice for this program, though, because it
conflicts with the use of ellipsis by Outline mode to denote collapsed
subtrees.  The author uses \":\" because it agrees nicely with a set of
LaTeX macros he uses for typesetting annotated games."
  :type 'regexp
  :group 'gametree)

(defcustom gametree-full-ply-regexp (regexp-quote ".")
  "*Matches ends of numbers of moves by the \"first\" player.
For instance, it is an almost universal convention in chess to postfix
numbers of moves by White (if considered in isolation) by the dot \".\"."
  :type 'regexp
  :group 'gametree)

(defcustom gametree-half-ply-format "%d:"
  "*Output format for move numbers of moves by the \"second\" player.
Has to contain \"%d\" to output the actual number."
  :type 'string
  :group 'gametree)

(defcustom gametree-full-ply-format "%d."
  "*Output format for move numbers of moves by the \"first\" player.
Has to contain \"%d\" to output the actual number."
  :type 'string
  :group 'gametree)

(defcustom gametree-make-heading-function
  (function (lambda (level)
              (insert (make-string level ?*))))
  "A function of one numeric argument, LEVEL, to insert a heading at point.
You should change this if you change `outline-regexp'."
  :type 'function
  :group 'gametree)

(defvar gametree-local-layout nil
  "A list encoding the layout (i.e. the show or hide state) of the file.
If Emacs notices a local variable specification of this variable in
the first line of the buffer while saving the buffer to the visited
file, the local value will be saved there and restored the next time
the file is visited (subject to the usual restriction via
`enable-local-variables'), and the layout will be set accordingly.")

;;;; Helper functions

(defun gametree-prettify-heading ()
  "Insert/delete space between leading asterisks and heading text.
If the current variation is an internal node (i.e. starts with one or
more asterisks), ensure there's at least one space between the
asterisks and the text.  If on the other hand this is a leaf, there
should be no leading white space."
  (save-excursion
    (beginning-of-line 1)
    (if (re-search-forward (concat "\\=" outline-regexp) nil t)
        (if (not (looking-at "[ \t]+")) (insert " "))
      (delete-char (save-excursion (skip-chars-forward " \t"))))
    (if (re-search-forward (concat "\\=[ \t]*[1-9][0-9]*\\("
                                   gametree-full-ply-regexp "\\|"
                                   gametree-half-ply-regexp "\\)") nil t)
        (if (not (looking-at "[ \t]+")) (insert " ")
          (delete-char (1- (save-excursion (skip-chars-forward  " \t"))))))))

(defun gametree-looking-at-ply ()
  "Read and return the number of the ply under point."
  (if (eobp) 0
    (let ((boundary (concat "[ \t]*\\([1-9][0-9]*\\)\\(" 
                            gametree-full-ply-regexp "\\|"
                            gametree-half-ply-regexp "\\)"))
          (limit (save-excursion (beginning-of-line 1) (point))))
      (if (looking-at boundary)
          (+ (* 2 (string-to-int (match-string 1)))
             (if (string-match gametree-half-ply-regexp (match-string 2)) 1 0))
        (save-excursion 
          (re-search-backward boundary limit)
          (skip-chars-backward "0123456789")
          (1+ (* 2 (string-to-int
                    (buffer-substring (point) (match-end 1))))))))))
    
(defun gametree-current-branch-ply ()
  "Return the ply number of the first move of the current variation."
  (save-excursion
    (beginning-of-line 1)
    (re-search-forward (concat "\\=" outline-regexp) nil t)
    (gametree-looking-at-ply)))

(defun gametree-current-branch-depth ()
  "Return the depth of the current variation in the analysis tree.
This value is simply the outline heading level of the current line."
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at outline-regexp)
        (outline-level) 0)))

;;;; Functions related to the task of saving and restoring current
;;;; outline layout

(defun gametree-show-children-and-entry ()
  (show-children)
  (show-entry))

(defun gametree-entry-shown-p ()
  (save-excursion
    (forward-line 1)
    (and (bolp) (not (eobp)) (not (looking-at outline-regexp)))))

(defun gametree-children-shown-p ()
  (save-excursion
    (condition-case nil
        (let ((depth (gametree-current-branch-depth)))
          (outline-next-visible-heading 1)
          (< depth (gametree-current-branch-depth)))
      (error nil))))

(defun gametree-current-layout (depth &optional top-level)
  (let ((layout nil) (first-time t))
    (while (save-excursion
             (condition-case nil
                 (progn
                   (or (and first-time top-level
                            (bolp) (looking-at outline-regexp))
                       (setq first-time nil)
                       (outline-next-visible-heading 1))
                   (< depth (gametree-current-branch-depth)))
               (error nil)))
      (if (not first-time)
          (outline-next-visible-heading 1))
      (setq first-time nil)
      (if (not (gametree-children-shown-p))
          (setq layout
                (nconc layout
                       (if (gametree-entry-shown-p)
                           (list 'show-entry)
                         (list nil))))
        (setq layout (nconc layout (if (gametree-entry-shown-p)
                                       (list 'gametree-show-children-and-entry)
                                     (list 'show-children))))
        (let ((sub-layout
               (gametree-current-layout (gametree-current-branch-depth))))
          (setq layout (nconc layout (list sub-layout))))))
    layout))

(defun gametree-save-layout ()
  (save-excursion
    (goto-char (point-min))
    (setq gametree-local-layout (gametree-current-layout 0 t))))

(defun gametree-apply-layout (layout depth &optional top-level)
  (let ((first-time t))
    (while (and layout
                (save-excursion
                  (condition-case nil
                      (progn
                        (or (and first-time top-level
                                 (bolp) (looking-at outline-regexp))
                            (setq first-time nil)
                            (outline-next-visible-heading 1))
                        (< depth (gametree-current-branch-depth)))
                    (error nil))))
      (if (not first-time)
          (outline-next-visible-heading 1))
      (setq first-time nil)
      (hide-subtree)
      (if (nth 0 layout)
          (funcall (nth 0 layout)))
      (if (not (and (nth 1 layout) (listp (nth 1 layout))))
          (setq layout (cdr layout))
        (gametree-apply-layout (nth 1 layout)
                               (gametree-current-branch-depth))
        (setq layout (cdr (cdr layout)))))))

(defun gametree-restore-layout ()
  (save-excursion
    (goto-char (point-min))
    (gametree-apply-layout gametree-local-layout 0 t)))

(defun gametree-hack-file-layout ()
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "[^\n]*-\*-[^\n]*gametree-local-layout: \\([^;\n]*\\);")
        (progn
          (goto-char (match-beginning 1))
          (delete-region (point) (match-end 1))
          (let ((standard-output (current-buffer)))
            (princ gametree-local-layout))))))          

;;;; Commands

(defun gametree-insert-new-leaf (&optional at-depth)
  "Start a new leaf variation under the current branching point.
The new variation can later be split to be a branching point itself,
with \\[gametree-break-line-here].  If the point is currently on a
leaf variation, this command won't work; use \\[gametree-break-line-here]
on the current line first.

With a numeric arg AT-DEPTH, first go up the tree until a node of
depth AT-DEPTH or smaller is found."
  (interactive "P")
  (if (zerop (gametree-current-branch-depth))
      (outline-up-heading 0))
  (if at-depth
      (while (> (gametree-current-branch-depth)
                (prefix-numeric-value at-depth))
        (outline-up-heading 1)))
  (beginning-of-line 1)
  (let ((parent-depth (gametree-current-branch-depth)))
    (show-entry)
    (condition-case nil
        (outline-next-visible-heading 1)
      (error
       (goto-char (point-max))
       (if (not (bolp)) (insert "\n"))))
    (let ((starting-plys 
           (if (> (gametree-current-branch-depth) parent-depth)
               (gametree-current-branch-ply)
             (save-excursion (forward-line -1)
                             (gametree-current-branch-ply)))))
      (goto-char (1- (point)))
      (insert "\n")
      (insert (format (if (= 0 (mod starting-plys 2))
                          gametree-full-ply-format
                        gametree-half-ply-format)
                      (/ starting-plys 2))))))

(defun gametree-break-line-here (&optional at-move)
  "Split the variation node at the point position.
This command works whether the current variation node is a leaf, or is
already branching at its end.  The new node is created at a level that
reflects the number of game plys between the beginning of the current
variation and the breaking point.

With a numerical argument AT-MOVE, split the variation before
White's AT-MOVEth move, or Black's if negative.  The last option will
only work of Black's moves are explicitly numbered, for instance 
`1. e4 1: e5'."
  (interactive "P")
  (if at-move (progn
          (end-of-line 1)
          (let ((limit (point)))
            (beginning-of-line 1)
            (re-search-forward
             (concat
              (regexp-quote
               (int-to-string (abs (prefix-numeric-value at-move))))
              (if (> at-move 0) gametree-full-ply-regexp
                gametree-half-ply-regexp)) limit))
          (goto-char (match-beginning 0))))
  (let* ((pt (set-marker (make-marker) (point)))
         (plys (gametree-current-branch-ply))
         (depth (gametree-current-branch-depth))
         (old-depth depth))
    (if (= depth 0)
        (progn
          (save-excursion
            (outline-previous-visible-heading 1)
            (setq depth
                  (let ((old-branch-ply
                         (condition-case nil
                             (gametree-current-branch-ply)
                           (error 0))))
                    (if (zerop old-branch-ply)
                        (1+ (gametree-current-branch-depth))
                      (+ (gametree-current-branch-depth)
                         (- plys old-branch-ply))))))
          (save-excursion
            (beginning-of-line 1)
            (funcall gametree-make-heading-function depth)
            (gametree-prettify-heading))))
    (save-excursion
      (if (not (looking-at (concat "[ \t]*[1-9][0-9]*\\(" 
                                   gametree-full-ply-regexp "\\|" 
                                   gametree-half-ply-regexp "\\)")))
          (progn
            (insert (format (if (= 0 (mod (gametree-looking-at-ply) 2))
                                gametree-full-ply-format
                              gametree-half-ply-format)
                            (/ (gametree-looking-at-ply) 2)))
            (gametree-prettify-heading)
            (beginning-of-line 1)))
      (goto-char pt)
      (insert "\n")
      (if (not (= 0 old-depth))
          (funcall gametree-make-heading-function
                   (+ depth (- (gametree-current-branch-ply) plys))))
      (gametree-prettify-heading))))

(defun gametree-merge-line ()
  "Merges a variation with its only child.
Does *not* check if the variation has in fact a unique child; users beware."
  (interactive)
  (if (zerop (gametree-current-branch-depth))
      (outline-up-heading 0))
  (end-of-line 1)
  (let ((prev-depth (save-excursion (forward-line 1)
                                    (gametree-current-branch-depth))))
    (delete-char (1+ prev-depth))
    (if (zerop prev-depth)
        (save-excursion
          (beginning-of-line 1)
          (delete-char (gametree-current-branch-depth))
          (gametree-prettify-heading)))))

(defun gametree-layout-to-register (register)
  "Store current tree layout in register REGISTER.
Use \\[gametree-apply-register-layout] to restore that configuration.
Argument is a character, naming the register."
  (interactive "cLayout to register: ")
  (save-excursion
    (goto-char (point-min))
    (set-register register
                  (gametree-current-layout 0 t))))

(defun gametree-apply-register-layout (char)
  "Return to a tree layout stored in a register.
Argument is a character, naming the register."
  (interactive "cApply layout from register: ")
  (save-excursion
    (goto-char (point-min))
    (gametree-apply-layout (get-register char) 0 t)))

(defun gametree-save-and-hack-layout ()
  "Save the current tree layout and hack the file local variable spec.
This function saves the current layout in `gametree-local-layout' and,
if a local file varible specification for this variable exists in the
buffer, it is replaced by the new value.  See the documentation for
`gametree-local-layout' for more information."
  (interactive)
  (gametree-save-layout)
  (gametree-hack-file-layout)
  nil)

(define-derived-mode gametree-mode outline-mode "GameTree"
  "Major mode for managing game analysis trees.  
Useful to postal and email chess (and, it is hoped, also checkers, go,
shogi, etc.) players, it is a slightly modified version of Outline mode.

\\{gametree-mode-map}"
(auto-fill-mode 0)
(make-variable-buffer-local 'write-contents-hooks)
(add-hook 'write-contents-hooks 'gametree-save-and-hack-layout))

;;;; Key bindings

(define-key gametree-mode-map "\C-c\C-j" 'gametree-break-line-here)
(define-key gametree-mode-map "\C-c\C-v" 'gametree-insert-new-leaf)
(define-key gametree-mode-map "\C-c\C-m" 'gametree-merge-line)
(define-key gametree-mode-map "\C-c\C-r " 'gametree-layout-to-register)
(define-key gametree-mode-map "\C-c\C-r/" 'gametree-layout-to-register)
(define-key gametree-mode-map "\C-c\C-rj" 'gametree-apply-register-layout)
(define-key gametree-mode-map "\C-c\C-y" 'gametree-save-and-hack-layout)

;;;; Goodies for mousing users
(and (fboundp 'track-mouse)
     (defun gametree-mouse-break-line-here (event)
       (interactive "e")
       (mouse-set-point event)
       (gametree-break-line-here))
     (defun gametree-mouse-show-children-and-entry (event)
       (interactive "e")
       (mouse-set-point event)
       (gametree-show-children-and-entry))
     (defun gametree-mouse-show-subtree (event)
       (interactive "e")
       (mouse-set-point event)
       (show-subtree))
     (defun gametree-mouse-hide-subtree (event)
       (interactive "e")
       (mouse-set-point event)
       (hide-subtree))
     (define-key gametree-mode-map [M-down-mouse-2 M-mouse-2]
       'gametree-mouse-break-line-here)
     (define-key gametree-mode-map [S-down-mouse-1 S-mouse-1]
       'gametree-mouse-show-children-and-entry)
     (define-key gametree-mode-map [S-down-mouse-2 S-mouse-2]
       'gametree-mouse-show-subtree)
     (define-key gametree-mode-map [S-down-mouse-3 S-mouse-3]
       'gametree-mouse-hide-subtree))

(provide 'gametree)

;;; gametree.el ends here
