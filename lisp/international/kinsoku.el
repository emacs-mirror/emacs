;;; kinsoku.el --- `Kinsoku' processing functions.

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: kinsoku

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

;; `Kinsoku' processing is to prohibit specific characters to be
;; placed at beginning of line or at end of line.  Characters not to
;; be placed at beginning and end of line have character category `>'
;; and `<' respectively.   This restriction is dissolved by making a
;; line longer or shorter.
;;
;; `Kinsoku' is a Japanese word which originally means ordering to
;; stay in one place, and is used for the text processing described
;; above in the context of text formatting.

;;; Code:

(defvar kinsoku-limit 4
  "How many more columns we can make lines longer by `kinsoku' processing.
The value 0 means there's no limitation.")

;; Setting character category `>' for characters which should not be
;; placed at beginning of line.
(let* ((kinsoku-bol
	(concat
	 ;; ASCII
	 "!)-_~}]:;',.?"
	 ;; Japanese JISX0208
	 "$B!"!#!$!%!&!'!(!)!*!+!,!-!.!/!0!1!2!3!4!5!6!7!8!9!:!;!<!=!>(B\
$B!?!@!A!B!C!D!E!G!I!K!M!O!Q!S!U!W!Y![!k!l!m!n(B\
$B$!$#$%$'$)$C$c$e$g$n%!%#%%%'%)%C%c%e%g%n%u%v(B"
	 ;; Chinese GB2312
	 "$A!"!##.#,!$!%!&!'!(!)!*!+!,!-!/!1#)!3!5!7!9!;!=(B\
$A!?#;#:#?#!!@!A!B!C!c!d!e!f#/#\#"#_#~#|(e(B"
	 ;; Chinese BIG5
	 "$(0!"!#!$!%!&!'!(!)!*!+!,!-!.!/!0!1!2(B\
$(0!3!4!5!6!7!8!9!:!;!<!=!?!A!C!E!G!I!K(B\
$(0!M!O!Q(B	$(0!S!U!W!Y![!]!_!a!c!e!g!i!k!q(B\
$(0"#"$"%"&"'"(")"*"+","2"3"4"j"k"l"x%7(B"))
       (len (length kinsoku-bol))
       (idx 0)
       ch)
  (while (< idx len)
    (setq ch (sref kinsoku-bol idx)
	  idx (+ idx (char-bytes ch)))
    (modify-category-entry ch ?>)))

;; Setting character category `<' for characters which should not be
;; placed at end of line.
(let* ((kinsoku-eol
	(concat
	 ;; ASCII
	 "({[`"
	 ;; Japanese JISX0208
	 "$B!F!H!J!L!N!P!R!T!V!X!Z!k!l!m!n!w!x(B\
$A!.!0#"#(!2!4!6!8!:!<!>!c!d!e#@!f!l(B"
	 ;; Chinese GB2312
	 "$A(E(F(G(H(I(J(K(L(M(N(O(P(Q(R(S(T(U(V(W(X(Y(h(B\
$(0!>!@!B!D!F!H!J!L!N!P!R!T!V!X!Z!\!^!`!b(B"
	 ;; Chinese BIG5
	 "$(0!d!f!h!j!k!q!p"i"j"k"n"x$u$v$w$x$y$z${(B\
$(0$|$}$~%!%"%#%$%%%&%'%(%)%*%+%:(B"))
       (len (length kinsoku-eol))
       (idx 0)
       ch)
  (while (< idx len)
    (setq ch (sref kinsoku-eol idx)
	  idx (+ idx (char-bytes ch)))
    (modify-category-entry ch ?<)))

;; Try to resolve `kinsoku' restriction by making the current line longer.
(defun kinsoku-longer ()
  (let ((pos-and-column (save-excursion
			  (forward-char 1)
			  (while (aref (char-category-set (following-char)) ?>)
			    (forward-char 1))
			  (cons (point) (current-column)))))
    (if (or (<= kinsoku-limit 0)
	    (< (cdr pos-and-column) (+ (current-fill-column) kinsoku-limit)))
	(goto-char (car pos-and-column)))))

;; Try to resolve `kinsoku' restriction by making the current line shorter.
;; The line can't be broken before the buffer position LINEBEG."
(defun kinsoku-shorter (linebeg)
  (let ((pos (save-excursion
	       (forward-char -1)
	       (while (and (< linebeg (point))
			   (or (aref (char-category-set (preceding-char)) ?<)
			       (aref (char-category-set (following-char)) ?>)))
		 (forward-char -1))
	       (point))))
    (if (< linebeg pos)
	(goto-char pos))))

;;;###autoload
(defun kinsoku (linebeg)
  "Go to a line breaking position near point by doing `kinsoku' processing.
LINEBEG is a buffer position we can't break a line before.

`Kinsoku' processing is to prohibit specific characters to be placed
at beginning of line or at end of line.  Characters not to be placed
at beginning and end of line have character category `>' and `<'
respectively.  This restriction is dissolved by making a line longer or
shorter.

`Kinsoku' is a Japanese word which originally means ordering to stay
in one place, and is used for the text processing described above in
the context of text formatting."
  (if (or (and
	   ;; The character after point can't be placed at beginning
	   ;; of line.  
	   (aref (char-category-set (following-char)) ?>)
	   ;; We at first try to dissolve this situation by making a
	   ;; line longer.  If it fails, then try making a line
	   ;; shorter.
	   (not (kinsoku-longer)))
	  ;; The character before point can't be placed at end of line.
	  (aref (char-category-set (preceding-char)) ?<))
      (kinsoku-shorter linebeg)))

;; kinsoku.el ends here
