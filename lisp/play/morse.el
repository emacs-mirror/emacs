;;; morse.el --- convert text to morse code and back             -*- coding: utf-8 -*-

;; Copyright (C) 1995, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Rick Farnbach <rick_farnbach@MENTORG.COM>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Converts text to Morse code and back with M-x morse-region and
;; M-x unmorse-region (though Morse code is no longer official :-().

;;; Code:

(defvar morse-code '(("a" . ".-")
		     ("b" . "-...")
		     ("c" . "-.-.")
		     ("d" . "-..")
		     ("e" . ".")
		     ("f" . "..-.")
		     ("g" . "--.")
		     ("h" . "....")
		     ("i" . "..")
		     ("j" . ".---")
		     ("k" . "-.-")
		     ("l" . ".-..")
		     ("m" . "--")
		     ("n" . "-.")
		     ("o" . "---")
		     ("p" . ".--.")
		     ("q" . "--.-")
		     ("r" . ".-.")
		     ("s" . "...")
		     ("t" . "-")
		     ("u" . "..-")
		     ("v" . "...-")
		     ("w" . ".--")
		     ("x" . "-..-")
		     ("y" . "-.--")
		     ("z" . "--..")
		     ;; Punctuation
		     ("=" . "-...-")
		     ("?" . "..--..")
		     ("/" . "-..-.")
		     ("," . "--..--")
		     ("." . ".-.-.-")
		     (":" . "---...")
		     ("'" . ".----.")
		     ("-" . "-....-")
		     ("(" . "-.--.-")
		     (")" . "-.--.-")
		     ;; Numbers
		     ("0" . "-----")
		     ("1" . ".----")
		     ("2" . "..---")
		     ("3" . "...--")
		     ("4" . "....-")
		     ("5" . ".....")
		     ("6" . "-....")
		     ("7" . "--...")
		     ("8" . "---..")
		     ("9" . "----.")
		     ;; Non-ASCII
		     ("Ä" . ".-.-")
		     ("Æ" . ".-.-")
		     ("Á" . ".--.-")
		     ("Å" . ".--.-")
		     ;; ligature character?? ("Ch" . "----")
		     ("ß" . ".../...")
		     ("É" . "..-..")
		     ("Ñ" . "--.--")
		     ("Ö" . "---.")
		     ("Ø" . "---.")
		     ("Ü" . "..--")
		     ;; Recently standardized
		     ("@" . ".--.-."))
  "Morse code character set.")

;;;###autoload
(defun morse-region (beg end)
  "Convert all text in a given region to morse code."
  (interactive "r")
  (if (integerp end)
      (setq end (copy-marker end)))
  (save-excursion
    (let ((sep "")
	  str morse)
      (goto-char beg)
      (while (< (point) end)
	(setq str (downcase (buffer-substring (point) (1+ (point)))))
	(cond ((looking-at "\\s-+")
	       (goto-char (match-end 0))
	       (setq sep ""))
	      ((setq morse (assoc str morse-code))
	       (delete-char 1)
	       (insert sep (cdr morse))
	       (setq sep "/"))
	      (t
	       (forward-char 1)
	       (setq sep "")))))))

;;;###autoload
(defun unmorse-region (beg end)
  "Convert morse coded text in region to ordinary ASCII text."
  (interactive "r")
  (if (integerp end)
      (setq end (copy-marker end)))
  (save-excursion
    (let (str paren morse)
      (goto-char beg)
      (while (< (point) end)
	(if (null (looking-at "[-.]+"))
	    (forward-char 1)
	  (setq str (buffer-substring (match-beginning 0) (match-end 0)))
	  (if (null (setq morse (rassoc str morse-code)))
	      (goto-char (match-end 0))
	    (replace-match
		  (if (string-equal "(" (car morse))
		      (if (setq paren (null paren)) "(" ")")
		    (car morse)) t)
	    (if (looking-at "/")
		(delete-char 1))))))))

(provide 'morse)

;;; arch-tag: 3331e6c1-9a9e-453f-abfd-163a9c3f93a6
;;; morse.el ends here
