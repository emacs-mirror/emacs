;;; japan-util.el --- utilities for Japanese  -*- lexical-binding: t; -*-

;; Copyright (C) 2001-2022 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: mule, multilingual, Japanese

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;###autoload
(defun setup-japanese-environment-internal ()
  (prefer-coding-system (if (memq system-type '(windows-nt ms-dos cygwin))
			    'japanese-shift-jis
			  'utf-8))
  (use-cjk-char-width-table 'ja_JP))

(defconst japanese-kana-table
  '((?あ ?ア ?ｱ) (?い ?イ ?ｲ) (?う ?ウ ?ｳ) (?え ?エ ?ｴ) (?お ?オ ?ｵ)
    (?か ?カ ?ｶ) (?き ?キ ?ｷ) (?く ?ク ?ｸ) (?け ?ケ ?ｹ) (?こ ?コ ?ｺ)
    (?さ ?サ ?ｻ) (?し ?シ ?ｼ) (?す ?ス ?ｽ) (?せ ?セ ?ｾ) (?そ ?ソ ?ｿ)
    (?た ?タ ?ﾀ) (?ち ?チ ?ﾁ) (?つ ?ツ ?ﾂ) (?て ?テ ?ﾃ) (?と ?ト ?ﾄ)
    (?な ?ナ ?ﾅ) (?に ?ニ ?ﾆ) (?ぬ ?ヌ ?ﾇ) (?ね ?ネ ?ﾈ) (?の ?ノ ?ﾉ)
    (?は ?ハ ?ﾊ) (?ひ ?ヒ ?ﾋ) (?ふ ?フ ?ﾌ) (?へ ?ヘ ?ﾍ) (?ほ ?ホ ?ﾎ)
    (?ま ?マ ?ﾏ) (?み ?ミ ?ﾐ) (?む ?ム ?ﾑ) (?め ?メ ?ﾒ) (?も ?モ ?ﾓ)
    (?や ?ヤ ?ﾔ) (?ゆ ?ユ ?ﾕ) (?よ ?ヨ ?ﾖ)
    (?ら ?ラ ?ﾗ) (?り ?リ ?ﾘ) (?る ?ル ?ﾙ) (?れ ?レ ?ﾚ) (?ろ ?ロ ?ﾛ)
    (?わ ?ワ ?ﾜ) (?ゐ ?ヰ "ｲ") (?ゑ ?ヱ "ｴ") (?を ?ヲ ?ｦ)
    (?ん ?ン ?ﾝ)
    (?が ?ガ "ｶﾞ") (?ぎ ?ギ "ｷﾞ") (?ぐ ?グ "ｸﾞ") (?げ ?ゲ "ｹﾞ") (?ご ?ゴ "ｺﾞ")
    (?ざ ?ザ "ｻﾞ") (?じ ?ジ "ｼﾞ") (?ず ?ズ "ｽﾞ") (?ぜ ?ゼ "ｾﾞ") (?ぞ ?ゾ "ｿﾞ")
    (?だ ?ダ "ﾀﾞ") (?ぢ ?ヂ "ﾁﾞ") (?づ ?ヅ "ﾂﾞ") (?で ?デ "ﾃﾞ") (?ど ?ド "ﾄﾞ")
    (?ば ?バ "ﾊﾞ") (?び ?ビ "ﾋﾞ") (?ぶ ?ブ "ﾌﾞ") (?べ ?ベ "ﾍﾞ") (?ぼ ?ボ "ﾎﾞ")
    (?ぱ ?パ "ﾊﾟ") (?ぴ ?ピ "ﾋﾟ") (?ぷ ?プ "ﾌﾟ") (?ぺ ?ペ "ﾍﾟ") (?ぽ ?ポ "ﾎﾟ")
    (?ぁ ?ァ ?ｧ) (?ぃ ?ィ ?ｨ) (?ぅ ?ゥ ?ｩ) (?ぇ ?ェ ?ｪ) (?ぉ ?ォ ?ｫ)
    (?っ ?ッ ?ｯ)
    (?ゃ ?ャ ?ｬ) (?ゅ ?ュ ?ｭ) (?ょ ?ョ ?ｮ)
    (?ゎ ?ヮ "ﾜ")
    (?ゝ ?ヽ) (?ゞ ?ヾ)
    ("う゛" ?ヴ "ｳﾞ") (nil ?ヵ "ｶ") (nil ?ヶ "ｹ"))
  "Japanese JISX0208 Kana character table.
Each element is of the form (HIRAGANA KATAKANA HANKAKU-KATAKANA), where
HIRAGANA and KATAKANA belong to `japanese-jisx0208',
HANKAKU-KATAKANA belongs to `japanese-jisx0201-kana'.")

;; Put properties 'katakana, 'hiragana, and 'jix0201 to each Japanese
;; kana characters for conversion among them.
(let ((l japanese-kana-table)
      slot hiragana katakana jisx0201)
  (while l
    (setq slot (car l)
	  hiragana (car slot) katakana (nth 1 slot) jisx0201 (nth 2 slot)
	  l (cdr l))
    (if hiragana
	(if (stringp hiragana)
	    (if (> (length hiragana) 1)
		(let ((hira (aref hiragana 0)))
		  (put-char-code-property
		   hira 'kana-composition
		   (cons (cons (aref hiragana 1) katakana)
			 (get-char-code-property hira 'kana-composition)))))
	  (put-char-code-property hiragana 'katakana katakana)
	  (put-char-code-property hiragana 'jisx0201 jisx0201)))
    (when (integerp katakana)
      (put-char-code-property katakana 'hiragana hiragana)
      (put-char-code-property katakana 'jisx0201 jisx0201))
    (if jisx0201
	(if (stringp jisx0201)
	    (if (> (length jisx0201) 1)
		(let ((kana (aref jisx0201 0)))
		  (put-char-code-property
		   kana 'kana-composition
		   (cons (cons (aref jisx0201 1) katakana)
			 (get-char-code-property kana 'kana-composition)))))
	  (put-char-code-property jisx0201 'hiragana hiragana)
	  (put-char-code-property jisx0201 'katakana katakana)
	  (put-char-code-property jisx0201 'jisx0208 katakana)))))

(defconst japanese-symbol-table
  '((?\　 ?\ ) (?， ?,) (?． ?.) (?、 nil ?､) (?。 nil ?｡) (?・ nil ?･)
    (?： ?:) (?； ?\;) (?？ ??) (?！ ?!) (?゛ nil ?ﾞ) (?゜ nil ?ﾟ)
    (?´ ?') (?｀ ?`) (?＾ ?^) (?＿ ?_) (?ー nil ?ｰ) (?— ?-) (?‐ ?-)
    (?／ ?/) (?＼ ?\\) (?〜 ?~)  (?｜ ?|) (?‘ ?`) (?’ ?') (?“ ?\") (?” ?\")
    (?\（ ?\() (?\） ?\)) (?\［ ?\[) (?\］ ?\]) (?\｛ ?{) (?\｝ ?})
    (?〈 ?<) (?〉 ?>) (?\「 nil ?\｢) (?\」 nil ?\｣)
    (?＋ ?+) (?− ?-) (?＝ ?=) (?＜ ?<) (?＞ ?>)
    (?′ ?') (?″ ?\") (?￥ ?\\) (?＄ ?$) (?％ ?%) (?＃ ?#) (?＆ ?&) (?＊ ?*)
    (?＠ ?@)
    ;; cp932-2-byte
    (#x2015 ?-) (#xFF5E ?~) (#xFF0D ?-))
  "Japanese JISX0208 and CP932 symbol character table.
Each element is of the form (SYMBOL ASCII HANKAKU), where SYMBOL
belongs to `japanese-jisx0208' or `cp932', ASCII belongs to `ascii',
and HANKAKU belongs to `japanese-jisx0201-kana'.")

;; Put properties 'jisx0208, 'jisx0201, and 'ascii to each Japanese
;; symbol and ASCII characters for conversion among them.
(let ((l japanese-symbol-table)
      slot jisx0208 ascii jisx0201)
  (while l
    (setq slot (car l)
	  jisx0208 (car slot) ascii (nth 1 slot) jisx0201 (nth 2 slot)
	  l (cdr l))
    (if ascii
	(progn
	  (put-char-code-property jisx0208 'ascii ascii)
	  (if (encode-char jisx0208 'japanese-jisx0208)
	      (put-char-code-property ascii 'jisx0208 jisx0208))))
    (if jisx0201
	(progn
	  (put-char-code-property jisx0208 'jisx0201 jisx0201)
	  (if (encode-char jisx0208 'japanese-jisx0208)
	      (put-char-code-property jisx0201 'jisx0208 jisx0208))))))

(defconst japanese-alpha-numeric-table
  '((?０ . ?0) (?１ . ?1) (?２ . ?2) (?３ . ?3) (?４ . ?4)
    (?５ . ?5) (?６ . ?6) (?７ . ?7) (?８ . ?8) (?９ . ?9)
    (?Ａ . ?A) (?Ｂ . ?B) (?Ｃ . ?C) (?Ｄ . ?D) (?Ｅ . ?E)
    (?Ｆ . ?F) (?Ｇ . ?G) (?Ｈ . ?H) (?Ｉ . ?I) (?Ｊ . ?J)
    (?Ｋ . ?K) (?Ｌ . ?L) (?Ｍ . ?M) (?Ｎ . ?N) (?Ｏ . ?O)
    (?Ｐ . ?P) (?Ｑ . ?Q) (?Ｒ . ?R) (?Ｓ . ?S) (?Ｔ . ?T)
    (?Ｕ . ?U) (?Ｖ . ?V) (?Ｗ . ?W) (?Ｘ . ?X) (?Ｙ . ?Y) (?Ｚ . ?Z)
    (?ａ . ?a) (?ｂ . ?b) (?ｃ . ?c) (?ｄ . ?d) (?ｅ . ?e)
    (?ｆ . ?f) (?ｇ . ?g) (?ｈ . ?h) (?ｉ . ?i) (?ｊ . ?j)
    (?ｋ . ?k) (?ｌ . ?l) (?ｍ . ?m) (?ｎ . ?n) (?ｏ . ?o)
    (?ｐ . ?p) (?ｑ . ?q) (?ｒ . ?r) (?ｓ . ?s) (?ｔ . ?t)
    (?ｕ . ?u) (?ｖ . ?v) (?ｗ . ?w) (?ｘ . ?x) (?ｙ . ?y) (?ｚ . ?z))
  "Japanese JISX0208 alpha numeric character table.
Each element is of the form (ALPHANUMERIC . ASCII), where ALPHANUMERIC
belongs to `japanese-jisx0208', ASCII belongs to `ascii'.")

;; Put properties 'jisx0208 and 'ascii to each Japanese alpha numeric
;; and ASCII characters for conversion between them.
(let ((l japanese-alpha-numeric-table)
      slot jisx0208 ascii)
  (while l
    (setq slot (car l)
	  jisx0208 (car slot) ascii (cdr slot)
	  l (cdr l))
    (put-char-code-property jisx0208 'ascii ascii)
    (put-char-code-property ascii 'jisx0208 jisx0208)))

;; Convert string STR by FUNC and return a resulting string.
(defun japanese-string-conversion (str func &rest args)
  (let ((buf (get-buffer-create " *Japanese work*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert str)
      (apply func 1 (point) args)
      (buffer-string))))

;;;###autoload
(defun japanese-katakana (obj &optional hankaku)
  "Convert argument to Katakana and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional argument HANKAKU t means to convert to `hankaku' Katakana
\(`japanese-jisx0201-kana'), in which case return value
may be a string even if OBJ is a character if two Katakanas are
necessary to represent OBJ."
  (if (stringp obj)
      (japanese-string-conversion obj 'japanese-katakana-region hankaku)
    (or (get-char-code-property obj (if hankaku 'jisx0201 'katakana))
	obj)))

;;;###autoload
(defun japanese-hiragana (obj)
  "Convert argument to Hiragana and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy."
  (if (stringp obj)
      (japanese-string-conversion obj 'japanese-hiragana-region)
    (or (get-char-code-property obj 'hiragana)
	obj)))

;;;###autoload
(defun japanese-hankaku (obj &optional ascii-only)
  "Convert argument to `hankaku' and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional argument ASCII-ONLY non-nil means to return only ASCII character."
  (if (stringp obj)
      (japanese-string-conversion obj 'japanese-hankaku-region ascii-only)
    (or (and (not ascii-only)
	     (get-char-code-property obj 'jisx0201))
	(get-char-code-property obj 'ascii)
	obj)))

;;;###autoload
(defun japanese-zenkaku (obj)
  "Convert argument to `zenkaku' and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy."
  (if (stringp obj)
      (japanese-string-conversion obj 'japanese-zenkaku-region)
    (or (get-char-code-property obj 'jisx0208)
	obj)))

(defun japanese-replace-region (from to string)
  "Replace the region specified by FROM and TO to STRING."
  (goto-char from)
  (insert string)
  (delete-char (- to from)))

;;;###autoload
(defun japanese-katakana-region (from to &optional hankaku)
  "Convert Japanese `hiragana' chars in the region to `katakana' chars.
Optional argument HANKAKU t means to convert to `hankaku katakana' character
of which charset is `japanese-jisx0201-kana'."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region from to)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\cH\\|\\cK" nil t)
	(let* ((kana (preceding-char))
	       (composition
		(and (not hankaku)
		     (get-char-code-property kana 'kana-composition)))
	       slot) ;; next
	  (if (and composition (setq slot (assq (following-char) composition)))
	      (japanese-replace-region (match-beginning 0) (1+ (point))
				       (cdr slot))
	    (let ((kata (get-char-code-property
			 kana (if hankaku 'jisx0201 'katakana))))
	      (if kata
		  (japanese-replace-region (match-beginning 0) (point)
					   kata)))))))))


;;;###autoload
(defun japanese-hiragana-region (from to)
  "Convert Japanese `katakana' chars in the region to `hiragana' chars."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\cK\\|\\ck" nil t)
	(let* ((kata (preceding-char))
	       (composition (get-char-code-property kata 'kana-composition))
	       slot) ;; next
	  (if (and composition (setq slot (assq (following-char) composition)))
	      (japanese-replace-region (match-beginning 0) (1+ (point))
				       (get-char-code-property
					(cdr slot) 'hiragana))
	    (let ((hira (get-char-code-property kata 'hiragana)))
	      (if hira
		  (japanese-replace-region (match-beginning 0) (point)
					   hira)))))))))

;;;###autoload
(defun japanese-hankaku-region (from to &optional ascii-only)
  "Convert Japanese `zenkaku' chars in the region to `hankaku' chars.
`Zenkaku' chars belong to `japanese-jisx0208'
`Hankaku' chars belong to `ascii' or `japanese-jisx0201-kana'.
Optional argument ASCII-ONLY non-nil means to convert only to ASCII char."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region from to)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\cj" nil t)
	(let* ((zenkaku (preceding-char))
	       (hankaku (or (and (not ascii-only)
				 (get-char-code-property zenkaku 'jisx0201))
			    (get-char-code-property zenkaku 'ascii))))
	  (if hankaku
	      (japanese-replace-region (match-beginning 0) (match-end 0)
				       hankaku)))))))

;;;###autoload
(defun japanese-zenkaku-region (from to &optional katakana-only)
  "Convert hankaku' chars in the region to Japanese `zenkaku' chars.
`Zenkaku' chars belong to `japanese-jisx0208'
`Hankaku' chars belong to `ascii' or `japanese-jisx0201-kana'.
Optional argument KATAKANA-ONLY non-nil means to convert only KATAKANA char."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region from to)
    (save-excursion
      (goto-char (point-min))
      (while (or (and katakana-only
		      (re-search-forward "\\ck" nil t))
		 (and (not katakana-only)
		      (re-search-forward "\\ca\\|\\ck" nil t)))
	(let* ((hankaku (preceding-char))
	       (composition (get-char-code-property hankaku 'kana-composition))
	       slot) ;; next
	  (if (and composition (setq slot (assq (following-char) composition)))
	      (japanese-replace-region (match-beginning 0) (1+ (point))
				       (cdr slot))
	    (let ((zenkaku (japanese-zenkaku hankaku)))
	      (if zenkaku
		  (japanese-replace-region (match-beginning 0) (match-end 0)
					   zenkaku)))))))))

;;;###autoload
(defun read-hiragana-string (prompt &optional initial-input)
  "Read a Hiragana string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading."
  (read-multilingual-string prompt initial-input "japanese-hiragana"))

;;
(provide 'japan-util)

;;; japan-util.el ends here
