;; japan-util.el --  utilities for Japanese

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: mule, multilingual, Japanese

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

;;; Code:

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
    (?わ ?ワ ?ﾜ) (?ゐ ?ヰ nil) (?ゑ ?ヱ nil) (?を ?ヲ ?ｦ)
    (?ん ?ン ?ﾝ)
    (?が ?ガ "ｶﾞ") (?ぎ ?ギ "ｷﾞ") (?ぐ ?グ "ｸﾞ") (?げ ?ゲ "ｹﾞ") (?ご ?ゴ "ｺﾞ")
    (?ざ ?ザ "ｻﾞ") (?じ ?ジ "ｼﾞ") (?ず ?ズ "ｽﾞ") (?ぜ ?ゼ "ｾﾞ") (?ぞ ?ゾ "ｿﾞ")
    (?だ ?ダ "ﾀﾞ") (?ぢ ?ヂ "ﾁﾞ") (?づ ?ヅ "ﾂﾞ") (?で ?デ "ﾃﾞ") (?ど ?ド "ﾄﾞ")
    (?ば ?バ "ﾊﾞ") (?び ?ビ "ﾋﾞ") (?ぶ ?ブ "ﾌﾞ") (?べ ?ベ "ﾍﾞ") (?ぼ ?ボ "ﾎﾞ")
    (?ぱ ?パ "ﾊﾟ") (?ぴ ?ピ "ﾋﾟ") (?ぷ ?プ "ﾌﾟ") (?ぺ ?ペ "ﾍﾟ") (?ぽ ?ポ "ﾎﾟ")
    (?ぁ ?ァ ?ｧ) (?ぃ ?ィ ?ｨ) (?ぅ ?ゥ ?ｩ) (?ぇ ?ェ ?ｪ) (?ぉ ?ォ ?ｫ)
    (?っ ?ッ ?ｯ)
    (?ゃ ?ャ ?ｬ) (?ゅ ?ュ ?ｭ) (?ょ ?ョ ?ｮ)
    (?ゎ ?ヮ nil)
    (nil ?ヴ "ｳﾞ") (nil ?ヵ nil) (nil ?ヶ nil))
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
	(progn
	  (put-char-code-property hiragana 'katakana katakana)
	  (put-char-code-property katakana 'hiragana hiragana)
	  (if jisx0201
	      (progn
		(put-char-code-property hiragana 'jisx0201 jisx0201)
		(if (integerp jisx0201)
		    (put-char-code-property jisx0201 'hiragana hiragana))))))
    (if jisx0201
	(progn
	  (put-char-code-property katakana 'jisx0201 jisx0201)
	  (if (integerp jisx0201)
	      (put-char-code-property jisx0201 'katakana katakana))))))

(defconst japanese-symbol-table
  '((?\　 ?\ ) (?、 ?, ?､) (?。 ?. ?｡) (?， ?, ?､) (?． ?. ?｡) (?・ nil ?･)
    (?： ?:) (?； ?\;) (?？ ??) (?！ ?!) (?゛ nil ?ﾞ) (?゜ nil ?ﾟ)
    (?´ ?') (?｀ ?`) (?＾ ?^) (?＿ ?_) (?ー ?-) (?― ?-) (?‐ ?-)
    (?／ ?/) (?＼ ?\\) (?〜 ?~)  (?｜ ?|) (?‘ ?`) (?’ ?') (?“ ?\") (?” ?\")
    (?\（ ?\() (?\） ?\)) (?\［ ?[) (?\］ ?]) (?\｛ ?{) (?\｝ ?})
    (?〈 ?<) (?〉 ?>) (?＋ ?+) (?− ?-) (?＝ ?=) (?＜ ?<) (?＞ ?>)
    (?′ ?') (?″ ?\") (?￥ ?\\) (?＄ ?$) (?％ ?%) (?＃ ?#) (?＆ ?&) (?＊ ?*)
    (?＠ ?@))
  "Japanese JISX0208 symbol character table.
  Each element is of the form (SYMBOL ASCII HANKAKU), where SYMBOL
belongs to `japanese-jisx0208', ASCII belongs to `ascii', and HANKAKU
belongs to `japanese-jisx0201-kana'.")

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
	  (put-char-code-property ascii 'jisx0208 jisx0208)))
    (if jisx0201
	(progn
	  (put-char-code-property jisx0208 'jisx0201 jisx0201)
	  (put-char-code-property jisx0201 'jisx0208 jisx0208)))))

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
Each element is of the form (ALPHA-NUMERIC ASCII), where ALPHA-NUMERIC
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
    (save-excursion
      (set-buffer buf)
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
    (or (get-char-code-property obj 'ascii)
	(and (not ascii-only)
	     (get-char-code-property obj 'jisx0201))
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

;;;###autoload
(defun japanese-katakana-region (from to &optional hankaku)
  "Convert Japanese `hiragana' chars in the region to `katakana' chars.
Optional argument HANKAKU t means to convert to `hankaku katakana' character
of which charset is `japanese-jisx0201-kana'."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward "\\cH\\|\\cK" nil t)
      (let* ((hira (preceding-char))
	     (kata (japanese-katakana hira hankaku)))
	(if kata
	    (progn
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert kata)))))))

;;;###autoload
(defun japanese-hiragana-region (from to)
  "Convert Japanese `katakana' chars in the region to `hiragana'  chars."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward "\\cK\\|\\ck" nil t)
      (let* ((kata (preceding-char))
	     (hira (japanese-hiragana kata)))
	(if hira
	    (progn
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert hira)))))))

;;;###autoload
(defun japanese-hankaku-region (from to &optional ascii-only)
  "Convert Japanese `zenkaku' chars in the region to `hankaku' chars.
`Zenkaku' chars belong to `japanese-jisx0208'
`Hankaku' chars belong to `ascii' or `japanese-jisx0201-kana'.
Optional argument ASCII-ONLY non-nil means to convert only to ASCII char."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward "\\cj" nil t)
      (let* ((zenkaku (preceding-char))
	     (hankaku (japanese-hankaku zenkaku ascii-only)))
	(if hankaku
	    (progn
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert hankaku)))))))

;;;###autoload
(defun japanese-zenkaku-region (from to)
  "Convert hankaku' chars in the region to Japanese `zenkaku' chars.
`Zenkaku' chars belong to `japanese-jisx0208'
`Hankaku' chars belong to `ascii' or `japanese-jisx0201-kana'."
  (interactive "r")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (while (re-search-forward "\\ca\\|\\ck" nil t)
      (let* ((hankaku (preceding-char))
	     (zenkaku (japanese-zenkaku hankaku)))
	(if zenkaku
	    (progn
	      (delete-region (match-beginning 0) (match-end 0))
	      (insert zenkaku)))))))

;;;###autoload
(defun read-hiragana-string (prompt &optional initial-input)
  "Read a Hiragana string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading."
  (read-multilingual-string prompt initial-input
			    "Japanese" "quail-ja-hiragana"))

;;
(provide 'language/japan-util)

;;; Local Variables:
;;; generated-autoload-file: "../loaddefs.el"
;;; End:
;;; japan-util.el ends here
