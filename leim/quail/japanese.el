;;; quail/japanese.el --- Quail package for inputting Japanese

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, input method, Japanese

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

(require 'quail)
(require 'kkc)

;; Update Quail translation region for Japanese bizarre translation
;; rules.
(defun quail-japanese-update-translation (control-flag)
  (cond ((eq control-flag t)
	 (insert quail-current-str)
	 (quail-terminate-translation))
	((null control-flag)
	 (if (/= (aref quail-current-key 0) ?q)
	     (insert (or quail-current-str quail-current-key))))
	(t				; i.e. (numberp control-flag)
	 (cond ((= (aref quail-current-key 0) ?n)
		(insert ?ん))
	       ((= (aref quail-current-key 0) (aref quail-current-key 1))
		(insert ?っ))
	       (t
		(insert (aref quail-current-key 0))))
	 (setq unread-command-events
	       (list (aref quail-current-key control-flag)))
	 (quail-terminate-translation))))
	 
;; Flag to control the behavior of `quail-japanese-toggle-kana'.
(defvar quail-japanese-kana-state nil)
(make-variable-buffer-local 'quail-japanese-kana-state)

;; Convert Hiragana <-> Katakana in the current translation region.
(defun quail-japanese-toggle-kana ()
  (interactive)
  (if (quail-point-in-conversion-region)
      (let ((start (overlay-start quail-conv-overlay))
	    (end (overlay-end quail-conv-overlay)))
	(setq quail-japanese-kana-state
	      (if (eq last-command this-command)
		  (not quail-japanese-kana-state)))
	(if quail-japanese-kana-state
	    (japanese-hiragana-region start end)
	  (japanese-katakana-region start end))
	(goto-char (overlay-end quail-conv-overlay))
	(setq quail-exit-conversion-mode t))
    ;; When this command is invoked, the point is not in a valid
    ;; region.  Try the event which invoked this command again out of
    ;; conversion mode.
    (setq unread-command-events (list last-command-event))
    (throw 'exit nil)))

;; Convert Hiragana in the current translation region to Kanji by KKC
;; (Kana Kanji Converter) utility.
(defun quail-japanese-kanji-kkc ()
  (interactive)
  (if (quail-point-in-conversion-region)
      (let ((from (overlay-start quail-conv-overlay))
	    (to (overlay-end quail-conv-overlay))
	    newfrom)
	(delete-overlay quail-overlay)
	(delete-overlay quail-conv-overlay)
	(unwind-protect
	    (setq newfrom (kkc-region from to))
	  ;; Activate the original (or shrinked) conversion region
	  ;; again.	  
	  (if newfrom
	      ;; `kkc-region' is canceled.  
	      (move-overlay quail-conv-overlay newfrom (point))
	    ;; `kkc-region' is terminated normally.
	    (move-overlay quail-conv-overlay from (point))
	    (throw 'exit nil))))
    ;; When this command is invoked, the point is not in a valid
    ;; region.  Try the event which invoked this command again out of
    ;; conversion mode.
    (setq unread-command-events (list last-command-event))
    (throw 'exit nil)))

(defun quail-japanese-self-insert-and-switch-to-alpha (key idx)
  (quail-delete-region)
  (setq unread-command-events (list (aref key (1- idx))))
  (quail-japanese-switch-package "q" 1))

(defvar quail-japanese-switch-table
  '((?z . "japanese-zenkaku")
    (?k . "japanese-hankaku-kana")
    (?h . "japanese")
    (?q . ("japanese-ascii"))))

(defvar quail-japanese-package-saved nil)
(make-variable-buffer-local 'quail-japanese-package-saved)
(put 'quail-japanese-package-saved 'permanent-local t)

(defun quail-japanese-switch-package (key idx)
  (let ((pkg (cdr (assq (aref key (1- idx)) quail-japanese-switch-table))))
    (if (null pkg)
	(error "No package to be switched")
      (quail-delete-region)
      (if (stringp pkg)
	  (select-input-method pkg)
	(if (string= (car pkg) current-input-method)
	    (if quail-japanese-package-saved
		(select-input-method quail-japanese-package-saved))
	  (setq quail-japanese-package-saved current-input-method)
	  (select-input-method (car pkg))))
      (throw 'quail-tag nil))))

(quail-define-package
 "japanese" "Japanese" "Aあ"
 nil
 "Romaji -> Hiragana -> Kanji&Kana
---- Special key bindings ----
qq:	toggle between input methods `japanese' and `japanese-ascii'
qz:	use `japanese-zenkaku' package, \"qh\" puts you back to `japanese'
K:	convert to Katakana
SPC:	convert to Kanji&Kana
z:	insert one Japanese symbol according to a key which follows
"
 nil t t nil nil nil nil nil
 'quail-japanese-update-translation
 '(("K" . quail-japanese-toggle-kana)
   (" " . quail-japanese-kanji-kkc)
   ("\C-m" . quail-no-conversion)
   ([return] . quail-no-conversion))
 )

(quail-define-rules

( "a" "あ") ( "i" "い") ( "u" "う") ( "e" "え") ( "o" "お")
("ka" "か") ("ki" "き") ("ku" "く") ("ke" "け") ("ko" "こ")
("sa" "さ") ("si" "し") ("su" "す") ("se" "せ") ("so" "そ")
("ta" "た") ("ti" "ち") ("tu" "つ") ("te" "て") ("to" "と")
("na" "な") ("ni" "に") ("nu" "ぬ") ("ne" "ね") ("no" "の")
("ha" "は") ("hi" "ひ") ("hu" "ふ") ("he" "へ") ("ho" "ほ")
("ma" "ま") ("mi" "み") ("mu" "む") ("me" "め") ("mo" "も")
("ya" "や")             ("yu" "ゆ")             ("yo" "よ")
("ra" "ら") ("ri" "り") ("ru" "る") ("re" "れ") ("ro" "ろ")
("la" "ら") ("li" "り") ("lu" "る") ("le" "れ") ("lo" "ろ")
("wa" "わ") ("wi" "ゐ") ("wu" "う") ("we" "ゑ") ("wo" "を")
("n'" "ん")	 			     
("ga" "が") ("gi" "ぎ") ("gu" "ぐ") ("ge" "げ") ("go" "ご")
("za" "ざ") ("zi" "じ") ("zu" "ず") ("ze" "ぜ") ("zo" "ぞ")
("da" "だ") ("di" "ぢ") ("du" "づ") ("de" "で") ("do" "ど")
("ba" "ば") ("bi" "び") ("bu" "ぶ") ("be" "べ") ("bo" "ぼ")
("pa" "ぱ") ("pi" "ぴ") ("pu" "ぷ") ("pe" "ぺ") ("po" "ぽ")

("kya" ["きゃ"]) ("kyu" ["きゅ"]) ("kye" ["きぇ"]) ("kyo" ["きょ"])
("sya" ["しゃ"]) ("syu" ["しゅ"]) ("sye" ["しぇ"]) ("syo" ["しょ"])
("sha" ["しゃ"]) ("shu" ["しゅ"]) ("she" ["しぇ"]) ("sho" ["しょ"])
("cha" ["ちゃ"]) ("chu" ["ちゅ"]) ("che" ["ちぇ"]) ("cho" ["ちょ"])
("tya" ["ちゃ"]) ("tyu" ["ちゅ"]) ("tye" ["ちぇ"]) ("tyo" ["ちょ"])
("nya" ["にゃ"]) ("nyu" ["にゅ"]) ("nye" ["にぇ"]) ("nyo" ["にょ"])
("hya" ["ひゃ"]) ("hyu" ["ひゅ"]) ("hye" ["ひぇ"]) ("hyo" ["ひょ"])
("mya" ["みゃ"]) ("myu" ["みゅ"]) ("mye" ["みぇ"]) ("myo" ["みょ"])
("rya" ["りゃ"]) ("ryu" ["りゅ"]) ("rye" ["りぇ"]) ("ryo" ["りょ"])
("lya" ["りゃ"]) ("lyu" ["りゅ"]) ("lye" ["りぇ"]) ("lyo" ["りょ"])
("gya" ["ぎゃ"]) ("gyu" ["ぎゅ"]) ("gye" ["ぎぇ"]) ("gyo" ["ぎょ"])
("zya" ["じゃ"]) ("zyu" ["じゅ"]) ("zye" ["じぇ"]) ("zyo" ["じょ"])
("jya" ["じゃ"]) ("jyu" ["じゅ"]) ("jye" ["じぇ"]) ("jyo" ["じょ"])
( "ja" ["じゃ"]) ( "ju" ["じゅ"]) ( "je" ["じぇ"]) ( "jo" ["じょ"])
("bya" ["びゃ"]) ("byu" ["びゅ"]) ("bye" ["びぇ"]) ("byo" ["びょ"])
("pya" ["ぴゃ"]) ("pyu" ["ぴゅ"]) ("pye" ["ぴぇ"]) ("pyo" ["ぴょ"])

("kwa" ["くゎ"]) ("kwi" ["くぃ"]) ("kwe" ["くぇ"]) ("kwo" ["くぉ"])
("tsa" ["つぁ"]) ("tsi" ["つぃ"]) ("tse" ["つぇ"]) ("tso" ["つぉ"])
( "fa" ["ふぁ"]) ( "fi" ["ふぃ"]) ( "fe" ["ふぇ"]) ( "fo" ["ふぉ"])
("gwa" ["ぐゎ"]) ("gwi" ["ぐぃ"]) ("gwe" ["ぐぇ"]) ("gwo" ["ぐぉ"])

("dyi" ["でぃ"]) ("dyu" ["どぅ"]) ("dye" ["でぇ"]) ("dyo" ["どぉ"])
("xwi" ["うぃ"])                  ("xwe" ["うぇ"]) ("xwo" ["うぉ"])

("shi" "し") ("tyi" ["てぃ"]) ("chi" "ち") ("tsu" "つ") ("ji" "じ")
("fu"  "ふ")
("ye" ["いぇ"])

("va" ["ヴぁ"]) ("vi" ["ヴぃ"]) ("vu" "ヴ") ("ve" ["ヴぇ"]) ("vo" ["ヴぉ"])

("xa"  "ぁ") ("xi"  "ぃ") ("xu"  "ぅ") ("xe"  "ぇ") ("xo"  "ぉ")
("xtu" "っ") ("xya" "ゃ") ("xyu" "ゅ") ("xyo" "ょ") ("xwa" "ゎ")
("xka" "ヵ") ("xke" "ヶ")

("1" "１") ("2" "２") ("3" "３") ("4" "４") ("5" "５")
("6" "６") ("7" "７") ("8" "８") ("9" "９") ("0" "０")

("!" "！") ("@" "＠") ("#" "＃") ("$" "＄") ("%" "％")
("^" "＾") ("&" "＆") ("*" "＊") ("(" "（") (")" "）")
("-" "ー") ("=" "＝") ("`" "｀") ("\\" "￥") ("|" "｜")
("_" "＿") ("+" "＋") ("~" "￣") ("[" "「") ("]" "」")
("{" "｛") ("}" "｝") (":" "：") (";" "；") ("\""  "”")
("'" "’") ("." "。") ("," "、") ("<" "＜") (">" "＞")
("?" "？") ("/" "／")

("z1" "○") ("z!" "●")
("z2" "▽") ("z@" "▼")
("z3" "△") ("z#" "▲")
("z4" "□") ("z$" "■")
("z5" "◇") ("z%" "◆")
("z6" "☆") ("z^" "★")
("z7" "◎") ("z&" "£")
("z8" "¢") ("z*" "×")
("z9" "♂") ("z(" "【")
("z0" "♀") ("z)" "】")
("z-" "〜") ("z_" "∴")
("z=" "≠") ("z+" "±")
("z\\" "＼") ("z|" "‖")
("z`" "´") ("z~" "¨")

("zq" "《") ("zQ" "〈")
("zw" "》") ("zW" "〉")
("zr" "々") ("zR" "仝")
("zt" "〆") ("zT" "§")
("zp" "〒") ("zP" "↑")
("z[" "『") ("z{" "〔")
("z]" "』") ("z}" "〕")

("zs" "ヽ") ("zS" "ヾ")
("zd" "ゝ") ("zD" "ゞ")
("zf" "〃") ("zF" "→")
("zg" "‐") ("zG" "―")
("zh" "←")
("zj" "↓")
("zk" "↑")
("zl" "→")
("z;" "゛") ("z:" "゜")
("z\'" "‘") ("z\"" "“")

("zx" ":-") ("zX" ":-)")
("zc" "〇") ("zC" "℃")
("zv" "※") ("zV" "÷")
("zb" "°") ("zB" "←")
("zn" "′") ("zN" "↓")
("zm" "″") ("zM" "〓")
("z," "‥") ("z<" "≦")
("z." "…") ("z>" "≧")
("z/" "・") ("z?" "∞")

("\\\\" quail-japanese-self-insert-and-switch-to-alpha)
("{{" quail-japanese-self-insert-and-switch-to-alpha)
("}}" quail-japanese-self-insert-and-switch-to-alpha)

("qq" quail-japanese-switch-package)
("qz" quail-japanese-switch-package)

)

(quail-define-package
 "japanese-ascii" "Japanese" "Aa"
 nil
 "Temporary ASCII input mode while using Quail package `japanese'
Type \"qq\" to go back to previous package."
 nil t t)

(quail-define-rules ("qq" quail-japanese-switch-package))

(quail-define-package
 "japanese-zenkaku" "Japanese" "Ａ"
 nil
 "Japanese zenkaku alpha numeric character input method
---- Special key bindings ----
qq:	toggle between `japanese-zenkaku' and `japanese-ascii'
qh:	use `japanese' package, \"qz\" puts you back to `japanese-zenkaku'
"
 nil t t)

(quail-define-rules

(" " "　") ("!" "！") ("\"" "″") ("#" "＃")
("$" "＄") ("%" "％") ("&" "＆") ("'" "′")
("(" "（") (")" "）") ("*" "＊") ("+" "＋")
("," "，") ("-" "−") ("." "．") ("/" "／")
("0" "０") ("1" "１") ("2" "２") ("3" "３")
("4" "４") ("5" "５") ("6" "６") ("7" "７")
("8" "８") ("9" "９") (":" "：") (";" "；")
("<" "＜") ("=" "＝") (">" "＞") ("?" "？")
("@" "＠") ("A" "Ａ") ("B" "Ｂ") ("C" "Ｃ")
("D" "Ｄ") ("E" "Ｅ") ("F" "Ｆ") ("G" "Ｇ")
("H" "Ｈ") ("I" "Ｉ") ("J" "Ｊ") ("K" "Ｋ")
("L" "Ｌ") ("M" "Ｍ") ("N" "Ｎ") ("O" "Ｏ")
("P" "Ｐ") ("Q" "Ｑ") ("R" "Ｒ") ("S" "Ｓ")
("T" "Ｔ") ("U" "Ｕ") ("V" "Ｖ") ("W" "Ｗ")
("X" "Ｘ") ("Y" "Ｙ") ("Z" "Ｚ") ("[" "［")
("\\" "￥") ("]" "］") ("^" "＾") ("_" "＿")
("`" "‘") ("a" "ａ") ("b" "ｂ") ("c" "ｃ")
("d" "ｄ") ("e" "ｅ") ("f" "ｆ") ("g" "ｇ")
("h" "ｈ") ("i" "ｉ") ("j" "ｊ") ("k" "ｋ")
("l" "ｌ") ("m" "ｍ") ("n" "ｎ") ("o" "ｏ")
("p" "ｐ") ("q" "ｑ") ("r" "ｒ") ("s" "ｓ")
("t" "ｔ") ("u" "ｕ") ("v" "ｖ") ("w" "ｗ")
("x" "ｘ") ("y" "ｙ") ("z" "ｚ") ("{" "｛")
("|" "｜") ("}" "｝") ("~" "〜") 

("qq" quail-japanese-switch-package)
("qh" quail-japanese-switch-package)
)

(defun quail-japanese-hankaku-update-translation (control-flag)
  (cond ((eq control-flag t)
	 (insert (japanese-hankaku quail-current-str))
	 (quail-terminate-translation))
	((null control-flag)
	 (insert (if quail-current-str
		     (japanese-hankaku quail-current-str)
		   quail-current-key)))
	(t				; i.e. (numberp control-flag)
	 (cond ((= (aref quail-current-key 0) ?n)
		(insert ?ﾝ))
	       ((= (aref quail-current-key 0) (aref quail-current-key 1))
		(insert ?ｯ))
	       (t
		(insert (aref quail-current-key 0))))
	 (setq unread-command-events
	       (list (aref quail-current-key control-flag)))
	 (quail-terminate-translation))))


(quail-define-package
 "japanese-hankaku-kana"
 "Japanese" "ｱ"
 nil
 "Japanese hankaku katakana input method by Roman transliteration
---- Special key bindings ----
qq:	toggle between `japanese-hankaku-kana' and `japanese-ascii'
"
 nil t t nil nil nil nil nil
 'quail-japanese-hankaku-update-translation)

;; Use the same map as that of `japanese'.
(setcar (cdr (cdr quail-current-package))
	(nth 2 (assoc "japanese" quail-package-alist)))

(quail-define-package
 "japanese-hiragana" "Japanese" "あ"
 nil
 "Japanese hiragana input method by Roman transliteration"
 nil t t nil nil nil nil nil
 'quail-japanese-update-translation)

;; Use the same map as that of `japanese'.
(setcar (cdr (cdr quail-current-package))
	(nth 2 (assoc "japanese" quail-package-alist)))

;; Update Quail translation region while converting Hiragana to Katakana.
(defun quail-japanese-katakana-update-translation (control-flag)
  (cond ((eq control-flag t)
	 (insert (japanese-katakana quail-current-str))
	 (quail-terminate-translation))
	((null control-flag)
	 (insert (if quail-current-str
		     (japanese-katakana quail-current-str)
		   quail-current-key)))
	(t				; i.e. (numberp control-flag)
	 (cond ((= (aref quail-current-key 0) ?n)
		(insert ?ン))
	       ((= (aref quail-current-key 0) (aref quail-current-key 1))
		(insert ?ッ))
	       (t
		(insert (aref quail-current-key 0))))
	 (setq unread-command-events
	       (list (aref quail-current-key control-flag)))
	 (quail-terminate-translation))))

(quail-define-package 
 "japanese-katakana" "Japanese" "ア"
 nil
 "Japanese katakana input method by Roman transliteration"
 nil t t nil nil nil nil nil
 'quail-japanese-katakana-update-translation)

;; Use the same map as that of `japanese'.
(setcar (cdr (cdr quail-current-package))
	(nth 2 (assoc "japanese" quail-package-alist)))
