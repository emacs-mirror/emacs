;;; ogonek.el --- basic editing commands for Emacs

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: W{\l}odek Bzyl, Ryszard Kubiak
;; Maintainer: rysiek@ipipan.gda.pl (Ryszard Kubiak)
;; Keywords: i18n

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

;; To use this library load it using
;;                 M-x load-library [enter] ogonek
;;              Then, you may get a short info by calling one of
;;                 M-x ogonek-jak        -- in Polish  
;;                 M-x ogonek-how        -- in English  "

(defconst ogonek-name-encoding-alist
  '(("ascii"      . (?A  ?C  ?E  ?L  ?N  ?O  ?S  ?Z  ?Z 
                     ?a  ?c  ?e  ?l  ?n  ?o  ?s  ?z  ?z)) 
    ("iso8859-2"  . (161 198 202 163 209 211 166 172 175 
                     177 230 234 179 241 243 182 188 191))    
    ("mazovia"    . (143 149 144 156 165 163 152 160 161 
                     134 141 145 146 164 162 158 166 167))
    ("windows-EE" . (165 198 202 163 209 211 140 143 175 
                     185 230 234 179 241 243 156 159 191))
    ("windows-PL" . (165 198 202 163 209 211 140 143 175 
                     185 230 234 179 241 243 156 159 191))
    ("latin-2"    . (164 143 168 157 227 224 151 141 189 
                     165 134 169 136 228 162 152 171 190))
    ("CP852"      . (164 143 168 157 227 224 151 141 189 
                     165 134 169 136 228 162 152 171 190))
    ("MeX"        . (129 130 134 138 139 211 145 153 155 
                     161 162 166 170 171 243 177 185 187))
    ("CorelDraw"  . (197 242 201 163 209 211 255 225 237 
                     229 236 230 198 241 243 165 170 186))
    ("Amiga"      . (194 202 203 206 207 211 212 218 219 
                     226 234 235 238 239 243 244 250 251))
    ("Mac"        . (132 140 162 252 193 238 229 143 251 
                     136 141 171 184 196 151 230 144 253))
 ) 
  "The constant `ogonek-name-encoding-alist' is a list of (NAME.LIST) pairs.
Each LIST contains codes for 18 Polish diacritic characters.
The codes are given in the following order:
  Aogonek Cacute Eogonek Lslash Nacute Oacute Sacute Zacute Zdotaccent
  aogonek cacute eogonek lslash nacute oacute sacute zacute zdotaccent.")

; ------ A Little Info in Polish ---------------

(defconst ogonek-informacja
  "   FUNKCJE INTERAKCYJNE UDOST/EPNIANE PRZEZ BIBLIOTEK/E `ogonek'

Je/sli czytasz ten tekst, to albo przegl/adasz plik /zr/od/lowy
biblioteki `ogonek.el', albo wywo/la/le/s polecenie `ogonek-jak'. 
W drugim przypadku mo/zesz usun/a/c tekst z ekranu, stosuj/ac 
polecenie `M-x kill-buffer'.

Niniejsza bibliteka dostarcza funkcji do zmiany kodowania polskich
znak/ow diakrytycznych. Funkcje te mo/zna pogrupowa/c nast/epuj/aco.

 1. Funkcje `ogonek-recode-region' oraz `ogonek-recode-buffer' 
    przekodowu/j/a zaznaczony fragment wzgl/ednie ca/ly buffor.    
    Po wywo/laniu interakcyjnym funkcji zadawane s/a
    pytania o parametry przekodowania, czyli o nazw/e kodowania
    w tek/scie /zr/od/lowym i nazw/e kodowania docelowego. 
    Poni/zsze przyk/lady pokazuj/a, jakich parametr/ow 
    oczekuj/a wymienione funkcje:

      (ogonek-recode-region (poczatek) (koniec)
         nazwa-kodowania-w-tekscie-zrodlowym nazwa-kodowania-docelowa)
      (ogonek-recode-buffer 
         nazwa-kodowania-w-tekscie-zrodlowym nazwa-kodowania-docelowa)    

 2. Funkcje `ogonek-prefixify-region' oraz `ogonek-prefixify-buffer'
    do wprowadzania notacji prefiksowej.

      (ogonek-prefixify-region (poczatek) (koniec)
         nazwa-kodowania-w-tekscie-zrodlowym znak-prefiksu)
      (ogonek-prefixify-buffer 
         nazwa-kodowania-w-tekscie-zrodlowym znak-prefiksu)     

 3. Funkcje `ogonek-deprefixify-region' oraz `ogonek-deprefixify-buffer' 
    do usuwania notacji prefiksowej.
     
      (ogonek-deprefixify-region (poczatek) (koniec)
         znak-prefiksu nazwa-kodowania-docelowa)
      (ogonek-prefixify-buffer 
         znak-prefiksu nazwa-kodowania-docelowa)         

 U/zycie klawisza TAB w trybie interakcyjnym powoduje wy/swietlenie
 listy dopuszczalnych nazw kod/ow, odczytywanych ze sta/lej
 `ogonek-name-encoding-alist'. 

 Funkcje biblioteki odwo/luj/a si/e do pi/eciu zmiennych, kt/ore 
 przechowuj/a podpowiedzi do zadawanych pyta/n. Nazwy tych zmiennych
 oraz ich warto/sci domy/slne s/a nast/epuj/ace: 

   ogonek-from-encoding           iso8859-2
   ogonek-to-encoding             mazovia
   ogonek-prefix-char              /
   ogonek-prefix-from-encoding    iso8859-2
   ogonek-prefix-to-encoding      iso8859-2

 Powy/zsze warto/sci domy/slne mo/zna zmieni/c przez umieszczenie w pliku
 konfiguracyjnym `~/.emacs' odpowiednich przypisa/n, na przyk/lad:

 (setq ogonek-prefix-char ?/)
 (setq ogonek-prefix-to-encoding \"iso8859-2\")

 Zamiast wczytywania ca/lej biblioteki `ogonek.el' mo/zna w pliku
 `~/.emacs' za/z/ada/c wczytania wybranych funkcji i to dopiero w
 chwili ich wywo/lania:

 (autoload 'ogonek-jak \"ogonek\")
 (autoload 'ogonek-recode-region \"ogonek\")
 (autoload 'ogonek-prefixify-region \"ogonek\")
 (autoload 'ogonek-deprefixify-region \"ogonek\")

 Cz/esto wyst/epuj/ace kombinacje wywo/la/n funkcji mo/zna dla wygody
 skr/oci/c i przypisa/c klawiszom. Oto praktyczne przyk/lady:

 (defun deprefixify-iso8859-2-region ()
   (interactive \"*\")
   (ogonek-deprefixify-region 
    (region-beginning) (region-end) ?/ \"iso8859-2\"))
 (global-set-key \"\\C-cd\" 'deprefixify-iso8859-2-region) ; ctrl-c d

 (defun mazovia-to-iso8859-2 () 
   (interactive \"*\")
   (ogonek-recode-region \"mazovia\" \"iso8859-2\"))
 (global-set-key \"\\C-cr\" 'mazovia-to-iso8859-2) ; ctrl-c r

 (defun prefixify-iso8859-2-region ()
   (interactive \"*\")
   (ogonek-prefixify-region 
    (region-beginning) (region-end) \"iso8859-2\" ?/))
 (global-set-key \"\\C-cp\" 'prefixify-iso8859-2-region) ; ctrl-c p

 Ka/zd/a operacj/e przekodowania mo/zna w ca/lo/sci odwo/la/c
 przez wykonanie polecenia `undo'.")

(defun ogonek-jak ()
  "Display the string constant `ogonek-informacja' 
by inserting it into an auxiliary *ogonek-jak* buffer."
  (interactive)
  (set-buffer  (get-buffer-create " *ogonek-jak*"))
  (insert ogonek-informacja)
  (switch-to-buffer " *ogonek-jak*")
  (beginning-of-buffer))

; ------ A Little Info in English --------

(defconst ogonek-information
  "   INTERACTIVE FUNCTIONS PROVIDED BY THE LIBRARY `ogonek'.

If you read this text then you are either looking at the library's
source text or you have called the `ogonek-howto' command. In the
latter case you may remove this text using `M-x kill-buffer'.

The library provides functions for changing the encoding of Polish
diacritic characters, the ones with an `ogonek' below or above them.
The functions come in the following gropus.

 1. Functions `ogonek-recode-region' and `ogonek-recode-buffer' to
    change between one-character encodings, such as `iso-8859-2',
    `mazovia', plain `ascii' or `TeX'. As the names suggest you may
    recode either the entire current buffer or just a marked region 
    of it. You may use these functions interactively as commands. Once 
    you call a command you will be asked about the code used in 
    the source text and the target encoding, the one you want to get. 
    The following examples show a non-interactive use of the functions 
    in a program. They also illustrtate what parameters the functions 
    expect:

      (ogonek-recode-region (region-beginning) (region-end)
                                from-code-name to-code-name)
      (ogonek-recode-buffer from-code-name to-code-name)         

 2. Functions `ogonek-prefixify-region' and `ogonek-prefixify-buffer'.
    for introducing prefix notation:

      (ogonek-prefixify-region (region-beginning) (region-end)
                                     from-code-name prefix-char)
      (ogonek-prefixify-buffer from-code-name prefix-char)         
  
 3. Functions  `ogonek-deprefixify-region' and `ogonek-deprefixify-buffer' 
    for removing prefix notation:
     
      (ogonek-deprefixify-region (region-beginning) (region-end)
                                   prefix-char to-code-name)
      (ogonek-prefixify-buffer prefix-char to-code-name)         

 The use of the TAB character in interactive makes `emacs' display
 the list of encodings recognized by the library - the code names
 are stored in the constant  `ogonek-name-encoding-alist'

 The functions of the library refer to five variables that keep
 hints to the questions asked. The names of those variables as well
 as their default values are:
 
   ogonek-from-encoding           iso8859-2
   ogonek-to-encoding             mazovia
   ogonek-prefix-char              /
   ogonek-prefix-from-encoding    iso8859-2
   ogonek-prefix-to-encoding      iso8859-2

 The above default values can be changed by placing appropriate settings 
 in the '~/.emacs' file:

 (setq ogonek-prefix-char ?/)
 (setq ogonek-prefix-to-encoding \"iso8859-2\")

 Instead of loading the whole library `ogonek.el' it may be better to
 autoload chosen functions in `~/.emacs':

 (autoload 'ogonek-jak \"ogonek\")
 (autoload 'ogonek-recode-region \"ogonek\")
 (autoload 'ogonek-prefixify-region \"ogonek\")
 (autoload 'ogonek-deprefixify-region \"ogonek\")

 The most frequent function calls can be abbreviated and assigned to
 keyboard keys. Here are a few practical examples:

 (setq ogonek-from-code-name \"iso8859-2\")
 (setq ogonek-to-code-name \"mazovia\")
 (setq ogonek-prefix-char ?/)
 (setq ogonek-prefix-from-code-name \"iso8859-2\")
 (setq ogonek-prefix-to-code-name \"iso8859-2\")

 (defun deprefixify-iso8859-2-region ()
   (interactive \"*\")
   (ogonek-deprefixify-region 
    (region-beginning) (region-end) ?/ \"iso8859-2\"))
 (global-set-key \"\\C-cd\" 'deprefixify-iso8859-2-region) ; ctrl-c d

 (defun mazovia-to-iso8859-2 () 
   (interactive \"*\")
   (ogonek-recode-region \"mazovia\" \"iso8859-2\"))
 (global-set-key \"\\C-cr\" 'mazovia-to-iso8859-2) ; ctrl-c r

 (defun prefixify-iso8859-2-region ()
   (interactive \"*\")
   (ogonek-prefixify-region 
    (region-beginning) (region-end) \"iso8859-2\" ?/))
 (global-set-key \"\\C-cp\" 'prefixify-iso8859-2-region) ; ctrl-c p

 Each recoding opertation can be called off by executing the `undo'
 command.")

(defun ogonek-how ()
  "Display the string constant `ogonek-information' 
by inserting it into an auxiliary *recode-help* buffer."
  (interactive "*")
  (set-buffer  (get-buffer-create " *ogonek-help*"))
  (insert ogonek-information)
  (switch-to-buffer " *ogonek-help*")
  (beginning-of-buffer))

;; ------ Variables for keeping hints to the questions ---------

(defvar ogonek-from-encoding "iso8859-2"
  "*Encoding in the source file of recoding.")
(defvar ogonek-to-encoding "ascii"
  "*Encoding in the target file of recoding.")
(defvar ogonek-prefix-char ?/
  "*Prefix character for prefix encodings.") 
(defvar ogonek-prefix-from-encoding "iso8859-2"
  "*Encoding in the source file subject to prefixifation.") 
(defvar ogonek-prefix-to-encoding "iso8859-2"
  "*Encoding in the target file subject to deprefixifation.") 

;; ------- Utilities for reading function parameters -------------

(defun ogonek-read-encoding (prompt default-name-var)
  "Change with completion based on alist `ogonek-name-encoding-alist'."
 (let ((encoding 
        (completing-read 
         (format "%s (default %s): " prompt (eval default-name-var))
         ogonek-name-encoding-alist nil t)))
  ; set the new default name to be the one just read
   (set default-name-var 
       (if (string= encoding "") (eval default-name-var) encoding))  
  ; return the new default as the name you read
  (eval default-name-var)))

(defun ogonek-read-prefix (prompt default-prefix-var)
  "Change prefix."
  (let ((prefix-string 
         (read-string 
          (format "%s (default %s): " prompt 
                  (char-to-string (eval default-prefix-var))))))
    (if (> (length prefix-string) 1)
        (error "! Only one character expected.") 
      ; set the default prefix character to the one just read
      (set default-prefix-var
           (if (string= prefix-string "") 
             (eval default-prefix-var) 
           (string-to-char prefix-string)))
      ; return the new default prefix as the code you read)
      (eval default-prefix-var))))

(defun ogonek-lookup-encoding (encoding)
  "Pick up an association for `encoding' in `ogonek-name-encoding-alist'.
Before returning a result test whether it has been properly set
which should be true if the encoding is one of those in
`ogonek-name-encoding-alist'"
  (let ((code-list (assoc encoding ogonek-name-encoding-alist)))
    (if (null code-list)
      (error "! Name `%s' not known in `ogonek-name-encoding-alist'."
               encoding)
      (cdr code-list))))

; -------  A utility for zipping two lists -----------

(defun ogonek-zip-lists (xs ys)
  "Build a list of pairs with elements from lists `xs' and `ys'. 
We assume that `xs' and `ys' are of the same length."
  (let ((pairs nil)) 
    (while xs
      (setq pairs (cons (cons (car xs) (car ys)) pairs))
      (setq xs (cdr xs))
      (setq ys (cdr ys)))
;   `pairs' are the function's result
    pairs))

; -------- Dealing with one-character cencodings -------

(defun ogonek-build-table (recoding-pairs) 
  "Build a table nedeed by emacs's `translate-region' function.
The  `recoding-pairs' argument is a list of pairs of characters.
By using the built-in `translate-region' function
we gain better performance compared to converting characters
by a hand-written routine as it is done for prefix encodings."
  (let ((table (make-string 256 0))
        (i 0))
    (while (< i 256) 
      (aset table i i) 
      (setq i (1+ i)))
    ; make changes in `table' according to `recoding-pairs'
    (while recoding-pairs
      (aset table (car (car recoding-pairs)) (cdr (car recoding-pairs)))
      (setq recoding-pairs (cdr recoding-pairs)))
    ; return the table just built
    table))

(defun ogonek-recode-region (start end from-encoding to-encoding) 
  "This function recodes text in a region delineated by the current-mark 
and the current point according to the defaults set by the variables
`ogonek-from-encoding' and `ogonek-to-encoding'."
  (interactive (progn (barf-if-buffer-read-only)
                (list 
                (region-beginning) 
                (region-end)
                (ogonek-read-encoding "From code" 'ogonek-from-encoding)
                (ogonek-read-encoding "To code" 'ogonek-to-encoding))))
  (save-excursion
    (translate-region 
     start end
     (ogonek-build-table 
      (ogonek-zip-lists
       (ogonek-lookup-encoding from-encoding)
       (ogonek-lookup-encoding to-encoding))))))

(defun ogonek-recode-buffer (from-encoding to-encoding)
  "Call `ogonek-region' on the entire buffer."
  (interactive (progn (barf-if-buffer-read-only)
                (list 
                (ogonek-read-encoding "From code" 'ogonek-from-encoding)
                (ogonek-read-encoding "To code" 'ogonek-to-encoding))))
  (ogonek-recode-region 
   (point-min) (point-max) from-encoding to-encoding))

; --------------------------------
; Recoding with prefix notation 
; --------------------------------

(defconst prefix-code '(?A  ?C  ?E  ?L  ?N  ?O  ?S  ?X  ?Z 
                        ?a  ?c  ?e  ?l  ?n  ?o  ?s  ?x  ?z)) 
    
(defun ogonek-prefixify-region (start end from-encoding prefix-char)
  "Replace -- in the region delineated by the current-mark 
and the point -- each character from `ogonek-from-encoding'
by two characters: `ogonek-prefix-char' and the corresponding 
character from the `prefix' list. Double the character 
`ogonek-prefix-char'"
  (interactive (progn (barf-if-buffer-read-only)
    (list 
    (region-beginning) 
    (region-end) 
    (ogonek-read-encoding "From code" 'ogonek-prefix-from-encoding)
    (ogonek-read-prefix "Prefix character" 'ogonek-prefix-char))))
  (let* 
      ((from-code (ogonek-lookup-encoding from-encoding))
       (to-code prefix-code)
       (recoding-pairs  ; we add `ogonek-prefix-char' for doubling
        (ogonek-zip-lists 
         (cons prefix-char from-code)  
         (cons prefix-char to-code))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((pair (assoc (following-char) recoding-pairs)))
          (if (null pair)
              ; not a Polish character -- skip it
              (forward-char 1)
            ; Polish character -- replace it by a two characters
            (delete-char 1)
            (insert ogonek-prefix-char)
            (insert (cdr pair))
            ; the region is now one character longer
            (setq end (1+ end))))))))

(defun ogonek-prefixify-buffer (from-encoding prefix-char)
  "Call `ogonek-prefixify-region' on the entire buffer."
  (interactive (progn (barf-if-buffer-read-only)
    (list 
     (ogonek-read-encoding "From code" 'ogonek-prefix-from-encoding)
     (ogonek-read-prefix "Prefix character" 'ogonek-prefix-char))))
  (ogonek-prefixify-region 
   (point-min) (point-max) from-encoding prefix-char))

(defun ogonek-deprefixify-region (start end prefix-char to-encoding)
  "Replace `ogonek-prefix-char' followed by a character from 
the `prefix' list or another `ogonek-prefix-char' by 
the corresponding character from `ogonek-from-encoding' 
or by one `ogonek-prefix-char'."
  (interactive (progn (barf-if-buffer-read-only)
                (list (region-beginning) 
                      (region-end)
                      (ogonek-read-prefix 
                        "Prefix character" 'ogonek-prefix-char)
                      (ogonek-read-encoding 
                       "To code" 'ogonek-prefix-to-encoding))))
  (let* 
      ((from-code prefix-code)
       (to-code (ogonek-lookup-encoding to-encoding))
       (recoding-pairs 
        (ogonek-zip-lists 
          (cons prefix-char from-code)  
          (cons prefix-char to-code))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (forward-char 1)
       (if (or (not (= (preceding-char) prefix-char)) (= (point) end))
           ; non-prefix character or the end-of-region -- do nothing
           ()
         ; now, we can check the next character
         (let ((pair (assoc (following-char) recoding-pairs)))
           (if (null pair) 
               ; `following-char' is not a Polish character nor it is
               ;  `prefix-char' since the one is among `recoding-pairs'
               (forward-char 1)     
           ; else prefix followed by a Polish character has been found
           ; replace it by the corresponding Polish character
           (backward-char 1)
           (delete-char 2)
           (insert (cdr pair))
           ; the region got shorter by one character
           (setq end (1- end)))))))))

(defun ogonek-deprefixify-buffer (prefix-char to-encoding)
  "Call `ogonek-deprefixify-region' on the entire buffer."
  (interactive (progn (barf-if-buffer-read-only)
    (list 
     (ogonek-read-prefix "Prefix character" 'ogonek-prefix-char)
     (ogonek-read-encoding "To code" 'ogonek-prefix-to-encoding))))
  (ogonek-deprefixify-region 
   (point-min) (point-max) prefix-char to-encoding))

(provide 'ogonek)

;;; ogonek.el ends here
