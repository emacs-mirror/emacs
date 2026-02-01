;; Emacs versions when certain symbols and variables were first introduced,
;; for use in `describe-function'.
;;
;; This file is used to explicitly override the heuristic scraping NEWS*
;; files, when that would result in misleading information.
;;
;; It should contain a single list of (VERSION TYPE SYMBOL), where
;; VERSION is the Emacs version when SYMBOL was introduced as a TYPE,
;; TYPE being `fun' or `var'.

(
 ("31.1" fun any)
 ("31.1" fun all)
 ("30.1" fun dired-click-to-select-mode)
 ("30.1" var dired-click-to-select-mode)
 ("29.1" fun plistp)
 ("29.1" fun help-key)
 ("28.1" fun always)
 ("27.1" fun project-files)
 ("26.1" fun flymake--diag-region)
 ("26.1" fun flymake-make-diagnostic)
 ("26.1" fun when-let*)
 ("26.1" fun and-let*)
 ("26.1" fun if-let*)
 ("26.1" fun caaar)
 ("26.1" fun caadr)
 ("26.1" fun cadar)
 ("26.1" fun caddr)
 ("26.1" fun cdaar)
 ("26.1" fun cdadr)
 ("26.1" fun cddar)
 ("26.1" fun cdddr)
 ("26.1" fun caaaar)
 ("26.1" fun caaadr)
 ("26.1" fun caadar)
 ("26.1" fun caaddr)
 ("26.1" fun cadaar)
 ("26.1" fun cadadr)
 ("26.1" fun caddar)
 ("26.1" fun cadddr)
 ("26.1" fun cdaaar)
 ("26.1" fun cdaadr)
 ("26.1" fun cdadar)
 ("26.1" fun cdaddr)
 ("26.1" fun cddaar)
 ("26.1" fun cddadr)
 ("26.1" fun cdddar)
 ("26.1" fun cddddr)
 ("24.4" fun set-transient-map)
 ("24.3" fun cl-defun)
 ("22.1" fun clear-string)
 ("22.1" fun version=)
 ("22.1" fun version<)
 ("22.1" fun version<=)
 ("22.1" fun read-number)
 ("21.1" var text-property-default-nonsticky)

;; Since much of early Emacs source history is lost, these versions are
;; conservative estimates: the actual version of first appearance may very
;; well be much earlier.
;; 13.8 may be the earliest surviving version with source code, although
;; damaged.  See
;; https://github.com/larsbrinkhoff/emacs-history/tree/sources/decuslib.com/decus/vax85b/gnuemax

 ("19.34" fun make-directory)
 ("19.7" fun defsubst)
 ("18.59" fun current-global-map)
 ("18.59" fun mark)
 ("13.8" fun nthcdr)
 ("13.8" fun nreverse)
 ("13.8" fun let*)
 ("13.8" fun rassq)
 ("13.8" fun >=)
 ("13.8" fun transpose-sexps)
 ("13.8" fun buffer-modified-p)
 ("13.8" fun current-column)
 ("13.8" fun downcase)
 ("13.8" fun previous-line)
 ("13.8" fun catch)
 ("13.8" fun throw)
 ("13.8" fun count-lines)
 )
