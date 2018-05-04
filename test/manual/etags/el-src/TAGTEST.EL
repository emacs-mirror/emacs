(foo::defmumble bletch beuarghh)
(defun foo==bar () (message "hi"))  ; Bug#5624
;;; Ctags test file for lisp mode.

;; from emacs/lisp/delsel.el:76:
(defalias 'pending-delete-mode 'delete-selection-mode)

;; compare with:
(defalias (quote explicitly-quoted-pending-delete-mode) 'delete-selection-mode)

;;
;; Output from original ctags:
;;
;'pending-delete-mode	tagstest.el	/^(defalias 'pending-delete-mode 'delete-selection-m/
;(quote	tagstest.el	/^(defalias (quote explicitly-quoted-pending-delete-/
;;
;; Output from ctags with my patch in L-getit():
;;
;pending-delete-mode	tagstest.el	/^(defalias 'pending-delete-mode 'delete-selection-m/
;explicitly-quoted-pending-delete-mode	tagstest.el	/^(defalias (quote explicitly-quoted-pending-delete-/
;; Output from original etags:
;;
;tagstest.el,61
;(defalias 'pending-delete-mode 4,68
;(defalias (quote 7,141
;;
;; Output from patched etags:
;;
;tagstest.el,99
;(defalias 'pending-delete-mode 4,68
;(defalias (quote explicitly-quoted-pending-delete-mode)7,141
;;
