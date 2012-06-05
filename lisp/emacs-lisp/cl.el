;;; cl.el --- Compatibility aliases for the old CL library.

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: extensions

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a compatibility file which provides the old names provided by CL
;; before we cleaned up its namespace usage.

;;; Code:

(require 'cl-lib)

;; (defun cl--rename ()
;;   (let ((vdefs ())
;;         (fdefs ())
;;         (case-fold-search nil)
;;         (files '("cl.el" "cl-macs.el" "cl-seq.el" "cl-extra.el")))
;;     (dolist (file files)
;;       (with-current-buffer (find-file-noselect file)
;;         (goto-char (point-min))
;;         (while (re-search-forward
;;                 "^(\\(def[^ \t\n]*\\) +'?\\(\\(\\sw\\|\\s_\\)+\\)" nil t)
;;           (let ((name (match-string-no-properties 2))
;;                 (type (match-string-no-properties 1)))
;;             (unless (string-match-p "\\`cl-" name)
;;               (cond
;;                ((member type '("defvar" "defconst"))
;;                 (unless (member name vdefs) (push name vdefs)))
;;                ((member type '("defun" "defsubst" "defalias" "defmacro"))
;;                 (unless (member name fdefs) (push name fdefs)))
;;                ((member type '("def-edebug-spec" "defsetf" "define-setf-method"
;;                                "define-compiler-macro"))
;;                 nil)
;;                (t (error "Unknown type %S" type))))))))
;;     (let ((re (concat "\\_<" (regexp-opt (append vdefs fdefs)) "\\_>"))
;;           (conflicts ()))
;;       (dolist (file files)
;;         (with-current-buffer (find-file-noselect file)
;;           (goto-char (point-min))
;;           (while (re-search-forward re nil t)
;;             (replace-match "cl-\\&"))
;;           (save-buffer))))
;;     (with-current-buffer (find-file-noselect "cl-rename.el")
;;       (dolist (def vdefs)
;;         (insert (format "(defvaralias '%s 'cl-%s)\n" def def)))
;;       (dolist (def fdefs)
;;         (insert (format "(defalias '%s 'cl-%s)\n" def def)))
;;       (save-buffer))))

;; (defun cl--unrename ()
;;   ;; Taken from "Naming Conventions" node of the doc.
;;   (let* ((names '(defun* defsubst* defmacro* function* member*
;;                          assoc* rassoc* get* remove* delete*
;;                          mapcar* sort* floor* ceiling* truncate*
;;                          round* mod* rem* random*))
;;          (files '("cl.el" "cl-lib.el" "cl-macs.el" "cl-seq.el" "cl-extra.el"))
;;          (re (concat "\\_<cl-" (regexp-opt (mapcar #'symbol-name names))
;;                      "\\_>")))
;;     (dolist (file files)
;;       (with-current-buffer (find-file-noselect file)
;;         (goto-char (point-min))
;;         (while (re-search-forward re nil t)
;;           (delete-region (1- (point)) (point)))
;;         (save-buffer)))))
(dolist (var '(
               ;; loop-result-var
               ;; loop-result
               ;; loop-initially
               ;; loop-finally
               ;; loop-bindings
               ;; loop-args
               ;; bind-inits
               ;; bind-block
               ;; lambda-list-keywords
               float-negative-epsilon
               float-epsilon
               least-negative-normalized-float
               least-positive-normalized-float
               least-negative-float
               least-positive-float
               most-negative-float
               most-positive-float
               ;; custom-print-functions
               ))
  (defvaralias var (intern (format "cl-%s" var))))

(dolist (fun '(
               (get* . cl-get)
               (random* . cl-random)
               (rem* . cl-rem)
               (mod* . cl-mod)
               (round* . cl-round)
               (truncate* . cl-truncate)
               (ceiling* . cl-ceiling)
               (floor* . cl-floor)
               (rassoc* . cl-rassoc)
               (assoc* . cl-assoc)
               (member* . cl-member)
               (delete* . cl-delete)
               (remove* . cl-remove)
               (defsubst* . cl-defsubst)
               (sort* . cl-sort)
               (function* . cl-function)
               (defmacro* . cl-defmacro)
               (defun* . cl-defun)
               (mapcar* . cl-mapcar)

               remprop
               getf
               tailp
               list-length
               nreconc
               revappend
               concatenate
               subseq
               random-state-p
               make-random-state
               signum
               isqrt
               lcm
               gcd
               notevery
               notany
               every
               some
               mapcon
               mapcan
               mapl
               maplist
               map
               equalp
               coerce
               tree-equal
               nsublis
               sublis
               nsubst-if-not
               nsubst-if
               nsubst
               subst-if-not
               subst-if
               subsetp
               nset-exclusive-or
               set-exclusive-or
               nset-difference
               set-difference
               nintersection
               intersection
               nunion
               union
               rassoc-if-not
               rassoc-if
               assoc-if-not
               assoc-if
               member-if-not
               member-if
               merge
               stable-sort
               search
               mismatch
               count-if-not
               count-if
               count
               position-if-not
               position-if
               position
               find-if-not
               find-if
               find
               nsubstitute-if-not
               nsubstitute-if
               nsubstitute
               substitute-if-not
               substitute-if
               substitute
               delete-duplicates
               remove-duplicates
               delete-if-not
               delete-if
               remove-if-not
               remove-if
               replace
               fill
               reduce
               compiler-macroexpand
               define-compiler-macro
               assert
               check-type
               typep
               deftype
               defstruct
               define-modify-macro
               callf2
               callf
               letf*
               letf
               rotatef
               shiftf
               remf
               psetf
               setf
               get-setf-method
               defsetf
               define-setf-expander
               define-setf-method
               declare
               the
               locally
               multiple-value-setq
               multiple-value-bind
               lexical-let*
               lexical-let
               symbol-macrolet
               macrolet
               labels
               flet
               progv
               psetq
               do-all-symbols
               do-symbols
               dotimes
               dolist
               do*
               do
               loop
               return-from
               return
               block
               etypecase
               typecase
               ecase
               case
               load-time-value
               eval-when
               destructuring-bind
               gentemp
               gensym
               pairlis
               acons
               subst
               adjoin
               copy-list
               ldiff
               list*
               cddddr
               cdddar
               cddadr
               cddaar
               cdaddr
               cdadar
               cdaadr
               cdaaar
               cadddr
               caddar
               cadadr
               cadaar
               caaddr
               caadar
               caaadr
               caaaar
               cdddr
               cddar
               cdadr
               cdaar
               caddr
               cadar
               caadr
               caaar
               tenth
               ninth
               eighth
               seventh
               sixth
               fifth
               fourth
               third
               endp
               rest
               second
               first
               svref
               copy-seq
               evenp
               oddp
               minusp
               plusp
               floatp-safe
               declaim
               proclaim
               nth-value
               multiple-value-call
               multiple-value-apply
               multiple-value-list
               values-list
               values
               pushnew
               push
               pop
               decf
               incf
               ))
  (let ((new (if (consp fun) (prog1 (cdr fun) (setq fun (car fun)))
               (intern (format "cl-%s" fun)))))
    (defalias fun new)
    ;; If `cl-foo' is declare inline, then make `foo' inline as well, and
    ;; similarly.  Same for edebug specifications, indent rules and
    ;; doc-string position.
    ;; FIXME: For most of them, we should instead follow aliases
    ;; where applicable.
    (dolist (prop '(byte-optimizer doc-string-elt edebug-form-spec
                    lisp-indent-function))
      (if (get new prop)
        (put fun prop (get new prop))))))

(provide 'cl)
;;; cl.el ends here
