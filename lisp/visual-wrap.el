;;; visual-wrap.el --- Smart line-wrapping with wrap-prefix -*- lexical-binding: t -*-

;; Copyright (C) 2011-2021, 2024 Free Software Foundation, Inc.

;; Author: Stephen Berman <stephen.berman@gmx.net>
;;         Stefan Monnier <monnier@iro.umontreal.ca>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience
;; Package: emacs

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the `visual-wrap-prefix-mode' minor mode
;; which sets the wrap-prefix property on the fly so that
;; single-long-line paragraphs get word-wrapped in a way similar to
;; what you'd get with M-q using visual-fill-mode, but without
;; actually changing the buffer's text.

;;; Code:

(defcustom visual-wrap-extra-indent 0
  "Number of extra spaces to indent in `visual-wrap-prefix-mode'.

`visual-wrap-prefix-mode' indents the visual lines to the level
of the actual line plus `visual-wrap-extra-indent'.  A negative
value will do a relative de-indent.

Examples:

actual indent = 2
extra indent = -1

  Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
 do eiusmod tempor incididunt ut labore et dolore magna
 aliqua. Ut enim ad minim veniam, quis nostrud exercitation
 ullamco laboris nisi ut aliquip ex ea commodo consequat.

actual indent = 2
extra indent = 2

  Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
    do eiusmod tempor incididunt ut labore et dolore magna
    aliqua. Ut enim ad minim veniam, quis nostrud exercitation
    ullamco laboris nisi ut aliquip ex ea commodo consequat."
  :type 'integer
  :safe 'integerp
  :group 'visual-line)

(defun visual-wrap--face-extend-p (face)
  ;; Before Emacs 27, faces always extended beyond EOL, so we check
  ;; for a non-default background instead.
  (cond
   ((listp face)
    (plist-get face (if (fboundp 'face-extend-p) :extend :background)))
   ((symbolp face)
    (if (fboundp 'face-extend-p)
        (face-extend-p face nil t)
      (face-background face nil t)))))

(defun visual-wrap--prefix-face (fcp _beg end)
  ;; If the fill-context-prefix already specifies a face, just use that.
  (cond ((get-text-property 0 'face fcp))
        ;; Else, if the last character is a newline and has a face
        ;; that extends beyond EOL, assume that this face spans the
        ;; whole line and apply it to the prefix to preserve the
        ;; "block" visual effect.
        ;;
        ;; NB: the face might not actually span the whole line: see
        ;; for example removed lines in diff-mode, where the first
        ;; character has the diff-indicator-removed face, while the
        ;; rest of the line has the diff-removed face.
        ((= (char-before end) ?\n)
         (let ((eol-face (get-text-property (1- end) 'face)))
           ;; `eol-face' can be a face, a "face value"
           ;; (plist of face properties) or a list of one of those.
           (if (or (not (consp eol-face)) (keywordp (car eol-face)))
               ;; A single face.
               (if (visual-wrap--face-extend-p eol-face) eol-face)
             ;; A list of faces.  Keep the ones that extend beyond EOL.
             (delq nil (mapcar (lambda (f)
                                 (if (visual-wrap--face-extend-p f) f))
                               eol-face)))))))

(defun visual-wrap--prefix (fcp)
  (let ((fcp-len (string-width fcp)))
    (cond
     ((= 0 visual-wrap-extra-indent)
      fcp)
     ((< 0 visual-wrap-extra-indent)
      (concat fcp (make-string visual-wrap-extra-indent ?\s)))
     ((< 0 (+ visual-wrap-extra-indent fcp-len))
      (substring fcp
                 0
                 (+ visual-wrap-extra-indent fcp-len)))
     (t
      ""))))

(defun visual-wrap-fill-context-prefix (beg end)
  "Like `fill-context-prefix', but with length adjusted by
`visual-wrap-extra-indent'."
  (let* ((fcp
          ;; `fill-context-prefix' ignores prefixes that look like
          ;; paragraph starts, in order to avoid inadvertently
          ;; creating a new paragraph while filling, but here we're
          ;; only dealing with single-line "paragraphs" and we don't
          ;; actually modify the buffer, so this restriction doesn't
          ;; make much sense (and is positively harmful in
          ;; taskpaper-mode where paragraph-start matches everything).
          (or (let ((paragraph-start "\\`\\'a"))
                    (fill-context-prefix beg end))
                  ;; Note: fill-context-prefix may return nil; See:
                  ;; http://article.gmane.org/gmane.emacs.devel/156285
              ""))
         (prefix (visual-wrap--prefix fcp))
         (face (visual-wrap--prefix-face fcp beg end)))
    (if face
        (propertize prefix 'face face)
      prefix)))

(defun visual-wrap-prefix-function (beg end)
  "Indent the region between BEG and END with visual filling."
  ;; Any change at the beginning of a line might change its wrap
  ;; prefix, which affects the whole line.  So we need to "round-up"
  ;; `end' to the nearest end of line.  We do the same with `beg'
  ;; although it's probably not needed.
  (goto-char end)
  (unless (bolp) (forward-line 1))
  (setq end (point))
  (goto-char beg)
  (forward-line 0)
  (setq beg (point))
  (while (< (point) end)
    (let ((lbp (point)))
      (put-text-property
       (point) (progn (search-forward "\n" end 'move) (point))
       'wrap-prefix
       (let ((pfx (visual-wrap-fill-context-prefix
		   lbp (point))))
	 ;; Remove any `wrap-prefix' property that might have been
	 ;; added earlier.  Otherwise, we end up with a string
	 ;; containing a `wrap-prefix' string containing a
	 ;; `wrap-prefix' string ...
	 (remove-text-properties
	  0 (length pfx) '(wrap-prefix) pfx)
         (let ((dp (get-text-property 0 'display pfx)))
           (when (and dp (eq dp (get-text-property (1- lbp) 'display)))
             ;; There's a `display' property which covers not just the
             ;; prefix but also the previous newline.  So it's not
             ;; just making the prefix more pretty and could interfere
             ;; or even defeat our efforts (e.g. it comes from
             ;; `visual-fill-mode').
             (remove-text-properties
	      0 (length pfx) '(display) pfx)))
	 pfx))))
  `(jit-lock-bounds ,beg . ,end))

;;;###autoload
(define-minor-mode visual-wrap-prefix-mode
  "Display continuation lines with prefixes from surrounding context."
  :lighter ""
  :group 'visual-line
  (if visual-wrap-prefix-mode
      (progn
        ;; HACK ATTACK!  We want to run after font-lock (so our
        ;; wrap-prefix includes the faces applied by font-lock), but
        ;; jit-lock-register doesn't accept an `append' argument, so
        ;; we add ourselves beforehand, to make sure we're at the end
        ;; of the hook (bug#15155).
        (add-hook 'jit-lock-functions
                  #'visual-wrap-prefix-function 'append t)
        (jit-lock-register #'visual-wrap-prefix-function))
    (jit-lock-unregister #'visual-wrap-prefix-function)
    (with-silent-modifications
      (save-restriction
        (widen)
        (remove-text-properties (point-min) (point-max) '(wrap-prefix nil))))))

(provide 'visual-wrap)
;;; visual-wrap.el ends here
