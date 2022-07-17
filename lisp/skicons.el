;;; icons.el --- Icon support in buffers, mode-line, etc.  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefan@marxist.se>
;; Keywords: faces, multimedia

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

;; * Introduction
;;
;; This library provides support for icons, that can be used for
;; things like decorating a buffer, toolbar buttons or the mode-line.
;; Emacs comes bundled with several sets of icons, but you can also
;; install new sets from package archives like GNU ELPA.
;;
;; icons.el allows users to change the appearance of icons all over
;; Emacs, in libraries supporting it, by customizing
;; `icons-set-priority'.  You can think of this as icon themes for
;; Emacs.
;;
;; Run `M-x customize-group RET icons RET' to see all user options.
;;
;; * Using this library as an Emacs Lisp developer
;;
;; To use these icons from Lisp, see `icons-get' and `icons-insert'.
;; To use an icon in the modeline, use `icons-get-for-modeline'.
;; Type `M-x list-icons' to see a list of all supported icons.
;;
;; * Adding Icon sets
;;
;; If you want to add a new icon set, the best starting point is to
;; study an existing icon set.  It comes down to one file containing a
;; `icons-define-set' form, and then the actual image files.  It is
;; highly recommended to use SVG icons, but consider providing XPM and
;; PBM fall-backs for users on older machines.
;;
;; To add SVG icons, you need to make sure that the SVG files don't
;; contain any unnecessary or incorrect markup that stops them from
;; being displayed correctly.
;;
;; There is optional support for more than one size of the same
;; icon. This is to avoid icons looking bad when resized, and adapt
;; things like line thickness for display at various sizes.  See also
;; the explanation on this page:
;; https://github.com/primer/octicons/blob/main/docs/content/guidelines/usage.mdx#sizing
;;
;; If you intend to distribute your icon set to others, it is
;; important that they have a license that allows it.  We cannot give
;; legal advice, but typically this means a GPL compatible license.
;; You can find more information about licenses here:
;; https://www.gnu.org/licenses/license-list.html

;;; Code:

;; TODO:
;; - UTF-8 icons
;; - Icon aliases

(require 'cl-lib)
(with-eval-after-load 'icons
  (require 'icons-material)
  (require 'icons-octicons))


;;;; User options.

(defgroup icons nil
  "Graphical icons in Emacs."
  :group 'multimedia
  :version "29.1")

(defcustom icons-enabled (display-graphic-p)
  "If non-nil, enable graphical icons."
  :type 'boolean
  :version "29.1")

(defcustom icons-set-priority '(octicons)
  "Priority of icon sets used by `icons-insert' et al."
  :type '(list symbol)
  :version "29.1")

(defconst icons-type-priority '(svg png ppm xpm pbm)
  "Priority of icon formats used by `icons-insert' et al.")


;;;; Data.

;; FIXME: Is this needed?
(defvar icons-defined-sets nil
  "List of all icon sets defined with `icons-define-set'.")

(defvar icons-alist nil
  "Alist containing all icon sets defined by `icons-define-set'.
Has the form (NAME . ICONS), where NAME is a symbol representing
a particular icon, and ICONS is a list of `icons-icon'
structures.

Note that the list of icons might belong to different defined
sets of icons, and which one is used depends on the user
option `icons-set-priority' and `icons-type-priority'.")

(cl-defstruct (icons-icon (:constructor icons-icon-create)
                     (:copier icons-icon-copy))
  "Structure containing information about an individual icon file."
  ( filename nil
    :documentation "Filename of this icon (string)."
    :type string)
  ( type nil
    :documentation "Image type of the icon (symbol)."
    :type symbol)
  ( size nil
    :documentation "Size of this icon in pixels (integer).
Specifies the size at which this icon is best viewed."
    :type integer)
  ( set nil
    :documentation "Icon set that this icon belongs to (symbol)."
    :type symbol))

(defun icons-add-icon (name icon)
  "Add `icons-icon' ICON with NAME to `icons-alist'."
  (unless (icons-icon-p icon)
    (error "Not an icon: %S" icon))
  (if-let ((orig (cdr (assoc name icons-alist))))
      (setf (cdr (assoc name icons-alist)) (cons icon orig))
    (push (cons name (list icon)) icons-alist)))

(defun icons--remove-set (set)
  "Remove all icons belonging to SET from `icons-alist'."
  (setq icons-alist
        (seq-filter
         (lambda (elem) (> (length elem) 1))
         (mapcar (lambda (is)
                   (append
                    (list (car is))
                    (seq-filter (lambda (i)
                                  (not (eq (icons-icon-set i) set)))
                                (cdr is))))
                 icons-alist))))

(defun icons-define-set (set icons)
  "Define a new icon SET from ICONS and add it to `icons-alist'.
SET is a symbol naming the new set.

ICONS is a list on the form (NAME FILE SIZE) where NAME is the
name of the icon (a string), FILE is a filename, and SIZE is the
pixel size at which this is best viewed."
  (declare (indent defun))
  (icons--remove-set set)
  (cl-pushnew set icons-defined-sets)
  (dolist (icon icons)
    (let* ((name (car icon))
           (filename (cadr icon))
           (size (caddr icon))
           ;; Infer the type from the filename.
           (type (intern
                  (progn
                    (string-match (rx "." (group (+ alnum)) eos)
                                  filename)
                    (match-string 1 filename)))))
      (icons-add-icon name (icons-icon-create :filename filename
                                    :size size
                                    :type type
                                    :set set)))))


;;;; Inserting and getting icons.

(defun icons--get-sorted-icons (name &optional _size)
  "Return icons for NAME sorted by type and set.
The order is given by `icons-type-priority', `icons-set-priority'
and SIZE in that order.

Optional argument SIZE, if non-nil."
  (let ((icons (copy-sequence (cdr (assoc name icons-alist)))))
    (sort icons
          (lambda (A B)
            (let ((Af (icons-icon-type A))
                  (Bf (icons-icon-type B))
                  (As (icons-icon-set A))
                  (Bs (icons-icon-set B)))
              (or (< (or (cl-position Af icons-type-priority) most-positive-fixnum)
                     (or (cl-position Bf icons-type-priority) most-positive-fixnum))
                  (< (or (cl-position As icons-set-priority) most-positive-fixnum)
                     (or (cl-position Bs icons-set-priority) most-positive-fixnum))))))))

(defun icons--image-spec-from-icon (icon)
  "Return a specification for `find-image' based on `icons-icon' ICON."
  (cl-assert (icons-icon-p icon))
  (list :file (icons-icon-filename icon)
        :type (icons-icon-type icon)
        :ascent 'center
        :height '(1 . em)))

(defun icons--get-icon (name &optional _size)
  "Return the best icon to use for NAME.
The icon is found by `icons--get-sorted-icons' (which see)."
  (or (when-let ((icons (icons--get-sorted-icons name)))
        (find-image (mapcar #'icons--image-spec-from-icon icons)))
      (error "Unable to find icon: `%s'" name)))

;;;###autoload
(defun icons-get (name &optional _size)
  "Return icon NAME for inserting into a buffer.
NAME is a string."
  ;; FIXME: Size, based on default face.
  ;; You should also be able to pass in a different face.
  (if icons-enabled
      (propertize " " 'display (icons--get-icon name))
    ""))

;;;###autoload
(defun icons-get-filename (name &optional _size)
  "Return filename of icon NAME.
NAME is a string."
  (plist-get (cdr (icons--get-icon name)) :file))

;;;###autoload
(defun icons-get-for-modeline (name)
  "Return icon NAME for use in `mode-line-format'.
NAME is as in `icons-get'."
  (if icons-enabled
      `(:propertize (" ") display ,(icons--get-icon name))
    ""))

;;;###autoload
(defun icons-insert (name &optional _size)   ; FIXME: Is this very useful?
  "Insert icon NAME at point.
NAME is as in `icons-get'."
  (when icons-enabled
    (insert (icons-get name))))

;; (defun icons--filename-for-size (font-size filename-alist)
;;   "Return filename from FILENAME-ALIST closest to FONT-SIZE."
;;   (if (listp filename-alist)
;;       (let* ((sizes (map-keys filename-alist))
;;              (size (icons--closest-to font-size sizes)))
;;         (cdr (assq size filename-alist)))
;;     filename-alist))


;;;; Describing icons.

(defface icons-description-title '((t :inherit bold)) "")
(defface icons-icon-075 '((t :height 0.75)) "")
(defface icons-icon-100 '((t :height 1.0)) "")
(defface icons-icon-150 '((t :height 1.5)) "")
(defface icons-icon-200 '((t :height 2.0)) "")
(defface icons-icon-300 '((t :height 3.0)) "")
(defface icons-icon-400 '((t :height 4.0)) "")

(defun describe-icon (name)
  "Describe icon NAME."
  (interactive (list (completing-read (format-prompt "Describe icon" nil)
                                      (sort (mapcar #'car icons-alist) #'string<))))
  (let ((icon (cadr (assoc name icons-alist))))
    (help-setup-xref (list #'describe-icon name)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        ;; TODO: Link the set name to corresponding `describe-icon-set'.
        (insert (format-message "%S belongs to the icon set `%s'.\n\n"
                                name (icons-icon-set icon)))

        (insert (propertize "Filename:" 'face 'icons-description-title)
                " "
                (icons-icon-filename icon)
                "\n")
        (insert (propertize "Size:" 'face 'icons-description-title)
                " "
                (format "%s" (icons-icon-size icon))
                "\n")
        (insert (propertize "Type:" 'face 'icons-description-title)
                " "
                (format "%s" (icons-icon-type icon))
                "\n")

        (insert "\n")
        (dolist (face '( icons-icon-075 icons-icon-100 icons-icon-150
                         icons-icon-200 icons-icon-300 icons-icon-400))
          (insert (propertize " " 'display '(space :align-to 2)))
          (insert (propertize (icons-get name) 'face face))
          (insert "\n\n"))))))

;; (defun describe-icon-set (name)
;;   "Describe icon set NAME."
;;   (interactive
;;    (list (completing-read (format-prompt "Describe icon set" nil)
;;                           (sort icons-defined-sets
;;                                 (lambda (a b)
;;                                   (string< (symbol-name a) (symbol-name b)))))))
;;   (help-setup-xref (list #'describe-icon-set name)
;;                    (called-interactively-p 'interactive))
;;   (with-help-window (help-buffer)
;;     (with-current-buffer standard-output
;;       ;; TODO: Link the set name to corresponding `describe-icon-set'.
;;       (princ (format-message "Icon set `%s'.\n\n"
;;                              (icons-icon-set icon)))
;;       ;; TODO: Show all alternative icons in different sizes.
;;       (icons-insert name))))


;;;; Listing icons.

(defconst icons-list-buffer-name "*Icons*")

(defun icons-list-make-entries ()
  "Make list of all icons for `tabulated-list-entries'."
  (let (entries)
    (dolist (ic icons-alist)
      (let ((icon-name (car ic))
            (icons (cdr ic)))
        (dolist (icon icons)
          (push (list (cons icon-name icon)
                      (vector (symbol-name (icons-icon-set icon))
                              icon-name
                              (format "%s" (or (icons-icon-size icon) ""))
                              (format "%s" (icons-icon-type icon))
                              (find-image (list (icons--image-spec-from-icon icon )))))
                entries))))
    entries))

;; Ignore arguments to be usable for `revert-buffer-function'.
(defun icons-list-display (&optional _ _ _)
  "Prepare buffer for `tabulated-list-mode' based on `icons-alist'."
  (setq tabulated-list-entries (icons-list-make-entries))
  (tabulated-list-init-header)
  (tabulated-list-print t))

(defvar-keymap icons-list-mode-map
  :doc "Keymap for `icons-list-mode'."
  "w" #'icons-list-copy-name)

(define-derived-mode icons-list-mode tabulated-list-mode "Icons List Mode"
  "Major mode for listing icons."
  :interactive nil
  (setq tabulated-list-format [("Icon set" 13 icons-list--icon-set-predicate)
                               ("Name" 30 icons-list--name-predicate)
                               ("Size" 6 icons-list--size-predicate)
                               ("Type" 5)
                               ("Icon" -1)])
  (setq revert-buffer-function #'icons-list-display)
  (setq tabulated-list-sort-key (cons "Size" nil)))

;;;###autoload
(defun list-icons ()
  "Display all defined icons."
  (interactive)
  (pop-to-buffer (get-buffer-create icons-list-buffer-name))
  (icons-list-mode)
  (icons-list-display))

(defun icons-list--name-predicate (A B)
  "Predicate to sort `list-icons' by \"Name\"."
  (string< (caar A) (caar B)))

(defun icons-list--size-predicate (A B)
  "Predicate to sort `list-icons' by \"Size\"."
  (< (string-to-number (aref (cadr A) 2))
     (string-to-number (aref (cadr B) 2))))

(defun icons-list--icon-set-predicate (A B)
  "Predicate to sort `list-icons' by Icon Set."
  (let ((Aset (aref (cadr A) 0))
        (Bset (aref (cadr B) 0)))
    (if (string= Aset Bset)
        ;; Secondary sort keys.
        (or (icons-list--size-predicate A B)
            (icons-list--name-predicate A B))
      (string< Aset Bset))))

(defun icons-list-copy-name ()
  "In `icons-list-mode', copy name of icon at point."
  (interactive nil icons-list-mode)
  (let ((icon-name (aref (get-text-property (point) 'tabulated-list-entry) 1)))
    (when icon-name
      (kill-new icon-name)
      (message "%s" icon-name))))

;; (get-char-property (point) 'read-face-name)
;; (get-char-property (point) 'font-lock-face)
;; (get-char-property (point) 'face)


;;;; Util.

(defun icons--face-height-at-point ()
  "Return font height at point."
  (let* ((scale (cadr (assoc :height (assoc 'default face-remapping-alist))))
         (face (face-font (or (face-at-point t) 'default)))
         (height (* (aref (font-info face) 2) (if scale scale 1))))
    height))

(defun icons--closest-to (num candidates)
  "Return the closest number to NUM among CANDIDATES."
  (car (sort candidates (lambda (a b) (<= (abs (- num a))
                                     (abs (- num b)))))))

(icons-define-set 'emacs
  '(
    ("attach" "attach.pbm")
    ("attach" "attach.xpm")
    ("back-arrow" "back-arrow.pbm")
    ("back-arrow" "back-arrow.xpm")
    ("bookmark_add" "bookmark_add.pbm")
    ("bookmark_add" "bookmark_add.xpm")
    ("cancel" "cancel.pbm")
    ("cancel" "cancel.xpm")
    ("checkbox-mixed" "checkbox-mixed.svg")
    ("checked" "checked.svg")
    ("checked" "checked.xpm")
    ("close" "close.pbm")
    ("close" "close.xpm")
    ("connect" "connect.pbm")
    ("connect" "connect.xpm")
    ("contact" "contact.pbm")
    ("contact" "contact.xpm")
    ("copy" "copy.pbm")
    ("copy" "copy.xpm")
    ("custom/down" "custom/down.pbm")
    ("custom/down" "custom/down.xpm")
    ("custom/down-pushed" "custom/down-pushed.pbm")
    ("custom/down-pushed" "custom/down-pushed.xpm")
    ("custom/right" "custom/right.pbm")
    ("custom/right" "custom/right.xpm")
    ("custom/right-pushed" "custom/right-pushed.pbm")
    ("custom/right-pushed" "custom/right-pushed.xpm")
    ("cut" "cut.pbm")
    ("cut" "cut.xpm")
    ("data-save" "data-save.pbm")
    ("data-save" "data-save.xpm")
    ("delete" "delete.pbm")
    ("delete" "delete.xpm")
    ("describe" "describe.pbm")
    ("describe" "describe.xpm")
    ("diropen" "diropen.pbm")
    ("diropen" "diropen.xpm")
    ("disconnect" "disconnect.pbm")
    ("disconnect" "disconnect.xpm")
    ("down" "down.svg")
    ("exit" "exit.pbm")
    ("exit" "exit.xpm")
    ("ezimage/bits" "ezimage/bits.pbm")
    ("ezimage/bits" "ezimage/bits.xpm")
    ("ezimage/bitsbang" "ezimage/bitsbang.pbm")
    ("ezimage/bitsbang" "ezimage/bitsbang.xpm")
    ("ezimage/box" "ezimage/box.pbm")
    ("ezimage/box" "ezimage/box.xpm")
    ("ezimage/box-minus" "ezimage/box-minus.pbm")
    ("ezimage/box-minus" "ezimage/box-minus.xpm")
    ("ezimage/box-plus" "ezimage/box-plus.pbm")
    ("ezimage/box-plus" "ezimage/box-plus.xpm")
    ("ezimage/checkmark" "ezimage/checkmark.pbm")
    ("ezimage/checkmark" "ezimage/checkmark.xpm")
    ("ezimage/dir" "ezimage/dir.pbm")
    ("ezimage/dir" "ezimage/dir.xpm")
    ("ezimage/dir-minus" "ezimage/dir-minus.pbm")
    ("ezimage/dir-minus" "ezimage/dir-minus.xpm")
    ("ezimage/dir-plus" "ezimage/dir-plus.pbm")
    ("ezimage/dir-plus" "ezimage/dir-plus.xpm")
    ("ezimage/doc" "ezimage/doc.pbm")
    ("ezimage/doc" "ezimage/doc.xpm")
    ("ezimage/doc-minus" "ezimage/doc-minus.pbm")
    ("ezimage/doc-minus" "ezimage/doc-minus.xpm")
    ("ezimage/doc-plus" "ezimage/doc-plus.pbm")
    ("ezimage/doc-plus" "ezimage/doc-plus.xpm")
    ("ezimage/info" "ezimage/info.pbm")
    ("ezimage/info" "ezimage/info.xpm")
    ("ezimage/key" "ezimage/key.pbm")
    ("ezimage/key" "ezimage/key.xpm")
    ("ezimage/label" "ezimage/label.pbm")
    ("ezimage/label" "ezimage/label.xpm")
    ("ezimage/lock" "ezimage/lock.pbm")
    ("ezimage/lock" "ezimage/lock.xpm")
    ("ezimage/mail" "ezimage/mail.pbm")
    ("ezimage/mail" "ezimage/mail.xpm")
    ("ezimage/page" "ezimage/page.pbm")
    ("ezimage/page" "ezimage/page.xpm")
    ("ezimage/page-minus" "ezimage/page-minus.pbm")
    ("ezimage/page-minus" "ezimage/page-minus.xpm")
    ("ezimage/page-plus" "ezimage/page-plus.pbm")
    ("ezimage/page-plus" "ezimage/page-plus.xpm")
    ("ezimage/tag" "ezimage/tag.pbm")
    ("ezimage/tag" "ezimage/tag.xpm")
    ("ezimage/tag-gt" "ezimage/tag-gt.pbm")
    ("ezimage/tag-gt" "ezimage/tag-gt.xpm")
    ("ezimage/tag-minus" "ezimage/tag-minus.pbm")
    ("ezimage/tag-minus" "ezimage/tag-minus.xpm")
    ("ezimage/tag-plus" "ezimage/tag-plus.pbm")
    ("ezimage/tag-plus" "ezimage/tag-plus.xpm")
    ("ezimage/tag-type" "ezimage/tag-type.pbm")
    ("ezimage/tag-type" "ezimage/tag-type.xpm")
    ("ezimage/tag-v" "ezimage/tag-v.pbm")
    ("ezimage/tag-v" "ezimage/tag-v.xpm")
    ("ezimage/unlock" "ezimage/unlock.pbm")
    ("ezimage/unlock" "ezimage/unlock.xpm")
    ("fwd-arrow" "fwd-arrow.pbm")
    ("fwd-arrow" "fwd-arrow.xpm")
    ("gnus" "gnus.pbm")
    ("gnus/followup" "gnus/followup.pbm")
    ("gnus/followup" "gnus/followup.xpm")
    ("gnus/fuwo" "gnus/fuwo.pbm")
    ("gnus/fuwo" "gnus/fuwo.xpm")
    ("gnus/gnus" "gnus/gnus.png")
    ("gnus/gnus" "gnus/gnus.svg")
    ("gnus/gnus" "gnus/gnus.xbm")
    ("gnus/gnus" "gnus/gnus.xpm")
    ("gnus/gnus-pointer" "gnus/gnus-pointer.xbm")
    ("gnus/gnus-pointer" "gnus/gnus-pointer.xpm")
    ("gnus/kill-group" "gnus/kill-group.pbm")
    ("gnus/kill-group" "gnus/kill-group.xpm")
    ("gnus/mail-reply" "gnus/mail-reply.pbm")
    ("gnus/mail-reply" "gnus/mail-reply.xpm")
    ("gnus/mail-send" "gnus/mail-send.pbm")
    ("gnus/mail-send" "gnus/mail-send.xpm")
    ("gnus/preview" "gnus/preview.xbm")
    ("gnus/preview" "gnus/preview.xpm")
    ("gnus/toggle-subscription" "gnus/toggle-subscription.pbm")
    ("gnus/toggle-subscription" "gnus/toggle-subscription.xpm")
    ("gud/all" "gud/all.pbm")
    ("gud/all" "gud/all.xpm")
    ("gud/break" "gud/break.pbm")
    ("gud/break" "gud/break.xpm")
    ("gud/cont" "gud/cont.pbm")
    ("gud/cont" "gud/cont.xpm")
    ("gud/down" "gud/down.pbm")
    ("gud/down" "gud/down.xpm")
    ("gud/finish" "gud/finish.pbm")
    ("gud/finish" "gud/finish.xpm")
    ("gud/go" "gud/go.pbm")
    ("gud/go" "gud/go.xpm")
    ("gud/next" "gud/next.pbm")
    ("gud/next" "gud/next.xpm")
    ("gud/nexti" "gud/nexti.pbm")
    ("gud/nexti" "gud/nexti.xpm")
    ("gud/pp" "gud/pp.pbm")
    ("gud/pp" "gud/pp.xpm")
    ("gud/print" "gud/print.pbm")
    ("gud/print" "gud/print.xpm")
    ("gud/pstar" "gud/pstar.pbm")
    ("gud/pstar" "gud/pstar.xpm")
    ("gud/rcont" "gud/rcont.pbm")
    ("gud/rcont" "gud/rcont.xpm")
    ("gud/recstart" "gud/recstart.pbm")
    ("gud/recstart" "gud/recstart.xpm")
    ("gud/recstop" "gud/recstop.pbm")
    ("gud/recstop" "gud/recstop.xpm")
    ("gud/remove" "gud/remove.pbm")
    ("gud/remove" "gud/remove.xpm")
    ("gud/rfinish" "gud/rfinish.pbm")
    ("gud/rfinish" "gud/rfinish.xpm")
    ("gud/rnext" "gud/rnext.pbm")
    ("gud/rnext" "gud/rnext.xpm")
    ("gud/rnexti" "gud/rnexti.pbm")
    ("gud/rnexti" "gud/rnexti.xpm")
    ("gud/rstep" "gud/rstep.pbm")
    ("gud/rstep" "gud/rstep.xpm")
    ("gud/rstepi" "gud/rstepi.pbm")
    ("gud/rstepi" "gud/rstepi.xpm")
    ("gud/run" "gud/run.pbm")
    ("gud/run" "gud/run.xpm")
    ("gud/step" "gud/step.pbm")
    ("gud/step" "gud/step.xpm")
    ("gud/stepi" "gud/stepi.pbm")
    ("gud/stepi" "gud/stepi.xpm")
    ("gud/stop" "gud/stop.pbm")
    ("gud/stop" "gud/stop.xpm")
    ("gud/thread" "gud/thread.pbm")
    ("gud/thread" "gud/thread.xpm")
    ("gud/until" "gud/until.pbm")
    ("gud/until" "gud/until.xpm")
    ("gud/up" "gud/up.pbm")
    ("gud/up" "gud/up.xpm")
    ("gud/watch" "gud/watch.pbm")
    ("gud/watch" "gud/watch.xpm")
    ("help" "help.pbm")
    ("help" "help.xpm")
    ("home" "home.pbm")
    ("home" "home.xpm")
    ("index" "index.pbm")
    ("index" "index.xpm")
    ("info" "info.pbm")
    ("info" "info.xpm")
    ("jump-to" "jump-to.pbm")
    ("jump-to" "jump-to.xpm")
    ("left" "left.svg")
    ("left-arrow" "left-arrow.pbm")
    ("left-arrow" "left-arrow.xpm")
    ("letter" "letter.pbm")
    ("letter" "letter.xpm")
    ("lock" "lock.pbm")
    ("lock" "lock.xpm")
    ("lock-broken" "lock-broken.pbm")
    ("lock-broken" "lock-broken.xpm")
    ("lock-ok" "lock-ok.pbm")
    ("lock-ok" "lock-ok.xpm")
    ("low-color/back-arrow" "low-color/back-arrow.xpm")
    ("low-color/copy" "low-color/copy.xpm")
    ("low-color/cut" "low-color/cut.xpm")
    ("low-color/fwd-arrow" "low-color/fwd-arrow.xpm")
    ("low-color/help" "low-color/help.xpm")
    ("low-color/home" "low-color/home.xpm")
    ("low-color/index" "low-color/index.xpm")
    ("low-color/jump-to" "low-color/jump-to.xpm")
    ("low-color/left-arrow" "low-color/left-arrow.xpm")
    ("low-color/new" "low-color/new.xpm")
    ("low-color/next-node" "low-color/next-node.xpm")
    ("low-color/open" "low-color/open.xpm")
    ("low-color/paste" "low-color/paste.xpm")
    ("low-color/preferences" "low-color/preferences.xpm")
    ("low-color/prev-node" "low-color/prev-node.xpm")
    ("low-color/print" "low-color/print.xpm")
    ("low-color/right-arrow" "low-color/right-arrow.xpm")
    ("low-color/save" "low-color/save.xpm")
    ("low-color/saveas" "low-color/saveas.xpm")
    ("low-color/search" "low-color/search.xpm")
    ("low-color/spell" "low-color/spell.xpm")
    ("low-color/undo" "low-color/undo.xpm")
    ("low-color/up-arrow" "low-color/up-arrow.xpm")
    ("low-color/up-node" "low-color/up-node.xpm")
    ("mail/compose" "mail/compose.pbm")
    ("mail/compose" "mail/compose.xpm")
    ("mail/copy" "mail/copy.pbm")
    ("mail/copy" "mail/copy.xpm")
    ("mail/flag-for-followup" "mail/flag-for-followup.pbm")
    ("mail/flag-for-followup" "mail/flag-for-followup.xpm")
    ("mail/forward" "mail/forward.pbm")
    ("mail/forward" "mail/forward.xpm")
    ("mail/inbox" "mail/inbox.pbm")
    ("mail/inbox" "mail/inbox.xpm")
    ("mail/move" "mail/move.pbm")
    ("mail/move" "mail/move.xpm")
    ("mail/not-spam" "mail/not-spam.pbm")
    ("mail/not-spam" "mail/not-spam.xpm")
    ("mail/outbox" "mail/outbox.pbm")
    ("mail/outbox" "mail/outbox.xpm")
    ("mail/preview" "mail/preview.pbm")
    ("mail/preview" "mail/preview.xpm")
    ("mail/repack" "mail/repack.pbm")
    ("mail/repack" "mail/repack.xpm")
    ("mail/reply" "mail/reply.pbm")
    ("mail/reply" "mail/reply.xpm")
    ("mail/reply-all" "mail/reply-all.pbm")
    ("mail/reply-all" "mail/reply-all.xpm")
    ("mail/reply-from" "mail/reply-from.pbm")
    ("mail/reply-from" "mail/reply-from.xpm")
    ("mail/reply-to" "mail/reply-to.pbm")
    ("mail/reply-to" "mail/reply-to.xpm")
    ("mail/save" "mail/save.xpm")
    ("mail/save-draft" "mail/save-draft.pbm")
    ("mail/save-draft" "mail/save-draft.xpm")
    ("mail/send" "mail/send.pbm")
    ("mail/send" "mail/send.xpm")
    ("mail/spam" "mail/spam.xpm")
    ("mh-logo" "mh-logo.pbm")
    ("mh-logo" "mh-logo.xpm")
    ("mpc/add" "mpc/add.pbm")
    ("mpc/add" "mpc/add.xpm")
    ("mpc/ffwd" "mpc/ffwd.pbm")
    ("mpc/ffwd" "mpc/ffwd.xpm")
    ("mpc/next" "mpc/next.pbm")
    ("mpc/next" "mpc/next.xpm")
    ("mpc/pause" "mpc/pause.pbm")
    ("mpc/pause" "mpc/pause.xpm")
    ("mpc/play" "mpc/play.pbm")
    ("mpc/play" "mpc/play.xpm")
    ("mpc/prev" "mpc/prev.pbm")
    ("mpc/prev" "mpc/prev.xpm")
    ("mpc/rewind" "mpc/rewind.pbm")
    ("mpc/rewind" "mpc/rewind.xpm")
    ("mpc/stop" "mpc/stop.pbm")
    ("mpc/stop" "mpc/stop.xpm")
    ("new" "new.pbm")
    ("new" "new.xpm")
    ("newsticker/browse-url" "newsticker/browse-url.xpm")
    ("newsticker/get-all" "newsticker/get-all.xpm")
    ("newsticker/mark-immortal" "newsticker/mark-immortal.xpm")
    ("newsticker/mark-read" "newsticker/mark-read.xpm")
    ("newsticker/narrow" "newsticker/narrow.xpm")
    ("newsticker/next-feed" "newsticker/next-feed.xpm")
    ("newsticker/next-item" "newsticker/next-item.xpm")
    ("newsticker/prev-feed" "newsticker/prev-feed.xpm")
    ("newsticker/prev-item" "newsticker/prev-item.xpm")
    ("newsticker/rss-feed" "newsticker/rss-feed.png")
    ("newsticker/rss-feed" "newsticker/rss-feed.svg")
    ("newsticker/update" "newsticker/update.xpm")
    ("next-node" "next-node.pbm")
    ("next-node" "next-node.xpm")
    ("next-page" "next-page.pbm")
    ("next-page" "next-page.xpm")
    ("open" "open.pbm")
    ("open" "open.xpm")
    ("paste" "paste.pbm")
    ("paste" "paste.xpm")
    ("preferences" "preferences.pbm")
    ("preferences" "preferences.xpm")
    ("prev-node" "prev-node.pbm")
    ("prev-node" "prev-node.xpm")
    ("print" "print.pbm")
    ("print" "print.xpm")
    ("radio" "radio.svg")
    ("radio-checked" "radio-checked.svg")
    ("radio-mixed" "radio-mixed.svg")
    ("redo" "redo.pbm")
    ("redo" "redo.xpm")
    ("refresh" "refresh.pbm")
    ("refresh" "refresh.xpm")
    ("right" "right.svg")
    ("right-arrow" "right-arrow.pbm")
    ("right-arrow" "right-arrow.xpm")
    ("save" "save.pbm")
    ("save" "save.xpm")
    ("saveas" "saveas.pbm")
    ("saveas" "saveas.xpm")
    ("search" "search.pbm")
    ("search" "search.xpm")
    ("search-replace" "search-replace.pbm")
    ("search-replace" "search-replace.xpm")
    ("separator" "separator.pbm")
    ("separator" "separator.xpm")
    ("show" "show.pbm")
    ("show" "show.xpm")
    ("smilies/blink" "smilies/blink.pbm")
    ("smilies/blink" "smilies/blink.xpm")
    ("smilies/braindamaged" "smilies/braindamaged.pbm")
    ("smilies/braindamaged" "smilies/braindamaged.xpm")
    ("smilies/cry" "smilies/cry.pbm")
    ("smilies/cry" "smilies/cry.xpm")
    ("smilies/dead" "smilies/dead.pbm")
    ("smilies/dead" "smilies/dead.xpm")
    ("smilies/evil" "smilies/evil.pbm")
    ("smilies/evil" "smilies/evil.xpm")
    ("smilies/forced" "smilies/forced.pbm")
    ("smilies/forced" "smilies/forced.xpm")
    ("smilies/frown" "smilies/frown.pbm")
    ("smilies/frown" "smilies/frown.xpm")
    ("smilies/grin" "smilies/grin.pbm")
    ("smilies/grin" "smilies/grin.xpm")
    ("smilies/indifferent" "smilies/indifferent.pbm")
    ("smilies/indifferent" "smilies/indifferent.xpm")
    ("smilies/sad" "smilies/sad.pbm")
    ("smilies/sad" "smilies/sad.xpm")
    ("smilies/smile" "smilies/smile.pbm")
    ("smilies/smile" "smilies/smile.xpm")
    ("smilies/wry" "smilies/wry.pbm")
    ("smilies/wry" "smilies/wry.xpm")
    ("sort-ascending" "sort-ascending.pbm")
    ("sort-ascending" "sort-ascending.xpm")
    ("sort-column-ascending" "sort-column-ascending.pbm")
    ("sort-column-ascending" "sort-column-ascending.xpm")
    ("sort-criteria" "sort-criteria.pbm")
    ("sort-criteria" "sort-criteria.xpm")
    ("sort-descending" "sort-descending.pbm")
    ("sort-descending" "sort-descending.xpm")
    ("sort-row-ascending" "sort-row-ascending.pbm")
    ("sort-row-ascending" "sort-row-ascending.xpm")
    ("spell" "spell.pbm")
    ("spell" "spell.xpm")
    ("tabs/close" "tabs/close.xpm")
    ("tabs/left-arrow" "tabs/left-arrow.xpm")
    ("tabs/new" "tabs/new.xpm")
    ("tabs/right-arrow" "tabs/right-arrow.xpm")
    ("tree-widget/default/close" "tree-widget/default/close.png")
    ("tree-widget/default/close" "tree-widget/default/close.xpm")
    ("tree-widget/default/empty" "tree-widget/default/empty.png")
    ("tree-widget/default/empty" "tree-widget/default/empty.xpm")
    ("tree-widget/default/end-guide" "tree-widget/default/end-guide.png")
    ("tree-widget/default/end-guide" "tree-widget/default/end-guide.xpm")
    ("tree-widget/default/guide" "tree-widget/default/guide.png")
    ("tree-widget/default/guide" "tree-widget/default/guide.xpm")
    ("tree-widget/default/handle" "tree-widget/default/handle.png")
    ("tree-widget/default/handle" "tree-widget/default/handle.xpm")
    ("tree-widget/default/leaf" "tree-widget/default/leaf.png")
    ("tree-widget/default/leaf" "tree-widget/default/leaf.xpm")
    ("tree-widget/default/no-guide" "tree-widget/default/no-guide.png")
    ("tree-widget/default/no-guide" "tree-widget/default/no-guide.xpm")
    ("tree-widget/default/no-handle" "tree-widget/default/no-handle.png")
    ("tree-widget/default/no-handle" "tree-widget/default/no-handle.xpm")
    ("tree-widget/default/open" "tree-widget/default/open.png")
    ("tree-widget/default/open" "tree-widget/default/open.xpm")
    ("tree-widget/folder/close" "tree-widget/folder/close.png")
    ("tree-widget/folder/close" "tree-widget/folder/close.xpm")
    ("tree-widget/folder/empty" "tree-widget/folder/empty.png")
    ("tree-widget/folder/empty" "tree-widget/folder/empty.xpm")
    ("tree-widget/folder/end-guide" "tree-widget/folder/end-guide.png")
    ("tree-widget/folder/end-guide" "tree-widget/folder/end-guide.xpm")
    ("tree-widget/folder/guide" "tree-widget/folder/guide.png")
    ("tree-widget/folder/guide" "tree-widget/folder/guide.xpm")
    ("tree-widget/folder/handle" "tree-widget/folder/handle.png")
    ("tree-widget/folder/handle" "tree-widget/folder/handle.xpm")
    ("tree-widget/folder/leaf" "tree-widget/folder/leaf.png")
    ("tree-widget/folder/leaf" "tree-widget/folder/leaf.xpm")
    ("tree-widget/folder/no-guide" "tree-widget/folder/no-guide.png")
    ("tree-widget/folder/no-guide" "tree-widget/folder/no-guide.xpm")
    ("tree-widget/folder/no-handle" "tree-widget/folder/no-handle.png")
    ("tree-widget/folder/no-handle" "tree-widget/folder/no-handle.xpm")
    ("tree-widget/folder/open" "tree-widget/folder/open.png")
    ("tree-widget/folder/open" "tree-widget/folder/open.xpm")
    ("unchecked" "unchecked.pbm")
    ("unchecked" "unchecked.svg")
    ("unchecked" "unchecked.xpm")
    ("undo" "undo.pbm")
    ("undo" "undo.xpm")
    ("up" "up.svg")
    ("up-arrow" "up-arrow.pbm")
    ("up-arrow" "up-arrow.xpm")
    ("up-node" "up-node.pbm")
    ("up-node" "up-node.xpm")
    ("zoom-in" "zoom-in.pbm")
    ("zoom-in" "zoom-in.xpm")
    ("zoom-out" "zoom-out.pbm")
    ("zoom-out" "zoom-out.xpm")
    ;; ("smilies/grayscale/blink" "smilies/grayscale/blink.xpm")
    ;; ("smilies/grayscale/braindamaged" "smilies/grayscale/braindamaged.xpm")
    ;; ("smilies/grayscale/cry" "smilies/grayscale/cry.xpm")
    ;; ("smilies/grayscale/dead" "smilies/grayscale/dead.xpm")
    ;; ("smilies/grayscale/evil" "smilies/grayscale/evil.xpm")
    ;; ("smilies/grayscale/forced" "smilies/grayscale/forced.xpm")
    ;; ("smilies/grayscale/frown" "smilies/grayscale/frown.xpm")
    ;; ("smilies/grayscale/grin" "smilies/grayscale/grin.xpm")
    ;; ("smilies/grayscale/indifferent" "smilies/grayscale/indifferent.xpm")
    ;; ("smilies/grayscale/reverse-smile" "smilies/grayscale/reverse-smile.xpm")
    ;; ("smilies/grayscale/sad" "smilies/grayscale/sad.xpm")
    ;; ("smilies/grayscale/smile" "smilies/grayscale/smile.xpm")
    ;; ("smilies/grayscale/wry" "smilies/grayscale/wry.xpm")
    ;; ("smilies/medium/blink" "smilies/medium/blink.xpm")
    ;; ("smilies/medium/braindamaged" "smilies/medium/braindamaged.xpm")
    ;; ("smilies/medium/cry" "smilies/medium/cry.xpm")
    ;; ("smilies/medium/dead" "smilies/medium/dead.xpm")
    ;; ("smilies/medium/evil" "smilies/medium/evil.xpm")
    ;; ("smilies/medium/forced" "smilies/medium/forced.xpm")
    ;; ("smilies/medium/frown" "smilies/medium/frown.xpm")
    ;; ("smilies/medium/grin" "smilies/medium/grin.xpm")
    ;; ("smilies/medium/indifferent" "smilies/medium/indifferent.xpm")
    ;; ("smilies/medium/reverse-smile" "smilies/medium/reverse-smile.xpm")
    ;; ("smilies/medium/sad" "smilies/medium/sad.xpm")
    ;; ("smilies/medium/smile" "smilies/medium/smile.xpm")
    ;; ("smilies/medium/wry" "smilies/medium/wry.xpm")
    ;; ("splash" "splash.bmp")
    ;; ("splash" "splash.pbm")
    ;; ("splash" "splash.png")
    ;; ("splash" "splash.svg")
    ;; ("splash" "splash.xpm")
    ))

(provide 'icons)
;;; icons.el ends here
