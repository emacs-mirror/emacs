;;; icons.el --- Handling icons  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>
;; Keywords: icons buttons

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

(require 'cl-lib)

(defface icon
  '((t :underline nil))
  "Face for buttons."
  :version "29.1"
  :group 'customize)

(defface icon-button
  '((((type x w32 ns haiku pgtk) (class color))
     :inherit icon
     :box (:line-width (3 . -1) :color "#404040" :style flat-button)
     :background "#808080"
     :foreground "black"))
  "Face for buttons."
  :version "29.1"
  :group 'customize)

(defcustom icon-preference '(image emoji symbol text)
  "List of icon types to use, in order of preference.
Emacs will choose the icon of the highest preference possible
on the current display, and \"degrade\" gracefully to an icon
type that's available."
  :version "29.1"
  :group 'customize
  :type '(repeat (choice (const :tag "Images" image)
                         (const :tag "Colorful Emojis" emoji)
                         (const :tag "Monochrome Symbols" symbol)
                         (const :tag "Text Only" text))))

(defmacro define-icon (name parent specification documentation &rest keywords)
  "Define an icon identified by NAME.
If non-nil, inherit the specification from PARENT.  Entries from
SPECIFICATION will override inherited specifications.

SPECIFICATION is an alist of entries where the first element is
the type, and the rest are icons of that type.  Valid types are
`image', `emoji', `symbol' and `text'.

KEYWORDS specify additional information.  Valid keywords are:

`:version': The first Emacs version to include this icon; this is
mandatory.

`:group': The customization group the icon belongs in; this is
inferred if not present.

`:help-echo': Informational text that explains what happens if
the icon is used as a button and you click it."
  (declare (indent 2))
  (unless (symbolp name)
    (error "NAME must be a symbol: %S" name))
  (unless (plist-get keywords :version)
    (error "There must be a :version keyword in `define-icon'"))
  `(icons--register ',name ',parent ,specification ,documentation
                    ',keywords))

(defun icons--register (name parent spec doc keywords)
  (put name 'icon--properties (list parent spec doc keywords))
  (custom-add-to-group
   (or (plist-get keywords :group)
       (custom-current-group))
   name 'custom-icon))

(defun icon-spec-keywords (spec)
  (seq-drop-while (lambda (e) (not (keywordp e))) (cdr spec)))

(defun icon-spec-values (spec)
  (seq-take-while (lambda (e) (not (keywordp e))) (cdr spec)))

(defun iconp (object)
  "Return nil if OBJECT is not an icon.
If OBJECT is an icon, return the icon properties."
  (get object 'icon--properties))

(defun icon-documentation (icon)
  "Return the documentation for ICON."
  (let ((props (iconp icon)))
    (unless props
      (user-error "%s is not a valid icon" icon))
    (nth 2 props)))

(defun icons--spec (icon)
  (nth 1 (iconp icon)))

(defun icons--copy-spec (spec)
  (mapcar #'copy-sequence spec))

(defun icon-complete-spec (icon &optional inhibit-theme inhibit-inheritance)
  "Return the merged spec for ICON."
  (pcase-let ((`(,parent ,spec _ _) (iconp icon)))
    ;; We destructively modify `spec' when merging, so copy it.
    (setq spec (icons--copy-spec spec))
    ;; Let the Customize theme override.
    (unless inhibit-theme
      (when-let ((theme-spec (cadr (car (get icon 'theme-icon)))))
        (setq spec (icons--merge-spec (icons--copy-spec theme-spec) spec))))
    ;; Inherit from the parent spec (recursively).
    (unless inhibit-inheritance
      (while parent
        (let ((parent-props (get parent 'icon--properties)))
          (when parent-props
            (setq spec (icons--merge-spec spec (cadr parent-props))))
          (setq parent (car parent-props)))))
    spec))

(defun icon-string (name)
  "Return a string suitable for display in the current buffer for icon NAME."
  (let ((props (iconp name)))
    (unless props
      (user-error "%s is not a valid icon" name))
    (pcase-let ((`(_ ,spec _ ,keywords) props))
      (setq spec (icon-complete-spec name))
      ;; We now have a full spec, so check the intersection of what
      ;; the user wants and what this Emacs is capable of showing.
      (let ((icon-string
             (catch 'found
               (dolist (type icon-preference)
                 (let* ((type-spec (assq type spec))
                        ;; Find the keywords at the end of the section
                        ;; (if any).
                        (type-keywords (icon-spec-keywords type-spec)))
                   ;; Go through all the variations in this section
                   ;; and return the first one we can display.
                   (dolist (icon (icon-spec-values type-spec))
                     (when-let ((result
                                 (icons--create type icon type-keywords)))
                       (throw 'found
                              (if-let ((face (plist-get type-keywords :face)))
                                  (propertize result 'face face)
                                result)))))))))
        (unless icon-string
          (error "Couldn't find any way to display the %s icon" name))
        (when-let ((help (plist-get keywords :help-echo)))
          (setq icon-string (propertize icon-string 'help-echo help)))
        (propertize icon-string 'rear-nonsticky t)))))

(defun icon-elements (name)
  "Return the elements of icon NAME.
The elements are represented as a plist where the keys are
`string', `face' and `image'.  The `image' element is only
present if the icon is represented by an image."
  (let ((string (icon-string name)))
    (list 'face (get-text-property 0 'face string)
          'image (get-text-property 0 'display string)
          'string (substring-no-properties string))))

(defun icons--merge-spec (merged parent-spec)
  (dolist (elem parent-spec)
    (let ((current (assq (car elem) merged)))
      (if (not current)
          ;; Just add the entry.
          (push elem merged)
        ;; See if there are any keywords to inherit.
        (let ((parent-keywords (icon-spec-keywords elem))
              (current-keywords (icon-spec-keywords current)))
          (while parent-keywords
            (unless (plist-get current-keywords (car parent-keywords))
              (nconc current (take 2 parent-keywords)))
            (setq parent-keywords (cddr parent-keywords)))))))
  merged)

(cl-defmethod icons--create ((_type (eql 'image)) icon keywords)
  (let* ((file (if (file-name-absolute-p icon)
                   icon
                 (and (fboundp 'image-search-load-path)
                      (image-search-load-path icon))))
         (file-exists (and (stringp file) (file-readable-p file))))
    (and file-exists
         (display-images-p)
         (fboundp 'image-supported-file-p)
         (image-supported-file-p file)
         (propertize
          " " 'display
          (let ((props
                 (append
                  (if-let ((height (plist-get keywords :height)))
                      (list :height (if (eq height 'line)
                                        (window-default-line-height)
                                      height)))
                  (if-let ((width (plist-get keywords :width)))
                      (list :width (if (eq width 'font)
                                       (default-font-width)
                                     width)))
                  '(:scale 1)
                  (if-let ((rotation (plist-get keywords :rotation)))
                      (list :rotation rotation))
                  (if-let ((margin (plist-get keywords :margin)))
                      (list :margin margin))
                  (list :ascent (if (plist-member keywords :ascent)
                                    (plist-get keywords :ascent)
                                  'center)))))
            (apply 'create-image file nil nil props))))))

(cl-defmethod icons--create ((_type (eql 'emoji)) icon _keywords)
  (when-let ((font (and (display-multi-font-p)
                        ;; FIXME: This is not enough for ensuring
                        ;; display of color Emoji.
                        (car (internal-char-font nil ?🟠)))))
    (and (font-has-char-p font (aref icon 0))
         icon)))

(cl-defmethod icons--create ((_type (eql 'symbol)) icon _keywords)
  (and (cl-every #'char-displayable-p icon)
       icon))

(cl-defmethod icons--create ((_type (eql 'text)) icon _keywords)
  icon)

(define-icon button nil
  '((image :face icon-button)
    (emoji "🔵" :face icon)
    (symbol "●" :face icon-button)
    (text "button" :face icon-button))
  "Base icon for buttons."
  :version "29.1")

;;;###autoload
(defun describe-icon (icon)
  "Pop to a buffer to describe ICON."
  (interactive
   (list (intern (completing-read "Describe icon: " obarray 'iconp t))))
  (let ((help-buffer-under-preparation t))
    (help-setup-xref (list #'describe-icon icon)
		     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (insert "Icon: " (symbol-name icon) "\n\n")
        (insert "Documentation:\n"
                (substitute-command-keys (icon-documentation icon)))
        (ensure-empty-lines)
        (let ((spec (icon-complete-spec icon))
              (plain (icon-complete-spec icon t t)))
          (insert "Specification including inheritance and theming:\n")
          (icons--describe-spec spec)
          (unless (equal spec plain)
            (insert "\nSpecification not including inheritance and theming:\n")
            (icons--describe-spec plain)))))))

(defun icons--describe-spec (spec)
  (dolist (elem spec)
    (let ((type (car elem))
          (values (icon-spec-values elem))
          (keywords (icon-spec-keywords elem)))
      (when (or values keywords)
        (insert (format "\nType: %s\n" type))
        (dolist (value values)
          (insert (format "  %s\n" value)))
        (while keywords
          (insert (format "    %s: %s\n" (pop keywords) (pop keywords))))))))

(provide 'icons)

;;; icons.el ends here
