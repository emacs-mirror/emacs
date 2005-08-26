;;; tree-widget.el --- Tree widget

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 16 Feb 2001
;; Keywords: extensions

;; This file is part of GNU Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provide a tree widget useful to display data
;; structures organized in a hierarchical order.
;;
;; The following properties are specific to the tree widget:
;;
;; :open
;;    Set to non-nil to expand the tree.  By default the tree is
;;    collapsed.
;;
;; :node
;;    Specify the widget used to represent the value of a tree node.
;;    By default this is an `item' widget which displays the
;;    tree-widget :tag property value if defined, or a string
;;    representation of the tree-widget value.
;;
;; :keep
;;    Specify a list of properties to keep when the tree is collapsed
;;    so they can be recovered when the tree is expanded.  This
;;    property can be used in child widgets too.
;;
;; :expander (obsoletes :dynargs)
;;    Specify a function to be called to dynamically provide the
;;    tree's children in response to an expand request.  This function
;;    will be passed the tree widget and must return a list of child
;;    widgets.
;;
;;    *Please note:* Child widgets returned by the :expander function
;;    are stored in the :args property of the tree widget.  To speed
;;    up successive expand requests, the :expander function is not
;;    called again when the :args value is non-nil.  To refresh child
;;    values, it is necessary to set the :args property to nil, then
;;    redraw the tree.
;;
;; :open-control  (default `tree-widget-open-control')
;; :close-control (default `tree-widget-close-control')
;; :empty-control (default `tree-widget-empty-control')
;; :leaf-control  (default `tree-widget-leaf-control')
;; :guide         (default `tree-widget-guide')
;; :end-guide     (default `tree-widget-end-guide')
;; :no-guide      (default `tree-widget-no-guide')
;; :handle        (default `tree-widget-handle')
;; :no-handle     (default `tree-widget-no-handle')
;;    Those properties define the widgets used to draw the tree, and
;;    permit to customize its look and feel.  For example, using
;;    `item' widgets with these :tag values:
;;
;;    open-control     "[-] "      (OC)
;;    close-control    "[+] "      (CC)
;;    empty-control    "[X] "      (EC)
;;    leaf-control     "[>] "      (LC)
;;    guide            " |"        (GU)
;;    noguide          "  "        (NG)
;;    end-guide        " `"        (EG)
;;    handle           "-"         (HA)
;;    no-handle        " "         (NH)
;;
;;    A tree will look like this:
;;
;;    [-] 1                        (OC :node)
;;     |-[+] 1.0                   (GU+HA+CC :node)
;;     |-[X] 1.1                   (GU+HA+EC :node)
;;     `-[-] 1.2                   (EG+HA+OC :node)
;;        |-[>] 1.2.1              (NG+NH+GU+HA+LC child)
;;        `-[>] 1.2.2              (NG+NH+EG+HA+LC child)
;;
;; By default, images will be used instead of strings to draw a
;; nice-looking tree.  See the `tree-widget-image-enable',
;; `tree-widget-themes-directory', and `tree-widget-theme' options for
;; more details.

;;; History:
;;

;;; Code:
(eval-when-compile (require 'cl))
(require 'wid-edit)

;;; Customization
;;
(defgroup tree-widget nil
  "Customization support for the Tree Widget library."
  :version "22.1"
  :group 'widgets)

(defcustom tree-widget-image-enable
  (not (or (featurep 'xemacs) (< emacs-major-version 21)))
  "*Non-nil means that tree-widget will try to use images."
  :type  'boolean
  :group 'tree-widget)

(defcustom tree-widget-themes-directory "tree-widget"
  "*Name of the directory where to look up for image themes.
When nil use the directory where the tree-widget library is located.
When a relative name is specified, try to locate that sub directory in
`load-path', then in the data directory, and use the first one found.
The data directory is the value of the variable `data-directory' on
Emacs, and what `(locate-data-directory \"tree-widget\")' returns on
XEmacs.
The default is to use the \"tree-widget\" relative name."
  :type '(choice (const :tag "Default" "tree-widget")
                 (const :tag "With the library" nil)
                 (directory :format "%{%t%}:\n%v"))
  :group 'tree-widget)

(defcustom tree-widget-theme nil
  "*Name of the theme where to look up for images.
It must be a sub directory of the directory specified in variable
`tree-widget-themes-directory'.  The default is \"default\".  When an
image is not found in this theme, the default theme is searched too.
A complete theme must contain images with these file names with a
supported extension (see also `tree-widget-image-formats'):

\"open\"
  Represent an expanded node.
\"close\"
  Represent a collapsed node.
\"empty\"
  Represent an expanded node with no child.
\"leaf\"
  Represent a leaf node.
\"guide\"
  A vertical guide line.
\"no-guide\"
  An invisible vertical guide line.
\"end-guide\"
  End of a vertical guide line.
\"handle\"
  Horizontal guide line that joins the vertical guide line to a node.
\"no-handle\"
  An invisible handle."
  :type '(choice (const  :tag "Default" nil)
                 (string :tag "Name"))
  :group 'tree-widget)

(defcustom tree-widget-image-properties-emacs
  '(:ascent center :mask (heuristic t))
  "*Default properties of Emacs images."
  :type 'plist
  :group 'tree-widget)

(defcustom tree-widget-image-properties-xemacs
  nil
  "*Default properties of XEmacs images."
  :type 'plist
  :group 'tree-widget)

;;; Image support
;;
(eval-and-compile ;; Emacs/XEmacs compatibility stuff
  (cond
   ;; XEmacs
   ((featurep 'xemacs)
    (defsubst tree-widget-use-image-p ()
      "Return non-nil if image support is currently enabled."
      (and tree-widget-image-enable
           widget-glyph-enable
           (console-on-window-system-p)))
    (defsubst tree-widget-create-image (type file &optional props)
      "Create an image of type TYPE from FILE, and return it.
Give the image the specified properties PROPS."
      (apply 'make-glyph `([,type :file ,file ,@props])))
    (defsubst tree-widget-image-formats ()
      "Return the alist of image formats/file name extensions.
See also the option `widget-image-file-name-suffixes'."
      (delq nil
            (mapcar
             #'(lambda (fmt)
                 (and (valid-image-instantiator-format-p (car fmt)) fmt))
             widget-image-file-name-suffixes)))
    )
   ;; Emacs
   (t
    (defsubst tree-widget-use-image-p ()
      "Return non-nil if image support is currently enabled."
      (and tree-widget-image-enable
           widget-image-enable
           (display-images-p)))
    (defsubst tree-widget-create-image (type file &optional props)
      "Create an image of type TYPE from FILE, and return it.
Give the image the specified properties PROPS."
      (apply 'create-image `(,file ,type nil ,@props)))
    (defsubst tree-widget-image-formats ()
      "Return the alist of image formats/file name extensions.
See also the option `widget-image-file-name-suffixes'."
      (delq nil
            (mapcar
             #'(lambda (fmt)
                 (and (image-type-available-p (car fmt)) fmt))
             widget-image-conversion)))
    ))
  )

;; Buffer local cache of theme data.
(defvar tree-widget--theme nil)

(defsubst tree-widget-theme-name ()
  "Return the current theme name, or nil if no theme is active."
  (and tree-widget--theme (aref tree-widget--theme 0)))

(defsubst tree-widget-set-theme (&optional name)
  "In the current buffer, set the theme to use for images.
The current buffer must be where the tree widget is drawn.
Optional argument NAME is the name of the theme to use.  It defaults
to the value of the variable `tree-widget-theme'.
Does nothing if NAME is already the current theme."
  (or name (setq name (or tree-widget-theme "default")))
  (unless (string-equal name (tree-widget-theme-name))
    (set (make-local-variable 'tree-widget--theme)
         (make-vector 4 nil))
    (aset tree-widget--theme 0 name)))

(defun tree-widget-themes-directory ()
  "Locate the directory where to search for a theme.
It is defined in variable `tree-widget-themes-directory'.
Return the absolute name of the directory found, or nil if the
specified directory is not accessible."
  (let ((found (aref tree-widget--theme 1)))
    (if found
        ;; The directory is available in the cache.
        (unless (eq found 'void) found)
      (cond
       ;; Use the directory where tree-widget is located.
       ((null tree-widget-themes-directory)
        (setq found (locate-library "tree-widget"))
        (when found
          (setq found (file-name-directory found))
          (or (file-accessible-directory-p found)
              (setq found nil))))
       ;; Check accessibility of absolute directory name.
       ((file-name-absolute-p tree-widget-themes-directory)
        (setq found (expand-file-name tree-widget-themes-directory))
        (or (file-accessible-directory-p found)
            (setq found nil)))
       ;; Locate a sub-directory in `load-path' and data directory.
       (t
        (let ((path
               (append load-path
                       (list (if (fboundp 'locate-data-directory)
                                 ;; XEmacs
                                 (locate-data-directory "tree-widget")
                               ;; Emacs
                               data-directory)))))
          (while (and path (not found))
            (when (car path)
              (setq found (expand-file-name
                           tree-widget-themes-directory (car path)))
              (or (file-accessible-directory-p found)
                  (setq found nil)))
            (setq path (cdr path))))))
      ;; Store the result in the cache for later use.
      (aset tree-widget--theme 1 (or found 'void))
      found)))

(defsubst tree-widget-set-image-properties (props)
  "In current theme, set images properties to PROPS."
  (aset tree-widget--theme 2 props))

(defun tree-widget-image-properties (file)
  "Return the properties of an image in current theme.
FILE is the absolute file name of an image.

If there is a \"tree-widget-theme-setup\" library in the theme
directory, where is located FILE, load it to setup theme images
properties.  Typically it should contain something like this:

  (tree-widget-set-image-properties
   (if (featurep 'xemacs)
       '(:ascent center)
     '(:ascent center :mask (heuristic t))
     ))

Default global properties are provided for respectively Emacs and
XEmacs in the variables `tree-widget-image-properties-emacs', and
`tree-widget-image-properties-xemacs'."
  ;; If properties are in the cache, use them.
  (let ((plist (aref tree-widget--theme 2)))
    (unless plist
      ;; Load tree-widget-theme-setup if available.
      (load (expand-file-name "tree-widget-theme-setup"
                              (file-name-directory file)) t t)
      ;; If properties have been setup, use them.
      (unless (setq plist (aref tree-widget--theme 2))
        ;; By default, use supplied global properties.
        (setq plist (if (featurep 'xemacs)
                        tree-widget-image-properties-xemacs
                      tree-widget-image-properties-emacs))
        ;; Setup the cache.
        (tree-widget-set-image-properties plist)))
    plist))

(defconst tree-widget--cursors
  ;; Pointer shapes when the mouse pointer is over tree-widget images.
  ;; This feature works since Emacs 22, and ignored on older versions,
  ;; and XEmacs.
  '(
    ("open"      . hand )
    ("close"     . hand )
    ("empty"     . arrow)
    ("leaf"      . arrow)
    ("guide"     . arrow)
    ("no-guide"  . arrow)
    ("end-guide" . arrow)
    ("handle"    . arrow)
    ("no-handle" . arrow)
    ))

(defun tree-widget-lookup-image (name)
  "Look up in current theme for an image with NAME.
Search first in current theme, then in default theme (see also the
variable `tree-widget-theme').
Return the first image found having a supported format, or nil if not
found."
  (let ((default-directory (tree-widget-themes-directory)))
    (when default-directory
      (let (file (theme (tree-widget-theme-name)))
        (catch 'found
          (dolist (dir (if (string-equal theme "default")
                           '("default") (list theme "default")))
            (dolist (fmt (tree-widget-image-formats))
              (dolist (ext (cdr fmt))
                (setq file (expand-file-name (concat name ext) dir))
                (and
                 (file-readable-p file)
                 (file-regular-p file)
                 (throw
                  'found
                  (tree-widget-create-image
                   (car fmt) file
                   ;; Add the pointer shape
                   (cons :pointer
                         (cons
                          (cdr (assoc name tree-widget--cursors))
                          (tree-widget-image-properties file)))))))))
          nil)))))

(defun tree-widget-find-image (name)
  "Find the image with NAME in current theme.
NAME is an image file name sans extension.
Return the image found, or nil if not found."
  (when (tree-widget-use-image-p)
    ;; Ensure there is an active theme.
    (tree-widget-set-theme (tree-widget-theme-name))
    (let ((image (assoc name (aref tree-widget--theme 3))))
      ;; The image NAME is found in the cache.
      (if image
          (cdr image)
        ;; Search the image in current, and default themes.
        (prog1
            (setq image (tree-widget-lookup-image name))
          ;; Store image reference in the cache for later use.
          (push (cons name image) (aref tree-widget--theme 3))))
      )))

;;; Widgets
;;
(defvar tree-widget-button-keymap
  (let ((km (make-sparse-keymap)))
    (if (boundp 'widget-button-keymap)
        ;; XEmacs
        (progn
          (set-keymap-parent km widget-button-keymap)
          (define-key km [button1] 'widget-button-click))
      ;; Emacs
      (set-keymap-parent km widget-keymap)
      (define-key km [down-mouse-1] 'widget-button-click))
    km)
  "Keymap used inside node buttons.
Handle mouse button 1 click on buttons.")

(define-widget 'tree-widget-control 'push-button
  "Basic widget other tree-widget node buttons are derived from."
  :format        "%[%t%]"
  :button-keymap tree-widget-button-keymap ; XEmacs
  :keymap        tree-widget-button-keymap ; Emacs
  )

(define-widget 'tree-widget-open-control 'tree-widget-control
  "Button for an expanded tree-widget node."
  :tag       "[-] "
  ;;:tag-glyph (tree-widget-find-image "open")
  :notify    'tree-widget-close-node
  :help-echo "Collapse node"
  )

(define-widget 'tree-widget-empty-control 'tree-widget-open-control
  "Button for an expanded tree-widget node with no child."
  :tag       "[X] "
  ;;:tag-glyph (tree-widget-find-image "empty")
  )

(define-widget 'tree-widget-close-control 'tree-widget-control
  "Button for a collapsed tree-widget node."
  :tag       "[+] "
  ;;:tag-glyph (tree-widget-find-image "close")
  :notify    'tree-widget-open-node
  :help-echo "Expand node"
  )

(define-widget 'tree-widget-leaf-control 'item
  "Representation of a tree-widget leaf node."
  :tag       " " ;; Need at least one char to display the image :-(
  ;;:tag-glyph (tree-widget-find-image "leaf")
  :format    "%t"
  )

(define-widget 'tree-widget-guide 'item
  "Vertical guide line."
  :tag       " |"
  ;;:tag-glyph (tree-widget-find-image "guide")
  :format    "%t"
  )

(define-widget 'tree-widget-end-guide 'item
  "End of a vertical guide line."
  :tag       " `"
  ;;:tag-glyph (tree-widget-find-image "end-guide")
  :format    "%t"
  )

(define-widget 'tree-widget-no-guide 'item
  "Invisible vertical guide line."
  :tag       "  "
  ;;:tag-glyph (tree-widget-find-image "no-guide")
  :format    "%t"
  )

(define-widget 'tree-widget-handle 'item
  "Horizontal guide line that joins a vertical guide line to a node."
  :tag       " "
  ;;:tag-glyph (tree-widget-find-image "handle")
  :format    "%t"
  )

(define-widget 'tree-widget-no-handle 'item
  "Invisible handle."
  :tag       " "
  ;;:tag-glyph (tree-widget-find-image "no-handle")
  :format    "%t"
  )

(define-widget 'tree-widget 'default
  "Tree widget."
  :format         "%v"
  :convert-widget 'widget-types-convert-widget
  :value-get      'widget-value-value-get
  :value-delete   'widget-children-value-delete
  :value-create   'tree-widget-value-create
  :open-control   'tree-widget-open-control
  :close-control  'tree-widget-close-control
  :empty-control  'tree-widget-empty-control
  :leaf-control   'tree-widget-leaf-control
  :guide          'tree-widget-guide
  :end-guide      'tree-widget-end-guide
  :no-guide       'tree-widget-no-guide
  :handle         'tree-widget-handle
  :no-handle      'tree-widget-no-handle
  )

;;; Widget support functions
;;
(defun tree-widget-p (widget)
  "Return non-nil if WIDGET is a tree-widget."
  (let ((type (widget-type widget)))
    (while (and type (not (eq type 'tree-widget)))
      (setq type (widget-type (get type 'widget-type))))
    (eq type 'tree-widget)))

(defun tree-widget-node (widget)
  "Return WIDGET's :node child widget.
If not found, setup an `item' widget as default.
Signal an error if the :node widget is a tree-widget.
WIDGET is, or derives from, a tree-widget."
  (let ((node (widget-get widget :node)))
    (if node
        ;; Check that the :node widget is not a tree-widget.
        (and (tree-widget-p node)
             (error "Invalid tree-widget :node %S" node))
      ;; Setup an item widget as default :node.
      (setq node `(item :tag ,(or (widget-get widget :tag)
                                  (widget-princ-to-string
                                   (widget-value widget)))))
      (widget-put widget :node node))
    node))

(defun tree-widget-keep (arg widget)
  "Save in ARG the WIDGET's properties specified by :keep."
  (dolist (prop (widget-get widget :keep))
    (widget-put arg prop (widget-get widget prop))))

(defun tree-widget-children-value-save (widget &optional args node)
  "Save WIDGET children values.
WIDGET is, or derives from, a tree-widget.
Children properties and values are saved in ARGS if non-nil, else in
WIDGET's :args property value.  Properties and values of the
WIDGET's :node sub-widget are saved in NODE if non-nil, else in
WIDGET's :node sub-widget."
  (let ((args (cons (or node (widget-get widget :node))
                    (or args (widget-get widget :args))))
        (children (widget-get widget :children))
        arg child)
    (while (and args children)
      (setq arg      (car args)
            args     (cdr args)
            child    (car children)
            children (cdr children))
       (if (tree-widget-p child)
;;;; The child is a tree node.
           (progn
             ;; Backtrack :args and :node properties.
             (widget-put arg :args (widget-get child :args))
             (widget-put arg :node (widget-get child :node))
             ;; Save :open property.
             (widget-put arg :open (widget-get child :open))
             ;; The node is open.
             (when (widget-get child :open)
               ;; Save the widget value.
               (widget-put arg :value (widget-value child))
               ;; Save properties specified in :keep.
               (tree-widget-keep arg child)
               ;; Save children.
               (tree-widget-children-value-save
                child (widget-get arg :args) (widget-get arg :node))))
;;;; Another non tree node.
         ;; Save the widget value.
         (widget-put arg :value (widget-value child))
         ;; Save properties specified in :keep.
         (tree-widget-keep arg child)))))

(defvar tree-widget-after-toggle-functions nil
  "Hooks run after toggling a tree-widget expansion.
Each function will receive the tree-widget as its unique argument.
This hook should be local in the buffer used to display widgets.")

(defun tree-widget-close-node (widget &rest ignore)
  "Collapse the tree-widget, parent of WIDGET.
WIDGET is, or derives from, a tree-widget-open-control widget.
IGNORE other arguments."
  (let ((tree (widget-get widget :parent)))
    ;; Before to collapse the node, save children values so next open
    ;; can recover them.
    (tree-widget-children-value-save tree)
    (widget-put tree :open nil)
    (widget-value-set tree nil)
    (run-hook-with-args 'tree-widget-after-toggle-functions tree)))

(defun tree-widget-open-node (widget &rest ignore)
  "Expand the tree-widget, parent of WIDGET.
WIDGET is, or derives from, a tree-widget-close-control widget.
IGNORE other arguments."
  (let ((tree (widget-get widget :parent)))
    (widget-put tree :open t)
    (widget-value-set tree t)
    (run-hook-with-args 'tree-widget-after-toggle-functions tree)))

(defun tree-widget-value-create (tree)
  "Create the TREE tree-widget."
  (let* ((node   (tree-widget-node tree))
         (flags  (widget-get tree :tree-widget--guide-flags))
         (indent (widget-get tree :indent))
         ;; Setup widget's image support.  Looking up for images, and
         ;; setting widgets' :tag-glyph is done here, to allow to
         ;; dynamically change the image theme.
         (widget-image-enable (tree-widget-use-image-p))     ; Emacs
         (widget-glyph-enable widget-image-enable)           ; XEmacs
         children buttons)
    (and indent (not (widget-get tree :parent))
         (insert-char ?\  indent))
    (if (widget-get tree :open)
;;;; Expanded node.
        (let ((args     (widget-get tree :args))
              (xpandr   (or (widget-get tree :expander)
                            (widget-get tree :dynargs)))
              (leaf     (widget-get tree :leaf-control))
              (guide    (widget-get tree :guide))
              (noguide  (widget-get tree :no-guide))
              (endguide (widget-get tree :end-guide))
              (handle   (widget-get tree :handle))
              (nohandle (widget-get tree :no-handle))
              (leafi    (tree-widget-find-image "leaf"))
              (guidi    (tree-widget-find-image "guide"))
              (noguidi  (tree-widget-find-image "no-guide"))
              (endguidi (tree-widget-find-image "end-guide"))
              (handli   (tree-widget-find-image "handle"))
              (nohandli (tree-widget-find-image "no-handle"))
              child)
          ;; Request children at run time, when not already done.
          (when (and (not args) xpandr)
            (setq args (mapcar 'widget-convert (funcall xpandr tree)))
            (widget-put tree :args args))
          ;; Insert the node "open" button.
          (push (widget-create-child-and-convert
                 tree (widget-get
                       tree (if args :open-control :empty-control))
                 :tag-glyph (tree-widget-find-image
                             (if args "open" "empty")))
                buttons)
          ;; Insert the :node element.
          (push (widget-create-child-and-convert tree node)
                children)
          ;; Insert children.
          (while args
            (setq child (car args)
                  args  (cdr args))
            (and indent (insert-char ?\  indent))
            ;; Insert guide lines elements from previous levels.
            (dolist (f (reverse flags))
              (widget-create-child-and-convert
               tree (if f guide noguide)
               :tag-glyph (if f guidi noguidi))
              (widget-create-child-and-convert
               tree nohandle :tag-glyph nohandli))
            ;; Insert guide line element for this level.
            (widget-create-child-and-convert
             tree (if args guide endguide)
             :tag-glyph (if args guidi endguidi))
            ;; Insert the node handle line
            (widget-create-child-and-convert
             tree handle :tag-glyph handli)
            ;; If leaf node, insert a leaf node button.
            (unless (tree-widget-p child)
              (push (widget-create-child-and-convert
                     tree leaf :tag-glyph leafi)
                    buttons))
            ;; Finally, insert the child widget.
            (push (widget-create-child-and-convert
                   tree child
                   :tree-widget--guide-flags (cons (if args t) flags))
                  children)))
;;;; Collapsed node.
      ;; Insert the "closed" node button.
      (push (widget-create-child-and-convert
             tree (widget-get tree :close-control)
             :tag-glyph (tree-widget-find-image "close"))
            buttons)
      ;; Insert the :node element.
      (push (widget-create-child-and-convert tree node)
            children))
    ;; Save widget children and buttons.  The :node child is the first
    ;; element in children.
    (widget-put tree :children (nreverse children))
    (widget-put tree :buttons  buttons)
    ))

(provide 'tree-widget)

;; arch-tag: c3a1ada2-1663-41dc-9d16-2479ed8320e8
;;; tree-widget.el ends here
