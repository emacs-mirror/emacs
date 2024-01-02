;;; vtable.el --- Displaying data in tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'text-property-search)
(require 'mule-util)

(defface vtable
  '((t :inherit variable-pitch))
  "Face used (by default) for vtables."
  :version "29.1"
  :group 'faces)

(cl-defstruct vtable-column
  "A vtable column."
  name
  width
  min-width
  max-width
  primary
  align
  getter
  formatter
  displayer
  -numerical)

(defclass vtable ()
  ((columns :initarg :columns :accessor vtable-columns)
   (objects :initarg :objects :accessor vtable-objects)
   (objects-function :initarg :objects-function
                     :accessor vtable-objects-function)
   (getter :initarg :getter :accessor vtable-getter)
   (formatter :initarg :formatter :accessor vtable-formatter)
   (displayer :initarg :displayer :accessor vtable-displayer)
   (use-header-line :initarg :use-header-line
                    :accessor vtable-use-header-line)
   (face :initarg :face :accessor vtable-face)
   (actions :initarg :actions :accessor vtable-actions)
   (keymap :initarg :keymap :accessor vtable-keymap)
   (separator-width :initarg :separator-width :accessor vtable-separator-width)
   (divider :initarg :divider :accessor vtable-divider :initform nil)
   (sort-by :initarg :sort-by :accessor vtable-sort-by)
   (ellipsis :initarg :ellipsis :accessor vtable-ellipsis)
   (column-colors :initarg :column-colors :accessor vtable-column-colors)
   (row-colors :initarg :row-colors :accessor vtable-row-colors)
   (-cached-colors :initform nil)
   (-cache :initform (make-hash-table :test #'equal))
   (-cached-keymap :initform nil)
   (-has-column-spec :initform nil))
  "An object to hold the data for a table.")

(defvar-keymap vtable-map
  "S" #'vtable-sort-by-current-column
  "{" #'vtable-narrow-current-column
  "}" #'vtable-widen-current-column
  "g" #'vtable-revert-command
  "M-<left>" #'vtable-previous-column
  "M-<right>" #'vtable-next-column)

(defvar-keymap vtable-header-line-map
  :parent vtable-map
  "<follow-link>" 'mouse-face
  "<mouse-2>" #'vtable-header-line-sort)

(cl-defun make-vtable (&key columns objects objects-function
                            getter
                            formatter
                            displayer
                            (use-header-line t)
                            (face 'vtable)
                            actions keymap
                            (separator-width 1)
                            divider
                            divider-width
                            sort-by
                            (ellipsis t)
                            (insert t)
                            row-colors
                            column-colors)
  "Create and insert a vtable at point.
The vtable object is returned.  If INSERT is nil, the table won't
be inserted.

See info node `(vtable)Top' for vtable documentation."
  (when objects-function
    (setq objects (funcall objects-function)))
  ;; We'll be altering the list, so create a copy.
  (setq objects (copy-sequence objects))
  (let ((table
         (make-instance
          'vtable
          :objects objects
          :objects-function objects-function
          :getter getter
          :formatter formatter
          :displayer displayer
          :use-header-line use-header-line
          :face face
          :actions actions
          :keymap keymap
          :separator-width separator-width
          :sort-by sort-by
          :row-colors row-colors
          :column-colors column-colors
          :ellipsis ellipsis)))
    ;; Store whether the user has specified columns or not.
    (setf (slot-value table '-has-column-spec) (not (not columns)))
    ;; Auto-generate the columns.
    (unless columns
      (unless objects
        (error "Can't auto-generate columns; no objects"))
      (setq columns (make-list (length (car objects)) "")))
    (setf (vtable-columns table)
          (mapcar (lambda (column)
                    (cond
                     ;; We just have the name (as a string).
                     ((stringp column)
                      (make-vtable-column :name column))
                     ;; A plist of keywords/values.
                     ((listp column)
                      (apply #'make-vtable-column column))
                     ;; A full `vtable-column' object.
                     (t
                      column)))
                  columns))
    ;; Compute missing column data.
    (setf (vtable-columns table) (vtable--compute-columns table))
    ;; Compute the colors.
    (when (or row-colors column-colors)
      (setf (slot-value table '-cached-colors)
            (vtable--compute-colors row-colors column-colors)))
    ;; Compute the divider.
    (when (or divider divider-width)
      (setf (vtable-divider table)
            (propertize
             (or (copy-sequence divider)
                 (propertize
                  " " 'display
                  (list 'space :width
                        (list (vtable--compute-width table divider-width)))))
             'mouse-face 'highlight
             'keymap
             (define-keymap
               "<drag-mouse-1>" #'vtable--drag-resize-column
               "<down-mouse-1>" #'ignore))))
    ;; Compute the keymap.
    (setf (slot-value table '-cached-keymap) (vtable--make-keymap table))
    (unless sort-by
      (seq-do-indexed (lambda (column index)
                        (when (vtable-column-primary column)
                          (push (cons index (vtable-column-primary column))
                                (vtable-sort-by table))))
                      (vtable-columns table)))
    (when insert
      (vtable-insert table))
    table))

(defun vtable--compute-colors (row-colors column-colors)
  (cond
   ((null column-colors)
    (mapcar #'vtable--make-color-face row-colors))
   ((null row-colors)
    (mapcar #'vtable--make-color-face column-colors))
   (t
    (cl-loop for row in row-colors
             collect (cl-loop for column in column-colors
                              collect (vtable--face-blend
                                       (vtable--make-color-face row)
                                       (vtable--make-color-face column)))))))

(defun vtable--make-color-face (object)
  (if (stringp object)
      (list :background object)
    object))

(defun vtable--face-blend (face1 face2)
  (let ((foreground (vtable--face-color face1 face2 #'face-foreground
                                        :foreground))
        (background (vtable--face-color face1 face2 #'face-background
                                        :background)))
    `(,@(and foreground (list :foreground foreground))
      ,@(and background (list :background background)))))

(defun vtable--face-color (face1 face2 accessor slot)
  (let ((col1 (if (facep face1)
                  (funcall accessor face1)
                (plist-get face1 slot)))
        (col2 (if (facep face2)
                  (funcall accessor face2)
                (plist-get face2 slot))))
    (if (and col1 col2)
        (vtable--color-blend col1 col2)
      (or col1 col2))))

;;; FIXME: This is probably not the right way to blend two colors, is
;;; it?
(defun vtable--color-blend (color1 color2)
  (cl-destructuring-bind (r g b)
      (mapcar (lambda (n) (* (/ n 2) 255.0))
              (cl-mapcar #'+ (color-name-to-rgb color1)
                         (color-name-to-rgb color2)))
    (format "#%02X%02X%02X" r g b)))

;;; Interface utility functions.

(defun vtable-current-table ()
  "Return the table under point."
  (get-text-property (point) 'vtable))

(defun vtable-current-object ()
  "Return the object under point."
  (get-text-property (point) 'vtable-object))

(defun vtable-current-column ()
  "Return the index of the column under point."
  (get-text-property (point) 'vtable-column))

(defun vtable-beginning-of-table ()
  "Go to the start of the current table."
  (if (text-property-search-backward 'vtable (vtable-current-table))
      (point)
    (goto-char (point-min))))

(defun vtable-end-of-table ()
  "Go to the end of the current table."
  (if (text-property-search-forward 'vtable (vtable-current-table))
      (point)
    (goto-char (point-max))))

(defun vtable-goto-object (object)
  "Go to OBJECT in the current table.
Return the position of the object if found, and nil if not."
  (let ((start (point)))
    (vtable-beginning-of-table)
    (save-restriction
      (narrow-to-region (point) (save-excursion (vtable-end-of-table)))
      (if (text-property-search-forward 'vtable-object object #'eq)
          (progn
            (forward-line -1)
            (point))
        (goto-char start)
        nil))))

(defun vtable-goto-table (table)
  "Go to TABLE in the current buffer.
If TABLE is found, return the position of the start of the table.
If it can't be found, return nil and don't move point."
  (let ((start (point)))
    (goto-char (point-min))
    (if-let ((match (text-property-search-forward 'vtable table t)))
        (goto-char (prop-match-beginning match))
      (goto-char start)
      nil)))

(defun vtable-goto-column (column)
  "Go to COLUMN on the current line."
  (beginning-of-line)
  (if-let ((match (text-property-search-forward 'vtable-column column t)))
      (goto-char (prop-match-beginning match))
    (end-of-line)))

(defun vtable-update-object (table object old-object)
  "Replace OLD-OBJECT in TABLE with OBJECT."
  (let* ((objects (vtable-objects table))
         (inhibit-read-only t))
    ;; First replace the object in the object storage.
    (if (eq old-object (car objects))
        ;; It's at the head, so replace it there.
        (setf (vtable-objects table)
              (cons object (cdr objects)))
      ;; Otherwise splice into the list.
      (while (and (cdr objects)
                  (not (eq (cadr objects) old-object)))
        (setq objects (cdr objects)))
      (unless objects
        (error "Can't find the old object"))
      (setcar (cdr objects) object))
    ;; Then update the cache...
    (let* ((line-number (seq-position old-object (car (vtable--cache table))))
           (line (elt (car (vtable--cache table)) line-number)))
      (unless line
        (error "Can't find cached object"))
      (setcar line object)
      (setcdr line (vtable--compute-cached-line table object))
      ;; ... and redisplay the line in question.
      (save-excursion
        (vtable-goto-object old-object)
        (let ((keymap (get-text-property (point) 'keymap))
              (start (point)))
          (delete-line)
          (vtable--insert-line table line line-number
                               (nth 1 (vtable--cache table))
                               (vtable--spacer table))
          (add-text-properties start (point) (list 'keymap keymap
                                                   'vtable table))))
      ;; We may have inserted a non-numerical value into a previously
      ;; all-numerical table, so recompute.
      (vtable--recompute-numerical table (cdr line)))))

(defun vtable-remove-object (table object)
  "Remove OBJECT from TABLE.
This will also remove the displayed line."
  ;; First remove from the objects.
  (setf (vtable-objects table) (delq object (vtable-objects table)))
  ;; Then adjust the cache and display.
  (let ((cache (vtable--cache table))
        (inhibit-read-only t))
    (setcar cache (delq (assq object (car cache)) (car cache)))
    (save-excursion
      (vtable-goto-table table)
      (when (vtable-goto-object object)
        (delete-line)))))

(defun vtable-insert-object (table object &optional after-object)
  "Insert OBJECT into TABLE after AFTER-OBJECT.
If AFTER-OBJECT is nil (or doesn't exist in the table), insert
OBJECT at the end.
This also updates the displayed table."
  ;; First insert into the objects.
  (let (pos)
    (if (and after-object
             (setq pos (memq after-object (vtable-objects table))))
        ;; Splice into list.
        (setcdr pos (cons object (cdr pos)))
      ;; Append.
      (nconc (vtable-objects table) (list object))))
  ;; Then adjust the cache and display.
  (save-excursion
    (vtable-goto-table table)
    (let* ((cache (vtable--cache table))
           (inhibit-read-only t)
           (keymap (get-text-property (point) 'keymap))
           (ellipsis (if (vtable-ellipsis table)
                         (propertize (truncate-string-ellipsis)
                                     'face (vtable-face table))
                       ""))
           (ellipsis-width (string-pixel-width ellipsis))
           (elem (and after-object
                      (assq after-object (car cache))))
           (line (cons object (vtable--compute-cached-line table object))))
      (if (not elem)
          ;; Append.
          (progn
            (setcar cache (nconc (car cache) (list line)))
            (vtable-end-of-table))
        ;; Splice into list.
        (let ((pos (memq elem (car cache))))
          (setcdr pos (cons line (cdr pos)))
          (unless (vtable-goto-object after-object)
            (vtable-end-of-table))))
      (let ((start (point)))
        ;; FIXME: We have to adjust colors in lines below this if we
        ;; have :row-colors.
        (vtable--insert-line table line 0
                             (nth 1 cache) (vtable--spacer table)
                             ellipsis ellipsis-width)
        (add-text-properties start (point) (list 'keymap keymap
                                                 'vtable table)))
      ;; We may have inserted a non-numerical value into a previously
      ;; all-numerical table, so recompute.
      (vtable--recompute-numerical table (cdr line)))))

(defun vtable-column (table index)
  "Return the name of the INDEXth column in TABLE."
  (vtable-column-name (elt (vtable-columns table) index)))

;;; Generating the table.

(defun vtable--get-value (object index column table)
  "Compute a cell value."
  (cond
   ((vtable-column-getter column)
    (funcall (vtable-column-getter column)
             object table))
   ((vtable-getter table)
    (funcall (vtable-getter table)
             object index table))
   ;; No getter functions; standard getters.
   ((stringp object)
    object)
   (t
    (elt object index))))

(defun vtable--compute-columns (table)
  (let ((numerical (make-vector (length (vtable-columns table)) t))
        (columns (vtable-columns table)))
    ;; First determine whether there are any all-numerical columns.
    (dolist (object (vtable-objects table))
      (seq-do-indexed
       (lambda (_elem index)
         (unless (numberp (vtable--get-value object index (elt columns index)
                                             table))
           (setf (elt numerical index) nil)))
       (vtable-columns table)))
    ;; Then fill in defaults.
    (seq-map-indexed
     (lambda (column index)
       ;; This is used when displaying.
       (unless (vtable-column-align column)
         (setf (vtable-column-align column)
               (if (elt numerical index)
                   'right
                 'left)))
       ;; This is used for sorting.
       (setf (vtable-column--numerical column)
             (elt numerical index))
       column)
     (vtable-columns table))))

(defun vtable--spacer (table)
  (vtable--compute-width table (vtable-separator-width table)))

(defun vtable--recompute-cache (table)
  (let* ((data (vtable--compute-cache table))
         (widths (vtable--compute-widths table data)))
    (setf (gethash (vtable--cache-key) (slot-value table '-cache))
          (list data widths))))

(defun vtable--ensure-cache (table)
  (or (vtable--cache table)
      (vtable--recompute-cache table)))

(defun vtable-insert (table)
  (let* ((spacer (vtable--spacer table))
         (start (point))
         (ellipsis (if (vtable-ellipsis table)
                       (propertize (truncate-string-ellipsis)
                                   'face (vtable-face table))
                     ""))
         (ellipsis-width (string-pixel-width ellipsis))
         ;; We maintain a cache per screen/window width, so that we render
         ;; correctly if Emacs is open on two different screens (or the
         ;; user resizes the frame).
         (widths (nth 1 (vtable--ensure-cache table))))
    ;; Don't insert any header or header line if the user hasn't
    ;; specified the columns.
    (when (slot-value table '-has-column-spec)
      (if (vtable-use-header-line table)
          (vtable--set-header-line table widths spacer)
        ;; Insert the header line directly into the buffer, and put a
        ;; keymap to be able to sort the columns there (by clicking on
        ;; them).
        (vtable--insert-header-line table widths spacer)
        (add-text-properties start (point)
                             (list 'keymap vtable-header-line-map
                                   'rear-nonsticky t
                                   'vtable table))
        (setq start (point))))
    (vtable--sort table)
    ;; Insert the data.
    (let ((line-number 0))
      (dolist (line (car (vtable--cache table)))
        (vtable--insert-line table line line-number widths spacer
                             ellipsis ellipsis-width)
        (setq line-number (1+ line-number))))
    (add-text-properties start (point)
                         (list 'rear-nonsticky t
                               'vtable table))
    (goto-char start)))

(defun vtable--insert-line (table line line-number widths spacer
                                  &optional ellipsis ellipsis-width)
  (let ((start (point))
        (columns (vtable-columns table))
        (column-colors
         (and (vtable-column-colors table)
              (if (vtable-row-colors table)
                  (elt (slot-value table '-cached-colors)
                       (mod line-number (length (vtable-row-colors table))))
                (slot-value table '-cached-colors))))
        (divider (vtable-divider table))
        (keymap (slot-value table '-cached-keymap)))
    (seq-do-indexed
     (lambda (elem index)
       (let ((value (nth 0 elem))
             (column (elt columns index))
             (pre-computed (nth 2 elem)))
         ;; See if we have any formatters here.
         (cond
          ((vtable-column-formatter column)
           (setq value (funcall (vtable-column-formatter column) value)
                 pre-computed nil))
          ((vtable-formatter table)
           (setq value (funcall (vtable-formatter table)
                                value index table)
                 pre-computed nil)))
         (let ((displayed
                ;; Allow any displayers to have their say.
                (cond
                 ((vtable-column-displayer column)
                  (funcall (vtable-column-displayer column)
                           value (elt widths index) table))
                 ((vtable-displayer table)
                  (funcall (vtable-displayer table)
                           value index (elt widths index) table))
                 (pre-computed
                  ;; If we don't have a displayer, use the pre-made
                  ;; (cached) string value.
                  (if (> (nth 1 elem) (elt widths index))
                      (concat
                       (vtable--limit-string
                        pre-computed (- (elt widths index)
                                        (or ellipsis-width 0)))
                       ellipsis)
                    pre-computed))
                 ;; Recompute widths.
                 (t
                  (if (> (string-pixel-width value) (elt widths index))
                      (concat
                       (vtable--limit-string
                        value (- (elt widths index)
                                 (or ellipsis-width 0)))
                       ellipsis)
                    value))))
               (start (point))
               ;; Don't insert the separator after the final column.
               (last (= index (- (length line) 2))))
           (if (eq (vtable-column-align column) 'left)
               (progn
                 (insert displayed)
                 (insert (propertize
                          " " 'display
                          (list 'space
                                :width (list
                                        (+ (- (elt widths index)
                                              (string-pixel-width displayed))
                                           (if last 0 spacer)))))))
             ;; Align to the right.
             (insert (propertize " " 'display
                                 (list 'space
                                       :width (list (- (elt widths index)
                                                       (string-pixel-width
                                                        displayed)))))
                     displayed)
             (unless last
               (insert (propertize " " 'display
                                   (list 'space
                                         :width (list spacer))))))
           (put-text-property start (point) 'vtable-column index)
           (put-text-property start (point) 'keymap keymap)
           (when column-colors
             (add-face-text-property
              start (point)
              (elt column-colors (mod index (length column-colors)))))
           (when divider
             (insert divider)
             (setq start (point))))))
     (cdr line))
    (insert "\n")
    (put-text-property start (point) 'vtable-object (car line))
    (unless column-colors
      (when-let ((row-colors (slot-value table '-cached-colors)))
        (add-face-text-property
         start (point)
         (elt row-colors (mod line-number (length row-colors))))))))

(defun vtable--cache-key ()
  (cons (frame-terminal) (window-width)))

(defun vtable--cache (table)
  (gethash (vtable--cache-key) (slot-value table '-cache)))

(defun vtable--clear-cache (table)
  (setf (gethash (vtable--cache-key) (slot-value table '-cache)) nil))

(defun vtable--sort (table)
  (pcase-dolist (`(,index . ,direction) (vtable-sort-by table))
    (let ((cache (vtable--cache table))
          (numerical (vtable-column--numerical
                      (elt (vtable-columns table) index)))
          (numcomp (if (eq direction 'descend)
                       #'> #'<))
          (stringcomp (if (eq direction 'descend)
                          #'string> #'string<)))
      (setcar cache
              (sort (car cache)
                    (lambda (e1 e2)
                      (let ((c1 (elt e1 (1+ index)))
                            (c2 (elt e2 (1+ index))))
                        (if numerical
                            (funcall numcomp (car c1) (car c2))
                          (funcall
                           stringcomp
                           (if (stringp (car c1))
                               (car c1)
                             (format "%s" (car c1)))
                           (if (stringp (car c2))
                               (car c2)
                             (format "%s" (car c2))))))))))))

(defun vtable--indicator (table index)
  (let ((order (car (last (vtable-sort-by table)))))
    (if (eq index (car order))
        ;; We're sorting by this column last, so return an indicator.
        (catch 'found
          (dolist (candidate (nth (if (eq (cdr order) 'ascend)
                                      1
                                    0)
                                  '((?▼ ?v)
                                    (?▲ ?^))))
            (when (char-displayable-p candidate)
              (throw 'found (string candidate)))))
      "")))

(defun vtable--insert-header-line (table widths spacer)
  ;; Insert the header directly into the buffer.
  (let ((start (point))
        (divider (vtable-divider table))
        (cmap (define-keymap
                "<header-line> <drag-mouse-1>" #'vtable--drag-resize-column
                "<header-line> <down-mouse-1>" #'ignore))
        (dmap (define-keymap
                "<header-line> <drag-mouse-1>"
                (lambda (e)
                  (interactive "e")
                  (vtable--drag-resize-column e t))
                "<header-line> <down-mouse-1>" #'ignore)))
    (seq-do-indexed
     (lambda (column index)
       (let* ((name (propertize
                     (vtable-column-name column)
                     'face (list 'header-line (vtable-face table))
                     'mouse-face 'header-line-highlight
                     'keymap cmap))
              (start (point))
              (indicator (vtable--indicator table index))
              (indicator-width (string-pixel-width indicator))
              (last (= index (1- (length (vtable-columns table)))))
              displayed)
         (setq displayed
               (if (> (string-pixel-width name)
                      (- (elt widths index) indicator-width))
                   (vtable--limit-string
                    name (- (elt widths index) indicator-width))
                 name))
         (let ((fill-width
                (+ (- (elt widths index)
                      (string-pixel-width displayed)
                      indicator-width
                      (vtable-separator-width table)
                      ;; We want the indicator to not be quite flush
                      ;; right.
                      (/ (vtable--char-width table) 2.0))
                   (if last 0 spacer))))
           (if (or (not last)
                   (zerop indicator-width)
                   (< (seq-reduce #'+ widths 0) (window-width nil t)))
               ;; Normal case.
               (insert
                displayed
                (propertize " " 'display
                            (list 'space :width (list fill-width)))
                indicator)
             ;; This is the final column, and we have a sorting
             ;; indicator, and the table is too wide for the window.
             (let* ((pre-indicator (string-pixel-width
                                    (buffer-substring (point-min) (point))))
                    (pre-fill
                     (- (window-width nil t)
                        pre-indicator
                        (string-pixel-width displayed))))
               (insert
                displayed
                (propertize " " 'display
                            (list 'space :width (list pre-fill)))
                indicator
                (propertize " " 'display
                            (list 'space :width
                                  (list (- fill-width pre-fill))))))))
         (when (and divider (not last))
           (insert (propertize divider 'keymap dmap)))
         (insert (propertize
                  " " 'display
                  (list 'space :width (list
                                       (/ (vtable--char-width table) 2.0)))))
         (put-text-property start (point) 'vtable-column index)))
     (vtable-columns table))
    (insert "\n")
    (add-face-text-property start (point) 'header-line)))

(defun vtable--drag-resize-column (e &optional next)
  "Resize the column by dragging.
If NEXT, do the next column."
  (interactive "e")
  (let* ((pos-start (event-start e))
	 (obj (posn-object pos-start)))
    (with-current-buffer (window-buffer (posn-window pos-start))
      (let ((column
             ;; In the header line we have a text property on the
             ;; divider.
             (or (get-text-property (if obj (cdr obj)
                                      (posn-point pos-start))
			            'vtable-column
			            (car obj))
                 ;; For reasons of efficiency, we don't have that in
                 ;; the buffer itself, so find the column.
                 (save-excursion
                   (goto-char (posn-point pos-start))
                   (1+
                    (get-text-property
                     (prop-match-beginning
                      (text-property-search-backward 'vtable-column))
                     'vtable-column)))))
            (start-x (car (posn-x-y pos-start)))
            (end-x (car (posn-x-y (event-end e)))))
        (when (or (> column 0) next)
          (vtable--alter-column-width (vtable-current-table)
                                      (if next
                                          column
                                        (1- column))
                                      (- end-x start-x)))))))

(defun vtable--recompute-numerical (table line)
  "Recompute numericalness of columns if necessary."
  (let ((columns (vtable-columns table))
        (recompute nil))
    (seq-do-indexed
     (lambda (elem index)
       (when (and (vtable-column--numerical (elt columns index))
                  (not (numberp elem)))
         (setq recompute t)))
     line)
    (when recompute
      (vtable--compute-columns table))))

(defun vtable--set-header-line (table widths spacer)
  (setq header-line-format
        (string-replace
         "%" "%%"
         (with-temp-buffer
           (insert " ")
           (vtable--insert-header-line table widths spacer)
           ;; Align the header with the (possibly) fringed buffer text.
           (put-text-property
            (point-min) (1+ (point-min))
            'display '(space :align-to 0))
           (buffer-substring (point-min) (1- (point-max))))))
  (vtable-header-mode 1))

(defun vtable--limit-string (string pixels)
  (while (and (length> string 0)
              (> (string-pixel-width string) pixels))
    (setq string (substring string 0 (1- (length string)))))
  string)

(defun vtable--char-width (table)
  (string-pixel-width (propertize "x" 'face (vtable-face table))))

(defun vtable--compute-width (table spec)
  (cond
   ((numberp spec)
    (* spec (vtable--char-width table)))
   ((string-match "\\([0-9.]+\\)ex" spec)
    (* (string-to-number (match-string 1 spec)) (vtable--char-width table)))
   ((string-match "\\([0-9.]+\\)px" spec)
    (string-to-number (match-string 1 spec)))
   ((string-match "\\([0-9.]+\\)%" spec)
    (/ (* (string-to-number (match-string 1 spec)) (window-width nil t))
       100))
   (t
    (error "Invalid spec: %s" spec))))

(defun vtable--compute-widths (table cache)
  "Compute the display widths for TABLE."
  (seq-into
   (seq-map-indexed
    (lambda (column index)
      (let ((width
             (or
              ;; Explicit widths.
              (and (vtable-column-width column)
                   (vtable--compute-width table (vtable-column-width column)))
              ;; Compute based on the displayed widths of
              ;; the data.
              (seq-max (seq-map (lambda (elem)
                                  (nth 1 (elt (cdr elem) index)))
                                cache)))))
        ;; Let min-width/max-width specs have their say.
        (when-let ((min-width (and (vtable-column-min-width column)
                                   (vtable--compute-width
                                    table (vtable-column-min-width column)))))
          (setq width (max width min-width)))
        (when-let ((max-width (and (vtable-column-max-width column)
                                   (vtable--compute-width
                                    table (vtable-column-max-width column)))))
          (setq width (min width max-width)))
        width))
    (vtable-columns table))
   'vector))

(defun vtable--compute-cache (table)
  (seq-map
   (lambda (object)
     (cons object (vtable--compute-cached-line table object)))
   (vtable-objects table)))

(defun vtable--compute-cached-line (table object)
  (seq-map-indexed
   (lambda (column index)
     (let* ((value (vtable--get-value object index column table))
            (string (if (stringp value)
                        (copy-sequence value)
                      (format "%s" value))))
       (add-face-text-property 0 (length string)
                               (vtable-face table)
                               t string)
       ;; We stash the computed width and string here -- if there are
       ;; no formatters/displayers, we'll be using the string, and
       ;; then won't have to recreate it.
       (list value (string-pixel-width string) string)))
   (vtable-columns table)))

(defun vtable--make-keymap (table)
  (let ((map (if (or (vtable-actions table)
                     (vtable-keymap table))
                 (copy-keymap vtable-map)
               vtable-map)))
    (when-let ((actions (vtable-actions table)))
      (while actions
        (funcall (lambda (key binding)
                   (keymap-set map key
                               (lambda (object)
                                 (interactive (list (vtable-current-object)))
                                 (funcall binding object))))
                 (car actions) (cadr actions))
        (setq actions (cddr actions))))
    (if (vtable-keymap table)
        (progn
          (setf (vtable-keymap table)
                (copy-keymap (vtable-keymap table)))
          ;; Respect any previously set parent keymaps.
          (set-keymap-parent (vtable-keymap table)
                             (if (keymap-parent (vtable-keymap table))
                                 (append (ensure-list
                                          (vtable-keymap table))
                                         (list map))
                               map))
          (vtable-keymap table))
      map)))

(defun vtable-revert ()
  "Regenerate the table under point."
  (let ((table (vtable-current-table))
        (object (vtable-current-object))
        (column (vtable-current-column))
        (inhibit-read-only t))
    (unless table
      (user-error "No table under point"))
    (delete-region (vtable-beginning-of-table) (vtable-end-of-table))
    (vtable-insert table)
    (when object
      (vtable-goto-object object))
    (when column
      (vtable-goto-column column))))

(defun vtable--widths (table)
  (nth 1 (vtable--ensure-cache table)))

;;; Commands.

(defvar-keymap vtable-header-mode-map
  "<header-line> <mouse-1>" 'vtable-header-line-sort
  "<header-line> <mouse-2>" 'vtable-header-line-sort)

(define-minor-mode vtable-header-mode
  "Minor mode for buffers with vtables with headers."
  :keymap vtable-header-mode-map)

(defun vtable-narrow-current-column (&optional n)
  "Narrow the current column by N characters.
If N isn't given, N defaults to 1.

Interactively, N is the prefix argument."
  (interactive "p")
  (let* ((table (vtable-current-table))
         (column (vtable-current-column)))
    (unless column
      (user-error "No column under point"))
    (vtable--alter-column-width table column
                                (- (* (vtable--char-width table) (or n 1))))))

(defun vtable--alter-column-width (table column delta)
  (let ((widths (vtable--widths table)))
    (setf (aref widths column)
          (max (* (vtable--char-width table) 2)
               (+ (aref widths column) delta)))
    ;; Store the width so it'll be respected on a revert.
    (setf (vtable-column-width (elt (vtable-columns table) column))
          (format "%dpx" (aref widths column)))
    (vtable-revert)))

(defun vtable-widen-current-column (&optional n)
  "Widen the current column by N characters.
If N isn't given, N defaults to 1.

Interactively, N is the prefix argument."
  (interactive "p")
  (vtable-narrow-current-column (- n)))

(defun vtable-previous-column ()
  "Go to the previous column."
  (interactive)
  (vtable-goto-column
   (max 0 (1- (or (vtable-current-column)
                  (length (vtable--widths (vtable-current-table))))))))

(defun vtable-next-column ()
  "Go to the next column."
  (interactive)
  (when (vtable-current-column)
    (vtable-goto-column
     (min (1- (length (vtable--widths (vtable-current-table))))
          (1+ (vtable-current-column))))))

(defun vtable-revert-command ()
  "Re-query data and regenerate the table under point."
  (interactive)
  (let ((table (vtable-current-table)))
    (when (vtable-objects-function table)
      (setf (vtable-objects table) (funcall (vtable-objects-function table))))
    (vtable--clear-cache table))
  (vtable-revert))

(defun vtable-sort-by-current-column ()
  "Sort the table under point by the column under point."
  (interactive)
  (unless (vtable-current-column)
    (user-error "No current column"))
  (let* ((table (vtable-current-table))
         (last (car (last (vtable-sort-by table))))
         (index (vtable-current-column)))
    ;; First prune any previous appearance of this column.
    (setf (vtable-sort-by table)
          (delq (assq index (vtable-sort-by table))
                (vtable-sort-by table)))
    ;; Then insert this as the last sort key.
    (setf (vtable-sort-by table)
          (append (vtable-sort-by table)
                  (list (cons index
                              (if (eq (car last) index)
                                  (if (eq (cdr last) 'ascend)
                                      'descend
                                    'ascend)
                                'ascend))))))
  (vtable-revert))

(defun vtable-header-line-sort (e)
  "Sort a vtable from the header line."
  (interactive "e")
  (let* ((pos (event-start e))
	 (obj (posn-object pos)))
    (with-current-buffer (window-buffer (posn-window pos))
      (goto-char (point-min))
      (vtable-goto-column
       (get-text-property (if obj (cdr obj) (posn-point pos))
			  'vtable-column
			  (car obj)))
      (vtable-sort-by-current-column))))

(provide 'vtable)

;;; vtable.el ends here
