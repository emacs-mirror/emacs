;;; ruler-mode.el --- display a ruler in the header line

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 24 Mar 2001
;; Version: 1.4
;; Keywords: environment convenience

;; This file is part of GNU Emacs.

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This library provides a minor mode to display a ruler in the header
;; line.  It works only on Emacs 21.
;;
;; You can use the mouse to change the `fill-column', `window-margins'
;; and `tab-stop-list' settings:
;;
;; [header-line (shift down-mouse-1)] set left margin to the ruler
;; graduation where the mouse pointer is on.
;;
;; [header-line (shift down-mouse-3)] set right margin to the ruler
;; graduation where the mouse pointer is on.
;;
;; [header-line down-mouse-2] set `fill-column' to the ruler
;; graduation where the mouse pointer is on.
;;
;; [header-line (control down-mouse-1)] add a tab stop to the ruler
;; graduation where the mouse pointer is on.
;;
;; [header-line (control down-mouse-3)] remove the tab stop at the
;; ruler graduation where the mouse pointer is on.
;;
;; [header-line (control down-mouse-2)] or M-x
;; `ruler-mode-toggle-show-tab-stops' toggle showing and visually
;; editing `tab-stop-list' setting.  The `ruler-mode-show-tab-stops'
;; option controls if the ruler shows tab stops by default.
;;
;; In the ruler the character `ruler-mode-current-column-char' shows
;; the `current-column' location, `ruler-mode-fill-column-char' shows
;; the `fill-column' location and `ruler-mode-tab-stop-char' shows tab
;; stop locations.  `window-margins' areas are shown with a different
;; background color.
;;
;; It is also possible to customize the following characters:
;;
;; - `ruler-mode-margins-char' character used to pad margin areas
;;   (space by default).
;; - `ruler-mode-basic-graduation-char' character used for basic
;;   graduations ('.' by default).
;; - `ruler-mode-inter-graduation-char' character used for
;;   intermediate graduations ('!' by default).
;;
;; The following faces are customizable:
;;
;; - `ruler-mode-default-face' the ruler default face.
;; - `ruler-mode-fill-column-face' the face used to highlight the
;;   `fill-column' character.
;; - `ruler-mode-current-column-face' the face used to highlight the
;;   `current-column' character.
;; - `ruler-mode-tab-stop-face' the face used to highlight tab stop
;;   characters.
;; - `ruler-mode-margins-face' the face used to highlight the
;;   `window-margins' areas.
;; - `ruler-mode-column-number-face' the face used to highlight the
;;   number graduations.
;;
;; `ruler-mode-default-face' inherits from the built-in `default' face.
;; All `ruler-mode' faces inerit from `ruler-mode-default-face'.
;;
;; WARNING: To keep ruler graduations aligned on text columns it is
;; important to use the same font family and size for ruler and text
;; areas.

;; Installation
;;
;; To automatically display the ruler in specific major modes use:
;;
;;    (add-hook '<major-mode>-hook 'ruler-mode)
;;

;;; History:
;;

;;; Code:
(eval-when-compile
  (require 'wid-edit))

(defgroup ruler-mode nil
  "Display a ruler in the header line."
  :version "21.4"
  :group 'environment)

(defcustom ruler-mode-show-tab-stops nil
  "*If non-nil the ruler shows tab stop positions.
Also allowing to visually change `tab-stop-list' setting using
<C-down-mouse-1> and <C-down-mouse-3> on the ruler to respectively add
or remove a tab stop.  \\[ruler-mode-toggle-show-tab-stops] or
<C-down-mouse-2> on the ruler toggles showing/editing of tab stops."
  :group 'ruler-mode
  :type 'boolean)

;; IMPORTANT: This function must be defined before the following
;; defcustoms because it is used in their :validate clause.
(defun ruler-mode-character-validate (widget)
  "Ensure WIDGET value is a valid character value."
  (save-excursion
    (let ((value (widget-value widget)))
      (if (char-valid-p value)
          nil
        (widget-put widget :error
                    (format "Invalid character value: %S" value))
        widget))))
      
(defcustom ruler-mode-fill-column-char (if window-system
                                           ?\¶
                                         ?\|)
  "*Character used at the `fill-column' location."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-current-column-char (if window-system
                                              ?\¦
                                            ?\@)
  "*Character used at the `current-column' location."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-tab-stop-char ?\T
  "*Character used at `tab-stop-list' locations."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-margins-char ?\ 
  "*Character used in margin areas."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-basic-graduation-char ?\.
  "*Character used for basic graduations."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defcustom ruler-mode-inter-graduation-char ?\!
  "*Character used for intermediate graduations."
  :group 'ruler-mode
  :type '(choice
          (character :tag "Character")
          (integer :tag "Integer char value"
                   :validate ruler-mode-character-validate)))

(defface ruler-mode-default-face
  '((((type tty))
     (:inherit default
               :background "grey64"
               :foreground "grey50"
               ))
    (t
     (:inherit default
               :background "grey76"
               :foreground "grey64"
               :box (:color "grey76"
                            :line-width 1
                            :style released-button)
               )))
  "Default face used by the ruler."
  :group 'ruler-mode)

(defface ruler-mode-column-number-face
  '((t
     (:inherit ruler-mode-default-face
               :foreground "black"
               )))
  "Face used to highlight number graduations."
  :group 'ruler-mode)

(defface ruler-mode-fill-column-face
  '((t
     (:inherit ruler-mode-default-face
               :foreground "red"
               )))
  "Face used to highlight the fill column character."
  :group 'ruler-mode)

(defface ruler-mode-tab-stop-face
  '((t
     (:inherit ruler-mode-default-face
               :foreground "steelblue"
               )))
  "Face used to highlight tab stop characters."
  :group 'ruler-mode)

(defface ruler-mode-margins-face
  '((((type tty))
     (:inherit ruler-mode-default-face
               :background "grey50"
               ))
    (t
     (:inherit ruler-mode-default-face
               :background "grey64"
               )))
  "Face used to highlight the `window-margins' areas."
  :group 'ruler-mode)

(defface ruler-mode-current-column-face
  '((t
     (:inherit ruler-mode-default-face
               :weight bold
               :foreground "yellow"
               )))
  "Face used to highlight the `current-column' character."
  :group 'ruler-mode)

(defun ruler-mode-mouse-set-left-margin (start-event)
  "Set left margin to the graduation where the mouse pointer is on.
START-EVENT is the mouse click event."
  (interactive "e")
  (let* ((start (event-start start-event))
         (end   (event-end   start-event))
         w col m lm0 lm rm)
    (if (eq start end) ;; mouse click
        (save-selected-window
          (select-window (posn-window start))
          (setq m   (window-margins)
                lm0 (or (car m) 0)
                rm  (or (cdr m) 0)
                w   (window-width)
                col (car (posn-col-row start))
                lm  (min (- w rm) col))
          (message "Left margin set to %d (was %d)" lm lm0)
          (set-window-margins nil lm rm)))))

(defun ruler-mode-mouse-set-right-margin (start-event)
  "Set right margin to the graduation where the mouse pointer is on.
START-EVENT is the mouse click event."
  (interactive "e")
  (let* ((start (event-start start-event))
         (end   (event-end   start-event))
         m col w lm rm0 rm)
    (if (eq start end) ;; mouse click
        (save-selected-window
          (select-window (posn-window start))
          (setq m   (window-margins)
                rm0 (or (cdr m) 0)
                lm  (or (car m) 0)
                col (car (posn-col-row start))
                w   (window-width)
                rm  (max 0 (- w col)))
          (message "Right margin set to %d (was %d)" rm rm0)
          (set-window-margins nil lm rm)))))

(defun ruler-mode-mouse-set-fill-column (start-event)
  "Set `fill-column' to the graduation where the mouse pointer is on.
START-EVENT is the mouse click event."
  (interactive "e")
  (let* ((start (event-start start-event))
         (end   (event-end   start-event))
         m col w lm rm hs fc)
    (if (eq start end) ;; mouse click
        (save-selected-window
          (select-window (posn-window start))
          (setq m   (window-margins)
                lm  (or (car m) 0)
                rm  (or (cdr m) 0)
                col (- (car (posn-col-row start)) lm)
                w   (window-width)
                hs  (window-hscroll)
                fc  (+ col hs))
          (and (>= col 0) (< (+ col lm rm) w)
               (progn
                 (message "Fill column set to %d (was %d)" fc fill-column)
                 (setq fill-column fc)))))))

(defun ruler-mode-mouse-add-tab-stop (start-event)
  "Add a tab stop to the graduation where the mouse pointer is on.
START-EVENT is the mouse click event."
  (interactive "e")
  (if ruler-mode-show-tab-stops
      (let* ((start (event-start start-event))
             (end   (event-end   start-event))
             m col w lm rm hs ts)
        (if (eq start end) ;; mouse click
            (save-selected-window
              (select-window (posn-window start))
              (setq m   (window-margins)
                    lm  (or (car m) 0)
                    rm  (or (cdr m) 0)
                    col (- (car (posn-col-row start)) lm)
                    w   (window-width)
                    hs  (window-hscroll)
                    ts  (+ col hs))
              (and (>= col 0) (< (+ col lm rm) w)
                   (not (member ts tab-stop-list))
                   (progn
                     (message "Tab stop set to %d" ts)
                     (setq tab-stop-list
                           (sort (cons ts tab-stop-list)
                                 #'<)))))))))

(defun ruler-mode-mouse-del-tab-stop (start-event)
  "Delete tab stop at the graduation where the mouse pointer is on.
START-EVENT is the mouse click event."
  (interactive "e")
  (if ruler-mode-show-tab-stops
      (let* ((start (event-start start-event))
             (end   (event-end   start-event))
             m col w lm rm hs ts)
        (if (eq start end) ;; mouse click
            (save-selected-window
              (select-window (posn-window start))
              (setq m   (window-margins)
                    lm  (or (car m) 0)
                    rm  (or (cdr m) 0)
                    col (- (car (posn-col-row start)) lm)
                    w   (window-width)
                    hs  (window-hscroll)
                    ts  (+ col hs))     
              (and (>= col 0) (< (+ col lm rm) w)
                   (member ts tab-stop-list)
                   (progn
                     (message "Tab stop at %d deleted" ts)
                     (setq tab-stop-list
                           (delete ts tab-stop-list)))))))))

(defun ruler-mode-toggle-show-tab-stops ()
  "Toggle showing of tab stops on the ruler."
  (interactive)
  (setq ruler-mode-show-tab-stops (not ruler-mode-show-tab-stops))
  (force-mode-line-update))

(defvar ruler-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [header-line down-mouse-1]
      #'ignore)
    (define-key km [header-line down-mouse-3]
      #'ignore)
    (define-key km [header-line down-mouse-2]
      #'ruler-mode-mouse-set-fill-column)
    (define-key km [header-line (shift down-mouse-1)]
      #'ruler-mode-mouse-set-left-margin)
    (define-key km [header-line (shift down-mouse-3)]
      #'ruler-mode-mouse-set-right-margin)
    (define-key km [header-line (control down-mouse-1)]
      #'ruler-mode-mouse-add-tab-stop)
    (define-key km [header-line (control down-mouse-3)]
      #'ruler-mode-mouse-del-tab-stop)
    (define-key km [header-line (control down-mouse-2)]
      #'ruler-mode-toggle-show-tab-stops)
    km)
  "Keymap for ruler minor mode.")

(defvar ruler-mode-header-line-format-old nil
  "Hold previous value of `header-line-format'.")
(make-variable-buffer-local 'ruler-mode-header-line-format-old)

(defconst ruler-mode-header-line-format
  '(:eval (ruler-mode-ruler))
  "`header-line-format' used in ruler mode.")

;;;###autoload
(define-minor-mode ruler-mode
  "Display a ruler in the header line if ARG > 0."
  nil nil
  ruler-mode-map
  :group 'ruler-mode
  (if ruler-mode
      (progn
        ;; When `ruler-mode' is on save previous header line format
        ;; and install the ruler header line format.
        (setq ruler-mode-header-line-format-old header-line-format
              header-line-format ruler-mode-header-line-format)
        (add-hook 'post-command-hook    ; add local hook
                  #'force-mode-line-update nil t))
    ;; When `ruler-mode' is off restore previous header line format if
    ;; the current one is the ruler header line format.
    (if (eq header-line-format ruler-mode-header-line-format)
        (setq header-line-format ruler-mode-header-line-format-old))
    (remove-hook 'post-command-hook     ; remove local hook
                 #'force-mode-line-update t)))

;; Add ruler-mode to the minor mode menu in the mode line
(define-key mode-line-mode-menu [ruler-mode]
  `(menu-item "Ruler" ruler-mode
	      :button (:toggle . ruler-mode)))

(defconst ruler-mode-ruler-help-echo
  "\
S-mouse-1/3: set L/R margin, \
mouse-2: set fill col, \
C-mouse-2: show tabs"
  "Help string shown when mouse pointer is over the ruler.
`ruler-mode-show-tab-stops' is nil.")

(defconst ruler-mode-ruler-help-echo-tab
  "\
C-mouse1/3: set/unset tab, \
C-mouse-2: hide tabs"
  "Help string shown when mouse pointer is over the ruler.
`ruler-mode-show-tab-stops' is non-nil.")

(defconst ruler-mode-left-margin-help-echo
  "Left margin %S"
  "Help string shown when mouse is over the left margin area.")

(defconst ruler-mode-right-margin-help-echo
  "Right margin %S"
  "Help string shown when mouse is over the right margin area.")

(defmacro ruler-mode-left-fringe-cols ()
  "Return the width, measured in columns, of the left fringe area."
  '(round (or (frame-parameter nil 'left-fringe) 0)
          (frame-char-width)))

(defmacro ruler-mode-right-fringe-cols ()
  "Return the width, measured in columns, of the right fringe area."
  '(round (or (frame-parameter nil 'right-fringe) 0)
          (frame-char-width)))

(defmacro ruler-mode-left-scroll-bar-cols ()
  "Return the width, measured in columns, of the left vertical scrollbar."
  '(if (eq (frame-parameter nil 'vertical-scroll-bars) 'left)
       (round (or (frame-parameter nil 'scroll-bar-width) 0)
              (frame-char-width))
     0))

(defmacro ruler-mode-right-scroll-bar-cols ()
  "Return the width, measured in columns, of the right vertical scrollbar."
  '(if (eq (frame-parameter nil 'vertical-scroll-bars) 'right)
       (round (or (frame-parameter nil 'scroll-bar-width) 0)
              (frame-char-width))
     0))

(defun ruler-mode-ruler ()
  "Return a string ruler."
  (if ruler-mode
      (let* ((j     (+ (ruler-mode-left-fringe-cols)
                       (ruler-mode-left-scroll-bar-cols)))
             (w     (+ (window-width) j))
             (m     (window-margins))
             (l     (or (car m) 0))
             (r     (or (cdr m) 0))
             (o     (- (window-hscroll) l j))
             (i     0)
             (ruler (concat
                     ;; unit graduations
                     (make-string w ruler-mode-basic-graduation-char)
                     ;; extra space to fill the header line
                     (make-string (+ (ruler-mode-right-fringe-cols)
                                     (ruler-mode-right-scroll-bar-cols))
                                  ?\ )))
             c k)

        ;; Setup default face and help echo.
        (put-text-property 0 (length ruler)
                           'face 'ruler-mode-default-face
                           ruler)
        (put-text-property 0 (length ruler)
                           'help-echo 
                           (if ruler-mode-show-tab-stops
                               ruler-mode-ruler-help-echo-tab
                             ruler-mode-ruler-help-echo)
                           ruler)
        ;; Setup the local map.
        (put-text-property 0 (length ruler)
                           'local-map ruler-mode-map
                           ruler)

        (setq j (+ l j))
        ;; Setup the left margin area.
        (put-text-property
         i j 'face 'ruler-mode-margins-face
         ruler)
        (put-text-property
         i j 'help-echo (format ruler-mode-left-margin-help-echo l)
         ruler)
        (while (< i j)
          (aset ruler i ruler-mode-margins-char)
          (setq i (1+ i)))

        ;; Setup the ruler area.
        (setq r (- w r))
        (while (< i r)
          (setq j (+ i o))
          (cond
           ((= (mod j 10) 0)
            (setq c (number-to-string (/ j 10))
                  m (length c)
                  k i)
            (put-text-property
             i (1+ i) 'face 'ruler-mode-column-number-face
             ruler)
            (while (and (> m 0) (>= k 0))
              (aset ruler k (aref c (setq m (1- m))))
              (setq k (1- k)))
            )
           ((= (mod j 5) 0)
            (aset ruler i ruler-mode-inter-graduation-char)
            )
           )
          (setq i (1+ i)))

        ;; Setup the right margin area.
        (put-text-property
         i (length ruler) 'face 'ruler-mode-margins-face
         ruler)
        (put-text-property
         i (length ruler) 'help-echo
         (format ruler-mode-right-margin-help-echo (- w r))
         ruler)
        (while (< i (length ruler))
          (aset ruler i ruler-mode-margins-char)
          (setq i (1+ i)))
         
        ;; Show the `fill-column' marker.
        (setq i (- fill-column o))
        (and (>= i 0) (< i r)
             (aset ruler i ruler-mode-fill-column-char)
             (put-text-property
              i (1+ i) 'face 'ruler-mode-fill-column-face
              ruler))

        ;; Show the `tab-stop-list' markers.
        (if ruler-mode-show-tab-stops
            (let ((tsl tab-stop-list) ts)
              (while tsl
                (setq ts  (car tsl)
                      tsl (cdr tsl)
                      i   (- ts o))
                (and (>= i 0) (< i r)
                     (aset ruler i ruler-mode-tab-stop-char)
                     (put-text-property
                      i (1+ i)
                      'face (cond
                             ;; Don't override the fill-column face
                             ((eq ts fill-column)
                              'ruler-mode-fill-column-face)
                             (t
                              'ruler-mode-tab-stop-face))
                      ruler)))))

        ;; Show the `current-column' marker.
        (setq i (- (current-column) o))
        (and (>= i 0) (< i r)
             (aset ruler i ruler-mode-current-column-char)
             (put-text-property
              i (1+ i) 'face 'ruler-mode-current-column-face
              ruler))
         
        ruler)))

(provide 'ruler-mode)

;; Local Variables:
;; coding: iso-latin-1
;; End:

;;; ruler-mode.el ends here
