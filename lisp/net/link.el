;;; link.el --- Hypertext links in text buffers

;; Author: Torsten Hilbrich <torsten.hilbrich@gmx.net>
;; Keywords: interface, hypermedia
;; Version: 1.11

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains functions for using links in buffers.  A link is
;; a part of the buffer marked with a special face, beeing
;; hightlighted while the mouse points to it and beeing activated when
;; pressing return or clicking the button2.

;; Which each link a function and some data are associated.  Upon
;; clicking the function is called with the data as only
;; argument. Both the function and the data are stored in text
;; properties.
;;
;; link-create-link       - insert a new link for the text in the given range
;; link-initialize-keymap - install the keybinding for selecting links

;;; Code:

(eval-when-compile
  (require 'cl))

(defun link-create-link (start end face function &optional data help)
  "Create a link in the current buffer starting from `start' going to `end'.
The `face' is used for displaying, the `data' are stored together with the
link.  Upon clicking the `function' is called with `data' as argument."
  (let ((properties `(face ,face
                      mouse-face highlight
                      link t
                      link-data ,data
                      help-echo ,help
                      link-function ,function)))
    (remove-text-properties start end properties)
    (add-text-properties start end properties)))

(defun link-insert-link (text face function &optional data help)
  "Insert the `text' at point to be formatted as link.
The `face' is used for displaying, the `data' are stored together with the
link.  Upon clicking the `function' is called with `data' as argument."
  (let ((start (point)))
    (insert text)
    (link-create-link start (point) face function data help)))

(defun link-selected (&optional all)
  "Is called upon clicking or otherwise visiting the link."
  (interactive)

  (let* ((properties (text-properties-at (point)))
         (function (plist-get properties 'link-function))
         (data (plist-get properties 'link-data)))
    (if function
        (funcall function data all))))

(defun link-selected-all ()
  "Called for meta clicking the link"
  (interactive)
  (link-selected 'all))

(defun link-mouse-click (event &optional all)
  "Is called upon clicking the link."
  (interactive "@e")

  (mouse-set-point event)
  (link-selected))

(defun link-mouse-click-all (event)
  "Is called upon meta clicking the link."
  (interactive "@e")

  (mouse-set-point event)
  (link-selected-all))

(defun link-next-link ()
  "Return the position of the next link or nil if there is none"
  (let* ((pos (point))
         (pos (next-single-property-change pos 'link)))
    (if pos
        (if (text-property-any pos (min (1+ pos) (point-max)) 'link t)
            pos
          (next-single-property-change pos 'link))
      nil)))


(defun link-prev-link ()
  "Return the position of the previous link or nil if there is none"
  (let* ((pos (point))
         (pos (previous-single-property-change pos 'link)))
    (if pos
        (if (text-property-any pos (1+ pos) 'link t)
            pos
          (let ((val (previous-single-property-change pos 'link)))
            (if val
                val
              (text-property-any (point-min) (1+ (point-min)) 'link t))))
      nil)))

(defun link-initialize-keymap (keymap)
  "Defines the necessary bindings inside keymap"

  (if (and (boundp 'running-xemacs) running-xemacs)
      (progn
        (define-key keymap [button2] 'link-mouse-click)
        (define-key keymap [(meta button2)] 'link-mouse-click-all))
    (define-key keymap [mouse-2] 'link-mouse-click)
    (define-key keymap [M-mouse-2] 'link-mouse-click-all))
    (define-key keymap "\r" 'link-selected)
    (define-key keymap "\M-\r" 'link-selected-all))

(provide 'link)
;;; link.el ends here
