;;; ol-w3m.el --- Copy and Paste From W3M            -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2025 Free Software Foundation, Inc.

;; Author: Andy Stewart <lazycat dot manatee at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements copying HTML content from a w3m buffer and
;; transforming the text on the fly so that it can be pasted into an
;; Org buffer with hot links.  It will also work for regions in gnus
;; buffers that have been washed with w3m.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Acknowledgments:

;; Richard Riley <rileyrgdev at googlemail dot com>
;;
;;      The idea of transforming the HTML content with Org syntax is
;;      proposed by Richard, I'm just coding it.
;;

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ol)

(defvar w3m-current-url)
(defvar w3m-current-title)

(org-link-set-parameters "w3m" :store #'org-w3m-store-link)
(defun org-w3m-store-link ()
  "Store a link to a w3m buffer."
  (when (eq major-mode 'w3m-mode)
    (org-link-store-props
     :type "w3m"
     :link w3m-current-url
     :url (url-view-url t)
     :description (or w3m-current-title w3m-current-url))))

(defun org-w3m-copy-for-org-mode ()
  "Copy current buffer content or active region with Org style links.
This will encode `link-title' and `link-location' with
`org-link-make-string', and insert the transformed test into the kill ring,
so that it can be yanked into an Org  buffer with links working correctly."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (transform-start (point-min))
         (transform-end (point-max))
         return-content
         link-location link-title
         temp-position out-bound)
    (when regionp
      (setq transform-start (region-beginning))
      (setq transform-end (region-end))
      ;; Deactivate mark if current mark is activate.
      (deactivate-mark))
    (message "Transforming links...")
    (save-excursion
      (goto-char transform-start)
      (while (and (not out-bound)	; still inside region to copy
                  (not (org-w3m-no-next-link-p))) ; no next link current buffer
        ;; store current point before jump next anchor
        (setq temp-position (point))
        ;; move to next anchor when current point is not at anchor
        (or (get-text-property (point) 'w3m-href-anchor) (org-w3m-get-next-link-start))
        (cond
         ((<= (point) transform-end) ; point is inside transform bound
          ;; get content between two links.
          (when (> (point) temp-position)
            (setq return-content (concat return-content
                                         (buffer-substring
                                          temp-position (point)))))
          (cond
           ((setq link-location (get-text-property (point) 'w3m-href-anchor))
            ;; current point is a link
            ;; (we thus also got link location at current point)
            ;; get link title at current point.
            (setq link-title (buffer-substring (point)
                                               (org-w3m-get-anchor-end)))
            ;; concat Org style url to `return-content'.
            (setq return-content
                  (concat return-content
                          (if (org-string-nw-p link-location)
                              (org-link-make-string link-location link-title)
                            link-title))))
           ((setq link-location (get-text-property (point) 'w3m-image))
            ;; current point is an image
            ;; (we thus also got image link location at current point)
            ;; get link title at current point.
            (setq link-title (buffer-substring (point) (org-w3m-get-image-end)))
            ;; concat Org style url to `return-content'.
            (setq return-content
                  (concat return-content
                          (if (org-string-nw-p link-location)
                              (org-link-make-string link-location link-title)
                            link-title))))
           (t nil))); current point is neither a link nor an image
         (t ; point is NOT inside transform bound
          (goto-char temp-position) ; reset point before jump next anchor
          (setq out-bound t))))	    ; for break out `while' loop
      ;; add the rest until end of the region to be copied
      (when (< (point) transform-end)
        (setq return-content
              (concat return-content
                      (buffer-substring (point) transform-end))))
      (org-kill-new return-content)
      (message "Transforming links...done, use C-y to insert text into Org file")
      (message "Copy with link transformation complete."))))

(defun org-w3m-get-anchor-start ()
  "Move cursor to the start of current anchor.  Return point."
  ;; get start position of anchor or current point
  ;; NOTE: This function seems never to be used. Should it be removed?
  (goto-char (or (previous-single-property-change (point) 'w3m-anchor-sequence)
                 (point))))

(defun org-w3m-get-anchor-end ()
  "Move cursor to the end of current anchor.  Return point."
  ;; get end position of anchor or point
  (goto-char (or (next-single-property-change (point) 'w3m-anchor-sequence)
		 (point))))

(defun org-w3m-get-image-end ()
  "Move cursor to the end of current image.  Return point."
  ;; get end position of image or point
  ;; NOTE: Function `org-w3m-get-image-start' was not created because
  ;;       function `org-w3m-get-anchor-start' is never used.
  (goto-char (or (next-single-property-change (point) 'w3m-image)
		 (point))))

(defun org-w3m-get-next-link-start ()
  "Move cursor to the start of next link or image.  Return point."
  (let (pos start-pos anchor-pos image-pos)
    (setq pos (setq start-pos (point)))
    (setq anchor-pos
          (catch 'reach
            (while (setq pos (next-single-property-change pos 'w3m-anchor-sequence))
              (when (get-text-property pos 'w3m-href-anchor)
                (throw 'reach pos)))))
    (setq pos start-pos)
    (setq image-pos
          (catch 'reach
            (while (setq pos (next-single-property-change pos 'w3m-image))
              (when (get-text-property pos 'w3m-image)
                (throw 'reach pos)))))
    (goto-char (min (or anchor-pos (point-max)) (or image-pos (point-max))))))

(defun org-w3m-get-prev-link-start ()
  "Move cursor to the start of previous link.  Return point."
  ;; NOTE: This function is only called by `org-w3m-no-prev-link-p',
  ;;       which itself seems never to be used. Should it be removed?
  ;;
  ;; WARNING: This function has not been updated to account for
  ;;      `w3m-image'. See `org-w3m-get-next-link-start'.
  (catch 'reach
    (let ((pos (point)))
      (while (setq pos (previous-single-property-change pos 'w3m-anchor-sequence))
        (when (get-text-property pos 'w3m-href-anchor)
          ;; jump to previous anchor
          (goto-char pos)
          ;; return point when current is valid link
          (throw 'reach nil)))))
  (point))

(defun org-w3m-no-next-link-p ()
  "Whether there is no next link after the cursor.
Return t if there is no next link; otherwise, return nil."
  (save-excursion
    (equal (point) (org-w3m-get-next-link-start))))

(defun org-w3m-no-prev-link-p ()
  "Whether there is no previous link after the cursor.
Return t if there is no previous link; otherwise, return nil."
  ;; NOTE: This function seems never to be used. Should it be removed?
  (save-excursion
    (equal (point) (org-w3m-get-prev-link-start))))

;; Install keys into the w3m keymap
(defvar w3m-mode-map)
(defvar w3m-minor-mode-map)
(when (and (boundp 'w3m-mode-map)
           (keymapp w3m-mode-map))
  (define-key w3m-mode-map "\C-c\C-x\M-w" 'org-w3m-copy-for-org-mode)
  (define-key w3m-mode-map "\C-c\C-x\C-w" 'org-w3m-copy-for-org-mode))
(when (and (boundp 'w3m-minor-mode-map)
           (keymapp w3m-minor-mode-map))
  (define-key w3m-minor-mode-map "\C-c\C-x\M-w" 'org-w3m-copy-for-org-mode)
  (define-key w3m-minor-mode-map "\C-c\C-x\C-w" 'org-w3m-copy-for-org-mode))
(add-hook
 'w3m-mode-hook
 (lambda ()
   (define-key w3m-mode-map "\C-c\C-x\M-w" 'org-w3m-copy-for-org-mode)
   (define-key w3m-mode-map "\C-c\C-x\C-w" 'org-w3m-copy-for-org-mode)))
(add-hook
 'w3m-minor-mode-hook
 (lambda ()
   (define-key w3m-minor-mode-map "\C-c\C-x\M-w" 'org-w3m-copy-for-org-mode)
   (define-key w3m-minor-mode-map "\C-c\C-x\C-w" 'org-w3m-copy-for-org-mode)))

(provide 'ol-w3m)

;;; ol-w3m.el ends here
