;;; mh-xemacs-compat.el --- GNU Emacs Functions needed by XEmacs

;; Copyright (C) 2001, 02, 2003 Free Software Foundation, Inc.

;; Author: FSF
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Change Log:

;;; Code:

;;; Some requires:
(require 'rfc822)

(eval-when-compile (require 'mh-utils))

;;; Simple compatibility:

(unless (fboundp 'match-string-no-properties)
  (defsubst match-string-no-properties (match)
    (buffer-substring-no-properties
     (match-beginning match) (match-end match))))

(unless (fboundp 'line-beginning-position)
  (defalias 'line-beginning-position 'point-at-bol))
(unless (fboundp 'line-end-position)
  (defalias 'line-end-position 'point-at-eol))

(unless (fboundp 'timerp)
  (defalias 'timerp 'itimerp))
(unless (fboundp 'cancel-timer)
  (defalias 'cancel-timer 'delete-itimer))

;; Set up the modeline glyph
(defconst mh-modeline-logo
  "/* XPM */
static char * file[] = {
\"18 13 2 1\",
\"# c #666699\",
\". c None s None\",
\"........##........\",
\".......####.......\",
\"......######......\",
\"......######......\",
\"....#########.....\",
\"..##############..\",
\".##...######....#.\",
\"##...#.#.####...#.\",
\"....#..#.##.#...#.\",
\"...#..##.#.#.#....\",
\"...#..#..#..#.#...\",
\"...#..#.##..#.##..\",
\"...#..#.#..#....#.\"};"
  "The image for the modeline logo.")

(mh-do-in-xemacs
  (defvar mh-modeline-glyph
    (progn
      (let* ((data mh-modeline-logo)
             (glyph (make-glyph
                     (cond ((and (featurep 'xpm)
                                 (device-on-window-system-p)
                                 has-modeline-p)
                            `[xpm :data ,data])
                           (t [string :data "MH-E"])))))
        (set-glyph-face glyph 'modeline-buffer-id)
        glyph))
    "Cute little logo to put in the modeline of MH-E buffers."))

(provide 'mh-xemacs-compat)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: f531e3cc-98ba-4f9f-b6a1-e282173a6aa9
;;; mh-xemacs-compat.el ends here
