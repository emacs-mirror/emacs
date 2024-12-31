;;; elb-scroll.el --- Benchmark scrolling performance  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Benchmarking the scrolling of a font-locked buffer.
;; This is hard to do in a way that reflects real use because it
;; requires drawing and such, which in turns requires an interactive
;; session, whereas we usually want to run benchmarks in batch mode.
;;
;; We use `redisplay-skip-initial-frame' and (redisplay 'force) to
;; get the redisplay to do its job when running in batch mode, but
;; this benchmark will still give very different results in an interactive
;; session.

;;; Code:

(unless (and noninteractive (not (boundp 'redisplay-skip-initial-frame)))
  (defun elb-scroll-entry ()
    ;; FIXME: This relies on `elb-smie.el' being compiled already which is
    ;; not necessarily the case if we're only running some of the benchmarks.
    (load (expand-file-name "elb-smie" elb-bench-directory))
    (setq redisplay-skip-initial-frame nil)
    (with-temp-buffer
      (rename-buffer (generate-new-buffer-name "elb-scroll"))
      (switch-to-buffer (current-buffer))
      (insert-file-contents (expand-file-name
                             "../resources/xmenu.c" elb-bench-directory))
      (redisplay 'force) ;; Refresh the window dimensions.
      (enlarge-window (- 23 (window-height)))
      (enlarge-window (- 80 (window-width)) 'horiz)
      (redisplay 'force) ;; Refresh the window dimensions.
      (message "Window size: %S x %S" (window-height) (window-width))
      (unless (and (equal 23 (window-height))
                   (equal 80 (window-width)))
        (error "Window size not as stipulated by the benchmark"))
      (dotimes (_ 10)
        (elb-smie-mode)
        (goto-char (point-min))
        (condition-case nil
            (while t (scroll-up nil) (redisplay 'force))
          (end-of-buffer nil))))))

(provide 'elb-scroll)
;;; elb-scroll.el ends here
