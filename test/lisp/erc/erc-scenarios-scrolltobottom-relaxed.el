;;; erc-scenarios-scrolltobottom-relaxed.el --- erc-scrolltobottom-all relaxed -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

;; TODO assert behavior of prompt input spanning multiple lines, with
;; and without line endings.

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-goodies)

(ert-deftest erc-scenarios-scrolltobottom--relaxed ()
  :tags `(:expensive-test
          ,@(and (getenv "ERC_TESTS_GRAPHICAL") '(:erc--graphical)))
  (when (version< emacs-version "29") (ert-skip "Times out"))

  (should-not erc-scrolltobottom-all)

  (erc-scenarios-common-with-noninteractive-in-term
      ((erc-scenarios-common-dialog "scrolltobottom")
       (dumb-server (erc-d-run "localhost" t 'help))
       (port (process-contact dumb-server :service))
       (erc-modules `(scrolltobottom fill-wrap ,@erc-modules))
       (erc-scrolltobottom-all 'relaxed)
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter))
       lower upper)

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :full-name "tester"
                                :nick "tester")
        (funcall expect 10 "debug mode")))

    (with-current-buffer "foonet"
      (should (looking-at " and"))
      (set-window-buffer nil (current-buffer))
      (delete-other-windows)
      (split-window-below 15)
      (recenter 0)

      (ert-info ("Moving into prompt does not trigger scroll")
        (with-selected-window (next-window)
          (should-not (erc-scenarios-common--at-win-end-p))
          (recenter 0)
          (goto-char (1- erc-insert-marker))
          (execute-kbd-macro "\C-n")
          (should-not (erc-scenarios-common--at-win-end-p))
          (should (= (point) (point-max)))
          (setq lower (count-screen-lines (window-start) (window-point)))))

      (ert-info ("Module `move-to-prompt' still works")
        ;; Prompt is somewhere in the middle of the window.
        (should (erc-scenarios-common--above-win-end-p))
        (should-not (= (point-max) (point)))
        ;; Hitting a self-insert key triggers `move-to-prompt' but not
        ;; a scroll (to bottom).
        (execute-kbd-macro "hi")
        ;; Prompt and input appear on same line.
        (should (= (point-max) (point)))
        (setq upper (count-screen-lines (window-start) (window-point)))
        (should-not (= upper (window-body-height))))

      (ert-info ("Command `recenter-top-bottom' allowed at prompt")
        ;; Hitting C-l recenters the window.
        (should (= upper (count-screen-lines (window-start) (window-point))))
        (let ((lines (list upper)))
          (erc-scenarios-common--recenter-top-bottom)
          (push (count-screen-lines (window-start) (window-point)) lines)
          (erc-scenarios-common--recenter-top-bottom)
          (push (count-screen-lines (window-start) (window-point)) lines)
          (erc-scenarios-common--recenter-top-bottom)
          (push (count-screen-lines (window-start) (window-point)) lines)
          (setq lines (delete-dups lines))
          (should (= (length lines) 4))))

      (ert-info ("Command `beginning-of-buffer' allowed at prompt")
        ;; Hitting C-< goes to beginning of buffer.
        (execute-kbd-macro "\M-<")
        (should (= 1 (point)))
        (redisplay)
        (should (zerop (count-screen-lines (window-start) (window-point))))
        (should (erc-scenarios-common--prompt-past-win-end-p)))

      (ert-info ("New message doesn't trigger scroll when away from prompt")
        ;; Arriving insertions don't trigger a scroll when away from the
        ;; prompt.  New output not seen.
        (erc-cmd-MSG "NickServ help register")
        (save-excursion (erc-d-t-search-for 10 "End of NickServ"))
        (should (= 1 (point)))
        (should (zerop (count-screen-lines (window-start) (window-point))))
        (should (erc-scenarios-common--prompt-past-win-end-p)))

      (ert-info ("New insertion keeps prompt stationary in other window")
        (let ((w (next-window)))
          ;; We're at prompt and completely stationary.
          (should (>= (window-point w) erc-input-marker))
          (erc-d-t-wait-for 10
              (= lower (count-screen-lines (window-start w) (window-point w))))
          (erc-d-t-ensure-for 0.5
              (= lower (count-screen-lines (window-start w)
                                           (window-point w))))))

      (should (= 2 (length (window-list))))
      (ert-info ("New message does not trigger a scroll when at prompt")
        ;; Recenter so prompt is above rather than at window's end.
        (funcall expect 10 "End of NickServ HELP")
        (recenter 0)
        (set-window-point nil (point-max))
        (setq upper (count-screen-lines (window-start) (window-point)))
        ;; Prompt is somewhere in the middle of the window.
        (erc-d-t-wait-for 10 (erc-scenarios-common--above-win-end-p))
        (erc-scenarios-common-say "/msg NickServ help identify")
        ;; New arriving messages don't move prompt.
        (erc-d-t-ensure-for 1
            (= upper (count-screen-lines (window-start) (window-point))))
        (funcall expect 10 "IDENTIFY lets you login")))))

;;; erc-scenarios-scrolltobottom-relaxed.el ends here
