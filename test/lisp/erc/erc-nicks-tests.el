;;; erc-nicks-tests.el --- Tests for erc-nicks  -*- lexical-binding:t -*-

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

;;; Commentary:

;; Unlike most of ERC's tests, the ones in this file can be run
;; interactively in the same session.

;; TODO:
;;
;; * Add mock session (or scenario) with buffer snapshots, like those
;;   in erc-fill-tests.el.  (Should probably move helpers to a common
;;   library under ./resources.)

;;; Code:

(require 'erc-nicks)
(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-tests-common)))

;; This function replicates the behavior of older "invert" strategy
;; implementations from EmacsWiki, etc.  The values for the lower and
;; upper bounds (0.33 and 0.66) are likewise inherited.  See
;; `erc-nicks--invert-classic--dark' below for one reason its results
;; may not be plainly obvious.
(defun erc-nicks-tests--invert-classic (color)
  (if (pcase (erc-nicks--bg-mode)
        ('dark (< (erc-nicks--get-luminance color) (/ 1 3.0)))
        ('light (> (erc-nicks--get-luminance color) (/ 2 3.0))))
      (list (- 1.0 (nth 0 color)) (- 1.0 (nth 1 color)) (- 1.0 (nth 2 color)))
    color))


(ert-deftest erc-nicks--get-luminance ()
  (should (eql 0.0 (erc-nicks--get-luminance "black")))
  (should (eql 1.0 (erc-nicks--get-luminance "white")))
  (should (eql 21.0 (/ (+ 0.05 1.0) (+ 0.05 0.0))))

  ;; RGB floats from a `display-graphic-p' session.
  (let ((a (erc-nicks--get-luminance ; #9439ad
            '(0.5803921568627451 0.2235294117647059 0.6784313725490196)))
        (b (erc-nicks--get-luminance ; #ae54c7
            '(0.6823529411764706 0.32941176470588235 0.7803921568627451)))
        (c (erc-nicks--get-luminance ; #d19ddf
            '(0.8196078431372549 0.615686274509804 0.8745098039215686)))
        (d (erc-nicks--get-luminance ; #f5e8f8
            '(0.9607843137254902 0.9098039215686274 0.9725490196078431))))
    ;; Low, med, high contrast comparisons against known values from
    ;; an external source.
    (should (eql 1.42 (/ (round (* 100 (/ (+ 0.05 b) (+ 0.05 a)))) 100.0)))
    (should (eql 2.78 (/ (round (* 100 (/ (+ 0.05 c) (+ 0.05 a)))) 100.0)))
    (should (eql 5.16 (/ (round (* 100 (/ (+ 0.05 d) (+ 0.05 a)))) 100.0)))))

(ert-deftest erc-nicks-invert--classic ()
  (let ((convert (lambda (n) (apply #'color-rgb-to-hex
                                    (erc-nicks-tests--invert-classic
                                     (color-name-to-rgb n))))))
    (let ((erc-nicks--bg-mode-value 'dark))
      (should (equal (funcall convert "white") "#ffffffffffff"))
      (should (equal (funcall convert "black") "#ffffffffffff"))
      (should (equal (funcall convert "green") "#0000ffff0000")))
    (let ((erc-nicks--bg-mode-value 'light))
      (should (equal (funcall convert "white") "#000000000000"))
      (should (equal (funcall convert "black") "#000000000000"))
      (should (equal (funcall convert "green") "#ffff0000ffff")))))

(ert-deftest erc-nicks--get-contrast ()
  (should (= 21.0 (erc-nicks--get-contrast "white" "black")))
  (should (= 21.0 (erc-nicks--get-contrast "black" "white")))
  (should (= 1.0 (erc-nicks--get-contrast "black" "black")))
  (should (= 1.0 (erc-nicks--get-contrast "white" "white"))))

(defun erc-nicks-tests--print-contrast (fn color)
  (let* ((erc-nicks-color-adjustments (list fn))
         (result (erc-nicks--reduce color))
         (start (point)))
    (insert (format "%16s%-16s%16s%-16s\n"
                    (concat color "-")
                    (concat ">" result)
                    (concat color " ")
                    (concat " " result)))
    (put-text-property (+ start 32) (+ start 48) 'face
                       (list :background color :foreground result))
    (put-text-property (+ start 48) (+ start 64) 'face
                       (list :background result :foreground color))
    result))

(ert-deftest erc-nicks--invert-classic--light ()
  (let ((erc-nicks--bg-luminance 1.0)
        (erc-nicks--bg-mode-value 'light)
        (show (lambda (c) (erc-nicks-tests--print-contrast
                           #'erc-nicks-tests--invert-classic c))))

    (with-current-buffer (get-buffer-create
                          "*erc-nicks--invert-classic--light*")
      (should (equal "#000000000000" (funcall show "white")))
      (should (equal "#000000000000" (funcall show "black")))
      (should (equal "#ffff00000000" (funcall show "red")))
      (should (equal "#ffff0000ffff" (funcall show "green"))) ; magenta
      (should (equal "#00000000ffff" (funcall show "blue")))

      (unless noninteractive
        (should (equal "#bbbbbbbbbbbb" (funcall show "#bbbbbbbbbbbb")))
        (should (equal "#cccccccccccc" (funcall show "#cccccccccccc")))
        (should (equal "#222122212221" (funcall show "#dddddddddddd")))
        (should (equal "#111011101110" (funcall show "#eeeeeeeeeeee"))))

      (when noninteractive
        (kill-buffer)))))

;; This shows that the output can be darker (have less contrast) than
;; the input.
(ert-deftest erc-nicks--invert-classic--dark ()
  (let ((erc-nicks--bg-luminance 0.0)
        (erc-nicks--bg-mode-value 'dark)
        (show (lambda (c) (erc-nicks-tests--print-contrast
                           #'erc-nicks-tests--invert-classic c))))

    (with-current-buffer (get-buffer-create
                          "*erc-nicks--invert-classic--dark*")
      (should (equal "#ffffffffffff" (funcall show "white")))
      (should (equal "#ffffffffffff" (funcall show "black")))
      (should (equal "#0000ffffffff" (funcall show "red"))) ; cyan
      (should (equal "#0000ffff0000" (funcall show "green")))
      (should (equal "#ffffffff0000" (funcall show "blue"))) ; yellow

      (unless noninteractive
        (should (equal "#aaaaaaaaaaaa" (funcall show "#555555555555")))
        (should (equal "#999999999999" (funcall show "#666666666666")))
        (should (equal "#888888888888" (funcall show "#777777777777")))
        (should (equal "#777777777777" (funcall show "#888888888888")))
        (should (equal "#666666666666" (funcall show "#999999999999")))
        (should (equal "#aaaaaaaaaaaa" (funcall show "#aaaaaaaaaaaa"))))

      (when noninteractive
        (kill-buffer)))))

;; These are the same as the legacy version but work in terms of
;; contrast ratios.  Converting the original bounds to contrast ratios
;; (assuming pure white and black backgrounds) gives:
;;
;;   min-lum of 0.33 ~~> 1.465
;;   max-lum of 0.66 ~~> 7.666
;;
(ert-deftest erc-nicks-invert--light ()
  (let ((erc-nicks--bg-luminance 1.0)
        (erc-nicks--bg-mode-value 'light)
        (erc-nicks-contrast-range '(1.465))
        (show (lambda (c) (erc-nicks-tests--print-contrast
                           #'erc-nicks-invert c))))

    (with-current-buffer (get-buffer-create
                          "*erc-nicks--invert-classic--light*")
      (should (equal "#000000000000" (funcall show "white")))
      (should (equal "#000000000000" (funcall show "black")))
      (should (equal "#ffff00000000" (funcall show "red")))
      (should (equal "#ffff0000ffff" (funcall show "green"))) ; magenta
      (should (equal "#00000000ffff" (funcall show "blue")))

      (unless noninteractive
        (should (equal "#bbbbbbbbbbbb" (funcall show "#bbbbbbbbbbbb")))
        (should (equal "#cccccccccccc" (funcall show "#cccccccccccc")))
        (should (equal "#222122212221" (funcall show "#dddddddddddd")))
        (should (equal "#111011101110" (funcall show "#eeeeeeeeeeee"))))

      (when noninteractive
        (kill-buffer)))))

(ert-deftest erc-nicks-invert--dark ()
  (let ((erc-nicks--bg-luminance 0.0)
        (erc-nicks--bg-mode-value 'dark)
        (erc-nicks-contrast-range '(7.666))
        (show (lambda (c) (erc-nicks-tests--print-contrast
                           #'erc-nicks-invert c))))

    (with-current-buffer (get-buffer-create "*erc-nicks-invert--dark*")
      (should (equal "#ffffffffffff" (funcall show "white")))
      (should (equal "#ffffffffffff" (funcall show "black")))
      (should (equal "#0000ffffffff" (funcall show "red"))) ; cyan
      (should (equal "#0000ffff0000" (funcall show "green")))
      (should (equal "#ffffffff0000" (funcall show "blue"))) ; yellow

      (unless noninteractive
        (should (equal "#aaaaaaaaaaaa" (funcall show "#555555555555")))
        (should (equal "#999999999999" (funcall show "#666666666666")))
        (should (equal "#888888888888" (funcall show "#777777777777")))
        (should (equal "#888888888888" (funcall show "#888888888888")))
        (should (equal "#999999999999" (funcall show "#999999999999"))))

      (when noninteractive
        (kill-buffer)))))

(ert-deftest erc-nicks-add-contrast ()
  (let ((erc-nicks--bg-luminance 1.0)
        (erc-nicks--bg-mode-value 'light)
        (erc-nicks--fg-rgb '(0.0 0.0 0.0))
        (erc-nicks-bg-color "white")
        (erc-nicks-contrast-range '(3.5))
        (show (lambda (c) (erc-nicks-tests--print-contrast
                           #'erc-nicks-add-contrast c))))

    (with-current-buffer (get-buffer-create "*erc-nicks-add-contrast*")
      (should (equal "#893a893a893a" (funcall show "white")))
      (should (equal "#893a893a893a" (funcall show "#893a893a893a")))
      (should (equal "#000000000000" (funcall show "black")))
      (should (equal "#ffff00000000" (funcall show "red")))
      (should (equal "#0000a12e0000" (funcall show "green")))
      (should (equal "#00000000ffff" (funcall show "blue")))

      ;; When the input is already near the desired ratio, the result
      ;; may not be in bounds, only close.  But the difference is
      ;; usually imperceptible.
      (unless noninteractive
        ;; Well inside (light slate gray)
        (should (equal "#777788889999" (funcall show "#777788889999")))
        ;; Slightly outside -> just outside
        (should (equal "#7c498bd39b5c" (funcall show "#88889999aaaa")))
        ;; Just outside -> just inside
        (should (equal "#7bcc8b479ac0" (funcall show "#7c498bd39b5c")))
        ;; Just inside
        (should (equal "#7bcc8b479ac0" (funcall show "#7bcc8b479ac0"))))

      (when noninteractive
        (kill-buffer)))))

(ert-deftest erc-nicks-cap-contrast ()
  (should (= 12.5 (cdr erc-nicks-contrast-range)))
  (let ((erc-nicks--bg-luminance 1.0)
        (erc-nicks--bg-mode-value 'light)
        (erc-nicks--fg-rgb '(0.0 0.0 0.0))
        (erc-nicks-bg-color "white")
        (show (lambda (c) (erc-nicks-tests--print-contrast
                           #'erc-nicks-cap-contrast c))))

    (with-current-buffer (get-buffer-create "*erc-nicks-remove-contrast*")
      (should (equal (funcall show "black") "#34e534e534e5" )) ; 21.0 -> 12.14
      (should ; 12.32 -> 12.32 (same)
       (equal (funcall show "#34e534e534e5") "#34e534e534e5"))
      (should (equal (funcall show "white") "#ffffffffffff"))

      (unless noninteractive
        (should (equal (funcall show "DarkRed") "#8b8b00000000"))
        (should (equal (funcall show "DarkGreen") "#000064640000"))
        ;; 15.29 -> 12.38
        (should (equal (funcall show "DarkBlue") "#1cf11cf198b5"))

        ;; 12.50 -> 12.22
        (should (equal (funcall show "#33e033e033e0") "#34ab34ab34ab"))
        ;; 12.57 -> 12.28
        (should (equal (funcall show "#338033803380") "#344c344c344c"))
        ;; 12.67 -> 12.37
        (should (equal (funcall show "#330033003300") "#33cc33cc33cc")))

      (when noninteractive
        (kill-buffer)))))

(ert-deftest erc-nicks--skip-p ()
  ;; Baseline
  (should-not (erc-nicks--skip-p 'bold nil 10000000))
  (should-not (erc-nicks--skip-p '(bold) nil 10000000))
  (should-not (erc-nicks--skip-p nil '(bold) 10000000))
  (should-not (erc-nicks--skip-p 'bold '(bold) 0))
  (should-not (erc-nicks--skip-p '(bold) '(bold) 0))
  (should-not (erc-nicks--skip-p 'bold '(foo bold) 0))
  (should-not (erc-nicks--skip-p '((:inherit bold)) '(bold) 1))
  (should (erc-nicks--skip-p 'bold '(bold) 1))
  (should (erc-nicks--skip-p 'bold '(fake bold) 1))
  (should (erc-nicks--skip-p 'bold '(foo bar bold) 1))
  (should (erc-nicks--skip-p '(bold) '(bold) 1))
  (should (erc-nicks--skip-p '((bold)) '(bold) 1))
  (should (erc-nicks--skip-p '((((bold)))) '(bold) 1))
  (should (erc-nicks--skip-p '(bold) '(foo bold) 1))
  (should (erc-nicks--skip-p '(:inherit bold) '((:inherit bold)) 1))
  (should (erc-nicks--skip-p '((:inherit bold)) '((:inherit bold)) 1))
  (should (erc-nicks--skip-p '(((:inherit bold))) '((:inherit bold)) 1))

  ;; Composed
  (should-not (erc-nicks--skip-p '(italic bold) '(bold) 1))
  (should-not (erc-nicks--skip-p '((italic) bold) '(bold) 1))
  (should-not (erc-nicks--skip-p '(italic (bold)) '(bold) 1))
  (should (erc-nicks--skip-p '(italic bold) '(bold) 2))
  (should (erc-nicks--skip-p '((italic) bold) '(bold) 2))
  (should (erc-nicks--skip-p '(italic (bold)) '(bold) 2))

  (should-not (erc-nicks--skip-p '(italic default bold) '(bold) 2))
  (should-not (erc-nicks--skip-p '((default italic) bold) '(bold) 2))
  (should-not (erc-nicks--skip-p '(italic (default bold)) '(bold) 2))
  (should-not (erc-nicks--skip-p '((default italic) (bold shadow)) '(bold) 2))
  (should (erc-nicks--skip-p '((default italic) bold) '(bold) 3))
  (should (erc-nicks--skip-p '(italic (default bold)) '(bold) 3))
  (should (erc-nicks--skip-p '((default italic) (bold shadow)) '(bold) 3))
  (should (erc-nicks--skip-p '(italic (default (bold shadow))) '(bold) 3)))

(ert-deftest erc-nicks--trim ()
  (should (equal (erc-nicks--trim "Bob`") "bob"))
  (should (equal (erc-nicks--trim "Bob``") "bob"))

  ;; `erc--casemapping-rfc1459'
  (let ((erc-nicks-ignore-chars "^"))
    (should (equal (erc-nicks--trim "Bob~") "bob^"))
    (should (equal (erc-nicks--trim "Bob^") "bob"))))

(defvar erc-nicks-tests--fake-face-list nil)

;; Since we can't delete faces, mock `face-list' to only return those
;; in `erc-nicks--face-table' created by the current test.
(defun erc-nicks-tests--face-list ()
  (let ((table (buffer-local-value 'erc-nicks--face-table
                                   (get-buffer "foonet")))
        out)
    (maphash (lambda (k v)
               (when (member k erc-nicks-tests--fake-face-list)
                 (push v out)))
             table)
    (nreverse out)))

(defun erc-nicks-tests--create-session (test alice bob)
  (should-not (memq 'nicks erc-modules))
  (advice-add 'face-list :override #'erc-nicks-tests--face-list)
  (let ((erc-modules (cons 'nicks erc-modules))
        (inhibit-message noninteractive)
        (erc-nicks-tests--fake-face-list
         (list (downcase alice) (downcase bob)))
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (with-current-buffer
        (cl-letf
            (((symbol-function 'erc-server-connect)
              (lambda (&rest _)
                (setq erc-server-process
                      (start-process "sleep" (current-buffer) "sleep" "1"))
                (set-process-query-on-exit-flag erc-server-process nil))))

          (erc-open "localhost" 6667 "tester" "Tester" 'connect
                    nil nil nil nil nil "tester"))

      (let ((inhibit-message noninteractive))
        (dolist (line (split-string "\
:irc.foonet.org 004 tester irc.foonet.org irc.d abc 123 456
:irc.foonet.org 005 tester NETWORK=foonet :are supported
:irc.foonet.org 376 tester :End of /MOTD command."
                                    "\n"))
          (erc-parse-server-response erc-server-process line)))

      (with-current-buffer (erc--open-target "#chan")
        (erc-update-channel-member
         "#chan" alice alice t nil nil nil nil nil "fake" "~u" nil nil t)

        (erc-update-channel-member
         "#chan" bob bob t nil nil nil nil nil "fake" "~u" nil nil t)

        (erc-display-message
         nil 'notice (current-buffer)
         (concat "This server is in debug mode and is logging all user I/O. "
                 "Blah " alice " (1) " bob " (2) blah."))

        (erc-display-message nil nil (current-buffer)
                             (erc-format-privmessage bob "Hi Alice" nil t))

        (erc-display-message nil nil (current-buffer)
                             (erc-format-privmessage alice "Hi Bob" nil t)))

      (funcall test)

      (when noninteractive
        (kill-buffer "#chan")
        (when (get-buffer " *Custom-Work*")
          (kill-buffer " *Custom-Work*"))
        (kill-buffer))))
  (advice-remove 'face-list #'erc-nicks-tests--face-list))

(ert-deftest erc-nicks-list-faces ()
  (erc-nicks-tests--create-session
   (lambda ()
     (erc-nicks-list-faces)
     (let ((table (buffer-local-value 'erc-nicks--face-table
                                      (get-buffer "foonet")))
           calls)
       (cl-letf (((symbol-function 'erc-nicks--list-faces-help-button-action)
                  (lambda (&rest r) (push r calls))))
         (with-current-buffer "*Faces*"
           (set-window-buffer (selected-window) (current-buffer))
           (goto-char (point-min))

           (ert-info ("Clicking on face link runs action function")
             (forward-button 1)
             (should (looking-at "erc-nicks-alice1-face"))
             (push-button)
             (should (eq (car (car calls)) (gethash "alice1" table))))

           (ert-info ("Clicking on sample text describes face")
             (forward-button 1)
             (should (looking-at (rx "#" (+ xdigit))))
             (push-button)
             (should (search-forward-regexp
                      (rx "Foreground: #" (group (+ xdigit)) eol)))
             (forward-button 2) ; skip Inherit:...
             (push-button))

           (ert-info ("First entry's sample is rendered correctly")
             (let ((hex (match-string 1)))
               (should (looking-at (concat "#" hex)))
               (goto-char (button-end (point)))
               (should (looking-back " foonet"))
               (should (eq (button-get (1- (point)) 'face) (car (pop calls))))
               (should-not calls)))

           (ert-info ("Clicking on another entry's face link runs action")
             (forward-button 1)
             (should (looking-at "erc-nicks-bob1-face"))
             (push-button)
             (should (eq (car (car calls)) (gethash "bob1" table))))

           (ert-info ("Second entry's sample is rendered correctly")
             (forward-button 1)
             (should (looking-at (rx "#" (+ xdigit))))
             (goto-char (button-end (point)))
             (should (looking-back " foonet"))
             (should (eq (button-get (1- (point)) 'face) (car (pop calls))))
             (should-not calls))

           (when noninteractive
             (kill-buffer))))))
   "Alice1" "Bob1"))

(ert-deftest erc-nicks-customize-face ()
  (unless (>= emacs-major-version 28)
    (ert-skip "Face link required in customize-face buffers"))
  (erc-nicks-tests--create-session
   (lambda ()
     (erc-nicks-list-faces)
     (with-current-buffer "*Faces*"
       (set-window-buffer (selected-window) (current-buffer))
       (goto-char (point-min))

       (ert-info ("Clicking on face link runs action function")
         (forward-button 1)
         (should (looking-at "erc-nicks-alice2"))
         (ert-simulate-keys "y\r"
           (call-interactively #'push-button nil)))

       (with-current-buffer "*Customize Face: Erc Nicks Alice2@Foonet Face*"
         (should (search-forward "Erc Nicks Alice2@Foonet Face" nil t))
         (widget-button-press (1- (point))))

       (with-current-buffer "*New face erc-nicks-alice2@foonet-face*"
         (goto-char (point-min))
         (should (search-forward "(use-package erc-nicks" nil t))
         (should (search-forward ":foreground \"#" nil t))
         (when noninteractive
           (kill-buffer)))

       (with-current-buffer  "*Customize Face: Erc Nicks Alice2@Foonet Face*"
         (should (search-forward "Foreground: #" nil t))
         (when noninteractive
           (kill-buffer)))

       (when noninteractive
         (kill-buffer))))
   "Alice2" "Bob2"))

(ert-deftest erc-nicks--gen-key-from-format-spec ()
  (let ((erc-network 'OFTC)
        (erc-nicks-key-suffix-format "@%-012n")
        (erc-server-current-nick "tester"))
    (should (equal (erc-nicks--gen-key-from-format-spec "bob")
                   "bob@OFTC00000000")))

  (let ((erc-network 'Libera.Chat)
        (erc-nicks-key-suffix-format "@%-012n")
        (erc-server-current-nick "tester"))
    (should (equal (erc-nicks--gen-key-from-format-spec "bob")
                   "bob@Libera.Chat0")))

  (let* ((erc-network 'Libera.Chat)
         (erc-nicks-key-suffix-format "@%n/%m")
         (erc-server-current-nick "tester"))
    (should (equal (erc-nicks--gen-key-from-format-spec "bob")
                   "bob@Libera.Chat/tester"))))

(ert-deftest erc-nicks--create-culled-pool ()
  (let ((erc-nicks--bg-luminance 1.0)
        (erc-nicks--bg-mode-value 'light)
        (erc-nicks--fg-rgb '(0.0 0.0 0.0))
        (erc-nicks-bg-color "white")
        ;;
        (erc-nicks--colors-rejects '(t)))

    ;; Reject
    (should-not (erc-nicks--create-culled-pool '(erc-nicks-invert) '("white")))
    (should (equal (pop erc-nicks--colors-rejects) "white")) ; too close
    (should-not
     (erc-nicks--create-culled-pool '(erc-nicks-cap-contrast) '("black")))
    (should (equal (pop erc-nicks--colors-rejects) "black")) ; too far
    (should-not
     (erc-nicks--create-culled-pool '(erc-nicks-ensaturate) '("white")))
    (should (equal (pop erc-nicks--colors-rejects) "white")) ; lacks color
    (should-not
     (erc-nicks--create-culled-pool '(erc-nicks-ensaturate) '("red")))
    (should (equal (pop erc-nicks--colors-rejects) "red")) ; too much color

    ;; Safe
    (should (equal (erc-nicks--create-culled-pool '(erc-nicks-invert)
                                                  '("black"))
                   '("black")))
    (should (equal (erc-nicks--create-culled-pool '(erc-nicks-add-contrast)
                                                  '("black"))
                   '("black")))
    (should (equal (erc-nicks--create-culled-pool '(erc-nicks-cap-contrast)
                                                  '("white"))
                   '("white")))
    (let ((erc-nicks-saturation-range '(0.5 . 1.0)))
      (should (equal (erc-nicks--create-culled-pool '(erc-nicks-ensaturate)
                                                    '("green"))
                     '("green"))))
    (let ((erc-nicks-saturation-range '(0.0 . 0.5)))
      (should (equal (erc-nicks--create-culled-pool '(erc-nicks-ensaturate)
                                                    '("gray"))
                     '("gray"))))
    (unless noninteractive
      (should (equal (erc-nicks--create-culled-pool '(erc-nicks-ensaturate)
                                                    '("firebrick"))
                     '("firebrick"))))
    (should (equal erc-nicks--colors-rejects '(t)))))

(ert-deftest erc-nicks--create-coerced-pool ()
  (let ((erc-nicks--bg-luminance 1.0)
        (erc-nicks--bg-mode-value 'light)
        (erc-nicks--fg-rgb '(0.0 0.0 0.0))
        (erc-nicks-bg-color "white")
        (num-colors (length (defined-colors)))
        ;;
        (erc-nicks--colors-rejects '(t)))

    ;; Deduplication.
    (when (= 8 num-colors)
      (should (equal (erc-nicks--create-coerced-pool '(erc-nicks-ensaturate)
                                                     '("#ee0000" "#f80000"))
                     '("red")))
      (should (equal (pop erc-nicks--colors-rejects) "#f80000")))

    ;; "Coercion" in Xterm.
    (unless noninteractive
      (when (= 665 num-colors)
        (pcase-dolist (`(,adjustments ,candidates ,result)
                       '(((erc-nicks-invert) ("white") ("gray10"))
                         ((erc-nicks-cap-contrast) ("black") ("gray20"))
                         ((erc-nicks-ensaturate) ("white") ("lavenderblush2"))
                         ((erc-nicks-ensaturate) ("red") ("firebrick"))))
          (should (equal (erc-nicks--create-coerced-pool adjustments
                                                         candidates)
                         result)))))

    (should (equal erc-nicks--colors-rejects '(t)))))

;;; erc-nicks-tests.el ends here
