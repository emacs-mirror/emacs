;; -*- no-byte-compile: t -*-
;; Define function key sequences for DEC terminals.

(defvar lk201-function-map nil
  "Function key definitions for DEC terminals.")

;; Make reloads faster.
(unless lk201-function-map
  (setq lk201-function-map (make-sparse-keymap))

  ;; Termcap or terminfo should set these.
  ;; (define-key lk201-function-map "\e[A" [up])
  ;; (define-key lk201-function-map "\e[B" [down])
  ;; (define-key lk201-function-map "\e[C" [right])
  ;; (define-key lk201-function-map "\e[D" [left])

  (define-key lk201-function-map "\e[1~" [find])
  (define-key lk201-function-map "\e[2~" [insert])
  (define-key lk201-function-map "\e[3~" [delete])
  (define-key lk201-function-map "\e[4~" [select])
  (define-key lk201-function-map "\e[5~" [prior])
  (define-key lk201-function-map "\e[6~" [next])
  (define-key lk201-function-map "\e[11~" [f1])
  (define-key lk201-function-map "\e[12~" [f2])
  (define-key lk201-function-map "\e[13~" [f3])
  (define-key lk201-function-map "\e[14~" [f4])
  (define-key lk201-function-map "\e[15~" [f5])
  (define-key lk201-function-map "\e[17~" [f6])
  (define-key lk201-function-map "\e[18~" [f7])
  (define-key lk201-function-map "\e[19~" [f8])
  (define-key lk201-function-map "\e[20~" [f9])
  (define-key lk201-function-map "\e[21~" [f10])
  ;; Customarily F11 is used as the ESC key.
  ;; The file that includes this one, takes care of that.
  (define-key lk201-function-map "\e[23~" [f11])
  (define-key lk201-function-map "\e[24~" [f12])
  (define-key lk201-function-map "\e[25~" [f13])
  (define-key lk201-function-map "\e[26~" [f14])
  (define-key lk201-function-map "\e[28~" [help])
  (define-key lk201-function-map "\e[29~" [menu])
  (define-key lk201-function-map "\e[31~" [f17])
  (define-key lk201-function-map "\e[32~" [f18])
  (define-key lk201-function-map "\e[33~" [f19])
  (define-key lk201-function-map "\e[34~" [f20])

  ;; Termcap or terminfo should set these.
  ;; (define-key lk201-function-map "\eOA" [up])
  ;; (define-key lk201-function-map "\eOB" [down])
  ;; (define-key lk201-function-map "\eOC" [right])
  ;; (define-key lk201-function-map "\eOD" [left])

  ;; Termcap or terminfo should set these, but doesn't properly.
  ;; Termcap sets these to k1-k4, which get mapped to f1-f4 in term.c
  (define-key lk201-function-map "\eOP" [kp-f1])
  (define-key lk201-function-map "\eOQ" [kp-f2])
  (define-key lk201-function-map "\eOR" [kp-f3])
  (define-key lk201-function-map "\eOS" [kp-f4])

  (define-key lk201-function-map "\eOI" [kp-tab])
  (define-key lk201-function-map "\eOj" [kp-multiply])
  (define-key lk201-function-map "\eOk" [kp-add])
  (define-key lk201-function-map "\eOl" [kp-separator])
  (define-key lk201-function-map "\eOM" [kp-enter])
  (define-key lk201-function-map "\eOm" [kp-subtract])
  (define-key lk201-function-map "\eOn" [kp-decimal])
  (define-key lk201-function-map "\eOo" [kp-divide])
  (define-key lk201-function-map "\eOp" [kp-0])
  (define-key lk201-function-map "\eOq" [kp-1])
  (define-key lk201-function-map "\eOr" [kp-2])
  (define-key lk201-function-map "\eOs" [kp-3])
  (define-key lk201-function-map "\eOt" [kp-4])
  (define-key lk201-function-map "\eOu" [kp-5])
  (define-key lk201-function-map "\eOv" [kp-6])
  (define-key lk201-function-map "\eOw" [kp-7])
  (define-key lk201-function-map "\eOx" [kp-8])
  (define-key lk201-function-map "\eOy" [kp-9]))

(defun terminal-init-lk201 ()
  ;; The terminal-local stuff only need to be set up on the first
  ;; frame on that device.
  (when (eq 1 (length (frames-on-display-list)))

    ;; Use inheritance to let the main keymap override these defaults.
    ;; This way we don't override terminfo-derived settings or settings
    ;; made in the .emacs file.
    (let ((m (copy-keymap lk201-function-map)))
      (set-keymap-parent m (keymap-parent (terminal-local-value 'local-function-key-map nil)))
      (set-keymap-parent (terminal-local-value 'local-function-key-map nil) m))))

;;; arch-tag: 7ffb4444-6a23-43e1-b457-43cf4f673c0d
;;; lk201.el ends here
