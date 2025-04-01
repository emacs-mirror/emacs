(defpackage :lem-tests/vi-mode
  (:use :cl
        :lem
        :testif)
  (:import-from :lem-fake-interface
                :fake-interface))
(in-package :lem-tests/vi-mode)

;;; このテストは不十分で動作も不安定なので、まだ動かさない

(test vi-mode
  (lem)
  (sleep 0.1)
  (lem-vi-mode:vi-mode)
  (test "enable hook"
    (test "initialize-vi-modeline"
      (ok (and (lem-vi-mode/core::vi-modeline-element-p
                lem-vi-mode/core::*modeline-element*)))
      (ok (find-if (lambda (element)
                     (and (lem-vi-mode/core::vi-modeline-element-p element)
                          (equal "[COMMAND]" (lem-vi-mode/core::element-name element))))
                   lem-core::*modeline-status-list*)))
    (test "(change-state 'normal)"
      (ok (eq lem-vi-mode/core::*current-state* 'lem-vi-mode::command))

      (ok (eq (mode-keymap (get 'lem-vi-mode:vi-mode 'lem-core::global-mode))
              (lem-vi-mode/core::vi-state-keymap (lem-vi-mode/core::ensure-state 'lem-vi-mode::command))))
      (ok (equal "[COMMAND]" (lem-vi-mode/core::element-name lem-vi-mode/core::*modeline-element*)))
      (ok (string= (if (eq :dark (lem-core::display-background-mode))
                       "white"
                       "dark")
                   (attribute-background (ensure-attribute 'cursor nil)))))))
