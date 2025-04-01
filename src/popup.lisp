(in-package :lem-core)

(defparameter *default-popup-message-timeout* 5)

(defvar lem-core/popup-message-interface:*popup-messenger*)

(defgeneric lem-core/popup-message-interface:display-popup-message
    (popup-messenger
     buffer-or-string
     &key timeout
          destination-window
          source-window
          style))

(defgeneric lem-core/popup-message-interface:delete-popup-message (popup-messenger popup-message))

(defun display-popup-message (buffer-or-string
                              &key (timeout *default-popup-message-timeout*)
                                   destination-window
                                   source-window
                                   style)
  (lem-core/popup-message-interface:display-popup-message
   lem-core/popup-message-interface:*popup-messenger*
   buffer-or-string
   :timeout timeout
   :destination-window destination-window
   :source-window source-window
   :style style))

(defun delete-popup-message (popup-message)
  (lem-core/popup-message-interface:delete-popup-message
   lem-core/popup-message-interface:*popup-messenger*
   popup-message))

(defun display-popup-menu (items
                           &rest args
                           &key action-callback
                                print-spec
                                style
                                max-display-items)
  (declare (ignore action-callback print-spec style max-display-items))
  (apply #'lem-if:display-popup-menu (implementation)
         items
         args))

(defun popup-menu-update (popup-menu items &rest args &key print-spec max-display-items keep-focus)
  (declare (ignore print-spec max-display-items keep-focus))
  (apply #'lem-if:popup-menu-update (implementation) popup-menu items args))

(defun popup-menu-quit (popup-menu)
  (lem-if:popup-menu-quit (implementation) popup-menu))

(defun popup-menu-down (popup-menu)
  (lem-if:popup-menu-down (implementation) popup-menu))

(defun popup-menu-up (popup-menu)
  (lem-if:popup-menu-up (implementation) popup-menu))

(defun popup-menu-first (popup-menu)
  (lem-if:popup-menu-first (implementation) popup-menu))

(defun popup-menu-last (popup-menu)
  (lem-if:popup-menu-last (implementation) popup-menu))

(defun popup-menu-select (popup-menu)
  (lem-if:popup-menu-select (implementation) popup-menu))
