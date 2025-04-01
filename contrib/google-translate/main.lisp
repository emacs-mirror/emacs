(uiop/package:define-package :lem-google-translate/main (:use :cl :lem))
(in-package :lem-google-translate/main)
;;;don't edit above
(defvar lem-user::*google-api-key* nil)

(define-command popup-google-translate (start end) (:region)
  (display-popup-message 
   (translate-client:translate 
    (points-to-string start end)
    :target :ja
    :api-key lem-user::*google-api-key*)))