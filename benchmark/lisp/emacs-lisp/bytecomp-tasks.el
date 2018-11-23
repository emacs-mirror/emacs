 ;;; bytecomp-tasks.el --- Byte Compilation -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Gemini Lasswell <gazally@runbox.com>

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

;;; Code:

(require 'erb-task)
(require 'bytecomp)

(defvar bytecomp-tasks-doctor-doc
  '(defun doctor-doc ()
     (cond
      ((equal doctor-sent '(foo))
       (doctor-type '(bar! (doc$ doctor--please) (doc$ doctor--continue) \.)))
      ((member doctor-sent doctor--howareyoulst)
       (doctor-type '(i\'m ok \.  (doc$ doctor--describe) yourself \.)))
      ((or (member doctor-sent '((good bye) (see you later) (i quit) (so long)
                                 (go away) (get lost)))
           (memq (car doctor-sent)
                 '(bye halt break quit done exit goodbye
                       bye\, stop pause goodbye\, stop pause)))
       (doctor-type (doc$ doctor--bye)))
      ((and (eq (car doctor-sent) 'you)
            (memq (cadr doctor-sent) doctor--abusewords))
       (setq doctor-found (cadr doctor-sent))
       (doctor-type (doc$ doctor--abuselst)))
      ((eq (car doctor-sent) 'whatmeans)
       (doctor-def (cadr doctor-sent)))
      ((equal doctor-sent '(parse))
       (doctor-type (list  'subj '= doctor-subj ",  "
                           'verb '= doctor-verb "\n"
                           'object 'phrase '= doctor-obj ","
                           'noun 'form '=  doctor-object "\n"
                           'current 'keyword 'is doctor-found
                           ", "
                           'most 'recent 'possessive
                           'is doctor-owner "\n"
                           'sentence 'used 'was
                           "..."
                           '(doc// doctor--bak))))
      ((memq (car doctor-sent) '(are is do has have how when where who why))
       (doctor-type (doc$ doctor--qlist)))
      ;;   ((eq (car sent) 'forget)
      ;;    (set (cadr sent) nil)
      ;;    (doctor-type '((doc$ doctor--isee) (doc$ doctor--please)
      ;;     (doc$ doctor--continue)\.)))
      (t
       (if (doctor-defq doctor-sent) (doctor-define doctor-sent doctor-found))
       (if (> (length doctor-sent) 12)
           (setq doctor-sent (doctor-shorten doctor-sent)))
       (setq doctor-sent (doctor-correct-spelling
                          (doctor-replace doctor-sent doctor--replist)))
       (cond ((and (not (memq 'me doctor-sent)) (not (memq 'i doctor-sent))
                   (memq 'am doctor-sent))
              (setq doctor-sent (doctor-replace doctor-sent '((am . (are)))))))
       (cond ((equal (car doctor-sent) 'yow) (doctor-zippy))
             ((< (length doctor-sent) 2)
              (cond ((eq (doctor-meaning (car doctor-sent)) 'howdy)
                     (doctor-howdy))
                    (t (doctor-short))))
             (t
              (if (memq 'am doctor-sent)
                  (setq doctor-sent (doctor-replace doctor-sent '((me . (i))))))
              (setq doctor-sent (doctor-fixup doctor-sent))
              (if (and (eq (car doctor-sent) 'do) (eq (cadr doctor-sent) 'not))
                  (cond ((zerop (random 3))
                         (doctor-type '(are you (doc$ doctor--afraidof) that \?)))
                        ((zerop (random 2))
                         (doctor-type '(don\'t tell me what to do \. i am the
                                               doctor here!))
                         (doctor-rthing))
                        (t
                         (doctor-type '((doc$ doctor--whysay) that i shouldn\'t
                                        (cddr doctor-sent)
                                        \?))))
                (doctor-go (doctor-wherego doctor-sent)))))))))

(erb-deftask bytecomp-tasks-compile-doc ()
  "Byte compile a function."
  (:version "1.0" :discard-first-sample t)
  (let ((byte-compile-warnings nil))
    (erb-task-time (byte-compile bytecomp-tasks-doctor-doc))))

(provide 'bytecomp-tasks)
;;; bytecomp-tasks.el ends here
