;;; tests/icalendar-ast.el --- Tests for icalendar-ast  -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Free Software Foundation, Inc.

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

;;; Code:
(require 'icalendar-ast)
(require 'icalendar-parser)
(require 'cl-lib)
(eval-when-compile (require 'icalendar-macs))


;; Tests for the high-level construction macros:
(ert-deftest iat:make-param/nonlist ()
  "Test that `icalendar-make-param' works as documented with a single value."
  (let ((cnparam-node (ical:make-param ical:cnparam "John Doe")))
    (should (ical:param-node-p cnparam-node))
    (should (eq 'ical:cnparam (ical:ast-node-type cnparam-node)))
    (ical:with-param cnparam-node
       (should (cl-typep value 'ical:text))
       (should (equal value "John Doe")))))

(ert-deftest iat:make-param/list ()
  "Test that `icalendar-make-param' works as documented with a list of values."
  (let ((deltoparam-node (ical:make-param ical:deltoparam
                                          (list "mailto:minionA@example.com"
                                                "mailto:minionB@example.com"))))
    (should (ical:param-node-p deltoparam-node))
    (should (eq 'ical:deltoparam (ical:ast-node-type deltoparam-node)))
    (ical:with-param deltoparam-node
       (should (and (listp value-nodes) (length= value-nodes 2)))
       (should (seq-every-p
                (lambda (n) (eq 'ical:cal-address (ical:ast-node-type n)))
                value-nodes))
       (should (equal "mailto:minionA@example.com" (car values)))
       (should (equal "mailto:minionB@example.com" (cadr values))))))

(ert-deftest iat:make-property/nonlist ()
  "Test that `icalendar-make-property' works as documented with a single value."
  (let ((attendee-node
         (ical:make-property ical:attendee "mailto:hermes@planetexpress.com"
                             (ical:cnparam "H. Conrad"))))
    (should (ical:property-node-p attendee-node))
    (should (eq 'ical:attendee (ical:ast-node-type attendee-node)))
    (ical:with-property attendee-node
         ((ical:cnparam :first cnparam-node :value cn))
       (should (eq value-type 'ical:cal-address))
       (should (equal value "mailto:hermes@planetexpress.com"))
       (should (eq 'ical:cnparam (ical:ast-node-type cnparam-node)))
       (should (equal cn "H. Conrad")))))

(ert-deftest iat:make-property/list ()
  "Test that `icalendar-make-property' works as documented with a list of values."
  (let ((rdate-node (icalendar-make-property icalendar-rdate
                                             (list '(2 1 2025) '(3 1 2025)))))
    (should (ical:property-node-p rdate-node))
    (should (eq 'ical:rdate (ical:ast-node-type rdate-node)))
    (ical:with-property rdate-node
         ((ical:valuetypeparam :first valtype-node :value valtype))
       (should (and (listp value-nodes) (length= value-nodes 2)))
       (should (seq-every-p
                (lambda (n) (eq 'ical:date (ical:ast-node-type n)))
                value-nodes))
       (should (equal '(2 1 2025) (car values)))
       (should (equal '(3 1 2025) (cadr values)))
       (should (ical:ast-node-p valtype-node))
       (should (eq 'ical:valuetypeparam (ical:ast-node-type valtype-node)))
       (should (eq 'ical:date valtype)))))

(ert-deftest iat:make-component ()
  "Test that `icalendar-make-component' works as documented."
  (let* ((others (list (icalendar-make-property ical:dtstart '(9 6 3003))
                       (icalendar-make-property ical:rrule '((FREQ DAILY)))))
         (vevent-node (icalendar-make-component ical:vevent
                                                (ical:summary "Party")
                                                (ical:location "Robot House")
                                                (@ others))))
    (should (ical:component-node-p vevent-node))
    (should (eq 'ical:vevent (ical:ast-node-type vevent-node)))
    (ical:with-component vevent-node
         ((ical:uid :first uid-node)
          (ical:dtstamp :first dtstamp-node)
          (ical:summary :value summary)
          (ical:location :value location)
          (ical:dtstart :first dtstart-node :value dtstart)
          (ical:rrule :first rrule-node :value rrule))
       (should (and (ical:ast-node-p uid-node)
                    (ical:ast-node-p dtstamp-node)))
       (should (equal summary "Party"))
       (should (equal location "Robot House"))
       (should (equal dtstart '(9 6 3003)))
       (should (equal rrule '((FREQ DAILY)))))))

;; TODO: properties, components too

;; Local Variables:
;; read-symbol-shorthands: (("iat:" . "icalendar-ast-test-") ("ical:" . "icalendar-"))
;; End:
;;; icalendar-ast-tests.el ends here
