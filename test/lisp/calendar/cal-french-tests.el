;;; cal-french-tests.el --- tests for cal-french.el  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

(require 'ert)
(require 'cal-french)

(defconst cal-french-test-cases
  '(
 (1792  9 22 "Primidi 1 Vendémiaire an 1 de la Révolution, jour du Raisin")
 (1793 10 23 "Duodi 2 Brumaire an 2 de la Révolution, jour du Céleri")
 (1794  7 27 "Nonidi 9 Thermidor an 2 de la Révolution, jour de la Mûre")
 (1794 11 23 "Tridi 3 Frimaire an 3 de la Révolution, jour de la Chicorée")
 (1795 10  5 "Tridi 13 Vendémiaire an 4 de la Révolution, jour du Potiron")
 (1795 12 25 "Quartidi 4 Nivôse an 4 de la Révolution, jour du Soufre")
 (1797  1 24 "Quintidi 5 Pluviôse an 5 de la Révolution, jour du Taureau")
 (1798  2 24 "Sextidi 6 Ventôse an 6 de la Révolution, jour de l'Asaret")
 (1799 11  9 "Octidi 18 Brumaire an 8 de la Révolution, jour de la Dentelaire")
 (1801  3 29 "Octidi 8 Germinal an 9 de la Révolution, jour de la Jonquille")
 (1804  4 30 "Décadi 10 Floréal an 12 de la Révolution, jour du Rateau")
 (1807  6  1 "Duodi 12 Prairial an 15 de la Révolution, jour de la Bétoine")
 (1810  7  3 "Quartidi 14 Messidor an 18 de la Révolution, jour de la Lavande")
 (1813  8  4 "Sextidi 16 Thermidor an 21 de la Révolution, jour de la Guimauve")
 (1816  9  4 "Octidi 18 Fructidor an 24 de la Révolution, jour du Nerprun")
 (2000  1  1 "Duodi 12 Nivôse an 208 de la Révolution, jour de l'Argile")
 (2021  7 11 "Tridi 23 Messidor an 229 de la Révolution, jour du Haricot")
 (2001  5 11 "Duodi 22 Floréal an 209 de la Révolution, jour de la Fritillaire")
 (1792  9 22 "Primidi 1 Vendémiaire an 1 de la Révolution, jour du Raisin")
 (1793  9 21 "Quintidi 5 jour complémentaire an 1 de la Révolution, jour des Récompenses")
 (1793  9 22 "Primidi 1 Vendémiaire an 2 de la Révolution, jour du Raisin")
 (1794  9 21 "Quintidi 5 jour complémentaire an 2 de la Révolution, jour des Récompenses")
 (1794  9 22 "Primidi 1 Vendémiaire an 3 de la Révolution, jour du Raisin")
 (1795  9 22 "Sextidi 6 jour complémentaire an 3 de la Révolution, jour de la Révolution")
 (1795  9 23 "Primidi 1 Vendémiaire an 4 de la Révolution, jour du Raisin")
 (1796  9 21 "Quintidi 5 jour complémentaire an 4 de la Révolution, jour des Récompenses")
 (1796  9 22 "Primidi 1 Vendémiaire an 5 de la Révolution, jour du Raisin")
 (1797  9 21 "Quintidi 5 jour complémentaire an 5 de la Révolution, jour des Récompenses")
 (1797  9 22 "Primidi 1 Vendémiaire an 6 de la Révolution, jour du Raisin")
 (1799  9 22 "Sextidi 6 jour complémentaire an 7 de la Révolution, jour de la Révolution")
 (1799  9 23 "Primidi 1 Vendémiaire an 8 de la Révolution, jour du Raisin")
 (1800  9 22 "Quintidi 5 jour complémentaire an 8 de la Révolution, jour des Récompenses")
 (1800  9 23 "Primidi 1 Vendémiaire an 9 de la Révolution, jour du Raisin")
 (1801  9 22 "Quintidi 5 jour complémentaire an 9 de la Révolution, jour des Récompenses")
 (1801  9 23 "Primidi 1 Vendémiaire an 10 de la Révolution, jour du Raisin")
 (1823  9 22 "Quintidi 5 jour complémentaire an 31 de la Révolution, jour des Récompenses")
 (1823  9 23 "Primidi 1 Vendémiaire an 32 de la Révolution, jour du Raisin")
 (1824  9 22 "Sextidi 6 jour complémentaire an 32 de la Révolution, jour de la Révolution")
 (1824  9 23 "Primidi 1 Vendémiaire an 33 de la Révolution, jour du Raisin")
 (1825  9 22 "Quintidi 5 jour complémentaire an 33 de la Révolution, jour des Récompenses")
 (1825  9 23 "Primidi 1 Vendémiaire an 34 de la Révolution, jour du Raisin")
 (1892  9 21 "Quintidi 5 jour complémentaire an 100 de la Révolution, jour des Récompenses")
 (1892  9 22 "Primidi 1 Vendémiaire an 101 de la Révolution, jour du Raisin")
 (1900  9 22 "Sextidi 6 jour complémentaire an 108 de la Révolution, jour de la Révolution")
 (1900  9 23 "Primidi 1 Vendémiaire an 109 de la Révolution, jour du Raisin")
 (1992  9 21 "Quintidi 5 jour complémentaire an 200 de la Révolution, jour des Récompenses")
 (1992  9 22 "Primidi 1 Vendémiaire an 201 de la Révolution, jour du Raisin")
 (2000  9 21 "Sextidi 6 jour complémentaire an 208 de la Révolution, jour de la Révolution")
 (2000  9 22 "Primidi 1 Vendémiaire an 209 de la Révolution, jour du Raisin")
 (2092  9 20 "Quintidi 5 jour complémentaire an 300 de la Révolution, jour des Récompenses")
 (2092  9 21 "Primidi 1 Vendémiaire an 301 de la Révolution, jour du Raisin")
 (2100  9 21 "Sextidi 6 jour complémentaire an 308 de la Révolution, jour de la Révolution")
 (2100  9 22 "Primidi 1 Vendémiaire an 309 de la Révolution, jour du Raisin")
 (2192  9 21 "Sextidi 6 jour complémentaire an 400 de la Révolution, jour de la Révolution")
 (2192  9 22 "Primidi 1 Vendémiaire an 401 de la Révolution, jour du Raisin")
 (2193  9 21 "Quintidi 5 jour complémentaire an 401 de la Révolution, jour des Récompenses")
 (2199  9 22 "Primidi 1 Vendémiaire an 408 de la Révolution, jour du Raisin")
 (2200  9 22 "Sextidi 6 jour complémentaire an 408 de la Révolution, jour de la Révolution")
 (2791  9 23 "Primidi 1 Vendémiaire an 1000 de la Révolution, jour du Raisin")
 (2792  9 22 "Primidi 1 Vendémiaire an 1001 de la Révolution, jour du Raisin")
 (3000  1  1 "Duodi 12 Nivôse an 1208 de la Révolution, jour de l'Argile")
 (3001  1  1 "Primidi 11 Nivôse an 1209 de la Révolution, jour du Granit")
 (3791  9 22 "Primidi 1 Vendémiaire an 2000 de la Révolution, jour du Raisin")
 (3792  9 22 "Primidi 1 Vendémiaire an 2001 de la Révolution, jour du Raisin")
 (4000  1  1 "Duodi 12 Nivôse an 2208 de la Révolution, jour de l'Argile")
 (4001  1  1 "Duodi 12 Nivôse an 2209 de la Révolution, jour de l'Argile")
 (4320  9 10 "Quartidi 24 Fructidor an 2528 de la Révolution, jour du Sorgho")
 (4320  9 11 "Quintidi 25 Fructidor an 2528 de la Révolution, jour de l'Écrevisse")
 (4791  9 23 "Primidi 1 Vendémiaire an 3000 de la Révolution, jour du Raisin")
 (4792  9 22 "Primidi 1 Vendémiaire an 3001 de la Révolution, jour du Raisin")
 (5000  1  1 "Duodi 12 Nivôse an 3208 de la Révolution, jour de l'Argile")
 (5001  1  1 "Primidi 11 Nivôse an 3209 de la Révolution, jour du Granit")
 (5791  9 22 "Primidi 1 Vendémiaire an 4000 de la Révolution, jour du Raisin")
 (5792  9 21 "Primidi 1 Vendémiaire an 4001 de la Révolution, jour du Raisin")
 (6000  1  1 "Tridi 13 Nivôse an 4208 de la Révolution, jour de l'Ardoise")
 (6001  1  1 "Tridi 13 Nivôse an 4209 de la Révolution, jour de l'Ardoise")
 (6791  9 22 "Primidi 1 Vendémiaire an 5000 de la Révolution, jour du Raisin")
 (6792  9 21 "Primidi 1 Vendémiaire an 5001 de la Révolution, jour du Raisin")
 (7791  9 21 "Primidi 1 Vendémiaire an 6000 de la Révolution, jour du Raisin")
 (7792  9 21 "Primidi 1 Vendémiaire an 6001 de la Révolution, jour du Raisin")
    ))

(ert-deftest cal-french-tests ()
  (pcase-dolist (`(,y ,m ,d ,str) cal-french-test-cases)
    (should (equal (calendar-french-date-string (list m d y)) str))))

(provide 'cal-french-tests)
;;; cal-french-tests.el ends here
