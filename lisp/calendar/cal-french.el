;;; cal-french.el --- calendar functions for the French Revolutionary calendar  -*- lexical-binding: t; -*-

;; Copyright (C) 1988-1989, 1992, 1994-1995, 1997, 2001-2022 Free
;; Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: calendar
;; Human-Keywords: French Revolutionary calendar, calendar, diary
;; Package: calendar

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

;; See calendar.el.

;;; Code:

(require 'calendar)

(defconst calendar-french-epoch (calendar-absolute-from-gregorian '(9 22 1792))
  "Absolute date of start of French Revolutionary calendar = Sept 22, 1792.")

(define-obsolete-variable-alias 'calendar-french-multibyte-month-name-array
  'calendar-french-month-name-array "28.1")

(defconst calendar-french-month-name-array
  ["Vendémiaire" "Brumaire" "Frimaire" "Nivôse" "Pluviôse" "Ventôse"
   "Germinal" "Floréal" "Prairial" "Messidor" "Thermidor" "Fructidor"
   "jour complémentaire"]
  "Array of month names in the French calendar.")

(defconst calendar-french-day-name-array
  ["Primidi" "Duodi" "Tridi" "Quartidi" "Quintidi" "Sextidi" "Septidi"
   "Octidi" "Nonidi" "Décadi"]
  "Array of day names in the French calendar.")

(define-obsolete-variable-alias 'calendar-french-multibyte-special-days-array
  'calendar-french-special-days-array "28.1")

(defconst calendar-french-special-days-array
  ["de la Vertu" "du Génie" "du Travail" "de la Raison" "des Récompenses"
   "de la Révolution"]
  "Array of special day names in the French calendar.")

(defconst calendar-french-feasts-array
  [;; Vendémiaire
   "du Raisin"             "du Safran"             "de la Châtaigne"
   "de la Colchique"       "du Cheval"             "de la Balsamine"
   "de la Carotte"         "de l'Amarante"         "du Panais"
   "de la Cuve"            "de la Pomme de terre"  "de l'Immortelle"
   "du Potiron"            "du Réséda"             "de l'Âne"
   "de la Belle de nuit"   "de la Citrouille"      "du Sarrasin"
   "du Tournesol"          "du Pressoir"           "du Chanvre"
   "de la Pêche"           "du Navet"              "de l'Amaryllis"
   "du Bœuf"               "de l'Aubergine"        "du Piment"
   "de la Tomate"          "de l'Orge"             "du Tonneau"
   ;; Brumaire
   "de la Pomme"           "du Céleri"             "de la Poire"
   "de la Betterave"       "de l'Oie"              "de l'Héliotrope"
   "de la Figue"           "de la Scorsonère"      "de l'Alisier"
   "de la Charrue"         "du Salsifis"           "de la Macre"
   "du Topinambour"        "de l'Endive"           "du Dindon"
   "du Chervis"            "du Cresson"            "de la Dentelaire"
   "de la Grenade"         "de la Herse"           "de la Bacchante"
   "de l'Azerole"          "de la Garance"         "de l'Orange"
   "du Faisan"             "de la Pistache"        "du Macjon"
   "du Coing"              "du Cormier"            "du Rouleau"
   ;; Frimaire
   "de la Raiponce"        "du Turneps"            "de la Chicorée"
   "de la Nèfle"           "du Cochon"             "de la Mâche"
   "du Chou-fleur"         "du Miel"               "du Genièvre"
   "de la Pioche"          "de la Cire"            "du Raifort"
   "du Cèdre"              "du Sapin"              "du Chevreuil"
   "de l'Ajonc"            "du Cyprès"             "du Lierre"
   "de la Sabine"          "du Hoyau"              "de l'Érable-sucre"
   "de la Bruyère"         "du Roseau"             "de l'Oseille"
   "du Grillon"            "du Pignon"             "du Liège"
   "de la Truffe"          "de l'Olive"            "de la Pelle"
   ;; Nivôse
   "de la Tourbe"          "de la Houille"         "du Bitume"
   "du Soufre"             "du Chien"              "de la Lave"
   "de la Terre végétale"  "du Fumier"             "du Salpêtre"
   "du Fléau"              "du Granit"             "de l'Argile"
   "de l'Ardoise"          "du Grès"               "du Lapin"
   "du Silex"              "de la Marne"           "de la Pierre à chaux"
   "du Marbre"             "du Van"                "de la Pierre à plâtre"
   "du Sel"                "du Fer"                "du Cuivre"
   "du Chat"               "de l'Étain"            "du Plomb"
   "du Zinc"               "du Mercure"            "du Crible"
   ;; Pluviôse
   "de la Lauréole"        "de la Mousse"          "du Fragon"
   "du Perce-neige"        "du Taureau"            "du Laurier-thym"
   "de l'Amadouvier"       "du Mézéréon"           "du Peuplier"
   "de la Cognée"          "de l'Ellébore"         "du Brocoli"
   "du Laurier"            "de l'Avelinier"        "de la Vache"
   "du Buis"               "du Lichen"             "de l'If"
   "de la Pulmonaire"      "de la Serpette"        "du Thlaspi"
   "du Thymelé"            "du Chiendent"          "de la Traînasse"
   "du Lièvre"             "de la Guède"           "du Noisetier"
   "du Cyclamen"           "de la Chélidoine"      "du Traîneau"
   ;; Ventôse
   "du Tussilage"          "du Cornouiller"        "du Violier"
   "du Troène"             "du Bouc"               "de l'Asaret"
   "de l'Alaterne"         "de la Violette"        "du Marsault"
   "de la Bêche"           "du Narcisse"           "de l'Orme"
   "de la Fumeterre"       "du Vélar"              "de la Chèvre"
   "de l'Épinard"          "du Doronic"            "du Mouron"
   "du Cerfeuil"           "du Cordeau"            "de la Mandragore"
   "du Persil"             "du Cochléaria"         "de la Pâquerette"
   "du Thon"               "du Pissenlit"          "de la Sylvie"
   "du Capillaire"         "du Frêne"              "du Plantoir"
   ;; Germinal
   "de la Primevère"       "du Platane"            "de l'Asperge"
   "de la Tulipe"          "de la Poule"           "de la Blette"
   "du Bouleau"            "de la Jonquille"       "de l'Aulne"
   "du Couvoir"            "de la Pervenche"       "du Charme"
   "de la Morille"         "du Hêtre"              "de l'Abeille"
   "de la Laitue"          "du Mélèze"             "de la Ciguë"
   "du Radis"              "de la Ruche"           "du Gainier"
   "de la Romaine"         "du Marronnier"         "de la Roquette"
   "du Pigeon"             "du Lilas"              "de l'Anémone"
   "de la Pensée"          "de la Myrtille"        "du Greffoir"
   ;; Floréal
   "de la Rose"            "du Chêne"              "de la Fougère"
   "de l'Aubépine"         "du Rossignol"          "de l'Ancolie"
   "du Muguet"             "du Champignon"         "de la Jacinthe"
   "du Rateau"             "de la Rhubarbe"        "du Sainfoin"
   "du Bâton-d'or"         "du Chamérisier"        "du Ver à soie"
   "de la Consoude"        "de la Pimprenelle"     "de la Corbeille-d'or"
   "de l'Arroche"          "du Sarcloir"           "du Statice"
   "de la Fritillaire"     "de la Bourrache"       "de la Valériane"
   "de la Carpe"           "du Fusain"             "de la Civette"
   "de la Buglosse"        "du Sénevé"             "de la Houlette"
   ;; Prairial
   "de la Luzerne"         "de l'Hémérocalle"      "du Trèfle"
   "de l'Angélique"        "du Canard"             "de la Mélisse"
   "du Fromental"          "du Martagon"           "du Serpolet"
   "de la Faux"            "de la Fraise"          "de la Bétoine"
   "du Pois"               "de l'Acacia"           "de la Caille"
   "de l'Œillet"           "du Sureau"             "du Pavot"
   "du Tilleul"            "de la Fourche"         "du Barbeau"
   "de la Camomille"       "du Chèvrefeuille"      "du Caille-lait"
   "de la Tanche"          "du Jasmin"             "de la Verveine"
   "du Thym"               "de la Pivoine"         "du Chariot"
   ;; Messidor
   "du Seigle"             "de l'Avoine"           "de l'Oignon"
   "de la Véronique"       "du Mulet"              "du Romarin"
   "du Concombre"          "de l'Échalotte"        "de l'Absinthe"
   "de la Faucille"        "de la Coriandre"       "de l'Artichaut"
   "de la Giroflée"        "de la Lavande"         "du Chamois"
   "du Tabac"              "de la Groseille"       "de la Gesse"
   "de la Cerise"          "du Parc"               "de la Menthe"
   "du Cumin"              "du Haricot"            "de l'Orcanète"
   "de la Pintade"         "de la Sauge"           "de l'Ail"
   "de la Vesce"           "du Blé"                "de la Chalémie"
   ;; Thermidor
   "de l'Épautre"          "du Bouillon-blanc"     "du Melon"
   "de l'Ivraie"           "du Bélier"             "de la Prèle"
   "de l'Armoise"          "du Carthame"           "de la Mûre"
   "de l'Arrosoir"         "du Panis"              "du Salicor"
   "de l'Abricot"          "du Basilic"            "de la Brebis"
   "de la Guimauve"        "du Lin"                "de l'Amande"
   "de la Gentiane"        "de l'Écluse"           "de la Carline"
   "du Câprier"            "de la Lentille"        "de l'Aunée"
   "de la Loutre"          "de la Myrte"           "du Colza"
   "du Lupin"              "du Coton"              "du Moulin"
   ;; Fructidor
   "de la Prune"           "du Millet"             "du Lycoperdon"
   "de l'Escourgeon"       "du Saumon"             "de la Tubéreuse"
   "du Sucrion"            "de l'Apocyn"           "de la Réglisse"
   "de l'Échelle"          "de la Pastèque"        "du Fenouil"
   "de l'Épine-vinette"    "de la Noix"            "de la Truite"
   "du Citron"             "de la Cardère"         "du Nerprun"
   "du Tagette"            "de la Hotte"           "de l'Églantier"
   "de la Noisette"        "du Houblon"            "du Sorgho"
   "de l'Écrevisse"        "de la Bagarade"        "de la Verge-d'or"
   "du Maïs"               "du Marron"             "du Panier"
   ;; jour complémentaire
   "de la Vertu"           "du Génie"              "du Travail"
   "de la Raison"          "des Récompenses"       "de la Révolution"]
  "Array of day feasts in the French calendar.")

(defun calendar-french-accents-p ()
  (declare (obsolete nil "28.1"))
  t)

(defun calendar-french-month-name-array ()
  "Return the array of month names, depending on whether accents are available."
  (declare (obsolete "use the variable of the same name instead" "28.1"))
  calendar-french-month-name-array)

(defun calendar-french-day-name-array ()
  "Return the array of day names."
  (declare (obsolete "use the variable of the same name instead" "28.1"))
  calendar-french-day-name-array)

(defun calendar-french-special-days-array ()
  "Return the special day names, depending on whether accents are available."
  (declare (obsolete "use the variable of the same name instead" "28.1"))
  calendar-french-special-days-array)

(defun calendar-french-trim-feast (feast)
  "Remove the article from the FEAST.
E.g. \"du Raisin\" -> \"Raisin\" or \"de la Vertu\" -> \"Vertu\"."
  (cond
   ((equal (substring feast 0 3) "du ")    (substring feast 3))
   ((equal (substring feast 0 6) "de la ") (substring feast 6))
   ((equal (substring feast 0 5) "de l'")  (substring feast 5))
   ((equal (substring feast 0 4) "des ")   (substring feast 4))
   (t feast)))

(defun calendar-french-leap-year-p (year)
  "True if YEAR is a leap year on the French Revolutionary calendar.
For Gregorian years 1793 to 1805, the years of actual operation of the
calendar, follows historical practice based on equinoxes (years 3, 7,
and 11 were leap years; 15 and 20 would have been leap years).  For later
years uses the proposed rule of Romme (never adopted)--leap years fall every
four years except century years not divisible 400 and century years that are
multiples of 4000."
  (or (memq year '(3 7 11)) ; actual practice--based on equinoxes
      (memq year '(15 20))  ; anticipated practice--based on equinoxes
      (and (> year 20)      ; Romme's proposal--never adopted
           (zerop (% year 4))
           (not (memq (% year 400) '(100 200 300)))
           (not (zerop (% year 4000))))))

(defun calendar-french-last-day-of-month (month year)
  "Return last day of MONTH, YEAR on the French Revolutionary calendar.
The 13th month is not really a month, but the 5 (6 in leap years) day period of
`sansculottides' at the end of the year."
  (if (< month 13)
      30
    (if (calendar-french-leap-year-p year)
        6
      5)))

(defun calendar-french-to-absolute (date)
  "Compute absolute date from French Revolutionary date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((month (calendar-extract-month date))
        (day (calendar-extract-day date))
        (year (calendar-extract-year date)))
    (+ (* 365 (1- year))                ; days in prior years
       ;; Leap days in prior years.
       (if (< year 20)
           (/ year 4) ; actual and anticipated practice (years 3, 7, 11, 15)
         ;; Romme's proposed rule (using the Principle of Inclusion/Exclusion).
         (+ (/ (1- year) 4) ; luckily, there were 4 leap years before year 20
            (- (/ (1- year) 100))
            (/ (1- year) 400)
            (- (/ (1- year) 4000))))
       (* 30 (1- month))              ; days in prior months this year
       day                            ; days so far this month
       (1- calendar-french-epoch))))  ; days before start of calendar

(defun calendar-french-from-absolute (date)
  "Compute the French Revolutionary equivalent for absolute date DATE.
The result is a list of the form (MONTH DAY YEAR).
The absolute date is the number of days elapsed since the
\(imaginary) Gregorian date Sunday, December 31, 1 BC."
  (if (< date calendar-french-epoch)
      (list 0 0 0)                     ; pre-French Revolutionary date
    (let* ((approx                     ; approximation from below
            (/ (- date calendar-french-epoch) 366))
           (year               ; search forward from the approximation
            (+ approx
               (calendar-sum y approx
                             (>= date (calendar-french-to-absolute
                                       (list 1 1 (1+ y))))
                             1)))
           (month                    ; search forward from Vendemiaire
            (1+ (calendar-sum m 1
                              (> date
                                 (calendar-french-to-absolute
                                  (list m
                                        (calendar-french-last-day-of-month
                                         m year)
                                        year)))
                              1)))
           (day                     ; calculate the day by subtraction
            (- date
               (1- (calendar-french-to-absolute (list month 1 year))))))
      (list month day year))))

;;;###cal-autoload
(defun calendar-french-date-string (&optional date)
  "String of French Revolutionary date of Gregorian DATE.
Returns the empty string if DATE is pre-French Revolutionary.
Defaults to today's date if DATE is not given."
  (let* ((french-date (calendar-french-from-absolute
                       (calendar-absolute-from-gregorian
                        (or date (calendar-current-date)))))
         (y (calendar-extract-year french-date))
         (m (calendar-extract-month french-date))
         (d (calendar-extract-day french-date)))
    (cond
     ((< y 1) "")
     (t (format
         "%s %d %s an %d de la Révolution, jour %s"
         (aref calendar-french-day-name-array (% (1- d) 10))
         d
         (aref calendar-french-month-name-array (1- m))
         y
         (aref calendar-french-feasts-array (+ -31 (* 30 m) d)))))))

;;;###cal-autoload
(defun calendar-french-print-date ()
  "Show the French Revolutionary calendar equivalent of the selected date."
  (interactive)
  (let ((f (calendar-french-date-string (calendar-cursor-to-date t))))
    (if (string-equal f "")
        (message "Date is pre-French Revolution")
      (message "French Revolutionary date: %s" f))))

;;;###cal-autoload
(defun calendar-french-goto-date (date &optional noecho)
  "Move cursor to French Revolutionary date DATE.
Echo French Revolutionary date unless NOECHO is non-nil."
  (interactive
   (let* ((months calendar-french-month-name-array)
          (feasts calendar-french-feasts-array)
          (year (progn
                  (calendar-read-sexp
                   "Année de la Révolution (>0)"
                   (lambda (x) (> x 0))
                   (calendar-extract-year
                    (calendar-french-from-absolute
                     (calendar-absolute-from-gregorian
                      (calendar-current-date)))))))
          (month-list
           (mapcar 'list
                   (append months
                           (if (calendar-french-leap-year-p year)
                               (mapcar #'calendar-french-trim-feast feasts)
                             (reverse
                              (cdr ; we don't want rev. day in a non-leap yr
                               (reverse
                                (mapcar #'calendar-french-trim-feast
                                        feasts))))))))
          (completion-ignore-case t)
          (month (cdr (assoc-string
                       (completing-read
                        "Mois ou \"jour complémentaire\" ou fête: "
                        month-list
                        nil t)
                       (calendar-make-alist month-list 1 'car) t)))
          (last-day (calendar-french-last-day-of-month (min month 13) year))
          (day (if (> month 13)
                   (- month 13)
                 (calendar-read-sexp
                  (format "Jour (1-%d): " last-day)
                  (lambda (x) (<= 1 x last-day)))))
          ;; All days in Vendémiaire and numbered 1 to 365 e.g., "Pomme"
          ;; gives 31 Vendémiaire automatically normalized to 1 Brumaire
          ;; "Céleri" gives 32 Vnd normalized to 2 Bru, "Raiponce" gives
          ;; 61 Vnd normalized to 1 Frimaire, etc until "Récompences" which
          ;; gives 365 Vnd normalized to 5 jour complémentaire.
          (month (if (> month 13) 1 month)))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-french-to-absolute date)))
  (or noecho (calendar-french-print-date)))

;; The function below is designed to be used in sexp diary entries,
;; and may be present in users' diary files, so suppress the warning
;; about this prefix-less dynamic variable.  It's called from
;; `diary-list-sexp-entries', which binds the variable.
(with-suppressed-warnings ((lexical date))
  (defvar date))

;;;###diary-autoload
(defun diary-french-date ()
  "French calendar equivalent of date diary entry."
  (let ((f (calendar-french-date-string date)))
    (if (string-equal f "")
        "Date is pre-French Revolution"
      (format "French Revolutionary date: %s" f))))

(provide 'cal-french)

;;; cal-french.el ends here
