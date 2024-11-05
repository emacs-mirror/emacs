;;; use-package.el --- A configuration macro for simplifying your .emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 17 Jun 2012
;; Version: 2.4.6
;; Package-Requires: ((emacs "24.3") (bind-key "2.4"))
;; Keywords: dotemacs startup speed config package extensions
;; URL: https://github.com/jwiegley/use-package

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

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

;; The `use-package' declaration macro allows you to isolate package
;; configuration in your init file in a way that is
;; performance-oriented and, well, just tidy.  I created it because I
;; have over 80 packages that I use in Emacs, and things were getting
;; difficult to manage.  Yet with this utility my total load time is
;; just under 1 second, with no loss of functionality!
;;
;; See the `use-package' info manual for more information.

;;; Code:

(require 'use-package-core)

(require 'use-package-bind-key)
(require 'use-package-diminish)
(require 'use-package-delight)
(require 'use-package-ensure)

(declare-function use-package-jump-to-package-form "use-package-jump")
(autoload #'use-package-jump-to-package-form "use-package-jump" nil t)

(provide 'use-package)

;;; use-package.el ends here
