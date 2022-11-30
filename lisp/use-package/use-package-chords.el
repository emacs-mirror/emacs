;;; use-package-chords.el --- key-chord keyword for use-package  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022 Free Software Foundation, Inc.

;; Author: Justin Talbott <justin@waymondo.com>
;; Keywords: convenience, tools, extensions
;; URL: https://github.com/jwiegley/use-package
;; Version: 0.2.1
;; Package-Requires: ((use-package "2.1") (bind-key "1.0") (bind-chord "0.2") (key-chord "0.6"))
;; Filename: use-package-chords.el

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

;;; Commentary:

;; The `:chords' keyword allows you to define `key-chord' bindings for
;; `use-package' declarations in the same manner as the `:bind'
;; keyword.

;;; Code:

(require 'use-package)
(require 'bind-chord)

;;;###autoload
(defalias 'use-package-autoloads/:chords 'use-package-autoloads-mode)

;;;###autoload
(defalias 'use-package-normalize/:chords 'use-package-normalize-binder)

;;;###autoload
(defun use-package-handler/:chords (name _keyword arg rest state)
  "Handler for `:chords' keyword in `use-package'."
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,(macroexpand
       `(bind-chords :package ,name ,@arg)))))

(add-to-list 'use-package-keywords :chords)

(provide 'use-package-chords)

;;; use-package-chords.el ends here
