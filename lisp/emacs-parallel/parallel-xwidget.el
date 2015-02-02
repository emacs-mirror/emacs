;;; parallel-xwidget.el ---

;; Copyright (C) 2013 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'parallel)
(require 'browse-url)

(defgroup parallel-xwidget nil
  "Browse the web in another emacs instance with XWidget."
  :group 'emacs)

(defvar parallel-xwidget--task nil)

(defcustom parallel-xwidget-config nil
  "Parallel configuration."
  :type 'alist
  :group 'parallel-xwidget)

(defun parallel-xwidget--init ()
  (setq parallel-xwidget--task
        (parallel-start (lambda ()
                          (require 'xwidget))
                        :graphical t
                        :continue-when-executed t
                        :config parallel-xwidget-config)))

(defun parallel-xwidget-browse-url (url &optional new-session)
  "Browse URL in another Emacs instance."
  (interactive (browse-url-interactive-arg "xwidget-webkit URL: "))
  (unless (and parallel-xwidget--task
               (eq 'run (parallel-status parallel-xwidget--task)))
    (parallel-xwidget--init))
  (parallel-send parallel-xwidget--task
                 (lambda (url new-session)
                   (xwidget-webkit-browse-url url new-session))
                 (url-tidy url) new-session))

(provide 'parallel-xwidget)

;;; parallel-xwidget.el ends here
