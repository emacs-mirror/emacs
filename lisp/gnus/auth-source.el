;;; auth-source.el --- authentication sources for Gnus and Emacs

;; Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is the auth-source.el package.  It lets users tell Gnus how to
;; authenticate in a single place.  Simplicity is the goal.  Instead
;; of providing 5000 options, we'll stick to simple, easy to
;; understand options.
;;; Code:

(eval-when-compile (require 'cl))

(defgroup auth-source nil
  "Authentication sources."
  :version "22.1"
  :group 'gnus)

(defcustom auth-source-choices nil
  "List of authentication sources.

Each entry is the authentication type with optional properties."
  :group 'auth-source
  :type '(repeat :tag "Authentication Sources"
		 (cons :tag "Source definition"
		       (group :tag "Select a source" :inline t
			      (const :format "" :value :source)
			      (choice :tag "Authentication information"
				      (const :tag "None" nil)
				      (file :tag "File")))
		       (checklist :tag "Options" :greedy t
				  (group :inline t
					 (choice :tag "Choose the hosts"
					  (group :tag "Select host by name" :inline t
						 (const :format "" :value :host)
						 (string :tag "Host name"))
					  (group :tag "Select host by regular expression" :inline t
						 (const :format "" :value :host-regex)
						 (regexp :tag "Host regular expression"))
					  (group :tag "Use any host" :inline t
						 (const :format "" :value :host-any)
						 (const :tag "Any" t))
					  (group :tag "Use if no other host matches" :inline t
						 (const :tag "Fallback" nil))))
				  (group :tag "Choose the protocol" :inline t
					 (const :format "" :value :protocol)
					 (choice :tag "Protocol"
						 (const :tag "Any" t)
						 (const :tag "Fallback (used if no others match)" nil)
						 (const :tag "IMAP" imap)
						 (const :tag "POP3" pop3)
						 (const :tag "SSH"  ssh)
						 (const :tag "SFTP" sftp)
						 (const :tag "SMTP" smtp)))))))

;; temp for debugging
;; (customize-variable 'auth-source-choices)
;; (setq auth-source-choices nil)
;; (format "%S" auth-source-choices)

(provide 'auth-source)

;; arch-tag: ff1afe78-06e9-42c2-b693-e9f922cbe4ab
;;; auth-source.el ends here
