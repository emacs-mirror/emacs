;;; multisession.el --- Multisession storage for variables  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

;; This library provides multisession variables for Emacs Lisp, to
;; make them persist between sessions.
;;
;; Use `define-multisession-variable' to define a multisession
;; variable, and `multisession-value' to read its value.  Use
;; `list-multisession-values' to list multisession variables.
;;
;; Users might want to customize `multisession-storage' and
;; `multisession-directory'.
;;
;; See Info node `(elisp) Multisession Variables' for more
;; information.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'sqlite)
(require 'tabulated-list)

(defcustom multisession-storage 'files
  "Storage method for multisession variables.
Valid methods are `sqlite' and `files'."
  :type '(choice (const :tag "SQLite" sqlite)
                 (const :tag "Files" files))
  :version "29.1"
  :group 'files)

(defcustom multisession-directory (expand-file-name "multisession/"
                                                    user-emacs-directory)
  "Directory to store multisession variables."
  :type 'file
  :version "29.1"
  :group 'files)

;;;###autoload
(defmacro define-multisession-variable (name initial-value &optional doc
                                             &rest args)
  "Make NAME into a multisession variable initialized from INITIAL-VALUE.
DOC should be a doc string, and ARGS are keywords as applicable to
`make-multisession'."
  (declare (indent defun))
  (unless (plist-get args :package)
    (setq args (nconc (list :package
                            (replace-regexp-in-string "-.*" ""
                                                      (symbol-name name)))
                      args)))
  `(defvar ,name
     (make-multisession :key ,(symbol-name name)
                        :initial-value ,initial-value
                        ,@args)
     ,@(list doc)))

(defconst multisession--unbound (make-symbol "unbound"))

(cl-defstruct (multisession
               (:constructor nil)
               (:constructor multisession--create)
               (:conc-name multisession--))
  "A persistent variable that will live across Emacs invocations."
  key
  (initial-value nil)
  package
  (storage multisession-storage)
  (synchronized nil)
  (cached-value multisession--unbound)
  (cached-sequence 0))

(cl-defun make-multisession (&key key initial-value package synchronized
                                  storage)
  "Create a multisession object."
  (unless package
    (error "No package for the multisession object"))
  (unless key
    (error "No key for the multisession object"))
  (unless (stringp package)
    (error "The package has to be a string"))
  (unless (stringp key)
    (error "The key has to be a string"))
  (multisession--create
   :key key
   :synchronized synchronized
   :initial-value initial-value
   :package package
   :storage (or storage multisession-storage)))

(defun multisession-value (object)
  "Return the value of the multisession OBJECT."
  (if (null user-init-file)
      ;; If we don't have storage, then just return the value from the
      ;; object.
      (if (eq (multisession--cached-value object) multisession--unbound)
          (multisession--initial-value object)
        (multisession--cached-value object))
    ;; We have storage, so we update from storage.
    (multisession-backend-value (multisession--storage object) object)))

(defun multisession--set-value (object value)
  "Set the stored value of OBJECT to VALUE."
  (if (null user-init-file)
      ;; We have no backend, so just store the value.
      (setf (multisession--cached-value object) value)
    ;; We have a backend.
    (multisession--backend-set-value (multisession--storage object)
                                     object value)))

(defun multisession-delete (object)
  "Delete OBJECT from the backend storage."
  (multisession--backend-delete (multisession--storage object) object))

(gv-define-simple-setter multisession-value multisession--set-value)

;; SQLite Backend

(declare-function sqlite-execute "sqlite.c")
(declare-function sqlite-select "sqlite.c")
(declare-function sqlite-open "sqlite.c")
(declare-function sqlite-pragma "sqlite.c")

(defvar multisession--db nil)

(defun multisession--ensure-db ()
  (unless multisession--db
    (let* ((file (expand-file-name "sqlite/multisession.sqlite"
                                   multisession-directory))
           (dir (file-name-directory file)))
      (unless (file-exists-p dir)
        (make-directory dir t))
      (setq multisession--db (sqlite-open file)))
    (with-sqlite-transaction multisession--db
      ;; Use a write-ahead-log (available since 2010), which makes
      ;; writes a lot faster.
      (sqlite-pragma multisession--db "journal_mode = WAL")
      (sqlite-pragma multisession--db "synchronous = NORMAL")
      (unless (sqlite-select
               multisession--db
               "select name from sqlite_master where type = 'table' and name = 'multisession'")
        ;; Tidy up the database automatically.
        (sqlite-pragma multisession--db "auto_vacuum = FULL")
        ;; Create the table.
        (sqlite-execute
         multisession--db
         "create table multisession (package text not null, key text not null, sequence number not null default 1, value text not null)")
        (sqlite-execute
         multisession--db
         "create unique index multisession_idx on multisession (package, key)")))))

(cl-defmethod multisession-backend-value ((_type (eql 'sqlite)) object)
  (multisession--ensure-db)
  (let ((id (list (multisession--package object)
                  (multisession--key object))))
    (cond
     ;; We have no value yet; check the database.
     ((eq (multisession--cached-value object) multisession--unbound)
      (let ((stored
             (car
              (sqlite-select
               multisession--db
               "select value, sequence from multisession where package = ? and key = ?"
               id))))
        (if stored
            (let ((value (car (read-from-string (car stored)))))
              (setf (multisession--cached-value object) value
                    (multisession--cached-sequence object) (cadr stored))
              value)
          ;; Nothing; return the initial value.
          (multisession--initial-value object))))
     ;; We have a value, but we want to update in case some other
     ;; Emacs instance has updated.
     ((multisession--synchronized object)
      (let ((stored
             (car
              (sqlite-select
               multisession--db
               "select value, sequence from multisession where sequence > ? and package = ? and key = ?"
               (cons (multisession--cached-sequence object) id)))))
        (if stored
            (let ((value (car (read-from-string (car stored)))))
              (setf (multisession--cached-value object) value
                    (multisession--cached-sequence object) (cadr stored))
              value)
          ;; Nothing, return the cached value.
          (multisession--cached-value object))))
     ;; Just return the cached value.
     (t
      (multisession--cached-value object)))))

(cl-defmethod multisession--backend-set-value ((_type (eql 'sqlite))
                                               object value)
  (catch 'done
    (let ((i 0))
      (while (< i 10)
        (condition-case nil
            (throw 'done (multisession--set-value-sqlite object value))
          (sqlite-locked-error
           (setq i (1+ i))
           (sleep-for (+ 0.1 (/ (float (random 10)) 10))))))
      (signal 'sqlite-locked-error "Database is locked"))))

(defun multisession--set-value-sqlite (object value)
  (multisession--ensure-db)
  (with-sqlite-transaction multisession--db
    (let ((id (list (multisession--package object)
                    (multisession--key object)))
          (pvalue
           (let ((print-length nil)
                 (print-circle t)
                 (print-level nil))
             (readablep value))))
      (when (and value (not pvalue))
        (error "Unable to store unreadable value: %s" value))
      (sqlite-execute
       multisession--db
       "insert into multisession(package, key, sequence, value) values(?, ?, 1, ?) on conflict(package, key) do update set sequence = sequence + 1, value = ?"
       (append id (list pvalue pvalue)))
      (setf (multisession--cached-sequence object)
            (caar (sqlite-select
                   multisession--db
                   "select sequence from multisession where package = ? and key = ?"
                   id)))
      (setf (multisession--cached-value object) value))))

(cl-defmethod multisession--backend-values ((_type (eql 'sqlite)))
  (multisession--ensure-db)
  (sqlite-select
   multisession--db
   "select package, key, value from multisession order by package, key"))

(cl-defmethod multisession--backend-delete ((_type (eql 'sqlite)) object)
  (sqlite-execute multisession--db
                  "delete from multisession where package = ? and key = ?"
                  (list (multisession--package object)
                        (multisession--key object))))

;; Files Backend

(defun multisession--encode-file-name (name)
  (url-hexify-string name))

(defun multisession--read-file-value (file object)
  (catch 'done
    (let ((i 0)
          last-error)
      (while (< i 10)
        (condition-case err
            (throw 'done
                   (with-temp-buffer
                     (let* ((time (file-attribute-modification-time
                                   (file-attributes file)))
                            (coding-system-for-read 'utf-8-emacs-unix))
                       (insert-file-contents file)
                       (let ((stored (read (current-buffer))))
                         (setf (multisession--cached-value object) stored
                               (multisession--cached-sequence object) time)
                         stored))))
          ;; Windows uses OS-level file locking that may preclude
          ;; reading the file in some circumstances.  In addition,
          ;; rename-file is not an atomic operation on MS-Windows,
          ;; when the target file already exists, so there could be a
          ;; small race window when the file to read doesn't yet
          ;; exist.  So when these problems happen, wait a bit and retry.
          ((permission-denied file-missing)
           (setq i (1+ i)
                 last-error err)
           (sleep-for (+ 0.1 (/ (float (random 10)) 10))))))
      (signal (car last-error) (cdr last-error)))))

(defun multisession--object-file-name (object)
  (expand-file-name
   (concat "files/"
           (multisession--encode-file-name (multisession--package object))
           "/"
           (multisession--encode-file-name (multisession--key object))
           ".value")
   multisession-directory))

(cl-defmethod multisession-backend-value ((_type (eql 'files)) object)
  (let ((file (multisession--object-file-name object)))
    (cond
     ;; We have no value yet; see whether it's stored.
     ((eq (multisession--cached-value object) multisession--unbound)
      (if (file-exists-p file)
          (multisession--read-file-value file object)
        ;; Nope; return the initial value.
        (multisession--initial-value object)))
     ;; We have a value, but we want to update in case some other
     ;; Emacs instance has updated.
     ((multisession--synchronized object)
      (if (and (file-exists-p file)
               (time-less-p (multisession--cached-sequence object)
                            (file-attribute-modification-time
                             (file-attributes file))))
          (multisession--read-file-value file object)
        ;; Nothing, return the cached value.
        (multisession--cached-value object)))
     ;; Just return the cached value.
     (t
      (multisession--cached-value object)))))

(cl-defmethod multisession--backend-set-value ((_type (eql 'files))
                                               object value)
  (let ((file (multisession--object-file-name object))
        (time (current-time)))
    ;; Ensure that the directory exists.
    (let ((dir (file-name-directory file)))
      (unless (file-exists-p dir)
        (make-directory dir t)))
    (with-temp-buffer
      (let ((print-length nil)
            (print-circle t)
            (print-level nil))
        (prin1 value (current-buffer)))
      (goto-char (point-min))
      (condition-case nil
          (read (current-buffer))
        (error (error "Unable to store unreadable value: %s" (buffer-string))))
      ;; Write to a temp file in the same directory and rename to the
      ;; file for somewhat better atomicity.
      (let ((coding-system-for-write 'utf-8-emacs-unix)
            (create-lockfiles nil)
            (temp (make-temp-name file))
            (write-region-inhibit-fsync nil))
        (write-region (point-min) (point-max) temp nil 'silent)
        (set-file-times temp time)
        (rename-file temp file t)))
    (setf (multisession--cached-sequence object) time
          (multisession--cached-value object) value)))

(cl-defmethod multisession--backend-values ((_type (eql 'files)))
  (mapcar (lambda (file)
            (let ((bits (file-name-split file)))
              (list (url-unhex-string (car (last bits 2)))
                    (url-unhex-string
                     (file-name-sans-extension (car (last bits))))
                    (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8-emacs-unix))
                        (insert-file-contents file)
                        (read (current-buffer)))))))
          (directory-files-recursively
           (expand-file-name "files" multisession-directory)
           "\\.value\\'")))

(cl-defmethod multisession--backend-delete ((_type (eql 'files)) object)
  (let ((file (multisession--object-file-name object)))
    (when (file-exists-p file)
      (delete-file file))))

;; Mode for editing.

(defvar-keymap multisession-edit-mode-map
  :parent tabulated-list-mode-map
  "d" #'multisession-delete-value
  "e" #'multisession-edit-value)

(define-derived-mode multisession-edit-mode special-mode "Multisession"
  "This mode lists all elements in the \"multisession\" database."
  :interactive nil
  (buffer-disable-undo)
  (setq-local buffer-read-only t
              truncate-lines t)
  (setq tabulated-list-format
        [("Package" 10)
         ("Key" 30)
         ("Value" 30)])
  (setq-local revert-buffer-function #'multisession-edit-mode--revert))

;;;###autoload
(defun list-multisession-values (&optional choose-storage)
  "List all values in the \"multisession\" database.
If CHOOSE-STORAGE (interactively, the prefix), query for the
storage method to list."
  (interactive "P")
  (let ((storage
         (if choose-storage
             (intern (completing-read "Storage method: " '(sqlite files) nil t))
           multisession-storage)))
    (pop-to-buffer (get-buffer-create (format "*Multisession %s*" storage)))
    (multisession-edit-mode)
    (setq-local multisession-storage storage)
    (multisession-edit-mode--revert)
    (goto-char (point-min))))

(defun multisession-edit-mode--revert (&rest _)
  (let ((inhibit-read-only t)
        (id (get-text-property (point) 'tabulated-list-id)))
    (erase-buffer)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (mapcar (lambda (elem)
                    (list
                     (cons (car elem) (cadr elem))
                     (vector (car elem) (cadr elem)
                             (string-replace "\n" "\\n"
                                             (format "%s" (caddr elem))))))
                  (multisession--backend-values multisession-storage)))
    (tabulated-list-print t)
    (goto-char (point-min))
    (when id
      (when-let ((match
                  (text-property-search-forward 'tabulated-list-id id t)))
        (goto-char (prop-match-beginning match))))))

(defun multisession-delete-value (id)
  "Delete the value at point."
  (interactive (list (get-text-property (point) 'tabulated-list-id))
               multisession-edit-mode)
  (unless id
    (error "No value on the current line"))
  (unless (yes-or-no-p "Really delete this item? ")
    (user-error "Not deleting"))
  (multisession--backend-delete multisession-storage
                                (make-multisession :package (car id)
                                                   :key (cdr id)))
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))))

(defun multisession-edit-value (id)
  "Edit the value at point."
  (interactive (list (get-text-property (point) 'tabulated-list-id))
               multisession-edit-mode)
  (unless id
    (error "No value on the current line"))
  (let* ((object (or
                  ;; If the multisession variable already exists, use
                  ;; it (so that we update it).
                  (if-let (sym (intern-soft (cdr id)))
                      (and (boundp sym) (symbol-value sym))
                    nil)
                  ;; Create a new object.
                  (make-multisession
                   :package (car id)
                   :key (cdr id)
                   :storage multisession-storage)))
         (value (multisession-value object)))
    (setf (multisession-value object)
          (car (read-from-string
                (read-string "New value: " (prin1-to-string value))))))
  (multisession-edit-mode--revert))

(provide 'multisession)

;;; multisession.el ends here
