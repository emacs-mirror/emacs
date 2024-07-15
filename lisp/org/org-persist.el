;;; org-persist.el --- Persist cached data across Emacs sessions         -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Ihor Radchenko <yantar92 at posteo dot net>
;; Keywords: cache, storage

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
;;
;; This file implements persistent cache storage across Emacs sessions.
;; Both global and buffer-local data can be stored.  This
;; implementation is not meant to be used to store important data -
;; all the caches should be safe to remove at any time.
;;
;; Entry points are `org-persist-register', `org-persist-write',
;; `org-persist-read', and `org-persist-load'.
;;
;; `org-persist-register' will mark the data to be stored.  By
;; default, the data is written on disk before exiting Emacs session.
;; Optionally, the data can be written immediately.
;;
;; `org-persist-write' will immediately write the data onto disk.
;;
;; `org-persist-read' will read the data and return its value or list
;; of values for each requested container.
;;
;; `org-persist-load' will read the data with side effects.  For
;; example, loading `elisp' container will assign the values to
;; variables.
;;
;; Example usage:
;;
;; 1. Temporarily cache Elisp symbol value to disk.  Remove upon
;;    closing Emacs:
;;    (org-persist-write 'variable-symbol)
;;    (org-persist-read 'variable-symbol) ;; read the data later
;;
;; 2. Temporarily cache a remote URL file to disk.  Remove upon
;;    closing Emacs:
;;    (org-persist-write 'url "https://static.fsf.org/common/img/logo-new.png")
;;    (org-persist-read 'url "https://static.fsf.org/common/img/logo-new.png")
;;    `org-persist-read' will return the cached file location or nil if cached file
;;    has been removed.
;;
;; 3. Temporarily cache a file, including TRAMP path to disk:
;;    (org-persist-write 'file "/path/to/file")
;;
;; 4. Cache file or URL while some other file exists.
;;    (org-persist-register '(url "https://static.fsf.org/common/img/logo-new.png") '(:file "/path to the other file") :expiry 'never :write-immediately t)
;;    or, if the other file is current buffer file
;;    (org-persist-register '(url "https://static.fsf.org/common/img/logo-new.png") (current-buffer) :expiry 'never :write-immediately t)
;;
;; 5. Cache value of a Elisp variable to disk.  The value will be
;;    saved and restored automatically (except buffer-local
;;    variables).
;;    ;; Until `org-persist-default-expiry'
;;    (org-persist-register 'variable-symbol)
;;    ;; Specify expiry explicitly
;;    (org-persist-register 'variable-symbol :expiry 'never)
;;    ;; Save buffer-local variable (buffer-local will not be
;;    ;; autoloaded!)
;;    (org-persist-register 'org-element--cache (current-buffer))
;;    ;; Save several buffer-local variables preserving circular links
;;    ;; between:
;;    (org-persist-register 'org-element--headline-cache (current-buffer)
;;               :inherit 'org-element--cache)
;;
;; 6. Load variable by side effects assigning variable symbol:
;;    (org-persist-load 'variable-symbol (current-buffer))
;;
;; 7. Version variable value:
;;    (org-persist-register '((elisp variable-symbol) (version "2.0")))
;;
;; 8. Define a named container group:
;;
;;    (let ((info1 "test")
;;          (info2 "test 2"))
;;      (org-persist-register
;;         `("Named data" (elisp info1 local) (elisp info2 local))
;;         nil :write-immediately t))
;;    (org-persist-read
;;       "Named data"
;;       nil nil nil :read-related t) ; => ("Named data" "test" "test2")
;;
;; 9. Cancel variable persistence:
;;    (org-persist-unregister 'variable-symbol 'all) ; in all buffers
;;    (org-persist-unregister 'variable-symbol) ;; global variable
;;    (org-persist-unregister 'variable-symbol (current-buffer)) ;; buffer-local
;;
;; Most common data type is variable data.  However, other data types
;; can also be stored.
;;
;; Persistent data is stored in individual files.  Each of the files
;; can contain a collection of related data, which is particularly
;; useful when, say, several variables cross-reference each-other's
;; data-cells and we want to preserve their circular structure.
;;
;; Each data collection can be associated with a local or remote file,
;; its inode number, contents hash.  The persistent data collection
;; can later be accessed using either file buffer, file, inode, or
;; contents hash.
;;
;; The data collections can be versioned and removed upon expiry.
;;
;; In the code below, I will use the following naming conventions:
;;
;; 1. Container :: a type of data to be stored
;;    Containers can store elisp variables, files, and version
;;    numbers.  Each container can be customized with container
;;    options.  For example, `elisp' container is customized with
;;    variable symbol.  (elisp variable) is a container storing
;;    Lisp variable value.  Similarly, (version "2.0") container
;;    will store version number.
;;
;;    Container can also refer to a group of containers:
;;
;;    ;; Three containers stored together.
;;    '((elisp variable) (file "/path") (version "x.x"))
;;
;;    Providing a single container from the list to `org-persist-read'
;;    is sufficient to retrieve all the containers (with appropriate
;;    optional parameter).
;;
;;    Example:
;;
;;    (org-persist-register '((version "My data") (file "/path/to/file")) '(:key "key") :write-immediately t)
;;    (org-persist-read '(version "My data") '(:key "key") :read-related t) ;; => '("My data" "/path/to/file/copy")
;;
;;    Individual containers can also take a short form (not a list):
;;
;;    '("String" file '(quoted elisp "value") :keyword)
;;    is the same with
;;    '((elisp-data "String") (file nil)
;;      (elisp-data '(quoted elisp "value")) (elisp-data :keyword))
;;
;;    Note that '(file "String" (elisp value)) would be interpreted as
;;    `file' container with "String" path and extra options.  See
;;    `org-persist--normalize-container'.
;;
;; 2. Associated :: an object the container is associated with.  The
;;    object can be a buffer, file, inode number, file contents hash,
;;    a generic key, or multiple of them.  Associated can also be nil.
;;
;;    Example:
;;
;;    '(:file "/path/to/file" :inode number :hash buffer-hash :key arbitrary-key)
;;
;;    When several objects are associated with a single container, it
;;    is not necessary to provide them all to access the container.
;;    Just using a single :file/:inode/:hash/:key is sufficient.  This
;;    way, one can retrieve cached data even when the file has moved -
;;    by contents hash.
;;
;; 3. Data collection :: a list of containers, the associated
;;    object/objects, expiry, access time, and information about where
;;    the cache is stored.  Each data collection can also have
;;    auxiliary records.  Their only purpose is readability of the
;;    collection index.
;;
;;    Example:
;;
;;    (:container
;;     ((index "2.7"))
;;     :persist-file "ba/cef3b7-e31c-4791-813e-8bd0bf6c5f9c"
;;     :associated nil :expiry never
;;     :last-access 1672207741.6422956 :last-access-hr "2022-12-28T09:09:01+0300")
;;
;; 4. Index file :: a file listing all the stored data collections.
;;
;; 5. Persist file :: a file holding data values or references to
;;    actual data values for a single data collection.  This file
;;    contains an alist associating each data container in data
;;    collection with its value or a reference to the actual value.
;;
;;    Example (persist file storing two elisp container values):
;;
;;    (((elisp org-element--headline-cache) . #s(avl-tree- ...))
;;     ((elisp org-element--cache)  . #s(avl-tree- ...)))
;;
;; All the persistent data is stored in `org-persist-directory'.  The data
;; collections are listed in `org-persist-index-file' and the actual data is
;; stored in UID-style subfolders.
;;
;; The `org-persist-index-file' stores the value of `org-persist--index'.
;;
;; Each collection is represented as a plist containing the following
;; properties:
;;
;; - `:container'   : list of data containers to be stored in single
;;                    file;
;; - `:persist-file': data file name;
;; - `:associated'  : list of associated objects;
;; - `:last-access' : last date when the container has been accessed;
;; - `:expiry'      : list of expiry conditions.
;; - all other keywords are ignored
;;
;; The available types of data containers are:
;; 1. (elisp variable-symbol scope) or just variable-symbol :: Storing
;;    elisp variable data.  SCOPE can be
;;
;;    - `nil'    :: Use buffer-local value in associated :file or global
;;                 value if no :file is associated.
;;    - string :: Use buffer-local value in buffer named STRING or
;;                with STRING `buffer-file-name'.
;;    - `local' :: Use symbol value in current scope.
;;                 Note: If `local' scope is used without writing the
;;                 value immediately, the actual stored value is
;;                 undefined.
;;
;; 2. (file) :: Store a copy of the associated file preserving the
;;    extension.

;;    (file "/path/to/a/file") :: Store a copy of the file in path.
;;
;; 3. (version "version number") :: Version the data collection.
;;     If the stored collection has different version than "version
;;     number", disregard it.
;;
;; 4. (url) :: Store a downloaded copy of URL object given by
;;             associated :file.
;;    (url "path") :: Use "path" instead of associated :file.
;;
;; The data collections can expire, in which case they will be removed
;; from the persistent storage at the end of Emacs session.  The
;; expiry condition can be set when saving/registering data
;; containers.  The expirty condition can be `never' - data will never
;; expire; nil - data will expire at the end of current Emacs session;
;; a number - data will expire after the number days from last access;
;; a function - data will expire if the function, called with a single
;; argument - collection, returns non-nil.
;;
;;
;; Data collections associated with files will automatically expire
;; when the file is removed.  If the associated file is remote, the
;; expiry is controlled by `org-persist-remote-files' instead.
;;
;; Data loading/writing can be more accurately controlled using
;; `org-persist-before-write-hook', `org-persist-before-read-hook',
;; and `org-persist-after-read-hook'.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'org-compat)
(require 'org-id)
(require 'xdg nil t)

(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-next-visible-heading "org" (arg))
(declare-function org-at-heading-p "org" (&optional invisible-not-ok))

;; Silence byte-compiler (used in `org-persist--write-elisp-file').
(defvar pp-use-max-width)

(defconst org-persist--storage-version "3.2"
  "Persistent storage layout version.")

(defgroup org-persist nil
  "Persistent cache for Org mode."
  :tag "Org persist"
  :group 'org)

(defcustom org-persist-directory
  (expand-file-name
   (org-file-name-concat
    (let ((cache-dir (when (fboundp 'xdg-cache-home)
                       (xdg-cache-home))))
      (if (or (seq-empty-p cache-dir)
              (not (file-exists-p cache-dir))
              (file-exists-p (org-file-name-concat
                              user-emacs-directory
                              "org-persist")))
          user-emacs-directory
        cache-dir))
    "org-persist/"))
  "Directory where the data is stored."
  :group 'org-persist
  :package-version '(Org . "9.6")
  :type 'directory)

(defcustom org-persist-remote-files 100
  "Whether to keep persistent data for remote files.

When this variable is nil, never save persistent data associated with
remote files.  When t, always keep the data.  When
`check-existence', contact remote server containing the file and only
keep the data when the file exists on the server.  When a number, keep
up to that number persistent values for remote files.

Note that the last option `check-existence' may cause Emacs to show
password prompts to log in."
  :group 'org-persist
  :package-version '(Org . "9.6")
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (number :tag "Keep not more than X files")
                 (const :tag "Check if exist on remote" check-existence)))

(defcustom org-persist-default-expiry 30
  "Default expiry condition for persistent data.

When this variable is nil, all the data vanishes at the end of Emacs
session.  When `never', the data never vanishes.  When a number, the
data is deleted that number days after last access.  When a function,
it should be a function returning non-nil when the data is expired.  The
function will be called with a single argument - collection."
  :group 'org-persist
  :package-version '(Org . "9.6")
  :type '(choice (const :tag "Never" never)
                 (const :tag "Always" nil)
                 (number :tag "Keep N days")
                 (function :tag "Function")))

(defconst org-persist-index-file "index.eld"
  "File name used to store the data index.")

(defconst org-persist-gc-lock-file "gc-lock.eld"
  "File used to store information about active Emacs sessions.
The file contains an alist of (`before-init-time' . LAST-ACTIVE-TIME).
`before-init-time' uniquely identifies Emacs process and
LAST-ACTIVE-TIME is written every `org-persist-gc-lock-interval'
seconds.  When LAST-ACTIVE-TIME is more than
`org-persist-gc-lock-expiry' seconds ago, that Emacs session is
considered not active.")

(defvar org-persist-gc-lock-interval (* 60 60) ; 1 hour
  "Interval in seconds for refreshing `org-persist-gc-lock-file'.")

(defvar org-persist-gc-lock-expiry (* 60 60 24) ; 1 day
  "Interval in seconds for expiring a record in `org-persist-gc-lock-file'.")

(defvar org-persist--disable-when-emacs-Q t
  "Disable persistence when Emacs is called with -Q command line arg.
When non-nil, this sets `org-persist-directory' to temporary directory.

This variable must be set before loading org-persist library.")

(defvar org-persist-before-write-hook nil
  "Abnormal hook ran before saving data.
The hook must accept the same arguments as `org-persist-write'.
The hooks will be evaluated until a hook returns non-nil.
If any of the hooks return non-nil, do not save the data.")

(defvar org-persist-before-read-hook nil
  "Abnormal hook ran before reading data.
The hook must accept the same arguments as `org-persist-read'.
The hooks will be evaluated until a hook returns non-nil.
If any of the hooks return non-nil, do not read the data.")

(defvar org-persist-after-read-hook nil
  "Abnormal hook ran after reading data.
The hook must accept the same arguments as `org-persist-read'.")

(defvar org-persist--index nil
  "Global index.

The index is a list of plists.  Each plist contains information about
persistent data storage.  Each plist contains the following
properties:

  - `:container'  : list of data containers to be stored in single file
  - `:persist-file': data file name
  - `:associated'  : list of associated objects
  - `:last-access' : last date when the container has been read
  - `:expiry'      : list of expiry conditions
  - all other keywords are ignored.")

(defvar org-persist--index-hash nil
  "Hash table storing `org-persist--index'.  Used for quick access.
The keys are conses of (container . associated).")

(defvar org-persist--index-age nil
  "The modification time of the index file, when it was loaded.")

(defvar org-persist--report-time nil
  "Whether to report read/write time.

When the value is a number, it is a threshold number of seconds.  If
the read/write time of a single persist file exceeds the threshold, a
message is displayed.

When the value is a non-nil non-number, always display the message.
When the value is nil, never display the message.")

;;;; Common functions

(defun org-persist--display-time (duration format &rest args)
  "Report DURATION according to FORMAT + ARGS message.
FORMAT and ARGS are passed to `message'."
  (when (or (and org-persist--report-time
                 (numberp org-persist--report-time)
                 (>= duration org-persist--report-time))
            (and org-persist--report-time
                 (not (numberp org-persist--report-time))))
    (apply #'message
           (format "org-persist: %s took %%.2f sec" format)
           (append args (list duration)))))

(defun org-persist--read-elisp-file (&optional buffer-or-file)
  "Read elisp data from BUFFER-OR-FILE or current buffer."
  (let (;; UTF-8 is explicitly used in `org-persist--write-elisp-file'.
        (coding-system-for-read 'emacs-internal)
        (buffer-or-file (or buffer-or-file (current-buffer))))
    (with-temp-buffer
      (if (bufferp buffer-or-file)
          (set-buffer buffer-or-file)
        (insert-file-contents buffer-or-file))
      (condition-case err
          (let ((read-circle t)
                (start-time (float-time)))
            ;; FIXME: Reading sometimes fails to read circular objects.
            ;; I suspect that it happens when we have object reference
            ;; #N# read before object definition #N=.  If it is really
            ;; so, it should be Emacs bug - either in `read' or in
            ;; `prin1'.  Meanwhile, just fail silently when `read'
            ;; fails to parse the saved cache object.
            (prog1
                (read (current-buffer))
              (org-persist--display-time
               (- (float-time) start-time)
               "Reading from %S" buffer-or-file)))
        ;; Recover gracefully if index file is corrupted.
        (error
         ;; Remove problematic file.
         (unless (bufferp buffer-or-file) (delete-file buffer-or-file))
         ;; Do not report the known error to user.
         (if (string-match-p "Invalid read syntax" (error-message-string err))
             (message "Emacs reader failed to read data in %S. The error was: %S"
                      buffer-or-file (error-message-string err))
           (warn "Emacs reader failed to read data in %S. The error was: %S"
                 buffer-or-file (error-message-string err)))
         nil)))))

(defun org-persist--write-elisp-file (file data &optional no-circular pp)
  "Write elisp DATA to FILE."
  ;; Fsync slightly reduces the chance of an incomplete filesystem
  ;; write, however on modern hardware its effectiveness is
  ;; questionable and it is insufficient to guarantee complete writes.
  ;; Coupled with the significant performance hit if writing many
  ;; small files, it simply does not make sense to use fsync here,
  ;; particularly as cache corruption is only a minor inconvenience.
  ;; With all this in mind, we ensure `write-region-inhibit-fsync' is
  ;; set.
  ;;
  ;; To read more about this, see the comments in Emacs' fileio.c, in
  ;; particular the large comment block in init_fileio.
  (let ((write-region-inhibit-fsync t)
        ;; We set UTF-8 here and in `org-persist--read-elisp-file'
        ;; to avoid the overhead from `find-auto-coding'.
        (coding-system-for-write 'emacs-internal)
        (print-circle (not no-circular))
        print-level
        print-length
        print-quoted
        (print-escape-control-characters t)
        (print-escape-nonascii t)
        (print-continuous-numbering t)
        print-number-table
        (start-time (float-time)))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    ;; Force writing even when the file happens to be opened by
    ;; another Emacs process.
    (cl-letf (((symbol-function #'ask-user-about-lock)
               ;; FIXME: Emacs 27 does not yet have `always'.
               (lambda (&rest _) t)))
      (with-temp-file file
        (insert ";;   -*- mode: lisp-data; -*-\n")
        (if pp
            (let ((pp-use-max-width nil)) ; Emacs bug#58687
              (pp data (current-buffer)))
          (prin1 data (current-buffer)))))
    (org-persist--display-time
     (- (float-time) start-time)
     "Writing to %S" file)))

(defmacro org-persist-gc:generic (container collection)
  "Garbage collect CONTAINER data from COLLECTION."
  `(let* ((c (org-persist--normalize-container ,container))
          (gc-func-symbol (intern (format "org-persist-gc:%s" (car c)))))
     (unless (fboundp gc-func-symbol)
       (error "org-persist: GC function %s not defined"
              gc-func-symbol))
     (funcall gc-func-symbol c ,collection)))

(defmacro org-persist--gc-expired-p (cnd collection)
  "Check if expiry condition CND triggers for COLLECTION."
  `(pcase ,cnd
     (`nil t)
     (`never nil)
     ((pred numberp)
      (when (plist-get ,collection :last-access)
        (> (float-time) (+ (plist-get ,collection :last-access) (* ,cnd 24 60 60)))))
     ((pred functionp)
      (funcall ,cnd ,collection))
     (_ (error "org-persist: Unsupported expiry type %S" ,cnd))))

;;;; Working with index

(defmacro org-persist-collection-let (collection &rest body)
  "Bind container and associated from COLLECTION and execute BODY."
  (declare (debug (form body)) (indent 1))
  `(with-no-warnings
     (let* ((container (plist-get ,collection :container))
            (associated (plist-get ,collection :associated))
            (path (plist-get associated :file))
            (inode (plist-get associated :inode))
            (hash (plist-get associated :hash))
            (key (plist-get associated :key)))
       ;; Suppress "unused variable" warnings.
       (ignore container associated path inode hash key)
       ,@body)))

(defun org-persist--find-index (collection)
"Find COLLECTION in `org-persist--index'."
(org-persist-collection-let collection
  (and org-persist--index-hash
       (catch :found
         (dolist (cont (cons container container))
           (let (r)
             (setq r (or (gethash (cons cont associated) org-persist--index-hash)
                         (and path (gethash (cons cont (list :file path)) org-persist--index-hash))
                         (and inode (gethash (cons cont (list :inode inode)) org-persist--index-hash))
                         (and hash (gethash (cons cont (list :hash hash)) org-persist--index-hash))
                         (and key (gethash (cons cont (list :key key)) org-persist--index-hash))))
             (when r (throw :found r))))))))

(defun org-persist--add-to-index (collection &optional hash-only)
  "Add or update COLLECTION in `org-persist--index'.
When optional HASH-ONLY is non-nil, only modify the hash table.
Return PLIST."
  (org-persist-collection-let collection
    (let ((existing (org-persist--find-index collection)))
      (if existing
          (progn
            (plist-put existing :container container)
            (plist-put (plist-get existing :associated) :file path)
            (plist-put (plist-get existing :associated) :inode inode)
            (plist-put (plist-get existing :associated) :hash hash)
            (plist-put (plist-get existing :associated) :key key)
            existing)
        (unless hash-only (push collection org-persist--index))
        (unless org-persist--index-hash (setq org-persist--index-hash (make-hash-table :test 'equal)))
        (dolist (cont (cons container container))
          (puthash (cons cont associated) collection org-persist--index-hash)
          (when path (puthash (cons cont (list :file path)) collection org-persist--index-hash))
          (when inode (puthash (cons cont (list :inode inode)) collection org-persist--index-hash))
          (when hash (puthash (cons cont (list :hash inode)) collection org-persist--index-hash))
          (when key (puthash (cons cont (list :key inode)) collection org-persist--index-hash)))
        collection))))

(defun org-persist--remove-from-index (collection)
  "Remove COLLECTION from `org-persist--index'."
  (let ((existing (org-persist--find-index collection)))
    (when existing
      (org-persist-collection-let collection
        (dolist (cont (cons container container))
          (unless (listp (car container))
            (org-persist-gc:generic cont collection)
            (dolist (afile (org-persist-associated-files:generic cont collection))
              (delete-file afile)))
          (remhash (cons cont associated) org-persist--index-hash)
          (when path (remhash (cons cont (list :file path)) org-persist--index-hash))
          (when inode (remhash (cons cont (list :inode inode)) org-persist--index-hash))
          (when hash (remhash (cons cont (list :hash hash)) org-persist--index-hash))
          (when key (remhash (cons cont (list :key key)) org-persist--index-hash))))
      (setq org-persist--index (delq existing org-persist--index)))))

(defun org-persist--get-collection (container &optional associated misc)
  "Return or create collection used to store CONTAINER for ASSOCIATED.
When ASSOCIATED is nil, it is a global CONTAINER.
ASSOCIATED can also be a (:buffer buffer) or buffer, (:file file-path)
or file-path, (:inode inode), (:hash hash), or or (:key key).
MISC, if non-nil will be appended to the collection.  It must be a plist."
  (unless (and (listp container) (listp (car container)))
    (setq container (list container)))
  (setq associated (org-persist--normalize-associated associated))
  (when (and misc (or (not (listp misc)) (= 1 (% (length misc) 2))))
    (error "org-persist: Not a plist: %S" misc))
  (or (org-persist--find-index
       `( :container ,(org-persist--normalize-container container)
          :associated ,associated))
      (org-persist--add-to-index
       (nconc
        (list :container (org-persist--normalize-container container)
              :persist-file
              (replace-regexp-in-string "^.." "\\&/" (org-id-uuid))
              :associated associated)
        misc))))

;;;; Reading container data.

(defun org-persist--normalize-container (container &optional inner)
  "Normalize CONTAINER representation into (type . settings).

When INNER is non-nil, do not try to match as list of containers."
  (pcase container
    ((or `elisp `elisp-data `version `file `index `url)
     `(,container nil))
    ((or (pred keywordp) (pred stringp) `(quote . ,_))
     `(elisp-data ,container))
    ((pred symbolp)
     `(elisp ,container))
    (`(,(or `elisp `elisp-data `version `file `index `url) . ,_)
     container)
    ((and (pred listp) (guard (not inner)))
     (mapcar (lambda (c) (org-persist--normalize-container c 'inner)) container))
    (_ (error "org-persist: Unknown container type: %S" container))))

(defvar org-persist--associated-buffer-cache (make-hash-table :weakness 'key)
  "Buffer hash cache.")

(defun org-persist--normalize-associated (associated)
  "Normalize ASSOCIATED representation into (:type value)."
  (pcase associated
    ((or (pred stringp) `(:file ,_))
     (unless (stringp associated)
       (setq associated (cadr associated)))
     (let* ((rtn `(:file ,associated))
            (inode (and
                    ;; Do not store :inode for remote files - it may
                    ;; be time-consuming on slow connections or even
                    ;; fail completely when ssh connection is closed.
                    (not (file-remote-p associated))
                    (fboundp 'file-attribute-inode-number)
                    (file-attribute-inode-number
                     (file-attributes associated)))))
       (when inode (plist-put rtn :inode inode))
       rtn))
    ((or (pred bufferp) `(:buffer ,_))
     (unless (bufferp associated)
       (setq associated (cadr associated)))
     (let ((cached (gethash associated org-persist--associated-buffer-cache))
           file inode hash)
       (if (and cached (eq (buffer-modified-tick associated)
                           (car cached)))
           (progn
             (setq file (nth 1 cached)
                   inode (nth 2 cached)
                   hash (nth 3 cached)))
         (setq file (buffer-file-name
                     (or (buffer-base-buffer associated)
                         associated)))
         (setq inode (when (and file
                                ;; Do not store :inode for remote files - it may
                                ;; be time-consuming on slow connections or even
                                ;; fail completely when ssh connection is closed.
                                (not (file-remote-p file))
                                (fboundp 'file-attribute-inode-number))
                       (file-attribute-inode-number
                        (file-attributes file))))
         (setq hash
               ;; `secure-hash' may trigger interactive dialog when it
               ;; cannot determine the coding system automatically.
               ;; Force coding system that works reliably for any text
               ;; to avoid it.  The hash will be consistent, as long
               ;; as we use the same coding system.
               (let ((coding-system-for-write 'emacs-internal))
                 (secure-hash 'md5 associated)))
         (puthash associated
                  (list (buffer-modified-tick associated)
                        file inode hash)
                  org-persist--associated-buffer-cache))
       (let ((rtn `(:hash ,hash)))
         (when file (setq rtn (plist-put rtn :file file)))
         (when inode (setq rtn (plist-put rtn :inode inode)))
         rtn)))
    ((pred listp)
     associated)
    (_ (error "Unknown associated object %S" associated))))

(defmacro org-persist-read:generic (container reference-data collection)
  "Read and return the data stored in CONTAINER.
REFERENCE-DATA is associated with CONTAINER in the persist file.
COLLECTION is the plist holding data collection."
  `(let* ((c (org-persist--normalize-container ,container))
          (read-func-symbol (intern (format "org-persist-read:%s" (car c)))))
     (setf ,collection (plist-put ,collection :last-access (float-time)))
     (setf ,collection (plist-put ,collection :last-access-hr (format-time-string "%FT%T%z" (float-time))))
     (unless (fboundp read-func-symbol)
       (error "org-persist: Read function %s not defined"
              read-func-symbol))
     (funcall read-func-symbol c ,reference-data ,collection)))

(defun org-persist-read:elisp (_ lisp-value __)
  "Read elisp container and return LISP-VALUE."
  lisp-value)

(defun org-persist-read:elisp-data (container _ __)
  "Read elisp-data CONTAINER."
  (cadr container))

(defalias 'org-persist-read:version #'org-persist-read:elisp-data)

(defun org-persist-read:file (_ path __)
  "Read file container from PATH."
  (when (and path (file-exists-p (org-file-name-concat org-persist-directory path)))
    (org-file-name-concat org-persist-directory path)))

(defun org-persist-read:url (_ path __)
  "Read file container from PATH."
  (when (and path (file-exists-p (org-file-name-concat org-persist-directory path)))
    (org-file-name-concat org-persist-directory path)))

(defun org-persist-read:index (cont index-file _)
  "Read index container CONT from INDEX-FILE."
  (when (file-exists-p index-file)
    (let ((index (org-persist--read-elisp-file index-file)))
      (when index
        (catch :found
          (dolist (collection index)
            (org-persist-collection-let collection
              (when (and (not associated)
                         (pcase container
                           (`((index ,version))
                            (equal version (cadr cont)))
                           (_ nil)))
                (throw :found index)))))))))

;;;; Applying container data for side effects.

(defmacro org-persist-load:generic (container reference-data collection)
  "Load the data stored in CONTAINER for side effects.
REFERENCE-DATA is associated with CONTAINER in the persist file.
COLLECTION is the plist holding data collection."
  `(let* ((container (org-persist--normalize-container ,container))
          (load-func-symbol (intern (format "org-persist-load:%s" (car container)))))
     (setf ,collection (plist-put ,collection :last-access (float-time)))
     (setf ,collection (plist-put ,collection :last-access-hr (format-time-string "%FT%T%z" (float-time))))
     (unless (fboundp load-func-symbol)
       (error "org-persist: Load function %s not defined"
              load-func-symbol))
     (funcall load-func-symbol container ,reference-data ,collection)))

(defun org-persist-load:elisp (container lisp-value collection)
  "Assign elisp CONTAINER in COLLECTION LISP-VALUE."
  (let ((lisp-symbol (cadr container))
        (buffer (when (plist-get (plist-get collection :associated) :file)
                  (get-file-buffer (plist-get (plist-get collection :associated) :file)))))
    (if buffer
        (with-current-buffer buffer
          (make-variable-buffer-local lisp-symbol)
          (set lisp-symbol lisp-value))
      (set lisp-symbol lisp-value))))

(defalias 'org-persist-load:elisp-data #'org-persist-read:elisp-data)
(defalias 'org-persist-load:version #'org-persist-read:version)
(defalias 'org-persist-load:file #'org-persist-read:file)

(defun org-persist-load:index (container index-file _)
  "Load `org-persist--index' from INDEX-FILE according to CONTAINER."
  (unless org-persist--index
    (setq org-persist--index (org-persist-read:index container index-file nil)
          org-persist--index-hash nil
          org-persist--index-age (file-attribute-modification-time
                                  (file-attributes index-file)))
    (if org-persist--index
        (mapc (lambda (collection) (org-persist--add-to-index collection 'hash)) org-persist--index)
      (setq org-persist--index nil)
      (when (file-exists-p org-persist-directory)
        (dolist (file (directory-files org-persist-directory 'absolute
                                       "\\`[^.][^.]"))
          (if (file-directory-p file)
              (delete-directory file t)
            (delete-file file))))
      (plist-put (org-persist--get-collection container) :expiry 'never))))

(defun org-persist--load-index ()
  "Load `org-persist--index'."
  (org-persist-load:index
   `(index ,org-persist--storage-version)
   (org-file-name-concat org-persist-directory org-persist-index-file)
   nil))

;;;; Writing container data

(defmacro org-persist-write:generic (container collection)
  "Write CONTAINER in COLLECTION."
  `(let* ((c (org-persist--normalize-container ,container))
          (write-func-symbol (intern (format "org-persist-write:%s" (car c)))))
     (unless (plist-get ,collection :last-access)
       (setf ,collection (plist-put ,collection :last-access (float-time)))
       (setf ,collection (plist-put ,collection :last-access-hr (format-time-string "%FT%T%z" (float-time)))))
     (unless (fboundp write-func-symbol)
       (error "org-persist: Write function %s not defined"
              write-func-symbol))
     (funcall write-func-symbol c ,collection)))

(defun org-persist-write:elisp (container collection)
  "Write elisp CONTAINER according to COLLECTION."
  (let ((scope (nth 2 container)))
    (pcase scope
      ((pred stringp)
       (when-let ((buf (or (get-buffer scope)
                           (get-file-buffer scope))))
         ;; FIXME: There is `buffer-local-boundp' introduced in Emacs 28.
         ;; Not using it yet to keep backward compatibility.
         (condition-case nil
             (buffer-local-value (cadr container) buf)
           (void-variable nil))))
      (`local
       (when (boundp (cadr container))
         (symbol-value (cadr container))))
      (`nil
       (if-let ((buf (and (plist-get (plist-get collection :associated) :file)
                          (get-file-buffer (plist-get (plist-get collection :associated) :file)))))
           ;; FIXME: There is `buffer-local-boundp' introduced in Emacs 28.
           ;; Not using it yet to keep backward compatibility.
           (condition-case nil
               (buffer-local-value (cadr container) buf)
             (void-variable nil))
         (when (boundp (cadr container))
           (symbol-value (cadr container))))))))

(defalias 'org-persist-write:elisp-data #'ignore)
(defalias 'org-persist-write:version #'ignore)

(defun org-persist-write:file (c collection)
  "Write file container C according to COLLECTION."
  (org-persist-collection-let collection
    (when (or (and path (file-exists-p path))
              (and (stringp (cadr c)) (file-exists-p (cadr c))))
      (when (and (stringp (cadr c)) (file-exists-p (cadr c)))
        (setq path (cadr c)))
      (let* ((persist-file (plist-get collection :persist-file))
             (ext (file-name-extension path))
             (file-copy (org-file-name-concat
                         org-persist-directory
                         (format "%s-%s.%s" persist-file (md5 path) ext))))
        (unless (file-exists-p file-copy)
          (unless (file-exists-p (file-name-directory file-copy))
            (make-directory (file-name-directory file-copy) t))
          (copy-file path file-copy 'overwrite))
        (format "%s-%s.%s" persist-file (md5 path) ext)))))

(defun org-persist-write:url (c collection)
  "Write url container C according to COLLECTION."
  (org-persist-collection-let collection
    (when (or path (cadr c))
      (when (cadr c) (setq path (cadr c)))
      (let* ((persist-file (plist-get collection :persist-file))
             (ext (file-name-extension path))
             (file-copy (org-file-name-concat
                         org-persist-directory
                         (format "%s-%s.%s" persist-file (md5 path) ext))))
        (unless (file-exists-p file-copy)
          (unless (file-exists-p (file-name-directory file-copy))
            (make-directory (file-name-directory file-copy) t))
          (if (org--should-fetch-remote-resource-p path)
              (url-copy-file path file-copy 'overwrite)
            (error "The remote resource %S is considered unsafe, and will not be downloaded"
                   path)))
        (format "%s-%s.%s" persist-file (md5 path) ext)))))

(defun org-persist--check-write-access (path)
  "Check write access to all missing directories in PATH.
Show message and return nil if there is no write access.
Otherwise, return t."
  (let* ((dir (directory-file-name (file-name-as-directory path)))
         (prev dir))
    (while (and (not (file-exists-p dir))
                (setq prev dir)
                (not (equal dir (setq dir (directory-file-name
                                         (file-name-directory dir)))))))
    (if (file-writable-p prev) t ; return t
      (message "org-persist: Missing write access rights to: %S" prev)
      ;; return nil
      nil)))

(defun org-persist-write:index (container _)
  "Write index CONTAINER."
  (org-persist--get-collection container)
  (unless (file-exists-p org-persist-directory)
    (condition-case nil
        (make-directory org-persist-directory 'parent)
      (t
       (warn "Failed to create org-persist storage in %s."
             org-persist-directory)
       (org-persist--check-write-access org-persist-directory))))
  (when (file-exists-p org-persist-directory)
    (let ((index-file
           (org-file-name-concat org-persist-directory org-persist-index-file)))
      (org-persist--merge-index-with-disk)
      (org-persist--write-elisp-file index-file org-persist--index t t)
      (setq org-persist--index-age
            (file-attribute-modification-time (file-attributes index-file)))
      index-file)))

(defun org-persist--save-index ()
  "Save `org-persist--index'."
  (org-persist-write:index
   `(index ,org-persist--storage-version) nil))

(defun org-persist--merge-index-with-disk ()
  "Merge `org-persist--index' with the current index file on disk."
  (let* ((index-file
          (org-file-name-concat org-persist-directory org-persist-index-file))
         (disk-index
          (and (file-exists-p index-file)
               (org-file-newer-than-p index-file org-persist--index-age)
               (org-persist-read:index `(index ,org-persist--storage-version) index-file nil)))
         (combined-index
          (org-persist--merge-index org-persist--index disk-index)))
    (when disk-index
      (setq org-persist--index combined-index
            org-persist--index-age
            (file-attribute-modification-time (file-attributes index-file))))))

(defun org-persist--merge-index (base other)
  "Attempt to merge new index items in OTHER into BASE.
Items with different details are considered too difficult, and skipped."
  (if other
      (let ((new (cl-set-difference other base :test #'equal))
            (base-files (mapcar (lambda (s) (plist-get s :persist-file)) base))
            (combined (reverse base)))
        (dolist (item (nreverse new))
          (unless (or (memq 'index (mapcar #'car (plist-get item :container)))
                      (not (file-exists-p
                            (org-file-name-concat org-persist-directory
                                                  (plist-get item :persist-file))))
                      (member (plist-get item :persist-file) base-files))
            (push item combined)))
        (nreverse combined))
    base))

;;;; Public API

(cl-defun org-persist-register (container &optional associated &rest misc
                               &key inherit
                               &key (expiry org-persist-default-expiry)
                               &key (write-immediately nil)
                               &allow-other-keys)
  "Register CONTAINER in ASSOCIATED to be persistent across Emacs sessions.
Optional key INHERIT makes CONTAINER dependent on another container.
Such dependency means that data shared between variables will be
preserved (see elisp#Circular Objects).
Optional key EXPIRY will set the expiry condition of the container.
It can be `never', nil - until end of session, a number of days since
last access, or a function accepting a single argument - collection.
EXPIRY key has no effect when INHERIT is non-nil.
Optional key WRITE-IMMEDIATELY controls whether to save the container
data immediately.
MISC will be appended to the collection.  It must be alternating :KEY
VALUE pairs.
When WRITE-IMMEDIATELY is non-nil, the return value will be the same
with `org-persist-write'."
  (unless org-persist--index (org-persist--load-index))
  (setq container (org-persist--normalize-container container))
  (when inherit
    (setq inherit (org-persist--normalize-container inherit))
    (let ((inherited-collection (org-persist--get-collection inherit associated))
          new-collection)
      (unless (member container (plist-get inherited-collection :container))
        (setq new-collection
              (plist-put (copy-sequence inherited-collection) :container
                         (cons container (plist-get inherited-collection :container))))
        (org-persist--remove-from-index inherited-collection)
        (org-persist--add-to-index new-collection))))
  (let ((collection (org-persist--get-collection container associated misc)))
    (when (and expiry (not inherit))
      (when expiry (plist-put collection :expiry expiry))))
  (when (or (bufferp associated) (bufferp (plist-get associated :buffer)))
    (with-current-buffer (if (bufferp associated)
                             associated
                           (plist-get associated :buffer))
      (add-hook 'kill-buffer-hook #'org-persist-write-all-buffer nil 'local)))
  (when write-immediately (org-persist-write container associated)))

(cl-defun org-persist-unregister (container &optional associated &key remove-related)
  "Unregister CONTAINER in ASSOCIATED to be persistent.
When ASSOCIATED is `all', unregister CONTAINER everywhere.
When REMOVE-RELATED is non-nil, remove all the containers stored with
the CONTAINER as well."
  (unless org-persist--index (org-persist--load-index))
  (setq container (org-persist--normalize-container container))
  (if (eq associated 'all)
      (mapc (lambda (collection)
              (when (member container (plist-get collection :container))
                (org-persist-unregister container (plist-get collection :associated) :remove-related remove-related)))
            org-persist--index)
    (setq associated (org-persist--normalize-associated associated))
    (let ((collection (org-persist--find-index `(:container ,container :associated ,associated))))
      (when collection
        (if (or remove-related (= (length (plist-get collection :container)) 1))
            (org-persist--remove-from-index collection)
          (plist-put collection :container
                     (remove container (plist-get collection :container)))
          (org-persist--add-to-index collection))))))

(defvar org-persist--write-cache (make-hash-table :test #'equal)
  "Hash table storing as-written data objects.

This data is used to avoid reading the data multiple times.")
(cl-defun org-persist-read (container &optional associated hash-must-match load &key read-related)
  "Restore CONTAINER data for ASSOCIATED.
When HASH-MUST-MATCH is non-nil, do not restore data if hash for
ASSOCIATED file or buffer does not match.

ASSOCIATED can be a plist, a buffer, or a string.
A buffer is treated as (:buffer ASSOCIATED).
A string is treated as (:file ASSOCIATED).

When LOAD is non-nil, load the data instead of reading.

When READ-RELATED is non-nil, return the data stored alongside with
CONTAINER as well.  For example:

    (let ((info \"test\"))
      (org-persist-register
        \\=`(\"My data\" (elisp-data ,info))
        nil :write-immediately t))
    (org-persist-read \"My data\") ; => \"My data\"
    (org-persist-read \"My data\" nil nil nil
                      :read-related t) ; => (\"My data\" \"test\")"
  (unless org-persist--index (org-persist--load-index))
  (setq associated (org-persist--normalize-associated associated))
  (setq container (org-persist--normalize-container container))
  (let* ((collection (org-persist--find-index `(:container ,container :associated ,associated)))
         (persist-file
          (when collection
            (org-file-name-concat
             org-persist-directory
             (plist-get collection :persist-file))))
         (data nil))
    (when (and collection
               (or (not (plist-get collection :expiry)) ; current session
                   (not (org-persist--gc-expired-p
                       (plist-get collection :expiry) collection)))
               (or (not hash-must-match)
                   (and (plist-get associated :hash)
                        (equal (plist-get associated :hash)
                               (plist-get (plist-get collection :associated) :hash))))
               (or (file-exists-p persist-file)
                   ;; Attempt to write data if it is not yet written.
                   (progn
                     (org-persist-write container associated 'no-read)
                     (file-exists-p persist-file))))
      (unless (seq-find (lambda (v)
                          (run-hook-with-args-until-success 'org-persist-before-read-hook v associated))
                        (plist-get collection :container))
        (setq data (or (gethash persist-file org-persist--write-cache)
                       (org-persist--read-elisp-file persist-file)))
        (when data
          (cl-loop for c in (plist-get collection :container)
                   with result = nil
                   do
                   (when (or read-related
                             (equal c container)
                             (member c container))
                     (if load
                         (push (org-persist-load:generic c (alist-get c data nil nil #'equal) collection) result)
                       (push (org-persist-read:generic c (alist-get c data nil nil #'equal) collection) result)))
                   (run-hook-with-args 'org-persist-after-read-hook c associated)
                   finally return (if (= 1 (length result)) (car result) (nreverse result))))))))

(cl-defun org-persist-load (container &optional associated hash-must-match &key read-related)
  "Load CONTAINER data for ASSOCIATED.
The arguments CONTAINER, ASSOCIATED, HASH-MUST-MATCH, and READ-RELATED
have the same meaning as in `org-persist-read'."
  (org-persist-read container associated hash-must-match t :read-related read-related))

(defun org-persist-load-all (&optional associated)
  "Restore all the persistent data associated with ASSOCIATED."
  (unless org-persist--index (org-persist--load-index))
  (setq associated (org-persist--normalize-associated associated))
  (let (all-containers)
    (dolist (collection org-persist--index)
      (when collection
        (cl-pushnew (plist-get collection :container) all-containers :test #'equal)))
    (dolist (container all-containers)
      (condition-case err
          (org-persist-load container associated t)
        (error
         (message "%s. Deleting bad index entry." err)
         (org-persist--remove-from-index (org-persist--find-index `(:container ,container :associated ,associated)))
         nil)))))

(defun org-persist-load-all-buffer ()
  "Call `org-persist-load-all' in current buffer."
  (org-persist-load-all (current-buffer)))

(defun org-persist-write (container &optional associated ignore-return)
  "Save CONTAINER according to ASSOCIATED.
ASSOCIATED can be a plist, a buffer, or a string.
A buffer is treated as (:buffer ASSOCIATED).
A string is treated as (:file ASSOCIATED).
The return value is nil when writing fails and the written value (as
returned by `org-persist-read') on success.
When IGNORE-RETURN is non-nil, just return t on success without calling
`org-persist-read'."
  (setq associated (org-persist--normalize-associated associated))
  ;; Update hash
  (when (and (plist-get associated :file)
             (plist-get associated :hash)
             (get-file-buffer (plist-get associated :file)))
    (setq associated (org-persist--normalize-associated (get-file-buffer (plist-get associated :file)))))
  (let ((collection (org-persist--get-collection container associated)))
    (setf collection (plist-put collection :associated associated))
    (unless (or
             ;; Prevent data leakage from encrypted files.
             ;; We do it in somewhat paranoid manner and do not
             ;; allow anything related to encrypted files to be
             ;; written.
             (and (plist-get associated :file)
                  (string-match-p epa-file-name-regexp (plist-get associated :file)))
             (seq-find (lambda (v)
                         (run-hook-with-args-until-success 'org-persist-before-write-hook v associated))
                       (plist-get collection :container)))
      (when (or (file-exists-p org-persist-directory) (org-persist--save-index))
        (let ((file (org-file-name-concat org-persist-directory (plist-get collection :persist-file)))
              (data (mapcar (lambda (c) (cons c (org-persist-write:generic c collection)))
                            (plist-get collection :container))))
          (puthash file data org-persist--write-cache)
          (org-persist--write-elisp-file file data)
          (or ignore-return (org-persist-read container associated)))))))

(defun org-persist-write-all (&optional associated)
  "Save all the persistent data.
When ASSOCIATED is non-nil, only save the matching data."
  (unless org-persist--index (org-persist--load-index))
  (setq associated (org-persist--normalize-associated associated))
  (if
      (and (equal 1 (length org-persist--index))
           ;; The single collection only contains a single container
           ;; in the container list.
           (equal 1 (length (plist-get (car org-persist--index) :container)))
           ;; The container is an `index' container.
           (eq 'index (caar (plist-get (car org-persist--index) :container)))
           (or (not (file-exists-p org-persist-directory))
               (org-directory-empty-p org-persist-directory)))
      ;; Do not write anything, and clear up `org-persist-directory' to reduce
      ;; clutter.
      (when (and (file-exists-p org-persist-directory)
                 (org-directory-empty-p org-persist-directory))
        (delete-directory org-persist-directory))
    ;; Write the data.
    (let (all-containers)
      (dolist (collection org-persist--index)
        (if associated
            (when collection
              (cl-pushnew (plist-get collection :container) all-containers :test #'equal))
          (condition-case err
              (org-persist-write (plist-get collection :container) (plist-get collection :associated) t)
            (error
             (message "%s. Deleting bad index entry." err)
             (org-persist--remove-from-index collection)
             nil))))
      (dolist (container all-containers)
        (let ((collection (org-persist--find-index `(:container ,container :associated ,associated))))
          (when collection
            (condition-case err
                (org-persist-write container associated t)
              (error
               (message "%s. Deleting bad index entry." err)
               (org-persist--remove-from-index collection)
               nil))))))))

(defun org-persist-write-all-buffer ()
  "Call `org-persist-write-all' in current buffer.
Do nothing in an indirect buffer."
  (unless (buffer-base-buffer (current-buffer))
    (org-persist-write-all (current-buffer))))

(defalias 'org-persist-gc:elisp #'ignore)
(defalias 'org-persist-gc:index #'ignore)
(defalias 'org-persist-gc:elisp-data #'ignore)
(defalias 'org-persist-gc:version #'ignore)
(defalias 'org-persist-gc:file #'ignore)
(defalias 'org-persist-gc:url #'ignore)

(defun org-persist--gc-persist-file (persist-file)
  "Garbage collect PERSIST-FILE."
  (when (file-exists-p persist-file)
    (delete-file persist-file)
    (when (org-directory-empty-p (file-name-directory persist-file))
      (delete-directory (file-name-directory persist-file)))))

(defmacro org-persist-associated-files:generic (container collection)
  "List associated files in `org-persist-directory' of CONTAINER in COLLECTION."
  `(let* ((c (org-persist--normalize-container ,container))
          (assocf-func-symbol (intern (format "org-persist-associated-files:%s" (car c)))))
     (if (fboundp assocf-func-symbol)
         (funcall assocf-func-symbol c ,collection)
       (error "org-persist: Read function %s not defined"
              assocf-func-symbol))))

(defalias 'org-persist-associated-files:elisp #'ignore)
(defalias 'org-persist-associated-files:index #'ignore)
(defalias 'org-persist-associated-files:elisp-data #'ignore)
(defalias 'org-persist-associated-files:version #'ignore)

(defun org-persist-associated-files:file (container collection)
  "List file CONTAINER associated files of COLLECTION in `org-persist-directory'."
  (let ((file (org-persist-read container (plist-get collection :associated))))
    (when (and file (file-exists-p file))
      (list file))))

(defun org-persist-associated-files:url (container collection)
  "List url CONTAINER associated files of COLLECTION in `org-persist-directory'."
  (let ((file (org-persist-read container (plist-get collection :associated))))
    (when (file-exists-p file)
      (list file))))

(defun org-persist--refresh-gc-lock ()
  "Refresh session timestamp in `org-persist-gc-lock-file'.
Remove expired sessions timestamps."
  (let* ((file (org-file-name-concat org-persist-directory org-persist-gc-lock-file))
         (alist (when (file-exists-p file) (org-persist--read-elisp-file file)))
         new-alist)
    (setf (alist-get before-init-time alist nil nil #'equal)
          (current-time))
    (dolist (record alist)
      (when (< (- (float-time (cdr record)) (float-time (current-time)))
               org-persist-gc-lock-expiry)
        (push record new-alist)))
    (org-persist--write-elisp-file file new-alist)))

(defun org-persist--gc-orphan-p ()
  "Return non-nil, when orphan files should be garbage-collected.
Remove current sessions from `org-persist-gc-lock-file'."
  (let* ((file (org-file-name-concat org-persist-directory org-persist-gc-lock-file))
         (alist (when (file-exists-p file) (org-persist--read-elisp-file file))))
    (setq alist (org-assoc-delete-all before-init-time alist))
    (org-persist--write-elisp-file file alist)
    ;; Only GC orphan files when there are no active sessions.
    (not alist)))

(defun org-persist-gc ()
  "Remove expired or unregistered containers and orphaned files.
Also, remove containers associated with non-existing files."
  (if org-persist--index
      (org-persist--merge-index-with-disk)
    (org-persist--load-index))
  (let (new-index
        (remote-files-num 0)
        (orphan-files
         (when (org-persist--gc-orphan-p) ; also removes current session from lock file.
           (delete (org-file-name-concat org-persist-directory org-persist-index-file)
                   (when (file-exists-p org-persist-directory)
                     (directory-files-recursively org-persist-directory ".+"))))))
    (dolist (collection org-persist--index)
      (let* ((file (plist-get (plist-get collection :associated) :file))
             (web-file (and file (string-match-p "\\`https?://" file)))
             (file-remote (when file (file-remote-p file)))
             (persist-file (when (plist-get collection :persist-file)
                             (org-file-name-concat
                              org-persist-directory
                              (plist-get collection :persist-file))))
             (expired? (org-persist--gc-expired-p
                        (plist-get collection :expiry) collection)))
        (when persist-file
          (setq orphan-files (delete persist-file orphan-files))
          (when (and file (not web-file))
            (when file-remote (cl-incf remote-files-num))
            (unless (if (not file-remote)
                        (file-exists-p file)
                      (pcase org-persist-remote-files
                        ('t t)
                        ('check-existence
                         (file-exists-p file))
                        ((pred numberp)
                         (< org-persist-remote-files remote-files-num))
                        (_ nil)))
              (setq expired? t)))
          (if expired?
              (org-persist--gc-persist-file persist-file)
            (push collection new-index)
            (dolist (container (plist-get collection :container))
              (dolist (associated-file
                       (org-persist-associated-files:generic
                        container collection))
                (setq orphan-files (delete associated-file orphan-files))))))))
    (mapc #'org-persist--gc-persist-file orphan-files)
    (setq org-persist--index (nreverse new-index))))

(defun org-persist-clear-storage-maybe ()
  "Clear `org-persist-directory' according to `org-persist--disable-when-emacs-Q'.

When `org-persist--disable-when-emacs-Q' is non-nil and Emacs is called with -Q
command line argument, `org-persist-directory' is created in potentially public
system temporary directory.  Remove everything upon existing Emacs in
such scenario."
  (when (and org-persist--disable-when-emacs-Q
             ;; FIXME: This is relying on undocumented fact that
             ;; Emacs sets `user-init-file' to nil when loaded with
             ;; "-Q" argument.
             (not user-init-file)
             (file-exists-p org-persist-directory))
    (delete-directory org-persist-directory 'recursive)))

;; Point to temp directory when `org-persist--disable-when-emacs-Q' is set.
(when (and org-persist--disable-when-emacs-Q
           ;; FIXME: This is relying on undocumented fact that
           ;; Emacs sets `user-init-file' to nil when loaded with
           ;; "-Q" argument.
           (not user-init-file))
  (setq org-persist-directory
        (make-temp-file "org-persist-" 'dir)))

;; Automatically write the data, but only when we have write access.
(when (org-persist--check-write-access org-persist-directory)
  (add-hook 'kill-emacs-hook #'org-persist-clear-storage-maybe) ; Run last.
  (add-hook 'kill-emacs-hook #'org-persist-write-all)
  ;; `org-persist-gc' should run before `org-persist-write-all'.
  ;; So we are adding the hook after `org-persist-write-all'.
  (add-hook 'kill-emacs-hook #'org-persist-gc))

(add-hook 'after-init-hook #'org-persist-load-all)

(defvar org-persist--refresh-gc-lock-timer nil
  "Timer used to refresh session timestamp in `org-persist-gc-lock-file'.")

(unless (and org-persist--disable-when-emacs-Q
             ;; FIXME: This is relying on undocumented fact that
             ;; Emacs sets `user-init-file' to nil when loaded with
             ;; "-Q" argument.
             (not user-init-file))
  (unless org-persist--refresh-gc-lock-timer
    (setq org-persist--refresh-gc-lock-timer
          (run-at-time nil org-persist-gc-lock-interval #'org-persist--refresh-gc-lock))))

(provide 'org-persist)

;;; org-persist.el ends here
