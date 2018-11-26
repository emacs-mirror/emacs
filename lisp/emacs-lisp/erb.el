;;; erb.el --- Emacs Regression Benchmarks -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Gemini Lasswell
;; Keywords: lisp, tools
;; Version: 0.1

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

;; ERB is a tool for automated benchmarking in Emacs Lisp.

;; ERB implements a user interface for building older versions of
;; Emacs, running the benchmark tasks in them, managing a database of
;; results, and presenting the results.

;; TODO define erb-hostname which defaults to system-name, and allow
;; changing it.
;;
;; TODO abbreviate commits to 12 characters or whatever.
;;
;; It would be nice to have filenames like:
;;    201811012312-47ccee220a49.eld
;;    201811021014-f05e930ca9ca.eld
;; because then they could be easily seen in date order in dired.
;; Or maybe it would be easy to extend dired to sort by date and
;; show the date.

;; For usage information, see ERB's info manual.
;;; Code:
(require 'ansi-color)
(require 'cl-lib)
(require 'cl-macs) ;; TODO eval-when-compile
(require 'erb-task)
(require 'find-func)
(require 'generator)
(require 'map)
(eval-when-compile (require 'pcase))
(require 'seq)
;; TODO (eval-when-compile (require 'subr-x))
(require 'subr-x)
(require 'thread)

(defgroup erb nil
  "ERB, the Emacs regression performance testing tool."
  :prefix "erb-"
  :group 'lisp)

'(defcustom erb-hostname-translation nil
  "Mapping from system names to machine names in ERB.
The keys of this alist should be system names as returned by
`system-name', and the values should be strings containing the
names to use for those systems in the benchmark results."
  :type (alist :key-type 'string :value-type 'string)
  :group 'erb
  :version "27.1")

(defconst erb-version "0.1")

;; TODO Alternatively, look in load-history?
(eval-and-compile
  (defvar erb-task-el-filename
    (expand-file-name
     "erb-task.el"
     (file-name-directory (or (bound-and-true-p byte-compile-current-file)
                              load-file-name
                              buffer-file-name)))
    "Location of erb.el (or erb.el.gz) for this installation of Emacs."))

;;; Buffer-local variables used by all ERB buffers

(defvar-local erb-suite-directory nil
  "Benchmark suite directory for the current `erb-mode' buffer.")

;; TODO allow a URL for the project, and create
;; a customize alist that points to a local clone.
;; If no local clone, clone it into a temp directory.
(defvar-local erb--config nil
  "Benchmark suite configuration for the current `erb-mode' buffer.")
(defvar-local erb--config-err nil
  "If non-nil, the error which occurred reading the benchmark configuration.")

;; TODO add a configurable cooldown time between building and benchmarking.
(defvar-local erb--machine-config nil
  "Machine configuration for the current `erb-mode' buffer.")
(defvar-local erb--machine-config-err nil
  "If non-nil, the error which occurred reading the machine configuration.")

;;; ERB directory configuration

;; TODO implement refusing to read newer configs
(defconst erb-default-config
  `((project-name
     "The name of the project."
     "GNU Emacs")
    (project-repo
     "The path to the git repository for the project to be benchmarked."
     "/path/to/your/project/git/repo")
    (benchmark-directory
     "The directory containing Lisp files declaring benchmark
tasks, relative to the project's git repository."
     "path/to/benchmark/tasks")
    (tags
     "A list of git tags or commits (as strings) to label on the
x-axis of benchmark plots."
     ("release-1.0" "release-2.0"))
    (erb-version
     "The ERB version which created this file."
     ,erb-version)))

(defun erb-initialize ()
  "Initialize `erb-suite-directory' to store benchmark results.
Write the default ERB configuration file to \"config.eld\" at the
top level of `erb-suite-directory'.  You should hand-edit
the file as desired."
  (interactive)
  ;; TODO prompt to overwrite if file exists
  (erb--write-formatted-alist-to-file
   erb-default-config
   (expand-file-name "config.eld" erb-suite-directory))
  (erb-summary-revert-buffer))

(defun erb--write-formatted-alist-to-file (alist filename)
  "Write ALIST to FILENAME in a human-friendly format.
Each element of ALIST should be (KEY DOC VALUE).  Write the ALIST
with DOC converted to comments so that when read back in by the
Lisp reader each element will become (KEY VALUE)."
  (let ((first t))
    (with-temp-file filename
      (let ((standard-output (current-buffer)))
        (princ "(")
        (pcase-dolist (`(,key ,doc ,value) alist)
          (if first
              (setq first nil)
            (princ "\n"))
          (princ "\n ;; ")
          (seq-doseq (char doc)
            (princ (if (eq char ?\n) "\n ;; " (string char))))
          (princ (format "\n (%s . %S)" key value)))
        (princ ")\n")))))

(defun erb--read-config (suite-dir)
  "Read the configuration file \"config.eld\" from SUITE-DIR."
  (let* ((filename (expand-file-name "config.eld" suite-dir)))
    (unless (file-readable-p filename)
      (error "%s has not been initialized to store benchmark results" suite-dir))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (read (current-buffer)))
    ;; TODO check that required keys are present,
    ;; and that it wasn't written by a newer version.
    ))

(defun erb--update-config-cache ()
  ;; TODO check alist structure, keys, value types?
  (setq erb--config-err nil)
  (setq erb--config
        (condition-case err
            (erb--read-config erb-suite-directory)
          (error (setq erb--config-err (format "%s" err))
                 nil)))
  (unless (or erb--config erb--config-err)
    (setq erb--config-err "Unable to read benchmark configuration")))

;;; Machine Configuration

(defconst erb-default-machine-config
  `((build-script
     "Where to look for the build script.  This may be an absolute
or relative path.  If it is a relative path, see the value
associated with `build-script-location'."
     "path/to/build-script")
    (build-script-location
     "If the value associated with `build-script' is a relative
path, look for the build script in the project directory if this
is `project', or in the ERB directory if this is `ERB'."
     ERB)
    (system-info-script
     "Where to look for the system info script.  This may be an
absolute or relative path.  If it is a relative path, see the
value associated with `system-info-script-location'."
     "path/to/system-info-script")
    (system-info-script-location
     "If the value associated with `system-info-script' is a
relative path, look for the system info script in the project
directory if this is `project', or in the ERB directory if this
is `ERB'."
     ERB)
    (simultaneous-builds
     "Number of builds to do at the same time." 1)
    (emacs-arguments
     "List of arguments to pass to the benchmarked Emacs executable."
     ("-Q" "--batch"))
    (erb-version
     "The ERB version which created this file."
     ,erb-version)))

(defun erb-initialize-machine-config ()
  "Write a machine configuration file for the current system.
Write the file to the `config' subdirectory of
`erb-suite-directory', as \"HOSTNAME.eld\"."
  (interactive)
  (unless erb--config
    (user-error "Use `erb-initialize' to create a benchmark configuration first"))
  ;; TODO prompt to overwrite if file exists
  (let* ((hostname (system-name))
         (filename (erb--machine-config-filename hostname))
         (machines-dir (expand-file-name "machines" erb-suite-directory)))
    (unless (file-directory-p machines-dir)
      (make-directory machines-dir))
    (erb--write-formatted-alist-to-file erb-default-machine-config filename))
  (erb-summary-revert-buffer)
  (erb-update-saved-machine-info))

(defun erb--read-machine-config (hostname)
  "Return the contents of the machine configuration file for HOSTNAME.
If the machine information file is empty or not present signal an
error."
  (let* ((filename (erb--machine-config-filename hostname))
         (config (when (file-readable-p filename)
                   (with-temp-buffer
                     (insert-file-contents filename)
                     (goto-char (point-min))
                     (read (current-buffer))))))
    (unless config
      (error "Machine configuration not found in \"%s\"" filename))
    config))

(defun erb--machine-config-filename (hostname)
  (thread-last erb-suite-directory
    (expand-file-name "machines")
    (expand-file-name (concat hostname ".eld"))))

(defun erb--update-machine-config-cache ()
  ;; TODO check alist structure, keys, value types?
  (setq erb--machine-config-err nil)
  (setq erb--machine-config
        (condition-case err
            (erb--read-machine-config (system-name))
          (error (setq erb--machine-config-err (format "%s" err))
                 nil)))
  (unless (or erb--machine-config erb--machine-config-err)
    (setq erb--machine-config-err
          "Unable to read machine configuration")))

(defun erb-update-saved-machine-info ()
  "Update the information ERB keeps on file about this machine.
Use this command to see operating system updates reflected in the
benchmark results report."
  ;; TODO prompt to confirm.
  ;; TODO allow adding a note (such as "Replaced hard drive with SSD.")
  ;; TODO save the configuration too? (emacs-arguments)
  (interactive)
  (erb--update-machine-config-cache)
  (when erb--machine-config-err
    (error "%s" erb--machine-config-err))
  (let* ((hostname (system-name))
         (info-file (erb--machine-info-file-name hostname))
         (old-info (erb--read-saved-machine-info hostname))
         (info (erb--get-this-machine-info
                hostname (erb--get-script-filename 'system-info))))
    (make-directory (file-name-directory info-file) t)
    (with-temp-file info-file
      (let ((standard-output (current-buffer)))
        (cl-prin1 (cons info old-info))
        (pp-buffer)))))

(defun erb--read-saved-machine-info (machine)
  "Read the saved list of system information about MACHINE.
Returns a list of alists, the most recent first."
  (let ((info-file (erb--machine-info-file-name machine)))
    (when (file-readable-p info-file)
      (with-temp-buffer
        (insert-file-contents info-file)
        (goto-char (point-min))
        (read (current-buffer))))))

(defun erb--machine-info-file-name (machine)
  (let* ((info-dir (thread-last erb-suite-directory
                     (expand-file-name "machines")
                     (expand-file-name "info")))
         (info-file (expand-file-name (format "%s.eld" machine)
                                      info-dir)))
    info-file))

;; TODO hostname no longer used
(defun erb--get-this-machine-info (_hostname system-info-script)
  "Return an alist of information about this machine.
Use strings for the informational keys of the alist, and include
a timestamp and the ERB version using keyword keys."
  (let ((lines (mapcar #'ansi-color-filter-apply
                       (process-lines system-info-script)))
        (machine-info `((:time . ,(truncate (time-to-seconds (current-time))))
                        (:erb-version . ,erb-version))))
    (dolist (line lines)
      (when (string-match "[A-Za-z]+: " line)
        (let ((key (substring line 0 (- (match-end 0) 2)))
              (value (substring line (match-end 0))))
          (push (cons key value) machine-info))))
    machine-info))

(defun erb--get-script-filename (script-type)
  "Locate the build script for the machine.
It could be at some absolute path, in the project repo, or in the
ERB directory.  The script type can be either `build' or
`system-info'."
  (let ((script-location (map-elt erb--machine-config
                                  (intern (format "%s-script-location"
                                                  script-type))))
        (script (map-elt erb--machine-config
                         (intern (format "%s-script" script-type)))))
    (if (file-name-absolute-p script)
        script
      (expand-file-name
       script
       (cl-case script-location
         ((ERB)     erb-suite-directory)
         ((project) (map-elt erb--config 'project-repo))
         (t (error
             (concat
              "In the ERB configuration, the value of `%s-script-location' "
              "should be either `project' or `ERB'")
             script-type)))))))

;;; Benchmark tasks and their metadata

(cl-defstruct erb--metadata
  name      ; Name of task defined by erb-deftask.
  filename  ; Relative pathname of file in which task was defined.
  version   ; Version of this task from keyword plist in definition.
  rev-list  ; Arguments to git rev-list.
  discard-first-sample ; Flag from task definition.
  documentation  ; Docstring from task definition.
  special   ; From keyword plist in definition.
  )

(defvar erb--benchmark-tasks nil
  "Information about the benchmark tasks found in the project.
A list of `erb--metadata' structures.")

(defun erb--read-benchmark-metadata ()
  "Extract benchmark task metadata from the project.
Save the results in `erb--benchmark-tasks' as an alist mapping
task names to `erb--metadata' structures.  This works by
evaluating the code in the benchmark task files in the project,
so it will have whatever side effects are caused by that code.
As a side effect, and by way of partial cleanup, delete all
defined benchmark tasks."
  ;; TODO error handling
  (let ((benchmark-task-files (directory-files-recursively
                               (erb--benchmark-dir) "\\-tasks.el$")))
    (setq erb--benchmark-tasks nil)
    (erb-delete-all-tasks)
    (dolist (filename benchmark-task-files)
      (with-temp-buffer
        (insert-file-contents filename)
        (eval-buffer))
      (dolist (symbol (erb-task--all-symbols))
        (let ((task (erb-task--get-task symbol)))
          (push (apply #'make-erb--metadata
                       (append
                        `(:filename ,filename :name ,symbol)
                        `(:documentation ,(erb-task--documentation task))
                        (erb-task--key-plist task)))
                erb--benchmark-tasks)))
      (erb-delete-all-tasks)))
  erb--benchmark-tasks)

(defun erb--benchmark-dir ()
  (map-let (project-repo benchmark-directory) erb--config
    (expand-file-name benchmark-directory project-repo)))

;;; Summary mode definition

(defvar erb-summary-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (set-keymap-parent map button-buffer-map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "C" 'erb-initialize)
    (define-key map "M" 'erb-initialize-machine-config)
    (define-key map "U" 'erb-update-saved-machine-info)
    (define-key map "r" 'erb-summary-run)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'mouse-select-window)

    map)
  "Local keymap for `erb-summary-mode' buffers.")

(define-derived-mode erb-mode special-mode "ERB-base"
  "Parent major mode from which ERB major modes inherit.

ERB is documented in info node `(erb)'."
  :group 'erb
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq show-trailing-whitespace nil)
  (setq-local line-move-visual t)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  ;; (add-hook 'post-command-hook #'magit-section-update-highlight t t)
  ;; (setq-local redisplay-highlight-region-function 'magit-highlight-region)
  ;; (setq-local redisplay-unhighlight-region-function 'magit-unhighlight-region)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (setq-local erb-suite-directory default-directory))

(define-derived-mode erb-summary-mode erb-mode "ERB"
  "Summarize information contained in an ERB benchmark suite directory.
\\<erb-summary-mode-map>
ERB is documented in info node `(erb)'."
  :group 'erb
  (setq-local revert-buffer-function #'erb-summary-revert-buffer))

(defun erb-summary-generate-new-buffer ()
  (let* ((name (format "*ERB: %s*" (file-name-nondirectory
                                    (directory-file-name default-directory))))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (erb-summary-mode)
      (add-to-list 'uniquify-list-buffers-directory-modes 'erb-summary-mode)
      (setq erb-suite-directory default-directory)
      (setq list-buffers-directory (abbreviate-file-name default-directory)))
    buffer))

;;;###autoload
(defun erb-summary ()
  "Show an overview of the benchmark suite in the current directory."
  ;; TODO prompt for directory with prefix argument.
  ;; OR look for config.eld in current directory and prompt
  ;; if not found
  (interactive)
  (let* ((dir default-directory)
         (buffer (or (seq-find (lambda (buf)
                                 (and (eq major-mode 'erb-summary-mode)
                                      (with-current-buffer buf
                                        (equal dir erb-suite-directory))))
                               (buffer-list))
                     (erb-summary-generate-new-buffer))))
    (switch-to-buffer buffer)
    (erb-summary-revert-buffer buffer)))

(defun erb-summary-revert-buffer (&rest _ignored)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (erb-summary--display-configuration)
    (when erb--config
      (erb-summary--display-machine-configuration)
      (erb-summary--display-results))
    (set-buffer-modified-p nil)))

(defun erb-summary--display-configuration ()
  (erb--update-config-cache)
  (if erb--config
      (let ((fmt "  %-30s%s\n"))
        (map-let (project-repo benchmark-directory) erb--config
            (insert
             (format "Benchmark configuration in %s:\n"
                     (abbreviate-file-name
                      (expand-file-name "config.eld" erb-suite-directory)))
             (format fmt "Project location:"
                     (abbreviate-file-name project-repo))
             (format fmt "Benchmark tasks subdirectory:" benchmark-directory)

             ;; TODO make this a link
             "\n  Edit Configuration\n\n")))
    (insert
     (substitute-command-keys
      (if (file-readable-p (expand-file-name "config.eld" erb-suite-directory))
          (format "Could not read the `config.eld' file in %s: %s\n"
                  erb-suite-directory erb--config-err)
        (format "No file named `config.eld' found in %s\n"
                erb-suite-directory)))
     (substitute-command-keys
      "\nUse `erb-initialize' to create a new file `config.eld'
containing a sample configuration for benchmarking.\n")))
  erb--config)

(defun erb-summary--display-machine-configuration ()
  ;; TODO it would be nice to use remote machine if benchmark
  ;; directory is remote
  (erb--update-machine-config-cache)
  (if erb--machine-config
      (let ((fmt "  %-30s%s\n"))
        (map-let (build-script build-script-location cpu-cores
                  system-info-script system-info-script-location
                  emacs-arguments)
            erb--machine-config
          (insert
           (substitute-command-keys
            (format "Configuration for `%s':\n" (system-name)))
           (format fmt "Build script:" build-script)
           (if (file-name-absolute-p build-script)
               ""
             (format fmt "Build script location:"
                     (cl-case build-script-location
                       ((ERB) "In the benchmark directory")
                       ((project) "In the project"))))
           (format fmt "System info script:" system-info-script)
           (if (file-name-absolute-p system-info-script)
               ""
             (format fmt "System info script location:"
                     (cl-case system-info-script-location
                       ((ERB) "In the benchmark directory")
                       ((project) "In the project"))))
           (format fmt "CPU cores to use:" cpu-cores)
           (format fmt "Emacs arguments:"
                   (mapconcat #'identity emacs-arguments " "))
           "\n  Change build script\n\n")))

    (insert
     (substitute-command-keys
      (format "Could not read `%s': %s\n"
              (erb--machine-config-filename (system-name))
              erb--machine-config-err))
     (substitute-command-keys
      (format "\nUse `erb-initialize-machine-config' to create a
new file `config/%s.eld' containing configuration for this
machine.\n\n" (system-name))))))

(defun erb-summary--display-results ()
  (let* ((machines-dirs (erb--machine-results-dirs)))
    (if machines-dirs
        (dolist (machine-dir machines-dirs)
          (let* ((machine-name (file-name-nondirectory machine-dir))
                 (runs-dir (expand-file-name "measurements" machine-dir))
                 (runs (directory-files runs-dir nil ".+\\.eld$" ))
                 (failed-runs-dir (erb--failed-runs-dir machine-name))
                 (failed-runs
                  (ignore-errors
                    (directory-files failed-runs-dir nil ".+\\.eld$")))
                 (failed-builds-dir (erb--failed-builds-dir machine-name))
                 (failed-builds
                  (ignore-errors
                    (directory-files failed-builds-dir nil ".+\\.log$"))))
            (insert (substitute-command-keys
                     (format "Results for `%s':\n" machine-name)))
            (insert (format "  %-25s%5d\n  %-25s%5d\n  %-25s%5d\n\n"
                            "Commits benchmarked:" (length runs)
                            "Commits with errors:" (length failed-runs)
                            "Build failures:" (length failed-builds)))))
      (insert (substitute-command-keys
               "No results yet.  Use `r' to start running benchmarks.\n")))))

;;; State variables for the benchmark runner

(cl-defstruct erb--job
  buffer
  commits)

(defvar erb--job (thread-make-message)
  "This contains all the information needed about what benchmark job to run.
It is created by `erb-run-start' and cleared when the benchmark job is
finished by `erb--benchmark-control-func'.

`erb-run-cancel' sets this to the symbol `cancel', which will
cause ERB's threads to stop any job they are working on and clean
up.")

;;; Run mode definition

(defcustom erb-run-refresh-seconds 0.2
  "Delay between updates of `erb-run' buffers."
  :type 'number
  :group 'erb
  :version "27.1")

;; Options settable in the erb-run-mode buffer.
;; TODO "settable"
(defvar-local erb-run--commit-range "emacs-25.1..bd013a448b")
(defvar-local erb-run--number-to-select 8)
(defvar-local erb-run--skip-building-previous-failures t)

(defvar erb-run-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (set-keymap-parent map button-buffer-map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "s" 'erb-run-start)
    (define-key map "c" 'erb-run-cancel)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'mouse-select-window)

    map)
  "Local keymap for `erb-run-mode' buffers.")

(define-derived-mode erb-run-mode erb-mode "ERB-run"
  "Mode for configuring and running benchmarks.
\\<erb-run-mode-map>
ERB is documented in info node `(erb)'."
  :group 'erb
  (setq-local revert-buffer-function #'erb-run-revert-buffer))

;; TODO make only one buffer? Since there is ony one set of worker threads
;; What happens if you try to switch directory while a job is running?
(defun erb-run-generate-new-buffer ()
  (let* ((name (format "*ERB-run: %s*" (file-name-nondirectory
                                        (directory-file-name default-directory))))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (erb-run-mode)
      (run-at-time erb-run-refresh-seconds nil
                   #'erb-run--timer-func buffer)
      (add-to-list 'uniquify-list-buffers-directory-modes 'erb-run-mode))
    buffer))

;; TODO autoload is just for now
;;;###autoload
(defun erb-summary-run ()
  "Switch to or create an `erb-run-mode' buffer for running benchmarks."
  (interactive)
  (let* ((dir default-directory)
         (buffer (or (seq-find (lambda (buf)
                                 (and (eq major-mode 'erb-run-mode)
                                      (with-current-buffer buf
                                        (equal dir erb-suite-directory))))
                               (buffer-list))
                     (erb-run-generate-new-buffer))))
    (switch-to-buffer buffer)
    (erb-run-revert-buffer buffer)))

(defun erb-run-revert-buffer (&rest _ignored)
  ;; TODO put all these status variables into a structure or alist so
  ;; they can be copied, and then only update the buffer if something
  ;; has changed.
  ;; TODO save and restore cursor position.
  (let ((inhibit-read-only t))
    (erase-buffer)
    (erb--update-config-cache)
    (if (not erb--config)
        (insert (format "Error reading `config.eld': \n  %s\n" erb--config-err))
      (map-let (project-repo) erb--config
        (insert
         (format "Project: %s\n" project-repo)
         (format "Commit range: %s\n" erb-run--commit-range)
         (format "Commits in range: %s\n"
                 (if-let ((count (erb--vc-get-commit-range-count
                                  erb-run--commit-range)))
                     count "Version control error"))
         "\n"

         (format "Number to select: %s\n" (if erb-run--number-to-select
                                              erb-run--number-to-select "All"))
         (format "Skip building previous failures: %s\n"
                 (if erb-run--skip-building-previous-failures "Yes" "No"))
         "\n"

         (let* ((done (length (erb--status 'finished)))
                (built (length (erb--status 'built)))
                (commits (length (erb--status 'commits)))
                (failed-builds (length (erb--status 'failed-builds)))
                (failed-runs (length (erb--status 'failed-runs))))
           (concat
            (format "Built: %s\n" (if (> built 0) built ""))
            (format "Benchmarked: %s\n" (if (> done 0) done ""))
            (format "Build Failures: %s\n" (if (erb--status 'state)
                                               failed-builds ""))
            (format "Run Failures: %s\n" (if (erb--status 'state)
                                             failed-runs ""))
            (format "Total: %s/%s\n" done commits)))
         "\n"

         (format "Started at: %s\n"
                 (if (erb--status 'start-time)
                     (format-time-string "%Y-%m-%d %T%z" (erb--status 'start-time))
                   ""))
         ;; TODO Building and Benchmarking, cycle through 0 and 5 .'s
         (cl-case (erb--status 'state)
           (building (concat "Building." (erb--dots)))
           (benchmarking (concat "Benchmarking." (erb--dots)))
           ((nil) "Ready.")
           (done "Finished.")
           (cancelled "Cancelled.")
           (t (format "State: %s" (erb--status 'state))))

         (format (if (eq (erb--status 'state) 'cancelled)
                     "\nCancelled at: %s\n"
                   "\nFinished at: %s\n")
                 (if (erb--status 'stop-time)
                     (format-time-string "%Y-%m-%d %T%z" (erb--status 'stop-time))
                   ""))))

      ;; TODO make these all buttons which go to the WIP buffer
      (unless (null (erb--status 'state))
        (insert "\n")
        (dolist (commit (erb--status 'commits))
          (insert
           (cond
            ((memq commit (erb--status 'finished))           ".")
            ((memq commit (erb--status 'building))           "B")
            ((memq commit (erb--status 'failed-runs))        "F")
            ((memq commit (erb--status 'failed-builds))      "E")
            ((memq commit (erb--status 'benchmarking))       "R")
            ((memq commit (erb--status 'built))              "b")
            (t "-"))))
        (insert "\n")))

    (set-buffer-modified-p nil)))


(defvar erb--status-dot-count 0)
(defvar erb--status-last-update  (time-to-seconds (current-time)))
(defvar erb--status-interval 1.0)
(defconst erb--status-dot-max 5)
(defun erb--dots ()
  (prog1
      (make-string erb--status-dot-count ?.)
    (when (> (- (time-to-seconds (current-time)) erb--status-last-update)
             erb--status-interval)
      (setq erb--status-dot-count (% (1+ erb--status-dot-count)
                                     erb--status-dot-max)
            erb--status-last-update (time-to-seconds (current-time))))))

(defun erb-run--timer-func (buffer)
  "Revert BUFFER and set a timer to do it again."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (revert-buffer))
    (run-at-time erb-run-refresh-seconds nil
                 #'erb-run--timer-func buffer)))

;;; Run mode commands

(defun erb-run-start ()
  "Start running benchmarks."
  (interactive)
  (when (thread-message-value erb--job)
    (user-error "Benchmarks are already running"))

  (unless erb-run--commit-range
    (user-error "Choose a commit or range of commits to benchmark"))

  (erb--update-config-cache)
  (unless erb--config
    (user-error "Error reading benchmark configuration: %s" erb--config-err))
  (erb--update-machine-config-cache)
  (unless erb--config
    (user-error "Error reading machine configuration: %s"
                erb--machine-config-err))

  (erb--start-benchmark-controller-thread)
  (erb--adjust-builder-threads)
  (erb--read-benchmark-metadata)

  (erb--clear-status)
  (thread-message-send
   erb--job
   (make-erb--job :commits (erb--vc-get-commits erb-run--commit-range
                                                erb-run--number-to-select)
                  :buffer (current-buffer))))

(defun erb-run-cancel ()
  "Stop running benchmarks."
  (interactive)
  (thread-message-send erb--job 'cancel))

(defun erb--cancel-now-p ()
  (eq (thread-message-value erb--job) 'cancel))

;;; The benchmark runner
;;;; Controller thread

(defvar erb--unbuilt-commits (thread-make-queue nil 'fifo)
  "A thread-safe queue of commits waiting to be built.")
(defvar erb--built-commits (thread-make-queue nil 'fifo)
  "A thread-safe queue of commits which have been built.")

(defvar erb--benchmark-controller nil)

(defun erb--start-benchmark-controller-thread ()
  "Start the benchmark controller thread if it is not already started."
  (unless erb--benchmark-controller
    (setq erb--benchmark-controller
          (make-thread #'erb--benchmark-control-func "ERB control"))))

(defun erb--benchmark-control-func ()
  "Process benchmark jobs.
Watch for incoming jobs arriving by a thread-safe message in
`erb--job'.  When a job becomes available, build and
benchmark all the commits and then clear the message."
  ;; If erb--job is set to the symbol `stop',
  ;; exit.  This is meant for development and debugging.
  (catch 'stop
    (while t
      (condition-case-unless-debug err
          (let* ((job (thread-message-wait erb--job))
                 (commits (when (erb--job-p job) (erb--job-commits job)))
                 (count (length commits))
                 (runbuf (when (erb--job-p job) (erb--job-buffer job)))
                 builds)
            (when (eq job 'stop)
              (message "ERB control thread stopping")
              (setq erb--benchmark-controller nil)
              (thread-message-cancel erb--job)
              (throw 'stop nil))

            (catch 'cancelled
              (unless (eq job 'cancel)
                (with-current-buffer runbuf
                  ;; First give all the commits to the builder thread(s).
                  (erb--status-set 'start-time (current-time))
                  (erb--status-set 'state 'building)
                  (erb--status-set 'commits commits)
                  (dolist (commit commits)
                    (erb--status-add commit 'waiting-to-build)
                    (thread-queue-put commit erb--unbuilt-commits))

                  ;; Collect all the build results, to make sure they
                  ;; are all finished before benchmarking starts.
                  (while (> count 0)
                    (let ((build (thread-queue-get erb--built-commits)))
                      (push build builds))
                    (cl-decf count))

                  ;; Reverse the list of finished builds to make the
                  ;; benchmarking order make a little more sense to the
                  ;; user watching the progress indicator.
                  (setq builds (nreverse builds))

                  ;; TODO customizable processor cooldown delay
                  ;; before running benchmarks.

                  ;; Then benchmark the build results, one at a time.
                  (erb--status-set 'state 'benchmarking)
                  (pcase-dolist (`(,commit ,result) builds)
                    (when (erb--cancel-now-p)
                      (throw 'cancelled nil))

                    (when result
                      (message "Benchmarking %s" commit)
                      (erb--status-set 'benchmarking (list commit))
                      (erb--cache-commit-time commit)
                      (erb--benchmark-one-commit commit result)
                      (erb--status-add commit 'finished)
                      (with-demoted-errors (delete-directory result t))))
                  (erb--status-set 'benchmarking nil))))

            (erb--status-set 'stop-time (current-time))
            (erb--status-set 'state (if (erb--cancel-now-p)
                                        'cancelled 'done))
            (thread-message-cancel erb--job)
            '(pcase-dolist (`(,_ ,result) builds)
              (when result
                (ignore-errors (delete-directory result t)))))
        ((error quit)
         (message "Error in ERB benchmark control thread: %s" err)
         (thread-message-cancel erb--job))))))

;;;; Builder threads

(defvar erb--builders 0
  "The number of threads which have been created to run builds.")
(defvar erb--builder-number 0
  "Used to make a unique identifier for each ERB build thread.
For debugging.")

(defun erb--adjust-builder-threads ()
  "Create the desired number of commit-building threads.
Get the number from the machine configuration.  If there are too
many threads already running, tell the extra ones to stop."
  (map-let (cpu-cores) erb--machine-config

    (unless (natnump cpu-cores)
      (user-error "The value of `cpu-cores' in the configuration for `%s'
must be a positive integer" (system-name)))

    (while (< erb--builders cpu-cores)
      (make-thread #'erb--builder-func
                   (format "ERB build %s" erb--builder-number))
      (cl-incf erb--builder-number)
      (cl-incf erb--builders))

    (while (> erb--builders cpu-cores)
      (thread-queue-put 'stop erb--unbuilt-commits)
      (cl-decf erb--builders))))

(defun erb--builder-func ()
  "Build commits from `erb--unbuilt-commits'."
  (catch 'stop
    (while t
      (condition-case-unless-debug err
          (let ((commit (thread-queue-get erb--unbuilt-commits))
                build-result)
            (when (eq commit 'stop)
              (message "ERB builder thread stopping")
              (throw 'stop nil))

            (erb--status-remove commit 'waiting-to-build)
            (erb--status-add commit 'building)

            (unwind-protect
                (let ((job (thread-message-value erb--job)))
                  (unless (eq job 'cancel)
                    (with-current-buffer (erb--job-buffer job)
                      (setq build-result (erb--build commit)))))

              (erb--status-remove commit 'building)
              (if build-result
                  (erb--status-add commit 'built)
                (erb--status-add commit 'failed-builds))
              (thread-queue-put (list commit build-result)
                                erb--built-commits)))
        ;; TODO in the event of an error, need to put commit on built-commmits.
        ((error quit) (message "Error in ERB benchmark build thread: %s" err))))))

(defun erb--build (commit)
  "Build Emacs from COMMIT.
Run the build in an asynchonous process in a temporary directory.
Save the directory name if the build is successful.  If the build
fails, save the output of the build script in the file COMMIT.log
in the results/MACHINE/failed-builds directory of
`erb-suite-directory'."
  ;; TODO make temp file on same machine as build script
  (let* ((temp-dir (file-name-as-directory (make-temp-file "erb" t)))
         (default-directory temp-dir)
         (name (format "ERB-build-%s" commit))
         (outbuf (generate-new-buffer name))
         (build-script (erb--get-script-filename 'build))
         process success)

    (unwind-protect
        (unless (and erb-run--skip-building-previous-failures
                     (erb--failure-log-exists-p commit))
          (map-let (project-repo) erb--config
            (setq project-repo (expand-file-name project-repo))
            (setq process
                  (condition-case _err
                      (start-file-process name outbuf build-script project-repo
                                          commit)
                    ((error quit) nil)))
            (if (null process)
                (progn
                  (message "Failed to start build process for commit `%s'"
                           commit)
                  (erb-run--record-failure commit "Failed to start build process"))
              (catch 'quit
                (while (process-live-p process)
                  ;; TODO narrow conditions to repro the EBADF heisenbug
                  ;; at process.c 5510.  Has happened whether passing
                  ;; process or nil to accept-process-output.  Attempt
                  ;; at standalone code sample in emacs/misc/apo.el, but
                  ;; it won't repro.
                  (accept-process-output nil 0.5)
                  (when (erb--cancel-now-p)
                    (delete-process process)
                    (throw 'quit nil)))
                (if (= (process-exit-status process) 0)
                    (progn
                      (setq success temp-dir)
                      (erb-run--remove-old-failure commit))
                  (message "Building commit `%s' failed" commit)
                  (erb-run--record-failure commit outbuf))))))
      (unless success
        ;; Delete temp directory.
        (delete-directory temp-dir t))
      (kill-buffer outbuf))
    success))

(defun erb-run--record-failure (commit explanation)
  "Record a failed build of COMMIT.
EXPLANATION may be a string containing an error message or a
buffer containing a log of the failed build.  Write EXPLANATION
to the file COMMIT.log in the results/MACHINE/failed-builds
directory of `erb-suite-directory', where MACHINE is the
current system."
  (let ((failures-dir (erb--failed-builds-dir))
        (text (if (stringp explanation)
                  (concat explanation "\n")
                (with-current-buffer explanation
                  (if (= (point-min) (point-max))
                      "Build failed without producing any output\n"
                    (buffer-string))))))
    (make-directory failures-dir t)
    (with-temp-file (erb-run--failure-log-file-name commit)
      (insert text))))

(defun erb--failure-log-exists-p (commit)
  "Return non-nil if a build failure log exists for COMMIT."
  (file-readable-p (erb-run--failure-log-file-name commit)))

(defun erb-run--remove-old-failure (commit)
  "Remove any old failure log which may be present for COMMIT.
If COMMIT.log exists in the results/MACHINE/failed-builds
directory of `erb-suite-directory', where MACHINE is the current
system, remove it."
  (ignore-errors
    (delete-file (erb-run--failure-log-file-name commit))))

(defun erb-run--failure-log-file-name (commit)
  (expand-file-name (format "%s.log" commit) (erb--failed-builds-dir)))

;;;; Run benchmarks

(defun erb--benchmark-one-commit (commit target-emacs)
  "Run the benchmark tasks for one COMMIT and record the results.
The executable to run should be found in the subdirectory
'result' of the directory TARGET-EMACS."
  (let* ((tasks (erb--vc-tasks-for-commmit commit))
         (benchmark-task-files (directory-files-recursively
                                (erb--benchmark-dir) "\\.el$"))
         (all-run-results (make-erb--run-results)))
    (when tasks
      (dolist (file benchmark-task-files)
        (when-let* ((selected-tasks (erb--filter-by-file tasks file)))
          (let* ((this-run-results (erb--run-tasks target-emacs
                                                   file selected-tasks)))
            (setq all-run-results
                  (erb--merge-run-results all-run-results
                                          this-run-results))
            (thread-yield)))))
    (erb--record-run-results commit (system-name) all-run-results)))

(defun erb--filter-by-file (tasks file)
  "Return the list of TASKS which can be found in FILE.
TASKS should be a list of `erb--metadata' structures."
  (seq-filter (lambda (task)
                (string= (erb--metadata-filename task) file))
              tasks))

(defun erb--run-tasks (target-emacs file tasks)
  "Run the TASKS in FILE in TARGET-EMACS.
Return the benchmark results, messages, errors and process output
in `erb-run-result' structures.

TARGET-EMACS should be a directory, and the Emacs executable
should be in \"result/bin/emacs\" within that directory.  TASKS
should be a list of `erb--metadata' structures."
  (let* ((filename (file-relative-name file (erb--benchmark-dir)))
         (special-tasks (seq-filter #'erb--metadata-special tasks))
         (regular-tasks (cl-set-difference tasks special-tasks))
         results outputs failures messages)

    (when regular-tasks
      (let ((target-output
             (erb--run-target-emacs target-emacs file regular-tasks
                                    erb-task-repetitions)))
        (setq results (erb--get-successful-results target-output)
              outputs (erb--get-outputs target-output)
              failures (erb--get-failures target-output))
        messages (erb--get-messages target-output)))

    ;; TODO consider making a way to define and dispatch special
    ;; tasks instead of putting them all into this cl-case.  What
    ;; other special tasks might be interesting?
    (dolist (task special-tasks)
      (cl-case (erb--metadata-special task)

        ;; Don't load ERB or tasks, just see how long the target Emas
        ;; takes to start up and shut down.
        (startup
         (let (samples)
           (catch 'break
             (dotimes (_ erb-task-repetitions)
               ;; TODO quit early if no results are returned (which means error)
               (let* (target-output
                      (sample (benchmark-run
                                  (setq target-output
                                        (erb--run-target-emacs target-emacs
                                                               file nil 1))))
                      (startup-outputs (erb--get-outputs target-output))
                      (startup-failures (erb--get-failures target-output)))
                 (setq failures (nconc failures startup-failures)
                       outputs (nconc outputs startup-outputs))
                 (if startup-failures
                     (throw 'break nil)
                   (push sample samples)))))
           (when (= erb-task-repetitions (length samples))
             (push (make-erb--result :name (erb--metadata-name task)
                                     :version (erb--metadata-version task)
                                     :file filename
                                     :time (truncate (time-to-seconds
                                                      (current-time)))
                                     :samples samples)
                   results))))

        ;; Run the task in its own process, and invoke multiple processes
        ;; to get multiple samples.
        (own-process
         (let (single-process-samples)
           (catch 'break
             (dotimes (_ erb-task-repetitions)
               (let* ((target-output
                       (erb--run-target-emacs target-emacs file
                                              (list task) 1))
                      (sp-failures (erb--get-failures target-output))
                      (sp-outputs (erb--get-outputs target-output))
                      (sp-messages (erb--get-messages target-output))
                      (sp-results (erb--get-successful-results target-output))
                      (sample
                       (when sp-results
                         (car (erb--result-samples (car sp-results))))))
                 (setq failures (nconc failures sp-failures)
                       messages (nconc messages sp-messages)
                       outputs (nconc outputs sp-outputs))
                 (if sp-failures
                     (throw 'break nil)
                   (push sample single-process-samples)))))
           (when (= erb-task-repetitions (length single-process-samples))
             (push (make-erb--result :name (erb--metadata-name task)
                                     :version (erb--metadata-version task)
                                     :file filename
                                     :time (truncate (time-to-seconds
                                                      (current-time)))
                                     :samples single-process-samples)
                   results))))
        (t (message "Unknown special task type %s used in %s"
                    (erb--metadata-special task)
                    (erb--metadata-name task)))))

    (make-erb--run-results :results results :messages messages
                           :outputs outputs :failures failures)))

(cl-defstruct erb--target-output
  file               ; Filename of file containing tasks
                     ; (relative to benchmark dir).
  tasks              ; Task name symbol or list of them.
  exit-code          ; Process exit code.
  output             ; Process stdout+stderr.
  results            ; Lisp object read from results file, or nil.
  results-string     ; Text read from results file.
  time)              ; Unix timestamp.

;; TODO should this copy erb.el to the Emacs directory, what if it is remote?
;; Ditto for file with benchmarks.
(defun erb--run-target-emacs (target-emacs file tasks repetitions)
  "Invoke a target Emacs to run TASKS from FILE, REPETITIONS times.
TARGET-EMACS is the directory in which the target Emacs was
built, and the executable should be in \"result/bin/emacs\"
relative to TARGET-EMACS.

Return an `erb--target-output' structure containing the results
of running the process, including exit code, benchmark results
and output.  If TASKS is nil, do not load ERB in the target Emacs
process."
  (let* ((filename (file-relative-name file (erb--benchmark-dir)))
         (tasks-file (when tasks
                       (erb--compile-tasks-file target-emacs file)))
         (results-file (expand-file-name "results.eld" target-emacs))
         ;; TODO wrap loads and evals with with-demoted-errors
         ;; to guarantee we always get to kill-emacs, even when running
         ;; interactively.
         (executable (expand-file-name "result/bin/emacs" target-emacs))
         (invoke-emacs-args (map-elt erb--machine-config 'emacs-arguments))
         (load-erb-and-task-args (when tasks
                                   `("-l" ,erb-task-el-filename
                                     "-l" ,tasks-file)))
         (repetitions-args
          `("--eval" ,(format "(setq erb-task-repetitions %s)" repetitions)))
         (task-names (mapcar #'erb--metadata-name tasks))
         (task-list (mapconcat #'symbol-name task-names " "))
         (invoke-erb-args
          (when tasks
            `("--eval" ,(format "(erb-task-run-batch '(%s) %S)"
                                task-list results-file))))
         (kill-emacs-args '("--eval" "(kill-emacs)"))
         (args (append invoke-emacs-args
                       repetitions-args
                       load-erb-and-task-args
                       invoke-erb-args
                       kill-emacs-args))
         (target-output (erb--call-process-read-results executable args
                                                        results-file)))
    (setf (erb--target-output-file target-output) filename
          (erb--target-output-tasks target-output) task-names)
    target-output))

(defun erb--call-process-read-results (executable args file &optional _async)
  "Invoke EXECUTABLE with ARGS.
Return the results of the process in an `erb--target-output'
structure.

FILE should be a filename.  If the file exists after the process
finishes, read a Lisp object from it and put it in the `results'
slot of the returned structure."
  (with-temp-buffer
    ;; TODO asynchronicity
    (let* ((outbuf (generate-new-buffer "ERB-task"))
           (exit-code (apply #'call-process
                             (append `(,executable nil ,outbuf nil) args)))
           (retval (make-erb--target-output
                    :exit-code exit-code
                    :output (with-current-buffer outbuf (buffer-string))
                    :time (truncate (time-to-seconds (current-time))))))
      (kill-buffer outbuf)
      (with-temp-buffer
        (when (file-readable-p file)
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case err
              (setf (erb--target-output-results retval)
                    (read (current-buffer)))
            (error
             (message "Invalid Lisp object in ERB: %s (%s)" err args)
             (setf (erb--target-output-results-string retval)
                   (buffer-string))))))
      retval)))

(defun erb--compile-tasks-file (target-emacs file)
  ;; TODO make a benchmark subdirectory in target-emacs and compile there
  (let ((dest (expand-file-name (file-name-nondirectory file) target-emacs)))
    (copy-file file dest t)
    (with-temp-buffer
      (let ((exit-code (call-process
                        (expand-file-name "result/bin/emacs" target-emacs)
                        nil t nil
                        "-Q" "--batch"
                        "-l" erb-task-el-filename
                        "-f" "batch-byte-compile" dest)))
        (when (> (point-max) (point-min))
          (message "%s" (buffer-string)))
        (unless (equal 0 exit-code)
          (message "Failed to byte-compile %s" file))
        ;; TODO log this somehow
        (format "%s%s" dest (if (equal 0 exit-code) "c" ""))))))

;;;; Benchmark runner status

;; TODO make this a cl-defstruct and implement clear with
;; introspection
(defconst erb--status-fields
  '(state                  ; nil, building, benchmarking or done
    waiting-to-build       ; The commits which have not yet been built.
    commits                ; All the commits in the job.
    building               ; The commits currently being built.
    built                  ; Commits which have been successfully built.
    failed-builds          ; Commits we tried and failed to build.
    waiting-to-benchmark   ; Built and waiting to be benchmarked.
    benchmarking           ; Commits currently being benchmarked.
    failed-runs            ; Commits with errors during benchmarking.
    finished               ; Commits done benchmarking.
    start-time             ; Time stamp when run started (see `current-time').
    stop-time))            ; Time stamp when run stopped.

(defvar erb--status (mapcar #'list erb--status-fields)
  "An alist containing the status of the ERB benchmark runner.")
(make-symbol-mutex 'erb--status)

(defun erb--clear-status ()
  "Reset all the ERB benchmarking status variables to their initial state."
  (with-symbol-mutex erb--status
    (setq erb--status (mapcar #'list erb--status-fields))))

(defun erb--status-set (field value)
  (with-symbol-mutex erb--status
    (setf (map-elt erb--status field)  value)))

(defun erb--status-add (value field)
  (with-symbol-mutex erb--status
    (push value (map-elt erb--status field))))

(defun erb--status-remove (value field)
  (with-symbol-mutex erb--status
    (setf (map-elt erb--status field)
          (remove value (map-elt erb--status field)))))

(defun erb--status (field)
  (with-symbol-mutex erb--status
    (map-elt erb--status field)))

;;;; Store and retrieve benchmark results

;;;;; Benchmark result data structures

(cl-defstruct erb--result
  machine commit name version file time samples)

(defun erb--result< (a b)
  "Return non-nil if A should be sorted before B.
A and B should be `erb--result' structures."
  (catch 'done
    (let ((slots '(file name version machine commit)))
      (dolist (slot slots)
        (let ((a-val (cl-struct-slot-value 'erb--result slot a))
              (b-val (cl-struct-slot-value 'erb--result slot b)))
          (unless (string= a-val b-val)
            (throw 'done (string< a-val b-val)))))
      (< (erb--result-time a) (erb--result-time b)))))

(defun erb--struct-match-p (type slots a b)
  "Return non-nil if the SLOTS in A and B are the same (using `equal').
SLOTS should be a list of symbols which are slot names in
TYPE (as defined by `cl-defstruct'), and A and B should be
instances of TYPE."
  (catch 'result
    (dolist (slot slots)
      (unless (equal (cl-struct-slot-value type slot a)
                     (cl-struct-slot-value type slot b))
        (throw 'result nil)))
    t))

(iter-defun erb--chunk-list (type slots structs)
  "Yield lists of entries from STRUCTS in which the values of SLOTS match.
STRUCTS should be a list of instances of TYPE (as defined by
`cl-defstruct') and SLOTS should be a list of symbols
corresponding to slots in TYPE.  Yield a list containing the
first remaining element of STRUCTS plus those elements immediately
following it which have the same slot values."
  (while structs
    (let ((first (car structs))
          matching)
      (while (and structs (erb--struct-match-p type slots first (car structs)))
        (push (pop structs) matching))
      (iter-yield (nreverse matching)))))

(cl-defstruct erb--failure
  machine    ; Hostname the target Emacs was run on.
  commit     ; Commit the target Emacs was built from.
  file       ; Name of the benchmark task definition file, relative to the
             ; benchmark directory.
  tasks      ; Single task or list of tasks provided to Emacs (as symbols).
  error      ; (SYMBOL MESSAGE)
  time       ; Integer Unix timestamp.
  )

(cl-defstruct erb--output
  machine commit file tasks output time)

(cl-defstruct erb--messages
  machine commit file name messages time)

(cl-defstruct erb--run-results
  results   ; A list of `erb--result's.
  messages  ; A list of `erb--messages'.
  outputs   ; A list of `erb--output's.
  failures  ; A list of `erb--failure's.
  )

(defun erb--merge-run-results (a b)
  "Return an `erb--run-results' structure by combining A and B.
A and B should be `erb--run-results' structures.   Destructively
modify A."
  (make-erb--run-results
   :results
   (nconc (erb--run-results-results a) (erb--run-results-results b))
   :messages
   (nconc (erb--run-results-messages a) (erb--run-results-messages b))
   :outputs
   (nconc (erb--run-results-outputs a) (erb--run-results-outputs b))
   :failures
   (nconc (erb--run-results-failures a) (erb--run-results-failures b))))

(defconst erb--run-result-dir-names-alist
  '((results "measurements")
    (messages "logs" "messages")
    (outputs "logs" "process-output")
    (failures "task-errors"))
  "Directories in which to save the components of `erb--run-results'.
An alist mapping `erb--run-results' slot names to lists of strings,
which are used to construct directory names.")

(defun erb--run-result-dir-name (slot)
  "Return the directory name used by SLOT of `erb--run-result.'"
  (let ((names (alist-get slot erb--run-result-dir-names-alist))
        (dirname (erb--results-dir)))
    (dolist (name names)
      (setq dirname (expand-file-name name dirname)))
    dirname))

(defun erb--record-run-results (commit machine run-result)
  "Add the contents of RUN-RESULT to the data saved for COMMIT and MACHINE."
  (dolist (slot (mapcar #'car erb--run-result-dir-names-alist))
    (when-let ((entries (cl-struct-slot-value 'erb--run-results slot run-result))
               (directory (erb--run-result-dir-name slot))
               (file (expand-file-name (format "%s.eld" commit) directory)))
      (make-directory directory t)
      (let ((table (if (file-readable-p file)
                       (erb--table-read file)
                     (make-erb--table-for-type
                      (type-of (car entries))
                      :constants (list :commit commit :machine machine)))))
        (erb--table-insert table entries)
        (erb--table-write table file)))))

;;;;; Detect errors

(defun erb--get-failures (target-output)
  "Return a list of failure conditions found in TARGET-OUTPUT.
Return a list of `erb-failure' structures for the following
conditions: nonzero process exit code, results that were entirely
missing, missing or invalid samples, errors recorded in samples,
and tasks without results."
  (let* (failures
         (file           (erb--target-output-file            target-output))
         (tasks          (erb--target-output-tasks           target-output))
         (results        (erb--target-output-results         target-output))
         (results-string (erb--target-output-results-string  target-output))
         (exit-code      (erb--target-output-exit-code       target-output))
         (time           (erb--target-output-time            target-output))
         (filename (file-relative-name file (erb--benchmark-dir))))
    (cl-flet ((add-failure (task err)
                           (push (make-erb--failure :file filename :tasks task
                                                    :error err :time time)
                                 failures)))
      (cond
       ((not (equal 0 exit-code))
        (add-failure tasks `(erb--process-failed
                             ,(format "Process exit code: %s" exit-code))))
       ((and tasks (null results))
        (add-failure tasks `(erb--invalid-results
                             ,(format "Contents of results file: %S"
                                      results-string))))
       (t (let (found)
            (dolist (result results)
              (map-let (name samples) result
                (push name found)
                (if (null samples)
                    (add-failure name '(erb--no-samples "No samples collected"))
                  (catch 'break
                    (dolist (sample samples)
                      (cond
                       ((and sample
                             (= (length sample) 2)
                             (symbolp (nth 0 sample))
                             (stringp (nth 1 sample)))
                        (add-failure name sample)
                        (throw 'break))
                       ((not (erb--valid-sample-p sample))
                        (add-failure name `(erb--invalid-sample
                                            ,(format "Invalid sample: %s"
                                                     sample)))
                        (throw 'break))))))))
            (dolist (not-found (cl-set-difference (if (listp tasks)
                                                      tasks (list tasks))
                                                  found))
              (add-failure not-found
                           `(erb--missing-task
                             ,(format "No samples created for task"))))))))
    failures))

(defun erb--valid-sample-p (sample)
  "Return non-nil if SAMPLE resembles a return value of `benchmark-run'."
  (and (listp sample)
       (= (length sample) 3)
       (floatp (nth 0 sample))
       (integerp (nth 1 sample))
       (floatp (nth 2 sample))))

;;;;; Extract structured data from output of target process

(defun erb--get-messages (target-output)
  "Make all the messages in TARGET-OUTPUT into `erb-message' structures.
Return a list."
  (let (message-structs
        (file           (erb--target-output-file      target-output))
        (results        (erb--target-output-results   target-output))
        (exit-code      (erb--target-output-exit-code target-output))
        (time           (erb--target-output-time      target-output)))
    (when (equal 0 exit-code)
      (dolist (result results)
        (map-let (name messages) result
          (let* ((unique-messages
                  (cl-remove-duplicates (remove "" messages)
                                        :test #'string=)))
            (when unique-messages
              (push (make-erb--messages :file file :name name
                                        :messages unique-messages :time time)
                    message-structs))))))
    message-structs))

(defun erb--get-outputs (target-output)
  "Return the process output from TARGET-OUTPUT, if there was any.
Return it as a list containing a single `erb--output' structure, or
nil if there was no output."
  (let ((file (erb--target-output-file target-output))
        (tasks (erb--target-output-tasks target-output))
        (output (erb--target-output-output target-output))
        (time (erb--target-output-time target-output)))
    (unless (string= output "")
      (list
       (make-erb--output :file file :tasks tasks :output output :time time)))))

(defun erb--get-successful-results (target-output)
  "Return only those results in TARGET-OUTPUT representing successful runs.
Returns a list of `erb--result' structures."
  (let (result-structs
        (file           (erb--target-output-file      target-output))
        (results        (erb--target-output-results   target-output))
        (exit-code      (erb--target-output-exit-code target-output))
        (time           (erb--target-output-time      target-output)))
    (when (equal 0 exit-code)
      (dolist (result results)
        (map-let (name version samples) result
          (when (and samples
                     (seq-every-p #'erb--valid-sample-p samples))
            (push (make-erb--result :file file :name name :version version
                                    :samples samples :time time)
                  result-structs)))))
    result-structs))

;;;;; Filenames and directories for benchmark results

(defun erb--failed-runs-dir (&optional machine)
  (expand-file-name "task-errors" (erb--results-dir machine)))

(defun erb--failed-builds-dir (&optional machine)
  (expand-file-name "build-errors" (erb--results-dir machine)))

(defun erb--results-dir (&optional machine)
  (unless machine (setq machine (system-name)))
  (thread-last erb-suite-directory
    (expand-file-name "results")
    (expand-file-name machine)))

(defun erb--machine-results-dirs ()
  (let ((files (directory-files
                (expand-file-name "results" erb-suite-directory) t
                "[^.].*")))
    (seq-filter #'file-directory-p files)))

;;; Minimal database API

(cl-defstruct (erb--table
               (:constructor make-erb--table)
               (:constructor make-erb--table-for-type
                             (type
                              &key (constants nil)
                              &aux
                              ;; Only those keys not in `constants'
                              (keys (erb--table-find-keys type constants)))))
  type keys constants rows)

(defun erb--table-find-keys (type constants)
  "Return the list of keys to be saved in the file.
Return the list of slot names for TYPE converted to keywords,
and without any keywords found in the plist CONSTANTS."
  (let* ((slots (cdr (mapcar #'car (cl-struct-slot-info type))))
         (keywords (mapcar (lambda (slot)
                             (intern (format ":%s" (symbol-name slot))))
                           slots))
         (constants (cl-loop for k in constants by #'cddr
                             collect k)))
    (cl-set-difference keywords constants)))

(defun erb--table-insert (table rows)
  "Insert ROWS into TABLE.
ROWS may be a single object or a list."
  (unless (listp rows)
    (setq rows (list rows)))
  (let ((type (erb--table-type table)))
    (mapc (lambda (rec) (cl-assert (eq (type-of rec) type))) rows))
  (setf (erb--table-rows table) (nconc (erb--table-rows table) rows))
  table)

(defun erb--table-read (filename)
  "Read a `erb--table' from FILENAME."
  (let (table)
    (condition-case err
        (when (file-readable-p filename)
          (with-temp-buffer
            (insert-file-contents filename)
            (goto-char (point-min))
            (setq table (read (current-buffer)))))
      (error "Error reading %s: %s" filename err))
    (unless (and table
                 (null (cl-set-exclusive-or (map-keys table)
                                            '(erb-version type keys
                                              constants rows))))
      ;; TODO the right thing about older/newer ERB versions
      ;; have an argument to the constructor for that
      (error "Incorrect keys in ERB data file: %s" filename))
    (map-let (erb-version type keys constants rows) table
      (let ((constructor (apply-partially #'erb--make-record
                                          type keys constants)))
        (make-erb--table :type type
                         :keys keys
                         :constants constants
                         :rows (mapcar constructor rows))))))

(defun erb--make-record (type keys constants values)
  "Return a new structure of TYPE initialized by VALUES.
The slots corresponding to KEYS will be set to the respective values in
VALUES.  The plist CONSTANTS will be included in the arguments
passed to the constructor."
  (let* ((kv-pairs (cl-mapcar #'list keys values))
         (kv-args (apply #'append kv-pairs))
         (args (append constants kv-args))
         (constructor (intern-soft (format "make-%s" (symbol-name type)))))
     (apply constructor args)))

(defun erb--table-write (table filename)
  "Write TABLE to FILENAME."
  (let* ((rows (erb--table-rows table))
         (values (mapcar (apply-partially #'erb--get-record-values table) rows))
         (alist `((erb-version     . ,erb-version)
                  (type            . ,(erb--table-type table))
                  (keys            . ,(erb--table-keys table))
                  (constants       . ,(erb--table-constants table))
                  (rows            . ,values))))
    (with-temp-file filename
      (let ((standard-output (current-buffer)))
        (cl-prin1 alist)
        (pp-buffer)))))

(defun erb--get-record-values (table record)
  "Return a list of the values corresponding to KEYS in RECORD."
  (mapcar (lambda (key)
            (cl-struct-slot-value (erb--table-type table)
                                  (intern (substring (symbol-name key) 1))
                                  record))
          (erb--table-keys table)))


(defun erb--table-select (table func)
  "Return all rows in TABLE for which FUNC returns non-nil."
  (let (results)
    (dolist (row (erb--table-rows table))
      (when (funcall func row)
        (push row results)))
    (nreverse results)))

(defun erb--table-update (table select-func update-func)
  "Update selected rows in TABLE.
Call SELECT-FUNC on each row in TABLE.  If it returns non-nil,
call UPDATE-FUNC on the row."
  (dolist (row (erb--table-rows table))
    (when (funcall select-func row)
      (funcall update-func row))))

;;; Cache commit times

(cl-defstruct erb--commit
  commit time)

(defun erb--cache-commit-time (commit)
  "Get the time of COMMIT from git if that has not yet been done.
Save it in a database in the \"commits\" subdirectory of the
machine results directory.  These could be combined into a single
file instead of one file per machine, but that would make
git-merging results harder."
  ;; TODO function to generate filename
  (let* ((dirname (expand-file-name "cache" (erb--results-dir)))
         (filename (progn (make-directory dirname t)
                          (expand-file-name "commits.eld" dirname)))
         (commit-db
          (if (file-readable-p filename)
              (erb--table-read filename)
            (make-erb--table-for-type 'erb--commit)))
         (select-func (lambda (rec) (string= commit (erb--commit-commit rec)))))
    (unless (erb--table-select commit-db select-func)
      (erb--table-insert commit-db
                         (make-erb--commit :commit commit
                                           :time (erb--vc-get-commit-time
                                                  commit)))
      (erb--table-write commit-db filename))))

(defun erb--read-commit-cache ()
  "Read all the cached commit times and return them in a hash table."
  (let ((all-commits (make-hash-table :test 'equal)))
    (dolist (machine (erb--machine-results-dirs))
      (when-let ((commit-file (thread-last machine
                                (expand-file-name "cache")
                                (expand-file-name "commits.eld")))
                 (table (with-demoted-errors "Error: %s"
                          (erb--table-read commit-file))))
        (dolist (row (erb--table-rows table))
          (puthash (erb--commit-commit row) (erb--commit-time row)
                   all-commits))))
    all-commits))

;;; Communication with version control

(defun erb--vc-get-commit-time (commit)
  "Get the UNIX timestamp for COMMIT."
  (let ((default-directory (map-elt erb--config 'project-repo)))
    (with-temp-buffer
      (call-process "git" nil t nil
                    "log" "-1" "--format=%ct" commit)
      (string-to-number (buffer-string)))))

(defun erb--vc-tasks-for-commmit (commit)
  "Return the list of tasks which should be run for COMMIT."
  (map-let (project-repo) erb--config
    (seq-filter (lambda (task)
                  (erb--vc-commit-appropriate-p task project-repo commit))
                erb--benchmark-tasks)))

;; TODO
(defun erb--vc-commit-appropriate-p (_task _src-repo _commit)
  "Return non-nil if TASK should be run for a build of COMMIT."
  t)

(defvar erb--commit-range-count-cache (make-hash-table :test 'equal))

(defun erb--vc-get-commit-range-count (range)
  "Return the number of commits in RANGE.
If there is an error trying to determine that, return nil."
  (if-let ((cached (gethash range erb--commit-range-count-cache)))
      cached
    (let ((default-directory (map-elt erb--config 'project-repo)))
      (with-temp-buffer
        (when (= (call-process "git" nil t nil "rev-list" "--count" range) 0)
          (let ((result (string-to-number (buffer-string))))
            (puthash range result erb--commit-range-count-cache)
            result))))))

(defun erb--vc-get-commits (range &optional select-count)
  "Return the list of commits in RANGE, ordered oldest to newest.
If SELECT-COUNT is provided, limit the number of commits returned
to that number, choosing them at intervals spaced out over the
entire list of commits."
  (let* ((default-directory (map-elt erb--config 'project-repo))
         (lines (process-lines "git" "rev-list" "--first-parent" range))
         (count (length lines))
         (num (or select-count count))
         (gap (max 1 (/ (- count 1) (if (> num 1) (- num 1.0) 1.0))))
         (indices (let ((index 0)
                        result)
                    (while (and (< index count) (< (length result) num))
                      (push (truncate index) result)
                      (cl-incf index gap))
                    (when (< (length result) num)
                      (push (1- count) result))
                    (cl-remove-duplicates result :test #'=))))
    (mapcar (lambda (index) (nth index lines)) indices)))

;;; Publish results

;;;; Customize which results are used and shown

(defcustom erb--include-older-samples nil
  "When non-nil, average the results of all the runs of each task."
  :type 'boolean
  :group 'erb
  :version "27.1")

(defcustom erb--show-all-task-versions nil
  "When non-nil, show results for older versions of tasks.
Otherwise only the results of the newest version of the task will
be shown."
  :type 'boolean
  :group 'erb
  :version "27.1")

;;;; Data structure for summarized results

(cl-defstruct erb--summary
  name           ; from erb--result
  file           ; from erb--result
  version-values ; ((VERSION . COMMIT-VALUES) ...)
                 ; COMMIT-VALUES is list (commit VALUES)
                 ; VALUES is an array of floats indexed by machine number
  )

;;;; Read and summarize benchmark results

(defun erb--read-all-results ()
  "Read all the benchmarking results from all the machines.
Return a sorted list of `erb-result' structures."
  (let ((machine-dirs (erb--machine-results-dirs))
        all-results
        (all-commits (make-hash-table :test 'equal)))

    ;; Collect all results from all machines into one list.
    (dolist (machine machine-dirs)
      (let ((measurements-dir (expand-file-name "measurements" machine)))
        (dolist (commit-file (directory-files measurements-dir t ".+\\.eld$"))
          (when-let ((commit (substring (file-name-nondirectory commit-file)
                                        0 (- (length ".eld"))))
                     (commit-time (gethash commit all-commits 0))
                     (table (erb--table-read commit-file)))
            (dolist (row (erb--table-rows table))
              (push row all-results))))))

    (sort all-results #'erb--result<)))

(defun erb--calculate-result-averages (results)
  "Calculate average times for each benchmark task.
RESULTS should be a sorted list of `erb--result' structures, one
for each task run.  Calculate the averages of all the samples for
each task run for each commit on each machine, and return a list
of `erb-result' structures, with the `samples' slot containing
the calculated average time."
  (let (averaged-results)
    (iter-do (matching (erb--chunk-list 'erb--result
                                        '(machine commit name file version)
                                        results))
      (let* ((newest (car (last matching)))
             (copy (copy-erb--result newest)))
        (setf (erb--result-samples copy)
              (erb--average-of-samples
               (if erb--include-older-samples
                   (let ((all-samples (apply #'append
                                             (mapcar #'erb--result-samples
                                                     matching))))
                     all-samples)
                 (erb--result-samples newest))))
        (push copy averaged-results)))
    (nreverse averaged-results)))

(defun erb--average-of-samples (samples)
  (/ (seq-reduce #'+ (mapcar #'car samples) 0.0) (length samples)))

(defun erb--summarize-task-results (machines averaged-results)
  "Collect results for each task into `erb--summary' structures.
MACHINES is a list of machine names.  AVERAGED-RESULTS should be
a sorted list of `erb--result' structures.  Collect all the
results for all the runs of each task into one `erb--summary'
structure per task."
  (let (summaries last-machine last-machine-index)

    ;; Since the list of results is sorted by machine, avoid
    ;; calls to cl-position by caching it.
    (cl-flet ((machine-index (machine)
                (unless (equal last-machine machine)
                  (setq last-machine machine
                        last-machine-index (cl-position machine machines
                                                        :test 'equal)))
                last-machine-index))

      (iter-do (task-results (erb--chunk-list 'erb--result '(name file)
                                              averaged-results))
        (let* ((first (car task-results))
               (summary (make-erb--summary :name (erb--result-name first)
                                           :file (erb--result-file first))))

          (iter-do (version-results (erb--chunk-list 'erb--result
                                                     '(version)
                                                     task-results))
            ;; Now we have a list where all entries have the same
            ;; file, task, name and version but different machines and
            ;; commits.  Make an alist where the keys are commits and
            ;; the values are arrays of measurements indexed by machines.
            ;; All commits are not necessarily present on all machines.
            (let ((commit-values-ht (make-hash-table :test 'equal))
                  (version (erb--result-version (car version-results))))
              (dolist (result version-results)
                (let ((existing (gethash (erb--result-commit result)
                                         commit-values-ht)))
                  (unless existing
                    (setq existing (make-vector (length machines) nil)))
                  (aset existing (machine-index (erb--result-machine result))
                        (erb--result-samples result))
                  (puthash (erb--result-commit result) existing
                           commit-values-ht)))

              ;; Convert the hash table to an alist.
              (let (commit-values)
                (maphash #'(lambda (c v) (push (cons c v) commit-values))
                         commit-values-ht)
                (push (cons version commit-values)
                      (erb--summary-version-values summary)))))
          (setf (erb--summary-version-values summary)
                (nreverse (erb--summary-version-values summary)))
          (push summary summaries))))
    (nreverse summaries)))

;;; Write org file containing results with gnuplot graphs

;; Todo something like sockeye (nixos) and rainbow (darwin) in graph keys
(defun erb-write-result-org-file ()
  (interactive)
  (erb--update-config-cache)
  (unless erb--config
    (insert (format "Error reading `config.eld': \n  %s\n" erb--config-err)))
  (unless erb--benchmark-tasks
    (erb--read-benchmark-metadata))
  (let* ((report-dir (expand-file-name "report" erb-suite-directory))
         (report-file (progn (make-directory report-dir t)
                             (expand-file-name "report.org" report-dir)))
         (machines (sort (mapcar #'file-name-nondirectory
                                 (erb--machine-results-dirs))
                         #'string<))
         (results (erb--read-all-results))
         (averages (erb--calculate-result-averages results))
         (summaries (erb--summarize-task-results machines averages))
         (commit-cache (erb--read-commit-cache))
         (title (format "#+TITLE: %s Benchmarks\n"
                        (map-elt erb--config 'project-name)))
         (xtics (format "set xtics rotate by -45 \\\n    (%s)\n"
                        (mapconcat (lambda (commit)
                                     (format "\"%s\" %s" commit
                                             (erb--vc-get-commit-time commit)))
                                   (map-elt erb--config 'tags) ", \\\n     ")))
         (this-buffer (current-buffer)))
    ;; TODO what does gnuplot do with empty list?

    (make-directory (expand-file-name "plots" report-dir) t)
    (with-temp-file report-file
      (insert
       title
       "#+OPTIONS: toc:2 num:2 author:nil\n"
       "#+LATEX_HEADER: \\usepackage[margin=0.5in]{geometry}\n"
       "* Benchmark results\n")
      (iter-do (file-summaries (erb--chunk-list 'erb--summary
                                                '(file) summaries))
        (let ((file (erb--summary-file (car file-summaries))))
          (insert
           (format "** %s\n" (with-current-buffer this-buffer
                               (erb--benchmark-file-description file)))
           (format "=%s=\n" file)))

        (dolist (summary file-summaries)
          (let* ((name (erb--summary-name summary))
                 (vv-alist (erb--summary-version-values summary))
                 (multiple-versions (> (length vv-alist) 1))
                 (versions (mapcar #'car
                                   (if erb--show-all-task-versions
                                       vv-alist
                                     (last vv-alist))))
                 (data-tables ""))
            (insert
             (format "*** %s\n" name)
             (if-let ((metadata
                       (seq-find (lambda (m)
                                   (equal name (erb--metadata-name m)))
                                 erb--benchmark-tasks)))
                 (format "%s\n" (substitute-command-keys
                                 (erb--metadata-documentation metadata)))
               ""))
            (dolist (version versions)
              (let* ((data-table-name (format "%s-%s" name version))
                     (measurements (erb--summary-measurements
                                    commit-cache version summary))
                     (x-axis (erb--analyze-x-axis measurements))
                     (y-axis (erb--analyze-y-axis measurements)))
                (insert
                 "#+BEGIN_SRC gnuplot "
                 (format ":var data=%1$s() :file plots/%1$s.png :noweb yes\n"
                         data-table-name)
                 "reset\n"
                 "set terminal png size 800, 600\n"
                 (format "set title \"%s%s\"\n" name
                         (if multiple-versions (format "-%s" version) ""))
                 "set xlabel \"Commit\"\n"
                 (format "set xrange [%s:%s]\n"
                         (map-elt x-axis 'actual-min)
                         (map-elt x-axis 'actual-max))
                 "<<xtics>>\n"
                 "set ylabel \"Run time (seconds)\"\n"
                 (format "set yrange [%s:%s]\n"
                         0 (* 1.1 (map-elt y-axis 'actual-max)))
                 "set key right bottom\n"
                 "plot "
                 (mapconcat (lambda (mach-index)
                              (format "data u 2:%d w lp lw 2 title '%s'"
                                      (+ mach-index 3)
                                      (nth mach-index machines)))
                            (number-sequence 0 (1- (length machines)))
                            ", \\\n     ")
                 "\n"

                 "#+END_SRC\n\n")
                ;; Because of the :noexport: tags, the data tables
                ;; have to come after the plots.  Print them to a
                ;; string now and insert them after the version loop,
                ;; to avoid having to recalculate `measurements'.
                (setq data-tables
                      (concat data-tables
                              (format "*** Measurements for %s :noexport:\n"
                                      data-table-name)
                              (format "#+NAME: %s\n" data-table-name)
                              "#+BEGIN_SRC emacs-lisp\n"
                              (with-temp-buffer
                                (let ((standard-output (current-buffer)))
                                  (princ "'")
                                  (cl-prin1 measurements))
                                (pp-buffer)
                                (buffer-string))
                              "#+END_SRC\n\n"))))
            (insert data-tables))))
      (insert "* Benchmark machine information\n")
      (dolist (machine machines)
        (let ((config (with-demoted-errors "Error: %s"
                        (erb--read-machine-config machine)))
              (info (with-demoted-errors "Error: %s"
                      (car-safe (erb--read-saved-machine-info machine))))
              (important '("OS" "Kernel" "CPU" "GPU" "Memory")))
          (insert
           (format "** %s\n" machine)
           "*** Configuration\n"
           (format "Arguments used to invoke Emacs: =%s=\n"
                   (mapconcat #'identity (map-elt config 'emacs-arguments) " "))
           "*** System information\n"
           "#+OPTIONS: ^:nil\n")
          (if (null info)
              (insert "Unavailable\n")
            (insert
             "#+BEGIN_SRC emacs-lisp :results value table :exports results\n"
             (with-temp-buffer
               (let ((standard-output (current-buffer))
                     cleaned-info)
                 (dolist (key important)
                   (when-let ((value (map-elt info key nil #'equal)))
                     (push (list key value) cleaned-info)))
                 (dolist (key (cl-set-difference (mapcar #'car info) important
                                                 :test #'equal))
                   (when-let ((is-string (stringp key))
                              (value (map-elt info key nil #'equal)))
                     (push (list key value) cleaned-info)))
                 (princ "'")
                 (cl-prin1 (nreverse cleaned-info)))
               (pp-buffer)
               (buffer-string))
             "#+END_SRC\n\n"
             (format-time-string
              "System information last updated: %Y-%m-%d %a %H:%M\n"
              (map-elt info :time))
             "* Xtics :noexport:\n"
	     "#+NAME: xtics\n"
             "#+BEGIN_SRC gnuplot :export none\n"
             xtics
             "#+END_SRC\n")))))))

(defun erb--benchmark-file-description (file)
  (let ((filename (expand-file-name file (erb--benchmark-dir))))
    (condition-case _err
        (with-temp-buffer
          (insert-file-contents filename)
          (goto-char (point-min))
          (re-search-forward ";+ .+? --- \\(.+?\\)\\( -*-.+?\\)$"
                             (save-excursion (forward-line) (point)))
          (match-string 1))
      (error "Failed to find description in first line"))))

(defun erb--analyze-x-axis (measurements)
  (erb--analyze-axis (mapcar #'cadr measurements)))

(defun erb--analyze-y-axis (measurements)
  (let* ((count (- (length (car measurements)) 2))
        (y-values (mapcar (lambda (measurement)
                            (last measurement count))
                          measurements)))
    (erb--analyze-axis (apply #'append y-values))))

;; TODO cl-defstruct
(defun erb--analyze-axis (numbers)
  (setq numbers (remq nil numbers))
  (let* ((actual-min (seq-reduce #'min numbers (car numbers)))
         (actual-max (seq-reduce #'max numbers (car numbers)))
         (range (- actual-max actual-min))
         (padded-min (max 0 (- actual-min (* 0.2 range))))
         (padded-max (+ actual-max (* 0.2 range))))
    `((actual-min . ,actual-min)
      (actual-max . ,actual-max)
      (range      . ,range)
      (padded-min . ,padded-min)
      (padded-max . ,padded-max))))

(defun erb--summary-measurements (commit-time-cache version summary)
  (let* ((values (map-elt (erb--summary-version-values summary) version
                          nil #'equal))
         (measurements
          (mapcar
           (pcase-lambda (`(,commit . ,machine-values))
             (let ((commit-time (gethash commit commit-time-cache 0)))
               (append (list commit commit-time) machine-values nil)))
           values)))
    ;; Return list sorted by commit time.
    (sort measurements (lambda (a b) (< (nth 1 a) (nth 1 b))))))

(provide 'erb)
;;; erb.el ends here
