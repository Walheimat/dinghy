;;; dinghy-rope.el --- Test and CI helpers -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/bydi
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: extensions

;;; Commentary:
;;
;; Path setup (for CI) as well as integration of `ert-runner' and
;; `undercover'.

;;; Code:

(require 'cl-lib)

(defvar dinghy-rope-report--temp-files nil)

(defvar dinghy-rope-report--env-coverage-with-json "COVERAGE_WITH_JSON"
  "If set, SimpleCov (JSON) format is used.")

(defvar dinghy-rope-report--env-ci "CI"
  "Set if in a CI environment.")

(defvar dinghy-rope-report--text-file "./coverage/results.txt"
  "The file used to store text coverage.")

(defvar dinghy-rope-report--json-file "./coverage/.resultset.json"
  "The file used to store the JSON coverage.")

(defun dinghy-rope-report--add-up-type (buf type)
  "Add all numbers of TYPE in buffer BUF."
  (let* ((regex (concat type ": \\(?1:[[:digit:]]+\\)"))
         (content (with-current-buffer buf (buffer-string)))
         (numbers (dinghy-rope-report--matches-in-string regex content)))

    (apply '+ (mapcar #'string-to-number numbers))))

(defun dinghy-rope-report--matches-in-string (regexp str)
  "Return all matches of REGEXP in STR."
  (let ((matches nil))

    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (match-string 1) matches)))
    matches))

(defun dinghy-rope-report--consume-undercover-report ()
  "Consume the report.

This returns the combined relevant and covered lines as well as
an average of the coverage."
  (with-temp-buffer
    (insert-file-contents dinghy-rope-report--text-file)

    (when-let* ((relevant (dinghy-rope-report--add-up-type (current-buffer) "Relevant"))
                (covered (dinghy-rope-report--add-up-type (current-buffer) "Covered")))

      (list
       (string-to-number (format "%.2f%%" (* 100 (/ (float covered) relevant))))
       relevant
       covered
       (- relevant covered)))))

(defun dinghy-rope-report--undercover-result ()
  "Print the undercover report.

This includes the average result on top of the full report."
  (and-let* (((file-exists-p dinghy-rope-report--text-file))
             (consumed (dinghy-rope-report--consume-undercover-report))
             (report (with-temp-buffer
                       (insert-file-contents-literally dinghy-rope-report--text-file)
                       (goto-char 0)
                       (delete-line)
                       (buffer-string))))
    (cl-destructuring-bind (average relevant covered missed) consumed
      (message "%s\nAverage : Percent %s%% [Relevant: %s Covered: %s Missed: %s]\n"
               report
               average
               relevant
               covered
               missed))))

(defun dinghy-rope-report--record-temp-file (name &rest _)
  "Record temp file NAME."
  (push name dinghy-rope-report--temp-files))

(defun dinghy-rope-report--print-temp-files (&rest _)
  "Print created temp files."
  (when dinghy-rope-report--temp-files
    (message
     "\nCreated the following temp files:\n%s"
     dinghy-rope-report--temp-files))

  (advice-remove 'ert-with-temp-file #'dinghy-rope-report--record-temp-file))

(defun dinghy-rope-report--setup-ert-runner (reporter)
  "Set up `ert-runner'.

An optional REPORTER function can be passed."
  (advice-add 'ert-with-temp-file :before #'dinghy-rope-report--record-temp-file)

  (add-hook
   'ert-runner-reporter-run-ended-functions
   #'dinghy-rope-report--print-temp-files)

  (when reporter
    (add-hook
     'ert-runner-reporter-run-ended-functions
     reporter)))

(declare-function project-root "project.el")

(defvar dinghy-rope-report--test-helper-location "test/test-helper.el")

(defun dinghy-rope-report--find-test-helper ()
  "Find the test helper file.

This calls `pop-to-buffer' with flag NO-RECORD."
  (let* ((project (project-current t))
         (root (project-root project))
         (helper (expand-file-name dinghy-rope-report--test-helper-location root)))

    (unless (file-exists-p helper)
      (user-error "Project at %s has no test helper!" root))

    (pop-to-buffer (find-file-noselect helper) nil t)))

;;; -- `undercover'

(defvar undercover-force-coverage)
(defvar undercover--merge-report)
(declare-function undercover--setup "ext:undercover.el")

(defun dinghy-rope-report--setup-undercover (patterns)
  "Set up `undercover' for PATTERNS.

The text report will be printed to stdout."
  (when (require 'undercover nil t)
    (message "Setting up `undercover' with %s" patterns)

    (let ((report-format 'text)
          (report-file dinghy-rope-report--text-file))

      (setq undercover-force-coverage t)

      (cond
       ((getenv dinghy-rope-report--env-ci)
        (setq report-format 'lcov
              report-file nil))

       ((getenv dinghy-rope-report--env-coverage-with-json)
        (setq undercover--merge-report nil
              report-format 'simplecov
              report-file dinghy-rope-report--json-file)))

      (undercover--setup
       (append patterns
               (list
                (list :report-format report-format)
                (list :report-file report-file)
                (list :send-report nil))))

      (when (eq 'text report-format)
        (add-hook 'kill-emacs-hook #'dinghy-rope-report--undercover-result 'last)))))

;;; -- Paths

(defvar dinghy-rope-path--env-github-workspace "GITHUB_WORKSPACE"
  "Location of the project in GitHub action.")

(defun dinghy-rope-path--setup-paths (paths)
  "Set up `load-path'.

Optionally, set up additional relative PATHS.

This function returns a list of the directories added to the
`load-path'."
  (let* ((source-dir (expand-file-name (or (getenv dinghy-rope-path--env-github-workspace)
                                           default-directory)))
         (paths (append (list source-dir) (mapcar (lambda (it) (expand-file-name it source-dir)) paths))))

    (message "Adding %s to `load-path'" paths)

    (dolist (it paths)
      (add-to-list 'load-path it))

    paths))

;;; -- API

;;;###autoload
(defun dinghy-rope-setup-paths (&optional paths)
  "Add PATHS to load path in a CI-aware way."
  (dinghy-rope-path--setup-paths paths))

;;;###autoload
(defun dinghy-rope-setup-ert-runner (&optional reporter)
  "Set up `ert-runner'.

An optional REPORTER function can be passed."
  (dinghy-rope-report--setup-ert-runner reporter))

;;;###autoload
(defun dinghy-rope-setup-undercover (patterns)
  "Set up `undercover' for PATTERNS."
  (dinghy-rope-report--setup-undercover patterns))

;;;###autoload
(defun dinghy-rope-find-test-helper ()
  "Find the test helper for the current project."
  (interactive)

  (dinghy-rope-report--find-test-helper))

;;;###autoload
(cl-defun dinghy-rope-setup-ert (&key increase-print-depth)
  "Setup `ert'.

If INCREASE-PRINT-DEPTH is t, `ert-batch-print-level' will be
greatly increased. This is especially useful when using
`dinghy-match-expansion' if the macro to match is complex."
  (defvar ert-batch-print-level)

  (when increase-print-depth
    (setq ert-batch-print-level 10)))

(provide 'dinghy-rope)

;;; dinghy-rope.el ends here
