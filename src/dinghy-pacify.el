;;; dinghy-pacify.el --- Check files with `flymake' -*- lexical-binding: t; -*-

;; Author: Krister Schuchardt <krister.schuchardt@gmail.com>
;; Homepage: https://github.com/Walheimat/dinghy
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: files

;;; Commentary:
;;
;; Checks Lisp files for warnings and errors.

;;; Code:

(require 'warnings)
(require 'flymake)

(defgroup dinghy-pacify nil
  "Customize `dinghy-pacify'."
  :group 'dinghy-pacify)

(defcustom dinghy-pacify-not-testable nil
  "Regex matching files that are currently not testable."
  :type 'regexp
  :group 'dinghy-pacify)

(defun dinghy-pacify--get-state (state)
  "Get the STATE."
  (when (hash-table-p flymake--state)
    (gethash state flymake--state)))

(defun dinghy-pacify--ready-p ()
  "Check if the report concluded."
  (when-let ((b-state (dinghy-pacify--get-state 'elisp-flymake-byte-compile))
             (d-state (dinghy-pacify--get-state 'elisp-flymake-checkdoc)))

    (and (flymake--state-reported-p b-state)
         (flymake--state-reported-p d-state))))

(defun dinghy-pacify--get-diags ()
  "Get all all state dialogs."
  (append (flymake--state-diags (dinghy-pacify--get-state 'elisp-flymake-byte-compile))
          (flymake--state-diags (dinghy-pacify--get-state 'elisp-flymake-checkdoc))))

(defun dinghy-pacify--get-severity-diags (severity)
  "Get all dialogs of SEVERITY."
  (cl-loop for diag in (dinghy-pacify--get-diags)
           if (eq (flymake--severity (flymake-diagnostic-type diag)) severity)
           collect (dinghy-pacify--get-info diag)))

(defvar dinghy-pacify--infos nil)
(defvar dinghy-pacify--debugs nil)
(defvar dinghy-pacify--warnings nil)
(defvar dinghy-pacify--errors nil)

(defvar dinghy-pacify--info 0)
(defvar dinghy-pacify--debug (warning-numeric-level :debug))
(defvar dinghy-pacify--warning (warning-numeric-level :warning))
(defvar dinghy-pacify--error (warning-numeric-level :error))

(defun dinghy-pacify--get-info (diag)
  "Get the relevant info from DIAG."
  (let ((text (flymake--diag-text diag))
        (beg (flymake--diag-beg diag))
        (locus (flymake--diag-locus diag)))

    (list :file (buffer-file-name locus) :line (line-number-at-pos beg) :text text)))

(defconst dinghy-pacify--checkers '(elisp-flymake-byte-compile elisp-flymake-checkdoc))

(defun dinghy-pacify--collect (file)
  "Collect reports for FILE."
  (let ((counter 0))

    (with-current-buffer (find-file-noselect file)
      (setq sentence-end-double-space nil)
      (setq flymake-diagnostic-functions dinghy-pacify--checkers)

      (flymake-mode)
      (flymake-start nil)

      (while (and (not (dinghy-pacify--ready-p)) (< counter 100))
        (setq counter (1+ counter))
        (sit-for 0.05))

      (unless (dinghy-pacify--ready-p)
        (error "Waited for five seconds for check to complete"))

      (let ((warnings (dinghy-pacify--get-severity-diags dinghy-pacify--warning))
            (errors (dinghy-pacify--get-severity-diags dinghy-pacify--error))
            (debugs (dinghy-pacify--get-severity-diags dinghy-pacify--debug))
            (infos (dinghy-pacify--get-severity-diags dinghy-pacify--info)))

        (setq dinghy-pacify--warnings (append dinghy-pacify--warnings warnings)
              dinghy-pacify--errors (append dinghy-pacify--errors errors)
              dinghy-pacify--debugs (append dinghy-pacify--debugs debugs)
              dinghy-pacify--infos (append dinghy-pacify--infos infos))))))

(defun dinghy-pacify--format (info)
  "Format INFO for output."
  (let ((file (plist-get info :file))
        (line (plist-get info :line))
        (text (plist-get info :text)))

    (format "%s:%s: %s" file line text)))

(defun dinghy-pacify-check--get-package-files ()
  "Get all testable package files."
  (let* ((files (shell-command-to-string "cask files"))
         (trimmed (string-trim files))
         (names (split-string trimmed "\n")))

    (if (stringp dinghy-pacify-not-testable)
        (progn
          (message "Filtering files matching %s" dinghy-pacify-not-testable)
          (seq-filter
           (lambda (it) (not (string-match dinghy-pacify-not-testable it)))
           names))
      names)))

(defun dinghy-pacify-check ()
  "Check all package files."
  (message "Checking package files with `flymake'")

  (condition-case err
      (dolist (it (dinghy-pacify-check--get-package-files))
        (dinghy-pacify--collect it))
    (error
     (message "Failed to check all of the packages: %s" (error-message-string err))
     (kill-emacs 0)))

  (let* ((severe (append dinghy-pacify--errors dinghy-pacify--warnings))
         (other (append dinghy-pacify--debugs dinghy-pacify--infos)))

    (when other
      (message "Found %s note(s)" (length other))
      (dolist (it other)
        (message (dinghy-pacify--format it))))

    (when severe
      (message "Found %s error(s)" (length severe))

      (dolist (it severe)
        (message (dinghy-pacify--format it)))
      (kill-emacs 1))))

(provide 'dinghy-pacify)

;;; dinghy-pacify.el ends here
