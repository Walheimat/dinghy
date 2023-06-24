;;; dinghy-pacify.el --- Check files with `flymake'. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Checks Lisp files for warnings and errors.

;;; Code:

(require 'warnings)
(require 'flymake)

(defgroup dinghy-pacify nil
  "Customize `dinghy-pacify'."
  :group 'dinghy-pacify)

(defcustom dp-not-testable nil
  "Regex matching files that are currently not testable."
  :type 'regexp
  :group 'dinghy-pacify)

(defun dp--get-state (state)
  "Get the STATE."
  (when (hash-table-p flymake--state)
    (gethash state flymake--state)))

(defun dp--ready-p ()
  "Check if the report concluded."
  (when-let ((b-state (dp--get-state 'elisp-flymake-byte-compile))
             (d-state (dp--get-state 'elisp-flymake-checkdoc)))

    (and (flymake--state-reported-p b-state)
         (flymake--state-reported-p d-state))))

(defun dp--get-diags ()
  "Get all all state dialogs."
  (append (flymake--state-diags (dp--get-state 'elisp-flymake-byte-compile))
          (flymake--state-diags (dp--get-state 'elisp-flymake-checkdoc))))

(defun dp--get-severity-diags (severity)
  "Get all dialogs of SEVERITY."
  (cl-loop for diag in (dp--get-diags)
           if (eq (flymake--severity (flymake-diagnostic-type diag)) severity)
           collect (dp--get-info diag)))

(defvar dp--infos nil)
(defvar dp--debugs nil)
(defvar dp--warnings nil)
(defvar dp--errors nil)

(defvar dp--info 0)
(defvar dp--debug (warning-numeric-level :debug))
(defvar dp--warning (warning-numeric-level :warning))
(defvar dp--error (warning-numeric-level :error))

(defun dp--get-info (diag)
  "Get the relevant info from DIAG."
  (let ((text (flymake--diag-text diag))
        (beg (flymake--diag-beg diag))
        (locus (flymake--diag-locus diag)))

    (list :file (buffer-file-name locus) :line (line-number-at-pos beg) :text text)))

(defconst dp--checkers '(elisp-flymake-byte-compile elisp-flymake-checkdoc))

(defun dp--collect (file)
  "Collect reports for FILE."
  (let ((counter 0))

    (with-current-buffer (find-file-noselect file)
      (setq sentence-end-double-space nil)
      (setq flymake-diagnostic-functions dp--checkers)

      (flymake-mode)
      (flymake-start nil)

      (while (and (not (dp--ready-p)) (< counter 100))
        (setq counter (1+ counter))
        (sit-for 0.05))

      (unless (dp--ready-p)
        (error "Waited for five seconds for check to complete"))

      (let ((warnings (dp--get-severity-diags dp--warning))
            (errors (dp--get-severity-diags dp--error))
            (debugs (dp--get-severity-diags dp--debug))
            (infos (dp--get-severity-diags dp--info)))

        (setq dp--warnings (append dp--warnings warnings)
              dp--errors (append dp--errors errors)
              dp--debugs (append dp--debugs debugs)
              dp--infos (append dp--infos infos))))))

(defun dp--format (info)
  "Format INFO for output."
  (let ((file (plist-get info :file))
        (line (plist-get info :line))
        (text (plist-get info :text)))

    (format "%s:%s: %s" file line text)))

(defun dp-check--get-package-files ()
  "Get all testable package files."
  (let* ((files (shell-command-to-string "cask files"))
         (trimmed (string-trim files))
         (names (split-string trimmed "\n")))

    (if (stringp dp-not-testable)
        (seq-filter
         (lambda (it) (not (string-match dp-not-testable it)))
         names)
      names)))

(defun dp-check ()
  "Check all package files."
  (message "Checking package files with `flymake'")
  (when (stringp dp-not-testable)
    (message "Filtering files matching %s" dp-not-testable))

  (condition-case err
      (dolist (it (dp-check--get-package-files))
        (dp--collect it))
    (error
     (message "Failed to check all of the packages: %s" (error-message-string err))
     (kill-emacs 0)))

  (let* ((severe (append dp--errors dp--warnings))
         (other (append dp--debugs dp--infos)))

    (when other
      (message "Found %s note(s)" (length other))
      (dolist (it other)
        (message (dp--format it))))

    (when severe
      (message "Found %s error(s)" (length severe))

      (dolist (it severe)
        (message (dp--format it)))
      (kill-emacs 1))))

(provide 'dinghy-pacify)

;;; dinghy-pacify.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("dp-" . "dinghy-pacify-"))
;; End:
