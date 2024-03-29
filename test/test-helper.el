;;; test-helper.el --- Test helpers. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Helper macros and functions.

;;; Code:

(require 'bydi)
(require 'undercover)

(let ((source-dir (expand-file-name (or (getenv "GITHUB_WORKSPACE")
                                        default-directory)))
      (report-format 'text)
      (report-file "./coverage/results.txt"))

  (add-to-list 'load-path source-dir)

  (setq undercover-force-coverage t)

  (cond
   ((getenv "CI")
    (setq report-format 'lcov
          report-file nil))

   ((getenv "COVERAGE_WITH_JSON")
    (setq undercover--merge-report nil
          report-format 'simplecov
          report-file "./coverage/.resultset.json")))

  (undercover--setup
   (append '("src/dinghy-pacify.el"
             "src/dinghy-rope.el")

           (list
            (list :report-format report-format)
            (list :report-file report-file)
            (list :send-report nil)))))

(setq ert-batch-print-level 20)

;;; test-helper.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
