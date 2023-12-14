;;; dinghy-rope-test.el --- Tests for custom functionality. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for the custom functionality.

;;; Code:

(require 'dinghy-rope)

(defvar coverage-file (ert-resource-file "coverage.txt"))
(defvar mock-coverage-file (ert-resource-file "mock-coverage.txt"))

(ert-deftest dinghy-rope-report--matches-in-string ()
  (let ((str "This 1 string has 3 matches, or is it 2?")
        (pattern "\\(?1:[[:digit:]]\\)"))

    (should (equal '("2" "3" "1") (dinghy-rope-report--matches-in-string pattern str)))))

(ert-deftest dinghy-rope-report--report ()
  (bydi (message)
    (let ((dinghy-rope-report--temp-files nil))
      (dinghy-rope-report--print-temp-files)
      (bydi-was-not-called message)

      (dinghy-rope-report--record-temp-file "test")
      (dinghy-rope-report--print-temp-files)
      (bydi-was-called-with message (list "\nCreated the following temp files:\n%s"
                                          '("test"))))))

;; This test will actually print out the coverage for `bydi', which is
;; a nice side effect.
(ert-deftest dinghy-rope-setup-undercover ()
  (bydi (undercover--setup
         (:mock getenv :with ignore))

    (shut-up
      (dinghy-rope-setup-undercover (list "bydi.el")))

    (bydi-was-called-with undercover--setup '(("bydi.el" (:report-format text)
                                               (:report-file "./coverage/results.txt")
                                               (:send-report nil))))))

(ert-deftest dinghy-rope-setup-undercover--ci ()
  (bydi (undercover--setup
         (:mock getenv :with (lambda (r) (string= "CI" r))))

    (shut-up
      (dinghy-rope-setup-undercover (list "bydi.el")))

    (bydi-was-called-with undercover--setup '(("bydi.el" (:report-format lcov)
                                               (:report-file nil)
                                               (:send-report nil))))))

(ert-deftest dinghy-rope-report--setup-undercover--json ()
  (bydi (undercover--setup
         (:mock getenv :with (lambda (r) (string= "COVERAGE_WITH_JSON" r))))

    (shut-up
      (dinghy-rope-report--setup-undercover (list "bydi.el")))

    (bydi-was-called-with undercover--setup '(("bydi.el" (:report-format simplecov)
                                               (:report-file "./coverage/.resultset.json")
                                               (:send-report nil))))))

(ert-deftest dinghy-rope-report--undercover-result ()
  (let ((dinghy-rope-report--text-file mock-coverage-file))

    (shut-up
      (ert-with-message-capture messages

        (bydi ((:mock dinghy-rope-report--consume-undercover-report :return '(10 3 2 1)))
          (dinghy-rope-report--undercover-result))

        (should (string=
                 "COVERAGE\n\nAverage : Percent 10% [Relevant: 3 Covered: 2 Missed: 1]\n\n"
                 messages))))))

(ert-deftest dinghy-rope-setup-ert-runner ()
  (bydi (add-hook)

    (dinghy-rope-setup-ert-runner 'always)

    (bydi-was-called add-hook)

    (bydi-was-called-nth-with add-hook '(ert-runner-reporter-run-ended-functions dinghy-rope-report--print-temp-files) 0)
    (bydi-was-called-nth-with add-hook '(ert-runner-reporter-run-ended-functions always) 1)))

(ert-deftest dinghy-rope-report--consume-undercover-report ()
  (let ((dinghy-rope-report--text-file coverage-file))

    (should (equal '(37.33 225 84 141) (dinghy-rope-report--consume-undercover-report)))))

(ert-deftest dinghy-rope-report--find-test-helper ()

  (defvar dinghy-rope-report--test-helper-location)

  (ert-with-temp-file helper-file
    (let ((root (file-name-parent-directory helper-file)))

      (bydi (project-current
             (:mock project-root :return root)
             pop-to-buffer)

        (should-error (dinghy-rope-find-test-helper))

        (setq dinghy-rope-report--test-helper-location (string-trim-left helper-file "/tmp/"))

        (dinghy-rope-find-test-helper)

        (bydi-was-called pop-to-buffer)))))

(ert-deftest dinghy-rope-setup-ert ()
  (defvar ert-batch-print-level)
  (bydi ((:watch ert-batch-print-level))

    (dinghy-rope-setup-ert :increase-print-depth t)

    (bydi-was-set-to ert-batch-print-level 10)))

(ert-deftest dinghy-rope-setup-paths ()
  (let ((load-path nil)
        (default-directory "/tmp"))

    (bydi ((:ignore getenv))

      (shut-up
        (dinghy-rope-setup-paths (list "test" "mock")))

      (should (equal load-path '("/tmp/mock" "/tmp/test" "/tmp"))))))

;;; dinghy-rope-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
