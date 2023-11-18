;;; dinghy-pacify-test.el --- Test pacify package. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests byte-compiler checker. This file makes sure that actual
;; `flycheck' is not loaded and `use-package' forms are stumped.

;;; Code:

(provide 'flymake)

(require 'dinghy-pacify nil t)

(defvar flymake--state nil)

(defmacro with-flymake-state (&rest body)
  "Execute BODY with `flymake' state."
  (declare (indent 0))

  `(let ((flymake--state ,(make-hash-table)))

     (puthash 'elisp-flymake-byte-compile 'byte flymake--state)
     (puthash 'elisp-flymake-checkdoc 'docs flymake--state)

     ,@body))

(ert-deftest dp-get-state ()
  (with-flymake-state
   (should-not (dinghy-pacify--get-state 'elisp-flymake-non-existence))
   (should (dinghy-pacify--get-state 'elisp-flymake-byte-compile))))

(ert-deftest dp--ready-p ()
  (with-flymake-state
   (bydi (flymake--state-reported-p)
     (should (dinghy-pacify--ready-p)))))

(ert-deftest dp--get-diags--appends ()
  (bydi ((:mock flymake--state-diags :return '(test))
         dinghy-pacify--get-state)
    (should (equal '(test test) (dinghy-pacify--get-diags)))))

(ert-deftest dp--get-severity-diags ()
  (bydi ((:mock dinghy-pacify--get-diags :return '(1 2 3 4 3))
         (:mock dinghy-pacify--get-info :with bydi-rf)
         (:mock flymake--severity :with bydi-rf)
         (:mock flymake-diagnostic-type :with bydi-rf))
    (should (equal '(3 3) (dinghy-pacify--get-severity-diags 3)))))

(ert-deftest dp--get-info ()
  (ert-with-temp-file info

    (bydi ((:mock flymake--diag-text :return "test")
           (:mock flymake--diag-beg :return 1)
           (:mock flymake--diag-locus :with (lambda (_) (get-buffer info))))

      (should (equal (dinghy-pacify--get-info nil)
                     (list :file (buffer-file-name (get-buffer info))
                           :line 1
                           :text "test"))))))

(ert-deftest dp--collect--collects-if-ready ()
  :tags '(hi)

  (let ((severities '(warning error debug info)))
    (ert-with-temp-file ready
      (bydi (flymake-mode
             flymake-start
             (:always dinghy-pacify--ready-p)
             (:mock dinghy-pacify--get-severity-diags :with (lambda (_) (pop severities))))

        (dinghy-pacify--collect ready)

        (bydi-was-called-n-times dinghy-pacify--get-severity-diags 4)))))

(ert-deftest dp--collect--errors-if-never-ready ()
  :tags '(test)

  (ert-with-temp-file never
    (bydi (flymake-mode
           flymake-start
           (:ignore dinghy-pacify--ready-p)
           sit-for)

      (should-error (dinghy-pacify--collect never) :type 'error))))

(ert-deftest dp--format ()
  (let ((info '(:file "test.txt" :line 42 :text "Answer revealed")))

    (should (string= "test.txt:42: Answer revealed" (dinghy-pacify--format info)))))

(ert-deftest dp-check--get-package-files ()
  (defvar dinghy-pacify-check--not-testable)

  (let ((dinghy-pacify-not-testable "notest\\|leaveme"))

    (bydi ((:mock shell-command-to-string :return "wal-notest.el\nwal-dotest.el\nwal-do-leaveme.el\nwal-useme.el\n"))

      (shut-up
        (should (equal '("wal-dotest.el" "wal-useme.el") (dinghy-pacify-check--get-package-files)))))))

(ert-deftest dp-check--get-package-files--returns-if-no-filter ()
  (defvar dinghy-pacify-check--not-testable)

  (let ((dinghy-pacify-not-testable nil))

    (bydi ((:mock shell-command-to-string :return "wal-notest.el\nwal-do-leaveme.el\n"))

      (should (equal '("wal-notest.el" "wal-do-leaveme.el") (dinghy-pacify-check--get-package-files))))))


(defvar dinghy-pacify--errors)
(defvar dinghy-pacify--warnings)
(defvar dinghy-pacify--debugs)
(defvar dinghy-pacify--infos)

(ert-deftest dp-check--exits-with-0-on-check-error ()
  (let ((dinghy-pacify--errors nil)
        (dinghy-pacify--warnings nil)
        (dinghy-pacify--debugs nil)
        (dinghy-pacify--infos nil))

    (bydi ((:mock dinghy-pacify-check--get-package-files :return '(one two three))
           (:mock dinghy-pacify--collect :with (lambda (_) (error "Oops")))
           wal-pacif--format
           kill-emacs)

      (shut-up
        (dinghy-pacify-check))

      (bydi-was-called-with kill-emacs 0))))

(ert-deftest dp-check--prints-infos-and-debugs-without-exit ()
  (let ((dinghy-pacify--errors nil)
        (dinghy-pacify--warnings nil)
        (dinghy-pacify--debugs '(one two))
        (dinghy-pacify--infos '(three four)))

    (bydi ((:mock dinghy-pacify-check--get-package-files :return '(one two three))
           dinghy-pacify--collect
           dinghy-pacify--format
           message
           kill-emacs)

      (dinghy-pacify-check)

      (bydi-was-not-called kill-emacs)

      (bydi-was-called-n-times dinghy-pacify--format 4))))

(ert-deftest dp-check--exits-with-0-on-severe-errors ()
  (let ((dinghy-pacify--errors '(one two))
        (dinghy-pacify--warnings '(three four))
        (dinghy-pacify--debugs nil)
        (dinghy-pacify--infos nil))

    (bydi ((:mock dinghy-pacify-check--get-package-files :return '(one two three))
           dinghy-pacify--collect
           dinghy-pacify--format
           message
           kill-emacs)

      (dinghy-pacify-check)

      (bydi-was-called-with kill-emacs 1)
      (bydi-was-called-n-times dinghy-pacify--format 4))))

;;; dinghy-pacify-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
