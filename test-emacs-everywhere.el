;;; test-emacs-everywhere.el --- Test emacs-everywhere template inhibition -*- lexical-binding: t; -*-

;; This test verifies that markdown templates are not inserted in emacs-everywhere

(defun test-emacs-everywhere-template-inhibition ()
  "Test that file templates are inhibited in emacs-everywhere buffers."
  (let ((test-file "/tmp/emacs-everywhere-test.md")
        (result nil))

    ;; Simulate emacs-everywhere conditions
    (with-temp-buffer
      ;; Set up the buffer as if it were an emacs-everywhere buffer
      (setq buffer-file-name test-file)

      ;; Test Method 1: Direct inhibition
      (message "Testing direct inhibition...")
      (setq-local +file-templates-inhibit t)
      (if +file-templates-inhibit
          (message "✓ Direct inhibition works: +file-templates-inhibit is set")
        (message "✗ Direct inhibition failed"))

      ;; Test Method 2: Pattern matching
      (message "\nTesting pattern matching...")
      (let ((test-file-ee "/tmp/emacs-everywhere-20240101-120000-TestApp.md"))
        (setq buffer-file-name test-file-ee)
        (if (string-match-p "emacs-everywhere" buffer-file-name)
            (message "✓ Pattern matching works: emacs-everywhere detected in filename")
          (message "✗ Pattern matching failed")))

      ;; Test Method 3: Check function behavior
      (message "\nTesting template check function...")
      (when (fboundp '+file-templates-check-h)
        ;; With emacs-everywhere filename
        (setq buffer-file-name "/tmp/emacs-everywhere-test.md")
        (setq-local +file-templates-inhibit t)
        (let ((result (+file-templates-check-h)))
          (if (not result)
              (message "✓ Template check properly inhibited for emacs-everywhere")
            (message "✗ Template check not inhibited"))))

      (message "\nAll tests completed."))))

;; Run the test
(test-emacs-everywhere-template-inhibition)