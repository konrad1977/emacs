;;; Test regex against the actual line

(load-file "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/periphery-config.el")

(let ((test-line "/Users/mikaelkonradsson/Documents/git/bruce/bruce-ios/Bruce/Screens/Classes/Views/ClassesDateHeader.swift:17:44: error: type 'Style.Color' has no member 'primaryElemen'")
      (regex (alist-get 'compiler periphery-builtin-patterns)))
  
  (message "\n=== REGEX TEST ===")
  (message "Test line: %s" test-line)
  (message "Regex: %s" regex)
  
  (if (string-match regex test-line)
      (progn
        (message "SUCCESS - Regex matches!")
        (message "Groups:")
        (message "  1 (file): %s" (match-string 1 test-line))
        (message "  2 (line): %s" (match-string 2 test-line))
        (message "  3 (col):  %s" (match-string 3 test-line))
        (message "  4 (type): %s" (match-string 4 test-line))
        (message "  5 (msg):  %s" (match-string 5 test-line)))
    (message "FAILED - Regex does NOT match!")))

;; Test a simpler pattern too
(let ((simple-regex "\\(/[^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\s*\\(\\w+\\):\\s*\\(.*\\)"))
  (message "\n=== SIMPLE REGEX TEST ===")
  (message "Simple regex: %s" simple-regex)
  (if (string-match simple-regex test-line)
      (progn
        (message "Simple regex MATCHES!")
        (message "Groups:")
        (message "  1: %s" (match-string 1 test-line))
        (message "  2: %s" (match-string 2 test-line))
        (message "  3: %s" (match-string 3 test-line))
        (message "  4: %s" (match-string 4 test-line))
        (message "  5: %s" (match-string 5 test-line)))
    (message "Simple regex does NOT match!")))