;;; Test the fixed regex

(let ((test-line "/Users/mikaelkonradsson/Documents/git/bruce/bruce-ios/Bruce/Screens/Classes/Views/ClassesDateHeader.swift:17:44: error: type 'Style.Color' has no member 'primaryElemen'")
      (fixed-regex "\\(/[^:]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)?\\s+\\(\\w+\\):\\s*\\(.*\\)"))
  
  (message "\n=== FIXED REGEX TEST ===")
  (message "Test line: %s" test-line)
  (message "Fixed regex: %s" fixed-regex)
  
  (if (string-match fixed-regex test-line)
      (progn
        (message "SUCCESS - Fixed regex matches!")
        (message "Groups:")
        (message "  1 (file): %s" (match-string 1 test-line))
        (message "  2 (line): %s" (match-string 2 test-line))
        (message "  3 (col):  %s" (match-string 3 test-line))
        (message "  4 (type): %s" (match-string 4 test-line))
        (message "  5 (msg):  %s" (match-string 5 test-line)))
    (message "FAILED - Fixed regex still doesn't match!")))