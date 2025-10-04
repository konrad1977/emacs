;;; Test with no space before error

(let ((test-line "/Users/mikaelkonradsson/Documents/git/bruce/bruce-ios/Bruce/Screens/Classes/Views/ClassesDateHeader.swift:17:44: error: type 'Style.Color' has no member 'primaryElemen'"))
  
  (message "\n=== NO SPACE TEST ===")
  
  ;; Test the actual pattern in the line
  (let ((after-44 ": error: type 'Style.Color' has no member 'primaryElemen'"))
    (message "Testing against: '%s'" after-44)
    
    ;; No space between : and error
    (if (string-match ":\\s*\\(\\w+\\):" after-44)
        (message "✓ No space pattern matches: '%s'" (match-string 1 after-44))
      (message "✗ No space pattern FAILS"))
    
    ;; Literal space
    (if (string-match ": \\(\\w+\\):" after-44)
        (message "✓ Literal space pattern matches: '%s'" (match-string 1 after-44))
      (message "✗ Literal space pattern FAILS")))
  
  ;; Test complete regex with literal space
  (let ((regex-literal-space "\\(/[^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(\\w+\\): \\(.*\\)"))
    (message "\nTesting with literal space: %s" regex-literal-space)
    (if (string-match regex-literal-space test-line)
        (progn
          (message "✓ Literal space regex MATCHES!")
          (message "Groups:")
          (message "  1 (file): %s" (match-string 1 test-line))
          (message "  2 (line): %s" (match-string 2 test-line))
          (message "  3 (col):  %s" (match-string 3 test-line))
          (message "  4 (type): %s" (match-string 4 test-line))
          (message "  5 (msg):  %s" (match-string 5 test-line)))
      (message "✗ Literal space regex FAILS"))))