;;; Debug each part of the regex

(let ((test-line "/Users/mikaelkonradsson/Documents/git/bruce/bruce-ios/Bruce/Screens/Classes/Views/ClassesDateHeader.swift:17:44: error: type 'Style.Color' has no member 'primaryElemen'"))
  
  (message "\n=== DETAILED REGEX DEBUG ===")
  (message "Test line: %s" test-line)
  
  ;; Test up to column with optional group
  (let ((regex1 "\\(/[^:]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)?"))
    (if (string-match regex1 test-line)
        (progn
          (message "✓ File+line+optional_col matches")
          (message "  Match: '%s'" (match-string 0 test-line))
          (message "  Groups: file='%s' line='%s' col='%s'" 
                   (match-string 1 test-line)
                   (match-string 2 test-line) 
                   (match-string 3 test-line)))
      (message "✗ File+line+optional_col FAILS")))
  
  ;; Test what comes after the column part
  (message "Characters after :44 are: '%s'" (substring test-line 104))
  
  ;; Test just the severity pattern after :44
  (let ((after-col ": error: type 'Style.Color' has no member 'primaryElemen'"))
    (message "Testing against: '%s'" after-col)
    
    ;; Test different space patterns
    (if (string-match ":\\s+\\(\\w+\\):" after-col)
        (message "✓ Space+word+colon matches: '%s'" (match-string 1 after-col))
      (message "✗ Space+word+colon FAILS"))
    
    (if (string-match ":\\s*\\(\\w+\\):" after-col)
        (message "✓ Optional_space+word+colon matches: '%s'" (match-string 1 after-col))
      (message "✗ Optional_space+word+colon FAILS"))
    
    (if (string-match ":\\s+\\(\\w+\\):\\s*\\(.*\\)" after-col)
        (message "✓ Full pattern matches: type='%s' msg='%s'" 
                 (match-string 1 after-col) (match-string 2 after-col))
      (message "✗ Full pattern FAILS")))
  
  ;; Test the complete regex piece by piece
  (let ((complete-regex "\\(/[^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\s+\\(\\w+\\):\\s*\\(.*\\)"))
    (message "\nTesting complete regex: %s" complete-regex)
    (if (string-match complete-regex test-line)
        (message "✓ Complete regex matches!")
      (message "✗ Complete regex FAILS"))))