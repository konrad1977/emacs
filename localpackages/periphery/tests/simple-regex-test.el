;;; Simple regex test without loading files

(let ((test-line "/Users/mikaelkonradsson/Documents/git/bruce/bruce-ios/Bruce/Screens/Classes/Views/ClassesDateHeader.swift:17:44: error: type 'Style.Color' has no member 'primaryElemen'")
      (regex "\\(/[^:]+\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)?\\s*\\(\\w+\\):\\s*\\(.*\\)"))
  
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

;; Test without optional column
(let ((test-line "/Users/mikaelkonradsson/Documents/git/bruce/bruce-ios/Bruce/Screens/Classes/Views/ClassesDateHeader.swift:17:44: error: type 'Style.Color' has no member 'primaryElemen'")
      (regex2 "\\(/[^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\s*\\(\\w+\\):\\s*\\(.*\\)"))
  
  (message "\n=== REGEX TEST 2 (required column) ===")
  (message "Regex2: %s" regex2)
  
  (if (string-match regex2 test-line)
      (progn
        (message "SUCCESS - Regex2 matches!")
        (message "Groups:")
        (message "  1 (file): %s" (match-string 1 test-line))
        (message "  2 (line): %s" (match-string 2 test-line))
        (message "  3 (col):  %s" (match-string 3 test-line))
        (message "  4 (type): %s" (match-string 4 test-line))
        (message "  5 (msg):  %s" (match-string 5 test-line)))
    (message "FAILED - Regex2 does NOT match!")))

;; Test each part step by step
(let ((test-line "/Users/mikaelkonradsson/Documents/git/bruce/bruce-ios/Bruce/Screens/Classes/Views/ClassesDateHeader.swift:17:44: error: type 'Style.Color' has no member 'primaryElemen'"))
  (message "\n=== STEP BY STEP TEST ===")
  
  ;; Test just the file part
  (if (string-match "\\(/[^:]+\\)" test-line)
      (message "File part matches: %s" (match-string 1 test-line))
    (message "File part FAILS"))
  
  ;; Test file + line
  (if (string-match "\\(/[^:]+\\):\\([0-9]+\\)" test-line)
      (message "File+line matches: %s:%s" (match-string 1 test-line) (match-string 2 test-line))
    (message "File+line FAILS"))
  
  ;; Test file + line + column
  (if (string-match "\\(/[^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" test-line)
      (message "File+line+col matches: %s:%s:%s" (match-string 1 test-line) (match-string 2 test-line) (match-string 3 test-line))
    (message "File+line+col FAILS"))
  
  ;; Test the severity part
  (if (string-match ":\\s*\\(\\w+\\):" test-line)
      (message "Severity matches: %s" (match-string 1 test-line))
    (message "Severity FAILS")))