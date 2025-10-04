;;; Test exact error from user

(load-file "periphery-config.el")
(load-file "periphery-core.el")
(load-file "periphery-parsers.el")

(setq periphery-debug t)

(let ((error-line "/Users/mikaelkonradsson/Documents/git/bruce/bruce-ios/Bruce/Screens/Classes/Views/ClassesDateHeader.swift:17:44: error: type 'Style.Color' has no member 'primaryElemen'"))
  
  (message "\nTesting exact error string:")
  (message "Input: %s" error-line)
  
  ;; Test regex
  (let ((regex (alist-get 'compiler periphery-builtin-patterns)))
    (message "\nRegex: %s" regex)
    (if (string-match regex error-line)
        (progn
          (message "MATCH SUCCESS!")
          (message "  File (1): %s" (match-string 1 error-line))
          (message "  Line (2): %s" (match-string 2 error-line))
          (message "  Col  (3): %s" (match-string 3 error-line))
          (message "  Type (4): %s" (match-string 4 error-line))
          (message "  Msg  (5): %s" (match-string 5 error-line)))
      (message "NO MATCH - REGEX FAILED")))
  
  ;; Test parser
  (message "\nTesting parser:")
  (let ((result (periphery-parser-compiler error-line)))
    (if result
        (message "Parser worked! Result: %S" result)
      (message "Parser returned nil"))))