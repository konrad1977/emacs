;;; Quick test for periphery parsing

(load-file "periphery-config.el")
(load-file "periphery-core.el") 
(load-file "periphery-parsers.el")
(load-file "periphery.el")

;; Enable debug
(setq periphery-debug t)

;; Test simple compiler output
(let ((test-input "/Users/test/file.swift:10:5: error: test error message"))
  (message "\n===== Testing Periphery Parsing =====")
  (message "Input: %s" test-input)
  
  ;; Check if parsers are registered
  (message "\nRegistered parsers:")
  (maphash (lambda (id config)
             (message "  %s: enabled=%s" id (plist-get config :enabled)))
           periphery-registered-parsers)
  
  ;; Test regex directly
  (message "\nTesting regex match:")
  (let ((regex (alist-get 'compiler periphery-builtin-patterns)))
    (message "Regex: %s" regex)
    (if (string-match regex test-input)
        (progn
          (message "MATCH!")
          (message "  1: %s" (match-string 1 test-input))
          (message "  2: %s" (match-string 2 test-input))
          (message "  3: %s" (match-string 3 test-input))
          (message "  4: %s" (match-string 4 test-input))
          (message "  5: %s" (match-string 5 test-input)))
      (message "NO MATCH")))
  
  ;; Test parser directly
  (message "\nTesting parser directly:")
  (let ((result (periphery-parser-compiler test-input)))
    (if result
        (message "Parser result: %S" result)
      (message "Parser returned nil")))
  
  ;; Test through core
  (message "\nTesting through periphery-core-parse:")
  (let ((results (periphery-core-parse :input test-input :type :compiler)))
    (message "Results: %S" results))
  
  ;; Test through periphery-run-parser
  (message "\nTesting through periphery-run-parser:")
  (periphery-run-parser test-input)
  (message "periphery-errorList: %S" periphery-errorList))