;;; Debug the full parsing chain

;; Load all components
(load-file "periphery-config.el")
(load-file "periphery-core.el")
(load-file "periphery-parsers.el")
(load-file "periphery-helper.el")
(load-file "periphery.el")

(setq periphery-debug t)

(message "\n===== DEBUGGING FULL PARSING CHAIN =====\n")

;; Your exact error
(defvar test-error "/Users/mikaelkonradsson/Documents/git/bruce/bruce-ios/Bruce/Screens/Classes/Views/ClassesDateHeader.swift:17:44: error: type 'Style.Color' has no member 'primaryElemen'")

;; Step 1: Check if parsers are initialized
(message "Step 1: Checking parser registration")
(message "Number of registered parsers: %d" (hash-table-count periphery-registered-parsers))
(let ((compiler-parser (gethash 'compiler periphery-registered-parsers)))
  (if compiler-parser
      (progn
        (message "Compiler parser found:")
        (message "  Name: %s" (plist-get compiler-parser :name))
        (message "  Type: %s" (plist-get compiler-parser :type))
        (message "  Enabled: %s" (plist-get compiler-parser :enabled))
        (message "  Parse-fn: %s" (plist-get compiler-parser :parse-fn)))
    (message "ERROR: Compiler parser NOT FOUND!")))

;; Step 2: Test the regex directly
(message "\nStep 2: Testing regex match")
(let ((regex (alist-get 'compiler periphery-builtin-patterns)))
  (message "Regex: %s" regex)
  (if (string-match regex test-error)
      (message "Regex MATCHES")
    (message "ERROR: Regex does NOT match")))

;; Step 3: Test the parser function directly
(message "\nStep 3: Testing parser function")
(let ((result (periphery-parser-compiler test-error)))
  (if result
      (progn
        (message "Parser returned a result:")
        (message "  Result type: %s" (type-of result))
        (message "  Result: %S" result))
    (message "ERROR: Parser returned nil")))

;; Step 4: Test periphery-core-build-entry
(message "\nStep 4: Testing periphery-core-build-entry")
(let ((entry (periphery-core-build-entry
              :path "/test/file.swift:10:5"
              :file "/test/file.swift"
              :line "10"
              :column "5"
              :severity "error"
              :message "test message"
              :face-fn (lambda (s) 'periphery-error-face))))
  (message "Built entry: %S" entry))

;; Step 5: Test through periphery-core-parse
(message "\nStep 5: Testing periphery-core-parse")
(let ((results (periphery-core-parse :input test-error :type :compiler)))
  (message "Core parse results: %d items" (length results))
  (when results
    (message "First result: %S" (car results))))

;; Step 6: Test through periphery-run-parser
(message "\nStep 6: Testing periphery-run-parser")
(setq periphery-errorList nil)
(let ((has-errors (periphery-run-parser test-error)))
  (message "periphery-run-parser returned: %s" has-errors)
  (message "periphery-errorList has %d items" (length periphery-errorList))
  (when periphery-errorList
    (message "First error: %S" (car periphery-errorList))))

;; Step 7: Check if the display buffer would show
(message "\nStep 7: Checking display")
(if (get-buffer periphery-buffer-name)
    (message "Buffer %s exists" periphery-buffer-name)
  (message "Buffer %s does not exist" periphery-buffer-name))