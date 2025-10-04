;;; periphery-test.el --- Test periphery parsing -*- lexical-binding: t -*-

;;; Commentary:
;; Test file to debug periphery parsing issues

;;; Code:

(require 'periphery)
(require 'periphery-core)
(require 'periphery-parsers)

(defun periphery-test-compiler-output ()
  "Test parsing of compiler output."
  (interactive)
  (let ((periphery-debug t)
        (test-output "/Users/test/project/Source/File.swift:42:15: error: cannot find 'someVariable' in scope
        someVariable = 42
        ^~~~~~~~~~~~
/Users/test/project/Source/Another.swift:100:5: warning: initialization of immutable value 'unused' was never used
    let unused = computeValue()
    ^~~"))
    
    (message "========== TESTING PERIPHERY ==========")
    (message "Test output:\n%s" test-output)
    
    ;; Test direct parser
    (message "\n--- Testing direct parser ---")
    (let ((lines (split-string test-output "\n")))
      (dolist (line lines)
        (message "Line: %s" line)
        (let ((result (periphery-parser-compiler line)))
          (if result
              (message "  PARSED: %S" result)
            (message "  NO MATCH")))))
    
    ;; Test through periphery-run-parser
    (message "\n--- Testing periphery-run-parser ---")
    (let ((result (periphery-run-parser test-output)))
      (message "Result: %s" result)
      (message "Error list: %S" periphery-errorList))
    
    ;; Check registered parsers
    (message "\n--- Registered parsers ---")
    (let ((compiler-parser (periphery-get-parser 'compiler)))
      (message "Compiler parser: %S" compiler-parser))
    
    ;; Test regex directly
    (message "\n--- Testing regex directly ---")
    (let ((regex (alist-get 'compiler periphery-builtin-patterns)))
      (message "Regex: %s" regex)
      (dolist (line (split-string test-output "\n"))
        (when (string-match regex line)
          (message "REGEX MATCH on: %s" line)
          (message "  File: %s" (match-string 1 line))
          (message "  Line: %s" (match-string 2 line))
          (message "  Col: %s" (match-string 3 line))
          (message "  Type: %s" (match-string 4 line))
          (message "  Msg: %s" (match-string 5 line)))))))

(defun periphery-test-simple ()
  "Test with a very simple error."
  (interactive)
  (let ((periphery-debug t))
    (message "Testing simple error...")
    (periphery-run-parser "/path/to/file.swift:10:5: error: test error message")))

(defun periphery-test-check-parsers ()
  "Check what parsers are registered."
  (interactive)
  (message "Checking registered parsers...")
  (maphash (lambda (id config)
             (message "Parser %s: %S" id config))
           periphery-registered-parsers))

(provide 'periphery-test)
;;; periphery-test.el ends here