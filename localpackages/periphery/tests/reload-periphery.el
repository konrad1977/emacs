;;; Reload all periphery components

;; Clear the parser registry
(setq periphery-registered-parsers (make-hash-table :test 'eq))

;; Reload all files in order
(load-file "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/periphery-helper.el")
(load-file "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/periphery-config.el")
(load-file "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/periphery-core.el")
(load-file "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/periphery-parsers.el")
(load-file "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/periphery.el")

;; Enable debug
(setq periphery-debug t)

;; Re-initialize parsers
(periphery-parsers-initialize)

;; Test that the parser is working
(let ((test-line "/Users/mikaelkonradsson/Documents/git/bruce/bruce-ios/Bruce/Screens/Classes/Views/ClassesDateHeader.swift:17:44: error: type 'Style.Color' has no member 'primaryElemen'"))
  (message "\n=== RELOAD TEST ===")
  (message "Testing parser after reload...")
  
  ;; Check if parser is registered
  (let ((parser (periphery-get-parser 'compiler)))
    (if parser
        (message "✓ Compiler parser is registered")
      (message "✗ Compiler parser NOT registered")))
  
  ;; Test parsing
  (let ((result (periphery-parser-compiler test-line)))
    (if result
        (progn
          (message "✓ Parser returns result")
          (message "Result: %S" result))
      (message "✗ Parser returns nil")))
  
  ;; Test highlighting
  (let ((highlighted (periphery-parser--apply-highlighting "type 'Style.Color' has no member 'primaryElemen'")))
    (message "Highlighted: %s" highlighted)
    (message "Properties at quote: %S" (text-properties-at 5 highlighted))))

(message "Periphery reloaded. Try building now.")