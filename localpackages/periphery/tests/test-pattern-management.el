;;; Test complete highlight pattern management system

(add-to-list 'load-path "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/")
(require 'periphery-config)

(message "\n=== PATTERN MANAGEMENT TEST ===")

;; Test initial patterns
(message "Initial patterns: %d" (length periphery-highlight-patterns))
(periphery-list-highlight-patterns)

;; Test adding a pattern
(message "\nAdding 'numbers' pattern...")
(periphery-add-highlight-pattern 'numbers "\\([0-9]+\\)" 'periphery-warning-face)

;; Test adding another pattern
(message "\nAdding 'emails' pattern...")
(periphery-add-highlight-pattern 'emails "\\([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}\\)" 'periphery-info-face)

(message "\nAfter adding patterns:")
(periphery-list-highlight-patterns)

;; Test removing a pattern
(message "\nRemoving 'numbers' pattern...")
(periphery-remove-highlight-pattern 'numbers)

(message "\nAfter removing numbers pattern:")
(periphery-list-highlight-patterns)

;; Test that patterns and faces are properly synced
(let ((pattern-elements (mapcar #'car periphery-highlight-patterns))
      (face-elements (mapcar #'car periphery-syntax-faces)))
  (message "\nPattern elements: %S" pattern-elements)
  (message "Face elements: %S" face-elements)
  
  ;; Check if they contain the same elements (order may differ)
  (if (and (cl-subsetp pattern-elements face-elements)
           (cl-subsetp face-elements pattern-elements))
      (message "✓ Patterns and faces are properly synchronized")
    (message "✗ Patterns and faces are NOT synchronized")))

(message "\nPattern management test completed.")