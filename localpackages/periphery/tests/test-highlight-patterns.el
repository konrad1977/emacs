;;; Test highlight patterns after moving to config

;; Load config
(add-to-list 'load-path "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/")
(require 'periphery-config)

;; Test that patterns are available
(message "\n=== HIGHLIGHT PATTERNS TEST ===")

(if (boundp 'periphery-highlight-patterns)
    (progn
      (message "✓ periphery-highlight-patterns is defined")
      (message "Available patterns: %S" (mapcar #'car periphery-highlight-patterns)))
  (message "✗ periphery-highlight-patterns NOT defined"))

;; Test adding a new pattern
(periphery-add-highlight-pattern 'brackets "\\(\\[[^]]+\\]\\)" 'periphery-warning-face)

(message "After adding brackets pattern:")
(message "Patterns: %S" (mapcar #'car periphery-highlight-patterns))
(message "Faces: %S" (mapcar #'car periphery-syntax-faces))

;; Test that the pattern was added
(let ((bracket-pattern (alist-get 'brackets periphery-highlight-patterns))
      (bracket-face (alist-get 'brackets periphery-syntax-faces)))
  (if (and bracket-pattern bracket-face)
      (message "✓ Successfully added brackets pattern: %s -> %s" bracket-pattern bracket-face)
    (message "✗ Failed to add brackets pattern")))

(message "Highlight patterns test completed.")