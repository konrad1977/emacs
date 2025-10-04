;;; Test face definitions after reorganization

;; Load just the config to test faces
(add-to-list 'load-path "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/")
(require 'periphery-config)

;; Test that face definitions are available
(let ((test-faces '(periphery-error-face-full
                    periphery-warning-face-full
                    periphery-identifier-face
                    periphery-message-face
                    periphery-todo-face-full
                    periphery-fix-face-full)))
  
  (message "\n=== FACE TEST ===")
  (dolist (face test-faces)
    (if (facep face)
        (message "✓ Face %s is defined" face)
      (message "✗ Face %s NOT defined" face))))

;; Test syntax faces configuration
(if (boundp 'periphery-syntax-faces)
    (progn
      (message "✓ periphery-syntax-faces is defined")
      (message "Syntax faces: %S" periphery-syntax-faces))
  (message "✗ periphery-syntax-faces NOT defined"))

(message "Face test completed.")