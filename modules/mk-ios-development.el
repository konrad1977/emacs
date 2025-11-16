;; -*- lexical-binding: t; -*-
;;; Code:

(use-package swift-ts-mode
  :mode "\\.swift\\'"
  :ensure nil
  :bind
  (:map swift-ts-mode-map
        ("C-c t s" . #'swift-ts:split-func-list))
  :custom
  (swift-ts-basic-offset 4)
  (swift-ts:indent-trailing-call-member t)
  :config
  (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-ts-mode)))

(use-package localizeable-mode
  :ensure nil
  :after swift-ts-mode
  :mode "\\.strings\\'"
  :bind (:map localizeable-mode-map
              ("C-c C-k" . #'periphery-run-loco)))

(use-package objc-mode
  :mode "\\.h\\'"
  :defer t
  :ensure nil)

(use-package ios-simulator
  :ensure nil
  :after swift-ts-mode)

(use-package swift-cache
  :ensure nil
  :after swift-ts-mode)

(use-package swift-development
  :ensure nil
  :after swift-ts-mode
  :custom
  (swift-development-use-periphery t))

(use-package swift-features
  :ensure nil
  :after swift-ts-mode)

(use-package domain-blocker
  :ensure nil
  :after swift-ts-mode)

(use-package xcode-project
  :ensure nil
  :after swift-ts-mode)

(use-package xcode-build-config
  :ensure nil
  :after swift-ts-mode)

(use-package swift-refactor
  :ensure nil
  :after (swift-ts-mode kotlin-ts-mode)
  :bind
  (:map kotlin-ts-mode-map
        ("C-c r s" . #'code-refactor-split-function-list)
        ("M-t" . #'swift-refactor-insert-todo)
        ("M-m" . #'swift-refactor-insert-mark)))

(use-package apple-docs-query
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
        ("C-x D" . #'apple-docs/query-thing-at-point)
        ("C-x d" . #'apple-docs/query)))


(use-package periphery-swiftformat
  :ensure nil
  :after (swift-ts-mode swift-mode)
  :bind
  (:map swift-ts-mode-map
        ("C-c C-o" . #'periphery-swiftformat-lint-buffer)
        ("M-o" . #'periphery-swiftformat-autocorrect-buffer)
        ("C-c C-p" . #'periphery-run-swiftformat-for-project)))

;; Load the unified minor mode that provides all Swift development keybindings
;; This mode automatically activates for swift-ts-mode, localizeable-mode, and ios-simulator buffers
(use-package swift-development-mode
  :ensure nil
  :after (swift-ts-mode swift-development ios-simulator xcode-project swift-refactor))

;; knockknock notification integration
(use-package knockknock
  :ensure nil
  :config
  (setopt knockknock-border-color "black")
  (defun my-xcode-knockknock-notify (&rest args)
    "Custom notification function using knockknock for xcode-project.
Accepts keyword arguments from xcode-project-notify:
  :message - The message to display
  :delay   - Optional delay (ignored for knockknock)
  :seconds - How long to show notification
  :reset   - Whether to reset (ignored for knockknock)
  :face    - Face for styling (ignored for knockknock)"
    (let* ((message-text (plist-get args :message))
           (seconds (or (plist-get args :seconds) 3))
           ;; Choose icon based on message content
           (icon (cond
                  ((string-match-p "\\(success\\|complete\\|passed\\)" message-text)
                   "nf-cod-check")
                  ((string-match-p "\\(error\\|fail\\)" message-text)
                   "nf-cod-error")
                  ((string-match-p "\\(warning\\|warn\\)" message-text)
                   "nf-cod-warning")
                  ((string-match-p "\\(build\\|compil\\)" message-text)
                   "nf-cod-tools")
                  (t "nf-dev-xcode")))
           ;; Try to extract title from message if it contains a colon
           (parts (split-string message-text ": " t))
           (title (if (> (length parts) 1) (car parts) "Swift-development"))
           (msg (if (> (length parts) 1)
                    (string-join (cdr parts) ": ")
                  message-text)))

      (knockknock-notify
       :title title
       :message msg
       :icon icon
       :duration seconds)))

  ;; Configure xcode-project to use custom backend
  (setq xcode-project-notification-backend 'custom)
  (setq xcode-project-notification-function #'my-xcode-knockknock-notify))

;;; Provide
(provide 'mk-ios-development)
;;; mk-ios-development.el ends here.
