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

;; Load the unified minor mode that provides all Swift development keybindings
;; This mode automatically activates for swift-ts-mode, localizeable-mode, and ios-simulator buffers
(use-package swift-development-mode
  :ensure nil
  :after (swift-ts-mode swift-development ios-simulator xcode-project swift-refactor))

;;; Provide
(provide 'mk-ios-development)
;;; mk-ios-development.el ends here.
