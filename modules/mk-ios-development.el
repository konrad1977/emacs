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
              ("C-c C-c" . #'swift-additions:compile-and-run)
              ("C-c C-k" . #'periphery-run-loco)))

(use-package objc-mode
  :mode "\\.h\\'"
  :defer t
  :ensure nil)

(use-package ios-simulator
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
        ("M-s" . #'ios-simulator:terminate-current-app)
        ("C-x s n" . #'ios-simulator:send-notification)
        ("C-x s t" . #'ios-simulator:toggle-buffer)
        ("C-x s l" . #'ios-simulator:change-language)))

(use-package swift-additions
  :ensure nil
  :after swift-ts-mode
  :custom
  (swift-additions:use-periphery t)
  :bind
  (:map swift-ts-mode-map
	("M-r" . #'swift-additions:run)
	("C-c t m" .  #'swift-additions:test-module-silent)
	("C-c t p" .  #'swift-additions:test-swift-package-from-file)
	("C-c C-c" . #'swift-additions:compile-and-run)
	("C-c C-b" . #'swift-additions:compile-app)
	("C-c C-x" . #'swift-additions:reset)
        ("C-x p t" . #'periphery-toggle-buffer)
	("C-c C-f" . #'periphery-search-dwiw-rg)))

(use-package swift-features
  :ensure nil
  :after swift-ts-mode)

(use-package domain-blocker
  :ensure nil
  :after swift-ts-mode)

(use-package xcode-additions
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
        ("M-K" .  #'xcode-additions:clean-build-folder)
        ("C-c C-d" . #'xcode-additions:start-debugging)
        ("C-c x t" . #'xcode-additions:toggle-device-choice)
        ("C-c x c" . #'xcode-additions:show-current-configuration)))
;;
(use-package swift-refactor
  :ensure nil
  :after (swift-ts-mode kotlin-ts-mode)
  :bind
  (:map kotlin-ts-mode-map
        ("C-c r s" . #'code-refactor:split-function-list)
        ("M-t" . #'swift-refactor:insert-todo)
        ("M-m" . #'swift-refactor:insert-mark))
  (:map swift-ts-mode-map
        ("M-t" . #'swift-refactor:insert-todo)
        ("M-m" . #'swift-refactor:insert-mark)
        ("C-c r a" . #'swift-refactor:wrap-selection)
        ("C-c r d" . #'swift-refactor:delete-current-line-with-matching-brace)
        ("C-c r i" . #'swift-refactor:tidy-up-constructor)
        ("C-c r r" . #'swift-refactor:extract-function)
        ("M-P" .  #'swift-refactor:print-thing-at-point)
        ("C-c r t" . #'swift-refactor:add-try-catch)))

(use-package apple-docs-query
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
        ("C-x D" . #'apple-docs/query-thing-at-point)
        ("C-x d" . #'apple-docs/query)))

;;; Provide
(provide 'mk-ios-development)
;;; mk-ios-development.el ends here.
