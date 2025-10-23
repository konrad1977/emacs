;;; mk-kotlin-development.el --- Kotlin development environment setup -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for Kotlin development in Emacs, including syntax checking and development tools.
;;; Code:

(use-package flycheck-kotlin
  :hook ((kotlin-mode kotlin-ts-mode) . flycheck-kotlin-setup))

(use-package kotlin-mode
  :ensure t
  :defer 3
  :mode ("\\.kt\\'" "\\.kts\\'"))

(use-package kotlin-ts-mode
  :ensure t
  :after kotlin-mode
  :mode ("\\.kt\\'" "\\.kts\\'"))

(use-package kotlin-development
  :after kotlin-ts-mode
  :hook ((kotlin-mode kotlin-ts-mode) . kotlin-development-mode-setup)
  :ensure nil  ; if it's a local package
  :bind ((:map kotlin-mode-map
               ("C-c C-c" . kotlin-development-build-and-run)
               ("M-K" . kotlin-development-clean-build)
               ("C-c C-e l" . kotlin-development-list-emulators)
               ("C-c C-e k" . kotlin-development-kill-emulator))
         (:map kotlin-ts-mode-map
               ("C-c r s" . code-refactor:split-function-list)
               ("C-c C-c" . kotlin-development-build-and-run)
               ("C-c C-e l" . kotlin-development-list-emulators)
               ("C-c C-e k" . kotlin-development-kill-emulator)))
  :init
  (require 'swift-refactor)
  :config
  (setenv "JAVA_OPTS" "-Xmx8g")
  (setq kotlin-development-emulator-name "Medium_Phone_API_35"))

;;; Provide
(provide 'mk-kotlin-development)
;;; mk-kotlin-development.el ends here
