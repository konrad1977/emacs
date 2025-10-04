;;; mk-misc.el --- Miscellaneous configurations -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains miscellaneous configurations and package setups for Emacs.
;;; Code:

(use-package weather-scout
  :defer t
  :commands (weather-scout-show-forecast)
  :init
  ;; Use Vim motions in the weather forecast buffer (optional)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'weather-scout-mode 'motion)))

;; (use-package markdown-mode
;;   :defer t
;;   :ensure t
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :config
;;   (setq markdown-fontify-code-blocks-natively t))

;;
;; (use-package apple-docs-query
;;   :ensure nil
;;   :defer t
;;   :bind
;;   ("C-c C-a" . #'apple-docs/query)
;;   ("C-c C-A" . #'apple-docs/query-thing-at-point))
;;
;; (use-package hacking-with-swift
;;   :ensure nil
;;   :defer t
;;   :bind
;;   ("C-c C-h" . #'hacking-ws/query)
;;   ("C-c C-H" . #'hacking-ws/query-thing-at-point))
;;
;; (use-package periphery-quick
;;   :defer 5
;;   :ensure nil
;;   :bind
;;   :bind (:map prog-mode-map
;;               ("C-c S" . #'periphery-quick:find-ask)
;;               ;; ("C-c f f" . #'periphery-quick:find-in-file)
;;               ("C-c f t" . #'periphery-quick:todos)))

;; (use-package periphery-search
;;   :defer 5
;;   :ensure nil
;;   :bind (:map prog-mode-map
;;               ("C-c C-s" . #'periphery-search-rg)
;;               ("C-c C-f" . #'periphery-search-dwiw-rg)
;;               ("C-x C-t" . #'periphery-query-todos-and-fixmes)
;;               ("C-x C-m" . #'periphery-query-marks)
;;               ("M-?" . #'periphery-toggle-buffer)))
;;

(use-package periphery-swiftformat
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
        ("C-c C-o" . #'periphery-swiftformat-lint-buffer)
        ("M-o" . #'periphery-swiftformat-autocorrect-buffer)
        ("C-c C-p" . #'periphery-run-swiftformat-for-project)))
;;
;; (use-package periphery-ktlint
;;   :ensure nil
;;   :after kotlin-ts-mode
;;   :bind
;;   (:map kotlin-ts-mode-map
;;         ("M-o" . #'periphery-ktlint-autocorrect-buffer)))
;;
;; ;; (use-package periphery-loco
;; ;;   :ensure nil
;; ;;   :after swift-ts-mode
;; ;;   :bind
;; ;;   ("C-c C-k" . #'periphery-run-loco))
;;
;; (use-package periphery-swiftlint
;;   :ensure nil
;;   :after swift-ts-mode
;;   :bind
;;   ("C-c C-l" . #'periphery-run-swiftlint))
;;
;; ;; (use-package svg-tag-mode
;; ;;   :defer 3
;; ;;   :hook ((swift-ts-mode . svg-tag-mode)
;; ;;          (localizeable-mode . svg-tag-mode)
;; ;;          (kotlin-ts-mode . svg-tag-mode))
;; ;;   :config
;; ;;   (plist-put svg-lib-style-default :font-family "Jetbrains Mono")
;; ;;   (plist-put svg-lib-style-default :font-size 15)
;; ;;   :init
;; ;;   (setq svg-tag-tags (periphery-svg-tags)))
;;

;; (use-package typescript-ts-mode
;;   :hook (typescript-ts-base-mode . (lambda ()
;;                                      (setq-local typescript-ts-indent-level 4
;;                                                  typescript-ts-mode-indent-offset 4
;;                                                  js-indent-level 4)))
;;   :mode (("\\.tsx\\'" . tsx-ts-mode)
;;          ("\\.js\\'"  . typescript-ts-mode)
;;          ("\\.mjs\\'" . typescript-ts-mode)
;;          ("\\.mts\\'" . typescript-ts-mode)
;;          ("\\.cjs\\'" . typescript-ts-mode)
;;          ("\\.ts\\'"  . typescript-ts-mode)
;;          ("\\.jsx\\'" . tsx-ts-mode)))

(use-package nxml-mode
  :ensure nil
  :mode "\\.xml\\'"
  :config
  :hook ((nxml-mode . setup-programming-mode)
         (nxml-mode . colorful-mode)
         (nxml-mode . display-line-numbers-mode)))

;;
;; (use-package jinx
;;   :hook ((text-mode-hook . jinx-mode)
;;          (org-mode-hook . jinx-mode)
;;          (git-commit-mode-hook . jinx-mode))
;;   :bind (("C-x j c" . jinx-correct)      ; Traditional Emacs spell-check binding
;;          ("C-x j l" . jinx-languages))   ; Quick language switching
;;   :config
;;   (setq jinx-languages "en")
;;   (setq jinx-exclude-modes
;;         '(minibuffer-mode          ; Mini buffer
;;           dired-mode               ; Directory editor
;;           fundamental-mode))       ; Fundamental mode
;;   (custom-set-faces
;;    '(jinx-misspelled ((t (:underline (:style wave :color "red")))))))
;;


;; (use-package music-control
;;   :ensure nil
;;   :hook (after-init . music-control-mode))


;; (use-package jira
;;   :defer t
;;   :commands (jira-issues jira-issues-menu)
;;   :vc (jira
;;        :url "git@github.com:unmonoqueteclea/jira.el"
;;        :branch "main"
;;        :rev :newest)
;;   :init
;;   (setq jira-username "mikael.konradsson@mobileinteraction.se"
;;         jira-token (getenv "JIRA_TOKEN")
;;         jira-base-url "https://mobileinteraction.atlassian.net" ;; Jira instance URL
;;         jira-statuses-done '("Klart" "Done" "Closed" "Resolved" "Waiting for QA")
;;         jira-statuses-todo '("Att göra" "Todo")
;;         jira-statuses-error'("Error" "Rejected" "In Progress - Error" "Under granskning")
;;         jira-statuses-progress '("Pågående" "QA staging" "In test" "In Progress" "In progress" "In Progress - Development" "In Progress - Design" "In Progress - Review" "In Progress - Testing")))



(provide 'mk-misc)
;;; mk-misc.el ends here
