;;; init.el --- optimized init file -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Personal Emacs configuration with optimized startup and modern packages.
;; Features include Evil mode, LSP support, project management, and development tools.

;;; Code:

;; Suppress all warnings when loading packages
(setq warning-minimum-level :error)
(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)

;; Load warning suppression
(load (expand-file-name "suppress-warnings" user-emacs-directory) nil t)

(dolist (path (list
               (expand-file-name "modules" user-emacs-directory)
               (expand-file-name "themes" user-emacs-directory)))
  (add-to-list 'load-path path))

;; Load modules with simple require - keeping it simple and reliable
(require 'mk-core)
(require 'mk-emacs)
(require 'mk-editing)
(require 'mk-lispediting)
(require 'mk-ios-development)
(require 'mk-org)
(require 'mk-development)
(require 'mk-evil)
(require 'mk-theme)
(require 'mk-code-completion)
(require 'mk-completion)
(require 'mk-vc)
(require 'mk-ai)

(use-package polymode
  :defer t
  :ensure t)

;; (use-package markdown-mode
;;   :defer t
;;   :ensure t
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :config
;;   (setq markdown-fontify-code-blocks-natively t))

(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-listing-switches "-aBhl --group-directories-first")
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(use-package weather-scout
  :defer t
  :commands (weather-scout-show-forecast)
  :init
  ;; Use Vim motions in the weather forecast buffer (optional)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'weather-scout-mode 'motion)))

(use-package candyshop
  :ensure nil
  :custom
  (candyshop-alpha-values '(100 92))
  :hook (emacs-startup . candyshop-init)
  :bind ("C-c t c" . candyshop-toggle))

(use-package welcome-dashboard
  :ensure nil
  :config
  (setq welcome-dashboard-latitude 56.7365
        welcome-dashboard-longitude 16.2981
        welcome-dashboard-use-nerd-icons t
        welcome-dashboard-max-number-of-projects 8
        welcome-dashboard-show-weather-info t
        welcome-dashboard-use-fahrenheit nil
        welcome-dashboard-max-left-padding 1
        welcome-dashboard-max-number-of-todos 5
        welcome-dashboard-path-max-length 70
        welcome-dashboard-min-left-padding 10
        welcome-dashboard-image-file "~/.emacs.d/themes/emacs.png"
        welcome-dashboard-image-width 200
        welcome-dashboard-image-height 200
        welcome-dashboard-title "Welcome Mikael. Have a great day!")
  (welcome-dashboard-create-welcome-hook))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon t)
  (setq nerd-icons-ibuffer-human-readable-size t))


(use-package mode-line-hud
  :ensure nil
  :config
  (setq show-in-echo-area nil))
;;
;; (use-package focus
;;   :defer t
;;   :custom
;;   (focus-echo-keystrokes 0.1)
;;   (focus-echo-keystrokes-delay 0.1)
;;   (focus-echo-keystrokes-timeout 0.1)
;;   (focus-echo-keystrokes-timeout-delay 0.1))
;;
;; (use-package ediff
;;   :defer t
;;   :after prog-mode
;;   :ensure nil
;;   :init
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (setq ediff-split-window-function 'split-window-horizontally)
;;   (setq ediff-merge-split-window-function 'split-window-horizontally)
;;   :custom
;;   (setq ediff-keep-variants nil)
;;   :config
;;   (winner-mode)
;;   (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
;;


;; (use-package corfu-terminal
;;   :defer t
;;   :if (< emacs-major-version 31)
;;   :unless (featurep 'tty-child-frames)
;;   :unless (display-graphic-p)
;;   :hook
;;   (corfu-mode-hook . corfu-terminal-mode))

;; ;; ;; Add extensions

(use-package darken-buffer
  :ensure nil
  :hook (after-init . darken-buffer-mode)
  :config
  (setq darken-buffer-percentage 0
        darken-buffer-ignore-buffers '("*Messages*" "*scratch*" "*Android Emulator*")
        darken-buffer-always-darken-buffers-regexp '("\\*.*\\*")
        darken-buffer-always-color-buffers '(
                                             ("*iOS Simulator*" . (:background "#252535" :foreground "#7e9cd8"))
                                             ("*dape-repl*" . (:background "#43242B" :foreground "#D27E99"))
                                             ("*dape-info*" . (:background "##43242B" :foreground "#D27E99"))
                                             ("*Help*" . (:background "#252535" :foreground "#7e9cd8"))
                                             ("*Embark Collect*" . (:background "#252535" :foreground "#7e9cd8"))
                                             ("*Embark Export*" . (:background "#252535" :foreground "#7e9cd8"))
                                             ("*Embark Live*" . (:background "#252535" :foreground "#7e9cd8"))
                                             ("*Corfu documentation*" . (:background "#252535" :foreground "#7e9cd8"))
                                             ("*Corfu location*" . (:background "#252535" :foreground "#7e9cd8")))
        darken-buffer-always-darken-percentage 10
        lighten-inactive-buffer-percentage 5))

(use-package project-treemacs
  :after treemacs)

(add-hook 'comint-mode-hook
          (lambda ()
            (setq-local comint-prompt-read-only t)
            (setq-local visual-line-mode t)))


(use-package imenu-list
  :after imenu-list
  :defer t
  :bind
  ("C-c i" . 'imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

(use-package darkroom
  :defer t
  :bind ("C-x C-d" . darkroom-tentative-mode)
  :config
  (setq darkroom-text-scale-increase 1.4
        darkroom-margins '(5 . 0)))

(use-package vterm
  :defer t
  :commands vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (setq vterm-timer-delay nil
        vterm-kill-buffer-on-exit t
	vterm-clipboard-warning-max-lines 20
	vterm-clipboard-warning-max-chars 256))
;;
;; (use-package paren
;;   :defer 2
;;   :config
;;   (show-paren-mode 1)
;;   (setq show-paren-delay 0.1
;;         show-paren-highlight-openparen t
;;         show-paren-when-point-inside-paren t
;;         show-paren-when-point-in-periphery t))
;;


(use-package restclient
  :defer t)

;;
;; (use-package elfeed
;;   :commands elfeed
;;   :config
;;
;;   (defun add-icon-to-title (icon tag all-tags title)
;;     "Add ICON if TAG is present in ALL-TAGS to the TITLE string."
;;     (if (member tag all-tags)
;;         (concat icon " ")
;;       ""))
                                        ;
;;   (defun my/elfeed-search-print-entry (entry)
;;     "Print ENTRY to the buffer."
;;     (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
;;            (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
;;            (title-faces (if (member 'unread (elfeed-entry-tags entry))
;;                             (elfeed-search--faces (elfeed-entry-tags entry))
;;                           '(:weight thin :inherit font-lock-comment-face)))
;;            (feed (elfeed-entry-feed entry))
;;            (feed-title (when feed
;;                          (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
;;            (tags (seq-filter
;;                   (lambda (tag) (not (string-equal feed-title tag)))
;;                   (mapcar #'symbol-name (elfeed-entry-tags entry))))
;;            (icons '(("swift" . "")
;;                     ("swiftui" . "")
;;                     ("emacs" . "")
;;                     ("neovim" . "")
;;                     ("artificialInteligence" . "")
;;                     ("singularity" . "")
;;                     ("kotlin" . "")
;;                     ("techcrunch" . "")))
;;            (title-with-icons (concat
;;                               (mapconcat
;;                                (lambda (icon-pair)
;;                                  (add-icon-to-title (cdr icon-pair) (car icon-pair) tags " "))
;;                                icons "")
;;                               " "
;;                               title))
;;            (tags-str (mapconcat
;;                       (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
;;                       tags ","))
;;            (title-width (- (window-width) 10 elfeed-search-trailing-width))
;;            (title-column (elfeed-format-column
;;                           title-with-icons (elfeed-clamp
;;                                             elfeed-search-title-min-width
;;                                             title-width
;;                                             elfeed-search-title-max-width)
;;                           :left)))
;;       (insert (propertize date 'face 'elfeed-search-date-face) " ")
;;       (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
;;       ;; (when feed-title
;;       ;;   (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
;;       (when tags
;;         (insert tags-str))))
;;
;;   (setq elfeed-search-print-entry-function #'my/elfeed-search-print-entry)
;;   (setq elfeed-feeds '(
;;                        ("https://www.reddit.com/r/emacs.rss" emacs)
;;                        ("https://www.reddit.com/r/neovim.rss" neovim)
;;                        ("https://www.reddit.com/r/kotlin.rss" kotlin)
;;                        ("https://www.reddit.com/r/swift.rss" swift)
;;                        ("https://www.reddit.com/r/swiftui.rss" swiftui)
;;                        ("https://www.reddit.com/r/artificialInteligence.rss" artificialInteligence)
;;                        ;; ("https://techcrunch.com/rss" techcrunch)
;;                        ))
;;
;;   (setq elfeed-search-face-alist
;;         '((emacs font-lock-function-name-face)
;;           (neovim font-lock-type-face)
;;           (kotlin font-lock-keyword-face)
;;           (swift font-lock-constant-face)
;;           (techcrunch font-lock-variable-name-face)
;;           (ai font-lock-number-face)
;;           (singularity font-lock-number-face)
;;           (read font-lock-comment-face)))
;;   (setq elfeed-search-filter "@4-days-ago +unread"
;;         elfeed-search-title-max-width 140
;;         elfeed-search-title-min-width 140))
;;
;;
;;

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
(use-package periphery-quick
  :defer 5
  :ensure nil
  :bind
  :bind (:map prog-mode-map
              ("C-c S" . #'periphery-quick:find-ask)
              ;; ("C-c f f" . #'periphery-quick:find-in-file)
              ("C-c f t" . #'periphery-quick:todos)))

(use-package periphery-search
  :defer 5
  :ensure nil
  :bind (:map prog-mode-map
              ("C-c C-s" . #'periphery-search-rg)
              ("C-c C-f" . #'periphery-search-dwiw-rg)
              ("C-x C-t" . #'periphery-query-todos-and-fixmes)
              ("C-x C-m" . #'periphery-query-marks)
              ("M-?" . #'periphery-toggle-buffer)))
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

(use-package indent-bars
  :defer t
  :vc (indent-bars
       :url "https://github.com/jdtsmith/indent-bars"
       :branch "main"
       :rev :newest)
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.15))
  (indent-bars-highlight-current-depth '(:blend 0.5)) ; pump up the BG blend on current
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("comment")) ; Ignore comments
  (indent-bars-width-frac 0.1)
  (indent-bars-prefer-character t))

(defun mk/browser-split-window (url &optional new-window)
  "Create a new browser (as URL as NEW-WINDOW) window to the right of the current one."
  (interactive)
  (let ((ignore-window-parameters t)
        (dedicated-p (window-dedicated-p)))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (xwidget-webkit-browse-url url)))

(defun toggle-vterm ()
  "Toggle vterm buffer."
  (interactive)
  (if (get-buffer "*vterm*")
      (if (eq (current-buffer) (get-buffer "*vterm*"))
          (delete-window)
        (switch-to-buffer-other-window "*vterm*"))
    (progn
      (vterm-other-window))))


(use-package typescript-ts-mode
  :hook (typescript-ts-base-mode . (lambda ()
                                     (setq-local typescript-ts-indent-level 4
                                                 typescript-ts-mode-indent-offset 4
                                                 js-indent-level 4)))
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)))

;; (use-package treesit
;;   :defer t
;;   :mode (("\\.tsx\\'" . tsx-ts-mode)
;;          ("\\.js\\'"  . typescript-ts-mode)
;;          ("\\.mjs\\'" . typescript-ts-mode)
;;          ("\\.mts\\'" . typescript-ts-mode)
;;          ("\\.cjs\\'" . typescript-ts-mode)
;;          ("\\.ts\\'"  . typescript-ts-mode)
;;          ("\\.jsx\\'" . tsx-ts-mode)
;;          ("\\.json\\'" .  json-ts-mode)
;;          ("\\.Dockerfile\\'" . dockerfile-ts-mode)
;;          ("\\.prisma\\'" . prisma-ts-mode))
;;   :preface
;;   (defun os/setup-install-grammars ()
;;     "Install Tree-sitter grammars if they are absent."
;;     (interactive)
;;     (dolist (grammar
;;              '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
;;                (bash "https://github.com/tree-sitter/tree-sitter-bash")
;;                (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
;;                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
;;                (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
;;                (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
;;                (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
;;                (kotlin  "https://github.com/fwcd/tree-sitter-kotlin")
;;                (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;                (make "https://github.com/alemuller/tree-sitter-make")
;;                (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;                (cmake "https://github.com/uyha/tree-sitter-cmake")
;;                (c "https://github.com/tree-sitter/tree-sitter-c")
;;                (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;                (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
;;                (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
;;                (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
;;                (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
;;       (add-to-list 'treesit-language-source-alist grammar)
;;       ;; Only install `grammar' if we don't already have it
;;       ;; installed. However, if you want to *update* a grammar then
;;       ;; this obviously prevents that from happening.
;;       (unless (treesit-language-available-p (car grammar))
;;         (treesit-install-language-grammar (car grammar)))))
;;   (dolist (mapping
;;            '((python-mode . python-ts-mode)
;;              (css-mode . css-ts-mode)
;;              (typescript-mode . typescript-ts-mode)
;;              (js-mode . typescript-ts-mode)
;;              (js2-mode . typescript-ts-mode)
;;              (c-mode . c-ts-mode)
;;              (c++-mode . c++-ts-mode)
;;              (c-or-c++-mode . c-or-c++-ts-mode)
;;              (bash-mode . bash-ts-mode)
;;              (css-mode . css-ts-mode)
;;              (json-mode . json-ts-mode)
;;              (js-json-mode . json-ts-mode)
;;              (sh-mode . bash-ts-mode)
;;              (sh-base-mode . bash-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping))
;;   :config
;;   (os/setup-install-grammars))

(use-package nxml-mode
  :ensure nil
  :mode "\\.xml\\'"
  :config
  :hook ((nxml-mode . setup-programming-mode)
         (nxml-mode . colorful-mode)
         (nxml-mode . display-line-numbers-mode)))

(use-package flycheck-kotlin
  :hook ((kotlin-mode kotlin-ts-mode) . flycheck-kotlin-setup))

(use-package kotlin-ts-mode
  :ensure t
  :defer 3
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


(use-package jira
  :defer t
  :commands (jira-issues jira-issues-menu)
  :vc (jira
       :url "git@github.com:unmonoqueteclea/jira.el"
       :branch "main"
       :rev :newest)
  :init
  (setq jira-username "mikael.konradsson@mobileinteraction.se"
        jira-token (getenv "JIRA_TOKEN")
        jira-base-url "https://mobileinteraction.atlassian.net" ;; Jira instance URL
        jira-statuses-done '("Klart" "Done" "Closed" "Resolved" "Waiting for QA")
        jira-statuses-todo '("Att göra" "Todo")
        jira-statuses-error'("Error" "Rejected" "In Progress - Error" "Under granskning")
        jira-statuses-progress '("Pågående" "QA staging" "In test" "In Progress" "In progress" "In Progress - Development" "In Progress - Design" "In Progress - Review" "In Progress - Testing")))
;;
;;
;;
;; (defun consult-list-all-project-files ()
;;   "Show all project files immediately with live preview."
;;   (interactive)
;;   (consult--read (project-files (project-current t))
;;                  :prompt "Project file: "
;;                  :category 'file
;;                  :state (consult--file-state)
;;                  :require-match t))

;; (use-package markdown-ts-mode
;;   :mode ("\\.md\\'" . markdown-ts-mode)
;;   :defer t)

(use-package punch-line
  :ensure nil
  :defer t
  :bind(("C-x t n" . punch-line-what-am-i-doing-next)
        ("C-x t N" . punch-line-what-am-i-doing-next-task)
        ("C-x t d" . punch-line-what-am-i-doing-done)
        ("C-x t a" . punch-line-what-am-i-doing-show-all)
        (:repeat-map punch-line-repeat-map
                     ("n" . punch-line-what-am-i-doing-next)
                     ("N" . punch-line-what-am-i-doing-next-task)
                     ("d" . punch-line-what-am-i-doing-done)
                     ("a" . punch-line-what-am-i-doing-show-all)))
  :hook ((after-init . punch-line-mode)
         (after-init . punch-weather-update)
         (after-init . punch-load-tasks))
  :config
  (setq punch-show-project-info nil
        punch-line-modal-use-fancy-icon nil
        punch-line-modal-divider-style 'none
        punch-line-modal-size 'small
        punch-line-left-separator ""
        punch-line-right-separator ""
        punch-line-section-padding 2
        punch-show-git-info t
        punch-show-lsp-info t
        punch-show-copilot-info nil
        punch-show-battery-info t
        punch-show-flycheck-info t
        punch-show-weather-info t
        punch-weather-latitude "56.7365"
        punch-weather-longitude "16.2981"
        punch-line-music-max-length 80
        punch-line-section-backgrounds 'auto
        punch-line-section-background-tint-start -15
        punch-line-section-background-tint-step 50
        ;; punch-line-music-info '(:service apple)
        ))


(provide 'init)
;;; init.el ends here
