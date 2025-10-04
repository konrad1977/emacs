;;; mk-ui.el --- UI enhancements -*- lexical-binding: t; -*-
;;; Commentary:
;; UI enhancements and visual improvements for Emacs.
;;; Code:

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

(use-package darkroom
  :defer t
  :bind ("C-x C-d" . darkroom-tentative-mode)
  :config
  (setq darkroom-text-scale-increase 1.4
        darkroom-margins '(5 . 0)))

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


(use-package imenu-list
  :after imenu-list
  :defer t
  :bind
  ("C-c i" . 'imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

(use-package candyshop
  :ensure nil
  :custom
  (candyshop-alpha-values '(100 92))
  :hook (emacs-startup . candyshop-init)
  :bind ("C-c t c" . candyshop-toggle))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon t)
  (setq nerd-icons-ibuffer-human-readable-size t))

(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-listing-switches "-aBhl --group-directories-first")
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(use-package mode-line-hud
  :ensure nil
  :config
  (setq show-in-echo-area nil))


;;; Provide
(provide 'mk-ui)
;;; mk-ui.el ends here.
