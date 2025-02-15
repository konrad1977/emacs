;;; init.el --- optimized init file -*- no-byte-compile: t; lexical-binding: t; -*-
;;; commentary:

;;; code:


;; Compile-time variable declarations to avoid warnings
(eval-when-compile
  (defvar display-time-24hr-format t)
  (defvar display-time-default-load-average nil))

;; Package management setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Ensure use-package is installed for Emacs versions < 29
(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

;; Upgrade built-in packages
(setopt package-install-upgrade-built-in t)

;; Reset version control backends to default
(setq vc-handled-backends (eval (car (get 'vc-handled-backends 'standard-value))))

;; Add themes directory to custom theme load path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(add-to-list 'custom-theme-load-path (expand-file-name "localpackages/kanagawa-emacs" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "localpackages/mito-laser-emacs" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "localpackages/neofusion-emacs" user-emacs-directory))

;; Add local packages directory to load-path
(let ((dir (expand-file-name "localpackages" user-emacs-directory)))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (let ((default-directory dir))
      (normal-top-level-add-subdirs-to-load-path))))

(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key [C-wheel-up])
(global-unset-key [C-wheel-down])

(use-package use-package
  :config
  (setq use-package-verbose nil
	use-package-expand-minimally t
	use-package-always-ensure t
	use-package-compute-statistics t
	use-package-minimum-reported-time 0.1))

(use-package rg
  :defer t
  :ensure t)

(use-package spinner
  :defer t
  :ensure t)

(use-package async
  :defer t
  :ensure t)

(use-package request
  :defer t
  :ensure t)

(use-package nerd-icons
  :defer t
  :ensure t
  :custom
  (nerd-icons-scale-factor 1.05))

(use-package all-the-icons
  :defer t
  :ensure t)

(use-package drag-stuff
  :defer t
  :ensure t)

(use-package dumb-jump
  :defer t
  :config
  (put 'dumb-jump-go 'byte-obsolete-info nil)
  (setq dumb-jump-window 'current
        dumb-jump-quiet t
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package candyshop
  :ensure nil
  :hook (after-init . candyshop-mode)
  :bind ("C-c t c" . candyshop-toggle)
  :config
  (setq candyshop-alpha-values '(100 95)))

(use-package emacs
  :init
  (global-hl-line-mode 1)
  (global-auto-revert-mode 1)
  (global-so-long-mode 1)
  ;; (pixel-scroll-precision-mode 1)
  (set-display-table-slot standard-display-table 0 ?\ )
  (display-battery-mode 1)
  :config
  (setq-default
   confirm-kill-emacs (lambda (prompt)
			(y-or-n-p-with-timeout prompt 2 nil))
   use-short-answers t
   confirm-kill-processes nil
   fringes-outside-margins nil
   indicate-buffer-boundaries nil
   indicate-empty-lines nil
   create-lockfiles nil
   auto-revert-verbose nil
   auto-revert-interval 1
   auto-save-no-message t
   delete-by-moving-to-trash t
   make-backup-files nil
   auto-save-default nil
   auto-save-interval 2000
   auto-save-timeout 20
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   ring-bell-function 'ignore
   line-spacing 0.08
   global-auto-revert-non-file-buffers t
   completion-ignore-case t
   display-line-numbers-width 4
   cursor-in-non-selected-windows nil
   find-file-visit-truename nil
   ad-redefinition-action 'accept
   large-file-warning-threshold (* 25 1024 1024)
   backup-by-copying t
   backup-directory-alist `(("." . "~/.saves"))
   auto-save-list-file-prefix (expand-file-name "var/auto-save/.saves-" user-emacs-directory)
   auto-save-file-name-transforms `((".*" ,(expand-file-name "var/auto-save/" user-emacs-directory) t))
   debug-on-error nil
   custom-file (concat user-emacs-directory "var/custom.el")
   scroll-margin 0
   scroll-conservatively 101
   scroll-preserve-screen-position t
   scroll-step 1
   auto-window-vscroll nil
   backward-delete-char-untabify-method 'hungry
   fast-but-imprecise-scrolling nil
   redisplay-skip-fontification-on-input nil
   line-move-visual nil)
  (when (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'none
          dired-use-ls-dired nil
          browse-url-browser-function #'mk/browser-split-window)
    (setq mac-redisplay-dont-reset-vscroll t
          mac-mouse-wheel-smooth-scroll nil))
  (when (file-exists-p custom-file)
    (load custom-file)))

(defun setup-programming-mode ()
  "Setup programming mode."
  ;; CONSIDER: Moving keybindings to a separate key-binding file

  (defun infer-indentation-style ()
  "Default to no tabs, but use tabs if already in project"
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count   (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq-default indent-tabs-mode nil))
    (if (> tab-count space-count) (setq-default indent-tabs-mode t))))

  (setq-default indent-tabs-mode nil)
  (infer-indentation-style)
  
  ;; (local-set-key (kbd "M-+") #'mk/toggle-flycheck-errors)
  (setq indicate-unused-lines nil
        left-fringe-width 10
        word-wrap nil
        show-trailing-whitespace nil
        column-number-mode nil
        truncate-lines t
        display-line-numbers 'relative))

(use-package window
  :ensure nil
  :bind (("C-x C-f" . toggle-frame-fullscreen)
         ("C-x C-s" . window-toggle-side-windows)
         ("C-x C-x" . safe-kill-buffer-and-window))
  :custom
  (setq window-resize-pixelwise nil
	frame-resize-pixelwise nil
	window-divider-default-places t
	window-divider-default-bottom-width 1
	window-divider-default-right-width 1)
  ;; (window-combination-resize t)
  ;; (even-window-sizes 'height-only)
  ;; (window-sides-vertical nil)
  ;; (fit-window-to-buffer-horizontally t)
  ;; (window-resize-pixelwise t)
  ;; (fit-frame-to-buffer t)
  (display-buffer-alist
   '(("\\*Async Shell Command\\*" (display-buffer-no-window))
     ("\\*xwidget\\*\\|\\*xref\\*"
      (display-buffer-in-side-window display-buffer-reuse-mode-window display-buffer-reuse-window)
      (body-function . select-window)
      (window-width . 0.4)
      (side . left))
     ("\\*occur\\|evil-marks\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-width . 0.10)
      (side . right)
      (slot . 0))
     ("\\*iOS Simulator\\|\\*swift package\\|\\*ios-device"
      (display-buffer-reuse-window display-buffer-in-side-window display-buffer-at-bottom)
      (window-height . 0.15)
      (window-parameters . ((mode-line-format . none)))
      (slot . 4))
     ("\\*Embark*"
      (display-buffer-reuse-window display-buffer-in-side-window display-buffer-at-bottom)
      (body-function . select-window)
      (window-height . 0.4)
      (slot . 1))
     ("\\*Copilot-chat"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-width . 0.3)
      (side . right)
      (slot . 2)
      (window-parameters . ((mode-line-format . none))))
     ("\\*Periphery\\*\\|\\*compilation\\*"
      (display-buffer-reuse-window display-buffer-in-side-window display-buffer-at-bottom)
      (body-function . select-window)
      (window-height . 0.2)
      (window-width . 0.30)
      (slot . 1))
     ("\\*Android Emulator\\*\\|\\*Android Logcat\\*\\|\\*Android Emulator Error\\*"
      (display-buffer-reuse-window display-buffer-in-side-window display-buffer-at-bottom)
      (body-function . select-window)
      (window-height . 0.2)
      (window-width . 0.70)
      (slot . 2))
     ("\\*Faces\\|[Hh]elp\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-width . 0.4)
      (side . right)
      (slot . 1))
     ("\\*e?shell\\|*ellama\\|\\*vterm\\*"
      (display-buffer-at-bottom display-buffer-reuse-window)
      (body-function . select-window)
      (window-height . 0.13)
      (window-parameters . ((mode-line-format . none)))
      (side . bottom)
      (slot . 10))
     ("\\*\\(Flycheck\\|Package-Lint\\).*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
      (display-buffer-at-bottom)
      (slot . 3)
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none)))))))

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

(when (fboundp 'set-message-beep)
  (set-message-beep 'silent))

(use-package nerd-icons-completion
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :custom (nerd-icons-corfu--space " "))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon t)
  (setq nerd-icons-ibuffer-human-readable-size t))

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :functions treemacs-load-theme
  :config
  (setq nerd-icons-color-icons nil)
  (treemacs-load-theme "nerd-icons"))

(use-package ligature
  :config
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                              "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                              "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                              "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                              "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                              "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                              "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                              "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                              "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                              "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                              "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                              ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                              "<:<" ";;;"))
  (global-ligature-mode t))

(use-package no-littering)

(use-package autothemer
  :init
   ;; (load-theme 'catppuccin-latte t)
  ;; (load-theme 'catppuccin-macchiato t)
  ;; (load-theme 'catppuccin-frappe t)
  ;; (load-theme 'catppuccin-mocha t)
  ;; (load-theme 'rose-pine t)
  ;; (load-theme 'oxographite t)
  ;; (load-theme 'kman t)
  ;; (load-theme 'kalmar-night t)
  ;; (load-theme 'kanagawa t)
  ;; (load-theme 'neofusion t)
  ;; (load-theme 'doom-gruvbox t)
  ;; (load-theme 'oxocarbon t)
  ;; (load-theme 'nordic t)
  (load-theme 'mito-laser t)
  ;; (load-theme 'doom-outrun-electric t)
  ;; (load-theme 'doom-laserwave t)
  )


;; (advice-add #'balance-windows :override #'balance-windows-area)

(use-package zoom
  :hook (after-init . zoom-mode)
  :config
  (setq zoom-size '(0.7 . 0.7))
  zoom-ignored-major-modes '(comint-mode
                             dape-mode
                             dired-mode
                             help-mode
                             help-mode-menu
                             helpful-mode
                             org-mode
                             rxt-help-mode
                             vterm-mode)
  zoom-ignore-predicates (list (lambda () (< (count-lines (point-min) (point-max)) 20)))
  (add-to-list 'zoom-ignored-buffer-name-regexps "*Ilist*"))

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)
	 (prog-mode . highlight-symbol-mode)
	 (prog-mode . electric-pair-mode)
	 (prog-mode . electric-indent-mode)
	 (prog-mode . drag-stuff-mode)
	 (prog-mode . dumb-jump-mode)
	 (prog-mode . hs-minor-mode)
	 (prog-mode . setup-programming-mode))
  :config
  (setq display-line-numbers-type 'relative))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package vertico
  :hook (after-init . vertico-mode)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)
        ("C-d" . vertico-scroll-down)
        ("C-u" . vertico-scroll-up))
  :config
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face '(:inherit font-lock-delimiter-face :weight bold))
                   "  ")
                 cand)))
  (setq enable-recursive-minibuffers nil) ;; Tillåt rekursiva minibuffers
  (minibuffer-depth-indicate-mode 1)
  (setq vertico-resize t
        vertico-count 12
        vertico-multiline nil
        vertico-scroll-margin 10
        vertico-cycle t))

(use-package vertico-posframe
  :after vertico
  :init
  (vertico-posframe-mode 1)
  :config
  (vertico-posframe-cleanup)
  (setq vertico-posframe-parameters
        '((left-fringe . 0)
          (right-fringe . 0)
          (alpha . 94))
        vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
        vertico-posframe-truncate-lines t
        vertico-posframe-min-height 1
        vertico-posframe-min-width 80
        vertico-posframe-width 160
        vertico-posframe-border-width 20))

;; Configure directory extension.
(use-package vertico-directory
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :commands (find-file)
  :ensure nil
  :bind (:map vertico-map
              ("<tab>" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless flex)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (orderless flex)))
                                        (eglot (styles . (orderless flex))))))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :after evil
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ("C-s" . (lambda () (interactive) (consult-line (thing-at-point 'symbol))))
  ("M-S" . #'consult-line)
  ("<backtab>" . #'consult-buffer)
  ("C-c C-a" . #'consult-apropos)
  ("C-c m m" . #'consult-imenu-multi)
  ("C-c m b" . #'consult-imenu)
  ("C-<tab>" . #'consult-project-buffer)
  ("M-R" . #'consult-recent-file)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package embark
  :defer t
  :bind
  ("C-." . embark-act))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :defer t)

(use-package recentf
  :config
  (setq recentf-max-saved-items 200
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package mode-line-hud
  :ensure nil
  :config
  (setq show-in-echo-area nil))

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
        punch-line-modal-use-fancy-icon t
        punch-line-modal-divider-style 'circle
        punch-line-modal-size 'small
        punch-line-left-separator "  "
        punch-line-right-separator "  "
        punch-show-git-info t
        punch-show-lsp-info t
        punch-show-copilot-info nil
        punch-show-battery-info t
        punch-show-flycheck-info t
        punch-show-weather-info t
        punch-weather-latitude "56.7365"
        punch-weather-longitude "16.2981"
        punch-line-music-max-length 80
        punch-line-music-info '(:service apple)))

(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-want-Y-yank-to-eol t)
  :init
  (setq-default evil-symbol-word-search t)
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-undo-system 'undo-fu
	evil-want-fine-undo t
	evil-want-C-u-scroll t
	evil-want-minibuffer t
	evil-respect-visual-line-mode t
	evil-search-module 'evil-search
	evil-vsplit-window-right t
	evil-ex-search-persistent-highlight nil  ;; Don't keep highlights after search
	evil-ex-search-case 'smart  ;; Smart case sensitivity
	evil-ex-search-vim-style-regexp t
	evil-split-window-below t
	evil-kill-on-visual-paste nil
	evil-default-cursor t
        evil-echo-state nil
	evil-want-C-i-jump t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (define-key evil-motion-state-map (kbd "<up>") 'ignore)
  (define-key evil-motion-state-map (kbd "<down>") 'ignore)
  (define-key evil-motion-state-map (kbd "<left>") 'ignore)
  (define-key evil-motion-state-map (kbd "<right>") 'ignore)
  (define-key evil-motion-state-map (kbd "C-+") #'(lambda () (interactive) (enlarge-window-horizontally 3)))
  (define-key evil-motion-state-map (kbd "C--") #'(lambda () (interactive) (shrink-window-horizontally 3)))
  (define-key evil-motion-state-map (kbd "C-M-+") #'(lambda () (interactive) (enlarge-window 3)))
  (define-key evil-motion-state-map (kbd "C-M--") #'(lambda () (interactive) (shrink-window 3)))
  (define-key evil-insert-state-map (kbd "TAB") #'tab-to-tab-stop)

  (setq evil-normal-state-cursor '(box "systemBlueColor")
        evil-insert-state-cursor '(bar "systemRedColor")
        evil-visual-state-cursor '(hollow "systemPurpleColor"))
  ;; (evil-set-initial-state 'minibuffer-mode 'emacs)

  (evil-define-key 'normal evil-ex-map "q" 'safe-kill-buffer-and-window)
  (evil-define-key 'normal 'global
     ;; "q" 'minibuffer-quit
    "\C-g" 'minibuffer-quit)
  (evil-mode 1))

(with-eval-after-load 'evil
  (dolist (state '(normal insert visual motion emacs))
    (evil-define-key state 'global (kbd "s-M") nil)
    (evil-define-key state 'global (kbd "C-.") nil)
    (evil-define-key state 'global (kbd "C-k") nil)))

(use-package evil-collection
  :after evil
  :custom
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-vterm-setup)
  :init
  (evil-collection-init))

(use-package evil-mc
  :hook (evil-mode . global-evil-mc-mode)
  :bind (
   ("C-M-e" . evil-mc-make-all-cursors)
   (:map evil-mc-key-map
         ("C-M-<return>" . evil-mc-toggle-cursors)
         ("C-M-j" . evil-mc-make-and-goto-next-match)
         ("C-M-k" . evil-mc-make-and-goto-prev-match)
         ;; ("C-M-p" . evil-mc-pause-cursors)
         ;; ("C-M-n" . evil-mc-resume-cursors)
         ("C-g" . evil-mc-undo-all-cursors)
         ("<escape>" . evil-mc-undo-all-cursors)))
  :custom
  (evil-mc-mode-line-text-inverse-colors t)
  (evil-mc-undo-cursors-on-keyboard-quit t)
  (evil-mc-mode-line-text-cursor-color t)
  :config
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg))

(use-package evil-surround
  :after evil
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))

     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))

     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (after-init . global-evil-surround-mode))

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode 1))

(use-package evil-goggles
  :init
  (evil-goggles-mode)
  :after evil
  :config
  (setq evil-goggles-pulse t
        evil-goggles-duration 0.3)
  (evil-goggles-use-diff-faces))

(use-package undo-fu
 :after evil
  :custom
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000))
  :config
  (setq undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :hook (after-init . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

;; (use-package ws-butler
;;   :hook (prog-mode . ws-butler-mode))

(use-package evil-visualstar
  :after evil
  :hook (after-init . global-evil-visualstar-mode))

(use-package minimap
  :commands (minimap-mode)
  :config
  (setq minimap-width-fraction 0.0
        minimap-minimum-width 10
        minimap-always-recenter nil
        minimap-hide-fringes t
        minimap-dedicated-window t
        minimap-enlarge-certain-faces nil
        minimap-recenter-type 'relative
        minimap-window-location 'right)
  :custom-face
  (minimap-font-face ((t (:family "Minimap" :height 0.17 :group 'minimap)))))

(use-package rainbow-delimiters
  :ensure nil
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package colorful-mode
  :defer t
  :ensure t
  :hook (emacs-lisp-mode . colorful-mode)
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-alignment 'left)
  (colorful-prefix-string "●"))

(use-package google-this
  :commands (google-this)
  :bind ("C-x C-g" . google-this))

(use-package expand-region
  :defer t
  :bind ("C-x e" . er/expand-region))

(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :custom
  (setq eldoc-box-clear-with-C-g t
        eldoc-box-max-pixel-width 120
        eldoc-box-max-pixel-height 220)
  :bind ("C-x C-e" . (lambda ()
                       (interactive)
                       (eldoc-box-help-at-point))))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("<escape>" . corfu-quit)
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-preview-current 'insert)
  :init
  ;; (corfu-popupinfo-mode 1)
  (setq corfu-auto-delay 0.2          ; Reduced from 0.3
	corfu-auto-prefix 2
        corfu-popupinfo-delay 0.1
        corfu-preselect 'valid
        cofru-preview-current t
	corfu-quit-at-boundary 'separator
	corfu-quit-no-match t))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-extra-space t)
  (kind-icon-blend-background t)
  (kind-icon-blend-frac 0.10)
  :config
  (setq kind-icon-default-face 'corfu-default ; to compute blended backgrounds correctly
        kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 1))
  (setq kind-icon-use-icons t)
  (setq kind-icon-mapping
	  '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
	    (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
	    (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
	    (command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
	    (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
	    (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
	    (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
	    (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
	    (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
	    (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
	    (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
	    (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
	    (statement      "st"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
	    (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
	    (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
	    (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
	    (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
	    (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
	    (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
	    (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
	    (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
	    (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
	    (namespace      "ns"  :icon "file-code-outline"  :face font-lock-preprocessor-face)
	    (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
	    (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
	    (param          "pa"  :icon "gear"               :face default                          :collection "vscode")
	    (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
	    (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
	    (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
	    (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
	    (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
	    (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
	    (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
	    (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
	    (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
	    (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
	    (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
	    ;; add instance for python
	    (instance       "in"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
	    (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode")))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-to-list 'kind-icon-mapping '(tabnine "ai" :icon "cloud" :face shadow) t))


(use-package savehist
  :defer 2
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))

;; Add extensions
(use-package cape
  :after evil
  :bind (
         ("<Tab>" . cape-complete)
         ("TAB" . cape-complete)
         ("C-c p p" . completion-at-point) ;; capf
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p r" . cape-rfc1345))
  :config
  (add-to-list 'completion-at-point-functions #'cape-capf-buster)

  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-file)

  (advice-add #'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package darken-buffer
  :ensure nil
  :hook (after-init . darken-buffer-mode)
  :config
  (setq darken-buffer-ignore-buffers-regexp '("^\\*.*\\*$")
        darken-buffer-percentage 0
        lighten-inactive-buffer-percentage 3))

(use-package avy
  :defer t
  :bind ("M-g" . avy-goto-word-1)
  :config
  (setq avy-single-candidate-jump t))

(use-package dape
  :defer t
  :commands (dape-info dape-repl dape)
  :bind
  :config
  (setq dape-buffer-window-arrangement 'right
        dape-stack-trace-levels 10)
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl))

(use-package repeat
  :ensure nil
  :config (add-hook 'dape-on-start-hooks #'repeat-mode)
  :custom
  (repeat-too-dangerous '(kill-this-buffer))
  (repeat-exit-timeout 5))

(use-package treemacs
  :defer t
  :commands (treemacs treemacs-select-window)
  :hook (treemacs-mode . treemacs-project-follow-mode)
  :bind (("M-J" . #'treemacs-find-file)
         ("M-0" . #'treemacs))
  :config
  (let ((font-family "SF Compact Text"))
    (custom-set-faces
     `(treemacs-directory-face ((t (:family ,font-family :height 0.8 :weight light))))
     `(treemacs-directory-collapsed-face ((t (:family ,font-family :height 0.8 :weight light))))
     `(treemacs-git-ignored-face ((t (:family ,font-family :height 0.8 :slant italic :weight light))))
     `(treemacs-git-conflict-face ((t (:family ,font-family :height 0.8 :slant italic :weight light))))
     `(treemacs-git-unmodified-face ((t (:family ,font-family :height 0.8 :weight light))))
     `(treemacs-git-untracked-face ((t (:family ,font-family :height 0.8 :weight light))))
     `(treemacs-git-added-face ((t (:family ,font-family :height 0.8 :weight light))))
     `(treemacs-git-renamed-face ((t (:family ,font-family :height 0.8 :weight light))))
     `(treemacs-git-modified-face ((t (:family ,font-family :height 0.8 :weight light))))
     `(treemacs-tags-face ((t (:family ,font-family :height 0.8))))))
  :config
  (setq treemacs-follow-after-init t
        treemacs-collapse-dirs 0
        treemacs-directory-name-transformer #'identity
        treemacs-file-name-transformer #'identity
        treemacs-show-cursor nil
        treemacs-display-current-project-exclusively t
        treemacs-filewatch-mode t
        treemacs-follow-mode nil
        treemacs-hide-dot-git-directory t
        treemacs-git-integration t
        treemacs-space-between-root-nodes nil
        treemacs-hide-gitignored-files-mode t
        treemacs-git-mode 'extended
        treemacs-silent-refresh	t
        treemacs-indentation 1
        treemacs-sorting 'treemacs--sort-alphabetic-case-insensitive-asc
        treemacs-width 40))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package project-treemacs
  :after treemacs)

(use-package restclient
  :commands (restclient))

(use-package flycheck-package
  :defer t
  :hook (emacs-lisp-mode . flycheck-package-setup))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :diminish t
  :bind
  ("C-c f n" . flycheck-next-error)
  ("C-c f p" . flycheck-previous-error)
  :config
  (add-to-list 'flycheck-checkers 'javascript-eslint)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  :custom
  (flycheck-checker-error-threshold 200)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
	flycheck-idle-change-delay 2.0))

(use-package flycheck-overlay
  :ensure nil
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-overlay-mode)
  (setq flycheck-overlay-virtual-line-type 'line-no-arrow)
  (setq flycheck-overlay-virtual-line-icon nil)
  ;; (setq flycheck-overlay-checkers '(flycheck))
  )

(use-package flycheck-eglot
  :hook (eglot-managed-mode . flycheck-eglot-mode)
  :custom (flycheck-eglot-exclusive nil))

(use-package consult-flycheck
  :bind ("M-+" . consult-flycheck))

(use-package markdown-mode
  :defer t
  :mode "\\.md\\'"
  :commands (markdown-mode)
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . (lambda ()
                            (setq left-fringe-width 20 right-fringe-width 20)
                            (set-window-fringes nil left-fringe-width right-fringe-width)))))

(use-package yaml-mode
  :commands (yaml-mode))

(use-package project
  :ensure nil
  :bind ("M-O" . project-find-file))

(use-package pulsar
  :hook (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.1
        pulsar-iterations 10
        pulsar-face 'pulsar-cyan
        pulsar-highlight-face 'evil-ex-lazy-highlight
        pulsar-pulse-functions '(
				 evil-yank
				 evil-yank-line
				 evil-delete
				 evil-delete-line
				 evil-jump-item
				 evil-scroll-down
				 evil-scroll-up
				 evil-scroll-page-down
				 evil-scroll-page-up
				 evil-scroll-line-down
				 evil-scroll-line-up
				 evil-window-up
                                 evil-window-rotate-upwards
                                 evil-window-rotate-downwards
                                 evil-window-down
                                 evil-window-left
                                 evil-window-right
                                 evil-window-vsplit
                                 evil-window-split)))

(use-package imenu-list
  :bind
  ("C-c i" . 'imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

(use-package darkroom
  :defer t
  :bind ("C-x C-d" . darkroom-tentative-mode)
  :config
  (setq darkroom-text-scale-increase 1.5
        darkroom-margins '(12 . 0)))

(use-package magit
  :defer t
  :commands (magit-status magit-ediff-show-working-tree)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-save-repository-buffers nil)
  (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package blamer
  :commands blamer-mode
  :config
  (setq blamer-tooltip-function 'blamer-tooltip-author-info)
  (setq blamer-view 'overlay
        blamer-type 'overlay-popup
        blamer-max-commit-message-length 270
        blamer-force-truncate-long-line nil
        blamer-show-avatar-p t
        blamer-author-formatter " ✎ %s "
        blamer-commit-formatter "● \'%s\' ● ")
  :custom
  (blamer-idle-time 1.0)
  :custom-face
  (blamer-face ((t :foreground "#E46876"
                   :height 130
                   :bold t
                   :italic t))))

(use-package git-gutter
  :defer 2
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :config
  (setq git-gutter:modified-sign "┃"
        git-gutter:added-sign "┃"
        git-gutter:deleted-sign "┃")
  (setq git-gutter:window-width 1)  ;; Set to minimum width
  (setq git-gutter:update-interval 2))

(use-package vterm
  :defer t
  :commands vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (setq vterm-timer-delay nil
	vterm-clipboard-warning-max-lines 20
	vterm-clipboard-warning-max-chars 256))


(use-package dall-e-shell
  :defer t
  :config
  (setq dall-e-shell-openai-key (shell-command-to-string "echo $OPENAI_API_KEY")))

(use-package chatgpt-shell
  :defer t
  :bind
  ("C-x C-v" . chatgpt-shell-quick-insert)
  ("C-x C-p" . chatgpt-shell-prompt-compose)
  ("C-x c g s" . chatgpt-shell-send-and-review-region)
  ("C-x c g r" . chatgpt-shell-refactor-code)
  ("C-x C-s" . (lambda ()
                 (interactive)
                 (split-window-right)
                 (other-window 1)
                 (chatgpt-shell)))
  :config
  (require 'chatgpt-shell-ollama)
  (defun add-qwen-to-ollama-models (orig-fun)
    "Add qwen2.5-coder to the list of Ollama models."
    (append (funcall orig-fun)
            (list
             (chatgpt-shell-ollama-make-model
              :version "deepseek-r1:8b"
              :token-width 4
              :context-window 8192))))
  (advice-add 'chatgpt-shell-ollama-models :around #'add-qwen-to-ollama-models)
  (setf chatgpt-shell-anthropic-key (getenv "ANTHROPIC_API_KEY"))
  (setf chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
  (setq chatgpt-shell-model-version "claude-3-5-sonnet-20241022"))

(use-package gptel
  :defer t
  :commands (gptel gptel-menu gptel-send gptel-request)
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'claude-3-5-sonnet-20241022)
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (getenv "ANTHROPIC_API_KEY"))))

(use-package ob-chatgpt-shell
  :ensure nil
  :commands (org-babel-execute:chatgpt-shell)
  :config
  (ob-chatgpt-shell-setup))

(defun project-root-override (dir)
  "Find DIR's project root by searching for a '.project.el' file."
  (let ((root (or (locate-dominating-file dir ".xcodeproj")
                  (locate-dominating-file dir ".envrc")
                  (locate-dominating-file dir ".projectile")))
        (backend (ignore-errors (vc-responsible-backend dir))))
    (when root (if (version<= emacs-version "28")
                    (cons 'vc root)
                 (list 'vc backend root)))))

(use-package project
  :defer t
  :ensure nil
  :config
  (add-hook 'project-find-functions #'project-root-override))

(use-package paren
  :defer 2
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; general
(use-package general
  :ensure t
  :config
  (general-create-definer mk/leader-keys
    :keymaps '(normal insert emacs visual operator hybrid xwidget-webkit)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (mk/leader-keys
    "TAB" '((lambda () (interactive) (switch-to-buffer nil)) :which-key "Toggle buffers")
    "SPC" '(execute-extended-command :which-key "M-x")
    "s" '(consult-line-multi :which-key "Consult multi search")
    "F" '(consult-line :which-key "Consult outline")
    "0" '(treemacs-select-window :which-key "Treemacs")
    "P" 'package-install
    "'" '((lambda () (interactive) (toggle-vterm)) :which-key "Term"))

  (mk/leader-keys
    "gg" '(google-this :which-key "Google this"))

  (mk/leader-keys
    "cc" '(calendar :which-key "Calendar"))

  (mk/leader-keys
    "bm" '(lambda () (interactive) (switch-to-buffer "*Messages*") :which-key "Message buffer")
    "bs" '(lambda () (interactive) (switch-to-buffer "*scratch*") :which-key "Scratch buffer"))

  (mk/leader-keys
    "ee" '(eval-expression :which-key "Eval expression")
    "eb" '(eval-buffer :which-key "Eval buffer")
    "el" '(eval-last-sexp :which-key "Eval before point")
    "ea" '(embark-act :which-key "Embark act")
    "er" '(eval-region :which-key "Eval region")
    "er" '(eval-region :which-key "Eval region")
    "ef" '(lambda () (interactive) (elfeed) :which-key "Elfeed"))

  (mk/leader-keys
    "fs" '(save-buffer :which-key "Save file")
    "fb" '(consult-buffer :which-key "Find buffer")
    "ff" '(find-file :which-key "Find file")
    "fl" '(consult-line-multi :which-key "Find line in project")
    "fr" '(consult-recent-file :which-key "Recent files")
    "fn" '(create-file-buffer :which-key "New file")
    "fR" '(dired-rename-file :which-key "Rename file")
    "fD" '(delete-file :which-key "Delete file")
    "fe" '(lambda () (interactive) (find-file user-init-file) :which-key "User configuration"))

  (mk/leader-keys
    "hc" '(helpful-command :which-key "Describe command")
    "hk" '(helpful-key :which-key "Describe key")
    "hv" '(helpful-variable :which-key "Describe variable")
    "ht" '(evil-tutor-start :which-key "Evil tutorial")
    "h." '(helpful-at-point :which-key "Describe at-point")
    "hp" '(describe-package :which-key "Describe package"))

  (mk/leader-keys
    "ts" '(sort-lines :which-key "Sort lines")
    "tx" '(delete-trailing-whitespace :which-key "Delete trailing whitespace"))

  (mk/leader-keys
    "wb" '((lambda () (interactive) (xwidget-webkit-browse-url "https://www.duckduckgo.com")) :which-key "Start a browser")
    "wp" '(previous-window-any-frame :which-key "Previous window")
    "wx" '(delete-window :hich-key "Delete window"))

  (mk/leader-keys
    "pf" '(consult-ripgrep :which-key "Find symbol in project")
    "ps" '(project-switch-project :which-key "Switch project"))

  (mk/leader-keys
    "vs" '(magit-status :which-key "Status")
    "vb" '(blamer-show-commit-info :which-key "Show git blame")
    "vd" '(magit-diff-buffer-file :which-key "Diff current buffer")
    "vw" '(magit-diff-working-tree :which-key "Diff working tree"))

  (mk/leader-keys
    "qq" '(save-buffers-kill-terminal :which-key "Quit emacs")
    "qr" '(restart-emacs :which-key "Restart emacs")))

(setq org-custom-todo-faces '
      (("TODO" :background "#FF5D62" :distant-foreground "#FF5D62" :foreground "#FFFFFF" :weight 'bold)
       ("NEXT" :background "#7FB4CA" :distant-foreground "#7FB4CA" :foreground "#1c1c24" :weight 'bold)
       ("STARTED" :background "#957FB8" :foreground "#FFFFFF" :weight 'bold)
       ("DELEGATED" :background "#7AA89F" :foreground "#1c1c24" :weight 'bold)
       ("QA" :background "#54536D" :weight 'bold)))

(use-package org
  :hook ((org-mode . org-display-inline-images))
  :config
  (setq org-ellipsis "…"

        ;; org-agenda-files '("work.org" "projekt.org")
        org-agenda-block-separator ?─
        org-agenda-files '("~/Desktop/org/agenda.org")
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-done t
        org-agenda-span 1
        org-agenda-start-day "+0d"
        org-agenda-tags-column 0
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-custom-commands '(("v" "Agenda and todos"
                                      ((tags "PRIORITY=\"A\""
                                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                              (org-agenda-overriding-header "HIGH PRIORITY unfinished tasks:")))
                                      (tags "PRIORITY=\"B\""
                                            ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                             (org-agenda-overriding-header "MEDIUM PRIORITY unfinished tasks:")))
                                      (tags "PRIORITY=\"C\""
                                            ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                             (org-agenda-overriding-header "LOW PRIORITY unfinished tasks:")))
                                      (agenda "")
                                      (alltodo ""))))
        org-auto-align-tags nil
        org-clock-sound "~/.emacs.d/etc/sound/bell.mp3"
        org-confirm-babel-evaluate nil
        org-cycle-separator-lines 2
        org-directory "~/Desktop/org/Todo/"
        org-fontify-emphasized-text t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-log-done 'time
        org-log-into-drawer t
        org-pretty-entities t
        org-return-follows-link t
        org-src-edit-src-content-indentation 0
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-startup-folded nil
        org-startup-with-inline-images t
        org-tags-column 0
        org-todo-keyword-faces org-custom-todo-faces
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "STARTED(s)" "DELEGATED(d)" "QA(q)" "|" "DONE(d)" "CANCELLED(c)"))
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────"))

(defun mk/play-sound (orgin-fn sound)
  "Play a sound when a task is marked as done."
  (cl-destructuring-bind (_ _ file) sound
    (make-process :name (concat "play-sound-" file)
                  :connection-type 'pipe
                  :command `("afplay" ,file))))
(advice-add 'play-sound :around 'mk/play-sound)

(with-eval-after-load 'org

  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))

  (require 'org-tempo)
  (require 'ob-swiftui)
  (ob-swiftui-setup)

  (setq-local org-confirm-babel-evaluate t)

 (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.2 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.18 :weight bold))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.16 :weight bold))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.15 :weight bold))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.1 :weight bold))))
   '(org-level-6 ((t (:inherit outline-5 :height 1 :weight bold))))
   '(org-level-7 ((t (:inherit outline-5 :height 1 :weight semi-bold)))))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (swift . t)
                                 (shell . t)
                                 (restclient . t)))

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sw" . "src swift"))
  (add-to-list 'org-structure-template-alist '("sr" . "src restclient"))
  (add-to-list 'org-structure-template-alist '("swiftui" . "src swiftui :view CustomView"))
  (add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))
  (add-to-list 'org-modules 'org-tempo t))

(use-package restclient
  :defer t)

(use-package ob-restclient
  :defer t)

(use-package ob-swift
  :defer t
  :after org-mode
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((swift . t)))))
(use-package ob-swiftui
  :defer t
  :after org-mode
  :config
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))
  (add-to-list 'org-babel-tangle-lang-exts
               '("swiftui" . "swift"))
  (add-to-list 'org-src-lang-modes
               '("swiftui" . swift))
  (ob-swiftui-setup))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :config
  (setq org-modern-todo-faces org-custom-todo-faces
        org-modern-hide-stars t
        org-modern-star '("❶" "❷" "❸" "❹" "❺" "❻" "❼")))

(use-package olivetti
  :hook ((org-mode . olivetti-mode))
  :custom
  (setq olivetti-style t
        olivetti-hide-mode-line t))

(use-package elfeed
  :commands elfeed
  :config

(defun add-icon-to-title (icon tag all-tags title)
  "Add ICON if TAG is present in ALL-TAGS to the TITLE string."
  (if (member tag all-tags)
      (concat icon " ")
    ""))

(defun my/elfeed-search-print-entry (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (if (member 'unread (elfeed-entry-tags entry))
                          (elfeed-search--faces (elfeed-entry-tags entry))
                        '(:weight thin :inherit font-lock-comment-face)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed
                      (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (seq-filter
                (lambda (tag) (not (string-equal feed-title tag)))
                (mapcar #'symbol-name (elfeed-entry-tags entry))))
         (icons '(("swift" . "")
                  ("swiftui" . "")
                  ("emacs" . "")
                  ("neovim" . "")
                  ("artificialInteligence" . "")
                  ("singularity" . "")
                  ("kotlin" . "")
                  ("techcrunch" . "")))
         (title-with-icons (concat
                           (mapconcat
                            (lambda (icon-pair)
                              (add-icon-to-title (cdr icon-pair) (car icon-pair) tags " "))
                            icons "")
                           " "
                           title))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title-with-icons (elfeed-clamp
                                elfeed-search-title-min-width
                                title-width
                                elfeed-search-title-max-width)
                        :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    ;; (when feed-title
    ;;   (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags
      (insert tags-str))))
  
  (setq elfeed-search-print-entry-function #'my/elfeed-search-print-entry)
  (setq elfeed-feeds '(
                       ("https://www.reddit.com/r/emacs.rss" emacs)
                       ("https://www.reddit.com/r/neovim.rss" neovim)
                       ("https://www.reddit.com/r/kotlin.rss" kotlin)
                       ("https://www.reddit.com/r/swift.rss" swift)
                       ("https://www.reddit.com/r/swiftui.rss" swiftui)
                       ("https://www.reddit.com/r/artificialInteligence.rss" artificialInteligence)
                       ;; ("https://techcrunch.com/rss" techcrunch)
                       ))

  (setq elfeed-search-face-alist
        '((emacs font-lock-function-name-face)
          (neovim font-lock-type-face)
          (kotlin font-lock-keyword-face)
          (swift font-lock-constant-face)
          (techcrunch font-lock-variable-name-face)
          (ai font-lock-number-face)
          (singularity font-lock-number-face)
          (read font-lock-comment-face)))
  (setq elfeed-search-filter "@4-days-ago +unread"
        elfeed-search-title-max-width 140
        elfeed-search-title-min-width 140))


(use-package highlight-symbol
  :defer t
  :config
  (setq highlight-symbol-idle-delay 0.8))

;; Drag lines and regions around
(use-package drag-stuff
  :ensure t
  :defer t
  :bind (:map evil-visual-state-map
	      ("C-j" . drag-stuff-down)
	      ("C-k" . drag-stuff-up)))

;; Quickly jump to definition or usage

(use-package swift-ts-mode
  :mode "\\.swift\\'"
  :ensure nil
  :custom
  (swift-ts-basic-offset 4)
  (swift-ts:indent-trailing-call-member t)
  :config
  (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-ts-mode)))

(use-package localizeable-mode
  :mode "\\.strings\\'"
  :bind (:map localizeable-mode-map
              ("C-c C-c" . #'swift-additions:compile-and-run)
              ("C-c C-k" . #'periphery-run-loco))
  :defer t
  :ensure nil)

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
        ("C-x s l" . #'ios-simulator:change-language)))

(use-package swift-additions
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
	("M-r" . #'swift-additions:run)
	("C-c t m" .  #'swift-additions:test-module-silent)
	("C-c t p" .  #'swift-additions:test-swift-package-from-file)
	("C-c C-c" . #'swift-additions:compile-and-run)
	("C-c C-b" . #'swift-additions:compile-app)
	("C-c C-f" . #'periphery-search-dwiw-rg)))

(use-package domain-blocker
  :ensure nil
  :after swift-ts-mode)

(use-package swift-lsp
  :ensure nil)

(use-package eglot
  :hook (((swift-ts-mode) . eglot-ensure)
	 ((kotlin-mode kotlin-ts-mode) . (lambda ()
                                           (eglot-ensure)
                                           (eldoc-mode -1))))
  :ensure nil
  :bind
  ("C-c e f" . #'eglot-code-action-quickfix)
  ("C-c e a" . #'eglot-code-actions)
  ("C-c e e" . #'eglot-code-action-extract)
  ("C-c e R" . #'eglot-code-action-rewrite)
  ("C-c e r" . #'eglot-rename)
  ("C-c e m" . #'eglot-menu)
  ("C-c e d" . #'eglot-find-declaration)
  ("C-c e D" . #'eglot-find-typeDefinition)
  ("C-c e i" . #'eglot-find-implementation)
  ("C-c e b" . #'eglot-format-buffer)
  :custom
  (eglot-report-progress nil)
  (eglot-autoshutdown nil)
  (eglot-connect-timeout 120)
  (eglot-sync-connect 3)
  (eglot-events-buffer-size 0)
  (eglot-events-buffer-config '(size: 0 :format full))
  :config
  (add-to-list 'eglot-server-programs '(swift-ts-mode . my-swift-mode:eglot-server-contact))
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-tsx-mode tsx-ts-mode) . ("typescript-language-server" "--stdio")))

  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  
  (setq eglot-stay-out-of '(corfu company flycheck)
	eglot-extend-to-xref t
	eglot-send-changes-idle-time 0.5
        eldoc-documentation-strategy 'eldoc-documentation-default
        jsonrpc-event-hook nil)
  (advice-add 'jsonrpc--log-event :override #'ignore))

(use-package xcode-additions
 :ensure nil
 :after swift-ts-mode
 :bind
 (:map swift-ts-mode-map
       ("M-K" .  #'xcode-additions:clean-build-folder)
       ("C-c C-d" . #'xcode-additions:start-debugging)
       ("C-c C-x" . #'xcode-additions:reset)))

(use-package swift-refactor
  :ensure nil
  :after swift-ts-mode
  :bind
  ("C-c r s" . #'swift-refactor:split-function-list)
  ("M-t" . #'swift-refactor:insert-todo)
  ("M-m" . #'swift-refactor:insert-mark)
  (:map swift-ts-mode-map
        ("C-c x t" . #'xcode-additions:toggle-device-choice)
        ("C-c x c" . #'xcode-additions:show-current-configuration)
        ("C-c r a" . #'swift-refactor:wrap-selection)
        ("C-c r d" . #'swift-refactor:delete-current-line-with-matching-brace)
        ("C-c r i" . #'swift-refactor:tidy-up-constructor)
        ("C-c r r" . #'swift-refactor:extract-function)
        ("M-P" .  #'swift-refactor:print-thing-at-point)
        ("C-c r t" . #'swift-refactor:add-try-catch)))

(use-package apple-docs-query
  :ensure nil
  :defer t
  :bind
  ("C-c C-a" . #'apple-docs/query)
  ("C-c C-A" . #'apple-docs/query-thing-at-point))

(use-package hacking-with-swift
  :ensure nil
  :defer t
  :bind
  ("C-c C-h" . #'hacking-ws/query)
  ("C-c C-H" . #'hacking-ws/query-thing-at-point))

(use-package periphery-quick
  :ensure nil
  :after prog-mode
  :bind
  ("C-c S" . #'periphery-quick:find-ask)
  ("M-F" . #'periphery-quick:find-ask)
  ;; ("C-c f f" . #'periphery-quick:find-in-file)
  ("C-c f t" . #'periphery-quick:todos))

(use-package periphery-search
  :ensure nil
  :after prog-mode
  :bind (:map prog-mode-map
              ("C-c C-s" . #'periphery-search-rg)
              ("C-c C-f" . #'periphery-search-dwiw-rg)
              ("C-x C-t" . #'periphery-query-todos-and-fixmes)
              ("C-x C-m" . #'periphery-query-marks)
              ("M-?" . #'periphery-toggle-buffer)))

(use-package periphery-swiftformat
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
        ("C-c C-o" . #'periphery-swiftformat-lint-buffer)
        ("M-o" . #'periphery-swiftformat-autocorrect-buffer)
        ("C-c C-p" . #'periphery-run-swiftformat-for-project)))

(use-package periphery-ktlint
  :ensure nil
  :after kotlin-ts-mode
  :bind
  (:map kotlin-ts-mode-map
        ("C-c C-o" . #'periphery-ktlint-lint-buffer)
        ("M-o" . #'periphery-ktlint-autocorrect-buffer)))

(use-package periphery-loco
  :ensure nil
  :after swift-ts-mode
  :bind
  ("C-c C-k" . #'periphery-run-loco))

(use-package periphery-swiftlint
  :ensure nil
  :after swift-ts-mode
  :bind
  ("C-c C-l" . #'periphery-run-swiftlint))

(use-package filer
  :ensure nil
  :bind
  ("C-c f f" . filer-find-file)
  :config
  (setq filer-include-project-name nil))

(use-package svg-tag-mode
  :defer 10
  :hook ((swift-ts-mode . svg-tag-mode)
         (localizeable-mode . svg-tag-mode)
         (kotlin-ts-mode . svg-tag-mode))
  :config
  (plist-put svg-lib-style-default :font-family "Jetbrains Mono")
  (plist-put svg-lib-style-default :font-size 15)
  :init
  (setq svg-tag-tags (periphery-svg-tags)))

(defun mk/browser-split-window (url &optional new-window)
  "Create a new browser (as URL as NEW-WINDOW) window to the right of the current one."
  (interactive)
  (let ((ignore-window-parameters t)
        (dedicated-p (window-dedicated-p)))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (xwidget-webkit-browse-url url)))

(defun mk/toggle-flycheck-errors ()
  "Function to toggle flycheck errors."
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
    (list-flycheck-errors)))

(defun toggle-vterm ()
  "Toggle vterm buffer."
  (interactive)
  (if (get-buffer "*vterm*")
      (if (eq (current-buffer) (get-buffer "*vterm*"))
          (delete-window)
        (switch-to-buffer-other-window "*vterm*"))
    (progn
      (vterm-other-window))))

(defun mk/recompile (&optional force)
  "Recompile files (as FORCE) force compilation."
  (interactive "p")
  (byte-recompile-directory (locate-user-emacs-file "localpackages") 0)
  (byte-recompile-directory (locate-user-emacs-file "themes") 0))

(defun xref-eglot+dumb-backend ()
  "Return the xref backend for eglot+dumb."
  'eglot+dumb)

(advice-add 'eglot-xref-backend :override 'xref-eglot+dumb-backend)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot+dumb)))
  "Return the identifier at point for eglot+dumb."
  (cons (xref-backend-identifier-at-point 'eglot)
        (xref-backend-identifier-at-point 'dumb-jump)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot+dumb)))
  "Return the completion table for eglot+dumb."
  (xref-backend-identifier-completion-table 'eglot))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot+dumb)) identifier)
  "Return the definitions for eglot+dumb."
  (or (xref-backend-definitions 'eglot (car identifier))
      (xref-backend-definitions 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-references ((_backend (eql eglot+dumb)) identifier)
  (or (xref-backend-references 'eglot (car identifier))
      (xref-backend-references 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot+dumb)) pattern)
  (xref-backend-apropos 'eglot pattern))

(put 'narrow-to-region 'disabled nil)

;; (use-package window-stool
;;   ;; :vc (:url "https://github.com/JasZhe/window-stool" :rev :newest)
;;   :hook (prog-mode . window-stool-mode)
;;   :defer t
;;   :config
;;   (setq window-stool-n-from-top 2
;;         window-stool-n-from-bottom 0))


(use-package treesit
  :ensure nil
  :config
  (setq treesit-font-lock-level 4))



  ;; :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
  ;; :vc (:url "https://github.com/JasZhe/window-stool" :rev :newest)
;; (package-vc-install "https://github.com/JasZhe/window-stool")
;; (package-vc-install "https://github.com/jdtsmith/eglot-booster")
;; (package-vc-install "https://github.com/chep/copilot-chat.el")
;; (package-vc-install "https://github.com/copilot-emacs/copilot.el"):
;; (package-vc-install "https://github.com/orzechowskid/tsx-mode.el.git")


(use-package nxml-mode
  :ensure nil
  :mode "\\.xml\\'"
  :hook ((nxml-mode . setup-programming-mode)
	 (nxml-mode . rainbow-mode)))

(defun safe-kill-buffer-and-window ()
  "Kill the current buffer and delete its window if it's not the last one."
  (interactive)
  (let* ((window-to-delete (selected-window))
         (buffer-to-kill (current-buffer))
         (frame (selected-frame))
         (windows (window-list frame)))
    (if (= 1 (length windows))
        (kill-buffer buffer-to-kill)
      (when (kill-buffer buffer-to-kill)
        (when (member window-to-delete windows)
          (delete-window window-to-delete))))))

(use-package compile
  :defer t
  :ensure nil
  :hook (compilation-finish-functions .
         (lambda (buf str)
           (when (string-match "finished" str)
             (run-at-time "1 sec" nil 'delete-windows-on buf)
             (bury-buffer buf))))
  :custom
  ((compilation-always-kill t)
   (compilation-auto-jump-to-first-error t)
   (compilation-ask-about-save nil)
   (compilation-skip-threshold 1)
   (compilation-scroll-output 'all)
   (compilation-highlight-overlay t)
   (compilation-environment '("TERM=dumb" "TERM=xterm-256color"))
   (compilation-window-height 15)
   (compilation-reuse-window t)
   (compilation-max-output-line-length nil))
  :config
  (setq compilation-scroll-output t)
  (require 'ansi-color)
  (defun my/colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer))

(use-package flycheck-kotlin
  :defer t
  :hook ((kotlin-mode kotlin-ts-mode) . flycheck-kotlin-setup))

(use-package kotlin-ts-mode
  :defer t
  :mode "\\.kt\\'"
  :config
  (setq treemacs-width 45)
  (setq treesit-font-lock-level 4))

(use-package kotlin-development
  :mode "\\.kt\\'"
  :defer t
  :hook ((kotlin-mode kotlin-ts-mode) . kotlin-development-mode-setup)
  :ensure nil  ; if it's a local package
  :bind ((:map kotlin-mode-map
               ("C-c C-c" . kotlin-development-build-and-run)
               ("M-K" . kotlin-development-clean-build)
               ("C-c C-e l" . kotlin-development-list-emulators)
               ("C-c C-e k" . kotlin-development-kill-emulator))
         (:map kotlin-ts-mode-map
               ("C-c C-c" . kotlin-development-build-and-run)
               ("C-c C-e l" . kotlin-development-list-emulators)
               ("C-c C-e k" . kotlin-development-kill-emulator)))
  :config
  (setq kotlin-development-emulator-name "Medium_Phone_API_35"))

(use-package copilot
  :vc (copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main" :rev :newest)
  :ensure nil
  :hook ((prog-mode localizeable-mode) . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("TAB" . copilot-accept-completion)
        ("C-c C-n" . copilot-next-completion)
        ("C-c C-p" . copilot-previous-completion))
  :config
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char 1000000))


(use-package copilot-chat
  :vc (copilot-chat
       :url "https://github.com/chep/copilot-chat.el"
       :branch "master"
       :rev :newest)
  :defer t
  :commands (copilot-chat-doc
             copilot-chat-explain
             copilot-chat-fix
             copilot-chat-test
             copilot-chat-review
             copilot-chat-optimize
             copilot-chat-add-current-buffer
             copilot-chat-ask-and-insert
             copilot-chat-custom-prompt-selection)
  :bind
  (("C-x c p t" . copilot-chat-transient)
   ("C-x c p d" . copilot-chat-doc)                     ;; Open documentation
   ("C-x c p e" . copilot-chat-explain)                 ;; Explain code
   ("C-x c p f" . copilot-chat-fix)                     ;; Fix code issues
   ("C-x c p r" . copilot-chat-review)                  ;; Review code
   ;; ("C-x c p b" . copilot-chat-review-whole-buffer)     ;; Review code
   ("C-x c p b" . copilot-chat-add-current-buffer)      ;; Add current buffer
   ("C-x c p a" . copilot-chat-ask-and-insert)          ;; Ask and insert
   ("C-x c p s" . copilot-chat-custom-prompt-selection) ;; Custom prompt selection
   ("C-x c p o" . copilot-chat-optimize)))              ;; Optimize code

(add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)

;; (use-package jinx
;;   :hook (after-init . global-jinx-mode)  ; Enable Jinx globally
;;   :bind (("C-x c k" . jinx-correct)      ; Traditional Emacs spell-check binding
;;          ("C-x c l" . jinx-languages))   ; Quick language switching
;;   :config
;;   (setq jinx-languages "en")
;;   (setq jinx-exclude-modes
;;         '(minibuffer-mode          ; Mini buffer
;;           dired-mode               ; Directory editor
;;           fundamental-mode))       ; Fundamental mode
;;   (custom-set-faces
;;    '(jinx-misspelled ((t (:underline (:style wave :color "red")))))))

(use-package ultra-scroll
  :vc (ultra-scroll
       :url "https://github.com/jdtsmith/ultra-scroll"
       :main-file "ultra-scroll.el"
       :branch "main"
       :rev :newest)
  :init
  (setq scroll-margin 0
        scroll-conservatively 101)
  :config
  (ultra-scroll-mode 1))

(use-package eglot-booster
  :vc (eglot-booster
       :url "https://github.com/jdtsmith/eglot-booster"
       :branch "main"
       :rev :newest)
  :after eglot
  :config (eglot-booster-mode))

(use-package indent-bars
  :vc (indent-bars
       :url "https://github.com/jdtsmith/indent-bars"
       :branch "main"
       :rev :newest)
  :hook ((emacs-lisp-mode swift-ts-mode kotlin-ts-mode) . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.15))
  (indent-bars-highlight-current-depth '(:blend 0.5)) ; pump up the BG blend on current
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("comment")) ; Ignore comments
  (indent-bars-width-frac 0.1)
  (indent-bars-prefer-character t))

(use-package music-control
  :ensure nil
  :hook (after-init . music-control-mode))

(use-package aidermacs
  :defer t
  :vc (aidermacs
       :url "https://github.com/MatthewZMD/aidermacs"
       :branch "main"
       :rev :newest)
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  (setq aidermacs-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  (setenv "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY"))
  ;; Or use chatgpt model since it is most well known
  ;; (setq aider-args '("--model" "o3-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or use gemini v2 model since it is very good and free
  ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu))

(provide 'init)
;;; init.el ends here
