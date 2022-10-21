;;; init.el --- -*- lexical-binding: t -*-
;;; Code:

;; Window

(eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

(display-battery-mode t)		  ;; Show battery.
(display-time-mode t)			  ;; Show time.
(set-fringe-mode 1)               ;; Give us some space.
(delete-selection-mode nil)		  ;; Use a more sane delete mode than evil.
(fset 'yes-or-no-p 'y-or-n-p)     ;; Set yes or no to y/n
(global-font-lock-mode 1)         ;; always highlight code
(global-auto-revert-mode 1)       ;; refresh a buffer if changed on disk
(global-hl-line-mode 1)           ;; Highlight current line
(savehist-mode 1)                 ;; Save history
(save-place-mode 1)               ;; when buffer is closed, save the cursor position
(blink-cursor-mode 1)

;; Setup fonts
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono" :height 160)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font Mono")
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 150)
(variable-pitch-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq ad-redefinition-action            'accept
      default-buffer-file-coding-system 'utf-8
      blink-cursor-interval             0.6       ;; Little slower cursor blinking . default is 0.5
      create-lockfiles                  nil
      idle-update-delay                 1.2    ;; Speed things up by not updating so often
      read-process-output-max           (* 8 1024 1024)
      ediff-split-window-function       'split-window-horizontally
      highlight-nonselected-windows     t
      auto-mode-case-fold               nil
      backup-by-copying                 t
      backup-directory-alist            '(("." . "~/.emacs.d/backups"))
      byte-compile-warnings             '(ck-functions)
      confirm-kill-processes            nil
      fast-but-imprecise-scrolling      t
      jit-lock-defer-time               0.0
      echo-keystrokes                   0.2
      kill-buffer-query-functions       nil    ;; Dont ask for closing spawned processes
      line-number-mode                  nil
      use-dialog-box                    nil
      load-prefer-newer                 t
      word-wrap                         nil
      visible-bell                      nil
      bidi-display-reordering           nil
      x-stretch-cursor                  t   ;; stretch cursor on tabs
      scroll-margin                     4   ;; scroll N to screen edge
      undo-limit                        6710886400 ;; 64mb
      undo-strong-limit                 100663296 ;; x 1.5 (96mb)
      undo-outer-limit                  1006632960) ;; x 10 (960mb), (Emacs uses x100), but this seems too high.

(setq gc-cons-threshold (eval-when-compile (* 50 1024 1024)))
(run-with-idle-timer 4 t (lambda () (garbage-collect)))

(setq use-package-verbose nil
      use-package-expand-minimally nil
      use-package-compute-statistics nil
      use-package-minimum-reported-time nil
      debug-on-error nil)

(setq-default display-line-numbers-width    4            ;; Set so we can display thousands of lines
              c-basic-offset                4            ;; Set tab indent for c/c++ to 4 tabs
              tab-width                     4            ;: Use four tabs
              line-spacing                  0            ;; Increase linespacing a bit
              truncate-lines                1			 ;; Truncate lines
              indent-tabs-mode              nil			 ;; Never use tabs. Use spaces instead
              completion-ignore-case        t            ;; Ignore case when completing
              indent-line-function          'insert-tab  ;; Use function to insert tabs
              history-length                100)

(add-to-list 'load-path (concat user-emacs-directory "localpackages"))
(eval-when-compile (defvar savehist-additional-variables))
(add-to-list 'savehist-additional-variables 'kill-ring)
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

;; Dont leave #file autosaves everywhere I go
(defvar my-auto-save-folder (concat user-emacs-directory "var/auto-save/"))
(setq auto-save-list-file-prefix (concat my-auto-save-folder ".saves-")
      auto-save-file-name-transforms `((".*", my-auto-save-folder t))
      custom-file (concat user-emacs-directory "var/custom.el"))
      
;; Initialize package sources
(require 'package)
;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Clean up all those temporary files
(use-package no-littering)

; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none
        dired-use-ls-dired nil
        frame-title-format ""
        browse-url-browser-function #'mk/browser-split-window)

  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(use-package flyspell
  :defer 10
  :config (setq ispell-program-name "aspell"))

(use-package autothemer
  :config
 ;; (load-theme 'catppuccin-latte t)
  ;; (load-theme 'catppuccin-frappe t)
   ;; (load-theme 'catppuccin-macchiato t)
 (load-theme 'catppuccin-mocha t)
  ;; (load-theme 'kanagawa t)
  ;; (load-theme 'doom-old-hope t)
  )

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize t
        vertico-cycle t))

(use-package vertico-posframe
  :after vertico
  :config (vertico-posframe-mode 1)
  (setq
   vertico-posframe-width 140
   vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
   ;; vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center
   ;; vertico-posframe-poshandler #'posframe-poshandler-frame-center ;
   vertico-posframe-height nil
   vertico-posframe-border-width 3
   vertico-posframe-parameters
   '((left-fringe . 1)
     (right-fringe . 1))))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("<tab>" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(substring orderless flex)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heave marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ("C-s" . consult-line-symbol-at-point)
  ("M-l" . consult-goto-line)
  ("M-f" . consult-imenu-multi))

(defun consult-line-symbol-at-point ()
  "Search consult - thing at point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(use-package consult-ag
  :after consult)

(use-package consult-projectile
  :after projectile)

(use-package embark
  :bind
  (("C-," . embark-act)         ;; pick some comfortable binding
   ("C-x C-e" . embark-dwim)        ;; good alternative: M-.
   ("C-x C-x" . kill-buffer-and-window)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package recentf
  :hook (after-init . recentf-mode))

;; Make sure we are up to date, atleast once a week
(use-package auto-package-update
  :defer t
  :custom
  (setq auto-package-update-interval 7
        auto-package-update-prompt-before-update t
        auto-package-update-hide-results nil))

;; Config and install modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 50
        doom-modeline-major-mode-icon nil
        doom-modeline-icon t
        doom-modeline-modal-icon t
        doom-modeline-lsp nil
        doom-modeline-major-mode-color-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-time-icon nil)
  (custom-set-faces
   '(mode-line ((t (:family "Iosevka Aile" :height 1.0))))
   '(mode-line-active ((t (:family "Iosevka Aile" :height 1.0)))) ; For 29+
   '(mode-line-inactive ((t (:family "Iosevka Aile" :height 0.95))))))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (concat user-emacs-directory "themes/emacs.png")
        dashboard-path-style 'truncate-beginning
        dashboard-banner-logo-title "Mikaels dashboard!"
        dashboard-set-file-icons t
        dashboard-set-init-info t
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-items '(
                          (projects . 3)
                          (recents . 9))))

; helpful
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package undo-fu
  :defer t)

; Use evil mode
(use-package evil
  :hook (after-init . evil-mode)
  :bind ("<escape>" . keyboard-escape-quit)
  :init
  (setq evil-want-integration t
        evil-want-minibuffer t
        evil-want-fine-undo t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-undo-system 'undo-fu
        evil-search-module 'evil-search
        evil-want-C-i-jump nil)
  :config
  (setq evil-emacs-state-cursor '("#FF5D62" box))
  (setq evil-normal-state-cursor '("#FF5D62" box))
  (setq evil-visual-state-cursor '("#98BB6C" box))
  (setq evil-insert-state-cursor '("#E82424" bar))
  (setq evil-replace-state-cursor '("#FF9E3B" hbar))
  (setq evil-operator-state-cursor '("#7E9CD8" hollow))

  (evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)

  (define-key evil-motion-state-map (kbd "C-M-<left>")  #'(lambda () (interactive) (xref-pop-marker-stack)))
  (define-key evil-motion-state-map (kbd "C-M-<right>") #'(lambda () (interactive) (xref-go-forward)))

  ;; searching
  (define-key evil-motion-state-map (kbd "M-F") #'consult-ag)

  ;; window resizing
  (define-key evil-motion-state-map (kbd "C-+") #'enlarge-window-horizontally)
  (define-key evil-motion-state-map (kbd "C--") #'shrink-window-horizontally)
  (define-key evil-motion-state-map (kbd "C-M-+") #'enlarge-window)
  (define-key evil-motion-state-map (kbd "C-M--") #'shrink-window)

  (define-key evil-motion-state-map (kbd "M-R") #'consult-projectile-recentf)
  (define-key evil-motion-state-map (kbd "M-0") #'treemacs)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-ex-nohighlight)
  (define-key evil-motion-state-map (kbd "<backtab>") #'consult-buffer)
  (define-key evil-motion-state-map (kbd "q") #'exit-minibuffer)
  (define-key evil-insert-state-map (kbd "TAB") #'tab-to-tab-stop)
  (define-key evil-insert-state-map (kbd "<backtab>") #'un-indent-by-removing-4-spaces))

  (add-to-list 'desktop-locals-to-save 'evil-markers-alist)

(use-package evil-multiedit
  :after evil
  :config
  (setq evil-multiedit-follow-matches t)
  (evil-multiedit-default-keybinds))

(use-package evil-collection
  :after evil
  :custom
  (setq evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode 1))

(use-package evil-lion
  :after evil
  :hook (prog-mode . evil-lion-mode))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-quickscope
  :after evil
  :hook (prog-mode . evil-quickscope-mode))

(use-package evil-tutor
  :commands evil-tutor)

(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-pulse t)
  (evil-goggles-use-diff-faces)
  :init
  (evil-goggles-mode))

(define-key global-map [remap quit-window] 'kill-buffer-and-window) ;; remap kill window to kill buffer also
(define-key global-map [remap kill-buffer] 'kill-buffer-and-window) ;; remap kill window to kill buffer also

(global-set-key (kbd "M-/") #'comment-dwim)

;; Theming
(use-package doom-themes
  :after doom-modeline
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
        doom-themes-treemacs-theme "doom-colors")

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons
  :after doom-modeline
  :custom
  (setq all-the-icons-scale-factor 1.1))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package svg-tag-mode
  :hook (org-mode . svg-tag-mode)
  :config
  (setq svg-tag-tags
        '(
          ("DONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))

          ("\\/\\/\\W?TODO\\b:" . ((lambda (tag) (svg-tag-make "TODO" :face 'font-lock-constant-face :inverse t :margin 0 :crop-right t))))
          ("TODO\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-constant-face :crop-left t))))

          ("\\/\\/\\W?MARK\\b:\\|MARK\\b:" . ((lambda (tag) (svg-tag-make "MARK" :face 'font-lock-doc-face :inverse t :margin 0 :crop-right t))))
          ("MARK\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-doc-face :crop-left t))))

          ("\\/\\/\\W?swiftlint:disable" . ((lambda (tag) (svg-tag-make "swiftlint:disable" :face 'org-level-1 :inverse t :margin 0 :crop-right t))))
          ("swiftlint:disable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-level-1 :crop-left t))))

          ("\\/\\/\\W?swiftlint:enable" . ((lambda (tag) (svg-tag-make "swiftlint:enabled" :face 'org-level-2 :inverse t :margin 0 :crop-right t))))
          ("swiftlint:enable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-level-2 :crop-left t))))

          ("\\/\\/\\W?FIXME\\b:\\|FIXME\\b:" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0 :crop-right t))))
          ("FIXME\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :crop-left t))))
          )))


(use-package dimmer
  :hook (prog-mode . dimmer-mode)
  :config
  (dimmer-configure-org)
  (dimmer-configure-magit)
  (dimmer-configure-company-box)
  (dimmer-configure-posframe)
  (dimmer-configure-hydra)
  (setq dimmer-watch-frame-focus-events nil
        dimmer-fraction 0.25)
  (add-to-list 'dimmer-exclusion-regexp-list "^\\**.*\\*$"))

;; rainbow-delimieters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; rainbow-mode show hex as colors
(use-package rainbow-mode
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package paren
  :hook (prog-mode . show-paren-mode)
  :config
  (setq show-paren-delay 0.2
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-ring-bell-on-mismatch t
        show-paren-when-point-in-periphery t))

(use-package tree-sitter
  :hook ((json-mode swift-mode sh-mode) . tree-sitter-hl-mode)
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Remember autocompletions
(use-package amx
  :after vertico
  :config
  (amx-mode 1))

;; ------------------ SEARCHING -------------------
;; the silver searcher
(use-package ag
  :defer t)

(use-package rg
  :defer t)

;; ------------------ EDITING -------------------
;; Navigate through blocks
(use-package consult-project-extra
  :after consult
  :bind
  ("C-<tab>" . #'consult-projectile-switch-to-buffer))

(use-package consult-ls-git
  :after consult)

(use-package google-this
  :commands (google-this)
  :bind ("C-x C-g" . google-this))

(use-package eglot
  :defer t
  :config
  (setq eglot-stay-out-of '(company)
        eglot-autoshutdown t
        eglot-events-buffer-size nil
        eglot-autoreconnect t
        eglot-send-changes-idle-time 0.5))

(use-package eldoc
  :hook (eglot-managed-mode . eldoc-mode))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :config
  (setq eldoc-box-cleanup-interval 0.5
        eldoc-box-clear-with-C-g t))

(use-package company
  :defer t
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("RET" . company-complete-selection)
        ("<return>" . company-complete-selection)
        ("<tab>" . company-complete-selection))
  :config
  (setq company-transformers '(company-sort-by-backend-importance)
        company-format-margin-function  'company-vscode-dark-icons-margin
        company-tooltip-margin              0
        company-dabbrev-downcase            nil
        company-dabbrev-ignore-case         t
        company-dabbrev-other-buffers       t
        company-dabbrev-time-limit          0.5
        company-minimum-prefix-length       1
        company-tooltip-align-annotations   t
        company-require-match               nil
        company-tooltip-limit               25
        company-tooltip-width-grow-only     nil
        company-tooltip-flip-when-above     t
        company-show-quick-access           'left
        company-async-wait                  0.1
        company-async-timeout               1
        company-idle-delay                  0.1
        company-frontends '(company-box-frontend))
  (push '(company-semantic :with company-yasnippet) company-backends))

(use-package company-box
  :after (company all-the-icons)
  :hook (company-mode . company-box-mode)
  :functions (all-the-icons-faicon
              all-the-icons-material
              all-the-icons-octicon
              all-the-icons-alltheicon)
  :config
  (setq company-box-frame-behavior 'point
        company-box-icons-alist 'company-box-icons-images
        company-box-backends-colors t
        company-box-icon-right-margin 0.5
        company-box-backends-colors '((company-yasnippet
                                       :all (:foreground "RosyBrown1" :background nil :italic t)
                                       :selected (:foreground "black" :background "RosyBrown4")))
        company-box-doc-delay 1))

(defun setup-swift-mode-company ()
  "Setup company with separate bakends merged into one."
  (setq-local company-backends
              '((company-capf :with company-files company-dabbrev-code company-yasnippet))))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

(use-package ace-jump-mode
  :commands (ace-jump-mode)
  :bind ("M-g" . ace-jump-mode))

(use-package yasnippet
  :hook (company-mode . yas-minor-mode))

(use-package consult-yasnippet
  :after company)

;; ------------------ FILES -----------------------
(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :bind ("M-J" . treemacs-find-file)
  :init (treemacs-project-follow-mode)
  :config
  (setq treemacs-follow-after-init t
        treemacs-collapse-dirs 1
        treemacs-display-current-project-exclusively t
        treemacs-filewatch-mode t
        treemacs-follow-mode t
        treemacs-git-integration t
        treemacs-git-mode 'extended
        treemacs-indentation 1
        treemacs-is-never-other-window nil
        treemacs-silent-refresh	t
        treemacs-sorting 'alphabetic-case-insensitive-desc
        treemacs-width 40))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package restclient
  :commands (restclient))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :diminish
  :custom
  (flycheck-indication-mode 'left-fringe)
  (flycheck-display-errors-delay 0.2)
  (flycheck-check-syntax-automatically '(save idle-change))
  (flycheck-idle-change-delay 2))

(use-package flycheck-inline
  :hook (flycheck-mode . turn-on-flycheck-inline))

(use-package swift-mode
  :hook (swift-mode . setup-eglot-for-swift)
  :bind
  ("C-c C-c" . #'swift-additions:compile-and-run-silent)
  ("C-c C-x" . #'swift-additions:reset-settings)
  ("C-c C-l" . #'periphery-run-swiftlint)
  ("C-c C-k" . #'periphery-run-loco)
  ("C-c C-t" .  #'swift-additions:test-module-silent)
  ("C-c C-s" .  #'swift-additions:split-func-list)
  ("M-L" .  #'swift-additions:clean-build-folder)
  ("M-P" .  #'swift-additions:print-thing-at-point)
  ("C-M-t" . #'swift-additions:insert-todo)
  ("M-m" . #'swift-additions:insert-mark)
  ("M-s" . #'swift-additions:terminate-all-running-apps)
  :config
  (setq swift-mode:basic-offset 4
        swift-mode:parenthesized-expression-offset 4)
  (setq-local indent-tabs-mode t))

(use-package markdown-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package clean-aindent-mode
  :hook (prog-mode . clean-aindent-mode)
  :config
  (setq clean-aindent-is-simple-indent t)
  (define-key global-map (kbd "RET") 'newline-and-indent))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :bind ("M-O" . projectile-find-file-dwim)  
  :custom
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-sort-order 'access-time
        projectile-indexing-method 'alien
        projectile-switch-project-action #'projectile-find-file-dwim
        projectile-ignored-files '(".orig$" ".yml$"))
  (add-to-list 'projectile-globally-ignored-directories '(("^\\.build$")
                                                          ("^\\.swiftpm$") 
                                                          ("^\\.swiftpm$")
                                                          ("^\\elpa$")
                                                          ("^\\xcodeproj$")
                                                          ("^\\pods$"))))
;; Restart emacs
(use-package restart-emacs
  :commands restart-emacs)

;; hydra
(use-package hydra
  :defer t)

(use-package pretty-hydra
  :after hydra
  :config
  (setq major-mode-hydra-title-generator
        '(lambda (mode)
           (s-concat (all-the-icons-icon-for-mode mode :v-adjust 0.0 :height 2.4)))))

(use-package window
  :ensure nil
  :bind
  ("C-x C-f" . toggle-frame-fullscreen)
  :custom
  (display-buffer-alist
   '(("*xwidget*"
      (display-buffer-in-side-window display-buffer-reuse-mode-window display-buffer-reuse-window)
      (body-function . select-window)
      (window-width . 0.3)
      (side . right))
     ("\\*occur\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.18)
      (side . bottom)
      (slot . 1))
     ("\\*xcodebuild\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.2)
      (side . bottom)
      (slot . 0))
     ("\\*Periphery\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.3)
      (side . bottom)
      (slot . 1))
     ("\\*Faces\\|[Hh]elp\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-width . 0.4)
      (side . right)
      (slot . 1))
     ("\\*e?shell\\|vterm*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.18)
      (side . bottom)
      (slot . -1))
     ("simulator logs\\|Flycheck errors\\|Async Shell Command\\|[Cc]olors\\*\\|Warnings"
      (display-buffer-in-side-window)
      (window-height . 0.18)
      (side . bottom)
      (slot . 2)))))

;; Winum - select windows easy ---------------------------
(use-package winum
  :after doom-modeline
  :bind
  ("M-1" . winum-select-window-1)
  ("M-2" . winum-select-window-2)
  ("M-3" . winum-select-window-3)
  ("M-4" . winum-select-window-4)
  ("M-5" . winum-select-window-5)
  ("M-6" . winum-select-window-6)
  :init
  (winum-mode 1))

;; darkroom (go to focus mode)
(use-package darkroom
  :commands darkroom-mode
  :bind ("C-x C-d" . darkroom-tentative-mode)
  :config
  (setq darkroom-text-scale-increase 1
        darkroom-margins 0.1))

;; Use git
(use-package magit
  :commands (magit-status magit-ediff-show-working-tree)
  :bind ("C-c C-d" . magit-ediff-show-working-tree)
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 4
        magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*"))
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))))

(use-package blamer
  :commands (blamer-mode)
  :config
  (setq blamer-view 'overlay
        blamer-type 'posframe-popup
        blamer-max-commit-message-length 70
        blamer-force-truncate-long-line nil
        blamer-author-formatter " ✎ [%s] - "
        blamer-commit-formatter "● %s ● ")
  :custom
  (blamer-idle-time 1.0)
  :custom-face
  (blamer-face ((t :foreground "#E46876"
                   :height 140
                   :italic t))))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :config
  (setq git-gutter:update-interval 0.5))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated)))

(use-package vterm
  :commands vterm)

;; general
(use-package general
  :config
  (general-create-definer mk/leader-keys
    :keymaps '(normal insert emacs visual operator hybrid xwidget-webkit)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (mk/leader-keys
    "TAB" '((lambda () (interactive) (switch-to-buffer nil)) :which-key "Toggle buffers")
    "SPC" '(execute-extended-command :which-key "M-x")
    "s" '(consult-line-multi :which-key "Consult multi search")
    "0" '(treemacs-select-window :which-key "Treemacs")
    "1" '(winum-select-window-1 :which-key "Window 1")
    "2" '(winum-select-window-2 :which-key "Window 2")
    "3" '(winum-select-window-3 :which-key "Window 3")
    "4" '(winum-select-window-4 :which-key "Window 4")
    "5" '(winum-select-window-5 :which-key "Window 5")
    "6" '(winum-select-window-6 :which-key "Window 6")
    "P" 'package-install
    "'" '((lambda () (interactive) (vterm)) :which-key "Term"))

  (mk/leader-keys
    "bm" '(lambda () (interactive) (switch-to-buffer "*Messages*") :which-key "Message buffer")
    "bs" '(lambda () (interactive) (switch-to-buffer "*scratch*") :which-key "Scratch buffer"))

  (mk/leader-keys
    "ee" '(eval-expression :which-key "Eval expression")
    "eb" '(eval-buffer :which-key "Eval buffer")
    "el" '(eval-last-sexp :which-key "Eval before point")
    "er" '(eval-region :which-key "Eval region"))

  (mk/leader-keys
    "fs" '(save-buffer :which-key "Save file")
    "ff" '(find-file :which-key "Find file")
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
    "wx" '(delete-window :which-key "Delete window"))

  (mk/leader-keys
    "pf" '(projectile-find-file-dwim :which-key "Find file")
    "pF" '(projectile-project-files :which-key "Project files")
    "pk" '(projectile-kill-buffers :which-key "Kill buffers")
    "ps" '(projectile-switch-project :which-key "Switch project")
    "pS" '(projectile-switch-open-project :which-key "Switch open project"))

  (mk/leader-keys
    "vs" '(magit-status :which-key "Status")
    "vb" '(blamer-show-commit-info :which-key "Show git blame")
    "vd" '(magit-diff-buffer-file :which-key "Diff current buffer")
    "vw" '(magit-diff-working-tree :which-key "Diff working tree"))

  (mk/leader-keys
    "qq" '(save-buffers-kill-terminal :which-key "Quit emacs")
    "qr" '(restart-emacs :which-key "Restart emacs")))


(use-package org
  :hook (org-mode . mk/org-mode-setup)
  :config
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-log-into-drawer t
        org-log-done 'time))

(with-eval-after-load 'org
  (setq org-confirm-babel-evaluate nil)
  (require 'org-tempo)

  ;; Setup fonts for org-mode
  (set-face-attribute 'org-block nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))
  ;; (org-babel-do-load-languages 'org-babel-load-languages
  ;;                               '((emacs-lisp t))

  ;; (add-to-list 'org-structure-template-alist
  ;;              '("sh" . "src shell")
  ;;               ("elisp" . "src emacs-lisp")
  ;;               ("swift" . "src swift"))
  (add-to-list 'org-modules 'org-tempo t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill-column
  :hook (org-mode . mk/org-mode-visual-fill))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds '(
                       ("https://news.ycombinator.com/rss")
                       ("https://www.reddit.com/r/emacs.rss")
                       ("https://xenodium.com/rss")
                       ("https://swiftbysundell.com/rss"))
        elfeed-search-filter "@7-days-ago +unread"
        elfeed-search-title-max-width 100
        elfeed-search-title-min-width 100))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method #'bitmap))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

;; Drag lines and regions around
(use-package drag-stuff
  :hook (prog-mode . drag-stuff-mode)
  :bind
  ("S-M-<down>" . drag-stuff-down)
  ("M-S-<up>" . drag-stuff-up))

;; Quickly jump to definition or usage
(use-package dumb-jump
  :hook (prog-mode . dumb-jump-mode)
  :config
  (put 'dumb-jump-go 'byte-obsolete-info nil)
  (define-key evil-motion-state-map [remap evil-goto-definition] #'dumb-jump-go)
  (setq dumb-jump-window 'current)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package localizeable-mode
  :mode "\\.strings\\'"
  :ensure nil
  :load-path "~/.emacs.d/localpackages/localizeable-mode.el")

(use-package smartparens
  :config
  (sp-local-pair 'swift-mode "\\(" nil :actions nil)
  (sp-local-pair 'swift-mode "\\(" ")")
  (sp-local-pair 'swift-mode "<" ">"))

(defun setup-eglot-for-swift ()
    "Setup eglot for swift buffers."
    (eglot-ensure)
    (when (boundp 'eglot-server-programs)
      (add-to-list 'eglot-server-programs
                   '(swift-mode . my-swift-mode:eglot-server-contact)))
    (setup-swift-mode-company))

(defun setup-swift-programming ()
  "Custom setting for swift programming."

  (setq tree-sitter-hl-use-font-lock-keywords t)
  (load "swift-additions")
  (load "periphery-swiftlint")
  (load "periphery-loco")

  (use-package flycheck-swift3
    :after flycheck
    :custom (flycheck-swift3-setup))

  (use-package flycheck-swiftlint
    :after flycheck
    :custom (flycheck-swiftlint-setup))

  (add-to-list 'flycheck-checkers 'swift3)
  (add-to-list 'flycheck-checkers 'swiftlint)

  (flycheck-add-next-checker 'swiftlint 'swift3))

(defun mk/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun mk/org-mode-setup()
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode t))

;;; esc quits
(defun mk/browser-split-window (url &optional new-window)
  "Create a new browser window to the right of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p)))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (xwidget-webkit-browse-url url)))

;; Setup Functions
(defun mk/setupProgrammingSettings ()
  "Programming mode."
  (local-set-key (kbd "C-c C-f") #'periphery-search-thing-at-point-rg)

  ;; Drag stuff
  (local-set-key (kbd "M-+") #'mk/toggle-flycheck-errors)
  (local-set-key (kbd "M-B") #'consult-projectile-switch-to-buffer)
  (local-set-key (kbd "C-M-B") #'projectile-switch-to-buffer-other-window)

  (hs-minor-mode)       ; Add support for folding code blocks
  (yas-global-mode 1)   ; Load our yassnippets
  (electric-pair-mode)  ; Auto insert pairs {} () [] etc
  (global-hl-todo-mode t)

  (setq highlight-indent-guides-mode t    ;; Turn on indent-guides
        indicate-empty-lines t            ;; Show empty lines
        indicate-unused-lines t           ;; Show unused lines
        show-trailing-whitespace nil      ;; Show or hide trailing whitespaces
        column-number-mode nil            ;; Show current line number highlighted
        display-line-numbers 'relative))  ;; Show line numbers

(defun mk/setup-flycheck ()
  "Setup margins for flycheck."
  (setq left-fringe-width 12 right-fringe-width 12
        left-margin-width 1 right-margin-width 0)
  (flycheck-refresh-fringes-and-margins))

(add-hook 'flycheck-mode-hook #'mk/setup-flycheck)
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)

(defun mk/toggle-flycheck-errors ()
  "Function to toggle flycheck errors."
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
    (list-flycheck-errors)))

(defun mk/setupOrgMode ()
  "My org config."
  (setq highlight-indent-guides-mode nil))

(defun mk/split-window-below ()
  "Split window below and select that."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun mk/split-window-right ()
  "Split window to the right and select window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun un-indent-by-removing-4-spaces ()
  "Remove 4 spaces from beginning of of line."
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

(add-hook 'org-mode-hook #'mk/setupOrgMode)
(add-hook 'prog-mode-hook #'mk/setupProgrammingSettings)

(with-eval-after-load 'swift-mode
  (load "periphery-search")
  (setup-swift-programming))

(setq gc-cons-threshold (* 2 1024 1024))

(advice-add 'eglot-xref-backend :override 'xref-eglot+dumb-backend)

(defun xref-eglot+dumb-backend () 'eglot+dumb)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot+dumb)))
  (cons (xref-backend-identifier-at-point 'eglot)
        (xref-backend-identifier-at-point 'dumb-jump)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot+dumb)))
  (xref-backend-identifier-completion-table 'eglot))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot+dumb)) identifier)
  (or (xref-backend-definitions 'eglot (car identifier))
      (xref-backend-definitions 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-references ((_backend (eql eglot+dumb)) identifier)
  (or (xref-backend-references 'eglot (car identifier))
      (xref-backend-references 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot+dumb)) pattern)
  (xref-backend-apropos 'eglot pattern))

(provide 'init)

;;; init.el ends here
