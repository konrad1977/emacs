;;; init.el --- -*- lexical-binding: t -*-
;;; Code:

;; Window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(eval-when-compile (defvar display-time-24hr-format))
(eval-when-compile (defvar display-time-default-load-average))

(setq byte-compile-warnings '(cl-functions)) ;; Turn off cl package is depricated warning

; (setq split-width-threshold t)        ;; Always split new buffers below
(setq auto-mode-case-fold nil)
(setq ad-redefinition-action            'accept
	  create-lockfiles                  nil
      compilation-scroll-output         t
      display-time-24hr-format          t
      display-time-default-load-average nil
      visible-bell                      nil
      backup-by-copying                 t
	  initial-scratch-message           ""
	  idle-update-delay                 1.0     ;; Speed things up by not updating so often
	  blink-cursor-interval             0.6		;; Little slower cursor blinking . default is 0.5
      echo-keystrokes                   0.1
	  fast-but-imprecise-scrolling      t
      confirm-kill-processes            nil
      ediff-split-window-function       'split-window-horizontally
	  read-process-output-max           (* 8 1024 1024)
      backup-directory-alist            '(("." . "~/.emacs.d/backups")))

(setq gc-cons-threshold (eval-when-compile (* 20 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

;; Helpout better with debugging
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(display-battery-mode t)		;; Show battery.
(display-time-mode t)			;; Show time.
(scroll-bar-mode -1)			;; Dont use scrollbars.
(set-fringe-mode 4)				;; Give us some space.
(tooltip-mode -1)				;; Disable tooltip.
(delete-selection-mode t)		;; Use a more sane delete mode than evil.
(fset 'yes-or-no-p 'y-or-n-p)	;; Set yes or no to y/n
(global-font-lock-mode 1)		;; always highlight code
(global-auto-revert-mode 1)		;; refresh a buffer if changed on disk
(desktop-save-mode 0)			;; Save desktop
(recentf-mode)					;; Recent file mode.
(savehist-mode 1)				;; Save history
(global-hl-line-mode 1)         ;; Highlight current line
(semantic-mode 1)               ;; help out with semantics
(save-place-mode 1)             ;; when buffer is closed, save the cursor position

(setq-default display-line-numbers-width    4            ;; Set so we can display thousands of lines
			  c-basic-offset                4            ;; Set tab indent for c/c++ to 4 tabs
			  tab-width                     4            ;: Use four tabs
			  line-spacing                  0.05         ;; Increase linespacing a bit
			  truncate-lines                10			 ;; Truncate lines
			  indent-tabs-mode              nil			 ;; Never use tabs. Use spaces instead
			  completion-ignore-case        t            ;; Ignore case when completing
              indent-line-function          'insert-tab  ;; Use function to insert tabs
              history-length                500)

(let* ((path (expand-file-name "localpackages" user-emacs-directory))
       (local-pkgs (mapcar 'file-name-directory (directory-files-recursively path ".*\\.el"))))
  (if (file-accessible-directory-p path)
      (mapc (apply-partially 'add-to-list 'load-path) local-pkgs)
    (make-directory path :parents)))

(eval-when-compile (defvar savehist-additional-variables))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
;(add-to-list 'load-path (concat user-emacs-directory "localpackages/"))
(add-to-list 'savehist-additional-variables 'kill-ring)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Dont leave #file autosaves everywhere I go
(defvar my-auto-save-folder (concat user-emacs-directory "var/auto-save/"))
(setq auto-save-list-file-prefix (concat my-auto-save-folder ".saves-")); set prefix for auto-saves
(setq auto-save-file-name-transforms `((".*", my-auto-save-folder t))); location for all auto-save files
(setq custom-file (concat user-emacs-directory "var/custom.el"))

;; Setup fonts
(set-face-attribute 'default nil :font "Source Code Pro" :height 156)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 156)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 156 :weight 'regular)

;; Initialize package sources
(require 'package)
(setq package-archives '(
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

;; Make sure we are up to date, atleast once a week
(use-package auto-package-update
  :custom
  (setq auto-package-update-interval 7
		auto-package-update-prompt-before-update t
		auto-package-update-hide-results nil))

(use-package no-littering)	;; Clean up all those temporary files
(use-package gcmh
  :init (gcmh-mode 1))		;; Better garbage collection seetings

(use-package autothemer
  :custom (setq custom-safe-themes t))

(load-theme 'kanagawa t)

;;  theming
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (make-local-variable 'face-remapping-alist)
            (add-to-list 'face-remapping-alist '(default (:background "#15121C")))))

(use-package centaur-tabs
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  (swift-mode . centaur-tabs-local-mode)
  :config
  (setq centaur-tabs-style "bar"
		centaur-tabs-height 28
		centaur-tabs-set-modified-marker t
		centaur-tabs-show-navigation-buttons nil
		centaur-tabs-plain-icons nil
		centaur-tabs-set-icons t
		centaur-tabs-set-bar 'left
		x-underline-at-descent-line t
		uniquify-buffer-name-style 'forward)
  (centaur-tabs-mode t)
  :bind  
  (:map evil-normal-state-map
	     ("g t" . centaur-tabs-forward)
	     ("g T" . centaur-tabs-backward)))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-startup-banner (concat user-emacs-directory "themes/catppuccin.png")
		dashboard-path-style 'truncate-beginning
		dashboard-banner-logo-title "Mikaels dashboard!"
		dashboard-set-file-icons t
		dashboard-set-init-info t
		dashboard-center-content t
		dashboard-set-heading-icons t
		dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
		dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-time
		dashboard-week-agenda t
		dashboard-items '(
						  (projects . 5)
						  (agenda)
						  (recents . 3)
						  )))

;; Which key
(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-prefix-prefix "◉ ")
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.3
		which-key-min-display-lines 4
		which-key-max-display-columns 5))

(use-package discover-my-major
  :commands discover-my-major)

 ; helpful
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ace-window
   :bind ("C-x C-o" . ace-window))

(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

; Use evil mode
(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (setq evil-emacs-state-cursor '("#A3D4D5" box)) 
  (setq evil-normal-state-cursor '("#A3D4D5" box)) 
  (setq evil-visual-state-cursor '("#7FB4CA" box))
  (setq evil-insert-state-cursor '("#FF9E3B" bar))
  (setq evil-replace-state-cursor '("red" hbar))
  (setq evil-operator-state-cursor '("red" hollow))
  
  (global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
  
  (define-key evil-motion-state-map (kbd "M-u") #'evil-undo)
  (define-key evil-motion-state-map (kbd "M-U") #'evil-redo)

  (define-key evil-motion-state-map (kbd "M-0") #'treemacs)
  (define-key evil-motion-state-map (kbd "q") #'exit-minibuffer)
  (define-key evil-motion-state-map (kbd "C-f") #'deadgrep)
  (define-key evil-motion-state-map "/" 'swiper))

(use-package evil-tutor
  :commands evil-tutor)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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

;; (use-package fira-code-mode
;;   :hook (prog-mode . fira-code-mode))

(use-package all-the-icons
  :after doom-modeline)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Config and install modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq
		doom-modeline-bar-width 2
		doom-modeline-buffer-file-name-style 'file-name
		doom-modeline-buffer-encoding nil
		doom-modeline-icon t
		doom-modeline-indent-info nil
		doom-modeline-major-mode-color-icon t
		doom-modeline-major-mode-icon t
		doom-modeline-modal-icon t
		doom-modeline-checker-simple-format t
		doom-modeline-env-version nil
		doom-modeline-hud nil
        doom-modeline-vcs-max-length 40
		doom-modeline-height 24)
  :custom
  (set-face-attribute 'mode-line nil
					  :family "Iosevka Nerd Font Mono"
					  :height 142
					  :box '(:line-width 1 :color "#0C0A10"))
  (set-face-attribute 'mode-line-inactive nil
					  :family "Iosevka Nerd Font Mono"
					  :height 132
					  :box '(:line-width 1 :color "#332E41")))

;; nyan cat
(use-package nyan-mode
  :hook (doom-modeline-mode . nyan-mode)
  :config
  (setq nyan-animate-nyancat t))

;; (use-package beacon
;;   :init (beacon-mode 1))

;; rainbow-delimieters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; rainbow-mode show hex as colors
(use-package rainbow-mode
  :commands rainbow-mode)

(use-package paren
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-ring-bell-on-mismatch t
        show-paren-when-point-in-periphery t)
  (show-paren-mode t))

;; Use ivy
(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-height 10
		ivy-use-virtual-buffers t
		ivy-count-format "(%d/%d) "
		ivy-use-selectable-prompt t
		ivy-display-style 'fancy)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-mode-map       (kbd "<escape>") nil)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit))

;; ;; Ivy rich
(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :custom
  (setq ivy-virtual-abbreviate 'abbreviate
		ivy-rich-switch-buffer-align-virtual-buffer nil
		ivy-rich-path-style 'full))

;; counsel
(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-rg-base-command
      "rg -i -M 120 --no-heading --line-number %s ."))

;; Remember autocompletions
(use-package amx
  :after ivy
  :config
  (amx-mode 1))

(use-package request
  :commands counsel-search)

;; Search files, and do it with speed and style
(use-package swiper
  :after ivy
  :config
  (setq swiper-goto-start-of-match t))

;; ------------------ SEARCHING -------------------
(use-package deadgrep
  :commands deadgrep)

;; the silver searcher
(use-package ag
  :defer t)

;; ------------------ EDITING -------------------
;; - anzu search and replace
(use-package anzu
  :hook (after-init . anzu-mode))

(use-package cycle-at-point
  :bind ("M-n" . cycle-at-point))

;; Multiple cursors evil mode
(use-package evil-multiedit
  :config (evil-multiedit-default-keybinds))

;; ------------------ AUTOCOMPLETIONS -------------
;; workaround for company-transformers
(setq company-tabnine--disable-next-transform nil)
(defun my-company--transform-candidates (func &rest args)
  (if (not company-tabnine--disable-next-transform)
      (apply func args)
    (setq company-tabnine--disable-next-transform nil)
    (car args)))

(defun my-company-tabnine (func &rest args)
  (when (eq (car args) 'candidates)
    (setq company-tabnine--disable-next-transform t))
  (apply func args))

(advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
(advice-add #'company-tabnine :around #'my-company-tabnine)

(use-package company
  :hook (prog-mode . company-mode)
  :init
  ;(setq company-format-margin-function      'company-vscode-dark-icons-margin)
  (setq company-format-margin-function      'company-dot-icons-margin)
  (setq company-dot-icons-format            " ● ")
  (setq company-backends                    '(
                                              company-tabnine
                                              company-sourcekit
                                              company-capf
                                              company-yasnippet
                                              company-dabbrev-code
                                              company-semantic
                                              company-keywords
                                              company-files
                                              )
        company-frontends                   '(company-pseudo-tooltip-frontend)
        company-tooltip-margin              2
        company-minimum-prefix-length       2
        company-tooltip-align-annotations   t
        company-search-regexp-function      'company-search-flex-regexp
        company-require-match               nil 
        company-tooltip-limit               20
        company-tooltip-flip-when-above     t
        company-tooltip-idle-delay          0.2
        company-async-wait                  0.4
        company-idle-delay                  0
        company-show-quick-access           'left
        company-async-timeout               2))

(use-package company-sourcekit
  :hook swift-mode
  :config
  (setq sourcekit-sourcekittendaemon-executable "/usr/local/bin/sourcekittend")
  :custom
  (setq company-sourcekit-verbose nil
		sourcekit-verbose nil
        company-sourcekit-use-yasnippet t))

(use-package company-tabnine
  :config
  (setq company-tabnine-max-num-results 9))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

(use-package ace-jump-mode
  :bind ("M-g" . ace-jump-mode))

(use-package yasnippet
  :hook (company-mode . yas-minor-mode))
;; (use-package yasnippet-snippets
;;   :after yasnippet)

;; ------------------ FILES -----------------------
(use-package treemacs
  :config
  (setq treemacs-follow-after-init t
		treemacs-project-follow-mode t
		treemacs-follow-mode t
		treemacs-filewatch-mode t		treemacs-fringe-indicator-mode 'always
        treemacs-width 40
        treemacs-indentation 1
        treemacs-git-integration t
        treemacs-collapse-dirs 0
        treemacs-silent-refresh	t
		treemacs-change-root-without-asking t
        treemacs-sorting 'alphabetic-case-insensitive-desc
        treemacs-show-hidden-files nil
        treemacs-never-persist nil
        treemacs-is-never-other-window nil
		treemacs-displacurrent-project-exclusively t
        treemacs-goto-tag-strategy 'refetch-index
		treemacs-text-scale	0)
  (treemacs-follow-mode)
  (treemacs-project-follow-mode))

(use-package treemacs-projectile
  :hook (treemacs-mode-hook))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :diminish
  :custom
  (flycheck-indication-mode 'left-fringe)
  (flycheck-display-errors-delay 0.1)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 0.8))

(use-package flycheck-inline
  :ensure t
  :hook (flycheck-mode . turn-on-flycheck-inline))

(defun my/set-flycheck-margins ()
  (setq left-fringe-width 8 right-fringe-width 8
        left-margin-width 1 right-margin-width 0)
  (flycheck-refresh-fringes-and-margins))
;; …every time Flycheck is activated in a new buffer
(add-hook 'flycheck-mode-hook #'my/set-flycheck-margins)

(defun exec-path-from-shell-setup ()
     (when (memq window-system '(mac ns x))
       (exec-path-from-shell-initialize)))

(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-setup))

(defun setup-xcode-menus ()
"Setup menus for use of Xcode."
  (defun xcode-build()
    "Start a build using Xcode."
	(interactive)
	(shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))
  (defun xcode-run()
    "Run application from Xcode."
	(interactive)
	(shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))
  (defun xcode-test()
    "Run current test scheme from Xcode."
	(interactive)
	(shell-command-to-string
	 "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'")))

(defun mk-sourcekit-lsp-command ()
    "Setup lsp."
    (interactive)
  (append (list (mk-sourcekit-lsp-executable)) mk-sourcekit-lsp-options))

(defun setup-swift-programming ()
  "Setup swift development environment."
  (use-package swift-mode
    :custom
    (setq swift-mode:parenthesized-expression-offset 4
		  swift-mode:multiline-statement-offset 4)) 

   ;(require 'ios-simulator)
                                        ;(load "ios-simulator")
  (require 'swift-additions)
  (load "swift-additions")
   
  (use-package swift-helpful
	:after swift-mode)
  
  (use-package flycheck-swiftx
    :after flycheck)

  (use-package flycheck-swift3
	:after flycheck
    :custom (flycheck-swift3-setup))

  (use-package flycheck-xcode
    :after flycheck
    :custom (flycheck-xcode-setup))
   
  (use-package flycheck-swiftlint
    :after flycheck
    :custom (flycheck-swiftlint-setup))

  (require 'flycheck)
  
 ; (add-to-list 'flycheck-checkers 'xcode)
 ; (add-to-list 'flycheck-checkers 'swiftx)
  (add-to-list 'flycheck-checkers 'swift3)
  (add-to-list 'flycheck-checkers 'swiftlint)

  (flycheck-add-next-checker 'swiftlint 'swift3))
 ; (flycheck-add-next-checker 'swift3 'xcode)
 ; (flycheck-add-next-checker 'xcode 'swiftx))

; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)

  (setup-xcode-menus)
  (setup-swift-programming)

  ;; Use existing frame when opening files.
  (setq ns-pop-up-frames nil)
  
    (defun macos-hardware-overview ()
      "View macOS hardware overview."
      (interactive)
      (shell-command "system_profiler SPHardwareDataType"))
  
  (use-package ns-auto-titlebar
	:config (ns-auto-titlebar-mode))

  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  (setq org-agenda-files '("~/Library/Mobile Documents/com~apple~CloudDocs/orgfiles/"))
  (setq mac-option-key-is-meta nil
		mac-command-key-is-meta t
		mac-command-modifier 'meta
		mac-option-modifier 'none
		dired-use-ls-dired nil
		frame-title-format ""))

(defun my-vterm/split-horizontal ()
  "Create a new vterm window under of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p)))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (vterm default-directory)))

(defun mk/browser-split-vertically ()
  "Create a new browser window to the right of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p)))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (xwidget-webkit-browse-url "https://duckduckgo.com")))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :diminish projectile-mode
  :config (projectile-mode)
  :custom
    (projectile-project-root-files-functions
     '(projectile-root-local
       projectile-root-top-down
       projectile-root-bottom-up
       projectile-root-top-down-recurring))
	(setq projectile-completion-system 'ivy
		  projectile-enable-caching t
		  projectile-sort-order 'recentf
		  projectile-indexing-method 'alien)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/git")
    (setq projectile-project-search-path '("~/Documents/git")))
  (setq projectile-switch-project-action #'projectile-find-file)
  (setq projectile-ignored-files '(".xcodeproj" ".m" ".h" ".pbxproj" ".orig")))

;; counsel-projectile
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode)
  (setq counsel-projectile-ag-use-gitignore-only nil))

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

(use-package major-mode-hydra
  :defer t
  :config
  (setq major-mode-hydra-invisible-quit-key "q"))

;; ;; Winum - select windows easy
(use-package winum
  :after doom-modeline
  :init
  (winum-mode 1))

;; darkroom (go to focus mode)
(use-package darkroom
  :commands darkroom)

;; Use git
(use-package magit
  :commands magit-status
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package blamer
  :hook (prog-mode . global-blamer-mode)
  :config
  (setq blamer-view 'overlay-right)
  (setq blamer-type 'both)
  (setq blamer--overlay-popup-position 'smart)
  (setq blamer-max-commit-message-length 120)
  (setq blamer-author-formatter " ✎ %s ")
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t))))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))

(custom-set-variables
 '(git-gutter:modified-sign " ≈ ") ;; two space
 '(git-gutter:added-sign " + ")    ;; multiple character is OK
 '(git-gutter:deleted-sign " - "))

(use-package forge
  :commands forge-pull)

(use-package vterm
  :commands vterm)

 ;; general
(use-package general
  :defer t
  :config
  (general-create-definer mk/leader-keys
	:keymaps '(normal insert emacs visual operator hybrid xwidget-webkit)
	:prefix "SPC"
	:non-normal-prefix "M-SPC")

  (mk/leader-keys
	"TAB" '((lambda () (interactive) (switch-to-buffer nil)) :which-key "Toggle buffers")
	"SPC" '(execute-extended-command :which-key "M-x")
	"s" '(swiper-thing-at-point :which-key "Swiper thing at point")
	"0" '(treemacs-select-window :which-key "Treemacs")
	"1" '(winum-select-window-1 :which-key "Window 1")
	"2" '(winum-select-window-2 :which-key "Window 2")
	"3" '(winum-select-window-3 :which-key "Window 3")
	"4" '(winum-select-window-4 :which-key "Window 4")
	"5" '(winum-select-window-5 :which-key "Window 5")
	"6" '(winum-select-window-6 :which-key "Window 6")
	"P" 'package-install
	"'" '((lambda () (interactive) (my-vterm/split-horizontal)) :which-key "Term")
	"!" 'shell-command
	":" 'eval-expression)

  (mk/leader-keys
	"a" '(:ignore t :which-key "Agenda")
   	"aa" '(org-agenda :which-key "Show agenda")
   	"as" '(org-agenda-schedule :which-key "Show schedule")
	"al" '(org-agenda-list :which-key "Show agenda list")
	"aF" '(org-agenda-file-to-front :which-key "Bring file to front")
	"at" '(:ignore t:which-key "Time/date")
	"att" '(org-time-stamp :which-key "Schedule")
	"atd" '(org-deadline :which-key "Add deadline"))

  (mk/leader-keys
	"A" '(:ignore t :which-key "Applications")
	"Af" '(elfeed-hydra/body :which-key "Feed")
	"As" '(sx-hydra/body :which-key "Stackoverflow"))

  (mk/leader-keys
    "b" '(:ignore t :which-key "Buffer")
    "bb" '(counsel-switch-buffer :which-key "List buffers")
    "bx" '(evil-delete-buffer :which-key "Delete buffer")
    "bk" '((lambda () (interactive) (kill-other-buffers)) :which-key "Kill other buffers")
    "bd" '(kill-current-buffer :which-key "Kill current buffer")
    "bp" '(previous-buffer :which-key "Previous buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "bC" '((lambda () (interactive) (switch-to-buffer "*Compile-Log*")) :which-key "Compile log-buffer")
    "bD" '((lambda () (interactive) (switch-to-buffer "*dashboard*")) :which-key "Sashboard-buffer")
    "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "Messages-buffer")
    "bs" '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "Scratch-buffer"))

  (mk/leader-keys
	"m" '(major-mode-hydra :which-key "Major mode"))

  (mk/leader-keys
    "c" '(:ignore t :which-key "Code")
	"ce" '(:ignore t :which-key "Error")
    "cee" '(counsel-flycheck :which-key "Counsel errors")
    "cel" '(flycheck-list-errors :which-key "List errors")
    "cp" 'check-parens
    "co" 'projectile-find-other-file
    "cl" '(comment-line :which-key "Comment line")
    "cr" '(comment-region :which-key "Comment region")
    "cu" '(lsp-ui-imenu :which-key "Lsp-ui-menu")
    "ct" '(lsp-treemacs-symbols :which-key "Treemacs symbols")
    "cf" '(dumb-jump-hydra/body :which-key "Go to definition"))

  (mk/leader-keys
	"e" '(:ignore t :which-key "Eval")
	"ee" '(eval-expression :which-key "Eval expression")
	"eb" '(eval-buffer :which-key "Eval buffer")
	"el" '(eval-last-sexp :which-key "Eval before point")
	"er" '(eval-region :which-key "Eval region"))

  (mk/leader-keys
	"f" '(:ignore t :which-key "Files")
	"fs" '(save-buffer :which-key "Save file")
	"fo" '(dired :which-key "Open file")
	"ff" '(counsel-find-file :which-key "Find file")
	"fr" '(counsel-recentf :which-key "Recent files")
	"fn" '(create-file-buffer :which-key "New file")
	"fR" '(dired-rename-file :which-key "Rename file")
	"fD" '(delete-file :which-key "Delete file")
	"fe" '(lambda () (interactive) (find-file user-init-file) :which-key "User configuration"))

  (mk/leader-keys
    "h" '(:ignore t :which-key "Help")
    "hc" '(helpful-command :which-key "Describe command")
    "hk" '(helpful-key :which-key "Describe key")
    "hf" '(helpful-function :which-key "Describe function")
    "hv" '(helpful-variable :which-key "Describe variable")
    "ht" '(evil-tutor-start :which-key "Evil tutorial")
    "h." '(helpful-at-point :which-key "Describe at-point")
    "hp" '(describe-package :which-key "Describe package"))

  (mk/leader-keys
    "t" '(:ignore t :which-key "Text")
    "tr" '(anzu-query-replace-at-cursor-thing :which-key "Replace text")
    "ts" '(sort-lines :which-key "Sort lines")
	"tS" '(hydra-text-scale/body :which-key "Scale text")
    "tx" '(delete-trailing-whitespace :which-key "Delete trailing whitespace")
    "tw" '(mark-word :which-key "Select word")
    "tp" '(mark-page :which-key "Select page"))

   (mk/leader-keys
     "w" '(:ignore t :which-key "Windows")
	 "wb" '((lambda () (interactive) (mk/browser-split-vertically)) :which-key "Start a browser")
     "wp" '(previous-window-any-frame :which-key "Previous window")
     "wx" '(delete-window :which-key "Delete window")
	 "wk" '(delete-window-internal :which-key "Delete window")
	 "wr" '(evil-window-rotate-upwards :which-key "Rotate clockwise")
	 "wR" '(evil-window-rotate-downwards :which-key "Rotate counter clockwise")
	 "w-" '(mk/split-window-below :which-key "Split window horizontally")
	 "w/" '(mk/split-window-right :which-key "Split window vertically")
	 "ww" '(hydra-windows-setup/body :which-key "Hydra menu")
     "wn" '(next-window-any-frame :which-key "Next window"))

   (mk/leader-keys
     "p" '(:ignore t :which-key "Project")
     "pp" '(:ignore t :which-key "Project management")
     "ppa" '(treemacs-add-project-to-workspace :which-key "Add project")
     "ppr" '(treemacs-remove-project-from-workspace :which-key "Remove project")
     "pf" '(projectile-find-file-dwim :which-key "Find file")
     "pt" '(counsel-projectile-ag :which-key "Find tag")
     "pF" '(projectile-project-files :which-key "Project files")
     "pk" '(projectile-kill-buffers :which-key "Kill buffers")
     "ps" '(projectile-switch-project :which-key "Switch project")
     "pS" '(projectile-switch-open-project :which-key "Switch open project"))

   (mk/leader-keys
     "v" '(:ignore t :which-key "Version control")
     "vs" '(magit-status :which-key "Status")
     "vb" '(blamer-show-commit-info :which-key "Show git blame")
     "vd" '(magit-diff-buffer-file :which-key "Diff current buffer")
     "vw" '(magit-diff-working-tree :which-key "Diff working tree"))

   (mk/leader-keys
     "g" '(:ignore t :which-key "Games")
     "gt" '(tetris :which-key "Tetris")
     "gh" '(hanoi :which-key "Tower of hanoi"))

   (mk/leader-keys
     "T" '(:ignore t :which-key "Tabs")
     "Tn" '(tab-new :which-key "New")
     "Tl" '(tab-list :which-key "List")
     "Tg" '(tab-close-group :which-key "Close group")
     "Td" '(tab-detach :which-key "Detach")
     "Tx" '(tab-close :which-key "Close")
	 "Tk" '(tab-close-other :which-key "Close other"))

  (mk/leader-keys
    "q" '(:ignore t :which-key "Quit")
    "qq" '(save-buffers-kill-terminal :which-key "Quit emacs")
    "qr" '(restart-emacs :which-key "Restart emacs")))


(defun mk/org-mode-setup()
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . mk/org-mode-setup)
  :config
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-hide-leading-stars t
	org-agenda-start-with-log-mode t
	org-log-into-drawer t
	org-log-done 'time))

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp t)))
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

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))
  (add-to-list 'org-modules 'org-tempo t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package ox-gfm
  :after org)

(defun mk/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mk/org-mode-visual-fill))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds '(
		       ("https://news.ycombinator.com/rss")
		       ("https://www.reddit.com/r/emacs.rss")
		       ("https://www.reddit.com/r/swift.rss")
			   ("https://www.osnews.com/feed/")
			   ("https://www.feber.se/rss/")
		       ("https://www.reddit.com/r/haikuos.rss"))))

(setq-default elfeed-search-filter "@2-days-ago +unread")
(setq-default elfeed-search-title-max-width 100)
(setq-default elfeed-search-title-min-width 100)

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method #'bitmap))

(use-package highlight-operators
  :hook (swift-mode . highlight-operators-mode))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

 (use-package prescient
  :after ivy
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode))

(use-package ivy-hydra
  :after ivy)

;;  query stackoverflow
(use-package sx
  :commands sx-search)

;; Drag lines and regions around
(use-package drag-stuff
  :hook (prog-mode . drag-stuff-mode))

(use-package dumb-jump
  :hook (prog-mode . dumb-jump-mode)
  :custom
  (dumb-jump-selector 'ivy)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq	dumb-jump-selector 'completing-read
		xref-show-definitions-function #'xref-show-definitions-completing-read))

;; Kill all other buffers
(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))) (delete-other-windows))

(defun mk/toggle-transparency ()
  (interactive
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eq
		 (if (numberp alpha)
			 alpha
		   (cdr alpha)) ; may also be nil
		 100)
		(set-frame-parameter nil 'alpha '(94 . 85))
      (set-frame-parameter nil 'alpha '(100 . 100))))))

;;; esc quits

;; Setup Functions
(defun mk/setupProgrammingSettings ()
  "Programming mode"

  (defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

  ;; Drag stuff
  (global-set-key (kbd "M-<down>") #'drag-stuff-down)
  (global-set-key (kbd "M-<up>") #'drag-stuff-up)
  
  ;; Line movement
  (define-key evil-motion-state-map (kbd "C-j") #'(lambda () (interactive) (next-line 10)))
  (define-key evil-motion-state-map (kbd "C-k") #'(lambda () (interactive) (next-line -10)))
  (define-key evil-motion-state-map (kbd "C-h") #'(lambda () (interactive) (evil-beginning-of-visual-line)))
  (define-key evil-motion-state-map (kbd "C-l") #'(lambda () (interactive) (evil-end-of-line-or-visual-line)))

  ; When jumping got forward and back
  (define-key evil-motion-state-map (kbd "C-M-<left>") #'(lambda () (interactive) (xref-pop-marker-stack)))
  (define-key evil-motion-state-map (kbd "C-M-<right>") #'(lambda () (interactive) (xref-go-forward)))

  (define-key evil-motion-state-map (kbd "M-.") #'(dumb-jump-go))
  (define-key evil-motion-state-map (kbd "M-f") #'(lambda () (interactive) (counsel-imenu)))

  (define-key evil-insert-state-map (kbd "TAB")     #'tab-to-tab-stop)
  (define-key evil-motion-state-map (kbd "M-O")     #'projectile-find-file)
  (define-key evil-motion-state-map (kbd "C-M-f")   #'counsel-projectile-rg)
  (define-key evil-motion-state-map (kbd "M-F")     #'xref-find-definitions-other-window)
  (define-key evil-motion-state-map (kbd "C-M-e")   #'anzu-replace-at-cursor-thing)
  (define-key evil-motion-state-map (kbd "C-M-r")   #'anzu-query-replace-at-cursor)
  (define-key evil-motion-state-map (kbd "M-R")     #'projectile-recentf)

  (electric-pair-mode) ;; Auto insert pairs {} () [] etc

  (setq highlight-indent-guides-mode t	;; Turn on indent-guides
		indicate-empty-lines t			;; Show empty lines
		indicate-unused-lines t			;; Show unused lines
		show-trailing-whitespace nil    ;; Show or hide trailing whitespaces
		word-wrap nil					;; Dont word wrap in code mode
		truncate-lines 1				;; Truncate lines
		column-number-mode t			;; Show current line number highlighted
		display-line-numbers t))		;; Show line numbers


(defun mk/setupOrgMode ()
  (setq word-wrap t)
  (setq highlight-indent-guides-mode nil))

(defun mk/split-window-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun mk/split-window-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun with-faicon (icon str &optional height v-adjust)
  "Displays an icon from Font Awesome icon."
	(s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-fileicon (icon str &optional height v-adjust)
  "Display an icon from Font Awesome icon"
  (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defvar mk-elfeed--title (with-faicon "rss-square" "" 1.5 -0.225))
(pretty-hydra-define elfeed-hydra
  (:color pink :quit-key "q" :title mk-elfeed--title)
  ("Feed"
   (
    ("u" elfeed-update "Update")
    ("F" elfeed "Feed")
    ("q" hydra-keyboard-quit "Quit menu"))))

(defvar mk-stackoverflow--title (with-faicon "stack-overflow" "" 1.5 -0.225))
(pretty-hydra-define sx-hydra
  (:color pink :quit-key "q" :title mk-stackoverflow--title)
  ("Stackoverflow"
   (
    ("s" sx-search "Search")
    ("q" hydra-keyboard-quit "Quit menu"))))

(defvar mk-dumb-jump--title (with-faicon "search" "" 1.5 -0.225))
(pretty-hydra-define dumb-jump-hydra
  (:color pink :quit-key "q" :title mk-dumb-jump--title)
  ("Find reference in project"
   (
    ("a" xref-find-apropos "Apropos")
    ("f" dumb-jump-go "Definitions")
    ("o" xref-find-definitions-other-window "Definitions in other window")
    ("F" counsel-projectile-ag "Symbols")
    ("r" xref-find-references "References" :exit t)
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
	("." swiper-thing-at-point "Find all in file")
	("<left>" xref-pop-marker-stack "Back")
    ("q" hydra-keyboard-quit "Quit menu"))))

(defvar mk-text-scale-appearance--title (with-faicon "text-height" "Text size" 1.5 -0.225))
(pretty-hydra-define hydra-text-scale
  (:color pink :quit-key "q" :title mk-text-scale-appearance--title)
  ("Size"
   (
	("+" text-scale-increase "Increase size")
	("-" text-scale-decrease "Decrease size")
	("q" hydra-keyboard-quit "Quit menu"))))

(defvar mk-windows-appearance--title (with-faicon "desktop" "Appearance" 1.5 -0.225))
(pretty-hydra-define hydra-windows-setup
  (:color amaranth :quit-key "q" :title mk-windows-appearance--title)
  ("Windows"
   (("1" winum-select-window-1 "Win 1")
	("2" winum-select-window-2 "Win 2")
	("3" winum-select-window-3 "Win 3")
	("4" winum-select-window-4 "Win 4"))

   "Splitting"
   (("/" mk/split-window-right "Right")
	("-" mk/split-window-below "Below")
	("=" balance-windows "Balance"))

   "Rotate"
   (("c" evil-window-rotate-downwards "Clockwise")
	("w" evil-window-rotate-upwards "Counter clockwise"))

   "Toggles"
   (("t" mk/toggle-transparency "Transparency")
	("f" toggle-frame-fullscreen "Fullscreen")
	("m" toggle-frame-maximized "Maximized")
	("s" scroll-bar-mode "Scrollbar"))

   "Sizing"
   (("<left> " evil-window-decrease-width "⇢⇠ Decrease")
	("<right>" evil-window-increase-width "⇠⇢ Increase")
	("<up>   " evil-window-decrease-height "Decrease height")
	("<down> " evil-window-increase-height "Incease height"))

   "Extras"
   (("x" delete-window "Delete window")
	("q" hydra-keyboard-quit "Quit menu"))))

(defvar mk-toggles--title (with-faicon "toggle-on" "Toggles" 1.5 -0.225))
(pretty-hydra-define mk-toggles
  (:color amaranth :quit-key "q" :title mk-toggles--title)
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("s" symbol-overlay-mode "symbol" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))

(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("p" describe-package "package")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

(major-mode-hydra-define swift-mode nil
  ("Build/test:"
	 (("r" xcode-run-and-show-logs "Run" :exit t)
      ("b" xcode-build "Build" :exit t)
      ("t" xcode-test "Test" :exit t))
	 "Help"
	 (("." swift-helpful "Describe" :exit t)
	  ("o" lsp-ui-imenu "Overview" :exit t)
	  ("e" lsp-treemacs-error-list "Error list" :exit t))
     "Simulator menu"
     (("s" ios-simulator-menu/body "Simcity"))))

(add-hook 'prog-mode-hook #'mk/setupProgrammingSettings)
(add-hook 'org-mode-hook #'mk/setupOrgMode)

(defun xcode-run-and-show-logs ()
  (interactive)
  (save-some-buffers t)
  (xcode-run)
 ;; (ios-simulator-logs)
  )

(defun mk/display-buffer (buffer &optional alist)
  "Select window for BUFFER (need to use word ALIST on the first line).
Returns thirth visible window if there are three visible windows, nil otherwise.
Minibuffer is ignored."
  (let ((wnr (if (active-minibuffer-window) 3 2)))
    (when (= (+ wnr 1) (length (window-list)))
      (let ((window (nth wnr (window-list))))
        (set-window-buffer window buffer)
        window)))
  )

(defvar mk/buffers-to-display-below
  '("^\\*Flycheck errors\\*$"
    "^\\*Colors\\*$"
    "^\\*simulator logs\\*$"
    "^\\*Faces\\*$"
    "^\\*Async Shell Command\\*$"))

(while mk/buffers-to-display-below
  (add-to-list 'display-buffer-alist
               `(, (car mk/buffers-to-display-below)
                   (display-buffer-reuse-window
                    mk/display-buffer
                    display-buffer-in-side-window)
                   (reusable-frames     . visible)
                   (side                . bottom)
                   (window-height       . 0.25)
                   ))
  (setq mk/buffers-to-display-below (cdr mk/buffers-to-display-below)))

(provide 'init)

;;; init.el ends here
