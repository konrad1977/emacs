;;; init.el --- -*- lexical-binding: t -*-
;;; Code:

;; Window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(eval-when-compile (defvar display-time-24hr-format))
(eval-when-compile (defvar display-time-default-load-average nil))

(display-battery-mode t)		;; Show battery.
(display-time-mode t)			;; Show time.
(scroll-bar-mode -1)			;; Dont use scrollbars.
(set-fringe-mode 1)             ;; Give us some space.
(tooltip-mode 1)                ;; Disable tooltip.
(delete-selection-mode 1)		;; Use a more sane delete mode than evil.
(fset 'yes-or-no-p 'y-or-n-p)	;; Set yes or no to y/n
(global-font-lock-mode 1)		;; always highlight code
(global-auto-revert-mode 1)		;; refresh a buffer if changed on disk
(global-hl-line-mode 1)         ;; Highlight current line
(semantic-mode 1)               ;; help out with semantics
(savehist-mode 1)				;; Save history
(save-place-mode 1)             ;; when buffer is closed, save the cursor position
(blink-cursor-mode)

;; Setup fonts
(set-face-attribute 'default nil :font "Source Code Pro" :height 158)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" )
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile")

(setq ad-redefinition-action            'accept
	  blink-cursor-interval             0.6		;; Little slower cursor blinking . default is 0.5
	  create-lockfiles                  nil
	  fast-but-imprecise-scrolling      1
      inhibit-compacting-font-caches    t
	  idle-update-delay                 1.0     ;; Speed things up by not updating so often
	  initial-scratch-message           ""
	  read-process-output-max           (* 8 1024 1024)
      auto-mode-case-fold               nil
      backup-by-copying                 t
      backup-directory-alist            '(("." . "~/.emacs.d/backups"))
      byte-compile-warnings             '(ck-functions)
      confirm-kill-processes            nil
      desktop-save-mode                 nil ;; Done save desktop (open buffers)
      display-time-24hr-format          t
      display-time-default-load-average nil
      echo-keystrokes                   0.1
      kill-buffer-query-functions       nil ; - Dont ask for closing spawned processes
      line-number-mode                  nil
      use-dialog-box                    nil
      visible-bell                      nil)

(setq gc-cons-threshold (eval-when-compile (* 36 1024 1024)))
(run-with-idle-timer 4 t (lambda () (garbage-collect)))

(setq use-package-verbose nil
      use-package-expand-minimally nil
      use-package-compute-statistics nil
      debug-on-error t)

(setq-default display-line-numbers-width    4            ;; Set so we can display thousands of lines
			  c-basic-offset                4            ;; Set tab indent for c/c++ to 4 tabs
			  tab-width                     4            ;: Use four tabs
			  line-spacing                  0.02         ;; Increase linespacing a bit
			  ;truncate-lines                1			 ;; Truncate lines
			  indent-tabs-mode              nil			 ;; Never use tabs. Use spaces instead
			  completion-ignore-case        t            ;; Ignore case when completing
              indent-line-function          'insert-tab  ;; Use function to insert tabs
              history-length                100)

(let* ((path (expand-file-name "localpackages" user-emacs-directory))
       (local-pkgs (mapcar 'file-name-directory (directory-files-recursively path ".*\\.el"))))
  (if (file-accessible-directory-p path)
      (mapc (apply-partially 'add-to-list 'load-path) local-pkgs)
    (make-directory path :parents)))

(eval-when-compile (defvar savehist-additional-variables))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
(add-to-list 'savehist-additional-variables 'kill-ring)

;; Dont leave #file autosaves everywhere I go
(defvar my-auto-save-folder (concat user-emacs-directory "var/auto-save/"))
(setq auto-save-list-file-prefix (concat my-auto-save-folder ".saves-")); set prefix for auto-saves
(setq auto-save-file-name-transforms `((".*", my-auto-save-folder t))); location for all auto-save files
(setq custom-file (concat user-emacs-directory "var/custom.el"))

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

(use-package recentf
  :hook (after-init . recentf-mode))

(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize))

;; Make sure we are up to date, atleast once a week
(use-package auto-package-update
  :defer t
  :custom
  (setq auto-package-update-interval 7
		auto-package-update-prompt-before-update t
		auto-package-update-hide-results nil))

(use-package no-littering)	;; Clean up all those temporary files

; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)
 
  (use-package ns-auto-titlebar
	:config (ns-auto-titlebar-mode))

  (setq mac-option-key-is-meta nil
		mac-command-key-is-meta t
		mac-command-modifier 'meta
		mac-option-modifier 'none
		dired-use-ls-dired nil
		frame-title-format ""
        ns-pop-up-frames nil
        browse-url-browser-function #'mk/browser-split-window)

  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Config and install modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-encoding nil
		doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 50
       	doom-modeline-height 37)
  (set-face-attribute 'mode-line nil :height 160 :box '(:line-width -1 :color "#0C0A10"))
  (set-face-attribute 'mode-line-inactive nil :height 150 :box '(:line-width -1 :color "#332E41")))

(use-package autothemer
  :custom (setq custom-safe-themes t))

(load-theme 'catppuccin t)

;;  theming
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (make-local-variable 'face-remapping-alist)
            (add-to-list 'face-remapping-alist '(default (:background "#16161D")))))

;; (use-package centaur-tabs
;;   :hook
;;   (dashboard-mode . centaur-tabs-local-mode)
;;   (vterm-mode . centaur-tabs-local-mode)
;;   (calendar-mode . centaur-tabs-local-mode)
;;   (org-agenda-mode . centaur-tabs-local-mode)
;;   (helpful-mode . centaur-tabs-local-mode)
;;   (xwidget-webkit-mode . centaur-tabs-local-mode)
;;   (periphery-mode . centaur-tabs-local-mode)
;;   :config
;;   (add-to-list 'centaur-tabs-excluded-prefixes "*xcodebuild")
;;   ;(centaur-tabs-mode)
;;   (centaur-tabs-headline-match)
;;   (centaur-tabs-group-by-projectile-project)
;;   (setq centaur-tabs-style "box"
;; 		centaur-tabs-height 27
;;         centaur-tabs-gray-out-icons 'buffer
;; 		centaur-tabs-set-modified-marker t
;; 		centaur-tabs-show-navigation-buttons nil
;; 		centaur-tabs-plain-icons t
;; 		centaur-tabs-set-icons t
;; 		uniquify-buffer-name-style 'forward)
;;   :bind
;;   (:map evil-normal-state-map
;; 	     ("g t" . centaur-tabs-forward)
;; 	     ("g T" . centaur-tabs-backward)))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
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
						  (recents . 7)
                          )))

;; Which key
(use-package which-key
  :hook (after-init . which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-prefix-prefix "◉ ")
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 1
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

(use-package undo-fu
  :defer t)

; Use evil mode
(use-package evil
  :hook (after-init . evil-mode)
  :init
  ;; (setq evil-undo-system 'undo-redo)
  (setq evil-want-integration t
        evil-want-fine-undo t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-undo-system 'undo-fu
        evil-want-C-i-jump nil)
  :config
  (setq evil-emacs-state-cursor '("#FF5D62" box))
  (setq evil-normal-state-cursor '("#FF5D62" box))
  (setq evil-visual-state-cursor '("#98BB6C" box))
  (setq evil-insert-state-cursor '("#E82424" bar))
  (setq evil-replace-state-cursor '("#FF9E3B" hbar))
  (setq evil-operator-state-cursor '("#7E9CD8" hollow))
  
  (evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)

  (define-key evil-motion-state-map (kbd "M-u") #'evil-undo)
  (define-key evil-motion-state-map (kbd "M-U") #'evil-redo)
  (define-key evil-motion-state-map (kbd "M-0") #'treemacs)
  (define-key evil-motion-state-map (kbd "q") #'exit-minibuffer)
  (define-key evil-motion-state-map (kbd "C-f") #'periphery-search-rg)
  (define-key evil-motion-state-map "/" 'swiper))

(use-package evil-tutor
  :commands evil-tutor)

(use-package evil-collection
  :after evil
  :custom
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(define-key global-map [remap quit-window] 'kill-buffer-and-window) ;; remap kill window to kill buffer also
(define-key global-map [remap kill-buffer] 'kill-buffer-and-window) ;; remap kill window to kill buffer also

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(global-set-key (kbd "M-1") 'winum-select-window-1)
(global-set-key (kbd "M-2") 'winum-select-window-2)
(global-set-key (kbd "M-3") 'winum-select-window-3)
(global-set-key (kbd "M-4") 'winum-select-window-4)
(global-set-key (kbd "M-5") 'winum-select-window-5)
(global-set-key (kbd "M-6") 'winum-select-window-6)

(global-set-key (kbd "C-c C-b") #'bm-toggle)
(global-set-key (kbd "C-c C-p") #'bm-previous)
(global-set-key (kbd "C-c C-n") #'bm-next)

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
  :after doom-modeline)

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

;; nyan cat
(use-package nyan-mode
  :hook (doom-modeline-mode . nyan-mode)
  :config
  (setq nyan-animate-nyancat t))

(use-package dimmer
  :hook (prog-mode . dimmer-mode)
  :config
  (dimmer-configure-org)
  (dimmer-configure-magit)
  (dimmer-configure-hydra)
  (dimmer-configure-which-key)
  (setq dimmer-fraction 0.15))

(use-package beacon
  :hook (after-init . beacon-mode)
  :config
  (setq beacon-color "#A3D4D5"
        beacon-blink-when-focused t
        beacon-size 40
        beacon-blink-when-window-scrolls nil))

;; rainbow-delimieters
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; rainbow-mode show hex as colors
(use-package rainbow-mode
  :commands rainbow-mode)

(use-package paren
  :hook (prog-mode . show-paren-mode)
  :config
  (setq show-paren-delay 0.2
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-ring-bell-on-mismatch t
        show-paren-when-point-in-periphery t))

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
  (define-key ivy-mode-map       (kbd "<escape>") #'kill-current-buffer)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit))

;; ;; Ivy rich
(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :custom
  (setq ivy-virtual-abbreviate 'abbreviate
		ivy-rich-switch-buffer-align-virtual-buffer nil
		ivy-rich-path-style 'abbreviate))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :after ivy
  :init (all-the-icons-ivy-rich-mode 1)
  :custom
  (setq all-the-icons-ivy-rich-icon t
        all-the-icons-ivy-rich-color-icon nil
        all-the-icons-ivy-rich-icon-size 1.0))

;; counsel
(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-rg-base-command
      "rg -i -M 120 --no-heading --line-number %s ."))

(use-package tree-sitter
  :hook (swift-mode . tree-sitter-mode))

;; Remember autocompletions
(use-package amx
  :after ivy
  :config
  (amx-mode 1))

(use-package request
  :commands counsel-search)

;; Search files, and do it with speed and style
(use-package swiper
  :commands (swiper-thing-at-point swiper)
  :bind ("C-s" . swiper-thing-at-point)
  :config
  (setq swiper-goto-start-of-match t))

;; ------------------ SEARCHING -------------------
;; the silver searcher
(use-package ag
  :defer t)

(use-package fzf
 :commands (fzf-git-files fzf-projectile fzf-recentf)
 :bind
 ("C-<tab>" . #'fzf-git-files)
 :config
 (setq fzf/args "-x --color --print-query  --margin=1,0 --no-hscroll"
  fzf/window-height 12))

;; ------------------ EDITING -------------------
;; - anzu search and replace/
(use-package anzu
  :commands (anzu-replace-at-cursor-thing anzu-query-replace)
  :bind
  ("C-M-e" . #'anzu-replace-at-cursor-thing)
  ("C-M-r" . #'anzu-query-replace))

(use-package multiple-cursors
  :defer t
  :bind
  ("C-M-s" . #'mc/edit-lines)
  ("C-M-a" . #'mc/mark-all-like-this)
  ("C-M-n" . #'mc/mark-next-symbol-like-this)
  ("C-M-p" . #'mc/mark-previous-symbol-like-this))

;; Navigate through blocks
(use-package block-nav
  :commands (block-nav-next-block 
             block-nav-previous-block 
             block-nav-next-indentation-level
             block-nav-previous-indentation-level)
  :bind
  ("C-c C-j" . block-nav-next-block)
  ("C-c C-k" . block-nav-previous-block)
  ("C-c C-l" . block-nav-next-indentation-level)
  ("C-c C-h" . block-nav-previous-indentation-level))


;; ------------------ autocompletions -------------
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
  (setq company-format-margin-function  'company-dot-icons-margin)
  (setq company-dot-icons-format        " ● ")
  (setq company-backends '(company-capf
                           company-tabnine
                           company-keywords
                           company-dabbrev-code
                           company-sourcekit
                           company-semantic
                           company-files)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-tooltip-margin              2
        company-minimum-prefix-length       2
        company-tooltip-align-annotations   t
        company-search-regexp-function      'company-search-flex-regexp
        company-require-match               'never
        company-tooltip-limit               9
        company-tooltip-width-grow-only     t
        company-tooltip-flip-when-above     t
        company-tooltip-idle-delay          0.3
        company-async-wait                  0.4
        company-idle-delay                  0.3
        company-show-quick-access           'left
        company-async-timeout               2)
  :custom-face
  (company-tooltip ((t (:font "Menlo" :height 155)))))

(use-package company-sourcekit
  :after company
  :config
  (setq sourcekit-sourcekittendaemon-executable "/usr/local/bin/sourcekittend"))

(use-package company-tabnine
  :after company
  :config
  (setq company-tabnine-max-num-results 9
        company-tabnine-show-annotation t
        company-tabnine-insert-arguments t
        company-tabnine-auto-fallback t
        company-tabnine-auto-balance t
        company-tabnine-use-native-json t
        company-tabnine-wait 0.1))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

(use-package ace-jump-mode
  :bind ("M-g" . ace-jump-mode))

(use-package yasnippet
  :hook (company-mode . yas-minor-mode))

;; ------------------ FILES -----------------------
(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :config
  (setq treemacs-follow-after-init t
        treemacs-expand-after-init t
		treemacs-project-follow-mode t
		treemacs-follow-mode t
		treemacs-filewatch-mode t
        treemacs-width 40
        treemacs-indentation 1
        treemacs-git-integration t
        treemacs-git-mode 'extended
        treemacs-collapse-dirs 0
        treemacs-silent-refresh	t
		treemacs-change-root-without-asking t
        treemacs-sorting 'alphabetic-case-insensitive-desc
        treemacs-show-hidden-files nil
        treemacs-never-persist nil
        treemacs-is-never-other-window nil
		treemacs-display-current-project-exclusively t
        treemacs-goto-tag-strategy 'refetch-index
		treemacs-text-scale	0)
  (treemacs-project-follow-mode t))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package restclient
  :defer t)

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :diminish
  :custom
  (flycheck-indication-mode 'left-fringe)
  (flycheck-display-errors-delay 0.1)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 1))

(use-package flycheck-inline
  :ensure t
  :hook (flycheck-mode . turn-on-flycheck-inline))

(use-package swift-mode
  :hook (swift-mode . setup-swift-programming)
  :custom
  (setq swift-mode:parenthesized-expression-offset 4
		swift-mode:multiline-statement-offset 4))

(defun mk/macos-hardware-overview ()
  "View macOS hardware overview."
  (interactive)
  (shell-command "system_profiler SPHardwareDataType"))

(use-package clean-aindent-mode
  :hook (prog-mode . clean-aindent-mode)
  :config
  (setq clean-aindent-is-simple-indent t)
  (define-key global-map (kbd "RET") 'newline-and-indent))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :diminish projectile-mode
  :custom
  (add-to-list 'projectile-globally-ignored-directories '"^\\.build$")
  (add-to-list 'projectile-globally-ignored-directories '"^\\build$")
  (add-to-list 'projectile-globally-ignored-directories '"^\\.swiftpm$")
  (add-to-list 'projectile-globally-ignored-directories '"^\\elpa$")
  (add-to-list 'projectile-globally-ignored-directories '"^\\xcodeproj$")
  (add-to-list 'projectile-globally-ignored-directories '"^\\pods$")
  (setq projectile-completion-system 'ivy
		projectile-enable-caching t
		projectile-sort-order 'recentf
		projectile-indexing-method 'alien
        projectile-switch-project-action #'projectile-find-file-dwim
        projectile-ignored-files '(".orig" ".yml"))
    (projectile-project-root-files-functions
     '(projectile-root-local
       projectile-root-top-down
       projectile-root-bottom-up
       projectile-root-top-down-recurring))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/git")
    (setq projectile-project-search-path '("~/Documents/git"))))

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

; Window / buffer configuration -----------------------------
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("*xwidget*"
      (display-buffer-in-side-window display-buffer-reuse-mode-window display-buffer-reuse-window)
      (body-function . select-window)
      (window-width . 0.5)
      (side . right))
     ("\\*occur\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.4)
      (side . bottom)
      (slot . 1))
     ("\\*xcodebuild\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.3)
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
      (window-height . 0.2)
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
  :init
  (winum-mode 1))

;; darkroom (go to focus mode)
(use-package darkroom
  :commands darkroom-mode)

;; Use git
(use-package magit
  :commands magit-status
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 4
        magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*"))
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME"))))
 
(use-package blamer
  :commands (blamer-mode)
  :config
  (setq blamer-view 'overlay-right
        blamer-type 'visual
        blamer-max-commit-message-length 180
        blamer-author-formatter " ✎ [%s] - "
        blamer-commit-formatter "● %s ● ")
  :custom
  (blamer-idle-time 1.0)
  (blamer-min-offset 10)
  :custom-face
  (blamer-face ((t :foreground "#E46876"
                    :height 140
                    :italic t))))
                   
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.5))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated)))

(use-package forge
  :commands forge-pull
  :config (setq auth-sources '("~/.authinfo")))

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
	"'" '((lambda () (interactive) (vterm)) :which-key "Term")
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
    "bi" '(ibuffer :which-key "ibuffers")
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
    "cl" '(comment-dwim :which-key "Comment line")
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
	 "wb" '((lambda () (interactive) (xwidget-webkit-browse-url "https://www.duckduckgo.com")) :which-key "Start a browser")
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
  ;; (require 'ob-swiftui)
  ;; (ob-swiftui-setup)
  ;; (org-babel-do-load-languages 'org-babel-load-languages
  ;;   		                   '((emacs-lisp t)
  ;;                               (swift t)))
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
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))
  (add-to-list 'org-modules 'org-tempo t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package ox-gfm
  :after org)

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
               ("https://swiftbysundell.com/rss")
		       ("https://www.reddit.com/r/haikuos.rss"))))

(setq-default elfeed-search-filter "@1-days-ago +unread")
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

;; Quickly jump to definition or usage
(use-package dumb-jump
  :hook (prog-mode . dumb-jump-mode))

(use-package bm
  :defer t
  :init
  (setq bm-restore-repository-on-load t) ;; restore on load (even before you require bm)
  :config
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  (setq bm-cycle-all-buffers t)
  (setq bm-repository-file "~/.emacs.d/bm-repository")  ;; where to store persistant files
  (setq-default bm-buffer-persistence t)                ;; save bookmarks
  (add-hook 'after-init-hook 'bm-repository-load)       ;; Loading the repository from file when on start up.
  (add-hook 'kill-buffer-hook #'bm-buffer-save)         ;; Saving bookmarks

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)
  
  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save))

(defun setup-swift-programming ()
  "Setup swift development environment."
  
  (setq tree-sitter-hl-use-font-lock-keywords nil)
  (defface tree-sitter-hl-face:case-pattern
    '((t :inherit tree-sitter-hl-face:property
         :foreground "#666bb2"))
    "Face for enum case names in a pattern match"
    :group 'tree-sitter-hl-faces)
  
  (defface tree-sitter-hl-face:variable.synthesized
    '((t :inherit tree-sitter-hl-face:variable))
    "Face for compiler-synthesized identifiers (prefixed with '$')"
    :group 'tree-sitter-hl-faces)
  
  (defface tree-sitter-hl-face:keyword.compiler
    '((t :inherit tree-sitter-hl-face:keyword
         :weight semi-bold))
    "Face for compile-time keywords"
    :group 'tree-sitter-hl-faces)

  (load "swift-additions")
  (load "periphery-search")
  (load "periphery-swiftlint")
  (load "swift-querying")
  (load "localizeable-mode")

  (tree-sitter-hl-mode)
  
  (local-set-key (kbd "M-P") #'swift-additions:print-thing-at-point)
  (local-set-key (kbd "M-m") #'swift-additions:insert-mark)
  (local-set-key (kbd "C-M-t") #'swift-additions:insert-todo)
  ;; (local-set-key (kbd "C-c C-f") #'swift-additions:functions-and-pragmas)
  (local-set-key (kbd "M-r") #'swift-additions:build-and-run-ios-app)
  (local-set-key (kbd "C-c C-a") #'swift-additions:analyze-using-periphery)
  (local-set-key (kbd "C-c C-l") #'periphery-run-swiftlint)
  (local-set-key (kbd "C-c C-c") #'swift-additions:compile-and-run-silent)
  (local-set-key (kbd "C-c C-x") #'swift-additions:reset-settings)
  (local-set-key (kbd "M-s") #'swift-additions:terminate-all-running-apps)
  (local-set-key (kbd "M-K") #'swift-additions:clean-build-folder)
  (local-set-key (kbd "M-L") #'swift-additions:clear-xcodebuild-buffer)
  (local-set-key (kbd "M-b") #'swift-additions:build-ios-app)
  (local-set-key (kbd "C-c C-s") #'swift-additions:split-func-list)
  (local-set-key (kbd "C-c C-r") #'xcode-build:run)
  (local-set-key (kbd "C-c C-f") #'periphery-search-thing-at-point-rg)

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
  (visual-line-mode 1))

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
  ;; (global-set-keyy (kbd "M-<left>") #'drag-stuff-left)
  ;; (global-set-key (kbd "M-<right>") #'drag-stuff-right)
  (global-set-key (kbd "M-+") #'mk/toggle-flycheck-errors)
  (global-set-key (kbd "C-x C-d") #'darkroom-mode)

  (hs-minor-mode)
  (local-set-key (kbd "M-B") #'counsel-projectile-switch-to-buffer)
  (local-set-key (kbd "C-M-B") #'projectile-switch-to-buffer-other-window)
  (local-set-key (kbd "M-t") #'ivy-magit-todos)
  (local-set-key (kbd "C-M-K") #'kill-other-buffers)

  ;; Line movement
  (define-key evil-motion-state-map (kbd "C-j") #'(lambda () (interactive) (next-line 10)))
  (define-key evil-motion-state-map (kbd "C-k") #'(lambda () (interactive) (next-line -10)))
  (define-key evil-motion-state-map (kbd "C-h") #'(lambda () (interactive) (evil-first-non-blank)))
  (define-key evil-motion-state-map (kbd "C-l") #'(lambda () (interactive) (evil-last-non-blank)))

  ; When jumping got forward and back
  (define-key evil-motion-state-map (kbd "C-M-<left>")  #'(lambda () (interactive) (xref-pop-marker-stack)))
  (define-key evil-motion-state-map (kbd "C-M-<right>") #'(lambda () (interactive) (xref-go-forward)))

  (define-key evil-motion-state-map (kbd "M-f")     #'(lambda () (interactive) (counsel-imenu)))

  (define-key evil-insert-state-map (kbd "TAB")     #'tab-to-tab-stop)
  (define-key evil-motion-state-map (kbd "M-O")     #'projectile-find-file-dwim)
  (define-key evil-motion-state-map (kbd "C-M-O")   #'projectile-find-file-dwim-other-window)
  (define-key evil-motion-state-map (kbd "M-F")     #'counsel-projectile-rg)
  (define-key evil-motion-state-map (kbd "M-R")     #'projectile-recentf)

  (electric-pair-mode) ;; Auto insert pairs {} () [] etc

  (setq highlight-indent-guides-mode t    ;; Turn on indent-guides
		indicate-empty-lines t            ;; Show empty lines
		indicate-unused-lines t           ;; Show unused lines
		show-trailing-whitespace nil      ;; Show or hide trailing whitespaces
		word-wrap t                       ;; Dont word wrap in code mode
		truncate-lines nil                ;; Truncate lines
		column-number-mode nil            ;; Show current line number highlighted
		display-line-numbers t))          ;; Show line numbers

(defun mk/setup-flycheck ()
  "Setup margins for flycheck."
  (setq left-fringe-width 12 right-fringe-width 12
        left-margin-width 1 right-margin-width 0)
  (flycheck-refresh-fringes-and-margins))

(add-hook 'flycheck-mode-hook #'mk/setup-flycheck)

(defun mk/toggle-flycheck-errors ()
  "Function to toggle flycheck errors."
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
    (list-flycheck-errors)))

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

(defun mk/tabnine-off ()
  "turn off TabNine for this buffer."
  (interactive)
  (setq-local company-backends (delete 'company-tabnine company-backends))
  (message "Turning off TabNine auto completion for current buffer"))

(defun mk/tabnine-on ()
  "turn on TabNine for this buffer."
  (interactive)
  (setq-local company-backends (add-to-list 'company-backends 'company-tabnine))
  (message "Turning on TabNine auto completion for current buffer"))

(defun with-faicon (icon str &optional height v-adjust)
  "Displays an ICON from Font Awesome icon."
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
   (("<left> " evil-window-increase-width "⇢⇠ Decrease")
	("<right>" evil-window-decrease-width "⇠⇢ Increase")
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
	 (("r" xcode-run "Run" :exit t)
      ("b" xcode-build "Build" :exit t)
      ("t" xcode-test "Test" :exit t))
	 "Tabnine"
	 (("1" mk/tabnine-on "TabNine on" :exit t)
	  ("0" mk/tabnine-off "TabNine off" :exit t))
	 "Help"
	 (("." swift-helpful "Describe" :exit t)
	  ("o" lsp-ui-imenu "Overview" :exit t)
	  ("e" lsp-treemacs-error-list "Error list" :exit t))
     "Simulator menu"
     (("s" ios-simulator-menu/body "Simcity"))))

(add-hook 'prog-mode-hook #'mk/setupProgrammingSettings)
(add-hook 'org-mode-hook #'mk/setupOrgMode)

(setq gc-cons-threshold (* 2 1024 1024))

(provide 'init)

;;; init.el ends here
