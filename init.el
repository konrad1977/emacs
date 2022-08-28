;;; init.el --- -*- lexical-binding: t -*-
;;; Code:

;; Window

(eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

(display-battery-mode t)		  ;; Show battery.
(display-time-mode t)			  ;; Show time.
(set-fringe-mode 1)               ;; Give us some space.
(tooltip-mode 1)                  ;; Disable tool-tip.
(delete-selection-mode 1)		  ;; Use a more sane delete mode than evil.
(fset 'yes-or-no-p 'y-or-n-p)     ;; Set yes or no to y/n
(global-font-lock-mode 1)         ;; always highlight code
(global-auto-revert-mode 1)       ;; refresh a buffer if changed on disk
(global-hl-line-mode 1)           ;; Highlight current line
(semantic-mode 1)                 ;; help out with semantics
(savehist-mode 1)                 ;; Save history
(save-place-mode 1)               ;; when buffer is closed, save the cursor position
(blink-cursor-mode 1)
  
;; Setup fonts
(set-face-attribute 'default nil :font "Source Code Pro" :height 158)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" )
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light)
(variable-pitch-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq ad-redefinition-action            'accept
      blink-cursor-interval             0.6       ;; Little slower cursor blinking . default is 0.5
      create-lockfiles                  nil
      idle-update-delay                 1.2    ;; Speed things up by not updating so often
      read-process-output-max           (* 8 1024 1024)
      highlight-nonselected-windows     t
      auto-mode-case-fold               nil
      backup-by-copying                 t
      backup-directory-alist            '(("." . "~/.emacs.d/backups"))
      byte-compile-warnings             '(ck-functions)
      confirm-kill-processes            nil
      fast-but-imprecise-scrolling      t
      jit-lock-defer-time               0.0
      desktop-save-mode                 nil    ;; Done save desktop (open buffers)
      echo-keystrokes                   0.2
      kill-buffer-query-functions       nil    ;; Dont ask for closing spawned processes
      line-number-mode                  nil
      use-dialog-box                    nil
      word-wrap                         nil
      visible-bell                      nil
      bidi-display-reordering           nil
      x-stretch-cursor                  t   ;; stretch cursor on tabs
      scroll-margin                     4   ;; scroll N to screen edge
      undo-limit                        6710886400 ;; 64mb
      undo-strong-limit                 100663296 ;; x 1.5 (96mb)
      undo-outer-limit                  1006632960 ;; x 10 (960mb), (Emacs uses x100), but this seems too high.
      )

(setq gc-cons-threshold (eval-when-compile (* 50 1024 1024)))
(run-with-idle-timer 4 t (lambda () (garbage-collect)))

(setq use-package-verbose nil
      use-package-expand-minimally t
      use-package-compute-statistics nil
      debug-on-error nil)

(setq-default display-line-numbers-width    4            ;; Set so we can display thousands of lines
              c-basic-offset                4            ;; Set tab indent for c/c++ to 4 tabs
              tab-width                     4            ;: Use four tabs
              line-spacing                  0.03          ;; Increase linespacing a bit
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

;; Clean up all those temporary files
(use-package no-littering)

; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)

  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize))

  (use-package ns-auto-titlebar
    :config (ns-auto-titlebar-mode))

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
  :defer t
  :config (setq ispell-program-name "aspell"))

(use-package autothemer)
(load-theme 'catppuccin t)
;; (load-theme 'kanagawa t)
;; (load-theme 'doom-old-hope t)

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize t
        vertico-cycle t))

(use-package vertico-posframe
  :after vertico
  :config (vertico-posframe-mode 1)
  (setq
   vertico-posframe-width 120
   ;; vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
   ;; vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center
   vertico-posframe-poshandler #'posframe-poshandler-frame-center ;
   vertico-posframe-height nil
   vertico-posframe-border-width 2
   vertico-posframe-parameters
   '(
     (left-fringe . 0)
     (right-fringe . 0))))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless basic)
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
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-x C-x" . kill-buffer-and-window)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
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
        doom-modeline-lsp nil
        doom-modeline-major-mode-color-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-time-icon nil))

(use-package centered-cursor-mode
  :hook (prog-mode . centered-cursor-mode))

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
        dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-time
        dashboard-week-agenda t
        dashboard-items '(
                          (projects . 5)
                          (bookmarks . 5)
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
        evil-want-minibuffer nil
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
 ;; (define-key evil-motion-state-map "M-f" 'consult-line)
  
  ;; window resizing
  (define-key evil-motion-state-map (kbd "C-+") #'enlarge-window-horizontally)
  (define-key evil-motion-state-map (kbd "C--") #'shrink-window-horizontally)
  (define-key evil-motion-state-map (kbd "C-M-+") #'enlarge-window)
  (define-key evil-motion-state-map (kbd "C-M--") #'shrink-window)

  (define-key evil-motion-state-map (kbd "M-R") #'consult-projectile-recentf)
  (define-key evil-motion-state-map (kbd "M-u") #'evil-undo)
  (define-key evil-motion-state-map (kbd "M-U") #'evil-redo)
  (define-key evil-motion-state-map (kbd "M-0") #'treemacs)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-ex-nohighlight)
  (define-key evil-motion-state-map (kbd "<backtab>") #'consult-buffer)
  (define-key evil-motion-state-map (kbd "q") #'exit-minibuffer)
  (define-key evil-motion-state-map (kbd "C-f") #'periphery-search-rg)
  (define-key evil-insert-state-map (kbd "TAB") #'tab-to-tab-stop)
  (define-key evil-insert-state-map (kbd "<backtab>") #'un-indent-by-removing-4-spaces))

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

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-tutor
  :commands evil-tutor)

(define-key global-map [remap quit-window] 'kill-buffer-and-window) ;; remap kill window to kill buffer also
(define-key global-map [remap kill-buffer] 'kill-buffer-and-window) ;; remap kill window to kill buffer also

(global-set-key (kbd "C-c C-b") #'consult-bookmark)
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

;; nyan cat
(use-package nyan-mode
  :hook (doom-modeline-mode . nyan-mode)
  :config
  (setq nyan-wavy-trail t
        nyan-bar-length 30
        nyan-animate-nyancat t))

(use-package dimmer
  :hook (prog-mode . dimmer-mode)
  :config
  (dimmer-configure-org)
  (dimmer-configure-magit)
  (dimmer-configure-company-box)
  (dimmer-configure-posframe)
  (dimmer-configure-hydra)
  (dimmer-configure-which-key)
  (add-to-list 'dimmer-exclusion-regexp-list "^\\*xcodebuild\\*$")
  (setq dimmer-fraction 0.5))

;; (use-package beacon
;;   :hook (after-init . beacon-mode)
;;   :config
;;   (setq beacon-color "#A3D4D5"
;;         beacon-blink-when-focused t
;;         beacon-size 40
;;         beacon-blink-when-window-scrolls nil))

;; rainbow-delimieters
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

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
  :hook (swift-mode . tree-sitter-hl-mode)
  :init
  (global-tree-sitter-mode)
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
;; - anzu search and replace/
(use-package anzu
  :commands (anzu-replace-at-cursor-thing anzu-query-replace)
  :bind
  ("C-M-e" . #'anzu-replace-at-cursor-thing)
  ("C-M-r" . #'anzu-query-replace))

(use-package multiple-cursors
  :hook (prog-mode . multiple-cursors-mode)
  :bind
  ("C-M-s" . #'mc/edit-lines)
  ("C-M-a" . #'mc/mark-all-like-this-dwim)
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

(use-package consult-project-extra
  :after consult
  :bind
  ("C-<tab>" . #'consult-projectile)
  ("M-O" . #'consult-project-extra-find))

(use-package consult-ls-git
  :after consult)

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

(use-package google-this
  :defer t
  :bind ("C-x C-g" . google-this))

(use-package eglot
    :ensure t
    :hook (swift-mode . eglot-ensure)
    :config
    (setq eglot-stay-out-of '(company))
    (add-to-list 'eglot-server-programs '(swift-mode . ("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))))

(use-package company
  :defer t
  :hook (prog-mode . company-mode)
  :config
  (setq company-transformers '(delete-consecutive-dups company-sort-by-occurrence)
        ;; company-transformers nil
        ;; company-format-margin-function  'company-vscode-dark-icons-margin
        ;; company-dot-icons-format        " ● "
        company-tooltip-margin              1
        company-minimum-prefix-length       1
        company-tooltip-align-annotations   t
        company-search-regexp-function      'company-search-words-in-any-order-regexp
        company-require-match               nil
        company-tooltip-limit               10
        company-tooltip-width-grow-only     nil
        company-tooltip-flip-when-above     t
        company-idle-delay                  0.5
        company-show-quick-access           'left
        company-async-wait                  0.1
        company-async-timeout               2
        company-backends '(company-capf
                           company-dabbrev-code
                           company-keywords
                           company-yasnippet)
        company-frontends '(company-box-frontend))
  :custom-face
  (company-tooltip ((t (:font "Iosevka Aile" :height 156)))))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-backends-colors '((company-yasnippet 
                                       :all (:foreground "PaleVioletRed2" :background nil) 
                                       :selected (:foreground "black" :background "PaleVioletRed4")))
        company-box-doc-delay 0.2))

(defun setup-swift-mode-company ()
  "Setup company with separate bakends merged into one."
  (setq-local company-backends
              '((company-capf company-dabbrev-code company-yasnippet :separate))))

(use-package consult-company
  :after company
  :config
  (define-key company-mode-map [remap completion-at-point] #'consult-company))

(defun tabnine//company-box-icons--tabnine (candidate)
  (when (eq (get-text-property 0 'company-backend candidate)
            'company-tabnine)
    'Reference))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

(use-package consult-project-extra
  :bind
  ("C-<tab>" . #'consult-projectile)
  ("M-O" . #'consult-project-extra-find-other-window)
  :after consult)

(use-package ace-jump-mode
  :bind ("M-g" . ace-jump-mode))

(use-package yasnippet
  :hook (company-mode . yas-minor-mode))

;; ------------------ FILES -----------------------
(use-package treemacs
  :commands (treemacs treemacs-select-window)
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
        treemacs-width 40)
  :init (treemacs-project-follow-mode))

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
  (flycheck-check-syntax-automatically '(save idle-change newline))
  (flycheck-idle-change-delay 1))

(use-package flycheck-inline
  :hook (flycheck-mode . turn-on-flycheck-inline))

(use-package swift-mode
  :hook (swift-mode . setup-swift-programming))

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
  (setq projectile-completion-system 'vertico
        projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-indexing-method 'alien
        projectile-switch-project-action #'consult-projectile-switch-project
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

                                        ; Window / buffer configuration -----------------------------
(use-package window
  :ensure nil
  :bind
  ("C-x C-f" . toggle-frame-fullscreen)
  :custom
  (display-buffer-alist
   '(("*xwidget*"
      (display-buffer-in-side-window display-buffer-reuse-mode-window display-buffer-reuse-window)
      (body-function . select-window)
      (window-width . 0.4)
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
   '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))))

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
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated)))

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
    "s" '(consult-line-multi :which-key "Consult multi search")
    "0" '(treemacs-select-window :which-key "Treemacs")
    "1" '(winum-select-window-1 :which-key "Window 1")
    "2" '(winum-select-window-2 :which-key "Window 2")
    "3" '(winum-select-window-3 :which-key "Window 3")
    "4" '(winum-select-window-4 :which-key "Window 4")
    "5" '(winum-select-window-5 :which-key "Window 5")
    "6" '(winum-select-window-6 :which-key "Window 6")
    "P" 'package-install
    "'" '((lambda () (interactive) (vterm)) :which-key "Term")
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
    "b" '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "Switch buffer")
    "bm" '(lambda () (interactive) (switch-to-buffer "*Messages*") :which-key "Message buffer")
    "bs" '(lambda () (interactive) (switch-to-buffer "*scratch*") :which-key "Scratch buffer")
    )
  
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
    "ff" '(find-file :which-key "Find file")
    "fr" '(consult-recent-file :which-key "Recent files")
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
    "wn" '(next-window-any-frame :which-key "Next window"))

  (mk/leader-keys
    "p" '(:ignore t :which-key "Project")
    "pp" '(:ignore t :which-key "Project management")
    "ppa" '(treemacs-add-project-to-workspace :which-key "Add project")
    "ppr" '(treemacs-remove-project-from-workspace :which-key "Remove project")
    "pf" '(projectile-find-file-dwim :which-key "Find file")
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
        org-log-into-drawer t
        org-log-done 'time))

(with-eval-after-load 'org
  ;; (require 'ob-swiftui)
  ;; (ob-swiftui-setup)
  ;; (org-babel-do-load-languages 'org-babel-load-languages
  ;;                               '((emacs-lisp t)
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

(use-package visual-fill-column
  :hook (org-mode . mk/org-mode-visual-fill))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds '(
                       ("https://news.ycombinator.com/rss")
                       ("https://ag91.github.io/rss")
                       ("https://www.reddit.com/r/emacs.rss")
                       ("https://xenodium.com/rss")
                       ("https://swiftbysundell.com/rss"))
        elfeed-search-filter "@1-days-ago +unread"
        elfeed-search-title-max-width 100
        elfeed-search-title-min-width 100))

(use-package elfeed-dashboard
  :after elfeed
  :config
(setq elfeed-dashboard-file "~/elfeed-dashboard.org")
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method #'character))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

;; Drag lines and regions around
(use-package drag-stuff
  :hook (prog-mode . drag-stuff-mode)
  :bind
  ("S-M-<down>" . drag-stuff-down)
  ("M-S-<up>" . drag-stuff-up)
  ("M-S-<left>" . left-stuff-left)
  ("M-S-<right>" . drag-stuff-right))

;; Quickly jump to definition or usage
(use-package dumb-jump
  :hook (prog-mode . dumb-jump-mode)
  :config
  (define-key evil-motion-state-map [remap evil-goto-definition] #'dumb-jump-go)
  (setq dumb-jump-selector 'vertico))

(defun setup-swift-programming ()
  "Custom setting for swift programming."
  (setup-swift-mode-company)

  (load "swift-additions")
  (load "periphery-swiftlint")
  (load "periphery-loco")
  ;; (load "swift-querying")
  (load "localizeable-mode")

  (local-set-key (kbd "M-P") #'swift-additions:print-thing-at-point)
  (local-set-key (kbd "M-m") #'swift-additions:insert-mark)
  (local-set-key (kbd "C-M-t") #'swift-additions:insert-todo)
  (local-set-key (kbd "M-r") #'swift-additions:build-and-run-ios-app)
  (local-set-key (kbd "C-c C-a") #'swift-additions:analyze-using-periphery)
  (local-set-key (kbd "C-c C-c") #'swift-additions:compile-and-run-silent)
  (local-set-key (kbd "C-c C-t") #'swift-additions:test-module-silent)
  (local-set-key (kbd "C-c C-x") #'swift-additions:reset-settings)
  (local-set-key (kbd "M-s") #'swift-additions:terminate-all-running-apps)
  (local-set-key (kbd "M-K") #'swift-additions:clean-build-folder)
  (local-set-key (kbd "M-L") #'swift-additions:clear-xcodebuild-buffer)
  (local-set-key (kbd "M-b") #'swift-additions:build-ios-app)
  (local-set-key (kbd "C-c C-s") #'swift-additions:split-func-list)
  (local-set-key (kbd "C-c C-r") #'xcode-build:run)
  (local-set-key (kbd "C-x C-l") #'periphery-run-swiftlint)

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

;; Kill all other buffers
(defun kill-other-buffers ()
  "Kill all other buffer than current."
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

  (load "periphery-search")
  (local-set-key (kbd "C-c C-f") #'periphery-search-thing-at-point-rg)

  ;; Drag stuff
  (local-set-key (kbd "M-+") #'mk/toggle-flycheck-errors)
  (local-set-key (kbd "M-B") #'consult-projectile-switch-to-buffer)
  (local-set-key (kbd "C-M-B") #'projectile-switch-to-buffer-other-window)
  (local-set-key (kbd "C-M-K") #'kill-other-buffers)

  (hs-minor-mode)       ; Add support for folding code blocks
  (yas-global-mode 1)   ; Load our yassnippets
  (electric-pair-mode)  ; Auto insert pairs {} () [] etc

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

(defun mk/tabnine-off ()
  "Turn off TabNine for this buffer."
  (interactive)
  (setq-local company-backends (delete 'company-tabnine company-backends))
  (message "Turning off TabNine auto completion for current buffer"))

(defun mk/tabnine-on ()
  "Turn on TabNine for this buffer."
  (interactive)
  (setq-local company-backends (add-to-list 'company-backends 'company-tabnine))
  (message "Turning on TabNine auto completion for current buffer"))

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

(add-hook 'prog-mode-hook #'mk/setupProgrammingSettings)
(add-hook 'org-mode-hook #'mk/setupOrgMode)

(setq gc-cons-threshold (* 2 1024 1024))

(provide 'init)

;;; init.el ends here
