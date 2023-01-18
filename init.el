;;; init.el --- -*- lexical-binding: t -*-
;;; Code:
;; Window

(eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

(display-battery-mode t)		  ;; Show battery.
(display-time-mode t)			  ;; Show time.
(set-fringe-mode 0)               ;; Give us some space.
(fset 'yes-or-no-p 'y-or-n-p)     ;; Set yes or no to y/n
(global-auto-revert-mode 1)       ;; refresh a buffer if changed on disk
(global-hl-line-mode 1)           ;; Highlight current line
(save-place-mode 1)               ;; when buffer is closed, save the cursor position
(blink-cursor-mode 1)               ;; Blink cursor
(pixel-scroll-precision-mode 1)

;; Setup fonts
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono" :height 160)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font Mono" :height 160)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 160)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq ad-redefinition-action            'accept
      auto-mode-case-fold               nil
      auto-revert-check-vc-info         t
      backup-by-copying                 t
      backup-directory-alist            '(("." . "~/.emacs.d/backups"))
      bidi-display-reordering           nil
      blink-cursor-interval             0.6       ;; Little slower cursor blinking . default is 0.5
      cursor-in-non-selected-windows    nil
      byte-compile-warnings             '(ck-functions)
      confirm-kill-processes            nil
      create-lockfiles                  nil
      echo-keystrokes                   0.2
      ediff-split-window-function       'split-window-horizontally
      fast-but-imprecise-scrolling      t
      find-file-visit-truename          t
      font-lock-maximum-decoration      t
      highlight-nonselected-windows     t
      idle-update-delay                 1.1    ;; Speed things up by not updating so often
      jit-lock-defer-time               0.0
      kill-buffer-query-functions       nil    ;; Dont ask for closing spawned processes
      line-number-mode                  nil
      load-prefer-newer                 t
      ;; read-process-output-max           (* 8 1024 1024)
      scroll-margin                     4   ;; scroll N to screen edge
      use-dialog-box                    nil
      visible-bell                      nil
      word-wrap                         nil
      max-lisp-eval-depth 10000
      max-specpdl-size 10000
      auto-mode-case-fold nil
      truncate-string-ellipsis          "..."
      undo-limit                        6710886400 ;; 64mb
      undo-strong-limit                 100663296 ;; x 1.5 (96mb)
      undo-outer-limit                  1006632960) ;; x 10 (960mb), (Emacs uses x100), but this seems too high.

(setq-default display-line-numbers-width	4       ;; Set so we can display thousands of lines
              c-basic-offset                2            ;; Set tab indent for c/c++ to 4 tabs
              tab-width                     2            ;: Use four tabs
              line-spacing                  0.05         ;; Increase linespacing a bit
              truncate-lines                t
              indent-tabs-mode              nil			 ;; Never use tabs. Use spaces instead
              completion-ignore-case        t            ;; Ignore case when completing
              indent-line-function          'insert-tab  ;; Use function to insert tabs
              history-length                100)

(add-to-list 'load-path (concat user-emacs-directory "localpackages"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none
        dired-use-ls-dired nil
        ns-use-native-fullscreen nil
        browse-url-browser-function #'mk/browser-split-window)
  
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; (add-to-list 'default-frame-alist '(ns-use-native-fullscreen . nil))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
(put 'narrow-to-page 'disabled nil)

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

(defconst jetbrains-ligature-mode--ligatures
   '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
     "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
     "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
     "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
     "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
     "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
     "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
     "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
     ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
     "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
     "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
     "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
     "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
     "&="))

(sort jetbrains-ligature-mode--ligatures (lambda (x y) (> (length x) (length y))))

(dolist (pat jetbrains-ligature-mode--ligatures)
  (set-char-table-range composition-function-table
                      (aref pat 0)
                      (nconc (char-table-range composition-function-table (aref pat 0))
                             (list (vector (regexp-quote pat)
                                           0
                                    'compose-gstring-for-graphic)))))

(use-package use-package
  :ensure nil
  :config
  (setq use-package-verbose t
        use-package-expand-minimally t
        use-package-always-ensure t
        use-package-compute-statistics t
        use-package-minimum-reported-time 0.1
        debug-on-error nil))

(use-package no-littering)

(use-package autothemer
  :config
 ;; (load-theme 'catppuccin-latte t)
  ;; (load-theme 'catppuccin-frappe t)
   ;; (load-theme 'catppuccin-macchiato t)
   (load-theme 'catppuccin-mocha t)
   ;; (load-theme 'kman t)
  ;; (load-theme 'kanagawa t)
  ;; (load-theme 'doom-old-hope t)
  )

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 9
        vertico-multiline nil
        vertico-scroll-margin 4
        vertico-cycle t))

(use-package vertico-posframe
  :after vertico
  :init
  (vertico-posframe-mode 1)
  (vertico-posframe-cleanup)
  (setq vertico-posframe-parameters
        '((left-fringe . 1)
          (right-fringe . 1)))
  :config
  (setq
        ;; vertico-posframe-font "Iosevka Aile"
        ;; vertico-posframe-poshandler #'posframe-poshandler-frame-top-left-corner
        ;; vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
        ;; vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center
        vertico-posframe-poshandler #'posframe-poshandler-frame-center ;
        vertico-posframe-truncate-lines t
        vertico-posframe-width 160
        ;; vertico-posframe-height nil
        vertico-posframe-min-height 2
        vertico-posframe-border-width 2))

;; Configure directory extension.
(use-package vertico-directory
  :commands (find-file)
  :ensure nil
  :bind (:map vertico-map
              ("<tab>" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion flex))
                                        (eglot (styles . (orderless flex))))))

(use-package marginalia
  :after (vertico)
  :config (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ("C-s" . (lambda () (interactive) (consult-line (thing-at-point 'symbol))))
  ("M-l" . consult-goto-line)
  ("<backtab>" . consult-buffer)
  ("C-c C-a" . consult-apropos)
  ("M-f" . consult-line))

(use-package consult-ag
  :after consult)

(use-package consult-projectile
  :after projectile)

(use-package embark
  :after vertico
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
  :custom
  (setq auto-package-update-interval 7
        auto-package-update-prompt-before-update t
        auto-package-update-hide-results nil))

; Config and install modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 50
        doom-modeline-major-mode-icon nil
        doom-modeline-project-detection 'projectile
        doom-modeline-icon t
        doom-modeline-modal-icon nil
        doom-modeline-lsp t
        doom-modeline-hud nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-time-icon nil)
  (custom-set-faces
   '(mode-line ((t (:family "Iosevka Aile" :height 0.95))))
   '(mode-line-active ((t (:family "Iosevka Aile" :height 0.95)))) ; For 29+
   '(mode-line-inactive ((t (:family "Iosevka Aile" :height 0.90))))))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq
   dashboard-banner-logo-title "Mikaels dashboard!"
   dashboard-startup-banner (concat user-emacs-directory "themes/emacs.png")
   dashboard-center-content t
   dashboard-path-style 'truncate-beginning
   dashboard-set-file-icons t
   dashboard-projects-show-base 'align
   dashboard-recentf-show-base t
   dashboard-show-shortcuts nil
   dashboard-image-banner-max-height 300
   dashboard-set-init-info t
   dashboard-set-navigator t
   dashboard-projects-item-format "%s"
   dashboard-recentf-item-format "%s"
   dashboard-set-heading-icons nil
   dashboard-items '(
                     (recents . 8)
                     (projects . 2)
                     )
   ;; dashboard-navigator-buttons
   ;; `(;; line1
   ;;   ;; Keybindings
   ;;   ((,(all-the-icons-octicon "settings" :height 1.2 :v-adjust -0.1)
   ;;     "Settings\t" nil
   ;;     (lambda (&rest _) (open-config-file)) nil "" "\tSPC f e"))

   ;;   ((,(all-the-icons-octicon "search" :height 1.2 :v-adjust -0.1)
   ;;     "Find files" nil
   ;;     (lambda (&rest _) (find-file)) nil "" "\tSPC f f"))
     
   ;;   ((,(all-the-icons-octicon "file-binary" :height 1.2 :v-adjust -0.1)
   ;;     "Recent \t" nil
   ;;     (lambda (&rest _) (consult-recent-files)) nil "" "\tSPC f r"))
     
   ;;   ((,(all-the-icons-octicon "package" :height 1.2 :v-adjust -0.1)
   ;;     "Open project" nil
   ;;     (lambda (&rest _) (projectile-switch-project)) nil "" "\tSPC p s"))

   ;;   ((,(all-the-icons-octicon "file-text" :height 1.2 :v-adjust -0.1)
   ;;     "Scratch\t" nil
   ;;     (lambda (&rest _) (switch-to-buffer "*scratch*")) nil "" "\tSPC b s"))

   ;;   ((,(all-the-icons-octicon "bug" :height 1.2 :v-adjust -0.1)
   ;;     "Elfeed\t" nil
   ;;     (lambda (&rest _) (elfeed)) nil "" "\tSPC a a"))

   ;;   ((,(all-the-icons-octicon "dashboard" :height 1.2 :v-adjust -0.1)
   ;;     "Google\t" nil
   ;;     (lambda (&rest _) (google-this)) nil "" "\tSPC g g"))

   ;;   ((,(all-the-icons-octicon "calendar" :height 1.2 :v-adjust -0.1)
   ;;     "Calendar\t" nil
   ;;     (lambda (&rest _) (calendar)) nil "" "\tSPC c c"))
   ;;   )
   ))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package undo-fu
  :defer t)

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
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-want-C-i-jump nil)
  :config

  (define-key evil-visual-state-map (kbd "C-u") 'undo)
  (evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)

  (define-key evil-motion-state-map (kbd "C-M-<left>")  #'(lambda () (interactive) (xref-pop-marker-stack)))
  (define-key evil-motion-state-map (kbd "C-M-<right>") #'(lambda () (interactive) (xref-go-forward)))
  
  (define-key evil-motion-state-map (kbd "C-x C-b") #'(lambda () (interactive) (evil-show-marks nil)))

  ;; searching
  (define-key evil-motion-state-map (kbd "M-F") #'consult-git-grep)

  ;; window resizing
  (define-key evil-motion-state-map (kbd "C-+") #'enlarge-window-horizontally)
  (define-key evil-motion-state-map (kbd "C--") #'shrink-window-horizontally)
  (define-key evil-motion-state-map (kbd "C-M-+") #'enlarge-window)
  (define-key evil-motion-state-map (kbd "C-M--") #'shrink-window)

  (define-key evil-motion-state-map (kbd "C-w C-s") #'mk/split-window-below)
  (define-key evil-motion-state-map (kbd "C-w C-v") #'mk/split-window-right)
  (define-key evil-motion-state-map (kbd "C-w C-b") #'evil-split-buffer)

  (define-key evil-motion-state-map (kbd "M-R") #'consult-projectile-recentf)
  (define-key evil-motion-state-map (kbd "M-0") #'treemacs)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-ex-nohighlight)
  ;; (define-key evil-motion-state-map (kbd "<backtab>") #'consult-buffer)
  (define-key evil-motion-state-map (kbd "q") #'exit-minibuffer)
  (define-key evil-insert-state-map (kbd "TAB") #'tab-to-tab-stop)

  (add-to-list 'desktop-locals-to-save 'evil-markers-alist))

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

;; (use-package evil-lion
;;   :after evil
;;   :hook (prog-mode . evil-lion-mode))

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
  (setq evil-goggles-pulse t
        evil-goggles-duration 0.3)
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
  :hook ((org-mode . svg-tag-mode)
         (swift-mode . svg-tag-mode))
  :config
  (setq svg-tag-tags (periphery-svg-tags)))

(use-package dimmer
  :hook (prog-mode . dimmer-mode)
  ;; :bind ("M-s" . dimmer-mode)
  :config
  (dimmer-configure-org)
  (dimmer-configure-magit)
  (dimmer-configure-company-box)
  (dimmer-configure-posframe)
  (dimmer-configure-hydra)
  (setq dimmer-watch-frame-focus-events t
        dimmer-fraction 0.4)
  (add-to-list 'dimmer-exclusion-regexp-list "^\\**.*\\*$"))

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
  :after swift-mode
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; ------------------ SEARCHING -------------------
;; the silver searcher
(use-package ag
  :defer t
  :config
  (setq ag-project-root-function
        (lambda (d) (let ((default-directory d)) (projectile-project-root)))))

(use-package rg
  :defer t)

;; ------------------ EDITING -------------------
(use-package consult-project-extra
  :after consult
  :bind
  ("C-<tab>" . #'consult-projectile-switch-to-buffer))

(use-package consult-ls-git
  :after consult)

(use-package dash-docs
  :defer t)

 (use-package consult-dash
    :bind ("C-c C-i" . consult-dash)
    :config
    (setq consult-dash-docsets '("swift"))
    ;; Use the symbol at point as initial search term
    (consult-customize consult-dash :initial (thing-at-point 'symbol)))

(use-package google-this
  :commands (google-this)
  :bind ("C-x C-g" . google-this))

(use-package eglot
  :hook (swift-mode . eglot-ensure)
  :commands (eglot eglot-ensure)
  :ensure nil
  :config
  (setq eglot-stay-out-of '(corfu company)
        eglot-autoshutdown t
        eglot-events-buffer-size nil
        eglot-autoreconnect t
        eglot-send-changes-idle-time 0.5
        eglot-ignored-server-capabilities '(:hoverProvider))
        (add-to-list 'eglot-server-programs '(swift-mode . my-swift-mode:eglot-server-contact)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-use-icons nil)
  (kind-icon-blend-background t)
  (kind-icon-blend-frac 0.15)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu
  :ensure corfu-doc
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("<escape>" . corfu-quit)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
  :custom
  (completion-cycle-threshold nil)
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-scroll-margin 0)
  (corfu-preview-current t)
  (corfu-preselect-first t)
  (corfu-min-width 50)
  (completion-styles '(basic))
  :init
  (setq corfu-popupinfo-delay 0.5
        corfu-quit-no-match 'separator)
  (corfu-popupinfo-mode)
  ;; (corfu-indexed-mode)
  (global-corfu-mode))

(use-package corfu-history
  :ensure nil
  :after (corfu savehist)
  :config
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode t))

(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Add extensions
(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :custom
  (setq cape-dabbrev-check-other-buffers t
        cape-dabbrev-min-length 3)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package ace-jump-mode
  :commands (ace-jump-mode) 
  :bind ("M-g" . ace-jump-mode))

(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :bind ("M-J" . treemacs-find-file)
  :init (treemacs-project-follow-mode)
  :config
  (setq treemacs-follow-after-init t
        treemacs-collapse-dirs 1
        treemacs-directory-name-transformer #'identity
        treemacs-file-name-transformer #'identity
        treemacs-file-follow-delay 0.2
        treemacs-display-current-project-exclusively t
        treemacs-filewatch-mode t
        treemacs-follow-mode t
        treemacs-hide-dot-git-directory t
        treemacs-git-integration t
        treemacs-git-mode 'extended
        treemacs-indentation 1
        treemacs-is-never-other-window nil
        treemacs-silent-refresh	t
        treemacs-sorting 'treemacs--sort-alphabetic-case-insensitive-asc
        treemacs-width 35)
    
  )

(use-package treemacs-magit
  :after treemacs magit)
(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package restclient
  :commands (restclient))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :diminish t
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-display-errors-delay 0.2)
  (flycheck-check-syntax-automatically '(save idle-change))
  (flycheck-idle-change-delay 1.0))

(use-package flycheck-posframe
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  (setq flycheck-posframe-warning-prefix "⚠️ "
        flycheck-posframe-error-prefix "🚫️ "
        flycheck-posframe-info-prefix "‼️️"
        flycheck-posframe-position 'posframe-poshandler-frame-top-left-corner
        ))

;; (use-package flycheck-inline
;;   :hook (flycheck-mode . turn-on-flycheck-inline))

(use-package markdown-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :bind 
  ("M-O" . projectile-find-file-dwim)
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  :custom                               
  (setq projectile-completion-system 'auto
        projectile-enable-caching nil
        projectile-sort-order 'access-time
        projectile-indexing-method 'hybrid
        projectile-project-root-files '(".xcworkspace" ".projectile" ".xcodeproj")
        projectile-switch-project-action #'projectile-commander
        projectile-ignored-files '(".orig$" ".yml$"))
  :config
  (add-to-list 'projectile-globally-ignored-directories "build")
  (setq projectile-globally-ignored-directories
        '(".git"
          "swiftpm"
          "pods"
          "xcodeproj"
          ".build")))

(use-package gcmh
  :config
  (gcmh-mode 1))

;; Restart emacs
(use-package restart-emacs
  :commands restart-emacs)

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
     ("\\*occur\\|evil-marks\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.18)
      (side . bottom)
      (slot . 1))
     ("\\*xcodebuild\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.2)
      (window-width . 0.3)
      (side . bottom)
      (slot . 0))
     ("\\*Periphery\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.3)
      (window-width . 0.7)
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
  :config 
  (setq winum-auto-setup-mode-line t)
  :init
  (winum-mode 1))

;; darkroom (go to focus mode)
(use-package darkroom
  :bind ("C-x C-d" . darkroom-tentative-mode)
  :config
  (setq darkroom-text-scale-increase 2.5
        darkroom-margins 0.1))

;; Use git
(use-package magit
  :commands (magit-status magit-ediff-show-working-tree)
  :bind ("C-c C-d" . magit-ediff-show-working-tree)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 10
        magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*"))
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME" "HACK"))))

(use-package blamer
  :commands (blamer-mode)
  :config
  (setq blamer-view 'overlay-right
        blamer-type 'visual
        blamer-max-commit-message-length 70
        blamer-force-truncate-long-line nil
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
    "aa" '(lambda () (interactive) (elfeed) :which-key "Elfeed"))
  
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
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-display-inline-images))
  :config
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-startup-folded t
        org-hide-leading-stars t
        org-src-edit-src-content-indentation 0
        org-log-into-drawer t
        org-clock-sound "~/.emacs.d/etc/sound/bell.mp3"
        org-log-done 'time))

(defun mk/play-sound (orgin-fn sound)
  (cl-destructuring-bind (_ _ file) sound
    (make-process :name (concat "play-sound-" file)
                  :connection-type 'pipe
                  :command `("afplay" ,file))))
(advice-add 'play-sound :around 'mk/play-sound)

(defun mk/org-timer-update-mode-line ()
  "Update the timer time in the mode line."
  (if org-timer-pause-time
      nil
    (setq org-timer-mode-line-string
	  (concat "🍅 " (substring (org-timer-value-string) 0 -1) ""))
    (force-mode-line-update)))

(with-eval-after-load 'org
  (mk/org-mode-setup)

  (advice-add 'org-timer-update-mode-line :override #'mk/org-timer-update-mode-line)
  
  (setq org-confirm-babel-evaluate nil)
  (require 'org-tempo)

  ;; Setup fonts for org-mode
  (set-face-attribute 'org-block nil                :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil                :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil              :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil                 :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil                :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil             :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil            :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil             :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil              :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp t)
                                 (swift t)
                                 (swiftui t)))

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sw" . "src swift"))
  (add-to-list 'org-structure-template-alist '("swiftui" . "src swiftui :view CustomView"))
  (add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))
  (add-to-list 'org-modules 'org-tempo t))

(use-package ob-swift
  :defer t)
  
(use-package ob-swiftui
  :defer t
  :config
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))
  (add-to-list 'org-babel-tangle-lang-exts
               '("swiftui" . "swift"))
  (add-to-list 'org-src-lang-modes
               '("swiftui" . swift)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 130
        visual-fill-column-center-text t))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds '(
                       ("https://news.ycombinator.com/rss")
                       ("http://nullprogram.com/feed/")
                       ("https://planet.emacslife.com/atom.xml")
                       ("https://www.reddit.com/r/emacs.rss")
                       ("https://www.reddit.com/r/swift.rss")
                       ("https://www.reddit.com/r/swiftui.rss")
                       ("https://xenodium.com/rss")
                       ("https://swiftbysundell.com/rss")
                       )
        elfeed-search-filter "@7-days-ago +unread"
        elfeed-search-title-max-width 100
        elfeed-search-title-min-width 100))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (highlight-indent-guides-method #'bitmap))

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
  :ensure nil)

(use-package yasnippet
  :init
  (yas-global-mode))

(use-package swift-mode
  :config
  (setq swift-mode:basic-offset 2
        swift-mode:parenthesized-expression-offset 2
	    swift-mode:multiline-statement-offset 2))

(use-package swift-additions
  :ensure nil
  :after swift-mode
  :bind
  ("C-c C-t" .  #'swift-additions:test-module-silent)
  ("C-c C-s" .  #'swift-additions:split-func-list)
  ("M-L" .  #'swift-additions:clean-build-folder)
  ("M-P" .  #'swift-additions:print-thing-at-point)
  ("M-t" . #'swift-additions:insert-todo)
  ("M-m" . #'swift-additions:insert-mark)
  ("M-s" . #'ios-simulator:terminate-current-app)
  ("C-c C-c" . #'swift-additions:compile-and-run-silent)
  ("M-r" . #'swift-additions:run-without-compiling)
  ("C-c C-x" . #'swift-additions:reset-settings))

(use-package apple-docs-query
  :ensure nil
  :after swift-mode
  :bind
  ("C-c C-a" . #'apple-docs/query)
  ("C-c C-A" . #'apple-docs/query-thing-at-point))

(use-package hacking-with-swift
  :ensure nil
  :after swift-mode
  :bind
  ("C-c C-h" . #'hacking-ws/query)
  ("C-c C-H" . #'hacking-ws/query-thing-at-point))

(use-package periphery-search
  :ensure nil
  :after prog-mode
  :bind
  ("C-c C-f" . #'periphery-search-dwiw-rg)
  ("M-f" . #'periphery-search-dwiw-rg)
  ("C-x C-t" . #'periphery-query-todos-and-fixmes)
  ("C-x C-m" . #'periphery-query-marks))

(use-package periphery-swiftformat
  :ensure nil
  :after swift-mode
  :bind
  ("C-c C-o" . #'periphery-swiftformat-lint-buffer)
  ("M-o" . #'periphery-swiftformat-autocorrect-buffer)
  ("C-c C-p" . #'periphery-run-swiftformat-for-project))

(use-package periphery-loco
  :ensure nil
  :after swift-mode
  :bind
  ("C-c C-k" . #'periphery-run-loco))

(use-package periphery-swiftlint
  :ensure nil
  :after swift-mode
  :bind
  ("C-c C-l" . #'periphery-run-swiftlint))

(defun setup-swift-programming ()
  "Custom setting for swift programming."
  
  (define-key swift-mode-map (kbd "C-c C-f") #'periphery-search-dwiw-rg)
  (setq tree-sitter-hl-use-font-lock-keywords t)

  (use-package flycheck-swift3
    :after flycheck
    :custom (flycheck-swift3-setup))
  
  (use-package flycheck-swiftlint
    :after flycheck
    :custom (flycheck-swiftlint-setup))

  (add-to-list 'flycheck-checkers 'swift3)
  (add-to-list 'flycheck-checkers 'swiftlint)
  (flycheck-add-next-checker 'swiftlint 'swift3)

  (defun mk/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-super-capf #'eglot-completion-at-point
                                       #'cape-dabbrev
                                       #'cape-line
                                       #'cape-file
                                       ;; (cape-company-to-capf #'company-yasnippet)
                                       ))))

  (add-hook 'eglot-managed-mode-hook #'mk/eglot-capf))

(defun mk/org-mode-setup()
  "Setup 'org-mode'."
  (setq-local completion-at-point-functions
              (list (cape-super-capf #'cape-ispell
                                     #'cape-file
                                     #'cape-dabbrev
                                     ;; (cape-company-to-capf #'company-yasnippet)
                                     )))
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode t)
  (setq evil-auto-indent nil))

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

  (local-set-key (kbd "C-c C-g") #'isearch-forward-thing-at-point)
  (local-set-key (kbd "M-+") #'mk/toggle-flycheck-errors)
  (local-set-key (kbd "C-M-B") #'projectile-switch-to-buffer-other-window)

  (hs-minor-mode)       ; Add support for folding code blocks
  (electric-pair-mode)  ; Auto insert pairs {} () [] etc
  ;; (global-hl-todo-mode t)

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

(defun mk/recompile (&optional force)
  "Recompile files (as FORCE) force compilation."
  (interactive "p")
  (byte-recompile-directory (locate-user-emacs-file "localpackages") 0)
  (byte-recompile-directory (locate-user-emacs-file "themes") 0))

(add-hook 'prog-mode-hook #'mk/setupProgrammingSettings)

(with-eval-after-load 'swift-mode
  (setup-swift-programming))

;; (setq gc-cons-threshold (* 2 1024 1024))

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
(put 'narrow-to-region 'disabled nil)
