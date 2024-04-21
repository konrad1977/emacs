;;; init.el --- -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (defvar display-time-24hr-format t))
(eval-when-compile (defvar display-time-default-load-average nil))

(set-face-attribute 'default nil
                    :font "JetBrainsMono Nerd Font Mono"
                    :height 150
                    :weight 'thin
                    :inverse-video nil
                    :slant 'normal)


(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font Mono" :height 150 :weight 'thin)
(set-face-attribute 'variable-pitch nil :font "Work Sans" :height 150 :weight 'light)

(custom-set-faces
 `(font-lock-comment-face ((t (:font "Work Sans" :italic t :height 1.05)))))

(display-battery-mode t)        ;; Show battery.
(display-time-mode t)           ;; Show time.
(fset 'yes-or-no-p 'y-or-n-p)   ;; Set yes or no to y/n
(global-auto-revert-mode)       ;; refresh a buffer if changed on disk
(global-hl-line-mode 1)         ;; Highlight current line
;; (pixel-scroll-precision-mode 1)

(setq-default display-line-numbers-width    5       ;; Set so we can display thousands of lines
              c-basic-offset                4            ;; Set tab indent for c/c++ to 4 tabs
              ediff-forward-word-function   'forward-char
              ediff-split-window-function   'split-window-horizontally
              fringe-indicator-alist        (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist)
              tab-width                     4            ;: Use four tabs
              indent-tabs-mode              nil			 ;; Never use tabs. Use spaces instead
              indent-line-function          'insert-tab  ;; Use function to insert tabs
              history-length                100)

(let* ((dir (expand-file-name (concat user-emacs-directory "localpackages")))
       (default-directory dir))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)
  (set-fontset-font t nil "SF Pro Display" nil 'append)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none
        dired-use-ls-dired nil
        browse-url-browser-function #'mk/browser-split-window))
(put 'narrow-to-page 'disabled nil)

;; Dont leave #file autosaves everywhere I go
(defvar my-auto-save-folder (concat user-emacs-directory "var/auto-save/"))
(setq auto-save-list-file-prefix (concat my-auto-save-folder ".saves-")
      auto-save-file-name-transforms `((".*", my-auto-save-folder t))
      custom-file (concat user-emacs-directory "var/custom.el"))

;; Initialize package sources
(require 'package)
(require 'use-package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(setopt package-install-upgrade-built-in t)

(use-package use-package
  :ensure nil
  :config
  (setq use-package-verbose nil
        use-package-expand-minimally t
        use-package-always-ensure t
        use-package-compute-statistics nil
        use-package-minimum-reported-time 0.2))

(use-package gcmh
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode))))
(use-package emacs
  :config
  (setq completion-cycle-threshold 3
        max-lisp-eval-depth 10000
        scroll-conservatively 101
        scroll-margin 0
        debug-on-error nil
        visible-bell nil
        auth-sources '((:source "~/.authinfo.gpg"))
        warning-minimum-level :emergency
        tab-always-indent 'complete
        ad-redefinition-action            'accept
        auto-revert-check-vc-info         t
        backup-by-copying                 t
        backup-directory-alist            '(("." . "~/.emacs.d/backups"))
        cursor-in-non-selected-windows    nil
        byte-compile-warnings             '(ck-functions)
        confirm-kill-processes            nil
        create-lockfiles                  nil
        echo-keystrokes                   0.2
        confirm-kill-emacs                'y-or-n-p
        find-file-visit-truename          t
        font-lock-maximum-decoration      t
        highlight-nonselected-windows     t
        fast-but-imprecise-scrolling      t
        kill-buffer-query-functions       nil    ;; Dont ask for closing spawned processes
        use-dialog-box                    nil
        word-wrap                         nil
        auto-mode-case-fold               nil
        bidi-inhibit-bpa                  t
        bidi-display-reordering           'left-to-right
        bidi-paragraph-direction          'left-to-right
        undo-limit                        6710886400 ;; 64mb
        undo-strong-limit                 100663296 ;; x 1.5 (96mb)
        undo-outer-limit                  1006632960 ;; x 10 (960mb), (Emacs uses x100), but this seems too high.
        jit-lock-defer-time 0))

(when (fboundp 'set-message-beep)
  (set-message-beep 'silent))

(use-package spinner :defer t)
(use-package request :defer t)
(use-package async :defer t)
(use-package all-the-icons :defer t)
(use-package rg :defer t)

(use-package nerd-icons
  :defer t
  :custom
  (setq nerd-icons-scale-factor 1.2))

(use-package ligature
  :hook (prog-mode . ligature-mode)
  :defer t
  :config
  (ligature-set-ligatures
   'prog-mode
   '(
     (";" (rx (+ ";")))
     ("&" (rx (+ "&")))
     ("%" (rx (+ "%")))
     ("?" (rx (or ":" "=" "\." (+ "?"))))
     ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
     ("\\" (rx (or "/" (+ "\\"))))
     ("+" (rx (or ">" (+ "+"))))
     (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
     ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!" "="))))
     ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
     ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]" "-" "=" ))))
     ("*" (rx (or ">" "/" ")" (+ "*"))))
     ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
     ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
     ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_" (+ "#"))))
     (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
     ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!" "-"  "/" "|" "="))))
     ("_" (rx (+ (or "_" "|"))))
     ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
     "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="
     ("w" (rx (+ "w")))
     ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
     "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft")))

(setq font-lock-maximum-decoration '((swift-mode . 2) (c++-mode . 2) (t . t)))

(use-package welcome-dashboard
  :ensure nil
  :custom-face
  (welcome-dashboard-path-face ((t (:height 0.8))))
  :config
  (setq welcome-dashboard-latitude 56.7365
        welcome-dashboard-longitude 16.2981
        welcome-dashboard-use-nerd-icons t
        welcome-dashboard-show-weather-info t
        welcome-dashboard-use-fahrenheit nil
        welcome-dashboard-path-max-length 50
        welcome-dashboard-min-left-padding 10
        welcome-dashboard-image-file "~/.emacs.d/themes/true.png"
        welcome-dashboard-image-width 200
        welcome-dashboard-image-height 169
        welcome-dashboard-title "Welcome Mikael. Have a great day!")
  (welcome-dashboard-create-welcome-hook))

(use-package no-littering)

(use-package autothemer
  :config
 ;; (load-theme 'catppuccin-latte t)
  ;; (load-theme 'catppuccin-frappe t)
   ;; (load-theme 'catppuccin-macchiato t)
  ;; (load-theme 'catppuccin-mocha t)
  ;; (load-theme 'rose-pine t)
    ;; (load-theme 'oxocarbon t)
   ;;(load-theme 'oxographite t)
   ;; (load-theme 'kman t)

  ;; (load-theme 'kalmar-night t)
  (load-theme 'kanagawa t))

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
  :custom
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  :config
  ;; (vertico-multiform-mode)
  (setq vertico-resize t
        vertico-count 12
        vertico-multiline nil
        vertico-scroll-margin 4
        vertico-cycle t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t))

(use-package vertico-posframe
  :after vertico
  :init
  (vertico-posframe-mode 1)
  (vertico-posframe-cleanup)
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 0)
          (right-fringe . 0))
        vertico-posframe-poshandler #'posframe-poshandler-frame-top-center
        vertico-posframe-truncate-lines t
        vertico-posframe-min-width 10
        vertico-posframe-width 150
        vertico-posframe-min-height 2
        vertico-posframe-border-width 24))

;; Configure directory extension.
(use-package vertico-directory
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :commands (find-file)
  :ensure nil
  :bind (:map vertico-map
              ("<tab>" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))


(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package orderless
  :after vertico
  :init
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-styles '(orderless flex)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (orderless flex)))
                                        (eglot (styles . (orderless flex))))))

(use-package marginalia
  :after (vertico)
  :config (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu)

;; (use-package multiple-cursors
;;   :hook (prog-mode . multiple-cursors-mode)
;;   :bind
;;   ("M-j" . 'mc/mark-all-dwim)
;;   ("C-M-c" . 'mc/edit-lines))

;; (global-unset-key (kbd "M-<down-mouse-1>"))
;; (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ("C-s" . (lambda () (interactive) (consult-line (thing-at-point 'symbol))))
  ("M-S" . #'consult-line)
  ("M-f" . #'consult-ripgrep)
  ;; ("M-l" . #'consult-goto-line)
  ("<backtab>" . #'consult-buffer)
  ("C-c C-a" . #'consult-apropos)
  ("C-c m m" . #'consult-imenu-multi)
  ("C-c m b" . #'consult-imenu)
  ;; ("M-O" . #'consult-projectile-find-file)
  )

(use-package embark-consult
  :after (embark consult))

(use-package consult-project-extra
  :after consult
  :bind
  ("C-<tab>" . #'consult-projectile-switch-to-buffer))

(use-package consult-ls-git
  :after consult)

(use-package consult-projectile
  :after projectile)

(use-package recentf
  :hook (after-init . recentf-mode))

(use-package mode-line-hud
  :ensure nil
  :config
  (setq show-in-echo-area nil))

(defun mk/check-mode-active (mode)
  "Check if MODE is active."
  (bound-and-true-p (symbol-value mode)))

(defun mk/hud-copilot ()
  "HUD for Copilot."
  (if (bound-and-true-p copilot-mode)
      (concat (propertize "" 'face '(:inherit success)) " Copilot")
    ""))

(defun mk/mood-separator ()
  "Mood line separator."
  (propertize " ⑊ " 'face '(:foreground "#5f5f6f")))

(defun mk/add-mood-line-segment (segment)
  "Add SEGMENT to mood line."
  (when segment
    (concat segment (mk/mood-separator))))

(use-package mood-line
  :config
  (setq mood-line-format
      (mood-line-defformat
       :left
       (((file-name-sans-extension (mood-line-segment-buffer-name))   . "|")
        ((concat (mood-line-segment-major-mode) (mk/mood-separator)) . "")
        ((mk/add-mood-line-segment (mood-line-segment-vc)) . "")
        )
       :right
        (
        ((mk/add-mood-line-segment (mood-line-segment-hud)) . "")
        ((mk/add-mood-line-segment (mood-line-segment-client)) . "")
        ((mk/add-mood-line-segment (mood-line-segment-process)) . "")
        ((mk/add-mood-line-segment (mood-line-segment-project)) . "")
        ((mk/add-mood-line-segment (mood-line-segment-anzu)) . "")
        ((mk/add-mood-line-segment (mood-line-segment-checker)) . "")
        ((mk/add-mood-line-segment (mk/hud-copilot)) . "")
        (display-time-string . " "))))
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ("C-x C-c" . describe-char)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package evil
  :hook (after-init . evil-mode)
  ;; :bind ("<escape>" . keyboard-escape-quit)
  :init
  (setq-default evil-symbol-word-search t
                evil-shift-width 2)
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-minibuffer t
        evil-undo-system 'undo-fu
        evil-want-fine-undo t
        evil-want-C-u-scroll t
        evil-search-module 'evil-search
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-want-C-i-jump nil)
  :config
  (evil-ex-define-cmd "q[uit]" 'kill-buffer-and-window)

  (define-key evil-visual-state-map (kbd "u") 'undo)

  (define-key evil-motion-state-map [remap evil-goto-definition] #'dumb-jump-go)
  (define-key evil-motion-state-map (kbd "C-M-<left>")  #'(lambda () (interactive) (evil-jump-backward)))
  (define-key evil-motion-state-map (kbd "C-M-<right>") #'(lambda () (interactive) (evil-jump-forward)))

  (define-key evil-motion-state-map (kbd "<up>") 'ignore)
  (define-key evil-motion-state-map (kbd "<down>") 'ignore)
  (define-key evil-motion-state-map (kbd "<left>") 'ignore)
  (define-key evil-motion-state-map (kbd "<right>") 'ignore)


  (define-key evil-motion-state-map (kbd "C-x C-b") #'(lambda () (interactive) (evil-show-marks nil)))

  ;; window resizing
  (define-key evil-motion-state-map (kbd "C-+") #'(lambda () (interactive) (enlarge-window-horizontally 3)))
  (define-key evil-motion-state-map (kbd "C--") #'(lambda () (interactive) (shrink-window-horizontally 3)))
  (define-key evil-motion-state-map (kbd "C-M-+") #'(lambda () (interactive) (enlarge-window 3)))
  (define-key evil-motion-state-map (kbd "C-M--") #'(lambda () (interactive) (shrink-window 3)))

  (define-key evil-motion-state-map (kbd "C-w C-s") #'mk/split-window-below)
  (define-key evil-motion-state-map (kbd "C-w C-v") #'mk/split-window-right)
  (define-key evil-motion-state-map (kbd "C-w C-b") #'evil-split-buffer)

  (define-key evil-motion-state-map (kbd "M-R") #'consult-projectile-recentf)
  (define-key evil-motion-state-map (kbd "M-0") #'treemacs)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-ex-nohighlight)
  (define-key evil-normal-state-map (kbd "<escape>") #'evil-ex-nohighlight)
  
  (define-key evil-motion-state-map (kbd "q") #'exit-minibuffer)
  (define-key evil-insert-state-map (kbd "TAB") #'tab-to-tab-stop)

  (add-to-list 'desktop-locals-to-save 'evil-markers-alist)

  (setq evil-normal-state-tag   (propertize "NORMAL" 'face '((:background "green" :foreground "black")))
        evil-emacs-state-tag    (propertize "EMACS" 'face '((:background "orange" :foreground "black")))
        evil-insert-state-tag   (propertize "INSERT" 'face '((:background "red") :foreground "white"))
        evil-motion-state-tag   (propertize "MOTION" 'face '((:background "blue") :foreground "white"))
        evil-visual-state-tag   (propertize "VISUAL" 'face '((:background "grey80" :foreground "black")))
        evil-operator-state-tag (propertize "OPERATOR" 'face '((:background "purple"))))

  (setq evil-normal-state-cursor '(box "#41a7fc")
        evil-insert-state-cursor '(bar "#FF5D62")
        evil-visual-state-cursor '(hollow "#FF5D62")))

(use-package evil-args
  :after evil
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  ;; (define-key evil-normal-state-map "L" 'evil-forward-arg)
  ;; (define-key evil-normal-state-map "H" 'evil-backward-arg)
  ;; (define-key evil-motion-state-map "L" 'evil-forward-arg)
  ;; (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(use-package evil-collection
  :after evil
  :custom
  (setq evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))

(use-package evil-mc
  :hook (evil-mode . global-evil-mc-mode)
  :bind
  ("C-g" . evil-mc-undo-all-cursors)
  ("<escape>" . evil-mc-undo-all-cursors)
  ("C-M-<down>" . (lambda () (interactive) (evil-mc-make-cursor-move-next-line 1)))
  ("C-M-<up>" . (lambda () (interactive) (evil-mc-make-cursor-move-prev-line 1)))
  ("C-c a" . (lambda () (interactive) (evil-mc-make-cursor-here)))
  ("C-M-e" . (lambda() (interactive) (evil-mc-make-all-cursors)))
  :custom
  (setq evil-mc-mode-line-text-inverse-colors t
        evil-mc-undo-cursors-on-keyboard-quit t
        evil-mc-mode-line-text-cursor-color t)
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg))

;; (use-package evil-iedit-state
;;   :after evil
;;   :config
;;   (setq iedit-only-at-symbol-boundaries t)
;;   :bind
;;   ("C-M-e" . evil-iedit-state/iedit-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode 1))

(use-package evil-quickscope
  :after evil
  :hook (prog-mode . evil-quickscope-mode))

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
  :config
  (setq undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :hook (after-init . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package minimap
  :commands (minimap-mode)
  :config
  (setq
        minimap-width-fraction 0.0
        minimap-minimum-width 10
        minimap-always-recenter nil
        minimap-hide-fringes t
        minimap-dedicated-window t
        minimap-enlarge-certain-faces nil
        minimap-recenter-type 'relative
        minimap-window-location 'right)
  :custom-face
  (minimap-font-face ((t (:family "Minimap" :height 0.17 :group 'minimap)))))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tree-sitter
  :hook (
         (swift-mode . tree-sitter-mode)
         (kotlin-mode . tree-sitter-mode))
  :config
  (setq tsc-dyn-get-from nil)
  (setq tree-sitter-hl-use-font-lock-keywords t
        tree-sitter-hl-enable-query-region-extension nil)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :defer t
  :ensure nil)

;; ------------------ SEARCHING -------------------

;; ------------------ EDITING -------------------
(use-package google-this
  :commands (google-this)
  :bind ("C-x C-g" . google-this))

(use-package eglot
  :hook ((swift-mode . eglot-ensure))
  :commands (eglot eglot-ensure)
  :ensure nil
  :config
  (setq eglot-stay-out-of '(corfu company)
        ;; eglot-send-changes-idle-time 0.1
        eglot-autoshutdown t
        eglot-events-buffer-config 0
        eglot-ignored-server-capabilities '(:hoverProvider)
        eglot-extend-to-xref t)
  (advice-add 'jsonrpc--log-event :override #'ignore)
  (add-to-list 'eglot-server-programs '(swift-mode . my-swift-mode:eglot-server-contact)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-extra-space t)
  (kind-icon-blend-background t)
  (kind-icon-blend-frac 0.20)
  :config
  (setq kind-icon-default-face 'corfu-default ; to compute blended backgrounds correctly
        kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.7 :scale 1.2))
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

(use-package corfu
  :hook ((prog-mode . corfu-mode)
         (localizeable-mode . corfu-mode)
         (org-mode . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
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
  (setq corfu-bar-width 10
        corfu-scroll-margin 2
        corfu-preselect-first t
        corfu-quit-at-boundary t
        corfu-auto-prefix 1
        corfu-min-width 40
        corfu-max-width 140
        corfu-left-margin-width 0.8
        corfu-right-margin-width 0.8
        corfu-count 15
        corfu-auto-delay 0.3
        corfu-quit-no-match 'separator
        corfu-popupinfo-resize t
        ;; corfu-popupinfo-hide t
        corfu-popupinfo-direction '(force-horizontal)
        corfu-popupinfo-min-width corfu-min-width
        corfu-popupinfo-max-width corfu-max-width))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode t))

;; (use-package yasnippet-capf
;;   :after cape
;;   :init
;;   (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; Add extensions
(use-package cape
  :after evil
  :defer t
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
  ;; :custom
  ;; (setq cape-dabbrev-check-other-buffers t
  ;;       cape-dabbrev-min-length 2)
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (advice-add #'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package avy
  :defer t
  :bind ("M-g" . avy-goto-word-1)
  :config
  (setq avy-single-candidate-jump t))

(use-package dape
  :commands (dape)
  :bind ("C-x C-d" . dape)
  :init
  (setq dape-buffer-window-arrangement 'right
        dape-stack-trace-levels 10)
  :config
  (remove-hook 'dape-on-start-hooks 'dape-repl)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)
  (add-to-list 'dape-configs
             `(ios
               modes (swift-mode)
               command-cwd dape-command-cwd
               command ,(file-name-concat dape-adapter-dir
                                          "codelldb"
                                          "extension"
                                          "adapter"
                                          "codelldb")
               command-args ("--port" :autoport
                             "--settings" "{\"sourceLanguages\":[\"swift\"]}"
                             "--liblldb" "/Applications/Xcode.app/Contents/SharedFrameworks/LLDB.framework/Versions/A/LLDB")
               port :autoport
               simulator-id "iPhone 14 crash"
               app-bundle-id "se.mobileinteraction.greenfee-qa"
               fn (dape-config-autoport
                   ,(lambda (config)
                      (with-temp-buffer
                        (let* ((command
                                (format "xcrun simctl launch --wait-for-debugger --terminate-running-process %S %S"
                                        (plist-get config 'simulator-id)
                                        (plist-get config 'app-bundle-id)))
                               (code (call-process-shell-command command nil (current-buffer))))
                          (dape--repl-message (format "* Running: %S *" command))
                          (dape--repl-message (buffer-string))
                          (save-match-data
                            (if (and (zerop code)
                                     (progn (goto-char (point-min))
                                            (search-forward-regexp "[[:digit:]]+" nil t)))
                                (plist-put config :pid (string-to-number (match-string 0)))
                              (dape--repl-message (format "* Running: %S *" command))
                              (dape--repl-message (format "Failed to start simulator:\n%s" (buffer-string)))
                              (user-error "Failed to start simulator")))))
                      config))
               :type "lldb"
               :request "attach"
               :cwd ".")))

(use-package repeat
  :ensure nil
  :hook (prog-mode . repeat-mode)
  :custom
  (repeat-too-dangerous '(kill-this-buffer))
  (repeat-exit-timeout 5))

(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :hook (treemacs-mode . treemacs-project-follow-mode)
  :bind (("M-J" . treemacs-find-file))
  :custom-face
  (treemacs-directory-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-directory-collapsed-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-ignored-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-unmodified-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-untracked-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-added-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-renamed-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-git-modified-face ((t (:family "SF Pro Display" :height 0.9))))
  (treemacs-tags-face ((t (:family "SF Pro Display" :height 0.9))))
  :config
  (setq treemacs-follow-after-init t
        treemacs-collapse-dirs 1
        treemacs-directory-name-transformer #'identity
        treemacs-file-name-transformer #'identity
        treemacs-show-cursor nil
        treemacs-display-current-project-exclusively t
        treemacs-filewatch-mode t
        treemacs-follow-mode nil
        treemacs-hide-dot-git-directory t
        treemacs-git-integration t
        treemacs-space-between-root-nodes t
        treemacs-hide-gitignored-files-mode t
        treemacs-git-mode 'extended
        treemacs-indentation 1
        treemacs-silent-refresh	t
        treemacs-sorting 'treemacs--sort-alphabetic-case-insensitive-asc
        treemacs-width 32))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package project-treemacs
  :after treemacs)

(use-package restclient
  :commands (restclient))

(use-package flyspell
  :ensure nil
  :config
  (setq flyspell-issue-message-flag nil
      ispell-local-dictionary "sv-SE"
      ispell-program-name "aspell"))

(use-package flycheck
 :hook (prog-mode . flycheck-mode)
 :diminish t
 :bind
  ("C-c e n" . flycheck-next-error)
  ("C-c e p" . flycheck-previous-error)
  :custom
  (setq flycheck-checker-error-threshold 20))

(use-package flycheck-inline
  :ensure nil
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(use-package flycheck-eglot
  :hook (swift-mode . global-flycheck-eglot-mode)
  :after flycheck
  :config
  (setq flycheck-eglot-exclusive t))

(use-package markdown-mode
  :commands (markdown-mode))

(use-package yaml-mode
  :commands (yaml-mode))

(use-package json-mode
  :commands (json-mode))

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :bind
  ("C-M-r" . projectile-replace)
  :custom
  (setq projectile-completion-system 'auto
        projectile-enable-caching t
        projectile-sort-order 'default
        projectile-indexing-method 'hybrid
        projectile-verbose nil
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

(use-package window
  :ensure nil
  :bind
  ("C-x C-f" . toggle-frame-fullscreen)
  ("C-x C-s" . window-toggle-side-windows)
  ("C-x C-x" . kill-buffer-and-window)
  :custom
  (display-buffer-alist
   '(("\\*Async Shell Command\\*"
      (display-buffer-no-window))
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
     ("\\*iOS Simulator\\|*swift package\\|*ios-device"
      (display-buffer-in-side-window display-buffer-reuse-window)
      (window-height . 0.2)
      (window-parameters . ((mode-line-format . none)))
      (side . bottom)
      (slot . 2))
     ("\\*Periphery\\*"
      (display-buffer-in-side-window display-buffer-reuse-window)
      (body-function . select-window)
      (window-height . 0.2)
      (side . bottom)
      (slot . 1))
     ("\\*Faces\\|[Hh]elp\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-width . 0.4)
      (side . right)
      (slot . 1))
     ("\\*e?shell\\|vterm*\\|*ellama\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . 0.25)
      (window-parameters . ((mode-line-format . none)))
      (side . bottom)
      (slot . 10))
     ("\\*Flycheck\\|[Cc]olors\\*\\|Warnings"
      (display-buffer-in-side-window display-buffer-reuse-window)
      (display-buffer-at-bottom)
      (window-height . 0.15)
      (side . bottom)
      (slot . 3)))))

(use-package darkroom
  :commands (darkroom-tentative-mode)
  ;; :bind ("C-x C-d" . darkroom-tentative-mode)
  :config
  (setq darkroom-text-scale-increase 1.5
        darkroom-margins '(15 . 0)))

;; Use git
(use-package magit
  :commands (magit-status magit-ediff-show-working-tree)
  :bind ("C-c C-d" . magit-ediff-show-working-tree)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package blamer
  :commands (blamer-mode)
  :config
  (setq blamer-tooltip-function 'blamer-tooltip-author-info)
  (setq blamer-view 'overlay
        blamer-type 'both
        ;; blamer-max-commit-message-length 70
        ;; blamer-force-truncate-long-line nil
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
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :config
  (setq git-gutter:update-interval 2))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (setq git-gutter-fr:side 'left-fringe)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated)))

(use-package vterm
  :commands vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (setq vterm-timer-delay nil))

(defun project-root-override (dir)
  "Find DIR's project root by searching for a '.project.el' file."
  (let ((root (or (locate-dominating-file dir ".xcodeproj")
                  (locate-dominating-file dir ".envrc")))
        (backend (ignore-errors (vc-responsible-backend dir))))
    (when root (if (version<= emacs-version "28")
                    (cons 'vc root)
                 (list 'vc backend root)))))

(use-package project
  :ensure nil
  :config
  (add-hook 'project-find-functions #'project-root-override))

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
    "tt" '(lambda () (interactive) (mk/toggle-transparency) :which-key "Toggle transparenty"))

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
    "ps" '(project-switch-project :which-key "Switch project")
    "pS" '(projectile-switch-open-project :which-key "Switch open project"))

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
  (setq org-ellipsis " ▾"
        org-auto-align-tags nil
        org-tags-column 0
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-with-inline-images t
        org-startup-folded nil
        org-directory "~/Desktop/org/Todo/"
        org-agenda-files (list "~/Desktop/org/Todo/")
        ;; org-agenda-files '("work.org" "projekt.org")
        org-hide-leading-stars t
        org-src-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "STARTED(s)" "DELEGATED(d)" "QA(q)" "|" "DONE(d)" "CANCELLED(c)"))
        org-log-into-drawer t
        org-todo-keyword-faces org-custom-todo-faces
        org-clock-sound "~/.emacs.d/etc/sound/bell.mp3"
        org-log-done 'time
        org-fontify-emphasized-text t
        org-fontify-whole-heading-line t
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-cycle-separator-lines 2
        org-return-follows-link t
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────"))

(use-package restclient
  :defer t)

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

  (advice-add 'org-timer-update-mode-line :override #'mk/org-timer-update-mode-line)

  (require 'org-tempo)

  (setq-local org-confirm-babel-evaluate nil)

  ;; (auto-fill-mode nil)
  (org-indent-mode)
  ;; (visual-line-mode)
  (variable-pitch-mode)

  (dolist (face '((org-level-1 . (1.5 .  bold))
                  (org-level-2 . (1.1 .  bold))
                  (org-level-3 . (1.0 .  semi-bold))
                  (org-level-4 . (1.0 .  semi-bold))
                  (org-level-5 . (1.0 .  normal))
                  (org-level-6 . (1.0 .  normal))
                  (org-level-7 . (1.0 .  normal))
                  (org-level-8 . (1.0 .  normal))))
    (set-face-attribute (car face) nil :font "Iosevka Nerd Font"
                        :weight (cdr (cdr face))
                        :height (car (cdr face))))

  ;; Setup fonts for org-mode
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
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
                               '((emacs-lisp . t)
                                 (shell . t)
                                 (swift . t)
                                 (swiftui . t)
                                 (restclient . t)))

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sw" . "src swift"))
  (add-to-list 'org-structure-template-alist '("sr" . "src restclient"))
  (add-to-list 'org-structure-template-alist '("swiftui" . "src swiftui :view CustomView"))
  (add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))
  (add-to-list 'org-modules 'org-tempo t))

(use-package ob-restclient
  :defer t)

(use-package ob-swift
  :defer t)

(use-package ob-swiftui
  :config
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))
  (add-to-list 'org-babel-tangle-lang-exts
               '("swiftui" . "swift"))
  (add-to-list 'org-src-lang-modes
               '("swiftui" . swift)))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :config
  (setq org-modern-todo-faces org-custom-todo-faces
        org-modern-hide-stars t
        org-modern-star '("❶" "❷" "❸" "❹" "❺" "❻" "❼")))

(use-package visual-fill-column
  :hook ((org-mode . visual-fill-column-mode))
  :config
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds '(("https://planet.emacslife.com/atom.xml")
                       ("https://www.reddit.com/r/emacs.rss")
                       ("https://xenodium.com/rss"))
        elfeed-search-filter "@7-days-ago +unread"
        elfeed-search-title-max-width 100
        elfeed-search-title-min-width 100))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.5))

;; (use-package highlight-indentation
;;   :hook ((prog-mode . highlight-indentation-current-column-mode))
;;   :config
;;   (setq highlight-indentation-blank-lines t))


;; Drag lines and regions around
(use-package drag-stuff
  :hook (prog-mode . drag-stuff-mode)
  :bind
  ("C-j" . drag-stuff-down)
  ("C-k" . drag-stuff-up))

;; Quickly jump to definition or usage
(use-package dumb-jump
  :hook (prog-mode . dumb-jump-mode)
  :config
  (put 'dumb-jump-go 'byte-obsolete-info nil)
  (setq dumb-jump-window 'current
        dumb-jump-force-searcher 'rg
        dumb-jump-quiet t
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; (use-package yasnippet
;;   :hook (swift-mode . yas-minor-mode)
;;   :diminish yas-minor-mode
;;   :commands (yas-reload-all)
;;   :config (yas-reload-all))

(use-package swift-mode
  :defer t
  :mode "\\.swift\\'"
  :config
  (setq swift-mode:basic-offset 4
        swift-mode:parenthesized-expression-offset 4
        swift-mode:multiline-statement-offset 0
        swift-mode:highlight-anchor t
        tree-sitter-hl-use-font-lock-keywords nil)
  (setq font-lock-maximum-decoration '((swift-mode . 2) (emacs-lisp-mode . 2) (t . t))))

(use-package localizeable-mode
  :mode "\\.strings\\'"
  :bind (:map localizeable-mode-map
              ("C-c C-c" . #'swift-additions:compile-and-run)
              ("C-c C-k" . #'periphery-run-loco))
  :ensure nil)

(use-package ios-simulator
  :ensure nil
  :after swift-mode
  :bind
  ("M-s" . #'ios-simulator:terminate-current-app)
  ("C-c x c" . #'ios-simulator:appcontainer)
  ("C-c x l" . #'ios-simulator:change-language))

;; (use-package xcode-build
;;   :ensure nil
;;   :after swift-mode
;;   :bind
;;   ("M-r" . #'xcode-build:run)
;;   ("M-s" . #'xcode-build:stop))

;; (use-package overlay-usage
;;   :hook (swift-mode . overlay-usage-mode)
;;   :ensure nil)

(use-package swift-additions
  :ensure nil
  :after swift-mode
  :bind
  ("C-c t m" .  #'swift-additions:test-module-silent)
  ("C-c t p" .  #'swift-additions:test-swift-package-from-file)
  ("C-c C-c" . #'swift-additions:compile-and-run)
  ("M-r" . #'swift-additions:run)
  ("M-B" . #'swift-additions:run-without-compiling))

(use-package xcode-additions
  :ensure nil
  :after swift-mode
  :bind
  ("M-K" .  #'xcode-additions:clean-build-folder)
  ("C-c C-x" . #'xcode-additions:reset))

(use-package swift-refactor
  :ensure nil
  :after swift-mode
  :bind
  ("C-c r a" . #'swift-refactor:insert-around)
  ("C-c r d" . #'swift-refactor:delete-current-line-with-matching-brace)
  ("C-c r i" . #'swift-refactor:tidy-up-constructor)
  ("C-c r s" . #'swift-refactor:split-function-list)
  ("C-c r r" . #'swift-refactor:extract-function)
  ("M-P" .  #'swift-refactor:print-thing-at-point)
  ("M-t" . #'swift-refactor:insert-todo)
  ("M-m" . #'swift-refactor:insert-mark)
  ("C-c r t" . #'swift-refactor:add-try-catch))

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

(use-package periphery-quick
  :ensure nil
  :after prog-mode
  :bind
  ("C-c q a" . #'periphery-quick:find-ask)
  ("M-F" . #'periphery-quick:find)
  ("C-c f f" . #'periphery-quick:find-in-file)
  ("C-c f t" . #'periphery-quick:todos))

(use-package periphery-search
  :ensure nil
  :after prog-mode
  :bind
  ("C-c C-s" . #'periphery-search-rg)
  ("C-c C-f" . #'periphery-search-dwiw-rg)
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

(use-package filer
  :ensure nil
  :defer t
  :bind
  ("M-O" . filer:find-file))

(defun mk/browser-split-window (url &optional new-window)
  "Create a new browser (as URL as NEW-WINDOW) window to the right of the current one."
  (interactive)
  (let ((ignore-window-parameters t)
        (dedicated-p (window-dedicated-p)))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (xwidget-webkit-browse-url url)))

(use-package electric
  :hook (prog-mode . electric-pair-mode)
  :ensure nil
  :config
  (setq electric-pair-preserve-balance t))

;; Setup Functions
(defun mk/setupProgrammingSettings ()
  "Programming mode."

  (local-set-key (kbd "C-c C-f") nil)
  (local-set-key (kbd "C-c C-f") #'periphery-search-dwiw-rg)
  (local-set-key (kbd "C-c C-g") #'isearch-forward-thing-at-point)
  (local-set-key (kbd "M-+") #'mk/toggle-flycheck-errors)
  (local-set-key (kbd "M-?") #'periphery-toggle-buffer)
  (hs-minor-mode)       ; Add support for folding code blocks

  (setq indicate-empty-lines nil            ;; Show empty lines
        indicate-unused-lines nil           ;; Show unused lines
        truncate-lines t
        left-fringe-width 32
        right-fringe-width 0
        show-trailing-whitespace nil      ;; Show or hide trailing whitespaces
        column-number-mode nil            ;; Show current line number highlighted
        display-line-numbers 'relative))   ;; Show line numbers

(defun mk/toggle-flycheck-errors ()
  "Function to toggle flycheck errors."
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
    (list-flycheck-errors)))

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

(setq mk-transparency-disabled-p t)

(defun mk/toggle-transparency ()
  "Toggle transparency"
  (interactive)
  (let* ((not-transparent-p (and (boundp 'mk-transparency-disabled-p) mk-transparency-disabled-p))
         (alpha (if not-transparent-p 95 100)))
    (setq mk-transparency-disabled-p (not not-transparent-p))
    (progn
      (set-frame-parameter (selected-frame) 'alpha `(,alpha . ,alpha))
      (add-to-list 'default-frame-alist `(alpha . (,alpha . ,alpha))))))

(add-hook 'prog-mode-hook #'mk/setupProgrammingSettings)

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

(put 'narrow-to-region 'disabled nil)

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
  "Change the mini-buffer to a little bigger text."
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 1.05))))

;; (use-package dirvish
;;   :init
;;   (dirvish-override-dired-mode)
;;   :config
;;   (setq dirvish-mode-line-format
;;         '(:left (sort symlink) :right (omit yank index)))
;;   (setq dirvish-attributes
;;         '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
;;   (setq delete-by-moving-to-trash t)
;;   (setq dirvish-default-layout '(0 0.4 0.6))
;;   ;; (setq dired-listing-switches
;;   ;;       "-l --almost-all --human-readable --group-directories-first --no-group")
;;   :bind
;;   (("C-c f" . dirvish-fd)
;;    :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
;;    ("a"   . dirvish-quick-access)
;;    ("f"   . dirvish-file-info-menu)
;;    ("y"   . dirvish-yank-menu)
;;    ("N"   . dirvish-narrow)
;;    ("^"   . dirvish-history-last)
;;    ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;;    ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;;    ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;;    ("TAB" . dirvish-subtree-toggle)
;;    ("M-f" . dirvish-history-go-forward)
;;    ("M-b" . dirvish-history-go-backward)
;;    ("M-l" . dirvish-ls-switches-menu)
;;    ("M-m" . dirvish-mark-menu)
;;    ("M-t" . dirvish-layout-toggle)
;;    ("M-s" . dirvish-setup-menu)
;;    ("M-e" . dirvish-emerge-menu)
;;    ("M-j" . dirvish-fd-jump))
;;   )

(use-package copilot
  :hook (prog-mode . copilot-mode)
  :ensure nil
  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("TAB" . copilot-accept-completion)
        ("C-c C-n" . copilot-next-completion)
        ("C-c C-p" . copilot-previous-completion)))
;; (package-vc-install "https://github.com/copilot-emacs/copilot.el.git")


;; (use-package window-stool
;;   :hook (prog-mode . window-stool-mode)
;;   :ensure nil
;;   :config
;;   (setq window-stool-n-from-top 2
;;         window-stool-n-from-bottom 0)
;;     ;; (package-vc-install "https://github.com/JasZhe/window-stool")
;;   )

(use-package eglot-booster
  :ensure nil
  :after eglot
  :config (eglot-booster-mode))
;; (package-vc-install "https://github.com/jdtsmith/eglot-booster")

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :ensure nil
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.1))
  (indent-bars-width-frac 0.3)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-pattern ".")
  (indent-bars-prefer-character t)
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)) ; blend=1: blend with BG only
  (indent-bars-highlight-current-depth '(:blend 0.5)) ; pump up the BG blend on current
  (indent-bars-display-on-blank-lines nil))
;; ;; (package-vc-install "https://github.com/jdtsmith/indent-bars")
;; rainbow-mode show hex as colors

(use-package rainbow-mode
  :hook (emacs-lisp-mode . rainbow-mode))

(provide 'init)

;;; init.el ends here
