;;; init.el --- My init.el

;;; Code:

(defvar comp-deferred-compilation)

(setq inhibit-startup-message t
      comp-deferred-compilation t
      package-enable-at-startup nil
      frame-inhibit-implied-resize t
      site-run-file nil                 
      inhibit-compacting-font-caches t)

(scroll-bar-mode -1)        ; Disable scrollbar
(menu-bar-mode -1)           ; Disable the menu bar
(tool-bar-mode -1)           ; Disable toolbar
(tooltip-mode -1)           ; Disable tooltip
(set-fringe-mode 10)         ; Give us some space
(display-time-mode t)        ; Show time
(display-battery-mode t)     ; Show battery

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

(setq display-time-format "%H:%M")
(setq display-time-default-load-average nil) ; Dont show avg load

;; dont word wrap
(add-hook 'prog-mode-hook '(lambda ()
    (setq truncate-lines t
          word-wrap nil)))

;; Set yes or no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; always highlight code
(global-font-lock-mode 1)

;; refresh a buffer if changed on disk
(global-auto-revert-mode 1)

;; saving
(desktop-save-mode 1)
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'kill-ring)

;; Set up the visible bell
;; (setq visible-bell t)

;; line-numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line number for following modes
(dolist (mode '(shell-mode-hook
		term-mode-hook
		vterm-mode-hook
		xwidget-webkit-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Source Code Pro" :height 145)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources

;;; Commentary:
;; 

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

; Which key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.15
	which-key-min-display-lines 5
	which-key-max-display-columns 4))

; Use evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; counsel
(use-package counsel)

(use-package solaire-mode
  :config (solaire-global-mode 1))

;; Ivy rich
(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package treemacs
  :config
  (treemacs-toggle-fixed-width nil)
  (setq treemacs-text-scale -1))

;; Theming
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
	doom-themes-treemacs-theme "doom-atom") 
  (load-theme 'doom-shades-of-purple t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 24)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

;; rainbow-delimieters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; company
(use-package company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))

; On macos use our custom settings
(when (eq system-type 'darwin)
  (use-package ns-auto-titlebar)
  (use-package swift-mode
    :hook (swift-mode . (lambda () (lsp)))
    :config
    (setq swift-mode:parenthesized-expression-offset 4
	  swift-mode:multiline-statement-offset 4))
  (use-package exec-path-from-shell)
  (use-package lsp-sourcekit
    :after lsp-mode
    :config
    (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))
  (exec-path-from-shell-initialize)
  (ns-auto-titlebar-mode)
  (setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none))

; helpful
(use-package helpful)

;; smex
(use-package smex)

(use-package vterm)

(defun my-vterm/split-horizontal ()
  "Create a new vterm window to the right of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p)))
    (split-window-vertically)
    (other-window 1)
    (vterm default-directory)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom
    (projectile-project-root-files-functions
     '(projectile-root-local
       projectile-root-top-down
       projectile-root-bottom-up
       projectile-root-top-down-recurring))
    ((projectile-completion-system 'ivy))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/git")
    (setq projectile-project-search-path '("~/Documents/git")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; Recent files
(recentf-mode t)

;; Restart emacs
(use-package restart-emacs)

; Hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Winum - select windows easy
(use-package winum
  :init
  (winum-mode 1))

;; darkroom (go to focus mode)
(use-package darkroom)

;; Use git
(use-package magit
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

;; Kill all other buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; general
(use-package general
  :config
  (general-create-definer mk/leader-keys
   :keymaps '(normal insert emacs visual operator hybrid)
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (mk/leader-keys
   "T" '(:ignore t :which-key "toggles")
   "Tt" '(counsel-load-theme :which-key "choose theme")
   "Ts" '(hydra-text-scale/body :which-key "scale text")
   "Tf" '(toggle-frame-fullscreen :which-key "fullscreen"))

  (mk/leader-keys
   "TAB" '((lambda () (interactive) (switch-to-buffer nil)) :which-key "toggle buffers")
   "SPC" '(counsel-M-x :which-key "M-x")
   "0" '(treemacs :which-key "treemacs")
   "1" '(winum-select-window-1 :which-key "window 1")
   "2" '(winum-select-window-2 :which-key "window 2")
   "3" '(winum-select-window-3 :which-key "window 3")
   "4" '(winum-select-window-4 :which-key "window 4")
   "5" '(winum-select-window-5 :which-key "window 5")
   "6" '(winum-select-window-6 :which-key "window 6")
   "s" 'swiper
   "P" 'package-install
   "'" '((lambda () (interactive) (my-vterm/split-horizontal)) :which-key "term")
   "!" 'shell-command
   ":" 'eval-expression)
  
  (mk/leader-keys
   "f" '(:ignore t :which-key "files")
   "fs" '(save-buffer :which-key "save file")
   "ff" '(find-file :which-key "find file")
   "fn" '(create-file-buffer :which-key "new file")
   "fR" 'eval-buffer
   "fe" '((lambda () (interactive) (find-file user-init-file)) :which-key "user configuration"))
  
  (mk/leader-keys
    "c" '(:ignore t :which-key "code")
    "cp" 'check-parens 
    "co" 'projectile-find-other-file
    "cl" '(comment-line :which-key "comment line")
    "cr" '(comment-region :which-key "comment region"))

  (mk/leader-keys
    "q" '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal
    "qr" 'restart-emacs)

   (mk/leader-keys
     "b" '(:ignore t :which-key "buffer")
     "bb" '(counsel-switch-buffer :which-key "list buffers")
     "bx" '(delete-window :which-key "close buffer")
     "bk" '((lambda () (interactive) (kill-other-buffers)) :which-key "kill other buffers")
     "bd" '(kill-current-buffer :which-key "kill current buffer")
     "bp" '(previous-buffer :which-key "previous buffer")
     "bn" '(next-buffer :which-key "next buffer")
     "be" '(eval-buffer :which-key "eval buffer")
     "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "messages-buffer")
     "bc" '((lambda () (interactive) (switch-to-buffer "*Compile*")) :which-key "compile-buffer")
     "bs" '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "scratch-buffer"))

   (mk/leader-keys
     "h" '(:ignore t :which-key "help")
     "hc" '(helpful-command :which-key "describe command")
     "hk" '(helpful-key :which-key "describe key")
     "hf" '(helpful-function :which-key "describe function")
     "hv" '(helpful-variable :which-key "describe variable")
     "hp" '(helpful-at-point :which-key "describe at-point"))

   (mk/leader-keys
     "w" '(:ignore t :which-key "windows")
     "wx" '(delete-window :which-key "delete window")
     "wk" '(kill-buffer-and-window :which-key "kill buffer and window")
     "w-" '(split-window-below :which-key "split horizontally")
     "w/" '(split-window-right :which-key "split vertically")
     "wb" '(xwidget-webkit-browse-url :which-key "start a browser"))

   (mk/leader-keys
     "p" '(:ignore t :which-key "project")
     "pf" '(projectile-find-file :which-key "find file")
     "pt" '(projectile-find-tag :which-key "find tag")
     "pp" '(projectile-project-files :which-key "project files")
     "pk" '(projectile-kill-buffers :which-key "kill buffers")
     "ps" '(projectile-switch-project :which-key "switch project")
     "pS" '(projectile-switch-open-project :which-key "switch open project"))

   (mk/leader-keys
     "v" '(:ignore t :which-key "version control")
     "vs" '(magit-status :which-key "status"))

   (mk/leader-keys
     "t" '(:ignore t :which-key "tabs")
     "tn" '(tab-new :which-key "new")
     "tl" '(tab-list :which-key "list")
     "tg" '(tab-close-group :which-key "close group")
     "td" '(tab-detach :which-key "detach")
     "tx" '(tab-close :which-key "close")
     "tk" '(tab-close-other :which-key "close other"))
   )

;; Forge - Git PR, Issues, etc
;;(use-package forge)

(use-package org
  :config
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t
	org-hide-leading-stars))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(forge evil-magit magit solaire-mode company general spaceline-all-the-icons spaceline all-the-icons doom-themes ivy evil which-key use-package))
 '(warning-suppress-log-types '((comp) (frameset) (use-package) (use-package)))
 '(warning-suppress-types '((frameset) (use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here
