;;; init.el --- My init.el

;;; Code:

(setq gc-cons-threshold (* 100 1000 1000))
(defvar comp-deferred-compilation)

(setq inhibit-startup-message t
      comp-deferred-compilation t
      package-enable-at-startup nil
      frame-inhibit-implied-resize t
      site-run-file nil
      inhibit-compacting-font-caches t
      frame-resize-pixelwise t
      window-resize-pixelwise t
      ns-pop-up-frames nil
      display-time-24hr-format t
      visible-bell t
      create-lockfiles nil)

(scroll-bar-mode -1)     ; Disable scrollbar
(menu-bar-mode -1)       ; Disable the menu bar
(tool-bar-mode -1)       ; Disable toolbar
(tooltip-mode -1)        ; Disable tooltip
(set-fringe-mode 4)      ; Give us some space
(display-time-mode t)    ; Show time
(display-battery-mode t) ; Show battery
(recentf-mode t)         ; Recent file mode
(global-hl-line-mode)    ; Show current line

(setq-default display-line-numbers-width 3)

;; Window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

;; dont word wrap
(add-hook 'prog-mode-hook #'(lambda ()
			     (setq truncate-lines t
				   electric-pair-mode t
				   company-mode t
				   semantic-mode t
				   show-trailing-whitespace t
				   indicate-unused-lines t
				   indicate-empty-lines t
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

;; line-numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line number for following modes
( dolist (mode '(shell-mode-hook
		term-mode-hook
		vterm-mode-hook
		helpful-mode-hook
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
(setq use-package-verbose nil)

(defun mk/display-startup-time()
  (message "Emacs loaded in %s with %d garbage collection."
	   (format "%.2f seconds"
		    (float-time (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'mk/display-startup-time)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Mikaels dashboard!"
	dashboard-set-file-icons t
	dashboard-set-init-info t
	dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
	dashboard-startup-banner 'logo
	dashboard-items '(
			  (projects . 5)
			  (recents . 3)
			  ;; (bookmarks . 5)
			  ;; (agenda . 5)
			  )))

;; Which key
(use-package which-key
  :defer 0
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

(use-package evil-tutor)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))

;; counsel
(use-package counsel
  :config (counsel-mode 1))

(use-package solaire-mode
  :config (solaire-global-mode 1))

;; Ivy rich
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

(use-package treemacs
  :defer t
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
  (load-theme 'doom-gruvbox t)

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
  (doom-modeline-height 1)
  (doom-modeline-bar-width 10)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 15)
  (doom-modeline-env-version t))

(set-face-attribute 'mode-line nil :family "Noto Sans" :height 136)
(set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 116)

;; rainbow-delimieters
(use-package rainbow-delimiters
 :hook (prog-mode . rainbow-delimiters-mode))

;; company --------------------------------------------
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :commands (lsp lsp-deffered)
  :init
  (setq lsp-keymap-prefix "m-l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package treemacs-projectile
  :hook (treemacs-mode-hook))

(use-package nyan-mode
  :hook doom-modeline-mode
  :config
  (nyan-mode))

; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)
  (use-package ns-auto-titlebar)
  (use-package swift-mode
    :hook (swift-mode . lsp-deferred)
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
(use-package helpful :defer t)

;; smex
(use-package smex :defer t)

(use-package vterm
  :commands vterm)

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

;; Restart emacs
(use-package restart-emacs
  :defer t)

; Hydra
(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Winum - select windows easy
(use-package winum
  :defer t
  :init
  (winum-mode 1))

;; darkroom (go to focus mode)
(use-package darkroom
  :commands darkroom)

;; Use git
(use-package magit
  :commands magit-status
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
  :defer t
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
   "a" '(:ignore t :which-key "applications")
   "af" '(:ignore t :which-key "feed")
   "afu" '(elfeed-update :which-key "update feed")
   "afs" '(elfeed :which-key "show feed"))

  (mk/leader-keys
   "f" '(:ignore t :which-key "files")
   "fs" '(save-buffer :which-key "save file")
   "ff" '(find-file :which-key "find file")
   "fn" '(create-file-buffer :which-key "new file")
   "fR" 'eval-buffer
   "fe" '(lambda () (interactive) (find-file user-init-file) :which-key "user configuration"))

  (mk/leader-keys
    "c" '(:ignore t :which-key "code")
    "cp" 'check-parens
    "co" 'projectile-find-other-file
    "cl" '(comment-line :which-key "comment line")
    "cr" '(comment-region :which-key "comment region")
    "ce" '(lsp-treemacs-errors-list :which-key "treemacs errors")
    "ct" '(lsp-treemacs-symbols :which-key "treemacs symbols")
    "cf" '(lsp-ivy-global-workspace-symbol :which-key "find symbol in workspace"))

  (mk/leader-keys
    "q" '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal
    "qr" 'restart-emacs)

  (mk/leader-keys
    "t" '(:ignore t :which-key "text")
    "tw" '(:ignore t :which-key "whitespace")
    "twx" '(delete-trailing-whitespace :which-key "delete trailing whitespace"))

   (mk/leader-keys
     "b" '(:ignore t :which-key "buffer")
     "bb" '(counsel-switch-buffer :which-key "list buffers")
     "bx" '(delete-window :which-key "close buffer")
     "bk" '((lambda () (interactive) (kill-other-buffers)) :which-key "kill other buffers")
     "bd" '(kill-current-buffer :which-key "kill current buffer")
     "bp" '(previous-buffer :which-key "previous buffer")
     "bn" '(next-buffer :which-key "next buffer")
     "be" '(eval-buffer :which-key "eval buffer")
     "br" '(revert-buffer :which-key "revert buffer")
     "bC" '((lambda () (interactive) (switch-to-buffer "*Compile-Log*")) :which-key "Compile log-buffer")
     "bD" '((lambda () (interactive) (switch-to-buffer "*dashboard*")) :which-key "dashboard-buffer")
     "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "messages-buffer")
     "Bs" '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "scratch-buffer"))

   (mk/leader-keys
     "h" '(:ignore t :which-key "help")
     "hc" '(helpful-command :which-key "describe command")
     "hk" '(helpful-key :which-key "describe key")
     "hf" '(helpful-function :which-key "describe function")
     "hv" '(helpful-variable :which-key "describe variable")
     "ht" '(evil-tutor-start :which-key "evil tutorial")
     "hp" '(helpful-at-point :which-key "describe at-point"))

   (mk/leader-keys
     "w" '(:ignore t :which-key "windows")
     "wx" '(delete-window :which-key "delete window")
     "wk" '(kill-buffer-and-window :which-key "kill buffer and window")
     "w-" '(split-window-below :which-key "split horizontally")
     "w/" '(split-window-right :which-key "split vertically")
     "wn" '(next-window-any-frame :which-key "next window")
     "wp" '(previous-window-any-frame :which-key "previous window")
     "wb" '(xwidget-webkit-browse-url :which-key "start a browser"))

   (mk/leader-keys
     "p" '(:ignore t :which-key "project")
     "pp" '(:ignore t :which-key "project management")
     "ppa" '(treemacs-add-project-to-workspace :which-key "add project")
     "ppr" '(treemacs-remove-project-from-workspace :which-key "remove project")
     "pf" '(projectile-find-file :which-key "find file")
     "pt" '(projectile-find-tag :which-key "find tag")
     "pF" '(projectile-project-files :which-key "project files")
     "pk" '(projectile-kill-buffers :which-key "kill buffers")
     "ps" '(projectile-switch-project :which-key "switch project")
     "pS" '(projectile-switch-open-project :which-key "switch open project"))

   (mk/leader-keys
     "v" '(:ignore t :which-key "version control")
     "vs" '(magit-status :which-key "status"))

   (mk/leader-keys
     "g" '(:ignore t :which-key "games")
     "gt" '(tetris :which-key "tetris")
     "gh" '(hanoi :which-key "tower of hanoi"))

   (mk/leader-keys
     "T" '(:ignore t :which-key "tabs")
     "Tn" '(tab-new :which-key "new")
     "Tl" '(tab-list :which-key "list")
     "Tg" '(tab-close-group :which-key "close group")
     "Td" '(tab-detach :which-key "detach")
     "Tx" '(tab-close :which-key "close")
     "Tk" '(tab-close-other :which-key "close other"))
   )

;; Forge - Git PR, Issues, etc
;;(use-package forge)

(defun mk/org-mode-setup()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . mk/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-hide-leading-stars))

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp t)))
  (setq org-confirm-babel-evaluate nil)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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
		       ("https://news.ycombinator.com/rss" Hacker News)
		       ("https://www.reddit.com/r/emacs.rss" emacs)
		       ("https://www.reddit.com/r/swift.rss" swift)
		       ("https://www.reddit.com/r/haikuos.rss" haiku)
		       )))

(setq-default elfeed-search-filter "@2-days-ago +unread")
(setq-default elfeed-search-title-max-width 100)
(setq-default elfeed-search-title-min-width 100)

(setq gc-cons-threshold (* 2 1000 1000))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ob-swift evil-tutor forge evil-magit magit solaire-mode company general spaceline-all-the-icons spaceline all-the-icons doom-themes ivy evil which-key use-package))
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
