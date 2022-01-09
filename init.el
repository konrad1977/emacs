;;; init.el --- My init.el

;;; Code:

(setq gc-cons-threshold (* 100 1024 1024))

(setq comp-deferred-compilation nil
      create-lockfiles nil
      display-time-24hr-format t
      display-time-default-load-average nil
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      indent-tabs-mode nil
      inhibit-compacting-font-caches t
      ns-pop-up-frames nil
      inhibit-startup-message t
      package-enable-at-startup nil
      site-run-file nil
      visible-bell t
      window-resize-pixelwise t)

(display-battery-mode t) ; Show battery
(display-time-mode t)    ; Show time
(global-hl-line-mode)    ; Show current line
(menu-bar-mode -1)       ; Disable the menu bar
(recentf-mode t)         ; Recent file mode
(scroll-bar-mode -1)     ; Disable scrollbar
(set-fringe-mode 4)      ; Give us some space
(tool-bar-mode -1)       ; Disable toolbar
(tooltip-mode -1)        ; Disable tooltip

(setq indent-line-function 'insert-tab)

(setq-default display-line-numbers-width 4
			  c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)

;; Window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

;; dont word wrap
(add-hook 'prog-mode-hook #'(lambda ()
			                  (setq company-mode t
				                    electric-pair-mode t
				                    highlight-indent-guides-mode t
				                    indicate-empty-lines t
				                    indicate-unused-lines t
				                    semantic-mode t
				                    show-trailing-whitespace t
				                    word-wrap nil
                                    column-number-mode t
                                    display-line-numbers t
                                    show-paren-mode t
                                    truncate-lines t)))

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

(set-face-attribute 'default nil :font "Source Code Pro" :height 148)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 148)
(set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 148 :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
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

(use-package dired
  :ensure nil
  :commands dired dired-jump
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package autothemer
    :defer t)

;;  theming
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (make-local-variable 'face-remapping-alist)
            (add-to-list 'face-remapping-alist '(default (:background "#15121C")))))

(load-theme 'catppuccin t)

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
	dashboard-week-agenda nil
	dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
	dashboard-items '(
			  (projects . 5)
			  (recents . 3)
			  (agenda . 10))))

;; Which key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.3
	which-key-min-display-lines 4
	which-key-max-display-columns 5))

; Use evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)

  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'normal))

(use-package evil-tutor
  :commands evil-tutor)

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

;; Ivy rich
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))


;; Theming
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
	    doom-themes-treemacs-theme "doom-atom")
  ;; (load-theme 'doom-outrun-electric  t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons
  :defer t)

(use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

;; Config and install modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-env-version t)
  :config
  (setq doom-modeline-height 38)
  (set-face-attribute 'mode-line nil :family "Source Code Pro" :height 136)
  (set-face-attribute 'mode-line-inactive nil :family "Source Code Pro" :height 128))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (set-face-background 'auto-dim-other-buffers-face nil)
  (set-face-foreground 'auto-dim-other-buffers-face "#988BA2"))

(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))

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
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
      company-box-icons-all-the-icons
      (let ((all-the-icons-scale-factor 1)
            (all-the-icons-default-adjust 0))
        `((Unknown       . ,(all-the-icons-faicon "question" :face 'all-the-icons-purple)) ;;question-circle is also good
          (Text          . ,(all-the-icons-faicon "file-text-o" :face 'all-the-icons-green))
          (Method        . ,(all-the-icons-faicon "cube" :face 'all-the-icons-dcyan))
          (Function      . ,(all-the-icons-faicon "cube" :face 'all-the-icons-dcyan))
          (Constructor   . ,(all-the-icons-faicon "cube" :face 'all-the-icons-dcyan))
          (Field         . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
          (Variable      . ,(all-the-icons-faicon "tag" :face 'all-the-icons-dpurple))
          (Class         . ,(all-the-icons-faicon "cog" :face 'all-the-icons-red))
          (Interface     . ,(all-the-icons-faicon "cogs" :face 'all-the-icons-red))
          (Module        . ,(all-the-icons-alltheicon "less" :face 'all-the-icons-red))
          (Property      . ,(all-the-icons-faicon "wrench" :face 'all-the-icons-red))
          (Unit          . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
          (Value         . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
          (Enum          . ,(all-the-icons-faicon "file-text-o" :face 'all-the-icons-red))
          (Keyword       . ,(all-the-icons-material "format_align_center" :face 'all-the-icons-red))
          (Snippet       . ,(all-the-icons-material "content_paste" :face 'all-the-icons-red))
          (Color         . ,(all-the-icons-material "palette" :face 'all-the-icons-red))
          (File          . ,(all-the-icons-faicon "file" :face 'all-the-icons-red))
          (Reference     . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
          (Folder        . ,(all-the-icons-faicon "folder" :face 'all-the-icons-red))
          (EnumMember    . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
          (Constant      . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
          (Struct        . ,(all-the-icons-faicon "cog" :face 'all-the-icons-red))
          (Event         . ,(all-the-icons-faicon "bolt" :face 'all-the-icons-red))
          (Operator      . ,(all-the-icons-faicon "tag" :face 'all-the-icons-red))
          (TypeParameter . ,(all-the-icons-faicon "cog" :face 'all-the-icons-red))
          (Template      . ,(all-the-icons-faicon "bookmark" :face 'all-the-icons-dgreen))))))

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
    (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "Xcrun --find sourcekit-lsp"))))
  (exec-path-from-shell-initialize)
  (ns-auto-titlebar-mode)
  (setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none))

; helpful
(use-package helpful
  :after ivy)

;; smex
(use-package smex
  :after ivy)

(defun my-vterm/split-horizontal ()
  "Create a new vterm window under of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p)))
    (split-window-vertically)
    (other-window 1)
    (vterm default-directory)))

(defun mk/browser-split-vertically ()
  "Create a new browser window to the right of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p)))
    (split-window-horizontally)
    (other-window 1)
    (xwidget-webkit-browse-url "https://google.com")))

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

(use-package vterm
  :commands vterm)

(use-package solaire-mode
  :defer t)

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
   "0" '(treemacs-select-window :which-key "treemacs")
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
   "e" '(:ignore t :which-key "eval")
   "ee" '(eval-expression :which-key "eval expression")
   "eb" '(eval-buffer :which-key "eval buffer")
   "el" '(eval-last-sexp :which-key "eval before point")
   "er" '(eval-region :which-key "eval region"))

  (mk/leader-keys
   "a" '(:ignore t :which-key "agenda")
   "aa" '(org-agenda :which-key "agenda")
   "al" '(org-agenda-list :which-key "agenda list")
   "aF" '(org-agenda-file-to-front :which-key "bring file to front"))

  (mk/leader-keys
   "A" '(:ignore t :which-key "applications")
   "Af" '(:ignore t :which-key "feed")
   "Afu" '(elfeed-update :which-key "update feed")
   "Afs" '(elfeed :which-key "show feed"))

  (mk/leader-keys
   "f" '(:ignore t :which-key "files")
   "fs" '(save-buffer :which-key "save file")
   "fo" '(dired :which-key "open file")
   "ff" '(counsel-find-file :which-key "find file")
  "fn" '(create-file-buffer :which-key "new file")
   "fr" '(dired-rename-file :which-key "rename file")
   "fD" '(delete-file :which-key "delete file")
   "fR" 'eval-buffer
   "fe" '(lambda () (interactive) (find-file user-init-file) :which-key "user configuration"))

  (mk/leader-keys
    "c" '(:ignore t :which-key "code")
    "cp" 'check-parens
    "co" 'projectile-find-other-file
    "cl" '(comment-line :which-key "comment line")
    "cr" '(comment-region :which-key "comment region")
    "cu" '(lsp-ui-imenu :which-key "lsp-ui-menu")
    "ce" '(lsp-treemacs-errors-list :which-key "treemacs errors")
    "ct" '(lsp-treemacs-symbols :which-key "treemacs symbols")
    "cf" '(lsp-ivy-global-workspace-symbol :which-key "find symbol in workspace"))

  (mk/leader-keys
    "q" '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal
    "qr" 'restart-emacs)

  (mk/leader-keys
    "t" '(:ignore t :which-key "text")
    "ts" '(sort-lines :which-key "sort lines")
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
     "bE" '(eval-last-sexp :which-key "eval to point")
     "br" '(revert-buffer :which-key "revert buffer")
     "bC" '((lambda () (interactive) (switch-to-buffer "*Compile-Log*")) :which-key "Compile log-buffer")
     "bD" '((lambda () (interactive) (switch-to-buffer "*dashboard*")) :which-key "dashboard-buffer")
     "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "messages-buffer")
     "Bs" '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "scratch-buffer"))

   (mk/leader-keys
     "h" '(:ignore t :which-key "help")
     "hc" '(helpful-command :which-key "describe command")
     "hk" '(helpful-key :which-key "describe key")
     "hf" '(counsel-describe-function :which-key "describe function")
     "hv" '(counsel-describe-variable :which-key "describe variable")
     "ht" '(evil-tutor-start :which-key "evil tutorial")
     "hp" '(helpful-at-point :which-key "describe at-point"))

   (mk/leader-keys
     "w" '(:ignore t :which-key "windows")
     "wx" '(delete-window :which-key "delete window")
     "wk" '(kill-buffer-and-window :which-key "kill buffer and window")
     "w-" '(split-window-below :which-key "split horizontally")
     "w/" '(split-window-right :which-key "split vertically")
     "wn" '(next-window-any-frame :which-key "next window")
	 "wb" '((lambda () (interactive) (mk/browser-split-vertically)) :which-key "start a browser")
     "wp" '(previous-window-any-frame :which-key "previous window"))
;;     "wb" '(xwidget-webkit-browse-url :which-key "start a browser"))

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
	org-hide-leading-stars
	org-agenda-start-with-log-mode t
	org-log-into-drawer t
	org-log-done 'time
	org-agenda-files '("~/Library/Mobile Documents/com~apple~CloudDocs/orgfiles/")))

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
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method 'bitmap))

;; Reset memory for Garbage collection
(setq gc-cons-threshold (* 5 1024 1024))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "aeca5b24d5683690fd4bcf6a689126afa4fc041fd52b5a31b2fcf7a6d02170d5" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "9f802342df5b5df84a202b523d9c69ecc9c7b46b91aa234fb2ab5667d6d7bd45" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "60f1890c909adc4c805fde450b4b46290fe55e45300bf8b3c825a66f58288498" "d52acf9fe3ad7f84a612bd9e78a05b3cd6544482602205f33b6b01dfa93ca093" default))
 '(org-agenda-files
   '("~/Library/Mobile Documents/com~apple~CloudDocs/orgfiles/work.org" "/Users/mikaelkonradsson/Library/Mobile Documents/com~apple~CloudDocs/orgfiles/Todo.org" "/Users/mikaelkonradsson/Library/Mobile Documents/com~apple~CloudDocs/orgfiles/Tasks.org" "/Users/mikaelkonradsson/Library/Mobile Documents/com~apple~CloudDocs/orgfiles/Stella.org" "/Users/mikaelkonradsson/Library/Mobile Documents/com~apple~CloudDocs/orgfiles/Matheo.org"))
 '(package-selected-packages
   '(dired-single ivy-posframe autothemer ob-swift evil-tutor forge evil-magit magit solaire-mode company general spaceline-all-the-icons spaceline all-the-icons doom-themes ivy evil which-key use-package))
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
