;;; init.el --- My init.el

;;; Code:

;; Window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(eval-when-compile (defvar display-time-24hr-format))
(eval-when-compile (defvar display-time-default-load-average))

(setq auto-mode-case-fold nil)
(setq ad-redefinition-action 'accept
	  create-lockfiles nil
	  word-wrap nil
	  global-hl-line-mode 1
      display-time-24hr-format t
      display-time-default-load-average nil
      visible-bell nil
      backup-by-copying t
	  initial-scratch-message ""
	  idle-update-delay 1.0			;; Speed things up by not updating so often
	  blink-cursor-interval 0.6		;; Little slower cursor blinking . default is 0.5
	  fast-but-imprecise-scrolling t
	  read-process-output-max (* 8 1024 1024)
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setenv "PATH" (concat (getenv "PATH") "/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Setup garbage collector
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))


(display-battery-mode t)	; Show battery.
(display-time-mode t)		; Show time.
(recentf-mode t)			; Recent file mode.
(scroll-bar-mode -1)		; Dont use scrollbars.
(set-fringe-mode 4)			; Give us some space.
(tooltip-mode -1)			; Disable tooltip.
(show-paren-mode t)			; Enable show paren matching mode.
(delete-selection-mode t)	; Use a more sane delete mode than evil.

(setq-default display-line-numbers-width 4		;; Set so we can display thousands of lines
			  c-basic-offset 4					;; Set tab indent for c/c++ to 4 tabs
			  tab-width 4						;; Use four tabs
			  truncate-lines 1					;; Truncate lines
			  indent-tabs-mode t				;; Indent tabs
              indent-line-function 'insert-tab) ;; Use function to insert tabs

(fset 'yes-or-no-p 'y-or-n-p)	;; Set yes or no to y/n
(global-font-lock-mode 1)		;; always highlight code
(global-auto-revert-mode 1)		;; refresh a buffer if changed on disk
(desktop-save-mode 1)			;; Save desktop
(savehist-mode 1)				;; Save autocompletions

(let* ((path (expand-file-name "localpackages" user-emacs-directory))
       (local-pkgs (mapcar 'file-name-directory (directory-files-recursively path ".*\\.el"))))
  (if (file-accessible-directory-p path)
      (mapc (apply-partially 'add-to-list 'load-path) local-pkgs)
    (make-directory path :parents)))

(eval-when-compile (defvar savehist-additional-variables))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
(add-to-list 'savehist-additional-variables 'kill-ring)

;; Setup fonts
(set-face-attribute 'default nil :font "Source Code Pro" :height 154)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 154)
(set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 154 :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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

(use-package gcmh
  :init (gcmh-mode 1))

;; Make sure we are up to date, atleast once a week
(use-package auto-package-update
  :custom
  (setq auto-package-update-interval 7
		auto-package-update-prompt-before-update t
		auto-package-update-hide-results nil))

(use-package no-littering)	;; Clean up all those temporary files

(setq custom-file (concat user-emacs-directory "var/custom.el"))

;; Dont leave #file autosaves everywhere I go
(defvar my-auto-save-folder (concat user-emacs-directory "var/auto-save/"))
(setq auto-save-list-file-prefix (concat my-auto-save-folder ".saves-")); set prefix for auto-saves
(setq auto-save-file-name-transforms `((".*", my-auto-save-folder t))); location for all auto-save files

(use-package dired
  :ensure nil
  :commands dired dired-jump
  :bind (("C-x C-j" . dired-jump)))

(use-package dired-single
  :after dired)

;;  theming
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (make-local-variable 'face-remapping-alist)
            (add-to-list 'face-remapping-alist '(default (:background "#15121C")))))

(use-package autothemer
  :ensure t
  :custom (setq custom-safe-themes t))

(load-theme 'catppuccin t)
;(add-hook 'after-init-hook (lambda () (load-theme 'catppuccin t)))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :init (progn
		  (setq dashboard-startup-banner (concat user-emacs-directory "themes/catppuccin.png"))
		  (setq dashboard-path-style 'truncate-beginning)
		  (setq dashboard-banner-logo-title "Mikaels dashboard!"
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
								  ))))

;; Which key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
 ;; (which-key-setup-minibuffer)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.3
		which-key-min-display-lines 4
		which-key-max-display-columns 5))

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
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (define-key evil-motion-state-map "/" 'swiper)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  (define-key evil-visual-state-map (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package undo-fu
  :after evil
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "U" 'undo-fu-only-redo)
  (define-key evil-normal-state-map "R" 'undo-fu-only-redo-all))

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

(use-package all-the-icons
  :after doom-modeline)

(use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

;; Config and install modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq
		doom-modeline-bar-width 3
		doom-modeline-buffer-file-name-style 'file-name
		doom-modeline-buffer-encoding nil
		doom-modeline-icon t
		doom-modeline-indent-info nil
		doom-modeline-major-mode-color-icon t
		doom-modeline-major-mode-icon t
		doom-modeline-modal-icon t
		doom-modeline-checker-simple-format t
		doom-modeline-persp-icon nil
		doom-modeline-persp-name nil
		doom-modeline-env-version t
		doom-modeline-hud t
		doom-modeline-height 36)
  (set-face-attribute 'mode-line nil :family "Source Code Pro" :height 148)
  (set-face-attribute 'mode-line-inactive nil :family "Source Code Pro" :height 132))

;; rainbow-delimieters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; rainbow-mode show hex as colors
(use-package rainbow-mode
  :commands rainbow-mode)

;; Use ivy
(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-height 12
		ivy-use-virtual-buffers t
		ivy-count-format "(%d/%d) "
		ivy-use-selectable-prompt t
		ivy-display-style 'fancy)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-mode-map       (kbd "<escape>") nil)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit))

;; counsel
(use-package counsel
  :hook (ivy-mode . counsel-mode))

(use-package swiper
  :after ivy
  :config
  (setq swiper-goto-start-of-match t))

;; ;; Ivy rich
(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode))

;; company --------------------------------------------
(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (require 'all-the-icons)
  (setf (alist-get 'min-height company-box-frame-parameters) 6)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-backends-colors nil

        ;; These are the Doom Emacs defaults
        company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
          (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
          (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
          (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
          (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
          (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
          (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
          (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
          (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
          (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
          (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
          (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
          (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
          (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
          (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
          (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
          (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
          (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
          (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
          (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
          (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
          (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
          (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))))

  ;; Add a space after the icon
  (dolist (elt company-box-icons-all-the-icons)
    (setcdr elt (concat (cdr elt) " "))))

(use-package lsp-mode
  :commands (lsp lsp-deffered)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (setq lsp-ui-doc-border (face-foreground 'nil)))

(use-package treemacs
  :config
  (setq treemacs-follow-after-init          t
        treemacs-width                      40
        treemacs-indentation                1
        treemacs-git-integration            t
        treemacs-collapse-dirs              0
        treemacs-silent-refresh             t
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          nil
        treemacs-never-persist              nil
        treemacs-is-never-other-window      t
        treemacs-goto-tag-strategy          'refetch-index
		treemacs-text-scale					0)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always))

(use-package lsp-treemacs
  :after lsp)

(use-package treemacs-projectile
  :hook (treemacs-mode-hook))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
  (setq flycheck-display-errors-delay 0.1))

(use-package nyan-mode
  :hook (doom-modeline-mode . nyan-mode)
  :config
  (setq nyan-animate-nyancat t))

(use-package yasnippet
  :defer t)

; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)

(defun xcode-build()
  (interactive)
  (shell-command-to-string
    "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))
(defun xcode-run()
  (interactive)
  (shell-command-to-string
    "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))
(defun xcode-test()
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'"))

(use-package ns-auto-titlebar
	:config (ns-auto-titlebar-mode))

  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  (use-package exec-path-from-shell
	:commands vterm)

  (use-package swift-mode
    :hook (swift-mode . lsp-deferred)
    :config
    (setq swift-mode:parenthesized-expression-offset 4
		  swift-mode:multiline-statement-offset 4))

  (add-hook 'swift-mode-hook
			(lambda () (local-set-key (kbd "M-RET") #'lsp-execute-code-action)))

  (use-package flycheck-swift3 :defer t)
  (use-package swift-helpful
	:defer t)

  (use-package flycheck-swiftlint
  :config
  (with-eval-after-load 'flycheck
    (flycheck-swiftlint-setup)))
  
  (use-package lsp-sourcekit
    :after lsp-mode
    :config
    (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "Xcrun --find sourcekit-lsp"))))

  (exec-path-from-shell-initialize)

  (defvar-local my/flycheck-local-cache nil)
  (defun my/flycheck-checker-get (fn checker property)
	(or (alist-get property (alist-get checker my/flycheck-local-cache))
		(funcall fn checker property))) 

  (advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

  (add-hook 'lsp-managed-mode-hook
			(lambda ()
              (when (derived-mode-p 'swift-mode)
				(setq my/flycheck-local-cache '((lsp . ((next-checkers . (swiftlint)))))))))
  
  (setq mac-option-key-is-meta nil
		mac-command-key-is-meta t
		mac-command-modifier 'meta
		mac-option-modifier 'none
		dired-use-ls-dired nil
		frame-title-format ""))

(use-package ag
  :defer t)

; helpful
(use-package helpful
  :after which-key)

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
		  projectile-indexing-method 'native)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/git")
    (setq projectile-project-search-path '("~/Documents/git")))
  (setq projectile-switch-project-action #'projectile-dired))

;; counsel-projectile
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; Restart emacs
(use-package restart-emacs
  :commands restart-emacs)

;; posframe
(use-package posframe
  :defer t)

;; hydra
(use-package hydra
  :defer t)

(use-package pretty-hydra
  :after hydra
  :config
  (setq major-mode-hydra-title-generator
		'(lambda (mode)
           (s-concat "\n"
					 (s-repeat 10 " ")
					 (all-the-icons-icon-for-mode mode :v-adjust 0.05)
					 " "
					 (symbol-name mode)
					 " commands"))))

(use-package major-mode-hydra
  :defer t
  :config
    (setq major-mode-hydra-invisible-quit-key "q"))

;; (use-package hydra-posframe
;;   :load-path "~/.emacs.d/localpackages/hydra-posframe"
;;   :config
;;   (hydra-posframe-mode 1)
;;   :custom
;;   (hydra-posframe-parameters
;;    '((left-fringe . 20) (right-fringe . 20) (top-fringe . 10) (bottom-fringe . 10) (height . 12) (width . 105) (min-height . 10) (max-height . 30) (top . 25))))

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

(use-package amx
  :after ivy
  :config (amx-mode 1))

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
	"SPC" '(counsel-M-x :which-key "M-x")
	"s" '(swiper :which-key "Swiper")
	"0" '(treemacs :which-key "Treemacs")
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
    "cp" 'check-parens
    "co" 'projectile-find-other-file
    "cl" '(comment-line :which-key "Comment line")
    "cr" '(comment-region :which-key "Comment region")
    "cu" '(lsp-ui-imenu :which-key "Lsp-ui-menu")
    "ce" '(lsp-treemacs-errors-list :which-key "Treemacs errors")
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
    "hf" '(counsel-describe-function :which-key "Describe function")
    "hv" '(counsel-describe-variable :which-key "Describe variable")
    "ht" '(evil-tutor-start :which-key "Evil tutorial")
    "h." '(helpful-at-point :which-key "Describe at-point")
    "hp" '(describe-package :which-key "Describe package"))

  (mk/leader-keys
    "t" '(:ignore t :which-key "Text")
    "ts" '(sort-lines :which-key "Sort lines")
	"tS" '(hydra-text-scale/body :which-key "Scale text")
    "tx" '(delete-trailing-whitespace :which-key "Delete trailing whitespace")
    "tw" '(mark-word :which-key "Select word")
    "te" '(mark-sexp :which-key "Select expression")
    "tf" '(mark-defun :which-key "Select function")
    "tb" '(mark-whole-buffer :which-key "Select whole buffer")
    "tp" '(mark-page :which-key "Select page"))

   (mk/leader-keys
     "w" '(:ignore t :which-key "Windows")
	 "wb" '((lambda () (interactive) (mk/browser-split-vertically)) :which-key "Start a browser")
     "wp" '(previous-window-any-frame :which-key "Previous window")
     "wx" '(delete-window :which-key "Delete window")
	 "wk" '(delete-window-internal :which-key "Delete window")
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
     "vs" '(magit-status :which-key "Status"))

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
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method #'bitmap))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package prescient
  :after ivy)

(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode))

;;  query stackoverflow
(use-package sx
	:commands sx-search)

(use-package dumb-jump
  :hook (prog-mode . dumb-jump-mode)
  :config
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

;; Setup Functions
(defun mk/setupProgrammingSettings ()
  "Programming mode"
  ; (define-key evil-motion-state-map (kbd "M-<left>") 'xref-pop-marker-stack)
  (setq company-mode t
		electric-pair-mode t			;; Auto insert pairs {} () [] etc
		highlight-indent-guides-mode t	;; Turn on indent-guides
		indicate-empty-lines t			;; Show empty lines
		indicate-unused-lines t			;; Show unused lines
		semantic-mode t					;; Get a little extra help for autocompletion
		show-trailing-whitespace t		;; Show trailing whitespaces
		column-number-mode t			;; Show current line number highlighted
		display-line-numbers t))		;; Show line numbers

(defun mk/setupOrgMode ()
  (setq word-wrap t))

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
	(s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)
  )

(defun with-fileicon (icon str &optional height v-adjust)
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

(add-hook 'prog-mode-hook #'mk/setupProgrammingSettings)
(add-hook 'org-mode-hook #'mk/setupOrgMode)

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

(defvar mk-swift--title (with-faicon "apple" "" 1.5 -0.225))
(major-mode-hydra-define swift-mode
  (:color amaranth :title mk-swift--title)
  ("Build/test:"
	 (("r" xcode-run "Run" :exit t)
      ("b" xcode-build "Build" :exit t)
      ("t" xcode-test "Test" :exit t))
	 "Help"
	 (("." swift-helpful "Describe" :exit t)
	  ("o" lsp-ui-imenu "Overview" :exit t)
	  ("e" lsp-treemacs-error-list "Error list" :exit t))))

(provide 'init)

;;; init.el ends here
