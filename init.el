;;; init.el --- My init.el

;;; Code:

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)            ; Disable tooltips
(set-fringe-mode 10)         ; Give some breathing room
(menu-bar-mode 1)           ; Disable the menu bar
(display-time-mode 1)        ; Show time
(display-battery-mode t)     ; Show battery

(setq display-time-format "%H:%M")
(setq package-enable-at-startup nil) ; Dont load packages at start up
(setq display-time-default-load-average nil) ; Dont show avg load

;; saving
(desktop-save-mode 1)
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'kill-ring)

;; Set up the visible bell
(setq visible-bell t)

;; line-numbers
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(shell-mode-hook
		term-mode-hook
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
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.05
	which-key-max-display-columns 2))

(defun custom/evil-hook ()
  (dolist (mode '(term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

; Use evil mode
(use-package evil
  :init
  :hook (evil-mode . custom/evil-hook)
  (setq evil-want-integration t)
  (setq evil-want-keybinding t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

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

(use-package treemacs)

;; Theming
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons)
;; (use-package spaceline)
;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))

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

; On macos change title bar
(when (eq system-type 'darwin)
  (use-package ns-auto-titlebar)
  (use-package swift-mode)
  (ns-auto-titlebar-mode))

; helpful
(use-package helpful)

; general
(use-package general)
;; smex
(use-package smex)

(defmacro general-global-menu-definer (def infix-key &rest body)
  "Create a definer named general-global-DEF wrapping global-definer.
The prefix map is named 'my-DEF-map'.
Argument DEF "
  `(progn
     (general-create-definer ,(intern (concat "general-global-" def))
       :wrapping global-definer
       :prefix-map (quote ,(intern (concat "my-" def "-map")))
       :infix ,infix-key
       :wk-full-keys nil
       "" '(:ignore t :which-key ,def))
     (,(intern (concat "general-global-" def))
      ,@body)))

(general-create-definer global-definer
  :keymaps 'override
  :states  '(insert emacs normal hybrid motion visual operator)
  :prefix  "SPC"
  :non-normal-prefix "S-SPC")

(global-definer
  "TAB" '((lambda () (interactive) (switch-to-buffer nil)) :which-key "toggle buffers")
  "SPC" '(counsel-M-x :which-key "M-x")
  "0" '(treemacs :which-key "treemacs")
  "s" 'swiper
  "!" 'shell-command
  ":" 'eval-expression)

(general-global-menu-definer
 "files" "f"
 "s" '(save-buffer :which-key "save file")
 "f" '(find-file :which-key "find file...")
 "e" '((lambda () (interactive) (find-file user-init-file)) :which-key "user configuration"))

(general-global-menu-definer
 "code" "c"
 "p" 'check-parens
 "l" '(comment-line :which-key "comment line")
 "r" '(comment-region :which-key "comment region"))

(general-global-menu-definer
 "quit" "q"
 "q" 'save-buffers-kill-terminal)

(general-global-menu-definer
 "help" "h"
 "c" '(helpful-command :which-key "describe command")
 "k" '(helpful-key :which-key "describe key")
 "f" '(helpful-callable :which-key "describe function")
 "v" '(helpful-variable :which-key "describe variable")
 "p" '(helpful-at-point :which-key "describe at-point"))

(general-global-menu-definer
 "packages" "P"
 "i" '(counsel-package :which-key "install Package"))

(general-global-menu-definer
 "windows" "w"
 "x"  '(kill-buffer-and-window :which-key "kill window")
 "h" '(split-window-below :which-key "split horizontally")
 "v" '(split-window-right :which-key "split vertically"))

(general-global-menu-definer
 "buffer" "b"
 "b" '(counsel-switch-buffer :which-key "list buffers")
 "x" '(delete-window :which-key "close buffer")
 "d" '(kill-current-buffer :which-key "kill current buffer")
 "p" '(previous-buffer :which-key "previous buffer")
 "n" '(next-buffer :which-key "next buffer")
 "m" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "messages-buffer")
 "s" '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "scratch-buffer"))

;; restart-emacs
(use-package restart-emacs
  :general
  (general-global-quit
    "r"    'restart-emacs))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(solaire-mode company general spaceline-all-the-icons spaceline all-the-icons doom-themes ivy evil which-key use-package))
 '(warning-suppress-log-types '((frameset) (frameset) (use-package) (use-package)))
 '(warning-suppress-types '((frameset) (use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here
