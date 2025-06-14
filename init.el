;;; init.el --- optimized init file -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Personal Emacs configuration with optimized startup and modern packages.
;; Features include Evil mode, LSP support, project management, and development tools.

;;; Code:
(eval-when-compile
  (defvar display-time-24hr-format t)
  (defvar display-time-default-load-average nil))

(package-initialize)

;; Add themes directory to custom theme load path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "localpackages/kanagawa-emacs" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "localpackages/mito-laser-emacs" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "localpackages/neofusion-emacs" user-emacs-directory))

(let ((paths '("/opt/homebrew/bin"
               "/opt/homebrew/sbin"
               "/usr/local/bin"
               "/System/Cryptexes/App/usr/bin"
               "/usr/bin"
               "/bin"
               "/usr/sbin"
               "/sbin"
               "~/.local/bin"
               "~/.nvm/versions/node/v22.11.0/bin"
               "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
               "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
               "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin")))
  (setenv "PATH" (string-join paths ":")
          (setq exec-path paths)))

;; Add local packages directory to load-path
(let ((dir (expand-file-name "localpackages" user-emacs-directory)))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (let ((default-directory dir))
      (normal-top-level-add-subdirs-to-load-path))))

(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key [C-wheel-up])
(global-unset-key [C-wheel-down])
(global-set-key (kbd "M-w") 'ns-do-hide-emacs)

(use-package use-package
  :custom
  (use-package-verbose nil)
  (use-package-expand-minimally t)
  (use-package-always-ensure t)
  (use-package-compute-statistics t)
  (use-package-minimum-reported-time 0.02))

(use-package no-littering)

(use-package emacs
  :hook (after-init . (lambda ()
                        (global-hl-line-mode 1)
                        (display-battery-mode 1)
                        (global-auto-revert-mode 1)
                        (pixel-scroll-precision-mode 1)))
  :custom
  (set-window-margins (selected-window) 5 5)
  (set-display-table-slot standard-display-table 0 ?\ )
  (column-number-mode nil)
  (line-number-mode nil)
  (delete-by-moving-to-trash t)
  (make-backup-files nil)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (visible-bell nil)
  (window-combination-resize t)
  (auto-revert-avoid-polling t) ;; Automatically reread from disk if the underlying file changes
  (auto-revert-check-vc-info t)
  (help-window-select t)
  (grep-command "rg -nS --no-heading ")
  (fringes-outside-margins nil)
  (global-auto-revert-non-file-buf t)
  (find-file-visit-truename nil)
  (display-time-default-load-average nil) ; this information is useless for most
  (debug-on-error nil)
  (cursor-in-non-selected-windows nil)
  (create-lockfiles nil)
  (confirm-kill-processes t)
  (completion-ignore-case t)
  (backward-delete-char-untabify-method 'hungry)
  (backup-by-copying t)
  (auto-save-no-message t)
  (auto-save-timeout 20)
  (auto-window-vscroll nil)
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "var/auto-save/" user-emacs-directory) t)))
  (auto-save-list-file-prefix (expand-file-name "var/auto-save/.saves-" user-emacs-directory))
  :config
  (setq confirm-kill-emacs (lambda (prompt)
                             (y-or-n-p-with-timeout prompt 2 nil)))
  (setopt history-length 300)
  (setopt indicate-buffer-boundaries nil)
  (setopt indicate-empty-lines nil)
  (setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
  (setopt kept-new-versions 6)
  (setopt kept-old-versions 2)
  (setopt kill-do-not-save-duplicates t)
  (setopt large-file-warning-threshold (* 15 1024 1024))
  (setopt line-move-visual nil)
  (setopt read-answer-short t)
  (setopt read-buffer-completion-ignore-case t)
  (setopt read-extended-command-predicate #'command-completion-default-include-p)
  (setopt recentf-auto-cleanup (if (daemonp) 300 'never))
  (setopt recentf-max-menu-items 15)
  (setopt recentf-max-saved-items 300)
  (setopt redisplay-skip-fontification-on-input nil)
  (setopt ring-bell-function 'ignore)
  (setopt split-width-threshold 300)
  (setopt switch-to-buffer-obey-display-actions t)
  (setopt use-short-answers t)
  (setopt version-control t)
  (setopt warning-minimum-level :emergency)
  (setopt xref-search-program 'ripgrep)
  (setopt fringes-outside-margins t)
  (setopt grep-find-ignored-directories
          '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  (setopt backup-directory-alist `(("." . "~/.saves")))
  (setopt custom-file (expand-file-name "var/custom.el" user-emacs-directory))
  (setopt delete-old-versions t)
  (setopt ad-redefinition-action 'accept)
  (when (eq system-type 'darwin)
    (setq
     mac-command-modifier 'meta
     mac-option-modifier 'none
     mac-command-key-is-meta t
     ns-use-proxy-icon nil
     ns-pop-up-frames nil
     ns-use-thin-smoothing t
     dired-use-ls-dired t
     insert-directory-program "/opt/homebrew/bin/gls"
     dired-listing-switches "-aBhl --group-directories-first"
     browse-url-browser-function #'mk/browser-split-window)
    ;; (setq mac-redisplay-dont-reset-vscroll t
    ;;       mac-mouse-wheel-smooth-scroll nil)
    ))

(use-package compile-angel
  :ensure t
  :demand t
  :config
  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose t)

  ;; Uncomment the line below to compile automatically when an Elisp file is saved
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files before they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode))

(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-listing-switches "-aBhl --group-directories-first")
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(use-package rg
  :defer t)

(use-package spinner
  :defer t)

(use-package async
  :defer t
  :config
  (async-bytecomp-package-mode 1))

(use-package request
  :defer t
  :ensure t)

(use-package nerd-icons
  :defer t
  :custom
  (nerd-icons-scale-factor 1.0))

(use-package drag-stuff
  :defer t)

(use-package weather-scout
  :defer t
  :commands (weather-scout-show-forecast)
  :init
  ;; Use Vim motions in the weather forecast buffer (optional)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'weather-scout-mode 'motion)))

(use-package ultra-scroll
  :defer t
  :vc (ultra-scroll
       :url "https://github.com/jdtsmith/ultra-scroll"
       :main-file "ultra-scroll.el"
       :branch "main"
       :rev :newest)
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :hook (after-init . ultra-scroll-mode))

(use-package dumb-jump
  :defer t
  :config
  (put 'dumb-jump-go 'byte-obsolete-info nil)
  (setq dumb-jump-window 'current
        dumb-jump-quiet t
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package magit
  :defer t
  :commands (magit-status magit-ediff-show-working-tree)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :init
  (setq magit-save-repository-buffers nil
        magit-repository-directories nil)
  :config
  (require 'magit-section)
  (setq magit-format-file-function #'magit-format-file-nerd-icons))

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :custom-face
  (magit-section-highlight ((t (:background "#181825"))))
  :config
  (add-to-list 'magit-delta-delta-args "--dark"))

(use-package isearch
  :ensure nil
  :defer t
  :config
  (setq isearch-allow-scroll t
        isearch-lazy-count t)
  (defun mk/project-search-from-isearch ()
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string)))
          (search-nonincremental-instead t))
      (isearch-update-ring isearch-string isearch-regexp)
      (isearch-done t t)
      (consult-ripgrep (project-root (project-current)) query)))

  (defun isearch-consult-line ()
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (consult-line query)))

  (defun isearch-with-region-or-thing ()
    "Use region as isearch text if active, otherwise use thing at point."
    (interactive)
    (setq isearch-case-fold-search nil) ;; Case sensitive search
    (if (region-active-p)
        (let ((region (buffer-substring-no-properties
                       (region-beginning)
                       (region-end))))
          (deactivate-mark)
          (isearch-yank-string region))
      (when-let ((thing (thing-at-point 'symbol)))
        (isearch-yank-string thing))))
  :bind
  (:map isearch-mode-map
        ("C-r" . isearch-repeat-backward)
        ("C-o" . isearch-occur)
        ("C-e" . iedit-mode)
        ("M-e" . isearch-edit-string)
        ("M-f" . isearch-consult-line)
        ("C-f" . (lambda ()
                   (interactive)
                   (condition-case nil
                       (mk/project-search-from-isearch)
                     (quit (isearch-abort)))))
        ("C-d" . isearch-with-region-or-thing)))

(use-package iedit
  :defer t
  :bind
  (:map iedit-mode-keymap
        ("C-<return>" . iedit-toggle-selection)
        ("C-j" . iedit-next-occurrence)
        ("C-k" . iedit-prev-occurrence)
        ("C-g" . iedit--quit))
  :ensure t)

(use-package emacs-websearch
  :defer t
  :commands emacs-websearch
  :vc (emacs-websearch
       :url "https://github.com/zhenhua-wang/emacs-websearch"
       :branch "main"
       :rev :newest)
  :bind (("C-c l" . emacs-websearch))
  :config
  (setq emacs-websearch-engine 'duckduckgo
        emacs-websearch-async t))

;; (use-package candyshop
;;   :ensure nil
;;   :defer 2
;;   ;; :hook (after-init . candyshop-mode)
;;   :bind ("C-c t c" . candyshop-toggle)
;;   :config
;;   (setq candyshop-alpha-values '(100 95)))


(defun setup-programming-mode ()
  "Setup programming mode."
  ;; CONSIDER: Moving keybindings to a separate key-binding file

  (defun infer-indentation-style ()
    "Default to no tabs, but use tabs if already in project"
    (let ((space-count (how-many "^  " (point-min) (point-max)))
          (tab-count   (how-many "^\t" (point-min) (point-max))))
      (if (> space-count tab-count) (setq-default indent-tabs-mode nil))
      (if (> tab-count space-count) (setq-default indent-tabs-mode t))))

  (setq-default indent-tabs-mode nil)
  (infer-indentation-style)
  
  ;; (local-set-key (kbd "M-+") #'mk/toggle-flycheck-errors)
  (setq indicate-unused-lines nil
        word-wrap nil
        show-trailing-whitespace nil
        truncate-lines t))

(defun mk/safe-kill-buffer-and-window ()
  "Kill the current buffer and delete its window if it's not the last one."
  (interactive)
  (let* ((window-to-delete (selected-window))
         (buffer-to-kill (current-buffer))
         (frame (selected-frame))
         (windows (window-list frame)))
    (if (and (> (length windows) 1)
             (cl-some (lambda (win)
                        (and (not (eq win window-to-delete))
                             (eq (window-buffer win) buffer-to-kill)))
                      windows))
        (delete-window window-to-delete)
      (if (= 1 (length windows))
          (kill-buffer buffer-to-kill)
        (when (kill-buffer buffer-to-kill)
          (delete-window window-to-delete))))))

(use-package aggressive-indent
  :defer t
  :commands aggressive-indent-mode
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;; Highlights function and variable definitions in Emacs Lisp mode
(use-package highlight-defined
  :defer t
  :commands highlight-defined-mode
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

;; (use-package paredit
;;   :defer t
;;   :commands paredit-mode
;;   :hook
;;   (emacs-lisp-mode . paredit-mode)
;;   :config
;;   (define-key paredit-mode-map (kbd "RET") nil))

(use-package page-break-lines
  :defer t
  :commands (page-break-lines-mode
             global-page-break-lines-mode)
  :hook
  (emacs-lisp-mode . page-break-lines-mode))

(use-package window
  :ensure nil
  :bind (("C-x C-f" . toggle-frame-fullscreen)
         ("C-x C-w" . toggle-frame-maximized)
         ("C-x C-s" . window-toggle-side-windows)
         ("C-x C-x" . mk/safe-kill-buffer-and-window))
  :custom
  (transient-display-buffer-action
   '(display-buffer-below-selected
     (window-height . fit-window-to-buffer)))
  (window-resize-pixelwise nil)
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)  ;; (window-combination-resize t)
  ;; (even-window-sizes 'height-only)
  ;; (window-sides-vertical nil)
  ;; (fit-window-to-buffer-horizontally t)
  ;; (window-resize-pixelwise t)
  ;; (fit-frame-to-buffer t)
  (display-buffer-alist
   '(("\\*Async Shell Command\\*" (display-buffer-no-window))
     ("\\*xwidget\\*\\|\\*xref\\*"
      (display-buffer-in-side-window display-buffer-reuse-mode-window display-buffer-reuse-window)
      (body-function . select-window)
      (window-width . 0.4)
      (side . left))
     ("evil-marks\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
      (window-width . 0.10)
      (side . right)
      (slot . 0))
     ("\\*iOS Simulator\\|\\*swift package\\|\\*ios-device"
      (display-buffer-reuse-window display-buffer-in-side-window display-buffer-at-bottom)
      (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
      (window-parameters . ((mode-line-format . none)))
      (slot . 4))
     ("\\*Embark*"
      (display-buffer-in-side-window display-buffer-reuse-mode-window display-buffer-at-bottom)
      (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
      (window-parameters . ((select-window . t)))
      ;; (dedicated . t)
      (slot . 5))
     ("\\*Copilot Chat*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-width . 0.45)
      (side . right)
      (slot . 2)
      (window-parameters . ((mode-line-format . none))))
     ("\\*Periphery\\*\\|\\*compilation\\*"
      (display-buffer-reuse-window display-buffer-in-side-window display-buffer-at-bottom)
      (body-function . select-window)
      (window-height . 0.2)
      (window-width . 0.30)
      (slot . 1))
     ("\\*Android Emulator\\*\\|\\*Android Logcat\\*\\|\\*Android Emulator Error\\*"
      (display-buffer-reuse-window display-buffer-in-side-window display-buffer-at-bottom)
      (body-function . select-window)
      (window-height . 0.2)
      (window-width . 0.70)
      (slot . 2))
     ("\\*Faces\\|[Hh]elp\\*\\|\\*Copilot*\\|\\*occur\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-width . 0.4)
      (side . right)
      (slot . 1))
     ("\\*e?shell\\|*ellama\\|\\*vterm\\*"
      (display-buffer-at-bottom display-buffer-reuse-window)
      (body-function . select-window)
      (window-height . 0.13)
      (window-parameters . ((mode-line-format . none)))
      (side . bottom)
      (slot . 10))
     ("\\*\\(Flycheck\\|Package-Lint\\).*"
      (display-buffer-reuse-window display-buffer-below-selected)
      (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
      (dedicated . t)
      ;; (preserve-size . (t . t))
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none)))))))

(use-package welcome-dashboard
  :ensure nil
  :config
  (setq welcome-dashboard-latitude 56.7365
        welcome-dashboard-longitude 16.2981
        welcome-dashboard-use-nerd-icons t
        welcome-dashboard-max-number-of-projects 8
        welcome-dashboard-show-weather-info t
        welcome-dashboard-use-fahrenheit nil
        welcome-dashboard-max-left-padding 1
        welcome-dashboard-max-number-of-todos 5
        welcome-dashboard-path-max-length 70
        welcome-dashboard-min-left-padding 10
        welcome-dashboard-image-file "~/.emacs.d/themes/emacs.png"
        welcome-dashboard-image-width 200
        welcome-dashboard-image-height 200
        welcome-dashboard-title "Welcome Mikael. Have a great day!")
  (welcome-dashboard-create-welcome-hook))

(when (fboundp 'set-message-beep)
  (set-message-beep 'silent))

(use-package ligature
  :defer t
  :config
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                       "<:<" ";;;")))


(use-package autothemer
  :init
  ;; (load-theme 'catppuccin-latte t)
  ;; (load-theme 'catppuccin-macchiato t)
  ;; (load-theme 'catppuccin-frappe t)
  ;; (load-theme 'catppuccin-mocha t)
  ;; (load-theme 'ef-deuteranopia-dark t)
  ;; (load-theme 'rose-pine t)
  ;; (load-theme 'oxographite t)
  ;; (load-theme 'kman t)
  ;; (load-theme 'kalmar-night t)
  ;; (load-theme 'kanagawa t)
  ;; (load-theme 'neofusion t)
  ;; (load-theme 'doom-gruvbox t)
  (load-theme 'oxocarbon t)
  ;; (load-theme 'nordic t)
  ;; (load-theme 'poimandres t)
  ;; (load-theme 'mito-laser t)
  ;; (load-theme 'doom-outrun-electric t)
  ;; (load-theme 'doom-laserwave t)
  )

(use-package which-key
  :defer 2
  :ensure nil
  :hook (after-init . which-key-mode)
  :custom
  (which-key-separator " → ")
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 2) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display, because the default is only 1
  (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 45)
  (which-key-allow-imprecise-window-fit nil))

;; In use-package, :custom needs direct variable-value pairs without setq. Here's the correct way:

(use-package prog-mode
  :ensure nil
  :hook ((emacs-lisp-mode . electric-indent-mode)
         (prog-mode . electric-pair-mode)
         (prog-mode . drag-stuff-mode)
         (prog-mode . dumb-jump-mode)
         (prog-mode . hs-minor-mode)
         (prog-mode . setup-programming-mode)
         (prog-mode . display-line-numbers-mode)
         (prog-mode . global-ligature-mode)
         (prog-mode . global-prettify-symbols-mode))
  :config
  (setopt display-line-numbers-type 'relative
          display-line-numbers-widen t
          display-line-numbers-width 4))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package vertico
  :defer t
  :hook (after-init . vertico-mode)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)
        ("C-d" . vertico-scroll-down)
        ("C-u" . vertico-scroll-up)
        ("C-g" . abort-recursive-edit))
  :config
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face '(:inherit font-lock-operator-face :weight black))
                   "  ")
                 cand)))
  (setq vertico-resize t
        vertico-count 11
        vertico-cycle t))

(use-package vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-parameters
        '((internal-border-width . 2)
          (undecorated . t)
          (alpha . 95))
        vertico-posframe-poshandler #'posframe-poshandler-frame-center
        vertico-posframe-min-height 1
        vertico-posframe-truncate-lines t
        vertico-posframe-min-width 120
        vertico-posframe-border-width 20)
  (setq vertico-multiform-commands
        '((consult-line (:not posframe))
          (xref-find-references (:not posframe))
          (consult-eglot-symbols (:not posframe))
          (consult-ripgrep (:not posframe))
          (consult-org-heading (:not posframe))
          (consult-xref (:not posframe))
          (consult-imenu (:not posframe))
          (t posframe)))
  (vertico-multiform-mode)
  (vertico-posframe-cleanup))

(use-package marginalia
  :after (vertico vertico-posframe)
  :config
  (marginalia-mode))

;; Configure Directory extension.
(use-package vertico-directory
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :commands (find-file)
  :ensure nil
  :bind (:map vertico-map
              ("<tab>" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package nerd-icons-completion
  :after (nerd-icons marginalia)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon t)
  (setq nerd-icons-ibuffer-human-readable-size t))

(use-package orderless
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        orderless-matching-styles '(orderless-literal orderless-regexp)
        completion-category-overrides
        '((file (styles partial-completion)))))

(defun consult-all-project-files ()
  "Show all project files immediately with live preview."
  (interactive)
  (consult--read (project-files (project-current t))
                 :prompt "Project file: "
                 :category 'file
                 :state (consult--file-state)
                 :require-match t))

(use-package consult
  :defer t
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (
   ("M-f" . (lambda () (interactive) (consult-line (thing-at-point 'symbol))))
   ;; ("M-f" . consult-line)
   ("<backtab>" . #'consult-buffer)
   ("C-c C-a" . #'consult-apropos)
   ("C-c m m" . #'consult-imenu-multi)
   ("C-c m b" . #'consult-imenu)
   ("C-<tab>" . #'consult-project-buffer)
   ("M-R" . #'consult-recent-file)
   :map isearch-mode-map
   ("C-M-e" . #'consult-isearch-history)
   ("M-s e" . #'consult-isearch-history)
   ("M-s L" . #'consult-line-multi)
   ("M-s l" . #'consult-line))
  :init
  (setq register-preview-delay 0.2
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package consult-xref
  :defer t
  :ensure consult
  :config
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

(use-package embark
  :commands (embark-act embark-collect-snapshot embark-collect-live)
  :bind
  (:map minibuffer-local-map
        ("C-." . embark-act))
  (:map embark-collect-mode-map
        (("C-." . embark-act)))
  :custom
  (embark-quit-after-action nil))

(use-package embark-consult
  :after (:all embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :defer t)

(use-package visual-replace
  :defer t
  :bind (("C-c r" . visual-replace)
         :map isearch-mode-map
         ("C-c r" . visual-replace-from-isearch)))

(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 40
        recentf-max-menu-items 40
        recentf-auto-cleanup 'never))

(use-package mode-line-hud
  :ensure nil
  :config
  (setq show-in-echo-area nil))

(use-package punch-line
  :ensure nil
  :defer t
  :bind(("C-x t n" . punch-line-what-am-i-doing-next)
        ("C-x t N" . punch-line-what-am-i-doing-next-task)
        ("C-x t d" . punch-line-what-am-i-doing-done)
        ("C-x t a" . punch-line-what-am-i-doing-show-all)
        (:repeat-map punch-line-repeat-map
                     ("n" . punch-line-what-am-i-doing-next)
                     ("N" . punch-line-what-am-i-doing-next-task)
                     ("d" . punch-line-what-am-i-doing-done)
                     ("a" . punch-line-what-am-i-doing-show-all)))
  :hook ((after-init . punch-line-mode)
         (after-init . punch-weather-update)
         (after-init . punch-load-tasks))
  :config
  (setq punch-show-project-info nil
        punch-line-modal-use-fancy-icon t
        punch-line-modal-divider-style 'ice
        punch-line-modal-size 'medium
        punch-line-left-separator "  "
        punch-line-right-separator "  "
        punch-show-git-info t
        punch-show-lsp-info t
        punch-show-copilot-info nil
        punch-show-battery-info t
        punch-show-flycheck-info t
        punch-show-weather-info t
        punch-weather-latitude "56.7365"
        punch-weather-longitude "16.2981"
        punch-line-music-max-length 80
        punch-line-music-info '(:service apple)))

(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-want-Y-yank-to-eol t)
  :init
  (setq-default evil-symbol-word-search t)
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-undo-system 'undo-fu
	evil-want-fine-undo t
	evil-want-C-u-scroll t
	evil-want-minibuffer t
	evil-respect-visual-line-mode t
	evil-search-module 'evil-search
	evil-vsplit-window-right t
	evil-ex-search-persistent-highlight nil  ;; Don't keep highlights after search
	evil-ex-search-case 'smart  ;; Smart case sensitivity
	evil-ex-search-vim-style-regexp t
	evil-split-window-below t
	evil-kill-on-visual-paste nil
	evil-default-cursor t
        evil-echo-state nil
 	evil-want-C-i-jump t)
  :config
  (setq evil-leader/in-all-states t)

  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'execute-extended-command)
  (evil-define-key 'normal 'global (kbd "<leader> .") 'embark-act)
  (evil-define-key 'normal 'global (kbd "<leader> P") 'package-install)
  (evil-define-key 'normal 'global (kbd "<leader> S") 'consult-line-multi)
  (evil-define-key 'normal 'global (kbd "<leader> F") 'consult-line)

  (evil-define-key 'normal 'global (kbd "<leader>TAB") '(lambda () (interactive) (switch-to-buffer nil)))
  (evil-define-key 'normal 'global (kbd "<leader>'") '(lambda () (interactive) (toggle-vterm)))

  (evil-define-key 'normal 'global (kbd "<leader> a a") 'android-emulator-start-logcat)
  (evil-define-key 'normal 'global (kbd "<leader> a q") 'android-emulator-quit-logcat)
  (evil-define-key 'normal 'global (kbd "<leader> a r") 'android-emulator-restart-logcat)
   ;;; Buffers
  (evil-define-key 'normal 'global (kbd "<leader> b b") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b x") 'bury-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b i") 'ibuffer)
  (evil-define-key 'normal 'global (kbd "<leader> b k") 'kill-current-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b p") 'project-list-buffers)
  (evil-define-key 'normal 'global (kbd "<leader> b m") '(lambda () (interactive) (switch-to-buffer "*Messages*")))
  (evil-define-key 'normal 'global (kbd "<leader> b s") '(lambda () (interactive) (switch-to-buffer "*scratch*")))

  (evil-define-key 'normal 'global (kbd "<leader> c p") 'copilot-chat-transient)
  (evil-define-key 'normal 'global (kbd "<leader> c c") 'claude-code-transient)
  (evil-define-key 'normal 'global (kbd "<leader> c a") 'aidermacs-transient-menu)
  (evil-define-key 'normal 'global (kbd "<leader> c e") 'consult-compile-error)
  (evil-define-key 'normal 'global (kbd "<leader> c m") 'copilot-chat-insert-commit-message)

  (evil-define-key 'normal 'global (kbd "<leader> f b") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> f d") 'delete-file)
  (evil-define-key 'normal 'global (kbd "<leader> f e") '(lambda () (interactive) (find-file user-init-file)))
  (evil-define-key 'normal 'global (kbd "<leader> f f") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader> f o") 'consult-find)
  (evil-define-key 'normal 'global (kbd "<leader> f l") 'consult-focus-lines)
  (evil-define-key 'normal 'global (kbd "<leader> f n") 'create-file-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> f r") 'consult-recent-file)
  (evil-define-key 'normal 'global (kbd "<leader> f m") 'focus-mode)
  (evil-define-key 'normal 'global (kbd "<leader> f s") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> f i") 'consult-imenu-multi)

  (evil-define-key 'normal 'global (kbd "<leader> h s") 'highlight-symbol-at-point)
  (evil-define-key 'normal 'global (kbd "<leader> h r") 'highlight-symbol-remove-all)
  (evil-define-key 'normal 'global (kbd "<leader> h n") 'highlight-symbol-next)
  (evil-define-key 'normal 'global (kbd "<leader> h N") 'highlight-symbol-prev)

  (evil-define-key 'normal 'global (kbd "<leader> j j") 'jira-issues)
  (evil-define-key 'normal 'global (kbd "<leader> j m") 'jira-issues-menu)
  (evil-define-key 'normal 'global (kbd "<leader> j a") 'jira-issues-actions-menu)

  (evil-define-key 'normal 'global (kbd "<leader> m i") 'imenu-list)
  (evil-define-key 'normal 'global (kbd "<leader> m I") 'consult-imenu)
  (evil-define-key 'normal 'global (kbd "<leader> m b") 'consult-bookmark)
  (evil-define-key 'normal 'global (kbd "<leader> m c") 'consult-mode-command)

  (evil-define-key 'normal 'global (kbd "<leader> g g") 'google-this)

  ;; Eval
  (evil-define-key 'normal 'global (kbd "<leader> e b") 'eval-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> e l") 'eval-last-sexp)

  (evil-define-key 'normal 'global (kbd "<leader> s s") 'isearch-forward)
  (evil-define-key 'normal 'global (kbd "<leader> s h") 'consult-isearch-history)
  (evil-define-key 'normal 'global (kbd "<leader> s o") 'occur)

  (evil-define-key 'normal 'global (kbd "<leader> t s") 'sort-lines)
  (evil-define-key 'normal 'global (kbd "<leader> t x") 'delete-trailing-whitespace)
  (evil-define-key 'normal 'global (kbd "<leader> t w") '(lambda () (interactive) (whitespace-mode 'toggle)))

  (evil-define-key 'normal 'global (kbd "<leader> p f") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader> p d") 'project-dired)
  (evil-define-key 'normal 'global (kbd "<leader> p g") 'project-find-regexp)
  (evil-define-key 'normal 'global (kbd "<leader> p s") 'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader> p k") 'project-kill-buffers)
  (evil-define-key 'normal 'global (kbd "<leader> p b") 'consult-project-buffer)


  (evil-define-key 'normal 'global (kbd "<leader> v s") 'magit-status)
  (evil-define-key 'normal 'global (kbd "<leader> v b") 'magit-diff-buffer-file)
  (evil-define-key 'normal 'global (kbd "<leader> v a") 'vc-annotate)
  (evil-define-key 'normal 'global (kbd "<leader> v l") 'magit-log-buffer-file)

  (evil-define-key 'normal 'global (kbd "<leader> w w") 'weather-scout-show-forecast)

  (evil-define-key 'normal 'global (kbd "<leader> w w") 'weather-scout-show-forecast)
  (evil-define-key 'normal 'global (kbd "<leader> w t") 'window-transpose-layout)
  (evil-define-key 'normal 'global (kbd "<leader> w r") 'window-layout-rotate-clockwise)
  (evil-define-key 'normal 'global (kbd "<leader> w R") 'window-layout-rotate-anticlockwise)
  (evil-define-key 'normal 'global (kbd "<leader> w f") 'window-layout-flip-topdown)
  (evil-define-key 'normal 'global (kbd "<leader> w F") 'window-layout-flip-leftright)

  (evil-define-key 'normal 'global (kbd "<leader> q r") 'restart-emacs)
  (evil-define-key 'normal 'global (kbd "<leader> q q") 'save-buffers-kill-terminal)

  (defun mk/consult-ripgrep-at-point-project ()
    "Search for the word at point using consult-ripgrep from project root."
    (interactive)
    (consult-ripgrep (project-root (project-current)) (thing-at-point 'symbol)))

  (evil-define-key 'normal 'global (kbd "<leader> p F") 'mk/consult-ripgrep-at-point-project)

  (evil-select-search-module 'evil-search-module 'evil-search)
  (define-key evil-motion-state-map (kbd "<up>") 'ignore)
  (define-key evil-motion-state-map (kbd "<down>") 'ignore)
  (define-key evil-motion-state-map (kbd "<left>") 'ignore)
  (define-key evil-motion-state-map (kbd "<right>") 'ignore)

  (define-key evil-motion-state-map (kbd "C-+") #'(lambda () (interactive) (enlarge-window-horizontally 15)))
  (define-key evil-motion-state-map (kbd "C--") #'(lambda () (interactive) (shrink-window-horizontally 15)))
  (define-key evil-motion-state-map (kbd "C-M-+") #'(lambda () (interactive) (enlarge-window 3)))
  (define-key evil-motion-state-map (kbd "C-M--") #'(lambda () (interactive) (shrink-window 3)))
  (define-key evil-insert-state-map (kbd "TAB") #'tab-to-tab-stop)

  (setq evil-normal-state-cursor '(box "systemBlueColor")
        evil-insert-state-cursor '(bar "systemRedColor")
        evil-visual-state-cursor '(hollow "systemPurpleColor"))
  ;; (evil-set-initial-state 'minibuffer-mode 'emacs)

  (evil-define-key 'normal evil-ex-map "q" 'mk/safe-kill-buffer-and-window)
  (evil-mode 1))

(with-eval-after-load 'evil
  (dolist (state '(normal insert visual motion emacs))
    (evil-define-key state 'global (kbd "s-M") nil)
    (evil-define-key state 'global (kbd "C-.") nil)
    (evil-define-key state 'global (kbd "C-k") nil)))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-vterm-setup)
  :init
  (evil-collection-init))

(use-package evil-mc
  :hook (evil-mode . global-evil-mc-mode)
  :bind (
         ("C-M-e" . evil-mc-make-all-cursors)
         ("C-M-n" . evil-mc-make-and-goto-next-match)
         ("C-M-p" . evil-mc-make-and-goto-prev-match)
         ("C-M-j" . evil-mc-make-cursor-move-next-line)
         ("C-M-k" . evil-mc-make-cursor-move-prev-line))
  :custom
  (evil-mc-mode-line-text-inverse-colors t)
  (evil-mc-undo-cursors-on-keyboard-quit t)
  (evil-mc-mode-line-text-cursor-color t)
  :config
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg))

(use-package evil-matchit
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :after evil
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))

     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))

     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (after-init . global-evil-surround-mode))

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode 1))

(use-package evil-visualstar
  :after evil
  :hook (after-init . global-evil-visualstar-mode))

(use-package undo-fu
  :after evil
  :custom
  (setopt undo-limit (* 13 160000))
  (setopt undo-outer-limit (* 13 24000000))
  (setopt undo-strong-limit (* 13 240000))
  :config
  (setq undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :hook (after-init . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")
        undo-fu-session-file-limit 10))

(use-package rainbow-delimiters
  :ensure nil
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package colorful-mode
  :defer t
  :ensure t
  :hook (emacs-lisp-mode . colorful-mode)
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-alignment 'left)
  (colorful-prefix-string "●"))

(use-package expand-region
  :defer t
  :bind ("C-x e" . er/expand-region))

(use-package eldoc-box
  :commands (eldoc-box-help-at-point)
  :bind
  ("C-c K" . eldoc-box-help-at-point)
  :config
  (setq eldoc-box-only-multi-line nil
        eldoc-box-max-pixel-width 600
        eldoc-box-clear-with-C-g t)
  ;; (setq eldoc-box-buffer-hook
  ;;       (remove 'eldoc-box--fontify-html eldoc-box-buffer-hook))
  ;; ;; Prettify `eldoc-box' frame
  (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 10
        (alist-get 'internal-border-width eldoc-box-frame-parameters) 1
        (alist-get 'right-fringe eldoc-box-frame-parameters) 10))

(use-package focus
  :defer t
  :custom
  (focus-echo-keystrokes 0.1)
  (focus-echo-keystrokes-delay 0.1)
  (focus-echo-keystrokes-timeout 0.1)
  (focus-echo-keystrokes-timeout-delay 0.1))

(use-package ediff
  :defer t
  :after prog-mode
  :ensure nil
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  :custom
  (setq ediff-keep-variants nil)
  :config
  (winner-mode)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package corfu
  :hook (prog-mode . global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("<escape>" . corfu-quit)
        ("C-g" . corfu-quit)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("C-l" . corfu-complete)
        ("C-d" . corfu-show-documentation)
        ("C-e" . corfu-show-location)
        ("C-h" . corfu-show-location)
        ("M-RET" . corfu-insert)
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-separator ?\s)
  ;; (corfu-popupinfo-mode 'dedicated)
  :config
  (setq corfu-auto-delay 0.2          ; Reduced from 0.3
        corfu-preselect 'valid
        corfu-popupinfo-direction '(force-right)
        corfu-popupinfo-max-width 100
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-quit-at-boundary 'separator
        corfu-popupinfo-delay '(0.5 . 0.2)
        corfu-quit-no-match t)
  :init
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :if (< emacs-major-version 31)
  :unless (featurep 'tty-child-frames)
  :unless (display-graphic-p)
  :hook
  (corfu-mode-hook . corfu-terminal-mode))

(use-package kind-icon
  :after corfu
  :custom
  (setq kind-icon-default-face 'corfu-default)
  (setq kind-icon-blend-background t)
  (setq kind-icon-blend-frac 0.15)
  (setq kind-icon-use-icons t)
  (setq kind-icon-extra-space t)
  (kind-icon-default-style
   '(:padding -0.5 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
  ;; (kind-icon-formatted 'variable)
  :config
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
          (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
          (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
          (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
          (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
          (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
          (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
          (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
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
          (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode")))
  (defun my-kind-icon--extra-space (orig-fun &rest args)
    "Custom advice for `kind-icon--extra-space`."
    (let ((result (apply orig-fun args)))
      (propertize "  " 'display '(space :width 1.0)))) ; Adjusted width to 1.0
  (advice-add 'kind-icon--extra-space :around #'my-kind-icon--extra-space)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package savehist
  :defer 2
  :hook (after-init . savehist-mode)
  :config
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))

;; ;; Add extensions
(use-package cape
  :after evil
  :init
  ;; Add more Elisp specific backends
  (defun mk/setup-elisp-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'cape-elisp-symbol
                       #'cape-elisp-block
                       #'cape-dabbrev
                       #'cape-file
                       #'cape-keyword
                       #'elisp-completion-at-point))))
  (add-hook 'emacs-lisp-mode-hook #'mk/setup-elisp-capf)
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
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package darken-buffer
  :ensure nil
  :hook (after-init . darken-buffer-mode)
  :config
  (setq darken-buffer-percentage 0
        darken-buffer-ignore-buffers '("*Messages*" "*scratch*" "*iOS Simulator*" "*Android Emulator*")
        darken-buffer-always-darken-percentage 10
        lighten-inactive-buffer-percentage 3))

(use-package avy
  :defer t
  :bind ("M-g" . avy-goto-word-1)
  :config
  (setq avy-single-candidate-jump t))

(use-package dape
  :commands (dape-info dape-repl dape)
  :hook
  ((dape-compile . kill-buffer) ; Kill compile buffer on build success
   (dape-display-source . pulse-momentary-highlight-one-line) ; Pulse source line (performance hit)
   (dape-stopped . dape-info) ; To display info and/or repl buffers on stopped
   (dape-stopped . dape-repl))
  :custom
  (dape-buffer-window-arrangement 'left) ;; Info buffers to the left
  (dape-breakpoint-margin-string "●") ;; ◯
  (dape-inlay-hints t)
  :config
  (dape-breakpoint-global-mode)
  (put 'dape--overlay-arrow-position
       'overlay-arrow-string   ;; ▶ →
       (propertize "▶" 'face 'dape-stack-trace-face)))

(use-package repeat
  :defer t
  :ensure nil
  :config (add-hook 'dape-on-start-hooks #'repeat-mode)
  :custom
  (repeat-too-dangerous '(kill-this-buffer))
  (repeat-exit-timeout 5))

(use-package treemacs
  :defer t
  :commands (treemacs treemacs-select-window)
  :hook ((treemacs-mode . treemacs-project-follow-mode)
         (treemacs-mode . (lambda () (set-window-fringes (treemacs-get-local-window) 0 0 nil))))
  :bind (("M-J" . #'treemacs-find-file)
         ("M-0" . #'treemacs))
  :config
  (let* ((font-family "SF Pro Display")
         (font-height 0.8))
    (custom-set-faces
     `(treemacs-directory-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-directory-opened-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-directory-closed-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-file-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-file-collapsed-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-tags-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-directory-collapsed-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-git-ignored-face ((t (:family ,font-family :height ,font-height :slant italic))))
     `(treemacs-git-conflict-face ((t (:family ,font-family :height ,font-height :slant italic))))
     `(treemacs-git-unmodified-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-git-untracked-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-git-added-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-git-deleted-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-git-renamed-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-git-modified-face ((t (:family ,font-family :height ,font-height))))
     `(treemacs-tags-face ((t (:family ,font-family :height ,font-height))))))
  (setq treemacs-follow-after-init t
        treemacs-hide-gitignored-files-mode t
        treemacs-space-between-root-nodes 10
        treemacs-project-follow-cleanup t
        treemacs-width-is-initially-locked nil
        treemacs-collapse-dirs 3
        treemacs-display-in-side-window t
        treemacs-is-never-other-window nil
        treemacs-indentation 2
        treemacs-indentation-string " "
        treemacs-filewatch-mode t
        treemacs-git-mode 'deferred
        treemacs-move-files-by-mouse-dragging nil
        treemacs-move-forward-on-expand t
        treemacs-pulse-on-success t
        treemacs-file-event-delay 0
        treemacs-deferred-git-apply-delay 0
        treemacs-git-commit-diff-mode 1
        treemacs-sorting 'treemacs--sort-alphabetic-case-insensitive-asc
        treemacs-width 40))

(use-package treemacs-nerd-icons
  :after treemacs
  :functions treemacs-load-theme
  :preface
  (defun treemacs--propagate-new-icons (_theme))
  :custom-face (cfrs-border-color ((t (:inherit posframe-border))))
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package project-treemacs
  :after treemacs)

(use-package flycheck-package
  :defer t
  :after flycheck
  :hook (emacs-lisp-mode . flycheck-package-setup))

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :diminish t
  :bind
  ("C-c f n" . flycheck-next-error)
  ("C-c f p" . flycheck-previous-error)
  ("C-c f g" . flycheck-mode)
  :config
  (add-to-list 'flycheck-checkers 'javascript-eslint)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  ;; (setq-default flycheck-indication-mode 'left-margin)
  ;; (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)
  :custom
  (setq flycheck-check-syntax-automatically
        '(save mode-enabled idle-change)
        flycheck-idle-change-delay 1.0
        flycheck-idle-buffer-switch-delay 1.0
        flycheck-checker-cache "~/.flycheck-cache"
        flycheck-indication-mode nil)
  (flycheck-checker-error-threshold 100))

(use-package flycheck-overlay
  :ensure nil
  :hook (flycheck-mode . flycheck-overlay-mode)
  :bind ("C-c f l" . flycheck-overlay-toggle)
  :config (add-hook 'flycheck-mode-hook #'flycheck-overlay-mode)
  (setq flycheck-overlay-virtual-line-type 'curved-arrow
        flycheck-overlay-percent-darker 60
        flycheck-overlay-text-tint-percent 70
        flycheck-overlay-text-tint 'lighter
        flycheck-overlay-show-at-eol nil
        flycheck-overlay-wrap-messages t
        flycheck-overlay-max-lines 110
        flycheck-overlay-background-lightness 25
        flycheck-overlay-virtual-line-icon nil))

(use-package flycheck-eglot
  :ensure nil
  :hook (eglot-managed-mode . flycheck-eglot-mode)
  :custom (flycheck-eglot-exclusive nil))

(use-package consult-flycheck
  :defer t
  :bind ("M-+" . consult-flycheck))

(use-package emojify
  :defer t
  :hook (org-mode . global-emojify-mode))

(add-hook 'comint-mode-hook
          (lambda ()
            (setq-local comint-prompt-read-only t)
            (setq-local visual-line-mode t)))

(use-package pulsar
  :hook (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.025
        pulsar-iterations 10
        pulsar-face 'pulsar-cyan
        pulsar-highlight-face 'evil-ex-lazy-highlight
        pulsar-pulse-functions '(
                                 evil-yank
                                 evil-yank-line
                                 evil-delete
                                 evil-delete-line
                                 evil-jump-item
                                 evil-scroll-down
                                 evil-scroll-up
                                 evil-scroll-page-down
                                 evil-scroll-page-up
                                 evil-scroll-line-down
                                 evil-scroll-line-up
                                 evil-window-up
                                 evil-window-rotate-upwards
                                 evil-window-rotate-downwards
                                 evil-window-down
                                 evil-window-left
                                 evil-window-right
                                 evil-window-vsplit
                                 evil-window-split)))

(use-package imenu-list
  :after imenu-list
  :defer t
  :bind
  ("C-c i" . 'imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

(use-package darkroom
  :defer t
  :bind ("C-x C-d" . darkroom-tentative-mode)
  :config
  (setq darkroom-text-scale-increase 1.5
        darkroom-margins '(12 . 0)))

(use-package blamer
  :commands blamer-mode
  :config
  (setq blamer-tooltip-function 'blamer-tooltip-author-info)
  (setq blamer-view 'overlay
        blamer-type 'overlay-popup
        blamer-max-commit-message-length 270
        blamer-force-truncate-long-line nil
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
  :defer 2
  :hook (prog-mode . git-gutter-mode)
  :diminish git-gutter-mode
  :config
  (setq git-gutter:modified-sign "┃"
        git-gutter:added-sign "┃"
        git-gutter:deleted-sign "┃")
  (setq git-gutter:window-width 3)  ;; Set to minimum width
  (setq git-gutter:update-interval 2))

(use-package vterm
  :defer t
  :commands vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (setq vterm-timer-delay nil
        vterm-kill-buffer-on-exit t
	vterm-clipboard-warning-max-lines 20
	vterm-clipboard-warning-max-chars 256))

(use-package dall-e-shell
  :defer t
  :config
  (setq dall-e-shell-openai-key (shell-command-to-string "echo $OPENAI_API_KEY")))

(use-package chatgpt-shell
  :defer t
  :bind
  ("C-x C-v" . chatgpt-shell-quick-insert)
  ("C-x C-p" . chatgpt-shell-prompt-compose)
  ("C-x c g s" . chatgpt-shell-send-and-review-region)
  ("C-x c g r" . chatgpt-shell-refactor-code)
  ("C-x C-s" . (lambda ()
                 (interactive)
                 (split-window-right)
                 (other-window 1)
                 (chatgpt-shell)))
  :config
  (setf chatgpt-shell-anthropic-key (getenv "ANTHROPIC_API_KEY"))
  (setf chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
  (setf chatgpt-shell-deepseek-key (getenv "DEEPSEEK_API_KEY"))
  (setq chatgpt-shell-model-version "claude-3-5-sonnet-latest"))

(use-package ob-chatgpt-shell
  :ensure nil
  :commands (org-babel-execute:chatgpt-shell)
  :config
  (ob-chatgpt-shell-setup))


(use-package project
  :bind ("M-O" . project-find-file)
  :defer t
  :ensure nil
  :config
  (defun project-root-override (dir)
    "Find DIR's project root by searching for a '.project.el' file."
    (let ((root (or (locate-dominating-file dir ".xcodeproj")
                    (locate-dominating-file dir ".envrc")
                    (locate-dominating-file dir ".projectile")))
          (backend (ignore-errors (vc-responsible-backend dir))))
      (when root (if (version<= emacs-version "28")
                     (cons 'vc root)
                   (list 'vc backend root)))))
  (add-hook 'project-find-functions #'project-root-override))

(use-package paren
  :defer 2
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(setq org-custom-todo-faces '
      (("TODO" :background "#FF5D62" :distant-foreground "#FF5D62" :foreground "#FFFFFF" :weight 'bold)
       ("NEXT" :background "#7FB4CA" :distant-foreground "#7FB4CA" :foreground "#1c1c24" :weight 'bold)
       ("STARTED" :background "#957FB8" :foreground "#FFFFFF" :weight 'bold)
       ("DELEGATED" :background "#7AA89F" :foreground "#1c1c24" :weight 'bold)
       ("QA" :background "#54536D" :weight 'bold)))

(use-package org
  :hook ((org-mode . org-display-inline-images)
         (org-mode . my/org-prettify-symbols))
  :config
  (defun my/org-prettify-symbols ()
    (setq prettify-symbols-alist
	  (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
		  '(("#+begin_src"      . 9998)         ; ✎
		    ("#+end_src"        . " ")         ; 
		    ("#+results:"       . 9776)         ; ☰
		    ("#+attr_latex:"    . "🄛")
		    ("#+attr_html:"     . "🄗")
		    ("#+attr_org:"      . "🄞")
		    ("#+name:"          . "🄝")         ; 127261
		    ("#+caption:"       . "🄒")         ; 127250
		    ("#+startup:"       . 10034)        ; ✲
		    ("#+options:"       . 9965)         ; ⛭
		    ("#+begin_quote"    . 187)          ; »
		    ("#+end_quote"      . 171)          ; «
                    ("#+begin_results"  . 8943)         ; ⋯
                    ("#+end_results"    . 8943)         ; ⋯
		    )))
    (setq prettify-symbols-unprettify-at-point t)
    (prettify-symbols-mode 1))
  
  (setq org-ellipsis "…"
        ;; org-agenda-files '("work.org" "projekt.org")
        org-agenda-block-separator ?─
        org-agenda-files '("~/Desktop/org/agenda.org")
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-done t
        org-agenda-span 1
        org-agenda-start-day "+0d"
        org-agenda-tags-column 0
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-custom-commands '(("v" "Agenda and todos"
                                      ((tags "PRIORITY=\"A\""
                                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                              (org-agenda-overriding-header "HIGH PRIORITY unfinished tasks:")))
                                       (tags "PRIORITY=\"B\""
                                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                              (org-agenda-overriding-header "MEDIUM PRIORITY unfinished tasks:")))
                                       (tags "PRIORITY=\"C\""
                                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                              (org-agenda-overriding-header "LOW PRIORITY unfinished tasks:")))
                                       (agenda "")
                                       (alltodo ""))))
        org-auto-align-tags nil
        org-clock-sound "~/.emacs.d/etc/sound/bell.mp3"
        org-confirm-babel-evaluate nil
        org-cycle-separator-lines 2
        org-directory "~/Desktop/org/Todo/"
        org-fontify-emphasized-text t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-log-done 'time
        org-log-into-drawer t
        org-pretty-entities t
        org-return-follows-link t
        org-src-edit-src-content-indentation 0
        org-src-fontify-natively t
        org-fontify-whole-heading-line t
        org-src-preserve-indentation t
        font-lock-maximum-decoration t
        org-element-use-cache t
        org-src-tab-acts-natively t
        org-startup-folded nil
        org-startup-with-inline-images t
        org-tags-column 0
        org-todo-keyword-faces org-custom-todo-faces
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "STARTED(s)" "DELEGATED(d)" "QA(q)" "|" "DONE(d)" "CANCELLED(c)"))
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────"))

(defun mk/play-sound (orgin-fn sound)
  "Play a sound when a task is marked as done."
  (cl-destructuring-bind (_ _ file) sound
    (make-process :name (concat "play-sound-" file)
                  :connection-type 'pipe
                  :command `("afplay" ,file))))
(advice-add 'play-sound :around 'mk/play-sound)

(with-eval-after-load 'org

  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))

  (require 'org-tempo)
  (require 'ob-swiftui)
  (ob-swiftui-setup)

  (setq-local org-confirm-babel-evaluate t)

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.2 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.18 :weight bold))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.16 :weight bold))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.15 :weight bold))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.1 :weight bold))))
   '(org-level-6 ((t (:inherit outline-5 :height 1 :weight bold))))
   '(org-level-7 ((t (:inherit outline-5 :height 1 :weight semi-bold))))
   '(org-block-begin-line ((t (:foreground "#678" :underline t :background nil))))
   '(org-block-end-line ((t (:foreground "#678" :overline t :background nil )))))


  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (swift . t)
                                 (kotlin . t)
                                 (shell . t)
                                 (restclient . t)))

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sw" . "src swift"))
  (add-to-list 'org-structure-template-alist '("sr" . "src restclient"))
  (add-to-list 'org-structure-template-alist '("swiftui" . "src swiftui :view CustomView"))
  (add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))
  (add-to-list 'org-modules 'org-tempo t))

(use-package restclient
  :defer t)

(use-package ob-restclient
  :defer t)

(use-package ob-swift
  :defer t
  :after org-mode
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((swift . t)))))
(use-package ob-swiftui
  :defer t
  :after org-mode
  :config
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))
  (add-to-list 'org-babel-tangle-lang-exts
               '("swiftui" . "swift"))
  (add-to-list 'org-src-lang-modes
               '("swiftui" . swift))
  (ob-swiftui-setup))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :config
  (setq org-modern-todo-faces org-custom-todo-faces
        org-modern-block t
        org-modern-block-name nil
        org-modern-star '("❶" "❷" "❸" "❹" "❺" "❻" "❼")))

(use-package olivetti
  :hook ((org-mode . olivetti-mode))
  :custom
  (olivetti-style 'fancy)
  (setq olivetti-style t
        olivetti-body-width 80
        olivetti-hide-mode-line t))

(use-package elfeed
  :commands elfeed
  :config

  (defun add-icon-to-title (icon tag all-tags title)
    "Add ICON if TAG is present in ALL-TAGS to the TITLE string."
    (if (member tag all-tags)
        (concat icon " ")
      ""))

  (defun my/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (if (member 'unread (elfeed-entry-tags entry))
                            (elfeed-search--faces (elfeed-entry-tags entry))
                          '(:weight thin :inherit font-lock-comment-face)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed
                         (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (seq-filter
                  (lambda (tag) (not (string-equal feed-title tag)))
                  (mapcar #'symbol-name (elfeed-entry-tags entry))))
           (icons '(("swift" . "")
                    ("swiftui" . "")
                    ("emacs" . "")
                    ("neovim" . "")
                    ("artificialInteligence" . "")
                    ("singularity" . "")
                    ("kotlin" . "")
                    ("techcrunch" . "")))
           (title-with-icons (concat
                              (mapconcat
                               (lambda (icon-pair)
                                 (add-icon-to-title (cdr icon-pair) (car icon-pair) tags " "))
                               icons "")
                              " "
                              title))
           (tags-str (mapconcat
                      (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                      tags ","))
           (title-width (- (window-width) 10 elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title-with-icons (elfeed-clamp
                                            elfeed-search-title-min-width
                                            title-width
                                            elfeed-search-title-max-width)
                          :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
      ;; (when feed-title
      ;;   (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags
        (insert tags-str))))

  (setq elfeed-search-print-entry-function #'my/elfeed-search-print-entry)
  (setq elfeed-feeds '(
                       ("https://www.reddit.com/r/emacs.rss" emacs)
                       ("https://www.reddit.com/r/neovim.rss" neovim)
                       ("https://www.reddit.com/r/kotlin.rss" kotlin)
                       ("https://www.reddit.com/r/swift.rss" swift)
                       ("https://www.reddit.com/r/swiftui.rss" swiftui)
                       ("https://www.reddit.com/r/artificialInteligence.rss" artificialInteligence)
                       ;; ("https://techcrunch.com/rss" techcrunch)
                       ))

  (setq elfeed-search-face-alist
        '((emacs font-lock-function-name-face)
          (neovim font-lock-type-face)
          (kotlin font-lock-keyword-face)
          (swift font-lock-constant-face)
          (techcrunch font-lock-variable-name-face)
          (ai font-lock-number-face)
          (singularity font-lock-number-face)
          (read font-lock-comment-face)))
  (setq elfeed-search-filter "@4-days-ago +unread"
        elfeed-search-title-max-width 140
        elfeed-search-title-min-width 140))


(use-package highlight-quoted
  :hook
  (emacs-lisp-mode-hook . highlight-quoted-mode))

(use-package eros
  :custom-face
  (eros-result-overlay-face ((t (:inherit shadow :box t))))
  :hook
  (emacs-lisp-mode-hook . eros-mode))

(use-package highlight-symbol
  :defer t
  ;; :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.8
        highlight-symbol-highlight-single-occurrence nil))

;; Drag lines and regions around
(use-package drag-stuff
  :ensure t
  :defer t
  :bind (:map evil-visual-state-map
	      ("C-j" . drag-stuff-down)
	      ("C-k" . drag-stuff-up)))

(use-package swift-ts-mode
  :mode "\\.swift\\'"
  :ensure nil
  :bind
  (:map swift-ts-mode-map
        ("C-c t s" . #'swift-ts:split-func-list))
  :custom
  (swift-ts-basic-offset 4)
  (swift-ts:indent-trailing-call-member t)
  :config
  (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-ts-mode)))

(use-package localizeable-mode
  :ensure nil
  :after swift-ts-mode
  :mode "\\.strings\\'"
  :bind (:map localizeable-mode-map
              ("C-c C-c" . #'swift-additions:compile-and-run)
              ("C-c C-k" . #'periphery-run-loco)))

(use-package objc-mode
  :mode "\\.h\\'"
  :defer t
  :ensure nil)

(use-package ios-simulator
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
        ("M-s" . #'ios-simulator:terminate-current-app)
        ("C-x s n" . #'ios-simulator:send-notification)
        ("C-x s l" . #'ios-simulator:change-language)))

(use-package swift-additions
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
	("M-r" . #'swift-additions:run)
	("C-c t m" .  #'swift-additions:test-module-silent)
	("C-c t p" .  #'swift-additions:test-swift-package-from-file)
	("C-c C-c" . #'swift-additions:compile-and-run)
	("C-c C-b" . #'swift-additions:compile-app)
	("C-c C-x" . #'swift-additions:reset)
	("C-c C-f" . #'periphery-search-dwiw-rg)))

(use-package domain-blocker
  :ensure nil
  :after swift-ts-mode)

(use-package eglot
  :defer t
  :hook (((swift-ts-mode) . (lambda ()
                              (require 'swift-lsp)
                              (eglot-ensure)))
	 ((kotlin-mode kotlin-ts-mode) . (lambda () (eglot-ensure))))
  :ensure nil
  :bind
  ("C-c e f" . #'eglot-code-action-quickfix)
  ("C-c e a" . #'eglot-code-actions)
  ("C-c e e" . #'eglot-code-action-extract)
  ("C-c e R" . #'eglot-code-action-rewrite)
  ("C-c e r" . #'eglot-rename)
  ("C-c e m" . #'eglot-menu)
  ("C-c e d" . #'eglot-find-declaration)
  ("C-c e D" . #'eglot-find-typeDefinition)
  ("C-c e i" . #'eglot-find-implementation)
  ("C-c e b" . #'eglot-format-buffer)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-sync-connect 1)
  (eglot-connect-timeout 90)
  (eglot-report-progress nil)
  (eglot-events-buffer-size 0)
  (eglot-events-buffer-config '(size: 0 :format full))
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  :config
  (add-to-list 'eglot-server-programs '(swift-ts-mode . my-swift-mode:eglot-server-contact))
  (add-to-list 'eglot-server-programs
               '((typescript-mode typescript-tsx-mode tsx-ts-mode) . ("typescript-language-server" "--stdio")))
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  ;; (add-to-list 'eglot-server-programs
  ;;              '(kotlin-mode . ("~/kotlin-lsp/kotlin-0.252.16998/kotlin-lsp.sh" "--stdio")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '(kotlin-ts-mode . ("bash" "/Users/mikaelkonradsson/kotlin-lsp/kotlin-lsp.sh" "--stdio")))
  (setq eglot-stay-out-of '(corfu flycheck)
        jsonrpc-event-hook nil)
  (advice-add 'jsonrpc--log-event :override #'ignore))

(use-package xcode-additions
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
        ("M-K" .  #'xcode-additions:clean-build-folder)
        ("C-c C-d" . #'xcode-additions:start-debugging)
        ("C-c C-x" . #'swift-additions:reset)))

(use-package swift-refactor
  :ensure nil
  :after (swift-ts-mode kotlin-ts-mode)
  :bind
  (:map kotlin-ts-mode-map
        ("C-c r s" . #'code-refactor:split-function-list)
        ("M-t" . #'swift-refactor:insert-todo)
        ("M-m" . #'swift-refactor:insert-mark))
  (:map swift-ts-mode-map
        ("M-t" . #'swift-refactor:insert-todo)
        ("M-m" . #'swift-refactor:insert-mark)
        ("C-c x t" . #'xcode-additions:toggle-device-choice)
        ("C-c x c" . #'xcode-additions:show-current-configuration)
        ("C-c r a" . #'swift-refactor:wrap-selection)
        ("C-c r d" . #'swift-refactor:delete-current-line-with-matching-brace)
        ("C-c r i" . #'swift-refactor:tidy-up-constructor)
        ("C-c r r" . #'swift-refactor:extract-function)
        ("M-P" .  #'swift-refactor:print-thing-at-point)
        ("C-c r t" . #'swift-refactor:add-try-catch)))

(use-package apple-docs-query
  :ensure nil
  :defer t
  :bind
  ("C-c C-a" . #'apple-docs/query)
  ("C-c C-A" . #'apple-docs/query-thing-at-point))

(use-package hacking-with-swift
  :ensure nil
  :defer t
  :bind
  ("C-c C-h" . #'hacking-ws/query)
  ("C-c C-H" . #'hacking-ws/query-thing-at-point))

(use-package periphery-quick
  :defer 2
  :ensure nil
  :bind
  :bind (:map prog-mode-map
              ("C-c S" . #'periphery-quick:find-ask)
              ;; ("C-c f f" . #'periphery-quick:find-in-file)
              ("C-c f t" . #'periphery-quick:todos)))

(use-package periphery-search
  :defer 2
  :ensure nil
  :bind (:map prog-mode-map
              ("C-c C-s" . #'periphery-search-rg)
              ("C-c C-f" . #'periphery-search-dwiw-rg)
              ("C-x C-t" . #'periphery-query-todos-and-fixmes)
              ("C-x C-m" . #'periphery-query-marks)
              ("M-?" . #'periphery-toggle-buffer)))

(use-package periphery-swiftformat
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
        ("C-c C-o" . #'periphery-swiftformat-lint-buffer)
        ("M-o" . #'periphery-swiftformat-autocorrect-buffer)
        ("C-c C-p" . #'periphery-run-swiftformat-for-project)))

(use-package periphery-ktlint
  :ensure nil
  :after kotlin-ts-mode
  :bind
  (:map kotlin-ts-mode-map
        ("M-o" . #'periphery-ktlint-autocorrect-buffer)))

;; (use-package periphery-loco
;;   :ensure nil
;;   :after swift-ts-mode
;;   :bind
;;   ("C-c C-k" . #'periphery-run-loco))

(use-package periphery-swiftlint
  :ensure nil
  :after swift-ts-mode
  :bind
  ("C-c C-l" . #'periphery-run-swiftlint))

;; (use-package svg-tag-mode
;;   :defer 3
;;   :hook ((swift-ts-mode . svg-tag-mode)
;;          (localizeable-mode . svg-tag-mode)
;;          (kotlin-ts-mode . svg-tag-mode))
;;   :config
;;   (plist-put svg-lib-style-default :font-family "Jetbrains Mono")
;;   (plist-put svg-lib-style-default :font-size 15)
;;   :init
;;   (setq svg-tag-tags (periphery-svg-tags)))

(use-package indent-bars
  :defer t
  :vc (indent-bars
       :url "https://github.com/jdtsmith/indent-bars"
       :branch "main"
       :rev :newest)
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.15))
  (indent-bars-highlight-current-depth '(:blend 0.5)) ; pump up the BG blend on current
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("comment")) ; Ignore comments
  (indent-bars-width-frac 0.1)
  (indent-bars-prefer-character t))

(defun mk/browser-split-window (url &optional new-window)
  "Create a new browser (as URL as NEW-WINDOW) window to the right of the current one."
  (interactive)
  (let ((ignore-window-parameters t)
        (dedicated-p (window-dedicated-p)))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (xwidget-webkit-browse-url url)))

(defun mk/toggle-flycheck-errors ()
  "Function to toggle flycheck errors."
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
    (list-flycheck-errors)))

(defun toggle-vterm ()
  "Toggle vterm buffer."
  (interactive)
  (if (get-buffer "*vterm*")
      (if (eq (current-buffer) (get-buffer "*vterm*"))
          (delete-window)
        (switch-to-buffer-other-window "*vterm*"))
    (progn
      (vterm-other-window))))

(defun xref-eglot+dumb-backend ()
  "Return the xref backend for eglot+dumb."
  'eglot+dumb)

(advice-add 'eglot-xref-backend :override 'xref-eglot+dumb-backend)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot+dumb)))
  "Return the identifier at point for eglot+dumb."
  (cons (xref-backend-identifier-at-point 'eglot)
        (xref-backend-identifier-at-point 'dumb-jump)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot+dumb)))
  "Return the completion table for eglot+dumb."
  (xref-backend-identifier-completion-table 'eglot))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot+dumb)) identifier)
  "Return the definitions for eglot+dumb."
  (or (xref-backend-definitions 'eglot (car identifier))
      (xref-backend-definitions 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-references ((_backend (eql eglot+dumb)) identifier)
  (or (xref-backend-references 'eglot (car identifier))
      (xref-backend-references 'dumb-jump (cdr identifier))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot+dumb)) pattern)
  (xref-backend-apropos 'eglot pattern))

(put 'narrow-to-region 'disabled nil)

(use-package treesit
  :defer t
  :ensure nil
  :config
  (setq treesit-font-lock-level 4))

(use-package nxml-mode
  :ensure nil
  :mode "\\.xml\\'"
  :config
  :hook ((nxml-mode . setup-programming-mode)
         (nxml-mode . colorful-mode)
         (nxml-mode . display-line-numbers-mode)))

(use-package compile
  :defer t
  :ensure nil
  :hook (compilation-mode . visual-line-mode)  ; Enable visual-line-mode in compilation buffers
  :custom
  (compilation-always-kill t)
  (compilation-auto-jump-to-first-error t)
  (compilation-ask-about-save nil)
  (compilation-skip-threshold 1)
  (compilation-scroll-output 'all)
  (compilation-highlight-overlay t)
  (compilation-environment '("TERM=dumb" "TERM=xterm-256color"))
  (compilation-window-height 10)
  (compilation-reuse-window t)
  (compilation-max-output-line-length nil)
  (compilation-error-screen-columns nil)
  (ansi-color-for-compilation-mode t)
  :config
  ;; Add ANSI color filtering
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  ;; Auto-close compilation buffer on success after 1 second
  (add-hook 'compilation-finish-functions
            (lambda (buf str)
              (when (string-match "finished" str)
                (run-at-time 1 nil (lambda ()
                                     (delete-windows-on buf)
                                     (bury-buffer buf)))))))

(use-package flycheck-kotlin
  :hook ((kotlin-mode kotlin-ts-mode) . flycheck-kotlin-setup))

(use-package kotlin-ts-mode
  :defer 3
  :mode ("\\.kt\\'" "\\.kts\\'"))

(use-package kotlin-development
  :after kotlin-ts-mode
  :hook ((kotlin-mode kotlin-ts-mode) . kotlin-development-mode-setup)
  :ensure nil  ; if it's a local package
  :bind ((:map kotlin-mode-map
               ("C-c C-c" . kotlin-development-build-and-run)
               ("M-K" . kotlin-development-clean-build)
               ("C-c C-e l" . kotlin-development-list-emulators)
               ("C-c C-e k" . kotlin-development-kill-emulator))
         (:map kotlin-ts-mode-map
               ("C-c r s" . code-refactor:split-function-list)
               ("C-c C-c" . kotlin-development-build-and-run)
               ("C-c C-e l" . kotlin-development-list-emulators)
               ("C-c C-e k" . kotlin-development-kill-emulator)))
  :init
  (require 'swift-refactor)
  :config
  (setenv "JAVA_OPTS" "-Xmx8g")
  (setq kotlin-development-emulator-name "Medium_Phone_API_35"))

(use-package copilot
  :defer 3
  :vc (copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main" :rev :newest)
  :ensure nil
  :hook ((prog-mode localizeable-mode) . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("TAB" . copilot-accept-completion)
        ("C-c C-n" . copilot-next-completion)
        ("C-c C-p" . copilot-previous-completion))
  :config
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char 1000000))

(use-package copilot-chat
  :vc (copilot-chat :url "https://github.com/chep/copilot-chat.el.git" :branch "main" :rev :newest)
  :hook ((copilot-chat . visual-line-mode)
         (git-commit-setup . copilot-chat-insert-commit-message))
  :config
  (setq copilot-chat-frontend 'org)
  (setq copilot-chat-follow nil))

(use-package jinx
  :hook ((text-mode-hook . jinx-mode)
         (org-mode-hook . jinx-mode)
         (git-commit-mode-hook . jinx-mode))
  :bind (("C-x j c" . jinx-correct)      ; Traditional Emacs spell-check binding
         ("C-x j l" . jinx-languages))   ; Quick language switching
  :config
  (setq jinx-languages "en")
  (setq jinx-exclude-modes
        '(minibuffer-mode          ; Mini buffer
          dired-mode               ; Directory editor
          fundamental-mode))       ; Fundamental mode
  (custom-set-faces
   '(jinx-misspelled ((t (:underline (:style wave :color "red")))))))

(use-package eglot-booster
  :hook (eglot-managed-mode . eglot-booster-mode)
  :config
  (setq eglot-booster-io-only t))

(use-package music-control
  :ensure nil
  :hook (after-init . music-control-mode))

(use-package aider
  :defer t)

(use-package aidermacs
  :defer t
  :commands aidermacs-transient-menu
  :vc (aidermacs
       :url "https://github.com/MatthewZMD/aidermacs"
       :branch "main"
       :rev :newest)
  :config
  (setq aidermacs-auto-commits nil
        aidermacs-use-architect-mode nil
        aidermacs-show-diff-after-change t)
  ;; (setq aidermacs-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
  ;; (setq aidermacs-args '("--model" "deepseek/deepseek-reasoner"))
  (setq aidermacs-args '("--model" "deepseek/deepseek-chat"))
  :bind (:map global-map
              ("C-c a" . aidermacs-transient-menu)))

(use-package breadcrumb
  :defer t
  :hook
  (prog-mode . breadcrumb-local-mode)
  :custom
  (breadcrumb-imenu-crumb-separator "::")
  (breadcrumb-project-crumb-separator " ")
  (breadcrumb-imenu-max-length 0.5)
  (breadcrumb-project-max-length 0.8)
  :preface
  (advice-add #'breadcrumb--format-project-node :around
              (lambda (og p more &rest r)
                "Icon For File"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (concat (nerd-icons-icon-for-file string)
                              " " string)
                    (concat (nerd-icons-faicon
                             "nf-fa-folder_open"
                             :face 'breadcrumb-project-crumbs-face)
                            " "
                            string)))))

  (advice-add #'breadcrumb--project-crumbs-1 :filter-return
              (lambda (return)
                "Icon for Parent Node"
                (if (listp return)
                    (setf (car return)
                          (concat
                           " "
                           (nerd-icons-faicon
                            "nf-fa-rocket"
                            :face 'breadcrumb-project-base-face)
                           " "
                           (car return))))
                return))

  (advice-add #'breadcrumb--format-ipath-node :around
              (lambda (og p more &rest r)
                "Icon for items"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (concat (nerd-icons-codicon
                               "nf-cod-symbol_field"
                               :face 'breadcrumb-imenu-leaf-face)
                              " " string)
                    (cond ((string= string "Packages")
                           (concat (nerd-icons-codicon "nf-cod-package" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Requires")
                           (concat (nerd-icons-codicon "nf-cod-file_submodule" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((or (string= string "Variable") (string= string "Variables"))
                           (concat (nerd-icons-codicon "nf-cod-symbol_variable" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Function")
                           (concat (nerd-icons-mdicon "nf-md-function_variant" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          (t string)))))))

(use-package jira
  :defer t
  :commands (jira-issues jira-issues-menu)
  :vc (jira
       :url "git@github.com:unmonoqueteclea/jira.el"
       :branch "main"
       :rev :newest)
  :init
  (setq jira-username "mikael.konradsson@mobileinteraction.se"
        jira-base-url "https://mobileinteraction.atlassian.net" ;; Jira instance URL
        jira-token (getenv "JIRA_TOKEN")
        jira-statuses-done '("Klart" "Done" "Closed" "Resolved" "Waiting for QA")
        jira-statuses-todo '("Att göra" "Todo")
        jira-statuses-error'("Error" "Rejected" "In Progress - Error" "Under granskning")
        jira-statuses-progress '("Pågående" "QA staging" "In test" "In Progress" "In progress" "In Progress - Development" "In Progress - Design" "In Progress - Review" "In Progress - Testing")))


(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  ;; Add Swift and SwiftUI to markdown code languages
  (add-to-list 'markdown-code-lang-modes '("swift" . swift-mode))
  (add-to-list 'markdown-code-lang-modes '("swiftui" . swift-mode))
  ;; Enable Eldoc in Markdown buffers
  (add-hook 'markdown-mode-hook #'eldoc-mode))

(defun consult-list-all-project-files ()
  "Show all project files immediately with live preview."
  (interactive)
  (consult--read (project-files (project-current t))
                 :prompt "Project file: "
                 :category 'file
                 :state (consult--file-state)
                 :require-match t))

(provide 'init)i
;;; init.el ends here
