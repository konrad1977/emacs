;;; mk-development.el --- Programming Mode Setup -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains configurations related to programming modes in Emacs.
;;; Code:

;; Dependencies loaded on demand
(declare-function flycheck-mode "flycheck")
(declare-function evil-define-key "evil")
(declare-function breadcrumb-local-mode "breadcrumb")

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
  ;; (infer-indentation-style)
  
  (local-set-key (kbd "M-+") #'mk/toggle-flycheck-errors)
  (setq indicate-unused-lines nil
        word-wrap nil
        show-trailing-whitespace nil
        truncate-lines t))

(use-package ligature
  :ensure t
  :defer t
  :config
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


(use-package indent-bars
  :defer t
  :vc (indent-bars
       :url "https://github.com/jdtsmith/indent-bars"
       :branch "main"
       :rev :newest)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.15))
  (indent-bars-highlight-current-depth '(:blend 0.5)) ; pump up the BG blend on current
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("comment")) ; Ignore comments
  (indent-bars-width-frac 0.1)
  (indent-bars-prefer-character t))


(use-package breadcrumb
  :defer t
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

(use-package rainbow-delimiters
  :ensure t)

(use-package highlight-symbol
  :defer t
  :config
  (setopt highlight-symbol-idle-delay 0.8
          highlight-symbol-highlight-single-occurrence nil))

(use-package flycheck
  :ensure t
  :defer t
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


;; Drag lines and regions around
(use-package drag-stuff
  :ensure t
  :defer t
  :bind (:map evil-visual-state-map
	      ("C-j" . drag-stuff-down)
	      ("C-k" . drag-stuff-up)))

(use-package dumb-jump
  :ensure t
  :defer t
  :config
  (put 'dumb-jump-go 'byte-obsolete-info nil)
  (setq dumb-jump-window 'current
        dumb-jump-quiet t
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package hl-todo
  :ensure t
  :defer t
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO"   . "#1E90FF")
          ("FIXME"  . "#FF4500")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF8C00")
          ("STUB"   . "#1E90FF")
          ("MARK"   . "#777777")
          ("NOTE"   . "#00CED1")
          ("HACK"   . "#FF0000")
          ("REVIEW" . "#ADFF2F"))))

(use-package expand-region
  :defer t
  :bind ("C-x e" . er/expand-region))

(defun mk/toggle-flycheck-errors ()
  "Function to toggle flycheck errors."
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
    (list-flycheck-errors)))
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

(defun project-root-override (dir)
  "Find DIR's project root by searching for a '.project.el' file."
  (let ((root (or (locate-dominating-file dir ".xcodeproj")
                  (locate-dominating-file dir ".envrc")
                  (locate-dominating-file dir ".projectile")))
        (backend (ignore-errors (vc-responsible-backend dir))))
    (when root (if (version<= emacs-version "28")
                   (cons 'vc root)
                 (list 'vc backend root)))))

(use-package project
  :bind (
         ("M-O" . project-find-file)
         ("M-f" . project-find-regexp))
  :defer t
  :ensure nil
  :config
  (setopt project-vc-ignores '(".git/" ".direnv/" "node_modules/" "dist/" ".*"))
  (add-hook 'project-find-functions #'project-root-override))

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
  (add-hook 'compilation-filter-hook #'mk/compilation-auto-scroll)
  (add-hook 'compilation-finish-functions
            (lambda (buf str)
              (cond
               ((string-match "finished" str)
                (message "✅ Compilation finished successfully")
                (run-at-time 1 nil (lambda ()
                                     (delete-windows-on buf)
                                     (bury-buffer buf))))
               ((string-match "exited abnormally" str)
                (message "❌ Compilation failed - use M-x mk/compilation-get-errors to see details")
                ;; Keep compilation buffer visible on errors
                )))))

(defun mk/compilation-auto-scroll ()
  "Ensure compilation buffer scrolls to bottom during compilation.
Only scrolls if compilation window is visible."
  (when-let* ((buffer (get-buffer "*compilation*"))
              (window (get-buffer-window buffer t)))
    (with-selected-window window
      (goto-char (point-max)))))

(defun mk/compilation-get-errors ()
  "Extract and display compilation errors from current compilation buffer."
  (interactive)
  (if (not compilation-locs)
      (message "No compilation errors found")
    (let ((errors '()))
      (maphash (lambda (_ value)
                 (when (and (car value) (compilation--loc->file-struct (car value)))
                   (let* ((loc (car value))
                          (file-struct (compilation--loc->file-struct loc))
                          (file (caar file-struct))
                          (line (compilation--loc->line loc))
                          (col (compilation--loc->col loc)))
                     (push (format "%s:%d:%d" file line (or col 0)) errors))))
               compilation-locs)
      (if errors
          (with-output-to-temp-buffer "*Compilation Errors*"
            (princ (format "Found %d error(s):\n\n" (length errors)))
            (dolist (error (sort errors 'string<))
              (princ (format "• %s\n" error))))
        (message "No compilation errors found")))))


(use-package dape
  :commands (dape-info dape-repl dape)
  :hook
  ((dape-compile . kill-buffer) ; Kill compile buffer on build success
   (dape-display-source . pulse-momentary-highlight-one-line) ; Pulse source line (performance hit)
   (dape-stopped . dape-info) ; To display info and/or repl buffers on stopped
   (dape-stopped . dape-repl))
  :custom
  (dape-breakpoint-margin-string (propertize "●" :face 'dape-breakpoint-face))
  (dape-buffer-window-arrangement 'right) ;; Info buffers to the left
  (dape-inlay-hints t)
  :config
  (set-face-attribute 'dape-breakpoint-face nil :stipple nil)
  ;; (dape-breakpoint-global-mode)
  ;; (put 'dape--overlay-arrow-position
  ;;      'overlay-arrow-string   ;; ▶ →
  ;;      (propertize "▶" 'face 'dape-stack-trace-face))
  )

(use-package repeat
  :defer t
  :ensure nil
  :config (add-hook 'dape-on-start-hooks #'repeat-mode)
  :custom
  (repeat-too-dangerous '(kill-this-buffer))
  (repeat-exit-timeout 5))

(use-package flycheck-package
  :defer t
  :after flycheck
  :hook (emacs-lisp-mode . flycheck-package-setup))

(use-package flyover
  :ensure nil
  :hook (flycheck-mode . flyover-mode)
  ;; :bind ("C-c f l" . flyover-toggle)
  :config (add-hook 'flycheck-mode-hook #'flyover-mode)
  (setq flyover-virtual-line-type 'curved-arrow
        flyover-percent-darker 40
        flyover-text-tint-percent 50
        flyover-text-tint 'lighter
        flyover-show-at-eol nil
        flyover-wrap-messages t
        flyover-max-lines 110
        flyover-debounce-interval 2.0
        flyover-background-lightness 35
        flyover-swap-foreground-background t
        flyover-virtual-line-icon nil))

(use-package flycheck-eglot
  :ensure nil
  :hook (eglot-managed-mode . flycheck-eglot-mode)
  :custom (flycheck-eglot-exclusive nil))

(defun my/multi-occur-in-project-buffers (&optional nlines)
  "Show all lines in current project buffers matching a regexp.
A prefix argument will not filter project buffers."
  (interactive "P")
  (when-let* ((project (project-current)))
    (multi-occur (my/project-buffers-filtered project)
                 (car (occur-read-primary-args))
                 nlines)))

(defvar my:project-buffer-regexp-filter
  '("\\` "
    "\\`*")
  "Filter regexps for `my/project-buffers-filtered'.")

(defun my/project-buffers-filtered (project &optional all-buffers)
  "Use a prefix argument to include all PROJECT buffers."
  (setq all-buffers (or all-buffers
                        current-prefix-arg))
  (if all-buffers
      (project-buffers project)
    (seq-remove (lambda (buffer)
                  (cl-loop for regexp in my:project-buffer-regexp-filter
                           thereis (string-match-p regexp (buffer-name buffer))))
                (project-buffers project))))

(use-package periphery-quick
  :ensure nil
  :after (periphery-helper async)
  :bind (("C-c p f" . periphery-quick:find)
         ("C-c p F" . periphery-quick:find-ask)
         ("C-c p t" . periphery-quick:todos)
         ("C-c p l" . periphery-quick:find-in-file)))

(use-package periphery-search
  :ensure nil
  :after (periphery-helper async)
  :bind (("C-x t t" . periphery-query-todos-and-fixmes)))

(use-package prog-mode
  :ensure nil
  :hook ((emacs-lisp-mode . electric-indent-mode)
         (prog-mode . electric-pair-mode)
         (prog-mode . highlight-symbol-mode)
         (prog-mode . drag-stuff-mode)
         (prog-mode . hl-todo-mode)
         ;; (prog-mode . dumb-jump-mode)
         (prog-mode . hs-minor-mode)
         (prog-mode . rainbow-delimiters-mode)
         (prog-mode . flycheck-mode)
         (prog-mode . breadcrumb-mode)
         (prog-mode . setup-programming-mode)
         (prog-mode . display-line-numbers-mode)
         (prog-mode . indent-bars-mode)
         (prog-mode . global-ligature-mode)
         (prog-mode . global-prettify-symbols-mode))
  :config
  (setopt display-line-numbers-widen t
          display-line-numbers-type 'relative
          display-line-numbers-width 4))


(use-package treesit
  :ensure nil
  :defer t
  :config
  (setopt treesit-font-lock-level 4))

;;; Provide
(provide 'mk-development)
;;; mk-development.el ends here.
