;;; mk-code-completion.el --- Code completion configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains configurations related to code completion in Emacs.
;;; Code:

;; Dependencies loaded on demand
(declare-function eglot-ensure "eglot")
(declare-function eldoc-box-hover-mode "eldoc-box")

(defun mk/setup-elisp-capf ()
  "Set `completion-at-point-functions´ for Elisp."
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'cape-elisp-symbol
                     #'cape-elisp-block
                     #'cape-dabbrev
                     #'cape-file
                     #'cape-keyword
                     #'elisp-completion-at-point))))
(use-package cape
  :ensure t
  :after evil
  :init
  ;; Add more Elisp specific backends
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
  (flymake-fringe-indicator-position nil)
  :config
  (add-to-list 'eglot-server-programs '(swift-ts-mode . swift-lsp-eglot-server-contact))
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

(use-package eldoc-box
  :after eldoc
  :bind
  ("C-c K" . eldoc-box-help-at-point)
  :config
  (setq eldoc-box-border-width 1
        eldoc-box-border-color "systemGrayColor"
        eldoc-box-max-pixel-height 400
        eldoc-box-max-pixel-width 600
        eldoc-box-clear-with-C-g t
        eldoc-box-use-multiline-p t
        eldoc-box-scrollbar-width 2
        eldoc-box-scrollbar-color "systemGrayColor"
        eldoc-box-scrollbar-background-color "systemGrayColor"
        eldoc-box-scrollbar-border-color "systemGrayColor"))

(use-package corfu
  :ensure t
  :hook (prog-mode . global-corfu-mode)
  :bind
  (:map corfu-map
        ("TAB" . corfu-insert)
        ("<tab>" . corfu-insert)
        ("SPC" . corfu-insert-separator)
        ("<escape>" . corfu-quit)
        ("C-g" . corfu-quit)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("RET" . corfu-insert)
        ("C-d" . corfu-show-documentation)
        ("C-e" . corfu-show-location)
        ("C-h" . corfu-show-location)
        ("M-RET" . corfu-complete)
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
        corfu-max-width 140
        corfu-preview-current 'insert
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match t)
  :init
  (corfu-history-mode))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package nerd-icons-corfu
  :ensure t
  :defer t
  :after (:all corfu))

(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

(use-package savehist
  :ensure t
  :defer 5
  :hook (after-init . savehist-mode)
  :config
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))


;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-extra-space t)
;;   (kind-icon-use-icons t)
;;   (kind-icon-blend-background t)
;;   (kind-icon-default-face 'corfu-default) ; only needed with blend-background
;;   (kind-icon-blend-frac 0.31)        ; only needed with blend-background)
;;   (kind-icon-default-style
;;    '(:padding -0.1 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
;;   ;; (kind-icon-formatted 'variable)
;;   :config
;;   (setq kind-icon-mapping
;;         '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
;;           (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
;;           (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
;;           (command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
;;           (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
;;           (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
;;           (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
;;           (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
;;           (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
;;           (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
;;           (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
;;           (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
;;           (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
;;           (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
;;           (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
;;           (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
;;           (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
;;           (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
;;           (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
;;           (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
;;           (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
;;           (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
;;           (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
;;           (param          "pa"  :icon "gear"               :face default                          :collection "vscode")
;;           (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
;;           (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
;;           (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
;;           (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
;;           (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
;;           (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
;;           (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
;;           (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
;;           (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
;;           (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
;;           (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
;;           (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode")))
;;   (defun my-kind-icon--extra-space (orig-fun &rest args)
;;     "Custom advice for `kind-icon--extra-space`."
;;     (let ((_ (apply orig-fun args)))
;;       (propertize " " 'display '(space :width 1.0)))) ; Adjusted width to 1.0
;;   (advice-add 'kind-icon--extra-space :around #'my-kind-icon--extra-space)
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :ensure t
  :config
  ;; (orderless-define-completion-style orderless+initialism
  ;;   (orderless-matching-styles '(orderless-initialism
  ;;                                orderless-literal
  ;;                                orderless-regexp)))

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; orderless-matching-styles '(orderless-literal orderless-regexp)
        completion-category-overrides
        '((file (styles partial-completion)))))

(defun my/file-completion-ignore-case ()
  "Make file completion case-insensitive in minibuffers and Consult/Project commands."
  (when (eq (completion-metadata-get
             (completion-metadata "" minibuffer-completion-table minibuffer-completion-predicate)
             'category)
            'file)
    (setq-local completion-ignore-case t)))

(add-hook 'minibuffer-setup-hook #'my/file-completion-ignore-case)

(provide 'mk-code-completion)
;;; mk-code-completion.el ends here
