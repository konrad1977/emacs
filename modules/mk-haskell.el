;;; mk-haskell.el --- Haskell mode configuration  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Configuration for Haskell programming in Emacs using haskell-ts-mode.

;;; Code:

(declare-function cape-capf-super "cape")
(declare-function eglot-completion-at-point "eglot")

(defun mk/setup-haskell-capf ()
  "Set up completion-at-point for Haskell with eglot as primary source."
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-dabbrev
                     #'cape-file))))


(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

;; Remappa haskell-mode -> haskell-ts-mode
(add-to-list 'major-mode-remap-alist
             '(haskell-mode . haskell-ts-mode))

;; Tree-sitter mode
(use-package haskell-ts-mode
  :ensure t
  :custom
  (haskell-ts-font-lock-level 4)
  (haskell-ts-use-indent t)
  (haskell-ts-ghci "ghci")

  :bind (:map haskell-ts-mode-map
              ("C-c C-l" . haskell-ts-run-haskell)
              ("C-c C-z" . haskell-ts-switch-to-repl)
              ("C-c C-t" . haskell-ts-type-at-point))

  :hook
  ;; Återanvänd funktioner från haskell-mode
  (haskell-ts-mode . interactive-haskell-mode)
  (haskell-ts-mode . eglot-ensure)
  (haskell-ts-mode . mk/setup-haskell-capf)
  (haskell-ts-mode . (lambda ()
                       (setq-local tab-width 2)
                       (setq-local indent-tabs-mode nil)))

  :config
  ;; Tree-sitter grammar
  (add-to-list 'treesit-language-source-alist
               '(haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"
                            "v0.23.1")))
  (unless (treesit-grammar-location 'haskell)
    (treesit-install-language-grammar 'haskell)))

(provide 'mk-haskell)
;;; mk-haskell.el ends here
