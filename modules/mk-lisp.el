;;; mk-lispediting.el --- Emacs Lisp editing enhancements -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains configurations and enhancements for editing Emacs Lisp code.

;;; Code:

(use-package page-break-lines
  :defer t
  :commands (page-break-lines-mode
             global-page-break-lines-mode)
  :hook
  (emacs-lisp-mode . page-break-lines-mode))

(use-package aggressive-indent
  :defer t
  :commands aggressive-indent-mode
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))
;;
;; ;; Highlights function and variable definitions in Emacs Lisp mode
(use-package highlight-defined
  :ensure t
  :defer t
  :commands highlight-defined-mode
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package paredit
  :defer t
  :commands paredit-mode
  :hook
  (emacs-lisp-mode . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil))

(use-package semel
  :vc (semel
       :url "https://github.com/eshelyaron/semel" :rev :newest)
  :hook (emacs-lisp-mode . semel-mode))

(use-package colorful-mode
  :defer t
  :ensure t
  :hook (emacs-lisp-mode . colorful-mode)
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-alignment 'left)
  (colorful-prefix-string "●"))

(use-package highlight-quoted
  :hook
  (emacs-lisp-mode-hook . highlight-quoted-mode))

(use-package eros
  :custom-face
  (eros-result-overlay-face ((t (:inherit shadow :box t))))
  :hook
  (emacs-lisp-mode-hook . eros-mode))

;;; Provide
(provide 'mk-lisp)
;;; mk-lispediting.el ends here.
