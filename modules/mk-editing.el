;;; mk-editing.el --- Editing configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains configurations related to editing in Emacs.
;;; Code:

(require 'cl-lib)
(require 'project)

;; Override custom-declare-variable temporarily to handle all missing variables
;; This is a comprehensive workaround for Emacs 30.2.50 compatibility issues
(cl-letf* ((original-func (symbol-function 'custom-declare-variable))
           ((symbol-function 'custom-declare-variable)
            (lambda (symbol value &rest args)
              (condition-case nil
                  (apply original-func symbol value args)
                (void-variable
                 (unless (boundp symbol)
                   (set symbol (if (functionp value)
                                   (ignore-errors (funcall value))
                                 value)))
                 symbol)
                (error
                 (unless (boundp symbol)
                   (set symbol nil))
                 symbol))))))

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
    (let ((search-text
           (if (region-active-p)
                   (buffer-substring-no-properties
              (region-beginning)
              (region-end))
                 (let ((bounds (bounds-of-thing-at-point 'symbol)))
             (when bounds
               (buffer-substring-no-properties (car bounds) (cdr bounds)))))))
      (when search-text
        (when isearch-mode
          (isearch-done))
        (deactivate-mark)
        (isearch-mode t nil nil nil)
        (setq isearch-case-fold-search nil)
        (isearch-yank-string search-text))))
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
  :ensure t
  :defer t
  :bind
  (:map iedit-mode-keymap
        ("C-<return>" . iedit-toggle-selection)
        ("C-j" . iedit-next-occurrence)
        ("C-k" . iedit-prev-occurrence)
        ("C-g" . iedit--quit)))

(use-package ultra-scroll
  :ensure t
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

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package rg
  :ensure t
  :defer t)

(use-package visual-replace
  :defer t
  :bind (("C-c r" . visual-replace)
         :map isearch-mode-map
         ("C-c r" . visual-replace-from-isearch)))

(use-package avy
  :defer t
  :bind
  ("M-g" . avy-goto-word-1)
  (:map isearch-mode-map
        ("C-a" . avy-isearch))
  :config
  (setq avy-single-candidate-jump t))

(provide 'mk-editing)
;;; mk-editing.el ends here
