;;; mk-completion.el --- Completion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion configuration using Vertico, Marginalia, and Nerd Icons.
;;; Code:

(use-package vertico
  :ensure t
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
              (lambda (orig cand prefix suffix index _)
                (setq cand (funcall orig cand prefix suffix index _))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face '(:inherit font-lock-operator-face :weight black))
                   "  ")
                 cand)))
  (setq vertico-resize t
        vertico-count 13
        vertico-cycle t))

(use-package vertico-posframe
  :ensure t
  :after vertico
  :config
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center
        vertico-posframe-min-height 2
        vertico-posframe-truncate-lines t
        vertico-posframe-min-width 120
        vertico-posframe-border-width 20
        vertico-posframe-parameters '((alpha . 0.9)))
  (setq vertico-multiform-commands
        '((consult-line (:not posframe))
          (xref-find-references (:not posframe))
          (consult-eglot-symbols (:not posframe))
          (consult-ripgrep (:not posframe))
          (consult-org-heading (:not posframe))
          (consult-xref (:not posframe))
          (t posframe)))
  (vertico-multiform-mode)
  (vertico-posframe-cleanup))

(custom-set-faces
 '(vertico-posframe-border ((t (:inherit vertico-posframe)))))

(use-package marginalia
  :ensure t
  :after (vertico vertico-posframe)
  :config
  (marginalia-mode))

(use-package vertico-directory
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :commands (find-file)
  :ensure nil
  :bind (:map vertico-map
              ("<tab>" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package nerd-icons-completion
  :ensure t
  :after (nerd-icons marginalia)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package consult
  :defer t
  :ensure t
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
  (setq register-preview-delay 0.4
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

;; (with-eval-after-load 'vertico
;;   (define-key evil-insert-state-map (kbd "<tab>") #'vertico-directory-enter)
;;   (define-key evil-insert-state-map (kbd "DEL") #'vertico-directory-delete-char))

(provide 'mk-completion)
;;; mk-completion.el ends here
