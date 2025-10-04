;;; mk-treemacs.el --- Treemacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Treemacs configuration with Nerd Icons and integration with Magit.
;;; Code:

(use-package treemacs
  :defer t
  :commands (treemacs treemacs-select-window)
  :hook ((treemacs-mode . treemacs-project-follow-mode))
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
        treemacs-hide-dot-git-directory t
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
        treemacs-show-hidden-files nil
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

(provide 'mk-treemacs)
;;; mk-treemacs.el ends here
