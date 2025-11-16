;;; mk-vc.el --- Version Control Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for version control systems in Emacs using Magit, Git Gutter,

;;; Code:

;; Ensure transient is installed and loaded before magit
(unless (package-installed-p 'transient)
  (package-install 'transient))
(require 'transient)

(use-package magit
  :ensure t
  :after transient
  :commands (magit-status magit-ediff-show-working-tree)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :init
  (setq magit-save-repository-buffers nil
        magit-repository-directories nil)
  :config
  (require 'magit-section)
  (setq magit-format-file-function #'magit-format-file-nerd-icons
        magit-diff-refine-hunk 'all)
  ;; Setup evil-collection for magit after it's loaded
  (when (featurep 'evil-collection)
    (evil-collection-init 'magit)))

(use-package git-timemachine
  :ensure t
  :defer t
  :commands (git-timemachine git-timemachine-toggle))

;; (use-package git-gutter
;;   :defer 5
;;   :hook (prog-mode . git-gutter-mode)
;;   :diminish git-gutter-mode
;;   :config
;;   (setq git-gutter:modified-sign "┃"
;;         git-gutter:added-sign "┃"
;;         git-gutter:deleted-sign "┃")
;;   (setq git-gutter:window-width 1)  ;; Set to minimum width
;;   (setq git-gutter:update-interval 3))

(use-package consult-gh
  :after consult
  :defer t
  :config
  (consult-gh-enable-default-keybindings))

(use-package diff-hl
  :defer t
  :ensure t
  :after evil
  :hook
  (prog-mode . (lambda ()
                 (global-diff-hl-mode)           ;; Enable Diff-HL mode for all files.
                 (diff-hl-flydiff-mode)          ;; Automatically refresh diffs.
                 (diff-hl-margin-mode)))         ;; Show diff indicators in the margin.
  :custom
  (diff-hl-side 'left)                           ;; Set the side for diff indicators.
  (diff-hl-margin-symbols-alist '((insert . "┃") ;; Customize symbols for each change type.
                                  (delete . "-")
                                  (change . "┃")
                                  (unknown . "┆")
                                  (ignored . "i"))))


;;
;; (use-package blamer
;;   :commands blamer-mode
;;   :config
;;   (setq blamer-tooltip-function 'blamer-tooltip-author-info)
;;   (setq blamer-view 'overlay
;;         blamer-type 'overlay-popup
;;         blamer-max-commit-message-length 270
;;         blamer-force-truncate-long-line nil
;;         blamer-show-avatar-p t
;;         blamer-author-formatter " ✎ %s "
;;         blamer-commit-formatter "● \'%s\' ● ")
;;   :custom
;;   (blamer-idle-time 1.0)
;;   :custom-face
;;   (blamer-face ((t :foreground "#E46876"
;;                    :height 130
;;                    :bold t
;;                    :italic t))))
;;

;;; Provide
(provide 'mk-vc)
;;; mk-vc.el ends here.
