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
  :after (transient evil-collection)
  :commands (magit magit-status magit-ediff-show-working-tree)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :init
  (setopt forge-add-default-sections t
          forge-add-default-bindings t)
  :config
  (require 'magit-section)
  (setq magit-repository-directories '(("~/.emacs.d/" . 2)
                                       ("~/Documents/git" . 1))
        magit-format-file-function #'magit-format-file-nerd-icons
        magit-diff-refine-hunk 'all)
  (evil-collection-init 'magit))

(use-package git-timemachine
  :ensure t
  :defer t
  :commands (git-timemachine git-timemachine-toggle))

(defun my/enable-diff-hl-if-git ()
  "Enable diff-hl-mode if the current buffer is part of a Git repository."
  (when-let* ((file buffer-file-name)
              (backend (vc-backend file)))
    (when (eq backend 'Git)
      (diff-hl-mode 1)
      (when (display-graphic-p)
        (diff-hl-margin-mode 1)))))

(defun my/diff-hl-reset-reference-revision ()
  "Reset diff-hl reference revision when switching buffers between repos."
  (when (and buffer-file-name
             (bound-and-true-p diff-hl-mode))
    (setq-local diff-hl-reference-revision nil)
    (vc-file-clearprops buffer-file-name)))

(use-package diff-hl
  :defer t
  :ensure t
  :hook ((prog-mode . my/enable-diff-hl-if-git)
         (buffer-list-update . my/diff-hl-reset-reference-revision))
  :config
  (setopt diff-hl-disable-on-remote t)
  :custom
  (diff-hl-side 'left)
  (diff-hl-margin-symbols-alist '((insert . "┃")
                                  (delete . "┃")
                                  (change . "┃")
                                  (unknown . "?")
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
