;;; mk-term.el --- Create and manage terminal buffers -*- lexical-binding: t; -*-
;;; Commentary:
;; This file provides functionality to create and manage terminal buffers in Emacs.
;;; Code:

(add-hook 'comint-mode-hook
          (lambda ()
            (setq-local comint-prompt-read-only t)
            (setq-local visual-line-mode t)))

(use-package vterm
  :defer t
  :commands vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (setq vterm-timer-delay nil
        vterm-kill-buffer-on-exit t))

(defun toggle-vterm ()
  "Toggle vterm buffer."
  (interactive)
  (if (get-buffer "*vterm*")
      (if (eq (current-buffer) (get-buffer "*vterm*"))
          (delete-window)
        (switch-to-buffer-other-window "*vterm*"))
    (progn
      (vterm-other-window))))

(provide 'mk-term)
;;; mk-term.el ends here
