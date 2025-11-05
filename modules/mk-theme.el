

;;; mk-theme.el --- My custom Emacs theme configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains configurations related to Emacs themes.
;;; Code:

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(use-package autothemer
  :ensure t
  :init
  ;; (load-theme 'catppuccin-mocha t)
  ;; (load-theme 'catppuccin-macchiato t)
  ;; (load-theme 'catppuccin-frappe t)
  ;; (load-theme 'catppuccin-latte t)
  ;; (load-theme 'ef-deuteranopia-dark t)
  ;; (load-theme 'rose-pine t)
  ;; (load-theme 'oxographite t)
  ;; (load-theme 'kman t)
  ;; (load-theme 'kalmar-night t)
  (load-theme 'kanagawa t)
  ;; (load-theme 'neofusion t)
  ;; (load-theme 'doom-gruvbox t)
  ;; (load-theme 'oxocarbon t)
  ;; (load-theme 'nordic t)
  ;; (load-theme 'poimandres t)
  ;; (load-theme 'mito-laser t)
  ;; (load-theme 'doom-outrun-electric t)
  ;; (load-theme 'doom-laserwave t)
  )


(provide 'mk-theme)
;;; mk-theme.el ends here
