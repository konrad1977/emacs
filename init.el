;;; init.el --- optimized init file -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Personal Emacs configuration with optimized startup and modern packages.
;; Features include Evil mode, LSP support, project management, and development tools.

;;; Code:

;; Suppress all warnings when loading packages
(setq warning-minimum-level :error)
(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)

;; Load warning suppression
(load (expand-file-name "suppress-warnings" user-emacs-directory) nil t)

(dolist (path (list
               (expand-file-name "modules" user-emacs-directory)))
  (add-to-list 'load-path path))

(require 'mk-core)
(require 'mk-emacs)
(require 'mk-completion)
(require 'mk-code-completion)
(require 'mk-development)
(require 'mk-editing)
(require 'mk-evil)
(require 'mk-ios-development)
;; (require 'mk-kotlin-development)
(require 'mk-lisp)
(require 'mk-misc)
;; (require 'mk-org)
(require 'mk-term)
(require 'mk-theme)
(require 'mk-treemacs)
(require 'mk-ui)
(require 'mk-ai)
(require 'mk-vc)
;; (require 'mk-web)
;; (require 'mk-elfeed)

(use-package welcome-dashboard
  :ensure nil
  :config
  (setq welcome-dashboard-latitude 56.7365
        welcome-dashboard-longitude 16.2981
        welcome-dashboard-use-nerd-icons t
        welcome-dashboard-max-number-of-projects 8
        welcome-dashboard-show-weather-info t
        welcome-dashboard-use-fahrenheit nil
        welcome-dashboard-max-left-padding 1
        welcome-dashboard-max-number-of-todos 5
        welcome-dashboard-path-max-length 70
        welcome-dashboard-min-left-padding 10
        welcome-dashboard-image-file "~/.emacs.d/themes/emacs.png"
        welcome-dashboard-image-width 200
        welcome-dashboard-image-height 200
        welcome-dashboard-title "Welcome Mikael. Have a great day!")
  (welcome-dashboard-create-welcome-hook))

(provide 'init)
;;; init.el ends here
(put 'narrow-to-page 'disabled nil)
