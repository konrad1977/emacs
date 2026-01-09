;;; early-init.el --- summary -*- lexical-binding: t; no-byte-compile: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;;; Commentary:
;; Early initialization file to optimize Emacs startup performance.

;;; Code:

(eval-when-compile
  (defvar display-time-24hr-format t)
  (defvar display-time-default-load-average nil))

(require 'package)
(setopt package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/"))
        package-archive-priorities '(("gnu" . 99)
                                     ("nongnu" . 80)
                                     ("melpa" . 70)
                                     ("melpa-stable" . 50)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (package-installed-p 'compat)
  (require 'compat nil t))

(require 'use-package-ensure) ;; Load use-package-always-ensure

(setopt package-enable-at-startup t
        package-check-signature nil
        package-quickstart t
        package-install-upgrade-built-in t
        package-quickstart-file
        (expand-file-name "package-quickstart.el" user-emacs-directory))

(use-package use-package
  :ensure t
  :custom
  (use-package-always-defer nil)
  (use-package-verbose nil)
  (use-package-minimum-reported-time 0.1)
  (use-package-expand-minimally t)
  (use-package-compute-statistics t)
  (use-package-always-ensure t)
  (use-package-enable-imenu-support t))

(dolist (path (list
               (expand-file-name "localpackages/kanagawa-emacs" user-emacs-directory)
               (expand-file-name "localpackages/mito-laser-emacs" user-emacs-directory)
               (expand-file-name "localpackages/neofusion-emacs" user-emacs-directory)))
  (add-to-list 'custom-theme-load-path path))


;; Add local packages directory to load-path
(let ((dir (expand-file-name "localpackages" user-emacs-directory)))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (let ((default-directory dir))
      (normal-top-level-add-subdirs-to-load-path))))

(let ((paths '("/opt/homebrew/bin"
               "/opt/homebrew/sbin"
               "/usr/local/bin"
               "/System/Cryptexes/App/usr/bin"
               "/usr/bin"
               "/bin"
               "/usr/sbin"
               "/Library/TeX/texbin"
               "/sbin"
               "~/.local/bin"
               "~/.nvm/versions/node/v22.11.0/bin"
               "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
               "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
               "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin")))
  (setenv "PATH" (string-join paths ":"))
  (setq exec-path paths))

(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key [C-wheel-up])
(global-unset-key [C-wheel-down])
(global-set-key (kbd "M-w") 'ns-do-hide-emacs)

;; Font settings with error handling
(condition-case nil
    (let ((mono-font "Iosevka Fixed Curly")
          (variable-font "Iosevka Aile"))
      (set-face-attribute 'default nil :family mono-font :width 'condensed :weight 'extra-light :height 180)
      (set-face-attribute 'fixed-pitch nil :family mono-font)
      (set-face-attribute 'variable-pitch nil :family variable-font :height 1.0))
  (error nil))
;; Emoji font fallback
(set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") nil 'append)

(use-package auto-compile
  :ensure t
  :defer 1
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter nil)
  (auto-compile-use-mode-line nil)
  (auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package nerd-icons
  :demand t)

(use-package posframe
  :demand t)

(provide 'mk-core)
;;; mk-core.el ends here
