;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar file-name-handler-alist-original file-name-handler-alist)

;; Disable unnecessary UI early
(push '(vertical-scroll-bars) default-frame-alist)
(push '(ns-use-native-fullscreen . t) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(push '(background-color . "#13131a") default-frame-alist)
(push '(foreground-color . "#a0a0ae") default-frame-alist)
;; (setq default-frame-alist '((undecorated-round . t)))

(setq site-run-file nil
      read-process-output-max (* 8 1024 1024)
      inhibit-compacting-font-caches t
      package-enable-at-startup t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      vc-handled-backends nil
      file-name-handler-alist nil
      kill-ring-max 100000
      mode-line-format nil
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      load-prefer-newer noninteractive
      frame-resize-pixelwise nil
      frame-inhibit-implied-resize t
      frame-title-format nil)

(set-face-attribute 'default nil :family "Iosevka" :height 170 :weight 'light :width 'wide)
(set-face-attribute 'variable-pitch nil :family "SF Pro Text")

;; ;; Native compilation settings
(when (featurep 'native-compile)
        (setq native-comp-jit-compilation t
              native-comp-async-report-warnings-errors nil))

(set-language-environment    "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(set-selection-coding-system 'utf-8)

;; (add-to-list 'default-frame-alist '(internal-border-width . 1))

(trace-function 'url-cache-extract)
;; Reset file-name-handler-alist after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1000 1000)
                  gc-cons-percentage 0.5
                  file-name-handler-alist file-name-handler-alist-original)))

(provide 'early-init)
;;; early-init.el ends here
