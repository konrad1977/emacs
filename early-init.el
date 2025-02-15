;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar file-name-handler-alist-original file-name-handler-alist)

;; Faster startup by reducing garbage collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent package.el loading packages prior to init.el loading
(setq package-enable-at-startup t)

;; Disable unnecessary UI early
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(push '(background-color . "#13131a") default-frame-alist)
(push '(foreground-color . "#a0a0ae") default-frame-alist)

;; MacOS specific settings
(when (eq system-type 'darwin)
  (push '(ns-use-native-fullscreen . nil) default-frame-alist)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

(setq site-run-file nil
      read-process-output-max (* 2 1024 1024) ;; 2mb
      inhibit-compacting-font-caches t
      frame-inhibit-implied-resize t
      vc-handled-backends nil
      file-name-handler-alist nil
      kill-ring-max 100000
      mode-line-format nil
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-screen t
      initial-scratch-message nil
      load-prefer-newer t
      ns-use-proxy-icon nil
      frame-title-format nil
      frame-resize-pixelwise t)

;; Font settings
(let ((mono-font "Iosevka Curly")
      (variable-font "Iosevka Aile"))
  (set-face-attribute 'default nil :family mono-font :height 180 :weight 'light)
  (set-face-attribute 'fixed-pitch nil :family mono-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family variable-font :height 1.0))

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.3
                  file-name-handler-alist file-name-handler-alist-original)))

(provide 'early-init)
;;; early-init.el ends here
