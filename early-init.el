;;; early-init.el --- -*- lexical-binding: t -*-

;;; Commentary: My early init file

;;; Code:

(setq gc-cons-threshold-original gc-cons-threshold
      gc-cons-threshold (* 1024 1024 1024))

;; (setq gc-cons-threshold most-positive-fixnum)

(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation nil)

(setq kill-ring-max 100000)
(custom-set-variables '(savehist-additional-variables '(kill-ring)))

(setq ffap-alist nil)                ; faster, dumber prompting
(setq ffap-url-regexp nil)           ; disable URL features in ffap
(setq ffap-shell-prompt-regexp nil)  ; disable shell prompt stripping
(setq ffap-gopher-regexp nil)        ; disable gopher bookmark matching
(setq ffip-use-rust-fd t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(push '(ns-use-native-fullscreen . t) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(background-color . "#1e1e2e") default-frame-alist)

 (setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      frame-title-format nil
      truncate-lines nil
      truncate-partial-width-windows t
      truncate-string-ellipsis          ".."
      package-enable-at-startup nil
      indicate-buffer-boundaries '((bottom . right))
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-message t
      inhibit-startup-screen t
      ;; inhibit-compacting-font-caches t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      load-prefer-newer noninteractive
      site-run-file nil)

(provide 'early-init)
;;; early-init.el ends here
