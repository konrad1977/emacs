;;; early-init.el --- -*- lexical-binding: t -*-

;;; Commentary: My early init file

;;; Code:

(setq gc-cons-threshold-original gc-cons-threshold
      gc-cons-threshold (* 512 1024 1024))

(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 96 1024 1024)))

(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)

(setq ffap-alist nil)                ; faster, dumber prompting
(setq ffap-url-regexp nil)           ; disable URL features in ffap
(setq ffap-shell-prompt-regexp nil)  ; disable shell prompt stripping
(setq ffap-gopher-regexp nil)        ; disable gopher bookmark matching

;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      frame-title-format nil
      indicate-buffer-boundaries '((bottom . right))
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-message t
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      load-prefer-newer noninteractive
      menu-bar-mode nil
      package-enable-at-startup nil
      package-native-compile t
      scroll-bar-mode nil
      site-run-file nil
      tool-bar-mode nil
      default-frame-alist
      '((background-color . "#161616")       ; Default background color
        (inhibit-double-buffering . t)
        (bottom-divider-width . 0)           ; Thin horizontal window divider
        (fullscreen . maximized)             ; Maximize the window by default
        (right-divider-width . 1)            ; Thin vertical window divider
        (ns-use-native-fullscreen . t)
        (ns-transparent-titlebar . t)
        (vertical-scroll-bars . nil)))       ; No vertical scroll-bars

(define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))

(provide 'early-init)
;;; early-init.el ends here
