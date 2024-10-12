;;; early.-init.el --- my early-init file -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary: Optimized early init file
;;; Code:

;; Garbage collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq custom-file null-device)
;; File name handler
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Fundamental optimizations
(setq read-process-output-max (* 4 1024 1024)) ; Increase from 64KB to 1MB
(setq process-adaptive-read-buffering nil)
(setq-default inhibit-redisplay t
              inhibit-message t)
(setq inhibit-compacting-font-caches t)

;; Encoding
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; Bidirectional text
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Frame parameters
(setq default-frame-alist
      '((ns-use-native-fullscreen . t)
        (ns-transparent-titlebar . t)
        (ns-appearance . dark)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (fullscreen . maximized)
        (vertical-scroll-bars . nil)
        (background-color . "#13131a")
        (foreground-color . "#a0a0ae")))

;; Frame behavior
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Startup optimizations
(setq frame-title-format nil
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-message t
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Package management
(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(package-initialize)

;; Restore file name handler after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024) ; 16mb
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original
                  inhibit-redisplay nil
                  inhibit-message nil)))

(provide 'early-init)
;;; early-init.el ends here
