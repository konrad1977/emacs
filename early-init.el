;;; early-init.el --- Early Init File -*- no-byte-compile: t; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.4)

;; Prevent unwanted runtime builds in gccemacs (native-comp); native-comp is available from Emacs 28+
(setq native-comp-deferred-compilation nil)

;; Prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

;; Optimize file-name-handler-alist
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Set frame parameters
(setq default-frame-alist
      '((ns-use-native-fullscreen . t)
        (ns-transparent-titlebar . t)
        (ns-appearance . dark)
        (fullscreen . maximized)
        (background-color . "#13131a")
        (foreground-color . "#a0a0ae")))

;; Optimize startup
(setq load-prefer-newer t
      custom-file null-device
      read-process-output-max (* 1 1024 1024)
      process-adaptive-read-buffering t
      inhibit-compacting-font-caches t
      default-input-method nil
      bidi-inhibit-bpa t
      frame-title-format nil
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Encoding and bidirectional text optimization
(set-language-environment "UTF-8")
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Restore file name handler and GC settings after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

(provide 'early-init)
;;; early-init.el ends here
