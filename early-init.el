;;; early-init.el --- Early Init File -*- no-byte-compile: t; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.4)

;; ;; Prevent unwanted runtime builds in gccemacs (native-comp); native-comp is available from Emacs 28+
(setq native-comp-deferred-compilation nil)

;; ;; Prevent package.el loading packages prior to their init-file loading

;; ;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; ;; Optimize file-name-handler-alist
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; ;; Set frame parameters
(setq default-frame-alist
      '((ns-use-native-fullscreen . t)
        (ns-transparent-titlebar . t)
        (ns-appearance . dark)
        (fullscreen . maximized)
        (background-color . "#13131a")
        (foreground-color . "#a0a0ae")))

;; ;; Optimize startup
(setq custom-file null-device
      read-process-output-max (* 8 1024 1024)
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      idle-update-delay 1.0
      inhibit-compacting-font-caches t
      default-input-method nil
      frame-inhibit-implied-resize t
      bidi-inhibit-bpa t
      frame-title-format nil
      initial-major-mode 'fundamental-mode
      mode-line-format nil
      max-lisp-eval-depth 13000
      large-file-warning-threshold 100000000
      initial-scratch-message nil)

;; ;; Encoding and bidirectional text optimization
;; (set-language-environment "UTF-8")
;; (setq-default bidi-display-reordering 'left-to-right
;;               bidi-paragraph-direction 'left-to-right)

;; ;; Don't want a mode line while loading init.

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; No Alarms by default.
(setq ring-bell-function 'ignore)

;; Restore file name handler and GC settings after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.2
                  file-name-handler-alist file-name-handler-alist-original)))

(provide 'early-init)
;;; early-init.el ends here
