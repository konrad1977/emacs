;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;; Commentary:
;; Emacs early initialization file loaded before package system and UI.
;; Optimized for performance and minimal startup time.

;;; Code:

;; ------------------------------------------------------------
;; Performance Optimizations
;; ------------------------------------------------------------

;; Save original file handlers to restore later
(defvar file-name-handler-alist-original file-name-handler-alist)

;; Disable bidirectional text support for faster display
(setq-default bidi-display-reordering 'left-to-right
              bidi-inhibit-bpa t)

;; Disable unnecessary UI elements and features for faster startup
(setq-default inhibit-startup-screen t
              inhibit-startup-message t
              inhibit-startup-echo-area-message user-login-name
              inhibit-startup-buffer-menu t
              initial-scratch-message nil
              initial-buffer-choice nil
              initial-major-mode 'fundamental-mode)

;; Memory and GC settings for faster startup
(setq gc-cons-threshold (* 128 1024 1024)  ; 128MB during startup
      gc-cons-percentage 0.6
      inhibit-compacting-font-caches t
      message-log-max 10000
      kill-ring-max 100000
      load-prefer-newer t
      file-name-handler-alist nil
      vc-handled-backends '(Git))

;; Process and I/O optimizations
(setq read-process-output-max (* 64 1024)  ; 64k
      process-adaptive-read-buffering nil
      warning-suppress-types '((comp) (lexical-binding)))

;; Package system optimizations
(setq package-enable-at-startup nil
      package-native-compile t
      package-quickstart t)

;; ------------------------------------------------------------
;; Frame and UI Configuration
;; ------------------------------------------------------------

;; Frame appearance settings
(setq default-frame-alist
      '((background-color . "#13131a")
        (foreground-color . "#a0a0ae")
        (cursor-type . (box . 4))
        (fullscreen . maximized)
        (inhibit-double-buffering . t)
        (undecorated-round . t)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (scroll-bar-width . 0)
        (internal-border-width . 0)
        (left-fringe . 0)
        (right-fringe . 0)
        (bottom-divider-width . 0)
        (right-divider-width . 0)
        (child-frame-border-width . 0)))


;; ------------------------------------------------------------
;; Platform-Specific Configuration
;; ------------------------------------------------------------

(when (eq system-type 'darwin)
  ;; macOS-specific settings
  (push '(ns-use-native-fullscreen . nil) default-frame-alist)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist)
  (push '(ns-use-srgb-colorspace . t) default-frame-alist)
  (setq ns-use-proxy-icon nil
        mac-command-modifier 'meta
        mac-option-modifier 'none))

;; Font settings
(setq use-default-font-for-symbols nil)
(let (
      ;; (mono-font "JetBrainsMono Nerd Font Mono")
      (mono-font "Iosevka Fixed Curly")
      (variable-font "Iosevka Aile"))
  (set-face-attribute 'default nil :family mono-font :height 170 :weight 'ultra-light)
  (set-face-attribute 'fixed-pitch nil :family mono-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family variable-font :height 1.0))
(set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") nil 'append)

                                        ; Native compilation settings
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-async-query-on-exit t
        native-comp-jit-compilation t))

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Use setq for buffer-independent variables

;; Use setq-default for buffer-local variables
(setq-default mode-line-format nil
              lexical-binding t)

;; Only enable archives after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                                     ("gnu" . "https://elpa.gnu.org/packages/")
                                     ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
                  package-archive-priorities '(("gnu" . 99)
                                               ("nongnu" . 80)
                                               ("melpa" . 0)))))

;; Restore file-name-handler-alist after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (file-exists-p custom-file)
              (load custom-file 'noerror 'nomessage))
            (setq gc-cons-threshold (* 16 1024 1024)  ; Reset to 16 MB
                  gc-cons-percentage 0.2)  ; Reset percentage for GC
            (setq file-name-handler-alist file-name-handler-alist-original)
            (run-with-idle-timer 1.2 t 'garbage-collect)))

(provide 'early-init)
;;; early-init.el ends here
