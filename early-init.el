;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar file-name-handler-alist-original file-name-handler-alist)

;; Faster startup by reducing garbage collection and UI overhead
(setq gc-cons-threshold (* 384 1024 1024)  ; Raise to 384 MB
      file-name-handler-alist nil
      read-process-output-max (* 2 1024 1024)  ; Double buffer size
      process-adaptive-read-buffering t
      inhibit-compacting-font-caches t  ; Prevent GC during font ops
      bidi-display-reordering 'left-to-right  ; Sipler text layout
      package-archives nil
      package-enable-at-startup nil
      package-quickstart t
      byte-compile-warnings '(not obsolete))

;; Configure frame parameters in one go
(setq default-frame-alist
      `(
        (background-color . "#13131a")
        (bottom-divider-width . 0)
        (cursor-type . 'bar)
        (foreground-color . "#a0a0ae")
        (fullscreen . maximized)
        (inhibit-double-buffering . t)  ; Better frame rendering
        ;; (internal-border-width . 32)
        (left-fringe . 0)
        (menu-bar-lines . 0)
        (right-divider-width . 0)
        (right-fringe . 0)
        (tool-bar-lines . 0)
        (undecorated-round . t)
        (vertical-scroll-bars . nil)
        ;; (font-backend . "ns")  ; Force Core Text renderer on macOS
        ))

;; macOS-specific performance tweaks
(when (eq system-type 'darwin)
  (push '(ns-use-native-fullscreen . nil) default-frame-alist)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

;; Font settings
(let ((mono-font "Iosevka Curly")
      (variable-font "Iosevka Aile"))
  (set-face-attribute 'default nil :family mono-font :height 170 :weight 'light)
  (set-face-attribute 'fixed-pitch nil :family mono-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family variable-font :height 1.0))

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        package-native-compile t
        jit-lock-defer-time 0
        native-comp-jit-compilation t))

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq-default site-run-file nil
              inhibit-compacting-font-caches t
              frame-inhibit-implied-resize t
              lexical-binding t
              bidi-inhibit-bpa t
              vc-handled-backends nil
              kill-ring-max 100000
              mode-line-format nil
              initial-buffer-choice nil
              inhibit-x-resources t
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

;; Only enable archives after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                                     ("gnu" . "https://elpa.gnu.org/packages/")
                                     ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
                  package-archive-priorities '(("gnu" . 99)
                                               ("nongnu" . 80)
                                               ("melpa" . 0)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)))

(provide 'early-init)
;;; early-init.el ends here
