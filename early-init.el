;;; early-init.el --- summary -*- lexical-binding: t; no-byte-compile: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-

;;; Commentary:
;; Emacs early initialization file loaded before package system and UI.
;; Optimized for maximum performance and minimal startup time.

;;; Code:

;; ;; Must be set very early to suppress all warnings
(setq package-enable-at-startup nil)
(setq warning-minimum-level :error)
(setq byte-compile-warnings nil)
(setq warning-suppress-types '((comp) (bytecomp) (obsolete)))
(setq warning-suppress-log-types '((comp) (bytecomp) (obsolete)))
(setq-default mode-line-format nil)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(setopt native-comp-async-report-warnings-errors nil)

;; Native-comp settings
(setopt native-comp-speed 2)
(setopt native-comp-deferred-compilation t)

(setopt frame-inhibit-implied-resize t
        frame-title-format "\n"
        gc-cons-threshold most-positive-fixnum
        inhibit-compacting-font-caches t
        inhibit-default-init t
        inhibit-splash-screen t
        inhibit-startup-buffer-menu t
        inhibit-startup-echo-area-message t
        inhibit-startup-message t
        inhibit-startup-screen t
        initial-buffer-choice nil
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil
	message-log-max nil
	default-directory "~/"
        read-process-output-max (* 8 1024 1024)
        default-frame-alist '((background-color . "#13131a")
                              (foreground-color . "#a0a0ae")
                              (vertical-scroll-bars . nil)
			      (min-height . 1)
			      (min-width . 1)
			      (height . 45)
			      (width . 81)
			      (internal-border-width . 24)
                              (left-fringe . 1)
                              (right-fringe . 1)
                              (horizontal-scroll-bars . nil)
                              (mac-transparent-titlebar . t)
                              (ns-transparent-titlebar . t)
                              (undecorated-round . t)
			      (fullscreen . maximized)))

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setopt tool-bar-mode nil
        menu-bar-mode nil
        tooltip-mode nil
        scroll-bar-mode nil)

;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; ;;   ;; Font settings with error handling
(condition-case nil
    (let ((mono-font "Iosevka Fixed Curly")
          (variable-font "Iosevka Aile"))
      (set-face-attribute 'default nil :family mono-font :width 'condensed :weight 'extra-light :height 180)
      (set-face-attribute 'fixed-pitch nil :family mono-font)
      (set-face-attribute 'variable-pitch nil :family variable-font :height 1.0))
  (error nil))

;; Emoji font fallback
(set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") nil 'append)
;; restore threshold to normal value after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore file handlers and GC settings
            (setq file-name-handler-alist file-name-handler-alist-original
                  gc-cons-threshold (* 256 1024 1024)
                  gc-cons-percentage 0.2)

            ;; Schedule periodic GC during idle time
            (run-with-idle-timer 5 t (lambda ()
                                       (let ((inhibit-message t))
                                         (garbage-collect))))

            ;; Log startup time
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done))
          100)

(provide 'early-init)
;;; early-init.el ends here
