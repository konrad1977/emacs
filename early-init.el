;; early-init.el --- summary -*- lexical-binding: t; no-byte-compile: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-

;;; Commentary:
;; Emacs early initialization file loaded before package system and UI.
;; Optimized for maximum performance and minimal startup time.

;;; Code:
(defvar file-name-handler-alist-original file-name-handler-alist)
(setopt file-name-handler-alist nil)

(setopt warning-minimum-level :error)
(setopt native-comp-async-report-warnings-errors nil
        native-comp-warning-on-missing-source nil
        byte-compile-warnings '(not cl-functions obsolete)
        warning-suppress-log-types '((comp) (bytecomp) (files))
        warning-suppress-types '((comp) (bytecomp) (files)))

;;; Startup performance optimizations
(setopt gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.8)

;;; UI suppression (must be early)
(setopt inhibit-default-init t
        inhibit-startup-message t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        inhibit-splash-screen t
        inhibit-startup-buffer-menu t
        initial-scratch-message nil
        initial-major-mode 'fundamental-mode
        frame-inhibit-implied-resize t)

;;; Frame defaults (safe here)
(setq default-frame-alist
      '((background-color . "#13131a")
        (foreground-color . "#a0a0ae")
        (min-height . 1)
        (min-width . 1)
        (height . 45)
        (width . 81)
        (internal-border-width . 1)
        (left-fringe . 8)
        (right-fringe . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars . nil)
        (vertical-scroll-bars . nil)
        (mac-transparent-titlebar . t)
        (ns-transparent-titlebar . t)
        (fullscreen . maximized)))

;;; Avoid mode-line work before init
(setq-default mode-line-format nil)

;;; Restore sane defaults after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original
                  gc-cons-threshold (* 384 1024 1024)
                  gc-cons-percentage 0.2)

            ;; Periodic GC during idle
            (run-with-idle-timer
             5 t
             (lambda ()
               (let ((inhibit-message t))
                 (garbage-collect))))

            (message "Emacs started in %.2f seconds with %d GCs"
                     (float-time
                      (time-subtract after-init-time before-init-time))
                     gcs-done)))

(provide 'early-init)
;;; early-init.el ends here
