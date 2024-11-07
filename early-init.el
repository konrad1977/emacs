;;; early-init.el --- Early Init File -*- no-byte-compile: t; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Commentary: My early init file

;;; Code:

(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      read-process-output-max (* 4 1024 1024)
      file-name-handler-alist nil
      kill-ring-max 100000
      mode-line-format nil
      process-adaptive-read-buffering nil)

(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable 'comp-async-report-warnings-errors nil)
(customize-set-variable 'native-comp-async-query-on-exit t)
(customize-set-variable 'comp-async-query-on-exit t)
(customize-set-variable 'native-comp-speed 3)
(customize-set-variable 'comp-speed 3)

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

(push '(ns-use-native-fullscreen . t) default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(background-color . "#13131a") default-frame-alist)
(push '(foreground-color . "#a0a0ae") default-frame-alist)

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
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      load-prefer-newer noninteractive
      ring-bell-function 'ignore
      ;; Remove the conflicting settings and use just one:
      byte-compile-warnings '(not obsolete free-vars unresolved noruntime lexical make-local)
      warning-minimum-level :emergency  ; More strict than :error
      warning-suppress-log-types '((comp) (bytecomp) (obsolete))
      warning-suppress-types '((comp) (bytecomp) (obsolete))
      site-run-file nil)

;; Add this to ensure warnings are suppressed even for already-loaded packages
(with-eval-after-load 'warnings
  (setq warning-suppress-types '((comp) (bytecomp) (obsolete))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024)
                  gc-cons-percentage 0.4
                  file-name-handler-alist file-name-handler-alist-original)))

(provide 'early-init)
;;; early-init.el ends here
