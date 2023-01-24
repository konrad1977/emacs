;;; early-init.el --- -*- lexical-binding: t -*-

;;; Commentary: My early init file

;;; Code:

(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default frame-inhibit-implied-resize t
              frame-resize-pixelwise t
              frame-title-format nil
              inhibit-startup-screen t
              inhibit-startup-buffer-menu t
              inhibit-startup-message t
              inhibit-splash-screen t
              inhibit-compacting-font-caches t
              initial-scratch-message nil
              load-prefer-newer t
              mode-line-format nil
              package-native-compile t
              default-frame-alist
              '((background-color . "#16161D")       ; Default background color
                (bottom-divider-width . 1)           ; Thin horizontal window divider
                (foreground-color . "#16161D")       ; Default foreground color
                (fullscreen . maximized)             ; Maximize the window by default
                ;; (alpha . (98 . 100))
                (horizontal-scroll-bars . nil)       ; No horizontal scroll-bars
                (menu-bar-lines . 0)                 ; No menu bar
                (right-divider-width . 1)            ; Thin vertical window divider
                (tool-bar-lines . 0)                 ; No tool bar
                (vertical-scroll-bars . nil)))       ; No vertical scroll-bars

(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)
(setq package-archives nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      package-enable-at-startup nil
      ns-pop-up-frames nil
      site-run-file nil
      tool-bar-mode nil
      initial-major-mode 'fundamental-mode
      menu-bar-mode nil
      scroll-bar-mode nil
      load-prefer-newer noninteractive
      idle-update-delay 0.2
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 10 1024 1024)))

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
