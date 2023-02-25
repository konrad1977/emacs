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
              inhibit-compacting-font-caches nil
              initial-scratch-message nil
              mode-line-format nil
              package-native-compile t
              package-enable-at-startup nil
              indicate-buffer-boundaries '((bottom . left))
              default-frame-alist
              '((background-color . "#1e1e2e")       ; Default background color
                (bottom-divider-width . 1)           ; Thin horizontal window divider
                (foreground-color . "#cdd6f4")       ; Default foreground color
                (fullscreen . maximized)             ; Maximize the window by default
                (horizontal-scroll-bars . nil)       ; No horizontal scroll-bars
                (menu-bar-lines . 0)                 ; No menu bar
                (right-divider-width . 1)            ; Thin vertical window divider
                (tool-bar-lines . 0)                 ; No tool bar
                (left-fringe . 16)
                (right-fringe . 0)
                (height . 27)
                (width . 101)
                (vertical-scroll-bars . nil)))       ; No vertical scroll-bars

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 5 1024 1024)))

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

;; ;; Defer garbage collection further back in the startup process
(setq package-enable-at-startup nil
      ns-pop-up-frames nil
      site-run-file nil
      tool-bar-mode nil
      initial-major-mode 'fundamental-mode
      menu-bar-mode nil
      scroll-bar-mode nil
      load-prefer-newer noninteractive
      garbage-collect-maybe t
      fringes-outside-margins t
      left-margin-width 1
      display-line-numbers-widen t
      redisplay-skip-fontification-on-input t)

;; (global-display-fill-column-indicator-mode)

(dolist (face '(mode-line mode-line-inactive))
  (set-face-attribute face nil :height 151))

(set-face-attribute 'fill-column-indicator nil
                    :family "SF Mono")

;; Make sure SF Symbols and Octicons will be displayed.
(when (display-graphic-p)
  (dolist (font-pua-points '(("SF Pro Text" . (#x100000 . #x102000))
                             ("github-octicons" . (#xf000 . #xf27c))))
    (let ((family (car font-pua-points))
          (range (cdr font-pua-points)))
      (set-fontset-font t range family nil 'prepend))))

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
