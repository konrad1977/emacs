;;; early-init.el --- -*- lexical-binding: t -*-

;;; Commentary: My early init file

;;; Code:

(setq gc-cons-threshold-original gc-cons-threshold
      gc-cons-threshold (* 512 1024 1024))

(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 4 1024 1024)))

(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq frame-inhibit-implied-resize t
      display-line-numbers-widen t
      frame-resize-pixelwise t
      frame-title-format nil
      fringes-outside-margins t
      idle-update-delay 1.0
      indicate-buffer-boundaries '((bottom . right))
      inhibit-compacting-font-caches t
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-message t
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      left-margin-width 1
      load-prefer-newer noninteractive
      menu-bar-mode nil
      mode-line-format nil
      max-lisp-eval-depth 10000
      ns-pop-up-frames nil
      package-enable-at-startup nil
      package-native-compile t
      redisplay-skip-fontification-on-input t
      scroll-bar-mode nil
      site-run-file nil
      tool-bar-mode nil
      default-frame-alist
      '((background-color . "#1e1e2e")       ; Default background color
        (bottom-divider-width . 0)           ; Thin horizontal window divider
        (foreground-color . "#cdd6f4")       ; Default foreground color
        (fullscreen . maximized)             ; Maximize the window by default
        (horizontal-scroll-bars . nil)       ; No horizontal scroll-bars
        (menu-bar-lines . 0)                 ; No menu bar
        (right-divider-width . 1)            ; Thin vertical window divider
        (tool-bar-lines . 0)                 ; No tool bar
        (right-fringe . 0)
        (ns-use-native-fullscreen . t)
        (ns-transparent-titlebar . t)
        (vertical-scroll-bars . nil)))       ; No vertical scroll-bars


;; (setq file-name-handler-alist-origial file-name-handler-alist)
;; (setq file-name-handler-alist nil)

;; (run-with-idle-timer
;;  3 nil
;;  (lambda ()
;;    (setq gc-cons-threshold gc-cons-threshold-original)
;;    (setq file-name-handler-alist file-name-handler-alist-original)
;;    (makunbound 'gc-cons-threshold-original)
;;    (makunbound 'file-name-handler-alist-original)))

;; (set-face-attribute 'fill-column-indicator nil
;;                     :family "SF Mono")

;; ;; Make sure SF Symbols and Octicons will be displayed.
;; (when (display-graphic-p)
;;   (dolist (font-pua-points '(("SF Pro Text" . (#x100000 . #x102000))
;;                              ("github-octicons" . (#xf000 . #xf27c))))
;;     (let ((family (car font-pua-points))
;;           (range (cdr font-pua-points)))
;;       (set-fontset-font t range family nil 'prepend))))

(provide 'early-init)
;;; early-init.el ends here
