;;; early-init.el --- -*- lexical-binding: t -*-

;;; Commentary: My early init file

;;; Code:

(setq read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil
      gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024))))

(set-language-environment "UTF-8")


;; (setq default-input-method nil)
;; (setq ad-redefinition-action 'accept)
;; (setq idle-update-delay 1.0)
;; (setq inhibit-compacting-font-caches t)
;; (set-language-environment "UTF-8")

;; (setq-default bidi-display-reordering 'left-to-right
;;               bidi-paragraph-direction 'left-to-right)

;(customize-set-variable 'native-comp-async-report-warnings-errors nil)
;(customize-set-variable 'comp-async-report-warnings-errors nil)
;(customize-set-variable 'native-comp-async-query-on-exit t)
;(customize-set-variable 'comp-async-query-on-exit t)
;(customize-set-variable 'native-comp-speed 3)
;(customize-set-variable 'comp-speed 3)

; (setq kill-ring-max 100000)
; (custom-set-variables '(savehist-additional-variables '(kill-ring)))

; (setq ffap-machine-p-known 'reject)
; (setq ffap-alist nil)                ; faster, dumber prompting
; (setq ffap-url-regexp nil)           ; disable URL features in ffap
; (setq ffap-shell-prompt-regexp nil)  ; disable shell prompt stripping
; (setq ffap-gopher-regexp nil)        ; disable gopher bookmark matching
; (setq ffip-use-rust-fd t)

;(prefer-coding-system 'utf-8
;(set-default-coding-systems 'utf-8)
;(set-terminal-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)
;
;(setq ffap-machine-p-known 'reject)
;(setq idle-update-delay 1.0)
;(setq auto-mode-case-fold nil)
;
;(setq-default bidi-display-reordering 'left-to-right
;              bidi-paragraph-direction 'left-to-right)

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
      auto-mode-case-fold nil
      bidi-inhibit-bpa t
      site-run-file nil)

;; (advice-add #'display-startup-echo-area-message :override #'ignore)
;; (advice-add #'display-startup-screen :override #'ignore)

(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ("stable" . 70)
                                                      ("melpa"  . 0)))

(package-initialize)
(setopt package-install-upgrade-built-in t)

(provide 'early-init)
;;; early-init.el ends here
