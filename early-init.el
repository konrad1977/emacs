;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;; Emacs early initialization file loaded before package system and UI.
;; Optimized for maximum performance and minimal startup time.

;;; Code:

;; Fix for custom-declare-variable errors in Emacs 30.2.50
;; This must be done very early before any packages load
(defun mk/custom-declare-variable-safe (orig-fun symbol value &rest args)
  "Safely declare custom variables, creating them if they don't exist."
  (condition-case err
      (apply orig-fun symbol value args)
    (void-variable
     ;; If we get a void-variable error, define the variable first
     (unless (boundp symbol)
       (set symbol (if (functionp value) 
                      (ignore-errors (funcall value))
                    value)))
     symbol)
    (error
     ;; For any other error, just define the variable with a safe default
     (unless (boundp symbol)
       (set symbol nil))
     symbol)))

(advice-add 'custom-declare-variable :around #'mk/custom-declare-variable-safe)

;; =====================
;; Suppress All Warnings
;; =====================

;; Must be set very early to suppress all warnings
(setq warning-minimum-level :error)
(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)
(setq warning-suppress-types '((comp) (bytecomp) (obsolete)))
(setq warning-suppress-log-types '((comp) (bytecomp) (obsolete)))

;; =====================
;; Critical Performance
;; =====================

(setenv "LSP_USE_PLISTS" "true")

;; Disable file handlers and garbage collection during startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation nil
      comp-deferred-compilation nil)

;; Inhibit resizing frame during startup
(setq frame-inhibit-implied-resize t)

;; Disable unnecessary UI elements
(setq-default inhibit-startup-screen t
              inhibit-startup-message t
              inhibit-startup-echo-area-message t
              inhibit-startup-buffer-menu t
              initial-scratch-message nil
              initial-buffer-choice nil
              initial-major-mode 'fundamental-mode)

;; Disable site-start processing for faster startup
(setq site-run-file nil)

;; ;; =====================
;; ;; Package System
;; ;; =====================

;; ;; Defer package system initialization
(setq package-enable-at-startup nil
      package-quickstart t
      package-native-compile t
      load-prefer-newer t
      byte-compile-warnings '(not obsolete))

;; =====================
;; UI/Frame Configuration
;; =====================

;; ;; Prevent mode-line updates during init
(setq-default mode-line-format nil)

;; Basic frame settings
(setq-default frame-title-format '("%b - Emacs")
              bidi-display-reordering 'left-to-right
              bidi-inhibit-bpa t
              bidi-paragraph-direction 'left-to-right
              cursor-type 'box
              cursor-in-non-selected-windows nil
              use-dialog-box nil
              use-file-dialog nil
              inhibit-compacting-font-caches t
              redisplay-skip-fontification-on-input t)

;; Minimal frame appearance
(setq default-frame-alist
      '((background-color . "#13131a")
        (foreground-color . "#a0a0ae")
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (ns-transparent-titlebar . t)
        (mac-transparent-titlebar . t)
        (fullscreen . maximized)
        (undecorated-round . t)
        (left-fringe . 12)
        (right-fringe . 12)))

;; Disable these modes early
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;; ;; =====================
;; ;; Platform Specific
;; ;; =====================

(when (eq system-type 'darwin)
  ;; macOS optimizations
  (setq ns-use-proxy-icon nil
        mac-command-modifier 'meta
        mac-option-modifier 'none
        ns-use-native-fullscreen t
        ns-use-srgb-colorspace t
        ns-pop-up-frames nil
        mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil)

  ;; Font settings with error handling
  (condition-case nil
      (let ((mono-font "Iosevka Curly")
            (variable-font "Iosevka Aile"))
        (set-face-attribute 'default nil :family mono-font :height 160 :weight 'light)
        (set-face-attribute 'fixed-pitch nil :family mono-font :weight 'light)
        (set-face-attribute 'variable-pitch nil :family variable-font :height 1.0))
    (error nil))
  
  ;; Emoji font fallback
  (set-fontset-font t 'unicode (font-spec :family "Apple Color Emoji") nil 'append))

;; ;; =====================
;; ;; Native Compilation
;; ;; =====================

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-jit-compilation t
        native-comp-async-report-warnings-errors 'silent
        native-comp-async-query-on-exit t
        native-comp-async-jobs-number (- (string-to-number (string-trim-right (shell-command-to-string "nproc"))) 1)))


;; ;; =====================
;; ;; Additional Optimizations
;; ;; =====================

;; Reduce rendering/line scan work
(setq auto-mode-case-fold nil
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      ffap-machine-p-known 'reject
      inhibit-default-init t
      idle-update-delay 1.0
      read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering t)

;; ;; =====================
;; ;; Post-Startup Hooks
;; ;; =====================

;; ;; Restore settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore file handlers and GC settings
            (setq file-name-handler-alist file-name-handler-alist-original
                  gc-cons-threshold (* 128 1024 1024)  ; 128MB - better for modern systems
                  gc-cons-percentage 0.1)  ; Lower percentage for more frequent but smaller GCs
            
            ;; Initialize package system with all archives
            (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                                     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                                     ("gnu" . "https://elpa.gnu.org/packages/")
                                     ("org" . "https://orgmode.org/elpa/"))
                  package-archive-priorities '(("gnu" . 99)
                                               ("nongnu" . 85)
                                               ("org" . 75)
                                               ("melpa" . 0)))
            
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
          100)  ; Run with high priority

(provide 'early-init)
;;; early-init.el ends here
