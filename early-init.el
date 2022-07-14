;;; early-init.el --- Early init

;;; Commentary: My early init file

;;; Code:
(setq idle-update-delay 0.2)

(set-frame-parameter (selected-frame) 'alpha '(98 . 95))
(add-to-list 'default-frame-alist '(alpha . (98 . 95)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)

(customize-set-variable 'native-comp-async-report-warnings-errors nil)
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)

(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 10 1024 1024)))

(customize-set-variable 'max-specpdl-size 13000)

(setq-default frame-inhibit-implied-resize t
              frame-resize-pixelwise t
              frame-title-format "\n"
              inhibit-startup-screen t
              inhibit-startup-message t
              inhibit-splash-screen t
              inhibit-compacting-font-caches t
              initial-scratch-message nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      package-enable-at-startup nil
      ns-pop-up-frames nil
      site-run-file nil
      tool-bar-mode nil
      initial-major-mode 'fundamental-mode
      menu-bar-mode nil
      scroll-bar-mode nil)

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(provide 'early-init)
;;; early-init.el ends here
