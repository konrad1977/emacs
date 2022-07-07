;;; early-init.el --- Early init

;;; Commentary: My early init file

;;; Code:
(setq idle-update-delay 0.2)

(set-frame-parameter (selected-frame) 'alpha '(98 . 95))
(add-to-list 'default-frame-alist '(alpha . (98 . 95)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 10 1024 1024)))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)
(setq inhibit-startup-message t)
(setq ns-pop-up-frames nil)
(setq site-run-file nil)
(setq inhibit-compacting-font-caches t)

(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(provide 'early-init)
;;; early-init.el ends here
