(setq native-comp-speed 2
      comp-speed 2)
(setq native-comp-async-report-warnings-errors nil
      comp-async-report-warnings-errors nil)
(setq native-comp-async-query-on-exit t
      comp-async-query-on-exit t)

(setq comp-deferred-compilation nil)

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max (* 10 1024 1024)))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)
(setq inhibit-startup-message t)
(setq ns-pop-up-frames nil)
(setq site-run-file nil)
(setq inhibit-compacting-font-caches t)

(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right
			  bidi-paragraph-direction 'left-to-right
			  cursor-in-non-selected-windows nil
			  highlight-nonselected-windows nil
			  fringes-outside-margins nil
			  indicate-buffer-boundaries nil
			  indicate-empty-lines nil)

(setq frame-resize-pixelwise t
	  frame-inhibit-implied-resize t
	  window-resize-pixelwise t)

(provide 'early-init)
