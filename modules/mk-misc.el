;;; mk-misc.el --- Miscellaneous configurations -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains miscellaneous configurations and package setups for Emacs.
;;; Code:

(use-package weather-scout
  :defer t
  :commands (weather-scout-show-forecast)
  :init
  ;; Use Vim motions in the weather forecast buffer (optional)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'weather-scout-mode 'motion)))

(use-package periphery-swiftformat
  :ensure nil
  :after swift-ts-mode
  :bind
  (:map swift-ts-mode-map
        ("C-c C-o" . #'periphery-swiftformat-lint-buffer)
        ("M-o" . #'periphery-swiftformat-autocorrect-buffer)
        ("C-c C-p" . #'periphery-run-swiftformat-for-project)))


(provide 'mk-misc)
;;; mk-misc.el ends here