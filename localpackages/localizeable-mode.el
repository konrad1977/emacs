;;; localizeable-mode.el --- Highlight strings.

;;; Code:

(defvar localizeable-mode-map nil "Keymap for localizeable.")
(setq localizeable-mode-map (make-sparse-keymap))

(add-to-list 'auto-mode-alist '("\\.strings\\'" . localizeable-mode))

;;;###autoload
(define-derived-mode localizeable-mode fundamental-mode
  (setq indicate-empty-lines t            ;; Show empty lines
		indicate-unused-lines t           ;; Show unused lines
        display-line-numbers t
        left-fringe-width 12
        left-margin-width 1
        mode-name "Localizable")
  (setq font-lock-defaults '(())))

(font-lock-add-keywords
 'localizeable-mode '(
                 ("\\(\"[^\"]+\"\\)\s+=\s+\\(\"[^\"]+\"\\)\\(;\\)"
                  (1 'font-lock-type-face t)
                  (2 'font-lock-string-face t)
                  (3 'font-lock-delimiter-face t)
                  )
                 ("\\(\\/\\/.*\\)" 0 'font-lock-comment-face t)
                 ("\\(=\\)" 0 'font-lock-operator-face t)))

(provide 'localizeable-mode)

;;; localizeable-mode.el ends here

