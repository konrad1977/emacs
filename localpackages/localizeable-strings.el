;;; localizeable-strings-mode.el --- Highlight strings.

(require 'projectile)
(require 'swift-additions)
(require 'periphery)

;;; Code:
(defface localizeable-variable-face
  '((t (:inherit font-lock-variable-name-face :italic t)))
  "The key in strings file."
  :group 'string-mode)

(defface localizeable-value-face
  '((t (:inherit font-lock-builtin-face :bold t)))
  "The value in strings file."
  :group 'string-mode)

(defface localizeable-delimiter-face
  '((t (:inherit font-lock-constant-face :italic t)))
  "The simicolon at the end."
  :group 'string-mode)

(defface localizeable-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Comments."
  :group 'string-mode)

(defface localizeable-equals-face
  '((t (:inherit font-lock-warning-face :bold t)))
  "Equals."
  :group 'string-mode)

(defconst bartycrouch-lint-command "bartycrouch lint")

;;;###autoload
(define-derived-mode localizeable-strings-mode fundamental-mode
  (setq indicate-empty-lines t            ;; Show empty lines
		indicate-unused-lines t           ;; Show unused lines
        display-line-numbers t
        left-fringe-width 12
        left-margin-width 1
        mode-name "Localizable")
  (setq font-lock-defaults '(())))

(font-lock-add-keywords
 'localizeable-strings-mode '(
                 ("^\\(\"[^\"]+\"\\)\s+=\s+\\(\"[^\"]+\"\\)\\(;\\)"
                  (1 'localizeable-value-face t)
                  (2 'localizeable-variable-face t)
                  (3 'localizeable-delimiter-face t)
                  )
                 ("\/\\*[^*]*\\*+\\(?:[^/*][^*]*\\*+\\)*/" 0 'localizeable-comment-face t)
                 ("\\(=\\)" 0 'localizeable-equals-face t)
                 ))

(add-to-list 'auto-mode-alist '("\\.strings\\'" . localizeable-strings-mode))

(defun localizeable-mode-analyze ()
  "Analyse all localizeable.strings."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (async-shell-command-to-string "Periphery" bartycrouch-lint-command #'bartycrouch-run-parser))
  (message-with-color "[Analysing]" "Localizeble.strings" '(:inherit 'warning)))

(provide 'localizeable-strings)

;;; localizeable-strings-mode.el ends here

