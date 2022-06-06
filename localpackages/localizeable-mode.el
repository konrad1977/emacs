;;; localizeable-mode.el --- Highlight strings.

;;; Code:

(require 'periphery)
(require 'swift-additions)
(load "swift-additions")

(defface localizeable-variable-face
  '((t (:inherit font-lock-variable-name-face :italic t)))
  "The key in strings file."
  :group 'localizeable-font)

(defface localizeable-value-face
  '((t (:inherit font-lock-builtin-face :bold t)))
  "The value in strings file."
  :group 'localizeable-font)

(defface localizeable-delimiter-face
  '((t (:inherit font-lock-constant-face :italic t)))
  "The simicolon at the end."
  :group 'localizeable-font)

(defface localizeable-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Comments."
  :group 'localizeable-font)

(defface localizeable-equals-face
  '((t (:inherit font-lock-warning-face :bold t)))
  "Equals."
  :group 'localizeable-font)

(defconst bartycrouch-lint-command "bartycrouch lint -x")

(defvar localizeable-mode-map nil "Keymap for localizeable.")
(setq localizeable-mode-map (make-sparse-keymap))
(define-key localizeable-mode-map (kbd "C-c C-c") #'localizeable-mode-analyze)

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
                 ("^\\(\"[^\"]+\"\\)\s+=\s+\\(\"[^\"]+\"\\)\\(;\\)"
                  (1 'localizeable-value-face t)
                  (2 'localizeable-variable-face t)
                  (3 'localizeable-delimiter-face t)
                  )
                 ("\/\\*[^*]*\\*+\\(?:[^/*][^*]*\\*+\\)*/" 0 'localizeable-comment-face t)
                 ("\\(=\\)" 0 'localizeable-equals-face t)))

(defun parse-localizeable (text)
  "Parse output from TEXT."
  (if (or (string-match-p (regexp-quote "BUILD FAILED") text)
          (string-match-p (regexp-quote "error") text)
          (string-match-p (regexp-quote "warning") text))
      (progn
        (periphery-run-bartycrouch-parser text (projectile-project-root))
        (message-with-color :tag "[Warning]" :text "Found errors in localization files." :attributes '(:inherit error)))
    (message-with-color :tag "[Success]" :text "No errors found." :attributes '(:inherit success))))

(defun localizeable-mode-analyze ()
  "Analyse all localizeable.strings."
  (interactive)
  (if (executable-find "bartycrouch")
      (progn
        (save-some-buffers t)
        (let ((default-directory (projectile-project-root)))
          (async-shell-command-to-string "Periphery" bartycrouch-lint-command #'parse-localizeable))
        (message-with-color :tag "[Analysing]" :text "Localizeble.strings" :attributes '(:inherit warning)))
    (message-with-color :tag "[Error]" :text "bartycrouch not installed. run 'brew install bartycrouch'" :attributes '(:inherit warning))))

(provide 'localizeable-mode)

;;; localizeable-mode.el ends here

