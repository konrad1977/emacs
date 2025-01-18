;;; Periphery-swiftformat --- Validate swift-format and show the result as flycheck list.  -*- lexical-binding: t; -*-

;;; Commentary: Package for showing linting as result in a tabulated list

;;; Code:
(require 'periphery)
(require 'periphery-helper)
(require 'cl-lib)
(require 'mode-line-hud)

(defvar swiftformat-command "swiftformat")
(defvar swift-version "6.0")
(defvar disabled-rules-list '(
                              "blankLinesAroundMark"
                              "blankLinesAtStartOfScope"
                              "trailingCommas"
                              "unusedArguments"
                              "wrapArguments"
                              "wrapMultilineStatementBraces"
                              ))

(defun send-swiftformat-result-to-periphery (text)
  "Let periphery parse the (as TEXT)."
  (periphery-run-parser text))

(defun periphery--create-disable-rules-list (list)
  "Create disable block for swift format (as LIST)."
  (if (> (length list) 0)
      (concat "--disable " (mapconcat 'identity list ","))
      ""))

;;;###autoload
(defun periphery-swiftformat-lint-buffer()
  "Lint current buffer using swiftsyntax."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (periphery-run-swiftformat-buffer
       :command (concat swiftformat-command " " file " --lint " (periphery--create-disable-rules-list disabled-rules-list) " --swiftversion " swift-version)
       :file file)))

;;;###autoload
(defun periphery-swiftformat-autocorrect-buffer()
  "Autocorrect current buffer using swiftsyntax."
  (interactive)
  (save-some-buffers t)
  (if-let* ((file (buffer-file-name)))
      (periphery-run-swiftformat-buffer
       :command (concat swiftformat-command " " file " " (periphery--create-disable-rules-list disabled-rules-list) " --swiftversion " swift-version)
       :file file)))

(cl-defun periphery-run-swiftformat-buffer (&key command &key file)
  "Run swiftformat for current buffer (as COMMAND FILE)."
  (if (executable-find swiftformat-command)
      (progn
        (mode-line-hud:notification :message
                                    (format "Formatting %s"
                                            (propertize (file-name-sans-extension (file-name-nondirectory file)) 'face 'success))
                                    :seconds 3)

        (async-start-command-to-string
         :command command
         :callback '(lambda (result)
                      (send-swiftformat-result-to-periphery result))))

    (mode-line-hud:notification
     :message (propertize "Swiftformat not installed" 'face 'font-lock-keyword-face)
     :seconds 2)))

;;;###autoload
(defun periphery-run-swiftformat-for-project()
  "Run LOCO linter."
  (interactive)
  (if (executable-find swiftformat-command)
      (progn
        (let ((default-directory (periphery-helper:project-root-dir)))
          (async-start-command-to-string
           :command (concat swiftformat-command " . --lint " (periphery--create-disable-rules-list disabled-rules-list) " --swiftversion " swift-version)
           :callback '(lambda (result)
                        (send-swiftformat-result-to-periphery result))))

        (mode-line-hud:notification :message
                                    (format "Formatting %s"
                                            (propertize (file-name-nondirectory (directory-file-name (file-name-directory (vc-root-dir))))))
                                    :seconds 2))

    (mode-line-hud:update :message (propertize "Swiftformat not installed" 'face 'font-lock-keyword-face))))

(provide 'periphery-swiftformat)

;;; periphery-swiftformat.el ends here.
