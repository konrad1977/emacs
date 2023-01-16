;;; Periphery-swiftformat --- Validate swift-format and show the result as flycheck list.  -*- lexical-binding: t; -*-

;;; Commentary: Package for showing linting as result in a tabulated list

;;; Code:
(require 'periphery-helper)
(require 'periphery)
(require 'cl-lib)

(defvar swiftformat-command "swiftformat")
(defvar swift-version "5.7.2")
(defvar disabled-rules-list '(
                              "wrapArguments"
                              "blankLinesAroundMark"
                              "blankLinesAtStartOfScope"
                              "unusedArguments"
                              "trailingCommas")
  )

(defun send-swiftformat-result-to-periphery (text)
  "Let periphery parse the (as TEXT)."
  (periphery-run-parser text))

(defun periphery--create-disable-rules-list (list)
  "Create disable block for swift format (as LIST)."
  (if (> (length list) 0)
      (concat "--disable " (mapconcat 'identity list ","))
      ""))

(defun periphery-swiftformat-lint-buffer()
  "Lint current buffer using swiftsyntax."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (periphery-run-swiftformat-buffer
       :command (concat swiftformat-command " " file " --lint " (periphery--create-disable-rules-list disabled-rules-list) " --swiftversion " swift-version)
       :file file)))

(defun periphery-swiftformat-autocorrect-buffer()
  "Autocorrect current buffer using swiftsyntax."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (periphery-run-swiftformat-buffer
       :command (concat swiftformat-command " " file " " (periphery--create-disable-rules-list disabled-rules-list) " --swiftversion " swift-version)
       :file file)))

(cl-defun periphery-run-swiftformat-buffer (&key command &key file)
  "Run swiftformat for current buffer."
  (if (executable-find swiftformat-command)
      (progn
            (async-shell-command-to-string
             :process-name "swiftformat"
             :command command
             :callback #'send-swiftformat-result-to-periphery)
            (message-with-color
             :tag "[Linting|swiftformat]"
             :text (file-name-nondirectory file)
             :attributes 'success)))
  (message-with-color
   :tag "[Failed]"
   :text (format "Install %s to use this command." swiftformat-command)
   :attributes 'warning))

(defun periphery-run-swiftformat-for-project()
  "Run LOCO linter."
  (interactive)
  (if (executable-find swiftformat-command)
      (progn
        (let ((default-directory (vc-root-dir)))
          (async-shell-command-to-string
           :process-name "swiftformat"
           :command (concat swiftformat-command " . -lint --swiftversion " swift-version (periphery--create-disable-rules-list disabled-rules-list))
           :callback #'send-swiftformat-result-to-periphery))
        (message-with-color
         :tag "[Linting|swiftformat]"
         :text (file-name-nondirectory (directory-file-name
                                        (file-name-directory (vc-root-dir))))
         :attributes 'success))
  (message-with-color
   :tag "[Failed]"
   :text (format "Install %s to use this command." swiftformat-command)
   :attributes 'warning)))

(provide 'periphery-swiftformat)
;;; periphery-swiftformat.el ends here.
