;;; Periphery-swiftformat --- Validate swift-format and show the result as flycheck list.  -*- lexical-binding: t; -*-

;;; Commentary: Package for showing linting as result in a tabulated list

;;; Code:
(require 'periphery-helper)
(require 'periphery)
(require 'cl-lib)

(defvar swiftformat-command "swiftformat")
(defvar swift-version "5.7.2")

(defun send-swiftformat-result-to-periphery (text)
  "Let periphery parse the (as TEXT)."
  (periphery-run-parser text))

(defun periphery-run-swiftformat-buffer ()
  "Run swiftformat for current buffer."
  (interactive)
  (if (executable-find swiftformat-command)
      (progn
        (let ((file (buffer-file-name)))
          (async-shell-command-to-string
           :process-name "swiftformat"
           :command (concat swiftformat-command " " file " -lint --swiftversion " swift-version)
           :callback #'send-swiftformat-result-to-periphery))
        (message-with-color
         :tag "[Linting|swiftformat]"
         :text (file-name-nondirectory file)
         :attributes 'success)))
  (message-with-color
   :tag "[Failed]"
   :text (format "Install %s to use this command." swiftformat-command)
   :attributes 'warning))

(defun periphery-run-swiftformat()
  "Run LOCO linter."
  (interactive)
  (if (executable-find swiftformat-command)
      (progn
        (let ((default-directory (vc-root-dir)))
          (async-shell-command-to-string
           :process-name "swiftformat"
           :command (concat swiftformat-command " . -lint --swiftversion " swift-version)
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
