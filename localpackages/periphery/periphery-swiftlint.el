;;; Periphery-swiftlint  -*- lexical-binding: t; -*-

;;; Commentary: Package for showing all swiftlint results abulated list

;;; Code:
(require 'periphery-helper)
(require 'periphery)
(require 'cl-lib)

(defun get-swiftlint-file-root ()
    "Get the path of the swiftlint file."
    (if-let* ((default-directory (periphery-helper:project-root-dir))
              (root (locate-dominating-file default-directory ".swiftlint.yml")))
        root)
    default-directory)

(defun periphery--swiftlint:analyze-result (result)
  "Analyze RESULT."
  (periphery-run-parser result (lambda ()
                                 (message-with-color
                                  :tag "[Success]"
                                  :text "No lint warning or errors."
                                  :attributes 'success))))

(defun periphery-run-swiftlint ()
  "Lint the whole project not just current file."
  (interactive)
  (if (executable-find "swiftlint")
      (progn
        (let ((default-directory (get-swiftlint-file-root)))
          (async-start-command-to-string
           :command "swiftlint"
           :callback '(lambda (result) (periphery--swiftlint:analyze-result result))))
        (message-with-color
         :tag "[Linting|Swiftlint]"
         :text (file-name-nondirectory (directory-file-name (file-name-directory default-directory)))
         :attributes 'success))
    (message-with-color
     :tag "[Failed]"
     :text (format "Install %s to use this command. 'swiftlint'")
     :attributes 'warning)))

(provide 'periphery-swiftlint)
;;; periphery-swiftlint.el ends here.

