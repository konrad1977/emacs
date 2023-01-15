;;; Periphery-swiftlint  -*- lexical-binding: t; -*-

;;; Commentary: Package for showing all swiftlint results abulated list

;;; Code:
(require 'periphery-helper)
(require 'periphery)
(require 'projectile)
(require 'cl-lib)

(defconst swiftlint "swiftlint")

(defun get-swiftlint-file-root ()
    "Get the path of the swiftlint file."
    (let* ((default-directory (projectile-project-root))
           (root (locate-dominating-file default-directory ".swiftlint.yml")))
      root))

(defun periphery--swiftlint:analyze-result (result)
  "Analyze RESULT."
  (periphery-run-parser result))

(defun periphery-run-swiftlint ()
  "Lint the whole project not just current file."
  (interactive)
  (if (executable-find swiftlint)
      (progn
        (let ((default-directory (get-swiftlint-file-root)))
          (async-shell-command-to-string
           :process-name swiftlint
           :command swiftlint
           :callback #'periphery--swiftlint:analyze-result))
        (message-with-color
         :tag "[Linting|Swiftlint]"
         :text (file-name-nondirectory (directory-file-name
                                        (file-name-directory (get-swiftlint-file-root))))
         :attributes 'success))
    (message-with-color
     :tag "[Failed]"
     :text (format "Install %s to use this command." swiftlint)
     :attributes 'warning)))

(provide 'periphery-swiftlint)
;;; periphery-swiftlint.el ends here.

