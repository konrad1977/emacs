;;; Periphery-loco --- Validate .strings and .swift file and show the result as flycheck list.  -*- lexical-binding: t; -*-

;;; Commentary: Package for showing linting as result in a tabulated list

;;; Code:
(require 'periphery-helper)
(require 'periphery)
(require 'cl-lib)

(defvar loco-command "loco")

(defun send-loco-result-to-periphery (text)
  "Let periphery parse the (as TEXT)."
  (periphery-run-parser text)
  (periphery-message-with-count
   :tag "[Done]"
   :text "Result"
   :attributes 'success))

(defun periphery-run-loco()
  "Run LOCO linter."
  (interactive)
  (if (executable-find loco-command)
      (progn
        (let ((default-directory (vc-root-dir)))
          (async-shell-command-to-string
           :process-name "loco"
           :command (concat loco-command " --no-color")
           :callback #'send-loco-result-to-periphery))
        (periphery-message
         :tag "[Linting|Loco]"
         :text (file-name-nondirectory (directory-file-name
                                        (file-name-directory (vc-root-dir))))
         :attributes 'success))
  (periphery-message
   :tag "[Failed]"
   :text (format "Install %s to use this command." loco-command)
   :attributes 'warning)))

(provide 'periphery-loco)
;;; periphery-loco.el ends here.
