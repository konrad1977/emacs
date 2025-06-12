;;; swift-lsp --- Language Server Protocol (LSP) support for Swift -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'cl-lib) ;; Ensure cl-lib is available for cl-pushnew

;;;###autoload
(defun ios-simulator-target ()
  "Get the current simulator sdk."
  (let* ((target-components (split-string (string-trim (shell-command-to-string "clang -print-target-triple")) "-"))
         (arch (nth 0 target-components))
         (vendor (nth 1 target-components))
         (version (string-trim (shell-command-to-string "xcrun --sdk iphonesimulator --show-sdk-version"))))
    (format "%s-%s-ios%s-simulator" arch vendor version)))

;;;###autoload
(defun lsp-arguments ()
  "Get the lsp arguments to support UIKit."
  (let* ((sdk (string-trim (shell-command-to-string "xcrun --show-sdk-path --sdk iphonesimulator")))
         (target (ios-simulator-target)))
    (list
     "-Xswiftc" "-sdk"
     "-Xswiftc" sdk
     "-Xswiftc" "-target"
     "-Xswiftc" target
     "-Xcc" "-DSWIFT_PACKAGE=0")))

;;;###autoload
(defun my-swift-mode:eglot-server-contact (_ignored)
  "Construct the list that eglot needs to start sourcekit-lsp."
  (let ((arglist (lsp-arguments))
        (sourcekit-lsp-path (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))
    (cl-pushnew sourcekit-lsp-path arglist :test #'equal)))

(provide 'swift-lsp)
;;; swift-lsp.el ends here
