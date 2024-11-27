;;; swift-lsp --- Language Server Protocol (LSP) support for Swift -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

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
     "-Xswiftc" target)))

;;;###autoload
(defun my-swift-mode:eglot-server-contact (_ignored)
  "Construct the list that eglot needs to start sourcekit-lsp."
  (let ((arglist (lsp-arguments))
        (sourcekit-lsp-path (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))
    (add-to-list 'arglist sourcekit-lsp-path)))

(defface tree-sitter-hl-face:case-pattern
  '((t :inherit tree-sitter-hl-face:property))
  "Face for enum case names in a pattern match"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comment.special
  '((t :inherit tree-sitter-hl-face:comment
       :weight semi-bold))
  "Face for comments with some markup-like meaning, like MARK"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:operator.special
  '((t :inherit font-lock-negation-char-face
       :weight semi-bold))
  "Face for operators that need to stand out, like unary negation"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:punctuation.type
  '((t :inherit tree-sitter-hl-face:type
       :weight normal))
  "Face for punctuation in type names or annotations"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:annotation
  '((t :inherit font-lock-keyword-face))
  "Face for annotations or attributes attached to declarations."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:annotation.builtin
  '((t :inherit tree-sitter-hl-face:annotation))
  "Face for declaration annotations which are built in to the language."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:annotation.type
  '((t :inherit tree-sitter-hl-face:annotation))
  "Face for annotations attached to type descriptors."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.annotation
  '((t :inherit tree-sitter-hl-face:annotation.builtin))
  "Face for subelements of annotations which are built in to the language."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.compiler
  '((t :inherit tree-sitter-hl-face:keyword
       :weight semi-bold))
  "Face for compile-time keywords"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.type
  '((t :inherit tree-sitter-hl-face:keyword))
  "Face for keywords that appear in type annotations"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:variable.synthesized
  '((t :inherit tree-sitter-hl-face:variable))
  "Face for compiler-synthesized identifiers"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:default
  '((t :inherit default))
  "Face to override other faces, forcing the base display
attributes."
  :group 'tree-sitter-hl-faces)


(provide 'swift-lsp)
;;; swift-lsp.el ends here
