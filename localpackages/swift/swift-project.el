;;; swift-project.el --- Swift project management utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;; Helper function for setting up project root

;;; Code:

(require 'project)

(defgroup swift-project nil
  "Swift project management utilities."
  :group 'swift)

(defvar swift-project--current-root nil
  "Cached value of current project root.")

(defun swift-project-root (&optional dir no-error)
  "Find Swift project root starting from DIR (default: current directory).
When NO-ERROR is non-nil, return nil instead of erroring when no project found."
  (let ((found-root
         (or swift-project--current-root
             (let ((proj (project-current nil dir)))
               (when proj
                 (directory-file-name
                  (file-name-as-directory
                   (expand-file-name (project-root proj)))))))))
    (cond
     (found-root
      (setq swift-project--current-root found-root)
      found-root)
     (no-error nil)
     (t (error "No Swift project found")))))

(defun swift-project-reset-root ()
  "Clear cached project root."
  (setq swift-project--current-root nil))

(provide 'swift-project)
;;; swift-project.el ends here
