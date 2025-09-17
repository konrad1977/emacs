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
Looks for .projectile or .project files to determine the project root.
When NO-ERROR is non-nil, return nil instead of erroring when no project found."
  (let ((found-root
         (or swift-project--current-root
             (swift-project--find-project-root (or dir default-directory)))))
    (cond
     (found-root
      (setq swift-project--current-root found-root)
      found-root)
     (no-error nil)
     (t (error "No Swift project found")))))

(defun swift-project--find-project-root (dir)
  "Find project root by looking for .projectile or .project files starting from DIR."
  (let ((root (locate-dominating-file dir (lambda (parent)
                                            (or (file-exists-p (expand-file-name ".projectile" parent))
                                                (file-exists-p (expand-file-name ".project" parent)))))))
    (when root
      (directory-file-name (file-name-as-directory (expand-file-name root))))))

(defun swift-project-reset-root ()
  "Clear cached project root."
  (setq swift-project--current-root nil))

(provide 'swift-project)
;;; swift-project.el ends here
