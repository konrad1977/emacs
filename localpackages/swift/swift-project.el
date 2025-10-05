;;; swift-project.el --- Swift project management utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;; Helper function for setting up project root

;;; Code:

(require 'project)
(require 'cl-lib)

(defgroup swift-project nil
  "Swift project management utilities."
  :group 'swift)

(defcustom swift-project-debug nil
  "Enable debug messages for Swift project detection."
  :type 'boolean
  :group 'swift-project)

(defcustom swift-project-ignore-folders
  '("Pods" "Packages" ".build" "Carthage" "DerivedData" ".swiftpm" "node_modules")
  "Folders to ignore when searching for project root."
  :type '(repeat string)
  :group 'swift-project)

(defcustom swift-project-marker-files
  '(".projectile" ".project")
  "Files that mark the project root (highest priority)."
  :type '(repeat string)
  :group 'swift-project)

(defvar swift-project--current-root nil
  "Cached value of current project root.")

(defvar swift-project--root-cache (make-hash-table :test 'equal)
  "Hash table to cache project roots by directory path.")

(defun swift-project-root (&optional dir no-error)
  "Find Swift project root starting from DIR (default: current directory).
Looks for .projectile or .project files to determine the project root.
When NO-ERROR is non-nil, return nil instead of erroring when no project found."
  (let* ((search-dir (expand-file-name (or dir default-directory)))
         (cached-root (gethash search-dir swift-project--root-cache))
         (found-root (or cached-root
                        (swift-project--find-project-root search-dir))))
    (cond
     (found-root
      ;; Cache the result for this directory
      (unless cached-root
        (puthash search-dir found-root swift-project--root-cache))
      ;; Update the global current root if this is the most recent lookup
      (setq swift-project--current-root found-root)
      found-root)
     (no-error nil)
     (t (error "No Swift project found starting from %s" search-dir)))))

;;;###autoload
(defun swift-project-clear-cache ()
  "Clear the project root cache."
  (interactive)
  (clrhash swift-project--root-cache)
  (setq swift-project--current-root nil)
  (message "Swift project root cache cleared"))

;;;###autoload
(defun swift-project-setup-working-directory ()
  "Ensure working directory is set to project root when opening Swift files.
This prevents .compile and buildServer.json from being created in wrong locations."
  (when (and buffer-file-name
             (string-match-p "\\.swift$" buffer-file-name))
    (let ((project-root (swift-project-root (file-name-directory buffer-file-name) t)))
      (when (and project-root 
                 (not (string= (expand-file-name default-directory) 
                              (expand-file-name project-root))))
        (setq default-directory project-root)
        (when (bound-and-true-p swift-project-debug)
          (message "Swift project: Set working directory to %s" project-root))
        
        ))))

;; Automatically setup working directory when opening Swift files
;;;###autoload
(add-hook 'swift-mode-hook #'swift-project-setup-working-directory)

(defun swift-project--should-ignore-folder-p (dir)
  "Return t if DIR should be ignored when searching for project root."
  (let ((basename (file-name-nondirectory (directory-file-name dir))))
    (member basename swift-project-ignore-folders)))

(defun swift-project--check-marker-files (parent)
  "Check if PARENT directory contains any marker files."
  (cl-some (lambda (marker)
             (file-exists-p (expand-file-name marker parent)))
           swift-project-marker-files))

(defun swift-project--find-project-root (dir)
  "Find project root using the following priority order from DIR:
1. Marker files from swift-project-marker-files (highest priority)
2. .xcworkspace directory
3. .xcodeproj directory  
4. Emacs project.el root (vc-root or project-root)

Ignores folders listed in swift-project-ignore-folders."
  ;; Priority 1: Look for marker files (configurable)
  (let ((marker-root (locate-dominating-file 
                      dir 
                      (lambda (parent)
                        (and (not (swift-project--should-ignore-folder-p parent))
                             (swift-project--check-marker-files parent))))))
    (if marker-root
        (directory-file-name (file-name-as-directory (expand-file-name marker-root)))
      ;; Priority 2: Look for .xcworkspace
      (let ((workspace-root (locate-dominating-file 
                            dir
                            (lambda (parent)
                              (and (not (swift-project--should-ignore-folder-p parent))
                                   (directory-files parent nil "\\.xcworkspace$" t 1))))))
        (if workspace-root
            (directory-file-name (file-name-as-directory (expand-file-name workspace-root)))
          ;; Priority 3: Look for .xcodeproj  
          (let ((xcodeproj-root (locate-dominating-file
                                dir
                                (lambda (parent)
                                  (and (not (swift-project--should-ignore-folder-p parent))
                                       (directory-files parent nil "\\.xcodeproj$" t 1))))))
            (if xcodeproj-root
                (directory-file-name (file-name-as-directory (expand-file-name xcodeproj-root)))
              ;; Priority 4: Use Emacs project.el
              (when (fboundp 'project-current)
                (let ((proj (project-current nil dir)))
                  (when proj
                    (let ((root (if (fboundp 'project-root)
                                   (project-root proj)  ; Emacs 29+
                                 (car (project-roots proj))))) ; Emacs 28
                      (when (and root (not (swift-project--should-ignore-folder-p root)))
                        (directory-file-name (file-name-as-directory (expand-file-name root)))))))))))))))

;;;###autoload
(defun swift-project-debug-root-detection ()
  "Debug the project root detection process."
  (interactive)
  (let ((dir (or default-directory (read-directory-name "Check from directory: "))))
    (message "=== Swift Project Root Detection Debug ===")
    (message "Starting from: %s" dir)
    (message "Ignore folders: %s" swift-project-ignore-folders)
    (message "Marker files: %s" swift-project-marker-files)
    (message "")
    
    ;; Check each priority level
    (let ((marker-root (locate-dominating-file 
                       dir 
                       (lambda (parent)
                         (and (not (swift-project--should-ignore-folder-p parent))
                              (swift-project--check-marker-files parent))))))
      (if marker-root
          (message "✓ Priority 1: Found marker files %s at: %s" swift-project-marker-files marker-root)
        (message "✗ Priority 1: No marker files found (%s)" swift-project-marker-files)))
    
    (let ((workspace-root (locate-dominating-file 
                          dir
                          (lambda (parent)
                            (and (not (swift-project--should-ignore-folder-p parent))
                                 (directory-files parent nil "\\.xcworkspace$" t 1))))))
      (if workspace-root
          (message "✓ Priority 2: Found .xcworkspace at: %s" workspace-root)
        (message "✗ Priority 2: No .xcworkspace found")))
    
    (let ((xcodeproj-root (locate-dominating-file
                          dir
                          (lambda (parent)
                            (and (not (swift-project--should-ignore-folder-p parent))
                                 (directory-files parent nil "\\.xcodeproj$" t 1))))))
      (if xcodeproj-root
          (message "✓ Priority 3: Found .xcodeproj at: %s" xcodeproj-root)
        (message "✗ Priority 3: No .xcodeproj found")))
    
    (when (fboundp 'project-current)
      (let ((proj (project-current nil dir)))
        (if proj
            (message "✓ Priority 4: Emacs project.el root: %s"
                    (if (fboundp 'project-root)
                        (project-root proj)
                      (car (project-roots proj))))
          (message "✗ Priority 4: No Emacs project found"))))
    
    (message "")
    (message "Final detected root: %s" (swift-project--find-project-root dir))
    (message "=== End Debug ==")))

(defun swift-project-reset-root ()
  "Clear cached project root."
  (setq swift-project--current-root nil))

(provide 'swift-project)
;;; swift-project.el ends here