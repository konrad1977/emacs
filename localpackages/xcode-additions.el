;;; xcode-additions.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(defgroup xcode-additions:xcodebuild nil
  "REPL."
  :tag "xcode-additions:xcodebuild"
  :group 'xcode-additions)

(defconst xcodeproject-extension ".*\\.xcodeproj$"
  "Xcode project extensions.")

(defconst workspace-extension ".*\\.xcworkspace$"
  "Xcode workspace extensions.")

(defun xcode-additions:filename-by-extension (extension directory)
  "Get filename based on (as EXTENSION)."
  (if-let* ((name (directory-files directory t extension)))
      (file-name-sans-extension (file-name-nondirectory (car name)))))

(defun xcode-additions:project-directory-p (directory)
  "Check if xcodeproj file exists in (DIRECTORY)."
  (consp (directory-files directory nil xcodeproject-extension)))

(defun xcode-additions:workspace-directory-p (directory)
  "Check if xcodeproj file exists in (DIRECTORY)."
  (consp (directory-files directory nil workspace-extension)))

(defun xcode-additions:find-xcode-project-directory (&optional directory)
  "Try to find xcode project in (DIRECTORY)."
  (xcode-additions:find-ancestor-or-self-directory 'xcode-additions:project-directory-p directory))

(defun xcode-additions:find-workspace-directory (&optional directory)
  "Try to find xcode workspace in (DIRECTORY)."
  (xcode-additions:find-ancestor-or-self-directory 'xcode-additions:workspace-directory-p directory))

(defun xcode-additions:find-ancestor-or-self-directory (predicate &optional directory)
  ""
  (unless directory (setq directory default-directory))
  (if (funcall predicate directory)
      directory
    (let ((parent (file-name-directory (directory-file-name directory))))
      (if (or (null parent) (string-equal parent directory))
          nil
        (xcode-additions:find-ancestor-or-self-directory predicate parent)))))

(defun xcode-additions:workspace-name ()
  "Get the workspace name."
  (if-let* ((default-directory (xcode-additions:find-workspace-directory)))
      (xcode-additions:filename-by-extension workspace-extension default-directory)))

(defun xcode-additions:project-name ()
  "Get the workspace name."
  (if-let* ((default-directory (xcode-additions:find-xcode-project-directory)))
      (xcode-additions:filename-by-extension xcodeproject-extension default-directory)))

(defun xcode-additions:list-xcscheme-files (folder)
  "List the names of '.xcscheme' files in the xcshareddata/xcshemes subfolder of FOLDER."
  (let ((xcscheme-names '()))
    (setq folder (expand-file-name folder))
    (setq xcshemes-folder (concat folder "xcshareddata/xcschemes/"))
    (when (file-directory-p xcshemes-folder)
      (dolist (item (directory-files xcshemes-folder t))
        (when (and (file-regular-p item)
                   (string-match-p ".*\\.xcscheme$" item))
          (setq xcscheme-names (cons (file-name-sans-extension (file-name-nondirectory item)) xcscheme-names)))))
    (if xcscheme-names
        (setq xcscheme-names (nreverse xcscheme-names))
      (message "No '.xcscheme' files found in %s" xcshemes-folder))
    xcscheme-names))

(defun xcode-additions:list-scheme-files ()
  "List the names of '.xcscheme' files in the xcshareddata/xcshemes subfolder of the current Xcode project or workspace directory."
  (let* ((xcshemes '())
         (project-name (concat (xcode-additions:project-name) ".xcodeproj/"))
         (project-directory (concat (xcode-additions:find-xcode-project-directory) project-name)))
    (cond
     (project-directory
      (let ((xcscheme-files (xcode-additions:list-xcscheme-files project-directory)))
        (if xcscheme-files
            xcscheme-files
          nil))))))

(provide 'xcode-additions)
;;; xcode-additions.el ends here
