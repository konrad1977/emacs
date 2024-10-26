;;; filer.el --- A small package for quickly searching for files using ripgrep -*- lexical-binding: t -*-

;;; Commentary:
;; This package provides functionality for quickly searching and opening files
;; using ripgrep (rg) and Emacs' built-in completion system.

;;; Code:

(require 'periphery-helper)
(require 'nerd-icons)

(defgroup filer nil
  "Customization group for filer package."
  :group 'files)

(defface filer-filename-face
  '((t (:foreground "white")))
  "Face for filenames in filer results."
  :group 'filer)

(defface filer-path-face
  '((t (:foreground "#888899")))
  "Face for file paths in filer results."
  :group 'filer)

(defface filer-project-name-face
  '((t (:foreground "#555565")))
  "Face for file paths in filer results."
  :group 'filer)

(defface filer-path-delimiter-face
  '((t (:foreground "#BBBBCC")))
  "Face for file paths in filer results."
  :group 'filer)

(defcustom filer-show-full-path nil
  "When non-nil, show full file paths in results."
  :group 'filer
  :type 'boolean)

(defcustom filer-max-results 500
  "Maximum number of results to process."
  :group 'filer
  :type 'integer)

(defun filer-format-candidate (file project-root)
  "Format FILE as a candidate for completion, relative to PROJECT-ROOT."
  (let* ((full-path (expand-file-name file))
         (relative-path (file-relative-name full-path project-root))
         (file-name (file-name-nondirectory relative-path))
         (file-dir (file-name-directory relative-path))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (icon (nerd-icons-icon-for-file full-path)))
    (cons
     (if filer-show-full-path
         (format "%s %s"
                 icon
                 (propertize relative-path 'face 'filer-filename-face))
       (format "%s %s %s%s"
               icon
               (propertize file-name 'face 'filer-filename-face)
               (propertize project-name 'face 'filer-project-name-face)
               (if file-dir
                   (concat (propertize "/" 'face 'filer-path-delimiter-face)
                           (propertize (directory-file-name file-dir) 'face 'filer-path-face))
                 "")))
     full-path)))

(defun filer-find-file ()
  "Interactively find and open a file using ripgrep and completion."
  (interactive)
  (let* ((project-root (periphery-helper:project-root-dir))
         (default-directory project-root)
         (command "rg --files --color=never --no-heading --sortr=accessed --smart-case --follow")
         (process (start-process-shell-command "filer-rg" nil command))
         (candidates '())
         (candidate-count 0)
         (selection nil)
         (done nil))
    (set-process-filter
     process
     (lambda (_process output)
       (dolist (file (split-string output "\n" t))
         (when (and (< candidate-count filer-max-results)
                    (file-regular-p (expand-file-name file project-root)))
           (push (filer-format-candidate file project-root) candidates)
           (setq candidate-count (1+ candidate-count))))))
    (set-process-sentinel
     process
     (lambda (process _event)
       (when (eq (process-status process) 'exit)
         (setq done t))))
    (while (not done)
      (accept-process-output process 0.1))
    (setq candidates (nreverse candidates))
    (setq selection (completing-read "Select file: " candidates nil t))
    (when selection
      (let ((file (cdr (assoc selection candidates))))
        (if file
            (if (file-exists-p file)
                (find-file file)
              (message "File not found: %s" file))
          (message "No matching file found for selection: %s" selection))))))

(provide 'filer)
;;; filer.el ends here
