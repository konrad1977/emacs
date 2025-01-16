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
  '((t (:inherit default :foreground nil)))
  "Face for filenames in filer results."
  :group 'filer)

(defface filer-path-face
  '((t (:inherit font-lock-comment-face)))
  "Face for file paths in filer results."
  :group 'filer)

(defface filer-project-name-face
  '((t (:inherit font-lock-number-face)))
  "Face for project names in filer results."
  :group 'filer)

(defface filer-path-delimiter-face
  '((t (:inherit font-lock-delimiter-face)))
  "Face for path delimiters in filer results."
  :group 'filer)

(defcustom filer-show-full-path nil
  "When non-nil, show full file paths in results."
  :group 'filer
  :type 'boolean)

(defcustom filer-include-project-name t
  "When non-nil, include project name in results."
  :group 'filer
  :type 'boolean)

(defcustom filer-max-results 1000
  "Maximum number of results to process."
  :group 'filer
  :type 'integer)

(defcustom filer-path-position 'right
  "Position of the file path relative to the filename.
Can be either 'left or 'right."
  :group 'filer
  :type '(choice (const :tag "Right side" right)
                (const :tag "Left side" left)))

(defun filer-format-candidate (file project-root)
  "Format FILE as a candidate for completion, relative to PROJECT-ROOT."
  (let* ((full-path (expand-file-name file))
         (relative-path (file-relative-name full-path project-root))
         (file-name (file-name-nondirectory relative-path))
         (file-dir (file-name-directory relative-path))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (icon (nerd-icons-icon-for-file full-path))
         (path-part
          (concat
           (if filer-include-project-name
               (propertize (or project-name "") 'face 'filer-project-name-face)
             "")
           (if (and file-dir (not (string= file-dir "")))
               (concat (propertize "/" 'face 'filer-path-delimiter-face)
                      (propertize (directory-file-name file-dir)
                                'face 'filer-path-face))
             ""))))
    (cons
     (if filer-show-full-path
         (format "%s %s"
                 icon
                 (propertize relative-path 'face 'filer-filename-face))
       (pcase filer-path-position
         ('right (format "%s %s %s"
                        icon
                        (propertize file-name 'face 'filer-filename-face)
                        path-part))
         ('left (format "%s %s %s"
                       icon
                       path-part
                       (propertize file-name 'face 'filer-filename-face)))))
     full-path)))

(defun filer-find-file ()
  "Interactively find and open a file using ripgrep and completion."
  (interactive)
  (let* ((project-root (periphery-helper:project-root-dir))
         (default-directory project-root)
         (command "rg --files --color=never --no-heading --sortr=accessed --follow --smart-case")
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
