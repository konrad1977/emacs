;;; filer.el --- A small package for quickly searching for files using fzf. -*- lexical-binding: t -*-

;;; Code:
(require 'periphery-helper)
(require 'nerd-icons)

(defface filer-filename-face
  '((t (:foreground "white")))
  "Filename face."
  :group 'filer)

(defface filer-path-face
  '((t (:foreground "#888899")))
  "Filepath face."
  :group 'filer)

(defface filer-path-delimiter-face
  '((t (:foreground "#a0a0ae")))
  "Filepath-delimiter face."
  :group 'filer)

(defcustom filer-show-full-path nil
  "Show full path."
  :group 'filer
  :type '(boolean))

(cl-defun filer:parse (input)
  "Parse as (INPUT)."
  (setq list '())
  (dolist (line (split-string input "\n"))
    (if-let* ((entry line)
              (full-path (file-truename entry))
              (file-dir (file-name-directory entry))
              (file-name (file-name-nondirectory entry)))
        (if entry
            (push
             (list
              (if (not filer-show-full-path)
                (format "%s %s %s%s"
                        (nerd-icons-icon-for-file full-path)
                        (propertize file-name 'face 'filer-filename-face)
                        (propertize "/" 'face 'filer-path-delimiter-face)
                        (propertize (directory-file-name (file-name-directory file-dir)) 'face 'filer-path-face))
                (format "%s %s%s"
                        (nerd-icons-icon-for-file full-path)
                        (propertize file-dir 'face 'filer-path-face)
                        (propertize file-name 'face 'filer-filename-face)))
                full-path)
             list))))
(if-let ((file (filer:showmenu-with-title
                :title (format "~/%s Select file:"
                               (file-name-nondirectory
                                (directory-file-name
                                 (file-name-directory default-directory))))
                :list list)))
      (if (file-exists-p file)
          (find-file file)
        (message "File not found"))))

(defun add-right-padding-up-to (word max-length)
  "Add padding to (as WORD) if smaller then (as MAX-LENGTH)."
  (if (> (length word) max-length)
      (concat (substring word 0 max-length) "")
    (progn
      (setq copy word)
      (while (< (string-width copy) max-length)
        (setq copy (concat copy " "))))
    copy))

(defun filer:parse-line (line)
  "Parse (as LINE)."
  (save-match-data
    (and (string-match filer-rg-line-pattern line)
         (let* ((file (match-string 1 line)))
           (list (format "%s"
                         (propertize
                          file
                          'face 'periphery-filename-face))
                 fileWithLine)))))

;;;###autoload
(cl-defun filer:find-file ()
  "Run query (as QUERY)."
  (interactive)
  (let ((default-directory (periphery-helper:project-root-dir)))
    (async-start-command-to-string
     :command (format "rg --files | rg '' --color=never --no-heading --sortr=accessed --smart-case --no-heading --hidden --follow --max-columns 1000 --max-columns-preview")
     :callback '(lambda (output) (filer:parse output)))))

(cl-defun filer:showmenu-with-title (&key title &key list)
  "Build menu with (TITLE LIST)."
  (let* ((choices (seq-map (lambda (item) item) list))
         (choice (completing-read title choices)))
    (car (cdr (assoc choice choices)))))

(provide 'filer-cr)
;;; filer.el ends here '
