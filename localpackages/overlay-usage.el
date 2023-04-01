;;; overlay-usage.el --- ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Package for showing usage of functions. 

;;; Code:

(require 'rect)
(require 'project)

(defvar overlay-usage-mode nil)

(defvar-local overlays-list nil
  "List of overlays.")

(defgroup overlay-usage nil
  "Plugin shows complexity information."
  :prefix "overlay-usage-"
  :group 'comm)

(defface overlay-usage-default-face
  '((t :height 0.7 :foreground "#999999"))
  "Face added to code-usage display."
  :group 'overlay-usage)

(define-minor-mode overlay-usage-mode
  "Toggle overlay-usage-mode."
  :group overlay-usage
  :init-value nil
  :lighter "OverlayUsage"
  (if overlay-usage-mode
      (overlay-usage-enable)
    (overlay-usage-disable)))


(defun project-root-dir ()
  "Get the root directory of the current project."
  (let ((project (project-current)))
    (when project
      (project-root project))))

(defun overlay-usage-enable ()
  "Enable overlay-usage."
  (add-hook 'after-save-hook #'overlay-add-to-functions nil t)
  (overlay-add-to-functions))

(defun overlay-usage-disable ()
  "Disable overlay-usage-mode."
  (remove-hook 'after-save-hook #'overlay-add-to-functions t)
  (overlay-usage-remove-overlays))

(defun overlay-usage-remove-overlays ()
  "Clean up all overlays."
  (mapc #'delete-overlay overlays-list))

(defun add-overlay (position spaces extension)
  "Add overlay (as POSITION with SPACES and search EXTENSION)."
  (save-excursion
    (goto-char position)
    (let* ((function-name (thing-at-point 'symbol))
           (extension extension)
           (default-directory (project-root-dir))
           (count (string-to-number
                   (shell-command-to-string
                    (format "rg --glob '%s' -e '%s\\(' | wc -l" extension function-name)))))

      (let ((ov (make-overlay (line-beginning-position 0)
                              (line-end-position 0))))
        (overlay-put ov 'before-string "\n")
        (overlay-put ov 'after-string
                     (concat spaces 
                     (concat (propertize "⚡︎ " 'face '(:height 0.6))
                             (propertize (concat "Found " (number-to-string (if (> count 0)
                                                                                (- count 1) 0)) " references")
                                         'face 'overlay-usage-default-face))))
        (overlay-put ov 'overlay-usage t)
        (push ov overlays-list) ov))))

(defun extension-from-file ()
  (file-name-extension buffer-file-name))

(defun regex-for-file-type (extension)
  "Detect what the function start with."
  (cond
   ((string-match-p (regexp-quote "swift") extension) "func")
   ((string-match-p (regexp-quote "kt") extension) "fun")
   ((string-match-p (regexp-quote "el") extension) "(defun")))

(defun overlay-add-to-functions ()
  "Adds overlay to functions."
  (overlay-usage-remove-overlays)
  (goto-char (point-min))
  (let*  ((extension (extension-from-file))
          (func-regex (regex-for-file-type extension)))
    (while (search-forward-regexp  (concat func-regex " \\([a-zA-Z0-9_-\(]+\\)") nil t)
      (let* ((position (match-beginning 1))
             (column (save-excursion (back-to-indentation) (current-column))))
        (add-overlay position (spaces-string column) (format "*.%s" extension))))))

(provide 'overlay-usage)

;;; overlay-usage.el ends here
