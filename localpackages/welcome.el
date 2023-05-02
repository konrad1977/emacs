;;; welcome.el --- Simple welcome screen -*- lexical-binding: t; -*-

;;; Commentary:
;; Welcome screen

;;; code:

(require 'recentf)

(defvar welcome-mode nil)

(defgroup welcome nil
  "Welcome group."
  :group 'applications)

(defconst welcome-buffer "*welcome*"
  "Welcome buffer name.")

(defvar welcome-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'welcome--open-recent-file)
    (define-key map (kbd "<return>") 'welcome--open-recent-file)
    (define-key map (kbd "o") 'welcome--open-recent-file)

    ;; Add shortcuts for file indexes
    (dolist (i (number-sequence 1 9))
      (define-key map (kbd (concat "C-" (number-to-string i)))
        `(lambda ()
           (interactive)
           (welcome--open-recent-file-at-index ,i))))
    (message "welcome-mode-map initialized")
    map)
  "Keymap for `welcome-mode'.")

(define-derived-mode welcome-mode fundamental-mode "Welcome"
  "Major mode for the welcome screen."
  :group 'welcome
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local display-line-numbers nil)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (use-local-map welcome-mode-map))

(defface welcome-title-face
  '((t :inherit link :height 1.1 :bold t))
  "Face added to code-usage display."
  :group 'welcome)

(defface welcome-info-face
  '((t :inherit font-lock-keyword-face :height 0.9 :bold nil))
  "Face added to code-usage display."
  :group 'welcome)

(defface welcome-package-info-face
  '((t :inherit font-lock-number-face :height 0.9 :bold nil :italic nil))
  "Face added to code-usage display."
  :group 'welcome)

(defface welcome-path-face
  '((t :foreground "#63677D" :height 0.9 :bold nil :italic nil))
  "Face for the file path."
  :group 'welcome)

(defface welcome-filename-face
  '((t :inherit default :height 0.9 :bold t :italic nil))
  "Face for the file name."
  :group 'welcome)

(defun welcome--insert-centered (text)
  "Insert TEXT at the center of the current line."
  (let ((width (window-width)))
    (insert (make-string (/ (- width (length text)) 2) ?\ ))
    (insert text)))

(defun welcome--open-recent-file ()
  "Open the recent file on the current line."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (prop-pos (next-single-property-change line-start 'path nil line-end)))
    (when prop-pos
      (let ((file (get-text-property prop-pos 'path)))
        (message file)
        (if (file-exists-p file)
            (find-file file)
          (error "File %s does not exist" file))))))

(defun welcome--open-recent-file-at-index (index)
  "Open the recent file at the given INDEX in the list."
  (interactive "nIndex: ")
  (let ((files (seq-take recentf-list 9)))
    (when (<= 1 index (length files))
      (find-file (nth (1- index) files)))))

(defun welcome--insert-recent-files ()
  "Insert the first 9 recent files with icons in the welcome buffer."
  (recentf-mode)
  (insert "\n")
  (let* ((files (seq-take recentf-list 9))
         (max-length (apply 'max (mapcar 'length recentf-list)))
         (left-margin (/ (- (window-width) max-length) 2)))
    (dolist (file files)
      (let* ((index (cl-position file files :test #'equal))
             (full-path (file-truename file))
             (shortcut (format "%d" (+ index +1)))
             (file-name (file-name-nondirectory file))
             (file-dir (file-name-directory file))
             (title (format "%s %s%s"
                    (propertize (all-the-icons-icon-for-file file :v-adjust -0.05) 'face '(:family "all-the-icons" :height 1.0))
                    (propertize file-dir 'face 'welcome-path-face)
                    (propertize file-name 'face 'welcome-filename-face)))
             (title-with-path (propertize title 'path full-path))
             (title-with-path-and-shortcut (concat title-with-path (propertize (format " [%s]" shortcut) 'face '(:height 0.9 :inherit font-lock-constant-face))))
        (right-margin (- (window-width) max-length left-margin)))
        (insert (format "%s%s%s\n" (make-string left-margin ?\s) title-with-path-and-shortcut (make-string right-margin ?\s)))))))

(defun welcome--insert-text (text)
  "Insert (as TEXT)."
  (let* ((max-length (apply 'max (mapcar 'length recentf-list)))
         (left-margin (/ (- (window-width) max-length) 2)))
    (insert (format "%s%s\n" (make-string left-margin ?\s) text ))))

;;;###autoload
(defun welcome-create-welcome-hook ()
  "Setup welcome screen."
  (when (< (length command-line-args) 2)
    (add-hook 'emacs-startup-hook (lambda ()
                                    (welcome--refresh-screen)
                                    ))))

(defun welcome--refresh-screen ()
  "Show the welcome screen."
  (with-current-buffer (get-buffer-create welcome-buffer)
    (let* ((buffer-read-only)
           (image-path "~/.emacs.d/themes/true.png")
           (image (create-image image-path 'png nil :width 200 :height 169))
           (size (image-size image))
           (width (car size))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (title (format "Startup time: %s" (emacs-init-time "%.2f seconds")))
           (packages (format "%d packages loaded" (length package-activated-list))))
      (erase-buffer)
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (insert "\n")
        (welcome--insert-text (propertize "Recent: [C-x to open file]" 'face 'welcome-title-face))
        (welcome--insert-recent-files)
        (setq cursor-type nil)
        (insert "\n")
        (welcome--insert-text (propertize title 'face 'welcome-info-face))
        (welcome--insert-text (propertize packages 'face 'welcome-package-info-face))
        (insert "\n\n\n")
        (insert (make-string left-margin ?\ ))
        (insert-image image)
        (switch-to-buffer (current-buffer))
        (read-only-mode +1)
        (welcome-mode)
        (goto-char (point-min))
        (forward-line 2)))))

(provide 'welcome)
;;; welcome.el ends here
