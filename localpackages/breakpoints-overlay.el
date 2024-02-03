;;; lldb-breakpoints-overlay.el --- ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Package for showing usage of function.

;;; Code:

(defvar breakpoints-overlay-mode nil
  "Mode for showing breakpoints overlay.")

(defvar-local breakpoints-overlay-list nil
  "List of overlays for breakpoints.")

(defgroup breakpoints-overlay nil
  "Plugin shows complexity information."
  :prefix "breakpoints-overlay"
  :group 'comm)

(defface breakpoints-overlay-face
  '((t :foreground "#FF5D62" :background "#43242B" :height 0.7 :bold t :box (:line-width 1 :color "#884444")))
  "Face added to code-usage display."
  :group 'breakpoints-overlay)

;;;###autoload
(define-minor-mode breakpoints-overlay-mode
  "Toggle 'breakpoints-overlay-mode'."
  :group overlay-usage
  :init-value nil
  :lighter "OverlayUsage"
  (if breakpoints-overlay-mode
      (breakpoints-overlay:enable)
    (breakpoints-overlay:disable)))

(defun breakpoints-overlay:enable ()
  "Enable overlay-usage."
  (add-hook 'after-save-hook #'breakpoints-overlay:update-all-buffer nil t)
  (add-hook 'after-revert-hook #'breakpoints-overlay:update-all-buffer nil t)
  (breakpoints-overlay:add-all-overlays))

(defun breakpoints-overlay:disable ()
  "Disable 'overlay-usage-mode'."
  (remove-hook 'after-save-hook #'breakpoints-overlay:remove-overlays t)
  (remove-hook 'after-revert-hook #'breakpoints-overlay:remove-overlays t)
  (breakpoints-overlay:remove-overlays))

(defun breakpoints-overlay:update-all-buffer ()
  "Update all buffers."
  (dolist (buf (buffer-list))
    (when (breakpoints-overlay:buffer-visible-p buf)
      (breakpoints-overlay:add-all-overlays))))

(defun breakpoints-overlay:buffer-visible-p (buffer)
  "Return non-nil if BUFFER is visible in any window."
  (not (eq (get-buffer-window buffer 'visible) nil)))

(defun breakpoints-overlay:add-overlays (breakpoints)
  "Update overlays from BREAKPOINTS."
  (breakpoints-overlay:remove-overlays)
  (dolist (item breakpoints)
    (if-let* ((path (car item))
              (linenumber (cdr item))
              (buffer (find-buffer-visiting path)))
        (breakpoints-overlay:add-overlay :buffer buffer :linenumber linenumber))))

(defun breakpoints-overlay:add-all-overlays ()
  "Add all overlays."
  (breakpoints-overlay:remove-overlays))

(defun breakpoints-overlay:remove-overlays ()
  "Clean up all overlays for functions."
  (interactive)
  (mapc #'delete-overlay breakpoints-overlay-list))

(defun breakpoints-overlay:add-overlay (&key buffer &key linenumber)
  "Add overlay (as POSITION and FILENAME) for variables."
  (with-current-buffer buffer
    (goto-line linenumber)
    (let* ((ov (make-overlay
                (line-end-position)
                (line-end-position))))
      (overlay-put ov 'after-string (concat " " (propertize (concat " ⚡️︎︎ " "BREAKPOINT ") 'face 'breakpoints-overlay-face)))
      (push ov breakpoints-overlay-list))))

(provide 'breakpoints-overlay)
;;; breakpoints-overlay.el ends here
