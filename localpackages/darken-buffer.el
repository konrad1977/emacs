;;; darken-buffer.el --- Darken active or lighten inactive buffer background -*- lexical-binding: t -*-

;; Author: Mikael Konradson
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: faces, convenience
;; URL: https://github.com/yourusername/darken-buffer

;;; Commentary:
;; This package provides functionality to either darken the current buffer's
;; background color or lighten inactive buffers relative to the theme's
;; background color. The effect follows the active window.

;;; Code:
(require 'face-remap)

(defgroup darken-buffer nil
  "Customize darken-buffer package."
  :group 'faces)

(defcustom darken-buffer-percentage 10
  "Percentage by which to darken the active buffer background."
  :type 'integer
  :group 'darken-buffer)

(defcustom lighten-inactive-buffer-percentage 0
  "Percentage by which to lighten inactive buffer backgrounds."
  :type 'integer
  :group 'darken-buffer)

(defcustom darken-buffer-always-darken-percentage 15
  "Percentage by which to darken buffers in always-darken lists."
  :type 'integer
  :group 'darken-buffer)

(defvar-local darken-buffer--cookie nil
  "Face remapping cookie for the current buffer.")

(defvar-local darken-buffer--fringe-cookie nil
  "Face remapping cookie for the fringe in current buffer.")

(defvar-local darken-buffer--linenumber-cookie nil
  "Face remapping cookie for the linenumber in current buffer.")

(defvar darken-buffer-apply-effect-hook nil
  "Hook run after darken-buffer effects are applied.")

(defvar darken-buffer-remove-effect-hook nil
  "Hook run after darken-buffer effects are removed.")

(defvar-local darken-buffer--ignore-cache nil
  "Cache for whether the current buffer should be ignored.")

(defvar darken-buffer--cached-bg-color nil
  "Cached background color of the current theme.")

(defcustom darken-buffer-ignore-modes '(treemacs-mode vterm-mode eshell-mode shell-mode term-mode)
  "List of major modes where effects should not be applied."
  :type '(repeat symbol)
  :group 'darken-buffer)

(defcustom darken-buffer-ignore-buffers '("Messages" "dashboard" "*compilation*")
  "List of buffer names where effects should not be applied."
  :type '(repeat string)
  :group 'darken-buffer)

(defcustom darken-buffer-ignore-buffers-regexp '("posframe"
                                                 "\\*posframe\*"
                                                 "messages"
                                                 "compilation"
                                                 "faces"
                                                ;; "\*.\*"
                                                 )
  "List of regular expressions matching buffer names to ignore."
  :type '(repeat regexp)
  :group 'darken-buffer)

(defcustom darken-buffer-always-darken-buffers '("*compilation*")
  "List of buffer names that should always be darkened when shown."
  :type '(repeat string)
  :group 'darken-buffer)

(defcustom darken-buffer-always-darken-buffers-regexp '("\*which-key-\*"
                                                        "\*Flycheck.+\*"
                                                        "\*Flymake.+\*")
  "List of regular expressions matching buffer names that should always be darkened."
  :type '(repeat regexp)
  :group 'darken-buffer)

(defun darken-buffer-should-ignore-p ()
  "Return t if current buffer should be ignored."
  (or (memq major-mode darken-buffer-ignore-modes)
      (member (buffer-name) darken-buffer-ignore-buffers)
      (cl-some (lambda (regexp)
                 (string-match-p regexp (buffer-name)))
               darken-buffer-ignore-buffers-regexp)))

(defun darken-buffer-should-always-darken-p ()
  "Return t if current buffer should always be darkened."
  (or (member (buffer-name) darken-buffer-always-darken-buffers)
      (cl-some (lambda (regexp)
                 (string-match-p regexp (buffer-name)))
               darken-buffer-always-darken-buffers-regexp)))

(defun darken-buffer-color-to-rgb (color)
  "Convert COLOR (hex or name) to RGB components."
  (let ((rgb (color-values color)))
    (if rgb
        (mapcar (lambda (x) (/ x 256)) rgb)
      (error "Invalid color: %s" color))))

(defun darken-buffer-rgb-to-hex (r g b)
  "Convert R G B components to hex color string."
  (format "#%02x%02x%02x" r g b))

(defun darken-buffer-darken-color (color percent)
  "Darken COLOR by PERCENT."
  (let* ((rgb (darken-buffer-color-to-rgb color))
         (darkened (mapcar (lambda (component)
                            (min 255
                                 (floor (* component (- 100 percent) 0.01))))
                          rgb)))
    (apply 'darken-buffer-rgb-to-hex darkened)))

(defun darken-buffer-lighten-color (color percent)
  "Lighten COLOR by PERCENT."
  (let* ((rgb (darken-buffer-color-to-rgb color))
         (lightened (mapcar (lambda (component)
                             (min 255
                                  (floor (+ component
                                          (* (- 255 component)
                                             (/ percent 100.0))))))
                           rgb)))
    (apply 'darken-buffer-rgb-to-hex lightened)))

(defun darken-buffer-adjust-color (color percent lighten)
  "Adjust COLOR by PERCENT. If LIGHTEN is non-nil, lighten the color; otherwise, darken it."
  (let* ((rgb (darken-buffer-color-to-rgb color))
         (adjusted (mapcar (lambda (component)
                             (if lighten
                                 (min 255
                                      (floor (+ component
                                                (* (- 255 component)
                                                   (/ percent 100.0)))))
                               (max 0
                                    (floor (* component (- 100 percent) 0.01)))))
                           rgb)))
    (apply 'darken-buffer-rgb-to-hex adjusted)))

(defun darken-buffer-get-background-color ()
  "Get the current theme's background color, using a cached value if available."
  (or darken-buffer--cached-bg-color
      (setq darken-buffer--cached-bg-color
            (or (face-background 'default) "#000000"))))

(defun darken-buffer-count-visible-non-ignored-windows ()
  "Count number of visible windows that aren't in ignore lists."
  (let ((count 0))
    (dolist (window (window-list))
      (with-selected-window window
        (unless (darken-buffer-should-ignore-p)
          (setq count (1+ count)))))
    count))

(defun darken-buffer-apply-effect (is-active)
  "Apply darkening or lightening effect based on whether window IS-ACTIVE."
  (when darken-buffer--cookie
    (face-remap-remove-relative darken-buffer--cookie))
  (when darken-buffer--fringe-cookie
    (face-remap-remove-relative darken-buffer--fringe-cookie))
  (when darken-buffer--linenumber-cookie
    (face-remap-remove-relative darken-buffer--linenumber-cookie))

  (let* ((bg-color (darken-buffer-get-background-color))
         (modified-bg (cond
                       ((darken-buffer-should-always-darken-p)
                        (darken-buffer-darken-color bg-color darken-buffer-always-darken-percentage))
                       (is-active
                        (if (> darken-buffer-percentage 0)
                            (darken-buffer-darken-color bg-color darken-buffer-percentage)
                          bg-color))
                       ((> lighten-inactive-buffer-percentage 0)
                        (darken-buffer-lighten-color bg-color lighten-inactive-buffer-percentage))
                       (t bg-color))))

    (unless (string= modified-bg bg-color)
      (setq darken-buffer--cookie
            (face-remap-add-relative 'default :background modified-bg))
      (setq darken-buffer--fringe-cookie
            (face-remap-add-relative 'fringe :background modified-bg))
      (setq darken-buffer--linenumber-cookie
            (face-remap-add-relative 'line-number :background modified-bg)))))

(defun darken-buffer-remove-effect ()
  "Remove all effects from current buffer."
  (when darken-buffer--cookie
    (face-remap-remove-relative darken-buffer--cookie)
    (setq darken-buffer--cookie nil))
  (when darken-buffer--fringe-cookie
    (face-remap-remove-relative darken-buffer--fringe-cookie)
    (setq darken-buffer--fringe-cookie nil))
  (when darken-buffer--linenumber-cookie
    (face-remap-remove-relative darken-buffer--linenumber-cookie)
    (setq darken-buffer--linenumber-cookie nil))

  (run-hooks 'darken-buffer-remove-effect-hook))

(defun darken-buffer-window-switch-hook ()
  "Handle window focus change."
  (when (bound-and-true-p darken-buffer-mode)
    (let ((current-window (selected-window)))
      ;; Apply effects to all windows
      (dolist (window (window-list))
        (with-selected-window window
          (if (darken-buffer-should-ignore-p)
              (darken-buffer-remove-effect)
            ;; Check if this is the selected window
            (if (eq window current-window)
                (darken-buffer-apply-effect t)  ; Active window - darken it
              (darken-buffer-apply-effect nil))))))))

(defun darken-buffer-setup-hooks ()
  "Set up hooks for darken-buffer."
  (add-hook 'window-selection-change-functions
            'darken-buffer--window-selection-change-function)
  (add-hook 'window-configuration-change-hook
            'darken-buffer-window-switch-hook)
  (add-hook 'post-command-hook 'darken-buffer-window-switch-hook)

  (add-hook 'after-load-theme-hook #'darken-buffer-invalidate-caches)
  (add-hook 'buffer-list-update-hook #'darken-buffer-invalidate-caches))

(defun darken-buffer-remove-hooks ()
  "Remove hooks for darken-buffer."
    (remove-hook 'window-selection-change-functions
                 'darken-buffer--window-selection-change-function)
    (remove-hook 'window-configuration-change-hook
                 'darken-buffer-window-switch-hook)
    (remove-hook 'post-command-hook 'darken-buffer-window-switch-hook)
    
    (remove-hook 'after-load-theme-hook #'darken-buffer-invalidate-caches)
    (remove-hook 'buffer-list-update-hook #'darken-buffer-invalidate-caches))

(defun darken-buffer--window-selection-change-function (_)
  "Function to handle window selection change."
  (run-with-timer 0 nil #'darken-buffer-window-switch-hook))

(defun darken-buffer-toggle-effect ()
  "Toggle darken-buffer effects for the current buffer."
  (interactive)
  (if darken-buffer--cookie
      (darken-buffer-remove-effect)
    (darken-buffer-apply-effect (eq (selected-window) (get-buffer-window)))))

(defun darken-buffer-set-darken-percentage (percentage)
  "Set the darken percentage interactively."
  (interactive "nEnter darken percentage: ")
  (setq darken-buffer-percentage percentage)
  (darken-buffer-window-switch-hook))

(defun darken-buffer-invalidate-caches ()
  "Invalidate all caches used by darken-buffer."
  (setq darken-buffer--cached-bg-color nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq darken-buffer--ignore-cache nil))))

;;;###autoload
(define-minor-mode darken-buffer-mode
  "Minor mode to darken active or lighten inactive buffer backgrounds."
  :lighter " Darker"
  :global t
  (if darken-buffer-mode
      (progn
        (darken-buffer-setup-hooks)
        ;; ;; Use both hooks to ensure we catch all window selection changes
        ;; Apply immediately
        (darken-buffer-window-switch-hook))
    (darken-buffer-remove-hooks)

    ;; Remove effects from all windows
    (dolist (window (window-list))
      (with-selected-window window
        (darken-buffer-remove-effect)))))

(provide 'darken-buffer)
;;; darken-buffer.el ends here
