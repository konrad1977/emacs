;;; flycheck-overlay.el --- Display Flycheck errors with overlays -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Your Name <your.email@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (flycheck "0.23"))
;; Keywords: convenience, tools
;; URL: https://github.com/yourusername/flycheck-overlay

;;; Commentary:

;;; Code:

(define-advice flycheck-overlays-in (:override (_ _) disable-sorting)
  "Temporarily disable overlay sorting to debug issues."
  nil)

(require 'flycheck)
(require 'cl-lib)


(defgroup flycheck-overlay nil
  "Display Flycheck errors using overlays."
  :prefix "flycheck-overlay-"
  :group 'flycheck)

(defvar-local flycheck-overlay--overlays nil
  "List of overlays used in the current buffer.")

(defface flycheck-overlay-error
  '((t :background "#453246"
       :foreground "#ea8faa"
       :height 0.8
       :weight normal))
  "Face used for error overlays."
  :group 'flycheck-overlay)

(defface flycheck-overlay-warning
  '((t :background "#331100"
       :foreground "#DCA561"
       :height 0.8
       :weight normal))
  "Face used for warning overlays."
  :group 'flycheck-overlay)

(defface flycheck-overlay-info
  '((t :background "#374243"
       :foreground "#a8e3a9"
       :height 0.8
       :weight normal))
  "Face used for info overlays."
  :group 'flycheck-overlay)

(defface flycheck-overlay-marker
  '((t :inherit font-lock-number-face
       :height 0.8
       :weight semi-bold))
  "Face used for info overlays."
  :group 'flycheck-overlay)

(defun flycheck-overlay--sort-errors (errors)
  "Safely sort ERRORS by their buffer positions."
  (condition-case nil
      (seq-filter
       (lambda (err)
         (and (flycheck-error-p err)
              (flycheck-error-line err)
              (or (not (flycheck-error-column err))
                  (numberp (flycheck-error-column err)))))
       errors)
    (error errors)))

(defun flycheck-overlay--get-safe-position (line column)
  "Get a safe buffer position for LINE and COLUMN.
LINE and COLUMN are 1-based positions in the buffer.
Returns a buffer position that is guaranteed to be within bounds."
  (save-restriction
    (widen)
    (save-excursion
      (condition-case nil
          (progn
            (goto-char (point-min))
            (when (and line (numberp line) (> line 0))
              (forward-line (1- line)))
            (when (and column (numberp column) (> column 0))
              (forward-char (min (1- column)
                                 (- (line-end-position) (point)))))
            (point))
        (error (point-min))))))

(defun flycheck-overlay--get-error-region (err)
  "Get the start and end position for ERR.
ERR is a Flycheck error object. Returns a cons cell (START . END) representing the region."
  (condition-case region-err
      (progn
        (unless (flycheck-error-p err)
          (message "Debug region: Not a valid error object: %S" err)
          (signal 'wrong-type-argument `(flycheck-error-p ,err)))
        (let* ((line (flycheck-error-line err))
               (column (flycheck-error-column err))
               (start-pos (flycheck-overlay--get-safe-position line column)))
          (message "Debug region: line=%S column=%S start-pos=%S" line column start-pos)
          (when start-pos  ; Only proceed if we got a valid position
            (save-excursion
              (goto-char start-pos)
              (let ((end-pos (line-end-position)))
                (when (and (integer-or-marker-p end-pos)
                           (<= end-pos (point-max)))
                  (cons start-pos end-pos)))))))
    (error
     (message "Debug region: Error getting region: %S" region-err)
     nil)))

(defun flycheck-overlay--create-overlay (region type msg)
  "Create an overlay at REGION of TYPE with message MSG.
REGION should be a cons cell (BEG . END) of buffer positions."
  (condition-case ov-err
      (when (and region (consp region)
                 (integer-or-marker-p (car region))
                 (integer-or-marker-p (cdr region)))
        (let* ((beg (max (point-min) (car region)))
               (end (min (point-max) (cdr region)))
               (face (pcase type
                      ('error 'flycheck-overlay-error)
                      ('warning 'flycheck-overlay-warning)
                      ('info 'flycheck-overlay-info)
                      (_ 'flycheck-overlay-warning))))
          (message "Debug overlay: beg=%S end=%S" beg end)
          (when (and (<= beg end) (integer-or-marker-p beg) (integer-or-marker-p end))
            (let ((ov (make-overlay beg end nil t nil)))
              (overlay-put ov 'flycheck-overlay t)
              (overlay-put ov 'evaporate t)
              (let* ((col-pos (save-excursion
                                (goto-char beg)
                                (current-column)))
                     (indicator (pcase type
                                ('error "")
                                ('warning "")
                                ('info "")
                                (_ "")))
                     (existing-bg (face-background face nil t))
                     (display-msg (concat " " indicator "  " msg " "))
                     (display-string
                      (concat
                       "\n"
                       (make-string col-pos ?\s)
                       (propertize display-msg 'face face 'cursor-intangible t 'rear-nonsticky t))))
                (overlay-put ov 'after-string (flycheck-overlay--mark-all-symbols
                                               :input display-string
                                               :regex "\\('.*'\\)"
                                               :property `(:inherit flycheck-overlay-marker
                                                           :background ,existing-bg)))
                (overlay-put ov 'help-echo msg))
              (overlay-put ov 'priority 2000)
              ov))))
    (error
     (message "Error creating overlay: %S" ov-err)
     nil)))

(defun replace-curly-quotes (text)
  "Replace curly quotes with straight quotes in TEXT."
  (replace-regexp-in-string "[“”]" "\""
    (replace-regexp-in-string "[‘’]" "'" text)))

(cl-defun flycheck-overlay--mark-all-symbols (&key input regex property)
  "Highlight all symbols matching REGEX in INPUT with specified PROPERTY."
  (save-match-data
    (setq input (replace-curly-quotes input))  ; Replace curly quotes with straight quotes
    (let ((pos 0))
      (while (string-match regex input pos)
        (let* ((start (match-beginning 1))
               (end (match-end 1))
               (existing-face (text-properties-at start input))
               (new-face (append existing-face (list 'face property))))
          (add-text-properties start end new-face input)
          (setq pos end))))
    input))

(defun flycheck-overlay--clean-message (msg)
  "Remove all text up to and including the first ':' in MSG."
  (if (string-match ":\\(.*\\)" msg)
      (match-string 1 msg)
    msg))

(defun flycheck-overlay--display-errors (&optional errors)
  "Display ERRORS using overlays."
  (condition-case display-err
      (let ((errs (flycheck-overlay--sort-errors (or errors flycheck-current-errors))))
        ;; (message "Debug display-errors: starting with %d errors" (length errs))
        (when (listp errs)
          (flycheck-overlay--clear-overlays)
          (dolist (err errs)
            (condition-case err-handler
                (let* ((level (flycheck-error-level err))
                       (msg (flycheck-overlay--clean-message (flycheck-error-message err)))
                       ;; (_ (message "Debug: Processing error level=%S msg=%S" level msg))
                       (region (flycheck-overlay--get-error-region err)))
                  (when (and region (car region) (cdr region) msg)
                    (let ((overlay (flycheck-overlay--create-overlay region level msg)))
                      (when overlay
                        (push overlay flycheck-overlay--overlays)))))
              (error
               (message "Debug: Error handling individual error: %S" err-handler))))))
    (error
     (message "Debug: Top-level display error: %S" display-err))))


(defun flycheck-overlay--clear-overlays ()
  "Remove all flycheck overlays from the current buffer."
  (dolist (ov flycheck-overlay--overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
  (setq flycheck-overlay--overlays nil)
  (flycheck-delete-all-overlays))

;;;###autoload
(define-minor-mode flycheck-overlay-mode
  "minor mode for displaying flycheck errors using overlays."
  :lighter " fo"
  :group 'flycheck-overlay
  (if flycheck-overlay-mode
      (progn
        (add-hook 'flycheck-after-syntax-check-hook
                  #'flycheck-overlay--maybe-display-errors nil t)
        (add-hook 'after-change-functions
                  #'flycheck-overlay--handle-buffer-changes nil t)
        (when flycheck-current-errors
          (flycheck-overlay--maybe-display-errors)))
    (remove-hook 'flycheck-after-syntax-check-hook
                 #'flycheck-overlay--maybe-display-errors t)
    (remove-hook 'after-change-functions
                 #'flycheck-overlay--handle-buffer-changes t)
    (flycheck-overlay--clear-overlays)))

(defun flycheck-overlay--maybe-display-errors ()
  "Display errors only if buffer is not being modified."
  (unless (buffer-modified-p)
    (flycheck-overlay--display-errors)))

(defun flycheck-overlay--handle-buffer-changes (&rest _)
  "Handle buffer modifications by clearing overlays on the current line while editing."
  (when (buffer-modified-p)
    (let ((current-line (line-number-at-pos)))
      (dolist (ov flycheck-overlay--overlays)
        (when (and (overlayp ov)
                   (let ((ov-line (line-number-at-pos (overlay-start ov))))
                     (= ov-line current-line)))
          (delete-overlay ov)))
      (setq flycheck-overlay--overlays
            (cl-remove-if-not #'overlay-buffer flycheck-overlay--overlays)))))

(provide 'flycheck-overlay)
;;; flycheck-overlay.el ends here
