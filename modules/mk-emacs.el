;;; mk-emacs.el --- General Emacs settings -*- lexical-binding: t; -*-
;;; Commentary:
;; General Emacs settings for improved usability and performance.
;;; Code:

;; Predefine variables to avoid void-variable errors during macro expansion
;; This is a workaround for Emacs 30.2.50 compatibility issues

(use-package emacs
  :hook (after-init . (lambda ()
                        (global-hl-line-mode 1)
                        (display-battery-mode 1)
                        (global-auto-revert-mode 1)))
  :custom
  (context-menu-mode t)
  (set-window-margins (selected-window) 5 5)
  (set-display-table-slot standard-display-table 0 ?\ )
  (column-number-mode nil)
  (line-number-mode nil)
  (delete-by-moving-to-trash t)
  (make-backup-files nil)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (visible-bell nil)
  (window-combination-resize t)
  (auto-revert-avoid-polling t) ;; Automatically reread from disk if the underlying file changes
  (auto-revert-check-vc-info t)
  (help-window-select t)
  (grep-command "rg -nS --no-heading ")
  (global-auto-revert-non-file-buf t)
  (find-file-visit-truename nil)
  (display-time-default-load-average nil) ; this information is useless for most
  (debug-on-error nil)
  ;; (debug-on-signal t)
  (cursor-in-non-selected-windows nil)
  (create-lockfiles nil)
  (confirm-kill-processes t)
  (completion-ignore-case t)
  (backward-delete-char-untabify-method 'hungry)
  (backup-by-copying t)
  (auto-save-no-message t)
  ;; (window-divider-mode t)
  (auto-save-timeout 20)
  (auto-window-vscroll nil)
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "var/auto-save/" user-emacs-directory) t)))
  (auto-save-list-file-prefix (expand-file-name "var/auto-save/.saves-" user-emacs-directory))
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  (setq confirm-kill-emacs (lambda (prompt)
                             (y-or-n-p-with-timeout prompt 2 nil)))
  (setopt history-length 300)
  (setopt indicate-buffer-boundaries nil)
  (setopt indicate-empty-lines nil)
  (setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
  (setopt kept-new-versions 6)
  (setopt kept-old-versions 2)
  (setopt display-line-numbers-type 'relative)
  (setopt kill-do-not-save-duplicates t)
  (setopt large-file-warning-threshold (* 15 1024 1024))
  (setopt line-move-visual nil)
  (setopt read-answer-short t)
  (setopt read-buffer-completion-ignore-case t)
  (setopt read-extended-command-predicate #'command-completion-default-include-p)
  (setopt recentf-auto-cleanup (if (daemonp) 300 'never))
  (setopt recentf-max-menu-items 15)
  (setopt compilation-skip-threshold 2)
  (setopt recentf-max-saved-items 300)
  (setopt redisplay-skip-fontification-on-input nil)
  (setopt ring-bell-function 'ignore)
  (setopt split-width-threshold 300)
  (setopt switch-to-buffer-obey-display-actions t)
  (setopt use-short-answers t)
  (setopt version-control t)
  (setopt warning-minimum-level :emergency)
  ;; (setopt window-divider-default-right-width 4
  ;;         window-divider-default-bottom-width 1)
  (setopt xref-search-program 'ripgrep)
  (setopt project-vc-ignores '(".git/" ".direnv/" "node_modules/" "dist/" ".*"))
  (setopt grep-find-ignored-directories
          '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  (setopt backup-directory-alist `(("." . "~/.saves")))
  (setopt custom-file (expand-file-name "var/custom.el" user-emacs-directory))
  (setopt delete-old-versions t)
  (setopt ad-redefinition-action 'accept)
  (when (eq system-type 'darwin)
    (setq
     mac-command-modifier 'meta
     mac-option-modifier 'none
     mac-command-key-is-meta t
     ns-use-proxy-icon nil
     ns-pop-up-frames nil
     ns-use-thin-smoothing t
     dired-use-ls-dired t
     insert-directory-program "/opt/homebrew/bin/gls"
     dired-listing-switches "-aBhl --group-directories-first"
     browse-url-browser-function #'mk/browser-split-window)))

(defun mk/safe-kill-buffer-and-window ()
  "Safely kill the current buffer and delete its window if possible.
- If the buffer is shown elsewhere, only delete this window.
- If it's a special buffer or the minibuffer, don't delete the window."
  (interactive)
  (let* ((buf (current-buffer))
         (win (selected-window)))
    (cond
     ;; Never kill the minibuffer
     ((minibufferp buf)
      (message "Cannot kill the minibuffer."))
     ;; Buffer shown elsewhere: just delete this window
     ((cl-remove win (get-buffer-window-list buf nil t))
      (delete-window win))
     ;; Buffer is special? Just kill buffer, keep window
     ((or (string-match-p "^\\*" (buffer-name buf))
          (not (window-live-p win)))
      (kill-buffer buf))
     ;; Normal case: kill buffer and delete window
     (t
      (when (kill-buffer buf)
        (when (window-live-p win)
          (delete-window win)))))))

(use-package window
  :ensure nil
  :bind (("C-x C-f" . #'toggle-frame-fullscreen)
         ("C-x C-w" . #'toggle-frame-maximized)
         ("C-x C-s" . #'window-toggle-side-windows)
         ("C-x C-x" . #'mk/safe-kill-buffer-and-window))
  :custom
  (transient-display-buffer-action
   '(display-buffer-below-selected
     (window-height . fit-window-to-buffer)))
  (window-resize-pixelwise nil)
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)  ;; (window-combination-resize t)
  (display-buffer-alist
   '(("\\*Async Shell Command\\*" (display-buffer-no-window))
     ("\\*xwidget\\*\\|\\*xref\\*"
      (display-buffer-in-side-window display-buffer-reuse-mode-window display-buffer-reuse-window)
      (body-function . select-window)
      (window-width . 0.4)
      (side . left))
     ("evil-marks\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
      (window-width . 0.10)
      (side . right)
      (slot . 0))
     ("\\*iOS Simulator\\|\\*swift package\\|\\*ios-device"
      (display-buffer-reuse-window display-buffer-in-side-window display-buffer-at-bottom)
      (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
      (window-parameters . ((mode-line-format . none)))
      (slot . 4))
     ("\\*Embark*"
      (display-buffer-in-side-window display-buffer-reuse-mode-window display-buffer-at-bottom)
      (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
      (window-parameters . ((select-window . t)))
      ;; (dedicated . t)
      (slot . 5))
     ("\\*Copilot Chat*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-width . 0.45)
      (side . right)
      (slot . 2)
      (window-parameters . ((mode-line-format . none))))
     ("\\*Periphery\\*\\|\\*compilation\\*"
      (display-buffer-reuse-window display-buffer-in-side-window display-buffer-at-bottom)
      (body-function . select-window)
      (window-height . 0.2)
      (window-width . 0.30)
      (slot . 1))
     ("\\*Android Emulator\\*\\|\\*Android Logcat\\*\\|\\*Android Emulator Error\\*"
      (display-buffer-reuse-window display-buffer-in-side-window display-buffer-at-bottom)
      (body-function . select-window)
      (window-height . 0.2)
      (window-width . 0.70)
      (slot . 2))
     ("\\*Faces\\|[Hh]elp\\*\\|\\*Copilot*\\|\\*Occur\\*"
      (display-buffer-in-side-window)
      (body-function . select-window)
      (window-width . 0.4)
      (side . right)
      (slot . 1))
     ("\\*e?shell\\|*ellama\\|\\*vterm\\*"
      (display-buffer-at-bottom display-buffer-reuse-window)
      (body-function . select-window)
      (window-height . 0.13)
      (window-parameters . ((mode-line-format . none)))
      (side . bottom)
      (slot . 10))
     ("\\*\\(Flycheck\\|Package-Lint\\).*"
      (display-buffer-reuse-window display-buffer-below-selected)
      (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
      (dedicated . t)
      ;; (preserve-size . (t . t))
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none)))))))

(use-package which-key
  :defer 3
  :ensure nil
  :hook (after-init . which-key-mode)
  :custom
  (which-key-separator " → ")
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 2) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display, because the default is only 1
  (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 45)
  (which-key-allow-imprecise-window-fit nil))

(defun mk/browser-split-window (url &optional new-window)
  "Create a new browser (as URL as NEW-WINDOW) window to the right of the current one."
  (interactive)
  (let ((ignore-window-parameters t)
        (dedicated-p (window-dedicated-p)))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (xwidget-webkit-browse-url url)))


(provide 'mk-emacs)
;;; mk-emacs.el ends here
