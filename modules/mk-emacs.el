;;; mk-emacs.el --- General Emacs settings -*- lexical-binding: t; -*-
;;; Commentary:
;; General Emacs settings for improved usability and performance.
;;; Code:

;; Predefine variables to avoid void-variable errors during macro expansion
;; This is a workaround for Emacs 30.2.50 compatibility issues

(use-package emacs
  :hook (after-init . (lambda ()
                        (global-hl-line-mode 0)
                        (display-battery-mode 1)
                        (global-auto-revert-mode 1)))
  :custom
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "var/auto-save/" user-emacs-directory) t)))
  (auto-save-list-file-prefix (expand-file-name "var/auto-save/.saves-" user-emacs-directory))
  (auto-save-no-message t)
  (auto-save-timeout 20)
  (auto-window-vscroll nil)
  (backup-by-copying t)
  (backward-delete-char-untabify-method 'hungry)
  (column-number-mode nil)
  (completion-ignore-case t)
  (confirm-kill-processes t)
  (context-menu-mode t)
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  ;; (debug-on-error nil)
  (delete-by-moving-to-trash t)
  (display-time-default-load-average nil) ; this information is useless for most
  (find-file-visit-truename nil)
  (global-auto-revert-non-file-buf t)
  (grep-command "rg -nS --no-heading ")
  (help-window-select t)
  ;; (indicate-buffer-boundaries 'right) ; Show buffer top and bottom in the margin
  (line-number-mode nil)
  (make-backup-files nil)
  (read-buffer-completion-ignore-case t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (read-file-name-completion-ignore-case t)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (visible-bell nil)
  (window-combination-resize t)
  (window-sides-vertical t)
  ;; (debug-on-signal t)
  :config
  (set-display-table-slot standard-display-table 0 ?\ )
  (set-window-fringes (selected-window) 16 10)
  (set-window-margins (selected-window) 10 10)
  (setq confirm-kill-emacs (lambda (prompt)
                             (y-or-n-p-with-timeout prompt 2 nil)))
  (setopt history-length 300)
  (setopt auto-revert-interval 5) ;; Check for changes every secondhh
  (setopt auto-revert-avoid-polling t) ;; Automatically reread from disk if the underlying file changes
  (setopt auto-revert-check-vc-info t)
  (setopt indicate-empty-lines nil)
  (setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
  (setopt kept-new-versions 6)
  (setopt kept-old-versions 2)
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
     ;; mac-command-key-is-meta t
     ;; ns-use-proxy-icon nil
     ;; ns-pop-up-frames nil
     ;; ns-use-thin-smoothing t
     dired-use-ls-dired t
     insert-directory-program "/opt/homebrew/bin/gls"
     dired-listing-switches "-aBhl --group-directories-first"
     browse-url-browser-function #'mk/browser-split-window)))

;;;###autoload
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

(defun mk/browser-split-window (url &optional new-window)
  "Create a new browser (as URL as NEW-WINDOW) window to the right of the current one."
  (interactive)
  (let ((ignore-window-parameters t)
        (dedicated-p (window-dedicated-p)))
    (delete-other-windows)
    (split-window-horizontally)
    (other-window 1)
    (xwidget-webkit-browse-url url)))


;; (use-package savehist
;;   :ensure nil
;;   :config
;;   (savehist-mode 1))
;;
(provide 'mk-emacs)
;;; mk-emacs.el ends here
