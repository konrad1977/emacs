;;; mk-ai.el --- AI integrations -*- lexical-binding: t; -*-
;;; Commentary:
;; AI integrations for Emacs, including Copilot and Claude Code IDE.
;;; Code:

(require 'corfu)

;; (defun mk/tab-completion ()
;;   "Smart TAB: Corfu > Copilot > Completion → indent.  Vertico endast i minibuffer."
;;   (interactive)
;;   (if (minibuffer-window-active-p (selected-window))
;;       (call-interactively
;;        (or (lookup-key vertico-map (kbd "<tab>"))
;;            #'vertico-exit))
;;     (cond
;;      ;; 1. Corfu popup visible → accept
;;      ((and (bound-and-true-p corfu-mode)
;;            (bound-and-true-p completion-in-region-mode))
;;       (corfu-insert))
;;      ;; 2. Copilot active → accept
;;      ((and (bound-and-true-p copilot-mode)
;;            (copilot--overlay-visible))
;;       (copilot-accept-completion))
;;      ((bound-and-true-p corfu-mode)
;;       (completion-at-point))
;;      ;; 4. Fallback
;;      (t
;;       (indent-for-tab-command)))))

;; ;; Smart Tab in Evil Insert mode
;; (with-eval-after-load 'evil
;;   (define-key evil-insert-state-map (kbd "TAB") #'mk/tab-completion)
;;   (define-key evil-insert-state-map (kbd "<tab>") #'mk/tab-completion)
;;   (define-key evil-insert-state-map (kbd "C-i") #'mk/tab-completion))

(use-package copilot
  :ensure nil
  :defer 3
  :vc (copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main" :rev :newest)
  :hook ((prog-mode localizeable-mode) . copilot-mode)
  :config
  (setq copilot-indent-offset-warning-disable t)
  ;; Keybindings for copilot completion
  (define-key copilot-completion-map (kbd "C-<return>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-c C-n") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "C-c C-p") 'copilot-previous-completion))

(use-package copilot-chat
  :defer t
  :vc (copilot-chat :url "https://github.com/chep/copilot-chat.el.git" :branch "main" :rev :newest)
  :hook ((copilot-chat . visual-line-mode)
         (git-commit-setup . copilot-chat-insert-commit-message))
  :config
  (setq copilot-chat-backend 'curl
        copilot-chat-frontend 'org
        copilot-chat-follow t))

;; (use-package claude-code-ide
;;   :defer t
;;   :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
;;   :config
;;   (setopt claude-code-ide-vterm-anti-flicker t
;;           claude-code-ide-vterm-render-delay 0.2)
;;   (claude-code-ide-emacs-tools-setup))

;;
;; (use-package dall-e-shell
;;   :defer t
;;   :config
;;   (setq dall-e-shell-openai-key (shell-command-to-string "echo $OPENAI_API_KEY")))
;;
;; (use-package chatgpt-shell
;;   :defer t
;;   :bind
;;   ("C-x C-v" . chatgpt-shell-quick-insert)
;;   ("C-x C-p" . chatgpt-shell-prompt-compose)
;;   ("C-x c g s" . chatgpt-shell-send-and-review-region)
;;   ("C-x c g r" . chatgpt-shell-refactor-code)
;;   ("C-x C-s" . (lambda ()
;;                  (interactive)
;;                  (split-window-right)
;;                  (other-window 1)
;;                  (chatgpt-shell)))
;;   :config
;;   (setf chatgpt-shell-anthropic-key (getenv "ANTHROPIC_API_KEY"))
;;   (setf chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
;;   (setf chatgpt-shell-deepseek-key (getenv "DEEPSEEK_API_KEY"))
;;   (setq chatgpt-shell-model-version "claude-3-5-sonnet-latest"))
;;
;; (use-package ob-chatgpt-shell
;;   :ensure nil
;;   :commands (org-babel-execute:chatgpt-shell)
;;   :config
;;   (ob-chatgpt-shell-setup))
;;
;;

;; (use-package aidermacs
;;   :ensure t
;;   :commands aidermacs-transient-menu
;;   :vc (aidermacs
;;        :url "https://github.com/MatthewZMD/aidermacs"
;;        :branch "main"
;;        :rev :newest)
;;   :config
;;   (setq aidermacs-auto-commits nil
;;         aidermacs-use-architect-mode nil
;;         aidermacs-show-diff-after-change t)
;;   ;; (setq aidermacs-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
;;   ;; (setq aidermacs-args '("--model" "deepseek/deepseek-reasoner"))
;;   (setq aidermacs-args '("--model" "deepseek/deepseek-chat"))
;;   :bind (:map global-map
;;               ("C-c a" . aidermacs-transient-menu)))


;;; Provide
(provide 'mk-ai)
;;; mk-ai.el ends here.
