;;; mk-ai.el --- AI integrations -*- lexical-binding: t; -*-
;;; Commentary:
;; AI integrations for Emacs, including Copilot and Claude Code IDE.
;;; Code:

(require 'corfu)

(defun mk/tab-completion ()
  "Smart tab: prioritera Corfu/Cape, annars Copilot."
  (interactive)
  (cond
   ;; 1. If Corfu is active and in completion-in-region-mode, use Corfu
   ((and (bound-and-true-p corfu-mode)
         completion-in-region-mode)
    (corfu-insert))
   ;; 2. If copilot-mode is active and has a visible overlay,
   ((and (bound-and-true-p copilot-mode)
         (copilot--overlay-visible))
    (copilot-accept-completion))
   ;; 3. Fallback to default tab behavior
   (t
    (tab-to-tab-stop))))

(use-package copilot
  :ensure nil
  :defer 3
  :vc (copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main" :rev :newest)
  :hook ((prog-mode localizeable-mode) . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("C-c C-n" . copilot-next-completion)
        ("C-c C-p" . copilot-previous-completion))
  :config
  (setq copilot-indent-offset-warning-disable t))

(use-package copilot-chat
  :defer t
  :vc (copilot-chat :url "https://github.com/chep/copilot-chat.el.git" :branch "main" :rev :newest)
  :hook ((copilot-chat . visual-line-mode)
         (git-commit-setup . copilot-chat-insert-commit-message))
  :config
  (setq copilot-chat-backend 'curl
        copilot-chat-frontend 'org
        copilot-chat-follow t))

(use-package claude-code-ide
  :defer t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :config
  (setopt claude-code-ide-vterm-anti-flicker t
          claude-code-ide-vterm-render-delay 0.2)
  (claude-code-ide-emacs-tools-setup))

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
