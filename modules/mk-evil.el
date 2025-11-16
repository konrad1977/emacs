;; -*- lexical-binding: t; -*-
;; Code:;;
;; Workaround för saknad evil-mode-buffers variabel
(defvar evil-mode-buffers nil
  "Lista över buffertar där Evil mode är aktivt.")

(use-package evil
  :defer 1
  :ensure t
  :custom
  (evil-want-Y-yank-to-eol t)
  :init
  (setq-default evil-symbol-word-search t)
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-undo-system 'undo-fu
	evil-want-fine-undo t
	evil-want-C-u-scroll t
	evil-want-minibuffer t
	evil-respect-visual-line-mode t
	evil-search-module 'evil-search
	evil-vsplit-window-right t
	evil-ex-search-persistent-highlight nil  ;; Don't keep highlights after search
	evil-ex-search-case 'smart  ;; Smart case sensitivity
	evil-ex-search-vim-style-regexp t
	evil-split-window-below t
	evil-kill-on-visual-paste nil
	evil-default-cursor t
        evil-echo-state nil
 	evil-want-C-i-jump t)
  :config
  (setq evil-leader/in-all-states t)

  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'execute-extended-command)
  (evil-define-key 'normal 'global (kbd "<leader> .") 'embark-act)
  (evil-define-key 'normal 'global (kbd "<leader> P") 'package-install)
  (evil-define-key 'normal 'global (kbd "<leader> S") 'consult-line-multi)
  (evil-define-key 'normal 'global (kbd "<leader> F") 'consult-line)

  (evil-define-key 'normal 'global (kbd "<leader>TAB") '(lambda () (interactive) (switch-to-buffer nil)))
  (evil-define-key 'normal 'global (kbd "<leader>'") '(lambda () (interactive) (toggle-vterm)))

  (evil-define-key 'normal 'global (kbd "<leader> a a") 'android-emulator-start-logcat)
  (evil-define-key 'normal 'global (kbd "<leader> a q") 'android-emulator-quit-logcat)
  (evil-define-key 'normal 'global (kbd "<leader> a r") 'android-emulator-restart-logcat)
   ;;; Buffers
  (evil-define-key 'normal 'global (kbd "<leader> b b") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b x") 'bury-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b i") 'ibuffer)
  (evil-define-key 'normal 'global (kbd "<leader> b k") 'kill-current-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> b p") 'project-list-buffers)
  (evil-define-key 'normal 'global (kbd "<leader> b m") '(lambda () (interactive) (switch-to-buffer "*Messages*")))
  (evil-define-key 'normal 'global (kbd "<leader> b s") '(lambda () (interactive) (switch-to-buffer "*scratch*")))

  (evil-define-key 'normal 'global (kbd "<leader> c p") 'copilot-chat-transient)
  (evil-define-key 'normal 'global (kbd "<leader> c c") 'claude-code-ide-menu)
  ;; (evil-define-key 'normal 'global (kbd "<leader> c a") 'aidermacs-transient-menu)
  (evil-define-key 'normal 'global (kbd "<leader> c e") 'consult-compile-error)
  (evil-define-key 'normal 'global (kbd "<leader> c l") 'mk/compilation-get-errors)
  (evil-define-key 'normal 'global (kbd "<leader> c m") 'copilot-chat-insert-commit-message)
  (evil-define-key 'normal 'global (kbd "<leader> e a") 'embark-act)

  (evil-define-key 'normal 'global (kbd "<leader> f b") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> f d") 'delete-file)
  (evil-define-key 'normal 'global (kbd "<leader> f e") '(lambda () (interactive) (find-file user-init-file)))
  (evil-define-key 'normal 'global (kbd "<leader> f f") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader> f o") 'consult-find)
  (evil-define-key 'normal 'global (kbd "<leader> f l") 'consult-focus-lines)
  (evil-define-key 'normal 'global (kbd "<leader> f n") 'create-file-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> f r") 'consult-recent-file)
  (evil-define-key 'normal 'global (kbd "<leader> f m") 'focus-mode)
  (evil-define-key 'normal 'global (kbd "<leader> f s") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> f i") 'consult-imenu-multi)

  (evil-define-key 'normal 'global (kbd "<leader> h s") 'highlight-symbol-at-point)
  (evil-define-key 'normal 'global (kbd "<leader> h r") 'highlight-symbol-remove-all)
  (evil-define-key 'normal 'global (kbd "<leader> h n") 'highlight-symbol-next)
  (evil-define-key 'normal 'global (kbd "<leader> h N") 'highlight-symbol-prev)

  (evil-define-key 'normal 'global (kbd "<leader> j j") 'jira-issues)
  (evil-define-key 'normal 'global (kbd "<leader> j m") 'jira-issues-menu)
  (evil-define-key 'normal 'global (kbd "<leader> j a") 'jira-issues-actions-menu)

  (evil-define-key 'normal 'global (kbd "<leader> m l") 'imenu-list-smart-toggle)
  (evil-define-key 'normal 'global (kbd "<leader> m i") 'consult-imenu)
  (evil-define-key 'normal 'global (kbd "<leader> m b") 'consult-bookmark)
  (evil-define-key 'normal 'global (kbd "<leader> m c") 'consult-mode-command)

  (evil-define-key 'normal 'global (kbd "<leader> g g") 'google-this)

  ;; Eval
  (evil-define-key 'normal 'global (kbd "<leader> e b") 'eval-buffer)
  (evil-define-key 'normal 'global (kbd "<leader> e l") 'eval-last-sexp)

  (evil-define-key 'normal 'global (kbd "<leader> s s") 'isearch-forward)
  (evil-define-key 'normal 'global (kbd "<leader> s h") 'consult-isearch-history)
  (evil-define-key 'normal 'global (kbd "<leader> s o") 'occur)

  (evil-define-key 'normal 'global (kbd "<leader> t s") 'sort-lines)
  (evil-define-key 'normal 'global (kbd "<leader> t x") 'delete-trailing-whitespace)
  (evil-define-key 'normal 'global (kbd "<leader> t w") '(lambda () (interactive) (whitespace-mode 'toggle)))

  (evil-define-key 'normal 'global (kbd "<leader> p f") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader> p d") 'project-dired)
  (evil-define-key 'normal 'global (kbd "<leader> p g") 'project-find-regexp)
  (evil-define-key 'normal 'global (kbd "<leader> p s") 'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader> p k") 'project-kill-buffers)
  (evil-define-key 'normal 'global (kbd "<leader> p b") 'consult-project-buffer)


  (evil-define-key 'normal 'global (kbd "<leader> v s") 'magit-status)
  (evil-define-key 'normal 'global (kbd "<leader> v b") 'magit-diff-buffer-file)
  (evil-define-key 'normal 'global (kbd "<leader> v a") 'vc-annotate)
  (evil-define-key 'normal 'global (kbd "<leader> v l") 'magit-log-buffer-file)
  (evil-define-key 'normal 'global (kbd "<leader> v t") 'git-timemachine-toggle)

  (evil-define-key 'normal 'global (kbd "<leader> w w") 'weather-scout-show-forecast)

  (evil-define-key 'normal 'global (kbd "<leader> w t") 'window-transpose-layout)
  (evil-define-key 'normal 'global (kbd "<leader> w r") 'window-layout-rotate-clockwise)
  (evil-define-key 'normal 'global (kbd "<leader> w R") 'window-layout-rotate-anticlockwise)
  (evil-define-key 'normal 'global (kbd "<leader> w f") 'window-layout-flip-topdown)
  (evil-define-key 'normal 'global (kbd "<leader> w F") 'window-layout-flip-leftright)

  (evil-define-key 'normal 'global (kbd "<leader> q r") 'restart-emacs)
  (evil-define-key 'normal 'global (kbd "<leader> q q") 'save-buffers-kill-terminal)

  (evil-select-search-module 'evil-search-module 'isearch)
  (define-key evil-motion-state-map (kbd "<up>") 'ignore)
  (define-key evil-motion-state-map (kbd "<down>") 'ignore)
  (define-key evil-motion-state-map (kbd "<left>") 'ignore)
  (define-key evil-motion-state-map (kbd "<right>") 'ignore)

  (define-key evil-motion-state-map (kbd "C-+") #'(lambda () (interactive) (enlarge-window-horizontally 10)))
  (define-key evil-motion-state-map (kbd "C--") #'(lambda () (interactive) (shrink-window-horizontally 10)))
  (define-key evil-motion-state-map (kbd "C-M-+") #'(lambda () (interactive) (enlarge-window 3)))
  (define-key evil-motion-state-map (kbd "C-M--") #'(lambda () (interactive) (shrink-window 3)))


  (setq evil-normal-state-cursor '(box "DodgerBlue")
        evil-insert-state-cursor '(bar "DeepPink")
        evil-visual-state-cursor '(hollow "orchid"))
  ;; (evil-set-initial-state 'minibuffer-mode 'emacs)

  (evil-define-key 'normal evil-ex-map "q" 'mk/safe-kill-buffer-and-window)
  (evil-mode 1))

(with-eval-after-load 'evil
  (dolist (state '(normal insert visual motion emacs))
    (evil-define-key state 'global (kbd "s-M") nil)
    (evil-define-key state 'global (kbd "C-.") nil)
    (evil-define-key state 'global (kbd "C-k") nil)))

(use-package evil-collection
  :defer 3
  :after evil
  :config
  (evil-collection-init))

;; (use-package evil-mc
;;   :hook (evil-mode . global-evil-mc-mode)
;;   :bind (
;;          ("C-M-e" . evil-mc-make-all-cursors)
;;          ("C-M-n" . evil-mc-make-and-goto-next-match)
;;          ("C-M-s" . evil-mc-skip-and-goto-next-match)
;;          ("C-M-p" . evil-mc-make-and-goto-prev-match)
;;          ("C-M-j" . evil-mc-make-cursor-move-next-line)
;;          ("C-M-k" . evil-mc-make-cursor-move-prev-line))
;;   :custom
;;   (evil-mc-mode-line-text-inverse-colors t)
;;   (evil-mc-undo-cursors-on-keyboard-quit t)
;;   (evil-mc-mode-line-text-cursor-color t)
;;   :config
;;   (evil-define-key 'visual evil-mc-key-map
;;     "A" #'evil-mc-make-cursor-in-visual-selection-end
;;     "I" #'evil-mc-make-cursor-in-visual-selection-beg))

(use-package evil-matchit
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))

;; (use-package evil-surround
;;   :after evil
;;   :commands global-evil-surround-mode
;;   :custom
;;   (evil-surround-pairs-alist
;;    '((?\( . ("(" . ")"))
;;      (?\[ . ("[" . "]"))
;;      (?\{ . ("{" . "}"))

;;      (?\) . ("(" . ")"))
;;      (?\] . ("[" . "]"))
;;      (?\} . ("{" . "}"))

;;      (?< . ("<" . ">"))
;;      (?> . ("<" . ">"))))
;;   :hook (after-init . global-evil-surround-mode))

(use-package evil-commentary
  :ensure t
  :after (evil prog-mode)
  :init
  (evil-commentary-mode 1))

;; (use-package evil-visualstar
;;   :ensure t
;;   :after evil
;;   :hook (after-init . global-evil-visualstar-mode))

(use-package evil-snipe
  :ensure t
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package undo-fu
  :ensure t
  :defer t
  :custom
  (setopt undo-limit (* 13 160000))
  (setopt undo-outer-limit (* 13 24000000))
  (setopt undo-strong-limit (* 13 240000))
  :config
  (setq undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :hook (after-init . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")
        undo-fu-session-file-limit 10))

;; (use-package hardtime
;;   :ensure nil
;;   :after evil
;;   :config
;;   (global-hardtime-mode 1))

(use-package pulsar
  :hook (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.035
        pulsar-iterations 12
        pulsar-face 'pulsar-cyan
        pulsar-highlight-face 'evil-ex-lazy-highlight
        pulsar-pulse-functions '(
                                 evil-yank
                                 evil-yank-line
                                 evil-delete
                                 evil-delete-line
                                 evil-jump-item
                                 evil-scroll-down
                                 evil-scroll-up
                                 evil-scroll-page-down
                                 evil-scroll-page-up
                                 evil-scroll-line-down
                                 evil-scroll-line-up
                                 evil-window-up
                                 evil-window-rotate-upwards
                                 evil-window-rotate-downwards
                                 evil-window-down
                                 evil-window-left
                                 evil-window-right
                                 evil-window-vsplit
                                 evil-window-split)))

;; (defun my/recenter-after-jump (&rest _)
;;   "Centrera fönstret efter hoppkommandon i Evil."
;;   (recenter))

;; (dolist (fn '(evil-jump-backward
;;               evil-jump-forward
;;               evil-forward-section-begin
;;               evil-backward-section-begin
;;               evil-forward-sentence-begin
;;               evil-backward-sentence-begin
;;               evil-goto-definition))
;;   (advice-add fn :after #'my/recenter-after-jump))

;;; Provide
(provide 'mk-evil)
;;; mk-evil.el ends here.
