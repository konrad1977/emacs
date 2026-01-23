;;; mk-org.el --- Org mode configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for Org mode and related packages.
;;; Code:

;; Fix org-fold-region missing SPEC error (org 9.7+ compatibility issue)
;; This must be loaded before org to patch the bug

(use-package org
  :demand t
  :hook ((org-mode . org-display-inline-images))
  :config
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.2 :weight black))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.1 :weight bold))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.075 :weight semi-bold))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.050 :weight medium))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-checkbox ((t (:height 1.2 :weight light))))
   (set-face-attribute 'org-document-title nil :height 1.0))
  (setopt org-clock-sound "~/.emacs.d/etc/sound/bell.mp3"
          org-confirm-babel-evaluate nil
          org-cycle-separator-lines 2
          org-directory "~/gtd/"
          org-fontify-emphasized-text t
          org-fontify-whole-heading-line t
          org-hide-emphasis-markers t
          org-adapt-indentation t
          org-hide-leading-stars t
          org-log-done 'time
          org-log-into-drawer t
          org-return-follows-link t
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-fontify-whole-heading-line t
          org-src-preserve-indentation t
          font-lock-maximum-decoration t
          org-element-use-cache t
          org-src-tab-acts-natively t
          org-startup-folded nil
          org-startup-indented t
          org-startup-with-inline-images t
          org-tags-column 0))

(defun mk/play-sound (_ sound)
  "Play a sound when a task is marked as done.
ORIGIN-FN is the original function being advised.
SOUND is the sound specification to play."
  (cl-destructuring-bind (_ _ file) sound
    (make-process :name (concat "play-sound-" file)
                  :connection-type 'pipe
                  :command `("afplay" ,file))))
(advice-add 'play-sound :around 'mk/play-sound)

(with-eval-after-load 'org

  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))

  (require 'org-tempo)
  (setq-local org-confirm-babel-evaluate t)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 ;; (swift . t)
                                 ;; (kotlin . t)
                                 (shell . t)
                                 ;; (restclient . t)
                                 ))

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sw" . "src swift"))
  (add-to-list 'org-structure-template-alist '("sr" . "src restclient"))
  (add-to-list 'org-structure-template-alist '("swiftui" . "src swiftui :view CustomView"))
  (add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))
  (add-to-list 'org-modules 'org-tempo t))

;; (use-package ob-restclient
;;   :defer t)
;;
(use-package ob-swift
  :defer t
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("swift" . swift))
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((swift . t)))))
(use-package ob-swiftui
  :defer t
  :after org
  :config
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))
  (add-to-list 'org-babel-tangle-lang-exts
               '("swiftui" . "swift"))
  (add-to-list 'org-src-lang-modes
               '("swiftui" . swift))
  (ob-swiftui-setup))

(use-package org-modern
  :hook ((org-mode . global-org-modern-mode))
  :config
  (setq org-todo-keywords
        '((sequence
           "REFILE(r)" "TODO(t)" "NEXT(n)" "DOING(o)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")
          (sequence
           "PROJ(p)" "|" "COMPLETE" "CANCELED")))
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•")))
  (setq org-modern-star 'replace
        org-modern-horizontal-rule (make-string 36 ?─)
        org-fold-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-modern-block-name t
        org-ellipsis "…"))

(use-package olivetti
  :hook (((org-mode markdown-ts-mode). olivetti-mode))
  :custom
  (olivetti-style t)
  (olivetti-body-width 120)
  (olivetti-hide-mode-line t))

(use-package org-gtd
  :ensure t
  :after org
  :defer t
  :init
  (require 'org)  ;; Must load org before org-gtd
  (setq org-gtd-update-ack "4.0.5")
  :custom
  (org-gtd-directory "~/gtd/")

  ;; Configure TODO keyword states (options like "TODO(t)" or "DONE(d!)" are fine)
  (org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL")))
  (org-gtd-keyword-mapping '((todo . "TODO")
                             (next . "NEXT")
                             (wait . "WAIT")
                             (canceled . "CNCL")))

  :config
  (org-edna-mode 1)
  (setopt org-agenda-files (list org-gtd-directory))
  (add-hook 'org-gtd-clarify-mode-hook
            (lambda () (breadcrumb-mode -1)))

  :bind
  ;; Global keybindings (work anywhere in Emacs)
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-reflect-stuck-projects)

   ;; Keybinding for organizing items (only works in clarify buffers)
   :map org-gtd-clarify-mode-map
   ("C-c c" . org-gtd-organize)

   ;; Quick actions on tasks in agenda views (optional but recommended)
   :map org-agenda-mode-map
   ("C-c ." . org-gtd-agenda-transient)))

;;; Provide
(provide 'mk-org)
;;; mk-org.el ends here.
