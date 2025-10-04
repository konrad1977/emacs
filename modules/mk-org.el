;; -*- lexical-binding: t; -*-
;;; Code:
(use-package org
  :hook ((org-mode . org-display-inline-images)
         (org-mode . my/org-prettify-symbols))
  :config
  (defun my/org-prettify-symbols ()
    (setq prettify-symbols-alist
	  (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
		  '(("#+begin_src"      . 9998)         ; ✎
		    ("#+end_src"        . " ")         ; 
		    ("#+results:"       . 9776)         ; ☰
		    ("#+attr_latex:"    . "🄛")
		    ("#+attr_html:"     . "🄗")
		    ("#+attr_org:"      . "🄞")
		    ("#+name:"          . "🄝")         ; 127261
		    ("#+caption:"       . "🄒")         ; 127250
		    ("#+startup:"       . 10034)        ; ✲
		    ("#+options:"       . 9965)         ; ⛭
		    ("#+begin_quote"    . 187)          ; »
		    ("#+end_quote"      . 171)          ; «
                    ("#+begin_results"  . 8943)         ; ⋯
                    ("#+end_results"    . 8943)         ; ⋯
		    )))
    (setq prettify-symbols-unprettify-at-point t)
    (prettify-symbols-mode 1))

  (setq org-lowest-priority ?F)  ;; Gives us priorities A through F
  (setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].

  (setq org-priority-faces
        '((65 . "#BF616A")
          (66 . "#EBCB8B")
          (67 . "#B48EAD")
          (68 . "#81A1C1")
          (69 . "#5E81AC")
          (70 . "#4C566A")))

  (setq org-ellipsis "…"
        ;; org-agenda-files '("work.org" "projekt.org")
        org-agenda-block-separator ?─
        org-agenda-files '("~/Desktop/org/agenda.org")
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-done t
        org-agenda-span 1
        org-agenda-start-day "+0d"
        org-agenda-tags-column 0
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-custom-commands '(("v" "Agenda and todos"
                                      ((tags "PRIORITY=\"A\""
                                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                              (org-agenda-overriding-header "HIGH PRIORITY unfinished tasks:")))
                                       (tags "PRIORITY=\"B\""
                                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                              (org-agenda-overriding-header "MEDIUM PRIORITY unfinished tasks:")))
                                       (tags "PRIORITY=\"C\""
                                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                              (org-agenda-overriding-header "LOW PRIORITY unfinished tasks:")))
                                       (agenda "")
                                       (alltodo ""))))
        org-auto-align-tags nil
        org-clock-sound "~/.emacs.d/etc/sound/bell.mp3"
        org-confirm-babel-evaluate nil
        org-cycle-separator-lines 2
        org-directory "~/Desktop/org/Todo/"
        org-fontify-emphasized-text t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers t
        org-adapt-indentation t
        org-hide-leading-stars t
        org-log-done 'time
        org-log-into-drawer t
        org-pretty-entities t
        org-return-follows-link t
        org-src-edit-src-content-indentation 0
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-fontify-whole-heading-line t
        org-src-preserve-indentation t
        font-lock-maximum-decoration t
        org-element-use-cache t
        org-src-tab-acts-natively t
        org-startup-folded nil
        org-startup-with-inline-images t
        org-tags-column 0
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────")

  (setq org-todo-keywords
        '((sequence
	   "TODO" "PROJ" "READ" "CHECK" "IDEA" ; Needs further action
	   "|"
	   "DONE")))                           ; Needs no action currently

  (setq org-todo-keyword-faces
        '(("TODO"      :inherit (org-todo region) :foreground "#A3BE8C" :weight bold)
	  ("PROJ"      :inherit (org-todo region) :foreground "#88C0D0" :weight bold)
          ("READ"      :inherit (org-todo region) :foreground "#8FBCBB" :weight bold)
	  ("CHECK"     :inherit (org-todo region) :foreground "#81A1C1" :weight bold)
	  ("IDEA"      :inherit (org-todo region) :foreground "#EBCB8B" :weight bold)
	  ("DONE"      :inherit (org-todo region) :foreground "#30343d" :weight bold)))

  )

(defun mk/play-sound (origin-fn sound)
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
  (require 'ob-swiftui)
  (ob-swiftui-setup)

  (setq-local org-confirm-babel-evaluate t)

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.3 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.18 :weight bold))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.16 :weight bold))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.15 :weight bold))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.1 :weight bold))))
   '(org-level-6 ((t (:inherit outline-5 :height 1 :weight bold))))
   '(org-level-7 ((t (:inherit outline-5 :height 1 :weight semi-bold))))
   '(org-block-begin-line ((t (:foreground "#678" :underline t :background nil))))
   '(org-block-end-line ((t (:foreground "#678" :overline t :background nil )))))

  (setq org-custom-todo-faces '
        (("TODO" :background "#FF5D62" :distant-foreground "#FF5D62" :foreground "#FFFFFF" :weight 'bold)
         ("NEXT" :background "#7FB4CA" :distant-foreground "#7FB4CA" :foreground "#1c1c24" :weight 'bold)
         ("STARTED" :background "#957FB8" :foreground "#FFFFFF" :weight 'bold)
         ("DELEGATED" :background "#7AA89F" :foreground "#1c1c24" :weight 'bold)
         ("QA" :background "#54536D" :weight 'bold)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (swift . t)
                                 (kotlin . t)
                                 (shell . t)
                                 (restclient . t)))

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sw" . "src swift"))
  (add-to-list 'org-structure-template-alist '("sr" . "src restclient"))
  (add-to-list 'org-structure-template-alist '("swiftui" . "src swiftui :view CustomView"))
  (add-to-list 'org-structure-template-alist '("elisp" . "src emacs-lisp"))
  (add-to-list 'org-modules 'org-tempo t))

(use-package ob-restclient
  :defer t)

(use-package ob-swift
  :defer t
  :after org-mode
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((swift . t)))))
(use-package ob-swiftui
  :defer t
  :after org-mode
  :config
  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (when org-inline-image-overlays
                                              (org-redisplay-inline-images))))
  (add-to-list 'org-babel-tangle-lang-exts
               '("swiftui" . "swift"))
  (add-to-list 'org-src-lang-modes
               '("swiftui" . swift))
  (ob-swiftui-setup))

(use-package ob-kotlin
  :defer t
  :after org-mode
  :config
  (add-to-list 'org-babel-tangle-lang-exts
               '("kotlin" . "kt"))
  (add-to-list 'org-src-lang-modes
               '("kotlin" . kotlin))
  (ob-kotlin-setup))

(use-package org-superstar
  :after org
  :config
  (setq org-superstar-leading-bullet ?\s)
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("DONE" . 9744)
                                          ("READ" . 9744)
                                          ("IDEA" . 9744)
                                          ("WAITING" . 9744)
                                          ("CANCELLED" . 9744)
                                          ("PROJECT" . 9744)
                                          ("POSTPONED" . 9744))))

(use-package org-modern
  :hook ((org-mode . global-org-modern-mode))
  :config
  (setq
   org-auto-align-tags t
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-modern-block t
   org-modern-block-name nil
   org-modern-star '("❶" "❷" "❸" "❹" "❺" "❻" "❼")

   ;; Don't style the following
   org-modern-tag nil
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

(use-package olivetti
  :hook ((org-mode . olivetti-mode))
  :custom
  (olivetti-style 'fancy)
  (setq olivetti-style t
        olivetti-body-width 140
        olivetti-hide-mode-line t))


;;; Provide
(provide 'mk-org)
;;; mk-org.el ends here.
