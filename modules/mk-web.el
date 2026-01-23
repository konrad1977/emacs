;;; mk-web.el --- Web development and markup languages -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for web development including HTML, XML, CSS, JavaScript, and TypeScript.
;;; Code:

(use-package nxml-mode
  :ensure nil
  :mode "\\.xml\\'"
  :hook ((nxml-mode . setup-programming-mode)
         (nxml-mode . colorful-mode)
         (nxml-mode . display-line-numbers-mode)))

(use-package typescript-ts-mode
  :defer t
  :hook (typescript-ts-base-mode . (lambda ()
                                     (setq-local typescript-ts-indent-level 4
                                                 typescript-ts-mode-indent-offset 4
                                                 js-indent-level 4)))
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)))

;; (use-package markdown-mode
;;   :defer t
;;   :ensure t
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :config
;;   (setq markdown-fontify-code-blocks-natively t))

(use-package markdown-ts-mode
  :mode "\\.md\\'"
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode)))

(provide 'mk-web)
;;; mk-web.el ends here
