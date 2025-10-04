;; -*- lexical-binding: t; -*-
;;; Code:

(eval-when-compile
  (defvar display-time-24hr-format t)
  (defvar display-time-default-load-average nil))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Initialize packages to ensure they're available
(package-initialize)

(dolist (path (list
               (expand-file-name "localpackages/kanagawa-emacs" user-emacs-directory)
               (expand-file-name "localpackages/mito-laser-emacs" user-emacs-directory)
               (expand-file-name "localpackages/neofusion-emacs" user-emacs-directory)))
  (add-to-list 'custom-theme-load-path path))

;; Add local packages directory to load-path
(let ((dir (expand-file-name "localpackages" user-emacs-directory)))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (let ((default-directory dir))
      (normal-top-level-add-subdirs-to-load-path))))

(let ((paths '("/opt/homebrew/bin"
               "/opt/homebrew/sbin"
               "/usr/local/bin"
               "/System/Cryptexes/App/usr/bin"
               "/usr/bin"
               "/bin"
               "/usr/sbin"
               "/Library/TeX/texbin"
               "/sbin"
               "~/.local/bin"
               "~/.nvm/versions/node/v22.11.0/bin"
               "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
               "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
               "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin")))
  (setenv "PATH" (string-join paths ":")
          (setq exec-path paths)))

(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key [C-wheel-up])
(global-unset-key [C-wheel-down])
(global-set-key (kbd "M-w") 'ns-do-hide-emacs)


(provide 'mk-core)
;;; mk-core.el ends here
