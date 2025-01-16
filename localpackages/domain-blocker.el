;;; domain-blocker.el --- Block/unblock domains via /etc/hosts -*- lexical-binding: t; -*-

;;; Commentary:
;; A simple package to manage domain blocking via /etc/hosts file.
;; Useful for development environments where you need to temporarily
;; block certain domains.

;;; Code:

(defgroup domain-blocker nil
  "Domain blocking utilities."
  :group 'tools
  :prefix "domain-blocker-")

(defcustom domain-blocker-target-domain "developerservices2.apple.com"
  "The domain to block/unblock."
  :type 'string
  :group 'domain-blocker)

(defvar domain-blocker-hosts-backup nil
  "Temporary backup of /etc/hosts content.")

(defvar domain-blocker-debug nil
  "Enable debug output when non-nil.")

(defun domain-blocker-log (format-string &rest args)
  "Log debug message using FORMAT-STRING and ARGS when debug is enabled."
  (when domain-blocker-debug
    (apply #'message (concat "[Domain Blocker Debug] " format-string) args)))

;;;###autoload
(defun domain-blocker-block ()
  "Block domain by adding it to /etc/hosts."
  (interactive)
  (unless domain-blocker-target-domain
    (error "No target domain specified"))
  (domain-blocker-log "Attempting to block domain: %s" domain-blocker-target-domain)
  (let ((hosts-content (with-temp-buffer
                        (insert-file-contents "/etc/hosts")
                        (buffer-string))))
    (setq domain-blocker-hosts-backup hosts-content)
    (with-temp-file "/tmp/hosts.tmp"
      (insert hosts-content)
      (goto-char (point-max))
      (insert (format "\n127.0.0.1 %s" domain-blocker-target-domain)))
    (let ((command "osascript -e 'do shell script \"cat /tmp/hosts.tmp > /etc/hosts\" with administrator privileges'"))
      (async-shell-command command "*Domain Blocker*")
      (domain-blocker-log "Blocking domain command initiated"))))

;;;###autoload
(defun domain-blocker-unblock ()
  "Unblock domain by restoring original /etc/hosts."
  (interactive)
  (domain-blocker-log "Attempting to unblock domain")
  (when domain-blocker-hosts-backup
    (with-temp-file "/tmp/hosts.tmp"
      (insert domain-blocker-hosts-backup))
    (let ((command "osascript -e 'do shell script \"cat /tmp/hosts.tmp > /etc/hosts\" with administrator privileges'"))
      (async-shell-command command "*Domain Blocker*")
      (domain-blocker-log "Unblocking domain command initiated")
      (setq domain-blocker-hosts-backup nil))))

(defun domain-blocker--cleanup ()
  "Cleanup any temporary files and restore /etc/hosts if needed."
  (when domain-blocker-hosts-backup
    (domain-blocker-unblock)))

;; Ensure cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'domain-blocker--cleanup)

(provide 'domain-blocker)
;;; domain-blocker.el ends here
