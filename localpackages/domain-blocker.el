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
  (unless (domain-blocker-is-blocked-p)
    (let ((command (format "osascript -e 'do shell script \"echo \\\"127.0.0.1 %s\\\" >> /etc/hosts\" with administrator privileges'"
                           domain-blocker-target-domain)))
      (call-process-shell-command command nil 1)
      (domain-blocker-log "Blocking domain command initiated"))))

;;;###autoload
(defun domain-blocker-unblock ()
  "Unblock domain by removing it from /etc/hosts."
  (interactive)
  (domain-blocker-log "Attempting to unblock domain")
  (when domain-blocker-target-domain
    (let ((command (format "osascript -e 'do shell script \"awk '!/%s/' /etc/hosts > /tmp/hosts.tmp && cp /tmp/hosts.tmp /etc/hosts\" with administrator privileges'"
                           (regexp-quote domain-blocker-target-domain))))
      (call-process-shell-command command nil 0)
      (domain-blocker-log "Unblocking domain command initiated"))))

;;;###autoload
(defun domain-blocker-is-blocked-p ()
  "Check if the target domain is currently blocked."
  (when domain-blocker-target-domain
    (with-temp-buffer
      (insert-file-contents "/etc/hosts")
      (goto-char (point-min))
      (search-forward domain-blocker-target-domain (point-max) t))))

;;;###autoload
(defun domain-blocker-status ()
  "Show the current blocking status of the target domain."
  (interactive)
  (if domain-blocker-target-domain
      (message "Domain %s is currently %s"
               domain-blocker-target-domain
               (if (domain-blocker-is-blocked-p)
                   "BLOCKED"
                 "NOT BLOCKED"))
    (message "No target domain specified")))

(defun domain-blocker--cleanup ()
  "Cleanup any temporary files and restore /etc/hosts if needed."
  (when domain-blocker-hosts-backup
    (domain-blocker-unblock)))

;; Ensure cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'domain-blocker--cleanup)

(provide 'domain-blocker)
;;; domain-blocker.el ends here
