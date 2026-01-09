;;; mk-notifications.el --- Enhanced Emacs notifications using KnockKnock -*- lexical-binding:t -*-

;;; Commentary:
;; This module integrates KnockKnock to provide desktop notifications

;;; Code:

(require 'knockknock nil t)

(defvar my-knockknock-last-message nil
  "Used to prevent duplicate knockknock notifications.")

(defun my-knockknock--classify-icon (msg)
  "Return a Nerd Font icon based on message content."
  (cond
   ((string-match-p "\\(error\\|failed?\\|exception\\)" msg)
    "nf-cod-error")
   ((string-match-p "\\(warn\\|warning\\)" msg)
    "nf-cod-warning")
   ((string-match-p "\\(success\\|completed?\\|passed\\)" msg)
    "nf-cod-check")
   ((string-match-p "\\(build\\|compil\\|make\\)" msg)
    "nf-cod-tools")
   ((string-match-p "\\(info\\|note\\)" msg)
    "nf-cod-info")
   (t "nf-fa-info_circle"))) ;; default


(defun my-knockknock--split-title (msg)
  "If MSG contains a colon, treat part before colon as title.
Otherwise default title to 'Emacs'."
  (let* ((parts (split-string msg ": " t)))
    (if (> (length parts) 1)
        (list (car parts) (string-join (cdr parts) ": "))
      (list "Emacs" msg))))


;; (defun my-knockknock-message-hook ()
;;   "Show Emacs messages through knockknock with filtering and icons."
;;   (let ((msg (current-message)))
;;     (when (and msg
;;                (stringp msg)
;;                (not (equal msg my-knockknock-last-message)))

;;       (setq my-knockknock-last-message msg)

;;       ;; Ignore empty or whitespace messages
;;       (when (not (string-match-p "^[[:space:]\n]*$" msg))

;;         ;; Filter: only interesting messages
;;         (when (string-match-p
;;                "\\(error\\|failed?\\|exception\\|warn\\|warning\\|build\\|compil\\|success\\|passed\\)"
;;                (downcase msg))

;;           (pcase-let* ((`(,title ,body) (my-knockknock--split-title msg))
;;                        (icon (my-knockknock--classify-icon msg)))
;;             (knockknock-notify
;;              :title title
;;              :message body
;;              :icon icon
;;              :duration 3)))))))

;; (add-hook 'message-log-hook #'my-knockknock-message-hook)

(defvar my-knockknock--inhibit nil)

(defun my-knockknock--capture-message (&rest args)
  "Capture all messages after they were printed, safely."
  (let ((msg (apply #'format args)))
    (unless my-knockknock--inhibit
      (let ((my-knockknock--inhibit t))
        (when (and msg (not (string-empty-p msg)))
          ;; Log manually so message-log-hook fires
          (with-current-buffer (get-buffer-create "*Messages*")
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert msg "\n"))))))))


(advice-add 'message :after #'my-knockknock--capture-message)


(provide 'mk-notifications)
;;; mk-notifications.el ends here
