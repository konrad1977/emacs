;;; suppress-warnings.el --- Suppress obsolete warnings -*- lexical-binding: t; -*-
;;; Code:

(defvar suppress-warnings--active t
  "Non-nil means warning suppression is active.")

(defun suppress-obsolete-warnings (orig-fun type message &optional level buffer-name)
  "Filter out obsolete warnings from display-warning."
  (when (or (not suppress-warnings--active)
            (not (stringp message))
            (not (string-match-p
                  "obsolete\\|defadvice\\|when-let\\|if-let\\|deprecated\\|lexical-binding\\|positional arguments"
                  message)))
    (funcall orig-fun type message level buffer-name)))

(advice-add 'display-warning :around #'suppress-obsolete-warnings)

(defun suppress-warning-messages (orig-fun &optional format-string &rest args)
  "Filter out warning messages from message function."
  (cond
   ((null format-string)
    (funcall orig-fun nil))
   ((not (stringp format-string))
    (apply orig-fun format-string args))
   ((and suppress-warnings--active
         (string-match-p "Warning:\\|is deprecated\\|obsolete" format-string))
    nil)
   (t
    (apply orig-fun format-string args))))

(advice-add 'message :around #'suppress-warning-messages)

;; Säkerhetshake: stäng av under debugging
(defun suppress-warnings-disable ()
  "Temporarily disable warning suppression."
  (interactive)
  (setq suppress-warnings--active nil)
  (message "Warning suppression disabled"))

(defun suppress-warnings-enable ()
  "Re-enable warning suppression."
  (interactive)
  (setq suppress-warnings--active t))

(provide 'suppress-warnings)
;;; suppress-warnings.el ends here
