;;; suppress-warnings.el --- Suppress obsolete warnings -*- lexical-binding: t; -*-
;;; Commentary:
;; This file completely suppresses obsolete warnings and other annoying messages

;;; Code:

(defun suppress-obsolete-warnings (orig-fun &rest args)
  "Filter out obsolete warnings from display-warning."
  (let ((_ (car args))
        (message (cadr args)))
    (unless (and (stringp message)
                 (or (string-match-p "obsolete" message)
                     (string-match-p "defadvice" message)
                     (string-match-p "when-let" message)
                     (string-match-p "if-let" message)
                     (string-match-p "deprecated" message)))
      (apply orig-fun args))))

(advice-add 'display-warning :around #'suppress-obsolete-warnings)

;; Also suppress messages about warnings
(defun suppress-warning-messages (orig-fun &rest args)
  "Filter out warning messages from message function."
  (let ((msg (car args)))
    (unless (and (stringp msg)
                 (or (string-match-p "Warning:" msg)
                     (string-match-p "\\.el: Warning:" msg)))
      (apply orig-fun args))))

(advice-add 'message :around #'suppress-warning-messages)

(provide 'suppress-warnings)
;;; suppress-warnings.el ends here
