;;; mode-line-hud.el --- package for interacting with the mood-line -*- lexical-binding: t; -*-

;;; commentary:

;;; package for interacting with mood-line.

;;; code:

(defgroup mode-line-hud nil
  "Mode-line hud."
  :tag "mode-line-hud"
  :group 'mode-line-hud)

(defvar-local mood-line-segment-hud--text "")

(defcustom show-in-echo-area t
  "Show weather temperature in fahrenheit."
  :group 'mode-line-hud
  :type '(boolean))

;;;###autoload
(defun mood-line-segment-hud ()
  "Return the number of active multiple-cursors."
  mood-line-segment-hud--text)

;;;###autoload
(cl-defun mode-line-hud:update (&key message)
  "Update modeline as (MESSAGE)."
  (run-with-timer 0.05 nil
                  (lambda ()
                    (setq mood-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

;;;###autoload
(cl-defun mode-line-hud:updateWith (&key message &key delay)
  "Update modeline as (MESSAGE DELAY)."
  (run-with-timer delay nil
                  (lambda ()
                    (setq mood-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

;;;###autoload
(cl-defun mode-line-hud:notification (&key message &key seconds)
  "Update modeline as (MESSAGE SECONDS)."
  (run-with-timer 0.025 nil
                  (lambda ()
                    (mode-line-hud:reset :message mood-line-segment-hud--text :delay seconds)
                    (setq mood-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

(cl-defun mode-line-hud:reset (&key message &key delay)
  "Reset to previous MESSAGE."
  (run-with-timer delay nil
                  (lambda ()
                    (setq mood-line-segment-hud--text message)
                    (when show-in-echo-area
                      (message message))
                    (force-mode-line-update))))

(provide 'mode-line-hud)

;;; mode-line-hud.el ends here
