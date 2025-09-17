;;; xcodebuildserver.el --- package for automate setup of xcode-build-server -*- lexical-binding: t; -*-

;;; commentary:

;;; code:
;; Optional dependency
(defvar mode-line-hud-available-p (require 'mode-line-hud nil t)
  "Whether mode-line-hud is available.")

(defun xcodebuildserver:safe-mode-line-update (&rest args)
  "Safely call mode-line-hud:update if available."
  (when mode-line-hud-available-p
    (apply #'mode-line-hud:update args)))

(defun xcodebuildserver:safe-mode-line-notification (&rest args)
  "Safely call mode-line-hud:notification if available."
  (when mode-line-hud-available-p
    (apply #'mode-line-hud:notification args)))

(defgroup xcodebuildserver nil
  "Xcodebuildserver."
  :tag "xcodebuidserver"
  :group 'xcodebuildserver)

(defvar xcodebuildserver:debug nil
  "Debug mode for xcodebuildserver.")

(cl-defun xcodebuildserver:check-configuration (&key root workspace scheme)
  "Check if there is a configuration in (as ROOT) (as WORKSPACE) (as SCHEME)."
  (when xcodebuildserver:debug
    (message "Checking configuration for: %s|%s" workspace scheme))

  (when (not (xcodebuildserver:does-configuration-file-exist root))
    (xcodebuildserver:safe-mode-line-notification
     :message (format "Generating BSP configuration for: %s|%s"
                           (propertize workspace 'face 'font-lock-builtin-face)
                           (propertize scheme 'face 'font-lock-negation-char-face))
     :seconds 2)

    (let ((default-directory root)
          (command (format "xcode-build-server config %s -scheme %s > /dev/null 2>&1" workspace scheme)))
      (inhibit-sentinel-messages #'async-shell-command command))))

(defun xcodebuildserver:does-configuration-file-exist (root)
  "Check if configuration file exists in (as ROOT)."
  (file-exists-p (format "%s/%s" root "buildServer.json")))

(provide 'xcodebuildserver)

;;; xcodebuildserver.el ends here
