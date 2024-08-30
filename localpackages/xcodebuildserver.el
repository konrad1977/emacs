;;; xcodebuildserver.el --- package for automate setup of xcode-build-server -*- lexical-binding: t; -*-

;;; commentary:

;;; code:
(require 'mode-line-hud)

(defgroup xcodebuildserver nil
  "Xcodebuildserver."
  :tag "xcodebuidserver"
  :group 'xcodebuildserver)

(cl-defun xcodebuildserver:check-configuration (&key root &key workspace &key scheme)
  "Check if there is a configuration in (as ROOT) (as WORKSPACE) (as SCHEME)."
  (mode-line-hud:update :message (format "Preparing %s|%s"
                                         (propertize workspace 'face 'font-lock-builtin-face)
                                         (propertize scheme 'face 'font-lock-negation-char-face)))

  (when (not (xcodebuildserver:does-configuration-file-exist root))
    (let ((default-directory root)
          (command (format "xcode-build-server config %s -scheme '%s' > /dev/null 2>&1" workspace scheme)))
      (async-shell-command command))))

(defun xcodebuildserver:does-configuration-file-exist (root)
  "Check if configuration file exists in (as ROOT)."
  (file-exists-p (format "%s/%s" root "buildServer.json")))

(provide 'xcodebuildserver)

;;; xcodebuildserver.el ends here
