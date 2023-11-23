;;; xcode-build.el --- Simple package for building and running swift apps in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Run project using Xcode

;;; code:

(defun xcode-build:build()
  "Start a build using Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'")
  (message "Build project using Xcode..."))

(defun xcode-build:stop()
  "Stop application from Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'end tell'")
  (message "Stopping simulator ..."))

(defun xcode-build:run()
  "Run application from Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'")
  (message "Run project using Xcode..."))

(defun xcode-build:test()
  "Run current test scheme from Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'")
  (message "Test project using Xcode..."))

(provide 'xcode-build)
;;; xcode-build.el ends here
