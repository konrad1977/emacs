;;; Simulator --- A small package for viewing iOS simulator logs -*- lexical-binding: t -*-
;;; Commentary: This package provides some support for iOS Simulator
;;; Code:

(require 'periphery-helper)

(defgroup ios-simulator nil
  "ios-simulator."
  :tag "ios-simulator"
  :group 'ios-simulator)

(defconst list-simulators-command
  "xcrun simctl list devices iPhone available -j"
  "List available simulators.")

(defconst get-booted-simulator-command
  "xcrun simctl list devices | grep -m 1 \"(Booted)\" | grep -E -o -i \"([0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12})\""
  "Get booted simulator id if any.")

(defvar current-language-selection "en-EN")

(cl-defun ios-simulator:build-language-menu (&key title)
  "Build language menu (as TITLE)."
  (interactive)
  (defconst languageList '(
                           ("🏴󠁧󠁢󠁥󠁮󠁧󠁿 󠁿English " "en-EN")
                           ("🇫🇷 French" "fr-FR")
                           ("🇳🇴 Norwegian (Bokmål)" "nb-NO")
                           ("🇯🇵 Japanese" "ja-JP")
                           ("🇩🇪 German" "de-DE")
                           ("🇪🇸 Spanish" "es-ES")
                           ("🇸🇪 Swedish" "sv-SE")))
  (progn
    (let* ((choices (seq-map (lambda (item) item) languageList))
           (choice (completing-read title choices)))
      (car (cdr (assoc choice choices))))))

(defun ios-simulator:booted-simulator ()
  "Get booted simulator if any."
  (let ((device-id (shell-command-to-string get-booted-simulator-command)))
    (if (not (string= "" device-id))
        (clean-up-newlines device-id)
      nil)))

(cl-defun ios-simulator:terminate-app (&key simulatorID &key appIdentifier)
  "Terminate app (as APPIDENTIFIER as SIMULATORID)."
  (inhibit-sentinel-messages #'call-process-shell-command
   (concat
    (if simulatorID
        (format "xcrun simctl terminate %s %s" simulatorID appIdentifier)
      (format "xcrun simctl terminate booted %s" appIdentifier)))))


(provide 'ios-simulator)
;;; ios-simulator.el ends here

