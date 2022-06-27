;;; Simulator --- A small package for viewing iOS simulator logs

;;; Commentary: This package provides some support for iOS Simulator

(require 's)

;;; Code:

;; ----------------- READ SIMULATOR LOGS --------------------------------------------
(defun run-simulator-command (app-identifier)
    "Run simulator with simctl with (as APP-IDENTIFIER)."
    (format "xcrun simctl launch --console-pty booted %s -MyDefaultKey YES" app-identifier))

(defun ios-simulator-logs (app-identifier)
  "Show simulator logs in a buffer with (APP-IDENTIFIER)."
  (interactive)
  (with-output-to-temp-buffer "*simulator logs*"
    (async-shell-command
     (format "bash -c %s"
             (shell-quote-argument (run-simulator-command app-identifier))) "*simulator logs*")
    (pop-to-buffer "*simulator logs*")
    (auto-revert-tail-mode)))

;; ----------------- OPEN SIMULATOR FOLDER --------------------------------------------
(setq simulator-folder
      (shell-command-to-string "xcrun simctl getenv booted SIMULATOR_LOG_ROOT"))

(setq simulator-id
      (file-name-nondirectory simulator-folder))

(defun ios-simulator-print-type-and-name ()
  "Print the currently booted simulator name."
  (interactive)
  (message
   (string-trim
    (shell-command-to-string
     (concat "xcrun simctl list | grep -m1 "
             (file-name-nondirectory
              (shell-command-to-string "xcrun simctl getenv booted SIMULATOR_LOG_ROOT")))))))


(defun ios-simulator-open-root ()
  "Opens up the folder of the currect simulator root"
  (shell-command
   (concat "open " simulator-folder)))

;; ----------------- HYDRA MENU FOR SIMULATOR --------------------------------------------
(require 'all-the-icons)
(defun with-faicon (icon str &optional height v-adjust)
  "Displays an icon from Font Awesome icon."
	(s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(require 'pretty-hydra)

(defvar simulator-hydra--title (with-faicon "mobile" "Simulator" 1.5 -0.225))
(pretty-hydra-define ios-simulator-menu
 (:color amaranth :quit-key "q" :title simulator-hydra--title)
  ("Simulators"
   (
    ("o" ios-simulator-open-root "Open simulator root")
    ("p" ios-simulator-print-type-and-name "Print name of simulator")
    )))

(provide 'ios-simulator)
;;; ios-simulator.el ends here

