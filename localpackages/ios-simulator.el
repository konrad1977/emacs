;;; Simulator --- A small package for viewing iOS simulator logs

;;; Commentary: This package provides some support for iOS Simulator

;; Code:

(require 's)

;; ----------------- READ SIMULATOR LOGS --------------------------------------------
(setq simctl-command
      (concat "xcrun simctl spawn booted log stream "
              "--level=Error "
              "--style=compact "
              "--color=always "
              "| grep --color=always -v "
              "-e com.apple. "
              "-e locationd "
              "-e proactiveeventtrackerd "
              "-e runningboardd "
              "-e RunningBoardServices "
              "-e cloudd "
              "-e rtcreportingd "
              "-e backboardd "
              "-e CFNetwork "
              "-e SpringBoard "
              "-e Pasteboard "
              "-e libMobileGestalt "
              "-e routined "
              "-e biomesyncd "
              "-e chronod "
              "-e mediaremoted "
              "-e useractivityd "
              "-e CoreFoundation "
              "-e NewsToday "
              "-e ExtensionFoundation "
              "-e LocationSupport "
              "-e UIKitCore "
              "-e Security "
              "-e EventDispatch "
              "-e boringssl "
              "-e BackgroundTask"))
              

(defun ios-simulator-logs ()
  (interactive)
  (with-output-to-temp-buffer "*simulator logs*"
    (async-shell-command (format "bash -c %s" (shell-quote-argument simctl-command)) "*simulator logs*")
    (pop-to-buffer "*simulator logs*")))

;;    (highlight-

;; ----------------- OPEN SIMULATOR FOLDER --------------------------------------------

(setq simulator-folder
      (shell-command-to-string "xcrun simctl getenv booted SIMULATOR_LOG_ROOT"))

(setq simulator-id
      (file-name-nondirectory simulator-folder))

(setq simulator-type-and-name 
      (s-chomp
       (s-trim-left 
        (shell-command-to-string (concat "xcrun simctl list | grep -m1 " simulator-id)))))

(defun ios-simulator-print-type-and-name ()
  (interactive)
  (message simulator-type-and-name))

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

