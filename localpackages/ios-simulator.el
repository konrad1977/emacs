;;; Simulator --- A small package for viewing iOS simulator logs

;;; Commentary: This package provides some support for iOS Simulator

;; Code:

(setq simctl-command
      (concat "xcrun simctl spawn booted log stream --level=Default --style=compact | grep -v "
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
    (pop-to-buffer "*simulator logs*"))
    (highlight-lines-matching-regexp "UI\\w+\\|NSLayout*\\w+"))

(provide 'ios-simulator) 

