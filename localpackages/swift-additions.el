;;; swift-additions.el --- Package for compiling and running swift apps in Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Package for building and runnning iOS/macos apps from Emacs

;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'dash)
(require 'cl-lib)
(require 'projectile)
(require 'flycheck)

(defconst xcodebuild-buffer "*xcodebuild*"
  "Xcodebuild buffer.")

(defgroup swift-additions:xcodebuild nil
  "REPL."
  :tag "swift-additions:xcodebuild"
  :group 'swift-additions)

(defcustom swift-additions:xcode-scheme "SecoTools-dev"
  "Current xcode scheme."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

(defcustom swift-additions:simulator-id "C1278718-C3C4-4AAD-AF0A-A51794D0F6BB"
  "Current simulator ID of choice."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

(defcustom swift-additions:app-identifier "com.secotools.dev"
  "Current app-identifier of choice."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

(defcustom swift-additions:build-configuration "Debug"
  "Build name from configuration."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

(setq invoked-from-buffer "")

(defun swift-additions:simulator-log-command ()
    "Command to filter and log the simulator."
    (concat "xcrun simctl spawn booted log stream "
            "--level error "
            "--style compact "
            "--color always "
            "| grep -Ei "
            "\'[Cc]onstraint|%s\'" (swift-additions:project-name)))
              
(defun swift-additions:show-ios-simulator-logs ()
  "Show simulator logs in a buffer."
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (async-shell-command (swift-additions:simulator-log-command) xcodebuild-buffer)
    (ansi-color-apply-on-region (point-min) (point-max))
    (auto-revert-tail-mode t)))
    
(defun swift-additions:match-product-name (text)
  "Match product name from (as TEXT)."
  (string-match "FULL_PRODUCT_NAME = \\(.*\\)" text)
  (match-string 1 text))

(defun swift-additions:build-settings-command ()
  "Build settings command."
  (concat
   "xcrun xcodebuild \\"
   (format "-scheme %s \\" swift-additions:xcode-scheme)
   (format "-workspace %s.xcworkspace \\" (swift-additions:xcode-workspace))
   (format "-configuration %s \\" swift-additions:build-configuration)
   "-showBuildSettings | grep 'FULL_PRODUCT_NAME'"))

(defun swift-additions:read-app-product-name ()
  "Read app product name."
  (swift-additions:match-product-name
    (async-shell-command (swift-additions:build-settings-command))))
  
(defun swift-additions:find-app ()
  "Find app to install in simulator."
    (car
     (directory-files-recursively
      (projectile-project-root) "\\.app$")))

(defun swift-additions:project-name ()
  "Get workspace name."
  (file-name-sans-extension
   (file-name-nondirectory
    (car
     (directory-files
      (projectile-project-root) t ".xcworkspace")))))
  
(defconst swift-additions:install-folder
  "build/Build/Products/Debug-iphonesimulator/")

(defun build-and-run-command (simulator-id)
  "Xcodebuild with simulator id (as SIMULATOR-ID)."
      (concat
       "env /usr/bin/arch -x86_64 \\"
       "xcrun xcodebuild \\"
       (format "-scheme %s \\" swift-additions:xcode-scheme)
       (format "-workspace %s.xcworkspace \\" (swift-additions:project-name))
       (format "-configuration %s \\" swift-additions:build-configuration)
       "-sdk iphonesimulator \\"
       "-jobs 4 \\"
       (format "-destination 'platform=iOS Simulator,id=%s' \\" simulator-id)
       "-derivedDataPath \\"
       "build | xcbeautify \n"))

(defun swift-additions:install-and-run-simulator-command ()
  "Install and launch app."
  (concat
   "env /usr/bin/arch -x86_64 \\"
   (format "xcrun simctl install %s %s%s.app\n" swift-additions:simulator-id swift-additions:install-folder swift-additions:xcode-scheme)
   (format "xcrun simctl launch %s %s" swift-additions:simulator-id swift-additions:app-identifier)))

(defun start-simulator-when-done (process signal)
  "Launching simular when done building (as PROCESS SIGNAL)."
  (when (memq (process-status process) '(exit signal))
    (with-current-buffer (get-buffer-create xcodebuild-buffer)
      (progn
        (if (swift-additions:buffer-contains-substring "BUILD FAILED")
            (progn
              (kill-buffer xcodebuild-buffer)
              (with-current-buffer invoked-from-buffer
                (list-flycheck-errors)
                (flycheck-first-error))))
        (if (swift-additions:buffer-contains-substring "Build Succeeded")
            (let ((default-directory (projectile-project-root)))
              (call-process-shell-command (swift-additions:install-and-run-simulator-command))
              (swift-additions:show-ios-simulator-logs)))))
      (shell-command-sentinel process signal)))

(defun swift-additions:clear-xcodebuild-buffer ()
  "Clear the xcodebuild buffer."
  (interactive)
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (erase-buffer)))

(defun swift-additions:build-and-run-ios-app ()
  "Build project using xcodebuild and then run iOS simulator."
  (interactive)
  (setq invoked-from-buffer (current-buffer))
  (save-some-buffers t)
  (swift-additions:terminate-app-in-simulator)
  (if (get-buffer-process xcodebuild-buffer)
        (delete-process xcodebuild-buffer))
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (erase-buffer)
    (pop-to-buffer (current-buffer))
    (setq buffer-read-only nil)
    (compilation-minor-mode t)
    (auto-revert-mode t)
    (let* ((default-directory (projectile-project-root))
           (proc (progn
                   (async-shell-command (build-and-run-command swift-additions:simulator-id) xcodebuild-buffer)
                   (get-buffer-process xcodebuild-buffer))))
      (if (process-live-p proc)
          (set-process-sentinel proc #'start-simulator-when-done))
      (message "No process running."))))

(defun swift-additions:build-ios-app ()
  "Build project using xcodebuild."
  (interactive)
  (save-some-buffers t)
  (swift-additions:terminate-app-in-simulator)
  (if (get-buffer-process xcodebuild-buffer)
        (delete-process xcodebuild-buffer))
  (with-current-buffer (get-buffer-create xcodebuild-buffer)
    (erase-buffer)
    (pop-to-buffer (current-buffer))
    (setq buffer-read-only nil)
    (fundamental-mode)
    (compilation-minor-mode)
    (let ((default-directory (projectile-project-root)))
      (async-shell-command (build-and-run-command swift-additions:simulator-id) xcodebuild-buffer))))

(defun swift-additions:clean-build-folder ()
  "Clean app build folder."
  (interactive)
  (let ((default-directory (concat (projectile-project-root) "build")))
    (if (file-directory-p default-directory)
        (progn
          (delete-directory default-directory t nil)
            (message "Removing build folder %s" default-directory))
    (message "Build folder %s doesnt exist" default-directory))))

(defun swift-additions:buffer-contains-substring (string)
  "Check if buffer contain (as STRING)."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun swift-additions:launch-app-in-simulator ()
  "Launch simulator and app."
  (interactive)
  (shell-command
   (concat
    "open -a simulator \n"
    (format "xcrun simctl launch %s %s" swift-additions:simulator-id swift-additions:app-identifier))))

(defun swift-additions:terminate-app-in-simulator ()
  "Terminate app."
  (interactive)
  (shell-command
   (concat
    (format "xcrun simctl terminate %s %s" swift-additions:simulator-id swift-additions:app-identifier))))

(defun ar/counsel-apple-search ()
  "Ivy interface for dynamically querying apple.com docs."
  (interactive)
  (require 'request)
  (require 'json)
  (require 'url-http)
  (ivy-read "apple docs: "
            (lambda (input)
              (let* ((url (url-encode-url (format "https://developer.apple.com/search/search_data.php?q=%s" input)))
                     (c1-width (round (* (- (window-width) 9) 0.3)))
                     (c2-width (round (* (- (window-width) 9) 0.5)))
                     (c3-width (- (window-width) 9 c1-width c2-width)))
                (or
                 (ivy-more-chars)
                 (let ((request-curl-options (list "-H" (string-trim (url-http-user-agent-string)))))
                   (request url
                     :type "GET"
                     :parser 'json-read
                     :success (cl-function
                               (lambda (&key data &allow-other-keys)
                                 (ivy-update-candidates
                                  (mapcar (lambda (item)
                                            (let-alist item
                                              (propertize
                                               (format "%s   %s   %s"
                                                       (truncate-string-to-width (propertize (or .title "")
                                                                                             'face '(:foreground "yellow")) c1-width nil ?\s "…")
                                                       (truncate-string-to-width (or .description "") c2-width nil ?\s "…")
                                                       (truncate-string-to-width (propertize (string-join (or .api_ref_data.languages "") "/")
                                                                                             'face '(:foreground "cyan1")) c3-width nil ?\s "…"))
                                               'url .url)))
                                          (cdr (car data)))))))
                   0))))
            :action (lambda (selection)
                      (browse-url (concat "https://developer.apple.com"
                                          (get-text-property 0 'url selection))))
            :dynamic-collection t
            :caller 'ar/counsel-apple-search))

(defun ar/counsel-hacking-with-swift-search ()
  "Ivy interface to query hackingwithswift.com."
  (interactive)
  (require 'request)
  (require 'json)
  (require 'url-http)
  (ivy-read "hacking with swift: "
            (lambda (input)
              (or
               (ivy-more-chars)
               (let ((request-curl-options (list "-H" (string-trim (url-http-user-agent-string)))))
                 (request
                   "https://www.hackingwithswift.com/example-code/search"
                   :type "GET"
                   :params (list
                            (cons "search" input))
                   :parser 'json-read
                   :success (cl-function
                             (lambda (&key data &allow-other-keys)
                               (ivy-update-candidates
                                (mapcar (lambda (item)
                                          (let-alist item
                                            (propertize .title 'url .url)))
                                        data)))))
                 0)))
            :action (lambda (selection)
                      (browse-url (concat "https://www.hackingwithswift.com"
                                          (get-text-property 0 'url selection))))
            :dynamic-collection t
            :caller 'ar/counsel-hacking-with-swift-search))

(defun swift-additions:functions-and-pragmas ()
  "Show swift file compressed functions and pragmas."
   (interactive)
  (let ((list-matching-lines-face nil))
    (occur "\\(func\\)\\|\\(#pragma mark\\)\\|\\(MARK:\\)")))

(defun swift-additions:print-thing-at-point ()
  "Print thing at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (end-of-line)
    (newline-and-indent)
    (insert (format "print(\"%s:\ \\(%s\)\")" word word))))

(defun swift-additions:insert-mark ()
  "Insert a mark at line."
  (interactive)
  (insert "// MARK: - ")
  (move-end-of-line nil))

(defun swift-additions:insert-todo ()
  "Insert a Todo."
  (interactive)
  (indent-for-tab-command)
  (insert "// TODO: ")
  (move-end-of-line nil))

(defun mk/toggle-flycheck-errors ()
  "Function to toggle flycheck errors."
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
    (list-flycheck-errors)))

(provide 'swift-additions)

;;; swift-additions.el ends here


