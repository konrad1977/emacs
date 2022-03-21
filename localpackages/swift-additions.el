;;; swift-additions.el -*- lexical-binding: t; -*-

;;; code:

(require 'cl-lib)
(require 'projectile)

(defgroup swift-additions:xcodebuild nil
  "REPL."
  :tag "swift-additions:xcodebuild"
  :group 'swift-additions)

(defcustom swift-additions:xcode-workspace "SecoTools"
  "Current xcode workspace."
  :type 'string
  :group 'swift-additions
  :safe 'stringp)

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

(defun swift-additions:add-mark ()
  "Insert a mark at line."
  (interactive)
  (end-of-line)
  (newline)
  (insert "// MARK: - ")
  (move-end-of-line nil))

(defun swift-additions:insert-todo ()
  "Insert a Todo."
  (interactive)
  (newline-and-indent)
  (indent-for-tab-command)
  (insert "// TODO ")
  (move-end-of-line nil))
  
(defvar swift-additions:install-folder
  "build/Build/Products/Debug-iphonesimulator/")

(defun build-and-run-command (simulator-id)
  "Xcodebuild with simulator id (as SIMULATOR-ID)."
      (concat
       "env /usr/bin/arch -x86_64 \\"
       "xcrun xcodebuild \\"
       (format "-scheme %s \\" swift-additions:xcode-scheme)
       (format "-workspace %s.xcworkspace \\" swift-additions:xcode-workspace)
       "-configuration Debug \\"
       "-sdk iphonesimulator \\"
       (format "-destination 'platform=iOS Simulator,id=%s' \\" simulator-id)
       "-derivedDataPath \\"
       "build | xcbeautify \n"
       (format "xcrun simctl install %s %s%s.app\n" swift-additions:simulator-id swift-additions:install-folder swift-additions:xcode-scheme)
       (format "xcrun simctl launch %s %s" swift-additions:simulator-id swift-additions:app-identifier)))

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

(defun swift-additions:build-and-run-ios-app ()
  "Build project using xcodebuild."
  (interactive)
  (save-some-buffers t)
  (swift-additions:terminate-app-in-simulator)
  (let ((buffer "*xcodebuild*"))
    (with-output-to-temp-buffer buffer
      (let ((default-directory (projectile-project-root)))
        (async-shell-command
         (format "bash -c %s"
                 (shell-quote-argument (build-and-run-command swift-additions:simulator-id))) buffer)
        (pop-to-buffer buffer)))))

(defun swift-additions:xcode-build()
  "Start a build using Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'")
  (message "Build project using Xcode..."))

(defun swift-additions:xcode-stop()
  "Stop application from Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'end tell'")
  (message "Stopping simulator ..."))

(defun swift-additions:xcode-run()
  "Run application from Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'")
  (message "Run project using Xcode..."))

(defun swift-additions:xcode-test()
  "Run current test scheme from Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'")
  (message "Test project using Xcode..."))

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

(defun mk/toggle-flycheck-errors ()
  "Function to toggle flycheck errors."
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (kill-buffer "*Flycheck errors*")
    (list-flycheck-errors)))

(provide 'swift-additions)

;;; swift-additions.el ends here


