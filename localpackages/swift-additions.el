;;; swift-additions.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; commentary:

;; package for building and runnning ios/macos apps from emacs

;;; code:

(require 'flycheck)
(require 'projectile)
(require 'periphery-helper)
(require 'periphery)
(require 'ios-simulator)

(defgroup swift-additions:xcodebuild nil
  "REPL."
  :tag "swift-additions:xcodebuild"
  :group 'swift-additions)

(defconst xcodebuild-buffer "*xcodebuild*")
(defconst periphery-command "periphery scan")
(defconst notifier-command "terminal-notifier -sender \"org.gnu.Emacs\" -ignoreDnd")
(defconst xcodebuild-list-config-command "xcrun xcodebuild -list -json")
(defconst list-simulators-command "xcrun simctl list devices iPhone available -j")

(defvar current-language-selection "en-EN")
(defvar current-xcode-scheme nil)
(defvar current-app-identifier nil)
(defvar current-project-root nil)
(defvar current-build-configuration nil)
(defvar current-environment-x86 t)
(defvar current-simulator-id nil)
(defvar secondary-simulator-id nil)
(defvar current-simulator-name nil)
(defvar current-buildconfiguration-json-data nil)
(defvar local-device-id nil)
(defvar DEBUG t)

(defun swift-additions:fetch-or-load-xcode-scheme ()
  "Get the xcode scheme if set otherwuse prompt user."
  (unless current-xcode-scheme
    (setq current-xcode-scheme (swift-additions:build-menu :title "Choose a scheme" :list (swift-additions:get-scheme-list))))
  current-xcode-scheme)

(defun swift-additions:fetch-or-load-build-configuration ()
  "Get the build configuration or promp user."
  (unless current-build-configuration
    (setq current-build-configuration (swift-additions:build-menu :title "Choose a configuration" :list (swift-additions:get-configuration-list))))
  current-build-configuration)

(defun swift-additions:fetch-or-load-app-identifier ()
  "Get the app identifier for the current configiration."
  (unless current-app-identifier
    (setq current-app-identifier (swift-additions:get-bundle-identifier (swift-additions:fetch-or-load-build-configuration))))
  current-app-identifier)

(defun swift-additions:setup-current-project (project)
  "Check if we have a new project (as PROJECT).  If true reset settings."
  (unless current-project-root
    (setq current-project-root project))
  (if (not
       (string= current-project-root project))
      (progn
        (swift-additions:reset-settings)
        (setq current-project-root project))))

(defun swift-additions:xcodebuild-command ()
  "Use x86 environement."
  (if current-environment-x86
      "env /usr/bin/arch -x86_64 xcrun xcodebuild build \\"
    "xcrun xcodebuild build\\"))

(defun swift-additions:get-build-folder ()
  "Fetch build folder."
  (let ((config (swift-additions:fetch-or-load-build-configuration)))
    (if local-device-id
        (format "build/Build/Products/%s-iphoneos/" config)
      (format "build/Build/Products/%s-iphonesimulator/" config))))

(defun swift-additions:get-number-of-cores ()
  "Fetch number of available cores."
  (if-let ((cores (replace-regexp-in-string "\n$" "" (shell-command-to-string "sysctl -n hw.ncpu"))))
      cores
    2))

(defun swift-additions:get-workspace-or-project ()
  "Check if there is workspace or project."
  (let ((workspace (swift-additions:workspace-name))
        (projectname (swift-additions:project-name)))
    (if workspace
        (format "-workspace %s.xcworkspace \\" workspace)
      (format "-project %s.xcodeproj \\" projectname))))

(cl-defun build-app-command (&simulatorId simulatorId)
  "Xcodebuild with (as SIMULATORID)."
  (concat
   (swift-additions:xcodebuild-command)
   (swift-additions:get-workspace-or-project)
   (format "-scheme '%s' \\" (swift-additions:fetch-or-load-xcode-scheme))
   (format "-configuration %s \\" (swift-additions:fetch-or-load-build-configuration))
   (format "-jobs %s \\" (swift-additions:get-number-of-cores))
   (format "-sdk %s \\" (swift-additions:get-current-sdk))
   (if simulatorId
       (format "-destination 'generic/platform=iOS Simulator,id=%s' \\" simulatorId)
     (format "-destination 'generic/platform=iOS' \\" ))
   "-UseModernBuildSystem=YES \\"
   "-destination-timeout 1 \\"
   "-scmProvider system \\"
   "-skipUnavailableActions \\"
   "-parallelizeTargets \\"
   "-hideShellScriptEnvironment \\"
   "-packageCachePath ~/Library/Cache/com.apple.swiftpm \\"
   "-quiet \\"
   ;; (format "-derivedDataPath %s \\" (swift-additions:full-build-folder))
   "-derivedDataPath \\"
   "build"))

(defun swift-additions:format-device-id (id)
  "Format device id (as ID)."
  (if id
      (if (not
           (string-match-p (regexp-quote "-") id))
          (concat (substring id 0 8) "-" (substring id 6))
        id)))

(defun swift-additions:full-build-folder ()
  "Full path to to the build folder."
  (let* ((folder (swift-additions:get-build-folder))
         (default-directory (concat current-project-root folder)))
    default-directory))

(defun swift-additions:copy-symbols-for-lsp ()
  "Copy symbols for LSP to work."
  (let* ((default-directory (swift-additions:full-build-folder))
         (command "rsync -avu --delete  . ../../../../.build/arm64-apple-macosx/debug"))
    (async-shell-command-to-string
           :process-name "Copying symbols"
           :command command
           :callback
           (lambda (txt)))))

(defun swift-additions:run-app()
  "Run app.  Either in simulator or on physical."
  (if local-device-id
      (swift-additions:install-app-on-device)
    (ios-simulator:install-and-run-app
     :rootfolder current-project-root
     :build-folder (swift-additions:get-build-folder)
     :simulatorId (ios-simulator:load-simulator-id)
     :appIdentifier (swift-additions:fetch-or-load-app-identifier)
     :buffer xcodebuild-buffer)))

(defun swift-additions:check-for-errors (text &optional callback)
  "Run periphery parser on TEXT (optional CALLBACK)."
  (if (or
       (string-match-p (regexp-quote "BUILD FAILED") text)
       (string-match-p (regexp-quote "error: ") text)
       (string-match-p (regexp-quote "warning: ") text))
      (progn
        (periphery-run-parser text)
        (when (not (string-match-p (regexp-quote "error: ") text))
          (if-let ((callback callback))
              (funcall callback))))
    (if-let ((callback callback))
        (funcall callback))))

(defun swift-additions:filename-by-extension (extension)
  "Get filename based on (as EXTENSION)."
  (let* ((name (directory-files current-project-root t extension)))
    (file-name-sans-extension (file-name-nondirectory (car name)))))

(defun swift-additions:project-name ()
  "Get project name."
  (swift-additions:filename-by-extension ".xcodeproj"))

(defun swift-additions:workspace-name ()
  "Get workspace name."
  (swift-additions:filename-by-extension ".xcworkspace"))

(cl-defun swift-additions:get-files-from (&key directory &key extension &key exclude)
  "Get files from DIRECTORY by EXTENSION and EXCLUDE."
  (let ((result '()))
    (mapcar (lambda (x)
              (cond
               ((not (string-match-p exclude (expand-file-name x directory)))
                (push x result))))
            (directory-files-recursively directory (format "\\%s$" extension) t))
    result))

(cl-defun find-project-root-folder-with (&key extension)
  "Find project folder where it has its project files EXTENSION."
  (let* ((project-root (expand-file-name (projectile-project-root)))
         (root (directory-files project-root nil (format "\\%s$" extension) 1))
            (subroot (swift-additions:get-files-from :directory project-root :extension extension :exclude ".build"))
            (workroot (or root subroot))
            (path (file-name-directory (car-safe workroot))))
    (if (and path (string-match-p (regexp-quote ".xcodeproj") path))
        (file-name-directory (directory-file-name path))
      path)))

(defun swift-additions:get-ios-project-root ()
  "Get the current root of the project."
  (let* ((workspace (find-project-root-folder-with :extension ".xcworkspace"))
         (xcodeproj (find-project-root-folder-with :extension ".xcodeproj")))
    (or workspace xcodeproj (expand-file-name (projectile-project-root)))))

(defun get-connected-device-id ()
  "Get the id of the connected device."
  (let ((device-id
         (clean-up-newlines
          (shell-command-to-string "system_profiler SPUSBDataType | sed -n -E -e '/(iPhone|iPad)/,/Serial/s/ *Serial Number: *(.+)/\\1/p'"))))
    (if (= (length device-id) 0)
        nil
      (swift-additions:format-device-id device-id))))

(defun swift-additions:get-current-sdk ()
  "Return the current SDK."
  (if local-device-id
      "iphoneos"
    "iphonesimulator"))

(defun swift-additions:install-app-on-device ()
  "Install an app on device."
  (when DEBUG (message (concat "Buildpath:" (swift-additions:get-build-folder))))

  (let* ((folder (swift-additions:get-build-folder))
         (app-name (ios-simulator:app-name-from :folder folder))
         (default-directory (concat current-project-root folder)))
    (message-with-color :tag "[Installing]" :text (format "%s onto physical device. Will launch app when done." app-name) :attributes 'warning)
    (run-async-command-in-buffer :command (format "ios-deploy -b %s.app -d" app-name))))

;;;###autoload
(defun build-using-compilation-mode ()
  "Build using builtin compile and 'compilation-mode'."
  (interactive)
  (let* ((default-directory (current-project-root))
         (compile-command (build-app-command :simulatorId (ios-simulator:load-simulator-id))))
    (compile compile-command)))

;;;###autoload
(defun swift-additions:reset-settings ()
  "Reset current settings.  Change current configuration."
  (interactive)
  (setq current-language-selection nil)
  (setq current-xcode-scheme nil)
  (setq current-app-identifier nil)
  (setq current-project-root nil)
  (setq current-build-configuration nil)
  (setq current-simulator-id nil)
  (setq current-simulator-name nil)
  (setq current-buildconfiguration-json-data nil)
  (setq local-device-id nil)
  (message-with-color :tag "[Resetting]" :text "Build configiration" :attributes 'warning))

(defun swift-additions:successful-build ()
  "Show that the build was successful."
  (swift-additions:copy-symbols-for-lsp)
  (message-with-color :tag "[Build]" :text "Successful" :attributes 'success))

;;;###autoload
(defun swift-additions:run-without-compiling ()
  "Run app in simulator/device without compiling."
  (interactive)
  (periphery-kill-buffer)
  (swift-additions:kill-xcode-buffer)
  (swift-additions:run-app))

;;;###autoload
(defun swift-additions:compile-and-run-app ()
  "Compile and run app."
  (interactive)
  (swift-additions:compile-and-run-silent t))

;;;###autoload
(defun swift-additions:compile-app ()
  "Compile app."
  (interactive)
  (swift-additions:compile-and-run-silent nil))

(defun swift-additions:compile-and-run-silent (runApp)
  "Build project using xcodebuild (as RUNAPP)."
  (save-some-buffers t)
  (periphery-kill-buffer)
  (swift-additions:kill-xcode-buffer)
  (ios-simulator:load-simulator-id)
  (setq device-or-simulator "[Building simulator target]")

  (if (swift-additions:is-xcodeproject)
      (progn
        (swift-additions:setup-current-project (swift-additions:get-ios-project-root))
        (let ((default-directory current-project-root))
          (async-shell-command-to-string
           :process-name "periphery"
           :command (build-app-command :simulatorId: current-simulator-id)
           :callback
           (lambda (text)
             (swift-additions:copy-symbols-for-lsp)
             (if runApp
                 (swift-additions:check-for-errors text #'swift-additions:run-app)
               (swift-additions:check-for-errors text #'swift-additions:successful-build)))))
        (animate-message-with-color
         :tag device-or-simulator
         :text (format "%s. Please wait. Patience is a virtue!" current-xcode-scheme)
         :attributes 'warning
         :times 2))
    (if (swift-additions:is-a-swift-package-base-project)
        (swift-additions:build-swift-package)
      (message "Not xcodeproject nor swift package"))))

;;;###autoload
(defun swift-additions:test-module-silent ()
  "Test module."
  (interactive)
  (save-some-buffers t)
  (periphery-kill-buffer)
  (swift-additions:kill-xcode-buffer)
  (swift-additions:test-swift-package))

;;;###autoload
(defun swift-additions:clean-build-folder ()
  "Clean app build folder."
  (interactive)
  (if (swift-additions:is-a-swift-package-base-project)
      (swift-additions:clean-build-folder-with (projectile-project-root) ".build" "swift package")
    (swift-additions:clean-build-folder-with (swift-additions:get-ios-project-root) "build" current-xcode-scheme)))

(defun swift-additions:clean-build-folder-with (projectRoot buildFolder projectName)
  "Clean build folder with PROJECTROOT BUILDFOLDER and PROJECTNAME."
  (message-with-color :tag "[Cleaning]" :text (format "Build folder for %s Standby..." projectName) :attributes '(:inherit warning))
  (let ((default-directory (concat projectRoot buildFolder)))
    (if (file-directory-p default-directory)
        (progn
          (message-with-color :tag "[Removing]" :text (format "Folder for %s" default-directory) :attributes '(:inherit warning))
          (delete-directory default-directory t nil))
      (message-with-color :tag "[Failed]" :text (format "Build folder %s doesn't exist" default-directory) :attributes '(:inherit error))))
  (message-with-color :tag "[Done]" :text "Ready to rumble." :attributes '(:inherit success)))

(defun swift-additions:buffer-contains-substring (string)
  "Check if buffer contain (as STRING)."
  (save-excursion
    (save-match-data
      (goto-char (point-max))
      (search-backward string nil t))))

(defun swift-additions:insert-text-and-go-to-eol (text)
  "Function that that insert (as TEXT) and go to end of line."
  (save-excursion
    (indent-for-tab-command)
    (insert text)
    (move-end-of-line nil))
  (goto-char (point-at-eol))
  (evil-insert-state t))

;;;###autoload
(defun swift-additions:functions-and-pragmas ()
  "Show swift file compressed functions and pragmas."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "\\(#pragma mark\\)\\|\\(MARK:\\)")))

;;;###autoload
(defun swift-additions:print-thing-at-point ()
  "Print thing at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (end-of-line)
    (newline-and-indent)
    (insert (format "debugPrint(\"%s: \ \\(%s\)\")" word word))))

;;;###autoload
(defun swift-additions:insert-mark ()
  "Insert a mark at line."
  (interactive)
  (swift-additions:insert-text-and-go-to-eol "// MARK: - "))

;;;###autoload
(defun swift-additions:insert-todo ()
  "Insert a Todo."
  (interactive)
  (swift-additions:insert-text-and-go-to-eol "// TODO: "))

;;;###autoload
(defun swift-additions:toggle-xcodebuild-buffer ()
  "Function to toggle xcodebuild-buffer."
  (interactive)
  (if (get-buffer xcodebuild-buffer)
      (bury-buffer xcodebuild-buffer)
    (pop-to-buffer xcodebuild-buffer)))

(defun swift-additions:get-bundle-identifier (config)
  "Get bundle identifier (as CONFIG)."
  (unless current-project-root
    (setq current-project-root (swift-additions:get-ios-project-root)))
  
  (let* ((default-directory current-project-root)
         (json (call-process-to-json "xcrun" "xcodebuild" "-showBuildSettings" "-configuration" config "-json")))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_BUNDLE_IDENTIFIER)))

(defun swift-additions:get-buildconfiguration-json ()
  "Return a cached version or load the build configuration."
  (unless current-buildconfiguration-json-data
    (setq current-buildconfiguration-json-data (call-process-to-json xcodebuild-list-config-command)))
  current-buildconfiguration-json-data)

(defun swift-additions:get-target-list ()
  "Get list of project targets."
  (unless current-project-root
    (setq current-project-root (swift-additions:get-ios-project-root))
    (message-with-color :tag "[Fetching]" :text "app targets.." :attributes '(:inherit warning)))

  (let* ((default-directory current-project-root)
         (json (swift-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (targets (cdr (assoc 'targets project))))
    targets))

(defun swift-additions:get-scheme-list ()
  "Get list of project schemes."
  (unless current-project-root
    (setq current-project-root (swift-additions:get-ios-project-root))
    (message-with-color :tag "[Fetching]" :text "build schemes.." :attributes '(:inherit warning)))

  (let* ((default-directory current-project-root)
         (json (swift-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (result (cdr (assoc 'schemes project))))
    result))

(defun swift-additions:get-configuration-list ()
  "Get list of project configurations."

  (unless current-project-root
    (setq current-project-root (swift-additions:get-ios-project-root))
    (message-with-color :tag "[Fetching]" :text "build configurations.." :attributes '(:inherit warning)))
  
  (let* ((default-directory current-project-root)
         (json (swift-additions:get-buildconfiguration-json))
         (project (assoc 'project json))
         (result (cdr (assoc 'configurations project))))
    result))

(cl-defun swift-additions:build-menu (&key title &key list)
  "Builds a widget menu from (as TITLE as LIST)."
  (if (<= (length list) 1)
      (elt list 0)
    (progn
      (let* ((choices (seq-map (lambda (item) (cons item item)) list))
             (choice (completing-read title choices)))
        (cdr (assoc choice choices))))))

(defun swift-additions:kill-xcode-buffer ()
  "Kill the xcode buffer."
  (when (get-buffer xcodebuild-buffer)
    (kill-buffer xcodebuild-buffer)))

(defun swift-additions:is-xcodeproject ()
  "Check if its an xcode-project."
  (if-let ((default-directory (swift-additions:get-ios-project-root)))
      (or
       (directory-files-recursively default-directory "\\xcworkspace$" t)
       (directory-files-recursively default-directory "\\xcodeproj$" t))))

(defun swift-additions:is-a-swift-package-base-project ()
  "Check if project is a swift package based."
  (let ((default-directory (projectile-project-root)))
    (file-exists-p "Package.swift")))

(defun swift-additions:check-for-spm-build-errors (text)
  "Check for Swift package build erros in TEXT."
  (when DEBUG (message text))
  (if (or
       (string-match-p (regexp-quote "error:") text)
       (string-match-p (regexp-quote "warning:") text))
      (progn
        (periphery-run-parser text)
        (when (not (string-match-p (regexp-quote "error:") text))
          (shell-command "swift run" xcodebuild-buffer)))
    (shell-command "swift run" xcodebuild-buffer)))

;;;###autoload
(defun swift-additions:build-swift-package ()
  "Build swift package module."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (async-shell-command-to-string :process-name "periphery" :command "swift build" :callback #'swift-additions:check-for-spm-build-errors)
    (animate-message-with-color :tag "[Building Package]" :text (format "%s. Please wait. Patience is a virtue!" (projectile-project-root)) :attributes 'warning :times 5)))

;;;###autoload
(defun swift-additions:test-swift-package ()
  "Test swift package module."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (async-shell-command-to-string :process-name "periphery" :command "swift test" :callback #'swift-additions:check-for-spm-build-errors)
    (message-with-color :tag "[Testing Package]" :text (format "%s. Please wait. Patience is a virtue!" (projectile-project-root)) :attributes 'warning)))

(defun swift-additions:lsp-arguments ()
  "Get the lsp arguments to support UIKit."
  (let* ((sdk (ios-simulator:sdk-path))
         (target (ios-simulator:target)))
    (list
     "-Xswiftc" "-sdk"
     "-Xswiftc" sdk
     "-Xswiftc" "-target"
     "-Xswiftc" target)))

;;;###autoload
(defun my-swift-mode:eglot-server-contact (_ignored)
  "Construct the list that eglot needs to start sourcekit-lsp."
  (setq arglist (swift-additions:lsp-arguments))
  (add-to-list 'arglist
               (clean-up-newlines (shell-command-to-string "xcrun --find sourcekit-lsp"))))

;; Taken from  https://gitlab.com/woolsweater/dotemacs.d/-/blob/main/modules/my-swift-mode.el
;;;###autoload
(defun swift-additions:split-func-list ()
  "While on either the header of a function-like declaration or a call to a function, split each parameter/argument to its own line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (condition-case nil
        (atomic-change-group
          (search-forward "(")
          (let ((end))
            (while (not end)
              (newline-and-indent)
              (let ((parens 0)
                    (angles 0)
                    (squares 0)
                    (curlies 0)
                    (comma))
                (while (not (or comma end))
                  (re-search-forward
                   (rx (or ?\( ?\) ?< ?> ?\[ ?\] ?{ ?} ?\" ?,))
                   (line-end-position))
                  (pcase (match-string 0)
                    ("(" (cl-incf parens))
                    (")" (if (> parens 0)
                             (cl-decf parens)
                           (backward-char)
                           (newline-and-indent)
                           (setq end t)))
                    ;; Note; these could be operators in an expression;
                    ;; there's no obvious way to fully handle that.
                    ("<" (cl-incf angles))
                    ;; At a minimum we can skip greater-than and func arrows
                    (">" (unless (zerop angles)
                           (cl-decf angles)))
                    ("[" (cl-incf squares))
                    ("]" (cl-decf squares))
                    ("{" (cl-incf curlies))
                    ("}" (cl-decf curlies))
                    ("\"" (let ((string-end))
                            (while (not string-end)
                              (re-search-forward (rx (or ?\" (seq ?\\ ?\")))
                                                 (line-end-position))
                              (setq string-end (equal (match-string 0) "\"")))))
                    ("," (when (and (zerop parens) (zerop angles)
                                    (zerop squares) (zerop curlies))
                           (setq comma t)))))))))
      (error (user-error "Cannot parse function decl or call here")))))

(defface tree-sitter-hl-face:repeat
  '((t :inherit tree-sitter-hl-face:keyword
       :foreground "#666bb2"))
  "Face for loops (for, in etc)."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:parameter
  '((t :inherit tree-sitter-hl-face:label
       :foreground "#666bb2"))
  "Face for parameters in function calls."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:conditional
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Face for if/switch."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:include
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Import/include face."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:boolean
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Face for true/false."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.return
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Face for 'return'."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.operator
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Face for operator."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.function
  '((t :inherit tree-sitter-hl-face:property
       :foreground "#666bb2"))
  "Face for 'func' keyword."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:case-pattern
  '((t :inherit tree-sitter-hl-face:property))
  "Face for enum case names in a pattern match."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comment.special
  '((t :inherit tree-sitter-hl-face:comment
       :weight semi-bold))
  "Face for comments with some markup-like meaning, like MARK."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:operator.special
  '((t :inherit font-lock-negation-char-face
       :weight semi-bold))
  "Face for operators that need to stand out, like unary negation."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:punctuation.type
  '((t :inherit tree-sitter-hl-face:type
       :weight normal))
  "Face for punctuation in type names (?, [], etc.)."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:keyword.compiler
  '((t :inherit tree-sitter-hl-face:keyword
       :weight semi-bold))
  "Face for compile-time keywords."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:variable.synthesized
  '((t :inherit tree-sitter-hl-face:variable))
  "Face for compiler-synthesized identifiers (prefixed with '$')."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:default
  '((t :inherit default))
  "Face to override other faces."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:case-pattern
  '((t :inherit tree-sitter-hl-face:property))
  "Face for enum case names in a pattern match."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comment.special
  '((t :inherit tree-sitter-hl-face:comment
       :weight semi-bold))
  "Face for comments with some markup-like meaning, like MARK."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:operator.special
  '((t :inherit font-lock-negation-char-face
       :weight semi-bold))
  "Face for operators that need to stand out, like unary negation."
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:punctuation.type
  '((t :inherit tree-sitter-hl-face:type
       :weight normal))
  "Face for punctuation in type names (?, [], etc.)."
  :group 'tree-sitter-hl-faces)

(provide 'swift-additions)

;;; swift-additions.el ends here
