;;; Swift-additions.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Package for building and running iOS/macOS apps from Emacs

;;; Code:

(require 'ios-device)
(require 'ios-simulator)

(defgroup swift-additions nil
  "Swift development tools and utilities for Emacs."
  :group 'programming
  :prefix "swift-additions")

(defcustom swift-additions:debug nil
  "Enable debug mode for swift additions."
  :type 'boolean
  :group 'swift-additions)

(defcustom swift-additions:default-configuration "Debug"
  "Default build configuration to use."
  :type 'string
  :group 'swift-additions)

(defcustom swift-additions:modern-build-system "YES"
  "Whether to use the modern build system."
  :type '(choice (const "YES")
                 (const "NO"))
  :group 'swift-additions)

(defcustom swift-additions:additional-build-flags '()
  "Additional flags to pass to xcodebuild."
  :type '(repeat string)
  :group 'swift-additions)

(defcustom swift-additions:other-swift-flags '
  (
   "-enable-batch-mode"
   "-no-whole-module-optimization"
   ;; "-disable-typecheck-performance-warnings"
   )
  "Additional flags to pass to the Swift compiler (OTHER_SWIFT_FLAGS)."
  :type '(repeat string)
  :group 'swift-additions)

(defvar swift-additions:optimization-level 'fast
  "Build optimization level.")

;; Internal variables
(defvar swift-additions:current-environment-x86 nil)
(defvar swift-additions:current-build-command nil)
(defvar swift-additions:build-progress-spinner nil)
(defvar swift-additions:compilation-time nil)

(defun swift-additions:log-debug (format-string &rest args)
  "Log debug message using FORMAT-STRING and ARGS when debug is enabled."
  (when swift-additions:debug
    (apply #'message (concat "[Swift Debug] " format-string) args)))

(defun swift-additions:handle-build-error (error-message)
  "Handle build ERROR-MESSAGE and display appropriate feedback."
  (swift-additions:cleanup)
  (mode-line-hud:update
   :message (format "Build failed: %s"
                   (propertize (truncate-string-to-width error-message 50) 'face 'error)))
  (periphery-run-parser error-message))

(defun swift-additions:reset ()
  "Reset build settings and clear all cached state."
  (interactive)
  (xcode-additions:reset)
  (ios-device:reset)
  (setq swift-additions:current-build-command nil
        swift-additions:current-environment-x86 nil
        swift-additions:build-progress-spinner nil
        swift-additions:compilation-time nil
        run-once-compiled nil))

(defun swift-additions:get-optimization-flags ()
  "Get optimization flags based on current optimization level."
  `(
    ,(format "OTHER_SWIFT_FLAGS=\"%s\""
             (mapconcat (lambda (flag) (concat "-Xfrontend " flag))
                       swift-additions:other-swift-flags " "))
    "SWIFT_BUILD_CACHE_ENABLED=YES"  ; Explicitly enable Swift build cache
    ;; "DEBUG_INFORMATION_FORMAT=dwarf"
    "SWIFT_PARALLEL_MODULE_JOBS=$(sysctl -n hw.ncpu)"  ; Use all available cores
    ;; "SWIFT_USE_PARALLEL_WHOLE_MODULE_OPTIMIZATION=YES"
    "SWIFT_ENABLE_BATCH_MODE=YES"
    "SWIFT_USE_PARALLEL_SOURCEJOB_TASKS=YES"
    ;; "GCC_OPTIMIZATION_LEVEL=0"
    ;; "SWIFT_TREAT_WARNINGS_AS_ERRORS=NO"
    ;; "ONLY_ACTIVE_ARCH=YES"
    ;; "VALIDATE_PRODUCT=NO"
    "CLANG_ENABLE_MODULES=YES"
    "SWIFT_CACHE_COMPILE_JOB=YES"    ; Cache compilation jobs
    "SWIFT_DEPENDENCY_CACHE_ENABLED=YES"
    "SWIFT_USE_DEVELOPER_MODE=YES"   ; Faster development cycles
    ;; "SWIFT_EMIT_LOCALIZED_STRINGS=NO" ; Disable localized strings processing
    "DERIVED_DATA_CACHE_VERSION=5"   ; More aggressive caching
    ;; "ENABLE_PRECOMPILED_HEADERS=YES" ; Enable precompiled headers
    ;; "CLANG_USE_OPTIMIZATION_PROFILE=YES"
    ;; "COMPILER_INDEX_STORE_ENABLE=NO" ; Disable indexing during build
    ;; "ENABLE_BITCODE=NO"              ; Disable bitcode generation
    "CODE_SIGNING_REQUIRED=NO"       ; Skip code signing
    ;; "CODE_SIGN_IDENTITY=\"\""        ; Empty code sign identity
    ;; "ENABLE_TESTABILITY=NO"          ; Disable testability features unless testing
    "ENABLE_PREVIEWS=NO"             ; Disable SwiftUI previews during builds
    ;; "ASSETCATALOG_COMPILER_OPTIMIZATION=space"
    "SWIFT_COMPILATION_MODE=incremental"
    ;; "SWIFT_OPTIMIZATION_LEVEL=-Osize" ; Optimize for size not speed in debug
    ;; "SWIFT_ACTIVE_COMPILATION_CONDITIONS=DEBUG"
    ;; "COPY_PHASE_STRIP=NO"            ; Don't strip debug symbols
    "DEAD_CODE_STRIPPING=YES"        ; Remove unused code
    "SWIFT_ENFORCE_EXCLUSIVE_ACCESS=off" ; Disable memory access checks
    ;; "SWIFT_SUPPRESS_WARNINGS=YES"    ; Suppress warnings
    ;; "SWIFT_STRICT_CONCURRENCY=minimal" ; Minimal concurrency checking
    ;; "SWIFT_REFLECTION_METADATA_LEVEL=none" ; Disable reflection metadata
    "SWIFT_SERIALIZE_DEBUGGING_OPTIONS=NO" ; Don't serialize debug options
    "SWIFT_WHOLE_MODULE_OPTIMIZATION=NO" ; Better for incremental builds
    ;; "SWIFT_DISABLE_SAFETY_CHECKS=YES" ; Disable safety checks
    "SWIFT_DISABLE_MODULE_CACHE_VALIDATION=YES" ; Skip module cache validation
    ))

(defun swift-additions:xcodebuild-command ()
"Use x86 environement."
(if swift-additions:current-environment-x86
    "env /usr/bin/arch -x86_64 xcrun xcodebuild build \\"
  "xcrun xcodebuild build \\"))

(defun swift-additions:get-optimal-jobs ()
  "Get get number of CPUs."
  (number-to-string (max 1 (num-processors))))

(defun swift-additions:setup-build-environment ()
  "Setup optimal environment variables for build with aggressive caching and parallelization."
  ;; Enable all available caching mechanisms
  (setenv "SWIFT_BUILD_CACHE_ENABLED" "1")
  (setenv "CLANG_MODULE_CACHE_PATH" (expand-file-name "~/Library/Caches/org.swift.swiftpm/ModuleCache"))
  (setenv "SWIFT_MODULE_CACHE_PATH" (expand-file-name "~/Library/Caches/org.swift.swiftpm/ModuleCache"))
  (setenv "OBJC_DISABLE_INITIALIZE_FORK_SAFETY" "YES")  ;; Speeds up forking
  (setenv "CLANG_ENABLE_MODULES" "YES") ;; Precompiled headers
  (setenv "SWIFT_USE_PRECOMPILED_HEADERS" "YES")
  (setenv "GCC_PRECOMPILE_PREFIX_HEADER" "YES")
  (setenv "XCODE_BUILD_ENABLE_DEPENDENCIES" "YES") ;; Better dependency tracking
  
  ;; Disable index-while-building for faster builds
  (setenv "COMPILER_INDEX_STORE_ENABLE" "NO")
  (setenv "INDEX_ENABLE_BUILD_ARENA" "NO")
  (setenv "INDEX_ENABLE_DATA_STORE" "NO")
  
  ;; Disable code signing during development builds
  (setenv "CODE_SIGNING_REQUIRED" "NO")
  (setenv "CODE_SIGN_IDENTITY" "")
  (setenv "CODE_SIGNING_ALLOWED" "NO")

  ;; Architecture settings - force arm64 only
  (setenv "ONLY_ACTIVE_ARCH" "YES")
  (setenv "ARCHS" "arm64")
  (setenv "VALID_ARCHS" "arm64")
  (setenv "EXCLUDED_ARCHS" "i386 x86_64")
  (setenv "SUPPORTED_PLATFORMS" "iphonesimulator iphoneos")
  
  ;; Explicitly disable all sanitizers
  (setenv "ENABLE_ADDRESS_SANITIZER" "NO")
  (setenv "ENABLE_THREAD_SANITIZER" "NO")
  (setenv "ENABLE_UNDEFINED_BEHAVIOR_SANITIZER" "NO")

  ;; Parallel compilation settings - more aggressive
  (let ((cores (max 1 (num-processors))))
    (setenv "SWIFT_MAX_PARALLEL_LTO_JOBS" (number-to-string cores))
    (setenv "SWIFT_PARALLEL_MODULE_JOBS" (number-to-string cores))
    (setenv "SWIFT_PARALLEL_COMPILE_JOBS" (number-to-string cores))
    (setenv "LLVM_PARALLEL_LINK_JOBS" (number-to-string (/ cores 2)))
    (setenv "SWIFT_DRIVER_JOBS" (number-to-string (* cores 2))) ;; More aggressive parallelism
    ;; New: Use all cores for various tasks
    (setenv "GCC_PARALLEL_COMPILE_JOBS" (number-to-string cores))
    (setenv "CLANG_PARALLEL_COMPILE_JOBS" (number-to-string cores))
    (setenv "SWIFT_PARALLEL_SOURCEJOB_TASKS" (number-to-string cores)))

  ;; File system caching
  (setenv "XCODE_BUILD_SYSTEM_ENABLE_INCREMENTAL_DISTRIBUTED_CACHE" "YES")
  (setenv "XCODE_BUILD_SYSTEM_USE_WATCHERS" "YES")
  (setenv "XCODE_BUILD_SYSTEM_CACHE_DIR" (expand-file-name "~/Library/Developer/Xcode/DerivedData/ModuleCache"))
  
  ;; Shared cache for all projects
  (setenv "SHARED_PRECOMPS_DIR" (expand-file-name "~/Library/Developer/Xcode/DerivedData/SharedPrecompiledHeaders"))
  (setenv "OBJROOT" (expand-file-name "~/Library/Developer/Xcode/DerivedData/Build/Intermediates"))

  ;; Incremental compilation optimizations
  (setenv "SWIFT_ENABLE_INCREMENTAL_COMPILATION" "YES")
  (setenv "SWIFT_OPTIMIZATION_LEVEL" "-O")
  (setenv "SWIFT_COMPILATION_MODE" "incremental")
  (setenv "SWIFT_WHOLE_MODULE_OPTIMIZATION" "NO")  ;; Better for incremental
  
  ;; Disable bitcode for faster builds
  (setenv "ENABLE_BITCODE" "NO")
  (setenv "BITCODE_GENERATION_MODE" "none")

  ;; Disable expensive checks
  (setenv "SWIFT_DISABLE_SAFETY_CHECKS" "1")
  (setenv "SWIFT_DISABLE_MODULE_CACHE_VALIDATION" "1")
  ;; (setenv "SWIFT_SKIP_FUNCTION_BODIES" "1")  ; Breaks compilation
  ;; (setenv "SWIFT_SKIP_TYPE_CHECKING" "1")    ; Breaks compilation
  (setenv "SWIFT_SUPPRESS_WARNINGS" "YES")

  ;; Memory management
  (setenv "SWIFT_MEMORY_ALLOCATOR" "malloc")
  (setenv "SWIFT_DETERMINISTIC_HASHING" "1")
  
  ;; Increase memory limits for compiler processes
  (setenv "SWIFT_EXEC_MEMLIMIT" "8192")  ;; 8GB memory limit
  (setenv "SWIFT_OPTIMIZATION_LEVEL_Release" "-Osize")

  ;; Debugging optimizations
  (setenv "SWIFT_SERIALIZE_DEBUGGING_OPTIONS" "NO")
  (setenv "SWIFT_REFLECTION_METADATA_LEVEL" "none")
  ;; (setenv "DEBUG_INFORMATION_FORMAT" "dwarf")  ;; Faster than dwarf-with-dsym

  ;; Enable experimental features
  (setenv "SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY" "1")
  (setenv "SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED" "1")
  
  ;; Disable unnecessary validations
  (setenv "VALIDATE_PRODUCT" "NO")
  (setenv "VALIDATE_WORKSPACE" "NO"))

(cl-defun swift-additions:build-app-command (&key sim-id derived-path)
  "Generate optimized xcodebuild command with aggressive parallelization."
  (if swift-additions:current-build-command
      swift-additions:current-build-command
    (concat
     (swift-additions:xcodebuild-command)
     (format "%s \\" (xcode-additions:get-workspace-or-project))
     (format "-scheme %s \\" (xcode-additions:scheme))
     ;; "-skipPackageUpdates \\" ; Skip automatic package updates
     "-packageCachePath ~/Library/Caches/org.swift.packages \\" ; Shared package cache
     ;; "-disableAutomaticPackageResolution \\" ; Manual package resolution control
     "-parallelizeTargets -jobs $(sysctl -n hw.ncpu) \\" ; Dynamic core count
     (if sim-id
         (format "-destination 'generic/platform=iOS Simulator,id=%s' -sdk %s \\" sim-id "iphonesimulator")
       (format "-destination 'generic/platform=%s' -sdk %s \\" "iOS" "iphoneos"))
     (format "-configuration %s \\" swift-additions:default-configuration)
     "-hideShellScriptEnvironment \\"
     "-skipUnavailableActions \\"
     "-enableAddressSanitizer NO \\"
     "-enableThreadSanitizer NO \\"
     "-IDEBuildOperationMaxNumberOfConcurrentCompileTasks=$(sysctl -n hw.ncpu) \\"
     "-IDEBuildOperationMaxNumberOfConcurrentLinkTasks=$(sysctl -n hw.ncpu) \\"
     "-enableUndefinedBehaviorSanitizer NO \\"
     ;; (mapconcat (lambda (flag) (concat flag " \\"))
     ;;            (append
     ;;             (swift-additions:get-optimization-flags)
     ;;             swift-additions:additional-build-flags)
     ;;            "")
     "-derivedDataPath .build | xcode-build-server parse -avv")))

(defun swift-additions:enable-build-caching ()
  "Enable Xcode build system caching for faster incremental builds."
  (setenv "ENABLE_PRECOMPILED_HEADERS" "YES")
  ;; (setenv "CLANG_ENABLE_MODULE_DEBUGGING" "NO")
  (setenv "SWIFT_USE_DEVELOPMENT_SNAPSHOT" "NO")
  (setenv "SWIFT_USE_PRECOMPILED_HEADERS" "YES")
  (setenv "GCC_PRECOMPILE_PREFIX_HEADER" "YES")
  (setenv "GCC_USE_HEADER_SYMAP" "YES")
  (setenv "CLANG_USE_OPTIMIZATION_PROFILE" "YES")
  (setenv "SWIFT_USE_INCREMENTAL_COMPILATION" "YES")
  (setenv "SWIFT_COMPILER_INDEX_STORE_ENABLE" "NO") ;; Disable indexing during build
  (setenv "XCODE_BUILD_SYSTEM_FORCE_ENABLE_PCH_VALIDATION" "NO")
  (setenv "SWIFT_DRIVER_JOBS" (number-to-string (* (num-processors) 2))) ;; Double the CPU cores
  (setenv "XCODE_BUILD_SYSTEM_SHOW_ENV" (if swift-additions:debug "YES" "NO")) ;; Debug logging
  
  ;; Generate temporary xcconfig file with optimized settings
  (swift-additions:generate-fast-build-xcconfig))

(defun swift-additions:generate-fast-build-xcconfig ()
  "Generate a temporary xcconfig file with optimized build settings."
  (with-temp-file "/tmp/fast-build.xcconfig"
    (insert "// Temporary xcconfig for faster builds\n\n")
    
    ;; Optimization settings
    (insert "// Optimization settings\n")
    (insert (format "OTHER_SWIFT_FLAGS = %s\n"
                   (mapconcat (lambda (flag) (concat "-Xfrontend " flag))
                             swift-additions:other-swift-flags " ")))
    (insert "SWIFT_OPTIMIZATION_LEVEL = -Osize\n")
    (insert "GCC_OPTIMIZATION_LEVEL = 0\n")
    (insert "SWIFT_COMPILATION_MODE = incremental\n")
    (insert "SWIFT_WHOLE_MODULE_OPTIMIZATION = NO\n\n")
    
    ;; Caching settings
    (insert "// Caching settings\n")
    (insert "SWIFT_BUILD_CACHE_ENABLED = YES\n")
    (insert "SWIFT_DEPENDENCY_CACHE_ENABLED = YES\n")
    (insert "SWIFT_CACHE_COMPILE_JOB = YES\n")
    (insert "ENABLE_PRECOMPILED_HEADERS = YES\n\n")
    
    ;; Parallelization
    (insert "// Parallelization\n")
    (insert (format "SWIFT_PARALLEL_MODULE_JOBS = %d\n" (num-processors)))
    (insert (format "SWIFT_PARALLEL_COMPILE_JOBS = %d\n" (num-processors)))
    (insert "SWIFT_USE_PARALLEL_WHOLE_MODULE_OPTIMIZATION = YES\n")
    (insert "SWIFT_USE_PARALLEL_SOURCEJOB_TASKS = YES\n\n")
    
    ;; Architecture settings
    (insert "// Architecture settings\n")
    (insert "ONLY_ACTIVE_ARCH = YES\n")
    (insert "ARCHS = arm64\n")
    (insert "VALID_ARCHS = arm64\n")
    (insert "EXCLUDED_ARCHS = i386 x86_64\n\n")
    
    ;; Disable unnecessary features
    (insert "// Disable unnecessary features\n")
    (insert "COMPILER_INDEX_STORE_ENABLE = NO\n")
    (insert "ENABLE_BITCODE = NO\n")
    (insert "CODE_SIGNING_REQUIRED = NO\n")
    (insert "CODE_SIGN_IDENTITY = \"\"\n")
    (insert "ENABLE_TESTABILITY = NO\n")
    (insert "ENABLE_PREVIEWS = NO\n")
    (insert "ENABLE_ADDRESS_SANITIZER = NO\n")
    (insert "ENABLE_THREAD_SANITIZER = NO\n")
    (insert "ENABLE_UNDEFINED_BEHAVIOR_SANITIZER = NO\n")
    (insert "SWIFT_TREAT_WARNINGS_AS_ERRORS = NO\n")
    (insert "SWIFT_SUPPRESS_WARNINGS = YES\n")
    (insert "SWIFT_ENFORCE_EXCLUSIVE_ACCESS = off\n")
    (insert "SWIFT_STRICT_CONCURRENCY = minimal\n")
    (insert "SWIFT_REFLECTION_METADATA_LEVEL = none\n")
    (insert "SWIFT_SERIALIZE_DEBUGGING_OPTIONS = NO\n")
    (insert "SWIFT_DISABLE_SAFETY_CHECKS = YES\n")
    (insert "SWIFT_DISABLE_MODULE_CACHE_VALIDATION = YES\n")
    (insert "DEBUG_INFORMATION_FORMAT = dwarf\n")
    (insert "VALIDATE_PRODUCT = NO\n")
    (insert "VALIDATE_WORKSPACE = NO\n")
    (insert "DEAD_CODE_STRIPPING = YES\n")
    (insert "COPY_PHASE_STRIP = NO\n")
    (insert "ASSETCATALOG_COMPILER_OPTIMIZATION = space\n")
    (insert "ASSETCATALOG_COMPILER_SKIP_APP_ICON_PROCESSING = YES\n")))

(defun swift-additions:compilation-time ()
  "Get the time of the compilation."
  (if-let* ((end-time (current-time))
           (start-time swift-additions:compilation-time))
      (format "%.1f" (float-time (time-subtract end-time start-time)))
    "N/A"))

(defun swift-additions:run-app-on-device-after-build ()
  "Run app on device after build."
  (mode-line-hud:update :message (format "Built %s in %s seconds"
                                         (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                                         (propertize (swift-additions:compilation-time) 'face 'warning)))

  (ios-device:install-app
   :buildfolder (xcode-additions:build-folder :device-type :device)
   :appIdentifier (xcode-additions:fetch-or-load-app-identifier)))

(defun swift-additions:run-app-after-build()
  "Either in simulator or on physical."
  (mode-line-hud:update :message (format "Built %s in %s seconds"
                                         (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                                         (propertize (swift-additions:compilation-time) 'face 'warning)))

  (swift-additions:cleanup)
  (ios-simulator:install-and-run-app
   :rootfolder (xcode-additions:project-root)
   :build-folder (xcode-additions:build-folder :device-type :simulator)
   :simulatorId (ios-simulator:simulator-identifier)
   :appIdentifier (xcode-additions:fetch-or-load-app-identifier)))

(defun swift-additions:check-if-build-was-successful (input-text)
  "Check if INPUT-TEXT does not contain build failure indicators."
  (when swift-additions:debug (message input-text))
  (not (string-match-p "\\(BUILD FAILED\\|BUILD INTERRUPTED\\|xcodebuild: error\\)" input-text)))

(defun swift-additions:check-for-errors (output callback)
  "Run periphery parser on TEXT (optional as OUTPUT CALLBACK)."
  (swift-additions:log-debug "Checking for errors in output: %s" output)
  (condition-case err
      (when (swift-additions:check-if-build-was-successful output)
        (funcall callback))
    (error
     (swift-additions:handle-build-error (error-message-string err))))
  (periphery-run-parser output))

(defun swift-additions:cleanup ()
  "Cleanup resources and state."
  (when swift-additions:build-progress-spinner
    (spinner-stop swift-additions:build-progress-spinner))
  (setq swift-additions:current-build-command nil
        swift-additions:compilation-time nil))

(defun swift-additions:successful-build ()
  "Show that the build was successful."
  (mode-line-hud:update :message (format "Successful build %s"
                                         (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face))))

(cl-defun swift-additions:compile-with-progress (&key command callback update-callback)
  "Run compilation COMMAND with progress indicator and CALLBACK/UPDATE-CALLBACK in background.
Returns a cons cell (PROCESS . LOG-BUFFER) where LOG-BUFFER accumulates the build output."
  (spinner-start 'progress-bar-filled)
  (setq swift-additions:build-progress-spinner spinner-current
        swift-additions:compilation-time (current-time))

  (let* ((log-buffer (generate-new-buffer " *xcodebuild-log*"))
         (process (make-process
                  :name "xcodebuild-background"
                  :buffer log-buffer
                  :command (list shell-file-name shell-command-switch command)
                  :noquery t   ; Detach from Emacs process list
                  :sentinel (lambda (proc event)
                             (when (memq (process-status proc) '(exit signal))
                               (spinner-stop swift-additions:build-progress-spinner)
                               (let ((output (with-current-buffer log-buffer
                                              (buffer-string))))
                                 (if (swift-additions:check-if-build-was-successful output)
                                     (progn
                                       (funcall callback output)
                                       (swift-additions:cleanup))
                                   (swift-additions:handle-build-error output))
                                 (kill-buffer log-buffer)))))))
    
    ;; Configure process handling
    (set-process-query-on-exit-flag process nil)
    
    ;; Start async output processing
    (when update-callback
      (set-process-filter process
                         (lambda (proc string)
                           (with-current-buffer log-buffer
                             (goto-char (point-max))
                             (insert string))
                           (when (functionp update-callback)
                             (funcall update-callback string)))))

    (swift-additions:log-debug "Running command: %s" command)
    (cons process log-buffer)))

(cl-defun swift-additions:compile (&key run)
  "Build project using xcodebuild (as RUN)."
  (save-some-buffers t)

  (if (xcode-additions:is-xcodeproject)
      (progn
        (periphery-kill-buffer)
        (ios-simulator:kill-buffer)
        ;; Only ask for device/simulator if not already set
        (unless xcode-additions:device-choice
          (xcode-addition:ask-for-device-or-simulator))
        (if (not (xcode-additions:run-in-simulator))
            (swift-additions:compile-for-device :run run)
          (swift-additions:compile-for-simulator :run run)))
    (if (swift-additions:is-a-swift-package-base-project)
        (swift-additions:build-swift-package)
      (message "Not xcodeproject nor swift package"))))

(defun swift-additions:precompile-common-headers ()
  "Precompile common headers to speed up subsequent builds."
  (when swift-additions:debug
    (message "Precompiling common headers..."))
  
  (let ((default-directory (xcode-additions:project-root))
        (headers-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData/SharedPrecompiledHeaders")))
    
    ;; Create headers directory if it doesn't exist
    (unless (file-exists-p headers-dir)
      (make-directory headers-dir t))
    
    ;; Find common Swift/Objective-C headers
    (dolist (framework '("Foundation" "UIKit" "SwiftUI" "Combine"))
      (let ((cmd (format "xcrun clang -x objective-c-header -arch arm64 -isysroot $(xcrun --sdk iphonesimulator --show-sdk-path) -I$(xcrun --sdk iphonesimulator --show-sdk-path)/System/Library/Frameworks/%s.framework/Headers -o %s/%s.pch /dev/null"
                        framework headers-dir framework)))
        (when swift-additions:debug
          (message "Running: %s" cmd))
        (start-process-shell-command (format "precompile-%s" framework) nil cmd)))))

(cl-defun swift-additions:compile-for-simulator (&key run)
  "Compile app for simulator with optional RUN after completion."
  (swift-additions:cleanup)
  (swift-additions:setup-build-environment)
  (swift-additions:enable-build-caching)
  ;; (swift-additions:precompile-common-headers)
  (setq swift-additions:current-build-command nil)
  (xcode-additions:setup-project)
  (setq run-once-compiled run)

  (let ((build-command (swift-additions:build-app-command
                        :sim-id (ios-simulator:simulator-identifier)))
        (default-directory (xcode-additions:project-root)))

    (mode-line-hud:update
     :message (format "Building %s|%s"
                      (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                      (propertize (ios-simulator:simulator-name) 'face 'font-lock-negation-char-face)))

    (xcode-additions:setup-xcodebuildserver)

    (swift-additions:compile-with-progress
     :command build-command
     :callback (lambda (text)
                 (if run-once-compiled
                     (swift-additions:check-for-errors text #'swift-additions:run-app-after-build)
                   (swift-additions:check-for-errors text #'swift-additions:successful-build)))
     :update-callback (lambda (text)
                        (xcode-additions:parse-compile-lines-output :input text)))))

(defun swift-additions:compile-for-device (&key run)
  "Compile and optionally RUN on device."
  (swift-additions:cleanup)
  (swift-additions:setup-build-environment)
  (swift-additions:enable-build-caching)
  (swift-additions:precompile-common-headers)
  (setq swift-additions:current-build-command nil)
  (xcode-additions:setup-project)
  (setq run-once-compiled run)

  (let ((build-command (swift-additions:build-app-command))
        (default-directory (xcode-additions:project-root)))

    (swift-additions:log-debug "Build-folder: %s" (xcode-additions:derived-data-path))

    (xcode-additions:setup-xcodebuildserver)
    (mode-line-hud:update
     :message (format "Compiling %s|%s"
                     (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face)
                     (propertize "Physical Device" 'face 'font-lock-negation-char-face)))

    (swift-additions:compile-with-progress
     :command build-command
     :callback (lambda (text)
                (if run-once-compiled
                    (swift-additions:check-for-errors
                     text #'swift-additions:run-app-on-device-after-build)
                  (swift-additions:check-for-errors
                   text #'swift-additions:successful-build)))
     :update-callback (lambda (text)
                       (xcode-additions:parse-compile-lines-output :input text)))))


(defun swift-additions:is-a-swift-package-base-project ()
  "Check if project is a swift package based."
  (let ((default-directory (periphery-helper:project-root-dir)))
    (file-exists-p "Package.swift")))

(defun swift-additions:check-for-spm-build-errors (text)
  "Check for Swift package build erros in TEXT."
  (when swift-additions:debug (message text))
  (if (or
       (string-match-p (regexp-quote "error:") text)
       (string-match-p (regexp-quote "warning:") text))
      (progn
        (when (not (string-match-p (regexp-quote "error:") text))
          (swift-additions:run-async-swift-package)))
    (swift-additions:run-async-swift-package)))

(defun swift-additions:run-async-swift-package ()
  "Run async swift package and hide the normal output."
  (inhibit-sentinel-messages #'async-shell-command
                             "swift run"
                             "*Swift Package*"))

(defun swift-additions:build-swift-package ()
  "Build swift package module."
  (interactive)
  (let ((default-directory (periphery-helper:project-root-dir)))
    (xcode-additions:reset)
    (async-shell-command-to-string :process-name "periphery" :command "swift build" :callback #'swift-additions:check-for-spm-build-errors)
    (message-with-color :tag "[ Package]" :text (format "%s. Please wait. Patience is a virtue!" (periphery-helper:project-root-dir)) :attributes 'warning)))

(defun swift-additions:test-swift-package ()
  "Test swift package module."
  (interactive)
  (swift-additions:test-swift-package :root (periphery-helper:project-root-dir)))

(defun swift-additions:test-swift-package-from-file ()
  "Test swift package module."
  (interactive)
  (swift-additions:test-swift-package :root (swift-additions:detect-package-root)))

(cl-defun swift-additions:test-swift-package (&key root)
  "Test package in ROOT."
  (let ((default-directory root)
        (package-name (file-name-nondirectory (directory-file-name root))))
    (spinner-start 'progress-bar-filled)
    (setq build-progress-spinner spinner-current)
    (async-start-command-to-string
     :command "swift test"
     :callback (lambda (text)
                  (spinner-stop build-progress-spinner)
                  (let ((filtered (periphery-helper:filter-keep-beginning-paths text)))
                    (periphery-run-test-parser filtered (lambda ()
                                                          (message-with-color
                                                           :tag "[All tests passed]"
                                                           :text ""
                                                           :attributes 'success)))))
     :debug swift-additions:debug)

    (message-with-color
     :tag (format "[Testing '%s'-package]" package-name)
     :text "Please wait. Patience is a virtue!"
     :attributes 'warning)))

(defun swift-additions:detect-package-root ()
  "Detects the root directory of the Swift package based on the current buffer."
  (let ((buffer-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (locate-dominating-file buffer-dir "Package.swift")))

;;;###autoload
(defun swift-additions:compile-and-run ()
  "Compile and run app."
  (interactive)
  (swift-additions:compile :run t))

;;;###autoload
(defun swift-additions:compile-app ()
  "Compile app."
  (interactive)
  (swift-additions:compile :run nil))

;;;###autoload
(defun swift-additions:run()
    "Rerun already compiled and installed app."
    (interactive)
    (periphery-kill-buffer)
    (ios-simulator:kill-buffer)

    (if (xcode-additions:run-in-simulator)
        (ios-simulator:install-and-run-app
         :rootfolder (xcode-additions:project-root)
         :build-folder (xcode-additions:build-folder :device-type :simulator)
         :simulatorId (ios-simulator:simulator-identifier)
         :appIdentifier (xcode-additions:fetch-or-load-app-identifier))
      (ios-device:install-app
       :buildfolder (xcode-additions:build-folder :device-type :device)
       :appIdentifier (xcode-additions:fetch-or-load-app-identifier))))

;;;###autoload
(defun swift-additions:test-module-silent ()
  "Test module."
  (interactive)
  (save-some-buffers t)
  (periphery-kill-buffer)
  (ios-simulator:kill-buffer)
  (swift-additions:test-swift-package))

;;;###autoload
(defun swift-additions:clear-derived-data ()
  "Clear Xcode's DerivedData folder to fix stubborn build issues."
  (interactive)
  (let ((derived-data-path (expand-file-name "~/Library/Developer/Xcode/DerivedData"))
        (project-build-path (expand-file-name ".build" (xcode-additions:project-root))))
    
    ;; First clear project-specific build folder
    (when (file-exists-p project-build-path)
      (message "Clearing project build folder at %s..." project-build-path)
      (async-start
       `(lambda ()
          (let ((default-directory ,project-build-path))
            (dolist (item (directory-files default-directory t "^[^.]"))
              (when (file-directory-p item)
                (delete-directory item t)))
            "completed"))
       (lambda (result)
         (message "Project build folder clearing %s" result))))
    
    ;; Then ask about clearing all derived data
    (when (file-exists-p derived-data-path)
      (when (yes-or-no-p "Clear all Xcode derived data? This will force a full rebuild.")
        (message "Clearing derived data...")
        (async-start
         `(lambda ()
            (let ((default-directory ,derived-data-path))
              (dolist (item (directory-files default-directory t "^[^.]"))
                (when (and (file-directory-p item)
                           (not (string-match-p "ModuleCache" item)))
                  (delete-directory item t)))
              "completed"))
         (lambda (result)
           (message "Derived data clearing %s" result)))))))

;;;###autoload
(defun swift-additions:optimize-build-system ()
  "Perform various optimizations to speed up the build system."
  (interactive)
  (message "Optimizing build system...")
  
  ;; Kill Xcode-related processes that might be locking files
  (dolist (process '("com.apple.dt.Xcode" "IBCocoaTouchImageCatalogTool" 
                     "IBCocoaTouchTool" "XCPreviewAgent" "SourceKitService"))
    (call-process "pkill" nil nil nil "-f" process))
  
  ;; Clear module caches
  (let ((module-cache (expand-file-name "~/Library/Developer/Xcode/DerivedData/ModuleCache")))
    (when (file-exists-p module-cache)
      (async-start
       `(lambda ()
          (delete-directory ,module-cache t)
          "completed")
       (lambda (_) (message "Module cache cleared")))))
  
  ;; Stop Swift Package Manager daemons
  (call-process "pkill" nil nil nil "-f" "swift-package")
  
  ;; Optimize disk I/O by moving derived data to RAM disk if available
  (when (file-exists-p "/Volumes/RAMDisk")
    (let ((ram-derived-data "/Volumes/RAMDisk/DerivedData"))
      (unless (file-exists-p ram-derived-data)
        (make-directory ram-derived-data t))
      (message "RAM disk detected - using it for derived data")))
  
  ;; Precompile common headers
  (swift-additions:precompile-common-headers)
  
  ;; Generate optimized xcconfig
  (swift-additions:generate-fast-build-xcconfig)
  
  ;; Clean SPM package cache
  (let ((spm-cache (expand-file-name "~/.swiftpm")))
    (when (file-exists-p spm-cache)
      (message "Cleaning Swift Package Manager cache...")
      (async-shell-command "rm -rf ~/.swiftpm/cache")))
  
  (message "Build system optimized. Next build should be faster."))

;;;###autoload
(defun swift-additions:fix-dependency-issues ()
  "Fix common dependency issues with Swift packages."
  (interactive)
  (let ((default-directory (xcode-additions:project-root)))
    (message "Fixing dependency issues...")
    
    ;; Remove Package.resolved to force re-resolution
    (when (file-exists-p "Package.resolved")
      (delete-file "Package.resolved")
      (message "Removed Package.resolved"))
    
    ;; Clean SPM cache
    (async-shell-command "rm -rf ~/.swiftpm/cache")
    
    ;; Clean project build folder
    (when (file-exists-p ".build")
      (message "Cleaning .build folder...")
      (async-shell-command "rm -rf .build"))
    
    ;; Clean Xcode project derived data for this project
    (let* ((project-name (xcode-additions:project-name))
           (derived-data-path (expand-file-name (concat "~/Library/Developer/Xcode/DerivedData/" project-name "*"))))
      (async-shell-command (format "rm -rf %s" derived-data-path)))
    
    ;; Reset package dependencies
    (async-shell-command-to-string 
     :command "xcodebuild -resolvePackageDependencies -disableAutomaticPackageResolution"
     :callback (lambda (output)
                 (message "Dependencies resolved. Ready to build.")))
    
    ;; Update xcconfig with architecture settings
    (swift-additions:generate-fast-build-xcconfig)
    
    (message "Dependency issues fixed. Please try building again.")))

(defun swift-additions:diagnose ()
  "Display diagnostic information about the current Swift development environment."
  (interactive)
  (with-help-window "*Swift Diagnostics*"
    (let ((standard-output (get-buffer-create "*Swift Diagnostics*")))
      (princ "Swift Development Environment Diagnostics\n")
      (princ "=====================================\n\n")

      ;; Project information
      (princ "Project Information:\n")
      (princ "-------------------\n")
      (princ (format "Project Root: %s\n" (xcode-additions:project-root)))
      (princ (format "Project Type: %s\n"
                    (cond ((xcode-additions:is-xcodeproject) "Xcode Project")
                          ((swift-additions:is-a-swift-package-base-project) "Swift Package")
                          (t "Unknown"))))
      (when (xcode-additions:is-xcodeproject)
        (princ (format "Scheme: %s\n" (xcode-additions:scheme)))
        (princ (format "Derived Data Path: %s\n" (xcode-additions:derived-data-path))))

      ;; Build configuration
      (princ "\nBuild Configuration:\n")
      (princ "------------------\n")
      (princ (format "Default Configuration: %s\n" swift-additions:default-configuration))
      (princ (format "Build System: %s\n" swift-additions:modern-build-system))
      (princ (format "Additional Build Flags: %s\n"
                    (if swift-additions:additional-build-flags
                        (mapconcat #'identity swift-additions:additional-build-flags " ")
                      "None")))

      ;; Environment
      (princ "\nEnvironment:\n")
      (princ "------------\n")
      (princ (format "x86 Environment: %s\n"
                    (if swift-additions:current-environment-x86 "Yes" "No")))
      (princ (format "Number of CPU Cores: %s\n" (swift-additions:get-optimal-jobs)))

      ;; Device/Simulator
      (princ "\nDevice Configuration:\n")
      (princ "-------------------\n")
      (if (xcode-additions:run-in-simulator)
          (progn
            (princ "Running in Simulator:\n")
            (princ (format "Simulator ID: %s\n" (ios-simulator:simulator-identifier)))
            (princ (format "Simulator Name: %s\n" (ios-simulator:simulator-name))))
        (princ "Running on Physical Device\n"))

      ;; Current build state
      (princ "\nCurrent Build State:\n")
      (princ "------------------\n")
      (when swift-additions:current-build-command
        (princ "Current Build Command:\n")
        (princ "------------------\n")
        (princ swift-additions:current-build-command)
        (princ "\n"))
      (when swift-additions:compilation-time
        (princ (format "\nLast Compilation Time: %s seconds\n"
                      (swift-additions:compilation-time))))

      ;; Debug settings
      (princ "\nDebug Settings:\n")
      (princ "--------------\n")
      (princ (format "Debug Mode: %s\n" (if swift-additions:debug "Enabled" "Disabled")))

      ;; File system checks
      (princ "\nFile System Checks:\n")
      (princ "-----------------\n")
      (let ((build-dir (xcode-additions:build-folder :device-type :simulator)))
        (princ (format "Build Directory Exists: %s\n"
                      (if (and build-dir (file-exists-p build-dir)) "Yes" "No")))
        (when build-dir
          (princ (format "Build Directory Path: %s\n" build-dir))))

      ;; Swift toolchain
      (princ "\nSwift Toolchain:\n")
      (princ "---------------\n")
      (let ((swift-version (shell-command-to-string "swift --version")))
        (princ swift-version))

      ;; System info
      (princ "\nSystem Information:\n")
      (princ "------------------\n")
      (princ (format "Emacs Version: %s\n" emacs-version))
      (princ (format "System Type: %s\n" system-type))

      ;(princ "\nSystem Information:\n")
      (princ "------------------\n")
      (princ (format "Build command: %s\n" (swift-additions:build-app-command
                       :sim-id (ios-simulator:simulator-identifier)
                       :derived-path (xcode-additions:derived-data-path))))

      (princ "\nRecommendations:\n")
      (princ "---------------\n")
      (unless (xcode-additions:is-xcodeproject)
        (unless (swift-additions:is-a-swift-package-base-project)
          (princ "WARNING: Neither Xcode project nor Swift package detected\n")))
      (unless swift-additions:current-build-command
        (princ "NOTE: No build command has been generated yet\n"))
      (when swift-additions:debug
        (princ "NOTE: Debug mode is enabled - this may affect performance\n")))))

(provide 'swift-additions)
;;; swift-additions.el ends here

