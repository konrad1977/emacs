;;; Swift-additions.el --- package for;;; Swift-additions.el --- package for compiling and running swift apps in emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Package for building and running iOS/macOS apps from Emacs

;;; Code:

(require 'ios-device)
(require 'ios-simulator)
(require 'compile) ;; For compilation-mode when not using periphery
(require 'swift-cache nil t) ;; Unified caching system
(require 'swift-error-handler nil t) ;; Enhanced error handling

;; Provide fallback for message-with-color when periphery is not available
(unless (fboundp 'message-with-color)
  (cl-defun message-with-color (&key tag text attributes)
    "Fallback for message-with-color when periphery is not loaded."
    (message "%s %s" (or tag "") (or text ""))))

(defgroup swift-additions nil
  "Swift development tools and utilities for Emacs."
  :group 'programming
  :prefix "swift-additions")

(defcustom swift-additions:debug nil
  "Enable debug mode for swift additions."
  :type 'boolean
  :group 'swift-additions)

(defcustom swift-additions:default-configuration nil
  "Default build configuration to use. If nil, use scheme's default."
  :type '(choice (const :tag "Use scheme's default" nil)
                 (string :tag "Configuration name"))
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

(defcustom swift-additions:other-swift-flags '(
                                               "-no-whole-module-optimization"
                                               )

  "Additional flags to pass to the Swift compiler (OTHER_SWIFT_FLAGS)."
  :type '(repeat string)
  :group 'swift-additions)

(defcustom swift-additions:enable-timing-summary t
  "Enable build timing summary to identify bottlenecks."
  :type 'boolean
  :group 'swift-additions)

(defcustom swift-additions:use-thin-lto nil
  "Use Thin LTO for faster linking (experimental)."
  :type 'boolean
  :group 'swift-additions)

(defcustom swift-additions:skip-package-resolution 'auto
  "Control Swift package dependency resolution during builds.
- 'auto: Automatically detect if packages need resolution (recommended)
- 'always: Always skip package resolution (faster but may fail if packages missing)
- 'never: Never skip, always resolve packages (slower but safer)"
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Always skip" always)
                 (const :tag "Never skip" never))
  :group 'swift-additions)

(defcustom swift-additions:force-package-resolution nil
  "Force package resolution even if packages appear to exist.
Useful when you suspect package corruption or version conflicts."
  :type 'boolean
  :group 'swift-additions)

(defcustom swift-additions:use-periphery t
  "Whether to use periphery for error parsing.
When non-nil, use periphery's custom error display.
When nil, use standard compilation-mode."
  :type 'boolean
  :group 'swift-additions)

(defcustom swift-additions:analysis-mode 'fast
  "Level of post-build analysis to perform.
- 'fast: Quick analysis, async periphery with truncation (recommended)
- 'full: Complete analysis, may be slower for large builds
- 'minimal: Basic success check only, fastest option
- 'disabled: Skip all analysis except build success detection"
  :type '(choice (const :tag "Fast (async with truncation)" fast)
                 (const :tag "Full (complete analysis)" full)
                 (const :tag "Minimal (basic check only)" minimal)
                 (const :tag "Disabled (success check only)" disabled))
  :group 'swift-additions)

(defvar swift-additions:optimization-level 'fast
  "Build optimization level.")

;; Internal variables
(defvar swift-additions:current-environment-x86 nil)
(defvar swift-additions:current-build-command nil)
(defvar swift-additions:build-progress-spinner nil)
(defvar swift-additions:active-build-process nil
  "Currently active build process, if any.")
(defvar swift-additions:active-build-buffer nil
  "Buffer name for the active build process.")
(defvar swift-additions:compilation-time nil)
(defvar swift-additions:last-build-succeeded nil
  "Track if the last build succeeded.
nil = unknown/never built, t = success, \\='failed = failed.")

(defun swift-additions:log-debug (format-string &rest args)
  "Log debug message using FORMAT-STRING and ARGS when debug is enabled."
  (when swift-additions:debug
    (apply #'message (concat "[Swift Debug] " format-string) args)))

(defun swift-additions:handle-build-error (error-message)
  "Handle build ERROR-MESSAGE and display appropriate feedback."
  (if swift-additions:debug
      (message "Build error: %s" error-message))
  (swift-additions:cleanup)
  (mode-line-hud:update
   :message (format "Build failed: %s"
                    (propertize (truncate-string-to-width error-message 50) 'face 'error)))
  (if swift-additions:use-periphery
      (periphery-run-parser error-message)
    (swift-additions:show-errors-in-compilation-mode error-message)))

(defun swift-additions:show-errors-in-compilation-mode (output)
  "Display build OUTPUT in compilation-mode buffer."
  (let ((buf (get-buffer-create "*Swift Build*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert output)
          (compilation-mode)
          (goto-char (point-min))))
      (display-buffer buf))))

(defun swift-additions:reset ()
  "Reset build settings and clear all cached state."
  (interactive)
  (xcode-additions:reset)
  (ios-device:reset)
  (setq swift-additions:current-build-command nil
        swift-additions:current-environment-x86 nil
        swift-additions:build-progress-spinner nil
        swift-additions:compilation-time nil
        run-once-compiled nil)
  ;; Also clear the build folder cache
  (setq current-build-folder nil
        xcode-additions:last-device-type nil))

(defun swift-additions:get-optimization-flags ()
  "Get optimization flags based on current optimization level."
  `(
    ,(format "OTHER_SWIFT_FLAGS=\"%s\""
             (mapconcat (lambda (flag) (concat "-Xfrontend " flag))
                        swift-additions:other-swift-flags " "))
    "SWIFT_BUILD_CACHE_ENABLED=YES"  ; Explicitly enable Swift build cache
    ,(format "SWIFT_PARALLEL_MODULE_JOBS=%d" (num-processors))  ; Use normal CPU cores, not double
    "SWIFT_USE_PARALLEL_SOURCEJOB_TASKS=YES"
    "SWIFT_COMPILATION_MODE=incremental"
    "SWIFT_SERIALIZE_DEBUGGING_OPTIONS=NO"
    "SWIFT_ENABLE_BUILD_CACHE=YES"
    "ENABLE_PREVIEWS=NO"
    "INDEX_ENABLE_DATA_STORE=NO"
    "SWIFTPM_ENABLE_BUILD_CACHES=1"
    "COMPILER_INDEX_STORE_ENABLE=NO"
    "ONLY_ACTIVE_ARCH=YES"
    "ENABLE_TESTABILITY=NO"
    "ENABLE_BITCODE=NO"
    "DEBUG_INFORMATION_FORMAT=dwarf"
    ;; Conservative optimizations that shouldn't break packages
    "RUN_CLANG_STATIC_ANALYZER=NO"  ; Disable Clang static analyzer
    "CLANG_ENABLE_CODE_COVERAGE=NO"  ; Disable code coverage
    "GCC_GENERATE_DEBUGGING_SYMBOLS=YES"  ; Keep symbols but optimize
    "BUILD_LIBRARY_FOR_DISTRIBUTION=NO"  ; Skip library evolution support
    ;; Conditional Thin LTO for faster linking
    ,(if swift-additions:use-thin-lto
         "LLVM_LTO=Thin"  ; Enable Thin LTO when requested
       "LLVM_LTO=NO")  ; Disable LTO for faster Debug builds
    "STRIP_INSTALLED_PRODUCT=NO"  ; Skip stripping in Debug
    "DEPLOYMENT_POSTPROCESSING=NO"  ; Skip deployment processing
    "COPY_PHASE_STRIP=NO"))

(defun swift-additions:xcodebuild-command ()
  "Use x86 environement."
  (if swift-additions:current-environment-x86
      "env /usr/bin/arch -x86_64 xcrun xcodebuild build"
    "xcrun xcodebuild build"))

(defun swift-additions:swift-packages-exist-p ()
  "Check if Swift packages are already downloaded.
Looks for packages in both the local .build folder and global caches."
  (let* ((project-root (xcode-additions:project-root))
         (local-packages (expand-file-name ".build/checkouts" project-root))
         (global-packages (expand-file-name "~/Library/Developer/Xcode/DerivedData"))
         (cache-packages (expand-file-name "~/Library/Caches/org.swift.packages"))
         (cloned-sources (expand-file-name "~/Library/Caches/org.swift.cloned-sources")))
    (or
     ;; Check local .build/checkouts
     (and (file-exists-p local-packages)
          (> (length (directory-files local-packages nil "^[^.]" t)) 0))
     ;; Check global package caches
     (and (file-exists-p cache-packages)
          (> (length (directory-files cache-packages nil "^[^.]" t)) 0))
     ;; Check cloned sources
     (and (file-exists-p cloned-sources)
          (> (length (directory-files cloned-sources nil "^[^.]" t)) 0)))))

(defun swift-additions:should-resolve-packages-p ()
  "Determine if package resolution should be performed.
Based on swift-additions:skip-package-resolution setting."
  (cond
   ;; Force resolution if explicitly requested
   (swift-additions:force-package-resolution t)
   ;; Check skip setting
   ((eq swift-additions:skip-package-resolution 'never) t)
   ((eq swift-additions:skip-package-resolution 'always) nil)
   ;; Auto mode: only resolve if packages don't exist
   ((eq swift-additions:skip-package-resolution 'auto)
    (not (swift-additions:swift-packages-exist-p)))
   ;; Default to resolving
   (t t)))

(defun swift-additions:resolve-package-dependencies ()
  "Resolve Swift package dependencies if needed.
Returns the command string to include in the build command or empty string."
  (if (swift-additions:should-resolve-packages-p)
      (progn
        (when swift-additions:debug
          (message "Resolving Swift package dependencies..."))
        "-resolvePackageDependencies \\")
    (progn
      (when swift-additions:debug
        (message "Skipping package resolution (packages exist or skip enabled)"))
      "")))

(defun swift-additions:get-package-optimization-flags ()
  "Get package management optimization flags based on current settings.
Returns flags to optimize or skip package resolution."
  (if (not (swift-additions:should-resolve-packages-p))
      ;; When skipping resolution, use these flags to prevent automatic updates
      (concat
       "-skipPackageUpdates \\"  ; Skip updating packages
       ;; Note: -skipPackagePluginValidation is available in Xcode 14+
       ;; We'll try to use it and let xcodebuild ignore it if not supported
       "-skipPackagePluginValidation \\"  ; Skip plugin validation (Xcode 14+)
       "")
    ;; When resolving, don't add these flags
    ""))

(defun swift-additions:get-optimal-jobs ()
  "Get get number of CPUs."
  (number-to-string (max 1 (num-processors))))

;; Swift Package Management Functions

(defun swift-additions:check-swift-packages-in-build ()
  "Check if Swift packages are being used/compiled in the build.
Checks both local .build and Xcode's DerivedData locations."
  (let* ((project-root (xcode-additions:project-root))
         (local-source-packages (expand-file-name ".build/SourcePackages" project-root))
         (local-checkouts (expand-file-name ".build/checkouts" project-root))
         (build-intermediates (expand-file-name ".build/Build/Intermediates.noindex" project-root))
         ;; Find DerivedData for this project
         (derived-data-glob (format "~/Library/Developer/Xcode/DerivedData/*%s*/SourcePackages" 
                                   (file-name-nondirectory (directory-file-name project-root))))
         (xcode-source-packages (car (file-expand-wildcards derived-data-glob))))
    (or 
     ;; Check local .build/SourcePackages/repositories (where packages actually are)
     (let ((local-repos (expand-file-name "repositories" local-source-packages)))
       (and (file-exists-p local-repos)
            (> (length (directory-files local-repos nil "^[^.]" t)) 0)))
     ;; Check local .build/checkouts (SPM command line)
     (and (file-exists-p local-checkouts)
          (> (length (directory-files local-checkouts nil "^[^.]" t)) 0))
     ;; Check Xcode's DerivedData SourcePackages/repositories
     (let ((xcode-repos (when xcode-source-packages 
                          (expand-file-name "repositories" xcode-source-packages))))
       (and xcode-repos
            (file-exists-p xcode-repos)
            (> (length (directory-files xcode-repos nil "^[^.]" t)) 0)))
     ;; Check build intermediates for package compilation
     (and (file-exists-p build-intermediates)
          (> (length (directory-files build-intermediates nil "\\.build$" t)) 0)))))

(defun swift-additions:should-resolve-packages-for-build ()
  "Determine if package resolution should be performed for .build-based builds.
For .build-based builds, we should resolve if packages aren't available locally in .build."
  (cond
   ;; Force resolution if explicitly requested
   (swift-additions:force-package-resolution t)
   ;; Check skip setting
   ((eq swift-additions:skip-package-resolution 'never) t)
   ((eq swift-additions:skip-package-resolution 'always) nil)
   ;; Auto mode: resolve if packages don't exist locally in .build
   ((eq swift-additions:skip-package-resolution 'auto)
    (let* ((project-root (xcode-additions:project-root))
           (local-source-packages (expand-file-name ".build/SourcePackages" project-root))
           (local-repos (expand-file-name "repositories" local-source-packages))
           (local-checkouts (expand-file-name ".build/checkouts" project-root)))
      ;; Only skip resolution if packages exist LOCALLY in .build
      (not (or (and (file-exists-p local-repos)
                    (> (length (directory-files local-repos nil "^[^.]" t)) 0))
               (and (file-exists-p local-checkouts)
                    (> (length (directory-files local-checkouts nil "^[^.]" t)) 0))))))
   ;; Default to resolving
   (t t)))

(defun swift-additions:get-package-resolution-flags ()
  "Get package resolution flags for .build-based builds."
  (if (swift-additions:should-resolve-packages-for-build)
      (progn
        (when swift-additions:debug
          (message "Resolving Swift package dependencies for .build..."))
        "-resolvePackageDependencies \\")
    (progn
      (when swift-additions:debug
        (message "Skipping package resolution (packages exist in .build)"))
      "")))

(defun swift-additions:get-package-cache-flags ()
  "Get package caching flags to ensure packages are downloaded to .build"
  (concat
   "-packageCachePath ~/Library/Caches/org.swift.packages \\" ; Global package cache
   "-clonedSourcePackagesDirPath ~/Library/Caches/org.swift.cloned-sources \\" ; Cache cloned sources
   (if (not (swift-additions:should-resolve-packages-for-build))
       ;; When skipping resolution, add flags to prevent updates
       (concat
        "-skipPackageUpdates \\"
        "-skipPackagePluginValidation \\")
     "")))

(cl-defun swift-additions:setup-build-environment (&key for-device)
  "Setup optimal environment variables for build with aggressive caching and parallelization.
If FOR-DEVICE is non-nil, setup for device build (with signing), otherwise for simulator."
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
  
  ;; CocoaPods-specific optimizations
  (when (swift-additions:uses-cocoapods-p)
    (setenv "COCOAPODS_DISABLE_STATS" "YES")  ; Disable CocoaPods analytics
    (setenv "COCOAPODS_SKIP_CACHE" "NO")      ; Use CocoaPods cache
    (setenv "COCOAPODS_PARALLEL_CODE_SIGN" "YES"))  ; Parallel code signing
  
  ;; Don't set code signing environment variables here - handle it in the build command
  ;; This avoids conflicts between environment variables and command-line flags

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
  "Generate optimized xcodebuild command with aggressive parallelization and CocoaPods support."
  (if swift-additions:current-build-command
      swift-additions:current-build-command
    (let ((workspace-or-project (xcode-additions:get-workspace-or-project))
          (has-pods (swift-additions:uses-cocoapods-p)))
      (mapconcat 'identity
                 (delq nil
                       (list
                        "xcrun xcodebuild build"
                        (format "%s" workspace-or-project)
                        (format "-scheme %s" (xcode-additions:scheme))
                        ;; NOTE: -resolvePackageDependencies cannot be used with build command!
                        ;; It makes xcodebuild ONLY resolve packages and then exit
                        ;; Package cache paths - use Xcode defaults for best compatibility
                        ;; Xcode will handle package placement automatically
                        ;; Skip updates if packages exist (only for subsequent builds)
                        (when (not (swift-additions:should-resolve-packages-for-build))
                          "-skipPackageUpdates")
                        ;; Performance optimizations (but allow package downloads when needed)
                        "-skipMacroValidation"  ; Skip macro validation (Xcode 15+)
                        ;; Parallelization
                        "-parallelizeTargets"
                        (format "-jobs %d" (* (num-processors) 2))
                        ;; Destination
                        (if sim-id
                            (format "-destination 'generic/platform=iOS Simulator,id=%s'" sim-id)
                          "-destination 'generic/platform=iOS' -sdk iphoneos")
                        ;; SDK for simulator
                        (when sim-id
                          "SDKROOT=$(xcrun --sdk iphonesimulator --show-sdk-path)")
                        ;; Configuration if specified
                        (when swift-additions:default-configuration
                          (format "-configuration %s" swift-additions:default-configuration))
                        ;; Code signing for simulator
                        (when sim-id
                          "CODE_SIGNING_REQUIRED=NO")
                        (when sim-id
                          "CODE_SIGN_IDENTITY=\"\"")
                        (when sim-id
                          "CODE_SIGNING_ALLOWED=NO")
                        (when sim-id
                          "DEVELOPMENT_TEAM=\"\"")
                        ;; Other build flags
                        "-skipUnavailableActions"
                        "-useNewBuildSystem=YES"
                        ;; Optimization flags as a list (only if they exist)
                        (when (and swift-additions:other-swift-flags
                                  (> (length swift-additions:other-swift-flags) 0))
                          (format "OTHER_SWIFT_FLAGS=\"%s\""
                                  (mapconcat (lambda (flag) (concat "-Xfrontend " flag))
                                            swift-additions:other-swift-flags " ")))
                        "SWIFT_BUILD_CACHE_ENABLED=YES"
                        (format "SWIFT_PARALLEL_MODULE_JOBS=%d" (num-processors))
                        "SWIFT_USE_PARALLEL_SOURCEJOB_TASKS=YES"
                        "SWIFT_COMPILATION_MODE=incremental"
                        "SWIFT_SERIALIZE_DEBUGGING_OPTIONS=NO"
                        "SWIFT_ENABLE_BUILD_CACHE=YES"
                        "ENABLE_PREVIEWS=NO"
                        "INDEX_ENABLE_DATA_STORE=NO"
                        "SWIFTPM_ENABLE_BUILD_CACHES=1"
                        "COMPILER_INDEX_STORE_ENABLE=NO"
                        "ONLY_ACTIVE_ARCH=YES"
                        "ENABLE_TESTABILITY=NO"
                        "ENABLE_BITCODE=NO"
                        "DEBUG_INFORMATION_FORMAT=dwarf"
                        "RUN_CLANG_STATIC_ANALYZER=NO"
                        "CLANG_ENABLE_CODE_COVERAGE=NO"
                        "GCC_GENERATE_DEBUGGING_SYMBOLS=YES"
                        "BUILD_LIBRARY_FOR_DISTRIBUTION=NO"
                        (if swift-additions:use-thin-lto
                            "LLVM_LTO=Thin"
                          "LLVM_LTO=NO")
                        "STRIP_INSTALLED_PRODUCT=NO"
                        "DEPLOYMENT_POSTPROCESSING=NO"
                        "COPY_PHASE_STRIP=NO"
                        ;; Derived data path
                        "-derivedDataPath .build"))
                 " "))))

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
  
  ;; Debug information for project switching issues
  (when swift-additions:debug
    (message "Installing app - Project: %s, App ID: %s, Build folder: %s"
             (xcode-additions:project-root)
             (xcode-additions:fetch-or-load-app-identifier)
             (xcode-additions:build-folder :device-type :simulator)))
  
  (ios-simulator:install-and-run-app
   :rootfolder (xcode-additions:project-root)
   :build-folder (xcode-additions:build-folder :device-type :simulator)
   :simulatorId (ios-simulator:simulator-identifier)
   :appIdentifier (xcode-additions:fetch-or-load-app-identifier)))

(defun swift-additions:check-if-build-was-successful (input-text)
  "Check if INPUT-TEXT indicates a successful build.
  
  Optimized to check only the last part of the output where xcodebuild
  writes its final status, avoiding false positives from build output."
  (when swift-additions:debug 
    (message "Checking build success with output length: %d" (length input-text)))
  
  ;; xcodebuild always puts "BUILD SUCCEEDED" or "BUILD FAILED" near the end
  ;; Check only the last 5000 characters for performance and accuracy
  (let* ((text-length (length input-text))
         (check-region (if (> text-length 5000)
                          (substring input-text (- text-length 5000))
                        input-text))
         ;; Look for explicit build status markers
         (has-success nil)
         (has-failure nil))
    
    ;; Check for BUILD SUCCEEDED - this is the definitive success marker
    (when (string-match-p "\\*\\* BUILD SUCCEEDED \\*\\*" check-region)
      (setq has-success t))
    
    ;; Check for explicit failure markers
    (when (or (string-match-p "\\*\\* BUILD FAILED \\*\\*" check-region)
              (string-match-p "\\*\\* BUILD INTERRUPTED \\*\\*" check-region)
              ;; Only check for xcodebuild-specific errors
              (string-match-p "^xcodebuild: error:" check-region)
              (string-match-p "^Command failed with exit code" check-region)
              (string-match-p "^The following build commands failed:" check-region))
      (setq has-failure t))
    
    (when swift-additions:debug 
      (message "Build check - Success: %s, Failure: %s (checked last %d chars)" 
               has-success has-failure (length check-region)))
    
    ;; Build is successful only if we found BUILD SUCCEEDED and no failures
    (and has-success (not has-failure))))

(defun swift-additions:check-for-errors (output callback)
  "Run error checking on OUTPUT, then call CALLBACK if build successful.
Analysis level controlled by `swift-additions:analysis-mode`."
  (swift-additions:log-debug "Checking for errors in output length: %d chars" (length output))
  
  ;; Always check if build was successful
  (condition-case err
      (when (swift-additions:check-if-build-was-successful output)
        (funcall callback))
    (error
     (swift-additions:handle-build-error (error-message-string err))))
  
  ;; Run analysis based on configured mode
  (when swift-additions:use-periphery
    (pcase swift-additions:analysis-mode
      ('disabled 
       ;; Skip all analysis
       nil)
      ('minimal 
       ;; Only basic error detection, no UI updates
       (swift-additions:run-minimal-analysis output))
      ('fast 
       ;; Async analysis with truncation (default)
       (swift-additions:run-periphery-async output))
      ('full 
       ;; Full synchronous analysis (may be slow)
       (periphery-run-parser output)))))

(defun swift-additions:run-minimal-analysis (output)
  "Run minimal error analysis on OUTPUT for fastest performance.
Only counts errors/warnings without full parsing or UI updates."
  (let ((error-count 0)
        (warning-count 0))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      ;; Quick count of errors and warnings
      (while (re-search-forward "\\berror:" nil t)
        (cl-incf error-count))
      (goto-char (point-min))
      (while (re-search-forward "\\bwarning:" nil t)
        (cl-incf warning-count)))
    
    ;; Only update mode line if there are issues
    (when (or (> error-count 0) (> warning-count 0))
      (xcode-additions:safe-mode-line-notification 
       :message (format "%d error(s), %d warning(s)" error-count warning-count)
       :urgency 'normal))))

(defun swift-additions:run-periphery-async (output)
  "Run periphery analysis on OUTPUT asynchronously to avoid blocking UI.
Uses intelligent truncation and caching for large outputs."
  (when (fboundp 'periphery-run-parser)
    ;; For very large outputs (>100KB), only analyze the last portion with errors
    (let* ((output-size (length output))
           (truncated-output 
            (if (> output-size 100000)
                (swift-additions:truncate-output-intelligently output)
              output)))
      
      ;; Run periphery in a timer to avoid blocking
      (run-with-idle-timer 0.1 nil 
                          (lambda (text)
                            (condition-case err
                                (periphery-run-parser text)
                              (error 
                               (message "Periphery analysis failed: %s" (error-message-string err)))))
                          truncated-output))))

(defun swift-additions:truncate-output-intelligently (output)
  "Truncate large OUTPUT intelligently, keeping error-relevant portions.
Keeps the end of the output where errors typically appear, and any lines with 'error' or 'warning'."
  (let* ((lines (split-string output "\n"))
         (total-lines (length lines))
         (keep-last-n 500)  ; Keep last 500 lines
         (error-lines '()))
    
    ;; Collect lines containing errors/warnings from earlier in the output
    (when (> total-lines keep-last-n)
      (dolist (line (seq-take lines (- total-lines keep-last-n)))
        (when (string-match-p "\\(error\\|warning\\|failed\\):" line)
          (push line error-lines))))
    
    ;; Combine error lines with the last portion of output
    (string-join
     (append (nreverse error-lines)
             '("... [truncated for performance] ...")
             (seq-drop lines (max 0 (- total-lines keep-last-n))))
     "\n")))

(defun swift-additions:run-xcode-build-server-parse (output)
  "Run xcode-build-server parse on OUTPUT asynchronously, avoiding .compile conflicts."
  (when (executable-find "xcode-build-server")
    (let* ((project-root (xcode-additions:project-root))
           (compile-file (expand-file-name ".compile" project-root))
           (compile-lock (expand-file-name ".compile.lock" project-root)))
      ;; Only clean up lock file if it exists (not the .compile file since we want to append)
      (when (file-exists-p compile-lock)
        (delete-file compile-lock))
      
      ;; Wait a moment to ensure build processes have finished writing
      (run-with-timer 1.0 nil
                      (lambda ()
                        (let ((temp-file (make-temp-file "xcodebuild-output-" nil ".log")))
                          ;; Write output to temp file
                          (with-temp-file temp-file
                            (insert output))
                          ;; Run xcode-build-server parse
                          (start-process-shell-command 
                           "xcode-build-server-parse"
                           nil
                           (format "cd '%s' && cat '%s' | xcode-build-server parse -a && rm -f '%s'" 
                                   project-root temp-file temp-file))))))))

(defun swift-additions:cleanup ()
  "Cleanup resources and state."
  (when swift-additions:build-progress-spinner
    (spinner-stop swift-additions:build-progress-spinner))
  (setq swift-additions:current-build-command nil
        swift-additions:compilation-time nil
        swift-additions:force-package-resolution nil))  ; Reset force flag after build

(defun swift-additions:successful-build ()
  "Show that the build was successful."
  (mode-line-hud:update :message (format "Successful build %s"
                                         (propertize (xcode-additions:scheme) 'face 'font-lock-builtin-face))))

(cl-defun swift-additions:compile-with-progress (&key command callback update-callback)
  "Run compilation COMMAND with progress indicator and CALLBACK/UPDATE-CALLBACK in background.
Returns a cons cell (PROCESS . LOG-BUFFER) where LOG-BUFFER accumulates the build output."
  (if (not swift-additions:use-periphery)
      ;; Use compilation-mode directly
      (progn
        (setq swift-additions:compilation-time (current-time))
        (let ((compilation-buffer-name-function (lambda (_) "*Swift Build*"))
              (compilation-scroll-output t)
              (compilation-auto-jump-to-first-error nil))
          (compile command)
          ;; Set up a sentinel to handle completion
          (let ((proc (get-buffer-process (get-buffer "*Swift Build*"))))
            (when proc
              ;; Track active build process
              (setq swift-additions:active-build-process proc)
              (setq swift-additions:active-build-buffer "*Swift Build*")
              (set-process-sentinel 
               proc 
               (lambda (process event)
                 (when (memq (process-status process) '(exit signal))
                   ;; Clear active process tracking
                   (setq swift-additions:active-build-process nil)
                   (setq swift-additions:active-build-buffer nil)
                   (let ((exit-status (process-exit-status process)))
                     (if (= exit-status 0)
                         (progn
                           (setq swift-additions:last-build-succeeded t)
                           (when callback
                             (funcall callback "Build succeeded")))
                       ;; Build failed - do NOT call callback to prevent installation
                       (progn
                         (setq swift-additions:last-build-succeeded 'failed)
                         ;; Extract actual error from build buffer
                         (let ((error-msg (with-current-buffer (get-buffer "*Swift Build*")
                                            (save-excursion
                                              (goto-char (point-max))
                                              ;; Look for actual xcodebuild errors
                                              (if (re-search-backward "^\\(error\\|\\*\\* BUILD FAILED \\*\\*\\|xcodebuild: error:\\)" nil t)
                                                  (buffer-substring-no-properties
                                                   (line-beginning-position)
                                                   (min (+ (point) 500) (point-max)))
                                                (format "Build failed with exit status %s" exit-status))))))
                           (swift-additions:handle-build-error error-msg))
                         ;; Reset run-once-compiled to prevent installation
                         (setq run-once-compiled nil)))))))))
          nil))
    ;; Async implementation with make-process
    (progn
      (spinner-start 'progress-bar-filled)
      (setq swift-additions:build-progress-spinner spinner-current
            swift-additions:compilation-time (current-time))

      ;; Create or get the output buffer for real-time display
      (let* ((output-buffer-name "*Swift Build Output*")
             (output-buffer (get-buffer-create output-buffer-name))
             (log-buffer (generate-new-buffer " *xcodebuild-log*")))
        
        ;; Clear and prepare output buffer
        (with-current-buffer output-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Building with xcodebuild...\n")
            (insert (format "Command: %s\n\n" command))))
        
        ;; Don't display buffer automatically - user can switch to it manually if needed
        
        (let ((process (make-process
                       :name "xcodebuild-background"
                       :buffer log-buffer
                       ;; Use exec to ensure the shell waits for the command to complete
                       :command (list shell-file-name shell-command-switch (concat "exec " command))
                       :noquery t   ; Detach from Emacs process list
                       :sentinel (lambda (proc event)
                                  (when (memq (process-status proc) '(exit signal))
                                    ;; Clear active process tracking
                                    (setq swift-additions:active-build-process nil)
                                    (setq swift-additions:active-build-buffer nil)
                                    (spinner-stop swift-additions:build-progress-spinner)
                                    (let* ((exit-status (process-exit-status proc))
                                           (output (with-current-buffer log-buffer
                                                    (buffer-string))))
                                      (when swift-additions:debug
                                        (message "Build process exited with status: %d, event: %s" 
                                                exit-status event))
                                      ;; Check both exit status and output
                                      (if (and (= exit-status 0)
                                              (swift-additions:check-if-build-was-successful output))
                                          (progn
                                            (setq swift-additions:last-build-succeeded t)
                                            (with-current-buffer output-buffer
                                              (let ((inhibit-read-only t))
                                                (goto-char (point-max))
                                                (insert "\n✅ BUILD SUCCEEDED\n")))
                                            ;; Run xcode-build-server parse asynchronously
                                            (swift-additions:run-xcode-build-server-parse output)
                                            (funcall callback output)
                                            (swift-additions:cleanup))
                                        (progn
                                          (setq swift-additions:last-build-succeeded 'failed)
                                          (with-current-buffer output-buffer
                                            (let ((inhibit-read-only t))
                                              (goto-char (point-max))
                                              (insert (format "\n❌ BUILD FAILED (exit status: %d)\n" exit-status))))
                                          ;; Run xcode-build-server parse for errors too
                                          (swift-additions:run-xcode-build-server-parse output)
                                          (swift-additions:handle-build-error output)))
                                      (kill-buffer log-buffer)))))))
          
          ;; Configure process handling
          (set-process-query-on-exit-flag process nil)
          
          ;; Track active build process
          (setq swift-additions:active-build-process process)
          (setq swift-additions:active-build-buffer (buffer-name log-buffer))
          
          ;; Start async output processing - show output in real-time
          (set-process-filter process
                             (lambda (proc string)
                               ;; Save to log buffer
                               (with-current-buffer log-buffer
                                 (goto-char (point-max))
                                 (insert string))
                               ;; Display output to visible buffer in real-time
                               (with-current-buffer output-buffer
                                 (let ((inhibit-read-only t))
                                   (goto-char (point-max))
                                   (insert string)
                                   ;; Auto-scroll to bottom
                                   (let ((windows (get-buffer-window-list output-buffer nil t)))
                                     (dolist (window windows)
                                       (with-selected-window window
                                         (goto-char (point-max))
                                         (recenter -1))))))
                               ;; Call update callback if provided
                               (when (and update-callback (functionp update-callback))
                                 (funcall update-callback string))))

          (swift-additions:log-debug "Running command: %s" command)
          (cons process log-buffer))))))

(cl-defun swift-additions:compile (&key run force)
  "Build project using xcodebuild (as RUN).
If FORCE is nil, skip compilation if sources are unchanged."
  (save-some-buffers t)

  (if (xcode-additions:is-xcodeproject)
      (progn
        ;; Check if rebuild is needed (unless forced)
        (if (and (not force) (not (swift-additions:needs-rebuild-p)))
            (progn
              (message "Build up-to-date, skipping compilation")
              ;; If run was requested and app is up-to-date, just run it
              (when run
                (swift-additions:run)))
          ;; Proceed with normal build
          (when swift-additions:use-periphery
            (periphery-kill-buffer))
          (ios-simulator:kill-buffer)
          ;; Only ask for device/simulator if not already set
          (unless xcode-additions:device-choice
            (xcode-addition:ask-for-device-or-simulator))
          (if (not (xcode-additions:run-in-simulator))
              (swift-additions:compile-for-device :run run)
            (swift-additions:compile-for-simulator :run run))))
    (if (swift-additions:is-a-swift-package-base-project)
        (swift-additions:build-swift-package)
      (message "Not xcodeproject nor swift package"))))

(defun swift-additions:warm-build-cache ()
  "Warm up build caches and precompile common modules."
  (interactive)
  (cl-block swift-additions:warm-build-cache
    (let* ((project-root (xcode-additions:project-root))
           (cache-key (if (fboundp 'swift-cache-project-key)
                         (swift-cache-project-key project-root "build-cache-warmed")
                       nil)))
      ;; Check if already warmed for this project
      (when (and cache-key (fboundp 'swift-cache-get))
        (when (swift-cache-get cache-key)
          (message "Build caches already warmed for this project")
          (cl-return-from swift-additions:warm-build-cache)))
    
    (let ((default-directory project-root)
          (cache-dir (expand-file-name "~/Library/Caches/org.swift.swiftpm/ModuleCache")))
      
      ;; Ensure cache directories exist
      (dolist (dir (list cache-dir
                         (expand-file-name "~/Library/Caches/org.swift.packages")
                         (expand-file-name "~/Library/Caches/org.swift.cloned-sources")
                         (expand-file-name "~/Library/Developer/Xcode/DerivedData/ModuleCache")))
        (unless (file-exists-p dir)
          (make-directory dir t)))
      
      ;; Precompile common system frameworks asynchronously
      (message "Warming build caches...")
      (dolist (framework '("Foundation" "UIKit" "SwiftUI" "Combine" "CoreData" "CoreGraphics"))
        (start-process-shell-command 
         (format "cache-%s" framework) 
         nil
         (format "xcrun swiftc -emit-module -module-name %s -sdk $(xcrun --sdk iphonesimulator --show-sdk-path) -target arm64-apple-ios15.0-simulator -O -whole-module-optimization /dev/null 2>/dev/null || true" framework)))
      
      ;; Precompile bridging headers if they exist
      (when-let ((bridging-header (car (directory-files-recursively default-directory ".*-Bridging-Header\\.h$" t))))
        (start-process-shell-command 
         "cache-bridging" 
         nil
         (format "xcrun clang -x objective-c-header -arch arm64 -isysroot $(xcrun --sdk iphonesimulator --show-sdk-path) -c %s -o /tmp/bridging.pch 2>/dev/null || true" bridging-header)))
      
      ;; Mark as warmed in cache
      (when (and cache-key (fboundp 'swift-cache-set))
        (swift-cache-set cache-key t 7200))  ; Cache for 2 hours
      
      (message "Build cache warming initiated in background")))))

(defun swift-additions:precompile-common-headers ()
  "Precompile common headers to speed up subsequent builds."
  (when swift-additions:debug
    (message "Precompiling common headers..."))
  
  (let ((default-directory (xcode-additions:project-root))
        (headers-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData/SharedPrecompiledHeaders")))
    
    ;; Create headers directory if it doesn't exist
    (unless (file-exists-p headers-dir)
      (make-directory headers-dir t))
    
    ;; Find common Swift/Objective-C headers with better optimization
    (dolist (framework '("Foundation" "UIKit" "SwiftUI" "Combine"))
      (let ((cmd (format "xcrun clang -x objective-c-header -arch arm64 -O2 -fmodules -fcxx-modules -isysroot $(xcrun --sdk iphonesimulator --show-sdk-path) -I$(xcrun --sdk iphonesimulator --show-sdk-path)/System/Library/Frameworks/%s.framework/Headers -o %s/%s.pch /dev/null 2>/dev/null || true"
                        framework headers-dir framework)))
        (when swift-additions:debug
          (message "Running: %s" cmd))
        (start-process-shell-command (format "precompile-%s" framework) nil cmd)))))

(cl-defun swift-additions:compile-for-simulator (&key run)
  "Compile app for simulator with optional RUN after completion."
  (swift-additions:cleanup)
  (swift-additions:setup-build-environment :for-device nil)
  (swift-additions:enable-build-caching)
  ;; (swift-additions:precompile-common-headers)
  (setq swift-additions:current-build-command nil)
  ;; Clear build folder cache to ensure fresh detection
  (setq current-build-folder nil
        xcode-additions:last-device-type nil)
  (xcode-additions:setup-project)
  (setq run-once-compiled run)

  (let ((build-command (swift-additions:build-app-command
                        :sim-id (ios-simulator:simulator-identifier)))
        (default-directory (xcode-additions:project-root)))

    (mode-line-hud:update
     :message (format "Building: %s|%s"
                      (propertize (swift-additions:format-scheme-name (xcode-additions:scheme)) 
                                 'face 'font-lock-builtin-face)
                      (propertize (swift-additions:format-simulator-name (ios-simulator:simulator-name)) 
                                 'face 'font-lock-negation-char-face)))

    (xcode-additions:setup-xcodebuildserver)

    (if swift-additions:use-periphery
        (swift-additions:compile-with-progress
         :command build-command
         :callback (lambda (text)
                     (if run-once-compiled
                         (swift-additions:check-for-errors text #'swift-additions:run-app-after-build)
                       (swift-additions:check-for-errors text #'swift-additions:successful-build)))
         :update-callback (lambda (text)
                            (xcode-additions:parse-compile-lines-output :input text)))
      ;; When using compilation-mode, we handle it differently
      (swift-additions:compile-with-progress
       :command build-command
       :callback (lambda (text)
                   ;; Only run/install if build was successful (callback is only called on success now)
                   (if run-once-compiled
                       (swift-additions:run-app-after-build)
                     (swift-additions:successful-build)))
       :update-callback nil))))

(defun swift-additions:compile-for-device (&key run)
  "Compile and optionally RUN on device."
  (swift-additions:cleanup)
  (swift-additions:setup-build-environment :for-device t)
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

    (if swift-additions:use-periphery
        (swift-additions:compile-with-progress
         :command build-command
         :callback (lambda (text)
                    (if run-once-compiled
                        (swift-additions:check-for-errors
                         text #'swift-additions:run-app-on-device-after-build)
                      (swift-additions:check-for-errors
                       text #'swift-additions:successful-build)))
         :update-callback (lambda (text)
                           (xcode-additions:parse-compile-lines-output :input text)))
      ;; When using compilation-mode, we handle it differently
      (swift-additions:compile-with-progress
       :command build-command
       :callback (lambda (text)
                   ;; Only run/install if build was successful (callback is only called on success now)
                   (if run-once-compiled
                       (swift-additions:run-app-on-device-after-build)
                     (swift-additions:successful-build)))
       :update-callback nil))))

;; ============================================================================
;; Smart Build Detection - Skip compilation if sources unchanged
;; ============================================================================

(defun swift-additions:file-mtime (file)
  "Get modification time as float for FILE.
Returns nil if file doesn't exist."
  (when (file-exists-p file)
    (float-time (file-attribute-modification-time
                 (file-attributes file)))))

(defun swift-additions:find-all-source-files ()
  "Find all .swift files in project (cached for 60 seconds)."
  (let* ((project-root (xcode-additions:project-root))
         (cache-key (when (fboundp 'swift-cache-project-key)
                      (swift-cache-project-key project-root "source-files"))))
    (if (and cache-key (fboundp 'swift-cache-with))
        (swift-cache-with cache-key 60  ; Cache for 1 minute
          (let ((default-directory project-root))
            (split-string
             (shell-command-to-string
              "find . -name '*.swift' -not -path '*/.*' -not -path '*/DerivedData/*' -type f")
             "\n" t)))
      ;; Fallback without cache
      (let ((default-directory (xcode-additions:project-root)))
        (split-string
         (shell-command-to-string
          "find . -name '*.swift' -not -path '*/.*' -not -path '*/DerivedData/*' -type f")
         "\n" t)))))

(defun swift-additions:find-newest-source-mtime ()
  "Find the newest modification time among all Swift source files."
  (let* ((source-files (swift-additions:find-all-source-files))
         (project-root (xcode-additions:project-root))
         (mtimes (mapcar (lambda (file)
                          (swift-additions:file-mtime
                           (expand-file-name file project-root)))
                        source-files)))
    (when mtimes
      (apply #'max (delq nil mtimes)))))

(defun swift-additions:get-important-files-mtime ()
  "Get the newest mtime of important project files (project.pbxproj, Podfile.lock, etc)."
  (let* ((project-root (xcode-additions:project-root))
         (important-files (list
                          ;; Find project.pbxproj
                          (car (directory-files-recursively
                                project-root "project\\.pbxproj$" nil
                                (lambda (dir) (not (string-match-p "/\\." dir)))
                                1))
                          ;; CocoaPods
                          (expand-file-name "Podfile.lock" project-root)
                          ;; Swift Package Manager
                          (expand-file-name "Package.resolved" project-root)))
         (mtimes (mapcar #'swift-additions:file-mtime
                        (delq nil important-files))))
    (when mtimes
      (apply #'max (delq nil mtimes)))))

(defun swift-additions:get-built-app-path ()
  "Get path to the built .app bundle."
  (let* ((build-folder (xcode-additions:build-folder
                       :device-type (if (xcode-additions:run-in-simulator)
                                        :simulator
                                      :device)))
         (product-name (xcode-additions:product-name))
         (app-path (when (and build-folder product-name)
                    (expand-file-name (format "%s.app" product-name) build-folder))))
    (when (and app-path (file-exists-p app-path))
      app-path)))

(defun swift-additions:needs-rebuild-p ()
  "Return t if rebuild is needed, nil if app is up-to-date.
Compares modification times of:
- All .swift source files
- Project configuration files (project.pbxproj, Podfile.lock, etc)
Against the built .app bundle."
  (let* ((app-path (swift-additions:get-built-app-path))
         (app-mtime (when app-path
                     (swift-additions:file-mtime app-path)))
         (newest-source-mtime (swift-additions:find-newest-source-mtime))
         (newest-project-mtime (swift-additions:get-important-files-mtime)))

    (when swift-additions:debug
      (message "Build check - App: %s (mtime: %s), Newest source: %s, Newest project file: %s, Last build: %s"
               app-path
               (when app-mtime (format-time-string "%Y-%m-%d %H:%M:%S" app-mtime))
               (when newest-source-mtime (format-time-string "%Y-%m-%d %H:%M:%S" newest-source-mtime))
               (when newest-project-mtime (format-time-string "%Y-%m-%d %H:%M:%S" newest-project-mtime))
               (cond ((eq swift-additions:last-build-succeeded t) "succeeded")
                     ((eq swift-additions:last-build-succeeded 'failed) "FAILED")
                     (t "unknown"))))

    (or (not app-path)                           ; No app exists
        (not app-mtime)                          ; Can't read app mtime
        (eq swift-additions:last-build-succeeded 'failed)  ; Last build failed
        (and newest-source-mtime
             (> newest-source-mtime app-mtime))  ; Source newer than app
        (and newest-project-mtime
             (> newest-project-mtime app-mtime)))))  ; Project files newer than app

(defun swift-additions:ensure-built (&optional force)
  "Ensure app is built. Build only if needed unless FORCE is non-nil.
Returns t if build was performed, nil if skipped."
  (interactive "P")
  (if (or force (swift-additions:needs-rebuild-p))
      (progn
        (when force
          (message "Forcing rebuild..."))
        (when (and (not force) swift-additions:debug)
          (message "Changes detected, building..."))
        (swift-additions:compile-and-run)
        t)
    (progn
      (message "Build up-to-date, skipping compilation")
      nil)))

;;;###autoload
(defun swift-additions:reset-build-status ()
  "Reset the build status tracking.
This forces the next build to run regardless of file timestamps."
  (interactive)
  (setq swift-additions:last-build-succeeded nil)
  (message "Build status reset - next build will run unconditionally"))

;;;###autoload
(defun swift-additions:build-status ()
  "Show current build status and whether rebuild is needed."
  (interactive)
  (let* ((app-path (swift-additions:get-built-app-path))
         (app-mtime (when app-path
                     (swift-additions:file-mtime app-path)))
         (newest-source-mtime (swift-additions:find-newest-source-mtime))
         (newest-project-mtime (swift-additions:get-important-files-mtime))
         (needs-rebuild (swift-additions:needs-rebuild-p)))

    (with-current-buffer (get-buffer-create "*Swift Build Status*")
      (erase-buffer)
      (insert "Swift Build Status\n")
      (insert "==================\n\n")

      (insert (format "Project: %s\n" (xcode-additions:project-root)))
      (insert (format "Scheme: %s\n" (or current-xcode-scheme "Not set")))
      (insert (format "Target: %s\n\n"
                     (if (xcode-additions:run-in-simulator) "Simulator" "Device")))

      (if app-path
          (progn
            (insert (format "Built App: %s\n" app-path))
            (insert (format "Last Built: %s\n\n"
                           (if app-mtime
                               (format-time-string "%Y-%m-%d %H:%M:%S" app-mtime)
                             "Unknown"))))
        (insert "Built App: Not found\n\n"))

      (when newest-source-mtime
        (insert (format "Newest Source File: %s\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S" newest-source-mtime))))

      (when newest-project-mtime
        (insert (format "Newest Project File: %s\n\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S" newest-project-mtime))))

      (insert (format "Last Build Result: %s\n"
                     (cond ((eq swift-additions:last-build-succeeded t)
                            (propertize "Success" 'face 'success))
                           ((eq swift-additions:last-build-succeeded 'failed)
                            (propertize "Failed" 'face 'error))
                           (t "Unknown"))))

      (insert (format "Status: %s\n"
                     (if needs-rebuild
                         (propertize "Rebuild Needed" 'face 'warning)
                       (propertize "Up-to-date" 'face 'success))))

      (when needs-rebuild
        (insert "\nReason: ")
        (cond
         ((not app-path)
          (insert "No built app found"))
         ((eq swift-additions:last-build-succeeded 'failed)
          (insert (propertize "Last build failed" 'face 'error)))
         ((and newest-source-mtime (> newest-source-mtime app-mtime))
          (insert "Source files modified"))
         ((and newest-project-mtime (> newest-project-mtime app-mtime))
          (insert "Project configuration modified"))
         (t
          (insert "Unknown"))))

      (display-buffer (current-buffer)))))

;;;###autoload
(defun swift-additions:show-last-build-errors ()
  "Show the last 50 lines of the build output, focusing on errors."
  (interactive)
  (if-let ((build-buffer (get-buffer "*Swift Build*")))
      (with-current-buffer (get-buffer-create "*Swift Build Errors*")
        (erase-buffer)
        (insert "Last Build Errors\n")
        (insert "=================\n\n")
        (insert (format "Build Status: %s\n\n"
                       (cond ((eq swift-additions:last-build-succeeded t)
                              (propertize "Success" 'face 'success))
                             ((eq swift-additions:last-build-succeeded 'failed)
                              (propertize "Failed" 'face 'error))
                             (t "Unknown"))))

        ;; Extract and show actual errors first
        (let ((errors (with-current-buffer build-buffer
                        (save-excursion
                          (goto-char (point-min))
                          (let ((error-lines '()))
                            (while (re-search-forward "^\\(.+:\\([0-9]+\\):\\([0-9]+\\): error:\\|\\*\\* BUILD FAILED \\*\\*\\|xcodebuild: error:\\|The following build commands failed:\\)" nil t)
                              (push (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (min (+ (line-end-position) 200) (point-max)))
                                   error-lines))
                            (nreverse error-lines))))))
          (when errors
            (insert "=== ERRORS FOUND ===\n")
            (dolist (err errors)
              (insert (propertize err 'face 'error))
              (insert "\n\n"))))

        (insert "\n\nLast 50 lines of build output:\n")
        (insert "-------------------------------\n")
        (insert (with-current-buffer build-buffer
                  (save-excursion
                    (goto-char (point-max))
                    (forward-line -50)
                    (buffer-substring-no-properties (point) (point-max)))))
        (compilation-mode)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
    (message "No *Swift Build* buffer found. Run a build first.")))

;; ============================================================================

(defun swift-additions:is-a-swift-package-base-project ()
  "Check if project is a swift package based."
  (let ((default-directory (swift-additions:get-project-root)))
    (file-exists-p "Package.swift")))

(defun swift-additions:get-scheme-configuration ()
  "Get the build configuration from the current scheme.
Returns the configuration name or 'Debug' as fallback."
  (or swift-additions:default-configuration  ; Use custom if set
      (condition-case nil
          (let* ((scheme-name (xcode-additions:scheme))
                 (workspace-or-project (xcode-additions:get-workspace-or-project))
                 (project-root (xcode-additions:project-root))
                 (scheme-file nil))
            ;; Try to find the scheme file
            (let ((scheme-paths (list
                                 (format "%s/.swiftpm/xcode/xcshareddata/xcschemes/%s.xcscheme" 
                                         project-root scheme-name)
                                 (format "%s/xcshareddata/xcschemes/%s.xcscheme" 
                                         project-root scheme-name))))
              ;; Also check in workspace/project directories
              (when (string-match "-workspace" workspace-or-project)
                (let ((workspace-name (replace-regexp-in-string ".*\\(\\w+\\)\\.xcworkspace.*" "\\1" workspace-or-project)))
                  (push (format "%s/%s.xcworkspace/xcshareddata/xcschemes/%s.xcscheme"
                                project-root workspace-name scheme-name) 
                        scheme-paths)))
              (when (string-match "-project" workspace-or-project)
                (let ((project-name (replace-regexp-in-string ".*\\(\\w+\\)\\.xcodeproj.*" "\\1" workspace-or-project)))
                  (push (format "%s/%s.xcodeproj/xcshareddata/xcschemes/%s.xcscheme"
                                project-root project-name scheme-name) 
                        scheme-paths)))
              
              ;; Find the first existing scheme file
              (setq scheme-file (cl-find-if #'file-exists-p scheme-paths)))
            
            ;; Parse the scheme file to find the configuration
            (if scheme-file
                (with-temp-buffer
                  (insert-file-contents scheme-file)
                  ;; Look for buildConfiguration in the LaunchAction
                  (if (re-search-forward "LaunchAction.*?buildConfiguration *= *\"\\([^\"]+\\)\"" nil t)
                      (let ((config (match-string 1)))
                        (swift-additions:log-debug "Detected configuration from scheme: %s" config)
                        config)
                    ;; Fallback to Debug if not found in LaunchAction
                    "Debug"))
              ;; If no scheme file found, try to detect from available configurations
              (swift-additions:detect-available-configuration)))
        (error "Debug"))))  ; Ultimate fallback

(defun swift-additions:detect-available-configuration ()
  "Try to detect a reasonable configuration from available build settings."
  (condition-case nil
      (let* ((project-root (xcode-additions:project-root))
             (output (shell-command-to-string
                      (format "cd '%s' && xcodebuild -list -json 2>/dev/null | grep -A 10 configurations"
                              project-root))))
        ;; Look for common development configurations
        (cond
         ((string-match "Release (Development)" output) "Release (Development)")
         ((string-match "Debug (Development)" output) "Debug (Development)")  
         ((string-match "Development" output) "Development")
         ((string-match "Debug" output) "Debug")
         (t "Debug")))
    (error "Debug")))

(defun swift-additions:uses-cocoapods-p ()
  "Check if the current project uses CocoaPods."
  (let* ((project-root (swift-additions:get-project-root))
         (cache-key (if (fboundp 'swift-cache-project-key)
                       (swift-cache-project-key project-root "uses-cocoapods")
                     nil)))
    (if (and cache-key (fboundp 'swift-cache-with))
        (swift-cache-with cache-key 1800  ; Cache for 30 minutes
          (let ((default-directory project-root))
            (or (file-exists-p "Podfile")
                (file-exists-p "Podfile.lock")
                (file-exists-p "Pods/Pods.xcodeproj"))))
      ;; Fallback without caching
      (let ((default-directory project-root))
        (or (file-exists-p "Podfile")
            (file-exists-p "Podfile.lock")
            (file-exists-p "Pods/Pods.xcodeproj"))))))

(defun swift-additions:uses-swift-packages-p ()
  "Check if the current project uses Swift Package Manager."
  (let* ((project-root (swift-additions:get-project-root))
         (cache-key (if (fboundp 'swift-cache-project-key)
                       (swift-cache-project-key project-root "uses-swift-packages")
                     nil)))
    (if (and cache-key (fboundp 'swift-cache-with))
        (swift-cache-with cache-key 1800  ; Cache for 30 minutes
          (let ((default-directory project-root))
            (or (file-exists-p "Package.swift")
                (file-exists-p "Package.resolved")
                ;; Check if Xcode project has package dependencies
                (and (xcode-additions:is-xcodeproject)
                     (swift-additions:project-has-package-dependencies-p)))))
      ;; Fallback without caching
      (let ((default-directory project-root))
        (or (file-exists-p "Package.swift")
            (file-exists-p "Package.resolved")
            ;; Check if Xcode project has package dependencies
            (and (xcode-additions:is-xcodeproject)
                 (swift-additions:project-has-package-dependencies-p)))))))

(defun swift-additions:project-has-package-dependencies-p ()
  "Check if the Xcode project has Swift Package dependencies."
  (let ((default-directory (swift-additions:get-project-root)))
    (or
     ;; Check for Package.resolved in project
     (file-exists-p "Package.resolved")
     ;; Check for SourcePackages directory (created by Xcode for SPM)
     (file-exists-p ".swiftpm/xcode/package.xcworkspace")
     ;; Look for project.pbxproj references to package dependencies
     (when-let* ((project-files (directory-files-recursively 
                                default-directory 
                                "project\\.pbxproj$" t)))
       (cl-some (lambda (file)
                  (with-temp-buffer
                    (insert-file-contents file)
                    (or (search-forward "XCRemoteSwiftPackageReference" nil t)
                        (search-forward "packageProductDependencies" nil t)
                        (search-forward "swift-package" nil t))))
                project-files)))))

(defun swift-additions:get-project-root ()
  "Get the project root directory."
  (cond
   ;; If periphery is available and enabled, use it
   ((and swift-additions:use-periphery
         (fboundp 'periphery-helper:project-root-dir))
    (periphery-helper:project-root-dir))
   ;; Try xcode-additions if available
   ((fboundp 'xcode-additions:project-root)
    (xcode-additions:project-root))
   ;; Try vc-root-dir for git projects
   ((vc-root-dir)
    (vc-root-dir))
   ;; Fallback to default-directory
   (t default-directory)))

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
  (if (fboundp 'inhibit-sentinel-messages)
      (inhibit-sentinel-messages #'async-shell-command
                                 "swift run"
                                 "*Swift Package*")
    ;; Fallback when periphery is not available
    (async-shell-command "swift run" "*Swift Package*")))

(defun swift-additions:build-swift-package ()
  "Build swift package module."
  (interactive)
  (let ((default-directory (swift-additions:get-project-root)))
    (xcode-additions:reset)
    (if swift-additions:use-periphery
        (progn
          (async-shell-command-to-string :process-name "periphery" :command "swift build" :callback #'swift-additions:check-for-spm-build-errors)
          (message-with-color :tag "[ Package]" :text (format "%s. Please wait. Patience is a virtue!" (swift-additions:get-project-root)) :attributes 'warning))
      ;; Use compilation-mode for building
      (let ((compilation-buffer-name-function (lambda (_) "*Swift Package Build*"))
            (compilation-scroll-output t))
        (compile "swift build")
        (message-with-color :tag "[ Package]" :text (format "Building %s..." (swift-additions:get-project-root)) :attributes 'warning)))))

(defun swift-additions:test-swift-package ()
  "Test swift package module."
  (interactive)
  (swift-additions:test-swift-package :root (swift-additions:get-project-root)))

(defun swift-additions:test-swift-package-from-file ()
  "Test swift package module."
  (interactive)
  (swift-additions:test-swift-package :root (swift-additions:detect-package-root)))

(cl-defun swift-additions:test-swift-package (&key root)
  "Test package in ROOT."
  (let ((default-directory root)
        (package-name (file-name-nondirectory (directory-file-name root))))
    (if swift-additions:use-periphery
        ;; Original async implementation for periphery
        (progn
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
           :attributes 'warning))
      ;; Use compilation-mode for testing
      (let ((compilation-buffer-name-function (lambda (_) "*Swift Test*"))
            (compilation-scroll-output t))
        (compile "swift test")
        (message-with-color
         :tag (format "[Testing '%s'-package]" package-name)
         :text "Running tests..."
         :attributes 'warning)))))

(defun swift-additions:detect-package-root ()
  "Detects the root directory of the Swift package based on the current buffer."
  (let ((buffer-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (locate-dominating-file buffer-dir "Package.swift")))

;;;###autoload
(defun swift-additions:toggle-periphery-mode ()
  "Toggle between periphery mode and compilation mode for error display."
  (interactive)
  (setq swift-additions:use-periphery (not swift-additions:use-periphery))
  (message "Swift error display mode: %s" 
           (if swift-additions:use-periphery 
               "Periphery (tabulated list)" 
               "Compilation mode"))
  ;; Kill existing buffers to ensure fresh start with new mode
  (when swift-additions:use-periphery
    (when-let ((buf (get-buffer "*Swift Build*")))
      (kill-buffer buf))
    (when-let ((buf (get-buffer "*Swift Test*")))
      (kill-buffer buf))
    (when-let ((buf (get-buffer "*Swift Package Build*")))
      (kill-buffer buf)))
  (when (not swift-additions:use-periphery)
    (when (fboundp 'periphery-kill-buffer)
      (periphery-kill-buffer))))

;;;###autoload
(defun swift-additions:toggle-debug ()
  "Toggle debug mode for swift-additions."
  (interactive)
  (setq swift-additions:debug (not swift-additions:debug))
  (message "Swift-additions debug mode: %s" 
           (if swift-additions:debug "ENABLED" "DISABLED")))

;;;###autoload
(defun swift-additions:show-build-output ()
  "Show the Swift build output buffer."
  (interactive)
  (let ((buf (get-buffer "*Swift Build Output*")))
    (if buf
        (display-buffer buf '((display-buffer-reuse-window
                               display-buffer-in-side-window)
                              (side . bottom)
                              (window-height . 15)))
      (message "No build output available yet"))))

;;;###autoload
(defun swift-additions:hide-build-output ()
  "Hide the Swift build output buffer."
  (interactive)
  (let ((buf (get-buffer "*Swift Build Output*")))
    (when buf
      (delete-windows-on buf))))

;;;###autoload
(defun swift-additions:enable-build-cache-sharing ()
  "Enable build cache sharing between builds for maximum speed.
This configures the build to share compiled modules and objects."
  (interactive)
  (let* ((project-root (xcode-additions:project-root))
         (build-cache-dir (expand-file-name ".build/ModuleCache" project-root))
         (shared-pch-dir (expand-file-name ".build/SharedPrecompiledHeaders" project-root)))
    ;; Create cache directories
    (unless (file-exists-p build-cache-dir)
      (make-directory build-cache-dir t))
    (unless (file-exists-p shared-pch-dir)
      (make-directory shared-pch-dir t))
    
    ;; Set environment for shared caching
    (setenv "SWIFT_MODULE_CACHE_PATH" build-cache-dir)
    (setenv "CLANG_MODULE_CACHE_PATH" build-cache-dir)
    (setenv "SHARED_PRECOMPS_DIR" shared-pch-dir)
    
    ;; Enable all caching mechanisms
    (setenv "SWIFT_ENABLE_BATCH_MODE" "YES")
    (setenv "SWIFT_USE_INTEGRATED_DRIVER" "YES")  ; New Swift driver for better caching
    (setenv "SWIFT_ENABLE_EXPLICIT_MODULES" "YES")  ; Explicit module builds
    
    (message "Build cache sharing enabled. Subsequent builds will be much faster!")))

;;;###autoload
(defun swift-additions:quick-rebuild ()
  "Perform a quick rebuild using all available optimizations.
This is the fastest way to rebuild after small changes."
  (interactive)
  ;; Set to maximum speed mode but respect periphery setting
  (let ((swift-additions:skip-package-resolution 'always)
        (swift-additions:analysis-mode 'minimal))  ; Use minimal instead of disabled
    (swift-additions:enable-build-cache-sharing)
    (swift-additions:compile :run t)))

;;;###autoload  
(defun swift-additions:toggle-build-output ()
  "Toggle visibility of Swift build output buffer."
  (interactive)
  (let ((buf (get-buffer "*Swift Build Output*")))
    (if (and buf (get-buffer-window buf))
        (swift-additions:hide-build-output)
      (swift-additions:show-build-output))))

;;;###autoload
(defun swift-additions:compile-and-run ()
  "Compile and run app."
  (interactive)
  (swift-additions:compile :run t))

(defun swift-additions:format-scheme-name (scheme-name)
  "Format SCHEME-NAME for display in mode-line.
  Removes all backslashes and formats nicely."
  (when scheme-name
    ;; Simply remove ALL backslashes from the name
    (let ((cleaned (replace-regexp-in-string "\\\\" "" scheme-name)))
      
      ;; Now try to extract the parts and format nicely
      (cond
       ;; Pattern 1: "Name (Config)" - parentheses without dash
       ((string-match "^\\([^(]+\\)[[:space:]]*(\\([^)]+\\))" cleaned)
        (format "%s-%s" 
                (string-trim (match-string 1 cleaned))
                (string-trim (match-string 2 cleaned))))
       ;; Pattern 2: "Name - (Config)" - with dash and parentheses
       ((string-match "^\\([^-]+\\)[[:space:]]*-[[:space:]]*(\\([^)]+\\))" cleaned)
        (format "%s-%s" 
                (string-trim (match-string 1 cleaned))
                (string-trim (match-string 2 cleaned))))
       ;; Pattern 3: Just return cleaned version without backslashes
       (t (string-trim cleaned))))))

;;;###autoload
(defun swift-additions:test-scheme-formatting ()
  "Test the scheme name formatting with various inputs."
  (interactive)
  (let ((test-cases '("Bruce - (Development)"
                      "Bruce - \\(Development)"
                      "Bruce - \\(Development\\)"
                      "MyApp - (Release)"
                      "MyApp - \\(Release\\)")))
    (dolist (test test-cases)
      (message "Input: '%s' → Output: '%s'" 
               test 
               (swift-additions:format-scheme-name test)))))

;;;###autoload
(defun swift-additions:debug-current-scheme ()
  "Debug the current scheme name to see exactly what we're dealing with."
  (interactive)
  (let* ((scheme (xcode-additions:scheme))
         (swift-additions:debug t))
    (message "=== Debugging Current Scheme ===")
    (message "Raw scheme from xcode-additions: %S" scheme)
    (message "Length: %d characters" (length scheme))
    (message "Character breakdown:")
    (dotimes (i (length scheme))
      (let ((char (aref scheme i)))
        (message "  Position %d: '%c' (code: %d, hex: %x)" 
                 i char char char)))
    (message "Formatted result: %s" (swift-additions:format-scheme-name scheme))
    (message "=== End Debug ==")))

(defun swift-additions:format-simulator-name (simulator-name)
  "Format SIMULATOR-NAME for better display.
  Ensures proper capitalization for iPhone/iPad."
  (if simulator-name
      ;; Fix common simulator name issues
      (replace-regexp-in-string "iphone" "iPhone"
                               (replace-regexp-in-string "ipad" "iPad" 
                                                        simulator-name t) t)
    ""))

;;;###autoload
(defun swift-additions:compile-app ()
  "Compile app."
  (interactive)
  (swift-additions:compile :run nil))

;;;###autoload
(defun swift-additions:run()
    "Rerun already compiled and installed app."
    (interactive)
    (when swift-additions:use-periphery
      (periphery-kill-buffer))
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
  (when swift-additions:use-periphery
    (periphery-kill-buffer))
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
      (when (yes-or-no-p "Clear all Xcode derived data. This will force a full rebuild?")
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
(defun swift-additions:enable-turbo-mode ()
  "Enable maximum build speed optimizations (may reduce debugging capability)."
  (interactive)
  (setq swift-additions:use-thin-lto nil  ; Thin LTO can actually slow down incremental builds
        swift-additions:enable-timing-summary t
        swift-additions:other-swift-flags 
        '("-no-whole-module-optimization"))  ; Keep it simple and working
  (swift-additions:reset)  ; Reset cached build commands
  (message "Turbo mode enabled. Next build will use speed optimizations."))

;;;###autoload
(defun swift-additions:enable-balanced-mode ()
  "Enable balanced build speed with some debugging capability."
  (interactive)
  (setq swift-additions:use-thin-lto nil
        swift-additions:enable-timing-summary t
        swift-additions:other-swift-flags 
        '("-no-whole-module-optimization"))  ; Remove problematic flags
  (swift-additions:reset)  ; Reset cached build commands
  (message "Balanced mode enabled. Next build will balance speed and debugging."))

;;;###autoload
(defun swift-additions:benchmark-build ()
  "Run a benchmark build to measure compilation performance."
  (interactive)
  (let ((start-time (current-time))
        (old-debug swift-additions:debug))
    (setq swift-additions:debug t)
    (message "Starting benchmark build...")
    (swift-additions:compile-app)
    (setq swift-additions:debug old-debug)
    (message "Benchmark started. Check build times in output.")))

;;;###autoload
(defun swift-additions:fix-dependency-issues ()
  "Fix common dependency issues with Swift packages and CocoaPods (supports hybrid projects)."
  (interactive)
  (let ((default-directory (xcode-additions:project-root))
        (uses-pods (swift-additions:uses-cocoapods-p))
        (uses-spm (swift-additions:uses-swift-packages-p)))
    (message "Fixing dependency issues...")
    
    ;; Handle CocoaPods dependencies if present
    (when uses-pods
      (message "Detected CocoaPods, applying CocoaPods fixes...")
      
      ;; Clean CocoaPods cache and reinstall
      (when (file-exists-p "Podfile.lock")
        (message "Cleaning CocoaPods cache...")
        (async-shell-command "pod cache clean --all"))
      
      ;; Remove Pods directory and reinstall
      (when (file-exists-p "Pods")
        (message "Removing Pods directory...")
        (async-shell-command "rm -rf Pods"))
      
      ;; Reinstall pods
      (message "Reinstalling CocoaPods dependencies...")
      (async-shell-command-to-string 
       :command "pod install --repo-update"
       :callback (lambda (output)
                   (message "CocoaPods dependencies updated."))))
    
    ;; Handle Swift Package Manager dependencies if present
    (when uses-spm
      (message "Detected Swift Package Manager, applying SPM fixes...")
      
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
      
      ;; Reset package dependencies (force resolution)
      (let ((swift-additions:force-package-resolution t))
        (async-shell-command-to-string 
         :command "xcodebuild -resolvePackageDependencies"
         :callback (lambda (output)
                     (message "Swift Package dependencies resolved.")))))
    
    ;; Always clean derived data regardless of dependency manager
    (let* ((project-name (or (xcode-additions:workspace-name) (xcode-additions:project-name)))
           (derived-data-path (expand-file-name (concat "~/Library/Developer/Xcode/DerivedData/" project-name "*"))))
      (async-shell-command (format "rm -rf %s" derived-data-path)))
    
    ;; Update xcconfig with architecture settings
    (swift-additions:generate-fast-build-xcconfig)
    
    (cond 
     ((and uses-pods uses-spm)
      (message "Hybrid project detected. Fixed both CocoaPods and Swift Package dependencies."))
     (uses-pods
      (message "CocoaPods dependency issues fixed."))
     (uses-spm
      (message "Swift Package dependency issues fixed."))
     (t
      (message "No package managers detected, but cleaned derived data.")))
    
    (message "Ready to build.")))

;;;###autoload
(defun swift-additions:toggle-package-resolution ()
  "Toggle Swift package resolution mode between auto/always/never."
  (interactive)
  (setq swift-additions:skip-package-resolution
        (cond
         ((eq swift-additions:skip-package-resolution 'auto) 'always)
         ((eq swift-additions:skip-package-resolution 'always) 'never)
         ((eq swift-additions:skip-package-resolution 'never) 'auto)
         (t 'auto)))
  (message "Swift package resolution mode: %s" swift-additions:skip-package-resolution))

;;;###autoload
(defun swift-additions:force-resolve-packages ()
  "Force Swift package dependency resolution on next build."
  (interactive)
  (setq swift-additions:force-package-resolution t)
  (message "Will force package resolution on next build"))

;;;###autoload
(defun swift-additions:check-package-status ()
  "Check and display Swift package status."
  (interactive)
  (let* ((packages-exist (swift-additions:swift-packages-exist-p))
         (project-root (xcode-additions:project-root))
         (local-packages (expand-file-name ".build/checkouts" project-root))
         (cache-packages (expand-file-name "~/Library/Caches/org.swift.packages"))
         (cloned-sources (expand-file-name "~/Library/Caches/org.swift.cloned-sources")))
    (message "Swift Package Status:
- Packages exist: %s
- Resolution mode: %s
- Force resolution: %s
- Local packages (.build): %s
- Package cache: %s
- Cloned sources: %s"
             (if packages-exist "Yes" "No")
             swift-additions:skip-package-resolution
             (if swift-additions:force-package-resolution "Yes" "No")
             (if (file-exists-p local-packages) "Exists" "Missing")
             (if (file-exists-p cache-packages) "Exists" "Missing")
             (if (file-exists-p cloned-sources) "Exists" "Missing"))))

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
      (princ (format "Uses CocoaPods: %s\n" (if (swift-additions:uses-cocoapods-p) "Yes" "No")))
      (princ (format "Uses Swift Packages: %s\n" (if (swift-additions:uses-swift-packages-p) "Yes" "No")))
      (let ((uses-pods (swift-additions:uses-cocoapods-p))
            (uses-spm (swift-additions:uses-swift-packages-p)))
        (when (and uses-pods uses-spm)
          (princ "Project Type: Hybrid (CocoaPods + Swift Packages)\n"))
        (when uses-pods
          (princ (format "Workspace Name: %s\n" (or (xcode-additions:workspace-name) "Not found")))
          (princ (format "Podfile exists: %s\n" (if (file-exists-p "Podfile") "Yes" "No")))
          (princ (format "Podfile.lock exists: %s\n" (if (file-exists-p "Podfile.lock") "Yes" "No")))
          (princ (format "Pods directory exists: %s\n" (if (file-exists-p "Pods") "Yes" "No"))))
        (when uses-spm
          (princ (format "Package.swift exists: %s\n" (if (file-exists-p "Package.swift") "Yes" "No")))
          (princ (format "Package.resolved exists: %s\n" (if (file-exists-p "Package.resolved") "Yes" "No")))
          (princ (format "Has package dependencies in Xcode project: %s\n" 
                        (if (swift-additions:project-has-package-dependencies-p) "Yes" "No")))))
      (when (xcode-additions:is-xcodeproject)
        (princ (format "Scheme: %s\n" (xcode-additions:scheme)))
        (princ (format "Derived Data Path: %s\n" (xcode-additions:derived-data-path))))

      ;; Build configuration
      (princ "\nBuild Configuration:\n")
      (princ "------------------\n")
      (princ (format "Configuration Override: %s\n" 
                    (if swift-additions:default-configuration 
                        swift-additions:default-configuration 
                        "None (using scheme's default)")))
      (princ (format "Will include -configuration flag: %s\n" 
                    (if swift-additions:default-configuration "Yes" "No")))
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

;;;###autoload
(defun swift-additions:auto-warm-cache-on-file-open ()
  "Automatically warm build cache when opening a Swift file in an Xcode project."
  (when (and (derived-mode-p 'swift-mode)
             (xcode-additions:is-xcodeproject))
    (let ((project-root (xcode-additions:project-root)))
      (when project-root
        (xcode-additions:setup-current-project project-root)))))

;;;###autoload
(defun swift-additions:test-auto-warm ()
  "Test the automatic cache warming for current project."
  (interactive)
  (let ((project-root (xcode-additions:project-root)))
    (if project-root
        (progn
          (message "Testing cache warming for project: %s" project-root)
          (xcode-additions:setup-current-project project-root))
      (message "No Xcode project found in current directory"))))

;; Swift Package Status and Management Functions

;;;###autoload
(defun swift-additions:watch-package-download ()
  "Watch the package download progress in real-time."
  (interactive)
  (let* ((project-root (xcode-additions:project-root))
         (source-packages (expand-file-name ".build/SourcePackages" project-root))
         (global-cache (expand-file-name "~/Library/Caches/org.swift.packages")))
    ;; Get package names from directories
    (let ((source-pkg-names (when (file-exists-p source-packages)
                              (mapcar (lambda (dir)
                                       (file-name-nondirectory dir))
                                     (directory-files source-packages t "^[^.]" t))))
          (cache-pkg-names (when (file-exists-p global-cache)
                            (mapcar (lambda (dir)
                                     (file-name-nondirectory dir))
                                   (directory-files global-cache t "^[^.]" t)))))
      
      (message "📦 Package Status:")
      (message "  Global cache: %s" 
               (if cache-pkg-names
                   (format "%d packages [%s...]" 
                           (length cache-pkg-names)
                           (string-join (seq-take cache-pkg-names 3) ", "))
                 "Empty"))
      (message "  .build/SourcePackages: %s" 
               (if source-pkg-names
                   (format "%d packages [%s...]" 
                           (length source-pkg-names)
                           (string-join (seq-take source-pkg-names 3) ", "))
                 "Not yet created"))
      
      ;; Check current activity in build output and update mode-line HUD
      (when (get-buffer "*Swift Build Output*")
        (with-current-buffer "*Swift Build Output*"
          (goto-char (point-max))
          (cond
           ;; Check for package operations and update mode-line HUD
           ((re-search-backward "Fetching \\(.*\\)" nil t)
            (let ((pkg-name (swift-additions:extract-package-name (match-string 1))))
              (mode-line-hud:update :message (format "⬇️ Fetching %s" 
                                                     (propertize pkg-name 'face 'font-lock-function-name-face)))))
           ((re-search-backward "Cloning \\(.*\\)" nil t)
            (let ((pkg-name (swift-additions:extract-package-name (match-string 1))))
              (mode-line-hud:update :message (format "📥 Cloning %s" 
                                                     (propertize pkg-name 'face 'font-lock-function-name-face)))))
           ((re-search-backward "Computing version for \\(.*\\)" nil t)
            (let ((pkg-name (swift-additions:extract-package-name (match-string 1))))
              (mode-line-hud:update :message (format "🔍 Computing %s" 
                                                     (propertize pkg-name 'face 'font-lock-variable-name-face)))))
           ((re-search-backward "Resolving \\(.*\\)" nil t)
            (let ((pkg-name (swift-additions:extract-package-name (match-string 1))))
              (mode-line-hud:update :message (format "🔄 Resolving %s" 
                                                     (propertize pkg-name 'face 'font-lock-keyword-face)))))
           ((re-search-backward "Creating working copy for \\(.*\\)" nil t)
            (let ((pkg-name (swift-additions:extract-package-name (match-string 1))))
              (mode-line-hud:update :message (format "📝 Creating %s" 
                                                     (propertize pkg-name 'face 'font-lock-type-face)))))
           ;; Check for compilation of packages
           ((re-search-backward "Building \\(.*\\)\\.o" nil t)
            (let ((module-name (file-name-nondirectory (match-string 1))))
              (mode-line-hud:update :message (format "🔨 Building %s" 
                                                     (propertize module-name 'face 'font-lock-builtin-face)))))
           ((re-search-backward "Compiling \\(.*\\)\\.swift" nil t)
            (let ((file-name (file-name-nondirectory (match-string 1))))
              (mode-line-hud:update :message (format "⚡ Compiling %s" 
                                                     (propertize file-name 'face 'warning)))))))))))

(defun swift-additions:extract-package-name (url-or-path)
  "Extract package name from URL-OR-PATH.
Examples:
  https://github.com/user/MyPackage.git -> MyPackage
  https://github.com/user/my-package -> my-package
  /path/to/LocalPackage -> LocalPackage"
  (let ((cleaned (replace-regexp-in-string "\\.git$" "" url-or-path)))
    (if (string-match "\\([^/]+\\)$" cleaned)
        (match-string 1 cleaned)
      url-or-path)))

;;;###autoload
(defun swift-additions:monitor-build-progress ()
  "Monitor build progress with package status updates."
  (interactive)
  (run-with-timer 0 2 'swift-additions:watch-package-download))

;;;###autoload
(defun swift-additions:check-package-status ()
  "Check and display Swift package status for .build-based builds."
  (interactive)
  (let* ((project-root (xcode-additions:project-root))
         (local-source-packages (expand-file-name ".build/SourcePackages" project-root))
         (local-checkouts (expand-file-name ".build/checkouts" project-root))
         (build-intermediates (expand-file-name ".build/Build/Intermediates.noindex" project-root))
         (build-dir (expand-file-name ".build" project-root))
         ;; Find DerivedData for this project
         (derived-data-glob (format "~/Library/Developer/Xcode/DerivedData/*%s*/SourcePackages" 
                                   (file-name-nondirectory (directory-file-name project-root))))
         (xcode-source-packages (car (file-expand-wildcards derived-data-glob)))
         (packages-exist (swift-additions:check-swift-packages-in-build))
         ;; Count packages in various locations
         (local-pkg-count (if (file-exists-p local-source-packages)
                             (length (directory-files local-source-packages nil "^[^.]" t)) 0))
         (xcode-pkg-count (if (and xcode-source-packages (file-exists-p xcode-source-packages))
                             (length (directory-files xcode-source-packages nil "^[^.]" t)) 0))
         (compiled-packages (when (file-exists-p build-intermediates)
                             (length (directory-files build-intermediates nil "\\.build$" t)))))
    (message "Swift Package Status:
📦 Packages available: %s
🏗️  Compiled in build: %s packages
📁 Local .build/SourcePackages: %d packages
🎯 Xcode DerivedData packages: %d packages  
📁 .build/checkouts: %s
📁 .build directory: %s
⚙️  Resolution mode: %s
🔄 Force resolution: %s
📍 Xcode SourcePackages location: %s"
             (if packages-exist "Yes" "No")
             (or compiled-packages 0)
             local-pkg-count
             xcode-pkg-count
             (if (file-exists-p local-checkouts) "Exists" "Missing")
             (if (file-exists-p build-dir) "Exists" "Missing")
             swift-additions:skip-package-resolution
             (if swift-additions:force-package-resolution "Yes" "No")
             (or xcode-source-packages "Not found"))))

;;;###autoload
(defun swift-additions:force-resolve-packages ()
  "Force Swift package dependency resolution on next build."
  (interactive)
  (setq swift-additions:force-package-resolution t)
  (message "Will force package resolution on next build"))

;;;###autoload
(defun swift-additions:toggle-package-resolution ()
  "Toggle Swift package resolution mode between auto/always/never."
  (interactive)
  (setq swift-additions:skip-package-resolution
        (cond
         ((eq swift-additions:skip-package-resolution 'auto) 'always)
         ((eq swift-additions:skip-package-resolution 'always) 'never)
         ((eq swift-additions:skip-package-resolution 'never) 'auto)
         (t 'auto)))
  (message "Swift package resolution mode: %s" swift-additions:skip-package-resolution))

;;;###autoload
(defun swift-additions:prebuild-packages ()
  "Pre-build Swift packages to speed up subsequent builds.
This builds all package dependencies once so they're cached for future builds."
  (interactive)
  (let* ((project-root (xcode-additions:project-root))
         (workspace-or-project (xcode-additions:get-workspace-or-project))
         (scheme (xcode-additions:scheme)))
    (message "Pre-building Swift packages for maximum performance...")
    (async-shell-command
     (format "cd '%s' && xcrun xcodebuild build %s -scheme %s -destination 'generic/platform=iOS Simulator' -derivedDataPath .build -onlyUsePackageVersionsFromResolvedFile -skipPackageUpdates -configuration Debug SWIFT_COMPILATION_MODE=wholemodule BUILD_LIBRARIES_FOR_DISTRIBUTION=YES | head -100"
             project-root workspace-or-project scheme)
     "*Swift Package Prebuild*")
    (message "Pre-building packages in background. This will make future builds much faster!")))

;;;###autoload
(defun swift-additions:optimize-for-incremental-builds ()
  "Configure project for fastest possible incremental builds.
Respects the periphery setting - use toggle-periphery-mode to control error display."
  (interactive)
  (setq swift-additions:skip-package-resolution 'auto  ; Smart package checking
        swift-additions:analysis-mode 'fast  ; Keep fast analysis (respects periphery setting)
        swift-additions:use-thin-lto nil  ; No LTO for debug builds
        swift-additions:other-swift-flags nil)  ; No extra Swift flags for max compatibility
  (swift-additions:setup-build-environment :for-device nil)
  (message "Optimized for incremental builds. Periphery: %s. Builds should be much faster now!" 
           (if swift-additions:use-periphery "ON" "OFF")))

;;;###autoload
(defun swift-additions:clean-build-packages ()
  "Clean .build directory to force fresh package download."
  (interactive)
  (let* ((project-root (xcode-additions:project-root))
         (build-dir (expand-file-name ".build" project-root)))
    (when (file-exists-p build-dir)
      (when (yes-or-no-p "Clean .build directory? This will force re-download of all packages.")
        (message "Cleaning .build directory...")
        (async-start
         `(lambda ()
            (delete-directory ,build-dir t)
            "completed")
         (lambda (result)
           (message ".build directory cleaned: %s" result)))))
    (unless (file-exists-p build-dir)
      (message ".build directory doesn't exist"))))

;; Add hooks to automatically warm cache when opening Swift files
;;;###autoload
(add-hook 'swift-mode-hook 'swift-additions:auto-warm-cache-on-file-open)

;; Also add to find-file-hook for broader coverage
;;;###autoload
(add-hook 'find-file-hook 
          (lambda ()
            (when (and buffer-file-name
                       (string-match-p "\\.swift$" buffer-file-name)
                       (fboundp 'xcode-additions:project-root))
              (let ((project-root (xcode-additions:project-root)))
                (when (and project-root 
                           (fboundp 'xcode-additions:is-xcodeproject)
                           (xcode-additions:is-xcodeproject))
                  (message "Auto-warming cache for %s..." (file-name-nondirectory project-root))
                  (xcode-additions:setup-current-project project-root))))))

;; Performance and Analysis Mode Controls

;;;###autoload
(defun swift-additions:toggle-analysis-mode ()
  "Cycle through analysis modes for performance tuning.
Modes: fast -> minimal -> disabled -> full -> fast"
  (interactive)
  (setq swift-additions:analysis-mode
        (pcase swift-additions:analysis-mode
          ('fast 'minimal)
          ('minimal 'disabled) 
          ('disabled 'full)
          ('full 'fast)
          (_ 'fast)))  ; fallback
  (message "Swift analysis mode: %s" swift-additions:analysis-mode))

;;;###autoload
(defun swift-additions:set-fast-mode ()
  "Set analysis to fast mode for optimal performance/features balance."
  (interactive)
  (setq swift-additions:analysis-mode 'fast)
  (message "Swift analysis set to fast mode (recommended)"))

;;;###autoload
(defun swift-additions:set-minimal-mode ()
  "Set analysis to minimal mode for fastest builds."
  (interactive)
  (setq swift-additions:analysis-mode 'minimal)
  (message "Swift analysis set to minimal mode (fastest)"))

;;;###autoload
(defun swift-additions:run-on-additional-simulator ()
  "Run current app on an additional simulator (e.g., iOS 18 and iOS 26 simultaneously)."
  (interactive)
  (require 'ios-simulator)
  (ios-simulator:run-on-additional-simulator))

(provide 'swift-additions)
;;; swift-additions.el ends here

