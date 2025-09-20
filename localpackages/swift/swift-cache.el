;;; swift-cache.el --- Unified caching system for Swift development tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a unified caching system for Swift/Xcode development tools

;;; Code:

(require 'cl-lib)

(defgroup swift-cache nil
  "Unified caching for Swift development tools."
  :group 'programming
  :prefix "swift-cache-")

(defcustom swift-cache-ttl 300
  "Default cache TTL in seconds (5 minutes)."
  :type 'integer
  :group 'swift-cache)

(defcustom swift-cache-debug nil
  "Enable debug logging for cache operations."
  :type 'boolean
  :group 'swift-cache)

;; Global cache storage
(defvar swift-cache--storage (make-hash-table :test 'equal)
  "Main cache storage hash table.")

(defvar swift-cache--timestamps (make-hash-table :test 'equal)
  "Cache timestamps for TTL tracking.")

(cl-defstruct swift-cache-entry
  "Cache entry structure."
  data
  timestamp
  ttl)

(defun swift-cache-get (key &optional default)
  "Get cached value for KEY, return DEFAULT if not found or expired."
  (let* ((entry (gethash key swift-cache--storage))
         (timestamp (and entry (swift-cache-entry-timestamp entry)))
         (ttl (and entry (swift-cache-entry-ttl entry)))
         (now (float-time)))
    (if (and entry
             timestamp
             (or (null ttl)
                 (< (- now timestamp) ttl)))
        (progn
          (when swift-cache-debug
            (message "[Cache HIT] Key: %s, Age: %.1fs" key (- now timestamp)))
          (swift-cache-entry-data entry))
      (progn
        (when swift-cache-debug
          (if entry
              (message "[Cache EXPIRED] Key: %s, Age: %.1fs" key (- now timestamp))
            (message "[Cache MISS] Key: %s" key)))
        (when entry
          (remhash key swift-cache--storage))
        default))))

(defun swift-cache-set (key value &optional ttl)
  "Set cache KEY to VALUE with optional TTL (defaults to swift-cache-ttl)."
  (let ((entry (make-swift-cache-entry
                :data value
                :timestamp (float-time)
                :ttl (or ttl swift-cache-ttl))))
    (puthash key entry swift-cache--storage)
    (when swift-cache-debug
      (message "[Cache SET] Key: %s, TTL: %s" key (or ttl swift-cache-ttl)))
    value))

(defun swift-cache-invalidate (key)
  "Invalidate cache entry for KEY."
  (when (gethash key swift-cache--storage)
    (remhash key swift-cache--storage)
    (when swift-cache-debug
      (message "[Cache INVALIDATE] Key: %s" key))
    t))

(defun swift-cache-clear ()
  "Clear all cache entries."
  (interactive)
  (let ((count (hash-table-count swift-cache--storage)))
    (clrhash swift-cache--storage)
    (when swift-cache-debug
      (message "[Cache CLEAR] Removed %d entries" count))
    (when (called-interactively-p 'interactive)
      (message "Swift cache cleared (%d entries removed)" count))))

(defun swift-cache-invalidate-pattern (pattern)
  "Invalidate all cache entries matching PATTERN."
  (let ((keys-to-remove '()))
    (maphash (lambda (key _value)
               (when (string-match-p pattern key)
                 (push key keys-to-remove)))
             swift-cache--storage)
    (dolist (key keys-to-remove)
      (swift-cache-invalidate key))
    (when swift-cache-debug
      (message "[Cache PATTERN INVALIDATE] Pattern: %s, Removed: %d entries" 
               pattern (length keys-to-remove)))
    (length keys-to-remove)))

(defmacro swift-cache-with (key ttl &rest body)
  "Execute BODY and cache result with KEY and TTL.
If KEY exists in cache and is not expired, return cached value without executing BODY."
  (declare (indent 2))
  `(let ((cached-value (swift-cache-get ,key 'swift-cache--not-found)))
     (if (not (eq cached-value 'swift-cache--not-found))
         cached-value
       (let ((result (progn ,@body)))
         (swift-cache-set ,key result ,ttl)
         result))))

;; Project-specific cache helpers
(defun swift-cache-project-key (project-root key-suffix)
  "Generate cache key for PROJECT-ROOT with KEY-SUFFIX."
  (format "%s::%s" (file-truename project-root) key-suffix))

(defun swift-cache-invalidate-project (project-root)
  "Invalidate all cache entries for PROJECT-ROOT."
  (swift-cache-invalidate-pattern (regexp-quote (file-truename project-root))))

;; Cache statistics
(defun swift-cache-stats ()
  "Display cache statistics."
  (interactive)
  (let ((total-entries (hash-table-count swift-cache--storage))
        (expired-count 0)
        (total-size 0)
        (now (float-time)))
    (maphash (lambda (_key entry)
               (let* ((timestamp (swift-cache-entry-timestamp entry))
                      (ttl (swift-cache-entry-ttl entry)))
                 (when (and timestamp ttl
                            (>= (- now timestamp) ttl))
                   (cl-incf expired-count))
                 ;; Rough size estimate
                 (cl-incf total-size (length (format "%S" (swift-cache-entry-data entry))))))
             swift-cache--storage)
    (if (called-interactively-p 'interactive)
        (message "Cache stats: %d total entries, %d expired, ~%d bytes"
                 total-entries expired-count total-size)
      (list :total total-entries
            :expired expired-count
            :size total-size))))

;; Auto-cleanup expired entries
(defvar swift-cache--cleanup-timer nil
  "Timer for periodic cache cleanup.")

(defun swift-cache-cleanup ()
  "Remove expired cache entries."
  (let ((removed-count 0)
        (now (float-time))
        (keys-to-remove '()))
    (maphash (lambda (key entry)
               (let* ((timestamp (swift-cache-entry-timestamp entry))
                      (ttl (swift-cache-entry-ttl entry)))
                 (when (and timestamp ttl
                            (>= (- now timestamp) ttl))
                   (push key keys-to-remove))))
             swift-cache--storage)
    (dolist (key keys-to-remove)
      (remhash key swift-cache--storage)
      (cl-incf removed-count))
    (when (and swift-cache-debug (> removed-count 0))
      (message "[Cache CLEANUP] Removed %d expired entries" removed-count))
    removed-count))

(defun swift-cache-start-cleanup-timer ()
  "Start periodic cache cleanup timer."
  (when swift-cache--cleanup-timer
    (cancel-timer swift-cache--cleanup-timer))
  (setq swift-cache--cleanup-timer
        (run-with-timer 60 60 #'swift-cache-cleanup)))

(defun swift-cache-stop-cleanup-timer ()
  "Stop periodic cache cleanup timer."
  (when swift-cache--cleanup-timer
    (cancel-timer swift-cache--cleanup-timer)
    (setq swift-cache--cleanup-timer nil)))

;; Initialize cleanup timer
(swift-cache-start-cleanup-timer)

(provide 'swift-cache)
;;; swift-cache.el ends here