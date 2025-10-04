;;; Test highlighting function

(load-file "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/periphery.el")
(load-file "/Users/mikaelkonradsson/.emacs.d/localpackages/periphery/periphery-parsers.el")

(setq periphery-debug t)

;; Test the highlighting function directly
(let ((test-message "type 'Style.Color' has no member 'primaryElemen'"))
  (message "\n=== HIGHLIGHTING TEST ===")
  (message "Input: %s" test-message)
  
  ;; Test periphery--mark-all-symbols directly
  (let ((quotes-regex "\\('[^']+'\\)")
        (result test-message))
    (message "Quotes regex: %s" quotes-regex)
    (if (string-match quotes-regex test-message)
        (progn
          (message "Regex matches: '%s'" (match-string 1 test-message))
          (setq result (periphery--mark-all-symbols
                        :input test-message
                        :regex quotes-regex
                        :property '(face highlight)))
          (message "After highlighting: %s" result)
          (message "Text properties: %S" (text-properties-at 5 result)))
      (message "Regex doesn't match")))
  
  ;; Test the full highlighting function
  (let ((highlighted (periphery-parser--apply-highlighting test-message)))
    (message "Full highlighting result: %s" highlighted)
    (message "Properties at pos 5: %S" (text-properties-at 5 highlighted))
    (message "Properties at pos 15: %S" (text-properties-at 15 highlighted))))