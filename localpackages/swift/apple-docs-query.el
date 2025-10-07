;;; apple-docs-query.el --- package for querying Apple Docs using completing-read -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(defconst apple-developer-url "https://developer.apple.com"
  "Developer apple site.")

(defun apple-docs/query (query)
  "Query Apple Documentation for QUERY."
  (interactive "sQuery: ")
  (when query
    (browse-url (format "%s/search/?q=%s" apple-developer-url (url-hexify-string query)))))

(defun apple-docs/query-thing-at-point ()
  "Query thing at point."
  (interactive)
  (when-let ((word (thing-at-point 'word)))
    (apple-docs/query word)))

(provide 'apple-docs-query)
;;; apple-docs-query.el ends here
