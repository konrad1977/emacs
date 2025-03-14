;;; apple-docs-query.el --- package for quering Apple Docs using completing-read -*- lexical-binding: t; -*-
;;; commentary:

;;; code:

(require 'request)
(require 'json)

(defgroup apple-docs-query nil
  "Apple Docs Query Group."
  :tag "apple-docs-query"
  :group 'apple-docs-query)

(defface apple-docs-title-face
  '((t (:inherit font-lock-keyword-face :bold t)))
   "Title face."
  :group 'apple-docs-query)

(defface apple-docs-description-face
  '((t (:inherit completions-annotations)))
  "Description face."
  :group 'apple-docs-query)

(defconst apple-developer-url "https://developer.apple.com"
  "Developer apple site.")

(cl-defun request-data-from-apple-docs (&key url)
  "Request data (as URL)."
  (let* ((request-curl-options (list "-H" (string-trim (url-http-user-agent-string)))))
    (request url
      :type "GET"
      :parser 'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let* ((c2-width (round (* (- (window-width) 9) 0.4)))
                (choices (mapcar (lambda (item)
                                   (let-alist item
                                     (cons
                                      (propertize
                                       (format "%s %s"
                                               (propertize
                                                (or
                                                 (assoc-default 'title item) "") 'face 'apple-docs-title-face)
                                               (truncate-string-to-width
                                                (propertize
                                                 (or
                                                  (assoc-default 'description item) "")
                                                 'face 'apple-docs-description-face) c2-width nil 32)))
                                      (assoc-default 'url item)))) (cdr (car data))))
                (selected (completing-read "Apple docs: " choices))
                (url (cdr (assoc selected choices))))
           (browse-apple-url url)))))
    nil))

(cl-defun browse-apple-url (url)
  "Browse URL."
  (if-let* ((url url))
      (browse-url (concat apple-developer-url url))))

(defun apple-docs/query (query)
  "Query Hacking with swift (as QUERY)."
  (interactive "sQuery:")
  (when-let* ((url (url-encode-url (format "%s/search/search_data.php?q=%s&type=Documentation" apple-developer-url query))))
    (request-data-from-apple-docs :url url)))

(defun apple-docs/query-thing-at-point ()
  "Query thing at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (apple-docs/query word)))

(provide 'apple-docs-query)
;;; apple-docs-query.el ends here
