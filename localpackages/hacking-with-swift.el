;;; hascking-with-swift.el --- package for quering with Hacking with swift -*- lexical-binding: t; -*-
;;; commentary:

;;; code:

(require 'request)
(require 'browse-url)
(require 'json)
(require 'url-http)
(require 'cl)

(defconst hackingWithSwiftUrl "https://www.hackingwithswift.com"
  "Hacking with swift base url.")

(cl-defun request-data (&key url &key query)
  "Request data (as URL & QUERY)."
  (let ((request-curl-options (list "-H" (string-trim (url-http-user-agent-string)))))
    (request url
      :type "GET"
      :params (list (cons "search" (url-encode-url query)))
      :parser 'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let* ((choices (mapcar (lambda (item)
                                   (let-alist item (cons
                                      (assoc-default 'title item)
                                      (assoc-default 'url item)))) data))
                (selected (completing-read "Choices: " choices))
                (url (cdr (assoc selected choices))))
           (browse url)))))))

(cl-defun browse (url)
  "Browse URL."
  (browse-url (concat hackingWithSwiftUrl url)))

(defun hacking-ws/query (query)
  "Query Hacking with swift (as QUERY)."
  (interactive "sQuery:")
  (request-data
   :url (concat hackingWithSwiftUrl "/example-code/search")
   :query query))
  
(defun hacking-ws/query-thing-at-point ()
  "Query thing at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (hacking-ws/query word)))

(provide 'hacking-with-swift)
;;; hacking-with-swift.el ends here
