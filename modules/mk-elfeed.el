;;; mk-elfeed.el --- Elfeed configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for Elfeed RSS reader with custom icons and tags.
;;; Code:

(use-package restclient
  :defer t)

(use-package elfeed
  :commands elfeed
  :config

  (defun add-icon-to-title (icon tag all-tags title)
    "Add ICON if TAG is present in ALL-TAGS to the TITLE string."
    (if (member tag all-tags)
        (concat icon " ")
      ""))
                                        ;
  (defun my/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (if (member 'unread (elfeed-entry-tags entry))
                            (elfeed-search--faces (elfeed-entry-tags entry))
                          '(:weight thin :inherit font-lock-comment-face)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed
                         (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (seq-filter
                  (lambda (tag) (not (string-equal feed-title tag)))
                  (mapcar #'symbol-name (elfeed-entry-tags entry))))
           (icons '(("swift" . "")
                    ("swiftui" . "")
                    ("emacs" . "")
                    ("neovim" . "")
                    ("artificialInteligence" . "")
                    ("singularity" . "")
                    ("kotlin" . "")
                    ("techcrunch" . "")))
           (title-with-icons (concat
                              (mapconcat
                               (lambda (icon-pair)
                                 (add-icon-to-title (cdr icon-pair) (car icon-pair) tags " "))
                               icons "")
                              " "
                              title))
           (tags-str (mapconcat
                      (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                      tags ","))
           (title-width (- (window-width) 10 elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title-with-icons (elfeed-clamp
                                            elfeed-search-title-min-width
                                            title-width
                                            elfeed-search-title-max-width)
                          :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
      ;; (when feed-title
      ;;   (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags
        (insert tags-str))))

  (setq elfeed-search-print-entry-function #'my/elfeed-search-print-entry)
  (setq elfeed-feeds '(
                       ("https://www.reddit.com/r/emacs.rss" emacs)
                       ("https://www.reddit.com/r/neovim.rss" neovim)
                       ("https://www.reddit.com/r/kotlin.rss" kotlin)
                       ("https://www.reddit.com/r/swift.rss" swift)
                       ("https://www.reddit.com/r/swiftui.rss" swiftui)
                       ("https://www.reddit.com/r/artificialInteligence.rss" artificialInteligence)
                       ;; ("https://techcrunch.com/rss" techcrunch)
                       ))

  (setq elfeed-search-face-alist
        '((emacs font-lock-function-name-face)
          (neovim font-lock-type-face)
          (kotlin font-lock-keyword-face)
          (swift font-lock-constant-face)
          (techcrunch font-lock-variable-name-face)
          (ai font-lock-number-face)
          (singularity font-lock-number-face)
          (read font-lock-comment-face)))
  (setq elfeed-search-filter "@4-days-ago +unread"
        elfeed-search-title-max-width 140
        elfeed-search-title-min-width 140))

(provide 'mk-elfeed)
;; mk-elfeed.el ends here
