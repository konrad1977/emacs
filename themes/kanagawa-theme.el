
;;; commentary: catppuccin based theme


;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))


(autothemer-deftheme
	kanagawa "A theme based on kanagawa color scheme"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (black			"#1E1F28" "#080808")
  (woodsmoke		"#16161D" "#000000")
  (shark			"#2A2A37" "#080808")
  (charade			"#1E1F28" "#121212")
  (tuna				"#363646" "#303030")
  (comet			"#54536D" "#4e4e4e")
  (cloud-burst		"#223249" "#4e4e4e")
  (san-juan			"#2D4F67" "#585858")
  (lynch			"#658594" "#8a8a8a")
  (flint			"#727169" "#ffffff")
  (amethyst-smoke   "#938AA9" "#d7d7ff")
  (lavender-purple  "#957FB8" "#d7d7ff")
  (glacier			"#7FB4CA" "#d7d7ff")
  (chetwode-blue	"#7E9CD8" "#d7d7ff")
  (sea-nymph		"#7AA89F" "#d7d7ff")
  (tana				"#DCD7BA" "#ffffff")
  (twine			"#C0A36E" "#ffd7df")
  (rock-blue		"#9CABCA" "#d7afd7")
  (bittersweet      "#E46876" "#d7afaf")
  (olive			"#98BB6C" "#ffafaf")
  (red				"#FF5E61" "#ff87af")
  (atomic-tangerine "#FFA066" "#ffaf87")
  (yellow			"#E6C384" "#ffd7af")
  (teal				"#B5E8E0" "#afd7d7")
  (blue				"#7E9CD8" "#afd7ff")

  ;; Dark - monochrome:ish
  (gray-2		"#C3BAC6" "#9e9e9e")
  (white		"#DFDEF1" "#eeeeee")
  (yellow-bg	"#383227" "#875f5f")

  )

 ;; Customize faces
  (
  (border                               (:background charade :foreground black))
  (button                               (:foreground sea-nymph))
  (child-frame		                    (:background black :foreground shark))
  (child-frame-border                   (:background charade :foreground shark))
  (cursor                               (:background rock-blue :foreground black))
  (default								(:foreground tana :background charade))
  (error                                (:foreground red))
  (fringe                               (:background charade :foreground san-juan))
  (glyph-face                           (:background red))
  (glyphless-char                       (:foreground san-juan))
  (header-line							(:background black))
  (highlight                            (:background comet :foreground amethyst-smoke))
  (hl-line                              (:background shark))
  (homoglyph                            (:foreground teal))
  (internal-border                      (:background charade :foreground charade))
  (line-number                          (:foreground comet :background nil))
  (line-number-current-line             (:foreground rock-blue :background tuna :bold t))
  (lv-separator                         (:foreground san-juan :background charade))
  (match                                (:background yellow :foreground black))
  (menu                                 (:background black :foreground tana))
  (mode-line                            (:background woodsmoke))
  (mode-line-inactive                   (:background nil :foreground lynch :bold nil))
  (mode-line-active		                (:background woodsmoke :foreground tana :bold nil))
  (mode-line-highlight                  (:foreground twine))
  (mode-line-buffer-id                  (:foreground sea-nymph :bold t))
  (numbers                              (:background sea-nymph :foreground black))
  (region                               (:background tana :foreground charade))
  (separator-line                       (:background charade))
  (shadow                               (:background black))
  (success                              (:foreground sea-nymph))
  (vertical-border                      (:foreground tuna :background nil))
  (warning                              (:foreground olive))
  (window-border                        (:background charade :foreground charade))
  (window-divider-first-pixel           (:foreground charade))
  (window-divider-last-pixel            (:background charade))

  ;; Font lock
  (font-lock-type-face                  (:foreground bittersweet))
  (font-lock-regexp-grouping-backslash  (:foreground yellow))
  (font-lock-keyword-face               (:bold t :foreground lynch))
  (font-lock-warning-face               (:foreground red))
  (font-lock-constant-face              (:foreground glacier))
  (font-lock-string-face                (:foreground sea-nymph))
  (font-lock-builtin-face               (:foreground amethyst-smoke))
  (font-lock-reference-face				(:foreground lavender-purple))
  (font-lock-constant-face              (:foreground twine))
  (font-lock-function-name-face         (:foreground teal))
  (font-lock-variable-name-face         (:foreground rock-blue))
  (font-lock-negation-char-face         (:foreground red))
  (font-lock-comment-face               (:foreground comet))
  (font-lock-comment-delimiter-face     (:foreground comet))
  (font-lock-doc-face                   (:foreground comet))
  (font-lock-doc-markup-face            (:foreground comet))
  (font-lock-preprocessor-face	   		(:foreground comet))
  (elisp-shorthand-font-lock-face       (:foreground atomic-tangerine))

  (info-xref                            (:foreground yellow))
  (highlight-quoted-symbol              (:foreground olive))
  (minibuffer-prompt-end                (:background woodsmoke :foreground atomic-tangerine))
  (minibuffer-prompt                    (:foreground atomic-tangerine :background black))
  (epa-mark                             (:foreground bittersweet))
  (dired-mark                           (:foreground bittersweet))

  (trailing-whitespace                  (:background comet))

  (mode-line (:background black :foreground tana :bold t))
  
  ;; ;; Battery colors
  (doom-modeline-battery-critical       (:foreground red))
  (doom-modeline-battery-warning        (:foreground olive))
  (doom-modeline-battery-charging       (:foreground gray-2))
  (doom-modeline-battery-error          (:foreground red))
  (doom-modeline-battery-normal         (:foreground gray-2))
  (doom-modeline-battery-full           (:foreground sea-nymph))

  ;; Doom visual state
  (doom-modeline-evil-motion-state      (:foreground teal))
  (doom-modeline-evil-emacs-state       (:foreground blue))
  (doom-modeline-evil-insert-state      (:foreground rock-blue))
  (doom-modeline-evil-normal-state      (:foreground tana))
  (doom-modeline-evil-visual-state      (:foreground glacier))
  (doom-modeline-evil-replace-state     (:foreground red))
  (doom-modeline-evil-operator-state    (:foreground blue))

  (doom-modeline-project-dir            (:bold t :foreground sea-nymph))
  (doom-modeline-buffer-path            (:inherit 'bold :foreground sea-nymph))
  (doom-modeline-buffer-file            (:inherit 'bold :foreground olive))
  (doom-modeline-buffer-modified        (:inherit 'bold :foreground yellow))
  (doom-modeline-error                  (:background red))
  (doom-modeline-buffer-major-mode      (:foreground sea-nymph :bold t))
  (doom-modeline-info                   (:bold t :foreground teal))
  (doom-modeline-project-dir            (:bold t :foreground atomic-tangerine))
  (doom-modeline-bar                    (:bold t :background amethyst-smoke))
  (doom-modeline-panel                  (:inherit 'bold :background twine :foreground charade))

  ;;elfeed
  (elfeed-search-feed-face              (:foreground amethyst-smoke))
  (elfeed-search-tag-face               (:foreground sea-nymph))

  ;; message colors
  (message-header-name                  (:foreground gray-2))
  (message-header-other                 (:foreground atomic-tangerine))
  (message-header-subject               (:foreground yellow))
  (message-header-to                    (:foreground white))
  (message-header-cc                    (:foreground sea-nymph))
  (message-header-xheader               (:foreground white))
  (custom-link                          (:foreground blue))
  (link                                 (:foreground blue))

  ;; org-mode
  (org-done                             (:foreground lynch))
  (org-code                             (:background black))
  (org-block                            (:background black))
  (org-block-begin-line                 (:background black :foreground lynch))
  (org-block-end-line	                (:background black :foreground lynch))
  (org-headline-done                    (:foreground lynch :strike-through t))
  (org-todo                             (:foreground sea-nymph :bold t))
  (org-headline-todo                    (:foreground charade))
  (org-upcoming-deadline                (:foreground red))
  (org-footnote                         (:foreground sea-nymph))
  (org-indent                           (:background charade :foreground charade))
  (org-hide	                            (:background charade :foreground charade))
  (org-date                             (:foreground san-juan))
  (org-ellipsis                         (:foreground san-juan :bold t))
  (org-level-1                          (:foreground red :height 1.3 :bold t))
  (org-level-2                          (:foreground rock-blue :height 1.15 :bold t))
  (org-level-3                          (:foreground twine :height 1.05))
  (org-level-4                          (:foreground tana))
  (org-level-5                          (:foreground tana))
  (org-level-6                          (:foreground yellow))
  (org-level-7                          (:foreground atomic-tangerine))
  (org-level-8                          (:foreground olive))

  ;; which-key
  (which-key-key-face                   (:inherit 'font-lock-variable-name-face))
  (which-func							(:inherit 'font-lock-function-name-face :bold t))
  (which-key-group-description-face     (:foreground bittersweet))
  (which-key-command-description-face   (:foreground blue))
  (which-key-local-map-description-face (:foreground yellow))
  (which-key-posframe					(:background woodsmoke))
  (which-key-posframe-border			(:background woodsmoke))

  ;; swiper
  (swiper-line-face                     (:foreground yellow))
  (swiper-background-match-face-1       (:background atomic-tangerine :foreground black))
  (swiper-background-match-face-2       (:background blue :foreground black))
  (swiper-background-match-face-3       (:background twine :foreground black))
  (swiper-background-match-face-4       (:background red :foreground black))
  (swiper-match-face-1					(:inherit 'swiper-background-match-face-1))
  (swiper-match-face-2					(:inherit 'swiper-background-match-face-2))
  (swiper-match-face-3					(:inherit 'swiper-background-match-face-3))
  (swiper-match-face-4					(:inherit 'swiper-background-match-face-4))

  (counsel-outline-default              (:foreground yellow))
  (info-header-xref                     (:foreground yellow))
  (xref-file-header                     (:foreground yellow))
  (xref-match		                    (:foreground atomic-tangerine))

  ;; rainbow delimiter
  (rainbow-delimiters-mismatched-face   (:foreground red))
  (rainbow-delimiters-unmatched-face    (:foreground sea-nymph))
  (rainbow-delimiters-base-error-face   (:foreground red))
  (rainbow-delimiters-base-face         (:foreground gray-2))
  (rainbow-delimiters-depth-1-face      (:foreground amethyst-smoke))
  (rainbow-delimiters-depth-2-face      (:foreground atomic-tangerine))
  (rainbow-delimiters-depth-3-face      (:foreground blue))
  (rainbow-delimiters-depth-4-face      (:foreground olive))
  (rainbow-delimiters-depth-5-face      (:foreground sea-nymph))
  (rainbow-delimiters-depth-6-face      (:foreground yellow))
  (rainbow-delimiters-depth-7-face      (:foreground bittersweet))
  (rainbow-delimiters-depth-8-face      (:foreground teal))
  (rainbow-delimiters-depth-9-face      (:foreground rock-blue))

  ;; show-paren
  (show-paren-match								(:background atomic-tangerine :foreground woodsmoke :bold t))
  (show-paren-match-expression					(:background atomic-tangerine :foreground woodsmoke :bold t))
  (show-paren-mismatch							(:background red :foreground white))

  (tooltip (:background black))

  ;; company-box  (company-tooltip            :inherit 'tooltip)
  (company-tooltip			  (:background black))
  (company-box				  (:background black))
  (company-tooltip-common     (:foreground yellow))
  (company-tooltip-search     (:background red :foreground black :distant-foreground tana))
  (company-tooltip-selection  (:background comet :foreground red :bold t))
  (company-tooltip-mouse      (:background red :foreground black :distant-foreground tana))
  (company-tooltip-annotation (:foreground sea-nymph))
  (company-scrollbar-bg       (:inherit 'tooltip))
  (company-scrollbar-fg       (:background red))
  (company-preview            (:foreground yellow))
  (company-preview-common     (:background red :foreground black))
  (company-preview-search     (:inherit 'company-tooltip-search))
  (company-template-field     (:inherit 'match))

  (flycheck-posframe-background-face	(:background black))
  (flycheck-posframe-face				(:background black))
  (flycheck-posframe-info-face  		(:background black :foreground teal))
  (flycheck-posframe-warning-face  		(:background black :foreground yellow))
  (flycheck-posframe-error-face  		(:background black :foreground rock-blue))
  (flycheck-fringe-warning				(:foreground yellow :background charade))
  (flycheck-fringe-error				(:foreground red :background charade))
  (flycheck-fringe-info					(:foreground blue :background charade))
  (flycheck-error-list-warning          (:foreground yellow :bold t))
  (flycheck-error-list-error            (:foreground red :bold t))
  (flycheck-error-list-info             (:foreground blue :bold t))

  ;; indent dots
  (highlight-indent-guides-character-face       (:foreground tuna))
  (highlight-indent-guides-stack-character-face (:foreground tuna))
  (highlight-indent-guides-stack-odd-face       (:foreground tuna))
  (highlight-indent-guides-stack-even-face      (:foreground comet))
  (highlight-indent-guides-stack-character-face (:foreground tuna))
  (highlight-indent-guides-even-face            (:foreground charade))
  (highlight-indent-guides-odd-face             (:foreground comet))

   ;;;; ivy
  (ivy-current-match                            (:background blue :foreground black :bold t))
  (ivy-subdir                                   (:background atomic-tangerine :foreground black))
  (ivy-action                                   (:background nil :foreground atomic-tangerine))
  (ivy-grep-line-number                         (:background nil :foreground atomic-tangerine))
  (ivy-minibuffer-match-face-1                  (:background nil :foreground yellow))
  (ivy-minibuffer-match-face-2                  (:background nil :foreground yellow))
  (ivy-minibuffer-match-highlight               (:foreground teal))
  (ivy-grep-info                                (:foreground teal))
  (ivy-grep-line-number                         (:foreground rock-blue))
  (ivy-confirm-face                             (:foreground sea-nymph))

  ;; posframe's
  (ivy-posframe                                 (:background black))
  (ivy-posframe-border                          (:inherit 'ivy-posframe))

  (treemacs-directory-collapsed-face			(:foreground tana))
  (treemacs-directory-face						(:foreground tana))
  (treemacs-file-face							(:foreground tana))

  (treemacs-git-added-face						(:foreground atomic-tangerine))
  (treemacs-git-renamed-face				   	(:foreground tana))
  (treemacs-git-ignored-face				   	(:foreground gray-2))
  (treemacs-git-unmodified-face		   			(:foreground tana))
  (treemacs-git-renamed-face		   			(:foreground tana))
  (treemacs-git-modified-face		   			(:foreground olive))

  (tree-sitter-hl-face:constant					(:background red))
  (tree-sitter-hl-face:constant.builtin			(:background red))
  (tree-sitter-hl-face:function.call			(:background rock-blue))

  ;; lets support solaire mode
  (solaire-default-face (:background shark))
  ;; lsp
  (lsp-headerline-breadcrumb-path-error-face (:underline (:color olive :style 'wave)
                                                         :foreground gray-2 :background black))
  (lsp-headerline-breadcrumb-path-face				(:background black))
  (lsp-headerline-breadcrumb-path-hint-face	   		(:background black))
  (lsp-headerline-breadcrumb-path-info-face	   		(:background black))
  (lsp-headerline-breadcrumb-separator-face			(:background black))
  (lsp-headerline-breadcrumb-symbols-face			(:background black))
  (lsp-headerline-breadcrumb-project-prefix-face	(:background black))
  (lsp-headerline-breadcrumb-symbols-error-face     (:foreground red))

  (lsp-ui-doc-background							(:background black :foreground red))
  (lsp-ui-doc-header								(:background black :foreground red))
  (lsp-ui-doc-border								(:background nil :foreground nil))
  (lsp-ui-peek-filename								(:foreground teal))
  (lsp-ui-sideline-code-action			   			(:foreground yellow))
  (lsp-ui-sideline-current-symbol					(:foreground glacier))
  (lsp-ui-sideline-symbol							(:foreground lynch))

  ;; dashboard
  (dashboard-heading								(:foreground rock-blue :bold t))
  (dashboard-items-face								(:bold nil :foreground tana))
  (dashboard-banner-logo-title						(:bold t :height 200))
  (dashboard-no-items-face							(:foreground gray-2))

  ;; all-the-icons
  (all-the-icons-dgreen							(:foreground sea-nymph))
  (all-the-icons-green							(:foreground sea-nymph))
  (all-the-icons-dpurple						(:foreground rock-blue))
  (all-the-icons-purple							(:foreground rock-blue))

  ;; evil
  (evil-ex-substitute-replacement (:foreground atomic-tangerine :strike-through nil :inherit 'evil-ex-substitute-matches))
  (evil-search-highlight-persist-highlight-face (:background yellow))

  (term (:background woodsmoke :foreground woodsmoke))
  (term-color-blue (:background blue :foreround blue))
  (term-color-bright-blue (:inherit 'term-color-blue))
  (term-color-green (:background sea-nymph :foreround sea-nymph))
  (term-color-bright-green (:inherit 'term-color-green))
  (term-color-black (:background woodsmoke :foreground black))
  (term-color-bright-black (:background shark :foreground shark))
  (term-color-white (:background tana :foreground tana))
  (term-color-bright-white (:background white :foreground white))
  (term-color-red (:background red :foreground red))
  (term-color-bright-red (:background olive :foreground olive))
  (term-color-yellow (:background yellow :foreground yellow))
  (term-color-bright-yellow (:background yellow :foreground yellow))
  (term-color-cyan (:background glacier :foreground glacier))
  (term-color-bright-cyan (:background glacier :foreground glacier))
  (term-color-magenta (:background rock-blue :foreground rock-blue))
  (term-color-bright-magenta (:background rock-blue :foreground rock-blue))

  (popup-face (:inherit 'tooltip))
  (popup-selection-face (:inherit 'tooltip))
  (popup-tip-face (:inherit 'tooltip))


  (anzu-match-1 (:foreground sea-nymph :background charade))
  (anzu-match-2 (:foreground yellow :background charade))
  (anzu-match-3 (:foreground teal :background charade))

  (anzu-mode-line		(:foreground black :background rock-blue))
  (anzu-mode-no-match	(:foreground tana :background red))
  (anzu-replace-to		(:foreground yellow :background yellow-bg))

  (ace-jump-face-background (:foreground san-juan))
  (ace-jump-face-foreground (:foreground red :background black :bold t))

  (vertico-current (:background tuna :foreground rock-blue :bold t))

 ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kanagawa)
;;; kanagawa-theme.el ends here
