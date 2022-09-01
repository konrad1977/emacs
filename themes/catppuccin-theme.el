;;; commentary: catppuccin based theme


;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))


(autothemer-deftheme
	catppuccin "A theme based on catppuccin's amazing color scheme"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (rosewater  "#F5E0DC" "#ffffff")
  (flamingo   "#F2CDCD" "#ffd7df")
  (mauve      "#DDB6F2" "#d7afd7")
  (pink       "#F5C2E7" "#d7afaf")
  (maroon     "#E8A2AF" "#ffafaf")
  (red        "#F28FAD" "#ff87af")
  (peach      "#F8BD96" "#ffaf87")
  (yellow     "#FAE3B0" "#ffd7af")
  (green      "#ABE9B3" "#87afaf")
  (teal       "#B5E8E0" "#afd7d7")
  (blue       "#96CDFB" "#00d7ff")
  (sky        "#89DCEB" "#afffff")
  (lavender   "#C9CBFF" "#d7d7ff")

  ;; Dark - monochrome:ish
  (black       "#15121C" "#080808")
  (black-1     "#1B1923" "#080808")
  (black-2     "#1E1E28" "#121212")
  (black-3     "#332E41" "#303030")
  (black-4     "#575268" "#4e4e4e")
  (blue-bg     "#0C121A" "#875f5f")
  (blue-bg-2   "#243341" "#875f5f")
  (dark        "#0C0A10" "#000000")
  (gray        "#6E6C7E" "#585858")
  (gray-1      "#988BA2" "#8a8a8a")
  (gray-2      "#C3BAC6" "#9e9e9e")
  (red-bg      "#18090E" "#875f5f")
  (red-bg-2    "#18090E" "#875f5f")
  (white       "#DFDEF1" "#eeeeee")
  (yellow-bg   "#383227" "#875f5f")
  (yellow-bg-2 "#383227" "#875f5f")
  )

 ;; Customize faces
  (
  (border                               (:background black-2 :foreground black))
  (bookmark-face                        (:foreground red :background black-2))
  (button                               (:foreground green))
  (child-frame		                    (:background black :foreground black-1))
  (child-frame-border                   (:background black-2 :foreground black-1))
  (cursor                               (:background mauve :foreground black))
  (default								(:foreground rosewater :background black-2))
  (error                                (:foreground red))
  (fringe                               (:background black-2 :foreground gray))
  (glyph-face                           (:background red))
  (glyphless-char                       (:foreground gray))
  (header-line							(:background black))
  (highlight                            (:background black-4 :foreground lavender))
  (hl-line                              (:background black-1))
  (homoglyph                            (:foreground teal))
  (internal-border                      (:background black-2 :foreground black-2))
  (line-number                          (:foreground black-4 :background nil))
  (line-number-current-line             (:foreground mauve :background black-3 :bold t))
  (lv-separator                         (:foreground gray :background black-2))
  (match                                (:background yellow :foreground black))
  (menu                                 (:background black :foreground rosewater))
  (fill-column-indicator                (:foreground black-3))
  (mode-line                            (:background black-3))
  (mode-line-inactive                   (:background black-2 :foreground black-4 :bold nil))
  (mode-line-active		                (:background black-3 :foreground gray-2 :bold t))
  (mode-line-highlight                  (:foreground flamingo))
  (mode-line-buffer-id                  (:foreground green :bold t))
  (numbers                              (:background green :foreground black))
  (region                               (:background black-3))
  (separator-line                       (:background black-2))
  (shadow                               (:background black))
  (success                              (:foreground green))
  (vertical-border                      (:foreground black-3 :background nil))
  (warning                              (:foreground yellow))
  (window-border                        (:foreground peach))
  (window-divider                       (:foreground black-3))

  ;; ;; Font lock
  (font-lock-type-face                  (:foreground lavender))
  (font-lock-regexp-grouping-backslash  (:foreground yellow))
  (font-lock-keyword-face               (:bold t :foreground maroon))
  (font-lock-warning-face               (:inherit 'warning))
  (font-lock-constant-face              (:foreground lavender))
  (font-lock-string-face                (:foreground green :italic t))
  (font-lock-builtin-face               (:foreground yellow))
  (font-lock-reference-face				(:foreground sky))
  (font-lock-constant-face              (:foreground flamingo))
  (font-lock-function-name-face         (:foreground blue))
  (font-lock-variable-name-face         (:foreground mauve))
  (font-lock-negation-char-face         (:foreground red))
  (font-lock-comment-face               (:foreground gray :italic t))
  (font-lock-comment-delimiter-face     (:foreground gray :italic t))
  (font-lock-doc-face                   (:foreground gray))
  (font-lock-doc-markup-face            (:foreground gray))
  (font-lock-preprocessor-face	   		(:foreground gray))
  (elisp-shorthand-font-lock-face       (:foreground peach))

  (highlight-operators-face             (:foreground red))
  (highlight-quoted-symbol              (:foreground maroon))
  (highlight-numbers-face               (:foreground pink))
  (highlight-symbol-face                (:background black-3 :foreground green :weight 'semi-bold))
  (info-xref                            (:foreground yellow))
  
  (minibuffer-prompt-end                (:foreground red))
  (minibuffer-prompt                    (:foreground mauve))
  (epa-mark                             (:foreground pink))
  (dired-mark                           (:foreground pink))

  (trailing-whitespace                  (:background black-4))
  
  ;; ;; Battery colors
  (doom-modeline-battery-critical       (:inherit 'error))
  (doom-modeline-battery-warning        (:inherit 'warning))
  (doom-modeline-battery-charging       (:foreground gray-2))
  (doom-modeline-battery-error          (:inherit 'eror))
  (doom-modeline-battery-normal         (:foreground gray))
  (doom-modeline-battery-full           (:foreground green))

  ;; Doom visual state
  (doom-modeline-evil-motion-state      (:foreground teal))
  (doom-modeline-evil-emacs-state       (:foreground blue))
  (doom-modeline-evil-insert-state      (:foreground mauve))
  (doom-modeline-evil-normal-state      (:foreground rosewater))
  (doom-modeline-evil-visual-state      (:foreground sky))
  (doom-modeline-evil-replace-state     (:foreground red))
  (doom-modeline-evil-operator-state    (:foreground blue))

  (doom-modeline-project-dir            (:foreground gray))
  (doom-modeline-buffer-path            (:foreground gray))
  (doom-modeline-buffer-file            (:foreground gray-1 :bold t))
  (doom-modeline-buffer-major-mode      (:foreground gray :bold t))
  (doom-modeline-buffer-modified        (:foreground gray-2 :italic t :bold t))
  (doom-modeline-error                  (:background red))
  (doom-modeline-info                   (:foreground gray))
  (doom-modeline-project-dir            (:foreground peach))
  (doom-modeline-bar                    (:background lavender :foreground black-4))
  (doom-modeline-panel                  (:inherit 'bold :background flamingo :foreground black-2))
  (doom-modeline                        (:foreground gray))
  (doom-themes-visual-bell              (:background red))

  ;;elfeed
  (elfeed-search-feed-face              (:foreground lavender))
  (elfeed-search-tag-face               (:foreground green))

  ;; message colors
  (message-header-name                  (:foreground gray-2))
  (message-header-other                 (:foreground peach))
  (message-header-subject               (:foreground yellow))
  (message-header-to                    (:foreground white))
  (message-header-cc                    (:foreground green))
  (message-header-xheader               (:foreground white))
  (custom-link                          (:foreground blue))
  (link                                 (:foreground blue))

  ;; org-mode
  (org-done                             (:foreground gray-1))
  (org-code                             (:background black))
  (org-meta-line                        (:background blue-bg :foreground blue))
  (org-block                            (:background black))
  (org-block-begin-line                 (:background black :foreground gray-1))
  (org-block-end-line	                (:background black :foreground gray-1))
  (org-headline-done                    (:foreground gray-1 :strike-through t))
  (org-todo                             (:foreground green :bold t))
  (org-headline-todo                    (:foreground black-2))
  (org-upcoming-deadline                (:foreground red))
  (org-footnote                         (:foreground green))
  (org-indent                           (:background black-2 :foreground black-2))
  (org-hide	                            (:background black-2 :foreground black-2))
  (org-date                             (:foreground gray))
  (org-ellipsis                         (:foreground gray :bold t))
  (org-level-1                          (:foreground red :height 1.3 :bold t))
  (org-level-2                          (:foreground mauve :height 1.15 :bold t))
  (org-level-3                          (:foreground flamingo :height 1.05))
  (org-level-4                          (:foreground rosewater))
  (org-level-5                          (:foreground rosewater))
  (org-level-6                          (:foreground yellow))
  (org-level-7                          (:foreground peach))
  (org-level-8                          (:foreground maroon))

  ;; which-key
  (which-key-key-face                   (:inherit 'font-lock-variable-name-face))
  (which-func							(:inherit 'font-lock-function-name-face :bold t))
  (which-key-group-description-face     (:foreground pink))
  (which-key-command-description-face   (:foreground blue))
  (which-key-local-map-description-face (:foreground yellow))
  (which-key-posframe					(:background dark))
  (which-key-posframe-border			(:background dark))

  ;; swiper
  (swiper-line-face                     (:foreground yellow))
  (swiper-background-match-face-1       (:background peach :foreground black))
  (swiper-background-match-face-2       (:background blue :foreground black))
  (swiper-background-match-face-3       (:background flamingo :foreground black))
  (swiper-background-match-face-4       (:background red :foreground black))
  (swiper-match-face-1					(:inherit 'swiper-background-match-face-1))
  (swiper-match-face-2					(:inherit 'swiper-background-match-face-2))
  (swiper-match-face-3					(:inherit 'swiper-background-match-face-3))
  (swiper-match-face-4					(:inherit 'swiper-background-match-face-4))

  (counsel-outline-default              (:foreground yellow))
  (info-header-xref                     (:foreground yellow))
  (xref-file-header                     (:foreground yellow))
  (xref-match		                    (:foreground peach))

  ;; rainbow delimiter
  (rainbow-delimiters-mismatched-face   (:foreground red :background yellow))
  (rainbow-delimiters-unmatched-face    (:foreground green :background yellow))
  (rainbow-delimiters-base-error-face   (:foreground red :background yellow))
  (rainbow-delimiters-base-face         (:foreground gray-2))
  (rainbow-delimiters-depth-1-face      (:foreground red))
  (rainbow-delimiters-depth-2-face      (:foreground blue))
  (rainbow-delimiters-depth-3-face      (:foreground pink))
  (rainbow-delimiters-depth-4-face      (:foreground green))
  (rainbow-delimiters-depth-5-face      (:foreground mauve))
  (rainbow-delimiters-depth-6-face      (:foreground lavender))
  (rainbow-delimiters-depth-7-face      (:foreground flamingo))
  (rainbow-delimiters-depth-8-face      (:foreground teal))
  (rainbow-delimiters-depth-9-face      (:foreground mauve))

  ;; show-paren
  (show-paren-match						(:background peach :foreground dark :bold t))
  (show-paren-match-expression			(:background peach :foreground dark :bold t))
  (show-paren-mismatch					(:background red :foreground white))

  (company-tooltip                      (:background black :foreground gray-2))
  (company-tooltip-common               (:foreground red :distant-foreground red :bold t))
  (company-tooltip-search               (:background peach))
  (company-tooltip-selection            (:background blue-bg-2 :foreground rosewater :distant-foreground rosewater :bold t))
  (company-tooltip-mouse                (:background nil :foreground black :distant-foreground rosewater))
  (company-tooltip-annotation           (:foreground green))
  (company-tooltip-scrollbar-track      (:background peach))
  (company-tooltip-scrollbar-thumb      (:background flamingo))
  (company-tooltip-quick-access         (:foreground blue :distant-foreground blue-bg))
  (company-scrollbar-bg                 (:inherit 'tooltip))
  (company-scrollbar-fg                 (:background red))
  (company-preview                      (:foreground blue))
  (company-preview-common               (:background red :foreground black))
  (company-preview-search               (:inherit 'company-tooltip-search))
  (company-template-field               (:inherit 'match))

  (company-box-annotation               (:foreground red :background red-bg))
  (company-box-numbers                  (:foreground blue :background blue-bg))

  ;; Eldoc
  (eldoc-box-body                       (:background black-3 :foreground peach))
  (eldoc-box-border                     (:background nil :foreground nil))

  (markdown-hr-face (:background nil :foreground black-3))

  ;; Flycheck
  (flycheck-posframe-background-face	(:background black))
  (flycheck-posframe-face				(:background black))
  (flycheck-posframe-info-face  		(:background black :inherit 'info))
  (flycheck-posframe-warning-face  		(:background black :inherit 'warning))
  (flycheck-posframe-error-face  		(:background black :inherit 'error))
  (flycheck-fringe-warning				(:inherit 'warning :background black-2))
  (flycheck-fringe-error				(:inherit 'error :background black-2))
  (flycheck-fringe-info					(:inherit 'info :background black-2))
  (flycheck-error-list-warning          (:inherit 'warning :bold t))
  (flycheck-error-list-error            (:inheirt 'error :bold t))
  (flycheck-error-list-info             (:inherit 'info :bold t))
  (flycheck-inline-error                (:foreground red-bg :background red :weight 'semi-bold :height 128 :box '(:line-width 1 :color red-bg)))
  (flycheck-inline-info                 (:foreground blue-bg :background blue :weight 'semi-bold :height 128 :box '(:line-width 1 :color blue-bg)))
  (flycheck-inline-warning              (:foreground yellow-bg :background yellow :weight 'semi-bold :height 128 :box '(:line-width 1 :color yellow-bg)))

  ;; indent dots
  (highlight-indent-guides-character-face       (:foreground black-3))
  (highlight-indent-guides-stack-character-face (:foreground black-3))
  (highlight-indent-guides-stack-odd-face       (:foreground black-3))
  (highlight-indent-guides-stack-even-face      (:foreground black-4))
  (highlight-indent-guides-stack-character-face (:foreground black-3))
  (highlight-indent-guides-even-face            (:foreground black-2))
  (highlight-indent-guides-odd-face             (:foreground black-4))

   ;;;; ivy
  (ivy-current-match                            (:background blue :foreground black :bold t))
  (ivy-action                                   (:background nil :foreground lavender))
  (ivy-grep-line-number                         (:background nil :foreground peach))
  (ivy-minibuffer-match-face-1                  (:background nil :foreground blue :bold t))
  (ivy-minibuffer-match-face-2                  (:background nil :foreground green))
  (ivy-minibuffer-match-highlight               (:foreground blue))
  (ivy-grep-info                                (:foreground blue))
  (ivy-grep-line-number                         (:foreground mauve))
  (ivy-confirm-face                             (:foreground green))

  ;; posframe's
  (ivy-posframe                                 (:background black))
  (ivy-posframe-border                          (:inherit 'ivy-posframe))

  (vertico-multiline                            (:background peach))
  (vertico-group-title                          (:foreground peach :bold t))
  (vertico-group-separator                      (:foreground peach :strike-through t))
  (vertico-current                              (:foreground peach :bold t :background yellow-bg-2))

  (vertico-posframe-border                      (:background black-4))
  (vertico-posframe                             (:background black))
  
  (comint-highlight-prompt                      (:background peach :foreground black))

  (completions-annotations (:background nil :foreground gray-1 :italic t))

  (treemacs-directory-collapsed-face			(:foreground rosewater))
  (treemacs-directory-face						(:foreground rosewater))
  (treemacs-file-face							(:foreground rosewater))

  (treemacs-git-added-face						(:foreground peach))
  (treemacs-git-renamed-face				   	(:foreground rosewater))
  (treemacs-git-ignored-face				   	(:foreground gray-2))
  (treemacs-git-unmodified-face		   			(:foreground rosewater))
  (treemacs-git-renamed-face		   			(:foreground rosewater))
  (treemacs-git-modified-face		   			(:foreground maroon))

  ;; lets support solaire mode
  (solaire-default-face (:background black-1))
  ;; lsp
  (lsp-headerline-breadcrumb-path-error-face (:underline (:color maroon :style 'wave)
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
  (lsp-ui-sideline-current-symbol					(:foreground sky))
  (lsp-ui-sideline-symbol							(:foreground gray-1))

  ;; dashboard
  (dashboard-heading								(:foreground mauve :bold t))
  (dashboard-items-face								(:bold nil :foreground rosewater))
  (dashboard-banner-logo-title						(:bold t :height 200))
  (dashboard-no-items-face							(:foreground gray-2))

  ;; all-the-icons
  (all-the-icons-dgreen							(:foreground green))
  (all-the-icons-green							(:foreground green))
  (all-the-icons-dpurple						(:foreground mauve))
  (all-the-icons-purple							(:foreground mauve))

  ;; evil
  (evil-ex-lazy-highlight         (:foreground yellow :background yellow-bg :bold t))
  (evil-ex-substitute-matches     (:background red-bg :foreground red :strike-through t))
  (evil-ex-substitute-replacement (:foreground blue :background blue-bg-2 :bold t))
  (evil-search-highlight-persist-highlight-face (:background yellow))

  (ansi-color-black (:background black-2))

  (term (:background black-2 :foreground rosewater))
  (term-color-blue (:background blue :foreground blue))
  (term-color-bright-blue (:inherit 'term-color-blue))
  (term-color-red (:background red :foreground red))
  (term-color-bright-red (:background maroon :foreground maroon))
  (term-color-yellow (:background yellow :foreground yellow))
  (term-color-bright-yellow (:background yellow :foreground yellow))

  (term-color-green (:background green :foreground green))
  (term-color-bright-green (:inherit 'term-color-green))

  (term-color-bright-black (:background black-1 :foreground red))
  (term-color-white (:background rosewater :foreground rosewater))
  (term-color-bright-white (:background white :foreground white))
  (term-color-cyan (:background sky :foreground sky))
  (term-color-bright-cyan (:background sky :foreground sky))
  (term-color-magenta (:background mauve :foreground mauve))
  (term-color-bright-magenta (:background mauve :foreground mauve))
  (term-underline (:background mauve :foreground blue))

  (vterm-color-black (:background black-1 :foreground black-1))
  (vterm-color-blue (:background blue :foreground blue))
  (vterm-color-cyan (:background sky :foreground sky))
  (vterm-color-green (:background green :foreground green))
  (vterm-color-magenta (:background maroon :foreground maroon))
  (vterm-color-yellow (:background peach :foreground yellow))
  (vterm-color-red (:background red :foreground red))
  (vterm-color-white (:background rosewater :foreground rosewater))
  
  (popup-face (:inherit 'tooltip))
  (popup-selection-face (:inherit 'tooltip))
  (popup-tip-face (:inherit 'tooltip))

  (anzu-match-1 (:foreground green :background black-2))
  (anzu-match-2 (:foreground yellow :background black-2))
  (anzu-match-3 (:foreground teal :background black-2))

  (anzu-mode-line		(:foreground black :background mauve))
  (anzu-mode-no-match	(:foreground rosewater :background red))
  (anzu-replace-to		(:foreground yellow :background yellow-bg))

  (ace-jump-face-background (:foreground gray))
  (ace-jump-face-foreground (:foreground red :background black :bold t))


  (hydra-face-amaranth		(:foreground mauve))
  (hydra-face-blue			(:foreground blue))
  (hydra-face-pink			(:foreground pink))
  (hydra-face-red			(:foreground red))
  (hydra-face-teal			(:foreground teal))

  ;; Bookmarks
  (bm-fringe-face                           (:background red :foreground black))
  (bm-fringe-persistent-face                (:background red :foreground black))

  (centaur-tabs-active-bar-face				(:background black-2 :foreground rosewater))
  (centaur-tabs-selected					(:background black-2 :foreground rosewater :bold t))
  (centaur-tabs-selected-modified			(:background black-2 :foreground rosewater))
  (centaur-tabs-modified-marker-selected	(:background black-2 :foreground rosewater))
  (centaur-tabs-close-selected				(:inherit 'centaur-tabs-selected))

  (centaur-tabs-unselected					(:background black-1 :foreground gray-2))
  (centaur-tabs-unselected-modified			(:background black-1 :foreground mauve))
  (centaur-tabs-modified-marker-unselected	(:background black-1 :foreground gray-2))
  (centaur-tabs-close-unselected			(:background black-1 :foreground gray-2))

  (centaur-tabs-close-mouse-face			(:background nil :foreground red))
  (centaur-tabs-default						(:background black-1))
  (centaur-tabs-name-mouse-face				(:foreground blue :bold t))

  (git-gutter:added                              (:foreground green))
  (git-gutter:deleted                            (:foreground red))
  (git-gutter:modified                           (:foreground blue))
  
  (tree-sitter-hl-face:attribute                 (:foreground blue))
  (tree-sitter-hl-face:escape                    (:foreground blue))
  (tree-sitter-hl-face:constructor               (:foreground red :weight 'semi-bold))
  
  (tree-sitter-hl-face:constant                  (:foreground yellow))
  (tree-sitter-hl-face:constant.builtin          (:foreground yellow :weight 'semi-bold))

  (tree-sitter-hl-face:embedded                  (:foreground lavender))
  
  (tree-sitter-hl-face:function                  (:foreground peach))
  (tree-sitter-hl-face:function.builtin          (:foreground peach))
  (tree-sitter-hl-face:function.call             (:foreground mauve))
  (tree-sitter-hl-face:function.macro            (:foreground blue))
  (tree-sitter-hl-face:function.special          (:foreground peach))
  (tree-sitter-hl-face:function.label            (:foreground peach))
 
  (tree-sitter-hl-face:method                    (:foreground lavender))
  (tree-sitter-hl-face:method.call               (:foreground lavender))

  (tree-sitter-hl-face:property                  (:foreground flamingo))
  (tree-sitter-hl-face:property.definition       (:foreground flamingo :italic t))
  
  (tree-sitter-hl-face:tag                       (:foreground yellow))

  (tree-sitter-hl-face:type                      (:foreground mauve :bold t))
  (tree-sitter-hl-face:type.argument             (:foreground mauve :bold t))
  (tree-sitter-hl-face:type.builtin              (:foreground mauve))
  (tree-sitter-hl-face:type.parameter            (:foreground mauve))
  (tree-sitter-hl-face:type.super                (:foreground mauve :bold t))

  (tree-sitter-hl-face:variable                  (:foreground rosewater :italic t))
  (tree-sitter-hl-face:variable.builtin          (:foreground rosewater))
  (tree-sitter-hl-face:variable.parameter        (:foreground rosewater :italic t))
  (tree-sitter-hl-face:variable.special          (:foreground rosewater))
  (tree-sitter-hl-face:variable.synthesized      (:foreground rosewater))

  (tree-sitter-hl-face:number                    (:foreground pink))
  (tree-sitter-hl-face:operator                  (:foreground pink :bold t))
  
  (tree-sitter-hl-face:punctuation               (:foreground red))
  (tree-sitter-hl-face:punctuation.bracket       (:foreground red :bold t))
  (tree-sitter-hl-face:punctuation.delimiter     (:foreground red :bold t))
  (tree-sitter-hl-face:punctuation.special       (:foreground red))

  (tree-sitter-hl-face:case-pattern              (:foreground red))
  (tree-sitter-hl-face:variable.synthesized      (:foreground red))
  (tree-sitter-hl-face:keyword.compiler          (:foreground red :bold t :italic t))

 ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'catppuccin)
;;; catppuccin-theme.el ends here
