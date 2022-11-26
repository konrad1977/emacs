;;; commentary: kman based theme

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))


(autothemer-deftheme
	kman "A dark KMan theme"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette

  (rosewater  "#FFFBFB" "#ffffff")
  (flamingo   "#D19A66" "#ffd7df")
  (pink       "#C678DD" "#d7afaf")
  (mauve      "#D55986" "#d7afd7")
  (red        "#E06D76" "#ff87af")
  (maroon     "#eba0ac" "#ffafaf")
  (peach      "#EC8077" "#ffaf87")
  (yellow     "#FDD153" "#ffd7af")
  (green      "#4CB78B" "#87afaf")
  (teal       "#76B0A0" "#afd7d7")
  (sky        "#1BE8AC" "#afffff")
  (sapphire   "#56B6C2" "#afffff")
  (blue       "#61AFEE" "#00d7ff")
  (lavender   "#b4befe" "#d7d7ff")
  (text-light "#FFFFFF" "#ffffff")
  (text       "#C7CBD3" "#ffffff")
  (subtext1   "#ABB2BF" "#ffffff")
  (subtext0   "#5C6370" "#ffffff")
  (overlay2   "#9399b2" "#ffffff")
  (overlay1   "#7f849c" "#ffffff")
  (overlay0   "#6c7086" "#ffffff")
  (surface2   "#3B414C" "#ffffff")
  (surface1   "#2D323C" "#ffffff")
  (surface0   "#282C34" "#ffffff")
  (base       "#21252B" "#ffffff")
  (mantle     "#191C20" "#ffffff")
  (crust      "#181A1F" "#ffffff")
  (dark       "#111316" "#000000"))

 ;; Customize faces
 (
  (default								(:foreground text :background base))
  (border                               (:background base :foreground base))
  (bookmark-face                        (:foreground red :background base))
  (button                               (:foreground blue))
  (child-frame		                    (:background base))
  (child-frame-border                   (:background base))
  (cursor                               (:background mauve :foreground base))
  (error                                (:foreground red))
  (fringe                               (:background base :foreground surface1))
  (glyph-face                           (:background red))
  (glyphless-char                       (:foreground overlay2))
  (header-line							(:background base))
  (highlight                            (:background surface1 :foreground lavender))
  (hl-line                              (:background surface0))
  (homoglyph                            (:foreground teal))
  (internal-border                      (:background base :foreground base))
  (line-number                          (:foreground subtext0 :background nil))
  (line-number-current-line             (:foreground yellow :background surface0 :bold t))
  (lv-separator                         (:foreground overlay2 :background base))
  (match                                (:background yellow :foreground crust))
  (menu                                 (:background base :foreground rosewater))
  (fill-column-indicator                (:foreground surface0))
  (mode-line                            (:background mantle))
  (mode-line-inactive                   (:background surface0 :foreground subtext0 :bold nil))
  (mode-line-active		                (:background dark :foreground subtext1 :bold t))
  (mode-line-highlight                  (:foreground flamingo))
  (mode-line-buffer-id                  (:foreground subtext0 :bold t))
  (numbers                              (:background green :foreground base))
  (region                               (:background dark :foreground yellow))
  (separator-line                       (:background red))
  (shadow                               (:foreground peach))
  (success                              (:foreground green))
  (vertical-border                      (:foreground surface0 :background red))
  (warning                              (:foreground flamingo))
  (window-border                        (:foreground surface0))
  (window-divider                       (:foreground surface0))

  ;; ;; Font lock
  (font-lock-keyword-face               (:foreground sapphire :weight 'semi-bold))
  (font-lock-type-face                  (:foreground peach))
  (font-lock-regexp-grouping-backslash  (:foreground text))
  (font-lock-warning-face               (:inherit 'warning))
  (font-lock-constant-face              (:foreground yellow))
  (font-lock-string-face                (:foreground green :italic t))
  (font-lock-builtin-face               (:foreground red))
  (font-lock-reference-face				(:foreground text))
  (font-lock-function-name-face         (:foreground teal))
  (font-lock-variable-name-face         (:foreground subtext1 :italic t))
  (font-lock-negation-char-face         (:foreground red))
  (font-lock-comment-face               (:foreground subtext0 :italic t))
  (font-lock-comment-delimiter-face     (:foreground overlay2 :italic t))
  (font-lock-doc-face                   (:foreground overlay2))
  (font-lock-doc-markup-face            (:foreground overlay2))
  (font-lock-preprocessor-face	   		(:foreground overlay2))
  (elisp-shorthand-font-lock-face       (:foreground text-light))
  
  (highlight-operators-face             (:foreground red))
  (highlight-quoted-symbol              (:foreground maroon))
  (highlight-numbers-face               (:foreground pink))
  (highlight-symbol-face                (:background mantle :foreground green :weight 'semi-bold))
  (info-xref                            (:foreground yellow))

  (minibuffer-prompt-end                (:foreground red))
  (minibuffer-prompt                    (:foreground red))
  (epa-mark                             (:foreground pink))
  (dired-mark                           (:foreground pink))
  (dired-ignored                        (:background peach))

  (trailing-rosewaterspace              (:background surface1))

  ;; ;; Battery colors
  (doom-modeline-battery-critical       (:inherit 'error))
  (doom-modeline-battery-warning        (:inherit 'warning))
  (doom-modeline-battery-charging       (:foreground overlay2))
  (doom-modeline-battery-error          (:inherit 'eror))
  (doom-modeline-battery-normal         (:foreground overlay2))
  (doom-modeline-battery-full           (:foreground overlay1))

  ;; Doom visual state
  (doom-modeline-evil-motion-state      (:foreground teal))
  (doom-modeline-evil-emacs-state       (:foreground blue))
  (doom-modeline-evil-insert-state      (:foreground maroon))
  (doom-modeline-evil-normal-state      (:foreground subtext0))
  (doom-modeline-evil-visual-state      (:foreground sky))
  (doom-modeline-evil-replace-state     (:foreground red))
  (doom-modeline-evil-operator-state    (:foreground blue))

  (doom-modeline-project-dir            (:foreground overlay2))
  (doom-modeline-buffer-path            (:foreground overlay2))
  (doom-modeline-buffer-file            (:foreground overlay2 :bold t))
  (doom-modeline-buffer-major-mode      (:foreground peach :bold t))
  (doom-modeline-buffer-modified        (:foreground overlay2 :italic t :bold t))
  (doom-modeline-error                  (:background red))
  (doom-modeline-info                   (:foreground overlay2))
  (doom-modeline-project-dir            (:foreground peach))
  (doom-modeline-bar                    (:background lavender :foreground crust))
  (doom-modeline-panel                  (:inherit 'bold :background flamingo :foreground mantle))
  (doom-modeline                        (:foreground overlay1))
  (doom-themes-visual-bell              (:background red))


  (telephone-line-accent-active         (:background surface0 :foreground subtext1))
  (telephone-line-accent-inactive       (:background surface0 :foreground subtext0))
  (telephone-line-evil-normal           (:background overlay1 :foreground crust :bold t))
  (telephone-line-evil-visual           (:background yellow :foreground crust :bold t))
  (telephone-line-evil-motion           (:background blue :foreground crust :bold t))
  (telephone-line-evil-insert           (:background green :foreground crust :bold t))

  ;;elfeed
  (elfeed-search-feed-face              (:foreground lavender))
  (elfeed-search-tag-face               (:foreground green))

  ;; message colors
  (message-header-name                  (:foreground overlay2))
  (message-header-other                 (:foreground peach))
  (message-header-subject               (:foreground yellow))
  (message-header-to                    (:foreground rosewater))
  (message-header-cc                    (:foreground green))
  (message-header-xheader               (:foreground rosewater))
  (custom-link                          (:foreground blue))
  (link                                 (:foreground blue))
  
  ;; org-mode
  (org-done                             (:foreground overlay2))
  (org-code                             (:background crust))
  (org-meta-line                        (:background surface1 :foreground blue))
  (org-block                            (:background crust))
  (org-block-begin-line                 (:background crust :foreground surface2 :italic t))
  (org-block-end-line	                (:background crust :foreground surface2 :italic t))
  (org-headline-done                    (:foreground overlay2 :strike-through t))
  (org-todo                             (:foreground green :bold t))
  (org-headline-todo                    (:foreground base))
  (org-upcoming-deadline                (:foreground red))
  (org-footnote                         (:foreground green))
  (org-indent                           (:background base :foreground base))
  (org-hide	                            (:background base :foreground base))
  (org-date                             (:foreground overlay2))
  (org-ellipsis                         (:foreground overlay2 :bold t))
  (org-level-1                          (:foreground red :height 1.3 :bold t))
  (org-level-2                          (:foreground mauve :height 1.15 :bold t))
  (org-level-3                          (:foreground flamingo :height 1.05))
  (org-level-4                          (:foreground text))
  (org-level-5                          (:foreground text))
  (org-level-6                          (:foreground yellow))
  (org-level-7                          (:foreground peach))
  (org-level-8                          (:foreground maroon))

  ;; which-key
  (which-key-key-face                   (:inherit 'font-lock-variable-name-face))
  (which-func							(:inherit 'font-lock-function-name-face :bold t))
  (which-key-group-description-face     (:foreground pink))
  (which-key-command-description-face   (:foreground blue))
  (which-key-local-map-description-face (:foreground yellow))
  (which-key-posframe					(:background crust))
  (which-key-posframe-border			(:background crust))

  ;; swiper
  (swiper-line-face                     (:foreground yellow))
  (swiper-background-match-face-1       (:background peach :foreground crust))
  (swiper-background-match-face-2       (:background blue :foreground crust))
  (swiper-background-match-face-3       (:background flamingo :foreground crust))
  (swiper-background-match-face-4       (:background red :foreground crust))
  (swiper-match-face-1					(:inherit 'swiper-background-match-face-1))
  (swiper-match-face-2					(:inherit 'swiper-background-match-face-2))
  (swiper-match-face-3					(:inherit 'swiper-background-match-face-3))
  (swiper-match-face-4					(:inherit 'swiper-background-match-face-4))

  (counsel-outline-default              (:foreground yellow))
  (info-header-xref                     (:foreground yellow))
  (xref-file-header                     (:foreground yellow))
  (xref-match		                    (:foreground text-light))

  ;; rainbow delimiter
  (rainbow-delimiters-mismatched-face   (:foreground red :background yellow))
  (rainbow-delimiters-unmatched-face    (:foreground green :background yellow))
  (rainbow-delimiters-base-error-face   (:foreground red :background yellow))
  (rainbow-delimiters-base-face         (:foreground overlay2))
  (rainbow-delimiters-depth-1-face      (:foreground text))
  (rainbow-delimiters-depth-2-face      (:foreground red))
  (rainbow-delimiters-depth-3-face      (:foreground blue))
  (rainbow-delimiters-depth-4-face      (:foreground yellow))
  (rainbow-delimiters-depth-5-face      (:foreground green))
  (rainbow-delimiters-depth-6-face      (:foreground mauve))
  (rainbow-delimiters-depth-7-face      (:foreground peach))
  (rainbow-delimiters-depth-8-face      (:foreground flamingo))
  (rainbow-delimiters-depth-9-face      (:foreground teal))

  ;; show-paren
  (show-paren-match						(:background pink :bold t))
  (show-paren-match-expression			(:background pink :bold t))
  (show-paren-mismatch					(:background red))

  (company-tooltip                          (:background surface0 :foreground text))
  (company-tooltip-common                   (:foreground text-light :distant-foreground crust :bold t))
  (company-tooltip-search                   (:foreground yellow))
  (company-tooltip-selection                (:background surface1 :bold t :underline t))
  (company-tooltip-mouse                    (:background nil :foreground crust :distant-foreground text))
  (company-tooltip-annotation               (:foreground overlay0 :distant-foreground yellow))
  (company-tooltip-scrollbar-track          (:background peach))
  (company-tooltip-scrollbar-thumb          (:background flamingo))
  (company-tooltip-quick-access             (:foreground surface0))
  (company-tooltip-quick-access-selection   (:foreground peach))
  (company-scrollbar-bg                     (:inherit 'tooltip))
  (company-scrollbar-fg                     (:background red))
  (company-preview                          (:foreground blue))
  (company-preview-common                   (:background red :foreground crust))
  (company-preview-search                   (:inherit 'company-tooltip-search))
  (company-template-field                   (:inherit 'match))

  (corfu-annotations                        (:background surface1 :foreground teal))
  (corfu-current                            (:background surface2 :foreground yellow :distant-foreground subtext0 :bold t))
  (corfu-border                             (:background mantle))
  (corfu-bar                                (:background yellow :foreground mauve))
  (corfu-default                            (:background surface0 :foreground subtext1))
  
  ;; Eldoc
  (eldoc-box-body                       (:foreground peach))
  (eldoc-box-border                     (:background nil :foreground nil))
  (markdown-hr-face                     (:foreground surface0))

  ;; Flycheck
  (flycheck-posframe-background-face	(:background crust))
  (flycheck-posframe-face				(:background crust))
  (flycheck-posframe-info-face  		(:background crust :inherit 'info))
  (flycheck-posframe-warning-face  		(:background crust :inherit 'warning))
  (flycheck-posframe-error-face  		(:background crust :inherit 'error))
  (flycheck-fringe-warning				(:inherit 'warning))
  (flycheck-fringe-error				(:inherit 'error))
  (flycheck-fringe-info					(:inherit 'info ))
  (flycheck-error-list-warning          (:inherit 'warning :bold t))
  (flycheck-error-list-error            (:inheirt 'error :bold t))
  (flycheck-error-list-info             (:inherit 'info :bold t))
  (flycheck-inline-error                (:foreground "black" :background red :height 128))
  (flycheck-inline-info                 (:foreground "black" :background blue :height 128))
  (flycheck-inline-warning              (:foreground "black" :background yellow :height 128))

  ;; indent dots
  (highlight-indent-guides-character-face       (:foreground surface0))
  (highlight-indent-guides-stack-character-face (:foreground surface0))
  (highlight-indent-guides-stack-odd-face       (:foreground surface0))
  (highlight-indent-guides-stack-even-face      (:foreground surface1))
  (highlight-indent-guides-stack-character-face (:foreground surface0))
  (highlight-indent-guides-even-face            (:foreground surface0))
  (highlight-indent-guides-odd-face             (:foreground surface1))

   ;;;; ivy
  (ivy-current-match                            (:background blue :foreground crust :bold t))
  (ivy-action                                   (:background nil :foreground lavender))
  (ivy-grep-line-number                         (:background nil :foreground peach))
  (ivy-minibuffer-match-face-1                  (:background nil :foreground blue :bold t))
  (ivy-minibuffer-match-face-2                  (:background nil :foreground green))
  (ivy-minibuffer-match-highlight               (:foreground blue))
  (ivy-grep-info                                (:foreground blue))
  (ivy-grep-line-number                         (:foreground mauve))
  (ivy-confirm-face                             (:foreground green))

  ;; posframe's
  (ivy-posframe                                 (:background surface2))
  (ivy-posframe-border                          (:inherit 'ivy-posframe))

  (vertico-multiline                            (:background crust))
  (vertico-group-title                          (:foreground subtext0 :bold t))
  (vertico-group-separator                      (:foreground overlay1 :strike-through t))
  (vertico-current                              (:background surface1 :foreground yellow :bold t :distant-foreground text-light))
  (consult-file                                 (:distant-foreground text :bold t))

  (vertico-posframe-border                      (:background surface2))
  (vertico-posframe                             (:background crust :foreground subtext0 :bold nil))

  (orderless-match-face-0                       (:foreground text-light :bold t))
  (orderless-match-face-1                       (:foreground blue :bold t))
  (orderless-match-face-2                       (:foreground green :bold t))
  (orderless-match-face-4                       (:foreground yellow :bold t))

  (comint-highlight-prompt                      (:background peach :foreground crust))

  (completions-annotations                      (:foreground subtext0 :italic t))
  (completions-common-part                      (:foreground text-light :distant-foreground crust :distant-background green :bold t :italic t))
  (completions-first-difference                 (:foreground subtext1))

  (treemacs-directory-collapsed-face			(:foreground subtext1))
  (treemacs-directory-face						(:foreground subtext1))
  (treemacs-file-face							(:foreground subtext1))

  (treemacs-git-added-face						(:foreground peach))
  (treemacs-git-renamed-face				   	(:foreground rosewater))
  (treemacs-git-ignored-face				   	(:foreground overlay2))
  (treemacs-git-unmodified-face		   			(:foreground subtext1))
  (treemacs-git-untracked-face		   			(:foreground subtext0))
  (treemacs-git-renamed-face		   			(:foreground subtext1))
  (treemacs-git-modified-face		   			(:foreground maroon :italic t))

  ;; lets support solaire mode
  (solaire-default-face (:background mantle))
  ;; lsp
  (lsp-headerline-breadcrumb-path-error-face (:underline (:color maroon :style 'wave)
                                                         :foreground overlay2 :background crust))

  (lsp-headerline-breadcrumb-path-face				(:background crust))
  (lsp-headerline-breadcrumb-path-hint-face	   		(:background crust))
  (lsp-headerline-breadcrumb-path-info-face	   		(:background crust))
  (lsp-headerline-breadcrumb-separator-face			(:background crust))
  (lsp-headerline-breadcrumb-symbols-face			(:background crust))
  (lsp-headerline-breadcrumb-project-prefix-face	(:background crust))
  (lsp-headerline-breadcrumb-symbols-error-face     (:foreground red))

  (lsp-ui-doc-background							(:background yellow :foreground red))
  (lsp-ui-doc-header								(:background yellow :foreground red))
  (lsp-ui-doc-border								(:foreground nil))
  (lsp-ui-peek-filename								(:foreground teal))
  (lsp-ui-sideline-code-action			   			(:foreground yellow))
  (lsp-ui-sideline-current-symbol					(:foreground sky))
  (lsp-ui-sideline-symbol							(:foreground overlay1))

  ;; dashboard
  (dashboard-heading								(:foreground mauve :bold t))
  (dashboard-items-face								(:bold nil :foreground text))
  (dashboard-banner-logo-title						(:bold t :height 200))
  (dashboard-no-items-face							(:foreground overlay2))

  ;; all-the-icons
  (all-the-icons-dgreen							(:foreground green))
  (all-the-icons-green							(:foreground green))
  (all-the-icons-dpurple						(:foreground mauve))
  (all-the-icons-purple							(:foreground mauve))

  ;; evil
  (evil-ex-lazy-highlight           (:foreground yellow :bold t))
  (evil-ex-substitute-matches       (:foreground red :strike-through t))
  (evil-ex-substitute-replacement   (:foreground blue :bold t))
  (evil-search-highlight-persist-highlight-face (:background yellow))
  (evil-quickscope-first-face       (:foreground yellow :underline t))
  (evil-quickscope-second-face      (:foreground peach :underline t))
  (evil-goggles-default-face        (:background nil))
  (evil-goggles-join-face           (:foreground blue))
  (evil-goggles-delete-face         (:background red))
  (evil-goggles-paste-face          (:background green))
  (evil-goggles-indent-face         (:background subtext0))
  (evil-goggles-set-marker-face     (:foreground red :background red))
  (evil-goggles-yank-face           (:foreground blue :background blue))

  (ansi-color-crust (:background crust))

  (term (:background crust :foreground text))
  (term-color-blue (:background blue :foreground blue))
  (term-color-bright-blue (:inherit 'term-color-blue))
  (term-color-red (:background red :foreground red))
  (term-color-bright-red (:background maroon :foreground maroon))
  (term-color-yellow (:background yellow :foreground yellow))
  (term-color-bright-yellow (:background yellow :foreground yellow))

  (term-color-green (:background green :foreground green))
  (term-color-bright-green (:inherit 'term-color-green))

  (term-color-bright-crust (:background mantle :foreground red))
  (term-color-rosewater (:background text :foreground text))
  (term-color-bright-rosewater (:background rosewater :foreground rosewater))
  (term-color-cyan (:background sky :foreground sky))
  (term-color-bright-cyan (:background sky :foreground sky))
  (term-color-magenta (:background mauve :foreground mauve))
  (term-color-bright-magenta (:background mauve :foreground mauve))
  (term-underline (:background mauve :foreground blue))

  (vterm-color-crust (:background mantle :foreground mantle))
  (vterm-color-blue (:background blue :foreground blue))
  (vterm-color-cyan (:background sky :foreground sky))
  (vterm-color-green (:background green :foreground green))
  (vterm-color-magenta (:background maroon :foreground maroon))
  (vterm-color-yellow (:background peach :foreground yellow))
  (vterm-color-red (:background red :foreground red))
  (vterm-color-rosewater (:background text :foreground text))

  (popup-face (:inherit 'tooltip))
  (popup-selection-face (:inherit 'tooltip))
  (popup-tip-face (:inherit 'tooltip))

  (anzu-match-1 (:foreground green :background crust))
  (anzu-match-2 (:foreground yellow :background crust))
  (anzu-match-3 (:foreground teal :background crust))

  (anzu-mode-line		(:foreground crust :background mauve))
  (anzu-mode-no-match	(:foreground text :background red))
  (anzu-replace-to		(:foreground yellow :background surface2))

  (ace-jump-face-background (:foreground overlay2))
  (ace-jump-face-foreground (:foreground red :background crust :bold t))

  (hydra-face-amaranth		(:foreground mauve))
  (hydra-face-blue			(:foreground blue))
  (hydra-face-pink			(:foreground pink))
  (hydra-face-red			(:foreground red))
  (hydra-face-teal			(:foreground teal))

  ;; Bookmarks
  (bm-fringe-face                           (:background red :foreground crust))
  (bm-fringe-persistent-face                (:background red :foreground crust))

  (centaur-tabs-active-bar-face				(:background crust :foreground text))
  (centaur-tabs-selected					(:background crust :foreground text :bold t))
  (centaur-tabs-selected-modified			(:background crust :foreground text))
  (centaur-tabs-modified-marker-selected	(:background crust :foreground text))
  (centaur-tabs-close-selected				(:inherit 'centaur-tabs-selected))

  (centaur-tabs-unselected					(:background mantle :foreground overlay2))
  (centaur-tabs-unselected-modified			(:background mantle :foreground mauve))
  (centaur-tabs-modified-marker-unselected	(:background mantle :foreground overlay2))
  (centaur-tabs-close-unselected			(:background mantle :foreground overlay2))

  (centaur-tabs-close-mouse-face			(:background nil :foreground red))
  (centaur-tabs-default						(:background mantle))
  (centaur-tabs-name-mouse-face				(:foreground blue :bold t))

  (git-gutter:added                              (:foreground green))
  (git-gutter:deleted                            (:foreground red))
  (git-gutter:modified                           (:foreground blue))

  ;; Tree sitter highlightning
  (tree-sitter-hl-face:function                  (:inherit 'font-lock-function-name-face))
  (tree-sitter-hl-face:function.call             (:inherit 'tree-sitter-hl-face:function :italic nil))
  (tree-sitter-hl-face:function.builtin          (:foreground sky))
  (tree-sitter-hl-face:function.special          (:foreground text :italic t :bold t))
  (tree-sitter-hl-face:function.macro            (:foreground sapphire))

  (tree-sitter-hl-face:method                    (:inherit 'tree-sitter-hl-face:function))
  (tree-sitter-hl-face:method.call               (:inherit 'tree-sitter-hl-face:method :italic nil))

  (tree-sitter-hl-face:type                      (:inherit 'font-lock-type-face))
  (tree-sitter-hl-face:type.parameter            (:foreground sapphire :italic t))
  (tree-sitter-hl-face:type.argument             (:foreground subtext0 :background peach))
  (tree-sitter-hl-face:type.builtin              (:inherit 'font-lock-builtin-face))
  (tree-sitter-hl-face:type.super                (:foreground maroon :bold t))
  (tree-sitter-hl-face:constructor               (:foreground teal :weight 'semi-bold))

  (tree-sitter-hl-face:variable                  (:inherit 'font-lock-variable-name-face))
  (tree-sitter-hl-face:variable.parameter        (:foreground flamingo))
  (tree-sitter-hl-face:variable.builtin          (:foreground mauve :weight 'semi-bold))
  (tree-sitter-hl-face:variable.special          (:foreground mauve))
  (tree-sitter-hl-face:property                  (:foreground sky))
  (tree-sitter-hl-face:property.definition       (:foreground sky :italic t))

  (tree-sitter-hl-face:comment                   (:inherit 'font-lock-comment-face))
  (tree-sitter-hl-face:doc                       (:inherit 'font-lock-comment-face))
  (tree-sitter-hl-face:string                    (:inherit 'font-lock-string-face))
  (tree-sitter-hl-face:string.special            (:inherit 'font-lock-string-face))
  (tree-sitter-hl-face:escape                    (:inherit 'font-lock-regexp-grouping-backslash))
  (tree-sitter-hl-face:embedded                  (:foreground teal))

  (tree-sitter-hl-face:keyword                   (:inherit 'font-lock-keyword-face))
  (tree-sitter-hl-face:operator                  (:foreground blue :bold t))
  (tree-sitter-hl-face:label                     (:inherit 'tree-sitter-hl-face:keyword :italic t))
  (tree-sitter-hl-face:constant                  (:inherit 'font-lock-constant-face))
  (tree-sitter-hl-face:constant.builtin          (:foreground yellow :weight 'semi-bold))
  (tree-sitter-hl-face:number                    (:foreground peach))

  (tree-sitter-hl-face:punctuation               (:foreground maroon))
  (tree-sitter-hl-face:punctuation.bracket       (:foreground text :weight 'semi-bold))
  (tree-sitter-hl-face:punctuation.delimiter     (:foreground text :bold t))
  (tree-sitter-hl-face:punctuation.special       (:foreground yellow))

  (tree-sitter-hl-face:case-pattern              (:foreground peach))
  (tree-sitter-hl-face:keyword.compiler          (:foreground overlay2 :bold t :italic t))

  ;; Custom for pinkus tree-sitter-swift
  (tree-sitter-hl-face:include                   (:foreground subtext0 :italic t :bold t))
  (tree-sitter-hl-face:parameter                 (:foreground subtext0 :italic t))
  (tree-sitter-hl-face:repeat                    (:foreground teal :bold t))
  (tree-sitter-hl-face:boolean                   (:foreground yellow))
  (tree-sitter-hl-face:keyword.return            (:inherit 'tree-sitter-hl-face:keyword :italic t))
  (tree-sitter-hl-face:keyword.operator          (:foreground sapphire :bold t))
  (tree-sitter-hl-face:keyword.function          (:inherit 'tree-sitter-hl-face:keyword))
  (tree-sitter-hl-face:conditional               (:inherit 'tree-sitter-hl-face:keyword))

  (swift-mode:preprocessor-keyword-face (:foreground text :italic t))
  (swift-mode:property-access-face (:foreground subtext1))
  (swift-mode:builtin-property-face (:foreground maroon))
  (swift-mode:builtin-enum-case-face (:foreground teal))
  (swift-mode:builtin-method-trailing-closure-face (:foreground teal))
  (swift-mode:builtin-function-trailing-closure-face (:foreground teal))
 ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kman)
;;; kman-theme.el ends here
