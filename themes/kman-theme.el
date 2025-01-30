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
  (light      "#FFFFFF" "#FFFFFF")  ; White
  (gray       "#E6E6E6" "#E6E6E6")  ; Light gray))
  (text       "#CCCCCC" "#CCCCCC")  ; Lightest gray
  (flamingo   "#C2C2C2" "#C2C2C2")
  (pink       "#B8B8B8" "#B8B8B8")
  (mauve      "#AEAEAE" "#AEAEAE")
  (red        "#A4A4A4" "#A4A4A4")
  (maroon     "#9A9A9A" "#9A9A9A")
  (peach      "#909090" "#909090")
  (yellow     "#868686" "#868686")
  (green      "#7C7C7C" "#7C7C7C")
  (teal       "#727272" "#727272")
  (sky        "#686868" "#686868")
  (blue       "#5E5E5E" "#5E5E5E")
  (lavender   "#545454" "#545454")
  (rosewater  "#4A4A4A" "#4A4A4A")
  (subtext1   "#404040" "#404040")
  (subtext0   "#363636" "#363636")
  (overlay2   "#2C2C2C" "#2C2C2C")
  (overlay1   "#222222" "#222222")
  (overlay0   "#1E1E1E" "#1E1E1E")
  (surface2   "#1A1A1A" "#1A1A1A")
  (surface1   "#161616" "#161616")
  (surface0   "#141414" "#141414")
  (base       "#101010" "#101010")   ; Base color
  (mantle     "#0C0C0C" "#0C0C0C")   ; Darker than base
  (crust      "#080808" "#080808")   ; Darkest
  (accent     "#fcc43f" "#fcc43f")
  (accent2    "#289dbd" "#299dbe")
  (accent3    "#5165e6" "#4961da")
  (accent4    "#f764a1" "#f764a1")
  (accent5    "#f7a8a1" "#f7a8a1")
  (accent6    "#55ec94" "#55ec94")
)

 ;; Customize faces
 ((default                              (:background surface0 :foreground text))
  (border                               (:foreground overlay1))
  (bookmark-face                        (:foreground red))
  (button                               (:foreground blue))
  (child-frame                          (:foreground mantle))
  (child-frame-border                   (:foreground mantle))
  (cursor                               (:background blue :foreground crust))
  (error                                (:foreground red))
  (link                                 (:foreground blue))
  (fringe                               (:foreground surface0))
  (file-name-shadow                     (:foreground accent6))
  (glyph-face                           (:background red :foreground overlay0))
  (glyphless-char                       (:foreground overlay2))
  (header-line                          (:background mantle :foreground maroon :weight 'semi-bold :italic t))
  (highlight                            (:background mantle :foreground blue :distant-foreground crust))
  (hl-line                              (:background surface0))
  (homoglyph                            (:foreground teal))
  (line-number                          (:foreground subtext1))
  (line-number-current-line             (:background surface0 :foreground accent :bold t))
  (match                                (:background yellow :foreground crust))
  (menu                                 (:foreground rosewater))
  (fill-column-indicator                (:foreground surface0))
  (mode-line                            (:background surface0 :foreground overlay2))
  (mode-line-inactive                   (:background surface0))
  (mode-line-active                     (:foreground subtext0 :background crust))
  (mode-line-highlight                  (:foreground flamingo))
  (mode-line-buffer-id                  (:foreground text :bold t))
  (numbers                              (:background peach))
  (region                               (:background text :foreground crust))
  (tooltip                              (:background overlay2 :foreground crust))
  (shadow                               (:foreground overlay2))
  (success                              (:foreground green))
  (vertical-border                      (:foreground surface0))
  (warning                              (:foreground yellow))
  (window-divider                       (:foreground base :distant-foreground mantle))

  (whitespace-newline                   (:foreground surface2))
  (whitespace-space                     (:foreground surface2))
  (whitespace-trailing                  (:foreground mantle :background red))

  ;; ;; Font lock
  (font-lock-bracket-face               (:foreground pink))
  (font-lock-builtin-face               (:foreground mauve))
  (font-lock-comment-delimiter-face     (:foreground rosewater :italic t))
  (font-lock-comment-face               (:foreground rosewater :italic t))
  (font-lock-constant-face              (:foreground accent :bold t))
  (font-lock-delimiter-face             (:foreground accent))
  (font-lock-doc-face                   (:foreground rosewater))
  (font-lock-doc-markup-face            (:foreground rosewater))
  (font-lock-escape-face                (:foreground accent))
  (font-lock-function-call-face         (:foreground yellow :weight 'normal))
  (font-lock-function-name-face         (:foreground yellow))
  (font-lock-keyword-face               (:foreground gray :weight 'semi-bold))
  (font-lock-misc-punctuation-face      (:foreground yellow))
  (font-lock-negation-char-face         (:foreground red))
  (font-lock-number-face                (:foreground accent5))
  (font-lock-operator-face              (:foreground accent4))
  (font-lock-preprocessor-face	   	(:foreground accent3))
  (font-lock-property-name-face	        (:foreground accent2 :weight 'semi-bold))
  (font-lock-property-use-face	        (:foreground light :weight 'thin))
  (font-lock-punctuation-face           (:foreground accent2 :weight 'normal))
  (font-lock-reference-face		(:foreground sky))
  (font-lock-regexp-grouping-backslash  (:foreground blue :weight 'semi-bold))
  (font-lock-string-face                (:foreground accent6 :italic t))
  (font-lock-type-face                  (:foreground red))
  (font-lock-variable-name-face         (:foreground flamingo :weight 'light))
  (font-lock-variable-use-face          (:foreground lavender :weight 'light))
  (font-lock-warning-face               (:inherit 'warning))

  (swift-ts-face-annotation                      (:foreground blue))
  (swift-ts-face-annotation.builtin              (:foreground blue))
  (swift-ts-face-annotation.type                 (:foreground green))

  (swift-ts-face-punctuation.type                (:inherit 'font-lock-punctuation-face))
  (swift-ts-face-compiler                        (:inherit 'font-lock-builtin-face))
  (swift-ts-face-constructor.call                (:inherit 'font-lock-function-call-face))

  (swift-ts-face-face-label                      (:foreground green))
  (swift-ts-face-method.call                     (:inherit 'font-lock-function-call-face))
  (swift-ts-face-method.name                     (:inherit 'font-lock-function-name-face))
  (swift-ts-face-keyword.annotation              (:foreground pink :background green :weight 'bold))
  (swift-ts-face-keyword.type                    (:inherit 'font-lock-type-face))
  (swift-ts-face-variable.synthesized            (:foreground blue))

  (highlight-operators-face             (:foreground red))
  (highlight-quoted-symbol              (:foreground maroon))
  (highlight-numbers-face               (:foreground pink))
  (highlight-symbol-face                (:background surface2 :foreground text))
  (info-xref                            (:foreground yellow))

  (minibuffer-prompt-end                (:foreground red))
  (minibuffer-prompt                    (:foreground red))
  (epa-mark                             (:foreground pink))
  (dired-mark                           (:foreground pink))
  (dired-ignored                        (:background peach))

  (iedit-occurrence                     (:background blue :foreground crust))
  (iedit-read-only-occurrence           (:background green :foreground crust))
  
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
  (doom-modeline-buffer-file            (:foreground text :bold t))
  (doom-modeline-buffer-major-mode      (:foreground maroon :bold t))
  (doom-modeline-buffer-modified        (:foreground text :italic t :bold t))
  (doom-modeline-error                  (:background red))
  (doom-modeline-info                   (:foreground subtext1 :bold t))
  (doom-modeline-time                   (:foreground overlay2 :weight 'semi-bold))
  (doom-modeline-project-dir            (:foreground blue))
  (doom-modeline-bar                    (:background yellow))
  (doom-modeline-bar-inactive           (:inherit 'mode-line-inactive))
  (doom-modeline-panel                  (:background blue :foreground crust :bold t))
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
  (custom-link                          (:foreground blue :underline t))

  (avy-lead-face                        (:background red :foreground crust))
  (avy-lead-face-0                      (:background blue :foreground crust))
  (avy-lead-face-1                      (:background green :foreground crust))
  (avy-lead-face-2                      (:background rosewater :foreground crust))
  
  ;; org-mode
  (org-done                             (:foreground overlay2))
  (org-code                             (:background crust))
  (org-meta-line                        (:background surface1 :foreground blue))
  (org-block                            (:background crust))
  (org-block-begin-line                 (:background crust :foreground overlay2))
  (org-block-end-line	                (:background crust :foreground overlay2))
  (org-headline-done                    (:foreground overlay2 :strike-through t))
  (org-todo                             (:foreground green :bold t))
  (org-headline-todo                    (:foreground base))
  (org-upcoming-deadline                (:foreground red))
  (org-footnote                         (:foreground green))
  (org-indent                           (:foreground base))
  (org-hide                             (:foreground base))
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
  (xref-match		                    (:foreground peach))

  ;; rainbow delimiter
  (rainbow-delimiters-mismatched-face   (:foreground crust :background red))
  (rainbow-delimiters-unmatched-face    (:foreground crust :background red))
  (rainbow-delimiters-base-error-face   (:foreground crust :background red))

  (rainbow-delimiters-base-face         (:foreground overlay2))

  (rainbow-delimiters-depth-1-face      (:foreground blue))
  (rainbow-delimiters-depth-2-face      (:foreground flamingo))
  (rainbow-delimiters-depth-3-face      (:foreground red))
  (rainbow-delimiters-depth-4-face      (:foreground sky))
  (rainbow-delimiters-depth-5-face      (:foreground mauve))
  (rainbow-delimiters-depth-6-face      (:foreground blue))
  (rainbow-delimiters-depth-7-face      (:foreground green))
  (rainbow-delimiters-depth-8-face      (:foreground yellow))
  (rainbow-delimiters-depth-9-face      (:foreground sky))

  ;; show-paren
  (show-paren-match						(:background maroon :foreground crust :bold t))
  (show-paren-match-expression			(:background maroon :foreground crust :bold t))
  (show-paren-mismatch					(:background red))

  (company-tooltip                          (:background surface0 :foreground text))
  (company-tooltip-common                   (:foreground peach :distant-foreground crust :bold t))
  (company-tooltip-search                   (:foreground yellow))
  (company-tooltip-selection                (:background surface1 :bold t :underline t))
  (company-tooltip-mouse                    (:foreground crust :distant-foreground text))
  (company-tooltip-annotation               (:foreground overlay0 :distant-foreground yellow))
  (company-tooltip-scrollbar-track          (:background peach))
  (company-tooltip-scrollbar-thumb          (:background flamingo))
  (company-tooltip-quick-access             (:foreground surface0))
  (company-tooltip-quick-access-selection   (:foreground peach))
  (company-scrollbar-bg                     (:inherit 'tooltip))
  (company-scrollbar-fg                     (:background red))
  (company-preview                          (:foreground red))
  (company-preview-common                   (:background red :foreground crust))
  (company-preview-search                   (:inherit 'company-tooltip-search))
  (company-template-field                   (:inherit 'match))

  (markdown-hr-face                     (:foreground surface0))

  ;; Flycheck
  (flycheck-posframe-background-face    (:background crust))
  (flycheck-posframe-face               (:background crust))
  (flycheck-fringe-warning              (:inherit 'warning))
  (flycheck-fringe-error                (:inherit 'error))
  (flycheck-fringe-info                 (:inherit 'info ))
  (flycheck-error-list-warning          (:inherit 'warning :bold t))
  (flycheck-error-list-error            (:inheirt 'error :bold t))
  (flycheck-error-list-info             (:background blue :bold t))
  (flycheck-inline-error                (:background red :foreground red :height 128))
  (flycheck-inline-info                 (:background blue :foreground blue :height 128))
  (flycheck-inline-warning              (:background yellow :foreground yellow :height 128))
  
  ;; indent dots
  (highlight-indent-guides-character-face       (:foreground red :background red))
  (highlight-indent-guides-stack-character-face (:foreground red :background red))
  (highlight-indent-guides-top-character-face   (:foreground red :background red))
  (highlight-indent-guides-stack-odd-face       (:foreground red :background red))
  (highlight-indent-guides-stack-even-face      (:foreground red :background red))
  (highlight-indent-guides-even-face            (:foreground red :background red))
  (highlight-indent-guides-odd-face             (:foreground red :background red))
  (highlight-indent-guides-top-odd-face         (:foreground red :background red))
  (highlight-indent-guides-top-even-face        (:foreground red :background red))

   ;;;; ivy
  (ivy-current-match                            (:background blue :foreground crust :bold t))
  (ivy-action                                   (:background crust :foreground lavender))
  (ivy-grep-line-number                         (:background crust :foreground peach))
  (ivy-minibuffer-match-face-1                  (:background crust :foreground blue :bold t))
  (ivy-minibuffer-match-face-2                  (:background crust :foreground green))
  (ivy-minibuffer-match-highlight               (:foreground blue))
  (ivy-grep-info                                (:foreground blue))
  (ivy-grep-line-number                         (:foreground mauve))
  (ivy-confirm-face                             (:foreground green))

  (vertico-multiline                            (:background crust))
  (vertico-group-title                          (:foreground subtext1 :weight 'semi-bold :height 180))
  (vertico-group-separator                      (:foreground overlay1 :strike-through t))
  (vertico-current                              (:background accent :distant-foreground crust :bold t :foreground crust))

  (vertico-posframe-border                      (:background crust))
  (vertico-posframe                             (:background crust :foreground subtext0))

  (corfu-annotations                            (:foreground overlay0))
  (corfu-current                                (:inherit 'vertico-current))
  (corfu-border                                 (:background surface1))
  (corfu-bar                                    (:background yellow))
  (corfu-default                                (:background crust :foreground text))
  (corfu-popupinfo                              (:background surface1 :foreground text :weight 'light :italic t))
  
  ;; posframe's
  (ivy-posframe                                 (:background surface2))
  (ivy-posframe-border                          (:inherit 'ivy-posframe))

  (orderless-match-face-0                       (:foreground accent2 :distant-foreground crust :weight 'bold))
  (orderless-match-face-1                       (:foreground accent :weight 'bold))
  (orderless-match-face-2                       (:foreground accent4 :weight 'bold))
  (orderless-match-face-3                       (:foreground accent5 :weight 'bold))

  (comint-highlight-prompt                      (:background accent :foreground crust))

  (completions-annotations                      (:foreground yellow :distant-foreground crust :italic t))
  (completions-highlight                        (:foreground text :italic t))
  (completions-common-part                      (:foreground yellow :distant-foreground crust :distant-background green :bold t :italic t))
  (completions-first-difference                 (:foreground red))

  (consult-file                                 (:foreground text :distant-foreground crust))

  (diff-added (:background green :foreground text))
  (diff-changed (:background yellow :foreground crust))
  
  (treemacs-directory-collapsed-face			(:foreground subtext0))
  (treemacs-directory-face						(:foreground subtext1))
  (treemacs-file-face							(:foreground subtext1))
  (treemacs-fringe-indicator-face               (:foreground red))

  (treemacs-git-added-face					(:foreground peach))
  (treemacs-git-renamed-face				   	(:foreground rosewater))
  (treemacs-git-ignored-face				   	(:foreground overlay2))
  (treemacs-git-unmodified-face		   			(:foreground text))
  (treemacs-git-renamed-face		   			(:foreground text))
  (treemacs-git-modified-face		   			(:foreground maroon))

  ;; lets support solaire mode
  (solaire-default-face (:background mantle))
  ;; lsp
  (lsp-headerline-breadcrumb-path-error-face (:underline (:color maroon :style 'wave)
                                                         :foreground overlay2 :background crust))

  (lsp-headerline-breadcrumb-path-face				(:background overlay0))
  (lsp-headerline-breadcrumb-path-hint-face	   		(:background crust))
  (lsp-headerline-breadcrumb-path-info-face	   		(:background overlay1))
  (lsp-headerline-breadcrumb-separator-face			(:background overlay2))
  (lsp-headerline-breadcrumb-symbols-face			(:background blue))
  (lsp-headerline-breadcrumb-project-prefix-face	(:background peach))
  (lsp-headerline-breadcrumb-symbols-error-face     (:foreground red))

  (lsp-ui-doc-background							(:background crust :foreground red))
  (lsp-ui-doc-header								(:background crust :foreground red))
  (lsp-ui-peek-filename								(:foreground teal))
  (lsp-ui-sideline-code-action			   			(:foreground yellow))
  (lsp-ui-sideline-current-symbol					(:foreground sky))
  (lsp-ui-sideline-symbol							(:foreground overlay1))

  ;; dashboard
  (dashboard-items-face								(:weight 'light :height 150))
  (dashboard-banner-logo-title						(:weight 'thin :height 320))
  (dashboard-heading								(:foreground subtext1 :weight 'thin :height 170))
  (dashboard-no-items-face							(:foreground overlay2))

  ;; all-the-icons
  (all-the-icons-dgreen							(:foreground green))
  (all-the-icons-green							(:foreground green))
  (all-the-icons-dpurple						(:foreground mauve))
  (all-the-icons-purple							(:foreground mauve))

  ;; evil
  (evil-ex-lazy-highlight           (:foreground accent2 :background yellow :bold t))
  (evil-ex-substitute-matches       (:foreground accent :strike-through t))
  (evil-ex-substitute-replacement   (:foreground accent4 :bold t))
  (evil-search-highlight-persist-highlight-face (:background yellow))
  (evil-quickscope-first-face       (:foreground yellow :underline t))
  (evil-quickscope-second-face      (:foreground peach :underline t))
  (evil-goggles-default-face        (:background peach))
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
  (hydra-face-blue		(:foreground blue))
  (hydra-face-pink		(:foreground pink))
  (hydra-face-red		(:foreground red))
  (hydra-face-teal		(:foreground teal))

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

  (centaur-tabs-close-mouse-face			(:foreground red))
  (centaur-tabs-default						(:background mantle))
  (centaur-tabs-name-mouse-face				(:foreground blue :bold t))

  (git-gutter:added                              (:foreground green))
  (git-gutter:deleted                            (:foreground red))
  (git-gutter:modified                           (:foreground blue))

  (goggles-added (:background green))
  (goggles-changed (:background blue))
  (goggles-removed (:background red))

  (punch-line-evil-normal-face  (:foreground crust :background text :weight 'bold))
  (punch-line-evil-visual-face  (:foreground crust :background accent5 :weight 'bold))
  (punch-line-evil-replace-face (:foreground crust :background accent4 :weight 'bold))
  (punch-line-evil-insert-face  (:foreground crust :background accent :weight 'bold))
  (punch-line-project-face      (:foreground blue :weight 'bold))
  (punch-line-buffer-name-face  (:foreground sky :weight 'bold))
  (punch-line-time-face         (:foreground crust))
  (punch-line-major-mode-face   (:foreground light))
  (punch-line-separator-face    (:foreground subtext0 :weight 'thin))

  
  (flycheck-overlay-error (:background overlay1 :foreground accent4 :height 0.9))
  (flycheck-overlay-warning (:background overlay1 :foreground accent :height 0.9))
  (flycheck-overlay-info (:background overlay1 :foreground accent2 :height 0.9))
  (flycheck-overlay-marker (:foreground accent5 :weight 'semi-bold :italic t :height 0.9))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kman)
;;; kman-theme.el ends here
