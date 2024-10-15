
;;; package: --- A theme inspired by the colors of the famous painting by Katsushika Hokusa
;;; Commentary: Original theme created by rebelot see: https://github.com/rebelot/kanagawa.nvim
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(autothemer-deftheme
 mito-laser "A theme based on mito-laser"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (white    "#eee8d5" "#eee8d5")
  (line     "#2b215f" "#2b215f")
  (darkest  "#120e28" "#120e28")
  (darker   "#1d1741" "#1d1741")
  (black    "#201947" "#201947")
  (black2   "#271e56" "#271e56")
  (one      "#2e2466" "#2e2466")
  (one2     "#352975" "#352975")
  (one3     "#3e318a" "#3e318a")
  (dark     "#322880" "#322880")
  (grey     "#423494" "#423494")
  (grey2    "#4c3ca9" "#4c3ca9")
  (grey3    "#5442bb" "#5442bb")
  (light    "#6d5dc6" "#6d5dc6")
  (purple   "#7E74CC" "#7E74CC")
  (red      "#ff047d" "#ff047d")
  (baby     "#ff1d8a" "#ff1d8a")
  (pink     "#e61d7e" "#e61d7e")
  (green    "#859900" "#859900")
  (vibrant  "#b2c62d" "#b2c62d")
  (nord     "#197ec5" "#197ec5")
  (blue     "#268bd2" "#268bd2")
  (yellow   "#b58900" "#b58900")
  (sun      "#c4980f" "#c4980f")
  (orange   "#c85106" "#c85106")
  (teal     "#74c5aa" "#74c5aa")
  (cyan     "#37dcf6" "#37dcf6")
  )

 ;; Customize faces
 (
  (default                      (:background black :foreground white))
  (fringe                       (:background black))
  (hl-line                      (:background darker))
  (header-line                  (:background darker))
  (line-number                  (:foreground grey2))
  (line-number-current-line     (:background darker :foreground white :weight 'semi-bold))
  (region                       (:background one))
  (mode-line                    (:background line))
  (mode-line-inactive           (:background one2 :foreground purple))
  (mode-line-active             (:background line :foreground purple))
  (mode-line-highlight          (:foreground red))
  (mode-line-buffer-id          (:foreground nord))
  (vertical-border              (:foreground one))
  (link                         (:foreground blue))
  (custom-link                  (:foreground blue))
  (warning                      (:foreground sun))
  (error                        (:foreground pink))
  (success                      (:foreground teal))

  (vertico-posframe-border      (:background darkest))
  (vertico-posframe             (:background darkest))
  (vertico-current              (:inherit 'region))

  (corfu-current                (:inherit 'vertico-current))
  (corfu-annotations            (:background darkest))
  (corfu-default                (:background darkest :foreground white))
  (corfu-border                 (:background darkest))
  (corfu-popupinfo              (:background darkest :foreground blue :box (:line-width 2 :color black)))

  (orderless-match-face-0                        (:foreground pink :weight 'bold))
  (orderless-match-face-1                        (:foreground nord :weight 'bold))
  (orderless-match-face-2                        (:foreground sun :weight 'bold))
  (orderless-match-face-3                        (:foreground cyan :weight 'bold))

  (minibuffer-prompt-end                         (:foreground purple :background darkest))
  (minibuffer-prompt                             (:foreground baby :background darkest :bold t))

  (window-stool-face (:background one2))

  (eldoc-box-body (:background darker :foreground purple))
  (eldoc-box-border (:background one2))

  ;;treemacs
  (treemacs-window-background-face               (:background darker))
  (treemacs-directory-face                       (:foreground purple))
  (treemacs-file-face                            (:foreground purple))
  (treemacs-nerd-icons-file-face                 (:inherit 'treemacs-file-face))
  (treemacs-nerd-icons-root-face                 (:inherit 'treemacs-directory-face))
  (treemacs-git-added-face                       (:foreground teal))
  (treemacs-git-renamed-face                     (:foreground pink))
  (treemacs-git-ignored-face                     (:foreground grey3 :italic t))
  (treemacs-git-unmodified-face                  (:foreground purple))
  (treemacs-git-untracked-face                   (:foreground vibrant))
  (treemacs-git-modified-face                    (:foreground nord))

  (font-lock-keyword-face                        (:foreground light :weight 'normal))
  (font-lock-variable-name-face                  (:foreground teal))
  (font-lock-type-face                           (:foreground pink :weight 'bold))
  (font-lock-warning-face                        (:foreground sun))
  (font-lock-string-face                         (:foreground green :italic t :weight 'normal))
  (font-lock-builtin-face                        (:foreground blue))
  (font-lock-reference-face                      (:foreground yellow))
  (font-lock-constant-face                       (:foreground teal));;
  (font-lock-comment-face                        (:foreground grey2 :italic t :weight 'normal))
  (font-lock-comment-delimiter-face              (:inherit 'font-lock-comment-face))
  (font-lock-function-name-face                  (:foreground blue))
  (font-lock-number-face                         (:foreground purple :bold t))
  (font-lock-doc-face                            (:inherit 'font-lock-comment-face))
  (font-lock-doc-markup-face                     (:inherit 'font-lock-doc-face))
  (font-lock-negation-char-face                  (:foreground pink))
  (font-lock-preprocessor-face	                 (:foreground orange))
  (elisp-shorthand-font-lock-face                (:foreground pink))
  (font-lock-regexp-grouping-backslash           (:foreground pink))
  (font-lock-operator-face                       (:foreground pink))
  (font-lock-misc-punctuation-face               (:foreground sun))
  (font-lock-punctuation-face                    (:foreground orange :weight 'normal))

  (tree-sitter-hl-face:annotation                (:foreground blue :weight 'semi-bold))
  (tree-sitter-hl-face:annotation.builtin        (:foreground nord :weight 'semi-bold))
  (tree-sitter-hl-face:annotation.type           (:foreground pink))

  (tree-sitter-hl-face:function                  (:inherit 'font-lock-function-name-face))
  (tree-sitter-hl-face:function.call             (:foreground nord :weight 'normal))
  (tree-sitter-hl-face:function.builtin          (:foreground blue))
  (tree-sitter-hl-face:function.special          (:foreground green :italic t))
  (tree-sitter-hl-face:function.macro            (:foreground blue))
  (tree-sitter-hl-face:function.label            (:foreground sun))

  (tree-sitter-hl-face:method                    (:inherit 'tree-sitter-hl-face:function))
  (tree-sitter-hl-face:method.call               (:foreground nord :weight 'normal))

  (tree-sitter-hl-face:type                      (:inherit 'font-lock-type-face))
  (tree-sitter-hl-face:type.parameter            (:foreground orange :italic t))
  (tree-sitter-hl-face:type.argument             (:foreground grey3))
  (tree-sitter-hl-face:type.builtin              (:inherit 'font-lock-builtin-face))
  (tree-sitter-hl-face:type.super                (:foreground pink))
  (tree-sitter-hl-face:constructor               (:foreground nord :weight 'normal))

  (tree-sitter-hl-face:variable                  (:foreground white))
  (tree-sitter-hl-face:variable.parameter        (:inherit 'tree-sitter-hl-face:type.parameter))
  (tree-sitter-hl-face:variable.builtin          (:foreground orange))
  (tree-sitter-hl-face:variable.special          (:foreground grey2 :italic t))
  (tree-sitter-hl-face:variable.synthesized      (:foreground vibrant))

  (tree-sitter-hl-face:property                  (:foreground teal :italic t))
  (tree-sitter-hl-face:property.definition       (:foreground teal))

  (tree-sitter-hl-face:comment                   (:inherit 'font-lock-comment-face))
  (tree-sitter-hl-face:doc                       (:inherit 'font-lock-comment-face))
  (tree-sitter-hl-face:string                    (:inherit 'font-lock-string-face))
  (tree-sitter-hl-face:string.special            (:inherit 'font-lock-string-face))
  (tree-sitter-hl-face:escape                    (:inherit 'font-lock-regexp-grouping-backslash))
  (tree-sitter-hl-face:embedded                  (:foreground yellow))

  (tree-sitter-hl-face:keyword                   (:inherit 'font-lock-keyword-face))
  (tree-sitter-hl-face:keyword.compiler          (:foreground pink :weight 'bold))
  (tree-sitter-hl-face:keyword.type              (:foreground pink))
  (tree-sitter-hl-face:operator                  (:inherit 'font-lock-operator-face))
  (tree-sitter-hl-face:label                     (:foreground grey2))
  (tree-sitter-hl-face:constant                  (:inherit 'font-lock-constant-face))
  (tree-sitter-hl-face:constant.builtin          (:inherit 'font-lock-constant-face :weight 'normal))
  (tree-sitter-hl-face:number                    (:inherit 'font-lock-number-face))

  (tree-sitter-hl-face:punctuation               (:inherit 'font-lock-punctuation-face))
  (tree-sitter-hl-face:punctuation.bracket       (:foreground red))
  (tree-sitter-hl-face:punctuation.delimiter     (:foreground red))
  (tree-sitter-hl-face:punctuation.special       (:foreground red))

  (tree-sitter-hl-face:case-pattern              (:foreground sun))

  ;;; Rainbow-delimiters
  (rainbow-delimiters-mismatched-face            (:foreground red))
  (rainbow-delimiters-unmatched-face             (:foreground orange))
  (rainbow-delimiters-base-error-face            (:foreground red))
  (rainbow-delimiters-base-face                  (:foreground vibrant))

  (rainbow-delimiters-depth-1-face               (:foreground red))
  (rainbow-delimiters-depth-2-face               (:foreground vibrant))
  (rainbow-delimiters-depth-3-face               (:foreground nord))
  (rainbow-delimiters-depth-4-face               (:foreground teal))
  (rainbow-delimiters-depth-5-face               (:foreground purple))
  (rainbow-delimiters-depth-6-face               (:foreground red))
  (rainbow-delimiters-depth-7-face               (:foreground baby))
  (rainbow-delimiters-depth-8-face               (:foreground orange))
  (rainbow-delimiters-depth-9-face               (:foreground blue))

  (cocaine-line-evil-normal-face  (:foreground white
                                   :background nord :weight 'bold
                                   :box (:line-width 8 :color nord)))

  (cocaine-line-evil-visual-face  (:foreground white
                                   :background light :weight 'bold
                                   :box (:line-width 8 :color light)))

  (cocaine-line-evil-replace-face  (:foreground white
                                   :background red :weight 'bold
                                   :box (:line-width 8 :color red)))

  (cocaine-line-evil-insert-face  (:foreground darker
                                   :background teal :weight 'bold
                                   :box (:line-width 8 :color teal)))

  (cocaine-line-project-face        (:foreground purple :weight 'bold))
  (cocaine-line-buffer-name-face    (:foreground white :weight 'bold))
  (cocaine-line-time-face           (:foreground purple))
  (cocaine-line-major-mode-face     (:foreground light))

  (term                     (:background darkest :foreground purple))
  (term-color-black         (:background darkest :foreground darkest))
  (term-color-yellow        (:background yellow :foreground yellow))
  (term-color-blue          (:background nord :foreground nord))
  (term-color-green         (:background green :foreground green))
  (term-color-red           (:background red :foreground red))

  (ansi-color-bright-cyan       (:background cyan :foreground cyan))
  (ansi-color-blue              (:background blue :foreground blue))
  (ansi-color-bright-magenta    (:background red :foreground red))

  (evil-mc-region-face          (:background teal :foreground darkest))
  (evil-mc-cursor-default-face  (:background blue :foreground darkest))
  (evil-mc-cursor-bar-face      (:background darkest :foreground teal))
  (evil-mc-cursor-hbar-face     (:background teal :foreground teal))

  (highlight-symbol-face        (:background green :foreground darkest :distant-foreground green))

  ))

(provide-theme 'mito-laser)
;;; mito-laser-theme.el ends here
