;;; package: --- A beautiful theme inspired by neon lights -*- lexical-binding: t -*-

;;; Commentary:
;; A neon theme

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(autothemer-deftheme
 nordic "A port of the Neovim theme"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (white3    "#ECEFF4" "#eee8d5")
  (black    "#1d2129" "#1c1c1c")
  (black0   "#191D24" "#120e28")
  (black1   "#1E222A" "#1d1741")
  (black2   "#222630" "#271e56")
  (gray0    "#242933" "#201947")
  (gray1    "#2E3440" "#2e2466")
  (gray2    "#3B4252" "#352975")
  (gray3    "#434C5E" "#3e318a")
  (gray4    "#4C566A" "#322880")
  (gray5    "#69728A" "#423494")
  (gray6    "#888ea2" "#423494")
  (magent_d "#A97EA1" "#7E74CC")
  (magenta  "#B48EAD" "#7E74CC")
  (magent_b "#BE9dB8" "#7E74CC")
  (red_d    "#B74E58" "#ff1d8a")
  (red      "#BF616A" "#ff047d")
  (red_b    "#C5727A" "#e61d7e")
  (green    "#A3BE8C" "#859900")
  (green_b  "#B1c89D" "#b2c62d")
  (blue0    "#5E81AC" "#268bd2")
  (blue1    "#81A1c1" "#197ec5")
  (blue2    "#88C0D0" "#197ec5")
  (yellow_d "#E7C173" "#b58900")
  (yellow   "#EBCB8B" "#b58900")
  (yellow_b "#EDD49F" "#c4980f")
  (orange_d "#CB775D" "#c85106")
  (orange   "#D08770" "#c85106")
  (orange_b "#D79784" "#c85106")
  (cyan_d   "#80B3B2" "#74c5aa")
  (cyan     "#8FBCBB" "#37dcf6")
  )

 ;; Customize faces
 (
  (default                      (:background gray0 :foreground white3))
  (file-name-shadow             (:foreground gray5))
  (fringe                       (:background gray0))
  (hl-line                      (:background gray1))
  (header-line                  (:background black1 :height 0.8 :box (:line-width 7 :color black1)))
  (line-number                  (:foreground gray2))
  (line-number-current-line     (:background gray1 :foreground white3 :weight 'semi-bold))
  (region                       (:background magent_d :foreground black0))
  (window-divider               (:background black :foreground black))
  (window-divider-first-pixel   (:background black :foreground black))
  (window-divider-last-pixel    (:background black :foreground black))

  (mode-line                    (:background gray0 :foreground gray5 ))
  (mode-line-inactive           (:background gray0 :foreground gray6))
  (mode-line-active             (:inherit 'mode-line))
  (mode-line-highlight          (:foreground gray6))
  (mode-line-buffer-id          (:foreground blue1))

  (vertical-border              (:foreground black))
  (link                         (:foreground blue0))
  (custom-link                  (:foreground blue0))
  (warning                      (:foreground yellow_b))
  (error                        (:foreground red_b))
  (success                      (:foreground cyan_d))

  (vertico-posframe-border      (:background black))
  (vertico-posframe             (:background black :foreground gray6 :weight 'light))
  (vertico-current              (:background gray3 :foreground blue2 :bold t))
  (vertico-multiline            (:background black0))
  (vertico-group-title          (:foreground gray3 :bold t))
  (vertico-group-separator      (:foreground gray5 :strike-through t))

  (corfu-current                (:background gray3 :foreground white3 :bold t))
  (corfu-annotations            (:background black0))
  (corfu-default                (:background black0 :foreground white3))
  (corfu-border                 (:background black0))
  (corfu-popupinfo              (:background black0 :weight 'light))
  
  (eldoc-box-body               (:inherit 'vertico-posframe))
  (eldoc-box-border             (:background black2))

  (orderless-match-face-0       (:foreground red_b :weight 'bold))
  (orderless-match-face-1       (:foreground blue1 :weight 'bold))
  (orderless-match-face-2       (:foreground yellow_b :weight 'bold))
  (orderless-match-face-3       (:foreground cyan :weight 'bold))

  (minibuffer-prompt-end        (:foreground yellow_b))
  (minibuffer-prompt            (:foreground yellow_b :weight 'bold))

  (window-stool-face            (:background gray2))

  ;;treemacs
  (treemacs-directory-collapsed-face            (:foreground gray6 :weight 'thin))
  (treemacs-directory-face                      (:foreground gray6))
  (treemacs-file-face				(:foreground white3))
  (treemacs-fringe-indicator-face               (:foreground red))
  (treemacs-git-added-face			(:foreground green))
  (treemacs-git-ignored-face			(:foreground gray5))
  (treemacs-git-modified-face		   	(:foreground green_b))
  (treemacs-git-renamed-face			(:foreground yellow_d))
  (treemacs-git-renamed-face		   	(:foreground orange_d))
  (treemacs-git-unmodified-face		   	(:foreground white3))
  (treemacs-nerd-icons-file-face                (:foreground orange :weight 'semi-bold))
  (treemacs-nerd-icons-root-face                (:foreground blue2 :weight 'semi-bold))
  (treemacs-window-background-face              (:background black))

  (elisp-shorthand-font-lock-face                (:foreground red_b))

  (font-lock-bracket-face                        (:foreground blue0))
  (font-lock-builtin-face                        (:foreground yellow))
  (font-lock-comment-delimiter-face              (:inherit 'font-lock-comment-face))
  (font-lock-comment-face                        (:foreground gray5 :italic t :weight 'thin))
  (font-lock-constant-face                       (:foreground magenta :weight 'semi-bold))
  (font-lock-delimiter-face                      (:foreground red_d))
  (font-lock-doc-face                            (:inherit 'font-lock-comment-face))
  (font-lock-doc-markup-face                     (:inherit 'font-lock-doc-face))
  (font-lock-function-call-face                  (:foreground blue1))
  (font-lock-function-name-face                  (:foreground blue2 :italic t))
  (font-lock-keyword-face                        (:foreground orange_d :weight 'bold))
  (font-lock-misc-punctuation-face               (:foreground yellow_b))
  (font-lock-negation-char-face                  (:foreground red_b))
  (font-lock-number-face                         (:foreground magenta))
  (font-lock-operator-face                       (:foreground red))
  (font-lock-preprocessor-face	                 (:foreground orange))
  (font-lock-property-name-face                  (:foreground white3))
  (font-lock-property-use-face                   (:foreground magent_b))
  (font-lock-punctuation-face                    (:foreground yellow_b :weight 'normal))
  (font-lock-reference-face                      (:foreground yellow))
  (font-lock-regexp-regex-face                   (:foreground red_b))
  (font-lock-regexp-grouping-backslash           (:foreground red_b))
  (font-lock-regexp-grouping-construct           (:foreground red_b))
  (font-lock-string-face                         (:foreground green_b :italic t :weight 'light))
  (font-lock-type-face                           (:foreground yellow_d))
  (font-lock-variable-name-face                  (:foreground cyan))
  (font-lock-variable-use-face                   (:foreground cyan_d))
  (font-lock-warning-face                        (:foreground yellow_b))

  (swift-ts-face-annotation                      (:foreground red_d))
  (swift-ts-face-annotation.builtin              (:foreground cyan_d))
  (swift-ts-face-annotation.type                 (:foreground green))

  (swift-ts-face-punctuation.type                (:inherit 'font-lock-punctuation-face))
  (swift-ts-face-compiler                        (:inherit 'font-lock-builtin-face))
  (swift-ts-face-constructor.call                (:inherit 'font-lock-function-call-face))

  (swift-ts-face-face-label                      (:foreground green))
  (swift-ts-face-method.call                     (:inherit 'font-lock-function-call-face))
  (swift-ts-face-method.name                     (:inherit 'font-lock-function-name-face))
  (swift-ts-face-keyword.annotation              (:foreground red_b :background green :weight 'bold))
  (swift-ts-face-keyword.type                    (:inherit 'font-lock-type-face))
  (swift-ts-face-variable.synthesized            (:foreground cyan))
  (swift-ts-face-property.declaration            (:foreground cyan_d))
  

  (eglot-inlay-hint-face (:foreground blue0 :background black1 :height 0.8 :weight 'light))
  (eglot-parameter-hint-face (:inherit 'eglot-inlay-hint-face))
  (eglot-type-hint-face (:inherit 'eglot-inlay-hint-face))

  ;;; Rainbow-delimiters
  (rainbow-delimiters-mismatched-face            (:foreground red))
  (rainbow-delimiters-unmatched-face             (:foreground orange))
  (rainbow-delimiters-base-error-face            (:foreground red))
  (rainbow-delimiters-base-face                  (:foreground green_b))

  (rainbow-delimiters-depth-1-face               (:foreground red))
  (rainbow-delimiters-depth-2-face               (:foreground magent_d))
  (rainbow-delimiters-depth-3-face               (:foreground blue0))
  (rainbow-delimiters-depth-4-face               (:foreground cyan_d))
  (rainbow-delimiters-depth-5-face               (:foreground gray5))
  (rainbow-delimiters-depth-6-face               (:foreground blue1))
  (rainbow-delimiters-depth-7-face               (:foreground red_d))
  (rainbow-delimiters-depth-8-face               (:foreground orange))
  (rainbow-delimiters-depth-9-face               (:foreground blue0))

  (punch-line-evil-normal-face  (:foreground black0 :background orange_b :weight 'semi-bold))
  (punch-line-evil-visual-face  (:foreground black0 :background red :weight 'semi-bold))
  (punch-line-evil-replace-face (:foreground black0 :background magenta :weight 'semi-bold))
  (punch-line-evil-insert-face  (:foreground black0 :background green_b :weight 'semi-bold))
  (punch-line-project-face      (:foreground gray6 :weight 'bold))
  (punch-line-buffer-name-face  (:foreground white3 :weight 'bold))
  (punch-line-time-face         (:foreground black0))
  (punch-line-major-mode-face   (:foreground gray5))
  (punch-line-separator-face    (:foreground gray0 :weight 'thin))

  (term                         (:background black0 :foreground magenta))
  (term-color-black             (:background black0 :foreground black0))
  (term-color-yellow            (:background yellow :foreground yellow))
  (term-color-blue              (:background blue1 :foreground blue1))
  (term-color-green             (:background green :foreground green))
  (term-color-red               (:background red :foreground red))

  (ansi-color-bright-cyan       (:background cyan :foreground cyan))
  (ansi-color-blue              (:background blue0 :foreground blue0))
  (ansi-color-bright-magenta    (:background red :foreground red))

  (evil-mc-region-face          (:background cyan_d :foreground black0))
  (evil-mc-cursor-default-face  (:background blue0 :foreground black0))
  (evil-mc-cursor-bar-face      (:background black0 :foreground cyan_d))
  (evil-mc-cursor-hbar-face     (:background cyan_d :foreground cyan_d))

  (lazy-highlight               (:inherit 'info-index-match))
  (evil-ex-lazy-highlight       (:inherit 'lazy-highlight))

  (highlight-symbol-face        (:foreground yellow_d :weight 'bold :underline t))

  (smerge-base		        (:background magenta))
  (smerge-markers		(:background black1 :foreground magenta))

  (smerge-upper			(:background black2))
  (smerge-lower			(:background black2))
  (smerge-refined-change        (:background blue1))
  (smerge-refined-removed	(:background orange :strike-through t))
  (smerge-refined-added 	(:background green :foreground black0))

  (elfeed-search-tag-face (:foreground blue1))

  (git-gutter:added (:foreground green_b))
  (git-gutter:deleted (:foreground red_b))
  (git-gutter:modified (:foreground magenta))

  (package-description (:foreground orange :weight 'light))
  (org-code (:foreground cyan))

  (semel-symbol-at-mouse        (:background blue0))
  (semel-free-variable          (:inherit 'font-lock-variable-name-face))
  (semel-condition              (:foreground red_b))
  (semel-throw-tag              (:foreground yellow_b))
  (semel-condition              (:foreground red_b))
  (semel-non-local-exit         (:foreground blue1 :underline "red"))
  (semel-local-variable         (:inherit 'font-lock-property-use-face))
  (semel-binding-variable       (:inherit 'font-lock-property-name-face))

  ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nordic)
;;; nordic-theme.el ends here
