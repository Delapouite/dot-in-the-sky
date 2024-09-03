;;; theme.el -*- lexical-binding: t; -*-

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "Iosevka" :size 24))
(setq doom-variable-pitch-font (font-spec :family "Liberation Serif" :size 24))
(setq doom-unicode-font (font-spec :family "Noto Color Emoji" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)
;; ~/.config/emacs/.local/straight/repos/themes
(setq
  dracula-background "#282a36"
  dracula-current-line "#44475a"
  dracula-comment "#6272a4"
  dracula-foreground "#f8f8f2"

  dracula-cyan "#8be9fd"
  dracula-green "#50fa7b"
  dracula-orange "#ffb86c"
  dracula-pink "#ff79c6"
  dracula-purple "#bd93f9"
  dracula-red "#ff5555"
  dracula-yellow "#f1fa8c"

  ; unofficial
  dracula-grey "#adb2cb")

(custom-set-faces!
  `(bold :weight bold :foreground ,dracula-orange)
  ;; yellow titles: in regular buffer and sidebar
  `(org-document-title :weight: light :height 2.0 :foreground ,dracula-foreground :family "Iosevka Etoile")
  `(org-roam-header-line :weight bold :height 2.0 :foreground ,dracula-foreground)
  ;; cyan Links / Backlinks titles in sidebar
  `(magit-section-heading :height 1.4 :foreground ,dracula-cyan)
  ;; outline in sidebar
  `(org-roam-olp :foreground ,dracula-comment)
  ;; nodes titles in sidebar
  `(org-roam-title :weight bold :height 1.4)
  `(org-meta-line :foreground ,dracula-comment)
  `(org-document-info-keyword :foreground ,dracula-comment)
  `(org-special-keyword :foreground ,dracula-comment)
  `(org-property-value :foreground ,dracula-grey)
  `(org-document-info :foreground ,dracula-grey)
  `(org-agenda-done :foreground ,dracula-grey)
  `(org-headline-done :foreground ,dracula-grey)
  `(org-quote :slant italic :foreground ,dracula-grey)
  `(org-link :weight normal :foreground ,dracula-foreground)
  `(org-block :foreground ,dracula-grey)
  `(org-inline-src-block :foreground ,dracula-grey)
  `(org-table :foreground ,dracula-grey)
  `(outline-1 :height 1.4 :foreground ,dracula-foreground :family "Iosevka Etoile")
  `(outline-2 :height 0.8 :foreground ,dracula-foreground)
  `(outline-3 :foreground ,dracula-foreground))
