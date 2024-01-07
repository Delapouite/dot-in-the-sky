;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; https://github.com/doomemacs/doomemacs/blob/master/templates/config.example.el

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "private.el")
(load! "describe.el")
(load! "doom-modeline.el")
(load! "org.el")
(load! "org-agenda.el")
(load! "org-roam.el")
(load! "org-roam-ql.el")
(load! "org-property-drawer.el")
(load! "fetcher.el")
(load! "marginalia.el")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Delapouite"
      user-mail-address "delapouite@gmail.com")

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
(setq doom-font (font-spec :family "iosevka" :size 12))

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

  dracula-grey "#adb2cb")

(custom-set-faces!
  `(bold :weight bold :foreground ,dracula-orange)
  ;; yellow titles: in regular buffer and sidebar
  `(org-document-title :height 2.0 :foreground ,dracula-yellow)
  `(org-roam-header-line :weight bold :height 2.0 :foreground ,dracula-yellow)
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
  `(org-quote :slant italic :foreground ,dracula-foreground)
  `(org-link :foreground ,dracula-cyan)
  `(outline-1 :height 1.4)
  `(outline-2 :height 0.8))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)
(setq scroll-margin 10)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :n "q" #'evil-backward-word-begin
      :n "Q" #'evil-backward-WORD-begin
      :n "U" #'evil-redo
      :n "b d" #'kill-current-buffer
      :n "b n" #'next-buffer
      :n "b p" #'previous-buffer
      :n "b s" #'save-buffer)
(map! :leader
      :desc "Fetch stats" "j" #'my/fetch-stats)

(defun iso8601-format (&optional time)
  "Format time string with %FT%T%z TIME"
  (format-time-string "%FT%T%z" time))


(setq company-selection-wrap-around t)
(setq completion-ignore-case t)

(use-package! org-download
  :config
  (setq-default org-download-image-dir "~/.cache/org-download")
  (org-download-enable))

; org-babel

(setq org-plantuml-exec-mode 'plantuml)
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lilypond . t)
   (plantuml . t)
   (emacs-lisp . nil)))

(defun my/search-cwd (prefix)
  (defun my/search-cwd-internal () (insert prefix))
  (minibuffer-with-setup-hook #'my/search-cwd-internal (call-interactively #'+default/search-cwd)))

(require 'ol-man)

(setq ispell-personal-dictionary "~/Sync/ispell.dictionary")

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(use-package! olivetti
  :config
  (setq olivetti-minimum-body-width 120)
  :hook (org-mode . olivetti-mode))
