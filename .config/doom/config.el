;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; https://github.com/doomemacs/doomemacs/blob/master/templates/config.example.el

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "private.el")
(load! "theme.el")
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
(map! :leader :desc "Fetch stats" "j" #'my/fetch-stats
      :leader :desc "Clip Link" "k" #'my/org-cliplink
      :leader :desc "Toggle org-link-display" "t k" #'org-toggle-link-display)

(defun iso8601-format (&optional time)
  "Format time string with %FT%T%z TIME"
  (format-time-string "%FT%T%z" time))

(defun iso8601-to-epoch (&optional iso)
  "Parse iso8601 string to unix epoch timestamp"
  (string-to-number (format-time-string "%s" (if iso (date-to-time iso)))))

(defun iso8601-diff-days (iso)
  "Return the number of days between today and ISO"
  (let ((now (iso8601-to-epoch))
        (past (iso8601-to-epoch iso)))
    (/ (- now past) 86400)))

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
