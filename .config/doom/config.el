;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; https://github.com/doomemacs/doomemacs/blob/master/templates/config.example.el

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "private.el")
(load! "describe.el")
(load! "org.el")
(load! "property-drawer.el")
(load! "fetcher.el")

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
      :desc "Find roam" "r" #'org-roam-node-find)
(map! :leader
      :desc "Fetch stats" "j" #'my/fetch-stats)

(defun iso8601-format (&optional time)
  "Format time string with %FT%T%z TIME"
  (format-time-string "%FT%T%z" time))

(use-package! doom-modeline
  :config

  (defun get-buffer-file-mtime ()
    (let ((mtime (file-attribute-modification-time
                  (file-attributes (buffer-file-name)))))
      (when mtime
        (concat " " (iso8601-format mtime)))))

  (doom-modeline-def-segment buffer-mtime
    "Define buffer-mtime modeline segment"
    (let ((mtime (get-buffer-file-mtime)))
      (propertize mtime 'face (if (or
                                   (string-prefix-p " 2022" mtime)
                                   (string-prefix-p " 2023-01" mtime))
                                  'compilation-error 'mode-line))))

  (doom-modeline-def-modeline 'delapouite
    '(bar window-number modals matches buffer-info-simple buffer-mtime buffer-position word-count parrot selection-info)
    '(objed-state misc-info debug lsp minor-modes input-method indent-info major-mode process checker))

  (defun doom-modeline-set-delapouite-modeline ()
    "Enable delapouite modeline"
    (doom-modeline-set-modeline 'delapouite)))

(use-package! org-roam
  :custom
  (org-roam-directory "~/Sync/org/roam")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags:")
      :unnarrowed t)
     ("l" "album" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: Album\n\n* Wiki\n* Tracks")
      :unnarrowed t)
     ("p" "person" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: Person\n")
      :unnarrowed t)
     ("r" "artist" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: Artist\n\n* Wiki\n* Albums")
      :unnarrowed t)
     ("t" "tool" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: Tool\n\n* Wiki\n* Doc\n\* Repo\n* Usage")
      :unnarrowed t)))

  :config
  (add-hook 'org-mode-hook `doom-modeline-set-delapouite-modeline)

  (setq org-roam-node-display-template "${title:*} @${my-level} | f${file:50} | ★${interest} | ↑${upgraded-at} | m${mtime} | ${tags:50}")
  (setq org-tags-exclude-from-inheritance '("Album" "Artist" "Debut" "Top"))

  (setq org-image-max-width 100)
  (setq org-startup-with-inline-images t)

  ; The gain in performance is quite significant, from 3 seconds to instant
  (setq org-roam-node-default-sort nil)
  (require 'memoize)
  (memoize 'org-roam-node-read--completions "10 minute")

  ; Sections in sidebar

  (defun my/org-roam-links-section (node)
    "The links section with counters for NODE."
    (magit-insert-section (org-roam-links)
      (magit-insert-heading "Links")
      (let ((hits (org-roam-db-query [:select [(funcall count pos) type]
                                      :from [links]
                                      :where (= source $s1)
                                      :group-by type]
                                     (org-roam-node-id node))))
        (dolist (hit hits) (insert (concat (number-to-string (car hit)) " - " (cadr hit) "\n"))))
      (insert ?\n)))

  (setq org-roam-mode-sections
        '(my/org-roam-links-section
          org-roam-backlinks-section
          org-roam-reflinks-section))

  (defface org-link-id
    '((t :foreground "#50fa7b"
         :weight bold
         :underline t))
    "Face for Org-Mode links starting with id:."
    :group 'org-faces)
  (defface org-link-file
    '((t :foreground "#ff5555"
         :weight bold
         :underline t))
    "Face for Org-Mode links starting with file:."
    :group 'org-faces)
  (org-link-set-parameters "id" :face 'org-link-id)
  (org-link-set-parameters "file" :face 'org-link-file)
  (org-link-set-parameters "env" :face 'org-formula)
  (org-link-set-parameters "flag" :face 'org-formula)
  (org-link-set-parameters "header" :face 'org-formula)

  ; override to store more stuffs in the properties column for https links

  (defun my/org-roam-db-insert-link (link)
    "Insert link data for LINK at current point into the Org-roam cache."
    (save-excursion
      (goto-char (org-element-property :begin link))
      (let* ((type (org-element-property :type link))
             (path (org-element-property :path link))
             (source (org-roam-id-at-point))
             (properties (list :outline (ignore-errors
                                          ;; This can error if link is not under any headline
                                          (org-get-outline-path 'with-self 'use-cache))
                               :drawer (if (string-equal type "https") (org-entry-properties) '()))))
        ;; For Org-ref links, we need to split the path into the cite keys
        (when (and source path)
          (if (and (boundp 'org-ref-cite-types)
                   (or (assoc type org-ref-cite-types)
                       (member type org-ref-cite-types)))
              (org-roam-db-query
               [:insert :into citations
                :values $v1]
               (mapcar (lambda (k) (vector source k (point) properties))
                       (org-roam-org-ref-path-to-keys path)))
            (org-roam-db-query
             [:insert :into links
              :values $v1]
             (vector (point) source path type properties)))))))

  '(advice-add 'org-roam-db-insert-link :override #'my/org-roam-db-insert-link)

  (defun my/org-roam-visit-node-at-point ()
    (interactive)
    (when-let (node (org-roam-node-from-title-or-alias (word-at-point t)))
      (org-roam-node-visit node)))

  (map! :n "M-<return>" #'my/org-roam-visit-node-at-point)

  (defun my/visited-at ()
    (when (eq 'headline (car (org-element-at-point)))
      (when (string= "https" (org-element-property :type (org-element-context)))
        (org-set-property "visited-at" (iso8601-format)))))

  '(add-hook 'org-follow-link-hook #'my/visited-at)

  ;; random predicate natively implemented in https://github.com/org-roam/org-roam/pull/2050

  (defun org-roam-node-random-person (&optional other-window)
    "Find and open a random Org-roam person node.
     With prefix argument OTHER-WINDOW, visit the node in another window instead."
    (interactive current-prefix-arg)
    (org-roam-node-random other-window
                          (lambda (node) (member "Person" (org-roam-node-tags node)))))

  (defun org-roam-node-random-tool (&optional other-window)
    "Find and open a random Org-roam tool node.
     With prefix argument OTHER-WINDOW, visit the node in another window instead."
    (interactive current-prefix-arg)
    (org-roam-node-random other-window
                          (lambda (node) (member "Tool" (org-roam-node-tags node)))))

  (defun org-roam-node-random-artist (&optional other-window)
    "Find and open a random Org-roam artist node.
     With prefix argument OTHER-WINDOW, visit the node in another window instead."
    (interactive current-prefix-arg)
    (org-roam-node-random other-window
                          (lambda (node) (member "Artist" (org-roam-node-tags node)))))

  (defun org-roam-node-random-album (&optional other-window)
    "Find and open a random Org-roam album node.
     With prefix argument OTHER-WINDOW, visit the node in another window instead."
    (interactive current-prefix-arg)
    (org-roam-node-random other-window
                          (lambda (node) (member "Album" (org-roam-node-tags node)))))

  (defun org-roam-node-random-2021 ()
    (interactive)
    (org-roam-node-random nil (lambda (node)
                                (string-prefix-p "2021" (org-roam-node-mtime node)))))

  (defun org-roam-node-random-2022 ()
    (interactive)
    (org-roam-node-random nil (lambda (node)
                                (string-prefix-p "2022" (org-roam-node-mtime node)))))

  (defun my/print-nodes-list (&rest search)
    "Print a org-roam nodes list having SEARCH tag(s)"
    ; (princ (concat "Generated at " (format-time-string "%Y-%m-%d %H:%M:%S\n" (current-time))))
    (let ((hits (apply #'org-roam-db-query (cons [:select [id title]
                                                  :from [nodes tags]
                                                  ; TODO fix 3 tags limit
                                                  :where (and (= node_id id) (in tag [$s2 $s3 $s4]))
                                                  :group-by title
                                                  :having (= (funcall count title) $s1)] (cons (length search) search)))))
      (when (> (length hits) 10) (princ (concat (number-to-string (length hits)) " items\n")))
      (dolist (hit hits) (princ (concat "[[id:" (car hit) "][" (cadr hit) "]]\n")))))

  (cl-defmethod org-roam-node-my-level ((node org-roam-node))
    (number-to-string (org-roam-node-level node)))

  (cl-defmethod org-roam-node-mtime ((node org-roam-node))
    (format-time-string "%Y-%m-%d" (org-roam-node-file-mtime node)))

  ;; not used because too slow :(
  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (cl-defmethod org-roam-node-upgraded-at ((node org-roam-node))
    (let* ((upgraded-at (or (cdr (assoc "UPGRADED-AT" (org-roam-node-properties node)))
                            "          ")))
      (substring upgraded-at 0 10)))

  (cl-defmethod org-roam-node-interest ((node org-roam-node))
    (or (cdr (assoc "INTEREST" (org-roam-node-properties node))) "  "))

  (advice-add 'org-insert-property-drawer :override #'my/org-insert-property-drawer)

  (defun my/org-capture-before-finalize ()
    (org-delete-property "ID")
    (setq org-property-format "%s %s")
    (org-set-property "id" (org-get-title))
    (my/created-at))

  (add-hook 'org-capture-before-finalize-hook 'my/org-capture-before-finalize))

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
