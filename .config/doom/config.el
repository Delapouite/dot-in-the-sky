;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "private.el")
(load! "fetcher.el")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Delapouite"
      user-mail-address "delapouite@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "iosevka" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)
(custom-set-faces!
  '(bold :weight bold :foreground "#8be9fd")
  '(org-roam-header-line :weight bold :height 180 :foreground "#f1fa8c")
  '(magit-section-heading :height 120 :foreground "#8be9fd")
  '(org-quote :slant italic :foreground "#f8f8f2")
  '(org-document-title :height 180)
  '(org-roam-title :weight bold :height 120)
  '(org-roam-olp :foreground "#6272a4")
  '(outline-1 :height 120)
  ;; TODO why the need to reset?
  '(outline-2 :height 90))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/org/")
;; The following settings just make Emacs hang :(
;; See the vulpea functions at the end for a dynamic list
; (setq org-agenda-files '("~/Sync/org/roam/"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq scroll-margin 10)

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :n "q" #'evil-backward-word-begin
      :n "Q" #'evil-backward-WORD-begin
      :n "U" #'evil-redo
      :n "b n" #'next-buffer
      :n "b p" #'previous-buffer
      :n "b s" #'save-buffer)
(map! :leader
      :desc "Find roam" "r" #'org-roam-node-find)

(use-package! doom-modeline
  :config
  (defun get-buffer-file-mtime ()
    (let* ((fname (buffer-file-name))
           (mtime (file-attribute-modification-time
                   (file-attributes fname))))
      (when mtime
        (format-time-string " %Y-%m-%d %H:%M:%S" mtime))))

  (doom-modeline-def-segment buffer-mtime
    "Define buffer-mtime modeline segment"
    (propertize (get-buffer-file-mtime) 'face 'mode-line))

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
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "album" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Album\n\n* Wiki\n* Tracks")
      :unnarrowed t)
     ("p" "person" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Person\n")
      :unnarrowed t)
     ("r" "artist" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Artist\n\n* Wiki\n* Albums")
      :unnarrowed t)
     ("t" "tool" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Tool\n\n* Wiki\n* Doc\n\* Repo\n* Usage")
      :unnarrowed t)))

  :config
  (add-hook 'org-mode-hook `doom-modeline-set-delapouite-modeline)
  (setq org-roam-node-display-template "${title:*} ${my-level} | ${mtime} | ${tags:50}")
  (setq org-tags-exclude-from-inheritance '("Album" "Artist" "Debut" "Top"))

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

  (setq org-roam-mode-section-functions
        (list #'my/org-roam-links-section
              #'org-roam-backlinks-section
              #'org-roam-reflinks-section))

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

  (defun my/org-roam-visit-node-at-point ()
    (interactive)
    (when-let (node (org-roam-node-from-title-or-alias (word-at-point t)))
      (org-roam-node-visit node)))

  ;; random predicate natively implemented in https://github.com/org-roam/org-roam/pull/2050

  (defun org-roam-node-random-tag (search &optional other-window)
    (let ((random-row (seq-random-elt (org-roam-db-query [:select [id file pos]
                                                          :from [nodes tags]
                                                          :where (and (= node_id id) (= tag $s1))] search))))
      (org-roam-node-visit (org-roam-node-create :id (nth 0 random-row)
                                                 :file (nth 1 random-row)
                                                 :point (nth 2 random-row))
                           other-window)))

  (defun org-roam-node-random-tool (&optional other-window)
    "Find and open a random Org-roam tool node.
        With prefix argument OTHER-WINDOW, visit the node in another window instead."
    (interactive current-prefix-arg)
    (org-roam-node-random-tag "Tool" other-window))

  (defun org-roam-node-random-artist (&optional other-window)
    "Find and open a random Org-roam artist node.
        With prefix argument OTHER-WINDOW, visit the node in another window instead."
    (interactive current-prefix-arg)
    (org-roam-node-random-tag "Artist" other-window))

  (defun org-roam-node-random-album (&optional other-window)
    "Find and open a random Org-roam album node.
        With prefix argument OTHER-WINDOW, visit the node in another window instead."
    (interactive current-prefix-arg)
    (org-roam-node-random-tag "Album" other-window))

  (defun my/print-nodes-list (&rest search)
    "Print a org-roam nodes list having SEARCH tag(s)"
    ; (princ (concat "Generated at " (format-time-string "%Y-%m-%d %H:%M:%S\n" (current-time))))
    (let ((hits (apply #'org-roam-db-query (cons [:select [id title]
                                                  :from [nodes tags]
                                                  ; TODO fix 3 tags limit
                                                  :where (and (= node_id id) (in tag [$s2 $s3 $s4]))
                                                  :group-by title
                                                  :having (= (funcall count title) $s1)] (cons (length search) search)))))
      (dolist (hit hits) (princ (concat "[[id:" (car hit) "][" (cadr hit) "]]\n")))))
  (cl-defmethod org-roam-node-my-level ((node org-roam-node))
    (number-to-string (org-roam-node-level node)))
  (cl-defmethod org-roam-node-mtime ((node org-roam-node))
    (format-time-string "%Y-%m-%d %H:%M:%S" (org-roam-node-file-mtime node)))
  ;; not used because too slow :(
  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count))))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(setq company-selection-wrap-around t)


