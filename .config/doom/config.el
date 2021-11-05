;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
 '(org-document-title :height 180))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/org/")
(setq org-roam-directory "~/Sync/org/roam")
(setq org-agenda-files '("~/Sync/org/roam/20201217195416-todo.org"))
(setq deft-directory "~/Sync/org/roam")
(setq deft-extensions '("txt" "tex" "org"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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

(map! :n "q" #'evil-backward-word-begin)
(map! :n "Q" #'evil-backward-WORD-begin)
(map! :n "U" #'evil-redo)
(map! :leader
      :desc "Find roam" "r" #'org-roam-node-find)

;; modeline
(defun get-buffer-file-mtime ()
   (let* ((fname (buffer-file-name))
          (mtime (file-attribute-modification-time
                      (file-attributes fname))))
     (when mtime
        (format-time-string " %Y-%m-%d %H:%M:%S" mtime))))

(use-package! doom-modeline
  :config
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
  :config
  (add-hook 'org-mode-hook `doom-modeline-set-delapouite-modeline)
  ;; only in v2
  (setq org-roam-node-display-template "${title:*} ${tags:50}")
  (setq org-roam-mode-section-functions
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              ;#'org-roam-unlinked-references-section
        ))
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
  (org-link-set-parameters
   "id"
   :face 'org-link-id)
  (org-link-set-parameters
   "file"
   :face 'org-link-file))

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

;; URL parsers and fetchers
;;
;; future candidates: Wikipedia

(require 'request)
(require 'json)

(defun my/get-current-line-content ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun my/parse-github-url ()
  (let ((line-content (my/get-current-line-content)))
    (string-match ".*?https://github.com/\\([a-zA-Z0-9-_\.]*\\)/\\([a-zA-Z0-9-_\.]*\\).*" line-content)
    (list (match-string 1 line-content)
          (match-string 2 line-content))))

(defun my/fetch-github-stats ()
  "Fetch GitHub REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (org name) (my/parse-github-url)
    (request
      (concat "https://api.github.com/repos/" org  "/" name)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-set-property "description" (assoc-default 'description data))
                  (org-set-property "stars" (number-to-string (assoc-default 'stargazers_count data)))
                  (org-set-property "open-issues" (number-to-string (assoc-default 'open_issues data)))
                  (org-set-property "language" (assoc-default 'language data))
                  (org-set-property "created-at" (assoc-default 'created_at data))
                  (org-set-property "updated-at" (assoc-default 'updated_at data))
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))))

(defun my/parse-stackoverflow-url ()
  (let ((line-content (my/get-current-line-content)))
    (string-match ".*?https://stackoverflow.com/questions/\\([0-9]*\\)/.*" line-content)
    (match-string 1 line-content)))


(defun my/fetch-stackoverflow-stats ()
  "Fetch StackOverflow REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (let ((stackoverflow-id (my/parse-stackoverflow-url)))
    (request
      (concat "https://api.stackexchange.com/2.3/questions/" stackoverflow-id "?site=stackoverflow")
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-set-property "title" (assoc-default 'title (aref (assoc-default 'items data) 0)))
                  (org-set-property "score" (number-to-string (assoc-default 'score (aref (assoc-default 'items data) 0))))
                  (org-set-property "views" (number-to-string (assoc-default 'view_count (aref (assoc-default 'items data) 0))))
                  (org-set-property "asked-at" (format-time-string "%Y-%m-%dT%TZ%z" (assoc-default 'creation_date (aref (assoc-default 'items data) 0))))
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))))

(defun my/parse-youtube-url ()
  (let ((line-content (my/get-current-line-content)))
    (string-match ".*?https://www.youtube.com/watch\\?v=\\([a-zA-Z0-9-_]*\\).*" line-content)
    (match-string 1 line-content)))

(defun my/fetch-youtube-stats ()
  "Fetch Youtube REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (let ((youtube-id (my/parse-youtube-url)))
    (request
      (concat "https://youtube.googleapis.com/youtube/v3/videos?part=snippet%2CcontentDetails%2Cstatistics&id=" youtube-id "&key=" youtube-api-key)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-set-property "channel" (assoc-default 'channelTitle (assoc-default 'snippet (aref (assoc-default 'items data) 0))))
                  (org-set-property "title" (assoc-default 'title (assoc-default 'snippet (aref (assoc-default 'items data) 0))))
                  (org-set-property "views" (assoc-default 'viewCount (assoc-default 'statistics (aref (assoc-default 'items data) 0))))
                  (org-set-property "duration" (assoc-default 'duration (assoc-default 'contentDetails (aref (assoc-default 'items data) 0))))
                  (org-set-property "published-at" (assoc-default 'publishedAt (assoc-default 'snippet (aref (assoc-default 'items data) 0))))
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))))

(defun my/parse-npm-url ()
  (let ((line-content (my/get-current-line-content)))
    (string-match ".*?https://www.npmjs.com/package/\\([a-zA-Z0-9-_]*\\).*" line-content)
    (match-string 1 line-content)))

(defun my/fetch-npm-stats ()
  "Fetch Npm REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (let ((npm-id (my/parse-npm-url)))
    (request
      (concat "https://api.npmjs.org/downloads/point/last-month/" npm-id)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-set-property "downloads" (number-to-string (assoc-default 'downloads data)))
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))
    (request
      (concat "https://registry.npmjs.com/" npm-id)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  ; TODO add dependencies
                  (org-set-property "last-version" (assoc-default 'latest (assoc-default 'dist-tags data)))
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))))

(defun my/parse-musicbrainz-url ()
  (let ((line-content (my/get-current-line-content)))
    (string-match ".*?https://musicbrainz.org/artist/\\([a-zA-Z0-9-_]*\\).*" line-content)
    (match-string 1 line-content)))

(defun my/fetch-musicbrainz-stats ()
  "Fetch MusicBrainz REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (let ((artist-id (my/parse-musicbrainz-url)))
    (request
      (concat "https://musicbrainz.org/ws/2/artist/" artist-id "?fmt=json&inc=url-rels")
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  ; append is needed to cast the vector into a list
                  (let ((bandcamp (--find (string-equal (assoc-default 'type it) "bandcamp") (append (assoc-default 'relations data) nil))))
                    (if bandcamp (org-set-property "bandcamp" (assoc-default 'resource (assoc-default 'url bandcamp)))))
                  (let ((songkick (--find (string-equal (assoc-default 'type it) "songkick") (append (assoc-default 'relations data) nil))))
                    (if songkick (org-set-property "songkick" (assoc-default 'resource (assoc-default 'url songkick)))))
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))))

