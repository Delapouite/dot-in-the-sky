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
  '(org-document-title :height 180)
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
  :custom
  (org-roam-directory "~/Sync/org/roam")
  (org-roam-mode-section-functions
   (list #'org-roam-backlinks-section
         #'org-roam-reflinks-section
        ;#'org-roam-unlinked-references-section
         ))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("a" "artist" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Artist\n\n* Wiki\n* Albums")
      :unnarrowed t)
     ("p" "person" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Person\n")
      :unnarrowed t)
     ("t" "tool" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Tool\n\n* Wiki\n* Doc\n\* Repo\n* Usage")
      :unnarrowed t)))
  :config
  (add-hook 'org-mode-hook `doom-modeline-set-delapouite-modeline)
  (setq org-roam-node-display-template "${title:*} ${my-level} | ${mtime} | ${tags:50}")
  (setq org-tags-exclude-from-inheritance '("Album" "Artist"))
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

;; URL parsers and fetchers
;;
;; future candidates: Wikipedia

(require 'request)
(require 'json)

(defun my/get-current-line-content ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun my/parse-url (re)
  (let ((line-content (my/get-current-line-content)))
    (string-match re line-content)
    (list (match-string 1 line-content)
          (match-string 2 line-content)
          (match-string 3 line-content))))

(defvar my/github-re ".*?https://github.com/\\([a-zA-Z0-9-_\.]*\\)/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-github-stats ()
  "Fetch GitHub REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (org name) (my/parse-url my/github-re)
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
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))
    (request
      (concat "https://api.github.com/repos/" org  "/" name "/commits")
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-set-property "last-commit-at" (assoc-default 'date (assoc-default 'author (assoc-default 'commit (aref data 0)))))
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))))

(cl-defun my/parse-stack-response (&key data &allow-other-keys)
  (progn
    (org-set-property "title" (assoc-default 'title (aref (assoc-default 'items data) 0)))
    (org-set-property "score" (number-to-string (assoc-default 'score (aref (assoc-default 'items data) 0))))
    (org-set-property "views" (number-to-string (assoc-default 'view_count (aref (assoc-default 'items data) 0))))
    (org-set-property "asked-at" (format-time-string "%Y-%m-%dT%TZ%z" (assoc-default 'creation_date (aref (assoc-default 'items data) 0))))
    (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z"))))

(defvar my/stackoverflow-re ".*?https://stackoverflow.com/questions/\\([0-9]*\\)/.*")

(defun my/fetch-stackoverflow-stats ()
  "Fetch StackOverflow REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (question-id) (my/parse-url my/stackoverflow-re)
    (request
      (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=stackoverflow")
      :parser 'json-read
      :success 'my/parse-stack-response)))

(defvar my/serverfault-re ".*?https://serverfault.com/questions/\\([0-9]*\\)/.*")

(defun my/fetch-serverfault-stats ()
  "Fetch ServerFault REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (question-id) (my/parse-url my/serverfault-re)
    (request
      (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=serverfault")
      :parser 'json-read
      :success 'my/parse-stack-response)))

(defvar my/superuser-re ".*?https://superuser.com/questions/\\([0-9]*\\)/.*")

(defun my/fetch-superuser-stats ()
  "Fetch SuperUser REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (question-id) (my/parse-url my/superuser-re)
    (request
      (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=superuser")
      :parser 'json-read
      :success 'my/parse-stack-response)))

(defvar my/askubuntu-re ".*?https://askubuntu.com/questions/\\([0-9]*\\)/.*")

(defun my/fetch-askubuntu-stats ()
  "Fetch AskUbuntu REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (question-id) (my/parse-url my/askubuntu-re)
    (request
      (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=askubuntu")
      :parser 'json-read
      :success 'my/parse-stack-response)))

(defvar my/stackexchange-re ".*?https://\\(.*?\\).stackexchange.com/questions/\\([0-9]*\\)/.*")

(defun my/fetch-stackexchange-stats ()
  "Fetch StackExchange REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (site question-id) (my/parse-url my/stackexchange-re)
    (request
      (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=" site)
      :parser 'json-read
      :success 'my/parse-stack-response)))

(defvar my/youtube-re ".*?https://www.youtube.com/watch\\?v=\\([a-zA-Z0-9-_]*\\).*")

(defun my/fetch-youtube-stats ()
  "Fetch Youtube REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (youtube-id) (my/parse-url my/youtube-re)
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

(defvar my/npm-re ".*?https://www.npmjs.com/package/\\([a-zA-Z0-9-_]*\\).*")

(defun my/fetch-npm-stats ()
  "Fetch Npm REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (npm-id) (my/parse-url my/npm-re)
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

(defvar my/musicbrainz-re ".*?https://musicbrainz.org/artist/\\([a-zA-Z0-9-_]*\\).*")

(defun my/fetch-musicbrainz-stats ()
  "Fetch MusicBrainz REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (artist-id) (my/parse-url my/musicbrainz-re)
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
                  (let ((soundcloud (--find (string-equal (assoc-default 'type it) "soundcloud") (append (assoc-default 'relations data) nil))))
                    (if soundcloud (org-set-property "soundcloud" (assoc-default 'resource (assoc-default 'url soundcloud)))))
                  (let ((youtube (--find (string-equal (assoc-default 'type it) "youtube") (append (assoc-default 'relations data) nil))))
                    (if youtube (org-set-property "youtube" (assoc-default 'resource (assoc-default 'url youtube)))))
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))))

(defvar my/archlinux-re ".*?https://archlinux.org/packages/\\(community\\|core\\|extra\\)/\\(any\\|x86_64\\)/\\([a-zA-Z0-9-_]*\\)/.*")

(defun my/fetch-archlinux-stats ()
  "Fetch Archlinux REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (repo arch package-id) (my/parse-url my/archlinux-re)
    (request
      (concat "https://www.archlinux.org/packages/" repo "/" arch "/" package-id "/json")
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-set-property "name" (assoc-default 'pkgname data))
                  (org-set-property "description" (assoc-default 'pkgdesc data))
                  (org-set-property "version" (assoc-default 'pkgver data))
                  (org-set-property "built-at" (assoc-default 'build_date data))
                  (org-set-property "updated-at" (assoc-default 'last_update data))
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))))

(defvar my/aur-re ".*?https://aur.archlinux.org/packages/\\([a-zA-Z0-9-_]*\\)/.*")

(defun my/fetch-aur-stats ()
  "Fetch AUR REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (package-id) (my/parse-url my/aur-re)
    (request
      (concat "https://aur.archlinux.org/rpc/?v=5&type=info&arg=" package-id)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-set-property "name" (assoc-default 'Name (aref (assoc-default 'results data) 0)))
                  (org-set-property "description" (assoc-default 'Description (aref (assoc-default 'results data) 0)))
                  (org-set-property "version" (assoc-default 'Version (aref (assoc-default 'results data) 0)))
                  (org-set-property "updated-at" (format-time-string "%Y-%m-%dT%TZ%z" (assoc-default 'LastModified (aref (assoc-default 'results data) 0))))
                  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z")))))))

(defun my/fetch-stats ()
  "Fetch current website REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (let ((line-content (my/get-current-line-content)))
    (cond
      ((string-match-p my/archlinux-re line-content) (my/fetch-archlinux-stats))
      ((string-match-p my/askubuntu-re line-content) (my/fetch-askubuntu-stats))
      ((string-match-p my/aur-re line-content) (my/fetch-aur-stats))
      ((string-match-p my/github-re line-content) (my/fetch-github-stats))
      ((string-match-p my/musicbrainz-re line-content) (my/fetch-musicbrainz-stats))
      ((string-match-p my/npm-re line-content) (my/fetch-npm-stats))
      ((string-match-p my/serverfault-re line-content) (my/fetch-serverfault-stats))
      ((string-match-p my/superuser-re line-content) (my/fetch-superuser-stats))
      ((string-match-p my/stackexchange-re line-content) (my/fetch-stackexchange-stats))
      ((string-match-p my/stackoverflow-re line-content) (my/fetch-stackoverflow-stats))
      ((string-match-p my/youtube-re line-content) (my/fetch-youtube-stats)))))

