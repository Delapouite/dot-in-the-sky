;;; fetcher.el -*- lexical-binding: t; -*-
;; need credentials from private.el

(require 'request)
(require 'json)
(require 'xml)

(defun my/fetched-at ()
  (org-set-property "fetched-at" (format-time-string "%Y-%m-%dT%TZ%z"))
  (setq org-property-format "%-10s %s"))

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
                  (setq org-property-format "%-16s %s")
                  (org-set-property "description" (or (assoc-default 'description data) "null"))
                  (org-set-property "stars" (number-to-string (assoc-default 'stargazers_count data)))
                  (org-set-property "open-issues" (number-to-string (assoc-default 'open_issues data)))
                  (org-set-property "language" (or (assoc-default 'language data) "null"))
                  (org-set-property "created-at" (assoc-default 'created_at data))
                  (org-set-property "updated-at" (assoc-default 'updated_at data))
                  (request
                    (concat "https://api.github.com/repos/" org  "/" name "/commits")
                    :parser 'json-read
                    :success (cl-function
                              (lambda (&key data &allow-other-keys)
                                (org-set-property "last-commit-at" (assoc-default 'date (assoc-default 'author (assoc-default 'commit (aref data 0)))))
                                (my/fetched-at)))))))))

(defvar my/github-issues-re ".*?https://github.com/\\([a-zA-Z0-9-_\.]*\\)/\\([a-zA-Z0-9-_\.]*\\)/issues/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-github-issues-stats ()
  "Fetch GitHub REST API for issues and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (org name issue-id) (my/parse-url my/github-issues-re)
    (request
      (concat "https://api.github.com/repos/" org  "/" name "/issues/" issue-id)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq org-property-format "%-12s %s")
                  (org-set-property "title" (assoc-default 'title data))
                  (org-set-property "state" (assoc-default 'state data))
                  (org-set-property "comments" (number-to-string (assoc-default 'comments data)))
                  (org-set-property "created-at" (assoc-default 'created_at data))
                  (org-set-property "updated-at" (assoc-default 'updated_at data))
                  (org-set-property "closed-at" (or (assoc-default 'closed_at data) "null"))
                  (my/fetched-at))))))

(defvar my/github-pull-re ".*?https://github.com/\\([a-zA-Z0-9-_\.]*\\)/\\([a-zA-Z0-9-_\.]*\\)/pull/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-github-pull-stats ()
  "Fetch GitHub REST API for pull-requests and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (org name pull-id) (my/parse-url my/github-pull-re)
    (request
      (concat "https://api.github.com/repos/" org  "/" name "/pulls/" pull-id)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq org-property-format "%-12s %s")
                  (org-set-property "title" (assoc-default 'title data))
                  (org-set-property "state" (assoc-default 'state data))
                  (org-set-property "comments" (number-to-string (assoc-default 'comments data)))
                  (org-set-property "created-at" (assoc-default 'created_at data))
                  (org-set-property "updated-at" (assoc-default 'updated_at data))
                  (org-set-property "closed-at" (or (assoc-default 'closed_at data) "null"))
                  (org-set-property "merged-at" (or (assoc-default 'merged_at data) "null"))
                  (my/fetched-at))))))

(defvar my/gitlab-re ".*?https://gitlab.com/\\([a-zA-Z0-9-_\.]*\\)/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-gitlab-stats ()
  "Fetch GitLab REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (org name) (my/parse-url my/gitlab-re)
    (request
      (concat "https://gitlab.com/api/v4/projects/" org  "%2F" name)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq org-property-format "%-13s %s")
                  (org-set-property "description" (assoc-default 'description data))
                  (org-set-property "stars" (number-to-string (assoc-default 'star_count data)))
                  (org-set-property "created-at" (assoc-default 'created_at data))
                  (org-set-property "updated-at" (assoc-default 'last_activity_at data))
                  (my/fetched-at))))))

(cl-defun my/parse-stack-response (&key data &allow-other-keys)
  (progn
    (setq org-property-format "%-12s %s")
    (org-set-property "title" (xml-substitute-special (assoc-default 'title (aref (assoc-default 'items data) 0))))
    (org-set-property "score" (number-to-string (assoc-default 'score (aref (assoc-default 'items data) 0))))
    (org-set-property "views" (number-to-string (assoc-default 'view_count (aref (assoc-default 'items data) 0))))
    (org-set-property "asked-at" (format-time-string "%Y-%m-%dT%TZ%z" (assoc-default 'creation_date (aref (assoc-default 'items data) 0))))
    (my/fetched-at)))

(cl-defun my/parse-stack-tags-response (&key data &allow-other-keys)
  (progn
    (setq org-property-format "%-12s %s")
    (org-set-property "count" (number-to-string (assoc-default 'count (aref (assoc-default 'items data) 0))))
    (my/fetched-at)))

(defvar my/stackoverflow-re ".*?https://stackoverflow.com/questions/\\([0-9]*\\)/.*")
(defvar my/stackoverflow-tags-re ".*?https://stackoverflow.com/questions/tagged/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-stackoverflow-stats ()
  "Fetch StackOverflow REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (question-id) (my/parse-url my/stackoverflow-re)
    (request
      (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=stackoverflow")
      :parser 'json-read
      :success 'my/parse-stack-response)))

(defun my/fetch-stackoverflow-tags-stats ()
  "Fetch StackOverflow REST API for tags and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (tag) (my/parse-url my/stackoverflow-tags-re)
    (request
      (concat "https://api.stackexchange.com/2.3/tags/" tag "/info?site=stackoverflow")
      :parser 'json-read
      :success 'my/parse-stack-tags-response)))

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
(defvar my/stackexchange-tags-re ".*?https://\\(.*?\\).stackexchange.com/questions/tagged/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-stackexchange-stats ()
  "Fetch StackExchange REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (site question-id) (my/parse-url my/stackexchange-re)
    (request
      (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=" site)
      :parser 'json-read
      :success 'my/parse-stack-response)))

(defun my/fetch-stackexchange-tags-stats ()
  "Fetch StackExchange REST API for tag and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (site tag) (my/parse-url my/stackexchange-tags-re)
    (request
      (concat "https://api.stackexchange.com/2.3/tags/" tag "/info?site=" site)
      :parser 'json-read
      :success 'my/parse-stack-tags-response)))

(defvar my/youtube-re ".*?https://www.youtube.com/watch\\?v=\\([a-zA-Z0-9-_]*\\).*")

(defun my/fetch-youtube-stats ()
  "Fetch Youtube REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (youtube-id) (my/parse-url my/youtube-re)
    (request
      (concat "https://youtube.googleapis.com/youtube/v3/videos?part=snippet%2CcontentDetails%2Cstatistics&id=" youtube-id "&key=" my/youtube-api-key)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq org-property-format "%-14s %s")
                  (org-set-property "channel" (assoc-default 'channelTitle (assoc-default 'snippet (aref (assoc-default 'items data) 0))))
                  (org-set-property "title" (assoc-default 'title (assoc-default 'snippet (aref (assoc-default 'items data) 0))))
                  (org-set-property "views" (assoc-default 'viewCount (assoc-default 'statistics (aref (assoc-default 'items data) 0))))
                  (org-set-property "duration" (assoc-default 'duration (assoc-default 'contentDetails (aref (assoc-default 'items data) 0))))
                  (org-set-property "published-at" (assoc-default 'publishedAt (assoc-default 'snippet (aref (assoc-default 'items data) 0))))
                  (my/fetched-at))))))

(defvar my/npm-re ".*?https://www.npmjs.com/package/\\([a-zA-Z0-9-_@/]*\\).*")

(defun my/fetch-npm-stats ()
  "Fetch Npm REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (npm-id) (my/parse-url my/npm-re)
    (request
      (concat "https://registry.npmjs.com/" npm-id)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((last-version (assoc-default 'latest (assoc-default 'dist-tags data)))
                         (version (assoc-default (intern last-version) (assoc-default 'versions data)))
                         (dependencies (length (assoc-default 'dependencies version)))
                         (types (assoc-default 'types version)))
                    (setq org-property-format "%-14s %s")
                    (org-set-property "dependencies" (number-to-string dependencies))
                    (org-set-property "last-version" last-version)
                    (org-set-property "types" (or types "null"))
                    (org-set-property "created-at" (assoc-default 'created (assoc-default 'time data)))
                    (org-set-property "updated-at" (assoc-default 'modified (assoc-default 'time data)))
                    (request
                      (concat "https://api.npmjs.org/downloads/point/last-month/" npm-id)
                      :parser 'json-read
                      :success (cl-function
                                (lambda (&key data &allow-other-keys)
                                  (org-set-property "downloads" (number-to-string (assoc-default 'downloads data)))
                                  (my/fetched-at))))))))))

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
                  (setq org-property-format "%-12s %s")
                  ; append is needed to cast the vector into a list
                  (let ((bandcamp (--find (string-equal (assoc-default 'type it) "bandcamp") (append (assoc-default 'relations data) nil))))
                    (if bandcamp (org-set-property "bandcamp" (assoc-default 'resource (assoc-default 'url bandcamp)))))
                  (let ((discogs (--find (string-equal (assoc-default 'type it) "discogs") (append (assoc-default 'relations data) nil))))
                    (if discogs (org-set-property "discogs" (assoc-default 'resource (assoc-default 'url discogs)))))
                  (let ((lastfm (--find (string-equal (assoc-default 'type it) "last.fm") (append (assoc-default 'relations data) nil))))
                    (if lastfm (org-set-property "lastfm" (assoc-default 'resource (assoc-default 'url lastfm)))))
                  (let ((songkick (--find (string-equal (assoc-default 'type it) "songkick") (append (assoc-default 'relations data) nil))))
                    (if songkick (org-set-property "songkick" (assoc-default 'resource (assoc-default 'url songkick)))))
                  (let ((soundcloud (--find (string-equal (assoc-default 'type it) "soundcloud") (append (assoc-default 'relations data) nil))))
                    (if soundcloud (org-set-property "soundcloud" (assoc-default 'resource (assoc-default 'url soundcloud)))))
                  (let ((youtube (--find (string-equal (assoc-default 'type it) "youtube") (append (assoc-default 'relations data) nil))))
                    (if youtube (org-set-property "youtube" (assoc-default 'resource (assoc-default 'url youtube)))))
                  (my/fetched-at))))))

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
                  (setq org-property-format "%-17s %s")
                  (org-set-property "name" (assoc-default 'pkgname data))
                  (org-set-property "description" (assoc-default 'pkgdesc data))
                  (org-set-property "remote-version" (assoc-default 'pkgver data))
                  (org-set-property "system-version" (string-trim (shell-command-to-string (concat "pacman -Q " package-id " 2> /dev/null | awk '{ print $2 }'"))))
                  (org-set-property "dependencies" (number-to-string (length (assoc-default 'depends data))))
                  (org-set-property "compressed-size" (number-to-string (assoc-default 'compressed_size data)))
                  (org-set-property "installed-size" (number-to-string (assoc-default 'installed_size data)))
                  (org-set-property "built-at" (assoc-default 'build_date data))
                  (org-set-property "updated-at" (assoc-default 'last_update data))
                  (my/fetched-at))))))

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
                  (setq org-property-format "%-13s %s")
                  (org-set-property "name" (assoc-default 'Name (aref (assoc-default 'results data) 0)))
                  (org-set-property "description" (assoc-default 'Description (aref (assoc-default 'results data) 0)))
                  (org-set-property "version" (assoc-default 'Version (aref (assoc-default 'results data) 0)))
                  (org-set-property "updated-at" (format-time-string "%Y-%m-%dT%TZ%z" (assoc-default 'LastModified (aref (assoc-default 'results data) 0))))
                  (my/fetched-at))))))

(defvar my/docker-hub-re "*.?https://hub.docker.com/r/\\([a-zA-Z0-9-_]*\\)/\\([a-zA-Z0-9-_]*\\).*")
(defvar my/docker-hub-official-re "*.?https://hub.docker.com/_/\\([a-zA-Z0-9-_]*\\).*")

(defun my/fetch-docker-hub ()
  "Try to fetch a pseudo Dockerfile"
  (interactive)
  (seq-let (orga image) (my/parse-url my/docker-hub-re)
    (let ((dockerfile (shell-command-to-string (concat "skopeo inspect --config docker://docker.io/" orga "/" image " | jq -r '.history | map(.created_by) | join(\"\n\")'"))))
      (end-of-line)
      (insert (concat "\n#+begin_src dockerfile\n" dockerfile "#+end_src")))))

(defun my/fetch-docker-official-hub ()
  "Try to fetch an official pseudo Dockerfile"
  (interactive)
  (seq-let (image) (my/parse-url my/docker-hub-official-re)
    (let ((dockerfile (shell-command-to-string (concat "skopeo inspect --config docker://docker.io/" image " | jq -r '.history | map(.created_by) | join(\"\n\")'"))))
      (end-of-line)
      (insert (concat "\n#+begin_src dockerfile\n" dockerfile "#+end_src")))))

(defvar my/bundlephobia-re "*.?https://bundlephobia.com/package/\\([a-zA-Z0-9-_@.]*\\).*")

(defun my/fetch-bundlephobia-stats ()
  "Fetch Bundle Phobia REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (package) (my/parse-url my/bundlephobia-re)
    (request
      (concat "https://bundlephobia.com/api/size?record=true&package=" package)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq org-property-format "%-12s %s")
                  (org-set-property "size" (number-to-string (assoc-default 'size data)))
                  (org-set-property "gzip" (number-to-string (assoc-default 'gzip data)))
                  (my/fetched-at))))))

(defvar my/vscode-re "*.?https://marketplace.visualstudio.com/items\\?itemName=\\([a-zA-Z0-9-_@.]*\\).*")

(defun my/fetch-vscode ()
  "Fetch VSCode Marketplace REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (extension-name) (my/parse-url my/vscode-re)
    (request
      "https://marketplace.visualstudio.com/_apis/public/gallery/extensionquery"
      :headers '(("accept" . "application/json;api-version=3.0-preview.1") ("content-type" . "application/json"))
      :data (concat "{\"filters\":[{\"criteria\":[{\"filterType\":7,\"value\":\"" extension-name "\"},{\"filterType\":8,\"value\":\"Microsoft.VisualStudio.Code\"}],\"pageNumber\":1,\"pageSize\":1,\"sortBy\":0,\"sortOrder\":0}],\"assetTypes\":[],\"flags\":950}")
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let* ((extension (aref (assoc-default 'extensions (aref (assoc-default 'results data) 0)) 0))
                         (last-version (aref (assoc-default 'versions extension) 0))
                         (manifest (assoc-default 'source (aref (assoc-default 'files last-version) 0))))
                    (setq org-property-format "%-18s %s")
                    (org-set-property "name" (assoc-default 'displayName extension))
                    (org-set-property "description" (assoc-default 'shortDescription extension))
                    (org-set-property "version" (assoc-default 'version last-version))
                    (org-set-property "manifest" manifest)
                    (org-set-property "updated-at" (assoc-default 'lastUpdated extension))
                    (request manifest
                      :parser 'json-read
                      :success (cl-function
                                (lambda (&key data &allow-other-keys)
                                  (let* ((contributes (assoc-default 'contributes data)))
                                    (org-set-property "commands" (number-to-string(length (assoc-default 'commands contributes))))
                                    (org-set-property "configuration" (number-to-string(length (assoc-default 'configuration contributes))))
                                    (org-set-property "grammars" (number-to-string(length (assoc-default 'grammars contributes))))
                                    (org-set-property "keybindings" (number-to-string(length (assoc-default 'keybindings contributes))))
                                    (org-set-property "languages" (number-to-string(length (assoc-default 'language contributes))))
                                    (org-set-property "menus" (number-to-string(length (assoc-default 'menus contributes))))
                                    (org-set-property "json-validation" (number-to-string(length (assoc-default 'jsonValidation contributes))))
                                    (org-set-property "views" (number-to-string(length (assoc-default 'views contributes))))
                                    (org-set-property "views-containers" (number-to-string(length (assoc-default 'viewsContainers contributes))))
                                    (my/fetched-at)))))))))))

(defvar my/wikipedia-re "*.?https://en.wikipedia.org/wiki/\\([a-zA-Z0-9-_@.]*\\).*")

(defun my/fetch-wikipedia-stats ()
  "Fetch Wikipedia REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (page) (my/parse-url my/wikipedia-re)
    (request
      (concat "https://en.wikipedia.org/w/rest.php/v1/page/" page "/history")
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq org-property-format "%-12s %s")
                  (org-set-property "updated-at" (assoc-default 'timestamp (aref (assoc-default 'revisions data) 0)))
                  (my/fetched-at))))))

(defun my/fetch-stats ()
  "Fetch current website REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (let ((line-content (my/get-current-line-content)))
    (cond
      ((string-match-p my/archlinux-re line-content) (my/fetch-archlinux-stats))
      ((string-match-p my/askubuntu-re line-content) (my/fetch-askubuntu-stats))
      ((string-match-p my/aur-re line-content) (my/fetch-aur-stats))
      ((string-match-p my/bundlephobia-re line-content) (my/fetch-bundlephobia-stats))
      ((string-match-p my/docker-hub-re line-content) (my/fetch-docker-hub))
      ((string-match-p my/docker-hub-official-re line-content) (my/fetch-docker-official-hub))
      ((string-match-p my/github-issues-re line-content) (my/fetch-github-issues-stats))
      ((string-match-p my/github-pull-re line-content) (my/fetch-github-pull-stats))
      ((string-match-p my/github-re line-content) (my/fetch-github-stats))
      ((string-match-p my/gitlab-re line-content) (my/fetch-gitlab-stats))
      ((string-match-p my/musicbrainz-re line-content) (my/fetch-musicbrainz-stats))
      ((string-match-p my/npm-re line-content) (my/fetch-npm-stats))
      ((string-match-p my/serverfault-re line-content) (my/fetch-serverfault-stats))
      ((string-match-p my/superuser-re line-content) (my/fetch-superuser-stats))
      ((string-match-p my/stackexchange-tags-re line-content) (my/fetch-stackexchange-tags-stats))
      ((string-match-p my/stackexchange-re line-content) (my/fetch-stackexchange-stats))
      ((string-match-p my/stackoverflow-tags-re line-content) (my/fetch-stackoverflow-tags-stats))
      ((string-match-p my/stackoverflow-re line-content) (my/fetch-stackoverflow-stats))
      ((string-match-p my/vscode-re line-content) (my/fetch-vscode))
      ((string-match-p my/wikipedia-re line-content) (my/fetch-wikipedia-stats))
      ((string-match-p my/youtube-re line-content) (my/fetch-youtube-stats)))))
