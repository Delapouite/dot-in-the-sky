;;; fetcher.el -*- lexical-binding: t; -*-
;; need credentials from private.el

(require 'request)
(require 'json)
(require 'xml)

(defun round-2 (x)
  (/ (round (* 100 x)) 100.0))

(defun my/fetch (url success)
  '(message (concat "fetching " url))
  (request url :parser 'json-read :success success :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                   (message "Got error: %s %S" url error-thrown)))))

(defun my/get-current-line-content ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun my/parse-url (re)
  (let ((line-content (my/get-current-line-content)))
    (string-match re line-content)
    (list (match-string 1 line-content)
          (match-string 2 line-content)
          (match-string 3 line-content))))

(defun my/visit-url (&optional initial-read)
  (interactive)
  (let* ((links (org-roam-db-query [:select [dest source properties] :from [links] :where (= type "https")]))
         (links (mapcar (lambda (link)
                          (let* ((url (cl-first link))
                                 (source (cl-second link))
                                 (outline (cl-second (cl-third link)))
                                 (outline (cl-remove-if (lambda (o) (string-equal o (concat "https:" url))) outline))
                                 (title (string-join outline " > "))
                                 ; drawer is non Org-Roam native
                                 (drawer (cl-fourth (cl-third link)))
                                 (stars (cdr (assoc "STARS" drawer)))
                                 (stars (if stars (concat stars "⭐") ""))
                                 (fetched-at (cdr (assoc "FETCHED-AT" drawer))))
                            (put-text-property 0 (length source) 'face 'bold source)
                            (put-text-property 0 (length title) 'face 'org-macro title)
                            (put-text-property 0 (length stars) 'face 'org-code stars)
                            (put-text-property 0 (length fetched-at) 'face 'font-lock-comment-face fetched-at)
                            (cons (concat url " (" source ") " title " " fetched-at " " stars) link))) links))
         (link (completing-read "Links: " links nil nil initial-read)))
    (browse-url (concat "https:" (cadr (assoc link links))))))

(defun my/visit-ietf () (interactive) (my/visit-url "datatracker.ietf.org"))

(defun my/visit-github () (interactive) (my/visit-url "github.com"))
(defvar my/github-re ".*?https://github.com/\\([a-zA-Z0-9-_\.]*\\)/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-github-stats ()
  "Fetch GitHub REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (org name) (my/parse-url my/github-re)
    (my/fetch (concat "https://api.github.com/repos/" org  "/" name)
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-16s %s")
                 (my/empty-property-drawer)
                 (org-set-property "description" (or (assoc-default 'description data) "null"))
                 (my/org-set-number-prop "stars" 'stargazers_count data)
                 (my/org-set-number-prop "open-issues" 'open_issues data)
                 (org-set-property "language" (or (assoc-default 'language data) "null"))
                 (my/org-set-boolean-prop "archived" 'archived data)
                 (my/org-set-prop "created-at" 'created_at data)
                 (my/org-set-prop "updated-at" 'updated_at data)
                 (my/fetch (concat "https://api.github.com/repos/" org  "/" name "/commits")
                           (cl-function
                            (lambda (&key data &allow-other-keys)
                              (org-set-property "last-commit-at" (assoc-default 'date (assoc-default 'author (assoc-default 'commit (aref data 0)))))
                              (my/fetch (concat "https://api.github.com/repos/" org  "/" name "/releases")
                                        (cl-function
                                         (lambda (&key data &allow-other-keys)
                                           (when (> (length data) 0)
                                             (org-set-property "last-release" (assoc-default 'tag_name (aref data 0)))
                                             (org-set-property "published-at" (assoc-default 'published_at (aref data 0))))
                                           (my/fetch (concat "https://api.github.com/repos/" org "/" name "/commits?author=Delapouite&per_page=1")
                                                     (cl-function
                                                      (lambda (&key data &allow-other-keys)
                                                        (when (> (length data) 0)
                                                          (org-set-property "contributed-at" (assoc-default 'date (assoc-default 'author (assoc-default 'commit (aref data 0)))))
                                                          (org-set-property "contributions" (concat "https://github.com/" org "/" name "/commits?author=Delapouite")))
                                                        (request (concat "https://api.github.com/user/starred/" org "/" name)
                                                          :sync t
                                                          :headers `(("Authorization" . ,(concat "Bearer " my/github-api-key)))
                                                          :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                                                                (message "Got error: starred %S" error-thrown)))
                                                          :complete (cl-function
                                                                     (lambda (&key response &allow-other-keys)
                                                                       (when (= 204 (request-response-status-code response))
                                                                         (org-set-property "starred" "true")))))
                                                        (request (concat "https://api.github.com/user/subscriptions/" org "/" name)
                                                          :sync t
                                                          :headers `(("Authorization" . ,(concat "Bearer " my/github-api-key)))
                                                          :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                                                                (message "Got error: subscriptions %S" error-thrown)))
                                                          :complete (cl-function
                                                                     (lambda (&key response &allow-other-keys)
                                                                       (when (= 204 (request-response-status-code response))
                                                                         (org-set-property "subscribed" "true")))))
                                                        (my/fetched-at)))))))))))))))

(defun my/visit-github-issues () (interactive) (my/visit-url "github.com issues"))
(defvar my/github-issues-re ".*?https://github.com/\\([a-zA-Z0-9-_\.]*\\)/\\([a-zA-Z0-9-_\.]*\\)/issues/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-github-issues-stats ()
  "Fetch GitHub REST API for issues and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (org name issue-id) (my/parse-url my/github-issues-re)
    (my/fetch (concat "https://api.github.com/repos/" org  "/" name "/issues/" issue-id)
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-12s %s")
                 (my/empty-property-drawer)
                 (my/org-set-prop "title" 'title data)
                 (org-set-property "author"  (assoc-default 'login (assoc-default 'user data)))
                 (my/org-set-prop "state" 'state data)
                 (my/org-set-number-prop "comments" 'comments data)
                 (my/org-set-prop "created-at" 'created_at data)
                 (my/org-set-prop "updated-at" 'updated_at data)
                 (org-set-property "closed-at" (or (assoc-default 'closed_at data) "null"))
                 (my/fetched-at))))))

(defun my/visit-github-pull () (interactive) (my/visit-url "github.com pull"))
(defvar my/github-pull-re ".*?https://github.com/\\([a-zA-Z0-9-_\.]*\\)/\\([a-zA-Z0-9-_\.]*\\)/pull/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-github-pull-stats ()
  "Fetch GitHub REST API for pull-requests and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (org name pull-id) (my/parse-url my/github-pull-re)
    (my/fetch (concat "https://api.github.com/repos/" org  "/" name "/pulls/" pull-id)
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-12s %s")
                 (my/empty-property-drawer)
                 (my/org-set-prop "title" 'title data)
                 (org-set-property "author"  (assoc-default 'login (assoc-default 'user data)))
                 (my/org-set-prop "state" 'state data)
                 (my/org-set-number-prop "comments" 'comments data)
                 (my/org-set-prop "created-at" 'created_at data)
                 (my/org-set-prop "updated-at" 'updated_at data)
                 (org-set-property "closed-at" (or (assoc-default 'closed_at data) "null"))
                 (org-set-property "merged-at" (or (assoc-default 'merged_at data) "null"))
                 (my/fetched-at))))))

(defvar my/github-user-re ".*?https://github.com/\\([a-zA-Z0-9-_\.]*\\).*")
(defun my/fetch-github-user-stats ()
  "Fetch GitHub REST API for user and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (username) (my/parse-url my/github-user-re)
    (my/fetch (concat "https://api.github.com/users/" username)
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-12s %s")
                 (my/empty-property-drawer)
                 (org-set-property "company" (or (assoc-default 'company data) "null"))
                 (org-set-property "location" (or (assoc-default 'location data) "null"))
                 (org-set-property "bio" (or (assoc-default 'bio data) "null"))
                 (my/org-set-number-prop "followers" 'followers data)
                 (my/org-set-number-prop "following" 'following data)
                 (my/org-set-prop "created-at" 'created_at data)
                 (my/org-set-prop "updated-at" 'updated_at data)
                 (my/fetched-at))))))

(defun my/visit-gitlab () (interactive) (my/visit-url "gitlab.com"))
(defvar my/gitlab-re ".*?https://gitlab.com/\\([a-zA-Z0-9-_\.]*\\)/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-gitlab-stats ()
  "Fetch GitLab REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (org name) (my/parse-url my/gitlab-re)
    (my/fetch (concat "https://gitlab.com/api/v4/projects/" org  "%2F" name)
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-13s %s")
                 (my/empty-property-drawer)
                 (my/org-set-prop "description" 'description data)
                 (my/org-set-number-prop "stars" 'star_count data)
                 (my/org-set-prop "created-at" 'created_at data)
                 (my/org-set-prop "updated-at" 'last_activity_at data)
                 (my/fetched-at))))))

(cl-defun my/parse-stack-response (&key data &allow-other-keys)
  (progn
    (setq org-property-format "%-12s %s")
    (my/empty-property-drawer)
    (let ((item (aref (assoc-default 'items data) 0)))
      (org-set-property "title" (xml-substitute-special (assoc-default 'title item)))
      (org-set-property "author"  (assoc-default 'display_name (assoc-default 'owner item)))
      (my/org-set-number-prop "score" 'score item)
      (my/org-set-number-prop "views" 'view_count item)
      (org-set-property "asked-at" (iso8601-format (assoc-default 'creation_date item)))
      (org-set-property "updated-at" (iso8601-format (assoc-default 'last_activity_date item))))
    (my/fetched-at)))

(cl-defun my/parse-stack-tags-response (&key data &allow-other-keys)
  (progn
    (setq org-property-format "%-12s %s")
    (my/empty-property-drawer)
    (org-set-property "count" (number-to-string (assoc-default 'count (aref (assoc-default 'items data) 0))))
    (my/fetched-at)))

(defun my/visit-stackoverflow () (interactive) (my/visit-url "stackoverflow.com"))
(defvar my/stackoverflow-re ".*?https://stackoverflow.com/questions/\\([0-9]*\\)/.*")
(defvar my/stackoverflow-tags-re ".*?https://stackoverflow.com/questions/tagged/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-stackoverflow-stats ()
  "Fetch StackOverflow REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (question-id) (my/parse-url my/stackoverflow-re)
    (my/fetch (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=stackoverflow")
              'my/parse-stack-response)))

(defun my/fetch-stackoverflow-tags-stats ()
  "Fetch StackOverflow REST API for tags and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (tag) (my/parse-url my/stackoverflow-tags-re)
    (my/fetch (concat "https://api.stackexchange.com/2.3/tags/" tag "/info?site=stackoverflow")
              'my/parse-stack-tags-response)))

(defun my/visit-serverfault () (interactive) (my/visit-url "serverfault.com"))
(defvar my/serverfault-re ".*?https://serverfault.com/questions/\\([0-9]*\\)/.*")

(defun my/fetch-serverfault-stats ()
  "Fetch ServerFault REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (question-id) (my/parse-url my/serverfault-re)
    (my/fetch (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=serverfault")
              'my/parse-stack-response)))

(defun my/visit-superuser () (interactive) (my/visit-url "superuser.com"))
(defvar my/superuser-re ".*?https://superuser.com/questions/\\([0-9]*\\)/.*")
(defvar my/superuser-tags-re ".*?https://superuser.com/questions/tagged/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-superuser-stats ()
  "Fetch SuperUser REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (question-id) (my/parse-url my/superuser-re)
    (my/fetch (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=superuser")
              'my/parse-stack-response)))

(defun my/fetch-superuser-tags-stats ()
  "Fetch SuperUser REST API for tag and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (tag) (my/parse-url my/superuser-tags-re)
    (my/fetch (concat "https://api.stackexchange.com/2.3/tags/" tag "/info?site=superuser")
              'my/parse-stack-tags-response)))

(defun my/visit-askubuntu () (interactive) (my/visit-url "askubuntu.com"))
(defvar my/askubuntu-re ".*?https://askubuntu.com/questions/\\([0-9]*\\)/.*")

(defun my/fetch-askubuntu-stats ()
  "Fetch AskUbuntu REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (question-id) (my/parse-url my/askubuntu-re)
    (my/fetch (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=askubuntu")
              'my/parse-stack-response)))

(defun my/visit-stackexchange () (interactive) (my/visit-url "stackexchange.com"))
(defvar my/stackexchange-re ".*?https://\\(.*?\\).stackexchange.com/questions/\\([0-9]*\\)/.*")
(defvar my/stackexchange-tags-re ".*?https://\\(.*?\\).stackexchange.com/questions/tagged/\\([a-zA-Z0-9-_\.]*\\).*")

(defun my/fetch-stackexchange-stats ()
  "Fetch StackExchange REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (site question-id) (my/parse-url my/stackexchange-re)
    (my/fetch (concat "https://api.stackexchange.com/2.3/questions/" question-id "?site=" site)
              'my/parse-stack-response)))

(defun my/fetch-stackexchange-tags-stats ()
  "Fetch StackExchange REST API for tag and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (site tag) (my/parse-url my/stackexchange-tags-re)
    (my/fetch (concat "https://api.stackexchange.com/2.3/tags/" tag "/info?site=" site)
              'my/parse-stack-tags-response)))

(defun my/visit-youtube () (interactive) (my/visit-url "youtube.com"))
(defvar my/youtube-re ".*?https://www.youtube.com/watch\\?v=\\([a-zA-Z0-9-_]*\\).*")

(defun my/fetch-youtube-stats ()
  "Fetch Youtube REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (youtube-id) (my/parse-url my/youtube-re)
    (my/fetch (concat "https://youtube.googleapis.com/youtube/v3/videos?part=snippet%2CcontentDetails%2Cstatistics&id=" youtube-id "&key=" my/youtube-api-key)
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-14s %s")
                 (my/empty-property-drawer)
                 (let ((item (aref (assoc-default 'items data) 0)))
                   (org-set-property "channel" (assoc-default 'channelTitle (assoc-default 'snippet item)))
                   (org-set-property "title" (assoc-default 'title (assoc-default 'snippet item)))
                   (org-set-property "views" (assoc-default 'viewCount (assoc-default 'statistics item)))
                   (org-set-property "duration" (assoc-default 'duration (assoc-default 'contentDetails item)))
                   (org-set-property "published-at" (assoc-default 'publishedAt (assoc-default 'snippet item))))
                 (my/fetched-at))))))

(defun my/visit-npm () (interactive) (my/visit-url "npmjs.com"))
(defvar my/npm-re ".*?https://www.npmjs.com/package/\\([a-zA-Z0-9-_@/]*\\).*")

(defun my/fetch-npm-stats ()
  "Fetch Npm REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (npm-id) (my/parse-url my/npm-re)
    (my/fetch (concat "https://registry.npmjs.com/" npm-id)
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (let* ((last-version (assoc-default 'latest (assoc-default 'dist-tags data)))
                        (version (assoc-default (intern last-version) (assoc-default 'versions data)))
                        (dependencies (length (assoc-default 'dependencies version)))
                        (types (assoc-default 'types version))
                        (npm-id-hex (url-hexify-string npm-id)))
                   (setq org-property-format "%-14s %s")
                   (my/empty-property-drawer)
                   (my/org-set-prop "license" 'license data)
                   (org-set-property "dependencies" (number-to-string dependencies))
                   (org-set-property "last-version" last-version)
                   (org-set-property "types" (or types "null"))
                   ;; final slash is important to get the listing
                   (org-set-property "unpkg" (concat "https://unpkg.com/" npm-id "/"))
                   (org-set-property "libraries.io" (concat "https://libraries.io/npm/" npm-id-hex))
                   (org-set-property "bundlephobia" (concat "https://bundlephobia.com/package/" npm-id-hex))
                   (org-set-property "pkgphobia" (concat "https://packagephobia.com/result?p=" npm-id))
                   (org-set-property "created-at" (assoc-default 'created (assoc-default 'time data)))
                   (org-set-property "updated-at" (assoc-default 'modified (assoc-default 'time data)))
                   (my/fetch (concat "https://libraries.io/api/npm/" npm-id-hex "?api_key=" my/libraries-io-api-key)
                             (cl-function
                              (lambda (&key data &allow-other-keys)
                                (my/org-set-number-prop "dependents" 'dependents_count data)
                                (my/org-set-number-prop "source-rank" 'rank data)
                                (my/fetch (concat "https://packagephobia.com/api.json?p=" npm-id)
                                          (cl-function
                                           (lambda (&key data &allow-other-keys)
                                             (my/org-set-number-prop "install-size" 'installSize data)
                                             (my/fetch (concat "https://api.npmjs.org/downloads/point/last-month/" npm-id)
                                                       (cl-function
                                                        (lambda (&key data &allow-other-keys)
                                                          (my/org-set-number-prop "downloads" 'downloads data)
                                                          (my/fetched-at))))))))))))))))

(defun my/visit-musicbrainz () (interactive) (my/visit-url "musicbrainz.org"))
(defvar my/musicbrainz-re ".*?https://musicbrainz.org/artist/\\([a-zA-Z0-9-_]*\\).*")

(defun my/fetch-musicbrainz-stats ()
  "Fetch MusicBrainz REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (artist-id) (my/parse-url my/musicbrainz-re)
    (my/fetch (concat "https://musicbrainz.org/ws/2/artist/" artist-id "?fmt=json&inc=url-rels")
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-12s %s")
                 (my/empty-property-drawer)
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

(defun my/visit-archlinux () (interactive) (my/visit-url "/archlinux.org"))
(defun my/visit-archlinux-man () (interactive) (my/visit-url "man.archlinux.org"))
(defun my/visit-archlinux-wiki () (interactive) (my/visit-url "wiki.archlinux.org"))
(defvar my/archlinux-re ".*?https://archlinux.org/packages/\\(community\\|core\\|extra\\)/\\(any\\|x86_64\\)/\\([a-zA-Z0-9-_]*\\)/.*")

(defun my/fetch-archlinux-stats ()
  "Fetch Archlinux REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (repo arch package-id) (my/parse-url my/archlinux-re)
    (my/fetch (concat "https://www.archlinux.org/packages/" repo "/" arch "/" package-id "/json")
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-17s %s")
                 (my/empty-property-drawer)
                 (my/org-set-prop "name" 'pkgname data)
                 (my/org-set-prop "description" 'pkgdesc data)
                 (my/org-set-prop "remote-version" 'pkgver data)
                 (org-set-property "system-version" (string-trim (shell-command-to-string (concat "pacman -Q " package-id " 2> /dev/null | awk '{ print $2 }'"))))
                 (org-set-property "dependencies" (number-to-string (length (assoc-default 'depends data))))
                 (my/org-set-number-prop "compressed-size" 'compressed_size data)
                 (my/org-set-number-prop "installed-size" 'installed_size data)
                 (my/org-set-prop "built-at" 'build_date data)
                 (my/org-set-prop "updated-at" 'last_update data)
                 (my/fetched-at))))))

(defun my/visit-aur () (interactive) (my/visit-url "aur.archlinux.org"))
(defvar my/aur-re ".*?https://aur.archlinux.org/packages/\\([a-zA-Z0-9-_]*\\).*")

(defun my/fetch-aur-stats ()
  "Fetch AUR REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (package-id) (my/parse-url my/aur-re)
    (my/fetch (concat "https://aur.archlinux.org/rpc/?v=5&type=info&arg=" package-id)
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-16s %s")
                 (my/empty-property-drawer)
                 (let ((item (aref (assoc-default 'results data) 0)))
                   (my/org-set-prop "name" 'Name item)
                   (my/org-set-prop "description" 'Description item)
                   (my/org-set-prop "remote-version" 'Version item)
                   (org-set-property "system-version" (string-trim (shell-command-to-string (concat "pacman -Q " package-id " 2> /dev/null | awk '{ print $2 }'"))))
                   (org-set-property "created-at" (iso8601-format (assoc-default 'FirstSubmitted item)))
                   (org-set-property "updated-at" (iso8601-format (assoc-default 'LastModified item))))
                 (my/fetched-at))))))

(defun my/visit-docker-hub () (interactive) (my/visit-url "hub.docker.com"))
(defvar my/docker-hub-re ".*?https://hub.docker.com/r/\\([a-zA-Z0-9-_]*\\)/\\([a-zA-Z0-9-_]*\\).*")
(defvar my/docker-hub-official-re ".*?https://hub.docker.com/_/\\([a-zA-Z0-9-_]*\\).*")

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

(defun my/visit-bundlephobia () (interactive) (my/visit-url "bundlephobia.com"))
(defvar my/bundlephobia-re ".*?https://bundlephobia.com/package/\\([a-zA-Z0-9-_@.]*\\).*")

(defun my/fetch-bundlephobia-stats ()
  "Fetch Bundle Phobia REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (package) (my/parse-url my/bundlephobia-re)
    (my/fetch (concat "https://bundlephobia.com/api/size?record=true&package=" package)
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-12s %s")
                 (my/empty-property-drawer)
                 (my/org-set-number-prop "size" 'size data)
                 (my/org-set-number-prop "gzip" 'gzip data)
                 (my/fetched-at))))))

(defun my/visit-vscode () (interactive) (my/visit-url "marketplace.visualstudio.com"))
(defvar my/vscode-re ".*?https://marketplace.visualstudio.com/items\\?itemName=\\([a-zA-Z0-9-_@.]*\\).*")

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
                         (stats (assoc-default 'statistics extension))
                         (manifest (assoc-default 'source (aref (assoc-default 'files last-version) 0))))
                    (setq org-property-format "%-18s %s")
                    (my/empty-property-drawer)
                    (my/org-set-prop "name" 'displayName extension)
                    (my/org-set-prop "description" 'shortDescription extension)
                    (my/org-set-prop "version" 'version last-version)
                    (org-set-property "score" (number-to-string (round-2 (cdr (nth 1 (aref stats 1))))))
                    (org-set-property "manifest" manifest)
                    (my/org-set-prop "updated-at" 'lastUpdated extension)
                    (my/fetch manifest
                              (cl-function
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

(defun my/visit-wikipedia () (interactive) (my/visit-url "wikipedia.org"))
(defvar my/wikipedia-re ".*?https://\\(.*?\\).wikipedia.org/wiki/\\([a-zA-Z0-9-_@.%()]*\\).*")

(defun my/fetch-wikipedia-stats ()
  "Fetch Wikipedia REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (lang page) (my/parse-url my/wikipedia-re)
    (my/fetch (concat "https://" lang ".wikipedia.org/w/rest.php/v1/page/" page "/history/counts/edits")
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-12s %s")
                 (my/empty-property-drawer)
                 (my/org-set-number-prop "revisions" 'count data)
                 (my/fetch (concat "https://" lang ".wikipedia.org/w/rest.php/v1/page/" page "/history")
                           (cl-function
                            (lambda (&key data &allow-other-keys)
                              (org-set-property "updated-at" (assoc-default 'timestamp (aref (assoc-default 'revisions data) 0)))
                              (my/fetched-at)))))))))

(defun my/visit-archlinux-wiki () (interactive) (my/visit-url "wiki.archlinux.org"))
(defvar my/archlinux-wiki-re ".*?https://wiki.archlinux.org/title/\\([a-zA-Z0-9-_@.%]*\\).*")

(defun my/fetch-archlinux-wiki-stats ()
  "Fetch Arch Wiki REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (page) (my/parse-url my/archlinux-wiki-re)
    (my/fetch (concat "https://wiki.archlinux.org/rest.php/v1/page/" page "/history")
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-12s %s")
                 (my/empty-property-drawer)
                 (org-set-property "updated-at" (assoc-default 'timestamp (aref (assoc-default 'revisions data) 0)))
                 (my/fetched-at))))))

(defun my/visit-hacker-news () (interactive) (my/visit-url "news.ycombinator.com"))
(defvar my/hacker-news-re ".*?https://news.ycombinator.com/item\\?id=\\([a-zA-Z0-9-_]*\\).*")

(defun my/fetch-hacker-news-stats ()
  "Fetch Hacker News REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (seq-let (item) (my/parse-url my/hacker-news-re)
    (my/fetch (concat "https://hacker-news.firebaseio.com/v0/item/" item ".json")
              (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq org-property-format "%-12s %s")
                 (my/empty-property-drawer)
                 (org-set-property "title" (or (assoc-default 'title data) "null"))
                 (org-set-property "url" (or (assoc-default 'url data) "null"))
                 (my/org-set-prop "author" 'by data)
                 (my/org-set-number-prop "score" 'score data)
                 (my/org-set-number-prop "posts" 'descendants data)
                 (org-set-property "created-at" (iso8601-format (assoc-default 'time data)))
                 (my/fetched-at))))))

(defun my/fetch-stats ()
  "Fetch current website REST API and add the returned values in a PROPERTIES drawer"
  (interactive)
  (let ((line-content (my/get-current-line-content)))
    (cond
      ((string-match-p my/archlinux-re line-content) (my/fetch-archlinux-stats))
      ((string-match-p my/archlinux-wiki-re line-content) (my/fetch-archlinux-wiki-stats))
      ((string-match-p my/askubuntu-re line-content) (my/fetch-askubuntu-stats))
      ((string-match-p my/aur-re line-content) (my/fetch-aur-stats))
      ((string-match-p my/bundlephobia-re line-content) (my/fetch-bundlephobia-stats))
      ((string-match-p my/docker-hub-re line-content) (my/fetch-docker-hub))
      ((string-match-p my/docker-hub-official-re line-content) (my/fetch-docker-official-hub))
      ((string-match-p my/hacker-news-re line-content) (my/fetch-hacker-news-stats))
      ((string-match-p my/github-issues-re line-content) (my/fetch-github-issues-stats))
      ((string-match-p my/github-pull-re line-content) (my/fetch-github-pull-stats))
      ((string-match-p my/github-re line-content) (my/fetch-github-stats))
      ((string-match-p my/gitlab-re line-content) (my/fetch-gitlab-stats))
      ((string-match-p my/musicbrainz-re line-content) (my/fetch-musicbrainz-stats))
      ((string-match-p my/npm-re line-content) (my/fetch-npm-stats))
      ((string-match-p my/serverfault-re line-content) (my/fetch-serverfault-stats))
      ((string-match-p my/superuser-tags-re line-content) (my/fetch-superuser-tags-stats))
      ((string-match-p my/superuser-re line-content) (my/fetch-superuser-stats))
      ((string-match-p my/stackexchange-tags-re line-content) (my/fetch-stackexchange-tags-stats))
      ((string-match-p my/stackexchange-re line-content) (my/fetch-stackexchange-stats))
      ((string-match-p my/stackoverflow-tags-re line-content) (my/fetch-stackoverflow-tags-stats))
      ((string-match-p my/stackoverflow-re line-content) (my/fetch-stackoverflow-stats))
      ((string-match-p my/vscode-re line-content) (my/fetch-vscode))
      ((string-match-p my/wikipedia-re line-content) (my/fetch-wikipedia-stats))
      ((string-match-p my/youtube-re line-content) (my/fetch-youtube-stats)))))
