;;; org-roam-ql.el -*- lexical-binding: t; -*-

(use-package! org-roam-ql
  :config

  (defun org-dblock-write:org-roam-albums (params)
    "Write org block for org-roam-albums with PARAMS."
    (let ((artist (plist-get params :artist))
          (year (plist-get params :year)))
      (when artist
        (org-dblock-write:org-roam-ql `(:query (and (tags "album") (properties "artist" ,artist))
                                        :columns (released-at link)
                                        :sort "released-at"
                                        :no-link true)))
      (when year
        (org-dblock-write:org-roam-ql `(:query (and (tags "album") (properties "released-at" ,year))
                                        :columns (link artist)
                                        :no-link true)))))

  (defun org-dblock-write:org-roam-tracks (params)
    "Write org block for org-roam-tracks with PARAMS."
    (let ((artist (plist-get params :artist)))
      (org-dblock-write:org-roam-ql `(:query (and (tags "track") (properties "artist" ,artist))
                                      :columns (link tier played-at play-count)
                                      :no-link true)))))
