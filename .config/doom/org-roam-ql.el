;;; org-roam-ql.el -*- lexical-binding: t; -*-
;;; https://github.com/ahmed-shariff/org-roam-ql

(use-package! org-roam-ql
  :config

  (defun org-dblock-write:combos (params)
    "Write org block for org-roam-combos with PARAMS."
    (let ((title (or (plist-get params :title) (org-get-title))))
      (org-dblock-write:org-roam-ql `(:query (and (title ,(concat "\\(^" title "·\\)\\|\\(·" title "$\\)")))
                                      :columns (combo-link combos)
                                      :no-link true))))

  (defun org-dblock-write:org-roam-albums (params)
    "Write org block for org-roam-albums with PARAMS."
    (let ((artist (plist-get params :artist))
          (year (plist-get params :year)))
      (when artist
        (org-dblock-write:org-roam-ql `(:query (and (tags "album") (properties "artist" ,(concat "\\[" artist "\\]\\]")))
                                        :columns (released-at album-link)
                                        :sort "released-at"
                                        :no-link true)))
      (when year
        (org-dblock-write:org-roam-ql `(:query (and (tags "album") (properties "released-at" ,year))
                                        :columns (artist album-link debut)
                                        :no-link true)))))

  (defun org-dblock-write:albums-by-artist (_params)
    (org-dblock-write:org-roam-albums `(:artist ,(org-get-title))))

  (defun org-dblock-write:albums-by-year (_params)
    (org-dblock-write:org-roam-albums `(:year ,(org-get-title))))

  (defun org-dblock-write:org-roam-tracks (params)
    "Write org block for org-roam-tracks with PARAMS."
    (let ((artist (plist-get params :artist)))
      (org-dblock-write:org-roam-ql `(:query (and (tags "track") (properties "artist" ,(concat "\\[" artist "\\]\\]")))
                                      :columns (link tier played-at play-count)
                                      :no-link true))))

  (defun org-dblock-write:tracks-by-artist (_params)
    (org-dblock-write:org-roam-tracks `(:artist ,(org-get-title))))

  (defun org-dblock-write:org-roam-books (params)
    "Write org block for org-roam-books with PARAMS."
    (let ((author (plist-get params :author))
          (year (plist-get params :year)))
      (when author
        (org-dblock-write:org-roam-ql `(:query (and (tags "book") (properties "author" ,author))
                                        :columns (released-at link)
                                        :sort "released-at"
                                        :no-link true)))
      (when year
        (org-dblock-write:org-roam-ql `(:query (and (tags "book") (properties "released-at" ,year))
                                        :columns (link author)
                                        :no-link true)))))

  (defun org-dblock-write:books-by-author (_params)
    (org-dblock-write:org-roam-books `(:author ,(org-get-title))))

  (defun org-dblock-write:books-by-year (_params)
    (org-dblock-write:org-roam-books `(:year ,(org-get-title))))

  (defun org-dblock-write:org-roam-people (params)
    "Write org block for org-roam-people with PARAMS."
    (let ((year (plist-get params :year))
          (country (plist-get params :country)))
      (when year
        (org-dblock-write:org-roam-ql `(:query (and (tags "person") (or (properties "born-at" ,year) (properties "died-at" ,year)))
                                        :columns (link born-at died-at country)
                                        :no-link true)))
      (when country
        (org-dblock-write:org-roam-ql `(:query (and (tags "person") (properties "country" ,country))
                                        :columns (link born-at died-at)
                                        :no-link true)))))

  (defun org-dblock-write:people-by-year (_params)
    (org-dblock-write:org-roam-people `(:year ,(org-get-title))))

  (defun org-dblock-write:people-by-country (_params)
    (org-dblock-write:org-roam-people `(:country ,(org-get-title))))

  (defun org-dblock-write:org-roam-departements (params)
    "Write org block for org-roam-departements with PARAMS."
    (let ((region (plist-get params :region)))
      (org-dblock-write:org-roam-ql `(:query (and (tags "departement") (properties "region" ,region))
                                      :columns (link prefecture)
                                      :no-link true))))

  (defun org-dblock-write:departements-by-region (_params)
    (org-dblock-write:org-roam-departements `(:region ,(org-get-title))))

  (defun org-dblock-write:org-roam-cities (params)
    "Write org block for org-roam-cities with PARAMS."
    (let ((departement (plist-get params :departement)))
      (org-dblock-write:org-roam-ql `(:query (and (tags "city") (properties "departement" ,departement))
                                      :columns (link population role live)
                                      :no-link true))))

  (defun org-dblock-write:cities-by-departement (_params)
    (org-dblock-write:org-roam-cities `(:departement ,(org-get-title)))))
