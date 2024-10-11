;;; org-roam-ql.el -*- lexical-binding: t; -*-
;;; https://github.com/ahmed-shariff/org-roam-ql

(use-package! org-roam-ql
  :config

  (defun my/sort-by-released-at (node1 node2)
    (string< (org-roam-node-released-at node1) (org-roam-node-released-at node2)))
  (org-roam-ql-register-sort-fn "released-at" #'my/sort-by-released-at)

  (defun org-dblock-write:combos (params)
    "Write org block for org-roam-combos with PARAMS."
    (let* ((title (or (plist-get params :title) (my/org-get-acronym) (org-get-title)))
           (title-parts (s-split "·" title))
           (compound (if (> (length title-parts) 1)
                         (concat "\\|\\(^" (nth 0 title-parts) "·.*·" (nth 1 title-parts) "$\\)") "")))
      (org-dblock-write:org-roam-ql `(:query (and (title ,(concat "\\(^" title "·\\)\\|\\(·" title "$\\)\\|\\(·" title "·\\)" compound)))
                                      :columns (combo-link combos acronym bathonym)
                                      :no-link true))))

  (defun org-dblock-write:org-roam-artist (params)
    "Write org block for org-roam-artist with PARAMS."
    (let ((country (plist-get params :country)))
      (when country
        (org-dblock-write:org-roam-ql `(:query (and (tags "artist") (properties "country" ,country))
                                        :columns (link)
                                        :no-link true)))))

  (defun org-dblock-write:artists-by-country (_params)
    (org-dblock-write:org-roam-artist `(:country ,(org-get-title))))

  (defun org-dblock-write:org-roam-albums (params)
    "Write org block for org-roam-albums with PARAMS."
    (let ((artist (plist-get params :artist))
          (year (plist-get params :year)))
      (when artist
        (org-dblock-write:org-roam-ql `(:query (and (properties type "album") (properties "artist" ,(concat "\\[" artist "\\]\\]")))
                                        :columns (released-at album-link tracks-count debut)
                                        :sort "released-at"
                                        :no-link true)))
      (when year
        (org-dblock-write:org-roam-ql `(:query (and (properties type "album") (properties "released-at" ,year))
                                        :columns (artist album-link tracks-count debut)
                                        :no-link true)))))

  (defun org-dblock-write:albums-by-artist (_params)
    (org-dblock-write:org-roam-albums `(:artist ,(org-get-title))))

  (defun org-dblock-write:albums-by-year (_params)
    (org-dblock-write:org-roam-albums `(:year ,(org-get-title))))

  (defun org-dblock-write:org-roam-tracks (params)
    "Write org block for org-roam-tracks with PARAMS."
    (let ((artist (plist-get params :artist)))
      (org-dblock-write:org-roam-ql `(:query (and (properties type "track") (properties "artist" ,(concat "\\[" artist "\\]\\]")))
                                      :columns (link instruments features tier played-at play-count)
                                      :no-link true))))

  (defun org-dblock-write:tracks-by-artist (_params)
    (org-dblock-write:org-roam-tracks `(:artist ,(org-get-title))))

  (defun org-dblock-write:org-roam-books (params)
    "Write org block for org-roam-books with PARAMS."
    (let ((author (plist-get params :author))
          (year (plist-get params :year)))
      (when author
        (org-dblock-write:org-roam-ql `(:query (and (properties type "book") (properties "author" ,author))
                                        :columns (released-at link live)
                                        :sort "released-at"
                                        :no-link true)))
      (when year
        (org-dblock-write:org-roam-ql `(:query (and (properties type "book") (properties "released-at" ,year))
                                        :columns (link author live)
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

  ;; geo

  (defun org-dblock-write:people-by-country (_params)
    (org-dblock-write:org-roam-people `(:country ,(org-get-title))))

  (defun org-dblock-write:org-roam-departements (params)
    "Write org block for org-roam-departements with PARAMS."
    (let ((region (plist-get params :region)))
      (org-dblock-write:org-roam-ql `(:query (and (properties type "departement") (properties "region" ,region))
                                      :columns (link prefecture)
                                      :no-link true))))

  (defun org-dblock-write:departements-by-region (_params)
    (org-dblock-write:org-roam-departements `(:region ,(org-get-title))))

  (defun org-dblock-write:org-roam-cities (params)
    "Write org block for org-roam-cities with PARAMS."
    (let ((departement (plist-get params :departement))
          (country (plist-get params :country)))
      (when departement
        (org-dblock-write:org-roam-ql `(:query (and (properties type "city") (properties "departement" ,departement))
                                        :columns (link population role live)
                                        :no-link true)))
      (when country
        (org-dblock-write:org-roam-ql `(:query (and (properties type "city") (properties "country" ,country))
                                        :columns (link population role live)
                                        :no-link true)))))

  (defun org-dblock-write:cities-by-country (_params)
    (org-dblock-write:org-roam-cities `(:country ,(org-get-title))))

  (defun org-dblock-write:cities-by-departement (_params)
    (org-dblock-write:org-roam-cities `(:departement ,(org-get-title))))

  ;; companies

  (defun org-dblock-write:org-roam-companies (params)
    "Write org block for org-roam-companies with PARAMS."
    (let ((year (plist-get params :year))
          (country (plist-get params :country)))
      (when year
        (org-dblock-write:org-roam-ql `(:query (and (properties type "company") (properties "released-at" ,year))
                                        :columns (link country)
                                        :no-link true)))
      (when country
        (org-dblock-write:org-roam-ql `(:query (and (properties type "company") (properties "country" ,country))
                                        :columns (link released-at)
                                        :no-link true)))))

  (defun org-dblock-write:companies-by-year (_params)
    (org-dblock-write:org-roam-companies `(:year ,(org-get-title))))

  (defun org-dblock-write:companies-by-country (_params)
    (org-dblock-write:org-roam-companies `(:country ,(org-get-title)))))
