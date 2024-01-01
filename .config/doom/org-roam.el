;;; org-roam.el -*- lexical-binding: t; -*-

(use-package! org-roam
  :custom

  (org-roam-directory "~/Sync/org/roam")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags:")
      :unnarrowed t)
     ("b" "book" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :book:\n\n* Wiki")
      :unnarrowed t)
     ("c" "concept" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :concept:\n\n* Wiki")
      :unnarrowed t)
     ("i" "artist" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :artist:\n\n* Wiki\n* Albums")
      :unnarrowed t)
     ("l" "album" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :album:\n\n* Wiki\n* Tracks")
      :unnarrowed t)
     ("p" "person" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :person:\n")
      :unnarrowed t)
     ("t" "tool" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+filetags: :tool:\n\n* Wiki\n* Doc\n\* Repo\n* Usage")
      :unnarrowed t)))

  :config

  ; images

  (setq org-image-max-width 100)
  (setq org-startup-with-inline-images t)

  ; minibuffer

  (defun my/org-roam-node-find-album ()
    (interactive)
    (my/org-roam-template-default)
    (org-roam-node-find nil "#album"))

  (defun my/org-roam-node-find-book ()
    (interactive)
    (my/org-roam-template-default)
    (org-roam-node-find nil "#book"))

  (defun my/org-roam-node-find-artist ()
    (interactive)
    (my/org-roam-template-default)
    (org-roam-node-find nil "#artist"))

  (defun my/org-roam-node-find-concept ()
    (interactive)
    (my/org-roam-template-default)
    (org-roam-node-find nil "#concept"))

  (defun my/org-roam-node-find-person ()
    (interactive)
    (my/org-roam-template-default)
    (org-roam-node-find nil "#person"))

  (defun my/org-roam-node-find-tool ()
    (interactive)
    (my/org-roam-template-default)
    (org-roam-node-find nil "#tool"))

  (defun my/org-roam-node-find-default ()
    (interactive)
    (my/org-roam-template-default)
    (org-roam-node-find))

  (defun my/org-roam-template-default ()
    (interactive)
    (setq org-roam-node-display-template "${my-title:*} | @${my-level} | f${file:50} | ★${interest} | ↑${upgraded-at} | m${mtime} | ${my-tags:50}"))

  (setq org-tags-exclude-from-inheritance '("album" "artist" "debut" "top"))

  ; The gain in performance is quite significant, from 3 seconds to instant
  (setq org-roam-node-default-sort nil)
  (require 'memoize)
  (memoize 'org-roam-node-read--completions "10 minute")

  (defun memoize-force-update (func &optional timeout)
    (when (get func :memoize-original-function)
      (progn (memoize-restore func)
             (memoize func timeout))))

  (defun my/force-update-org-roam-node-read-if-memoized (&optional timeout)
    (interactive)
    (memoize-force-update 'org-roam-node-read--completions
                          (if timeout timeout memoize-default-timeout)))

  (run-with-idle-timer 180 t #'my/force-update-org-roam-node-read-if-memoized)

  (my/org-roam-template-default)

  ; sections in sidebar

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

  ; org-link

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
                          (lambda (node) (member "person" (org-roam-node-tags node)))))

  (defun org-roam-node-random-tool (&optional other-window)
    "Find and open a random Org-roam tool node.
     With prefix argument OTHER-WINDOW, visit the node in another window instead."
    (interactive current-prefix-arg)
    (org-roam-node-random other-window
                          (lambda (node) (member "tool" (org-roam-node-tags node)))))

  (defun org-roam-node-random-artist (&optional other-window)
    "Find and open a random Org-roam artist node.
     With prefix argument OTHER-WINDOW, visit the node in another window instead."
    (interactive current-prefix-arg)
    (org-roam-node-random other-window
                          (lambda (node) (member "artist" (org-roam-node-tags node)))))

  (defun org-roam-node-random-album (&optional other-window)
    "Find and open a random Org-roam album node.
     With prefix argument OTHER-WINDOW, visit the node in another window instead."
    (interactive current-prefix-arg)
    (org-roam-node-random other-window
                          (lambda (node) (member "album" (org-roam-node-tags node)))))

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

  ;; not used because too slow :(
  (cl-defmethod org-roam-node-links-count ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= source $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "%d" count)))

  ;; not used because too slow :(
  (cl-defmethod org-roam-node-backlinks-count ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "%d" count)))

  (cl-defmethod org-roam-node-my-level ((node org-roam-node))
    (number-to-string (org-roam-node-level node)))

  (cl-defmethod org-roam-node-mtime ((node org-roam-node))
    (format-time-string "%Y-%m-%d" (org-roam-node-file-mtime node)))

  (cl-defmethod org-roam-node-upgraded-at ((node org-roam-node))
    (let* ((upgraded-at (or (cdr (assoc "UPGRADED-AT" (org-roam-node-properties node)))
                            "          ")))
      (substring upgraded-at 0 10)))

  (cl-defmethod org-roam-node-interest ((node org-roam-node))
    (or (cdr (assoc "INTEREST" (org-roam-node-properties node))) "  "))

  (cl-defmethod org-roam-node-acronym ((node org-roam-node))
    (or (cdr (assoc "ACRONYM" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-my-title ((node org-roam-node))
    (let* ((acronym (cdr (assoc "ACRONYM" (org-roam-node-properties node))))
           (title (org-roam-node-title node)))
      (cond
       ((and acronym (not (string-equal acronym title))) (concat title " ‹" acronym "›"))
       ((and acronym (string-equal acronym title)) (concat "‹" acronym "›"))
       (t title))))

  (cl-defmethod org-roam-node-released-at ((node org-roam-node))
    (or (cdr (assoc "RELEASED-AT" (org-roam-node-properties node))) "    "))

  (cl-defmethod org-roam-node-artist ((node org-roam-node))
    (or (cdr (assoc "ARTIST" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-link ((node org-roam-node))
    (format "[[id:%s][%s]]" (org-roam-node-id node) (org-roam-node-title node)))

  (cl-defmethod org-roam-node-description ((node org-roam-node))
    (or (cdr (assoc "DESCRIPTION" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-country ((node org-roam-node))
    (or (cdr (assoc "COUNTRY" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-population ((node org-roam-node))
    (or (cdr (assoc "POPULATION" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-my-tags ((node org-roam-node))
    (let* ((country (cdr (assoc "COUNTRY" (org-roam-node-properties node))))
           (country (if country (concat " ⚑" country) ""))
           (population (cdr (assoc "POPULATION" (org-roam-node-properties node))))
           (population (if population (concat " 🯅" population) ""))
           (born-at (cdr (assoc "BORN-AT" (org-roam-node-properties node))))
           (born-at (if born-at (concat " ⧖" born-at) ""))
           (died-at (cdr (assoc "DIED-AT" (org-roam-node-properties node))))
           (died-at (if died-at (concat " ⧗" died-at) ""))
           (released-at (cdr (assoc "RELEASED-AT" (org-roam-node-properties node))))
           (released-at (if released-at (concat " ⧗" released-at) ""))
           (tags (mapconcat (lambda (v) (concat "#" v)) (org-roam-node-tags node)  " ")))
      (concat tags country population born-at died-at released-at)))

  ; capture

  (defun my/org-capture-before-finalize ()
    (org-delete-property "ID")
    (setq org-property-format "%s %s")
    (org-set-property "id" (org-get-title))
    (my/created-at))

  (add-hook 'org-capture-before-finalize-hook 'my/org-capture-before-finalize)

  ; keys

  (map! :n "M-<return>" #'my/org-roam-visit-node-at-point)
  (map! :leader
        :desc "Find book" "r b" #'my/org-roam-node-find-book
        :desc "Find concept" "r c" #'my/org-roam-node-find-concept
        :desc "Find artist" "r i" #'my/org-roam-node-find-artist
        :desc "Find album" "r l" #'my/org-roam-node-find-album
        :desc "Find person" "r p" #'my/org-roam-node-find-person
        :desc "Find tool" "r t" #'my/org-roam-node-find-tool
        :desc "Find node" "r r" #'my/org-roam-node-find-default))

(use-package! org-roam-ql
  :config

  (defun my/sort-by-released-at (node1 node2)
    (string< (org-roam-node-released-at node1) (org-roam-node-released-at node2)))
  (org-roam-ql-register-sort-fn "released-at" #'my/sort-by-released-at))
