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

  ;; finders

  (setq org-tags-exclude-from-inheritance '("album" "artist" "debut" "top"))

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
    `((t :foreground ,dracula-green
         :underline t))
    "Face for Org-Mode links starting with id:."
    :group 'org-faces)
  (defface org-link-bidirectional
    `((t :foreground ,dracula-green
         :weight bold
         :underline t))
    "Face for Org-Mode bidirectional links."
    :group 'org-faces)
  (defface org-link-file
    `((t :foreground ,dracula-orange
         :underline t))
    "Face for Org-Mode links starting with file:."
    :group 'org-faces)
  (defface org-link-interpunct
    `((t :foreground ,dracula-grey
         :underline nil))
    "Face for · in Org-Mode links"
    :group 'org-faces)
  (defface org-link-error
    `((t :foreground ,dracula-red
         :underline t))
    "Face for broken Org-Mode links"
    :group 'org-faces)

  (org-link-set-parameters
   "id"
   :face (lambda (path)
           ;; let's try with title (which is the same as id in our case), it seems more reliable with caching issues
           (let* ((id (org-get-title)) ;(org-with-point-at 1 (org-id-get)))
                  (ids (my/org-roam-incoming-ids id)))
             (if (member path ids)
                 'org-link-bidirectional
               (if (org-roam-id-find path) 'org-link-id 'org-link-error)))))
  (org-link-set-parameters "file" :face 'org-link-file)
  (org-link-set-parameters "env" :face 'org-formula)
  (org-link-set-parameters "flag" :face 'org-formula)
  (org-link-set-parameters "header" :face 'org-formula)

  (defun my/org-roam-incoming-ids (id)
    (interactive)
    (seq-uniq
     (seq-map #'car
      (org-roam-db-query [:select source
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")] id))))

  (defun my/org-roam-ids ()
    (interactive)
    (seq-map #'car
     (org-roam-db-query [:select id :from nodes])))
  (setq my/org-roam-ids (my/org-roam-ids))

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

  (defun my/org-roam-save-count ()
    "Save links and backlinks count in database"
    (interactive)
    (org-with-point-at 1
      (when (and (= (org-outline-level) 0) (org-roam-db-node-p))
        (when-let ((id (org-id-get)))
          (letrec ((node (org-roam-node-from-id id))
                   (properties (org-entry-properties))
                   (links-count (org-roam-node-links-count node))
                   (backlinks-count (org-roam-node-backlinks-count node)))
            (unless (assoc "LINKS-COUNT" properties) (push '("LINKS-COUNT" . 0) properties))
            (setcdr (assoc "LINKS-COUNT" properties) links-count)
            (unless (assoc "BACKLINKS-COUNT" properties) (push '("BACKLINKS-COUNT" . 0) properties))
            (setcdr (assoc "BACKLINKS-COUNT" properties) backlinks-count)
            (org-roam-db-query
             [:update nodes
              :set (= properties $s2)
              :where (= id $s1)] id properties))))))
  '(advice-add 'org-roam-db-update-file :after #'my/org-roam-save-count)

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

  ; property accessors

  (cl-defmethod org-roam-node-mtime ((node org-roam-node))
    (format-time-string "%Y-%m-%d" (org-roam-node-file-mtime node)))

  (cl-defmethod org-roam-node-upgraded-at ((node org-roam-node))
    (let* ((upgraded-at (or (cdr (assoc "UPGRADED-AT" (org-roam-node-properties node)))
                            "          ")))
      (substring upgraded-at 0 10)))

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    (or (cdr (assoc "TYPE" (org-roam-node-properties node))) ""))


  (cl-defmethod org-roam-node-artist ((node org-roam-node))
    (or (cdr (assoc "ARTIST" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-author ((node org-roam-node))
    (or (cdr (assoc "AUTHOR" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-country ((node org-roam-node))
    (or (cdr (assoc "COUNTRY" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-prefecture ((node org-roam-node))
    (or (cdr (assoc "PREFECTURE" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-role ((node org-roam-node))
    (or (cdr (assoc "ROLE" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-population ((node org-roam-node))
    (or (cdr (assoc "POPULATION" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-play-count ((node org-roam-node))
    (or (cdr (assoc "PLAY-COUNT" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-combos ((node org-roam-node))
    (or (cdr (assoc "COMBOS" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-instruments ((node org-roam-node))
    (or (cdr (assoc "INSTRUMENTS" (org-roam-node-properties node))) ""))

  ;; alt identities

  (cl-defmethod org-roam-node-acronym ((node org-roam-node))
    (or (cdr (assoc "ACRONYM" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-description ((node org-roam-node))
    (or (cdr (assoc "DESCRIPTION" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-gerund ((node org-roam-node))
    (or (cdr (assoc "GERUND" (org-roam-node-properties node))) ""))

  ;; scales

  (cl-defmethod org-roam-node-interest ((node org-roam-node))
    (or (cdr (assoc "INTEREST" (org-roam-node-properties node))) "  "))

  (cl-defmethod org-roam-node-stage ((node org-roam-node))
    (or (cdr (assoc "STAGE" (org-roam-node-properties node))) " "))

  (cl-defmethod org-roam-node-tier ((node org-roam-node))
    (or (cdr (assoc "TIER" (org-roam-node-properties node))) ""))

  ;; links

  (cl-defmethod org-roam-node-link ((node org-roam-node))
    (format "[[id:%s][%s]]" (org-roam-node-id node) (org-roam-node-title node)))

  (cl-defmethod org-roam-node-album-link ((node org-roam-node))
    (format "[[id:%s][%s]]" (org-roam-node-id node)
            (string-join (cdr (split-string (org-roam-node-title node) " - ")) " ")))

  (cl-defmethod org-roam-node-combo-link ((node org-roam-node))
    (letrec ((link-parts (s-split "·" (org-roam-node-title node)))
             (acronym-or-title (or (my/org-get-acronym) (org-get-title)))
             (current-parts (s-split "·" acronym-or-title))
             (description (s-join "·" (-difference link-parts current-parts))))
      (format "[[id:%s][%s]]" (org-roam-node-id node) description)))

  ;; booleans

  (cl-defmethod org-roam-node-debut ((node org-roam-node))
    (or (cdr (assoc "DEBUT" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-live ((node org-roam-node))
    (or (cdr (assoc "LIVE" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-tab ((node org-roam-node))
    (or (cdr (assoc "TAB" (org-roam-node-properties node))) ""))

  ;; dates

  (cl-defmethod org-roam-node-born-at ((node org-roam-node))
    (or (cdr (assoc "BORN-AT" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-died-at ((node org-roam-node))
    (or (cdr (assoc "DIED-AT" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-played-at ((node org-roam-node))
    (or (cdr (assoc "PLAYED-AT" (org-roam-node-properties node))) ""))

  (cl-defmethod org-roam-node-released-at ((node org-roam-node))
    (or (cdr (assoc "RELEASED-AT" (org-roam-node-properties node))) "    "))

  ; fuzzy candidate templates

  (cl-defmethod org-roam-node-template-title ((node org-roam-node))
    (let* ((acronym (cdr (assoc "ACRONYM" (org-roam-node-properties node))))
           (pacronym (propertize (concat "‹" acronym "›") 'face 'org-property-value))
           (description (cdr (assoc "DESCRIPTION" (org-roam-node-properties node))))
           (pdescription (propertize (concat "«" description "»") 'face 'org-property-value))
           (gerund (cdr (assoc "GERUND" (org-roam-node-properties node))))
           (pgerund (propertize (concat "+ing") 'face 'org-property-value))
           (aliases (org-roam-node-aliases node))
           (title (org-roam-node-title node))
           (parent (car (split-string title " > ")))
           (child (cadr (split-string title " > ")))
           (ptitle (if (member child my/org-roam-ids) (concat parent " > " (propertize child 'face 'org-code)) title)))
      (cond
       ((and acronym (not (string-equal acronym title))) (concat ptitle " " pacronym))
       ((and acronym (string-equal acronym title)) pacronym)
       ((and description) (concat ptitle " " pdescription))
       ((and gerund) (concat ptitle " " pgerund))
       ((and aliases) (concat ptitle "*"))
       (t ptitle))))

  (cl-defmethod org-roam-node-template-level ((node org-roam-node))
    (number-to-string (org-roam-node-level node)))

  (cl-defmethod org-roam-node-template-mtime ((node org-roam-node))
    (let ((mtime (org-roam-node-mtime node)))
      (propertize mtime 'face (get-mtime-face mtime))))

  (cl-defmethod org-roam-node-template-links ((node org-roam-node))
    (let* ((backlinks-count (cdr (assoc "BACKLINKS-COUNT" (org-roam-node-properties node))))
           (backlinks-count (if backlinks-count (concat (string-pad backlinks-count 2) "→" ) "  →"))
           (links-count (cdr (assoc "LINKS-COUNT" (org-roam-node-properties node))))
           (links-count (if links-count (concat " →" (string-pad links-count 2)) " →  ")))
      (concat backlinks-count links-count)))

  (cl-defmethod org-roam-node-template-tags ((node org-roam-node))
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
           (type (cdr (assoc "TYPE" (org-roam-node-properties node))))
           (type (if type (concat " ‡" type) ""))
           (tags (mapconcat (lambda (v) (concat "#" v)) (org-roam-node-tags node)  " ")))
      (concat tags type country population born-at died-at released-at)))

  (defun my/org-roam-template-default ()
    (interactive)
    (setq org-roam-node-display-template "${template-title:*} | @${template-level} | f${file:50} | ${template-links} | ∧${stage} | ★${interest} | ↑${upgraded-at} | m${template-mtime} | ${combos} ${template-tags:50}"))

  (my/org-roam-template-default)

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
    (setq my/org-roam-ids (my/org-roam-ids))
    (memoize-force-update 'org-roam-node-read--completions
                          (if timeout timeout memoize-default-timeout)))

  (run-with-idle-timer 180 t #'my/force-update-org-roam-node-read-if-memoized)

  ; capture

  (defun my/org-capture-before-finalize ()
    (org-delete-property "ID")
    (setq org-property-format "%s %s")
    (org-set-property "id" (org-get-title))
    (my/created-at))

  (add-hook 'org-capture-before-finalize-hook 'my/org-capture-before-finalize)
  (add-hook 'org-mode-hook #'(lambda () (highlight-regexp "·" "org-link-interpunct")))
  (add-hook 'org-mode-hook #'my/org-replace-drawers-with-recap)

  ; navigation

  (defun my/org-get-acronym ()
    (interactive)
    (org-with-point-at 1 (cdr (assoc "ACRONYM" (org-entry-properties)))))

  (defun my/org-roam-node-from-acronym (acronym)
    (org-roam-db-query [:select [id] :from nodes
                        :where (like properties $r1)]
                       (concat "%\"ACRONYM\" . \"" acronym "\"%")))

  (defun my/org-roam-node-from-title-or-alias-or-acronym (s)
    "Return an `org-roam-node' for the node with title or alias or acronym S.
     Return nil if the node does not exist.
     Throw an error if multiple choices exist."
    (let ((matches (seq-uniq (append
                              (org-roam-db-query [:select [id] :from nodes
                                                  :where (= title $s1)] s)
                              (org-roam-db-query [:select [node-id] :from aliases
                                                  :where (= alias $s1)] s)
                              (my/org-roam-node-from-acronym s)))))
      (cond
       ((seq-empty-p matches)
        nil)
       ((= 1 (length matches))
        (org-roam-populate (org-roam-node-create :id (caar matches))))
       (t
        (user-error "Multiple nodes exist with title or alias \"%s\"" s)))))

  (defun my/org-roam-goto (s)
    "Goto roam node where S is either its title, its alias or its acronym."
    (when-let ((node (my/org-roam-node-from-title-or-alias-or-acronym s)))
      (org-mark-ring-push)
      (org-roam-node-visit node nil 'force)))

  (defun my/org-roam-goto-combo (index)
    "Goto roam node INDEX of the combo"
    (interactive)
    (let ((node (nth index (s-split "·" (org-get-title)))))
      (my/org-roam-goto node)))

  ; keys

  (map! :n "M-<return>" #'my/org-roam-visit-node-at-point)
  (map! :leader
        :desc "Find book" "r b" (lambda () (interactive) (org-roam-node-find nil "#book "))
        :desc "Find concept" "r c" (lambda () (interactive) (org-roam-node-find nil "#concept "))
        :desc "Find artist" "r i" (lambda () (interactive) (org-roam-node-find nil "#artist "))
        :desc "Find album" "r l" (lambda () (interactive) (org-roam-node-find nil "#album "))
        :desc "Find combo" "r o" (lambda () (interactive) (org-roam-node-find nil (concat "· " (or (my/org-get-acronym) (org-get-title)) " ")))
        :desc "Find person" "r p" (lambda () (interactive) (org-roam-node-find nil "#person "))
        :desc "Find tool" "r t" (lambda () (interactive) (org-roam-node-find nil "#tool "))
        :desc "Find node" "r r" (lambda () (interactive) (org-roam-node-find))
        :desc "Find ★1" "r 1" (lambda () (interactive) (org-roam-node-find nil "★+1 "))
        :desc "Goto first combo" "g 1" (lambda () (interactive) (my/org-roam-goto-combo 0))
        :desc "Goto second combo" "g 2" (lambda () (interactive) (my/org-roam-goto-combo 1))
        :desc "Goto third combo" "g 3" (lambda () (interactive) (my/org-roam-goto-combo 2))))

(use-package! org-roam-ql
  :config

  (defun my/sort-by-released-at (node1 node2)
    (string< (org-roam-node-released-at node1) (org-roam-node-released-at node2)))
  (org-roam-ql-register-sort-fn "released-at" #'my/sort-by-released-at))
