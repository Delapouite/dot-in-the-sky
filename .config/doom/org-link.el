;;; org-link.el -*- lexical-binding: t; -*-

;;; https://orgmode.org/guide/Hyperlinks.html
;;; https://orgmode.org/manual/Hyperlinks.html

(map! :leader "l a" #'my/org-link-description-to-acronym
      :leader "l b" #'my/org-link-description-to-bathonym
      :leader "l c" #'my/org-link-description-to-bathonym-acronym
      :leader "l d" #'my/org-link-description-downcase
      :leader "l i" #'org-insert-link
      :leader "l k" #'my/org-link-kill-raw
      :leader "l ·" #'my/org-link-description-switch
      :leader "l s" #'my/org-link-description-pluralize
      :leader "l t" #'org-toggle-link-display)

(defun my/org-link-kill-raw (&optional link)
  "Kill raw LINK"
  (interactive)
  (unless link (setq link (org-element-context)))
  (kill-new (my/org-link-raw)))

(defun my/org-link-rename-plus ()
  (interactive)
  (let ((raw-path (my/org-link-raw))
        (description (my/org-link-description)))
    (if (org-in-regexp org-link-bracket-re 1)
        (delete-region (match-beginning 0) (match-end 0)))
    (org-insert-link nil (s-replace " + " "·" raw-path) (s-replace " + " "·" description))))

(defun my/org-link-raw (&optional link)
  "Return raw LINK"
  (unless link (setq link (org-element-context)))
  (org-element-property :raw-link link))

(defun my/org-link-message-raw ()
  "Message raw link at point"
  (interactive)
  (message (my/org-link-raw)))

(defun my/org-link-path (&optional link)
  "Return path (scheme-less) of LINK"
  (unless link (setq link (org-element-context)))
  (org-element-property :path link))

(defun my/org-link-message-path ()
  "Message path (scheme-less) of link at point"
  (interactive)
  (message (my/org-link-path)))

(defun my/org-link-description-region (&optional link)
  "Return description's region of LINK"
  (unless link (setq link (org-element-context)))
  (list (org-element-property :contents-begin link)
        (org-element-property :contents-end link)))

(defun my/org-link-description (&optional link)
  "Return description of LINK"
  (unless link (setq link (org-element-context)))
  (let ((region (my/org-link-description-region link)))
    (buffer-substring (nth 0 region) (nth 1 region))))

(defun my/org-link-description-replace (description &optional link)
  "Replace LINK's description by DESCRIPTION"
  (unless link (setq link (org-element-context)))
  (let ((region (my/org-link-description-region link)))
    (goto-char (nth 0 region))
    (delete-region (nth 0 region) (nth 1 region))
    (insert description)))

(defun my/org-roam-node-from-link ()
  "Return Org-Roam node from link at point"
  (org-roam-node-from-id (my/org-link-path)))

(defun my/org-link-from-word ()
  "Turn word under point into link"
  (interactive)
  (forward-word)
  (let ((word (word-at-point)))
    (backward-word)
    (kill-word nil)
    (insert (concat "[[id:" word "][" word "]]"))
    (message word)))

;;; description replacers

(defun my/org-link-description-downcase ()
  "Downcase description of link at point"
  (interactive)
  (my/org-link-description-replace (downcase (my/org-link-description))))

(defun my/org-link-description-pluralize ()
  "Pluralize description of link at point"
  (interactive)
  (my/org-link-description-replace (concat (my/org-link-description) "s")))

(defun my/org-link-description-switch ()
  "Switch the two parts of a combo (·) in the description of link at point"
  (interactive)
  (let* ((description (my/org-link-description))
         (parts (split-string description "·" t " ")))
      (my/org-link-description-replace (concat (nth 1 parts) "·" (nth 0 parts)))))

(defun my/org-link-description-to-acronym ()
  "Turn the description of link into its acronym if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (acronym (org-roam-node-acronym node)))
    (when (not (string= "" acronym))
      (my/org-link-description-replace (concat "‹" acronym "›")))))

(defun my/org-link-description-to-bathonym ()
  "Turn the description of link into its bathonym or title if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (bathonym (org-roam-node-bathonym node))
         (title (org-roam-node-title node)))
    (cond
     ((not (string= "" bathonym)) (my/org-link-description-replace bathonym))
     ((not (string= "" title)) (my/org-link-description-replace title)))))

(defun my/org-link-description-to-acronym-bathonym ()
  "Turn the description of link into its ‹acronym› bathonym if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (title (org-roam-node-title node))
         (bathonym (org-roam-node-bathonym node))
         (acronym (org-roam-node-acronym node)))
    (when (not (string= "" acronym))
      (cond
       ((not (string= "" bathonym)) (my/org-link-description-replace (concat "‹" acronym "› " bathonym)))
       ((not (string= "" title)) (my/org-link-description-replace (concat "‹" acronym "› " title)))))))

(defun my/org-link-description-to-bathonym-acronym ()
  "Turn the description of link into its bathonym ‹acronym› if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (title (org-roam-node-title node))
         (bathonym (org-roam-node-bathonym node))
         (acronym (org-roam-node-acronym node)))
    (when (not (string= "" acronym))
      (cond
       ((not (string= "" bathonym)) (my/org-link-description-replace (concat bathonym " ‹" acronym "›")))
       ((not (string= "" title)) (my/org-link-description-replace (concat title " ‹" acronym "›")))))))

(defun my/org-link-description-to-spec ()
  "Turn the description of link into its spec «description» if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (title (org-roam-node-title node))
         (year (org-roam-node-released-at node))
         (spec-description (org-roam-node-description node)))
    (when (not (string= "" spec-description))
      (if (string= "    " year)
          (my/org-link-description-replace (concat title " «" spec-description "»"))
        (my/org-link-description-replace (concat title " (" year ") «" spec-description "»"))))))

(defun my/org-link-description-to-gerund ()
  "Turn the description of link into its gerund if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (gerund (org-roam-node-gerund node)))
    (when (not (string= "" gerund))
      (my/org-link-description-replace gerund))))

(defun my/org-link-description-to-tion ()
  "Turn the description of link into its tion if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (tion (org-roam-node-tion node)))
    (when (not (string= "" tion))
      (my/org-link-description-replace tion))))

(defun my/org-link-description-with-released-at ()
  "Append released-at property to the description of link if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (title (org-roam-node-title node))
         (released-at (org-roam-node-released-at node)))
    (when (not (string= "    " released-at))
       (my/org-link-description-replace (concat title " (" released-at ")")))))

(defun my/org-link-description-with-born-died-at ()
  "Append born-died-at properties to the description of link if they exist"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (title (org-roam-node-title node))
         (born-at (org-roam-node-born-at node))
         (died-at (org-roam-node-died-at node)))
    (when (and (not (string= "" born-at)) (not (string= "" died-at)))
       (my/org-link-description-replace (concat title " (" born-at "/" died-at ")")))))

(defun my/org-link-description-prepend-fa ()
  "Prepend Font Awesome Icon for well-known brands"
  (interactive)
  (let ((description (my/org-link-description)))
    (when (s-ends-with? " - Wikipedia" description)
      (my/org-link-description-replace (concat " " (s-chop-suffix " - Wikipedia" description))))
    (when (s-ends-with? " · GitHub" description)
      (my/org-link-description-replace (concat " " (s-chop-suffix " · GitHub" description))))
    (when (s-starts-with? "GitHub - " description)
      (my/org-link-description-replace (concat " " (s-chop-prefix "GitHub - " description))))
    (when (s-ends-with? " - Stack Overflow" description)
      (my/org-link-description-replace (concat " " (s-chop-suffix " - Stack Overflow" description))))
    (when (s-ends-with? " - Server Fault" description)
      (my/org-link-description-replace (concat " " (s-chop-suffix " - Server Fault" description))))
    (when (s-ends-with? " - Unix & Linux Stack Exchange" description)
      (my/org-link-description-replace (concat " " (s-chop-suffix " - Unix & Linux Stack Exchange" description))))
    (when (s-ends-with? " - Super User" description)
      (my/org-link-description-replace (concat " " (s-chop-suffix " - Super User" description))))
    (when (s-ends-with? " - Emacs Stack Exchange" description)
      (my/org-link-description-replace (concat " " (s-chop-suffix " - Emacs Stack Exchange" description))))
    (when (s-ends-with? " - YouTube" description)
      (my/org-link-description-replace (concat " " (s-chop-suffix " - YouTube" description))))
    (when (s-ends-with? " | Hacker News" description)
      (my/org-link-description-replace (concat " " (s-chop-suffix " | Hacker News" description))))
    (when (s-ends-with? " - OpenBSD manual pages" description)
      (my/org-link-description-replace (concat " " (s-chop-suffix " - OpenBSD manual pages" description))))
    ))

(defun my/org-link-description-prepend-fa-wikipedia ()
  (interactive)
  (my/org-link-description-replace (concat " " (my/org-link-description))))

(defun my/org-link-description-prepend-fa-docker ()
  (interactive)
  (my/org-link-description-replace (concat " " (my/org-link-description))))

(defun my/org-link-description-prepend-fa-github ()
  (interactive)
  (my/org-link-description-replace (concat " " (my/org-link-description))))

(defun my/org-link-description-prepend-fa-stackoverflow ()
  (interactive)
  (my/org-link-description-replace (concat " " (my/org-link-description))))
