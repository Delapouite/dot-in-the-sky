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

(defun my/org-link-rename-vs ()
  (interactive)
  (let ((raw-path (my/org-link-raw))
        (description (my/org-link-description)))
    (if (org-in-regexp org-link-bracket-re 1)
        (delete-region (match-beginning 0) (match-end 0)))
    (org-insert-link nil (s-replace " vs " "·" raw-path) (s-replace " vs " "·" description))))

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
  "Turn the description of link into its bathonym if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (title (org-roam-node-title node)))
    (when (not (string= "" title))
      (my/org-link-description-replace title))))

(defun my/org-link-description-to-bathonym-acronym ()
  "Turn the description of link into its bathonym ‹acronym› if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (title (org-roam-node-title node))
         (acronym (org-roam-node-acronym node)))
    (when (not (string= "" acronym))
      (my/org-link-description-replace (concat title " ‹" acronym "›")))))

(defun my/org-link-description-to-spec ()
  "Turn the description of link into its spec «description» if it exists"
  (interactive)
  (let* ((node (my/org-roam-node-from-link))
         (title (org-roam-node-title node))
         (spec-description (org-roam-node-description node)))
    (when (not (string= "" spec-description))
      (my/org-link-description-replace (concat title " «" spec-description "»")))))
