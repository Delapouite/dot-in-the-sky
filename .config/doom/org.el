;;; org.el -*- lexical-binding: t; -*-

(defun my/org-link-description-region (link)
  "Return description's region of LINK at point"
  (list (org-element-property :contents-begin link)
        (org-element-property :contents-end link)))

(defun my/org-link-description (link)
  "Return description of LINK at point"
  (let ((region (my/org-link-description-region link)))
    (buffer-substring (nth 0 region) (nth 1 region))))

(defun my/org-link-description-replace (link description)
  "Replace LINK's description by DESCRIPTION"
  (let ((region (my/org-link-description-region link)))
    (goto-char (nth 0 region))
    (delete-region (nth 0 region) (nth 1 region))
    (insert description)))

(defun my/org-link-description-downcase ()
  "Downcase description of link at point"
  (interactive)
  (let ((link (org-element-context)))
    (my/org-link-description-replace link (downcase (my/org-link-description link)))))

(defun my/org-link-description-pluralize ()
  "Pluralize description of link at point"
  (interactive)
  (let ((link (org-element-context)))
    (my/org-link-description-replace link (concat (my/org-link-description link) "s"))))

;; To dynamically build org-agenda-files when a TODO is present
(defun my/org-get-filetags ()
  (interactive)
  (split-string-and-unquote (or (cadr (assoc "FILETAGS"
                                             (org-collect-keywords '("filetags")))) "")
                            "[ :]"))

(defun my/org-set-filetags (tags)
  (org-roam-set-keyword "filetags" (org-make-tag-string (seq-uniq tags))))

(defun my/org-clean-filetags ()
  (interactive)
  (my/org-set-filetags (sort (my/org-get-filetags) #'string<)))

(defun my/org-todos-p ()
  "Return non-nil if current buffer has any todo entry.

    TODO entries marked as done are ignored, meaning that this
    function returns nil if current buffer contains only completed tasks."
  (seq-find
   (lambda (type) (eq type 'todo))
   (org-element-map
       (org-element-parse-buffer 'headline)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun my/org-update-agenda-tag ()
  "Update AGENDA tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (org-roam-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (my/org-get-filetags))
             (original-tags tags))
        (if (my/org-todos-p)
            (setq tags (cons "agenda" tags))
          (setq tags (remove "agenda" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (my/org-set-filetags tags))))))

(defun my/org-agenda-files ()
  "Return a list of note files containing 'agenda' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"agenda\"%"))]))))

(defun my/org-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (my/org-agenda-files)))

;; (add-hook 'find-file-hook #'my/org-update-agenda-tag)
(add-hook 'before-save-hook #'my/org-update-agenda-tag)

(advice-add 'org-agenda :before #'my/org-agenda-files-update)
