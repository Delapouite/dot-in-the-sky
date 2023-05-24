;;; org.el -*- lexical-binding: t; -*-

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
            (setq tags (cons "Agenda" tags))
          (setq tags (remove "Agenda" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (my/org-set-filetags tags))))))

(defun my/org-agenda-files ()
  "Return a list of note files containing 'Agenda' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"Agenda\"%"))]))))

(defun my/org-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (my/org-agenda-files)))

;; (add-hook 'find-file-hook #'my/org-update-agenda-tag)
(add-hook 'before-save-hook #'my/org-update-agenda-tag)

(advice-add 'org-agenda :before #'my/org-agenda-files-update)
