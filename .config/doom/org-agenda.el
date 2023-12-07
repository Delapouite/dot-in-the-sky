;;; org-agenda.el -*- lexical-binding: t; -*-

(defun my/org-todos-p ()
  "Return non-nil if current buffer has any todo entry.

   TODO entries marked as done are ignored, meaning that this
   function returns nil if current buffer contains only completed tasks."
  (seq-find
   (lambda (type) (eq type 'todo))
   (org-element-map
       (org-element-parse-buffer 'headline)
       'headline
     (lambda (h) (org-element-property :todo-type h)))))

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
  (interactive)
  (setq org-agenda-files (my/org-agenda-files)))

(add-hook 'before-save-hook #'my/org-update-agenda-tag)
(advice-add 'org-agenda :before #'my/org-agenda-files-update)

(defun my/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-todo-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'my/org-summary-todo)
