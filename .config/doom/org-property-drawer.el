;;; property-drawer.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun my/kill-drawer ()
  "Kill drawer"
  (interactive)
  (let ((element (org-element-context)))
    (unless (eq (car element) 'drawer)
      (setq element (org-element-property :parent element)))
    (kill-region (org-element-property :begin element)
                 (org-element-property :end element))))

(defun my/empty-property-drawer ()
  (let ((element (org-get-property-block)))
    (when element
      (kill-region (car element) (cdr element)))))

(defun my/org-set-prop (prop key alist)
  (org-set-property prop (assoc-default key alist)))

(defun my/org-set-number-prop (prop key alist)
  (org-set-property prop (number-to-string (assoc-default key alist))))

(defun my/org-set-boolean-prop (prop key alist)
  (unless (eq :json-false (assoc-default key alist))
    (org-set-property prop "true")))

(defun my/fetched-at ()
  "Set drawer property fetched-at to now in ISO8601"
  (org-set-property "fetched-at" (iso8601-format))
  (setq org-property-format "%-10s %s"))

(defun my/upgraded-at ()
  "Set drawer property upgrated-at to now in ISO8601"
  (interactive)
  (org-set-property "upgraded-at" (iso8601-format)))

(defun my/published-at (date)
  "Set drawer property published-at to provided DATE"
  (interactive "spublished-at? ")
  (org-set-property "published-at" date))

(defun my/created-at ()
  "Set drawer property created-at to now in ISO8601"
  (interactive)
  (org-set-property "created-at" (iso8601-format)))

(defun my/played-at ()
  "Set drawer property played-at to now in ISO8601"
  (interactive)
  (org-set-property "played-at" (iso8601-format)))

(defun my/infer-created-at ()
  "Set drawer property created-at to date in buffer name"
  (interactive)
  (let ((y (substring (buffer-name) 0 4))
        (M (substring (buffer-name) 4 6))
        (d (substring (buffer-name) 6 8))
        (h (substring (buffer-name) 8 10))
        (m (substring (buffer-name) 10 12))
        (s (substring (buffer-name) 12 14)))
    (org-with-point-at 1
      (org-set-property "created-at" (concat y "-" M "-" d "T" h ":" m ":" s "+0200")))))

;; overriden lowercase version
(defun my/org-insert-property-drawer ()
  "Insert a property drawer into the current entry.
Do nothing if the drawer already exists.  The newly created
drawer is immediately hidden."
  (org-with-wide-buffer
   ;; Set point to the position where the drawer should be inserted.
   (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
       (org-back-to-heading-or-point-min t)
     (org-with-limited-levels (org-back-to-heading-or-point-min t)))
   (if (org-before-first-heading-p)
       (while (and (org-at-comment-p) (bolp)) (forward-line))
     (forward-line)
     (when (looking-at-p org-planning-line-re) (forward-line)))
   (unless (looking-at-p org-property-drawer-re)
     ;; Make sure we start editing a line from current entry, not from
     ;; next one.  It prevents extending text properties or overlays
     ;; belonging to the latter.
     (when (and (bolp) (> (point) (point-min))) (backward-char))
     (let ((begin (if (bobp) (point) (1+ (point))))
           (inhibit-read-only t))
       (unless (bobp) (insert "\n"))
       (insert ":properties:\n:end:")
       (org-fold-region (line-end-position 0) (point) t (if (eq org-fold-core-style 'text-properties) 'drawer 'outline))
       (when (or (eobp) (= begin (point-min))) (insert "\n"))
       (org-indent-region begin (point))))))

(advice-add 'org-insert-property-drawer :override #'my/org-insert-property-drawer)

(defun my/org-keywords-to-lowercase ()
  "Lower case Org keywords and block identifiers.

Example: \"#+TITLE\" -> \"#+title\"
         \"#+BEGIN_EXAMPLE\" -> \"#+begin_example\"

Inspiration:
https://code.orgmode.org/bzg/org-mode/commit/13424336a6f30c50952d291e7a82906c1210daf0."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      ;; Match examples: "#+FOO bar", "#+FOO:", "=#+FOO=", "~#+FOO~",
      ;;                 "‘#+FOO’", "“#+FOO”", ",#+FOO bar",
      ;;                 "#+FOO_bar<eol>", "#+FOO<eol>".
      (while (re-search-forward "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)" nil :noerror)
        (setq count (1+ count))
        (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil 1))
      (message "Lower-cased %d matches" count))))

(defun my/org-drawer-recap (drawer-props)
  "Compute recap of an org properties-drawer"
  (let ((parts '()))
    ;; dates
    (when-let (prop (cdr (assoc "FETCHED-AT" drawer-props)))
      (push (substring prop 0 7) parts))
    (when-let (prop (cdr (assoc "RELEASED-AT" drawer-props)))
      (push (substring prop 0 4) parts))
    (when-let (prop (cdr (assoc "PUBLISHED-AT" drawer-props)))
      (push (substring prop 0 4) parts))
    (when-let (prop (cdr (assoc "UPGRADED-AT" drawer-props)))
      (push (concat "↑" (substring prop 0 10)) parts))
    (when-let (prop (cdr (assoc "PLAYED-AT" drawer-props)))
      (push (substring prop 0 7) parts))
    ;; suffixes
    (when-let (prop (cdr (assoc "REVISIONS" drawer-props)))
      (push (concat prop "revisions") parts))
    (when-let (prop (cdr (assoc "FOLLOWERS" drawer-props)))
      (push (concat prop "followers") parts))
    (when-let (prop (cdr (assoc "SCORE" drawer-props)))
      (push (concat prop "★") parts))
    (when-let (prop (cdr (assoc "STARS" drawer-props)))
      (push (concat prop "★") parts))
    (when-let (prop (cdr (assoc "VIEWS" drawer-props)))
      (push (concat prop "views") parts))
    (when-let (prop (cdr (assoc "DEPENDENCIES" drawer-props)))
      (push (concat prop "dependencies") parts))
    ;; raws
    (when-let (prop (cdr (assoc "ACRONYM" drawer-props)))
      (push prop parts))
    (when-let (prop (cdr (assoc "STATE" drawer-props)))
      (push prop parts))
    (when-let (prop (cdr (assoc "LANGUAGE" drawer-props)))
      (push prop parts))
    (let ((recap (s-join " " (reverse parts))))
      (if (string-empty-p recap) "properties:" recap))))

(defun my/org-replace-drawers-with-recap ()
  "Replace all org properties-drawers beginnings with a recap"
  (interactive)
  (let ((parsed-buffer (org-element-parse-buffer)))
    (org-element-map parsed-buffer 'property-drawer
      (lambda (drawer)
        (let* ((begin (org-element-property :begin drawer))
               (props (org-entry-properties begin))
               (overlay (make-overlay begin (+ begin (length ":properties:")))))
          (overlay-put overlay 'display (my/org-drawer-recap props)))))))
