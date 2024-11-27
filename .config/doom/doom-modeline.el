;;; doom-modeline.el -*- lexical-binding: t; -*-
;;; https://github.com/seagle0128/doom-modeline
;;; https://github.com/doomemacs/doomemacs/tree/master/modules/ui/modeline

(use-package! doom-modeline
  :config

  (setq doom-modeline-total-line-number t)
  (setq doom-modeline-enable-word-count t)

  (defun get-buffer-file-mtime ()
    (let ((mtime (file-attribute-modification-time
                  (file-attributes (buffer-file-name)))))
      (when mtime (iso8601-format mtime))))

  (defun get-mtime-face (mtime)
    "Select a face in a gradient depending on decay"
    (let ((days (iso8601-diff-days mtime)))
      (cond ((> days 300) 'error)
            ((> days 180) 'warning)
            ((< days 30) 'success)
            (t 'mode-line))))

  (doom-modeline-def-segment buffer-mtime
    "Define buffer-mtime modeline segment"
    (let* ((mtime (get-buffer-file-mtime))
           (days (iso8601-diff-days mtime))
           (face (get-mtime-face mtime)))
      (concat (doom-modeline-wspc)
              (propertize (concat mtime " " (number-to-string days) "d") 'face face))))

  (doom-modeline-def-segment org-roam-node-segment
    "Define org-roam modeline segment using node property accessors"
    (letrec ((node (org-roam-node-from-id (org-with-point-at 1 (org-id-get))))
             (links (if node (org-roam-node-links-count node) ""))
             (backlinks (if node (org-roam-node-backlinks-count node) ""))
             (stage (if node (org-roam-node-stage node) ""))
             (interest (if node (org-roam-node-interest node) ""))
             (upgraded-at (if node (org-roam-node-upgraded-at node) ""))
             (combos (if node (org-roam-node-combos node) ""))
             (tags (if node (org-roam-node-template-tags node) "")))
      (concat (doom-modeline-wspc)
              (propertize backlinks 'face (if (string-prefix-p "0" backlinks) 'error 'mode-line))
              "→ →"
              (propertize links 'face (if (string-prefix-p "0" links) 'error 'mode-line))
              " ∧"
              (propertize stage 'face 'mode-line)
              " ★"
              (propertize interest 'face 'mode-line)
              " ↑"
              (propertize upgraded-at 'face 'mode-line)
              " "
              (propertize combos 'face 'mode-line)
              " "
              (propertize tags 'face 'mode-line)
              )))

  (doom-modeline-def-modeline 'my-modeline
    '(bar window-number modals matches buffer-info-simple buffer-mtime buffer-position word-count selection-info)
    '(org-roam-node-segment objed-state misc-info debug lsp minor-modes input-method indent-info major-mode process checker))

  (defun doom-modeline-set-my-modeline ()
    "Enable my modeline"
    (interactive)
    (doom-modeline-set-modeline 'my-modeline))

  (add-hook 'org-mode-hook `doom-modeline-set-my-modeline))
