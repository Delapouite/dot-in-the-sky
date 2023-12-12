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
      (when mtime
        (concat (doom-modeline-wspc) (iso8601-format mtime)))))

  (doom-modeline-def-segment buffer-mtime
    "Define buffer-mtime modeline segment"
    (let ((mtime (get-buffer-file-mtime)))
      (propertize mtime 'face (if (string-prefix-p " 2023-0" mtime) 'compilation-error 'mode-line))))

  (doom-modeline-def-segment org-roam-node-segment
    "Define org-roam modeline segment"
    (letrec ((node (org-roam-node-from-id (org-with-point-at 1 (org-id-get))))
             (links (if node (org-roam-node-links-count node) ""))
             (backlinks (if node (org-roam-node-backlinks-count node) ""))
             (interest (if node (org-roam-node-interest node) ""))
             (upgraded-at (if node (org-roam-node-upgraded-at node) ""))
             (tags (if node (org-roam-node-my-tags node) "")))
      (concat (doom-modeline-wspc)
              (propertize backlinks 'face (if (string-prefix-p "0" backlinks) 'compilation-error 'mode-line))
              "→ →"
              (propertize links 'face (if (string-prefix-p "0" links) 'compilation-error 'mode-line))
              " ★"
              (propertize interest 'face 'mode-line)
              " ↑"
              (propertize upgraded-at 'face 'mode-line)
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
