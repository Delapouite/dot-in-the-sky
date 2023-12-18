;;; org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/org/")

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

;; this huge function is a copy of the original
;; the only nuance is about the filetags handling :(
(defun my/org-fontify-meta-lines-and-blocks-1 (limit)
  "Fontify #+ lines and blocks."
  (let ((case-fold-search t))
    (when (re-search-forward
	   (rx bol (group (zero-or-more (any " \t")) "#"
			  (group (group (or (seq "+" (one-or-more (any "a-zA-Z")) (optional ":"))
					    (any " \t")
					    eol))
				 (optional (group "_" (group (one-or-more (any "a-zA-Z"))))))
			  (zero-or-more (any " \t"))
			  (group (group (zero-or-more (not (any " \t\n"))))
				 (zero-or-more (any " \t"))
				 (group (zero-or-more any)))))
	   limit t)
      (let ((beg (match-beginning 0))
	    (end-of-beginline (match-end 0))
	    ;; Including \n at end of #+begin line will include \n
	    ;; after the end of block content.
	    (block-start (match-end 0))
	    (block-end nil)
	    (lang (match-string 7)) ; The language, if it is a source block.
	    (bol-after-beginline (line-beginning-position 2))
	    (dc1 (downcase (match-string 2)))
	    (dc3 (downcase (match-string 3)))
	    (whole-blockline org-fontify-whole-block-delimiter-line)
	    beg-of-endline end-of-endline nl-before-endline quoting block-type)
	(cond
	 ((and (match-end 4) (equal dc3 "+begin"))
	  ;; Truly a block
	  (setq block-type (downcase (match-string 5))
		;; Src, example, export, maybe more.
		quoting (member block-type org-protecting-blocks))
	  (when (re-search-forward
		 (rx-to-string `(group bol (or (seq (one-or-more "*") space)
					       (seq (zero-or-more (any " \t"))
						    "#+end"
						    ,(match-string 4)
						    word-end
						    (zero-or-more any)))))
		 ;; We look further than LIMIT on purpose.
		 nil t)
	    ;; We do have a matching #+end line.
	    (setq beg-of-endline (match-beginning 0)
		  end-of-endline (match-end 0)
		  nl-before-endline (1- (match-beginning 0)))
	    (setq block-end (match-beginning 0)) ; Include the final newline.
	    (when quoting
	      (org-remove-flyspell-overlays-in bol-after-beginline nl-before-endline)
	      (remove-text-properties beg end-of-endline
				      '(display t invisible t intangible t)))
	    (add-text-properties
	     beg end-of-endline '(font-lock-fontified t font-lock-multiline t))
	    (org-remove-flyspell-overlays-in beg bol-after-beginline)
	    (org-remove-flyspell-overlays-in nl-before-endline end-of-endline)
            (cond
	     ((and org-src-fontify-natively
                   (string= block-type "src"))
	      (save-match-data
                (org-src-font-lock-fontify-block (or lang "") block-start block-end))
	      (add-text-properties bol-after-beginline block-end '(src-block t)))
	     (quoting
	      (add-text-properties
	       bol-after-beginline beg-of-endline
	       (list 'face
		     (list :inherit
			   (let ((face-name
				  (intern (format "org-block-%s" lang))))
			     (append (and (facep face-name) (list face-name))
				     '(org-block)))))))
	     ((not org-fontify-quote-and-verse-blocks))
	     ((string= block-type "quote")
	      (add-face-text-property
	       bol-after-beginline beg-of-endline 'org-quote t))
	     ((string= block-type "verse")
	      (add-face-text-property
	       bol-after-beginline beg-of-endline 'org-verse t)))
	    ;; Fontify the #+begin and #+end lines of the blocks
	    (add-text-properties
	     beg (if whole-blockline bol-after-beginline end-of-beginline)
	     '(face org-block-begin-line))
	    (unless (eq (char-after beg-of-endline) ?*)
	      (add-text-properties
	       beg-of-endline
	       (if whole-blockline
		   (let ((beg-of-next-line (1+ end-of-endline)))
		     (min (point-max) beg-of-next-line))
		 (min (point-max) end-of-endline))
	       '(face org-block-end-line)))
	    t))
	 ((member dc1 '("+title:" "+subtitle:" "+author:" "+email:" "+date:" "+filetags:"))
	  (org-remove-flyspell-overlays-in
	   (match-beginning 0)
	   (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
	  (add-text-properties
	   beg (match-end 3)
	   (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
	       '(font-lock-fontified t invisible t)
	     '(font-lock-fontified t face org-document-info-keyword)))
	  (add-text-properties
	   (match-beginning 6) (min (point-max) (1+ (match-end 6)))
	   (if (string-equal dc1 "+title:")
	       '(font-lock-fontified t face org-document-title)
	     '(font-lock-fontified t face org-document-info))))
	 ((string-prefix-p "+caption" dc1)
	  (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(display t invisible t intangible t))
	  ;; Handle short captions
	  (save-excursion
	    (forward-line 0)
	    (looking-at (rx (group (zero-or-more (any " \t"))
				   "#+caption"
				   (optional "[" (zero-or-more any) "]")
				   ":")
			    (zero-or-more (any " \t")))))
	  (add-text-properties (line-beginning-position) (match-end 1)
			       '(font-lock-fontified t face org-meta-line))
	  (add-text-properties (match-end 0) (line-end-position)
			       '(font-lock-fontified t face org-block))
	  t)
	 ((member dc3 '(" " ""))
	  ;; Just a comment, the plus was not there
	  (org-remove-flyspell-overlays-in beg (match-end 0))
	  (add-text-properties
	   beg (match-end 0)
	   '(font-lock-fontified t face font-lock-comment-face)))
	 (t ;; Just any other in-buffer setting, but not indented
	  (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(display t invisible t intangible t))
	  (add-text-properties beg (match-end 0)
			       '(font-lock-fontified t face org-meta-line))
	  t))))))

;; https://github.com/rexim/org-cliplink
(defun my/org-cliplink ()
  "Takes a URL from the clipboard and inserts an org-mode link
with the title of a page and the optional target found by the URL
into the current buffer"
  (interactive)
  (org-cliplink-insert-transformed-title
   (org-cliplink-clipboard-content) ;take the URL from the CLIPBOARD
   (lambda (url title)
     (let* ((parsed-url (url-generic-parse-url url))
            (clean-title
             (cond
              ;; if the host is github.com, cleanup the title
              ;;((string= (url-host parsed-url) "github.com")
              ;;(replace-regexp-in-string "GitHub - .*: \\(.*\\)" "\\1" title))
              ;; otherwise keep the original title
              (t title)))
            (target (url-target parsed-url))
            (clean-title (if target (concat clean-title "#" target) clean-title)))
       (message url)
       (message clean-title)
       ;; forward the title to the default org-cliplink transformer
       (org-cliplink-org-mode-link-transformer url clean-title)))))

;; (add-hook 'find-file-hook #'my/org-update-agenda-tag)
(add-hook 'before-save-hook #'my/org-update-agenda-tag)

(advice-add 'org-agenda :before #'my/org-agenda-files-update)
(advice-add 'org-fontify-meta-lines-and-blocks-1 :override #'my/org-fontify-meta-lines-and-blocks-1)
