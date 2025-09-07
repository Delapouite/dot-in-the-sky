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

(defun my/empty-property-drawer (len)
  (let ((element (org-get-property-block)))
    (when element
      (setq org-property-format (concat "%-" (number-to-string len) "s %s"))
      (kill-region (car element) (cdr element)))))

(defun my/org-set-prop (prop key alist)
  (let ((value (or (assoc-default key alist) "null")))
    (cond ((numberp value) (org-set-property prop (number-to-string value)))
          (t (org-set-property prop value)))))

(defun my/org-set-number-prop (prop key alist)
  (org-set-property prop (number-to-string (assoc-default key alist))))

(defun my/org-set-boolean-prop (prop key alist)
  (unless (eq :json-false (assoc-default key alist))
    (org-set-property prop "true")))

;; -at

(defun my/fetched-at ()
  "Set drawer property fetched-at to now in ISO8601"
  (org-set-property "fetched-at" (iso8601-format))
  (setq org-property-format "%-10s %s"))

(defun my/upgraded-at ()
  "Set drawer property upgrated-at to now in ISO8601"
  (interactive)
  (org-set-property "upgraded-at" (iso8601-format)))

(defun my/released-at (date)
  "Set drawer property released-at to provided DATE"
  (interactive "sreleased-at? ")
  (org-set-property "released-at" date))

(defun my/created-at ()
  "Set drawer property created-at to now in ISO8601"
  (interactive)
  (org-set-property "created-at" (iso8601-format)))

(defun my/played-at ()
  "Set drawer property played-at to now in ISO8601"
  (interactive)
  (org-set-property "played-at" (iso8601-format)))

(defun my/visited-at ()
  "Set drawer property visited-at to now in ISO8601"
  (interactive)
  (org-set-property "visited-at" (iso8601-format)))

(defun my/listened-at ()
  "Set drawer property listened-at to now in ISO8601"
  (interactive)
  (org-set-property "listened-at" (iso8601-format)))

(defun my/sloc (count)
  "Set drawer property sloc to provided COUNT"
  (interactive "sSLOC? ")
  (org-set-property "sloc" count))

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


(defun my/substring-ym (string)
  (substring string 0 (min 10 (length string))))

(defun my/org-drawer-recap (drawer-props)
  "Compute recap of an org properties-drawer"
  (let ((parts '()))
    (when-let (prop (cdr (assoc "TYPE" drawer-props)))
      (push (concat "‡" prop) parts))
    ;; dates
    (when-let (prop (cdr (assoc "FETCHED-AT" drawer-props)))
      (push (concat "↓" (substring prop 0 10)) parts))
    (when-let (prop (cdr (assoc "UPGRADED-AT" drawer-props)))
      (push (concat "↑" (substring prop 0 10)) parts))
    (when-let (prop (cdr (assoc "RELEASED-AT" drawer-props)))
      (push (my/substring-ym prop) parts))
    (when-let (prop (cdr (assoc "BORN-AT" drawer-props)))
      (push (concat "⧖" (my/substring-ym prop)) parts))
    (when-let (prop (cdr (assoc "DIED-AT" drawer-props)))
      (push (concat "⧗" (my/substring-ym prop)) parts))
    (when-let (prop (cdr (assoc "PLAYED-AT" drawer-props)))
      (push (substring prop 0 7) parts))
    ;; suffixes
    (when-let (prop (cdr (assoc "REVISIONS" drawer-props)))
      (push (concat (my/number-approx prop) "_revisions") parts))
    (when-let (prop (cdr (assoc "FOLLOWERS" drawer-props)))
      (push (concat (my/number-approx prop) "_followers") parts))
    (when-let (prop (cdr (assoc "SCORE" drawer-props)))
      (push (concat (my/number-approx prop) "★") parts))
    (when-let (prop (cdr (assoc "STARS" drawer-props)))
      (push (concat (my/number-approx prop) (if (cdr (assoc "STARRED" drawer-props)) "★" "☆")) parts))
    (when-let (prop (cdr (assoc "VIEWS" drawer-props)))
      (push (concat (my/number-approx prop) "_views") parts))
    (when-let (prop (cdr (assoc "RATINGS" drawer-props)))
      (push (concat (my/number-approx prop) "_ratings") parts))
    (when-let (prop (cdr (assoc "REVIEWS" drawer-props)))
      (push (concat (my/number-approx prop) "_reviews") parts))
    (when-let (prop (cdr (assoc "DEPENDENCIES" drawer-props)))
      (push (concat prop "dependencies") parts))
    (when-let (prop (cdr (assoc "REPOSITORIES" drawer-props)))
      (push (concat prop "repositories") parts))
    (when-let (prop (cdr (assoc "SOURCE-RANK" drawer-props)))
      (push (concat prop "source-rank") parts))
    (when-let (prop (cdr (assoc "SLOC" drawer-props)))
      (push (concat prop "SLOC") parts))
    ;; booleans
    (when-let (prop (cdr (assoc "TYPES" drawer-props)))
      (push (if (not (string= prop "null")) "types" "") parts))
    ;; raws
    (when-let (prop (cdr (assoc "ACRONYM" drawer-props)))
      (push prop parts))
    (when-let (prop (cdr (assoc "STATE" drawer-props)))
      (push prop parts))
    (when-let (prop (cdr (assoc "LANGUAGE" drawer-props)))
      (push prop parts))
    ;; handy for youtube bass covers
    (when-let (prop (cdr (assoc "CHANNEL" drawer-props)))
      (push prop parts))
    (let ((recap (s-join " " (reverse parts))))
      (if (string-empty-p recap) ":properties:" recap))))

(defun my/org-replace-drawers-with-recap ()
  "Replace all org properties-drawers beginnings with a recap"
  (interactive)
  (org-element-map (org-element-parse-buffer) 'property-drawer
    (lambda (drawer)
      (let* ((begin (org-element-property :begin drawer))
             (props (org-entry-properties begin))
             (overlay (make-overlay begin (+ begin (length ":properties:")))))
        (overlay-put overlay 'display (my/org-drawer-recap props))))))

;; https://stackoverflow.com/questions/5580562/formatting-an-integer-using-iso-prefixes-for-kb-mb-gb-and-kib-mib-gib
(defconst number-to-string-approx-suffixes
  '("k" "M" "G" "T" "P" "E" "Z" "Y"))
(defun number-to-string-approx-suffix (n &optional binary)
  "Return an approximate decimal representation of NUMBER as a string,
followed by a multiplier suffix (k, M, G, T, P, E, Z, Y). The representation
is at most 5 characters long for numbers between 0 and 10^19-5*10^16.
Uses a minus sign if negative.
NUMBER may be an integer or a floating point number.
If the optional argument BINARY is non-nil, use 1024 instead of 1000 as
the base multiplier."
  (if (zerop n)
      "0"
    (let ((sign "")
          (b (if binary 1024 1000))
          (suffix "")
          (bigger-suffixes number-to-string-approx-suffixes))
      (if (< n 0)
          (setq n (- n)
                sign "-"))
      (while (and (>= n 999.5) (consp bigger-suffixes))
        (setq n (/ n b) ; TODO: this is rounding down; nearest would be better
              suffix (car bigger-suffixes)
              bigger-suffixes (cdr bigger-suffixes)))
      (concat sign
              (if (integerp n) (int-to-string n) (number-to-string (floor n)))
              suffix))))
(defun my/number-approx (s)
  (number-to-string-approx-suffix (string-to-number s)))

(defun my/stars (stars)
  "Set drawer property stars to provided STARS"
  (interactive "sstars? ")
  (org-set-property "stars" stars))

(defun my/ratings (ratings)
  "Set drawer property ratings to provided RATINGS"
  (interactive "sratings? ")
  (org-set-property "ratings" ratings))

(defun my/reviews (reviews)
  "Set drawer property reviews to provided REVIEWS"
  (interactive "sreviews? ")
  (org-set-property "reviews" reviews))
